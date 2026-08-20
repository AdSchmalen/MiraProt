package main

// To regenerate the Windows exe icon and tray icon from MiraProt_icon.png:
//
//	go run gen_ico.go -write-source
//	goversioninfo -o resource.syso versioninfo.json
//
//go:generate go run gen_ico.go -write-source
//go:generate goversioninfo -o resource.syso versioninfo.json

import (
	"flag"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"syscall"
)

// Version is set at build time via -ldflags.
var Version = "dev"

func main() {
	var (
		port        int
		appDir      string
		rHome       string
		debug       bool
		version     bool
		noBrowser   bool
		noTray      bool
		idleTimeout int
	)

	flag.IntVar(&port, "port", DefaultPort, "Preferred TCP port for the Shiny server")
	flag.StringVar(&appDir, "app-dir", "", "Path to the Shiny application directory")
	flag.StringVar(&rHome, "r-home", "", "Path to the portable R installation (e.g. r-portable/)")
	flag.BoolVar(&debug, "debug", false, "Enable verbose logging")
	flag.BoolVar(&version, "version", false, "Print version and exit")
	flag.BoolVar(&noBrowser, "no-browser", false, "Do not open the system browser automatically")
	flag.BoolVar(&noTray, "no-tray", false, "Do not show system tray icon (headless mode)")
	flag.IntVar(&idleTimeout, "idle-timeout", DefaultIdleTimeoutMin,
		"Minutes of inactivity before auto-shutdown (0 = disabled)")
	flag.Parse()

	if version {
		fmt.Printf("MiraProt Launcher %s (%s/%s)\n", Version, runtime.GOOS, runtime.GOARCH)
		os.Exit(0)
	}

	// --- Resolve default paths relative to the launcher binary ---
	exePath := executableDir()
	if appDir == "" {
		appDir = filepath.Join(exePath, "shiny-app")
	}
	appDir = resolveAppDir(exePath, appDir)
	if rHome == "" {
		rHome = resolveRHome(exePath)
	}

	// --- Initialize logger ---
	logger := &Logger{}
	if err := logger.Init(LogDir()); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to initialize logger: %v\n", err)
		os.Exit(1)
	}
	defer logger.Close()

	logger.Log("LAUNCHER", fmt.Sprintf("MiraProt Launcher %s starting", Version))
	logger.Log("LAUNCHER", fmt.Sprintf("Platform: %s/%s", runtime.GOOS, runtime.GOARCH))
	logger.Log("LAUNCHER", fmt.Sprintf("App dir:  %s", appDir))
	logger.Log("LAUNCHER", fmt.Sprintf("R home:   %s", rHome))
	logger.Log("LAUNCHER", fmt.Sprintf("Data dir: %s", AppDataDir()))
	logger.Log("LAUNCHER", fmt.Sprintf("Log file: %s", logger.GetLogPath()))

	// --- Single instance lock ---
	lock := NewLockFile()
	if err := lock.Acquire(); err != nil {
		logger.Log("LAUNCHER", fmt.Sprintf("Lock error: %v", err))
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		fmt.Fprintf(os.Stderr, "If no other instance is running, delete: %s\n",
			filepath.Join(AppDataDir(), "launcher.lock"))
		os.Exit(1)
	}
	defer lock.Release()

	// --- Validate paths ---
	if err := validatePaths(appDir, rHome); err != nil {
		logger.Log("LAUNCHER", fmt.Sprintf("Path validation failed: %v", err))
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// --- Find a free port ---
	actualPort, err := FindFreePort(port, MaxPort)
	if err != nil {
		logger.Log("LAUNCHER", fmt.Sprintf("Port error: %v", err))
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	if actualPort != port {
		logger.Log("LAUNCHER", fmt.Sprintf("Port %d in use, using %d instead", port, actualPort))
	}
	logger.Log("LAUNCHER", fmt.Sprintf("Using port %d", actualPort))

	url := fmt.Sprintf("http://127.0.0.1:%d", actualPort)

	// quitCh is the unified shutdown signal. Closing it triggers cleanup from
	// any source: tray Quit button, idle timeout, OS signal, or R crash.
	quitCh := make(chan struct{})

	// --- Build R process ---
	rproc := &RProcess{
		port:   actualPort,
		appDir: appDir,
		rHome:  rHome,
		logger: logger,
	}

	// startApp contains the core startup logic. When the system tray is
	// enabled this runs inside the onReady callback (which fires on the
	// main thread on macOS). Without a tray it runs directly.
	startApp := func() {
		// Start R process.
		if err := rproc.Start(); err != nil {
			logger.Log("LAUNCHER", fmt.Sprintf("Failed to start R: %v", err))
			fmt.Fprintf(os.Stderr, "Failed to start R process: %v\n", err)
			fmt.Fprintf(os.Stderr, "Check the log file: %s\n", logger.GetLogPath())
			os.Exit(1)
		}

		// Wait for Shiny readiness (or early R death).
		logger.Log("LAUNCHER", "Waiting for Shiny server to start...")
		readyCh := make(chan error, 1)
		go func() {
			readyCh <- WaitForShiny(url, StartupTimeoutMs, PollIntervalMs, logger)
		}()

		select {
		case err := <-readyCh:
			if err != nil {
				logger.Log("LAUNCHER", fmt.Sprintf("Shiny readiness failed: %v", err))
				fmt.Fprintf(os.Stderr, "Shiny server failed to start: %v\n", err)
				fmt.Fprintf(os.Stderr, "Check the log file: %s\n", logger.GetLogPath())
				rproc.Stop()
				os.Exit(1)
			}
		case <-rproc.Done():
			logger.Log("LAUNCHER", fmt.Sprintf("R process exited during startup (code %d)", rproc.ExitCode()))
			fmt.Fprintf(os.Stderr, "R process exited unexpectedly during startup.\n")
			fmt.Fprintf(os.Stderr, "Check the log file: %s\n", logger.GetLogPath())
			os.Exit(1)
		}

		// Open browser.
		if !noBrowser {
			logger.Log("LAUNCHER", fmt.Sprintf("Opening browser at %s", url))
			if err := OpenBrowser(url); err != nil {
				logger.Log("LAUNCHER", fmt.Sprintf("Browser error (non-fatal): %v", err))
			}
		}

		fmt.Printf("\nMiraProt is running at %s\n", url)
		fmt.Printf("Press Ctrl+C to stop.\n\n")

		// Check for a newer release in the background (non-blocking).
		go func() {
			if msg := CheckForNewRelease(Version, logger); msg != "" {
				fmt.Printf("\n  [RELEASE] %s\n\n", msg)
			}
		}()

		// Start idle monitor.
		idle := &IdleMonitor{
			url:        url,
			timeoutMin: idleTimeout,
			logger:     logger,
			quitCh:     quitCh,
		}
		idle.Start()

		// Listen for OS signals.
		sigCh := make(chan os.Signal, 1)
		signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)

		// Wait for any shutdown trigger.
		select {
		case sig := <-sigCh:
			logger.Log("LAUNCHER", fmt.Sprintf("Received signal: %v", sig))
		case <-rproc.Done():
			logger.Log("LAUNCHER", fmt.Sprintf("R process exited (code %d)", rproc.ExitCode()))
		case <-quitCh:
			logger.Log("LAUNCHER", "Shutdown requested via tray or idle timeout")
		}

		// If we're inside the tray event loop, closing quitCh will cause
		// the tray select to call systray.Quit(), which unblocks RunTray().
		select {
		case <-quitCh:
			// Already closed.
		default:
			close(quitCh)
		}
	}

	// shutdownApp cleans up after the event loop ends.
	shutdownApp := func() {
		logger.Log("LAUNCHER", "Shutting down...")
		rproc.Stop()
		logger.Log("LAUNCHER", "Shutdown complete")
	}

	// --- Run with or without system tray ---
	if noTray || !trayAvailable() {
		startApp()
		shutdownApp()
	} else {
		tray := &TrayApp{
			url:    url,
			logger: logger,
			quitCh: quitCh,
		}
		// systray.Run blocks on the main goroutine. The onReady callback
		// runs the app logic; the onExit callback performs cleanup.
		tray.RunTray(startApp, shutdownApp)
	}
}

// executableDir returns the directory containing the launcher binary.
func executableDir() string {
	exe, err := os.Executable()
	if err != nil {
		return "."
	}
	return filepath.Dir(exe)
}

// resolveAppDir determines the Shiny app directory. It checks several
// conventional locations relative to the launcher binary.
func resolveAppDir(exeDir, fallback string) string {
	candidates := []string{
		fallback,
		filepath.Join(exeDir, "shiny-app"),
		filepath.Join(exeDir, "..", "shiny-app"),
		filepath.Join(exeDir, "..", "Resources", "app"), // macOS .app bundle
	}
	for _, c := range candidates {
		if fi, err := os.Stat(filepath.Join(c, "app.R")); err == nil && !fi.IsDir() {
			return c
		}
	}
	return fallback
}

// resolveRHome determines the R installation directory.
func resolveRHome(exeDir string) string {
	candidates := []string{
		filepath.Join(exeDir, "r-portable"),
		filepath.Join(exeDir, "..", "r-portable"),
		filepath.Join(exeDir, "..", "Resources", "R"), // macOS .app bundle
	}
	for _, c := range candidates {
		rscript := "Rscript"
		if runtime.GOOS == "windows" {
			rscript = "Rscript.exe"
		}
		if _, err := os.Stat(filepath.Join(c, "bin", rscript)); err == nil {
			return c
		}
	}
	return filepath.Join(exeDir, "r-portable")
}

// validatePaths checks that the app directory and R home exist and contain the
// expected files.
func validatePaths(appDir, rHome string) error {
	appR := filepath.Join(appDir, "app.R")
	if _, err := os.Stat(appR); err != nil {
		return fmt.Errorf("app.R not found at %s -- is --app-dir correct?", appR)
	}

	rscript := "Rscript"
	if runtime.GOOS == "windows" {
		rscript = "Rscript.exe"
	}
	rscriptPath := filepath.Join(rHome, "bin", rscript)
	if _, err := os.Stat(rscriptPath); err != nil {
		return fmt.Errorf("Rscript not found at %s -- is --r-home correct?", rscriptPath)
	}

	return nil
}
