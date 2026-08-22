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
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"sync"
	"syscall"
)

// Version is set at build time via -ldflags.
var Version = "dev"

type launcherConfig struct {
	port, idleTimeout int
	appDir, rHome     string
	debug, version    bool
	noBrowser, noTray bool
}

func parseConfig(args []string, stderr io.Writer) (launcherConfig, error) {
	var cfg launcherConfig
	fs := flag.NewFlagSet("miraprot-launcher", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.IntVar(&cfg.port, "port", DefaultPort, "Preferred TCP port for the Shiny server")
	fs.StringVar(&cfg.appDir, "app-dir", "", "Path to the Shiny application directory")
	fs.StringVar(&cfg.rHome, "r-home", "", "Path to the portable R installation (e.g. r-portable/)")
	fs.BoolVar(&cfg.debug, "debug", false, "Enable verbose logging")
	fs.BoolVar(&cfg.version, "version", false, "Print version and exit")
	fs.BoolVar(&cfg.noBrowser, "no-browser", false, "Do not open the system browser automatically")
	fs.BoolVar(&cfg.noTray, "no-tray", false, "Do not show system tray icon (headless mode)")
	fs.IntVar(&cfg.idleTimeout, "idle-timeout", DefaultIdleTimeoutMin, "Minutes of inactivity before auto-shutdown (0 = disabled)")
	return cfg, fs.Parse(args)
}

func main() {
	cfg, err := parseConfig(os.Args[1:], os.Stderr)
	if err == nil && cfg.version {
		fmt.Printf("MiraProt Launcher %s (%s/%s)\n", Version, runtime.GOOS, runtime.GOARCH)
		return
	}
	if err == nil {
		err = run(cfg)
	}
	if err != nil {
		fmt.Fprintf(os.Stderr, "MiraProt Launcher: %v\n", err)
		os.Exit(1)
	}
}

type managedRProcess interface {
	Start() error
	Stop() error
	Done() <-chan struct{}
	ExitCode() int
}

type lifecycleDeps struct {
	newLock     func() InstanceLock
	newProcess  func(int, string, string, *Logger) managedRProcess
	validate    func(string, string) error
	findPort    func(int, int) (int, error)
	waitReady   func(string, int, int, *Logger) error
	openBrowser func(string) error
}

var defaultLifecycleDeps = lifecycleDeps{
	newLock: NewInstanceLock,
	newProcess: func(port int, appDir, rHome string, logger *Logger) managedRProcess {
		return &RProcess{port: port, appDir: appDir, rHome: rHome, logger: logger}
	},
	validate: validatePaths, findPort: FindFreePort, waitReady: WaitForShiny, openBrowser: OpenBrowser,
}

func run(cfg launcherConfig) error { return runWithDeps(cfg, defaultLifecycleDeps) }

func runWithDeps(cfg launcherConfig, deps lifecycleDeps) error {
	exePath := executableDir()
	if cfg.appDir == "" {
		cfg.appDir = filepath.Join(exePath, "shiny-app")
	}
	cfg.appDir = resolveAppDir(exePath, cfg.appDir)
	if cfg.rHome == "" {
		cfg.rHome = resolveRHome(exePath)
	}

	logger := &Logger{}
	if err := logger.Init(LogDir()); err != nil {
		return fmt.Errorf("initialize logger: %w", err)
	}
	defer logger.Close()
	logger.Log("LAUNCHER", fmt.Sprintf("MiraProt Launcher %s starting", Version))
	logger.Log("LAUNCHER", fmt.Sprintf("Platform: %s/%s", runtime.GOOS, runtime.GOARCH))
	logger.Log("LAUNCHER", fmt.Sprintf("App dir:  %s", cfg.appDir))
	logger.Log("LAUNCHER", fmt.Sprintf("R home:   %s", cfg.rHome))

	lock := deps.newLock()
	if err := lock.Acquire(); err != nil {
		return fmt.Errorf("acquire instance lock: %w", err)
	}
	defer lock.Release()
	if err := deps.validate(cfg.appDir, cfg.rHome); err != nil {
		return fmt.Errorf("validate paths: %w", err)
	}
	actualPort, err := deps.findPort(cfg.port, MaxPort)
	if err != nil {
		return fmt.Errorf("find free port: %w", err)
	}
	url := fmt.Sprintf("http://127.0.0.1:%d", actualPort)
	logger.Log("LAUNCHER", fmt.Sprintf("Using port %d", actualPort))

	quitCh := make(chan struct{})
	var quitOnce sync.Once
	requestQuit := func() { quitOnce.Do(func() { close(quitCh) }) }
	rproc := deps.newProcess(actualPort, cfg.appDir, cfg.rHome, logger)
	resultCh := make(chan error, 1)
	startApp := func() {
		err := runApp(cfg, url, logger, rproc, quitCh, requestQuit, deps)
		resultCh <- err
		requestQuit()
	}

	if cfg.noTray || !trayAvailable() {
		startApp()
		return <-resultCh
	}
	tray := &TrayApp{url: url, logger: logger, quitCh: quitCh}
	// RunTray remains on this goroutine: Cocoa requires this on macOS.
	// Its ready callback launches startApp, while its exit callback wakes the
	// lifecycle on a user-initiated tray Quit.
	tray.RunTray(startApp, requestQuit)
	return <-resultCh
}

func runApp(cfg launcherConfig, url string, logger *Logger, rproc managedRProcess, quitCh chan struct{}, requestQuit func(), deps lifecycleDeps) error {
	if err := rproc.Start(); err != nil {
		return fmt.Errorf("start R process: %w", err)
	}
	defer func() {
		logger.Log("LAUNCHER", "Shutting down...")
		if err := rproc.Stop(); err != nil {
			logger.Log("LAUNCHER", fmt.Sprintf("R shutdown error: %v", err))
		}
	}()

	logger.Log("LAUNCHER", "Waiting for Shiny server to start...")
	readyCh := make(chan error, 1)
	go func() { readyCh <- deps.waitReady(url, StartupTimeoutMs, PollIntervalMs, logger) }()
	select {
	case err := <-readyCh:
		if err != nil {
			return fmt.Errorf("wait for Shiny readiness: %w", err)
		}
	case <-rproc.Done():
		return fmt.Errorf("R process exited during startup (code %d)", rproc.ExitCode())
	case <-quitCh:
		return nil
	}

	if !cfg.noBrowser {
		if err := deps.openBrowser(url); err != nil {
			logger.Log("LAUNCHER", fmt.Sprintf("Browser error (non-fatal): %v", err))
		}
	}
	fmt.Printf("\nMiraProt is running at %s\nPress Ctrl+C to stop.\n\n", url)
	go func() {
		if msg := CheckForNewRelease(Version, logger); msg != "" {
			fmt.Printf("\n  [RELEASE] %s\n\n", msg)
		}
	}()
	(&IdleMonitor{url: url, timeoutMin: cfg.idleTimeout, logger: logger, quitCh: quitCh}).Start()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(sigCh)
	select {
	case sig := <-sigCh:
		logger.Log("LAUNCHER", fmt.Sprintf("Received signal: %v", sig))
	case <-rproc.Done():
		logger.Log("LAUNCHER", fmt.Sprintf("R process exited (code %d)", rproc.ExitCode()))
	case <-quitCh:
		logger.Log("LAUNCHER", "Shutdown requested via tray or idle timeout")
	}
	requestQuit()
	return nil
}

// executableDir returns the directory containing the launcher binary.
func executableDir() string {
	exe, err := os.Executable()
	if err != nil {
		return "."
	}
	return filepath.Dir(exe)
}

func resolveAppDir(exeDir, fallback string) string {
	candidates := []string{fallback, filepath.Join(exeDir, "shiny-app"), filepath.Join(exeDir, "..", "shiny-app"), filepath.Join(exeDir, "..", "Resources", "app")}
	for _, c := range candidates {
		if fi, err := os.Stat(filepath.Join(c, "app.R")); err == nil && !fi.IsDir() {
			return c
		}
	}
	return fallback
}

func resolveRHome(exeDir string) string {
	candidates := []string{filepath.Join(exeDir, "r-portable"), filepath.Join(exeDir, "..", "r-portable"), filepath.Join(exeDir, "..", "Resources", "R")}
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
