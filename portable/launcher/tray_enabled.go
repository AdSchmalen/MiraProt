//go:build !notray

package main

import (
	"fmt"
	"os/exec"
	"runtime"

	"fyne.io/systray"
)

// trayAvailable reports whether systray support was compiled in.
func trayAvailable() bool { return true }

// TrayApp holds references needed by the system tray menu handlers.
type TrayApp struct {
	url     string
	logger  *Logger
	quitCh  chan struct{} // closed when user clicks Quit
}

// RunTray starts the system tray icon and blocks until systray.Quit() is
// called or the quit channel is closed externally. This function must be
// called from the main goroutine on macOS (Cocoa requirement).
//
// Because systray.Run blocks, the actual app logic (R process, polling, etc.)
// runs in the onReady callback or in goroutines started before this call.
func (ta *TrayApp) RunTray(onReady func(), onExit func()) {
	systray.Run(func() {
		systray.SetTitle("MiraProt")
		systray.SetTooltip("MiraProt - Proteomics Analysis")
		systray.SetIcon(iconData)

		mOpen := systray.AddMenuItem("Open in Browser", "Open MiraProt in your default browser")
		mLogs := systray.AddMenuItem("View Log File", "Open the current log file")
		systray.AddSeparator()
		mQuit := systray.AddMenuItem("Quit MiraProt", "Stop the server and exit")

		// Signal that the tray is ready.
		if onReady != nil {
			go onReady()
		}

		// Handle menu clicks.
		for {
			select {
			case <-mOpen.ClickedCh:
				ta.logger.Log("TRAY", "User clicked Open in Browser")
				_ = OpenBrowser(ta.url)

			case <-mLogs.ClickedCh:
				ta.logger.Log("TRAY", "User clicked View Log File")
				openFileInOS(ta.logger.GetLogPath())

			case <-mQuit.ClickedCh:
				ta.logger.Log("TRAY", "User clicked Quit")
				systray.Quit()
				return

			case <-ta.quitCh:
				// External shutdown request (e.g. idle timeout, R crash).
				systray.Quit()
				return
			}
		}
	}, func() {
		if onExit != nil {
			onExit()
		}
	})
}

// openFileInOS opens a file with the default OS handler (text editor, etc.).
func openFileInOS(path string) {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "windows":
		cmd = exec.Command("cmd", "/c", "start", "", path)
	case "darwin":
		cmd = exec.Command("open", path)
	default:
		cmd = exec.Command("xdg-open", path)
	}
	if err := cmd.Start(); err != nil {
		fmt.Printf("Failed to open file: %v\n", err)
	}
}
