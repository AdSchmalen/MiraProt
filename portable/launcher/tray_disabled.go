//go:build notray

package main

// trayAvailable reports whether systray support was compiled in.
func trayAvailable() bool { return false }

// TrayApp is a no-op stub when compiled with the "notray" build tag.
type TrayApp struct {
	url     string
	logger  *Logger
	quitCh  chan struct{}
}

// RunTray runs without a system tray: calls onReady immediately and onExit
// when the quit channel is closed.
func (ta *TrayApp) RunTray(onReady func(), onExit func()) {
	if onReady != nil {
		onReady()
	}
	<-ta.quitCh
	if onExit != nil {
		onExit()
	}
}
