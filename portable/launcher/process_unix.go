//go:build !windows

package main

import (
	"os"
	"syscall"
)

// processExists reports whether a process with the given PID is running.
// On Unix, sending signal 0 checks existence without affecting the process.
func processExists(pid int) bool {
	proc, err := os.FindProcess(pid)
	if err != nil {
		return false
	}
	err = proc.Signal(syscall.Signal(0))
	return err == nil
}
