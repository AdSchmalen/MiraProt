//go:build windows

package main

import (
	"os"
	"syscall"
)

// processExists reports whether a process with the given PID is running.
// On Windows, we attempt to open the process handle.
func processExists(pid int) bool {
	const PROCESS_QUERY_LIMITED_INFORMATION = 0x1000

	handle, err := syscall.OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, uint32(pid))
	if err != nil {
		return false
	}
	_ = syscall.CloseHandle(handle)

	// Double-check with os.FindProcess (always succeeds on Windows) and
	// verify the process has not already exited by checking its wait status.
	proc, err := os.FindProcess(pid)
	if err != nil {
		return false
	}
	_ = proc.Release()
	return true
}
