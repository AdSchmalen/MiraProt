package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// LockFile prevents multiple instances of the launcher from running at once.
// The lock file contains the PID of the owning process.
type LockFile struct {
	path string
}

// NewLockFile creates a LockFile in the app data directory.
func NewLockFile() *LockFile {
	return &LockFile{
		path: filepath.Join(AppDataDir(), "launcher.lock"),
	}
}

// Acquire attempts to create the lock file. If a lock already exists and the
// owning process is still running, it returns an error. Stale locks (dead PID)
// are automatically removed.
func (lf *LockFile) Acquire() error {
	data, err := os.ReadFile(lf.path)
	if err == nil {
		pid, convErr := strconv.Atoi(strings.TrimSpace(string(data)))
		if convErr == nil && processExists(pid) {
			return fmt.Errorf("another MiraProt instance is already running (PID %d)", pid)
		}
		// Stale lock file -- remove it.
		_ = os.Remove(lf.path)
	}

	return os.WriteFile(lf.path, []byte(strconv.Itoa(os.Getpid())), 0o644)
}

// Release removes the lock file.
func (lf *LockFile) Release() {
	_ = os.Remove(lf.path)
}
