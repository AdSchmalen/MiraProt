//go:build !windows

package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"

	"golang.org/x/sys/unix"
)

type fileInstanceLock struct {
	path string
	file *os.File
}

// NewInstanceLock creates a lock backed by launcher.lock. The file's contents
// have no meaning; ownership is represented solely by the advisory file lock.
func NewInstanceLock() InstanceLock {
	return &fileInstanceLock{path: filepath.Join(AppDataDir(), "launcher.lock")}
}

func (lock *fileInstanceLock) Acquire() error {
	if lock.file != nil {
		return errAnotherInstance
	}
	file, err := os.OpenFile(lock.path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return fmt.Errorf("open instance lock: %w", err)
	}
	if err := unix.Flock(int(file.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		_ = file.Close()
		if errors.Is(err, unix.EWOULDBLOCK) || errors.Is(err, syscall.EAGAIN) {
			return errAnotherInstance
		}
		return fmt.Errorf("acquire instance lock: %w", err)
	}
	lock.file = file
	return nil
}

func (lock *fileInstanceLock) Release() {
	if lock.file == nil {
		return
	}
	_ = unix.Flock(int(lock.file.Fd()), unix.LOCK_UN)
	_ = lock.file.Close()
	lock.file = nil
}
