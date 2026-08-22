//go:build windows

package main

import (
	"fmt"
	"unsafe"

	"golang.org/x/sys/windows"
)

const instanceSemaphoreName = `Local\MiraProt.Launcher.InstanceLock`

var procCreateSemaphoreW = windows.NewLazySystemDLL("kernel32.dll").NewProc("CreateSemaphoreW")

type semaphoreInstanceLock struct {
	handle windows.Handle
}

// NewInstanceLock creates a per-session named object. Object existence is the
// ownership marker, so Windows destroys the lock if its process terminates.
func NewInstanceLock() InstanceLock { return &semaphoreInstanceLock{} }

func (lock *semaphoreInstanceLock) Acquire() error {
	if lock.handle != 0 {
		return errAnotherInstance
	}
	name, err := windows.UTF16PtrFromString(instanceSemaphoreName)
	if err != nil {
		return fmt.Errorf("encode instance lock name: %w", err)
	}
	handle, _, callErr := procCreateSemaphoreW.Call(0, 1, 1, uintptr(unsafe.Pointer(name)))
	if handle == 0 {
		return fmt.Errorf("create instance semaphore: %w", callErr)
	}
	if callErr == windows.ERROR_ALREADY_EXISTS {
		_ = windows.CloseHandle(windows.Handle(handle))
		return errAnotherInstance
	}
	lock.handle = windows.Handle(handle)
	return nil
}

func (lock *semaphoreInstanceLock) Release() {
	if lock.handle != 0 {
		_ = windows.CloseHandle(lock.handle)
		lock.handle = 0
	}
}
