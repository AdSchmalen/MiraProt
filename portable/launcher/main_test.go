package main

import (
	"errors"
	"testing"
)

type fakeLifecycleLock struct {
	acquired bool
	released bool
}

func (l *fakeLifecycleLock) Acquire() error { l.acquired = true; return nil }
func (l *fakeLifecycleLock) Release()       { l.released = true }

type fakeLifecycleProcess struct {
	started  bool
	stopped  bool
	done     chan struct{}
	startErr error
}

func (p *fakeLifecycleProcess) Start() error          { p.started = true; return p.startErr }
func (p *fakeLifecycleProcess) Stop() error           { p.stopped = true; return nil }
func (p *fakeLifecycleProcess) Done() <-chan struct{} { return p.done }
func (p *fakeLifecycleProcess) ExitCode() int         { return 0 }

func testLifecycleDeps(lock InstanceLock, process managedRProcess) lifecycleDeps {
	return lifecycleDeps{
		newLock:     func() InstanceLock { return lock },
		newProcess:  func(int, string, string, *Logger) managedRProcess { return process },
		validate:    func(string, string) error { return nil },
		findPort:    func(port, _ int) (int, error) { return port, nil },
		waitReady:   func(string, int, int, *Logger) error { return nil },
		openBrowser: func(string) error { return nil },
	}
}

func TestRunReleasesLockAfterPostAcquisitionFailure(t *testing.T) {
	t.Setenv("XDG_DATA_HOME", t.TempDir())
	lock := &fakeLifecycleLock{}
	process := &fakeLifecycleProcess{done: make(chan struct{}), startErr: errors.New("start failed")}
	deps := testLifecycleDeps(lock, process)

	err := runWithDeps(launcherConfig{noTray: true}, deps)
	if err == nil {
		t.Fatal("runWithDeps returned nil error")
	}
	if !lock.acquired {
		t.Fatal("instance lock was not acquired")
	}
	if !lock.released {
		t.Fatal("instance lock was not released")
	}
}

func TestRunStopsProcessAfterPostStartFailure(t *testing.T) {
	t.Setenv("XDG_DATA_HOME", t.TempDir())
	lock := &fakeLifecycleLock{}
	process := &fakeLifecycleProcess{done: make(chan struct{})}
	deps := testLifecycleDeps(lock, process)
	deps.waitReady = func(string, int, int, *Logger) error { return errors.New("not ready") }

	err := runWithDeps(launcherConfig{noTray: true, noBrowser: true}, deps)
	if err == nil {
		t.Fatal("runWithDeps returned nil error")
	}
	if !process.started {
		t.Fatal("R process was not started")
	}
	if !process.stopped {
		t.Fatal("R process was not stopped")
	}
	if !lock.released {
		t.Fatal("instance lock was not released")
	}
}
