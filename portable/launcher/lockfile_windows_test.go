//go:build windows

package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"testing"
)

func TestInstanceLockLifecycleIgnoresLegacyPID(t *testing.T) {
	t.Setenv("LOCALAPPDATA", t.TempDir())
	legacyPath := filepath.Join(AppDataDir(), "launcher.lock")
	if err := os.WriteFile(legacyPath, []byte(strconv.Itoa(os.Getpid())), 0o644); err != nil {
		t.Fatal(err)
	}
	first, second := NewInstanceLock(), NewInstanceLock()
	if err := first.Acquire(); err != nil {
		t.Fatalf("initial acquisition with live legacy PID: %v", err)
	}
	if err := second.Acquire(); err != errAnotherInstance {
		t.Fatalf("concurrent acquisition error = %v", err)
	}
	first.Release()
	if err := second.Acquire(); err != nil {
		t.Fatalf("reacquisition after release: %v", err)
	}
	second.Release()
}

func TestInstanceLockReleasedWhenWindowsOwnerTerminates(t *testing.T) {
	cmd := exec.Command(os.Args[0], "-test.run=^TestWindowsInstanceLockHelperProcess$")
	cmd.Env = append(os.Environ(), "MIRAPROT_WINDOWS_LOCK_HELPER=1")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	scanner := bufio.NewScanner(stdout)
	if !scanner.Scan() || scanner.Text() != "acquired" {
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		t.Fatalf("helper did not acquire lock: %q", scanner.Text())
	}
	if err := cmd.Process.Kill(); err != nil {
		t.Fatal(err)
	}
	_ = cmd.Wait()
	lock := NewInstanceLock()
	if err := lock.Acquire(); err != nil {
		t.Fatalf("acquisition after abnormal termination: %v", err)
	}
	lock.Release()
}

func TestWindowsInstanceLockHelperProcess(t *testing.T) {
	if os.Getenv("MIRAPROT_WINDOWS_LOCK_HELPER") != "1" {
		return
	}
	lock := NewInstanceLock()
	if err := lock.Acquire(); err != nil {
		fmt.Println(err)
		os.Exit(2)
	}
	fmt.Println("acquired")
	select {}
}
