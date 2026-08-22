//go:build !windows

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

func useTemporaryDataDir(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	t.Setenv("XDG_DATA_HOME", root)
	t.Setenv("HOME", root)
	return AppDataDir()
}

func TestInstanceLockLifecycleAndLegacyContents(t *testing.T) {
	dataDir := useTemporaryDataDir(t)
	lockPath := filepath.Join(dataDir, "launcher.lock")
	if err := os.MkdirAll(dataDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(lockPath, []byte(strconv.Itoa(os.Getpid())), 0o644); err != nil {
		t.Fatal(err)
	}

	first := NewInstanceLock()
	if err := first.Acquire(); err != nil {
		t.Fatalf("initial acquisition with a live legacy PID: %v", err)
	}
	second := NewInstanceLock()
	if err := second.Acquire(); err == nil || err.Error() != "Another MiraProt instance is already running." {
		t.Fatalf("concurrent acquisition error = %v", err)
	}

	first.Release()
	if _, err := os.Stat(lockPath); err != nil {
		t.Fatalf("backing file should persist after release: %v", err)
	}
	if err := second.Acquire(); err != nil {
		t.Fatalf("reacquisition after release: %v", err)
	}
	second.Release()
}

func TestInstanceLockSurvivesAbnormalOwnerTermination(t *testing.T) {
	dataDir := useTemporaryDataDir(t)
	cmd := exec.Command(os.Args[0], "-test.run=^TestInstanceLockHelperProcess$")
	cmd.Env = append(os.Environ(), "MIRAPROT_LOCK_HELPER=1")
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
		t.Fatalf("helper did not acquire lock: %q (%v)", scanner.Text(), scanner.Err())
	}

	contender := NewInstanceLock()
	if err := contender.Acquire(); err == nil {
		contender.Release()
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		t.Fatal("acquired lock while helper owned it")
	}
	if err := cmd.Process.Kill(); err != nil {
		t.Fatal(err)
	}
	if err := cmd.Wait(); err == nil {
		t.Fatal("killed helper exited successfully")
	}
	if err := contender.Acquire(); err != nil {
		t.Fatalf("acquisition after abnormal termination: %v", err)
	}
	contender.Release()
	if _, err := os.Stat(filepath.Join(dataDir, "launcher.lock")); err != nil {
		t.Fatalf("backing file should persist after owner death: %v", err)
	}
}

func TestInstanceLockHelperProcess(t *testing.T) {
	if os.Getenv("MIRAPROT_LOCK_HELPER") != "1" {
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
