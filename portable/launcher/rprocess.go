package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"time"
)

// RProcess manages the lifecycle of the embedded R / Shiny server process.
type RProcess struct {
	cmd    *exec.Cmd
	port   int
	appDir string
	rHome  string
	logger *Logger

	mu       sync.Mutex
	stopped  bool
	exitCode int
	exitErr  error
	done     chan struct{}
}

// Start launches Rscript with shiny::runApp(), capturing stdout/stderr.
func (rp *RProcess) Start() error {
	rp.mu.Lock()
	defer rp.mu.Unlock()

	rscript := rp.rscriptPath()
	if _, err := os.Stat(rscript); err != nil {
		return fmt.Errorf("Rscript not found at %s: %w", rscript, err)
	}

	rExpr := fmt.Sprintf(
		"shiny::runApp('%s', host='127.0.0.1', port=%d, launch.browser=FALSE)",
		strings.ReplaceAll(rp.appDir, "\\", "/"),
		rp.port,
	)

	rp.cmd = exec.Command(rscript, "--vanilla", "-e", rExpr)
	rp.cmd.Dir = rp.appDir
	rp.cmd.Env = rp.buildEnv()

	stdout, err := rp.cmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("stdout pipe: %w", err)
	}
	stderr, err := rp.cmd.StderrPipe()
	if err != nil {
		return fmt.Errorf("stderr pipe: %w", err)
	}

	if err := rp.cmd.Start(); err != nil {
		return fmt.Errorf("failed to start R process: %w", err)
	}

	rp.done = make(chan struct{})
	rp.logger.Log("LAUNCHER", fmt.Sprintf("R process started (PID %d)", rp.cmd.Process.Pid))

	// Stream stdout/stderr to logger in background goroutines.
	go rp.streamLines("R:stdout", bufio.NewScanner(stdout))
	go rp.streamLines("R:stderr", bufio.NewScanner(stderr))

	// Wait for process exit in the background.
	go func() {
		err := rp.cmd.Wait()
		rp.mu.Lock()
		rp.exitErr = err
		if rp.cmd.ProcessState != nil {
			rp.exitCode = rp.cmd.ProcessState.ExitCode()
		}
		rp.mu.Unlock()
		close(rp.done)
	}()

	return nil
}

// Stop sends a graceful termination signal and falls back to a forced kill
// after ShutdownTimeoutMs.
func (rp *RProcess) Stop() error {
	rp.mu.Lock()
	if rp.stopped || rp.cmd == nil || rp.cmd.Process == nil {
		rp.mu.Unlock()
		return nil
	}
	rp.stopped = true
	pid := rp.cmd.Process.Pid
	rp.mu.Unlock()

	rp.logger.Log("LAUNCHER", fmt.Sprintf("Stopping R process (PID %d)...", pid))

	// On Windows we must kill the process tree; on Unix SIGINT is sufficient.
	if runtime.GOOS == "windows" {
		// taskkill /T kills the process and its children.
		_ = exec.Command("taskkill", "/F", "/T", "/PID", strconv.Itoa(pid)).Run()
	} else {
		_ = rp.cmd.Process.Signal(os.Interrupt)
	}

	// Wait for exit or timeout.
	select {
	case <-rp.done:
		rp.logger.Log("LAUNCHER", "R process exited gracefully")
	case <-time.After(time.Duration(ShutdownTimeoutMs) * time.Millisecond):
		rp.logger.Log("LAUNCHER", "Shutdown timeout reached, force-killing R process")
		_ = rp.cmd.Process.Kill()
		<-rp.done
	}

	return nil
}

// IsRunning reports whether the R process is still alive.
func (rp *RProcess) IsRunning() bool {
	rp.mu.Lock()
	defer rp.mu.Unlock()
	if rp.cmd == nil || rp.cmd.Process == nil {
		return false
	}
	select {
	case <-rp.done:
		return false
	default:
		return true
	}
}

// Done returns a channel that is closed when the R process exits.
func (rp *RProcess) Done() <-chan struct{} {
	return rp.done
}

// ExitCode returns the exit code after the process has terminated.
func (rp *RProcess) ExitCode() int {
	rp.mu.Lock()
	defer rp.mu.Unlock()
	return rp.exitCode
}

// rscriptPath returns the absolute path to the Rscript binary.
func (rp *RProcess) rscriptPath() string {
	if runtime.GOOS == "windows" {
		return filepath.Join(rp.rHome, "bin", "Rscript.exe")
	}
	return filepath.Join(rp.rHome, "bin", "Rscript")
}

// buildEnv constructs the environment variables for the R child process.
func (rp *RProcess) buildEnv() []string {
	// Seed caches from shipped distribution on first launch so the app
	// does not need to download the AnnotationHub index on startup.
	SeedCache("annotation_cache", rp.logger)
	SeedCache("go_cache", rp.logger)

	// Start with the current process environment.
	env := os.Environ()

	dataDir := AppDataDir()
	rLibrary := filepath.Join(filepath.Dir(rp.rHome), "r-library")

	extra := map[string]string{
		"R_LIBS_USER":          rLibrary,
		"MIRAPROT_IN_PORTABLE": "true",
		"MIRAPROT_PORT":        strconv.Itoa(rp.port),
		"MIRAPROT_GO_CACHE":    CacheDir("go_cache"),
		"ANNOTATION_HUB_CACHE":  CacheDir("annotation_cache"),
		"MIRAPROT_LOG_DIR":     filepath.Join(dataDir, "logs"),
	}

	// Override or append each key.
	for key, val := range extra {
		found := false
		prefix := key + "="
		for i, e := range env {
			if strings.HasPrefix(e, prefix) {
				env[i] = prefix + val
				found = true
				break
			}
		}
		if !found {
			env = append(env, prefix+val)
		}
	}

	return env
}

// streamLines reads lines from a scanner and writes them to the logger.
func (rp *RProcess) streamLines(source string, sc *bufio.Scanner) {
	for sc.Scan() {
		rp.logger.Log(source, sc.Text())
	}
}
