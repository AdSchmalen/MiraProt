package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Logger writes timestamped log lines to a daily file and to stdout.
type Logger struct {
	file    *os.File
	logDir  string
	logPath string
}

// Init opens (or creates) the daily log file and removes stale logs.
func (l *Logger) Init(logDir string) error {
	l.logDir = logDir
	if err := os.MkdirAll(logDir, 0o755); err != nil {
		return err
	}

	filename := fmt.Sprintf("miraprot-%s.log", time.Now().Format("2006-01-02"))
	l.logPath = filepath.Join(logDir, filename)

	f, err := os.OpenFile(l.logPath, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return err
	}
	l.file = f

	l.cleanOldLogs()
	return nil
}

// Log writes a single line with timestamp and source tag.
func (l *Logger) Log(source, message string) {
	ts := time.Now().Format("2006-01-02 15:04:05")
	line := fmt.Sprintf("[%s] [%s] %s\n", ts, source, message)
	fmt.Print(line)
	if l.file != nil {
		_, _ = l.file.WriteString(line)
	}
}

// GetLogPath returns the path of the current log file.
func (l *Logger) GetLogPath() string {
	return l.logPath
}

// Close flushes and closes the log file.
func (l *Logger) Close() {
	if l.file != nil {
		_ = l.file.Close()
	}
}

// cleanOldLogs removes log files older than LogMaxAgeDays.
func (l *Logger) cleanOldLogs() {
	cutoff := time.Now().AddDate(0, 0, -LogMaxAgeDays)
	entries, err := os.ReadDir(l.logDir)
	if err != nil {
		return
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasPrefix(e.Name(), "miraprot-") || !strings.HasSuffix(e.Name(), ".log") {
			continue
		}
		info, err := e.Info()
		if err != nil {
			continue
		}
		if info.ModTime().Before(cutoff) {
			_ = os.Remove(filepath.Join(l.logDir, e.Name()))
		}
	}
}
