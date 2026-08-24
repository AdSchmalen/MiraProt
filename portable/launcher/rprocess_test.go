package main

import (
	"bufio"
	"os"
	"strings"
	"testing"
)

func TestStreamLinesCapturesEssentialRestoreDiagnostics(t *testing.T) {
	logger := &Logger{}
	if err := logger.Init(t.TempDir()); err != nil {
		t.Fatalf("initialize logger: %v", err)
	}
	defer logger.Close()

	rp := &RProcess{logger: logger}
	line := "[ MAIN APP 12:00:00 ] [RestoreCallback:error] generation=7 phase=render owner=PCA reason=render job_id=restore-7-1 code=CALLBACK_ERROR"
	rp.streamLines("R:stdout", bufio.NewScanner(strings.NewReader(line+"\n")))

	contents, err := os.ReadFile(logger.GetLogPath())
	if err != nil {
		t.Fatalf("read launcher log: %v", err)
	}
	if !strings.Contains(string(contents), line) {
		t.Fatalf("launcher log did not capture essential restore diagnostic: %q", contents)
	}
}
