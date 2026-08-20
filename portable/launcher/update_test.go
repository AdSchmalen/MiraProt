package main

import (
	"net/http"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
)

func TestSemanticVersionComparison(t *testing.T) {
	tests := []struct {
		name           string
		left, right    string
		wantComparison int
		wantValid      bool
	}{
		{"equal versions", "1.2.3", "1.2.3", 0, true},
		{"numeric minor comparison", "1.9.0", "1.10.0", -1, true},
		{"leading v", "v1.10.0", "1.9.0", 1, true},
		{"prerelease before release", "1.2.3-rc.1", "1.2.3", -1, true},
		{"project prerelease ordering", "1.2.3-rc.2", "v1.2.3-rc.10", -1, true},
		{"malformed tag", "release-1.2.3", "1.2.3", 0, false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			left, leftOK := parseSemanticVersion(test.left)
			right, rightOK := parseSemanticVersion(test.right)
			if leftOK != test.wantValid || rightOK != true {
				t.Fatalf("parse validity = (%v, %v), want (%v, true)", leftOK, rightOK, test.wantValid)
			}
			if test.wantValid && compareSemanticVersions(left, right) != test.wantComparison {
				t.Fatalf("comparison = %d, want %d", compareSemanticVersions(left, right), test.wantComparison)
			}
		})
	}
}

func TestCheckForNewRelease(t *testing.T) {
	tests := []struct {
		name, current string
		status        int
		body          string
		wantMessage   string
		wantLog       string
		wantRequests  int
	}{
		{"new numeric minor", "1.9.0", http.StatusOK, `{"tag_name":"v1.10.0","html_url":"https://example.test/release"}`, "A newer MiraProt release is available", "[RELEASE]", 1},
		{"equal versions", "v1.10.0", http.StatusOK, `{"tag_name":"1.10.0","html_url":"https://example.test/release"}`, "", "Up to date", 1},
		{"prerelease update", "1.2.0-rc.1", http.StatusOK, `{"tag_name":"v1.2.0-rc.2","html_url":"https://example.test/release"}`, "Obtain the newer source", "[RELEASE]", 1},
		{"development literal", "dev", http.StatusOK, `{}`, "", "", 0},
		{"plain commit", "a673a3c", http.StatusOK, `{}`, "", "", 0},
		{"plain dirty commit", "a673a3c-dirty", http.StatusOK, `{}`, "", "", 0},
		{"long commit", "0123456789abcdef0123456789abcdef01234567", http.StatusOK, `{}`, "", "", 0},
		{"git describe development build", "v1.2.0-4-gabc1234", http.StatusOK, `{}`, "", "", 0},
		{"dirty git describe development build", "v1.2.0-4-gabc1234-dirty", http.StatusOK, `{}`, "", "", 0},
		{"malformed current tag", "banana", http.StatusOK, `{}`, "", "unsupported launcher version format", 0},
		{"malformed release tag", "1.0.0", http.StatusOK, `{"tag_name":"latest","html_url":"https://example.test/release"}`, "", "unrecognized tag", 1},
		{"API failure", "1.0.0", http.StatusInternalServerError, `{}`, "", "Release check returned HTTP 500", 1},
		{"missing release", "1.0.0", http.StatusNotFound, `{}`, "", "Release check returned HTTP 404", 1},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			requests := 0
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				requests++
				w.WriteHeader(test.status)
				_, _ = w.Write([]byte(test.body))
			}))
			defer server.Close()

			logger := &Logger{}
			if err := logger.Init(t.TempDir()); err != nil {
				t.Fatal(err)
			}
			defer logger.Close()

			message := checkForNewRelease(test.current, logger, server.Client(), server.URL)
			if !strings.Contains(message, test.wantMessage) || (test.wantMessage == "" && message != "") {
				t.Fatalf("message = %q, want substring %q", message, test.wantMessage)
			}
			if requests != test.wantRequests {
				t.Fatalf("requests = %d, want %d", requests, test.wantRequests)
			}
			logContents, err := os.ReadFile(logger.GetLogPath())
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(logContents), test.wantLog) || (test.wantLog == "" && len(logContents) != 0) {
				t.Fatalf("log = %q, want substring %q", logContents, test.wantLog)
			}
		})
	}
}

func TestCheckForNewReleaseNetworkFailure(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {}))
	client := server.Client()
	url := server.URL
	server.Close()

	logger := &Logger{}
	if message := checkForNewRelease("v1.0.0", logger, client, url); message != "" {
		t.Fatalf("message = %q, want empty notification", message)
	}
}
