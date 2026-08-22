package main

import (
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

func TestWaitForShiny(t *testing.T) {
	tests := []struct {
		name       string
		timeout    time.Duration
		interval   time.Duration
		setup      func(t *testing.T) (string, func())
		wantErr    string
		maxElapsed time.Duration
	}{
		{
			name: "connection refusal followed by availability", timeout: 500 * time.Millisecond, interval: 10 * time.Millisecond,
			setup: func(t *testing.T) (string, func()) {
				address := unusedLocalAddress(t)
				server := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusOK) })}
				done := make(chan struct{})
				go func() {
					defer close(done)
					time.Sleep(40 * time.Millisecond)
					listener, err := net.Listen("tcp", address)
					if err == nil {
						_ = server.Serve(listener)
					}
				}()
				return "http://" + address, func() { _ = server.Close(); <-done }
			},
		},
		{
			name: "non-200 followed by 200", timeout: 500 * time.Millisecond, interval: 10 * time.Millisecond,
			setup: func(t *testing.T) (string, func()) {
				var requests atomic.Int32
				server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
					if requests.Add(1) < 3 {
						w.WriteHeader(http.StatusServiceUnavailable)
						return
					}
					w.WriteHeader(http.StatusOK)
				}))
				return server.URL, server.Close
			},
		},
		{
			name: "permanent connection failure", timeout: 80 * time.Millisecond, interval: 10 * time.Millisecond,
			setup: func(t *testing.T) (string, func()) {
				return "http://" + unusedLocalAddress(t), func() {}
			},
			wantErr: "last connection error",
		},
		{
			name: "permanent non-200", timeout: 80 * time.Millisecond, interval: 10 * time.Millisecond,
			setup: func(t *testing.T) (string, func()) {
				server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusTeapot) }))
				return server.URL, server.Close
			},
			wantErr: "last HTTP status 418",
		},
		{
			name: "immediate success", timeout: 500 * time.Millisecond, interval: 100 * time.Millisecond,
			setup: func(t *testing.T) (string, func()) {
				server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusOK) }))
				return server.URL, server.Close
			},
			maxElapsed: 250 * time.Millisecond,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			url, cleanup := tt.setup(t)
			defer cleanup()
			start := time.Now()
			err := WaitForShiny(url, int(tt.timeout/time.Millisecond), int(tt.interval/time.Millisecond), &Logger{})
			elapsed := time.Since(start)
			if tt.wantErr == "" && err != nil {
				t.Fatalf("WaitForShiny() error = %v", err)
			}
			if tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("WaitForShiny() error = %v, want diagnostic %q", err, tt.wantErr)
			}
			if tt.wantErr != "" && !strings.Contains(err.Error(), fmt.Sprint(tt.timeout)) {
				t.Errorf("timeout error %q does not include configured timeout %s", err, tt.timeout)
			}
			if tt.maxElapsed > 0 && elapsed >= tt.maxElapsed {
				t.Errorf("WaitForShiny() took %s, want less than %s", elapsed, tt.maxElapsed)
			}
		})
	}
}

func unusedLocalAddress(t *testing.T) string {
	t.Helper()
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	address := listener.Addr().String()
	if err := listener.Close(); err != nil {
		t.Fatal(err)
	}
	return address
}
