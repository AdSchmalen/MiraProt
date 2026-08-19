package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"
)

const (
	// DefaultIdleTimeoutMin is the number of minutes with no active Shiny
	// sessions before the launcher initiates an automatic shutdown.
	// Set to 0 to disable idle shutdown.
	DefaultIdleTimeoutMin = 0

	// idleCheckIntervalSec is how often we poll the Shiny server to see if
	// any sessions are still connected.
	idleCheckIntervalSec = 30
)

// healthResponse is the JSON payload returned by the R /__health endpoint.
type healthResponse struct {
	Status   string `json:"status"`
	Sessions int    `json:"sessions"`
}

// IdleMonitor periodically checks whether the Shiny server still has active
// connections. If the server is unreachable (crashed) or has been idle for
// longer than the timeout, it signals shutdown via the quit channel.
type IdleMonitor struct {
	url        string
	timeoutMin int
	logger     *Logger
	quitCh     chan struct{}
}

// Start begins monitoring in a background goroutine. It does nothing if
// timeoutMin is 0 (disabled).
func (im *IdleMonitor) Start() {
	if im.timeoutMin <= 0 {
		return
	}

	im.logger.Log("IDLE", fmt.Sprintf("Idle shutdown enabled: %d minutes", im.timeoutMin))

	go func() {
		client := &http.Client{Timeout: 5 * time.Second}
		idleSince := time.Time{}
		ticker := time.NewTicker(time.Duration(idleCheckIntervalSec) * time.Second)
		defer ticker.Stop()

		for {
			select {
			case <-ticker.C:
				sessions, err := im.checkHealth(client)
				if err != nil {
					// Server not responding -- treat as idle start.
					if idleSince.IsZero() {
						idleSince = time.Now()
						im.logger.Log("IDLE", fmt.Sprintf("Health check failed: %v, starting idle timer", err))
					}
				} else if sessions > 0 {
					// Active sessions -- reset idle timer.
					if !idleSince.IsZero() {
						im.logger.Log("IDLE", fmt.Sprintf("%d active session(s), idle timer reset", sessions))
					}
					idleSince = time.Time{}
				} else {
					// Server alive but no sessions -- start/continue idle timer.
					if idleSince.IsZero() {
						idleSince = time.Now()
						im.logger.Log("IDLE", "No active sessions, starting idle timer")
					}
				}

				if !idleSince.IsZero() {
					elapsed := time.Since(idleSince)
					if elapsed >= time.Duration(im.timeoutMin)*time.Minute {
						im.logger.Log("IDLE", fmt.Sprintf(
							"Idle timeout reached (%d min), initiating shutdown", im.timeoutMin))
						close(im.quitCh)
						return
					}
				}

			case <-im.quitCh:
				return
			}
		}
	}()
}

// checkHealth queries the /__health endpoint and returns the number of active
// sessions. Falls back to a simple GET / if the health endpoint is unavailable.
func (im *IdleMonitor) checkHealth(client *http.Client) (int, error) {
	resp, err := client.Get(im.url + "/__health")
	if err != nil {
		return 0, err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(io.LimitReader(resp.Body, 1024))
	if err != nil {
		return 0, fmt.Errorf("read health response: %w", err)
	}

	var hr healthResponse
	if err := json.Unmarshal(body, &hr); err != nil {
		// Not a JSON health response (e.g. Shiny returned HTML).
		// The server is alive; assume 1 session to be safe.
		return 1, nil
	}

	return hr.Sessions, nil
}
