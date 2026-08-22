package main

import (
	"context"
	"fmt"
	"net/http"
	"time"
)

// WaitForShiny polls the Shiny server at the given URL until it responds with
// HTTP 200, or until the timeout expires. It returns nil on success.
func WaitForShiny(url string, timeoutMs, intervalMs int, logger *Logger) error {
	start := time.Now()
	timeout := time.Duration(timeoutMs) * time.Millisecond
	deadline := start.Add(timeout)
	interval := time.Duration(intervalMs) * time.Millisecond
	var lastRequestErr error
	var lastStatus int
	var receivedResponse bool
	var latestWasResponse bool

	client := &http.Client{}
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	for {
		if !time.Now().Before(deadline) {
			return shinyTimeoutError(timeout, time.Since(start), receivedResponse, latestWasResponse, lastStatus, lastRequestErr)
		}

		req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
		if err != nil {
			return fmt.Errorf("create Shiny readiness request: %w", err)
		}
		resp, err := client.Do(req)
		if err != nil {
			lastRequestErr = err
			latestWasResponse = false
		} else {
			resp.Body.Close()
			receivedResponse = true
			latestWasResponse = true
			lastStatus = resp.StatusCode
			if resp.StatusCode == http.StatusOK {
				logger.Log("LAUNCHER", fmt.Sprintf("Shiny server is ready at %s after %s", url, conciseDuration(time.Since(start))))
				return nil
			}
		}

		remaining := time.Until(deadline)
		if remaining <= 0 {
			continue
		}
		wait := min(interval, remaining)
		timer := time.NewTimer(wait)
		select {
		case <-timer.C:
		case <-ctx.Done():
			timer.Stop()
		}
	}
}

func conciseDuration(d time.Duration) time.Duration {
	return d.Round(time.Millisecond)
}

func shinyTimeoutError(timeout, elapsed time.Duration, receivedResponse, latestWasResponse bool, lastStatus int, lastRequestErr error) error {
	duration := conciseDuration(elapsed)
	if timeout > 0 {
		duration = timeout
	}
	if receivedResponse && latestWasResponse {
		return fmt.Errorf("Shiny server was not ready after %s: last HTTP status %d", duration, lastStatus)
	}
	if lastRequestErr != nil {
		return fmt.Errorf("Shiny server was not ready after %s: last connection error: %w", duration, lastRequestErr)
	}
	if receivedResponse {
		return fmt.Errorf("Shiny server was not ready after %s: last HTTP status %d", duration, lastStatus)
	}
	return fmt.Errorf("Shiny server was not ready after %s: no HTTP response received", duration)
}
