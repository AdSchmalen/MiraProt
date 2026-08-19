package main

import (
	"fmt"
	"net/http"
	"time"
)

// WaitForShiny polls the Shiny server at the given URL until it responds with
// HTTP 200, or until the timeout expires. It returns nil on success.
func WaitForShiny(url string, timeoutMs, intervalMs int, logger *Logger) error {
	deadline := time.Now().Add(time.Duration(timeoutMs) * time.Millisecond)
	interval := time.Duration(intervalMs) * time.Millisecond

	client := &http.Client{Timeout: 2 * time.Second}

	for {
		if time.Now().After(deadline) {
			return fmt.Errorf("Shiny server did not respond within %d seconds", timeoutMs/1000)
		}

		resp, err := client.Get(url)
		if err == nil {
			resp.Body.Close()
			if resp.StatusCode == http.StatusOK {
				logger.Log("LAUNCHER", fmt.Sprintf("Shiny server is ready at %s", url))
				return nil
			}
		}

		time.Sleep(interval)
	}
}
