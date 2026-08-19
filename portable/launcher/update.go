package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"time"
)

const (
	githubRepo     = "AdSchmalen/MiraProt"
	updateCheckURL = "https://api.github.com/repos/" + githubRepo + "/releases/latest"
)

// githubRelease is the minimal structure returned by the GitHub Releases API.
type githubRelease struct {
	TagName string `json:"tag_name"`
	HTMLURL string `json:"html_url"`
}

// CheckForUpdate queries the GitHub Releases API and compares the latest
// release tag against the running version. It returns a user-facing message
// if an update is available, or an empty string if up to date.
// This function is non-blocking and safe to call in a goroutine.
func CheckForUpdate(currentVersion string, logger *Logger) string {
	if currentVersion == "dev" {
		return ""
	}

	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Get(updateCheckURL)
	if err != nil {
		logger.Log("UPDATE", fmt.Sprintf("Update check failed: %v", err))
		return ""
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		logger.Log("UPDATE", fmt.Sprintf("Update check returned HTTP %d", resp.StatusCode))
		return ""
	}

	var release githubRelease
	if err := json.NewDecoder(resp.Body).Decode(&release); err != nil {
		logger.Log("UPDATE", fmt.Sprintf("Failed to parse release info: %v", err))
		return ""
	}

	latest := strings.TrimPrefix(release.TagName, "v")
	current := strings.TrimPrefix(currentVersion, "v")

	if latest != current && latest > current {
		msg := fmt.Sprintf("A new version is available: %s (you have %s). Download at: %s",
			release.TagName, currentVersion, release.HTMLURL)
		logger.Log("UPDATE", msg)
		return msg
	}

	logger.Log("UPDATE", fmt.Sprintf("Up to date (version %s)", currentVersion))
	return ""
}
