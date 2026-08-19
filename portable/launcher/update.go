package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"time"
)

const (
	githubRepo     = "AdSchmalen/MiraProt"
	updateCheckURL = "https://api.github.com/repos/" + githubRepo + "/releases/latest"
)

var developmentVersion = regexp.MustCompile(`^v?[0-9]+\.[0-9]+\.[0-9]+-[0-9]+-g[0-9a-f]+(?:-dirty)?$`)

// githubRelease is the minimal structure returned by the GitHub Releases API.
type githubRelease struct {
	TagName string `json:"tag_name"`
	HTMLURL string `json:"html_url"`
}

type semanticVersion struct {
	major, minor, patch int
	prerelease          []string
}

// parseSemanticVersion accepts the project's release tag form: three numeric
// components, an optional leading v, and optional SemVer prerelease identifiers.
func parseSemanticVersion(tag string) (semanticVersion, bool) {
	tag = strings.TrimPrefix(tag, "v")
	tag = strings.SplitN(tag, "+", 2)[0]
	parts := strings.SplitN(tag, "-", 2)
	core := strings.Split(parts[0], ".")
	if len(core) != 3 {
		return semanticVersion{}, false
	}
	numbers := make([]int, 3)
	for i, component := range core {
		if component == "" || (len(component) > 1 && component[0] == '0') {
			return semanticVersion{}, false
		}
		value, err := strconv.Atoi(component)
		if err != nil || value < 0 {
			return semanticVersion{}, false
		}
		numbers[i] = value
	}

	var prerelease []string
	if len(parts) == 2 {
		prerelease = strings.Split(parts[1], ".")
		for _, identifier := range prerelease {
			if identifier == "" || !validPrereleaseIdentifier(identifier) {
				return semanticVersion{}, false
			}
			if _, err := strconv.Atoi(identifier); err == nil && len(identifier) > 1 && identifier[0] == '0' {
				return semanticVersion{}, false
			}
		}
	}
	return semanticVersion{numbers[0], numbers[1], numbers[2], prerelease}, true
}

func validPrereleaseIdentifier(identifier string) bool {
	for _, char := range identifier {
		if !(char >= '0' && char <= '9') && !(char >= 'A' && char <= 'Z') && !(char >= 'a' && char <= 'z') && char != '-' {
			return false
		}
	}
	return true
}

// compareSemanticVersions returns -1, 0, or 1 when left is older than, equal
// to, or newer than right. Both arguments must have been parsed successfully.
func compareSemanticVersions(left, right semanticVersion) int {
	for _, pair := range [][2]int{{left.major, right.major}, {left.minor, right.minor}, {left.patch, right.patch}} {
		if pair[0] < pair[1] {
			return -1
		}
		if pair[0] > pair[1] {
			return 1
		}
	}
	if len(left.prerelease) == 0 && len(right.prerelease) > 0 {
		return 1
	}
	if len(left.prerelease) > 0 && len(right.prerelease) == 0 {
		return -1
	}
	for i := 0; i < len(left.prerelease) && i < len(right.prerelease); i++ {
		l, lerr := strconv.Atoi(left.prerelease[i])
		r, rerr := strconv.Atoi(right.prerelease[i])
		switch {
		case lerr == nil && rerr == nil:
			if l < r {
				return -1
			}
			if l > r {
				return 1
			}
		case lerr == nil:
			return -1
		case rerr == nil:
			return 1
		default:
			if left.prerelease[i] < right.prerelease[i] {
				return -1
			}
			if left.prerelease[i] > right.prerelease[i] {
				return 1
			}
		}
	}
	if len(left.prerelease) < len(right.prerelease) {
		return -1
	}
	if len(left.prerelease) > len(right.prerelease) {
		return 1
	}
	return 0
}

// CheckForUpdate queries the latest GitHub Release tag and reports newer source.
func CheckForUpdate(currentVersion string, logger *Logger) string {
	client := &http.Client{Timeout: 10 * time.Second}
	return checkForUpdate(currentVersion, logger, client, updateCheckURL)
}

func checkForUpdate(currentVersion string, logger *Logger, client *http.Client, url string) string {
	if currentVersion == "dev" || developmentVersion.MatchString(currentVersion) {
		return ""
	}
	current, ok := parseSemanticVersion(currentVersion)
	if !ok {
		logger.Log("UPDATE", fmt.Sprintf("Skipping update check for unrecognized version %q", currentVersion))
		return ""
	}

	resp, err := client.Get(url)
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
	latest, ok := parseSemanticVersion(release.TagName)
	if !ok {
		logger.Log("UPDATE", fmt.Sprintf("Latest release has unrecognized tag %q", release.TagName))
		return ""
	}
	if compareSemanticVersions(latest, current) > 0 {
		msg := fmt.Sprintf("A new version is available: %s (you have %s). Obtain the newer source at: %s, then rebuild your portable installation.", release.TagName, currentVersion, release.HTMLURL)
		logger.Log("UPDATE", msg)
		return msg
	}

	logger.Log("UPDATE", fmt.Sprintf("Up to date (version %s)", currentVersion))
	return ""
}
