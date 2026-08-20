package main

import (
	"io"
	"os"
	"path/filepath"
	"runtime"
)

// Network and timing constants, mirrored from the former Electron constants.js.
const (
	DefaultPort       = 3838
	MaxPort           = 4838
	StartupTimeoutMs  = 120000
	PollIntervalMs    = 500
	ShutdownTimeoutMs = 5000
	LogMaxAgeDays     = 7
)

// AppDataDir returns the platform-specific directory for MiraProt user data
// (caches, logs, settings). The directory is created if it does not exist.
func AppDataDir() string {
	var base string

	switch runtime.GOOS {
	case "windows":
		base = os.Getenv("LOCALAPPDATA")
		if base == "" {
			base = filepath.Join(os.Getenv("USERPROFILE"), "AppData", "Local")
		}
	case "darwin":
		home, _ := os.UserHomeDir()
		base = filepath.Join(home, "Library", "Application Support")
	default: // linux and other unix
		base = os.Getenv("XDG_DATA_HOME")
		if base == "" {
			home, _ := os.UserHomeDir()
			base = filepath.Join(home, ".local", "share")
		}
	}

	dir := filepath.Join(base, "MiraProt")
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

// CacheDir returns the writable path to a named cache subdirectory. Flat
// portable bundles use go-cache/ next to the executable. Packaged bundles keep
// their shipped cache read-only and use the user's application data directory.
func CacheDir(name string) string {
	if root := goCacheRoot(); root != "" {
		dir := filepath.Join(root, name)
		_ = os.MkdirAll(dir, 0o755)
		return dir
	}
	if packagedGoCacheRoot() != "" {
		dir := filepath.Join(AppDataDir(), "cache", name)
		_ = os.MkdirAll(dir, 0o755)
		return dir
	}
	// go-cache not found — create it next to the executable so portable mode
	// keeps everything self-contained instead of falling back to AppData.
	if root := createGoCacheRoot(); root != "" {
		dir := filepath.Join(root, name)
		_ = os.MkdirAll(dir, 0o755)
		return dir
	}
	// Last resort: if we cannot determine the exe path at all, use AppData.
	dir := filepath.Join(AppDataDir(), "cache", name)
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

// goCacheRoot locates the writable flat-bundle cache next to the executable.
// Returns "" if no go-cache directory is found.
func goCacheRoot() string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	exeDir := filepath.Dir(exe)
	root := filepath.Join(exeDir, "go-cache")
	if fi, err := os.Stat(root); err == nil && fi.IsDir() {
		return root
	}
	return ""
}

// packagedGoCacheRoot locates a cache embedded in an installer, AppImage, or
// macOS app.
// These locations may be mounted read-only and are used only as seed data.
func packagedGoCacheRoot() string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	exeDir := filepath.Dir(exe)
	candidates := []string{
		filepath.Join(exeDir, "resources", "go-cache"),        // Windows installer
		filepath.Join(exeDir, "..", "go-cache"),              // AppImage usr/go-cache
		filepath.Join(exeDir, "..", "Resources", "go-cache"), // macOS .app bundle
	}
	for _, candidate := range candidates {
		if fi, err := os.Stat(candidate); err == nil && fi.IsDir() {
			return candidate
		}
	}
	return ""
}

// createGoCacheRoot creates the go-cache directory next to the executable and
// returns its path. Returns "" if the executable path cannot be determined.
func createGoCacheRoot() string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	root := filepath.Join(filepath.Dir(exe), "go-cache")
	if err := os.MkdirAll(root, 0o755); err != nil {
		return ""
	}
	return root
}

// LogDir returns the path to the logs subdirectory inside AppDataDir.
func LogDir() string {
	dir := filepath.Join(AppDataDir(), "logs")
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

// ShippedCacheDir returns the path to a pre-built cache subdirectory shipped
// either alongside the launcher or in a packaged resource location. Returns
// "" if the directory does not exist.
func ShippedCacheDir(name string) string {
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	exeDir := filepath.Dir(exe)
	candidates := []string{filepath.Join(exeDir, "go-cache", name)}
	if root := packagedGoCacheRoot(); root != "" {
		candidates = append(candidates, filepath.Join(root, name))
	}
	for _, c := range candidates {
		if fi, err := os.Stat(c); err == nil && fi.IsDir() {
			return c
		}
	}
	return ""
}

// SeedCache copies a shipped pre-built cache into the user's AppData cache
// directory when the target is empty. This avoids the slow first-launch
// AnnotationHub download in the portable distribution.
// When CacheDir already points to the shipped go-cache (portable mode),
// seeding is skipped because we use the shipped directory directly.
func SeedCache(name string, logger *Logger) {
	shipped := ShippedCacheDir(name)
	if shipped == "" {
		return
	}

	target := CacheDir(name)

	// If CacheDir already resolved to the shipped location, no copy needed.
	if filepath.Clean(shipped) == filepath.Clean(target) {
		logger.Log("LAUNCHER", "Using shipped "+name+" cache directly (portable mode)")
		return
	}

	// Only seed when the target directory is empty (first launch).
	entries, err := os.ReadDir(target)
	if err != nil {
		logger.Log("LAUNCHER", "Cache seeding skipped: cannot inspect destination: "+err.Error())
		return
	}
	if len(entries) > 0 {
		// Target already has content — leave it alone.
		return
	}

	logger.Log("LAUNCHER", "Seeding "+name+" cache from shipped distribution")
	if err := copyDir(shipped, target); err != nil {
		logger.Log("LAUNCHER", "Cache seeding failed: "+err.Error())
	} else {
		logger.Log("LAUNCHER", "Cache seeded successfully: "+target)
	}
}

// copyDir recursively copies src directory contents into dst.
func copyDir(src, dst string) error {
	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		rel, err := filepath.Rel(src, path)
		if err != nil {
			return err
		}
		target := filepath.Join(dst, rel)

		if info.IsDir() {
			return os.MkdirAll(target, 0o755)
		}

		return copyFile(path, target)
	})
}

// copyFile copies a single file from src to dst.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	return out.Close()
}
