# MiraProt Standalone Edition — Developer Guide

This guide explains how to build, test, and release the portable desktop edition
of MiraProt. It covers compiling the Go launcher, bundling R and all packages,
creating platform-specific installers, and using the CI/CD pipeline.

---

## Table of Contents

1. [Prerequisites](#1-prerequisites)
2. [Project Structure](#2-project-structure)
3. [Building the Go Launcher](#3-building-the-go-launcher)
4. [Bundling a Portable Distribution (Linux/macOS)](#4-bundling-a-portable-distribution-linuxmacos)
5. [Bundling a Portable Distribution (Windows)](#5-bundling-a-portable-distribution-windows)
6. [Testing the Build](#6-testing-the-build)
7. [Creating Installers](#7-creating-installers)
8. [CI/CD: Automated Builds and Releases](#8-cicd-automated-builds-and-releases)
9. [Launcher CLI Reference](#9-launcher-cli-reference)
10. [Architecture Overview](#10-architecture-overview)
11. [Troubleshooting](#11-troubleshooting)

---

## 1. Prerequisites

### All platforms

| Tool | Version | Purpose |
|---|---|---|
| **Go** | 1.22 or later | Compile the launcher binary |
| **R** | 4.6.0 | Bundled R runtime and package compilation |
| **Git** | Any recent version | Version tagging and CI/CD |

**Install Go:**

| OS | Command |
|---|---|
| Windows | Download the installer from https://go.dev/dl/ |
| macOS | `brew install go` |
| Linux | `sudo snap install go --classic` or download from https://go.dev/dl/ |

Verify: `go version` should print `go1.22` or later.

**Install R 4.6.0:**

| OS | Command |
|---|---|
| Windows | Download from https://cran.r-project.org/bin/windows/base/ |
| macOS | `brew install r` or use [rig](https://github.com/r-lib/rig): `rig add 4.6.0` |
| Linux | `sudo apt install r-base` or use rig: `rig add 4.6.0` |

Verify: `Rscript --version` should print `4.6.0` or later.

### Linux only

Install system libraries required for compiling R packages and the system tray:

```bash
sudo apt-get install -y \
  libfreetype6-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libtiff5-dev \
  libjpeg-dev \
  libpng-dev \
  librsvg2-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgtk-3-dev \
  libayatana-appindicator3-dev
```

You also need `rsync` (usually pre-installed) and `gcc` (for CGO, required by
the system tray library).

### macOS only

You need `rsync` (pre-installed on macOS) and Xcode Command Line Tools:

```bash
xcode-select --install
```

### Windows only

- **PowerShell 5.1+** (built into Windows 10/11)
- **Inno Setup 6** (for building the installer):
  ```
  choco install innosetup -y
  ```
  Or download from https://jrsoftware.org/isdl.php

---

## 2. Project Structure

All portable-edition files live under the `portable/` directory:

```
portable/
├── launcher/                    # Go source code
│   ├── main.go                  # Entry point, CLI flags, startup orchestration
│   ├── config.go                # Constants, platform-specific data paths
│   ├── rprocess.go              # R subprocess lifecycle management
│   ├── health.go                # HTTP health-check polling
│   ├── browser.go               # Cross-platform browser opening
│   ├── portfinder.go            # Find a free TCP port
│   ├── lockfile.go              # Single-instance PID lock
│   ├── idle.go                  # Idle timeout monitor
│   ├── logger.go                # File + console logging
│   ├── update.go                # GitHub Releases update checker
│   ├── tray_enabled.go          # System tray (default build)
│   ├── tray_disabled.go         # No-op stub (build tag: notray)
│   ├── process_unix.go          # Unix-specific process handling
│   ├── process_windows.go       # Windows-specific process handling
│   ├── icon_data.go             # Embedded tray icon bytes
│   ├── gen_icon.go              # Icon generation helper
│   ├── go.mod                   # Go module (requires Go 1.22)
│   └── go.sum                   # Dependency checksums
│
├── scripts/
│   ├── bundle-r.sh              # Linux/macOS: bundle R + packages + app + launcher
│   ├── bundle-r-windows.ps1     # Windows: same, using PowerShell
│   └── install-packages.R       # Install all R packages into a library directory
│
├── installers/
│   ├── windows/
│   │   └── MiraProt.iss        # Inno Setup installer script
│   ├── macos/
│   │   ├── create-dmg.sh        # Build .app bundle and .dmg disk image
│   │   └── Info.plist           # macOS app bundle metadata
│   └── linux/
│       └── create-appimage.sh   # Build Linux AppImage
│
└── resources/                   # Icons and assets
```

---

## 3. Building the Go Launcher

The launcher is a standalone Go binary that starts the bundled R/Shiny server,
opens the browser, shows a system tray icon, and monitors for shutdown triggers.

### Basic build

```bash
cd portable/launcher
go build -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher .
```

On Windows:
```powershell
cd portable\launcher
go build -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher.exe .
```

Verify:
```bash
./MiraProt-launcher --version
# Output: MiraProt Launcher dev (linux/amd64)
```

### What the flags mean

| Flag | Purpose |
|---|---|
| `-s -w` | Strip debug symbols and DWARF info (smaller binary) |
| `-X main.Version=dev` | Embed the version string at compile time |

### Build tags

The launcher includes system tray support by default. To build **without** the
system tray (useful for headless servers or if GTK is unavailable):

```bash
go build -tags=notray -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher .
```

### CGO requirements

| OS | CGO_ENABLED | Why |
|---|---|---|
| Linux | `1` (default) | Required by the system tray library (`libgtk-3`, `libayatana-appindicator3`) |
| macOS | `0` | System tray uses native Cocoa APIs via cgo-free bindings |
| Windows | `0` | System tray uses native Win32 APIs via cgo-free bindings |

If you build on Linux without the system tray, you can disable CGO:
```bash
CGO_ENABLED=0 go build -tags=notray -ldflags "-s -w" -o MiraProt-launcher .
```

### Cross-compilation

To build for a different platform (without system tray):
```bash
GOOS=windows GOARCH=amd64 CGO_ENABLED=0 go build -tags=notray \
  -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher.exe .
```

Cross-compiling **with** the system tray on Linux requires the target platform's
C toolchain and GTK headers, which is complex. Use the CI/CD pipeline instead
(see [section 8](#8-cicd-automated-builds-and-releases)).

---

## 4. Bundling a Portable Distribution (Linux/macOS)

The `bundle-r.sh` script creates a complete self-contained distribution
directory containing the launcher, a copy of R, all R packages, and the Shiny
application.

### Running the bundler

```bash
cd /path/to/MiraProt
chmod +x portable/scripts/bundle-r.sh
./portable/scripts/bundle-r.sh --r-version 4.6.0 --output-dir ./dist
```

Or using environment variables:
```bash
R_VERSION=4.6.0 OUTPUT_DIR=./dist ./portable/scripts/bundle-r.sh
```

### What it does (step by step)

1. **Copies R** — Copies your system R installation into `dist/r-portable/`.
2. **Installs system dependencies** (Linux only) — Checks for and installs
   missing `-dev` packages via `apt-get`.
3. **Installs R packages** — Runs `install-packages.R` to install all ~97
   packages into `dist/r-library/`. This takes **30-60 minutes** on a fresh
   build. Packages are: 17 Bioconductor, 62 CRAN, 18 optional, 1 GitHub
   (shinyTree).
4. **Copies the Shiny application** — Uses `rsync` to copy the project
   (excluding `.git`, `cache/`, `portable/`, `scripts/`, `tests/`, etc.) into
   `dist/shiny-app/`. The top-level `scripts/` and `tests/` directories remain
   in the source checkout for development and pre-package validation, but are
   omitted from the generated runtime distribution.
5. **Builds the Go launcher** — Compiles the launcher into `dist/MiraProt-launcher`.

### Output directory structure

After bundling, `dist/` contains:
```
dist/
├── MiraProt-launcher     # Go binary (or .exe on Windows)
├── shiny-app/             # The MiraProt R application
├── r-portable/            # Portable R installation
└── r-library/             # Pre-compiled R packages
```

---

## 5. Bundling a Portable Distribution (Windows)

The `bundle-r-windows.ps1` script does the same thing as `bundle-r.sh` but
for Windows, using PowerShell.

### Running the bundler

```powershell
cd C:\path\to\MiraProt
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.6.0" -OutputDir ".\dist"
```

### What it does (step by step)

1. **Downloads R** — Downloads the R 4.6.0 installer from CRAN
   (`R-4.6.0-win.exe`) to your temp directory.
2. **Silent install** — Runs the R installer silently into `dist\r-portable\`.
3. **Installs R packages** — Same as Linux/macOS, ~30-60 minutes.
4. **Copies the Shiny application** — Uses `robocopy` (built into Windows) to
   copy the project into `dist\shiny-app\`.
5. **Builds the Go launcher** — Compiles into `dist\MiraProt-launcher.exe`.

---

## 6. Testing the Build

After bundling, run through these checks to verify the distribution works.

### 1. Version check

```bash
./dist/MiraProt-launcher --version
```
Should print: `MiraProt Launcher <version> (<os>/<arch>)`

### 2. Full startup with debug logging

```bash
./dist/MiraProt-launcher --debug
```

Watch for these log messages:
```
[LAUNCHER] MiraProt Launcher <version> starting
[LAUNCHER] R process started (PID ...)
[LAUNCHER] Waiting for Shiny server to start...
[LAUNCHER] Opening browser at http://127.0.0.1:3838
```

### 3. Browser and app

Open `http://127.0.0.1:3838` in your browser. Verify:
- All module tabs are visible (Data Wizard, PCA, Volcano, etc.)
- Upload a test file in Data Wizard
- Switch between modules

### 4. System tray

Check that the MiraProt icon appears in the system tray (near the clock).
Right-click it to see the menu (Open in Browser, View Log File, Quit).

### 5. Idle timeout

```bash
./dist/MiraProt-launcher --idle-timeout 1
```
Open the browser, then close the tab. The launcher should shut down after
approximately 1 minute of no active connections.

### 6. Stop-on-close

Start normally and close the browser tab. The launcher should detect the
disconnection and shut down automatically within a few seconds.

### 7. Headless mode

```bash
./dist/MiraProt-launcher --no-tray --no-browser
```
The launcher should start without opening a browser or showing a tray icon.
Useful for server or CI environments.

### 8. Log files

Check that log files are created at the platform-specific data directory:

| OS | Log directory |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt\logs\` |
| macOS | `~/Library/Application Support/MiraProt/logs/` |
| Linux | `~/.local/share/MiraProt/logs/` |

Log files are named `miraprot-YYYY-MM-DD.log` and are automatically cleaned
up after 7 days.

---

## 7. Creating Installers

After bundling into `dist/`, you can create platform-specific installers.

### Windows — Inno Setup

```powershell
& "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" `
  /DAppVersion="1.0.0" `
  portable\installers\windows\MiraProt.iss
```

This creates `output/MiraProt-1.0.0-windows-setup.exe`. The installer:
- Installs to `C:\Program Files\MiraProt` by default (user can change)
- Optionally creates a desktop shortcut
- Does **not** require admin rights (`PrivilegesRequired=lowest`)
- Includes an uninstaller

### macOS — DMG

```bash
bash portable/installers/macos/create-dmg.sh \
  --dist-dir dist \
  --version 1.0.0 \
  --output-dir output
```

This creates `output/MiraProt-1.0.0-macos-<arch>.dmg` containing a `.app`
bundle. The script:
1. Creates a `MiraProt.app` bundle with the standard macOS structure
2. Places the launcher in `Contents/MacOS/`
3. Places R, packages, and the Shiny app in `Contents/Resources/`
4. Substitutes the version into `Info.plist`
5. Packages everything into a compressed DMG with an Applications symlink

### Linux — AppImage

```bash
bash portable/installers/linux/create-appimage.sh \
  --dist-dir dist \
  --version 1.0.0 \
  --output-dir output
```

This creates `output/MiraProt-1.0.0-linux-<arch>.AppImage`. The script:
1. Downloads `appimagetool` automatically if not in PATH
2. Creates an AppDir with the launcher, app, R, and packages
3. Generates `AppRun`, `.desktop`, and icon files
4. Packages everything into a single executable AppImage

---

## 8. CI/CD: Automated Builds and Releases

The GitHub Actions workflow `.github/workflows/portable-build.yml` automates
building and releasing the portable edition.

### Triggers

| Trigger | When |
|---|---|
| Tag push | Push a tag matching `v*` (e.g., `v1.0.0`, `v1.0.0-beta.1`) |
| Manual dispatch | Click "Run workflow" in the GitHub Actions UI |

### Build matrix

The workflow builds on 4 platforms in parallel:

| Runner | Output name | Archive format |
|---|---|---|
| `ubuntu-latest` | `miraprot-linux-amd64` | `.tar.gz` |
| `macos-13` | `miraprot-macos-amd64` | `.tar.gz` |
| `macos-14` | `miraprot-macos-arm64` | `.tar.gz` |
| `windows-latest` | `miraprot-windows-amd64` | `.zip` |

### Build steps (per platform)

1. Checkout repository (full history for `git describe`)
2. Setup Go 1.22 and R 4.6.0
3. Install system dependencies (Linux only)
4. Restore R library cache (keyed by `install-packages.R` hash)
5. Install R packages (skipped on cache hit)
6. Copy portable R from the runner's system installation
7. Copy Shiny application
8. Build Go launcher with version from tag
9. Smoke test: `MiraProt-launcher --version`
10. Create archive (`.tar.gz` or `.zip`)
11. Build platform installer (Inno Setup / DMG / AppImage)
12. Upload artifacts

### R library caching

R package installation (~30-60 min) is cached across builds. The cache key
includes the OS, architecture, R version, and a hash of `install-packages.R`.
If you change the package list, the cache is invalidated and packages are
reinstalled.

### Creating a release

```bash
git tag v1.0.0
git push origin v1.0.0
```

The `release` job runs after all 4 builds succeed. It:
1. Downloads all build artifacts
2. Creates a GitHub Release with auto-generated release notes
3. Uploads all archives (`.tar.gz`, `.zip`) and installers (`.exe`, `.dmg`,
   `.AppImage`) as release assets

### Manual dispatch

To trigger a build without creating a tag:
1. Go to **Actions** > **Build Portable Desktop** in GitHub
2. Click **Run workflow**
3. Optionally change the R version (default: 4.6.0)
4. Click **Run workflow**

Build artifacts are available for download from the workflow run page for 30
days.

---

## 9. Launcher CLI Reference

| Flag | Type | Default | Description |
|---|---|---|---|
| `--port` | int | 3838 | Preferred TCP port for the Shiny server. If in use, the launcher scans upward to port 4838. |
| `--app-dir` | string | (auto-detect) | Path to the Shiny application directory. Default: `shiny-app/` next to the launcher binary. |
| `--r-home` | string | (auto-detect) | Path to the portable R installation. Default: `r-portable/` next to the launcher binary. |
| `--debug` | bool | false | Enable verbose logging to console and log file. |
| `--version` | bool | false | Print version string and exit. |
| `--no-browser` | bool | false | Do not open the system browser automatically. |
| `--no-tray` | bool | false | Do not show the system tray icon (headless mode). |
| `--idle-timeout` | int | 0 | Minutes of inactivity before auto-shutdown. 0 means disabled. |

### Environment variables set for R

The launcher sets these environment variables before starting the R subprocess:

| Variable | Value | Purpose |
|---|---|---|
| `R_LIBS_USER` | `<exe-dir>/r-library` | Tells R where the bundled packages are |
| `MIRAPROT_IN_PORTABLE` | `true` | Lets the app detect portable mode |
| `MIRAPROT_PORT` | `<port>` | The actual port being used |
| `MIRAPROT_GO_CACHE` | `<datadir>/cache/go_cache` | Gene Ontology cache directory |
| `ANNOTATION_HUB_CACHE` | `<datadir>/cache/annotation_cache` | AnnotationHub cache directory |
| `MIRAPROT_LOG_DIR` | `<datadir>/logs` | Log file directory |

Where `<exe-dir>` is the directory containing the launcher binary and
`<datadir>` is the platform-specific application data directory (see
[section 6, log files](#6-testing-the-build)).

---

## 10. Architecture Overview

### Startup flow

```
User double-clicks launcher
  → Find a free port (3838-4838)
  → Acquire single-instance lock file
  → Validate paths (app.R, Rscript)
  → Start R subprocess: Rscript --vanilla -e "shiny::runApp(...)"
  → Poll http://127.0.0.1:<port>/__health until ready (timeout: 2 min)
  → Open default browser
  → Start idle monitor (if --idle-timeout > 0)
  → Show system tray icon (unless --no-tray)
  → Wait for shutdown signal (Ctrl+C, tray Quit, R crash, or idle timeout)
  → Send SIGINT to R (or taskkill on Windows)
  → Wait up to 5 seconds for graceful exit, then force-kill
  → Release lock file and exit
```

### Path auto-detection

The launcher looks for `shiny-app/` and `r-portable/` relative to its own
binary location. On macOS `.app` bundles, it also checks
`../Resources/app` and `../Resources/R` (the standard bundle layout).

### Single-instance lock

A PID lock file at `<datadir>/launcher.lock` prevents multiple instances from
running simultaneously. If the launcher finds an existing lock, it checks
whether the recorded PID is still alive. Stale locks from crashed processes
are automatically cleaned up.

### Update checking

On startup, the launcher queries the GitHub Releases API for the latest tag.
If a newer version is available, it prints a message to the console with a
download link. This check is non-blocking and does not affect startup time.

---

## 11. Troubleshooting

### "Rscript not found at ..."

The launcher cannot find the R binary. Check that:
- `r-portable/bin/Rscript` (or `Rscript.exe`) exists in the distribution
- If using `--r-home`, the path is correct

### "app.R not found at ..."

The launcher cannot find the Shiny application. Check that:
- `shiny-app/app.R` exists in the distribution
- If using `--app-dir`, the path is correct

### "another MiraProt instance is already running"

Delete the lock file and try again:

| OS | Lock file |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt\launcher.lock` |
| macOS | `~/Library/Application Support/MiraProt/launcher.lock` |
| Linux | `~/.local/share/MiraProt/launcher.lock` |

### Port conflict

If port 3838 is in use, the launcher automatically scans ports 3839-4838. If
all are taken, specify a different port:
```bash
./MiraProt-launcher --port 5000
```

### System tray icon does not appear (Linux)

Install the GTK 3 and AppIndicator development packages:
```bash
sudo apt-get install -y libgtk-3-dev libayatana-appindicator3-dev
```

If you cannot install these, build with `--tags=notray` or run with
`--no-tray`. The launcher works the same way without the tray icon.

### R package installation fails

- **Linux**: Make sure all system libraries from [section 1](#1-prerequisites)
  are installed.
- **All platforms**: Check that you have an internet connection (packages are
  downloaded from CRAN and Bioconductor).
- Check the R output for the specific package that failed and look for
  missing system dependencies in the error message.

### Go build fails with "CGO not enabled"

On Linux, the system tray requires CGO. Install `gcc` and the GTK development
headers (see [section 1](#1-prerequisites)). If you do not need the system
tray, build with `-tags=notray` and `CGO_ENABLED=0`.

### Bundler fails: "R not found on this system"

Install R 4.6.0 before running the bundler script. The script looks for
`Rscript` in your PATH. On Linux, you can install via `apt install r-base` or
use [rig](https://github.com/r-lib/rig). On macOS, use `brew install r` or
rig.

## Test/check workflow for session save/restore changes

Run the fastest static checks before any full app startup or heavier restore smoke
tests. CI mirrors this order so parse warnings/errors fail early.

Required real-R parse checks:

```bash
Rscript -e 'parse(file="R/session_save_restore/session_save_restore_orchestration.R")'
Rscript -e 'parse(file="R/session_save_restore/session_save_restore_core_helpers.R")'
Rscript -e 'parse(file="R/session_save_restore/session_save_restore_module_registration.R")'
Rscript -e 'parse(file="R/session_save_restore.R")'
```

Then run the lightweight repository checks:

```bash
git diff --check
python3 scripts/check-delimiter-quote-balance.py
python3 tests/static/test_heatmap_download_handler_source.py
```

Agents or developer environments without `Rscript` must explicitly report that
limitation in their results, but they should still run `git diff --check` and
`python3 scripts/check-delimiter-quote-balance.py` before proceeding.
