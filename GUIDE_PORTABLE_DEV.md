# MiraProt Standalone Edition — Developer Guide

This guide explains how to build and test the portable desktop edition of
MiraProt from source. It covers compiling the Go launcher, bundling R and all
packages, creating optional platform-specific packages, and using CI builds.
MiraProt's authoritative distribution is the source repository; generated
portable binaries are not normally published as GitHub Release assets.

---

## Table of Contents

1. [Prerequisites](#1-prerequisites)
2. [Project Structure](#2-project-structure)
3. [Building the Go Launcher](#3-building-the-go-launcher)
4. [Bundling a Portable Distribution (Linux/macOS)](#4-bundling-a-portable-distribution-linuxmacos)
5. [Bundling a Portable Distribution (Windows)](#5-bundling-a-portable-distribution-windows)
6. [Testing the Build](#6-testing-the-build)
7. [Creating Optional Local Packages](#7-creating-optional-local-packages)
8. [Build Artifact Types and CI](#8-build-artifact-types-and-ci)
9. [Launcher CLI Reference](#9-launcher-cli-reference)
10. [Architecture Overview](#10-architecture-overview)
11. [Troubleshooting](#11-troubleshooting)

---

## 1. Prerequisites

### All platforms

| Tool | Version | Purpose |
|---|---|---|
| **Go** | 1.22 or later | Compile the launcher binary |
| **R** | 4.5.2 | Bundled R runtime and package compilation |
| **Git** | Any recent version | Version tagging and CI/CD |

**Install Go:**

| OS | Command |
|---|---|
| Windows | Download the installer from https://go.dev/dl/ |
| macOS | `brew install go` |
| Linux | `sudo snap install go --classic` or download from https://go.dev/dl/ |

Verify: `go version` should print `go1.22` or later.

**Install R 4.5.2:**

| OS | Command |
|---|---|
| Windows | Download from https://cran.r-project.org/bin/windows/base/ |
| macOS | `brew install r` or use [rig](https://github.com/r-lib/rig): `rig add 4.5.2` |
| Linux | `sudo apt install r-base` or use rig: `rig add 4.5.2` |

Verify: `Rscript --version` must report exactly `4.5.2` for the default portable build. The maintained default lives in `portable/R_VERSION`.

### Linux only (Ubuntu/Debian-family local builds)

Ubuntu/Debian-family Linux is the currently implemented local-build path. The
dependency-installation block below uses `apt-get` package names and `dpkg`;
the Linux target exercised by CI is specifically Ubuntu on amd64. Do not
translate this block into unverified package-manager commands: Fedora/RHEL-family,
Arch-family, and openSUSE are **not supported by the current
dependency-installation block**. In particular, do not publish `dnf`, `pacman`,
or `zypper` commands until their package mappings and complete builds have been
implemented and verified.

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
- **Git** for a Git checkout. Git is optional for an extracted source archive;
  the bundler records `unknown` values in `BUILD_INFO` when `.git` metadata is
  absent.
- **Internet access** to CRAN and the Go module proxy/GitHub for R, packages,
  and the pinned launcher-resource helper
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
C toolchain and GTK headers, which is complex. Use a native machine or the CI
build matrix instead (see [section 8](#8-build-artifact-types-and-ci)).

---

## 4. Bundling a Portable Distribution (Linux/macOS)

Linux and macOS currently require the requested, matching native R to be
preinstalled and selected on `PATH`; the script validates the exact version
before copying it. On Linux, Ubuntu/Debian-family systems are the currently
implemented local-build path, while Ubuntu amd64 is the CI-tested Linux target.
The `bundle-r.sh` script performs basic bundling: it creates a flat, relocatable
distribution directory containing a native launcher, a copy of R, all R
packages, and the Shiny application. It does not create a macOS `.app` or DMG;
that optional packaging step is described separately in section 7. Linux output
is relocatable within compatible systems, not completely self-contained.

On macOS, the copied R runtime, compiled R packages, Go launcher, and optional
packaging host must have compatible native architectures. CI provides evidence
for Intel (`x86_64`) on `macos-13` and Apple Silicon (`arm64`) on `macos-14`.
No universal binary is assembled, and Rosetta operation is not supported by a
test claim; do not treat translation as permission to combine architectures.

### Running the bundler

```bash
cd /path/to/MiraProt
chmod +x portable/scripts/bundle-r.sh
./portable/scripts/bundle-r.sh --r-version 4.5.2 --output-dir portable/dist
```

The command-line options override the `R_VERSION` and `OUTPUT_DIR` environment
variables. If an option is omitted, its environment variable is used; if that
is also unset, the R version comes from `portable/R_VERSION` and the output is
`portable/dist`. The default output is resolved relative to the script, so it
is `portable/dist` regardless of the working directory from which the script
is invoked.

Or use the documented environment-variable fallbacks:
```bash
R_VERSION=4.5.2 OUTPUT_DIR=portable/dist ./portable/scripts/bundle-r.sh
```

### What it does (step by step)

1. **Copies R** — Copies your system R installation into `dist/r-portable/`.
2. **Installs system dependencies** (Linux only) — Checks for and installs
   eight missing development packages via `apt-get`:
   `libfreetype6-dev`, `libfontconfig1-dev`, `libharfbuzz-dev`,
   `libfribidi-dev`, `libtiff5-dev`, `libjpeg-dev`, `libpng-dev`, and
   `librsvg2-dev`.
3. **Installs R packages** — Runs `install-packages.R` to install all ~97
   packages into `dist/r-library/`. This takes **30-60 minutes** on a fresh
   build. Packages are: 17 Bioconductor, 62 CRAN, 18 optional, 1 GitHub
   (shinyTree).
4. **Prebuilds and merges caches (optional)** — Attempts
   `prebuild-cache.R`, but warns and continues if AnnotationHub is unavailable;
   runtime download is the supported fallback on every platform. It then merges
   any project source caches as described in [Portable cache resolution and
   assembly](#portable-cache-resolution-and-assembly).
5. **Copies the Shiny application** — Uses `rsync` to copy the project
   (excluding `.git`, `cache/`, `portable/`, `scripts/`, `tests/`, etc.) into
   `dist/shiny-app/`. The top-level `scripts/` and `tests/` directories remain
   in the source checkout for development and pre-package validation, but are
   omitted from the generated runtime distribution.
6. **Builds the Go launcher** — Compiles the launcher into `dist/MiraProt-launcher`.

The CI workflow adds five development libraries beyond the bundler's eight:
`libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libgtk-3-dev`, and
`libayatana-appindicator3-dev`. These support native R-package networking/XML
dependencies and the tray-enabled launcher. This inventory describes the
Ubuntu/Debian package mapping only; Fedora/RHEL-family, Arch-family, and
openSUSE are not supported by the current dependency-installation block.

Copying R and native R packages does not remove their dynamic-library
requirements. They can retain dependencies on glibc, libstdc++, OpenSSL,
libcurl, libxml2, font and graphics libraries, and desktop-integration
libraries such as GTK and AppIndicator. Build Linux artifacts on the oldest
compatible target distribution you intend to support, then validate native
dependencies before release with platform tools such as `ldd`, `readelf`, and
the target package manager's ownership/query commands. Test the resulting
bundle on each claimed target system and architecture.

### Output directory structure

After bundling, `dist/` contains:
```
dist/
├── MiraProt-launcher     # Go binary (or .exe on Windows)
├── shiny-app/             # The MiraProt R application
├── r-portable/            # Portable R installation
├── r-library/             # Pre-compiled R packages
└── go-cache/              # Optional prebuilt/seeded runtime cache
```

`dist/` (or `portable/dist/` when the default is used) is a **local build
artifact**. Generated bundle directories are normally ignored by Git and must
not be committed. A completed artifact can be launched repeatedly without
rerunning the bundler. Rebuild it when installing newer source or deliberately
changing the bundled environment (R, packages, application files, or seeded
caches), rather than before every launch.

---

## 5. Bundling a Portable Distribution (Windows)

The `bundle-r-windows.ps1` script does the same thing as `bundle-r.sh` but for
Windows, using PowerShell. Unlike Linux/macOS, it downloads R automatically. It
checks CRAN's current installer location and then the versioned archive, and
clearly fails if neither contains the requested release.

Before doing build work, the script verifies that Go is on `PATH`, that Git is
available when `.git` metadata is present, that CRAN is reachable, and that the
output directory can be created and written. An extracted source archive does
not need Git: its `BUILD_INFO` uses `unknown` commit fields and launcher version
`dev`.

### Running the bundler

```powershell
cd C:\path\to\MiraProt
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.5.2" -OutputDir ".\dist"
```

### What it does (step by step)

1. **Downloads R** — Downloads the R 4.5.2 installer from CRAN (the default
   comes from `portable/R_VERSION`).
   (`R-4.5.2-win.exe`) to your temp directory.
2. **Silent install** — Runs the R installer silently into `dist\r-portable\`.
3. **Installs R packages** — Same as Linux/macOS, ~30-60 minutes. Compatible
   Windows binaries are preferred when repositories offer them. Rtools is only
   required when a dependency must compile from source; note that
   `install-packages.R` may currently try to install Rtools automatically when
   it detects that build tools are missing.
4. **Copies the Shiny application** — Uses `robocopy` (built into Windows) to
   copy the project into `dist\shiny-app\`.
5. **Builds the Go launcher** — Compiles into `dist\MiraProt-launcher.exe`.

### Troubleshooting Windows launcher resources

Near the end of `portable\scripts\bundle-r-windows.ps1`, the resource-generation
block prepares the icon and Windows metadata before `go build`:

1. `go install github.com/tc-hib/go-winres@v0.3.3` installs the reviewed,
   pinned `go-winres` build helper.
2. `go run gen_ico.go` creates the `.ico` input from the launcher artwork.
3. `go-winres make` reads the resource configuration and generates
   `portable\launcher\rsrc_windows_amd64.syso`.
4. The subsequent `go build` automatically links that `.syso` file into the
   Windows launcher.

Use the following symptoms to locate failures:

- **`go-winres` installation/execution fails:** `go install` places the
  executable in `GOBIN`; when `GOBIN` is empty, it uses the first `GOPATH`
  entry's `bin` directory. The script queries `go env GOBIN`/`go env GOPATH`,
  constructs the full `go-winres.exe` path, and invokes that path directly, so
  the directory does not need to be on `PATH`. Review errors from `go install
  github.com/tc-hib/go-winres@v0.3.3` for network or toolchain failures.
- **Blocked-executable or application-control message:** Windows Smart App
  Control, or an organization-managed application-control policy, may prevent
  `go-winres.exe` from running. This blocks a build-time helper; it does not by
  itself show that the completed MiraProt application is incompatible. Do not
  disable Smart App Control or weaken organizational policy as the default
  workaround. Build on a trusted workstation or controlled VM, use GitHub
  Actions, or use signed/trusted build infrastructure approved by your
  organization.
- **Missing resource symptoms:** if `go-winres make` reports an icon,
  configuration, or output error, or if
  `portable\launcher\rsrc_windows_amd64.syso` is absent afterward, the resource
  step did not complete. Run `go run gen_ico.go` and then `go-winres make` from
  `portable\launcher`, address the first reported error, and verify the `.syso`
  exists before rerunning the build. A launcher that fails to compile because
  resources are missing, or one built manually without the resource step that
  lacks the expected icon/version metadata, points to this stage rather than
  to the bundled R application.

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

## 7. Creating Optional Local Packages

After bundling into `dist/`, a maintainer can create platform-specific packages
for local testing or explicitly approved distribution. Creating one of these
files does not publish it and does not make it an authoritative release asset.

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

This is optional packaging, separate from the basic flat directory produced by
`portable/scripts/bundle-r.sh`. Run it on a host whose native architecture is
compatible with the copied R runtime, compiled R packages, and Go launcher:

```bash
bash portable/installers/macos/create-dmg.sh \
  --dist-dir dist \
  --version 1.0.0 \
  --output-dir output
```

This creates `output/MiraProt-1.0.0-macos-<uname-m>.dmg` (generally
`MiraProt-<version>-macos-<uname-m>.dmg`) containing `MiraProt.app`. The script:
1. Creates a `MiraProt.app` bundle with the standard macOS structure
2. Places the launcher in `Contents/MacOS/`
3. Places R, packages, and the Shiny app in `Contents/Resources/`
4. Substitutes the version into `Info.plist`
5. Packages everything into a compressed DMG with an Applications symlink

The script does not combine Intel and Apple Silicon code into a universal
binary. The matrix's `macos-13` Intel and `macos-14` Apple Silicon jobs are the
CI evidence for the two separate native outputs; they are not evidence of
Rosetta compatibility.

The locally created app is unsigned and unnotarized. Consequently Gatekeeper
or a downloaded file's quarantine attribute can produce a warning, refusal, or
explicit-open prompt at runtime. Diagnose this separately from compilation:
successful R-package and Go build logs establish compilation, while `codesign
--verify --deep --strict /path/to/MiraProt.app`, `spctl --assess --type execute
--verbose=4 /path/to/MiraProt.app`, and `xattr -p com.apple.quarantine
/path/to/MiraProt.app` inspect signature/policy/quarantine state. Signature and
policy assessment failure is expected for an unsigned local app.

Distributed builds should sign nested code and the app with the appropriate
Developer ID, then be notarized and stapled. For a build whose source and
contents have been independently verified and trusted locally, use Finder's
Control-click **Open** flow. If quarantine still prevents opening, remove it
only from that app (`xattr -dr com.apple.quarantine /path/to/MiraProt.app`) and
retry. The same targeted inspection can be applied to the native launcher in
the flat directory. Never advise or require disabling Gatekeeper globally.

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

## 8. Build Artifact Types and CI

Do not use “artifact” and “release asset” interchangeably. MiraProt has three
distinct output categories:

| Output | Created by | Lifetime and visibility | Distribution status |
|---|---|---|---|
| **Local build artifact** | A developer runs a bundler into `dist/` or `portable/dist/` | Remains on that machine until removed; normally ignored by Git | Normal portable workflow; launch it locally as often as needed |
| **Workflow artifact** | `.github/workflows/portable-build.yml` uploads a matrix build | Attached to an Actions run for 30 days; accessible according to repository/Actions permissions | Temporary CI validation or maintainer handoff, not a public release |
| **GitHub Release asset** | A maintainer explicitly publishes a file on a GitHub Release | Public and persistent until the release/asset is changed or deleted | Not produced by the current workflow and not the authoritative distribution path |

The GitHub Actions workflow `.github/workflows/portable-build.yml` automates
building and testing the portable edition. It deliberately stops after
uploading 30-day workflow artifacts: it does not call `gh release create`.

### Triggers

| Trigger | When |
|---|---|
| Tag push | Push a tag matching `v*`; builds temporary workflow artifacts but does not publish a Release |
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
2. Setup Go 1.22 and R 4.5.2
3. Install system dependencies (Linux only)
4. Restore R library cache (keyed by `install-packages.R` hash)
5. Install R packages (skipped on cache hit)
6. Copy portable R from the runner's system installation
7. Copy Shiny application
8. Build Go launcher with version from tag
9. Smoke test: `MiraProt-launcher --version`
10. Create archive (`.tar.gz` or `.zip`)
11. Build platform installer (Inno Setup / DMG / AppImage)
12. Upload 30-day workflow artifacts

### R library caching

R package installation (~30-60 min) is cached across builds. The cache key
includes the OS, architecture, R version, and a hash of `install-packages.R`.
If you change the package list, the cache is invalidated and packages are
reinstalled.

### Source distribution policy

Tags may be pushed to validate a version across the build matrix, but a `v*`
tag does **not** publish the resulting binaries. GitHub can still expose its
automatically generated source archives for a tag; those are source snapshots,
not MiraProt portable binary assets.

Publishing portable binaries as GitHub Release assets is a separate policy
decision. If the project owner later approves public binary distribution, that
decision must include provenance, support, retention, signing, and update
expectations. Only then should an explicit, permission-gated publishing job be
added and this guide and the user guide updated together.

### Manual dispatch

To trigger a build without creating a tag:
1. Go to **Actions** > **Build Portable Desktop** in GitHub
2. Click **Run workflow**
3. Optionally override the R version (otherwise the workflow reads
   `portable/R_VERSION`)
4. Click **Run workflow**

Workflow artifacts are available from the workflow run page for 30 days. They
are temporary CI outputs, not GitHub Release assets.

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
| `MIRAPROT_GO_CACHE` | `<writable-cache>/go_cache` | Gene Ontology cache directory |
| `ANNOTATION_HUB_CACHE` | `<writable-cache>/annotation_cache` | AnnotationHub cache directory |
| `MIRAPROT_LOG_DIR` | `<datadir>/logs` | Log file directory |

Where `<exe-dir>` is the directory containing the launcher binary and
`<datadir>` is the platform-specific application data directory (see
[section 6, log files](#6-testing-the-build)).

#### Portable cache layouts

Cache behavior differs by distribution layout:

- **Flat portable directory:** `go-cache/` sits directly beside the launcher.
  It remains the writable runtime cache, preserving the self-contained portable
  behavior. `MIRAPROT_GO_CACHE` and `ANNOTATION_HUB_CACHE` point to its
  `go_cache/` and `annotation_cache/` subdirectories.
- **macOS app and Linux AppImage:** the packaged `go-cache/` is immutable seed
  data (`Contents/Resources/go-cache` in the app bundle and `usr/go-cache` in
  the AppImage). On first launch, the launcher copies each shipped cache only
  when its destination is empty. Runtime writes and both cache environment
  variables use `<datadir>/cache/`; existing user caches are never overwritten.

This separation is required for AppImage's read-only mount and also avoids
modifying installed application resources on macOS.

#### Portable cache resolution and assembly

The launcher resolves cache paths in `portable/launcher/config.go`:

1. `CacheDir(name)` asks `goCacheRoot()` for an existing `go-cache/` beside
   the executable. A flat Windows, Linux, or macOS distribution uses that
   directory directly and creates the requested `name` subdirectory.
2. If no adjacent cache exists but `packagedGoCacheRoot()` detects AppImage
   `usr/go-cache` or macOS `Contents/Resources/go-cache`, `CacheDir()` chooses
   `<datadir>/cache/<name>` instead. Packaged resources are seed data, never a
   writable runtime target.
3. Otherwise `createGoCacheRoot()` attempts to create `go-cache/` beside the
   executable, preserving flat-bundle portability. This means the directory
   containing a flat launcher must be writable. Only if that creation/path
   resolution fails does `CacheDir()` finally fall back to
   `<datadir>/cache/<name>`.

`ShippedCacheDir(name)` finds a named cache either beside the launcher or in a
packaged resource root. Before R starts, `SeedCache()` handles both
`annotation_cache` and `go_cache`. It does nothing when no shipped cache exists,
uses an adjacent flat cache in place, and copies packaged seed contents to the
application-data target only when that target is empty. It never overwrites a
user's existing cache. Logs (`<datadir>/logs`) and the single-instance lock
(`<datadir>/launcher.lock`) always remain in application data and are not part
of `go-cache/`.

During assembly, `prebuild-cache.R` attempts to download the AnnotationHub
index, the default `org.Hs.eg.db` resource, and derived organism metadata into
`go-cache/annotation_cache` and `go-cache/go_cache`. This prebuild is an
optimization, **not a required build step**: the Unix bundler, Windows bundler,
and CI all warn and continue on failure because runtime download is supported.
Afterward the bundlers/CI merge source `cache/GO_Cache/` into
`go-cache/go_cache/`; source files win over same-named prebuilt files. They do
not combine nested organism `ah_cache` databases with the top-level
`annotation_cache`, because independent BiocFileCache indexes cannot safely be
merged. Source `cache/BioMart_Cache/` is placed at
`go-cache/go_cache/BioMart_Cache/`, matching BioMart's portable-mode path under
`MIRAPROT_GO_CACHE`.

Consequently a flat archive and the Windows installed/flat layout read and
write the adjacent cache. A DMG-created macOS app and an AppImage ship the same
data as immutable resources and seed per-user application data on first launch.
If assembly supplied no cache, seeding is simply skipped: the first
AnnotationHub or organism operation downloads into the resolved writable
directory, so it is slower and requires network access, while subsequent uses
reuse it. BioMart/STRING operations retain their own online-service needs.

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
If that tag is newer than the running semantic version, it prints a message
directing the user to obtain the newer source and rebuild the portable
installation. The authoritative distribution is source-only: the launcher
performs no in-place update and does not download or install software
automatically. This check is non-blocking and does not affect startup time.

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

Install exactly R 4.5.2 before running the Linux/macOS bundler script. The script looks for
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
