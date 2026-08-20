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
7. [Stage 2: Creating Optional Local Packages](#7-stage-2-creating-optional-local-packages)
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
| Windows | The Windows bundler downloads the requested R runtime automatically |
| macOS | `brew install r` or use [rig](https://github.com/r-lib/rig): `rig add 4.5.2` |
| Linux | `sudo apt install r-base` or use rig: `rig add 4.5.2` |

For Linux/macOS, `Rscript --version` must report exactly `4.5.2` for the default
portable build. The maintained default lives in `portable/R_VERSION`. Windows
does not require a separately installed system R for the normal bundler path.

### Linux only (Ubuntu/Debian-family local builds)

Ubuntu/Debian-family Linux is the currently implemented local-build path. The
dependency-installation block below uses `apt-get` package names and `dpkg`;
the Linux target exercised by CI is specifically Ubuntu on amd64. Do not
translate this block into unverified package-manager commands:
Fedora/RHEL-family, Arch-family, and openSUSE are **not supported by the
current dependency-installation block**. In particular, do not publish `dnf`,
`pacman`, or `zypper` commands until their package mappings and complete builds
have been implemented and verified.

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

- **PowerShell 5.1+** (built into Windows 10/11); PowerShell 7 is recommended.
- **Git** for a Git checkout. Git is optional for an extracted source archive;
  the bundler records `unknown` values in `BUILD_INFO` when `.git` metadata is
  absent.
- **Internet access** to CRAN and the Go module proxy/GitHub for R, packages,
  and the pinned launcher-resource helper.
- **Inno Setup 6** only for the optional Windows installer packaging stage. It
  is not used to build the basic portable bundle.

Install Inno Setup 6 interactively with `winget`:

```powershell
winget install --id JRSoftware.InnoSetup -e -s winget -i
```

Inno Setup can install per-user or machine-wide. A common per-user compiler
path is:

```text
%LOCALAPPDATA%\Programs\Inno Setup 6\ISCC.exe
```

A common machine-wide path is:

```text
C:\Program Files (x86)\Inno Setup 6\ISCC.exe
```

The exact compiler path should therefore be discovered rather than assumed.

---

## 2. Project Structure

### Independent version domains

MiraProt's application version comes from Git history/build metadata and
`R/version_info.R`; portable artifacts copy the equivalent values into
`BUILD_INFO`. This application identity is independent of the bundled R
runtime, Go launcher version, platform-installer version, and saved-session
schema version. Do not synchronize or infer one of these versions from another.
In particular, bundler `-RVersion`/`--r-version` values select only R.

All portable-edition files live under the `portable/` directory:

```text
portable/
├── launcher/
│   ├── main.go
│   ├── config.go
│   ├── rprocess.go
│   ├── health.go
│   ├── browser.go
│   ├── portfinder.go
│   ├── lockfile.go
│   ├── idle.go
│   ├── logger.go
│   ├── update.go
│   ├── tray_enabled.go
│   ├── tray_disabled.go
│   ├── process_unix.go
│   ├── process_windows.go
│   ├── icon_data_windows.go
│   ├── icon_data_nonwindows.go
│   ├── gen_ico.go
│   ├── MiraProt.ico
│   ├── go.mod
│   └── go.sum
│
├── scripts/
│   ├── bundle-r.sh
│   ├── bundle-r-windows.ps1
│   ├── install-packages.R
│   └── prebuild-cache.R
│
├── installers/
│   ├── windows/
│   │   └── MiraProt.iss
│   ├── macos/
│   │   ├── create-dmg.sh
│   │   └── Info.plist
│   └── linux/
│       └── create-appimage.sh
│
└── resources/
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
Set-Location .\portable\launcher; go build -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher.exe .
```

Verify:

```bash
./MiraProt-launcher --version
```

Expected:

```text
MiraProt Launcher dev (linux/amd64)
```

### What the flags mean

| Flag | Purpose |
|---|---|
| `-s -w` | Strip debug symbols and DWARF info (smaller binary) |
| `-X main.Version=dev` | Embed the version string at compile time |

### Build tags

The launcher includes system tray support by default. To build **without** the
system tray:

```bash
go build -tags=notray -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher .
```

### CGO requirements

| OS | CGO_ENABLED | Why |
|---|---|---|
| Linux | `1` (default) | Required by the system tray library (`libgtk-3`, `libayatana-appindicator3`) |
| macOS | `0` | System tray uses native Cocoa APIs via cgo-free bindings |
| Windows | `0` | System tray uses native Win32 APIs via cgo-free bindings |

If you build on Linux without the system tray:

```bash
CGO_ENABLED=0 go build -tags=notray -ldflags "-s -w" -o MiraProt-launcher .
```

### Cross-compilation

To build for a different platform without the system tray:

```bash
GOOS=windows GOARCH=amd64 CGO_ENABLED=0 go build -tags=notray -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher.exe .
```

Cross-compiling **with** the system tray on Linux requires the target platform's
C toolchain and GTK headers, which is complex. Use a native machine or the CI
build matrix instead.

---

## 4. Bundling a Portable Distribution (Linux/macOS)

This is **stage 1: basic bundling**. Complete and test this stage before using
any optional platform packager in section 7.

Linux and macOS currently require the requested, matching native R to be
preinstalled and selected on `PATH`; the script validates the exact version
before copying it.

On Linux, Ubuntu/Debian-family systems are the currently implemented local-build
path, while Ubuntu amd64 is the CI-tested Linux target.

The `bundle-r.sh` script performs basic bundling: it creates a flat, relocatable
distribution directory containing a native launcher, a copy of R, all R
packages, the Shiny application, and optional seeded caches. It does not create
a macOS `.app` or DMG; that optional packaging step is described separately in
section 7.

Linux output is relocatable within compatible systems, not completely
self-contained.

On macOS, the copied R runtime, compiled R packages, Go launcher, and optional
packaging host must have compatible native architectures. CI provides evidence
for Intel (`x86_64`) on `macos-13` and Apple Silicon (`arm64`) on `macos-14`.
No universal binary is assembled, and Rosetta operation is not supported by a
test claim; do not treat translation as permission to combine architectures.

### Running the bundler

```bash
cd /path/to/MiraProt
```

```bash
chmod +x portable/scripts/bundle-r.sh
```

```bash
./portable/scripts/bundle-r.sh --output-dir portable/dist
```

`--r-version` selects the R runtime, not the MiraProt application version.
Ordinary builds should omit it and use the maintained `portable/R_VERSION`
default.

The command-line options override the `R_VERSION` and `OUTPUT_DIR` environment
variables. If an option is omitted, its environment variable is used; if that
is also unset, the R version comes from `portable/R_VERSION` and the output is
`portable/dist`.

The default output is resolved relative to the script, so it is
`portable/dist` regardless of the working directory from which the script is
invoked.

Or use the environment-variable fallbacks:

```bash
R_VERSION="$(cat portable/R_VERSION)" OUTPUT_DIR=portable/dist ./portable/scripts/bundle-r.sh
```

### What it does

#### Shiny application payload manifest

Every basic bundler and the CI workflow assemble `shiny-app/` from the same
runtime allowlist rather than copying the repository and maintaining a growing
exclusion list:

- `app.R`;
- `R/`;
- `modules/`;
- `Documentation/*.R`;
- `AutoAssign/`;
- `GSEA/`;
- `MiraProt_icon.png`;
- generated `BUILD_INFO`.

Everything else is excluded unless runtime code begins to require it.

In particular, top-level `scripts/`, `tests/`, `benchmarks/`, repository guides,
Git/build metadata, user data, and the `portable/` tooling itself are
development or build inputs, not application payload.

An allowlist is also the recursion guard. Output commonly lives below the
source tree (`dist/` or `portable/dist/`); copying only the named runtime paths
means neither that output directory nor an older bundle nested inside it can
be copied into `shiny-app/`.

Keep the manifest synchronized in:

- `portable/scripts/bundle-r.sh`;
- `portable/scripts/bundle-r-windows.ps1`;
- `.github/workflows/portable-build.yml`.

Current high-level sequence:

1. copy/validate R;
2. install required packages;
3. seed available project GO/BioMart/AnnotationHub cache data;
4. normalize and validate the portable cache;
5. use network fallback only for required missing bootstrap data;
6. copy the Shiny application payload;
7. build the Go launcher.

Cache prebuild is optional. Failure warns and continues because runtime download
is supported.

The CI workflow adds five development libraries beyond the bundler's basic
Linux checks:

- `libcurl4-openssl-dev`;
- `libssl-dev`;
- `libxml2-dev`;
- `libgtk-3-dev`;
- `libayatana-appindicator3-dev`.

Copying R and native R packages does not remove their dynamic-library
requirements. They can retain dependencies on glibc, libstdc++, OpenSSL,
libcurl, libxml2, font and graphics libraries, and desktop-integration
libraries such as GTK and AppIndicator.

Build Linux artifacts on the oldest compatible target distribution you intend
to support, then validate native dependencies before release with platform tools
such as `ldd`, `readelf`, and the target package manager's ownership/query
commands.

### Output directory structure

After bundling, the output contains approximately:

```text
dist/
├── MiraProt-launcher
├── shiny-app/
├── r-portable/
├── r-library/
├── go-cache/
├── LICENSE.md
├── README.md
├── THIRD_PARTY_NOTICES.md
└── citation.cff
```

The documentation and `go-cache/` are optional when their sources are
unavailable.

Generated bundle directories are normally ignored by Git and must not be
committed.

A completed artifact can be launched repeatedly without rerunning the bundler.
Rebuild it when installing newer source or deliberately changing the bundled
environment.

### GSEA GMT resource resolution

The runtime implementation is `gsea_list_gmt_files()` in
`modules/GSEA/GSEA_module_Gene_Sets.R`. Its default argument is `./GSEA`.

Because the launcher starts R with the packaged Shiny application as its
working directory, that relative path resolves as follows:

| Layout | Runtime GSEA directory |
|---|---|
| Source tree | `<repository>/GSEA/` |
| Flat Windows/Linux/macOS bundle | `<dist>/shiny-app/GSEA/` |
| Windows installer | `<installation>/shiny-app/GSEA/` |
| macOS `.app` | `MiraProt.app/Contents/Resources/app/GSEA/` |
| Linux AppImage contents | `usr/bin/shiny-app/GSEA/` |

Source mode and portable mode therefore do not share a live GMT directory.

When the application runs from the repository root, `./GSEA` is the source
tree's top-level `GSEA/`.

During a portable build, that directory is copied to `shiny-app/GSEA/`; the
portable launcher then sets `shiny-app/` or the packaged equivalent as R's
working directory.

Adding a GMT file to the source tree after assembly does not update an existing
portable artifact. Copy it to the artifact's applicable directory or rebuild
and repackage the artifact.

`gsea_list_gmt_files()` calls `list.files()` without `recursive = TRUE`, using
the pattern `\\.gmt$`. Only files directly inside the directory are returned;
subdirectories are not searched.

Use a lowercase `.gmt` suffix for consistent behavior on case-sensitive
filesystems.

Missing directories and directories with no matching files produce an empty
choice list rather than a startup error.

The helper caches each result by normalized directory path and directory
modification time in `.gsea_gmt_file_cache`. The GSEA observer initializes its
choices from that helper.

The **Refresh Gene Sets** control calls it with `force_refresh = TRUE`, rescans
immediately, updates the selector, and reports the number of files found. A
restart also creates a fresh process cache.

The repository ignores `GSEA/*.gmt`. The build does not download collections
from MSigDB.

---

## 5. Bundling a Portable Distribution (Windows)

This is **stage 1: basic bundling**. It produces a runnable directory, not an
installer.

Inno Setup is used only by the optional Windows-installer stage in section 7;
the PowerShell bundler neither requires nor invokes it.

The `bundle-r-windows.ps1` script performs the Windows stage-1 assembly. Unlike
Linux/macOS, it downloads R automatically. It checks CRAN's current installer
location and then the versioned archive, and clearly fails if neither contains
the requested release.

Before doing build work, the script verifies that Go is on `PATH`, that Git is
available when `.git` metadata is present, that CRAN is reachable, and that the
output directory can be created and written.

An extracted source archive does not need Git: its `BUILD_INFO` uses `unknown`
commit fields and launcher version `dev`.

### Running the bundler

```powershell
Set-Location C:\path\to\MiraProt
```

```powershell
.\portable\scripts\bundle-r-windows.ps1 -OutputDir ".\dist"
```

A custom output directory is supported, for example:

```powershell
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.6.1" -OutputDir "..\MiraProt_Portable"
```

`-RVersion` selects the R runtime to download and bundle; it does not set the
MiraProt application version.

Omit it for normal builds so `portable\R_VERSION` supplies the maintained
default.

### What it does

1. **Downloads and validates R** — downloads the selected Windows R runtime and
   verifies the installer before use.
2. **Safe staged install** — installs R into a unique short staging directory
   below `%TEMP%`, validates it, then safely promotes it.
3. **Installs R packages** — installs runtime dependencies into `r-library/`,
   preferring compatible Windows binaries where available.
4. **Seeds available project caches** — copies existing source
   `cache/GO_Cache/` and `cache/BioMart_Cache/` into the portable cache and may
   seed a complete human AnnotationHub cache.
5. **Normalizes/validates portable cache** — uses `prebuild-cache.R` to
   reconstruct canonical organism cache files from existing local cache data
   where possible and only falls back to network for required missing bootstrap
   pieces.
6. **Copies root documentation** — includes `LICENSE.md`, `README.md`,
   `THIRD_PARTY_NOTICES.md`, and `citation.cff` when present; missing
   documentation warns and is skipped.
7. **Copies the Shiny application** — uses the runtime allowlist to assemble
   `shiny-app/`.
8. **Builds the Go launcher** — generates Windows resources in a temporary
   launcher stage and compiles `MiraProt-launcher.exe`.

The staged runtime must contain:

```text
bin\R.exe
bin\Rscript.exe
bin\x64\R.dll
```

and must pass startup and runtime version probes before promotion.

It does not rely on a top-level `VERSION` file.

Absolute-path `R.exe --version` and `Rscript.exe --version` invocations are
startup probes only and are invoked natively by PowerShell.

For the authoritative exact version, the bundler writes `getRversion()` to a
temporary UTF-8 script and runs it as absolute-path
`Rscript.exe --vanilla <version-probe.R>`.

Inherited R configuration is removed for every probe, preventing a local R on
`PATH` from substituting for either staged executable.

If `r-portable` already exists it is moved to a unique backup; the validated
stage is promoted, validated again at its final path, and only then is the
backup removed.

A promotion failure restores the old runtime.

Failed staging and its separate installer/probe log directory are retained by
default and their exact paths are printed.

### Inspecting a failed Windows R stage

The failure message prints the retained `%TEMP%\MiraProt-R-...` stage and its
`...-logs` sibling.

Example diagnostic commands:

```powershell
$stage='C:\path\printed\by\the\bundler'; $logs="$stage-logs"; Get-Item "$stage\bin\R.exe","$stage\bin\Rscript.exe","$stage\bin\x64\R.dll" | Select FullName,Length,LastWriteTime,@{n='FileVersion';e={$_.VersionInfo.FileVersion}},@{n='ProductVersion';e={$_.VersionInfo.ProductVersion}}
```

```powershell
Get-ChildItem Env: | Where-Object Name -in 'R_HOME','R_ARCH','R_LIBS','R_LIBS_USER','R_LIBS_SITE','R_ENVIRON','R_ENVIRON_USER','R_PROFILE','R_PROFILE_USER'
```

```powershell
& "$stage\bin\R.exe" --version; $code=$LASTEXITCODE; '{0} (0x{1:X8})' -f $code,[uint32]$code
```

```powershell
& "$stage\bin\Rscript.exe" --version; $code=$LASTEXITCODE; '{0} (0x{1:X8})' -f $code,[uint32]$code
```

```powershell
$probe=Join-Path ([IO.Path]::GetTempPath()) ("miraprot-r-version-"+[guid]::NewGuid().ToString("N")+".R"); [IO.File]::WriteAllText($probe,"cat(as.character(getRversion()))`n",(New-Object Text.UTF8Encoding($false))); try { & "$stage\bin\Rscript.exe" --vanilla $probe; $code=$LASTEXITCODE; "`n$code (0x$('{0:X8}' -f [uint32]$code))" } finally { Remove-Item $probe -Force -ErrorAction SilentlyContinue }
```

```powershell
Get-ChildItem $logs; Get-Content "$logs\installer.log"; Get-Content "$logs\*probe*.log"
```

Do not manually copy a failed stage into `r-portable`. Fix the cause and rerun
so safe promotion and rollback remain active.

### Troubleshooting Windows launcher resources

Near the end of `portable\scripts\bundle-r-windows.ps1`, the resource-generation
block prepares the icon and Windows metadata before `go build`:

1. `go install github.com/tc-hib/go-winres@v0.3.3` installs the reviewed pinned
   `go-winres` helper.
2. Launcher sources are copied to a temporary directory.
3. `go run gen_ico.go -output-dir <temporary-launcher-directory>` creates the
   tray and executable icon inputs in that temporary stage.
4. `go-winres make` creates the staged Windows `.syso` resource.
5. `go build` links it into the final launcher.

Ordinary portable builds therefore do not rewrite the committed
`icon_data_windows.go`, `icon_data_nonwindows.go`, or `MiraProt.ico`.

---

## 6. Testing the Build

After bundling, run through these checks.

### 1. Version check

```bash
./dist/MiraProt-launcher --version
```

Should print:

```text
MiraProt Launcher <version> (<os>/<arch>)
```

### 2. Full startup with debug logging

```bash
./dist/MiraProt-launcher --debug
```

Watch for messages similar to:

```text
[LAUNCHER] MiraProt Launcher <version> starting
[LAUNCHER] R process started (PID ...)
[LAUNCHER] Waiting for Shiny server to start...
[LAUNCHER] Opening browser at http://127.0.0.1:3838
```

### 3. Browser and app

Open the reported local URL and verify:

- Data Wizard loads;
- analysis tabs are visible;
- a test file can be imported;
- modules can be switched.

### 4. System tray

Check that the MiraProt icon appears in the system tray and that its menu works.

### 5. Idle timeout

```bash
./dist/MiraProt-launcher --idle-timeout 1
```

### 6. Stop-on-close

Validate the expected shutdown behavior.

### 7. Headless mode

```bash
./dist/MiraProt-launcher --no-tray --no-browser
```

### 8. Log files

| OS | Log directory |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt\logs\` |
| macOS | `~/Library/Application Support/MiraProt/logs/` |
| Linux | `~/.local/share/MiraProt/logs/` |

Log files are named `miraprot-YYYY-MM-DD.log` and are automatically cleaned up
after 7 days.

---

## 7. Stage 2: Creating Optional Local Packages

After creating and testing the basic stage-1 bundle, a maintainer can run one
platform-specific stage-2 packager for local testing or explicitly approved
distribution.

These packagers consume the basic bundle; they do not replace or rebuild it.

Creating one of these files does not publish it and does not make it an
authoritative release asset.

### Windows — Inno Setup

The installer definition is:

```text
portable/installers/windows/MiraProt.iss
```

It consumes an already-built Windows portable directory and is compiled with
**Inno Setup 6**.

The installer does not:

- download R;
- install R packages;
- rebuild the launcher;
- rerun `prebuild-cache.R`;
- refresh BioMart;
- refresh AnnotationHub;
- rebuild GO caches.

#### Install Inno Setup 6

```powershell
winget install --id JRSoftware.InnoSetup -e -s winget -i
```

Inno Setup can be installed per-user or machine-wide.

Discover `ISCC.exe`:

```powershell
$iscc=@("$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe","C:\Program Files (x86)\Inno Setup 6\ISCC.exe","C:\Program Files\Inno Setup 6\ISCC.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1; $iscc; Test-Path $iscc
```

A normal per-user result can be:

```text
C:\Users\<user>\AppData\Local\Programs\Inno Setup 6\ISCC.exe
```

#### Select the stage-1 bundle

Use an absolute `DistDir`.

For repository-root `dist`:

```powershell
$dist=(Resolve-Path ".\dist").Path; $dist
```

For the user-guide default `portable\dist`:

```powershell
$dist=(Resolve-Path ".\portable\dist").Path; $dist
```

For a custom build such as `..\MiraProt_Portable`:

```powershell
$dist=(Resolve-Path "..\MiraProt_Portable").Path; $dist
```

Required stage-1 components are:

```text
MiraProt-launcher.exe
shiny-app\
r-portable\
r-library\
```

Verify them:

```powershell
@("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_);Path=Join-Path $dist $_} } | Format-Table -AutoSize
```

The Inno script also checks these at compile time and aborts with the selected
path if one is missing.

Optional stage-1 contents are:

```text
go-cache\
LICENSE.md
README.md
THIRD_PARTY_NOTICES.md
citation.cff
```

The cache and documentation are included when present but do not make installer
creation fail when absent.

#### Determine the application version

The application version is independent from `-RVersion`.

To derive it from the stage-1 bundle:

```powershell
$versionFile=Join-Path $dist "shiny-app\R\version_info.R"; $buildInfoFile=Join-Path $dist "shiny-app\BUILD_INFO"; $baseMatch=Select-String -Path $versionFile -Pattern 'MIRAPROT_VERSION_BASE\s*<-\s*"([^"]+)"'; $versionBase=$baseMatch.Matches[0].Groups[1].Value; $buildInfo=Get-Content $buildInfoFile -Raw | ConvertFrom-StringData; $appVersion=if($buildInfo.COMMIT_COUNT -match '^\d+$'){"$versionBase.$($buildInfo.COMMIT_COUNT)"}else{$versionBase}; $appVersion
```

For a deliberately assigned packaging version, `$appVersion` can instead be
set manually.

#### Compile

Use:

```powershell
& $iscc "/DAppVersion=$appVersion" "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

The PowerShell double quotes group each complete argument. They are **not**
extra quotes around the Inno preprocessor value.

Do not pass embedded quote characters after `=`.

For example, avoid passing a `DistDir` value that literally begins with `'C:`
or `"C:`.

Incorrect quoting can produce errors such as:

```text
Unknown filename prefix "'C:"
```

or:

```text
Unknown filename prefix "\C:"
```

The correct form is:

```text
"/DDistDir=$dist"
```

not a value containing another quoted path literal.

The Inno definition keeps repository-root `dist/` as its backward-compatible
default when `DistDir` is omitted, but explicitly supplying an absolute path is
preferred.

#### Installer output

The result is written to repository-root:

```text
output/
```

with a filename such as:

```text
MiraProt-1.0.0-windows-setup.exe
```

Verify:

```powershell
$installer=(Resolve-Path ".\output\MiraProt-$appVersion-windows-setup.exe").Path; Get-Item $installer | Select-Object FullName,Length,LastWriteTime
```

The locally created setup executable is unsigned unless a separate signing step
is performed.

#### Windows installed cache architecture

The stage-1 flat bundle may contain:

```text
go-cache\
├── annotation_cache\
└── go_cache\
    ├── BioMart_Cache\
    └── <organism caches>
```

When present, the installer copies these bytes unchanged to:

```text
{app}\resources\go-cache\
```

This is packaged **seed data**, not the writable installed cache.

The installer always creates:

```text
{app}\resources\go-cache\
```

even when the stage-1 bundle contains no cache. This directory acts as the
installed-layout signal.

The launcher recognizes:

```text
<exe-dir>\resources\go-cache
```

through `packagedGoCacheRoot()`.

For the installed Windows layout, `CacheDir()` therefore uses:

```text
%LOCALAPPDATA%\MiraProt\cache\
```

and not the installation directory.

The two main writable cache locations are:

```text
%LOCALAPPDATA%\MiraProt\cache\annotation_cache
%LOCALAPPDATA%\MiraProt\cache\go_cache
```

BioMart remains:

```text
%LOCALAPPDATA%\MiraProt\cache\go_cache\BioMart_Cache
```

Before R starts, `SeedCache()` attempts to seed:

```text
annotation_cache
go_cache
```

from the packaged resources.

Seeding occurs only when the corresponding user destination is empty.

Existing user cache contents are never overwritten.

If no shipped cache exists, seeding simply does nothing and normal runtime
download/population remains supported.

### macOS — DMG

The packaging script is:

```text
portable/installers/macos/create-dmg.sh
```

Run:

```bash
bash portable/installers/macos/create-dmg.sh --dist-dir dist --version 1.0.0 --output-dir output
```

This creates:

```text
output/MiraProt-1.0.0-macos-<uname-m>.dmg
```

containing `MiraProt.app`.

The script does not combine Intel and Apple Silicon code into a universal
binary.

The locally created app is unsigned and unnotarized.

Distributed builds should sign nested code and the app with an appropriate
Developer ID, then be notarized and stapled.

### Linux — AppImage

The packaging script is:

```text
portable/installers/linux/create-appimage.sh
```

Run:

```bash
bash portable/installers/linux/create-appimage.sh --dist-dir dist --version 1.0.0 --output-dir output
```

This creates:

```text
output/MiraProt-1.0.0-linux-<arch>.AppImage
```

The resulting AppImage is unsigned unless a separate signing process is used.

### Generated launcher resources versus disposable build products

Some generated launcher inputs are intentionally committed so a direct
`go build` in `portable/launcher` has known-good defaults:

- `MiraProt.ico`;
- `icon_data_windows.go`;
- `icon_data_nonwindows.go`.

Ordinary Windows portable builds regenerate these inputs only in a temporary
launcher staging directory and do not alter the committed copies.

The launcher resource configuration under `portable/launcher/winres/` is also
committed source configuration.

To intentionally update committed icons, replace the root
`MiraProt_icon.png`, then run:

```bash
go run gen_ico.go -write-source
```

from `portable/launcher`, inspect the generated artifacts, and commit them
deliberately.

By contrast, `.syso` files, locally compiled launcher binaries, installer
outputs, DMGs, AppImages, and temporary package trees are disposable build
products and must not be committed.

---

## 8. Build Artifact Types and CI

Do not use “artifact” and “release asset” interchangeably.

MiraProt has three distinct output categories:

| Output | Created by | Lifetime and visibility | Distribution status |
|---|---|---|---|
| **Local build artifact** | Developer runs a bundler | Remains locally until removed | Normal portable workflow |
| **Workflow artifact** | GitHub Actions | Temporary Actions artifact | CI validation/handoff |
| **GitHub Release asset** | Explicit maintainer publication | Public/persistent | Not produced by the current normal workflow |

The GitHub Actions workflow:

```text
.github/workflows/portable-build.yml
```

automates building and testing portable outputs.

### Triggers

| Trigger | When |
|---|---|
| Tag push | Push a tag matching `v*` |
| Manual dispatch | Run the workflow from GitHub Actions |

### Build matrix

| Runner | Output |
|---|---|
| `ubuntu-latest` | Linux amd64 |
| `macos-13` | macOS amd64 |
| `macos-14` | macOS arm64 |
| `windows-latest` | Windows amd64 |

### General build sequence

1. checkout repository;
2. set up Go and R;
3. install system dependencies where required;
4. restore/install R library;
5. assemble portable runtime;
6. build launcher;
7. smoke test;
8. archive/package output;
9. upload workflow artifacts.

### Source distribution policy

Tags may be pushed to validate a version across the build matrix, but a tag does
not by itself make the resulting portable binaries authoritative MiraProt
release assets.

Publishing portable binaries publicly is a separate policy decision that must
include licensing, provenance, support, signing, and retention considerations.

---

## 9. Launcher CLI Reference

| Flag | Type | Default | Description |
|---|---|---|---|
| `--port` | int | 3838 | Preferred TCP port for Shiny |
| `--app-dir` | string | auto | Shiny application path |
| `--r-home` | string | auto | Portable R path |
| `--debug` | bool | false | Verbose logging |
| `--version` | bool | false | Print version and exit |
| `--no-browser` | bool | false | Do not open browser |
| `--no-tray` | bool | false | Disable tray |
| `--idle-timeout` | int | 0 | Idle shutdown in minutes |

### Environment variables set for R

| Variable | Purpose |
|---|---|
| `R_LIBS_USER` | Bundled package library |
| `MIRAPROT_IN_PORTABLE` | Portable-mode flag |
| `MIRAPROT_PORT` | Actual Shiny port |
| `MIRAPROT_GO_CACHE` | Writable GO/BioMart cache root |
| `ANNOTATION_HUB_CACHE` | Writable AnnotationHub cache |
| `MIRAPROT_LOG_DIR` | Log directory |

### Portable cache layouts

Cache behavior differs by distribution layout.

#### Flat portable directory

`go-cache/` sits directly beside the launcher and is writable runtime data.

Example:

```text
MiraProt-launcher.exe
go-cache\
├── annotation_cache\
└── go_cache\
```

`MIRAPROT_GO_CACHE` points to:

```text
go-cache\go_cache
```

and `ANNOTATION_HUB_CACHE` points to:

```text
go-cache\annotation_cache
```

#### Packaged layouts

Windows installer, macOS app, and Linux AppImage use packaged cache resources
as seed data.

Locations:

```text
Windows: <installation>\resources\go-cache
macOS:   Contents/Resources/go-cache
Linux:   usr/go-cache
```

Runtime writes go to:

```text
<datadir>/cache/
```

The Windows installer always creates its packaged resource root, even when no
seed cache ships.

On first launch, the launcher copies each shipped cache only when its
destination is empty.

Existing user caches are never overwritten.

### Portable cache resolution and assembly

The launcher resolves cache paths in `portable/launcher/config.go`:

1. `goCacheRoot()` checks for adjacent flat `go-cache/`.
2. If absent, `packagedGoCacheRoot()` checks packaged locations including
   Windows `resources/go-cache`, Linux `usr/go-cache`, and macOS
   `Contents/Resources/go-cache`.
3. Packaged layouts use `<datadir>/cache/<name>` as writable runtime cache.
4. If neither flat nor packaged cache exists, the launcher attempts to create an
   adjacent flat `go-cache/`.
5. Only if that fails does it fall back to application data.

`ShippedCacheDir(name)` locates shipped cache data.

`SeedCache(name)`:

- does nothing when no shipped cache exists;
- uses adjacent flat cache directly;
- copies packaged seed data only when the user destination is empty;
- never overwrites existing user cache.

During stage-1 assembly, cache priority is:

1. existing portable output cache;
2. source project cache;
3. offline reconstruction/normalization;
4. network fallback for required missing bootstrap pieces.

The builders copy source:

```text
cache/GO_Cache/
```

into:

```text
go-cache/go_cache/
```

and source:

```text
cache/BioMart_Cache/
```

into:

```text
go-cache/go_cache/BioMart_Cache/
```

Independent per-organism AnnotationHub/BiocFileCache directories are not merged.

Instead, usable local OrgDb SQLite data can be canonicalized into the portable
organism cache.

The complete default-human source AnnotationHub cache may seed:

```text
go-cache/annotation_cache/
```

when appropriate.

A missing BioMart cache does not trigger an automatic full BioMart build.

Consequently:

- **flat bundles** read and write adjacent `go-cache/`;
- **Windows installed builds** use packaged `resources/go-cache` only as seed
  data and read/write `%LOCALAPPDATA%\MiraProt\cache`;
- **macOS app and Linux AppImage builds** likewise seed writable application
  data rather than modifying packaged resources.

If assembly supplies no cache, seeding is skipped and normal runtime download
remains supported.

---

## 10. Architecture Overview

### Startup flow

```text
User starts launcher
  → Find free port
  → Acquire single-instance lock
  → Resolve application/R/cache paths
  → Seed packaged cache if appropriate
  → Start Rscript --vanilla
  → Poll Shiny health endpoint
  → Open browser
  → Start tray and optional idle monitor
  → Wait for shutdown
  → Stop R process
  → Release lock
```

### Path auto-detection

The launcher looks for `shiny-app/` and `r-portable/` relative to its own binary
location.

On packaged platforms it also checks the appropriate resource locations.

### Single-instance lock

The lock file is stored at:

```text
<datadir>/launcher.lock
```

If an existing lock is found, the launcher checks whether the stored PID still
exists.

Stale locks from crashed processes are cleaned up.

### Update checking

On startup, the launcher queries the GitHub Releases API for the latest tag.

If a newer tag is available, it can notify the user.

The authoritative distribution remains source-only: the launcher does not
perform an automatic in-place software update.

---

## 11. Troubleshooting

### `Rscript not found`

Check that:

```text
r-portable/bin/Rscript
```

or on Windows:

```text
r-portable\bin\Rscript.exe
```

exists.

### `app.R not found`

Check that:

```text
shiny-app/app.R
```

exists.

### Another MiraProt instance is already running

Lock-file locations:

| OS | Lock file |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt\launcher.lock` |
| macOS | `~/Library/Application Support/MiraProt/launcher.lock` |
| Linux | `~/.local/share/MiraProt/launcher.lock` |

### Port conflict

If port 3838 is occupied, the launcher scans higher ports automatically.

Specify one manually if required:

```bash
./MiraProt-launcher --port 5000
```

### System tray icon does not appear on Linux

Install the GTK/AppIndicator packages:

```bash
sudo apt-get install -y libgtk-3-dev libayatana-appindicator3-dev
```

Or run without the tray.

### R package installation fails

- Check internet access.
- Read the first package/system-library error.
- On Linux, verify the required development libraries.
- On macOS, verify Xcode Command Line Tools.
- On Windows, use matching Rtools only when source compilation is genuinely
  required.

### Windows Inno Setup compiler cannot be found

Discover it:

```powershell
$iscc=@("$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe","C:\Program Files (x86)\Inno Setup 6\ISCC.exe","C:\Program Files\Inno Setup 6\ISCC.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1; $iscc; Test-Path $iscc
```

### Windows Inno Setup reports `Unknown filename prefix`

This usually indicates incorrect `/D` quoting.

Use:

```powershell
& $iscc "/DAppVersion=$appVersion" "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

Do not embed single or double quote characters around the actual value after the
`=` sign.

`$dist` should already contain an absolute path obtained with `Resolve-Path`.

### Windows Inno Setup reports a missing required component

Verify:

```powershell
@("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_)} } | Format-Table -AutoSize
```

The installer deliberately refuses to package an incomplete stage-1 bundle.

### Installed Windows cache is not seeded

Check the packaged seed:

```text
<installation>\resources\go-cache\
```

and the writable user cache:

```text
%LOCALAPPDATA%\MiraProt\cache\
```

Remember that `SeedCache()` does not overwrite a non-empty existing user cache.

### Bundler reports that R is unavailable on Linux/macOS

Install/select exactly the version in:

```text
portable/R_VERSION
```

before running `bundle-r.sh`.

## Test/check workflow for session save/restore changes

Run the fastest static checks before any full app startup or heavier restore
smoke tests.

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
```

```bash
python3 scripts/check-delimiter-quote-balance.py
```

```bash
python3 tests/static/test_heatmap_download_handler_source.py
```

Agents or developer environments without `Rscript` must explicitly report that
limitation in their results, but they should still run the available static
checks before proceeding.