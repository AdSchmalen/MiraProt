# MiraProt Standalone Edition — Developer Guide

This guide documents the portable desktop architecture of MiraProt, including the native Go launcher, stage-1 bundlers, cache handling, GSEA resource propagation, and optional platform-specific packages.

MiraProt's authoritative distribution is the source repository. Generated portable binaries and installers are locally created artifacts unless a separate release decision explicitly publishes them.

---

## Table of Contents

1. [Validation and support status](#1-validation-and-support-status)
2. [Version domains](#2-version-domains)
3. [Portable project structure](#3-portable-project-structure)
4. [Go launcher](#4-go-launcher)
5. [Stage 1 — Linux/macOS bundling](#5-stage-1--linuxmacos-bundling)
6. [Stage 1 — Windows bundling](#6-stage-1--windows-bundling)
7. [Runtime payload and GSEA resources](#7-runtime-payload-and-gsea-resources)
8. [Portable cache architecture](#8-portable-cache-architecture)
9. [Stage 2 — Windows installer](#9-stage-2--windows-installer)
10. [Stage 2 — macOS DMG](#10-stage-2--macos-dmg)
11. [Stage 2 — Linux AppImage](#11-stage-2--linux-appimage)
12. [Testing](#12-testing)
13. [Launcher CLI](#13-launcher-cli)
14. [Troubleshooting](#14-troubleshooting)

---

# 1. Validation and support status

Portable-build tooling exists for Windows, Ubuntu/Debian-family Linux, and macOS, but the current validation status is not equivalent across platforms.

| Platform | Current status |
|---|---|
| **Windows x86-64** | Manually built and tested. Windows stage 1 and Inno Setup packaging are the currently validated portable workflow. |
| **Linux, Ubuntu/Debian family** | Stage-1 builder and AppImage packager are implemented but have not yet been manually validated end-to-end on a native Linux target. Experimental. |
| **macOS, Intel / Apple Silicon** | Stage-1 builder and DMG packager are implemented but have not yet been manually validated end-to-end on native macOS targets. Experimental. |

Do not describe Linux/macOS portable packages as tested or supported at the same level as Windows until corresponding native validation evidence exists.

The repository currently should not claim CI evidence for Linux or macOS unless an actual active workflow providing that evidence exists and has successfully run against the relevant revision.

The portability qualification applies to the packaged desktop distribution, not automatically to source-mode R/Shiny execution.

---

# 2. Version domains

MiraProt has several independent version domains.

Do not infer one from another.

## MiraProt application version

The canonical release version lives in:

```text
VERSION
```

`R/version_info.R` reads that stable value. Normal commits do not change it;
Git SHA/date (and portable `BUILD_INFO`) remain separate revision metadata.
`CITATION.cff` must carry the same version, and release tags conventionally
use `v<VERSION>`.

## Portable R version

The maintained portable runtime version lives in:

```text
portable/R_VERSION
```

Bundler options such as:

```text
-RVersion
```

or:

```text
--r-version
```

select only the bundled R runtime.

They do not select the MiraProt application version.

## Launcher version

The Go launcher embeds a build-time version string through:

```text
-X main.Version=<value>
```

Both maintained bundlers read `VERSION` and inject it into `main.Version`, so
the launcher and application report the same release SemVer.

## Platform package version

Windows setup, DMG, and AppImage filenames have their own packaging version input.

The package version should describe the application artifact being packaged, but it is not the R runtime version.

---

# 3. Portable project structure

Portable-related source is under:

```text
portable/
├── R_VERSION
├── launcher/
│   ├── main.go
│   ├── config.go
│   ├── rprocess.go
│   ├── health.go
│   ├── browser.go
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
├── scripts/
│   ├── bundle-r.sh
│   ├── bundle-r-windows.ps1
│   ├── install-packages-portable.R
│   ├── install-packages.R
│   └── prebuild-cache.R
└── installers/
    ├── windows/
    │   └── MiraProt.iss
    ├── macos/
    │   ├── create-dmg.sh
    │   └── Info.plist
    └── linux/
        └── create-appimage.sh
```

Generated bundle directories, binaries, `.syso` files, installers, DMGs, AppImages, and temporary packaging directories are build products and should not be committed.

## Friendly wrappers, bootstrap, and Stage 1

Portable builds have three layers:

1. the root entry points (`Build-MiraProt.cmd`, `Build-MiraProt.command`, and `Build-MiraProt.sh`) provide memorable double-click or terminal commands;
2. the Stage-0 bootstrap wrappers (`portable/scripts/start-build-windows.ps1` and `portable/scripts/start-build-unix.sh`) validate the host, create a timestamped log, and invoke the correct builder;
3. the Stage-1 builders (`bundle-r-windows.ps1` and `bundle-r.sh`) assemble and verify the flat portable bundle. Stage 2 only consumes that tested output and is unchanged by these wrappers.

`Build-MiraProt.cmd` and `Build-MiraProt.command` enable interactive mode so a double-clicked window waits for Enter after success or failure. `Build-MiraProt.sh` is non-interactive by default, returns the builder's exit status directly, and is the preferred Unix automation entry point. Passing `--interactive` to the Unix shell entry point opts into the pause; it changes only the final prompt, not validation or build behavior. The bootstrap always writes its preflight, builder output, verification, final status, and exit code to:

```text
portable/logs/build-<timestamp>-<platform>.log
```

`portable/logs/` and generated bundles are ignored local artifacts. Bootstrap options are forwarded through the root entry points:

| Platform | Supported bootstrap arguments |
|---|---|
| Windows | `-Interactive`, `-OutputDir <directory>`, `-RVersion <MAJOR.MINOR.PATCH>` (the `.cmd` entry point supplies `-Interactive`) |
| Linux/macOS | `--interactive`, `--output-dir <directory>`, `--r-version <MAJOR.MINOR.PATCH>`, `-h` / `--help` |

The Windows friendly entry point defaults to the sibling directory `../MiraProt_Portable`; the Unix entry points retain `portable/dist`. Relative output paths are resolved from the repository root. The maintained `portable/R_VERSION` should normally be used; an R-version option selects the runtime, never the MiraProt application version.

### Bootstrap platform prerequisites

All platforms require a supported 64-bit architecture, Go 1.22 or later, writable output space, and CRAN network access. Git is required only when `.git` exists. Windows requires x86-64 Windows and PowerShell 5.1 or later; its Stage-1 builder downloads R, so system R is not required. Linux/macOS require native `R`, `Rscript`, Go, and `rsync`, with R exactly matching `portable/R_VERSION`. Linux currently also requires `apt-get` and `dpkg`; macOS requires Xcode Command Line Tools. The detailed native libraries remain listed in the platform sections below.

For a Git checkout, bootstrap requires Git and Stage 1 records actual `HEAD` revision metadata. For an extracted source archive without `.git`, Git is skipped; canonical `VERSION` provides SemVer and `BUILD_INFO` explicitly records unavailable commit data and `REVISION=source-archive`.

Build-time dependencies are not automatically finished-runtime dependencies. PowerShell, Git, Go, compilers, `rsync`, Rtools, Xcode tools, and packaging utilities assemble the artifact. The finished bundle includes its own R runtime, R library, Shiny payload, and launcher. A Windows target does not require those build tools to run; Linux/macOS targets may still require compatible OS shared libraries such as glibc, graphics/font libraries, GTK, or AppIndicator.

Package installation prefers the committed `renv.lock`, installing only packages
missing from the destination `r-library`. Existing portable packages are
deliberately preserved, including when their versions differ from the lockfile.
If the lockfile or its committed renv bootstrap is unavailable or unusable, the
builder automatically runs the existing package-list installer. Portable builds
read but never modify `renv.lock`.

---

# 4. Go launcher

The Go launcher:

- resolves packaged paths;
- starts bundled R through `Rscript --vanilla`;
- sets the application working directory;
- configures portable cache environment variables;
- waits for the Shiny health endpoint;
- opens the browser;
- provides tray integration when enabled;
- manages shutdown;
- keeps logs under the platform-specific application-data directory.

## Basic build

From:

```text
portable/launcher
```

build with:

```text
go build -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher .
```

Windows:

```powershell
Set-Location .\portable\launcher; go build -ldflags "-s -w -X main.Version=dev" -o MiraProt-launcher.exe .
```

Check:

```text
MiraProt-launcher --version
```

or Windows:

```powershell
.\MiraProt-launcher.exe --version
```

## Tray support

The launcher uses:

```text
fyne.io/systray
```

for system-tray integration.

The tray-enabled implementation intentionally runs the systray loop on the main goroutine because macOS Cocoa integration requires this architecture.

Native tray behavior must nevertheless be manually tested on each claimed platform.

## Browser handling

The launcher uses:

- Windows: `rundll32`;
- macOS: `open`;
- Linux/Unix: `xdg-open`.

## R startup

R is started approximately as:

```text
Rscript --vanilla -e "shiny::runApp(...)"
```

The child process working directory is the packaged `shiny-app` directory.

This is why relative application resources such as:

```text
./GSEA
```

resolve inside the packaged Shiny application.

---

# 5. Stage 1 — Linux/macOS bundling

The Unix stage-1 builder is:

```text
portable/scripts/bundle-r.sh
```

It contains separate Linux and macOS execution paths.

## Validation status

The implementation is real but currently **experimental** because complete native end-to-end validation has not yet been performed.

Do not claim CI-tested Linux/macOS targets unless an active workflow actually exists and has successfully validated them.

## Git checkout and source-archive metadata

The Unix bundler supports both Git checkouts and extracted source archives. When
`.git` metadata exists, Git is required and the bundler fails if the checkout or
`HEAD` is invalid. It records the real commit count, short SHA, and commit date.

Without `.git` metadata, the canonical `VERSION` supplies the application and
launcher SemVer. `BUILD_INFO` explicitly marks commit fields as unavailable and
identifies the revision as `source-archive`; it does not fabricate repository
metadata.

## R requirements

Unlike the Windows builder, `bundle-r.sh` currently expects a compatible native R installation to exist on the build system.

It validates:

- `R --version`;
- `Rscript --version`;
- `getRversion()` through `Rscript --vanilla`.

The exact version must match:

```text
portable/R_VERSION
```

## Linux target

The Linux branch expects tools such as:

```text
apt-get
dpkg
```

Therefore the current documented implementation target is Ubuntu/Debian-family Linux.

Do not publish guessed `dnf`, `pacman`, or `zypper` translations without actual validation.

## Linux prerequisites

One-line Ubuntu/Debian example:

```bash
sudo apt-get update && sudo apt-get install -y r-base golang-go git rsync gcc libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libtiff5-dev libjpeg-dev libpng-dev librsvg2-dev libcurl4-openssl-dev libssl-dev libxml2-dev libgtk-3-dev libayatana-appindicator3-dev
```

Tray runtime dependencies can additionally require:

```bash
sudo apt-get install -y libgtk-3-0 libayatana-appindicator3-1
```

The resulting Linux bundle is relocatable only within sufficiently compatible systems.

Copied R and native packages can retain dependencies on:

- glibc;
- libstdc++;
- OpenSSL;
- libcurl;
- libxml2;
- font/graphics libraries;
- GTK/AppIndicator;
- other native dependencies.

A successful bundle build is not sufficient evidence of portability to another distribution or older system.

## macOS requirements

Install Xcode Command Line Tools where compilation is needed:

```bash
xcode-select --install
```

The current builder recognizes:

```text
x86_64
```

and:

```text
arm64
```

Do not combine native architectures arbitrarily.

R, compiled R packages, the Go launcher, and package host should use compatible native architectures.

Rosetta compatibility is not a substitute for testing.

## Build command

From repository root:

```bash
bash portable/scripts/bundle-r.sh --output-dir portable/dist
```

Optional runtime override:

```bash
bash portable/scripts/bundle-r.sh --r-version 4.6.1 --output-dir portable/dist
```

`--r-version` selects R only.

---

# 6. Stage 1 — Windows bundling

The Windows builder is:

```text
portable/scripts/bundle-r-windows.ps1
```

The Windows path is currently the manually validated portable-build implementation.

## Standard build

From repository root:

```powershell
.\portable\scripts\bundle-r-windows.ps1
```

Custom example:

```powershell
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.6.1" -OutputDir "..\MiraProt_Portable"
```

## High-level sequence

The Windows builder:

1. validates build prerequisites;
2. downloads and validates the requested R installer;
3. installs R into a temporary staging directory;
4. validates the staged R executable set;
5. safely promotes the validated runtime;
6. installs required R packages into `r-library`;
7. seeds available source caches;
8. normalizes/fills portable cache data;
9. copies the runtime Shiny payload;
10. copies optional local `GSEA/*.gmt` files;
11. writes `BUILD_INFO`;
12. generates Windows launcher resources in temporary staging;
13. builds `MiraProt-launcher.exe`.

## R staging

The Windows builder installs R into a unique temporary location.

The staged runtime must contain:

```text
bin\R.exe
bin\Rscript.exe
bin\x64\R.dll
```

It then validates the runtime before promotion.

The old runtime remains recoverable until final-path validation succeeds.

This prevents a failed rebuild from unnecessarily destroying a previously usable `r-portable`.

## renv isolation

Build-time portable R calls use:

```text
--vanilla
```

and cleaned R environment variables.

The source project's development `renv` state must not affect the portable runtime library.

The runtime payload does not include:

```text
renv/
.Rprofile
renv.lock
```

Portable runtime startup likewise uses:

```text
Rscript --vanilla
```

---

# 7. Runtime payload and GSEA resources

Stage 1 uses a runtime allowlist rather than recursively copying the complete source repository.

The runtime application includes approximately:

```text
app.R
R/
modules/
Documentation/*.R
AutoAssign/
GSEA/
MiraProt_icon.png
BUILD_INFO
```

Developer/build-only resources remain outside `shiny-app`.

## GMT files

Source GMT files are intentionally ignored by Git.

The builders preserve the normal GSEA directory copy while deliberately handling immediate lowercase:

```text
GSEA/*.gmt
```

as optional local build inputs.

Mapping:

```text
<source>/GSEA/example.gmt
```

becomes:

```text
<stage-1>/shiny-app/GSEA/example.gmt
```

If no GMT files are present:

- the build succeeds;
- no placeholder is created;
- the bundle simply contains no gene-set database.

Only immediate files are relevant because the GSEA runtime scans the immediate `./GSEA` directory rather than recursively traversing arbitrary subdirectories.

Do not:

- commit GMT files;
- weaken `GSEA/.gitignore`;
- automatically download MSigDB data;
- imply redistribution rights.

## Stage-2 behavior

Stage-2 packagers should consume only the already-built stage-1 distribution.

Correct:

```text
source resource → stage 1 → stage 2 package
```

Incorrect:

```text
stage 2 package → reaches back into source GSEA/cache directories
```

The tested stage-1 tree is the authoritative package input.

---

# 8. Portable cache architecture

Portable cache behavior depends on layout.

## Flat portable bundle

A flat bundle has:

```text
MiraProt-launcher
go-cache/
├── annotation_cache/
└── go_cache/
```

On Windows:

```text
MiraProt-launcher.exe
go-cache\
├── annotation_cache\
└── go_cache\
```

The adjacent `go-cache` is writable runtime application data.

The launcher resolves:

```text
MIRAPROT_GO_CACHE = go-cache/go_cache
ANNOTATION_HUB_CACHE = go-cache/annotation_cache
```

The flat portable directory therefore needs to be writable.

## Packaged distributions

Packaged distributions use a packaged cache root only as seed data.

Recognized locations include:

```text
Windows installer:
<exe directory>\resources\go-cache
```

```text
Linux AppImage:
<exe directory>\..\go-cache
```

```text
macOS app:
<exe directory>\..\Resources\go-cache
```

Runtime writes go to the user application-data directory.

## Application-data roots

Windows:

```text
%LOCALAPPDATA%\MiraProt
```

macOS:

```text
~/Library/Application Support/MiraProt
```

Linux:

```text
${XDG_DATA_HOME:-~/.local/share}/MiraProt
```

Writable packaged cache destinations are:

```text
<application-data>/cache/annotation_cache
<application-data>/cache/go_cache
```

## Cache seeding

Before starting R, the launcher calls cache seeding for:

```text
annotation_cache
go_cache
```

If stage-1/package cache seed data exist and the writable destination is empty, they are copied.

If the destination already contains data, it is left untouched.

If no seed exists, runtime download remains supported.

## Builder cache source mapping

Source:

```text
cache/GO_Cache/
```

maps to stage-1:

```text
go-cache/go_cache/
```

Source:

```text
cache/BioMart_Cache/
```

maps to:

```text
go-cache/go_cache/BioMart_Cache/
```

A complete appropriate AnnotationHub cache can seed:

```text
go-cache/annotation_cache/
```

Independent organism BiocFileCache indexes must not be blindly merged.

Cache normalization and canonicalization belong in stage 1, not stage 2.

---

# 9. Stage 2 — Windows installer

The Windows installer definition is:

```text
portable/installers/windows/MiraProt.iss
```

It consumes an already-built stage-1 portable directory.

It requires:

```text
MiraProt-launcher.exe
shiny-app\
r-portable\
r-library\
```

Optional resources include:

```text
go-cache\
LICENSE.md
README.md
THIRD_PARTY_NOTICES.md
CITATION.cff
```

GMT files inside:

```text
shiny-app\GSEA\
```

are automatically included through the recursive `shiny-app` packaging rule.

Do not add a second rule pointing directly to source `GSEA`.

## Install Inno Setup

```powershell
winget install --id JRSoftware.InnoSetup -e -s winget -i
```

Discover `ISCC.exe`:

```powershell
$iscc=@("$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe","C:\Program Files (x86)\Inno Setup 6\ISCC.exe","C:\Program Files\Inno Setup 6\ISCC.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1; $iscc; Test-Path $iscc
```

## Select DistDir

Default stage-1 path:

```powershell
$dist=(Resolve-Path ".\portable\dist").Path; $dist
```

Custom:

```powershell
$dist=(Resolve-Path "..\MiraProt_Portable").Path; $dist
```

## Validate required contents

```powershell
@("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_);Path=Join-Path $dist $_} } | Format-Table -AutoSize
```

## Determine application version

```powershell
$appVersion=(Get-Content (Join-Path $dist "VERSION") -Raw).Trim(); $appVersion
```

## Correct PowerShell/Inno syntax

Compile with:

```powershell
& $iscc "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

The quotes shown above are PowerShell argument grouping.

Do not pass quote characters as part of the actual `DistDir` value.

Incorrect embedded quotes can cause:

```text
Unknown filename prefix "'C:"
```

or:

```text
Unknown filename prefix "\C:"
```

## Installed cache layout

The installer copies stage-1:

```text
go-cache\
```

into:

```text
{app}\resources\go-cache\
```

when present.

The installer always creates:

```text
{app}\resources\go-cache\
```

as the installed-layout signal.

Writable installed cache state is kept under:

```text
%LOCALAPPDATA%\MiraProt\cache\
```

Existing populated user caches are not overwritten.

---

# 10. Stage 2 — macOS DMG

The packager is:

```text
portable/installers/macos/create-dmg.sh
```

Current status: **experimental / not yet manually validated end-to-end on native macOS.**

Run after stage 1:

```bash
bash portable/installers/macos/create-dmg.sh --dist-dir portable/dist --output-dir output
```

The package layout is approximately:

```text
MiraProt.app/
└── Contents/
    ├── MacOS/
    │   └── MiraProt-launcher
    └── Resources/
        ├── app/
        ├── R/
        ├── r-library/
        └── go-cache/
```

The stage-1 `shiny-app` is copied to:

```text
Contents/Resources/app
```

so GMT files already present in stage 1 are preserved automatically.

Optional `go-cache` is copied into:

```text
Contents/Resources/go-cache
```

If stage-1 cache is absent, the packager creates an empty packaged cache root.

This keeps packaged-cache detection functional.

## Important native validation items

Before claiming DMG support as validated, confirm on a real Mac:

- Intel and/or ARM architecture as claimed;
- `.app` launch;
- tray integration;
- browser opening;
- bundled R startup;
- native R package loading;
- cache seeding;
- Data Wizard workflow;
- representative analyses;
- GSEA resources;
- session save/restore;
- shutdown/relaunch;
- DMG copy/install behavior;
- Gatekeeper behavior.

## Current icon caveat

`Info.plist` expects an application icon.

The current package script searches for an `icon.icns`, but the icon-production/copy path should be verified on a real packaged build.

Do not call the macOS package presentation release-ready until the intended icon is confirmed.

## Signing

Local builds are unsigned and unnotarized unless a separate process is used.

Distributed packages should use proper Developer ID signing, notarization, and stapling.

---

# 11. Stage 2 — Linux AppImage

The packager is:

```text
portable/installers/linux/create-appimage.sh
```

Current status: **experimental / not yet manually validated end-to-end on native Linux.**

Run after stage 1:

```bash
bash portable/installers/linux/create-appimage.sh --dist-dir portable/dist --output-dir output
```

The AppDir structure includes approximately:

```text
MiraProt.AppDir/
├── AppRun
├── MiraProt.desktop
└── usr/
    ├── bin/
    │   ├── MiraProt-launcher
    │   ├── shiny-app/
    │   ├── r-portable/
    │   └── r-library/
    └── go-cache/
```

Stage-1 GMT files are preserved through the full `shiny-app` copy.

Optional cache data are copied into:

```text
usr/go-cache
```

If absent, the package creates an empty root.

## Current icon caveat

The AppImage packager looks for an existing MiraProt PNG resource.

If none is found, it currently falls back to a minimal transparent placeholder.

That is functional packaging fallback behavior, but it is not release-quality visual presentation.

Before calling AppImage support validated, ensure the real MiraProt icon is packaged reliably.

## Reproducibility caveat

The script can download `appimagetool` automatically.

For a reproducible release process, prefer pinning or otherwise recording the exact tool version rather than depending indefinitely on a moving `continuous` build.

## Required native validation

Test:

- AppImage execution;
- FUSE/extraction behavior on target distributions;
- launcher startup;
- tray;
- `xdg-open`;
- native R dependencies;
- Data Wizard import;
- representative analyses;
- GO/AnnotationHub;
- BioMart;
- GSEA;
- session save/restore;
- shutdown/relaunch.

---

# 12. Testing

## Windows stage-1 minimum

Build:

```powershell
.\portable\scripts\bundle-r-windows.ps1
```

Verify:

```powershell
Test-Path .\portable\dist\MiraProt-launcher.exe; Test-Path .\portable\dist\r-portable\bin\Rscript.exe; Test-Path .\portable\dist\shiny-app\app.R
```

Launcher version:

```powershell
.\portable\dist\MiraProt-launcher.exe --version
```

Bundled R:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --version
```

Shiny package check:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --vanilla -e ".libPaths(c(normalizePath('portable/dist/r-library'),.libPaths())); stopifnot(requireNamespace('shiny',quietly=TRUE)); cat('OK\n')"
```

Launch:

```powershell
.\portable\dist\MiraProt-launcher.exe
```

## Functional manual smoke test

At minimum test:

- application startup;
- browser opening;
- Data Wizard file import;
- metadata assignment;
- table propagation;
- abundance view;
- PCA/UMAP;
- volcano/statistics;
- GO;
- GSEA when GMT files are supplied;
- STRING where network access is available;
- heatmap;
- export;
- session save/restore;
- clean shutdown.

## GMT propagation

Create a temporary ignored GMT file only for validation if needed.

Confirm it is ignored:

```text
git check-ignore -v GSEA/__miraprot_local_test__.gmt
```

After stage 1, confirm:

```text
shiny-app/GSEA/__miraprot_local_test__.gmt
```

exists and matches the source bytes.

Remove the temporary file before committing.

Never use:

```text
git add -f
```

for GMT resources.

## Cache combinations

Validate:

### Cache present / GMT present

Both should reach stage 1 and stage 2.

### Cache absent / GMT present

Packaging should succeed and GMT should remain present.

### Cache present / GMT absent

Packaging should succeed with cache only.

### Cache absent / GMT absent

Packaging should still succeed and runtime should populate missing cache resources when needed.

---

# 13. Launcher CLI

Current launcher options include:

| Flag | Purpose |
|---|---|
| `--port` | Preferred Shiny port |
| `--app-dir` | Override application directory |
| `--r-home` | Override portable R location |
| `--debug` | Verbose logging |
| `--version` | Print launcher version and exit |
| `--no-browser` | Do not open the browser automatically |
| `--no-tray` | Disable tray integration |
| `--idle-timeout` | Idle shutdown timeout |

Example:

```text
MiraProt-launcher --debug
```

Windows:

```powershell
.\MiraProt-launcher.exe --debug
```

## Environment supplied to R

The launcher configures variables including:

```text
R_LIBS_USER
MIRAPROT_IN_PORTABLE
MIRAPROT_PORT
MIRAPROT_GO_CACHE
ANNOTATION_HUB_CACHE
MIRAPROT_LOG_DIR
```

R is started with:

```text
--vanilla
```

so source-development startup profiles do not control portable runtime behavior.

---

# 14. Troubleshooting

## Windows portable build unexpectedly activates renv

Portable build-time and runtime R invocations should use:

```text
--vanilla
```

and clean R startup environment variables.

If output shows source-project renv activation or `renv:shims`, inspect the exact `Rscript` invocation.

## `Rscript` missing

Windows expected path:

```text
r-portable\bin\Rscript.exe
```

Unix expected path:

```text
r-portable/bin/Rscript
```

## Linux/macOS checkout metadata is invalid

When `.git` metadata exists, the Unix builder requires a working Git command and
a valid checkout with `HEAD`. Repair the checkout or build from a source archive
that does not contain `.git` metadata. Archive builds use canonical `VERSION`
SemVer and explicitly unavailable revision metadata.

## Linux launcher fails because of shared libraries

Inspect:

```text
ldd
```

output for the launcher and relevant R binaries/native package libraries.

A copied R installation does not eliminate Linux system-library dependencies.

## macOS app does not display the expected icon

Verify whether an actual:

```text
icon.icns
```

was included under:

```text
MiraProt.app/Contents/Resources/
```

The current macOS icon pipeline has not yet been manually validated.

## Linux AppImage has no visible MiraProt icon

The current packager can fall back to a transparent placeholder if it does not find the intended PNG resource.

Fix/verify the icon resource path before treating the package as release-quality.

## Inno Setup `Unknown filename prefix`

Use:

```powershell
& $iscc "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

Do not embed additional quote characters around the `DistDir` value.

## Installed Windows cache is not replaced

This is intentional.

Packaged seed data do not overwrite an already populated:

```text
%LOCALAPPDATA%\MiraProt\cache
```

destination.

Use a fresh Windows profile or temporarily move the existing cache if testing true first-launch seed behavior.

## Linux/macOS validation policy

Until native end-to-end tests have been completed, documentation and release notes should use wording such as:

```text
Experimental portable-build support
```

rather than:

```text
fully supported
```

or:

```text
tested
```

Successful static inspection or successful package assembly alone is not enough to claim runtime validation.
