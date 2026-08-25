# Build and Run MiraProt on Your Computer

This guide creates a local portable MiraProt bundle from the source code.

You do not need to be a developer, but the first build downloads or installs a substantial software environment and can take considerable time. Allow several gigabytes of free disk space and use a reliable internet connection.

## Platform validation status

Portable-build support is not currently validated equally on all operating systems.

| Platform | Status |
|---|---|
| **Windows x86-64** | Manually built and tested. This is the currently validated portable desktop workflow. |
| **Linux, Ubuntu/Debian family** | Build and AppImage tooling is implemented but has not yet been manually validated end-to-end on a native Linux installation. Treat this path as experimental. |
| **macOS, Intel / Apple Silicon** | Build and DMG tooling is implemented but has not yet been manually validated end-to-end on native macOS systems. Treat this path as experimental. |

The Linux/macOS qualification concerns the portable desktop packaging. It does not mean that the R/Shiny source application itself is inherently Windows-only.

### How to use this guide

The workflow is:

1. install the build tools required for your operating system;
2. obtain the MiraProt source;
3. verify that the terminal is in the repository root;
4. build the stage-1 portable bundle;
5. verify and launch it;
6. optionally create a platform-specific package such as the Windows installer;
7. optionally supply GSEA GMT files.

**Windows is the recommended portable-build path at present.**

---

# 1. Get the MiraProt source

On Windows, Linux, or macOS, either a Git checkout or an extracted source archive
can be used. Checkout builds require Git and record real revision metadata.
Archive builds use the canonical `VERSION` for application and launcher SemVer
and explicitly report that commit metadata is unavailable.

## Clone with Git

Open PowerShell on Windows or Terminal on Linux/macOS:

```text
git clone https://github.com/AdSchmalen/MiraProt.git; cd MiraProt
```

Run all build commands from the repository root.

The correct directory contains at least:

```text
app.R
portable/
R/
modules/
```

---

# 2. Windows

The Windows portable build currently targets **64-bit x86 Windows (`amd64`)**.

Windows ARM64 is not currently a native portable-build target.

## Step 1 — Open PowerShell

Open Windows PowerShell from the Start menu.

Check PowerShell and `winget`:

```powershell
$PSVersionTable.PSVersion; winget --version
```

PowerShell 5.1 can run the builder, but PowerShell 7 is recommended.

## Step 2 — Install PowerShell 7

```powershell
winget install --id Microsoft.PowerShell --exact --source winget --accept-package-agreements --accept-source-agreements
```

Close the old PowerShell window and open **PowerShell 7**.

Verify:

```powershell
$PSVersionTable.PSVersion
```

The major version should be `7`.

## Step 3 — Check the processor architecture

```powershell
[System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
```

Expected:

```text
X64
```

If the result is `Arm64`, the current Windows portable builder is not a native ARM64 build path.

## Step 4 — Install Git

```powershell
winget install --id Git.Git --exact --source winget --accept-package-agreements --accept-source-agreements
```

Reopen PowerShell 7 and verify:

```powershell
git --version
```

## Step 5 — Install Go

Go is used to compile the native MiraProt launcher.

```powershell
winget install --id GoLang.Go --exact --source winget --accept-package-agreements --accept-source-agreements
```

Reopen PowerShell 7 and verify:

```powershell
go version
```

The output should indicate a Windows amd64 Go installation.

Go is a build dependency. It is not required to run the finished portable bundle.

## Step 6 — Enter the MiraProt source directory

If you cloned the repository into your home directory:

```powershell
Set-Location "$HOME\MiraProt"
```

Verify:

```powershell
Get-Location; Test-Path .\app.R; Test-Path .\portable\scripts\bundle-r-windows.ps1
```

The two `Test-Path` results should both be:

```text
True
```

## Step 7 — Check the portable R version and internet access

The Windows builder downloads its own R runtime.

You do **not** need a normal system R installation for the standard Windows portable build.

Check the configured R version and CRAN connectivity:

```powershell
Get-Content .\portable\R_VERSION; Test-NetConnection cloud.r-project.org -Port 443 | Select-Object ComputerName,TcpTestSucceeded
```

Check available disk space:

```powershell
$drive=(Split-Path $HOME -Qualifier).TrimEnd(':'); [math]::Round((Get-PSDrive -Name $drive).Free / 1GB,1)
```

Several gigabytes are required. At least approximately 10 GB of free working space is a practical starting point.

## Step 8 — Build the portable bundle

For the friendly default workflow, double-click `Build-MiraProt.cmd` in File Explorer. The window remains open at the end so you can read the result. The terminal equivalent is:

```powershell
.\Build-MiraProt.cmd
```

The wrapper checks prerequisites, writes a timestamped file under `portable\logs\`, and builds the default `portable\dist` output. Advanced users can invoke the Stage-1 builder directly, including to select a separate output directory:

```powershell
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.6.1" -OutputDir "..\MiraProt_Portable"
```

`-RVersion` selects the **R runtime version**, not the MiraProt application version.

Ordinary users should normally omit `-RVersion` and use the maintained value in:

```text
portable\R_VERSION
```

The first build performs several operations:

- downloads and validates portable R;
- installs the required R packages;
- reuses compatible local GO, AnnotationHub, and BioMart cache data when available;
- fills required missing bootstrap cache data where possible;
- copies the runtime application;
- includes locally present immediate `GSEA/*.gmt` files when available;
- builds the Go launcher.

A cache-prebuild warning is recoverable because missing cache data can be obtained at runtime.

A package installation failure or Go build failure is not recoverable within that build and should be resolved before retrying.

Successful output ends with:

```text
=== Bundle complete ===
```

and prints the launcher path.

## Step 9 — Verify the Windows bundle

For the default output:

```powershell
Test-Path .\portable\dist\MiraProt-launcher.exe; Test-Path .\portable\dist\r-portable\bin\Rscript.exe; Test-Path .\portable\dist\shiny-app\app.R
```

Expected:

```text
True
True
True
```

For a custom output directory:

```powershell
$dist=(Resolve-Path "..\MiraProt_Portable").Path; @("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_)} } | Format-Table -AutoSize
```

All four required components should be present.

Check the launcher version if desired:

```powershell
.\portable\dist\MiraProt-launcher.exe --version
```

Check bundled R:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --version
```

Check that the bundled library can load Shiny:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --vanilla -e ".libPaths(c(normalizePath('portable/dist/r-library'),.libPaths())); stopifnot(requireNamespace('shiny',quietly=TRUE)); cat('OK\n')"
```

Expected:

```text
OK
```

## Step 10 — Launch MiraProt

```powershell
.\portable\dist\MiraProt-launcher.exe
```

For a custom output:

```powershell
& "$dist\MiraProt-launcher.exe"
```

The launcher normally starts MiraProt at:

```text
http://127.0.0.1:3838
```

and opens the default browser.

Use the MiraProt tray icon to quit.

When launched from PowerShell, **Ctrl+C** can also stop the process.

Keep the entire portable directory together.

The following build tools are not required merely to launch a finished Windows portable bundle:

- PowerShell 7;
- Git;
- Go;
- Rtools;
- Inno Setup;
- system R.

---

# 3. Optional Windows installer

Once the stage-1 Windows portable bundle has been built and tested, it can optionally be packaged into a conventional Windows setup executable using **Inno Setup 6**.

The installer consumes the existing portable directory.

It does not:

- rebuild MiraProt;
- reinstall R;
- reinstall R packages;
- regenerate GO caches;
- refresh BioMart;
- refresh AnnotationHub.

## Step 1 — Install Inno Setup 6

```powershell
winget install --id JRSoftware.InnoSetup -e -s winget -i
```

Inno Setup may be installed per-user or machine-wide.

A common per-user location is:

```text
C:\Users\<user>\AppData\Local\Programs\Inno Setup 6\ISCC.exe
```

A common machine-wide location is:

```text
C:\Program Files (x86)\Inno Setup 6\ISCC.exe
```

Discover the compiler automatically:

```powershell
$iscc=@("$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe","C:\Program Files (x86)\Inno Setup 6\ISCC.exe","C:\Program Files\Inno Setup 6\ISCC.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1; $iscc; Test-Path $iscc
```

Expected:

```text
True
```

If no path is found:

```powershell
Get-ChildItem "C:\Program Files*","$env:LOCALAPPDATA\Programs" -Filter ISCC.exe -Recurse -ErrorAction SilentlyContinue | Select-Object FullName
```

Then set the discovered path manually, for example:

```powershell
$iscc="C:\Users\<user>\AppData\Local\Programs\Inno Setup 6\ISCC.exe"; Test-Path $iscc
```

## Step 2 — Select the existing portable bundle

For the default stage-1 output:

```powershell
$dist=(Resolve-Path ".\portable\dist").Path; $dist
```

For a custom build such as:

```text
..\MiraProt_Portable
```

use:

```powershell
$dist=(Resolve-Path "..\MiraProt_Portable").Path; $dist
```

Use an absolute path.

`Resolve-Path` provides the appropriate absolute filesystem path for Inno Setup.

## Step 3 — Verify required components

```powershell
@("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_);Path=Join-Path $dist $_} } | Format-Table -AutoSize
```

All required components should show:

```text
True
```

## Step 4 — Check optional resources

```powershell
@("go-cache","LICENSE.md","README.md","THIRD_PARTY_NOTICES.md","CITATION.cff") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_)} } | Format-Table -AutoSize
```

These are optional.

If `go-cache` exists, its contents are included as packaged seed data.

GMT files do not require a separate installer rule. Files already present in:

```text
shiny-app\GSEA\
```

are automatically included because the installer recursively packages the complete stage-1 `shiny-app` directory.

## Step 5 — Determine the application version

```powershell
$appVersion=(Get-Content (Join-Path $dist "VERSION") -Raw).Trim(); $appVersion
```

## Step 6 — Compile the installer

Use this exact argument format:

```powershell
& $iscc "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

Do **not** embed additional single or double quote characters around the path after `=`.

Correct:

```text
"/DDistDir=$dist"
```

Incorrect values that literally contain quotes can result in errors such as:

```text
Unknown filename prefix "'C:"
```

or:

```text
Unknown filename prefix "\C:"
```

## Step 7 — Verify the installer

```powershell
$installer=(Resolve-Path ".\output\MiraProt-$appVersion-windows-setup.exe").Path; $installer; Get-Item $installer | Select-Object FullName,Length,LastWriteTime
```

The generated setup executable is unsigned unless a separate signing process is performed.

## Step 8 — Install and test

```powershell
& $installer
```

For an initial test, a user-writable test directory can be selected during installation.

The installed application contains the stage-1 application payload and, when available, packaged cache seed data under:

```text
<installation>\resources\go-cache\
```

The installed launcher uses writable runtime caches under:

```text
%LOCALAPPDATA%\MiraProt\cache\
```

Typical paths are:

```text
%LOCALAPPDATA%\MiraProt\cache\annotation_cache
%LOCALAPPDATA%\MiraProt\cache\go_cache
%LOCALAPPDATA%\MiraProt\cache\go_cache\BioMart_Cache
```

Existing populated user caches are not overwritten by a newly installed packaged seed.

---

# 4. Linux — experimental portable path

> **Validation status:** the Linux portable-build path is implemented for Ubuntu/Debian-family systems but has not yet been manually validated end-to-end on a native Linux installation. Treat this packaging path as experimental.

The current build script uses `apt-get` and `dpkg`.

Therefore the documented local-build path is specifically aimed at Ubuntu/Debian-family systems.

Fedora/RHEL-family, Arch-family, openSUSE, and other distributions are not currently supported by verified package mappings.

## Requirements

Install:

- the exact R version configured in `portable/R_VERSION`;
- Git, when building from a Git checkout;
- Go;
- `rsync`;
- `gcc`;
- required development libraries.

For Ubuntu/Debian-family systems:

```bash
sudo apt-get update && sudo apt-get install -y r-base golang-go git rsync gcc libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libtiff5-dev libjpeg-dev libpng-dev librsvg2-dev libcurl4-openssl-dev libssl-dev libxml2-dev libgtk-3-dev libayatana-appindicator3-dev
```

The tray-enabled runtime also relies on compatible GTK/AppIndicator libraries.

Install the runtime counterparts where necessary:

```bash
sudo apt-get install -y libgtk-3-0 libayatana-appindicator3-1
```

Exact package names can differ between Ubuntu/Debian releases.

## Verify R

```bash
cat portable/R_VERSION; Rscript --version
```

The versions must match.

## Build

From the repository root, use the friendly non-interactive terminal entry point:

```bash
./Build-MiraProt.sh
```

It performs bootstrap checks and records output under `portable/logs/`. Advanced users may invoke Stage 1 directly with `bash portable/scripts/bundle-r.sh --output-dir portable/dist`.

The stage-1 output contains:

```text
portable/dist/
├── MiraProt-launcher
├── shiny-app/
├── r-portable/
├── r-library/
└── go-cache/
```

The cache and some documentation resources are optional.

## Launch

```bash
./portable/dist/MiraProt-launcher
```

The launcher should start the local Shiny server and open the browser through the system's `xdg-open` mechanism.

Because this path has not yet been manually validated end-to-end, verify at minimum:

- launcher startup;
- browser opening;
- tray behavior;
- Data Wizard import;
- a representative downstream analysis;
- GO/AnnotationHub cache use;
- BioMart behavior;
- GSEA file discovery when GMT files are supplied;
- shutdown;
- relaunch.

## Optional AppImage

After testing the stage-1 flat bundle:

```bash
bash portable/installers/linux/create-appimage.sh --dist-dir portable/dist --output-dir output
```

The script creates:

```text
output/MiraProt-1.0.0-linux-<arch>.AppImage
```

The stage-1 `shiny-app` is copied into the AppImage, so bundled GMT files under:

```text
shiny-app/GSEA/
```

are preserved automatically.

Optional `go-cache` data are copied as packaged seed resources.

If no cache exists, the package still creates the expected packaged cache root so runtime cache population can occur later.

The generated AppImage is currently considered experimental until it has been built and exercised on a native target system.

---

# 5. macOS — experimental portable path

> **Validation status:** the macOS portable-build and DMG packaging paths are implemented for Intel (`x86_64`) and Apple Silicon (`arm64`), but have not yet been manually validated end-to-end on native Macs. Treat these packaging paths as experimental.

## Requirements

Install:

- the exact R version configured in `portable/R_VERSION`;
- Git, when building from a Git checkout;
- Go 1.22 or later;
- Xcode Command Line Tools when native compilation is required.

Install Xcode Command Line Tools:

```bash
xcode-select --install
```

Check architecture:

```bash
uname -m
```

Expected values are:

```text
x86_64
```

or:

```text
arm64
```

Use a native R and Go toolchain matching that architecture.

Do not assume Rosetta makes a mixed-architecture bundle valid.

## Verify R

```bash
cat portable/R_VERSION; Rscript --version
```

The versions must match.

## Build

In Finder, double-click `Build-MiraProt.command`; its interactive terminal remains open so you can read the final status. The terminal equivalent is:

```bash
./Build-MiraProt.command
```

For non-interactive automation use `./Build-MiraProt.sh`. Advanced users may invoke Stage 1 directly with `bash portable/scripts/bundle-r.sh --output-dir portable/dist`. All friendly entry points record bootstrap and build output under `portable/logs/`.

The script creates a flat portable directory containing the copied R runtime, package library, Shiny application, cache resources, and native launcher.

## Launch the flat bundle

```bash
./portable/dist/MiraProt-launcher
```

Because macOS has not yet been manually validated end-to-end, verify at minimum:

- launcher execution;
- system tray behavior;
- browser opening;
- Data Wizard import;
- representative downstream analyses;
- cache access;
- GSEA resource discovery;
- shutdown;
- relaunch.

## Optional DMG

After the flat bundle has been tested:

```bash
bash portable/installers/macos/create-dmg.sh --dist-dir portable/dist --output-dir output
```

The script produces:

```text
output/MiraProt-1.0.0-macos-<architecture>.dmg
```

The app layout places:

```text
shiny-app
```

under:

```text
MiraProt.app/Contents/Resources/app/
```

and bundled GMT files therefore appear under:

```text
MiraProt.app/Contents/Resources/app/GSEA/
```

Optional cache data are packaged under:

```text
MiraProt.app/Contents/Resources/go-cache/
```

and are used as seed data for the writable per-user cache.

### Current macOS packaging caveat

The DMG/application packaging implementation exists, but the application-icon path has not yet been verified on a native packaged build.

Before calling the macOS package release-ready, verify that:

- `MiraProt.app` displays the intended icon;
- the executable passes expected macOS runtime checks;
- the tray icon appears correctly;
- Gatekeeper behavior is understood;
- the complete application launches after being copied from the DMG.

### Signing and Gatekeeper

Locally created MiraProt applications are unsigned and unnotarized unless a separate signing process is performed.

macOS may therefore warn about or block execution of a locally generated app.

Do not disable Gatekeeper globally.

For a trusted local build, use the standard Finder Control-click **Open** workflow or inspect the exact app with macOS security tools.

For distributed builds, an appropriate Developer ID signing and notarization workflow should be used.

---

# 6. GSEA resources

MiraProt does not distribute GMT gene-set files.

Source-mode files are placed directly under:

```text
GSEA/
```

The stage-1 portable builders copy immediate lowercase files matching:

```text
GSEA/*.gmt
```

into:

```text
shiny-app/GSEA/
```

If no GMT files exist, the portable build continues normally.

The correct runtime locations are:

| Mode | Location |
|---|---|
| Source | `<source>/GSEA/` |
| Flat portable | `<bundle>/shiny-app/GSEA/` |
| Windows installer | `<installation>/shiny-app/GSEA/` |
| macOS app | `MiraProt.app/Contents/Resources/app/GSEA/` |
| Linux AppImage | packaged `usr/bin/shiny-app/GSEA/` |

GMT files are intentionally ignored by Git.

If you create a bundle containing third-party GMT files, verify that the applicable terms permit redistribution before sharing that artifact.

Adding a GMT file to the source directory after stage 1 has already been built does not mutate the existing portable bundle.

Either rebuild stage 1 or add the file to the bundle before stage-2 packaging.

---

# 7. Cache behavior

Flat bundles use a writable adjacent:

```text
go-cache/
```

directory.

The cache can contain:

```text
go-cache/
├── annotation_cache/
└── go_cache/
    ├── BioMart_Cache/
    └── organism-specific cache data
```

Windows installed packages, macOS apps, and Linux AppImages use packaged cache data only as **seed resources**.

Writable cache data are stored in per-user application-data locations.

Typical application-data directories are:

| Platform | Directory |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt` |
| macOS | `~/Library/Application Support/MiraProt` |
| Linux | `${XDG_DATA_HOME:-~/.local/share}/MiraProt` |

Existing populated user cache destinations are not overwritten by a packaged seed.

If no prebuilt cache is available, MiraProt can populate required cache data at runtime.

The first affected operation may therefore require internet access and take longer.

---

# 8. Updating or rebuilding

A completed portable bundle can be launched repeatedly.

You do not need to rebuild it for every use.

With a Git checkout, update source with:

```text
git pull --ff-only
```

Then perform a clean rebuild if you want the portable artifact to reflect the updated source.

Windows:

```powershell
Remove-Item -Recurse -Force .\portable\dist -ErrorAction SilentlyContinue; .\Build-MiraProt.cmd
```

Linux/macOS:

```bash
rm -rf portable/dist && ./Build-MiraProt.sh
```

Direct builder commands remain available as advanced alternatives when bypassing bootstrap checks and logging is intentional.

For Windows installer users, rebuild the installer after rebuilding and testing the new stage-1 bundle.

Stage-2 packages do not automatically change when stage 1 changes.

---

# 9. Quick troubleshooting

## Windows builder cannot find Go

```powershell
go version
```

Reopen PowerShell after installing Go.

## Wrong working directory

Windows:

```powershell
Get-Location; Test-Path .\app.R; Test-Path .\portable
```

Linux/macOS:

```bash
pwd; test -f app.R && echo app.R_OK; test -d portable && echo portable_OK
```

## R package installation fails

Read the first package/build error rather than only the final summary.

On Windows, Rtools is required only when a required package genuinely must compile from source.

On Linux/macOS, native R packages may require system development libraries or Xcode Command Line Tools.

## Inno Setup reports `Unknown filename prefix`

Use:

```powershell
& $iscc "/DDistDir=$dist" ".\portable\installers\windows\MiraProt.iss"
```

Do not add quote characters around the actual path value after `=`.

## Installer reports a missing stage-1 component

```powershell
@("MiraProt-launcher.exe","shiny-app","r-portable","r-library") | ForEach-Object { [pscustomobject]@{Component=$_;Present=Test-Path (Join-Path $dist $_)} } | Format-Table -AutoSize
```

## Launcher does not open the browser

The default address is normally:

```text
http://127.0.0.1:3838
```

The launcher may select another available port if 3838 is occupied.

## More diagnostic output

Use the launcher's debug mode:

```text
MiraProt-launcher --debug
```

On Windows:

```powershell
.\MiraProt-launcher.exe --debug
```

Logs are stored under the platform-specific MiraProt application-data directory.

---

# 10. Before claiming Linux/macOS portable support as validated

A successful script run alone is not sufficient.

For Linux, manually test a complete native build and at least:

- launcher startup;
- tray integration;
- browser opening;
- Data Wizard import;
- downstream analysis;
- GO/AnnotationHub cache access;
- BioMart access;
- GMT/GSEA resource use;
- export;
- session save/restore;
- shutdown and relaunch;
- AppImage packaging and launch.

For macOS, test the same workflow plus:

- Intel and/or Apple Silicon architecture as claimed;
- application icon;
- `.app` launch;
- DMG installation;
- Gatekeeper behavior;
- tray integration;
- packaged-cache seeding.

Until those checks have been performed successfully, Linux and macOS portable packaging should remain documented as experimental.
