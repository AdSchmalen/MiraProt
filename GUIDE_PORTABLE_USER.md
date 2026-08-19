# Build and Run MiraProt on Your Computer

This guide creates a local MiraProt bundle from the source code. You do not
need to be a developer, but the first build downloads R and R packages and can
take 30–60 minutes. Allow several gigabytes of free disk space.

### How to use this guide

The instructions are organized as a beginner workflow rather than a list of
developer internals:

1. install the few tools needed to build on your operating system;
2. run a check after each installation and compare the expected result;
3. obtain the MiraProt source and verify that the terminal is in the right
   folder;
4. run one platform-specific build command;
5. verify the generated launcher, then start MiraProt; and
6. optionally add GSEA gene-set files.

Commands are shown on one line whenever possible so they can be copied and
pasted. Text marked **Expected** describes a successful checkpoint; versions
and local paths can differ slightly.

**First-time Windows user:** go directly to the complete
[Windows walkthrough](#windows). It includes installing the tools and cloning
the repository, so do not clone it separately in section 1 first. Linux and
macOS users should obtain the source in section 1 before following their
platform section.

## 1. Get the MiraProt source

Choose **one** of these methods:

### Option A: clone with Git

Open PowerShell (Windows) or Terminal (Linux/macOS), then run:

```bash
git clone https://github.com/AdSchmalen/MiraProt.git
cd MiraProt
```

### Option B: use a source archive

Download a source-code ZIP or tar archive from the MiraProt repository, extract
it, and open PowerShell or Terminal in the extracted folder. The correct folder
contains `portable`, `app.R`, and this guide.

> **Run every build command below from this repository root.** Do not run it
> from inside `portable` or `portable/scripts`.

An archive is fine for a one-time build. A Git clone is easier to update later.

## 2. Build on your operating system

Build tools are used to assemble the bundle; that does not mean they are all
needed to run it. In particular, **R, Go, Git, compilers, and package installers
used during assembly are not automatically runtime prerequisites of the
finished bundle**. MiraProt includes its own copy of R and its R packages.
Linux and macOS bundles can still depend on operating-system shared libraries,
as described below.

### Windows

The Windows portable build currently targets **64-bit x86 Windows (`amd64`)**.
It does not build a Windows ARM64 launcher. Start with a reliable internet
connection, several gigabytes of free disk space, and enough time for the first
package installation.

#### Step 1 — Open Windows PowerShell

Open the **Start** menu, type `PowerShell`, and open **Windows PowerShell**.
Do not choose **Run as administrator**; a normal user window is preferred.

Check that the built-in PowerShell and Windows package manager are available:

```powershell
$PSVersionTable.PSVersion; winget --version
```

**Expected:** a PowerShell version of at least `5.1` and a `winget` version
such as `v1.x`. If `winget` is not found, install or update **App Installer**
from Microsoft Store, then reopen PowerShell. Microsoft's alternative
PowerShell installation methods are documented at
<https://learn.microsoft.com/powershell/scripting/install/installing-powershell-on-windows>.

#### Step 2 — Install the current PowerShell 7 release

PowerShell 5.1 can run the bundler, but PowerShell 7 provides the clearest
current command-line experience. Install the latest stable PowerShell 7
published through Microsoft's `winget` package:

```powershell
winget install --id Microsoft.PowerShell --exact --source winget --accept-package-agreements --accept-source-agreements
```

Close Windows PowerShell. Open the Start menu again and choose
**PowerShell 7**, then check it:

```powershell
$PSVersionTable.PSVersion
```

**Expected:** the first number under `Major` is `7`. Future `7.x` releases are
fine; the guide does not require one particular patch number.

#### Step 3 — Confirm the supported processor architecture

```powershell
[System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
```

**Expected:** `X64`. If it reports `Arm64`, stop: the Windows bundler currently
forces an amd64 launcher and this guide does not claim native Windows ARM64
support.

#### Step 4 — Install Git

Git downloads the MiraProt source and makes later updates simple:

```powershell
winget install --id Git.Git --exact --source winget --accept-package-agreements --accept-source-agreements
```

Close and reopen PowerShell 7 so it receives the updated `PATH`, then check:

```powershell
git --version
```

**Expected:** output beginning with `git version`, for example
`git version 2.x.x.windows.x`.

#### Step 5 — Install Go

Go is needed only to compile `MiraProt-launcher.exe`. It is not included in the
finished bundle and is not needed each time MiraProt runs.

```powershell
winget install --id GoLang.Go --exact --source winget --accept-package-agreements --accept-source-agreements
```

Close and reopen PowerShell 7 again, then check:

```powershell
go version
```

**Expected:** output similar to `go version go1.22.x windows/amd64` or a newer
Go release ending in `windows/amd64`. If `winget` cannot install Go, use the
official Windows installer at <https://go.dev/dl/>, reopen PowerShell 7, and
repeat `go version`.

#### Step 6 — Download MiraProt with Git

The following command switches to your user folder, downloads MiraProt into a
new `MiraProt` folder, and enters it:

```powershell
Set-Location $HOME; git clone https://github.com/AdSchmalen/MiraProt.git; Set-Location "$HOME\MiraProt"
```

**Expected:** Git ends with `done.`, and the prompt's current path ends in
`MiraProt`. If Git says the destination already exists, do not clone over it;
enter the existing checkout with `Set-Location "$HOME\MiraProt"` and use the
update instructions in section 4.

Confirm that this is the repository root:

```powershell
Get-Location; Test-Path .\app.R; Test-Path .\portable\scripts\bundle-r-windows.ps1
```

**Expected:** the path ends in `MiraProt`, followed by `True` and `True`. Do not
continue until both checks are `True`.

If Git cannot be used, download the repository's **Source code (zip)** in a web
browser, extract the complete archive, open PowerShell 7, and enter that folder
with `Set-Location "C:\path\to\the\extracted\MiraProt-folder"`. Run the same
two `Test-Path` checks before continuing.

#### Step 7 — Check internet access and the selected R version

Windows users do **not** install normal system R for this build. The bundler
reads the required version from `portable\R_VERSION`, downloads that Windows R
installer from CRAN, and installs it inside the portable output.

```powershell
Get-Content .\portable\R_VERSION; Test-NetConnection cloud.r-project.org -Port 443 | Select-Object ComputerName,TcpTestSucceeded
```

**Expected:** one R version such as `4.5.2`, and
`TcpTestSucceeded : True`. A false connection result usually means that a
proxy, firewall, DNS setting, or internet connection must be fixed first.

Check free space on the drive containing your user folder:

```powershell
$drive=(Split-Path $HOME -Qualifier).TrimEnd(':'); [math]::Round((Get-PSDrive -Name $drive).Free / 1GB,1)
```

**Expected:** a number representing free gigabytes. At least 10 GB free is a
practical starting point for downloads, temporary files, R, and its package
library; actual use varies by package versions and caches.

#### Step 8 — Build the portable bundle

Stay in the repository root and run this one command:

```powershell
.\portable\scripts\bundle-r-windows.ps1
```

The first build downloads portable R, installs many R packages, optionally
prebuilds caches, copies the MiraProt application, generates Windows resources,
and compiles the launcher. Package installation is normally the longest step.
The CRAN/Bioconductor output can be long, can appear quiet while a package is
compiling, and will differ as package versions change. Do not interrupt it
while it is still producing output. A `Cache pre-build failed` warning is
recoverable because MiraProt can download that cache at runtime. `R package
installation failed`, `Go build failed`, another terminating red error, or a
return to the prompt without `=== Bundle complete ===` means the build did not
finish.

Near the end, **Expected** output includes:

```text
=== Bundle complete ===
To run: ...\portable\dist\MiraProt-launcher.exe
```

The basic bundle does not require Inno Setup. Inno Setup is only used by
developers who deliberately create a separate Windows installer.

`-RVersion` is an advanced override for the **R runtime**, not a MiraProt
application-version selector. Ordinary users should omit it so the maintained
value in `portable\R_VERSION` is used. MiraProt's own version comes from
Git/build metadata and `R/version_info.R`, independently of R, the launcher, a
platform installer, and the saved-session schema. R is installed first into a
unique stage under `%TEMP%`, not directly into `r-portable`. Only a stage with
`R.exe`, `Rscript.exe`, and `bin\x64\R.dll` that passes probes is promoted. An
existing runtime remains recoverable until final-path validation, enabling safe
rollback. `r-library` is the separate package library populated afterward.
Windows R does not provide or require a top-level `VERSION` file. The absolute
staged `R.exe --version` and `Rscript.exe --version` commands are startup probes;
the absolute staged `Rscript.exe` running `getRversion()` is authoritative. The
same query runs again after promotion with inherited R configuration removed,
so an R found elsewhere on `PATH` cannot validate the portable runtime.

#### Step 9 — Verify the result

```powershell
Test-Path .\portable\dist\MiraProt-launcher.exe; Test-Path .\portable\dist\r-portable\bin\Rscript.exe; Test-Path .\portable\dist\shiny-app\app.R
```

**Expected:** `True`, `True`, and `True`.

Check the launcher without starting the application:

```powershell
.\portable\dist\MiraProt-launcher.exe --version
```

**Expected:** a line beginning with `MiraProt Launcher` and ending with
`(windows/amd64)`.

Check the bundled R runtime:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --version
```

**Expected:** `Rscript (R) version` followed by the version printed earlier
from `portable\R_VERSION`.

Check that the bundled library can load Shiny:

```powershell
& .\portable\dist\r-portable\bin\Rscript.exe --vanilla -e ".libPaths(c(normalizePath('portable/dist/r-library'),.libPaths())); stopifnot(requireNamespace('shiny',quietly=TRUE)); cat('OK\n')"
```

**Expected:** `OK`. If this check stops with an error, package installation was
incomplete; use the package troubleshooting below before launching MiraProt.

If runtime installation fails, the bundler retains the failed stage and a
separate installer/probe log directory by default and prints both paths. Put
the printed stage path in `$stage` to inspect the evidence:

```powershell
$stage = 'C:\path\printed\by\the\bundler'; $logs = "$stage-logs"
Get-Item "$stage\bin\R.exe", "$stage\bin\Rscript.exe", "$stage\bin\x64\R.dll" | Select FullName,Length,LastWriteTime,@{n='FileVersion';e={$_.VersionInfo.FileVersion}},@{n='ProductVersion';e={$_.VersionInfo.ProductVersion}}
Get-ChildItem Env: | Where-Object Name -in 'R_HOME','R_ARCH','R_LIBS','R_LIBS_USER','R_LIBS_SITE','R_ENVIRON','R_ENVIRON_USER','R_PROFILE','R_PROFILE_USER'
& "$stage\bin\R.exe" --version; $code=$LASTEXITCODE; '{0} (0x{1:X8})' -f $code,[uint32]$code
& "$stage\bin\Rscript.exe" --version; $code=$LASTEXITCODE; '{0} (0x{1:X8})' -f $code,[uint32]$code
& "$stage\bin\Rscript.exe" --vanilla -s -e "cat(as.character(getRversion()))"; $code=$LASTEXITCODE; "`n$code (0x$('{0:X8}' -f [uint32]$code))"
Get-ChildItem $logs; Get-Content "$logs\installer.log"
```

These show file metadata, inherited R variables (ignored for bundler probes),
signed/hex exit statuses, and expression output. Probe logs retain stdout and
stderr. Do not manually promote a failed stage; retain it when asking for help.

#### Step 10 — Start and stop MiraProt

```powershell
.\portable\dist\MiraProt-launcher.exe
```

**Expected:** the launcher reports a local address, normally
`http://127.0.0.1:3838`, and MiraProt opens in the default web browser. Keep the
PowerShell window open while using MiraProt.

Use the MiraProt tray icon to quit. If there is no usable tray icon, return to
PowerShell and press **Ctrl+C**. Closing only the browser tab may leave MiraProt
running. You can launch the completed bundle again without rebuilding it.
Keep the entire `portable\dist` directory together. Normal launches use the R
runtime and packages inside that directory; PowerShell 7, Git, Go, Rtools, and
system R are not required merely to start the finished Windows bundle.

#### Windows security and build tools

During the build, the script installs the pinned `go-winres` helper through Go
and uses it to add the icon and version information to the launcher. Windows
Smart App Control or an organization's application-control policy can block
that helper. Typical symptoms are a security notification, `go-winres` being
blocked, or failure during `Generating Windows resources`.

**Do not disable Smart App Control or organizational security controls as the
standard workaround.** Smart App Control may not be designed to be switched
back on without resetting Windows. Prefer a trusted build workstation, a
controlled virtual machine, GitHub Actions, or signed/trusted build
infrastructure. This is a build-time helper issue; it does not automatically
mean that the finished MiraProt launcher is incompatible.

If PowerShell blocks only `bundle-r-windows.ps1` after you downloaded a source
ZIP, first verify that the archive came from the official MiraProt repository.
Then remove the download marker from that one script and rerun it:

```powershell
Unblock-File .\portable\scripts\bundle-r-windows.ps1
```

Do not use `Set-ExecutionPolicy Unrestricted` or weaken the machine-wide
execution policy for this build. PowerShell script blocking and Smart App
Control blocking `go-winres.exe` are different problems; `Unblock-File` does
not bypass application control.

The package installer prefers Windows binary R packages. If it explicitly says
that a dependency must compile from source and no build tools are available,
install the Rtools release matching the version shown by
`Get-Content .\portable\R_VERSION`, reopen PowerShell, remove the incomplete
`portable\dist` folder, and retry. Rtools is not a normal runtime dependency.

### Linux (Ubuntu/Debian-family local-build path only)

Ubuntu/Debian-family Linux is the currently implemented local-build path. The
script expects tools such as `apt-get` and `dpkg`, and Ubuntu amd64 is the
CI-tested Linux target. Fedora/RHEL-family, Arch-family, and openSUSE are **not
supported by the current dependency-installation block**. Do not use or
publish guessed `dnf`, `pacman`, or `zypper` translations until package
mappings and complete builds for those families have been implemented and
verified.

#### Before building

Install R matching the exact version in `portable/R_VERSION`, Git, Go, `rsync`,
and `gcc`. The following development libraries are used by the bundler or when
R packages and the tray-enabled launcher are compiled:

```bash
sudo apt-get update
sudo apt-get install -y \
  r-base golang-go git rsync gcc \
  libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  libtiff5-dev libjpeg-dev libpng-dev librsvg2-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libgtk-3-dev libayatana-appindicator3-dev
```

The bundling script itself checks for and, when absent, installs these eight
packages: `libfreetype6-dev`, `libfontconfig1-dev`, `libharfbuzz-dev`,
`libfribidi-dev`, `libtiff5-dev`, `libjpeg-dev`, `libpng-dev`, and
`librsvg2-dev`. Installing the full list above first also covers compilation of
the R packages and Go launcher.

**Build libraries versus runtime libraries:** the `-dev` packages, `gcc`, Go,
Git, and the package installer are assembly tools. The completed bundle still
uses Linux shared libraries supplied by the operating system. Keep the runtime
counterparts of the graphics/network libraries above installed (FreeType,
Fontconfig, HarfBuzz, FriBidi, TIFF, JPEG, PNG, librsvg, libcurl, OpenSSL, and
libxml2). The tray launcher additionally needs `libgtk-3-0` and
`libayatana-appindicator3-1`:

```bash
sudo apt-get install -y libgtk-3-0 libayatana-appindicator3-1
```

Runtime package names for libraries such as OpenSSL can differ between
Ubuntu/Debian releases; installing the development packages above normally
pulls in the matching runtime packages. R itself is copied into the bundle and
the system R is not automatically required afterward.

The Linux result is relocatable within compatible systems rather than
completely self-contained. Copied R and native R packages can retain
dependencies on glibc, libstdc++, OpenSSL, libcurl, libxml2, font and graphics
libraries, and desktop-integration libraries such as GTK and AppIndicator.
For an artifact intended for other machines, build on the oldest compatible
target distribution you plan to support. Before release, inspect native
dependencies with platform tools such as `ldd` and `readelf`, use the target
package manager's query tools to identify providers, and run the bundle on
every claimed target system and architecture.

#### Build

Confirm that `Rscript --version` reports the version in `portable/R_VERSION`,
then run this exact command from the repository root:

```bash
bash portable/scripts/bundle-r.sh --output-dir portable/dist
```

Using `bash` is intentional until executable-bit behavior is verified. The
script also accepts `--r-version VERSION`, which selects only the R runtime,
not the MiraProt application version. Ordinary users should omit it so
`portable/R_VERSION` supplies the maintained default. Command-line options take
precedence over the `R_VERSION` and `OUTPUT_DIR` environment-variable fallbacks. With no
output option or environment override, the output is always `portable/dist`,
resolved from the script's location rather than the current working directory.
The finished launcher is:

```text
portable/dist/MiraProt-launcher
```

#### Launch and stop

```bash
./portable/dist/MiraProt-launcher
```

Quit from the tray icon. When running from a terminal, **Ctrl+C** also stops it.
Closing only the browser tab may not stop the background process.

### macOS

#### Before building

You need Git, Go 1.22 or newer, `rsync` (included with macOS), internet access,
and a native installation of the exact R version in `portable/R_VERSION`.
Install the Xcode Command Line Tools when an R package or the launcher must be
compiled from source:

```bash
xcode-select --install
```

The architecture must match throughout the build. The copied R runtime,
compiled R packages, Go launcher, and (when used) DMG packaging host must all
have compatible native architectures. On an Intel Mac, use Intel (`x86_64`)
R and Go and package on Intel. On an Apple Silicon Mac, use native Apple
Silicon (`arm64`) R and Go and package on Apple Silicon. The CI evidence covers
Intel builds on `macos-13` and Apple Silicon builds on `macos-14`. MiraProt does
not assemble a universal binary, and Rosetta operation has not been tested or
claimed; do not mix architectures merely because Rosetta may make one build
tool executable.

R, Go, Git, Xcode's compiler, and package installers used to assemble the
bundle are not automatically required to run the result. The copied R runtime
and compiled packages may, however, continue to use compatible macOS system
libraries. Moving a bundle to a different Mac therefore requires the same CPU
architecture and a compatible macOS version.

#### Build

Confirm that `Rscript --version` reports the version in `portable/R_VERSION`,
then run from the repository root:

```bash
bash portable/scripts/bundle-r.sh --output-dir portable/dist
```

This is the complete **basic bundling** workflow. `portable/scripts/bundle-r.sh`
also accepts `--r-version VERSION`, which selects only R, not the MiraProt
application version. Ordinary users should omit it and use the maintained
`portable/R_VERSION` default. Its command-line options override the `R_VERSION`
and `OUTPUT_DIR` environment-variable fallbacks. When neither an
output option nor `OUTPUT_DIR` is supplied, the default remains
`portable/dist` from any working directory because it is resolved relative to
the script. The script produces a flat distribution directory containing the
copied R runtime, R
packages, Shiny application, and this native launcher; it does not create an
application bundle or disk image:

```text
portable/dist/MiraProt-launcher
```

#### Optional app and DMG packaging

Packaging is a separate, optional step after the flat directory has been
built. On the same compatible native architecture, run:

```bash
bash portable/installers/macos/create-dmg.sh \
  --dist-dir portable/dist \
  --version 1.0.0 \
  --output-dir output
```

This produces `output/MiraProt-1.0.0-macos-<uname-m>.dmg` (in general,
`MiraProt-<version>-macos-<uname-m>.dmg`) containing `MiraProt.app`. Packaging
only rearranges the already-built flat distribution; it does not make mixed
architectures compatible.

#### macOS security for local builds

Locally created `MiraProt.app` bundles are unsigned and unnotarized. Gatekeeper
or quarantine may therefore warn, block, or require an explicit confirmation
when the app or flat launcher is opened. Those prompts are macOS runtime
security behavior, not evidence that R packages or the Go launcher failed to
compile.

For builds intended for distribution, use an appropriate Developer ID to sign
the nested code and app, then notarize and staple the distributed package.
For a trusted local build, inspect only the item you intend to run, for example
with `codesign --verify --deep --strict /path/to/MiraProt.app`,
`spctl --assess --type execute --verbose=4 /path/to/MiraProt.app`, and
`xattr -p com.apple.quarantine /path/to/MiraProt.app`. An unsigned local app is
expected to fail the signature or policy assessment. After independently
verifying its source and contents, use Finder's Control-click **Open** flow; if
necessary, remove quarantine only from that trusted app with
`xattr -dr com.apple.quarantine /path/to/MiraProt.app` and open it again. Apply
the analogous targeted check to `portable/dist/MiraProt-launcher` when using
the flat bundle. Never disable Gatekeeper globally.

#### Launch and stop

```bash
./portable/dist/MiraProt-launcher
```

Quit from the tray icon, or press **Ctrl+C** in the Terminal window that
started it. Closing only the browser tab may leave MiraProt running.

## 3. What to expect when launching

The launcher starts the bundled R/Shiny application and normally opens the
default browser. If no page opens, try `http://127.0.0.1:3838`. MiraProt selects
another available port when 3838 is already occupied.

Keep the **entire** `portable/dist` folder together: the launcher needs the
neighboring `r-portable`, `r-library`, and `shiny-app` folders. In a flat
Windows, Linux, or macOS bundle, the adjacent `go-cache/` is application data,
not a log directory: it holds the writable AnnotationHub, organism, and BioMart
caches. If it is absent, the flat launcher creates it beside itself. Therefore
the portable directory **must be writable by the user running MiraProt**. Put
the repository and finished bundle in a location such as your Documents folder;
do not run the flat bundle from read-only media, a protected system folder, or
a location with restrictive corporate permissions.

Logs and the single-instance `launcher.lock` are separate from that adjacent
cache. They live in the per-user application-data directory below (`logs/` and
`launcher.lock`, respectively):

| System | Application-data location |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt` |
| macOS | `~/Library/Application Support/MiraProt` |
| Linux | `~/.local/share/MiraProt` |

Installed package formats behave differently. A macOS `.app`/DMG or Linux
AppImage treats its packaged `go-cache/` as read-only seed data and copies it,
on first launch and only when the destination is empty, into
`<application-data>/cache/`. A flat portable directory (including the Windows
installer's installed layout) uses its adjacent writable `go-cache/` directly.

Cache prebuild is optional on every platform. When a build contains no usable
prebuilt cache—or the cache is absent on first use—MiraProt creates the writable
cache location and AnnotationHub/organism features download the data they need
at runtime. The first affected operation can consequently be slower and needs
internet access; later uses reuse the downloaded files. BioMart and STRING are
online services as well. These runtime requirements are separate from the build
tools.

### Add gene sets for GSEA

MiraProt discovers GSEA collections from `.gmt` files that you provide. Obtain
the desired collections from [MSigDB](https://www.gsea-msigdb.org/gsea/msigdb/)
or another source whose terms permit your use. MSigDB may require registration,
authentication, and acceptance of its current terms.

The correct directory depends on how MiraProt is running:

| MiraProt version | Where to place `.gmt` files |
|---|---|
| Non-portable/source version | `<MiraProt source repository>/GSEA/` |
| Portable flat bundle | `<portable bundle>/shiny-app/GSEA/` (with the default build: `portable/dist/shiny-app/GSEA/`) |
| macOS `MiraProt.app` | `MiraProt.app/Contents/Resources/app/GSEA/` |

For the **non-portable/source version**, add files to the top-level `GSEA/`
folder beside `app.R`. This is the directory used when MiraProt is started from
the repository root. Do not put source-mode GMT files in
`portable/dist/shiny-app/GSEA/`; that is a separate copy used only by an
already-built portable bundle.

For a **portable flat bundle**, after the build completes:

1. Open the exact folder `portable/dist/shiny-app/GSEA/`.
2. Place each `.gmt` file directly in that folder. Do not put it in a
   subdirectory: MiraProt scans only the immediate contents of `GSEA/`.
3. If MiraProt is already running, open its GSEA module and click **Refresh
   Gene Sets**. Restarting MiraProt also causes a fresh initial scan.
4. Confirm that the file name appears in **Select Gene Set File** before
   starting the analysis.

Files added to the source repository's `GSEA/` folder **after** building are not
automatically copied into an existing portable bundle. Add them to the
portable bundle too, or rebuild the bundle.

Use the lowercase `.gmt` extension. The current filename match is
case-sensitive on case-sensitive filesystems, so a file ending in `.GMT` may
not be listed. An AppImage is read-only after it is packaged, so add the GMT
files to `portable/dist/shiny-app/GSEA/` **before** running
`create-appimage.sh`; create a new AppImage when its collections change.

## 4. Rebuild or update

You do not need to rebuild for each launch. Rebuild after updating MiraProt, or
when you intentionally want newer bundled packages or a clean cache.

On startup, the launcher checks the latest GitHub Release tag and may notify
you when that tag is newer than the version you are running. MiraProt's
authoritative distribution is source-only: the check performs no in-place
update and does not download or install any software automatically. Obtain the
newer source using one of the methods below, then rebuild the portable
installation yourself.

With a Git clone, update from the repository root:

```bash
git pull --ff-only
```

For a source archive, download and extract the newer archive instead. Then
delete the old generated `portable/dist` folder and run the platform's build
command again. On Windows PowerShell:

```powershell
Remove-Item -Recurse -Force .\portable\dist
.\portable\scripts\bundle-r-windows.ps1
```

On verified Ubuntu/Debian or macOS:

```bash
rm -rf portable/dist
bash portable/scripts/bundle-r.sh
```

Deleting `portable/dist` is important for a completely clean rebuild. Updating
the source alone does not change a bundle that was already generated.

## 5. Quick failure recovery

1. **Build stops immediately:** verify that the terminal is at the repository
   root and that Git and Go are found (`git --version`, `go version`). On
   Linux/macOS also check `Rscript --version` and `rsync --version`.
2. **The requested R version does not match (Linux/macOS):** install/select the
   exact version shown in `portable/R_VERSION`, remove `portable/dist`, and
   rebuild. Windows downloads that version automatically.
3. **An R package will not install:** check internet access and read the first
   missing-library message. Reinstall the Linux build-library list above; on
   macOS install Xcode Command Line Tools; on Windows use Rtools only when the
   error says source compilation is required.
4. **A partial build behaves strangely:** delete `portable/dist` and rebuild.
5. **The launcher says another instance is running:** stop the existing
   instance. If it has crashed, remove `launcher.lock` from the application-data
   location in the table above, then retry.
6. **The browser does not open:** visit `http://127.0.0.1:3838`, or start the
   launcher with `--port 5000` and visit that port.
7. **More detail is needed:** start the launcher from a terminal with `--debug`
   and inspect the newest file in the application-data `logs` folder. See
   `GUIDE_PORTABLE_DEV.md` for advanced build and packaging diagnostics.
