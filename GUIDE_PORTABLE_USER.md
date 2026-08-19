# Build and Run MiraProt on Your Computer

This guide creates a local MiraProt bundle from the source code. You do not
need to be a developer, but the first build downloads R and R packages and can
take 30–60 minutes. Allow several gigabytes of free disk space.

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

#### Before building

You need:

- Windows PowerShell 5.1 or newer;
- Git (also needed when building from an archive because the script records
  version information);
- Go 1.22 or newer; and
- internet access for R, Go tools, and R package downloads.

The basic bundle does **not** require a normal system installation of R. The
script downloads the required R version into the bundle. It also does **not**
require Inno Setup; that is only for making a separate installer.

Most R packages have ready-made Windows binaries. If a package instead has to
compile from source and reports that build tools are missing, install the
Rtools release appropriate for the bundled R version and retry. Rtools is a
fallback for assembly, not a normal runtime requirement.

#### Build

From the repository root in PowerShell, run:

```powershell
.\portable\scripts\bundle-r-windows.ps1
```

> **Windows security note:** Windows Smart App Control or a managed
> application-control policy may block `go-winres.exe` while the script is
> compiling the launcher. This affects the build helper and does not, by
> itself, establish that the finished MiraProt application is incompatible.
> Use a trusted build workstation or controlled VM, GitHub Actions, or other
> signed/trusted build infrastructure rather than disabling Smart App Control
> or weakening your organization's policy by default.

When it finishes, the launcher is:

```text
portable\dist\MiraProt-launcher.exe
```

#### Launch and stop

Double-click `MiraProt-launcher.exe`, or run:

```powershell
.\portable\dist\MiraProt-launcher.exe
```

Use the MiraProt tray icon to quit. If it was started in PowerShell and there
is no usable tray icon, return to that window and press **Ctrl+C**. Closing only
the browser tab may leave MiraProt running.

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
bash portable/scripts/bundle-r.sh
```

Using `bash` is intentional until executable-bit behavior is verified. The
finished launcher is:

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

The architecture must match throughout the build. On an Intel Mac, use Intel
(`x86_64`) R and Go. On an Apple Silicon Mac, use native Apple Silicon
(`arm64`) R and Go. Do not mix an Intel R running through Rosetta with a native
Apple Silicon build (or the reverse), because compiled packages may not load.

R, Go, Git, Xcode's compiler, and package installers used to assemble the
bundle are not automatically required to run the result. The copied R runtime
and compiled packages may, however, continue to use compatible macOS system
libraries. Moving a bundle to a different Mac therefore requires the same CPU
architecture and a compatible macOS version.

#### Build

Confirm that `Rscript --version` reports the version in `portable/R_VERSION`,
then run from the repository root:

```bash
bash portable/scripts/bundle-r.sh
```

The basic workflow creates a **flat launcher**, not an application or disk
image:

```text
portable/dist/MiraProt-launcher
```

Optional DMG packaging is deliberately outside this basic workflow.

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
neighboring `r-portable`, `r-library`, `shiny-app`, and cache folders. Put the
repository and finished bundle in a location where your account can write,
such as your Documents folder. Avoid read-only media, protected system folders,
and locations managed with restrictive corporate permissions. MiraProt writes
logs and working data to these application-data locations:

| System | Application-data location |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt` |
| macOS | `~/Library/Application Support/MiraProt` |
| Linux | `~/.local/share/MiraProt` |

Some features still need internet access while running: STRING and biomaRt use
online services, and organism/AnnotationHub data may be downloaded on first
use. Those online-service requirements are separate from the build tools.

## 4. Rebuild or update

You do not need to rebuild for each launch. Rebuild after updating MiraProt, or
when you intentionally want newer bundled packages or a clean cache.

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
