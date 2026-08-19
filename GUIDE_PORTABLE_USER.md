# MiraProt Local Portable Build — User Guide

MiraProt's portable desktop application is generated from the source repository
on the computer where it will run. Public, prebuilt portable binaries are not the
normal installation path. The standard workflow is:

1. obtain the MiraProt source repository;
2. install the prerequisites for your platform;
3. run the local bundler;
4. optionally add external resources; and
5. launch the generated artifact.

The result contains the MiraProt application, an R runtime and its packages, and
a small launcher that opens the locally running Shiny application in your web
browser. R and RStudio are not required *after* the bundle has been built.

## 1. Obtain the source repository

Install Git, open a terminal, and clone the authoritative source repository:

```bash
git clone https://github.com/AdSchmalen/MiraProt.git
cd MiraProt
```

To use a particular source version, check out its branch, tag, or commit before
building. A GitHub-generated source archive may be used instead of Git: extract
it, open a terminal in the extracted `MiraProt` directory, and continue below.
Do not look for an installer, AppImage, DMG, or portable archive on the Releases
page as the normal way to install MiraProt.

## 2. Install platform prerequisites

All platforms need internet access while building (for R and package downloads),
several gigabytes of free disk space, and:

- **Go 1.22 or later** (`go version`)
- **R 4.6.0** available as `Rscript` on Linux/macOS
- **Git** to clone and later update the source

### Windows

Install:

- [Go](https://go.dev/dl/)
- PowerShell 5.1 or later (included with Windows 10/11)

The Windows bundler downloads and installs its own R 4.6.0 copy into the
generated bundle; a system R installation is not required.

### macOS

Install R, Go, and the Xcode command-line tools. For example, with Homebrew:

```bash
brew install go r
xcode-select --install
```

`rsync` is included with macOS.

### Ubuntu/Debian Linux

Install R, Go, `rsync`, a compiler, and the package/system-tray development
libraries:

```bash
sudo apt-get update
sudo apt-get install -y r-base golang-go rsync gcc \
  libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  libtiff5-dev libjpeg-dev libpng-dev librsvg2-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libgtk-3-dev libayatana-appindicator3-dev
```

Equivalent packages may be used on another supported Linux distribution. The
bundler itself currently expects Debian/Ubuntu package tooling when it needs to
install a missing library.

## 3. Run the local bundler

Run the command from the repository root. A first build commonly takes 30–60
minutes because R packages must be downloaded and compiled.

### Windows (PowerShell)

```powershell
.\portable\scripts\bundle-r-windows.ps1 -RVersion "4.6.0" -OutputDir ".\portable\dist"
```

### macOS or Linux

```bash
chmod +x portable/scripts/bundle-r.sh
./portable/scripts/bundle-r.sh --r-version 4.6.0 --output-dir ./portable/dist
```

The generated local artifact is `portable/dist/`, with this principal layout:

```text
portable/dist/
├── MiraProt-launcher       # MiraProt-launcher.exe on Windows
├── shiny-app/
├── r-portable/
├── r-library/
└── go-cache/
```

This directory is generated output, is normally ignored by Git, and should not
be committed. Keep it if you plan to run the same build again.

## 4. Add optional external resources

This step is optional. Most features work without adding anything after the
build; resources needed by online modules can be fetched and cached at runtime.

To preseed annotation data, place compatible cache content in the generated
artifact before launching:

- GO/organism data: `portable/dist/go-cache/go_cache/`
- BioMart data: `portable/dist/go-cache/go_cache/BioMart_Cache/`
- AnnotationHub data: `portable/dist/go-cache/annotation_cache/`

Preserve each cache's directory structure and do not merge independent
AnnotationHub cache databases. As an alternative, place project caches under
`cache/GO_Cache/` or `cache/BioMart_Cache/` **before** running the bundler; the
bundler seeds them into the corresponding generated locations.

## 5. Launch the locally generated artifact

### Windows

Double-click:

```text
portable\dist\MiraProt-launcher.exe
```

Or run it from PowerShell:

```powershell
.\portable\dist\MiraProt-launcher.exe
```

### macOS or Linux

```bash
./portable/dist/MiraProt-launcher
```

The launcher starts its bundled R/Shiny process and normally opens the default
browser automatically. If it does not, open `http://127.0.0.1:3838`. It may use
the next available port if 3838 is occupied.

The same generated launcher can be used repeatedly without rebuilding. Closing
and reopening MiraProt does not reinstall R or its packages. Generally rebuild
only when you check out newer MiraProt source or intentionally want to change
the bundled R version, packages, caches, or other bundled environment. Re-run
the same bundler command to rebuild; remove `portable/dist/` first when you need
a completely clean environment.

## Updating MiraProt

Update the source checkout, then rebuild the local artifact:

```bash
git pull --ff-only
rm -rf portable/dist
./portable/scripts/bundle-r.sh --r-version 4.6.0 --output-dir ./portable/dist
```

On Windows, use `Remove-Item -Recurse -Force .\portable\dist` and rerun the
PowerShell bundler. Updating source files alone does not modify an already
generated artifact because the bundler copied the application into it.

## Runtime data and internet access

Most analysis remains local. GO/AnnotationHub may need internet access the first
time an organism is used, STRING connects for each network query, and biomaRt
connects to Ensembl for annotation queries. Downloaded caches and logs are kept
with the portable cache or in the platform application-data location.

| OS | Application-data directory |
|---|---|
| Windows | `%LOCALAPPDATA%\MiraProt` |
| macOS | `~/Library/Application Support/MiraProt` |
| Linux | `~/.local/share/MiraProt` |

## Troubleshooting

- Run the launcher from a terminal with `--debug` and inspect the latest file
  in the application-data `logs/` directory.
- If another instance is reported, close it; stale `launcher.lock` files can be
  removed from the application-data directory.
- Select a different preferred port with `--port 5000`.
- On Linux, install `libgtk-3-0` and `libayatana-appindicator3-1` if the tray
  icon is unavailable. The application can still run from a terminal.
- For detailed build and packaging diagnostics, see `GUIDE_PORTABLE_DEV.md`.
