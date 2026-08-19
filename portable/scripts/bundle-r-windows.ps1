# bundle-r-windows.ps1 — Create a portable MiraProt distribution for Windows
#
# Usage:
#   .\bundle-r-windows.ps1 [-RVersion "4.6.0"] [-OutputDir ".\dist"]
#
# Prerequisites:
#   - PowerShell 5.1+
#   - Internet access (to download R installer)
#   - Go toolchain (for building the launcher)

[CmdletBinding()]
param(
    [string]$RVersion = "4.6.0",
    [string]$OutputDir = (Join-Path $PSScriptRoot "..\dist")
)

$ErrorActionPreference = "Stop"

$ScriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..\..")).Path

Write-Host "=== MiraProt Portable Bundler (Windows) ===" -ForegroundColor Cyan
Write-Host "R version: $RVersion"
Write-Host "Output:    $OutputDir"
Write-Host ""

# Create output directory and resolve to absolute path so it stays correct after Push-Location
New-Item -ItemType Directory -Force -Path $OutputDir  | Out-Null
$OutputDir = (Resolve-Path $OutputDir).Path

$RPortable = Join-Path $OutputDir "r-portable"
$RLibrary  = Join-Path $OutputDir "r-library"
$ShinyApp  = Join-Path $OutputDir "shiny-app"

New-Item -ItemType Directory -Force -Path $RLibrary   | Out-Null

# -----------------------------------------------------------------------
# Step 1: Download and install portable R for Windows
# -----------------------------------------------------------------------
$RscriptPath = Join-Path $RPortable "bin\Rscript.exe"

if (Test-Path $RscriptPath) {
    Write-Host "--- R already present at $RPortable ---"
} else {
    Write-Host "--- Downloading R $RVersion for Windows ---"

    $RUrl = "https://cran.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
    $RInstaller = Join-Path $env:TEMP "R-$RVersion-win.exe"

    if (-not (Test-Path $RInstaller)) {
        Write-Host "Downloading from: $RUrl"
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -Uri $RUrl -OutFile $RInstaller -UseBasicParsing
    } else {
        Write-Host "Using cached installer: $RInstaller"
    }

    # Silent install to the portable directory
    Write-Host "Installing R to $RPortable (this may take a few minutes)..."
    $installArgs = @(
        "/VERYSILENT"
        "/DIR=$RPortable"
        "/NORESTART"
        "/SUPPRESSMSGBOXES"
        "/COMPONENTS=main,x64"
    )
    Start-Process -FilePath $RInstaller -ArgumentList $installArgs -Wait -NoNewWindow

    if (-not (Test-Path $RscriptPath)) {
        Write-Error "R installation failed - Rscript.exe not found at $RscriptPath"
        exit 1
    }

    Write-Host "Portable R installed at: $RPortable"
}
Write-Host ""

# -----------------------------------------------------------------------
# Step 2: Install R packages
# -----------------------------------------------------------------------
Write-Host "--- Installing R packages into $RLibrary ---"

$InstallScript = Join-Path $ScriptDir "install-packages.R"
& $RscriptPath $InstallScript $RLibrary

if ($LASTEXITCODE -ne 0) {
    Write-Error "R package installation failed with exit code $LASTEXITCODE"
    exit 1
}
Write-Host ""

# -----------------------------------------------------------------------
# Step 3: Pre-build AnnotationHub cache
# -----------------------------------------------------------------------
$GoCache = Join-Path $OutputDir "go-cache"

if ((Test-Path (Join-Path $GoCache "annotation_cache")) -and (Test-Path (Join-Path $GoCache "go_cache"))) {
    Write-Host "--- go-cache already present at $GoCache ---"
} else {
    Write-Host "--- Pre-building AnnotationHub cache into $GoCache ---"
    New-Item -ItemType Directory -Force -Path $GoCache | Out-Null
    & $RscriptPath (Join-Path $ScriptDir "prebuild-cache.R") $GoCache $RLibrary

    if ($LASTEXITCODE -ne 0) {
        Write-Warning "Cache pre-build failed (exit code $LASTEXITCODE) - portable app will download on first launch"
    }
}
Write-Host ""

# -----------------------------------------------------------------------
# Step 3b: Seed go-cache from the project's non-portable ./cache/ folder
# -----------------------------------------------------------------------
# The non-portable app persists its annotation caches in:
#   ./cache/GO_Cache/       - GO / AnnotationHub organism caches
#   ./cache/BioMart_Cache/  - BioMart species + mapping caches
#
# The portable launcher exposes these at runtime via:
#   MIRAPROT_GO_CACHE   -> go-cache\go_cache\
#   ANNOTATION_HUB_CACHE -> go-cache\annotation_cache\
#
# BioMart in portable mode stores its cache in $MIRAPROT_GO_CACHE\BioMart_Cache.
# Merge the developer's cache into the portable distribution so every cached
# database file is shipped - not just the single organism downloaded by
# prebuild-cache.R. Existing files are overwritten so the project cache wins.
$ProjectCache = Join-Path $ProjectRoot "cache"
if (Test-Path $ProjectCache) {
    Write-Host "--- Seeding go-cache from $ProjectCache ---"

    $ProjectGoCache      = Join-Path $ProjectCache "GO_Cache"
    $ProjectBioMartCache = Join-Path $ProjectCache "BioMart_Cache"

    if (Test-Path $ProjectGoCache) {
        $PortableGoCacheSub = Join-Path $GoCache "go_cache"
        Write-Host "Copying cache\GO_Cache -> $PortableGoCacheSub"
        New-Item -ItemType Directory -Force -Path $PortableGoCacheSub | Out-Null
        # Per-organism <orgdb>.sqlite files land at
        # go-cache\go_cache\<orgdb>\<orgdb>.sqlite, which load_organism_cache()
        # finds via its canonical-path fallback (GO_module_hub.R:1372-1382).
        # Nested ah_cache\ folders are preserved but unused in portable mode -
        # we intentionally do NOT merge them into annotation_cache\ because
        # each is its own BiocFileCache with a SQLite index, and merging
        # would clobber the index and leave orphan blobs.
        & robocopy $ProjectGoCache $PortableGoCacheSub /E /NFL /NDL /NJH /NJS /NP *> $null
        if ($LASTEXITCODE -ge 8) {
            Write-Warning "robocopy failed while copying GO_Cache (exit $LASTEXITCODE)"
        }
        $global:LASTEXITCODE = 0
    } else {
        Write-Host "No cache\GO_Cache\ directory found - skipping GO cache seed."
    }

    if (Test-Path $ProjectBioMartCache) {
        $PortableBioMart = Join-Path $GoCache "go_cache\BioMart_Cache"
        Write-Host "Copying cache\BioMart_Cache -> $PortableBioMart"
        New-Item -ItemType Directory -Force -Path $PortableBioMart | Out-Null
        & robocopy $ProjectBioMartCache $PortableBioMart /E /NFL /NDL /NJH /NJS /NP *> $null
        if ($LASTEXITCODE -ge 8) {
            Write-Warning "robocopy failed while copying BioMart_Cache (exit $LASTEXITCODE)"
        }
        $global:LASTEXITCODE = 0
    } else {
        Write-Host "No cache\BioMart_Cache\ directory found - skipping BioMart cache seed."
    }
} else {
    Write-Host "--- No .\cache\ folder at $ProjectCache - nothing to seed ---"
}
Write-Host ""

# -----------------------------------------------------------------------
# Step 4: Copy Shiny application
# -----------------------------------------------------------------------
Write-Host "--- Copying Shiny application ---"

if (Test-Path $ShinyApp) {
    Remove-Item $ShinyApp -Recurse -Force
}

# Use robocopy for efficient directory copy with exclusions
$excludeDirs = @(".git", "cache", "portable", ".Rproj.user", ".Ruserdata", "user_data", "dist")
# If OutputDir lives inside the project root, exclude it explicitly to avoid
# recursively copying previously generated portable bundles into shiny-app/.
if ($OutputDir.StartsWith($ProjectRoot, [System.StringComparison]::OrdinalIgnoreCase)) {
    $excludeDirs += $OutputDir
}
$excludeFiles = @(".RData", ".Rhistory")

$robocopyArgs = @(
    $ProjectRoot
    $ShinyApp
    "/E"       # Copy subdirectories including empty ones
    "/NFL"     # No file list
    "/NDL"     # No directory list
    "/NJH"     # No job header
    "/NJS"     # No job summary
    "/NP"      # No progress percentage
)

foreach ($dir in $excludeDirs) {
    $robocopyArgs += "/XD"
    $robocopyArgs += $dir
}
foreach ($file in $excludeFiles) {
    $robocopyArgs += "/XF"
    $robocopyArgs += $file
}

# Suppress all robocopy output (including locale-specific text)
& robocopy @robocopyArgs *> $null
# robocopy returns non-zero for success (1 = files copied), only 8+ is error
if ($LASTEXITCODE -ge 8) {
    Write-Error "File copy failed with robocopy exit code $LASTEXITCODE"
    exit 1
}

# Report results in English
$fileCount = (Get-ChildItem -Path $ShinyApp -Recurse -File).Count
$sizeMB = [math]::Round((Get-ChildItem -Path $ShinyApp -Recurse -File | Measure-Object -Property Length -Sum).Sum / 1MB, 1)
Write-Host "App copied to: $ShinyApp ($fileCount files, $sizeMB MB)"
Write-Host ""

# -----------------------------------------------------------------------
# Step 5: Build Go launcher
# -----------------------------------------------------------------------
Write-Host "--- Building Go launcher ---"

$LauncherDir = Join-Path $ScriptDir "..\launcher"

Push-Location $LauncherDir
try {
    $version = git -C $ProjectRoot describe --tags --always 2>$null
    if (-not $version) { $version = "dev" }

    $env:GOOS = "windows"
    $env:GOARCH = "amd64"

    $launcherOut = Join-Path $OutputDir "MiraProt-launcher.exe"

    # Generate Windows resources (exe icon + version info) via go-winres.
    # go-winres produces rsrc_windows_amd64.syso which the Go toolchain
    # only links on GOOS=windows, avoiding linker errors on other platforms.
    Write-Host "Generating Windows resources (exe icon)..."
    if (-not (Get-Command go-winres -ErrorAction SilentlyContinue)) {
        Write-Host "Installing go-winres..."
        go install github.com/tc-hib/go-winres@latest
    }
    go run gen_ico.go
    go-winres make

    Write-Host "Compiling launcher (version: $version)..."
    go build `
        -ldflags "-s -w -X main.Version=$version" `
        -o $launcherOut .

    if ($LASTEXITCODE -ne 0) {
        Write-Error "Go build failed with exit code $LASTEXITCODE"
        exit 1
    }
} finally {
    Pop-Location
}

$launcherFile = Join-Path $OutputDir "MiraProt-launcher.exe"
$launcherSizeMB = [math]::Round((Get-Item $launcherFile).Length / 1MB, 1)
Write-Host "Launcher built: $launcherFile ($launcherSizeMB MB)"
Write-Host ""

# -----------------------------------------------------------------------
# Done
# -----------------------------------------------------------------------
Write-Host "=== Bundle complete ===" -ForegroundColor Green
Write-Host ""
Write-Host "Contents of ${OutputDir}:"
Get-ChildItem $OutputDir | Format-Table Name, Length -AutoSize
Write-Host ""
Write-Host "To run: $(Join-Path $OutputDir 'MiraProt-launcher.exe')"
