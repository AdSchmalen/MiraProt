# bundle-r-windows.ps1 — Create a portable MiraProt distribution for Windows
#
# Usage:
#   .\bundle-r-windows.ps1 [-RVersion VERSION] [-OutputDir ".\dist"]
#
# Prerequisites:
#   - PowerShell 5.1+
#   - Internet access (to download R, Go tools, and R packages)
#   - Go toolchain (for building the launcher)
#   - Git when building from a Git checkout (source archives are also supported)

[CmdletBinding()]
param(
    [string]$RVersion,
    [string]$OutputDir = (Join-Path $PSScriptRoot "..\dist")
)

$ErrorActionPreference = "Stop"

$ScriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..\..")).Path

# Fail early, before downloading or modifying a partial bundle.
if (-not (Get-Command go -ErrorAction SilentlyContinue)) {
    throw "Go was not found on PATH. Install Go 1.22 or later from https://go.dev/dl/ and reopen PowerShell."
}
$HasGitMetadata = Test-Path (Join-Path $ProjectRoot ".git")
if ($HasGitMetadata -and -not (Get-Command git -ErrorAction SilentlyContinue)) {
    throw "This is a Git checkout, but Git was not found on PATH. Install Git or build from a source archive without .git metadata."
}

try {
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    Invoke-WebRequest -Uri "https://cloud.r-project.org/" -Method Head -UseBasicParsing -TimeoutSec 15 | Out-Null
} catch {
    throw "Internet preflight failed: cannot reach CRAN at https://cloud.r-project.org/. Check DNS, proxy, firewall, and TLS settings, then retry. $($_.Exception.Message)"
}

try {
    New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
    $OutputDir = (Resolve-Path $OutputDir).Path
    $WriteProbe = Join-Path $OutputDir (".miraprot-write-test-" + [guid]::NewGuid().ToString("N"))
    [IO.File]::WriteAllText($WriteProbe, "write test")
    Remove-Item $WriteProbe -Force
} catch {
    throw "Output path '$OutputDir' cannot be created or written. Choose a writable local directory. $($_.Exception.Message)"
}

if (-not $RVersion) {
    $RVersion = (Get-Content (Join-Path $ScriptDir "..\R_VERSION") -Raw).Trim()
}
if ($RVersion -notmatch '^\d+\.\d+\.\d+$') {
    throw "Invalid R version '$RVersion' (expected MAJOR.MINOR.PATCH)."
}

Write-Host "=== MiraProt Portable Bundler (Windows) ===" -ForegroundColor Cyan
Write-Host "R version: $RVersion"
Write-Host "Output:    $OutputDir"
Write-Host ""

$RPortable = Join-Path $OutputDir "r-portable"
$RLibrary  = Join-Path $OutputDir "r-library"
$ShinyApp  = Join-Path $OutputDir "shiny-app"

New-Item -ItemType Directory -Force -Path $RLibrary   | Out-Null

# -----------------------------------------------------------------------
# Step 1: Download and install portable R for Windows
# -----------------------------------------------------------------------
$RscriptPath = Join-Path $RPortable "bin\Rscript.exe"

if (Test-Path $RscriptPath) {
    $InstalledVersion = (& $RscriptPath --vanilla -s -e "cat(as.character(getRversion()))").Trim()
    if ($InstalledVersion -ne $RVersion) {
        throw "Requested R $RVersion, but the existing portable runtime is R $InstalledVersion at $RscriptPath. Remove '$RPortable' or request R $InstalledVersion."
    }
    Write-Host "--- R already present at $RPortable ---"
} else {
    Write-Host "--- Downloading R $RVersion for Windows ---"

    $RInstaller = Join-Path $env:TEMP "R-$RVersion-win.exe"

    if (-not (Test-Path $RInstaller)) {
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        $RUrls = @(
            "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
            "https://cran.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
        )
        $Downloaded = $false
        foreach ($RUrl in $RUrls) {
            Write-Host "Trying: $RUrl"
            try {
                Invoke-WebRequest -Uri $RUrl -OutFile $RInstaller -UseBasicParsing
                $Downloaded = $true
                break
            } catch {
                Remove-Item $RInstaller -Force -ErrorAction SilentlyContinue
                Write-Host "Installer not available at this location."
            }
        }
        if (-not $Downloaded) {
            throw "R $RVersion is unavailable from both the current and archived CRAN Windows installer locations. Check the version at https://cran.r-project.org/bin/windows/base/ and retry."
        }
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

    $InstalledVersion = (& $RscriptPath --vanilla -s -e "cat(as.character(getRversion()))").Trim()
    if ($InstalledVersion -ne $RVersion) {
        throw "CRAN installer mismatch: requested R $RVersion, but the installed runtime reports R $InstalledVersion."
    }

    Write-Host "Portable R installed at: $RPortable"
}
Write-Host ""

# -----------------------------------------------------------------------
# Step 2: Install R packages
# -----------------------------------------------------------------------
Write-Host "--- Installing R packages into $RLibrary ---"
Write-Host "The installer prefers compatible Windows binary packages when repositories provide them."
Write-Host "Rtools is needed only for dependencies that must compile from source; install-packages.R may attempt to install Rtools automatically if build tools are required and missing."

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
if ($HasGitMetadata) {
    $commitCount = git -C $ProjectRoot rev-list --count HEAD
    $commitSha = git -C $ProjectRoot rev-parse --short=7 HEAD
    $commitDate = git -C $ProjectRoot log -1 --format=%cs
    if ($LASTEXITCODE -ne 0) { throw "Git metadata exists but BUILD_INFO could not be generated. Verify that the checkout and HEAD are valid." }
} else {
    Write-Host "No .git metadata found; writing archive-safe BUILD_INFO values."
    $commitCount = "unknown"
    $commitSha = "unknown"
    $commitDate = "unknown"
}
@(
    "COMMIT_COUNT=$commitCount"
    "COMMIT_SHA=$commitSha"
    "COMMIT_DATE=$commitDate"
) | Set-Content -Path (Join-Path $ShinyApp "BUILD_INFO") -Encoding ascii
Write-Host ""

# -----------------------------------------------------------------------
# Step 5: Build Go launcher
# -----------------------------------------------------------------------
Write-Host "--- Building Go launcher ---"

$LauncherDir = Join-Path $ScriptDir "..\launcher"

Push-Location $LauncherDir
try {
    $version = if ($HasGitMetadata) { git -C $ProjectRoot describe --tags --always 2>$null } else { "dev" }
    if (-not $version) { $version = "dev" }

    $env:GOOS = "windows"
    $env:GOARCH = "amd64"

    $launcherOut = Join-Path $OutputDir "MiraProt-launcher.exe"

    # Generate Windows resources (exe icon + version info) via go-winres.
    # go-winres produces rsrc_windows_amd64.syso which the Go toolchain
    # only links on GOOS=windows, avoiding linker errors on other platforms.
    Write-Host "Generating Windows resources (exe icon)..."
    $GoWinresVersion = "v0.3.3"
    $GoBin = (go env GOBIN).Trim()
    if (-not $GoBin) {
        $GoPath = ((go env GOPATH).Trim() -split [IO.Path]::PathSeparator)[0]
        $GoBin = Join-Path $GoPath "bin"
    }
    $GoWinres = Join-Path $GoBin "go-winres.exe"
    Write-Host "Installing reviewed go-winres $GoWinresVersion to $GoWinres..."
    go install "github.com/tc-hib/go-winres@$GoWinresVersion"
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path $GoWinres)) {
        throw "go-winres $GoWinresVersion installation failed or did not create '$GoWinres'."
    }
    go run gen_ico.go
    & $GoWinres make

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
