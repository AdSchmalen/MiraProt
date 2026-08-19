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

function Get-ValidatedRVersion {
    param(
        [Parameter(Mandatory = $true)]
        [string]$RscriptPath
    )

    if (-not (Test-Path -LiteralPath $RscriptPath -PathType Leaf)) {
        throw "Rscript executable was not found at '$RscriptPath'."
    }

    $output = & $RscriptPath --vanilla -s -e "cat(as.character(getRversion()))"
    $exitCode = $LASTEXITCODE
    $detectedVersion = (@($output) -join [Environment]::NewLine).Trim()

    if ($exitCode -ne 0 -or [string]::IsNullOrWhiteSpace($detectedVersion)) {
        throw "Unable to detect the R version using '$RscriptPath' (exit code $exitCode): the process failed or returned an empty version."
    }
    if ($detectedVersion -notmatch '^\d+\.\d+\.\d+$') {
        throw "Rscript at '$RscriptPath' returned invalid version '$detectedVersion' (exit code $exitCode; expected MAJOR.MINOR.PATCH)."
    }
    if ($detectedVersion -ne $RVersion) {
        throw "R version mismatch for '$RscriptPath' (exit code $exitCode): requested R $RVersion, but detected R $detectedVersion."
    }

    return $detectedVersion
}

function Test-WindowsExecutableHeader {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    # Check both the DOS MZ header and the PE signature it points to. This
    # catches HTML error pages and truncated downloads even when CRAN does not
    # publish a checksum alongside an older installer.
    try {
        $stream = [IO.File]::OpenRead($Path)
        try {
            if ($stream.Length -lt 64) { return $false }
            $reader = New-Object IO.BinaryReader($stream)
            if ($reader.ReadUInt16() -ne 0x5A4D) { return $false }
            $stream.Position = 0x3C
            $peOffset = $reader.ReadUInt32()
            if ($peOffset -gt ($stream.Length - 4)) { return $false }
            $stream.Position = $peOffset
            return ($reader.ReadUInt32() -eq 0x00004550)
        } finally {
            $stream.Dispose()
        }
    } catch {
        return $false
    }
}

function Get-CranInstallerMd5 {
    param(
        [Parameter(Mandatory = $true)]
        [string]$InstallerUrl,
        [Parameter(Mandatory = $true)]
        [string]$InstallerName
    )

    $checksumUrl = $InstallerUrl.Substring(0, $InstallerUrl.LastIndexOf('/') + 1) + "md5sum.txt"
    try {
        $response = Invoke-WebRequest -Uri $checksumUrl -UseBasicParsing -TimeoutSec 30
        $checksumText = [string]$response.Content
        foreach ($line in ($checksumText -split "`r?`n")) {
            if ($line -match ('^\s*([0-9a-fA-F]{32})\s+\*?' + [regex]::Escape($InstallerName) + '\s*$')) {
                return $Matches[1].ToUpperInvariant()
            }
        }
    } catch {
        Write-Verbose "No CRAN checksum available at $checksumUrl`: $($_.Exception.Message)"
    }
    return $null
}

function Test-RInstaller {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string[]]$SourceUrls
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $false }

    # R's Windows installer is normally tens of megabytes. Ten MiB is a
    # deliberately conservative floor that rejects empty/truncated responses.
    $minimumInstallerBytes = 10MB
    $length = (Get-Item -LiteralPath $Path).Length
    if ($length -lt $minimumInstallerBytes) {
        Write-Warning "Rejecting installer '$Path': size $length bytes is implausibly small."
        return $false
    }
    if (-not (Test-WindowsExecutableHeader -Path $Path)) {
        Write-Warning "Rejecting installer '$Path': it does not have a valid Windows PE executable header."
        return $false
    }

    # Downloads use a temporary suffix until validation succeeds, but CRAN's
    # checksum manifest contains the final installer filename.
    $installerName = (Split-Path -Leaf $Path) -replace '\.download$', ''
    $foundChecksum = $false
    $actualMd5 = $null
    foreach ($sourceUrl in $SourceUrls) {
        $expectedMd5 = Get-CranInstallerMd5 -InstallerUrl $sourceUrl -InstallerName $installerName
        if ($expectedMd5) {
            $foundChecksum = $true
            if (-not $actualMd5) {
                $actualMd5 = (Get-FileHash -LiteralPath $Path -Algorithm MD5).Hash.ToUpperInvariant()
            }
            if ($actualMd5 -eq $expectedMd5) {
                Write-Host "Verified installer against the checksum published by CRAN."
                return $true
            }
        }
    }
    if ($foundChecksum) {
        Write-Warning "Rejecting installer '$Path': its checksum does not match CRAN."
        return $false
    }

    Write-Warning "CRAN did not provide a checksum for this installer; validated its size and Windows PE header only."
    return $true
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
$UseExistingRuntime = $false

if (Test-Path -LiteralPath $RscriptPath -PathType Leaf) {
    try {
        $InstalledVersion = Get-ValidatedRVersion -RscriptPath $RscriptPath
        $UseExistingRuntime = $true
    } catch {
        Write-Warning "Existing portable R is incomplete or invalid and will be replaced: $($_.Exception.Message)"
    }
}

if ($UseExistingRuntime) {
    Write-Host "--- R already present at $RPortable ---"
} else {
    Write-Host "--- Downloading R $RVersion for Windows ---"

    $RInstaller = Join-Path $env:TEMP "R-$RVersion-win.exe"
    $RUrls = @(
        "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
        "https://cran.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
    )

    if (Test-RInstaller -Path $RInstaller -SourceUrls $RUrls) {
        Write-Host "Using validated cached installer: $RInstaller"
    } else {
        Remove-Item -LiteralPath $RInstaller -Force -ErrorAction SilentlyContinue
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        $Downloaded = $false
        foreach ($RUrl in $RUrls) {
            Write-Host "Trying: $RUrl"
            $downloadPath = "$RInstaller.download"
            try {
                Remove-Item -LiteralPath $downloadPath -Force -ErrorAction SilentlyContinue
                Invoke-WebRequest -Uri $RUrl -OutFile $downloadPath -UseBasicParsing
                if (Test-RInstaller -Path $downloadPath -SourceUrls @($RUrl)) {
                    Move-Item -LiteralPath $downloadPath -Destination $RInstaller -Force
                    $Downloaded = $true
                    break
                }
                Write-Warning "Downloaded installer failed validation; trying another CRAN location."
            } catch {
                Write-Host "Installer not available at this location."
            } finally {
                Remove-Item -LiteralPath $downloadPath -Force -ErrorAction SilentlyContinue
            }
        }
        if (-not $Downloaded) {
            throw "R $RVersion is unavailable from both the current and archived CRAN Windows installer locations. Check the version at https://cran.r-project.org/bin/windows/base/ and retry."
        }
    }

    # Install into a fresh staging directory so an incomplete prior runtime can
    # never satisfy the post-install checks.
    $RStaging = Join-Path $OutputDir (".r-portable-staging-" + [guid]::NewGuid().ToString("N"))
    $StagedRscriptPath = Join-Path $RStaging "bin\Rscript.exe"
    Write-Host "Installing R to temporary staging directory $RStaging (this may take a few minutes)..."
    $installArgs = @(
        "/VERYSILENT"
        "/DIR=$RStaging"
        "/NORESTART"
        "/SUPPRESSMSGBOXES"
        "/COMPONENTS=main,x64"
    )
    try {
        $process = Start-Process -FilePath $RInstaller -ArgumentList $installArgs -Wait -NoNewWindow -PassThru
        if ($process.ExitCode -ne 0) {
            throw "R installer '$RInstaller' failed with exit code $($process.ExitCode). The cached installer may be removed and downloaded again before retrying."
        }

        $InstalledVersion = Get-ValidatedRVersion -RscriptPath $StagedRscriptPath
        if (Test-Path -LiteralPath $RPortable) {
            Remove-Item -LiteralPath $RPortable -Recurse -Force
        }
        Move-Item -LiteralPath $RStaging -Destination $RPortable
    } finally {
        if (Test-Path -LiteralPath $RStaging) {
            Remove-Item -LiteralPath $RStaging -Recurse -Force
        }
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

# Runtime payload manifest. Keep synchronized with bundle-r.sh and CI.
foreach ($runtimeDir in @("R", "modules", "AutoAssign", "GSEA")) {
    $source = Join-Path $ProjectRoot $runtimeDir
    $destination = Join-Path $ShinyApp $runtimeDir
    New-Item -ItemType Directory -Force -Path $destination | Out-Null
    & robocopy $source $destination /E /NFL /NDL /NJH /NJS /NP *> $null
    if ($LASTEXITCODE -ge 8) { throw "Failed to copy runtime directory '$runtimeDir' (robocopy exit $LASTEXITCODE)." }
}
$DocumentationDestination = Join-Path $ShinyApp "Documentation"
New-Item -ItemType Directory -Force -Path $DocumentationDestination | Out-Null
& robocopy (Join-Path $ProjectRoot "Documentation") $DocumentationDestination "*.R" /NFL /NDL /NJH /NJS /NP *> $null
if ($LASTEXITCODE -ge 8) { throw "Failed to copy runtime Documentation sources (robocopy exit $LASTEXITCODE)." }
foreach ($runtimeFile in @("app.R", "MiraProt_icon.png")) {
    Copy-Item (Join-Path $ProjectRoot $runtimeFile) (Join-Path $ShinyApp $runtimeFile) -Force
}
$global:LASTEXITCODE = 0

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
