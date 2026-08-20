# bundle-r-windows.ps1 — Create a portable MiraProt distribution for Windows
#
# Usage:
#   .\bundle-r-windows.ps1 [-RVersion VERSION] [-OutputDir ".\dist"]
#       [-KeepFailedStaging <bool>] [-AllowUnverifiedRInstaller]
# Ordinary users should omit -RVersion: portable\R_VERSION supplies the
# maintained R runtime default. -RVersion does not select the MiraProt
# application version, which comes from Git/build metadata and R/version_info.R.
#
# KeepFailedStaging defaults to $true so a failed R installation and its logs
# remain available for diagnosis. Pass -KeepFailedStaging:$false to remove a
# failed staging tree before the script exits.
#
# Prerequisites:
#   - PowerShell 5.1+
#   - Internet access (to download R, Go tools, and R packages)
#   - Go toolchain (for building the launcher)
#   - Git when building from a Git checkout (source archives are also supported)

[CmdletBinding()]
param(
    [string]$RVersion,
    [string]$OutputDir = (Join-Path $PSScriptRoot "..\dist"),
    [switch]$KeepFailedStaging = $true,
    # Intended only for historical installers for which CRAN publishes no
    # checksum and Windows cannot establish the expected publisher identity.
    [switch]$AllowUnverifiedRInstaller,
    # Dot-source with -HelpersOnly to load the functions without performing
    # preflights, creating output directories, or starting the bundler.
    [switch]$HelpersOnly
)

$ErrorActionPreference = "Stop"

function Write-LifecycleEvent {
    param([Parameter(Mandatory = $true)][string]$Name)
    Write-Host "LIFECYCLE: $Name"
}

# R consults these variables before its own installation and site files.  A
# developer's interactive R configuration must never influence the runtime we
# are validating or the library/cache that we put in the portable bundle.
$RProcessEnvironmentVariables = @(
    "R_HOME", "R_ARCH", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE",
    "R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE", "R_PROFILE_USER"
)

function Invoke-WithCleanREnvironment {
    param(
        [Parameter(Mandatory = $true)]
        [scriptblock]$Action,
        [hashtable]$Environment = @{}
    )

    $snapshot = @{}
    $present = @()
    foreach ($name in $RProcessEnvironmentVariables) {
        if (Test-Path -LiteralPath "Env:$name") {
            $snapshot[$name] = (Get-Item -LiteralPath "Env:$name").Value
            $present += $name
        }
    }
    if ($present.Count -gt 0) {
        Write-Host "Ignoring inherited R environment variables: $($present -join ', ')"
    }

    try {
        foreach ($name in $RProcessEnvironmentVariables) {
            Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue
        }
        foreach ($name in $Environment.Keys) {
            if ($RProcessEnvironmentVariables -notcontains $name) {
                throw "Unsupported R process environment override '$name'."
            }
            Set-Item -LiteralPath "Env:$name" -Value ([string]$Environment[$name])
        }
        & $Action
    } finally {
        foreach ($name in $RProcessEnvironmentVariables) {
            Remove-Item -LiteralPath "Env:$name" -ErrorAction SilentlyContinue
            if ($snapshot.ContainsKey($name)) {
                Set-Item -LiteralPath "Env:$name" -Value $snapshot[$name]
            }
        }
    }
}

$ScriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..\..")).Path

# Fail early, before downloading or modifying a partial bundle. The isolated
# process tests opt out of host/network preflights, but still exercise the real
# runtime and installer control flow below.
$BundlerTestMode = ($HelpersOnly -or $env:MIRAPROT_BUNDLER_TEST_MODE -eq "1")
if (-not $BundlerTestMode -and -not (Get-Command go -ErrorAction SilentlyContinue)) {
    throw "Go was not found on PATH. Install Go 1.22 or later from https://go.dev/dl/ and reopen PowerShell."
}
$HasGitMetadata = Test-Path (Join-Path $ProjectRoot ".git")
if (-not $BundlerTestMode -and $HasGitMetadata -and -not (Get-Command git -ErrorAction SilentlyContinue)) {
    throw "This is a Git checkout, but Git was not found on PATH. Install Git or build from a source archive without .git metadata."
}

if (-not $BundlerTestMode) { try {
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    Invoke-WebRequest -Uri "https://cloud.r-project.org/" -Method Head -UseBasicParsing -TimeoutSec 15 | Out-Null
} catch {
    throw "Networking failure: Internet preflight failed; cannot reach CRAN at https://cloud.r-project.org/. Check DNS, proxy, firewall, and TLS settings, then retry. $($_.Exception.Message)"
} }

if (-not $HelpersOnly) { try {
    New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
    $OutputDir = (Resolve-Path $OutputDir).Path
    $WriteProbe = Join-Path $OutputDir (".miraprot-write-test-" + [guid]::NewGuid().ToString("N"))
    [IO.File]::WriteAllText($WriteProbe, "write test")
    Remove-Item $WriteProbe -Force
} catch {
    throw "Output path '$OutputDir' cannot be created or written. Choose a writable local directory. $($_.Exception.Message)"
} }

if (-not $HelpersOnly -and -not $RVersion) {
    $RVersion = (Get-Content (Join-Path $ScriptDir "..\R_VERSION") -Raw).Trim()
}
if (-not $HelpersOnly -and $RVersion -notmatch '^\d+\.\d+\.\d+$') {
    throw "Invalid R version '$RVersion' (expected MAJOR.MINOR.PATCH)."
}

function Test-RRuntimeStructure {
    param(
        [Parameter(Mandatory = $true)]
        [string]$RHome
    )

    # Check for files that identify an installed, usable R tree. In particular,
    # do not apply an installer-size heuristic to R.exe or Rscript.exe: the
    # launchers shipped by R are intentionally small.
    $requiredFiles = @(
        "bin\R.exe"
        "bin\Rscript.exe"
        "bin\x64\R.dll"
        "etc\Rconsole"
        "etc\Rprofile.site"
        "library\base\DESCRIPTION"
    )
    foreach ($relativePath in $requiredFiles) {
        $path = Join-Path $RHome $relativePath
        if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
            throw "Portable R is missing required file '$relativePath' at '$path'."
        }
    }
    $libraryPath = Join-Path $RHome "library"
    if (-not (Test-Path -LiteralPath $libraryPath -PathType Container)) {
        throw "Portable R is missing required directory 'library' at '$libraryPath'."
    }
}

function Invoke-RValidationProbe {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FilePath,
        [Parameter(Mandatory = $true)]
        [string[]]$ArgumentList,
        [Parameter(Mandatory = $true)]
        [string]$Label,
        [string]$LogDirectory,
        [string]$EventName
    )

    Write-LifecycleEvent "$EventName start"
    Write-Host "Probe: $Label"
    $captureRoot = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-r-probe-" + [guid]::NewGuid().ToString("N"))
    $stdoutPath = "$captureRoot.stdout"
    $stderrPath = "$captureRoot.stderr"
    try {
        try {
            $processResult = Invoke-WithCleanREnvironment -Action {
                # Let PowerShell pass the argument vector to the absolute staged
                # executable instead of rebuilding a Windows command line.
                & $FilePath @ArgumentList 1> $stdoutPath 2> $stderrPath
                [pscustomobject]@{
                    ExitCode = [int]$LASTEXITCODE
                }
            }
        } catch {
            throw "Process-start exception while running '$Label' with '$FilePath': $($_.Exception.Message)"
        }
        $stdout = if (Test-Path -LiteralPath $stdoutPath) { Get-Content -LiteralPath $stdoutPath -Raw } else { "" }
        $stderr = if (Test-Path -LiteralPath $stderrPath) { Get-Content -LiteralPath $stderrPath -Raw } else { "" }
        $exitCode = $processResult.ExitCode
    } finally {
        Remove-Item -LiteralPath $stdoutPath, $stderrPath -Force -ErrorAction SilentlyContinue
    }
    $unsignedExitCode = [BitConverter]::ToUInt32([BitConverter]::GetBytes($exitCode), 0)
    $hexExitCode = "0x{0:X8}" -f $unsignedExitCode
    Write-Host "  stdout: $stdout"
    Write-Host "  stderr: $stderr"
    Write-Host "  exit code: $exitCode ($hexExitCode)"
    if ($LogDirectory) {
        New-Item -ItemType Directory -Force -Path $LogDirectory | Out-Null
        $safeName = $EventName -replace '[^A-Za-z0-9_.-]', '-'
        $probeLog = Join-Path $LogDirectory "$safeName.log"
        @("Executable: $FilePath", "Arguments: $($ArgumentList -join ' ')", "R environment: inherited R_* overrides cleared", "Exit code: $exitCode ($hexExitCode)", "--- stdout ---", $stdout, "--- stderr ---", $stderr) |
            Set-Content -LiteralPath $probeLog -Encoding UTF8
    }
    Write-LifecycleEvent "$EventName exit ($exitCode)"

    return [pscustomobject]@{
        Label = $Label
        Stdout = $stdout
        Stderr = $stderr
        ExitCode = $exitCode
        HexExitCode = $hexExitCode
    }
}

function Assert-RProbeSucceeded {
    param(
        [Parameter(Mandatory = $true)]
        [psobject]$Probe
    )

    if ($Probe.HexExitCode -eq "0xC0000005") {
        throw "R process '$($Probe.Label)' terminated with access violation 0xC0000005 (signed exit code $($Probe.ExitCode))."
    }
    if ($Probe.ExitCode -ne 0) {
        throw "R process '$($Probe.Label)' failed with nonzero exit code $($Probe.ExitCode) ($($Probe.HexExitCode))."
    }
}

function Test-RRuntimeProcesses {
    param(
        [Parameter(Mandatory = $true)]
        [string]$RHome,
        [Parameter(Mandatory = $true)]
        [string]$RequestedVersion,
        [string]$LogDirectory,
        [string]$EventPrefix = "staging"
    )

    $rPath = Join-Path $RHome "bin\R.exe"
    $rscriptPath = Join-Path $RHome "bin\Rscript.exe"
    $phaseLabel = if ($EventPrefix -eq "promoted") { "Promoted-runtime revalidation" } else { "Staged native startup" }
    try {
        # These probes establish native process startup only. Their human-readable
        # output is deliberately not parsed as authoritative version evidence.
        $rVersionProbe = Invoke-RValidationProbe -FilePath $rPath -ArgumentList @("--version") -Label "R.exe --version" -LogDirectory $LogDirectory -EventName "$EventPrefix probe R.exe --version"
        Assert-RProbeSucceeded -Probe $rVersionProbe

        $rscriptVersionProbe = Invoke-RValidationProbe -FilePath $rscriptPath -ArgumentList @("--version") -Label "Rscript.exe --version" -LogDirectory $LogDirectory -EventName "$EventPrefix probe Rscript.exe --version"
        Assert-RProbeSucceeded -Probe $rscriptVersionProbe
    } catch {
        throw "$phaseLabel failure: $($_.Exception.Message)"
    }

    $comparisonLayer = if ($EventPrefix -eq "promoted") { "Promoted-runtime revalidation" } else { "Staged authoritative version comparison" }
    $probeScriptPath = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-r-version-" + [guid]::NewGuid().ToString("N") + ".R")
    try {
        # UTF-8 without a BOM is deterministic across Windows PowerShell and
        # PowerShell, and passing the file as one native argument supports spaces.
        [IO.File]::WriteAllText($probeScriptPath, "cat(as.character(getRversion()))`n", (New-Object Text.UTF8Encoding($false)))
        $expressionProbe = Invoke-RValidationProbe -FilePath $rscriptPath -ArgumentList @("--vanilla", $probeScriptPath) -Label "Rscript.exe --vanilla <version-probe.R>" -LogDirectory $LogDirectory -EventName "$EventPrefix probe R version script"
        Assert-RProbeSucceeded -Probe $expressionProbe
    } catch {
        throw "$comparisonLayer failure: authoritative getRversion() probe failed. $($_.Exception.Message)"
    } finally {
        Remove-Item -LiteralPath $probeScriptPath -Force -ErrorAction SilentlyContinue
    }
    $rawVersion = $expressionProbe.Stdout
    if ([string]::IsNullOrWhiteSpace($rawVersion)) {
        throw "$comparisonLayer failure: authoritative getRversion() expression returned an empty version."
    }
    $detectedVersion = $rawVersion.Trim()
    if ($detectedVersion -notmatch '^\d+\.\d+\.\d+$') {
        throw "$comparisonLayer failure: authoritative getRversion() expression returned malformed version '$detectedVersion' (expected MAJOR.MINOR.PATCH)."
    }
    # The exact requested-version comparison deliberately follows successful
    # expression execution and output validation.
    Write-LifecycleEvent "$EventPrefix version comparison"
    if ($detectedVersion -cne $RequestedVersion) {
        throw "$comparisonLayer failure: R version mismatch in the $EventPrefix runtime; detected R $detectedVersion, requested R $RequestedVersion."
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

function Get-HttpContentLength {
    param(
        [AllowNull()]
        [object]$HeaderValue
    )

    $normalizedLength = [int64]0
    $hasLength = $false
    foreach ($entry in @($HeaderValue)) {
        foreach ($token in ([string]$entry -split ',')) {
            $token = $token.Trim()
            if ($token.Length -eq 0) { continue }

            $parsedLength = [int64]0
            if (-not [int64]::TryParse(
                $token,
                [Globalization.NumberStyles]::Integer,
                [Globalization.CultureInfo]::InvariantCulture,
                [ref]$parsedLength
            )) { return $null }
            if ($parsedLength -lt 0) { return $null }
            if ($hasLength -and $parsedLength -ne $normalizedLength) { return $null }

            $normalizedLength = $parsedLength
            $hasLength = $true
        }
    }

    if (-not $hasLength) { return $null }
    return $normalizedLength
}

function Test-RInstaller {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string[]]$SourceUrls,
        [Parameter(Mandatory = $true)]
        [string]$RequestedVersion,
        [psobject]$DownloadRecord
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $false }

    # Tiny locally-built executables are accepted only by the isolated test
    # fixture.  Keeping this opt-in separate from BundlerTestMode ensures that
    # tests can still exercise (and reject with) the production identity path.
    $fixtureIdentity = ($BundlerTestMode -and $env:MIRAPROT_TEST_INSTALLER_FIXTURE -eq "1")

    # R's Windows installer is normally tens of megabytes. Ten MiB is a
    # deliberately conservative floor that rejects empty/truncated responses.
    $minimumInstallerBytes = 10MB
    $length = (Get-Item -LiteralPath $Path).Length
    if (-not $fixtureIdentity -and $length -lt $minimumInstallerBytes) {
        Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; downloaded file size $length bytes is implausibly small."
        return $false
    }
    if (-not (Test-WindowsExecutableHeader -Path $Path)) {
        Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; it does not have a valid Windows PE executable header."
        return $false
    }

    if ($fixtureIdentity) {
        Write-Host "Accepting explicitly scoped local installer fixture."
        return $true
    }

    # A record is evidence that this exact file was just downloaded (or was
    # previously downloaded and retained with its response metadata).  Do not
    # confer the same provenance on an executable found in the cache alone.
    $provenanceVerified = $false
    if ($null -ne $DownloadRecord) {
        Write-Host "Installer source:       $($DownloadRecord.SourceUrl)"
        Write-Host "HTTP success:           $($DownloadRecord.HttpSuccess) (status $($DownloadRecord.StatusCode))"
        Write-Host "Final response URL:     $($DownloadRecord.FinalUrl)"
        $responseContentLength = Get-HttpContentLength -HeaderValue $DownloadRecord.ResponseContentLength
        Write-Host "HTTP content length:    $($DownloadRecord.ResponseContentLength)"
        Write-Host "Downloaded file size:   $length"
        if (-not $DownloadRecord.HttpSuccess -or
            $DownloadRecord.StatusCode -lt 200 -or $DownloadRecord.StatusCode -ge 300) {
            Write-Warning "Download provenance failure: rejecting installer '$Path'; its download did not record HTTP success."
            return $false
        }
        $expectedInstallerName = "R-$RequestedVersion-win.exe"
        try { $finalUri = [Uri]$DownloadRecord.FinalUrl } catch {
            Write-Warning "Download provenance failure: rejecting installer '$Path'; recorded final URL '$($DownloadRecord.FinalUrl)' is invalid."
            return $false
        }
        if ($finalUri.Scheme -cne "https") {
            Write-Warning "Download provenance failure: rejecting installer '$Path'; final URL '$finalUri' does not use HTTPS."
            return $false
        }
        if ($finalUri.Host -notmatch '(?i)(^|\.)r-project\.org$') {
            Write-Warning "Download provenance failure: rejecting installer '$Path'; final URL host '$($finalUri.Host)' is not an r-project.org host."
            return $false
        }
        if ([Uri]::UnescapeDataString((Split-Path -Leaf $finalUri.AbsolutePath)) -cne $expectedInstallerName) {
            Write-Warning "Download provenance failure: rejecting installer '$Path'; final URL does not identify expected installer '$expectedInstallerName'."
            return $false
        }
        if ($null -eq $responseContentLength) {
            Write-Warning "Download provenance warning: HTTP Content-Length evidence is unavailable; installer identity validation will continue using the downloaded file size and other evidence."
        } elseif ($responseContentLength -ne $length) {
            Write-Warning "Download provenance failure: HTTP Content-Length $responseContentLength bytes does not match actual downloaded size $length bytes; rejecting installer '$Path'."
            return $false
        }
        $provenanceVerified = $true
    } else {
        Write-Host "Installer source: cached file (no recorded HTTP response)"
    }

    $versionInfo = (Get-Item -LiteralPath $Path).VersionInfo
    $metadataText = @($versionInfo.ProductName, $versionInfo.FileDescription, $versionInfo.ProductVersion, $versionInfo.FileVersion) -join " | "
    Write-Host "Installer metadata:     $metadataText"
    $metadataAvailable = -not [string]::IsNullOrWhiteSpace(($metadataText -replace '[|\s]', ''))
    if (-not $metadataAvailable) {
        Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; embedded file/product metadata is missing and cannot identify requested R $RequestedVersion."
        return $false
    }
    if ($metadataText -notmatch ('(?<!\d)' + [regex]::Escape($RequestedVersion) + '(?!\d)')) {
        Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; file/product metadata does not identify requested R $RequestedVersion."
        return $false
    }

    $signature = $null
    $signatureUnavailable = $false
    try {
        $signature = Get-AuthenticodeSignature -LiteralPath $Path -ErrorAction Stop
    } catch {
        $signatureUnavailable = $true
        Write-Warning "Authenticode evaluation is unavailable on this platform: $($_.Exception.Message)"
    }
    $signerSubject = if ($signature -and $signature.SignerCertificate) { $signature.SignerCertificate.Subject } else { "<none>" }
    $expectedSigner = ($signerSubject -match '(?i)(R Core Team|R Foundation)')
    $trustedSignature = ($signature -and $signature.Status -eq [System.Management.Automation.SignatureStatus]::Valid -and $expectedSigner)
    $signatureStatus = if ($signatureUnavailable) { "Unavailable" } elseif ($signature) { [string]$signature.Status } else { "Unavailable" }
    Write-Host "Authenticode status:    $signatureStatus"
    Write-Host "Authenticode signer:    $signerSubject"

    # Downloads use a temporary suffix until validation succeeds, but CRAN's
    # checksum manifest contains the final installer filename.
    $installerName = (Split-Path -Leaf $Path) -replace '\.download$', ''
    $foundChecksum = $false
    $checksumVerified = $false
    $actualMd5 = $null
    foreach ($sourceUrl in $SourceUrls) {
        $expectedMd5 = Get-CranInstallerMd5 -InstallerUrl $sourceUrl -InstallerName $installerName
        if ($expectedMd5) {
            $foundChecksum = $true
            if (-not $actualMd5) {
                $actualMd5 = (Get-FileHash -LiteralPath $Path -Algorithm MD5).Hash.ToUpperInvariant()
            }
            if ($actualMd5 -eq $expectedMd5) {
                $checksumVerified = $true
                Write-Host "Verified installer against the checksum published by CRAN."
                return $true
            }
        }
    }
    if ($foundChecksum -and -not $checksumVerified) {
        Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; its checksum does not match CRAN."
        return $false
    }
    if ($trustedSignature -and $metadataAvailable) {
        if (-not $foundChecksum) { Write-Warning "CRAN did not publish a checksum; accepting the valid expected signature and matching R metadata." }
        return $true
    }
    $reason = "Authenticode status '$signatureStatus', expected signer=$expectedSigner, version metadata available=$metadataAvailable"
    if ($provenanceVerified) {
        Write-Warning "CRAN did not publish a checksum and Authenticode could not establish trust; accepting the provenance-verified download with matching installer identity ($reason)."
        return $true
    }
    if ($AllowUnverifiedRInstaller) {
        Write-Warning "Explicit opt-in accepted installer whose identity could not otherwise be established ($reason)."
        return $true
    }
    Write-Warning "Installer identity/integrity failure: rejecting installer '$Path'; insufficient installer identity ($reason). Use -AllowUnverifiedRInstaller only after independently confirming this installer."
    return $false
}

if ($HelpersOnly) { return }

Write-Host "=== MiraProt Portable Bundler (Windows) ===" -ForegroundColor Cyan
Write-Host "R version: $RVersion"
Write-Host "Output:    $OutputDir"
Write-Host ""

$RPortable = Join-Path $OutputDir "r-portable"
$RLibrary  = Join-Path $OutputDir "r-library"
$ShinyApp  = Join-Path $OutputDir "shiny-app"

New-Item -ItemType Directory -Force -Path $RLibrary   | Out-Null

foreach ($documentationFile in @("LICENSE.md", "README.md", "THIRD_PARTY_NOTICES.md", "citation.cff")) {
    $documentationSource = Join-Path $ProjectRoot $documentationFile
    if (Test-Path -LiteralPath $documentationSource -PathType Leaf) {
        Write-Host "Including portable documentation: $documentationFile"
        Copy-Item -LiteralPath $documentationSource -Destination (Join-Path $OutputDir $documentationFile) -Force
    } else {
        Write-Warning "Portable documentation file not found; skipping: $documentationFile"
    }
}

# -----------------------------------------------------------------------
# Step 1: Download and install portable R for Windows
# -----------------------------------------------------------------------
$RscriptPath = Join-Path $RPortable "bin\Rscript.exe"
$UseExistingRuntime = $false

if (Test-Path -LiteralPath $RPortable -PathType Container) {
    try {
        Test-RRuntimeStructure -RHome $RPortable
        $InstalledVersion = Test-RRuntimeProcesses -RHome $RPortable -RequestedVersion $RVersion
        $UseExistingRuntime = $true
    } catch {
        Write-Warning "Existing portable R is incomplete or invalid and will be replaced: $($_.Exception.Message)"
    }
}

if ($UseExistingRuntime) {
    Write-Host "--- R already present at $RPortable ---"
} else {
    Write-Host "--- Downloading R $RVersion for Windows ---"

    $RInstaller = if ($BundlerTestMode -and $env:MIRAPROT_TEST_INSTALLER_PATH) {
        $env:MIRAPROT_TEST_INSTALLER_PATH
    } else {
        Join-Path $env:TEMP "R-$RVersion-win.exe"
    }
    $RUrls = @(
        "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
        "https://cran.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
    )
    $InstallerRecordPath = "$RInstaller.source.json"
    $CachedDownloadRecord = $null
    if (Test-Path -LiteralPath $InstallerRecordPath -PathType Leaf) {
        try { $CachedDownloadRecord = Get-Content -LiteralPath $InstallerRecordPath -Raw | ConvertFrom-Json }
        catch { Write-Warning "Ignoring unreadable installer source record '$InstallerRecordPath'." }
    }

    if (Test-RInstaller -Path $RInstaller -SourceUrls $RUrls -RequestedVersion $RVersion -DownloadRecord $CachedDownloadRecord) {
        Write-Host "Using validated cached installer: $RInstaller"
    } elseif ($BundlerTestMode) {
        throw "Cached R installer '$RInstaller' is missing or invalid; refusing to continue with an unvalidated installer."
    } else {
        Remove-Item -LiteralPath $RInstaller -Force -ErrorAction SilentlyContinue
        Remove-Item -LiteralPath $InstallerRecordPath -Force -ErrorAction SilentlyContinue
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        $Downloaded = $false
        $DownloadFailures = @()
        foreach ($RUrl in $RUrls) {
            Write-Host "Trying: $RUrl"
            $downloadPath = "$RInstaller.download"
            try {
                Remove-Item -LiteralPath $downloadPath -Force -ErrorAction SilentlyContinue
                $response = Invoke-WebRequest -Uri $RUrl -OutFile $downloadPath -UseBasicParsing -PassThru
                $statusCode = [int]$response.StatusCode
                $finalUrl = if ($response.BaseResponse.ResponseUri) { [string]$response.BaseResponse.ResponseUri.AbsoluteUri } else { [string]$RUrl }
                Write-Host "Effective URL after successful request: $finalUrl"
                $responseContentLength = Get-HttpContentLength -HeaderValue $response.Headers['Content-Length']
                $downloadedLength = (Get-Item -LiteralPath $downloadPath).Length
                $downloadRecord = [pscustomobject]@{
                    HttpSuccess = ($statusCode -ge 200 -and $statusCode -lt 300)
                    StatusCode = $statusCode
                    FinalUrl = $finalUrl
                    ContentLength = $downloadedLength
                    ResponseContentLength = $responseContentLength
                    SourceUrl = $RUrl
                    RecordedAtUtc = [DateTime]::UtcNow.ToString('o')
                }
                if (Test-RInstaller -Path $downloadPath -SourceUrls @($RUrl) -RequestedVersion $RVersion -DownloadRecord $downloadRecord) {
                    Move-Item -LiteralPath $downloadPath -Destination $RInstaller -Force
                    $downloadRecord | ConvertTo-Json | Set-Content -LiteralPath $InstallerRecordPath -Encoding UTF8
                    $Downloaded = $true
                    break
                }
                $reason = "Installer identity/integrity failure: downloaded installer from '$RUrl' failed local identity validation."
                $DownloadFailures += $reason
                Write-Warning "$reason Trying another CRAN location."
            } catch {
                $reason = "Networking/download failure: installer attempt '$RUrl' failed: $($_.Exception.Message)"
                $DownloadFailures += $reason
                Write-Warning $reason
            } finally {
                Remove-Item -LiteralPath $downloadPath -Force -ErrorAction SilentlyContinue
            }
        }
        if (-not $Downloaded) {
            throw "Download provenance failure: no validated R $RVersion installer was acquired. Attempts: $($DownloadFailures -join ' | ')"
        }
    }

    Write-Host "Installer validation passed: provenance and identity/integrity checks succeeded."

    # Keep this path short: deeply nested output directories can otherwise make
    # the R installer exceed legacy Windows path limits.
    $stagingParent = if ($BundlerTestMode -and $env:MIRAPROT_TEST_STAGING_ROOT) { $env:MIRAPROT_TEST_STAGING_ROOT } else { $env:TEMP }
    New-Item -ItemType Directory -Force -Path $stagingParent | Out-Null
    $RStaging = Join-Path $stagingParent ("MiraProt-R-$RVersion-" + [guid]::NewGuid().ToString("N"))
    $StagingLogs = "$RStaging-logs"
    $InstallerLog = Join-Path $StagingLogs "installer.log"
    $StagedRscriptPath = Join-Path $RStaging "bin\Rscript.exe"
    Write-LifecycleEvent "staging creation start"
    New-Item -ItemType Directory -Path $RStaging | Out-Null
    New-Item -ItemType Directory -Path $StagingLogs | Out-Null
    Write-LifecycleEvent "staging creation completion"
    Write-Host "Installing R to temporary staging directory $RStaging (this may take a few minutes)..."
    $installArgs = @(
        "/VERYSILENT"
        "/DIR=$RStaging"
        "/NORESTART"
        "/SUPPRESSMSGBOXES"
        "/COMPONENTS=main,x64"
        "/LOG=$InstallerLog"
    )
    $PromotionStarted = $false
    $PromotionCompleted = $false
    $RBackup = $null
    try {
        Write-LifecycleEvent "installer start"
        if ($BundlerTestMode -and $env:MIRAPROT_TEST_INSTALLER_COMMAND) {
            & $env:MIRAPROT_TEST_INSTALLER_COMMAND $RStaging *> $InstallerLog
            $installerExitCode = $LASTEXITCODE
        } else {
            $process = Start-Process -FilePath $RInstaller -ArgumentList $installArgs -Wait -NoNewWindow -PassThru
            $installerExitCode = $process.ExitCode
        }
        Write-LifecycleEvent "installer exit ($installerExitCode)"
        if ($installerExitCode -ne 0) {
            throw "Installer execution failure: R installer '$RInstaller' failed with exit code $installerExitCode. Installer log: '$InstallerLog'. The cached installer may be removed and downloaded again before retrying."
        }

        Write-LifecycleEvent "staging static checks start"
        try { Test-RRuntimeStructure -RHome $RStaging }
        catch { throw "Staged native startup failure: $($_.Exception.Message)" }
        Write-LifecycleEvent "staging static checks completion"
        $InstalledVersion = Test-RRuntimeProcesses -RHome $RStaging -RequestedVersion $RVersion -LogDirectory $StagingLogs -EventPrefix "staging"

        Write-LifecycleEvent "promotion start"
        $PromotionStarted = $true
        if (Test-Path -LiteralPath $RPortable) {
            $RBackup = Join-Path $OutputDir (".r-portable-backup-" + [guid]::NewGuid().ToString("N"))
            Move-Item -LiteralPath $RPortable -Destination $RBackup
        }
        try {
            Move-Item -LiteralPath $RStaging -Destination $RPortable
            $PromotionCompleted = $true
            Write-LifecycleEvent "promotion completion"

            Write-LifecycleEvent "promoted-runtime validation start"
            Write-LifecycleEvent "promoted static checks start"
            try { Test-RRuntimeStructure -RHome $RPortable }
            catch { throw "Promoted-runtime revalidation failure: $($_.Exception.Message)" }
            Write-LifecycleEvent "promoted static checks completion"
            $InstalledVersion = Test-RRuntimeProcesses -RHome $RPortable -RequestedVersion $RVersion -LogDirectory $StagingLogs -EventPrefix "promoted"
            Write-LifecycleEvent "promoted-runtime validation completion"
        } catch {
            # The old runtime remains recoverable until the promoted tree has
            # passed exactly the same checks at its final path.
            if (Test-Path -LiteralPath $RPortable) {
                Move-Item -LiteralPath $RPortable -Destination $RStaging
            }
            if ($RBackup -and (Test-Path -LiteralPath $RBackup)) {
                Move-Item -LiteralPath $RBackup -Destination $RPortable
            }
            throw
        }

        Write-LifecycleEvent "cleanup start"
        if ($RBackup -and (Test-Path -LiteralPath $RBackup)) {
            Remove-Item -LiteralPath $RBackup -Recurse -Force
        }
        if (Test-Path -LiteralPath $RStaging) {
            Remove-Item -LiteralPath $RStaging -Recurse -Force
        }
        if (Test-Path -LiteralPath $StagingLogs) {
            Remove-Item -LiteralPath $StagingLogs -Recurse -Force
        }
        Write-LifecycleEvent "cleanup completion"
    } catch {
        $failure = $_
        # If promotion itself failed before its inner rollback ran, restore the
        # uniquely named backup here as well.
        if ($PromotionStarted -and -not $PromotionCompleted -and $RBackup -and (Test-Path -LiteralPath $RBackup)) {
            if (Test-Path -LiteralPath $RPortable) { Remove-Item -LiteralPath $RPortable -Recurse -Force }
            Move-Item -LiteralPath $RBackup -Destination $RPortable
        }
        if (-not $KeepFailedStaging) {
            Write-LifecycleEvent "cleanup start"
            if (Test-Path -LiteralPath $RStaging) { Remove-Item -LiteralPath $RStaging -Recurse -Force }
            if (Test-Path -LiteralPath $StagingLogs) { Remove-Item -LiteralPath $StagingLogs -Recurse -Force }
            Write-LifecycleEvent "cleanup completion"
        } else {
            Write-Host "Failed staging retained at: $RStaging" -ForegroundColor Yellow
            Write-Host "Failure logs retained at: $StagingLogs" -ForegroundColor Yellow
        }
        Write-Error "Portable R installation failed before package installation: $($failure.Exception.Message) Installer log: '$InstallerLog'."
        exit 1
    }

    Write-Host "Portable R installed at: $RPortable"
}
Write-Host ""

if ($BundlerTestMode) {
    Write-Host "Portable R validation completed; stopping before package installation."
    exit 0
}

# -----------------------------------------------------------------------
# Step 2: Install R packages
# -----------------------------------------------------------------------
Write-Host "--- Installing R packages into $RLibrary ---"
Write-Host "The installer prefers compatible Windows binary packages when repositories provide them."
Write-Host "Rtools is needed only for dependencies that genuinely must compile from source; the installer will identify the compatible generation if it is missing."

$InstallScript = Join-Path $ScriptDir "install-packages.R"
Invoke-WithCleanREnvironment -Environment @{ R_LIBS_USER = $RLibrary } -Action {
    & $RscriptPath --vanilla $InstallScript $RLibrary
}

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
    Invoke-WithCleanREnvironment -Environment @{ R_LIBS_USER = $RLibrary } -Action {
        & $RscriptPath --vanilla (Join-Path $ScriptDir "prebuild-cache.R") $GoCache $RLibrary
    }

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
    if ($runtimeDir -eq "GSEA") {
        & robocopy $source $destination /E /XF "*.gmt" /NFL /NDL /NJH /NJS /NP *> $null
    } else {
        & robocopy $source $destination /E /NFL /NDL /NJH /NJS /NP *> $null
    }
    if ($LASTEXITCODE -ge 8) { throw "Failed to copy runtime directory '$runtimeDir' (robocopy exit $LASTEXITCODE)." }
}
$GseaReadme = Join-Path $ProjectRoot "GSEA\README.md"
if (-not (Test-Path -LiteralPath $GseaReadme -PathType Leaf)) {
    Write-Warning "GSEA/README.md not found; portable build will continue without it."
}
$DocumentationDestination = Join-Path $ShinyApp "Documentation"
New-Item -ItemType Directory -Force -Path $DocumentationDestination | Out-Null
& robocopy (Join-Path $ProjectRoot "Documentation") $DocumentationDestination "*.R" /NFL /NDL /NJH /NJS /NP *> $null
if ($LASTEXITCODE -ge 8) { throw "Failed to copy runtime Documentation sources (robocopy exit $LASTEXITCODE)." }
foreach ($runtimeFile in @("app.R", "MiraProt_icon.png")) {
    Copy-Item (Join-Path $ProjectRoot $runtimeFile) (Join-Path $ShinyApp $runtimeFile) -Force
}

# Source-development renv state is outside the runtime allowlist. Remove it
# defensively if that manifest is expanded in the future.
foreach ($renvPath in @("renv", ".Rprofile", "renv.lock")) {
    Remove-Item -LiteralPath (Join-Path $ShinyApp $renvPath) -Recurse -Force -ErrorAction SilentlyContinue
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
