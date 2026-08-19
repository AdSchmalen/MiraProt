# bundle-r-windows.ps1 — Create a portable MiraProt distribution for Windows
#
# Usage:
#   .\bundle-r-windows.ps1 [-RVersion VERSION] [-OutputDir ".\dist"]
#       [-KeepFailedStaging <bool>] [-AllowUnverifiedRInstaller]
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
    [switch]$AllowUnverifiedRInstaller
)

$ErrorActionPreference = "Stop"

function Write-LifecycleEvent {
    param([Parameter(Mandatory = $true)][string]$Name)
    Write-Host "LIFECYCLE: $Name"
}

$ScriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..\..")).Path

# Fail early, before downloading or modifying a partial bundle. The isolated
# process tests opt out of host/network preflights, but still exercise the real
# runtime and installer control flow below.
$BundlerTestMode = ($env:MIRAPROT_BUNDLER_TEST_MODE -eq "1")
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
    throw "Internet preflight failed: cannot reach CRAN at https://cloud.r-project.org/. Check DNS, proxy, firewall, and TLS settings, then retry. $($_.Exception.Message)"
} }

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
        "VERSION"
        "library\base\DESCRIPTION"
    )
    foreach ($relativePath in $requiredFiles) {
        $path = Join-Path $RHome $relativePath
        if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
            throw "Portable R is missing required file '$relativePath' at '$path'."
        }
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
    $process = New-Object System.Diagnostics.Process
    $process.StartInfo.FileName = $FilePath
    $process.StartInfo.UseShellExecute = $false
    $process.StartInfo.CreateNoWindow = $true
    $process.StartInfo.RedirectStandardOutput = $true
    $process.StartInfo.RedirectStandardError = $true
    # All probe arguments are fixed tokens without whitespace. Arguments is
    # used instead of ProcessStartInfo.ArgumentList for Windows PowerShell 5.1.
    $process.StartInfo.Arguments = ($ArgumentList -join " ")
    try {
        try {
            if (-not $process.Start()) { throw "Process.Start returned false." }
        } catch {
            throw "Process-start exception while running '$Label' with '$FilePath': $($_.Exception.Message)"
        }
        $stdoutTask = $process.StandardOutput.ReadToEndAsync()
        $stderrTask = $process.StandardError.ReadToEndAsync()
        $process.WaitForExit()
        $stdout = $stdoutTask.GetAwaiter().GetResult()
        $stderr = $stderrTask.GetAwaiter().GetResult()
        $exitCode = [int]$process.ExitCode
    } finally {
        $process.Dispose()
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
        @("Command: $FilePath $($ArgumentList -join ' ')", "Exit code: $exitCode ($hexExitCode)", "--- stdout ---", $stdout, "--- stderr ---", $stderr) |
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
    $rVersionProbe = Invoke-RValidationProbe -FilePath $rPath -ArgumentList @("--version") -Label "R.exe --version" -LogDirectory $LogDirectory -EventName "$EventPrefix probe R.exe --version"
    Assert-RProbeSucceeded -Probe $rVersionProbe
    if ([string]::IsNullOrWhiteSpace($rVersionProbe.Stdout)) {
        throw "R process 'R.exe --version' returned empty output."
    }

    $rscriptVersionProbe = Invoke-RValidationProbe -FilePath $rscriptPath -ArgumentList @("--version") -Label "Rscript.exe --version" -LogDirectory $LogDirectory -EventName "$EventPrefix probe Rscript.exe --version"
    Assert-RProbeSucceeded -Probe $rscriptVersionProbe
    if ([string]::IsNullOrWhiteSpace($rscriptVersionProbe.Stdout)) {
        throw "R process 'Rscript.exe --version' returned empty output."
    }

    $expressionProbe = Invoke-RValidationProbe -FilePath $rscriptPath -ArgumentList @("--vanilla", "-s", "-e", "cat(as.character(getRversion()))") -Label 'Rscript.exe --vanilla -s -e "cat(as.character(getRversion()))"' -LogDirectory $LogDirectory -EventName "$EventPrefix probe R version expression"
    Assert-RProbeSucceeded -Probe $expressionProbe
    $detectedVersion = $expressionProbe.Stdout.Trim()
    if ([string]::IsNullOrWhiteSpace($detectedVersion)) {
        throw "R version expression returned an empty version."
    }
    if ($detectedVersion -notmatch '^\d+\.\d+\.\d+$') {
        throw "R version expression returned malformed version '$detectedVersion' (expected MAJOR.MINOR.PATCH)."
    }
    # The exact requested-version comparison deliberately follows successful
    # expression execution and output validation.
    Write-LifecycleEvent "$EventPrefix version comparison"
    if ($detectedVersion -cne $RequestedVersion) {
        throw "R version mismatch: requested R $RequestedVersion, but detected R $detectedVersion."
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
        [string[]]$SourceUrls,
        [Parameter(Mandatory = $true)]
        [string]$RequestedVersion,
        [psobject]$DownloadRecord
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

    if ($BundlerTestMode) {
        return $true
    }

    if ($DownloadRecord) {
        Write-Host "Installer source:       $($DownloadRecord.SourceUrl)"
        Write-Host "HTTP success:           $($DownloadRecord.HttpSuccess) (status $($DownloadRecord.StatusCode))"
        Write-Host "Final response URL:     $($DownloadRecord.FinalUrl)"
        Write-Host "HTTP content length:    $($DownloadRecord.ContentLength)"
        if (-not $DownloadRecord.HttpSuccess) {
            Write-Warning "Rejecting installer '$Path': its download did not record HTTP success."
            return $false
        }
        if ($DownloadRecord.ResponseContentLength -and ([int64]$DownloadRecord.ResponseContentLength -ne $length)) {
            Write-Warning "Rejecting installer '$Path': downloaded length $length does not match HTTP content length $($DownloadRecord.ResponseContentLength)."
            return $false
        }
    } else {
        Write-Host "Installer source: cached file (no recorded HTTP response)"
    }

    $versionInfo = (Get-Item -LiteralPath $Path).VersionInfo
    $metadataText = @($versionInfo.ProductName, $versionInfo.FileDescription, $versionInfo.ProductVersion, $versionInfo.FileVersion) -join " | "
    Write-Host "Installer metadata:     $metadataText"
    $metadataAvailable = -not [string]::IsNullOrWhiteSpace(($metadataText -replace '[|\s]', ''))
    if ($metadataAvailable -and $metadataText -notmatch ('(?<!\d)' + [regex]::Escape($RequestedVersion) + '(?!\d)')) {
        Write-Warning "Rejecting installer '$Path': file/product metadata does not identify requested R $RequestedVersion."
        return $false
    }

    $signature = Get-AuthenticodeSignature -LiteralPath $Path
    $signerSubject = if ($signature.SignerCertificate) { $signature.SignerCertificate.Subject } else { "<none>" }
    $expectedSigner = ($signerSubject -match '(?i)(R Core Team|R Foundation)')
    $trustedSignature = ($signature.Status -eq [System.Management.Automation.SignatureStatus]::Valid -and $expectedSigner)
    Write-Host "Authenticode status:    $($signature.Status)"
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
        Write-Warning "Rejecting installer '$Path': its checksum does not match CRAN."
        return $false
    }
    if ($trustedSignature -and $metadataAvailable) {
        if (-not $foundChecksum) { Write-Warning "CRAN did not publish a checksum; accepting the valid expected signature and matching R metadata." }
        return $true
    }
    $reason = "Authenticode status '$($signature.Status)', expected signer=$expectedSigner, version metadata available=$metadataAvailable"
    if ($AllowUnverifiedRInstaller) {
        Write-Warning "Explicit opt-in accepted installer whose identity could not otherwise be established ($reason)."
        return $true
    }
    Write-Warning "Rejecting installer '$Path': insufficient installer identity ($reason). Use -AllowUnverifiedRInstaller only after independently confirming this installer."
    return $false
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
        foreach ($RUrl in $RUrls) {
            Write-Host "Trying: $RUrl"
            $downloadPath = "$RInstaller.download"
            try {
                Remove-Item -LiteralPath $downloadPath -Force -ErrorAction SilentlyContinue
                $response = Invoke-WebRequest -Uri $RUrl -OutFile $downloadPath -UseBasicParsing -PassThru
                $statusCode = [int]$response.StatusCode
                $finalUrl = if ($response.BaseResponse.ResponseUri) { [string]$response.BaseResponse.ResponseUri.AbsoluteUri } else { [string]$RUrl }
                $responseContentLength = $null
                if ($response.Headers['Content-Length']) { $responseContentLength = [int64]$response.Headers['Content-Length'] }
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

    # Keep this path short: deeply nested output directories can otherwise make
    # the R installer exceed legacy Windows path limits.
    $RStaging = Join-Path $env:TEMP ("MiraProt-R-$RVersion-" + [guid]::NewGuid().ToString("N"))
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
            throw "R installer '$RInstaller' failed with exit code $installerExitCode. Installer log: '$InstallerLog'. The cached installer may be removed and downloaded again before retrying."
        }

        Write-LifecycleEvent "staging static checks start"
        Test-RRuntimeStructure -RHome $RStaging
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
            Test-RRuntimeStructure -RHome $RPortable
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
