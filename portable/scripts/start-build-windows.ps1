# Stage-0 preflight and logging wrapper. Stage 1 remains bundle-r-windows.ps1.
[CmdletBinding()]
param(
    [switch]$Interactive,
    [string]$OutputDir,
    [string]$RVersion
)

$ErrorActionPreference = "Stop"
$script:ExitCode = 1
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..\..")).Path
if (-not $OutputDir) { $OutputDir = Join-Path (Split-Path -Parent $ProjectRoot) "MiraProt_Portable" }
if (-not [IO.Path]::IsPathRooted($OutputDir)) { $OutputDir = Join-Path $ProjectRoot $OutputDir }
if (-not $RVersion) { $RVersion = (Get-Content -LiteralPath (Join-Path $ProjectRoot "portable\R_VERSION") -Raw).Trim() }
$LogDir = Join-Path $ProjectRoot "portable\logs"
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null
$LogFile = Join-Path $LogDir ("build-{0}-windows.log" -f (Get-Date -Format "yyyy-MM-dd-HHmmss"))
[IO.File]::WriteAllText($LogFile, "", (New-Object Text.UTF8Encoding($false)))

function Write-Log([string]$Message) {
    $line = "[{0}] {1}" -f (Get-Date -Format "yyyy-MM-dd HH:mm:ss"), $Message
    Write-Host $line
    Add-Content -LiteralPath $LogFile -Value $line -Encoding UTF8
}
function Pass([string]$Message) { Write-Log "PREFLIGHT PASS: $Message" }
function Assert-Preflight([bool]$Condition, [string]$Message) { if (-not $Condition) { throw $Message } }
function Quote-Argument([string]$Value) { return "'" + $Value.Replace("'", "''") + "'" }
function Get-NativeOutput([string]$FilePath, [string[]]$ArgumentList) {
    $output = & $FilePath @ArgumentList 2>&1 | ForEach-Object { $_.ToString(); Add-Content -LiteralPath $LogFile -Value $_.ToString() -Encoding UTF8 }
    return [pscustomobject]@{ Output = @($output); ExitCode = $LASTEXITCODE }
}

try {
    Write-Log "MiraProt Stage-0 build wrapper started"
    Write-Log "Platform: Windows $env:PROCESSOR_ARCHITECTURE; repository: $ProjectRoot"
    Assert-Preflight ([Environment]::Is64BitOperatingSystem -and [Environment]::Is64BitProcess -and $env:PROCESSOR_ARCHITECTURE -eq "AMD64") "Windows x86-64 and a 64-bit PowerShell process are required."
    Pass "Windows x86-64 host and process validated"
    Assert-Preflight ($PSVersionTable.PSVersion -ge [version]"5.1") "PowerShell 5.1 or later is required."
    Pass "PowerShell $($PSVersionTable.PSVersion) is supported"
    Assert-Preflight ($RVersion -match '^\d+\.\d+\.\d+$') "portable/R_VERSION or -RVersion must be MAJOR.MINOR.PATCH (got '$RVersion')."

    $VersionPath = Join-Path $ProjectRoot "VERSION"
    Assert-Preflight (Test-Path -LiteralPath $VersionPath -PathType Leaf) "Missing VERSION."
    $versionLines = @(Get-Content -LiteralPath $VersionPath)
    Assert-Preflight ($versionLines.Count -eq 1 -and $versionLines[0] -match '^\d+\.\d+\.\d+$') "VERSION must contain exactly one MAJOR.MINOR.PATCH value."
    Pass "VERSION is $($versionLines[0])"

    $LockPath = Join-Path $ProjectRoot "renv.lock"
    Assert-Preflight (Test-Path -LiteralPath $LockPath -PathType Leaf) "Missing renv.lock."
    try { $lock = Get-Content -LiteralPath $LockPath -Raw | ConvertFrom-Json } catch { throw "renv.lock is not valid JSON: $($_.Exception.Message)" }
    Assert-Preflight ($null -ne $lock.R -and [string]$lock.R.Version -eq $RVersion) "renv.lock top-level R version '$($lock.R.Version)' does not match requested R '$RVersion'."
    Pass "renv.lock top-level R version is $RVersion"

    $go = Get-Command go -ErrorAction SilentlyContinue
    Assert-Preflight ($null -ne $go) "Go 1.22 or later was not found on PATH."
    $goText = (& go version 2>&1 | Out-String).Trim()
    Assert-Preflight ($LASTEXITCODE -eq 0 -and $goText -match ' go(\d+\.\d+(?:\.\d+)?)') "Unable to determine the Go version."
    Assert-Preflight ([version]$Matches[1] -ge [version]"1.22") "Go 1.22 or later is required (detected $($Matches[1]))."
    Pass "Go $($Matches[1]) is available"
    if (Test-Path -LiteralPath (Join-Path $ProjectRoot ".git") -PathType Container) {
        Assert-Preflight ($null -ne (Get-Command git -ErrorAction SilentlyContinue)) "Git is required for this Git checkout."
        Pass "Git is available for checkout metadata"
    } else { Write-Log "PREFLIGHT SKIP: Git is not required for a source archive" }

    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    try { Invoke-WebRequest -Uri "https://cloud.r-project.org/" -Method Head -UseBasicParsing -TimeoutSec 15 | Out-Null } catch { throw "Cannot reach CRAN: $($_.Exception.Message)" }
    Pass "CRAN is reachable"
    New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
    $OutputDir = (Resolve-Path -LiteralPath $OutputDir).Path
    $probe = Join-Path $OutputDir (".miraprot-write-test-" + [guid]::NewGuid().ToString("N"))
    try { [IO.File]::WriteAllText($probe, "test"); Remove-Item -LiteralPath $probe -Force } catch { throw "Output directory is not writable: $OutputDir. $($_.Exception.Message)" }
    Pass "output directory is writable and will be retained: $OutputDir"

    $Builder = Join-Path $ScriptDir "bundle-r-windows.ps1"
    $BuilderArgs = @("-RVersion", $RVersion, "-OutputDir", $OutputDir)
    Write-Log ("BUILDER INVOCATION: & {0} {1}" -f (Quote-Argument $Builder), (($BuilderArgs | ForEach-Object { Quote-Argument $_ }) -join " "))
    & powershell.exe -NoProfile -ExecutionPolicy Bypass -File $Builder @BuilderArgs 2>&1 | ForEach-Object { Write-Host $_; Add-Content -LiteralPath $LogFile -Value $_.ToString() -Encoding UTF8 }
    $builderExit = $LASTEXITCODE
    if ($null -eq $builderExit) { $builderExit = if ($?) { 0 } else { 1 } }
    Write-Log "BUILDER EXIT CODE: $builderExit"
    if ($builderExit -ne 0) { $script:ExitCode = [int]$builderExit; throw "Stage-1 builder failed with exit code $builderExit." }

    $Launcher = Join-Path $OutputDir "MiraProt-launcher.exe"
    Assert-Preflight (Test-Path -LiteralPath $Launcher -PathType Leaf) "Builder succeeded but launcher is missing: $Launcher"
    Assert-Preflight ((Get-Item -LiteralPath $Launcher).Length -gt 0) "Builder succeeded but launcher is empty: $Launcher"
    $verification = Get-NativeOutput -FilePath $Launcher -ArgumentList @("--version")
    $verification.Output | ForEach-Object { Write-Host $_ }
    Assert-Preflight ($verification.ExitCode -eq 0) "Launcher --version failed with exit code $($verification.ExitCode)."
    Assert-Preflight (-not [string]::IsNullOrWhiteSpace(($verification.Output -join "`n"))) "Launcher --version returned empty output."
    Write-Log "VERIFICATION PASS: non-empty launcher and successful --version invocation"
    $script:ExitCode = 0
} catch {
    Write-Log "ERROR: $($_.Exception.Message)"
    if ($script:ExitCode -eq 0) { $script:ExitCode = 1 }
} finally {
    if ($script:ExitCode -eq 0) { Write-Log "FINAL STATUS: SUCCESS" } else { Write-Log "FINAL STATUS: FAILED" }
    Write-Log "EXIT CODE: $script:ExitCode"
    if ($Interactive) { Read-Host "Press Enter to close" | Out-Null }
}
exit $script:ExitCode
