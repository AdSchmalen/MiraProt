$ErrorActionPreference = "Stop"
$Root = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$Bundler = Join-Path $Root "portable\scripts\bundle-r-windows.ps1"

# HelpersOnly is deliberately side-effect free: in particular, dot-sourcing the
# production helpers must not perform the bundler's CRAN preflight or create an
# output directory.
. $Bundler -HelpersOnly

function Assert-True {
    param([bool]$Condition, [string]$Message)
    if (-not $Condition) { throw $Message }
}

foreach ($value in @(
    "123456",
    (, @("123456")),
    (, @("123456", "123456")),
    "123456, 123456"
)) {
    try { $actual = Get-HttpContentLength -HeaderValue $value }
    catch { throw "Valid Content-Length representation threw: $($_.Exception.Message)" }
    Assert-True ($actual -is [int64]) "Valid Content-Length did not return Int64."
    Assert-True ($actual -eq [int64]123456) "Valid Content-Length normalized to '$actual', not 123456."
}

try { $nullLength = Get-HttpContentLength -HeaderValue $null }
catch { throw "Null Content-Length threw: $($_.Exception.Message)" }
Assert-True ($null -eq $nullLength) "Null Content-Length did not remain null."

foreach ($value in @(
    (, @("123456", "654321")),
    (, @("garbage")),
    "",
    "-1",
    "-9223372036854775808",
    "9223372036854775808"
)) {
    try { $actual = Get-HttpContentLength -HeaderValue $value }
    catch { throw "Invalid Content-Length representation threw: $($_.Exception.Message)" }
    Assert-True ($null -eq $actual) "Invalid Content-Length '$value' became '$actual' instead of null."
}

$TempRoot = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-content-length-tests-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $TempRoot | Out-Null
try {
    $installer = Join-Path $TempRoot "R-4.5.2-win.exe"
    $stream = [IO.File]::Create($installer)
    try { $stream.SetLength(11MB) } finally { $stream.Dispose() }

    # The policy cases target provenance-length handling. Bypass only the PE
    # parser; the metadata rejection is the stronger identity check proving
    # that missing/unparseable lengths did not accidentally approve the file.
    function Test-WindowsExecutableHeader { param([string]$Path) return $true }
    Remove-Item Env:MIRAPROT_TEST_INSTALLER_FIXTURE -ErrorAction SilentlyContinue

    function Invoke-LengthPolicyCase {
        param([string]$Name, [object]$HeaderValue, [bool]$ShouldReachIdentity)
        $record = [pscustomobject]@{
            SourceUrl = "https://cran.r-project.org/bin/windows/base/R-4.5.2-win.exe"
            HttpSuccess = $true
            StatusCode = 200
            FinalUrl = "https://cran.r-project.org/bin/windows/base/R-4.5.2-win.exe"
            ResponseContentLength = $HeaderValue
        }
        $script:policyResult = $null
        try {
            $output = (& { $script:policyResult = Test-RInstaller -Path $installer -SourceUrls @($record.SourceUrl) -RequestedVersion "4.5.2" -DownloadRecord $record } *>&1 | Out-String)
        } catch { throw "$Name policy case threw: $($_.Exception.Message)" }
        Assert-True (-not $script:policyResult) "$Name unexpectedly accepted the installer."
        Assert-True (-not $output.Contains('Cannot convert the "System.String[]" value of type "System.String[]" to type "System.Int64".')) "$Name emitted the array-to-Int64 conversion error."
        if ($ShouldReachIdentity) {
            Assert-True ($output.Contains("embedded file/product metadata is missing")) "$Name did not continue to stronger identity validation:`n$output"
        } else {
            Assert-True ($output.Contains("does not match actual downloaded size")) "$Name did not reject a usable mismatching length:`n$output"
            Assert-True (-not $output.Contains("embedded file/product metadata is missing")) "$Name continued after a usable length mismatch."
        }
        return $output
    }

    $size = [string](11MB)
    $captured = @()
    $captured += Invoke-LengthPolicyCase "matching scalar" $size $true
    $captured += Invoke-LengthPolicyCase "matching array" @($size, $size) $true
    $captured += Invoke-LengthPolicyCase "missing" $null $true
    $captured += Invoke-LengthPolicyCase "unparseable" @("garbage") $true
    $captured += Invoke-LengthPolicyCase "mismatch" "123456" $false
    $allOutput = $captured -join "`n"
    Assert-True (-not $allOutput.Contains('Cannot convert the "System.String[]" value of type "System.String[]" to type "System.Int64".')) "Focused suite captured the array-to-Int64 conversion error."

    Write-Host "bundle-r-windows Content-Length and installer-policy checks passed"
} finally {
    Remove-Item $TempRoot -Recurse -Force -ErrorAction SilentlyContinue
}
