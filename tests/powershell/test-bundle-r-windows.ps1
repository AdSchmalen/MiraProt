$ErrorActionPreference = "Stop"
$Root = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$Bundler = Join-Path $Root "portable\scripts\bundle-r-windows.ps1"
$TempRoot = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-bundler-tests-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $TempRoot | Out-Null

try {
    $helperSource = Join-Path $TempRoot "fake-r.go"
    @'
package main
import ("fmt"; "os"; "path/filepath"; "strings"; "syscall")
func main() {
  if len(os.Args) == 2 && os.Args[1] != "--version" {
    if os.Getenv("FAKE_INSTALL_EXIT") != "" { os.Exit(17) }
    if os.Getenv("FAKE_INSTALL_NO_RSCRIPT") == "" {
      data, _ := os.ReadFile(os.Args[0])
      for _, rel := range []string{"bin/R.exe", "bin/Rscript.exe"} {
        if strings.EqualFold(os.Getenv("FAKE_INSTALL_OMIT"), filepath.FromSlash(rel)) { continue }
        dst := filepath.Join(os.Args[1], rel); os.MkdirAll(filepath.Dir(dst), 0755); os.WriteFile(dst, data, 0755)
      }
      for _, rel := range []string{"bin/x64/R.dll", "etc/Rconsole", "etc/Rprofile.site", "library/base/DESCRIPTION"} {
        if strings.EqualFold(os.Getenv("FAKE_INSTALL_OMIT"), filepath.FromSlash(rel)) { continue }
        dst := filepath.Join(os.Args[1], rel); os.MkdirAll(filepath.Dir(dst), 0755); os.WriteFile(dst, []byte("fixture"), 0644)
      }
    }
    return
  }
  base := strings.ToLower(filepath.Base(os.Args[0]))
  prefix := "FAKE_RSCRIPT_"; if base == "r.exe" { prefix = "FAKE_R_" }
  if os.Getenv("FAKE_REQUIRE_CLEAN") != "" {
    for _, name := range []string{"R_HOME","R_ARCH","R_LIBS","R_LIBS_USER","R_LIBS_SITE","R_ENVIRON","R_ENVIRON_USER","R_PROFILE","R_PROFILE_USER"} {
      if os.Getenv(name) != "" { fmt.Fprint(os.Stderr, "inherited "+name); os.Exit(29) }
    }
  }
  if os.Getenv("FAKE_FINAL_FAILURE") != "" && strings.Contains(strings.ToLower(os.Args[0]), "r-portable") { os.Exit(37) }
  if os.Getenv(prefix+"ACCESS_VIOLATION") != "" { syscall.NewLazyDLL("kernel32.dll").NewProc("ExitProcess").Call(0xC0000005) }
  if len(os.Args) == 2 && os.Args[1] == "--version" {
    fmt.Fprint(os.Stdout, os.Getenv(prefix+"VERSION_OUTPUT"))
    fmt.Fprint(os.Stderr, os.Getenv(prefix+"VERSION_ERROR"))
    if os.Getenv(prefix+"VERSION_EXIT") != "" { os.Exit(23) }
    return
  }
  fmt.Fprint(os.Stdout, os.Getenv("FAKE_R_STDOUT"))
  fmt.Fprint(os.Stderr, os.Getenv("FAKE_R_STDERR"))
  if os.Getenv("FAKE_R_EXIT") != "" { os.Exit(23) }
}
'@ | Set-Content -LiteralPath $helperSource -Encoding ascii
    $helper = Join-Path $TempRoot "fake-process.exe"
    & go build -o $helper $helperSource
    if ($LASTEXITCODE -ne 0) { throw "Could not build fake process helper." }
    function Invoke-Case {
        param(
            [string]$Name, [string]$VersionOutput, [switch]$VersionFailure,
            [string]$VersionError = "", [switch]$NoExistingRuntime,
            [switch]$InstallerFailure, [switch]$InstallerOmitsRscript,
            [switch]$InvalidInstaller, [switch]$InvalidInstallerIdentity,
            [string]$MissingRequired, [switch]$FinalFailure,
            [switch]$AccessViolation, [switch]$ContaminatedEnvironment, [switch]$PartialRuntime,
            [switch]$RStartupFailure, [switch]$RscriptStartupFailure, [switch]$FakeRBeforePath,
            [string]$RStartupOutput = "R version 4.5.2", [string]$RStartupError = "",
            [string]$RscriptStartupOutput = "Rscript (R) version 4.5.2", [string]$RscriptStartupError = "",
            [string]$StagingRoot = "",
            [int]$ExpectedStatus, [string[]]$ExpectedMessages,
            [string]$OutputDir = (Join-Path $TempRoot $Name)
        )
        Remove-Item -LiteralPath $OutputDir -Recurse -Force -ErrorAction SilentlyContinue
        New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
        $rscript = Join-Path $OutputDir "r-portable\bin\Rscript.exe"
        if (-not $NoExistingRuntime) {
            New-Item -ItemType Directory -Force -Path (Split-Path $rscript) | Out-Null
            Copy-Item $helper $rscript
            Copy-Item $helper (Join-Path $OutputDir "r-portable\bin\R.exe")
            foreach ($relativePath in @("bin\x64\R.dll", "etc\Rconsole", "etc\Rprofile.site", "library\base\DESCRIPTION")) {
                $fixturePath = Join-Path (Join-Path $OutputDir "r-portable") $relativePath
                New-Item -ItemType Directory -Force -Path (Split-Path $fixturePath) | Out-Null
                Set-Content -LiteralPath $fixturePath -Value "fixture"
            }
            if ($MissingRequired) { Remove-Item -LiteralPath (Join-Path (Join-Path $OutputDir "r-portable") $MissingRequired) -Recurse -Force }
        } elseif ($PartialRuntime) {
            New-Item -ItemType Directory -Force -Path (Join-Path $OutputDir "r-portable\lib") | Out-Null
            Set-Content (Join-Path $OutputDir "r-portable\lib\partial.txt") "partial"
        }
        $installer = Join-Path $TempRoot "$Name-installer.exe"
        if ($InvalidInstaller) { Set-Content $installer "not an installer" }
        else {
            Copy-Item $helper $installer -Force
            if ($InvalidInstallerIdentity) { $s = [IO.File]::OpenWrite($installer); try { $s.SetLength(11MB) } finally { $s.Dispose() } }
        }

        $env:MIRAPROT_BUNDLER_TEST_MODE = "1"
        $env:MIRAPROT_TEST_INSTALLER_PATH = $installer
        $env:MIRAPROT_TEST_INSTALLER_COMMAND = $helper
        $env:MIRAPROT_TEST_INSTALLER_FIXTURE = if ($InvalidInstallerIdentity) { "" } else { "1" }
        $env:MIRAPROT_TEST_STAGING_ROOT = $StagingRoot
        $env:FAKE_INSTALL_OMIT = $MissingRequired
        $env:FAKE_FINAL_FAILURE = if ($FinalFailure) { "1" } else { "" }
        $env:FAKE_R_ACCESS_VIOLATION = if ($AccessViolation) { "1" } else { "" }
        $env:FAKE_R_VERSION_OUTPUT = $RStartupOutput
        $env:FAKE_R_VERSION_ERROR = $RStartupError
        $env:FAKE_RSCRIPT_VERSION_OUTPUT = $RscriptStartupOutput
        $env:FAKE_RSCRIPT_VERSION_ERROR = $RscriptStartupError
        $env:FAKE_R_VERSION_EXIT = if ($RStartupFailure) { "1" } else { "" }
        $env:FAKE_RSCRIPT_VERSION_EXIT = if ($RscriptStartupFailure) { "1" } else { "" }
        $env:FAKE_R_STDOUT = $VersionOutput
        $env:FAKE_R_STDERR = $VersionError
        $env:FAKE_R_EXIT = if ($VersionFailure) { "1" } else { "" }
        $env:FAKE_INSTALL_EXIT = if ($InstallerFailure) { "1" } else { "" }
        $env:FAKE_INSTALL_NO_RSCRIPT = if ($InstallerOmitsRscript) { "1" } else { "" }
        $originalPath = $env:PATH
        if ($FakeRBeforePath) {
            $fakePath = Join-Path $TempRoot "fake-local-r"
            New-Item -ItemType Directory -Force -Path $fakePath | Out-Null
            Copy-Item $helper (Join-Path $fakePath "R.exe") -Force
            Copy-Item $helper (Join-Path $fakePath "Rscript.exe") -Force
            $env:PATH = "$fakePath$([IO.Path]::PathSeparator)$originalPath"
        }
        $env:FAKE_REQUIRE_CLEAN = if ($ContaminatedEnvironment) { "1" } else { "" }
        if ($ContaminatedEnvironment) { $env:R_HOME = "C:\host-r"; $env:R_PROFILE_USER = "C:\host-profile" }
        $global:LASTEXITCODE = 0
        $text = (& pwsh -NoProfile -File $Bundler -RVersion 4.5.2 -OutputDir $OutputDir 2>&1 | Out-String)
        $status = $LASTEXITCODE
        $env:PATH = $originalPath
        Remove-Item Env:R_HOME, Env:R_PROFILE_USER -ErrorAction SilentlyContinue
        if ($status -ne $ExpectedStatus) { throw "$Name expected status $ExpectedStatus, got $status`n$text" }
        foreach ($message in $ExpectedMessages) {
            if (-not $text.Contains($message)) { throw "$Name did not report '$message':`n$text" }
        }
        if ($ExpectedStatus -ne 0 -and $text.Contains("Installing R packages")) {
            throw "$Name continued to package installation after failure."
        }
        return $text
    }

    Invoke-Case valid "4.5.2" -ExpectedStatus 0 -ExpectedMessages @("validation completed") | Out-Null
    Invoke-Case wrong "4.5.1" -ExpectedStatus 1 -ExpectedMessages @("R version mismatch", "requested R 4.5.2") | Out-Null
    Invoke-Case empty-zero "" -ExpectedStatus 1 -ExpectedMessages @("returned an empty version") | Out-Null
    Invoke-Case malformed "R version 4.5.2" -ExpectedStatus 1 -ExpectedMessages @("malformed version", "expected MAJOR.MINOR.PATCH") | Out-Null
    Invoke-Case r-startup-stderr-banner "4.5.2" -RStartupOutput "" -RStartupError "R version 4.5.2 (2025-10-31 ucrt)" -ExpectedStatus 0 -ExpectedMessages @('Rscript.exe --vanilla -s -e "cat(as.character(getRversion()))"', "validation completed") | Out-Null
    Invoke-Case r-startup-stdout-banner "4.5.2" -RStartupError "" -ExpectedStatus 0 -ExpectedMessages @("validation completed") | Out-Null
    Invoke-Case rscript-startup-stderr-banner "4.5.2" -RscriptStartupOutput "" -RscriptStartupError "Rscript (R) version 4.5.2" -ExpectedStatus 0 -ExpectedMessages @('Rscript.exe --vanilla -s -e "cat(as.character(getRversion()))"', "validation completed") | Out-Null
    Invoke-Case rscript-startup-stdout-banner "4.5.2" -RscriptStartupError "" -ExpectedStatus 0 -ExpectedMessages @("validation completed") | Out-Null
    Invoke-Case r-startup-failure "4.5.2" -RStartupOutput "" -RStartupError "loader failure" -RStartupFailure -ExpectedStatus 1 -ExpectedMessages @("R.exe --version", "loader failure", "nonzero exit code 23") | Out-Null
    Invoke-Case rscript-startup-failure "4.5.2" -RscriptStartupOutput "" -RscriptStartupError "startup failure" -RscriptStartupFailure -ExpectedStatus 1 -ExpectedMessages @("Rscript.exe --version", "startup failure", "nonzero exit code 23") | Out-Null
    Invoke-Case empty-nonzero "" -VersionFailure -VersionError "loader failed" -ExpectedStatus 1 -ExpectedMessages @("nonzero exit code 23", "0x00000017") | Out-Null
    Invoke-Case missing-executable "4.5.2" -NoExistingRuntime -InstallerOmitsRscript -FakeRBeforePath -ExpectedStatus 1 -ExpectedMessages @("missing required file", "bin\Rscript.exe") | Out-Null
    Invoke-Case failed-installer "4.5.2" -NoExistingRuntime -InstallerFailure -ExpectedStatus 1 -ExpectedMessages @("failed with exit code 17") | Out-Null
    Invoke-Case invalid-cache "4.5.2" -NoExistingRuntime -InvalidInstaller -ExpectedStatus 1 -ExpectedMessages @("missing or invalid", "refusing to continue") | Out-Null
    Invoke-Case invalid-identity "4.5.2" -NoExistingRuntime -InvalidInstallerIdentity -ExpectedStatus 1 -ExpectedMessages @("embedded file/product metadata is missing", "refusing to continue") | Out-Null
    foreach ($required in @("bin\R.exe", "bin\Rscript.exe", "bin\x64\R.dll", "etc\Rconsole", "etc\Rprofile.site", "library", "library\base\DESCRIPTION")) {
        Invoke-Case ("missing-" + ($required -replace '[\\.]','-')) "4.5.2" -NoExistingRuntime -MissingRequired $required -ExpectedStatus 1 -ExpectedMessages @("missing required file", $required, "Failed staging retained", "Failure logs retained") | Out-Null
    }
    Invoke-Case access-violation "4.5.2" -AccessViolation -ExpectedStatus 1 -ExpectedMessages @("0xC0000005", "signed exit code -1073741819") | Out-Null
    Invoke-Case contaminated "4.5.2" -ContaminatedEnvironment -ExpectedStatus 0 -ExpectedMessages @("Ignoring inherited R environment variables", "validation completed") | Out-Null
    Invoke-Case promoted-rollback "4.5.2" -NoExistingRuntime -FinalFailure -ExpectedStatus 1 -ExpectedMessages @("promotion completion", "promoted-runtime validation start", "Failed staging retained") | Out-Null
    Invoke-Case partial-runtime "4.5.2" -NoExistingRuntime -PartialRuntime -ExpectedStatus 0 -ExpectedMessages @("Portable R installed", "validation completed") | Out-Null
    $outside = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-output-outside-" + [guid]::NewGuid().ToString("N"))
    try {
        Invoke-Case outside-output "4.5.2" -ExpectedStatus 0 -OutputDir $outside -ExpectedMessages @("Output:    $outside", "validation completed") | Out-Null
        $spaced = Join-Path $outside "output and staging spaces"
        Invoke-Case spaced-output "4.5.2" -NoExistingRuntime -OutputDir $spaced -StagingRoot (Join-Path $outside "staging path with spaces") -ExpectedStatus 0 -ExpectedMessages @("promotion completion", "Portable R installed") | Out-Null
    } finally { Remove-Item $outside -Recurse -Force -ErrorAction SilentlyContinue }
    Write-Host "bundle-r-windows isolated process checks passed"
} finally {
    Remove-Item $TempRoot -Recurse -Force -ErrorAction SilentlyContinue
}
