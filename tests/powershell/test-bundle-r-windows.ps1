$ErrorActionPreference = "Stop"
$Root = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$Bundler = Join-Path $Root "portable\scripts\bundle-r-windows.ps1"
$TempRoot = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-bundler-tests-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $TempRoot | Out-Null

try {
    $helperSource = Join-Path $TempRoot "fake-r.go"
    @'
package main
import ("fmt"; "os"; "path/filepath")
func main() {
  if len(os.Args) == 2 && os.Args[1] != "--version" {
    if os.Getenv("FAKE_INSTALL_EXIT") != "" { os.Exit(17) }
    if os.Getenv("FAKE_INSTALL_NO_RSCRIPT") == "" {
      data, _ := os.ReadFile(os.Args[0])
      for _, rel := range []string{"bin/R.exe", "bin/Rscript.exe"} {
        dst := filepath.Join(os.Args[1], rel); os.MkdirAll(filepath.Dir(dst), 0755); os.WriteFile(dst, data, 0755)
      }
      for _, rel := range []string{"bin/x64/R.dll", "etc/Rconsole", "etc/Rprofile.site", "VERSION", "library/base/DESCRIPTION"} {
        dst := filepath.Join(os.Args[1], rel); os.MkdirAll(filepath.Dir(dst), 0755); os.WriteFile(dst, []byte("fixture"), 0644)
      }
    }
    return
  }
  if len(os.Args) == 2 && os.Args[1] == "--version" {
    fmt.Fprint(os.Stdout, "R version 4.5.2")
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
    # A PE overlay is valid and lets the helper satisfy the production minimum
    # installer-size check without downloading R.
    $stream = [IO.File]::OpenWrite($helper)
    try { $stream.SetLength(11MB) } finally { $stream.Dispose() }

    function Invoke-Case {
        param(
            [string]$Name, [string]$VersionOutput, [switch]$VersionFailure,
            [string]$VersionError = "", [switch]$NoExistingRuntime,
            [switch]$InstallerFailure, [switch]$InstallerOmitsRscript,
            [switch]$InvalidInstaller, [switch]$PartialRuntime,
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
            foreach ($relativePath in @("bin\x64\R.dll", "etc\Rconsole", "etc\Rprofile.site", "VERSION", "library\base\DESCRIPTION")) {
                $fixturePath = Join-Path (Join-Path $OutputDir "r-portable") $relativePath
                New-Item -ItemType Directory -Force -Path (Split-Path $fixturePath) | Out-Null
                Set-Content -LiteralPath $fixturePath -Value "fixture"
            }
        } elseif ($PartialRuntime) {
            New-Item -ItemType Directory -Force -Path (Join-Path $OutputDir "r-portable\lib") | Out-Null
            Set-Content (Join-Path $OutputDir "r-portable\lib\partial.txt") "partial"
        }
        $installer = Join-Path $TempRoot "$Name-installer.exe"
        if ($InvalidInstaller) { Set-Content $installer "not an installer" }
        else { Copy-Item $helper $installer -Force }

        $env:MIRAPROT_BUNDLER_TEST_MODE = "1"
        $env:MIRAPROT_TEST_INSTALLER_PATH = $installer
        $env:MIRAPROT_TEST_INSTALLER_COMMAND = $helper
        $env:FAKE_R_STDOUT = $VersionOutput
        $env:FAKE_R_STDERR = $VersionError
        $env:FAKE_R_EXIT = if ($VersionFailure) { "1" } else { "" }
        $env:FAKE_INSTALL_EXIT = if ($InstallerFailure) { "1" } else { "" }
        $env:FAKE_INSTALL_NO_RSCRIPT = if ($InstallerOmitsRscript) { "1" } else { "" }
        $global:LASTEXITCODE = 0
        $text = (& pwsh -NoProfile -File $Bundler -RVersion 4.5.2 -OutputDir $OutputDir 2>&1 | Out-String)
        $status = $LASTEXITCODE
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
    Invoke-Case empty-nonzero "" -VersionFailure -VersionError "loader failed" -ExpectedStatus 1 -ExpectedMessages @("nonzero exit code 23", "0x00000017") | Out-Null
    Invoke-Case missing-executable "4.5.2" -NoExistingRuntime -InstallerOmitsRscript -ExpectedStatus 1 -ExpectedMessages @("missing required file") | Out-Null
    Invoke-Case failed-installer "4.5.2" -NoExistingRuntime -InstallerFailure -ExpectedStatus 1 -ExpectedMessages @("failed with exit code 17") | Out-Null
    Invoke-Case invalid-cache "4.5.2" -NoExistingRuntime -InvalidInstaller -ExpectedStatus 1 -ExpectedMessages @("missing or invalid", "refusing to continue") | Out-Null
    Invoke-Case partial-runtime "4.5.2" -NoExistingRuntime -PartialRuntime -ExpectedStatus 0 -ExpectedMessages @("Portable R installed", "validation completed") | Out-Null
    $outside = Join-Path ([IO.Path]::GetTempPath()) ("miraprot-output-outside-" + [guid]::NewGuid().ToString("N"))
    try {
        Invoke-Case outside-output "4.5.2" -ExpectedStatus 0 -OutputDir $outside -ExpectedMessages @("Output:    $outside", "validation completed") | Out-Null
    } finally { Remove-Item $outside -Recurse -Force -ErrorAction SilentlyContinue }
    Write-Host "bundle-r-windows isolated process checks passed"
} finally {
    Remove-Item $TempRoot -Recurse -Force -ErrorAction SilentlyContinue
}
