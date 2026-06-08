<#
.SYNOPSIS
  Assemble a clean, upload-ready copy of the SIACS source for GitHub.

.DESCRIPTION
  Copies ONLY the files that belong in the git repo into
  <repo>\dist_upload\SIACS-repo-content\, excluding build artifacts, bundles,
  run outputs, backups, and the CRAN reference copy. The result mirrors what a
  `git add` (filtered by .gitignore) would stage, but as a plain folder you can
  inspect, drag-upload, or `git init` inside.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File installer\make_repo_content.ps1
#>
param(
  [string] $OutDir = ""
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot  = Split-Path -Parent $ScriptDir
if ([string]::IsNullOrWhiteSpace($OutDir)) {
  $OutDir = Join-Path $RepoRoot "dist_upload"
}
$Dest = Join-Path $OutDir "SIACS-repo-content"

Write-Host "=== Assembling clean repo content ===" -ForegroundColor Cyan
Write-Host "Source : $RepoRoot"
Write-Host "Dest   : $Dest"
Write-Host ""

if (Test-Path $Dest) { Remove-Item $Dest -Recurse -Force }
New-Item -ItemType Directory -Force -Path $Dest | Out-Null

# Top-level FILES to include (the application + scripts + docs).
$files = @(
  "main_app.R", "advanced_module.R", "wizard_module.R", "wizard_helpers.R",
  "wizard_defaults.R", "shared.r", "siacs_diagnostics.R",
  "SIACS_0924_Merged_2.R", "SIACS_Compile_Source.R",
  "SIACS_main_function.R", "SIACSFunctions.R", "SIACSInterpolationFunctions.R",
  "SIACSPreProcessSources.R", "SIACSCalcDerivatives.R", "SIACSBasicPlots.R",
  "SIACSPostProcessing.R", "SIACSLights.R", "SIACSmodelODEs.R",
  "SIACSMechanismInfo.R", "SIACSMessageFunctions.R", "SIACSRateConstantsFncs.R",
  "SAPRC07T.R", "SAPRC99.R", "Comparison_GS.R",
  "spcSAPRC99.csv", "USER_GUIDE.md", ".gitignore"
)

# Top-level FOLDERS to include (recursively).
$folders = @(
  "Input",
  "tuv5.3.1.exe",
  "installer"
)

$copiedFiles = 0
foreach ($f in $files) {
  $src = Join-Path $RepoRoot $f
  if (Test-Path $src) {
    Copy-Item $src -Destination (Join-Path $Dest $f) -Force
    $copiedFiles++
  } else {
    Write-Host "  (skip, missing) $f" -ForegroundColor DarkYellow
  }
}

foreach ($d in $folders) {
  $src = Join-Path $RepoRoot $d
  if (Test-Path $src) {
    Copy-Item $src -Destination $Dest -Recurse -Force
  } else {
    Write-Host "  (skip, missing) $d\" -ForegroundColor DarkYellow
  }
}

# Safety scrub: remove anything that should never ship even if it slipped into
# one of the copied folders (e.g. a stray .Rc, log, or run output under Input).
$scrub = @("*.Rc", "*.log", "log__*.txt", "Rplots.pdf", ".RData", ".Rhistory")
foreach ($pat in $scrub) {
  Get-ChildItem $Dest -Recurse -Filter $pat -File -ErrorAction SilentlyContinue |
    Remove-Item -Force -ErrorAction SilentlyContinue
}
# Remove any accidentally-copied run dirs inside Input or elsewhere.
Get-ChildItem $Dest -Recurse -Directory -ErrorAction SilentlyContinue |
  Where-Object { $_.Name -match '^(Output_|Input_tmp_|Input_).*' -or $_.Name -eq 'Output' } |
  Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

# Report.
$totalFiles = (Get-ChildItem $Dest -Recurse -File).Count
$totalMB    = "{0:N1}" -f ((Get-ChildItem $Dest -Recurse -File | Measure-Object Length -Sum).Sum / 1MB)
$big        = Get-ChildItem $Dest -Recurse -File | Where-Object { $_.Length -gt 100MB }

Write-Host ""
Write-Host "=== DONE ===" -ForegroundColor Green
Write-Host "Top-level files copied : $copiedFiles"
Write-Host "Total files in bundle  : $totalFiles"
Write-Host "Total size             : $totalMB MB"
if ($big) {
  Write-Host "WARNING: files over 100 MB (GitHub will reject these):" -ForegroundColor Red
  $big | ForEach-Object { Write-Host "  $($_.FullName)" -ForegroundColor Red }
} else {
  Write-Host "No files over 100 MB. Safe for GitHub." -ForegroundColor Green
}
Write-Host ""
Write-Host "Clean content is at:" -ForegroundColor Cyan
Write-Host "  $Dest"
