<#
.SYNOPSIS
  Build a self-contained, double-click-to-run SIACS bundle for Windows.

.DESCRIPTION
  Produces a folder (and ZIP) containing:
    R-portable\      a portable R installation
    library\         all required R packages, pre-installed
    app\             the SIACS application source (copied from the repo)
    launch_siacs.R   bootstrap
    SIACS.bat        clickable launcher
    README-RUN.txt   end-user run instructions

  The resulting ZIP is what you attach to a GitHub Release on vilacqua/SIACS.
  End users download it, unzip locally, and double-click SIACS.bat.

.PARAMETER RVersion
  R version to bundle. Default "latest" auto-detects the current CRAN release.
  You may also pass an explicit version (e.g. "4.5.1"); the script first tries
  the current-release path and then the CRAN old/ archive.

.PARAMETER OutDir
  Where to assemble the bundle (default: ..\dist relative to this script).

.PARAMETER SkipRDownload
  Reuse an already-downloaded/extracted R-portable if present.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File build_release.ps1
#>
param(
  [string] $RVersion = "latest",
  [string] $OutDir   = "",
  [switch] $SkipRDownload
)

$ErrorActionPreference = "Stop"
$ProgressPreference    = "SilentlyContinue"   # faster downloads

# ---- Paths ----------------------------------------------------------------
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot  = Split-Path -Parent $ScriptDir          # MayUpdate5\
if ([string]::IsNullOrWhiteSpace($OutDir)) {
  $OutDir = Join-Path $RepoRoot "dist"
}
$BundleName = "SIACS-windows"
$BundleDir  = Join-Path $OutDir $BundleName
$RPortable  = Join-Path $BundleDir "R-portable"
$LibDir     = Join-Path $BundleDir "library"
$AppDir     = Join-Path $BundleDir "app"

Write-Host "=== SIACS Windows bundle builder ===" -ForegroundColor Cyan
Write-Host "Repo root : $RepoRoot"
Write-Host "Output    : $BundleDir"
Write-Host "R version : $RVersion"
Write-Host ""

# ---- Clean / create bundle dir --------------------------------------------
if (Test-Path $BundleDir) {
  if (-not $SkipRDownload) {
    Write-Host "Removing existing bundle dir..." -ForegroundColor Yellow
    Remove-Item $BundleDir -Recurse -Force
  }
}
New-Item -ItemType Directory -Force -Path $BundleDir | Out-Null
New-Item -ItemType Directory -Force -Path $LibDir    | Out-Null

# ---------------------------------------------------------------------------
# 1) Obtain a portable R.
#    Strategy: download the official R Windows installer and extract it
#    silently into R-portable. The installer supports /DIR and silent flags.
#
#    CRAN keeps only the CURRENT release at .../base/R-x.y.z-win.exe; older
#    releases move to .../base/old/x.y.z/R-x.y.z-win.exe. We resolve the URL
#    robustly: if -RVersion is "latest", auto-detect the current release;
#    otherwise try the base path, then fall back to the old/ archive.
# ---------------------------------------------------------------------------
if ($SkipRDownload -and (Test-Path (Join-Path $RPortable "bin\Rscript.exe"))) {
  Write-Host "Reusing existing R-portable (--SkipRDownload)." -ForegroundColor Green
  # Resolve the real version from the reused R so the ZIP is named correctly
  # (otherwise -RVersion stays the literal "latest").
  try {
    $verOut = & (Join-Path $RPortable "bin\Rscript.exe") "-e" "cat(as.character(getRversion()))"
    if ($verOut -match '^\d+\.\d+\.\d+$') {
      $RVersion = $verOut.Trim()
      Write-Host "  Reused R version: $RVersion" -ForegroundColor Green
    }
  } catch {
    Write-Host "  (could not query reused R version; ZIP name may say 'latest')" -ForegroundColor Yellow
  }
} else {

  function Test-UrlExists([string]$Url) {
    try {
      $resp = Invoke-WebRequest -Uri $Url -Method Head -UseBasicParsing -TimeoutSec 30
      return ($resp.StatusCode -eq 200)
    } catch {
      return $false
    }
  }

  # Resolve "latest" to the actual current version string from CRAN.
  if ($RVersion -eq "latest") {
    Write-Host "Detecting latest R release from CRAN..." -ForegroundColor Cyan
    $verUrl = "https://cloud.r-project.org/bin/windows/base/release.html"
    try {
      $html = (Invoke-WebRequest -Uri $verUrl -UseBasicParsing -TimeoutSec 30).Content
      $m = [regex]::Match($html, "R-(\d+\.\d+\.\d+)-win\.exe")
      if ($m.Success) {
        $RVersion = $m.Groups[1].Value
        Write-Host "  Latest R is $RVersion" -ForegroundColor Green
      } else {
        throw "Could not parse latest R version from $verUrl"
      }
    } catch {
      throw "Failed to detect latest R version: $($_.Exception.Message)"
    }
  }

  # Candidate URLs in priority order: current-release path, then old/ archive.
  $base = "https://cloud.r-project.org/bin/windows/base"
  $candidates = @(
    "$base/R-$RVersion-win.exe",
    "$base/old/$RVersion/R-$RVersion-win.exe"
  )
  $RUrl = $null
  foreach ($c in $candidates) {
    Write-Host "Checking $c" -ForegroundColor DarkGray
    if (Test-UrlExists $c) { $RUrl = $c; break }
  }
  if (-not $RUrl) {
    throw ("Could not find an R $RVersion Windows installer at CRAN.`n" +
           "Tried:`n  " + ($candidates -join "`n  ") + "`n" +
           "Use -RVersion latest, or pick a version listed at`n" +
           "  https://cloud.r-project.org/bin/windows/base/old/")
  }

  $RInstaller = Join-Path $env:TEMP "R-$RVersion-win.exe"
  Write-Host "Downloading R $RVersion from CRAN..." -ForegroundColor Cyan
  Write-Host "  $RUrl"
  Invoke-WebRequest -Uri $RUrl -OutFile $RInstaller

  Write-Host "Extracting R silently into R-portable..." -ForegroundColor Cyan
  if (Test-Path $RPortable) { Remove-Item $RPortable -Recurse -Force }
  New-Item -ItemType Directory -Force -Path $RPortable | Out-Null

  # Inno Setup silent install with a custom dir.
  # IMPORTANT: the /DIR value must be wrapped in quotes because the bundle path
  # can contain spaces (e.g. "OneDrive - ICF"). Passing an unquoted path with
  # spaces causes Inno Setup to ignore /DIR and silently install nothing to our
  # target. We pass each arg as its own array element and quote /DIR's value.
  $rargs = @("/VERYSILENT", "/SUPPRESSMSGBOXES", "/SP-", "/NOICONS",
             "/NORESTART", "/CURRENTUSER", "/DIR=`"$RPortable`"")
  Write-Host "  installer: $RInstaller" -ForegroundColor DarkGray
  Write-Host "  /DIR=`"$RPortable`"" -ForegroundColor DarkGray
  $p = Start-Process -FilePath $RInstaller -ArgumentList $rargs -Wait -PassThru
  if ($p.ExitCode -ne 0) {
    throw "R installer exited with code $($p.ExitCode)."
  }

  # The installer may briefly hold files; wait until Rscript.exe materializes.
  $deadline = (Get-Date).AddSeconds(60)
  while (-not (Test-Path (Join-Path $RPortable "bin\Rscript.exe")) -and
         (Get-Date) -lt $deadline) {
    Start-Sleep -Milliseconds 500
  }
  if (-not (Test-Path (Join-Path $RPortable "bin\Rscript.exe"))) {
    Write-Host "Contents of $RPortable after install attempt:" -ForegroundColor Yellow
    Get-ChildItem $RPortable -ErrorAction SilentlyContinue | Format-Table Name | Out-String | Write-Host
    throw ("R-portable\bin\Rscript.exe not found after extraction.`n" +
           "The silent install likely failed. Try running the installer once `n" +
           "manually to confirm it works, or build into a path WITHOUT spaces `n" +
           "using -OutDir, e.g.:`n" +
           "  -OutDir C:\SIACS_build")
  }
  Write-Host "R extracted OK." -ForegroundColor Green
}

$Rscript = Join-Path $RPortable "bin\Rscript.exe"

# Make the bundled R relocatable: write an Renviron.site pointing the user
# library at the bundle's library\ folder. (.libPaths is also set by the
# launcher, but this makes ad-hoc Rscript calls during build consistent.)
$EtcDir = Join-Path $RPortable "etc"
New-Item -ItemType Directory -Force -Path $EtcDir | Out-Null
"R_LIBS_USER=`${R_HOME}/../library" | Out-File -FilePath (Join-Path $EtcDir "Renviron.site") -Encoding ascii

# ---------------------------------------------------------------------------
# 2) Copy the application source into app\.
#    Include only what the app needs at runtime; exclude dev/build artifacts,
#    previous run outputs, and the CRAN reference copy.
# ---------------------------------------------------------------------------
Write-Host "Copying application files into app\ ..." -ForegroundColor Cyan
New-Item -ItemType Directory -Force -Path $AppDir | Out-Null

# Top-level files & folders to include (relative to repo root).
$include = @(
  "main_app.R", "advanced_module.R", "wizard_module.R", "wizard_helpers.R",
  "wizard_defaults.R", "shared.r", "siacs_diagnostics.R",
  "SIACS_0924_Merged_2.R", "SIACS_Compile_Source.R",
  "SIACS_main_function.R", "SIACSFunctions.R", "SIACSInterpolationFunctions.R",
  "SIACSPreProcessSources.R", "SIACSCalcDerivatives.R", "SIACSBasicPlots.R",
  "SIACSPostProcessing.R", "SIACSLights.R", "SIACSmodelODEs.R",
  "SIACSMechanismInfo.R", "SIACSMessageFunctions.R", "SIACSRateConstantsFncs.R",
  "SAPRC07T.R", "SAPRC99.R", "spcSAPRC99.csv", "USER_GUIDE.md",
  "Input", "tuv5.3.1.exe"
)
foreach ($item in $include) {
  $src = Join-Path $RepoRoot $item
  if (Test-Path $src) {
    Copy-Item $src -Destination $AppDir -Recurse -Force
  } else {
    Write-Host "  WARN: missing, skipped: $item" -ForegroundColor Yellow
  }
}

# ---------------------------------------------------------------------------
# 3) Copy launcher + bootstrap to the bundle root.
# ---------------------------------------------------------------------------
Copy-Item (Join-Path $ScriptDir "SIACS.bat")       -Destination $BundleDir -Force
Copy-Item (Join-Path $ScriptDir "launch_siacs.R")  -Destination $BundleDir -Force
if (Test-Path (Join-Path $ScriptDir "README-RUN.txt")) {
  Copy-Item (Join-Path $ScriptDir "README-RUN.txt") -Destination $BundleDir -Force
}

# ---------------------------------------------------------------------------
# 4) Pre-install all R packages into the bundle library\.
#    Done with the bundled Rscript so the compiled package binaries match the
#    bundled R version exactly. Uses a temporary install script.
# ---------------------------------------------------------------------------
Write-Host "Pre-installing R packages into library\ (this can take a while)..." -ForegroundColor Cyan

$pkgList = @(
  # GUI
  "shiny","jsonlite","tidygeocoder","dplyr","DT","shinyFiles",
  "rhandsontable","shinyjs","shinyBS","readxl","zoo","processx","plotly","reshape2",
  # Engine
  "deSolve","reshape","ggplot2","openxlsx","doParallel","foreach","rstudioapi"
)
$pkgVector = ($pkgList | ForEach-Object { "`"$_`"" }) -join ","

$installScript = Join-Path $env:TEMP "siacs_install_pkgs.R"
$libForR = ($LibDir -replace '\\','/')
@"
options(repos = c(CRAN = "https://cloud.r-project.org"))
lib <- "$libForR"
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
pkgs <- c($pkgVector)
have <- rownames(installed.packages(lib.loc = lib))
need <- setdiff(pkgs, have)
cat("Need to install:", length(need), "packages\n")
if (length(need) > 0) {
  install.packages(need, lib = lib, dependencies = c("Depends","Imports","LinkingTo"))
}
# Verify
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("FAILED to install:", paste(missing, collapse=", "), "\n"); quit(status = 1)
}
cat("All", length(pkgs), "packages present in bundle library.\n")
"@ | Out-File -FilePath $installScript -Encoding ascii

& $Rscript "--vanilla" $installScript
if ($LASTEXITCODE -ne 0) {
  throw "Package pre-installation failed (exit $LASTEXITCODE)."
}
Remove-Item $installScript -Force -ErrorAction SilentlyContinue

# ---------------------------------------------------------------------------
# 5) Strip stale .Rc bytecode from app\ (rebuilt at runtime in a user cache).
# ---------------------------------------------------------------------------
Get-ChildItem $AppDir -Recurse -Filter *.Rc -ErrorAction SilentlyContinue |
  Remove-Item -Force -ErrorAction SilentlyContinue

# ---------------------------------------------------------------------------
# 6) Zip the bundle for release.
# ---------------------------------------------------------------------------
$Zip = Join-Path $OutDir "$BundleName-R$RVersion.zip"
Write-Host "Creating release ZIP: $Zip" -ForegroundColor Cyan
if (Test-Path $Zip) { Remove-Item $Zip -Force }
Compress-Archive -Path $BundleDir -DestinationPath $Zip -CompressionLevel Optimal

$zipSize = "{0:N1} MB" -f ((Get-Item $Zip).Length / 1MB)
Write-Host ""
Write-Host "=== DONE ===" -ForegroundColor Green
Write-Host "Bundle folder : $BundleDir"
Write-Host "Release ZIP   : $Zip  ($zipSize)"
Write-Host ""
Write-Host "Next steps:"
Write-Host "  1. Test: open $BundleDir and double-click SIACS.bat"
Write-Host "  2. Upload the ZIP to a GitHub Release on vilacqua/SIACS"
Write-Host "     (see installer\BUILD.md for the gh release command)."
