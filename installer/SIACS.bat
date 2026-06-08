@echo off
REM ===========================================================================
REM  SIACS launcher (Windows)
REM  Double-click this file to start SIACS. No RStudio or separate R install
REM  is required: a portable copy of R is bundled in this folder.
REM
REM  Layout expected next to this file:
REM     R-portable\       portable R installation (bin\Rscript.exe)
REM     library\          pre-installed R packages
REM     app\              the SIACS application (main_app.R, engine, Input, ...)
REM     launch_siacs.R    bootstrap sourced by R
REM ===========================================================================
setlocal

REM Resolve the directory this .bat lives in (handles spaces; trailing slash).
set "BUNDLE=%~dp0"

REM Locate the bundled Rscript.
set "RSCRIPT=%BUNDLE%R-portable\bin\Rscript.exe"
if not exist "%RSCRIPT%" (
  echo.
  echo ERROR: Bundled R not found at:
  echo   %RSCRIPT%
  echo.
  echo This SIACS bundle appears to be incomplete. Please re-download the full
  echo release ZIP from GitHub and unzip it completely before running.
  echo.
  pause
  exit /b 1
)

echo Starting SIACS... a browser window will open shortly.
echo Keep THIS window open while you use SIACS. Close it to stop the app.
echo.

REM --no-save/--no-restore keep the session clean but still honor .Renviron so
REM the bundled library path resolves. We pass the bootstrap via --file.
"%RSCRIPT%" --no-save --no-restore --encoding=UTF-8 "%BUNDLE%launch_siacs.R"

set "EXITCODE=%ERRORLEVEL%"
if not "%EXITCODE%"=="0" (
  echo.
  echo SIACS exited with code %EXITCODE%.
  echo If this was unexpected, check the log files in your SIACS workspace
  echo folder ^(shown in the app's startup messages^) and report the issue.
  echo.
  pause
)
endlocal
