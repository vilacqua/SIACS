# launch_siacs.R
# ---------------------------------------------------------------------------
# Bootstrap launched by SIACS.bat inside the portable-R bundle. It:
#   1. Points the library path at the bundled ./library so packages that were
#      pre-installed at build time are found offline.
#   2. Installs any still-missing packages (only if the user has internet and
#      the bundle was built without pre-installing them).
#   3. Launches the Shiny app in the user's default browser.
#
# This file lives at the ROOT of the distributable bundle, next to SIACS.bat,
# the app/ folder, R-portable/ and library/. It is NOT the same as the app's
# own main_app.R (which lives in app/).
# ---------------------------------------------------------------------------

# Resolve the bundle root = directory containing this script.
bundle_root <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1L) normalizePath(dirname(f), winslash = "/", mustWork = FALSE)
  else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}, error = function(e) normalizePath(getwd(), winslash = "/", mustWork = FALSE))

app_dir <- file.path(bundle_root, "app")
lib_dir <- file.path(bundle_root, "library")

# Use the bundled library FIRST so pre-installed packages win over anything on
# the user's machine, and create it if missing (first-run install fallback).
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

cat("==============================================\n")
cat(" SIACS launcher\n")
cat("   bundle root :", bundle_root, "\n")
cat("   app dir     :", app_dir, "\n")
cat("   library     :", lib_dir, "\n")
cat("   R version   :", R.version.string, "\n")
cat("==============================================\n")

# Packages the GUI needs to START. (Engine packages such as deSolve are
# ensured by main_app.R at startup, but we list the core GUI set here so the
# launcher fails loudly with a clear message if the bundle is incomplete.)
gui_pkgs <- c("shiny", "jsonlite", "tidygeocoder", "dplyr", "DT", "shinyFiles",
              "rhandsontable", "shinyjs", "shinyBS", "readxl", "zoo",
              "processx", "plotly", "reshape2")

missing <- gui_pkgs[!vapply(gui_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing missing packages into bundle library:\n  ",
      paste(missing, collapse = ", "), "\n")
  ok <- tryCatch({
    install.packages(missing, lib = lib_dir,
                     repos = "https://cloud.r-project.org")
    TRUE
  }, error = function(e) { cat("Install error:", conditionMessage(e), "\n"); FALSE })
  still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still) > 0) {
    cat("\nFATAL: could not load required packages:", paste(still, collapse = ", "), "\n")
    cat("If this machine has no internet access, rebuild the bundle with\n")
    cat("packages pre-installed (see installer/BUILD.md).\n")
    Sys.sleep(8)
    quit(status = 1)
  }
}

# Run the app. setwd to app/ so main_app.R's relative source()/Input paths
# resolve; main_app.R itself then switches the working dir to a writable
# per-user workspace for all runtime output.
setwd(app_dir)
suppressPackageStartupMessages(library(shiny))
cat("Starting SIACS — your browser will open shortly.\n")
cat("Keep this window open while you use SIACS. Close it to stop the app.\n")

# main_app.R ends with a shinyApp(...) call and was written to run in the
# global environment. Source it there so all its top-level objects resolve as
# they did under RStudio, and capture the returned shiny.appobj for runApp().
# (runApp() has no 'appFile' arg — it takes a directory or an app object.)
app_obj <- source(file.path(app_dir, "main_app.R"), local = FALSE)$value
if (inherits(app_obj, "shiny.appobj")) {
  shiny::runApp(app_obj, launch.browser = TRUE)
} else {
  cat("SIACS app object not returned by main_app.R; if the app did not open,\n")
  cat("check the log files in your SIACS workspace folder.\n")
}
