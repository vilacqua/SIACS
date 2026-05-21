# siacs_diagnostics.R
# ----------------------------------------------------------------------------
# Pre-library, base-R-only diagnostics used by main_app.R and the child
# Rscript spawned for each simulation run.
#
# The goal of this file is to give us SOMETHING to look at when SIACS dies
# silently on a colleague's machine before any per-instance log file is
# created. Every function here uses only base R so it is safe to source
# before library() calls.
#
# Outputs:
#   siacs_startup.log       — written by main_app.R during app launch
#   siacs_child_started.log — written by the child Rscript on entry
#   siacs_child_steps.log   — appended to by the child at each milestone
#
# All three files live in the project working directory. They are overwritten
# on each fresh launch so a colleague can simply zip and send them.
# ----------------------------------------------------------------------------

# Single-line, append-mode logger. Always flushes to disk so a hard crash
# preserves the last line we wrote.
siacs_log_line <- function(file, msg) {
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  line <- paste0("[", ts, "] ", msg)
  con  <- tryCatch(file(file, open = "at"), error = function(e) NULL)
  if (is.null(con)) return(invisible(NULL))
  on.exit(close(con), add = TRUE)
  writeLines(line, con)
  flush(con)
  invisible(line)
}

# Truncate-and-create a fresh log file at app startup.
siacs_log_init <- function(file, header) {
  tryCatch({
    con <- file(file, open = "wt")
    on.exit(close(con), add = TRUE)
    writeLines(c(
      paste0("# ", header),
      paste0("# Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("# R version: ", R.version.string),
      paste0("# Platform: ",  R.version$platform),
      paste0("# OS: ",        Sys.info()[["sysname"]], " ",
                              Sys.info()[["release"]]),
      paste0("# User: ",      Sys.info()[["user"]]),
      paste0("# Working dir: ", getwd()),
      paste0("# .libPaths():"),
      paste0("#   - ", .libPaths()),
      paste0("# R_LIBS_USER: ", Sys.getenv("R_LIBS_USER")),
      paste0("# R_LIBS_SITE: ", Sys.getenv("R_LIBS_SITE")),
      paste0("# R_HOME: ",      Sys.getenv("R_HOME")),
      "# ----------------------------------------------------------------"
    ), con)
    flush(con)
  }, error = function(e) {
    # If we can't even write a log file, we're toast — fall back to stderr.
    message("[siacs_diagnostics] Could not create log file '", file,
            "': ", conditionMessage(e))
  })
  invisible(NULL)
}

# Detect whether the working directory is inside a OneDrive-synced folder.
# Returns a list(in_onedrive = logical, path = character or NA).
siacs_detect_onedrive <- function(path = getwd()) {
  norm <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
                   error = function(e) path)
  # Common OneDrive root markers on Windows: "OneDrive - <Org>", "OneDrive"
  is_od <- grepl("OneDrive", norm, ignore.case = TRUE)
  list(in_onedrive = isTRUE(is_od),
       path        = if (isTRUE(is_od)) norm else NA_character_)
}

# Delete cross-version stale R bytecode (.Rc) files that ship with the
# project. SIACS_Compile_Source.R will rebuild them at runtime via cmpfile().
# This avoids segfaults / silent crashes on machines whose R version differs
# from whoever last committed the .Rc files.
siacs_clean_rc_files <- function(dir = getwd(), log_file = NULL) {
  rc <- list.files(dir, pattern = "\\.Rc$", full.names = TRUE,
                   recursive = FALSE)
  if (length(rc) == 0) {
    if (!is.null(log_file))
      siacs_log_line(log_file, "[clean_rc] no .Rc files to remove")
    return(invisible(character(0)))
  }
  removed <- character(0)
  failed  <- character(0)
  for (f in rc) {
    ok <- tryCatch(file.remove(f), error = function(e) FALSE,
                   warning = function(w) FALSE)
    if (isTRUE(ok)) removed <- c(removed, basename(f))
    else            failed  <- c(failed,  basename(f))
  }
  if (!is.null(log_file)) {
    siacs_log_line(log_file, sprintf(
      "[clean_rc] removed %d .Rc file(s); failed %d",
      length(removed), length(failed)))
    if (length(failed) > 0)
      siacs_log_line(log_file, paste0(
        "[clean_rc] FAILED to remove (likely OneDrive lock or permissions): ",
        paste(failed, collapse = ", ")))
  }
  invisible(removed)
}

# Safe library loader. Tries to load a package; on failure, logs the package
# name and the underlying error message to log_file. Returns TRUE/FALSE.
# Set fail_action = "stop" to halt; default keeps going so the log captures
# every missing package in one pass.
siacs_safe_library <- function(pkg, log_file = NULL,
                               fail_action = c("warn", "stop", "silent"),
                               install_if_missing = FALSE) {
  fail_action <- match.arg(fail_action)
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (!ok && isTRUE(install_if_missing)) {
    if (!is.null(log_file))
      siacs_log_line(log_file, paste0("[lib] '", pkg,
        "' not installed — attempting install.packages()"))
    tryCatch(install.packages(pkg, quiet = TRUE),
             error   = function(e) NULL,
             warning = function(w) NULL)
    ok <- requireNamespace(pkg, quietly = TRUE)
  }
  if (!ok) {
    msg <- paste0("[lib] FAILED to find package '", pkg, "'")
    if (!is.null(log_file)) siacs_log_line(log_file, msg)
    if (fail_action == "stop")
      stop("Required package not available: ", pkg)
    if (fail_action == "warn")
      warning(msg, call. = FALSE)
    return(FALSE)
  }
  loaded <- tryCatch({
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE))
    TRUE
  }, error = function(e) {
    if (!is.null(log_file))
      siacs_log_line(log_file, paste0(
        "[lib] FAILED to load '", pkg, "': ", conditionMessage(e)))
    FALSE
  })
  if (isTRUE(loaded) && !is.null(log_file)) {
    ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                    error = function(e) "?")
    siacs_log_line(log_file, paste0("[lib] OK ", pkg, " ", ver))
  }
  loaded
}

# Convenience: load a vector of packages. Returns names of any that failed.
siacs_load_packages <- function(pkgs, log_file = NULL,
                                install_if_missing = FALSE) {
  failed <- character(0)
  for (p in pkgs) {
    if (!siacs_safe_library(p, log_file = log_file,
                            fail_action = "warn",
                            install_if_missing = install_if_missing))
      failed <- c(failed, p)
  }
  failed
}

# Ensure a set of packages is installed in the user's library WITHOUT loading
# them in the current R session. Used for engine packages that we want
# available when the background simulation Rscript runs, but that we don't
# want polluting the GUI process's namespace (reshape masks reshape2, etc.).
# Returns names of packages that could not be installed.
siacs_ensure_packages_installed <- function(pkgs, log_file = NULL) {
  missing <- pkgs[!vapply(pkgs, requireNamespace,
                          logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(character(0)))
  if (!is.null(log_file))
    siacs_log_line(log_file, paste0(
      "[ensure] missing engine packages: ",
      paste(missing, collapse = ", "),
      " — attempting install.packages()"))
  for (p in missing) {
    tryCatch(install.packages(p, quiet = TRUE),
             error   = function(e) NULL,
             warning = function(w) NULL)
  }
  # Re-check
  still_missing <- missing[!vapply(missing, requireNamespace,
                                    logical(1), quietly = TRUE)]
  if (!is.null(log_file)) {
    installed_now <- setdiff(missing, still_missing)
    if (length(installed_now) > 0)
      siacs_log_line(log_file, paste0(
        "[ensure] installed: ", paste(installed_now, collapse = ", ")))
    if (length(still_missing) > 0)
      siacs_log_line(log_file, paste0(
        "[ensure] FAILED to install: ",
        paste(still_missing, collapse = ", ")))
  }
  invisible(still_missing)
}
