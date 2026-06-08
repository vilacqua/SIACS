# Compiles Scripts necessary to run SIACS batch script

library(compiler)
enableJIT(0)

# -----------------------------------------------------------------------
# WRITABLE BYTECODE CACHE
# -----------------------------------------------------------------------
# cmpfile() writes a compiled ".Rc" file next to each source script by
# default. When SIACS is installed in a protected location (Program Files,
# or any folder guarded by Windows "Controlled Folder Access" — which covers
# Documents/Desktop even on the C: drive), writing those .Rc files into the
# install directory triggers a Windows administrator-approval prompt and can
# fail outright.
#
# To avoid this, we compile bytecode into a per-user, per-R-version cache
# directory that is always writable and never inside the install tree:
#   - tools::R_user_dir("SIACS", "cache")/bytecode-<Rversion>/   (R >= 4.0)
#   - falls back to a tempdir() location if R_user_dir is unavailable.
# Namespacing by R version prevents loading bytecode compiled by a different
# R version (which is not portable and can crash).
#
# All three helpers below resolve the SAME cache path, so the process that
# compiles (SIACS.batch in the main child process) and the worker processes
# that loadcmp() agree on where the .Rc files live.
siacs_bytecode_dir <- function() {
  base <- tryCatch(
    tools::R_user_dir("SIACS", "cache"),
    error = function(e) file.path(tempdir(), "SIACS-cache"))
  dir <- file.path(base, paste0("bytecode-", getRversion()))
  if (!dir.exists(dir)) {
    ok <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    # Last-resort fallback if even R_user_dir is not writable.
    if (!ok && !dir.exists(dir)) {
      dir <- file.path(tempdir(), paste0("SIACS-bytecode-", getRversion()))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  dir
}

# Compiled-file path for a given source script, inside the writable cache.
siacs_bytecode_path <- function(script, cache = siacs_bytecode_dir()) {
  file.path(cache, paste0(script, "c"))
}

.siacs_supporting_scripts <- c(
  "SIACSMessageFunctions.R", "SIACSMechanismInfo.R", "SIACSRateConstantsFncs.R",
  "SIACSFunctions.R", "SIACSInterpolationFunctions.R", "SIACSPreProcessSources.R",
  "SIACSCalcDerivatives.R", "SIACSBasicPlots.R", "SIACSPostProcessing.R",
  "SIACSLights.R", "SIACSmodelODEs.R", "SIACS_main_function.R")

compile_and_source_supporting_scripts <- function() {
  cache <- siacs_bytecode_dir()
  for (script in .siacs_supporting_scripts) {
    outfile <- siacs_bytecode_path(script, cache)
    cmpfile(script, outfile)
    loadcmp(outfile)
  }
}

compile_supporting_scripts <- function() {
  cache <- siacs_bytecode_dir()
  for (script in .siacs_supporting_scripts) {
    cmpfile(script, siacs_bytecode_path(script, cache))
  }
}

source_supporting_scripts <- function() {
  cache <- siacs_bytecode_dir()
  for (script in .siacs_supporting_scripts) {
    outfile <- siacs_bytecode_path(script, cache)
    # If the compiled file is missing (e.g. a worker started before the main
    # process compiled, or the cache was cleared), compile it on demand.
    if (!file.exists(outfile)) cmpfile(script, outfile)
    loadcmp(outfile)
  }
}
