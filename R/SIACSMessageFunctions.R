# Define Functions to produce progress reports and warnings for User
## Version 0922 (September 2022)

PrintComputationTime <- function(MessageT, starting, ending, instance, runs) {
  comptime <- round(difftime(ending, starting, units = "secs"), 3)
  cat(MessageT, instance, " of ", runs, ": ", comptime, " seconds\n")
}

PrintDataLoaded <- function(MessageF, name, coldesignation = "variable(s)", variab, rowdesignation = "observation(s)", obs) {
  cat(MessageF, " Read ", obs, " ", rowdesignation, " of ", variab, " ", coldesignation, " from ", name, "\n")
}

SkipAnalisisMessages <- function(type, Filenames) {
  if (type == 1) {
    commontext <- " cannot be performed without calculating time derivatives. It will be ignored, or you can stop and restart after choosing a file name for time derivatives."
    if (Filenames$OutputSensitivity != "None") message("Sensitivity coefficients analysis", commontext)
    if (Filenames$OutputUncertainty != "None") message("Uncertainty analysis", commontext)
    if (Filenames$OutputMassBalanceComponents != "None") message("Mass balance analysis", commontext)
  }
  if (type == 2) {
    commontext <- " cannot be performed without calculating dydx derivatives. It will be ignored, or you can stop and restart after choosing a file name for sensitivity coefficients."
    if (Filenames$OutputUncertainty != "None") message("Uncertainty analysis", commontext)
  }
}

MissingInputs <- function(type) {
  if (type == 1) {
    message("No such file found")
  }
}
