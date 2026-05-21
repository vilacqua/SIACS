# Compiles Scripts necessary to run SIACS batch script

library(compiler)
enableJIT(0)

compile_and_source_supporting_scripts <- function() {
  scripts_to_source <- c("SIACSMessageFunctions.R", "SIACSMechanismInfo.R", "SIACSRateConstantsFncs.R", "SIACSFunctions.R", "SIACSInterpolationFunctions.R", "SIACSPreProcessSources.R", "SIACSCalcDerivatives.R", "SIACSBasicPlots.R", "SIACSPostProcessing.R", "SIACSLights.R", "SIACSmodelODEs.R", "SIACS_main_function.R")

  for (script in scripts_to_source) {
    cmpfile(script)
    loadcmp(paste0(script, "c"))
  }
}

compile_supporting_scripts <- function() {
  scripts_to_source <- c("SIACSMessageFunctions.R", "SIACSMechanismInfo.R", "SIACSRateConstantsFncs.R", "SIACSFunctions.R", "SIACSInterpolationFunctions.R", "SIACSPreProcessSources.R", "SIACSCalcDerivatives.R", "SIACSBasicPlots.R", "SIACSPostProcessing.R", "SIACSLights.R", "SIACSmodelODEs.R", "SIACS_main_function.R")

  for (script in scripts_to_source) {
    cmpfile(script)
  }
}

source_supporting_scripts <- function() {
  scripts_to_source <- c("SIACSMessageFunctions.R", "SIACSMechanismInfo.R", "SIACSRateConstantsFncs.R", "SIACSFunctions.R", "SIACSInterpolationFunctions.R", "SIACSPreProcessSources.R", "SIACSCalcDerivatives.R", "SIACSBasicPlots.R", "SIACSPostProcessing.R", "SIACSLights.R", "SIACSmodelODEs.R", "SIACS_main_function.R")

  for (script in scripts_to_source) {
    loadcmp(paste0(script, "c"))
  }
}
