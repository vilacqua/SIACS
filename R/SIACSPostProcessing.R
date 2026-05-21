# Functions for formatting and analyzing outputs of the model runs
## Version 0924 (September 2024)

## ----------------------Save results-----------------------------------

Save.results <- function(out, DataFiles, instance = 1, chemistry, mechanism, parms) {
  SIACSVersion <- parms$SV

  ## save numerical output to file
  u <- Units.header(out, parms) # creates a header with units for file to be saved
  outfile <- paste0(DataFiles$OutputTable, instance, ".csv") #
  # Creates a file header listing the input files
  sink(outfile)
  cat("#Results produced with ", SIACSVersion, " using ", mechanism, " mechanism and chemistry = ", chemistry, "\n")
  cat("#Box data: ", DataFiles$BoxData, ";   Time data: ", DataFiles$Time, ";    Deposition and Penetration data: ", DataFiles$DepositionV, "\n")
  cat("#Physical Environment: ", DataFiles$PhysicalEnvironment, ";    Ambient concentrations: ", DataFiles$OutdoorConcentrations, "\n")
  cat("#Emission Profiles: ", DataFiles$EmissionProfiles, ";    Activities data: ", DataFiles$Activities, "\n")
  cat("#Indoor Light: ", DataFiles$IndoorLight, "\n")
  sink()
  # Writes data table
  withCallingHandlers(
    { # This is to avoid warning that it is appending column names to a file
      write.table(t(u), file = outfile, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE) # transposed header with units first
      write.table(out, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE, append = TRUE)
    },
    warning = function(w) {
      if (grep("appending column", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  cat("Results written to ", outfile, "\n")
} # Save.results <- function()

## ----------------------Create Plots-----------------------------------------------
##
Create.result.plots <- function(out, outfile, maxTime, parms) {
  cat("Working on results plots...")
  speciestoplot <- c("HCHO", "RCHO", "ALK", "ARO", "NO2", "NO", "NO3", "HNO3", "NH3", "RO2", "OH", "O3", "CO2", "CO", "PM25_10", "PM25")
  Multi.results.plot(out, speciestoplot, maxTime, parms)

  # Saves plot to file
  # outfile <- paste0(DataFiles$OutputBasicChart[DataFiles$Instance == instance],instance,".png") #
  ggsave(outfile, device = "png", width = 11, height = 8.5, units = "in")
  cat("Results plots saved to ", outfile, "\n")
}

## --------------Optional Analyses--------------------------------------


Deptime.derivative.do <- function(outsaved, outfile, parms, gvs) {
  ## Calculates derivatives and saves dy/dt to file
  dydtMatrix <- Calc.dydt(outsaved, parms, gvs)
  # outfile <- paste0(DataFiles$OutputTimeDerivatives[DataFiles$Instance == instance], instance,".csv") #shc
  write.table(dydtMatrix, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE) # shc
  cat(" Time derivatives of results variables written to ", outfile, "\n")
  return(dydtMatrix)
}

Mass.balance.do <- function(out, outfile, V, dydtMatrix, B, dv.df, Ef, gvs) {
  # Performs analysis separating mass balance components and saves it to file
  nspc <- gvs$nspc
  spcnames <- gvs$sn


  mb <- MassBalance.analysis(out, V, B, dydtMatrix, dv.df, Ef, gvs)
  # outfile <- paste0(DataFiles$OutputMassBalanceComponents[DataFiles$Instance == instance], instance,".xlsx")
  cat("Saving Mass Balance components...\n")
  wb <- createWorkbook() # create an empty workbook to save data to
  for (i in 1:nspc) {
    addWorksheet(wb, sheetName = spcnames[i])
    writeData(wb, sheet = spcnames[i], x = mb[, , i])
  }

  saveWorkbook(wb, outfile, overwrite = TRUE)
  cat("Mass balance components written to ", outfile, "\n")
  return(mb)
}

Dydx.do <- function(out, outfile, time.read, dydtMatrix, gvs) {
  # Calculates additional derivatives for sensitivity and uncertainty analyses
  # Performs sensitivity analysis and saves to file

  SIACSVersion <- gvs$SV

  dxdtMatrix <- Calc.dxdt(out, time.read, dydtMatrix, gvs) # time derivatives of independent variables
  dydxMatrix <- Calc.dydx(dydtMatrix, dxdtMatrix, gvs) # partial derivatives of dependent variables with respect to independent variables
  cat("Calculating sensitivity coefficients...\n")
  sensit <- sensitivity.coefficients(dydxMatrix, gvs) # a kind of error sensitivity analysis based on partial derivatives

  # outfile <- paste0(DataFiles$OutputSensitivity[DataFiles$Instance == instance], instance,".csv")
  write.csv(sensit, file = outfile, row.names = TRUE)
  cat("Sensitivity coefficients written to", outfile, "\n")
  message("The analysis of sensitivity coefficients is stil under development in ", SIACSVersion, " and results have limited value\n")
  derivatives <- list(dxdt = dxdtMatrix, dydx = dydxMatrix, sensitivity = sensit)
  return(derivatives)
}

Uncertainty.do <- function(out, outfile, dydxMatrix, Unc, gvs) {
  # Performs uncertainty analysis based on error propagation and saves to file

  O.uncert <- Unc$ounc
  S.uncert <- Unc$sunc
  E.uncert <- Unc$eunc
  SIACSVersion <- gvs$SV

  uncert.res <- Uncertainty.Propagation(O.uncert, S.uncert, E.uncert, dydxMatrix, gvs)

  # outfile <- paste0(DataFiles$OutputUncertainty[DataFiles$Instance == instance], instance,".csv")
  write.table(uncert.res, file = outfile, sep = ",", col.names = TRUE, row.names = FALSE)
  cat("Uncertainty analysis based on error propagation written to", outfile, "\n")
  message("This uncertainty analysis is stil under development in ", SIACSVersion, " and results have limited value\n")
  return(uncert.res)
}
