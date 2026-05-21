## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE   # set FALSE so examples don't run during CRAN check
)


## ----install------------------------------------------------------------------
# From local source:
install.packages("path/to/SIACS_0.2.0.tar.gz", repos = NULL, type = "source")

# Load the package:
library(SIACS)


## ----gui----------------------------------------------------------------------
library(SIACS)
runSIACSApp()


## ----batch-inputs-------------------------------------------------------------
library(SIACS)
library(readxl)

input_dir <- system.file("extdata", "Input", package = "SIACS")

# Read example inputs for one instance:
time_data   <- read.csv(file.path(input_dir, "Time.csv"))
box_data    <- read.csv(file.path(input_dir, "BoxData - Atlanta.csv"))
phys_env    <- read.csv(file.path(input_dir,
                 "PhysicalEnvironmentData_CMAQ Atlanta_48hrs.csv"))
outdoor     <- read.csv(file.path(input_dir,
                 "Outdoor Concentrations - Atlanta July CMAQ.csv"))
emissions   <- read.csv(file.path(input_dir, "EmissionProfiles.csv"))
activities  <- read.csv(file.path(input_dir, "Activities.csv"))
deposition  <- read.csv(file.path(input_dir, "Vd&P-Carslaw 2012.csv"))
initial_val <- read.csv(file.path(input_dir,
                 "InitialIndoorConcentrations.csv"))
indoor_light <- read_excel(file.path(input_dir, "IndoorLightAtlanta.xlsx"),
                            sheet = "J_values")


## ----batch-assemble-----------------------------------------------------------
input_data_list <- list(
  Time                 = list(time_data),
  BoxData              = list(box_data),
  PhysicalEnvironment  = list(phys_env),
  OutdoorConcentrations = list(outdoor),
  EmissionProfiles     = list(emissions),
  Activities           = list(activities),
  DepositionV          = list(deposition),
  InitialValues        = list(initial_val),
  IndoorLight          = list(
    list(J_values    = as.data.frame(indoor_light),
         EnergyFlux  = data.frame(Time = ..., EnergyFlux = ...))
  )
)


## ----batch-outputs------------------------------------------------------------
output_list <- list(
  list(
    OutputTable                  = "./Output/Results",
    OutputBasicChart             = "./Output/Plot",
    OutputTimeDerivatives        = "./Output/Derivatives",
    OutputMassBalanceComponents  = "./Output/MassBalance",
    OutputSensitivity            = "None",
    OutputUncertainty            = "None"
  )
)


## ----batch-run----------------------------------------------------------------
results <- SIACS.batch(
  input_data_list = input_data_list,
  instances       = 1,
  OutputList      = output_list,
  mechanism       = "SAPRC99",
  chemistry       = TRUE,
  use_parallel    = FALSE   # use TRUE for multiple instances
)


## ----batch-results------------------------------------------------------------
# Main output data frame:
out <- results[[1]]$Simulation1$alldata

# View key species over time:
head(out[, c("time", "O3", "NO2", "HCHO", "OH", "PM25")])

# Time is in minutes; concentrations in ppm (gases) or µg/m³ (PM)


## ----citation-----------------------------------------------------------------
citation("SIACS")

