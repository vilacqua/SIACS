## ---------------------------------------------------------------------
## chemical species and mechanism information
## Version for SIACS - 0924


## -------some static parameters--------------------------------------------------
SIACS.env <- new.env()

SIACS.env$R <- 8.3144598 # ideal gas constant in Pa m3/(K mol)
SIACS.env$Avogadro <- 6.02214E+23 # Avogadro's number; molecules per mole
SIACS.env$O2Fraction <- 0.2095 # Oxygen in the atmosphere at 20.95%
sigdig <- 7 # significant digits for roundings. This means 1.x ppm concentrations rounded
# to ppt and mg/m3 rounded to ng
nphys <- 9 # number of physical variables

## ------------Miscellanea---------------------------------------------------------

ODEFile <- "SIACSmodelODEs.R"
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)
# miscellanea (to avoid "reached elapsed time limit" warning)

## ------------Mechanisms---------------------------------------------------------
source("SAPRC99.R") # functions and variables defining the chemical mechanism based on SAPRC 99
source("SAPRC07T.R") # functions and variables defining the chemical mechanism based on SAPRC 07T !!! currently just a copy of SAPRC99 !!!


chemical.mechanism.selection <- function(mechanism) {
  if (mechanism == "SAPRC99") {
    # Chemical Mechanism variables
    nrxn <- 211 # number of photochemical reactions
    species.df <- read.csv("./spcSAPRC99.csv", stringsAsFactors = FALSE, comment.char = "#") # spcname, mw (g/mol), physical phase
    spcnames <- as.character(species.df$spcname)
    nspc <- length(spcnames)
    ngas <- sum(species.df$gas)
    naer <- nspc - ngas

    # Chemical Mechanism functions
    Reaction.Rates <- Reaction.Rates.SAPRC99
    Reactions.Sum <- Reactions.Sum.SAPRC99
    ReturnRCT <- ReturnRCT.SAPRC99
    LU_IROW <- LU_IROW.SPRC99
    LU_ICOL <- LU_ICOL.SPRC99
    Jacobian.reaction.components <- Jacobian.reaction.components.SAPRC99
    Jacobian.Terms <- Jacobian.Terms.SAPRC99
  } # if SAPRC99

  else if (mechanism == "SAPRC07T") {
    # Chemical Mechanism variables
    nrxn <- 211 # number of photochemical reactions
    species.df <- read.csv("./spcSAPRC07T.csv", stringsAsFactors = FALSE, comment.char = "#") # spcname, mw (g/mol), physical phase
    spcnames <- as.character(species.df$spcname)
    nspc <- length(spcnames)
    ngas <- sum(species.df$gas)
    naer <- nspc - ngas

    # Chemical Mechanism functions
    Reaction.Rates <- Reaction.Rates.SAPRC07T
    Reactions.Sum <- Reactions.Sum.SAPRC07T
    ReturnRCT <- ReturnRCT.SAPRC07T
    LU_IROW <- LU_IROW.SPRC07T
    LU_ICOL <- LU_ICOL.SPRC07T
    Jacobian.reaction.components <- Jacobian.reaction.components.SAPRC07T
    Jacobian.Terms <- Jacobian.Terms.SAPRC07T
  } # else if SAPRCX

  else {
    on.exit(message("Chemical mechanism ", mechanism, " not defined in ", SIACSVersion))
    stop()
  }

  # Prepares variables needed for solving ODE based on mechanism selected
  RCT <- rep(0.0, nrxn) # instantaneous reaction rate constants (units depend on order of reactions)
  AR <- rep(0.0, nrxn) # instantaneous reaction rates (molecules cm-3 s-1)

  Emissions <- rep(0.0, nspc)
  names(Emissions) <- paste0(spcnames, ".S") # Adds an .S to distinguish variable names for emissions from those for indoor and outdoor concentrations
  isgas <- species.df$gas # 1 if gas; 0 if aerosol
  OutdoorConcs <- rep(0.0, nspc)
  names(OutdoorConcs) <- paste0(spcnames, ".O") # Adds an .O to distinguish variable names for outdoor concentrations from those for emissions and indoor concentrations

  aP <- rep(0.0, nspc) # penetration-adjusted infiltration rate s-1


  cms <- list(
    nrxn,
    species.df,
    spcnames,
    nspc,
    ngas,
    naer,
    Reaction.Rates,
    Reactions.Sum,
    ReturnRCT,
    LU_IROW,
    LU_ICOL,
    Jacobian.reaction.components,
    Jacobian.Terms,
    isgas
  )
  return(cms)
} # chemical.mechanism.selection
