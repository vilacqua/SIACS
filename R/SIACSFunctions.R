## Version 0924
# Functions for SIACS 0924


## --------Ventilation Functions-------------------------------------------------------------
# (Based on ASHRAE Fundamentals)
# Total ventilation as sum of ventilation components. Varying penetration factor P for inflow vs. outflow calculations
# all ventilation inputs in homogeneous [volume]/[time] units, except P (non-dimensional)
Q.tot <- function(qinf, P = 1, IFE = 0, qbal = 0, qunbal = 0, qnat = 0) {
  result <- qbal * (1 - IFE) + sqrt(qunbal^2 + (P * qinf)^2 + qnat^2)
  # result <- qbal + sqrt(qunbal^2+(P*qinf)^2+qnat^2)
}
# Ventilation from infiltration
# infiltration surface area in m2; stack coefficient in (L/s)^2/(cm^4 K); T indoor and T ouutdoor in K
# wind coefficient in (L/s)^2/(cm^4 (m/s)^2); wind in m/s; output in m3/s
Q.inf <- function(infsurf, stackcoeff, Ti, To, windcoeff, wind) {
  result <- infsurf * 1E4 * sqrt(stackcoeff * abs(Ti - To) + windcoeff * wind^2) / 1E3
}
# Natural ventilation
# Window area in m2; Cd and Cv non-dimensional; g in m/s2; midpoint height
# and neutral pressure level (NPL) in m; Ti and To in K; wind speed in m/s; output in m3/s
Q.nat <- function(windowarea, Cd, g, midpoint, NPL, Ti, To, Cv, wind) {
  stackeffect <- Cd * windowarea * 0.5 * sqrt(2 * g * abs(midpoint - NPL) * abs(Ti - To) / max(Ti, To))
  windeffect <- Cv * windowarea * 0.5 * wind
  result <- sqrt(stackeffect^2 + windeffect^2)
}

#--------------Empirical Functions---------------------------------------------------------
# Defines functions for empirical physical-chemical relationships
WaterVaporMassConc <- function(RH, Ti, BP) {
  vaporconc <- RH * (5.018 + 0.32321 * (Ti - 273.15) + 8.1847E-3 * (Ti - 273.15)^2 + 3.1243E-4 * (Ti - 273.15)^3) / 18 * SIACS.env$Avogadro * 1e-6
  # in molecules/cm3
  # Source of equation: (CRC Handbook of Chemistry and Physics, 1963. 44th Ed., p. 2544) as reported in multiple other papers
  # Verified against results of psychometric formulas in ASHRAE Fundamentals
  # adjusting to account for barometric pressure? Vapor pressure does not change (?), but mass of air does
}

# Defines ozone reaction with human skin lipids
ozone.skin.deposition <- function(dv, n.people, reactive.area, A) {
  skin.dv <- 0.4167 # cm/s
  skin.area <- n.people * reactive.area
  dv <- (A * dv + skin.area * skin.dv) / (A + skin.area)
  # returns the modified deposition velocity in the same units (cm/s) as the base one, weighted by the relative areas of skin and other surfaces
  return(dv)
  # Source of skin deposition velocity for ozone: Weschler, C. J., & Nazaroff, W. W. (2023). Human skin oil: a major ozone reactant indoors. Environmental Science: Atmospheres, 3(4), 640-661
  # a value of 15 m/h was used in the example, about the average of various studies; This corresponds to 0.4167 cm/s. Note stdv in other studies as high as +- 10 m/s
}

#---------------Initial values functions-------------------------------------
# Functions to load from data or estimate initial indoor concentrations

InitialValues <- function(InitialVals, Ambient, Source, start, V, AtoV, Ef, DvelAppFncs.lst, Nspc = nspc, Spcnames = spcnames, Ngas = ngas, PEAFl = PhysEnvAppFncs.lst, Env = E,
                          sdf = species.df, aal = aP.app.lst) {
  nspc <- Nspc
  spcnames <- Spcnames
  ngas <- Ngas
  PhysEnvAppFncs.lst <- PEAFl
  E <- Env
  species.df <- sdf
  aP.app.lst <- aal

  InitialV <- Ambient[, -1]
  InitialSource <- Source[, -1]
  V <- V
  if (is.null(InitialVals)) {
    InitialV <- ApproxInitialValue(0, InitialV, InitialSource, start, V, AtoV, Ef, DvelAppFncs.lst, Nspc = nspc, Ngas = ngas, Env = E, PEAFl = PhysEnvAppFncs.lst, sdf = species.df, aal = aP.app.lst) # approximates equilibrium concentrations w/o chemistry & emissions
    InitialV[is.na(InitialV) == TRUE] <- 0 # sets to zero source terms that have NA because they don't have a molecular weight
  } else {
    tmp.df <- ChemSpeciesStandardize(InitialVals, withtime = FALSE, default = 0, Nspc = nspc, Spcnames = spcnames) # standardizes order and assigns 0 where no input was provided

    for (i in 1:nspc) { # if no initial value was supplied for a species, use equilibrium approximation that ignores chemistry
      if (is.na(tmp.df[i])) {
        InitialV[i] <- ApproxInitialValue(i, InitialV, InitialSource, start, V, AtoV, Ef, DvelAppFncs.lst, Nspc = nspc, Ngas = ngas, Env = E, PEAFl = PhysEnvAppFncs.lst, sdf = species.df, aal = aP.app.lst)
      } else {
        InitialV[i] <- tmp.df[i]
      }
    }
  }
  n <- seq(from = 1, to = ngas, by = 1)
  InitialV[n] <- InitialV[n] * PhysEnvAppFncs.lst$Mair.in(start * 60.0) * 1e-6
  # adjust units for gas species to molecules cm-3 and leave unchanged for particles

  return(unlist(InitialV))
} # InitialValues <- function



ApproxInitialValue <- function(n, AmbientIni, SourceIni, t0, V, AtoV, Ef, DvelAppFncs.lst, Nspc = nspc, Ngas = ngas, Env = E, PEAFl = PhysEnvAppFncs.lst, sdf = species.df, aal = aP.app.lst) {
  nspc <- Nspc
  ngas <- Ngas
  E <- Env
  PhysEnvAppFncs.lst <- PEAFl
  species.df <- sdf
  aP.app.lst <- aal


  if (n == 0) spseq <- seq(from = 1, to = nspc, by = 1) else spseq <- n


  pe <- physicalinterpolation(t0 * 60, full = TRUE, E, PhysEnvAppFncs.lst) ## approximate values of physical environment variables at time t (t [=] minutes)
  # air exchange rate at initial time (in s-1)
  # filter air flow at initial time (in m3/s)
  aP <- lapply(aP.app.lst, function(f) f(t0 * 60)) # air exchange rate adjust for penetration factor and intake filter effect (s-1)
  dvel <- unlist(lapply(DvelAppFncs.lst, function(f) f(t0 * 60)))
  S0 <- SourceIni / 60 # g/min to g/s (or 1/s for UFP)
  S0[spseq <= ngas] <- S0[spseq <= ngas] / species.df$mw[spseq <= ngas] * SIACS.env$R * (pe$Ti) / pe$BP * 1e6 # g/s to ppm m3/s for gases
  S0[(ngas + 1):(ngas + 2)] <- S0[(ngas + 1):(ngas + 2)] * 1e6 # from g/s to ug/s for particle mass
  # S0 for particle number (spseq(ngas+3) remains unchanged in s-1)

  Initial <- unlist((AmbientIni[spseq] * aP[spseq] + S0[spseq] / V) / (pe$a + dvel[spseq] * AtoV + Ef[spseq] * pe$QFilter / V))
  # Approximates unknown or unspecified initial values with concetration in equilibrium with ambient air, sources, filtration, but without chemistry
  return(Initial)
} # ApproxInitialValue <- function

#---------------------Re-ordering inputs, extracting and adding missing values, adjusting units------------------

# This is used when user inputs for chemical species are in random order and may be
# an incomplete set e.g. Outdoor Concentrations, Indoor Emissions, Initial values
# Take a matrix with some species in random order and create one with all species in standard order
ChemSpeciesStandardize <- function(x, withtime = FALSE, default = 0, Nspc = nspc, Spcnames = spcnames) {
  rowsn <- nrow(x)
  reorderedx <- data.frame(matrix(data = default, rowsn, ncol = Nspc))
  # Note: variables not part of inputs will remain set to 0
  names(reorderedx) <- Spcnames
  if (withtime) reorderedx <- cbind(Time = rep(0, rowsn), reorderedx)
  varnames <- names(x)
  standardnames <- names(reorderedx)

  for (i in 1:ncol(x)) { # Assigns available inputs to standard matrix
    ismatch <- match(varnames[i], standardnames)
    if (is.na(ismatch)) {
      warning(paste0("Species ", varnames[i], " not found in ", SIACSVersion, " chemical model"))
      next
    }
    reorderedx[, ismatch] <- x[, i]
  }
  return(reorderedx)
} # ChemSpeciesReOrder <- function(x)


# This function removes uncertainty data from 1st line of data, updates variables storing uncertainty information and standardizes them
ExtractUncertainty <- function(x, whichdata, NSPC = nspc, SPCNAMES = spcnames) {
  uncertt <- x[1, -1] # First row of data matrix, without Time column
  x <- x[-1, ] # Removes first row from data matrix
  if (whichdata == "Physical") {
    # uncertainty is expressed in absolute terms for physical quantities
    uncert <- cbind(uncertt, a = 0) # to be calculated later from uncertainty of Q, V, Ts, Wind
    # more processing needed for uncertainty in a and aPs; reframe vector to have shape of p in ODE solver
  }
  if (whichdata == "Outdoor") {
    uncertt <- uncertt * apply(x[, -1], 2, mean, na.rm = TRUE) # converts uncertainty from relative to absolute terms. Time column is ignored
    uncert <- ChemSpeciesStandardize(uncertt, Nspc = NSPC, Spcnames = SPCNAMES)
  }
  if (whichdata == "Emissions") {
    # absolute uncertainty must be calculated later, as x is not the matrix of values over time
    uncert <- ChemSpeciesStandardize(uncertt, Nspc = NSPC, Spcnames = SPCNAMES)
  }
  # if (whichdata == "Building") { ## For later use
  return(list(x, uncert))
}

Adjust.output <- function(output, V, LightEnergyAppFnc, Spcnames = spcnames, sdf = species.df) {
  ## convert indoor concentrations from molecules cm-3 to ppm and emissions to g/min
  ## (outdoor concentrations were already in units of pppm.)

  spcnames <- Spcnames
  species.df <- sdf

  output <- as.data.frame(output)
  for (spcname in spcnames) {
    ispc <- which(spcnames == spcname)
    iflag <- species.df$gas[ispc]
    sname <- paste0(spcname, ".S")
    if (iflag == 1) {
      output[, spcname] <- output[, spcname] / output[, "Mair.in"] * 1e6
      output[, sname] <- output[, sname] * species.df$mw[ispc] / (SIACS.env$Avogadro / (60.0 * V) * 1e-6)
    } else {
      (output[, sname] <- output[, sname] * 60 * V / 1e6)
    }
  } # for ( spcname in spcnames )

  LightFlux <- LightEnergyAppFnc(output[, "time"])
  output <- data.frame(append(output, list(LightFlux = LightFlux), after = match("a", names(output))))
  # adds the interpolated Light energy flux after the air exchange column ("a")

  output[, "time"] <- output[, "time"] / 60.0 # convert units for time from seconds to minutes
  output[, "a"] <- output[, "a"] * 3600.0 # convert units for air exchange from 1/second to 1/hour
  output[, "To"] <- output[, "To"] - 273.15 # convert temperature from K to Celsius
  output[, "Ti"] <- output[, "Ti"] - 273.15

  return(output)
} # Adjust.output <- function(output)

Units.header <- function(results, parms) {
  ngas <- parms$ngas
  nspc <- parms$nspc
  naer <- parms$naer
  # Creates a header with units for each column for saving results file

  m.unit <- rep("?", ncol(results))
  m.unit[1] <- "min"
  m.unit[2:(ngas + 1)] <- m.unit[(nspc + 2):(ngas + 1 + nspc)] <- "ppm" # indoor and outdoor concentrations for gases
  m.unit[(ngas + 2):(ngas + naer)] <- m.unit[(ngas + 2 + nspc):(ngas + naer + nspc)] <- "ug/m3" # indoor and outdoor concentration for particle mass terms
  m.unit[nspc + 1] <- m.unit[2 * nspc + 1] <- "cm-3" # indoor and outdoor concentration for particle numbers
  m.unit[(2 * nspc + 2):(ngas + 1 + 2 * nspc)] <- m.unit[(ngas + 2 + 2 * nspc):(ngas + naer + 2 * nspc)] <- "g/min" # all source strengths, expect particle number
  m.unit[3 * nspc + 1] <- "1/min" # source strength for particle number


  m.unit[(3 * nspc + 2):(3 * nspc + 3)] <- "⁰C"
  m.unit[3 * nspc + 4] <- "1/hr"
  m.unit[3 * nspc + 5] <- "mW/cm2"
  m.unit[3 * nspc + 6] <- "%"
  m.unit[3 * nspc + 7] <- "Pa"
  m.unit[3 * nspc + 8] <- "molec/cm3"
  m.unit[3 * nspc + 9] <- "m3/s" # units for physical constants
  m.unit[(3 * nspc + 10):(ncol(results))] <- "ppm" # indoor concentrations for additional grouped (gas) species

  return(m.unit)
} # Units.header

combine.species <- function(out) {
  ## add diagnostic variables to output matrix

  ALK <- out[, "ALK1"] + out[, "ALK2"] + out[, "ALK3"] + out[, "ALK4"] + out[, "ALK5"]
  ALK.O <- out[, "ALK1.O"] + out[, "ALK2.O"] + out[, "ALK3.O"] + out[, "ALK4.O"] + out[, "ALK5.O"]
  ARO <- out[, "ARO1"] + out[, "ARO2"]
  ARO.O <- out[, "ARO1.O"] + out[, "ARO2.O"]
  OLE <- out[, "OLE1"] + out[, "OLE2"]
  OLE.O <- out[, "OLE1.O"] + out[, "OLE2.O"]
  NOy <- out[, "NO"] + out[, "NO2"] + out[, "NO3"] + out[, "HNO3"] + out[, "PAN"] +
    out[, "PAN2"] + out[, "MA_PAN"] + out[, "PBZN"] + out[, "HONO"] + 2 * out[, "N2O5"] +
    out[, "HNO4"] + out[, "RNO3"]
  NOy.O <- out[, "NO.O"] + out[, "NO2.O"] + out[, "NO3.O"] + out[, "HNO3.O"] + out[, "PAN.O"] +
    out[, "PAN2.O"] + out[, "MA_PAN.O"] + out[, "PBZN.O"] + out[, "HONO.O"] + 2 * out[, "N2O5.O"] +
    out[, "HNO4.O"] + out[, "RNO3.O"]
  RO2 <- out[, "RO2_R"] + out[, "RO2_N"]
  RO2.O <- out[, "RO2_R.O"] + out[, "RO2_N.O"]
  HOx <- out[, "OH"] + out[, "HO2"] + RO2
  HOx.O <- out[, "OH.O"] + out[, "HO2.O"] + RO2.O

  combined.variables <- cbind(ALK, ARO, OLE, NOy, RO2, HOx, ALK.O, ARO.O, OLE.O, NOy.O, RO2.O, HOx.O)
  out <- cbind(out, combined.variables)
  added <- ncol(combined.variables) # how many of these have been added to the output
  combined.results <- list(main = out, n = added)

  return(combined.results)
} # combine.species <- function()

variable.dep_vel <- function(dv, actv, A) { # creates a time-varying matrix of deposition velocities from their baseline values
  # currently only adjusting Ozone deposition velocity for people present; other mechanisms and other species may be added in the future

  tdv <- t(dv[, 1:2])
  vdv <- as.data.frame(matrix(0, ncol = ncol(tdv) + 1, nrow = nrow(actv)))
  colnames(vdv) <- c("Time", tdv[1, ])
  vdv[, 1] <- actv[, 1] # the time in this matrix is the same as supplied for Activities
  vdv[, -1] <- rep(as.numeric(tdv[2, ]), each = nrow(vdv)) # this matrix has constant deposition velocities over time

  # checks that columns "Adult" and "Child" exist
  if ("Adult" %in% colnames(actv)) {
    vdv$O3 <- ozone.skin.deposition(vdv$O3, actv$Adult, rep(1.8, nrow(actv)), A) # 1.8 m2 of reactive surface per adult
  }
  if ("Child" %in% colnames(actv)) {
    vdv$O3 <- ozone.skin.deposition(vdv$O3, actv$Child, rep(1, nrow(actv)), A) # 1 m2 of reactive surface per child
  }
  # if neither exists returns deposition velocities that are constant
  return(vdv)
}


#-------------------Uncertainty functions----------------------------------------
# Functions used to estimate uncertainty in results from uncertainty in input data

Uncertainty.Propagation <- function(Out.unc, Source.unc, Phys.unc, dydxMatrix, gvs) {
  nspc <- gvs$nspc
  spcnames <- gvs$sn

  Input.unc <- as.data.frame(c(Out.unc, Source.unc, Phys.unc)) # uncertainties as supplied with inputs
  indep.n <- ncol(dydxMatrix) # partial derivatives of dependent variables with respect to input variables, excluding those that are always zero
  indep.names <- colnames(dydxMatrix)
  InUse.unc <- unlist(rep(0.0, indep.n)) # vector of uncertainties in use, i.e. associated with derivatives that are not always zero

  dep.n <- nspc
  timepoints <- as.numeric(row.names(dydxMatrix))
  Uncertainty <- data.frame(matrix(data = NA, nrow = length(timepoints), ncol = (dep.n + 1)))
  names(Uncertainty) <- c("time", spcnames)
  Uncertainty[, 1] <- timepoints
  for (i in 1:indep.n) { # Constructs a vector of uncertainties with only variables in use (i.e. dy/dx<>0 at least somewhere)
    ix <- match(indep.names[i], names(Input.unc))
    InUse.unc[i] <- Input.unc[ix]
  }

  names(InUse.unc) <- indep.names
  InUse.unc <- unlist(InUse.unc)
  for (i in 1:dep.n) { # for each dependent variable, calculates uncertainty at each point in time
    # using variance fomula s(f) = sqrt((s(x1) *dy/dx1)^2 + (s(x2) *dy/dx2)^2+...)
    dydxuncert.prod <- t(t(dydxMatrix[, , i]) * InUse.unc) # double transpose to have each row of the matrix be the product err(xi)* dy/dxi
    dydxuncert.prod <- dydxuncert.prod^2
    variab.uncert <- apply(dydxuncert.prod, 1, sum)
    Uncertainty[, i + 1] <- sqrt(variab.uncert)
  }

  return(Uncertainty)
} # Uncertainty.Propagation <- function(Out.unc, Source.unc, Phys.unc)

sensitivity.coefficients <- function(dydxM, gvs) {
  # Calculates sensitivity coefficients based on the partial derivatives dydx
  # browser()
  nspc <- gvs$nspc
  spcnames <- gvs$sn

  sensitivity <- data.frame(matrix(data = NA, nrow = nspc * 3, ncol = ncol(dydxM)))
  colnames(sensitivity) <- colnames(dydxM)
  rnames <- NULL
  # sensitivity matrix has dependent variables as rows and independent variables as columns

  for (i in 1:nspc) {
    n <- 3 * i - 2
    sensitivity[n, ] <- apply(dydxM[, , i], 2, median)
    sensitivity[n + 1, ] <- apply(dydxM[, , i], 2, min)
    sensitivity[n + 2, ] <- apply(dydxM[, , i], 2, max)
    # sensitivity[n+3,] <-
    rnames <- c(rnames, paste0(spcnames[i], " median"), paste0(spcnames[i], " min"), paste0(spcnames[i], " max"))
  }
  rownames(sensitivity) <- rnames
  # browser()
  return(sensitivity)
} # sensitivity.coefficients <- function(dydxM)


#-------------------Mass Balance analysis functions----------------------------------

MassBalance.analysis <- function(outdata, V, B, dydtMatrix, dv.df, Ef, gvs) {
  nspc <- gvs$nspc
  spcnames <- gvs$sn
  species.df <- gvs$sdf
  aP.app.lst <- gvs$aP.app.lst
  # Separates mass balance components, for each species
  #------Must still adjust for species without MW
  cat("Calculating mass balance components... ")
  res <- as.data.frame(outdata)
  rows.n <- length(res[, 1])
  cmpts <- c("time", "Indoor Conc.", "Outdoor Conc.", "dC/dt", "Infiltration", "Exfiltration", "Emissions", "Surf. deposition", "Filtration", "Reaction")
  MassBalance <- array(data = NA, dim = c(rows.n, 10, nspc), dimnames = list(timepoints = res[, 1], components = cmpts, species = spcnames))
  # Stack of matrices, one per species. Each matrix has time as a dimension (rows)
  # and the mass balance components as the other (columns)

  X <- matrix(ncol = 10, nrow = rows.n) # temporary matrix to store results
  X[, 1] <- (res[, 1]) # time
  convfactor.in <- ((res$BP) * 1e-6 / (SIACS.env$R * (res$Ti + 273.15))) # Converstion Indoor (ppm to mol/m3)
  convfactor.out <- ((res$BP) * 1e-6 / (SIACS.env$R * (res$To + 273.15)))
  noconv <- rep(1, rows.n)

  for (h in 1:nspc) { # Calculate mass balance estimates for each species
    if (species.df$gas[h]) {
      cvi <- convfactor.in * (species.df$mw[h])
      cvo <- convfactor.out * (species.df$mw[h])
    } else {
      cvo <- cvi <- noconv
    }
    # Conversion factor for Indoor  and Outdoor (g/m3)

    # Conversion factor for Outdoor (g/m3)
    X[, 2] <- res[, h + 1] * cvi # indoor concentrations (g/m3)
    X[, 3] <- res[, paste0(spcnames[h], ".O")] * cvo # outdoor concentration (g/m3)
    X[, 4] <- (dydtMatrix[, h + 1] * cvi) # Derivative (g/m3/min)
    X[, 5] <- X[, 3] * aP.app.lst[[h]](X[, 1] * 60) * 60 # Infiltration (g/m3/min)
    X[, 6] <- (-res$a / 60 * X[, 2]) # Exfiltration (g/m3/min)
    X[, 7] <- (res[, paste0(spcnames[h], ".S")] / V) # Emissions (g/m3/min)
    X[, 8] <- (-(dv.df$dvel[h]) * B$AreaToVolume * X[, 2]) # Surface Deposition (g/m3/min)
    X[, 9] <- -Ef[h] * res$QFilter / V * 60 * X[, 2] # Filtration or air cleaning (g/m3/min)
    X[, 10] <- (X[, 4] - X[, 5] - X[, 6] - X[, 7] - X[, 8] - X[, 9]) # Reaction (g/m3/min)
    MassBalance[, , h] <- X
  } # for(h in 1:nspc)
  cat(" Completed.\n")
  return(MassBalance)
} # MassBalance.analysis <- function(outdata)

# Test code
# Uncertainty <- Uncertainty.Propagation(O.uncert, S.uncert, E.uncert)
# sensit <- deriv.sensitivity.analysis(dydxMatrix)
# mb<-MassBalance.analysis(out)
