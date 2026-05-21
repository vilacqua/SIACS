# Functions that interpolate input data at observation times to return a value at any time
# Also functions that calculate reaction rates based on these interpolations
# For SIACS 0924
#---------------------------------------------------------------------------------------

# This function returns a list of functions that interpolate various input variables
DefineInterpolationSet <- function(fcsetname, fctsetdata, fcsetlength, P, fctsetuncert = 0, IfE, V, B, sdf = species.df,
                                   EmissionNames = Emissions) {
  species.df <- sdf
  Emissions <- EmissionNames


  AppFncs <- vector(mode = "list", length = fcsetlength)
  #x <- fctsetdata$Time
  x <- fctsetdata[, "Time"]
  y <- subset(fctsetdata, select = -Time)

  if (fcsetname == "Physical") { # Functions that interpolate the physical variables
    AppFncs[[1]] <- DefineInterpolationFunction(x, y$Ti, fctsetuncert$Ti) # indoor temperature (K)
    AppFncs[[2]] <- DefineInterpolationFunction(x, y$To, fctsetuncert$To) # outdoor temperature (K)
    AppFncs[[3]] <- DefineInterpolationFunction(x, y$BP, fctsetuncert$BP) # barometric pressure (Pa)
    AppFncs[[4]] <- DefineInterpolationFunction(x, y$RH, fctsetuncert$RH) # indoor relative humidity (fraction)
    AppFncs[[5]] <- DefineInterpolationFunction(x, y$BP / (SIACS.env$R * y$Ti) * SIACS.env$Avogadro * 1e-6) #  indoor air concentration (molec cm-3) from n=PV/RT; 1e-6 factor is for m3->cm3 conversion
    AppFncs[[6]] <- DefineInterpolationFunction(
      x, Q.tot(Q.inf(B$InfiltrationSurfaceArea, B$StackCoefficient, y$Ti, y$To, B$WindCoefficient, y$Wind),
        1,
        IFE = 0, qbal = y$QBal, qunbal = y$QUnbal,
        qnat = Q.nat(y$OpenWindowArea, B$DischargeCoefficient, B$GravityAccel, B$MidpointHeightofWindow, B$NeutralPressureLevel, y$Ti, y$To, B$OpeningEffectivenessCoefficient, y$Wind)
      ) / V # air exchange rate (s-1), calculated from
    ) # ventilation functions that use information on building, weather,windows, and mechanical ventilation
    AppFncs[[7]] <- DefineInterpolationFunction(x, y$QFilter, fctsetuncert$QFilter) # air flow through filter or air cleaner (m3/s)

    names(AppFncs) <- c("Ti", "To", "BP", "RH", "Mair.in", "a", "QFilter")
    return(AppFncs)
  } # if(fcsetname == "Physical")

  if (fcsetname == "Infiltration") { # Functions that interpolate infiltration rates, species-specific based on their penetration factors
    for (i in 1:fcsetlength) {
      AppFncs[[i]] <- DefineInterpolationFunction(
        x, Q.tot(Q.inf(B$InfiltrationSurfaceArea, B$StackCoefficient, y$Ti, y$To, B$WindCoefficient, y$Wind),
          P[i],
          IFE = IfE[i], qbal = y$QBal, qunbal = y$QUnbal,
          qnat = Q.nat(y$OpenWindowArea, B$DischargeCoefficient, B$GravityAccel, B$MidpointHeightofWindow, B$NeutralPressureLevel, y$Ti, y$To, B$OpeningEffectivenessCoefficient, y$Wind)
        ) / V # Infiltration rates for each species, using penetration factors (s-1), calculated from
      ) # ventilation functions that use information on building, weather,windows, and mechanical ventilation
    } # for i

    names(AppFncs) <- colnames(y)
    return(AppFncs)
  } # if(fcsetname == "Infiltration")

  if (fcsetname == "Outdoor") { # Functions that interpolate outdoor concentrations
    for (i in 1:fcsetlength) {
      col_vals <- y[, i]
      # FIX: be robust to NAs in user-supplied outdoor data. The original
      # `sum(y[, i]) == 0` returned NA when any NA was present, causing
      # `if (NA)` -> "missing value where TRUE/FALSE needed". Now we
      # explicitly skip NAs in the all-zero check, and the interpolation
      # itself is built on the NA-stripped pairs to avoid splinefun warnings.
      if (sum(col_vals, na.rm = TRUE) == 0) {
        AppFncs[[i]] <- function(t) {
          0
        }
        next
      }
      keep <- !is.na(col_vals)
      AppFncs[[i]] <- DefineInterpolationFunction(x[keep], col_vals[keep], fctsetuncert[[i]])
    } # for i
    # names(AppFncs) <- names(OutdoorConcs)
    return(AppFncs)
    # this is provided and returned in ppm. The interpolation cannot convert to molecules/cm3 here, because it depends on physical variables that may need interpolation to match times of obervation
  }

  if (fcsetname == "Sources") {
    for (i in 1:fcsetlength) {
      if (sum(y[, i], na.rm = TRUE) == 0) {
        AppFncs[[i]] <- function(t) {
          0
        }
        next
      }
      Emiss <- y[, i] * 1e-6 / (60 * V) # convert from g/min to g/cm3/s: 60 for min/s; 1e-6 for m3/cm3
      if (species.df$gas[i]) Emiss <- Emiss * SIACS.env$Avogadro / species.df$mw[i] # conversion to molecules/cm3/s
      AppFncs[[i]] <- DefineInterpolationFunction(x, Emiss, fctsetuncert[[i]])
    } # for i

    names(AppFncs) <- names(Emissions)
    return(AppFncs)
  }
  if (fcsetname == "Photolysis") { # functions that interpolate over time the photolysis rates (J values) calculated from indoor light by Photolysis.rates() function
    for (i in 1:fcsetlength) {
      if (sum(y[, i]) == 0) {
        AppFncs[[i]] <- function(t) {
          0
        }
        next
      }
      AppFncs[[i]] <- DefineInterpolationFunction(x, y[, i])
    }
    names(AppFncs) <- colnames(y)
    return(AppFncs)
  }

  if (fcsetname == "Deposition") { # functions that interpolate over time the deposition velocities, based on reaction with changing surface (e.g human skin)
    for (i in 1:fcsetlength) {
      if (sum(y[, i]) == 0) {
        AppFncs[[i]] <- function(t) {
          0
        }
        next
      }
      AppFncs[[i]] <- DefineInterpolationFunction(x, y[, i])
    } # for i
    names(AppFncs) <- colnames(y)
    return(AppFncs)
  }
} # DefineInterpolationSet <- function(fcsetname,fcsetlength)

DefineInterpolationFunction <- function(timeobs, varobs, uncert = 0) {
  # Interpolates a variable from observations at points in time
  # time supplied in minutes, interpolation returns results for time in seconds (60 s/min)
  # Method that tested best is Monotone Hermite spline according to the method of Fritsch and Carlson
  # Interpolates square root so that later squaring results in non-negative interpolation

  fctn <- splinefun(60 * timeobs, sqrt(varobs), method = "monoH.FC")
  fct <- AddUncertaintyNoise(fctn, uncert, phaseshift = runif(1, min = 1.2, max = 2))
  return(fct)
}

AddUncertaintyNoise <- function(q, uncert = 0, phaseshift = 1) {
  # Adds variation to interpolated variable based on measurement uncertainty
  # Also calculates square value of interpolation, which was previously calculated on square root
  # for non-negative interpolation
  if (uncert & SIACS.env$perturbation) {
    function(t) {
      abs((q(t))^2 + uncert * sin(t * 8.72664626E-04 / phaseshift))
      # 8.72E-4 is pi/3600, i.e. one half cycle per hour
      # the phaseshift is to avoid having all interpolations in sync
      # Uncertainty is based on random errors and normally distributed, so a random number should be generated based on that distribution
      # The current uncertainty is NOT from a normal distrbution and oversamples extremes
      # Absolute value is used to avoid generating negative numbers
    }
  } else {
    function(t) {
      (q(t))^2 # to avoid burdening calculation with pointless operations if uncertainty is 0
    }
  }
}


# This function returns the interpolated physical variables at a specific point in time
physicalinterpolation <- function(thistime, full = TRUE, Env = E, PEAFl) {
  E <- Env
  PhysEnvAppFncs.lst <- PEAFl

  p <- E[1, ]
  p[1, 1] <- thistime # to match vector returned
  p$Ti <- PhysEnvAppFncs.lst$Ti(thistime) # temperature
  p$RH <- PhysEnvAppFncs.lst$RH(thistime) # relative humidity
  p$BP <- PhysEnvAppFncs.lst$BP(thistime) # barometric pressure

  # Outdoor variables, ventilation information not interpolated in calls from Source Strength Pre-processing
  if (full) {
    p$To <- PhysEnvAppFncs.lst$To(thistime) # outdoor temperature (K)
    p$QFilter <- PhysEnvAppFncs.lst$QFilter(thistime) # filter/air cleaner flow rate (m3/s)
    a <- PhysEnvAppFncs.lst$a(thistime)
    names(a) <- "a" # air exchange rate (s-1)
    H2O <- WaterVaporMassConc(p$RH, p$Ti, p$BP)
    names(H2O) <- "H2O" # indoor water vapor concentration  molecules/cm3
    Mair.in <- PhysEnvAppFncs.lst$Mair.in(thistime)
    names(Mair.in) <- "Mair.in" # indoor air concentration in molecules cm-3

    p <- c(p, a, H2O, Mair.in)
  }
  return(p)
}
