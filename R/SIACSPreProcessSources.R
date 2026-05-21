# Functions to calculate and adjust emission data accounting for activities, emission profiles, temperature and humidity prior to model run
## Version 0922 (September 2022)


PreProcessSourceStrength <- function(physical, profiles, timeactivity, A, Env = E, PEAFl = PhysEnvAppFncs.lst) {
  E <- Env
  PhysEnvAppFncs.lst <- PEAFl

  l <- length(timeactivity[, 1])
  x <- matrix(0, nrow = l, ncol = ncol(profiles) - 1) # matrix that will contain emissions by species (columns) and time (rows)
  M <- match(colnames(timeactivity), profiles[, 1]) # vector that indicates at which row of the emission profiles each activity is found
  for (c in 2:length(M)) {
    if (is.na(M[c])) message("Activity ", names(timeactivity[c]), " could not be matched to an emission profile. It has been ignored.")
  }
  ####
  apply_timeactivity <- function(ts, A) {
    tmp <- do.call(rbind, lapply(2:length(M), function(c) {
      if (is.na(M[c])) {
        return(NULL)
      }

      sourceemissions <- profiles[M[c], ]
      fctname <- paste(unlist(sourceemissions[1], use.names = FALSE)) # name of environment-dependence function associated with this profile
      sourceemissions <- sourceemissions[-1] # lose non-numeric part

      physicalenvironment <- physicalinterpolation(timeactivity[ts, 1], full = FALSE, Env = E, PEAFl = PhysEnvAppFncs.lst) # interpolates physical variables for the same time in timeactivity data, which may not exist in the physical environment data

      if (timeactivity[ts, c] & (fctname %in% names(Emission.effect.environment))) {
        if (fctname == "Generic") {
          sourceemissions <- Emission.effect.environment[[fctname]](sourceemissions, physicalenvironment, A) # pass A here
        } else {
          sourceemissions <- Emission.effect.environment[[fctname]](sourceemissions, physicalenvironment)
        }
      }

      sourceemissions <- sourceemissions * timeactivity[ts, c] # emissions from a profile set to the multiplier (including 0) at a point in time

      return(sourceemissions)
    }))

    if (is.null(tmp)) {
      return(rep(0, ncol(profiles) - 1)) # return a vector of zeros if tmp is NULL
    } else {
      return(apply(tmp, 2, sum, na.rm = TRUE))
    }
  }

  x <- t(sapply(seq_len(nrow(timeactivity)), function(ts) apply_timeactivity(ts, A)))
  ####

  colnames(x) <- colnames(profiles[-1]) # adds names of species from profile matrix
  x <- cbind(timeactivity[1], x) # adds time column from activities schedule
  x <- as.data.frame(x)

  # To consider later: add transition times, to simplify data inputs

  return(x)
}



# This section has functions that modify the emissions depending on environmental conditions (temperature, RH,...)
# It's list of function. Each function applies to a particular source profile. Each function receives a vector of emission strengths and modifies emissions for all known species

Emission.effect.environment <- list(
  Generic = function(x, p, A) {
    x$HCHO <- HCHO.Xiong.2016(x$HCHO, p$Ti, p$RH, A) # HCHO emissions dependent on temperature, RH and surface area
    return(x)
  },
  Adult = function(x, p) {
    x$NH3 <- NH3.Li.2020(x$NH3, p$Ti) # NH3 emissions dependent on temperature

    return(x)
  },
  Child = function(x, p) {
    x$NH3 <- NH3.Li.2020(x$NH3, p$Ti) # NH3 emissions dependent on temperature

    return(x)
  }
)


NH3.Li.2020 <- function(emission, T) {
  emission <- (emission * 1 / (60 * 1000)) * exp(-27.5 * (1000 / T) + 91.4) # NH3 emissions dependent on temperature
  return(emission)
}

HCHO.Xiong.2016 <- function(emission, T, RH, A) {
  emission <- emission * 1.48e11 * (T^0.75) * exp(2.78 * RH - (7450 / T)) * (1e-6 / 60) * A # HCHO emissions dependent on temperature, RH and surface area
  return(emission)
}
