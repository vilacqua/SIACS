## Version 0920 (September 2020)

Calc.dydt <- function(outsaved, parms, gvs) {
  spcnames <- gvs$sn
  species.df <- gvs$sdf
  # Function that numerically calculates the partial time derivatives of
  # dependent (or output) variables
  ## extract time and indoor concentrations from output of ODE solver

  cat("Working on time derivatives of result variables. ")
  tVector <- outsaved[, "time"] # time [=] seconds
  yMatrix <- outsaved[, spcnames] # concentration [=] molec cm-3 for gas, .. for PM
  ntime <- length(tVector)

  ## calculate dy/dt one time step as a time

  dydtMatrix <- vector(mode = "numeric")
  # for ( itime in seq(ntime) ) {
  #     t <- tVector[itime]  # time in seconds
  #     y <- yMatrix[itime,] # concentrations (molec/cm3 for gas)
  #     tmp <- model.engine (t, y, parms=NULL) # output from the ODE function
  #     dydtVector <- tmp[[1]] # dy/dt is contained in the first part of model.engine output
  #     Mair <- outsaved[itime,"Mair.in"] # indoor air concentration [=] molecules/cm3
  #     ## convert units
  #     dydtVector[species.df$gas == 1] <- dydtVector[species.df$gas == 1]/Mair*1e6*60.0 # convert to ppm/min
  #     # gas species
  #     dydtVector[species.df$gas == 0] <- dydtVector[species.df$gas == 0]*60.0 # convert to ug/(m3 min)
  #     # aerosol species
  #     dydtMatrix <- rbind(dydtMatrix, c(t/60.0,dydtVector) )
  #
  # } # for ( itime in seq(ntime) )


  # Vectorize for loop
  # Define a function to process each time step
  process_time_step <- function(itime) {
    t <- tVector[itime] # Time in seconds
    y <- yMatrix[itime, ] # Concentrations (molec/cm3 for gas)
    tmp <- model.engine(t, y, parms) # output from the ODE function
    # tmp <- model.engine(t, y, parms=NULL)  # Output from the ODE function
    dydtVector <- tmp[[1]] # dy/dt is contained in the first part of model.engine output
    Mair <- outsaved[itime, "Mair.in"] # Indoor air concentration [=] molecules/cm3

    # Convert units
    dydtVector[species.df$gas == 1] <- dydtVector[species.df$gas == 1] / Mair * 1e6 * 60.0 # Convert to ppm/min for gas species
    dydtVector[species.df$gas == 0] <- dydtVector[species.df$gas == 0] * 60.0 # Convert to ug/(m3 min) for aerosol species

    # Return the result for this time step
    return(c(t / 60.0, dydtVector))
  }

  # Apply the function to all time steps and combine results
  dydtMatrix <- do.call(rbind, lapply(seq(ntime), process_time_step))




  colnames(dydtMatrix) <- c("time", spcnames)
  cat("Completed.\n")
  return(dydtMatrix)
} # Calc.dydt <- function()

Calc.dxdt <- function(out, time.read, dydtMatrix, gvs) {
  spcnames <- gvs$sn
  nspc <- gvs$nspc
  combined.variables.n <- gvs$combined.variables.n
  # Numerically Calculates the partial time derivatives of independent ( or input) variables

  cat("Working on time derivatives of input variables... ")
  tVector <- out[, "time"] # time [=] minutes
  ntime <- length(tVector)
  xMatrix <- out[, -seq(from = 2, to = (nspc + 1))] # removes the indoor dependent variables
  # keeping a as an independent variable for now, useful for analysis
  nvar <- ncol(xMatrix)
  xMatrix <- xMatrix[, -seq(from = nvar - combined.variables.n + 1, to = nvar)] # removes the combined variables used for graphing
  xMatrix <- xMatrix[, colnames(xMatrix) != "Mair.in"] # removes concentration of air
  ninputvars <- ncol(xMatrix) # should just do those for supplied values?
  dxdtMatrix <- data.frame(matrix(data = 0, nrow = ntime, ncol = ninputvars))
  names(dxdtMatrix) <- colnames(xMatrix)
  nalist <- NULL

  # Vectorized calculation for the 3-point centered numerical approximation of derivative
  dxdtMatrix[2:(ntime - 1), ] <- (xMatrix[3:ntime, ] - xMatrix[1:(ntime - 2), ]) / (2 * time.read$TimeStep)

  # Rounding to significant digits
  dxdtMatrix[2:(ntime - 1), ] <- round(dxdtMatrix[2:(ntime - 1), ], sigdig)




  dxdtMatrix[1, ] <- (xMatrix[2, ] - xMatrix[1, ]) / (time.read$TimeStep)
  dxdtMatrix[ntime, ] <- (xMatrix[ntime, ] - xMatrix[ntime - 1, ]) / (time.read$TimeStep)
  # 2-point forward or backward numerical approximation of derivative for 1st and last values
  dxdtMatrix[1, ] <- round(dxdtMatrix[1, ], sigdig)
  dxdtMatrix[ntime, ] <- round(dxdtMatrix[ntime, ], sigdig)
  nalist <- which(unlist(dxdtMatrix[1, ], use.names = FALSE) %in% NA) # list of variables with NA value (e.g. because they have undefined MW)
  dxdtMatrix[, 1] <- tVector
  dxdtMatrix <- dxdtMatrix[, -nalist] # removes variables with NA values
  dydtMatrix <- cbind(dydtMatrix, dxdtMatrix[, "a"]) # adds the air exchange rate to list of dependent variable derivatives
  # the air exchange rate can be useful both as dependent or independent variable, so left here and added to dydt
  cat(" Completed.\n")
  return(dxdtMatrix)
} # Calc.dxdt <- function()

Calc.dydx <- function(dydtM, dxdtM, gvs) {
  # Function that approximates partial derivatives of dependent (or output)
  # variables with respect to independent (or input) variables at each point in time
  # Uses chain rule and relies on assumptions about unchanging derivatives
  # The resulting 3-d matrix can also be used for sensitivity coefficients

  spcnames <- gvs$sn
  nspc <- gvs$nspc

  cat("Working on partial derivatives of results with respect to input variables... ")
  zeroderiv <- apply(dxdtM, 2, sum) # vector of variables with derivative always 0
  dxdtM <- dxdtM[zeroderiv != 0] # remove variables that are always zero.
  # The assumption is that they had no effect on uncertainty
  indvar.n <- ncol(dxdtM) - 1
  depvar.n <- ncol(dydtM) - 1
  timepoints <- nrow(dydtM)
  spnames <- c(spcnames, "a") # air exchange was added to list od dependent variables
  dydxMatrix <- array(data = NA, dim = c(timepoints, indvar.n, depvar.n), dimnames = list(timepoints = dydtM[, 1], ddx = colnames(dxdtM[, -1]), ys = colnames(dydtM[, -1])))
  # Stack of matrices, one per dependent variable. Each matrix has time as a dimension (rows)
  # and the derivatives with respect to each independent variable as the other (columns)

  # Convert dydtM to a matrix with the correct number of columns
  # dydtM <- matrix(dydtM, ncol = dim(dydtM)[2])
  dydtM <- as.matrix(dydtM)
  # Convert dxdtM to a numeric matrix
  dxdtM <- as.matrix(dxdtM)
  dxdtM <- apply(dxdtM, 2, as.numeric)

  # Define a function to process each time point
  process_time_point <- function(time_point) {
    dydt_row <- matrix(dydtM[time_point, -1], nrow = 1)
    dxdt_row <- matrix(dxdtM[time_point, -1], nrow = 1)
    result <- t(dydt_row) %*% (1 / dxdt_row)
    return(result)
  }


  # Apply the function to all elements in the timepoints vector
  results_list <- lapply(seq(1, timepoints, by = 1), process_time_point)

  # Check the length and structure of results_list
  # print(length(results_list))  # Should be 325
  # print(dim(results_list[[1]]))  # Should be ncol(dydtM) - 1, ncol(dxdtM) - 1

  # Combine the results into an array with the correct dimensions
  dydxMatrix <- array(unlist(results_list), dim = c((ncol(dydtM) - 1), (ncol(dxdtM) - 1), timepoints))
  dydxMatrix <- aperm(dydxMatrix, c(3, 2, 1))
  # Assign column names to dydxMatrix
  # dimnames(dydxMatrix) <- list(NULL, colnames(dxdtM)[-1], colnames(dydtM)[-1])
  dimnames(dydxMatrix) <- list(timepoints = dydtM[, 1], ddx = colnames(dxdtM[, -1]), ys = colnames(dydtM[, -1]))

  #########
  dydxMatrix[abs(dydxMatrix) == Inf] <- 0
  dydxMatrix[is.nan(dydxMatrix)] <- 0
  # based on assumption above, replaces infinite and undefined values with zero
  cat(" Completed.\n")
  return(dydxMatrix)
} # Calc.dydx <- function(dydtM, dxdtM)

Centered.derivative <- function(M) {
  # Calculates time derivative of variables in a matrix that has time in the 1st column
  # not yet in use; for later use with uncertainty of a and Mair
  timepoints <- M[, 1]
  Mrows <- length(timepoints)
  Mcols <- ncol(M)
  dtMatrix <- matrix(data = NA, nrow = Mrows, ncol = Mcols)


  # Vectorized computation of the 3-point centered numerical approximation
  time_diff <- timepoints[3:Mrows] - timepoints[1:(Mrows - 2)]
  dtMatrix[2:(Mrows - 1), ] <- (M[3:Mrows, ] - M[1:(Mrows - 2), ]) / time_diff

  # Rounding to significant digits
  dtMatrix[2:(Mrows - 1), ] <- round(dtMatrix[2:(Mrows - 1), ], sigdig)

  dtMatrix[1, ] <- (M[2, ] - M[1, ]) / (timepoints[2] - timepoints[1])
  dtMatrix[Mrows, ] <- (M[Mrows, ] - M[Mrows - 1, ]) / (timepoints[Mrows] - timepoints[Mrows - 1])
  # 2-point forward or backward numerical approximation of derivative for 1st and last values
  return(dtMatrix)
} # Centered.derivative <- function(M)
