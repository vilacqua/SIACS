SIACS <- function(times, deposition, building, physical.env, outdoor, emission.profiles, Activities, Indoor.Flux, Initial.values, perturbation = FALSE, chemistry = TRUE, mechanism = "SAPRC99",
                  numspc = nspc, eunc = E.uncert, ounc = O.uncert, sunc = S.uncert, OutdoorNames = OutdoorConcs, sn = spcnames,
                  sdf = species.df, EmissionNames = Emissions, start = start.time, Ngas = ngas, N = nrxn) {
  # Main function; prepares functions from input values received and launches ODE solver

  # Rename previously global variables
  E.uncert <- eunc
  O.uncert <- ounc
  S.unc <- sunc

  nspc <- numspc
  B <- building
  E <- emission.profiles
  spcnames <- sn
  species.df <- sdf
  Emissions <- EmissionNames
  start.time <- start
  ngas <- Ngas
  nrxn <- N

  #-----------------Sets variables that would be clunky to pass as arguments

  SIACS.env$perturbation <- perturbation
  SIACS.env$chemistry <- chemistry
  chemical.mechanism.selection(mechanism) # assigns the variables and functions for the requested chemical mechanism

  #----------------- Read in input data --------------------------------------------



  deposition$dvel <- deposition$dvel * 100. / 60. # deposition velocities, (time-independent baseline), read as m/min, converted to cm s-1
  P <- deposition$PFactor # penetration factors, dimensionless
  IfE <- deposition$IntakeFilterEfficiency # efficiency of filters at air intake, dimensionless
  Ef <- deposition$FilterEfficiency # efficiency of air cleaner or HVAC filter, dimensionless

  V <- building$FloorSurfaceArea * building$RoomHeight # room volume in m3
  A <- building$AreaToVolume * V # surface area in m2
  AtoV <- 0.01 * building$AreaToVolume # area/volume in cm-1

  Dep_vel <- variable.dep_vel(deposition, Activities, A) # calculates time-varying deposition velocities
  DvelAppFncs.lst <- DefineInterpolationSet("Deposition", Dep_vel, nspc, P = P, fctsetuncert = 0, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions)
  # creates time interpolation of variable deposition velocities

  PhotoAppFncs.lst <- DefineInterpolationSet("Photolysis", Indoor.Flux$J_values, ncol(Indoor.Flux$J_values) - 1, P = P, fctsetuncert = 0, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions)

  LightEnergyAppFnc <- DefineInterpolationFunction(Indoor.Flux$EnergyFlux[, 1], Indoor.Flux$EnergyFlux[, 2], uncert = 0)
  # creates time interpolation for photolysis rates and Energy flux

  PhysEnvAppFncs.lst <- DefineInterpolationSet("Physical", physical.env, nphys - 2, P = P, E.uncert, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions)
  # create time interpolation functions for input data specified in physical environment file (7 variables)
  # the interpolation functions have various units and time in seconds
  aP.app.lst <- DefineInterpolationSet("Infiltration", physical.env, nspc, P = P, E.uncert, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions)
  # create time interpolation functions for infiltration specific to each species, using penetration factors, building information, ventilation variables
  # the interpolation functions have units of s-1 and time in seconds

  OutdoorConcAppFncs.lst <- DefineInterpolationSet("Outdoor", outdoor, nspc, P = P, O.uncert, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions) # create time interpolation functions for outdoor concentrations.
  names(OutdoorConcAppFncs.lst) <- names(OutdoorNames)
  ## Interpolated outdoor concentrations are in units of ppm. The interpolation functions will use time in seconds as input argument.

  ## Pre-processing function that creates the Indoor emissions (source strengths) over time based on activities, emission profiles, temperature and humidity
  S <- PreProcessSourceStrength(physical.env, emission.profiles, Activities, A, Env = E, PEAFl = PhysEnvAppFncs.lst)
  S <- ChemSpeciesStandardize(S, withtime = TRUE, Nspc = nspc, Spcnames = spcnames) # standardizes order and adds 0s for missing inputs
  S.uncert <- S.unc * apply(S[, -1], 2, mean, na.rm = TRUE) # converts uncertainty from relative to absolute terms; must be done after S has been computed; time column is ignored
  EmisAppFncs.lst <- DefineInterpolationSet("Sources", S, nspc, P = P, S.unc, IfE = IfE, V = V, B, sdf = species.df, EmissionNames = Emissions) # create time interpolation functions for source (emission) rates
  # Interpolated emissions are in units of molecules cm-3/s. The interpolation functions will use time in seconds as input argument.

  Y.ini <- InitialValues(Initial.values, outdoor[outdoor$Time == start.time, ], S[S$Time == start.time, ], start.time, V, AtoV, Ef, DvelAppFncs.lst,
    Nspc = nspc, Spcnames = spcnames, Ngas = ngas, PEAFl = PhysEnvAppFncs.lst, Env = E, sdf = species.df, aal = aP.app.lst
  )
  # molecules/cm3 for gases; ug/m3 or cm-3 for particles

  parms <- list(
    Ef = Ef, AtoV = AtoV, V = V, DvelAppFncs.lst = DvelAppFncs.lst, PhotoAppFncs.lst = PhotoAppFncs.lst,
    Env = E, PEAFl = PhysEnvAppFncs.lst, OCAFl = OutdoorConcAppFncs.lst, EAFl = EmisAppFncs.lst, N = nrxn,
    Nspc = nspc, sdf = species.df, aal = aP.app.lst
  ) # converting these parameters from global to local

  ## -------------------Solve the ODEs--------------------------------------------------

  comptime1 <- Sys.time() # takes time before solver is launched
  cat("Solving differential equations...\n")
  out <- lsode(
    func = model.engine, parms = parms, y = Y.ini, times = times,
    jactype = "fullusr", jacfunc = fulljac, verbose = FALSE, maxsteps = 5000
  )
  # class(out) is "deSolve" "matrix";
  # attributes(out)$type
  # indicates what solver was
  # used; diagnostics(out) gives
  # some information
  comptime2 <- Sys.time() # takes time after running the solver
  PrintComputationTime("Time in ODE solver for simulation ", comptime1, comptime2, instance = 0, runs = 0)

  ## ------------Convert units of ODE solver output---------------------------------

  outsaved <- out
  out <- Adjust.output(out, V, LightEnergyAppFnc, Spcnames = spcnames, sdf = species.df) # converts units and adds Energy flux
  tmplst <- combine.species(out) # Adds a few species combining results
  out <- tmplst$main
  combined.variables.n <- tmplst$n

  Results <- list(alldata = out, deSolve = outsaved, parms = parms)
  invisible(list(Results, aP.app.lst, combined.variables.n))
}
