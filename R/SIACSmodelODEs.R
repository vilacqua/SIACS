## Version 0924 (September 2024)

model.engine <- function(t, y, parms) {
  ## approximation of input data at time t --------------------------------------------
  Ef <- parms$Ef
  AtoV <- parms$AtoV
  V <- parms$V
  DvelAppFncs.lst <- parms$DvelAppFncs.lst
  PhotoAppFncs.lst <- parms$PhotoAppFncs.lst
  E <- parms$Env
  PhysEnvAppFncs.lst <- parms$PEAFl
  OutdoorConcAppFncs.lst <- parms$OCAFl
  EmisAppFncs.lst <- parms$EAFl
  nrxn <- parms$N
  RCT <- rep(0.0, nrxn)
  AR <- rep(0.0, nrxn)
  nspc <- parms$Nspc
  species.df <- parms$sdf
  aP.app.lst <- parms$aal

  Reaction.Rates <- Reaction.Rates.SAPRC99
  Reactions.Sum <- Reactions.Sum.SAPRC99
  ReturnRCT <- ReturnRCT.SAPRC99
  LU_IROW <- LU_IROW.SPRC99
  LU_ICOL <- LU_ICOL.SPRC99
  Jacobian.reaction.components <- Jacobian.reaction.components.SAPRC99
  Jacobian.Terms <- Jacobian.Terms.SAPRC99




  ## approximate values of physical environment variables at time t (t [=] seconds)
  p <- physicalinterpolation(t, full = TRUE, Env = E, PEAFl = PhysEnvAppFncs.lst)

  ## approximate of ambient concentrations (molecules cm-3), emission
  ## rates (molecules cm-3 s-1), penetration-adjusted infiltration rate (s-1), J values for photochemistry (1/s), and deposition velocities (cms/s)
  ## at time t (seconds)
  aP <- unlist(lapply(aP.app.lst, function(f) f(t))) # s-1
  OutdoorConcs <- unlist(lapply(OutdoorConcAppFncs.lst, function(f) f(t))) # ppm
  Emissions <- unlist(lapply(EmisAppFncs.lst, function(f) f(t))) # molecules cm-3 s-1
  J <- lapply(PhotoAppFncs.lst, function(f) f(t)) # s-1
  dvel <- unlist(lapply(DvelAppFncs.lst, function(f) f(t))) # cm/s

  O2 <- p$Mair.in * SIACS.env$O2Fraction # O2 concentration in molecules cm-3
  H2 <- 0.0 # H2 concentration in molecules cm-3. This is a simplification that neglects H2 indoors
  cfactor <- p$Mair.in * 1e-6 # ppm in terms of molecules cm-3; 1e-6 is for partial pressure (ppm)


  ## reaction rate constants, units in terms of molecules, cm3, and seconds------------
  RCT <- ReturnRCT(J, p$Ti, cfactor, RCT) # can memoise from Jacobian be used here instead of repeating?

  # computation of reaction rates in molecules cm-3 s-1 ------------------------------
  AR <- Reaction.Rates(RCT, y, p, O2, H2, AR)

  ## Finite differences from Differential Equations in the mechanism; molecules cm-3 s-1 ------------------------------
  dy <- Reactions.Sum(AR, Nspc = nspc)

  ## add air exchange, deposition, emission, and filtration terms for the ODEs -----------------
  OutdoorConc <- OutdoorConcs
  OutdoorConc[species.df$gas == 1] <- OutdoorConc[species.df$gas == 1] * cfactor # molecules cm-3
  dy <- dy +
    aP * OutdoorConc - p$a * y - # air exchange
    dvel * AtoV * y + # deposition
    Emissions - # emission
    Ef * p$QFilter / V * y # filtration

  list(c(dy) # molecules cm-3 s-1
    , OutdoorConcs, Emissions,
    Ti = p$Ti, To = p$To, a = p$a, SunFactor = p$SunFactor, RH = p$RH, BP = p$BP,
    Mair.in = p$Mair.in, QFilter = p$QFilter
  )
} # model.engine <- function (t, y, parms)

## Model Jacobian, following page 22 [section 6) of
## https://cran.r-project.org/web/packages/deSolve/vignettes/deSolve.pdf

fulljac <- function(t, y, params) {
  ## approximation of input data at time t
  Ef <- params$Ef
  AtoV <- params$AtoV
  V <- params$V

  nrxn <- params$N
  RCT <- rep(0.0, nrxn)
  AR <- rep(0.0, nrxn)
  nspc <- params$Nspc
  species.df <- params$sdf
  E <- params$Env
  PhysEnvAppFncs.lst <- params$PEAFl

  Reaction.Rates <- Reaction.Rates.SAPRC99
  Reactions.Sum <- Reactions.Sum.SAPRC99
  ReturnRCT <- ReturnRCT.SAPRC99
  LU_IROW <- LU_IROW.SPRC99
  LU_ICOL <- LU_ICOL.SPRC99
  Jacobian.reaction.components <- Jacobian.reaction.components.SAPRC99
  Jacobian.Terms <- Jacobian.Terms.SAPRC99



  DvelAppFncs.lst <- params$DvelAppFncs.lst
  PhotoAppFncs.lst <- params$PhotoAppFncs.lst
  p <- physicalinterpolation(t, full = TRUE, Env = E, PEAFl = PhysEnvAppFncs.lst)
  J <- lapply(PhotoAppFncs.lst, function(f) f(t))
  dvel <- unlist(lapply(DvelAppFncs.lst, function(f) f(t))) # cm/s

  O2 <- p$Mair.in * SIACS.env$O2Fraction # O2 concentration in molecules cm-3
  H2 <- 0.0 # H2 concentration in molecules cm-3
  cfactor <- p$Mair.in * 1e-6 # ppm in terms of molecules cm-3

  ## reaction rate constants, units in terms of molecules, cm3, and seconds
  RCT <- ReturnRCT(J, p$Ti, cfactor, RCT)

  B <- Jacobian.reaction.components(RCT, y, p, O2, H2)

  ## Construct the Jacobian terms from B's
  ## Jac_Full[i,j] = d/dyj (dyi/dt), .e.g, Jac_Full[3,62] = d/dO3 (dCCO_OH/dt)

  JVS <- Jacobian.Terms(B)

  jac <- matrix(data = 0, nrow = nspc, ncol = nspc)
  # for ( count in seq(length(JVS)) ) {
  #   jac[LU_IROW[count],LU_ICOL[count]] <- JVS[count] # s-1
  # } # for ( count in seq(njacentries) )

  ####
  jac[cbind(LU_IROW, LU_ICOL)] <- JVS
  ####

  # This set of instructions manually modified the KPP output to add methane chemistry to SAPRC99.
  # It can be deleted when a new KPP-output is available, for SAPRC99 and for other mechanisms
  jac[75, 74] <- -B[318] # shc Jacobian term for d/d[OH](d[CH4]/dt)
  jac[75, 75] <- -B[319] # shc d/d[CH4](d[CH4]/dt])
  jac[74, 75] <- -B[319] # shc d/d[CH4](d[OH]/dt])
  jac[66, 75] <- B[319] # shc d/d[CH4](d[C_O2]/dt])
  # end of instructions to be deleted when a new KPP output is available

  ## add position and air exchange terms
  # for ( ispc in seq(nspc) ) {
  #   jac[ispc,ispc] <- jac[ispc,ispc] - p$a - dvel[ispc]*AtoV - Ef[ispc]*p$QFilter/V # s-1
  # } # for ( ispc in seq(nspc) )

  ####
  jac[cbind(seq(nspc), seq(nspc))] <- jac[cbind(seq(nspc), seq(nspc))] - p$a - dvel * AtoV - Ef * p$QFilter / V
  ####

  return(jac)
} # fulljac <- function (t,y,params)
