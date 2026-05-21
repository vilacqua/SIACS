## Version 0922 (September 2022)
# Light calculation functions for SIACS 0922

IndoorLightFlux <- function(time.read, Outdoor.Flux.direct, Outdoor.Flux.diffuse, Artificial.Flux, windows, glass, B) {
  # windows is supplied with a header, glass is supplied without
  # windows <- read.csv("./Input/Windows.csv", comment.char = "#")
  # glass <- read.csv("./Input/GlassTransmissions.csv", header=FALSE)

  max.duration <- min(max(Outdoor.Flux.direct[, 1]), max(Artificial.Flux[, 1]))
  if (max.duration / 60 < time.read$Duration) stop("Data on light not supplied for the entire duration of the simulation")
  Outdoor.Flux.direct <- as.data.frame(Outdoor.Flux.direct[Outdoor.Flux.direct[, 1] <= max.duration, ])
  Outdoor.Flux.diffuse <- as.data.frame(Outdoor.Flux.diffuse[Outdoor.Flux.diffuse[, 1] <= max.duration, ])
  Artificial.Flux <- as.data.frame(Artificial.Flux[Artificial.Flux$Time <= max.duration, ])
  cat(sprintf("  IndoorLightFlux: OutdoorDirect=%d rows, OutdoorDiffuse=%d rows, ArtifFlux=%d rows, max.duration=%.1f\n",
              nrow(Outdoor.Flux.direct), nrow(Outdoor.Flux.diffuse), nrow(Artificial.Flux), max.duration))
  if (nrow(Outdoor.Flux.direct) < 2) stop("OutdoorLightDirect has fewer than 2 rows after filtering — cannot interpolate.")
  abs.time.interp <- splinefun(x = Outdoor.Flux.direct$Time, y = Outdoor.Flux.direct$AbsTime, method = "monoH.FC")
  # This needs to be fixed, as interpolation of times between 23:00 and 0:00 causes wrong values

  # combine times and wavelengths for outdoor light and artificial lights, as they may have different wavelengths and time points
  joined.times <- c(Outdoor.Flux.direct[, 1], Artificial.Flux[, 1])
  joined.times <- unique(joined.times) # removes duplicate entries
  joined.times <- sort(joined.times)
  l <- length(joined.times)
  joined.wl <- c(names(Outdoor.Flux.direct[, 5:ncol(Outdoor.Flux.direct)]), names(Artificial.Flux[, 2:ncol(Artificial.Flux)]))
  joined.wl <- unique(joined.wl)
  joined.wl <- sort(joined.wl)
  n <- length(joined.wl)

  # creates output data structures
  Indoor.Flux <- as.data.frame(matrix(0, nrow = l, ncol = n + 2)) # data frame that will contain the wavelength-specific flux at different points in time
  Directflux.in <- as.data.frame(matrix(0, nrow = l, ncol = n + 2))
  Diffuseflux.in <- as.data.frame(matrix(0, nrow = l, ncol = n + 2))
  Indoor.Flux[, 1] <- Directflux.in[, 1] <- Diffuseflux.in[, 1] <- joined.times
  Indoor.Flux[, 2] <- Directflux.in[, 2] <- Diffuseflux.in[, 2] <- abs.time.interp(joined.times)
  colnames(Indoor.Flux) <- colnames(Directflux.in) <- colnames(Diffuseflux.in) <- c("Time", "AbsTime", joined.wl)

  outdoor.wavelengths <- as.numeric(substr(colnames(Outdoor.Flux.direct)[5:ncol(Outdoor.Flux.direct)], start = 2, stop = 8))
  indoor.wavelengths <- as.numeric(substr(colnames(Indoor.Flux)[3:(n + 2)], start = 2, stop = 8))
  artificial.wavelengths <- as.numeric(substr(colnames(Artificial.Flux)[2:ncol(Artificial.Flux)], start = 2, stop = 8))
  # gets wavelengths from names of columns in the flux data

  Dims <- Dimensions(windows, B)
  WGM <- match(windows$GlassType, glass[, 1]) # vector that indicates at which row of the glass transparency matrix each window corresponds

  # FIX: Extract wavelengths from column names (not glass[1,-1] which contains
  # transmittance values, not wavelengths). glass[1,-1] = all 1s for "None" glass
  # -> splinefun x-values all identical -> "length(m) must be at least two".
  # Column names like "100","200" are mangled to "X100","X200" by read.csv (check.names=TRUE).
  # Strip any leading non-numeric characters before converting to numeric.
  glass_wavelengths <- suppressWarnings(as.numeric(sub("^[^0-9.]+", "", names(glass)[-1])))
  glass_wavelengths_valid <- !is.na(glass_wavelengths)
  glass_wl <- glass_wavelengths[glass_wavelengths_valid]
  cat(sprintf("  Glass: %d wavelength points (%.0f-%.0f nm)\n",
              length(glass_wl), min(glass_wl), max(glass_wl)))


  # for each window, calculates direct and diffuse contributions
  # for (i in 1:nrow(windows)) {
  #   wavelength.transparency.interp <- splinefun(x = glass[1, -1], y = glass[WGM[i], -1], method = "monoH.FC")
  #   transparencies <- wavelength.transparency.interp(outdoor.wavelengths)
  #
  #   # Direct outdoor light to indoor
  #
  #   # angle corrections
  #   perp.corr <- 1 / sin(DtoR(Outdoor.Flux.direct$SolarElevationAngle)) # correction factor from TUV output on horizontal surface to perpendicular to the sun direction
  #   incid.corr <- perp.corr * (cos(DtoR(Outdoor.Flux.direct$SolarElevationAngle)) * sin(DtoR(90)) * cos(DtoR(windows$Orientation[i] - Outdoor.Flux.direct$SolarAzimuthAngle)) + sin(DtoR(Outdoor.Flux.direct$SolarElevationAngle)) * cos(DtoR(90)))
  #   # from https://www.pveducation.org/pvcdrom/properties-of-sunlight/arbitrary-orientation-and-tilt; 90 is from the windows being on vertical surfaces, so skylights are excluded here
  #   incid.corr[incid.corr < 0 | Outdoor.Flux.direct$SolarElevationAngle < 0] <- 0
  #   # this removes negative lighting corrections; and also the theoretical possibility of counting light coming in from the opposite side (facing in) of the window (though all TUV values were 0 with negative elevation anyways)
  #
  #   directflux.through <- t(t(Outdoor.Flux.direct[, 5:ncol(Outdoor.Flux.direct)]) * transparencies)
  #   directflux.through <- directflux.through * (1 - windows$ObstructedAreaFraction[i]) # reduces flux by fraction of window that is obstructed (curtains, nearby trees or structures)
  #   # this calculates the direct light coming through, if any. The transposed of the vector times the transposed matrix is so that each row of the flux matrix is multiplied by each row of the transparency vector
  #   if ((windows$Orientation[i] - B$OrientationWiderSide) %% 180 == 0) {
  #     room.depth <- Dims$Width
  #     room.width <- Dims$Length
  #   } else if ((windows$Orientation[i] - B$OrientationWiderSide) %% 180 == 90) {
  #     room.depth <- Dims$Length
  #     room.width <- Dims$Width
  #   } else {
  #     message("SIACS Warning: window ", i, " is at an angle to the building. The direct light contribution of this complex geometry cannot be calculated and will be ignored")
  #     room.depth <- 0
  #     room.width <- 0
  #   }
  #
  #   # This section calculates the indoor volume receiving direct light; see "Incident to indoor light.xls" for explanations and geometry calculations
  #   SurfaceTilt <- 90 # only vertical walls are assumed for now
  #   non_intersect.parllgm.vol <- Dims$W$wA[i] * room.depth
  #   alpha <- abs(Outdoor.Flux.direct$SolarAzimuthAngle - windows$Orientation[i]) # incident light angle to building, from above
  #   beta <- 90 - (SurfaceTilt - Outdoor.Flux.direct$SolarElevationAngle) # incident light angle to building, in cross section
  #   vol.planeL.intercept <- rep(NA, n)
  #   vol.planeL.intercept <- 1 / 2 * Dims$W$wH[i] * tan(DtoR(alpha)) * (room.depth - Dims$W$wL2[i] / tan(DtoR(alpha)))^2
  #   vol.planeL.intercept[alpha < RtoD(atan(Dims$W$wL2[i] / room.depth))] <- 0 # this is the case when all the window projection ends up on the opposite wall (horizontal angle)
  #   vol.planeL.intercept[alpha >= RtoD(atan((room.width - Dims$W$wL2[i]) / room.depth))] <- 1 / 2 * Dims$W$wH[i] * tan(DtoR(alpha[alpha >= RtoD(atan((room.width - Dims$W$wL2[i]) / room.depth))])) * ((room.depth - Dims$W$wL2[i] / tan(DtoR(alpha[alpha >= RtoD(atan((room.width - Dims$W$wL2[i]) / room.depth))])))^2 - (room.depth - (room.width - Dims$W$wL2[i]) / tan(DtoR(alpha[alpha >= RtoD(atan((room.width - Dims$W$wL2[i]) / room.depth))])))^2)
  #   # this is the case when the window projection ends up partly on the opposite wall and partly on the side wall
  #   vol.planeF.intercept <- rep(NA, n)
  #   vol.planeF.intercept <- 1 / 2 * Dims$W$wW[i] * tan(DtoR(beta)) * (room.depth - Dims$W$wH2[i] / tan(DtoR(beta)))^2
  #   vol.planeF.intercept[beta < RtoD(atan(Dims$W$wH2[i]))] <- 0
  #   vol.planeF.intercept[beta >= RtoD(atan((Dims$Height - Dims$W$wH2[i]) / room.depth))] <- 1 / 2 * Dims$W$wW[i] * tan(DtoR(beta[beta >= RtoD(atan((Dims$Height - Dims$W$wH2[i]) / room.depth))])) * ((room.depth - Dims$W$wH2[i] / tan(DtoR(beta[beta >= RtoD(atan((Dims$Height - Dims$W$wH2[i]) / room.depth))])))^2 - (room.depth - (Dims$Height - Dims$W$wH2[i]) / tan(DtoR(beta[beta >= RtoD(atan((Dims$Height - Dims$W$wH2[i]) / room.depth))])))^2)
  #
  #   DirectlyLitVolume <- non_intersect.parllgm.vol * (1 - vol.planeL.intercept / non_intersect.parllgm.vol) * (1 - vol.planeF.intercept / non_intersect.parllgm.vol)
  #   DirectlyLitVolume[DirectlyLitVolume < 0] <- 0 # this eliminates negative values from trigonometric functions
  #   DirectlyLitVolume[Outdoor.Flux.direct$SolarElevationAngle < windows$HorizonElevationAngle[i]] <- 0 # this eliminates values when the sun is below the (obstructed) horizon
  #   DirectlyLitVolume[Outdoor.Flux.direct$SolarAzimuthAngle < windows$Orientation[i] - 90 | Outdoor.Flux.direct$SolarAzimuthAngle > windows$Orientation[i] + 90] <- 0
  #   # this eliminates light coming in from the "inside" side a window
  #   DirectFluxFraction <- DirectlyLitVolume / Dims$V
  #   directflux.in <- (apply(directflux.through, 2, "*", DirectFluxFraction)) # this performs a scalar multiplication of  each row of the through flux for the corresponding value in the DirectFluxFraction vector
  #
  #   M <- match(colnames(Indoor.Flux), colnames(directflux.in))
  #   M2 <- match(Indoor.Flux$Time, Outdoor.Flux.direct$Time)
  #   Indoor.Flux[!is.na(M2), !is.na(M)] <- directflux.in[M2[!is.na(M2)], M[!is.na(M)]] + Indoor.Flux[!is.na(M2), !is.na(M)]
  #   Directflux.in[!is.na(M2), !is.na(M)] <- directflux.in[M2[!is.na(M2)], M[!is.na(M)]] + Directflux.in[!is.na(M2), !is.na(M)]
  #   # Updates the indoor spectra with this window's direct light contribution
  #   # the row and coulmn indices are to assign values only to times and wavelengths that exist in the outdoor data
  #
  #   # Diffuse outdoor light to indoor, based on Building Research Establishment (BRE) formula (https://cmadeubi.files.wordpress.com/2016/10/jcarlos-jgb-paper.pdf)
  #   # also https://www.new-learn.info/packages/clear/visual/daylight/analysis/hand/daylight_factor.html
  #   # DF formula modified as glass tranmittance is already calculated elsewhere and reflectance is calculated for total light
  #   diffuseflux.through <- t(t(Outdoor.Flux.diffuse[, 5:ncol(Outdoor.Flux.diffuse)]) * transparencies)
  #   diffuseflux.through <- diffuseflux.through * (1 - windows$ObstructedAreaFraction[i]) # reduces flux by fraction of window that is obstructed (curtains, nearby trees or structures)
  #   teta <- 90 - windows$HorizonElevationAngle[i] # angle of sky visible from center of window (but this works only for ground floor)
  #   DF.BRE <- (Dims$W$wA[i] * teta * (1 - windows$ObstructedAreaFraction[i])) / (Dims$SurfacesArea) / 100
  #   diffuseflux.in <- diffuseflux.through * DF.BRE
  #   Indoor.Flux[!is.na(M2), !is.na(M)] <- diffuseflux.in[M2[!is.na(M2)], M[!is.na(M)]] + Indoor.Flux[!is.na(M2), !is.na(M)]
  #   Diffuseflux.in[!is.na(M2), !is.na(M)] <- diffuseflux.in[M2[!is.na(M2)], M[!is.na(M)]] + Diffuseflux.in[!is.na(M2), !is.na(M)]
  #   # Updates the indoor spectra with this window's diffuse light contribution
  #   # the row and coulmn indices are to assign values only to times and wavelengths that exist in the outdoor data
  # } # for (i in 1:nrow(windows))

  ####
  # Vectorized implementation using lapply
  # FIX: The previous version returned one full Indoor.Flux copy per window and
  # combined with do.call(rbind,...), stacking rows instead of adding flux values.
  # With 3 windows this tripled the row count (117 → 351 rows), making W2
  # (rows not in outdoor data) enormous and leaving too few matched rows for
  # splinefun (needs ≥2) → "length(m) must be at least two".
  #
  # Fix: each window returns zero-initialised delta frames for its contribution
  # only; these are then accumulated additively — matching the original += logic.
  # Window loop — written as an explicit for loop so each line can be run
  # and inspected independently (set i <- 1 before stepping through).
  for (i in seq_len(nrow(windows))) {

    IF_delta <- as.data.frame(matrix(0, nrow = l, ncol = n + 2))
    DF_delta <- as.data.frame(matrix(0, nrow = l, ncol = n + 2))
    Di_delta <- as.data.frame(matrix(0, nrow = l, ncol = n + 2))
    colnames(IF_delta) <- colnames(DF_delta) <- colnames(Di_delta) <- colnames(Indoor.Flux)

    # ── Glass transmittance ────────────────────────────────────────────────
    # x-axis: glass_wl (wavelengths from column names, pre-computed above).
    # y-axis: transmittance values for this window's glass type.
    # When WGM[i] is NA (GlassType="None" or not found), use full transparency.
    if (!is.na(WGM[i])) {
      glass_trans_vals <- as.numeric(glass[WGM[i], -1])[glass_wavelengths_valid]
      wavelength.transparency.interp <- splinefun(x = glass_wl, y = glass_trans_vals, method = "monoH.FC")
      transparencies <- pmax(pmin(wavelength.transparency.interp(outdoor.wavelengths), 1), 0)
    } else {
      transparencies <- rep(1.0, length(outdoor.wavelengths))  # no glass = fully transparent
    }

    # ── Direct outdoor light ───────────────────────────────────────────────
    perp.corr  <- 1 / sin(DtoR(Outdoor.Flux.direct$SolarElevationAngle))
    incid.corr <- perp.corr * (
      cos(DtoR(Outdoor.Flux.direct$SolarElevationAngle)) * sin(DtoR(90)) *
        cos(DtoR(windows$Orientation[i] - Outdoor.Flux.direct$SolarAzimuthAngle)) +
      sin(DtoR(Outdoor.Flux.direct$SolarElevationAngle)) * cos(DtoR(90))
    )
    incid.corr[incid.corr < 0 | Outdoor.Flux.direct$SolarElevationAngle < 0] <- 0

    directflux.through <- t(t(Outdoor.Flux.direct[, 5:ncol(Outdoor.Flux.direct)]) * transparencies)
    directflux.through <- directflux.through * (1 - windows$ObstructedAreaFraction[i])

    if ((windows$Orientation[i] - B$OrientationWiderSide) %% 180 == 0) {
      room.depth <- Dims$Width
      room.width <- Dims$Length
    } else if ((windows$Orientation[i] - B$OrientationWiderSide) %% 180 == 90) {
      room.depth <- Dims$Length
      room.width <- Dims$Width
    } else {
      message("SIACS Warning: window ", i, " is at an angle to the building. ",
              "Direct light contribution cannot be calculated and will be ignored.")
      room.depth <- 0
      room.width <- 0
    }

    SurfaceTilt            <- 90
    non_intersect.parllgm.vol <- Dims$W$wA[i] * room.depth
    alpha <- abs(Outdoor.Flux.direct$SolarAzimuthAngle - windows$Orientation[i])
    beta  <- 90 - (SurfaceTilt - Outdoor.Flux.direct$SolarElevationAngle)

    # ── vol.planeL.intercept ───────────────────────────────────────────────
    # Base formula (valid where tan(alpha) > 0 and projection fits the room)
    vol.planeL.intercept <- 1/2 * Dims$W$wH[i] *
      tan(DtoR(alpha)) * (room.depth - Dims$W$wL2[i] / tan(DtoR(alpha)))^2

    # Case 1: horizontal angle so small that all projection hits opposite wall
    mask_L0 <- !is.na(alpha) & alpha < RtoD(atan(Dims$W$wL2[i] / room.depth))
    vol.planeL.intercept[mask_L0] <- 0

    # Case 2: projection spills onto the side wall as well
    thresh_L <- RtoD(atan((room.width - Dims$W$wL2[i]) / room.depth))
    mask_L2  <- !is.na(alpha) & alpha >= thresh_L
    if (any(mask_L2)) {
      a2  <- alpha[mask_L2]
      vol.planeL.intercept[mask_L2] <- 1/2 * Dims$W$wH[i] * tan(DtoR(a2)) *
        ((room.depth - Dims$W$wL2[i] / tan(DtoR(a2)))^2 -
         (room.depth - (room.width - Dims$W$wL2[i]) / tan(DtoR(a2)))^2)
    }

    # ── vol.planeF.intercept ───────────────────────────────────────────────
    vol.planeF.intercept <- 1/2 * Dims$W$wW[i] *
      tan(DtoR(beta)) * (room.depth - Dims$W$wH2[i] / tan(DtoR(beta)))^2

    mask_F0 <- !is.na(beta) & beta < RtoD(atan(Dims$W$wH2[i]))
    vol.planeF.intercept[mask_F0] <- 0

    thresh_F <- RtoD(atan((Dims$Height - Dims$W$wH2[i]) / room.depth))
    mask_F2  <- !is.na(beta) & beta >= thresh_F
    if (any(mask_F2)) {
      b2 <- beta[mask_F2]
      vol.planeF.intercept[mask_F2] <- 1/2 * Dims$W$wW[i] * tan(DtoR(b2)) *
        ((room.depth - Dims$W$wH2[i] / tan(DtoR(b2)))^2 -
         (room.depth - (Dims$Height - Dims$W$wH2[i]) / tan(DtoR(b2)))^2)
    }

    # ── Directly lit volume fraction ───────────────────────────────────────
    # Guard against divide-by-zero when room.depth = 0 (angled window case)
    if (non_intersect.parllgm.vol > 0) {
      DirectlyLitVolume <- non_intersect.parllgm.vol *
        (1 - vol.planeL.intercept / non_intersect.parllgm.vol) *
        (1 - vol.planeF.intercept / non_intersect.parllgm.vol)
    } else {
      DirectlyLitVolume <- rep(0, nrow(Outdoor.Flux.direct))
    }
    DirectlyLitVolume[DirectlyLitVolume < 0] <- 0
    DirectlyLitVolume[Outdoor.Flux.direct$SolarElevationAngle < windows$HorizonElevationAngle[i]] <- 0
    DirectlyLitVolume[
      Outdoor.Flux.direct$SolarAzimuthAngle < windows$Orientation[i] - 90 |
      Outdoor.Flux.direct$SolarAzimuthAngle > windows$Orientation[i] + 90
    ] <- 0

    DirectFluxFraction <- DirectlyLitVolume / Dims$V
    directflux.in      <- apply(directflux.through, 2, "*", DirectFluxFraction)

    M  <- match(colnames(IF_delta), colnames(directflux.in))
    M2 <- match(IF_delta$Time,      Outdoor.Flux.direct$Time)
    IF_delta[!is.na(M2), !is.na(M)] <- directflux.in[M2[!is.na(M2)], M[!is.na(M)]]
    DF_delta[!is.na(M2), !is.na(M)] <- directflux.in[M2[!is.na(M2)], M[!is.na(M)]]

    # ── Diffuse outdoor light ──────────────────────────────────────────────
    diffuseflux.through <- t(t(Outdoor.Flux.diffuse[, 5:ncol(Outdoor.Flux.diffuse)]) * transparencies)
    diffuseflux.through <- diffuseflux.through * (1 - windows$ObstructedAreaFraction[i])
    teta    <- 90 - windows$HorizonElevationAngle[i]
    DF.BRE  <- (Dims$W$wA[i] * teta * (1 - windows$ObstructedAreaFraction[i])) / Dims$SurfacesArea / 100
    diffuseflux.in <- diffuseflux.through * DF.BRE
    IF_delta[!is.na(M2), !is.na(M)] <- IF_delta[!is.na(M2), !is.na(M)] +
                                         diffuseflux.in[M2[!is.na(M2)], M[!is.na(M)]]
    Di_delta[!is.na(M2), !is.na(M)] <- diffuseflux.in[M2[!is.na(M2)], M[!is.na(M)]]

    # ── Accumulate this window's contribution into Indoor.Flux ────────────
    Indoor.Flux[,    3:(n+2)] <- Indoor.Flux[,    3:(n+2)] + IF_delta[, 3:(n+2)]
    Directflux.in[,  3:(n+2)] <- Directflux.in[,  3:(n+2)] + DF_delta[, 3:(n+2)]
    Diffuseflux.in[, 3:(n+2)] <- Diffuseflux.in[, 3:(n+2)] + Di_delta[, 3:(n+2)]

  } # end for (i in seq_len(nrow(windows)))
  ####





  # This section interpolates the missing values of light from outside for times and wavelengths that were not in the outdoor data
  W <- which(is.na(match(colnames(Indoor.Flux), colnames(Outdoor.Flux.direct)))) # columns not in the outdoor flux data
  W2 <- which(is.na(match(Indoor.Flux$Time, Outdoor.Flux.direct$Time))) # rows not in the outdoor flux data
  h <- 3:ncol(Indoor.Flux)
  h <- setdiff(h, W) # column numbers that exist in both Outdoor and Indoor spectra

  # Interpolate outdoor wavelengths to artificial-light timepoints (W2 rows).
  # Guard: only run if there are both missing timepoints (W2) and shared wavelengths (h).
  if (length(W2) > 0 && length(h) > 0) {
    for (j in seq_len(length(h))) {
      Indoor.Flux[W2, h[j]]    <- approx(x = Indoor.Flux$Time[-W2], y = Indoor.Flux[-W2, h[j]],    xout = Indoor.Flux$Time[W2], method = "linear", rule = 2)$y
      Directflux.in[W2, h[j]]  <- approx(x = Indoor.Flux$Time[-W2], y = Directflux.in[-W2, h[j]],  xout = Indoor.Flux$Time[W2], method = "linear", rule = 2)$y
      Diffuseflux.in[W2, h[j]] <- approx(x = Indoor.Flux$Time[-W2], y = Diffuseflux.in[-W2, h[j]], xout = Indoor.Flux$Time[W2], method = "linear", rule = 2)$y
    }
  }

  # Interpolate artificial-only wavelengths from outdoor wavelength values.
  # Guard: only run this block if W is non-empty AND there are enough outdoor
  # wavelengths (not in W) to fit a spline (need at least 2 knots).
  outdoor_wl_indices <- setdiff(3:ncol(Indoor.Flux), W)  # column indices of outdoor wavelengths
  x_knots <- indoor.wavelengths[outdoor_wl_indices - 2]  # wavelength values for spline knots

  cat(sprintf("  Wavelength interpolation: %d outdoor wl, %d artificial-only wl\n",
              length(x_knots), length(W)))

  if (length(W) > 0 && length(x_knots) >= 2) {
    Indoor.Flux[, W] <- apply(Indoor.Flux, 1, function(row) {
      rowinterp <- splinefun(x = x_knots, y = row[outdoor_wl_indices], method = "monoH.FC")
      rowinterp(indoor.wavelengths[W - 2])
    })
    Directflux.in[, W] <- apply(Directflux.in, 1, function(row) {
      rowinterp.dir <- splinefun(x = x_knots, y = row[outdoor_wl_indices], method = "monoH.FC")
      rowinterp.dir(indoor.wavelengths[W - 2])
    })
    Diffuseflux.in[, W] <- apply(Diffuseflux.in, 1, function(row) {
      rowinterp.dif <- splinefun(x = x_knots, y = row[outdoor_wl_indices], method = "monoH.FC")
      rowinterp.dif(indoor.wavelengths[W - 2])
    })
  } else if (length(W) > 0 && length(x_knots) < 2) {
    # Not enough outdoor knots for a spline — use nearest-neighbour fill (approx)
    cat("  WARNING: too few outdoor wavelengths for spline; using linear approx for artificial-only columns.\n")
    for (wc in W) {
      Indoor.Flux[, wc]    <- approx(x = x_knots, y = Indoor.Flux[1, outdoor_wl_indices],    xout = indoor.wavelengths[wc - 2], rule = 2)$y
      Directflux.in[, wc]  <- approx(x = x_knots, y = Directflux.in[1, outdoor_wl_indices],  xout = indoor.wavelengths[wc - 2], rule = 2)$y
      Diffuseflux.in[, wc] <- approx(x = x_knots, y = Diffuseflux.in[1, outdoor_wl_indices], xout = indoor.wavelengths[wc - 2], rule = 2)$y
    }
  }
  ####


  # This section adds the artificial light to the indoor flux
  # The values of the (sparser) artificial light flux are interpolated, first for the additional wavelengths and then for all timepoints
  # CODING: this seems awkward.Is there a better way without for cycles? Problem is splinefun() does not accept 2D vectors as arguments
  Artificial.Flux.add <- as.data.frame(matrix(0, nrow = l, ncol = n)) # defines the conforming matrix of artificial light flux that will be added to Indoor.Flux
  colnames(Artificial.Flux.add) <- joined.wl
  AiIR <- which(!is.na(match(Indoor.Flux$Time, Artificial.Flux$Time))) # list of rows in Indoor.Flux that exist in Artificial.Flux
  # for (j in 1:nrow(Artificial.Flux)) {
  #   #interpolates first the values at timepoints defined in Artificial.Flux for all wavelengths in Indoor.Flux
  #   AF.rowinterp <- splinefun(x=artificial.wavelengths, y=Artificial.Flux[j,2:ncol(Artificial.Flux)], method ="monoH.FC")
  #   Artificial.Flux.add[AiIR[j],] <- AF.rowinterp(indoor.wavelengths)
  # }


  # Step 1: Interpolate Artificial.Flux wavelengths to match indoor wavelengths,
  # for the timepoints that exist in Artificial.Flux.
  # apply() over rows (timepoints) returns n_ArtifRows x n_indoor_wl matrix.
  AF.interp.rows <- apply(Artificial.Flux, 1, function(row) {
    AF.rowinterp <- splinefun(x = artificial.wavelengths, y = row[2:ncol(Artificial.Flux)], method = "monoH.FC")
    AF.rowinterp(indoor.wavelengths)
  })
  # Result is n_indoor_wl x n_ArtifRows — transpose to n_ArtifRows x n_indoor_wl
  AF.interp.rows <- t(AF.interp.rows)

  # Step 2: Place these interpolated values into the correct rows of Artificial.Flux.add
  # (rows indexed by AiIR — the Indoor.Flux row positions that match Artificial.Flux times).
  # Artificial.Flux.add is already initialised as l x n zeros above.
  Artificial.Flux.add[AiIR, ] <- AF.interp.rows
  Artificial.Flux.add[Artificial.Flux.add < 0] <- 0

  # Step 3: For all Indoor.Flux timepoints NOT in Artificial.Flux (-AiIR),
  # interpolate each wavelength column using the known AiIR timepoints.
  # Only do this if there are missing timepoints to fill.
  if (length(AiIR) < l) {
    for (j in seq_len(ncol(Artificial.Flux.add))) {
      AF.colinterp <- splinefun(
        x = Indoor.Flux$Time[AiIR],
        y = Artificial.Flux.add[AiIR, j],
        method = "monoH.FC"
      )
      Artificial.Flux.add[-AiIR, j] <- AF.colinterp(Indoor.Flux$Time[-AiIR])
    }
    Artificial.Flux.add[Artificial.Flux.add < 0] <- 0
  }

  Indoor.Flux[, 3:(n + 2)] <- Indoor.Flux[, 3:(n + 2)] + Artificial.Flux.add

  # This section adds indoor reflections to the indoor flux (of outdoro or artificial origin)
  Refl.Data <- data.frame(x = c(200, 300, 400, 700, 1000), y = c(0.05, 0.05, B$IndoorReflectance, B$IndoorReflectance, 0.7))
  # Reflectance varies by wavelength as materials absorb more UV than visible or IR
  # USGS spectral library https://crustal.usgs.gov/speclab/QueryAll07a.php A lot of materials start from 5% in the UV and increase to 70% by 1000 nm
  Refl.interp <- splinefun(x = Refl.Data$x, y = Refl.Data$y, method = "monoH.FC")
  ReflFactors <- LightReflectionFactor(Refl.interp(indoor.wavelengths), sum(Dims$W$wA) / Dims$SurfacesArea) # multiplier of flux based on reflected light
  Indoor.Flux[, 3:(n + 2)] <- t(t(Indoor.Flux[, 3:(n + 2)]) * ReflFactors) #
  # this updates the indoor light flux with the indoor-reflected flux. The transposed of the vector times the transposed matrix is so that each row of the flux matrix is multiplied by each row of the reflectance vector

  # Total light energy flux
  energy.conv <- matrix(NA, ncol = 3, nrow = n)
  energy.flux <- matrix(NA, ncol = 2, nrow = l)
  energy.conv[, 1] <- as.numeric(substr(joined.wl, start = 2, stop = 8))
  energy.conv[1:(n - 1), 2] <- energy.conv[2:n, 1] - energy.conv[1:(n - 1), 1] # calculates width of wavelength intervals
  energy.conv[n, 2] <- energy.conv[n - 1, 2] # last element assumed as wide as previous one
  c <- 2.9979E+08 # speed of light (m/s)
  h <- 6.6261E-34 # Planck constant (J s)
  energy.conv[, 3] <- h * c / (energy.conv[, 1] * 1E-9) * energy.conv[, 2] # E = hc/lambda ; 1E-9 for nm -> m
  energy.flux[, 1] <- Indoor.Flux$Time

  # Vectorized implementation using apply
  energy.flux[, 2] <- apply(Indoor.Flux, 1, function(row) {
    sum(row[3:(n + 2)] * energy.conv[, 3]) * 1000
  })

  IF <- list(Total = Indoor.Flux, Direct = Directflux.in, Diffuse = Diffuseflux.in, Artificial = cbind(Indoor.Flux[, 1:2], Artificial.Flux.add), EnergyFlux = energy.flux)
  return(IF)
} # IndoorLightFlux

OutdoorLightFlux <- function(time.read, spectrum.type, comptime0) {
  # returns a matrix of outdoor actinic fluxes from TUV, in quanta cm-2 s-1 nm-1. First row is beginning of wavelength intervals; last wavelength interval is 725-735 nm
  hi <- 4 # number of interval per hour for TUV calculations; 4 means every 15 min; to optimize calculation, but won't be a user selection
  if (spectrum.type != "direct") spectrum.type <- "diffuse"
  cat("Calculating", spectrum.type, "outdoor light with TUV program...\n")

  # computing dates and times from input data
  y <- as.character(time.read$StartTimeYear)
  m <- as.character(time.read$StartTimeMonth)
  d <- as.character(time.read$StartTimeDay) # absolute start times: year month and day
  ti <- as.character(time.read$StartTime) # absolute start time of day
  t <- paste(y, m, d, sep = "-") # connecting date
  t2 <- paste(t, ti) # connecting time with date for start time
  standard <- as.character(time.read$StartTimeStandard) # timezone provided by user
  UTC <- as.POSIXlt(t2, tz = "UTC") # establishing UTC timezone with start time
  timezone_start <- as.POSIXlt(t2, tz = standard) # establishing input timezone for start time
  timedif <- as.numeric(difftime(UTC, timezone_start, units = "hours")) # UTC offset in hours
  start.dec <- Hour.Dec(time.read$StartTime)
  n.days <- ceiling((start.dec + time.read$Duration) / 24) # how many different days will need to be called with TUV, which can only be called for a single date
  timezone_end <- timezone_start + 3600 * time.read$Duration # calculates date and time of end of simulation
  ti_end.str <- as.character(timezone_end)
  end.dec <- Hour.Dec(substr(ti_end.str, nchar(ti_end.str) - 8 + 1, nchar(ti_end.str)))

  Outdoor.Flux <- matrix(0, nrow = time.read$Duration * hi + 1 + 1, ncol = 159) # data frame that will contain the wavelength-specific flux at different points in time; +1 for timepoints and +1 to store wavelengths numerically
  k <- 2 # counter to fill results matrix

  # Calls TUV for the required number of days, then adds daily flux to flux for entire simulation period
  # for(i in 1:n.days) {
  #   comptime0 <- Sys.time()
  #   whichday <- as.POSIXlt(as.POSIXlt(t)+(i-1)*3600*24)
  #   starthr <- if(i==1) start.dec else 0 # starting time for first day, midnight for all other days
  #   endhr <- if(i==n.days) end.dec else 23+(hi-1)/hi #ending time for last day, 23:00 or later for all other days
  #   intv <- as.integer((endhr-starthr)*hi) + 1 #Number of time steps (nt) must be a whole number, no decimals; if this is not a whole number, TUV will not run
  #
  #   daily <- TUV.call(whichday$year + 1900,whichday$mon + 1,whichday$mday,starthr,endhr,timedif,spectrum.type,intv) #POSIXlt stores months counting from 0 and years from 1900
  #   if(i==1) {
  #     Outdoor.Flux[1:(intv+1),] <- daily
  #   } else {
  #     daily <- daily[-1,]
  #     Outdoor.Flux[k:(k+intv-1),] <- daily
  #   }
  #   k <- k+intv
  #   comptime1 <- Sys.time()
  #   PrintComputationTime("Time calculating outdoor light flux for day ", comptime0, comptime1, i, n.days)
  #
  # }# for(i in 1:n.days)

  #### vetorized version
  # Precompute whichday, starthr, endhr, and intv for all days
  whichday <- as.POSIXlt(as.POSIXlt(t) + (0:(n.days - 1)) * 3600 * 24)
  starthr <- c(start.dec, rep(0, n.days - 1))
  endhr <- c(rep(23 + (hi - 1) / hi, n.days - 1), end.dec)
  intv <- as.integer((endhr - starthr) * hi) + 1

  # Initialize Outdoor.Flux
  # FIX A: Remove the dummy TUV.call(0,0,0,...) that was used only to get ncol.
  # It fired a real TUV process with garbage inputs before the loop even started,
  # crashing in background Rscript sessions. TUV always returns 159 columns.
  #
  # FIX B: TUV.call returns intv[i]+1 rows (1 wavelength-header row + intv[i]
  # data rows). The loop writes day 1 into rows 1:(intv[1]+1) and day 2 into
  # rows (intv[1]+1):(intv[1]+intv[2]), so the matrix needs sum(intv)+1 rows.
  # The original sum(intv) allocation was one row short, causing the last
  # time-step to be missing and "Data on light not supplied for the entire
  # duration of the simulation" in IndoorLightFlux.
  Outdoor.Flux <- matrix(NA, nrow = sum(intv) + 1, ncol = 159)

  # Vectorized computation
  k <- 1
  comptime0 <- Sys.time()
  for (i in 1:n.days) {
    daily <- TUV.call(whichday[i]$year + 1900, whichday[i]$mon + 1, whichday[i]$mday, starthr[i], endhr[i], timedif, spectrum.type, intv[i], B)
    if (i == 1) {
      Outdoor.Flux[1:(intv[i] + 1), ] <- daily
    } else {
      daily <- daily[-1, ]
      Outdoor.Flux[k:(k + intv[i] - 1), ] <- daily
    }
    k <- k + intv[i]
  }
  comptime1 <- Sys.time()
  PrintComputationTime("Time calculating outdoor light flux for all days", comptime0, comptime1, 1, n.days)
  ####

  # Replaces midpoints of wavelength intervals with beginning points of the interval
  midpoint <- Outdoor.Flux[1, 4]
  Outdoor.Flux[1, 4] <- 120
  # for(i in 5:159) {
  #   nextmidpoint <- Outdoor.Flux[1,i]
  #   Outdoor.Flux[1,i] <- 2*midpoint-Outdoor.Flux[1,i-1] #easier to understand as (midpoint-Outdoor.Flux[1,i-1])*2 + Outdoor.Flux[1,i-1]
  #   midpoint <- nextmidpoint
  # }
  ####
  midpoints <- Outdoor.Flux[1, 5:159]
  Outdoor.Flux[1, 5:159] <- 2 * c(midpoint, midpoints[-length(midpoints)]) - Outdoor.Flux[1, 4:158]
  ####

  # changes first row to column names, adds other names, and relative time
  fr <- Outdoor.Flux[1, ]
  lt <- rep("x", length(fr))
  fr <- paste0(lt, fr) # this is needed as variable names cannot start with a number
  fr[1] <- "AbsTime"
  fr[2] <- "SolarElevationAngle"
  fr[3] <- "SolarAzimuthAngle"
  cnames <- colnames(Outdoor.Flux) <- fr
  Outdoor.Flux <- Outdoor.Flux[-1, ]
  rt <- seq(from = time.read$RelativeStartTime, by = 60 / hi, along.with = Outdoor.Flux[, 1])
  Outdoor.Flux <- cbind(rt, Outdoor.Flux)
  colnames(Outdoor.Flux) <- c("Time", cnames)

  return(Outdoor.Flux)
  # The matrix returned has angles in degrees, wavelengths in nm, Time in min from start, absolute time in 24hr decimal format, flux values in quanta cm-2 s-1
  # azimuth angle is CW from North
} # OutdoorLightFlux

Hour.Dec <- function(x) {
  sum(unlist(lapply(strsplit(as.character(x), split = ":"), as.numeric)) * c(1, 1 / 60, 1 / 3600)) # converts a hh:mm:ss time to decimal
} # Hour.Dec

# ------------------------------------------------------------------------------
# PER-WORKER TUV SANDBOX
# ------------------------------------------------------------------------------
# TUV writes its output to <tuv_root>/SIACS.txt and reads auxiliary data from
# <tuv_root>/tuv/ (DATAE1, DATAJ1, etc.). When SIACS.batch runs in parallel
# mode, every worker calling TUV in the same on-disk directory would race on
# SIACS.txt — producing "no lines available in input" errors.
#
# Fix: the first TUV call on each worker copies tuv5.3.1.exe/ to
# <tempdir>/tuv_sandbox_<pid>/ once and caches the path in .GlobalEnv. All
# subsequent TUV calls on that worker read/write inside the sandbox, and each
# PSOCK worker therefore has its own SIACS.txt.
#
# tempdir() is session-scoped, so the sandbox is cleaned automatically when the
# worker R process exits at the end of the batch.
tuv_sandbox_dir <- function() {
  # Return cached sandbox if valid
  if (exists(".TUV_SANDBOX_DIR", envir = .GlobalEnv)) {
    cached <- get(".TUV_SANDBOX_DIR", envir = .GlobalEnv)
    if (is.character(cached) && length(cached) == 1L &&
        dir.exists(cached) &&
        file.exists(file.path(cached, "tuv", "tuv.exe"))) {
      return(cached)
    }
  }

  # Resolve source TUV installation. Workers inherit the parent R process cwd
  # (project root) via PSOCK, but fall back to SIACS_ROOT if it was exported.
  siacs_root <- if (exists("SIACS_ROOT", envir = .GlobalEnv) &&
                    is.character(get("SIACS_ROOT", envir = .GlobalEnv))) {
    get("SIACS_ROOT", envir = .GlobalEnv)
  } else {
    getwd()
  }
  src <- file.path(siacs_root, "tuv5.3.1.exe")
  if (!dir.exists(src)) {
    stop("TUV installation not found at: ", src,
         "\nSIACS_ROOT must point to the directory containing tuv5.3.1.exe/.")
  }

  # Create per-worker destination (PID makes it unique across PSOCK workers)
  dst <- file.path(tempdir(), paste0("tuv_sandbox_", Sys.getpid()))
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    # Copy every child of src into dst. file.copy with recursive=TRUE and a
    # directory argument copies the dir contents into dst.
    children <- list.files(src, full.names = TRUE, include.dirs = TRUE,
                           all.files = TRUE, no.. = TRUE)
    file.copy(from = children, to = dst, recursive = TRUE, copy.date = TRUE)
  }

  # Sanity check the copy succeeded
  if (!file.exists(file.path(dst, "tuv", "tuv.exe"))) {
    stop("TUV sandbox copy incomplete — missing tuv/tuv.exe at: ", dst)
  }

  assign(".TUV_SANDBOX_DIR", dst, envir = .GlobalEnv)
  cat(sprintf("[TUV] Sandbox ready (pid %d): %s\n", Sys.getpid(), dst))
  dst
}

TUV.call <- function(year, month, day, starthour, endhour, time_offset, spect_type, intervals, B) {
  # TUV program will calculate spectra only within a single date. For time spans longer than a day, multiple calls are needed

  date <- paste(year, month, day, sep = "-")

  # creating command text
  cmdtxt <- paste(c(
    "iyear\n", year, "\nimonth\n", month, "\niday\n", day,
    "\nlat\n", B$Latitude, "\nlon\n", B$Longitude, "\nzout\n", B$Altitude / 1000, "\nzstart\n", B$Altitude / 1000,
    "\nalsurf\n", B$SurfaceAlbedo, "\ntaucld\n", B$CloudOpticalDepth, "\nzbase\n", B$CloudBase, "\nztop\n", B$CloudTop,
    "\ntmzone\n", time_offset, "\nnt\n", intervals, "\ntstop\n", endhour, "\ntstart\n", starthour, "\n"
  ), collapse = "")
  # altitude units conversion from m(asl) to km(asl)

  if (spect_type == "direct") {
    cmdtxt <- paste(c("\n2\noutfil\nSIACS\n", cmdtxt, "dirsun\n1\ndifdn\n0\ndifup\n0\n\n\n"), collapse = "")
  } else {
    cmdtxt <- paste(c("\n2\noutfil\nSIACS\n", cmdtxt, "dirsun\n0\ndifdn\n1\ndifup\n1\n\n\n"), collapse = "")
  } # if(spect_type
  # else {cmdtxt <- paste(c("\n2\noutfil\nSIACS\n",cmdtxt,"dirsun\n0\ndifdn\n1\ndifup\n0\n\n\n"),collapse ="")

  # running external TUV program
  # FIX: stdout = FALSE in a background Rscript child process passes TUV the
  # same stdout fd that the parent GUI reads via proc$read_output(), corrupting
  # the log stream. TUV writes results to SIACS.txt via internal Fortran I/O,
  # so stdout here is only incidental status text. Redirect both streams to
  # temp files to isolate them from the R process pipes entirely.
  #
  # FIX (parallel safety): run TUV inside a per-worker sandbox. Each PSOCK
  # worker gets its own copy of tuv5.3.1.exe/ in tempdir() so SIACS.txt is
  # private to the worker and parallel calls never race.
  sandbox_root  <- tuv_sandbox_dir()
  tuv_work_dir  <- file.path(sandbox_root, "tuv")
  siacs_out_file <- file.path(sandbox_root, "SIACS.txt")

  oldwd <- getwd()
  setwd(tuv_work_dir) # need to set working space so TUV finds DATAE1/ etc.
  tuv_stdout <- tempfile(fileext = ".txt")
  tuv_stderr <- tempfile(fileext = ".txt")
  system2(command = "tuv", input = cmdtxt, wait = TRUE,
          stdout = tuv_stdout, stderr = tuv_stderr)
  setwd(oldwd)

  # reading results from the per-worker sandbox output
  Headers <- read.delim(siacs_out_file, header = FALSE, skip = 3, nrows = 2, sep = "")
  Headers <- t(Headers[, -1]) # skips first column and transposes
  Headers <- matrix(suppressWarnings(as.numeric(Headers)), ncol = ncol(Headers)) # converting text matrix to a numeric one
  spectrum <- t(read.delim(siacs_out_file, header = FALSE, skip = 5, nrow = 156, sep = ""))

  # calculating azimuth angle
  # based on NOAA's https://gml.noaa.gov/grad/solcalc/ https://gml.noaa.gov/grad/solcalc/calcdetails.html NOAA_Solar_Calculations_day.xls
  if (spect_type == "direct") {
    POSIXStart <- as.POSIXlt("1899-12-29 23:00") # this odd starting point is needed to match NOAA's calculation for Julian day
    timeDatelt <- as.POSIXlt(date)
    julian.century <- as.numeric(((timeDatelt - POSIXStart + 2415018.5 + as.numeric(Headers[-1, 1]) / 24 - time_offset / 24) - 2451545) / 36525)
    gmls <- (280.46646 + julian.century * (36000.76983 + julian.century * 0.0003032)) %% 360 # geom mean long Sun (deg)
    gmas <- 357.52911 + julian.century * (35999.05029 - 0.0001537 * julian.century) # Geom Mean Anom Sun (deg)
    ecc <- 0.016708634 - julian.century * (0.000042037 + 0.0000001267 * julian.century) # Eccent Earth Orbit
    seqc <- sin(DtoR(gmas)) * (1.914602 - julian.century * (0.004817 + 0.000014 * julian.century)) + sin(DtoR(2 * gmas)) * (0.019993 - 0.000101 * julian.century) + sin(DtoR(3 * gmas)) * 0.000289 # Sun Eq of Ctr
    stl <- gmls + seqc # Sun True Long (deg)
    # sta <- gmas + seqc #Sun True Anom (deg)
    sal <- stl - 0.00569 - 0.00478 * sin(DtoR(125.04 - 1934.136 * julian.century)) # Sun App Long (deg)
    obliq.corr <- (23 + (26 + ((21.448 - julian.century * (46.815 + julian.century * (0.00059 - julian.century * 0.001813)))) / 60) / 60) + 0.00256 * cos(DtoR(125.04 - 1934.136 * julian.century)) # Obliq Corr (deg)
    sun.declin <- RtoD(asin(sin(DtoR(obliq.corr)) * sin(DtoR(sal)))) # Sun Declin (deg)
    var.y <- (tan(DtoR(obliq.corr / 2)))^2
    eq.time <- 4 * RtoD(var.y * sin(2 * DtoR(gmls)) - 2 * ecc * sin(DtoR(gmas)) + 4 * ecc * var.y * sin(DtoR(gmas)) * cos(2 * DtoR(gmls)) - 0.5 * (var.y)^2 * sin(4 * DtoR(gmls)) - 1.25 * (ecc)^2 * sin(2 * DtoR(gmas))) # Eq of Time (minutes)
    tst <- (as.numeric(Headers[-1, 1]) * 60 + eq.time + 4 * B$Longitude - 60 * time_offset) %% 1440 # True Solar Time (min)
    hour.angle <- tst / 4 + 180
    hour.angle[tst >= 0] <- tst[tst >= 0] / 4 - 180 # Hour Angle (deg)
    zenith.noaa <- RtoD(acos(sin(DtoR(B$Latitude)) * sin(DtoR(sun.declin)) + cos(DtoR(B$Latitude)) * cos(DtoR(sun.declin)) * cos(DtoR(hour.angle)))) # Solar Zenith Angle (deg)
    azimuth <- (RtoD(acos(((sin(DtoR(B$Latitude)) * cos(DtoR(zenith.noaa))) - sin(DtoR(sun.declin))) / (cos(DtoR(B$Latitude)) * sin(DtoR(zenith.noaa))))) + 180) %% 360 # Solar Azimuth Angle (deg cw from N)
    azimuth[hour.angle <= 0] <- (540 - RtoD(acos(((sin(DtoR(B$Latitude)) * cos(DtoR(zenith.noaa[hour.angle <= 0]))) - sin(DtoR(sun.declin[hour.angle <= 0]))) / (cos(DtoR(B$Latitude)) * sin(DtoR(zenith.noaa[hour.angle <= 0])))))) %% 360
    elevation <- 90 - zenith.noaa
    aar <- (-20.772 / tan(DtoR(elevation))) / 3600 # Approx Atmospheric Refraction (deg)
    aar[elevation > -0.575] <- (1735 + elevation[elevation > -0.575] * (-518.2 + elevation[elevation > -0.575] * (103.4 + elevation[elevation > -0.575] * (-12.79 + elevation[elevation > -0.575] * 0.711)))) / 3600
    aar[elevation > 5] <- (58.1 / tan(DtoR(elevation[elevation > 5])) - 0.07 / (tan(DtoR(elevation[elevation > 5]))^3) + 0.000086 / (tan(DtoR(elevation[elevation > 5]))^5)) / 3600
    aar[elevation > 85] <- 0
    elevation <- elevation + aar
  } else { # Diffused light is not directional, so elevation and azimuth need not be calculated
    elevation <- rep(NA, length(Headers[-1, 1]))
    azimuth <- rep(NA, length(Headers[-1, 1]))
  } # if(spect_type == "direct")

  # preparing results
  azimuth <- c(NA, azimuth)
  elevation <- c(NA, elevation)
  Headers <- cbind(Headers[, 1], elevation, azimuth)
  # Headers[1,1] <- "NA"
  spectrum <- cbind(Headers, spectrum)
  # spectrum <- matrix(suppressWarnings(as.numeric(spectrum)), ncol=ncol(spectrum)) #converting text matrix to a numeric one

  return(spectrum)
} # TUV.call

Integral.product.unequal.spacing <- function(wl, flux, xqy) {
  # integral of the product of flux function and cross-section*quantum yield function, with unequal wavelength spacing
  # wl is the set of wavelengths where flux intensities are calculated (not paired together as they are generated separately by the calling function)
  joined.wl <- c(wl, xqy[, 1])
  joined.wl <- unique(joined.wl) # removes duplicate entries
  joined.wl <- sort(joined.wl)
  l <- length(joined.wl)
  integral.table <- matrix(NA, ncol = 5, nrow = l)
  colnames(integral.table) <- c("Wavelength_min", "Wavelength_width", "Flux", "X-sec.QY", "Product")
  integral.table[, 1] <- joined.wl
  integral.table[1:(l - 1), 2] <- integral.table[2:l, 1] - integral.table[1:(l - 1), 1] # calculates width of wavelength intervals
  integral.table[l, 2] <- integral.table[l - 1, 2] # last element assumed as wide as previous one

  # fills table with values reported for the 2 vectors
  pv <- match(joined.wl, wl)
  integral.table[!is.na(pv), 3] <- flux[pv[!is.na(pv)]]
  pv <- match(joined.wl, xqy[, 1])
  integral.table[, 4] <- xqy[pv, 2]

  # fills table with 0s when outside the wavelength ranges for the 2 variables
  integral.table[integral.table[, 1] < wl[1], 3] <- 0 # We need to check the QY and cross sections in use extend well into UV, rather than put 0
  integral.table[integral.table[, 1] < xqy[1, 1], 4] <- 0
  integral.table[integral.table[, 1] > wl[length(wl)], 3] <- 0
  integral.table[integral.table[, 1] > xqy[length(xqy[, 1]), 1], 4] <- 0

  # interpolates other missing values
  flux.interp <- splinefun(x = integral.table[, 1], y = integral.table[, 3], method = "monoH.FC")
  xqy.interp <- splinefun(x = integral.table[, 1], y = integral.table[, 4], method = "monoH.FC")
  integral.table[is.na(integral.table[, 3]) == TRUE, 3] <- flux.interp(integral.table[is.na(integral.table[, 3]) == TRUE, 1])
  integral.table[is.na(integral.table[, 4]) == TRUE, 4] <- xqy.interp(integral.table[is.na(integral.table[, 4]) == TRUE, 1])

  integral.table[, 5] <- integral.table[, 2] * integral.table[, 3] * integral.table[, 4]

  return(integral.table)
} # Integral.product.unequal.spacing

XSectionQY <- function(filename) {
  # reads and returns crosssection * quantum yield for J-value calculations of a reaction specified by the filename
  # will need to consider temperature dependence of both quantum yield and cross section
  # latest IUPAC data at https://iupac-aeris.ipsl.fr/#
  tmp <- read.csv(filename)
  xqy <- matrix(nrow = length(tmp[, 1]), ncol = 2)
  xqy[, 1] <- tmp[, 1] * 1000

  # interpolating missing quantum yields or cross section points.
  # This needs more discussion on whther it is appropriate, especially for extrapolations
  QY.interp <- splinefun(x = tmp$Wavelength, y = tmp$yield, method = "monoH.FC")
  XSection.interp <- splinefun(x = tmp$Wavelength, y = tmp$X, method = "monoH.FC")
  tmp$yield[is.na(tmp$yield)] <- QY.interp(tmp$Wavelength[is.na(tmp$yield)])
  tmp$X[is.na(tmp$X)] <- XSection.interp(tmp$Wavelength[is.na(tmp$X)])
  tmp$X[tmp$X < 0] <- 0
  tmp$yield[tmp$yield < 0] <- 0

  xqy[, 2] <- tmp[, 2] * tmp[, 3]
  return(xqy)
} # XSectionQY

DtoR <- function(d) {
  r <- pi * d / 180
}

RtoD <- function(r) {
  d <- r * 180 / pi
}

RoomVolumeAveraging <- function(delta, w, d, h, light.h, light.w, light.d, size, geometry) {
  # calculates the volume average (any units) of light intensity (with intensity of 1 in arbitrary units)
  # from a point source located at light.h,w,d position in a room
  # delta is the size of the cubes for the integration, size is the size of the light source (the point where the surface flux was measured)
  # Could this be coded to be faster?

  # distance dependence
  point <- function(r) {
    # this approximates the intensity dependence for a point source of finite size
    if (r > size * 10) {
      return(1 / (r^2))
    } # inverse square
    else if (r < size / 10) {
      return(1)
    } # close to surface approximately the same as from an infinite area source
    else {
      return(r^2 / (r^2 + size^2))
    } # at transition distances, less than 1 but more than inverse square
  }

  if (geometry == "point") f <- point
  # These are for sources of different geometries, approximations to be researched
  # if(geometry == "line") f <- line  e.g. fluorescent tubes
  # if(geometry == "surface") f <- area e.g. light tiles,

  nw <- w / delta
  nd <- d / delta
  nh <- h / delta
  space <- array(rep(NA, nw * nd * nh), dim = c(nw, nd, nh))

  i <- rep(1:nw, each = nd * nh)
  j <- rep(rep(1:nd, each = nh), times = nw)
  k <- rep(1:nh, times = nw * nd)

  r <- sqrt((i * delta - light.w)^2 + (j * delta - light.d)^2 + (k * delta - light.h)^2)
  space <- array(f(r) * delta^3, dim = c(nw, nd, nh))

  vol <- w * d * h
  integr <- sum(space) / vol

  return(integr)
} # RoomVolumeAveraging

RoomVolumeAveraging2 <- function(n, room.sizes, light.center, light.direction, light.size, light.geometry) {
  # calculates the volume average (any units) of light intensity (with intensity of 1 in arbitrary units)
  # from a source of characteristic size light.size located at light.center(x,y,z), possibly with a direction
  # This uses a statistical sampling approach, calculating values at n random places, for speed; always seems biased low compared to RoomVolumeAveraging(), by around 1%, but much faster

  # intensity dependence functions
  point <- function(r) {
    # this approximates the intensity dependence for a point source of finite size, uniform in every direction
    intensity <- rep(NA, length = length(r))
    intensity[r >= light.size * 10] <- 1 / (r[r >= light.size * 10]^2) # inverse square
    intensity[r <= light.size / 10] <- 1 # close to surface approximately the same as from an infinite area source
    intensity[(r < light.size * 10) & (r > light.size / 10)] <- r[(r < light.size * 10) & (r > light.size / 10)]^2 / (r[(r < light.size * 10) & (r > light.size / 10)]^2 + light.size^2) # at transition distances, less than 1 but more than inverse square
    return(intensity)
  }

  line <- function(r) {
    # this approximates the intensity dependence for a line source (e.g. fluorescent tube)
    # to be developed
  }

  if (light.geometry == "point" | light.geometry == "surface") f <- point else f <- line
  # These are for sources of different geometries, approximations to be researched
  # if(geometry == "line") f <- line  e.g. fluorescent tubes
  # if(geometry == "surface") f <- area e.g. light tiles,

  x <- runif(n, 0, room.sizes[1])
  y <- runif(n, 0, room.sizes[2])
  z <- runif(n, 0, room.sizes[3])
  r <- sqrt((x - light.center[1])^2 + (y - light.center[2])^2 + (z - light.center[3])^2)
  light.sample <- f(r)
  if (light.geometry == "surface") {
    p <- data.frame(x = x, y = y, z = z)
    p$x <- p$x - light.center[1]
    p$y <- p$y - light.center[2]
    p$z <- p$z - light.center[3]
    is.lit <- t(apply(p, 1, function(x) x)) %*% light.direction
    # the dot product of a oriented plane direction and a point vector is positive if the point is in the direction of the plane orientation;
    light.sample[is.lit < 0] <- 0 # points behind the surface source (e.g a computer screen) do not receive direct light
  }


  return(mean(light.sample))
} # RoomVolumeAveraging2


LightReflectionFactor <- function(reflectance, WindowedFraction) {
  # Calculates the increase in light flux due to reflection on indoor surfaces
  r <- reflectance * (1 - WindowedFraction) # the light reflected indoors towards windows is lost to further reflections
  # indoors; reflection from glass is neglected
  IntegralReflection <- 1 / (1 - r) # this is the limit for n -> infinity of the geometric series  SUM(1/(r^n))
  return(IntegralReflection)
} # LightReflection

Dimensions <- function(Windows, Building) {
  # Calculates additional geometric dimensions for the simulation box based on the windows data and building/box data
  # box/room dimensions
  Volume <- Building$FloorSurfaceArea * Building$RoomHeight
  Length <- sqrt(Building$FloorSurfaceArea / Building$AspectRatio)
  Width <- Length * Building$AspectRatio
  Height <- Building$RoomHeight
  LongWallArea <- Length * Height
  ShortWallArea <- Width * Height
  SurfacesArea <- 2 * (Building$FloorSurfaceArea + LongWallArea + ShortWallArea)
  # or is better to use: Building$AreaToVolume * Building$FloorSurfaceArea * Height ?

  # window dimensions
  n <- nrow(Windows)
  wA <- rep(NA, n)
  wW <- rep(NA, n)
  wH <- rep(NA, n)
  wH2 <- rep(NA, n)
  wL2 <- rep(NA, n)
  for (i in 1:n) {
    if ((Windows$Orientation[i] - Building$OrientationWiderSide) %% 180 == 0) WallArea <- LongWallArea else WallArea <- ShortWallArea
    wA[i] <- Windows$WallSurfaceFraction[i] * WallArea # window area
    wW[i] <- sqrt(wA[i] / Windows$AspectRatio[i]) # window width
    wH[i] <- wW[i] * Windows$AspectRatio[i] # window height
    wH2[i] <- (Height - wH[i]) / 2 # wall length above and below window
    wL2[i] <- (WallArea / Height - wW[i]) / 2 # wall length on either side of the window
  } # for (i in 1:nrow
  W <- list(wA = wA, wW = wW, wH = wH, wH2 = wH2, wL2 = wL2)

  dims <- list(
    Length = Length, Width = Width, Height = Height, LongWallArea = LongWallArea,
    ShortWallArea = ShortWallArea, SurfacesArea = SurfacesArea, V = Volume, W = W
  )
  return(dims)  # FIX: missing return — without this, Dimensions() returns NULL
                # making every Dims$* access return NULL, corrupting all geometry
} # Dimensions


LightFlux.plotting <- function(Indoor.Flux, Outdoor.Flux, time = NULL, wavelength = NULL, logaxis = "", lposition = "left") {
  # Plots spectra or time course of indoor light. Indoor.Flux must be a list returned by IndoorLightFlux(). If time is specified, it produces the spectrum at tha that time
  # if wavelength is specified, it produces the time course at that wavelength
  # for information and diagnostics, not used by SIACS routines
  if (is.null(time) & is.null(wavelength)) stop("Either time or wavelength must be specified as arguments")
  if (!is.null(time) & !is.null(wavelength)) stop("Cannot specify both time and wavelength as arguments")
  if (missing(Indoor.Flux)) i.flag <- FALSE else i.flag <- TRUE
  if (missing(Outdoor.Flux)) o.flag <- FALSE else o.flag <- TRUE
  if (!(i.flag | o.flag)) stop("No spectrum data was supplied")

  if (!is.null(time)) {
    # graph of spectrum at specific time
    ttl <- paste0("Time of day: ", time)
    if (i.flag) n <- ncol(Indoor.Flux$Total)
    if (o.flag) m <- ncol(Outdoor.Flux$Direct)
    if (i.flag) tin <- match(time, Indoor.Flux$Total$AbsTime)
    if (o.flag) tout <- match(time, Outdoor.Flux$Direct$AbsTime)
    if (i.flag) x.in <- as.numeric(substr(colnames(Indoor.Flux$Total)[3:n], start = 2, stop = 8)) else x.in <- 0
    if (o.flag) x.out <- as.numeric(substr(colnames(Outdoor.Flux$Direct)[5:m], start = 2, stop = 8)) else x.out <- 0
    if (i.flag) yInTot <- Indoor.Flux$Total[tin, 3:n] else yInTot <- 0
    if (i.flag) yInDir <- Indoor.Flux$Direct[tin, 3:n] else yInDir <- 0
    if (i.flag) yInDif <- Indoor.Flux$Diffuse[tin, 3:n] else yInDif <- 0
    if (i.flag) yInArt <- Indoor.Flux$Artificial[tin, 3:n] else yInArt <- 0
    if (o.flag) yOutDir <- Outdoor.Flux$Direct[tout, 5:m] else yOutDir <- 0
    if (o.flag) yOutDif <- Outdoor.Flux$Diffuse[tout, 5:m] else yOutDif <- 0
    if (o.flag) yOutTot <- unlist(yOutDir + yOutDif) else yOutTot <- 0
    xlim <- c(100, max(x.in, x.out))
    xlab <- "Wavelength (nm)"
  } else {
    # graph of time course at specific wavelength
    ttl <- paste0("Wavelength: ", wavelength, " nm")
    if (i.flag) n <- nrow(Indoor.Flux$Total)
    if (o.flag) m <- nrow(Outdoor.Flux$Direct)
    if (i.flag) wv.in <- match(paste0("x", wavelength), colnames(Indoor.Flux$Total))
    if (o.flag) wv.out <- match(paste0("x", wavelength), colnames(Outdoor.Flux$Direct))
    if (i.flag) x.in <- Indoor.Flux$Total$AbsTime else x.in <- 0
    if (o.flag) x.out <- Outdoor.Flux$Direct$AbsTime else x.out <- 0
    if (i.flag) yInTot <- Indoor.Flux$Total[, wv.in] else yInTot <- 0
    if (i.flag) yInDir <- Indoor.Flux$Direct[, wv.in] else yInDir <- 0
    if (i.flag) yInDif <- Indoor.Flux$Diffuse[, wv.in] else yInDif <- 0
    if (i.flag) yInArt <- Indoor.Flux$Artificial[, wv.in] else yInArt <- 0
    if (o.flag) yOutDir <- Outdoor.Flux$Direct[, wv.out] else yOutDir <- 0
    if (o.flag) yOutDif <- Outdoor.Flux$Diffuse[, wv.out] else yOutDif <- 0
    if (o.flag) yOutTot <- unlist(yOutDir + yOutDif) else yOutTot <- 0
    if (i.flag) xlim <- c(min(Indoor.Flux$Total$AbsTime), max(Indoor.Flux$Total$AbsTime)) else xlim <- c(min(Outdoor.Flux$Direct$AbsTime), max(Outdoor.Flux$Direct$AbsTime))
    xlab <- "Time of day"
  }
  if (logaxis == "" | logaxis == "x") y0 <- 0 else y0 <- 0.1
  ylim <- c(y0, max(yInTot, yOutTot))
  ylab <- "Flux (quanta cm-2 s-1 nm-1)"
  totalcol <- "#33CC33"
  directcol <- "#FFCC33"
  diffusecol <- "#000099"
  artifcol <- "#9900CC"

  plot(x.out, yOutTot, type = "l", log = logaxis, lty = 1, col = totalcol, xlab = xlab, ylab = ylab, main = ttl, xlim = xlim, ylim = ylim)
  lines(x.out, yOutDir, type = "l", lty = 1, col = directcol)
  lines(x.out, yOutDif, type = "l", lty = 1, col = diffusecol)
  lines(x.in, yInDif, type = "l", lty = 2, col = diffusecol)
  lines(x.in, yInDir, type = "l", lty = 2, col = directcol)
  lines(x.in, yInArt, type = "l", lty = 2, col = artifcol)
  lines(x.in, yInTot, type = "l", lty = 2, col = totalcol)

  leg.text <- NULL
  col.list <- NULL
  line.list <- NULL
  if (o.flag) {
    leg.text <- c("Direct Outdoor", "Diffuse outdoor", "Total outdoor")
    col.list <- c(directcol, diffusecol, totalcol)
    line.list <- c(1, 1, 1)
  }
  if (i.flag) {
    leg.text <- c(leg.text, "Direct indoor average", "Diffuse indoor average", "Artificial indoor average", "Total indoor")
    col.list <- c(col.list, directcol, diffusecol, artifcol, totalcol)
    line.list <- c(line.list, 2, 2, 2, 2)
  }
  legend(lposition, legend = leg.text, col = col.list, lty = line.list, cex = 0.8)
} # LightFlux.plotting()

FluxRatio <- function(I.Flux, O.Flux) {
  # calculates what fraction of outdoor light is found indoors, at different times and wavelengths
  # for information and diagnostics, not used by SIACS routines
  IFlux <- I.Flux$Total
  OFlux <- O.Flux$Direct + O.Flux$Diffuse
  M <- match(colnames(IFlux), colnames(OFlux))
  M2 <- match(IFlux$AbsTime, O.Flux$Direct$AbsTime)
  confIFlux <- IFlux[!is.na(M2), !is.na(M)]
  times <- confIFlux[, 1:2]
  confIFlux <- confIFlux[, 3:ncol(confIFlux)]
  confOflux <- OFlux[5:ncol(OFlux)]
  RatioMatrix <- confIFlux / confOflux
  RatioMatrix <- cbind(times, RatioMatrix)
  return(RatioMatrix)
} # FluxRatio

Photolysis.rates <- function(LightFlux) {
  # Calculates J-values from wavelength-specific light flux and cross-section + quantum yield tables
  cat("Loading molecular cross sections and quantum yields...\n")
  m <- ncol(LightFlux)
  # loc <- "./Input/Cross-sections_fromSAPRC/"
  loc <- "./Input/Molecular Cross sections Quantum yields/"
  wl <- as.numeric(substr(colnames(LightFlux)[3:m], start = 2, stop = 8))
  ending <- "_k.csv"
  filenames <- listnames <- c(
    "ACET", "ACROLEIN", "BACL", "BALD", "C2CHO", "CCHO_R", "COOH", "GLY_M", "GLY_R",
    "H2O2", "HCHOM", "HCHOR", "HNO3", "HNO4", "HONO", "HONO_NO2", "IC3ONO2",
    "KETONE", "MACR", "MEK", "MGLY", "MGLY_ABS", "MVK", "NO2", "NO3NO", "NO3NO2", "O3O1D",
    "O3O3P"
  )
  filenames <- paste0(loc, filenames, ending)
  l <- length(listnames)
  xQY.data <- list()
  cat("Calculating photochemical reaction rates...\n")
  # for(i in 1:l) {
  #   xQY.data[[i]] <- XSectionQY(filenames[i])
  # }
  ####
  xQY.data <- lapply(filenames, XSectionQY)
  ####
  names(xQY.data) <- listnames
  n <- nrow(LightFlux)
  photolysis.vectors <- as.data.frame(matrix(0, nrow = n, ncol = l))
  colnames(photolysis.vectors) <- listnames

  # for(i in 1:n) { #this needs to be made faster
  #   for (j in 1:l) {
  #     rrt <- Integral.product.unequal.spacing(wl, unlist(LightFlux[i,3:m]),xQY.data[[j]])
  #     photolysis.vectors[i,j] <- sum(rrt[,5])
  #   }
  # }

  # ####
  # Vectorized version using lapply and sapply
  photolysis.vectors <- t(sapply(1:n, function(i) {
    sapply(1:l, function(j) {
      rrt <- Integral.product.unequal.spacing(wl, unlist(LightFlux[i, 3:m]), xQY.data[[j]])
      sum(rrt[, 5])
    })
  }))
  # ####

  photolysis.vectors <- cbind(Time = LightFlux$Time, photolysis.vectors)
  return(photolysis.vectors) # J values are in s-1
}

ArtificialLightFlux <- function(Light.list, Light.spectra, Light.sched, Building) {
  # This function returns the indoor flux of artificial lights by wavelength over time (photons/cm2/s/nm)
  # Inputs are light specifications, spectra, and schedule (expressed as nominal power of light source over time)

  Length <- sqrt(Building$FloorSurfaceArea / Building$AspectRatio)
  Width <- Length * Building$AspectRatio
  if (Length < Width) { # swaps values so that length is always the greater of the 2
    tmp <- Length
    Length <- Width
    Width <- tmp
  }
  n.lights <- length(Light.list[, 1])
  n.sched <- length(Light.sched[1, ]) - 1
  if (n.lights > n.sched) warning("No schedule provided for ", n.lights - n.sched, " artificial light(s), which will be considered off")
  if (n.lights < n.sched) warning("No artificial light definition provided for ", n.sched - n.lights, " light(s) schedules, which will be ignored")

  # sets up results matrix
  n.wl <- nrow(Light.spectra)
  n.time <- nrow(Light.sched)
  AFlux <- as.data.frame(matrix(0, nrow = n.time, ncol = n.wl + 1)) # data frame that will contain the wavelength-specific flux at different points in time
  fr <- Light.spectra[, 1]
  lt <- rep("x", length(fr))
  fr <- paste0(lt, fr) # this is needed as variable names cannot start with a number
  fr <- c("Time", fr)
  colnames(AFlux) <- fr
  AFlux$Time <- Light.sched$Time

  m <- match(Light.list$Spectrum, colnames(Light.spectra))
  # for(i in 1:min(n.lights, n.sched)) {
  #   #for each artificial light that can be processed
  #   Flux <- Light.spectra[, m[i]]
  #      #interpolates any missing values in the data
  #      fl.interp <- splinefun(x= Light.spectra[!is.na(Flux),1], y= Flux[!is.na(Flux)], method = "monoH.FC")
  #      Flux[is.na(Flux)] <- fl.interp(Light.spectra[is.na(Flux),1])
  #   light.position <- c(Width*Light.list$DistanceLongerWall[i], Length*Light.list$DistanceShorterWall[i], Light.list$Height[i] * Building$RoomHeight)
  #   direction <- c(Light.list$DirectionShorter[i], Light.list$DirectionLonger[i], Light.list$DirectionHeight[i])
  #   vol.average <- RoomVolumeAveraging2(1e4, c(Width,Length,Building$RoomHeight), light.position, direction, Light.list$Size[i], Light.list$Geometry[i])
  #   Flux <- Flux * vol.average*Light.list$PowerEfficiency[i]
  #   for(j in 1:n.time ) {
  #     #calculates flux for each time point in the schedule
  #     AFlux[j,2:(n.wl+1)] <- AFlux[j,2:(n.wl+1)] + Flux * Light.sched[j,i+1]
  #     #This can probably be sped up by apply functions. Note that AFlux[,2:(n.wl+1)] <- AFlux[,2:(n.wl+1)] + Flux * Light.sched[,i+1] does not work as it returns a matrix
  #   }
  # }
  ####
  # Precompute light positions and directions
  light.positions <- t(sapply(1:min(n.lights, n.sched), function(i) {
    c(Width * Light.list$DistanceLongerWall[i], Length * Light.list$DistanceShorterWall[i], Light.list$Height[i] * Building$RoomHeight)
  }))

  directions <- t(sapply(1:min(n.lights, n.sched), function(i) {
    c(Light.list$DirectionShorter[i], Light.list$DirectionLonger[i], Light.list$DirectionHeight[i])
  }))

  # Precompute volume averages
  vol.averages <- sapply(1:min(n.lights, n.sched), function(i) {
    RoomVolumeAveraging2(1e4, c(Width, Length, Building$RoomHeight), light.positions[i, ], directions[i, ], Light.list$Size[i], Light.list$Geometry[i])
  })

  # Optimized loop
  for (i in 1:min(n.lights, n.sched)) {
    # Interpolate missing values in the data
    Flux <- Light.spectra[, m[i]]
    fl.interp <- splinefun(x = Light.spectra[!is.na(Flux), 1], y = Flux[!is.na(Flux)], method = "monoH.FC")
    Flux[is.na(Flux)] <- fl.interp(Light.spectra[is.na(Flux), 1])

    # Apply volume average and power efficiency
    Flux <- Flux * vol.averages[i] * Light.list$PowerEfficiency[i]

    # Calculate flux for each time point in the schedule using vectorized operations
    AFlux[, 2:(n.wl + 1)] <- AFlux[, 2:(n.wl + 1)] + matrix(Flux, nrow = n.time, ncol = n.wl, byrow = TRUE) * Light.sched[, i + 1]
  }
  ####


  return(AFlux)
} # ArtificialLightFlux