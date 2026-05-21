# wizard_defaults.R
# =======================================================================
# Load default values from Input/ directory default files (same as Advanced)
# =======================================================================

load_wizard_defaults <- function() {

  defaults <- list()

  read_csv_safe <- function(path) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
    tryCatch(read.csv(path, stringsAsFactors = FALSE, comment.char = "#"), error = function(e) NULL)
  }

  find_path <- function(pattern) {
    if (!exists("file_paths_advanced")) return(NULL)
    idx <- which(grepl(pattern, file_paths_advanced, ignore.case = TRUE))
    if (length(idx) > 0) file_paths_advanced[idx[1]] else NULL
  }

  # ---- TIME ----------------------------------------------------------------
  time_data <- read_csv_safe(file_paths_advanced[8])
  if (!is.null(time_data) && nrow(time_data) > 0) {
    defaults$start_year          <- time_data$StartTimeYear[1]
    defaults$start_month         <- time_data$StartTimeMonth[1]
    defaults$start_day           <- time_data$StartTimeDay[1]
    defaults$start_time          <- time_data$StartTime[1]
    defaults$timezone            <- time_data$StartTimeStandard[1]
    defaults$relative_start      <- time_data$RelativeStartTime[1]
    defaults$timestep            <- time_data$TimeStep[1]
    defaults$duration            <- time_data$Duration[1]
    defaults$activity_transition <- 0.1  # always 0.1 regardless of file value
  }

  # ---- BOX DATA ------------------------------------------------------------
  box_data <- read_csv_safe(file_paths_advanced[6])
  if (!is.null(box_data) && nrow(box_data) > 0) {
    defaults$floor_area             <- box_data$FloorSurfaceArea[1]
    defaults$room_height            <- box_data$RoomHeight[1]
    defaults$aspect_ratio           <- box_data$AspectRatio[1]
    defaults$orientation            <- box_data$OrientationWiderSide[1]
    defaults$area_to_volume         <- box_data$AreaToVolume[1]
    defaults$infiltration_area      <- box_data$InfiltrationSurfaceArea[1]
    defaults$stack_coeff            <- if ("StackCoefficient"                 %in% names(box_data)) box_data$StackCoefficient[1]               else NA
    defaults$wind_coeff             <- if ("WindCoefficient"                  %in% names(box_data)) box_data$WindCoefficient[1]                else NA
    defaults$discharge_coeff        <- box_data$DischargeCoefficient[1]
    defaults$gravity                <- box_data$GravityAccel[1]
    defaults$midpoint_height        <- box_data$MidpointHeightofWindow[1]
    # neutral_pressure deliberately not loaded from file - set reactively as 1.35 * num_stories
    defaults$opening_effectiveness  <- box_data$OpeningEffectivenessCoefficient[1]
    defaults$latitude               <- box_data$Latitude[1]
    defaults$longitude              <- box_data$Longitude[1]
    defaults$altitude               <- box_data$Altitude[1]
    defaults$surface_albedo         <- box_data$SurfaceAlbedo[1]
    defaults$cloud_optical_depth    <- box_data$CloudOpticalDepth[1]
    defaults$cloud_base             <- box_data$CloudBase[1]
    defaults$cloud_top              <- box_data$CloudTop[1]
    defaults$indoor_reflectance     <- box_data$IndoorReflectance[1]
  }

  # ---- WINDOWS -------------------------------------------------------------
  windows_data <- read_csv_safe(file_paths_advanced[12])
  if (!is.null(windows_data)) {
    defaults$num_windows <- nrow(windows_data)
    defaults$windows     <- windows_data
  }

  # ---- GLASS TYPES ---------------------------------------------------------
  glass_path <- find_path("GlassTransmission")
  if (is.null(glass_path)) glass_path <- file.path("Input", "GlassTransmission.csv")
  glass_data <- read_csv_safe(glass_path)
  if (!is.null(glass_data)) {
    glass_cols <- setdiff(names(glass_data), c("Wavelength","wavelength","nm","NM","lambda","wl"))
    defaults$glass_types <- if (length(glass_cols) > 0) c("None", glass_cols) else c("None","Laminate996")
  } else {
    defaults$glass_types <- c("None","Laminate996")
  }

  # ---- ARTIFICIAL LIGHT LIST -----------------------------------------------
  lights_data <- read_csv_safe(file_paths_advanced[15])
  if (!is.null(lights_data)) {
    defaults$num_lights <- nrow(lights_data)
    defaults$lights     <- lights_data
  }

  # ---- ARTIFICIAL LIGHT SCHEDULE -------------------------------------------
  ls_path <- find_path("ArtificialLightSchedule")
  ls_data <- read_csv_safe(ls_path)
  if (!is.null(ls_data) && nrow(ls_data) > 0) {
    ls_data <- ls_data[!is.na(suppressWarnings(as.numeric(as.character(ls_data$Time)))), ]
    if (nrow(ls_data) > 0) {
      ls_data$Time <- as.numeric(as.character(ls_data$Time))
      for (col in setdiff(names(ls_data),"Time"))
        ls_data[[col]] <- as.numeric(as.character(ls_data[[col]]))
      defaults$light_schedule <- ls_data
    }
  }

  # ---- PHYSICAL ENVIRONMENT ------------------------------------------------
  phys_data <- read_csv_safe(file_paths_advanced[3])
  if (!is.null(phys_data) && nrow(phys_data) > 0) {
    phys_num <- phys_data[!is.na(suppressWarnings(as.numeric(as.character(phys_data$Time)))), ]
    if (nrow(phys_num) > 0) {
      phys_num$Time <- as.numeric(as.character(phys_num$Time))
      for (col in setdiff(names(phys_num),"Time"))
        phys_num[[col]] <- as.numeric(as.character(phys_num[[col]]))
      defaults$phys_data           <- phys_num
      defaults$temp_indoor_init    <- phys_num$Ti[1]
      defaults$temp_outdoor_init   <- phys_num$To[1]
      defaults$rh_init             <- phys_num$RH[1]
      defaults$bp_init             <- phys_num$BP[1]
      defaults$wind_init           <- if ("Wind" %in% names(phys_num)) phys_num$Wind[1] else 0
      last <- nrow(phys_num)
      defaults$temp_indoor_final   <- phys_num$Ti[last]
      defaults$temp_outdoor_final  <- phys_num$To[last]
      defaults$rh_final            <- phys_num$RH[last]
      defaults$bp_final            <- phys_num$BP[last]
      defaults$wind_final          <- if ("Wind" %in% names(phys_num)) phys_num$Wind[last] else 0
    }
  }

  # ---- ACTIVITIES ----------------------------------------------------------
  activities_data <- read_csv_safe(file_paths_advanced[5])
  if (!is.null(activities_data)) defaults$activities <- activities_data

  # ---- OUTDOOR CONCENTRATIONS (species list for manual entry) --------------
  oc_path <- find_path("Outdoor")
  oc_data <- read_csv_safe(oc_path)
  if (!is.null(oc_data)) {
    defaults$outdoor_species <- setdiff(names(oc_data), c("Time","Uncertainty"))
    defaults$outdoor_path    <- oc_path
  }

  # ---- OUTPUT DEFAULTS -----------------------------------------------------
  if (exists("get_default_output_values")) {
    dv <- get_default_output_values()
    defaults$output_table  <- unname(dv[["OutputTable"]])
    defaults$output_chart  <- unname(dv[["OutputBasicChart"]])
    defaults$output_deriv  <- unname(dv[["OutputTimeDerivatives"]])
    defaults$output_bal    <- unname(dv[["OutputMassBalanceComponents"]])
    defaults$output_sens   <- unname(dv[["OutputSensitivity"]])
    defaults$output_uncert <- unname(dv[["OutputUncertainty"]])
  }

  cat("Loaded wizard defaults from Advanced default Input files\n")
  return(defaults)
}

# -----------------------------------------------------------------------
# Push scalar defaults into already-rendered inputs
# -----------------------------------------------------------------------
update_wizard_inputs_with_defaults <- function(session, defaults) {
  upN <- function(id, v) if (!is.null(v) && !is.na(v)) updateNumericInput(session, id, value = v)
  upT <- function(id, v) if (!is.null(v))               updateTextInput(  session, id, value = as.character(v))
  upS <- function(id, v) if (!is.null(v))               updateSelectInput(session, id, selected = as.character(v))

  upN("wiz_latitude",  defaults$latitude);  upN("wiz_longitude", defaults$longitude)
  upN("wiz_altitude",  defaults$altitude);  upN("wiz_gravity",   defaults$gravity)

  upN("wiz_start_year",   defaults$start_year);   upN("wiz_start_month",  defaults$start_month)
  upN("wiz_start_day",    defaults$start_day);    upT("wiz_start_time",   defaults$start_time)
  upS("wiz_timezone",     defaults$timezone);     upN("wiz_relative_start", defaults$relative_start)

  upN("wiz_duration",            defaults$duration)
  upN("wiz_timestep",            defaults$timestep)
  # wiz_activity_transition: hardcoded to 0.1 in UI, not pushed from defaults

  upN("wiz_floor_area",   defaults$floor_area);  upN("wiz_room_height",  defaults$room_height)
  upN("wiz_aspect_ratio", defaults$aspect_ratio);upN("wiz_orientation",  defaults$orientation)

  upN("wiz_area_to_volume",        defaults$area_to_volume)
  upN("wiz_infiltration_area",     defaults$infiltration_area)
  upN("wiz_indoor_reflectance",    defaults$indoor_reflectance)
  # wiz_neutral_pressure: set reactively from num_stories (1.35 * n), not pushed from defaults
  upN("wiz_midpoint_height",       defaults$midpoint_height)
  upN("wiz_discharge_coeff",       defaults$discharge_coeff)
  upN("wiz_opening_effectiveness", defaults$opening_effectiveness)
  upN("wiz_num_windows",           defaults$num_windows)
  if (!is.null(defaults$stack_coeff) && !is.na(defaults$stack_coeff)) upN("wiz_stack_coeff", defaults$stack_coeff)
  if (!is.null(defaults$wind_coeff)  && !is.na(defaults$wind_coeff))  upN("wiz_wind_coeff",  defaults$wind_coeff)

  upN("wiz_surface_albedo",      defaults$surface_albedo)
  upN("wiz_cloud_base",          defaults$cloud_base)
  upN("wiz_cloud_top",           defaults$cloud_top)
  upN("wiz_cloud_optical_depth", defaults$cloud_optical_depth)

  upN("wiz_num_lights", defaults$num_lights)

  upN("wiz_temp_indoor_init",   defaults$temp_indoor_init);  upN("wiz_temp_indoor_final",  defaults$temp_indoor_final)
  upN("wiz_temp_outdoor_init",  defaults$temp_outdoor_init); upN("wiz_temp_outdoor_final", defaults$temp_outdoor_final)
  upN("wiz_rh_init",   defaults$rh_init);  upN("wiz_rh_final",   defaults$rh_final)
  upN("wiz_bp_init",   defaults$bp_init);  upN("wiz_bp_final",   defaults$bp_final)
  upN("wiz_wind_init", defaults$wind_init);upN("wiz_wind_final", defaults$wind_final)

  upT("wiz_output_table", defaults$output_table); upT("wiz_output_chart", defaults$output_chart)
  updateCheckboxInput(session,"wiz_output_derivatives",value=TRUE)
  updateCheckboxInput(session,"wiz_output_massbalance", value=TRUE)
  updateCheckboxInput(session,"wiz_output_sensitivity", value=TRUE)
  updateCheckboxInput(session,"wiz_output_uncertainty", value=TRUE)
  upT("wiz_output_derivatives_file", defaults$output_deriv)
  upT("wiz_output_massbalance_file",  defaults$output_bal)
  upT("wiz_output_sensitivity_file",  defaults$output_sens)
  upT("wiz_output_uncertainty_file",  defaults$output_uncert)

  cat("Updated wizard inputs with default values\n")
}

# -----------------------------------------------------------------------
populate_activity_defaults <- function(wizard_state, defaults) {
  if (is.null(defaults$activities) || nrow(defaults$activities) == 0) return(invisible(NULL))
  act_df <- defaults$activities
  for (col in c("Generic","Adult","Smoking","GasCooking.Persily1998","Incense.Manoukian2013")) {
    matched <- names(act_df)[trimws(names(act_df)) == col]
    if (length(matched) == 1) {
      wizard_state$activity_schedules[[col]] <- data.frame(
        Time  = as.numeric(act_df$Time),
        Value = as.numeric(act_df[[matched]]),
        stringsAsFactors = FALSE
      )
    }
  }
  cat("Pre-populated activity schedules from default Activities.csv\n")
  invisible(NULL)
}

populate_light_schedule_defaults <- function(wizard_state, defaults) {
  if (is.null(defaults$light_schedule)) return(invisible(NULL))
  wizard_state$light_schedule <- defaults$light_schedule
  cat("Pre-populated light schedule from default ArtificialLightSchedule.csv\n")
  invisible(NULL)
}

populate_phys_schedule_defaults <- function(wizard_state, defaults) {
  if (is.null(defaults$phys_data)) return(invisible(NULL))
  wizard_state$phys_schedule <- defaults$phys_data
  cat("Pre-populated physical environment schedule from default file\n")
  invisible(NULL)
}

populate_window_defaults <- function(session, defaults) {
  if (is.null(defaults$windows)) return(invisible(NULL))
  for (i in 1:nrow(defaults$windows)) {
    w <- defaults$windows[i,]
    updateNumericInput(session, paste0("wiz_window_",i,"_orientation"), value = w$Orientation)
    if ("AspectRatio"            %in% names(w)) updateNumericInput(session, paste0("wiz_window_",i,"_aspect"),      value=w$AspectRatio)
    if ("WallSurfaceFraction"    %in% names(w)) updateNumericInput(session, paste0("wiz_window_",i,"_fraction"),    value=w$WallSurfaceFraction)
    if ("GlassType"              %in% names(w)) updateSelectInput( session, paste0("wiz_window_",i,"_glass"),       selected=w$GlassType)
    if ("ObstructedAreaFraction" %in% names(w)) updateNumericInput(session, paste0("wiz_window_",i,"_obstruction"), value=w$ObstructedAreaFraction)
    if ("HorizonElevationAngle"  %in% names(w)) updateNumericInput(session, paste0("wiz_window_",i,"_horizon"),     value=w$HorizonElevationAngle)
  }
  cat("Populated", nrow(defaults$windows), "window defaults\n")
  invisible(NULL)
}

populate_light_defaults <- function(session, defaults) {
  if (is.null(defaults$lights)) return(invisible(NULL))
  for (i in 1:nrow(defaults$lights)) {
    l <- defaults$lights[i,]
    if ("Geometry"            %in% names(l)) updateSelectInput( session, paste0("wiz_light_",i,"_geometry"),   selected=l$Geometry)
    if ("Size"                %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_size"),       value=l$Size)
    if ("Height"              %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_height"),     value=l$Height)
    if ("DistanceShorterWall" %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_dist_short"), value=l$DistanceShorterWall)
    if ("DistanceLongerWall"  %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_dist_long"),  value=l$DistanceLongerWall)
    if ("DirectionShorter"    %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_dir_short"),  value=l$DirectionShorter)
    if ("DirectionLonger"     %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_dir_long"),   value=l$DirectionLonger)
    if ("DirectionHeight"     %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_dir_height"), value=l$DirectionHeight)
    if ("PowerEfficiency"     %in% names(l)) updateNumericInput(session, paste0("wiz_light_",i,"_efficiency"), value=l$PowerEfficiency)
    if ("Spectrum"            %in% names(l)) updateSelectInput( session, paste0("wiz_light_",i,"_bulb"),       selected=l$Spectrum)
  }
  cat("Populated", nrow(defaults$lights), "light defaults\n")
  invisible(NULL)
}

# -----------------------------------------------------------------------
# Restore all wizard inputs from a previously saved snapshot
# (called when the user clicks Edit on a wizard-created simulation)
# -----------------------------------------------------------------------
restore_wizard_from_snapshot <- function(session, wizard_state, snap) {
  upN <- function(id, v) if (!is.null(v) && !is.na(v)) updateNumericInput(session, id, value = v)
  upT <- function(id, v) if (!is.null(v))               updateTextInput(  session, id, value = as.character(v))
  upS <- function(id, v) if (!is.null(v))               updateSelectInput(session, id, selected = as.character(v))
  upR <- function(id, v) if (!is.null(v))               updateRadioButtons(session, id, selected = as.character(v))
  upC <- function(id, v) if (!is.null(v))               updateCheckboxInput(session, id, value = isTRUE(v))

  # Screen 1 — Location
  upN("wiz_latitude",  snap$latitude);  upN("wiz_longitude", snap$longitude)
  upN("wiz_altitude",  snap$altitude);  upN("wiz_gravity",   snap$gravity)

  # Screen 2 — Start Time
  # Normalise any legacy 4-value POSIX names saved before the global list was added
  legacy_tz_map <- c(
    "EST5EDT" = "EST5EDT", "CST6CDT" = "CST6CDT",
    "MST7MDT" = "MST7MDT", "PST8PDT" = "PST8PDT"
  )
  tz_val <- snap$timezone
  if (!is.null(tz_val) && tz_val %in% names(legacy_tz_map))
    tz_val <- legacy_tz_map[[tz_val]]   # maps to itself — still valid
  upN("wiz_start_year",   snap$start_year);  upN("wiz_start_month",     snap$start_month)
  upN("wiz_start_day",    snap$start_day);   upT("wiz_start_time",      snap$start_time)
  upS("wiz_timezone",     tz_val);           upN("wiz_relative_start",  snap$relative_start)

  # Screen 3 — Simulation Time
  upN("wiz_duration", snap$duration); upN("wiz_timestep", snap$timestep)
  upN("wiz_activity_transition", snap$activity_transition)

  # Screen 4 — The Box
  upN("wiz_floor_area",   snap$floor_area);  upN("wiz_room_height",  snap$room_height)
  upN("wiz_aspect_ratio", snap$aspect_ratio);upN("wiz_orientation",  snap$orientation)

  # Screen 5 — Building
  upN("wiz_area_to_volume",        snap$area_to_volume)
  upN("wiz_infiltration_area",     snap$infiltration_area)
  upN("wiz_indoor_reflectance",    snap$indoor_reflectance)
  upN("wiz_num_stories",           snap$num_stories)
  upN("wiz_neutral_pressure",      snap$neutral_pressure)
  upN("wiz_midpoint_height",       snap$midpoint_height)
  upN("wiz_discharge_coeff",       snap$discharge_coeff)
  upN("wiz_opening_effectiveness", snap$opening_effectiveness)
  upN("wiz_stack_coeff",           snap$stack_coeff)
  upN("wiz_num_windows",           snap$num_windows)

  # Screen 6 — Outside
  upN("wiz_surface_albedo",      snap$surface_albedo)
  upN("wiz_cloud_base",          snap$cloud_base)
  upN("wiz_cloud_top",           snap$cloud_top)
  upN("wiz_cloud_optical_depth", snap$cloud_optical_depth)
  upS("wiz_shelter_class",       snap$shelter_class)
  upN("wiz_wind_coeff",          snap$wind_coeff)

  # Screen 7 — Artificial Lights
  upR("wiz_lights_mode", snap$lights_mode)
  upN("wiz_num_lights",  snap$num_lights)

  # Screen 9 — Physical Environment
  upR("wiz_phys_env_mode",        snap$phys_env_mode)
  upN("wiz_temp_indoor_init",  snap$temp_indoor_init);  upN("wiz_temp_indoor_final",  snap$temp_indoor_final)
  upN("wiz_temp_outdoor_init", snap$temp_outdoor_init); upN("wiz_temp_outdoor_final", snap$temp_outdoor_final)
  upN("wiz_rh_init",   snap$rh_init);   upN("wiz_rh_final",   snap$rh_final)
  upN("wiz_bp_init",   snap$bp_init);   upN("wiz_bp_final",   snap$bp_final)
  upN("wiz_wind_init", snap$wind_init); upN("wiz_wind_final", snap$wind_final)

  # Screen 10 — Activities
  upR("wiz_act_mode", snap$act_mode)

  # Screen 11 — Outdoor Concentrations
  upR("wiz_outdoor_conc_mode", snap$outdoor_conc_mode)

  # Screen 12 — Outputs & Mechanism
  upT("wiz_output_table",            snap$output_table)
  upT("wiz_output_chart",            snap$output_chart)
  upC("wiz_output_derivatives",      snap$output_derivatives)
  upT("wiz_output_derivatives_file", snap$output_derivatives_file)
  upC("wiz_output_massbalance",      snap$output_massbalance)
  upT("wiz_output_massbalance_file", snap$output_massbalance_file)
  upC("wiz_output_sensitivity",      snap$output_sensitivity)
  upT("wiz_output_sensitivity_file", snap$output_sensitivity_file)
  upC("wiz_output_uncertainty",      snap$output_uncertainty)
  upT("wiz_output_uncertainty_file", snap$output_uncertainty_file)
  upR("wiz_mechanism",               snap$mechanism)

  # Restore complex schedule / window state
  if (!is.null(snap$light_schedule))     wizard_state$light_schedule     <- snap$light_schedule
  if (!is.null(snap$phys_schedule))      wizard_state$phys_schedule      <- snap$phys_schedule
  if (!is.null(snap$activity_schedules)) wizard_state$activity_schedules <- snap$activity_schedules
  if (!is.null(snap$windows_data))       wizard_state$windows_data       <- snap$windows_data

  cat("Restored wizard inputs from snapshot\n")
  invisible(NULL)
}