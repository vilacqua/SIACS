# ===============================================================================
# SIACS TITLE AND HEADER INFORMATION
# ===============================================================================
# Simplified Indoor Air Chemistry Simulator (SIACS)
#
# DESCRIPTION:
# Indoor air chemistry model based on SAPRC99 gas-phase mechanism for simulating
# chemical reactions and mass balance in indoor environments. This model accounts
# for emissions, infiltration, deposition, filtration, and photochemical reactions.
#
# DEVELOPER: US EPA
# OFFICE: Office of Air & Radiation - Office of Radiation and Indoor Air -
#         Indoor Environments Division
#
# VERSION: 0924 (September 2024)
# KEY FEATURES:
# - Connection to GUI interface
# - Implements parallel batch processing (dopar)
# - Byte compiles supporting scripts for improved performance
# - Supports uncertainty analysis and sensitivity calculations
# ===============================================================================

## Record simulation start time for performance monitoring
start_time <- Sys.time()

## Define version identifier for tracking and output labeling
SIACSVersion <- "SIACS 0924"

# ===============================================================================
# REQUIRED LIBRARY IMPORTS
# ===============================================================================
# Load essential packages for numerical computation, data manipulation,
# visualization, file I/O, and parallel processing

# ── Pre-library sentinel logging ────────────────────────────────────────────
# The child Rscript launched by main_app.R already opened siacs_child_steps.log
# with helper siacs_log_line(). Reuse it here if available so we get a
# step-by-step trail of where the engine got to before any silent failure.
# Fall back to a fresh open if the helper isn't loaded (i.e. SIACS engine
# was sourced directly from RStudio, not via the GUI).
if (!exists("siacs_log_line", mode = "function")) {
  if (file.exists("siacs_diagnostics.R")) {
    try(source("siacs_diagnostics.R"), silent = TRUE)
  }
  if (!exists("CHILD_LOG"))
    CHILD_LOG <- file.path(getwd(), "siacs_child_steps.log")
}
.siacs_engine_log <- function(msg) {
  if (exists("siacs_log_line", mode = "function") && exists("CHILD_LOG"))
    try(siacs_log_line(CHILD_LOG, msg), silent = TRUE)
}
.siacs_engine_log("[engine] entered SIACS_0924_Merged_2.R")

# Wrap each library() so a missing/broken package logs WHICH one fails.
.siacs_engine_libs <- c("deSolve", "reshape", "ggplot2", "openxlsx",
                        "doParallel", "foreach", "rstudioapi")
for (.pkg in .siacs_engine_libs) {
  .siacs_engine_log(paste0("[engine] loading library: ", .pkg))
  .ok <- tryCatch({
    suppressPackageStartupMessages(
      library(.pkg, character.only = TRUE, quietly = TRUE))
    TRUE
  }, error = function(e) {
    .siacs_engine_log(paste0("[engine] FAILED library ", .pkg, ": ",
                             conditionMessage(e)))
    FALSE
  })
  if (!isTRUE(.ok)) {
    .siacs_engine_log(paste0("[engine] FATAL: missing package ", .pkg))
    stop("SIACS engine cannot load required package: ", .pkg)
  }
}
.siacs_engine_log("[engine] all engine libraries loaded OK")

# ===============================================================================
# WORKING DIRECTORY SETUP (OPTIONAL)
# ===============================================================================
# Automatically set working directory to script location (useful for RStudio)
# Currently commented out to allow manual directory management
# setwd(dirname(getActiveDocumentContext()$path)) # Set the working directory to the location of the current script

# ===============================================================================
# SOURCE COMPILATION AND SUPPORTING SCRIPTS
# ===============================================================================
# Load and compile all supporting functions and modules required for SIACS operation
# This includes chemical mechanism definitions, utility functions, and analysis modules
.siacs_engine_log("[engine] sourcing SIACS_Compile_Source.R")
tryCatch(
  source("SIACS_Compile_Source.R"),
  error = function(e) {
    .siacs_engine_log(paste0("[engine] FAILED to source ",
                             "SIACS_Compile_Source.R: ", conditionMessage(e)))
    stop(e)
  }
)
.siacs_engine_log("[engine] SIACS_Compile_Source.R loaded OK")

# ===============================================================================
# MAIN SIACS BATCH PROCESSING FUNCTION
# ===============================================================================
SIACS.batch <- function(position = 1, 
                        perturbation = FALSE, 
                        chemistry = TRUE, 
                        mechanism = "SAPRC99", 
                        input_data_list, 
                        instances,
                        use_parallel = TRUE,
                        n_cores = NULL) {
  # =============================================================================
  # PRIMARY FUNCTION FOR LAUNCHING SIACS SIMULATIONS
  # =============================================================================
  # This is the main entry point for running SIACS simulations in batch mode.
  # It handles parallel processing, data loading, simulation execution, and
  # post-processing analysis.
  #
  # PARAMETERS:
  # position     - Starting position in the instance list (allows resuming interrupted batches)
  # perturbation - Boolean flag for perturbation analysis (sensitivity testing)
  # chemistry    - Boolean flag to enable/disable chemical reactions
  # mechanism    - Chemical mechanism to use (default: "SAPRC99")
  # input_data_list - List containing all input data from GUI
  # instances    - Vector of instance indices to process
  # use_parallel - Boolean flag to enable/disable parallel processing
  # n_cores      - Number of cores to use (NULL = auto-detect)
  #
  # RETURNS:
  # List of results from all simulation instances including concentrations,
  # derivatives, mass balance components, and uncertainty analysis
  # =============================================================================
  
  # Compile all supporting scripts for improved performance
  .siacs_engine_log("[engine] compile_supporting_scripts() — cmpfile() x12")
  tryCatch(
    compile_supporting_scripts(),
    error = function(e) {
      .siacs_engine_log(paste0(
        "[engine] FAILED compile_supporting_scripts(): ",
        conditionMessage(e),
        " (likely .Rc file write conflict — OneDrive lock or permissions?)"))
      stop(e)
    }
  )
  .siacs_engine_log("[engine] compile_supporting_scripts() OK")
  
  # Determine total number of simulation instances to run
  runs <- length(instances) # how many times the simulation must be run
  
  # Validate that requested starting position exists in the file list
  if (position > runs) stop("Specified position beyond end of file list")
  
  # =============================================================================
  # PARALLEL PROCESSING SETUP
  # =============================================================================
  # Configure parallel computing backend for batch processing
  # Reserve one core for system operations to maintain responsiveness
  if (use_parallel && runs > 1) {
    # Determine number of cores to use
    if (is.null(n_cores)) {
      no_cores <- min(detectCores() - 1, runs) # Don't use more cores than instances
    } else {
      no_cores <- min(n_cores, runs)
    }
    
    cat("Using parallel processing with", no_cores, "cores for", runs, "simulations\n")
    .siacs_engine_log(paste0("[engine] makeCluster(", no_cores, ") for ",
                              runs, " sims"))
    cl <- tryCatch(makeCluster(no_cores), error = function(e) {
      .siacs_engine_log(paste0("[engine] FAILED makeCluster(): ",
                                conditionMessage(e)))
      stop(e)
    })
    registerDoParallel(cl)
    .siacs_engine_log("[engine] cluster registered, exporting to workers")
    
    # Export necessary objects to cluster
    # STRATEGY: Export input_data_list once to all workers
    # Each worker will access only its assigned instance
    # FIX: pull instance_input_dirs from .GlobalEnv (main_app saves it there
    # via the child-process RData file) so workers save auto-generated light
    # files into the correct Input_ folder rather than getwd().
    instance_input_dirs <- if (exists("instance_input_dirs", envir = .GlobalEnv))
      get("instance_input_dirs", envir = .GlobalEnv) else NULL
    # Export SIACS_ROOT (project root) so tuv_sandbox_dir() on each worker
    # can locate the source tuv5.3.1.exe/ regardless of the worker's cwd.
    SIACS_ROOT <- getwd()
    clusterExport(cl, c("input_data_list", "instances", "mechanism",
                        "perturbation", "chemistry", "SIACSVersion", "OutputList",
                        "instance_input_dirs", "SIACS_ROOT"),
                  envir = environment())
    
    # Load required packages on each worker
    clusterEvalQ(cl, {
      library(deSolve)
      library(reshape)
      library(ggplot2)
      library(openxlsx)
    })
    
    parallel_mode <- TRUE
    .siacs_engine_log("[engine] parallel cluster ready, entering foreach loop")
  } else {
    cat("Running in sequential mode\n")
    .siacs_engine_log("[engine] sequential mode (no cluster)")
    parallel_mode <- FALSE
    no_cores <- 1
  }
  
  # =============================================================================
  # MAIN BATCH PROCESSING LOOP
  # =============================================================================
  ## Begin the main model iteration. Each iteration can use a different set of
  ## input data and save different outputs
  Results <- foreach(
    instance = position:runs,
    .packages = c("openxlsx", "deSolve", "ggplot2", "reshape"),
    .errorhandling = "pass" # Continue even if one instance fails
  ) %dopar% {
    
    # ===========================================================================
    # LOGGING SETUP FOR PARALLEL EXECUTION
    # ===========================================================================
    # Create individual log files for each parallel worker since console output
    # is not available during parallel execution
    # Always write a log file — for sequential runs (single instance) parallel_mode
    # is FALSE and the old code skipped the sink, so all errors were invisible.
    log_file <- paste0("log__", instance, ".txt")
    file.create(log_file)
    sink(log_file, append = TRUE, split = TRUE)  # split=TRUE also echoes to stdout -> GUI log

    # Use withCallingHandlers instead of tryCatch so that variables assigned
    # inside the body (B, E, O, EP, ...) remain visible to called functions
    # that look them up by name in the parent/global environment.
    # tryCatch() creates a new evaluation frame which hides those variables.
    sim_error <- NULL
    withCallingHandlers({

    # ===========================================================================
    # RE-INITIALIZE DATA AND FUNCTIONS FOR PARALLEL WORKER
    # ===========================================================================
    SIACSVersion <- "SIACS 0924"
    source("SIACS_Compile_Source.R")
    source_supporting_scripts()

    cat(SIACSVersion, "\n")
    Results <- list()

    # ── Diagnostic: input_data_list keys and slot contents ──────────────────
    cat("\n--- input_data_list diagnostic (instance", instance, ") ---\n")
    for (key in names(input_data_list)) {
      entry <- input_data_list[[key]]
      if (is.list(entry) && !is.data.frame(entry)) {
        val <- entry[[instance]]
        cat(sprintf("  %-30s : slot[[%d]] = %s\n", key, instance,
          if (is.null(val)) "NULL"
          else if (is.data.frame(val)) paste0("data.frame(", nrow(val), "x", ncol(val), ")")
          else if (is.character(val)) paste0("'", val, "'")
          else class(val)))
      } else {
        cat(sprintf("  %-30s : %s\n", key,
          if (is.null(entry)) "NULL"
          else if (is.data.frame(entry)) paste0("data.frame(", nrow(entry), "x", ncol(entry), ")")
          else class(entry)))
      }
    }
    cat("--- end diagnostic ---\n\n")

    chemical.mechanism.selection(mechanism)
    
    # ===========================================================================
    # CHEMICAL MECHANISM INITIALIZATION
    # ===========================================================================
    # Load and configure the specified chemical mechanism (e.g., SAPRC99)
    # This returns all necessary variables and functions for chemical kinetics
    cms <- chemical.mechanism.selection(mechanism)
    
    # Extract chemical mechanism components and assign to local variables
    # These variables define the chemical system being modeled
    nrxn <- cms[[1]] # Number of chemical reactions
    species.df <- cms[[2]] # Data frame containing species properties
    spcnames <- cms[[3]] # Vector of chemical species names
    nspc <- cms[[4]] # Total number of chemical species
    ngas <- cms[[5]] # Number of gas-phase species
    naer <- cms[[6]] # Number of aerosol species
    Reaction.Rates <- cms[[7]] # Function to calculate reaction rates
    Reactions.Sum <- cms[[8]] # Function to sum reaction contributions
    ReturnRCT <- cms[[9]] # Function to return rate constants
    LU_IROW <- cms[[10]] # Matrix indices for LU decomposition
    LU_ICOL <- cms[[11]] # Matrix indices for LU decomposition
    Jacobian.reaction.components <- cms[[12]] # Jacobian matrix components
    Jacobian.Terms <- cms$Jacobian.Terms[[13]] # Terms for Jacobian calculation
    isgas <- cms[[14]] # Flag array: 1 for gas species, 0 for aerosol
    
    # ===========================================================================
    # INITIALIZE CHEMICAL KINETICS ARRAYS
    # ===========================================================================
    # Create storage arrays for reaction kinetics calculations
    RCT <- rep(0.0, nrxn) # Instantaneous reaction rate constants (units vary by reaction order)
    AR <- rep(0.0, nrxn) # Instantaneous reaction rates (molecules cm-3 s-1)
    
    # ===========================================================================
    # INITIALIZE CONCENTRATION AND EMISSION ARRAYS
    # ===========================================================================
    # Set up arrays for emissions, outdoor concentrations, and penetration factors
    # Use descriptive naming conventions to distinguish variable types
    
    # Emissions array with .S suffix (Source emissions)
    Emissions <- rep(0.0, as.numeric(nspc))
    names(Emissions) <- paste0(spcnames, ".S")
    
    # Outdoor concentrations array with .O suffix (Outdoor concentrations)
    OutdoorConcs <- rep(0.0, as.numeric(nspc))
    names(OutdoorConcs) <- paste0(spcnames, ".O")
    
    # Penetration-adjusted infiltration rates (s-1) for each species
    aP <- rep(0.0, as.numeric(nspc))
    
    # ===========================================================================
    # UTILITY FUNCTIONS FOR DATA VALIDATION
    # ===========================================================================
    CheckDuration <- function(input, filename) {
      # =========================================================================
      # VALIDATE INPUT DATA TIME COVERAGE
      # =========================================================================
      # Ensures that input time series data extends through the entire
      # simulation period. Prevents extrapolation errors during simulation.
      #
      # PARAMETERS:
      # input    - Data frame containing time series with 'Time' column
      # filename - Name of input file for error reporting
      # =========================================================================
      n <- nrow(input)
      max.input.time <- input$Time[n]
      if (max.input.time < endTime) {
        stop(
          "Input data in ", filename, " ends at ", max.input.time,
          " minutes, while simulation runs for ", endTime, " minutes"
        )
      }
      invisible(NULL)
    }
    
    # ===========================================================================
    # ACCESS INSTANCE-SPECIFIC DATA FROM input_data_list
    # ===========================================================================
    # Note: input_data_list is available through cluster export
    # Each worker accesses only its assigned instance index
    current_instance_idx <- instances[instance]
    
    # ===========================================================================
    # INPUT FILE NAMES AND SIMULATION CONFIGURATION
    # ===========================================================================
    # Log progress and store file configuration for this simulation instance
    cat("Reading data for simulation", instance, "of", runs, "\n")
    result <- list()
    # Store file configuration for this simulation instance
    result[[paste0("FileNames", instance)]] <- OutputList[[current_instance_idx]]
    
    # ===========================================================================
    # OPTIONAL ANALYSES CONFIGURATION
    # ===========================================================================
    # Determine which optional post-processing analyses should be performed
    # based on user specifications in input files. This creates a dependency
    # chain where some analyses require others to be completed first.
    OptionalAnalyses <- list(dts = FALSE, dydx = FALSE, uncert = FALSE, mb = FALSE)
    
    if (OutputList[[current_instance_idx]]$OutputTimeDerivatives == "None") {
      SkipAnalisisMessages(type = 1, OutputList[[current_instance_idx]])
    } else {
      # Enable time derivatives analysis
      OptionalAnalyses$dts <- TRUE
      
      # Mass balance analysis depends on time derivatives
      if (OutputList[[current_instance_idx]]$OutputMassBalanceComponents != "None") {
        OptionalAnalyses$mb <- TRUE
      }
      
      # Check if sensitivity analysis is requested
      if (OutputList[[current_instance_idx]]$OutputSensitivity == "None") {
        SkipAnalisisMessages(type = 2, OutputList[[current_instance_idx]])
      } else {
        # Enable sensitivity analysis
        OptionalAnalyses$dydx <- TRUE
        
        # Uncertainty analysis depends on sensitivity analysis
        if (OutputList[[current_instance_idx]]$OutputUncertainty != "None") {
          OptionalAnalyses$uncert <- TRUE
        }
      }
    }
    
    # Record computation start time for performance monitoring
    comptime0 <- Sys.time()
    
    # ===========================================================================
    # TIME PARAMETERS LOADING AND CONFIGURATION
    # ===========================================================================
    # Load temporal parameters that control simulation execution timing
    time.read <- input_data_list$Time[[current_instance_idx]]
    
    # Extract and convert time parameters to appropriate units
    start.time <- time.read$RelativeStartTime # relative time the simulation starts (in minutes)
    dTime <- time.read$TimeStep * 60 # time step converted into seconds
    maxTime <- time.read$Duration * 60 * 60 # total simulation time converted into seconds
    times <- seq(from = start.time, to = maxTime, by = dTime) # units of time is minutes
    endTime <- (time.read$RelativeStartTime + time.read$Duration * 60) # relative time for end of the simulation (in minutes)
    
    # ===========================================================================
    # DEPOSITION AND FILTRATION PARAMETERS
    # ===========================================================================
    # Load species-specific deposition velocities, penetration factors, and
    # filtration efficiencies that control removal processes
    dv.df <- input_data_list$DepositionV[[current_instance_idx]]
    # Units: deposition velocity (m/min), penetration factors (dimensionless),
    #        filtration efficiency (fraction)
    
    # ===========================================================================
    # BUILDING CHARACTERISTICS DATA
    # ===========================================================================
    # Load physical building parameters that define the indoor environment
    B <- input_data_list$BoxData[[current_instance_idx]] # Adding room data parameters (e.g., FloorSurfaceArea, RoomHeight, etc.) from GUI.
    # Contains: FloorSurfaceArea (m²), RoomHeight (m), AreaToVolume (m⁻¹),
    #          InfiltrationSurfaceArea (m²), StackCoefficient ((L/s)²/(cm⁴ K)),
    #          WindCoefficient ((L/s)²/(cm⁴ (m/s)²))
    
    # ===========================================================================
    # PHYSICAL ENVIRONMENT CONDITIONS
    # ===========================================================================
    # Load time-varying physical conditions including temperature, ventilation,
    # humidity, pressure, and meteorological data
    E <- input_data_list$PhysicalEnvironment[[current_instance_idx]] # Load physical environment data: temperature (indoor/outdoor), open window area, ventilation rate (balanced/unbalanced), relative humidity, barometric pressure, and wind speed.
    # Convert "Uncertainty" to NA and ensure numeric columns are correctly formatted
    E <- as.data.frame(lapply(E, function(column) {
      column[column == "Uncertainty"] <- NA
      return(column)
    }))
    # Apply numeric conversion to all columns
    E[] <- lapply(E, function(column) {
      if (is.character(column)) {
        return(as.numeric(column))
      }
      return(column)
    })
    E <- E
    
    # Extract and store uncertainty information for later propagation analysis
    Etemp <- ExtractUncertainty(E, "Physical") # Removes and stores input uncertainty information
    E <- Etemp[[1]] # Clean data without uncertainty columns
    E.uncert <- Etemp[[2]] # Uncertainty parameters
    E.uncert$LightFlux <- 0 # Initialize light flux uncertainty (future development)
    
    # Validate that environmental data covers the full simulation period
    CheckDuration(E, "Physical Environment")
    
    # ===========================================================================
    # OUTDOOR AIR CONCENTRATIONS
    # ===========================================================================
    # Load time-varying outdoor concentrations for all chemical species
    O <- input_data_list$OutdoorConcentrations[[current_instance_idx]]
    # Units: ppm for gas-phase species, μg/m³ for aerosol mass, cm⁻³ for particle count
    
    # Replace "Uncertainty" with NA and convert columns to numeric
    O <- as.data.frame(lapply(O, function(column) {
      column[column == "Uncertainty"] <- NA
      if (is.character(column)) {
        return(as.numeric(column))
      }
      return(column)
    }))
    
    # Ensure O is a replicate of O_org
    O <- O
    
    # Standardize species order and add zeros for missing species
    O <- ChemSpeciesStandardize(O, withtime = TRUE, Nspc = nspc, Spcnames = spcnames) # Standardize order of chemical species and add 0s for missing inputs.
    
    # Extract uncertainty information for outdoor concentrations
    Otemp <- ExtractUncertainty(O, "Outdoor", NSPC = nspc, SPCNAMES = spcnames) # Removes and stores input uncertainty information
    O <- Otemp[[1]]
    O.uncert <- Otemp[[2]]
    names(O.uncert) <- paste0(names(O.uncert), ".O") # Add .O suffix for outdoor
    
    CheckDuration(O, "Outdoor Concentrations")
    
    # ===========================================================================
    # EMISSION PROFILES DATABASE
    # ===========================================================================
    # Load library of emission profiles for different indoor sources
    EP <- input_data_list$EmissionProfiles[[current_instance_idx]]
    # Units: emission rates (g/min) for each chemical species and source type
    
    # Replace "Uncertainty" with NA in the Profile Name column only
    EP$ProfileName[EP$ProfileName == "Uncertainty"] <- NA
    
    # Apply numeric conversion to all columns except ProfileName
    EP[] <- lapply(names(EP), function(column_name) {
      column <- EP[[column_name]]
      if (is.character(column) && column_name != "ProfileName") {
        return(suppressWarnings(as.numeric(column)))
      }
      return(column)
    })
    
    # Convert the list back to a data frame
    EP <- as.data.frame(EP)
    
    # Extract uncertainty information for emission profiles
    EPtemp <- ExtractUncertainty(EP, "Emissions", NSPC = nspc, SPCNAMES = spcnames) # Removes and stores input uncertainty information
    EP <- EPtemp[[1]]
    S.uncert <- EPtemp[[2]]
    names(S.uncert) <- paste0(names(S.uncert), ".S") # Add .S suffix for sources
    
    # ===========================================================================
    # ACTIVITY SCHEDULE DATA
    # ===========================================================================
    # Load time-varying activity data that determines which emission profiles
    # are active and at what intensity (% of maximum emission rate)
    Activities <- input_data_list$Activities[[current_instance_idx]]
    CheckDuration(Activities, "Activities Schedule")
    
    # ===========================================================================
    # INITIAL CONDITIONS SETUP
    # ===========================================================================
    # Handle initial indoor concentrations - either user-specified or calculated
    # from equilibrium conditions without chemistry
    Y.ini <- input_data_list$InitialValues[[current_instance_idx]]
    # FIX: The GUI stores "None" (a character string) when the user leaves the
    # Initial Values file blank rather than NULL. Downstream code checks
    # is.null(Y.ini) to decide whether to compute equilibrium start conditions,
    # so normalise the sentinel to NULL here.
    if (is.character(Y.ini) && length(Y.ini) == 1 && trimws(Y.ini) == "None") {
      cat("InitialValues set to 'None' — starting simulation from chemical equilibrium.\n")
      Y.ini <- NULL
    }
    
    # ===========================================================================
    # INDOOR LIGHT FLUX CALCULATION AND MANAGEMENT
    # ===========================================================================
    # Handle photolysis rate calculations by managing indoor light flux data.
    # This involves either loading pre-calculated data or computing from outdoor
    # solar radiation and artificial lighting sources.
    Indoor.Flux <- list()
    
    # FIX: Resolve the input directory for this instance so all auto-generated
    # light files are saved into the correct Input_ folder, not getwd().
    # Resolve the input directory for this instance so auto-generated light
    # files are saved into the correct Input_ folder, not getwd().
    # instance_input_dirs is a character vector exported by main_app (1..N order).
    input_save_dir <- tryCatch({
      if (exists("instance_input_dirs") && length(instance_input_dirs) >= instance) {
        d <- instance_input_dirs[[instance]]
        if (!is.null(d) && nzchar(d) && dir.exists(d)) d else getwd()
      } else {
        getwd()
      }
    }, error = function(e) getwd())
    cat("Input save directory for instance", instance, ":", input_save_dir, "\n")
    
    if (!is.null(input_data_list$IndoorLight[[current_instance_idx]])) {
      Indoor.Flux$J_values <- input_data_list[["IndoorLight"]][[current_instance_idx]][["J_values"]]
      Indoor.Flux$J_values <- as.data.frame(Indoor.Flux$J_values)
      Indoor.Flux$EnergyFlux <- input_data_list[["IndoorLight"]][[current_instance_idx]][["EnergyFlux"]]
      Indoor.Flux$EnergyFlux <- as.data.frame(Indoor.Flux$EnergyFlux)
    } else {
      cat("Indoor Light flux not available. It will be calculated...\n")
      
      if (!is.null(input_data_list$OutdoorLightDirect[[current_instance_idx]])) {
        DirectFlux.out <- input_data_list$OutdoorLightDirect[[current_instance_idx]]
      } else {
        # FIX: OutdoorLightFlux looks up B by name in its definition environment.
        # Inject B into that environment before calling so it resolves correctly
        # without altering the function's signature.
        environment(OutdoorLightFlux)$B <- B
        DirectFlux.out <- OutdoorLightFlux(time.read, "direct", comptime0)
        write.csv(DirectFlux.out,
                  file.path(input_save_dir, paste0("OutdoorLightDirect_", instance, ".csv")),
                  row.names = FALSE)
      }
      
      if (!is.null(input_data_list$OutdoorLightDiffuse[[current_instance_idx]])) {
        DiffuseFlux.out <- input_data_list$OutdoorLightDiffuse[[current_instance_idx]]
      } else {
        # FIX: same B injection as above; set unconditionally so the diffuse-only
        # path (when OutdoorLightDirect was pre-loaded) is also covered.
        environment(OutdoorLightFlux)$B <- B
        DiffuseFlux.out <- OutdoorLightFlux(time.read, "diffuse", comptime0)
        write.csv(DiffuseFlux.out,
                  file.path(input_save_dir, paste0("OutdoorLightDiffuse_", instance, ".csv")),
                  row.names = FALSE)
      }
      
      if (!is.null(input_data_list$ArtificialLight[[current_instance_idx]])) {
        Artificial.Flux <- input_data_list$ArtificialLight[[current_instance_idx]]
      } else {
        if (!is.null(input_data_list$ArtificialLightList[[current_instance_idx]])) {
          ArtificialLightList <- input_data_list$ArtificialLightList[[current_instance_idx]]
        } else {
          stop("Artificial lights list not found")
        }
        if (!is.null(input_data_list$ArtificialLightSpectra[[current_instance_idx]])) {
          ArtificialLightSpectra <- input_data_list$ArtificialLightSpectra[[current_instance_idx]]
        } else {
          stop("Artificial lights spectra not found")
        }
        if (!is.null(input_data_list$ArtificialLightSchedule[[current_instance_idx]])) {
          ArtificialLightSchedule <- input_data_list$ArtificialLightSchedule[[current_instance_idx]]
        } else {
          stop("Artificial lights schedule not found")
        }
        
        Artificial.Flux <- ArtificialLightFlux(
          ArtificialLightList, ArtificialLightSpectra,
          ArtificialLightSchedule, B
        )
        
        # FIX: ArtificialLightFlux produces one day's schedule. IndoorLightFlux
        # checks coverage against endTime; tile the result for multi-day runs.
        if ("Time" %in% names(Artificial.Flux) && nrow(Artificial.Flux) > 0) {
          day_duration <- max(Artificial.Flux$Time)
          if (day_duration > 0 && endTime > day_duration) {
            n_tiles <- ceiling(endTime / day_duration)
            tiles <- lapply(seq_len(n_tiles) - 1L, function(k) {
              tile <- Artificial.Flux
              tile$Time <- tile$Time + k * day_duration
              tile
            })
            Artificial.Flux <- do.call(rbind, tiles)
            Artificial.Flux <- Artificial.Flux[Artificial.Flux$Time <= endTime + day_duration, ]
            cat("Artificial light flux tiled to cover", n_tiles, "day(s).\n")
          }
        }
        
        write.csv(Artificial.Flux,
                  file.path(input_save_dir, paste0("ArtificialLight_", instance, ".csv")),
                  row.names = FALSE)
      }
      
      if (!is.null(input_data_list$Windows[[current_instance_idx]])) {
        windows <- input_data_list$Windows[[current_instance_idx]]
      } else {
        stop("No windows information found")
      }
      if (!is.null(input_data_list$GlassTransmission[[current_instance_idx]])) {
        glass <- input_data_list$GlassTransmission[[current_instance_idx]]
      } else {
        stop("No glass transparency information found")
      }
      
      cat("Calculating indoor light flux from outdoors and artificial lights...\n")
      Indoor.Flux <- IndoorLightFlux(
        time.read, DirectFlux.out, DiffuseFlux.out,
        Artificial.Flux, windows, glass, B
      )
      Indoor.Flux$J_values <- Photolysis.rates(Indoor.Flux$Total)
      Indoor.Flux$J_values <- as.data.frame(Indoor.Flux$J_values)
      expected_J_names <- c(
        "Time", "ACET", "ACROLEIN", "BACL", "BALD", "C2CHO", "CCHO_R", "COOH",
        "GLY_M", "GLY_R", "H2O2", "HCHOM", "HCHOR", "HNO3", "HNO4", "HONO",
        "HONO_NO2", "IC3ONO2", "KETONE", "MACR", "MEK", "MGLY", "MGLY_ABS",
        "MVK", "NO2", "NO3NO", "NO3NO2", "O3O1D", "O3O3P"
      )
      if (ncol(Indoor.Flux$J_values) == length(expected_J_names)) {
        colnames(Indoor.Flux$J_values) <- expected_J_names
      } else {
        stop("Indoor.Flux$J_values has unexpected number of columns")
      }
      
      cat("Saving indoor light flux...\n")
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "Total");    writeData(wb, sheet = "Total",    x = Indoor.Flux$Total)
      addWorksheet(wb, sheetName = "Direct");   writeData(wb, sheet = "Direct",   x = Indoor.Flux$Direct)
      addWorksheet(wb, sheetName = "Diffuse");  writeData(wb, sheet = "Diffuse",  x = Indoor.Flux$Diffuse)
      addWorksheet(wb, sheetName = "Artificial"); writeData(wb, sheet = "Artificial", x = Indoor.Flux$Artificial)
      addWorksheet(wb, sheetName = "J_values"); writeData(wb, sheet = "J_values", x = Indoor.Flux$J_values)
      addWorksheet(wb, sheetName = "EnergyFlux"); writeData(wb, sheet = "EnergyFlux", x = Indoor.Flux$EnergyFlux)
      indoor_light_path <- file.path(input_save_dir, paste0("IndoorLight_", instance, ".xlsx"))
      saveWorkbook(wb, indoor_light_path, overwrite = TRUE)
      cat("Indoor light flux components written to", indoor_light_path, "\n")
    }
    
    # Record time after data preparation for performance analysis
    comptime1 <- Sys.time() # takes time before solver is launched
    PrintComputationTime("Time preparing data for simulation ", comptime0, comptime1, instance, runs)
    
    # ===========================================================================
    # MAIN SIMULATION EXECUTION
    # ===========================================================================
    # Launch the core SIACS simulation with all loaded parameters and data
    # This function integrates the system of ODEs representing chemical kinetics
    # and mass balance processes over the specified time period
    simulation_temp <- SIACS(times, dv.df,
                             building = B, E, O, EP, Activities, Indoor.Flux, Y.ini, perturbation, chemistry, mechanism,
                             numspc = nspc, eunc = E.uncert, ounc = O.uncert, sunc = S.uncert, OutdoorNames = OutdoorConcs,
                             sn = spcnames, sdf = species.df, EmissionNames = Emissions, start = start.time,
                             Ngas = ngas, N = nrxn
    )
    
    # Extract simulation results and supporting variables
    simulation <- simulation_temp[[1]] # Main simulation results
    aP.app.lst <- simulation_temp[[2]] # Apparent penetration factors
    combined.variables.n <- simulation_temp[[3]] # Combined variable names
    
    # Store simulation results in output structure
    result[[paste0("Simulation", instance)]] <- simulation
    
    # ===========================================================================
    # PARAMETER ORGANIZATION FOR POST-PROCESSING
    # ===========================================================================
    # Group parameters and variables needed for post-processing analyses
    # This organization simplifies function calls in subsequent analysis steps
    
    # Core simulation parameters
    prms <- list(
      nspc = nspc, # Number of species
      ngas = ngas, # Number of gas-phase species
      naer = naer, # Number of aerosol species
      SV = SIACSVersion, # Software version
      OC = OutdoorConcs, # Outdoor concentration names
      sdf = species.df, # Species properties data frame
      sn = spcnames, # Species names vector
      aP.app.lst = aP.app.lst, # Apparent penetration factors
      combined.variables.n = combined.variables.n # Combined variable names
    )
    
    # Uncertainty parameters for propagation analysis
    uncs <- list(eunc = E.uncert, ounc = O.uncert, sunc = S.uncert)
    
    # ===========================================================================
    # PRIMARY RESULTS OUTPUT
    # ===========================================================================
    # Save main simulation results (concentration time series) to specified file
    Save.results(simulation$alldata, OutputList[[current_instance_idx]], instance,
                 chemistry, mechanism, parms = prms
    ) # saves the results of this simulation to a file
    
    # ===========================================================================
    # BASIC VISUALIZATION OUTPUT
    # ===========================================================================
    # Generate standard plots of concentration time series if requested
    if (OutputList[[current_instance_idx]]$OutputBasicChart != "None") {
      Create.result.plots(simulation$alldata,
                          outfile = paste0(OutputList[[current_instance_idx]]$OutputBasicChart, instance, ".png"),
                          maxTime, parms = prms
      )
    } else {
      cat("No plots file chosen for simulation ", instance, "\n")
    }
    
    # ===========================================================================
    # OPTIONAL POST-PROCESSING ANALYSES
    # ===========================================================================
    # Perform requested optional analyses in dependency order
    # Each analysis builds upon results from previous analyses
    
    #---------------------------------------------------------------------------
    # TIME DERIVATIVES ANALYSIS
    #---------------------------------------------------------------------------
    # Calculate time derivatives (dy/dt) for mass balance analysis and debugging
    if (OptionalAnalyses$dts) {
      dydtMatrix <- Deptime.derivative.do(simulation$deSolve,
                                          outfile = paste0(OutputList[[current_instance_idx]]$OutputTimeDerivatives, instance, ".csv"),
                                          simulation$parms, gvs = prms
      )
      result[[paste0("dydt", instance)]] <- dydtMatrix
    } else {
      cat("No derivative file chosen for simulation ", instance, "\n")
    }
    
    #---------------------------------------------------------------------------
    # MASS BALANCE COMPONENTS ANALYSIS
    #---------------------------------------------------------------------------
    # Analyze individual mass balance terms (emissions, infiltration, deposition, etc.)
    # This analysis breaks down the concentration change into component processes:
    # - Indoor emissions from various sources
    # - Infiltration from outdoor air
    # - Deposition to indoor surfaces
    # - Removal by air filtration systems
    # - Chemical production and loss reactions
    if (OptionalAnalyses$mb) {
      Ef <- dv.df$FilterEfficiency # Extract filtration efficiency for each species
      V <- B$FloorSurfaceArea * B$RoomHeight # Calculate total room volume (m³)
      
      # Perform comprehensive mass balance analysis and export to Excel
      mb <- Mass.balance.do(simulation$alldata,
                            outfile = paste0(OutputList[[current_instance_idx]]$OutputMassBalanceComponents, instance, ".xlsx"),
                            V, dydtMatrix, B, dv.df, Ef, gvs = prms
      )
      result[[paste0("MassBalance", instance)]] <- mb
    } else {
      cat("No mass balance analysis file chosen for simulation ", instance, "\n")
    }
    
    #---------------------------------------------------------------------------
    # SENSITIVITY COEFFICIENTS ANALYSIS
    #---------------------------------------------------------------------------
    # Calculate sensitivity coefficients (dy/dx) showing how output concentrations
    # respond to changes in input parameters. This analysis is essential for:
    # - Understanding which parameters most influence results
    # - Identifying critical measurement requirements
    # - Supporting uncertainty propagation calculations
    if (OptionalAnalyses$dydx) {
      derivatives <- Dydx.do(simulation$alldata,
                             outfile = paste0(OutputList[[current_instance_idx]]$OutputSensitivity, instance, ".csv"),
                             time.read, dydtMatrix, gvs = prms
      )
      
      # Extract derivative matrices for potential additional analyses
      dxdtMatrix <- derivatives$dxdt # Parameter time derivatives
      dydxMatrix <- derivatives$dydx # Sensitivity coefficients matrix
      sens.coeff <- derivatives$sensit # Normalized sensitivity coefficients
      
      result[[paste0("Derivatives", instance)]] <- derivatives
      rm(derivatives, inherits = TRUE) # Clean up memory
    } else {
      cat("No sensitivity coefficients file chosen for simulation ", instance, "\n")
    }
    
    #---------------------------------------------------------------------------
    # UNCERTAINTY PROPAGATION ANALYSIS
    #---------------------------------------------------------------------------
    # Propagate input parameter uncertainties through the model to estimate
    # output concentration uncertainties. Uses Monte Carlo or analytical methods
    # based on sensitivity coefficients and input uncertainty distributions.
    if (OptionalAnalyses$uncert) {
      Result.uncert <- Uncertainty.do(simulation$alldata,
                                      outfile = paste0(OutputList[[current_instance_idx]]$OutputUncertainty, instance, ".csv"),
                                      dydxMatrix, Unc = uncs, gvs = prms
      )
      result[[paste0("Uncertainty", instance)]] <- Result.uncert
    } else {
      cat("No uncertainty analysis file chosen for simulation ", instance, "\n")
    }
    
    # ===========================================================================
    # SIMULATION COMPLETION AND PERFORMANCE REPORTING
    # ===========================================================================
    # Record total computation time for this simulation instance
    comptime3 <- Sys.time()
    PrintComputationTime("Total time for simulation ", comptime0, comptime3, instance, runs)
    
    # Return results from this simulation instance
    result

    }, error = function(e) {
      sim_error <<- e
      # Capture the live call stack at the point of failure BEFORE we unwind.
      # Snapshot first so even if cat()/message() routing fails (sink() may be
      # mid-flush during error unwind), we still build a printable string.
      .calls <- sys.calls()
      tb <- character(0)
      for (.i in seq_along(.calls)) {
        tb <- c(tb, sprintf("  frame %d: %s", .i,
                            paste(deparse(.calls[[.i]], nlines = 3L), collapse = " ")))
      }
      msg <- paste0(
        "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "SIACS SIMULATION ERROR (instance ", instance, "):\n",
        conditionMessage(e), "\n",
        "--- traceback ---\n",
        paste(tb, collapse = "\n"), "\n",
        "--- end traceback ---\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
      )
      cat(msg)
      message(msg)
      # Belt-and-braces: write the traceback directly to the per-instance log
      # so we still get it even if the sink() got out of sync during unwind.
      try(writeLines(msg, paste0("log__", instance, ".txt")), silent = TRUE)
      try(writeLines(msg, con = file(paste0("log__", instance, "_traceback.txt"),
                                       open = "wt")), silent = TRUE)
      invokeRestart("abort")   # stop execution and unwind
    })
    # Always close the sink so the log file is flushed and complete
    tryCatch(sink(), error = function(e) invisible(NULL))
    if (!is.null(sim_error))
      return(list(error = conditionMessage(sim_error), instance = instance))
  } # End of foreach parallel loop processing all simulation instances
  
  # =============================================================================
  # PARALLEL PROCESSING CLEANUP
  # =============================================================================
  # Properly shut down the parallel cluster to free system resources
  if (parallel_mode) {
    stopCluster(cl)
  }
  
  # Return all simulation results invisibly (suppresses console printing)
  invisible(Results)
} # End of SIACS.batch() function

# ===============================================================================
# EXECUTION SECTION
# ===============================================================================
# Execute the main SIACS batch processing function with default parameters
# This will process all simulation instances specified in the input files
# Parallel execution is safe again: TUV now runs inside a per-worker sandbox
# (see tuv_sandbox_dir() in SIACSLights.R) so the shared SIACS.txt race is gone.
.siacs_engine_log("[engine] calling SIACS.batch()")
a <- SIACS.batch(input_data_list = input_data_list, instances = instances) # output shape may be slightly different from non-parallel version
.siacs_engine_log("[engine] SIACS.batch() returned")

# ===============================================================================
# PERFORMANCE MONITORING
# ===============================================================================
# Calculate and display total runtime for the entire batch processing session
end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "secs")
print(runtime)