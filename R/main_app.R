# main_app.R
# Complete modular app with shared validators, helpers, and globals (sourced)

# ===== Pre-library Diagnostics =====
# This block runs BEFORE any library() call so we can capture environment
# information and report missing packages cleanly. The diagnostics file
# lives next to the project root and is overwritten on each launch.
source('siacs_diagnostics.R')

SIACS_STARTUP_LOG <- file.path(getwd(), "siacs_startup.log")
siacs_log_init(SIACS_STARTUP_LOG, "SIACS GUI startup log")
siacs_log_line(SIACS_STARTUP_LOG, "[startup] main_app.R sourcing started")

# ── OneDrive detection ───────────────────────────────────────────────────────
# Running SIACS from a OneDrive-synced folder is a known source of silent
# failures: the sync client can lock files mid-run (especially the .Rc
# bytecode that we recompile), and "Files On-Demand" can leave R source files
# as cloud-only placeholders. Log a clear warning if we detect this.
.siacs_od <- siacs_detect_onedrive(getwd())
if (isTRUE(.siacs_od$in_onedrive)) {
  siacs_log_line(SIACS_STARTUP_LOG, paste0(
    "[startup] WARNING: working directory is inside OneDrive: ",
    .siacs_od$path,
    " — recommend copying SIACS to a non-synced local folder for runs."))
} else {
  siacs_log_line(SIACS_STARTUP_LOG, "[startup] working dir is not OneDrive")
}

# ── Stale .Rc bytecode cleanup ───────────────────────────────────────────────
# R bytecode (.Rc) files are NOT portable across R versions. The project ships
# with .Rc files compiled on the maintainer's machine; loading those on a
# different R version can crash silently. Remove them at startup so that
# SIACS_Compile_Source.R rebuilds them fresh in the current R version.
siacs_clean_rc_files(getwd(), log_file = SIACS_STARTUP_LOG)

# ===== Libraries =====
# Load each package via siacs_safe_library() so any missing or broken
# package is logged with its name (instead of a silent app crash). The
# function falls through on failure; we collect the failures and surface them
# to the user when the Shiny app starts.
.siacs_required_pkgs <- c(
  "shiny", "jsonlite", "tidygeocoder", "dplyr", "DT", "shinyFiles",
  "rhandsontable", "shinyjs", "shinyBS", "readxl", "zoo",
  "processx", "plotly", "reshape2"
)
siacs_log_line(SIACS_STARTUP_LOG,
  paste0("[startup] loading packages: ",
         paste(.siacs_required_pkgs, collapse = ", ")))
.siacs_failed_pkgs <- siacs_load_packages(
  .siacs_required_pkgs,
  log_file           = SIACS_STARTUP_LOG,
  install_if_missing = TRUE
)
if (length(.siacs_failed_pkgs) > 0) {
  siacs_log_line(SIACS_STARTUP_LOG, paste0(
    "[startup] FATAL: failed to load packages: ",
    paste(.siacs_failed_pkgs, collapse = ", ")))
  stop("SIACS could not load required packages: ",
       paste(.siacs_failed_pkgs, collapse = ", "),
       ". See siacs_startup.log for details.")
}
siacs_log_line(SIACS_STARTUP_LOG, "[startup] all packages loaded OK")

# ── Engine packages: install (but don't load) here so the background
# simulation process won't crash later with "there is no package called X".
# The engine itself loads them in its own R session; we just guarantee
# they are present in the user's library before that session starts.
.siacs_engine_pkgs <- c("deSolve", "reshape", "ggplot2", "openxlsx",
                        "doParallel", "foreach", "rstudioapi")
siacs_log_line(SIACS_STARTUP_LOG,
  paste0("[startup] ensuring engine packages installed: ",
         paste(.siacs_engine_pkgs, collapse = ", ")))
.siacs_engine_missing <- siacs_ensure_packages_installed(
  .siacs_engine_pkgs, log_file = SIACS_STARTUP_LOG)
if (length(.siacs_engine_missing) > 0) {
  siacs_log_line(SIACS_STARTUP_LOG, paste0(
    "[startup] WARNING: engine packages still missing after install: ",
    paste(.siacs_engine_missing, collapse = ", "),
    " — runs will fail until these are installed."))
} else {
  siacs_log_line(SIACS_STARTUP_LOG,
    "[startup] all engine packages present")
}

# ===== Source Shared Code (validators, helpers, globals) =====
siacs_log_line(SIACS_STARTUP_LOG, "[startup] sourcing shared.R")
source('shared.R')

# ===== Source Modules =====
# NOTE: Standard module is retired and replaced by Wizard
# source('standard_module.R')   # <-- removed
siacs_log_line(SIACS_STARTUP_LOG, "[startup] sourcing modules")
source('advanced_module.R')
source('wizard_module.R')
source('wizard_defaults.R')
source('wizard_helpers.R')
siacs_log_line(SIACS_STARTUP_LOG, "[startup] modules sourced OK")

# ===== Main UI =====
ui <- navbarPage(
  title = NULL,
  selected = "Simulation Queue",
  position = "static-top",

  # ── Simulation Queue (combined) ──────────────────────────────────────────
  tabPanel(
    useShinyjs(),
    title = "Simulation Queue",

    tags$head(tags$style(HTML("
      .queue-toolbar { padding: 10px 0 6px 0; display: flex; gap: 6px; align-items: center; flex-wrap: wrap; }
      .queue-toolbar .btn { min-width: 90px; }
      .queue-toolbar .btn-run { margin-left: auto; }
      .queue-toolbar .btn-save { }
      .dt-queue table.dataTable tbody tr { cursor: pointer; }
      .dt-queue table.dataTable tbody tr.selected { background-color: #cce5ff !important; }
    "))),

    div(style = "padding: 12px 16px;",

      # ── Environment health banner (only shown when problems detected) ────
      uiOutput("siacs_env_banner"),

      # ── Toolbar ───────────────────────────────────────────────────────────
      div(class = "queue-toolbar",
        modal_actionButton("submit", "＋ Add", target = "sim_type_modal",
          class = "btn btn-success"),
        actionButton("edit_sim",   "✏ Edit",        class = "btn btn-default",
          disabled = "disabled"),
        actionButton("copy_sim",   "⧉ Copy",        class = "btn btn-default",
          disabled = "disabled"),
        actionButton("remove_sim", "🗑 Remove",      class = "btn btn-danger",
          disabled = "disabled"),
        actionButton("clear_queue","✕ Clear Queue", class = "btn btn-warning"),
        div(style = "margin-left:auto; display:flex; gap:6px;",
          actionButton("run_queue", "▶ Run Queue",  class = "btn btn-primary"),
          actionButton("ok",        "💾 Save",      class = "btn btn-default")
        )
      ),

      # ── Queue table ───────────────────────────────────────────────────────
      DT::dataTableOutput("queue_table"),

      # ── Log console (shown while running) ────────────────────────────────
      conditionalPanel(
        condition = "output.siacs_running",
        div(
          style = "margin-top:16px;",
          div(
            style = paste0(
              "background:#1e1e1e;color:#d4d4d4;font-family:monospace;font-size:12px;",
              "padding:12px;border-radius:6px;max-height:400px;overflow-y:auto;",
              "white-space:pre-wrap;word-break:break-all;"
            ),
            id = "siacs_log_box",
            verbatimTextOutput("siacs_log", placeholder = TRUE)
          ),
          tags$script(HTML(
            "Shiny.addCustomMessageHandler('scroll_log', function(x) {",
            "  var el = document.getElementById('siacs_log_box');",
            "  if (el) el.scrollTop = el.scrollHeight;",
            "});"
          ))
        )
      )
    ),

    # ── Modals ───────────────────────────────────────────────────────────────

    # Simulation type selection
    modal_ui(
      id = "sim_type_modal",
      title = "Add Simulation",
      static = TRUE,
      fluidRow(
        column(
          12,
          p("Choose a configuration method for the new simulation run."),
          radioButtons(
            "sim_type",
            label = "Simulation Type",
            choiceNames  = c("Standard", "Advanced"),
            choiceValues = c("standard", "advanced"),
            inline = TRUE
          )
        )
      ),
      footer = tagList(
        actionButton(inputId = "open_sim_menu", label = "Continue"),
        modalButton("Cancel")
      )
    ),

    # Simulation Summary modal — shown after wizard/advanced completes (Add)
    modal_ui(
      id    = "sim_summary_modal",
      title = "Simulation Summary",
      static = TRUE,
      fluidRow(
        column(12,
          textInput("run_name", "Run Name:",
            placeholder = "Letters, numbers, hyphens, underscores only"),
          uiOutput("run_name_error")
        )
      ),
      tags$hr(),
      fluidRow(
        column(4,
          tags$label("Location"),
          verbatimTextOutput("summary_location", placeholder = TRUE)
        ),
        column(4,
          tags$label("Duration (hours)"),
          verbatimTextOutput("summary_duration", placeholder = TRUE)
        ),
        column(4,
          tags$label("Chemical Mechanism"),
          verbatimTextOutput("summary_mechanism", placeholder = TRUE)
        )
      ),
      fluidRow(
        column(12,
          div(style = "background:#f8f9fa;border-radius:6px;padding:10px;margin-top:12px;",
            strong("Directories will be named:"),
            verbatimTextOutput("preview_dir_name", placeholder = TRUE)
          )
        )
      ),
      footer = tagList(
        actionButton("sim_summary_confirm", "Add to Queue",
          style = "background:#4CAF50;color:#fff;"),
        actionButton("sim_summary_cancel", "Cancel",
          style = "margin-left:8px;")
      )
    ),

    # Edit modal — Run Name + Chemical Mechanism only
    modal_ui(
      id    = "edit_sim_modal",
      title = "Edit Simulation",
      static = TRUE,
      fluidRow(
        column(12,
          textInput("edit_run_name", "Run Name:",
            placeholder = "Letters, numbers, hyphens, underscores only"),
          uiOutput("edit_run_name_error")
        )
      ),
      tags$hr(),
      fluidRow(
        column(6,
          radioButtons("edit_mechanism", "Chemical Mechanism",
            choiceNames  = list("SAPRC99 (default)"),
            choiceValues = c("SAPRC99"),
            selected = "SAPRC99"),
          # SAPRC07T placeholder — see wizard_module.R for the same pattern.
          div(style="margin-left:20px;color:#999;font-size:13px;",
              "SAPRC07T")
        ),
        column(6,
          tags$label("Location (read-only)"),
          verbatimTextOutput("edit_location_display", placeholder = TRUE),
          tags$label("Duration (read-only)"),
          verbatimTextOutput("edit_duration_display", placeholder = TRUE)
        )
      ),
      footer = tagList(
        actionButton("edit_sim_confirm", "Save Changes",
          style = "background:#2196F3;color:#fff;"),
        actionButton("edit_sim_cancel", "Cancel",
          style = "margin-left:8px;")
      )
    ),

    # Clear Queue confirmation modal
    modal_ui(
      id    = "clear_queue_modal",
      title = "Clear Queue",
      static = FALSE,
      p("This will remove all simulations from the queue and delete their input directories on disk."),
      p(strong("This cannot be undone. Are you sure?")),
      footer = tagList(
        actionButton("clear_queue_confirm", "Yes, Clear All",
          style = "background:#e74c3c;color:#fff;"),
        actionButton("clear_queue_cancel", "Cancel",
          style = "margin-left:8px;")
      )
    ),

    # Include module modals
    advanced_module_ui(),
    wizard_module_ui()
  ),

  # Results
  tabPanel(
    title = "Results",
    div(style = "padding:16px;",

      # ── Folder picker row ─────────────────────────────────────────────────
      fluidRow(
        column(7,
          tags$label("Output folder"),
          div(style = "display:flex; gap:6px; align-items:center;",
            textInput("results_folder", label = NULL,
              value       = getwd(),
              placeholder = "Path to folder containing Output_* directories",
              width       = "100%"),
            shinyDirButton("results_browse", "Browse",
              title  = "Select output folder",
              icon   = icon("folder-open"),
              style  = "white-space:nowrap;")
          )
        ),
        column(5,
          div(style = "padding-top:22px;",
            uiOutput("results_status_banner")
          )
        )
      ),

      # ── File selector row ─────────────────────────────────────────────────
      fluidRow(
        column(7,
          selectInput("results_sim_select", "Simulation result file",
            choices  = character(0),
            selected = NULL,
            width    = "100%")
        ),
        column(2, style = "padding-top:25px;",
          actionButton("results_scan", "Scan Folder",
            style = "width:100%;")
        ),
        column(2, style = "padding-top:25px;",
          actionButton("results_load", "Load",
            style = "background:#2E86C1;color:#fff;width:100%;")
        )
      ),

      # ── CSV Preview — always visible once a file is selected ─────────────
      uiOutput("csv_raw_preview"),

      # ── Plot views — only after successful load ───────────────────────────
      conditionalPanel(
        condition = "output.results_loaded",
        tabsetPanel(
          id = "results_view_tabs",

          # Single Panel
          tabPanel(
            title = "Single Panel",
            div(style = "padding-top:12px;",
              fluidRow(
                column(4,
                  selectInput("single_sp", "Variable to plot",
                    choices = character(0), selected = NULL, width = "100%")
                ),
                column(3,
                  div(style = "padding-top:4px;",
                    checkboxGroupInput("single_show_lines", "Show",
                      choices  = c("indoor", "outdoor"),
                      selected = c("indoor", "outdoor"),
                      inline   = TRUE)
                  )
                ),
                column(5,
                  div(style = "padding-top:10px;",
                    tags$small(style = "color:#666;",
                      "Gas in ppb (x1000 from ppm); PM in ug/m3; others in native units.")
                  )
                )
              ),
              plotlyOutput("single_panel_plot", height = "65vh")
            )
          ),

          # Multi Panel
          tabPanel(
            title = "Multi Panel",
            div(style = "padding-top:12px;",
              fluidRow(
                column(3,
                  div(style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;padding:12px;max-height:70vh;overflow-y:auto;",
                    div(style = "margin-bottom:8px;",
                      actionButton("results_select_none", "None", style="font-size:11px;padding:2px 8px;margin-right:4px;"),
                      actionButton("results_select_key",  "Key",  style="font-size:11px;padding:2px 8px;")
                    ),
                    checkboxGroupInput("results_show_lines", "Show",
                      choices  = c("indoor", "outdoor"),
                      selected = c("indoor", "outdoor"),
                      inline   = TRUE),
                    hr(),
                    uiOutput("results_species_checkboxes")
                  )
                ),
                column(9,
                  # Dynamic grid of individual plots — 4 per row, scrollable
                  tags$head(tags$style(HTML("
                    .multi-panel-grid {
                      display: grid;
                      grid-template-columns: repeat(4, 1fr);
                      gap: 8px;
                    }
                    .multi-panel-grid .plotly-plot {
                      min-width: 0;
                    }
                  "))),
                  div(style = "overflow-y:auto; max-height:82vh;",
                    uiOutput("results_plot_grid")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  # Archive
  tabPanel("Archive", fluidRow(column(12, "Archive content goes here."))),

  # Help
  tabPanel(
    "Help",
    fluidRow(column(12, wellPanel(
      h3("SIACS GUI Help"),
      h4("Getting Started"),
      p("1. Click 'Add Simulation' to create a new simulation configuration"),
      p("2. Choose Wizard (recommended) or Advanced mode"),
      p("3. Configure your simulation parameters and input files"),
      p("4. Validate your inputs before running"),
      p("5. Click 'Run Queue' to execute all simulations"),
      h4("Validation"),
      p("The GUI includes comprehensive input validation to catch errors before simulation:"),
      tags$ul(
        tags$li("Use validation tools in Advanced mode"),
        tags$li("Wizard uses guided inputs and default templates")
      )
    )))
  ),

  # Credits
  tabPanel("Credits", fluidRow(column(12, "Credits content goes here."))),

  header = tags$head(
    tags$style(HTML(
    " .navbar-nav > li > a { font-size: 12px; }
      .navbar { background-color: #f8f9fa; height: 15px; }
      .navbar-nav { float: right; }
      table { margin-left: auto; margin-right: auto; }
      .container-fluid { padding-top: 10px; }
      .modal-header .btn[data-dismiss='modal'] { float: right; border: none; font-weight: 700; padding: 0 }
      @media (min-width: 768px) { .modal-xl { width: 95%; } }
      .modal-body { max-height: 90vh; overflow: auto; }
      #advanced_menu .shiny-panel-conditional { margin-bottom: 10px; }
      .validation-pass { color: #28a745; font-weight: bold; }
      .validation-warning { color: #ffc107; font-weight: bold; }
      .validation-error { color: #dc3545; font-weight: bold; } "
  )),
    # Disable the SAPRC07T option in every Chemical Mechanism radio group.
    # Currently a no-op because SAPRC07T is rendered as a static greyed
    # label (not a radio option) while the mechanism wiring is incomplete.
    # Left in place so re-enabling SAPRC07T later is a one-line change:
    # just restore the radio option in all three modules and this script
    # will keep working or can be removed.
    tags$script(HTML(paste0(
      "(function(){\n",
      "  var TARGETS = ['wiz_mechanism','adv_mechanism','edit_mechanism'];\n",
      "  function disableSAPRC07T(){\n",
      "    TARGETS.forEach(function(id){\n",
      "      document.querySelectorAll('input[name=\"'+id+'\"][value=\"SAPRC07T\"]').forEach(function(el){\n",
      "        el.disabled = true;\n",
      "        el.tabIndex = -1;\n",
      "        el.checked = false;\n",
      "        el.style.display = 'none';\n",
      "        var lbl = el.closest('label');\n",
      "        if (lbl) lbl.style.display = 'none';\n",
      "      });\n",
      "    });\n",
      "  }\n",
      "  disableSAPRC07T();\n",
      "  var obs = new MutationObserver(disableSAPRC07T);\n",
      "  obs.observe(document.body, {childList: true, subtree: true});\n",
      "})();"
    )))
  )
)

# ===== Main Server =====
server <- function(input, output, session) {

  # Stop app when browser session ends
  session$onSessionEnded(function() {
    stopApp()
  })

  # ── Environment health banner ────────────────────────────────────────────
  # Renders a yellow strip if we detected a OneDrive working directory at
  # startup. Silent otherwise. Helps a remote user immediately see the
  # most common cause of silent run crashes without opening the log file.
  output$siacs_env_banner <- renderUI({
    if (isTRUE(.siacs_od$in_onedrive)) {
      div(style = paste0(
        "background:#fff3cd; border:1px solid #ffeeba; color:#856404;",
        "padding:10px 14px; border-radius:6px; margin-bottom:10px;",
        "font-size:13px;"),
        tags$b("\u26a0 OneDrive folder detected: "),
        "SIACS is running from a OneDrive-synced location ",
        tags$code(.siacs_od$path),
        ". File-sync conflicts can cause silent crashes (empty Output ",
        "folders with no log file). For best results, copy the SIACS ",
        "folder to a non-synced local location (e.g. ",
        tags$code("C:\\SIACS\\"), ") before running. ",
        tags$br(),
        tags$small(
          "Diagnostic logs: ",
          tags$code("siacs_startup.log"), ", ",
          tags$code("siacs_child_started.log"), ", ",
          tags$code("siacs_child_steps.log"),
          " (in the project folder)."
        )
      )
    } else {
      NULL
    }
  })

  # ── Helper: compact sparse input_data_list / OutputList to sequential indices ──
  # After sim removals, instances may be non-contiguous (e.g. c(1,3)).
  # The model's foreach loop uses position 1..N, so we remap data to 1..N
  # and pass instances = 1:N, making everything consistent.
  compact_for_run <- function(instances_vec, idl, ol) {
    n <- length(instances_vec)
    cat(sprintf("\n[compact_for_run] called with instances_vec = c(%s), n = %d\n",
                paste(instances_vec, collapse=", "), n))

    # ── FIX 1: keys-outer loop, fresh vector("list",n) per key ──────────────
    # The old pos-outer/keys-inner loop used `compact_idl[[key]][[pos]] <- NULL`
    # which is a NO-OP (or removes an existing slot) because [[<-NULL never
    # *extends* a list in R.
    #
    # This fix pre-allocates new_sub with vector("list",n) — all slots explicitly
    # exist as NULL — then assigns every slot with the SINGLE-BRACKET form
    # `new_sub[pos] <- list(val)`.  Critical: using [pos] <- list(NULL) instead
    # of [[pos]] <- NULL preserves the slot (single-bracket assignment never
    # removes elements regardless of value; double-bracket with NULL removes).
    compact_idl <- idl
    for (key in names(idl)) {
      if (is.list(idl[[key]]) && !is.data.frame(idl[[key]])) {
        new_sub <- vector("list", n)          # length n, all slots NULL
        for (new_pos in seq_len(n)) {
          orig_idx <- instances_vec[new_pos]
          val <- if (orig_idx <= length(idl[[key]])) idl[[key]][[orig_idx]] else NULL
          new_sub[new_pos] <- list(val)       # [<- list() ALWAYS preserves length
        }
        compact_idl[[key]] <- new_sub         # guaranteed length n
      }
    }

    # ── Optional-light key diagnostics (per-slot view only) ─────────────────
    # NOTE: Do NOT collapse mixed-NULL keys to NULL. SIACS checks each slot
    # individually via `!is.null(input_data_list$IndoorLight[[current_idx]])`,
    # so a list(df, NULL) is the correct shape for "sim 1 uses provided data,
    # sim 2 lets SIACS calculate". Collapsing the whole key to NULL forces
    # SIACS to recompute lights for every instance — wrong, and also triggers
    # the TUV shared-output-file race when multiple workers run in parallel.
    optional_light_keys <- c("IndoorLight", "OutdoorLightDirect",
                             "OutdoorLightDiffuse", "ArtificialLight",
                             "InitialValues")
    cat("[compact_for_run] --- optional-light key diagnostics ---\n")
    for (key in optional_light_keys) {
      sub <- compact_idl[[key]]
      if (is.null(sub)) {
        cat(sprintf("  %-30s: NULL (SIACS will calculate)\n", key))
      } else if (is.list(sub)) {
        types <- vapply(sub, function(x)
          if (is.null(x)) "NULL"
          else if (is.data.frame(x)) sprintf("data.frame(%dx%d)", nrow(x), ncol(x))
          else sprintf("%s(len=%d)", class(x)[1L], length(x)),
          character(1L))
        cat(sprintf("  %-30s: list(len=%d) slots=[%s]\n",
                    key, length(sub), paste(types, collapse=", ")))
      } else {
        cat(sprintf("  %-30s: %s (unexpected)\n", key, class(sub)[1L]))
      }
    }
    cat("[compact_for_run] --- end diagnostics ---\n")

    # ── Rebuild OutputList ───────────────────────────────────────────────────
    compact_ol <- vector("list", n)
    for (new_pos in seq_len(n)) {
      orig_idx <- instances_vec[new_pos]
      compact_ol[[new_pos]] <- if (orig_idx <= length(ol)) ol[[orig_idx]] else list()
    }

    list(instances = seq_len(n), input_data_list = compact_idl, OutputList = compact_ol)
  }


  # Reactive state initialization
  sim_num <- reactiveVal(1)
  data <- reactiveVal(data.frame(
    Simulation_No   = character(),
    Run_Name        = character(),
    Location        = character(),
    Duration        = numeric(),
    Chemical_Model  = character(),
    Creation_Method = character(),   # "wizard" or "advanced"
    stringsAsFactors = FALSE
  ))

  # Duration reactive shared with modules
  # Default 72 is just a fallback; both Wizard and Advanced modules update
  # this as soon as the user supplies a real duration (Wizard: via the
  # duration input; Advanced: parsed from the Time data file's Duration
  # column when that file is loaded). Keeping this reactive in sync prevents
  # validation warnings like "data ends at 2880 minutes but simulation
  # duration is 4320 minutes" from firing against the stale 72h default.
  simulation_duration <- reactiveVal(72)
  get_duration <- function() simulation_duration()
  set_duration <- function(hours) {
    h <- suppressWarnings(as.numeric(hours))
    if (length(h) == 1L && !is.na(h) && h > 0)
      simulation_duration(h)
  }

  # ── Helpers ──────────────────────────────────────────────────────────────────

  # Sanitise run name for filesystem use: spaces → underscores, strip illegal chars
  sanitise_run_name <- function(nm) {
    nm <- trimws(nm)
    nm <- gsub(" +", "_", nm)
    nm <- gsub("[^A-Za-z0-9_\\-]", "", nm)
    nm
  }

  # Build a unique dir prefix: <safe_run_name>_<timestamp>
  make_dir_prefix <- function(run_name) {
    paste0(sanitise_run_name(run_name), "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
  }

  # Return an error string if the run name is invalid, else ""
  validate_run_name <- function(nm) {
    nm <- trimws(nm)
    if (!nzchar(nm))             return("Run name cannot be empty.")
    if (nchar(nm) > 60)          return("Run name must be 60 characters or fewer.")
    if (grepl("[^A-Za-z0-9 _\\-]", nm))
      return("Only letters, numbers, spaces, hyphens and underscores are allowed.")
    ""
  }

  # Write a JSON-style config file listing files used for this run
  write_run_config <- function(snap_dir, entry) {
    meta <- get_meta()
    cfg <- list(
      run_name       = entry$Run_Name,
      location       = entry$Location,
      city_name      = meta$city_name %||% NULL,
      latitude       = if (!is.na(meta$lat)) meta$lat else NULL,
      longitude      = if (!is.na(meta$lon)) meta$lon else NULL,
      duration_hours = entry$Duration,
      chemical_model = entry$Chemical_Model,
      mechanism      = meta$mechanism %||% "SAPRC99",
      simulation_no  = entry$Simulation_No,
      created_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      input_files    = as.list(list.files(snap_dir, full.names = FALSE))
    )
    cfg_path <- file.path(snap_dir,
      paste0(sanitise_run_name(entry$Run_Name), "_config.json"))
    writeLines(jsonlite::toJSON(cfg, pretty = TRUE, auto_unbox = TRUE), cfg_path)
    cat("Config written to:", cfg_path, "\n")
  }

  # ── Pending state ─────────────────────────────────────────────────────────────
  pending_entry    <- reactiveVal(NULL)
  pending_sim_type <- reactiveVal(NULL)

  # Reactive metadata — updated from .GlobalEnv each time a module signals
  # completion, so summary modal renders always show the current run's values.
  pending_meta_rv <- reactiveVal(list(lat = NA, lon = NA, duration = NA, mechanism = "SAPRC99"))

  get_meta <- function() pending_meta_rv()

  # Read-only metadata displays
  output$summary_location <- renderText({
    m <- get_meta()
    city <- m$city_name %||% NA
    if (!is.na(city) && nzchar(city)) city
    else if (!is.na(m$lat) && !is.na(m$lon)) sprintf("%.4f, %.4f", m$lat, m$lon)
    else "— (not available)"
  })

  output$summary_duration <- renderText({
    m <- get_meta()
    if (is.na(m$duration)) "— (not available)"
    else paste0(m$duration, " h")
  })

  output$summary_mechanism <- renderText({
    m <- get_meta()
    m$mechanism %||% "SAPRC99"
  })

  # Live dir-name preview shown in the summary modal
  output$preview_dir_name <- renderText({
    nm  <- trimws(input$run_name %||% "")
    err <- validate_run_name(nm)
    if (nzchar(err)) return("(fix the run name to see preview)")
    pfx <- make_dir_prefix(nm)
    paste0("Input_",  pfx, "\nOutput_", pfx)
  })

  # Inline validation — format + uniqueness
  output$run_name_error <- renderUI({
    nm  <- trimws(input$run_name %||% "")
    if (!nzchar(nm)) return(NULL)
    err <- validate_run_name(nm)
    if (nzchar(err))
      return(div(style = "color:#c0392b;font-size:12px;margin-top:2px;", err))
    # Uniqueness: check against all committed run names
    existing <- data()$Run_Name
    if (tolower(nm) %in% tolower(existing))
      return(div(style = "color:#c0392b;font-size:12px;margin-top:2px;",
        paste0("\u26a0 A run named \"", nm, "\" already exists in the queue.")))
    # Also check on-disk dirs
    safe <- sanitise_run_name(nm)
    existing_dirs <- list.dirs(".", recursive = FALSE, full.names = FALSE)
    collides <- any(grepl(paste0("^Input_", safe, "_"), existing_dirs))
    if (collides)
      return(div(style = "color:#e67e22;font-size:12px;margin-top:2px;",
        "\u26a0 An input folder with this run name already exists on disk."))
    div(style = "color:#27ae60;font-size:12px;margin-top:2px;", "\u2713 Valid")
  })

  # ── Open sim menu ─────────────────────────────────────────────────────────────
  observeEvent(input$open_sim_menu, {
    type <- input$sim_type %||% "standard"
    pending_sim_type(type)

    # Pre-register pending sim number so wizard_helpers gets the right current_idx
    instances_preview <- c(as.integer(data()$Simulation_No), as.integer(sim_num()))
    assign("instances", instances_preview, envir = .GlobalEnv)

    # Create a temporary per-instance input dir immediately so wizard/advanced
    # can write files into it. main_app will rename it with the run name once
    # the Simulation Summary modal is confirmed.
    pending_idx <- as.integer(sim_num())
    tmp_in_dir  <- paste0("Input_tmp_", pending_idx, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    dir.create(tmp_in_dir, recursive = TRUE, showWarnings = FALSE)
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    instance_dirs[[as.character(pending_idx)]] <- list(input = tmp_in_dir, output = NULL)
    assign("instance_dirs", instance_dirs, envir = .GlobalEnv)

    modal_toggle("sim_type_modal", "hide")

    # Clear stale metadata from any previous run so the summary modal
    # never shows values from a prior instance while the new one is configuring.
    if (exists("pending_sim_metadata", envir = .GlobalEnv))
      rm("pending_sim_metadata", envir = .GlobalEnv)
    pending_meta_rv(list(lat = NA, lon = NA, duration = NA, mechanism = "SAPRC99"))

    switch(type,
      standard = {
        shinyjs::runjs("Shiny.setInputValue('wizard_open_trigger', Math.random(), {priority: 'event'})")
        modal_toggle("wizard_modal", "show")
      },
      advanced = {
        # Tell the advanced module to wipe its state so the second "+ Add"
        # does not show inputs from the first simulation. Edit mode goes
        # through advanced_edit_trigger and is unaffected.
        shinyjs::runjs("Shiny.setInputValue('advanced_open_trigger', Math.random(), {priority: 'event'})")
        modal_toggle("advanced_inputs_menu", "show")
      }
    )
  })

  # ── Both wizard and advanced signal here when config is done ─────────────────
  # We open the Simulation Summary modal for the user to fill in metadata.
  show_summary_modal <- function() {
    # Pull fresh metadata written by the module just now
    fresh <- if (exists("pending_sim_metadata", envir = .GlobalEnv))
      get("pending_sim_metadata", envir = .GlobalEnv)
    else
      list(lat = NA, lon = NA, duration = NA, mechanism = "SAPRC99")

    # Reverse-geocode lat/lon to city + state via OSM Nominatim
    fresh$city_name <- tryCatch({
      if (!is.na(fresh$lat) && !is.na(fresh$lon)) {
        df <- data.frame(lat = fresh$lat, long = fresh$lon)
        result <- df %>%
          reverse_geocode(lat = lat, long = long,
                          method = "osm", full_results = TRUE) %>%
          select(any_of(c("city", "town", "village", "county", "state")))
        # Build "City, State" string from whichever columns came back
        place <- coalesce(
          result$city[1], result$town[1],
          result$village[1], result$county[1], NA_character_)
        state <- if ("state" %in% names(result)) result$state[1] else NA_character_
        parts <- na.omit(c(place, state))
        if (length(parts) > 0) paste(parts, collapse = ", ")
        else sprintf("%.4f, %.4f", fresh$lat, fresh$lon)
      } else NA_character_
    }, error = function(e) {
      cat("Geocoding failed:", conditionMessage(e), "\n")
      if (!is.na(fresh$lat) && !is.na(fresh$lon))
        sprintf("%.4f, %.4f", fresh$lat, fresh$lon)
      else NA_character_
    })

    pending_meta_rv(fresh)
    updateTextInput(session, "run_name", value = "")
    modal_toggle("sim_summary_modal", "show")
  }

  observeEvent(input$wizard_confirmed_internal, {
    show_summary_modal()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$advanced_confirmed_internal, {
    show_summary_modal()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── Simulation Summary — confirm ──────────────────────────────────────────────
  observeEvent(input$sim_summary_confirm, {
    nm  <- trimws(input$run_name %||% "")

    # Format validation
    err <- validate_run_name(nm)
    if (nzchar(err)) {
      showNotification(err, type = "error", duration = 4)
      return()
    }

    # Uniqueness check against queue
    if (tolower(nm) %in% tolower(data()$Run_Name)) {
      showNotification(
        paste0('A run named "', nm, '" already exists in the queue. Choose a different name.'),
        type = "error", duration = 5)
      return()
    }

    # Uniqueness check against on-disk dirs
    safe <- sanitise_run_name(nm)
    existing_dirs <- list.dirs(".", recursive = FALSE, full.names = FALSE)
    if (any(grepl(paste0("^Input_", safe, "_"), existing_dirs))) {
      showNotification(
        paste0('An input folder for "', nm, '" already exists on disk. Choose a different name.'),
        type = "warning", duration = 5)
      return()
    }

    # Read metadata written by wizard/advanced on completion
    meta <- get_meta()

    # Rename the per-instance temp input dir to the final run-name-prefixed name
    prefix      <- make_dir_prefix(nm)
    in_dir      <- paste0("Input_",  prefix)
    out_dir     <- paste0("Output_", prefix)
    pending_idx <- as.integer(sim_num())

    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    tmp_in <- instance_dirs[[as.character(pending_idx)]]$input
    if (!is.null(tmp_in) && dir.exists(tmp_in) && tmp_in != in_dir) {
      file.rename(tmp_in, in_dir)
      cat("Renamed input dir:", tmp_in, "->", in_dir, "\n")
    } else if (!dir.exists(in_dir)) {
      dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
    }
    instance_dirs[[as.character(pending_idx)]] <- list(input = in_dir, output = out_dir)
    assign("instance_dirs", instance_dirs, envir = .GlobalEnv)

    # Build location string — prefer geocoded city name, fall back to lat/lon
    city <- meta$city_name %||% NA
    loc_str <- if (!is.na(city) && nzchar(city)) city
               else if (!is.na(meta$lat) && !is.na(meta$lon))
                 sprintf("%.4f, %.4f", meta$lat, meta$lon)
               else "—"

    new_entry <- data.frame(
      Simulation_No   = as.integer(sim_num()),
      Run_Name        = nm,
      Location        = loc_str,
      Duration        = if (!is.na(meta$duration)) as.numeric(meta$duration) else NA_real_,
      Chemical_Model  = meta$mechanism %||% "SAPRC99",
      Creation_Method = pending_sim_type() %||% "wizard",
      stringsAsFactors = FALSE
    )
    simulation_duration(new_entry$Duration)
    data(rbind(data(), new_entry))
    assign("instances",  as.integer(data()$Simulation_No), envir = .GlobalEnv)

    # Persist mechanism so run handlers pass it to SIACS.batch
    assign("mechanism", new_entry$Chemical_Model, envir = .GlobalEnv)

    sim_num(sim_num() + 1)
    pending_entry(NULL)

    modal_toggle("sim_summary_modal", "hide")

    # Write the config file into the per-instance input dir
    tryCatch({
      if (dir.exists(in_dir)) write_run_config(in_dir, new_entry)
    }, error = function(e) cat("Config write warning:", conditionMessage(e), "\n"))

    # Signal the rest of the app that a run was committed
    shinyjs::runjs("Shiny.setInputValue('wizard_confirmed', Math.random(), {priority: 'event'})")
  })

  # ── Simulation Summary — cancel ───────────────────────────────────────────────
  observeEvent(input$sim_summary_cancel, {
    modal_toggle("sim_summary_modal", "hide")
    # Remove the temp input dir for the cancelled pending instance
    pending_idx <- as.integer(sim_num())
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    tmp_in <- instance_dirs[[as.character(pending_idx)]]$input
    if (!is.null(tmp_in) && dir.exists(tmp_in))
      unlink(tmp_in, recursive = TRUE)
    instance_dirs[[as.character(pending_idx)]] <- NULL
    assign("instance_dirs", instance_dirs, envir = .GlobalEnv)
    pending_entry(NULL)
    assign("instances", as.integer(data()$Simulation_No), envir = .GlobalEnv)
  })

  # ── Wizard cancelled before finishing ─────────────────────────────────────────
  observeEvent(input$wizard_cancelled, {
    pending_entry(NULL)
    # Remove the temp input dir for the cancelled pending instance
    pending_idx <- as.integer(sim_num())
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    tmp_in <- instance_dirs[[as.character(pending_idx)]]$input
    if (!is.null(tmp_in) && dir.exists(tmp_in))
      unlink(tmp_in, recursive = TRUE)
    instance_dirs[[as.character(pending_idx)]] <- NULL
    assign("instance_dirs", instance_dirs, envir = .GlobalEnv)
    assign("instances", as.integer(data()$Simulation_No), envir = .GlobalEnv)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Save simulation queue
  observeEvent(input$ok, {

    # Save the simulation metadata
    assign("simulation_data", data(), envir = .GlobalEnv)

    # Update instances whenever we save
    instances <- as.integer(data()$Simulation_No)
    assign("instances", instances, envir = .GlobalEnv)

    # Check if input_data_list exists in global environment
    input_data_exists <- exists("input_data_list", envir = .GlobalEnv)

    if (input_data_exists) {
      input_data_list <- get("input_data_list", envir = .GlobalEnv)
      num_configs <- length(input_data_list)

      cat("\n========================================\n")
      cat("DATA SAVED SUCCESSFULLY\n")
      cat("========================================\n")
      cat("Simulation metadata: simulation_data (", nrow(data()), " simulations)\n", sep = "")
      cat("Input configurations: input_data_list (", num_configs, " configurations)\n", sep = "")
      cat("Instance array: instances\n")
      cat("All data is ready for batch processing!\n")
      cat("========================================\n\n")

      showModal(modalDialog(
        title = "✅ Data Saved Successfully",
        HTML(paste0(
          "<p style='color: green; font-weight: bold;'>The simulation queue has been saved to the R environment.</p>",
          "<p><strong>Simulation metadata:</strong> <code>simulation_data</code> (", nrow(data()), " simulations)</p>",
          "<p><strong>Input configurations:</strong> <code>input_data_list</code> (", num_configs, " configurations)</p>",
          "<p><strong>Instance array:</strong> <code>instances</code></p>",
          "<hr>",
          "<p style='color: #155724;'>Click <strong>Run Queue</strong> to run the simulation.</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))

    } else {

      cat("\n========================================\n")
      cat("DATA SAVED WITH WARNING\n")
      cat("========================================\n")
      cat("Simulation metadata: simulation_data (", nrow(data()), " simulations)\n", sep = "")
      cat("Instance array: instances\n")
      cat("WARNING: No input configurations found in input_data_list.\n")
      cat("Make sure to complete the wizard/advanced setup for each simulation before running.\n")
      cat("========================================\n\n")

      showModal(modalDialog(
        title = "⚠️ Data Saved with Warning",
        HTML(paste0(
          "<p>The simulation queue has been saved to the R environment.</p>",
          "<p><strong>Simulation metadata:</strong> <code>simulation_data</code> (", nrow(data()), " simulations)</p>",
          "<p><strong>Instance array:</strong> <code>instances</code></p>",
          "<hr>",
          "<p style='color: orange;'><strong>Note:</strong> No input configurations found in ",
          "<code>input_data_list</code>. Make sure to complete configuration for each simulation.</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })

  # ── Reactive state for background SIACS process ──────────────────────────
  siacs_proc     <- reactiveVal(NULL)   # processx process object
  siacs_log      <- reactiveVal("")     # accumulated log text
  siacs_out_dir  <- reactiveVal(NULL)   # output folder for this run
  siacs_ol_orig  <- reactiveVal(NULL)   # original OutputList for restore

  # ── Child-script builder ────────────────────────────────────────────────
  # Returns a path to a tempfile containing the R script the background
  # Rscript will execute. The script writes step-by-step sentinel logs to
  # `siacs_child_steps.log` and `siacs_child_started.log` so we can tell
  # exactly where a silent crash occurred (before, during, or after each
  # of: load .RData, source SIACS_0924_Merged_2.R, etc.).
  #
  # rdata_file: path to the saved .RData with input_data_list, instances, etc.
  build_siacs_child_script <- function(rdata_file) {
    project_dir       <- getwd()
    diag_helper       <- file.path(project_dir, "siacs_diagnostics.R")
    child_started_log <- file.path(project_dir, "siacs_child_started.log")
    child_steps_log   <- file.path(project_dir, "siacs_child_steps.log")
    siacs_engine      <- file.path(project_dir, "SIACS_0924_Merged_2.R")

    script_file <- tempfile(fileext = ".R")
    writeLines(c(
      # ── Sentinel 0: confirm the child Rscript even launched ─────────────
      # Write a sentinel BEFORE anything else. If this file appears but
      # siacs_child_steps.log does not, the child died loading the diag
      # helper. If neither appears, Rscript itself never launched.
      paste0("writeLines(paste0('child started at ', Sys.time()), ",
             deparse(child_started_log), ")"),

      # Source the base-R-only diagnostics helper
      paste0("source(", deparse(diag_helper), ")"),
      paste0("CHILD_LOG <- ", deparse(child_steps_log)),
      paste0("siacs_log_init(CHILD_LOG, 'SIACS child Rscript')"),
      "siacs_log_line(CHILD_LOG, '[child] sentinel 0: helper sourced')",

      # ── Sentinel 1: setwd to project dir ────────────────────────────────
      paste0("siacs_log_line(CHILD_LOG, paste0('[child] setwd to: ', ",
             deparse(project_dir), "))"),
      paste0("setwd(", deparse(project_dir), ")"),

      # ── Sentinel 2: load .RData with run inputs ─────────────────────────
      paste0("siacs_log_line(CHILD_LOG, paste0('[child] loading RData: ', ",
             deparse(rdata_file), "))"),
      paste0("tryCatch(load(", deparse(rdata_file), "),",
             "  error = function(e) { siacs_log_line(CHILD_LOG,",
             "    paste0('[child] FATAL load failed: ', conditionMessage(e)));",
             "    stop(e) })"),
      "siacs_log_line(CHILD_LOG, '[child] sentinel 2: RData loaded OK')",

      # ── Sentinel 3: optional-light key state diagnostic ─────────────────
      "siacs_log_line(CHILD_LOG, '[child] sentinel 3: optional-light keys')",
      "cat('\\n=== CHILD PROCESS: optional-light key check ===\\n')",
      "opt_keys <- c('IndoorLight','OutdoorLightDirect','OutdoorLightDiffuse','ArtificialLight','InitialValues')",
      "for (.k in opt_keys) {",
      "  .v <- input_data_list[[.k]]",
      "  if (is.null(.v)) { cat(sprintf('  %-28s: NULL\\n', .k)) }",
      "  else if (is.list(.v)) {",
      "    .t <- vapply(.v, function(x) if(is.null(x)) 'NULL' else if(is.data.frame(x)) sprintf('df(%dx%d)',nrow(x),ncol(x)) else sprintf('%s(len=%d)',class(x)[1L],length(x)), character(1L))",
      "    cat(sprintf('  %-28s: list[%s]\\n', .k, paste(.t, collapse=',')))",
      "  } else { cat(sprintf('  %-28s: %s\\n', .k, class(.v)[1L])) }",
      "}",
      "cat('=== END CHILD DIAGNOSTICS ===\\n')",

      # ── Sentinel 4: about to source the SIACS engine ────────────────────
      paste0("siacs_log_line(CHILD_LOG, paste0('[child] sentinel 4: ",
             "about to source engine: ', ", deparse(siacs_engine), "))"),

      # withCallingHandlers fires BEFORE SIACS's internal tryCatch, so we
      # capture a full call stack at the exact point of any error. The
      # error handler also writes to the steps log so a hard parent-side
      # truncation of stdout still leaves us a forensic trail on disk.
      "withCallingHandlers(",
      paste0("  source(", deparse(siacs_engine), "),"),
      "  error = function(.e) {",
      "    .msg <- paste0('[child] ENGINE ERROR: ', conditionMessage(.e))",
      "    siacs_log_line(CHILD_LOG, .msg)",
      "    cat('\\n!!! DIAGNOSTIC TRACEBACK !!!\\n')",
      "    cat('Error:', conditionMessage(.e), '\\n')",
      "    .calls <- sys.calls()",
      "    for (.i in seq_along(.calls)) {",
      "      .frame <- paste(deparse(.calls[[.i]], nlines=3L), collapse=' ')",
      "      cat(sprintf('  frame %d: %s\\n', .i, .frame))",
      "      siacs_log_line(CHILD_LOG, sprintf('[child] frame %d: %s', .i, .frame))",
      "    }",
      "    cat('!!! END TRACEBACK !!!\\n')",
      "  }",
      ")",

      # ── Sentinel 5: engine returned cleanly ─────────────────────────────
      "siacs_log_line(CHILD_LOG, '[child] sentinel 5: engine source returned')"
    ), script_file)
    script_file
  }

  # ── Launcher with safety net ────────────────────────────────────────────
  # Wraps process$new() so a spawn failure surfaces in the GUI instead of
  # propagating up and freezing the Shiny session.
  launch_siacs_child <- function(script_file) {
    rscript_bin <- file.path(R.home("bin"), "Rscript")
    siacs_log(paste0(siacs_log(),
      "[parent] launching child Rscript: ", rscript_bin, "\n",
      "[parent] script: ", script_file, "\n"))
    proc <- tryCatch(
      process$new(
        command = rscript_bin,
        # NOTE: replaced --vanilla with --no-save --no-restore. --vanilla
        # implies --no-environ which suppresses .Renviron, so user-installed
        # packages under R_LIBS_USER become invisible to the child — a
        # frequent cause of silent child crashes.
        args    = c("--no-save", "--no-restore", script_file),
        stdout  = "|",
        stderr  = "|"
      ),
      error = function(e) {
        siacs_log(paste0(siacs_log(),
          "\n[parent] FATAL: failed to spawn Rscript: ",
          conditionMessage(e), "\n"))
        showModal(modalDialog(
          title = "Cannot launch simulation",
          HTML(paste0(
            "<p>Failed to start the background R process:</p>",
            "<pre>", htmltools::htmlEscape(conditionMessage(e)), "</pre>",
            "<p>Check that R is installed and that <code>",
            htmltools::htmlEscape(rscript_bin),
            "</code> exists.</p>",
            "<p>See <code>siacs_startup.log</code> in the project folder ",
            "for diagnostic details.</p>"
          )),
          easyClose = TRUE, footer = modalButton("Close")
        ))
        NULL
      }
    )
    proc
  }

  # Expose running state to UI (drives conditionalPanel)
  output$siacs_running <- reactive({ !is.null(siacs_proc()) })
  outputOptions(output, "siacs_running", suspendWhenHidden = FALSE)

  output$siacs_log <- renderText({ siacs_log() })

  # Poll every 500 ms while a process is alive
  observe({
    proc <- siacs_proc()
    if (is.null(proc)) return()
    invalidateLater(500, session)

    # Read any new stdout
    new_out <- tryCatch(proc$read_output(), error = function(e) "")
    new_err <- tryCatch(proc$read_error(),  error = function(e) "")
    new_text <- paste0(new_out, new_err)
    if (nzchar(new_text)) {
      siacs_log(paste0(siacs_log(), new_text))
      session$sendCustomMessage("scroll_log", list())
    }

    # Check if process has finished
    if (!proc$is_alive()) {
      # Drain any remaining output
      final_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      final_err <- tryCatch(proc$read_error_lines(),  error = function(e) character(0))
      final_text <- paste(c(final_out, final_err), collapse = "\n")
      if (nzchar(final_text))
        siacs_log(paste0(siacs_log(), "\n", final_text))

      # Restore original OutputList
      ol_orig <- siacs_ol_orig()
      if (!is.null(ol_orig))
        assign("OutputList", ol_orig, envir = .GlobalEnv)

      # FIX: siacs_out_dir() is now a character vector of per-instance dirs.
      # Iterate each dir individually for dir.exists() and file counting.
      out_dirs_vec <- siacs_out_dir()
      exit_code    <- tryCatch(proc$get_exit_status(), error = function(e) NA)

      siacs_proc(NULL)   # mark as done

      # Build one summary line per output directory
      dir_summaries <- if (!is.null(out_dirs_vec) && length(out_dirs_vec) > 0) {
        paste(sapply(out_dirs_vec, function(od) {
          n_files <- if (dir.exists(od)) length(list.files(od)) else 0
          paste0("<p style='color:#155724;background:#d4edda;padding:8px;border-radius:4px;margin-bottom:4px;'>",
                 "\U0001f4c1 Outputs saved to: <strong>", od, "</strong>",
                 " (", n_files, " file(s))</p>")
        }), collapse = "")
      } else ""

      showModal(modalDialog(
        title = if (isTRUE(exit_code == 0)) "\u2705 Simulation Complete" else "\u26a0\ufe0f Simulation Finished",
        HTML(paste0(
          dir_summaries,
          "<p style='color:#666;font-size:12px;'>",
          "Full log is shown in the panel above. Exit code: ", exit_code, "</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  # Run Queue — launches SIACS_0924_Merged_2.R in a background R process
  observeEvent(input$run_queue, {

    if (nrow(data()) == 0) {
      showModal(modalDialog(
        title = "Error",
        "No simulations in the queue. Please add at least one simulation.",
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return()
    }

    if (!exists("input_data_list", envir = .GlobalEnv) ||
        !exists("instances",       envir = .GlobalEnv)) {
      showModal(modalDialog(
        title = "Error",
        "No saved input configurations found. Please configure and save at least one simulation first.",
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return()
    }

    if (!is.null(siacs_proc()) && siacs_proc()$is_alive()) {
      showModal(modalDialog(
        title = "Already Running",
        "A simulation is already in progress. Please wait for it to finish.",
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return()
    }

    # ── Validate all input data files for every queued instance ─────────────
    queue_errors   <- character(0)
    queue_warnings <- character(0)

    input_data_list <- get("input_data_list", envir = .GlobalEnv)
    instances_vec   <- as.integer(get("instances",       envir = .GlobalEnv))

    # file_type_map maps integer position → validation type string (from shared.R)
    # titles_advanced gives the human-readable names at the same positions
    for (inst in instances_vec) {
      for (i in seq_along(file_type_map)) {
        ftype    <- file_type_map[i]
        ftitle   <- titles_advanced[i]
        data_key <- gsub(" ", "", titles_advanced[i])

        # Retrieve the data frame for this instance and file type
        df <- tryCatch({
          idl <- input_data_list[[data_key]]
          if (is.list(idl) && !is.data.frame(idl)) idl[[inst]] else idl
        }, error = function(e) NULL)

        if (is.null(df) || !is.data.frame(df)) next

        res <- tryCatch(
          validate_data_file(df, ftype),
          error = function(e) NULL
        )
        if (is.null(res)) next

        label <- paste0("Sim ", inst, " / ", ftitle)
        if (length(res$errors)   > 0)
          queue_errors   <- c(queue_errors,   paste0("[", label, "] ", res$errors))
        if (length(res$warnings) > 0)
          queue_warnings <- c(queue_warnings, paste0("[", label, "] ", res$warnings))
      }
    }

    if (length(queue_errors) > 0) {
      showModal(modalDialog(
        title = HTML("<span style='color:#7b0000;'>&#9888; Cannot Run — Input Data Errors Found</span>"),
        HTML(paste0(
          "<p>The following errors were found in the queued simulation data. ",
          "Please correct them before running:</p>",
          "<div style='max-height:400px;overflow-y:auto;'>",
          "<ul style='padding-left:20px;'>",
          paste0("<li style='color:#7b0000;margin-bottom:4px;'>", queue_errors, "</li>", collapse = ""),
          "</ul>",
          if (length(queue_warnings) > 0) paste0(
            "<hr><p style='color:#7d4e00;'><b>&#9432; Warnings (non-blocking, shown for reference):</b></p>",
            "<ul style='padding-left:20px;'>",
            paste0("<li style='color:#7d4e00;'>", queue_warnings, "</li>", collapse = ""),
            "</ul>") else "",
          "</div>"
        )),
        size = "l", easyClose = TRUE, footer = modalButton("Go Back and Fix")
      ))
      return()
    }

    if (length(queue_warnings) > 0) {
      # Warnings are non-blocking — log them to the simulation log and proceed.
      # (Blocking the run on warnings like duration mismatches causes silent
      # failures when users miss the modal. Warnings are shown in the run log.)
      siacs_log(paste0(
        "⚠️ Warnings (non-blocking):
",
        paste0("  - ", queue_warnings, collapse = "
"), "
",
        "─────────────────
"
      ))
    }

    output_path_keys <- c("OutputTable", "OutputBasicChart",
                          "OutputTimeDerivatives", "OutputMassBalanceComponents",
                          "OutputSensitivity", "OutputUncertainty")

    # ── Compact sparse lists so the model always sees sequential 1..N indices ──
    # FIX: save orig_instances BEFORE compaction — instance_dirs_run is keyed
    # by original Simulation_No values, not the remapped 1..N positions.
    orig_instances   <- as.integer(get("instances",       envir = .GlobalEnv))
    idl_run          <- get("input_data_list", envir = .GlobalEnv)
    ol_run           <- if (exists("OutputList", envir = .GlobalEnv)) get("OutputList", envir = .GlobalEnv) else list()
    compacted        <- compact_for_run(orig_instances, idl_run, ol_run)
    instances_run    <- compacted$instances   # now always 1..N
    idl_run          <- compacted$input_data_list
    ol_run           <- compacted$OutputList

    # ── Create per-instance output dirs and redirect OutputList paths ────────
    instance_dirs_run <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    ts_fallback <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    # FIX: iterate over orig_instances (original Simulation_No values) so the
    # lookup into instance_dirs_run (keyed by original sim number) succeeds.
    for (new_pos in seq_along(orig_instances)) {
      orig_idx <- orig_instances[new_pos]
      od <- instance_dirs_run[[as.character(orig_idx)]]$output %||%
            paste0("Output_", orig_idx, "_", ts_fallback)
      dir.create(od, recursive = TRUE, showWarnings = FALSE)
      instance_dirs_run[[as.character(orig_idx)]]$output <- od
    }
    assign("instance_dirs", instance_dirs_run, envir = .GlobalEnv)

    # Stamp per-instance output paths in the compacted OutputList
    siacs_ol_orig(get("OutputList", envir = .GlobalEnv))  # save original for restore
    for (new_pos in seq_along(ol_run)) {
      # FIX: use orig_instances[new_pos] to look up instance_dirs_run, not
      # instances_run[new_pos] (which is just new_pos after compaction).
      orig_idx <- orig_instances[new_pos]
      od <- instance_dirs_run[[as.character(orig_idx)]]$output %||%
            paste0("Output_", orig_idx, "_", ts_fallback)
      for (key in output_path_keys) {
        val <- ol_run[[new_pos]][[key]]
        if (!is.null(val) && !identical(val, "None") && nzchar(trimws(val)))
          ol_run[[new_pos]][[key]] <- file.path(od, basename(val))
      }
    }

    # ── Save all globals the child process needs into a temp .RData file ────
    rdata_file        <- tempfile(fileext = ".RData")
    run_env           <- new.env(parent = emptyenv())
    run_env$instances      <- instances_run
    run_env$input_data_list <- idl_run
    run_env$OutputList      <- ol_run
    # FIX: export compact input-dir vector (1..N, matching instances_run) so the
    # child process saves auto-generated light files into the correct Input_ folder
    # rather than the working directory.
    # Build a named list of input dirs indexed 1..N (matching instances_run).
    # Using a list (not sapply vector) ensures [[i]] positional indexing is safe
    # even when dir paths contain special characters.
    run_env$instance_input_dirs <- lapply(seq_along(orig_instances), function(i) {
      orig_idx <- orig_instances[i]
      d <- instance_dirs_run[[as.character(orig_idx)]]$input
      if (!is.null(d) && nzchar(d)) d else getwd()
    })
    # Also carry over any other needed globals
    for (v in c("mechanism", "perturbation", "chemistry", "SIACSVersion")) {
      if (exists(v, envir = .GlobalEnv)) assign(v, get(v, envir = .GlobalEnv), envir = run_env)
    }
    save(list = ls(run_env), file = rdata_file, envir = run_env)

    # ── Build the R script the child will run ───────────────────────────────
    # Helper writes a tempfile with sentinel logging at every step. See
    # build_siacs_child_script() for the per-step diagnostic trail.
    script_file <- build_siacs_child_script(rdata_file)

    # ── Launch background process ───────────────────────────────────────────
    proc <- launch_siacs_child(script_file)
    if (is.null(proc)) return()

    # FIX: store a character vector of individual dirs (not a comma-joined
    # string) so dir.exists() and dirname() work correctly in the poll observer.
    run_out_dirs_vec <- sapply(seq_along(orig_instances), function(i) {
      orig_idx <- orig_instances[i]
      instance_dirs_run[[as.character(orig_idx)]]$output %||%
        paste0("Output_", orig_idx)
    })
    siacs_log(paste0("▶ Started: ", paste(run_out_dirs_vec, collapse = ", "), "\n",
                     "─────────────────────────────────\n"))
    siacs_out_dir(run_out_dirs_vec)
    siacs_proc(proc)
  })

  # Fires when user clicks "Run Anyway" from the warnings-only modal
  observeEvent(input$run_queue_confirmed, {
    removeModal()

    # Guard: check still-alive (user could have re-triggered somehow)
    if (!is.null(siacs_proc()) && siacs_proc()$is_alive()) {
      showModal(modalDialog(
        title = "Already Running",
        "A simulation is already in progress. Please wait for it to finish.",
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return()
    }

    output_path_keys <- c("OutputTable", "OutputBasicChart",
                          "OutputTimeDerivatives", "OutputMassBalanceComponents",
                          "OutputSensitivity", "OutputUncertainty")

    # ── Compact sparse lists so the model always sees sequential 1..N indices ──
    # FIX: save orig_instances BEFORE compaction — instance_dirs_run is keyed
    # by original Simulation_No values, not the remapped 1..N positions.
    orig_instances   <- as.integer(get("instances",       envir = .GlobalEnv))
    idl_run          <- get("input_data_list", envir = .GlobalEnv)
    ol_run           <- if (exists("OutputList", envir = .GlobalEnv)) get("OutputList", envir = .GlobalEnv) else list()
    compacted        <- compact_for_run(orig_instances, idl_run, ol_run)
    instances_run    <- compacted$instances   # now always 1..N
    idl_run          <- compacted$input_data_list
    ol_run           <- compacted$OutputList

    # ── Create per-instance output dirs and redirect OutputList paths ────────
    instance_dirs_run <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    ts_fallback <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    # FIX: iterate over orig_instances (original Simulation_No values) so the
    # lookup into instance_dirs_run (keyed by original sim number) succeeds.
    for (new_pos in seq_along(orig_instances)) {
      orig_idx <- orig_instances[new_pos]
      od <- instance_dirs_run[[as.character(orig_idx)]]$output %||%
            paste0("Output_", orig_idx, "_", ts_fallback)
      dir.create(od, recursive = TRUE, showWarnings = FALSE)
      instance_dirs_run[[as.character(orig_idx)]]$output <- od
    }
    assign("instance_dirs", instance_dirs_run, envir = .GlobalEnv)

    # Stamp per-instance output paths in the compacted OutputList
    siacs_ol_orig(get("OutputList", envir = .GlobalEnv))  # save original for restore
    for (new_pos in seq_along(ol_run)) {
      # FIX: use orig_instances[new_pos] to look up instance_dirs_run, not
      # instances_run[new_pos] (which is just new_pos after compaction).
      orig_idx <- orig_instances[new_pos]
      od <- instance_dirs_run[[as.character(orig_idx)]]$output %||%
            paste0("Output_", orig_idx, "_", ts_fallback)
      for (key in output_path_keys) {
        val <- ol_run[[new_pos]][[key]]
        if (!is.null(val) && !identical(val, "None") && nzchar(trimws(val)))
          ol_run[[new_pos]][[key]] <- file.path(od, basename(val))
      }
    }

    # ── Save all globals the child process needs into a temp .RData file ────
    rdata_file        <- tempfile(fileext = ".RData")
    run_env           <- new.env(parent = emptyenv())
    run_env$instances      <- instances_run
    run_env$input_data_list <- idl_run
    run_env$OutputList      <- ol_run
    # FIX: export compact input-dir vector so the child saves auto-generated
    # light files into the correct Input_ folder rather than the working directory.
    # Build a named list of input dirs indexed 1..N (matching instances_run).
    # Using a list (not sapply vector) ensures [[i]] positional indexing is safe
    # even when dir paths contain special characters.
    run_env$instance_input_dirs <- lapply(seq_along(orig_instances), function(i) {
      orig_idx <- orig_instances[i]
      d <- instance_dirs_run[[as.character(orig_idx)]]$input
      if (!is.null(d) && nzchar(d)) d else getwd()
    })
    # Also carry over any other needed globals
    for (v in c("mechanism", "perturbation", "chemistry", "SIACSVersion")) {
      if (exists(v, envir = .GlobalEnv)) assign(v, get(v, envir = .GlobalEnv), envir = run_env)
    }
    save(list = ls(run_env), file = rdata_file, envir = run_env)

    # ── Build the R script the child will run ───────────────────────────────
    script_file <- build_siacs_child_script(rdata_file)

    # ── Launch background process ───────────────────────────────────────────
    proc <- launch_siacs_child(script_file)
    if (is.null(proc)) return()

    # FIX: store a character vector of individual dirs (not a comma-joined
    # string) so dir.exists() and dirname() work correctly in the poll observer.
    run_out_dirs_vec <- sapply(seq_along(orig_instances), function(i) {
      orig_idx <- orig_instances[i]
      instance_dirs_run[[as.character(orig_idx)]]$output %||%
        paste0("Output_", orig_idx)
    })
    siacs_log(paste0("▶ Started: ", paste(run_out_dirs_vec, collapse = ", "), "\n",
                     "─────────────────────────────────\n"))
    siacs_out_dir(run_out_dirs_vec)
    siacs_proc(proc)
  })

  # ==========================================================================
  # SIMULATION QUEUE TABLE (DT)
  # ==========================================================================

  # DT table with single-row selection
  output$queue_table <- DT::renderDataTable({
    df <- data()
    display <- df[, c("Simulation_No","Run_Name","Location","Duration","Chemical_Model")]
    colnames(display) <- c("No.", "Run Name", "Location", "Duration (h)", "Chemical Model")
    DT::datatable(
      display,
      selection  = "single",
      rownames   = FALSE,
      class      = "dt-queue cell-border stripe hover",
      options    = list(
        pageLength = 25,
        dom        = "tip",   # table + info + pagination only (no search box)
        scrollX    = FALSE,
        columnDefs = list(list(width = "40px", targets = 0))
      )
    )
  })

  # Reactive: currently selected Simulation_No (NULL if nothing selected)
  selected_sim_no <- reactive({
    sel <- input$queue_table_rows_selected
    if (length(sel) == 0) return(NULL)
    as.integer(data()$Simulation_No[sel])
  })

  # Enable/disable Edit, Copy, Remove based on selection
  observe({
    has_sel <- !is.null(selected_sim_no())
    has_rows <- nrow(data()) > 0
    if (has_sel) {
      shinyjs::enable("edit_sim")
      shinyjs::enable("copy_sim")
      shinyjs::enable("remove_sim")
    } else {
      shinyjs::disable("edit_sim")
      shinyjs::disable("copy_sim")
      shinyjs::disable("remove_sim")
    }
  })

  # ==========================================================================
  # REMOVE (toolbar button — replaces old inline remove_sim_id approach)
  # ==========================================================================
  # Helper that does the actual reindex work, keyed by Simulation_No
  do_remove_sim <- function(sim_id_to_remove) {
    current <- data()
    row_idx  <- which(current$Simulation_No == sim_id_to_remove)
    if (length(row_idx) == 0) return()

    old_ids  <- current$Simulation_No
    kept_ids <- old_ids[-row_idx]

    # 1. Drop the row and renumber Simulation_No to 1..N
    updated <- current[-row_idx, , drop = FALSE]
    updated$Simulation_No <- seq_len(nrow(updated))
    data(updated)

    # 2. Reindex input_data_list sub-lists to new 1..N positions
    if (exists("input_data_list", envir = .GlobalEnv)) {
      idl <- get("input_data_list", envir = .GlobalEnv)
      for (key in names(idl)) {
        sub <- idl[[key]]
        if (is.list(sub) && !is.data.frame(sub)) {
          new_sub <- vector("list", length(kept_ids))
          for (new_pos in seq_along(kept_ids)) {
            slot <- kept_ids[new_pos]
            new_sub[[new_pos]] <- if (slot <= length(sub)) sub[[slot]] else NULL
          }
          idl[[key]] <- new_sub
        }
      }
      assign("input_data_list", idl, envir = .GlobalEnv)
    }

    # 3. Reindex OutputList
    if (exists("OutputList", envir = .GlobalEnv)) {
      ol <- get("OutputList", envir = .GlobalEnv)
      new_ol <- vector("list", length(kept_ids))
      for (new_pos in seq_along(kept_ids))
        new_ol[[new_pos]] <- if (kept_ids[new_pos] <= length(ol)) ol[[kept_ids[new_pos]]] else list()
      assign("OutputList", new_ol, envir = .GlobalEnv)
    }

    # 4. Update instances & sim_num
    assign("instances", seq_len(nrow(updated)), envir = .GlobalEnv)
    sim_num(nrow(updated) + 1L)

    # 5. Delete on-disk input dir; rebuild instance_dirs with sequential keys
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    removed_in <- instance_dirs[[as.character(sim_id_to_remove)]]$input
    if (!is.null(removed_in) && dir.exists(removed_in)) {
      unlink(removed_in, recursive = TRUE)
      cat("Deleted input dir for removed sim", sim_id_to_remove, ":", removed_in, "\n")
    }
    new_dirs <- list()
    for (new_pos in seq_along(kept_ids)) {
      old_id <- kept_ids[new_pos]
      new_dirs[[as.character(new_pos)]] <- instance_dirs[[as.character(old_id)]]
    }
    assign("instance_dirs", new_dirs, envir = .GlobalEnv)

    # 6. Reindex wizard state snapshots to new sequential keys
    if (exists("sim_wizard_snapshots", envir = .GlobalEnv)) {
      old_snaps <- get("sim_wizard_snapshots", envir = .GlobalEnv)
      new_snaps <- list()
      for (new_pos in seq_along(kept_ids)) {
        old_key <- as.character(kept_ids[new_pos])
        if (!is.null(old_snaps[[old_key]]))
          new_snaps[[as.character(new_pos)]] <- old_snaps[[old_key]]
      }
      assign("sim_wizard_snapshots", new_snaps, envir = .GlobalEnv)
    }
  }

  observeEvent(input$remove_sim, {
    sim_no <- selected_sim_no()
    if (is.null(sim_no)) return()
    do_remove_sim(sim_no)
  })

  # Keep legacy remove_sim_id signal working (used by any existing inline buttons)
  observeEvent(input$remove_sim_id, {
    do_remove_sim(as.integer(input$remove_sim_id))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ==========================================================================
  # EDIT — Re-open the original wizard or advanced editor for the selected sim
  # ==========================================================================
  # Track which sim_no is being edited
  editing_sim_no <- reactiveVal(NULL)

  observeEvent(input$edit_sim, {
    sim_no <- selected_sim_no()
    if (is.null(sim_no)) return()
    row    <- data()[data()$Simulation_No == sim_no, ]
    editing_sim_no(sim_no)

    # Creation_Method stores "standard" (wizard) or "advanced"
    method <- if ("Creation_Method" %in% names(row) && nzchar(row$Creation_Method %||% ""))
      row$Creation_Method else "standard"

    if (method != "advanced") {
      # Standard / wizard-created simulation — reopen the wizard
      assign("wizard_edit_sim_no", sim_no, envir = .GlobalEnv)
      assign("wizard_edit_mode",   TRUE,   envir = .GlobalEnv)
      shinyjs::runjs("Shiny.setInputValue('wizard_edit_trigger', Math.random(), {priority: 'event'})")
    } else {
      # Advanced-created simulation — reopen the Advanced inputs modal
      assign("advanced_edit_sim_no", sim_no,              envir = .GlobalEnv)
      assign("advanced_edit_mode",   TRUE,                envir = .GlobalEnv)
      updateRadioButtons(session, "adv_mechanism", selected = row$Chemical_Model %||% "SAPRC99")
      shinyjs::runjs("Shiny.setInputValue('advanced_edit_trigger', Math.random(), {priority: 'event'})")
    }
  })

  # Wizard completed in edit mode — update queue row, skip summary modal
  observeEvent(input$wizard_edit_confirmed, {
    sim_no <- editing_sim_no()
    if (is.null(sim_no)) return()
    fresh <- if (exists("pending_sim_metadata", envir = .GlobalEnv))
      get("pending_sim_metadata", envir = .GlobalEnv)
    else list(mechanism = "SAPRC99", duration = NA)
    df      <- data()
    row_idx <- which(df$Simulation_No == sim_no)
    if (length(row_idx) > 0) {
      df$Chemical_Model[row_idx] <- fresh$mechanism %||% df$Chemical_Model[row_idx]
      dur <- tryCatch(as.numeric(fresh$duration), warning = function(e) NA, error = function(e) NA)
      if (!is.na(dur)) df$Duration[row_idx] <- dur
    }
    data(df)
    editing_sim_no(NULL)
    if (exists("wizard_edit_mode",   envir = .GlobalEnv)) rm("wizard_edit_mode",   envir = .GlobalEnv)
    if (exists("wizard_edit_sim_no", envir = .GlobalEnv)) rm("wizard_edit_sim_no", envir = .GlobalEnv)
    showNotification("Simulation updated successfully.", type = "message", duration = 3)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # (advanced_edit_trigger is handled inside advanced_module.R which pre-loads
  #  data and opens the modal — no duplicate observer needed here)

  # Advanced completed in edit mode — update queue row, skip summary modal
  observeEvent(input$advanced_edit_confirmed, {
    sim_no <- editing_sim_no()
    if (is.null(sim_no)) return()
    fresh <- if (exists("pending_sim_metadata", envir = .GlobalEnv))
      get("pending_sim_metadata", envir = .GlobalEnv)
    else list(mechanism = "SAPRC99", duration = NA)
    df      <- data()
    row_idx <- which(df$Simulation_No == sim_no)
    if (length(row_idx) > 0) {
      df$Chemical_Model[row_idx] <- fresh$mechanism %||% df$Chemical_Model[row_idx]
      dur <- tryCatch(as.numeric(fresh$duration), warning = function(e) NA, error = function(e) NA)
      if (!is.na(dur)) df$Duration[row_idx] <- dur
    }
    data(df)
    editing_sim_no(NULL)
    if (exists("advanced_edit_mode",   envir = .GlobalEnv)) rm("advanced_edit_mode",   envir = .GlobalEnv)
    if (exists("advanced_edit_sim_no", envir = .GlobalEnv)) rm("advanced_edit_sim_no", envir = .GlobalEnv)
    showNotification("Simulation updated successfully.", type = "message", duration = 3)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ==========================================================================
  # COPY — deep-copy metadata + input files to a new folder
  # ==========================================================================
  observeEvent(input$copy_sim, {
    sim_no <- selected_sim_no()
    if (is.null(sim_no)) return()

    src_row <- data()[data()$Simulation_No == sim_no, ]
    new_no  <- as.integer(sim_num())
    new_name <- paste0(src_row$Run_Name, "_copy")

    # Make new name unique if needed
    existing_names <- data()$Run_Name
    base_copy <- new_name
    counter   <- 1
    while (tolower(new_name) %in% tolower(existing_names)) {
      new_name <- paste0(base_copy, counter)
      counter  <- counter + 1
    }

    # Deep-copy input files to a new per-instance directory
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    src_in  <- instance_dirs[[as.character(sim_no)]]$input
    prefix  <- make_dir_prefix(new_name)
    new_in  <- paste0("Input_",  prefix)
    new_out <- paste0("Output_", prefix)

    if (!is.null(src_in) && dir.exists(src_in)) {
      tryCatch({
        dir.create(new_in, recursive = TRUE, showWarnings = FALSE)
        src_files <- list.files(src_in, full.names = TRUE, recursive = FALSE)
        file.copy(src_files, new_in)
        cat("Copied input dir", src_in, "->", new_in, "\n")
      }, error = function(e) {
        showNotification(paste("Could not copy input files:", conditionMessage(e)), type = "error")
        return()
      })
    } else {
      dir.create(new_in, recursive = TRUE, showWarnings = FALSE)
    }

    # Register new instance_dirs entry
    instance_dirs[[as.character(new_no)]] <- list(input = new_in, output = new_out)
    assign("instance_dirs", instance_dirs, envir = .GlobalEnv)

    # Deep-copy input_data_list entries for this instance
    if (exists("input_data_list", envir = .GlobalEnv)) {
      idl <- get("input_data_list", envir = .GlobalEnv)
      for (key in names(idl)) {
        sub <- idl[[key]]
        if (is.list(sub) && !is.data.frame(sub) && sim_no <= length(sub))
          idl[[key]][[new_no]] <- sub[[sim_no]]
      }
      assign("input_data_list", idl, envir = .GlobalEnv)
    }

    # Extend OutputList
    if (exists("OutputList", envir = .GlobalEnv)) {
      ol <- get("OutputList", envir = .GlobalEnv)
      if (length(ol) < new_no) length(ol) <- new_no
      ol[[new_no]] <- if (sim_no <= length(ol)) ol[[sim_no]] else list()
      assign("OutputList", ol, envir = .GlobalEnv)
    }

    # Add new row to queue
    new_row <- src_row
    new_row$Simulation_No <- new_no
    new_row$Run_Name      <- new_name
    data(rbind(data(), new_row))
    assign("instances", as.integer(data()$Simulation_No), envir = .GlobalEnv)
    sim_num(new_no + 1L)

    showNotification(paste0("Copied as \"", new_name, "\""), type = "message", duration = 3)
  })

  # ==========================================================================
  # CLEAR QUEUE
  # ==========================================================================
  observeEvent(input$clear_queue, {
    if (nrow(data()) == 0) {
      showNotification("Queue is already empty.", type = "message", duration = 3)
      return()
    }
    modal_toggle("clear_queue_modal", "show")
  })

  observeEvent(input$clear_queue_confirm, {
    # Delete all input dirs on disk
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    for (entry in instance_dirs) {
      if (!is.null(entry$input) && dir.exists(entry$input))
        unlink(entry$input, recursive = TRUE)
    }
    # Reset all state
    data(data.frame(
      Simulation_No = character(), Run_Name = character(),
      Location = character(), Duration = numeric(), Chemical_Model = character(),
      stringsAsFactors = FALSE
    ))
    assign("instances",       integer(0), envir = .GlobalEnv)
    assign("instance_dirs",   list(),     envir = .GlobalEnv)
    assign("input_data_list", list(),     envir = .GlobalEnv)
    assign("OutputList",      list(),     envir = .GlobalEnv)
    sim_num(1L)
    modal_toggle("clear_queue_modal", "hide")
    showNotification("Queue cleared.", type = "message", duration = 3)
  })

  observeEvent(input$clear_queue_cancel, {
    modal_toggle("clear_queue_modal", "hide")
  })

  # Launch module servers
  advanced_module_server(input, output, session, get_duration, set_duration)
  wizard_module_server(input, output, session)

  # ==========================================================================
  # RESULTS TAB
  # ==========================================================================

  # Reactive storage for loaded result data
  results_data      <- reactiveVal(NULL)   # data frame: time + all columns
  results_out_cols  <- reactiveVal(NULL)   # character: outdoor col names (end in .O)
  results_file_path <- reactiveVal(NULL)   # path that was loaded

  output$results_loaded <- reactive({ !is.null(results_data()) })
  outputOptions(output, "results_loaded", suspendWhenHidden = FALSE)

  # ── shinyFiles: map available volumes for the folder browser ──────────────
  volumes <- c(
    "Working dir" = getwd(),
    getVolumes()()           # adds drive roots (C:\, D:\, / etc.)
  )
  shinyDirChoose(input, "results_browse",
    roots   = volumes,
    session = session,
    restrictions = system.file(package = "base"))

  # When the user picks a folder via Browse, update the text input
  observeEvent(input$results_browse, {
    req(!is.integer(input$results_browse))   # fires before any selection
    path <- parseDirPath(volumes, input$results_browse)
    if (length(path) > 0 && nzchar(path)) {
      updateTextInput(session, "results_folder", value = normalizePath(path, winslash = "/"))
    }
  }, ignoreInit = TRUE)

  # ── Core helper: scan a folder for result CSVs ────────────────────────────
  # Logic: look for Output_* sub-dirs inside root. If found, use the most
  # recent one. If none found, treat root itself as the output dir.
  # Returns named character vector (display label → full path) or character(0).
  scan_results_folder <- function(root) {
    root <- normalizePath(root, winslash = "/", mustWork = FALSE)
    if (!dir.exists(root)) return(character(0))

    # Look for Output_<RunName>_YYYY-MM-DD_HHMMSS sub-dirs
    subdirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    out_dirs <- subdirs[grepl("Output_.*_\\d{4}-\\d{2}-\\d{2}_\\d{6}$", basename(subdirs))]

    target_dir <- if (length(out_dirs) > 0) {
      # Use the most recent Output_* subfolder
      sort(out_dirs)[length(out_dirs)]
    } else {
      # No subfolders — treat root itself as the output dir
      root
    }

    excl <- "(Sensitivity|Uncertainty|Derivative|MassBalance|LightDirect|LightDiffuse|ArtificialLight)"
    f    <- list.files(target_dir, pattern = "\\.csv$", full.names = TRUE)
    f    <- f[!grepl(excl, basename(f))]
    if (length(f) == 0) return(character(0))

    # Label: "RunFolder / filename.csv"  so the user knows where it came from
    folder_label <- basename(target_dir)
    setNames(f, paste0(folder_label, " / ", basename(f)))
  }

  # ── Populate file selector ─────────────────────────────────────────────────
  # show_warning controls whether we display a "no CSVs found" toast. We set
  # this FALSE for automatic scans (init, polling) so the user is not spammed
  # with warnings about empty Output_* folders before any sim has run. Only
  # explicit user actions (Scan button) and post-run completion show toasts.
  do_scan <- function(show_warning = FALSE) {
    root  <- trimws(input$results_folder %||% getwd())
    files <- scan_results_folder(root)
    if (length(files) == 0) {
      updateSelectInput(session, "results_sim_select",
        choices = setNames("", "— no result CSVs found —"), selected = "")
      if (isTRUE(show_warning)) {
        showNotification(
          paste0("No result CSVs found in: ", root),
          type = "warning", duration = 5)
      }
    } else {
      updateSelectInput(session, "results_sim_select",
        choices  = files,
        selected = files[length(files)])   # most recent / last alphabetically
    }
  }

  # Scan on button click — user explicitly asked, so show warnings.
  observeEvent(input$results_scan, { do_scan(show_warning = TRUE) })

  # Scan whenever the folder path changes — no toast, just refresh the list.
  observeEvent(input$results_folder, {
    req(nzchar(trimws(input$results_folder %||% "")))
    do_scan(show_warning = FALSE)
  }, ignoreInit = FALSE)

  # ── Reactive file inventory — polls every 3 s while app is running ────────
  # Detects renamed or newly-written files without the user clicking Scan.
  folder_files_poll <- reactivePoll(
    intervalMillis = 3000,
    session        = session,
    checkFunc = function() {
      root <- trimws(input$results_folder %||% getwd())
      if (!dir.exists(root)) return(NULL)
      # Fast check: mtime of the target output dir
      subdirs  <- list.dirs(root, recursive = FALSE, full.names = TRUE)
      out_dirs <- subdirs[grepl("Output_.*_\\d{4}-\\d{2}-\\d{2}_\\d{6}$", basename(subdirs))]
      target   <- if (length(out_dirs) > 0) sort(out_dirs)[length(out_dirs)] else root
      if (!dir.exists(target)) return(NULL)
      # Return list of filenames + their mtimes as the check value
      fls <- list.files(target, pattern = "\\.csv$", full.names = TRUE)
      paste(sort(basename(fls)), collapse = "|")
    },
    valueFunc = function() {
      do_scan(show_warning = FALSE)
      Sys.time()   # return value unused; side-effect is the dropdown update
    }
  )
  # Consume the reactive so the poll actually runs
  observe({ folder_files_poll() })

  # Also scan automatically when a run completes (siacs_proc transitions)
  observe({
    siacs_proc()   # take dependency
    isolate({
      # Update folder input to the freshly-written output dir, then scan
      out_dirs <- siacs_out_dir()
      if (!is.null(out_dirs) && length(out_dirs) > 0) {
        # siacs_out_dir is a vector of per-instance dirs; pick parent of first
        parent <- dirname(normalizePath(out_dirs[1], winslash = "/", mustWork = FALSE))
        updateTextInput(session, "results_folder", value = parent)
      }
      # Don't toast on completion either — the run-complete modal already
      # tells the user where outputs went. A toast here would re-fire on
      # every siacs_proc state change, including the NULL transition.
      do_scan(show_warning = FALSE)
    })
  })

  # ── Robust SIACS CSV reader ───────────────────────────────────────────────
  # Dynamically finds the header row regardless of how many # comment lines exist.
  # Structure: N×(# comment) → units row → header row ("time","H2SO4",...) → data
  read_siacs_csv <- function(path) {
    raw             <- readLines(path, warn = FALSE)
    non_comment_idx <- which(!grepl("^#", raw))
    if (length(non_comment_idx) < 2)
      stop("Cannot find header row (need at least 2 non-comment lines)")
    header_line <- non_comment_idx[2]
    skip_n      <- header_line - 1
    read.table(path, header = TRUE, sep = ",",
               comment.char = "", skip = skip_n,
               stringsAsFactors = FALSE, check.names = FALSE,
               fill = TRUE, quote = "\"",
               na.strings = c("NA", "NaN", ""))
  }

  # ── Load button ───────────────────────────────────────────────────────────
  observeEvent(input$results_load, {
    req(input$results_sim_select)
    path <- input$results_sim_select
    if (!nzchar(path) || !file.exists(path)) {
      showNotification("File not found — scan the folder first.", type = "error")
      return()
    }
    df <- tryCatch(
      read_siacs_csv(path),
      error = function(e) { showNotification(paste("Read error:", e$message), type = "error"); NULL }
    )
    if (is.null(df)) return()
    names(df)[tolower(names(df)) == "time"] <- "time"
    if (!"time" %in% names(df)) {
      showNotification("No 'time' column found in file.", type = "warning")
      return()
    }
    out_cols <- names(df)[grepl("\\.O$", names(df))]
    results_data(df)
    results_out_cols(out_cols)
    results_file_path(path)
    showNotification(
      paste0("Loaded: ", basename(path), " (", nrow(df), " rows, ",
             length(names(df)) - 1, " variables)"),
      type = "message")
  })

  # ── Status banner ─────────────────────────────────────────────────────────
  output$results_status_banner <- renderUI({
    path <- results_file_path()
    df   <- results_data()
    if (is.null(df))
      return(p(style = "color:#888;font-size:12px;margin:6px 0;", "No data loaded yet."))
    n_vars <- length(names(df)) - 1
    n_rows <- nrow(df)
    div(
      style = "background:#d4edda;color:#155724;padding:6px 10px;border-radius:4px;font-size:12px;",
      sprintf("\u2705 %s | %d time steps | %d variables", basename(path), n_rows, n_vars)
    )
  })

  # ── Species grouping helper ───────────────────────────────────────────────
  categorise_species <- function(all_cols, out_cols) {
    # Strip .O suffix to get base names
    base_indoor  <- setdiff(all_cols, c("time", out_cols))
    base_outdoor_stripped <- sub("\\.O$", "", out_cols)

    # "others2plot" variables (physical / environmental)
    others <- c("RH", "LightFlux", "T", "Ti", "To", "a")
    # PM species (ends in PM or contains PM)
    pm_spc   <- base_indoor[grepl("^PM", base_indoor, ignore.case=TRUE)]
    # Light/flux
    light_spc <- base_indoor[grepl("^(LightFlux|a$|RH$|Ti$|T$)", base_indoor)]
    # All gas/chemistry species = everything else
    gas_spc <- setdiff(base_indoor, c(pm_spc, light_spc, "time"))

    list(gas = sort(gas_spc), pm = sort(pm_spc),
         physical = sort(light_spc), all_indoor = base_indoor)
  }

  # ── Species checkbox UI ───────────────────────────────────────────────────
  output$results_species_checkboxes <- renderUI({
    df <- results_data()
    if (is.null(df)) return(NULL)
    out_cols <- results_out_cols() %||% character(0)
    all_cols <- names(df)
    cats <- categorise_species(all_cols, out_cols)

    # Key species shown by default
    key_species <- c("HCHO","RCHO","NO2","O3","CO","PM25","PM25_10","NH3","T","RH","LightFlux","a")

    make_group <- function(label, choices, selected) {
      if (length(choices) == 0) return(NULL)
      tagList(
        strong(style="font-size:11px;color:#555;", label),
        checkboxGroupInput(paste0("results_sp_", gsub(" ","_",label)),
          label    = NULL,
          choices  = choices,
          selected = selected)
      )
    }

    tagList(
      make_group("Gas / Chemistry", cats$gas,
        intersect(cats$gas, key_species)),
      make_group("Particulate Matter", cats$pm,
        intersect(cats$pm, key_species)),
      make_group("Physical / Other", cats$physical,
        intersect(cats$physical, key_species))
    )
  })

  # Collect all selected species across groups
  selected_species <- reactive({
    df <- results_data()
    if (is.null(df)) return(character(0))
    out_cols <- results_out_cols() %||% character(0)
    cats <- categorise_species(names(df), out_cols)

    sp <- c(
      input[[paste0("results_sp_Gas_/_Chemistry")]],
      input[[paste0("results_sp_Particulate_Matter")]],
      input[[paste0("results_sp_Physical_/_Other")]]
    )
    sp[!is.na(sp)]
  })

  # Select All / None / Key buttons
  observeEvent(input$results_select_none, {
    df <- results_data(); if (is.null(df)) return()
    updateCheckboxGroupInput(session, "results_sp_Gas_/_Chemistry",    selected = character(0))
    updateCheckboxGroupInput(session, "results_sp_Particulate_Matter", selected = character(0))
    updateCheckboxGroupInput(session, "results_sp_Physical_/_Other",   selected = character(0))
  })
  observeEvent(input$results_select_key, {
    df <- results_data(); if (is.null(df)) return()
    out_cols <- results_out_cols() %||% character(0)
    cats <- categorise_species(names(df), out_cols)
    key <- c("HCHO","RCHO","NO2","O3","CO","PM25","PM25_10","NH3","T","RH","LightFlux","a")
    updateCheckboxGroupInput(session, "results_sp_Gas_/_Chemistry",    selected = intersect(cats$gas,      key))
    updateCheckboxGroupInput(session, "results_sp_Particulate_Matter", selected = intersect(cats$pm,       key))
    updateCheckboxGroupInput(session, "results_sp_Physical_/_Other",   selected = intersect(cats$physical, key))
  })

  # ── Multi-panel grid renderer ────────────────────────────────────────────
  # Renders each species as its own independent plotly figure inside a
  # 4-column CSS grid. Titles are native plotly titles — always centred.
  # Extra rows scroll naturally; no subplot() coordinate arithmetic needed.

  is_gas <- function(nm) !grepl("^PM", nm, ignore.case = TRUE) &
                         !nm %in% c("RH", "LightFlux", "T", "Ti", "a")
  col_indoor  <- "#E07070"
  col_outdoor <- "#4BBFBF"

  # Register one renderPlotly per species slot (up to a reasonable cap)
  MAX_PANELS <- 60
  for (pid in seq_len(MAX_PANELS)) {
    local({
      panel_id <- pid
      output[[paste0("panel_plot_", panel_id)]] <- renderPlotly({
        df         <- results_data();   if (is.null(df)) return(plotly_empty())
        sp_list    <- selected_species()
        show_lines <- input$results_show_lines %||% c("indoor", "outdoor")
        if (panel_id > length(sp_list)) return(plotly_empty())
        sp  <- sp_list[[panel_id]]
        p   <- plot_ly()

        if ("indoor" %in% show_lines && sp %in% names(df)) {
          yv <- df[[sp]]; if (is_gas(sp)) yv <- yv * 1000
          p <- add_trace(p, x = df$time / 60, y = yv,
            type = "scatter", mode = "lines", name = "indoor",
            line = list(color = col_indoor, width = 2),
            legendgroup = "indoor", showlegend = TRUE)
        }
        out_col <- paste0(sp, ".O")
        if ("outdoor" %in% show_lines && out_col %in% names(df)) {
          yv <- df[[out_col]]; if (is_gas(sp)) yv <- yv * 1000
          p <- add_trace(p, x = df$time / 60, y = yv,
            type = "scatter", mode = "lines", name = "outdoor",
            line = list(color = col_outdoor, width = 2),
            legendgroup = "outdoor", showlegend = TRUE)
        }
        y_label <- if (grepl("^PM", sp, ignore.case = TRUE)) "ug/m3"
                   else if (sp %in% c("RH")) "%"
                   else if (sp %in% c("T", "Ti")) "K"
                   else if (sp %in% c("LightFlux", "a")) "—"
                   else "ppb"
        p %>% layout(
          title  = list(text = sp, x = 0.5, xanchor = "center",
                        font = list(size = 12), pad = list(t = 4)),
          xaxis  = list(title = list(text = "Time (hours)",
                                     font = list(size = 10),
                                     standoff = 12),
                        showgrid = TRUE, gridcolor = "#dddddd"),
          yaxis  = list(title = list(text = y_label,
                                     font = list(size = 10)),
                        rangemode = "tozero",
                        showgrid = TRUE, gridcolor = "#dddddd"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.35, font = list(size = 10)),
          margin = list(t = 40, b = 65, l = 45, r = 10),
          paper_bgcolor = "#f9f9f9",
          plot_bgcolor  = "#ffffff"
        )
      })
    })
  }

  # Render the grid of plotlyOutputs — only as many as species selected
  output$results_plot_grid <- renderUI({
    sp_list <- selected_species()
    n <- length(sp_list)
    if (n == 0) {
      return(div(style = "padding:20px;color:#888;",
        "Select at least one species from the list on the left."))
    }
    plot_tags <- lapply(seq_len(min(n, MAX_PANELS)), function(pid) {
      div(style = "background:#f9f9f9;border-radius:4px;",
        plotlyOutput(paste0("panel_plot_", pid), height = "220px")
      )
    })
    div(class = "multi-panel-grid", tagList(plot_tags))
  })

  # ==========================================================================
  # CSV RAW PREVIEW  (reads from selected path, always visible)
  # ==========================================================================

  output$csv_raw_preview <- renderUI({
    path <- input$results_sim_select
    if (is.null(path) || !nzchar(path)) return(NULL)
    if (!file.exists(path)) {
      return(div(
        style = "background:#fff3cd;color:#856404;padding:8px 12px;border-radius:4px;font-size:12px;margin-top:6px;",
        strong("File not found: "), path
      ))
    }
    raw_lines  <- tryCatch(readLines(path, n = 35, warn = FALSE),
                           error = function(e) paste("Error reading file:", e$message))
    n_comment  <- sum(grepl("^#", raw_lines))
    data_lines <- raw_lines[!grepl("^#", raw_lines)]
    sep_info   <- if (length(data_lines) > 0) {
      if (grepl("\t", data_lines[1])) "TAB  (wrong — needs COMMA)" else
      if (grepl(",",  data_lines[1])) "COMMA  correct"             else "UNKNOWN"
    } else "no data lines found"

    tagList(
      tags$details(
        tags$summary(
          style = "cursor:pointer;font-size:12px;color:#2E86C1;font-weight:bold;margin:6px 0 4px 0;",
          paste0("CSV Preview: ", basename(path), "  [click to expand]")
        ),
        div(
          style = "background:#e8f4fd;border:1px solid #b8daff;border-radius:4px;padding:8px 12px;margin-bottom:6px;font-size:12px;",
          strong("Full path: "),           path,                        tags$br(),
          strong("Separator detected: "),  sep_info,                    tags$br(),
          strong("Comment lines (#): "),   as.character(n_comment),     tags$br(),
          strong("Non-comment line 1: "),  "units row (skipped)",       tags$br(),
          strong("Non-comment line 2: "),  "header row (column names)", tags$br(),
          strong("Non-comment line 3+: "), "data"
        ),
        div(
          style = paste0(
            "background:#1e1e1e;font-family:monospace;font-size:11px;",
            "padding:12px;border-radius:6px;max-height:40vh;overflow:auto;white-space:pre;"
          ),
          lapply(seq_along(raw_lines), function(i) {
            line  <- raw_lines[i]
            non_c <- which(!grepl("^#", raw_lines))
            color <- if (grepl("^#", line))            "#6a9955" else
                     if (i == non_c[1])                "#ce9178" else
                     if (length(non_c) >= 2 &&
                         i == non_c[2])                "#9cdcfe" else
                     "#d4d4d4"
            tags$span(style = paste0("color:", color, ";display:block;"),
              paste0(sprintf("%3d: ", i), line))
          })
        ),
        div(style = "font-size:10px;color:#888;margin-top:4px;",
          tags$span(style = "color:#6a9955;", "Green = comment  "),
          tags$span(style = "color:#ce9178;", "Orange = units row  "),
          tags$span(style = "color:#9cdcfe;", "Blue = header  "),
          "White = data"
        )
      )
    )
  })

  # ==========================================================================
  # SINGLE PANEL PLOT
  # ==========================================================================

  observe({
    df <- results_data()
    if (is.null(df)) return()
    out_cols <- results_out_cols() %||% character(0)

    indoor_vars  <- setdiff(names(df), c("time", out_cols))
    outdoor_base <- sub("[.]O$", "", out_cols)
    all_vars     <- sort(unique(c(indoor_vars, outdoor_base)))
    all_vars     <- all_vars[all_vars != "time"]

    phys         <- c("RH", "LightFlux", "T", "Ti", "a")
    phys_present <- intersect(phys, all_vars)
    rest         <- sort(setdiff(all_vars, phys_present))
    choices_grouped <- c(
      if (length(phys_present) > 0)
        setNames(phys_present, paste0("[env] ", phys_present)),
      setNames(rest, rest)
    )
    default_sp <- if ("NO2" %in% all_vars) "NO2" else all_vars[1]
    updateSelectInput(session, "single_sp",
      choices  = choices_grouped,
      selected = default_sp
    )
  })

  output$single_panel_plot <- renderPlotly({
    df <- results_data()
    if (is.null(df)) return(plotly_empty())
    sp <- input$single_sp
    req(sp)

    show_lines <- input$single_show_lines %||% c("indoor", "outdoor")
    is_gas_sp  <- !grepl("^PM", sp, ignore.case = TRUE) &&
                  !sp %in% c("RH", "LightFlux", "T", "Ti", "a")
    scale_val  <- if (is_gas_sp) 1000 else 1
    y_label    <- if (is_gas_sp)                          "Concentration (ppb)"   else
                  if (grepl("^PM", sp, ignore.case=TRUE)) "Concentration (ug/m3)" else sp

    p <- plot_ly()
    if ("indoor" %in% show_lines && sp %in% names(df)) {
      p <- add_trace(p, x = df$time / 60, y = df[[sp]] * scale_val,
        name = "indoor", type = "scatter", mode = "lines",
        line = list(color = "#E07070", width = 2.5))
    }
    out_col <- paste0(sp, ".O")
    if ("outdoor" %in% show_lines && out_col %in% names(df)) {
      p <- add_trace(p, x = df$time / 60, y = df[[out_col]] * scale_val,
        name = "outdoor", type = "scatter", mode = "lines",
        line = list(color = "#4BBFBF", width = 2.5))
    }
    p %>% layout(
      title     = list(text = sp, font = list(size = 20)),
      xaxis     = list(title = "Time (hours)", showgrid = TRUE,
                       gridcolor = "#e0e0e0", zeroline = FALSE),
      yaxis     = list(title = y_label, rangemode = "tozero",
                       showgrid = TRUE, gridcolor = "#e0e0e0"),
      legend    = list(orientation = "h", x = 0.5, xanchor = "center",
                       y = -0.12, font = list(size = 14)),
      hovermode     = "x unified",
      paper_bgcolor = "#f8f8f8",
      plot_bgcolor  = "#ffffff",
      margin        = list(t = 60, b = 80, l = 70, r = 30)
    )
  })

}

# ===== Run App =====
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))