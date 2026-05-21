# wizard_module.R
# =======================================================================
# SIACS Wizard GUI - 12-Screen Configuration Wizard
# =======================================================================

# ---- Lookup tables (module-level, available to UI and server) ----
# Shelter class labels follow Table S3 (LBL model descriptions)
shelter_classes <- c(
  "1 – Exposed (no obstructions)"                                        = 1,
  "2 – Normal (isolated rural house)"                                    = 2,
  "3 – Normal (buildings across the street)"                             = 3,
  "4 – Normal (urban, obstacles > one building height away)"             = 4,
  "5 – Well-shielded (adjacent structures < one building height away)"   = 5
)
light_geometries <- c("point","line","area","surface")
bulb_types       <- c("Kowal.LED","Kowal.Incandescent","Kowal.CFL")

# Table S1: Stack coefficients ks [(L/s)²/(cm⁴·K)]
# One value per number of stories — does NOT vary by shelter class
stack_coeff_table <- data.frame(
  stories = 1:3,
  ks      = c(0.000145, 0.000290, 0.000435)
)

# Table S2: Wind coefficients kw [(L/s)²/(cm⁴·(m/s)²)]
# Rows = shelter class (1–5), Columns = stories (1–3)
wind_coeff_table <- data.frame(
  shelter_class = 1:5,
  one   = c(0.000319, 0.000246, 0.000174, 0.000104, 0.000032),
  two   = c(0.000420, 0.000325, 0.000231, 0.000137, 0.000042),
  three = c(0.000494, 0.000382, 0.000271, 0.000161, 0.000049)
)

# ================================
# WIZARD UI
# ================================
wizard_module_ui <- function() {
  tagList(
    tags$head(tags$style(HTML("
      .wizard-progress{width:100%;height:28px;background:#e0e0e0;border-radius:14px;margin-bottom:18px;overflow:hidden}
      .wizard-progress-bar{height:100%;background:linear-gradient(90deg,#4CAF50,#45a049);transition:width .3s ease;
        display:flex;align-items:center;justify-content:center;color:#fff;font-weight:bold;font-size:13px}
      .wizard-screen{min-height:380px;padding:18px}
      .wizard-nav-buttons{margin-top:16px;padding-top:16px;border-top:2px solid #ddd}
      .screen-title{color:#2E86C1;font-size:22px;font-weight:bold;margin-bottom:16px}
      .panel-title{color:#2874A6;font-size:16px;font-weight:bold;margin:12px 0 8px}
      #wizard_modal .modal-body{max-height:calc(100vh - 200px)!important;overflow-y:auto}
      .event-panel{background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;padding:10px;margin-bottom:8px}
      .htCore td{white-space:nowrap}
    "))),

    modal_ui(
      id="wizard_modal",
      title=textOutput("wizard_title_text"),
      size="xl",
      static=TRUE,

      # Progress bar
      fluidRow(column(12,
        div(style="display:none;",numericInput("wizard_current_screen_input","Screen",value=1)),
        div(class="wizard-progress",
          div(class="wizard-progress-bar",style="width:8.33%;",uiOutput("wizard_progress_text")))
      )),

      # ── Screen 1: Location ───────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==1",
        div(class="wizard-screen",
          div(class="screen-title","Location"),
          uiOutput("wiz_val_banner_1"),
          fluidRow(
            column(6,
              numericInput("wiz_latitude",  "Latitude (° N)",           value=38.7509, step=0.0001),
              numericInput("wiz_longitude", "Longitude (° E)",          value=-77.4753,step=0.0001)),
            column(6,
              numericInput("wiz_altitude", "Altitude (m above sea level)", value=94, min=0),
              numericInput("wiz_gravity",  "Gravity (m/s²)",               value=9.8, min=0.1, step=0.01)))
        )
      ),

      # ── Screen 2: Start Time ─────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==2",
        div(class="wizard-screen",
          div(class="screen-title","When does the simulation start?"),
          uiOutput("wiz_val_banner_2"),
          # ── Default data availability notice ────────────────────────────
          div(style=paste0("background:#fff3cd;color:#856404;border:1px solid #ffc107;",
                           "border-radius:5px;padding:10px 14px;margin-bottom:12px;",
                           "font-size:12px;line-height:1.5;"),
            tags$b("⚠ Important — default data availability"),
            tags$br(),
            "Built-in default files for outdoor concentrations, physical environment, ",
            "and activity/source schedules are ", tags$b("only available for Atlanta, GA"),
            " on specific historical dates for which measured data exist. ",
            "They ", tags$b("cannot"), " be used for other locations or future dates ",
            "(e.g., a 2026 run). ",
            tags$br(),
            "If you are running a simulation for a different location or date you must ",
            "supply your own outdoor concentrations, physical environment, and activity ",
            "data on the corresponding screens. Defaults will be unavailable."
          ),
          fluidRow(
            column(4,
              numericInput("wiz_start_year", "Year",        value=2022,min=1900,max=2100,step=1),
              numericInput("wiz_start_month","Month (1-12)",value=7,   min=1,   max=12,  step=1),
              numericInput("wiz_start_day",  "Day (1-31)",  value=7,   min=1,   max=31,  step=1)),
            column(4,
              textInput( "wiz_start_time","Local Time (HH:MM:SS)",value="20:00:00"),
              selectInput(
                inputId  = "wiz_timezone",
                label    = "Time Zone (POSIX)",
                selected = "EST5EDT",
                choices  = list(
                  "North America" = list(
                    "Eastern (EST/EDT)"           = "EST5EDT",
                    "Central (CST/CDT)"           = "CST6CDT",
                    "Mountain (MST/MDT)"          = "MST7MDT",
                    "Pacific (PST/PDT)"           = "PST8PDT",
                    "Alaska (AKST/AKDT)"          = "AKST9AKDT",
                    "Hawaii (HST)"                = "HST10",
                    "Atlantic (AST/ADT)"          = "AST4ADT",
                    "Newfoundland (NST/NDT)"      = "NST3:30NDT"
                  ),
                  "Latin America" = list(
                    "Mexico City (CST/CDT)"       = "CST6CDT",
                    "Colombia / Peru (COT)"       = "COT5",
                    "Venezuela (VET)"             = "VET4:30",
                    "Bolivia / Paraguay (BOT)"    = "BOT4",
                    "Argentina (ART)"             = "ART3",
                    "Brazil Brasilia (BRT)"       = "BRT3BRST",
                    "Chile (CLT)"                 = "CLT4CLST"
                  ),
                  "Europe / Africa" = list(
                    "Western Europe (WET/WEST)"   = "WET0WEST",
                    "Central Europe (CET/CEST)"   = "CET-1CEST",
                    "Eastern Europe (EET/EEST)"   = "EET-2EEST",
                    "Moscow (MSK)"                = "MSK-3",
                    "West Africa (WAT)"           = "WAT-1",
                    "Central Africa (CAT)"        = "CAT-2",
                    "East Africa (EAT)"           = "EAT-3",
                    "South Africa (SAST)"         = "SAST-2"
                  ),
                  "Middle East" = list(
                    "Arabia (AST)"                = "AST-3",
                    "Iran (IRST/IRDT)"            = "IRST-3:30IRDT",
                    "Gulf / Pakistan (PKT)"       = "PKT-5",
                    "Israel (IST/IDT)"            = "IST-2IDT"
                  ),
                  "South & Central Asia" = list(
                    "India (IST)"                 = "IST-5:30",
                    "Bangladesh (BST)"            = "BST-6",
                    "Nepal (NPT)"                 = "NPT-5:45",
                    "Sri Lanka (SLST)"            = "SLST-5:30",
                    "Kazakhstan (ALMT)"           = "ALMT-6"
                  ),
                  "East & Southeast Asia" = list(
                    "China / Philippines (CST)"   = "CST-8",
                    "Japan / Korea (JST/KST)"     = "JST-9",
                    "Indonesia West (WIB)"        = "WIB-7",
                    "Indonesia Central (WITA)"    = "WITA-8",
                    "Indonesia East (WIT)"        = "WIT-9",
                    "Thailand / Vietnam (ICT)"    = "ICT-7",
                    "Malaysia / Singapore (MYT)"  = "MYT-8"
                  ),
                  "Oceania" = list(
                    "Australia Western (AWST)"    = "AWST-8",
                    "Australia Central (ACST)"    = "ACST-9:30ACDT",
                    "Australia Eastern (AEST)"    = "AEST-10AEDT",
                    "New Zealand (NZST/NZDT)"     = "NZST-12NZDT",
                    "Fiji (FJT)"                  = "FJT-12",
                    "Hawaii (HST)"                = "HST10"
                  ),
                  "UTC" = list(
                    "Coordinated Universal Time"  = "UTC0"
                  )
                )
              )),
            column(4,
              numericInput("wiz_relative_start","Relative Start Time (min)",value=0,min=0,step=1),
              p(style="color:#666;font-size:12px;","Offset for later-start simulations (default: 0)")))
        )
      ),

      # ── Screen 3: Simulation Time ─────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==3",
        div(class="wizard-screen",
          div(class="screen-title","Simulation Time"),
          uiOutput("wiz_val_banner_3"),
          fluidRow(
            column(4,numericInput("wiz_duration", "Duration (hours)",     value=27, min=1,  max=168)),
            column(4,numericInput("wiz_timestep", "Time Step (minutes)",  value=5,  min=0.1,max=60)),
            column(4,
              numericInput("wiz_activity_transition","Activity Transition (minutes)",
                value=0.1,min=0.01,max=5,step=0.01))
          )
        )
      ),

      # ── Screen 4: The Box ─────────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==4",
        div(class="wizard-screen",
          div(class="screen-title","The Box"),
          uiOutput("wiz_val_banner_4"),
          fluidRow(
            column(6,
              numericInput("wiz_floor_area",  "Floor Surface Area (m²)",             value=140, min=1),
              numericInput("wiz_room_height", "Room Height (m)",                      value=2.5, min=0.1,step=0.1)),
            column(6,
              numericInput("wiz_aspect_ratio","Floor Aspect Ratio (width/depth)",    value=0.66,min=0.1,step=0.01),
              numericInput("wiz_orientation", "Wider Side Orientation (° CW from N)",value=0,   min=0,  max=360))
          ),

          # ── Initial Indoor Concentrations (optional) ───────────────────
          # Collapsible block. Three modes:
          #   "Use default file"   -> load Input/InitialIndoorConcentrations.csv
          #   "Upload custom file" -> user-supplied CSV with same structure
          #   "Let SIACS calculate" -> leave NULL; engine seeds from equilibrium
          tags$details(
            style="margin-top:18px;border:1px solid #dee2e6;border-radius:6px;padding:10px 14px;background:#f8f9fa;",
            tags$summary(
              style="cursor:pointer;font-weight:bold;color:#2874A6;font-size:14px;",
              "Initial Indoor Concentrations (optional)"
            ),
            div(style="margin-top:12px;",
              p(style="color:#555;font-size:12px;margin-bottom:10px;",
                "Set the indoor concentration of each species at t=0. If you ",
                "skip this, SIACS seeds the simulation from chemical equilibrium ",
                "with ambient air, sources, and filtration (chemistry off)."),
              radioButtons("wiz_init_values_mode",
                "How would you like to provide initial indoor concentrations?",
                choices=c("Use default file","Upload custom file","Let SIACS calculate"),
                selected="Use default file", inline=TRUE),

              # Use default
              conditionalPanel(condition="input.wiz_init_values_mode=='Use default file'",
                div(style="margin-top:6px;",
                  p(style="font-size:12px;color:#333;",
                    "Using: Input/InitialIndoorConcentrations.csv"),
                  actionButton("wiz_preview_init_default","Preview Default File",
                    style="background:#3498DB;color:#fff;"))
              ),

              # Upload
              conditionalPanel(condition="input.wiz_init_values_mode=='Upload custom file'",
                div(style="margin-top:6px;",
                  fileInput("wiz_init_values_file",
                    "Upload Initial Indoor Concentrations CSV", accept=".csv"),
                  p(style="color:#666;font-size:12px;",
                    "Must match the structure of InitialIndoorConcentrations.csv: ",
                    "three header rows (units, species names, values) in ppm, µg/m³, or cm⁻³."))
              ),

              # Calculate
              conditionalPanel(condition="input.wiz_init_values_mode=='Let SIACS calculate'",
                div(style="margin-top:6px;background:#d1ecf1;color:#0c5460;border:1px solid #bee5eb;border-radius:5px;padding:10px;",
                  p(style="margin:0;font-size:12px;",
                    "\u2139\ufe0f SIACS will compute equilibrium initial concentrations at runtime."))
              )
            )
          )
        )
      ),

      # ── Screen 5: Building ────────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==5",
        div(class="wizard-screen",
          div(class="screen-title","Approximating a Building"),
          uiOutput("wiz_val_banner_5"),
          fluidRow(
            column(6,
              div(class="panel-title","Building Parameters"),
              fluidRow(
                column(6,
                  numericInput("wiz_area_to_volume",        "Area-to-Volume Ratio (m⁻¹)",       value=2.5,  min=0,step=0.1),
                  numericInput("wiz_infiltration_area",     "Infiltration Surface Area (m²)",    value=0.1,  min=0,step=0.01),
                  numericInput("wiz_indoor_reflectance",    "Indoor Light Reflectance (0–1)",          value=0.5,  min=0,max=1,step=0.01),
                  numericInput("wiz_num_stories",           "Number of Stories",           value=2,    min=1,max=3,step=1)),
                column(6,
                  numericInput("wiz_neutral_pressure",      "Neutral Pressure Level (m)",        value=2.5,  min=0,step=0.01),
                  numericInput("wiz_midpoint_height",       "Window Midpoint Height (m)",        value=0.91, min=0,step=0.01),
                  numericInput("wiz_discharge_coeff",       "Discharge Coefficient",             value=0.65, min=0,max=1,step=0.01),
                  numericInput("wiz_opening_effectiveness", "Opening Effectiveness",             value=0.3,  min=0,max=1,step=0.01))
              ),
              div(class="panel-title","Stack Coefficient"),
              numericInput("wiz_stack_coeff","Stack Coefficient ks [(L/s)²/(cm⁴·K)]",value=0.000290,min=0,step=0.000001),
              uiOutput("wiz_stack_coeff_display"),
              numericInput("wiz_num_windows","Number of Windows",value=3,min=0,max=10,step=1)
            ),
            column(6,
              div(class="panel-title","Window Configuration"),
              p(style="color:#666;font-size:12px;font-style:italic;",
                "Windows are idealized as single windows centred on each wall for daylight calculations."),
              div(style="max-height:52vh;overflow-y:auto;padding-right:8px;",
                uiOutput("wiz_windows_ui"))
            )
          )
        )
      ),

      # ── Screen 6: Outside ─────────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==6",
        div(class="wizard-screen",
          div(class="screen-title","What is outside the building?"),
          uiOutput("wiz_val_banner_6"),
          fluidRow(
            column(6,
              numericInput("wiz_surface_albedo",     "Surface Albedo (0–1)",  value=0.1,min=0,max=1,step=0.01),
              numericInput("wiz_cloud_base",         "Cloud Base (km)", value=4,  min=0,step=0.1),
              numericInput("wiz_cloud_top",          "Cloud Top (km)",  value=5,  min=0,step=0.1)),
            column(6,
              numericInput("wiz_cloud_optical_depth","Cloud Optical Depth",value=0,min=0,step=0.1),
              selectInput("wiz_shelter_class","Shelter Class",choices=names(shelter_classes),selected=names(shelter_classes)[3]),
              numericInput("wiz_wind_coeff","Wind Coefficient kw [(L/s)²/(cm⁴·(m/s)²)]",value=0.000231,min=0,step=0.000001),
              uiOutput("wiz_wind_coeff_display"))
          )
        )
      ),

      # ── Screen 7: Artificial Lights ───────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==7",
        div(class="wizard-screen",
          div(class="screen-title","Artificial Lights"),
          fluidRow(column(12,
            radioButtons("wiz_lights_mode",
              "How would you like to provide artificial light data?",
              choices=c("Use default files","Upload custom files","Configure manually",
                        "Let SIACS calculate"),
              selected="Use default files", inline=TRUE)
          )),

          # Let SIACS calculate
          conditionalPanel(condition="input.wiz_lights_mode=='Let SIACS calculate'",
            fluidRow(column(12,
              div(style="background:#d1ecf1;color:#0c5460;border:1px solid #bee5eb;border-radius:5px;padding:12px;",
                tags$b("\u2139\ufe0f SIACS will calculate artificial light flux automatically."),
                tags$br(),
                "ArtifLight.csv, DirectLight.csv, DiffuseLight.csv, and IndoorLight.xlsx ",
                "will all be calculated at runtime from your location, date/time, window geometry, ",
                "and the ArtificialLightList + ArtificialLightSpectra + ArtificialLightSchedule ",
                "default files, then saved to your output folder for reuse.",
                tags$br(), tags$br(),
                tags$b("Note:"), " Windows and Glass Transmission data (configured on Screen 5) ",
                "are still required for the indoor light calculation."
              )
            ))
          ),

          # Use default
          conditionalPanel(condition="input.wiz_lights_mode=='Use default files'",
            fluidRow(column(12,
              p("Using: Input/ArtificialLightList.csv and Input/ArtificialLightSchedule.csv"),
              actionButton("wiz_preview_lights_default","Preview Default Files",
                style="background:#3498DB;color:#fff;")
            ))
          ),

          # Upload
          conditionalPanel(condition="input.wiz_lights_mode=='Upload custom files'",
            fluidRow(
              column(6,
                fileInput("wiz_lights_list_file","Upload Light List CSV",accept=".csv"),
                p(style="color:#666;font-size:12px;",
                  "Must contain: LightNumber, Geometry, Size, Height, ",
                  "DistanceShorterWall, DistanceLongerWall, DirectionShorter, ",
                  "DirectionLonger, DirectionHeight, PowerEfficiency, Spectrum")),
              column(6,
                fileInput("wiz_lights_sched_file","Upload Light Schedule CSV",accept=".csv"),
                p(style="color:#666;font-size:12px;",
                  "Must contain: Time (minutes), Light1, Light2, ... (Watts)"))
            )
          ),

          # Configure manually
          conditionalPanel(condition="input.wiz_lights_mode=='Configure manually'",
            fluidRow(
              column(3,
                div(class="panel-title","Light Count"),
                numericInput("wiz_num_lights","How many lights?",value=3,min=0,max=20,step=1)),
              column(9,
                div(class="panel-title","Light Details"),
                div(style="max-height:52vh;overflow-y:auto;padding-right:8px;",
                  uiOutput("wiz_lights_ui")))
            )
          )
        )
      ),

      # ── Screen 8: Light Schedule ──────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==8",
        div(class="wizard-screen",
          div(class="screen-title","Artificial Light Schedule"),

          # Let SIACS calculate — nothing to do here
          conditionalPanel(condition="input.wiz_lights_mode=='Let SIACS calculate'",
            fluidRow(column(12,
              div(style="background:#d1ecf1;color:#0c5460;border:1px solid #bee5eb;border-radius:5px;padding:14px;",
                p(style="margin:0;",
                  "\u2139\ufe0f You selected 'Let SIACS calculate' on the previous screen. ",
                  "No light schedule entry is needed — SIACS will derive all light data at runtime."))
            ))
          ),

          # If files were uploaded or defaults used, just confirm
          conditionalPanel(condition="input.wiz_lights_mode!='Configure manually' && input.wiz_lights_mode!='Let SIACS calculate'",
            fluidRow(column(12,
              div(style="background:#d4edda;padding:14px;border-radius:6px;",
                p(style="color:#155724;margin:0;",
                  "\u2705 Light schedule will be taken from the ",
                  "file selected on Screen 7. No manual schedule entry needed."))
            ))
          ),

          # Manual schedule editing (only when configuring manually)
          conditionalPanel(condition="input.wiz_lights_mode=='Configure manually'",
            fluidRow(
              column(4,
                div(class="event-panel",
                  h5("Add step-change event"),
                  selectInput("wiz_light_sel","Light",choices=c()),
                  numericInput("wiz_light_event_time","At time (min)",value=0,min=0),
                  numericInput("wiz_light_event_power","Power level (W)",value=60,min=0),
                  fluidRow(
                    column(6,
                      actionButton("wiz_light_on","Time On",
                        style="background:#27ae60;color:#fff;width:100%;")),
                    column(6,
                      actionButton("wiz_light_off","Time Off",
                        style="background:#c0392b;color:#fff;width:100%;")))
                ),
                p(style="color:#666;font-size:11px;margin-top:6px;",
                  "'Time On' sets power to the specified level; 'Time Off' sets it to 0. ",
                  "Both insert a transition row at (time \u2212 transition time) using the current value."),
                actionButton("wiz_light_sched_reset","Reset to Default",
                  style="background:#95a5a6;color:#fff;width:100%;margin-top:6px;")
              ),
              column(8,
                h5("Current Schedule (directly editable)"),
                p(style="color:#666;font-size:11px;",
                  "Edit cells directly, or use the buttons on the left to add step changes."),
                div(style="overflow-x:auto;",
                  rHandsontableOutput("wiz_light_schedule_table"))
              )
            )
          )
        )
      ),

      # ── Screen 9: Physical Environment ────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==9",
        div(class="wizard-screen",
          div(class="screen-title","Physical Environment Data"),
          uiOutput("wiz_val_banner_9"),
          fluidRow(column(12,
            radioButtons("wiz_phys_env_mode",
              "How would you like to provide this data?",
              choices=c("Use default file","Upload custom file","Manual entry"),
              selected="Use default file",inline=TRUE)
          )),

          # Use default
          conditionalPanel(condition="input.wiz_phys_env_mode=='Use default file'",
            fluidRow(column(12,
              p("Using: Input/PhysicalEnvironmentData_CMAQ.csv"),
              actionButton("wiz_preview_phys_default","Preview Default File",
                style="background:#3498DB;color:#fff;")
            ))
          ),

          # Upload
          conditionalPanel(condition="input.wiz_phys_env_mode=='Upload custom file'",
            fileInput("wiz_phys_env_file","Upload Physical Environment CSV",accept=".csv")
          ),

          # Manual entry
          conditionalPanel(condition="input.wiz_phys_env_mode=='Manual entry'",
            fluidRow(
              # Left: initial/final values
              column(5,
                div(class="event-panel",
                  h5("Initial and Final Values"),
                  fluidRow(
                    column(6,
                      strong("Indoor Temp Ti (K)"),
                      numericInput("wiz_temp_indoor_init",  "Initial", value=295.15),
                      numericInput("wiz_temp_indoor_final", "Final",   value=295.15)),
                    column(6,
                      strong("Outdoor Temp To (K)"),
                      numericInput("wiz_temp_outdoor_init",  "Initial", value=303.544),
                      numericInput("wiz_temp_outdoor_final", "Final",   value=298))
                  ),
                  fluidRow(
                    column(6,
                      strong("Rel. Humidity RH"),
                      numericInput("wiz_rh_init",  "Initial", value=0.55, min=0,max=1,step=0.01),
                      numericInput("wiz_rh_final", "Final",   value=0.55, min=0,max=1,step=0.01)),
                    column(6,
                      strong("Baro. Pressure BP (Pa)"),
                      numericInput("wiz_bp_init",  "Initial", value=101325),
                      numericInput("wiz_bp_final", "Final",   value=101325))
                  ),
                  fluidRow(
                    column(6,
                      strong("Wind Speed (m/s)"),
                      numericInput("wiz_wind_init",  "Initial", value=1.41),
                      numericInput("wiz_wind_final", "Final",   value=1.41)),
                    column(6,
                      br(),br(),
                      actionButton("wiz_init_phys_schedule","Initialize Schedule",
                        style="background:#3498DB;color:#fff;width:100%;margin-top:14px;"))
                  )
                ),
                div(class="event-panel",
                  h5("Add step-change event"),
                  p(style="color:#666;font-size:11px;",
                    "For Open Window, Ventilation, and Filter variables only. ",
                    "Inserts rows at (t − transition time) and t."),
                  selectInput("wiz_phys_step_var","Variable",
                    choices=c(
                      "Open Window Area (m²)"     ="OpenWindowArea",
                      "Balanced Ventilation (m³/s)"   ="QBal",
                      "Unbalanced Ventilation (m³/s)" ="QUnbal",
                      "Filter Flow Rate (m³/s)"        ="QFilter")),
                  fluidRow(
                    column(6,numericInput("wiz_phys_step_time", "At time (min)", value=60, min=0)),
                    column(6,numericInput("wiz_phys_step_value","New value (m² or m³/s)",      value=0))),
                  actionButton("wiz_add_phys_step","Add Event",
                    style="background:#9B59B6;color:#fff;width:100%;")
                )
              ),
              # Right: editable table
              column(7,
                h5("Current Schedule (directly editable)"),
                p(style="color:#666;font-size:11px;",
                  "Click 'Initialize Schedule' first. Ti, To, RH, BP, Wind vary linearly between ",
                  "initial and final values; use 'Add Event' to insert step changes for ventilation."),
                div(style="background:#d4edda;border:1px solid #c3e6cb;border-radius:4px;padding:7px 10px;margin-bottom:8px;",
                  p(style="color:#155724;font-size:11px;margin:0;",
                    "ℹ️ Default values pre-loaded from ",
                    strong("Input/PhysicalEnvironmentData_CMAQ.csv"),
                    ". Click 'Initialize Schedule' to apply your own initial/final values above.")),
                div(style="overflow-x:auto;",
                  rHandsontableOutput("wiz_phys_schedule_table"))
              )
            )
          )
        )
      ),

      # ── Screen 10: Indoor Sources ─────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==10",
        div(class="wizard-screen",
          div(class="screen-title","Indoor Sources (Activities)"),
          fluidRow(column(12,
            radioButtons("wiz_act_mode",
              "How would you like to provide this data?",
              choices=c("Use default file","Upload custom file","Manual entry"),
              selected="Use default file",inline=TRUE)
          )),

          conditionalPanel(condition="input.wiz_act_mode=='Use default file'",
            p("Using: Input/Activities.csv (pre-loaded)"),
            actionButton("wiz_preview_act_default","Preview Default File",
              style="background:#3498DB;color:#fff;")
          ),

          conditionalPanel(condition="input.wiz_act_mode=='Upload custom file'",
            fileInput("wiz_act_file","Upload Activities CSV",accept=".csv")
          ),

          conditionalPanel(condition="input.wiz_act_mode=='Manual entry'",
            fluidRow(
              column(5,
                div(class="event-panel",
                  h5("Select Activity"),
                  selectInput("wiz_activity_type","Source Profile",
                    choices=c("Generic","Adult","Smoking",
                      "Gas Cooking"    ="GasCooking.Persily1998",
                      "Incense Burning"="Incense.Manoukian2013")),
                  # ── Per-profile descriptions ───────────────────────────
                  uiOutput("wiz_activity_profile_desc"),
                  fluidRow(
                    column(6,numericInput("wiz_activity_init", HTML("Initial value (× emission profile)"),value=0,min=0)),
                    column(6,numericInput("wiz_activity_final",HTML("Final value (× emission profile)"),   value=0,min=0))),
                  actionButton("wiz_init_activity","Initialize Schedule",
                    style="background:#3498DB;color:#fff;width:100%;")
                ),
                div(class="event-panel",
                  h5("Add step-change event"),
                  p(style="color:#666;font-size:11px;",
                    "Inserts rows at (t − transition time) and t."),
                  fluidRow(
                    column(6,numericInput("wiz_activity_event_time", "At time (min)",value=0,min=0)),
                    column(6,numericInput("wiz_activity_event_value",HTML("New value (× emission profile)"),     value=0,min=0))),
                  actionButton("wiz_add_activity_event","Add Event",
                    style="background:#E67E22;color:#fff;width:100%;"),
                  actionButton("wiz_reset_activity","Reset to Default",
                    style="background:#95a5a6;color:#fff;width:100%;margin-top:4px;")
                )
              ),
              column(7,
                h5("Current Schedule for Selected Activity (directly editable)"),
                p(style="color:#666;font-size:11px;",
                  "Time (minutes) and Value (multiples of emission profile)."),
                div(style="overflow-x:auto;",
                  rHandsontableOutput("wiz_activity_schedule_table"))
              )
            )
          )
        )
      ),

      # ── Screen 11: Outdoor Concentrations ────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==11",
        div(class="wizard-screen",
          div(class="screen-title","Outdoor Concentrations"),
          fluidRow(column(12,
            radioButtons("wiz_outdoor_conc_mode",
              "How would you like to provide this data?",
              choices=c("Use default file","Upload custom file","Manual entry"),
              selected="Use default file",inline=TRUE)
          )),

          conditionalPanel(condition="input.wiz_outdoor_conc_mode=='Use default file'",
            p("Using: Input/Outdoor Concentrations - CMAQ.csv (~80 species)"),
            actionButton("wiz_preview_outdoor_default","Preview Default File",
              style="background:#3498DB;color:#fff;")
          ),

          conditionalPanel(condition="input.wiz_outdoor_conc_mode=='Upload custom file'",
            fileInput("wiz_outdoor_conc_file","Upload Outdoor Concentrations CSV",accept=".csv"),
            p(style="color:#666;font-size:12px;",
              "Must contain a 'Time' column plus one column per chemical species (ppm).")
          ),

          conditionalPanel(condition="input.wiz_outdoor_conc_mode=='Manual entry'",
            fluidRow(
              column(12,
                p(style="color:#666;font-size:12px;",
                  "Columns are all species from the default outdoor file. Values in ppm ",
                  "(or µg/m³ for PM, cm⁻³ for UFP). Starts with rows at t=0 and t=duration."),
                fluidRow(
                  column(3,
                    numericInput("wiz_outdoor_add_time","Add row at time (min)",value=60,min=0)),
                  column(3,
                    actionButton("wiz_outdoor_add_row","Add Time Point",
                      style="background:#27ae60;color:#fff;margin-top:25px;")),
                  column(3,
                    actionButton("wiz_init_outdoor","Initialize Table",
                      style="background:#3498DB;color:#fff;margin-top:25px;"))
                ),
                div(style="overflow-x:auto;margin-top:10px;",
                  rHandsontableOutput("wiz_outdoor_manual_table"))
              )
            )
          )
        )
      ),

      # ── Screen 12: Outputs ────────────────────────────────────────────────
      conditionalPanel(condition="input.wizard_current_screen_input==12",
        div(class="wizard-screen",
          div(class="screen-title","Output Settings"),
          uiOutput("wiz_val_banner_12"),
          fluidRow(
            column(6,
              textInput("wiz_output_table","Output Table File",value="./Output/ATL-CMAQ for Ambient"),
              textInput("wiz_output_chart","Basic Chart File", value="./Output/ATL-CMAQ for Ambient"),
              checkboxInput("wiz_output_derivatives","Generate Time Derivatives",value=TRUE),
              conditionalPanel(condition="input.wiz_output_derivatives",
                textInput("wiz_output_derivatives_file","Derivatives File",
                  value="./Output/ATL-CMAQ for Ambient Derivatives")),
              checkboxInput("wiz_output_massbalance","Generate Mass Balance",value=TRUE),
              conditionalPanel(condition="input.wiz_output_massbalance",
                textInput("wiz_output_massbalance_file","Mass Balance File",
                  value="./Output/ATL-CMAQ for Ambient Mass Balance"))
            ),
            column(6,
              checkboxInput("wiz_output_sensitivity","Generate Sensitivity Analysis",value=TRUE),
              conditionalPanel(condition="input.wiz_output_sensitivity",
                textInput("wiz_output_sensitivity_file","Sensitivity File",
                  value="./Output/ATL-CMAQ for Ambient Sensitivity")),
              checkboxInput("wiz_output_uncertainty","Generate Uncertainty Estimate",value=TRUE),
              conditionalPanel(condition="input.wiz_output_uncertainty",
                textInput("wiz_output_uncertainty_file","Uncertainty File",
                  value="./Output/ATL-CMAQ for Ambient uncertainty")),
              div(style="margin-top:16px;padding:12px;background:#eaf4fb;border-radius:5px;border-left:4px solid #2E86C1;",
              radioButtons("wiz_mechanism", "Chemical Mechanism",
                choiceNames  = list("SAPRC99 (default)"),
                choiceValues = c("SAPRC99"),
                selected = "SAPRC99", inline = FALSE),
              # SAPRC07T is planned but not yet wired through SIACS. Shown
              # here as a static greyed label (no radio input) so it is
              # visibly "on the roadmap" but literally cannot be clicked
              # or selected.
              div(style="margin-left:20px;color:#999;font-size:13px;",
                  "SAPRC07T")
              ),
              div(style="margin-top:12px;padding:14px;background:#d4edda;border-radius:5px;",
                h5(style="color:#155724;margin-top:0;","Ready to Generate Configuration"),
                p(style="color:#155724;margin-bottom:0;","Click 'Finish' to generate all input files."))
            )
          )
        )
      ),

      # Navigation
      footer=tagList(
        div(class="wizard-nav-buttons",
          fluidRow(
            column(4,conditionalPanel(condition="input.wizard_current_screen_input>1",
              actionButton("wizard_back","← Back",style="width:100%;"))),
            column(4,actionButton("wizard_cancel","Cancel",   style="width:100%;")),
            column(4,
              conditionalPanel(condition="input.wizard_current_screen_input<12",
                actionButton("wizard_next","Next →",
                  style="width:100%;background:#4CAF50;color:#fff;")),
              conditionalPanel(condition="input.wizard_current_screen_input==12",
                actionButton("wizard_finish","Finish",
                  style="width:100%;background:#2196F3;color:#fff;")))
          )
        )
      )
    )
  )
}

# ================================
# WIZARD SERVER
# ================================
wizard_module_server <- function(input, output, session) {

  # -----------------------------------------------------------------------
  # Load defaults once at server startup
  # -----------------------------------------------------------------------
  wizard_defaults <- tryCatch(load_wizard_defaults(),
    error=function(e){cat("WARNING: Could not load wizard defaults:",conditionMessage(e),"\n"); list()})

  # Glass types from file (for window renderUI)
  glass_types_avail <- c("None", "Laminate996")

  # Outdoor species from file (for manual entry table)
  outdoor_species <- if (!is.null(wizard_defaults$outdoor_species))
    wizard_defaults$outdoor_species else character(0)

  # -----------------------------------------------------------------------
  # Reactive state
  # -----------------------------------------------------------------------
  wizard_state <- reactiveValues(
    current_screen     = 1,
    max_screen         = 12,
    light_schedule     = NULL,   # data.frame: Time, Light1, Light2, ...
    phys_schedule      = NULL,   # data.frame: Time, Ti, To, OpenWindowArea, ...
    activity_schedules = list(), # named list of Time/Value data.frames
    outdoor_manual     = NULL,   # data.frame: Time + all species
    windows_data       = NULL,   # data.frame: one row per window — file-sourced truth
    initial_values_df  = NULL,   # data.frame: uploaded InitialIndoorConcentrations
    initial_values_name = NULL,  # character(1): original filename for snapshot
    defaults_loaded    = FALSE
  )

  # -----------------------------------------------------------------------
  # One-time initialisation (fires exactly once, no reactive deps)
  # -----------------------------------------------------------------------
  # Resets wizard_state to a clean baseline and re-applies the shipped
  # defaults. Called both on the first session flush AND on every
  # wizard_open_trigger (i.e. every time the user clicks "+ Add" and
  # picks Standard) so the wizard never shows stale fields from a
  # previous simulation.
  reset_wizard_to_defaults <- function() {
    isolate({
      wizard_state$current_screen      <- 1
      wizard_state$light_schedule      <- NULL
      wizard_state$phys_schedule       <- NULL
      wizard_state$activity_schedules  <- list()
      wizard_state$outdoor_manual      <- NULL
      wizard_state$windows_data        <- NULL
      wizard_state$initial_values_df   <- NULL
      wizard_state$initial_values_name <- NULL
      wizard_state$uploaded_lights_list  <- NULL
      wizard_state$uploaded_lights_sched <- NULL
      wizard_state$defaults_loaded     <- FALSE
    })
    updateNumericInput(session, "wizard_current_screen_input", value = 1)
    if (length(wizard_defaults) > 0) {
      update_wizard_inputs_with_defaults(session, wizard_defaults)
      isolate({
        populate_activity_defaults(wizard_state, wizard_defaults)
        populate_light_schedule_defaults(wizard_state, wizard_defaults)
        populate_phys_schedule_defaults(wizard_state, wizard_defaults)
        if (!is.null(wizard_defaults$windows) &&
            nrow(wizard_defaults$windows) > 0) {
          wizard_state$windows_data <- wizard_defaults$windows
        }
        wizard_state$defaults_loaded <- TRUE
      })
    }
    invisible(NULL)
  }

  session$onFlushed(function() {
    reset_wizard_to_defaults()
  }, once = TRUE)

  # -----------------------------------------------------------------------
  # Progress bar & title
  # -----------------------------------------------------------------------
  output$wizard_progress_text <- renderUI({
    pct <- wizard_state$current_screen / wizard_state$max_screen * 100
    shinyjs::runjs(sprintf("$('.wizard-progress-bar').css('width','%f%%');", pct))
    paste0(wizard_state$current_screen, " / ", wizard_state$max_screen)
  })

  output$wizard_title_text <- renderText({
    titles <- c("Location","Start Time","Simulation Time","The Box",
                "Building","Outside","Artificial Lights","Light Schedule",
                "Physical Environment","Indoor Sources","Outdoor Concentrations","Output Settings")
    paste("SIACS Wizard —", titles[wizard_state$current_screen])
  })

  # -----------------------------------------------------------------------
  # Screen 5: stories → neutral_pressure, stack/wind helpers
  # -----------------------------------------------------------------------
  # Helper: look up wind coefficient from Table S2
  get_wind_coeff <- function(stories, shelter_class) {
    n  <- as.integer(stories)
    sc <- as.integer(shelter_class)
    if (is.na(n) || is.na(sc) || n < 1 || n > 3 || sc < 1 || sc > 5) return(NA)
    col <- c("one","two","three")[n]
    wind_coeff_table[wind_coeff_table$shelter_class == sc, col]
  }

  # Stack coefficient updates when number of stories changes (Table S1)
  observeEvent(input$wiz_num_stories, {
    req(input$wiz_num_stories)
    n <- as.integer(input$wiz_num_stories)
    if (n >= 1 && n <= 3) {
      updateNumericInput(session, "wiz_neutral_pressure", value = round(1.35 * n, 3))
      updateNumericInput(session, "wiz_stack_coeff",      value = stack_coeff_table$ks[n])
    }
  }, ignoreInit=TRUE)

  # Wind coefficient updates when stories OR shelter class changes (Table S2)
  observe({
    n  <- input$wiz_num_stories  %||% 2
    sc <- input$wiz_shelter_class %||% names(shelter_classes)[3]
    sc_val <- shelter_classes[sc]
    kw <- get_wind_coeff(n, sc_val)
    if (!is.na(kw))
      updateNumericInput(session, "wiz_wind_coeff", value = kw)
  })

  output$wiz_stack_coeff_display <- renderUI({
    n <- as.integer(input$wiz_num_stories %||% 2)
    if (n < 1 || n > 3) return(NULL)
    p(style="color:#666;font-size:12px;",
      sprintf("%d stor%s: ks = %s",
        n, ifelse(n==1,"y","ies"),
        format(stack_coeff_table$ks[n], scientific=TRUE, digits=3)))
  })

  output$wiz_wind_coeff_display <- renderUI({
    n  <- as.integer(input$wiz_num_stories  %||% 2)
    sc <- input$wiz_shelter_class %||% names(shelter_classes)[3]
    sc_val <- shelter_classes[sc]
    kw <- get_wind_coeff(n, sc_val)
    if (is.na(kw)) return(NULL)
    p(style="color:#666;font-size:12px;",
      sprintf("Class %d, %d stor%s: kw = %s",
        sc_val, n, ifelse(n==1,"y","ies"),
        format(kw, scientific=TRUE, digits=3)))
  })

  # -----------------------------------------------------------------------
  # Screen 5: Dynamic Windows UI  (default values baked in — no timing issue)
  # -----------------------------------------------------------------------
  output$wiz_windows_ui <- renderUI({
    n <- input$wiz_num_windows
    if (is.null(n) || n == 0) return(p("No windows configured."))
    win_df <- wizard_defaults$windows
    lapply(1:n, function(i) {
      has <- !is.null(win_df) && i <= nrow(win_df)
      row <- if (has) win_df[i,] else NULL
      gv  <- function(col, fb) if (!is.null(row) && col %in% names(row)) row[[col]] else fb
      wellPanel(
        h5(paste("Window", i)),
        fluidRow(
          column(6,
            numericInput(paste0("wiz_window_",i,"_orientation"),"Orientation (° CW from N)",
              value=gv("Orientation",90), min=0,max=360),
            numericInput(paste0("wiz_window_",i,"_aspect"),"Aspect Ratio (H/W)",
              value=gv("AspectRatio",2), min=0.1,step=0.1),
            numericInput(paste0("wiz_window_",i,"_fraction"),"Wall Surface Fraction (0–1)",
              value=gv("WallSurfaceFraction",0.2),min=0,max=1,step=0.01)),
          column(6,
            selectInput(paste0("wiz_window_",i,"_glass"),"Glass Type",
              choices=glass_types_avail,
              selected=gv("GlassType","None")),
            numericInput(paste0("wiz_window_",i,"_obstruction"),"Obstructed Area Fraction (0–1)",
              value=gv("ObstructedAreaFraction",0),min=0,max=1,step=0.01),
            numericInput(paste0("wiz_window_",i,"_horizon"),"Horizon Elevation Angle (°)",
              value=0,min=0,max=90))
        )
      )
    })
  })

  # -----------------------------------------------------------------------
  # Screen 5: Capture explicit user edits → wizard_state$windows_data
  #
  # IMPORTANT: This observer uses req() on input$wiz_window_1_orientation
  # (a per-window field that only exists in the browser after Screen 5 is
  # rendered via renderUI).  That req() acts as a guard: the observer does
  # NOT fire during startup while those inputs are absent, so it cannot
  # overwrite the file-seeded wizard_state$windows_data with stale
  # definition-time defaults.  It only runs after the user has visited
  # Screen 5 at least once and the browser has acknowledged the inputs.
  # -----------------------------------------------------------------------
  observeEvent(
    # Depend on ALL per-window inputs so any edit triggers a rebuild.
    # The list() wraps them into a single reactive expression.
    list(
      input$wiz_num_windows,
      input$wiz_window_1_orientation, input$wiz_window_1_aspect,
      input$wiz_window_1_fraction,    input$wiz_window_1_glass,
      input$wiz_window_1_obstruction, input$wiz_window_1_horizon,
      input$wiz_window_2_orientation, input$wiz_window_2_aspect,
      input$wiz_window_2_fraction,    input$wiz_window_2_glass,
      input$wiz_window_2_obstruction, input$wiz_window_2_horizon,
      input$wiz_window_3_orientation, input$wiz_window_3_aspect,
      input$wiz_window_3_fraction,    input$wiz_window_3_glass,
      input$wiz_window_3_obstruction, input$wiz_window_3_horizon
    ),
    {
      n <- input$wiz_num_windows
      # Guard: only proceed if Screen 5 has been rendered in the browser.
      # input$wiz_window_1_orientation is a renderUI-generated input that does
      # not exist server-side until the client has rendered it at least once.
      req(!is.null(input$wiz_window_1_orientation), n, n > 0)

      rows <- lapply(seq_len(n), function(i) {
        # For windows beyond the first (whose inputs may not yet exist if
        # wiz_num_windows was just increased), fall back to wizard_defaults.
        win_df <- wizard_defaults$windows
        gv_fb  <- function(input_val, col, fb) {
          if (!is.null(input_val)) return(input_val)
          if (!is.null(win_df) && i <= nrow(win_df) && col %in% names(win_df))
            return(win_df[i, col])
          fb
        }
        data.frame(
          WindowNumber           = as.integer(i),
          Orientation            = gv_fb(input[[paste0("wiz_window_",i,"_orientation")]], "Orientation",            90),
          AspectRatio            = gv_fb(input[[paste0("wiz_window_",i,"_aspect")]],      "AspectRatio",            2),
          WallSurfaceFraction    = gv_fb(input[[paste0("wiz_window_",i,"_fraction")]],    "WallSurfaceFraction",    0.2),
          GlassType              = gv_fb(input[[paste0("wiz_window_",i,"_glass")]],       "GlassType",              "None"),
          ObstructedAreaFraction = gv_fb(input[[paste0("wiz_window_",i,"_obstruction")]], "ObstructedAreaFraction", 0),
          HorizonElevationAngle  = gv_fb(input[[paste0("wiz_window_",i,"_horizon")]],     "HorizonElevationAngle",  0),
          stringsAsFactors = FALSE
        )
      })
      wizard_state$windows_data <- do.call(rbind, rows)
    },
    ignoreInit = TRUE   # never run at startup — only on genuine user changes
  )

  # -----------------------------------------------------------------------
  # Screen 7: Dynamic Lights UI
  # -----------------------------------------------------------------------
  output$wiz_lights_ui <- renderUI({
    n <- input$wiz_num_lights
    if (is.null(n) || n == 0) return(p("No lights configured."))
    lit_df <- wizard_defaults$lights
    lapply(1:n, function(i) {
      has <- !is.null(lit_df) && i <= nrow(lit_df)
      row <- if (has) lit_df[i,] else NULL
      gv  <- function(col, fb) if (!is.null(row) && col %in% names(row)) row[[col]] else fb
      wellPanel(
        h5(paste("Light", i)),

        # ── Position ──────────────────────────────────────────────────────
        div(class="panel-title", style="font-size:13px;margin-bottom:4px;", "Position"),
        p(style="color:#666;font-size:11px;margin-top:0;margin-bottom:6px;",
          "Fractional position within the room. 0 = at the wall/floor, 1 = at the opposite wall/ceiling. ",
          "(0.5, 0.5) = centred in plan view. The model multiplies these by actual room dimensions to get metres."),
        fluidRow(
          column(4,
            numericInput(paste0("wiz_light_",i,"_height"),
              "Height (0=floor, 1=ceiling)",
              value=gv("Height",0.9), min=0, max=1, step=0.01)),
          column(4,
            numericInput(paste0("wiz_light_",i,"_dist_short"),
              "Position along shorter wall (0–1)",
              value=gv("DistanceShorterWall",0.5), min=0, max=1, step=0.01)),
          column(4,
            numericInput(paste0("wiz_light_",i,"_dist_long"),
              "Position along longer wall (0–1)",
              value=gv("DistanceLongerWall",0.5), min=0, max=1, step=0.01))
        ),

        # ── Light source properties ────────────────────────────────────────
        div(class="panel-title", style="font-size:13px;margin-bottom:4px;margin-top:8px;", "Light Source"),
        fluidRow(
          column(4,
            selectInput(paste0("wiz_light_",i,"_geometry"),
              "Geometry",
              choices=light_geometries, selected=gv("Geometry","point"))),
          column(4,
            numericInput(paste0("wiz_light_",i,"_size"),
              "Size — characteristic radius (m)",
              value=gv("Size",0.1), min=0, step=0.01)),
          column(4,
            numericInput(paste0("wiz_light_",i,"_efficiency"),
              "Power Efficiency (0–1)",
              value=gv("PowerEfficiency",0.95), min=0, max=1, step=0.01))
        ),
        fluidRow(
          column(4,
            selectInput(paste0("wiz_light_",i,"_bulb"),
              "Bulb / Spectrum Type",
              choices=bulb_types, selected=gv("Spectrum","Kowal.LED")))
        ),

        # ── Emission direction (surface geometry only) ─────────────────────
        div(class="panel-title", style="font-size:13px;margin-bottom:4px;margin-top:8px;", "Emission Direction"),
        p(style="color:#666;font-size:11px;margin-top:0;margin-bottom:6px;",
          "Used only for ‘surface’ geometry. Defines the 3-D unit vector the surface faces: ",
          "points on the opposite side receive no direct light. ",
          "For ‘point’ geometry these values are ignored. ",
          "Example: ceiling downlight facing the floor = (0, 0, −1)."),
        fluidRow(
          column(4,
            numericInput(paste0("wiz_light_",i,"_dir_short"),
              "Along shorter wall (−1 to 1)",
              value=gv("DirectionShorter",0), min=-1, max=1, step=0.1)),
          column(4,
            numericInput(paste0("wiz_light_",i,"_dir_long"),
              "Along longer wall (−1 to 1)",
              value=gv("DirectionLonger",0), min=-1, max=1, step=0.1)),
          column(4,
            numericInput(paste0("wiz_light_",i,"_dir_height"),
              "Along height (−1=down, +1=up)",
              value=gv("DirectionHeight",0), min=-1, max=1, step=0.1))
        )
      )
    })
  })

  # Update light selector when count changes (only relevant in manual mode)
  observe({
    n <- input$wiz_num_lights
    if (!is.null(n) && n > 0)
      updateSelectInput(session,"wiz_light_sel", choices=paste0("Light",1:n))
  })

  # -----------------------------------------------------------------------
  # Screen 7: Preview default lights files
  # -----------------------------------------------------------------------
  observeEvent(input$wiz_preview_lights_default, {
    list_path  <- find_input_file("ArtificialLightList")
    sched_path <- find_input_file("ArtificialLightSchedule")

    list_df  <- if (!is.null(list_path))  tryCatch(read.csv(list_path,  stringsAsFactors=FALSE), error=function(e) NULL) else NULL
    sched_df <- if (!is.null(sched_path)) tryCatch(read.csv(sched_path, stringsAsFactors=FALSE), error=function(e) NULL) else NULL

    showModal(modalDialog(
      title = "Default Artificial Light Files",
      size  = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      if (!is.null(list_df)) tagList(
        h5(paste0("ArtificialLightList.csv (", nrow(list_df), " lights)")),
        div(style="overflow-x:auto;margin-bottom:16px;",
          renderTable(head(list_df, 20)))
      ) else p(style="color:orange;", "ArtificialLightList.csv not found in Input/"),
      if (!is.null(sched_df)) tagList(
        h5(paste0("ArtificialLightSchedule.csv (", nrow(sched_df), " rows)")),
        div(style="overflow-x:auto;",
          renderTable(head(sched_df, 10)))
      ) else p(style="color:orange;", "ArtificialLightSchedule.csv not found in Input/")
    ))
  })

  # -----------------------------------------------------------------------
  # Screen 4: Handle uploaded / preview for Initial Indoor Concentrations
  # -----------------------------------------------------------------------
  # Reads the user-supplied CSV, validates it with the same rule the Advanced
  # module uses (validate_initial_concentrations from shared.r), and caches
  # the parsed data.frame in wizard_state$initial_values_df. The assembler
  # (handle_wizard_initial_values in wizard_helpers.R) picks it up at Finish.
  observeEvent(input$wiz_init_values_file, {
    req(input$wiz_init_values_file)
    df <- tryCatch(
      read.csv(input$wiz_init_values_file$datapath,
               stringsAsFactors = FALSE, comment.char = "#"),
      error = function(e) {
        showNotification(paste("Could not read initial concentrations file:",
                               e$message), type = "error")
        NULL
      }
    )
    if (is.null(df)) return(NULL)

    # Validate with the same rule the Advanced module uses
    vres <- tryCatch(validate_initial_concentrations(df),
                     error = function(e) list(valid = FALSE,
                                              errors = paste("Validator error:", e$message),
                                              warnings = character(0)))
    if (isFALSE(vres$valid)) {
      showModal(modalDialog(
        title = HTML("<span style='color:#7b0000;'>&#9888; Initial Concentrations — Validation Failed</span>"),
        HTML(paste0(
          "<p>The uploaded file could not be accepted:</p><ul>",
          paste0("<li style='color:#7b0000;'>", vres$errors, "</li>", collapse=""),
          "</ul>")),
        easyClose = TRUE, footer = modalButton("OK")
      ))
      wizard_state$initial_values_df <- NULL
      return(NULL)
    }

    wizard_state$initial_values_df <- df
    wizard_state$initial_values_name <- input$wiz_init_values_file$name
    showNotification(
      paste0("Initial concentrations loaded: ", ncol(df), " species."),
      type = "message")
  })

  # Preview default InitialIndoorConcentrations.csv
  observeEvent(input$wiz_preview_init_default, {
    fpath <- file.path("Input", "InitialIndoorConcentrations.csv")
    if (!file.exists(fpath)) {
      showNotification("Default InitialIndoorConcentrations.csv not found.", type = "error")
      return(NULL)
    }
    df <- tryCatch(read.csv(fpath, stringsAsFactors = FALSE, comment.char = "#"),
                   error = function(e) NULL)
    if (is.null(df)) {
      showNotification("Could not read default initial concentrations file.", type = "error")
      return(NULL)
    }
    # Show only first 15 columns to keep modal readable (file has ~45 species)
    df_show <- df[, 1:min(15, ncol(df)), drop = FALSE]
    output$wiz_init_preview_tbl <- renderTable(df_show)
    showModal(modalDialog(
      title = sprintf("Default Initial Indoor Concentrations — first 15 of %d species",
                      ncol(df)),
      div(style = "overflow-x:auto;", tableOutput("wiz_init_preview_tbl")),
      size = "l", easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # -----------------------------------------------------------------------
  # Screen 7: Handle uploaded light files — load into wizard_state so
  # screen 8 confirmation stays consistent and assembly picks them up
  # -----------------------------------------------------------------------
  observeEvent(input$wiz_lights_list_file, {
    req(input$wiz_lights_list_file)
    df <- tryCatch(
      read.csv(input$wiz_lights_list_file$datapath, stringsAsFactors=FALSE),
      error = function(e) { showNotification(paste("Could not read light list:", e$message), type="error"); NULL }
    )
    if (!is.null(df)) {
      wizard_state$uploaded_lights_list <- df
      showNotification(paste0("Light list loaded: ", nrow(df), " light(s)."), type="message")
    }
  })

  observeEvent(input$wiz_lights_sched_file, {
    req(input$wiz_lights_sched_file)
    df <- tryCatch(
      read.csv(input$wiz_lights_sched_file$datapath, stringsAsFactors=FALSE),
      error = function(e) { showNotification(paste("Could not read light schedule:", e$message), type="error"); NULL }
    )
    if (!is.null(df)) {
      wizard_state$uploaded_lights_sched <- df
      showNotification(paste0("Light schedule loaded: ", nrow(df), " row(s)."), type="message")
    }
  })

  # -----------------------------------------------------------------------
  # Screen 8: Light Schedule  – Time On / Time Off
  # -----------------------------------------------------------------------

  # Helper: get or initialise light schedule
  get_light_schedule <- function() {
    if (!is.null(wizard_state$light_schedule)) return(wizard_state$light_schedule)
    n    <- input$wiz_num_lights %||% 0
    if (n == 0) return(NULL)
    dur  <- (input$wiz_duration %||% 27) * 60
    df   <- data.frame(Time = c(0, dur), stringsAsFactors = FALSE)
    for (lc in paste0("Light",1:n)) df[[lc]] <- 0
    df
  }

  output$wiz_light_schedule_table <- renderRHandsontable({
    sched <- get_light_schedule()
    if (is.null(sched)) return(NULL)
    rhandsontable(sched, rowHeaders=NULL, stretchH="all") %>%
      hot_table(highlightCol=TRUE, highlightRow=TRUE)
  })

  # Capture edits
  observeEvent(input$wiz_light_schedule_table, {
    req(input$wiz_light_schedule_table)
    wizard_state$light_schedule <- fill_schedule_gaps(hot_to_r(input$wiz_light_schedule_table))
  }, ignoreInit=TRUE)

  # Time On
  observeEvent(input$wiz_light_on, {
    req(input$wiz_light_sel, input$wiz_light_event_time, input$wiz_light_event_power)
    sched    <- get_light_schedule()
    if (is.null(sched)) return()
    col      <- input$wiz_light_sel
    t        <- input$wiz_light_event_time
    new_val  <- input$wiz_light_event_power
    trans    <- input$wiz_activity_transition %||% 0.1
    wizard_state$light_schedule <- fill_schedule_gaps(insert_step_change_wide(sched, col, t, new_val, trans))
  })

  # Time Off
  observeEvent(input$wiz_light_off, {
    req(input$wiz_light_sel, input$wiz_light_event_time)
    sched    <- get_light_schedule()
    if (is.null(sched)) return()
    col      <- input$wiz_light_sel
    t        <- input$wiz_light_event_time
    trans    <- input$wiz_activity_transition %||% 0.1
    wizard_state$light_schedule <- fill_schedule_gaps(insert_step_change_wide(sched, col, t, 0, trans))
  })

  # Reset to default schedule
  observeEvent(input$wiz_light_sched_reset, {
    isolate(populate_light_schedule_defaults(wizard_state, wizard_defaults))
    showNotification("Light schedule reset to default.", type="message")
  })

  # -----------------------------------------------------------------------
  # Screen 9: Physical Environment
  # -----------------------------------------------------------------------

  # Initialize schedule from current initial/final values
  build_phys_schedule <- function() {
    dur <- (input$wiz_duration %||% 27) * 60
    data.frame(
      Time           = c(0, dur),
      Ti             = c(input$wiz_temp_indoor_init  %||% 295.15,  input$wiz_temp_indoor_final  %||% 295.15),
      To             = c(input$wiz_temp_outdoor_init %||% 303.544, input$wiz_temp_outdoor_final %||% 298),
      OpenWindowArea = c(0, 0),
      QBal           = c(0, 0),
      QUnbal         = c(0, 0),
      QFilter        = c(0, 0),
      RH             = c(input$wiz_rh_init  %||% 0.55,   input$wiz_rh_final  %||% 0.55),
      BP             = c(input$wiz_bp_init  %||% 101325, input$wiz_bp_final  %||% 101325),
      Wind           = c(input$wiz_wind_init %||% 1.41,  input$wiz_wind_final %||% 1.41),
      stringsAsFactors = FALSE
    )
  }

  # Auto-initialize when switching to Manual if schedule not yet set
  observeEvent(input$wiz_phys_env_mode, {
    if (input$wiz_phys_env_mode == "Manual entry" && is.null(wizard_state$phys_schedule))
      isolate(wizard_state$phys_schedule <- build_phys_schedule())
  })

  observeEvent(input$wiz_init_phys_schedule, {
    wizard_state$phys_schedule <- build_phys_schedule()
    showNotification("Physical environment schedule initialized.", type="message")
  })

  output$wiz_phys_schedule_table <- renderRHandsontable({
    req(wizard_state$phys_schedule)
    rhandsontable(wizard_state$phys_schedule, rowHeaders=NULL, stretchH="all") %>%
      hot_table(highlightCol=TRUE, highlightRow=TRUE)
  })

  observeEvent(input$wiz_phys_schedule_table, {
    req(input$wiz_phys_schedule_table)
    wizard_state$phys_schedule <- fill_schedule_gaps(hot_to_r(input$wiz_phys_schedule_table))
  }, ignoreInit=TRUE)

  observeEvent(input$wiz_add_phys_step, {
    req(wizard_state$phys_schedule, input$wiz_phys_step_var,
        input$wiz_phys_step_time, input$wiz_phys_step_value)
    trans <- input$wiz_activity_transition %||% 0.1
    wizard_state$phys_schedule <- fill_schedule_gaps(insert_step_change_wide(
      wizard_state$phys_schedule,
      input$wiz_phys_step_var,
      input$wiz_phys_step_time,
      input$wiz_phys_step_value,
      trans
    ))
  })

  # Preview default physical environment file
  observeEvent(input$wiz_preview_phys_default, {
    phys_path <- file_paths_advanced[3]
    if (!file.exists(phys_path)) {
      showModal(modalDialog(title="File Not Found",
        "Default physical environment file not found.", easyClose=TRUE, footer=modalButton("OK")))
      return()
    }
    df <- tryCatch(
      read.csv(phys_path, stringsAsFactors=FALSE, comment.char="#", nrows=10),
      error=function(e) NULL)
    if (is.null(df)) return()
    output$wiz_phys_preview_tbl <- renderTable(head(df, 10))
    showModal(modalDialog(
      title="Physical Environment — first 10 rows",
      tableOutput("wiz_phys_preview_tbl"),
      size="l", easyClose=TRUE, footer=modalButton("Close")))
  })

  # -----------------------------------------------------------------------
  # Screen 10: source profile description panel
  output$wiz_activity_profile_desc <- renderUI({
    prof <- input$wiz_activity_type %||% "Generic"
    desc <- switch(prof,
      "Generic" = list(
        title = "Generic",
        body  = paste0(
          "A general-purpose emission profile not tied to any specific source. ",
          "Emission rates are set to 1 (dimensionless multiplier) by default, so the ",
          "'Value' you enter directly scales total emissions. ",
          tags$b("Limitation:"), " No real-world measurement basis — treat results as ",
          "order-of-magnitude estimates only. Requires expert judgement to assign ",
          "meaningful emission rates.")
      ),
      "Adult" = list(
        title = "Adult occupant",
        body  = paste0(
          "Represents the presence of one adult occupant in the room. Accounts for ",
          "CO₂, water vapour, and trace VOC emissions from human metabolism and ",
          "breath. Based on standard occupant emission factors from the literature. ",
          tags$b("Limitation:"), " Assumes average metabolic activity (sedentary). ",
          "Strenuous activity increases emission rates substantially.")
      ),
      "Smoking" = list(
        title = "Cigarette smoking",
        body  = paste0(
          "Represents one person smoking one cigarette at a time. Emission factors are ",
          "derived from chamber studies of mainstream and sidestream smoke. ",
          tags$b("Assumption:"), " one adult smoker present in the room. ",
          tags$b("Limitation:"), " Emission rates vary substantially by cigarette brand, ",
          "puffing frequency, and ventilation. Validate against measured data where ",
          "possible. Ensure at least one adult occupant is also active (use the Adult ",
          "profile) for realistic occupancy conditions.")
      ),
      "GasCooking.Persily1998" = list(
        title = "Gas cooking (Persily et al. 1998)",
        body  = paste0(
          "Emission profile for a natural gas cooking stove derived from measurements ",
          "reported in Persily et al. (1998). Covers CO, NOₓ, and VOC species ",
          "generated during cooking events. ",
          tags$b("Limitation:"), " Based on a single study; burner type, heat setting, ",
          "and food being cooked are not captured. Best used for residential kitchens ",
          "with standard burners. Should be combined with realistic ventilation settings.")
      ),
      "Incense.Manoukian2013" = list(
        title = "Incense burning (Manoukian et al. 2013)",
        body  = paste0(
          "Emission profile for a single burning incense stick based on measurements in ",
          "Manoukian et al. (2013). Covers PM, CO, and a range of carbonyl and aromatic ",
          "VOC species. ",
          tags$b("Limitation:"), " Highly variable across incense types and brands. ",
          "The profile represents one specific product tested under controlled conditions. ",
          "Extrapolation to other incense types introduces significant uncertainty.")
      ),
      list(title = prof, body = "No description available for this profile.")
    )
    div(style = paste0("background:#eaf4fb;border-left:4px solid #2E86C1;",
                       "border-radius:4px;padding:8px 12px;margin-top:6px;font-size:11px;",
                       "line-height:1.5;color:#1a1a1a;"),
      tags$b(desc$title), tags$br(),
      HTML(desc$body)
    )
  })

  # Screen 10: Activities
  # -----------------------------------------------------------------------

  # Initialize single activity to flat schedule at initial/final values
  observeEvent(input$wiz_init_activity, {
    act <- input$wiz_activity_type
    req(act)
    dur <- (input$wiz_duration %||% 27) * 60
    wizard_state$activity_schedules[[act]] <- data.frame(
      Time  = c(0, dur),
      Value = c(input$wiz_activity_init %||% 0, input$wiz_activity_final %||% 0),
      stringsAsFactors = FALSE
    )
    showNotification(paste("Schedule for", act, "initialized."), type="message")
  })

  # Track which activity was rendered into the table last.
  # When the user switches activity type, renderRHandsontable fires first
  # (updating the DOM) and input$wiz_activity_type has already changed to the
  # new value.  Without this guard the write-back observer would store the
  # OLD activity's data into the NEW activity's slot, corrupting both.
  activity_table_rendered_for <- reactiveVal(NULL)

  output$wiz_activity_schedule_table <- renderRHandsontable({
    act <- input$wiz_activity_type
    req(act, wizard_state$activity_schedules[[act]])
    activity_table_rendered_for(act)   # record which activity this table shows
    rhandsontable(wizard_state$activity_schedules[[act]], rowHeaders=NULL, stretchH="all") %>%
      hot_table(highlightCol=TRUE, highlightRow=TRUE)
  })

  observeEvent(input$wiz_activity_schedule_table, {
    rendered_for <- isolate(activity_table_rendered_for())
    req(input$wiz_activity_schedule_table, !is.null(rendered_for))
    wizard_state$activity_schedules[[rendered_for]] <-
      fill_schedule_gaps(hot_to_r(input$wiz_activity_schedule_table))
  }, ignoreInit=TRUE)

  observeEvent(input$wiz_add_activity_event, {
    act <- input$wiz_activity_type
    req(act, input$wiz_activity_event_time, input$wiz_activity_event_value)
    trans <- input$wiz_activity_transition %||% 0.1
    sched <- wizard_state$activity_schedules[[act]]
    if (is.null(sched)) {
      dur   <- (input$wiz_duration %||% 27) * 60
      sched <- data.frame(Time=c(0,dur), Value=c(0,0), stringsAsFactors=FALSE)
    }
    wizard_state$activity_schedules[[act]] <- fill_schedule_gaps(insert_step_change_two_col(
      sched, input$wiz_activity_event_time, input$wiz_activity_event_value, trans))
  })

  observeEvent(input$wiz_reset_activity, {
    isolate(populate_activity_defaults(wizard_state, wizard_defaults))
    showNotification("All activity schedules reset to default.", type="message")
  })

  # Preview default activities file
  observeEvent(input$wiz_preview_act_default, {
    act_path <- file_paths_advanced[5]
    if (!file.exists(act_path)) {
      showModal(modalDialog(title="File Not Found",
        "Default activities file not found.", easyClose=TRUE, footer=modalButton("OK")))
      return()
    }
    df <- tryCatch(
      read.csv(act_path, stringsAsFactors=FALSE, comment.char="#"),
      error=function(e) NULL)
    if (is.null(df)) return()
    output$wiz_act_preview_tbl <- renderTable(df)
    showModal(modalDialog(
      title="Activities — default file",
      tableOutput("wiz_act_preview_tbl"),
      size="l", easyClose=TRUE, footer=modalButton("Close")))
  })

  # -----------------------------------------------------------------------
  # Screen 11: Outdoor Concentrations
  # -----------------------------------------------------------------------

  # Initialize manual entry table: Time=c(0,duration), all species = 0
  build_outdoor_manual <- function() {
    dur   <- (input$wiz_duration %||% 27) * 60
    spcols <- if (length(outdoor_species) > 0) outdoor_species else
      c("O3","NO","NO2","CO","SO2","PM25")
    df <- data.frame(Time = c(0, dur), stringsAsFactors=FALSE)
    for (sp in spcols) df[[sp]] <- 0
    df
  }

  observeEvent(input$wiz_init_outdoor, {
    wizard_state$outdoor_manual <- build_outdoor_manual()
    showNotification("Outdoor concentrations table initialized.", type="message")
  })

  # Auto-initialize when mode switches to Manual
  observeEvent(input$wiz_outdoor_conc_mode, {
    if (input$wiz_outdoor_conc_mode == "Manual entry" && is.null(wizard_state$outdoor_manual))
      isolate(wizard_state$outdoor_manual <- build_outdoor_manual())
  })

  # Add row at specified time
  observeEvent(input$wiz_outdoor_add_row, {
    req(wizard_state$outdoor_manual, input$wiz_outdoor_add_time)
    df    <- wizard_state$outdoor_manual
    t_new <- input$wiz_outdoor_add_time
    if (t_new %in% df$Time) return()
    # Copy the last fully-populated row as the template (not row 1)
    last_complete_idx <- max(which(complete.cases(df)))
    new_row <- df[last_complete_idx, , drop = FALSE]
    new_row$Time <- t_new
    df <- rbind(df, new_row)
    wizard_state$outdoor_manual <- fill_schedule_gaps(df[order(df$Time), ])
  })

  output$wiz_outdoor_manual_table <- renderRHandsontable({
    req(wizard_state$outdoor_manual)
    rhandsontable(wizard_state$outdoor_manual, rowHeaders=NULL, stretchH="none") %>%
      hot_table(highlightCol=TRUE, highlightRow=TRUE)
  })

  observeEvent(input$wiz_outdoor_manual_table, {
    req(input$wiz_outdoor_manual_table)
    wizard_state$outdoor_manual <- fill_schedule_gaps(hot_to_r(input$wiz_outdoor_manual_table))
  }, ignoreInit=TRUE)

  # Preview default outdoor file
  observeEvent(input$wiz_preview_outdoor_default, {
    oc_path <- if (!is.null(wizard_defaults$outdoor_path)) wizard_defaults$outdoor_path else
      tryCatch(file_paths_advanced[which(grepl("Outdoor",titles_advanced,ignore.case=TRUE))[1]],
               error=function(e) NULL)
    if (is.null(oc_path) || !file.exists(oc_path)) {
      showModal(modalDialog(title="File Not Found",
        "Default outdoor concentrations file not found.", easyClose=TRUE, footer=modalButton("OK")))
      return()
    }
    df <- tryCatch(
      read.csv(oc_path, stringsAsFactors=FALSE, comment.char="#", nrows=5),
      error=function(e) NULL)
    if (is.null(df)) return()
    # Show only first 12 columns to keep modal readable
    df_show <- df[, 1:min(12, ncol(df)), drop=FALSE]
    output$wiz_outdoor_preview_tbl <- renderTable(df_show)
    showModal(modalDialog(
      title=sprintf("Outdoor Concentrations — first 5 rows, first 12 of %d columns", ncol(df)),
      div(style="overflow-x:auto;", tableOutput("wiz_outdoor_preview_tbl")),
      size="xl", easyClose=TRUE, footer=modalButton("Close")))
  })

  # -----------------------------------------------------------------------
  # VALIDATION HELPERS
  # -----------------------------------------------------------------------

  # Render an inline banner from a list(errors=..., warnings=...)
  render_banner <- function(res) {
    if (length(res$errors) == 0 && length(res$warnings) == 0) return(NULL)
    err_html <- if (length(res$errors) > 0)
      paste0("<div style='background:#fdecea;border:1px solid #f5c6cb;border-radius:4px;",
             "padding:8px 12px;margin-bottom:6px;'>",
             "<b style='color:#7b0000;'>&#9888; Errors:</b><ul style='margin:4px 0 0 0;padding-left:20px;'>",
             paste0("<li style='color:#7b0000;'>", res$errors, "</li>", collapse=""),
             "</ul></div>") else ""
    warn_html <- if (length(res$warnings) > 0)
      paste0("<div style='background:#fff3cd;border:1px solid #ffc107;border-radius:4px;",
             "padding:8px 12px;margin-bottom:6px;'>",
             "<b style='color:#7d4e00;'>&#9432; Warnings:</b><ul style='margin:4px 0 0 0;padding-left:20px;'>",
             paste0("<li style='color:#7d4e00;'>", res$warnings, "</li>", collapse=""),
             "</ul></div>") else ""
    HTML(paste0(err_html, warn_html))
  }

  # ── Per-screen validators ───────────────────────────────────────────────

  validate_screen_1 <- function(input) {
    errors <- character(0); warnings <- character(0)
    lat <- input$wiz_latitude  %||% NA
    lon <- input$wiz_longitude %||% NA
    alt <- input$wiz_altitude  %||% NA
    grv <- input$wiz_gravity   %||% NA
    if (!is.na(lat) && (lat < -90  || lat > 90))   errors   <- c(errors,   "Latitude must be between -90 and +90°")
    if (!is.na(lon) && (lon < -180 || lon > 180))   errors   <- c(errors,   "Longitude must be between -180 and +180°")
    if (!is.na(alt) && alt < 0)                     errors   <- c(errors,   "Altitude cannot be negative")
    if (!is.na(grv) && grv <= 0)                    errors   <- c(errors,   "Gravity must be greater than 0 m/s²")
    if (!is.na(grv) && (grv < 9.0 || grv > 10.0))  warnings <- c(warnings, "Gravity is outside the typical range (9.0–10.0 m/s²)")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_2 <- function(input) {
    errors <- character(0); warnings <- character(0)
    yr  <- input$wiz_start_year    %||% NA
    mo  <- input$wiz_start_month   %||% NA
    dy  <- input$wiz_start_day     %||% NA
    rst <- input$wiz_relative_start %||% NA
    st  <- input$wiz_start_time    %||% ""
    if (!is.na(yr) && yr != floor(yr))           errors   <- c(errors,   "Year must be a whole number")
    if (!is.na(mo) && mo != floor(mo))           errors   <- c(errors,   "Month must be a whole number")
    if (!is.na(dy) && dy != floor(dy))           errors   <- c(errors,   "Day must be a whole number")
    if (!is.na(yr) && (yr < 1900 || yr > 2100)) warnings <- c(warnings, "Year is outside typical range (1900–2100)")
    if (!is.na(yr) && yr < 0)                   errors   <- c(errors,   "Year cannot be negative")
    if (!is.na(mo) && (mo < 1 || mo > 12))      errors   <- c(errors,   "Month must be between 1 and 12")
    if (!is.na(dy) && (dy < 1 || dy > 31))      errors   <- c(errors,   "Day must be between 1 and 31")
    if (!is.na(rst) && rst < 0)                 errors   <- c(errors,   "Relative Start Time cannot be negative")
    if (nzchar(st) && !grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$", st))
      warnings <- c(warnings, "Start Time should be in HH:MM or HH:MM:SS format")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_3 <- function(input) {
    errors <- character(0); warnings <- character(0)
    dur  <- input$wiz_duration            %||% NA
    ts   <- input$wiz_timestep            %||% NA
    at   <- input$wiz_activity_transition %||% NA
    if (!is.na(dur) && dur <= 0)           errors   <- c(errors,   "Duration must be greater than 0 hours")
    if (!is.na(dur) && dur > 8760)         warnings <- c(warnings, "Duration exceeds 1 year (8760 hours)")
    if (!is.na(ts)  && ts <= 0)            errors   <- c(errors,   "Time Step must be greater than 0 minutes")
    if (!is.na(ts)  && ts > 60)            warnings <- c(warnings, "Time Step greater than 60 minutes may be too coarse")
    if (!is.na(dur) && !is.na(ts) && ts >= dur * 60)
      errors <- c(errors, "Time Step must be smaller than total simulation duration")
    if (!is.na(at) && !is.na(ts) && at >= ts)
      warnings <- c(warnings, "Activity Transition time should be smaller than the Time Step")
    if (!is.na(at) && at <= 0)             errors   <- c(errors,   "Activity Transition must be greater than 0 minutes")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_4 <- function(input) {
    errors <- character(0); warnings <- character(0)
    fa  <- input$wiz_floor_area   %||% NA
    rh  <- input$wiz_room_height  %||% NA
    ar  <- input$wiz_aspect_ratio %||% NA
    ori <- input$wiz_orientation  %||% NA
    if (!is.na(fa)  && fa <= 0)               errors <- c(errors, "Floor Surface Area must be greater than 0 m²")
    if (!is.na(rh)  && rh <= 0)               errors <- c(errors, "Room Height must be greater than 0 m")
    if (!is.na(ar)  && ar <= 0)               errors <- c(errors, "Aspect Ratio must be greater than 0")
    if (!is.na(ori) && (ori < 0 || ori > 360)) errors <- c(errors, "Orientation must be between 0 and 360°")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_5 <- function(input) {
    errors <- character(0); warnings <- character(0)
    atv  <- input$wiz_area_to_volume    %||% NA
    ia   <- input$wiz_infiltration_area %||% NA
    ir   <- input$wiz_indoor_reflectance %||% NA
    np   <- input$wiz_neutral_pressure   %||% NA
    mh   <- input$wiz_midpoint_height    %||% NA
    dc   <- input$wiz_discharge_coeff    %||% NA
    oe   <- input$wiz_opening_effectiveness %||% NA
    sc   <- input$wiz_stack_coeff        %||% NA
    rh   <- input$wiz_room_height        %||% NA
    if (!is.na(atv) && atv <= 0)          errors   <- c(errors,   "Area-to-Volume Ratio must be greater than 0")
    if (!is.na(ia)  && ia < 0)            errors   <- c(errors,   "Infiltration Surface Area cannot be negative")
    if (!is.na(ia)  && ia == 0)           warnings <- c(warnings, "Infiltration Surface Area is 0 — no infiltration will occur")
    if (!is.na(ir)  && (ir < 0 || ir > 1)) errors  <- c(errors,   "Indoor Light Reflectance must be between 0 and 1")
    if (!is.na(np)  && np <= 0)           errors   <- c(errors,   "Neutral Pressure Level must be greater than 0 m")
    if (!is.na(np) && !is.na(rh) && !is.na(rh) && rh > 0 && np > rh)
      errors <- c(errors, "Neutral Pressure Level cannot exceed Room Height")
    if (!is.na(mh)  && mh <= 0)           errors   <- c(errors,   "Window Midpoint Height must be greater than 0 m")
    if (!is.na(mh) && !is.na(rh) && !is.na(rh) && rh > 0 && mh >= rh)
      errors <- c(errors, "Window Midpoint Height must be less than Room Height")
    if (!is.na(dc)  && dc <= 0)           errors   <- c(errors,   "Discharge Coefficient must be greater than 0")
    if (!is.na(dc)  && dc > 1)            errors   <- c(errors,   "Discharge Coefficient cannot exceed 1")
    if (!is.na(oe)  && (oe < 0 || oe > 1)) errors  <- c(errors,   "Opening Effectiveness must be between 0 and 1")
    if (!is.na(sc)  && sc <= 0)           errors   <- c(errors,   "Stack Coefficient must be greater than 0")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_6 <- function(input) {
    errors <- character(0); warnings <- character(0)
    sa  <- input$wiz_surface_albedo      %||% NA
    cb  <- input$wiz_cloud_base          %||% NA
    ct  <- input$wiz_cloud_top           %||% NA
    cod <- input$wiz_cloud_optical_depth %||% NA
    wc  <- input$wiz_wind_coeff          %||% NA
    if (!is.na(sa)  && (sa < 0 || sa > 1))    errors   <- c(errors,   "Surface Albedo must be between 0 and 1")
    if (!is.na(cb)  && cb < 0)                errors   <- c(errors,   "Cloud Base cannot be negative")
    if (!is.na(ct)  && ct < 0)                errors   <- c(errors,   "Cloud Top cannot be negative")
    if (!is.na(cb) && !is.na(ct) && ct <= cb) errors   <- c(errors,   "Cloud Top must be greater than Cloud Base")
    if (!is.na(cod) && cod < 0)               errors   <- c(errors,   "Cloud Optical Depth cannot be negative")
    if (!is.na(wc)  && wc <= 0)               errors   <- c(errors,   "Wind Coefficient must be greater than 0")
    list(errors=errors, warnings=warnings)
  }

  validate_screen_9 <- function(input) {
    errors <- character(0); warnings <- character(0)
    mode <- input$wiz_phys_env_mode %||% "Use default file"
    if (mode == "Manual entry") {
      for (nm in c("wiz_temp_indoor_init","wiz_temp_indoor_final")) {
        v <- input[[nm]] %||% NA
        if (!is.na(v) && (v < 200 || v > 350)) warnings <- c(warnings, paste(nm, "is outside typical range (200–350 K)"))
      }
      for (nm in c("wiz_temp_outdoor_init","wiz_temp_outdoor_final")) {
        v <- input[[nm]] %||% NA
        if (!is.na(v) && (v < 200 || v > 350)) warnings <- c(warnings, paste(nm, "is outside typical range (200–350 K)"))
      }
      for (nm in c("wiz_rh_init","wiz_rh_final")) {
        v <- input[[nm]] %||% NA
        if (!is.na(v) && (v < 0 || v > 1)) errors <- c(errors, paste("Relative Humidity", nm, "must be between 0 and 1"))
      }
      for (nm in c("wiz_bp_init","wiz_bp_final")) {
        v <- input[[nm]] %||% NA
        if (!is.na(v) && (v < 80000 || v > 110000)) warnings <- c(warnings, paste("Barometric Pressure", nm, "is outside typical range (80000–110000 Pa)"))
      }
      for (nm in c("wiz_wind_init","wiz_wind_final")) {
        v <- input[[nm]] %||% NA
        if (!is.na(v) && v < 0)  errors   <- c(errors,   paste("Wind speed", nm, "cannot be negative"))
        if (!is.na(v) && v > 50) warnings <- c(warnings, paste("Wind speed", nm, "exceeds 50 m/s"))
      }
    }
    list(errors=errors, warnings=warnings)
  }

  validate_screen_12 <- function(input) {
    errors <- character(0); warnings <- character(0)
    if (!nzchar(trimws(input$wiz_output_table %||% "")))
      errors <- c(errors, "Output Table file path cannot be empty")
    if (!nzchar(trimws(input$wiz_output_chart %||% "")))
      errors <- c(errors, "Basic Chart file path cannot be empty")
    if (isTRUE(input$wiz_output_derivatives) && !nzchar(trimws(input$wiz_output_derivatives_file %||% "")))
      errors <- c(errors, "Derivatives file path cannot be empty when 'Generate Time Derivatives' is checked")
    if (isTRUE(input$wiz_output_massbalance) && !nzchar(trimws(input$wiz_output_massbalance_file %||% "")))
      errors <- c(errors, "Mass Balance file path cannot be empty when 'Generate Mass Balance' is checked")
    if (isTRUE(input$wiz_output_sensitivity) && !nzchar(trimws(input$wiz_output_sensitivity_file %||% "")))
      errors <- c(errors, "Sensitivity file path cannot be empty when 'Generate Sensitivity Analysis' is checked")
    if (isTRUE(input$wiz_output_uncertainty) && !nzchar(trimws(input$wiz_output_uncertainty_file %||% "")))
      errors <- c(errors, "Uncertainty file path cannot be empty when 'Generate Uncertainty Estimate' is checked")
    list(errors=errors, warnings=warnings)
  }

  # Aggregate all screens
  validate_all_screens <- function(input) {
    screen_fns <- list(
      "Screen 1 (Location)"              = validate_screen_1,
      "Screen 2 (Start Time)"            = validate_screen_2,
      "Screen 3 (Simulation Time)"       = validate_screen_3,
      "Screen 4 (The Box)"               = validate_screen_4,
      "Screen 5 (Building)"              = validate_screen_5,
      "Screen 6 (Outside)"               = validate_screen_6,
      "Screen 9 (Physical Environment)"  = validate_screen_9,
      "Screen 12 (Outputs)"              = validate_screen_12
    )
    all_errors <- character(0); all_warnings <- character(0)
    for (nm in names(screen_fns)) {
      res <- screen_fns[[nm]](input)
      if (length(res$errors)   > 0) all_errors   <- c(all_errors,   paste0("[", nm, "] ", res$errors))
      if (length(res$warnings) > 0) all_warnings <- c(all_warnings, paste0("[", nm, "] ", res$warnings))
    }
    list(errors=all_errors, warnings=all_warnings)
  }

  # ── Reactive banner renderers (update live as user types) ───────────────
  output$wiz_val_banner_1  <- renderUI({ render_banner(validate_screen_1(input))  })
  output$wiz_val_banner_2  <- renderUI({ render_banner(validate_screen_2(input))  })
  output$wiz_val_banner_3  <- renderUI({ render_banner(validate_screen_3(input))  })
  output$wiz_val_banner_4  <- renderUI({ render_banner(validate_screen_4(input))  })
  output$wiz_val_banner_5  <- renderUI({ render_banner(validate_screen_5(input))  })
  output$wiz_val_banner_6  <- renderUI({ render_banner(validate_screen_6(input))  })
  output$wiz_val_banner_9  <- renderUI({ render_banner(validate_screen_9(input))  })
  output$wiz_val_banner_12 <- renderUI({ render_banner(validate_screen_12(input)) })

  # -----------------------------------------------------------------------
  # Navigation
  # -----------------------------------------------------------------------
  observeEvent(input$wizard_next, {
    # Validate the screen the user is leaving before advancing
    screen_validators <- list(
      `1`  = validate_screen_1,
      `2`  = validate_screen_2,
      `3`  = validate_screen_3,
      `4`  = validate_screen_4,
      `5`  = validate_screen_5,
      `6`  = validate_screen_6,
      `9`  = validate_screen_9,
      `12` = validate_screen_12
    )
    cur <- wizard_state$current_screen
    vfn <- screen_validators[[as.character(cur)]]
    if (!is.null(vfn)) {
      res <- vfn(input)
      if (length(res$errors) > 0) {
        showModal(modalDialog(
          title = HTML("<span style='color:#7b0000;'>&#9888; Cannot Proceed — Errors on This Screen</span>"),
          HTML(paste0(
            "<p>Please fix the following errors before continuing:</p>",
            "<ul>", paste0("<li style='color:#7b0000;'>", res$errors, "</li>", collapse=""), "</ul>"
          )),
          easyClose = TRUE, footer = modalButton("OK"),
          size = "m"
        ))
        return()
      }
    }
    if (wizard_state$current_screen < wizard_state$max_screen) {
      wizard_state$current_screen <- wizard_state$current_screen + 1
      updateNumericInput(session,"wizard_current_screen_input",value=wizard_state$current_screen)
    }
  })
  observeEvent(input$wizard_back, {
    if (wizard_state$current_screen > 1) {
      wizard_state$current_screen <- wizard_state$current_screen - 1
      updateNumericInput(session,"wizard_current_screen_input",value=wizard_state$current_screen)
    }
  })
  observeEvent(input$wizard_cancel, {
    modal_toggle("wizard_modal","hide")
    # Signal main_app to roll back the pending (uncommitted) queue row
    shinyjs::runjs("Shiny.setInputValue('wizard_cancelled', Math.random(), {priority: 'event'})")
  })

  # Every time the user opens the wizard for a NEW simulation (clicks Add ▸
  # Standard), reset wizard_state to defaults and jump back to Screen 1.
  # This prevents fields from a prior simulation pre-populating the form
  # of the next one. Edit mode uses wizard_edit_trigger instead and
  # explicitly restores from snapshot, so this does not affect editing.
  observeEvent(input$wizard_open_trigger, {
    reset_wizard_to_defaults()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Edit mode: restore prior wizard state and open at Screen 1
  # -----------------------------------------------------------------------
  observeEvent(input$wizard_edit_trigger, {
    sim_no <- if (exists("wizard_edit_sim_no", envir = .GlobalEnv))
      get("wizard_edit_sim_no", envir = .GlobalEnv) else NULL

    if (!is.null(sim_no)) {
      snaps <- if (exists("sim_wizard_snapshots", envir = .GlobalEnv))
        get("sim_wizard_snapshots", envir = .GlobalEnv) else list()
      snap <- snaps[[as.character(sim_no)]]
      if (!is.null(snap)) {
        restore_wizard_from_snapshot(session, wizard_state, snap)
        cat("Wizard edit: restored snapshot for sim", sim_no, "\n")
      } else {
        cat("Wizard edit: no snapshot found for sim", sim_no, "\xe2\x80 opening with current defaults\n")
      }
    }

    wizard_state$current_screen <- 1
    updateNumericInput(session, "wizard_current_screen_input", value = 1)
    modal_toggle("wizard_modal", "show")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # -----------------------------------------------------------------------
  # Finish
  # -----------------------------------------------------------------------
  observeEvent(input$wizard_finish, {
    # ── Full validation pass across all screens ──────────────────────────
    all_res <- validate_all_screens(input)
    if (length(all_res$errors) > 0) {
      showModal(modalDialog(
        title = HTML("<span style='color:#7b0000;'>&#9888; Cannot Save — Validation Errors Found</span>"),
        HTML(paste0(
          "<p>The following errors must be fixed before the configuration can be saved:</p>",
          "<ul>", paste0("<li style='color:#7b0000;margin-bottom:4px;'>", all_res$errors, "</li>", collapse=""), "</ul>",
          if (length(all_res$warnings) > 0) paste0(
            "<hr><p style='color:#7d4e00;'><b>Warnings (non-blocking):</b></p>",
            "<ul>", paste0("<li style='color:#7d4e00;'>", all_res$warnings, "</li>", collapse=""), "</ul>") else ""
        )),
        easyClose = TRUE, footer = modalButton("Go Back and Fix"),
        size = "l"
      ))
      return()
    }
    # ── Proceed with saving ──────────────────────────────────────────────
    # Detect edit mode BEFORE the save so we can route the index correctly
    is_edit_mode <- exists("wizard_edit_mode", envir = .GlobalEnv) &&
                    isTRUE(get("wizard_edit_mode", envir = .GlobalEnv))

    # Get the per-instance input dir from instance_dirs (registered by main_app
    # when open_sim_menu fires). Fall back to a temp dir if not yet set.
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    pending_idx <- if (is_edit_mode) {
      as.integer(get("wizard_edit_sim_no", envir = .GlobalEnv))
    } else {
      tryCatch(
        tail(as.integer(get("instances", envir = .GlobalEnv)), 1), error = function(e) 1L)
    }
    output_dir <- instance_dirs[[as.character(pending_idx)]]$input %||%
      paste0("Input_tmp_", pending_idx, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    result     <- assemble_and_write_wizard_data(input, wizard_state, output_dir)

    current_sim_num <- tryCatch(
      max(get("instances",envir=.GlobalEnv)), error=function(e) 1)

    warn_block <- if (length(all_res$warnings) > 0 ||
                       length(result$assembly_warnings) > 0) {
      combined_warn <- c(
        if (length(all_res$warnings) > 0) paste0("[Input] ", all_res$warnings) else character(0),
        result$assembly_warnings %||% character(0)
      )
      paste0("<hr><p><b style='color:#7d4e00;'>&#9432; Warnings:</b></p><ul>",
             paste0("<li style='color:#7d4e00;'>", combined_warn, "</li>", collapse=""),
             "</ul>")
    } else ""

    assembly_err_block <- if (length(result$assembly_errors %||% character(0)) > 0)
      paste0("<hr><p><b style='color:#7b0000;'>&#9888; Assembly Errors:</b></p><ul>",
             paste0("<li style='color:#7b0000;'>", result$assembly_errors, "</li>", collapse=""),
             "</ul>") else ""

    showModal(modalDialog(
      title="✅ Configuration Complete",
      HTML(paste0(
        "<p style='color:green;font-weight:bold;'>Wizard configuration saved successfully!</p>",
        "<p><strong>Simulation:</strong> ", current_sim_num, "</p>",
        "<p><strong>Directory:</strong> ", output_dir, "</p>",
        "<p><strong>Files created:</strong> ", length(result$files_created), "</p>",
        "<ul>",
        paste0("<li>",basename(unlist(result$files_created)),"</li>",collapse=""),
        "</ul>",
        assembly_err_block,
        warn_block)),
      easyClose=TRUE, footer=modalButton("OK")))

    modal_toggle("wizard_modal","hide")

    # ── Save state snapshot so this sim can be re-edited later ───────────────────
    snap_sim_no <- if (is_edit_mode) {
      get("wizard_edit_sim_no", envir = .GlobalEnv)
    } else {
      tryCatch(max(get("instances", envir = .GlobalEnv)), error = function(e) 1L)
    }
    snap <- list(
      latitude=input$wiz_latitude, longitude=input$wiz_longitude,
      altitude=input$wiz_altitude, gravity=input$wiz_gravity,
      start_year=input$wiz_start_year, start_month=input$wiz_start_month,
      start_day=input$wiz_start_day, start_time=input$wiz_start_time,
      timezone=input$wiz_timezone, relative_start=input$wiz_relative_start,
      duration=input$wiz_duration, timestep=input$wiz_timestep,
      activity_transition=input$wiz_activity_transition,
      floor_area=input$wiz_floor_area, room_height=input$wiz_room_height,
      aspect_ratio=input$wiz_aspect_ratio, orientation=input$wiz_orientation,
      area_to_volume=input$wiz_area_to_volume,
      infiltration_area=input$wiz_infiltration_area,
      indoor_reflectance=input$wiz_indoor_reflectance,
      num_stories=input$wiz_num_stories,
      neutral_pressure=input$wiz_neutral_pressure,
      midpoint_height=input$wiz_midpoint_height,
      discharge_coeff=input$wiz_discharge_coeff,
      opening_effectiveness=input$wiz_opening_effectiveness,
      stack_coeff=input$wiz_stack_coeff, num_windows=input$wiz_num_windows,
      surface_albedo=input$wiz_surface_albedo,
      cloud_base=input$wiz_cloud_base, cloud_top=input$wiz_cloud_top,
      cloud_optical_depth=input$wiz_cloud_optical_depth,
      shelter_class=input$wiz_shelter_class, wind_coeff=input$wiz_wind_coeff,
      lights_mode=input$wiz_lights_mode, num_lights=input$wiz_num_lights,
      phys_env_mode=input$wiz_phys_env_mode,
      temp_indoor_init=input$wiz_temp_indoor_init,
      temp_indoor_final=input$wiz_temp_indoor_final,
      temp_outdoor_init=input$wiz_temp_outdoor_init,
      temp_outdoor_final=input$wiz_temp_outdoor_final,
      rh_init=input$wiz_rh_init, rh_final=input$wiz_rh_final,
      bp_init=input$wiz_bp_init, bp_final=input$wiz_bp_final,
      wind_init=input$wiz_wind_init, wind_final=input$wiz_wind_final,
      act_mode=input$wiz_act_mode,
      outdoor_conc_mode=input$wiz_outdoor_conc_mode,
      output_table=input$wiz_output_table, output_chart=input$wiz_output_chart,
      output_derivatives=input$wiz_output_derivatives,
      output_derivatives_file=input$wiz_output_derivatives_file,
      output_massbalance=input$wiz_output_massbalance,
      output_massbalance_file=input$wiz_output_massbalance_file,
      output_sensitivity=input$wiz_output_sensitivity,
      output_sensitivity_file=input$wiz_output_sensitivity_file,
      output_uncertainty=input$wiz_output_uncertainty,
      output_uncertainty_file=input$wiz_output_uncertainty_file,
      mechanism=input$wiz_mechanism,
      # Complex reactive state
      light_schedule     = isolate(wizard_state$light_schedule),
      phys_schedule      = isolate(wizard_state$phys_schedule),
      activity_schedules = isolate(wizard_state$activity_schedules),
      windows_data       = isolate(wizard_state$windows_data)
    )
    existing_snaps <- if (exists("sim_wizard_snapshots", envir = .GlobalEnv))
      get("sim_wizard_snapshots", envir = .GlobalEnv) else list()
    existing_snaps[[as.character(snap_sim_no)]] <- snap
    assign("sim_wizard_snapshots", existing_snaps, envir = .GlobalEnv)
    cat("Wizard: saved state snapshot for sim", snap_sim_no, "\n")

    # Store metadata so main_app / edit handler can update the queue row
    assign("pending_sim_metadata", list(
      lat       = input$wiz_latitude  %||% NA,
      lon       = input$wiz_longitude %||% NA,
      duration  = input$wiz_duration  %||% NA,
      mechanism = input$wiz_mechanism %||% "SAPRC99"
    ), envir = .GlobalEnv)

    if (is_edit_mode) {
      # Edit mode: overwrite the existing queue row — no summary modal needed
      shinyjs::runjs("Shiny.setInputValue('wizard_edit_confirmed', Math.random(), {priority: 'event'})")
    } else {
      # New simulation: open the Simulation Summary modal to collect run name
      shinyjs::runjs("Shiny.setInputValue('wizard_confirmed_internal', Math.random(), {priority: 'event'})")
    }
  })
}