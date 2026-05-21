# advanced_module.R
# =======================================================================
# Advanced simulation module - WITH MANUAL EDIT VALIDATION
# =======================================================================

# -------------------------
# Advanced UI
# -------------------------
advanced_module_ui <- function() {
  tagList(
    # Advanced Inputs Modal
    modal_ui(
      id    = "advanced_inputs_menu",
      title = "Advanced Simulation - Input Files",
      size  = "xl",
      fluidRow(
        column(
          4,
          div(
            style = "max-height: 90vh; overflow-y: auto;",
            wellPanel(
              actionButton(
                "preload_all_files", "Preload All Default Input Files",
                style = "background-color: #17a2b8; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              actionButton(
                "clear_optional_files", "\u2715 Clear Optional Auto-Generated Files",
                style = "background-color: #c0392b; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              actionButton(
                "validate_all_files", "Validate All Input Files",
                style = "background-color: #28a745; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              actionButton(
                "show_file_structure", "Show File Structure (Diagnostic)",
                style = "background-color: #6c757d; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              helpText("Use 'Preload All' to load defaults, or 'Clear Optional' to let SIACS auto-calculate lights and initial values."),
              # ── Auto-generated files notice ──────────────────────────────
              div(
                style = paste0("background:#d1ecf1;color:#0c5460;border:1px solid #bee5eb;",
                               "border-radius:5px;padding:10px 12px;margin-bottom:10px;font-size:12px;"),
                tags$b("\u2139\ufe0f Auto-generated files \u2014 safe to leave blank:"),
                tags$ul(style = "margin:6px 0 0 0; padding-left:18px;",
                  tags$li(tags$b("Initial Values"),
                          " \u2014 SIACS starts from chemical equilibrium if omitted."),
                  tags$li(tags$b("Artificial Light"),
                          " \u2014 calculated from Light List + Spectra + Schedule if omitted."),
                  tags$li(tags$b("Outdoor Light Direct / Diffuse"),
                          " \u2014 calculated from location, date and time if omitted."),
                  tags$li(tags$b("Indoor Light (IndoorLight.xlsx)"),
                          " \u2014 calculated from all of the above and saved for reuse if omitted.")
                )
              ),
              div(
                id = "validation_summary_link",
                style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
                HTML("<strong>View validation results in the 'Validation Summary' tab →</strong>")
              ),
              
              # Group 1: Simulation Timing — must come first so duration is
              # available when all other files are validated
              h4("Simulation Timing"),
              render_input_block(8),
              
              # Group 2: Environment Setup
              h4("Environment Setup"),
              lapply(c(1, 3, 6, 7), function(i) render_input_block(i)),
              
              # Group 3: Source & Activity Setup
              h4("Source & Activity Setup"),
              lapply(c(4, 5, 2), function(i) render_input_block(i)),
              
              # Group 4: Light & Exposure
              h4("Light & Exposure"),
              lapply(c(9, 10, 11, 12, 13, 14, 15, 16, 17), function(i) render_input_block(i)),
              
              tags$hr(),
              actionButton("continue_to_outputs", "Next: Output Settings →")
            )
          )
        ),
        column(
          8,
          div(
            style = "max-height: 90vh; overflow-y: auto;",
            tabsetPanel(
              id = "preview_tabs",
              tabPanel("Data Preview", uiOutput("sheet_tabs"), value = "data_preview"),
              tabPanel("Validation Summary", div(style = "padding: 15px;", htmlOutput("validation_summary")), value = "validation_summary")
            )
          )
        )
      )
    ),
    
    # Advanced Outputs Modal
    modal_ui(
      id    = "advanced_outputs_menu",
      title = "Advanced Simulation - Output Settings",
      size  = "l",
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Output Settings"),
            helpText("ℹ️ Edit output file paths below, or leave the defaults. Click ‘Save & Add Simulation’ when ready — no separate save step needed."),
            output_ui("output_module_advanced"),
            tags$hr(),
            div(style="padding:12px;background:#eaf4fb;border-radius:5px;border-left:4px solid #2E86C1;margin-bottom:12px;",
              radioButtons("adv_mechanism", "Chemical Mechanism",
                choiceNames  = list("SAPRC99 (default)"),
                choiceValues = c("SAPRC99"),
                selected = "SAPRC99", inline = TRUE),
              # SAPRC07T placeholder — see wizard_module.R for the same pattern.
              div(style="margin-left:20px;color:#999;font-size:13px;display:inline-block;",
                  "SAPRC07T")
            ),
            actionButton("save_advanced", "💾 Save & Add Simulation", style = "background-color:#27ae60;color:white;font-weight:bold;width:100%;")
          )
        )
      )
    )
  )
}

# -------------------------
# Advanced Server
# -------------------------
# `set_duration` is optional; when supplied it lets the Advanced module push
# the simulation Duration parsed from a loaded Time data file back to the
# main app's shared reactive. This keeps validators in sync with the user's
# actual configuration instead of comparing every other file against the
# stale 72h default.
advanced_module_server <- function(input, output, session, get_duration,
                                   set_duration = NULL) {
  
  # ---- Reactive storage & helpers ----
  validation_results   <- reactiveValues()
  data_list_advanced   <- reactiveValues()
  # Tracks the ORIGINAL filename that populated each data source slot, so the
  # input snapshot preserves the user's actual filenames (e.g. a Manassas
  # diffuse-light CSV loaded from "Input/DiffuseLight-Manassas.csv" lands in
  # the snapshot with that same filename, not the hardcoded Atlanta default).
  #
  # Keys mirror data_list_advanced:
  #   "name{i}"         — uploaded via fileInput
  #   "name_online{i}"  — fetched from URL / path
  #   "name_preload{i}" — read from file_paths_advanced[i]
  #   "name_create{i}"  — created in-GUI (no source file)
  source_names_advanced <- reactiveValues()
  counter_advanced     <- reactiveVal(1)
  current_file_index   <- reactiveVal(NULL)  # Track which file is being previewed

  # ── Parse Duration from a Time data frame and push to the shared
  #    simulation_duration reactive. Called from every Time-data load path
  #    (upload / import / preload / create). Silent on missing column or
  #    non-numeric values — we only update when the value is sane.
  push_duration_from_time_df <- function(df) {
    if (is.null(set_duration)) return(invisible(NULL))
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
    if (!"Duration" %in% names(df)) return(invisible(NULL))
    val <- suppressWarnings(as.numeric(df$Duration[1]))
    if (length(val) == 1L && !is.na(val) && val > 0) {
      cat(sprintf("[advanced_module] Time data loaded — pushing duration = %g h\n", val))
      set_duration(val)
    }
    invisible(NULL)
  }

  # ── effective_duration(): returns the simulation duration in hours if a
  # Time data file has been loaded; otherwise NULL. Validators interpret
  # NULL as "no authoritative duration available, skip duration consistency
  # warning". This prevents misleading "data ends at X but simulation
  # duration is 4320 minutes" warnings firing against the 72h fallback when
  # the user has not actually configured a duration yet.
  time_loaded <- function() {
    !is.null(data_list_advanced[[paste0("data", 8)]]) ||
    !is.null(data_list_advanced[[paste0("data_online", 8)]]) ||
    !is.null(data_list_advanced[[paste0("data_preload", 8)]]) ||
    !is.null(data_list_advanced[[paste0("data_create", 8)]])
  }
  effective_duration <- function() {
    if (time_loaded()) get_duration() else NULL
  }
  
  # helper to read CSV/XLSX, first sheet if multiple
  read_data <- function(file_path, show_error_modal = TRUE) {
    tryCatch({
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        df <- read.csv(file_path, comment.char = "#", na.strings = "None")
        if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
        # Trim leading/trailing whitespace from column names (e.g. "Smoking " -> "Smoking")
        names(df) <- trimws(names(df))
        return(df)
        
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        sheets <- excel_sheets(file_path)
        if (length(sheets) == 1) {
          df <- tryCatch(read_excel(file_path), error = function(e) read_excel(file_path, comment = "#"))
          if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
          return(list(df))
        } else {
          df_list <- lapply(sheets, function(sheet) read_excel(file_path, sheet = sheet))
          names(df_list) <- sheets
          return(df_list)
        }
      } else {
        return(NULL)
      }
    }, error = function(e) {
      if (show_error_modal) {
        showModal(modalDialog(
          title = "File Reading Error",
          paste("Error reading file:", e$message),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
      return(NULL)
    })
  }
  
  # Render tabs for preview (single or multi-sheet) - WITH EDIT CAPTURE
  # FIX: each file uses its own unique table ID ("table_advanced_{i}") instead
  # of the shared "table_advanced" ID.  The old shared ID caused every file's
  # observeEvent(input$table_advanced) to fire whenever ANY table changed,
  # which overwrote data_list_advanced[["data{i}"]] for ALL i with whatever
  # was last rendered — so uploading one file silently corrupted the stored
  # data for every other file, producing wrong validation results.
  render_sheet_tabs <- function(df_list, i, read_only = FALSE) {
    current_file_index(i)  # Track which file is being edited
    
    # Per-file table output ID (unique per i)
    table_id <- paste0("table_advanced_", i)
    
    output$sheet_tabs <- renderUI({
      if (is.list(df_list) && !is.data.frame(df_list)) {
        do.call(tabsetPanel, c(
          id = paste0("tabs_", i),
          lapply(names(df_list), function(sheet) {
            tabPanel(title = sheet, rHandsontableOutput(outputId = paste0("table_", i, "_", sheet)))
          })
        ))
      } else {
        tagList(rHandsontableOutput(table_id))
      }
    })
    
    if (is.list(df_list) && !is.data.frame(df_list)) {
      lapply(names(df_list), function(sheet) {
        local({
          sheet_name <- sheet
          output[[paste0("table_", i, "_", sheet_name)]] <- renderRHandsontable({
            rhandsontable(df_list[[sheet_name]], readOnly = read_only)
          })
        })
      })
    } else {
      # Use the per-file output ID so renders don't collide across files.
      # FIX: make the render reactive so the table re-draws after we normalize
      # the edited data below. Read the current df from data_list_advanced so
      # inserted step-transition rows become visible in the UI.
      local({
        tid <- table_id
        fi  <- i
        output[[tid]] <- renderRHandsontable({
          df_current <- NULL
          if (!is.null(data_list_advanced[[paste0("data", fi)]])) {
            df_current <- data_list_advanced[[paste0("data", fi)]]
          } else if (!is.null(data_list_advanced[[paste0("data_online", fi)]])) {
            df_current <- data_list_advanced[[paste0("data_online", fi)]]
          } else if (!is.null(data_list_advanced[[paste0("data_preload", fi)]])) {
            df_current <- data_list_advanced[[paste0("data_preload", fi)]]
          } else if (!is.null(data_list_advanced[[paste0("data_create", fi)]])) {
            df_current <- data_list_advanced[[paste0("data_create", fi)]]
          }
          if (is.null(df_current) || (is.list(df_current) && !is.data.frame(df_current))) {
            df_current <- df_list  # fall back to the closure copy
          }
          rhandsontable(df_current, readOnly = read_only)
        })
      })

      # CAPTURE MANUAL EDITS - Update the data when user changes values.
      # Observe the per-file input ID so only this file's edits are captured.
      if (!read_only) {
        local({
          tid <- table_id
          fi  <- i
          observeEvent(input[[tid]], {
            req(input[[tid]])
            edited_df <- hot_to_r(input[[tid]])

            # Normalize pre-transition rows for schedules that use step-change
            # semantics. Only insert a pre-row (at t - 0.1 min) in FRONT of
            # rows the user actually edited in this firing — pre-existing
            # value changes in the preloaded file are left untouched.
            #   5  = Activities
            #   17 = Artificial Light Schedule
            if (fi %in% c(5L, 17L) && is.data.frame(edited_df) &&
                "Time" %in% names(edited_df) &&
                exists("insert_pretransitions_for_edits", mode = "function")) {
              # Baseline for diff: whatever is currently stored for this file
              stored_df <- NULL
              for (prefix in c("data", "data_online", "data_preload", "data_create")) {
                k <- paste0(prefix, fi)
                if (!is.null(data_list_advanced[[k]])) {
                  stored_df <- data_list_advanced[[k]]
                  break
                }
              }
              if (is.data.frame(stored_df)) {
                edited_df <- tryCatch(
                  insert_pretransitions_for_edits(edited_df, stored_df, trans = 0.1),
                  error = function(e) { warning(e); edited_df }
                )
              }
            }

            # Determine which data source to update based on what was loaded.
            # Guard against write-when-unchanged: if the current stored value
            # is already identical to the normalized edit, skip the assignment
            # so the reactive render doesn't loop.
            assign_if_changed <- function(slot_key, new_df) {
              cur <- data_list_advanced[[slot_key]]
              if (!is.null(cur) && is.data.frame(cur) &&
                  identical(cur, new_df)) {
                return(invisible(FALSE))
              }
              data_list_advanced[[slot_key]] <- new_df
              TRUE
            }

            changed <- FALSE
            if (!is.null(data_list_advanced[[paste0("data", fi)]])) {
              changed <- assign_if_changed(paste0("data", fi), edited_df)
            } else if (!is.null(data_list_advanced[[paste0("data_online", fi)]])) {
              changed <- assign_if_changed(paste0("data_online", fi), edited_df)
            } else if (!is.null(data_list_advanced[[paste0("data_preload", fi)]])) {
              changed <- assign_if_changed(paste0("data_preload", fi), edited_df)
            } else if (!is.null(data_list_advanced[[paste0("data_create", fi)]])) {
              changed <- assign_if_changed(paste0("data_create", fi), edited_df)
            }

            # Clear validation result only when data actually changed
            if (isTRUE(changed)) {
              validation_results[[paste0("result_", fi)]] <- NULL
              # If the user edited the Time table, push the new Duration to
              # the shared simulation_duration so other validations stay
              # consistent.
              if (fi == 8L) push_duration_from_time_df(edited_df)
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  }
  
  # ---- Per-file observers: uploads/imports/preloads/validation/preview ----
  lapply(seq_along(titles_advanced), function(i) {
    
    # Upload
    observeEvent(input[[paste0("file", i)]], {
      req(input[[paste0("file", i)]])
      df <- read_data(input[[paste0("file", i)]]$datapath)
      if (!is.null(df)) {
        data_list_advanced[[paste0("data", i)]] <- df
        # Remember the user-supplied filename for the snapshot writer
        source_names_advanced[[paste0("name", i)]] <-
          input[[paste0("file", i)]]$name
        # If this is the Time data file (index 8), parse Duration and push
        # to the shared simulation_duration so subsequent validations of
        # other files compare against the user's actual configuration.
        if (i == 8) {
          push_duration_from_time_df(
            if (is.list(df) && !is.data.frame(df)) df[[1]] else df)
        }
        # Auto-validate for types with specific validators
        if (i %in% c(1:8, 12)) {
          df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
          result <- validate_data_file(df_to_validate, file_type_map[i], effective_duration())
          validation_results[[paste0("result_", i)]] <- result
        }
      }
    })
    
    # Import (URL or local path)
    observeEvent(input[[paste0("import_table", i)]], {
      req(input[[paste0("url", i)]])
      df <- read_data(input[[paste0("url", i)]])
      if (!is.null(df)) {
        data_list_advanced[[paste0("data_online", i)]] <- df
        source_names_advanced[[paste0("name_online", i)]] <-
          basename(input[[paste0("url", i)]])
        if (i == 8) {
          push_duration_from_time_df(
            if (is.list(df) && !is.data.frame(df)) df[[1]] else df)
        }
      }
    })
    
    # Preload from defaults
    observeEvent(input[[paste0("preload_table", i)]], {
      if (file_paths_advanced[i] != "none") {
        df <- read_data(file_paths_advanced[i])
        if (!is.null(df)) {
          data_list_advanced[[paste0("data_preload", i)]] <- df
          source_names_advanced[[paste0("name_preload", i)]] <-
            basename(file_paths_advanced[i])
          if (i == 8) {
            push_duration_from_time_df(
              if (is.list(df) && !is.data.frame(df)) df[[1]] else df)
          }
        }
      } else {
        data_list_advanced[[paste0("data_preload", i)]] <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
        source_names_advanced[[paste0("name_preload", i)]] <- NULL
      }
    })
    
    # Validate (uploaded)
    observeEvent(input[[paste0("validate", i)]], {
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) {
        showModal(modalDialog(
          title = "Validation Error",
          "No data loaded for validation. Please upload or preload data first.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return()
      }
      df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
      result <- validate_data_file(df_to_validate, file_type_map[i], effective_duration())
      validation_results[[paste0("result_", i)]] <- result
      
      if (result$valid && length(result$warnings) == 0) {
        showModal(modalDialog(
          title = "Validation Passed",
          HTML(paste0("<p style='color: green; font-weight: bold;'>", titles_advanced[i], 
                      " passed all validation checks.</p>")),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      } else {
        showModal(modalDialog(
          title = paste("Validation Results:", titles_advanced[i]),
          HTML(format_validation_messages(result)),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
    })
    
    # Validate (imported)
    observeEvent(input[[paste0("validate_import", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) {
        showModal(modalDialog(
          title = "Validation Error", 
          "No data loaded for validation.", 
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return()
      }
      df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
      result <- validate_data_file(df_to_validate, file_type_map[i], effective_duration())
      validation_results[[paste0("result_", i)]] <- result
      
      if (result$valid && length(result$warnings) == 0) {
        showModal(modalDialog(
          title = "Validation Passed",
          HTML(paste0("<p style='color: green; font-weight: bold;'>", titles_advanced[i], 
                      " passed all validation checks.</p>")),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      } else {
        showModal(modalDialog(
          title = paste("Validation Results:", titles_advanced[i]),
          HTML(format_validation_messages(result)),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
    })
    
    # --- Preview buttons ---
    observeEvent(input[[paste0("show_table", i)]], {
      df <- data_list_advanced[[paste0("data", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("import_table", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("preload_table", i)]], {
      df <- data_list_advanced[[paste0("data_preload", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("create_table", i)]], {
      df <- data_list_advanced[[paste0("data_create", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("preview_preload", i)]], {
      df <- data_list_advanced[[paste0("data_preload", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    observeEvent(input[[paste0("preview_import", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    observeEvent(input[[paste0("preview_create", i)]], {
      df <- data_list_advanced[[paste0("data_create", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    
    # ---- Validation status indicator (used by render_input_block) ----
    output[[paste0("validation_status_", i)]] <- renderUI({
      result <- validation_results[[paste0("result_", i)]]
      if (is.null(result)) {
        return(tags$div(style = "color: gray; font-size: 12px;", "Not validated"))
      }
      if (result$valid && length(result$warnings) == 0) {
        return(tags$div(style = "color: green; font-size: 12px; font-weight: bold;", "✓ Valid"))
      } else if (result$valid && length(result$warnings) > 0) {
        return(tags$div(style = "color: orange; font-size: 12px; font-weight: bold;", "⚠ Valid with warnings"))
      } else {
        return(tags$div(style = "color: red; font-size: 12px; font-weight: bold;", "✗ Errors found"))
      }
    })

    # ---- Unload button — visible only when any data is loaded for this slot ----
    output[[paste0("unload_btn_", i)]] <- renderUI({
      has_data <- !is.null(data_list_advanced[[paste0("data",         i)]]) ||
                  !is.null(data_list_advanced[[paste0("data_preload", i)]]) ||
                  !is.null(data_list_advanced[[paste0("data_online",  i)]]) ||
                  !is.null(data_list_advanced[[paste0("data_create",  i)]])
      if (!has_data) return(NULL)
      actionButton(
        paste0("unload_file", i),
        label = HTML("&#x2715; Clear loaded data"),
        style = paste0(
          "background-color:#c0392b;color:white;font-size:11px;",
          "padding:2px 8px;margin-top:4px;margin-bottom:2px;",
          "border:none;border-radius:3px;width:100%;"
        )
      )
    })

    # ---- Observe the unload click ----
    local({
      idx <- i
      observeEvent(input[[paste0("unload_file", idx)]], {
        data_list_advanced[[paste0("data",         idx)]] <- NULL
        data_list_advanced[[paste0("data_preload", idx)]] <- NULL
        data_list_advanced[[paste0("data_online",  idx)]] <- NULL
        data_list_advanced[[paste0("data_create",  idx)]] <- NULL
        validation_results[[paste0("result_",      idx)]] <- NULL
        # Clear the source-filename tracker for this slot too
        source_names_advanced[[paste0("name",         idx)]] <- NULL
        source_names_advanced[[paste0("name_preload", idx)]] <- NULL
        source_names_advanced[[paste0("name_online",  idx)]] <- NULL
        source_names_advanced[[paste0("name_create",  idx)]] <- NULL
        cat("Unloaded data for slot", idx, ":", titles_advanced[idx], "\n")
        showNotification(
          paste0("\u2715 \"", titles_advanced[idx], "\" data cleared — SIACS will auto-calculate if optional."),
          type = "warning", duration = 4
        )
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    })
  })
  
  # ---- Clear optional auto-generated files (indices 1, 9, 10, 11, 14) ----
  # These are: Initial Values, Indoor Light, Outdoor Light Direct,
  #            Outdoor Light Diffuse, Artificial Light
  observeEvent(input$clear_optional_files, {
    optional_indices <- c(1, 9, 10, 11, 14)
    cleared <- character(0)
    for (idx in optional_indices) {
      had_data <- !is.null(data_list_advanced[[paste0("data",         idx)]]) ||
                  !is.null(data_list_advanced[[paste0("data_preload", idx)]]) ||
                  !is.null(data_list_advanced[[paste0("data_online",  idx)]]) ||
                  !is.null(data_list_advanced[[paste0("data_create",  idx)]])
      data_list_advanced[[paste0("data",         idx)]] <- NULL
      data_list_advanced[[paste0("data_preload", idx)]] <- NULL
      data_list_advanced[[paste0("data_online",  idx)]] <- NULL
      data_list_advanced[[paste0("data_create",  idx)]] <- NULL
      validation_results[[paste0("result_",      idx)]] <- NULL
      # Clear source-filename tracker for this slot
      source_names_advanced[[paste0("name",         idx)]] <- NULL
      source_names_advanced[[paste0("name_preload", idx)]] <- NULL
      source_names_advanced[[paste0("name_online",  idx)]] <- NULL
      source_names_advanced[[paste0("name_create",  idx)]] <- NULL
      if (had_data) cleared <- c(cleared, titles_advanced[idx])
    }
    if (length(cleared) > 0) {
      cat("Cleared optional files:", paste(cleared, collapse = ", "), "\n")
      showNotification(
        paste0("\u2715 Cleared: ", paste(cleared, collapse = ", "),
               ". SIACS will auto-calculate these."),
        type = "warning", duration = 5
      )
    } else {
      showNotification(
        "No optional files were loaded \u2014 nothing to clear.",
        type = "message", duration = 3
      )
    }
  })

  # ---- Batch validate ----
  observeEvent(input$validate_all_files, {
    validated_count <- 0
    error_count     <- 0
    warning_count   <- 0
    
    for (i in seq_along(titles_advanced)) {
      # Check all possible data sources in priority order
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_preload", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_create", i)]]
      
      if (!is.null(df)) {
        df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
        result <- validate_data_file(df_to_validate, file_type_map[i], effective_duration())
        validation_results[[paste0("result_", i)]] <- result
        validated_count <- validated_count + 1
        if (!result$valid) error_count   <- error_count + 1
        if (length(result$warnings) > 0) warning_count <- warning_count + 1
      }
    }
    
    updateTabsetPanel(session, "preview_tabs", selected = "validation_summary")
    showModal(modalDialog(
      title = "Batch Validation Complete",
      HTML(paste0(
        "<p>Validated ", validated_count, " files.</p>",
        "<p style='color: red;'>Files with errors: ", error_count, "</p>",
        "<p style='color: orange;'>Files with warnings: ", warning_count, "</p>",
        "<p><strong>The Validation Summary tab (on the right) now shows detailed results.</strong></p>",
        "<p style='font-size: 12px; color: #666;'>Note: If you manually edited values in the preview tables, ",
        "those changes have been captured and validated.</p>"
      )),
      easyClose = TRUE, footer = modalButton("OK")
    ))
  })
  
  # ---- File structure diagnostic ----
  observeEvent(input$show_file_structure, {
    file_structure <- ""
    for (i in seq_along(titles_advanced)) {
      # Check all possible data sources
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_preload", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_create", i)]]
      
      if (!is.null(df)) {
        df_to_check <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
        file_structure <- paste0(
          file_structure,
          "<div style='margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;'>",
          "<strong>", titles_advanced[i], "</strong><br>",
          "<em>Columns (", ncol(df_to_check), "):</em> ",
          paste(names(df_to_check), collapse = ", "),
          "<br><em>Rows:</em> ", nrow(df_to_check),
          "</div>"
        )
      }
    }
    showModal(modalDialog(
      title = "File Structure Diagnostic",
      HTML(paste0("<div style='max-height: 60vh; overflow-y: auto;'>", file_structure, "</div>")),
      size = "l",
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  # ---- Validation summary panel ----
  output$validation_summary <- renderUI({
    results_list <- reactiveValuesToList(validation_results)
    if (length(results_list) == 0) {
      return(HTML("<p>No files have been validated yet.</p><p>Click 'Validate All Input Files' or validate individual files.</p>"))
    }
    
    html_output <- "<h4>Validation Summary</h4>"
    for (i in seq_along(titles_advanced)) {
      result <- validation_results[[paste0("result_", i)]]
      if (!is.null(result)) {
        status_class <- if (result$valid && length(result$warnings) == 0) {
          "validation-pass"
        } else if (result$valid && length(result$warnings) > 0) {
          "validation-warning"
        } else { 
          "validation-error" 
        }
        
        status_text <- if (result$valid && length(result$warnings) == 0) {
          "VALID"
        } else if (result$valid && length(result$warnings) > 0) {
          "VALID (with warnings)"
        } else { 
          "ERRORS DETECTED" 
        }
        
        html_output <- paste0(
          html_output, 
          "<div style='margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;'>",
          "<strong><span class='", status_class, "'>[", status_text, "]</span> ", 
          titles_advanced[i], "</strong><br>"
        )
        
        if (length(result$errors) > 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #dc3545; margin-top: 5px;'>",
            "<strong>Errors:</strong><ul style='margin: 5px 0;'>",
            paste0("<li>", result$errors, "</li>", collapse = ""),
            "</ul></div>"
          )
        }
        
        if (length(result$warnings) > 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #ffc107; margin-top: 5px;'>",
            "<strong>Warnings:</strong><ul style='margin: 5px 0;'>",
            paste0("<li>", result$warnings, "</li>", collapse = ""),
            "</ul></div>"
          )
        }
        
        if (result$valid && length(result$warnings) == 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #28a745; margin-top: 5px;'>All validation checks passed.</div>"
          )
        }
        
        html_output <- paste0(html_output, "</div>")
      }
    }
    HTML(html_output)
  })
  
  # ---- Navigate to outputs (with validation check) ----
  observeEvent(input$continue_to_outputs, {
    critical_files <- c(8, 6, 3, 7)  # Time, BoxData, PhysicalEnvironment, OutdoorConcentrations
    missing_validations <- c()
    for (i in critical_files) {
      if (is.null(validation_results[[paste0("result_", i)]])) {
        missing_validations <- c(missing_validations, titles_advanced[i])
      }
    }
    if (length(missing_validations) > 0) {
      showModal(modalDialog(
        title = "Validation Warning",
        HTML(paste0(
          "<p>The following critical input files have not been validated:</p><ul>",
          paste0("<li>", missing_validations, "</li>", collapse = ""),
          "</ul><p>It is recommended to validate all inputs before proceeding.</p>",
          "<p><strong>Note:</strong> If you manually edited values in preview tables, ",
          "click 'Validate All Input Files' to validate your changes.</p>",
          "<p>Do you want to continue anyway?</p>"
        )),
        footer = tagList(
          actionButton("continue_anyway", "Continue Anyway"),
          modalButton("Go Back")
        ),
        easyClose = FALSE
      ))
    } else {
      modal_toggle("advanced_inputs_menu", "hide")
      modal_toggle("advanced_outputs_menu", "show")
    }
  })
  
  observeEvent(input$continue_anyway, {
    removeModal()
    modal_toggle("advanced_inputs_menu", "hide")
    modal_toggle("advanced_outputs_menu", "show")
  })
  
  # ---- Save Advanced configuration - WITH COMPREHENSIVE FINAL VALIDATION ----
  observeEvent(input$save_advanced, {
    
    # =========================================================================
    # STEP 1: COLLECT ALL DATA THAT WILL BE SAVED (including manual edits)
    # =========================================================================
    data_to_save <- list()
    
    for (i in seq_along(titles_advanced)) {
      # Check ALL data sources in priority order:
      # Priority reflects what user intends: uploaded > online > preloaded > created
      if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_online", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_preload", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_create", i)]]
      } else {
        data_to_save[i] <- list(NULL)  # FIX: preserves list length (see perform_save for full explanation)
      }
    }
    
    # =========================================================================
    # STEP 2: VALIDATE THE FINAL DATA BEFORE SAVING (including manual edits)
    # =========================================================================
    cat("\n=== VALIDATING FINAL DATA BEFORE SAVE (including manual edits) ===\n")
    
    final_validation_results <- list()
    # Files 1 (InitialValues), 9 (IndoorLight), 10 (OutdoorLightDirect),
    # 11 (OutdoorLightDiffuse), 14 (ArtificialLight) are intentionally excluded
    # from this list: they are auto-generated by SIACS when absent and must not
    # block saving even if left blank.
    critical_files <- c(8, 6, 3, 7, 2, 4, 5, 12)  # All files that have validators (excluding auto-generated)
    files_with_errors <- c()
    files_with_warnings <- c()
    
    for (i in critical_files) {
      if (!is.null(data_to_save[[i]])) {
        # Extract dataframe for validation
        df_to_validate <- if (is.list(data_to_save[[i]]) && !is.data.frame(data_to_save[[i]])) {
          data_to_save[[i]][[1]]
        } else {
          data_to_save[[i]]
        }
        
        # Run validation
        result <- validate_data_file(df_to_validate, file_type_map[i], effective_duration())
        final_validation_results[[i]] <- result
        
        # Update the validation_results reactive for display
        validation_results[[paste0("result_", i)]] <- result
        
        # Track results
        if (!result$valid) {
          files_with_errors <- c(files_with_errors, titles_advanced[i])
          cat(sprintf("❌ %s: FAILED validation\n", titles_advanced[i]))
          cat("Errors:\n")
          for (err in result$errors) {
            cat(sprintf("  - %s\n", err))
          }
        } else if (length(result$warnings) > 0) {
          files_with_warnings <- c(files_with_warnings, titles_advanced[i])
          cat(sprintf("⚠️  %s: Valid with warnings\n", titles_advanced[i]))
          cat("Warnings:\n")
          for (warn in result$warnings) {
            cat(sprintf("  - %s\n", warn))
          }
        } else {
          cat(sprintf("✅ %s: Valid\n", titles_advanced[i]))
        }
      } else {
        cat(sprintf("⚪ %s: No data provided\n", titles_advanced[i]))
      }
    }
    
    # =========================================================================
    # STEP 3: BLOCK SAVE IF CRITICAL ERRORS FOUND
    # =========================================================================
    if (length(files_with_errors) > 0) {
      # Switch to validation summary tab
      updateTabsetPanel(session, "preview_tabs", selected = "validation_summary")
      
      # Build detailed error message
      error_details <- ""
      for (file_name in files_with_errors) {
        idx <- which(titles_advanced == file_name)
        if (length(idx) > 0) {
          result <- final_validation_results[[idx]]
          if (!is.null(result) && length(result$errors) > 0) {
            error_details <- paste0(
              error_details,
              "<div style='margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 5px;'>",
              "<strong>", file_name, ":</strong><ul style='margin: 5px 0;'>",
              paste0("<li>", result$errors, "</li>", collapse = ""),
              "</ul></div>"
            )
          }
        }
      }
      
      showModal(modalDialog(
        title = "❌ Validation Errors Detected - Save Blocked",
        HTML(paste0(
          "<p style='color: red; font-weight: bold;'>The following files have validation errors and must be corrected:</p>",
          error_details,
          "<hr>",
          "<p><strong>Actions required:</strong></p>",
          "<ol>",
          "<li>Review the errors in the 'Validation Summary' tab (now open)</li>",
          "<li>If you manually entered wrong values, click 'Preview' to edit the table again</li>",
          "<li>Correct the errors in your data</li>",
          "<li>Click 'Validate All Input Files' to verify your corrections</li>",
          "<li>Try saving again</li>",
          "</ol>",
          if (length(files_with_warnings) > 0) {
            paste0(
              "<hr>",
              "<p style='color: orange;'><strong>Note:</strong> The following files also have warnings (non-blocking):</p>",
              "<ul>", paste0("<li>", files_with_warnings, "</li>", collapse = ""), "</ul>"
            )
          } else {
            ""
          }
        )),
        size = "l",
        easyClose = TRUE, 
        footer = modalButton("OK")
      ))
      return()
    }
    
    # =========================================================================
    # STEP 4: SHOW WARNINGS IF PRESENT (but allow save)
    # =========================================================================
    if (length(files_with_warnings) > 0) {
      # Build warning details
      warning_details <- ""
      for (file_name in files_with_warnings) {
        idx <- which(titles_advanced == file_name)
        if (length(idx) > 0) {
          result <- final_validation_results[[idx]]
          if (!is.null(result) && length(result$warnings) > 0) {
            warning_details <- paste0(
              warning_details,
              "<div style='margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 5px;'>",
              "<strong>", file_name, ":</strong><ul style='margin: 5px 0;'>",
              paste0("<li>", result$warnings, "</li>", collapse = ""),
              "</ul></div>"
            )
          }
        }
      }
      
      showModal(modalDialog(
        title = "⚠️ Validation Warnings - Review Before Saving",
        HTML(paste0(
          "<p style='color: orange; font-weight: bold;'>The following files have warnings:</p>",
          warning_details,
          "<hr>",
          "<p>Warnings do not block saving, but you may want to review them.</p>",
          "<p><strong>Do you want to proceed with saving?</strong></p>"
        )),
        size = "l",
        footer = tagList(
          actionButton("save_with_warnings", "Save Anyway", 
                       style = "background-color: #ffc107; color: black;"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
      
      # Wait for user decision
      return()
    }
    
    # If no errors and no warnings, proceed directly to save
    perform_save(data_to_save)
  })
  
  # Handle save with warnings
  observeEvent(input$save_with_warnings, {
    removeModal()
    
    # Recollect data to save
    data_to_save <- list()
    for (i in seq_along(titles_advanced)) {
      if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_online", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_preload", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_create", i)]]
      } else {
        data_to_save[i] <- list(NULL)  # FIX: preserves list length (see perform_save for full explanation)
      }
    }
    
    perform_save(data_to_save)
  })
  
  # =========================================================================
  # PERFORM_SAVE FUNCTION - Extracted for reuse (MATCHES ORIGINAL LOGIC)
  # =========================================================================
  perform_save <- function(data_to_save) {
    cat("\n✅ All validation passed or warnings accepted. Proceeding with save...\n")

    # Detect edit mode: overwrite existing slot rather than appending
    is_edit_mode <- exists("advanced_edit_mode", envir = .GlobalEnv) &&
                    isTRUE(get("advanced_edit_mode", envir = .GlobalEnv))

    # Indices of files that SIACS auto-generates when absent.
    # We must NOT write a non-NULL entry into input_data_list for these
    # when the user has left them blank — otherwise SIACS skips calculation.
    # Index mapping (from titles_advanced / file_type_map):
    #   1  = InitialValues
    #   9  = IndoorLight
    #   10 = OutdoorLightDirect
    #   11 = OutdoorLightDiffuse
    #   14 = ArtificialLight
    optional_auto_generated <- c(1L, 9L, 10L, 11L, 14L)

    # upload_only_indices: slots where ONLY an explicit user upload (data{i})
    # is accepted into input_data_list. Preloaded defaults and online imports
    # are intentionally ignored for these indices.
    #
    # Index 1 = InitialValues (InitialIndoorConcentrations.csv):
    #   The wizard intentionally omits this file so SIACS starts from chemical
    #   equilibrium. The advanced module must match that behaviour when the user
    #   has not explicitly uploaded their own InitialValues file.
    #   Without this guard, clicking "Preload All Default Files" in Advanced
    #   seeds the file into input_data_list$InitialValues, forcing a
    #   non-equilibrium start that diverges from the wizard output (e.g. RCHO
    #   indoor concentration is suppressed relative to the equilibrium-start run).
    upload_only_indices <- c(1L)

    # Save each file to global environment using priority order:
    # uploaded > online > preloaded > created  (same as data_to_save collection above)
    # Use assignment by index (current_save_idx), not append, so repeated saves
    # for the same instance overwrite rather than accumulate extra entries.
    if (!exists("input_data_list", envir = .GlobalEnv))
      assign("input_data_list", list(), envir = .GlobalEnv)

    # FIX: derive the slot index from the global `instances` vector (same source
    # used by the wizard module and by compact_for_run) rather than from the
    # independent counter_advanced() reactive.
    #
    # counter_advanced() starts at 1 and increments once per advanced save,
    # completely ignoring how many wizard (or other) sims are already in the
    # queue.  When a wizard sim occupies slot 1 and the advanced sim should
    # occupy slot 2, counter_advanced() still returns 1 — writing advanced data
    # into slot 1 (silently overwriting the wizard sim's inputs) while the
    # metadata read-back at current_idx_ol = tail(instances,1) = 2 finds NULL
    # (blank Location / Duration in the queue table).  On run, the advanced sim
    # at slot 2 has no inputs and fails silently.
    #
    # Using tail(instances, 1) mirrors what the wizard module does via
    # current_idx <- tail(as.integer(instances), 1) in assemble_wizard_data,
    # guaranteeing both write and read always target the same slot.
    current_save_idx <- if (is_edit_mode) {
      as.integer(get("advanced_edit_sim_no", envir = .GlobalEnv))
    } else if (exists("instances", envir = .GlobalEnv)) {
      tail(as.integer(get("instances", envir = .GlobalEnv)), 1)
    } else {
      1L   # fallback: only reachable if instances was never initialised
    }

    lapply(seq_along(titles_advanced), function(i) {

      file_key <- gsub(" ", "", titles_advanced[i])
      # Correct any key names that differ between the UI title and what the
      # engine reads from input_data_list (engine uses abbreviated keys).
      key_corrections <- c("DepositionVelocity" = "DepositionV")
      if (file_key %in% names(key_corrections)) file_key <- key_corrections[[file_key]]

      # upload_only: only an explicit user upload (data{i}) counts.
      # Preloads and online imports are silently skipped — leave key NULL.
      if (i %in% upload_only_indices) {
        df_uploaded <- data_list_advanced[[paste0("data", i)]]
        if (is.null(df_uploaded) ||
            (is.data.frame(df_uploaded) && nrow(df_uploaded) == 0)) {
          cat(sprintf("  \u24d8 %s: no explicit upload \u2014 SIACS will use equilibrium start.\n",
                      titles_advanced[i]))
          return(invisible(NULL))
        }
        input_data_list <- get("input_data_list", envir = .GlobalEnv)
        if (is.null(input_data_list[[file_key]])) input_data_list[[file_key]] <- list()
        input_data_list[[file_key]][[current_save_idx]] <- df_uploaded
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
        return(invisible(NULL))
      }

      # Determine the single winning data source (priority order)
      df_to_write <- if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        data_list_advanced[[paste0("data", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        data_list_advanced[[paste0("data_online", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        data_list_advanced[[paste0("data_preload", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        data_list_advanced[[paste0("data_create", i)]]
      } else {
        NULL
      }

      if (is.null(df_to_write)) return(invisible(NULL))

      # Optional auto-generated files: if the data frame is empty the user
      # deliberately left it blank — keep the key NULL so SIACS calculates it.
      if (i %in% optional_auto_generated) {
        if (is.data.frame(df_to_write) && nrow(df_to_write) == 0) {
          cat(sprintf("  \u24d8 %s left blank \u2014 SIACS will auto-generate.\n", titles_advanced[i]))
          return(invisible(NULL))
        }
      }

      # Write at the correct index (overwrite if re-saving the same instance)
      input_data_list <- get("input_data_list", envir = .GlobalEnv)
      if (is.null(input_data_list[[file_key]])) input_data_list[[file_key]] <- list()
      input_data_list[[file_key]][[current_save_idx]] <- df_to_write
      assign("input_data_list", input_data_list, envir = .GlobalEnv)
    })

    # Do NOT force InitialValues[[1]] = "None" here. The engine already handles
    # NULL InitialValues correctly (equilibrium start). The old unconditional
    # write to slot [[1]] corrupted multi-instance runs where current_save_idx > 1.
    
    # Only advance the counter for new simulations, not edits
    if (!is_edit_mode) counter_advanced(counter_advanced() + 1)
    
    # =========================================================================
    # SAVE TIMESTAMPED INPUT FOLDER (mirrors wizard behaviour)
    # =========================================================================
    # All instances share a single Input_<timestamp> folder (created on the
    # first call and reused thereafter).  Each file is suffixed with the
    # current instance index so it never overwrites a sibling instance's file,
    # e.g.  PhysicalEnvironmentData_CMAQ_1.csv, ..._2.csv, etc.

    # Determine current instance index from the global instances vector
    # FIX: reuse current_save_idx computed above — both the input_data_list
    # write (lapply block) and the OutputList / metadata read-back below must
    # target the same slot.  The old code re-derived this independently using
    # tail(instances, 1), which was correct in isolation but meant any future
    # divergence between the two derivations would reintroduce the bug.
    current_idx <- current_save_idx

    # Reuse or create the shared input folder
    # Get per-instance input dir from instance_dirs (set by main_app after summary modal)
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    output_dir <- instance_dirs[[as.character(current_idx)]]$input %||%
      paste0("Input_tmp_", current_idx, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Helper: path inside the per-instance output_dir (no suffix needed)
    inp_path <- function(filename) {
      file.path(output_dir, filename)
    }

    files_written <- character(0)
    files_skipped <- character(0)
    folder_ok <- tryCatch({
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      TRUE
    }, error = function(e) {
      cat("WARNING: Could not create input snapshot folder:", conditionMessage(e), "\n")
      FALSE
    })

    if (folder_ok) {
      cat("Saving input snapshot to:", output_dir, "(instance", current_idx, ")\n")

      for (i in seq_along(titles_advanced)) {
        df <- data_to_save[[i]]
        if (is.null(df)) next

        # ---- Determine output filename --------------------------------------
        # Priority matches data_to_save collection (upload > online > preload
        # > create); the winning source's original filename is used so the
        # snapshot preserves whatever the user actually loaded rather than
        # the hardcoded default path.
        tracked_name <- NULL
        if (!is.null(data_list_advanced[[paste0("data", i)]])) {
          tracked_name <- source_names_advanced[[paste0("name", i)]]
        } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
          tracked_name <- source_names_advanced[[paste0("name_online", i)]]
        } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
          tracked_name <- source_names_advanced[[paste0("name_preload", i)]]
        } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
          tracked_name <- source_names_advanced[[paste0("name_create", i)]]
        }

        base_name <- if (!is.null(tracked_name) && nzchar(tracked_name)) {
          tracked_name
        } else {
          # Fallback to the hardcoded default path's filename, or synthesize
          # one from the UI title when the file type has no default.
          orig_path <- file_paths_advanced[i]
          if (!is.null(orig_path) && orig_path != "none" && nzchar(orig_path)) {
            basename(orig_path)
          } else {
            paste0(gsub("[^A-Za-z0-9._-]", "_", titles_advanced[i]), ".csv")
          }
        }
        file_name <- base_name   # no suffix — each instance has its own folder
        out_path <- file.path(output_dir, file_name)

        # ---- Write CSV or XLSX -----------------------------------------------
        tryCatch({
          if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
            # Multi-sheet workbook: df is a named list of data frames
            if (is.list(df) && !is.data.frame(df)) {
              wb <- openxlsx::createWorkbook()
              for (sheet_name in names(df)) {
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet = sheet_name, x = df[[sheet_name]])
              }
              openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
            } else {
              # Single data frame stored for an xlsx slot — write as xlsx
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb, "Sheet1")
              openxlsx::writeData(wb, sheet = "Sheet1", x = df)
              openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
            }
          } else {
            # CSV (the common case): write without row names
            write.csv(df, out_path, row.names = FALSE)
          }
          files_written <- c(files_written, file_name)
          cat("  Written:", file_name, "\n")
        }, error = function(e) {
          files_skipped <- c(files_skipped, file_name)
          cat("  WARNING: Could not write", file_name, "—", conditionMessage(e), "\n")
        })
      }

      cat("Input snapshot complete:", length(files_written), "file(s) written to", output_dir, "\n")
    }

    # =========================================================================
    # SUCCESS MODAL
    # =========================================================================
    folder_line <- if (folder_ok) {
      paste0(
        "<hr>",
        "<p style='font-size: 12px; color: #155724; background-color: #d4edda;",
        " padding: 8px; border-radius: 4px;'>",
        "📁 Input snapshot saved to: <strong>", output_dir, "</strong>",
        " (", length(files_written), " file(s))",
        if (length(files_skipped) > 0)
          paste0("<br>⚠️ Could not write: ", paste(files_skipped, collapse = ", "))
        else "",
        "</p>"
      )
    } else {
      "<hr><p style='color: orange; font-size: 12px;'>⚠️ Input snapshot folder could not be created.</p>"
    }

    showModal(modalDialog(
      title = "✅ Configuration Saved Successfully",
      HTML(paste0(
        "<p style='color: green; font-weight: bold;'>Advanced simulation configuration saved successfully!</p>",
        "<p>All input files (including any manual edits) passed final validation.</p>",
        "<p><strong>Ready to run simulation.</strong></p>",
        "<hr>",
        "<p style='font-size: 12px; color: #666;'>Configuration has been saved to global environment as 'input_data_list'.</p>",
        folder_line
      )),
      easyClose = TRUE, footer = modalButton("OK")
    ))
    modal_toggle("advanced_outputs_menu", "hide")

    # ── Populate OutputList from the live output table (no separate save needed) ──
    current_idx_ol <- current_idx   # already resolved above (edit-aware)
    if (!exists("OutputList", envir = .GlobalEnv))
      assign("OutputList", list(), envir = .GlobalEnv)
    ol <- get("OutputList", envir = .GlobalEnv)
    if (length(ol) < current_idx_ol) length(ol) <- current_idx_ol
    live_output <- tryCatch(output_mod$get_output_values(), error = function(e) NULL)
    has_live <- !is.null(live_output) &&
                !is.null(live_output$OutputTable) &&
                nzchar(trimws(live_output$OutputTable %||% ""))
    ol[[current_idx_ol]] <- if (has_live) live_output else wizard_output_defaults()
    assign("OutputList", ol, envir = .GlobalEnv)

    # ── Derive metadata from uploaded input files ──────────────────────────────
    adv_duration <- tryCatch({
      idl <- get("input_data_list", envir = .GlobalEnv)
      time_df <- idl$Time[[current_idx_ol]]
      if (!is.null(time_df) && "Duration" %in% names(time_df))
        as.numeric(time_df$Duration[1])
      else NA
    }, error = function(e) NA)

    # Read lat/lon from BoxData (always present in advanced uploads)
    adv_lat <- tryCatch({
      bd <- get("input_data_list", envir = .GlobalEnv)$BoxData[[current_idx_ol]]
      if (!is.null(bd) && "Latitude"  %in% names(bd)) as.numeric(bd$Latitude[1])  else NA
    }, error = function(e) NA)
    adv_lon <- tryCatch({
      bd <- get("input_data_list", envir = .GlobalEnv)$BoxData[[current_idx_ol]]
      if (!is.null(bd) && "Longitude" %in% names(bd)) as.numeric(bd$Longitude[1]) else NA
    }, error = function(e) NA)

    assign("pending_sim_metadata", list(
      lat       = adv_lat,
      lon       = adv_lon,
      duration  = adv_duration,
      mechanism = input$adv_mechanism %||% "SAPRC99"
    ), envir = .GlobalEnv)

    if (is_edit_mode) {
      # Edit mode: update the existing queue row — no summary modal needed
      shinyjs::runjs("Shiny.setInputValue('advanced_edit_confirmed', Math.random(), {priority: 'event'})")
    } else {
      # New simulation: open Simulation Summary modal to collect run name
      shinyjs::runjs("Shiny.setInputValue('advanced_confirmed_internal', Math.random(), {priority: 'event'})")
    }
  }
  
  # ---- Preload all defaults ----
  observeEvent(input$preload_all_files, {
    loaded_count <- 0
    failed_files <- character(0)
    
    for (i in seq_along(file_paths_advanced)) {
      if (file_paths_advanced[i] != "none") {
        df <- read_data(file_paths_advanced[i], show_error_modal = FALSE)
        if (!is.null(df)) {
          data_list_advanced[[paste0("data_preload", i)]] <- df
          source_names_advanced[[paste0("name_preload", i)]] <-
            basename(file_paths_advanced[i])
          loaded_count <- loaded_count + 1
        } else {
          failed_files <- c(failed_files, titles_advanced[i])
        }
      } else {
        data_list_advanced[[paste0("data_preload", i)]] <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
        source_names_advanced[[paste0("name_preload", i)]] <- NULL
        loaded_count <- loaded_count + 1
      }
    }
    
    if (length(failed_files) == 0) {
      showModal(modalDialog(
        title = "Preload Complete",
        HTML(paste0("<p style='color: green;'>Successfully preloaded all ", loaded_count, " default input files.</p>")),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    } else if (length(failed_files) == length(file_paths_advanced)) {
      showModal(modalDialog(
        title = "Preload Failed",
        HTML("<p style='color: red;'>Failed to load any files. Please check that the Input directory exists and contains the required files.</p>"),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    } else {
      showModal(modalDialog(
        title = "Preload Partially Complete",
        HTML(paste0(
          "<p style='color: orange;'>Loaded ", loaded_count, " out of ", length(file_paths_advanced), " files.</p>",
          "<p style='color: red;'><strong>Failed to load:</strong></p><ul>",
          paste0("<li>", failed_files, "</li>", collapse = ""),
          "</ul><p>Please check that these files exist in the Input directory.</p>"
        )),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    }
  })
  
  # ---- Wire the shared output server module ----
  output_mod <- output_server("output_module_advanced")

  # ---- Edit mode: re-open the advanced inputs modal with existing data ----
  observeEvent(input$advanced_edit_trigger, {
    sim_no <- if (exists("advanced_edit_sim_no", envir = .GlobalEnv))
      as.integer(get("advanced_edit_sim_no", envir = .GlobalEnv)) else NULL

    if (!is.null(sim_no) && exists("input_data_list", envir = .GlobalEnv)) {
      idl <- get("input_data_list", envir = .GlobalEnv)
      key_corrections_inv <- c("DepositionV" = "DepositionVelocity")
      for (i in seq_along(titles_advanced)) {
        file_key <- gsub(" ", "", titles_advanced[i])
        key_corrections <- c("DepositionVelocity" = "DepositionV")
        if (file_key %in% names(key_corrections)) file_key <- key_corrections[[file_key]]
        df <- tryCatch(idl[[file_key]][[sim_no]], error = function(e) NULL)
        if (!is.null(df)) {
          # Load as a preload so it appears in the data preview and validation
          data_list_advanced[[paste0("data_preload", i)]] <- df
        }
      }
      cat("Advanced edit: pre-loaded input_data_list for sim", sim_no, "\n")
    }
    modal_toggle("advanced_inputs_menu", "show")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ---- Reset to a clean state for a new simulation ----
  # Wipes every data slot, validation result, source-name record, and
  # cached input. Called from the advanced_open_trigger observer below
  # so that opening "+ Add ▸ Advanced" for a second simulation does not
  # show files from the previous one.
  reset_advanced_to_blank <- function() {
    isolate({
      # data_list_advanced: clear all slot variants for every file index
      for (i in seq_along(titles_advanced)) {
        for (variant in c("data", "data_online", "data_preload", "data_create")) {
          key <- paste0(variant, i)
          if (!is.null(data_list_advanced[[key]]))
            data_list_advanced[[key]] <- NULL
        }
      }
      # source_names_advanced: same shape
      for (i in seq_along(titles_advanced)) {
        for (variant in c("name", "name_online", "name_preload", "name_create")) {
          key <- paste0(variant, i)
          if (!is.null(source_names_advanced[[key]]))
            source_names_advanced[[key]] <- NULL
        }
      }
      # validation_results: clear every per-file result
      for (i in seq_along(titles_advanced)) {
        key <- paste0("result_", i)
        if (!is.null(validation_results[[key]]))
          validation_results[[key]] <- NULL
      }
      current_file_index(NULL)
    })
    # Reset every "data source" radio control back to the default option
    # so the UI doesn't keep showing "Upload a file" / "Preload data" from
    # the previous run. The UI lists "Preload data" first as the default.
    for (i in seq_along(titles_advanced)) {
      updateSelectInput(session, paste0("data_source", i),
                        selected = "Preload data")
      # Also reset the file inputs / URL fields tied to this slot
      shinyjs::reset(paste0("file", i))
      updateTextInput(session, paste0("url", i), value = "")
    }
    cat("[advanced_module] reset to blank state for new simulation\n")
    invisible(NULL)
  }

  # Fired by main_app every time the user picks "+ Add ▸ Advanced". Edit
  # mode goes through advanced_edit_trigger instead and pre-loads from the
  # saved snapshot, so this only affects the New flow.
  observeEvent(input$advanced_open_trigger, {
    reset_advanced_to_blank()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
