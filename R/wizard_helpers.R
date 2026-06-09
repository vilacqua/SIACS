# wizard_helpers.R
# =======================================================================
# Helper functions for SIACS Wizard
# =======================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Find a file in the Input/ directory by partial name match (same logic as
# wizard_defaults find_path but usable at assembly time without file_paths_advanced)
find_input_file <- function(pattern) {
  candidates <- list.files("Input", full.names=TRUE, ignore.case=TRUE)
  hits <- candidates[grepl(pattern, basename(candidates), ignore.case=TRUE)]
  if (length(hits) > 0) hits[1] else NULL
}

normalize_output_path <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(NULL)
  if (identical(x,"None")) return("None")
  if (grepl("^\\./"  ,x)) return(x)
  if (grepl("^Output/",x)) return(paste0("./",x))
  return(x)
}

wizard_output_defaults <- function() {
  if (exists("get_default_output_values")) return(as.list(get_default_output_values()))
  list(
    OutputTable                 = "./Output/ATL-CMAQ for Ambient",
    OutputBasicChart            = "./Output/ATL-CMAQ for Ambient",
    OutputTimeDerivatives       = "./Output/ATL-CMAQ for Ambient Derivatives",
    OutputMassBalanceComponents = "./Output/ATL-CMAQ for Ambient Mass Balance",
    OutputSensitivity           = "./Output/ATL-CMAQ for Ambient Sensitivity",
    OutputUncertainty           = "./Output/ATL-CMAQ for Ambient uncertainty"
  )
}

# -----------------------------------------------------------------------
# INSERT STEP-CHANGE HELPERS
# Used by Screen 8 (light schedule), Screen 9 (phys env), Screen 10 (activities)
# -----------------------------------------------------------------------

# Wide-format step change: inserts a row at (t - trans) with the CURRENT
# value of `col`, and a row at t with `new_val`.  Works for both the
# light schedule (many light columns) and the physical environment
# schedule (many variable columns).
insert_step_change_wide <- function(df, col, t, new_val, trans = 0.1) {
  if (!(col %in% names(df))) {
    warning("Column '", col, "' not found in schedule. Skipping.")
    return(df)
  }
  df <- df[order(df$Time), ]

  # Value of col just BEFORE the event. Use strictly-less-than so that if a row
  # already sits exactly at t (e.g. from a prior event), we capture the value
  # that preceded it rather than that row's own value — otherwise stacking a
  # second event on the same time would treat the first event's value as the
  # pre-jump baseline.
  before_strict <- df[df$Time < t, ]
  prev_val    <- if (nrow(before_strict) > 0) tail(before_strict[[col]], 1) else 0

  # Full row to clone for new rows: prefer the row strictly before t; fall back
  # to the first row when the event is at/before the start of the schedule.
  last_before   <- if (nrow(before_strict) > 0) tail(before_strict, 1) else df[1, ]

  # Event row
  new_at        <- last_before
  new_at$Time   <- t
  new_at[[col]] <- new_val

  if (t > 0) {
    t_pre          <- t - max(trans, 0.001)
    new_pre        <- last_before
    new_pre$Time   <- t_pre
    new_pre[[col]] <- prev_val   # unchanged value right before the jump
    # BUG FIX: if rows already exist at exactly t_pre or t, drop them FIRST so
    # the freshly-built event rows win. Previously we appended the new rows and
    # then used !duplicated(Time), which keeps the FIRST occurrence — i.e. the
    # stale pre-existing row — silently discarding the new event value whenever
    # the event time coincided with an existing schedule point.
    df <- df[!(df$Time %in% c(t_pre, t)), , drop = FALSE]
    df <- rbind(df, new_pre, new_at)
    df <- df[order(df$Time), ]
    df <- df[!duplicated(df$Time), ]
  } else {
    # Update the existing t=0 row in-place so deduplication doesn't drop the new value
    df[df$Time == 0, col] <- new_val
  }
  rownames(df) <- NULL
  df
}

# Two-column (Time / Value) step change — for the activity schedule table.
insert_step_change_two_col <- function(df, t, new_val, trans = 0.1) {
  df   <- df[order(df$Time), ]
  before <- df[df$Time <= t, ]
  prev_val <- if (nrow(before) > 0) tail(before$Value, 1) else 0

  # Only insert a pre-event transition row if t > 0; subtracting transition
  # time from t=0 would produce a negative time, which is meaningless.
  if (t > 0) {
    t_pre <- t - max(trans, 0.001)
    # BUG FIX: drop existing rows at exactly t_pre / t first so the new event
    # rows win deduplication (otherwise !duplicated keeps the stale row and the
    # new value is silently discarded when the event lands on an existing time).
    df <- df[!(df$Time %in% c(t_pre, t)), , drop = FALSE]
    df <- rbind(df,
      data.frame(Time = t_pre, Value = prev_val, stringsAsFactors = FALSE),
      data.frame(Time = t,     Value = new_val,  stringsAsFactors = FALSE))
    df <- df[order(df$Time), ]
    df <- df[!duplicated(df$Time), ]
  } else {
    # Update the existing t=0 row in-place so deduplication doesn't drop the new value
    df[df$Time == 0, "Value"] <- new_val
  }
  rownames(df) <- NULL
  df
}

# -----------------------------------------------------------------------
# WINDOWED-EVENT HELPERS (event with an explicit END time)
# -----------------------------------------------------------------------
# A plain step change sets a value at time t that persists until the next
# existing time point or the end of the simulation. That causes events to
# "run too long" when the user only wants them active for a bounded window.
#
# These helpers add an event that turns ON at t_start (value = new_val) and
# automatically returns to the value that was in effect just BEFORE the
# event (the baseline) at t_end. Each edge gets its own pre-transition row
# so the change is a sharp step rather than a linear ramp.
#
# If t_end is NULL/NA/<= t_start, the behaviour falls back to a plain
# open-ended step change (backwards compatible with the old buttons).

# Wide format (light schedule, physical environment).
#
# For a bounded event [t_start, t_end] we must ensure the event value holds
# across the ENTIRE window, not just at the boundary rows. Any pre-existing
# schedule points that fall strictly inside the window would otherwise
# interrupt the event (e.g. a default hourly point at t=60 inside a 56–106
# window pulls the value back down). We handle this by overwriting the target
# column to new_val for every existing row inside the open interval
# (t_start, t_end). Other columns in those rows are left untouched.
insert_event_window_wide <- function(df, col, t_start, new_val, t_end = NULL,
                                      trans = 0.1) {
  # Capture the baseline value — the value in effect STRICTLY BEFORE t_start —
  # BEFORE we mutate df, so the "return to baseline" at t_end is correct even
  # if the window lies between existing schedule points. Using strictly-before
  # (Time < t_start, not <=) matters when a prior event already placed a row
  # exactly at t_start: we want the value that preceded that event, not the
  # event's own value. (Fixes: Time On then Time Off on the same window left
  # the first event's value behind at t_end instead of the true baseline.)
  df0 <- df[order(df$Time), ]
  before_rows <- df0[df0$Time < t_start, ]
  baseline <- if (nrow(before_rows) > 0 && col %in% names(df0))
    tail(before_rows[[col]], 1) else 0

  bounded <- !is.null(t_end) && !is.na(t_end) && t_end > t_start

  # Turn ON at t_start
  df <- insert_step_change_wide(df, col, t_start, new_val, trans)

  if (bounded) {
    # Overwrite the target column for any existing rows strictly inside the
    # window so intermediate schedule points don't interrupt the event.
    if (col %in% names(df)) {
      inside <- df$Time > t_start & df$Time < t_end
      if (any(inside)) df[inside, col] <- new_val
    }
    # Turn OFF (return to baseline) at t_end.
    df <- insert_step_change_wide(df, col, t_end, baseline, trans)
  }
  df
}

# Two-column (Time / Value) — activity schedule.
insert_event_window_two_col <- function(df, t_start, new_val, t_end = NULL,
                                        trans = 0.1) {
  df0 <- df[order(df$Time), ]
  before_rows <- df0[df0$Time < t_start, ]    # strictly before — see note above
  baseline <- if (nrow(before_rows) > 0) tail(before_rows$Value, 1) else 0

  bounded <- !is.null(t_end) && !is.na(t_end) && t_end > t_start

  df <- insert_step_change_two_col(df, t_start, new_val, trans)

  if (bounded) {
    # Hold the event value across the whole window: overwrite the value of any
    # existing rows strictly inside (t_start, t_end) so they don't interrupt.
    inside <- df$Time > t_start & df$Time < t_end
    if (any(inside)) df[inside, "Value"] <- new_val
    # Return to baseline at t_end.
    df <- insert_step_change_two_col(df, t_end, baseline, trans)
  }
  df
}

# -----------------------------------------------------------------------
# UNCERTAINTY-ROW PREPENDER
# -----------------------------------------------------------------------
# SIACS requires Physical Environment and Outdoor Concentration data to carry
# an "Uncertainty" row as the FIRST data row (right after the header). The
# engine's ExtractUncertainty() reads x[1, -1] as per-column uncertainty and
# x[-1, ] as the actual time series. Manually-entered wizard schedules are
# generated WITHOUT this row, so the engine silently drops the first time
# point (and the assembly check flags an error). This helper prepends a
# correctly-shaped uncertainty row when one is not already present.
#
#   df        : data frame with a leading Time column
#   defaults  : named list mapping column name -> default uncertainty value;
#               any column not listed uses `fallback`.
#   fallback  : uncertainty for unlisted columns (default 0 = no uncertainty)
#
# Returns df unchanged if it already starts with an "Uncertainty" row.
prepend_uncertainty_row <- function(df, defaults = list(), fallback = 0) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  time_col <- names(df)[1]
  # Already has an uncertainty row?
  if (identical(as.character(df[[time_col]][1]), "Uncertainty")) return(df)

  unc <- df[1, , drop = FALSE]          # clone structure
  unc[[time_col]] <- "Uncertainty"
  for (col in names(df)[-1]) {
    val <- if (!is.null(defaults[[col]])) defaults[[col]] else fallback
    unc[[col]] <- val
  }
  # Time column must be character to hold "Uncertainty"; coerce so rbind keeps
  # the label rather than turning it into NA.
  df[[time_col]]  <- as.character(df[[time_col]])
  unc[[time_col]] <- as.character(unc[[time_col]])
  out <- rbind(unc, df)
  rownames(out) <- NULL
  out
}

# Per-variable default uncertainties for the Physical Environment schedule,
# matching the absolute-uncertainty convention used in the default
# PhysicalEnvironmentData_*.csv files (Ti/To in K, RH fraction, etc.).
.siacs_phys_uncertainty_defaults <- list(
  Ti = 0.1, To = 0.1, OpenWindowArea = 0,
  QBal = 0.05, QUnbal = 0.05, QFilter = 0.05,
  RH = 0.01, BP = 5, Wind = 0.05, SunFactor = 0
)

# -----------------------------------------------------------------------
# LIVE-TABLE UNCERTAINTY ROW (display + edit split)
# -----------------------------------------------------------------------
# The wizard keeps Physical Environment / Outdoor Concentration schedules as
# purely numeric data frames internally (so sorting and event math work). To
# let the user SEE and EDIT the uncertainty values in the live rhandsontable,
# we splice an "Uncertainty" row on top for display, then split it back out on
# every edit. The numeric schedule and the one-row uncertainty data frame are
# stored separately in wizard_state.

# Build a one-row uncertainty data frame matching `schedule`'s columns.
make_default_uncertainty_row <- function(schedule, defaults = list(), fallback = 0) {
  time_col <- names(schedule)[1]
  row <- schedule[1, , drop = FALSE]
  row[[time_col]] <- "Uncertainty"
  for (col in names(schedule)[-1]) {
    row[[col]] <- if (!is.null(defaults[[col]])) defaults[[col]] else fallback
  }
  rownames(row) <- NULL
  row
}

# Combine an uncertainty row + numeric schedule into one character data frame
# for display in the live table. The uncertainty row is always first.
combine_with_uncertainty <- function(schedule, unc_row) {
  if (is.null(schedule)) return(NULL)
  if (is.null(unc_row))  return(schedule)
  time_col <- names(schedule)[1]
  common   <- intersect(names(unc_row), names(schedule))
  out <- rbind(
    setNames(as.data.frame(lapply(unc_row[common],  as.character), stringsAsFactors = FALSE), common),
    setNames(as.data.frame(lapply(schedule[common], as.character), stringsAsFactors = FALSE), common)
  )
  rownames(out) <- NULL
  out
}

# Split a combined (edited) table back into list(uncertainty=<1-row df>,
# schedule=<numeric df>). If no Uncertainty row is present, uncertainty is NULL.
split_uncertainty <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list(uncertainty = NULL, schedule = df))
  time_col <- names(df)[1]
  is_unc   <- as.character(df[[time_col]]) == "Uncertainty"
  unc_row  <- NULL
  if (any(is_unc)) {
    unc_row <- df[which(is_unc)[1], , drop = FALSE]
    df      <- df[!is_unc, , drop = FALSE]
  }
  # Coerce remaining schedule columns back to numeric.
  for (col in names(df)) df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  df <- df[!is.na(df[[time_col]]), , drop = FALSE]
  rownames(df) <- NULL
  list(uncertainty = unc_row, schedule = df)
}

# -----------------------------------------------------------------------
# COLUMN-HEADER UNITS (live tables)
# -----------------------------------------------------------------------
# Mirrors the "Duration (hours)" labelling used elsewhere in the wizard by
# appending units to the live-table column HEADERS for display only. The
# underlying data frame keeps clean column names (Ti, To, ...) so the engine
# and all downstream code are unaffected. apply_header_units() renames for
# display; strip_header_units() reverses it after an edit.

# clean column name -> unit string (no parentheses)
.siacs_phys_units <- c(
  Time = "min", Ti = "Kelvin", To = "Kelvin", OpenWindowArea = "m2",
  QBal = "m3/s", QUnbal = "m3/s", QFilter = "m3/s",
  RH = "[0,1]", BP = "Pascal", Wind = "m/s", SunFactor = "[0,1]"
)

# Rename a data frame's columns to "Name (unit)" for display. `units` is a
# named character vector mapping clean column name -> unit; columns not in
# `units` are left unchanged. For species tables (outdoor concentrations) a
# single `default_unit` applies to every non-Time column.
apply_header_units <- function(df, units = NULL, default_unit = NULL) {
  if (is.null(df) || ncol(df) == 0) return(df)
  nm <- names(df)
  new_nm <- vapply(nm, function(col) {
    u <- NULL
    if (!is.null(units) && col %in% names(units)) {
      u <- units[[col]]
    } else if (!is.null(default_unit) && col != names(df)[1]) {
      u <- default_unit
    }
    if (!is.null(u) && nzchar(u)) paste0(col, " (", u, ")") else col
  }, character(1), USE.NAMES = FALSE)
  names(df) <- new_nm
  df
}

# Reverse apply_header_units(): strip a trailing " (unit)" from each column
# name so the stored data frame keeps clean names. Safe to call on names that
# have no unit suffix.
strip_header_units <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  names(df) <- sub("\\s*\\([^)]*\\)\\s*$", "", names(df))
  df
}

# -----------------------------------------------------------------------
# DIFF-BASED PRE-TRANSITION INSERTER
# Like ensure_step_transitions but only acts on rows that actually CHANGED
# relative to a baseline.  Used by advanced_module.R where the user is
# editing a multi-row CSV cell-by-cell: we only want a pre-transition row
# in front of the cells they just edited, not in front of every value
# change that happens to exist in the preloaded file.
#
# Returns edited_df with zero or more pre-transition rows added at
# (edited_row$Time - trans) for each row whose values differ from
# stored_df at the same Time (or whose Time is not in stored_df at all).
#
# Left untouched:
#   - Rows in edited_df identical to stored_df at the same Time.
#   - Rows whose previous row (in sorted order) already holds the same
#     values (no actual step, so no pre-row needed).
#   - Deletions from stored_df: ignored (edited_df is authoritative).
# -----------------------------------------------------------------------
insert_pretransitions_for_edits <- function(edited_df, stored_df, trans = 0.1) {
  if (is.null(edited_df) || nrow(edited_df) == 0) return(edited_df)
  if (!"Time" %in% names(edited_df)) return(edited_df)

  non_time_cols <- setdiff(names(edited_df), "Time")
  if (length(non_time_cols) == 0) return(edited_df)

  trans_eff <- max(trans, 0.001)
  near_tol  <- 1e-6

  edited_df <- edited_df[order(edited_df$Time), , drop = FALSE]
  rownames(edited_df) <- NULL

  # No baseline means we have no basis to compute a diff; bail out rather
  # than normalize the whole table the way ensure_step_transitions would.
  if (is.null(stored_df) || nrow(stored_df) == 0) return(edited_df)

  stored_df <- stored_df[order(stored_df$Time), , drop = FALSE]
  rownames(stored_df) <- NULL

  common_cols <- intersect(non_time_cols, names(stored_df))

  # Identify which rows in edited_df differ from the baseline at the same Time
  rows_changed <- logical(nrow(edited_df))
  for (i in seq_len(nrow(edited_df))) {
    t_i <- edited_df$Time[i]
    matches <- which(abs(stored_df$Time - t_i) < near_tol)
    if (length(matches) == 0) {
      rows_changed[i] <- TRUE  # time not in baseline => new anchor
    } else if (length(common_cols) > 0) {
      sv <- unlist(stored_df[matches[1], common_cols, drop = FALSE])
      ev <- unlist(edited_df[i,          common_cols, drop = FALSE])
      rows_changed[i] <- !isTRUE(all.equal(sv, ev,
                                            check.attributes = FALSE,
                                            tolerance = near_tol))
    }
  }

  if (!any(rows_changed)) return(edited_df)

  # For each changed row, add a pre-row iff values differ from its predecessor
  pre_rows <- list()
  for (i in which(rows_changed)) {
    if (i == 1) next
    prev_row <- edited_df[i - 1, , drop = FALSE]
    curr_row <- edited_df[i,     , drop = FALSE]

    pv <- unlist(prev_row[, non_time_cols, drop = FALSE])
    cv <- unlist(curr_row[, non_time_cols, drop = FALSE])
    if (isTRUE(all.equal(pv, cv, check.attributes = FALSE,
                          tolerance = near_tol))) next  # no real step

    t_pre <- curr_row$Time - trans_eff
    if (t_pre <= prev_row$Time + near_tol) next                 # no room
    if (any(abs(edited_df$Time - t_pre) < near_tol,
            na.rm = TRUE)) next                                 # already there

    pr       <- prev_row
    pr$Time  <- t_pre
    pre_rows[[length(pre_rows) + 1]] <- pr
  }

  if (length(pre_rows) == 0) return(edited_df)

  out <- rbind(edited_df, do.call(rbind, pre_rows))
  out <- out[order(out$Time), , drop = FALSE]
  out <- out[!duplicated(out$Time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------
# NORMALIZE PRE-TRANSITION ROWS
# For any time column (Time) + value column(s), ensure that every step
# change from one user-placed "anchor" to the next has a pre-transition
# row at (anchor_i.time - trans) carrying the PRIOR anchor's value.
#
# Anchor identification (position-based, not value-based):
#   A row at time t is a pre-transition candidate iff another row exists
#   at t + trans (within tolerance).  Otherwise it's an anchor.
#   The first and last rows are always treated as anchors.
#
# Rebuild rule: keep only anchors, then between consecutive anchors where
# any value column differs, insert a pre-transition row whose values copy
# the PRIOR anchor (full row, so wide tables stay consistent across all
# columns). This guarantees:
#   - Initial-to-final transitions get their pre-row even when the only
#     events are those auto-placed endpoints.
#   - Stale pre-rows left over from earlier Add-Event calls are discarded
#     and re-derived from the current anchor chain (fixes ghost rows).
#   - Direct edits to anchor rows are preserved; direct edits to pre-
#     transition rows are overwritten by the prior anchor's value (the
#     user should edit the anchor instead).
#
# Idempotent: running ensure_step_transitions twice produces the same
# schedule.  Safe to call from fill_schedule_gaps on every update.
# -----------------------------------------------------------------------
ensure_step_transitions <- function(df, trans = 0.1) {
  if (is.null(df) || nrow(df) < 2) return(df)
  if (!"Time" %in% names(df)) return(df)

  non_time_cols <- setdiff(names(df), "Time")
  if (length(non_time_cols) == 0) return(df)

  trans_eff <- max(trans, 0.001)
  near_tol  <- 1e-6

  df <- df[order(df$Time), , drop = FALSE]
  rownames(df) <- NULL

  # ── Identify anchors by position ──────────────────────────────────────
  # Row i is a pre-transition row iff there's a row later in the table at
  # time Time[i] + trans_eff.  Otherwise row i is an anchor.
  is_anchor <- rep(TRUE, nrow(df))
  for (i in seq_len(nrow(df) - 1)) {
    target <- df$Time[i] + trans_eff
    later  <- df$Time[(i + 1):nrow(df)]
    if (any(abs(later - target) < near_tol)) {
      is_anchor[i] <- FALSE
    }
  }
  # Force first and last rows to be treated as anchors (endpoints never
  # act as pre-transitions for something even-earlier / even-later).
  is_anchor[1]          <- TRUE
  is_anchor[nrow(df)]   <- TRUE

  anchors <- df[is_anchor, , drop = FALSE]
  rownames(anchors) <- NULL

  # ── Rebuild: anchors + pre-transition rows where values change ────────
  out <- anchors[1, , drop = FALSE]
  if (nrow(anchors) >= 2) {
    for (i in seq(2, nrow(anchors))) {
      prev_a <- anchors[i - 1, , drop = FALSE]
      curr_a <- anchors[i,     , drop = FALSE]

      prev_vals <- unlist(prev_a[, non_time_cols, drop = FALSE])
      curr_vals <- unlist(curr_a[, non_time_cols, drop = FALSE])
      changed <- !isTRUE(all.equal(prev_vals, curr_vals,
                                    check.attributes = FALSE,
                                    tolerance = near_tol))

      if (changed) {
        t_pre <- curr_a$Time - trans_eff
        # Only insert when the pre-row fits strictly between the two anchors
        if (t_pre > prev_a$Time + near_tol) {
          pre_row       <- prev_a         # carries prior anchor's values
          pre_row$Time  <- t_pre
          out <- rbind(out, pre_row)
        }
      }
      out <- rbind(out, curr_a)
    }
  }

  out <- out[order(out$Time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------
# FORWARD-FILL HELPER
# After a row is deleted or a new event row is inserted, some cells may be
# NA (rhandsontable returns NA for cells the user never touched).  This
# function replaces every NA in non-Time columns with the value from the
# last row *above* that position which had a fully-populated value for that
# column.  It also removes any rows where the Time cell is NA (blank rows
# that rhandsontable can create when a row is right-click-deleted).
#
# step_transitions: when TRUE, also re-derive pre-transition rows via
# ensure_step_transitions() so every value change becomes a sharp step.
# This is only appropriate for schedules driven by discrete step-change
# EVENTS (ventilation, lights, activities). It must be FALSE (the default)
# for directly-edited or loaded schedules that contain smoothly-varying
# data — otherwise it turns continuous CMAQ temperature/wind series into a
# staircase and injects spurious 59.9/119.9/... rows before every hour.
# -----------------------------------------------------------------------
fill_schedule_gaps <- function(df, step_transitions = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Drop rows where Time is NA (blank rows left by rhandsontable delete)
  df <- df[!is.na(df$Time), , drop = FALSE]
  if (nrow(df) == 0) return(df)

  df <- df[order(df$Time), ]
  rownames(df) <- NULL

  non_time_cols <- setdiff(names(df), "Time")

  for (col in non_time_cols) {
    for (i in seq_len(nrow(df))) {
      if (is.na(df[i, col])) {
        # Find the nearest complete value above
        above <- which(!is.na(df[seq_len(i - 1), col]))
        if (length(above) > 0) {
          df[i, col] <- df[max(above), col]
        }
        # If still NA (nothing above), look below
        if (is.na(df[i, col])) {
          below <- which(!is.na(df[seq(i + 1, nrow(df)), col]))
          if (length(below) > 0) {
            df[i, col] <- df[i + below[1], col]
          }
        }
      }
    }
  }

  # Only force step-transition rows when explicitly requested (discrete-event
  # schedules). Direct edits / loaded continuous data are left untouched.
  if (isTRUE(step_transitions)) {
    df <- ensure_step_transitions(df, trans = 0.1)
  }

  df
}

# -----------------------------------------------------------------------
# LEGACY event-based generators (still used when assembling from the old
# per-variable phys_events data.frame, and for backward compatibility)
# -----------------------------------------------------------------------
generate_timeseries_from_events <- function(events, duration_minutes,
                                            initial_value, final_value,
                                            transition_time = 0.1,
                                            variable_name = "Value") {
  times  <- c(0, duration_minutes)
  values <- c(initial_value, final_value)

  if (!is.null(events) && nrow(events) > 0) {
    for (i in seq_len(nrow(events))) {
      et <- events$Time[i]; ev <- events$Value[i]
      times  <- c(times, max(0, et - transition_time))
      values <- c(values, values[length(values)])
      times  <- c(times, et)
      values <- c(values, ev)
    }
  }

  ord  <- order(times); times <- times[ord]; values <- values[ord]
  keep <- !duplicated(times); times <- times[keep]; values <- values[keep]

  result <- data.frame(Time = times)
  result[[variable_name]] <- values
  result
}

generate_light_schedule <- function(light_events, num_lights, duration_minutes,
                                    transition_time = 0.1) {
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (i in seq_len(num_lights)) schedule[[paste0("Light",i)]] <- 0

  if (is.null(light_events) || nrow(light_events) == 0) return(schedule)

  for (i in seq_len(num_lights)) {
    ln   <- paste0("Light",i)
    sub  <- light_events[light_events$Light == ln, ]
    if (nrow(sub) > 0) {
      ts <- generate_timeseries_from_events(
        data.frame(Time=sub$Time, Value=sub$Power),
        duration_minutes, 0, 0, transition_time, ln)
      schedule <- merge(schedule, ts, by="Time", all=TRUE)
      schedule[[paste0(ln,".x")]] <- NULL
      names(schedule)[names(schedule) == paste0(ln,".y")] <- ln
    }
  }
  for (col in names(schedule)[-1]) {
    schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
    schedule[[col]][is.na(schedule[[col]])] <- 0
  }
  schedule[order(schedule$Time), ]
}

generate_physical_environment <- function(phys_events, duration_minutes,
                                          initial_values, final_values,
                                          transition_time = 0.1) {
  all_vars <- c("Ti","To","OpenWindowArea","QBal","QUnbal","QFilter","RH","BP","Wind")
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (var in all_vars) {
    ev  <- if (!is.null(phys_events) && nrow(phys_events) > 0)
      phys_events[phys_events$Variable == var, ] else data.frame(Time=numeric(0),Value=numeric(0))
    ts  <- generate_timeseries_from_events(
      data.frame(Time=ev$Time, Value=ev$Value),
      duration_minutes, initial_values[[var]], final_values[[var]],
      transition_time, var)
    schedule <- merge(schedule, ts, by="Time", all=TRUE)
  }
  for (col in names(schedule)[-1]) schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
  schedule[order(schedule$Time), ]
}

generate_activities_schedule <- function(activity_schedules, duration_minutes,
                                         transition_time = 0.1) {
  all_activities <- c("Generic","Adult","Smoking","GasCooking.Persily1998","Incense.Manoukian2013")
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (act in all_activities) {
    events <- activity_schedules[[act]]
    if (!is.null(events) && nrow(events) > 0) {
      init_val  <- if (events$Time[1] == 0) events$Value[1] else 0
      final_val <- events$Value[nrow(events)]
      ts <- generate_timeseries_from_events(events, duration_minutes,
        init_val, final_val, transition_time, act)
      schedule <- merge(schedule, ts, by="Time", all=TRUE)
    } else {
      schedule[[act]] <- 0
    }
  }
  for (col in names(schedule)[-1]) {
    schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
    schedule[[col]][is.na(schedule[[col]])] <- 0
  }
  schedule[order(schedule$Time), ]
}

write_csv_with_units <- function(data, filepath, units_row = NULL) {
  if (!is.null(units_row)) {
    units_line <- paste0("# ", paste(units_row, collapse = ","))
    writeLines(units_line, filepath)
    suppressWarnings(
      write.table(data, filepath, append = TRUE, sep = ",",
                  row.names = FALSE, quote = FALSE)
    )
  } else {
    write.csv(data, filepath, row.names = FALSE, quote = FALSE)
  }
  cat("Written:", filepath, "\n")
}

# -----------------------------------------------------------------------
# ASSEMBLE AND WRITE WIZARD DATA
# -----------------------------------------------------------------------
assemble_and_write_wizard_data <- function(input, wizard_state, output_dir = NULL) {

  # Local coefficient tables (fallback when wiz_stack_coeff not present)
  stack_coeff_table <- data.frame(
    stories=1:3,
    two=c(0.000420,0.000326,0.000231))
  wind_coeff_table <- data.frame(
    stories=1:3,
    two=c(0.000494,0.000382,0.000271))

  cat("\n=== ASSEMBLING AND WRITING WIZARD DATA ===\n")

  if (is.null(output_dir)) {
    output_dir <- paste0("Input_", format(Sys.time(),"%Y-%m-%d_%H%M%S"))
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive=TRUE)
    cat("Created directory:", output_dir, "\n")
  }

  files_created <- list()
  assembly_errors   <- character(0)
  assembly_warnings <- character(0)

  # Helper: run a validate_* function and collect into assembly results
  run_assembly_check <- function(df, type, label, duration_hours = NULL) {
    res <- tryCatch(
      validate_data_file(df, type, duration_hours),
      error = function(e) create_validation_result(TRUE, character(0),
        paste("Could not validate", label, ":", e$message))
    )
    if (length(res$errors)   > 0)
      assembly_errors   <<- c(assembly_errors,   paste0("[", label, "] ", res$errors))
    if (length(res$warnings) > 0)
      assembly_warnings <<- c(assembly_warnings, paste0("[", label, "] ", res$warnings))
    invisible(res)
  }

  # Ensure global lists exist
  if (!exists("input_data_list",envir=.GlobalEnv)) assign("input_data_list",list(),envir=.GlobalEnv)
  input_data_list <- get("input_data_list",envir=.GlobalEnv)
  if (!exists("OutputList",envir=.GlobalEnv))      assign("OutputList",list(),envir=.GlobalEnv)
  OutputList <- get("OutputList",envir=.GlobalEnv)
  if (!exists("instances",envir=.GlobalEnv))       assign("instances",c(1),envir=.GlobalEnv)
  instances <- get("instances",envir=.GlobalEnv)

  current_idx <- if (exists("wizard_edit_mode", envir = .GlobalEnv) &&
                        isTRUE(get("wizard_edit_mode", envir = .GlobalEnv))) {
    as.integer(get("wizard_edit_sim_no", envir = .GlobalEnv))
  } else {
    if (length(instances) > 0) tail(as.integer(instances),1) else 1
  }
  if (is.na(current_idx) || current_idx < 1) current_idx <- 1

  # Helper: build a path inside the per-instance output_dir.
  # Files are no longer suffixed — each instance has its own folder.
  inp_path <- function(filename) {
    file.path(output_dir, filename)
  }

  # -------------------------------------------------------------------
  # 1) Seed all 17 default files into current_idx slot
  # -------------------------------------------------------------------
  read_default_file <- function(filepath) {
    if (!file.exists(filepath)) return(NULL)
    if (grepl("\\.xlsx$",filepath,ignore.case=TRUE)) {
      if (!requireNamespace("readxl",quietly=TRUE)) return(NULL)
      tryCatch({
        sheets <- readxl::excel_sheets(filepath)
        if (length(sheets)==1) as.data.frame(readxl::read_excel(filepath))
        else { df_list <- lapply(sheets,function(s) as.data.frame(readxl::read_excel(filepath,sheet=s))); names(df_list)<-sheets; df_list }
      },error=function(e) NULL)
    } else {
      tryCatch(read.csv(filepath,comment.char="#",stringsAsFactors=FALSE),error=function(e) NULL)
    }
  }

  # Indices of files that SIACS auto-generates when absent.
  # Deliberately NOT seeded here so input_data_list entries remain NULL,
  # allowing SIACS to calculate them at runtime.
  #   1  = InitialValues (starts from equilibrium) — overridden per wizard mode,
  #        see handle_wizard_initial_values() below for the three-way logic.
  #   9  = IndoorLight   (calculated from components)
  #   10 = OutdoorLightDirect  (calculated from lat/lon/time)
  #   11 = OutdoorLightDiffuse (calculated from lat/lon/time)
  #   14 = ArtificialLight     (calculated from List+Spectra+Schedule)
  optional_auto_generated_idx <- c(1L, 9L, 10L, 11L, 14L)

  cat("Loading default files into input_data_list index",current_idx,"...\n")
  # Key correction map: UI title (spaces removed) -> engine key name
  key_corrections <- c("DepositionVelocity" = "DepositionV")
  for (i in seq_along(titles_advanced)) {
    if (i %in% optional_auto_generated_idx) next  # leave NULL — SIACS will generate
    file_key  <- gsub(" ","",titles_advanced[i])
    if (file_key %in% names(key_corrections)) file_key <- key_corrections[[file_key]]
    file_path <- file_paths_advanced[i]
    if (is.null(input_data_list[[file_key]])) input_data_list[[file_key]] <- list()
    if (file_path != "none") {
      df <- read_default_file(file_path)
      if (!is.null(df)) input_data_list[[file_key]][[current_idx]] <- df
    }
  }

  # -------------------------------------------------------------------
  # 2) OutputList
  # -------------------------------------------------------------------
  adv <- wizard_output_defaults()
  out <- adv
  out$OutputTable       <- normalize_output_path(input$wiz_output_table) %||% adv$OutputTable
  out$OutputBasicChart  <- normalize_output_path(input$wiz_output_chart) %||% adv$OutputBasicChart
  out$OutputTimeDerivatives       <- if (isTRUE(input$wiz_output_derivatives))
    normalize_output_path(input$wiz_output_derivatives_file) %||% adv$OutputTimeDerivatives else "None"
  out$OutputMassBalanceComponents <- if (isTRUE(input$wiz_output_massbalance))
    normalize_output_path(input$wiz_output_massbalance_file) %||% adv$OutputMassBalanceComponents else "None"
  out$OutputSensitivity           <- if (isTRUE(input$wiz_output_sensitivity))
    normalize_output_path(input$wiz_output_sensitivity_file) %||% adv$OutputSensitivity else "None"
  out$OutputUncertainty           <- if (isTRUE(input$wiz_output_uncertainty))
    normalize_output_path(input$wiz_output_uncertainty_file) %||% adv$OutputUncertainty else "None"

  OutputList[[current_idx]] <- out
  assign("OutputList",OutputList,envir=.GlobalEnv)

  # -------------------------------------------------------------------
  # 3) Overwrite with wizard-generated data
  # -------------------------------------------------------------------
  update_input_list <- function(key, data) {
    # Use <<- to modify input_data_list in the enclosing function scope,
    # not create a local copy inside this closure. Without <<- every call
    # modifies a throw-away local binding and the changes are lost.
    if (is.null(input_data_list[[key]])) input_data_list[[key]] <<- list()
    input_data_list[[key]][[current_idx]] <<- data
  }

  # 3a. TIME
  time_data <- data.frame(
    StartTimeYear      = as.integer(input$wiz_start_year),
    StartTimeMonth     = as.integer(input$wiz_start_month),
    StartTimeDay       = as.integer(input$wiz_start_day),
    StartTime          = input$wiz_start_time,
    StartTimeStandard  = input$wiz_timezone,
    RelativeStartTime  = input$wiz_relative_start,
    TimeStep           = input$wiz_timestep,
    Duration           = input$wiz_duration,
    ActivityTransition = input$wiz_activity_transition,
    stringsAsFactors=FALSE)
  time_file <- inp_path("Time.csv")
  write_csv_with_units(time_data, time_file,
    c("Gregorian","1 to 12","1 to 31","hour:min:sec",
      "R POSIX time zones","min","min","hours","min"))
  run_assembly_check(time_data, "Time", "Time")
  files_created$Time <- time_file
  update_input_list("Time", time_data)

  # 3b. BOX DATA
  n_stories <- input$wiz_num_stories %||% 2
  stack_c   <- input$wiz_stack_coeff %||% stack_coeff_table$two[min(n_stories,3)]
  wind_c    <- input$wiz_wind_coeff  %||% wind_coeff_table$two[ min(n_stories,3)]

  box_data <- data.frame(
    FloorSurfaceArea              = input$wiz_floor_area,
    RoomHeight                    = input$wiz_room_height,
    AspectRatio                   = input$wiz_aspect_ratio,
    OrientationWiderSide          = input$wiz_orientation,
    AreaToVolume                  = input$wiz_area_to_volume,
    InfiltrationSurfaceArea       = input$wiz_infiltration_area,
    StackCoefficient              = stack_c,
    WindCoefficient               = wind_c,
    DischargeCoefficient          = input$wiz_discharge_coeff,
    GravityAccel                  = input$wiz_gravity,
    MidpointHeightofWindow        = input$wiz_midpoint_height,
    NeutralPressureLevel          = input$wiz_neutral_pressure,
    OpeningEffectivenessCoefficient = input$wiz_opening_effectiveness,
    Latitude                      = input$wiz_latitude,
    Longitude                     = input$wiz_longitude,
    Altitude                      = input$wiz_altitude,
    SurfaceAlbedo                 = input$wiz_surface_albedo,
    CloudOpticalDepth             = input$wiz_cloud_optical_depth,
    CloudBase                     = input$wiz_cloud_base,
    CloudTop                      = input$wiz_cloud_top,
    IndoorReflectance             = input$wiz_indoor_reflectance,
    stringsAsFactors=FALSE)
  box_file <- inp_path("BoxData.csv")
  write.csv(box_data, box_file, row.names=FALSE)
  run_assembly_check(box_data, "BoxData", "Box Data")
  files_created$BoxData <- box_file
  update_input_list("BoxData", box_data)

  # 3c. WINDOWS
  # Source-of-truth priority:
  #   Tier 1 — wizard_state$windows_data: seeded at server startup from the
  #             file-read default (wizard_defaults$windows), then updated live
  #             whenever the user edits Screen 5.  Always file-authoritative;
  #             never subject to the Shiny async UI round-trip.
  #   Tier 2 — wizard_defaults$windows: direct file-read object available in
  #             the assemble function's environment via parent scope.  Used only
  #             if wizard_state$windows_data is still NULL (extreme startup race).
  #   Tier 3 — UI inputs: last resort; may be stale definition-time values if
  #             Screen 5 was never visited in the browser.
  num_windows <- input$wiz_num_windows %||% 0
  if (!is.null(num_windows) && num_windows > 0) {

    windows_data <- NULL

    # ── Tier 1: wizard_state (file-seeded, kept live by Screen-5 observer) ──
    if (!is.null(wizard_state$windows_data) &&
        is.data.frame(wizard_state$windows_data) &&
        nrow(wizard_state$windows_data) > 0) {
      windows_data <- wizard_state$windows_data
      cat("[Wizard] Windows: using wizard_state$windows_data (",
          nrow(windows_data), "row(s)) — file-authoritative\n")

    # ── Tier 2: wizard_defaults (direct file-read, bypasses UI entirely) ────
    } else {
      wd <- tryCatch(get("wizard_defaults", envir = parent.frame()), error = function(e) NULL)
      if (!is.null(wd) && !is.null(wd$windows) &&
          is.data.frame(wd$windows) && nrow(wd$windows) > 0) {
        windows_data <- wd$windows
        cat("[Wizard] Windows: falling back to wizard_defaults$windows (",
            nrow(windows_data), "row(s))\n")

      # ── Tier 3: reconstruct from UI inputs (stale-value risk) ─────────────
      } else {
        cat("[Wizard] Windows: reconstructing from UI inputs",
            "(Screen 5 may not have been visited — values may be stale)\n")
        windows_data <- do.call(rbind, lapply(seq_len(num_windows), function(i) {
          data.frame(
            WindowNumber           = as.integer(i),
            Orientation            = input[[paste0("wiz_window_",i,"_orientation")]],
            AspectRatio            = input[[paste0("wiz_window_",i,"_aspect")]],
            WallSurfaceFraction    = input[[paste0("wiz_window_",i,"_fraction")]],
            GlassType              = input[[paste0("wiz_window_",i,"_glass")]],
            ObstructedAreaFraction = input[[paste0("wiz_window_",i,"_obstruction")]],
            HorizonElevationAngle  = input[[paste0("wiz_window_",i,"_horizon")]],
            stringsAsFactors = FALSE)
        }))
      }
    }

    if (!is.null(windows_data) && nrow(windows_data) > 0) {
      windows_file <- inp_path("Windows.csv")
      write.csv(windows_data, windows_file, row.names=FALSE)
      files_created$Windows <- windows_file
      update_input_list("Windows", windows_data)
    }
  }

  # 3d. ARTIFICIAL LIGHT LIST + SCHEDULE
  lights_mode <- input$wiz_lights_mode %||% "Use default files"

  # "Let SIACS calculate" — deliberately leave ArtificialLight*, OutdoorLightDirect,
  # OutdoorLightDiffuse, and IndoorLight keys NULL in input_data_list so the engine
  # runs its own calculation and saves the results.
  if (lights_mode == "Let SIACS calculate") {
    cat("Lights mode: 'Let SIACS calculate' — light keys left NULL in input_data_list.\n")

  } else if (lights_mode == "Upload custom files") {
    # ── Uploaded files ────────────────────────────────────────────────────
    list_upload  <- input$wiz_lights_list_file
    sched_upload <- input$wiz_lights_sched_file

    if (!is.null(list_upload)) {
      lights_data <- tryCatch(read.csv(list_upload$datapath, stringsAsFactors=FALSE, comment.char="#"),
                              error=function(e) NULL)
      if (!is.null(lights_data)) {
        # Preserve the user's uploaded filename; fall back to the canonical
        # name if Shiny's fileInput somehow delivered no $name.
        upload_name <- if (!is.null(list_upload$name) && nzchar(list_upload$name))
          list_upload$name else "ArtificialLightList.csv"
        lights_file <- inp_path(upload_name)
        write.csv(lights_data, lights_file, row.names=FALSE)
        files_created$ArtificialLightList <- lights_file
        update_input_list("ArtificialLightList", lights_data)
      }
    }

    if (!is.null(sched_upload)) {
      light_schedule <- tryCatch(read.csv(sched_upload$datapath, stringsAsFactors=FALSE, comment.char="#"),
                                 error=function(e) NULL)
      if (!is.null(light_schedule)) {
        upload_name <- if (!is.null(sched_upload$name) && nzchar(sched_upload$name))
          sched_upload$name else "ArtificialLightSchedule.csv"
        sched_file <- inp_path(upload_name)
        write.csv(light_schedule, sched_file, row.names=FALSE)
        files_created$ArtificialLightSchedule <- sched_file
        update_input_list("ArtificialLightSchedule", light_schedule)
      }
    }

  } else if (lights_mode == "Configure manually") {
    # ── Manual configuration ──────────────────────────────────────────────
    num_lights <- input$wiz_num_lights
    if (!is.null(num_lights) && num_lights > 0) {
      lights_data <- do.call(rbind, lapply(1:num_lights, function(i) {
        data.frame(
          LightNumber         = as.integer(i),
          Geometry            = input[[paste0("wiz_light_",i,"_geometry")]],
          Size                = input[[paste0("wiz_light_",i,"_size")]],
          Height              = input[[paste0("wiz_light_",i,"_height")]],
          DistanceShorterWall = input[[paste0("wiz_light_",i,"_dist_short")]],
          DistanceLongerWall  = input[[paste0("wiz_light_",i,"_dist_long")]],
          DirectionShorter    = input[[paste0("wiz_light_",i,"_dir_short")]],
          DirectionLonger     = input[[paste0("wiz_light_",i,"_dir_long")]],
          DirectionHeight     = input[[paste0("wiz_light_",i,"_dir_height")]],
          PowerEfficiency     = input[[paste0("wiz_light_",i,"_efficiency")]],
          Spectrum            = input[[paste0("wiz_light_",i,"_bulb")]],
          stringsAsFactors=FALSE)
      }))
      lights_file <- inp_path("ArtificialLightList.csv")
      write.csv(lights_data, lights_file, row.names=FALSE)
      files_created$ArtificialLightList <- lights_file
      update_input_list("ArtificialLightList", lights_data)

      # Use HOT-edited schedule if available, otherwise generate from defaults
      light_schedule <- if (!is.null(wizard_state$light_schedule) &&
                            nrow(wizard_state$light_schedule) > 0) {
        wizard_state$light_schedule
      } else {
        generate_light_schedule(
          data.frame(Time=numeric(0), Light=character(0), Power=numeric(0)),
          num_lights,
          (input$wiz_duration %||% 27) * 60,
          input$wiz_activity_transition %||% 0.1)
      }
      sched_file <- inp_path("ArtificialLightSchedule.csv")
      write.csv(light_schedule, sched_file, row.names=FALSE)
      files_created$ArtificialLightSchedule <- sched_file
      update_input_list("ArtificialLightSchedule", light_schedule)
    }

  } else {
    # ── Use default files (copy from Input/ into timestamped folder) ──────
    default_list_path  <- find_input_file("ArtificialLightList")
    default_sched_path <- find_input_file("ArtificialLightSchedule")

    for (pair in list(
      list(src=default_list_path,  key="ArtificialLightList",  dest="ArtificialLightList.csv"),
      list(src=default_sched_path, key="ArtificialLightSchedule", dest="ArtificialLightSchedule.csv")
    )) {
      if (!is.null(pair$src) && file.exists(pair$src)) {
        df <- tryCatch(read.csv(pair$src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
        if (!is.null(df)) {
          out_path <- inp_path(pair$dest)
          write.csv(df, out_path, row.names=FALSE)
          files_created[[pair$key]] <- out_path
          update_input_list(pair$key, df)
        }
      }
    }
  }

  # 3e. PHYSICAL ENVIRONMENT
  phys_mode <- input$wiz_phys_env_mode %||% "Use default file"

  if (phys_mode == "Manual entry") {
    # Prefer the HOT-edited schedule table if available
    phys_env <- if (!is.null(wizard_state$phys_schedule) &&
                    nrow(wizard_state$phys_schedule) > 0) {
      wizard_state$phys_schedule
    } else {
      initial_values <- list(
        Ti=input$wiz_temp_indoor_init, To=input$wiz_temp_outdoor_init,
        RH=input$wiz_rh_init, BP=input$wiz_bp_init, Wind=input$wiz_wind_init,
        OpenWindowArea=0, QBal=0, QUnbal=0, QFilter=0)
      final_values <- list(
        Ti=input$wiz_temp_indoor_final, To=input$wiz_temp_outdoor_final,
        RH=input$wiz_rh_final, BP=input$wiz_bp_final, Wind=input$wiz_wind_final,
        OpenWindowArea=0, QBal=0, QUnbal=0, QFilter=0)
      generate_physical_environment(
        data.frame(Time=numeric(0),Variable=character(0),Value=numeric(0)),
        (input$wiz_duration %||% 27)*60, initial_values, final_values,
        input$wiz_activity_transition %||% 0.1)
    }
    phys_file <- inp_path("PhysicalEnvironmentData.csv")
    # Manual schedules are stored numeric-only; the editable uncertainty row
    # lives separately in wizard_state$phys_uncertainty. Use the user's edited
    # uncertainties if present, otherwise fall back to per-variable defaults.
    phys_unc <- if (!is.null(wizard_state$phys_uncertainty))
      wizard_state$phys_uncertainty else NULL
    if (!is.null(phys_unc)) {
      phys_env <- combine_with_uncertainty(phys_env, phys_unc)
    } else {
      phys_env <- prepend_uncertainty_row(phys_env,
        defaults = .siacs_phys_uncertainty_defaults, fallback = 0)
    }
    # Units header for the Physical Environment file.
    phys_units <- c("min","Kelvin","Kelvin","m2","m3/s","m3/s","m3/s",
                    "[0,1]","Pascal","m/s")
    write_csv_with_units(phys_env, phys_file,
      units_row = if (ncol(phys_env) == length(phys_units)) phys_units else NULL)
    run_assembly_check(phys_env, "PhysicalEnvironment", "Physical Environment",
      duration_hours = input$wiz_duration %||% 27)
    files_created$PhysicalEnvironment <- phys_file
    update_input_list("PhysicalEnvironment", phys_env)

  } else if (phys_mode == "Upload custom file") {
    req_file <- input$wiz_phys_env_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$PhysicalEnvironment <- dest
        update_input_list("PhysicalEnvironment", df)
      }
    }
  }
  # "Use default file" — already seeded from file_paths_advanced in step 1

  # 3f. ACTIVITIES
  act_mode <- input$wiz_act_mode %||% "Use default file"

  if (act_mode == "Manual entry") {
    all_acts  <- c("Generic","Adult","Smoking","GasCooking.Persily1998","Incense.Manoukian2013")
    dur_min   <- (input$wiz_duration %||% 27) * 60
    all_times <- sort(unique(c(0, dur_min, unlist(lapply(all_acts, function(a) {
      s <- wizard_state$activity_schedules[[a]]
      if (!is.null(s) && nrow(s) > 0) s$Time else numeric(0)
    })))))
    acts_wide <- data.frame(Time = all_times, stringsAsFactors = FALSE)
    for (act_col in all_acts) {
      s <- wizard_state$activity_schedules[[act_col]]
      if (!is.null(s) && nrow(s) > 0) {
        s <- s[order(s$Time), ]
        acts_wide[[act_col]] <- sapply(all_times, function(t) {
          before <- s[s$Time <= t, ]
          if (nrow(before) == 0) s$Value[1] else tail(before$Value, 1)
        })
      } else {
        acts_wide[[act_col]] <- 0
      }
    }
    act_file <- inp_path("Activities.csv")
    write.csv(acts_wide, act_file, row.names=FALSE)
    run_assembly_check(acts_wide, "Activities", "Activities",
      duration_hours = input$wiz_duration %||% 27)
    files_created$Activities <- act_file
    update_input_list("Activities", acts_wide)

  } else if (act_mode == "Upload custom file") {
    req_file <- input$wiz_act_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$Activities <- dest
        update_input_list("Activities", df)
      }
    }
  }
  # "Use default file" — seeded from file_paths_advanced in step 1

  # 3g. OUTDOOR CONCENTRATIONS
  oc_mode <- input$wiz_outdoor_conc_mode %||% "Use default file"

  if (oc_mode == "Manual entry" && !is.null(wizard_state$outdoor_manual)) {
    oc_file <- inp_path("OutdoorConcentrations.csv")
    # Use the user's edited uncertainty row if present, else default 0.05.
    oc_unc <- if (!is.null(wizard_state$outdoor_uncertainty))
      wizard_state$outdoor_uncertainty else NULL
    oc_data <- if (!is.null(oc_unc)) {
      combine_with_uncertainty(wizard_state$outdoor_manual, oc_unc)
    } else {
      prepend_uncertainty_row(wizard_state$outdoor_manual,
        defaults = list(), fallback = 0.05)
    }
    # Units header: Time has no unit; every species column is ppm.
    oc_units <- c("min", rep("ppm", ncol(oc_data) - 1))
    write_csv_with_units(oc_data, oc_file, units_row = oc_units)
    run_assembly_check(oc_data, "OutdoorConcentrations",
      "Outdoor Concentrations", duration_hours = input$wiz_duration %||% 27)
    files_created$OutdoorConcentrations <- oc_file
    update_input_list("OutdoorConcentrations", oc_data)

  } else if (oc_mode == "Upload custom file") {
    req_file <- input$wiz_outdoor_conc_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$OutdoorConcentrations <- dest
        update_input_list("OutdoorConcentrations", df)
      }
    }
  }
  # "Use default file" — seeded in step 1

  # 3h. Copy default files to the snapshot folder (suffixed by instance).
  #     Covers both the unconditional statics and the "Use default file/files"
  #     modes for PhysicalEnvironment, Activities, OutdoorConcentrations,
  #     and all light-related files.

  # Always-copied statics.
  # NOTE: InitialIndoorConcentrations.csv is intentionally NOT included here.
  # When absent from input_data_list$InitialValues, SIACS starts from chemical
  # equilibrium automatically. Users who want specific initial concentrations
  # should upload the file via the Advanced module.
  static_defaults <- c("GlassTransmission.csv", "EmissionProfiles.csv",
                        "Vd&P-Carslaw 2012.csv",
                        "ArtificialLightSpectra.csv")
  # Map each static default filename to its engine role key so the snapshot
  # path can be recorded in files_created (used for output-file metadata).
  static_role_map <- c(
    "GlassTransmission.csv"     = "GlassTransmission",
    "EmissionProfiles.csv"      = "EmissionProfiles",
    "Vd&P-Carslaw 2012.csv"     = "DepositionV",
    "ArtificialLightSpectra.csv" = "ArtificialLightSpectra"
  )
  # Resolve a default input file robustly. find_input_file() looks in the
  # RELATIVE "Input" dir, but the app setwd()s to the workspace at startup
  # (which has no Input/ folder), so that lookup fails at runtime. Prefer the
  # rebased-absolute file_paths_advanced, then the install dir, then relative.
  resolve_default_input <- function(fname) {
    if (exists("file_paths_advanced")) {
      hit <- file_paths_advanced[basename(file_paths_advanced) == fname]
      if (length(hit) > 0 && file.exists(hit[1])) return(hit[1])
    }
    if (exists("SIACS_INSTALL_DIR", envir = .GlobalEnv)) {
      p <- file.path(get("SIACS_INSTALL_DIR", envir = .GlobalEnv), "Input", fname)
      if (file.exists(p)) return(p)
    }
    src <- find_input_file(tools::file_path_sans_ext(fname))
    if (!is.null(src) && file.exists(src)) return(src)
    p2 <- file.path("Input", fname)
    if (file.exists(p2)) return(p2)
    NULL
  }
  for (f in static_defaults) {
    src <- resolve_default_input(f)
    if (!is.null(src) && file.exists(src)) {
      dest <- inp_path(f)
      file.copy(src, dest, overwrite=TRUE)
      role <- static_role_map[[f]]
      if (!is.null(role)) files_created[[role]] <- dest
    }
  }

  # Physical Environment — copy default when not manually entered / uploaded
  if ((input$wiz_phys_env_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[3]   # PhysicalEnvironmentData
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        # Copy the file verbatim so the units comment line and Uncertainty row
        # are preserved exactly (read.csv+write.csv would drop the units line).
        file.copy(src, dest, overwrite = TRUE)
        files_created$PhysicalEnvironment <- dest
        update_input_list("PhysicalEnvironment", df)
      }
    }
  }

  # Activities — copy default when not manually entered / uploaded
  if ((input$wiz_act_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[5]   # Activities
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        file.copy(src, dest, overwrite = TRUE)
        files_created$Activities <- dest
        update_input_list("Activities", df)
      }
    }
  }

  # Outdoor Concentrations — copy default when not manually entered / uploaded
  if ((input$wiz_outdoor_conc_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[7]   # Outdoor Concentrations
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        file.copy(src, dest, overwrite = TRUE)
        files_created$OutdoorConcentrations <- dest
        update_input_list("OutdoorConcentrations", df)
      }
    }
  }

  # Light files — PATCHED: enforce Advanced-equivalent behavior.
  #
  # The Advanced module leaves these derived light inputs NULL when the user
  # leaves them blank, so SIACS recalculates them at runtime. The Wizard
  # previously copied default/previously-generated versions for indices 9/10/11/14,
  # which populated input_data_list and caused SIACS to SKIP recalculation,
  # leading to different Results plots.
  #
  # Fix: If user selected "Let SIACS calculate", force the per-instance entries
  # for these keys to NULL and remove any stale light files in this instance
  # folder so SIACS must regenerate them (matching Advanced).

  handle_wizard_light_inputs <- function(input, input_data_list, current_idx, output_dir) {
    lights_mode <- input$wiz_lights_mode %||% "Use default files"

    # Engine keys for the auto-generated inputs
    auto_keys <- c("InitialValues", "IndoorLight", "OutdoorLightDirect", "OutdoorLightDiffuse", "ArtificialLight")

    if (lights_mode == "Use default files") {
      # FIX: The default loading loop unconditionally skips optional_auto_generated_idx
      # (indices 9, 10, 11, 14) so those input_data_list keys are always NULL after
      # that loop, regardless of lights_mode.  handle_wizard_light_inputs had no
      # branch for "Use default files", so the keys stayed NULL and SIACS
      # recalculated light fluxes even when the user loaded default light files.
      #
      # Indices skipped by the loading loop that need populating here:
      #   9  = IndoorLight
      #   10 = OutdoorLightDirect
      #   11 = OutdoorLightDiffuse
      #   14 = ArtificialLight  (pre-computed; skip if file_path == "none")
      # Index 1 (InitialValues) is deliberately kept NULL (equilibrium start).
      light_load_idx <- c(9L, 10L, 11L, 14L)
      for (i in light_load_idx) {
        file_path <- file_paths_advanced[i]
        if (is.null(file_path) || file_path == "none") next
        file_key <- gsub(" ", "", titles_advanced[i])
        if (file_key %in% names(key_corrections)) file_key <- key_corrections[[file_key]]
        if (is.null(input_data_list[[file_key]])) input_data_list[[file_key]] <- list()
        df <- read_default_file(file_path)
        if (!is.null(df)) {
          input_data_list[[file_key]][[current_idx]] <- df
          cat("[Wizard] Use default files: loaded", file_key,
              "from", basename(file_path), "\n")
          # Copy the default into the snapshot folder and record its path so the
          # output-file metadata reports the real file (not "calculated"). Only
          # IndoorLight is surfaced in the metadata header, but copying all keeps
          # the snapshot self-contained.
          if (file.exists(file_path)) {
            dest <- file.path(output_dir, basename(file_path))
            tryCatch(file.copy(file_path, dest, overwrite = TRUE),
                     error = function(e) NULL)
            if (i == 9L) files_created[["IndoorLight"]] <<- dest
          }
        }
      }

    } else if (lights_mode == "Let SIACS calculate") {
      cat("\n[Wizard] Let SIACS calculate — forcing SIACS light recomputation for this instance\n")

      # 1) Ensure the current instance entries are NULL (do not affect other instances)
      for (k in auto_keys) {
        if (!is.null(input_data_list[[k]]) && is.list(input_data_list[[k]])) {
          # FIX: use single-bracket assignment to preserve list length.
          # [[k]] <- NULL removes the element and shrinks the list, causing
          # later positional access in compact_for_run to go out of bounds.
          input_data_list[[k]][current_idx] <- list(NULL)
										 
											  
										 
				
																										 
							 
										   
												
										   
										 
		   
        }
      }

      # 2) Remove any stale generated files from this run directory that could be reused
      patterns <- c(
        "InitialIndoorConcentrations.*\\.csv$",
        "ArtifLight.*\\.csv$",
        "DirectLight.*\\.csv$",
        "DiffuseLight.*\\.csv$",
        "IndoorLight.*\\.xlsx$"
      )

      for (pat in patterns) {
        fls <- list.files(output_dir, pattern = pat, ignore.case = TRUE, full.names = TRUE)
        if (length(fls) > 0) {
          suppressWarnings(file.remove(fls))
          cat("[Wizard] Removed stale light file(s):\n")
          cat(paste0("  - ", fls, collapse = "\n"), "\n")
        }
      }

      cat("[Wizard] Light inputs cleared for current instance; SIACS will auto-generate.\n")
    }

    input_data_list
  }

  # ---------------------------------------------------------------------
  # Initial Indoor Concentrations (Screen 4, collapsible block)
  # ---------------------------------------------------------------------
  # Three modes, mirroring the lights pattern:
  #   "Use default file"    -> load Input/InitialIndoorConcentrations.csv into
  #                            input_data_list$InitialValues[[current_idx]] and
  #                            copy the CSV into the snapshot Input_ folder.
  #   "Upload custom file"  -> use wizard_state$initial_values_df (validated on
  #                            upload in wizard_module.R) and write it as
  #                            InitialIndoorConcentrations.csv in the snapshot.
  #   "Let SIACS calculate" -> leave slot NULL; engine seeds from equilibrium.
  handle_wizard_initial_values <- function(input, wizard_state, input_data_list,
                                            current_idx, output_dir) {
    mode <- input$wiz_init_values_mode %||% "Use default file"

    # Local helper to resolve the snapshot path
    inp_path_local <- function(fname) file.path(output_dir, fname)

    if (is.null(input_data_list$InitialValues))
      input_data_list$InitialValues <- list()

    if (mode == "Use default file") {
      src <- file_paths_advanced[1]  # ./Input/InitialIndoorConcentrations.csv
      df  <- if (!is.null(src) && file.exists(src))
        tryCatch(read.csv(src, stringsAsFactors = FALSE, comment.char = "#"),
                 error = function(e) NULL) else NULL
      if (!is.null(df)) {
        input_data_list$InitialValues[[current_idx]] <- df
        dest <- inp_path_local("InitialIndoorConcentrations.csv")
        file.copy(src, dest, overwrite = TRUE)
        cat("[Wizard] Initial concentrations: loaded default (",
            basename(src), ")\n")
      } else {
        cat("[Wizard] Initial concentrations: default file unavailable; ",
            "falling back to SIACS equilibrium seed.\n")
        input_data_list$InitialValues[current_idx] <- list(NULL)
      }

    } else if (mode == "Upload custom file") {
      df <- wizard_state$initial_values_df
      if (is.null(df)) {
        cat("[Wizard] Initial concentrations: upload mode selected but no ",
            "validated file provided — falling back to SIACS equilibrium seed.\n")
        input_data_list$InitialValues[current_idx] <- list(NULL)
      } else {
        input_data_list$InitialValues[[current_idx]] <- df
        # Use the uploaded filename when available; fall back to the canonical
        # default if the observer didn't record a name (defensive).
        upload_name <- wizard_state$initial_values_name
        if (is.null(upload_name) || !nzchar(upload_name))
          upload_name <- "InitialIndoorConcentrations.csv"
        dest <- inp_path_local(upload_name)
        write.csv(df, dest, row.names = FALSE)
        cat("[Wizard] Initial concentrations: using uploaded file (",
            ncol(df), "species, saved as ", basename(dest), ").\n", sep = "")
      }

    } else {
      # "Let SIACS calculate"
      # Preserve list length with single-bracket assignment (see lights block
      # for the same rationale).
      input_data_list$InitialValues[current_idx] <- list(NULL)
      cat("[Wizard] Initial concentrations: left NULL — SIACS will seed from ",
          "chemical equilibrium.\n")
    }

    input_data_list
  }

  # Apply once, just before Finalize
  input_data_list <- handle_wizard_initial_values(input, wizard_state,
                                                   input_data_list,
                                                   current_idx, output_dir)
  input_data_list <- handle_wizard_light_inputs(input, input_data_list, current_idx, output_dir)


  # ── Record input-file provenance into the OutputList entry ──────────────────
  # Save.results() (SIACSPostProcessing.R) writes a metadata comment block at
  # the top of each output file listing the input files used. Those fields are
  # read from OutputList[[idx]]$BoxData, $Time, etc. Populate them now from the
  # snapshot files we just wrote (files_created is keyed by engine role).
  OutputList <- get("OutputList", envir = .GlobalEnv)
  if (length(OutputList) >= current_idx && !is.null(OutputList[[current_idx]])) {
    fc_path <- function(key) {
      v <- files_created[[key]]
      if (!is.null(v) && nzchar(v)) basename(v) else ""
    }
    OutputList[[current_idx]]$BoxData               <- fc_path("BoxData")
    OutputList[[current_idx]]$Time                  <- fc_path("Time")
    OutputList[[current_idx]]$DepositionV           <- fc_path("DepositionV")
    OutputList[[current_idx]]$PhysicalEnvironment   <- fc_path("PhysicalEnvironment")
    OutputList[[current_idx]]$OutdoorConcentrations <- fc_path("OutdoorConcentrations")
    OutputList[[current_idx]]$EmissionProfiles      <- fc_path("EmissionProfiles")
    OutputList[[current_idx]]$Activities            <- fc_path("Activities")
    # IndoorLight may be auto-generated (no file recorded); report as such.
    il <- fc_path("IndoorLight")
    OutputList[[current_idx]]$IndoorLight <-
      if (nzchar(il)) il else "(calculated by SIACS)"
    assign("OutputList", OutputList, envir = .GlobalEnv)
  }

  # Finalize
  assign("input_data_list", input_data_list, envir=.GlobalEnv)
  cat("=== DATA ASSEMBLY COMPLETE ===\n")
  cat("OutputList populated with:", length(OutputList), "entries\n")
  cat("input_data_list keys:", length(input_data_list), "file types\n")

  list(files_created=files_created, output_dir=output_dir,
       input_data_list=input_data_list,
       assembly_errors=assembly_errors, assembly_warnings=assembly_warnings)
}