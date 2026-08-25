# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_lifecycle_observers.R
# Purpose:
#   Provide the core lifecycle observers portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Choose between live and restored metadata while restore observers are
# replaying.  Dataset alignment is the first-order invariant; meaningfulness
# only breaks a tie between two candidates for the same live dataset.
select_datawizard_restore_lifecycle_metadata <- function(current_metadata,
                                                         restored_metadata,
                                                         current_data) {
  current_aligned <- metadata_matches_dataset(current_metadata, current_data)
  restored_aligned <- metadata_matches_dataset(restored_metadata, current_data)

  if (isTRUE(current_aligned)) {
    if (isTRUE(restored_aligned) &&
        is_meaningful_metadata(restored_metadata) &&
        !is_meaningful_metadata(current_metadata)) {
      return(restored_metadata)
    }
    return(current_metadata)
  }

  if (isTRUE(restored_aligned)) restored_metadata else current_metadata
}

#' Register core data/metadata lifecycle observers
#' @param loader_out File loader module output
#' @param core_values Core reactive values container
#' @param rv Shared app reactiveValues
#' @param data_loading_timestamp ReactiveVal tracking data load timestamp
register_core_data_lifecycle_observers <- function(loader_out, core_values, rv, data_loading_timestamp) {

  primary_data_state <- create_primary_data_state_adapter(
    rv = rv,
    core_values = core_values,
    debug_log_fn = debug_log
  )

  restore_guard_active <- function() {
    (!is.null(rv) &&
       (isTRUE(rv$session_restoring) ||
          datawizard_restore_phase_active(rv))) ||
      (is.list(loader_out) &&
         is.function(loader_out$loader_mode) &&
         identical(loader_out$loader_mode(), "restore_replay"))
  }

  header_reprocess_guard_active <- function() {
    (!is.null(rv) &&
       (isTRUE(rv$datawizard_header_reprocess_active) ||
          isTRUE(rv$header_reprocess_active))) ||
      (is.list(loader_out) &&
         is.function(loader_out$header_reprocess_active) &&
         isTRUE(loader_out$header_reprocess_active()))
  }

  is_row_index_only_metadata <- function(metadata) {
    if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0) {
      return(FALSE)
    }

    if (!"Content" %in% names(metadata)) {
      return(FALSE)
    }

    content_values <- metadata$Content
    valid_content <- content_values[!is.na(content_values) & nzchar(trimws(content_values))]

    length(valid_content) > 0 && all(valid_content == "Row Index")
  }

  in_critical_loading_window <- function() {
    timestamp <- data_loading_timestamp()
    if (is.null(timestamp)) return(FALSE)

    time_diff <- as.numeric(difftime(Sys.time(), timestamp, units = "secs"))
    return(time_diff < 2)
  }

  observe({
    loader_data <- validate_reactive_value(loader_out$primary, "loader_primary")

    if (!is.null(loader_data)) {
      current_data <- core_values$primary_data_raw()

      should_update <- TRUE
      if (!is.null(current_data) && !is.null(loader_data)) {
        if (ncol(current_data) > ncol(loader_data)) {
          should_update <- FALSE
          debug_log("Observer 1 BLOCKED - processed data detected (more columns)", level = 1)
        }
      }

      if (should_update && (is.null(current_data) || !identical(current_data, loader_data))) {
        debug_log(paste("Observer 1: Loading primary data:", nrow(loader_data), "rows"), level = 1)

        primary_data_state$set_raw_imported_data(loader_data, "loader primary observer")
      }
    }
  })

  observe({
    current_primary <- core_values$primary_data_raw()

    if (!is.null(current_primary) && !is.null(rv)) {
      if (!identical(rv$primary_data_raw, current_primary) &&
          !isTRUE(loader_out$loading_active()) &&
          !in_critical_loading_window()) {

        debug_log("Syncing module data update to rv through state adapter", level = 2)
        primary_data_state$set_raw_imported_data(current_primary, "core primary sync")
      }
    }
  })

  observe({
    current_metadata <- core_values$handson_metadata()

    if (!is.null(rv) && is.data.frame(current_metadata)) {
      if (header_reprocess_guard_active()) {
        debug_log(
          "Metadata lifecycle: header reprocess active; skipping redundant metadata sync",
          level = 2
        )
        return()
      }

      if (restore_guard_active() && is_row_index_only_metadata(current_metadata)) {
        current_primary <- tryCatch(
          resolve_datawizard_dataset(
            "primary_working", core_values = core_values, rv = rv
          )$data,
          error = function(e) NULL
        )
        restored_metadata <- tryCatch(
          datawizard_drop_deprecated_metadata_columns(rv$data_def),
          error = function(e) NULL
        )
        selected_metadata <- select_datawizard_restore_lifecycle_metadata(
          current_metadata, restored_metadata, current_primary
        )

        if (!identical(selected_metadata, current_metadata)) {
          core_values$handson_metadata(selected_metadata)
          debug_log(
            "Metadata lifecycle: preferred meaningful restored metadata aligned with live data",
            level = 1
          )
        } else if (metadata_matches_dataset(current_metadata, current_primary)) {
          debug_log(
            "Metadata lifecycle: preserving Row-Index-only metadata aligned with live data",
            level = 1
          )
          if (!identical(rv$data_def, current_metadata)) {
            primary_data_state$set_metadata_for_current_data(current_metadata)
          }
        } else {
          debug_log(
            "Metadata lifecycle: deferred misaligned Row-Index-only metadata during restore",
            level = 1
          )
        }

        return()
      }

      if (!identical(rv$data_def, current_metadata) &&
          !isTRUE(loader_out$loading_active()) &&
          !in_critical_loading_window()) {

        if (restore_guard_active()) {
          current_primary <- tryCatch(
            resolve_datawizard_dataset(
              "primary_working",
              core_values = core_values,
              rv = rv
            )$data,
            error = function(e) NULL
          )
          if (is.data.frame(current_primary) &&
              !metadata_matches_dataset(current_metadata, current_primary)) {
            debug_log(
              "Metadata lifecycle: restore replay produced a transient data/metadata mismatch; deferring synchronization",
              level = 1
            )
            return()
          }
        }

        debug_log("Syncing metadata update through state adapter", level = 2)
        primary_data_state$set_metadata_for_current_data(current_metadata)
      }
    }
  })

  observe({
    if (!core_values$metadata_observer_active()) {
      return()
    }

    if (header_reprocess_guard_active()) {
      debug_log(
        "Metadata lifecycle: header reprocess active; deferring metadata mismatch checks",
        level = 2
      )
      return()
    }

    current_data <- core_values$primary_data_raw()
    current_meta <- core_values$handson_metadata()
    restored_meta <- if (!is.null(rv)) tryCatch(
      datawizard_drop_deprecated_metadata_columns(rv$data_def),
      error = function(e) NULL) else NULL

    restore_selected_meta <- if (restore_guard_active()) {
      select_datawizard_restore_lifecycle_metadata(
        current_meta, restored_meta, current_data
      )
    } else NULL

    if (restore_guard_active() &&
        is.data.frame(restored_meta) &&
        !is_row_index_only_metadata(restored_meta) &&
        identical(restore_selected_meta, restored_meta)) {
      if (!identical(current_meta, restore_selected_meta) &&
          !is.null(core_values) &&
          is.function(core_values$handson_metadata)) {
        core_values$handson_metadata(restore_selected_meta)
      }
      debug_log(
        "Metadata lifecycle: restore replay active with restored metadata; skipping placeholder sync/recreation",
        level = 1
      )
      return()
    }

    metadata_reference_data <- select_datawizard_primary_display_data(
      core_values = core_values,
      rv = rv,
      context = "Metadata lifecycle",
      debug_log_fn = debug_log,
      publish_raw_if_missing = TRUE
    )

    if (is.null(metadata_reference_data)) {
      debug_log(
        "Metadata lifecycle: no displayable primary data available; deferring metadata creation",
        level = 2
      )
      return()
    }

    # During restore replay, never recreate/replace an existing metadata
    # snapshot; defer all metadata reconstruction to explicit interactive loads.
    if (restore_guard_active() && !is.null(current_meta)) {
      debug_log(
        "Metadata lifecycle: restore replay active; preserving restored metadata and skipping recreation",
        level = 1
      )
      return()
    }

    can_rebuild_metadata <- TRUE
    if (is.list(loader_out) && is.function(loader_out$can_rebuild_metadata)) {
      can_rebuild_metadata <- isTRUE(loader_out$can_rebuild_metadata())
    }

    should_recreate <- is.null(current_meta)

      if (!should_recreate && !is.null(current_meta)) {
        expected_cols <- ncol(metadata_reference_data)
        meta_rows <- nrow(current_meta)

        if (!is.data.frame(current_meta) || !("Column" %in% names(current_meta))) {
          debug_log("Metadata mismatch - current metadata is missing the Column field", level = 2)
          should_recreate <- TRUE
        } else if (meta_rows != expected_cols) {
          debug_log(paste("Metadata mismatch - Expected:", expected_cols, "Got:", meta_rows), level = 2)
          should_recreate <- TRUE
        } else if (!identical(as.character(current_meta$Column),
                              as.character(names(metadata_reference_data)))) {
          # Equal row count but different column names means the metadata
          # describes a different table (e.g. after a sheet switch where the
          # new sheet has the same number of columns but different names).
          debug_log("Metadata mismatch - column names diverge from data", level = 2)
          should_recreate <- TRUE
        }
      }

      if (should_recreate && !is.null(current_meta) && !isTRUE(can_rebuild_metadata)) {
        debug_log(
          "Metadata lifecycle: recreation blocked (non-interactive mode/no live file); preserving restored metadata",
          level = 1
        )
        should_recreate <- FALSE
      }

      if (should_recreate) {
        debug_log(paste("Creating/recreating metadata for", ncol(metadata_reference_data), "columns"), level = 1)

        tryCatch({
          data_cols <- names(metadata_reference_data)

          new_metadata <- data.frame(
            Column = data_cols,
            Content = rep(NA_character_, length(data_cols)),
            Options = rep(NA_character_, length(data_cols)),
            Numerator = rep(NA_character_, length(data_cols)),
            Denominator = rep(NA_character_, length(data_cols)),
            Transformation = rep(NA_character_, length(data_cols)),
            Sample = rep(NA_character_, length(data_cols)),
            stringsAsFactors = FALSE
          )

          row_index_row <- which(new_metadata$Column == "Row Index")
          if (length(row_index_row) > 0) {
            new_metadata$Content[row_index_row[1]] <- "Row Index"
            debug_log(paste("Row Index assigned to column at position:", row_index_row[1]), level = 2)
          } else {
            first_col_name <- new_metadata$Column[1]
            if (grepl("Row|Index|ID", first_col_name, ignore.case = TRUE)) {
              new_metadata$Content[1] <- "Row Index"
              debug_log(paste("Row Index assigned to first column (pattern match):", first_col_name), level = 2)
            }
          }

          core_values$handson_metadata(new_metadata)
          if (is.function(core_values$metadata_lifecycle_state)) {
            core_values$metadata_lifecycle_state("metadata_placeholder")
          }
          if (is.function(core_values$metadata_meaningful_ready)) {
            core_values$metadata_meaningful_ready(FALSE)
          }
          if (!is.null(rv)) {
            rv$datawizard_metadata_lifecycle_state <- "metadata_placeholder"
            rv$datawizard_metadata_meaningful_ready <- FALSE
          }
          debug_log("Metadata created/recreated successfully", level = 1)

        }, error = function(e) {
          debug_log(paste("Failed to create metadata:", e$message), level = 1)
        })
      }
  })

  observe({
    if (!is.null(data_loading_timestamp())) {
      timestamp <- data_loading_timestamp()
      time_diff <- as.numeric(difftime(Sys.time(), timestamp, units = "secs"))

      if (time_diff >= 2.5) {
        current_data <- core_values$primary_data_raw()
        current_meta <- core_values$handson_metadata()

        if (!is.null(current_data) && is.null(current_meta)) {
          debug_log("Triggering delayed metadata creation", level = 1)
          isolate({
            primary_data_state$set_raw_imported_data(current_data, "delayed metadata creation")
          })
        }

        data_loading_timestamp(NULL)
      } else {
        invalidateLater(500)
      }
    }
  })
}
