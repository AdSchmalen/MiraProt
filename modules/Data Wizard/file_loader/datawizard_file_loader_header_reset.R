# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_header_reset.R
# Purpose:
#   Provide the file loader header reset portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Mechanical observer/output-family extraction from datawizard_file_loader.R.
register_datawizard_file_loader_header_reset <- function(loader_environment = parent.frame()) {
  evalq({
    # ========================================
    # Enhanced Event Handlers with Robust Guards
    # ========================================

    # Primary data processing triggered only by data or header changes.
    # eventExpr only builds the dependency list; it must not stash a value
    # (e.g. `current_data`) for the handler to read, since observeEvent runs
    # eventExpr/handlerExpr as separate scopes (referencing it there fails
    # with "object 'current_data' not found"). The handler reads data_fixed()
    # directly instead.
    observeEvent({
      list(data_fixed(), header_primary_debounced())
    }, {
      if (isTRUE(restore_observer_guard_active("Header row observer"))) {
        return()
      }

      restored_primary_cache <- can_use_restored_sheet_cache_for_header_primary()
      raw_primary_data <- data_fixed()
      if (!is_valid_data(raw_primary_data) && !isTRUE(restored_primary_cache)) {
        return()
      }

      debug_log("Header row observer triggered", level = 2)
      debug_log(paste("header_row value:", header_primary_debounced()), level = 2)

      # Only process when module is initialized
      if (!isTRUE(module_initialized())) {
        debug_log("Header row observer: module not initialized, returning", level = 2)
        return()
      }

      if (isTRUE(skip_next_restore_header_reprocess_primary()) &&
          isTRUE(is_loader_restore_replay_context())) {
        skip_next_restore_header_reprocess_primary(FALSE)
        debug_log("Header row observer: consumed one-shot restore skip", level = 2)
        return()
      }

      if (isTRUE(skip_next_programmatic_header_update_primary())) {
        skip_next_programmatic_header_update_primary(FALSE)
        debug_log("Header row observer: consumed one-shot programmatic header update skip", level = 2)
        return()
      }

      # Hard rule: live header-driven reprocessing still requires explicit
      # interactive_load mode with a live primary file input. Restored-cache
      # replay is allowed only by the narrow header-observer predicate above.
      if (!isTRUE(can_header_reprocess_primary(restored_primary_cache))) {
        debug_log("Header row observer: skipped by loader mode gate", level = 2)
        return()
      }

      # # Critical: Block if processing results exist in rv$data_mod
      # if (!is.null(rv) && !is.null(rv$data_mod)) {
      #   rv_cols <- names(rv$data_mod)
      #   if (any(grepl("^Imputed |^Batch Corrected |^Pivoted |^Merged |^Ratio_", rv_cols))) {
      #     debug_log("Blocking header row observer - processing results detected in rv$data_mod", level = 1)
      #     return()
      #   }
      # }

      if (isTRUE(header_reprocess_active())) {
        debug_log("Header row observer: header reprocess already active, returning", level = 2)
        return()
      }

      # Process data with error handling. Keep a guard raised from before
      # header processing starts until the matching data + metadata publications
      # have both completed, so preview renderers never style new data with an
      # old metadata table.
      set_header_reprocess_active(TRUE)
      tryCatch({
        hr_input <- header_primary_debounced()
        hr_input <- if (is.null(hr_input)) 1L else as.integer(hr_input)
        if (is.na(hr_input) || hr_input < 1) hr_input <- 1L

        if (isTRUE(restored_primary_cache) && !is_live_excel_upload(input$file)) {
          selected_primary <- selected_sheet_primary() %||% input$sheetDropdown
          cached_sheet <- get_cached_sheet_data(sheet_cache_primary(), selected_primary)
          result <- normalize_cached_sheet_data(
            cached_sheet,
            header_primary_debounced(),
            "primary header change from restored cache"
          )
        } else {
          result <- process_data_with_header(raw_primary_data, hr_input, "primary data processing")
        }
        if (result$success) {
          data_primary(result$data)
          clear_derived_primary_state_for_sheet_change("Primary header change:")

          primary_data_state$set_raw_imported_data(result$data, "primary header change")
          debug_log("Header change: updated primary data through state adapter", level = 1)

          # Rebuild metadata against the final processed data frame so the
          # Column values exactly match result$data names (including Row Index
          # and any canonicalized header names). Drop rv$data_def first to
          # prevent downstream modules from reading stale metadata between the
          # data and metadata writes.
          if (!is.null(rv)) {
            rv$data_def <- NULL
          }
          header_metadata <- rebuild_metadata_for_dataset("primary_working", header_row = 1L)
          if (is.null(header_metadata)) {
            header_metadata <- init_handson_table_dw(result$data, header_row = 1L)
          }
          primary_data_state$set_metadata_for_current_data(header_metadata)
          debug_log("Header change: rebuilt metadata for processed primary data", level = 1)

          debug_log("Primary data processing completed", 2)
        } else {
          debug_log(paste("Primary data processing failed:", result$error), 1)
          showNotification(paste("Warning: Primary data processing failed:", result$error),
                           type = "warning", duration = 6)
        }
      }, error = function(e) {
        error_msg <- safe_error_message(e)
        debug_log(paste("Error in primary data processing:", error_msg), 1)
      }, finally = {
        release_header_reprocess_after_flush()
      })
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Additional data processing triggered only by data or header changes.
    # Same eventExpr/handlerExpr scope rationale as the primary observer
    # (see comment above the primary observeEvent).
    observeEvent({
      list(data2_fixed(), header_additional_debounced())
    }, {
      if (isTRUE(restore_observer_guard_active("Secondary header row observer"))) {
        return()
      }

      restored_secondary_cache <- can_use_restored_sheet_cache_for_header_secondary()
      raw_secondary_data <- data2_fixed()
      if (!is_valid_data(raw_secondary_data) && !isTRUE(restored_secondary_cache)) {
        return()
      }

      debug_log("Secondary header row observer triggered", level = 2)
      debug_log(paste("header_row2 value:", header_additional_debounced()), level = 2)

      # Only process when module is initialized
      if (!isTRUE(module_initialized())) {
        return()
      }

      if (isTRUE(skip_next_restore_header_reprocess_secondary()) &&
          isTRUE(is_loader_restore_replay_context())) {
        skip_next_restore_header_reprocess_secondary(FALSE)
        debug_log("Secondary header row observer: consumed one-shot restore skip", level = 2)
        return()
      }

      if (isTRUE(skip_next_programmatic_header_update_secondary())) {
        skip_next_programmatic_header_update_secondary(FALSE)
        debug_log("Secondary header row observer: consumed one-shot programmatic header update skip", level = 2)
        return()
      }

      # Same hard rule for secondary data: live reprocessing still requires
      # explicit interactive_load mode with a live secondary file input, while
      # restored-cache replay is allowed only by the narrow predicate above.
      if (!isTRUE(can_header_reprocess_secondary(restored_secondary_cache))) {
        debug_log("Secondary header row observer: skipped by loader mode gate", level = 2)
        return()
      }

      # Critical: Block if processing results exist in rv$data_mod
      if (!is.null(rv) && !is.null(rv$data_mod)) {
        rv_cols <- names(rv$data_mod)
        if (any(grepl("^Imputed |^Batch Corrected |^Pivoted |^Merged |^Ratio_", rv_cols))) {
          debug_log("Blocking secondary header row observer - processing results detected in rv$data_mod", level = 1)
          return()
        }
      }

      if (isTRUE(header_reprocess_active())) {
        debug_log("Secondary header row observer: header reprocess already active, returning", level = 2)
        return()
      }

      # Process data with error handling
      set_header_reprocess_active(TRUE)
      tryCatch({
        hr2_input <- header_additional_debounced()
        hr2_input <- if (is.null(hr2_input)) 1L else as.integer(hr2_input)
        if (is.na(hr2_input) || hr2_input < 1) hr2_input <- 1L

        if (isTRUE(restored_secondary_cache) && !is_live_excel_upload(input$file2)) {
          selected_secondary <- selected_sheet_secondary() %||% input$sheetDropdown2
          cached_sheet <- get_cached_sheet_data(sheet_cache_secondary(), selected_secondary)
          result <- normalize_cached_sheet_data(
            cached_sheet,
            header_additional_debounced(),
            "secondary header change from restored cache"
          )
        } else {
          result <- process_data_with_header(raw_secondary_data, hr2_input, "additional data processing")
        }
        if (result$success) {
          data_additional(result$data)
          publish_secondary_current_sheet(result$data, "secondary header change")
          debug_log("Additional data processing completed", 2)
        } else {
          debug_log(paste("Additional data processing failed:", result$error), 1)
          showNotification(paste("Warning: Additional data processing failed:", result$error),
                           type = "warning", duration = 6)
        }
      }, error = function(e) {
        error_msg <- safe_error_message(e)
        debug_log(paste("Error in additional data processing:", error_msg), 1)
      }, finally = {
        release_header_reprocess_after_flush()
      })
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    observeEvent(input$reset_btn_dw, {
      reset_release_scheduled <- FALSE
      release_reset_guard_after_flush <- function() {
        reset_release_scheduled <<- TRUE
        session$onFlushed(function() {
          reset_replay_active(FALSE)
          debug_log("RESET: header-row and sheet reset guard released", level = 2)
        }, once = TRUE)
      }

      tryCatch({
        debug_log("Reset button triggered", 1)

        # Keep every reset-state publication behind the same explicit guard.
        # Header-row observers check can_header_reprocess_*(), and sheet-change
        # observers return before applying cached/live sheet data while this is
        # TRUE. The guard stays TRUE until Shiny has flushed the UI updates below.
        reset_replay_active(TRUE)

        registry <- primary_data_state$registry()
        original_entry <- if (!is.null(registry) && is.function(registry$get_latest_entry)) {
          registry$get_latest_entry("primary_original")
        } else {
          NULL
        }
        original_raw_data <- if (!is.null(original_entry)) original_entry$data else NULL
        if (is.null(original_raw_data)) {
          original_raw_data <- primary_data_original() %||% data_fixed()
        }

        if (is.null(original_raw_data)) {
          reset_replay_active(FALSE)
          debug_log("RESET: No original raw data available", 1)
          showNotification("No original raw data to restore", type = "warning", duration = 5)
          return()
        }

        # Reset writes are grouped so downstream consumers see one final set of
        # non-debounced revision bumps after all raw, working, secondary,
        # metadata, filtered, and final state has been synchronized.
        primary_data_state$begin_datawizard_transaction("reset button")
        final_data <- primary_data_state$reset_primary_to_original(original_raw_data) %||% original_raw_data
        original_additional_data <- secondary_data_original() %||% data2_fixed()
        final_additional <- primary_data_state$reset_secondary_to_original(original_additional_data)
        primary_data_state$reset_processing_state_for_primary()

        # Primary data holders are synchronized first, in one canonical order,
        # before sheet selections or header-row inputs are reset.
        data_fixed(final_data)
        data_primary(final_data)
        primary_data_original(final_data)
        if (!is.null(final_additional)) {
          data2_fixed(final_additional)
          data_additional(final_additional)
          secondary_data_original(final_additional)
        }
        primary_data_state$publish_legacy_mirrors()
        primary_data_state$commit_datawizard_transaction()
        debug_log("RESET: Restored immutable original data and cleared derived processing state", level = 1)

        # Reset primary and secondary sheet selections before header-row inputs.
        skip_next_programmatic_header_update_primary(TRUE)
        skip_next_programmatic_header_update_secondary(TRUE)

        primary_meta <- primary_file_meta()
        primary_sheet_choices <- primary_meta$sheet_names %||% character(0)
        primary_sheet_choices <- as.character(primary_sheet_choices)
        reset_primary_sheet <- get_reset_sheet_from_state(sheet_cache_primary(), primary_meta)
        selected_sheet_primary(reset_primary_sheet)
        if (!is.null(reset_primary_sheet)) {
          if (length(primary_sheet_choices) > 0) {
            updateSelectInput(session, "sheetDropdown",
                              choices = primary_sheet_choices,
                              selected = reset_primary_sheet)
          } else {
            updateSelectInput(session, "sheetDropdown", selected = reset_primary_sheet)
          }
        }

        secondary_meta <- secondary_file_meta()
        secondary_sheet_choices <- secondary_meta$sheet_names %||% character(0)
        secondary_sheet_choices <- as.character(secondary_sheet_choices)
        reset_secondary_sheet <- get_reset_sheet_from_state(sheet_cache_secondary(), secondary_meta)
        selected_sheet_secondary(reset_secondary_sheet)
        if (!is.null(reset_secondary_sheet)) {
          if (length(secondary_sheet_choices) > 0) {
            updateSelectInput(session, "sheetDropdown2",
                              choices = secondary_sheet_choices,
                              selected = reset_secondary_sheet)
          } else {
            updateSelectInput(session, "sheetDropdown2", selected = reset_secondary_sheet)
          }
        }

        # Reset primary and secondary header-row inputs last. The reset guard
        # remains active until after this UI batch flushes, preventing these
        # updates from reprocessing stale sheet/header state into rv$data_mod.
        updateNumericInput(session, "header_row", value = 1)
        updateNumericInput(session, "header_row2", value = 1)
        release_reset_guard_after_flush()

        if (!is.null(final_data)) {
          shinyjs::show(id = ns("header_sheet_1"), asis = TRUE)
        }
        if (!is.null(data_additional())) {
          shinyjs::show(id = ns("header_sheet_2"), asis = TRUE)
        }

        # Cache / Fehler leeren.
        file_cache(list())
        loading_errors(list())

        debug_log("Reset completed - original raw data restored", 2)
        showNotification("Reset completed - original raw data restored", type = "message", duration = 3)

      }, error = function(e) {
        primary_data_state$commit_datawizard_transaction()
        if (!isTRUE(reset_release_scheduled)) {
          reset_replay_active(FALSE)
        }
        debug_log(paste("Error during reset:", e$message), 1)
        showNotification("Error during reset operation", type = "error", duration = 8)
      })
    })
  }, envir = loader_environment)
  invisible(NULL)
}
