# ============================================================================
# MiraProt File Contract: modules/Data Wizard/datawizard_file_loader.R
# Purpose:
#   Provide the file loader portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Compatibility loader and public File Loader module composition root.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Exactly one loader context owns loader-local state per module session; canonical datasets use the injected primary-data adapter.
# Mutation Authority:
#   Only the one loader server/context and its focused observer registrars may mutate loader state.
# Source-Order Assumptions:
#   Focused implementations load in declared order before modFileLoaderUI/modFileLoaderServer are exposed.
# Session/Restore Implications:
#   The existing loader session-state API, bounded restore attempts, and idempotent handoff remain unchanged.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# modules/datawizard_file_loader.R
# Enhanced File Loader Module with Performance Optimizations and Advanced Features
#
# Required packages: readr, data.table, stringi, readxl, tools, shiny, shinyjs

############
# Helper Functions

# Load file-loader utilities into this module's evaluation environment so the
# legacy loader path continues to expose the complete public interface.
source("modules/Data Wizard/file_loader/datawizard_file_reading.R", local = environment())
source("modules/Data Wizard/file_loader/datawizard_file_canonicalization.R", local = environment())
source("modules/Data Wizard/file_loader/datawizard_file_loader_ui.R", local = TRUE)
source("modules/Data Wizard/file_loader/datawizard_file_loader_interactive.R", local = TRUE)
source("modules/Data Wizard/file_loader/datawizard_file_loader_header_reset.R", local = TRUE)
source("modules/Data Wizard/file_loader/datawizard_file_loader_restore.R", local = TRUE)
source("modules/Data Wizard/file_loader/datawizard_file_loader_diagnostics.R", local = TRUE)
source("modules/Data Wizard/file_loader/datawizard_file_loader_context.R", local = TRUE)

# Extraction index for static lifecycle auditing: the context owns
# proc.time()[["elapsed"]], filenames_may_be_logged, duplicate_work, object_mb=,
# and the "validation", "sheet_enumeration", "xlsx_parsing", "normalization",
# "reset", and "cache_insertion" telemetry phases.

############
# UI Module

############
# Server Module

#' Enhanced File Loader Module Server with Performance Optimizations
#' @param id module namespace id
#' @param rv reactive values object (legacy parameter for compatibility)
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modFileLoaderServer <- function(id, rv = NULL, core_values = NULL, debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management
    # ========================================

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "FILE LOADER", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ FILE LOADER ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    # Enhanced error message handling
    safe_error_message <- function(e) {
      if (is.null(e) || is.null(e$message) || e$message == "" || e$message == " ") {
        return("Unknown error occurred")
      }
      return(e$message)
    }

    debug_log("File loader module server starting", 1)

    primary_data_state <- create_primary_data_state_adapter(
      rv = rv,
      core_values = core_values,
      debug_log_fn = debug_log
    )


    # ========================================
    # Reactive Values with Enhanced Features
    # ========================================

    loader_context <- create_datawizard_file_loader_context(
      input = input,
      output = output,
      session = session,
      rv = rv,
      debug_level = debug_level,
      debug_log = debug_log,
      safe_error_message = safe_error_message,
      primary_data_state = primary_data_state,
      core_values = core_values
    )
    loader_context_exports <- attr(loader_context, "exports")
    loader_context_binding_environment <- environment()
    loader_context_mutable_bindings <- c(
      "applying_loader_restore",
      "last_applied_loader_restore_signature",
      "last_applied_loader_restore_generation"
    )
    for (loader_context_name in setdiff(loader_context_exports, loader_context_mutable_bindings)) {
      assign(loader_context_name, loader_context[[loader_context_name]], inherits = FALSE)
    }
    for (loader_context_name in intersect(loader_context_exports, loader_context_mutable_bindings)) {
      local({
        context_name <- loader_context_name
        makeActiveBinding(context_name, function(value) {
          if (missing(value)) return(loader_context[[context_name]])
          loader_context[[context_name]] <- value
          invisible(value)
        }, env = loader_context_binding_environment)
      })
    }

    # Conditional UI visibility for header/sheet controls.
    # Keep these outputs unsuspended so tab switches and hidden states do not
    # freeze visibility updates.
    #
    # IMPORTANT: On session restore, older snapshots may have data_fixed/data2_fixed
    # populated while *_data_original is NULL. Use a broader "data available"
    # predicate so restored sessions reliably show header/sheet controls.
    has_loaded_primary_data <- reactive({
      candidates <- list(
        data_primary(),
        data_fixed(),
        primary_data_original(),
        if (!is.null(rv)) rv$primary_data_original else NULL,
        if (!is.null(rv)) rv$primary_data_raw else NULL,
        if (!is.null(rv)) rv$data_mod else NULL
      )
      any(vapply(candidates, is_valid_data, logical(1)))
    })

    has_loaded_secondary_data <- reactive({
      candidates <- list(
        data_additional(),
        data2_fixed(),
        secondary_data_original(),
        if (!is.null(rv)) rv$secondary_data_original else NULL
      )
      any(vapply(candidates, is_valid_data, logical(1)))
    })


    # ========================================
    # Enhanced Reactive Expressions
    # ========================================

    header_primary <- reactive({
      # Check module initialization first
      if (!isTRUE(module_initialized())) {
        return(1L)
      }

      # Check if input exists
      if (is.null(input$header_row)) {
        debug_log("Header row input not available yet", 2)
        return(1L)
      }

      tryCatch({
        hr <- as.integer(input$header_row)
        if (is.na(hr) || hr < 1) return(1L)
        return(hr)
      }, error = function(e) {
        debug_log(paste("Error in header_primary reactive:", safe_error_message(e)), 2)
        return(1L)
      })
    })

    header_additional <- reactive({
      # Check module initialization first
      if (!isTRUE(module_initialized())) {
        return(1L)
      }

      # Check if input exists
      if (is.null(input$header_row2)) {
        debug_log("Header row 2 input not available yet", 2)
        return(1L)
      }

      tryCatch({
        hr <- as.integer(input$header_row2)
        if (is.na(hr) || hr < 1) return(1L)
        return(hr)
      }, error = function(e) {
        debug_log(paste("Error in header_additional reactive:", safe_error_message(e)), 2)
        return(1L)
      })
    })

    header_primary_debounced <- debounce(header_primary, millis = 400)
    header_additional_debounced <- debounce(header_additional, millis = 400)

    # ========================================
    # Enhanced UI Outputs
    # ========================================

    is_excel_file_meta <- function(meta) {
      if (is.null(meta) || !is.list(meta)) return(FALSE)
      ext_raw <- meta$ext %||% ""
      name_raw <- meta$name %||% ""
      ext <- tolower(gsub("^\\.+", "", as.character(ext_raw)))
      if (!nzchar(ext) && nzchar(as.character(name_raw))) {
        ext <- tolower(tools::file_ext(as.character(name_raw)))
      }
      isTRUE(ext %in% c("xlsx", "xls"))
    }



    handoff_session_restore_upload <- function(file_input, upload_label = "session file") {
      restore_handler <- if (!is.null(rv) && is.function(rv$restore_session_from_file)) {
        rv$restore_session_from_file
      } else {
        session$userData$handle_session_restore_upload
      }

      if (!is.function(restore_handler)) {
        msg <- "Session restore is unavailable. Please use the Session Restore upload control."
        debug_log(paste("RDS handoff failed for", upload_label, "-", msg), 1)
        showNotification(msg, type = "error", duration = 8)
        return(invisible(FALSE))
      }

      debug_log(paste("Handing", upload_label, "RDS upload to session restore handler:", file_input$name %||% file_input$datapath), 1)
      showNotification(
        "Detected .rds file; attempting to restore a MiraProt session.",
        type = "message",
        duration = 5
      )
      restore_handler(file_input, source_label = "Data Wizard file loader")
      invisible(TRUE)
    }

    register_datawizard_file_loader_interactive(environment())
    register_datawizard_file_loader_header_reset(environment())
    register_datawizard_file_loader_restore(environment())
    register_datawizard_file_loader_diagnostics(environment())

    # ========================================
    # Return Interface (unchanged for compatibility)
    # ========================================

    return(list(
      # Core interface (unchanged)
      primary           = data_primary,
      additional        = data_additional,
      data_fixed        = data_fixed,
      data2_fixed       = data2_fixed,
      header_primary    = header_primary,
      header_additional = header_additional,
      init_meta         = init_handson_table_dw,

      # Enhanced features
      loading_errors = reactive({ loading_errors() }),
      loading_active = loading_active,
      current_operation = current_operation,
      loading_history = reactive({ loading_history() }),
      has_primary_data = reactive({ !is.null(data_primary()) }),
      has_additional_data = reactive({ !is.null(data_additional()) }),
      file_cache = reactive({ file_cache() }),
      module_initialized = module_initialized,
      loader_mode = reactive({ loader_mode() }),
      can_rebuild_metadata = reactive({
        identical(loader_mode(), "interactive_load") && !is.null(input$file)
      }),
      header_reprocess_active = reactive({ header_reprocess_active() }),

      # Functions
      load_file_enhanced = load_file_enhanced,
      module_health_check = module_health_check,
      set_additional_working_data = function(new_data) {
        if (is.null(new_data) || !is.data.frame(new_data)) return(invisible(FALSE))
        data2_fixed(new_data)
        data_additional(new_data)
        invisible(TRUE)
      },

      # Utility functions
      clear_cache = function() {
        tryCatch({
          file_cache(list())
          debug_log("Cache cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing cache:", e$message), 1)
        })
      },

      # Session save/restore hooks — consumed by
      # modules/Data Wizard/datawizard_integration.R
      # via get_all_submodule_ui_states / set_all_submodule_ui_states.
      get_session_state = function(mode = "full") {
        isolate({
          inputs <- list(
            sheetDropdown    = input$sheetDropdown %||% selected_sheet_primary(),
            sheetDropdown2   = input$sheetDropdown2 %||% selected_sheet_secondary(),
            header_row       = input$header_row,
            header_row2      = input$header_row2,
            data_source_type = input$data_source_type
          )

          if (identical(mode, "labels_only")) {
            return(list(
              version             = "1.0",
              mode                = "labels_only",
              primary_file_meta   = primary_file_meta(),
              secondary_file_meta = secondary_file_meta(),
              inputs              = inputs,
              restore_skip_publish_working_data = TRUE
            ))
          }

          primary_cache <- load_workbook_sheet_cache_for_session(
            sheet_cache_primary(), primary_file_meta(), input$file,
            "Primary session save"
          )
          secondary_cache <- load_workbook_sheet_cache_for_session(
            sheet_cache_secondary(), secondary_file_meta(), input$file2,
            "Secondary session save"
          )
          update_workbook_manifest(primary_file_meta, inputs$sheetDropdown, inputs$header_row, primary_cache)
          update_workbook_manifest(secondary_file_meta, inputs$sheetDropdown2, inputs$header_row2, secondary_cache)

          list(
            version                 = "1.0",
            mode                    = mode,
            data_fixed              = data_fixed(),
            data2_fixed             = data2_fixed(),
            sheet_cache_primary      = primary_cache,
            sheet_cache_secondary    = secondary_cache,
            primary_data_original   = primary_data_original(),
            secondary_data_original = secondary_data_original(),
            primary_file_meta       = primary_file_meta(),
            secondary_file_meta     = secondary_file_meta(),
            inputs = inputs,
            restore_skip_publish_working_data = TRUE
          )
        })
      },

      get_minimal_session_state = function() {
        isolate({
          list(
            version             = "1.0",
            mode                = "labels_only",
            primary_file_meta   = primary_file_meta(),
            secondary_file_meta = secondary_file_meta(),
            inputs = list(
              sheetDropdown    = input$sheetDropdown %||% selected_sheet_primary(),
              sheetDropdown2   = input$sheetDropdown2 %||% selected_sheet_secondary(),
              header_row       = input$header_row,
              header_row2      = input$header_row2,
              data_source_type = input$data_source_type
            )
          )
        })
      },

      set_session_state = function(state) {
        if (is.null(state) || !is.list(state)) return(invisible(FALSE))
        set_loader_mode("restore_replay", "session state staged")
        state$inputs <- state$inputs %||% list()
        state$inputs$header_row <- suppressWarnings(as.integer(state$inputs$header_row %||% 1L))
        state$inputs$header_row2 <- suppressWarnings(as.integer(state$inputs$header_row2 %||% 1L))
        if (is.na(state$inputs$header_row) || state$inputs$header_row < 1L) state$inputs$header_row <- 1L
        if (is.na(state$inputs$header_row2) || state$inputs$header_row2 < 1L) state$inputs$header_row2 <- 1L
        if (is_labels_only_loader_state(state)) {
          state$sheet_cache_primary <- list()
          state$sheet_cache_secondary <- list()
        } else {
          state$sheet_cache_primary <- state$sheet_cache_primary %||% state$all_sheets_primary %||% list()
          state$sheet_cache_secondary <- state$sheet_cache_secondary %||% state$all_sheets_secondary %||% list()
        }
        state$restore_generation <- state$restore_generation %||% get_loader_restore_generation()
        state$restore_signature <- state$restore_signature %||% compute_loader_restore_signature(state)
        pending_loader_state(state)
        inputs <- state$inputs
        debug_log(paste0(
          "Loader session state staged [",
          format_loader_restore_id(state$restore_generation, state$restore_signature),
          "]: header_row=",
          (inputs$header_row %||% "NA"),
          " sheetDropdown=",
          (inputs$sheetDropdown %||% "NA")), 2)
        invisible(TRUE)
      },

      get_loading_summary = reactive({
        tryCatch({
          primary_data <- data_primary()
          additional_data <- data_additional()
          summary_parts <- character()

          if (!is.null(primary_data)) {
            summary_parts <- c(summary_parts,
                               paste("Primary:", nrow(primary_data), "x", ncol(primary_data)))
          }

          if (!is.null(additional_data)) {
            summary_parts <- c(summary_parts,
                               paste("Additional:", nrow(additional_data), "x", ncol(additional_data)))
          }

          error_count <- length(loading_errors())
          if (error_count > 0) {
            summary_parts <- c(summary_parts, paste("Errors:", error_count))
          }

          cache_size <- length(file_cache())
          if (cache_size > 0) {
            summary_parts <- c(summary_parts, paste("Cached files:", cache_size))
          }

          return(if (length(summary_parts) > 0) paste(summary_parts, collapse = " | ") else "No data loaded")

        }, error = function(e) {
          return("Error getting summary")
        })
      })
    ))
  })
}
