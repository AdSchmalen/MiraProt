# ============================================================================
# File: modules/Data Wizard/datawizard_pivot.R
# Purpose:
#   Orchestrate the Data Wizard Pivot submodule by initializing dependencies,
#   wiring data access and UI-config helpers, and delegating output rendering,
#   observer registration, cleanup, and API assembly to the pivot sub-layer.
#
# Architecture role:
#   Single orchestrator. Owns the only server function (`modPivotServer`) for
#   the pivot module. Initialization logic lives here; all reactive outputs
#   are registered via pivot_register_outputs(); all observeEvent handlers are
#   registered via pivot_register_observers(); cleanup is wired via
#   pivot_register_cleanup(); the public API is assembled via pivot_build_api().
#
# Sub-layer responsibilities:
#   - datawizard_pivot_logic.R  : pure, stateless computation functions
#   - datawizard_pivot_observer.R: reactive outputs, observers, cleanup, API builder
#   - datawizard_pivot_state.R  : reactive state factory
#   - datawizard_pivot_ui.R     : static UI composition
#
# Safe maintenance notes:
#   - Keep `modPivotServer` as the only server function in this module tree.
#   - Keep the public signature of `modPivotServer` stable.
#   - Pure computation belongs in datawizard_pivot_logic.R.
#   - Observers and output rendering belong in datawizard_pivot_observer.R.
# ============================================================================

source("modules/Data Wizard/pivot/datawizard_pivot_ui.R",      local = modEnv)
source("modules/Data Wizard/pivot/datawizard_pivot_state.R",   local = modEnv)
source("modules/Data Wizard/pivot/datawizard_pivot_observer.R", local = modEnv)
source("modules/Data Wizard/pivot/datawizard_pivot_logic.R",   local = modEnv)

#' Pivot Module Server
#'
#' Single server entry point for the Data Wizard Pivot submodule.
#' @param id module namespace id
#' @param get_data function to get primary data
#' @param set_data function to set primary data
#' @param get_data2 function to get secondary data
#' @param set_data2 function to set secondary data
#' @param init_meta function to regenerate metadata skeleton
#' @param UI_config reactive containing UI configuration for import (optional)
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @param safe_ui_system optional safe UI helper system from parent module
#' @export
modPivotServer <- function(id, get_data = NULL, set_data = NULL, get_data2 = NULL,
                           set_data2 = NULL, init_meta = NULL, UI_config = NULL,
                           session_restore_trigger = reactive(NULL),
                           debug_level = 0, safe_ui_system = NULL,
                           primary_working_revision_debounced = reactive(NULL),
                           secondary_revision_debounced = reactive(NULL),
                           data_revision_signature = reactive(NULL),
                           initialized = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Debug Management System
    # ========================================

    # Helper function for controlled debug output
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "PIVOT", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ PIVOT ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Pivot module server starting", 1)

    # ========================================
    # State and UI System
    # ========================================

    state      <- pivot_create_state()
    ui_system  <- pivot_create_ui_system(session, safe_ui_system, debug_log)

    # Unpack reactive state handles for direct use in this scope.
    pivot_errors                 <- state$pivot_errors
    last_operation_time          <- state$last_operation_time
    operation_history            <- state$operation_history
    ui_config_applied            <- state$ui_config_applied
    ui_config_source             <- state$ui_config_source
    ui_config_update_in_progress <- state$ui_config_update_in_progress
    pivot_options_state          <- state$pivot_options_state
    preview_error_count          <- state$preview_error_count
    preview_last_error_time      <- state$preview_last_error_time
    pivot_operation_params       <- state$pivot_operation_params

    # ========================================
    # Error Tracking
    # ========================================

    # Add an error entry to the pivot_errors reactive list.
    # Safe for both reactive and non-reactive contexts.
    add_pivot_error <- function(operation, message, level = "error") {
      if (level == "error") {
        debug_log(paste("ERROR in", operation, ":", message), 1)
      } else if (level == "warning") {
        debug_log(paste("WARNING in", operation, ":", message), 1)
      } else {
        debug_log(paste("INFO", operation, ":", message), 2)
      }

      tryCatch({
        isolate({
          error_entry <- list(
            timestamp = Sys.time(),
            operation = operation,
            message   = message,
            level     = level
          )
          pivot_errors(c(pivot_errors(), list(error_entry)))
        })
      }, error = function(e) {
        debug_log(paste("Note: Error logging outside reactive context for", operation), 2)
      })
    }

    # ========================================
    # UI Config Management
    # ========================================

    # Collect current pivot-type-specific option inputs as a named list.
    collect_pivot_ui_options <- function(pivot_type = input$pivot_type_dw) {
      current_options <- list()

      if (!is.null(pivot_type)) {
        if (pivot_type == "wider") {
          if (!is.null(input$wider_names_from))  current_options$wider_names_from  <- input$wider_names_from
          if (!is.null(input$wider_values_from)) current_options$wider_values_from <- input$wider_values_from
          if (!is.null(input$wider_id_cols))     current_options$wider_id_cols     <- input$wider_id_cols
        } else if (pivot_type == "longer") {
          if (!is.null(input$longer_cols))      current_options$longer_cols      <- input$longer_cols
          if (!is.null(input$longer_names_to))  current_options$longer_names_to  <- input$longer_names_to
          if (!is.null(input$longer_values_to)) current_options$longer_values_to <- input$longer_values_to
        }
        # "transpose" has no additional option inputs; current_options stays empty.
      }

      current_options
    }

    # Collect normalized pivot UI state for export or config saving.
    collect_pivot_ui_state <- function(use_defaults = FALSE) {
      pivot_data_value <- input$pivot_data_dw
      pivot_type_value <- input$pivot_type_dw

      if (use_defaults) {
        pivot_data_value <- pivot_data_value %||% "primary"
        pivot_type_value <- pivot_type_value %||% "wider"
      }

      list(
        pivot_data_dw = pivot_data_value,
        pivot_type_dw = pivot_type_value,
        pivot_options = collect_pivot_ui_options(pivot_type_value)
      )
    }

    # Apply a UI configuration payload (internal, used by apply_ui_config and import).
    apply_pivot_ui_config_internal <- function(config, notify_user = FALSE,
                                               source_label = "import") {
      if (is.null(config) || !is.list(config)) {
        add_pivot_error("ui_config", "Configuration payload is missing or not a list", "warning")
        return(FALSE)
      }

      if (ui_config_update_in_progress()) {
        debug_log("UI config update skipped because another update is in progress", 2)
        return(FALSE)
      }

      ui_config_update_in_progress(TRUE)
      on.exit(ui_config_update_in_progress(FALSE), add = TRUE)

      # Guard: reject closures/non-atomic leaves before they reach
      # %in% / updateSelectInput(). A legacy save file can carry a stale
      # reactive closure, which would otherwise trigger
      # "cannot coerce type 'closure' to vector of type 'character'".
      is_applyable <- function(v) {
        !is.function(v) && (is.null(v) || is.atomic(v))
      }

      tryCatch({
        debug_log("Applying UI config for pivot module", 2)

        if (!is.null(config$pivot_data_dw)) {
          if (!is_applyable(config$pivot_data_dw)) {
            debug_log("[Pivot] restore: skipping non-scalar field pivot_data_dw", level = 1)
          } else if (config$pivot_data_dw %in% c("primary", "secondary")) {
            ui_system$update_input_safely("pivot_data_dw", config$pivot_data_dw, "selectInput")
          } else {
            add_pivot_error("ui_config", paste("Invalid data selection:", config$pivot_data_dw), "warning")
          }
        }

        if (!is.null(config$pivot_type_dw)) {
          if (!is_applyable(config$pivot_type_dw)) {
            debug_log("[Pivot] restore: skipping non-scalar field pivot_type_dw", level = 1)
          } else if (config$pivot_type_dw %in% c("wider", "longer", "transpose")) {
            ui_system$update_input_safely("pivot_type_dw", config$pivot_type_dw, "selectInput")
          } else {
            add_pivot_error("ui_config", paste("Invalid pivot type:", config$pivot_type_dw), "warning")
          }
        }

        if (!is.null(config$pivot_options) && is.list(config$pivot_options)) {
          # Strip any closure leaves from the nested options list before
          # staging it for UI creation (downstream update*Input calls would
          # otherwise fail on a closure).
          clean_opts <- config$pivot_options
          for (nm in names(clean_opts)) {
            if (!is_applyable(clean_opts[[nm]])) {
              debug_log(paste0("[Pivot] restore: skipping non-scalar pivot_options$", nm), level = 1)
              clean_opts[[nm]] <- NULL
            }
          }
          pivot_options_state(clean_opts)
          debug_log(
            paste("Queued pivot options for UI creation (fields:",
                  paste(names(clean_opts), collapse = ", "), ")"),
            2
          )
        }

        ui_config_applied(TRUE)
        ui_config_source(source_label)

        if (isTRUE(notify_user)) {
          ui_system$show_notification_safely("Pivot configuration applied successfully", "message", 3)
        }

        debug_log("Pivot UI config applied successfully", 1)
        TRUE
      }, error = function(e) {
        debug_log(paste("[Pivot] Error applying UI configuration:", e$message), level = 1)
        add_pivot_error("ui_config", paste("Error applying UI configuration:", e$message), "error")
        if (isTRUE(notify_user)) {
          ui_system$show_notification_safely("Error applying pivot configuration", "error", 6)
        }
        FALSE
      })
    }

    apply_ui_config <- function(ui_config) {
      apply_pivot_ui_config_internal(ui_config, notify_user = TRUE, source_label = "import")
    }

    get_current_ui_state <- function() {
      tryCatch({
        ui_state <- collect_pivot_ui_state(use_defaults = FALSE)
        debug_log(
          paste("Current UI state collected - Type:", ui_state$pivot_type_dw,
                "Data:", ui_state$pivot_data_dw,
                "Options:", length(ui_state$pivot_options)),
          2
        )
        ui_state
      }, error = function(e) {
        add_pivot_error("ui_state", paste("Error collecting UI state:", e$message), "error")
        NULL
      })
    }

    # ========================================
    # Data Access
    # ========================================

    get_primary_data <- function(log_null = TRUE) {
      tryCatch({
        if (!is.null(get_data) && is.function(get_data)) {
          data <- get_data()
          if (is.null(data)) {
            if (isTRUE(log_null)) debug_log("Primary data is NULL", 2)
          } else if (!is.data.frame(data)) {
            add_pivot_error("data_access", "Primary data is not a data frame", "warning")
          } else {
            debug_log(paste("Primary data accessed:", nrow(data), "x", ncol(data)), 2)
          }
          return(data)
        }
        return(NULL)
      }, error = function(e) {
        add_pivot_error("data_access", paste("Error accessing primary data:", e$message), "error")
        return(NULL)
      })
    }

    set_primary_data <- function(new_data) {
      tryCatch({
        if (!is.null(set_data) && is.function(set_data)) {
          if (is.null(new_data)) {
            add_pivot_error("data_update", "Attempting to set NULL primary data", "warning")
            return(FALSE)
          }
          if (!is.data.frame(new_data)) {
            add_pivot_error("data_update", "Attempting to set non-dataframe as primary data", "error")
            return(FALSE)
          }
          result <- set_data(new_data)
          debug_log(paste("Primary data updated:", nrow(new_data), "x", ncol(new_data)), 2)
          return(result)
        }
        return(FALSE)
      }, error = function(e) {
        add_pivot_error("data_update", paste("Error setting primary data:", e$message), "error")
        return(FALSE)
      })
    }

    get_secondary_data <- function() {
      tryCatch({
        if (!is.null(get_data2) && is.function(get_data2)) {
          data <- get_data2()
          if (!is.null(data) && is.data.frame(data)) {
            debug_log(paste("Secondary data accessed:", nrow(data), "x", ncol(data)), 2)
          }
          return(data)
        }
        return(NULL)
      }, error = function(e) {
        add_pivot_error("data_access", paste("Error accessing secondary data:", e$message), "error")
        return(NULL)
      })
    }

    set_secondary_data <- function(new_data) {
      tryCatch({
        if (!is.null(set_data2) && is.function(set_data2)) {
          if (!is.null(new_data) && !is.data.frame(new_data)) {
            add_pivot_error("data_update", "Attempting to set non-dataframe as secondary data", "error")
            return(FALSE)
          }
          result <- set_data2(new_data)
          if (!is.null(new_data)) {
            debug_log(paste("Secondary data updated:", nrow(new_data), "x", ncol(new_data)), 2)
          }
          return(result)
        }
        return(FALSE)
      }, error = function(e) {
        add_pivot_error("data_update", paste("Error setting secondary data:", e$message), "error")
        return(FALSE)
      })
    }

    set_current_data <- function(new_data) {
      tryCatch({
        if (is.null(input$pivot_data_dw)) {
          add_pivot_error("data_update", "No data selection for update", "warning")
          return(FALSE)
        }

        if (input$pivot_data_dw == "primary") {
          return(set_primary_data(new_data))
        } else if (input$pivot_data_dw == "secondary") {
          return(set_secondary_data(new_data))
        }

        add_pivot_error("data_update",
                        paste("Unknown data selection for update:", input$pivot_data_dw), "error")
        return(FALSE)
      }, error = function(e) {
        add_pivot_error("data_update", paste("Error setting current data:", e$message), "error")
        return(FALSE)
      })
    }

    # ========================================
    # Module Diagnostics
    # ========================================

    is_pivot_configured <- function() {
      tryCatch({
        if (is.null(input$pivot_type_dw)) return(FALSE)

        if (input$pivot_type_dw == "wider") {
          return(!is.null(input$wider_names_from) && !is.null(input$wider_values_from))
        } else if (input$pivot_type_dw == "longer") {
          return(!is.null(input$longer_cols) && length(input$longer_cols) > 0)
        } else if (input$pivot_type_dw == "transpose") {
          return(TRUE)
        }

        return(FALSE)
      }, error = function(e) {
        add_pivot_error("configuration_check",
                        paste("Error checking pivot configuration:", e$message), "error")
        return(FALSE)
      })
    }

    get_pivot_summary <- function() {
      tryCatch({
        if (!is_pivot_configured()) return("No pivot configured")

        config_details <- if (input$pivot_type_dw == "wider") {
          paste("Names from:", input$wider_names_from, "| Values from:", input$wider_values_from)
        } else if (input$pivot_type_dw == "longer") {
          paste("Columns:", length(input$longer_cols), "| Names to:", input$longer_names_to)
        } else if (input$pivot_type_dw == "transpose") {
          "Swaps all rows and columns; Row Index column is re-mapped"
        } else {
          ""
        }

        return(paste("Operation:", input$pivot_type_dw,
                     "| Data:", input$pivot_data_dw, "|", config_details))
      }, error = function(e) {
        add_pivot_error("summary", paste("Error getting pivot summary:", e$message), "error")
        return("Error getting summary")
      })
    }

    module_health_check <- function() {
      tryCatch({
        health_status <- list(
          module_name         = "Pivot",
          status              = "OK",
          debug_level         = DEBUG_LEVEL,
          error_count         = length(pivot_errors()),
          last_operation_time = last_operation_time(),
          ui_config_applied   = ui_config_applied(),
          ui_config_source    = ui_config_source()
        )

        warnings <- character()

        if (health_status$error_count > 3) {
          warnings <- c(warnings, paste("High error count:", health_status$error_count))
        }

        current_data <- tryCatch({
          # Access via isolate to avoid reactive dependency in a non-reactive context.
          isolate(get_primary_data())
        }, error = function(e) {
          warnings <<- c(warnings, "Cannot access current data")
          return(NULL)
        })

        if (is.null(current_data)) {
          warnings <- c(warnings, "No data available")
        } else if (!is.data.frame(current_data)) {
          warnings <- c(warnings, "Data is not a data frame")
        } else if (nrow(current_data) == 0) {
          warnings <- c(warnings, "Data is empty")
        }

        health_status$warnings       <- warnings
        health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

        debug_log(paste("Module health check - Status:", health_status$overall_health), 2)
        return(health_status)

      }, error = function(e) {
        debug_log(paste("Error in module health check:", e$message), 1)
        return(list(module_name = "Pivot", status = "ERROR",
                    error_message = e$message, overall_health = "Critical"))
      })
    }

    # ========================================
    # UI Export / Import Helpers
    # ========================================

    get_current_ui_values <- function() {
      tryCatch({
        collect_pivot_ui_state(use_defaults = FALSE)
      }, error = function(e) {
        list(pivot_data_dw = "primary", pivot_type_dw = "wider", pivot_options = list())
      })
    }

    get_pivot_ui_config_for_export <- function() {
      tryCatch({
        config <- collect_pivot_ui_state(use_defaults = TRUE)
        config$export_timestamp <- Sys.time()
        config$export_version   <- "enhanced_v1.0"
        config
      }, error = function(e) {
        list(pivot_data_dw = "primary", pivot_type_dw = "wider",
             pivot_options = list(), export_error = TRUE)
      })
    }

    set_pivot_ui_config_from_import <- function(config) {
      apply_pivot_ui_config_internal(config, notify_user = FALSE, source_label = "import")
    }

    # ========================================
    # Register Outputs, Observers, Cleanup
    # ========================================

    # Build the base context shared by all registration functions.
    base_ctx <- list(
      initialized                 = initialized,
      input                       = input,
      output                      = output,
      session                     = session,
      ns                          = ns,
      UI_config                   = UI_config,
      DEBUG_LEVEL                 = DEBUG_LEVEL,
      debug_log                   = debug_log,
      ui_system                   = ui_system,
      pivot_errors                = pivot_errors,
      last_operation_time         = last_operation_time,
      operation_history           = operation_history,
      ui_config_applied           = ui_config_applied,
      ui_config_source            = ui_config_source,
      ui_config_update_in_progress = ui_config_update_in_progress,
      pivot_options_state         = pivot_options_state,
      preview_error_count         = preview_error_count,
      preview_last_error_time     = preview_last_error_time,
      pivot_operation_params      = pivot_operation_params,
      add_pivot_error             = add_pivot_error,
      apply_ui_config             = apply_ui_config,
      get_current_ui_state        = get_current_ui_state,
      get_primary_data            = get_primary_data,
      get_primary_data_silent     = function() get_primary_data(log_null = FALSE),
      set_primary_data            = set_primary_data,
      get_secondary_data          = get_secondary_data,
      set_secondary_data          = set_secondary_data,
      set_current_data            = set_current_data,
      is_pivot_configured         = is_pivot_configured,
      get_pivot_summary           = get_pivot_summary,
      module_health_check         = module_health_check,
      primary_working_revision_debounced = primary_working_revision_debounced,
      secondary_revision_debounced = secondary_revision_debounced,
      data_revision_signature = data_revision_signature,
      get_current_ui_values       = get_current_ui_values,
      get_pivot_ui_config_for_export  = get_pivot_ui_config_for_export,
      set_pivot_ui_config_from_import = set_pivot_ui_config_from_import
    )

    # Register reactive outputs; get back get_current_data and pivot_preview_data.
    output_state <- pivot_register_outputs(base_ctx)
    full_ctx     <- c(base_ctx, output_state)

    # Register observeEvent handlers (apply/confirm pivot, UI_config changes).
    pivot_register_observers(full_ctx)

    # Register session cleanup.
    pivot_register_cleanup(full_ctx)

    # Session-restore bridge: flat closure-free snapshot + deferred apply.
    pivot_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        pivot_data_dw    = "selectInput",
        pivot_type_dw    = "selectInput",
        wider_names_from = "selectizeInput",
        wider_values_from = "selectizeInput",
        wider_id_cols    = "selectizeInput",
        longer_cols      = "selectizeInput",
        longer_names_to  = "textInput",
        longer_values_to = "textInput"
      ),
      module_label    = "Pivot",
      restore_trigger = session_restore_trigger
    )
    full_ctx$get_session_state <- pivot_session_state$get_session_state
    full_ctx$set_session_state <- pivot_session_state$set_session_state

    # Assemble and return the public API.
    return(pivot_build_api(full_ctx))
  })
}
