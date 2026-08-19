# ============================================================================
# File: modules/Data Wizard/batch_effects/datawizard_batch_effects_state.R
#
# What this file does:
#   Centralizes all reactive state for the Batch Effects module. Provides
#   initialization, cleanup, and state-query functions (configuration check,
#   summary, health check, UI state export).
#
# How it fits into the module architecture:
#   datawizard_batch_effects.R (orchestrator)
#     -> sources this file into modEnv
#     -> calls init_batch_effects_state() to create all reactiveVals
#     -> creates local aliases so remaining orchestrator code can reference
#        them directly (e.g. batchCounter instead of state$batchCounter)
#     -> passes the state list to sub-file functions (handlers, correction)
#
# File structure:
#   1. init_batch_effects_state()       -- creates and returns all reactiveVals
#   2. cleanup_batch_effects_state()    -- resets all reactiveVals to defaults
#   3. get_batch_effects_ui_state()     -- collects current UI state for export
#   4. check_batch_correction_configured() -- checks if correction is configured
#   5. get_batch_effects_summary()      -- generates a human-readable summary
#   6. run_batch_effects_health_check() -- module health diagnostics
#   7. apply_batch_correction_check()   -- defensive check for central apply
#
# What future developers need to know:
#   - Every reactiveVal in the module MUST be declared in
#     init_batch_effects_state() and nowhere else.
#   - If you add a new reactiveVal, also add its reset logic in
#     cleanup_batch_effects_state().
#   - State query functions receive input, state, and debug_log as arguments;
#     they do NOT close over module-level variables.
#   - The orchestrator creates local aliases for backward compatibility.
#     When code moves to sub-files (handlers, correction), those sub-files
#     receive the state list directly instead.
# ============================================================================

#' Initialize all reactive state for the Batch Effects module.
#'
#' @return Named list of reactiveVal objects.
init_batch_effects_state <- function() {
  list(
    # Batch group management
    batchCounter             = reactiveVal(2),
    batch_inputs             = reactiveVal(list()),

    # UI config import/export tracking
    ui_config_applied        = reactiveVal(FALSE),
    ui_config_source         = reactiveVal("none"),
    ui_config_update_in_progress = reactiveVal(FALSE),
    ui_config_errors         = reactiveVal(list()),

    # Performance monitoring
    last_correction_time     = reactiveVal(NULL),
    correction_history       = reactiveVal(list())
  )
}

#' Reset all reactive state to initial defaults.
#'
#' @param state Named list of reactiveVal objects (from init_batch_effects_state).
#' @param debug_log Logging function.
cleanup_batch_effects_state <- function(state, debug_log) {
  debug_log("Executing [Batch effects] cleanup", 2)

  state$batchCounter(2)
  state$batch_inputs(list())
  state$ui_config_applied(FALSE)
  state$ui_config_source("none")
  state$ui_config_update_in_progress(FALSE)
  state$ui_config_errors(list())
  state$last_correction_time(NULL)
  state$correction_history(list())

  debug_log("[Batch effects] cleanup completed", 2)
}

#' Collect the current UI state for export / template saving.
#'
#' @param input   Shiny input object.
#' @param state   Named list of reactiveVal objects.
#' @param debug_log Logging function.
#' @return Named list with current UI settings, or NULL on error.
get_batch_effects_ui_state <- function(input, state, debug_log) {
  tryCatch({
    ui_state <- list(
      batch_method           = input$batch_method,
      imputation_method_batch = input$imputation_method_batch,
      transformation_batch   = input$transformation_batch,
      remove_imputed_batch   = input$remove_imputed_batch,
      batch_counter          = state$batchCounter(),
      batch_inputs           = state$batch_inputs()
    )

    debug_log("Current UI state collected for export", 2)
    debug_log(paste("  Method:", ui_state$batch_method), 2)
    debug_log(paste("  Imputation:", ui_state$imputation_method_batch), 2)
    debug_log(paste("  Transformation:", ui_state$transformation_batch), 2)
    debug_log(paste("  Remove imputed:", ui_state$remove_imputed_batch), 2)
    debug_log(paste("  Batch groups:", ui_state$batch_counter), 2)

    return(ui_state)

  }, error = function(e) {
    debug_log(paste("Error collecting UI state:", e$message), 1)
    return(NULL)
  })
}

#' Check whether batch correction is configured (method selected and groups defined).
#'
#' @param input   Shiny input object.
#' @param state   Named list of reactiveVal objects.
#' @param debug_log Logging function.
#' @return Logical TRUE/FALSE.
check_batch_correction_configured <- function(input, state, debug_log) {
  tryCatch({
    batch_method <- isolate({
      tryCatch({
        if (!is.null(input$batch_method)) input$batch_method else NULL
      }, error = function(e) {
        debug_log(paste("Error accessing batch_method:", e$message), 1)
        NULL
      })
    })

    batch_inputs_data <- isolate({
      tryCatch({
        state$batch_inputs()
      }, error = function(e) {
        debug_log(paste("Error accessing batch_inputs:", e$message), 1)
        NULL
      })
    })

    is_configured <- !is.null(batch_method) &&
      nzchar(batch_method) &&
      batch_method != "None" &&
      !is.null(batch_inputs_data) &&
      length(batch_inputs_data) > 0

    debug_log(paste("Batch correction configured:", is_configured), 2)
    return(is_configured)

  }, error = function(e) {
    debug_log(paste("Error checking batch correction configuration:", e$message), 1)
    return(FALSE)
  })
}

#' Generate a human-readable batch correction summary string.
#'
#' @param input   Shiny input object.
#' @param state   Named list of reactiveVal objects.
#' @param debug_log Logging function.
#' @return Character string summarizing the current configuration.
get_batch_effects_summary <- function(input, state, debug_log) {
  tryCatch({
    if (!check_batch_correction_configured(input, state, debug_log)) {
      return("No batch correction configured")
    }

    method <- isolate({
      tryCatch({
        if (!is.null(input$batch_method)) input$batch_method else "Unknown"
      }, error = function(e) {
        debug_log(paste("Error getting method for summary:", e$message), 1)
        "Unknown"
      })
    })

    groups <- isolate({
      tryCatch({
        groups_data <- state$batch_inputs()
        if (!is.null(groups_data)) length(groups_data) else 0
      }, error = function(e) {
        debug_log(paste("Error getting groups for summary:", e$message), 1)
        0
      })
    })

    summary_text <- paste("Method:", method, "| Groups:", groups)
    debug_log(paste("Batch summary generated:", summary_text), 2)
    return(summary_text)

  }, error = function(e) {
    debug_log(paste("Error getting batch summary:", e$message), 1)
    return("Batch correction: Error getting summary")
  })
}

#' Defensive readiness check for programmatic (central) batch correction apply.
#'
#' @param input       Shiny input object.
#' @param state       Named list of reactiveVal objects.
#' @param get_file_data Function to retrieve current data.
#' @param debug_log   Logging function.
#' @return List with \code{success} (logical) and \code{message} (character).
apply_batch_correction_check <- function(input, state, get_file_data, debug_log) {
  tryCatch({
    debug_log("Central apply batch correction called", 2)

    if (!check_batch_correction_configured(input, state, debug_log)) {
      debug_log("No batch correction configured for central apply", 2)
      return(list(success = TRUE, message = "No batch correction configured"))
    }

    current_data <- get_file_data()
    if (is.null(current_data)) {
      debug_log("No data available for central batch correction", 1)
      return(list(success = FALSE, message = "No data available"))
    }

    correction_method <- isolate({
      tryCatch({
        if (!is.null(input$batch_method)) input$batch_method else "ComBat"
      }, error = function(e) {
        debug_log(paste("Error getting batch method:", e$message), 1)
        "ComBat"
      })
    })

    batch_groups <- isolate({
      tryCatch({
        state$batch_inputs()
      }, error = function(e) {
        debug_log(paste("Error getting batch groups:", e$message), 1)
        list()
      })
    })

    if (is.null(correction_method) || length(batch_groups) == 0) {
      debug_log("Batch correction not properly configured for central apply", 1)
      return(list(success = FALSE, message = "Batch correction not properly configured"))
    }

    debug_log(paste("Batch correction ready for central apply:", correction_method), 2)
    return(list(success = TRUE, message = paste("Batch correction ready:", correction_method)))

  }, error = function(e) {
    debug_log(paste("Error in apply_batch_correction:", e$message), 1)
    return(list(success = FALSE, message = paste("Batch correction error:", e$message)))
  })
}

#' Run a module health check and return a diagnostic status list.
#'
#' @param input       Shiny input object.
#' @param state       Named list of reactiveVal objects.
#' @param debug_log   Logging function.
#' @param DEBUG_LEVEL Current debug level integer.
#' @return Named list with health status fields.
run_batch_effects_health_check <- function(input, state, debug_log, DEBUG_LEVEL) {
  tryCatch({
    is_configured <- check_batch_correction_configured(input, state, debug_log)

    health_status <- list(
      module_name                 = "Batch Effects",
      status                      = "OK",
      batch_correction_configured = is_configured,
      ui_config_applied           = state$ui_config_applied(),
      ui_config_source            = state$ui_config_source(),
      error_count                 = length(state$ui_config_errors()),
      batch_groups                = state$batchCounter(),
      debug_level                 = DEBUG_LEVEL,
      performance_data            = !is.null(state$last_correction_time())
    )

    warnings <- character()
    if (!health_status$batch_correction_configured) {
      warnings <- c(warnings, "Batch correction not configured")
    }
    if (health_status$error_count > 0) {
      warnings <- c(warnings, paste("UI config errors:", health_status$error_count))
    }
    if (health_status$batch_groups < 2) {
      warnings <- c(warnings, "Insufficient batch groups for meaningful correction")
    }

    health_status$warnings <- warnings
    health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

    debug_log(paste("Module health check - Status:", health_status$overall_health), 2)
    return(health_status)

  }, error = function(e) {
    debug_log(paste("Error in module health check:", e$message), 1)
    return(list(
      module_name    = "Batch Effects",
      status         = "ERROR",
      error_message  = e$message,
      overall_health = "Critical"
    ))
  })
}
