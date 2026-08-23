# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_safe_ui.R
# Purpose:
#   Provide the core safe ui portion of the Data Wizard without changing public behavior.
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

#' Create safe UI system for robust module UI updates
#' @param session Shiny session object
#' @param module_name Name of the module (for debugging)
#' @param debug_level Debug level (1 or 2)
#' @return List of safe UI functions
create_safe_ui_system <- function(session, module_name, debug_level = 0) {

  # Helper function for controlled debug output
  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, module_name, message)
    } else {
      effective_level <- tryCatch(
        get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
        error = function(e) debug_level
      )
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ ", module_name, " ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  # Safe input update function
  update_input_safely <- function(input_id, value, input_type = "selectInput") {
    tryCatch({
      # Try to freeze reactive value first (only works in reactive context)
      tryCatch({
        freezeReactiveValue(session$input, input_id)
      }, error = function(e) {
        debug_log(paste("Could not freeze reactive value for", input_id), 2)
      })

      # Apply the update based on input type
      switch(input_type,
             "selectInput" = updateSelectInput(session, input_id, selected = value),
             "textInput" = updateTextInput(session, input_id, value = value),
             "numericInput" = updateNumericInput(session, input_id, value = value),
             "checkboxInput" = updateCheckboxInput(session, input_id, value = value),
             "radioButtons" = updateRadioButtons(session, input_id, selected = value),
             "checkboxGroupInput" = updateCheckboxGroupInput(session, input_id, selected = value),
             # Default fallback
             {
               debug_log(paste("Unknown input type:", input_type, "using selectInput"), 1)
               updateSelectInput(session, input_id, selected = value)
             }
      )

      debug_log(paste("Successfully updated", input_id, "to", value), 2)
      return(TRUE)

    }, error = function(e) {
      debug_log(paste("Error updating", input_id, ":", e$message), 1)
      return(FALSE)
    })
  }

  # Safe notification function
  show_notification_safely <- function(message, type = "message", duration = 3) {
    tryCatch({
      showNotification(message, type = type, duration = duration)
    }, error = function(e) {
      debug_log(paste("Notification:", message, "(", type, ")"), 1)
    })
  }

  #' Schedule an interactive UI update after the current Shiny flush.
  #'
  #' This helper is not a restore dispatcher. Restore owners must use the shared
  #' restore callback runner so generation, phase, owner, and job identity are
  #' retained. Requiring caller intent here keeps scheduling provenance explicit
  #' instead of trying to infer it from Shiny's runtime state.
  #' @param update_function interactive update callback accepting `config`
  #' @param config configuration passed to `update_function`
  #' @param caller_intent must be the explicit string `"interactive"`
  execute_when_ready <- function(update_function, config, caller_intent) {
    if (missing(caller_intent) || !identical(caller_intent, "interactive")) {
      stop("execute_when_ready() is interactive-only; set caller_intent = \"interactive\"")
    }
    if (!is.function(update_function)) {
      stop("update_function must be a function")
    }

    debug_log("Scheduling interactive UI update after flush", 2)
    session$onFlushed(function() {
      tryCatch({
        update_function(config)
      }, error = function(e) {
        debug_log(paste("Interactive post-flush update failed:", e$message), 1)
      })
    }, once = TRUE)
  }

  # Return the safe UI system
  list(
    update_input_safely = update_input_safely,
    show_notification_safely = show_notification_safely,
    execute_when_ready = execute_when_ready,
    debug_log = debug_log
  )
}
