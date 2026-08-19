# ============================================================================
# Orchestrator: Data Wizard Edit Module
#
# Purpose:
#   Central coordination file for the Edit module. Sources all sub-scripts,
#   defines modEditServer(), and wires together the module's layers:
#   reactive state -> outputs -> handlers -> cleanup -> API return.
#
# Architecture:
#   This file is intentionally thin. All domain logic lives in sub-scripts
#   under modules/Data Wizard/edit/:
#
#   datawizard_edit_reactive_state.R  – Reactive state factory + reset
#   datawizard_edit_utils.R           – Pure functions (type detection,
#                                       validation, operation application,
#                                       parameter serialization)
#   datawizard_edit_UI.R              – modEditUI() Shiny UI definition
#   datawizard_edit_outputs.R         – All output$ render functions
#   datawizard_edit_handlers.R        – All observe/observeEvent + internal
#                                       helpers (mark_operation_executed,
#                                       add_operation_to_queue)
#   datawizard_edit_api.R             – External API functions + return-list
#                                       builder (the module's public contract)
#
# Data Flow:
#   1. modEditServer() creates reactive state via create_edit_reactive_state()
#   2. State variables are destructured into local scope
#   3. register_edit_outputs()  binds output renderers
#   4. register_edit_handlers() registers observers and event handlers
#   5. cleanup_manager registers session teardown
#   6. register_edit_api()      defines API functions and builds the
#                                return list (module's public interface)
#
# Shared Environment Pattern:
#   Each register_*() function receives environment() and uses evalq()
#   to inject its definitions into this moduleServer scope. This gives
#   all sub-scripts transparent access to input, session, ns, get_data,
#   set_data, metadata_def, operations_table, debug_log, DEBUG_LEVEL,
#   and all destructured reactive state variables.
#
# Dependencies:
#   External: digest (MD5 hashing), shiny, DT
#   Internal: cleanup_manager (from app framework)
# ============================================================================

# Source sub-module files
source("modules/Data Wizard/edit/datawizard_edit_reactive_state.R", local = modEnv)
source("modules/Data Wizard/edit/datawizard_edit_utils.R", local = modEnv)
source("modules/Data Wizard/edit/datawizard_edit_UI.R", local = modEnv)
source("modules/Data Wizard/edit/datawizard_edit_outputs.R", local = modEnv)
source("modules/Data Wizard/edit/datawizard_edit_handlers.R", local = modEnv)
source("modules/Data Wizard/edit/datawizard_edit_api.R", local = modEnv)

#' Data Editing Module Server - Enhanced with Operations Table Import and Stability
#'
#' Server logic for data replacement and editing operations with template import support
#' @param id module namespace id
#' @param get_data function to get current data
#' @param set_data function to set updated data
#' @param metadata_def reactive containing metadata definition
#' @param operations_table reactive containing operations table for import (OPTIONAL)
#' @param has_data optional function returning TRUE when primary/processed data is available
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modEditServer <- function(id, get_data, set_data, metadata_def,
                          operations_table = reactive(NULL),
                          session_restore_trigger = reactive(NULL),
                          has_data = NULL,
                          debug_level = 0,
                          data_revision_signature = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper function for controlled debug output
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "EDIT", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ EDIT ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Edit module server starting", 1)

    # ========================================
    # Reactive State (from datawizard_edit_reactive_state.R)
    # ========================================

    state <- create_edit_reactive_state()

    # Destructure into local scope for direct access by handlers, outputs, and API
    original_data                       <- state$original_data
    original_data_hash                  <- state$original_data_hash
    pending_operations                  <- state$pending_operations
    current_data_column_signature       <- state$current_data_column_signature
    selected_columns_info               <- state$selected_columns_info
    column_type_cache                   <- state$column_type_cache
    operations_table_applied            <- state$operations_table_applied
    operations_table_source_info        <- state$operations_table_source_info
    operations_table_update_in_progress <- state$operations_table_update_in_progress
    operations_table_errors             <- state$operations_table_errors
    last_operation_time                 <- state$last_operation_time
    operation_performance               <- state$operation_performance
    max_log_entries                     <- state$max_log_entries

    # ========================================
    # UI Outputs (from datawizard_edit_outputs.R)
    # ========================================

    register_edit_outputs(environment())

    # ========================================
    # Handlers (from datawizard_edit_handlers.R)
    # ========================================

    register_edit_handlers(environment())

    # ========================================
    # Session Cleanup
    # ========================================

    cleanup_manager$register_module("Edit", function() {
      debug_log("Executing [Edit] cleanup", 2)
      reset_edit_state(state, debug_log)
      debug_log("[Edit] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    edit_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        category_select = "selectInput",
        column_select   = "selectInput"
      ),
      module_label = "Edit",
      get_extra = function() {
        list(pending_operations = tryCatch(isolate(pending_operations()),
                                           error = function(e) NULL))
      },
      apply_extra = function(extra) {
        if (is.list(extra) && is.data.frame(extra$pending_operations)) {
          pending_operations(extra$pending_operations)
          debug_log(paste("[Edit] restored pending_operations queue with",
                          nrow(extra$pending_operations), "rows"), 2)
        }
      },
      restore_trigger = session_restore_trigger
    )

    # ========================================
    # API & Return Interface (from datawizard_edit_api.R)
    # ========================================

    api <- register_edit_api(environment())
    api$get_session_state <- edit_session_state$get_session_state
    api$set_session_state <- edit_session_state$set_session_state
    return(api)
  })
}
