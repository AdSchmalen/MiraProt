# ============================================================================
# Sub-Script: Data Wizard Edit Reactive State
#
# Purpose:
#   Own all reactive objects used by the Edit module and provide a single,
#   reusable state container for the orchestrator and internal sub-scripts.
#
# Architectural Role:
#   Source-of-truth state layer. This file defines and initializes every
#   reactiveVal and shared constant used across the Edit module. No observer
#   registration, no UI rendering, no external module calls.
#
# Structure:
#   1. create_empty_operations_df()  -- factory for the canonical empty
#      operations data frame (6 columns). Used wherever the queue needs
#      to be initialized or cleared.
#   2. create_default_columns_info() -- factory for the default column-type
#      info list. Used at init and after reset.
#   3. create_edit_reactive_state()  -- main factory that returns a named
#      list of all reactiveVals and constants. Called once from the
#      orchestrator inside moduleServer.
#   4. reset_edit_state()            -- resets all reactiveVals to their
#      initial values. Called by session cleanup.
#
# Future Developer Notes:
#   - If you add a new reactiveVal to the Edit module, declare it HERE and
#     expose it through the state list. Do NOT declare reactiveVals inline
#     in the orchestrator or in handler/output files.
#   - The state list is destructured in the orchestrator so that handlers,
#     outputs, and API code can reference variables directly by name.
#   - keep create_empty_operations_df() in sync with any schema changes to
#     the operations queue (e.g. adding new columns).
# ============================================================================


#' Create an empty operations data frame with the canonical schema
#'
#' This is the single source of truth for the operations queue structure.
#' Used at initialization, after clearing the queue, and during cleanup.
#'
#' @return data.frame with zero rows and the 6 standard columns
create_empty_operations_df <- function() {
  data.frame(
    Operation   = character(),
    Type        = character(),
    Columns     = character(),
    Parameters  = character(),
    Description = character(),
    Executed    = logical(),
    stringsAsFactors = FALSE
  )
}


#' Create the default column-type information list
#'
#' Returned when no columns are selected or after a state reset.
#'
#' @return named list with default column info fields
create_default_columns_info <- function() {
  list(
    overall_type     = "unknown",
    individual_types = character(0),
    type_summary     = "No columns selected",
    compatible       = FALSE,
    existing_columns = character(0),
    last_updated     = NULL
  )
}


#' Create the full Edit module reactive state
#'
#' Allocates and returns every reactiveVal and constant used by the module.
#' The orchestrator destructures the returned list into local scope so that
#' handlers, outputs, and API code can reference them by name.
#'
#' @return named list of reactiveVals and constants
create_edit_reactive_state <- function() {
  list(
    # --- Data integrity ---
    original_data      = reactiveVal(NULL),
    original_data_hash = reactiveVal(NULL),

    # --- Operations queue ---
    pending_operations = reactiveVal(create_empty_operations_df()),

    # --- Column type detection ---
    current_data_column_signature = reactiveVal(NULL),
    selected_columns_info         = reactiveVal(create_default_columns_info()),
    column_type_cache             = reactiveVal(list()),

    # --- Template import tracking ---
    operations_table_applied            = reactiveVal(FALSE),
    operations_table_source_info        = reactiveVal("none"),
    operations_table_update_in_progress = reactiveVal(FALSE),
    operations_table_errors             = reactiveVal(list()),

    # --- Performance monitoring ---
    last_operation_time  = reactiveVal(NULL),
    operation_performance = reactiveVal(list()),

    # --- Constants ---
    max_log_entries = 100
  )
}


#' Reset all edit module reactive state to initial values
#'
#' Called during session cleanup to release memory and prevent stale state.
#'
#' @param state named list as returned by create_edit_reactive_state()
#' @param debug_log logging function (optional)
reset_edit_state <- function(state, debug_log = function(...) {}) {
  debug_log("Resetting edit module reactive state", 2)

  state$original_data(NULL)
  state$original_data_hash(NULL)

  state$pending_operations(create_empty_operations_df())

  state$current_data_column_signature(NULL)
  state$selected_columns_info(create_default_columns_info())
  state$column_type_cache(list())

  state$operations_table_applied(FALSE)
  state$operations_table_source_info("none")
  state$operations_table_update_in_progress(FALSE)
  state$operations_table_errors(list())

  state$last_operation_time(NULL)
  state$operation_performance(list())

  debug_log("Edit module reactive state reset complete", 2)
}
