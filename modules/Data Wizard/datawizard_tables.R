# ============================================================================
# MiraProt File Contract: modules/Data Wizard/datawizard_tables.R
# Purpose:
#   Provide the tables portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Compatibility loader and public Tables module composition root.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Exactly one Tables context owns table-local state per module session; canonical metadata/data stay with injected owners.
# Mutation Authority:
#   Mutations flow only through observers registered against that context and existing injected setter callbacks.
# Source-Order Assumptions:
#   Logic/state/context and phase implementations load before the observer coordinator and UI; consumers source only this path.
# Session/Restore Implications:
#   Public Tables outputs and canonical-state rehydration behavior remain unchanged.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# ==============================================================================
# File: modules/Data Wizard/datawizard_tables.R
#
# Purpose:
#   Orchestrator for the Data Wizard Tables submodule. Sources all sub-files,
#   defines the UI wrapper (modDataTablesUI), and wires the server function
#   (modDataTablesServer). This is the only file in the module that contains
#   a server function.
#
# Architectural Role:
#   This file delegates concerns to six sub-files inside tables/:
#     - datawizard_tables_logic.R    : Pure logic functions (no Shiny dependency)
#     - datawizard_tables_state.R    : Reactive state factory (create_tables_state)
#     - datawizard_tables_observer_context.R   : Shared observer closure state
#     - datawizard_tables_observer_metadata.R  : Metadata hydration/editing unit
#     - datawizard_tables_observer_rendering.R : Other output/event registrations
#     - datawizard_tables_observer.R           : Observer coordinator
#     - datawizard_tables_ui.R       : Static UI layout and output placeholders
#
# Structure:
#   1. Source sub-files into modEnv
#   2. modDataTablesUI()    - UI wrapper calling datawizard_tables_UI()
#   3. modDataTablesServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_tables_state()
#      c. Observer registration via register_tables_observers()
#      d. Return interface
#
# Return Interface (public API):
#   current_metadata             : reactiveVal with user-edited metadata
#   current_handson_metadata     : alias for current_metadata (backward compat)
#   has_metadata                 : reactive; TRUE when editable metadata exists
#   has_final_metadata           : reactive; TRUE when final metadata exists
#   is_data_modified             : reactive; TRUE when data differs from raw
#   create_content_color_mapping : pure function exported for downstream use
#
# Notes for future developers:
#   - No server logic may live in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_tables_observers().
#   - debug_log is defined here and passed explicitly to every function that logs.
#   - The debug_log prefix is "[ TABLES" to distinguish tables output from other
#     module prefixes in the console.
#   - DEBUG_LEVEL is received as a parameter from the parent integration module
#     (datawizard_integration.R) so that the global debug verbosity is respected.
#   - The module accepts an optional debug_level parameter (default 1) so it can
#     be instantiated with a higher verbosity during development.
# ==============================================================================

source("modules/Data Wizard/tables/datawizard_tables_logic.R",    local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_state.R",    local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_observer_context.R", local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_observer_mutations.R", local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_observer_metadata.R", local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_observer_rendering.R", local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_observer.R", local = modEnv)
source("modules/Data Wizard/tables/datawizard_tables_ui.R",       local = modEnv)


# ==============================================================================
# UI
# ==============================================================================

#' Data Tables Module UI
#'
#' Creates UI components for displaying primary data, additional data, and
#' the editable metadata definition table.
#'
#' @param id Module namespace id.
#' @export
modDataTablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    datawizard_tables_UI(ns)
  )
}


# ==============================================================================
# SERVER
# ==============================================================================

#' Data Tables Module Server
#'
#' Server logic for displaying and managing data tables with metadata editing
#' capabilities.
#'
#' @param id               Module namespace id.
#' @param primary_data     Reactive returning the primary dataset (may be filtered).
#' @param additional_data  Reactive returning the additional dataset.
#' @param metadata_skeleton Reactive returning the baseline metadata for editing.
#' @param metadata_final   Reactive returning the final processed metadata.
#' @param filter_applied   Reactive returning a logical indicating active filters,
#'                         or NULL.
#' @param original_rows    Reactive returning original row count before filtering,
#'                         or NULL (retained for API compatibility, not currently used).
#' @param data_modified    Reactive returning a logical indicating data modification,
#'                         or NULL.
#' @param modules_list     Named list of other submodule outputs (used to read
#'                         condition_options from assign_rules_out), or NULL.
#' @param parent_session   Parent session object, or NULL (falls back to
#'                         session$parent).
#' @param rv               Main reactive data handler (reactiveValues), or NULL.
#' @param set_metadata     Optional callback function(new_meta) that writes
#'                         edited metadata back to the canonical core state.
#'                         When NULL, manual edits are not synced to core state.
#' @param record_modification Optional callback function(operation, details) that
#'                         records data modification history entries. Metadata-only
#'                         edits should not call this callback.
#' @param debug_level      Numeric verbosity level passed from the parent module.
#'                         1 = important information, 2 = verbose tracing.
#' @export
modDataTablesServer <- function(id, primary_data, additional_data,
                                metadata_skeleton, metadata_final,
                                filter_applied  = NULL,
                                original_rows   = NULL,
                                data_modified   = NULL,
                                modules_list    = NULL,
                                parent_session  = NULL,
                                rv              = NULL,
                                set_metadata    = NULL,
                                set_primary_data = NULL,
                                set_additional_data = NULL,
                                record_modification = NULL,
                                primary_working_revision_debounced = reactive(NULL),
                                metadata_revision_debounced = reactive(NULL),
                                metadata_content_signature_debounced = reactive(NULL),
                                secondary_revision_debounced = reactive(NULL),
                                debug_level     = 1) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    if (is.null(parent_session)) {
      parent_session <- session$parent
    }

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "TABLES", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ TABLES ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Tables module server starting", 1)

    # --------------------------------------------------------------------------
    # State initialization
    # --------------------------------------------------------------------------

    state <- create_tables_state(debug_log)

    current_handson_metadata <- state$current_handson_metadata

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------

    tables_api <- register_tables_observers(
      input             = input,
      output            = output,
      session           = session,
      ns                = ns,
      state             = state,
      primary_data      = primary_data,
      additional_data   = additional_data,
      metadata_skeleton = metadata_skeleton,
      metadata_final    = metadata_final,
      filter_applied    = filter_applied,
      data_modified     = data_modified,
      modules_list      = modules_list,
      rv                = rv,
      set_metadata      = set_metadata,
      set_primary_data  = set_primary_data,
      set_additional_data = set_additional_data,
      record_modification = record_modification,
      primary_working_revision_debounced = primary_working_revision_debounced,
      metadata_revision_debounced = metadata_revision_debounced,
      metadata_content_signature_debounced = metadata_content_signature_debounced,
      secondary_revision_debounced = secondary_revision_debounced,
      debug_log         = debug_log
    )

    debug_log("Tables module initialized successfully", 1)

    # --------------------------------------------------------------------------
    # Session Cleanup
    # --------------------------------------------------------------------------

    cleanup_manager$register_module("Tables", function() {
      debug_log("Executing [Tables] cleanup", 2)

      current_handson_metadata(NULL)

      debug_log("[Tables] cleanup completed", 2)
    })

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    list(
      current_metadata         = current_handson_metadata,
      current_handson_metadata = current_handson_metadata,
      set_current_metadata     = tables_api$set_current_metadata,
      refresh_primary_table_style = tables_api$refresh_primary_table_style,

      has_metadata = reactive({
        !is.null(current_handson_metadata()) &&
          nrow(current_handson_metadata()) > 0
      }),

      has_final_metadata = reactive({
        !is.null(metadata_final()) && nrow(metadata_final()) > 0
      }),

      is_data_modified = reactive({
        if (!is.null(data_modified)) {
          tryCatch({
            return(data_modified())
          }, error = function(e) {
            return(FALSE)
          })
        }
        current_data <- primary_data()
        if (is.null(current_data)) return(FALSE)
        any(grepl("^Imputed |^Batch Corrected |^Pivoted |^Merged ",
                  names(current_data)))
      }),

      create_content_color_mapping = create_content_color_mapping
    )
  })
}
