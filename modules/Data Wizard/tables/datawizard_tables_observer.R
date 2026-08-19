# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer.R
# Purpose:
#   Provide the tables observer portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# ==============================================================================
# File: modules/Data Wizard/tables/datawizard_tables_observer.R
#
# Purpose:
#   Contains all observe(), observeEvent(), and output handler registrations
#   for the Tables submodule of the Data Wizard. Coordinates observer registration across the rendering, mutation, and
#   safety-critical metadata observer units.
#
# Architectural Role:
#   Observer layer of the tables module. Called from modDataTablesServer() via
#   register_tables_observers() after state is initialized. All observers run
#   inside the moduleServer() closure of the orchestrator. Pure logic is
#   delegated to functions from datawizard_tables_logic.R. Reactive state
#   comes from datawizard_tables_state.R via the `state` list argument.
#
# Structure:
#   1. register_tables_observers() - Registration function containing:
#      a. Metadata hydration registration phase
#      b. General table rendering/mutation registration
#      c. Metadata editing registration phase
#      d. output$metadata_sync_controls - renderUI
#      e. output$primary_table_info     - renderText
#      e. output$primary_table_preview_ui / keyed preview output - renderUI/renderDT
#      f. Additional data section visibility observer
#      g. output$additional_table_info  - renderText
#      h. output$additional_table_preview_ui / keyed preview output - renderUI/renderDT
#      i. output$metadata_table         - renderRHandsontable
#      j. Metadata edit handler         - observeEvent(input$metadata_table)
#
# Notes for future developers:
#   - Metadata hydration and editing stay together in the dedicated metadata
#     safety unit; all registrations share the same observer context.
#   - debug_log is received as a parameter and must be forwarded to any logic
#     function that requires logging.
#   - The condition observer is wrapped in tryCatch because modules_list may
#     be NULL when the tables module is used in a minimal context.
#   - Notification throttling uses metadata_notif_state from the state list.
#     The 3-second window prevents notification spam during rapid cell edits.
#   - Emojis are intentionally avoided in all user-facing strings to remain
#     consistent with the project style guide.
# ==============================================================================


#' Register all observers and output handlers for the tables module.
#'
#' @param input           Shiny input object from the moduleServer closure.
#' @param output          Shiny output object from the moduleServer closure.
#' @param session         Shiny session object.
#' @param ns              Namespace function for this module.
#' @param state           Named list from create_tables_state().
#' @param primary_data    Reactive returning the primary data frame.
#' @param additional_data Reactive returning the additional data frame.
#' @param metadata_skeleton Reactive returning the baseline metadata data frame.
#' @param metadata_final  Reactive returning the final processed metadata.
#' @param filter_applied  Reactive returning a logical, or NULL.
#' @param data_modified   Reactive returning a logical, or NULL.
#' @param modules_list    List of other module outputs, or NULL.
#' @param rv              Main reactive data handler (reactiveValues), or NULL.
#' @param set_metadata    Optional callback function(new_meta) that writes
#'                        edited metadata back to canonical core state, or NULL.
#' @param record_modification Optional callback function(operation, details) that
#'                        records data-changing table operations, or NULL.
#' @param debug_log       Logging function with signature (message, level).
register_tables_observers <- function(input, output, session, ns,
                                      state,
                                      primary_data, additional_data,
                                      metadata_skeleton, metadata_final,
                                      filter_applied, data_modified,
                                      modules_list, rv,
                                      set_metadata = NULL,
                                      set_primary_data = NULL,
                                      set_additional_data = NULL,
                                      record_modification = NULL,
                                      primary_working_revision_debounced = reactive(NULL),
                                      metadata_revision_debounced = reactive(NULL),
                                      metadata_content_signature_debounced = reactive(NULL),
                                      secondary_revision_debounced = reactive(NULL),
                                      debug_log) {
  context <- create_tables_observer_context(
    input = input, output = output, session = session, ns = ns, state = state,
    primary_data = primary_data, additional_data = additional_data,
    metadata_skeleton = metadata_skeleton, metadata_final = metadata_final,
    filter_applied = filter_applied, data_modified = data_modified,
    modules_list = modules_list, rv = rv, set_metadata = set_metadata,
    set_primary_data = set_primary_data, set_additional_data = set_additional_data,
    record_modification = record_modification,
    primary_working_revision_debounced = primary_working_revision_debounced,
    metadata_revision_debounced = metadata_revision_debounced,
    metadata_content_signature_debounced = metadata_content_signature_debounced,
    secondary_revision_debounced = secondary_revision_debounced, debug_log = debug_log
  )
  register_tables_rendering(context)
}
