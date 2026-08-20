# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_context.R
# Purpose:
#   Provide the tables observer context portion of the Data Wizard without changing public behavior.
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

# Shared observer context for the Data Wizard tables module.
#
# The canonical metadata objects deliberately remain references to `state`.
# Incoming revision reactives are also stored without wrapping them, preserving
# their debounce and invalidation contracts.
create_tables_observer_context <- function(input, output, session, ns, state,
                                           primary_data, additional_data,
                                           metadata_skeleton, metadata_final,
                                           filter_applied, data_modified,
                                           modules_list, rv,
                                           set_metadata = NULL,
                                           set_primary_data = NULL,
                                           set_additional_data = NULL,
                                           record_modification = NULL,
                                           primary_working_revision_debounced,
                                           metadata_revision_debounced,
                                           metadata_content_signature_debounced,
                                           secondary_revision_debounced,
                                           debug_log) {
  context <- new.env(parent = environment())
  bindings <- as.list(environment())
  bindings$context <- NULL
  list2env(bindings, envir = context)

  # Preserve the single state instance; do not recreate canonical metadata,
  # option refresh, notification throttle, or the deferred write-back guard.
  context$current_handson_metadata <- state$current_handson_metadata
  context$metadata_options_refresh <- state$metadata_options_refresh
  context$metadata_notif_state <- state$metadata_notif_state
  context$metadata_write_back_guard <- state$metadata_write_back_guard

  # Every additional mutable/reactive container is instantiated exactly once.
  context$suppress_metadata_edit_echo <- reactiveVal(FALSE)
  context$programmatic_metadata_update_active <- reactiveVal(FALSE)
  context$suppress_next_final_metadata_sync <- reactiveVal(FALSE)
  context$metadata_sync_pending <- reactiveVal(FALSE)
  context$metadata_sync_last_error <- reactiveVal("")
  context$primary_remove_in_progress <- reactiveVal(FALSE)
  context$additional_remove_in_progress <- reactiveVal(FALSE)
  context$primary_table_rendered <- reactiveVal(FALSE)
  context$additional_table_rendered <- reactiveVal(FALSE)
  context$primary_show_full_table <- reactiveVal(FALSE)
  context$primary_preview_render_revision <- reactiveVal(0L)
  context$additional_show_full_table <- reactiveVal(FALSE)
  context$primary_serialization_dispatched_at <- reactiveVal(NULL)
  context$table_duplicate_counts <- new.env(parent = emptyenv())

  context
}
