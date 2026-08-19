# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_state.R
# Purpose:
#   Provide the tables state portion of the Data Wizard without changing public behavior.
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
# File: modules/Data Wizard/tables/datawizard_tables_state.R
#
# Purpose:
#   Centralizes all reactive state for the Tables submodule of the Data Wizard
#   in a single factory function. This is the single source of truth for all
#   tables reactive containers.
#
# Architectural Role:
#   State layer of the tables module. Called once from modDataTablesServer()
#   during initialization. The returned list is passed to
#   register_tables_observers() so that all reactive instances are shared
#   across the module without re-creation.
#
# Structure:
#   1. create_tables_state() - Factory function returning:
#      - current_handson_metadata  reactiveVal: metadata being edited by user
#      - metadata_options_refresh reactiveVal: condition dropdown refresh counter
#      - metadata_notif_state      environment: throttle state for notifications
#
# Notes for future developers:
#   - This file must contain no observers, no renderUI, and no side effects.
#     It only creates reactive containers and wires derived reactives.
#   - The factory must be called INSIDE moduleServer() so that the reactive
#     context is active.
#   - debug_log must be defined in the calling environment and passed in before
#     create_tables_state() is invoked. It is accepted as an argument to allow
#     future logging without coupling this file to a specific closure.
#   - metadata_notif_state is a plain environment (not reactive) used as a
#     mutable counter for notification throttling. It must not be confused
#     with reactive state.
# ==============================================================================


#' Create all reactive state for the tables module.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return A named list containing all reactive containers and helper state.
create_tables_state <- function(debug_log) {

  # --------------------------------------------------------------------------
  # Metadata being actively edited by the user in the rHandsontable widget.
  # Initialized from metadata_skeleton() by an observer in the observer file.
  # --------------------------------------------------------------------------

  current_handson_metadata <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # Refresh counter for metadata condition dropdown options. Incremented by the
  # assign_rules integration observer so the rHandsontable can re-render when
  # available condition choices change without rewriting unchanged metadata.
  # --------------------------------------------------------------------------

  metadata_options_refresh <- reactiveVal(0L)

  # --------------------------------------------------------------------------
  # Notification throttle state.
  # Tracks the last time a "Metadata updated" notification was shown and
  # counts suppressed rapid-edit events. Plain environment, not reactive.
  # --------------------------------------------------------------------------

  metadata_notif_state <- local({
    e <- new.env(parent = emptyenv())
    e$last      <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
    e$suppressed <- 0L
    e
  })

  # --------------------------------------------------------------------------
  # Guard flag for metadata write-back.
  # When TRUE, the skeleton observer skips overwriting the local metadata to
  # prevent feedback loops caused by back-syncing manual edits to core state.
  #
  # This is a plain environment flag (not reactive) because Shiny observers
  # fire in a deferred flush cycle. A reactiveVal would reset to FALSE in the
  # finally-block before the skeleton observer runs, making the guard useless.
  # The flag is set before write-back, and cleared by the skeleton observer
  # itself after it has skipped one invalidation.
  # --------------------------------------------------------------------------

  metadata_write_back_guard <- local({
    e <- new.env(parent = emptyenv())
    e$active <- FALSE
    e
  })

  # --------------------------------------------------------------------------
  # Return all state as a named list
  # --------------------------------------------------------------------------

  list(
    current_handson_metadata   = current_handson_metadata,
    metadata_options_refresh   = metadata_options_refresh,
    metadata_notif_state       = metadata_notif_state,
    metadata_write_back_guard  = metadata_write_back_guard
  )
}
