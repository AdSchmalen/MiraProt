# ==============================================================================
# File: modules/sampleids/sampleids_state.R
#
# Purpose:
#   Centralizes all reactive state for the Sample IDs module in a single
#   factory function. This is the single source of truth for all reactive
#   containers used by the module.
#
# Architectural Role:
#   State layer of the Sample IDs module. Called once from modSampleIDsServer()
#   during initialization. The returned list is passed to
#   register_sampleids_observers() so that all parts of the module share the
#   same reactive instances.
#
# Structure:
#   1. create_sampleids_state() - Factory function that creates and returns:
#      - character_indices        reactiveVal: integer vector of character column
#                                             indices detected from data_mod.
#      - numeric_indices          reactiveVal: integer vector of numeric column
#                                             indices detected from data_mod.
#      - ggplot_object_SampleIDTab reactiveVal: current ggplot2 object (or NULL).
#      - plotly_object_SampleIDTab reactiveVal: current plotly object (or NULL).
#      - plot_ui_cache           reactiveVal: UI snapshot used for plot replay.
#
# Notes for future developers:
#   - This file is purely declarative. It must contain no observers, no
#     renderUI, and no side effects. It only creates reactive containers.
#   - The factory must be called INSIDE moduleServer() so that the reactive
#     graph context is available.
#   - If additional persistent module state is required in the future, add it
#     here and expose it via the returned list.
# ==============================================================================


#' Create all reactive state for the Sample IDs module.
#'
#' @return A named list containing all reactive containers for the module.
create_sampleids_state <- function() {
  list(
    character_indices         = reactiveVal(NULL),
    numeric_indices           = reactiveVal(NULL),
    ggplot_object_SampleIDTab = reactiveVal(NULL),
    plotly_object_SampleIDTab = reactiveVal(NULL),
    # Indicates whether a plot existed when the session snapshot was saved.
    # Used during restore to decide whether the module should regenerate a plot
    # after UI inputs have been synchronized.
    had_plot_on_save          = reactiveVal(FALSE),
    # Staged by set_session_state(); consumed by the session_restore_trigger
    # observer in sampleids_observer.R to push saved input values back into
    # the UI widgets after session restore.  Implemented as a reactiveVal so
    # that writes from set_session_state() are visible to the observer
    # closures (R's list assignment would otherwise create a local copy).
    pending_ui_inputs         = reactiveVal(NULL),
    # Restore-only cached pair of canonical data captured at plot creation/save.
    restore_plot_data_cache   = reactiveVal(NULL),
    # TRUE while the displayed plot is still the session-restored cached plot.
    # Manual Create Plot clears this so subsequent renders use live rv data.
    plot_from_restore_cache   = reactiveVal(FALSE),
    # Snapshot of the exact data_mod/data_def pair used when the current plot
    # was generated. Persisted so session save can export plot-faithful data.
    plot_creation_cache      = reactiveVal(NULL),
    # UI snapshot captured at plot creation time. Used by session save/restore
    # and cached plot replay paths in sampleids_module.R and sampleids_observer.R.
    plot_ui_cache            = reactiveVal(NULL),
    # Lightweight session schema 2.0 restore metadata. These never hold rendered
    # ggplot/plotly objects or duplicated data_mod/data_def payloads.
    plot_request             = reactiveVal(NULL),
    plot_data_cache_ref      = reactiveVal(NULL)
  )
}
