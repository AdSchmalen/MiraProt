# ==============================================================================
# File: modules/abundances/abundances_state.R
#
# Purpose:
#   Centralizes all reactive state for the Abundances module in a single
#   factory function. This is the single source of truth for all reactive
#   containers used by the module.
#
# Architectural Role:
#   State layer of the Abundances module. Called once from modAbundancesServer()
#   during initialization. The returned list is passed to
#   register_abundances_observers() so that all parts of the module share the
#   same reactive instances.
#
# Structure:
#   1. create_abundances_state() - Factory function that creates and returns:
#      - plot_object_abundanceTab    reactiveVal: current plotly object (or NULL)
#      - ggplot_object_abundanceTab  reactiveVal: current ggplot object (or NULL)
#
# Notes for future developers:
#   - This file is purely declarative. It must contain no observers, no
#     renderUI, and no side effects. It only creates reactive containers.
#   - The factory must be called INSIDE moduleServer() so that the reactive
#     graph context is available.
#   - If additional persistent module state is required in the future (e.g. a
#     reactive tracking the selected data type), add it here and expose it via
#     the returned list.
# ==============================================================================


#' Create all reactive state for the Abundances module.
#'
#' @return A named list containing all reactive containers for the module.
create_abundances_state <- function() {
  list(
    plot_object_abundanceTab   = reactiveVal(NULL),
    ggplot_object_abundanceTab = reactiveVal(NULL),
    # Indicates whether a plot existed when the session was saved.
    # Used during restore to decide whether plot regeneration is required.
    had_plot_on_save           = reactiveVal(FALSE),
    # Staged by set_session_state(); consumed by the session_restore_trigger
    # observer in abundances_observer.R to push saved input values back into
    # the UI widgets after session restore.  Implemented as a reactiveVal so
    # that writes from set_session_state() are visible to the observer
    # closures (R's list assignment would otherwise create a local copy).
    pending_ui_inputs          = reactiveVal(NULL),
    # Restore-only cached pair of canonical data captured at plot creation/save.
    restore_plot_data_cache   = reactiveVal(NULL),
    # TRUE while the displayed plot is still the session-restored cached plot.
    # Manual Create Plot clears this so subsequent renders use live rv data.
    plot_from_restore_cache   = reactiveVal(FALSE),
    # TRUE when session restore should rebuild from cached data/ui snapshots
    # without requiring live Data Wizard choices to be populated first.
    restore_mode_cached       = reactiveVal(FALSE),
    # Snapshot of the exact data_mod/data_def pair used when the current plot
    # was generated. Persisted so session save can export plot-faithful data.
    plot_creation_cache      = reactiveVal(NULL),
    # UI snapshot captured at plot creation time (for plot-faithful replay).
    plot_ui_cache            = reactiveVal(NULL),
    # Dedicated trigger used only by cached-restore rebuild observer path.
    restore_rebuild_nonce    = reactiveVal(0L),
    # Explicit render dependency trigger for renderPlot/renderPlotly.
    plot_render_nonce        = reactiveVal(0L)
  )
}
