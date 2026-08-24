# =============================================================================
# modules/venn/venn_state.R
#
# Purpose:
#   Centralizes all reactive state for the Venn module in a single factory
#   function. This is the single source of truth for all reactive containers
#   used by the module.
#
# Architectural role:
#   State layer of the Venn module. Called once from modVennServer() during
#   initialization. The returned list is passed to register_venn_observers()
#   so that all parts of the module share the same reactive instances.
#
# File structure:
#   1. create_venn_state() - Factory function returning all reactive containers.
#
# Notes for future developers:
#   - This file must contain no observers, no renderUI, and no side effects.
#     It only creates reactive containers.
#   - The factory must be called inside moduleServer() so that the reactive
#     graph context is available.
#   - Add new persistent module state here and expose it via the returned list.
# =============================================================================


#' Create all reactive state for the Venn module
#'
#' @return A named list containing all reactive containers for the module.
create_venn_state <- function() {
  list(
    list_count_Venn           = reactiveVal(3),
    list_data_Venn            = reactiveValues(
      names  = list("List1", "List2", "List3"),
      lists  = list("", "", ""),
      colors = list("#EFC000FF", "#CD534CFF", "#0073C2FF"),
      gsea   = list(NULL, NULL, NULL),
      go     = list(NULL, NULL, NULL),
      sample = list(NULL, NULL, NULL),
      core_enriched = list(TRUE, TRUE, TRUE)
    ),
    intersection_list         = reactiveVal(NULL),
    num_intersections_export  = reactiveVal(NULL),
    go_results_for_extraction = reactiveVal(NULL),
    current_venn_plot         = reactiveVal(NULL),
    current_plot_type         = reactiveVal(NULL),
    plot_active               = reactiveVal(FALSE),
    # Staged pair (data_mod/data_def) used to rebuild from snapshot context
    # during session restore. This is referenced by schema 2.0 through
    # plot_data_cache_ref; rendered plot/grid objects are intentionally not
    # persisted and must be recreated from this data plus set_definitions.
    # Written by set_session_state().
    restore_plot_data_cache   = reactiveVal(NULL),
    # Canonical data pair used to create the current compact/rendered result.
    # Unlike restore_plot_data_cache this survives successful reconstruction.
    plot_creation_cache       = reactiveVal(NULL),
    # Canonical plot intent staged by set_session_state(). The restore-trigger
    # observer must read this before registering its plot-rebuild job: FALSE
    # restores only list/UI state, while TRUE enters the armed polling flow.
    had_plot_on_save          = reactiveVal(FALSE),
    # Staging area for captured UI inputs during session restore.
    # Written by set_session_state(), consumed by session_restore_trigger observer.
    # Implemented as a reactiveVal so writes from set_session_state() are
    # visible to the observer closures (R's plain list assignment would
    # otherwise create a local copy in the closure that wrote to it).
    pending_ui_inputs         = reactiveVal(NULL),
    # Snapshot of UI inputs captured when a plot cache was successfully built.
    # Used by session save as authoritative fallback when live inputs were
    # reset by later data/metadata changes.
    last_plot_ui_inputs       = reactiveVal(NULL),
    # Restore policy: when TRUE and a plot existed at save-time, the restore
    # flow prefers cached snapshot data and treats live fallback as degraded.
    restore_require_cached_data = reactiveVal(TRUE),
    # Structured diagnostics for the latest restore attempt.
    last_restore_report       = reactiveVal(NULL),
    restored_plot_cache       = reactiveVal(NULL)
  )
}
