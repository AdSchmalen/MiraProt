# ==============================================================================
# volcano_reactive_state.R
# ==============================================================================
#
# PURPOSE:
#   Centralizes all reactive state for the volcano module. Provides a single
#   initialization function that creates and returns all reactiveValues and
#   reactiveVal objects used across the volcano module's sub-scripts.
#
# ARCHITECTURAL ROLE:
#   Reactive State -- the single source of truth for all mutable state within
#   the volcano module. Every sub-script that needs to read or write reactive
#   state must use the objects returned by init_volcano_state().
#
# RESPONSIBILITIES:
#   - Define and initialize all reactiveValues (volcano_state)
#   - Define and initialize all reactiveVal objects (labels, selections, triggers)
#   - Provide init_volcano_state() as the sole entry point for state creation
#   - Provide reset_volcano_labeling_system() for labeling state reset
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Side effects (showNotification, runjs, etc.)
#   - Business logic or data processing functions
#   - Direct references to input, output, or session
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - None (this is a leaf dependency; other scripts depend on it)
#   External packages:
#     - shiny: reactiveValues, reactiveVal
#
# INTERACTIONS:
#   Called by:
#     - volcano_module.R: calls init_volcano_state() during server initialization
#   Calls into:
#     - None
#   Data flow:
#     - IN:  None
#     - OUT: Named list of all reactive state objects
#
# LAST UPDATED: 2026-03-03
# ==============================================================================

#' Initialize all reactive state for the volcano module
#'
#' Creates and returns a named list containing every reactiveValues and
#' reactiveVal object used by the volcano module.  The orchestrator
#' (volcano_module.R) calls this once during server startup and passes
#' the returned list to all sub-scripts that need reactive state.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{volcano_state}{reactiveValues -- core plot and UI state}
#'     \item{plot_update_trigger}{reactiveVal(0) -- counter to force plot re-render}
#'     \item{selected_data_Volcano}{reactiveVal(data.frame()) -- full data rows of selected proteins}
#'     \item{selected_protein_vector_Volcano}{reactiveVal(character()) -- vector of selected protein IDs}
#'     \item{volcano_original_plots}{reactiveVal(list()) -- deep copies of plots before label application}
#'     \item{volcano_labels}{reactiveVal(list()) -- per-plot label data (plot_title -> data.frame)}
#'     \item{protein_label_settings}{reactiveVal(data.frame()) -- per-protein color/dot settings}
#'     \item{selected_points_interactive}{reactiveVal(data.frame()) -- proteins selected via plotly interaction}
#'   }
init_volcano_state <- function() {

  state <- list()

  # --------------------------------------------------------------------------
  # Core plot and UI state
  # --------------------------------------------------------------------------
  state$volcano_state <- reactiveValues(
    plot_titles                         = NULL,
    static_plots                        = NULL,
    plot_axis_settings                  = NULL,
    plot_axis_overrides                 = list(),
    current_plotly_data                 = NULL,
    current_pairs                       = NULL,
    selected_genes                      = character(),
    label_storage                       = NULL,
    auto_range_set                      = FALSE,
    manual_axis_override                = FALSE,
    auto_axis_update_in_progress        = FALSE,
    plot_creation_in_progress           = FALSE,
    plot_selection_update_in_progress   = FALSE,
    plot_selection_update_pending_value = NULL,
    axis_update_suppressed_inputs       = character(0),
    had_static_plots_on_save            = FALSE,
    restore_in_progress                 = FALSE,
    preferred_plot_title                = NULL,
    plot_requests_by_title              = NULL,
    plot_data_by_title                  = NULL,
    plot_cache_ref_by_title             = NULL,
    preplot_skip_log_signature          = NULL,
    preplot_metadata_skip_logged        = FALSE,
    preplot_data_skip_logged            = FALSE
  )

  # Initialize label storage structure
  state$volcano_state$label_storage <- list(
    labels         = vector("list", 20),
    selected_genes = character(),
    last_update    = Sys.time()
  )

  # --------------------------------------------------------------------------
  # Plot update trigger (counter-based invalidation)
  # --------------------------------------------------------------------------
  state$plot_update_trigger <- reactiveVal(0)

  # --------------------------------------------------------------------------
  # Protein selection state
  # --------------------------------------------------------------------------
  state$selected_data_Volcano           <- reactiveVal(data.frame())
  state$selected_protein_vector_Volcano <- reactiveVal(character())

  # --------------------------------------------------------------------------
  # Label management state
  # --------------------------------------------------------------------------

  # Deep copies of plots before any labels are applied

  state$volcano_original_plots <- reactiveVal(list())

  # Per-plot label data: named list where names are plot titles,
  # values are data.frames with columns: ID, Abundance_Ratio,
  # Adjusted_p_Value, LabelColor, x_plot, y_plot, and optionally
  # UseCustomDotColor, CustomDotColor
  state$volcano_labels <- reactiveVal(list())

  # Per-protein visual settings for the labeling UI
  state$protein_label_settings <- reactiveVal(data.frame(
    protein_id         = character(),
    label_color        = character(),
    dot_color          = character(),
    use_custom_dot_color = logical(),
    stringsAsFactors   = FALSE
  ))

  # --------------------------------------------------------------------------
  # Interactive selection state (from plotly click/lasso/box)
  # --------------------------------------------------------------------------
  state$selected_points_interactive <- reactiveVal(data.frame())

  return(state)
}

#' Reset the labeling sub-system to its initial state
#'
#' Clears all labels and original plot copies.  Called during
#' labeling system initialization and when the user explicitly
#' resets labels.
#'
#' @param state The state list returned by init_volcano_state()
reset_volcano_labeling_system <- function(state) {
  state$volcano_labels(list())
  state$volcano_original_plots(list())
}
