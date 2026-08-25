# ==============================================================================
# File: modules/PCA/pca_module_state.R
#
# Purpose:
#   Defines the reactive state factory for the PCA module. Creates and returns
#   all reactiveVal instances and the accessor/storage helpers used by the
#   rest of the module.
#
# Architectural Role:
#   Called once from modPCAServer in pca_module.R. The returned list is the
#   canonical state object (pca_state) passed by reference to every
#   registration function in the module. No Shiny observers or outputs live
#   here; this file only declares reactive containers and pure helper
#   functions that operate on them.
#
# Structure:
#   1. init_pca_state(input, debug_log) - factory function
#      a. Reactive value declarations (all module-level state)
#      b. get_current_analysis_results - reactive that routes by method/target
#      c. get_all_analysis_results     - snapshot function (non-reactive)
#      d. store_analysis_results       - dispatch to per-type reactiveVals
#      e. debug_stored_results         - no-op placeholder for diagnostics
#      f. Return list of all state handles
#
# Notes for future developers:
#   - All reactive values are session-local and garbage-collected automatically
#     when the Shiny session ends. No explicit cleanup is required.
#   - debug_log is received as a parameter and must be forwarded from
#     modPCAServer; do not redefine it here.
#   - The legacy analysis_results reactiveVal is kept for backward
#     compatibility with register_pca_rendering_core and export handlers.
#   - If new analysis methods or targets are added, extend store_analysis_results
#     and get_current_analysis_results with the matching cases.
# ==============================================================================


# Session save/restore schema for PCA. Version 2.0 persists only serializable
# inputs, compact analysis coordinates/variance, plot request metadata, a plot
# data cache reference, and the had_plot flag. The additive plot_request
# label_state field has its own version (2.0), so this compatible extension does
# not require a PCA envelope version increase. Rendered ggplot/plotly objects are
# intentionally excluded and recreated after restore.
PCA_SESSION_SCHEMA_VERSION <- "2.0"

# `had_plot` is the canonical saved-plot intent in the PCA session envelope.
# `plots_ready` is retained only for snapshots written before that field became
# authoritative; importantly, the presence of compact analysis coordinates is
# not itself a request to recreate a plot.
pca_saved_plot_intent <- function(state) {
  if (!is.list(state)) return(FALSE)
  if (!is.null(state$had_plot)) return(isTRUE(state$had_plot))
  isTRUE(state$plots_ready)
}

init_pca_state <- function(input, debug_log) {
  # Analysis results storage (legacy)
  analysis_results <- reactiveVal(NULL)

  # Plot readiness flags
  plots_ready <- reactiveVal(FALSE)

  # Runtime-only ownership of the displayed result. Restored cache results are
  # deliberately independent of the current Data Wizard dataset.
  result_origin <- reactiveVal(NULL)
  live_revision <- reactiveVal(NULL)

  # Selected points for interactive plots
  selected_points_interactive <- reactiveVal(data.frame())

  # Labeled proteins for static plots
  labeled_proteins <- reactiveVal(character())

  # Protein search suggestions
  protein_suggestions <- reactiveVal(character())

  # Current plot objects
  static_plot_obj <- reactiveVal(NULL)
  interactive_plot_obj <- reactiveVal(NULL)

  # Available components for axis selection
  available_components <- reactiveVal(list(x = character(), y = character()))

  # Basic protein selection storage
  selected_data_pca <- reactiveVal(NULL)
  selected_protein_vector_pca <- reactiveVal(character())

  # Initialize protein selection system
  init_protein_selection_system <- function() {
    selected_data_pca(NULL)
    selected_protein_vector_pca(character())
    debug_log("Protein selection system initialized", 2)
  }

  # Call initialization
  init_protein_selection_system()

  ggplot_object_PCATab <- reactiveVal(NULL)

  scree_plot_obj <- reactiveVal(NULL)

  # Item selection (works for both proteins and samples)
  selected_items_vector_pca <- reactiveVal(character())
  selected_data_pca <- reactiveVal(data.frame())

  # Label settings storage
  item_label_settings_pca <- reactiveVal(data.frame(
    item_id = character(),
    label_color = character(),
    dot_color = character(),
    use_custom_dot_color = logical(),
    stringsAsFactors = FALSE
  ))

  # Reactive value to store sample labeling state (default: enabled for samples mode)
  sample_labeling_active_pca <- reactiveVal(TRUE)

  sample_label_settings_pca <- reactiveVal(list(
    master_label_color = "#000000",
    master_dot_color = "#E0E0E0",
    use_master_dot_color = FALSE,
    max_overlaps = 10,
    label_distance = 0.25,
    line_thickness = 0.5,
    label_size = 8,
    labeled_dot_size = 2,
    active = TRUE
  ))

  executed_method <- reactiveVal(NULL)

  # Separate storage for different analysis types
  sample_pca_results <- reactiveVal(NULL)
  protein_pca_results <- reactiveVal(NULL)
  sample_umap_results <- reactiveVal(NULL)
  protein_umap_results <- reactiveVal(NULL)

  # Legacy reactive (for backward compatibility)
  analysis_results <- reactiveVal(NULL)

  # Routes to the appropriate method/target-specific reactiveVal
  # (sample_pca_results, protein_pca_results, sample_umap_results,
  # protein_umap_results) based on the current UI selection.
  get_current_analysis_results <- reactive({
    current_method <- input$analysis_method %||% "pca"
    current_target <- input$comparison_target %||% "samples"

    if (current_method == "pca" && current_target == "samples") {
      sample_pca_results()
    } else if (current_method == "pca" && current_target == "proteins") {
      protein_pca_results()
    } else if (current_method == "umap" && current_target == "samples") {
      sample_umap_results()
    } else if (current_method == "umap" && current_target == "proteins") {
      protein_umap_results()
    } else {
      NULL
    }
  })

  # Non-reactive snapshot of all currently stored analysis results
  get_all_analysis_results <- function() {
    all_results <- list()
    if (!is.null(sample_pca_results()))   all_results$sample_pca   <- sample_pca_results()
    if (!is.null(protein_pca_results()))  all_results$protein_pca  <- protein_pca_results()
    if (!is.null(sample_umap_results()))  all_results$sample_umap  <- sample_umap_results()
    if (!is.null(protein_umap_results())) all_results$protein_umap <- protein_umap_results()
    return(all_results)
  }

  # Placeholder retained for diagnostic call sites; no-op by design.
  debug_stored_results <- function() {
  }

  # Dispatch analysis results to the per-type reactiveVal and to the legacy reactive
  store_analysis_results <- function(results) {
    if (is.null(results)) {
      debug_log("store_analysis_results: results is NULL", 1)
      return()
    }

    method <- tolower(trimws(results$method %||% "unknown"))
    target <- tolower(trimws(results$comparison_target %||% "unknown"))

    debug_log(paste("Storing analysis results - method:", method, "target:", target), 1)

    if (method == "pca" && target == "samples") {
      sample_pca_results(results)
    } else if (method == "pca" && target == "proteins") {
      protein_pca_results(results)
    } else if (method == "umap" && target == "samples") {
      sample_umap_results(results)
    } else if (method == "umap" && target == "proteins") {
      protein_umap_results(results)
    } else {
      debug_log(paste("Unknown method/target combination:", method, target), 1)
    }

    # Also update legacy reactiveVal used by rendering and export functions
    analysis_results(results)
  }

  return(list(
    analysis_results = analysis_results,
    plots_ready = plots_ready,
    result_origin = result_origin,
    live_revision = live_revision,
    selected_points_interactive = selected_points_interactive,
    labeled_proteins = labeled_proteins,
    protein_suggestions = protein_suggestions,
    static_plot_obj = static_plot_obj,
    interactive_plot_obj = interactive_plot_obj,
    available_components = available_components,
    selected_data_pca = selected_data_pca,
    selected_protein_vector_pca = selected_protein_vector_pca,
    ggplot_object_PCATab = ggplot_object_PCATab,
    scree_plot_obj = scree_plot_obj,
    selected_items_vector_pca = selected_items_vector_pca,
    item_label_settings_pca = item_label_settings_pca,
    sample_labeling_active_pca = sample_labeling_active_pca,
    sample_label_settings_pca = sample_label_settings_pca,
    executed_method = executed_method,
    sample_pca_results = sample_pca_results,
    protein_pca_results = protein_pca_results,
    sample_umap_results = sample_umap_results,
    protein_umap_results = protein_umap_results,
    get_current_analysis_results = get_current_analysis_results,
    get_all_analysis_results = get_all_analysis_results,
    debug_stored_results = debug_stored_results,
    store_analysis_results = store_analysis_results,
    # Session restore support: flag to suppress plot regeneration during
    # the restore cascade, and staging area for captured UI inputs.
    # These are reactiveVals (rather than plain list slots) so that writes
    # made via set_session_state() are visible to every observer closure
    # that captured `state` -- assigning into a plain list slot would
    # create a local copy in the writer's frame and leave readers seeing
    # the original NULL/FALSE values.
    restore_in_progress = reactiveVal(FALSE),
    pending_ui_inputs = reactiveVal(NULL),
    # Dynamic label rows may hydrate only after ordinary restored widgets have
    # received their saved values on the client.
    ordinary_ui_restore_complete = reactiveVal(FALSE),
    # Label-editor controls are restored separately from ordinary PCA inputs.
    # Dynamic row inputs do not exist until enhanced_selectedItems_pca has
    # rendered, so this payload must survive the ordinary-input flush.
    pending_label_ui_state = reactiveVal(NULL),
    # A generation-tagged stage prevents callbacks from an older restore from
    # hydrating (or completing) a newer restore.
    label_restore_stage = reactiveVal(list(generation = 0L, stage = "idle")),
    restore_generation = reactiveVal(0L),
    # Expected input values after restore; used by the comparison_target
    # observer to distinguish restore echoes from user-initiated changes.
    restored_comparison_target = reactiveVal(NULL),
    # Expected identifier dropdown value after restore. The identifier observer
    # consumes the matching client echo without re-running the saved analysis.
    restored_identifier_column = reactiveVal(NULL),
    # One-shot values expected to echo from update*Input calls after the main
    # restore guard is released.  Observers consume only an exact match; a
    # different value is treated as a real user edit and clears the marker.
    expected_restore_input_echoes = reactiveVal(list()),
    # Staged restore cache bundle consumed by rebuild observers.
    restore_plot_data_cache = reactiveVal(NULL),
    # Monotonic token used to explicitly invalidate plot renderers after
    # restore/UI synchronization has completed and a fresh re-render is desired.
    render_nonce = reactiveVal(0L)
  ))
}
