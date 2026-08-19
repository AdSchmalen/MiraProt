# ==============================================================================
# volcano_observers.R
# ==============================================================================
#
# PURPOSE:
#   All observer, render, and internal helper logic for the volcano module.
#   Contains every observeEvent, observe, output$render*, and supporting
#   functions (update_ui_choices, generateVolcanoPlots_fixed, trigger_live_update,
#   trigger_data_update, get_current_display_plot, store_original_plots).
#
# ARCHITECTURAL ROLE:
#   Observers -- the reactive behaviour layer of the volcano module. All
#   side effects (UI updates, plot generation, selection, labeling, export)
#   are registered here via a single entry point: register_volcano_observers().
#
# RESPONSIBILITIES:
#   - Data validation and UI choice updates
#   - Master controls (label color, dot color, custom dot checkbox)
#   - GSEA/GO pathway integration
#   - Protein search, selection, transfer, removal
#   - Plot generation, rendering (static + interactive), and live styling
#   - Labeling system (apply, clear, status display)
#   - Axis auto-range and manual override
#   - Download, clipboard, grid integration
#   - Interactive selection (plotly click, select, brush)
#   - Session cleanup
#
# MUST NOT CONTAIN:
#   - Pure data transformation functions (those are in volcano_data_processing.R)
#   - Pure plot creation functions (those are in volcano_plot_static.R /
#     volcano_plot_interactive.R)
#   - Reactive state declarations (those are in volcano_reactive_state.R)
#   - UI definitions (those are in volcano_module_UI.R)
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - volcano_data_processing.R: find_ratio_pvalue_pairs_smart,
#       generate_plot_title_from_pair, calculate_optimal_ranges,
#       get_filter_string_Volcano, prepare_volcano_plot_data_safe
#     - volcano_plot_static.R: create_single_volcano_plot_safe,
#       extract_plot_parameters_safe, apply_live_styling_to_plot,
#       apply_all_labels_to_plot_enhanced, create_volcano_label_data_enhanced,
#       get_default_dot_colors_for_proteins, generate_plot_titles_robust
#     - volcano_plot_interactive.R: prepare_interactive_plot_data,
#       create_plotly_volcano, configure_plotly_layout, add_plotly_interactivity
#     - volcano_export.R: copy_to_clipboard, save_volcano_plot,
#       add_volcano_to_grid, reset_volcano_axis_settings
#   External packages:
#     - shiny, shinyjs, plotly, DT, dplyr, colourpicker
#
# INTERACTIONS:
#   Called by:
#     - volcano_module.R: register_volcano_observers() called once during init
#   Calls into:
#     - All volcano sub-scripts (via functions sourced into modEnv)
#   Data flow:
#     - IN:  input, rv, res_GSEA, GO_res, reactive state objects

register_volcano_observers <- function(
    input, output, session, rv,
    res_GSEA, GO_res, module_outputs,
    volcano_state, plot_update_trigger,
    selected_data_Volcano, selected_protein_vector_Volcano,
    volcano_original_plots, volcano_labels, protein_label_settings,
    selected_points_interactive_Volcano,
    data_in, data_def_in, debug_log, ns, modEnv
) {
  compute_data_signature <- .volcano_data_signature
  has_dataset_mismatch_with_existing_plots <- function() {
    if (is.null(volcano_state$static_plots) || length(volcano_state$static_plots) == 0) return(FALSE)
    source_sig <- volcano_state$source_data_signature %||% NA_character_
    current_sig <- compute_data_signature(data_in(), data_def_in())
    is.character(source_sig) && nzchar(source_sig) && !identical(source_sig, current_sig)
  }

  shared <- list(input, output, session, rv,     res_GSEA, GO_res, module_outputs,     volcano_state, plot_update_trigger,     selected_data_Volcano, selected_protein_vector_Volcano,     volcano_original_plots, volcano_labels, protein_label_settings,     selected_points_interactive_Volcano,     data_in, data_def_in, debug_log, ns, modEnv)
  data_choice_helpers <- do.call(register_volcano_data_choice_observers, c(shared, list(
    compute_data_signature = compute_data_signature,
    has_dataset_mismatch_with_existing_plots = has_dataset_mismatch_with_existing_plots
  )))
  register_volcano_protein_selection_observers(input, output, session, rv,     res_GSEA, GO_res, module_outputs,     volcano_state, plot_update_trigger,     selected_data_Volcano, selected_protein_vector_Volcano,     volcano_original_plots, volcano_labels, protein_label_settings,     selected_points_interactive_Volcano,     data_in, data_def_in, debug_log, ns, modEnv)
  lifecycle_helpers <- do.call(register_volcano_plot_lifecycle_observers, c(shared, list(
    compute_data_signature = compute_data_signature,
    has_dataset_mismatch_with_existing_plots = has_dataset_mismatch_with_existing_plots,
    resolve_plot_selection = data_choice_helpers$resolve_plot_selection,
    reset_preplot_skip_log_flags = data_choice_helpers$reset_preplot_skip_log_flags,
    update_identifier_choices = data_choice_helpers$update_identifier_choices
  )))
  do.call(register_volcano_selection_restore_observers, c(shared, list(
    compute_data_signature = compute_data_signature,
    update_identifier_choices = data_choice_helpers$update_identifier_choices,
    calculate_optimal_ranges_for_selected_plot = lifecycle_helpers$calculate_optimal_ranges_for_selected_plot,
    store_original_plots = lifecycle_helpers$store_original_plots,
    generateVolcanoPlots_fixed = lifecycle_helpers$generateVolcanoPlots_fixed
  )))
  invisible(NULL)
}
