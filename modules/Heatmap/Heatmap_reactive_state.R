# ==============================================================================
# Heatmap Module - Reactive State Definitions
# ==============================================================================
#
# Purpose:
#   Centralizes all reactiveVal() definitions for the Heatmap module.
#   This file is sourced with local = TRUE inside the moduleServer() scope,
#   so all reactive values are available to every other sub-script.
#
# Design Decision:
#   Extracting reactive state into a dedicated file improves readability
#   of the main orchestrator and makes the module's state surface explicit.
#   No logic or side effects are contained here -- only state declarations.
#
# Sourced by: Heatmap_module.R (must be sourced FIRST, before all other sub-scripts)
# ==============================================================================

# ------------------------------------------------------------------------------
# Section: Core Heatmap Data State
# ------------------------------------------------------------------------------
heatmap_data <- reactiveVal(NULL)
heatmap_expression_matrix <- reactiveVal(NULL)
heatmap_protein_cor_matrix <- reactiveVal(NULL)
heatmap_sample_cor_matrix <- reactiveVal(NULL)
heatmap_selected_proteins <- reactiveVal(NULL)
heatmap_plots <- reactiveVal(list(
  expr = NULL,
  prot = NULL,
  ratio = NULL,
  basemean = NULL
))
heatmap_pure_expression <- reactiveVal(NULL)
heatmap_cluster_info <- reactiveVal(NULL)
heatmap_selected_row_indices <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Section: Shared Fixed Heatmap Objects (for consistent layout across tabs)
# ------------------------------------------------------------------------------
heatmap_fixed_expression <- reactiveVal(NULL)
heatmap_fixed_protein_correlation <- reactiveVal(NULL)
heatmap_fixed_sample_correlation <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Section: Shared Ordering Information
# ------------------------------------------------------------------------------
heatmap_shared_row_order <- reactiveVal(NULL)
heatmap_shared_col_order <- reactiveVal(NULL)
heatmap_shared_sample_cor_col_order <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Section: Extension Heatmap Objects (single column heatmaps)
# ------------------------------------------------------------------------------
heatmap_fixed_basemean <- reactiveVal(NULL)
heatmap_fixed_abundance_ratio <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Section: Extension Heatmap Data (for recreating objects)
# ------------------------------------------------------------------------------
heatmap_basemean_values <- reactiveVal(NULL)
heatmap_abundance_ratio_values <- reactiveVal(NULL)
heatmap_extension_signature <- reactiveVal(NULL)
heatmap_col_dend_notice_mode <- reactiveVal(NULL)
heatmap_auto_update_notice_state <- reactiveVal(list(key = NULL, time = as.POSIXct(NA)))

# ------------------------------------------------------------------------------
# Section: Protein Selection Panel State
# ------------------------------------------------------------------------------
selected_data_Heatmap <- reactiveVal(NULL)
selected_protein_vector_Heatmap <- reactiveVal(NULL)
heatmap_highlighted_proteins <- reactiveVal(NULL)
heatmap_protein_annotation <- reactiveVal(NULL)
heatmap_single_tab_refresh_cache <- reactiveVal(list())

# ------------------------------------------------------------------------------
# Section: Sort State Tracking
# Purpose: Stores the sort settings that were actually used for the last
#          successful create run. Draw paths use this to avoid transient
#          redraws with stale matrices when UI sort inputs change.
# ------------------------------------------------------------------------------
heatmap_applied_sort_state <- reactiveVal(list(
  sort_proteins_by = "z_score",
  sort_samples_by = "none"
))

# ------------------------------------------------------------------------------
# Section: Session Restore Coordination
# Purpose: Mirrors the pca_state / dotplot_state pattern. The restore guard is
#          raised by set_session_state() before any reactive writes occur, and
#          cleared at the end of the session_restore_trigger onFlushed cascade.
#          Observers that react to UI inputs check this flag and skip work
#          while a restore is in progress, so they cannot overwrite restored
#          state with stale input defaults. `pending_ui_inputs` stages the
#          captured UI inputs list for the deferred update cascade.
# ------------------------------------------------------------------------------
heatmap_state <- new.env(parent = emptyenv())
heatmap_state$restore_in_progress <- FALSE
heatmap_state$pending_ui_inputs   <- NULL
heatmap_state$pending_dynamic_ui_inputs <- NULL
heatmap_state$pending_matrix_payload <- NULL
heatmap_state$pending_plot_data_cache_ref <- NULL
heatmap_state$pending_plot_data_cache_fingerprint <- NULL
heatmap_state$pending_data_mod_revision_id <- NULL
heatmap_state$pending_data_def_revision_id <- NULL
heatmap_state$pending_annotation_state <- NULL
heatmap_state$pending_had_heatmap <- FALSE
