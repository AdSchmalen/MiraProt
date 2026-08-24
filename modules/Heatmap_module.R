# ==============================================================================
# Heatmap Module - Orchestrator
# ==============================================================================
#
# Purpose:
#   Single entry point for the Heatmap module. Defines modHeatmapUI() and
#   modHeatmapServer() and orchestrates all sub-scripts by sourcing them in
#   dependency order inside the moduleServer() closure.
#
# Architecture role:
#   This file is the only file in the Heatmap module that defines a server
#   function. It does not implement rendering, observer, or creation logic
#   directly; all such logic lives in the helper files listed below.
#
# Sub-script responsibilities:
#   Heatmap_ui.R             - UI builders and namespaced component construction.
#   Heatmap_utils.R          - Pure utility functions, statistical analysis
#                              pipeline, and alignment verification.
#   Heatmap_reactive_state.R - All reactiveVal() definitions (must be first).
#   Heatmap_creation.R       - Heatmap object builder helpers (color, font, etc.).
#   Heatmap_create_expression.R - Expression heatmap creation pipeline.
#   Heatmap_create_correlation.R - Correlation heatmap creation pipeline.
#   Heatmap_rendering.R      - compute_current_ordering reactive, all draw
#                              functions, and all output$/renderUI bindings.
#   Heatmap_observers.R      - All observe()/observeEvent() registrations.
#   Heatmap_download.R       - Download handlers and Grid export integration.
#
# Data flow:
#   Primary inputs come from the shared rv object: rv$data_mod, rv$data_def,
#   rv$ab_validate. Data access reactives defined inline below act as the
#   stable interface between external state and module internals.
#
# Sourcing order (MUST NOT change without careful dependency review):
#   1. Heatmap_utils.R          - no reactive state dependency
#   2. Heatmap_reactive_state.R - must precede all other sub-scripts
#   3. Heatmap_creation.R       - depends on utils
#   4. Heatmap_create_expression.R - depends on reactive state and creation
#   5. Heatmap_create_correlation.R - depends on reactive state and expression
#   6. Heatmap_rendering.R      - depends on all above; registers output$
#   7. Heatmap_observers.R      - depends on all above; registers observers
#   8. Heatmap_download.R       - depends on rendering helpers
#
# Logging:
#   Centralized through heatmap_debug_log() (alias: debug_log).
#   level 1 = state transitions, validation checkpoints, control flow.
#   level 2 = dimensions, intermediate diagnostics, verbose tracing.
#
# Cleanup:
#   Session cleanup is registered via cleanup_manager$register_module() only.
#   No session$onSessionEnded() calls are used in this module.
#
# Sourced at file level (outside moduleServer):
#   Heatmap_ui.R and R/utils.R are sourced at file level so that modHeatmapUI()
#   is available to app.R at load time.
# ==============================================================================

source("./modules/Heatmap/Heatmap_ui.R", local = TRUE)
source("./R/utils.R", local = TRUE)

# ==============================================================================
# Module UI Function
# ==============================================================================
modHeatmapUI <- function(id) {
  ns <- NS(id)
  create_heatmap_ui(ns)
}

# ==============================================================================
# Module Server Function
# ==============================================================================
modHeatmapServer <- function(id, rv, res_GSEA = NULL, GO_res = NULL, module_outputs = NULL, debug_level = 0, modEnv = new.env()) {
  DEBUG_LEVEL <- suppressWarnings(as.integer(debug_level))[1]
  if (length(DEBUG_LEVEL) == 0 || !is.finite(DEBUG_LEVEL)) DEBUG_LEVEL <- 0
  moduleServer(id, function(input, output, session, local = modEnv) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug Management (scoped to this module)
    # --------------------------------------------------------------------------
    heatmap_debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "HEATMAP MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ HEATMAP MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }
    # Legacy compatibility alias for helper signatures that still accept debug_log.
    debug_log <- heatmap_debug_log

    # Heatmap_utils.R is sourced first so heatmap_debug_log is available to it.
    source("./modules/Heatmap/Heatmap_utils.R", local = TRUE)
    source("./modules/Heatmap/Heatmap_statistical_analysis.R", local = TRUE)

    heatmap_debug_log("HEATMAP module server starting", 1)

    # --------------------------------------------------------------------------
    # Reactive State
    # All reactiveVal() definitions are in Heatmap_reactive_state.R.
    # MUST be sourced before any other Heatmap sub-script.
    # --------------------------------------------------------------------------
    source("./modules/Heatmap/Heatmap_reactive_state.R", local = TRUE)
    heatmap_last_plot_request <- reactiveVal(NULL)
    heatmap_cached_plot_ui_inputs <- reactiveVal(NULL)

    # --------------------------------------------------------------------------
    # Creation Helpers
    # --------------------------------------------------------------------------
    source("./modules/Heatmap/Heatmap_creation.R",            local = TRUE)
    source("./modules/Heatmap/Heatmap_create_expression.R",   local = TRUE)

    # --------------------------------------------------------------------------
    # Data Access Reactives
    # These three reactives form the stable data contract between external state
    # (rv) and module internals. They are kept inline to make the dependency on
    # rv explicit at the orchestrator level.
    # --------------------------------------------------------------------------
    heatmap_data_modified <- reactive({
      req(rv$data_mod)
      rv$data_mod
    })

    heatmap_df_data_definition <- reactive({
      req(rv$data_def)
      rv$data_def
    })

    abundance_validate <- reactive({
      av <- rv$ab_validate
      if (is.null(av)) return(list(numerator = NULL, denominator = NULL))
      av
    })

    # --------------------------------------------------------------------------
    # Remaining Sub-scripts (order is a strict requirement)
    # --------------------------------------------------------------------------
    source("./modules/Heatmap/Heatmap_create_correlation.R",  local = TRUE)
    source("./modules/Heatmap/Heatmap_rendering.R",           local = TRUE)
    source("./modules/Heatmap/Heatmap_observers.R",           local = TRUE)
    source("./modules/Heatmap/Heatmap_download.R",            local = TRUE)

    # --------------------------------------------------------------------------
    # Session Cleanup
    # --------------------------------------------------------------------------
    cleanup_manager$register_module("Heatmap", function() {
      heatmap_debug_log("Executing [Heatmap] cleanup", 2)
      heatmap_data(NULL)
      heatmap_expression_matrix(NULL)
      heatmap_protein_cor_matrix(NULL)
      heatmap_sample_cor_matrix(NULL)
      heatmap_fixed_expression(NULL)
      heatmap_fixed_protein_correlation(NULL)
      heatmap_fixed_sample_correlation(NULL)
      heatmap_fixed_basemean(NULL)
      heatmap_fixed_abundance_ratio(NULL)
      heatmap_plots(list(expr = NULL, prot = NULL, ratio = NULL, basemean = NULL))
      heatmap_pure_expression(NULL)
      heatmap_selected_proteins(NULL)
      heatmap_cluster_info(NULL)
      heatmap_basemean_values(NULL)
      heatmap_abundance_ratio_values(NULL)
      selected_data_Heatmap(NULL)
      selected_protein_vector_Heatmap(NULL)
      heatmap_protein_annotation(NULL)
      heatmap_single_tab_refresh_cache(list())
      heatmap_debug_log("[Heatmap] cleanup completed", 2)
    })

    # --------------------------------------------------------------------------
    # Module Return Values (Public API)
    # --------------------------------------------------------------------------
    heatmap_debug_log("Heatmap module server initialization completed", 1)

    return(list(
      get_expression_matrix  = reactive({ heatmap_expression_matrix() }),
      get_protein_correlation = reactive({ heatmap_protein_cor_matrix() }),

      get_sample_correlation = reactive({
        # Method 1: regular reactive value
        sample_corr <- heatmap_sample_cor_matrix()
        if (!is.null(sample_corr) && is.matrix(sample_corr) && nrow(sample_corr) > 0) {
          final_sample <- heatmap_resolve_final_sample_order(
            expr_colnames = colnames(sample_corr),
            context = "get_sample_correlation_method1"
          )
          sample_corr <- heatmap_sync_sample_correlation_axes(
            sample_corr,
            canonical_order = final_sample$order,
            context = "get_sample_correlation_method1"
          )
          return(sample_corr)
        }

        # Method 2: fixed sample correlation (ComplexHeatmap object)
        fixed_sample_corr <- heatmap_fixed_sample_correlation()
        if (!is.null(fixed_sample_corr)) {
          if (methods::is(fixed_sample_corr, "Heatmap")) {
            sample_matrix <- fixed_sample_corr@matrix
            if (!is.null(sample_matrix) && is.matrix(sample_matrix) && nrow(sample_matrix) > 0) {
              final_sample <- heatmap_resolve_final_sample_order(
                expr_colnames = colnames(sample_matrix),
                context = "get_sample_correlation_method2"
              )
              sample_matrix <- heatmap_sync_sample_correlation_axes(
                sample_matrix,
                canonical_order = final_sample$order,
                context = "get_sample_correlation_method2"
              )
              heatmap_debug_log(paste("Sample correlation export: extracted matrix",
                                      nrow(sample_matrix), "x", ncol(sample_matrix)), 2)
              return(sample_matrix)
            }
          } else if (is.matrix(fixed_sample_corr)) {
            return(fixed_sample_corr)
          }
        }

        # Method 3: calculate from expression matrix
        expr_matrix <- heatmap_expression_matrix()
        if (!is.null(expr_matrix) && is.matrix(expr_matrix) && ncol(expr_matrix) > 1) {
          tryCatch({
            sample_cor_matrix <- suppressWarnings(
              stats::cor(expr_matrix, use = "pairwise.complete.obs", method = "pearson"))
            sample_cor_matrix[is.na(sample_cor_matrix)] <- 0
            sample_names <- colnames(expr_matrix)
            rownames(sample_cor_matrix) <- sample_names
            colnames(sample_cor_matrix) <- sample_names
            if (nrow(sample_cor_matrix) > 0) {
              final_sample <- heatmap_resolve_final_sample_order(
                expr_colnames = sample_names,
                context = "get_sample_correlation_method3"
              )
              sample_cor_matrix <- heatmap_sync_sample_correlation_axes(
                sample_cor_matrix,
                canonical_order = final_sample$order,
                context = "get_sample_correlation_method3"
              )
              return(sample_cor_matrix)
            }
          }, error = function(e) {
            heatmap_debug_log(paste("Sample correlation export error (from expression matrix):", e$message), 1)
          })
        }

        # Method 4: calculate from fixed expression object
        fixed_expr <- heatmap_fixed_expression()
        if (!is.null(fixed_expr) && methods::is(fixed_expr, "Heatmap")) {
          tryCatch({
            expr_matrix <- fixed_expr@matrix
            if (!is.null(expr_matrix) && is.matrix(expr_matrix) && ncol(expr_matrix) > 1) {
              sample_cor_matrix <- suppressWarnings(
                stats::cor(expr_matrix, use = "pairwise.complete.obs", method = "pearson"))
              sample_cor_matrix[is.na(sample_cor_matrix)] <- 0
              sample_names <- colnames(expr_matrix)
              rownames(sample_cor_matrix) <- sample_names
              colnames(sample_cor_matrix) <- sample_names
              if (nrow(sample_cor_matrix) > 0) {
                final_sample <- heatmap_resolve_final_sample_order(
                  expr_colnames = sample_names,
                  context = "get_sample_correlation_method4"
                )
                sample_cor_matrix <- heatmap_sync_sample_correlation_axes(
                  sample_cor_matrix,
                  canonical_order = final_sample$order,
                  context = "get_sample_correlation_method4"
                )
                return(sample_cor_matrix)
              }
            }
          }, error = function(e) {
            heatmap_debug_log(paste("Sample correlation export error (from fixed expression):", e$message), 1)
          })
        }

        heatmap_debug_log("All methods failed: No sample correlation data available for export", 1)
        return(NULL)
      }),

      get_selected_proteins = reactive({ heatmap_selected_proteins() }),

      refresh_heatmap = function() {
        run_heatmap_creation(trigger_source = "external-refresh")
      },

      # Basemean heatmap export
      get_basemean_heatmap_for_export = reactive({
        fixed_basemean <- heatmap_fixed_basemean()
        if (!is.null(fixed_basemean) && methods::is(fixed_basemean, "Heatmap")) {
          basemean_matrix <- fixed_basemean@matrix
          if (!is.null(basemean_matrix) && is.matrix(basemean_matrix)) return(basemean_matrix)
        }
        basemean_values <- heatmap_basemean_values()
        if (!is.null(basemean_values) && length(basemean_values) > 0) {
          basemean_matrix <- matrix(basemean_values, ncol = 1)
          rownames(basemean_matrix) <- names(basemean_values)
          colnames(basemean_matrix) <- if (isTRUE(input$skip_log_transform_heatmap)) "Basemean" else "log2_Basemean"
          return(basemean_matrix)
        }
        heatmap_debug_log("No basemean data available for export", 1)
        return(NULL)
      }),

      # Abundance ratio heatmap export
      get_abundance_ratio_heatmap_for_export = reactive({
        fixed_ratio <- heatmap_fixed_abundance_ratio()
        if (!is.null(fixed_ratio) && methods::is(fixed_ratio, "Heatmap")) {
          heatmap_debug_log("Restore/export path: abundance ratio matrix <- fixed Heatmap object", 1)
          ratio_matrix <- validate_ratio_matrix(fixed_ratio@matrix, context = "get_abundance_ratio_heatmap_for_export/fixed")
          if (!is.null(ratio_matrix)) return(ratio_matrix)
        }
        ratio_values <- heatmap_abundance_ratio_values()
        if (!is.null(ratio_values) && length(ratio_values) > 0) {
          heatmap_debug_log("Restore/export path: abundance ratio matrix <- reactive ratio values", 1)
          ratio_matrix <- matrix(ratio_values, ncol = 1, dimnames = list(names(ratio_values), "log2_Abundance_Ratio"))
          ratio_matrix <- validate_ratio_matrix(ratio_matrix, context = "get_abundance_ratio_heatmap_for_export/values")
          if (!is.null(ratio_matrix)) return(ratio_matrix)
        }
        heatmap_debug_log("No abundance ratio data available for export", 1)
        return(NULL)
      }),

      # Export data availability summary (for debugging)
      get_export_summary = reactive({
        expr_matrix   <- heatmap_expression_matrix()
        protein_corr  <- heatmap_protein_cor_matrix()
        sample_corr   <- heatmap_sample_cor_matrix()
        fixed_sample  <- heatmap_fixed_sample_correlation()
        basemean_vals <- heatmap_basemean_values()
        ratio_vals    <- heatmap_abundance_ratio_values()
        fixed_bm      <- heatmap_fixed_basemean()
        fixed_ratio   <- heatmap_fixed_abundance_ratio()

        list(
          has_expression                 = !is.null(expr_matrix)  && is.matrix(expr_matrix)  && nrow(expr_matrix)  > 0,
          has_protein_correlation        = !is.null(protein_corr) && is.matrix(protein_corr) && nrow(protein_corr) > 0,
          has_sample_correlation_regular = !is.null(sample_corr)  && is.matrix(sample_corr)  && nrow(sample_corr)  > 0,
          has_sample_correlation_fixed   = !is.null(fixed_sample),
          has_basemean_values            = !is.null(basemean_vals) && length(basemean_vals) > 0,
          has_abundance_ratio_values     = !is.null(ratio_vals)    && length(ratio_vals)    > 0,
          has_fixed_basemean             = !is.null(fixed_bm),
          has_fixed_abundance_ratio      = !is.null(fixed_ratio),
          expression_dimensions          = if (!is.null(expr_matrix)) paste(nrow(expr_matrix), "x", ncol(expr_matrix)) else "NULL",
          basemean_count                 = if (!is.null(basemean_vals)) length(basemean_vals) else 0L,
          ratio_count                    = if (!is.null(ratio_vals))    length(ratio_vals)    else 0L
        )
      }),

      # ----------------------------------------------------------------------
      # Session save/restore interface (v2.0 — data-driven rebuild)
      # ----------------------------------------------------------------------
      # v2.0 stores UI widget values, annotation state, and either a Data Wizard
      # plot_data_cache_ref or a compact matrix_payload only when a matrix cannot
      # be reproduced from cache. ComplexHeatmap S4 / HeatmapList objects and
      # grob bundles are NOT serialised — the session_restore_trigger observer
      # in Heatmap_observers.R rebuilds every heatmap object from scratch using
      # the restored data + UI inputs.  This avoids "node stack overflow"
      # errors from serialising closures captured inside ComplexHeatmap
      # objects and keeps snapshots small and portable.
      get_session_state = function() {
        state <- list(version = "2.0")
        # --- Data ---
        state$heatmap_data                        <- tryCatch(isolate(heatmap_data()), error = function(e) NULL)
        state$heatmap_expression_matrix           <- tryCatch(isolate(heatmap_expression_matrix()), error = function(e) NULL)
        state$heatmap_protein_cor_matrix          <- tryCatch(isolate(heatmap_protein_cor_matrix()), error = function(e) NULL)
        state$heatmap_sample_cor_matrix           <- tryCatch(isolate(heatmap_sample_cor_matrix()), error = function(e) NULL)
        state$heatmap_pure_expression             <- tryCatch(isolate(heatmap_pure_expression()), error = function(e) NULL)
        state$heatmap_cluster_info                <- tryCatch(isolate(heatmap_cluster_info()), error = function(e) NULL)
        # --- Ordering (critical for row order + dendrogram) ---
        state$heatmap_shared_row_order            <- tryCatch(isolate(heatmap_shared_row_order()), error = function(e) NULL)
        state$heatmap_shared_col_order            <- tryCatch(isolate(heatmap_shared_col_order()), error = function(e) NULL)
        state$heatmap_shared_sample_cor_col_order <- tryCatch(isolate(heatmap_shared_sample_cor_col_order()), error = function(e) NULL)
        # --- Extension data ---
        state$heatmap_basemean_values             <- tryCatch(isolate(heatmap_basemean_values()), error = function(e) NULL)
        state$heatmap_abundance_ratio_values      <- tryCatch(isolate(heatmap_abundance_ratio_values()), error = function(e) NULL)
        # --- Protein selection ---
        state$selected_data_Heatmap               <- tryCatch(isolate(selected_data_Heatmap()), error = function(e) NULL)
        state$selected_protein_vector_Heatmap     <- tryCatch(isolate(selected_protein_vector_Heatmap()), error = function(e) NULL)
        state$heatmap_highlighted_proteins        <- tryCatch(isolate(heatmap_highlighted_proteins()), error = function(e) NULL)
        state$heatmap_applied_sort_state          <- tryCatch(isolate(heatmap_applied_sort_state()), error = function(e) NULL)
        state$heatmap_selected_proteins           <- tryCatch(isolate(heatmap_selected_proteins()), error = function(e) NULL)
        state$plot_request                         <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
        state$plot_ui_inputs                       <- tryCatch(isolate(heatmap_cached_plot_ui_inputs()), error = function(e) NULL)
        state$had_heatmap                          <- !is.null(state$heatmap_expression_matrix) && is.matrix(state$heatmap_expression_matrix) && nrow(state$heatmap_expression_matrix) > 0L
        has_heatmap_restore_intent <- isTRUE(state$had_heatmap) ||
          (is.list(state$plot_request) && length(state$plot_request) > 0L)
        # Explicit restore dependency contract for orchestration/cache graph.
        # Heatmap can restore matrix-native only when at least one matrix
        # payload is present.  Lean snapshots that omit matrices must ask the
        # session restore preprocessor to hydrate restore_plot_data_cache so the
        # deferred rebuild path can recreate the heatmap from Data Wizard data.
        has_matrix_payload <- any(vapply(
          list(
            state$heatmap_expression_matrix,
            state$heatmap_protein_cor_matrix,
            state$heatmap_sample_cor_matrix
          ),
          function(x) is.matrix(x) && nrow(x) > 0L,
          logical(1)
        ))
        heatmap_restore_strategy <- if (isTRUE(has_matrix_payload)) {
          "native_matrices"
        } else if (isTRUE(has_heatmap_restore_intent)) {
          "shared_cache"
        } else {
          "none"
        }
        state$restore_cache_dependency <- switch(
          heatmap_restore_strategy,
          native_matrices = "module_matrix_payload",
          shared_cache = "shared_plot_data_cache_pool",
          "none"
        )
        # This payload is a transient save-time cache candidate, never durable
        # Heatmap state. The central builder moves an accepted pair into the
        # shared pool and removes both embedded compatibility fields.
        if (identical(heatmap_restore_strategy, "shared_cache")) {
          cache_candidate <- tryCatch({
            if (inherits(rv$data_mod, "data.frame") && inherits(rv$data_def, "data.frame")) {
              list(data_mod = rv$data_mod, data_def = rv$data_def)
            } else NULL
          }, error = function(e) NULL)
          if (is.list(cache_candidate)) {
            state$plot_data_cache_ref <- tryCatch(
              .build_plot_data_cache_id(
                data_mod = cache_candidate$data_mod,
                data_def = cache_candidate$data_def
              ),
              error = function(e) NA_character_
            )
            state$plot_data_cache_payload <- cache_candidate
          }
        }
        heatmap_cache_ref <- state$plot_data_cache_ref %||% NULL
        heatmap_restore_value_present <- function(value) {
          !is.null(value) &&
            !(is.character(value) && length(value) == 0L) &&
            !(is.character(value) && !any(nzchar(value)))
        }

        # --- UI inputs ---
        state$ui_inputs <- tryCatch({
          cosmetic_live_ids <- c(
            "show_expr_row_labels", "show_expr_col_labels",
            "show_corr_row_labels", "show_corr_col_labels",
            "show_sample_row_labels", "show_sample_col_labels",
            "show_basemean_row_labels", "show_basemean_col_labels",
            "show_abundance_ratio_row_labels", "show_abundance_ratio_col_labels",
            "row_font_size", "col_font_size",
            "legend_title_font_size", "legend_text_font_size", "legend_position", "legend_plot_gap_heatmap",
            "Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3",
            "missing_value_color_heatmap", "correlation_enhanced_contrast", "hideTitle_Heatmap",
            "show_correlation_diagonal", "diagonal_line_color", "diagonal_line_width", "diagonal_rotate",
            "resolution_DPI_Heatmaps", "plotWidthInch_Heatmaps", "plotHeightInch_Heatmaps", "downloadFormat_Heatmaps"
          )
          heatmap_session_ids <- unique(c(ids, "GSEA_IdentifierFilter_Heatmap", "GO_IdentifierFilter_Heatmap",
                                          "Intersect_IdentifierFilter_Heatmap", "CoreEnriched_IdentifierFilter_Heatmap"))
          vals <- lapply(heatmap_session_ids, function(id) tryCatch(isolate(input[[id]]), error = function(e) NULL))
          names(vals) <- heatmap_session_ids
          vals$skip_log_transform_heatmap <- isTRUE(vals$skip_log_transform_heatmap)
          vals$hideTitle_Heatmap <- isTRUE(vals$hideTitle_Heatmap)
          vals$matrix_source <- vals$custom_col_sel_heatmap
          vals$filters <- vals[c(
            "remove_na_abundance_heatmap", "min_abundance_values_per_row_heatmap", "max_proteins_heatmap",
            "custom_proteins_filter", "custom_protein_order", "custom_protein_fallback_sort",
            "GSEA_IdentifierFilter_Heatmap", "GO_IdentifierFilter_Heatmap",
            "Intersect_IdentifierFilter_Heatmap", "CoreEnriched_IdentifierFilter_Heatmap",
            "enable_pvalue_filter_heatmap", "pval_type_heatmap", "pval_col_heatmap", "pval_threshold_heatmap",
            "enable_ratio_filter_heatmap", "abundance_ratio_col_heatmap", "ratio_filter_mode_heatmap", "ratio_threshold_heatmap"
          )]
          vals$clustering <- vals[c("sort_proteins_by", "sort_samples_by", "show_row_dendrogram", "show_column_dendrogram")]
          vals$scaling <- vals[c("skip_log_transform_heatmap")]
          vals$annotation <- vals[c(
            "GeneIdentifierColumn_Heatmap", "show_expr_row_labels", "show_expr_col_labels",
            "show_corr_row_labels", "show_corr_col_labels", "show_sample_row_labels", "show_sample_col_labels",
            "show_basemean_row_labels", "show_basemean_col_labels", "show_abundance_ratio_row_labels", "show_abundance_ratio_col_labels"
          )]
          vals$colors <- vals[c(
            "Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3",
            "missing_value_color_heatmap", "correlation_enhanced_contrast", "diagonal_line_color"
          )]
          vals
        }, error = function(e) NULL)

        heatmap_restore_samples_from_columns <- function(data_type, columns) {
          if (!heatmap_restore_value_present(data_type) ||
              is.null(columns) || length(columns) == 0L ||
              !inherits(rv$data_def, "data.frame") ||
              !all(c("Content", "Sample", "Column") %in% names(rv$data_def))) {
            return(character(0))
          }
          dd <- rv$data_def
          sample_rows <- which(
            normalize_content_value(dd$Content) == normalize_content_value(data_type) &
              !is.na(dd$Sample) &
              (dd$Column %in% columns | dd$Sample %in% columns)
          )
          unique(as.character(dd$Sample[sample_rows]))
        }

        heatmap_infer_data_type_from_metadata <- function(data_def, columns = NULL) {
          if (!inherits(data_def, "data.frame") || !all(c("Content", "Column", "Sample") %in% names(data_def))) return(NULL)
          possible_values <- c(
            "Normalized Abundance", "Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
            "Imputed Raw Abundance", "Batch Corrected Normalized Abundance",
            "Batch Corrected Raw Abundance"
          )
          abundance_rows <- normalize_content_value(data_def$Content) %in% normalize_content_value(possible_values)
          if (!any(abundance_rows)) return(NULL)
          columns <- as.character(columns %||% character(0))
          columns <- columns[nzchar(columns)]
          if (length(columns) > 0L) {
            matched_rows <- abundance_rows & (as.character(data_def$Column) %in% columns | as.character(data_def$Sample) %in% columns)
            if (any(matched_rows)) {
              counts <- sort(table(as.character(data_def$Content[matched_rows])), decreasing = TRUE)
              if (length(counts) > 0L) return(names(counts)[1])
            }
          }
          for (candidate in possible_values) {
            if (any(normalize_content_value(data_def$Content[abundance_rows]) == normalize_content_value(candidate))) return(candidate)
          }
          as.character(data_def$Content[which(abundance_rows)[1]])
        }

        heatmap_build_restore_inputs <- function(plot_request, ui_inputs) {
          restore_inputs <- if (is.list(plot_request)) plot_request else list()
          if (is.list(ui_inputs)) {
            for (input_name in names(ui_inputs)) {
              if (!heatmap_restore_value_present(restore_inputs[[input_name]]) &&
                  heatmap_restore_value_present(ui_inputs[[input_name]])) {
                restore_inputs[[input_name]] <- ui_inputs[[input_name]]
              }
            }
          }

          restored_columns <- restore_inputs$shared_col_order %||% restore_inputs$column_order %||% tryCatch(isolate(heatmap_shared_col_order()), error = function(e) NULL)
          selected_type <- restore_inputs$custom_col_sel_heatmap %||% (if (is.list(ui_inputs)) ui_inputs$custom_col_sel_heatmap else NULL)
          if (!heatmap_restore_value_present(selected_type)) {
            selected_type <- heatmap_infer_data_type_from_metadata(tryCatch(rv$data_def, error = function(e) NULL), restored_columns)
            if (heatmap_restore_value_present(selected_type)) {
              heatmap_debug_log(paste("[Heatmap] session save/restore: inferred abundance data type from metadata:", selected_type), 1)
            }
          }
          selected_samples <- restore_inputs$select_samples_heatmap %||% character(0)
          selected_samples <- as.character(selected_samples)
          selected_samples <- selected_samples[nzchar(selected_samples)]

          if (length(selected_samples) == 0L) {
            if (is.null(restored_columns) || length(restored_columns) == 0L) {
              expr_matrix <- tryCatch(isolate(heatmap_expression_matrix()), error = function(e) NULL)
              if (is.matrix(expr_matrix)) restored_columns <- colnames(expr_matrix)
            }
            selected_samples <- heatmap_restore_samples_from_columns(selected_type, restored_columns)
          }

          if (length(selected_samples) > 0L) {
            restore_inputs$select_samples_heatmap <- selected_samples
          }
          restore_inputs$custom_col_sel_heatmap <- selected_type
          restore_inputs$restore_input_version <- "1.0"
          restore_inputs
        }

        state$plot_request <- heatmap_build_restore_inputs(state$plot_request, state$ui_inputs)
        if (!is.list(state$plot_ui_inputs)) {
          state$plot_ui_inputs <- heatmap_build_restore_inputs(state$plot_request, state$ui_inputs)
        } else {
          state$plot_ui_inputs <- heatmap_build_restore_inputs(state$plot_ui_inputs, state$plot_request)
        }
        state$restore_inputs <- heatmap_build_restore_inputs(state$plot_ui_inputs, state$plot_request)

        heatmap_first_metadata_value <- function(content, column = "Column") {
          if (!inherits(rv$data_def, "data.frame") ||
              !all(c("Content", column) %in% names(rv$data_def))) {
            return(NULL)
          }
          rows <- which(normalize_content_value(rv$data_def$Content) == normalize_content_value(content))
          if (length(rows) == 0L) return(NULL)
          value <- as.character(rv$data_def[[column]][rows[1]])
          if (length(value) == 0L || is.na(value) || !nzchar(value)) NULL else value
        }

        heatmap_build_metadata_dependent_inputs <- function(restore_inputs, ui_inputs) {
          read_restore <- function(id) {
            value <- restore_inputs[[id]]
            if (!heatmap_restore_value_present(value) && is.list(ui_inputs)) value <- ui_inputs[[id]]
            value
          }

          metadata_inputs <- list(
            custom_col_sel_heatmap = read_restore("custom_col_sel_heatmap"),
            select_samples_heatmap = read_restore("select_samples_heatmap"),
            GeneIdentifierColumn_Heatmap = read_restore("GeneIdentifierColumn_Heatmap"),
            pval_type_heatmap = read_restore("pval_type_heatmap"),
            pval_col_heatmap = read_restore("pval_col_heatmap"),
            abundance_ratio_col_heatmap = read_restore("abundance_ratio_col_heatmap"),
            custom_proteins_filter = read_restore("custom_proteins_filter"),
            GSEA_IdentifierFilter_Heatmap = read_restore("GSEA_IdentifierFilter_Heatmap"),
            GO_IdentifierFilter_Heatmap = read_restore("GO_IdentifierFilter_Heatmap"),
            Intersect_IdentifierFilter_Heatmap = read_restore("Intersect_IdentifierFilter_Heatmap"),
            CoreEnriched_IdentifierFilter_Heatmap = read_restore("CoreEnriched_IdentifierFilter_Heatmap"),
            selected_proteins_for_labeling = tryCatch(isolate(heatmap_highlighted_proteins()), error = function(e) NULL),
            selected_data_for_labeling = tryCatch(isolate(selected_data_Heatmap()), error = function(e) NULL),
            selected_protein_vector_for_labeling = tryCatch(isolate(selected_protein_vector_Heatmap()), error = function(e) NULL),
            plot_data_cache_ref = state$plot_data_cache_ref %||% NULL,
            plot_data_cache_fingerprint = state$plot_data_cache_fingerprint %||% NULL
          )

          if (!heatmap_restore_value_present(metadata_inputs$GeneIdentifierColumn_Heatmap)) {
            metadata_inputs$GeneIdentifierColumn_Heatmap <- heatmap_first_metadata_value("Identifier", "Column")
          }
          if (!heatmap_restore_value_present(metadata_inputs$abundance_ratio_col_heatmap)) {
            metadata_inputs$abundance_ratio_col_heatmap <- heatmap_first_metadata_value("Abundance Ratio", "Column")
          }
          metadata_inputs$metadata_dependent_input_version <- "1.0"
          metadata_inputs
        }

        state$metadata_dependent_inputs <- heatmap_build_metadata_dependent_inputs(state$restore_inputs, state$plot_ui_inputs %||% state$ui_inputs)

        # Save native matrices only when the heatmap cannot be rebuilt from the
        # shared Data Wizard plot-data cache. This avoids full duplicated data
        # frames and never serializes rendered ComplexHeatmap/raster objects.
        if (identical(heatmap_restore_strategy, "native_matrices")) {
          state$matrix_payload <- list(
            expression_matrix = tryCatch(isolate(heatmap_expression_matrix()), error = function(e) NULL),
            protein_cor_matrix = tryCatch(isolate(heatmap_protein_cor_matrix()), error = function(e) NULL),
            sample_cor_matrix = tryCatch(isolate(heatmap_sample_cor_matrix()), error = function(e) NULL),
            pure_expression = tryCatch(isolate(heatmap_pure_expression()), error = function(e) NULL),
            cluster_info = tryCatch(isolate(heatmap_cluster_info()), error = function(e) NULL),
            shared_row_order = tryCatch(isolate(heatmap_shared_row_order()), error = function(e) NULL),
            shared_col_order = tryCatch(isolate(heatmap_shared_col_order()), error = function(e) NULL),
            shared_sample_cor_col_order = tryCatch(isolate(heatmap_shared_sample_cor_col_order()), error = function(e) NULL),
            basemean_values = tryCatch(isolate(heatmap_basemean_values()), error = function(e) NULL),
            abundance_ratio_values = tryCatch(isolate(heatmap_abundance_ratio_values()), error = function(e) NULL),
            selected_row_indices = tryCatch(isolate(heatmap_selected_row_indices()), error = function(e) NULL),
            applied_sort_state = tryCatch(isolate(heatmap_applied_sort_state()), error = function(e) NULL)
          )

          # matrix_payload is the single durable native reconstruction shape.
          # Do not also retain the legacy top-level copies (or any cache pair).
          state[c(
            "heatmap_expression_matrix", "heatmap_protein_cor_matrix",
            "heatmap_sample_cor_matrix", "heatmap_pure_expression",
            "heatmap_cluster_info", "heatmap_shared_row_order",
            "heatmap_shared_col_order", "heatmap_shared_sample_cor_col_order",
            "heatmap_basemean_values", "heatmap_abundance_ratio_values",
            "plot_data_cache_payload", "plot_data_cache_ref",
            "plot_cache_ref_by_title"
          )] <- NULL
        }

        if (identical(heatmap_restore_strategy, "shared_cache") &&
            !is.null(heatmap_cache_ref)) {
          canonical_key <- .build_canonical_plot_cache_key("heatmap", "heatmap", "main")
          state$plot_cache_ref_by_title <- stats::setNames(list(heatmap_cache_ref), canonical_key)
        }

        heatmap_debug_log(sprintf(
          "Heatmap session save (schema 2.0) | had_heatmap=%s | cache_ref=%s | matrix_payload=%s",
          isTRUE(state$had_heatmap), as.character(state$plot_data_cache_ref %||% "<none>"), is.list(state$matrix_payload)
        ), level = 0)
        state
      },
      set_session_state = function(state) {
        if (is.null(state)) return()
        # Legacy v1.x snapshots stored serialized ComplexHeatmap grob bundles
        # under state$heatmap_plots.  Those are no longer used; the restore
        # trigger rebuilds heatmaps from data.  If we encounter a legacy
        # snapshot, restore the data/orderings we can and leave heatmap_plots
        # empty so the rebuild path produces a fresh object.
        legacy <- !is.null(state$version) && !identical(state$version, "2.0")

        # Establish restore intent before staging reconstruction data. An
        # explicit flag is authoritative; only older snapshots that omitted it
        # infer intent from their top-level or schema-2 expression matrix.
        had_heatmap <- if (!is.null(state$had_heatmap)) {
          isTRUE(state$had_heatmap)
        } else {
          isTRUE(!is.null(state$heatmap_expression_matrix)) ||
            isTRUE(!is.null(state$matrix_payload$expression_matrix))
        }

        # Raise guard BEFORE any reactive writes so observers that react to
        # dynamic-choices repopulation (which fires when heatmap_data is set)
        # skip work while the restore cascade is mid-flight.
        heatmap_state$restore_in_progress <- TRUE
        heatmap_state$restore_generation <- isolate(rv$session_restore_generation %||% NA_integer_)
        heatmap_state$restore_callbacks_pending <- 0L
        heatmap_state$restore_job_settled <- FALSE
        register_restore_job <- session$userData$register_restore_job
        heatmap_state$restore_job_id <- if (isTRUE(had_heatmap) && is.function(register_restore_job)) tryCatch(
          register_restore_job("Heatmap", "replay and render restored heatmap", "render", 45),
          error = function(e) {
            heatmap_debug_log(paste("[Heatmap] restore job registration failed:", e$message), 1)
            NULL
          }
        ) else NULL
        heatmap_state$pending_matrix_payload <- if (isTRUE(had_heatmap) && is.list(state$matrix_payload)) state$matrix_payload else NULL
        heatmap_state$pending_plot_data_cache_ref <- if (isTRUE(had_heatmap)) state$plot_data_cache_ref %||% NULL else NULL
        heatmap_state$pending_plot_data_cache_fingerprint <- if (isTRUE(had_heatmap)) state$plot_data_cache_fingerprint %||% NULL else NULL
        heatmap_state$pending_data_mod_revision_id <- if (isTRUE(had_heatmap)) state$data_mod_revision_id %||% NULL else NULL
        heatmap_state$pending_data_def_revision_id <- if (isTRUE(had_heatmap)) state$data_def_revision_id %||% NULL else NULL
        heatmap_state$pending_annotation_state <- if (is.list(state$annotation_state)) state$annotation_state else NULL
        heatmap_state$pending_had_heatmap <- had_heatmap
        if (is.list(state$plot_request)) {
          heatmap_last_plot_request(state$plot_request)
        }

        # Restore matrix-native payloads only when present. Schema 2.0 prefers
        # plot_data_cache_ref + staged UI inputs so the restore observer can
        # rebuild from Data Wizard data/metadata without duplicating frames.
        payload <- if (isTRUE(had_heatmap)) heatmap_state$pending_matrix_payload else NULL
        if (is.list(payload)) {
          if (!is.null(payload$expression_matrix))          heatmap_expression_matrix(payload$expression_matrix)
          if (!is.null(payload$protein_cor_matrix))         heatmap_protein_cor_matrix(payload$protein_cor_matrix)
          if (!is.null(payload$sample_cor_matrix))          heatmap_sample_cor_matrix(payload$sample_cor_matrix)
          if (!is.null(payload$pure_expression))            heatmap_pure_expression(payload$pure_expression)
          if (!is.null(payload$cluster_info))               heatmap_cluster_info(payload$cluster_info)
          if (!is.null(payload$shared_row_order))           heatmap_shared_row_order(payload$shared_row_order)
          if (!is.null(payload$shared_col_order))           heatmap_shared_col_order(payload$shared_col_order)
          if (!is.null(payload$shared_sample_cor_col_order)) heatmap_shared_sample_cor_col_order(payload$shared_sample_cor_col_order)
          if (!is.null(payload$basemean_values))            heatmap_basemean_values(payload$basemean_values)
          if (!is.null(payload$abundance_ratio_values))     heatmap_abundance_ratio_values(payload$abundance_ratio_values)
          if (!is.null(payload$selected_row_indices))       heatmap_selected_row_indices(payload$selected_row_indices)
          if (!is.null(payload$applied_sort_state) && is.list(payload$applied_sort_state)) {
            heatmap_applied_sort_state(payload$applied_sort_state)
          }
        }

        # Restore data/cache.  When matrix payloads are omitted, orchestration
        # resolves plot_data_cache_ref into state$restore_plot_data_cache; publish
        # it to rv because Heatmap_observers.R resolves rebuild data from rv.
        if (isTRUE(had_heatmap) &&
            is.list(state$restore_plot_data_cache) &&
            inherits(state$restore_plot_data_cache$data_mod, "data.frame") &&
            inherits(state$restore_plot_data_cache$data_def, "data.frame")) {
          tryCatch({ rv$restore_plot_data_cache <- state$restore_plot_data_cache }, error = function(e) NULL)
        }
        # Restore data
        if (isTRUE(had_heatmap)) {
          if (!is.null(state$heatmap_data))                       heatmap_data(state$heatmap_data)
          if (!is.null(state$heatmap_expression_matrix))          heatmap_expression_matrix(state$heatmap_expression_matrix)
          if (!is.null(state$heatmap_protein_cor_matrix))         heatmap_protein_cor_matrix(state$heatmap_protein_cor_matrix)
          if (!is.null(state$heatmap_sample_cor_matrix))          heatmap_sample_cor_matrix(state$heatmap_sample_cor_matrix)
          if (!is.null(state$heatmap_pure_expression))            heatmap_pure_expression(state$heatmap_pure_expression)
          if (!is.null(state$heatmap_cluster_info))               heatmap_cluster_info(state$heatmap_cluster_info)
          # Restore ordering
          if (!is.null(state$heatmap_shared_row_order))           heatmap_shared_row_order(state$heatmap_shared_row_order)
          if (!is.null(state$heatmap_shared_col_order))           heatmap_shared_col_order(state$heatmap_shared_col_order)
          if (!is.null(state$heatmap_shared_sample_cor_col_order)) heatmap_shared_sample_cor_col_order(state$heatmap_shared_sample_cor_col_order)
          # Restore extension data
          if (!is.null(state$heatmap_basemean_values))            heatmap_basemean_values(state$heatmap_basemean_values)
          if (!is.null(state$heatmap_abundance_ratio_values))     heatmap_abundance_ratio_values(state$heatmap_abundance_ratio_values)
        }
        # Restore protein selection
        if (!is.null(state$selected_data_Heatmap))              selected_data_Heatmap(state$selected_data_Heatmap)
        if (!is.null(state$selected_protein_vector_Heatmap))    selected_protein_vector_Heatmap(state$selected_protein_vector_Heatmap)
        if (!is.null(state$heatmap_highlighted_proteins))       heatmap_highlighted_proteins(state$heatmap_highlighted_proteins)
        if (!is.null(state$heatmap_selected_proteins))          heatmap_selected_proteins(state$heatmap_selected_proteins)
        if (!is.null(state$heatmap_applied_sort_state) && is.list(state$heatmap_applied_sort_state)) {
          heatmap_applied_sort_state(state$heatmap_applied_sort_state)
        }
        if (is.list(state$annotation_state)) {
          if (!is.null(state$annotation_state$selected_data)) selected_data_Heatmap(state$annotation_state$selected_data)
          if (!is.null(state$annotation_state$selected_protein_vector)) selected_protein_vector_Heatmap(state$annotation_state$selected_protein_vector)
          if (!is.null(state$annotation_state$highlighted_proteins)) heatmap_highlighted_proteins(state$annotation_state$highlighted_proteins)
          if (!is.null(state$annotation_state$selected_proteins)) heatmap_selected_proteins(state$annotation_state$selected_proteins)
          if (!is.null(state$annotation_state$protein_annotation)) heatmap_protein_annotation(state$annotation_state$protein_annotation)
        }
        if (is.list(state$metadata_dependent_inputs)) {
          mdi <- state$metadata_dependent_inputs
          if (!is.null(mdi$selected_data_for_labeling)) selected_data_Heatmap(mdi$selected_data_for_labeling)
          if (!is.null(mdi$selected_protein_vector_for_labeling)) selected_protein_vector_Heatmap(mdi$selected_protein_vector_for_labeling)
          if (!is.null(mdi$selected_proteins_for_labeling)) heatmap_highlighted_proteins(mdi$selected_proteins_for_labeling)
        }
        # Clear any existing heatmap_plots so the restore_trigger observer
        # knows to rebuild.  Legacy v1 grob bundles are discarded.
        heatmap_plots(list(expr = NULL, prot = NULL, ratio = NULL, basemean = NULL))
        heatmap_fixed_expression(NULL)
        heatmap_fixed_protein_correlation(NULL)
        heatmap_fixed_sample_correlation(NULL)
        heatmap_fixed_basemean(NULL)
        heatmap_fixed_abundance_ratio(NULL)

        # Stage UI inputs for deferred update in the restore_trigger observer.
        # Dynamic-choices inputs (custom_col_sel_heatmap, select_samples_heatmap,
        # GeneIdentifierColumn_Heatmap, pval_col_heatmap, abundance_ratio_col_heatmap)
        # are pushed after the choice-repopulation observers flush.
        heatmap_restore_value_present <- function(value) {
          !is.null(value) &&
            !(is.character(value) && length(value) == 0L) &&
            !(is.character(value) && !any(nzchar(value)))
        }
        heatmap_merge_restore_inputs <- function(metadata_inputs, restore_inputs, ui_inputs, plot_request, plot_ui_inputs = NULL) {
          merged <- if (is.list(ui_inputs)) ui_inputs else list()
          for (source_inputs in list(plot_request, restore_inputs, plot_ui_inputs)) {
            if (!is.list(source_inputs)) next
            for (input_name in names(source_inputs)) {
              if (!heatmap_restore_value_present(merged[[input_name]]) &&
                  heatmap_restore_value_present(source_inputs[[input_name]])) {
                merged[[input_name]] <- source_inputs[[input_name]]
              }
            }
          }
          if (is.list(metadata_inputs)) {
            for (input_name in names(metadata_inputs)) {
              if (heatmap_restore_value_present(metadata_inputs[[input_name]])) {
                merged[[input_name]] <- metadata_inputs[[input_name]]
              }
            }
          }
          merged
        }
        authoritative_ui_inputs <- heatmap_merge_restore_inputs(state$metadata_dependent_inputs, state$restore_inputs, state$ui_inputs, state$plot_request, state$plot_ui_inputs)
        if (!is.null(authoritative_ui_inputs) && is.list(authoritative_ui_inputs)) {
          state$ui_inputs <- authoritative_ui_inputs
          if (is.null(state$ui_inputs$skip_log_transform_heatmap)) {
            state$ui_inputs$skip_log_transform_heatmap <- FALSE
          } else {
            state$ui_inputs$skip_log_transform_heatmap <- isTRUE(state$ui_inputs$skip_log_transform_heatmap)
          }
          if (is.null(state$ui_inputs$hideTitle_Heatmap)) {
            state$ui_inputs$hideTitle_Heatmap <- FALSE
          } else {
            state$ui_inputs$hideTitle_Heatmap <- isTRUE(state$ui_inputs$hideTitle_Heatmap)
          }
          heatmap_state$pending_ui_inputs <- state$ui_inputs
          heatmap_state$pending_dynamic_ui_inputs <- state$ui_inputs[intersect(
            names(state$ui_inputs),
            c(
              "custom_col_sel_heatmap", "select_samples_heatmap",
              "GeneIdentifierColumn_Heatmap", "pval_type_heatmap", "pval_col_heatmap",
              "abundance_ratio_col_heatmap",
              "GSEA_IdentifierFilter_Heatmap", "GO_IdentifierFilter_Heatmap"
            )
          )]
        } else {
          state$ui_inputs <- list(skip_log_transform_heatmap = FALSE, hideTitle_Heatmap = FALSE)
          heatmap_state$pending_ui_inputs <- state$ui_inputs
          heatmap_state$pending_dynamic_ui_inputs <- list()
        }

        if (isTRUE(had_heatmap)) {
          skip_log_restored <- isTRUE(state$ui_inputs$skip_log_transform_heatmap)
          heatmap_debug_log(
            sprintf(
              "Heatmap session restore staged | Skip log2 before Z-score: %s | Basemean scale: %s",
              skip_log_restored,
              if (skip_log_restored) "raw" else "log2"
            ),
            level = 0
          )
        }

        if (isTRUE(legacy)) {
          heatmap_debug_log(paste("[Heatmap] legacy session snapshot (version=",
                                  state$version, ") — grob bundles discarded, data preserved"), 1)
        }
        heatmap_debug_log("[Heatmap] session state staged; awaiting session_restore_trigger", 1)
      }
    ))
  })
}
