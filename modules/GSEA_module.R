# GSEA_module.R
#
# Purpose:
#   Orchestrator and the only server entry point for the GSEA module. Loads
#   all subfiles, defines the UI function, and coordinates the server function
#   by calling init_gsea_state() and init_gsea_observers().
#
# Architecture:
#   This file is the single integration point. No server logic lives here
#   beyond the moduleServer() call and the thin wiring between init functions.
#   All observable, state, logic, gene set, and parallelization concerns are
#   delegated to the respective subfiles.
#
# File loading order (dependencies must be loaded before dependents):
#   1. GSEA_module_logic.R         - pure functions; no Shiny dependency
#   2. GSEA_module_Gene_Sets.R     - gene set helpers; depends on logic (gsea_debug_log)
#   3. GSEA_module_parallelization.R - parallel backend; depends on logic (gsea_debug_log)
#   4. GSEA_module_state.R         - reactive state factory; depends on Shiny
#   5. GSEA_module_observer.R      - observer/output setup; depends on state + all above
#   6. GSEA_ui.R                   - UI definition
#   7. GSEA_plots.R                - plot rendering helpers
#
# Developer notes:
#   - GSEA_export.R is loaded separately by app.R and is not sourced here.
#   - To add a new observer, add it in GSEA_module_observer.R inside
#     init_gsea_observers().
#   - To add new reactive state, add it in GSEA_module_state.R inside
#     init_gsea_state() and expose it in the returned list.
#   - To add a new pure helper function, add it to GSEA_module_logic.R.
#   - To add gene set handling logic, add it to GSEA_module_Gene_Sets.R.
#   - All parallelization changes go to GSEA_module_parallelization.R.

# Load subfiles - order matters (see notes above)
source("modules/GSEA/GSEA_module_logic.R",           local = modEnv)
source("modules/GSEA/GSEA_module_Gene_Sets.R",        local = modEnv)
source("modules/GSEA/GSEA_module_parallelization.R",  local = modEnv)
source("modules/GSEA/GSEA_module_state.R",             local = modEnv)
source("modules/GSEA/GSEA_module_observer.R",          local = modEnv)
source("modules/GSEA/GSEA_ui.R",                       local = modEnv)
source("modules/GSEA/GSEA_plots.R",                    local = modEnv)

.gsea_session_safe_value <- function(x) {
  if (is.null(x) || is.atomic(x)) return(x)
  if (is.function(x) || is.environment(x)) return(NULL)
  if (inherits(x, "ggplot", which = FALSE)) return(NULL)
  if (isS4(x)) return(NULL)
  if (is.data.frame(x)) {
    x[] <- lapply(x, .gsea_session_safe_value)
    return(x)
  }
  if (is.list(x)) return(lapply(x, .gsea_session_safe_value))
  x
}

.gsea_session_contains_ggplot <- function(x, depth = 0, max_depth = 8) {
  if (depth > max_depth || is.null(x) || is.atomic(x) || is.function(x) || is.environment(x)) return(FALSE)
  if (inherits(x, "ggplot", which = FALSE)) return(TRUE)
  if (is.data.frame(x) || is.list(x)) {
    return(any(vapply(x, .gsea_session_contains_ggplot, logical(1), depth = depth + 1, max_depth = max_depth)))
  }
  if (isS4(x)) {
    slots <- tryCatch(methods::slotNames(x), error = function(e) character())
    return(any(vapply(slots, function(slot_name) {
      .gsea_session_contains_ggplot(tryCatch(methods::slot(x, slot_name), error = function(e) NULL), depth + 1, max_depth)
    }, logical(1))))
  }
  FALSE
}

# ========================================
# UI Function
# ========================================

GSEA_module_ui <- function(id) {
  ns <- NS(id)
  GSEA_ui_definition(ns)
}

# ========================================
# Server Function
# ========================================


GSEA_module_server <- function(id, rv, debug_level = 0, modEnv = new.env()) {
  DEBUG_LEVEL <- suppressWarnings(as.integer(debug_level))[1]
  if (length(DEBUG_LEVEL) == 0 || !is.finite(DEBUG_LEVEL)) DEBUG_LEVEL <- 0
  moduleServer(id, function(input, output, session, local = modEnv) {

    # ============================================================
    # Debug Setup
    # ============================================================

    ns          <- session$ns

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "GSEA MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ GSEA MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("GSEA module server starting", 1)

    pending_session_ui_restore <- reactiveVal(NULL)
    last_applied_restore_signature <- reactiveVal(NULL)
    active_restore_signature <- reactiveVal(NULL)
    assign("pending_session_ui_restore", pending_session_ui_restore, envir = modEnv)
    assign("last_applied_restore_signature", last_applied_restore_signature, envir = modEnv)
    assign("active_restore_signature", active_restore_signature, envir = modEnv)

    # ============================================================
    # Module Setup
    # ============================================================

    libs_loaded  <- gsea_load_required_libraries(DEBUG_LEVEL)
    if (!libs_loaded) {
      showNotification("Critical GSEA libraries not available", type = "error", duration = NULL)
    }

    rank_methods <- gsea_get_rank_methods()

    # ============================================================
    # Reactive State
    # ============================================================

    state <- init_gsea_state(rv = rv, debug_log = debug_log)

    # Unpack for backwards compatibility and for the module interface
    data_modified               <- state$data_modified
    df_data_definition_post_mod <- state$df_data_definition_post_mod
    res_GSEA                    <- state$res_GSEA
    current_rankings            <- state$current_rankings
    selected_enrichment         <- state$selected_enrichment
    plot_height                 <- state$plot_height
    current_plot                <- state$current_plot
    gsea_session_workers        <- state$gsea_session_workers
    analysis_metadata           <- state$analysis_metadata
    imported_gsea_results       <- state$imported_gsea_results
    import_status_message       <- state$import_status_message

    # ============================================================
    # Observers and Outputs
    # ============================================================

    init_gsea_observers(
      input        = input,
      output       = output,
      session      = session,
      rv           = rv,
      ns           = ns,
      state        = state,
      modEnv       = modEnv,
      DEBUG_LEVEL  = DEBUG_LEVEL,
      debug_log    = debug_log,
      rank_methods = rank_methods,
      libs_loaded  = libs_loaded
    )

    # ============================================================
    # Module Interface
    # ============================================================

    return(list(
      # Data access
      get_results            = reactive(res_GSEA()),
      get_current_rankings   = reactive(current_rankings()),
      get_selected_enrichment = reactive(selected_enrichment()),
      get_selected_pathways  = reactive(input$custom_Enrich_select),
      get_plot_type          = reactive(input$plot_type_GSEA),
      get_analysis_metadata  = reactive(analysis_metadata()),

      # Status functions
      has_results = reactive({
        !is.null(imported_gsea_results()) || !is.null(res_GSEA())
      }),
      analysis_ready = reactive({
        !is.null(df_data_definition_post_mod()) &&
          !is.null(data_modified()) &&
          length(input$Identifier_GSEA) > 0 &&
          length(input$fileSelector_GSEA) > 0
      }),

      # Results access for other modules
      get_significant_pathways = reactive({
        results <- res_GSEA()
        if (is.null(results)) return(character(0))
        as.data.frame(results$Results)$Description
      }),

      get_pathway_genes = function(pathway_name) {
        results <- tryCatch(isolate(res_GSEA()), error = function(e) NULL)
        if (is.null(results) || is.null(pathway_name)) return(character(0))
        results_df <- as.data.frame(results$Results)
        pathway_row <- which(results_df$Description == pathway_name)
        if (length(pathway_row) == 0) return(character(0))
        core_genes <- results_df$core_enrichment[pathway_row]
        if (is.na(core_genes) || core_genes == "") return(character(0))
        unlist(strsplit(core_genes, "/"))
      },

      get_gene_rankings = reactive({
        rankings <- current_rankings()
        if (is.null(rankings)) return(NULL)
        list(
          genes        = names(rankings$Ranks),
          ranks        = as.numeric(rankings$Ranks),
          fold_changes = rankings$FC
        )
      }),

      # Configuration
      set_plot_height = function(height) {
        plot_height(height)
      },

      # Export for PCA integration
      export_results_for_pca = reactive({
        results  <- res_GSEA()
        rankings <- current_rankings()
        if (is.null(results) || is.null(rankings)) return(NULL)
        list(
          gsea_results     = results$Results,
          gene_ranks       = rankings$Ranks,
          fold_changes     = rankings$FC,
          selected_pathway = selected_enrichment()
        )
      }),

      # Module health check
      module_health_check = reactive({
        list(
          libraries_loaded    = libs_loaded,
          data_available      = !is.null(data_modified()) && !is.null(df_data_definition_post_mod()),
          results_available   = !is.null(res_GSEA()),
          gene_sets_available = dir.exists("./GSEA") && length(list.files("./GSEA", pattern = "\\.gmt$")) > 0,
          last_analysis       = if (!is.null(res_GSEA())) Sys.time() else NULL
        )
      }),

      clear_imported_results = function() {
        imported_gsea_results(NULL)
        import_status_message("")
        debug_log("Imported GSEA results cleared", 1)
      },

      # Session save/restore interface
      get_session_state = function(mode = "session") {
        state <- list(
          version = "1.1",
          mode = mode,
          ui_inputs = list(
            plot_type_GSEA = tryCatch(input$plot_type_GSEA, error = function(e) NULL),
            fileSelector_GSEA = tryCatch(input$fileSelector_GSEA, error = function(e) NULL),
            Identifier_GSEA = tryCatch(input$Identifier_GSEA, error = function(e) NULL),
            custom_Enrich_select = tryCatch(input$custom_Enrich_select, error = function(e) NULL),
            GSEAColorInput_down = tryCatch(input$GSEAColorInput_down, error = function(e) NULL),
            GSEAColorInput_zero = tryCatch(input$GSEAColorInput_zero, error = function(e) NULL),
            GSEAColorInput_up = tryCatch(input$GSEAColorInput_up, error = function(e) NULL),
            AxisTitleSize_GSEA = tryCatch(input$AxisTitleSize_GSEA, error = function(e) NULL),
            tickSize_GSEA = tryCatch(input$tickSize_GSEA, error = function(e) NULL),
            LegendTextSize_GSEA = tryCatch(input$LegendTextSize_GSEA, error = function(e) NULL),
            LegendTitleSize_GSEA = tryCatch(input$LegendTitleSize_GSEA, error = function(e) NULL),
            ThemeSelect_GSEA = tryCatch(input$ThemeSelect_GSEA, error = function(e) NULL),
            LegendPosition_GSEA = tryCatch(input$LegendPosition_GSEA, error = function(e) NULL),
            LabelSize_GSEA = tryCatch(input$LabelSize_GSEA, error = function(e) NULL),
            dotplot_swap_panels = tryCatch(input$dotplot_swap_panels, error = function(e) NULL),
            dotplot_y_ticks_right = tryCatch(input$dotplot_y_ticks_right, error = function(e) NULL),
            RefenceValues_GSEA = tryCatch(input$RefenceValues_GSEA, error = function(e) NULL),
            RankinkMethod_GSEA = tryCatch(input$RankinkMethod_GSEA, error = function(e) NULL),
            numeratorSel_GSEA = tryCatch(input$numeratorSel_GSEA, error = function(e) NULL),
            denominatorSel_GSEA = tryCatch(input$denominatorSel_GSEA, error = function(e) NULL),
            absolute_GSEA = tryCatch(input$absolute_GSEA, error = function(e) NULL),
            ties_GSEA = tryCatch(input$ties_GSEA, error = function(e) NULL),
            PADOG_GSEA = tryCatch(input$PADOG_GSEA, error = function(e) NULL),
            gsea_min_valid_values = tryCatch(input$gsea_min_valid_values, error = function(e) NULL),
            gsea_validation_rule = tryCatch(input$gsea_validation_rule, error = function(e) NULL),
            AbundanceRatio_GSEA_precalc = tryCatch(input$AbundanceRatio_GSEA_precalc, error = function(e) NULL),
            pVal_GSEA_precalc = tryCatch(input$pVal_GSEA_precalc, error = function(e) NULL),
            absolute_GSEA_precalc = tryCatch(input$absolute_GSEA_precalc, error = function(e) NULL),
            ties_GSEA_precalc = tryCatch(input$ties_GSEA_precalc, error = function(e) NULL),
            PADOG_GSEA_precalc = tryCatch(input$PADOG_GSEA_precalc, error = function(e) NULL),
            numPermutations_GSEA = tryCatch(input$numPermutations_GSEA, error = function(e) NULL),
            randomSeed_GSEA = tryCatch(input$randomSeed_GSEA, error = function(e) NULL),
            cnet_layout_method = tryCatch(input$cnet_layout_method, error = function(e) NULL),
            cnet_node_size = tryCatch(input$cnet_node_size, error = function(e) NULL),
            emap_layout = tryCatch(input$emap_layout, error = function(e) NULL),
            emap_node_size = tryCatch(input$emap_node_size, error = function(e) NULL),
            plotWidthInch_GSEA = tryCatch(input$plotWidthInch_GSEA, error = function(e) NULL),
            plotHeightInch_GSEA = tryCatch(input$plotHeightInch_GSEA, error = function(e) NULL),
            resolution_DPI_GSEA = tryCatch(input$resolution_DPI_GSEA, error = function(e) NULL),
            downloadFormat_GSEA = tryCatch(input$downloadFormat_GSEA, error = function(e) NULL),
            grid_label = tryCatch(input$grid_label, error = function(e) NULL),
            plot_height = tryCatch(isolate(plot_height()), error = function(e) 600)
          ),
          import_status = tryCatch(isolate(import_status_message()), error = function(e) ""),
          plot_height = tryCatch(isolate(plot_height()), error = function(e) 600)
        )
        if (identical(mode, "ui_only")) {
          return(state)
        }

        state$res_GSEA             <- tryCatch(isolate(res_GSEA()), error = function(e) NULL)
        state$current_rankings     <- tryCatch(isolate(current_rankings()), error = function(e) NULL)
        state$selected_enrichment  <- tryCatch(isolate(selected_enrichment()), error = function(e) NULL)
        state$analysis_metadata    <- tryCatch(isolate(analysis_metadata()), error = function(e) NULL)
        state$imported_gsea_results <- tryCatch(isolate(imported_gsea_results()), error = function(e) NULL)
        state$plot_recreation_state <- list(
          plot_type = state$ui_inputs$plot_type_GSEA,
          selected_pathways = state$ui_inputs$custom_Enrich_select,
          plot_height = state$plot_height
        )
        # Do not serialize current_plot; ggplot/ggproto trees make Data &
        # Analysis sanitization very slow. Restore recreates the plot from
        # res_GSEA plus saved UI settings.
        state
      },
      set_session_state = function(state) {
        if (is.null(state)) return()
        if (!is.null(state$res_GSEA))              res_GSEA(state$res_GSEA)
        if (!is.null(state$current_rankings))       current_rankings(state$current_rankings)
        if (!is.null(state$selected_enrichment))    selected_enrichment(state$selected_enrichment)
        if (!is.null(state$analysis_metadata))      analysis_metadata(state$analysis_metadata)
        if (!is.null(state$imported_gsea_results))  imported_gsea_results(state$imported_gsea_results)
        if (!is.null(state$import_status))          import_status_message(state$import_status)
        if (!is.null(state$plot_height))            plot_height(state$plot_height)
        restored_inputs <- state$ui_inputs %||% list()
        metadata_dependent_restore <- restored_inputs[c(
          "Identifier_GSEA", "RefenceValues_GSEA", "numeratorSel_GSEA",
          "denominatorSel_GSEA", "AbundanceRatio_GSEA_precalc",
          "pVal_GSEA_precalc", "custom_Enrich_select", "fileSelector_GSEA"
        )]
        metadata_dependent_restore <- metadata_dependent_restore[!vapply(metadata_dependent_restore, is.null, logical(1))]
        if (length(metadata_dependent_restore) > 0L) {
          tryCatch({ rv$gsea_pending_ui_restore <- metadata_dependent_restore }, error = function(e) NULL)
        }
        restore_select <- function(id, value) {
          if (!is.null(value) && length(value) >= 1L && all(!is.na(value))) {
            tryCatch(updateSelectInput(session, id, selected = value), error = function(e) NULL)
          }
        }
        restore_numeric <- function(id, value) {
          if (!is.null(value) && length(value) == 1L && !is.na(suppressWarnings(as.numeric(value)))) {
            tryCatch(updateNumericInput(session, id, value = value), error = function(e) NULL)
          }
        }
        apply_gsea_ui_inputs <- function() {
          restore_select("plot_type_GSEA", restored_inputs$plot_type_GSEA)
          # restore_select("fileSelector_GSEA", restored_inputs$fileSelector_GSEA)
          # restore_select("Identifier_GSEA", restored_inputs$Identifier_GSEA)
          # restore_select("custom_Enrich_select", restored_inputs$custom_Enrich_select)
          tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_down", value = restored_inputs$GSEAColorInput_down), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_zero", value = restored_inputs$GSEAColorInput_zero), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_up", value = restored_inputs$GSEAColorInput_up), error = function(e) NULL)
          restore_numeric("AxisTitleSize_GSEA", restored_inputs$AxisTitleSize_GSEA)
          restore_numeric("tickSize_GSEA", restored_inputs$tickSize_GSEA)
          restore_numeric("LegendTextSize_GSEA", restored_inputs$LegendTextSize_GSEA)
          restore_numeric("LegendTitleSize_GSEA", restored_inputs$LegendTitleSize_GSEA)
          restore_numeric("LabelSize_GSEA", restored_inputs$LabelSize_GSEA)
          restore_select("ThemeSelect_GSEA", restored_inputs$ThemeSelect_GSEA)
          restore_select("LegendPosition_GSEA", restored_inputs$LegendPosition_GSEA)
          if (!is.null(restored_inputs$dotplot_swap_panels)) tryCatch(updateCheckboxInput(session, "dotplot_swap_panels", value = isTRUE(restored_inputs$dotplot_swap_panels)), error = function(e) NULL)
          if (!is.null(restored_inputs$dotplot_y_ticks_right)) tryCatch(updateCheckboxInput(session, "dotplot_y_ticks_right", value = isTRUE(restored_inputs$dotplot_y_ticks_right)), error = function(e) NULL)
          # Legacy sessions used GSEA_type_select as a separate dispatch control.
          restored_ranking_method <- if (identical(restored_inputs$GSEA_type_select, "Precalculated Ranking") ||
                                           identical(restored_inputs$RankinkMethod_GSEA, "Precalculated statistics")) {
            restored_inputs$RankingMetric_GSEA_precalc
          } else {
            restored_inputs$RankinkMethod_GSEA
          }
          legacy_ranking_method <- c(
            "log2(FC)" = "log2 Ratio (precalculated)",
            "log2(FC) x -log10(p)" = "log2 Ratio x -log10(p-Value)",
            "-log10(p)" = "-log10(p-Value)"
          )[restored_ranking_method]
          if (length(legacy_ranking_method) == 1L && !is.na(legacy_ranking_method)) restored_ranking_method <- legacy_ranking_method
          # restore_select("RefenceValues_GSEA", restored_inputs$RefenceValues_GSEA)
          restore_select("RankinkMethod_GSEA", restored_ranking_method)
          # if (!is.null(restored_inputs$numeratorSel_GSEA)) tryCatch(updateSelectizeInput(session, "numeratorSel_GSEA", selected = restored_inputs$numeratorSel_GSEA, server = TRUE), error = function(e) NULL)
          # if (!is.null(restored_inputs$denominatorSel_GSEA)) tryCatch(updateSelectizeInput(session, "denominatorSel_GSEA", selected = restored_inputs$denominatorSel_GSEA, server = TRUE), error = function(e) NULL)
          if (!is.null(restored_inputs$absolute_GSEA)) tryCatch(updateCheckboxInput(session, "absolute_GSEA", value = isTRUE(restored_inputs$absolute_GSEA)), error = function(e) NULL)
          if (!is.null(restored_inputs$ties_GSEA)) tryCatch(updateCheckboxInput(session, "ties_GSEA", value = isTRUE(restored_inputs$ties_GSEA)), error = function(e) NULL)
          if (!is.null(restored_inputs$PADOG_GSEA)) tryCatch(updateCheckboxInput(session, "PADOG_GSEA", value = isTRUE(restored_inputs$PADOG_GSEA)), error = function(e) NULL)
          restore_numeric("gsea_min_valid_values", restored_inputs$gsea_min_valid_values)
          restore_select("gsea_validation_rule", restored_inputs$gsea_validation_rule)
          # restore_select("AbundanceRatio_GSEA_precalc", restored_inputs$AbundanceRatio_GSEA_precalc)
          # restore_select("pVal_GSEA_precalc", restored_inputs$pVal_GSEA_precalc)
          if (!is.null(restored_inputs$absolute_GSEA_precalc)) tryCatch(updateCheckboxInput(session, "absolute_GSEA_precalc", value = isTRUE(restored_inputs$absolute_GSEA_precalc)), error = function(e) NULL)
          if (!is.null(restored_inputs$ties_GSEA_precalc)) tryCatch(updateCheckboxInput(session, "ties_GSEA_precalc", value = isTRUE(restored_inputs$ties_GSEA_precalc)), error = function(e) NULL)
          if (!is.null(restored_inputs$PADOG_GSEA_precalc)) tryCatch(updateCheckboxInput(session, "PADOG_GSEA_precalc", value = isTRUE(restored_inputs$PADOG_GSEA_precalc)), error = function(e) NULL)
          restore_numeric("numPermutations_GSEA", restored_inputs$numPermutations_GSEA)
          restore_numeric("randomSeed_GSEA", restored_inputs$randomSeed_GSEA)
          restore_numeric("cnet_layout_method", restored_inputs$cnet_layout_method)
          restore_numeric("cnet_node_size", restored_inputs$cnet_node_size)
          restore_numeric("emap_layout", restored_inputs$emap_layout)
          restore_numeric("emap_node_size", restored_inputs$emap_node_size)
          restore_numeric("plotWidthInch_GSEA", restored_inputs$plotWidthInch_GSEA)
          restore_numeric("plotHeightInch_GSEA", restored_inputs$plotHeightInch_GSEA)
          restore_numeric("resolution_DPI_GSEA", restored_inputs$resolution_DPI_GSEA)
          restore_select("downloadFormat_GSEA", restored_inputs$downloadFormat_GSEA)
          if (!is.null(restored_inputs$grid_label)) tryCatch(updateTextInput(session, "grid_label", value = restored_inputs$grid_label), error = function(e) NULL)
          if (!is.null(restored_inputs$plot_height)) {
            tryCatch(updateNumericInput(session, "plot_height_gsea", value = restored_inputs$plot_height), error = function(e) NULL)
            plot_height(restored_inputs$plot_height)
          }
        }
        recreate_gsea_plot <- function() {
          if (is.null(tryCatch(isolate(res_GSEA()), error = function(e) NULL))) return(invisible(FALSE))
          selected <- restored_inputs$custom_Enrich_select
          if (is.null(selected) || length(selected) == 0L) {
            selected <- tryCatch(isolate(selected_enrichment()), error = function(e) character(0))
          }
          plot_type <- restored_inputs$plot_type_GSEA %||% "General Running Score Plot"
          colors <- c(restored_inputs$GSEAColorInput_down %||% "blue",
                      restored_inputs$GSEAColorInput_zero %||% "#EDEDED",
                      restored_inputs$GSEAColorInput_up %||% "orange")
          sizes <- list(
            axisTitle = as.numeric(restored_inputs$AxisTitleSize_GSEA %||% 12),
            tick = as.numeric(restored_inputs$tickSize_GSEA %||% 10),
            legendText = as.numeric(restored_inputs$LegendTextSize_GSEA %||% 10),
            legendTitle = as.numeric(restored_inputs$LegendTitleSize_GSEA %||% 12),
            label = as.numeric(restored_inputs$LabelSize_GSEA %||% 12),
            labelSize = as.numeric(restored_inputs$LabelSize_GSEA %||% 12)
          )
          legend_position <- restored_inputs$LegendPosition_GSEA %||% "right"
          theme <- get_selected_theme(restored_inputs$ThemeSelect_GSEA %||% "Black and White")
          plot_height_val <- as.numeric(restored_inputs$plot_height %||% 600)
          if (is.na(plot_height_val) || plot_height_val < 300) plot_height_val <- 600
          results <- isolate(res_GSEA())

          plot_result <- tryCatch({
            switch(plot_type,
              "General Running Score Plot" = {
                idx <- get_pathway_index(results, selected, single = TRUE)
                create_general_running_score_plot(results, idx, theme, sizes)
              },
              "Enrichment score dotplot" = create_enrichment_dotplot(
                results, selected, colors, theme, sizes, legend_position,
                swap_panels = isTRUE(restored_inputs$dotplot_swap_panels),
                y_ticks_right = isTRUE(restored_inputs$dotplot_y_ticks_right)
              ),
              "Cnet plot (log2FC)" = create_cnet_plot_fc(results, selected, colors, theme, sizes, legend_position),
              "Cnet plot (Ranking Metrics)" = create_cnet_plot_ranking(results, selected, colors, theme, sizes, legend_position),
              "Enrichment map" = create_enrichment_map(results, selected, colors, theme, sizes, legend_position),
              "Heatmap (log2FC)" = create_heatmap_fc(results, selected, colors, theme, sizes, legend_position),
              "Heatmap (Ranking Metrics)" = create_heatmap_ranking(results, selected, colors, theme, sizes, legend_position),
              "Ridgeline plot" = create_ridgeline_plot(results, selected, colors, theme, sizes, legend_position),
              "Running score plot" = {
                ids <- get_pathway_indices(results, selected)
                create_running_score_plot(results, ids, theme, sizes, legend_position, colors)
              },
              "Pubmed citations" = create_pubmed_plot(selected, colors, theme, sizes, legend_position),
              list(plot = NULL, message = paste("Plot type not implemented:", plot_type))
            )
          }, error = function(e) {
            debug_log(paste("GSEA plot recreation after session restore failed:", e$message), 1)
            list(plot = NULL, message = e$message)
          })

          if (!is.null(plot_result$plot)) {
            current_plot(plot_result$plot)
            output$GSEAplot_container <- renderUI({
              tagList(
                div(style = paste0("background-color:#d4edda;border:1px solid #c3e6cb;",
                                   "border-radius:4px;padding:10px;margin-bottom:10px;color:#155724;"),
                    plot_result$message %||% paste("Restored GSEA", plot_type, "plot")),
                plotOutput(ns("GSEAplot_custom"), height = paste0(plot_height_val, "px"), width = "100%")
              )
            })
            output$GSEAplot_custom <- renderPlot({ print(plot_result$plot) }, height = plot_height_val)
            debug_log("GSEA plot recreated from restored session UI state", 1)
            return(invisible(TRUE))
          }
          invisible(FALSE)
        }

        apply_gsea_ui_inputs()
        if (is.function(session$onFlushed)) {
          session$onFlushed(once = TRUE, function() {
            apply_gsea_ui_inputs()
            session$onFlushed(once = TRUE, function() {
              apply_gsea_ui_inputs()
              recreate_gsea_plot()
            })
          })
        } else {
          recreate_gsea_plot()
        }

        debug_log("[GSEA] session state restored via set_session_state", 1)
      },
      set_results = function(results) {
        res_GSEA(results)
        if (!is.null(results)) {
          debug_log("GSEA results set via session restore", 1)
          # Level-0 logs for session-restored GSEA results
          meta_l0      <- tryCatch(isolate(analysis_metadata()), error = function(e) NULL)
          db_l0        <- tryCatch(meta_l0$gmt_file %||% meta_l0$gene_set, error = function(e) NA_character_)
          type_l0      <- tryCatch(meta_l0$ranking_type,                   error = function(e) NA_character_)
          pval_cut_l0  <- tryCatch(as.character(meta_l0$analysis_params$pvalueCutoff),    error = function(e) NA_character_)
          padj_l0      <- tryCatch(as.character(meta_l0$analysis_params$pAdjustMethod),   error = function(e) NA_character_)
          mingss_l0    <- tryCatch(as.character(meta_l0$analysis_params$minGSSize),       error = function(e) NA_character_)
          maxgss_l0    <- tryCatch(as.character(meta_l0$analysis_params$maxGSSize),       error = function(e) NA_character_)
          nperm_l0     <- tryCatch(as.character(
            meta_l0$analysis_params$nPermSimple %||%
              meta_l0$analysis_params$numPermutations
          ), error = function(e) NA_character_)
          n_input_l0   <- tryCatch(length(results$GeneList), error = function(e) NA_integer_)
          ranking_desc_l0 <- if (isTRUE(type_l0 == "Sample-derived")) {
            sprintf("Ranking method: %s",
                    tryCatch(meta_l0$ranking_method_name, error = function(e) NA_character_))
          } else if (isTRUE(type_l0 == "Precalculated statistics")) {
            sprintf("Ranking: Precalculated statistics | Abundance ratio column: %s | p-value column: %s | Ranking metric: %s",
                    tryCatch(meta_l0$ab_ratio_col,   error = function(e) NA_character_),
                    tryCatch(meta_l0$pval_col,       error = function(e) NA_character_),
                    tryCatch(meta_l0$ranking_metric, error = function(e) NA_character_))
          } else {
            sprintf("Analysis type: %s",
                    tryCatch(meta_l0$analysis_type %||% type_l0, error = function(e) NA_character_))
          }
          debug_log(
            sprintf(
              paste0(
                "GSEA analysis inputs",
                " | Gene set file: %s",
                " | %s",
                " | p-value cutoff: %s",
                " | p-adjust method: %s",
                " | Min GS size: %s",
                " | Max GS size: %s",
                " | nPermSimple: %s",
                " | Input genes: %d"
              ),
              db_l0, ranking_desc_l0, pval_cut_l0, padj_l0,
              mingss_l0, maxgss_l0, nperm_l0, n_input_l0
            ),
            level = 0
          )
          n_ranked_l0   <- tryCatch(length(results$GeneList), error = function(e) NA_integer_)
          n_in_sets_l0  <- tryCatch({
            gs_genes <- unique(unlist(results$Results@geneSets))
            sum(names(results$GeneList) %in% gs_genes)
          }, error = function(e) NA_integer_)
          n_sig_l0      <- tryCatch(nrow(as.data.frame(results$Results)), error = function(e) NA_integer_)
          debug_log(
            sprintf(
              paste0(
                "GSEA analysis outcome",
                " | Genes ranked: %d",
                " | Genes recognized in gene sets: %d",
                " | Significant pathways: %d"
              ),
              n_ranked_l0, n_in_sets_l0, n_sig_l0
            ),
            level = 0
          )
        }
      }
    ))
  })
}
