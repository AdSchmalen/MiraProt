# ==============================================================================
# File: modules/PCA/pca_module_server_pipeline.R
#
# Purpose:
#   Defines the analysis pipeline and rendering pipeline for the PCA module.
#   Contains three registration functions that are called from the orchestrator
#   (pca_module.R): register_pca_analysis_core, register_pca_rendering_core,
#   and register_pca_export_handlers.
#
# Architectural Role:
#   This file owns the server-side computation and output logic. It is the only
#   file that contains eventReactives, renderPlot, renderPlotly, and output$
#   assignments for the main plot area, scree plot, selection handling, and
#   data/plot export. No UI layout or pure logic functions live here.
#
# Structure:
#   1. register_pca_analysis_core     - Main analysis pipeline triggered by the
#                                       "Run Analysis" button. Validates data,
#                                       runs PCA/UMAP, stores results.
#   2. register_pca_rendering_core    - All plot outputs, selection outputs,
#                                       interactive plotly event handlers, and
#                                       the plots_ready module communication flag.
#   3. register_pca_export_handlers   - Download handlers for data, plots, and
#                                       add-to-grid integration.
#
# Notes for future developers:
#   - data_available is computed inside register_pca_analysis_core; it must
#     not be defined in pca_module.R.
#   - register_pca_rendering_core receives the full pca_state object and
#     extracts handles at the top of the function. Add new state extractions
#     there if a new reactiveVal is introduced in pca_module_state.R.
#   - All debug_log calls use level = 1 for important transitions and
#     level = 2 for verbose tracing. Hot reactive paths (output$ blocks) must
#     not log at level = 1 unless the event is meaningful (e.g. errors).
# ==============================================================================

register_pca_analysis_core <- function(
    input,
    rv,
    analysis_results,
    store_analysis_results,
    plots_ready,
    debug_log,
    state = NULL
) {
  # Local fallback resolver so PCA can run even when session_save_restore
  # helpers are not sourced into this module environment (fresh sessions).
  resolve_data_pair_local <- function(rv, restore_bundle = NULL, debug_log = NULL, module_label = "PCA") {
    bundle <- restore_bundle
    if (is.list(bundle) &&
        inherits(bundle$data_mod, "data.frame") &&
        inherits(bundle$data_def, "data.frame")) {
      if (is.function(debug_log)) {
        debug_log(sprintf("[%s] data_pair_for_restore resolved from staged restore bundle", module_label), 2)
      }
      return(list(data_mod = bundle$data_mod, data_def = bundle$data_def, source = "restore_bundle"))
    }

    live_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
    live_def <- tryCatch(rv$data_def, error = function(e) NULL)
    if (is.function(debug_log)) {
      debug_log(sprintf("[%s] data_pair_for_restore fell back to live rv$data_mod/rv$data_def", module_label), 2)
    }
    list(data_mod = live_mod, data_def = live_def, source = "live_rv")
  }

  data_pair_for_restore <- function() {
    restore_bundle <- NULL
    if (!is.null(state) && is.list(state) && is.function(state$restore_plot_data_cache)) {
      restore_bundle <- tryCatch(isolate(state$restore_plot_data_cache()), error = function(e) NULL)
    }
    if (exists("resolve_data_pair_for_restore", mode = "function")) {
      resolve_data_pair_for_restore(
        rv = rv,
        restore_bundle = restore_bundle,
        debug_log = debug_log,
        module_label = "PCA"
      )
    } else {
      resolve_data_pair_local(
        rv = rv,
        restore_bundle = restore_bundle,
        debug_log = debug_log,
        module_label = "PCA"
      )
    }
  }

  # User-triggered PCA/UMAP runs must always use the currently loaded dataset.
  # The restore cache is only for rebuilding the saved plot context after a
  # session restore; it must not shadow rv$data_mod/rv$data_def when the user
  # clicks Create Plot after loading or restoring a different active dataset.
  data_pair_for_live_analysis <- function() {
    list(
      data_mod = tryCatch(rv$data_mod, error = function(e) NULL),
      data_def = tryCatch(rv$data_def, error = function(e) NULL),
      source = "live_rv"
    )
  }

  # Validates that data and metadata are present and structurally sound.
  # Defined as a reactive within this registration function to keep the
  # orchestrator free of reactive expressions.
  data_available <- reactive({
    tryCatch({
      data_pair <- data_pair_for_live_analysis()
      data     <- data_pair$data_mod
      metadata <- data_pair$data_def

      if (is.null(data) || is.null(metadata)) {
        debug_log("Data or metadata not available", 2)
        return(FALSE)
      }

      if (nrow(data) == 0 || nrow(metadata) == 0) {
        debug_log("Empty data or metadata", 2)
        return(FALSE)
      }

      validation <- validate_dimension_reduction_data(data)
      if (!validation$valid) {
        debug_log(paste("Data validation failed:", validation$messages$error), 1)
        return(FALSE)
      }

      return(TRUE)
    }, error = function(e) {
      debug_log(paste("Error checking data availability:", e$message), 1)
      return(FALSE)
    })
  })

  # ========================================
  # Main Analysis Function
  # ========================================
  perform_analysis <- eventReactive(input$create_plot, {

    if (!data_available()) {
      showNotification("Data not available", type = "error", duration = 3)
      return(NULL)
    }

    debug_log("Starting dimension reduction analysis", 1)

    # Show progress
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Performing analysis...", value = 0)

    tryCatch({
      # Get data and metadata
      data_pair <- data_pair_for_live_analysis()
      data <- data_pair$data_mod
      metadata <- data_pair$data_def

      # Validate abundance data type selection
      selected_data_type <- input$custom_col_sel_pca
      if (is.null(selected_data_type) || selected_data_type == "") {
        showNotification("Please select a data type", type = "error", duration = 5)
        return(NULL)
      }

      # Validate individual sample selection
      selected_samples <- input$select_samples_pca
      if (is.null(selected_samples) || length(selected_samples) == 0) {
        showNotification("Please select at least one sample", type = "error", duration = 5)
        return(NULL)
      }

      if (length(selected_samples) < 2) {
        showNotification("Please select at least 2 samples for analysis", type = "error", duration = 5)
        return(NULL)
      }

      # Validate identifier selection
      selected_identifier <- input$GeneIdentifierColumn_pca
      if (is.null(selected_identifier) || selected_identifier == "") {
        showNotification("Please select an identifier column", type = "error", duration = 5)
        return(NULL)
      }

      debug_log(paste("Analysis parameters - Data type:", selected_data_type), 2)
      debug_log(paste("Selected samples:", paste(selected_samples, collapse = ", ")), 2)
      debug_log(paste("Identifier:", selected_identifier), 2)

      progress$inc(0.2, detail = "Preparing data...")

      prep_data <- prepare_pca_analysis_data(
        data = data,
        metadata = metadata,
        selected_data_type = selected_data_type,
        selected_samples = selected_samples,
        selected_identifier = selected_identifier,
        debug_log = debug_log
      )

      if (is.null(prep_data)) {
        showNotification("Failed to prepare data", type = "error", duration = 5)
        return(NULL)
      }

      # Check minimum requirements
      min_samples <- if (input$comparison_target == "samples") nrow(prep_data$data) else ncol(prep_data$data)
      min_features <- if (input$comparison_target == "samples") ncol(prep_data$data) else nrow(prep_data$data)

      if (min_samples < 3) {
        showNotification("At least 3 samples required for analysis", type = "error", duration = 5)
        return(NULL)
      }

      if (min_features < 2) {
        showNotification("At least 2 features required for analysis", type = "error", duration = 5)
        return(NULL)
      }

      progress$inc(0.2, detail = "Running analysis...")

      # Get analysis matrix
      if (input$comparison_target == "samples") {
        analysis_matrix <- t(prep_data$data)
        point_names <- prep_data$samples
      } else {
        analysis_matrix <- prep_data$data
        point_names <- prep_data$identifiers
      }

      # Perform analysis based on selected method
      method <- input$analysis_method
      debug_log(paste("Performing", method, "analysis on", nrow(analysis_matrix), "x", ncol(analysis_matrix), "matrix"), 1)

      results <- switch(
        method,
        "pca" = perform_pca(analysis_matrix, input, debug_log),
        "umap" = perform_umap(analysis_matrix, input, debug_log)
      )

      if (is.null(results)) {
        showNotification("Analysis failed - check parameters and try again", type = "error", duration = 5)
        return(NULL)
      }

      progress$inc(0.4, detail = "Finalizing results...")

      # Add metadata to results
      results$point_names <- point_names
      results$comparison_target <- input$comparison_target
      results$method <- method
      results$metadata <- prep_data$metadata
      results$raw_metadata <- rv$data_def
      results$identifier_col <- selected_identifier
      results$selected_samples <- selected_samples
      results$selected_data_type <- selected_data_type
      results$full_data <- data

      if (identical(input$comparison_target, "proteins")) {
        # Build the same plot data that rendering will consume before making
        # the result observable. This prevents a partially mappable protein
        # result from ever entering analysis_results/session state.
        invariant_plot_data <- tryCatch(
          create_plot_data(
            results,
            results$raw_metadata %||% results$metadata,
            selected_identifier,
            debug_log = debug_log
          ),
          error = function(e) {
            debug_log(paste("Protein result alignment failed:", e$message), 1)
            NULL
          }
        )
        invariant_counts <- c(
          analysis_matrix = nrow(analysis_matrix),
          coordinates = nrow(results$coordinates),
          point_names = length(point_names),
          plot_data = if (is.null(invariant_plot_data)) NA_integer_ else nrow(invariant_plot_data)
        )
        if (anyNA(invariant_counts) || length(unique(invariant_counts)) != 1L) {
          diagnostic <- sprintf(
            paste0("Protein row alignment failed: analysis matrix=%d, coordinates=%d, ",
                   "point_names=%d, plot data=%s. The analysis was not saved."),
            invariant_counts[["analysis_matrix"]], invariant_counts[["coordinates"]],
            invariant_counts[["point_names"]], as.character(invariant_counts[["plot_data"]])
          )
          debug_log(diagnostic, 1)
          showNotification(diagnostic, type = "error", duration = 8)
          return(NULL)
        }
        # Persist the final identifiers next to the coordinates. These are the
        # sole mapping used by protein plot construction.
        results$point_names <- as.character(point_names)
        results$coordinate_identifiers <- as.character(point_names)
      }

      # Store results
      analysis_results(results)
      store_analysis_results(results)
      plots_ready(TRUE)

      showNotification("Analysis completed successfully", type = "message", duration = 3)
      debug_log(paste("Analysis completed:", method, "with", length(point_names), "points"), 1)
      selected_samples_str <- if (length(selected_samples) > 0) {
        paste(selected_samples, collapse = ", ")
      } else {
        "none"
      }
      analysis_target_label <- if (identical(input$comparison_target, "samples")) "sample mode" else "protein mode"
      method_label <- if (identical(method, "umap")) "UMAP mode" else "PCA mode"

      debug_log(
        sprintf(
          paste0(
            "Dimensionality reduction summary",
            " | Method: %s",
            " | Analysis target: %s",
            " | Data type: %s",
            " | Selected samples (%d): [%s]",
            " | Input matrix dimensions: %d x %d",
            " | Points generated: %d"
          ),
          method_label,
          analysis_target_label,
          as.character(selected_data_type),
          length(selected_samples),
          selected_samples_str,
          nrow(analysis_matrix),
          ncol(analysis_matrix),
          length(point_names)
        ),
        level = 0
      )

      return(results)

    }, error = function(e) {
      debug_log(paste("Analysis error:", e$message), 1)
      showNotification(paste("Analysis error:", e$message), type = "error", duration = 5)
      return(NULL)
    })
  })

  # Trigger analysis
  observeEvent(input$create_plot, {

    if (input$create_plot == "PCA of samples") {

      # Check if we have the necessary data
      if (!is.null(rv$data_def)) {
        metadata <- rv$data_def
        debug_log(paste("Metadata available with", nrow(metadata), "rows"), 2)

        # Check Options column
        if ("Options" %in% colnames(metadata)) {
          options_values <- unique(metadata$Options[!is.na(metadata$Options)])
          debug_log(paste("Available options/conditions:", paste(options_values, collapse = ", ")), 2)
        } else {
          debug_log("No Options column in metadata", 1)
        }
      } else {
        debug_log("No metadata available", 2)
      }
    }

    perform_analysis()
  })

  return(list(perform_analysis = perform_analysis))
}

register_pca_rendering_core <- function(input, output, session, rv, state, debug_log) {
  plots_ready <- state$plots_ready
  analysis_results <- state$analysis_results
  available_components <- state$available_components
  sample_label_settings_pca <- state$sample_label_settings_pca
  sample_labeling_active_pca <- state$sample_labeling_active_pca
  item_label_settings_pca <- state$item_label_settings_pca
  selected_items_vector_pca <- state$selected_items_vector_pca
  selected_data_pca <- state$selected_data_pca
  selected_protein_vector_pca <- state$selected_protein_vector_pca
  selected_points_interactive <- state$selected_points_interactive
  static_plot_obj <- state$static_plot_obj
  interactive_plot_obj <- state$interactive_plot_obj
  scree_plot_obj <- state$scree_plot_obj
  ggplot_object_PCATab <- state$ggplot_object_PCATab
  labeled_proteins <- state$labeled_proteins
  render_nonce <- state$render_nonce

  update_restore_render_report <- function(generation, status, error_message = NULL) {
    updated <- isolate({
      reports <- rv$restore_reports
      if (!is.list(reports)) reports <- list()
      report <- reports$PCA %||% list()
      if (!identical(report$restore_generation, generation) ||
          !identical(state$restore_generation(), generation) ||
          !identical(report$session_restore_generation,
                     rv$session_restore_generation %||% NA_integer_)) return(FALSE)
      report$render_status <- status
      report$render_completed <- identical(status, "render_completed")
      report$plot_recreated <- report$render_completed
      report$render_failed <- identical(status, "render_failed")
      report$render_timed_out <- identical(status, "render_timed_out")
      if (!is.null(error_message)) report$render_error <- error_message
      reports$PCA <- report
      rv$restore_reports <- reports
      if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
        rv$restore_diagnostics <- reports
      }
      TRUE
    })
    if (!isTRUE(updated)) return(FALSE)
    report <- isolate((rv$restore_reports %||% list())$PCA %||% list())
    resolver <- session$userData$resolve_restore_job
    if (is.function(resolver) && !is.null(report$render_job_id)) {
      outcome <- if (identical(status, "render_completed")) "success" else "failure"
      error <- if (identical(outcome, "failure")) error_message %||% "PCA render failed" else NULL
      resolver(report$render_job_id, outcome, error)
    }
    debug_log(sprintf("[PCA] restore %s (generation=%s)", status, generation), 1)
    TRUE
  }

  debounced_pca_plot_inputs <- debounce(reactive({
    list(
      comparison_target = input$comparison_target,
      AxisTitleSize_PCATab = input$AxisTitleSize_PCATab,
      tickSize_PCATab = input$tickSize_PCATab,
      LegendTitleSize_PCATab = input$LegendTitleSize_PCATab,
      LegendTextSize_PCATab = input$LegendTextSize_PCATab,
      axis_x = input$axis_x,
      axis_y = input$axis_y,
      point_size = input$point_size,
      color_palette = input$color_palette,
      reverse_colors = input$reverse_colors,
      GeneIdentifierColumn_pca = input$GeneIdentifierColumn_pca,
      defaultProteinColor_pca = input$defaultProteinColor_pca,
      plot_theme = input$plot_theme,
      legend_position = input$legend_position,
      maxOverlaps_pca = input$maxOverlaps_pca,
      labelDistance_pca = input$labelDistance_pca,
      lineThickness_pca = input$lineThickness_pca,
      labelSize_pca = input$labelSize_pca,
      dotSizeLabeled_pca = input$dotSizeLabeled_pca,
      dotSizeLabeled_samples = input$dotSizeLabeled_samples,
      show_scree = input$show_scree
    )
  }), 700)

  # ========================================
  # Static Plot Outputs
  # ========================================

  output$static_plot <- renderPlot({
    render_nonce()
    debounced_pca_plot_inputs()

    if (!plots_ready()) return(NULL)

    results <- analysis_results()
    if (is.null(results)) return(NULL)
    restore_cache <- tryCatch(isolate(state$restore_plot_data_cache()), error = function(e) NULL)
    if (is.list(restore_cache) &&
        inherits(restore_cache$data_mod, "data.frame") &&
        inherits(restore_cache$data_def, "data.frame")) {
      # Once a session restore supplied cached plot data/metadata, keep that
      # metadata authoritative for both the first rebuild and later static
      # re-renders. Otherwise live Data Wizard metadata can erase restored
      # sample Conditions after rv$pca_restore_rebuild_expected is consumed.
      results <- pca_attach_cached_restore_metadata(results, restore_cache)
    }

    # During session restore, wait until UI inputs are fully synced and the
    # restore guard is released. Then rebuild fresh from analysis results.
    if (isTRUE(isolate(state$restore_in_progress()))) {
      debug_log("[PCA] Skipping static plot build while session restore is in progress", 2)
      return(NULL)
    }
    if (isTRUE(isolate(rv$pca_restore_rebuild_expected))) {
      debug_log(sprintf("[PCA] restore rebuild observer fired (selected_cache_key=%s, cache_hit=%s)",
                        as.character(isolate(rv$pca_restore_selected_cache_key) %||% NA_character_),
                        as.character(isTRUE(isolate(!is.null(state$restore_plot_data_cache()) &&
                          inherits(state$restore_plot_data_cache()$data_mod, 'data.frame') &&
                          inherits(state$restore_plot_data_cache()$data_def, 'data.frame'))))), 1)
      rv$pca_restore_rebuild_expected <- FALSE
    }

    sample_label_settings_pca()
    sample_labeling_active_pca()
    selected_items_vector_pca()
    item_label_settings_pca()
    available_components()

    isolate({
      render_generation <- state$restore_generation()
      tryCatch({
      debug_log("Creating static plot", 2)

      # Validate that we have the right type of results for current comparison target
      current_target <- input$comparison_target
      if (!is.null(results$comparison_target) && results$comparison_target != current_target) {
        debug_log(paste("Results mismatch: have", results$comparison_target, "but current target is", current_target), 1)
        return(NULL)  # Return NULL to prevent rendering with mismatched data
      }

      # Collect font size parameters from UI
      font_sizes <- list(
        axis_title = input$AxisTitleSize_PCATab,
        tick = input$tickSize_PCATab,
        legend_title = input$LegendTitleSize_PCATab,
        legend_text = input$LegendTextSize_PCATab
      )

      plot_params <- list(
        axis_x = input$axis_x,
        axis_y = input$axis_y,
        point_size = input$point_size,
        #label_size = input$label_size,
        color_palette = input$color_palette,
        reverse_colors = input$reverse_colors,
        identifier_col = input$GeneIdentifierColumn_pca,
        default_protein_color = input$defaultProteinColor_pca,
        debug_log = debug_log
      )

      # Ensure axes are valid for current results
      available_x <- available_components()$x
      available_y <- available_components()$y

      if (length(available_x) == 0 || length(available_y) == 0) {
        debug_log("No available components for axis selection", 1)
        return(NULL)
      }

      if (!plot_params$axis_x %in% available_x) {
        plot_params$axis_x <- available_x[1]
        debug_log(paste("Reset axis_x to:", plot_params$axis_x), 2)
      }
      if (!plot_params$axis_y %in% available_y) {
        plot_params$axis_y <- available_y[1]
        debug_log(paste("Reset axis_y to:", plot_params$axis_y), 2)
      }

      # Get legend position from UI
      legend_position <- input$legend_position %||% "right"

      # ========================================
      # PREPARE LABELING SYSTEM BASED ON MODE
      # ========================================

      enhanced_labeling <- NULL

      if (results$comparison_target == "samples") {
        # SAMPLE LABELING MODE
        sample_settings <- sample_label_settings_pca()
        is_active <- sample_labeling_active_pca()

        if (!is.null(sample_settings) && length(sample_settings) > 0 &&
            isTRUE(is_active) && isTRUE(sample_settings$active)) {

          enhanced_labeling <- list(
            mode = "samples",
            label_all_samples = TRUE,
            master_label_color = sample_settings$master_label_color,
            master_dot_color = sample_settings$master_dot_color,
            use_master_dot_color = sample_settings$use_master_dot_color,
            labeling_params = list(
              max_overlaps = sample_settings$max_overlaps,
              label_distance = sample_settings$label_distance,
              line_thickness = sample_settings$line_thickness,
              label_size = sample_settings$label_size,
              labeled_dot_size = input$dotSizeLabeled_samples %||% 2
            )
          )

          debug_log("Sample labeling prepared for all samples", 2)
        } else {
          debug_log("Sample labeling disabled or no settings", 2)
        }

      } else {
        # PROTEIN LABELING MODE
        selected_items <- selected_items_vector_pca()

        if (!is.null(selected_items) && length(selected_items) > 0) {
          debug_log(paste("Preparing protein labeling for", length(selected_items), "items"), 2)

          enhanced_labeling <- list(
            mode = "proteins",
            selected_items = selected_items,
            item_settings = item_label_settings_pca(),
            default_protein_color = input$defaultProteinColor_pca %||% "#3182bd",
            labeling_params = list(
              max_overlaps = input$maxOverlaps_pca %||% 10,
              label_distance = input$labelDistance_pca %||% 0.25,
              line_thickness = input$lineThickness_pca %||% 0.5,
              label_size = as.numeric(input$labelSize_pca %||% 3) %||% 8,
              labeled_dot_size = input$dotSizeLabeled_pca %||% 2
            )
          )

          debug_log("Protein labeling prepared", 2)
        } else {
          debug_log("No items selected for protein labeling", 2)
        }
      }

      # ========================================
      # CREATE BASE PLOT WITH ENHANCED LABELING
      # ========================================

      # Create base plot with enhanced labeling system
      p <- create_static_plot(
        results = results,
        plot_params = plot_params,
        labeled_proteins = if(is.null(enhanced_labeling)) labeled_proteins() else NULL,
        theme_name = input$plot_theme,
        font_sizes = font_sizes,
        enhanced_labeling = enhanced_labeling,
        legend_position = legend_position
      )

      if (is.null(p)) {
        return(NULL)
      }
      # A rebuild request only means the renderer was invalidated. Completion
      # is recorded here, after create_static_plot() produced a real plot for
      # the same restore generation.
      update_restore_render_report(render_generation, "render_completed")
      debug_log(sprintf(
        "[PCA restore] static plot render completed (pca_generation=%s)",
        render_generation
      ), 1)

      ggplot_object_PCATab(p)
      static_plot_obj(p)
      # The compact analysis result and rendered plot are now authoritative.
      # Keep those (and the canonical cache reference), but release the large
      # restore-only pair only after this generation actually rendered.
      if (isTRUE(isolate(state$restore_in_progress()))) {
        state$restore_plot_data_cache(NULL)
      }
      debug_log(paste("PCA plot rendered: X-axis = PC", input$axis_x,
                      "| Y-axis = PC", input$axis_y), level = 1)
      return(p)

    }, error = function(e) {
      update_restore_render_report(render_generation, "render_failed", e$message)
      debug_log(sprintf(
        "[PCA restore] static plot render failed (pca_generation=%s): %s",
        render_generation, e$message
      ), 1)
      debug_log(paste("Error creating static plot:", e$message), 1)

      # Check if this is the column selection error
      if (grepl("nicht definierte Spalten|undefined columns", e$message, ignore.case = TRUE)) {
        debug_log("Detected column selection error - likely due to comparison target mismatch", 1)
        showNotification("Please run analysis again after changing comparison mode", type = "warning", duration = 5)
      } else {
        showNotification(paste("Plot creation error:", e$message), type = "error", duration = 5)
      }

      return(NULL)
      })
    })
  }, height = 600)

  output$scree_plot <- renderPlot({
    render_nonce()
    plot_input_snapshot <- debounced_pca_plot_inputs()

    results <- analysis_results()
    if (is.null(results) || results$method != "pca" || !isTRUE(plot_input_snapshot$show_scree)) return(NULL)

    # During session restore, defer scree rebuild until restore guard clears.
    if (isTRUE(isolate(state$restore_in_progress()))) {
      debug_log("[PCA] Skipping scree plot build while session restore is in progress", 2)
      return(NULL)
    }

    isolate({
      font_sizes <- list(
      axis_title = input$AxisTitleSize_PCATab,
      tick = input$tickSize_PCATab,
      label_size = as.numeric(input$labelSize_pca %||% 3)
    )

    p <- tryCatch({
      create_scree_plot(results, font_sizes = font_sizes, theme_name = input$plot_theme %||% "theme_minimal", debug_log = debug_log)
    }, error = function(e) {
      debug_log(paste("Error creating scree plot:", e$message), 1)
      return(NULL)
    })

    # Store for grid usage
    scree_plot_obj(p)

    return(p)
    })
  }, height = 400)

  # Apply Sample Labeling Observer
  observeEvent(input$applySampleLabeling_pca, {
    tryCatch({
      results <- analysis_results()

      if (is.null(results) || results$comparison_target != "samples") {
        showNotification("Sample labeling only available in samples comparison mode", type = "warning")
        return()
      }

      debug_log("Applying sample labeling to ALL samples", 2)

      # Create sample labeling settings from UI inputs
      sample_labeling_settings <- list(
        # Master colors
        master_label_color = input$masterLabelColor_samples %||% "#000000",
        master_dot_color = input$masterDotColor_samples %||% "#E0E0E0",
        use_master_dot_color = input$masterCustomDot_samples %||% FALSE,

        # Labeling settings
        max_overlaps = input$maxOverlaps_samples %||% 10,
        label_distance = input$labelDistance_samples %||% 0.25,
        line_thickness = input$lineThickness_samples %||% 0.5,
        label_size = input$labelSize_samples %||% 8,
        labeled_dot_size = input$dotSizeLabeled_samples %||% 2,

        # State
        active = TRUE
      )

      # Store sample labeling settings
      sample_label_settings_pca(sample_labeling_settings)
      sample_labeling_active_pca(TRUE)

      debug_log("Sample labeling settings applied - plot will update automatically", 2)
      debug_log(paste("Settings: Label Color =", sample_labeling_settings$master_label_color,
                      "| Custom Dots =", sample_labeling_settings$use_master_dot_color), 2)

      showNotification("Applied labeling to all samples in the plot", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("ERROR in applySampleLabeling_pca:", e$message), 1)
      showNotification(paste("Error applying sample labeling:", e$message), type = "error")
    })
  })

  # Reset Label Settings Observer - ONLY RESETS UI
  observeEvent(input$resetLabelSettings_pca, {
    tryCatch({
      # Reset sample-specific master controls
      updateColourInput(session, "masterLabelColor_samples", value = "#000000")
      updateColourInput(session, "masterDotColor_samples", value = "#E0E0E0")
      updateCheckboxInput(session, "masterCustomDot_samples", value = FALSE)

      # Reset labeling settings
      updateNumericInput(session, "maxOverlaps_pca", value = 10)
      updateNumericInput(session, "labelDistance_pca", value = 0.25)
      updateNumericInput(session, "lineThickness_pca", value = 0.5)
      updateNumericInput(session, "labelSize_pca", value = 8)
      updateNumericInput(session, "dotSizeLabeled_pca", value = 2)
      updateNumericInput(session, "dotSizeLabeled_samples", value = 2)
      updateColourInput(session, "defaultProteinColor_pca", value = "#3182bd")

      debug_log("Reset sample labeling UI settings to defaults", 2)
      showNotification("Reset label settings to defaults", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error resetting label settings:", e$message), 1)
      showNotification("Error resetting label settings", type = "error")
    })
  })

  # Clear Sample Labels Observer - REMOVES APPLIED LABELS
  observeEvent(input$clearSampleLabels_pca, {
    tryCatch({
      # Clear sample labeling settings and deactivate
      sample_label_settings_pca(list())
      sample_labeling_active_pca(FALSE)

      debug_log("Cleared all sample labels", 2)
      showNotification("Cleared all sample labels from plot", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error clearing sample labels:", e$message), 1)
      showNotification("Error clearing sample labels", type = "error")
    })
  })

  # ========================================
  # Automatic Re-Run on Identifier Change
  # ========================================
  observeEvent(input$GeneIdentifierColumn_pca, {
    # Restored analysis results and their cache are already authoritative. Do
    # not inspect or mutate them while the restore cascade updates this input.
    if (isTRUE(state$restore_in_progress())) {
      debug_log("[PCA] Skipping identifier reanalysis during session restore", 2)
      return()
    }

    new_id <- input$GeneIdentifierColumn_pca
    restored_id <- isolate(state$restored_identifier_column())
    expected_echoes <- isolate(state$expected_restore_input_echoes()) %||% list()
    expected_identifier_echo <- expected_echoes$GeneIdentifierColumn_pca
    if (!is.null(expected_identifier_echo) && identical(expected_identifier_echo, new_id)) {
      expected_echoes$GeneIdentifierColumn_pca <- NULL
      state$expected_restore_input_echoes(expected_echoes)
      debug_log(paste0("[PCA] Consumed restored identifier UI echo (", new_id, ")"), 2)
      return()
    }
    if (!is.null(restored_id)) {
      # Consume exactly one matching input echo after the restore guard drops.
      # A differing value is a genuine user change, so discard the stale marker
      # and preserve the existing re-analysis behavior below.
      state$restored_identifier_column(NULL)
      if (identical(restored_id, new_id)) {
        debug_log(paste0("[PCA] Consumed restored identifier echo (", new_id, ")"), 2)
        return()
      }
    }

    # Ensure an analysis exists first
    if (is.null(analysis_results())) {
      return()
    }

    if (is.null(new_id) || new_id == "") return()

    results <- analysis_results()

    # If nothing changed, do nothing
    if (!is.null(results$identifier_col) && identical(results$identifier_col, new_id)) {
      debug_log(paste("Identifier unchanged (", new_id, ") - skipping re-analysis", sep = ""), 2)
      return()
    }

    debug_log(paste("Identifier column changed to:", new_id), 1)

    # SAMPLE MODE: only update identifier_col (no need to re-run)
    if (!is.null(results$comparison_target) && results$comparison_target == "samples") {
      results$identifier_col <- new_id
      analysis_results(results)
      showNotification("Identifier updated (samples mode) – no re-run required.", type = "message", duration = 3)
      return()
    }

    # PROTEIN MODE: full re-run required
    if (!is.null(results$comparison_target) && results$comparison_target == "proteins") {
      debug_log("Re-running analysis for protein mode with new identifier", 1)

      progress <- shiny::Progress$new()
      on.exit(progress$close(), add = TRUE)
      progress$set(message = "Updating identifiers...", value = 0)

      # Rebuild prepared data with new identifier
      prep_data <- prepare_pca_analysis_data(
        data = results$full_data,
        metadata = results$raw_metadata,
        selected_data_type = results$selected_data_type,
        # reuse stored sample selection (all inside results)
        selected_samples = results$selected_samples,
        selected_identifier = new_id,
        debug_log = debug_log
      )

      if (is.null(prep_data)) {
        showNotification("Failed to rebuild data with new identifier", type = "error", duration = 5)
        debug_log("Aborting identifier re-run: prepare_pca_analysis_data returned NULL", 1)
        return()
      }
      progress$inc(0.4, detail = "Prepared data")

      # Build analysis matrix for protein comparison (no transpose)
      analysis_matrix <- prep_data$data
      point_names <- prep_data$identifiers

      method <- results$method
      debug_log(paste("Re-running method:", method), 2)
      progress$inc(0.55, detail = paste("Running", toupper(method)))

      new_core_results <- switch(
        method,
        "pca"  = perform_pca(analysis_matrix, input, debug_log),
        "umap" = perform_umap(analysis_matrix, input, debug_log),
        NULL
      )

      if (is.null(new_core_results)) {
        showNotification("Analysis failed after identifier change", type = "error", duration = 5)
        debug_log("New core results are NULL - aborting", 1)
        return()
      }

      progress$inc(0.75, detail = "Finalizing results")

      # Transfer essential fields
      new_core_results$point_names        <- point_names
      new_core_results$comparison_target  <- "proteins"
      new_core_results$method             <- method
      new_core_results$metadata           <- prep_data$metadata
      new_core_results$raw_metadata       <- results$raw_metadata
      new_core_results$identifier_col     <- new_id
      new_core_results$selected_samples   <- results$selected_samples
      new_core_results$selected_data_type <- results$selected_data_type
      new_core_results$full_data          <- results$full_data

      invariant_plot_data <- tryCatch(
        create_plot_data(
          new_core_results,
          new_core_results$raw_metadata %||% new_core_results$metadata,
          new_id,
          debug_log = debug_log
        ),
        error = function(e) {
          debug_log(paste("Protein identifier re-run alignment failed:", e$message), 1)
          NULL
        }
      )
      invariant_counts <- c(
        analysis_matrix = nrow(analysis_matrix),
        coordinates = nrow(new_core_results$coordinates),
        point_names = length(point_names),
        plot_data = if (is.null(invariant_plot_data)) NA_integer_ else nrow(invariant_plot_data)
      )
      if (anyNA(invariant_counts) || length(unique(invariant_counts)) != 1L) {
        diagnostic <- sprintf(
          paste0("Protein row alignment failed after identifier change: analysis matrix=%d, ",
                 "coordinates=%d, point_names=%d, plot data=%s. The analysis was not saved."),
          invariant_counts[["analysis_matrix"]], invariant_counts[["coordinates"]],
          invariant_counts[["point_names"]], as.character(invariant_counts[["plot_data"]])
        )
        debug_log(diagnostic, 1)
        showNotification(diagnostic, type = "error", duration = 8)
        return()
      }
      new_core_results$point_names <- as.character(point_names)
      new_core_results$coordinate_identifiers <- as.character(point_names)

      # Store
      analysis_results(new_core_results)
      plots_ready(TRUE)

      # Clear previous selections & labeling (they reference old identifiers)
      selected_items_vector_pca(character())
      item_label_settings_pca(data.frame(
        item_id = character(),
        label_color = character(),
        dot_color = character(),
        use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))
      selected_data_pca(data.frame())
      selected_protein_vector_pca(character())

      progress$inc(0.95, detail = "Clearing old selections")

      showNotification("Identifier changed: analysis re-run. Previous selections cleared.",
                       type = "message", duration = 5)
      debug_log("Identifier change handling complete", 2)
    }
  }, ignoreNULL = TRUE)

  # ========================================
  # Interactive Plot Output
  # ========================================

  output$interactive_plot_output <- renderPlotly({
    render_nonce()
    debounced_pca_plot_inputs()

    if (!plots_ready()) {
      debug_log("plots_ready() is FALSE - returning NULL", 2)
      return(NULL)
    }

    results <- analysis_results()
    if (is.null(results)) {
      debug_log("analysis_results() is NULL - returning NULL", 2)
      return(NULL)
    }
    restore_cache <- tryCatch(isolate(state$restore_plot_data_cache()), error = function(e) NULL)
    if (is.list(restore_cache) &&
        inherits(restore_cache$data_mod, "data.frame") &&
        inherits(restore_cache$data_def, "data.frame")) {
      # Once a session restore supplied cached plot data/metadata, keep that
      # metadata authoritative for both the first rebuild and later static
      # re-renders. Otherwise live Data Wizard metadata can erase restored
      # sample Conditions after rv$pca_restore_rebuild_expected is consumed.
      results <- pca_attach_cached_restore_metadata(results, restore_cache)
    }

    # During session restore, wait until metadata-dependent UI inputs have
    # echoed back to Shiny before recreating the plotly object from saved
    # coordinates and restored settings.
    if (isTRUE(isolate(state$restore_in_progress()))) {
      debug_log("[PCA] Skipping interactive plot build while session restore is in progress", 2)
      return(NULL)
    }

    isolate({
    render_generation <- state$restore_generation()
    # Collect font size parameters from UI
    font_sizes <- list(
      axis_title = input$AxisTitleSize_PCATab,
      tick = input$tickSize_PCATab,
      legend_title = input$LegendTitleSize_PCATab,
      legend_text = input$LegendTextSize_PCATab
    )

    # Get theme name from UI (THIS WAS MISSING!)
    theme_name <- input$plot_theme %||% "theme_minimal"
    legend_position <- input$legend_position %||% "right"  # NEW: Get legend position
    debug_log(paste("Theme from UI:", theme_name, "| Legend position:", legend_position), 2)

    tryCatch({
      debug_log("Creating interactive plot", 2)

      plot_params <- list(
        axis_x = input$axis_x,
        axis_y = input$axis_y,
        point_size = input$point_size,
        color_palette = input$color_palette,
        reverse_colors = input$reverse_colors,
        identifier_col = input$GeneIdentifierColumn_pca,
        debug_log = debug_log
      )

      # Ensure axes are valid
      if (!plot_params$axis_x %in% available_components()$x) {
        plot_params$axis_x <- available_components()$x[1]
      }
      if (!plot_params$axis_y %in% available_components()$y) {
        plot_params$axis_y <- available_components()$y[1]
      }

      debug_log(paste("Final axes:", plot_params$axis_x, "vs", plot_params$axis_y), 2)

      # PASS THEME_NAME TO INTERACTIVE PLOT (THIS WAS MISSING!)
      p <- create_pca_interactive_plot(results,
                                       plot_params,
                                       font_sizes = font_sizes,
                                       theme_name = theme_name,
                                       legend_position = legend_position
                                       )

      if (is.null(p)) {
        debug_log("create_pca_interactive_plot returned NULL", 1)
        return(NULL)
      }

      update_restore_render_report(render_generation, "render_completed")
      debug_log("Interactive plot created successfully", 2)

      interactive_plot_obj(p)
      return(p)

    }, error = function(e) {
      update_restore_render_report(render_generation, "render_failed", e$message)
      debug_log(paste("Error creating interactive plot:", e$message), 1)
      debug_log(paste("Error class:", class(e)), 1)
      debug_log(paste("Error traceback:", paste(traceback(), collapse = " | ")), 1)
      return(NULL)
      })
    })
  })

  # Show scree tab only when PCA mode and the scree plot option are active
  observe({
    show_scree_tab <- identical(input$analysis_method %||% "pca", "pca") &&
      isTRUE(input$show_scree)

    if (show_scree_tab) {
      shiny::showTab(inputId = "plot_tabs", target = "scree_tab", session = session)
    } else {
      shiny::updateTabsetPanel(session, "plot_tabs", selected = "Main Plot")
      shiny::hideTab(inputId = "plot_tabs", target = "scree_tab", session = session)
    }
  })

  # ========================================
  # Interactive Plot Selection Handling
  # ========================================

  # Box/lasso selection handler for protein scatter mode
  observeEvent(event_data("plotly_selected", source = "pca_plot"), {

    selection <- event_data("plotly_selected", source = "pca_plot")

    if (is.null(selection) || !is.data.frame(selection)) {
      debug_log("Selection is NULL or not a data.frame - ignoring event", 2)
      return()
    }

    n_sel <- suppressWarnings(nrow(selection))
    if (is.na(n_sel) || n_sel == 0) {
      debug_log("Empty selection (0 rows or NA) - ignoring event", 2)
      return()
    }

    results <- analysis_results()
    if (is.null(results) || results$comparison_target != "proteins") {
      return()
    }

    debug_log(paste("Processing box/lasso selection with", n_sel, "points"), 1)

    tryCatch({
      plot_data <- create_plot_data(results, results$raw_metadata %||% results$metadata, input$GeneIdentifierColumn_pca, debug_log = debug_log)
      coords <- get_plot_coordinates(results, input$axis_x, input$axis_y)
      plot_data$x <- coords$x
      plot_data$y <- coords$y

      selected_data <- data.frame()

      if ("pointNumber" %in% colnames(selection)) {
        selected_indices <- selection$pointNumber + 1
        selected_indices <- selected_indices[selected_indices <= nrow(plot_data)]
        if (length(selected_indices) > 0) {
          selected_data <- plot_data[selected_indices, , drop = FALSE]
        }
      }

      if (nrow(selected_data) > 0) {
        selected_data <- selected_data[!duplicated(selected_data$Name), , drop = FALSE]
      }

      current_selection <- selected_points_interactive()
      is_additive <- isTRUE(input$additive_selection)

      if (!is_additive && !is.null(current_selection) && nrow(current_selection) > 0 && nrow(selected_data) > 0) {
        common_proteins <- intersect(current_selection$Name, selected_data$Name)
        if (length(common_proteins) > 0) {
          debug_log("Detected overlapping selection - treating as additive (Ctrl+drag)", 2)
          is_additive <- TRUE
        }
      }

      if (is_additive) {
        if (!is.null(current_selection) && nrow(current_selection) > 0) {
          all_data <- rbind(current_selection, selected_data)
          final_selection <- all_data[!duplicated(all_data$Name), , drop = FALSE]
        } else {
          final_selection <- selected_data
        }
        selected_points_interactive(final_selection)
        debug_log(paste("Additive selection completed:", nrow(final_selection), "total proteins"), 1)
        showNotification(paste("Total selected:", nrow(final_selection), "proteins"),
                         type = "message", duration = 2)
      } else {
        selected_points_interactive(selected_data)
        debug_log(paste("Box/lasso selection processed:", nrow(selected_data), "proteins selected"), 1)
        showNotification(paste("Selected", nrow(selected_data), "proteins"),
                         type = "message", duration = 2)
      }

    }, error = function(e) {
      debug_log(paste("Error handling box/lasso selection:", e$message), 1)
    })
  })

  # Single-point click handler for protein scatter mode
  observeEvent(event_data("plotly_click", source = "pca_plot"), {

    click_data <- event_data("plotly_click", source = "pca_plot")

    if (is.null(click_data)) {
      debug_log("No click data received", 2)
      return()
    }

    results <- analysis_results()
    if (is.null(results) || results$comparison_target != "proteins") {
      return()
    }

    tryCatch({
      plot_data <- create_plot_data(results, results$raw_metadata %||% results$metadata, input$GeneIdentifierColumn_pca, debug_log = debug_log)
      coords <- get_plot_coordinates(results, input$axis_x, input$axis_y)
      plot_data$x <- coords$x
      plot_data$y <- coords$y

      clicked_point <- NULL

      if ("pointNumber" %in% colnames(click_data)) {
        point_idx <- click_data$pointNumber[1] + 1
        if (point_idx > 0 && point_idx <= nrow(plot_data)) {
          clicked_point <- plot_data[point_idx, , drop = FALSE]
          debug_log(paste("Found clicked point:", clicked_point$Name), 2)
        }
      }

      if (!is.null(clicked_point)) {
        current_selection <- selected_points_interactive()
        additive_mode <- isTRUE(input$additive_selection)

        if (additive_mode) {
          if (is.null(current_selection) || nrow(current_selection) == 0) {
            new_selection <- clicked_point
          } else {
            if (clicked_point$Name %in% current_selection$Name) {
              new_selection <- current_selection[current_selection$Name != clicked_point$Name, , drop = FALSE]
              debug_log(paste("Removed from selection:", clicked_point$Name), 1)
            } else {
              new_selection <- rbind(current_selection, clicked_point)
              debug_log(paste("Added to selection:", clicked_point$Name), 1)
            }
          }
          showNotification(paste("Total selected:", nrow(new_selection), "proteins"),
                           type = "message", duration = 1.5)
        } else {
          new_selection <- clicked_point
          showNotification(paste("Selected:", clicked_point$Name),
                           type = "message", duration = 1.5)
        }

        selected_points_interactive(new_selection)
      }

    }, error = function(e) {
      debug_log(paste("Error handling click:", e$message), 1)
    })
  })

  # Clear interactive plot selection
  observeEvent(input$clear_selection, {
    selected_points_interactive(data.frame())
    showNotification("Selection cleared", type = "message", duration = 2)
    debug_log("Interactive selection cleared", 2)
  })

  # ========================================
  # Selection Display Outputs
  # ========================================

  output$selected_items_display <- renderText({
    selected <- selected_points_interactive()
    if (is.null(selected) || nrow(selected) == 0) return("No proteins selected")
    paste("Selected", nrow(selected), "proteins")
  })

  output$selected_items_list <- renderText({
    selected <- selected_points_interactive()
    if (is.null(selected) || nrow(selected) == 0) {
      return("Select proteins in the plot above to see them here...")
    }
    paste(selected$Name, collapse = "\n")
  })

  # ========================================
  # Module Communication
  # ========================================

  output$plots_ready <- reactive({
    if (plots_ready()) "true" else "false"
  })
  outputOptions(output, "plots_ready", suspendWhenHidden = FALSE)

}


register_pca_export_handlers <- function(input, output, session, rv, state, modEnv, ns, debug_log) {
  analysis_results <- state$analysis_results
  labeled_proteins <- state$labeled_proteins
  ggplot_object_PCATab <- state$ggplot_object_PCATab
  static_plot_obj <- state$static_plot_obj
  scree_plot_obj <- state$scree_plot_obj


  pca_plot_choices <- function() {
    results <- analysis_results()
    result_method <- if (!is.null(results)) results$method else NULL

    if (identical(input$analysis_method, "umap") || identical(result_method, "umap")) {
      list("Main Plot" = "main")
    } else {
      list("Main Plot" = "main", "Scree Plot" = "scree")
    }
  }

  selected_pca_plot_type <- function() {
    selected <- input$plot_type_grid %||% "main"
    available_values <- unname(unlist(pca_plot_choices()))

    if (selected %in% available_values) selected else "main"
  }

  get_selected_pca_plot <- function(plot_type = selected_pca_plot_type(), for_grid = FALSE) {
    if (identical(plot_type, "scree")) {
      results <- analysis_results()
      if (is.null(results) || !identical(results$method, "pca")) {
        showNotification("Scree plot is only available for PCA analysis.", type = "message")
        return(NULL)
      }

      p <- scree_plot_obj()
      if (is.null(p)) {
        font_sizes <- list(
          axis_title = input$AxisTitleSize_PCATab,
          tick = input$tickSize_PCATab,
          label_size = as.numeric(input$labelSize_pca %||% 3)
        )
        p <- tryCatch({
          create_scree_plot(results, font_sizes = font_sizes, theme_name = input$plot_theme %||% "theme_minimal", debug_log = debug_log)
        }, error = function(e) {
          debug_log(paste("Error creating scree plot:", e$message), 1)
          showNotification("Error creating scree plot", type = "error", duration = 3)
          NULL
        })
        if (!is.null(p)) scree_plot_obj(p)
      }
      return(p)
    }

    if (isTRUE(for_grid)) static_plot_obj() else ggplot_object_PCATab()
  }

  observe({
    choices <- pca_plot_choices()
    selected <- selected_pca_plot_type()

    updateSelectInput(session, "plot_type_grid",
                      choices = choices,
                      selected = selected)
  })

  output$downloadPlotButton_PCATab <- downloadHandler(
    filename = function() {
      format_ext <- input$downloadFormat_PCATab %||% "png"
      plot_type <- selected_pca_plot_type()
      prefix <- if (identical(plot_type, "scree")) "PCA_scree_plot" else "PCA_plot"
      paste0(prefix, "_", Sys.Date(), ".", format_ext)
    },
    content = function(file) {
      plot_type <- selected_pca_plot_type()
      p <- get_selected_pca_plot(plot_type)
      req(p)

      widthIn  <- input$plotWidthInch_PCATab
      heightIn <- input$plotHeightInch_PCATab
      res      <- input$resolution_DPI_PCATab
      format   <- input$downloadFormat_PCATab %||% "png"

      switch(format,
             png  = { png (file, width = widthIn, height = heightIn, units = "in", res = res) },
             tiff = { tiff(file, width = widthIn, height = heightIn, units = "in", res = res) },
             jpeg = { jpeg(file, width = widthIn, height = heightIn, units = "in", res = res) },
             svg  = { svg (file, width = widthIn, height = heightIn) },
             pdf  = { pdf (file, width = widthIn, height = heightIn) }
      )
      on.exit(try(dev.off(), silent = TRUE), add = TRUE)
      print(p)
    }
  )

  observeEvent(input$add_to_grid, {
    debug_log("PCA: add_to_grid clicked", 2)

    plot_type <- selected_pca_plot_type()
    plot_type_label <- if (identical(plot_type, "scree")) "Scree" else "Main"
    p <- get_selected_pca_plot(plot_type, for_grid = TRUE)

    if (is.null(p)) {
      showNotification(paste(plot_type_label, "plot not available. Please run analysis first."), type = "error")
      return()
    }

    if (!inherits(p, "ggplot")) {
      showNotification("Only ggplot objects can be added to the grid.", type = "error")
      return()
    }

    sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
    lbl_raw <- input$grid_label
    lbl_id <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
    plot_id <- paste0(ns(""), "PCA_", plot_type, "_", lbl_id)

    lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else paste("PCA", plot_type_label)

    debug_log(paste("PCA: adding to grid id=", plot_id), 2)
    modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "PCA")
    showNotification(paste(plot_type_label, "plot added to grid selection."), type = "message")
  })
}
