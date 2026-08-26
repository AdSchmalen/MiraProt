# Observer registration group extracted from volcano_observers.R.
# Reactive state and plot objects are supplied by the module entry point.

register_volcano_plot_lifecycle_observers <- function(
    input, output, session, rv,
    res_GSEA, GO_res, module_outputs,
    volcano_state, plot_update_trigger,
    selected_data_Volcano, selected_protein_vector_Volcano,
    volcano_original_plots, volcano_labels, protein_label_settings,
    selected_points_interactive_Volcano,
    data_in, data_def_in, debug_log, ns, modEnv,
    compute_data_signature, has_dataset_mismatch_with_existing_plots,
    resolve_plot_selection, reset_preplot_skip_log_flags, update_identifier_choices
) {
  safe_axis_fallback <- list(x_range = c(-8, 8), y_range = c(0, 18), x_tick = 2, y_tick = 2)
  valid_axis_settings <- function(settings) {
    is.list(settings) && length(settings$x_range) == 2L && length(settings$y_range) == 2L &&
      length(settings$x_tick) == 1L && length(settings$y_tick) == 1L &&
      all(is.finite(c(settings$x_range, settings$y_range, settings$x_tick, settings$y_tick))) &&
      settings$x_range[1] < settings$x_range[2] && settings$y_range[1] < settings$y_range[2] &&
      settings$x_tick > 0 && settings$y_tick > 0
  }
  effective_axis_settings <- function(title) {
    overrides <- volcano_state$plot_axis_overrides %||% list()
    automatic <- volcano_state$plot_axis_settings %||% list()
    if (!is.null(title) && valid_axis_settings(overrides[[title]])) return(overrides[[title]])
    if (!is.null(title) && valid_axis_settings(automatic[[title]])) return(automatic[[title]])
    safe_axis_fallback
  }
  # ============================================================================
  # SECTION 5: Plot Selection and Label Management
  # ============================================================================

  # Auto-select a valid plot when titles are available
  observe({
    req(volcano_state$plot_titles)
    current_selection <- input$PlotSelect_Volcano
    available_titles <- volcano_state$plot_titles

    if (!is.null(available_titles) && length(available_titles) > 0) {
      if (is.null(current_selection) || current_selection == "none" ||
          !current_selection %in% available_titles) {
        selected_title <- resolve_plot_selection(available_titles)
        debug_log(paste("Auto-selecting valid plot:", selected_title), 2)
        updateSelectInput(session, "PlotSelect_Volcano", selected = selected_title)
      }
    }
  })

  calculate_optimal_ranges_for_selected_plot <- function(data, data_def, selected_title = NULL,
                                                         pairing_result = NULL,
                                                         source_data_signature = NULL) {
    pairs <- NULL
    tryCatch({
      if (!is.data.frame(data) || !is.data.frame(data_def)) {
        return(list(x_range = c(-8, 8), y_range = c(0, 18), x_tick = 2, y_tick = 2))
      }

      pval_pref <- input$pValueSel_Volcano %||% "Adjusted p-value"
      if (has_valid_cached_pairing_result(pairing_result, source_data_signature)) {
        debug_log("Using cached column pairs for selected-plot auto-range", 2)
      } else {
        debug_log("Cached column pairs missing or stale; recomputing for selected-plot auto-range", 2)
        pairing_result <- find_ratio_pvalue_pairs_smart(data_def, pval_pref, debug_log)
      }
      pairs <- pairing_result$pairs
      if (is.null(pairs) || length(pairs) == 0) {
        return(calculate_optimal_ranges(data, data_def, debug_log, pairs = pairs))
      }

      plot_titles <- vapply(pairs, function(pair) generate_plot_title_from_pair(pair), character(1))
      chosen_idx <- if (!is.null(selected_title) && nzchar(selected_title) && selected_title %in% plot_titles) {
        which(plot_titles == selected_title)[1]
      } else {
        1L
      }

      pair <- pairs[[chosen_idx]]
      plot_df <- prepare_volcano_plot_data_safe(data, data_def, pair$ratio_idx, pair$pval_idx, debug_log)
      if (is.null(plot_df) || nrow(plot_df) == 0) {
        return(calculate_optimal_ranges(data, data_def, debug_log, pairs = pairs))
      }

      volcano_axis_settings_from_values(plot_df$x, plot_df$y, debug_log)
    }, error = function(e) {
      debug_log(paste("Error calculating selected-plot optimal range:", e$message), 1)
      calculate_optimal_ranges(data, data_def, debug_log, pairs = pairs)
    })
  }

  axis_input_ids <- c(
    "xLimInput_Volcano", "yLimInput_Volcano",
    "xTick_Volcano", "yTick_Volcano"
  )

  suppress_next_axis_input_updates <- function(ids = axis_input_ids) {
    volcano_state$axis_update_suppressed_inputs <- unique(c(
      volcano_state$axis_update_suppressed_inputs %||% character(0),
      ids
    ))
    invisible(NULL)
  }

  axis_input_suppression_pending <- function(id) {
    id %in% (isolate(volcano_state$axis_update_suppressed_inputs) %||% character(0))
  }

  consume_axis_input_suppression <- function(id) {
    suppressed <- isolate(volcano_state$axis_update_suppressed_inputs) %||% character(0)
    if (id %in% suppressed) {
      volcano_state$axis_update_suppressed_inputs <- setdiff(suppressed, id)
      debug_log(paste("Suppressed programmatic axis input update:", id), 2)
      return(TRUE)
    }
    FALSE
  }

  suppress_plot_selection_update <- function(selected_title = NULL) {
    volcano_state$plot_selection_update_in_progress <- TRUE
    volcano_state$plot_selection_update_pending_value <- selected_title

    # Safety net for no-op selector updates: if Shiny does not emit an input
    # event because the selected value is already current, clear the guard after
    # the client has had two flush cycles to consume the pending update.
    session$onFlushed(function() {
      session$onFlushed(function() {
        if (isTRUE(isolate(volcano_state$plot_selection_update_in_progress))) {
          volcano_state$plot_selection_update_in_progress <- FALSE
          volcano_state$plot_selection_update_pending_value <- NULL
          debug_log("Plot selector update guard cleared after UI settled", 2)
        }
      }, once = TRUE)
    }, once = TRUE)

    invisible(NULL)
  }

  consume_plot_selection_update_suppression <- function(selected_title) {
    if (isTRUE(isolate(volcano_state$plot_selection_update_in_progress))) {
      pending_value <- isolate(volcano_state$plot_selection_update_pending_value)
      if (is.null(pending_value) || identical(selected_title, pending_value)) {
        volcano_state$plot_selection_update_in_progress <- FALSE
        volcano_state$plot_selection_update_pending_value <- NULL
        debug_log(paste("Suppressed programmatic plot selector update:", selected_title), 2)
        return(TRUE)
      }
    }
    FALSE
  }

  axis_input_value_changed <- function(current_value, updated_value) {
    if (is.null(current_value)) return(TRUE)
    !isTRUE(all.equal(current_value, updated_value, check.attributes = FALSE))
  }

  update_axis_ui_controls <- function(optimal, reason = "", update_slider_bounds = FALSE) {
    if (!valid_axis_settings(optimal)) optimal <- safe_axis_fallback

    volcano_state$auto_axis_update_in_progress <- TRUE
    on.exit({ volcano_state$auto_axis_update_in_progress <- FALSE }, add = TRUE)

    x_slider_value <- optimal$x_range
    y_slider_value <- optimal$y_range
    x_capacity <- max(10, max(abs(x_slider_value)) * 1.1)
    y_capacity <- max(50, y_slider_value[2] * 1.1)

    suppress_next_axis_input_updates(c(
      if (axis_input_value_changed(isolate(input$xLimInput_Volcano), x_slider_value)) "xLimInput_Volcano",
      if (axis_input_value_changed(isolate(input$yLimInput_Volcano), y_slider_value)) "yLimInput_Volcano",
      if (axis_input_value_changed(isolate(input$xTick_Volcano), optimal$x_tick)) "xTick_Volcano",
      if (axis_input_value_changed(isolate(input$yTick_Volcano), optimal$y_tick)) "yTick_Volcano"
    ))

    updateSliderInput(session, "xLimInput_Volcano", min = -x_capacity, max = x_capacity, value = x_slider_value)
    updateSliderInput(session, "yLimInput_Volcano", min = 0, max = y_capacity, value = y_slider_value)
    updateNumericInput(session, "xTick_Volcano", value = optimal$x_tick)
    updateNumericInput(session, "yTick_Volcano", value = optimal$y_tick)

    volcano_state$auto_range_set <- TRUE

    debug_log(paste0("Axis UI updated",
                     if (nzchar(reason)) paste0(" (", reason, ")") else "",
                     " - X:", paste(optimal$x_range, collapse = ","),
                     " tick:", optimal$x_tick,
                     " Y:", paste(optimal$y_range, collapse = ","),
                     " tick:", optimal$y_tick), 1)
    invisible(TRUE)
  }

  apply_auto_range_for_selected_plot <- function(reason = "") {
    if (isTRUE(volcano_state$restore_in_progress)) {
      debug_log("Skipping auto-range recompute while restore is in progress", 2)
      return(invisible(FALSE))
    }

    data <- data_in()
    data_def <- data_def_in()
    if (isTRUE(has_dataset_mismatch_with_existing_plots()) &&
        is.list(volcano_state$plot_creation_cache) &&
        inherits(volcano_state$plot_creation_cache$data_mod, "data.frame") &&
        inherits(volcano_state$plot_creation_cache$data_def, "data.frame")) {
      data <- volcano_state$plot_creation_cache$data_mod
      data_def <- volcano_state$plot_creation_cache$data_def
      debug_log("Auto-range uses cached plot-creation data due dataset mismatch", 2)
    }
    if (is.null(data) || is.null(data_def)) return(invisible(FALSE))

    selected_title <- input$PlotSelect_Volcano %||% NULL
    optimal <- NULL
    if (is.character(selected_title) && length(selected_title) == 1L &&
        nzchar(selected_title) && selected_title != "none" &&
        is.list(volcano_state$plot_axis_settings) &&
        selected_title %in% names(volcano_state$plot_axis_settings)) {
      optimal <- effective_axis_settings(selected_title)
      debug_log(paste("Using effective stored axis settings for rendered plot:", selected_title), 2)
    }

    if (is.null(optimal)) {
      source_data_signature <- compute_data_signature(data, data_def)
      optimal <- calculate_optimal_ranges_for_selected_plot(
        data, data_def, selected_title,
        pairing_result = volcano_state$current_pairs,
        source_data_signature = source_data_signature
      )
    }

    update_axis_ui_controls(optimal, reason = paste("auto-range", reason), update_slider_bounds = FALSE)
  }

  observeEvent(input$PlotSelect_Volcano, {
    req(input$PlotSelect_Volcano)
    if (consume_plot_selection_update_suppression(input$PlotSelect_Volcano)) {
      if (!identical(input$PlotSelect_Volcano, "none")) {
        volcano_state$preferred_plot_title <- input$PlotSelect_Volcano
      }
      return()
    }
    if (isTRUE(volcano_state$restore_in_progress)) {
      debug_log("Plot selection change observed during restore; deferring auto-range update", 2)
      return()
    }
    if (!isTRUE(volcano_state$auto_axis_update_in_progress)) {
      volcano_state$auto_range_set <- FALSE
      debug_log("Plot selection changed - axis auto mode reset", 2)
      if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0 &&
          input$PlotSelect_Volcano != "none") {
        volcano_state$preferred_plot_title <- input$PlotSelect_Volcano
        apply_auto_range_for_selected_plot(reason = "plot switch")
      }
    }
  }, ignoreInit = TRUE)

  # Store original plots only when a newly generated plot set differs from
  # the last stored baseline. Label/render observers should not refresh this
  # baseline, otherwise label edits can overwrite the pristine generated plots.
  get_static_plots_signature <- function() {
    static_plots <- volcano_state$static_plots
    if (is.null(static_plots) || length(static_plots) == 0) {
      return(NA_character_)
    }

    plot_names <- names(static_plots)
    if (is.null(plot_names)) {
      plot_names <- rep("", length(static_plots))
    }

    paste(
      paste(plot_names, collapse = "|"),
      length(static_plots),
      volcano_state$source_data_signature %||% "",
      volcano_state$plot_generation_counter %||% 0L,
      sep = "|"
    )
  }

  store_original_plots <- function() {
    if (is.null(volcano_state$static_plots) || length(volcano_state$static_plots) == 0) {
      return(invisible(FALSE))
    }

    current_signature <- get_static_plots_signature()
    if (identical(volcano_state$last_stored_original_plots_signature %||% NA_character_,
                  current_signature)) {
      return(invisible(FALSE))
    }

    original_plots <- vector("list", length(volcano_state$static_plots))
    names(original_plots) <- names(volcano_state$static_plots)
    for (plot_index in seq_along(volcano_state$static_plots)) {
      original_plots[[plot_index]] <- volcano_state$static_plots[[plot_index]]
    }
    volcano_original_plots(original_plots)
    volcano_state$last_stored_original_plots_signature <- current_signature
    debug_log(paste("Stored", length(original_plots), "original plots"), 2)
    invisible(TRUE)
  }

  # Clear labels
  observeEvent(input$clearLabels_Volcano, {
    tryCatch({
      selected_plot_title <- input$PlotSelect_Volcano

      if (is.null(selected_plot_title) || selected_plot_title == "none") {
        if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
          selected_plot_title <- names(volcano_state$static_plots)[1]
        } else {
          showNotification("No plot selected", type = "warning")
          return()
        }
      }

      current_all_labels <- volcano_labels()
      current_all_labels[[selected_plot_title]] <- data.frame()
      volcano_labels(current_all_labels)

      plot_update_trigger(plot_update_trigger() + 1)

      debug_log("Cleared all labels from plot", 1)
      showNotification("Cleared all labels from plot", type = "message", duration = 3)
    }, error = function(e) {
      debug_log(paste("Error clearing labels:", e$message), 1)
    })
  })

  # Clear protein selection (quick clear from protein selection section)
  observeEvent(input$clearButton_Volcano, {
    tryCatch({
      selected_data_Volcano(NULL)
      selected_protein_vector_Volcano(character())
      debug_log("Cleared all selected proteins via Clear button", 1)
      showNotification("Cleared protein selection", type = "message", duration = 2)
    }, error = function(e) {
      debug_log(paste("Error clearing selection:", e$message), 1)
    })
  })

  # Clear selection
  observeEvent(input$clearSelection_Volcano, {
    tryCatch({
      selected_data_Volcano(NULL)
      selected_protein_vector_Volcano(character())

      protein_label_settings(data.frame(
        protein_id = character(), label_color = character(),
        dot_color = character(), use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      debug_log("Cleared all selected proteins and settings", 1)
      showNotification("Cleared protein selection and settings", type = "message", duration = 3)
    }, error = function(e) {
      debug_log(paste("Error clearing selection:", e$message), 1)
    })
  })

  # Label statistics
  observe({
    req(input$PlotSelect_Volcano)
    selected_plot_title <- input$PlotSelect_Volcano

    if (selected_plot_title != "none") {
      current_labels <- volcano_labels()
      if (selected_plot_title %in% names(current_labels) &&
          nrow(current_labels[[selected_plot_title]]) > 0) {
        plot_labels <- current_labels[[selected_plot_title]]
        color_counts <- table(plot_labels$LabelColor)
        color_summary <- paste(names(color_counts), "(", color_counts, ")", collapse = ", ")
        debug_log(paste("Plot", selected_plot_title, "has", nrow(plot_labels), "labels:", color_summary), 2)
      }
    }
  })

  # Label status display
  output$labelStatus_Volcano <- renderText({
    req(input$PlotSelect_Volcano)
    selected_plot_title <- input$PlotSelect_Volcano

    if (selected_plot_title == "none") return("No plot selected")

    current_labels <- volcano_labels()

    if (!selected_plot_title %in% names(current_labels) ||
        nrow(current_labels[[selected_plot_title]]) == 0) {
      return(paste("Plot:", selected_plot_title, "\nLabels: None"))
    }

    plot_labels <- current_labels[[selected_plot_title]]
    label_count <- nrow(plot_labels)
    color_groups <- split(plot_labels$ID, plot_labels$LabelColor)
    color_summary <- sapply(names(color_groups), function(color) {
      proteins <- color_groups[[color]]
      paste0(color, " (", length(proteins), "): ",
             paste(head(proteins, 3), collapse = ", "),
             if (length(proteins) > 3) "..." else "")
    })

    paste(c(paste("Plot:", selected_plot_title),
            paste("Total Labels:", label_count),
            color_summary), collapse = "\n")
  })

  # ============================================================================
  # SECTION 6: Plot Generation and Rendering
  # ============================================================================

  generateVolcanoPlots_fixed <- function(data, data_def, input, debug_log, optimal_settings = NULL, force_optimal_axes = FALSE) {
    debug_log("Starting generateVolcanoPlots_fixed with UI integration", 1)

    base_plot_params <- extract_plot_parameters_safe(input, debug_log, optimal_settings, force_optimal_axes = force_optimal_axes)
    pairing_result <- find_ratio_pvalue_pairs_smart(data_def, base_plot_params$pval_type, debug_log)
    pairing_result$source_data_signature <- compute_data_signature(data, data_def)

    # Store pairing result so interactive plot path can reuse it
    volcano_state$current_pairs <- pairing_result

    # Show notification when selected p-value type was not available
    if (isTRUE(pairing_result$pval_type_fallback)) {
      if (base_plot_params$pval_type == "Adjusted p-value") {
        showNotification("No adjusted p-values available. Using raw p-values.",
                         type = "message", duration = 5)
      } else {
        showNotification("No raw p-values available. Using adjusted p-values.",
                         type = "message", duration = 5)
      }
    }

    # Show notification for ambiguous pairings
    if (isTRUE(pairing_result$has_ambiguous)) {
      showNotification(
        paste("Multiple p-value columns found for the same comparison.",
              "First match was used. Please check metadata assignments."),
        type = "warning", duration = 10
      )
    }

    num_plots <- length(pairing_result$pairs)
    debug_log(paste("Preparing to generate", num_plots, "plots"), 1)

    if (num_plots == 0) {
      debug_log("No column pairs found - skipping plot generation", 1)
      return(list())
    }

    plot_list <- list()
    plot_axis_settings <- list()

    for (i in seq_len(num_plots)) {
      debug_log(paste("Generating plot", i, "of", num_plots), 2)
      tryCatch({
        pair <- pairing_result$pairs[[i]]
        plot_title <- generate_plot_title_from_pair(pair)
        # Automatic baselines always belong to the individual transformed pair.
        pair_optimal_settings <- calculate_optimal_ranges(
          data, data_def, debug_log, pairs = list(pair)
        )
        plot_params <- extract_plot_parameters_safe(
          input, debug_log, pair_optimal_settings, force_optimal_axes = TRUE
        )

        plot <- create_single_volcano_plot_safe(
          data = data, data_def = data_def,
          ratio_idx = pair$ratio_idx, pval_idx = pair$pval_idx,
          plot_title = plot_title, plot_params = plot_params,
          debug_log = debug_log
        )

        if (!is.null(plot)) {
          plot_axis_settings[[plot_title]] <- pair_optimal_settings
          effective <- effective_axis_settings(plot_title)
          # The newly calculated baseline is not visible through reactive state
          # until the loop completes, so use it unless this title has an override.
          override <- (volcano_state$plot_axis_overrides %||% list())[[plot_title]]
          if (valid_axis_settings(override)) effective <- override else effective <- pair_optimal_settings
          plot_params$x_limits <- effective$x_range
          plot_params$y_limits <- effective$y_range
          plot_params$x_tick <- effective$x_tick
          plot_params$y_tick <- effective$y_tick
          plot <- apply_live_styling_to_plot(plot, plot_params, debug_log, plot_title = plot_title)
          plot_list[[plot_title]] <- plot
        } else {
          debug_log(paste("Plot", i, "returned NULL"), 1)
        }
      }, error = function(e) {
        debug_log(paste("Error in plot creation for index", i, ":", e$message), 1)
      })
    }

    volcano_state$plot_axis_settings <- plot_axis_settings
    return(plot_list)
  }

  last_significance_signature <- reactiveVal(NULL)

  get_current_display_plot <- function() {
    if (is.null(volcano_state$static_plots) || length(volcano_state$static_plots) == 0) {
      return(NULL)
    }

    base_plot <- NULL
    selected_plot_title <- input$PlotSelect_Volcano
    if (is.null(selected_plot_title) || !nzchar(selected_plot_title) || selected_plot_title == "none") {
      pref <- volcano_state$preferred_plot_title
      if (is.character(pref) && length(pref) == 1L && nzchar(pref)) {
        selected_plot_title <- pref
      }
    }

    if (is.null(selected_plot_title) || !nzchar(selected_plot_title) || selected_plot_title == "none") {
      debug_log("No plot selected yet", 2)
      return(NULL)
    }

    if (selected_plot_title %in% names(volcano_state$static_plots)) {
      base_plot <- volcano_state$static_plots[[selected_plot_title]]
      debug_log(paste("Found base plot for:", selected_plot_title), 2)
    } else if (length(volcano_state$static_plots) > 0) {
      base_plot <- volcano_state$static_plots[[1]]
      selected_plot_title <- names(volcano_state$static_plots)[1]
      debug_log("Using fallback: first available plot", 2)
    }

    if (is.null(base_plot)) {
      debug_log("No base plot available", 1)
      return(NULL)
    }

    # Level-0 significance summary for the currently selected/rendered plot.
    tryCatch({
      plot_data <- base_plot$data
      if (is.data.frame(plot_data) && all(c("x", "y") %in% names(plot_data))) {
        pval_threshold <- suppressWarnings(as.numeric(input$pvalueInput_Volcano %||% 0.05))
        fold_threshold <- suppressWarnings(as.numeric(input$AbundanceInput_Volcano %||% 1))
        if (!is.finite(pval_threshold) || pval_threshold <= 0 || pval_threshold >= 1) pval_threshold <- 0.05
        if (!is.finite(fold_threshold) || fold_threshold < 0) fold_threshold <- 1

        sig_mask <- plot_data$y > -log10(pval_threshold)
        n_up <- sum(plot_data$x > fold_threshold & sig_mask, na.rm = TRUE)
        n_down <- sum(plot_data$x < -fold_threshold & sig_mask, na.rm = TRUE)

        signature <- paste(selected_plot_title, n_up, n_down, pval_threshold, fold_threshold, sep = "|")
        if (!identical(last_significance_signature(), signature)) {
          debug_log(paste0("Current Volcano [", selected_plot_title,
                           "]: significantly more abundant = ", n_up,
                           ", significantly less abundant = ", n_down,
                           " (p<", pval_threshold, ", |log2FC|>", fold_threshold, ")"), 0)
          last_significance_signature(signature)
        }
      }
    }, error = function(e) {
      debug_log(paste("Could not compute per-plot significance summary:", e$message), 1)
    })

    current_labels <- volcano_labels()
    if (!is.null(current_labels) && selected_plot_title %in% names(current_labels)) {
      plot_labels <- current_labels[[selected_plot_title]]
      if (is.data.frame(plot_labels) && nrow(plot_labels) > 0) {
        debug_log(paste("Applying", nrow(plot_labels), "labels to plot"), 1)
        labeled_dot_size <- input$dotSizeLabeled_Volcano %||% 2
        final_plot <- apply_all_labels_to_plot_enhanced(
          base_plot, plot_labels, input, debug_log, labeled_dot_size
        )
        debug_log("Labels applied successfully", 2)
        return(final_plot)
      }
    }

    debug_log("No labels to apply, returning base plot", 3)
    base_plot
  }

  clear_plot_creation_guard_after_flush <- function() {
    session$onFlushed(function() {
      session$onFlushed(function() {
        volcano_state$plot_creation_in_progress <- FALSE
        debug_log("Plot creation guard cleared after UI updates settled", 2)
      }, once = TRUE)
    }, once = TRUE)
    invisible(NULL)
  }

  # Main plot generation observer
  observeEvent(input$update_Volcano, {
    req(data_in(), data_def_in())

    volcano_state$plot_creation_in_progress <- TRUE
    on.exit(clear_plot_creation_guard_after_flush(), add = TRUE)

    volcano_state$manual_axis_override <- FALSE
    volcano_state$plot_axis_overrides <- list()
    volcano_state$auto_range_set <- FALSE
    volcano_state$auto_axis_update_in_progress <- FALSE
    volcano_state$plot_axis_settings <- NULL

    debug_log("Starting plot generation", 1)
    reset_preplot_skip_log_flags()

    data <- data_in()
    data_def <- data_def_in()

    optimal_settings <- calculate_optimal_ranges(data, data_def, debug_log)

    if (!is.data.frame(data) || nrow(data) == 0) {
      showNotification("Invalid data format", type = "error", duration = 5)
      return()
    }

    if (!is.data.frame(data_def) || nrow(data_def) == 0) {
      showNotification("Invalid metadata format", type = "error", duration = 5)
      return()
    }

    has_abundance_ratio <- "Content" %in% names(data_def) &&
      any(trimws(data_def$Content) == "Abundance Ratio", na.rm = TRUE)

    if (!has_abundance_ratio) {
      debug_log("Metadata contains no Abundance Ratio columns - aborting plot generation", 1)
      showNotification(
        "Metadata does not define any Abundance Ratio columns. Please configure metadata before creating plots.",
        type = "warning", duration = 8
      )
      return()
    }

    tryCatch({
      volcano_state$static_plots <- generateVolcanoPlots_fixed(
        data = data, data_def = data_def,
        input = input, debug_log = debug_log,
        optimal_settings = optimal_settings,
        force_optimal_axes = TRUE
      )

      if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
        debug_log(paste0("Volcano plot generation completed | plots=",
                         length(volcano_state$static_plots),
                         " | FC threshold=", input$AbundanceInput_Volcano %||% 1,
                         " | p-value threshold=", input$pvalueInput_Volcano %||% 0.05,
                         " | p-value type=", input$pValueSel_Volcano %||% "p-value"), level = 0)

        volcano_state$plot_titles <- names(volcano_state$static_plots)

        selected_axis_settings <- optimal_settings
        if (length(volcano_state$plot_titles) > 0) {
          plot_choices <- volcano_state$plot_titles
          names(plot_choices) <- volcano_state$plot_titles
          selected_title <- resolve_plot_selection(volcano_state$plot_titles)
          if (is.list(volcano_state$plot_axis_settings) &&
              selected_title %in% names(volcano_state$plot_axis_settings)) {
            selected_axis_settings <- effective_axis_settings(selected_title)
          }
          suppress_plot_selection_update(selected_title)
          updateSelectInput(session, "PlotSelect_Volcano",
                            choices = plot_choices, selected = selected_title)
        }

        # Initial plot creation already calculated the optimal axis ranges and tick
        # intervals used to render each plot. Mirror the selected/rendered plot's
        # values into the visible axis controls without changing slider bounds.
        update_axis_ui_controls(selected_axis_settings, reason = "create plot", update_slider_bounds = FALSE)

        volcano_state$plot_creation_cache <- list(data_mod = data, data_def = data_def)
        volcano_state$source_data_signature <- compute_data_signature(data, data_def)
        volcano_state$plot_generation_counter <- (volcano_state$plot_generation_counter %||% 0L) + 1L
        store_original_plots()
        volcano_state$plot_cache_ref_by_title <- setNames(
          rep(volcano_state$source_data_signature, length(volcano_state$plot_titles)),
          volcano_state$plot_titles
        )
        volcano_state$plot_ui_cache <- list(
          PlotSelect_Volcano = input$PlotSelect_Volcano,
          plotTitle_Volcano = input$plotTitle_Volcano,
          hideTitle_Volcano = isTRUE(input$hideTitle_Volcano),
          ThemeSelect_Volcano = input$ThemeSelect_Volcano,
          pValueSel_Volcano = input$pValueSel_Volcano,
          Identifier_Volcano = input$Identifier_Volcano,
          AbundanceInput_Volcano = input$AbundanceInput_Volcano,
          pvalueInput_Volcano = input$pvalueInput_Volcano,
          xLimInput_Volcano = selected_axis_settings$x_range,
          yLimInput_Volcano = selected_axis_settings$y_range,
          xTick_Volcano = selected_axis_settings$x_tick,
          yTick_Volcano = selected_axis_settings$y_tick
        )
        showNotification("Volcano plots generated successfully!", type = "message", duration = 3)
      } else {
        debug_log("Plot generation returned NULL or empty list", 1)
        showNotification("No plots could be generated", type = "error", duration = 5)
      }
    }, error = function(e) {
      debug_log(paste("Plot generation error:", e$message), 1)
      showNotification(paste("Plot generation failed:", e$message), type = "error", duration = 5)
    })
  })

  # Static plot rendering
  output$volcanoPlot <- renderPlot({
    plot_update_trigger()
    if (isTRUE(volcano_state$restore_in_progress)) {
      debug_log("Skipping volcano render while restore is in progress", 2)
      return(NULL)
    }
    get_current_display_plot()
  })

  # Interactive plot rendering
  output$volcanoPlotly <- renderPlotly({
    req(volcano_state$static_plots, input$PlotSelect_Volcano,
        input$cechbox_interactive_Volcano)

    if (!input$cechbox_interactive_Volcano) return(NULL)

    selected_plot <- volcano_state$static_plots[[input$PlotSelect_Volcano]]
    if (is.null(selected_plot)) return(NULL)

    selected_plot_title <- input$PlotSelect_Volcano
    plot_names <- names(volcano_state$static_plots)
    plot_index <- if (selected_plot_title %in% plot_names) {
      which(plot_names == selected_plot_title)[1]
    } else {
      1
    }

    # IMPORTANT: Interactive mode must reuse the exact data snapshot used when
    # static plots were generated. This prevents dataset-switch drift where the
    # user loads a new dataset/metadata but has not pressed "Create Plot" yet.
    cached_data <- volcano_state$plot_creation_cache$data_mod
    cached_data_def <- volcano_state$plot_creation_cache$data_def
    source_data <- if (is.data.frame(cached_data) && nrow(cached_data) > 0) cached_data else data_in()
    source_data_def <- if (is.data.frame(cached_data_def) && nrow(cached_data_def) > 0) cached_data_def else data_def_in()

    plot_data <- prepare_interactive_plot_data(
      data = source_data, data_def = source_data_def,
      plot_index = plot_index, input = input, debug_log = debug_log,
      stored_pairs = volcano_state$current_pairs
    )

    if (is.null(plot_data)) return(NULL)

    volcano_state$current_plotly_data <- plot_data
    debug_log(paste("Stored plotly data with", nrow(plot_data), "points for selection handlers"), 2)

    p <- create_plotly_volcano(plot_data, input)
    p <- configure_plotly_layout(p, input)
    p <- add_plotly_interactivity(p)

    return(p)
  })

  # Plot count output
  output$plot_count <- reactive({
    if (!is.null(volcano_state$static_plots)) length(volcano_state$static_plots) else 0
  })
  outputOptions(output, "plot_count", suspendWhenHidden = FALSE)

  # ============================================================================
  # SECTION 7: Gene Search and Display
  # ============================================================================

  observeEvent(input$searchGene_Volcano, {
    req(data_in(), data_def_in(), input$Identifier_Volcano)

    search_text <- trimws(input$searchGene_Volcano)
    if (search_text == "") {
      volcano_state$selected_genes <- character()
      return()
    }

    genes <- unlist(strsplit(search_text, "\n"))
    genes <- trimws(genes[genes != ""])
    volcano_state$selected_genes <- genes
    debug_log(paste("Updated selected genes:", length(genes), "genes"), 2)
  })

  output$search_identifier_label_Volcano <- renderText({

    selected_identifier <-
      input$Identifier_Volcano

    current_data <-
      tryCatch(
        rv$data_mod,
        error = function(e) NULL
      )

    identifier_available <-
      is.character(selected_identifier) &&
      length(selected_identifier) == 1L &&
      !is.na(selected_identifier) &&
      nzchar(selected_identifier) &&
      is.data.frame(current_data) &&
      selected_identifier %in% names(current_data)

    if (!identifier_available) {
      return(
        "Select identifier column"
      )
    }

    paste0(
      "Search for ",
      selected_identifier,
      ":"
    )
  })

  output$geneSymbolList_Volcano <- renderPrint({
    tryCatch({
      req(rv$data_mod, rv$data_def)

      data <- rv$data_mod
      selected_identifier <- input$Identifier_Volcano
      gene_symbols_text <- ""

      if (!is.null(input$searchGene_Volcano) && input$searchGene_Volcano != "" &&
          !is.null(selected_identifier) && selected_identifier != "") {

        quiet_log <- function(...) invisible(NULL)
        filter_data <- get_filter_string_Volcano(input$searchGene_Volcano, selected_identifier, quiet_log)

        if (nrow(filter_data) > 0) {
          filter_data <- as.vector(filter_data[,1])

          if (length(filter_data) > 0) {
            all_identifiers <- data[[selected_identifier]]
            all_identifiers <- all_identifiers[!is.na(all_identifiers)]
            pattern <- paste(filter_data, collapse = "|")
            identifier <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)
            identifier <- unique(identifier)
            gene_symbols_text <- paste(identifier, collapse = "\n")
          }
        }
      }

      cat(gene_symbols_text)
    }, error = function(e) {
      cat("")
    })
  })

  # ============================================================================
  # SECTION 8: Axis Override and Settings
  # ============================================================================

  record_selected_axis_override <- function(description) {
    if (isTRUE(volcano_state$auto_axis_update_in_progress)) return(invisible(FALSE))
    title <- isolate(input$PlotSelect_Volcano)
    settings <- list(
      x_range = isolate(input$xLimInput_Volcano),
      y_range = isolate(input$yLimInput_Volcano),
      x_tick = isolate(input$xTick_Volcano),
      y_tick = isolate(input$yTick_Volcano)
    )
    if (is.null(title) || identical(title, "none") || !valid_axis_settings(settings)) {
      debug_log(paste("Ignored invalid or unscoped manual axis change:", description), 1)
      return(invisible(FALSE))
    }
    overrides <- volcano_state$plot_axis_overrides %||% list()
    overrides[[title]] <- settings
    volcano_state$plot_axis_overrides <- overrides
    volcano_state$manual_axis_override <- TRUE
    volcano_state$auto_range_set <- TRUE
    debug_log(paste("Stored manual axis override for", title, "-", description), 2)
    invisible(TRUE)
  }

  observeEvent(input$xLimInput_Volcano, {
    if (consume_axis_input_suppression("xLimInput_Volcano")) return()
    record_selected_axis_override("X-axis range")
  }, ignoreInit = TRUE)

  observeEvent(input$yLimInput_Volcano, {
    if (consume_axis_input_suppression("yLimInput_Volcano")) return()
    record_selected_axis_override("Y-axis range")
  }, ignoreInit = TRUE)

  observeEvent(input$xTick_Volcano, {
    if (consume_axis_input_suppression("xTick_Volcano")) return()
    record_selected_axis_override("X-axis tick spacing")
  }, ignoreInit = TRUE)

  observeEvent(input$yTick_Volcano, {
    if (consume_axis_input_suppression("yTick_Volcano")) return()
    record_selected_axis_override("Y-axis tick spacing")
  }, ignoreInit = TRUE)

  # Apply settings (labeling)
  observeEvent(input$applySettings_Volcano, {
    tryCatch({
      req(rv$data_mod, rv$data_def)
      debug_log("=== APPLY SETTINGS & LABEL START ===", 1)

      selected_proteins <- selected_protein_vector_Volcano()
      if (is.null(selected_proteins) || length(selected_proteins) == 0) {
        showNotification("No proteins to configure", type = "warning")
        return()
      }

      debug_log(paste("Processing", length(selected_proteins), "proteins"), 1)

      selected_plot_title <- input$PlotSelect_Volcano
      if (is.null(selected_plot_title) || selected_plot_title == "none") {
        if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
          selected_plot_title <- names(volcano_state$static_plots)[1]
        } else {
          showNotification("No plots available for labeling", type = "warning")
          return()
        }
      }

      current_all_labels <- volcano_labels()
      current_plot_labels <- if (selected_plot_title %in% names(current_all_labels)) {
        current_all_labels[[selected_plot_title]]
      } else {
        data.frame()
      }

      if (nrow(current_plot_labels) > 0) {
        current_plot_labels <- current_plot_labels[!current_plot_labels$ID %in% selected_proteins, ]
      }

      new_settings <- data.frame(
        protein_id = selected_proteins,
        label_color = character(length(selected_proteins)),
        dot_color = character(length(selected_proteins)),
        use_custom_dot_color = logical(length(selected_proteins)),
        stringsAsFactors = FALSE
      )

      for (i in seq_along(selected_proteins)) {
        new_settings$label_color[i] <- input[[paste0("labelColor_Volcano_", i)]] %||% "#000000"
        new_settings$dot_color[i] <- input[[paste0("dotColor_Volcano_", i)]] %||% "#2E86AB"
        new_settings$use_custom_dot_color[i] <- input[[paste0("useDotColor_Volcano_", i)]] %||% FALSE
      }

      protein_label_settings(new_settings)

      new_label_data <- create_volcano_label_data_enhanced(
        selected_proteins, selected_plot_title, rv, input, volcano_state, debug_log, new_settings
      )

      if (is.null(new_label_data) || nrow(new_label_data) == 0) {
        showNotification("Could not create label data for selected proteins", type = "error")
        return()
      }

      all_plot_labels <- if (nrow(current_plot_labels) > 0) {
        rbind(current_plot_labels, new_label_data)
      } else {
        new_label_data
      }

      current_all_labels[[selected_plot_title]] <- all_plot_labels
      volcano_labels(current_all_labels)

      plot_update_trigger(plot_update_trigger() + 1)
      debug_log(paste("Successfully applied settings and labeled", length(selected_proteins), "proteins"), 1)
      showNotification(paste("Applied settings and labeled", length(selected_proteins), "proteins"),
                       type = "message", duration = 3)
    }, error = function(e) {
      debug_log(paste("Error in applySettings_Volcano:", e$message), 1)
      showNotification(paste("Error applying settings:", e$message), type = "error")
    })
  })

  # Reset color settings
  observeEvent(input$resetColors_Volcano, {
    tryCatch({
      selected_proteins <- selected_protein_vector_Volcano()
      if (is.null(selected_proteins) || length(selected_proteins) == 0) return()

      protein_label_settings(data.frame(
        protein_id = character(), label_color = character(),
        dot_color = character(), use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      debug_log("Reset protein color settings", 1)
      showNotification("Reset protein color settings to defaults", type = "message", duration = 3)
    }, error = function(e) {
      debug_log(paste("Error resetting settings:", e$message), 1)
    })
  })

  # ============================================================================
  # SECTION 9: Labeling UI and Protein Removal
  # ============================================================================

  output$enhanced_selectedProteins_Volcano <- renderUI({
    tryCatch({
      selected_proteins <- selected_protein_vector_Volcano()

      if (is.null(selected_proteins) || length(selected_proteins) == 0) {
        return(div(
          style = "padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f8f9fa; min-height: 120px;",
          p("No proteins selected", style = "color: #666; margin: 0; text-align: center; padding-top: 30px;")
        ))
      }

      debug_log(paste("Generating UI for", length(selected_proteins), "proteins"), 2)

      current_settings <- protein_label_settings()
      default_dot_colors <- get_default_dot_colors_for_proteins(selected_proteins, rv, input, debug_log)

      protein_rows <- lapply(seq_along(selected_proteins), function(i) {
        protein <- selected_proteins[i]
        existing_row <- current_settings[current_settings$protein_id == protein, ]

        if (nrow(existing_row) > 0) {
          label_color <- existing_row$label_color[1]
          dot_color <- existing_row$dot_color[1]
          use_custom <- existing_row$use_custom_dot_color[1]
        } else {
          label_color <- "#000000"
          dot_color <- default_dot_colors[i]
          use_custom <- FALSE
        }

        fluidRow(
          style = "border-bottom: 1px solid #eee; padding: 12px 5px; margin: 0px;",
          column(width = 3, div(style = "padding-top: 10px;", strong(substr(protein, 1, 16)))),
          column(width = 3, div(style = "padding: 2px;",
            colourInput(ns(paste0("labelColor_Volcano_", i)), "Label:", value = label_color))),
          column(width = 3, div(style = "padding: 2px;",
            colourInput(ns(paste0("dotColor_Volcano_", i)), "Dot:", value = dot_color))),
          column(width = 2, div(style = "padding-top: 18px;",
            checkboxInput(ns(paste0("useDotColor_Volcano_", i)), "", value = use_custom, width = "100%"))),
          column(width = 1, div(style = "padding-top: 15px; text-align: center;",
            tags$button(
              class = "btn btn-danger btn-xs",
              style = "padding: 1px 6px; font-size: 11px; line-height: 1.4;",
              onclick = sprintf(
                "Shiny.setInputValue(\"%s\", \"%s\", {priority: \"event\"});",
                ns("remove_protein_click_Volcano"),
                gsub('"', '\\\\"', protein, fixed = TRUE)
              ),
              icon("times")
            )))
        )
      })

      tagList(
        fluidRow(
          style = "background-color: #f8f9fa; padding: 12px; margin: 0px; border: 1px solid #ddd; border-bottom: none; font-weight: bold;",
          column(width = 3, "Protein"),
          column(width = 3, "Label Color"),
          column(width = 3, "Dot Color"),
          column(width = 2, "Custom Dot Color"),
          column(width = 1, "Remove")
        ),
        div(
          style = "border: 1px solid #ddd; border-top: none; padding: 8px; max-height: 400px; min-height: 200px; overflow-y: auto;",
          protein_rows
        )
      )
    }, error = function(e) {
      debug_log(paste("Error generating enhanced UI:", e$message), 1)
      return(div("Error generating protein controls"))
    })
  })

  # Remove a single protein via its per-row button in the Selected Proteins list
  observeEvent(input$remove_protein_click_Volcano, {
    tryCatch({
      protein_to_remove <- input$remove_protein_click_Volcano
      if (is.null(protein_to_remove) || !nzchar(protein_to_remove)) return()

      debug_log(paste("Removing protein:", protein_to_remove), 1)

      current_data <- selected_data_Volcano()
      if (is.null(current_data) || !is.data.frame(current_data) || nrow(current_data) == 0) return()

      selected_identifier <- input$Identifier_Volcano
      Identifier_indices <- which(grepl(selected_identifier, rv$data_def$Options, fixed = TRUE))

      if (length(Identifier_indices) > 0) {
        filtered_data <- current_data[current_data[[Identifier_indices[1]]] != protein_to_remove, , drop = FALSE]
        selected_data_Volcano(filtered_data)

        if (nrow(filtered_data) > 0) {
          selected_protein_vector_Volcano(as.vector(filtered_data[, Identifier_indices[1]]))
        } else {
          selected_protein_vector_Volcano(character())
        }

        current_settings <- protein_label_settings()
        if (nrow(current_settings) > 0) {
          protein_label_settings(current_settings[current_settings$protein_id != protein_to_remove, ])
        }

        debug_log(paste("Successfully removed protein:", protein_to_remove), 1)
        showNotification(paste("Removed", protein_to_remove, "from selection"),
                         type = "message", duration = 2)
      }
    }, error = function(e) {
      debug_log(paste("Error in remove_protein_click_Volcano:", e$message), 1)
    })
  })

  # ============================================================================
  # SECTION 10: Download, Live Updates, Grid, and Reset
  # ============================================================================

  output$downloadPlotButton_Volcano <- downloadHandler(
    filename = function() {
      sanitized <- gsub("[^A-Za-z0-9]", "_", input$PlotSelect_Volcano)
      paste0("Volcano_", sanitized, ".", input$downloadFormat_Volcano)
    },
    content = function(file) {
      plot_to_save <- get_current_display_plot()
      if (!is.null(plot_to_save)) {
        save_volcano_plot(file, plot_to_save,
                          width = input$plotWidthInch_Volcano,
                          height = input$plotHeightInch_Volcano,
                          dpi = input$resolution_DPI,
                          debug_log = debug_log)
      }
    }
  )

  live_styling_input_snapshot <- reactiveVal(NULL)
  live_styling_input_ids <- c(
    "dotColorInput_Volcano", "dotColorInputUp_Volcano",
    "dotColorInputDown_Volcano", "dotSizeInput_Volcano",
    "dotSizeInputUp_Volcano", "dotSizeInputDown_Volcano",
    "ThemeSelect_Volcano", "plotTitle_Volcano",
    "hideTitle_Volcano", "plotTitleSize_Volcano", "AxisTitleSize_Volcano",
    "tickSize_Volcano", "xLimInput_Volcano",
    "yLimInput_Volcano", "xTick_Volcano", "yTick_Volcano"
  )

  get_live_styling_inputs <- function() {
    stats::setNames(lapply(live_styling_input_ids, function(id) input[[id]]),
                    live_styling_input_ids)
  }

  # Live styling observer
  observeEvent({
    get_live_styling_inputs()
  }, {
    req(volcano_state$static_plots)

    current_inputs <- get_live_styling_inputs()
    previous_inputs <- live_styling_input_snapshot()
    changed_inputs <- names(current_inputs)
    if (is.list(previous_inputs)) {
      changed_inputs <- names(current_inputs)[vapply(names(current_inputs), function(id) {
        !isTRUE(identical(current_inputs[[id]], previous_inputs[[id]]))
      }, logical(1))]
    }
    live_styling_input_snapshot(current_inputs)

    if (isTRUE(volcano_state$auto_axis_update_in_progress)) {
      debug_log("Live styling update skipped during auto-axis update", 2)
      return()
    }

    suppressed_axis_inputs <- intersect(changed_inputs, axis_input_ids)
    suppressed_axis_inputs <- suppressed_axis_inputs[vapply(
      suppressed_axis_inputs, axis_input_suppression_pending, logical(1)
    )]
    if (length(suppressed_axis_inputs) > 0) {
      debug_log(paste("Live styling update skipped for programmatic axis input update:",
                      paste(suppressed_axis_inputs, collapse = ", ")), 2)
      return()
    }

    if (isTRUE(volcano_state$plot_creation_in_progress)) {
      debug_log("Live styling update skipped during plot creation", 2)
      return()
    }

    if (datawizard_restore_phase_active(rv)) {
      debug_log("Restore phase active; skipping Volcano live styling update", 2)
      return()
    }

    debug_log("UI styling parameters changed - triggering live update", 2)
    trigger_live_update()
  }, ignoreInit = TRUE)

  # Data parameter observer
  observeEvent({
    list(input$pValueSel_Volcano, input$pvalueInput_Volcano, input$AbundanceInput_Volcano)
  }, {
    req(volcano_state$static_plots)
    if (datawizard_restore_phase_active(rv)) {
      debug_log("Restore phase active; skipping Volcano live data update", 2)
      return()
    }
    debug_log("UI data parameters changed - triggering data update", 1)
    invalidateLater(1000)
    trigger_data_update()
  }, ignoreInit = TRUE)

  # Grid integration
  observeEvent(input$add_to_grid, {
    debug_log("Volcano: add_to_grid clicked", 2)

    if (is.null(volcano_state$static_plots)) {
      showNotification("No plot list available.", type = "error")
      return()
    }
    if (is.null(input$PlotSelect_Volcano) || !nzchar(input$PlotSelect_Volcano)) {
      showNotification("Select a plot first.", type = "error")
      return()
    }

    p <- tryCatch(get_current_display_plot(), error = function(e) {
      debug_log(paste("Error accessing plot for grid:", e$message), 1)
      showNotification("Could not access the selected volcano plot.", type = "error")
      NULL
    })

    add_volcano_to_grid(p, input$grid_label, ns, rv, modEnv, debug_log)
  })

  # Update trigger functions
  trigger_live_update <- function() {
    debug_log("Triggering live update", 2)
    tryCatch({
      current_params <- extract_plot_parameters_safe(input, debug_log)

      if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
        for (plot_name in names(volcano_state$static_plots)) {
          original_plot <- volcano_state$static_plots[[plot_name]]
          plot_params <- current_params
          axes <- effective_axis_settings(plot_name)
          plot_params$x_limits <- axes$x_range
          plot_params$y_limits <- axes$y_range
          plot_params$x_tick <- axes$x_tick
          plot_params$y_tick <- axes$y_tick
          updated_plot <- apply_live_styling_to_plot(
            original_plot, plot_params, debug_log, plot_title = plot_name
          )
          volcano_state$static_plots[[plot_name]] <- updated_plot
        }
        debug_log("Live styling updates applied to all plots", 2)
        plot_update_trigger(plot_update_trigger() + 1)
      }
    }, error = function(e) {
      debug_log(paste("Error in live update:", e$message), 1)
    })
  }

  trigger_data_update <- function() {
    debug_log("Triggering data update", 1)
    tryCatch({
      current_sig <- compute_data_signature(data_in(), data_def_in())
      source_sig  <- volcano_state$source_data_signature %||% NA_character_
      if (is.character(source_sig) && nzchar(source_sig) &&
          !identical(source_sig, current_sig)) {
        debug_log("Skipping volcano data update: active dataset differs from plot creation dataset", 1)
        return(invisible(NULL))
      }

      volcano_state$static_plots <- generateVolcanoPlots_fixed(
        data = data_in(), data_def = data_def_in(),
        input = input, debug_log = debug_log,
        force_optimal_axes = TRUE
      )

      if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
        valid_titles <- names(volcano_state$static_plots)
        volcano_state$plot_axis_overrides <-
          (volcano_state$plot_axis_overrides %||% list())[valid_titles]
        selected <- isolate(input$PlotSelect_Volcano)
        if (selected %in% valid_titles) {
          update_axis_ui_controls(effective_axis_settings(selected), reason = "data update")
        }
        volcano_state$source_data_signature <- compute_data_signature(data_in(), data_def_in())
        volcano_state$plot_generation_counter <- (volcano_state$plot_generation_counter %||% 0L) + 1L
        store_original_plots()
        debug_log("Data update completed successfully", 1)
        plot_update_trigger(plot_update_trigger() + 1)
        showNotification("Plot data updated", type = "message", duration = 1)
      }
    }, error = function(e) {
      debug_log(paste("Error in data update:", e$message), 1)
    })
  }

  # Plot title debounced update
  observeEvent(input$plotTitle_Volcano, {
    req(volcano_state$static_plots)
    invalidateLater(500)
    isolate({
      debug_log("Plot title changed (debounced) - triggering update", 2)
      trigger_live_update()
    })
  }, ignoreInit = TRUE)

  # Toggle protein controls panel
  observeEvent(input$toggle_protein_controls, {
    tryCatch({
      debug_log("Toggling protein controls visibility", 2)
      shinyjs::toggle("protein_controls_content")
      current_display <- input$toggle_protein_controls %% 2

      if (current_display == 1) {
        shinyjs::removeClass("protein_controls_icon", "fa-chevron-right")
        shinyjs::addClass("protein_controls_icon", "fa-chevron-down")
      } else {
        shinyjs::removeClass("protein_controls_icon", "fa-chevron-down")
        shinyjs::addClass("protein_controls_icon", "fa-chevron-right")
      }
    }, error = function(e) {
      debug_log(paste("Error toggling protein controls:", e$message), 1)
    })
  })

  # Reset the right-side plot control panels to their UI defaults
  observeEvent(input$resetButton_Volcano, {
    debug_log("Resetting volcano right-side plot controls to UI defaults", 1)

    # --- Plot Settings ---
    updateTextInput(session, "plotTitle_Volcano", value = "")
    updateCheckboxInput(session, "hideTitle_Volcano", value = FALSE)
    updateNumericInput(session, "plotTitleSize_Volcano", value = 20)
    updateNumericInput(session, "AxisTitleSize_Volcano", value = 20)
    updateNumericInput(session, "tickSize_Volcano", value = 18)
    updateSelectInput(session, "ThemeSelect_Volcano", selected = "Classic")
    updateSelectInput(session, "pValueSel_Volcano", selected = "Adjusted p-value")

    update_identifier_choices(data_def_in())

    # --- Appearance ---
    updateSliderInput(session, "dotSizeInput_Volcano", value = 1)
    updateSliderInput(session, "dotSizeInputUp_Volcano", value = 1.5)
    updateSliderInput(session, "dotSizeInputDown_Volcano", value = 1.5)
    colourpicker::updateColourInput(session, "dotColorInput_Volcano", value = "#E0E0E0")
    colourpicker::updateColourInput(session, "dotColorInputUp_Volcano", value = "#EFC000FF")
    colourpicker::updateColourInput(session, "dotColorInputDown_Volcano", value = "#440154FF")

    # --- Thresholds ---
    updateNumericInput(session, "pvalueInput_Volcano", value = 0.05)
    updateNumericInput(session, "AbundanceInput_Volcano", value = 1)

    # --- Axes ---
    updateSliderInput(session, "xLimInput_Volcano", min = -10, max = 10, value = c(-8, 8))
    updateNumericInput(session, "xTick_Volcano", value = 2)
    updateSliderInput(session, "yLimInput_Volcano", min = 0, max = 50, value = c(0, 18))
    updateNumericInput(session, "yTick_Volcano", value = 2)

    # --- Reset internal state flags ---
    volcano_state$auto_range_set <- TRUE
    volcano_state$manual_axis_override <- FALSE
    volcano_state$plot_axis_overrides <- list()
    volcano_state$auto_axis_update_in_progress <- FALSE

    showNotification("Plot controls reset to defaults.", type = "message", duration = 3)
    debug_log("Volcano right-side plot controls reset to UI defaults", 1)
  })

  list(
    calculate_optimal_ranges_for_selected_plot = calculate_optimal_ranges_for_selected_plot,
    store_original_plots = store_original_plots,
    generateVolcanoPlots_fixed = generateVolcanoPlots_fixed,
    effective_axis_settings = effective_axis_settings,
    update_axis_ui_controls = update_axis_ui_controls
  )
}
