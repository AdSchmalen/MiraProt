# ==============================================================================
# dotplot_server_config.R - Dotplot configuration observers
#
# Purpose: Hosts axis, identifier, transformation, and range management observers
# for the Dotplot server module.
#
# Structure:
#   - Range labels and auto/manual range management
#   - Identifier updates and selected identifier display
#   - Axis column, label, and transformation synchronization
#   - Plot preset updates for volcano-style defaults
#
# Dependencies: shiny, colourpicker, dotplot_utils.R helpers
# Called by: modDotPlotServer()
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_config_observers
# Purpose: Initializes all Dotplot configuration observers and outputs.
# Structure:
#   - Section 1: Render range labels and maintain auto/manual axis ranges.
#   - Section 2: Handle identifier updates and axis range slider interactions.
#   - Section 3: Synchronize axis columns, labels, transforms, and presets.
# Parameters:
#   - input: [reactivevalues] - Module input values.
#   - output: [shinyoutput] - Module output bindings.
#   - session: [shinysession] - Active module session.
#   - ns: [function] - Namespace function.
#   - dotplot_debug_log: [function] - Dotplot debug logger from parent module.
#   - rv: [reactivevalues] - Shared application reactiveValues.
#   - dotplot_state: [reactivevalues] - Dotplot state container.
#   - data_in: [reactive] - Input data reactive.
#   - data_def_in: [reactive] - Metadata reactive.
#   - dot_plot_parameters: [reactiveVal] - Plot parameter cache.
#   - user_range_settings: [reactiveVal] - User range override flags.
#   - plot_update_trigger: [reactiveVal] - Trigger for full plot rebuilds.
#   - range_render_trigger: [reactiveVal] - Render-only trigger for axis range changes.
#   - selected_points_interactive_dot: [reactiveVal] - Interactive point selection store.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# dotplot_safe_basemean_quantiles
# Purpose: Safely compute Q1/Q3 for Basemean-like values and return stable
#   defaults if data are missing or invalid.
# Parameters:
#   - data: [data.frame] - Input table.
#   - column_name: [character] - Name of the basemean column.
# Returns: list(q1 = numeric(1), q3 = numeric(1), source = character(1)).
# ------------------------------------------------------------------------------
dotplot_safe_basemean_quantiles <- function(data, column_name) {
  fallback <- list(q1 = 1, q3 = 3, source = "fallback")

  if (is.null(data) || !is.data.frame(data) || is.null(column_name) || !nzchar(column_name)) {
    return(fallback)
  }

  if (!column_name %in% names(data)) {
    return(fallback)
  }

  values <- suppressWarnings(as.numeric(data[[column_name]]))
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(fallback)
  }

  qs <- suppressWarnings(stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7))

  if (length(qs) != 2 || any(!is.finite(qs))) {
    return(fallback)
  }

  q1 <- qs[1]
  q3 <- qs[2]

  if (!is.finite(q1) || !is.finite(q3)) {
    return(fallback)
  }

  if (isTRUE(all.equal(q1, q3))) {
    eps <- max(abs(q1) * 0.05, 1e-6)
    q1 <- q1 - eps
    q3 <- q3 + eps
  }

  list(q1 = q1, q3 = q3, source = "data")
}

# ------------------------------------------------------------------------------
# dotplot_transform_threshold_value
# Purpose: Transform threshold values into plotted scale so threshold lines align
#   with transformed axes.
# Parameters:
#   - value: [numeric(1)] - Raw threshold value.
#   - transform_type: [character] - Axis transformation key.
# Returns: numeric(1) transformed value, or NA_real_ if invalid.
# ------------------------------------------------------------------------------
dotplot_transform_threshold_value <- function(value, transform_type) {
  if (is.null(value) || length(value) == 0) return(NA_real_)

  numeric_value <- suppressWarnings(as.numeric(value)[1])
  if (!is.finite(numeric_value)) return(NA_real_)

  transform_key <- suppressWarnings(as.character(transform_type %||% "raw")[1])
  transform_key <- tolower(trimws(transform_key %||% "raw"))
  if (is.na(transform_key) || !transform_key %in% c("raw", "log2", "log10", "neg_log10")) {
    transform_key <- "raw"
  }

  transformed_value <- tryCatch({
    switch(transform_key,
           "raw" = numeric_value,
           "log2" = {
             safe_value <- if (numeric_value <= 0) abs(numeric_value) + 1e-10 else numeric_value
             log2(safe_value)
           },
           "log10" = {
             safe_value <- if (numeric_value <= 0) abs(numeric_value) + 1e-10 else numeric_value
             log10(safe_value)
           },
           "neg_log10" = {
             safe_value <- min(max(numeric_value, 1e-300), 1)
             -log10(safe_value)
           })
  }, error = function(e) {
    NA_real_
  })

  transformed_value <- suppressWarnings(as.numeric(transformed_value)[1])
  if (!is.finite(transformed_value)) return(NA_real_)

  transformed_value
}

dotplot_init_config_observers <- function(input, output, session, ns, dotplot_debug_log, rv, dotplot_state, data_in, data_def_in,
                                          dot_plot_parameters, user_range_settings, plot_update_trigger, range_render_trigger,
                                          selected_points_interactive_dot, region_configs = NULL) {
  is_active_dataset_mismatch <- function() {
    if (!isTRUE(dotplot_state$plot_ready)) return(FALSE)
    source_sig <- dotplot_state$source_data_signature %||% NA_character_
    if (!is.character(source_sig) || !nzchar(source_sig)) return(FALSE)
    if (!is.data.frame(rv$data_mod) || !is.data.frame(rv$data_def)) return(FALSE)
    current_sig <- paste0(nrow(rv$data_mod), "x", ncol(rv$data_mod), "::",
                          nrow(rv$data_def), "x", ncol(rv$data_def), "::",
                          paste(colnames(rv$data_mod), collapse = "|"), "::",
                          paste(colnames(rv$data_def), collapse = "|"))
    !identical(source_sig, current_sig)
  }

  restored_cache_authoritative <- function() {
    isTRUE(isolate(dotplot_state$plot_from_restore_cache)) &&
      is.list(isolate(dotplot_state$restore_plot_data_cache))
  }

  get_live_data_mod <- function() {
    if (is.data.frame(rv$data_mod)) return(rv$data_mod)
    tryCatch(data_in(), error = function(e) NULL)
  }

  get_live_data_def <- function() {
    if (is.data.frame(rv$data_def)) return(rv$data_def)
    tryCatch(data_def_in(), error = function(e) NULL)
  }

  mark_programmatic_range_update <- function(ids) {
    ids <- intersect(ids, c("x_axis_range", "y_axis_range"))
    if (length(ids) == 0L) return(invisible(FALSE))
    pending <- isolate(dotplot_state$programmatic_range_update_pending %||% character(0))
    dotplot_state$programmatic_range_update_pending <- unique(c(pending, ids))
    invisible(TRUE)
  }

  update_cached_range_inputs <- function(x_range = NULL, y_range = NULL) {
    cache <- isolate(dotplot_state$plot_ui_cache %||% list())
    if (!is.list(cache)) cache <- list()
    axis_ranges <- cache$axis_ranges %||% list()
    if (is.numeric(x_range) && length(x_range) == 2L && all(is.finite(x_range))) {
      cache$x_axis_range <- x_range
      axis_ranges$x_axis_range <- x_range
    }
    if (is.numeric(y_range) && length(y_range) == 2L && all(is.finite(y_range))) {
      cache$y_axis_range <- y_range
      axis_ranges$y_axis_range <- y_range
    }
    cache$axis_ranges <- axis_ranges
    dotplot_state$plot_ui_cache <- cache
    invisible(TRUE)
  }

  begin_preset_update <- function() {
    generation <- isolate(dotplot_state$preset_update_generation %||% 0) + 1
    dotplot_state$preset_update_generation <- generation
    dotplot_state$preset_update_in_progress <- TRUE
    generation
  }

  finish_preset_update <- function(generation) {
    session$onFlushed(function() {
      session$onFlushed(function() {
        if (!identical(isolate(dotplot_state$preset_update_generation), generation)) {
          return()
        }

        dotplot_state$preset_update_in_progress <- FALSE

        plot_ready <- isTRUE(isolate(dotplot_state$plot_ready))
        dataset_mismatch <- isTRUE(isolate(is_active_dataset_mismatch()))

        if (plot_ready && !dataset_mismatch) {
          dotplot_debug_log("Preset update settled; triggering final plot update", 1)
          plot_update_trigger(isolate(plot_update_trigger()) + 1)
        }
      }, once = TRUE)
    }, once = TRUE)
  }

  observe({
    datawizard_import_ready_signature(rv)
    if (datawizard_import_barrier_active(rv)) {
      dotplot_debug_log("Import barrier active; preserving Dotplot choices until ready", 2)
      return()
    }
    if (length(dotplot_state$thresholds) > 0) {
      migrated_thresholds <- migrate_threshold_thickness(dotplot_state$thresholds)
      if (!identical(migrated_thresholds, dotplot_state$thresholds)) {
        dotplot_state$thresholds <- migrated_thresholds
        dotplot_debug_log("Migrated existing thresholds to include thickness and label_size parameters", 1)
      }
    }
  }, priority = 1000)

output$x_range_label <- renderText({
  plot_params <- dot_plot_parameters()

  if (!is.null(plot_params) && !is.null(plot_params$x_col) && !is.null(plot_params$x_transform)) {
    base_label <- plot_params$x_col
    transform_label <- create_transform_label(plot_params$x_transform)

    if (nzchar(transform_label)) {
      paste0(transform_label, "(", base_label, ") Range")
    } else {
      paste0(base_label, " Range")
    }
  } else {
    "X-Axis Range"
  }
})

# Dynamic Y-axis range label
output$y_range_label <- renderText({
  plot_params <- dot_plot_parameters()

  if (!is.null(plot_params) && !is.null(plot_params$y_col) && !is.null(plot_params$y_transform)) {
    base_label <- plot_params$y_col
    transform_label <- create_transform_label(plot_params$y_transform)

    if (nzchar(transform_label)) {
      paste0(transform_label, "(", base_label, ") Range")
    } else {
      paste0(base_label, " Range")
    }
  } else {
    "Y-Axis Range"
  }
})

# Update axis ranges when data or transformations change
observe({
  plot_params <- dot_plot_parameters()

  # Keep restored slider bounds/selection authoritative until restore replay settles.
  if (isTRUE(isolate(dotplot_state$restore_in_progress))) {
    return()
  }
  if (restored_cache_authoritative()) {
    return()
  }

  # When restored plot cache intentionally targets a different dataset than
  # currently loaded RV data, keep restored slider state authoritative.
  if (isTRUE(isolate(is_active_dataset_mismatch()))) {
    return()
  }

  # Only auto-update if:
  # 1. Auto-update is enabled
  # 2. We have plot parameters
  # 3. User hasn't manually set ranges OR they want auto-update
  if (!isTRUE(input$auto_update_ranges) || is.null(plot_params)) {
    return()
  }

  user_settings <- user_range_settings()

  # Skip auto-update if user has manually set ranges and auto-update is disabled
  if ((user_settings$x_range_user_set || user_settings$y_range_user_set) &&
      !isTRUE(input$auto_update_ranges)) {
    dotplot_debug_log("Skipping auto-update: User has set manual ranges", 1)
    return()
  }

  req(data_in(), plot_params$x_col, plot_params$y_col,
      plot_params$x_transform, plot_params$y_transform)

  tryCatch({
    dotplot_debug_log("Auto-updating axis ranges (user hasn't overridden)", 1)

    # Get current plot to extract actual ranges
    current_plot <- dotplot_state$current_plot

    if (!is.null(current_plot)) {
      # Extract actual plot ranges from ggplot object
      plot_ranges <- extract_plot_ranges(current_plot)

      if (!is.null(plot_ranges)) {
        dotplot_debug_log(paste("Using actual plot ranges - X:", paste(round(plot_ranges$x_range, 3), collapse = " to "),
                        "Y:", paste(round(plot_ranges$y_range, 3), collapse = " to ")), 1)

        # Calculate slider limits based on actual plot ranges
        x_limits <- calculate_smart_transformed_limits(plot_ranges$x_range, plot_params$x_transform)
        y_limits <- calculate_smart_transformed_limits(plot_ranges$y_range, plot_params$y_transform)

        # Only update if user hasn't set ranges manually
        if (!user_settings$x_range_user_set) {
          mark_programmatic_range_update("x_axis_range")
          updateSliderInput(session, "x_axis_range",
                            min = x_limits$min,
                            max = x_limits$max,
                            value = plot_ranges$x_range,
                            step = x_limits$step)
          dotplot_debug_log("Auto-updated X-axis range", 1)
        }

        if (!user_settings$y_range_user_set) {
          mark_programmatic_range_update("y_axis_range")
          updateSliderInput(session, "y_axis_range",
                            min = y_limits$min,
                            max = y_limits$max,
                            value = plot_ranges$y_range,
                            step = y_limits$step)
          dotplot_debug_log("Auto-updated Y-axis range", 1)
        }
        update_cached_range_inputs(
          x_range = if (!user_settings$x_range_user_set) plot_ranges$x_range else input$x_axis_range,
          y_range = if (!user_settings$y_range_user_set) plot_ranges$y_range else input$y_axis_range
        )

        return()
      }
    }

    # Fallback: Calculate from data if plot extraction fails
    dotplot_debug_log("Fallback: Calculating ranges from data", 1)

    x_range <- calculate_conservative_axis_range(data_in(), plot_params$x_col, plot_params$x_transform)
    y_range <- calculate_conservative_axis_range(data_in(), plot_params$y_col, plot_params$y_transform)

    x_limits <- calculate_smart_transformed_limits(x_range, plot_params$x_transform)
    y_limits <- calculate_smart_transformed_limits(y_range, plot_params$y_transform)

    # Only update ranges that user hasn't set manually
    if (!user_settings$x_range_user_set) {
      mark_programmatic_range_update("x_axis_range")
      updateSliderInput(session, "x_axis_range",
                        min = x_limits$min, max = x_limits$max,
                        value = x_range, step = x_limits$step)
    }

    if (!user_settings$y_range_user_set) {
      mark_programmatic_range_update("y_axis_range")
      updateSliderInput(session, "y_axis_range",
                        min = y_limits$min, max = y_limits$max,
                        value = y_range, step = y_limits$step)
    }
    update_cached_range_inputs(
      x_range = if (!user_settings$x_range_user_set) x_range else input$x_axis_range,
      y_range = if (!user_settings$y_range_user_set) y_range else input$y_axis_range
    )

    dotplot_debug_log(paste("Fallback ranges calculated - X:", paste(round(x_range, 3), collapse = " to "),
                    "Y:", paste(round(y_range, 3), collapse = " to ")), 1)

  }, error = function(e) {
    dotplot_debug_log(paste("Error in smart auto-update:", e$message), 1)
  })
}, priority = 1)

# Reset both ranges
observeEvent(input$reset_ranges, {
  req(dotplot_state$current_plot)

  tryCatch({
    dotplot_debug_log("Resetting ranges with smart step calculation", 1)

    # Extract original ranges from plot
    plot_ranges <- extract_ggplot_ranges(dotplot_state$current_plot)

    # Calculate smart configurations
    x_slider_config <- expand_range_for_slider(plot_ranges$x_range)
    y_slider_config <- expand_range_for_slider(plot_ranges$y_range)

    # Reset sliders with smart steps
    mark_programmatic_range_update("x_axis_range")
    updateSliderInput(session, "x_axis_range",
                      min = round(x_slider_config$min, x_slider_config$decimals),
                      max = round(x_slider_config$max, x_slider_config$decimals),
                      value = round(plot_ranges$x_range, x_slider_config$decimals),
                      step = x_slider_config$step)

    mark_programmatic_range_update("y_axis_range")
    updateSliderInput(session, "y_axis_range",
                      min = round(y_slider_config$min, y_slider_config$decimals),
                      max = round(y_slider_config$max, y_slider_config$decimals),
                      value = round(plot_ranges$y_range, y_slider_config$decimals),
                      step = y_slider_config$step)

    dotplot_debug_log(paste("Ranges reset with smart steps - X:", x_slider_config$step,
                    "Y:", y_slider_config$step), 1)
    showNotification("Ranges reset with optimized steps", type = "message", duration = 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error resetting smart ranges:", e$message), 1)
  })
})


observeEvent(input$GeneIdentifierColumn_dot, {
  # Only react if we have a plot and valid identifier
  if (!is.null(input$GeneIdentifierColumn_dot) &&
      nzchar(input$GeneIdentifierColumn_dot) &&
      !is.null(dotplot_state$current_plot)) {

    dotplot_debug_log(paste("Identifier changed to:", input$GeneIdentifierColumn_dot), 1)

    # Clear current selection since identifiers changed
    selected_points_interactive_dot(data.frame())
    dotplot_debug_log("Cleared selection due to identifier change", 2)

    # Note: The interactive plot will automatically update because it's reactive
    # to input$GeneIdentifierColumn_dot through the renderPlotly
    dotplot_debug_log("Interactive plot will update with new identifier", 2)

    if (isTRUE(dotplot_state$plot_ready)) {
      if (restored_cache_authoritative()) {
        dotplot_debug_log("Skipping live update while restored cache is authoritative", 1)
      } else if (isTRUE(isolate(is_active_dataset_mismatch()))) {
        dotplot_debug_log("Skipping live update on dataset mismatch; keep restored plot faithful", 1)
      } else {
      dotplot_debug_log("Identifier change detected after plot creation; triggering live plot update", 1)
      plot_update_trigger(plot_update_trigger() + 1)
      }
    }

    showNotification(paste("Identifier changed to:", input$GeneIdentifierColumn_dot),
                     type = "message", duration = 2)
  }
}, ignoreInit = TRUE)  # Don't trigger on initial load


observeEvent(input$x_axis_range, {
  req(dotplot_state$current_plot)

  pending <- isolate(dotplot_state$programmatic_range_update_pending %||% character(0))
  if ("x_axis_range" %in% pending) {
    dotplot_state$programmatic_range_update_pending <- setdiff(pending, "x_axis_range")
    update_cached_range_inputs(x_range = input$x_axis_range)
    return()
  }

  update_cached_range_inputs(x_range = input$x_axis_range)
  dotplot_debug_log("X-axis range changed by user - updating plot", 2)
  range_render_trigger(range_render_trigger() + 1)
}, ignoreInit = TRUE)

observeEvent(input$y_axis_range, {
  req(dotplot_state$current_plot)

  pending <- isolate(dotplot_state$programmatic_range_update_pending %||% character(0))
  if ("y_axis_range" %in% pending) {
    dotplot_state$programmatic_range_update_pending <- setdiff(pending, "y_axis_range")
    update_cached_range_inputs(y_range = input$y_axis_range)
    return()
  }

  update_cached_range_inputs(y_range = input$y_axis_range)
  dotplot_debug_log("Y-axis range changed by user - updating plot", 2)
  range_render_trigger(range_render_trigger() + 1)
}, ignoreInit = TRUE)

# ========================================
# Suggested Identifiers Display
# ========================================

output$geneSymbolList_dot <- renderPrint({
  req(rv$data_mod, rv$data_def)

  quiet_log <- function(...) invisible(NULL)

  # Get current identifier selection (you'll need to add this UI element)
  selected_identifier <- input$GeneIdentifierColumn_dot  # Add this to UI

  if (is.null(selected_identifier) || selected_identifier == "") {
    cat("Please select an identifier type first")
    return()
  }

  data <- rv$data_mod
  identifier <- c()
  gene_symbols_text <- ""

  if (!is.null(input$searchGene_dot) && input$searchGene_dot != "") {
    filter_data <- get_filter_string_dot(input$searchGene_dot, selected_identifier, quiet_log)
    filter_data <- as.vector(filter_data[,1])

    if (length(filter_data) > 0) {
      pattern <- paste(filter_data, collapse = "|")
      identifier <- grep(pattern, data[[selected_identifier]], ignore.case = TRUE, value = TRUE)
      gene_symbols_text <- paste(identifier, collapse = "\n")
    }
  }

  cat(gene_symbols_text)
})


# ========================================

last_axis_choice_signature <- NULL

observe({
  meta <- get_live_data_def()
  req(meta)
  if (restored_cache_authoritative()) {
    dotplot_debug_log("Skipping live axis choice updates while restored cache is authoritative", 2)
    return()
  }
  if (datawizard_metadata_defer_downstream_choices(rv)) {
    dotplot_debug_log("Metadata assignment pending; deferring axis choices", 2)
    return()
  }
  dotplot_debug_log("Data definition changed - updating axis choices", 2)

  tryCatch({
    data_live <- get_live_data_mod()
    numeric_cols <- dotplot_get_numeric_columns(meta, data_live)

    if (length(numeric_cols) > 0) {
      axis_choice_signature <- paste(
        paste(names(numeric_cols), collapse = "\r"),
        paste(unname(numeric_cols), collapse = "\r"),
        isolate(input$x_axis_column %||% ""),
        isolate(input$y_axis_column %||% ""),
        sep = "\n"
      )

      if (identical(axis_choice_signature, last_axis_choice_signature)) {
        dotplot_debug_log("Axis choices unchanged; skipping select input updates", 2)
        return()
      }

      last_axis_choice_signature <<- axis_choice_signature
      updateSelectInput(session, "x_axis_column",
                        choices = c("Select column..." = "", numeric_cols))
      updateSelectInput(session, "y_axis_column",
                        choices = c("Select column..." = "", numeric_cols))
      dotplot_debug_log(paste("Updated axis choices:", length(numeric_cols), "numeric columns available"), 2)
    } else {
      dotplot_debug_log("No numeric columns found in loaded data", 1)
      showNotification("No numeric columns found in data", type = "warning")
    }
  }, error = function(e) {
    dotplot_debug_log(paste("Error updating UI choices:", e$message), 1)
  })
})

# ========================================
# Axis Configuration Observers
# ========================================

observeEvent(c(input$x_axis_column, input$y_axis_column), {
  req(input$x_axis_column, input$y_axis_column)

  # During session restore, set_session_state() has already restored
  # axis_config$x_col/y_col and sync_dotplot_ui_from_state() has pushed the
  # saved x_axis_label/y_axis_label.  The input echo from updateSelectInput()
  # would re-fire this observer and overwrite the restored labels with
  # auto-derived ones.  Skip entirely while the restore guard is active.
  if (isTRUE(isolate(dotplot_state$restore_in_progress)) || restored_cache_authoritative()) {
    dotplot_debug_log("Skipping axis column observer while restored cache is authoritative", 2)
    return()
  }

  if (input$x_axis_column != "" && input$y_axis_column != "") {

    dotplot_state$axis_config$x_col <- input$x_axis_column
    dotplot_state$axis_config$y_col <- input$y_axis_column

    if (isTRUE(isolate(dotplot_state$preset_update_in_progress))) {
      return()
    }

    tryCatch({
      # Check if selected columns might be non-numeric and warn user
      if (!is.null(data_def_in())) {
        x_matches <- which(data_def_in()$Column == input$x_axis_column)
        y_matches <- which(data_def_in()$Column == input$y_axis_column)

        # Safe content extraction for warnings
        x_content <- if (length(x_matches) > 0) data_def_in()$Content[x_matches[1]] else "Unknown"
        y_content <- if (length(y_matches) > 0) data_def_in()$Content[y_matches[1]] else "Unknown"

        # Warn for potentially non-numeric columns
        non_numeric_indicators <- c("Identifier", "Gene Symbol", "Protein Name", "Description", "Accession")

        if (!is.na(x_content) && x_content %in% non_numeric_indicators) {
          showNotification(paste("Warning: X-axis column", input$x_axis_column, "may not be numeric"),
                           type = "warning", duration = 3)
        }

        if (!is.na(y_content) && y_content %in% non_numeric_indicators) {
          showNotification(paste("Warning: Y-axis column", input$y_axis_column, "may not be numeric"),
                           type = "warning", duration = 3)
        }

        data_live <- get_live_data_mod()
        if (is.data.frame(data_live)) {
          non_numeric_axes <- character(0)
          if (input$x_axis_column %in% names(data_live) && !is.numeric(data_live[[input$x_axis_column]])) {
            non_numeric_axes <- c(non_numeric_axes, paste0("X-axis '", input$x_axis_column, "'"))
          }
          if (input$y_axis_column %in% names(data_live) && !is.numeric(data_live[[input$y_axis_column]])) {
            non_numeric_axes <- c(non_numeric_axes, paste0("Y-axis '", input$y_axis_column, "'"))
          }
          if (length(non_numeric_axes) > 0) {
            showNotification(
              paste(
                "Dotplot axes must be numeric in the loaded data. Non-numeric selection(s):",
                paste(non_numeric_axes, collapse = "; ")
              ),
              type = "warning",
              duration = 5
            )
            dotplot_debug_log(
              paste("Non-numeric axis selection:", paste(non_numeric_axes, collapse = "; ")),
              1
            )
          }
        }

        # Update axis labels based on content and current transformation
        x_label <- dotplot_create_content_based_label(input$x_axis_column, data_def_in(),
                                                      dotplot_state$axis_config$x_transform)
        y_label <- dotplot_create_content_based_label(input$y_axis_column, data_def_in(),
                                                      dotplot_state$axis_config$y_transform)

        updateTextInput(session, "x_axis_label", value = x_label)
        updateTextInput(session, "y_axis_label", value = y_label)

        dotplot_debug_log(paste("Updated labels based on content - X:", x_label, "Y:", y_label), 1)
      }
    }, error = function(e) {
      dotplot_debug_log(paste("Error updating axis labels:", e$message), 1)
    })

    dotplot_debug_log(paste("Axis configuration updated - X:", input$x_axis_column, "Y:", input$y_axis_column), 1)

    if (isTRUE(dotplot_state$plot_ready)) {
      if (restored_cache_authoritative()) {
        dotplot_debug_log("Skipping live update while restored cache is authoritative", 1)
      } else if (isTRUE(isolate(is_active_dataset_mismatch()))) {
        dotplot_debug_log("Skipping live update on dataset mismatch; keep restored plot faithful", 1)
      } else {
      dotplot_debug_log("Axis column change detected after plot creation; triggering live plot update", 1)
      plot_update_trigger(plot_update_trigger() + 1)
      }
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)



# Axis Labels Observer
observeEvent(c(input$x_axis_label, input$y_axis_label), {
  preset_update_in_progress <- isTRUE(isolate(dotplot_state$preset_update_in_progress))
  if (restored_cache_authoritative()) {
    dotplot_debug_log("Skipping axis label observer while restored cache is authoritative", 2)
    return()
  }

  if (!preset_update_in_progress) {
    dotplot_debug_log(paste("Axis label inputs changed - X:", input$x_axis_label, "Y:", input$y_axis_label), 1)
  }

  dotplot_state$axis_config$x_label <- input$x_axis_label
  dotplot_state$axis_config$y_label <- input$y_axis_label

  if (preset_update_in_progress) {
    return()
  }

  dotplot_debug_log(paste("Axis config updated - X:", dotplot_state$axis_config$x_label, "Y:", dotplot_state$axis_config$y_label), 1)

  if (isTRUE(dotplot_state$plot_ready)) {
      if (isTRUE(isolate(is_active_dataset_mismatch()))) {
        dotplot_debug_log("Skipping live update on dataset mismatch; keep restored plot faithful", 1)
      } else {
    dotplot_debug_log("Axis label change detected after plot creation; triggering live plot update", 2)
    plot_update_trigger(plot_update_trigger() + 1)
      }
  }
}, ignoreInit = TRUE, ignoreNULL = FALSE)

observeEvent(c(input$x_transform, input$y_transform), {
  req(input$x_transform, input$y_transform)

  # During session restore, axis_config$x_transform/y_transform are already
  # set by set_session_state() and the saved labels are pushed by
  # sync_dotplot_ui_from_state().  The echo from updateRadioButtons() must not
  # overwrite the restored labels with auto-derived ones.
  if (isTRUE(isolate(dotplot_state$restore_in_progress)) || restored_cache_authoritative()) {
    dotplot_debug_log("Skipping transform observer while restored cache is authoritative", 2)
    return()
  }

  dotplot_state$axis_config$x_transform <- input$x_transform
  dotplot_state$axis_config$y_transform <- input$y_transform

  if (isTRUE(isolate(dotplot_state$preset_update_in_progress))) {
    return()
  }

  # Update UI labels when transformation changes
  if (!is.null(input$x_axis_column) && !is.null(input$y_axis_column) &&
      input$x_axis_column != "" && input$y_axis_column != "") {

    tryCatch({
      # Generate new labels with updated transformations
      x_label <- dotplot_generate_enhanced_axis_label(input$x_axis_column, data_def_in(),
                                                      input$x_transform, "x")
      y_label <- dotplot_generate_enhanced_axis_label(input$y_axis_column, data_def_in(),
                                                      input$y_transform, "y")

      updateTextInput(session, "x_axis_label", value = x_label)
      updateTextInput(session, "y_axis_label", value = y_label)

      dotplot_debug_log(paste("Labels updated for transformation change - X:", x_label, "Y:", y_label), 2)
    }, error = function(e) {
      dotplot_debug_log(paste("Error updating labels on transformation change:", e$message), 1)
    })
  }

  dotplot_debug_log(paste("Transformations updated - X:", input$x_transform, "Y:", input$y_transform), 2)

  if (isTRUE(dotplot_state$plot_ready)) {
      if (isTRUE(isolate(is_active_dataset_mismatch()))) {
        dotplot_debug_log("Skipping live update on dataset mismatch; keep restored plot faithful", 1)
      } else {
    dotplot_debug_log("Transformation change detected after plot creation; triggering live plot update", 1)
    plot_update_trigger(plot_update_trigger() + 1)
      }
  }
}, ignoreInit = TRUE)


observeEvent(input$apply_volcano_preset, {
  meta <- get_live_data_def()
  req(meta)

  dotplot_debug_log("Applying volcano plot preset", 1)
  preset_update_generation <- begin_preset_update()
  on.exit(finish_preset_update(preset_update_generation), add = TRUE)

  applied <- character(0)
  missing <- character(0)

  # Find suitable columns
  ratio_cols <- which(grepl("Abundance.*Ratio", meta$Content, ignore.case = TRUE) &
                        !grepl("p[-_]?[Vv]alue", meta$Content))
  pval_cols  <- which(grepl("p[-_]?[Vv]alue", meta$Content))

  if (length(ratio_cols) > 0) {
    target_x_col <- meta$Column[ratio_cols[1]]
    dotplot_state$axis_config$x_col <- target_x_col
    updateSelectInput(session, "x_axis_column", selected = target_x_col)
    applied <- c(applied, "X-axis (Abundance Ratio)")
  } else {
    missing <- c(missing, "Abundance Ratio column (X-axis)")
  }

  if (length(pval_cols) > 0) {
    target_y_col <- meta$Column[pval_cols[1]]
    dotplot_state$axis_config$y_col <- target_y_col
    updateSelectInput(session, "y_axis_column", selected = target_y_col)
    applied <- c(applied, "Y-axis (p-Value)")
  } else {
    missing <- c(missing, "p-Value column (Y-axis)")
  }

  target_x_transform <- "log2"
  target_y_transform <- "neg_log10"
  dotplot_state$axis_config$x_transform <- target_x_transform
  dotplot_state$axis_config$y_transform <- target_y_transform
  updateRadioButtons(session, "x_transform", selected = target_x_transform)
  updateRadioButtons(session, "y_transform", selected = target_y_transform)

  if (exists("target_x_col", inherits = FALSE) && exists("target_y_col", inherits = FALSE)) {
    x_label <- dotplot_generate_enhanced_axis_label(target_x_col, meta, target_x_transform, "x")
    y_label <- dotplot_generate_enhanced_axis_label(target_y_col, meta, target_y_transform, "y")
    dotplot_state$axis_config$x_label <- x_label
    dotplot_state$axis_config$y_label <- y_label
    updateTextInput(session, "x_axis_label", value = x_label)
    updateTextInput(session, "y_axis_label", value = y_label)
    applied <- c(applied, "axis labels")
  }

  updateTextInput(session, "plot_title", value = "Volcano Plot")
  applied <- c(applied, "transforms (log2 / -log10)", "title")

  # Add standard thresholds
  dotplot_state$thresholds <- list(
    list(id = "volcano_fc_pos", type = "vertical",   value = 1,   color = "#e74c3c", style = "dashed", label = "FC > 2",    thickness = 1, label_size = 3),
    list(id = "volcano_fc_neg", type = "vertical",   value = -1,  color = "#e74c3c", style = "dashed", label = "FC < 0.5",  thickness = 1, label_size = 3),
    list(id = "volcano_pval",   type = "horizontal", value = 1.3, color = "#E0E0E0", style = "dashed", label = "p < 0.05", thickness = 1, label_size = 3)
  )
  applied <- c(applied, "thresholds")

  # Defer Volcano region styling until threshold-derived tiles are available.
  # Top-left (2_1) and top-right (2_3) correspond to significant hits on either side of the FC boundary.
  dotplot_state$pending_volcano_region_styles <- list(
    "1_1" = list(color = "#440154", size = 1, alpha = 1, shape = 19),
    "1_3" = list(color = "#EFC000", size = 1, alpha = 1, shape = 19)
  )

  if (length(missing) == 0) {
    showNotification("Volcano plot preset applied.", type = "message")
    dotplot_debug_log("Volcano preset applied successfully", 1)
  } else {
    showNotification(
      paste0("Volcano preset applied (", paste(applied, collapse = ", "), "). ",
             "Missing: ", paste(missing, collapse = ", "), "."),
      type = "warning"
    )
    dotplot_debug_log(paste("Volcano preset partially applied; missing:", paste(missing, collapse = ", ")), 1)
  }
})


observeEvent(input$apply_ma_preset, {
  meta <- get_live_data_def()
  data_live <- get_live_data_mod()
  req(meta)

  dotplot_debug_log("Applying MA plot preset", 1)
  preset_update_generation <- begin_preset_update()
  on.exit(finish_preset_update(preset_update_generation), add = TRUE)

  applied <- character(0)
  missing <- character(0)

  # Y-axis: prefer Content == "Basemean", fallback to case-insensitive base[-_]?mean column name
  basemean_rows <- which(!is.na(meta$Content) & meta$Content == "Basemean")
  if (length(basemean_rows) == 0) {
    basemean_rows <- which(grepl("base[-_]?mean", meta$Column, ignore.case = TRUE))
  }
  basemean_col <- if (length(basemean_rows) > 0) meta$Column[basemean_rows[1]] else NULL

  if (!is.null(basemean_col) && nzchar(basemean_col)) {
    dotplot_state$axis_config$y_col <- basemean_col
    updateSelectInput(session, "y_axis_column", selected = basemean_col)
    applied <- c(applied, "Y-axis (Basemean)")
  } else {
    missing <- c(missing, "Basemean column (Y-axis)")
  }

  # X-axis: prefer Content matching "Abundance Ratio" (not p-value variants)
  ratio_rows <- which(
    !is.na(meta$Content) &
    grepl("Abundance Ratio", meta$Content, ignore.case = TRUE) &
    !grepl("p[-_]?[Vv]alue", meta$Content)
  )
  ratio_col <- if (length(ratio_rows) > 0) meta$Column[ratio_rows[1]] else NULL

  if (!is.null(ratio_col) && nzchar(ratio_col)) {
    dotplot_state$axis_config$x_col <- ratio_col
    updateSelectInput(session, "x_axis_column", selected = ratio_col)
    applied <- c(applied, "X-axis (Abundance Ratio)")
  } else {
    missing <- c(missing, "Abundance Ratio column (X-axis)")
  }

  # Reduce reactive churn during MA preset application (performance)
  freezeReactiveValue(input, "x_axis_column")
  freezeReactiveValue(input, "y_axis_column")
  freezeReactiveValue(input, "x_axis_label")
  freezeReactiveValue(input, "y_axis_label")
  freezeReactiveValue(input, "x_transform")
  freezeReactiveValue(input, "y_transform")
  freezeReactiveValue(input, "plot_title")

  # Always set labels even when target columns are missing
  dotplot_state$axis_config$x_label <- "log2(Abundance Ratio)"
  dotplot_state$axis_config$y_label <- "log2(BaseMean)"
  updateTextInput(session, "x_axis_label", value = dotplot_state$axis_config$x_label)
  updateTextInput(session, "y_axis_label", value = dotplot_state$axis_config$y_label)
  applied <- c(applied, "axis labels")

  target_x_transform <- "log2"
  target_y_transform <- "log2"
  dotplot_state$axis_config$x_transform <- target_x_transform
  dotplot_state$axis_config$y_transform <- target_y_transform
  updateRadioButtons(session, "x_transform", selected = target_x_transform)
  updateRadioButtons(session, "y_transform", selected = target_y_transform)
  updateTextInput(session, "plot_title", value = "MA Plot")
  applied <- c(applied, "transforms (log2 / log2)", "title")

  quantiles <- tryCatch(
    dotplot_safe_basemean_quantiles(data_live, basemean_col),
    error = function(e) {
      dotplot_debug_log(paste("Failed to calculate Basemean quantiles; using fallback:", e$message), 1)
      list(q1 = 1, q3 = 3, source = "fallback_error")
    }
  )

  y_transform_for_thresholds <- target_y_transform %||% (input$y_transform %||% "raw")
  q1_transformed <- dotplot_transform_threshold_value(quantiles$q1, y_transform_for_thresholds)
  q3_transformed <- dotplot_transform_threshold_value(quantiles$q3, y_transform_for_thresholds)

  if (!is.finite(q1_transformed) || !is.finite(q3_transformed)) {
    dotplot_debug_log("Invalid transformed quantile threshold(s); using fallback transformed defaults", 1)
    q1_transformed <- dotplot_transform_threshold_value(1, y_transform_for_thresholds)
    q3_transformed <- dotplot_transform_threshold_value(3, y_transform_for_thresholds)
  }

  low_intensity_label <- paste0("Low intensity (≤ Q1", if (quantiles$source == "data") "" else ", fallback", ")")
  high_intensity_label <- paste0("High intensity (≥ Q3", if (quantiles$source == "data") "" else ", fallback", ")")

  # MA thresholds: FC boundaries on abundance-ratio axis + low/high intensity on Basemean axis
  dotplot_state$thresholds <- list(
    list(id = "ma_fc_pos",         type = "vertical",   value =  1,              color = "#e74c3c", style = "dashed", label = "log2 FC = +1"),
    list(id = "ma_fc_neg",         type = "vertical",   value = -1,              color = "#e74c3c", style = "dashed", label = "log2 FC = -1"),
    list(id = "ma_low_intensity",  type = "horizontal", value = q1_transformed, color = "#888888", style = "dashed", label = low_intensity_label),
    list(id = "ma_high_intensity", type = "horizontal", value = q3_transformed, color = "#888888", style = "dashed", label = high_intensity_label)
  )
  applied <- c(applied, "quantile-based thresholds (Q1/Q3)")

  dotplot_debug_log(
    paste0("MA preset quantiles source=", quantiles$source,
           ", raw Q1=", signif(quantiles$q1, 4), ", raw Q3=", signif(quantiles$q3, 4),
           ", transformed Q1=", signif(q1_transformed, 4), ", transformed Q3=", signif(q3_transformed, 4),
           ", y_transform=", y_transform_for_thresholds),
    2
  )

  # Defer MA region styling until threshold-derived region tiles are available.
  # This avoids painting the whole plot when only a 1x1 region matrix exists.
  dotplot_state$pending_ma_region_styles <- list(
    "1_3" = list(color = "#EFC000", size = 1, alpha = 1, shape = 19),
    "1_1" = list(color = "#440154", size = 1, alpha = 1, shape = 19),
    "2_3" = list(color = "#FAE79B", size = 1, alpha = 1, shape = 19),
    "2_1" = list(color = "#8E79A8", size = 1, alpha = 1, shape = 19)
  )

  if (length(missing) == 0) {
    showNotification("MA plot preset applied.", type = "message")
    dotplot_debug_log("MA preset applied successfully", 1)
  } else {
    showNotification(
      paste0("MA preset applied (", paste(applied, collapse = ", "), "). ",
             "Missing: ", paste(missing, collapse = ", "), "."),
      type = "warning"
    )
    dotplot_debug_log(paste("MA preset partially applied; missing:", paste(missing, collapse = ", ")), 1)
  }
})



  invisible(NULL)
}
