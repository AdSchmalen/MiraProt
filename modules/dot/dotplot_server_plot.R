# ==============================================================================
# dotplot_server_plot.R - Dotplot plot and render observers
#
# Purpose: Hosts plot generation, rendering, download, and grid integration
# observers for the Dotplot module.
#
# Structure:
#   - Threshold and plot generation handlers
#   - Static/interactive rendering and download handlers
#   - Grid integration and plot refresh/tick management
#
# Dependencies: shiny, ggplot2, plotly, DT
# Called by: modDotPlotServer()
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_plot_observers
# Purpose: Initializes Dotplot plotting observers and rendering outputs.
# Structure:
#   - Section 1: Manage threshold creation and plot generation triggers.
#   - Section 2: Render static and interactive plots and configure downloads.
#   - Section 3: Handle grid export, tick settings, and update-trigger re-renders.
# Parameters:
#   - input/output/session/ns/dotplot_debug_log: [various] - Standard module dependencies.
#   - rv: [reactivevalues] - Shared application state.
#   - dotplot_state: [reactivevalues] - Dotplot state container.
#   - data_in: [reactive] - Input data reactive.
#   - data_def_in: [reactive] - Metadata reactive.
#   - region_configs: [reactiveVal] - Region style configuration store.
#   - region_structure: [reactive] - Region matrix metadata reactive.
#   - plot_update_trigger: [reactiveVal] - Full plot rebuild trigger counter.
#   - range_render_trigger: [reactiveVal] - Render-only range update trigger counter.
#   - modEnv: [environment] - Shared module environment with grid integration.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
dotplot_init_plot_observers <- function(input, output, session, ns, dotplot_debug_log, rv, dotplot_state, data_in, data_def_in,
                                        region_configs, region_structure, plot_update_trigger, range_render_trigger, modEnv,
                                        selected_region = NULL,
                                        pending_ui_inputs = NULL,
                                        dotplot_ui_input_ids = character(),
                                        dot_protein_labels = NULL) {
  auto_range_recalc_pending <- reactiveVal(FALSE)
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

  store_natural_axis_ranges <- function(plot_result) {
    ranges <- tryCatch(
      extract_dotplot_data_ranges(plot_result$data),
      error = function(e) NULL
    )

    if (!is.null(ranges)) {
      dotplot_state$natural_axis_ranges <- ranges
      dotplot_debug_log("Stored natural axis ranges from prepared dotplot data", 2)
    } else {
      dotplot_state$natural_axis_ranges <- NULL
      dotplot_debug_log("Prepared dotplot data ranges unavailable; ggplot fallback may be used", 2)
    }

    invisible(ranges)
  }

  get_natural_axis_ranges <- function(plot_result = NULL) {
    stored_ranges <- tryCatch(isolate(dotplot_state$natural_axis_ranges), error = function(e) NULL)
    if (!is.null(stored_ranges) &&
        is.numeric(stored_ranges$x_range) && length(stored_ranges$x_range) == 2L &&
        all(is.finite(stored_ranges$x_range)) &&
        is.numeric(stored_ranges$y_range) && length(stored_ranges$y_range) == 2L &&
        all(is.finite(stored_ranges$y_range))) {
      return(stored_ranges)
    }

    if (is.null(plot_result)) {
      plot_result <- tryCatch(isolate(dotplot_state$current_plot), error = function(e) NULL)
    }

    data_ranges <- tryCatch(
      extract_dotplot_data_ranges(plot_result$data),
      error = function(e) NULL
    )
    if (!is.null(data_ranges)) {
      dotplot_state$natural_axis_ranges <- data_ranges
      return(data_ranges)
    }

    if (!is.null(plot_result)) {
      return(tryCatch(extract_ggplot_ranges(plot_result), error = function(e) NULL))
    }

    NULL
  }

  mark_programmatic_range_update <- function(ids) {
    ids <- intersect(ids, c("x_axis_range", "y_axis_range"))
    if (length(ids) == 0L) return(invisible(FALSE))
    pending <- isolate(dotplot_state$programmatic_range_update_pending %||% character(0))
    dotplot_state$programmatic_range_update_pending <- unique(c(pending, ids))
    invisible(TRUE)
  }


  observeEvent(input$generate_plot, {
    dotplot_state$plot_from_restore_cache <- FALSE
    data_live <- get_live_data_mod()
    data_def_live <- get_live_data_def()
    req(data_live, data_def_live)

    auto_range_recalc_pending(TRUE)
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(c(input$x_axis_column, input$y_axis_column, input$x_transform, input$y_transform), {
    if (isTRUE(isolate(dotplot_state$restore_in_progress)) ||
        isTRUE(isolate(dotplot_state$plot_from_restore_cache))) {
      auto_range_recalc_pending(FALSE)
      dotplot_debug_log("Skipping auto range/tick recalculation for restored cached dotplot", 2)
      return()
    }
    if (isTRUE(dotplot_state$plot_ready)) {
      auto_range_recalc_pending(TRUE)
      dotplot_debug_log("Queued auto range/tick recalculation due to axis mapping/transformation change", 2)
    }
  }, ignoreInit = TRUE)

threshold_row_click_state <- reactiveValues(last_row = NA_integer_, last_time = as.POSIXct(NA))
threshold_edit_row <- reactiveVal(NULL)

show_threshold_modal <- function(mode = c("add", "edit"), threshold = NULL, row_index = NULL) {
  mode <- match.arg(mode)
  is_edit <- identical(mode, "edit")

  if (is_edit && (is.null(threshold) || is.null(row_index))) {
    dotplot_debug_log("Threshold edit requested without valid row context", 1)
    return(invisible(NULL))
  }

  modal_title <- if (is_edit) "Edit Threshold" else "Add Threshold"
  confirm_label <- if (is_edit) "Save" else "Add"

  showModal(modalDialog(
    title = modal_title,
    size = "m",
    fluidRow(
      column(6,
             radioButtons(ns("threshold_type"), "Type:",
                          choices = c("Vertical (X-value)" = "vertical",
                                      "Horizontal (Y-value)" = "horizontal"),
                          selected = threshold$type %||% "vertical")
      ),
      column(6,
             numericInput(ns("threshold_value"), "Value:", value = threshold$value %||% 0)
      )
    ),
    fluidRow(
      column(4,
             colourInput(ns("threshold_color"), "Color:", value = threshold$color %||% "#e74c3c")
      ),
      column(4,
             selectInput(ns("threshold_style"), "Line Style:",
                         choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"),
                         selected = threshold$style %||% "dashed")
      ),
      column(4,
             numericInput(ns("threshold_thickness"), "Thickness:",
                          value = threshold$thickness %||% 1, min = 0.1, max = 5, step = 0.1)
      )
    ),
    fluidRow(
      column(8,
             textInput(ns("threshold_label"), "Label (optional):", value = threshold$label %||% "", width = "100%")
      ),
      column(4,
             numericInput(ns("threshold_label_size"), "Label Size:",
                          value = threshold$label_size %||% 3, min = 1, max = 8, step = 0.5)
      )
    ),
    footer = tagList(
      actionButton(ns("confirm_add_threshold"), confirm_label, class = "btn-primary"),
      modalButton("Cancel")
    )
  ))

  if (is_edit) {
    threshold_edit_row(row_index)
    dotplot_debug_log(paste("Threshold edit modal opened for row", row_index), 2)
  } else {
    threshold_edit_row(NULL)
    dotplot_debug_log("Threshold add modal opened", 2)
  }

  invisible(NULL)
}

observeEvent(input$add_threshold, {
  show_threshold_modal(mode = "add")
})

observeEvent(input$threshold_table_cell_clicked, {
  cell_info <- input$threshold_table_cell_clicked

  if (is.null(cell_info) || is.null(cell_info$row)) {
    return()
  }

  clicked_row <- suppressWarnings(as.integer(cell_info$row))
  if (!is.finite(clicked_row) || clicked_row < 1 || clicked_row > length(dotplot_state$thresholds)) {
    dotplot_debug_log("Ignoring threshold table click with invalid row", 2)
    return()
  }

  current_time <- Sys.time()
  last_row <- threshold_row_click_state$last_row
  last_time <- threshold_row_click_state$last_time
  delta_seconds <- if (!is.na(last_time)) as.numeric(difftime(current_time, last_time, units = "secs")) else Inf

  threshold_row_click_state$last_row <- clicked_row
  threshold_row_click_state$last_time <- current_time

  if (!isTRUE(last_row == clicked_row) || delta_seconds > 0.5) {
    return()
  }

  selected_threshold <- dotplot_state$thresholds[[clicked_row]]
  dotplot_debug_log(paste("Detected threshold table double-click on row", clicked_row), 1)
  show_threshold_modal(mode = "edit", threshold = selected_threshold, row_index = clicked_row)
}, ignoreInit = TRUE)

observeEvent(input$confirm_add_threshold, {
  threshold_payload <- list(
    type = input$threshold_type,
    value = input$threshold_value,
    color = input$threshold_color,
    style = input$threshold_style,
    thickness = input$threshold_thickness %||% 1,
    label = input$threshold_label,
    label_size = input$threshold_label_size %||% 3
  )

  edit_row <- threshold_edit_row()
  is_edit <- !is.null(edit_row)

  if (is_edit) {
    if (edit_row < 1 || edit_row > length(dotplot_state$thresholds)) {
      showNotification("Selected threshold row is out of range.", type = "error", duration = 3)
      dotplot_debug_log("Threshold edit failed because selected row was out of range", 1)
      threshold_edit_row(NULL)
      removeModal()
      return()
    }

    threshold_payload$id <- dotplot_state$thresholds[[edit_row]]$id %||% paste0("thresh_", edit_row)
    dotplot_state$thresholds[[edit_row]] <- threshold_payload
    dotplot_debug_log(paste("Threshold updated on row", edit_row, ":", threshold_payload$type, "=", threshold_payload$value), 1)
    showNotification("Threshold updated successfully.", type = "message", duration = 2)
  } else {
    threshold_payload$id <- paste0("thresh_", length(dotplot_state$thresholds) + 1)
    dotplot_state$thresholds <- append(dotplot_state$thresholds, list(threshold_payload))
    dotplot_debug_log(paste("Threshold added:", threshold_payload$type, "=", threshold_payload$value,
                    "thickness =", threshold_payload$thickness, "label_size =", threshold_payload$label_size), 1)
    showNotification("Threshold added successfully.", type = "message", duration = 2)
  }

  threshold_edit_row(NULL)
  removeModal()

  if (isTRUE(dotplot_state$plot_ready)) {
    dotplot_debug_log("Threshold change applied after initial plot; triggering live plot update", 1)
    plot_update_trigger(plot_update_trigger() + 1)
  }
})

observeEvent(input$remove_threshold, {
  selected_row <- input$threshold_table_rows_selected

  if (is.null(selected_row) || length(selected_row) == 0) {
    showNotification("Please select a threshold row to remove.", type = "warning", duration = 3)
    dotplot_debug_log("Threshold removal requested without a selected row", 1)
    return()
  }

  if (selected_row < 1 || selected_row > length(dotplot_state$thresholds)) {
    showNotification("Selected threshold row is out of range.", type = "error", duration = 3)
    dotplot_debug_log("Threshold removal failed because selected row was out of range", 1)
    return()
  }

  removed_threshold <- dotplot_state$thresholds[[selected_row]]
  dotplot_state$thresholds <- dotplot_state$thresholds[-selected_row]

  dotplot_debug_log(paste("Threshold removed:", removed_threshold$type, "=", removed_threshold$value), 1)
  showNotification("Threshold removed successfully.", type = "message", duration = 2)

  if (isTRUE(dotplot_state$plot_ready)) {
    dotplot_debug_log("Threshold removed after initial plot; triggering live plot update", 1)
    plot_update_trigger(plot_update_trigger() + 1)
  }
}, ignoreInit = TRUE)

# ========================================
# Plot Generation with Region Support
# ========================================

observeEvent(input$generate_plot, {
  dotplot_state$plot_from_restore_cache <- FALSE
  data_live <- get_live_data_mod()
  data_def_live <- get_live_data_def()
  req(data_live, data_def_live)

  dotplot_debug_log("Starting plot generation with custom text sizes and tick intervals", 1)

  # Validate configuration
  validation <- dotplot_validate_config(dotplot_state$axis_config, data_def_live, data_live)
  if (!validation$valid) {
    showNotification(validation$message, type = "error")
    return()
  }

  # Generate plot with region support
  tryCatch({

    # Create UI labels manually
    ui_labels <- list(
      x_label = input$x_axis_label,
      y_label = input$y_axis_label
    )

    # Extract plot parameters INCLUDING the new text sizes and tick intervals
    plot_params <- dotplot_extract_plot_parameters(input)
    dotplot_state$plot_request <- plot_params
    dotplot_debug_log(paste("Using text sizes - Title:", plot_params$title_size,
                    "Axis:", plot_params$axis_title_size, "Tick:", plot_params$tick_size), 2)
    dotplot_debug_log(paste("Using tick intervals - X:", plot_params$x_tick_interval,
                    "Y:", plot_params$y_tick_interval), 2)

    # Use enhanced function if it exists, otherwise fallback
    if (exists("dotplot_create_complete_plot_with_regions_enhanced")) {
      plot_result <- dotplot_create_complete_plot_with_regions_enhanced(
        data = data_live,
        data_def = data_def_live,
        axis_config = dotplot_state$axis_config,
        thresholds = dotplot_state$thresholds,
        color_rules = dotplot_state$color_rules,
        plot_params = plot_params,
        region_configs = region_configs(),
        region_structure = region_structure(),
        ui_labels = ui_labels
      )
    } else {
      # Fallback to standard function
      plot_result <- dotplot_create_complete_plot_with_regions(
        data = data_live,
        data_def = data_def_live,
        axis_config = dotplot_state$axis_config,
        thresholds = dotplot_state$thresholds,
        color_rules = dotplot_state$color_rules,
        plot_params = plot_params,
        region_configs = region_configs(),
        region_structure = region_structure()
      )
    }

    # Store the plot with NATURAL ggplot ranges
    if (!is.null(plot_result)) {
      store_natural_axis_ranges(plot_result)
      dotplot_state$current_plot <- plot_result
      dotplot_state$base_plot_without_labels <- plot_result
      dotplot_state$plot_ready <- TRUE
      dotplot_state$plot_creation_cache <- list(data_mod = data_live, data_def = data_def_live)
      dotplot_state$source_data_signature <- if (is.data.frame(data_live) && is.data.frame(data_def_live)) {
        paste0(nrow(data_live), "x", ncol(data_live), "::", nrow(data_def_live), "x", ncol(data_def_live), "::",
               paste(colnames(data_live), collapse = "|"), "::", paste(colnames(data_def_live), collapse = "|"))
      } else NULL
      if (isTRUE(isolate(dotplot_state$restore_in_progress)) && is.list(dotplot_state$plot_ui_cache)) {
        # During restore, keep the captured UI snapshot authoritative;
        # live input echoes can still be transient/default at this point.
      } else {
        dotplot_state$plot_ui_cache <- dotplot_flatten_plot_ui_cache_for_restore(
          dotplot_capture_plot_ui_cache(input, dotplot_state, region_configs, region_structure)
        )
      }
      dotplot_state$plot_cache_ref_by_title <- stats::setNames(
        list(dotplot_state$source_data_signature %||% ""),
        as.character(input$plot_title %||% "default")[1]
      )
      dotplot_state$plot_data_cache_ref <- dotplot_build_cache_key()

      dotplot_debug_log("Plot generation completed with custom styling", 1)

      selected_region_label <- if (!is.null(selected_region)) {
        tryCatch(selected_region(), error = function(e) NULL)
      } else {
        NULL
      }
      if (is.null(selected_region_label) || !nzchar(as.character(selected_region_label))) {
        selected_region_label <- "none"
      }

      axis_cfg <- dotplot_state$axis_config %||% list()
      x_col <- as.character(axis_cfg$x_col %||% "n/a")
      y_col <- as.character(axis_cfg$y_col %||% "n/a")
      x_transform <- as.character(axis_cfg$x_transform %||% "raw")
      y_transform <- as.character(axis_cfg$y_transform %||% "raw")

      plot_params_safe <- plot_params %||% list()
      input_row_count <- nrow(data_live %||% data.frame())
      point_data <- tryCatch(plot_result$data, error = function(e) NULL)
      plotted_point_count <- if (is.data.frame(point_data)) nrow(point_data) else NA_integer_
      dropped_row_count <- if (is.na(plotted_point_count)) NA_integer_ else input_row_count - plotted_point_count
      row_count_summary <- paste0("Input rows: ", input_row_count)
      if (!is.na(plotted_point_count)) {
        row_count_summary <- paste0(row_count_summary, " | Plotted points: ", plotted_point_count)
      }
      if (!is.na(dropped_row_count) && dropped_row_count > 0L) {
        row_count_summary <- paste0(row_count_summary, " | Dropped rows: ", dropped_row_count)
      }

      dotplot_debug_log(
        sprintf(
          paste0(
            "Dot plot summary",
            " | %s",
            " | X column: %s",
            " | Y column: %s",
            " | X transform: %s",
            " | Y transform: %s",
            " | Selected region: %s",
            " | Dot size: %s",
            " | Dot alpha: %s",
            " | Color palette: %s",
            " | Theme: %s"
          ),
          row_count_summary,
          x_col,
          y_col,
          x_transform,
          y_transform,
          as.character(selected_region_label),
          as.character(plot_params_safe$dot_size %||% "n/a"),
          as.character(plot_params_safe$dot_alpha %||% "n/a"),
          as.character(plot_params_safe$color_palette %||% "n/a"),
          as.character(plot_params_safe$theme %||% "n/a")
        ),
        level = 0
      )

      thresholds_safe <- dotplot_state$thresholds
      if (!is.list(thresholds_safe)) thresholds_safe <- list()
      if (length(thresholds_safe) == 0) {
        dotplot_debug_log("Dot plot thresholds summary | Thresholds: none", level = 0)
      } else {
        threshold_entries <- vapply(seq_along(thresholds_safe), function(i) {
          th <- thresholds_safe[[i]] %||% list()
          sprintf(
            "#%d type=%s; value=%s; color=%s; style=%s; thickness=%s; label=%s; label_size=%s",
            i,
            as.character(th$type %||% "n/a"),
            as.character(th$value %||% "n/a"),
            as.character(th$color %||% "n/a"),
            as.character(th$style %||% "n/a"),
            as.character(th$thickness %||% "n/a"),
            as.character(th$label %||% ""),
            as.character(th$label_size %||% "n/a")
          )
        }, FUN.VALUE = character(1))
        dotplot_debug_log(
          sprintf(
            "Dot plot thresholds summary | Threshold count: %d | Definitions: %s",
            length(thresholds_safe),
            paste(threshold_entries, collapse = " | ")
          ),
          level = 0
        )
      }

      region_configs_safe <- region_configs() %||% list()
      if (!is.list(region_configs_safe) || length(region_configs_safe) == 0) {
        dotplot_debug_log("Dot plot region style summary | Region-specific styles: none", level = 0)
      } else {
        region_style_entries <- vapply(names(region_configs_safe), function(region_id) {
          cfg <- region_configs_safe[[region_id]] %||% list()
          sprintf(
            "%s: color=%s; size=%s; alpha=%s; shape=%s",
            as.character(region_id),
            as.character(cfg$color %||% "n/a"),
            as.character(cfg$size %||% "n/a"),
            as.character(cfg$alpha %||% "n/a"),
            as.character(cfg$shape %||% "n/a")
          )
        }, FUN.VALUE = character(1))
        dotplot_debug_log(
          sprintf(
            "Dot plot region style summary | Styled regions: %d | Definitions: %s",
            length(region_style_entries),
            paste(region_style_entries, collapse = " | ")
          ),
          level = 0
        )
      }

      if (is.data.frame(point_data) && "region_id" %in% names(point_data) && nrow(point_data) > 0) {
        region_counts <- table(as.character(point_data$region_id), useNA = "no")
        region_count_str <- paste(
          paste0(names(region_counts), "=", as.integer(region_counts)),
          collapse = ", "
        )
        dotplot_debug_log(
          sprintf(
            "Dot plot region point summary | Total points: %d | Region counts: {%s}",
            nrow(point_data),
            region_count_str
          ),
          level = 0
        )
      } else {
        dotplot_debug_log(
          sprintf(
            "Dot plot region point summary | Total points: %d | Region counts: unavailable",
            nrow(data_in() %||% data.frame())
          ),
          level = 0
        )
      }

    } else {
      dotplot_debug_log("Plot generation returned NULL", 1)
      showNotification("Plot generation failed", type = "error")
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Plot generation error:", e$message), 1)
    showNotification(paste("Error:", e$message), type = "error")
  })
})

# --------------------------------------------------------------------------
# get_current_display_dotplot
#
# Dynamic label application (mirrors volcano's get_current_display_plot()).
# Returns the plot that should be rendered RIGHT NOW based on the current
# reactive state.  When dot_protein_labels() is a non-empty data.frame and
# base_plot_without_labels is available, this rebuilds the display plot by
# re-applying the labels on top of the unlabelled base.  This path is the
# only thing that makes session restoration of protein labels reliable:
# regardless of whether the saved current_plot survives serialization with
# its ggrepel layer intact, as long as dot_protein_labels was restored, the
# render pass can rebuild the labelled plot from scratch.
#
# Falls back to dotplot_state$current_plot for the case where labels have
# never been applied (or the base plot is unavailable), preserving the
# existing behaviour on a fresh session.
# --------------------------------------------------------------------------
get_current_display_dotplot <- function() {
  current <- dotplot_state$current_plot
  base    <- dotplot_state$base_plot_without_labels
  labels  <- tryCatch(
    if (!is.null(dot_protein_labels)) dot_protein_labels() else NULL,
    error = function(e) NULL
  )

  has_labels <- !is.null(labels) && is.data.frame(labels) && nrow(labels) > 0

  # Preferred path: base plot + dynamic labels (always reflects the current
  # dot_protein_labels reactive, so restored label data is honoured).
  if (has_labels && !is.null(base)) {
    labelled <- tryCatch(
      apply_labels_to_dot_plot_enhanced_FIXED(base, labels, input, dotplot_debug_log),
      error = function(e) {
        dotplot_debug_log(paste("Dynamic label application failed:", e$message), 1)
        NULL
      }
    )
    if (!is.null(labelled)) return(labelled)
  }

  # Fallback path: return whatever is in current_plot.  Covers the
  # no-labels-yet case and the defensive case where re-application failed.
  current
}

output$dotplot_main <- renderPlot({
  # React to full plot rebuilds and range-only render updates.
  plot_update_trigger()
  range_render_trigger()

  req(dotplot_state$plot_ready, dotplot_state$current_plot)

  dotplot_debug_log("Rendering plot with current slider ranges", 2)

  # Use the dynamic-label helper so restored dot_protein_labels are always
  # visible on the rendered plot, even if the restored current_plot lost
  # its ggrepel layer during (de)serialization.
  plot_obj <- get_current_display_dotplot()
  if (is.null(plot_obj)) plot_obj <- dotplot_state$current_plot

  # Use plotmath for transformed axis labels in the native rendered plot,
  # matching download and Grid rendering.
  plot_obj <- dotplot_prepare_static_axis_labels(
    plot_obj
  )

  # Apply axis ranges. During restore, the slider inputs can briefly hold
  # transient/default values before sync_dotplot_ui_from_state() settles.
  # Prefer captured restore values in that window so the static plot matches
  # the interactive plot geometry immediately.
  resolve_axis_range <- function(input_val, cache_key) {
    candidate <- input_val
    if (isTRUE(isolate(dotplot_state$restore_in_progress)) &&
        is.list(dotplot_state$plot_ui_cache) &&
        !is.null(dotplot_state$plot_ui_cache[[cache_key]])) {
      candidate <- dotplot_state$plot_ui_cache[[cache_key]]
    }
    if (is.null(candidate) || length(candidate) != 2 || !all(is.finite(candidate))) {
      return(NULL)
    }
    # Degenerate ranges (min == max) collapse the axis into a single line.
    if (isTRUE(all.equal(candidate[1], candidate[2]))) {
      return(NULL)
    }
    candidate
  }

  x_range <- resolve_axis_range(input$x_axis_range, "x_axis_range")
  y_range <- resolve_axis_range(input$y_axis_range, "y_axis_range")

  if (!is.null(x_range) && length(x_range) == 2 && all(is.finite(x_range))) {
    if (!is.null(y_range) && length(y_range) == 2 && all(is.finite(y_range))) {
      plot_obj <- plot_obj + coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE)
      dotplot_debug_log(paste("Applied both ranges - X:", paste(round(x_range, 3), collapse = " to "),
                      "Y:", paste(round(y_range, 3), collapse = " to ")), 2)
    } else {
      plot_obj <- plot_obj + coord_cartesian(xlim = x_range, expand = FALSE)
      dotplot_debug_log(paste("Applied X range only:", paste(round(x_range, 3), collapse = " to ")), 2)
    }
  } else if (!is.null(y_range) && length(y_range) == 2 && all(is.finite(y_range))) {
    plot_obj <- plot_obj + coord_cartesian(ylim = y_range, expand = FALSE)
    dotplot_debug_log(paste("Applied Y range only:", paste(round(y_range, 3), collapse = " to ")), 2)
  } else {
    dotplot_debug_log("No valid ranges to apply - using default plot", 2)
  }

  return(plot_obj)
}, height = 600)

# ========================================
# Interactive Plot Output
# ========================================

output$dotplot_interactive <- renderPlotly({
  req(dotplot_state$plot_ready, dotplot_state$current_plot)

  tryCatch({
    identifier_col <- input$GeneIdentifierColumn_dot

    # Use the same dynamic-label helper so the interactive plot reflects
    # restored protein labels on top of the base plot (ggplotly typically
    # drops ggrepel segments, but having the label-bearing geom_text_repel
    # layer present still surfaces the labels in the rendered output).
    display_plot <- get_current_display_dotplot()
    if (is.null(display_plot)) display_plot <- dotplot_state$current_plot

    # Convert ggplot to plotly
    plotly_obj <- plotly::ggplotly(display_plot, tooltip = c("x", "y"))

    # Resolve axis labels from axis_config (kept in sync with input$x_axis_label / y_axis_label)
    axis_cfg <- dotplot_state$axis_config
    x_label <- if (!is.null(axis_cfg) && !is.null(axis_cfg$x_label) && nzchar(axis_cfg$x_label)) {
      axis_cfg$x_label
    } else if (!is.null(axis_cfg) && !is.null(axis_cfg$x_col)) {
      axis_cfg$x_col
    } else {
      "X"
    }
    y_label <- if (!is.null(axis_cfg) && !is.null(axis_cfg$y_label) && nzchar(axis_cfg$y_label)) {
      axis_cfg$y_label
    } else if (!is.null(axis_cfg) && !is.null(axis_cfg$y_col)) {
      axis_cfg$y_col
    } else {
      "Y"
    }

    # Guard: ensure plotly_obj has the expected structure before modifying traces
    has_traces <- !is.null(plotly_obj$x$data) && length(plotly_obj$x$data) > 0

    identifier_applied <- FALSE
    if (has_traces &&
        !is.null(identifier_col) && nzchar(identifier_col) &&
        tryCatch(!is.null(data_in()), error = function(e) FALSE) &&
        identifier_col %in% colnames(data_in())) {

      # Retrieve plot data; row_index maps each plot row back to the raw data row
      plot_data <- tryCatch(dotplot_state$current_plot$data, error = function(e) NULL)
      raw_ids   <- data_in()[[identifier_col]]

      row_index_ok <- !is.null(plot_data) &&
                      is.data.frame(plot_data) &&
                      "row_index" %in% colnames(plot_data) &&
                      nrow(plot_data) > 0 &&
                      all(is.finite(plot_data$row_index)) &&
                      length(raw_ids) >= max(plot_data$row_index)

      if (row_index_ok) {
        # Build identifier vector aligned with plot_data rows
        plotdata_ids <- raw_ids[plot_data$row_index]

        # Pre-compute a named lookup: coordinate key -> identifier.
        # Use a separator that cannot appear in numeric representations.
        KEY_SEP  <- "__|__"
        plot_keys <- paste(plot_data$x, plot_data$y, sep = KEY_SEP)
        id_lookup <- setNames(as.character(plotdata_ids), plot_keys)

        hover_template <- paste0(
          "<b>%{customdata}</b><br>",
          x_label, ": %{x}<br>",
          y_label, ": %{y}<extra></extra>"
        )

        for (i in seq_along(plotly_obj$x$data)) {
          n_pts <- length(plotly_obj$x$data[[i]]$x)
          if (n_pts == 0) next

          trace_keys <- paste(plotly_obj$x$data[[i]]$x,
                              plotly_obj$x$data[[i]]$y, sep = KEY_SEP)
          trace_ids  <- unname(id_lookup[trace_keys])

          plotly_obj$x$data[[i]]$customdata    <- trace_ids
          plotly_obj$x$data[[i]]$hovertemplate <- hover_template
        }

        identifier_applied <- TRUE
        dotplot_debug_log(paste("Applied identifier hover for column:", identifier_col), 2)
      } else {
        dotplot_debug_log("row_index unavailable or length mismatch - skipping identifier hover", 1)
      }
    }

    if (!identifier_applied && has_traces) {
      # No identifier selected or unavailable: still show axis-labeled hover
      hover_template_no_id <- paste0(
        x_label, ": %{x}<br>",
        y_label, ": %{y}<extra></extra>"
      )
      for (i in seq_along(plotly_obj$x$data)) {
        if (length(plotly_obj$x$data[[i]]$x) > 0) {
          plotly_obj$x$data[[i]]$hovertemplate <- hover_template_no_id
        }
      }
      dotplot_debug_log("No identifier column selected - applied axis-labeled hover", 2)
    }

    # Enable box selection by default
    plotly_obj <- plotly_obj %>%
      plotly::layout(dragmode = "select")

    plotly_obj$x$source <- "dotplot_plot"

    # Register events to prevent warnings and enable interactivity
    plotly_obj <- plotly_obj %>%
      plotly::event_register("plotly_selected") %>%
      plotly::event_register("plotly_click")

    dotplot_debug_log("Interactive plot with enhanced hover created", 2)
    return(plotly_obj)

  }, error = function(e) {
    dotplot_debug_log(paste("Error creating interactive plot:", e$message), 1)
    return(NULL)
  })
})

# ========================================
# Download Handler
# ========================================

output$downloadPlotButton_Dotplot <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0("dotplot_", timestamp, ".", input$downloadFormat_Dotplot)
  },
  content = function(file) {
    req(dotplot_state$current_plot)

    dotplot_debug_log("DOTPLOT DOWNLOAD: Starting plot download", 1)

    tryCatch({
      width_inches <- input$plotWidthInch_Dotplot
      height_inches <- input$plotHeightInch_Dotplot
      dpi <- input$resolution_DPI_Dotplot
      format <- input$downloadFormat_Dotplot

      plot_obj <-
        dotplot_prepare_static_axis_labels(
          dotplot_state$current_plot
        )

      x_range <- input$x_axis_range
      y_range <- input$y_axis_range

      if (!is.null(x_range) && length(x_range) == 2 && all(is.finite(x_range))) {
        if (!is.null(y_range) && length(y_range) == 2 && all(is.finite(y_range))) {
          plot_obj <- plot_obj + coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE)
        } else {
          plot_obj <- plot_obj + coord_cartesian(xlim = x_range, expand = FALSE)
        }
      } else if (!is.null(y_range) && length(y_range) == 2 && all(is.finite(y_range))) {
        plot_obj <- plot_obj + coord_cartesian(ylim = y_range, expand = FALSE)
      }

      previous_device <- grDevices::dev.cur()

      if (format %in% c("png", "jpeg", "tiff")) {
        device_function <- match.fun(format)
        device_function(
          filename = file,
          width = width_inches,
          height = height_inches,
          units = "in",
          res = dpi,
          bg = "white"
        )
      } else if (format == "svg") {
        svg(
          filename = file,
          width = width_inches,
          height = height_inches
        )
      } else if (format == "pdf") {
        pdf(
          file = file,
          width = width_inches,
          height = height_inches,
          onefile = FALSE
        )
      } else {
        stop("Unsupported download format: ", format)
      }

      current_device <- grDevices::dev.cur()
      if (current_device != previous_device) {
        on.exit(grDevices::dev.off(), add = TRUE)
      }

      print(plot_obj)

      dotplot_debug_log("DOTPLOT DOWNLOAD: Plot download completed", 1)
    }, error = function(e) {
      dotplot_debug_log(paste("DOTPLOT DOWNLOAD: Error during download:", e$message), 1)
      showNotification(paste("Error during download:", e$message), type = "error")
    })
  }
)

# ========================================
# Threshold Table Output
# ========================================

output$threshold_table <- DT::renderDataTable({
  if (length(dotplot_state$thresholds) == 0) {
    return(data.frame(
      Type = character(),
      Value = numeric(),
      Color = character(),
      Style = character(),
      Thickness = numeric(),
      Label = character(),
      `Label Size` = numeric(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  threshold_df <- do.call(rbind, lapply(dotplot_state$thresholds, function(t) {
    data.frame(
      Type = tools::toTitleCase(t$type),
      Value = t$value,
      Color = t$color,
      Style = tools::toTitleCase(t$style),
      Thickness = t$thickness %||% 1,
      Label = ifelse(nzchar(t$label), t$label, "-"),
      `Label Size` = t$label_size %||% 3,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))

  return(threshold_df)
}, options = list(pageLength = 5, dom = 't', scrollX = TRUE), selection = 'single')


observeEvent(input$add_to_grid, {
  dotplot_debug_log("Dotplot add_to_grid clicked", 1)

  # Validate plot availability
  if (is.null(dotplot_state$current_plot)) {
    showNotification("No plot available to add. Please generate a plot first.", type = "error")
    dotplot_debug_log("Dotplot: current_plot is NULL", 1)
    return()
  }

  # Get the current plot. Native/Grid rendering uses plotmath axis labels so
  # transformed-axis subscripts do not depend on Unicode font support.
  p <- tryCatch({

    dotplot_prepare_static_axis_labels(
      dotplot_state$current_plot
    )

  }, error = function(e) {
    dotplot_debug_log(paste("Dotplot: error accessing plot:", e$message), 1)
    NULL
  })

  # Validate plot object
  if (is.null(p)) {
    showNotification("No plot available to add.", type = "error")
    return()
  }

  if (!inherits(p, "ggplot")) {
    showNotification("Only ggplot objects can be added to the grid (Phase 1).", type = "error")
    dotplot_debug_log("Dotplot: current plot is not a ggplot", 1)
    return()
  }

  # Create plot ID and label following the same pattern as other modules
  sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
  lbl_raw <- input$grid_label
  lbl_id <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
  plot_id <- paste0(ns(""), "Dotplot_", lbl_id)

  # Visible label shown in the Grid tab
  lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "Dot Plot"

  # Add snapshot to the central grid selection
  dotplot_debug_log(paste("Adding dotplot to grid with id:", plot_id), 1)
  modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "Dotplot")
  showNotification("Added to grid selection.", type = "message")
})

# Observer to auto-recalculate ranges/ticks only when explicitly requested
observeEvent(dotplot_state$current_plot, {
  req(dotplot_state$current_plot)

  if (!isTRUE(auto_range_recalc_pending())) {
    return()
  }
  if (isTRUE(isolate(dotplot_state$restore_in_progress)) ||
      isTRUE(isolate(dotplot_state$plot_from_restore_cache))) {
    auto_range_recalc_pending(FALSE)
    dotplot_debug_log("Skipping auto range/tick recalculation for restored cached dotplot", 2)
    return()
  }

  tryCatch({
    # Prefer ranges from prepared dotplot data; only build ggplot as fallback.
    plot_ranges <- get_natural_axis_ranges(dotplot_state$current_plot)
    if (is.null(plot_ranges)) {
      plot_ranges <- list(x_range = c(-1, 1), y_range = c(-1, 1))
    }

    # Get transformation types for smart tick calculation
    x_transform <- dotplot_state$axis_config$x_transform %||% "raw"
    y_transform <- dotplot_state$axis_config$y_transform %||% "raw"

    # Calculate smart slider configurations with tick intervals
    x_slider_config <- expand_range_for_slider_with_ticks(plot_ranges$x_range, x_transform)
    y_slider_config <- expand_range_for_slider_with_ticks(plot_ranges$y_range, y_transform)

    # Update sliders
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

    # Also refresh tick defaults when auto-recalculation is explicitly requested.
    updateNumericInput(session, "x_tick_interval", value = x_slider_config$tick_interval)
    updateNumericInput(session, "y_tick_interval", value = y_slider_config$tick_interval)

    dotplot_debug_log("Auto range/tick recalculation applied", 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error in range update:", e$message), 1)
  }, finally = {
    auto_range_recalc_pending(FALSE)
  })
}, priority = 2)

# Text size changes trigger IMMEDIATE plot update
observeEvent(c(input$title_size, input$axis_title_size, input$tick_size), {
  req(dotplot_state$plot_ready)

  dotplot_debug_log("Text size settings changed; triggering plot update", 1)
  plot_update_trigger(plot_update_trigger() + 1)
}, ignoreInit = TRUE)

observeEvent(c(input$plot_title, input$hide_title, input$theme_select), {
  req(dotplot_state$plot_ready)

  if (isTRUE(isolate(dotplot_state$preset_update_in_progress))) {
    return()
  }

  dotplot_debug_log("Plot title visibility/theme changed; triggering plot update", 1)
  plot_update_trigger(plot_update_trigger() + 1)
}, ignoreInit = TRUE)

# Tick interval changes only update plot, never reset intervals
observeEvent(input$x_tick_interval, {
  req(dotplot_state$plot_ready)
  dotplot_debug_log(paste("User changed X tick interval to:", input$x_tick_interval), 1)
  plot_update_trigger(plot_update_trigger() + 1)
}, ignoreInit = TRUE)

observeEvent(input$y_tick_interval, {
  req(dotplot_state$plot_ready)
  dotplot_debug_log(paste("User changed Y tick interval to:", input$y_tick_interval), 1)
  plot_update_trigger(plot_update_trigger() + 1)
}, ignoreInit = TRUE)

# ========================================
# Plot Update Trigger Observer - MISSING PIECE!
# ========================================

observeEvent(plot_update_trigger(), {
  req(plot_update_trigger() > 0,
      dotplot_state$plot_ready,
      data_in(),
      data_def_in())

  # While a session restore is still settling, the captured theme/title/
  # tick widgets are being pushed back into the UI via update*Input().
  # Their resulting input changes bump plot_update_trigger() and would
  # regenerate the plot here using whatever widget values have arrived
  # so far -- replacing the authoritative restored ggplot with one built
  # from partially-synced UI state (and losing per-region colouring in
  # the process).  Short-circuit while the guard is raised.
  if (isTRUE(isolate(dotplot_state$restore_in_progress))) {
    dotplot_debug_log("Skipping plot_update_trigger regeneration during session restore", 2)
    return()
  }
  if (isTRUE(isolate(dotplot_state$plot_from_restore_cache))) {
    dotplot_debug_log("Skipping live plot regeneration while restored cache is authoritative", 1)
    return()
  }
  if (isTRUE(isolate(is_active_dataset_mismatch()))) {
    dotplot_debug_log("Skipping live plot regeneration: active dataset differs from plot source dataset", 1)
    return()
  }

  dotplot_debug_log("Plot update triggered with current parameters", 1)

  tryCatch({
    validation <- dotplot_validate_config(dotplot_state$axis_config, data_def_in(), data_in())
    if (!isTRUE(validation$valid)) {
      dotplot_debug_log(paste("Skipping live update due to invalid axis configuration:", validation$message), 1)
      showNotification(validation$message, type = "warning", duration = 3)
      return()
    }

    # Get CURRENT UI parameters
    current_plot_params <- dotplot_extract_plot_parameters(input)
    dotplot_state$plot_request <- current_plot_params

    dotplot_debug_log(paste("Update parameters - title size:", current_plot_params$title_size,
                    "axis title size:", current_plot_params$axis_title_size,
                    "tick size:", current_plot_params$tick_size), 2)
    dotplot_debug_log(paste("Update tick intervals - X:", current_plot_params$x_tick_interval,
                    "Y:", current_plot_params$y_tick_interval), 2)

    # Create UI labels
    ui_labels <- list(
      x_label = input$x_axis_label,
      y_label = input$y_axis_label
    )

    # Re-generate plot with current parameters
    if (exists("dotplot_create_complete_plot_with_regions_enhanced")) {
      updated_plot <- dotplot_create_complete_plot_with_regions_enhanced(
        data = data_in(),
        data_def = data_def_in(),
        axis_config = dotplot_state$axis_config,
        thresholds = dotplot_state$thresholds,
        color_rules = dotplot_state$color_rules,
        plot_params = current_plot_params,  # Use CURRENT parameters
        region_configs = region_configs(),
        region_structure = region_structure(),
        ui_labels = ui_labels
      )
    } else {
      updated_plot <- dotplot_create_complete_plot_with_regions(
        data = data_in(),
        data_def = data_def_in(),
        axis_config = dotplot_state$axis_config,
        thresholds = dotplot_state$thresholds,
        color_rules = dotplot_state$color_rules,
        plot_params = current_plot_params,  # Use CURRENT parameters
        region_configs = region_configs(),
        region_structure = region_structure()
      )
    }

    if (!is.null(updated_plot)) {
      store_natural_axis_ranges(updated_plot)
      dotplot_state$current_plot <- updated_plot
      # Keep the base (unlabelled) plot in sync with the freshly regenerated
      # plot so the dynamic-label overlay in get_current_display_dotplot()
      # always layers labels on top of the current theme/title/tick settings.
      # Without this, a theme tweak after applySettings_dot would render the
      # stale pre-tweak base with labels on top and visibly "lose" the tweak.
      dotplot_state$base_plot_without_labels <- updated_plot
      dotplot_state$plot_data_cache_ref <- dotplot_build_cache_key()
      dotplot_debug_log("Plot successfully updated with current parameters", 1)
    } else {
      dotplot_debug_log("Plot update returned NULL", 1)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error in plot update:", e$message), 1)
  })

}, ignoreInit = TRUE)


  # ==========================================================================
  # Session restore: fallback plot regeneration
  # ==========================================================================
  # After a session restore, the ggplot object is restored directly via
  # set_session_state().  If the ggplot object could not be deserialized
  # (e.g. NULL), this observer falls back to regenerating the plot from
  # the restored axis config, thresholds, and color rules.
  # Helper: push captured UI widget values back into the live inputs after
  # a session restore.  Deferred via session$onFlushed so that the widgets
  # have been rendered on the client before update*Input() fires.  Also
  # retriggers region UI selection so restored region_configs propagate to
  # colour pickers / numeric inputs.
  # Schedule the restore guard to clear after the widget-echo cascade
  # has had two full reactive flush cycles to settle.  Nested
  # session$onFlushed defers the clear until after:
  #   flush N   -- this update*Input / selected_region toggle batch runs
  #   flush N+1 -- client message sent; observers re-evaluate on echo
  #   flush N+2 -- any follow-up cascades complete; safe to clear
  # The restore_in_progress flag is checked at the start of every
  # plot-regenerating observer, so clearing it late is safe (no plot
  # regeneration can happen before it clears, only after).
  schedule_restore_guard_clear <- function() {
    session$onFlushed(function() {
      session$onFlushed(function() {
        if (isTRUE(isolate(dotplot_state$restore_in_progress))) {
          dotplot_state$restore_in_progress <- FALSE
          dotplot_debug_log(
            "[Dotplot] session restore guard cleared; live refreshes re-enabled", 1)
        }
      }, once = TRUE)
    }, once = TRUE)
  }

  register_dotplot_restore_job <- function(reason, phase = "ui-sync") {
    api <- session$userData$restore_jobs
    if (!is.list(api) || !is.function(api$register_restore_job)) return(NULL)
    tryCatch(api$register_restore_job("Dotplot", reason, phase, timeout = 15),
             error = function(e) NULL)
  }
  resolve_dotplot_restore_job <- function(job_id, outcome = "completed", error = NULL) {
    api <- session$userData$restore_jobs
    if (is.null(job_id) || !is.list(api) || !is.function(api$resolve_restore_job)) return(FALSE)
    isTRUE(api$resolve_restore_job(job_id, outcome, error))
  }
  schedule_dotplot_restore_flush <- function(reason, callback, phase = "ui-sync") {
    job_id <- register_dotplot_restore_job(reason, phase)
    session$onFlushed(function() {
      outcome <- "completed"
      problem <- NULL
      on.exit(resolve_dotplot_restore_job(job_id, outcome, problem), add = TRUE)
      tryCatch(callback(), error = function(e) {
        outcome <<- "error"
        problem <<- e$message
        dotplot_debug_log(paste("[Dotplot]", reason, "failed:", e$message), 1)
      })
    }, once = TRUE)
    invisible(job_id)
  }

  sync_dotplot_ui_from_state <- function() {
    # Always schedule the restore guard release up front -- even if no
    # UI values were captured or the update loop below errors, the
    # reactive graph must eventually return to normal operation.
    schedule_restore_guard_clear()

    captured <- if (!is.null(pending_ui_inputs)) {
      tryCatch(pending_ui_inputs(), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(captured) || !is.list(captured) || length(captured) == 0L) {
      return(invisible())
    }
    apply_captured_inputs <- function(captured_vals) {
      numeric_input_ids <- c(
        "title_size", "axis_title_size", "tick_size",
        "x_tick_interval", "y_tick_interval",
        "region_point_size", "region_point_alpha",
        "maxOverlaps_dot", "labelDistance_dot",
        "lineThickness_dot", "labelSize_dot", "dotSizeLabeled_dot"
      )
      text_input_ids <- c("x_axis_label", "y_axis_label", "plot_title")
      for (id in intersect(names(captured_vals), dotplot_ui_input_ids)) {
        val <- captured_vals[[id]]
        if (is.null(val)) next
        if (id %in% c("x_axis_range", "y_axis_range")) {
          # Axis sliders are restored together with explicit bounds in
          # restore_axis_slider_bounds(); skipping generic replay here avoids
          # transient out-of-range warnings when the saved value is replayed
          # before min/max are restored.
          next
        }
        if (is.numeric(val) && length(val) == 2L) {
          # Paired numeric ⇒ sliderInput range value.
          updateSliderInput(session, id, value = val)
        } else if (is.numeric(val)) {
          # Could be either a slider or a numericInput; updateNumericInput
          # is the safe default (slider accepts the same call shape).
          updateNumericInput(session, id, value = val)
        } else if (is.logical(val)) {
          updateCheckboxInput(session, id, value = val)
        } else if (is.character(val)) {
          if (grepl("^#[0-9A-Fa-f]{6,8}$", val) &&
              requireNamespace("colourpicker", quietly = TRUE)) {
            tryCatch(
              colourpicker::updateColourInput(session, id, value = val),
              error = function(e) updateTextInput(session, id, value = val)
            )
          } else if (id %in% numeric_input_ids) {
            updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
          } else if (id %in% c("x_transform", "y_transform")) {
            updateRadioButtons(session, id, selected = val)
          } else if (grepl("_column$|_select$|_shape$|Column_dot$", id)) {
            updateSelectInput(session, id, selected = val)
          } else if (id %in% text_input_ids) {
            updateTextInput(session, id, value = val)
          } else {
            updateTextInput(session, id, value = val)
          }
        }
      }
    }

    restore_axis_slider_bounds <- function(captured_vals) {
      cached_pair <- tryCatch(isolate(dotplot_state$restore_plot_data_cache), error = function(e) NULL)
      cached_data <- if (is.list(cached_pair) && is.data.frame(cached_pair$data_mod)) cached_pair$data_mod else NULL
      x_col <- captured_vals$x_axis_column %||% (dotplot_state$axis_config$x_col %||% NULL)
      y_col <- captured_vals$y_axis_column %||% (dotplot_state$axis_config$y_col %||% NULL)
      x_transform <- captured_vals$x_transform %||% (dotplot_state$axis_config$x_transform %||% "raw")
      y_transform <- captured_vals$y_transform %||% (dotplot_state$axis_config$y_transform %||% "raw")

      valid_slider_cfg <- function(cfg) {
        is.list(cfg) &&
          is.numeric(cfg$min) && length(cfg$min) == 1L && is.finite(cfg$min) &&
          is.numeric(cfg$max) && length(cfg$max) == 1L && is.finite(cfg$max)
      }
      x_cfg <- if (valid_slider_cfg(captured_vals$x_axis_slider_state)) captured_vals$x_axis_slider_state else NULL
      y_cfg <- if (valid_slider_cfg(captured_vals$y_axis_slider_state)) captured_vals$y_axis_slider_state else NULL
      ranges <- NULL
      if (is.numeric(captured_vals$natural_x_range) && length(captured_vals$natural_x_range) == 2L &&
          all(is.finite(captured_vals$natural_x_range)) &&
          is.numeric(captured_vals$natural_y_range) && length(captured_vals$natural_y_range) == 2L &&
          all(is.finite(captured_vals$natural_y_range))) {
        ranges <- list(x_range = captured_vals$natural_x_range, y_range = captured_vals$natural_y_range)
      }

      # Restore-exclusive preferred path: use the saved slider bounds/ranges
      # exactly as captured. Only derive ranges from cached data when older
      # sessions lack those fields.
      if ((is.null(x_cfg) || is.null(y_cfg)) &&
          is.data.frame(cached_data) && is.character(x_col) && nzchar(x_col) &&
          is.character(y_col) && nzchar(y_col)) {
        dotplot_debug_log(paste(
          "[Dotplot][restore ranges] cached-data path",
          "x_col=", x_col, "y_col=", y_col,
          "x_transform=", x_transform, "y_transform=", y_transform
        ), 2)
        x_range_cached <- tryCatch(
          calculate_conservative_axis_range(cached_data, x_col, x_transform),
          error = function(e) NULL
        )
        y_range_cached <- tryCatch(
          calculate_conservative_axis_range(cached_data, y_col, y_transform),
          error = function(e) NULL
        )
        if (is.numeric(x_range_cached) && length(x_range_cached) == 2L &&
            all(is.finite(x_range_cached)) &&
            is.numeric(y_range_cached) && length(y_range_cached) == 2L &&
            all(is.finite(y_range_cached))) {
          ranges <- list(x_range = x_range_cached, y_range = y_range_cached)
          x_cfg <- expand_range_for_slider_with_ticks(x_range_cached, x_transform)
          y_cfg <- expand_range_for_slider_with_ticks(y_range_cached, y_transform)
          dotplot_debug_log(paste(
            "[Dotplot][restore ranges] cached-data ranges",
            "X=", paste(round(x_range_cached, 4), collapse = " to "),
            "Y=", paste(round(y_range_cached, 4), collapse = " to ")
          ), 2)
        } else {
          dotplot_debug_log("[Dotplot][restore ranges] cached-data range derivation failed; falling back", 2)
        }
      } else {
        dotplot_debug_log("[Dotplot][restore ranges] cached-data path unavailable; falling back", 2)
      }

      if (is.null(x_cfg) || is.null(y_cfg)) {
        p <- tryCatch(isolate(dotplot_state$current_plot), error = function(e) NULL)
        ranges <- get_natural_axis_ranges(p)
        if (!is.null(ranges) && !is.null(ranges$x_range) && !is.null(ranges$y_range)) {
          x_cfg <- expand_range_for_slider_with_ticks(ranges$x_range, x_transform)
          y_cfg <- expand_range_for_slider_with_ticks(ranges$y_range, y_transform)
          dotplot_debug_log("[Dotplot][restore ranges] using current_plot extracted ranges", 2)
        }
      }

      if (is.null(x_cfg) || is.null(y_cfg)) return(FALSE)

      x_value <- captured_vals$x_axis_range %||% if (!is.null(ranges$x_range)) ranges$x_range else NULL
      y_value <- captured_vals$y_axis_range %||% if (!is.null(ranges$y_range)) ranges$y_range else NULL
      if (!is.numeric(x_value) || length(x_value) != 2L || any(!is.finite(x_value))) x_value <- c(x_cfg$min, x_cfg$max)
      if (!is.numeric(y_value) || length(y_value) != 2L || any(!is.finite(y_value))) y_value <- c(y_cfg$min, y_cfg$max)

      clamp_slider_value <- function(value, min_val, max_val) {
        lo <- min(min_val, max_val)
        hi <- max(min_val, max_val)
        clamped <- pmin(pmax(as.numeric(value), lo), hi)
        sort(clamped)
      }
      x_value <- clamp_slider_value(x_value, x_cfg$min, x_cfg$max)
      y_value <- clamp_slider_value(y_value, y_cfg$min, y_cfg$max)

      x_decimals <- x_cfg$decimals %||% 3
      y_decimals <- y_cfg$decimals %||% 3
      mark_programmatic_range_update("x_axis_range")
      updateSliderInput(session, "x_axis_range",
                        min = round(x_cfg$min, x_decimals),
                        max = round(x_cfg$max, x_decimals),
                        value = round(x_value, x_decimals),
                        step = x_cfg$step %||% 0.1)
      mark_programmatic_range_update("y_axis_range")
      updateSliderInput(session, "y_axis_range",
                        min = round(y_cfg$min, y_decimals),
                        max = round(y_cfg$max, y_decimals),
                        value = round(y_value, y_decimals),
                        step = y_cfg$step %||% 0.1)

      if (!is.null(captured_vals$x_tick_interval) && is.finite(captured_vals$x_tick_interval)) {
        updateNumericInput(session, "x_tick_interval", value = captured_vals$x_tick_interval)
      } else {
        updateNumericInput(session, "x_tick_interval", value = x_cfg$tick_interval)
      }
      if (!is.null(captured_vals$y_tick_interval) && is.finite(captured_vals$y_tick_interval)) {
        updateNumericInput(session, "y_tick_interval", value = captured_vals$y_tick_interval)
      } else {
        updateNumericInput(session, "y_tick_interval", value = y_cfg$tick_interval)
      }
      TRUE
    }

    run_sync_pass <- function(pass_no = 1L) {
      tryCatch({
        apply_captured_inputs(captured)
        slider_ok <- isTRUE(restore_axis_slider_bounds(captured))

        # Re-apply after one additional flush so selectInput targets are
        # preserved even if their choices are rebuilt asynchronously from
        # restored data_def.
        schedule_dotplot_restore_flush("second-pass UI synchronization", function() {
          isolate(tryCatch({
            apply_captured_inputs(captured)
            slider_ok_2 <- isTRUE(restore_axis_slider_bounds(captured))

            if ((!slider_ok || !slider_ok_2) && pass_no < 4L) {
              schedule_dotplot_restore_flush(
                sprintf("UI synchronization retry pass %d", pass_no + 1L),
                function() isolate(run_sync_pass(pass_no + 1L))
              )
              return(invisible())
            }

            # Nudge region UI to re-render using restored region_configs.
            if (!is.null(selected_region)) {
              cur_sel <- tryCatch(isolate(selected_region()), error = function(e) NULL)
              if (!is.null(cur_sel)) {
                selected_region(NULL)
                selected_region(cur_sel)
              }
            }
            dotplot_debug_log("[Dotplot] session restore: UI widgets synced from captured state", 1)
            pending_ui_inputs(NULL)
          }, error = function(e) {
            dotplot_debug_log(paste("[Dotplot] second-pass UI sync failed:", e$message), 1)
            pending_ui_inputs(NULL)
          }))
        })
      }, error = function(e) {
        dotplot_debug_log(paste("[Dotplot] UI sync failed:", e$message), 1)
        pending_ui_inputs(NULL)
      })
    }

    schedule_dotplot_restore_flush(
      "first-pass UI synchronization",
      function() isolate(run_sync_pass(1L))
    )
  }

  # Regenerate the dotplot from the restored config + region_configs.  Used
  # both as the immediate fallback path when the deserialized ggplot is
  # NULL, and as a final "authoritative rerender" step scheduled after the
  # restore guard cascade has settled -- thresholds have propagated into
  # region_structure, region_configs has been preserved by the remap guard,
  # and the UI widgets have echoed back.  At that point rebuilding the
  # ggplot guarantees the on-screen plot reflects the per-region styling
  # even if the serialized ggplot failed to carry the scale_identity
  # colour/size/alpha/shape vectors through saveRDS.
  capture_dotplot_restore_snapshot <- function() isolate({
    cached_pair <- dotplot_state$restore_plot_data_cache
    use_cache <- is.list(cached_pair) && is.data.frame(cached_pair$data_mod) &&
      is.data.frame(cached_pair$data_def)
    allow_live <- isTRUE(dotplot_state$restore_live_fallback_available)
    list(
      contract = "dotplot-restore-snapshot-v1",
      cached_pair = cached_pair,
      data = if (use_cache) cached_pair$data_mod else if (allow_live) tryCatch(data_in(), error = function(e) NULL) else NULL,
      data_def = if (use_cache) cached_pair$data_def else if (allow_live) tryCatch(data_def_in(), error = function(e) NULL) else NULL,
      axis_config = dotplot_state$axis_config %||% list(),
      plot_request_axis = tryCatch(dotplot_state$plot_request$axis_config, error = function(e) NULL),
      plot_ui_cache = dotplot_state$plot_ui_cache,
      thresholds = dotplot_state$thresholds,
      color_rules = dotplot_state$color_rules,
      region_configs = region_configs(),
      region_structure = region_structure(),
      input_values = reactiveValuesToList(input),
      restore_in_progress = isTRUE(dotplot_state$restore_in_progress),
      plot_from_restore_cache = use_cache
    )
  })

  regenerate_dotplot_from_restored_state <- function(snapshot, reason = "session restore") {
    if (!is.list(snapshot) || !identical(snapshot$contract, "dotplot-restore-snapshot-v1")) {
      stop("regenerate_dotplot_from_restored_state requires a captured restore snapshot", call. = FALSE)
    }
    data <- snapshot$data
    data_def <- snapshot$data_def
    if (is.null(data) || is.null(data_def)) return(invisible(FALSE))
    axis_config <- snapshot$axis_config

    replay_ui_for_axis <- snapshot$plot_ui_cache
    request_axis <- snapshot$plot_request_axis
    if (is.list(request_axis)) {
      axis_config <- utils::modifyList(request_axis, axis_config, keep.null = TRUE)
    }
    if (is.list(replay_ui_for_axis)) {
      axis_config$x_col <- axis_config$x_col %||% replay_ui_for_axis$x_axis_column
      axis_config$y_col <- axis_config$y_col %||% replay_ui_for_axis$y_axis_column
      axis_config$x_transform <- axis_config$x_transform %||% replay_ui_for_axis$x_transform %||% "raw"
      axis_config$y_transform <- axis_config$y_transform %||% replay_ui_for_axis$y_transform %||% "raw"
      axis_config$x_label <- axis_config$x_label %||% replay_ui_for_axis$x_axis_label
      axis_config$y_label <- axis_config$y_label %||% replay_ui_for_axis$y_axis_label
    }

    validation <- tryCatch(
      dotplot_validate_config(axis_config, data_def, data),
      error = function(e) list(valid = FALSE, message = e$message)
    )
    if (!isTRUE(validation$valid)) return(invisible(FALSE))

    replay_ui <- snapshot$plot_ui_cache
    dot_input <- if (is.list(replay_ui)) utils::modifyList(snapshot$input_values, replay_ui, keep.null = TRUE) else snapshot$input_values
    plot_params <- tryCatch(dotplot_extract_plot_parameters(dot_input), error = function(e) NULL)
    if (is.null(plot_params)) return(invisible(FALSE))

    ui_labels <- list(
      x_label = dot_input$x_axis_label %||% "",
      y_label = dot_input$y_axis_label %||% ""
    )

    plot_result <- tryCatch({
      if (exists("dotplot_create_complete_plot_with_regions_enhanced")) {
        dotplot_create_complete_plot_with_regions_enhanced(
          data = data, data_def = data_def,
          axis_config = axis_config,
          thresholds = snapshot$thresholds,
          color_rules = snapshot$color_rules,
          plot_params = plot_params,
          region_configs = snapshot$region_configs,
          region_structure = snapshot$region_structure,
          ui_labels = ui_labels
        )
      } else {
        dotplot_create_complete_plot_with_regions(
          data = data, data_def = data_def,
          axis_config = axis_config,
          thresholds = snapshot$thresholds,
          color_rules = snapshot$color_rules,
          plot_params = plot_params,
          region_configs = snapshot$region_configs,
          region_structure = snapshot$region_structure
        )
      }
    }, error = function(e) {
      dotplot_debug_log(paste("[Dotplot]", reason, "regeneration failed:", e$message), 1)
      NULL
    })

    if (!is.null(plot_result)) {
      store_natural_axis_ranges(plot_result)
      dotplot_state$axis_config <- axis_config
      dotplot_state$current_plot <- plot_result
      dotplot_state$base_plot_without_labels <- plot_result
      dotplot_state$plot_ready <- TRUE
      dotplot_state$plot_creation_cache <- list(data_mod = data, data_def = data_def)
      dotplot_state$source_data_signature <- if (is.data.frame(data) && is.data.frame(data_def)) {
        paste0(nrow(data), "x", ncol(data), "::", nrow(data_def), "x", ncol(data_def), "::",
               paste(colnames(data), collapse = "|"), "::", paste(colnames(data_def), collapse = "|"))
      } else NULL
      if (isTRUE(snapshot$restore_in_progress) && is.list(snapshot$plot_ui_cache)) {
        # During restore, keep the captured UI snapshot authoritative;
        # live input echoes can still be transient/default at this point.
      } else dotplot_state$plot_ui_cache <- snapshot$plot_ui_cache
      dotplot_state$plot_cache_ref_by_title <- stats::setNames(
        list(dotplot_state$source_data_signature %||% ""),
        as.character(dot_input$plot_title)[1]
      )
      dotplot_state$plot_data_cache_ref <- dotplot_build_cache_key()
      dotplot_state$plot_from_restore_cache <- snapshot$plot_from_restore_cache
      if (isTRUE(snapshot$restore_in_progress)) {
        # The rebuilt plot, plot_creation_cache, UI cache, labels and canonical
        # reference are complete. Drop only the transient restore aliases.
        dotplot_state$restore_plot_data_cache <- NULL
      }
      dotplot_debug_log(paste("[Dotplot]", reason, "plot (re)built with restored region_configs"), 1)
      return(invisible(TRUE))
    }
    invisible(FALSE)
  }

  observeEvent(rv$session_restore_trigger, {
    tryCatch({
      # Always push restored UI values back into the widgets -- the plot
      # may already be restored but the sliders/colour pickers default to
      # factory values until this sync runs.
      sync_dotplot_ui_from_state()

      # The module setter is authoritative for whether the saved session had a
      # plot. UI restoration is harmless, but everything below creates plot
      # state or deferred render work.
      if (!isTRUE(isolate(dotplot_state$restore_rebuild_requested))) {
        dotplot_debug_log(
          "[Dotplot] session restore: UI synchronized; no plot rebuild requested", 1)
        return(invisible(NULL))
      }

      # Immediate fallback: if the deserialized ggplot is NULL, regenerate
      # now so the first render has something to show.  The remap guard in
      # dotplot_server_regions.R keeps region_configs intact even at this
      # early point, so the regenerated plot already carries per-region
      # styling.
      if (is.null(dotplot_state$current_plot)) {
        dotplot_debug_log("[Dotplot] session restore: regenerating plot from restored config", 1)
        regenerate_dotplot_from_restored_state(
          capture_dotplot_restore_snapshot(), reason = "session restore fallback")
      } else {
        dotplot_debug_log("[Dotplot] session restore: ggplot object present", 1)
      }

      # Authoritative rerender: after the sync cascade has run (widgets
      # echoed, thresholds propagated into region_structure, region_configs
      # preserved by the remap guard), rebuild the ggplot from the current
      # reactive state.  This guarantees that per-region styling --
      # especially the scale_identity colour/size/alpha/shape mappings
      # derived from region_configs on top of the threshold-defined grid --
      # is actually present on the rendered plot.  The user's intuition is
      # correct: thresholds need to exist before per-region styling can be
      # applied, so we explicitly wait for that propagation via nested
      # session$onFlushed calls (same cadence the guard clear uses) before
      # the final rerender.
      #
      # After the rerender (or if no rerender was needed), re-apply saved
      # dot labels from dot_protein_labels onto the current plot.  The
      # label data carries pre-computed x/y coordinates and style info so
      # it does not depend on the identifier column UI being set yet.
      session$onFlushed(function() {
        schedule_dotplot_restore_flush("authoritative rerender", function() {
          reconstruction_succeeded <- FALSE
          isolate(tryCatch({
            snapshot <- capture_dotplot_restore_snapshot()
            reconstruction_succeeded <- isTRUE(regenerate_dotplot_from_restored_state(
              snapshot, reason = "session restore authoritative rerender"))
          }, error = function(e) {
            dotplot_debug_log(
              paste("[Dotplot] authoritative rerender failed:", e$message), 1)
          }))

          # Re-apply labels from the restored dot_protein_labels onto the
          # (possibly regenerated) base plot.  apply_labels_to_dot_plot_enhanced_FIXED
          # reads cosmetic settings from input (maxOverlaps, labelDistance, …)
          # which are available by this point thanks to sync_dotplot_ui_from_state.
          isolate(tryCatch({
            labels <- if (!is.null(dot_protein_labels)) {
              isolate(dot_protein_labels())
            } else {
              NULL
            }
            if (!is.null(labels) && is.data.frame(labels) && nrow(labels) > 0) {
              base <- isolate(dotplot_state$base_plot_without_labels)
              if (!is.null(base)) {
                labeled_plot <- apply_labels_to_dot_plot_enhanced_FIXED(
                  base, labels, input, dotplot_debug_log)
                dotplot_state$current_plot <- labeled_plot
                dotplot_debug_log(paste(
                  "[Dotplot] session restore: re-applied", nrow(labels), "labels"), 1)
              }
            }
          }, error = function(e) {
            dotplot_debug_log(
              paste("[Dotplot] label re-application failed:", e$message), 1)
          }))

          # Notifications describe the final reconstruction outcome, never the
          # preliminary state-staging pass. A resolved saved cache is silent;
          # otherwise emit exactly one warning for this restore generation.
          isolate(if (isTRUE(dotplot_state$restore_rebuild_requested) &&
                      !isTRUE(dotplot_state$restore_notification_emitted)) {
            if (!isTRUE(dotplot_state$restore_cache_resolved)) {
              if (isTRUE(dotplot_state$restore_live_fallback_available) &&
                  isTRUE(reconstruction_succeeded)) {
                showNotification(
                  "Dotplot restored using current dataset (cached plot data unavailable).",
                  type = "warning", duration = 6
                )
              } else {
                showNotification(
                  paste(
                    "Dotplot could not be rebuilt because neither cached plot data",
                    "nor compatible current dataset data could reconstruct it."
                  ),
                  type = "warning", duration = 6
                )
              }
            }
            dotplot_state$restore_notification_emitted <- TRUE
          })
        }, phase = "authoritative-rerender")
      }, once = TRUE)
    }, error = function(e) {
      dotplot_debug_log(paste("[Dotplot] session restore fallback failed:", e$message), 1)
    })
  }, ignoreInit = TRUE)

  invisible(NULL)
}
