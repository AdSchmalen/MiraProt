# ==============================================================================
# File: modules/PCA/pca_module_interactive.R
#
# Purpose:
#   Provides create_pca_interactive_plot(), which converts the static ggplot2
#   scatter plot to an interactive plotly object. Mirrors the visual output of
#   create_static_plot() while adding hover tooltips and event source tagging
#   required for plotly selection callbacks.
#
# Architectural Role:
#   Pure conversion function with no Shiny dependency and no reactive state.
#   The output plotly object is consumed by register_pca_rendering_core in
#   pca_module_server_pipeline.R, where it is rendered via renderPlotly and
#   where the plotly_selected / plotly_click event handlers are registered.
#
# Structure:
#   1. create_pca_interactive_plot(results, plot_params, theme_name,
#                                  font_sizes, enhanced_labeling,
#                                  legend_position, debug_log)
#      - Creates the base ggplot2 plot via create_static_plot()
#      - Converts to plotly with ggplotly()
#      - Tags the source as "pca_plot" for event routing
#
# Notes for future developers:
#   - The plotly source tag "pca_plot" is referenced by the selection observers
#     in register_pca_rendering_core. If the source tag changes here it must
#     also change there.
#   - debug_log is passed explicitly and must not be removed from the signature.
# ==============================================================================

#' Create interactive dimension reduction plot - FINAL CORRECTED VERSION
#' This version exactly matches the static plot behavior, including theme support
#' @param results Analysis results from dimension reduction
#' @param plot_params List of plot parameters
#' @param font_sizes List of font sizes for different elements
#' @param theme_name Name of ggplot2 theme to use (now properly supported)
#' @param legend_position Legend position ("right", "left", "top", "bottom", "none")
#' @return plotly object
create_pca_interactive_plot <- function(results, plot_params, font_sizes = NULL, theme_name = "theme_minimal", legend_position = "right") {

  debug_log <- plot_params$debug_log %||% function(msg, level) cat("DEBUG:", msg, "\n")
  debug_log("=== INTERACTIVE PLOT CREATION START ===", 2)

  # Extract parameters (same as static plot)
  axis_x <- plot_params$axis_x %||% "PC1"
  axis_y <- plot_params$axis_y %||% "PC2"
  point_size <- plot_params$point_size %||% 3
  color_palette <- plot_params$color_palette %||% "Set1"
  reverse_colors <- plot_params$reverse_colors %||% FALSE

  # Extract font sizes with defaults (same as static plot)
  axis_title_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$axis_title) else 20
  tick_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$tick) else 16
  legend_title_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$legend_title) else 18
  legend_text_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$legend_text) else 14

  debug_log(paste("Interactive plot parameters - Axis:", axis_x, "vs", axis_y, "| Point size:", point_size), 2)

  # Get plot data (same as static plot)
  plot_data <- create_plot_data(results, results$raw_metadata %||% results$metadata, plot_params$identifier_col, debug_log = debug_log)
  debug_log(paste("Interactive plot data created with", nrow(plot_data), "points"), 2)

  # Get coordinates for selected axes (same as static plot)
  coords <- get_plot_coordinates(results, axis_x, axis_y)
  plot_data$x <- coords$x
  plot_data$y <- coords$y

  # Get axis labels (same as static plot)
  axis_labels <- get_axis_labels(results, axis_x, axis_y, debug_log = debug_log)

  # Create hover text
  if ("Condition" %in% colnames(plot_data) && !all(is.na(plot_data$Condition))) {
    hover_text <- paste0(
      "<b>", plot_data$Name, "</b><br>",
      "Condition: ", plot_data$Condition, "<br>",
      axis_x, ": ", round(plot_data$x, 3), "<br>",
      axis_y, ": ", round(plot_data$y, 3)
    )
  } else {
    hover_text <- paste0(
      "<b>", plot_data$Name, "</b><br>",
      axis_x, ": ", round(plot_data$x, 3), "<br>",
      axis_y, ": ", round(plot_data$y, 3)
    )
  }
  plot_data$hover_text <- hover_text

  # Base ggplot scaffold (same as static plot)
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    labs(x = axis_labels$x, y = axis_labels$y)

  # SAMPLE MODE with conditions - exact same logic as static plot
  if ("Condition" %in% colnames(plot_data) && results$comparison_target == "samples") {

    unique_conditions <- unique(plot_data$Condition[!is.na(plot_data$Condition)])
    n_conditions <- length(unique_conditions)
    debug_log(paste("Sample mode detected with", n_conditions, "conditions"), 2)

    if (n_conditions > 0) {
      # Build palette - EXACT SAME LOGIC AS STATIC PLOT
      if (color_palette %in% c("Viridis","Plasma","Inferno","Magma","Cividis")) {
        plot_colors <- viridis::viridis(
          n_conditions,
          option = tolower(color_palette),
          begin = 0.05,
          end   = 0.95,
          alpha = 1
        )
        if (reverse_colors) plot_colors <- rev(plot_colors)
      } else {
        plot_colors <- tryCatch({
          min_needed <- max(n_conditions, 3)
          base_cols  <- RColorBrewer::brewer.pal(min_needed, color_palette)
          cols <- base_cols[1:n_conditions]
          if (reverse_colors) cols <- rev(cols)
          cols
        }, error = function(e) {
          debug_log(paste("Error with RColorBrewer palette:", e$message), 1)
          tmp <- rainbow(n_conditions)
          if (reverse_colors) tmp <- rev(tmp)
          tmp
        })
      }
      names(plot_colors) <- unique_conditions

      # Add points with conditions (no polygons for plotly compatibility)
      p <- p +
        geom_point(aes(color = Condition), size = point_size) +
        scale_color_manual(values = plot_colors)

      debug_log(paste("Applied colors for", n_conditions, "conditions"), 2)
    } else {
      # No valid conditions
      p <- p + geom_point(size = point_size, color = "steelblue")
    }
  } else {
    # PROTEIN MODE or no conditions - single color
    p <- p + geom_point(size = point_size, color = "steelblue")
  }

  # Apply theme - EXACT SAME LOGIC AS STATIC PLOT
  theme_func <- switch(theme_name,
                       "theme_gray"     = theme_gray(),
                       "theme_bw"       = theme_bw(),
                       "theme_linedraw" = theme_linedraw(),
                       "theme_light"    = theme_light(),
                       "theme_dark"     = theme_dark(),
                       "theme_minimal"  = theme_minimal(),
                       "theme_classic"  = theme_classic(),
                       "theme_void"     = theme_void(),
                       theme_minimal())

  # Apply legend position and direction
  legend_direction <- if (legend_position %in% c("top", "bottom")) "horizontal" else "vertical"

  p <- p + theme_func +
    theme(
      axis.title  = element_text(size = axis_title_size),
      axis.text.x = element_text(size = tick_size),
      axis.text.y = element_text(size = tick_size),
      legend.title = element_text(size = legend_title_size),
      legend.text  = element_text(size = legend_text_size),
      legend.position = legend_position,
      legend.direction = legend_direction
    )

  # Convert to plotly
  p_plotly <- plotly::ggplotly(p, tooltip = "text")

  # Add custom hover text at plotly trace level to avoid ggplot unknown-aesthetic warnings.
  # ggplotly splits colored point layers into separate traces, so trace-local point
  # numbers do not necessarily match row positions in plot_data. Match each marker
  # back to the displayed x/y coordinates so tooltips always describe the visible
  # point and the currently selected axes.
  if (!is.null(plot_data$hover_text)) {
    used_rows <- rep(FALSE, nrow(plot_data))
    for (i in seq_along(p_plotly$x$data)) {
      trace <- p_plotly$x$data[[i]]
      trace_mode <- trace$mode %||% ""
      is_marker_trace <- grepl("markers", trace_mode, fixed = TRUE)
      n_points <- length(trace$x %||% numeric(0))

      if (!is_marker_trace || n_points == 0 || is.null(trace$y)) next

      trace_text <- rep(NA_character_, n_points)
      for (j in seq_len(n_points)) {
        x_val <- suppressWarnings(as.numeric(trace$x[j]))
        y_val <- suppressWarnings(as.numeric(trace$y[j]))
        if (is.na(x_val) || is.na(y_val)) next

        candidate_rows <- which(
          !used_rows &
            abs(plot_data$x - x_val) <= sqrt(.Machine$double.eps) &
            abs(plot_data$y - y_val) <= sqrt(.Machine$double.eps)
        )

        if (length(candidate_rows) == 0) {
          candidate_rows <- which(
            abs(plot_data$x - x_val) <= sqrt(.Machine$double.eps) &
              abs(plot_data$y - y_val) <= sqrt(.Machine$double.eps)
          )
        }

        if (length(candidate_rows) > 0) {
          row_idx <- candidate_rows[1]
          trace_text[j] <- plot_data$hover_text[row_idx]
          used_rows[row_idx] <- TRUE
        }
      }

      if (any(!is.na(trace_text))) {
        p_plotly$x$data[[i]]$text <- trace_text
        p_plotly$x$data[[i]]$hovertemplate <- "%{text}<extra></extra>"
      }
    }
  }

  # Configure plotly interaction (keep same as before)
  p_plotly <- p_plotly %>%
    plotly::layout(
      dragmode = "select",
      hovermode = "closest"
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c('pan2d', 'autoscale2d')
    )

  # Set source for event handling
  p_plotly$x$source <- "pca_plot"

  # REGISTER EVENTS TO PREVENT WARNINGS
  p_plotly <- p_plotly %>%
    event_register('plotly_hover') %>%
    event_register('plotly_selected') %>%
    event_register('plotly_click')

  debug_log("Event registration completed for PCA plot", 2)
  debug_log("Interactive plot created successfully", 2)
  debug_log("=== INTERACTIVE PLOT CREATION END ===", 2)

  return(p_plotly)
}
