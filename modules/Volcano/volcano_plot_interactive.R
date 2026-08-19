# ==============================================================================
# volcano_plot_interactive.R
# ==============================================================================
#
# PURPOSE:
#   Interactive (plotly) plot creation for the volcano module. Converts prepared
#   plot data into plotly scatter plots with hover text, threshold lines, theme
#   matching, and selection/click event support.
#
# ARCHITECTURAL ROLE:
#   Interactive Plotting -- pure rendering functions that receive prepared data
#   as arguments and return plotly objects. No reactive state access.
#
# RESPONSIBILITIES:
#   - Prepare interactive plot data by enriching unified data with identifiers,
#     categories, and hover text (prepare_interactive_plot_data)
#   - Create plotly volcano scatter plots (create_plotly_volcano)
#   - Configure plotly layout and axis labels (configure_plotly_layout)
#   - Add threshold lines to plotly plots (add_plotly_threshold_lines)
#   - Add interactivity options and event registration (add_plotly_interactivity)
#   - Match ggplot themes to plotly styling (apply_plotly_theme_matching_ggplot)
#   - Create hover text for data points (create_hover_text)
#   - Process plotly selection events (process_plotly_selection)
#   - Keep plotly widgets ephemeral; session schema 2.0 restores interactive
#     views by rebuilding from lightweight plot data/cache state
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Direct access to reactive state (volcano_state, session, etc.)
#   - Data transformation logic (uses prepare_volcano_plot_data_safe from utils)
#   - Export/clipboard functions (those belong in volcano_export.R)
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - volcano_data_processing.R: prepare_volcano_plot_data_safe (unified data prep)
#     - volcano_plot_static.R: extract_plot_parameters, assign_point_attributes
#   External packages:
#     - plotly: plot_ly, add_trace, add_segments, layout, config, event_register
#
# INTERACTIONS:
#   Called by:
#     - volcano_module.R (orchestrator): renderPlotly output block
#   Calls into:
#     - volcano_data_processing.R: prepare_volcano_plot_data_safe
#     - volcano_plot_static.R: extract_plot_parameters, assign_point_attributes
#   Data flow:
#     - IN:  raw data frame, data_def metadata, input parameters
#     - OUT: plotly plot objects, enriched plot data frames
#
# LAST UPDATED: 2026-03-10
# ==============================================================================

# ========================================
# Data Preparation for Interactive Plot
# ========================================

#' Prepare plot data for interactive (plotly) rendering.
#'
#' Uses prepare_volcano_plot_data_safe() as the single canonical data
#' preparation path (same transformations as static plots), then enriches
#' the result with identifier, category, and hover text columns needed
#' by the plotly traces.
#'
#' @param data        Raw data frame
#' @param data_def    Data definition frame with Content, Transformation, etc.
#' @param plot_index  1-based index selecting which ratio/pval pair to plot
#' @param input       Shiny input list (for pValueSel, Identifier, thresholds)
#' @param debug_log   Logging function
#' @return data.frame with columns: x, y, row_idx, original_index, identifier,
#'         ID, category, color, size, hover_text -- or NULL on failure
prepare_interactive_plot_data <- function(data, data_def, plot_index, input,
                                          debug_log = function(msg, level = 1) cat(msg, "\n"),
                                          stored_pairs = NULL) {

  debug_log(paste("Preparing interactive plot data for index:", plot_index), 2)

  tryCatch({
    # Use stored pairing result from static plot generation if available
    if (!is.null(stored_pairs) && !is.null(stored_pairs$pairs) &&
        plot_index <= length(stored_pairs$pairs)) {
      pair <- stored_pairs$pairs[[plot_index]]
      ratio_idx <- pair$ratio_idx
      pval_idx <- pair$pval_idx
      debug_log(paste("Using stored pair:", pair$ratio_col, "with", pair$pval_col), 2)
    } else {
      # Fallback: resolve column indices independently
      abundance_cols <- which(grepl("^Abundance Ratio$", data_def$Content))

      if (input$pValueSel_Volcano == "Adjusted p-value") {
        pval_cols <- which(grepl("Adj.*p-Value$", data_def$Content))
      } else {
        pval_cols <- which(grepl("p-Value$", data_def$Content) &
                             !grepl("Adj", data_def$Content))
      }

      if (plot_index > length(abundance_cols) || plot_index > length(pval_cols)) {
        debug_log(paste("Plot index", plot_index, "out of bounds. Abundance cols:",
                        length(abundance_cols), "P-val cols:", length(pval_cols)), 1)
        return(NULL)
      }

      ratio_idx <- abundance_cols[plot_index]
      pval_idx  <- pval_cols[plot_index]
    }

    ratio_idx <- suppressWarnings(as.integer(ratio_idx[1]))
    pval_idx <- suppressWarnings(as.integer(pval_idx[1]))
    if (!is.data.frame(data) || !is.data.frame(data_def) ||
        !is.finite(ratio_idx) || !is.finite(pval_idx) ||
        ratio_idx < 1L || pval_idx < 1L ||
        ratio_idx > ncol(data) || pval_idx > ncol(data) ||
        ratio_idx > nrow(data_def) || pval_idx > nrow(data_def)) {
      debug_log(paste(
        "Interactive Volcano restore skipped invalid cached pair indices:",
        "ratio_idx=", ratio_idx, "pval_idx=", pval_idx,
        "data_cols=", if (is.data.frame(data)) ncol(data) else NA_integer_,
        "metadata_rows=", if (is.data.frame(data_def)) nrow(data_def) else NA_integer_
      ), 1)
      return(NULL)
    }

    # Use the unified data preparation (handles all transformation variants)
    plot_data <- prepare_volcano_plot_data_safe(data, data_def, ratio_idx, pval_idx, debug_log)

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      debug_log("Unified data preparation returned NULL or empty", 1)
      return(NULL)
    }

    debug_log(paste("Base plot data prepared:", nrow(plot_data), "rows"), 2)

    # Map row_idx (from prepare_volcano_plot_data_safe) to original_index
    plot_data$original_index <- plot_data$row_idx

    # Add identifier column from the original data using preserved row indices
    identifier_col <- input$Identifier_Volcano
    if (!is.null(identifier_col) && identifier_col %in% colnames(data)) {
      plot_data$identifier <- data[plot_data$row_idx, identifier_col]
      plot_data$ID <- plot_data$identifier
      debug_log(paste("Added identifier column:", identifier_col), 2)
    } else {
      plot_data$identifier <- paste0("Point_", seq_len(nrow(plot_data)))
      plot_data$ID <- plot_data$identifier
      debug_log("Using generic point identifiers", 2)
    }

    # Assign point categories and visual attributes
    if (!"category" %in% colnames(plot_data)) {
      params <- extract_plot_parameters(input)
      plot_data <- assign_point_attributes(plot_data, params)
    }

    # Add hover text
    if (!"hover_text" %in% colnames(plot_data)) {
      plot_data$hover_text <- create_hover_text(plot_data, input)
    }

    debug_log(paste("Interactive data preparation completed:", nrow(plot_data), "points"), 2)

    return(plot_data)

  }, error = function(e) {
    debug_log(paste("Error in interactive data preparation:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Plotly Plot Creation
# ========================================

create_plotly_volcano <- function(plot_data, input) {

  # Separate data by category for different traces
  neutral_data <- plot_data[plot_data$category == "neutral", ]
  up_data <- plot_data[plot_data$category == "up", ]
  down_data <- plot_data[plot_data$category == "down", ]

  # Create plot with separate traces
  p <- plot_ly()

  # Add neutral points
  if (nrow(neutral_data) > 0) {
    p <- p %>% add_trace(
      data = neutral_data,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        color = input$dotColorInput_Volcano,
        size = input$dotSizeInput_Volcano * 5,
        opacity = 0.6
      ),
      text = ~hover_text,
      hoverinfo = 'text',
      name = 'Not significant',
      customdata = ~identifier
    )
  }

  # Add upregulated points
  if (nrow(up_data) > 0) {
    p <- p %>% add_trace(
      data = up_data,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        color = input$dotColorInputUp_Volcano,
        size = input$dotSizeInputUp_Volcano * 5,
        opacity = 0.8
      ),
      text = ~hover_text,
      hoverinfo = 'text',
      name = 'Upregulated',
      customdata = ~identifier
    )
  }

  # Add downregulated points
  if (nrow(down_data) > 0) {
    p <- p %>% add_trace(
      data = down_data,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        color = input$dotColorInputDown_Volcano,
        size = input$dotSizeInputDown_Volcano * 5,
        opacity = 0.8
      ),
      text = ~hover_text,
      hoverinfo = 'text',
      name = 'Downregulated',
      customdata = ~identifier
    )
  }

  # Add threshold lines
  p <- add_plotly_threshold_lines(p, input)

  return(p)
}

# ========================================
# Hover Text Creation
# ========================================

create_hover_text <- function(plot_data, input) {

  fold_change <- sprintf("%.3f", plot_data$x)
  pval_display <- sprintf("%.2e", 10^(-plot_data$y))

  hover_text <- paste0(
    "<b>", plot_data$identifier, "</b><br>",
    "Log2 FC: ", fold_change, "<br>",
    "Fold Change: ", sprintf("%.3f", 2^plot_data$x), "<br>",
    if (input$pValueSel_Volcano == "Adjusted p-value") "Adj. " else "",
    "p-value: ", pval_display, "<br>",
    "-log10(p): ", sprintf("%.2f", plot_data$y)
  )

  return(hover_text)
}

# ========================================
# Plotly Layout Configuration
# ========================================

configure_plotly_layout <- function(p, input) {

  x_label <- "Log2 Fold Change"

  y_label <- if (input$pValueSel_Volcano == "Adjusted p-value") {
    "-log10(Adjusted p-value)"
  } else {
    "-log10(p-value)"
  }

  p <- p %>% layout(
    title = list(
      text = if (isTRUE(input$hideTitle_Volcano)) NULL else if (!is.null(input$plotTitle_Volcano) && nzchar(input$plotTitle_Volcano)) input$plotTitle_Volcano else NULL,
      font = list(size = input$plotTitleSize_Volcano)
    ),
    xaxis = list(
      title = x_label,
      titlefont = list(size = input$AxisTitleSize_Volcano),
      tickfont = list(size = input$tickSize_Volcano),
      range = input$xLimInput_Volcano,
      dtick = input$xTick_Volcano,
      zeroline = TRUE,
      zerolinewidth = 1,
      zerolinecolor = 'rgb(200,200,200)'
    ),
    yaxis = list(
      title = y_label,
      titlefont = list(size = input$AxisTitleSize_Volcano),
      tickfont = list(size = input$tickSize_Volcano),
      range = input$yLimInput_Volcano,
      dtick = input$yTick_Volcano,
      zeroline = FALSE
    ),
    showlegend = TRUE,
    legend = list(
      x = 1.02,
      y = 1,
      xanchor = 'left',
      yanchor = 'top'
    ),
    hovermode = 'closest',
    dragmode = 'select'
  )

  # Apply theme styling
  p <- apply_plotly_theme_matching_ggplot(p, input$ThemeSelect_Volcano)

  return(p)
}

# ========================================
# Threshold Lines for Plotly
# ========================================

add_plotly_threshold_lines <- function(p, input) {

  # Vertical lines for fold change threshold
  p <- p %>%
    add_segments(
      x = input$AbundanceInput_Volcano,
      xend = input$AbundanceInput_Volcano,
      y = input$yLimInput_Volcano[1],
      yend = input$yLimInput_Volcano[2],
      line = list(color = "black", width = 1, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = 'skip'
    ) %>%
    add_segments(
      x = -input$AbundanceInput_Volcano,
      xend = -input$AbundanceInput_Volcano,
      y = input$yLimInput_Volcano[1],
      yend = input$yLimInput_Volcano[2],
      line = list(color = "black", width = 1, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = 'skip'
    )

  # Horizontal line for p-value threshold
  p <- p %>%
    add_segments(
      x = input$xLimInput_Volcano[1],
      xend = input$xLimInput_Volcano[2],
      y = -log10(input$pvalueInput_Volcano),
      yend = -log10(input$pvalueInput_Volcano),
      line = list(color = "black", width = 1, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = 'skip'
    )

  return(p)
}

# ========================================
# Interactivity Options
# ========================================

add_plotly_interactivity <- function(p) {

  p <- p %>%
    layout(
      dragmode = "select",
      hovermode = "closest",
      clickmode = "event+select"
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "volcano_plot",
        height = 600,
        width = 800,
        scale = 2
      ),
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'autoscale2d'),
      displayModeBar = TRUE,
      scrollZoom = TRUE
    )

  # Set source for event handling
  p$x$source <- "volcano_plot"

  # Register events to prevent warnings
  p <- p %>%
    event_register('plotly_hover') %>%
    event_register('plotly_selected') %>%
    event_register('plotly_click')

  return(p)
}

# ========================================
# Theme Application for Plotly
# ========================================

apply_plotly_theme_matching_ggplot <- function(p, theme_name) {

  theme_config <- switch(theme_name,
                         "Gray" = list(
                           plot_bgcolor = 'rgb(229, 229, 229)',
                           paper_bgcolor = 'rgb(229, 229, 229)',
                           gridcolor = 'white',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'black',
                           linewidth = 1
                         ),
                         "Black and White" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgb(200, 200, 200)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'black',
                           linewidth = 1
                         ),
                         "Classic" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgba(0,0,0,0)',
                           font_color = 'black',
                           showgrid = FALSE,
                           linecolor = 'black',
                           linewidth = 2
                         ),
                         "Linedraw" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgb(0, 0, 0)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'black',
                           linewidth = 1
                         ),
                         "Light" = list(
                           plot_bgcolor = 'rgb(250, 250, 250)',
                           paper_bgcolor = 'rgb(250, 250, 250)',
                           gridcolor = 'rgb(220, 220, 220)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'black',
                           linewidth = 1
                         ),
                         "Dark" = list(
                           plot_bgcolor = 'rgb(50, 50, 50)',
                           paper_bgcolor = 'rgb(40, 40, 40)',
                           gridcolor = 'rgb(100, 100, 100)',
                           font_color = 'white',
                           showgrid = TRUE,
                           linecolor = 'white',
                           linewidth = 1
                         ),
                         "Minimal" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgb(240, 240, 240)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'rgb(100, 100, 100)',
                           linewidth = 0.5
                         ),
                         "Void" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgba(0,0,0,0)',
                           font_color = 'black',
                           showgrid = FALSE,
                           linecolor = 'rgba(0,0,0,0)',
                           linewidth = 0
                         ),
                         # Default fallback
                         list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgb(200, 200, 200)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'black',
                           linewidth = 1
                         )
  )

  # Update layout with theme
  p <- p %>% layout(
    plot_bgcolor = theme_config$plot_bgcolor,
    paper_bgcolor = theme_config$paper_bgcolor,
    xaxis = list(
      showgrid = theme_config$showgrid,
      gridcolor = theme_config$gridcolor,
      tickcolor = theme_config$linecolor,
      linecolor = theme_config$linecolor,
      linewidth = theme_config$linewidth,
      mirror = FALSE
    ),
    yaxis = list(
      showgrid = theme_config$showgrid,
      gridcolor = theme_config$gridcolor,
      tickcolor = theme_config$linecolor,
      linecolor = theme_config$linecolor,
      linewidth = theme_config$linewidth,
      mirror = FALSE
    )
  )

  # Update font colors
  p <- p %>% layout(
    font = list(color = theme_config$font_color),
    title = list(font = list(color = theme_config$font_color)),
    xaxis = list(
      titlefont = list(color = theme_config$font_color),
      tickfont = list(color = theme_config$font_color)
    ),
    yaxis = list(
      titlefont = list(color = theme_config$font_color),
      tickfont = list(color = theme_config$font_color)
    ),
    legend = list(
      font = list(color = theme_config$font_color)
    )
  )

  return(p)
}

# ========================================
# Selection Processing
# ========================================

process_plotly_selection <- function(selected_data, plot_data, data, identifier_col) {

  if (is.null(selected_data) || nrow(selected_data) == 0) {
    return(NULL)
  }

  # Get indices of selected points
  selected_indices <- selected_data$pointNumber + 1  # R is 1-indexed

  # Map to original data
  if ("original_index" %in% names(plot_data)) {
    original_indices <- plot_data$original_index[selected_indices]
    selected_rows <- data[original_indices, ]
  } else {
    selected_rows <- data[selected_indices, ]
  }

  # Extract identifiers
  if (identifier_col %in% colnames(selected_rows)) {
    identifiers <- selected_rows[[identifier_col]]
  } else {
    identifiers <- rownames(selected_rows)
  }

  return(list(
    data = selected_rows,
    identifiers = identifiers,
    count = length(identifiers)
  ))
}
