# ==============================================================================
# dotplot_interactive_utils.R - Dotplot Plotly augmentation utilities
#
# Plotly hover, theme, selection-data, and interactive plot construction helpers.
# Sourced after dotplot_utils.R by modules/dotplot_module.R.
# ==============================================================================

# ========================================
# Comprehensive Hover Text for All Plot Types
# ========================================

add_custom_hover_to_dotplot_comprehensive <- function(plotly_obj, data, axis_config, identifier_col) {
  tryCatch({
    dotplot_debug_log("Adding comprehensive hover text to dotplot", 2)

    # Get axis labels
    x_label <- if (!is.null(axis_config$x_label) && nzchar(axis_config$x_label)) {
      axis_config$x_label
    } else {
      axis_config$x_col
    }

    y_label <- if (!is.null(axis_config$y_label) && nzchar(axis_config$y_label)) {
      axis_config$y_label
    } else {
      axis_config$y_col
    }

    # Get identifier data
    identifier_data <- if (identifier_col %in% colnames(data)) {
      data[[identifier_col]]
    } else {
      paste("Point", seq_len(nrow(data)))
    }

    # Apply to ALL traces in the plotly object
    for (i in seq_along(plotly_obj$x$data)) {
      trace <- plotly_obj$x$data[[i]]

      # Get the number of points in this trace
      n_points <- length(trace$x)

      if (n_points > 0) {
        # Create identifier subset for this trace
        if (length(identifier_data) >= n_points) {
          trace_identifiers <- identifier_data[1:n_points]
        } else {
          trace_identifiers <- rep("Unknown", n_points)
        }

        # Set comprehensive hover template
        plotly_obj$x$data[[i]]$hovertemplate <- paste0(
          "<b>%{text}</b><br>",
          x_label, ": %{x}<br>",
          y_label, ": %{y}<extra></extra>"
        )

        # Add identifier as text
        plotly_obj$x$data[[i]]$text <- trace_identifiers

        dotplot_debug_log(paste("Applied hover to trace", i, "with", n_points, "points"), 2)
      }
    }

    dotplot_debug_log("Comprehensive hover text applied successfully", 2)
    return(plotly_obj)

  }, error = function(e) {
    dotplot_debug_log(paste("Error adding comprehensive hover:", e$message), 1)
    return(plotly_obj)  # Return original object on error
  })
}

# ========================================
# Apply Plotly Theme for Dot Plot
# ========================================

apply_dotplot_plotly_theme <- function(p, theme_name) {
  theme_name <- dotplot_normalize_theme_name(theme_name)

  dotplot_debug_log(paste("Applying plotly theme:", theme_name), 2)

  # Map UI theme values to plotly styling
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
                           gridcolor = 'rgba(0,0,0,0)',  # No grid lines
                           font_color = 'black',
                           showgrid = FALSE,  # Turn off grid completely
                           linecolor = 'black',
                           linewidth = 2  # Thicker axis lines
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
                         "Minimal" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgb(240, 240, 240)',
                           font_color = 'black',
                           showgrid = TRUE,
                           linecolor = 'rgb(100, 100, 100)',
                           linewidth = 0.5
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
                         "Void" = list(
                           plot_bgcolor = 'white',
                           paper_bgcolor = 'white',
                           gridcolor = 'rgba(0,0,0,0)',
                           font_color = 'black',
                           showgrid = FALSE,
                           linecolor = 'rgba(0,0,0,0)',
                           linewidth = 0
                         ),
                         # Default (fallback)
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

  dotplot_debug_log(paste("Using theme config for:", theme_name), 2)

  # Update layout and fonts together to avoid overwriting axis grid settings
  p <- p %>% layout(
    plot_bgcolor = theme_config$plot_bgcolor,
    paper_bgcolor = theme_config$paper_bgcolor,
    font = list(color = theme_config$font_color),
    title = list(font = list(color = theme_config$font_color)),
    xaxis = list(
      showgrid = theme_config$showgrid,
      gridcolor = theme_config$gridcolor,
      tickcolor = theme_config$linecolor,
      linecolor = theme_config$linecolor,
      linewidth = theme_config$linewidth,
      mirror = FALSE,  # No frame/border
      titlefont = list(color = theme_config$font_color),
      tickfont = list(color = theme_config$font_color)
    ),
    yaxis = list(
      showgrid = theme_config$showgrid,
      gridcolor = theme_config$gridcolor,
      tickcolor = theme_config$linecolor,
      linecolor = theme_config$linecolor,
      linewidth = theme_config$linewidth,
      mirror = FALSE,  # No frame/border
      titlefont = list(color = theme_config$font_color),
      tickfont = list(color = theme_config$font_color)
    ),
    legend = list(
      font = list(color = theme_config$font_color)
    )
  )

  dotplot_debug_log("Theme applied successfully", 2)
  return(p)
}


# ========================================
# Enhanced Interactive Plot Functions
# ========================================

# ------------------------------------------------------------------------------
# dotplot_create_interactive
# Purpose: Convert a ggplot dotplot to plotly with hover metadata, selection
#   support, and registered click/lasso/box events.
# Parameters:
#   - ggplot_obj: ggplot - Static plot object to convert.
#   - data: data.frame - Source data used for customdata/hover text.
#   - axis_config: list - Axis selections and labels for hover display.
#   - input: reactivevalues/list - Optional Shiny inputs for theme/identifier.
# Returns: plotly object or NULL when conversion fails.
# ------------------------------------------------------------------------------
dotplot_create_interactive <- function(ggplot_obj, data, axis_config, input = NULL) {
  # Convert ggplot to interactive plotly object with enhanced selection capabilities

  dotplot_debug_log("Creating enhanced interactive plot with selection support", 2)

  tryCatch({
    # Convert to plotly with basic tooltips first
    plotly_obj <- plotly::ggplotly(ggplot_obj, tooltip = c("x", "y"))

    # Enhance with custom hover and selection data
    if (!is.null(input) && !is.null(input$GeneIdentifierColumn_dot)) {
      plotly_obj <- add_selection_data_to_dotplot(
        plotly_obj, data, axis_config, input$GeneIdentifierColumn_dot
      )
    }

    # Apply theme if input is provided
    if (!is.null(input) && !is.null(input$theme_select)) {
      dotplot_debug_log(paste("Applying theme:", input$theme_select), 2)
      plotly_obj <- apply_dotplot_plotly_theme(plotly_obj, input$theme_select)
    }

    # Configure for selection and interactivity
    plotly_obj <- plotly_obj %>%
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("autoScale2d"),  # Keep selection tools
        displaylogo = FALSE
      ) %>%
      plotly::layout(
        hovermode = "closest",
        dragmode = "select"  # Enable box selection by default
      )

    # Set source for event handling (CRITICAL for selection events)
    plotly_obj$x$source <- "dotplot_interactive"

    # Register events to prevent warnings
    plotly_obj <- plotly_obj %>%
      plotly::event_register('plotly_hover') %>%
      plotly::event_register('plotly_selected') %>%
      plotly::event_register('plotly_click')

    dotplot_debug_log("Enhanced interactive plot created successfully with selection support", 2)
    return(plotly_obj)

  }, error = function(e) {
    dotplot_debug_log(paste("Error creating enhanced interactive plot:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Selection Data Enhancement for Dot Plot
# ========================================

add_selection_data_to_dotplot <- function(plotly_obj, data, axis_config, identifier_col) {
  # Add custom data and hover information to support selection

  tryCatch({
    dotplot_debug_log("Adding selection data to dot plot", 2)

    # Get identifier data if available
    identifier_data <- if (identifier_col %in% colnames(data)) {
      data[[identifier_col]]
    } else {
      paste("Point", seq_len(nrow(data)))
    }

    # Get axis labels for display
    x_label <- if (!is.null(axis_config$x_label) && nzchar(axis_config$x_label)) {
      axis_config$x_label
    } else {
      axis_config$x_col
    }

    y_label <- if (!is.null(axis_config$y_label) && nzchar(axis_config$y_label)) {
      axis_config$y_label
    } else {
      axis_config$y_col
    }

    # Enhance the first trace (main scatter plot) with custom data
    if (length(plotly_obj$x$data) > 0) {
      # Add customdata for reliable selection
      plotly_obj$x$data[[1]]$customdata <- identifier_data

      # Create enhanced hover template
      hover_template <- paste0(
        "<b>%{customdata}</b><br>",
        x_label, ": %{x}<br>",
        y_label, ": %{y}<br>",
        "<extra></extra>"  # Removes the trace name box
      )

      plotly_obj$x$data[[1]]$hovertemplate <- hover_template

      dotplot_debug_log(paste("Enhanced", length(identifier_data), "points with selection data"), 2)
    }

    return(plotly_obj)

  }, error = function(e) {
    dotplot_debug_log(paste("Error adding selection data to dot plot:", e$message), 1)
    return(plotly_obj)  # Return original if enhancement fails
  })
}
