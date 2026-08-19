# ==============================================================================
# File: modules/abundances/abundances_logic.R
#
# Purpose:
#   Contains all pure logic and helper functions for the Abundances module.
#   These functions build ggplot2 and plotly objects from pre-processed data
#   and carry no Shiny dependency.
#
# Architectural Role:
#   Logic layer of the Abundances module. Called by observer functions defined
#   in abundances_observer.R. Sourced into modEnv via abundances_module.R so
#   that observer code can call these functions directly by name.
#
# Structure:
#   1. sanitize_plot_id(x)        - Replaces non-alphanumeric characters in a
#                                   string to produce a safe plot identifier.
#   2. build_abundances_plot(...) - Builds ggplot2 and plotly abundance boxplots
#                                   from a long-format data frame and user options.
#                                   Returns a named list or NULL on failure.
#
# Notes for future developers:
#   - Every function in this file must remain Shiny-free (no input, output,
#     session, reactive, observe). This preserves unit-testability.
#   - build_abundances_plot receives debug_log as its last parameter. Always
#     pass the debug_log closure from modAbundancesServer when calling it.
#   - Return values follow the convention: named list on success, NULL on error.
#   - Do not introduce global state, side-effects, or reactive wrappers here.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. sanitize_plot_id
# ------------------------------------------------------------------------------

#' Replace non-alphanumeric characters in a string to produce a safe ID.
#'
#' @param x Character scalar to sanitize.
#' @return Character scalar with non-alphanumeric runs replaced by underscores.
sanitize_plot_id <- function(x) {
  gsub("[^[:alnum:]_]+", "_", x)
}


# ------------------------------------------------------------------------------
# 2. build_abundances_plot
# ------------------------------------------------------------------------------

#' Build ggplot2 and plotly abundance boxplots from long-format data.
#'
#' @param data_long       Long-format data frame with columns Variable and Value.
#' @param data_abundance  Character scalar: selected abundance type label (used
#'                        in y-axis annotation).
#' @param abundance_title Character scalar: plot title.
#' @param color_input     Character scalar: color palette name or "Black and White".
#' @param selected_layout Character scalar: ggplot2 theme name.
#' @param hide_title      Logical: whether to suppress the plot title.
#' @param plot_title_size Numeric: font size for the plot title.
#' @param axis_title_size Numeric: font size for axis titles.
#' @param tick_size       Numeric: font size for axis tick labels.
#' @param debug_log       Logging function with signature (message, level).
#' @return Named list with elements `ggplot_object` and `plotly_object`, or NULL
#'         if data_long is empty or invalid.
build_abundances_plot <- function(data_long, data_abundance, abundance_title,
                                  color_input, selected_layout, hide_title,
                                  plot_title_size, axis_title_size, tick_size,
                                  debug_log) {

  if (is.null(data_long) || nrow(data_long) == 0) {
    debug_log("build_abundances_plot: data_long is empty — returning NULL", 1)
    return(NULL)
  }

  # Y-axis label using bquote
  y_lab <- bquote(log[2]("" * .(data_abundance) * ""))
  plot_title <- if (isTRUE(hide_title)) NULL else abundance_title

  # Build base plot (two cases: colored vs. black and white)
  if (color_input == "Black and White") {
    p <- ggplot(data_long, aes(x = Variable, y = log2(Value))) +
      geom_boxplot(fill = "white", colour = "black") +
      xlab("Sample") +
      ylab(y_lab) +
      labs(title = plot_title)

    p_ly <- ggplot(data_long, aes(x = Variable, y = log2(Value))) +
      geom_boxplot(fill = "white", colour = "black") +
      xlab("Sample") +
      labs(title = plot_title)
  } else {
    # Viridis color palette
    color_key      <- tolower(color_input)
    color_palette2 <- viridis(length(unique(data_long$Variable)), option = color_key)

    p <- ggplot(data_long, aes(x = Variable, y = log2(Value), fill = Variable)) +
      geom_boxplot() +
      xlab("Sample") +
      ylab(y_lab) +
      labs(title = plot_title) +
      scale_fill_manual(values = color_palette2)

    p_ly <- ggplot(data_long, aes(x = Variable, y = log2(Value), fill = Variable)) +
      geom_boxplot() +
      xlab("Sample") +
      labs(title = plot_title) +
      scale_fill_manual(values = color_palette2)
  }

  # Select layout theme
  layout_theme <- switch(selected_layout,
                         "Gray"          = theme_gray(),
                         "Black and White" = theme_bw(),
                         "Linedraw"      = theme_linedraw(),
                         "Light"         = theme_light(),
                         "Dark"          = theme_dark(),
                         "Minimal"       = theme_minimal(),
                         "Classic"       = theme_classic(),
                         "Void"          = theme_void(),
                         theme_gray())

  shared_theme <- theme(
    axis.text.x    = element_text(angle = 90, vjust = 0.5, hjust = 1),
    text           = element_text(size = 16),
    plot.title     = if (isTRUE(hide_title)) element_blank() else element_text(size = plot_title_size),
    axis.title     = element_text(size = axis_title_size),
    axis.text      = element_text(size = tick_size)
  )

  p <- p + layout_theme + shared_theme + theme(legend.position = "none")

  p_ly <- p_ly + layout_theme + shared_theme

  # Create interactive plotly object; errors are caught
  plotly_object <- tryCatch({
    ggplotly(p_ly) %>%
      layout(yaxis = list(title = paste0(
        "log<sub>2</sub>(", data_abundance, ")"
      )))
  }, error = function(e) {
    debug_log(paste("build_abundances_plot: ggplotly conversion failed:", e$message), 1)
    NULL
  })

  debug_log(
    sprintf(
      "Abundance plot summary | Abundance type: %s | Samples: %s | Color palette: %s | Theme: %s",
      data_abundance,
      length(unique(data_long$Variable)),
      color_input,
      selected_layout
    ),
    level = 0
  )

  list(
    ggplot_object  = p,
    plotly_object  = plotly_object
  )
}
