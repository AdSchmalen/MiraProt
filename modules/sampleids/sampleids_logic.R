# ==============================================================================
# File: modules/sampleids/sampleids_logic.R
#
# Purpose:
#   Contains all pure logic and helper functions for the Sample IDs module.
#   These functions build ggplot2 and plotly objects from pre-processed data
#   and carry no Shiny dependency.
#
# Architectural Role:
#   Logic layer of the Sample IDs module. Called by observer functions defined
#   in sampleids_observer.R. Sourced into modEnv via sampleids_module.R so
#   that observer code can call these functions directly by name.
#
# Structure:
#   1. sanitize_plot_id(x)           - Replaces non-alphanumeric characters in a
#                                      string to produce a safe plot identifier.
#   2. get_legend_side(pos)          - Validates and returns a legend position string.
#   3. get_legend_direction(pos)     - Derives legend direction from position.
#   4. apply_sampleids_theme(...)    - Applies ggplot2 theme, text sizes, and legend
#                                      settings to a plot object.
#   5. build_sampleids_char_plot(...)  - Builds bar chart for character string data.
#   6. build_sampleids_num_plot(...)   - Builds boxplot/violin/bar for numeric data.
#
# Notes for future developers:
#   - Every function in this file must remain Shiny-free (no input, output,
#     session, reactive, observe). This preserves unit-testability.
#   - build_sampleids_char_plot and build_sampleids_num_plot receive debug_log
#     as their last parameter. Always pass the debug_log closure from
#     modSampleIDsServer when calling them.
#   - Return values follow the convention: named list with ggplot_object and
#     plotly_object on success, NULL on invalid plot_type for numeric plots.
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
# 2. get_legend_side
# ------------------------------------------------------------------------------

#' Validate and return a legend position string.
#'
#' @param legend_position Character scalar from UI input (e.g. "none", "right").
#' @return One of "none", "left", "right", "top", "bottom".
get_legend_side <- function(legend_position) {
  pos <- tolower(legend_position %||% "none")
  if (!pos %in% c("none", "left", "right", "top", "bottom")) pos <- "none"
  pos
}


# ------------------------------------------------------------------------------
# 3. get_legend_direction
# ------------------------------------------------------------------------------

#' Derive the legend direction from a legend position string.
#'
#' @param legend_position Character scalar (same domain as get_legend_side).
#' @return "horizontal" for top/bottom positions, "vertical" otherwise.
get_legend_direction <- function(legend_position) {
  side <- get_legend_side(legend_position)
  if (side %in% c("top", "bottom")) "horizontal" else "vertical"
}


# ------------------------------------------------------------------------------
# 4. apply_sampleids_theme
# ------------------------------------------------------------------------------

#' Apply ggplot2 theme, text sizes, and legend settings to a plot object.
#'
#' @param plot             A ggplot2 object.
#' @param theme_name       Character: theme name (e.g. "Classic", "Minimal").
#' @param title_size       Numeric: font size for the plot title.
#' @param axis_title_size  Numeric: font size for axis titles.
#' @param tick_size        Numeric: font size for axis tick labels.
#' @param legend_title_size Numeric: font size for legend title.
#' @param legend_text_size  Numeric: font size for legend text.
#' @param legend_side      Character: legend position (validated by get_legend_side).
#' @param legend_dir       Character: legend direction from get_legend_direction.
#' @return The modified ggplot2 object.
apply_sampleids_theme <- function(plot, theme_name,
                                  title_size, axis_title_size, tick_size,
                                  legend_title_size, legend_text_size,
                                  legend_side, legend_dir) {
  layout_theme <- switch(theme_name,
    "Gray"            = theme_gray(),
    "Black and White" = theme_bw(),
    "Linedraw"        = theme_linedraw(),
    "Light"           = theme_light(),
    "Dark"            = theme_dark(),
    "Minimal"         = theme_minimal(),
    "Classic"         = theme_classic(),
    "Void"            = theme_void(),
    theme_minimal()
  )

  extra_legend_theme <- if (legend_dir == "horizontal") {
    theme(
      legend.box       = "horizontal",
      legend.spacing.x = grid::unit(6, "mm"),
      legend.key.width = grid::unit(6, "mm"),
      legend.text      = element_text(
        size   = legend_text_size,
        margin = ggplot2::margin(l = 4, r = 4)
      )
    )
  } else {
    theme(legend.text = element_text(size = legend_text_size))
  }

  plot +
    layout_theme +
    theme(
      plot.title       = element_text(size = title_size),
      axis.title       = element_text(size = axis_title_size),
      axis.text.x      = element_text(size = tick_size, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y      = element_text(size = tick_size),
      legend.title     = element_text(size = legend_title_size),
      legend.position  = legend_side,
      legend.direction = legend_dir
    ) +
    extra_legend_theme
}


# ------------------------------------------------------------------------------
# 5. build_sampleids_char_plot
# ------------------------------------------------------------------------------

#' Build a bar chart for character string data in the Sample IDs module.
#'
#' Handles two sub-cases based on the number of unique string values:
#'   - More than 10 unique values: unique-count bar chart (one bar per sample).
#'   - Up to 10 unique values: stacked bar chart with custom hover text.
#'
#' @param df               Data frame containing only the selected character columns.
#' @param col_indices      Integer vector: column indices within data_def for label lookup.
#' @param data_def         Data frame: column metadata with Sample and Column fields.
#' @param sort_values      Character vector: selected string values to include (or NULL).
#' @param abs_rel          Character: "Absolute" or "Relative".
#' @param label_type       Character: "Sample name" or "Column name".
#' @param color_input      Character: lowercase color palette name or "gray".
#' @param col_reverse      Logical: whether to reverse the color palette.
#' @param theme_name       Character: ggplot2 theme name.
#' @param title            Character: plot title.
#' @param title_size       Numeric: font size for the plot title.
#' @param axis_title_size  Numeric: font size for axis titles.
#' @param tick_size        Numeric: font size for tick labels.
#' @param legend_title_size Numeric: font size for legend title.
#' @param legend_text_size  Numeric: font size for legend text.
#' @param legend_position  Character: legend position string.
#' @param debug_log        Logging function with signature (message, level).
#' @return Named list with elements ggplot_object and plotly_object.
build_sampleids_char_plot <- function(df, col_indices, data_def,
                                      sort_values, abs_rel, label_type,
                                      color_input, col_reverse,
                                      theme_name, title,
                                      title_size, axis_title_size, tick_size,
                                      legend_title_size, legend_text_size,
                                      legend_position, debug_log) {
  legend_side <- get_legend_side(legend_position)
  legend_dir  <- get_legend_direction(legend_position)

  sample_labels <- if (label_type == "Sample name") {
    labels <- data_def$Sample[col_indices]
    if (is.null(labels) || length(labels) == 0) data_def$Column[col_indices] else labels
  } else {
    data_def$Column[col_indices]
  }

  df_long <- melt(df, id.vars = NULL, variable.name = "Sample", value.name = "Value")

  if (!is.null(sort_values) && length(sort_values) > 0) {
    df_long_filtered <- df_long %>% filter(Value %in% sort_values)
    df_long_filtered$Value <- factor(df_long_filtered$Value, levels = sort_values)
  } else {
    df_long_filtered <- df_long
  }

  n_unique_vals <- length(unique(df_long_filtered$Value))

  if (n_unique_vals > 10) {
    # Unique-count bar chart: one bar per sample showing how many distinct strings
    debug_log("build_sampleids_char_plot: building unique counts bar chart", 2)

    unique_counts <- sapply(df, function(col) length(unique(na.omit(col))))
    unique_counts_df <- data.frame(Sample = names(unique_counts), Count = unique_counts)

    n_samples <- length(unique(unique_counts_df$Sample))
    color_palette2 <- if (color_input == "gray") {
      gray.colors(n_samples, start = 0.2, end = 0.8)
    } else {
      viridis(n_samples, option = color_input)
    }
    if (col_reverse) color_palette2 <- rev(color_palette2)

    plot <- ggplot(unique_counts_df, aes(x = Sample, y = Count, fill = Sample)) +
      geom_bar(stat = "identity", color = "black") +
      scale_x_discrete(labels = sample_labels) +
      labs(x = NULL, y = "Number of Unique Strings", fill = "Sample", title = title) +
      scale_fill_manual(values = color_palette2)

    if (abs_rel == "Relative") {
      n_rows <- nrow(df)
      plot <- plot +
        aes(y = Count / n_rows) +
        labs(y = "Proportion of Unique Strings") +
        scale_y_continuous(labels = scales::percent)
    }

    plot <- apply_sampleids_theme(
      plot, theme_name,
      title_size, axis_title_size, tick_size,
      legend_title_size, legend_text_size,
      legend_side, legend_dir
    )

    debug_log("build_sampleids_char_plot: unique counts bar chart built", 1)

    plotly_object <- tryCatch(
      ggplotly(plot),
      error = function(e) {
        debug_log(paste("build_sampleids_char_plot: ggplotly failed:", e$message), 1)
        NULL
      }
    )

    return(list(ggplot_object = plot, plotly_object = plotly_object))
  }

  # Stacked bar chart with custom hover text
  debug_log("build_sampleids_char_plot: building stacked bar chart", 2)

  sample_totals <- df_long_filtered %>%
    group_by(Sample) %>%
    summarise(Total = n(), .groups = "drop")

  df_long_filtered <- df_long_filtered %>%
    left_join(sample_totals, by = "Sample") %>%
    group_by(Sample, Value) %>%
    mutate(Count = n()) %>%
    ungroup()

  df_long_filtered$hover_text <- if (abs_rel == "Absolute") {
    paste0(
      "<b>Sample:</b> ", df_long_filtered$Sample, "<br>",
      "<b>Category:</b> ", df_long_filtered$Value, "<br>",
      "<b>Count:</b> ", df_long_filtered$Count, "<br>",
      "<b>Total:</b> ", df_long_filtered$Total
    )
  } else {
    paste0(
      "<b>Sample:</b> ", df_long_filtered$Sample, "<br>",
      "<b>Category:</b> ", df_long_filtered$Value, "<br>",
      "<b>Count:</b> ", df_long_filtered$Count, "<br>",
      "<b>Proportion:</b> ", round(df_long_filtered$Count / df_long_filtered$Total * 100, 1), "%<br>",
      "<b>Total:</b> ", df_long_filtered$Total
    )
  }

  color_palette2 <- if (color_input == "gray") {
    gray.colors(n_unique_vals, start = 0.2, end = 0.8)
  } else {
    viridis(n_unique_vals, option = color_input)
  }
  if (col_reverse) color_palette2 <- rev(color_palette2)

  pos_arg <- if (abs_rel == "Absolute") "stack" else "fill"
  y_lab   <- if (abs_rel == "Absolute") "Count" else "Proportion"

  plot <- ggplot(df_long_filtered, aes(x = Sample, fill = Value, text = hover_text)) +
    geom_bar(position = pos_arg, color = "black") +
    scale_x_discrete(labels = sample_labels) +
    labs(title = title, x = "Sample", y = y_lab, fill = "Category") +
    scale_fill_manual(values = color_palette2)

  if (abs_rel == "Relative") {
    plot <- plot + scale_y_continuous(labels = scales::percent)
  }

  plot <- apply_sampleids_theme(
    plot, theme_name,
    title_size, axis_title_size, tick_size,
    legend_title_size, legend_text_size,
    legend_side, legend_dir
  )

  plotly_object <- tryCatch({
    plotly::ggplotly(plot, tooltip = "text") %>%
      plotly::config(
        displayModeBar          = TRUE,
        displaylogo             = FALSE,
        modeBarButtonsToRemove  = c("select2d", "lasso2d", "autoScale2d")
      ) %>%
      plotly::layout(hovermode = "closest")
  }, error = function(e) {
    debug_log(paste("build_sampleids_char_plot: ggplotly failed:", e$message), 1)
    NULL
  })

  debug_log("build_sampleids_char_plot: stacked bar chart built", 1)
  list(ggplot_object = plot, plotly_object = plotly_object)
}


# ------------------------------------------------------------------------------
# 6. build_sampleids_num_plot
# ------------------------------------------------------------------------------

#' Build a boxplot, violin plot, or bar chart for numeric data.
#'
#' @param df               Data frame containing only the selected, transformed numeric
#'                         columns.
#' @param col_indices      Integer vector: column indices within data_def for label lookup.
#' @param data_def         Data frame: column metadata with Sample and Column fields.
#' @param plot_type        Character: "Boxplot", "Violinplot", or "Barplot".
#' @param label_type       Character: "Sample name" or "Column name".
#' @param color_input      Character: lowercase color palette name or "gray".
#' @param col_reverse      Logical: whether to reverse the color palette.
#' @param theme_name       Character: ggplot2 theme name.
#' @param title            Character: plot title.
#' @param title_size       Numeric: font size for the plot title.
#' @param axis_title_size  Numeric: font size for axis titles.
#' @param tick_size        Numeric: font size for tick labels.
#' @param legend_title_size Numeric: font size for legend title.
#' @param legend_text_size  Numeric: font size for legend text.
#' @param legend_position  Character: legend position string.
#' @param debug_log        Logging function with signature (message, level).
#' @return Named list with elements ggplot_object and plotly_object, or NULL if
#'         plot_type is not recognized.
build_sampleids_num_plot <- function(df, col_indices, data_def,
                                     plot_type, label_type,
                                     color_input, col_reverse,
                                     theme_name, title,
                                     title_size, axis_title_size, tick_size,
                                     legend_title_size, legend_text_size,
                                     legend_position, debug_log) {
  legend_side <- get_legend_side(legend_position)
  legend_dir  <- get_legend_direction(legend_position)

  sample_labels <- if (label_type == "Sample name") {
    labels <- data_def$Sample[col_indices]
    if (is.null(labels) || length(labels) == 0) data_def$Column[col_indices] else labels
  } else {
    data_def$Column[col_indices]
  }

  df_long <- melt(df, id.vars = NULL, variable.name = "Sample", value.name = "Value")
  df_long <- df_long[!is.na(df_long$Value), ]

  color_palette2 <- if (color_input == "gray") {
    gray.colors(length(unique(df_long$Sample)), start = 0.2, end = 0.8)
  } else {
    viridis(length(unique(df_long$Sample)), option = color_input)
  }
  if (col_reverse) color_palette2 <- rev(color_palette2)

  if (plot_type == "Boxplot") {
    plot <- ggplot(df_long, aes(x = Sample, y = Value, fill = Sample)) +
      scale_x_discrete(labels = sample_labels) +
      labs(x = NULL, y = "Value", fill = "Sample", title = title) +
      scale_fill_manual(values = color_palette2) +
      geom_boxplot()

  } else if (plot_type == "Violinplot") {
    plot <- ggplot(df_long, aes(x = Sample, y = Value, fill = Sample)) +
      scale_x_discrete(labels = sample_labels) +
      labs(x = NULL, y = "Value", fill = "Sample", title = title) +
      scale_fill_manual(values = color_palette2) +
      geom_violin(color = "black", fill = NA, trim = FALSE) +
      geom_boxplot(aes(fill = Sample), width = 0.4, color = "black", outlier.shape = NA)

  } else if (plot_type == "Barplot") {
    df_long_bar <- df_long[df_long$Value > 0, ]
    plot <- ggplot(df_long_bar, aes(x = Sample, fill = Sample)) +
      geom_bar(color = "black") +
      scale_x_discrete(labels = sample_labels) +
      labs(x = "Sample", y = "Count", fill = "Sample", title = title) +
      scale_fill_manual(values = color_palette2)

  } else {
    debug_log(paste("build_sampleids_num_plot: unrecognized plot_type:", plot_type), 1)
    return(NULL)
  }

  plot <- apply_sampleids_theme(
    plot, theme_name,
    title_size, axis_title_size, tick_size,
    legend_title_size, legend_text_size,
    legend_side, legend_dir
  )

  plotly_object <- tryCatch(
    ggplotly(plot),
    error = function(e) {
      debug_log(paste("build_sampleids_num_plot: ggplotly failed:", e$message), 1)
      NULL
    }
  )

  debug_log("build_sampleids_num_plot: numeric plot built", 1)
  list(ggplot_object = plot, plotly_object = plotly_object)
}


# ------------------------------------------------------------------------------
# 7. Session-state plot request helpers
# ------------------------------------------------------------------------------

#' Build a lightweight, serializable Sample IDs plot request.
#'
#' @param ui_values Named list of UI values captured for the plot.
#' @return Named list describing how to rebuild the plot without plot objects.
build_sampleids_plot_request <- function(ui_values = list()) {
  list(
    data_source = as.character(ui_values$FileSample_SampleIDTab %||% "")[1],
    data_type = as.character(ui_values$data_SampleIDTab %||% "")[1],
    character_options = list(
      sort_values = as.character(ui_values$Sort_SampleIDTab %||% character()),
      scale = as.character(ui_values$AbsRel_SampleIDTab %||% "Absolute")[1]
    ),
    numeric_options = list(
      plot_type = as.character(ui_values$NumericPlotType_SampleIDTab %||% "Boxplot")[1],
      transform = as.character(ui_values$Transform_SampleIDTab %||% "none")[1]
    ),
    labels = list(
      label_mode = as.character(ui_values$label_SampleIDTab %||% "Sample name")[1],
      title = as.character(ui_values$plotTitle_SampleIDTab %||% "Sample IDs")[1],
      hide_title = isTRUE(ui_values$hideTitle_SampleIDTab),
      title_size = ui_values$TitleSize_SampleIDTab,
      axis_title_size = ui_values$AxisTitleSize_SampleIDTab,
      tick_size = ui_values$tickSize_SampleIDTab,
      legend_title_size = ui_values$LegendTitleSize_SampleIDTab,
      legend_text_size = ui_values$LegendTextSize_SampleIDTab,
      legend_position = as.character(ui_values$sampleIDs_legend_position %||% "none")[1]
    ),
    color_theme = list(
      palette = as.character(ui_values$col_SampleIDTab %||% "Viridis")[1],
      reverse = isTRUE(ui_values$col_reverse_SampleIDTab),
      theme = as.character(ui_values$ThemeSelect_SampleIDTab %||% "Classic")[1]
    )
  )
}

#' Convert a Sample IDs plot request back to UI values for restore.
#'
#' @param plot_request Named list produced by build_sampleids_plot_request().
#' @return Named list of UI values accepted by regenerate_sampleids_plot().
sampleids_plot_request_to_ui <- function(plot_request = list()) {
  if (!is.list(plot_request)) return(NULL)
  list(
    FileSample_SampleIDTab = plot_request$data_source,
    data_SampleIDTab = plot_request$data_type,
    Sort_SampleIDTab = plot_request$character_options$sort_values,
    AbsRel_SampleIDTab = plot_request$character_options$scale,
    NumericPlotType_SampleIDTab = plot_request$numeric_options$plot_type,
    Transform_SampleIDTab = plot_request$numeric_options$transform,
    label_SampleIDTab = plot_request$labels$label_mode,
    ThemeSelect_SampleIDTab = plot_request$color_theme$theme,
    col_SampleIDTab = plot_request$color_theme$palette,
    col_reverse_SampleIDTab = isTRUE(plot_request$color_theme$reverse),
    plotTitle_SampleIDTab = plot_request$labels$title,
    hideTitle_SampleIDTab = isTRUE(plot_request$labels$hide_title),
    TitleSize_SampleIDTab = plot_request$labels$title_size,
    AxisTitleSize_SampleIDTab = plot_request$labels$axis_title_size,
    tickSize_SampleIDTab = plot_request$labels$tick_size,
    LegendTitleSize_SampleIDTab = plot_request$labels$legend_title_size,
    LegendTextSize_SampleIDTab = plot_request$labels$legend_text_size,
    sampleIDs_legend_position = plot_request$labels$legend_position
  )
}
