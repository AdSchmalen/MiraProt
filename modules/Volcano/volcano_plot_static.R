# ==============================================================================
# volcano_plot_static.R
# ==============================================================================
#
# PURPOSE:
#   Single source of truth for all ggplot2-based static volcano plot creation,
#   styling, theming, label application, and parameter extraction. Consolidates
#   functions for the volcano module's static plot rendering pipeline.
#
# ARCHITECTURAL ROLE:
#   Static Plotting -- pure functions that create and style ggplot2 volcano
#   plots. Functions receive data and parameters as arguments and return
#   plot objects without accessing reactive state directly.
#
# RESPONSIBILITIES:
#   - Plot data preparation and transformation (prepare_plot_data)
#   - Point attribute assignment (color, size, category)
#   - Parameter extraction from Shiny input (extract_plot_parameters,
#     extract_plot_parameters_safe)
#   - Theme selection and application (get_volcano_theme_by_name)
#   - Safe single-plot creation (create_single_volcano_plot_safe)
#   - Live styling updates without data regeneration (apply_live_styling_to_plot)
#   - Enhanced label application with individual colors
#     (apply_all_labels_to_plot_enhanced)
#   - Label data creation (simple and enhanced with per-protein colors)
#   - Default dot color determination based on thresholds
#   - Plot title generation from metadata
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Direct access to reactive state (volcano_state, session, etc.)
#   - Data fetching or column pairing logic (those are in
#     volcano_data_processing.R)
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - volcano_data_processing.R: prepare_volcano_plot_data_safe()
#   External packages:
#     - ggplot2: ggplot, geom_point, geom_hline, geom_vline, theme_*, labs,
#       scale_color_identity, scale_size_identity, scale_x_continuous,
#       scale_y_continuous, expansion, element_text, element_blank, margin,
#       coord_cartesian
#     - ggrepel: geom_text_repel
#     - shiny: req(), %||%
#
# INTERACTIONS:
#   Called by:
#     - volcano_observers.R: plot generation, live styling, label application,
#       parameter extraction
#     - volcano_plot_interactive.R: prepare_plot_data, assign_point_attributes
#   Calls into:
#     - volcano_data_processing.R: prepare_volcano_plot_data_safe()
#   Data flow:
#     - IN:  prepared plot data, Shiny input parameters, label data frames
#     - OUT: ggplot2 plot objects, parameter lists, label data frames
#
# LAST UPDATED: 2026-03-10
# ==============================================================================

# ========================================
# Plot Data Preparation
# ========================================

prepare_plot_data <- function(data, data_def, plot_index, pval_type, debug_log) {

  debug_log(paste("Starting prepare_plot_data with plot_index:", plot_index), 2)
  debug_log(paste("Data dimensions:", nrow(data), "x", ncol(data)), 2)
  debug_log(paste("Data_def dimensions:", nrow(data_def), "x", ncol(data_def)), 2)

  # Validate inputs
  if (is.null(data) || is.null(data_def)) {
    debug_log("ERROR: NULL data or data_def", 1)
    return(NULL)
  }

  if (!is.data.frame(data) || !is.data.frame(data_def)) {
    debug_log("ERROR: Invalid data frame structure", 1)
    return(NULL)
  }

  if (nrow(data) == 0 || nrow(data_def) == 0) {
    debug_log("ERROR: Empty data or data_def", 1)
    return(NULL)
  }

  # Check required columns in data_def
  required_cols <- c("Column", "Content")
  missing_cols <- setdiff(required_cols, names(data_def))
  if (length(missing_cols) > 0) {
    debug_log(paste("ERROR: Missing columns in data_def:", paste(missing_cols, collapse = ", ")), 1)
    return(NULL)
  }

  # Find relevant columns
  abundance_cols <- which(grepl("^Abundance Ratio$", data_def$Content))
  debug_log(paste("Found abundance columns at indices:", paste(abundance_cols, collapse = ", ")), 2)

  if (pval_type == "Adjusted p-value") {
    pval_cols <- which(grepl("Adj.*p-Value$", data_def$Content))
    debug_log(paste("Looking for adjusted p-values, found at indices:", paste(pval_cols, collapse = ", ")), 2)
  } else {
    pval_cols <- which(grepl("p-Value$", data_def$Content) &
                         !grepl("Adj", data_def$Content))
    debug_log(paste("Looking for regular p-values, found at indices:", paste(pval_cols, collapse = ", ")), 2)
  }

  # Validate plot_index against available columns
  if (length(abundance_cols) == 0) {
    debug_log("ERROR: No abundance ratio columns found", 1)
    return(NULL)
  }

  if (length(pval_cols) == 0) {
    debug_log("ERROR: No p-value columns found", 1)
    return(NULL)
  }

  if (plot_index > length(abundance_cols)) {
    debug_log(paste("ERROR: plot_index", plot_index, "exceeds abundance columns count", length(abundance_cols)), 1)
    return(NULL)
  }

  if (plot_index > length(pval_cols)) {
    debug_log(paste("ERROR: plot_index", plot_index, "exceeds p-value columns count", length(pval_cols)), 1)
    return(NULL)
  }

  # Get column indices
  abundance_col_idx <- abundance_cols[plot_index]
  pval_col_idx <- pval_cols[plot_index]

  debug_log(paste("Using abundance column index:", abundance_col_idx), 2)
  debug_log(paste("Using p-value column index:", pval_col_idx), 2)

  # Validate column indices against data dimensions
  if (abundance_col_idx > ncol(data)) {
    debug_log(paste("ERROR: abundance_col_idx", abundance_col_idx, "exceeds data columns", ncol(data)), 1)
    return(NULL)
  }

  if (pval_col_idx > ncol(data)) {
    debug_log(paste("ERROR: pval_col_idx", pval_col_idx, "exceeds data columns", ncol(data)), 1)
    return(NULL)
  }

  # Extract column names for verification
  abundance_col_name <- if (abundance_col_idx <= ncol(data)) colnames(data)[abundance_col_idx] else "INVALID"
  pval_col_name <- if (pval_col_idx <= ncol(data)) colnames(data)[pval_col_idx] else "INVALID"

  debug_log(paste("Abundance column name:", abundance_col_name), 2)
  debug_log(paste("P-value column name:", pval_col_name), 2)

  # Extract and validate data
  tryCatch({
    abundance_data <- data[, abundance_col_idx]
    pval_data <- data[, pval_col_idx]

    debug_log(paste("Abundance data range:", min(abundance_data, na.rm = TRUE), "to", max(abundance_data, na.rm = TRUE)), 2)
    debug_log(paste("P-value data range:", min(pval_data, na.rm = TRUE), "to", max(pval_data, na.rm = TRUE)), 2)

  }, error = function(e) {
    debug_log(paste("ERROR extracting data:", e$message), 1)
    return(NULL)
  })

  # Create initial plot dataframe
  plot_df <- data.frame(
    x = abundance_data,
    y = pval_data,
    stringsAsFactors = FALSE
  )

  # Robust numeric conversion (restored sessions can carry character/factor columns)
  if (!is.numeric(plot_df$x)) {
    x_numeric <- suppressWarnings(as.numeric(as.character(plot_df$x)))
    na_introduced_x <- sum(is.na(x_numeric) & !is.na(plot_df$x))
    if (na_introduced_x > 0) {
      debug_log(paste("Abundance numeric conversion introduced", na_introduced_x, "NA values"), 1)
    }
    plot_df$x <- x_numeric
  }

  if (!is.numeric(plot_df$y)) {
    y_numeric <- suppressWarnings(as.numeric(as.character(plot_df$y)))
    na_introduced_y <- sum(is.na(y_numeric) & !is.na(plot_df$y))
    if (na_introduced_y > 0) {
      debug_log(paste("P-value numeric conversion introduced", na_introduced_y, "NA values"), 1)
    }
    plot_df$y <- y_numeric
  }

  # Handle transformations safely
  tryCatch({
    # Check if Transformation column exists and has sufficient length
    has_transformation <- "Transformation" %in% names(data_def) &&
      nrow(data_def) >= max(abundance_col_idx, pval_col_idx)

    if (has_transformation) {
      debug_log("Transformation column available", 2)

      # Handle abundance ratio transformation
      abundance_transform <- data_def$Transformation[abundance_col_idx]
      if (!is.na(abundance_transform) && nzchar(abundance_transform)) {
        debug_log(paste("Abundance transformation:", abundance_transform), 2)

        if (abundance_transform != "log2") {
          # Retransform first if needed
          if (abundance_transform != "none") {
            plot_df$x <- switch(abundance_transform,
                                "log10" = 10^plot_df$x,
                                "-log10" = 10^(-plot_df$x),
                                "-log2" = 2^(-plot_df$x),
                                plot_df$x)
            plot_df$x <- suppressWarnings(log2(plot_df$x))
          } else {
            finite_x <- plot_df$x[is.finite(plot_df$x)]
            if (length(finite_x) > 0 && any(finite_x <= 0, na.rm = TRUE)) {
              debug_log("Abundance has <=0 with transform='none'; treating as already log-scale", 1)
            } else {
              plot_df$x <- suppressWarnings(log2(plot_df$x))
            }
          }
        }
      } else {
        finite_x <- plot_df$x[is.finite(plot_df$x)]
        if (length(finite_x) > 0 && any(finite_x <= 0, na.rm = TRUE)) {
          debug_log("No abundance transform but values <=0; treating as already log-scale", 1)
        } else {
          debug_log("No abundance transformation, applying log2", 2)
          plot_df$x <- suppressWarnings(log2(plot_df$x))
        }
      }

      # Handle p-value transformation
      pval_transform <- data_def$Transformation[pval_col_idx]
      if (!is.na(pval_transform) && nzchar(pval_transform)) {
        debug_log(paste("P-value transformation:", pval_transform), 2)

        if (pval_transform != "-log10") {
          # Retransform first if needed
          if (pval_transform != "none") {
            plot_df$y <- switch(pval_transform,
                                "log10" = 10^plot_df$y,
                                "log2" = 2^plot_df$y,
                                "-log2" = 2^(-plot_df$y),
                                plot_df$y)
            plot_df$y <- suppressWarnings(-log10(plot_df$y))
          } else {
            finite_y <- plot_df$y[is.finite(plot_df$y)]
            if (length(finite_y) > 0 && max(finite_y, na.rm = TRUE) > 1) {
              debug_log("P-values >1 with transform='none'; treating as already -log10 scale", 1)
            } else {
              plot_df$y <- suppressWarnings(-log10(plot_df$y))
            }
          }
        }
      } else {
        finite_y <- plot_df$y[is.finite(plot_df$y)]
        if (length(finite_y) > 0 && max(finite_y, na.rm = TRUE) > 1) {
          debug_log("No p-value transform but values >1; treating as already -log10 scale", 1)
        } else {
          debug_log("No p-value transformation, applying -log10", 2)
          plot_df$y <- suppressWarnings(-log10(plot_df$y))
        }
      }

    } else {
      debug_log("No transformation column, applying default transformations", 2)
      finite_x <- plot_df$x[is.finite(plot_df$x)]
      if (length(finite_x) > 0 && any(finite_x <= 0, na.rm = TRUE)) {
        debug_log("No transform metadata and abundance <=0; treating as already log-scale", 1)
      } else {
        plot_df$x <- suppressWarnings(log2(plot_df$x))
      }

      finite_y <- plot_df$y[is.finite(plot_df$y)]
      if (length(finite_y) > 0 && max(finite_y, na.rm = TRUE) > 1) {
        debug_log("No transform metadata and p-values >1; treating as already -log10 scale", 1)
      } else {
        plot_df$y <- suppressWarnings(-log10(plot_df$y))
      }
    }

  }, error = function(e) {
    debug_log(paste("ERROR in transformations:", e$message), 1)
    # Apply default transformations as fallback
    plot_df$x <- suppressWarnings(log2(abundance_data))
    plot_df$y <- suppressWarnings(-log10(pval_data))
  })

  # Remove non-finite values
  initial_rows <- nrow(plot_df)
  valid_rows <- is.finite(plot_df$x) & is.finite(plot_df$y) &
    plot_df$y > 0 & !is.na(plot_df$x) & !is.na(plot_df$y)
  plot_df <- plot_df[valid_rows, ]

  debug_log(paste("Removed", initial_rows - nrow(plot_df), "invalid rows, remaining:", nrow(plot_df)), 2)

  if (nrow(plot_df) == 0) {
    debug_log("ERROR: No valid data points after filtering", 1)
    return(NULL)
  }

  # Add identifier column if available
  tryCatch({
    if ("Options" %in% names(data_def)) {
      identifier_rows <- which(grepl("Identifier|Symbol|Accession", data_def$Options))
      if (length(identifier_rows) > 0) {
        # Get the corresponding column index
        identifier_col_name <- data_def$Column[identifier_rows[1]]
        identifier_col_idx <- which(colnames(data) == identifier_col_name)

        if (length(identifier_col_idx) > 0) {
          plot_df$identifier <- data[valid_rows, identifier_col_idx[1]]
          debug_log(paste("Added identifier column:", identifier_col_name), 2)
        }
      }
    }
  }, error = function(e) {
    debug_log(paste("Warning: Could not add identifier column:", e$message), 2)
  })

  debug_log(paste("Successfully prepared plot data with", nrow(plot_df), "points"), 2)
  return(plot_df)
}

# ========================================
# Point Attribute Assignment
# ========================================

assign_point_attributes <- function(plot_data, params) {

  # Default attributes
  plot_data$color <- params$color_neutral
  plot_data$size <- params$size_neutral
  plot_data$category <- "neutral"

  # Identify significant points
  sig_mask <- plot_data$y > -log10(params$pval_threshold)

  # Upregulated
  up_mask <- plot_data$x > params$fold_threshold & sig_mask
  plot_data$color[up_mask] <- params$color_up
  plot_data$size[up_mask] <- params$size_up
  plot_data$category[up_mask] <- "up"

  # Downregulated
  down_mask <- plot_data$x < -params$fold_threshold & sig_mask
  plot_data$color[down_mask] <- params$color_down
  plot_data$size[down_mask] <- params$size_down
  plot_data$category[down_mask] <- "down"

  return(plot_data)
}

# ========================================
# Parameter Extraction (Legacy - used by interactive.R)
# ========================================

extract_plot_parameters <- function(input) {

  # Extract all parameters from input
  params <- list(
    # Colors
    color_neutral = input$dotColorInput_Volcano,
    color_up = input$dotColorInputUp_Volcano,
    color_down = input$dotColorInputDown_Volcano,

    # Sizes
    size_neutral = input$dotSizeInput_Volcano,
    size_up = input$dotSizeInputUp_Volcano,
    size_down = input$dotSizeInputDown_Volcano,

    # Thresholds
    pval_threshold = input$pvalueInput_Volcano,
    fold_threshold = input$AbundanceInput_Volcano,

    # Axis limits
    x_limits = input$xLimInput_Volcano,
    y_limits = input$yLimInput_Volcano,
    x_tick = input$xTick_Volcano,
    y_tick = input$yTick_Volcano,

    # Labels
    x_label = expression(log[2]~"fold change"),
    y_label = if (input$pValueSel_Volcano == "Adjusted p-value") {
      expression(-log[10]~"adjusted p-value")
    } else {
      expression(-log[10]~"p-value")
    },

    # Text sizes
    custom_title = input$plotTitle_Volcano,
    hide_title = isTRUE(input$hideTitle_Volcano),
    title_size = as.numeric(input$plotTitleSize_Volcano),
    axis_title_size = as.numeric(input$AxisTitleSize_Volcano),
    tick_size = as.numeric(input$tickSize_Volcano),
    label_size = as.numeric(input$labelSize_Volcano),

    # Label parameters
    label_overlap = as.numeric(input$labelOverlap_Volcano),
    point_padding = as.numeric(input$pointPadding_Volcano),
    line_thickness = input$lineThickness_Volcano,

    # Theme
    theme = get_volcano_theme_by_name(input$ThemeSelect_Volcano),

    # P-value type
    pval_type = input$pValueSel_Volcano
  )

  return(params)
}

# ========================================
# Safe Parameter Extraction (Primary)
# ========================================

extract_plot_parameters_safe <- function(input, debug_log, optimal_settings = NULL, force_optimal_axes = FALSE) {

  debug_log("Extracting plot parameters safely with full UI integration", 2)

  # Fallback ticks and ranges if nothing is available
  default_x_tick <- 2
  default_y_tick <- 2
  default_x_limits <- c(-8, 8)
  default_y_limits <- c(0, 18)

  # If optimal_settings are provided, use those as defaults
  if (!is.null(optimal_settings)) {
    if (!is.null(optimal_settings$x_tick)) default_x_tick <- optimal_settings$x_tick
    if (!is.null(optimal_settings$y_tick)) default_y_tick <- optimal_settings$y_tick
    if (!is.null(optimal_settings$x_range) && length(optimal_settings$x_range) == 2L) {
      default_x_limits <- optimal_settings$x_range
    }
    if (!is.null(optimal_settings$y_range) && length(optimal_settings$y_range) == 2L) {
      default_y_limits <- optimal_settings$y_range
    }
  }

  # SAFE parameter extraction with guaranteed defaults
  params <- list(
    # Colors
    color_neutral = ifelse(!is.null(input$dotColorInput_Volcano), input$dotColorInput_Volcano, "#E0E0E0"),
    color_up = ifelse(!is.null(input$dotColorInputUp_Volcano), input$dotColorInputUp_Volcano, "orange"),
    color_down = ifelse(!is.null(input$dotColorInputDown_Volcano), input$dotColorInputDown_Volcano, "blue"),

    # Sizes
    size_neutral = ifelse(!is.null(input$dotSizeInput_Volcano), input$dotSizeInput_Volcano, 1),
    size_up = ifelse(!is.null(input$dotSizeInputUp_Volcano), input$dotSizeInputUp_Volcano, 1.5),
    size_down = ifelse(!is.null(input$dotSizeInputDown_Volcano), input$dotSizeInputDown_Volcano, 1.5),

    # Thresholds
    pval_threshold = ifelse(!is.null(input$pvalueInput_Volcano), input$pvalueInput_Volcano, 0.05),
    fold_threshold = ifelse(!is.null(input$AbundanceInput_Volcano), input$AbundanceInput_Volcano, 1),

    # Axis limits and ticks
    x_limits = if (isTRUE(force_optimal_axes) && !is.null(optimal_settings)) {
      default_x_limits
    } else if (!is.null(input$xLimInput_Volcano)) {
      input$xLimInput_Volcano
    } else {
      default_x_limits
    },
    y_limits = if (isTRUE(force_optimal_axes) && !is.null(optimal_settings)) {
      default_y_limits
    } else if (!is.null(input$yLimInput_Volcano)) {
      input$yLimInput_Volcano
    } else {
      default_y_limits
    },
    x_tick = if (isTRUE(force_optimal_axes) && !is.null(optimal_settings)) {
      default_x_tick
    } else if (!is.null(input$xTick_Volcano)) {
      input$xTick_Volcano
    } else {
      default_x_tick
    },
    y_tick = if (isTRUE(force_optimal_axes) && !is.null(optimal_settings)) {
      default_y_tick
    } else if (!is.null(input$yTick_Volcano)) {
      input$yTick_Volcano
    } else {
      default_y_tick
    },

    # Plot Title
    custom_title = ifelse(!is.null(input$plotTitle_Volcano) && nzchar(trimws(input$plotTitle_Volcano)),
                          trimws(input$plotTitle_Volcano),
                          ""),
    hide_title = isTRUE(input$hideTitle_Volcano),

    # Theme
    theme_name = ifelse(!is.null(input$ThemeSelect_Volcano), input$ThemeSelect_Volcano, "Black and White"),

    # P-Value Type
    pval_type = ifelse(!is.null(input$pValueSel_Volcano), input$pValueSel_Volcano, "p-value"),

    # Text sizes
    title_size = ifelse(!is.null(input$plotTitleSize_Volcano), as.numeric(input$plotTitleSize_Volcano), 14),
    axis_title_size = ifelse(!is.null(input$AxisTitleSize_Volcano), as.numeric(input$AxisTitleSize_Volcano), 12),
    tick_size = ifelse(!is.null(input$tickSize_Volcano), as.numeric(input$tickSize_Volcano), 10),

    # Labels - set to FALSE to avoid errors
    show_labels = FALSE,
    label_size = 3,
    label_color = "black"
  )

  debug_log(sprintf(
    "Volcano UI params | theme=%s | pvalue_type=%s | pvalue=%s | fold=%s | custom_title=%s",
    params$theme_name,
    params$pval_type,
    params$pval_threshold,
    params$fold_threshold,
    if (nzchar(params$custom_title)) "yes" else "no"
  ), 2)

  if (isTRUE(getOption("miraprot.volcano.ui_param_diagnostics", FALSE))) {
    debug_log("Volcano UI parameter diagnostics:", 2)
    debug_log(sprintf("  Custom Title: %s", if (nzchar(params$custom_title)) params$custom_title else "<empty>"), 2)
    debug_log(sprintf("  Theme: %s", params$theme_name), 2)
    debug_log(sprintf("  P-Value Type: %s", params$pval_type), 2)
    debug_log(sprintf("  Thresholds - pval: %s fold: %s", params$pval_threshold, params$fold_threshold), 2)
  }

  return(params)
}

# ========================================
# Theme Selection
# ========================================

get_volcano_theme_by_name <- function(theme_name) {

  switch(theme_name,
         "Gray" = theme_gray(),
         "Black and White" = theme_bw(),
         "Linedraw" = theme_linedraw(),
         "Light" = theme_light(),
         "Dark" = theme_dark(),
         "Minimal" = theme_minimal(),
         "Classic" = theme_classic(),
         "Void" = theme_void(),
         theme_bw()  # default fallback
  )
}

# ========================================
# Safe Single Plot Creation
# ========================================

create_single_volcano_plot_safe <- function(data, data_def, ratio_idx, pval_idx,
                                            plot_title, plot_params, debug_log) {

  debug_log(paste("Creating plot with UI parameters - Title: '", plot_params$custom_title,
                  "' Theme:", plot_params$theme_name), 2)

  # Prepare plot data
  plot_data <- prepare_volcano_plot_data_safe(data, data_def, ratio_idx, pval_idx, debug_log)

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    debug_log("No valid data for plotting", 1)
    return(NULL)
  }

  debug_log(paste("Plot data prepared with", nrow(plot_data), "points"), 2)

  # Assign point categories and sizes
  plot_data$category <- "neutral"
  plot_data$point_color <- plot_params$color_neutral
  plot_data$point_size <- plot_params$size_neutral

  # Upregulated: above fold threshold AND above significance threshold
  up_mask <- plot_data$x > plot_params$fold_threshold &
    plot_data$y > -log10(plot_params$pval_threshold)
  plot_data$category[up_mask] <- "up"
  plot_data$point_color[up_mask] <- plot_params$color_up
  plot_data$point_size[up_mask] <- plot_params$size_up

  # Downregulated: below fold threshold AND above significance threshold
  down_mask <- plot_data$x < -plot_params$fold_threshold &
    plot_data$y > -log10(plot_params$pval_threshold)
  plot_data$category[down_mask] <- "down"
  plot_data$point_color[down_mask] <- plot_params$color_down
  plot_data$point_size[down_mask] <- plot_params$size_down

  category_counts <- table(plot_data$category)
  debug_log(paste("Assigned categories:",
                  category_counts["up"], "up,",
                  category_counts["down"], "down,",
                  category_counts["neutral"], "neutral"), 2)

  # Determine final plot title
  final_title <- if (isTRUE(plot_params$hide_title)) {
    NULL
  } else if (!is.null(plot_params$custom_title) && nzchar(plot_params$custom_title)) {
    plot_params$custom_title
  } else {
    plot_title
  }

  debug_log(paste("Using plot title:", final_title), 2)

  # Create base plot with correct sizing
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(aes(color = point_color, size = point_size),
               alpha = 0.6) +
    scale_color_identity() +  # Use actual color values
    scale_size_identity() +   # Use actual size values
    labs(
      title = final_title,
      x = expression(log[2]("Fold Change")),
      y = if (plot_params$pval_type == "Adjusted p-value") {
        expression(-log[10]("Adjusted p-value"))
      } else {
        expression(-log[10]("p-value"))
      }
    )

  debug_log("Base plot created with custom title and axis labels", 2)

  # Apply theme
  volcano_theme <- get_volcano_theme_by_name(plot_params$theme_name)
  p <- p + volcano_theme

  debug_log(paste("Applied theme:", plot_params$theme_name), 2)

  # Apply text sizes and styling
  p <- p + theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",  # Remove legend since we use identity scales
    plot.title = if (isTRUE(plot_params$hide_title)) element_blank() else element_text(size = plot_params$title_size, hjust = 0.5),
    axis.title = element_text(size = plot_params$axis_title_size),
    axis.text = element_text(size = plot_params$tick_size)
  )

  # Add threshold lines
  p <- p +
    geom_hline(yintercept = -log10(plot_params$pval_threshold),
               linetype = "dashed", color = "darkgray", alpha = 0.7) +
    geom_vline(xintercept = c(-plot_params$fold_threshold, plot_params$fold_threshold),
               linetype = "dashed", color = "darkgray", alpha = 0.7)

  return(p)
}

#' Rebuild a volcano ggplot from saved lightweight plot data.
#'
#' Session schema 2.0 intentionally persists only tabular plot data and plot
#' requests, never ggplot/plotly/rendered plot objects.  This helper provides a
#' canonical way to turn restored `plot_data_by_title` entries back into a
#' normal static volcano plot before labels are applied.
create_volcano_plot_from_saved_data <- function(plot_data, plot_title, plot_params, debug_log) {
  if (!is.data.frame(plot_data) || nrow(plot_data) == 0L || !all(c("x", "y") %in% names(plot_data))) {
    debug_log(paste("Saved plot data unavailable for", plot_title), 1)
    return(NULL)
  }

  base_plot <- ggplot(plot_data, aes(x = x, y = y))
  apply_live_styling_to_plot(base_plot, plot_params, debug_log, plot_title = plot_title)
}

# ========================================
# Live Styling Application
# ========================================

apply_live_styling_to_plot <- function(base_plot, params, debug_log, plot_title = NULL) {

  debug_log("Applying live styling to single plot", 2)

  tryCatch({

    # Get the original plot data
    plot_data <- base_plot$data

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      debug_log("No plot data available for live styling", 2)
      return(base_plot)
    }

    # Update point colors and sizes based on current thresholds
    plot_data$category <- "neutral"
    plot_data$point_color <- params$color_neutral
    plot_data$point_size <- params$size_neutral

    # Apply current thresholds
    up_mask <- plot_data$x > params$fold_threshold &
      plot_data$y > -log10(params$pval_threshold)
    plot_data$category[up_mask] <- "up"
    plot_data$point_color[up_mask] <- params$color_up
    plot_data$point_size[up_mask] <- params$size_up

    down_mask <- plot_data$x < -params$fold_threshold &
      plot_data$y > -log10(params$pval_threshold)
    plot_data$category[down_mask] <- "down"
    plot_data$point_color[down_mask] <- params$color_down
    plot_data$point_size[down_mask] <- params$size_down

    # Determine final title
    final_title <- if (isTRUE(params$hide_title)) {
      NULL
    } else if (!is.null(params$custom_title) && nzchar(params$custom_title)) {
      params$custom_title
    } else {
      plot_title %||% base_plot$labels$title %||% "Volcano Plot"
    }

    # Rebuild plot with updated styling
    updated_plot <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point(aes(color = point_color, size = point_size), alpha = 0.6) +
      scale_color_identity() +
      scale_size_identity() +
      labs(
        title = final_title,
        x = expression(log[2]("Fold Change")),
        y = if (params$pval_type == "Adjusted p-value") {
          expression(-log[10]("Adjusted p-value"))
        } else {
          expression(-log[10]("p-value"))
        }
      )

    # Apply theme
    volcano_theme <- get_volcano_theme_by_name(params$theme_name)
    updated_plot <- updated_plot + volcano_theme

    # Apply styling
    updated_plot <- updated_plot + theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = if (isTRUE(params$hide_title)) element_blank() else element_text(size = params$title_size, hjust = 0.5),
      axis.title = element_text(size = params$axis_title_size),
      axis.text = element_text(size = params$tick_size)
    )

    # Add threshold lines
    updated_plot <- updated_plot +
      geom_hline(yintercept = -log10(params$pval_threshold),
                 linetype = "dashed", color = "darkgray", alpha = 0.7) +
      geom_vline(xintercept = c(-params$fold_threshold, params$fold_threshold),
                 linetype = "dashed", color = "darkgray", alpha = 0.7)

    # Apply axis limits and ticks
    tryCatch({
      x_breaks <- seq(params$x_limits[1], params$x_limits[2], by = params$x_tick)
      y_breaks <- seq(params$y_limits[1], params$y_limits[2], by = params$y_tick)

      updated_plot <- updated_plot +
        scale_x_continuous(
          limits = c(params$x_limits[1], params$x_limits[2]),
          breaks = x_breaks,
          expand = expansion(mult = 0.02)
        ) +
        scale_y_continuous(
          limits = c(params$y_limits[1], params$y_limits[2]),
          breaks = y_breaks,
          expand = expansion(mult = 0.02)
        )

    }, error = function(e) {
      debug_log(paste("Warning: Could not apply axis limits:", e$message), 2)
    })

    debug_log("Live styling applied successfully", 2)
    return(updated_plot)

  }, error = function(e) {
    debug_log(paste("Error in live styling:", e$message), 1)
    return(base_plot)  # Return original plot on error
  })
}

# ========================================
# Enhanced Label Application
# ========================================

apply_all_labels_to_plot_enhanced <- function(base_plot, labels_df, input, debug_log, labeled_dot_size = 2) {
  if (is.null(labels_df) || nrow(labels_df) == 0) {
    debug_log("No labels to apply", 2)
    return(base_plot)
  }

  # Get labeling settings (use individual colors, not global label color)
  max_overlaps <- input$maxOverlaps_Volcano %||% 10
  label_distance <- input$labelDistance_Volcano %||% 0.25
  line_thickness <- input$lineThickness_Volcano_Label %||% 0.5
  label_size <- as.numeric(input$labelSize_Volcano %||% 8)

  debug_log(paste("Applying", nrow(labels_df), "enhanced labels with individual colors and dot size", labeled_dot_size), 2)

  if (!"x_plot" %in% colnames(labels_df)) labels_df$x_plot <- log2(labels_df$Abundance_Ratio)
  if (!"y_plot" %in% colnames(labels_df)) labels_df$y_plot <- -log10(labels_df$Adjusted_p_Value)

  # Check if we have custom dot colors
  has_custom_dots <- "UseCustomDotColor" %in% colnames(labels_df) &&
    "CustomDotColor" %in% colnames(labels_df)

  if (has_custom_dots) {
    # Split into custom and default dot colors
    custom_dots <- labels_df[labels_df$UseCustomDotColor == TRUE & !is.na(labels_df$CustomDotColor), ]
    default_dots <- labels_df[labels_df$UseCustomDotColor == FALSE | is.na(labels_df$CustomDotColor), ]

    # Add default colored dots first
    if (nrow(default_dots) > 0) {
      debug_log(paste("Adding", nrow(default_dots), "proteins with default dot color and labeled size", labeled_dot_size), 2)
      base_plot <- base_plot +
        geom_point(
          data = default_dots,
          aes(x = x_plot, y = y_plot),
          color = "#E0E0E0",
          size = labeled_dot_size,
          alpha = 0.8
        )
    }

    # Add custom colored dots
    if (nrow(custom_dots) > 0) {
      debug_log(paste("Adding", nrow(custom_dots), "proteins with custom dot colors and labeled size", labeled_dot_size), 2)
      base_plot <- base_plot +
        geom_point(
          data = custom_dots,
          aes(x = x_plot, y = y_plot),
          color = custom_dots$CustomDotColor,
          size = labeled_dot_size,
          alpha = 0.8
        )
    }
  } else {
    # No custom color data, add all dots with default color and labeled size
    debug_log(paste("Adding ALL", nrow(labels_df), "proteins with default dot color and labeled size", labeled_dot_size), 2)
    base_plot <- base_plot +
      geom_point(
        data = labels_df,
        aes(x = x_plot, y = y_plot),
        color = "#E0E0E0",
        size = labeled_dot_size,
        alpha = 0.8
      )
  }

  # Keep labels visible whenever their dot is inside current user limits
  label_x <- labels_df$x_plot
  label_y <- labels_df$y_plot
  x_limits <- input$xLimInput_Volcano %||% range(label_x, na.rm = TRUE)
  y_limits <- input$yLimInput_Volcano %||% range(label_y, na.rm = TRUE)

  visible_rows <- !is.na(label_x) & !is.na(label_y) &
    label_x >= x_limits[1] & label_x <= x_limits[2] &
    label_y >= y_limits[1] & label_y <= y_limits[2]

  visible_labels <- labels_df[visible_rows, , drop = FALSE]
  debug_log(paste("Visible labels in current limits:", nrow(visible_labels), "of", nrow(labels_df)), 2)

  # Add labels using individual colors
  updated_plot <- base_plot
  if (nrow(visible_labels) > 0) {
    updated_plot <- base_plot +
      ggrepel::geom_text_repel(
        data = visible_labels,
        aes(x = x_plot, y = y_plot,
            label = ID, color = I(LabelColor)),
        size = label_size,
        max.overlaps = max_overlaps,
        nudge_x = label_distance,
        nudge_y = label_distance,
        min.segment.length = 0,
        segment.size = line_thickness,
        show.legend = FALSE
      ) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(8, 20, 8, 8))
  }

  debug_log("Enhanced labels applied successfully with visibility-by-dot logic", 2)
  return(updated_plot)
}

# ========================================
# Label Data Creation (Simple)
# ========================================

create_volcano_label_data_simple <- function(proteins_to_label, plot_title, rv, input, volcano_state, debug_log) {
  tryCatch({
    # Get data
    data <- rv$data_mod
    data_def <- rv$data_def
    selected_identifier <- input$Identifier_Volcano

    # Find identifier column
    Identifier_indices <- which(grepl(selected_identifier, data_def$Options, fixed = TRUE))
    if (length(Identifier_indices) == 0) {
      debug_log("Identifier column not found", 1)
      return(NULL)
    }

    # Determine plot index
    plot_titles <- volcano_state$plot_titles
    plot_index <- which(plot_titles == plot_title)
    plot_index <- if (length(plot_index) == 0) 1 else plot_index[1]

    # Get columns
    AbundanceRatio_cols <- which(grepl("^Abundance Ratio$", data_def$Content))
    if (length(AbundanceRatio_cols) < plot_index) return(NULL)

    p_val_name <- if (!is.null(input$pValueSel_Volcano) && input$pValueSel_Volcano == "Adjusted p-value") {
      "Abundance Ratio Adj. p-Value"
    } else {
      "Abundance Ratio p-Value"
    }

    AbundanceRatioP_cols <- which(grepl(p_val_name, data_def$Content))
    if (length(AbundanceRatioP_cols) < plot_index) {
      AbundanceRatioP_cols <- which(grepl("p-Value", data_def$Content))
    }
    if (length(AbundanceRatioP_cols) < plot_index) return(NULL)

    # Create label data
    protein_rows <- data[data[[Identifier_indices[1]]] %in% proteins_to_label, ]
    if (nrow(protein_rows) == 0) return(NULL)

    ratio_idx <- AbundanceRatio_cols[plot_index]
    pval_idx <- AbundanceRatioP_cols[plot_index]

    ratio_values <- suppressWarnings(as.numeric(protein_rows[[ratio_idx]]))
    pval_values <- suppressWarnings(as.numeric(protein_rows[[pval_idx]]))

    # Transform coordinates consistently with prepare_volcano_plot_data_safe
    ratio_transform <- if ("Transformation" %in% names(data_def) && nrow(data_def) >= ratio_idx) data_def$Transformation[ratio_idx] else NA
    pval_transform <- if ("Transformation" %in% names(data_def) && nrow(data_def) >= pval_idx) data_def$Transformation[pval_idx] else NA

    x_plot <- ratio_values
    if (!is.na(ratio_transform) && nzchar(ratio_transform) && ratio_transform != "none") {
      if (ratio_transform != "log2") {
        x_plot <- switch(ratio_transform,
                         "log10" = 10^x_plot,
                         "-log10" = 10^(-x_plot),
                         "-log2" = 2^(-x_plot),
                         x_plot)
        x_plot <- log2(x_plot)
      }
    } else {
      x_plot <- log2(x_plot)
    }

    y_plot <- pval_values
    if (!is.na(pval_transform) && nzchar(pval_transform) && pval_transform != "none") {
      if (pval_transform != "-log10") {
        y_plot <- switch(pval_transform,
                         "log10" = 10^y_plot,
                         "log2" = 2^y_plot,
                         "-log2" = 2^(-y_plot),
                         y_plot)
        y_plot <- -log10(y_plot)
      }
    } else {
      y_plot <- -log10(y_plot)
    }

    labeling_df <- data.frame(
      ID = protein_rows[[Identifier_indices[1]]],
      Abundance_Ratio = ratio_values,
      Adjusted_p_Value = pval_values,
      x_plot = x_plot,
      y_plot = y_plot,
      LabelColor = input$labelColor_Volcano %||% "#000000",
      stringsAsFactors = FALSE
    )

    # Filter valid plotting coordinates
    valid_rows <- is.finite(labeling_df$x_plot) & is.finite(labeling_df$y_plot)

    result <- labeling_df[valid_rows, ]
    debug_log(paste("Created label data for", nrow(result), "proteins"), 2)
    return(result)

  }, error = function(e) {
    debug_log(paste("Error creating label data:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Enhanced Label Data Creation
# ========================================

create_volcano_label_data_enhanced <- function(proteins_to_label, plot_title, rv, input, volcano_state, debug_log, protein_settings = NULL) {
  tryCatch({
    # Get existing function result
    base_result <- create_volcano_label_data_simple(proteins_to_label, plot_title, rv, input, volcano_state, debug_log)

    if (is.null(base_result) || nrow(base_result) == 0) {
      return(base_result)
    }

    # Apply individual color settings if available
    if (!is.null(protein_settings) && nrow(protein_settings) > 0) {
      for (i in seq_len(nrow(base_result))) {
        protein_id <- base_result$ID[i]
        setting_row <- protein_settings[protein_settings$protein_id == protein_id, ]

        if (nrow(setting_row) > 0) {
          # Apply custom label color
          base_result$LabelColor[i] <- setting_row$label_color[1]

          # Add custom dot color if enabled
          if (setting_row$use_custom_dot_color[1]) {
            base_result$CustomDotColor[i] <- setting_row$dot_color[1]
            base_result$UseCustomDotColor[i] <- TRUE
          } else {
            base_result$CustomDotColor[i] <- NA
            base_result$UseCustomDotColor[i] <- FALSE
          }
        }
      }

      debug_log(paste("Applied individual color settings to", nrow(base_result), "labels"), 2)
    }

    return(base_result)

  }, error = function(e) {
    debug_log(paste("Error in enhanced label creation:", e$message), 1)
    return(create_volcano_label_data_simple(proteins_to_label, plot_title, rv, input, volcano_state, debug_log))
  })
}

# ========================================
# Default Dot Color Determination
# ========================================

get_default_dot_colors_for_proteins <- function(proteins, rv, input, debug_log) {
  tryCatch({
    req(rv$data_mod, rv$data_def)

    data <- rv$data_mod
    data_def <- rv$data_def
    selected_identifier <- input$Identifier_Volcano

    # Find identifier column
    Identifier_indices <- which(grepl(selected_identifier, data_def$Options, fixed = TRUE))
    if (length(Identifier_indices) == 0) {
      debug_log("Cannot determine default colors - identifier not found", 2)
      return(rep("#E0E0E0", length(proteins)))
    }

    # Get current plot parameters
    fold_threshold <- input$AbundanceInput_Volcano %||% 1
    pval_threshold <- input$pvalueInput_Volcano %||% 0.05

    # Get colors from input
    color_neutral <- input$dotColorInput_Volcano %||% "#E0E0E0"
    color_up <- input$dotColorInputUp_Volcano %||% "orange"
    color_down <- input$dotColorInputDown_Volcano %||% "blue"

    # Determine plot index (assume first plot for simplicity)
    plot_index <- 1

    # Get abundance and p-value columns
    AbundanceRatio_cols <- which(grepl("^Abundance Ratio$", data_def$Content))

    p_val_name <- if (!is.null(input$pValueSel_Volcano) && input$pValueSel_Volcano == "Adjusted p-value") {
      "Abundance Ratio Adj. p-Value"
    } else {
      "Abundance Ratio p-Value"
    }
    AbundanceRatioP_cols <- which(grepl(p_val_name, data_def$Content))

    if (length(AbundanceRatio_cols) < plot_index || length(AbundanceRatioP_cols) < plot_index) {
      debug_log("Cannot determine default colors - data columns not found", 2)
      return(rep(color_neutral, length(proteins)))
    }

    # Calculate default colors for each protein
    default_colors <- sapply(proteins, function(protein) {
      protein_row <- data[data[[Identifier_indices[1]]] == protein, ]

      if (nrow(protein_row) == 0) {
        return(color_neutral)
      }

      abundance_ratio <- as.numeric(protein_row[[AbundanceRatio_cols[plot_index]]])
      p_value <- as.numeric(protein_row[[AbundanceRatioP_cols[plot_index]]])

      if (is.na(abundance_ratio) || is.na(p_value)) {
        return(color_neutral)
      }

      # Apply threshold logic
      log2_fc <- log2(abundance_ratio)
      log10_pval <- -log10(p_value)

      if (log10_pval > -log10(pval_threshold)) {
        if (log2_fc > fold_threshold) {
          return(color_up)
        } else if (log2_fc < -fold_threshold) {
          return(color_down)
        }
      }

      return(color_neutral)
    })

    debug_log(paste("Calculated default colors for", length(proteins), "proteins"), 2)
    return(default_colors)

  }, error = function(e) {
    debug_log(paste("Error calculating default colors:", e$message), 1)
    return(rep("#E0E0E0", length(proteins)))
  })
}

# ========================================
# Plot Title Generation
# ========================================

generate_plot_titles_robust <- function(data_def, debug_log) {

  debug_log("Generating plot titles", 2)

  tryCatch({
    abundance_cols <- which(grepl("^Abundance Ratio$", data_def$Content))

    if (length(abundance_cols) == 0) {
      debug_log("No abundance ratio columns found for titles", 1)
      return(character(0))
    }

    plot_titles <- sapply(abundance_cols, function(i) {
      if (i <= nrow(data_def) && "Column" %in% names(data_def)) {
        col_name <- data_def$Column[i]
        title <- gsub("^Abundance Ratio:\\s*", "", col_name)
        title <- gsub("Abundance_Ratio:_", "", title)
        return(title)
      } else {
        return(paste("Plot", i))
      }
    })

    debug_log(paste("Generated titles:", paste(plot_titles, collapse = ", ")), 2)
    return(as.character(plot_titles))

  }, error = function(e) {
    debug_log(paste("Error in plot title generation:", e$message), 1)
    return(c("Volcano Plot"))
  })
}
