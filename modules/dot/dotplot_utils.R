# ==============================================================================
# dotplot_utils.R - Dotplot plotting and data utility functions
#
# Purpose: Provides reusable utility functions for data handling, axis labeling,
# transformations, threshold logic, region assignment, and plot construction.
#
# Structure:
#   - Data and metadata helpers: Column detection and label generation
#   - Transformation helpers: Axis/data transformations and formatting
#   - Plot builders: ggplot and plotly creation pipelines
#   - Styling utilities: Threshold, region, and labeling style application
#   - Export helpers: Download and display support functions
#
# Dependencies: ggplot2, dplyr, plotly, scales, ggrepel
# Called by: modules/dotplot_module.R and dotplot server submodules
# ==============================================================================


# Minimum p-value used for -log10 transformations. Clamping at 1e-15 keeps
# zero p-values finite (-log10 = 15) while preserving NA values until later
# finite-value filtering.
DOTPLOT_MIN_PVALUE <- 1e-15
DOTPLOT_SESSION_SCHEMA_VERSION <- "2.0"

dotplot_clamp_pvalues <- function(values) {
  safe_values <- values
  finite_idx <- !is.na(safe_values)
  safe_values[finite_idx] <- pmin(pmax(safe_values[finite_idx], DOTPLOT_MIN_PVALUE), 1)
  safe_values
}

dotplot_build_cache_key <- function(plot_title = NULL) {
  .build_canonical_plot_cache_key(
    module = "dotplot",
    logical_plot_id = "main",
    variant = "main"
  )
}

# Normalize cache preprocessing around saved plot intent. A configuration-only
# snapshot has no cache identity to validate, so stale cache fields represent
# "none", rather than a miss or malformed identity.
dotplot_preprocess_restore_cache <- function(module_state, plot_data_cache_pool = list()) {
  if (!is.list(module_state)) return(module_state)
  if (!isTRUE(module_state$plot_ready)) {
    module_state$restore_plot_data_cache <- NULL
    module_state$restore_plot_data_cache_by_title <- NULL
    module_state$restore_cache_resolved <- FALSE
    module_state$restore_cache_degraded <- FALSE
    module_state$restore_cache_degraded_reason <- NULL
    module_state$restore_cache_resolution_mode <- "none"
    return(module_state)
  }

  # True intent retains strict shared cache-key and by-title validation.
  .resolve_plot_data_cache_for_module(module_state, plot_data_cache_pool)
}

dotplot_capture_ui_inputs <- function(input, input_ids) {
  vals <- lapply(input_ids, function(id) isolate(input[[id]]))
  names(vals) <- input_ids
  vals
}

dotplot_capture_plot_ui_cache <- function(input, dotplot_state, region_configs = NULL, region_structure = NULL) {
  natural_ranges <- tryCatch(isolate(dotplot_state$natural_axis_ranges), error = function(e) NULL)
  x_transform <- tryCatch(isolate(dotplot_state$axis_config$x_transform %||% "raw"), error = function(e) "raw")
  y_transform <- tryCatch(isolate(dotplot_state$axis_config$y_transform %||% "raw"), error = function(e) "raw")

  slider_state <- function(range, transform) {
    if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) return(NULL)
    cfg <- expand_range_for_slider_with_ticks(range, transform)
    list(min = cfg$min, max = cfg$max, step = cfg$step, decimals = cfg$decimals,
         tick_interval = cfg$tick_interval)
  }

  list(
    axes = list(
      x_axis_column = tryCatch(isolate(dotplot_state$axis_config$x_col), error = function(e) NULL),
      y_axis_column = tryCatch(isolate(dotplot_state$axis_config$y_col), error = function(e) NULL),
      x_transform = x_transform,
      y_transform = y_transform,
      x_axis_label = tryCatch(isolate(dotplot_state$axis_config$x_label %||% input$x_axis_label), error = function(e) NULL),
      y_axis_label = tryCatch(isolate(dotplot_state$axis_config$y_label %||% input$y_axis_label), error = function(e) NULL)
    ),
    axis_ranges = list(
      x_axis_range = tryCatch(isolate(input$x_axis_range), error = function(e) NULL),
      y_axis_range = tryCatch(isolate(input$y_axis_range), error = function(e) NULL),
      natural_x_range = natural_ranges$x_range %||% NULL,
      natural_y_range = natural_ranges$y_range %||% NULL
    ),
    axis_ticks = list(
      x_tick_interval = tryCatch(isolate(input$x_tick_interval), error = function(e) NULL),
      y_tick_interval = tryCatch(isolate(input$y_tick_interval), error = function(e) NULL),
      x_axis_slider_state = slider_state(natural_ranges$x_range %||% NULL, x_transform),
      y_axis_slider_state = slider_state(natural_ranges$y_range %||% NULL, y_transform)
    ),
    region_styling = list(
      region_configs = tryCatch(if (is.function(region_configs)) isolate(region_configs()) else list(), error = function(e) list()),
      selected_region = NULL,
      region_structure = tryCatch(if (is.function(region_structure)) isolate(region_structure()) else NULL, error = function(e) NULL)
    )
  )
}

dotplot_flatten_plot_ui_cache_for_restore <- function(plot_ui_cache) {
  if (!is.list(plot_ui_cache)) return(NULL)
  flat_ids <- c(
    "x_axis_column", "y_axis_column", "x_transform", "y_transform",
    "x_axis_label", "y_axis_label", "x_axis_range", "y_axis_range",
    "x_tick_interval", "y_tick_interval"
  )
  if (length(intersect(names(plot_ui_cache) %||% character(), flat_ids)) > 0L) {
    return(plot_ui_cache)
  }
  c(
    plot_ui_cache$axes %||% list(),
    plot_ui_cache$axis_ranges %||% list(),
    plot_ui_cache$axis_ticks %||% list()
  )
}

# ========================================
# Data Processing Functions
# ========================================

dotplot_is_text_identifier_content <- function(content) {
  if (is.null(content) || length(content) == 0 || is.na(content)) return(FALSE)

  grepl(
    "Identifier|Gene Symbol|Protein Name|Description|Accession|Name|Symbol|ID$|Annotation|Comment|Sequence",
    as.character(content),
    ignore.case = TRUE
  )
}

dotplot_is_quantitative_content <- function(content) {
  if (is.null(content) || length(content) == 0 || is.na(content)) return(FALSE)

  !dotplot_is_text_identifier_content(content) &&
    grepl(
      paste(
        c(
          "Abundance", "Ratio", "p[-_ ]?Value", "Adj\\.? p[-_ ]?Value", "q[-_ ]?Value",
          "Intensity", "Score", "Confidence", "Fold", "FDR", "Average", "Count",
          "Expression", "Basemean", "Mean", "Median", "Log"
        ),
        collapse = "|"
      ),
      as.character(content),
      ignore.case = TRUE
    )
}

dotplot_column_is_numeric <- function(data, column_name) {
  is.data.frame(data) &&
    !is.null(column_name) &&
    length(column_name) == 1 &&
    column_name %in% names(data) &&
    is.numeric(data[[column_name]])
}

dotplot_get_numeric_columns <- function(data_def, data = NULL) {
  # Return only columns that are actually numeric in the loaded data. Metadata
  # Content is used to prioritize quantitative columns, not to admit text values.

  if (is.null(data_def) || nrow(data_def) == 0) {
    dotplot_debug_log("No metadata available for column detection", 1)
    return(character())
  }

  if (!all(c("Column", "Content") %in% names(data_def))) {
    dotplot_debug_log("Metadata is missing Column/Content fields for axis choice detection", 1)
    return(character())
  }

  if (!is.data.frame(data)) {
    dotplot_debug_log("Loaded data unavailable for numeric axis choice detection", 1)
    return(character())
  }

  all_cols <- as.character(data_def$Column)
  content <- as.character(data_def$Content)

  numeric_idx <- which(vapply(all_cols, function(col) dotplot_column_is_numeric(data, col), logical(1)))
  if (length(numeric_idx) == 0) {
    dotplot_debug_log("No actual numeric columns found in loaded data", 1)
    return(character())
  }

  quantitative <- vapply(content, dotplot_is_quantitative_content, logical(1))
  obvious_text <- vapply(content, dotplot_is_text_identifier_content, logical(1))

  priority_idx <- intersect(which(quantitative & !obvious_text), numeric_idx)
  secondary_idx <- setdiff(numeric_idx, priority_idx)
  sorted_idx <- c(priority_idx, secondary_idx)

  numeric_cols <- all_cols[sorted_idx]
  names(numeric_cols) <- paste0(numeric_cols, " (", content[sorted_idx], ")")

  dotplot_debug_log(
    paste(
      "Returning", length(numeric_cols), "numeric columns for axis selection;",
      length(priority_idx), "quantitative metadata columns prioritized"
    ),
    2
  )
  return(numeric_cols)
}

dotplot_extract_plot_parameters_with_ui <- function(input) {
  # Extract plot parameters including UI labels

  plot_params <- list(
    plot_title = ifelse(!is.null(input$plot_title) && nzchar(input$plot_title),
                        input$plot_title,
                        "Dot Plot"),
    theme = ifelse(!is.null(input$theme_select), input$theme_select, "Black and White"),
    point_color = "#E0E0E0"
  )

  # Extract UI labels
  ui_labels <- list(
    x_label = input$x_axis_label,
    y_label = input$y_axis_label
  )

  return(list(
    plot_params = plot_params,
    ui_labels = ui_labels
  ))
}

dotplot_normalize_theme_name <- function(theme_name) {
  if (is.null(theme_name) || !nzchar(theme_name)) {
    return("Black and White")
  }

  legacy_map <- c(
    "gray" = "Gray",
    "bw" = "Black and White",
    "classic" = "Classic",
    "minimal" = "Minimal",
    "dark" = "Dark"
  )

  if (theme_name %in% names(legacy_map)) {
    return(legacy_map[[theme_name]])
  }

  theme_name
}

# ========================================
# Data Transformation Functions
# ========================================

dotplot_apply_transform_safe <- function(data_vector, transform_type, axis_name = "axis") {
  # Safe transformation - NEVER MODIFIES INPUT, ALWAYS RETURNS NEW VECTOR

  if (length(data_vector) == 0) {
    dotplot_debug_log(paste("Empty data vector for", axis_name, "transformation"), 1)
    return(NULL)
  }

  # WORK ON A COPY - NEVER MODIFY INPUT
  input_copy <- as.vector(data_vector)  # Explicit copy

  # Remove NA values for analysis (on copy)
  non_na_data <- input_copy[!is.na(input_copy)]

  if (length(non_na_data) == 0) {
    dotplot_debug_log(paste("All values are NA for", axis_name), 1)
    return(NULL)
  }

  tryCatch({
    result <- switch(transform_type,
                     "raw" = {
                       dotplot_debug_log(paste("Applying raw (no) transformation to", axis_name), 2)
                       input_copy  # Return copy unchanged
                     },
                     "log2" = {
                       dotplot_debug_log(paste("Applying log2 transformation to", axis_name), 2)

                       # Check for negative or zero values
                       min_val <- min(non_na_data, na.rm = TRUE)
                       if (min_val <= 0) {
                         dotplot_debug_log(paste("Warning:", axis_name, "contains values <=0, adding offset for log2 transformation"), 2)
                         # Add small offset to avoid log(0) or log(negative) - work on copy
                         offset <- abs(min_val) + 1e-10
                         safe_data <- input_copy + offset  # Creates new vector
                       } else {
                         safe_data <- input_copy  # Use copy
                       }

                       log2(safe_data)  # Returns new vector
                     },
                     "log10" = {
                       dotplot_debug_log(paste("Applying log10 transformation to", axis_name), 2)

                       min_val <- min(non_na_data, na.rm = TRUE)
                       if (min_val <= 0) {
                         dotplot_debug_log(paste("Warning:", axis_name, "contains values <=0, adding offset for log10 transformation"), 2)
                         offset <- abs(min_val) + 1e-10
                         safe_data <- input_copy + offset  # Creates new vector
                       } else {
                         safe_data <- input_copy  # Use copy
                       }

                       log10(safe_data)  # Returns new vector
                     },
                     "neg_log10" = {
                       dotplot_debug_log(paste("Applying -log10 transformation to", axis_name), 2)

                       # For -log10, data should be between 0 and 1 (like p-values)
                       max_val <- max(non_na_data, na.rm = TRUE)
                       min_val <- min(non_na_data, na.rm = TRUE)

                       if (min_val < 0) {
                         dotplot_debug_log(paste("Warning:", axis_name, "contains negative values, clamping to 0"), 2)
                       }
                       if (max_val > 1) {
                         dotplot_debug_log(paste("Warning:", axis_name, "contains values >1, may not be p-values"), 2)
                       }

                       # Clamp values to valid p-value range - creates new vector
                       safe_data <- dotplot_clamp_pvalues(input_copy)
                       -log10(safe_data)  # Returns new vector
                     },
                     {
                       dotplot_debug_log(paste("Unknown transformation type:", transform_type, "for", axis_name), 1)
                       input_copy  # Return copy unchanged
                     }
    )

    # Verify input was not modified
    if (transform_type != "raw") {
    }

    # Check transformation results
    if (transform_type != "raw") {
      infinite_count <- sum(is.infinite(result), na.rm = TRUE)
      new_na_count <- sum(is.na(result)) - sum(is.na(data_vector))

      if (infinite_count > 0) {
        dotplot_debug_log(paste("Warning:", transform_type, "transformation of", axis_name, "produced", infinite_count, "infinite values"), 2)
        # Replace infinite values with NA - creates new vector
        result[is.infinite(result)] <- NA
      }

      if (new_na_count > 0) {
        dotplot_debug_log(paste("Warning:", transform_type, "transformation of", axis_name, "produced", new_na_count, "additional NA values"), 2)
      }

      # Check if we still have enough valid values
      valid_count <- sum(is.finite(result))
      if (valid_count < 3) {
        dotplot_debug_log(paste("Error: Too few valid values after", transform_type, "transformation of", axis_name, ":", valid_count), 1)
        return(NULL)
      }
    }

    return(result)  # Always return new vector

  }, error = function(e) {
    dotplot_debug_log(paste("Error in", transform_type, "transformation of", axis_name, ":", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Plot Generation Functions
# ========================================

dotplot_prepare_plot_data <- function(data, data_def, axis_config) {
  # Prepare data for plotting - ALWAYS STARTS FROM ORIGINAL DATA

  dotplot_debug_log("Preparing dotplot data from source", 2)
  dotplot_debug_log(paste("Transformations - X:", axis_config$x_transform, "Y:", axis_config$y_transform), 2)

  # Resolve axis columns against the actual data by NAME first.
  # Do not rely on data_def row indices, because metadata order can drift
  # across restore/session transitions while column names stay stable.
  x_idx <- which(colnames(data) == axis_config$x_col)
  y_idx <- which(colnames(data) == axis_config$y_col)

  # Legacy fallback: if a direct name match fails, try metadata lookup by
  # column name and then resolve that name in the data columns.
  # IMPORTANT: never use data_def row position as a proxy for data column
  # position; metadata ordering can differ from data column ordering across
  # restore flows and dataset switches.
  if (length(x_idx) == 0 && is.data.frame(data_def) && "Column" %in% names(data_def)) {
    x_meta_idx <- which(data_def$Column == axis_config$x_col)
    if (length(x_meta_idx) > 0) {
      x_name <- as.character(data_def$Column[x_meta_idx[1]])
      x_idx <- which(colnames(data) == x_name)
    }
  }
  if (length(y_idx) == 0 && is.data.frame(data_def) && "Column" %in% names(data_def)) {
    y_meta_idx <- which(data_def$Column == axis_config$y_col)
    if (length(y_meta_idx) > 0) {
      y_name <- as.character(data_def$Column[y_meta_idx[1]])
      y_idx <- which(colnames(data) == y_name)
    }
  }

  if (length(x_idx) == 0 || length(y_idx) == 0) {
    dotplot_debug_log(paste("Axis columns not resolvable in data - X:", axis_config$x_col, "Y:", axis_config$y_col), 1)
    dotplot_debug_log(paste("Available data columns:", paste(colnames(data), collapse = ", ")), 2)
    return(NULL)
  }

  dotplot_debug_log(paste("Resolved data columns - X:", axis_config$x_col, "(idx", x_idx[1], ") Y:", axis_config$y_col, "(idx", y_idx[1], ")"), 2)

  # ALWAYS EXTRACT FRESH COPIES FROM ORIGINAL DATA - NEVER MODIFY ORIGINALS
  x_data_original <- as.vector(data[[x_idx[1]]])  # Explicit copy
  y_data_original <- as.vector(data[[y_idx[1]]])  # Explicit copy

  dotplot_debug_log(paste("Original data types - X:", class(x_data_original)[1], "Y:", class(y_data_original)[1]), 2)
  dotplot_debug_log(paste("Original data samples - X:", paste(head(x_data_original, 3), collapse = ", "), "Y:", paste(head(y_data_original, 3), collapse = ", ")), 2)

  # WORK ON COPIES - NEVER MODIFY ORIGINALS
  x_data_working <- x_data_original
  y_data_working <- y_data_original

  # ROBUST DATA TYPE VALIDATION ON WORKING COPIES
  if (!is.numeric(x_data_working)) {
    dotplot_debug_log(paste("X-axis data type", class(x_data_working)[1], "- attempting numeric conversion"), 2)
    x_data_converted <- suppressWarnings(as.numeric(as.character(x_data_working)))

    if (all(is.na(x_data_converted))) {
      dotplot_debug_log("X-axis data cannot be converted to numeric", 1)
      return(NULL)
    }
    x_data_working <- x_data_converted
    dotplot_debug_log("X-axis conversion successful", 2)
  }

  if (!is.numeric(y_data_working)) {
    dotplot_debug_log(paste("Y-axis data type", class(y_data_working)[1], "- attempting numeric conversion"), 2)
    y_data_converted <- suppressWarnings(as.numeric(as.character(y_data_working)))

    if (all(is.na(y_data_converted))) {
      dotplot_debug_log("Y-axis data cannot be converted to numeric", 1)
      return(NULL)
    }
    y_data_working <- y_data_converted
    dotplot_debug_log("Y-axis conversion successful", 2)
  }

  # Check for sufficient non-NA values before transformation
  valid_x_count <- sum(!is.na(x_data_working))
  valid_y_count <- sum(!is.na(y_data_working))

  if (valid_x_count < 3) {
    dotplot_debug_log(paste("Insufficient valid X-axis data points:", valid_x_count), 1)
    return(NULL)
  }

  if (valid_y_count < 3) {
    dotplot_debug_log(paste("Insufficient valid Y-axis data points:", valid_y_count), 1)
    return(NULL)
  }


  # APPLY TRANSFORMATIONS TO FRESH COPIES - ORIGINALS REMAIN UNTOUCHED
  x_transformed <- dotplot_apply_transform_safe(x_data_working, axis_config$x_transform, "X-axis")
  y_transformed <- dotplot_apply_transform_safe(y_data_working, axis_config$y_transform, "Y-axis")

  if (is.null(x_transformed) || is.null(y_transformed)) {
    dotplot_debug_log("Transformation failed for one or both axes", 1)
    return(NULL)
  }

  # Verify originals are unchanged

  # Create plot dataframe from transformed copies
  plot_df <- data.frame(
    x = x_transformed,
    y = y_transformed,
    row_index = seq_len(nrow(data)),
    stringsAsFactors = FALSE
  )

  # Remove rows with non-finite values
  finite_mask <- is.finite(plot_df$x) & is.finite(plot_df$y)
  valid_count <- sum(finite_mask)

  if (valid_count < 3) {
    dotplot_debug_log(paste("Insufficient valid data points after transformation:", valid_count), 1)
    return(NULL)
  }

  plot_df <- plot_df[finite_mask, ]

  dotplot_debug_log(paste("Prepared", nrow(plot_df), "valid data points for plotting"), 2)
  dotplot_debug_log("Original data remains unchanged after preparation", 2)
  return(plot_df)
}

dotplot_create_base_plot <- function(plot_data, axis_config, plot_params) {
  # Create base ggplot object - RENAMED TO AVOID CONFLICTS

  dotplot_debug_log("Creating base dotplot", 2)

  # Generate axis labels with transformation info
  x_label <- dotplot_transform_label(axis_config$x_col, axis_config$x_transform)
  y_label <- dotplot_transform_label(axis_config$y_col, axis_config$y_transform)

  dotplot_debug_log(paste("Using axis labels - X:", x_label, "Y:", y_label), 2)

  # Create plot
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(
      size = 1,
      alpha = 1,
      color = plot_params$point_color
    ) +
    labs(
      title = dotplot_resolve_plot_title(plot_params),
      x = ifelse(!is.null(axis_config$x_label) && nzchar(axis_config$x_label),
                 axis_config$x_label,
                 dotplot_transform_label(axis_config$x_col, axis_config$x_transform)),
      y = ifelse(!is.null(axis_config$y_label) && nzchar(axis_config$y_label),
                 axis_config$y_label,
                 dotplot_transform_label(axis_config$y_col, axis_config$y_transform))
    )

  dotplot_debug_log("Base dotplot created", 2)
  return(p)
}

# ------------------------------------------------------------------------------
# dotplot_transform_label
# Purpose: Build a readable axis label by combining the selected column name and
#   transformation marker.
# Parameters:
#   - column_name: character - Selected source column name for the axis.
#   - transform_type: character - Transformation key (raw/log2/log10/neg_log10).
# Returns: character axis label for ggplot and plotly displays.
# ------------------------------------------------------------------------------


dotplot_resolve_plot_title <- function(plot_params) {
  hide_title <- isTRUE(plot_params$hide_title)
  if (hide_title) {
    return(NULL)
  }

  title_value <- plot_params$plot_title
  if (is.null(title_value) || !nzchar(trimws(title_value))) {
    return("Dot Plot")
  }

  title_value
}

dotplot_add_thresholds <- function(plot, thresholds) {
  # Add threshold lines to plot with thickness and label size support

  dotplot_debug_log(paste("Adding", length(thresholds), "threshold lines with thickness and label size"), 2)

  for (threshold in thresholds) {
    # Get thickness and label size values (with defaults)
    line_thickness <- threshold$thickness %||% 1
    label_size <- threshold$label_size %||% 3

    dotplot_debug_log(paste("Adding", threshold$type, "threshold at", threshold$value,
                    "with thickness", line_thickness, "and label size", label_size), 2)

    if (threshold$type == "vertical") {
      plot <- plot +
        geom_vline(
          xintercept = threshold$value,
          color = threshold$color,
          linetype = threshold$style,
          size = line_thickness,
          alpha = 0.8
        )

      # Add label if specified with custom size
      if (nzchar(threshold$label)) {
        plot <- plot +
          annotate("text",
                   x = threshold$value,
                   y = Inf,
                   label = threshold$label,
                   hjust = 1.1,
                   vjust = 1.1,
                   color = threshold$color,
                   size = label_size)  # Use custom label size
      }

    } else if (threshold$type == "horizontal") {
      plot <- plot +
        geom_hline(
          yintercept = threshold$value,
          color = threshold$color,
          linetype = threshold$style,
          size = line_thickness,
          alpha = 0.8
        )

      # Add label if specified with custom size
      if (nzchar(threshold$label)) {
        plot <- plot +
          annotate("text",
                   x = Inf,
                   y = threshold$value,
                   label = threshold$label,
                   hjust = 1.1,
                   vjust = -0.5,
                   color = threshold$color,
                   size = label_size)  # Use custom label size
      }
    }
  }

  dotplot_debug_log("All threshold lines applied successfully with custom label sizes", 2)
  return(plot)
}

migrate_threshold_thickness <- function(thresholds) {
  # Add thickness and label_size parameters to existing thresholds that don't have them
  migrated_thickness <- 0
  migrated_label_size <- 0

  for (i in seq_along(thresholds)) {
    if (is.null(thresholds[[i]]$thickness)) {
      thresholds[[i]]$thickness <- 1  # Default thickness
      migrated_thickness <- migrated_thickness + 1
    }
    if (is.null(thresholds[[i]]$label_size)) {
      thresholds[[i]]$label_size <- 3  # Default label size
      migrated_label_size <- migrated_label_size + 1
    }
  }

  if (migrated_thickness > 0 || migrated_label_size > 0) {
    dotplot_debug_log(
      paste0(
        "Migrated threshold defaults: thickness added to ", migrated_thickness,
        " threshold(s), label_size added to ", migrated_label_size, " threshold(s)"
      ),
      3
    )
  }

  return(thresholds)
}

dotplot_apply_theme <- function(plot, plot_params) {
  # Apply theme and styling with SAFE parameter handling

  # SAFE parameter extraction with guaranteed defaults
  title_size <- if (!is.null(plot_params$title_size) && !is.na(plot_params$title_size)) {
    as.numeric(plot_params$title_size)
  } else {
    14
  }

  axis_title_size <- if (!is.null(plot_params$axis_title_size) && !is.na(plot_params$axis_title_size)) {
    as.numeric(plot_params$axis_title_size)
  } else {
    12
  }

  tick_size <- if (!is.null(plot_params$tick_size) && !is.na(plot_params$tick_size)) {
    as.numeric(plot_params$tick_size)
  } else {
    10
  }

  x_interval <- if (!is.null(plot_params$x_tick_interval) && !is.na(plot_params$x_tick_interval)) {
    as.numeric(plot_params$x_tick_interval)
  } else {
    1
  }

  y_interval <- if (!is.null(plot_params$y_tick_interval) && !is.na(plot_params$y_tick_interval)) {
    as.numeric(plot_params$y_tick_interval)
  } else {
    1
  }

  dotplot_debug_log(paste("Applying theme parameters - title size:", title_size,
                  "axis title size:", axis_title_size, "tick size:", tick_size), 2)
  dotplot_debug_log(paste("Applying tick intervals - X:", x_interval, "Y:", y_interval), 2)

  # Select theme
  theme_name <- dotplot_normalize_theme_name(if (!is.null(plot_params$theme)) plot_params$theme else "Black and White")
  plot <- plot + switch(theme_name,
                        "Gray" = theme_gray(),
                        "Black and White" = theme_bw(),
                        "Linedraw" = theme_linedraw(),
                        "Light" = theme_light(),
                        "Dark" = theme_dark(),
                        "Minimal" = theme_minimal(),
                        "Classic" = theme_classic(),
                        "Void" = theme_void(),
                        theme_bw()  # default
  )

  # Apply custom text sizing without overriding theme-specific grid defaults
  plot <- plot + theme(
    plot.title = if (isTRUE(plot_params$hide_title)) element_blank() else element_text(size = title_size, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = axis_title_size),
    axis.text = element_text(size = tick_size),
    legend.position = "bottom"
  )


  # Apply custom tick intervals with SAFE validation
  if (x_interval > 0 && x_interval < 1000) {  # Sanity check
    plot <- plot + scale_x_continuous(
      breaks = function(x) {
        if (length(x) != 2) return(pretty(x))
        seq(from = floor(x[1] / x_interval) * x_interval,
            to = ceiling(x[2] / x_interval) * x_interval,
            by = x_interval)
      }
    )
  }

  if (y_interval > 0 && y_interval < 1000) {  # Sanity check
    plot <- plot + scale_y_continuous(
      breaks = function(y) {
        if (length(y) != 2) return(pretty(y))
        seq(from = floor(y[1] / y_interval) * y_interval,
            to = ceiling(y[2] / y_interval) * y_interval,
            by = y_interval)
      }
    )
  }

  return(plot)
}

# ========================================
# Validation Functions
# ========================================

dotplot_validate_config <- function(axis_config, data_def, data = NULL) {
  # Validate plot configuration before generation

  dotplot_debug_log("Validating plot configuration", 2)

  # Check axis column selection
  if (is.null(axis_config$x_col) || is.null(axis_config$y_col)) {
    return(list(valid = FALSE, message = "Please select both X and Y axis columns"))
  }

  if (axis_config$x_col == "" || axis_config$y_col == "") {
    return(list(valid = FALSE, message = "Please select both X and Y axis columns"))
  }

  # Check if columns exist in metadata
  available_cols <- data_def$Column

  if (!axis_config$x_col %in% available_cols) {
    return(list(valid = FALSE, message = paste("X-axis column not found:", axis_config$x_col)))
  }

  if (!axis_config$y_col %in% available_cols) {
    return(list(valid = FALSE, message = paste("Y-axis column not found:", axis_config$y_col)))
  }

  # Check for same column selection
  if (axis_config$x_col == axis_config$y_col) {
    return(list(valid = FALSE, message = "X and Y axis cannot use the same column"))
  }

  # Axis plotting requires columns that are actually numeric in the loaded data.
  # Metadata Content helps prioritize selectors, but the data frame type is the
  # authoritative plotting check.
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      return(list(valid = FALSE, message = "Loaded data is not available for numeric axis validation"))
    }

    if (!axis_config$x_col %in% names(data)) {
      return(list(valid = FALSE, message = paste("X-axis column not found in loaded data:", axis_config$x_col)))
    }

    if (!axis_config$y_col %in% names(data)) {
      return(list(valid = FALSE, message = paste("Y-axis column not found in loaded data:", axis_config$y_col)))
    }

    non_numeric_axes <- character(0)
    if (!is.numeric(data[[axis_config$x_col]])) {
      non_numeric_axes <- c(non_numeric_axes, paste0("X-axis '", axis_config$x_col, "' (", class(data[[axis_config$x_col]])[1], ")"))
    }
    if (!is.numeric(data[[axis_config$y_col]])) {
      non_numeric_axes <- c(non_numeric_axes, paste0("Y-axis '", axis_config$y_col, "' (", class(data[[axis_config$y_col]])[1], ")"))
    }

    if (length(non_numeric_axes) > 0) {
      return(list(
        valid = FALSE,
        message = paste(
          "Dotplot axes must be numeric in the loaded data. Non-numeric selection(s):",
          paste(non_numeric_axes, collapse = "; ")
        )
      ))
    }
  }

  dotplot_debug_log("Plot configuration validation passed", 2)
  return(list(valid = TRUE, message = "Configuration valid"))
}

# ========================================
# Enhanced Plot Creation with Regions
# ========================================

dotplot_create_complete_plot_with_regions <- function(data, data_def, axis_config, thresholds,
                                                      color_rules, plot_params, region_configs = list(),
                                                      region_structure = NULL) {

  dotplot_debug_log("Creating plot with region support", 1)

  # If no region configs, use standard plot creation
  if (length(region_configs) == 0) {
    dotplot_debug_log("No region configs - using standard plot creation", 2)
    return(dotplot_create_complete_plot_enhanced(
      data,
      data_def,
      axis_config,
      thresholds,
      color_rules,
      plot_params,
      ui_labels = list()
    ))
  }

  dotplot_debug_log(paste("Using", length(region_configs), "region configurations"), 1)

  # Prepare plot data
  plot_data <- dotplot_prepare_plot_data(data, data_def, axis_config)
  if (is.null(plot_data)) {
    return(NULL)
  }

  # Assign points to regions
  plot_data <- dotplot_assign_points_to_regions(plot_data, region_structure)

  # Apply region-specific styling
  plot_data <- dotplot_apply_region_styling_fixed(plot_data, region_configs)

  # Create plot with individual point properties
  dotplot_debug_log("Creating plot with individual point styling", 2)
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(aes(color = I(color), size = I(size), alpha = I(alpha), shape = I(shape))) +
    labs(
      title = dotplot_resolve_plot_title(plot_params),
      x = ifelse(!is.null(axis_config$x_label) && nzchar(axis_config$x_label),
                 axis_config$x_label,
                 dotplot_transform_label(axis_config$x_col, axis_config$x_transform)),
      y = ifelse(!is.null(axis_config$y_label) && nzchar(axis_config$y_label),
                 axis_config$y_label,
                 dotplot_transform_label(axis_config$y_col, axis_config$y_transform))
    )

  # Add thresholds
  if (length(thresholds) > 0) {
    p <- dotplot_add_thresholds(p, thresholds)
  }

  # Apply theme
  p <- dotplot_apply_theme(p, plot_params)

  dotplot_debug_log("Region-enhanced plot creation completed", 1)
  return(p)
}

# MODIFIED: Enhanced plot creation function that uses UI labels
dotplot_create_complete_plot_enhanced <- function(data, data_def, axis_config, thresholds,
                                                  color_rules, plot_params, ui_labels = list()) {
  # Enhanced plot creation that prioritizes UI axis labels

  dotplot_debug_log("Creating plot with UI-prioritized axis labels", 1)

  # Prepare plot data
  plot_data <- dotplot_prepare_plot_data(data, data_def, axis_config)
  if (is.null(plot_data)) {
    dotplot_debug_log("Plot data preparation failed", 1)
    return(NULL)
  }

  # Generate axis labels using UI input + transformation
  x_label <- dotplot_create_axis_label_with_ui(ui_labels$x_label, axis_config$x_transform)
  y_label <- dotplot_create_axis_label_with_ui(ui_labels$y_label, axis_config$y_transform)

  dotplot_debug_log(paste("Using UI-enhanced axis labels - X:", x_label, "Y:", y_label), 1)

  # Create base plot
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(
      size = 1,
      alpha = 1,
      color = plot_params$point_color
    ) +
    labs(
      title = dotplot_resolve_plot_title(plot_params),
      x = x_label,  # Now uses UI textfield + transformation
      y = y_label   # Now uses UI textfield + transformation
    )

  # Add thresholds if any
  if (length(thresholds) > 0) {
    p <- dotplot_add_thresholds(p, thresholds)
  }

  # Apply theme
  p <- dotplot_apply_theme(p, plot_params)

  dotplot_debug_log("Enhanced plot creation completed", 1)
  return(p)
}

# MODIFIED: Enhanced plot creation with regions that uses UI labels
dotplot_create_complete_plot_with_regions_enhanced <- function(data, data_def, axis_config, thresholds,
                                                               color_rules, plot_params, region_configs = list(),
                                                               region_structure = NULL, ui_labels = list()) {

  dotplot_debug_log("Creating enhanced plot with region support and UI labels", 1)

  # If no region configs, use standard enhanced plot creation
  if (length(region_configs) == 0) {
    dotplot_debug_log("No region configs - using standard enhanced plot creation", 2)
    return(dotplot_create_complete_plot_enhanced(data, data_def, axis_config, thresholds, color_rules, plot_params, ui_labels))
  }

  dotplot_debug_log(paste("Using", length(region_configs), "region configurations with UI labels"), 1)

  # Prepare plot data
  plot_data <- dotplot_prepare_plot_data(data, data_def, axis_config)
  if (is.null(plot_data)) {
    return(NULL)
  }

  # Assign points to regions
  plot_data <- dotplot_assign_points_to_regions(plot_data, region_structure)

  # Apply region-specific styling
  plot_data <- dotplot_apply_region_styling_fixed(plot_data, region_configs)

  # Generate axis labels using UI input + transformation
  x_label <- dotplot_create_axis_label_with_ui(ui_labels$x_label, axis_config$x_transform)
  y_label <- dotplot_create_axis_label_with_ui(ui_labels$y_label, axis_config$y_transform)

  dotplot_debug_log(paste("Using UI-enhanced axis labels in regions plot - X:", x_label, "Y:", y_label), 1)

  # Create plot with individual point properties and UI labels
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(aes(color = I(color), size = I(size), alpha = I(alpha), shape = I(shape))) +
    labs(
      title = dotplot_resolve_plot_title(plot_params),
      x = x_label,  # Now uses UI textfield + transformation
      y = y_label   # Now uses UI textfield + transformation
    )

  # Add thresholds
  if (length(thresholds) > 0) {
    p <- dotplot_add_thresholds(p, thresholds)
  }

  # Apply theme
  p <- dotplot_apply_theme(p, plot_params)

  dotplot_debug_log("Enhanced region plot creation with UI labels completed", 1)
  return(p)
}

# ========================================
# Modified Parameter Extraction for UI Labels
# ========================================

# ------------------------------------------------------------------------------
# dotplot_extract_plot_parameters
# Purpose: Safely extract plot parameter values from Shiny input with robust
#   numeric fallbacks for styling and tick settings.
# Parameters:
#   - input: reactivevalues/list - Shiny input object used by the Dotplot module.
# Returns: named list with plot title, theme, color, and text/tick values.
# ------------------------------------------------------------------------------
dotplot_extract_plot_parameters <- function(input) {
  sanitize_numeric_input <- function(value, default, min_value = NULL) {
    numeric_value <- suppressWarnings(as.numeric(value))

    if (length(numeric_value) == 0 || is.na(numeric_value) || !is.finite(numeric_value)) {
      return(default)
    }

    if (!is.null(min_value) && numeric_value < min_value) {
      return(default)
    }

    numeric_value
  }

  # Extract plot parameters with ROBUST defaults and validation

  if (missing(input) || is.null(input)) {
    dotplot_debug_log("WARNING: input object missing, using defaults", 1)
    return(list(
      plot_title = "Dot Plot", hide_title = FALSE, theme = "Black and White", point_color = "#E0E0E0",
      title_size = 14, axis_title_size = 12, tick_size = 10, x_tick_interval = 1, y_tick_interval = 1
    ))
  }

  # ROBUST extraction with explicit NULL checks
  title_size_val <- sanitize_numeric_input(input$title_size, default = 14, min_value = 1)
  axis_title_size_val <- sanitize_numeric_input(input$axis_title_size, default = 12, min_value = 1)
  tick_size_val <- sanitize_numeric_input(input$tick_size, default = 10, min_value = 1)
  x_tick_interval_val <- sanitize_numeric_input(input$x_tick_interval, default = 1, min_value = 0.000001)
  y_tick_interval_val <- sanitize_numeric_input(input$y_tick_interval, default = 1, min_value = 0.000001)

  # Build parameter list with guaranteed valid values
  params <- list(
    plot_title = if (!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else "Dot Plot",
    hide_title = isTRUE(input$hide_title),
    theme = dotplot_normalize_theme_name(if (!is.null(input$theme_select)) input$theme_select else "Black and White"),
    point_color = "#E0E0E0",
    title_size = title_size_val,
    axis_title_size = axis_title_size_val,
    tick_size = tick_size_val,
    x_tick_interval = x_tick_interval_val,
    y_tick_interval = y_tick_interval_val
  )

  # DEBUG: Log extracted parameters
  dotplot_debug_log(paste("Extracted parameters - title size:", params$title_size,
                  "axis title size:", params$axis_title_size, "tick size:", params$tick_size,
                  "hide_title:", params$hide_title), 2)
  dotplot_debug_log(paste("Extracted tick intervals - X:", params$x_tick_interval,
                  "Y:", params$y_tick_interval), 2)

  return(params)
}

# ========================================
# Enhanced Data Preparation for Selection
# ========================================

dotplot_prepare_plot_data_with_identifiers <- function(data, data_def, axis_config, identifier_col = NULL) {
  # Enhanced version that includes identifier information for selection

  dotplot_debug_log("Preparing plot data with identifier information", 2)

  # Get base plot data
  plot_data <- dotplot_prepare_plot_data(data, data_def, axis_config)

  if (is.null(plot_data)) {
    dotplot_debug_log("Base plot data preparation failed", 1)
    return(NULL)
  }

  # Add identifier information if available
  if (!is.null(identifier_col) && nzchar(identifier_col)) {
    # Resolve identifier by data column name first (metadata index fallback).
    identifier_idx <- which(colnames(data) == identifier_col)
    if (length(identifier_idx) == 0 && is.data.frame(data_def) && "Column" %in% names(data_def)) {
      meta_idx <- which(data_def$Column == identifier_col)
      if (length(meta_idx) > 0 && meta_idx[1] <= ncol(data)) identifier_idx <- meta_idx[1]
    }

    if (length(identifier_idx) > 0) {
      # Extract identifier data
      identifier_data <- data[[identifier_idx[1]]]

      # Match identifiers to plot data rows
      if (length(identifier_data) >= nrow(plot_data)) {
        plot_data$identifier <- identifier_data[1:nrow(plot_data)]
        dotplot_debug_log(paste("Added identifier data for", nrow(plot_data), "points"), 2)
      } else {
        dotplot_debug_log("Identifier data length mismatch, using row indices", 1)
        plot_data$identifier <- paste0("Point_", seq_len(nrow(plot_data)))
      }
    } else {
      dotplot_debug_log("Identifier column not found in metadata", 1)
      plot_data$identifier <- paste0("Point_", seq_len(nrow(plot_data)))
    }
  } else {
    dotplot_debug_log("No identifier column specified, using row indices", 2)
    plot_data$identifier <- paste0("Point_", seq_len(nrow(plot_data)))
  }

  return(plot_data)
}
