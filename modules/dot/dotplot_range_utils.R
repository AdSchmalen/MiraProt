# ==============================================================================
# dotplot_range_utils.R - Dotplot range and slider utilities
#
# Range extraction, transformation, tick/step calculation, and slider expansion.
# Sourced after dotplot_utils.R by modules/dotplot_module.R.
# ==============================================================================

# ========================================
# Axis Range Calculation Functions
# ========================================

apply_transformation <- function(values, transform_type) {
  # Apply the specified transformation to values

  switch(transform_type,
         "raw" = values,
         "log2" = {
           # Handle zero/negative values for log transformation
           safe_values <- pmax(values, 1e-10)  # Avoid log(0)
           log2(safe_values)
         },
         "log10" = {
           safe_values <- pmax(values, 1e-10)
           log10(safe_values)
         },
         "neg_log10" = {
           # Typically for p-values
           safe_values <- dotplot_clamp_pvalues(values)
           -log10(safe_values)
         },
         values  # Default: return unchanged
  )
}

# ========================================
# Transformed Range Calculation Functions
# ========================================

create_transform_label <- function(transform_type) {
  # Create transformation label with subscripts
  switch(transform_type,
         "raw" = "",
         "log2" = "log₂",
         "log10" = "log₁₀",
         "neg_log10" = "-log₁₀",
         ""
  )
}

extract_dotplot_data_ranges <- function(plot_data) {
  # Extract natural transformed data ranges without building a ggplot object.
  if (!is.data.frame(plot_data) ||
      !all(c("x", "y") %in% names(plot_data))) {
    return(NULL)
  }

  x_values <- suppressWarnings(as.numeric(plot_data$x))
  y_values <- suppressWarnings(as.numeric(plot_data$y))
  x_values <- x_values[is.finite(x_values)]
  y_values <- y_values[is.finite(y_values)]

  if (length(x_values) == 0L || length(y_values) == 0L) {
    return(NULL)
  }

  list(
    x_range = range(x_values, na.rm = TRUE),
    y_range = range(y_values, na.rm = TRUE)
  )
}

extract_ggplot_ranges <- function(ggplot_obj) {
  # Extract NATURAL ranges from ggplot object - ENHANCED DEBUG VERSION

  if (is.null(ggplot_obj)) {
    dotplot_debug_log("Range extraction skipped: plot is NULL", 2)
    return(list(x_range = c(-1, 1), y_range = c(-1, 1)))
  }

  if (!inherits(ggplot_obj, "ggplot")) {
    dotplot_debug_log(
      paste0("Range extraction skipped: unsupported plot class (",
             paste(class(ggplot_obj), collapse = "/"), ")"),
      2
    )
    return(list(x_range = c(-1, 1), y_range = c(-1, 1)))
  }

  tryCatch({
    dotplot_debug_log("Starting range extraction from ggplot object", 2)

    # Check if plot has coord_cartesian applied (which would override natural ranges)
    plot_layers <- ggplot_obj$coordinates
    if (!is.null(plot_layers) && inherits(plot_layers, "CoordCartesian")) {
      dotplot_debug_log("WARNING: Plot has coord_cartesian applied - may not show natural ranges", 1)
    }

    # Build the plot to get actual ranges
    built_plot <- ggplot_build(ggplot_obj)

    # Check panel params
    if (is.null(built_plot$layout$panel_params) || length(built_plot$layout$panel_params) == 0) {
      dotplot_debug_log("ERROR: No panel_params found in built plot", 1)
      return(list(x_range = c(-1, 1), y_range = c(-1, 1)))
    }

    panel_params <- built_plot$layout$panel_params[[1]]

    # Extract coordinate ranges
    x_range <- panel_params$x.range
    y_range <- panel_params$y.range

    dotplot_debug_log(paste("Raw extracted ranges - X:", paste(x_range, collapse = ", "),
                    "Y:", paste(y_range, collapse = ", ")), 2)

    # Validate ranges
    if (is.null(x_range) || is.null(y_range) ||
        length(x_range) != 2 || length(y_range) != 2 ||
        !all(is.finite(x_range)) || !all(is.finite(y_range))) {

      dotplot_debug_log("ERROR: Invalid ranges detected", 1)
      dotplot_debug_log(paste("X range issues:", is.null(x_range), length(x_range) != 2, !all(is.finite(x_range))), 2)
      dotplot_debug_log(paste("Y range issues:", is.null(y_range), length(y_range) != 2, !all(is.finite(y_range))), 2)

      return(list(x_range = c(-1, 1), y_range = c(-1, 1)))
    }

    dotplot_debug_log(paste("Successfully extracted NATURAL ranges - X:", paste(round(x_range, 3), collapse = " to "),
                    "Y:", paste(round(y_range, 3), collapse = " to ")), 1)

    return(list(x_range = x_range, y_range = y_range))

  }, error = function(e) {
    dotplot_debug_log(paste("CRITICAL ERROR in range extraction:", e$message), 1)
    dotplot_debug_log("Stack trace:", 1)
    dotplot_debug_log(paste(deparse(traceback()), collapse = "\n"), 2)

    # Return safe fallback
    return(list(x_range = c(-1, 1), y_range = c(-1, 1)))
  })
}

expand_range_for_slider <- function(range_vals, expansion = 0.1) {
  # Smart range expansion with intelligent step size and decimal places

  if (length(range_vals) != 2 || !all(is.finite(range_vals))) {
    dotplot_debug_log("Invalid range values for expansion", 1)
    return(list(min = -1, max = 1, value = c(-1, 1), step = 0.1, decimals = 1))
  }

  range_span <- diff(range_vals)

  # Handle edge case where range is very small or zero
  if (range_span <= 0) {
    center <- mean(range_vals)
    padding <- max(abs(center) * 0.1, 1)
    step_info <- calculate_smart_step_size(c(center - padding/2, center + padding/2))

    return(list(
      min = center - padding,
      max = center + padding,
      value = c(center - padding/2, center + padding/2),
      step = step_info$step,
      decimals = step_info$decimals
    ))
  }

  # Calculate smart step size for the range
  step_info <- calculate_smart_step_size(range_vals)

  # Normal case: add expansion
  expansion_amount <- range_span * expansion

  # Round expansion to nice numbers based on step size
  expansion_amount <- round(expansion_amount / step_info$step) * step_info$step

  return(list(
    min = range_vals[1] - expansion_amount,
    max = range_vals[2] + expansion_amount,
    value = range_vals,
    step = step_info$step,
    decimals = step_info$decimals
  ))
}

# ========================================
# Smart Step Size Calculation Function
# ========================================

calculate_smart_step_size <- function(range_vals) {
  # Calculate intelligent step size and decimal places based on range magnitude

  range_span <- diff(range_vals)

  # Calculate the order of magnitude of the range
  magnitude <- floor(log10(range_span))

  # Target: approximately 100-1000 steps across the range
  base_step <- 10^(magnitude - 2)  # Start with 1/100 of the magnitude

  # Round to "nice" step sizes: 1, 2, 5, 10, 20, 50, 100, etc.
  nice_steps <- c(1, 2, 5) * base_step
  nice_steps <- c(nice_steps, c(1, 2, 5) * base_step * 10)

  # Choose the step that gives us roughly 100-500 steps
  target_steps <- range_span / nice_steps
  best_idx <- which.min(abs(target_steps - 200))  # Target ~200 steps

  optimal_step <- nice_steps[best_idx]

  # Calculate appropriate decimal places
  if (optimal_step >= 1) {
    decimals <- 0
  } else if (optimal_step >= 0.1) {
    decimals <- 1
  } else if (optimal_step >= 0.01) {
    decimals <- 2
  } else if (optimal_step >= 0.001) {
    decimals <- 3
  } else {
    decimals <- 4
  }

  dotplot_debug_log(paste("Smart step calculation - Range:", round(range_span, 4),
                  "Magnitude:", magnitude, "Step:", optimal_step, "Decimals:", decimals), 2)

  return(list(
    step = optimal_step,
    decimals = decimals
  ))
}

apply_plot_transformation <- function(values, transform_type) {
  # Apply the same transformation as used in the plot
  # This matches the dotplot_apply_transform_safe function logic

  switch(transform_type,
         "raw" = values,
         "log2" = {
           # Handle zero/negative values for log transformation
           safe_values <- pmax(values, 1e-10)  # Avoid log(0)
           log2(safe_values)
         },
         "log10" = {
           safe_values <- pmax(values, 1e-10)
           log10(safe_values)
         },
         "neg_log10" = {
           # Typically for p-values - use very small minimum
           safe_values <- dotplot_clamp_pvalues(values)
           -log10(safe_values)
         },
         values  # Default: return unchanged
  )
}

calculate_smart_transformed_limits <- function(range_vals, transform_type) {
  # Calculate smart slider limits based on transformed range

  range_span <- diff(range_vals)

  # Expansion factor depends on transformation type
  expansion_factor <- switch(transform_type,
                             "raw" = 0.5,        # 50% expansion for raw data
                             "log2" = 0.3,       # 30% expansion for log data
                             "log10" = 0.3,
                             "neg_log10" = 0.2,  # 20% expansion for -log10 (p-values)
                             0.4
  )

  expansion <- range_span * expansion_factor

  slider_min <- range_vals[1] - expansion
  slider_max <- range_vals[2] + expansion

  # Step size based on transformed range magnitude
  step_size <- range_span / 100

  # Ensure reasonable step size for different transformations
  if (transform_type %in% c("log2", "log10")) {
    step_size <- max(step_size, 0.01)  # Minimum for log scales
    step_size <- min(step_size, 0.5)   # Maximum for log scales
  } else if (transform_type == "neg_log10") {
    step_size <- max(step_size, 0.01)  # Fine control for p-values
    step_size <- min(step_size, 1)     # Reasonable maximum
  } else {
    step_size <- max(step_size, 0.01)  # General minimum
    step_size <- min(step_size, 1)     # General maximum
  }

  return(list(
    min = round(slider_min, 3),
    max = round(slider_max, 3),
    step = round(step_size, 3)
  ))
}

extract_plot_ranges <- function(ggplot_obj) {
  # Extract actual axis ranges from ggplot object

  tryCatch({
    # Build the plot to get ranges
    built_plot <- ggplot_build(ggplot_obj)

    # Extract ranges from plot layout
    x_range <- built_plot$layout$panel_params[[1]]$x.range
    y_range <- built_plot$layout$panel_params[[1]]$y.range

    dotplot_debug_log(paste("Extracted plot ranges - X:", paste(round(x_range, 3), collapse = " to "),
                    "Y:", paste(round(y_range, 3), collapse = " to ")), 2)

    return(list(
      x_range = x_range,
      y_range = y_range
    ))

  }, error = function(e) {
    dotplot_debug_log(paste("Could not extract plot ranges:", e$message), 2)
    return(NULL)
  })
}

calculate_conservative_axis_range <- function(data, column_name, transform_type, padding_factor = 0.02) {
  # More conservative range calculation (smaller padding)

  tryCatch({
    values <- data[[column_name]]
    values <- values[is.finite(values)]

    if (length(values) == 0) {
      return(c(-1, 1))
    }

    # Apply transformation
    transformed_values <- apply_plot_transformation(values, transform_type)
    transformed_values <- transformed_values[is.finite(transformed_values)]

    if (length(transformed_values) == 0) {
      return(c(-1, 1))
    }

    # Use smaller padding for more accurate range
    range_min <- min(transformed_values, na.rm = TRUE)
    range_max <- max(transformed_values, na.rm = TRUE)

    if (range_min == range_max) {
      padding <- max(abs(range_min) * 0.1, 0.1)
      return(c(range_min - padding, range_max + padding))
    }

    # Conservative padding
    data_range <- range_max - range_min
    padding <- data_range * padding_factor  # Only 2% padding

    final_range <- c(range_min - padding, range_max + padding)

    dotplot_debug_log(paste("Conservative range calculated:", final_range[1], "to", final_range[2]), 2)
    return(final_range)

  }, error = function(e) {
    dotplot_debug_log(paste("Error in conservative range calculation:", e$message), 1)
    return(c(-1, 1))
  })
}

# ========================================
# Smart Tick Interval Calculator
# ========================================

calculate_smart_tick_interval <- function(range_vals, transform_type = "raw") {
  if (length(range_vals) != 2 || !all(is.finite(range_vals))) {
    return(0.5)
  }

  range_span <- diff(range_vals)

  # Transformation-specific logic
  if (transform_type == "log2") {
    # For log2: intervals of 1 = 2-fold changes, 2 = 4-fold changes
    if (range_span <= 4) return(0.5)
    else if (range_span <= 8) return(1)
    else return(2)

  } else if (transform_type == "neg_log10") {
    # For -log10 p-values: common thresholds
    if (range_span <= 3) return(0.5)  # 0.5 = ~3-fold p-value changes
    else if (range_span <= 10) return(1)
    else return(2)

  } else if (transform_type == "log10") {
    # Similar to log2 but decades
    if (range_span <= 2) return(0.5)
    else if (range_span <= 4) return(1)
    else return(2)

  } else {
    # Raw data: simple 5-tick strategy
    raw_interval <- range_span / 5

    if (raw_interval >= 10) return(ceiling(raw_interval / 10) * 10)
    else if (raw_interval >= 1) return(ceiling(raw_interval))
    else if (raw_interval >= 0.1) return(ceiling(raw_interval * 10) / 10)
    else if (raw_interval >= 0.01) return(ceiling(raw_interval * 100) / 100)
    else return(ceiling(raw_interval * 1000) / 1000)
  }
}

# ========================================
# Enhanced Range Expansion with Tick Intervals
# ========================================

expand_range_for_slider_with_ticks <- function(range_vals, transform_type = "raw", expansion = 0.1) {
  # Enhanced version that also calculates smart tick intervals

  if (length(range_vals) != 2 || !all(is.finite(range_vals))) {
    return(list(
      min = -1, max = 1, value = c(-1, 1),
      step = 0.1, decimals = 1,
      tick_interval = 0.5
    ))
  }

  range_span <- diff(range_vals)

  # Handle edge case where range is very small or zero
  if (range_span <= 0) {
    center <- mean(range_vals)
    padding <- max(abs(center) * 0.1, 1)

    return(list(
      min = center - padding,
      max = center + padding,
      value = c(center - padding/2, center + padding/2),
      step = 0.1,
      decimals = 1,
      tick_interval = padding / 4
    ))
  }

  # Calculate smart step size for the range
  step_info <- calculate_smart_step_size(range_vals)

  # Calculate smart tick interval
  tick_interval <- calculate_smart_tick_interval(range_vals, transform_type)

  # Normal case: add expansion
  expansion_amount <- range_span * expansion
  expansion_amount <- round(expansion_amount / step_info$step) * step_info$step

  return(list(
    min = range_vals[1] - expansion_amount,
    max = range_vals[2] + expansion_amount,
    value = range_vals,
    step = step_info$step,
    decimals = step_info$decimals,
    tick_interval = tick_interval
  ))
}
