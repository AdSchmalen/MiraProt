# ==============================================================================
# dotplot_label_utils.R - Dotplot axis and point labeling utilities
#
# Label parsing, axis-label formatting, and enhanced point-label application.
# Sourced after dotplot_utils.R by modules/dotplot_module.R.
# ==============================================================================

# ========================================
dotplot_generate_axis_label <- function(column_name, data_def, axis_type) {
  # Generate meaningful axis labels - ROBUST VERSION

  # Robust input validation
  if (is.null(column_name) || is.null(data_def) ||
      length(column_name) == 0 || nchar(trimws(column_name)) == 0) {
    dotplot_debug_log("Invalid column_name for axis label generation", 2)
    return("Unknown Column")
  }

  if (nrow(data_def) == 0 || !("Column" %in% names(data_def)) || !("Content" %in% names(data_def))) {
    dotplot_debug_log("Invalid data_def structure for axis label generation", 2)
    return(column_name)
  }

  # Find column in metadata with robust matching
  tryCatch({
    col_idx <- which(data_def$Column == column_name)

    if (length(col_idx) == 0) {
      dotplot_debug_log(paste("Column not found in metadata:", column_name), 2)
      return(column_name)
    }

    content <- data_def$Content[col_idx[1]]

    # Handle NA or empty content
    if (is.na(content) || is.null(content) || nchar(trimws(content)) == 0) {
      dotplot_debug_log(paste("Empty content for column:", column_name), 2)
      return(column_name)
    }

    # Generate label based on content with robust pattern matching
    if (identical(content, "Abundance Ratio")) {
      return("Abundance Ratio")
    } else if (grepl("p-Value$", content, ignore.case = TRUE)) {
      return("p-value")
    } else if (grepl("Adj.*p-Value", content, ignore.case = TRUE)) {
      return("Adjusted p-value")
    } else if (grepl("Intensity", content, ignore.case = TRUE)) {
      return("Intensity")
    } else if (grepl("Average", content, ignore.case = TRUE)) {
      return("Average Expression")
    } else if (grepl("Additional Information", content, ignore.case = TRUE)) {
      return(paste("Additional Info:", column_name))
    } else {
      # Return content as is, but cleaned
      return(trimws(content))
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error in axis label generation:", e$message), 1)
    return(column_name)  # Safe fallback
  })
}

dotplot_create_content_based_label <- function(column_name, data_def, transform_type) {
  # Create axis label based on Content column from metadata

  dotplot_debug_log(paste("Creating content-based label for column:", column_name), 2)

  # Find the row in data_def where Column matches
  matching_rows <- which(data_def$Column == column_name)

  if (length(matching_rows) == 0) {
    dotplot_debug_log(paste("Column not found in metadata:", column_name), 1)
    base_content <- column_name  # Fallback to column name
  } else {
    # Get content from first matching row
    content_value <- data_def$Content[matching_rows[1]]

    # If content is empty/NA, use column name
    if (is.na(content_value) || is.null(content_value) || nzchar(trimws(content_value)) == FALSE) {
      dotplot_debug_log(paste("Content empty for column:", column_name, "- using column name"), 2)
      base_content <- column_name
    } else {
      dotplot_debug_log(paste("Using content from metadata:", content_value), 2)
      base_content <- trimws(content_value)
    }
  }

  # Apply transformation with subscripts
  transform_prefix <- switch(transform_type,
                             "raw" = "",
                             "log2" = "log₂",
                             "log10" = "log₁₀",
                             "neg_log10" = "-log₁₀",
                             ""
  )

  # Create final label
  if (nzchar(transform_prefix)) {
    final_label <- paste0(transform_prefix, "(", base_content, ")")
  } else {
    final_label <- base_content
  }

  dotplot_debug_log(paste("Generated content-based label:", final_label), 2)
  return(final_label)
}


dotplot_transform_label <- function(column_name, transform_type) {
  # Generate axis label including transformation

  base_label <- column_name

  transform_label <- switch(transform_type,
                            "raw" = "",
                            "log2" = "log2",
                            "log10" = "log10",
                            "neg_log10" = "-log10",
                            ""
  )

  if (nzchar(transform_label)) {
    return(paste0(transform_label, "(", base_label, ")"))
  } else {
    return(base_label)
  }
}


# Enhanced Axis Label Functions for Dotplot Module
# ========================================

dotplot_generate_enhanced_axis_label <- function(column_name, data_def, transform_type, axis_type = "axis") {
  # Generate enhanced axis label with transformation - for UI updates

  # Get base label from existing function
  base_label <- dotplot_generate_axis_label(column_name, data_def, axis_type)

  # Add transformation prefix with subscripts
  transform_prefix <- switch(transform_type,
                             "raw" = "",
                             "log2" = "log₂",
                             "log10" = "log₁₀",
                             "neg_log10" = "-log₁₀",
                             ""
  )

  # Combine transformation with base label
  if (nzchar(transform_prefix)) {
    enhanced_label <- paste0(transform_prefix, "(", base_label, ")")
    dotplot_debug_log(paste("Generated enhanced label for UI:", enhanced_label), 2)
    return(enhanced_label)
  } else {
    dotplot_debug_log(paste("No transformation, using base label:", base_label), 2)
    return(base_label)
  }
}

dotplot_create_axis_label_with_ui <- function(ui_label, transform_type) {
  # Use UI label directly if provided - this prevents double transformation
  # UI labels should already contain correct transformation from automatic updates

  if (!is.null(ui_label) && nzchar(trimws(ui_label))) {
    # User provided or auto-generated label - use exactly as provided
    final_label <- trimws(ui_label)
    dotplot_debug_log(paste("Using UI label as-is:", final_label), 2)
    return(final_label)
  } else {
    # Fallback if UI is completely empty
    transform_prefix <- switch(transform_type,
                               "raw" = "",
                               "log2" = "log₂",
                               "log10" = "log₁₀",
                               "neg_log10" = "-log₁₀",
                               ""
    )

    if (nzchar(transform_prefix)) {
      fallback_label <- paste0(transform_prefix, "(Data)")
    } else {
      fallback_label <- "Data"
    }

    dotplot_debug_log(paste("Using fallback label:", fallback_label), 2)
    return(fallback_label)
  }
}

dotplot_static_axis_label <- function(label) {

  if (is.null(label) ||
      inherits(label, "expression") ||
      is.language(label) ||
      !is.character(label) ||
      length(label) != 1L) {
    return(label)
  }

  text <- label[[1L]]

  extract_inner <- function(pattern) {

    matched <-
      regexec(
        pattern,
        text,
        perl = TRUE
      )

    parts <-
      regmatches(
        text,
        matched
      )[[1L]]

    if (length(parts) == 2L) {
      parts[[2L]]
    } else {
      NULL
    }
  }

  # Support both current Unicode UI labels and legacy/plain-text forms.
  base <- extract_inner(
    "^-log(?:\u2081\u2080|10)\\((.*)\\)$"
  )

  if (!is.null(base)) {
    return(
      as.expression(
        bquote(
          -log[10](.(base))
        )
      )
    )
  }

  base <- extract_inner(
    "^log(?:\u2081\u2080|10)\\((.*)\\)$"
  )

  if (!is.null(base)) {
    return(
      as.expression(
        bquote(
          log[10](.(base))
        )
      )
    )
  }

  base <- extract_inner(
    "^log(?:\u2082|2)\\((.*)\\)$"
  )

  if (!is.null(base)) {
    return(
      as.expression(
        bquote(
          log[2](.(base))
        )
      )
    )
  }

  label
}


dotplot_prepare_static_axis_labels <- function(plot_obj) {

  if (!inherits(
    plot_obj,
    "ggplot"
  )) {
    return(plot_obj)
  }

  plot_obj +
    ggplot2::labs(
      x = dotplot_static_axis_label(
        plot_obj$labels$x
      ),
      y = dotplot_static_axis_label(
        plot_obj$labels$y
      )
    )
}


#' Parse protein input text for Heatmap module
#' @param input_text Text from text area
#' @param selected_identifier Column name to use
#' @param dotplot_debug_log Debug logging function
#' @return Data frame with parsed identifiers
get_filter_string_dot <- function(input_text, selected_identifier, dotplot_debug_log) {
  dotplot_debug_log("Parsing protein input text for Heatmap module", 2)

  lines <- unlist(strsplit(input_text, "\n"))
  lines <- trimws(lines[lines != ""])
  num_lines <- length(lines)

  if (num_lines == 0) {
    return(data.frame())
  }

  df <- data.frame(matrix(nrow = num_lines, ncol = 1))
  colnames(df) <- c(selected_identifier)

  for (i in 1:num_lines) {
    line <- unlist(strsplit(lines[i], "[,\\s]+"))
    df[i, selected_identifier] <- line[1]  # Takes first element if comma/space separated
  }

  dotplot_debug_log(paste("Parsed", num_lines, "protein identifiers"), 2)
  return(df)
}

# ========================================
# Helper Functions for Enhanced Labeling
# ========================================

get_default_dot_colors_for_proteins_dot <- function(proteins) {
  tryCatch({
    # For dot plots, we use a standard color scheme
    # Could be enhanced to use region-based coloring if available
    default_color <- "#E0E0E0"  # Standard dot plot color

    # If dotplot_state has color_rules, apply them here
    if (exists("dotplot_state") && !is.null(dotplot_state$color_rules)) {
      # Apply color rules logic if needed
      dotplot_debug_log("Using dot plot color rules for default colors", 2)
    }

    default_colors <- rep(default_color, length(proteins))

    dotplot_debug_log(paste("Generated default colors for", length(proteins), "proteins in dot plot"), 2)
    return(default_colors)

  }, error = function(e) {
    dotplot_debug_log(paste("Error calculating default colors for dot plot:", e$message), 1)
    return(rep("#E0E0E0", length(proteins)))
  })
}

# ========================================
# Enhanced Labeling Functions for Dot Plots
# ========================================

create_dot_label_data_enhanced_FIXED <- function(proteins_to_label, rv, input, dotplot_state, dotplot_debug_log, protein_settings = NULL) {
  tryCatch({
    dotplot_debug_log("=== DOT LABEL COORDINATE FIXED VERSION ===", 1)

    # Get data
    data <- rv$data_mod
    data_def <- rv$data_def
    selected_identifier <- input$GeneIdentifierColumn_dot

    dotplot_debug_log(paste("Selected identifier:", selected_identifier), 1)
    dotplot_debug_log(paste("Number of proteins to label:", length(proteins_to_label)), 1)
    dotplot_debug_log(paste("Proteins:", paste(proteins_to_label, collapse = ", ")), 1)

    # Find identifier column
    Identifier_indices <- which(grepl(selected_identifier, data_def$Options, fixed = TRUE))
    if (length(Identifier_indices) == 0) {
      dotplot_debug_log("ERROR: Identifier column not found", 1)
      return(NULL)
    }

    dotplot_debug_log(paste("Identifier column index:", Identifier_indices[1]), 1)

    # Get axis configuration from dotplot_state
    if (is.null(dotplot_state$axis_config)) {
      dotplot_debug_log("ERROR: No axis_config found in dotplot_state", 1)
      return(NULL)
    }

    x_col <- dotplot_state$axis_config$x_col
    y_col <- dotplot_state$axis_config$y_col
    x_transform <- dotplot_state$axis_config$x_transform %||% "raw"
    y_transform <- dotplot_state$axis_config$y_transform %||% "raw"

    dotplot_debug_log(paste("Axis config - X:", x_col, "Y:", y_col), 1)
    dotplot_debug_log(paste("Transformations - X:", x_transform, "Y:", y_transform), 1)

    if (is.null(x_col) || is.null(y_col)) {
      dotplot_debug_log("ERROR: Axis columns not configured", 1)
      return(NULL)
    }

    # Find column indices
    x_col_idx <- which(colnames(data) == x_col)
    y_col_idx <- which(colnames(data) == y_col)

    if (length(x_col_idx) == 0 || length(y_col_idx) == 0) {
      dotplot_debug_log("ERROR: Axis columns not found in data", 1)
      dotplot_debug_log(paste("Looking for X:", x_col, "Y:", y_col), 1)
      dotplot_debug_log(paste("Available columns:", paste(colnames(data)[1:min(10, ncol(data))], collapse = ", ")), 1)
      return(NULL)
    }

    dotplot_debug_log(paste("Column indices - X:", x_col_idx[1], "Y:", y_col_idx[1]), 1)

    # Get protein data
    protein_rows <- data[data[[Identifier_indices[1]]] %in% proteins_to_label, ]
    if (nrow(protein_rows) == 0) {
      dotplot_debug_log("ERROR: No protein data found", 1)
      return(NULL)
    }

    dotplot_debug_log(paste("Found", nrow(protein_rows), "protein rows"), 1)

    # Extract RAW coordinates
    x_values_raw <- as.numeric(protein_rows[[x_col_idx[1]]])
    y_values_raw <- as.numeric(protein_rows[[y_col_idx[1]]])

    dotplot_debug_log("Raw coordinate values:", 1)
    for (i in 1:min(3, length(x_values_raw))) {
      dotplot_debug_log(paste("  Protein", protein_rows[[Identifier_indices[1]]][i], "- X:", x_values_raw[i], "Y:", y_values_raw[i]), 1)
    }

    # CRITICAL: Apply transformations correctly
    x_values_transformed <- x_values_raw
    y_values_transformed <- y_values_raw

    # X-Axis transformations
    if (x_transform == "log2" && all(x_values_raw > 0, na.rm = TRUE)) {
      x_values_transformed <- log2(x_values_raw)
      dotplot_debug_log("Applied log2 transformation to X", 2)
    } else if (x_transform == "log10" && all(x_values_raw > 0, na.rm = TRUE)) {
      x_values_transformed <- log10(x_values_raw)
      dotplot_debug_log("Applied log10 transformation to X", 2)
    } else if (x_transform == "neg_log10") {
      x_values_transformed <- -log10(dotplot_clamp_pvalues(x_values_raw))
      dotplot_debug_log("Applied neg_log10 transformation to X", 2)
    } else {
      dotplot_debug_log(paste("No X transformation applied (transform:", x_transform, ")"), 2)
    }

    # Y-Axis transformations - FIXED!
    if (y_transform == "log2" && all(y_values_raw > 0, na.rm = TRUE)) {
      y_values_transformed <- log2(y_values_raw)
      dotplot_debug_log("Applied log2 transformation to Y", 2)
    } else if (y_transform == "log10" && all(y_values_raw > 0, na.rm = TRUE)) {
      y_values_transformed <- log10(y_values_raw)
      dotplot_debug_log("Applied log10 transformation to Y", 2)
    } else if (y_transform == "neg_log10") {
      y_values_transformed <- -log10(dotplot_clamp_pvalues(y_values_raw))
      dotplot_debug_log("Applied neg_log10 transformation to Y", 2)
    } else if (y_transform == "-log10") {
      y_values_transformed <- -log10(dotplot_clamp_pvalues(y_values_raw))
      dotplot_debug_log("Applied -log10 transformation to Y", 2)
    } else {
      dotplot_debug_log(paste("No Y transformation applied (transform:", y_transform, ")"), 2)
    }

    dotplot_debug_log("Transformed coordinate values:", 1)
    for (i in 1:min(3, length(x_values_transformed))) {
      dotplot_debug_log(paste("  Protein", protein_rows[[Identifier_indices[1]]][i], "- X:", x_values_transformed[i], "Y:", y_values_transformed[i]), 1)
    }

    # Create label structure with transformed coordinates
    labeling_df <- data.frame(
      ID = protein_rows[[Identifier_indices[1]]],
      x = x_values_transformed,
      y = y_values_transformed,
      LabelColor = "#000000",
      stringsAsFactors = FALSE
    )

    # Apply individual color settings if available
    if (!is.null(protein_settings) && nrow(protein_settings) > 0) {
      dotplot_debug_log("Applying individual color settings", 2)

      for (i in seq_len(nrow(labeling_df))) {
        protein_id <- labeling_df$ID[i]
        setting_row <- protein_settings[protein_settings$protein_id == protein_id, ]

        if (nrow(setting_row) > 0) {
          labeling_df$LabelColor[i] <- setting_row$label_color[1]

          if (setting_row$use_custom_dot_color[1]) {
            labeling_df$CustomDotColor[i] <- setting_row$dot_color[1]
            labeling_df$UseCustomDotColor[i] <- TRUE
          } else {
            labeling_df$CustomDotColor[i] <- NA
            labeling_df$UseCustomDotColor[i] <- FALSE
          }
        }
      }
    }

    # Filter valid data
    valid_rows <- !is.na(labeling_df$x) & !is.na(labeling_df$y)
    result <- labeling_df[valid_rows, ]

    dotplot_debug_log(paste("Final result: Created label data for", nrow(result), "proteins"), 1)
    dotplot_debug_log("Final label coordinates:", 1)
    for (i in 1:min(3, nrow(result))) {
      dotplot_debug_log(paste("  Final", result$ID[i], "- X:", result$x[i], "Y:", result$y[i]), 1)
    }

    return(result)

  }, error = function(e) {
    dotplot_debug_log(paste("ERROR in create_dot_label_data_enhanced_FIXED:", e$message), 1)
    return(NULL)
  })
}

apply_labels_to_dot_plot_enhanced_FIXED <- function(base_plot, labels_df, input, dotplot_debug_log) {
  if (is.null(labels_df) || nrow(labels_df) == 0) {
    dotplot_debug_log("No labels to apply to dot plot", 2)
    return(base_plot)
  }

  dotplot_debug_log(paste("Applying labels to dot plot with", nrow(labels_df), "proteins"), 1)

  # Remove any existing text/label layers (in case of re-labeling)
  plot_layers <- base_plot$layers
  non_label_layers <- list()

  for (i in seq_along(plot_layers)) {
    layer_class <- class(plot_layers[[i]]$geom)
    if (!"GeomText" %in% layer_class &&
        !"GeomLabel" %in% layer_class &&
        !"GeomTextRepel" %in% layer_class) {
      non_label_layers <- append(non_label_layers, list(plot_layers[[i]]))
    } else {
      dotplot_debug_log(paste("Removing existing GeomTextRepel layer", i), 2)
    }
  }

  # Rebuild plot with only non-label layers
  clean_plot <- base_plot
  clean_plot$layers <- non_label_layers

  dotplot_debug_log(paste("Cleaned plot - removed",
                  length(plot_layers) - length(non_label_layers),
                  "existing label layers"), 1)

  # Get labeling settings from UI (with fallbacks)
  max_overlaps <- input$maxOverlaps_dot %||% 10
  label_distance <- input$labelDistance_dot %||% 0.25
  line_thickness <- input$lineThickness_dot %||% 0.5
  label_size <- as.numeric(input$labelSize_dot %||% 3.5)
  dot_size <- as.numeric(input$dotSize_dot %||% 1)  # Get dot size from UI

  dotplot_debug_log(paste("Label settings - Size:", label_size, "Distance:", label_distance,
                  "Overlaps:", max_overlaps, "Dot Size:", dot_size), 2)

  # Debug: Show coordinate ranges
  dotplot_debug_log(paste("Label X range:", min(labels_df$x, na.rm = TRUE), "to", max(labels_df$x, na.rm = TRUE)), 1)
  dotplot_debug_log(paste("Label Y range:", min(labels_df$y, na.rm = TRUE), "to", max(labels_df$y, na.rm = TRUE)), 1)

  # NEW APPROACH: Always add dots for ALL labeled proteins first
  # This ensures ALL labeled proteins get the dot size, regardless of custom color setting
  dotplot_debug_log(paste("Adding dots for ALL", nrow(labels_df), "labeled proteins with size", dot_size), 2)

  # Check if we have custom dot colors
  has_custom_dots <- "UseCustomDotColor" %in% colnames(labels_df) &&
    "CustomDotColor" %in% colnames(labels_df)

  if (has_custom_dots) {
    # Split into custom and default dot colors
    custom_dots <- labels_df[labels_df$UseCustomDotColor == TRUE & !is.na(labels_df$CustomDotColor), ]
    default_dots <- labels_df[labels_df$UseCustomDotColor == FALSE | is.na(labels_df$CustomDotColor), ]

    # Add default colored dots first
    if (nrow(default_dots) > 0) {
      dotplot_debug_log(paste("Adding", nrow(default_dots), "proteins with default dot color and size", dot_size), 2)
      clean_plot <- clean_plot +
        geom_point(
          data = default_dots,
          aes(x = x, y = y),
          color = "#E0E0E0",  # Default dot color
          size = dot_size,
          alpha = 0.9
        )
    }

    # Add custom colored dots
    if (nrow(custom_dots) > 0) {
      dotplot_debug_log(paste("Adding", nrow(custom_dots), "proteins with custom dot colors and size", dot_size), 2)
      clean_plot <- clean_plot +
        geom_point(
          data = custom_dots,
          aes(x = x, y = y),
          color = custom_dots$CustomDotColor,
          size = dot_size,
          alpha = 0.9
        )
    }
  } else {
    # No custom color data, just add all dots with default color
    dotplot_debug_log(paste("Adding ALL", nrow(labels_df), "proteins with default dot color and size", dot_size), 2)
    clean_plot <- clean_plot +
      geom_point(
        data = labels_df,
        aes(x = x, y = y),
        color = "#E0E0E0",  # Default dot color
        size = dot_size,
        alpha = 0.9
      )
  }

  # Add NEW labels using individual colors
  updated_plot <- clean_plot +
    ggrepel::geom_text_repel(
      data = labels_df,
      aes(x = x, y = y, label = ID, color = I(LabelColor)),  # I() forces individual colors
      size = label_size,
      max.overlaps = max_overlaps,
      nudge_x = label_distance,
      nudge_y = label_distance,
      min.segment.length = 0,
      segment.size = line_thickness,
      show.legend = FALSE
    )

  dotplot_debug_log("Enhanced dot plot labels with dot size applied to ALL labeled proteins", 1)
  return(updated_plot)
}
