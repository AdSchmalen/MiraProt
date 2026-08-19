# ==============================================================================
# File: modules/PCA/pca_module_static.R
#
# Purpose:
#   Provides create_static_plot(), the function responsible for building the
#   ggplot2 static scatter plot for PCA and UMAP results. Supports both
#   sample-mode (condition-colored) and protein-mode (custom label/dot colors)
#   rendering, including enhanced labeling and optional convex-hull polygons.
#
# Architectural Role:
#   This file contains only pure plot-building functions. It has no Shiny
#   dependency and no access to reactive state. All inputs are plain R objects
#   passed as arguments. The result is a ggplot2 object that is stored by the
#   rendering pipeline (register_pca_rendering_core in
#   pca_module_server_pipeline.R) in the static_plot_obj reactiveVal.
#
# Structure:
#   1. create_static_plot(results, plot_params, labeled_proteins,
#                         theme_name, font_sizes, enhanced_labeling,
#                         legend_position)
#      - Dispatches to sample or protein rendering branch
#      - Applies theme, font sizes, and legend position
#      - Calls ggrepel for label placement when labeling is active
#
# Notes for future developers:
#   - debug_log is accepted via plot_params$debug_log. Pass it explicitly when
#     calling create_static_plot.
#   - If new analysis methods are added, ensure the results structure is
#     compatible with create_plot_data() and get_plot_coordinates() in
#     pca_module_utils.R before extending this file.
# ==============================================================================

#' Create static dimension reduction plot with enhanced labeling system
#' @param results Analysis results from dimension reduction
#' @param plot_params List of plot parameters (axis_x, axis_y, etc.)
#' @param labeled_proteins Vector of protein names to label (legacy support)
#' @param theme_name Name of ggplot2 theme to use
#' @param font_sizes List of font sizes for different elements
#' @param enhanced_labeling List containing: selected_items, item_settings, labeling_params
#' @param legend_position Legend position ("right", "left", "top", "bottom", "none")
#' @return ggplot object
create_static_plot <- function(results,
                               plot_params,
                               labeled_proteins = NULL,
                               theme_name = "theme_minimal",
                               font_sizes = NULL,
                               enhanced_labeling = NULL,
                               legend_position = "right") {

  debug_log <- plot_params$debug_log %||% function(msg, level) cat("DEBUG:", msg, "\n")
  static_plot_diagnostics <- isTRUE(getOption("miraprot.pca.static_plot_diagnostics", FALSE))
  static_diagnostic_log <- function(message, level = 3) {
    if (static_plot_diagnostics) debug_log(message, level)
  }
  static_diagnostic_log("=== STATIC PLOT CREATION START ===", 3)

  # Extract basic parameters
  axis_x <- plot_params$axis_x %||% "PC1"
  axis_y <- plot_params$axis_y %||% "PC2"
  point_size <- plot_params$point_size %||% 3
  label_size <- plot_params$label_size %||% 4
  color_palette <- plot_params$color_palette %||% "Set1"
  reverse_colors <- plot_params$reverse_colors %||% FALSE
  default_protein_color <- plot_params$default_protein_color %||% "#3182bd"

  # Font sizes
  axis_title_size   <- if (!is.null(font_sizes)) as.numeric(font_sizes$axis_title)   else 20
  tick_size         <- if (!is.null(font_sizes)) as.numeric(font_sizes$tick)         else 16
  legend_title_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$legend_title) else 18
  legend_text_size  <- if (!is.null(font_sizes)) as.numeric(font_sizes$legend_text)  else 14

  static_diagnostic_log(paste("Params: X=", axis_x, " Y=", axis_y,
                              "| point_size=", point_size,
                              "| default_protein_color=", default_protein_color), 3)
  static_diagnostic_log(paste("Mode comparison_target:", results$comparison_target), 3)

  # Build plot_data
  static_detail_log <- function(message, level = 1) {
    if (grepl("^Condition assignment:", message)) level <- max(level, 3)
    debug_log(message, level)
  }
  plot_data <- create_plot_data(results,
                                results$raw_metadata %||% results$metadata,
                                plot_params$identifier_col,
                                debug_log = static_detail_log)
  static_diagnostic_log(paste("plot_data rows:", nrow(plot_data)), 3)

  if (identical(results$comparison_target, "proteins")) {
    invariant_counts <- c(
      coordinates = nrow(results$coordinates),
      point_names = length(results$point_names %||% character()),
      plot_data = nrow(plot_data)
    )
    if (length(unique(invariant_counts)) != 1L) {
      stop(sprintf(
        "Protein plot alignment failed: coordinate rows=%d, point_names=%d, plot rows=%d",
        invariant_counts[["coordinates"]], invariant_counts[["point_names"]],
        invariant_counts[["plot_data"]]
      ), call. = FALSE)
    }
  }

  # Add coordinates for selected axes
  coords <- get_plot_coordinates(results, axis_x, axis_y)
  plot_data$x <- coords$x
  plot_data$y <- coords$y

  # Axis labels (with variance info if available)
  axis_labels <- get_axis_labels(results, axis_x, axis_y, debug_log = debug_log)

  # Base ggplot scaffold
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    labs(x = axis_labels$x, y = axis_labels$y)

  # =========================
  # SAMPLE MODE (with conditions)
  # =========================
  if ("Condition" %in% colnames(plot_data) &&
      results$comparison_target == "samples") {

    unique_conditions <- unique(plot_data$Condition[!is.na(plot_data$Condition)])
    n_conditions <- length(unique_conditions)
    polygons_added <- 0

    if (n_conditions > 0) {
      # Build palette
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
          tmp <- rainbow(n_conditions)
          if (reverse_colors) tmp <- rev(tmp)
          tmp
        })
      }
      names(plot_colors) <- unique_conditions

      # Optional polygons (convex hulls) only if >1 condition
      if (n_conditions > 1) {
        hull_all <- data.frame()
        for (cond in unique_conditions) {
          cond_rows <- plot_data[!is.na(plot_data$Condition) &
                                   plot_data$Condition == cond, , drop = FALSE]
          if (nrow(cond_rows) >= 3) {
            hi <- chull(cond_rows$x, cond_rows$y)
            hull_all <- rbind(hull_all, cond_rows[hi, , drop = FALSE])
          }
        }
        if (nrow(hull_all) > 0) {
          polygons_added <- length(unique(hull_all$Condition[!is.na(hull_all$Condition)]))
          p <- p +
            geom_polygon(
              data = hull_all,
              aes(x = x, y = y, fill = Condition, color = Condition),
              alpha = 0.2,
              linewidth = 0.8,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
        }
      }

      debug_log(sprintf(
        "Condition assignment: %d/%d samples matched; polygons=%d",
        sum(!is.na(plot_data$Condition)),
        nrow(plot_data),
        polygons_added
      ), 2)

      p <- p +
        geom_point(aes(color = Condition), size = point_size) +
        scale_color_manual(values = plot_colors) +
        scale_fill_manual(values = plot_colors)
    } else {
      # Fallback: no valid conditions
      p <- p + geom_point(size = point_size, color = "grey50")
    }

  } else {
    # =========================
    # PROTEIN MODE BASE LAYER
    # =========================
    debug_log("Protein mode: constructing base layer (exclude labeled items)", 2)

    base_data <- plot_data

    # Remove all labeled proteins (will be redrawn with labeled_dot_size)
    if (!is.null(enhanced_labeling) &&
        enhanced_labeling$mode == "proteins" &&
        !is.null(enhanced_labeling$selected_items) &&
        length(enhanced_labeling$selected_items) > 0) {
      labeled_ids <- unique(enhanced_labeling$selected_items)
      before_n <- nrow(base_data)
      base_data <- base_data[!base_data$Name %in% labeled_ids, , drop = FALSE]
      debug_log(paste("Removed", before_n - nrow(base_data),
                      "labeled proteins from base layer"), 2)
    }

    if (nrow(base_data) > 0) {
      p <- p + geom_point(
        data = base_data,
        aes(x = x, y = y),
        size = point_size,
        color = default_protein_color
      )
    } else {
      debug_log("Base layer empty (all proteins labeled) - skipping base points", 2)
    }
  }

  # ========================================
  # ENHANCED LABELING SYSTEM
  # ========================================
  use_enhanced_labeling <- !is.null(enhanced_labeling) && (
    (!is.null(enhanced_labeling$selected_items) &&
       length(enhanced_labeling$selected_items) > 0) ||
      (enhanced_labeling$mode == "samples" &&
         isTRUE(enhanced_labeling$label_all_samples))
  )

  if (use_enhanced_labeling) {
    labeling_mode <- enhanced_labeling$mode %||% "proteins"

    if (labeling_mode == "samples") {
      # =========================
      # SAMPLE LABELING MODE
      # =========================
      if (isTRUE(enhanced_labeling$label_all_samples)) {
        master_label_color   <- enhanced_labeling$master_label_color %||% "#000000"
        master_dot_color     <- enhanced_labeling$master_dot_color %||% "#E0E0E0"
        use_master_dot_color <- enhanced_labeling$use_master_dot_color %||% FALSE
        lp                   <- enhanced_labeling$labeling_params %||% list()

        max_overlaps   <- lp$max_overlaps   %||% 10
        label_distance <- lp$label_distance %||% 0.25
        line_thickness <- lp$line_thickness %||% 0.5
        enhanced_label_size <- lp$label_size %||% 8
        labeled_dot_size    <- lp$labeled_dot_size %||% 4

        label_data <- plot_data
        label_data$LabelColor <- master_label_color
        label_data$CustomDotColor <- master_dot_color
        label_data$UseCustomDotColor <- use_master_dot_color

        # Dots (all samples) with chosen size
        if (use_master_dot_color) {
          p <- p + geom_point(
            data = label_data,
            aes(x = x, y = y),
            color = master_dot_color,
            size = labeled_dot_size,
            alpha = 1
          )
        } else {
          # Preserve original condition coloring if present
          if ("Condition" %in% colnames(label_data)) {
            p <- p + geom_point(
              data = label_data,
              aes(x = x, y = y, color = Condition),
              size = labeled_dot_size,
              alpha = 1
            )
          } else {
            p <- p + geom_point(
              data = label_data,
              aes(x = x, y = y),
              size = labeled_dot_size,
              alpha = 1
            )
          }
        }

        # Labels
        p <- p + ggrepel::geom_text_repel(
          data = label_data,
          aes(x = x, y = y, label = Name, color = I(master_label_color)),
          size = enhanced_label_size,
          max.overlaps = max_overlaps,
          nudge_x = label_distance,
          nudge_y = label_distance,
          min.segment.length = 0,
          segment.size = line_thickness,
          show.legend = FALSE
        )
      }

    } else {
      # =========================
      # PROTEIN LABELING MODE
      # =========================
      selected_items <- enhanced_labeling$selected_items
      item_settings  <- enhanced_labeling$item_settings %||% data.frame()
      lp             <- enhanced_labeling$labeling_params %||% list()

      max_overlaps       <- lp$max_overlaps   %||% 10
      label_distance     <- lp$label_distance %||% 0.25
      line_thickness     <- lp$line_thickness %||% 0.5
      enhanced_label_size <- lp$label_size    %||% 8
      labeled_dot_size    <- lp$labeled_dot_size %||% 4

      label_data <- plot_data[plot_data$Name %in% selected_items, , drop = FALSE]

      if (nrow(label_data) > 0) {
        # Initialize defaults
        label_data$LabelColor <- "#000000"
        label_data$CustomDotColor <- "#E0E0E0"
        label_data$UseCustomDotColor <- FALSE

        # Apply stored item settings
        if (nrow(item_settings) > 0) {
          for (i in seq_len(nrow(item_settings))) {
            itm <- item_settings$item_id[i]
            idx <- which(label_data$Name == itm)
            if (length(idx) > 0) {
              label_data$LabelColor[idx]       <- item_settings$label_color[i]
              label_data$CustomDotColor[idx]   <- item_settings$dot_color[i]
              label_data$UseCustomDotColor[idx]<- item_settings$use_custom_dot_color[i]
            }
          }
        }

        # Labels (text)
        p <- p + ggrepel::geom_text_repel(
          data = label_data,
          aes(x = x, y = y, label = Name),
          color = label_data$LabelColor,
          size = enhanced_label_size,
          max.overlaps = max_overlaps,
          nudge_x = label_distance,
          nudge_y = label_distance,
          min.segment.length = 0,
          segment.size = line_thickness,
          show.legend = FALSE
        )

        # Split labeled proteins for dot overlay
        labeled_with_custom    <- label_data[label_data$UseCustomDotColor == TRUE, , drop = FALSE]
        labeled_without_custom <- label_data[label_data$UseCustomDotColor == FALSE | is.na(label_data$UseCustomDotColor), , drop = FALSE]

        # Labeled WITHOUT custom color: default_protein_color, labeled_dot_size
        if (nrow(labeled_without_custom) > 0) {
          p <- p + geom_point(
            data = labeled_without_custom,
            aes(x = x, y = y),
            size = labeled_dot_size,
            color = default_protein_color,
            alpha = 1
          )
        }

        # Labeled WITH custom color: custom color, same labeled_dot_size
        if (nrow(labeled_with_custom) > 0) {
          p <- p + geom_point(
            data = labeled_with_custom,
            aes(x = x, y = y),
            size = labeled_dot_size,
            color = labeled_with_custom$CustomDotColor,
            alpha = 1
          )
        }
      }
    }

  } else if (!is.null(labeled_proteins) && length(labeled_proteins) > 0) {
    # Legacy labeling fallback
    leg_data <- plot_data[plot_data$Name %in% labeled_proteins, , drop = FALSE]
    if (nrow(leg_data) > 0) {
      p <- p + ggrepel::geom_text_repel(
        data = leg_data,
        aes(label = Name),
        size = label_size,
        box.padding = 0.5,
        point.padding = 0.5,
        segment.color = "gray50",
        max.overlaps = Inf
      )
    }
  }

  # Theme
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

  condition_count <- if ("Condition" %in% colnames(plot_data)) {
    length(unique(plot_data$Condition[!is.na(plot_data$Condition)]))
  } else {
    0L
  }
  debug_log(paste0("Static PCA plot created | x=", axis_x,
                   " | y=", axis_y,
                   " | points=", nrow(plot_data),
                   " | conditions=", condition_count), 2)
  static_diagnostic_log("=== STATIC PLOT CREATION END ===", 3)
  return(p)
}

#' Create scree plot for PCA
#' @param results PCA results object
#' @return ggplot object or NULL
create_scree_plot <- function(results, font_sizes = NULL, theme_name = "theme_minimal", debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(...) invisible(NULL)
  if (is.null(results) || results$method != "pca") return(NULL)
  if (is.null(results$var_explained) || length(results$var_explained) == 0) return(NULL)

  # Extract font sizes with defaults
  axis_title_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$axis_title) else 20
  tick_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$tick) else 16
  label_size <- if (!is.null(font_sizes)) as.numeric(font_sizes$label_size) else 3
  if (is.na(label_size)) label_size <- 3

  tryCatch({
    # Limit to reasonable number of components for display
    n_show <- min(length(results$var_explained), 20)

    # Create scree plot data
    scree_data <- data.frame(
      PC = factor(1:n_show, levels = 1:n_show),
      Variance = results$var_explained[1:n_show],
      Cumulative = results$cumvar_explained[1:n_show]
    )

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

    # Create plot
    p <- ggplot(scree_data, aes(x = PC)) +
      geom_col(aes(y = Variance), fill = "steelblue", alpha = 0.8) +
      geom_line(aes(y = Cumulative, group = 1), color = "red", size = 1.5) +
      geom_point(aes(y = Cumulative), color = "red", size = label_size) +
      labs(x = "Principal Component",
           y = "Variance Explained (%)",
           title = "Scree Plot - Variance Explained by Principal Components") +
      scale_y_continuous(
        limits = c(0, 100),
        sec.axis = sec_axis(~., name = "Cumulative Variance (%)")
      ) +
      theme_func +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = tick_size),
        axis.text.y = element_text(size = tick_size),
        axis.text.y.right = element_text(color = "red", size = tick_size),
        axis.title = element_text(size = axis_title_size),
        axis.title.y.right = element_text(color = "red", size = axis_title_size),
        plot.title = element_text(hjust = 0.5, size = axis_title_size + 2)
      )

    # Add 80% threshold line if applicable
    if (any(scree_data$Cumulative < 80)) {
      p <- p + geom_hline(yintercept = 80, linetype = "dashed",
                          color = "gray50", alpha = 0.7)
    }

    return(p)

  }, error = function(e) {
    debug_log(paste("Error creating scree plot:", e$message), 1)
    return(NULL)
  })
}

#' Find protein matches in data
#' @param search_terms Character vector of search terms
#' @param data Data frame with protein information
#' @param identifier_col Column to search in
#' @param max_suggestions Maximum number of suggestions per term
#' @return Character vector of matching proteins
find_protein_matches <- function(search_terms, data, identifier_col, max_suggestions = 10) {

  if (is.null(search_terms) || length(search_terms) == 0) {
    return(character())
  }

  if (is.null(data) || nrow(data) == 0) {
    return(character())
  }

  if (is.null(identifier_col) || !identifier_col %in% colnames(data)) {
    return(character())
  }

  all_matches <- character()

  # Get all unique identifiers
  all_identifiers <- unique(as.character(data[[identifier_col]]))
  all_identifiers <- all_identifiers[!is.na(all_identifiers) & all_identifiers != ""]

  for (term in search_terms) {
    term <- trimws(term)
    if (term == "") next

    # Try exact match first (case-insensitive)
    exact_matches <- all_identifiers[tolower(all_identifiers) == tolower(term)]

    if (length(exact_matches) > 0) {
      all_matches <- c(all_matches, exact_matches)
    } else {
      # Try prefix match (starts with)
      pattern <- paste0("^", gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", term))
      prefix_matches <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)

      if (length(prefix_matches) > 0) {
        # Sort by length (shorter matches first)
        prefix_matches <- prefix_matches[order(nchar(prefix_matches))]
        if (length(prefix_matches) > max_suggestions) {
          prefix_matches <- prefix_matches[1:max_suggestions]
        }
        all_matches <- c(all_matches, prefix_matches)
      } else {
        # Try substring match
        pattern <- gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", term)
        substring_matches <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)

        if (length(substring_matches) > 0) {
          # Sort by position of match (earlier matches first)
          match_positions <- regexpr(pattern, substring_matches, ignore.case = TRUE)
          substring_matches <- substring_matches[order(match_positions)]

          if (length(substring_matches) > max_suggestions) {
            substring_matches <- substring_matches[1:max_suggestions]
          }
          all_matches <- c(all_matches, substring_matches)
        }
      }
    }
  }

  # Remove duplicates while preserving order
  return(unique(all_matches))
}

#' Create biplot combining samples and loadings
#' @param results Analysis results
#' @param plot_params Plot parameters
#' @param scale_factor Factor to scale loadings
#' @return ggplot object or NULL
create_biplot <- function(results, plot_params, scale_factor = 1) {
  if (!results$method %in% c("pca", "plsda")) return(NULL)

  # Get sample plot
  sample_plot_data <- create_plot_data(results, results$raw_metadata %||% results$metadata, plot_params$identifier_col)

  # Get coordinates
  coords <- get_plot_coordinates(results, plot_params$axis_x, plot_params$axis_y)
  sample_plot_data$x <- coords$x
  sample_plot_data$y <- coords$y

  # Get loadings
  x_comp <- as.numeric(gsub("PC|Comp", "", plot_params$axis_x))
  y_comp <- as.numeric(gsub("PC|Comp", "", plot_params$axis_y))

  if (x_comp > ncol(results$loadings) || y_comp > ncol(results$loadings)) return(NULL)

  # Scale loadings to fit on same plot
  loading_scale <- scale_factor * max(abs(c(sample_plot_data$x, sample_plot_data$y))) /
    max(abs(results$loadings[, c(x_comp, y_comp)]))

  loadings_data <- data.frame(
    Feature = rownames(results$loadings),
    x = results$loadings[, x_comp] * loading_scale,
    y = results$loadings[, y_comp] * loading_scale,
    stringsAsFactors = FALSE
  )

  # Calculate loading magnitudes for labeling
  loadings_data$magnitude <- sqrt(loadings_data$x^2 + loadings_data$y^2)
  top_loadings <- loadings_data[order(loadings_data$magnitude, decreasing = TRUE)[1:10], ]

  # Get axis labels
  axis_labels <- get_axis_labels(results, plot_params$axis_x, plot_params$axis_y)

  # Create plot
  p <- ggplot() +
    # Sample points
    geom_point(data = sample_plot_data,
               aes(x = x, y = y, color = if("Condition" %in% names(sample_plot_data)) Condition else NULL),
               size = plot_params$point_size) +
    # Loading arrows
    geom_segment(data = loadings_data,
                 aes(x = 0, y = 0, xend = x, yend = y),
                 arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 0.5, color = "gray50") +
    # Loading labels
    ggrepel::geom_text_repel(data = top_loadings,
                             aes(x = x, y = y, label = Feature),
                             size = label_size, color = "gray30") +
    labs(x = axis_labels$x, y = axis_labels$y, title = "Biplot") +
    theme_func +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3)

  # Add color scale if conditions present
  if ("Condition" %in% colnames(sample_plot_data)) {
    if (plot_params$color_palette %in% c("Viridis", "Plasma", "Inferno", "Magma")) {
      p <- p + scale_color_viridis_d(option = tolower(plot_params$color_palette),
                                     direction = ifelse(plot_params$reverse_colors, -1, 1))
    } else {
      n_colors <- length(unique(sample_plot_data$Condition))
      colors <- RColorBrewer::brewer.pal(min(n_colors, 12), plot_params$color_palette)
      if (plot_params$reverse_colors) colors <- rev(colors)
      p <- p + scale_color_manual(values = colors)
    }
  }

  return(p)
}
