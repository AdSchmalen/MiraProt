# ==============================================================================
# Heatmap Module - Heatmap Object Builder Helpers
# ==============================================================================
#
# Purpose:
#   Contains helper functions that read Shiny input values and/or construct
#   render-time heatmap parameters. These functions are shared across the
#   creation, rendering, and download layers.
#
# Architecture role:
#   This is a helper utility layer sourced after Heatmap_utils.R and before the
#   creation pipelines. It does not define observers or output bindings.
#
# Structure:
#   1. Color/palette extraction (extract_color_scheme)
#   2. Font and dendrogram settings (extract_font_settings)
#   3. ComplexHeatmap object constructors (create_expression_heatmap_object,
#      create_protein_annotation, create_basemean_heatmap,
#      create_abundance_ratio_heatmap)
#   4. Legend and layout parameter converters (legend_side_from_input,
#      legend_direction_from_input, heatmap_draw_padding_from_input, etc.)
#
# Important notes for future developers:
#   - All functions accept explicit arguments (input, fs, etc.) rather than
#     reading from the reactive environment directly, making them testable
#     outside a Shiny session.
#   - The statistical analysis pipeline (heatmap_perform_statistical_analysis)
#     that appears at the end of the "Statistical Analysis" comment in this file
#     is actually defined in Heatmap_utils.R. The section header here is a
#     placeholder that marks the boundary.
#
# Sourced by: Heatmap_module.R (after Heatmap_reactive_state.R)
# ==============================================================================
# Build a two-line plotmath title with IDENTICAL line bounding boxes.
#
# atop() stacks two lines; the gap it inserts is driven by the ascent of
# line 1 and the descent of line 2 (and by each line's own descent/ascent).
# If any metric differs between titles, the baseline-to-baseline distance
# differs and the colorbars below them no longer align across legends.
#
# Both lines are wrapped in the SAME vphantom() sizing token, so every line
# reserves the same ascent AND descent regardless of what is drawn:
#
#   "(Ap)"[2] covers paren ascent (taller than cap height), paren/p descent
#   below baseline, and subscript depth from the plotmath "[2]".
#
# That phantom dominates every visible line we use — capitalised words,
# parens with or without a descender, and "log"[2] — so each line's bbox
# collapses to the phantom's bbox. atop()'s spacing then becomes a pure
# function of the phantom and is identical across titles.
#
# vphantom() has zero width, so horizontal centering of the visible text is
# unaffected.
#
# The "2" in log2 is rendered as a plotmath subscript, not the Unicode
# character U+2082, because the Grid module's SVG export cannot embed that
# code point reliably.
build_horizontal_legend_title <- function(line1, line2) {
  as.expression(bquote(
    atop(
      .(line1) * vphantom("(Ap)"[2]),
      .(line2) * vphantom("(Ap)"[2])
    )
  ))
}

extract_color_scheme <- function(input) {
  tryCatch({
    default_colors <- c("purple", "white", "#FFE100")
    if (is.null(input)) return(default_colors)
    colors <- c(
      input$Heatmap_ColorInput_1 %||% default_colors[1],
      input$Heatmap_ColorInput_2 %||% default_colors[2],
      input$Heatmap_ColorInput_3 %||% default_colors[3]
    )
    valid_colors <- sapply(colors, function(color) {
      tryCatch({ grDevices::col2rgb(color); color }, error = function(e) NULL)
    })
    for (i in seq_along(valid_colors)) if (is.null(valid_colors[[i]])) valid_colors[[i]] <- default_colors[i]
    unlist(valid_colors)
  }, error = function(e) {
    heatmap_debug_log(paste("Error extracting color scheme:", e$message), 1)
    c("purple", "white", "#FFE100")
  })
}

# Get font settings and dendrogram visibility flags from UI input
# Backward compatible with legacy input IDs

extract_font_settings <- function(input) {
  tryCatch({
    if (is.null(input)) {
      return(list(
        row_font_size = 8,
        col_font_size = 10,
        show_row_dend = TRUE,
        show_col_dend = TRUE
      ))
    }
    list(
      row_font_size = (input$row_font_size %||% input$font_size_rows) %||% 8,
      col_font_size = (input$col_font_size %||% input$font_size_columns) %||% 10,
      show_row_dend = (input$show_row_dendrogram %||% input$show_row_dend) %||% TRUE,
      show_col_dend = (input$show_column_dendrogram %||% input$show_column_dend) %||% TRUE,
      legend_title_font_size = input$legend_title_font_size %||% 12,
      legend_text_font_size  = input$legend_text_font_size %||% 10
    )
  }, error = function(e) {
    heatmap_debug_log(paste("Error extracting font settings:", e$message), 1)
    list(
      row_font_size = 8,
      col_font_size = 10,
      show_row_dend = TRUE,
      show_col_dend = TRUE,
      legend_title_font_size = 12,
      legend_text_font_size  = 10
    )
  })
}

create_expression_heatmap_object <- function(scaled_matrix, groups, input, cluster_info) {
  tryCatch({
    pal <- extract_color_scheme(input)
    # symmetric diverging around 0 for Z-scores
    min_val <- min(scaled_matrix, na.rm = TRUE)
    max_val <- max(scaled_matrix, na.rm = TRUE)
    a <- max(abs(c(min_val, max_val))); if (!is.finite(a) || a == 0) a <- 1
    col_fun <- circlize::colorRamp2(c(-a, 0, a), pal)

    fs <- extract_font_settings(input)

    show_row_names <- isTRUE(input$show_expr_row_labels)
    show_col_names <- isTRUE(input$show_expr_col_labels)
    missing_tile_color <- input$missing_value_color_heatmap %||% "#E0E0E0"

    # Dendrogram visibility only; order is locked via row_order/column_order.
    want_row_dend <- isTRUE(input$show_row_dendrogram)
    want_col_dend <- isTRUE(input$show_column_dendrogram)

    row_dend_obj <- NULL
    col_dend_obj <- NULL
    if (want_row_dend && !is.null(cluster_info$row_dend)) {
      row_dend_obj <- rotate_dend_to_order(cluster_info$row_dend, rownames(scaled_matrix)[cluster_info$row_order_idx])
      row_dend_obj <- ensure_dendrogram_order(row_dend_obj, rownames(scaled_matrix)[cluster_info$row_order_idx], "row")
    }
    if (want_col_dend && !is.null(cluster_info$col_dend)) {
      sample_sort_mode <- input$sort_samples_by %||% "none"
      col_dend_strict <- !(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))
      col_dend_obj <- rotate_dend_to_order(cluster_info$col_dend, colnames(scaled_matrix)[cluster_info$col_order_idx])
      col_dend_obj <- ensure_dendrogram_order(
        col_dend_obj,
        colnames(scaled_matrix)[cluster_info$col_order_idx],
        "column",
        strict_order = col_dend_strict
      )
    }

    # Derive legend direction from UI (top/bottom -> horizontal, left/right -> vertical)
    legend_dir <- legend_direction_from_input(input)

    ComplexHeatmap::Heatmap(
      matrix = scaled_matrix,
      name = "Expression",
      col = col_fun,
      na_col = missing_tile_color,

      # Names and sides
      show_row_names = show_row_names,
      show_column_names = show_col_names,
      row_names_side = "left",
      row_names_gp = safe_gp(fs$row_font_size),
      column_names_gp = safe_gp(fs$col_font_size),
      column_names_rot = 90,

      # Lock final order (no clustering to reorder on draw)
      cluster_rows = if (!is.null(row_dend_obj)) row_dend_obj else FALSE,
      cluster_columns = if (!is.null(col_dend_obj)) col_dend_obj else FALSE,
      row_dend_reorder = FALSE,
      column_dend_reorder = FALSE,
      row_order = cluster_info$row_order_idx,
      column_order = cluster_info$col_order_idx,

      # Show dendrograms if requested
      show_row_dend = !is.null(row_dend_obj),
      show_column_dend = !is.null(col_dend_obj),

      column_title = NULL,  # CHANGED: Remove title
      heatmap_legend_param = list(
        title = if (legend_dir == "horizontal") {
          build_horizontal_legend_title(quote("Abundance Heatmap"), quote("Z-Score"))
        } else {
          expression(atop("Abundance Heatmap"[phantom(2)], "Z-Score"))
        },
        legend_direction = legend_dir,
        title_position = if (legend_dir == "horizontal") "topcenter" else "topleft",
        title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
        labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
      )
    )
  }, error = function(e) {
    stop(paste("Error creating expression heatmap:", e$message))
  })
}

# Row-wise Z-score transformation that keeps NA cells as NA and uses available values per row.

create_basemean_heatmap <- function(basemean_values, input = NULL) {
  tryCatch({
    skip_log_transform <- isTRUE(input$skip_log_transform_heatmap)
    heatmap_debug_log(
      if (skip_log_transform) {
        "Creating aligned raw basemean heatmap (NO CLUSTERING)"
      } else {
        "Creating aligned log2 basemean heatmap (NO CLUSTERING)"
      },
      2
    )
    if (is.null(basemean_values) || length(basemean_values) == 0) {
      heatmap_debug_log("No basemean values available for heatmap creation", 2)
      return(NULL)
    }

    pal <- extract_color_scheme(input)
    col_fun <- col_fun_for_single_column_values(basemean_values, pal)

    m <- matrix(basemean_values, ncol = 1)
    rownames(m) <- names(basemean_values)
    colnames(m) <- if (skip_log_transform) "Basemean" else "log2(Basemean)"
    basemean_column_label <- if (skip_log_transform) "Basemean" else expression(log[2](Basemean))

    # DEBUG
    if (!is.null(rownames(m)) && length(rownames(m)) > 0) {
      first_5_rows <- head(rownames(m), 5)
      heatmap_debug_log(paste("Basemean heatmap row order (first 5):", paste(first_5_rows, collapse = ", ")), 1)
      heatmap_debug_log(paste("Total basemean heatmap rows:", nrow(m)), 1)
    } else {
      heatmap_debug_log("Basemean heatmap has no row names", 1)
    }

    fs <- extract_font_settings(input)
    legend_dir <- legend_direction_from_input(input)

    ComplexHeatmap::Heatmap(
      m,
      name = if (skip_log_transform) "basemean" else "log2_basemean",
      col = col_fun,
      width = grid::unit(8, "mm"),
      show_row_names = isTRUE(input$show_basemean_row_labels),
      show_column_names = isTRUE(input$show_basemean_col_labels),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      show_row_dend = FALSE,
      show_column_dend = FALSE,
      row_order = seq_len(nrow(m)),
      column_order = seq_len(ncol(m)),
      column_labels = basemean_column_label,
      column_names_gp = safe_gp(fs$col_font_size),
      row_names_gp = safe_gp(fs$row_font_size),
      heatmap_legend_param = {
        if (legend_dir == "horizontal") {
          list(
            title = if (skip_log_transform) "Basemean" else build_horizontal_legend_title(quote("log"[2]), quote("(Basemean)")),
            legend_direction = "horizontal",
            title_position = "topcenter",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size),
            # shorter horizontal bar (closer to default look)
            legend_width  = grid::unit(25, "mm"),
            legend_height = grid::unit(4, "mm")
          )
        } else {
          list(
            title = if (skip_log_transform) "Basemean" else expression(log[2](Basemean)),
            legend_direction = "vertical",
            title_position = "topleft",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
          )
        }
      }
    )
  }, error = function(e) {
    heatmap_debug_log(paste("Error creating aligned basemean heatmap:", e$message), 1)
    NULL
  })
}

# Updated create_abundance_ratio_heatmap with strict alignment (NO CLUSTERING)

create_abundance_ratio_heatmap <- function(ratio_values, input = NULL) {
  tryCatch({
    heatmap_debug_log("Creating aligned log2 abundance ratio heatmap (NO CLUSTERING)", 2)
    if (is.null(ratio_values) || length(ratio_values) == 0) {
      heatmap_debug_log("No abundance ratio values available for heatmap creation", 2)
      return(NULL)
    }

    v <- as.numeric(ratio_values)
    v[!is.finite(v)] <- 0

    pal <- extract_color_scheme(input)
    col_fun <- col_fun_for_single_column_values(v, pal)

    ratio_column_label <- expression(log[2]("Abundance Ratio"))

    col_label <- "log2(Abundance Ratio)"

    m <- matrix(v, ncol = 1, dimnames = list(names(ratio_values), "log2(Abundance Ratio)"))
    m <- validate_ratio_matrix(m, context = "create_abundance_ratio_heatmap")
    if (is.null(m)) {
      heatmap_debug_log("Abundance ratio heatmap creation aborted: empty/invalid restore-time matrix", 1)
      return(NULL)
    }

    # DEBUG
    if (!is.null(rownames(m)) && length(rownames(m)) > 0) {
      first_5_rows <- head(rownames(m), 5)
      heatmap_debug_log(paste("Abundance ratio heatmap row order (first 5):", paste(first_5_rows, collapse = ", ")), 1)
      heatmap_debug_log(paste("Total abundance ratio heatmap rows:", nrow(m)), 1)
    } else {
      heatmap_debug_log("Abundance ratio heatmap has no row names", 1)
    }

    fs <- extract_font_settings(input)
    legend_dir <- legend_direction_from_input(input)

    ComplexHeatmap::Heatmap(
      m,
      name = "log2_ratio",
      col = col_fun,
      width = grid::unit(8, "mm"),
      show_row_names = isTRUE(input$show_abundance_ratio_row_labels),
      show_column_names = isTRUE(input$show_abundance_ratio_col_labels),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      show_row_dend = FALSE,
      show_column_dend = FALSE,
      row_order = seq_len(nrow(m)),
      column_order = seq_len(ncol(m)),
      column_labels = ratio_column_label,
      column_names_gp = safe_gp(fs$col_font_size),
      row_names_gp = safe_gp(fs$row_font_size),
      heatmap_legend_param = {
        if (legend_dir == "horizontal") {
          list(
            title = build_horizontal_legend_title(quote("log"[2]), quote("(Abundance Ratio)")),
            legend_direction = "horizontal",
            title_position = "topcenter",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size),
            # shorter horizontal bar (closer to default look)
            legend_width  = grid::unit(25, "mm"),
            legend_height = grid::unit(4, "mm")
          )
        } else {
          list(
            title = expression(log[2]("Abundance Ratio")),
            legend_direction = "vertical",
            title_position = "topleft",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
          )
        }
      }
    )
  }, error = function(e) {
    heatmap_debug_log(paste("Error creating aligned log2 abundance ratio heatmap:", e$message), 1)
    NULL
  })
}

# ========================================
# ALIGNMENT VERIFICATION FOR EXTENSION HEATMAPS
# ========================================

# Updated calculate_correct_basemean_internal with log2 transformation

create_diagonal_line_cell_fun <- function(input) {
  show_diagonal <- isTRUE(input$show_correlation_diagonal)

  if (!show_diagonal) {
    return(NULL)
  }

  diagonal_color <- input$diagonal_line_color %||% "#FF0000"
  diagonal_width <- input$diagonal_line_width %||% 2
  diagonal_rotate <- isTRUE(input$diagonal_rotate)  # NEW: Check rotate option

  direction_text <- if (diagonal_rotate) "ROTATED (anti-diagonal)" else "NORMAL (main diagonal)"
  heatmap_debug_log(paste("Creating diagonal line cell function:", direction_text,
                  "color:", diagonal_color, "width:", diagonal_width), 1)

  function(j, i, x, y, width, height, fill) {
    if (i == j) {
      if (diagonal_rotate) {
        # ROTATED: Anti-diagonal (bottom-left to top-right)
        grid::grid.lines(
          x = grid::unit(c(0, 1), "npc"),     # Left to right
          y = grid::unit(c(0, 1), "npc"),     # Bottom to top
          vp = grid::viewport(x = x, y = y, width = width, height = height),
          gp = grid::gpar(
            col = diagonal_color,
            lwd = diagonal_width,
            lty = 1,
            lineend = "butt"
          )
        )
      } else {
        # NORMAL: Main diagonal (top-left to bottom-right)
        grid::grid.lines(
          x = grid::unit(c(0, 1), "npc"),     # Left to right
          y = grid::unit(c(1, 0), "npc"),     # Top to bottom
          vp = grid::viewport(x = x, y = y, width = width, height = height),
          gp = grid::gpar(
            col = diagonal_color,
            lwd = diagonal_width,
            lty = 1,
            lineend = "butt"
          )
        )
      }
    }
  }
}

#' Enhanced p-value availability check with broader content type support
#' @param rv reactive values object
#' @param pval_type specific p-value type to check (optional)
#' @param pval_col specific p-value column to check (optional)
#' @return logical indicating if p-value columns are present and contain data

legend_side_from_input <- function(input) {
  side <- tolower(input$legend_position %||% "right")
  if (!side %in% c("left", "right", "top", "bottom")) side <- "right"
  side
}

legend_direction_from_input <- function(input) {
  side <- legend_side_from_input(input)
  if (side %in% c("top", "bottom")) "horizontal" else "vertical"
}

legend_plot_gap_mm_from_input <- function(input) {
  gap_mm <- suppressWarnings(as.numeric(input$legend_plot_gap_heatmap %||% 2))
  if (!is.finite(gap_mm)) gap_mm <- 2
  max(0, min(gap_mm, 40))
}

row_anno_padding_from_input <- function(input) {
  grid::unit(legend_plot_gap_mm_from_input(input), "mm")
}

heatmap_draw_padding_from_input <- function(input, base_padding_mm = c(2, 2, 2, 2)) {
  pad <- as.numeric(base_padding_mm)
  if (length(pad) != 4 || any(!is.finite(pad))) {
    pad <- c(2, 2, 2, 2)
  }

  grid::unit(pad, "mm")
}

# ========================================
# Statistical Analysis
# ========================================
