# ==============================================================================
# Heatmap Module - Correlation Heatmap Creation
# ==============================================================================
#
# Purpose:
#   Implements the correlation heatmap creation pipeline: protein-vs-protein
#   correlation and sample-vs-sample correlation. Both pipelines produce
#   ComplexHeatmap objects that are axis-aligned with the expression heatmap
#   produced by Heatmap_create_expression.R.
#
# Architecture role:
#   This file sits between the expression creation layer and the rendering
#   layer. It depends on the expression matrix stored in heatmap_expression_matrix()
#   and on helpers from Heatmap_utils.R and Heatmap_creation.R.
#
# Structure:
#   1. Axis synchronization helpers (protein and sample correlation)
#   2. heatmap_create_protein_correlation - builds protein-correlation heatmap
#   3. heatmap_create_sample_correlation  - builds sample-correlation heatmap
#   4. align_expr_protein_by_row_labels   - post-creation alignment fix
#
# Important notes for future developers:
#   - Both creation functions receive the expression matrix as an argument; they
#     do not read heatmap_expression_matrix() directly.
#   - heatmap_resolve_final_sample_order is used by the public API in
#     Heatmap_module.R to re-order sample correlation matrices for export.
#
# Sourced by: Heatmap_module.R (after Heatmap_create_expression.R)
# ==============================================================================

    # ========================================
    # Aligned Correlation Heatmaps
    # ========================================
    # Last-moment safety: enforce identical axis order for protein-correlation matrices
    # immediately before building/drawing the heatmap.
    heatmap_sync_protein_correlation_axes <- function(cor_mat, canonical_order = NULL, context = "") {
      if (is.null(cor_mat) || !is.matrix(cor_mat) || nrow(cor_mat) == 0 || ncol(cor_mat) == 0) return(cor_mat)
      ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""

      # Optional canonical order pre-pass
      if (!is.null(canonical_order)) {
        canonical_order <- as.character(canonical_order)
        row_ids <- rownames(cor_mat)
        col_ids <- colnames(cor_mat)
        keep_idx <- which(!is.na(canonical_order) & canonical_order %in% row_ids & canonical_order %in% col_ids)
        shared <- canonical_order[keep_idx]
        if (length(shared) > 0) {
          # Keep canonical order exactly (including duplicates) so row count
          # stays aligned with the paired expression/basemean/ratio heatmaps.
          cor_mat <- cor_mat[shared, shared, drop = FALSE]
        }
      }

      # Final hard lock: columns must mirror row order exactly.
      row_ids <- rownames(cor_mat)
      col_ids <- colnames(cor_mat)
      if (!identical(row_ids, col_ids)) {
        heatmap_debug_log(paste0(ctx, "Protein correlation axis mismatch before render. Reordering columns to row order."), 1)
        row_keep <- row_ids[row_ids %in% col_ids]
        if (length(row_keep) > 0) {
          cor_mat <- cor_mat[row_keep, row_keep, drop = FALSE]
        }
      }

      # Name lock (for any residual mismatch after subsetting)
      colnames(cor_mat) <- rownames(cor_mat)
      cor_mat
    }

    heatmap_validate_protein_correlation_axes <- function(cor_mat, context = "") {
      if (is.null(cor_mat) || !is.matrix(cor_mat) || nrow(cor_mat) == 0 || ncol(cor_mat) == 0) {
        return(FALSE)
      }
      ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
      row_ids <- rownames(cor_mat)
      col_ids <- colnames(cor_mat)
      axes_identical <- identical(row_ids, col_ids)
      if (!axes_identical) {
        heatmap_debug_log(paste0(ctx, "INVARIANT VIOLATION: protein correlation row/column order differs. First row labels: ",
                         paste(head(row_ids, 5), collapse = ", "),
                         " | First column labels: ",
                         paste(head(col_ids, 5), collapse = ", ")), 1)
      }
      axes_identical
    }

    # Last-moment safety: for sample-correlation matrices, rows must mirror
    # final expression column order and columns must mirror rows.
    heatmap_sync_sample_correlation_axes <- function(cor_mat, canonical_order = NULL, context = "") {
      if (is.null(cor_mat) || !is.matrix(cor_mat) || nrow(cor_mat) == 0 || ncol(cor_mat) == 0) return(cor_mat)
      ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""

      if (!is.null(canonical_order)) {
        canonical_order <- as.character(canonical_order)
        row_ids <- rownames(cor_mat)
        col_ids <- colnames(cor_mat)
        keep_idx <- which(!is.na(canonical_order) & canonical_order %in% row_ids & canonical_order %in% col_ids)
        shared <- canonical_order[keep_idx]
        if (length(shared) > 0) {
          cor_mat <- cor_mat[shared, shared, drop = FALSE]
        }
      }

      row_ids <- rownames(cor_mat)
      col_ids <- colnames(cor_mat)
      if (!identical(row_ids, col_ids)) {
        heatmap_debug_log(paste0(ctx, "Sample correlation axis mismatch before render. Reordering columns to row order."), 1)
        row_keep <- row_ids[row_ids %in% col_ids]
        if (length(row_keep) > 0) {
          cor_mat <- cor_mat[row_keep, row_keep, drop = FALSE]
        }
      }

      colnames(cor_mat) <- rownames(cor_mat)
      cor_mat
    }

    heatmap_resolve_final_sample_order <- function(expr_colnames, context = "") {
      ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
      expr_colnames <- as.character(expr_colnames %||% character(0))
      shared_cols <- heatmap_shared_col_order()
      fallback_reason <- "none"

      if (!is.null(shared_cols)) {
        final_sample_order <- intersect(as.character(shared_cols), expr_colnames)
        if (!length(final_sample_order)) {
          fallback_reason <- "shared_col_order_no_overlap_used_expression_colnames"
          final_sample_order <- expr_colnames
        }
      } else {
        fallback_reason <- "shared_col_order_missing_used_expression_colnames"
        final_sample_order <- expr_colnames
      }

      heatmap_debug_log(paste0(ctx, "final_sample_order fallback used: ", !identical(fallback_reason, "none"),
                       " reason: ", fallback_reason), if (!identical(fallback_reason, "none")) 1 else 2)

      list(order = final_sample_order, fallback_reason = fallback_reason)
    }

    heatmap_build_locked_sample_correlation <- function(expr_fixed_mat, final_sample_order, context = "") {
      if (is.null(expr_fixed_mat) || !is.matrix(expr_fixed_mat) || ncol(expr_fixed_mat) < 1) {
        return(list(matrix = NULL, fallback_used = TRUE, fallback_reason = "invalid_expression_matrix"))
      }

      ctx <- if (nzchar(context)) paste0("[", context, "] ") else ""
      fallback_used <- FALSE
      fallback_reason <- "none"

      sample_cor_matrix <- suppressWarnings(tryCatch({
        m <- stats::cor(expr_fixed_mat, use = "pairwise.complete.obs", method = "pearson")
        m[is.na(m)] <- 0
        m
      }, error = function(e) {
        fallback_used <<- TRUE
        fallback_reason <<- paste("cor_failed:", e$message)
        matrix(0, nrow = ncol(expr_fixed_mat), ncol = ncol(expr_fixed_mat))
      }))

      rownames(sample_cor_matrix) <- colnames(expr_fixed_mat)
      colnames(sample_cor_matrix) <- colnames(expr_fixed_mat)

      if (length(final_sample_order)) {
        sample_cor_matrix <- sample_cor_matrix[final_sample_order, final_sample_order, drop = FALSE]
      }
      rownames(sample_cor_matrix) <- final_sample_order
      colnames(sample_cor_matrix) <- final_sample_order

      sample_cor_matrix <- heatmap_sync_sample_correlation_axes(
        sample_cor_matrix,
        canonical_order = final_sample_order,
        context = context
      )

      heatmap_debug_log(paste0(ctx, "sample_cor fallback used: ", fallback_used,
                       " reason: ", fallback_reason), if (isTRUE(fallback_used)) 1 else 2)

      list(matrix = sample_cor_matrix, fallback_used = fallback_used, fallback_reason = fallback_reason)
    }

    heatmap_create_protein_correlation <- function(expr_matrix, expression_heatmap = NULL) {
      tryCatch({
        heatmap_debug_log("Creating protein correlation aligned to expression rows", 2)

        if (is.null(expr_matrix) || nrow(expr_matrix) == 0) return(NULL)

        ordered_names <- rownames(expr_matrix)
        shared_rows <- heatmap_shared_row_order()
        if (!is.null(shared_rows)) {
          ordered_names <- intersect(shared_rows, ordered_names)
        } else if (!is.null(expression_heatmap)) {
          expr_core <- tryCatch(extract_core_heatmap(expression_heatmap), error = function(e) expression_heatmap)
          dx <- ComplexHeatmap::draw(expr_core)
          expr_row_order <- ComplexHeatmap::row_order(dx)
          if (is.list(expr_row_order)) expr_row_order <- unlist(expr_row_order, use.names = FALSE)
          ordered_names <- rownames(expr_core@matrix)[expr_row_order]
        }

        # Suppress warnings due to zero variance rows during correlation
        cor_mat <- suppressWarnings(cor(t(expr_matrix), use = "pairwise.complete.obs", method = "pearson"))
        cor_mat[is.na(cor_mat)] <- 0
        rownames(cor_mat) <- rownames(expr_matrix)
        colnames(cor_mat) <- rownames(expr_matrix)

        ordered_names <- intersect(ordered_names, rownames(cor_mat))
        if (length(ordered_names) == 0) return(NULL)
        cor_ord <- cor_mat[ordered_names, ordered_names, drop = FALSE]
        # Hard-lock symmetry and axis naming to the same canonical order.
        cor_ord <- cor_ord[ordered_names, ordered_names, drop = FALSE]
        rownames(cor_ord) <- ordered_names
        colnames(cor_ord) <- ordered_names

        pal <- extract_color_scheme(input)
        enhanced <- isTRUE(input$correlation_enhanced_contrast)
        col_fun <- col_fun_for_correlation(pal, enhanced_contrast = enhanced)
        fs <- extract_font_settings(input)

        # Dendrograms (optional display), but always computed from one symmetric tree
        want_row_dend_requested <- isTRUE(input$show_row_dendrogram)
        sort_method <- input$sort_proteins_by %||% "z_score"
        want_row_dend <- isTRUE(want_row_dend_requested) && identical(sort_method, "pearson_r")
        # Keep a dedicated local flag name to avoid any stale references in this render path.
        allow_protein_row_dend <- want_row_dend
        if (isTRUE(want_row_dend_requested) && !isTRUE(allow_protein_row_dend)) {
          heatmap_debug_log("Protein correlation row dendrogram disabled because sort method is not Pearson r.", 2)
        }
        # Column dendrogram is intentionally disabled for protein-correlation heatmaps.
        # The x-axis order must mirror the canonical row order exactly.
        want_col_dend <- FALSE

        # Use the centralized Pearson-r ordering/dendrogram function for consistency.
        # This is the single source of truth for hclust-based ordering across all render paths.
        pearson_result <- compute_pearson_r_leaf_order(expr_matrix)
        if (!is.null(pearson_result) && identical(sort_method, "pearson_r")) {
          # Overwrite ordered_names with the canonical pearson leaf order.
          ordered_names <- intersect(pearson_result$order, rownames(cor_mat))
          if (length(ordered_names) > 0) {
            cor_ord <- cor_mat[ordered_names, ordered_names, drop = FALSE]
            rownames(cor_ord) <- ordered_names
            colnames(cor_ord) <- ordered_names
          }
        } else if (identical(sort_method, "custom")) {
          ordered_names <- heatmap_apply_custom_protein_order(
            heatmap_custom_fallback_order(expr_matrix, input),
            input$custom_protein_order %||% ""
          )
          if (length(ordered_names) > 0) {
            cor_ord <- cor_mat[ordered_names, ordered_names, drop = FALSE]
            rownames(cor_ord) <- ordered_names
            colnames(cor_ord) <- ordered_names
          }
        }

        row_dend_obj <- NULL
        if (isTRUE(allow_protein_row_dend) && !is.null(pearson_result)) {
          # reindex_dendrogram_leaves() rewrites leaf integers to 1:n so that
          # ComplexHeatmap::draw() does not reorder the already-sorted cor_ord.
          row_dend_obj <- reindex_dendrogram_leaves(pearson_result$dendrogram)
        }

        # Legend direction/position from UI
        legend_dir <- legend_direction_from_input(input)

        cor_ord <- heatmap_sync_protein_correlation_axes(
          cor_ord,
          canonical_order = ordered_names,
          context = "create_protein_correlation"
        )
        if (!heatmap_validate_protein_correlation_axes(cor_ord, "create_protein_correlation")) {
          heatmap_debug_log("Protein correlation creation aborted: axis validation failed after synchronization.", 1)
          heatmap_protein_cor_matrix(NULL)
          return(NULL)
        }

        ht <- ComplexHeatmap::Heatmap(
          matrix = cor_ord,
          name = "Protein Correlation",
          col = col_fun,
          show_row_names = isTRUE(input$show_corr_row_labels),
          show_column_names = isTRUE(input$show_corr_col_labels),
          row_names_gp = safe_gp(fs$row_font_size),
          column_names_gp = safe_gp(fs$col_font_size),
          row_names_side = "right",
          column_names_rot = 90,

          # Dendrogram only for pearson_r mode (verified via ensure_dendrogram_order strict=TRUE).
          # In z_score mode: cluster_rows = FALSE; dendrogram belongs on Expression.
          cluster_rows = if (isTRUE(allow_protein_row_dend) && !is.null(row_dend_obj)) row_dend_obj else FALSE,

          cluster_columns = FALSE,

          row_dend_reorder = FALSE,

          column_dend_reorder = FALSE,
          show_row_dend = isTRUE(allow_protein_row_dend) && !is.null(row_dend_obj),
          show_column_dend = FALSE,

          row_order = seq_len(nrow(cor_ord)),
          column_order = seq_len(ncol(cor_ord)),
          column_title = NULL,
          cell_fun = create_diagonal_line_cell_fun(input),
          heatmap_legend_param = list(
            title = if (legend_dir == "horizontal") {
              build_horizontal_legend_title(quote("Pairwise Correlation"), quote("Pearson r"))
            } else {
              expression(atop("Pairwise Correlation"[phantom(2)], "Pearson r"))
            },
            legend_direction = legend_dir,
            title_position = if (legend_dir == "horizontal") "topcenter" else "topleft",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
          )
        )

        heatmap_protein_cor_matrix(cor_ord)
        ht
      }, error = function(e) {
        heatmap_debug_log(paste("Error creating protein correlation:", e$message), 1)
        NULL
      })
    }

    heatmap_create_sample_correlation <- function(expr_matrix, expression_heatmap = NULL) {
      tryCatch({
        if (is.null(expr_matrix) || ncol(expr_matrix) == 0) return(NULL)

        final_sample <- heatmap_resolve_final_sample_order(
          expr_colnames = colnames(expr_matrix),
          context = "create_sample_correlation"
        )
        final_sample_order <- final_sample$order
        if (!length(final_sample_order)) return(NULL)

        expr_for_sample <- expr_matrix[, final_sample_order, drop = FALSE]
        colnames(expr_for_sample) <- final_sample_order

        sample_cor <- heatmap_build_locked_sample_correlation(
          expr_fixed_mat = expr_for_sample,
          final_sample_order = final_sample_order,
          context = "create_sample_correlation"
        )
        cor_ord <- sample_cor$matrix
        if (is.null(cor_ord) || !nrow(cor_ord)) return(NULL)

        heatmap_shared_sample_cor_col_order(final_sample_order)
        heatmap_sample_cor_matrix(cor_ord)

        fs <- extract_font_settings(input)
        pal <- extract_color_scheme(input)
        col_fun <- col_fun_for_correlation(pal, enhanced_contrast = isTRUE(input$correlation_enhanced_contrast))

        # Legend direction/position from UI
        legend_dir <- legend_direction_from_input(input)

        ComplexHeatmap::Heatmap(
          matrix = cor_ord,
          name = "Sample Correlation",
          col = col_fun,
          show_row_names = TRUE,
          show_column_names = TRUE,
          row_names_gp = safe_gp(fs$row_font_size),
          column_names_gp = safe_gp(fs$col_font_size),
          column_names_rot = 90,
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          row_dend_reorder = FALSE,
          column_dend_reorder = FALSE,
          show_row_dend = FALSE,
          show_column_dend = FALSE,
          row_order = seq_len(nrow(cor_ord)),
          column_order = seq_len(ncol(cor_ord)),
          column_title = NULL,
          cell_fun = create_diagonal_line_cell_fun(input),
          heatmap_legend_param = list(
            title = if (legend_dir == "horizontal") {
              build_horizontal_legend_title(quote("Pairwise Correlation"), quote("Pearson r"))
            } else {
              expression(atop("Pairwise Correlation"[phantom(2)], "Pearson r"))
            },
            legend_direction = legend_dir,
            title_position = if (legend_dir == "horizontal") "topcenter" else "topleft",
            title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
            labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
          )
        )
      }, error = function(e) {
        heatmap_debug_log(paste("Error creating sample correlation:", e$message), 1)
        NULL
      })
    }
