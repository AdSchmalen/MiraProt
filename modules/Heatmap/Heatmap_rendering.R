# ==============================================================================
# Heatmap Module - Rendering Functions and Output Bindings
# ==============================================================================
#
# Purpose:
#   Implements the full rendering layer of the Heatmap module. This includes:
#   - The central compute_current_ordering reactive (shared by plots and titles)
#   - Grid draw functions (draw_grid_expr_corr_ui, draw_grid_expr_sample_ui)
#   - Single-panel draw functions (build/draw helpers, UI-identical variants)
#   - Single-column draw functions (draw_basemean_only_like_ui, etc.)
#   - All output$ renderPlot and renderUI bindings for the Heatmap panel
#   - The download helper draw_current_tab_exactly_like_ui used by Heatmap_download.R
#
# Architecture role:
#   This file is the rendering layer, sourced after Heatmap_create_correlation.R
#   so that all heatmap object builders and correlation helpers are already
#   defined. The output$ assignments execute at source time (they run inside the
#   moduleServer() closure because the file is sourced with local = TRUE).
#   Heatmap_module.R delegates ALL rendering and output wiring to this file.
#
# Structure:
#   1. heatmap_get_applied_sort_state helper
#   2. compute_current_ordering reactive
#   3. Grid draw functions (expression + protein corr, expression + sample corr)
#   4. Single-tab refresh helpers
#   5. Panel bundle builder and drawer
#   6. UI-identical single-panel draw functions (used by download path too)
#   7. draw_basemean_only_like_ui and draw_abundance_ratio_only_like_ui
#   8. output$ renderPlot bindings (one per heatmap tab)
#   9. output$ renderUI bindings (dynamic panel titles)
#  10. draw_current_tab_exactly_like_ui (download path entry point)
#
# Important notes for future developers:
#   - compute_current_ordering is a reactive; it is safe to call inside
#     renderPlot/renderUI/reactive contexts but not at top level.
#   - All output$ assignments run once at source time inside moduleServer().
#     They must not be wrapped in an additional reactive or observer.
#   - The download path (draw_current_tab_exactly_like_ui) uses the same
#     draw functions as the UI path to guarantee pixel-identical output.
#
# Sourced by: Heatmap_module.R (after Heatmap_create_correlation.R)
# ==============================================================================

    # --------------------------------------------------------------------------
    # Ordering contract logging
    # Purpose:
    #   Keep the normal verbose log compact while preserving detailed row/column
    #   previews behind an explicit diagnostic option.
    # --------------------------------------------------------------------------
    log_heatmap_ordering_contract <- function(sort_method,
                                              row_order_names,
                                              col_order_names,
                                              expr_fixed_mat,
                                              sample_sort_mode,
                                              row_dend_obj = NULL,
                                              col_dend_obj = NULL,
                                              want_row_dend = FALSE,
                                              want_col_dend = FALSE) {
      row_labels_match <- identical(rownames(expr_fixed_mat), row_order_names)
      col_labels_match <- identical(colnames(expr_fixed_mat), col_order_names)
      row_dend_fallback <- isTRUE(attr(row_dend_obj, "fallback_used"))
      col_dend_fallback <- isTRUE(attr(col_dend_obj, "fallback_used"))
      dend_fallback <- isTRUE(row_dend_fallback || col_dend_fallback)

      heatmap_debug_log(
        paste0(
          "Ordering contract | sort=", sort_method,
          " | rows=", length(row_order_names), " match=", row_labels_match,
          " | cols=", length(col_order_names), " match=", col_labels_match,
          " | sample_sort=", sample_sort_mode,
          " | dend_fallback=", dend_fallback
        ),
        2
      )

      if (isTRUE(getOption("miraprot.heatmap.ordering_diagnostics", FALSE))) {
        heatmap_debug_log(paste("sort_method:", sort_method), 2)
        heatmap_debug_log(paste("row_order_names (first 10):", paste(head(row_order_names, 10), collapse=", ")), 2)
        if (want_row_dend && !is.null(row_dend_obj)) {
          heatmap_debug_log(paste("row_dend labels (first 10):", paste(head(labels(row_dend_obj), 10), collapse=", ")), 2)
        }
        heatmap_debug_log(paste("row_labels_match:", row_labels_match), 2)
        heatmap_debug_log(paste("col_order_names (first 10):", paste(head(col_order_names, 10), collapse=", ")), 2)
        if (want_col_dend && !is.null(col_dend_obj)) {
          heatmap_debug_log(paste("col_dend labels (first 10):", paste(head(labels(col_dend_obj), 10), collapse=", ")), 2)
        }
        heatmap_debug_log(paste("sample_sort_mode:", sample_sort_mode), 2)
        heatmap_debug_log(paste("desired_col_order (first 10):", paste(head(col_order_names, 10), collapse=", ")), 2)
        heatmap_debug_log(paste("row_dend_fallback_used:", row_dend_fallback,
                                "reason:", attr(row_dend_obj, "fallback_reason") %||% "none"), 2)
        heatmap_debug_log(paste("col_dend_fallback_used:", col_dend_fallback,
                                "reason:", attr(col_dend_obj, "fallback_reason") %||% "none"), 2)
        heatmap_debug_log(paste("col_labels_match:", col_labels_match), 2)
      }
    }

    # --------------------------------------------------------------------------
    # compute_current_ordering
    # Purpose:
    #   Central reactive that resolves the display order (row and column) for all
    #   individual-tab plots and their title outputs. Uses the expression matrix
    #   stored in heatmap_plots() and the active sort method from input.
    # Returns:
    #   list(row_order, col_order, method) where row_order and col_order are
    #   character vectors of protein/sample names, or NULL when no heatmap exists.
    # --------------------------------------------------------------------------
    compute_current_ordering <- reactive({
      plots <- heatmap_plots()
      sort_method <- input$sort_proteins_by %||% "z_score"

      if (is.null(plots$expr)) {
        return(list(row_order = NULL, col_order = NULL, method = sort_method))
      }

      # After a session restore, plots$expr is a miraprot_ch_bundle (a
      # rendered grob, not an S4 Heatmap).  Ordering is driven by the
      # separately-restored expression matrix + shared row/col orders, which
      # are still available as plain reactiveVals -- so fall back to those
      # when the S4 path is not available.
      if (inherits(plots$expr, "miraprot_ch_bundle") ||
          (is.list(plots$expr) && identical(plots$expr$kind, "ch_grob"))) {
        expr_matrix <- heatmap_expression_matrix()
      } else {
        expr_core <- tryCatch(extract_core_heatmap(plots$expr), error = function(e) plots$expr)
        expr_matrix <- tryCatch(expr_core@matrix, error = function(e) NULL)
      }

      if (is.null(expr_matrix) || nrow(expr_matrix) < 2 || ncol(expr_matrix) < 2) {
        return(list(row_order = NULL, col_order = NULL, method = sort_method))
      }

      if (sort_method == "z_score") {
        expr_row_idx <- seq_len(nrow(expr_matrix))
        expr_row_names <- rownames(expr_matrix)
        shared_rows <- heatmap_shared_row_order()
        if (!is.null(shared_rows)) {
          row_idx <- match(shared_rows, rownames(expr_matrix))
          row_idx <- row_idx[!is.na(row_idx)]
          if (length(row_idx) > 0) {
            expr_row_idx <- row_idx
            expr_row_names <- rownames(expr_matrix)[expr_row_idx]
          }
        }

        expr_col_idx <- seq_len(ncol(expr_matrix))
        expr_col_names <- colnames(expr_matrix)
        shared_cols <- heatmap_shared_col_order()
        if (!is.null(shared_cols)) {
          col_idx <- match(shared_cols, colnames(expr_matrix))
          col_idx <- col_idx[!is.na(col_idx)]
          if (length(col_idx) > 0) {
            expr_col_idx <- col_idx
            expr_col_names <- colnames(expr_matrix)[expr_col_idx]
          }
        }

        return(list(row_order = expr_row_names, col_order = expr_col_names, method = sort_method))

      } else if (sort_method == "pearson_r") {
        pearson_result <- compute_pearson_r_leaf_order(expr_matrix)
        prot_row_names <- if (!is.null(pearson_result)) pearson_result$order else rownames(expr_matrix)

        expr_col_names <- colnames(expr_matrix)
        shared_cols <- heatmap_shared_col_order()
        if (!is.null(shared_cols)) {
          col_idx <- match(shared_cols, colnames(expr_matrix))
          col_idx <- col_idx[!is.na(col_idx)]
          if (length(col_idx) > 0) {
            expr_col_names <- colnames(expr_matrix)[col_idx]
          }
        }

        return(list(row_order = prot_row_names, col_order = expr_col_names, method = sort_method))

      } else if (sort_method == "custom") {
        shared_rows <- heatmap_shared_row_order()
        custom_row_names <- if (!is.null(shared_rows) && length(intersect(shared_rows, rownames(expr_matrix))) > 0) {
          intersect(shared_rows, rownames(expr_matrix))
        } else {
          heatmap_apply_custom_protein_order(
            heatmap_custom_fallback_order(expr_matrix, input),
            input$custom_protein_order %||% ""
          )
        }

        expr_col_names <- colnames(expr_matrix)
        shared_cols <- heatmap_shared_col_order()
        if (!is.null(shared_cols)) {
          col_idx <- match(shared_cols, colnames(expr_matrix))
          col_idx <- col_idx[!is.na(col_idx)]
          if (length(col_idx) > 0) {
            expr_col_names <- colnames(expr_matrix)[col_idx]
          }
        }

        return(list(row_order = custom_row_names, col_order = expr_col_names, method = sort_method))
      }

      heatmap_debug_log("Compute ordering: Fallback to original order", 2)
      return(list(row_order = rownames(expr_matrix), col_order = colnames(expr_matrix), method = sort_method))
    })

    heatmap_get_applied_sort_state <- function() {
      state <- heatmap_applied_sort_state()
      if (is.null(state) || !is.list(state)) {
        return(list(
          sort_proteins_by = input$sort_proteins_by %||% "z_score",
          sort_samples_by = input$sort_samples_by %||% "none"
        ))
      }
      list(
        sort_proteins_by = state$sort_proteins_by %||% "z_score",
        sort_samples_by = state$sort_samples_by %||% "none"
      )
    }

    heatmap_build_extension_signature <- function(selected_rows, input) {
      list(
        selected_rows = as.integer(selected_rows %||% integer(0)),
        abundance_type = input$custom_col_sel_heatmap %||% "Normalized Abundance",
        selected_samples = as.character(input$select_samples_heatmap %||% character(0)),
        ratio_column = input$abundance_ratio_col_heatmap %||% "",
        display_options = list(
          skip_log_transform = isTRUE(input$skip_log_transform_heatmap),
          show_basemean_heatmap = isTRUE(input$show_basemean_heatmap),
          show_basemean_row_labels = isTRUE(input$show_basemean_row_labels),
          show_basemean_col_labels = isTRUE(input$show_basemean_col_labels),
          show_abundance_ratio_heatmap = isTRUE(input$show_abundance_ratio_heatmap),
          show_abundance_ratio_row_labels = isTRUE(input$show_abundance_ratio_row_labels),
          show_abundance_ratio_col_labels = isTRUE(input$show_abundance_ratio_col_labels),
          colors = c(
            input$Heatmap_ColorInput_1 %||% "purple",
            input$Heatmap_ColorInput_2 %||% "white",
            input$Heatmap_ColorInput_3 %||% "#FFE100"
          ),
          row_font_size = (input$row_font_size %||% input$font_size_rows) %||% 8,
          col_font_size = (input$col_font_size %||% input$font_size_columns) %||% 10,
          legend_position = input$legend_position %||% "right",
          legend_title_font_size = input$legend_title_font_size %||% 12,
          legend_text_font_size = input$legend_text_font_size %||% 10
        )
      )
    }
    draw_grid_expr_corr_ui <- function() {
      # This is the SAME code path as output$heatmap_grid renderPlot.
      # Kept separate so download can reuse it 1:1.
      plots <- heatmap_plots()
      req(plots$expr)

      applied_sort <- heatmap_get_applied_sort_state()
      sort_method <- applied_sort$sort_proteins_by
      render_id <- paste0("gcec_", format(Sys.time(), "%H%M%OS3"), "_", sort_method)
      # ---------- ORDERING PHASE ----------
      # Single source of truth: define row_order_names and col_order_names early
      expr_core <- tryCatch(extract_core_heatmap(plots$expr), error = function(e) plots$expr)
      expr_matrix <- expr_core@matrix
      if (is.null(expr_matrix) || nrow(expr_matrix) < 2 || ncol(expr_matrix) < 2) {
        plot.new(); text(0.5, 0.5, "Expression matrix is too small or invalid.", col = "red", cex = 1)
        return(invisible(NULL))
      }

      row_order_names <- NULL
      col_order_names <- NULL
      # Preserve the data-driven dendrogram from the ordering phase for later reuse.
      # This avoids rebuilding a dendrogram that can't be rotated to the same leaf order.
      pearson_ordering_dend <- NULL

      if (sort_method == "z_score") {
        heatmap_debug_log("Using Z-Score ordering (expression-based)", 2)
        shared_rows <- heatmap_shared_row_order()
        row_order_names <- if (!is.null(shared_rows) && length(intersect(shared_rows, rownames(expr_matrix))) > 0) {
          intersect(shared_rows, rownames(expr_matrix))
        } else {
          rownames(expr_matrix)
        }
        col_order_names <- colnames(expr_matrix)
      } else if (sort_method == "pearson_r") {
        heatmap_debug_log("Using Pearson r ordering (correlation-based)", 2)
        # Use centralized compute_pearson_r_leaf_order() for consistency across all render paths.
        pearson_result <- compute_pearson_r_leaf_order(expr_matrix)
        prot_row_names <- if (!is.null(pearson_result)) pearson_result$order else rownames(expr_matrix)
        # Save the data-driven dendrogram for the protein-correlation panel below.
        pearson_ordering_dend <- if (!is.null(pearson_result)) pearson_result$dendrogram else NULL
        row_order_names <- prot_row_names
        col_order_names <- colnames(expr_matrix)
      } else if (sort_method == "custom") {
        heatmap_debug_log("Using Custom ordering with Z-Score fallback", 2)
        row_order_names <- heatmap_apply_custom_protein_order(
          heatmap_custom_fallback_order(expr_matrix, input),
          input$custom_protein_order %||% ""
        )
        col_order_names <- colnames(expr_matrix)
      } else {
        heatmap_debug_log("Unknown sort method, falling back to z-score", 1)
        sort_method <- "z_score"
        row_order_names <- rownames(expr_matrix)
        col_order_names <- colnames(expr_matrix)
      }

      # Build expr_fixed_mat using match() for proper ordering
      row_match_idx <- match(row_order_names, rownames(expr_matrix))
      col_match_idx <- match(col_order_names, colnames(expr_matrix))

      # Validate match results
      if (any(is.na(row_match_idx))) {
        heatmap_debug_log("ERROR: Some row_order_names not found in expr_matrix rownames", 1)
        plot.new(); text(0.5, 0.5, "Internal error: row ordering mismatch", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (any(is.na(col_match_idx))) {
        heatmap_debug_log("ERROR: Some col_order_names not found in expr_matrix colnames", 1)
        plot.new(); text(0.5, 0.5, "Internal error: column ordering mismatch", col = "red", cex = 1)
        return(invisible(NULL))
      }

      expr_fixed_mat <- expr_matrix[row_match_idx, col_match_idx, drop = FALSE]
      rownames(expr_fixed_mat) <- row_order_names
      colnames(expr_fixed_mat) <- col_order_names

      fs  <- extract_font_settings(input)
      pal <- extract_color_scheme(input)

      a <- max(abs(range(expr_fixed_mat, finite = TRUE)))
      if (!is.finite(a) || a == 0) a <- 1

      expr_col_fun <- circlize::colorRamp2(c(-a, 0, a), pal)

      want_row_dend <- isTRUE(input$show_row_dendrogram)
      want_col_dend <- isTRUE(input$show_column_dendrogram)
      sample_sort_mode <- applied_sort$sort_samples_by
      if (isTRUE(want_col_dend) && !(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))) {
        notice_key <- paste0("expr_corr_", sample_sort_mode)
        if (!identical(heatmap_col_dend_notice_mode(), notice_key)) {
          showNotification(
            "Column dendrogram uses constrained fixed-order layout for current sample sorting mode.",
            type = "message", duration = 4
          )
          heatmap_col_dend_notice_mode(notice_key)
        }
      }

      # Dendrograms must conform to canonical order (never dictate it)
      row_dend_obj <- NULL
      col_dend_obj <- NULL

      suppressWarnings(try({
        if (identical(sort_method, "pearson_r")) {
          cor_mat <- stats::cor(t(expr_matrix), use = "pairwise.complete.obs", method = "pearson")
          cor_mat[is.na(cor_mat)] <- 0
          d <- stats::as.dist(1 - cor_mat)
          hc <- stats::hclust(d, method = "average")
        } else {
          expr_for_row_clustering <- sanitize_matrix_for_clustering(expr_fixed_mat)
          d <- stats::dist(expr_for_row_clustering, method = "euclidean")
          hc <- stats::hclust(d, method = "ward.D2")
        }
        row_dend_obj <- rotate_dend_to_order(stats::as.dendrogram(hc), row_order_names)
      }, silent = TRUE))
      col_dend_strict <- !(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))
      if (sample_sort_mode %in% c("pearson_cluster", "distance_cluster")) {
        heatmap_debug_log("Cluster sample-sorting active: preserving clustered column dendrogram shape (set-validated); axis order remains hard-locked by fixed column_order.", 2)
      }
      suppressWarnings(try({
        if (identical(sample_sort_mode, "pearson_cluster")) {
          cor_cols <- stats::cor(expr_fixed_mat, use = "pairwise.complete.obs", method = "pearson")
          cor_cols[is.na(cor_cols)] <- 0
          d <- stats::as.dist(1 - cor_cols)
          hc <- stats::hclust(d, method = "average")
          col_dend_obj <- stats::as.dendrogram(hc)
        } else if (identical(sample_sort_mode, "distance_cluster")) {
          expr_for_col_clustering <- sanitize_matrix_for_clustering(expr_fixed_mat)
          d <- stats::dist(t(expr_for_col_clustering), method = "euclidean")
          hc <- stats::hclust(d, method = "ward.D2")
          col_dend_obj <- stats::as.dendrogram(hc)
        } else {
          col_dend_obj <- build_order_locked_dendrogram(expr_fixed_mat, seq_len(ncol(expr_fixed_mat)), axis = "column")
        }
        col_dend_obj <- rotate_dend_to_order(col_dend_obj, col_order_names)
      }, silent = TRUE))

      # Expression dendrogram: strict_order = FALSE because in z_score mode the
      # dendrogram IS the source of row ordering (its leaf order intentionally
      # differs from row_order_names). In pearson_r mode this dendrogram is not
      # used (expr_wants_row_dend = FALSE), so strict_order doesn't matter.
      row_dend_obj <- ensure_dendrogram_order(row_dend_obj, row_order_names, "row", strict_order = FALSE)
      col_dend_obj <- ensure_dendrogram_order(col_dend_obj, col_order_names, "column", strict_order = col_dend_strict)

      # Mandatory per-render logging
      log_heatmap_ordering_contract(
        sort_method = sort_method,
        row_order_names = row_order_names,
        col_order_names = col_order_names,
        expr_fixed_mat = expr_fixed_mat,
        sample_sort_mode = sample_sort_mode,
        row_dend_obj = row_dend_obj,
        col_dend_obj = col_dend_obj,
        want_row_dend = want_row_dend,
        want_col_dend = want_col_dend
      )

      # --- HARD INVARIANT CHECKS (Pflicht) ---
      if (!identical(rownames(expr_fixed_mat), row_order_names)) {
        heatmap_debug_log("FATAL: rownames(expr_fixed_mat) != row_order_names. This should never happen.", 1)
        plot.new(); text(0.5, 0.5, "Fatal internal error: row ordering invariant violated", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (!identical(colnames(expr_fixed_mat), col_order_names)) {
        heatmap_debug_log("FATAL: colnames(expr_fixed_mat) != col_order_names. This should never happen.", 1)
        plot.new(); text(0.5, 0.5, "Fatal internal error: column ordering invariant violated", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (want_row_dend && !is.null(row_dend_obj)) {
        if (!identical(labels(row_dend_obj), row_order_names)) {
          heatmap_debug_log("INVARIANT VIOLATION: row dendrogram labels != row_order_names. Disabling row dendrogram.", 1)
          row_dend_obj <- NULL
          want_row_dend <- FALSE
        }
      }
      if (want_col_dend && !is.null(col_dend_obj)) {
        col_labels <- tryCatch(as.character(labels(col_dend_obj)), error = function(e) character(0))
        col_order_chr <- as.character(col_order_names)
        if (isTRUE(col_dend_strict)) {
          if (!identical(unname(col_labels), unname(col_order_chr))) {
            heatmap_debug_log("INVARIANT VIOLATION: strict col dendrogram labels != col_order_names. Disabling col dendrogram.", 1)
            col_dend_obj <- NULL
            want_col_dend <- FALSE
          }
        } else {
          if (!identical(sort(col_labels), sort(col_order_chr))) {
            heatmap_debug_log("INVARIANT VIOLATION: cluster-mode col dendrogram label set mismatch. Disabling col dendrogram.", 1)
            col_dend_obj <- NULL
            want_col_dend <- FALSE
          }
        }
      }

      legend_dir_expr <- legend_direction_from_input(input)
      legend_line2 <- if (sort_method == "pearson_r") "Pearson r sorted" else if (sort_method == "custom") "Custom sorted" else "Z-Score"
      legend_title <- if (legend_dir_expr == "horizontal") {
        build_horizontal_legend_title(quote("Expression Heatmap"), legend_line2)
      } else {
        as.expression(bquote(atop("Expression Heatmap"[phantom(2)], .(legend_line2))))
      }
      missing_tile_color <- input$missing_value_color_heatmap %||% "#E0E0E0"

      # Row dendrogram belongs on Expression only in z_score mode.
      # In pearson_r mode, the dendrogram belongs on Protein Correlation.
      expr_wants_row_dend <- isTRUE(want_row_dend) && !identical(sort_method, "pearson_r") && !is.null(row_dend_obj)

      expr_ht_fixed <- ComplexHeatmap::Heatmap(
        expr_fixed_mat,
        name = tryCatch(plots$expr@name, error = function(e) "Expression"),
        col = expr_col_fun,
        na_col = missing_tile_color,
        show_row_names = isTRUE(input$show_expr_row_labels),
        show_column_names = isTRUE(input$show_expr_col_labels),
        row_names_side = "left",
        row_names_gp = safe_gp(fs$row_font_size),
        column_names_gp = safe_gp(fs$col_font_size),
        column_names_rot = 90,

        cluster_rows = if (expr_wants_row_dend) row_dend_obj else FALSE,

        cluster_columns = if (!is.null(col_dend_obj)) col_dend_obj else FALSE,

        row_dend_reorder = FALSE,

        column_dend_reorder = FALSE,
        row_order = seq_len(nrow(expr_fixed_mat)),
        column_order = seq_len(ncol(expr_fixed_mat)),
        show_row_dend = expr_wants_row_dend,
        show_column_dend = isTRUE(want_col_dend) && !is.null(col_dend_obj),

        column_title = NULL,
        heatmap_legend_param = list(
          title = legend_title,
          legend_direction = legend_dir_expr,
          title_position = if (legend_dir_expr == "horizontal") "topcenter" else "topleft",
          title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
          labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
        )
      )

      heatmap_fixed_expression(expr_ht_fixed)
      heatmap_shared_row_order(row_order_names)
      heatmap_shared_col_order(col_order_names)

      # ---------- HORIZONTAL SPACER (tile-aligned to expression) ----------
      n_tile_rows <- nrow(expr_fixed_mat)
      n_spacer_tile_cols <- 2  # <-- keep EXACT UI behavior

      spacer_mat_h <- matrix(NA_real_, nrow = n_tile_rows, ncol = n_spacer_tile_cols)

      spacer_h <- ComplexHeatmap::Heatmap(
        spacer_mat_h,
        name = paste0("spacer_h_main_", as.integer(stats::runif(1, 1, 1e9))),
        col = circlize::colorRamp2(c(0, 1), c("white", "white")),
        na_col = "white",
        show_heatmap_legend = FALSE,
        show_row_names = FALSE,
        show_column_names = FALSE,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        border = FALSE
      )

      ht_list <- expr_ht_fixed

      # ---------- PROTEIN CORRELATION (aligned) ----------
      if (!is.null(plots$prot)) {
        tryCatch({
          cor_matrix <- suppressWarnings(tryCatch({
            m <- stats::cor(t(expr_fixed_mat), use = "pairwise.complete.obs", method = "pearson")
            m[is.na(m)] <- 0
            m
          }, error = function(e) {
            heatmap_debug_log(paste("Correlation failed, zeros:", e$message), 1)
            matrix(0, nrow = nrow(expr_fixed_mat), ncol = nrow(expr_fixed_mat))
          }))

          rownames(cor_matrix) <- row_order_names
          colnames(cor_matrix) <- row_order_names
          # Hard-lock symmetry and axis naming to the same canonical order.
          cor_matrix <- cor_matrix[row_order_names, row_order_names, drop = FALSE]
          rownames(cor_matrix) <- row_order_names
          colnames(cor_matrix) <- row_order_names


          prot_pal <- extract_color_scheme(input)
          prot_col_fun <- col_fun_for_correlation(
            prot_pal,
            enhanced_contrast = isTRUE(input$correlation_enhanced_contrast)
          )

          want_row_dend <- isTRUE(input$show_row_dendrogram)
          # Column dendrogram is intentionally disabled for protein-correlation heatmaps.
          # The x-axis order must mirror the canonical row order exactly.
          want_col_dend <- FALSE

          # Reuse the data-driven dendrogram from the ordering phase.
          # Apply reindex_dendrogram_leaves() so that ComplexHeatmap::draw() does not
          # reorder the already-sorted cor_matrix via order.dendrogram() integers.
          row_dend_obj <- if (!is.null(pearson_ordering_dend)) reindex_dendrogram_leaves(pearson_ordering_dend) else NULL
          col_dend_obj <- NULL

          legend_dir_corr <- legend_direction_from_input(input)

          cor_matrix <- heatmap_sync_protein_correlation_axes(
            cor_matrix,
            canonical_order = row_order_names,
            context = "draw_grid_expr_corr_ui"
          )
          if (!heatmap_validate_protein_correlation_axes(cor_matrix, "draw_grid_expr_corr_ui")) {
            heatmap_debug_log("Protein correlation panel skipped: axis validation failed after synchronization.", 1)
            heatmap_fixed_protein_correlation(NULL)
            return(invisible(NULL))
          }

          # Protein-correlation dendrogram display is coupled to Pearson-r sorting only.
          want_prot_row_dend <- isTRUE(want_row_dend) && identical(sort_method, "pearson_r") && !is.null(row_dend_obj)
          if (isTRUE(want_row_dend) && !isTRUE(want_prot_row_dend)) {
            heatmap_debug_log("Protein correlation row dendrogram disabled in grid because sort method is not Pearson r.", 2)
          }

          prot_ht <- ComplexHeatmap::Heatmap(
            cor_matrix,
            name = "Protein Correlation",
            col = prot_col_fun,
            show_row_names = isTRUE(input$show_corr_row_labels),
            show_column_names = isTRUE(input$show_corr_col_labels),
            row_names_side = "right",
            row_names_gp = safe_gp(fs$row_font_size),
            column_names_gp = safe_gp(fs$col_font_size),
            column_names_rot = 90,

            # Dendrogram only for pearson_r mode (verified via ensure_dendrogram_order strict=TRUE).
            # In z_score mode: cluster_rows = FALSE; dendrogram belongs on Expression.
            cluster_rows = if (isTRUE(want_prot_row_dend)) row_dend_obj else FALSE,

            cluster_columns = FALSE,

            row_dend_reorder = FALSE,

            column_dend_reorder = FALSE,
            show_row_dend = isTRUE(want_prot_row_dend),
            show_column_dend = FALSE,

            row_order = seq_len(nrow(cor_matrix)),
            column_order = seq_len(ncol(cor_matrix)),
            column_title = NULL,

            cell_fun = create_diagonal_line_cell_fun(input),

            heatmap_legend_param = list(
              title = if (legend_dir_corr == "horizontal") {
                build_horizontal_legend_title(quote("Pairwise Correlation"), "Pearson r")
              } else {
                expression(atop("Pairwise Correlation"[phantom(2)], "Pearson r"))
              },
              legend_direction = legend_dir_corr,
              title_position = if (legend_dir_corr == "horizontal") "topcenter" else "topleft",
              title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
              labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
            )
          )

          heatmap_fixed_protein_correlation(prot_ht)

          ht_list <- ht_list + spacer_h + prot_ht
        }, error = function(e) {
          heatmap_debug_log(paste("Protein correlation error:", e$message), 1)
        })
      }

      # ---------- BASEMEAN / RATIO OPTIONAL ----------
      loadedData <- NULL; data_def <- NULL; selected_rows <- NULL
      try({ loadedData <- heatmap_data_modified() }, silent = TRUE)
      try({ data_def   <- heatmap_df_data_definition() }, silent = TRUE)
      try({ selected_rows <- heatmap_selected_row_indices() }, silent = TRUE)

      if (!is.null(loadedData) && !is.null(data_def) && !is.null(selected_rows)) {
        extension_signature <- heatmap_build_extension_signature(selected_rows, input)
        extension_signature_unchanged <- identical(extension_signature, isolate(heatmap_extension_signature()))

        # Basemean
        try({
          bm_ht <- isolate(heatmap_fixed_basemean())

          if (!is.null(bm_ht) && methods::is(bm_ht, "Heatmap")) {
            bm_rows <- tryCatch(
              rownames(bm_ht@matrix),
              error = function(e) NULL
            )

            if (!is.null(bm_rows) &&
                !identical(
                  as.character(bm_rows),
                  as.character(row_order_names)
                )) {
              bm_ht <- NULL
            }
          }

          # A fixed single-column heatmap may have been created with an older UI font
          # size. Updating the name graphics parameter is sufficient; the matrix,
          # ordering, colours and legend remain untouched.
          if (!is.null(bm_ht) && methods::is(bm_ht, "Heatmap")) {
            try({
              bm_ht@column_names_param$gp <- safe_gp(
                fs$col_font_size
              )
            }, silent = TRUE)
          }
          cached_basemean_values <- isolate(heatmap_basemean_values())
          if (is.null(bm_ht) && !is.null(cached_basemean_values) && length(cached_basemean_values) > 0L) {
            cached_rows_match <- !is.null(names(cached_basemean_values)) &&
              all(row_order_names %in% names(cached_basemean_values))
            if (isTRUE(cached_rows_match)) {
              cached_basemean_values <- cached_basemean_values[row_order_names]
            } else if (!identical(length(cached_basemean_values), length(row_order_names))) {
              cached_basemean_values <- NULL
            }
          }
          if (is.null(bm_ht) && !is.null(cached_basemean_values) && length(cached_basemean_values) > 0L) {
            bm_ht <- create_basemean_heatmap(cached_basemean_values, input)
            if (!is.null(bm_ht)) {
              heatmap_fixed_basemean(bm_ht)
              heatmap_basemean_values(cached_basemean_values)
            }
          }
          if (is.null(bm_ht) && !extension_signature_unchanged) {
            # Existing basemean heatmaps are plot snapshots; if live metadata no
            # longer matches the plot source data, do not clear the snapshot.
            # Rebuild only when valid values can be calculated, otherwise keep
            # any cached basemean values/heatmap for save/restore fidelity.
            basemean_values <- calculate_correct_basemean_internal(selected_rows, loadedData, data_def, input)
            if (!is.null(basemean_values)) {
              names(basemean_values) <- rownames(heatmap_expression_matrix())
              basemean_values <- basemean_values[row_order_names]
              heatmap_basemean_values(basemean_values)

              bm_ht <- create_basemean_heatmap(basemean_values, input)
              if (!is.null(bm_ht)) {
                heatmap_fixed_basemean(bm_ht)
              }
            } else {
              heatmap_debug_log("Basemean recalculation unavailable; keeping cached basemean snapshot", 1)
            }
          }
          if (!is.null(bm_ht) && isTRUE(input$show_basemean_heatmap)) {
            ht_list <- ht_list + spacer_h + bm_ht
          }
        }, silent = TRUE)

        # Ratio
        try({
          ratio_ht <- isolate(
            heatmap_fixed_abundance_ratio()
          )

          if (!is.null(ratio_ht) &&
              methods::is(ratio_ht, "Heatmap")) {

            ratio_rows <- tryCatch(
              rownames(ratio_ht@matrix),
              error = function(e) NULL
            )

            if (!is.null(ratio_rows) &&
                !identical(
                  as.character(ratio_rows),
                  as.character(row_order_names)
                )) {
              ratio_ht <- NULL
            }
          }

          # Keep the cached log2 abundance-ratio column label synchronized with the
          # current Column font size setting without rebuilding the heatmap.
          if (!is.null(ratio_ht) &&
              methods::is(ratio_ht, "Heatmap")) {
            try({
              ratio_ht@column_names_param$gp <- safe_gp(
                fs$col_font_size
              )
            }, silent = TRUE)
          }
          cached_ratio_values <- isolate(heatmap_abundance_ratio_values())
          if (is.null(ratio_ht) && !is.null(cached_ratio_values) && length(cached_ratio_values) > 0L) {
            cached_rows_match <- !is.null(names(cached_ratio_values)) &&
              all(row_order_names %in% names(cached_ratio_values))
            if (isTRUE(cached_rows_match)) {
              cached_ratio_values <- cached_ratio_values[row_order_names]
            } else if (!identical(length(cached_ratio_values), length(row_order_names))) {
              cached_ratio_values <- NULL
            }
          }
          if (is.null(ratio_ht) && !is.null(cached_ratio_values) && length(cached_ratio_values) > 0L) {
            ratio_ht <- create_abundance_ratio_heatmap(cached_ratio_values, input)
            if (!is.null(ratio_ht)) {
              heatmap_fixed_abundance_ratio(ratio_ht)
              heatmap_abundance_ratio_values(cached_ratio_values)
            }
          }
          if (is.null(ratio_ht) && !extension_signature_unchanged) {
            ratio_values <- calculate_abundance_ratios_internal(selected_rows, loadedData, data_def, input)
            if (!is.null(ratio_values)) {
              names(ratio_values) <- rownames(heatmap_expression_matrix())
              ratio_values <- ratio_values[row_order_names]
              heatmap_abundance_ratio_values(ratio_values)

              ratio_ht <- create_abundance_ratio_heatmap(ratio_values, input)
              if (!is.null(ratio_ht)) {
                heatmap_fixed_abundance_ratio(ratio_ht)
              }
            } else {
              heatmap_debug_log("Abundance ratio recalculation unavailable; keeping cached ratio snapshot", 1)
            }
          }
          if (!is.null(ratio_ht) && isTRUE(input$show_abundance_ratio_heatmap)) {
            ht_list <- ht_list + spacer_h + ratio_ht
          }
        }, silent = TRUE)

        if (!extension_signature_unchanged) {
          heatmap_extension_signature(extension_signature)
        }
      } else {
        # Fallback (session restore): heatmap_selected_row_indices() is not
        # serialized into snapshots, so it is NULL after restore even when
        # the extension columns were visible. Use the stored
        # heatmap_basemean_values() / heatmap_abundance_ratio_values() (which
        # ARE serialized) to reconstruct and append the single-column
        # heatmaps, so the composite grid matches the saved session.
        try({
          basemean_values <- heatmap_basemean_values()
          if (!is.null(basemean_values) && length(basemean_values) > 0) {
            # Align to current row_order_names. Stored values carry gene
            # names; mirror the alignment done in the primary branch above.
            if (!is.null(names(basemean_values)) &&
                all(row_order_names %in% names(basemean_values))) {
              basemean_values <- basemean_values[row_order_names]
            }
            bm_ht <- create_basemean_heatmap(basemean_values, input)
            if (!is.null(bm_ht)) {
              heatmap_fixed_basemean(bm_ht)
              if (isTRUE(input$show_basemean_heatmap)) {
                ht_list <- ht_list + spacer_h + bm_ht
              }
            }
          }
        }, silent = TRUE)

        try({
          ratio_values <- heatmap_abundance_ratio_values()
          if (!is.null(ratio_values) && length(ratio_values) > 0) {
            if (!is.null(names(ratio_values)) &&
                all(row_order_names %in% names(ratio_values))) {
              ratio_values <- ratio_values[row_order_names]
            }
            ratio_ht <- create_abundance_ratio_heatmap(ratio_values, input)
            if (!is.null(ratio_ht)) {
              heatmap_fixed_abundance_ratio(ratio_ht)
              if (isTRUE(input$show_abundance_ratio_heatmap)) {
                ht_list <- ht_list + spacer_h + ratio_ht
              }
            }
          }
        }, silent = TRUE)
      }

      # ---------- OPTIONAL PROTEIN ANNOTATION ----------
      highlighted_proteins <- heatmap_highlighted_proteins()
      if (!is.null(highlighted_proteins)) {
        try({
          protein_annotation <- create_protein_annotation(
            row_order_names,
            highlighted_proteins,
            fs$row_font_size,
            label_padding_mm = legend_plot_gap_mm_from_input(input)
          )
          if (!is.null(protein_annotation)) {
            ht_list <- ht_list + protein_annotation
            heatmap_protein_annotation(protein_annotation)
          }
        }, silent = TRUE)
      }

      # ---------- DRAW WITH ADAPTIVE LEGEND SPACING ----------
      legend_side <- legend_side_from_input(input)
      legend_dir  <- legend_direction_from_input(input)

      old_opt <- ComplexHeatmap::ht_opt()
      on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
      ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))

      if (legend_dir == "horizontal") {
        max_fs <- max(fs$row_font_size, fs$col_font_size,
                      fs$legend_title_font_size, fs$legend_text_font_size, na.rm = TRUE)
        gap_mm <- (max_fs / 1.5) + 8
        gap_mm <- max(6, min(gap_mm, 20))
        ComplexHeatmap::ht_opt(legend_gap = grid::unit(gap_mm, "mm"))
      }

      base_padding <- if (legend_side %in% c("top", "bottom")) c(4, 2, 4, 2) else c(2, 2, 2, 2)
      pad_vec <- heatmap_draw_padding_from_input(input, base_padding_mm = base_padding)

      tryCatch({
        draw_args <- list(
          object = ht_list,
          newpage = FALSE,
          merge_legends = TRUE,
          heatmap_legend_side = legend_side,
          annotation_legend_side = legend_side,
          auto_adjust = FALSE,
          padding = pad_vec
        )
        # main_heatmap = dendrogram carrier, so row sync uses the correct order.
        # pearson_r: Protein Correlation carries the dendrogram.
        # z_score:   Expression carries the dendrogram.
        if (!is.null(plots$prot)) {
          if (identical(sort_method, "pearson_r")) {
            draw_args$main_heatmap <- "Protein Correlation"
          } else {
            draw_args$main_heatmap <- "Expression"
          }
        }
        drawn_obj <- do.call(ComplexHeatmap::draw, draw_args)

      }, error = function(e) {
        heatmap_debug_log(paste("Final draw failed:", e$message), 1)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 1)
      })

      invisible(NULL)
    }
    draw_grid_expr_sample_ui <- function() {
      # This is the SAME code path as output$heatmap_expr_sample_grid renderPlot.
      # Kept separate so download can reuse it 1:1.
      plots <- heatmap_plots()
      req(plots$expr)

      applied_sort <- heatmap_get_applied_sort_state()
      sort_method <- applied_sort$sort_proteins_by

      # ---------- ORDERING PHASE ----------
      # Single source of truth: define row_order_names and col_order_names early
      expr_core <- tryCatch(extract_core_heatmap(plots$expr), error = function(e) plots$expr)
      expr_matrix <- expr_core@matrix
      if (is.null(expr_matrix) || nrow(expr_matrix) < 2 || ncol(expr_matrix) < 2) {
        plot.new(); text(0.5,0.5,"Expression matrix invalid.", col="red"); return(invisible(NULL))
      }

      row_order_names <- NULL
      col_order_names <- NULL
      sample_sort_mode <- applied_sort$sort_samples_by
      sample_cluster_dend_obj_precomputed <- NULL
      sample_cluster_dend_source_precomputed <- "none"
      sample_cluster_dend_fallback_reason_precomputed <- "none"

      if (sort_method == "z_score") {
        shared_rows <- heatmap_shared_row_order()
        row_order_names <- if (!is.null(shared_rows) && length(intersect(shared_rows, rownames(expr_matrix))) > 0) {
          intersect(shared_rows, rownames(expr_matrix))
        } else {
          rownames(expr_matrix)
        }
        col_order_names <- colnames(expr_matrix)
      } else if (sort_method == "pearson_r") {
        # Use centralized compute_pearson_r_leaf_order() for consistency across all render paths.
        pearson_result_sample <- compute_pearson_r_leaf_order(expr_matrix)
        prot_row_names <- if (!is.null(pearson_result_sample)) pearson_result_sample$order else rownames(expr_matrix)
        # Pearson: rows follow the exact same protein-correlation order logic as Expression+Protein grid
        row_order_names <- prot_row_names
        # Keep sample order exactly as provided by expression matrix
        col_order_names <- colnames(expr_matrix)
      } else if (sort_method == "custom") {
        heatmap_debug_log("Using Custom ordering with Z-Score fallback (vertical)", 2)
        row_order_names <- heatmap_apply_custom_protein_order(
          heatmap_custom_fallback_order(expr_matrix, input),
          input$custom_protein_order %||% ""
        )
        col_order_names <- colnames(expr_matrix)
      } else {
        heatmap_debug_log("Unknown sort method -> fallback z_score (vertical)", 1)
        sort_method <- "z_score"
        row_order_names <- rownames(expr_matrix)
        col_order_names <- colnames(expr_matrix)
      }

      # For cluster-based sample sorting modes, make the sample dendrogram-derived
      # order authoritative for the matrix axes in Expression+Sample grid.
      if (sample_sort_mode %in% c("pearson_cluster", "distance_cluster")) {
        expr_for_sample_cluster <- expr_matrix[row_order_names, , drop = FALSE]
        suppressWarnings(try({
          if (identical(sample_sort_mode, "pearson_cluster")) {
            cor_cols <- stats::cor(expr_for_sample_cluster, use = "pairwise.complete.obs", method = "pearson")
            cor_cols[is.na(cor_cols)] <- 0
            d <- stats::as.dist(1 - cor_cols)
            hc <- stats::hclust(d, method = "average")
            sample_cluster_dend_source_precomputed <- "sample_pearson_cluster"
          } else {
            d <- stats::dist(t(expr_for_sample_cluster), method = "euclidean")
            hc <- stats::hclust(d, method = "ward.D2")
            sample_cluster_dend_source_precomputed <- "sample_distance_cluster"
          }
          sample_cluster_dend_obj_precomputed <- stats::as.dendrogram(hc)
          clustered_order <- colnames(expr_for_sample_cluster)[hc$order]
          if (length(clustered_order) == ncol(expr_for_sample_cluster)) {
            col_order_names <- clustered_order
          } else {
            sample_cluster_dend_fallback_reason_precomputed <- "cluster_order_length_mismatch"
            sample_cluster_dend_obj_precomputed <- NULL
          }
        }, silent = TRUE))
        if (is.null(sample_cluster_dend_obj_precomputed) && identical(sample_cluster_dend_fallback_reason_precomputed, "none")) {
          sample_cluster_dend_fallback_reason_precomputed <- "cluster_dendrogram_build_failed"
        }
      }

      # ---------- EXPRESSION ----------
      # Build expr_fixed_mat using match() for proper ordering
      row_match_idx <- match(row_order_names, rownames(expr_matrix))
      col_match_idx <- match(col_order_names, colnames(expr_matrix))

      # Validate match results
      if (any(is.na(row_match_idx))) {
        heatmap_debug_log("ERROR: Some row_order_names not found in expr_matrix rownames", 1)
        plot.new(); text(0.5, 0.5, "Internal error: row ordering mismatch", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (any(is.na(col_match_idx))) {
        heatmap_debug_log("ERROR: Some col_order_names not found in expr_matrix colnames", 1)
        plot.new(); text(0.5, 0.5, "Internal error: column ordering mismatch", col = "red", cex = 1)
        return(invisible(NULL))
      }

      expr_fixed_mat <- expr_matrix[row_match_idx, col_match_idx, drop = FALSE]
      rownames(expr_fixed_mat) <- row_order_names
      colnames(expr_fixed_mat) <- col_order_names
      fs  <- extract_font_settings(input)
      pal <- extract_color_scheme(input)
      a <- max(abs(range(expr_fixed_mat, finite = TRUE))); if (!is.finite(a) || a == 0) a <- 1
      expr_col_fun <- circlize::colorRamp2(c(-a,0,a), pal)
      want_row_dend <- isTRUE(input$show_row_dendrogram)
      want_col_dend <- isTRUE(input$show_column_dendrogram)
      if (isTRUE(want_col_dend) && !(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))) {
        notice_key <- paste0("expr_sample_", sample_sort_mode)
        if (!identical(heatmap_col_dend_notice_mode(), notice_key)) {
          showNotification(
            "Column dendrogram uses constrained fixed-order layout for current sample sorting mode.",
            type = "message", duration = 4
          )
          heatmap_col_dend_notice_mode(notice_key)
        }
      }

      # Dendrograms must conform to canonical order (never dictate it)
      row_dend_obj <- NULL; col_dend_obj <- NULL
      row_dend_source <- "none"
      suppressWarnings(try({
        if (identical(sort_method, "pearson_r")) {
          cor_mat <- stats::cor(t(expr_matrix), use = "pairwise.complete.obs", method = "pearson")
          cor_mat[is.na(cor_mat)] <- 0
          d <- stats::as.dist(1 - cor_mat)
          hc <- stats::hclust(d, method = "average")
          row_dend_source <- "protein_correlation_pearson_hclust"
        } else {
          expr_for_row_clustering <- sanitize_matrix_for_clustering(expr_fixed_mat)
          d <- stats::dist(expr_for_row_clustering, method = "euclidean")
          hc <- stats::hclust(d, method="ward.D2")
          row_dend_source <- "expression_distance_hclust"
        }
        row_dend_obj <- rotate_dend_to_order(stats::as.dendrogram(hc), row_order_names)
      }, silent=TRUE))
      col_dend_strict <- !(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))
      if (sample_sort_mode %in% c("pearson_cluster", "distance_cluster")) {
        heatmap_debug_log("Cluster sample-sorting active: preserving clustered column dendrogram shape (set-validated); axis order remains hard-locked by fixed column_order.", 2)
      }
      suppressWarnings(try({
        if (identical(sample_sort_mode, "pearson_cluster")) {
          cor_cols <- stats::cor(expr_fixed_mat, use = "pairwise.complete.obs", method = "pearson")
          cor_cols[is.na(cor_cols)] <- 0
          d <- stats::as.dist(1 - cor_cols)
          hc <- stats::hclust(d, method = "average")
          col_dend_obj <- stats::as.dendrogram(hc)
        } else if (identical(sample_sort_mode, "distance_cluster")) {
          expr_for_col_clustering <- sanitize_matrix_for_clustering(expr_fixed_mat)
          d <- stats::dist(t(expr_for_col_clustering), method = "euclidean")
          hc <- stats::hclust(d, method = "ward.D2")
          col_dend_obj <- stats::as.dendrogram(hc)
        } else {
          col_dend_obj <- build_order_locked_dendrogram(expr_fixed_mat, seq_len(ncol(expr_fixed_mat)), axis = "column")
        }
        col_dend_obj <- rotate_dend_to_order(col_dend_obj, col_order_names)
      }, silent=TRUE))

      row_dend_obj <- ensure_dendrogram_order(row_dend_obj, row_order_names, "row", strict_order = FALSE)
      col_dend_obj <- ensure_dendrogram_order(col_dend_obj, col_order_names, "column", strict_order = col_dend_strict)

      # Mandatory per-render logging
      log_heatmap_ordering_contract(
        sort_method = sort_method,
        row_order_names = row_order_names,
        col_order_names = col_order_names,
        expr_fixed_mat = expr_fixed_mat,
        sample_sort_mode = sample_sort_mode,
        row_dend_obj = row_dend_obj,
        col_dend_obj = col_dend_obj,
        want_row_dend = want_row_dend,
        want_col_dend = want_col_dend
      )

      # --- HARD INVARIANT CHECKS (Pflicht) ---
      if (!identical(rownames(expr_fixed_mat), row_order_names)) {
        heatmap_debug_log("FATAL: rownames(expr_fixed_mat) != row_order_names. This should never happen.", 1)
        plot.new(); text(0.5, 0.5, "Fatal internal error: row ordering invariant violated", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (!identical(colnames(expr_fixed_mat), col_order_names)) {
        heatmap_debug_log("FATAL: colnames(expr_fixed_mat) != col_order_names. This should never happen.", 1)
        plot.new(); text(0.5, 0.5, "Fatal internal error: column ordering invariant violated", col = "red", cex = 1)
        return(invisible(NULL))
      }
      if (want_row_dend && !is.null(row_dend_obj)) {
        if (!identical(labels(row_dend_obj), row_order_names)) {
          heatmap_debug_log("INVARIANT VIOLATION: row dendrogram labels != row_order_names. Disabling row dendrogram.", 1)
          row_dend_obj <- NULL
          want_row_dend <- FALSE
        }
      }
      if (want_col_dend && !is.null(col_dend_obj)) {
        col_labels <- tryCatch(as.character(labels(col_dend_obj)), error = function(e) character(0))
        col_order_chr <- as.character(col_order_names)
        if (isTRUE(col_dend_strict)) {
          if (!identical(unname(col_labels), unname(col_order_chr))) {
            heatmap_debug_log("INVARIANT VIOLATION: strict col dendrogram labels != col_order_names. Disabling col dendrogram.", 1)
            col_dend_obj <- NULL
            want_col_dend <- FALSE
          }
        } else {
          if (!identical(sort(col_labels), sort(col_order_chr))) {
            heatmap_debug_log("INVARIANT VIOLATION: cluster-mode col dendrogram label set mismatch. Disabling col dendrogram.", 1)
            col_dend_obj <- NULL
            want_col_dend <- FALSE
          }
        }
      }

      # Expression + Sample grid contract:
      # Column dendrogram (if requested) is rendered on the SAMPLE correlation heatmap,
      # not on expression. This avoids cross-panel coupling.
      if (isTRUE(input$show_column_dendrogram)) {
        heatmap_debug_log("sample_grid: expression column dendrogram disabled; rendering column dendrogram on sample-correlation panel.", 2)
      }
      want_col_dend <- FALSE
      col_dend_obj <- NULL

      legend_dir_expr <- legend_direction_from_input(input)
      legend_line2 <- if (sort_method == "pearson_r") "Pearson r sorted" else if (sort_method == "custom") "Custom sorted" else "Z-Score"
      legend_title <- if (legend_dir_expr == "horizontal") {
        build_horizontal_legend_title(quote("Expression Heatmap"), legend_line2)
      } else {
        as.expression(bquote(atop("Expression Heatmap"[phantom(2)], .(legend_line2))))
      }
      missing_tile_color <- input$missing_value_color_heatmap %||% "#E0E0E0"
      expr_ht_fixed <- ComplexHeatmap::Heatmap(
        expr_fixed_mat,
        name = tryCatch(plots$expr@name, error=function(e) "Expression"),
        col = expr_col_fun,
        na_col = missing_tile_color,
        show_row_names = isTRUE(input$show_expr_row_labels),
        show_column_names = isTRUE(input$show_expr_col_labels),
        row_names_side = "left",
        row_names_gp = safe_gp(fs$row_font_size),
        column_names_gp = safe_gp(fs$col_font_size),
        column_names_rot = 90,
        cluster_rows = if (!is.null(row_dend_obj)) row_dend_obj else FALSE,
        cluster_columns = if (!is.null(col_dend_obj)) col_dend_obj else FALSE,
        row_dend_reorder = FALSE,
        column_dend_reorder = FALSE,
        row_order = seq_len(nrow(expr_fixed_mat)),
        column_order = seq_len(ncol(expr_fixed_mat)),
        show_row_dend = isTRUE(want_row_dend) && !is.null(row_dend_obj),
        show_column_dend = isTRUE(want_col_dend) && !is.null(col_dend_obj),
        column_title = NULL,
        heatmap_legend_param = list(
          title = legend_title,
          legend_direction = legend_dir_expr,
          title_position = if (legend_dir_expr == "horizontal") "topcenter" else "topleft",
          title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
          labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
        )
      )
      heatmap_fixed_expression(expr_ht_fixed)
      heatmap_shared_row_order(row_order_names)
      heatmap_shared_col_order(col_order_names)
      ht_list <- expr_ht_fixed

      # ---------- SAMPLE CORRELATION ----------
      if (!is.null(plots$sample_cor)) {
        try({
          if (sample_sort_mode %in% c("pearson_cluster", "distance_cluster")) {
            final_sample_order <- if (!is.null(sample_cluster_dend_obj_precomputed)) {
              as.character(labels(sample_cluster_dend_obj_precomputed))
            } else {
              colnames(expr_fixed_mat)
            }
          } else {
            final_sample <- heatmap_resolve_final_sample_order(
              expr_colnames = colnames(expr_fixed_mat),
              context = "draw_grid_expr_sample_ui"
            )
            final_sample_order <- final_sample$order
          }
          if (!length(final_sample_order)) {
            heatmap_debug_log("sample_grid: no final sample order available", 1)
            return(invisible(NULL))
          }

          expr_for_sample <- expr_fixed_mat[, final_sample_order, drop = FALSE]
          colnames(expr_for_sample) <- final_sample_order

          sample_cor <- heatmap_build_locked_sample_correlation(
            expr_fixed_mat = expr_for_sample,
            final_sample_order = final_sample_order,
            context = "draw_grid_expr_sample_ui"
          )
          sample_cor_matrix <- sample_cor$matrix
          fallback_used <- sample_cor$fallback_used
          fallback_reason <- sample_cor$fallback_reason

          sample_pal <- extract_color_scheme(input)
          sample_col_fun <- col_fun_for_correlation(sample_pal, enhanced_contrast = isTRUE(input$correlation_enhanced_contrast))
          want_row_dend <- FALSE
          want_col_dend_requested <- isTRUE(input$show_column_dendrogram)
          # In Expression + Sample grid, the column dendrogram is rendered on the
          # sample-correlation panel (not on expression). Axis order still remains
          # hard-locked by `column_order` below.
          want_col_dend <- want_col_dend_requested

          # Sample-correlation row/column dendrograms are disabled in hard-lock mode.
          sample_row_dend_obj <- NULL
          sample_col_dend_obj <- NULL

          sample_col_dend_source <- "none"
          sample_col_dend_real_cluster <- FALSE
          sample_col_dend_fallback <- FALSE
          sample_col_dend_fallback_reason <- "none"
          sample_col_dend_axis_lock_decision <- "axis_lock_preserved"
          sample_col_order_names <- final_sample_order
          if (isTRUE(want_col_dend)) {
            suppressWarnings(try({
              if (identical(sample_sort_mode, "pearson_cluster")) {
                sample_col_dend_obj <- sample_cluster_dend_obj_precomputed
                sample_col_dend_source <- "sample_pearson_cluster"
                sample_col_dend_real_cluster <- TRUE
              } else if (identical(sample_sort_mode, "distance_cluster")) {
                sample_col_dend_obj <- sample_cluster_dend_obj_precomputed
                sample_col_dend_source <- "sample_distance_cluster"
                sample_col_dend_real_cluster <- TRUE
              } else {
                sample_col_dend_obj <- build_order_locked_dendrogram(sample_cor_matrix, seq_len(ncol(sample_cor_matrix)), axis = "column")
                sample_col_dend_source <- "sample_order_locked"
              }
              if (!(sample_sort_mode %in% c("pearson_cluster", "distance_cluster"))) {
                sample_col_dend_obj <- rotate_dend_to_order(sample_col_dend_obj, final_sample_order)
              } else if (is.null(sample_col_dend_obj)) {
                if (identical(sample_sort_mode, "pearson_cluster")) {
                  cor_cols <- stats::cor(expr_for_sample, use = "pairwise.complete.obs", method = "pearson")
                  cor_cols[is.na(cor_cols)] <- 0
                  d <- stats::as.dist(1 - cor_cols)
                  hc <- stats::hclust(d, method = "average")
                  sample_col_dend_obj <- stats::as.dendrogram(hc)
                  sample_col_dend_source <- "sample_pearson_cluster_recomputed"
                } else if (identical(sample_sort_mode, "distance_cluster")) {
                  d <- stats::dist(t(expr_for_sample), method = "euclidean")
                  hc <- stats::hclust(d, method = "ward.D2")
                  sample_col_dend_obj <- stats::as.dendrogram(hc)
                  sample_col_dend_source <- "sample_distance_cluster_recomputed"
                }
              }
            }, silent = TRUE))

            if (is.null(sample_col_dend_obj)) {
              sample_col_dend_fallback <- TRUE
              sample_col_dend_fallback_reason <- "dendrogram_build_failed"
              want_col_dend <- FALSE
            } else {
              sample_col_labels <- tryCatch(as.character(labels(sample_col_dend_obj)), error = function(e) character(0))
              if (!identical(sort(sample_col_labels), sort(as.character(final_sample_order)))) {
                sample_col_dend_fallback <- TRUE
                sample_col_dend_fallback_reason <- "label_set_mismatch"
                sample_col_dend_axis_lock_decision <- "axis_lock_overrode_dendrogram_label_set"
                want_col_dend <- FALSE
                sample_col_dend_obj <- NULL
              } else {
                if (sample_sort_mode %in% c("pearson_cluster", "distance_cluster")) {
                  sample_col_order_names <- sample_col_labels
                  final_sample_order <- sample_col_labels
                }
                if (!identical(sample_col_labels, as.character(final_sample_order))) {
                  sample_col_dend_axis_lock_decision <- "axis_lock_overrode_cluster_leaf_order"
                  sample_col_dend_obj <- ensure_dendrogram_order(
                    sample_col_dend_obj,
                    final_sample_order,
                    axis_label = "sample_column",
                    strict_order = TRUE
                  )
                  if (isTRUE(attr(sample_col_dend_obj, "fallback_used"))) {
                    sample_col_dend_fallback <- TRUE
                    sample_col_dend_fallback_reason <- paste0("ensure_dendrogram_order_", attr(sample_col_dend_obj, "fallback_reason") %||% "unknown")
                  }
                }
              }
            }
          }

          # Keep sample-correlation columns locked to the final expression sample order;
          # the dendrogram is annotative and must not change the matrix axis order.
          sample_cor_matrix <- sample_cor_matrix[sample_col_order_names, sample_col_order_names, drop = FALSE]
          rownames(sample_cor_matrix) <- sample_col_order_names
          colnames(sample_cor_matrix) <- sample_col_order_names

          heatmap_debug_log(paste("sample_grid final_sample_order (first 10):",
                          paste(head(final_sample_order, 10), collapse = ", ")), 1)
          heatmap_debug_log(paste("sample_grid expr_fixed_mat colnames (first 10):",
                          paste(head(colnames(expr_fixed_mat), 10), collapse = ", ")), 1)
          heatmap_debug_log(paste("sample_grid sample_cor rownames pre-late-sync (first 10):",
                          paste(head(rownames(sample_cor_matrix), 10), collapse = ", ")), 1)
          heatmap_debug_log(paste("sample_grid sample_cor colnames pre-late-sync (first 10):",
                          paste(head(colnames(sample_cor_matrix), 10), collapse = ", ")), 1)

          heatmap_shared_sample_cor_col_order(sample_col_order_names)
          heatmap_sample_cor_matrix(sample_cor_matrix)
          if (isTRUE(sample_col_dend_fallback)) {
            heatmap_debug_log(paste("sample_grid col_dend fallback used:", sample_col_dend_fallback_reason), 2)
          }
          if (isTRUE(fallback_used)) {
            heatmap_debug_log(paste("sample_grid correlation fallback used:", fallback_reason), 2)
          }

          # LAST-MOMENT HARD SYNC: ensure sample-correlation rows follow the
          # final column order immediately before heatmap construction.
          sample_cor_matrix <- sample_cor_matrix[colnames(sample_cor_matrix), colnames(sample_cor_matrix), drop = FALSE]
          rownames(sample_cor_matrix) <- colnames(sample_cor_matrix)

          legend_dir_sample <- legend_direction_from_input(input)
          sample_ht_fixed <- ComplexHeatmap::Heatmap(
            sample_cor_matrix,
            name = "Sample Correlation",
            col = sample_col_fun,
            show_row_names = TRUE,
            show_column_names = TRUE,
            row_names_gp = safe_gp(fs$row_font_size),
            row_names_side = "left",
            column_names_gp = safe_gp(fs$col_font_size),
            column_names_rot = 90,
            cluster_rows = if (isTRUE(want_col_dend) && !is.null(sample_col_dend_obj)) sample_col_dend_obj else FALSE,
            cluster_columns = if (isTRUE(want_col_dend) && !is.null(sample_col_dend_obj)) sample_col_dend_obj else FALSE,
            row_dend_reorder = FALSE,
            column_dend_reorder = FALSE,
            show_row_dend = FALSE,
            show_column_dend = isTRUE(want_col_dend) && !is.null(sample_col_dend_obj),
            row_order = if (isTRUE(want_col_dend) && !is.null(sample_col_dend_obj)) NULL else seq_len(nrow(sample_cor_matrix)),
            column_order = if (isTRUE(want_col_dend) && !is.null(sample_col_dend_obj)) NULL else seq_len(ncol(sample_cor_matrix)),
            column_title = NULL,
            cell_fun = create_diagonal_line_cell_fun(input),
            heatmap_legend_param = list(
              title = if (legend_dir_sample == "horizontal") {
                build_horizontal_legend_title(quote("Pairwise Correlation"), "Pearson r")
              } else {
                expression(atop("Pairwise Correlation"[phantom(2)], "Pearson r"))
              },
              legend_direction = legend_dir_sample,
              title_position = if (legend_dir_sample == "horizontal") "topcenter" else "topleft",
              title_gp = grid::gpar(fontsize = fs$legend_title_font_size),
              labels_gp = grid::gpar(fontsize = fs$legend_text_font_size)
            )
          )
          heatmap_fixed_sample_correlation(sample_ht_fixed)

          n_tile_cols <- ncol(expr_fixed_mat)
          n_spacer_tile_rows <- 2
          spacer_mat <- matrix(NA_real_, nrow = n_spacer_tile_rows, ncol = n_tile_cols)
          spacer <- ComplexHeatmap::Heatmap(
            spacer_mat,
            name = paste0("spacer_v_main_", as.integer(stats::runif(1, 1, 1e9))),
            col = circlize::colorRamp2(c(0, 1), c("white", "white")),
            na_col = "white",
            show_heatmap_legend = FALSE,
            show_row_names = FALSE,
            show_column_names = FALSE,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            border = FALSE
          )

          ht_list <- ht_list %v% spacer %v% sample_ht_fixed
        }, silent = TRUE)
      }

      # ---------- DRAW WITH ADAPTIVE LEGEND SPACING ----------
      legend_side <- legend_side_from_input(input)
      legend_dir  <- legend_direction_from_input(input)
      old_opt <- ComplexHeatmap::ht_opt()
      on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
      ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))
      if (legend_dir == "horizontal") {
        max_fs <- max(fs$row_font_size, fs$col_font_size,
                      fs$legend_title_font_size, fs$legend_text_font_size, na.rm = TRUE)
        gap_mm <- (max_fs / 1.5) + 8
        gap_mm <- max(8, min(gap_mm, 22))
        ComplexHeatmap::ht_opt(legend_gap = grid::unit(gap_mm, "mm"))
      }
      base_padding <- if (legend_side %in% c("top", "bottom")) c(5, 3, 5, 3) else c(2, 2, 2, 2)
      pad_vec <- heatmap_draw_padding_from_input(input, base_padding_mm = base_padding)
      tryCatch({
        sample_grid_main_heatmap <- if (isTRUE(input$show_column_dendrogram)) "Sample Correlation" else "Expression"
        drawn_ht <- ComplexHeatmap::draw(
          ht_list,
          newpage = FALSE,
          merge_legends = TRUE,
          heatmap_legend_side = legend_side,
          annotation_legend_side = legend_side,
          auto_adjust = FALSE,
          padding = pad_vec,
          main_heatmap = sample_grid_main_heatmap
        )
        sample_grid_draw_col_order <- tryCatch(ComplexHeatmap::column_order(drawn_ht, "Sample Correlation"), error = function(e) NULL)
        sample_grid_draw_row_order <- tryCatch(ComplexHeatmap::row_order(drawn_ht, "Sample Correlation"), error = function(e) NULL)
        if (is.list(sample_grid_draw_col_order)) sample_grid_draw_col_order <- unlist(sample_grid_draw_col_order, use.names = FALSE)
        if (is.list(sample_grid_draw_row_order)) sample_grid_draw_row_order <- unlist(sample_grid_draw_row_order, use.names = FALSE)
      }, error = function(e) {
        plot.new(); text(0.5,0.5,paste("Error:", e$message), col="red")
      })

      invisible(NULL)
    }
    refresh_fixed_heatmaps_for_single_tab <- function(current_tab, context = "single_panel") {
      # [P3-Fix] Sort-State-Guard: skip refresh when UI sort diverges from applied sort
      current_sort <- tryCatch(
        isolate(input$sort_proteins_by) %||% "z_score",
        error = function(e) "z_score"
      )
      applied_state <- heatmap_applied_sort_state()
      applied_sort  <- if (!is.null(applied_state)) applied_state$sort_proteins_by %||% "z_score" else "z_score"

      if (!identical(current_sort, applied_sort)) {
        heatmap_debug_log(paste("[P3-guard] refresh SKIPPED - input sort:", current_sort,
                        "!= applied sort:", applied_sort), 2)
        return(invisible(NULL))
      }

      # Single-tab parity depends on fixed heatmap objects that are produced by grid draw paths.
      # Trigger the matching grid builder on a throwaway device so single tabs refresh immediately,
      # even if the user has not opened the grid tab yet.
      refresh_key <- paste(
        input$sort_proteins_by %||% "z_score",
        input$sort_samples_by %||% "none",
        isTRUE(input$show_row_dendrogram),
        isTRUE(input$show_column_dendrogram),
        (input$col_font_size %||% input$font_size_columns) %||% 10,
        input$create_heatmap_btn %||% 0,
        sep = "|"
      )
      refresh_cache <- heatmap_single_tab_refresh_cache() %||% list()
      if (identical(refresh_cache[[current_tab]], refresh_key)) {
        heatmap_debug_log(paste(context, "refresh skipped for", current_tab, "(cache hit)"), 2)
        return(invisible(NULL))
      }
      tryCatch({
        tmp_pdf <- tempfile(fileext = ".pdf")
        grDevices::pdf(tmp_pdf, width = 4, height = 3)
        on.exit({
          try(grDevices::dev.off(), silent = TRUE)
          try(unlink(tmp_pdf), silent = TRUE)
        }, add = TRUE)

        if (current_tab %in% c("expression_tab", "protein_cor_tab", "basemean_tab", "abundance_ratio_tab")) {
          draw_grid_expr_corr_ui()
        } else if (identical(current_tab, "sample_cor_tab")) {
          draw_grid_expr_sample_ui()
        }

        refresh_cache[[current_tab]] <- refresh_key
        heatmap_single_tab_refresh_cache(refresh_cache)
      }, error = function(e) {
        heatmap_debug_log(paste(context, "failed to refresh fixed objects for", current_tab, ":", e$message), 1)
      })
      invisible(NULL)
    }
    build_single_panel_parity_bundle <- function(current_tab, context = "single_panel") {
      refresh_fixed_heatmaps_for_single_tab(current_tab = current_tab, context = context)
      bundle <- build_heatmap_htlist_for_tab(current_tab)
      if (is.null(bundle) || is.null(bundle$ht)) {
        heatmap_debug_log(paste(context, "parity bundle missing for", current_tab, "- create heatmaps first."), 1)
        return(NULL)
      }
      bundle
    }
    log_single_panel_parity <- function(ht, panel_tag, context, row_label_toggle = NA, col_label_toggle = NA) {
      ht_core <- tryCatch(extract_core_heatmap(ht), error = function(e) ht)
      mat <- tryCatch(ht_core@matrix, error = function(e) NULL)
      if (is.null(mat) || !is.matrix(mat)) {
        heatmap_debug_log(paste(context, panel_tag, "matrix unavailable for parity logging"), 1)
        return(invisible(NULL))
      }
      if (panel_tag %in% c("protein_correlation", "sample_correlation")) {
        rn <- rownames(mat) %||% character(0)
        cn <- colnames(mat) %||% character(0)
        if (!identical(rn, cn)) {
          heatmap_debug_log(paste(context, panel_tag, "WARN: rownames != colnames"), 1)
        }
      }
      invisible(NULL)
    }
    draw_single_panel_from_bundle <- function(bundle, panel_tag, context, row_label_toggle = NA, col_label_toggle = NA) {
      if (is.null(bundle) || is.null(bundle$ht)) {
        plot.new(); text(0.5, 0.5, "No heatmap available\nCreate heatmaps first", col = "gray", cex = 1.1)
        return(invisible(NULL))
      }
      log_single_panel_parity(
        ht = bundle$ht,
        panel_tag = panel_tag,
        context = context,
        row_label_toggle = row_label_toggle,
        col_label_toggle = col_label_toggle
      )
      old_opt <- ComplexHeatmap::ht_opt()
      on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
      ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))
      ComplexHeatmap::draw(
        bundle$ht,
        newpage = FALSE,
        heatmap_legend_side = bundle$legend_side,
        annotation_legend_side = bundle$legend_side,
        padding = heatmap_draw_padding_from_input(input)
      )
      invisible(NULL)
    }
    draw_expression_only_like_ui <- function() {
      bundle <- build_single_panel_parity_bundle("expression_tab", context = "expression_only_download")
      draw_single_panel_from_bundle(
        bundle = bundle,
        panel_tag = "expression",
        context = "expression_only_download",
        row_label_toggle = isTRUE(input$show_expr_row_labels),
        col_label_toggle = isTRUE(input$show_expr_col_labels)
      )
    }
    draw_protein_correlation_only_like_ui <- function() {
      bundle <- build_single_panel_parity_bundle("protein_cor_tab", context = "protein_cor_only_download")
      draw_single_panel_from_bundle(
        bundle = bundle,
        panel_tag = "protein_correlation",
        context = "protein_cor_only_download",
        row_label_toggle = isTRUE(input$show_corr_row_labels),
        col_label_toggle = isTRUE(input$show_corr_col_labels)
      )
    }
    draw_sample_correlation_only_like_ui <- function() {
      bundle <- build_single_panel_parity_bundle("sample_cor_tab", context = "sample_cor_only_download")
      draw_single_panel_from_bundle(
        bundle = bundle,
        panel_tag = "sample_correlation",
        context = "sample_cor_only_download",
        row_label_toggle = TRUE,
        col_label_toggle = TRUE
      )
    }
    draw_current_tab_exactly_like_ui <- function(current_tab) {
      # This function is the single source of truth for download rendering.
      # It intentionally calls the SAME draw logic as the UI tabs.
      # Priority: do NOT change UI appearance.

      if (is.null(current_tab) || !nzchar(current_tab)) current_tab <- "grid_expr_corr"

      # After a session restore, heatmap_plots slots are miraprot_ch_bundle
      # grobs rather than ComplexHeatmap S4 objects.  Draw the rehydrated
      # grob directly so downloads continue to work without requiring the
      # user to re-run "Create Heatmap".
      tab_to_slots <- list(
        grid_expr_corr           = c("expr", "prot"),
        grid_expr_sample         = c("expr"),
        expression_tab           = c("expr"),
        protein_cor_tab          = c("prot"),
        sample_cor_tab           = c("expr"),
        basemean_tab             = c("basemean"),
        abundance_ratio_tab      = c("ratio")
      )
      target_slots <- tab_to_slots[[current_tab]]
      if (!is.null(target_slots) && .try_draw_restored_heatmap_bundle(target_slots)) {
        return(invisible(NULL))
      }

      if (identical(current_tab, "grid_expr_corr")) {
        # EXACTLY what the UI shows in "Expression + Protein Correlation Grid"
        # Use the same builder used by download + ensure it contains the SAME components as UI.
        bundle <- build_heatmap_htlist_for_tab("grid_expr_corr")
        if (is.null(bundle) || is.null(bundle$ht)) stop("No plot available: grid_expr_corr. Create the heatmap first.")

        # Draw with the same settings as the UI grid draw
        old_opt <- ComplexHeatmap::ht_opt()
        on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
        ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))

        fs <- bundle$fs
        legend_side <- bundle$legend_side
        legend_dir  <- bundle$legend_dir

        if (legend_dir == "horizontal") {
          max_fs <- max(fs$row_font_size, fs$col_font_size, fs$legend_title_font_size, fs$legend_text_font_size, na.rm = TRUE)
          gap_mm <- (max_fs / 1.5) + 8
          gap_mm <- max(6, min(gap_mm, 20))
          ComplexHeatmap::ht_opt(legend_gap = grid::unit(gap_mm, "mm"))
        }

        base_padding <- if (legend_side %in% c("top", "bottom")) c(4, 2, 4, 2) else c(2, 2, 2, 2)
        pad_vec <- heatmap_draw_padding_from_input(input, base_padding_mm = base_padding)

        draw_args <- list(
          object = bundle$ht,
          newpage = FALSE,
          merge_legends = TRUE,
          heatmap_legend_side = legend_side,
          annotation_legend_side = legend_side,
          auto_adjust = FALSE,
          padding = pad_vec
        )
        if (!is.null(bundle$main_heatmap) && nzchar(bundle$main_heatmap)) {
          draw_args$main_heatmap <- bundle$main_heatmap
        }
        do.call(ComplexHeatmap::draw, draw_args)
        return(invisible(NULL))
      }

      if (identical(current_tab, "grid_expr_sample")) {
        bundle <- build_heatmap_htlist_for_tab("grid_expr_sample")
        if (is.null(bundle) || is.null(bundle$ht)) stop("No plot available: grid_expr_sample. Create the heatmap first.")

        old_opt <- ComplexHeatmap::ht_opt()
        on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
        ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))

        fs <- bundle$fs
        legend_side <- bundle$legend_side
        legend_dir  <- bundle$legend_dir

        if (legend_dir == "horizontal") {
          max_fs <- max(fs$row_font_size, fs$col_font_size, fs$legend_title_font_size, fs$legend_text_font_size, na.rm = TRUE)
          gap_mm <- (max_fs / 1.5) + 8
          gap_mm <- max(8, min(gap_mm, 22))
          ComplexHeatmap::ht_opt(legend_gap = grid::unit(gap_mm, "mm"))
        }

        base_padding <- if (legend_side %in% c("top", "bottom")) c(5, 3, 5, 3) else c(2, 2, 2, 2)
        pad_vec <- heatmap_draw_padding_from_input(input, base_padding_mm = base_padding)

        sample_grid_main_heatmap <- if (isTRUE(input$show_column_dendrogram)) "Sample Correlation" else "Expression"
        ComplexHeatmap::draw(
          bundle$ht,
          newpage = FALSE,
          merge_legends = TRUE,
          heatmap_legend_side = legend_side,
          annotation_legend_side = legend_side,
          auto_adjust = FALSE,
          padding = pad_vec,
          main_heatmap = sample_grid_main_heatmap
        )
        return(invisible(NULL))
      }

      # Single tabs: must download exactly what is shown there (fixed objects)
      if (identical(current_tab, "expression_tab")) {
        draw_expression_only_like_ui()
        return(invisible(NULL))
      }

      if (identical(current_tab, "protein_cor_tab")) {
        draw_protein_correlation_only_like_ui()
        return(invisible(NULL))
      }

      # For the remaining tabs you can keep the existing fixed-object approach,
      # OR (recommended) do the same UI-identical approach for sample/basemean/ratio too.

      if (identical(current_tab, "sample_cor_tab")) {
        draw_sample_correlation_only_like_ui()
        return(invisible(NULL))
      }

      if (current_tab %in% c("basemean_tab", "abundance_ratio_tab")) {
        refresh_fixed_heatmaps_for_single_tab(current_tab = current_tab, context = "single_column_download")
        bundle <- build_heatmap_htlist_for_tab(current_tab)
        if (is.null(bundle) || is.null(bundle$ht)) stop(paste("No plot available for tab:", current_tab))
        ComplexHeatmap::draw(
          bundle$ht,
          newpage = FALSE,
          heatmap_legend_side = bundle$legend_side,
          annotation_legend_side = bundle$legend_side,
          padding = heatmap_draw_padding_from_input(input)
        )
        return(invisible(NULL))
      }

      stop(paste("Unknown tab:", current_tab))
    }

    # --------------------------------------------------------------------------
    # draw_basemean_only_like_ui
    # Purpose:
    #   Render the basemean single-column heatmap in a way that is identical to
    #   the UI display. Used by output$test_basemean_only and by the download path.
    # --------------------------------------------------------------------------
    draw_basemean_only_like_ui <- function() {
      refresh_fixed_heatmaps_for_single_tab(current_tab = "basemean_tab", context = "basemean_only_ui")
      plots <- heatmap_plots()
      if (is.null(plots) || is.null(plots$expr)) {
        plot.new()
        text(0.5, 0.5, "No basemean heatmap\n(Create in grid tab first)", col = "gray")
        return(invisible(NULL))
      }
      expr_row_names <- heatmap_shared_row_order()
      expr_col_names <- heatmap_shared_col_order()
      if (is.null(expr_row_names) || is.null(expr_col_names)) {
        plot.new()
        text(0.5, 0.5, "No ordering available\n(Create grid heatmaps first)", col = "gray")
        return(invisible(NULL))
      }
      fs <- extract_font_settings(input)
      legend_side <- legend_side_from_input(input)
      fixed_basemean <- heatmap_fixed_basemean()

      # The Basemean plot reuses a fixed Heatmap object. Synchronize only its
      # column-label font with the live UI setting before drawing.
      if (!is.null(fixed_basemean) &&
          methods::is(fixed_basemean, "Heatmap")) {
        try({
          fixed_basemean@column_names_param$gp <- safe_gp(
            fs$col_font_size
          )
        }, silent = TRUE)
      }

      if (!is.null(fixed_basemean)) {
        tryCatch({
          # Attach legend font sizes in case the object was created with an older version.
          fixed_basemean@heatmap_param$heatmap_legend_param$title_gp  <- grid::gpar(fontsize = fs$legend_title_font_size)
          fixed_basemean@heatmap_param$heatmap_legend_param$labels_gp <- grid::gpar(fontsize = fs$legend_text_font_size)
        }, silent = TRUE)
        tryCatch({
          ComplexHeatmap::draw(
            fixed_basemean,
            newpage = FALSE,
            heatmap_legend_side = legend_side,
            annotation_legend_side = legend_side,
            padding = heatmap_draw_padding_from_input(input)
          )
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      } else {
        plot.new()
        text(0.5, 0.6, "No Basemean Heatmap Available", col = "#666666", cex = 1.3, font = 2)
        text(0.5, 0.45, "Create grid heatmaps first", col = "#888888")
      }
    }

    # --------------------------------------------------------------------------
    # draw_abundance_ratio_only_like_ui
    # Purpose:
    #   Render the abundance ratio single-column heatmap in a way that is
    #   identical to the UI display. Used by output$test_abundance_ratio_only
    #   and by the download path.
    # --------------------------------------------------------------------------
    draw_abundance_ratio_only_like_ui <- function() {
      refresh_fixed_heatmaps_for_single_tab(current_tab = "abundance_ratio_tab", context = "abundance_ratio_only_ui")
      ordering <- compute_current_ordering()

      if (is.null(ordering$row_order)) {
        plot.new()
        text(0.5, 0.5, "No abundance ratio heatmap available\nCreate heatmaps first", col = "gray", cex = 1.2)
        return(invisible(NULL))
      }

      heatmap_debug_log(paste("Individual Abundance Ratio: Using", ordering$method, "ordering - proteins:", length(ordering$row_order)), 2)

      ratio_values <- heatmap_abundance_ratio_values()

      if (!is.null(ratio_values) && length(ratio_values) > 0) {
        tryCatch({
          value_names <- names(ratio_values)
          if (is.null(value_names)) {
            plot.new()
            text(0.5, 0.5, "Error: Abundance ratio values have no names", col = "red")
            return(invisible(NULL))
          }

          value_indices <- match(ordering$row_order, value_names)
          valid_indices <- !is.na(value_indices)

          if (sum(valid_indices) > 0) {
            final_order   <- ordering$row_order[valid_indices]
            final_indices <- value_indices[valid_indices]
            reordered_values <- ratio_values[final_indices]
            names(reordered_values) <- final_order

            v <- as.numeric(reordered_values)
            v[!is.finite(v)] <- 0

            pal     <- extract_color_scheme(input)
            col_fun <- col_fun_for_single_column_values(v, pal)
            fs      <- extract_font_settings(input)

            ratio_column_label <- expression(log[2]("Abundance Ratio"))

            m <- matrix(v, ncol = 1)
            rownames(m) <- names(reordered_values)
            colnames(m) <- "log2(Abundance Ratio)"

            legend_dir  <- legend_direction_from_input(input)
            legend_side <- legend_side_from_input(input)

            legend_title_expr <- if (legend_dir == "horizontal") {
              # Routed through the shared helper so every horizontal heatmap
              # title shares identical line bounding boxes and therefore
              # identical atop() spacing. Colorbars align across legends.
              build_horizontal_legend_title(quote("log"[2]), quote("(Abundance Ratio)"))
            } else {
              expression(log[2]("Abundance Ratio"))
            }

            dynamic_ratio_ht <- ComplexHeatmap::Heatmap(
              m,
              name = "log2_ratio",
              col = col_fun,
              width = grid::unit(8, "mm"),
              show_row_names    = isTRUE(input$show_abundance_ratio_row_labels),
              show_column_names = isTRUE(input$show_abundance_ratio_col_labels),
              cluster_rows      = FALSE,
              cluster_columns   = FALSE,
              show_row_dend     = FALSE,
              show_column_dend  = FALSE,
              row_order         = seq_len(nrow(m)),
              column_order      = seq_len(ncol(m)),
              column_labels     = ratio_column_label,
              column_names_gp   = safe_gp(fs$col_font_size),
              row_names_gp      = safe_gp(fs$row_font_size),
              heatmap_legend_param = list(
                title            = legend_title_expr,
                legend_direction = legend_dir,
                title_position   = if (legend_dir == "horizontal") "topcenter" else "topleft",
                title_gp         = grid::gpar(fontsize = fs$legend_title_font_size),
                labels_gp        = grid::gpar(fontsize = fs$legend_text_font_size)
              )
            )

            ComplexHeatmap::draw(
              dynamic_ratio_ht,
              newpage = FALSE,
              heatmap_legend_side = legend_side,
              annotation_legend_side = legend_side,
              padding = heatmap_draw_padding_from_input(input)
            )
          } else {
            plot.new()
            text(0.5, 0.5, "Error: Cannot match ordering to abundance ratio values", col = "red")
            heatmap_debug_log("Individual Abundance Ratio: Cannot match ordering to values", 1)
          }
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Rendering Error:", e$message), col = "red", cex = 1.2)
          heatmap_debug_log(paste("Individual Abundance Ratio Error:", e$message), 1)
        })
      } else {
        plot.new()
        par(mar = c(1, 1, 1, 1))
        text(0.5, 0.7, "Abundance Ratio Heatmap Not Created",         col = "#6c757d", cex = 1.6, font = 2)
        text(0.5, 0.5, "Abundance ratio heatmaps are created automatically", col = "#495057", cex = 1.2)
        text(0.5, 0.4, "when grid heatmaps are generated",            col = "#495057", cex = 1.2)
        text(0.5, 0.3, paste("Will use", ordering$method, "ordering"), col = "#6c757d", cex = 0.9, font = 3)
      }
    }

    # ==========================================================================
    # output$ renderPlot Bindings
    # ==========================================================================

    # Helper: if the current slot in heatmap_plots() is a restored grob bundle
    # (miraprot_ch_bundle, produced by session save/restore), draw the
    # rehydrated grid grob directly and return TRUE so the caller can skip
    # the build-from-S4 path that does not apply to a pure grob.
    .try_draw_restored_heatmap_bundle <- function(slot_names) {
      plots <- heatmap_plots()
      if (!is.list(plots)) return(FALSE)
      for (slot in slot_names) {
        item <- plots[[slot]]
        if (inherits(item, "miraprot_ch_bundle") ||
            (is.list(item) && identical(item$kind, "ch_grob"))) {
          grob <- .restore_ch_from_bundle(item)
          if (!is.null(grob)) {
            grid::grid.newpage()
            grid::grid.draw(grob)
            return(TRUE)
          }
        }
      }
      FALSE
    }

    output$heatmap_grid <- renderPlot({
      if (.try_draw_restored_heatmap_bundle(c("expr", "prot"))) return(invisible())
      draw_grid_expr_corr_ui()
    }, height = 600, width = 800)

    output$heatmap_expr_sample_grid <- renderPlot({
      if (.try_draw_restored_heatmap_bundle(c("expr"))) return(invisible())
      draw_grid_expr_sample_ui()
    }, height = 600, width = 800)

    output$test_expression_only <- renderPlot({
      if (.try_draw_restored_heatmap_bundle("expr")) return(invisible())
      bundle <- build_single_panel_parity_bundle("expression_tab", context = "expression_only_ui")
      draw_single_panel_from_bundle(
        bundle          = bundle,
        panel_tag       = "expression",
        context         = "expression_only_ui",
        row_label_toggle = isTRUE(input$show_expr_row_labels),
        col_label_toggle = isTRUE(input$show_expr_col_labels)
      )
    }, height = 600, width = 900)

    output$test_protein_cor_only <- renderPlot({
      if (.try_draw_restored_heatmap_bundle("prot")) return(invisible())
      bundle <- build_single_panel_parity_bundle("protein_cor_tab", context = "protein_cor_only_ui")
      draw_single_panel_from_bundle(
        bundle          = bundle,
        panel_tag       = "protein_correlation",
        context         = "protein_cor_only_ui",
        row_label_toggle = isTRUE(input$show_corr_row_labels),
        col_label_toggle = isTRUE(input$show_corr_col_labels)
      )
    }, height = 600, width = 900)

    output$test_sample_cor_only <- renderPlot({
      if (.try_draw_restored_heatmap_bundle("expr")) return(invisible())
      bundle <- build_single_panel_parity_bundle("sample_cor_tab", context = "sample_cor_only_ui")
      draw_single_panel_from_bundle(
        bundle          = bundle,
        panel_tag       = "sample_correlation",
        context         = "sample_cor_only_ui",
        row_label_toggle = TRUE,
        col_label_toggle = TRUE
      )
    }, height = 600, width = 900)

    output$test_basemean_only <- renderPlot({
      if (.try_draw_restored_heatmap_bundle("basemean")) return(invisible())
      draw_basemean_only_like_ui()
    }, height = 600, width = 800)

    output$test_abundance_ratio_only <- renderPlot({
      if (.try_draw_restored_heatmap_bundle("ratio")) return(invisible())
      draw_abundance_ratio_only_like_ui()
    }, height = 600, width = 600)

    # ==========================================================================
    # output$ renderUI Bindings (dynamic panel titles)
    # ==========================================================================

    output$grid_expr_corr_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      plots <- heatmap_plots()
      if (is.null(plots$expr)) return(NULL)
      sort_method <- input$sort_proteins_by %||% "z_score"
      sort_text   <- if (sort_method == "pearson_r") " (Pearson r sorted)" else if (sort_method == "custom") " (Custom sorted)" else " (Z-Score sorted)"
      h4(paste0("Expression and Pairwise Protein Correlation Heatmap", sort_text))
    })

    output$grid_expr_sample_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      plots <- heatmap_plots()
      if (is.null(plots$expr)) return(NULL)
      sort_method <- input$sort_proteins_by %||% "z_score"
      sort_text   <- if (sort_method == "pearson_r") " (Pearson r sorted)" else if (sort_method == "custom") " (Custom sorted)" else " (Z-Score sorted)"
      h4(paste0("Expression and Pairwise Sample Correlation Heatmap", sort_text))
    })

    output$expression_only_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      ordering <- compute_current_ordering()
      if (is.null(ordering$row_order) || is.null(ordering$col_order)) return(NULL)
      h4("Expression Heatmap")
    })

    output$protein_cor_only_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      ordering <- compute_current_ordering()
      if (is.null(ordering$row_order)) return(NULL)
      h4("Protein Correlation Heatmap")
    })

    output$sample_cor_only_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      ordering <- compute_current_ordering()
      if (is.null(ordering$col_order)) return(NULL)
      h4("Sample Correlation Heatmap")
    })

    output$basemean_only_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      ordering       <- compute_current_ordering()
      basemean_values <- heatmap_basemean_values()
      if (is.null(ordering$row_order) || is.null(basemean_values) || length(basemean_values) == 0) {
        return(NULL)
      }
      if (isTRUE(input$skip_log_transform_heatmap)) {
        h4("Individual Basemean Single Column Heatmap")
      } else {
        h4(tagList("Individual log", tags$sub("2"), "(Basemean) Single Column Heatmap"))
      }
    })

    output$abundance_ratio_only_title <- renderUI({
      if (isTRUE(input$hideTitle_Heatmap)) return(NULL)
      ordering     <- compute_current_ordering()
      ratio_values <- heatmap_abundance_ratio_values()
      if (is.null(ordering$row_order) || is.null(ratio_values) || length(ratio_values) == 0) {
        return(NULL)
      }
      h4(tagList("Individual log", tags$sub("2"), "(Abundance Ratio) Single Column Heatmap"))
    })
