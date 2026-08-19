# ==============================================================================
# Heatmap Module - Expression Heatmap Creation
# ==============================================================================
#
# Purpose:
#   Contains the heatmap_create_expression_heatmap() function which builds
#   the primary expression heatmap from statistical analysis results.
#
# Responsibilities:
#   1. Extract abundance data for selected proteins and samples
#   2. Apply sample sorting (7 modes: none, alpha_asc, alpha_desc,
#      pearson_cluster, distance_cluster, pca1_asc/desc, pca2_asc/desc)
#   3. Optionally apply log2 transformation and row-wise z-score normalization
#   4. Compute expression clustering and shared ordering
#   5. Create ComplexHeatmap object with optional protein annotations
#
# Scope:
#   This file is sourced with local = TRUE inside moduleServer().
#   It has closure access to: rv, input, heatmap_debug_log, and all reactiveVal()
#   definitions from Heatmap_reactive_state.R.
#
# Called by:
#   - Create Heatmap observeEvent (in Heatmap_module.R)
#
# Side Effects:
#   - Sets heatmap_shared_col_order() with final sample order
#   - Sets heatmap_expression_matrix() with z-scored matrix
#   - Sets heatmap_cluster_info() with clustering results
#   - Sets heatmap_shared_row_order() with clustered protein order
#   - Triggers showNotification() on validation errors
#
# Sourced by: Heatmap_module.R
# ==============================================================================

# ------------------------------------------------------------------------------
# Function: heatmap_create_expression_heatmap
# Purpose:  Build expression heatmap from statistical analysis results
# Description:
#   Takes results from the statistical analysis step (containing Row Index),
#   extracts abundance data for selected proteins and samples, applies
#   sample sorting, optional log2 transformation, z-score normalization, clustering,
#   and creates a ComplexHeatmap object.
# Inputs:
#   results - data.frame with at least a "Row Index" column from stat analysis
# Outputs:
#   ComplexHeatmap Heatmap object (or HeatmapList if protein annotation is added),
#   or NULL on failure
# Side Effects:
#   Sets reactive state: heatmap_shared_col_order, heatmap_expression_matrix,
#   heatmap_cluster_info, heatmap_shared_row_order
#   May trigger showNotification on error
# Used in:
#   Create Heatmap button observeEvent
# ------------------------------------------------------------------------------
heatmap_create_expression_heatmap <- function(
    results = results,
    heatmap_debug_log = heatmap_debug_log,
    data_pair = NULL,
    input_values = NULL,
    plot_request = NULL,
    silent_restore = FALSE
  ) {
  tryCatch({
    heatmap_debug_log("Creating expression heatmap with selected samples only", 2)
    effective_input <- plot_request %||% input_values %||% input

    if (is.null(results) || !"Row Index" %in% names(results)) {
      heatmap_debug_log("No valid results provided for expression heatmap", 1)
      return(NULL)
    }

    dm <- if (is.list(data_pair)) data_pair$data_mod else tryCatch(rv$data_mod, error = function(e) NULL)
    dd <- if (is.list(data_pair)) data_pair$data_def else tryCatch(rv$data_def, error = function(e) NULL)
    notify_or_log_expression_failure <- function(message_text, reason = message_text) {
      if (isTRUE(silent_restore)) {
        heatmap_debug_log(paste("[Heatmap] session restore expression heatmap failure:", reason), 1)
      } else {
        showNotification(message_text, type = "error", duration = 5)
      }
    }

    if (!inherits(dm, "data.frame") || !inherits(dd, "data.frame")) {
      notify_or_log_expression_failure("Data not loaded", "data_mod/data_def are not available data frames")
      heatmap_debug_log("Expression heatmap creation stopped: data_mod/data_def are not available data frames", 1)
      return(NULL)
    }

    # ------------------------------------------------------------------
    # Step 1: Resolve abundance columns for selected samples
    # ------------------------------------------------------------------
    data_type <- effective_input$custom_col_sel_heatmap %||% "Normalized Abundance"
    selected_samples <- effective_input$select_samples_heatmap %||% character(0)

    if (length(selected_samples) == 0) {
      notify_or_log_expression_failure("No samples selected for heatmap")
      return(NULL)
    }

    abundance_cols <- integer(0)
    for (s in selected_samples) {
      idx_s <- which(dd$Content == data_type & !is.na(dd$Sample) & dd$Sample == s)
      if (length(idx_s) > 0) {
        abundance_cols <- c(abundance_cols, idx_s)
      }
    }
    abundance_cols <- unique(abundance_cols)

    if (length(abundance_cols) == 0) {
      notify_or_log_expression_failure("No abundance columns found for selected samples")
      return(NULL)
    }

    # ------------------------------------------------------------------
    # Step 2: Build raw expression matrix
    # ------------------------------------------------------------------
    selected_row_indices <- results$`Row Index`
    heatmap_matrix <- as.matrix(dm[selected_row_indices, abundance_cols, drop = FALSE])

    # Set row names (protein identifiers)
    identifier_col <- effective_input$identifier_column %||% effective_input$GeneIdentifierColumn_Heatmap
    resolved_identifier_col <- if (exists("resolve_heatmap_identifier_column", mode = "function")) {
      resolve_heatmap_identifier_column(identifier_col, dd, dm)
    } else {
      identifier_col
    }
    if (!is.null(resolved_identifier_col) && nzchar(resolved_identifier_col) && resolved_identifier_col %in% colnames(dm)) {
      valid_identifiers <- trimws(as.character(dm[[resolved_identifier_col]][selected_row_indices]))
      fallback_ids <- paste0("Protein_", selected_row_indices)
      valid_identifiers[is.na(valid_identifiers) | !nzchar(valid_identifiers)] <- fallback_ids[is.na(valid_identifiers) | !nzchar(valid_identifiers)]
      rownames(heatmap_matrix) <- valid_identifiers
    } else {
      rownames(heatmap_matrix) <- paste0("Protein_", selected_row_indices)
    }

    # Set column names (selected sample names)
    sample_names <- dd$Sample[abundance_cols]
    colnames(heatmap_matrix) <- sample_names

    # Retransform abundance values to original scale when transformation metadata is present
    tr_vec <- if ("Transformation" %in% colnames(dd)) dd$Transformation[abundance_cols] else character(0)
    if (length(tr_vec) == ncol(heatmap_matrix)) {
      valid_tr <- !is.na(tr_vec) & nzchar(as.character(tr_vec))
      if (any(valid_tr)) {
        heatmap_debug_log("Retransforming selected abundance columns for expression heatmap", 2)
        hm_df <- as.data.frame(heatmap_matrix, check.names = FALSE, stringsAsFactors = FALSE)
        hm_df <- retransform_data_global(
          hm_df,
          index = which(valid_tr),
          transformation_df = as.character(tr_vec[valid_tr])
        )
        heatmap_matrix <- as.matrix(hm_df)
        rownames(heatmap_matrix) <- rownames(hm_df)
        colnames(heatmap_matrix) <- colnames(hm_df)
      }
    }

    # ------------------------------------------------------------------
    # Step 3: Apply sample sorting
    # ------------------------------------------------------------------
    sample_sort_mode <- effective_input$sort_samples_by %||% "none"
    skip_log_transform <- isTRUE(effective_input$skip_log_transform_heatmap)
    zscore_input_scale <- if (skip_log_transform) "metadata-retransformed raw scale" else "log2-transformed scale"

    if (!is.null(sample_names) && length(sample_names) > 1) {
      sort_idx <- seq_along(sample_names)
      canonical_sample_order <- as.character(sample_names)

      if (sample_sort_mode == "alpha_asc") {
        sort_idx <- order(sample_names)

      } else if (sample_sort_mode == "alpha_desc") {
        sort_idx <- order(sample_names, decreasing = TRUE)

      } else if (sample_sort_mode == "pearson_cluster") {
        suppressWarnings({
          cor_mat <- tryCatch(
            stats::cor(heatmap_matrix, use = "pairwise.complete.obs", method = "pearson"),
            error = function(e) {
              heatmap_debug_log(paste("Pearson clustering for samples failed:", e$message), 1)
              NULL
            }
          )
          if (!is.null(cor_mat)) {
            cor_mat[is.na(cor_mat)] <- 0
            d <- tryCatch(
              stats::as.dist(1 - cor_mat),
              error = function(e) {
                heatmap_debug_log(paste("as.dist(1 - cor) failed:", e$message), 1)
                NULL
              }
            )
            if (!is.null(d)) {
              hc <- tryCatch(
                stats::hclust(d, method = "average"),
                error = function(e) {
                  heatmap_debug_log(paste("hclust for sample Pearson clustering failed:", e$message), 1)
                  NULL
                }
              )
              if (!is.null(hc) && length(hc$order) == length(sample_names)) {
                dend <- stats::as.dendrogram(hc)
                dend <- rotate_dend_to_order(dend, canonical_sample_order)
                stable_labels <- tryCatch(as.character(labels(dend)), error = function(e) character(0))
                stable_idx <- match(stable_labels, canonical_sample_order)
                if (length(stable_idx) == length(sample_names) && !any(is.na(stable_idx))) {
                  sort_idx <- stable_idx
                } else {
                  sort_idx <- hc$order
                }
              }
            }
          }
        })

      } else if (sample_sort_mode == "distance_cluster") {
        distance_tmp <- tryCatch({
          if (skip_log_transform) {
            m <- heatmap_matrix
          } else {
            m <- log2(heatmap_matrix + 1)
          }
          m[!is.finite(m)] <- 0
          m
        }, error = function(e) {
          heatmap_debug_log(paste("Temporary matrix preparation for distance clustering failed:", e$message), 1)
          NULL
        })

        if (!is.null(distance_tmp)) {
          suppressWarnings({
            d <- tryCatch(
              stats::dist(t(distance_tmp)),
              error = function(e) {
                heatmap_debug_log(paste("dist(t(distance_tmp)) for sample clustering failed:", e$message), 1)
                NULL
              }
            )
            if (!is.null(d)) {
              hc <- tryCatch(
                stats::hclust(d, method = "ward.D2"),
                error = function(e) {
                  heatmap_debug_log(paste("hclust for distance-based sample clustering failed:", e$message), 1)
                  NULL
                }
              )
              if (!is.null(hc) && length(hc$order) == length(sample_names)) {
                dend <- stats::as.dendrogram(hc)
                dend <- rotate_dend_to_order(dend, canonical_sample_order)
                stable_labels <- tryCatch(as.character(labels(dend)), error = function(e) character(0))
                stable_idx <- match(stable_labels, canonical_sample_order)
                if (length(stable_idx) == length(sample_names) && !any(is.na(stable_idx))) {
                  sort_idx <- stable_idx
                } else {
                  sort_idx <- hc$order
                }
              }
            }
          })
        }

      } else if (sample_sort_mode %in% c("pca1_asc", "pca1_desc", "pca2_asc", "pca2_desc")) {
        pca_prep <- tryCatch({
          m <- heatmap_matrix
          initial_n_proteins <- nrow(m)

          if (initial_n_proteins < 2) {
            heatmap_debug_log(
              paste("Skipping PCA sample sorting: too few proteins before filtering (", initial_n_proteins, ")"),
              1
            )
            return(NULL)
          }
          if (ncol(m) < 2) {
            heatmap_debug_log(
              paste("Skipping PCA sample sorting: too few samples before filtering (", ncol(m), ")"),
              1
            )
            return(NULL)
          }

          if (ncol(m) > 0) {
            row_missing <- rowSums(is.na(m)) / ncol(m)
            keep_rows <- row_missing <= 0.5
            m <- m[keep_rows, , drop = FALSE]
          }

          if (nrow(m) < 2) {
            heatmap_debug_log(
              paste("Skipping PCA sample sorting: too few proteins after >50% NA filter (", nrow(m), ")"),
              1
            )
            return(NULL)
          }

          complete_rows <- complete.cases(m)
          m <- m[complete_rows, , drop = FALSE]
          if (nrow(m) < 2) {
            heatmap_debug_log(
              paste("Skipping PCA sample sorting: too few proteins after complete-case filtering (", nrow(m), ")"),
              1
            )
            return(NULL)
          }

          complete_cols <- apply(m, 2, function(x) !any(is.na(x)))
          keep_sample_idx <- which(complete_cols)
          m <- m[, complete_cols, drop = FALSE]

          if (ncol(m) < 2) {
            heatmap_debug_log(
              paste("Skipping PCA sample sorting: too few complete samples after NA filtering (", ncol(m), ")"),
              1
            )
            return(NULL)
          }

          min_val <- suppressWarnings(min(m, na.rm = TRUE))
          if (!is.finite(min_val)) min_val <- 0
          pseudocount <- if (min_val <= 0) abs(min_val) + 1 else 1

          zscore_tmp <- if (skip_log_transform) {
            m
          } else {
            log2(m + pseudocount)
          }
          zscore_tmp[!is.finite(zscore_tmp)] <- 0

          z_tmp <- t(apply(zscore_tmp, 1, function(row_vals) {
            mu <- mean(row_vals, na.rm = TRUE)
            sigma <- stats::sd(row_vals, na.rm = TRUE)
            if (!is.finite(sigma) || sigma == 0) {
              rep(0, length(row_vals))
            } else {
              (row_vals - mu) / sigma
            }
          }))
          z_tmp[is.na(z_tmp)] <- 0

          analysis_matrix <- t(z_tmp)

          col_vars <- apply(analysis_matrix, 2, stats::var, na.rm = TRUE)
          keep_feature_cols <- is.finite(col_vars) & col_vars > 0
          if (!any(keep_feature_cols)) return(NULL)
          analysis_matrix <- analysis_matrix[, keep_feature_cols, drop = FALSE]

          if (nrow(analysis_matrix) < 2 || ncol(analysis_matrix) < 2) {
            heatmap_debug_log(
              paste(
                "Skipping PCA sample sorting: too few dimensions after variance filtering (",
                nrow(analysis_matrix), "samples x", ncol(analysis_matrix), "features)"
              ),
              1
            )
            return(NULL)
          }

          scaled_matrix <- scale(analysis_matrix, center = TRUE, scale = TRUE)
          if (any(!is.finite(scaled_matrix))) {
            heatmap_debug_log("Skipping PCA sample sorting: non-finite values after scaling", 1)
            return(NULL)
          }

          list(
            data = scaled_matrix,
            keep_sample_idx = keep_sample_idx
          )
        }, error = function(e) {
          heatmap_debug_log(paste("PCA input preparation for sample sorting failed:", e$message), 1)
          NULL
        })

        if (!is.null(pca_prep) && !is.null(pca_prep$data)) {
          suppressWarnings({
            pca <- tryCatch(
              stats::prcomp(
                pca_prep$data,
                center = FALSE,
                scale. = FALSE,
                rank. = min(dim(pca_prep$data)) - 1
              ),
              error = function(e) {
                heatmap_debug_log(paste("PCA for sample sorting failed:", e$message), 1)
                NULL
              }
            )
            comp_idx <- if (sample_sort_mode %in% c("pca2_asc", "pca2_desc")) 2 else 1
            if (!is.null(pca) && ncol(pca$x) >= comp_idx) {
              pc_values <- pca$x[, comp_idx]
              rot <- tryCatch(pca$rotation[, comp_idx], error = function(e) NULL)
              if (!is.null(rot) && length(rot) > 0 && any(is.finite(rot))) {
                max_idx <- which.max(abs(rot))
                if (length(max_idx) == 1 && is.finite(rot[max_idx]) && rot[max_idx] < 0) {
                  pc_values <- -pc_values
                }
              }

              keep_idx <- pca_prep$keep_sample_idx
              if (!is.null(keep_idx) && length(keep_idx) == length(pc_values)) {
                order_in_keep <- if (sample_sort_mode %in% c("pca1_asc", "pca2_asc")) {
                  order(pc_values, na.last = NA)
                } else {
                  order(pc_values, decreasing = TRUE, na.last = NA)
                }

                sorted_keep <- keep_idx[order_in_keep]
                dropped_idx <- setdiff(seq_along(sample_names), keep_idx)
                sort_idx <- c(sorted_keep, dropped_idx)
              } else {
                heatmap_debug_log("Skipping PCA sample sorting: keep_sample_idx/PC score length mismatch", 1)
              }
            } else if (!is.null(pca) && ncol(pca$x) > 0) {
              heatmap_debug_log(
                paste("Skipping requested PCA component", comp_idx, "because only", ncol(pca$x), "component(s) are available"),
                1
              )
            }
          })
        }
      }

      heatmap_matrix <- heatmap_matrix[, sort_idx, drop = FALSE]
      sample_names <- sample_names[sort_idx]
      colnames(heatmap_matrix) <- sample_names
    }

    # Write final sample order to shared reactive state
    heatmap_shared_col_order(sample_names)
    heatmap_debug_log(paste("Expression matrix dimensions:", nrow(heatmap_matrix), "x", ncol(heatmap_matrix)), 2)

    # ------------------------------------------------------------------
    # Step 4: NA handling, optional log2 transformation, z-score normalization
    # ------------------------------------------------------------------
    remove_na_rows <- isTRUE(effective_input$remove_na_abundance_heatmap)

    heatmap_matrix[is.infinite(heatmap_matrix)] <- NA_real_
    if (remove_na_rows) {
      heatmap_matrix[is.na(heatmap_matrix)] <- 0
    }

    if (all(heatmap_matrix == 0, na.rm = TRUE) || nrow(heatmap_matrix) == 0 || ncol(heatmap_matrix) == 0) {
      showNotification("Matrix contains no valid expression data", type = "error", duration = 5)
      return(NULL)
    }

    zscore_input_matrix <- if (skip_log_transform) {
      heatmap_debug_log("Skipping log2 transformation before Z-score calculation; using metadata-retransformed raw abundance values", 1)
      heatmap_matrix
    } else {
      log2(heatmap_matrix + 1)
    }

    if (any(!is.finite(zscore_input_matrix))) {
      heatmap_debug_log(paste("Non-finite values detected in", zscore_input_scale, "before Z-score calculation"), 1)
      zscore_input_matrix[is.infinite(zscore_input_matrix)] <- NA_real_
      zscore_input_matrix[is.nan(zscore_input_matrix)] <- NA_real_
      if (remove_na_rows) {
        zscore_input_matrix[is.na(zscore_input_matrix)] <- 0
      }
    }

    if (remove_na_rows) {
      scaled_matrix <- t(scale(t(zscore_input_matrix)))
      scaled_matrix[is.na(scaled_matrix)] <- 0
    } else {
      scaled_matrix <- heatmap_rowwise_zscore(zscore_input_matrix)
    }

    heatmap_debug_log(paste("Z-score calculation completed on", zscore_input_scale, ".",
                    sum(is.finite(scaled_matrix)), "finite values out of",
                    length(scaled_matrix), "total values"), 2)

    # ------------------------------------------------------------------
    # Step 5: Store matrix, compute clustering, create heatmap object
    # ------------------------------------------------------------------
    heatmap_expression_matrix(scaled_matrix)

    cl_info <- compute_expression_clustering(scaled_matrix, effective_input)
    heatmap_cluster_info(cl_info)
    if (!is.null(cl_info$row_order_idx) && length(cl_info$row_order_idx) == nrow(scaled_matrix)) {
      protein_row_order <- rownames(scaled_matrix)[cl_info$row_order_idx]
      if (identical(effective_input$sort_proteins_by %||% "z_score", "custom")) {
        protein_row_order <- heatmap_apply_custom_protein_order(
          heatmap_custom_fallback_order(scaled_matrix, effective_input),
          effective_input$custom_protein_order %||% ""
        )
      }
      heatmap_shared_row_order(protein_row_order)
    }

    expr_ht <- create_expression_heatmap_object(scaled_matrix, groups = NULL, input = effective_input, cluster_info = cl_info)

    # Add protein annotations if custom proteins are selected
    custom_proteins <- heatmap_selected_proteins()
    if (!is.null(custom_proteins) && length(custom_proteins) > 0) {
      protein_annotation <- create_protein_annotation(rownames(scaled_matrix), custom_proteins)
      if (!is.null(protein_annotation)) {
        expr_ht <- protein_annotation + expr_ht
      }
    }

    heatmap_debug_log("Expression heatmap created successfully", 2)

    # Level-0 logs for session reproducibility — three calls covering data
    # selection, filtering, and customization settings.

    # -- 1. Data selection --
    {
      l0_data_type    <- tryCatch(data_type,                                         error = function(e) NA_character_)
      l0_samples      <- tryCatch(paste(selected_samples, collapse = ", "),           error = function(e) NA_character_)
      l0_identifier   <- tryCatch(effective_input$GeneIdentifierColumn_Heatmap %||% NA_character_, error = function(e) NA_character_)
      l0_sample_sort  <- tryCatch(sample_sort_mode,                                  error = function(e) NA_character_)
      l0_protein_sort <- tryCatch(effective_input$sort_proteins_by %||% NA_character_,         error = function(e) NA_character_)
      l0_skip_log     <- tryCatch(skip_log_transform,                                error = function(e) FALSE)
      heatmap_debug_log(
        sprintf(
          "Heatmap data selection | Data type: %s | Samples: %s | Gene identifier: %s | Sample sorting: %s | Protein sorting: %s | Skip log2 before Z-score: %s",
          l0_data_type, l0_samples, l0_identifier, l0_sample_sort, l0_protein_sort, l0_skip_log
        ),
        level = 0
      )
    }

    # -- 2. Filtering --
    {
      l0_rm_na        <- tryCatch(isTRUE(effective_input$remove_na_abundance_heatmap),         error = function(e) NA)
      l0_ratio_active <- tryCatch(isTRUE(effective_input$enable_ratio_filter_heatmap),         error = function(e) FALSE)
      l0_pval_active  <- tryCatch(isTRUE(effective_input$enable_pvalue_filter_heatmap),        error = function(e) FALSE)
      l0_n_proteins   <- nrow(scaled_matrix)
      l0_max_prot     <- tryCatch(as.character(effective_input$max_proteins_heatmap %||% NA_real_), error = function(e) NA_character_)
      l0_id_filter_text <- tryCatch(effective_input$custom_proteins_filter %||% "", error = function(e) "")
      l0_id_filter_summary <- tryCatch(attr(results, "identifier_filter_summary"), error = function(e) NULL)
      if (is.null(l0_id_filter_summary) && nzchar(paste(as.character(l0_id_filter_text), collapse = "\n"))) {
        l0_id_filter_details <- tryCatch(
          heatmap_identifier_filter_details(l0_id_filter_text, results$Identifier %||% character(0)),
          error = function(e) NULL
        )
        if (!is.null(l0_id_filter_details)) {
          l0_id_filter_summary <- list(
            active = length(l0_id_filter_details$parsed_identifiers) > 0,
            input_entries = length(l0_id_filter_details$raw_entries),
            unique_parsed_ids = length(l0_id_filter_details$parsed_identifiers),
            matched_rows = l0_id_filter_details$matched_rows,
            unique_matched_ids = length(l0_id_filter_details$unique_matched_identifiers)
          )
        }
      }

      ratio_part <- if (isTRUE(l0_ratio_active)) {
        sprintf(
          " | Abundance ratio: active | Column: %s | Mode: %s | Threshold: %s",
          tryCatch(effective_input$abundance_ratio_col_heatmap %||% NA_character_,          error = function(e) NA_character_),
          tryCatch(effective_input$ratio_filter_mode_heatmap   %||% NA_character_,          error = function(e) NA_character_),
          tryCatch(as.character(effective_input$ratio_threshold_heatmap %||% NA_real_),      error = function(e) NA_character_)
        )
      } else {
        " | Abundance ratio: inactive"
      }

      pval_part <- if (isTRUE(l0_pval_active)) {
        sprintf(
          " | P-value filter: active | Type: %s | Column: %s | Threshold: %s",
          tryCatch(effective_input$pval_type_heatmap                          %||% NA_character_, error = function(e) NA_character_),
          tryCatch(effective_input$pval_col_heatmap                           %||% NA_character_, error = function(e) NA_character_),
          tryCatch(as.character(effective_input$pval_threshold_heatmap %||% NA_real_),            error = function(e) NA_character_)
        )
      } else {
        " | P-value filter: inactive"
      }

      id_filter_part <- if (is.list(l0_id_filter_summary) && isTRUE(l0_id_filter_summary$active)) {
        sprintf(
          " | Identifier filter: active | Input entries: %d | Unique parsed IDs: %d | Matched rows: %d | Unique matched IDs: %d",
          as.integer(l0_id_filter_summary$input_entries %||% 0L),
          as.integer(l0_id_filter_summary$unique_parsed_ids %||% 0L),
          as.integer(l0_id_filter_summary$matched_rows %||% 0L),
          as.integer(l0_id_filter_summary$unique_matched_ids %||% 0L)
        )
      } else {
        " | Identifier filter: inactive"
      }

      heatmap_debug_log(
        paste0(
          sprintf("Heatmap filtering | Remove missing values: %s", l0_rm_na),
          ratio_part,
          pval_part,
          id_filter_part,
          sprintf(" | Proteins shown: %d | Max proteins: %s", l0_n_proteins, l0_max_prot)
        ),
        level = 0
      )
    }

    # -- 3. Customization --
    {
      l0_col_low     <- tryCatch(effective_input$Heatmap_ColorInput_1 %||% NA_character_, error = function(e) NA_character_)
      l0_col_mid     <- tryCatch(effective_input$Heatmap_ColorInput_2 %||% NA_character_, error = function(e) NA_character_)
      l0_col_high    <- tryCatch(effective_input$Heatmap_ColorInput_3 %||% NA_character_, error = function(e) NA_character_)
      l0_contrast    <- tryCatch(isTRUE(effective_input$correlation_enhanced_contrast),   error = function(e) NA)
      l0_diag_active <- tryCatch(isTRUE(effective_input$show_correlation_diagonal),       error = function(e) FALSE)
      l0_basemean    <- tryCatch(isTRUE(effective_input$show_basemean_heatmap),           error = function(e) NA)
      l0_ratio_col   <- tryCatch(isTRUE(effective_input$show_abundance_ratio_heatmap),    error = function(e) NA)

      diag_part <- if (isTRUE(l0_diag_active)) {
        sprintf(
          "active | Color: %s | Width: %s | Rotate: %s",
          tryCatch(effective_input$diagonal_line_color  %||% NA_character_,          error = function(e) NA_character_),
          tryCatch(as.character(effective_input$diagonal_line_width %||% NA_real_), error = function(e) NA_character_),
          tryCatch(isTRUE(effective_input$diagonal_rotate),                          error = function(e) NA)
        )
      } else {
        "inactive"
      }

      heatmap_debug_log(
        sprintf(
          "Heatmap customization | Colors: low=%s mid=%s high=%s | Contrast: %s | Diagonal line: %s | Basemean column: %s | Abundance ratio column: %s",
          l0_col_low, l0_col_mid, l0_col_high,
          l0_contrast,
          diag_part,
          if (isTRUE(l0_basemean)) "active" else "inactive",
          if (isTRUE(l0_ratio_col)) "active" else "inactive"
        ),
        level = 0
      )
    }

    return(expr_ht)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in heatmap_create_expression_heatmap:", e$message), 1)
    showNotification(paste("Failed to create expression heatmap:", e$message), type = "error", duration = 5)
    return(NULL)
  })
}
