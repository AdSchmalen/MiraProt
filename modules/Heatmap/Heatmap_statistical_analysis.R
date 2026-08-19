# ==============================================================================
# Heatmap Module - Statistical Analysis
# ==============================================================================

#' Apply log2 ratio filter to protein ratios
#' @param ratio_values numeric vector of ratio values
#' @param filter_mode character, filter mode ("abs_log2_gt" or "abs_log2_lt")
#' @param threshold numeric, threshold value
#' @return logical vector indicating which proteins pass the filter
heatmap_apply_log2_ratio_filter <- function(ratio_values, filter_mode, threshold) {
  tryCatch({
    heatmap_debug_log(paste("Applying log2 ratio filter - mode:", filter_mode, "threshold:", threshold), 2)

    # Convert to numeric and handle NAs
    r_vec <- suppressWarnings(as.numeric(ratio_values))

    # Initialize pass vector (all FALSE by default)
    pass <- rep(FALSE, length(r_vec))

    # Only process finite, positive values (ratios should be positive)
    valid_indices <- which(is.finite(r_vec) & r_vec > 0)

    if (length(valid_indices) == 0) {
      heatmap_debug_log("No valid ratio values found for filtering", 1)
      return(pass)
    }

    # Calculate log2 values for valid ratios
    log2_ratios <- log2(r_vec[valid_indices])

    # Apply filter based on mode
    if (filter_mode == "abs_log2_gt") {
      # Show proteins with large changes: |log2(ratio)| > threshold
      filter_pass <- abs(log2_ratios) > threshold
      heatmap_debug_log(paste("Filter: |log2(ratio)| >", threshold, "-", sum(filter_pass), "proteins pass"), 2)
    } else if (filter_mode == "abs_log2_lt") {
      # Show proteins with small changes: |log2(ratio)| < threshold
      filter_pass <- abs(log2_ratios) < threshold
      heatmap_debug_log(paste("Filter: |log2(ratio)| <", threshold, "-", sum(filter_pass), "proteins pass"), 2)
    } else {
      heatmap_debug_log(paste("Unknown filter mode:", filter_mode, "- using all proteins"), 1)
      filter_pass <- rep(TRUE, length(log2_ratios))
    }

    # Map results back to original indices
    pass[valid_indices] <- filter_pass

    heatmap_debug_log(paste("Ratio filter complete:", sum(pass), "out of", length(r_vec), "proteins passed"), 1)
    return(pass)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in ratio filtering:", e$message), 1)
    # Return all FALSE in case of error
    return(rep(FALSE, length(ratio_values)))
  })
}

#' Draw diagonal line over correlation heatmap
#' @param input shiny input object containing UI settings
heatmap_check_pvalue_availability <- function(rv = NULL, pval_type = NULL, pval_col = NULL) {
  tryCatch({
    if (is.null(rv) || is.null(rv$data_def)) {
      heatmap_debug_log("No data definition available for p-value check", 2)
      return(FALSE)
    }

    dd <- rv$data_def

    # EXPANDED: More comprehensive p-value content types
    pvalue_content_types <- c(
      "Abundance Ratio p-Value",
      "Abundance Ratio Adj. p-Value",
      "P-Value",
      "Adj. P-Value",
      "FDR",
      "p.value",
      "adj.p.value"
    )

    # If specific type/column requested, check for that specifically
    if (!is.null(pval_type) && !is.null(pval_col) && nzchar(pval_type) && nzchar(pval_col)) {
      heatmap_debug_log(paste("Checking for specific p-value column:", pval_type, "/", pval_col), 2)
      specific_idx <- which(dd$Content == pval_type & dd$Column == pval_col)

      if (length(specific_idx) == 0) {
        heatmap_debug_log(paste("Specific p-value column not found:", pval_type, "/", pval_col), 1)
        heatmap_debug_log(paste("Available Content types:", paste(unique(dd$Content), collapse = ", ")), 1)
        if ("Column" %in% names(dd)) {
          available_cols <- unique(dd$Column[dd$Content == pval_type])
          heatmap_debug_log(paste("Available columns for", pval_type, ":", paste(available_cols, collapse = ", ")), 1)
        }
        return(FALSE)
      }
      pvalue_cols <- specific_idx
      heatmap_debug_log(paste("Found specific p-value column at index:", specific_idx), 1)
    } else {
      # General check for any p-value columns
      heatmap_debug_log("Performing general p-value availability check", 2)
      pvalue_cols <- which(dd$Content %in% pvalue_content_types)

      heatmap_debug_log(paste("Available Content types:", paste(unique(dd$Content), collapse = ", ")), 1)
      heatmap_debug_log(paste("Looking for p-value types:", paste(pvalue_content_types, collapse = ", ")), 1)
      heatmap_debug_log(paste("Found p-value columns at indices:", paste(pvalue_cols, collapse = ", ")), 1)
    }

    if (length(pvalue_cols) == 0) {
      heatmap_debug_log("No p-value columns found in data definition", 2)
      return(FALSE)
    }

    # Additional check: verify that p-value columns contain actual valid data
    if (!is.null(rv$data_mod)) {
      dm <- rv$data_mod

      # Check if any of the p-value columns have valid numeric data
      has_valid_pvalues <- FALSE
      for (col_idx in pvalue_cols) {
        if (col_idx <= ncol(dm)) {
          col_data <- suppressWarnings(as.numeric(dm[[col_idx]]))
          # Check for finite values between 0 and 1 (valid p-values)
          valid_pvals <- is.finite(col_data) & col_data >= 0 & col_data <= 1

          if (any(valid_pvals)) {
            heatmap_debug_log(paste("Column", col_idx, "(", dd$Content[col_idx], ") has", sum(valid_pvals), "valid p-values"), 2)
            has_valid_pvalues <- TRUE
          } else {
            heatmap_debug_log(paste("Column", col_idx, "(", dd$Content[col_idx], ") has no valid p-values"), 2)
          }
        }
      }

      if (!has_valid_pvalues) {
        heatmap_debug_log("P-value columns found but contain no valid p-value data (0-1 range)", 1)
        return(FALSE)
      }
    }

    heatmap_debug_log(paste("SUCCESS: Found", length(pvalue_cols), "p-value columns with valid data"), 1)
    return(TRUE)

  }, error = function(e) {
    heatmap_debug_log(paste("Error checking p-value availability:", e$message), 1)
    return(FALSE)
  })
}

#' Enhanced abundance ratio availability check (unchanged - already good)
#' @param rv reactive values object
#' @return list with availability status and detailed info
heatmap_check_ratio_availability_enhanced <- function(rv = NULL) {
  tryCatch({
    if (is.null(rv) || is.null(rv$data_def)) {
      return(list(available = FALSE, reason = "No data definition available"))
    }

    dd <- rv$data_def

    # Check if there are any columns with "Abundance Ratio" content
    ratio_cols <- which(dd$Content == "Abundance Ratio")

    if (length(ratio_cols) == 0) {
      return(list(available = FALSE, reason = "No Abundance Ratio columns found"))
    }

    # Additional check: verify that the ratio columns contain actual data
    if (!is.null(rv$data_mod)) {
      dm <- rv$data_mod

      valid_ratio_cols <- 0
      for (col_idx in ratio_cols) {
        if (col_idx <= ncol(dm)) {
          col_data <- suppressWarnings(as.numeric(dm[[col_idx]]))
          # Check for finite, positive values (ratios should be positive)
          if (any(is.finite(col_data) & col_data > 0)) {
            valid_ratio_cols <- valid_ratio_cols + 1
          }
        }
      }

      if (valid_ratio_cols == 0) {
        return(list(available = FALSE, reason = "Abundance Ratio columns contain no valid positive data"))
      }

      return(list(
        available = TRUE,
        reason = paste("Found", valid_ratio_cols, "valid abundance ratio columns"),
        count = valid_ratio_cols
      ))
    }

    return(list(
      available = TRUE,
      reason = paste("Found", length(ratio_cols), "abundance ratio columns"),
      count = length(ratio_cols)
    ))

  }, error = function(e) {
    return(list(
      available = FALSE,
      reason = paste("Error checking ratio availability:", e$message)
    ))
  })
}

#' Early termination helper function
#' @param protein_mask current protein selection mask
#' @param filter_name name of the filter for notification
#' @param suggestion optional suggestion for user
#' @return TRUE if proteins remain, FALSE if should terminate
check_proteins_remaining <- function(protein_mask, filter_name, suggestion = NULL) {
  proteins_remaining <- sum(protein_mask)

  if (proteins_remaining == 0) {
    message_text <- paste("No proteins pass", filter_name, "criteria")
    if (!is.null(suggestion)) {
      message_text <- paste(message_text, ".", suggestion)
    }

    heatmap_debug_log(paste("TERMINATION:", message_text), 1)
    showNotification(message_text, type = "error", duration = 6)
    return(FALSE)
  }

  heatmap_debug_log(paste("After", filter_name, ":", proteins_remaining, "proteins remain"), 1)
  return(TRUE)
}

#' Filter 0: Remove proteins with NA values in abundance data
#' @param dm data matrix
#' @param abundance_col_indices column indices for selected abundance data
#' @param remove_na_enabled logical, whether NA removal is enabled
#' @return logical vector indicating proteins without NA values
heatmap_filter_na_abundance <- function(dm, abundance_col_indices, remove_na_enabled = TRUE) {
  tryCatch({
    total_proteins <- nrow(dm)
    heatmap_debug_log(paste("Starting with", total_proteins, "total proteins"), 1)
    heatmap_debug_log(paste("Checking", length(abundance_col_indices), "abundance columns for NA values"), 2)

    if (!remove_na_enabled) {
      heatmap_debug_log("NA filtering disabled - keeping all proteins", 1)
      return(rep(TRUE, total_proteins))
    }

    if (length(abundance_col_indices) == 0) {
      heatmap_debug_log("No abundance columns specified - keeping all proteins", 1)
      return(rep(TRUE, total_proteins))
    }

    # Extract abundance data for selected columns
    abundance_data <- dm[, abundance_col_indices, drop = FALSE]

    # Check each row for NA values
    # A protein passes if ALL selected abundance columns have non-NA values
    na_mask <- complete.cases(abundance_data)

    proteins_with_complete_data <- sum(na_mask)
    proteins_with_na <- total_proteins - proteins_with_complete_data

    heatmap_debug_log(paste("Proteins with complete abundance data:", proteins_with_complete_data), 1)
    heatmap_debug_log(paste("Proteins with NA abundance data:", proteins_with_na), 1)

    if (proteins_with_na > 0) {
      heatmap_debug_log(paste("Removing", proteins_with_na, "proteins due to missing abundance values"), 1)

      # Show which columns have the most NAs for debugging
      na_counts_per_col <- sapply(abundance_col_indices, function(col_idx) {
        sum(is.na(dm[[col_idx]]))
      })

      if (any(na_counts_per_col > 0)) {
        heatmap_debug_log(paste("NA counts per column:", paste(na_counts_per_col, collapse = ", ")), 2)
      }
    }

    heatmap_debug_log(paste("Filter 0 result:", proteins_with_complete_data, "proteins pass NA filter"), 1)
    return(na_mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in NA abundance filter:", e$message), 1)
    # In case of error, return all TRUE (don't filter anything)
    return(rep(TRUE, nrow(dm)))
  })
}


# -------------------------
# Legend helpers
# -------------------------

heatmap_perform_statistical_analysis <- function(rv, input, highlighted_proteins = NULL, data_pair_for_restore = NULL, plot_request = NULL, input_values = NULL, silent_restore = FALSE) {
  heatmap_debug_log("Running metadata-driven filtering with robust data availability handling", 2)

  data_pair <- tryCatch({
    if (is.function(data_pair_for_restore)) data_pair_for_restore() else list(data_mod = rv$data_mod, data_def = rv$data_def)
  }, error = function(e) {
    heatmap_debug_log(paste("Failed to resolve heatmap data pair:", e$message), 1)
    NULL
  })
  dd <- if (is.list(data_pair)) data_pair$data_def else NULL
  dm <- if (is.list(data_pair)) data_pair$data_mod else NULL

  # Get UI inputs - prefer an explicit restore request/snapshot over live Shiny input
  effective_input <- plot_request %||% input_values %||% input
  data_type   <- effective_input$custom_col_sel_heatmap
  samples     <- effective_input$select_samples_heatmap
  pval_type <- effective_input$pval_type_heatmap
  pval_col  <- effective_input$pval_col_heatmap

  # Filtering and the optional single-column ratio heatmap deliberately have
  # independent column selections.
  ratio_col <-
    effective_input$abundance_ratio_col_heatmap

  ratio_heatmap_col <-
    effective_input$abundance_ratio_col_extension_heatmap %||%
    ratio_col

  heatmap_restore_failure_context <- function(reason) {
    available_data_types <- character(0)
    available_samples <- character(0)
    if (inherits(dd, "data.frame")) {
      if ("Content" %in% names(dd)) {
        available_data_types <- sort(unique(stats::na.omit(as.character(dd$Content))))
      }
      if ("Sample" %in% names(dd)) {
        available_samples <- sort(unique(stats::na.omit(as.character(dd$Sample))))
      }
    }
    paste(
      "[Heatmap] session restore failure:", reason,
      "| selected data type=", paste(as.character(data_type %||% "<NULL>"), collapse = ", "),
      "| selected samples=", paste(as.character(samples %||% character(0)), collapse = ", "),
      "| available data types=", paste(available_data_types, collapse = ", "),
      "| available samples=", paste(available_samples, collapse = ", "),
      "| selected p-value type/column=", paste(as.character(c(pval_type %||% "<NULL>", pval_col %||% "<NULL>")), collapse = " / "),
      "| selected ratio column=", paste(as.character(ratio_col %||% "<NULL>"), collapse = ", ")
    )
  }

  notify_or_log_heatmap_restore_failure <- function(message_text, type = "error", duration = 5, reason = message_text) {
    if (isTRUE(silent_restore)) {
      heatmap_debug_log(heatmap_restore_failure_context(reason), 1)
    } else {
      showNotification(message_text, type = type, duration = duration)
    }
  }

  if (!inherits(dm, "data.frame") || !inherits(dd, "data.frame")) {
    notify_or_log_heatmap_restore_failure(
      "Data not loaded",
      type = "error",
      duration = 5,
      reason = "data_mod/data_def are not available data frames"
    )
    heatmap_debug_log("Heatmap creation stopped: data_mod/data_def are not available data frames", 1)
    return(NULL)
  }

  # NEW: NA filter control
  remove_na_abundance <- isTRUE(effective_input$remove_na_abundance_heatmap)

  # P-value and ratio filter controls
  enable_pvalue_filter <- isTRUE(effective_input$enable_pvalue_filter_heatmap)
  use_rfilter <- isTRUE(effective_input$enable_ratio_filter_heatmap)

  # Filter thresholds
  pthr        <- effective_input$pval_threshold_heatmap %||% 0.05
  max_proteins <- effective_input$max_proteins_heatmap %||% 50
  max_proteins <- suppressWarnings(as.integer(max_proteins))
  if (is.na(max_proteins) || !is.finite(max_proteins)) {
    max_proteins <- 50L
  }
  max_proteins <- max(2L, max_proteins)
  min_abundance_values <- effective_input$min_abundance_values_per_row_heatmap %||% 1
  rmode       <- effective_input$ratio_filter_mode_heatmap %||% "abs_log2_gt"
  rthr        <- effective_input$ratio_threshold_heatmap %||% 1.0

  # Custom proteins text input
  custom_filter_text <- paste(as.character(effective_input$custom_proteins_filter %||% ""), collapse = "\n")

  # Robust normalization for optional highlighted proteins
  highlighted_proteins <- highlighted_proteins %||% character(0)
  highlighted_proteins <- trimws(as.character(highlighted_proteins))
  highlighted_proteins <- highlighted_proteins[is.finite(nchar(highlighted_proteins)) & nzchar(highlighted_proteins)]
  highlighted_proteins <- unique(highlighted_proteins)

  heatmap_debug_log(paste("Data type selected:", data_type %||% "NULL"), 2)
  heatmap_debug_log(paste("Samples selected:", length(samples %||% c()), "samples"), 2)
  heatmap_debug_log(paste("NA abundance filtering enabled:", remove_na_abundance), 2)
  heatmap_debug_log(paste("P-value filtering enabled:", enable_pvalue_filter), 2)
  heatmap_debug_log(paste("Ratio filtering enabled:", use_rfilter), 2)
  heatmap_debug_log(paste("Minimum abundance values per row:", min_abundance_values), 2)
  heatmap_debug_log(paste("Max proteins:", max_proteins), 2)
  heatmap_debug_log(paste("Normalized highlighted proteins:", length(highlighted_proteins)), 2)

  # 1) Resolve expression column indices
  idx_expr <- which(dd$Content == data_type & !is.na(dd$Sample) & dd$Sample %in% samples)
  heatmap_debug_log(paste("Found", length(idx_expr), "expression columns for selected samples"), 2)

  if (length(idx_expr) == 0) {
    notify_or_log_heatmap_restore_failure(
      "No samples found for selected data type",
      type = "error",
      duration = 5,
      reason = "No samples found for selected data type"
    )
    return(NULL)
  }

  # 2) Get identifier column
  identifier_col <- as.character(effective_input$identifier_column %||% effective_input$GeneIdentifierColumn_Heatmap %||% "")[1]
  if (is.na(identifier_col) || !nzchar(identifier_col)) {
    notify_or_log_heatmap_restore_failure(
      "No identifier column selected",
      type = "error",
      duration = 5,
      reason = "No identifier column selected"
    )
    return(NULL)
  }

  resolved_identifier_col <- resolve_heatmap_identifier_column(identifier_col, dd, dm)
  if (is.null(resolved_identifier_col)) {
    notify_or_log_heatmap_restore_failure(
      "Identifier column not found",
      type = "error",
      duration = 5,
      reason = paste("Identifier column not found:", identifier_col)
    )
    return(NULL)
  }
  effective_input$GeneIdentifierColumn_Heatmap <- resolved_identifier_col

  # Extract identifiers vector from dm and normalize to character, trimmed
  identifiers <- dm[[resolved_identifier_col]]
  identifiers_chr <- trimws(as.character(identifiers))

  restored_row_indices <- if (isTRUE(silent_restore)) {
    suppressWarnings(as.integer(effective_input$selected_row_indices %||% effective_input$restored_row_indices %||% integer(0)))
  } else integer(0)
  restored_row_indices <- restored_row_indices[is.finite(restored_row_indices) & restored_row_indices >= 1L & restored_row_indices <= nrow(dm)]
  restored_row_indices <- unique(restored_row_indices)
  if (isTRUE(silent_restore) && length(restored_row_indices) > 0L) {
    max_proteins <- length(restored_row_indices)
    heatmap_debug_log(paste("[Heatmap] session restore: using saved selected row indices:", length(restored_row_indices)), 1)
  }

  # 3) Prepare expression data for basemean calculation
  expr_df <- as.data.frame(dm[, idx_expr, drop = FALSE])
  tr_vec  <- if ("Transformation" %in% names(dd)) dd$Transformation[idx_expr] else character(0)

  if (length(tr_vec) > 0) {
    heatmap_debug_log("Re-transforming expression data to original scale", 2)
    expr_df <- retransform_data_global(expr_df, seq_len(ncol(expr_df)), tr_vec)

    # Check for non-finite values
    non_finite_count <- sum(!is.finite(as.matrix(expr_df)))
    if (non_finite_count > 0) {
      heatmap_debug_log(paste("Warning:", non_finite_count, "non-finite values after re-transformation"), 1)
    }
  }

  # Coerce to numeric
  for (j in seq_len(ncol(expr_df))) {
    expr_df[[j]] <- suppressWarnings(as.numeric(expr_df[[j]]))
  }

  # Compute basemean
  basemean <- rowMeans(as.matrix(expr_df), na.rm = TRUE)

  heatmap_debug_log(paste("Starting with", nrow(dm), "total proteins"), 2)

  if (isTRUE(silent_restore) && length(restored_row_indices) > 0L) {
    selected_indices <- restored_row_indices
    results <- data.frame(
      `Row Index` = selected_indices,
      Identifier = identifiers_chr[selected_indices],
      baseMean = basemean[selected_indices],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (!is.null(pval_col) && nzchar(pval_col)) {
      p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)
      if (length(p_idx) == 1) {
        p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))
        results$p.value <- p_vec[selected_indices]
        results$adj.p.value <- results$p.value
      }
    }
    # Add the ratio values used by the optional single-column ratio heatmap.
    # This selection is independent of the ratio-filter column.
    if (!is.null(ratio_heatmap_col) &&
        nzchar(ratio_heatmap_col)) {

      r_idx <- which(
        dd$Content == "Abundance Ratio" &
          dd$Column == ratio_heatmap_col
      )

      if (length(r_idx) == 1L) {
        r_vec <-
          suppressWarnings(
            as.numeric(
              dm[[r_idx]]
            )
          )

        results$Abundance.Ratio <-
          r_vec[selected_indices]
      }
    }
    attr(results, "data_pair") <- list(data_mod = dm, data_def = dd)
    heatmap_debug_log(paste("[Heatmap] session restore: created results from saved row indices with", nrow(results), "proteins"), 1)
    return(results)
  }

  # ==========================================
  # FILTER 0: MINIMUM ABUNDANCE VALUES PER ROW
  # ==========================================
  if (is.null(min_abundance_values) || !is.finite(min_abundance_values)) {
    min_abundance_values <- 1
  }
  min_abundance_values <- max(1L, as.integer(round(min_abundance_values)))

  expr_matrix_for_count <- as.matrix(dm[, idx_expr, drop = FALSE])
  storage.mode(expr_matrix_for_count) <- "numeric"
  non_missing_counts <- rowSums(!is.na(expr_matrix_for_count))
  protein_mask <- non_missing_counts >= min_abundance_values

  heatmap_debug_log(
    paste(
      "Min-abundance-values filter:",
      sum(protein_mask),
      "proteins with >=",
      min_abundance_values,
      "values"
    ),
    1
  )

  if (!check_proteins_remaining(protein_mask, "minimum abundance values filter",
                                "Try lowering the minimum abundance value threshold")) {
    return(NULL)
  }

  # ==========================================
  # FILTER 1: NA ABUNDANCE FILTER (NEW!)
  # ==========================================
  na_mask <- heatmap_filter_na_abundance(
    dm = dm,
    abundance_col_indices = idx_expr,
    remove_na_enabled = remove_na_abundance
  )
  protein_mask <- protein_mask & na_mask

  # CRITICAL: Early termination check after NA filtering
  if (!check_proteins_remaining(protein_mask, "NA abundance filter",
                                "Most proteins have missing abundance data. Consider reviewing data quality or disabling NA filtering")) {
    return(NULL)
  }

  # ==========================================
  # FILTER 2: P-VALUE FILTERING
  # ==========================================
  if (enable_pvalue_filter) {
    # Enhanced p-value availability check
    pvalue_available <- FALSE
    if (!is.null(pval_type) && !is.null(pval_col) && nzchar(pval_type) && nzchar(pval_col)) {
      pvalue_available <- heatmap_check_pvalue_availability(rv, pval_type, pval_col)
    } else {
      pvalue_available <- heatmap_check_pvalue_availability(rv)
    }

    if (pvalue_available) {
      heatmap_debug_log("Applying p-value filtering", 2)

      p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)
      if (length(p_idx) == 1) {
        p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))

        if (any(is.finite(p_vec))) {
          p_pass <- p_vec <= pthr & is.finite(p_vec)
          p_pass[is.na(p_pass)] <- FALSE
          protein_mask <- protein_mask & p_pass

          heatmap_debug_log(paste("P-value filter:", sum(p_pass), "proteins pass out of",
                          sum(is.finite(p_vec)), "with valid p-values"), 2)
        } else {
          heatmap_debug_log("No finite p-values found, skipping p-value filter", 2)
        }
      } else {
        heatmap_debug_log("P-value column not found, skipping p-value filter", 2)
      }
    } else {
      heatmap_debug_log("P-value filtering requested but not available - skipping", 2)
      showNotification("P-value filtering disabled: requested p-values not available",
                       type = "warning", duration = 4)
    }
  } else {
    heatmap_debug_log("P-value filtering disabled", 2)
  }

  # CRITICAL: Early termination check after p-value filtering
  if (!check_proteins_remaining(protein_mask, "p-value filter",
                                "Consider increasing p-value threshold or disabling p-value filtering")) {
    return(NULL)
  }

  # ==========================================
  # FILTER 3: ABUNDANCE RATIO FILTERING
  # ==========================================
  if (use_rfilter) {
    ratio_check <- heatmap_check_ratio_availability_enhanced(rv)

    if (ratio_check$available && !is.null(ratio_col) && nzchar(ratio_col)) {
      heatmap_debug_log("Applying abundance ratio filter", 2)

      r_idx <- which(dd$Content == "Abundance Ratio" & dd$Column == ratio_col)
      if (length(r_idx) == 1) {
        r_vec <- suppressWarnings(as.numeric(dm[[r_idx]]))

        # Use the enhanced ratio filter function
        r_pass <- heatmap_apply_log2_ratio_filter(r_vec, rmode, rthr)
        protein_mask <- protein_mask & r_pass

        heatmap_debug_log(paste("Ratio filter applied:", sum(r_pass), "proteins passed"), 1)
      } else {
        heatmap_debug_log("Ratio column not found - skipping ratio filter", 1)
      }
    } else {
      heatmap_debug_log("Ratio filter enabled but no ratio data available - skipping", 1)
    }
  } else {
    heatmap_debug_log("Ratio filtering disabled", 2)
  }

  # CRITICAL: Early termination check after ratio filtering
  if (!check_proteins_remaining(protein_mask, "abundance ratio filter",
                                "Consider relaxing ratio threshold or disabling ratio filtering")) {
    return(NULL)
  }

  # ==========================================
  # FILTER 4: CUSTOM IDENTIFIER FILTERING
  # ==========================================
  identifier_filter_details <- NULL
  if (nzchar(custom_filter_text)) {
    heatmap_debug_log("Applying custom proteins filter", 2)
    identifier_filter_details <- heatmap_identifier_filter_details(custom_filter_text, identifiers_chr)
    custom_proteins <- identifier_filter_details$parsed_identifiers

    heatmap_debug_log(
      paste(
        "Identifier filter input summary | Raw input entries:",
        length(identifier_filter_details$raw_entries),
        "| Unique parsed identifiers:",
        length(identifier_filter_details$parsed_identifiers),
        "| Exact matched rows:",
        identifier_filter_details$matched_rows,
        "| Unique matched identifiers:",
        length(identifier_filter_details$unique_matched_identifiers)
      ),
      1
    )

    if (length(custom_proteins) > 0) {
      custom_mask <- identifier_filter_details$match_mask
      protein_mask <- protein_mask & custom_mask
    }
  } else {
    heatmap_debug_log("No custom protein filtering", 2)
  }

  # CRITICAL: Early termination check after custom filtering
  if (!check_proteins_remaining(protein_mask, "custom protein filter",
                                "Check your custom protein list for typos or try different identifiers")) {
    return(NULL)
  }

  # ==========================================
  # FILTER 5: MAX PROTEINS LIMITING
  # ==========================================
  proteins_passing_filters <- sum(protein_mask)
  heatmap_debug_log(paste("Before max protein filter:", proteins_passing_filters, "proteins available"), 2)

  # Final check before max protein limiting
  if (proteins_passing_filters < 2) {
    showNotification(
      paste("Insufficient proteins for heatmap:", proteins_passing_filters, "found. Minimum 2 required."),
      type = "error",
      duration = 6
    )
    return(NULL)
  }

  if (proteins_passing_filters > max_proteins) {
    heatmap_debug_log(paste("Applying max protein limit - selecting", max_proteins, "from", proteins_passing_filters, "candidates"), 2)

    # Get indices of proteins that passed filters
    passing_indices <- which(protein_mask)

    # Build deterministic ranking helper that preserves previous behavior:
    # use p-values when available/valid, otherwise row index.
    rank_indices <- function(indices) {
      if (length(indices) == 0) return(integer(0))
      if (enable_pvalue_filter && !is.null(pval_col) && nzchar(pval_col)) {
        p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)
        if (length(p_idx) == 1) {
          p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))
          p_values <- p_vec[indices]
          if (any(is.finite(p_values))) {
            return(indices[order(p_values, indices, na.last = TRUE)])
          }
        }
      }
      sort(indices)
    }

    highlight_indices <- integer(0)
    if (length(highlighted_proteins) > 0) {
      highlight_mask <- identifiers_chr[passing_indices] %in% highlighted_proteins
      highlight_indices <- passing_indices[highlight_mask]
    }
    non_highlight_indices <- setdiff(passing_indices, highlight_indices)

    ranked_highlight_indices <- rank_indices(highlight_indices)
    ranked_non_highlight_indices <- rank_indices(non_highlight_indices)

    if (length(ranked_highlight_indices) > 0) {
      heatmap_debug_log(
        paste(
          "Highlight-priority selection:",
          length(ranked_highlight_indices),
          "highlight proteins passed filters out of",
          length(highlighted_proteins),
          "requested"
        ),
        2
      )
    }

    selected_indices <- c(ranked_highlight_indices, ranked_non_highlight_indices)
    selected_indices <- selected_indices[seq_len(min(max_proteins, length(selected_indices)))]

    if (length(ranked_highlight_indices) > max_proteins) {
      heatmap_debug_log(
        paste(
          "More highlighted proteins passed filters than max limit:",
          length(ranked_highlight_indices),
          "-> showing top",
          max_proteins,
          "by ranking"
        ),
        2
      )
    }

    # Create new mask with only selected proteins
    final_mask <- rep(FALSE, length(protein_mask))
    final_mask[selected_indices] <- TRUE
    protein_mask <- final_mask
  }

  heatmap_debug_log(paste("After max protein filtering:", sum(protein_mask), "proteins remain"), 2)

  # ==========================================
  # FINAL RESULTS CREATION
  # ==========================================
  total_selected <- sum(protein_mask)
  heatmap_debug_log(paste("Total features passing all filters:", total_selected), 2)

  # Create results data frame
  selected_indices <- which(protein_mask)
  results <- data.frame(
    `Row Index` = selected_indices,
    Identifier = identifiers_chr[selected_indices],
    baseMean = basemean[selected_indices],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add p-values if available
  if (enable_pvalue_filter && !is.null(pval_col) && nzchar(pval_col)) {
    p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)
    if (length(p_idx) == 1) {
      p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))
      results$p.value <- p_vec[selected_indices]
      results$adj.p.value <- results$p.value  # Simplified for now
    }
  }

  # Add ratio values if available
  if (!is.null(ratio_heatmap_col) &&
      nzchar(ratio_heatmap_col)) {

    r_idx <- which(
      dd$Content == "Abundance Ratio" &
        dd$Column == ratio_heatmap_col
    )

    if (length(r_idx) == 1L) {
      r_vec <-
        suppressWarnings(
          as.numeric(
            dm[[r_idx]]
          )
        )

      results$Abundance.Ratio <-
        r_vec[selected_indices]
    }
  }

  attr(results, "data_pair") <- list(data_mod = dm, data_def = dd)
  if (!is.null(identifier_filter_details)) {
    attr(results, "identifier_filter_summary") <- list(
      active = length(identifier_filter_details$parsed_identifiers) > 0,
      input_entries = length(identifier_filter_details$raw_entries),
      unique_parsed_ids = length(identifier_filter_details$parsed_identifiers),
      matched_rows = identifier_filter_details$matched_rows,
      unique_matched_ids = length(identifier_filter_details$unique_matched_identifiers)
    )
  }
  heatmap_debug_log(paste("Created results data frame with", nrow(results), "proteins"), 2)
  heatmap_debug_log("Statistical analysis with NA filter completed successfully", 2)
  return(results)
}
