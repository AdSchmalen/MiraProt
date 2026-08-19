# R/utils_legacy.R
# ========================================
# LEGACY: Unused utility functions
# ========================================
# These functions were superseded by datawizard_ratios_core.R
# and other module-level implementations.
# Retained for backwards compatibility — safe to delete
# once confirmed no external scripts depend on them.
#
# Moved from R/utils.R during architecture refactoring.
# Still loaded into modEnv at startup.

# in R/utils.R
updateAbundanceSelects <- function(session, inputIds, choices, default = NULL) {
  for (id in inputIds) {
    updateSelectizeInput(
      session,
      id,
      choices  = choices,
      selected = if (is.null(default) || !(default %in% choices))
        choices[1]
      else
        default
    )
  }
}

# ========================================
# Grundlegende Hilfsfunktionen
# ========================================

#' Eindeutige Row Names erstellen
#' @param data data.frame
#' @return data.frame mit eindeutigen row.names
ensure_unique_rownames <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Verwende eine eindeutige Sequenz als row.names
  rownames(data) <- paste0("row_", seq_len(nrow(data)))
  return(data)
}

#' Sichere Row Index Extraktion
#' @param data input data
#' @return integer vector mit Row Index column oder NULL
find_row_index_column <- function(data) {
  if (is.null(data) || ncol(data) == 0) return(NULL)

  # Suche nach Row Index Spalten
  possible_names <- c("Row Index", "Row.Index", "RowIndex", "ID", "Index")

  for (name in possible_names) {
    if (name %in% names(data)) {
      return(which(names(data) == name)[1])
    }
  }

  # Fallback: Erste numerische Spalte die wie ein Index aussieht
  for (i in seq_len(ncol(data))) {
    col_data <- data[[i]]
    if (is.numeric(col_data) && all(col_data == seq_len(nrow(data)), na.rm = TRUE)) {
      return(i)
    }
  }

  return(NULL)
}

#' Filter data based on minimum valid values per group
#' @param data input data.frame
#' @param numerator_columns column indices for numerator
#' @param denominator_columns column indices for denominator
#' @param rowI_col row index column
#' @param min_count minimum valid values required
#' @param valid_logic "In total", "One group", or "Each group"
#' @return list with filtered data and metadata
filterDataForStatistics <- function(data, numerator_columns, denominator_columns,
                                    rowI_col, min_count, valid_logic = "In total") {
  tryCatch({
    if (is.null(data) || nrow(data) == 0) {
      stop("No data provided to filterDataForStatistics")
    }

    if (is.null(rowI_col) || is.na(rowI_col)) {
      data$`Row Index` <- seq_len(nrow(data))
      rowI_col <- ncol(data)
    }

    # Remove completely empty rows
    valid_rows <- rowSums(is.na(data)) != ncol(data)
    data <- data[valid_rows, , drop = FALSE]

    if (nrow(data) == 0) {
      stop("No valid rows after removing empty rows")
    }

    # Create subset including row index
    all_cols <- c(rowI_col, numerator_columns, denominator_columns)
    all_cols <- all_cols[!is.na(all_cols) & all_cols <= ncol(data)]

    df <- data[, all_cols, drop = FALSE]
    names(df)[1] <- "Row Index"
    df <- ensure_unique_rownames(df)

    # Extract column names for numerator and denominator
    num_names <- colnames(data)[numerator_columns]
    denom_names <- colnames(data)[denominator_columns]

    # Apply different filtering logics
    sufficient <- apply(df, 1, function(row) {
      tryCatch({
        num_vals <- suppressWarnings(as.numeric(row[num_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denom_names]))

        num_valid <- sum(!is.na(num_vals) & is.finite(num_vals))
        denom_valid <- sum(!is.na(denom_vals) & is.finite(denom_vals))
        total_valid <- num_valid + denom_valid

        result <- switch(valid_logic,
                         "In total" = total_valid >= min_count,
                         "One group" = (num_valid >= min_count) || (denom_valid >= min_count),
                         "Each group" = (num_valid >= min_count) && (denom_valid >= min_count),
                         total_valid >= min_count  # Default fallback
        )

        return(result)
      }, error = function(e) {
        return(FALSE)
      })
    })

    filtered_df <- df[sufficient, , drop = FALSE]
    filtered_df <- ensure_unique_rownames(filtered_df)

    return(list(
      filtered = filtered_df,
      num_names = num_names,
      denom_names = denom_names,
      indices = sufficient,
      original_row_count = nrow(data),
      filtered_row_count = nrow(filtered_df),
      filter_summary = list(
        min_count = min_count,
        valid_logic = valid_logic,
        removed_rows = sum(!sufficient)
      )
    ))

  }, error = function(e) {
    cat("Error in filterDataForStatistics:", e$message, "\n")

    return(list(
      filtered = data.frame(),
      num_names = character(0),
      denom_names = character(0),
      indices = logical(0),
      original_row_count = if (!is.null(data)) nrow(data) else 0,
      filtered_row_count = 0,
      error = e$message
    ))
  })
}

#' Compute abundance ratio for a single row
#' @param row single data row
#' @param num_cols numerator column names
#' @param denom_cols denominator column names
#' @return numeric abundance ratio or NA
computeAbundanceRatio <- function(row, num_cols, denom_cols) {
  tryCatch({
    num_vals <- suppressWarnings(as.numeric(row[num_cols]))
    denom_vals <- suppressWarnings(as.numeric(row[denom_cols]))

    # Remove NA and infinite values
    num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals)]
    denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals)]

    if (length(num_vals) == 0 || length(denom_vals) == 0) {
      return(NA_real_)
    }

    num_mean <- mean(num_vals)
    denom_mean <- mean(denom_vals)

    if (is.na(num_mean) || is.na(denom_mean) || denom_mean == 0) {
      return(NA_real_)
    }

    ratio <- num_mean / denom_mean

    if (is.finite(ratio)) {
      return(ratio)
    } else {
      return(NA_real_)
    }

  }, error = function(e) {
    return(NA_real_)
  })
}

#' Adjust p-values using various methods
#' @param p_values vector of p-values
#' @param method adjustment method
#' @return vector of adjusted p-values
adjustPvalues <- function(p_values, method) {
  tryCatch({
    valid_p <- !is.na(p_values) & is.finite(p_values)

    if (sum(valid_p) == 0) {
      return(p_values)
    }

    adjusted <- p_values
    adjusted[valid_p] <- switch(method,
                                "Bonferroni" = p.adjust(p_values[valid_p], method = "bonferroni"),
                                "FDR" = p.adjust(p_values[valid_p], method = "fdr"),
                                "Holm" = p.adjust(p_values[valid_p], method = "holm"),
                                "Hochberg" = p.adjust(p_values[valid_p], method = "hochberg"),
                                "Hommel" = p.adjust(p_values[valid_p], method = "hommel"),
                                "Benjamini & Hochberg" = p.adjust(p_values[valid_p], method = "BH"),
                                "Benjamini & Yekutieli" = p.adjust(p_values[valid_p], method = "BY"),
                                p_values[valid_p])

    return(adjusted)
  }, error = function(e) {
    cat("Error in adjustPvalues:", e$message, "\n")
    return(p_values)
  })
}

#' Enhanced error handling wrapper for statistical functions
#' @param func function to execute
#' @param fallback_result result to return on error
#' @param error_message error message prefix
#' @return function result or fallback
safe_statistical_execution <- function(func, fallback_result = NULL, error_message = "Statistical calculation failed") {
  tryCatch({
    result <- func()
    if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
      cat("Function returned empty result\n")
      return(fallback_result)
    }
    return(result)
  }, error = function(e) {
    cat("Error in statistical calculation:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste(error_message, ":", e$message), type = "error", duration = 5)
    }
    return(fallback_result)
  })
}

# ========================================
# VOLLSTÄNDIGE Statistische Funktionen
# ========================================

#' Perform Welch's T-Test analysis
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 3)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performWelchTTest <- function(data, numerator_columns, denominator_columns,
                              adjustment_method, rowI_col, comparison_name,
                              min_count = 3, valid_logic = "Each group",
                              return_format = "complete") {
  tryCatch({
    # Check sample size requirements
    if (length(numerator_columns) < 3 || length(denominator_columns) < 3) {
      if (exists("showNotification", mode = "function")) {
        showNotification("Welch's T-Test: Fewer than three samples selected.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Starting Welch's T-Test with", length(numerator_columns), "numerator and", length(denominator_columns), "denominator columns\n")

    # Filter the data using improved function
    filt <- filterDataForStatistics(data, numerator_columns, denominator_columns,
                                    rowI_col, min_count, valid_logic)

    if (!is.null(filt$error)) {
      stop("Error in data filtering: ", filt$error)
    }

    df <- filt$filtered
    num_names <- filt$num_names
    denom_names <- filt$denom_names

    if (nrow(df) == 0) {
      if (exists("showNotification", mode = "function")) {
        showNotification("No rows with sufficient abundance values found.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Filtered to", nrow(df), "rows for analysis\n")

    # Calculate statistics for each row
    results_list <- list()

    for (i in 1:nrow(df)) {
      row <- df[i, ]
      row_index <- row$`Row Index`

      tryCatch({
        # Extract values
        num_vals <- suppressWarnings(as.numeric(row[num_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denom_names]))

        # Remove NA and infinite values
        num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals)]
        denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals)]

        if (length(num_vals) < 3 || length(denom_vals) < 3) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = NA_real_,
            p_value = NA_real_,
            t_statistic = NA_real_,
            df = NA_real_,
            mean_numerator = NA_real_,
            mean_denominator = NA_real_,
            sd_numerator = NA_real_,
            sd_denominator = NA_real_,
            n_numerator = length(num_vals),
            n_denominator = length(denom_vals),
            log2_fold_change = NA_real_
          )
          next
        }

        # Calculate basic statistics
        mean_num <- mean(num_vals)
        mean_denom <- mean(denom_vals)
        sd_num <- sd(num_vals)
        sd_denom <- sd(denom_vals)

        # Calculate abundance ratio
        abundance_ratio <- if (mean_denom > 0) mean_num / mean_denom else NA_real_

        # Log2 transformation for t-test
        num_vals_log <- log2(num_vals[num_vals > 0])
        denom_vals_log <- log2(denom_vals[denom_vals > 0])

        if (length(num_vals_log) < 3 || length(denom_vals_log) < 3) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = abundance_ratio,
            p_value = NA_real_,
            t_statistic = NA_real_,
            df = NA_real_,
            mean_numerator = mean_num,
            mean_denominator = mean_denom,
            sd_numerator = sd_num,
            sd_denominator = sd_denom,
            n_numerator = length(num_vals),
            n_denominator = length(denom_vals),
            log2_fold_change = if (!is.na(abundance_ratio) && abundance_ratio > 0) log2(abundance_ratio) else NA_real_
          )
          next
        }

        # Perform Welch's T-Test
        test_res <- t.test(x = num_vals_log, y = denom_vals_log, var.equal = FALSE)

        # Calculate log2 fold change
        log2_fc <- mean(num_vals_log) - mean(denom_vals_log)

        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = abundance_ratio,
          p_value = test_res$p.value,
          t_statistic = test_res$statistic,
          df = test_res$parameter,
          mean_numerator = mean_num,
          mean_denominator = mean_denom,
          sd_numerator = sd_num,
          sd_denominator = sd_denom,
          n_numerator = length(num_vals),
          n_denominator = length(denom_vals),
          log2_fold_change = log2_fc,
          confidence_interval_lower = test_res$conf.int[1],
          confidence_interval_upper = test_res$conf.int[2]
        )

      }, error = function(e) {
        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = NA_real_,
          p_value = NA_real_,
          t_statistic = NA_real_,
          df = NA_real_,
          mean_numerator = NA_real_,
          mean_denominator = NA_real_,
          sd_numerator = NA_real_,
          sd_denominator = NA_real_,
          n_numerator = 0,
          n_denominator = 0,
          log2_fold_change = NA_real_
        )
      })
    }

    # Convert results to data.frame
    results_df <- data.frame(
      "Row Index" = sapply(results_list, function(x) x$row_index),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add statistics based on return_format
    if (return_format %in% c("complete", "extended")) {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
      results_df[[paste0(comparison_name, "_Log2_Fold_Change")]] <- sapply(results_list, function(x) x$log2_fold_change)
      results_df[[paste0(comparison_name, "_t_Statistic")]] <- sapply(results_list, function(x) x$t_statistic)
      results_df[[paste0(comparison_name, "_Degrees_Freedom")]] <- sapply(results_list, function(x) x$df)
    }

    if (return_format == "complete") {
      results_df[[paste0(comparison_name, "_Mean_Numerator")]] <- sapply(results_list, function(x) x$mean_numerator)
      results_df[[paste0(comparison_name, "_Mean_Denominator")]] <- sapply(results_list, function(x) x$mean_denominator)
      results_df[[paste0(comparison_name, "_SD_Numerator")]] <- sapply(results_list, function(x) x$sd_numerator)
      results_df[[paste0(comparison_name, "_SD_Denominator")]] <- sapply(results_list, function(x) x$sd_denominator)
      results_df[[paste0(comparison_name, "_N_Numerator")]] <- sapply(results_list, function(x) x$n_numerator)
      results_df[[paste0(comparison_name, "_N_Denominator")]] <- sapply(results_list, function(x) x$n_denominator)

      # Add confidence intervals
      ci_lower <- sapply(results_list, function(x) if(is.null(x$confidence_interval_lower)) NA_real_ else x$confidence_interval_lower)
      ci_upper <- sapply(results_list, function(x) if(is.null(x$confidence_interval_upper)) NA_real_ else x$confidence_interval_upper)
      results_df[[paste0(comparison_name, "_CI_Lower")]] <- ci_lower
      results_df[[paste0(comparison_name, "_CI_Upper")]] <- ci_upper
    }

    if (return_format == "essential") {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
    }

    # Adjust p-values
    p_values <- results_df[[paste0(comparison_name, "_p_Value")]]
    adj_p_values <- adjustPvalues(p_values, adjustment_method)
    results_df[[paste0(comparison_name, "_Adj_p_Value")]] <- adj_p_values

    # Set unique rownames
    results_df <- ensure_unique_rownames(results_df)

    cat("Welch T-Test completed, returning", nrow(results_df), "results with", ncol(results_df)-1, "analysis columns\n")

    return(results_df)

  }, error = function(e) {
    cat("Error in performWelchTTest:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("Welch's T-Test failed:", e$message), type = "error", duration = 5)
    }
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' Perform Limma analysis
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 2)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performLimmaAnalysis <- function(data, numerator_columns, denominator_columns,
                                 adjustment_method, rowI_col, comparison_name,
                                 min_count = 2, valid_logic = "Each group",
                                 return_format = "complete") {
  tryCatch({
    # Check if limma package is available
    if (!requireNamespace("limma", quietly = TRUE)) {
      if (exists("showNotification", mode = "function")) {
        showNotification("Limma package not available. Please install it first.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    if (length(numerator_columns) < 2 || length(denominator_columns) < 2) {
      if (exists("showNotification", mode = "function")) {
        showNotification("Limma: Fewer than two samples selected for numerator or denominator.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Starting Limma analysis with", length(numerator_columns), "numerator and", length(denominator_columns), "denominator columns\n")

    # Remove completely empty rows
    data <- data[rowSums(is.na(data)) != ncol(data), , drop = FALSE]

    if (nrow(data) == 0) {
      stop("No data remaining after removing empty rows")
    }

    # Handle row index
    if (is.null(rowI_col) || is.na(rowI_col)) {
      data$`Row Index` <- seq_len(nrow(data))
      rowI_col <- ncol(data)
    }

    numerator_cols <- data[, numerator_columns, drop = FALSE]
    denominator_cols <- data[, denominator_columns, drop = FALSE]
    rowID <- data[, rowI_col]

    numerator_names <- colnames(numerator_cols)
    denominator_names <- colnames(denominator_cols)

    # Combine data for filtering
    dfD <- cbind(rowID = rowID, numerator_cols, denominator_cols)

    # Apply filtering logic
    sufficient_samples <- apply(dfD, 1, function(row) {
      tryCatch({
        num_vals <- suppressWarnings(as.numeric(row[numerator_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denominator_names]))

        num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals) & num_vals > 0]
        denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals) & denom_vals > 0]

        result <- switch(valid_logic,
                         "In total" = (length(num_vals) + length(denom_vals)) >= min_count,
                         "One group" = (length(num_vals) >= min_count) || (length(denom_vals) >= min_count),
                         "Each group" = (length(num_vals) >= min_count) && (length(denom_vals) >= min_count),
                         (length(num_vals) >= min_count) && (length(denom_vals) >= min_count)  # Default
        )

        return(result)
      }, error = function(e) {
        return(FALSE)
      })
    })

    dfD <- dfD[sufficient_samples, , drop = FALSE]

    if (nrow(dfD) == 0) {
      if (exists("showNotification", mode = "function")) {
        showNotification("No rows with sufficient abundance values found after filtering.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Filtered to", nrow(dfD), "rows for Limma analysis\n")

    # Calculate abundance ratios first (before log transformation)
    abundance_ratios <- apply(dfD, 1, function(row) {
      computeAbundanceRatio(row, numerator_names, denominator_names)
    })

    # Prepare data for limma
    unique_rownames <- make.names(paste0("protein_", dfD$rowID), unique = TRUE)
    rownames(dfD) <- unique_rownames

    # Remove rowID column for analysis
    dfD_analysis <- dfD[, -1, drop = FALSE]

    # Log2 transformation with safety checks
    dfD_analysis[dfD_analysis <= 0] <- NA
    dfD_log <- log2(dfD_analysis)

    # Remove rows with too many NAs
    max_na_allowed <- ncol(dfD_log) * 0.5
    valid_rows <- rowSums(is.na(dfD_log)) <= max_na_allowed
    dfD_log <- dfD_log[valid_rows, , drop = FALSE]

    if (nrow(dfD_log) == 0) {
      stop("No valid rows remaining after log transformation and NA filtering")
    }

    # Create design matrix
    Cond_col <- rep("Condition", length(numerator_columns))
    Ref_col <- rep("Reference", length(denominator_columns))
    cond <- as.factor(c(Cond_col, Ref_col))
    design <- model.matrix(~0 + cond)
    colnames(design) <- levels(cond)

    # Create contrast
    contrast <- limma::makeContrasts(contrasts = "Condition-Reference", levels = design)

    # Fit limma model
    fit1 <- limma::lmFit(dfD_log, design)
    fit2 <- limma::contrasts.fit(fit1, contrast)
    fit3 <- limma::eBayes(fit2)

    # Extract COMPLETE results
    fit4 <- limma::topTable(fit3, sort = "none", n = Inf)

    if (nrow(fit4) == 0) {
      stop("Limma returned no results")
    }

    # Extract original Row Index from rownames
    result_rownames <- gsub("protein_", "", rownames(fit4))
    result_rownames <- gsub("X", "", result_rownames)
    row_indices <- as.numeric(result_rownames)

    # Get corresponding abundance ratios
    original_row_indices <- as.numeric(dfD$rowID[valid_rows])
    matched_abundance_ratios <- abundance_ratios[valid_rows]

    # Create result data.frame
    result <- data.frame(
      "Row Index" = original_row_indices,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add statistics based on return_format
    if (return_format %in% c("complete", "extended")) {
      result[[paste0(comparison_name, "_Abundance_Ratio")]] <- matched_abundance_ratios
      result[[paste0(comparison_name, "_Log2_Fold_Change")]] <- fit4$logFC
      result[[paste0(comparison_name, "_p_Value")]] <- fit4$P.Value
      result[[paste0(comparison_name, "_Average_Expression")]] <- fit4$AveExpr
      result[[paste0(comparison_name, "_t_Statistic")]] <- fit4$t
      result[[paste0(comparison_name, "_B_Statistic")]] <- fit4$B
    }

    if (return_format == "complete") {
      # Add additional Limma-specific statistics
      if ("CI.L" %in% names(fit4)) {
        result[[paste0(comparison_name, "_CI_Lower")]] <- fit4$CI.L
      }
      if ("CI.R" %in% names(fit4)) {
        result[[paste0(comparison_name, "_CI_Upper")]] <- fit4$CI.R
      }

      # Add moderated statistics
      result[[paste0(comparison_name, "_Moderated_t")]] <- fit4$t
      result[[paste0(comparison_name, "_Log_Odds")]] <- fit4$B

      # Add sample size information
      result[[paste0(comparison_name, "_N_Numerator")]] <- length(numerator_columns)
      result[[paste0(comparison_name, "_N_Denominator")]] <- length(denominator_columns)
    }

    if (return_format == "essential") {
      result[[paste0(comparison_name, "_Abundance_Ratio")]] <- matched_abundance_ratios
      result[[paste0(comparison_name, "_p_Value")]] <- fit4$P.Value
    }

    # Apply custom p-value adjustment
    if (adjustment_method != "FDR") {
      adj_p_values <- adjustPvalues(fit4$P.Value, adjustment_method)
    } else {
      adj_p_values <- fit4$adj.P.Val
    }
    result[[paste0(comparison_name, "_Adj_p_Value")]] <- adj_p_values

    # Set unique rownames
    result <- ensure_unique_rownames(result)

    cat("Limma analysis completed, returning", nrow(result), "results with", ncol(result)-1, "analysis columns\n")

    return(result)

  }, error = function(e) {
    cat("Error in performLimmaAnalysis:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("Limma analysis failed:", e$message), type = "error", duration = 5)
    }
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' Perform ANOVA analysis
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 3)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performANOVAAnalysis <- function(data, numerator_columns, denominator_columns,
                                 adjustment_method, rowI_col, comparison_name,
                                 min_count = 3, valid_logic = "Each group",
                                 return_format = "complete") {
  tryCatch({
    if (length(numerator_columns) < 2 || length(denominator_columns) < 2) {
      if (exists("showNotification", mode = "function")) {
        showNotification("ANOVA: Fewer than two samples selected.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Starting ANOVA analysis with", length(numerator_columns), "numerator and", length(denominator_columns), "denominator columns\n")

    # Filter the data
    filt <- filterDataForStatistics(data, numerator_columns, denominator_columns,
                                    rowI_col, min_count, valid_logic)

    if (!is.null(filt$error)) {
      stop("Error in data filtering: ", filt$error)
    }

    df <- filt$filtered
    num_names <- filt$num_names
    denom_names <- filt$denom_names

    if (nrow(df) == 0) {
      if (exists("showNotification", mode = "function")) {
        showNotification("No rows with sufficient abundance values found.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Filtered to", nrow(df), "rows for ANOVA analysis\n")

    # Calculate statistics for each row
    results_list <- list()

    for (i in 1:nrow(df)) {
      row <- df[i, ]
      row_index <- row$`Row Index`

      tryCatch({
        # Extract values
        num_vals <- suppressWarnings(as.numeric(row[num_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denom_names]))

        # Remove NA and infinite values
        num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals)]
        denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals)]

        if (length(num_vals) < 2 || length(denom_vals) < 2) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = NA_real_,
            p_value = NA_real_,
            f_statistic = NA_real_,
            df_between = NA_real_,
            df_within = NA_real_,
            mean_numerator = NA_real_,
            mean_denominator = NA_real_
          )
          next
        }

        # Calculate abundance ratio
        mean_num <- mean(num_vals)
        mean_denom <- mean(denom_vals)
        abundance_ratio <- if (mean_denom > 0) mean_num / mean_denom else NA_real_

        # Prepare data for ANOVA
        values <- c(num_vals, denom_vals)
        groups <- factor(c(rep("Numerator", length(num_vals)), rep("Denominator", length(denom_vals))))

        # Perform ANOVA
        anova_result <- tryCatch({
          aov_fit <- aov(values ~ groups)
          aov_summary <- summary(aov_fit)
          aov_summary
        }, error = function(e) {
          return(NULL)
        })

        if (is.null(anova_result)) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = abundance_ratio,
            p_value = NA_real_,
            f_statistic = NA_real_,
            df_between = NA_real_,
            df_within = NA_real_,
            mean_numerator = mean_num,
            mean_denominator = mean_denom
          )
        } else {
          f_stat <- anova_result[[1]]$`F value`[1]
          p_val <- anova_result[[1]]$`Pr(>F)`[1]
          df_between <- anova_result[[1]]$Df[1]
          df_within <- anova_result[[1]]$Df[2]

          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = abundance_ratio,
            p_value = p_val,
            f_statistic = f_stat,
            df_between = df_between,
            df_within = df_within,
            mean_numerator = mean_num,
            mean_denominator = mean_denom
          )
        }

      }, error = function(e) {
        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = NA_real_,
          p_value = NA_real_,
          f_statistic = NA_real_,
          df_between = NA_real_,
          df_within = NA_real_,
          mean_numerator = NA_real_,
          mean_denominator = NA_real_
        )
      })
    }

    # Convert results to data.frame
    results_df <- data.frame(
      "Row Index" = sapply(results_list, function(x) x$row_index),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add statistics based on return_format
    if (return_format %in% c("complete", "extended")) {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
      results_df[[paste0(comparison_name, "_F_Statistic")]] <- sapply(results_list, function(x) x$f_statistic)
      results_df[[paste0(comparison_name, "_DF_Between")]] <- sapply(results_list, function(x) x$df_between)
      results_df[[paste0(comparison_name, "_DF_Within")]] <- sapply(results_list, function(x) x$df_within)
    }

    if (return_format == "complete") {
      results_df[[paste0(comparison_name, "_Mean_Numerator")]] <- sapply(results_list, function(x) x$mean_numerator)
      results_df[[paste0(comparison_name, "_Mean_Denominator")]] <- sapply(results_list, function(x) x$mean_denominator)
    }

    if (return_format == "essential") {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
    }

    # Adjust p-values
    p_values <- results_df[[paste0(comparison_name, "_p_Value")]]
    adj_p_values <- adjustPvalues(p_values, adjustment_method)
    results_df[[paste0(comparison_name, "_Adj_p_Value")]] <- adj_p_values

    # Set unique rownames
    results_df <- ensure_unique_rownames(results_df)

    cat("ANOVA analysis completed, returning", nrow(results_df), "results with", ncol(results_df)-1, "analysis columns\n")

    return(results_df)

  }, error = function(e) {
    cat("Error in performANOVAAnalysis:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("ANOVA analysis failed:", e$message), type = "error", duration = 5)
    }
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' Perform Mann-Whitney U Test analysis (non-parametric)
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 3)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performMannWhitneyUTest <- function(data, numerator_columns, denominator_columns,
                                    adjustment_method, rowI_col, comparison_name,
                                    min_count = 3, valid_logic = "Each group",
                                    return_format = "complete") {
  tryCatch({
    if (length(numerator_columns) < 3 || length(denominator_columns) < 3) {
      if (exists("showNotification", mode = "function")) {
        showNotification("Mann-Whitney U: Fewer than three samples selected.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Starting Mann-Whitney U Test with", length(numerator_columns), "numerator and", length(denominator_columns), "denominator columns\n")

    # Filter the data
    filt <- filterDataForStatistics(data, numerator_columns, denominator_columns,
                                    rowI_col, min_count, valid_logic)

    if (!is.null(filt$error)) {
      stop("Error in data filtering: ", filt$error)
    }

    df <- filt$filtered
    num_names <- filt$num_names
    denom_names <- filt$denom_names

    if (nrow(df) == 0) {
      if (exists("showNotification", mode = "function")) {
        showNotification("No rows with sufficient abundance values found.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Filtered to", nrow(df), "rows for Mann-Whitney U analysis\n")

    # Calculate statistics for each row
    results_list <- list()

    for (i in 1:nrow(df)) {
      row <- df[i, ]
      row_index <- row$`Row Index`

      tryCatch({
        # Extract values
        num_vals <- suppressWarnings(as.numeric(row[num_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denom_names]))

        # Remove NA and infinite values
        num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals)]
        denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals)]

        if (length(num_vals) < 3 || length(denom_vals) < 3) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = NA_real_,
            p_value = NA_real_,
            w_statistic = NA_real_,
            median_numerator = NA_real_,
            median_denominator = NA_real_,
            mean_numerator = NA_real_,
            mean_denominator = NA_real_
          )
          next
        }

        # Calculate statistics
        mean_num <- mean(num_vals)
        mean_denom <- mean(denom_vals)
        median_num <- median(num_vals)
        median_denom <- median(denom_vals)
        abundance_ratio <- if (mean_denom > 0) mean_num / mean_denom else NA_real_

        # Perform Mann-Whitney U Test (Wilcoxon rank-sum test)
        test_result <- tryCatch({
          wilcox.test(num_vals, denom_vals, exact = FALSE)
        }, error = function(e) {
          return(NULL)
        })

        if (is.null(test_result)) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = abundance_ratio,
            p_value = NA_real_,
            w_statistic = NA_real_,
            median_numerator = median_num,
            median_denominator = median_denom,
            mean_numerator = mean_num,
            mean_denominator = mean_denom
          )
        } else {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = abundance_ratio,
            p_value = test_result$p.value,
            w_statistic = test_result$statistic,
            median_numerator = median_num,
            median_denominator = median_denom,
            mean_numerator = mean_num,
            mean_denominator = mean_denom
          )
        }

      }, error = function(e) {
        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = NA_real_,
          p_value = NA_real_,
          w_statistic = NA_real_,
          median_numerator = NA_real_,
          median_denominator = NA_real_,
          mean_numerator = NA_real_,
          mean_denominator = NA_real_
        )
      })
    }

    # Convert results to data.frame
    results_df <- data.frame(
      "Row Index" = sapply(results_list, function(x) x$row_index),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add statistics based on return_format
    if (return_format %in% c("complete", "extended")) {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
      results_df[[paste0(comparison_name, "_W_Statistic")]] <- sapply(results_list, function(x) x$w_statistic)
      results_df[[paste0(comparison_name, "_Median_Numerator")]] <- sapply(results_list, function(x) x$median_numerator)
      results_df[[paste0(comparison_name, "_Median_Denominator")]] <- sapply(results_list, function(x) x$median_denominator)
    }

    if (return_format == "complete") {
      results_df[[paste0(comparison_name, "_Mean_Numerator")]] <- sapply(results_list, function(x) x$mean_numerator)
      results_df[[paste0(comparison_name, "_Mean_Denominator")]] <- sapply(results_list, function(x) x$mean_denominator)
    }

    if (return_format == "essential") {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
    }

    # Adjust p-values
    p_values <- results_df[[paste0(comparison_name, "_p_Value")]]
    adj_p_values <- adjustPvalues(p_values, adjustment_method)
    results_df[[paste0(comparison_name, "_Adj_p_Value")]] <- adj_p_values

    # Set unique rownames
    results_df <- ensure_unique_rownames(results_df)

    cat("Mann-Whitney U Test completed, returning", nrow(results_df), "results with", ncol(results_df)-1, "analysis columns\n")

    return(results_df)

  }, error = function(e) {
    cat("Error in performMannWhitneyUTest:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("Mann-Whitney U Test failed:", e$message), type = "error", duration = 5)
    }
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' Perform MWT (Modified Welch Test) analysis - placeholder implementation
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 3)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performMWTAnalysis <- function(data, numerator_columns, denominator_columns,
                               adjustment_method, rowI_col, comparison_name,
                               min_count = 3, valid_logic = "Each group",
                               return_format = "complete") {
  # For now, MWT is implemented as modified Welch's T-Test
  # This can be extended with specific MWT algorithms as needed
  cat("MWT analysis: Using modified Welch's T-Test implementation\n")

  return(performWelchTTest(data, numerator_columns, denominator_columns,
                           adjustment_method, rowI_col, comparison_name,
                           min_count, valid_logic, return_format))
}

#' Perform DEqMS analysis - placeholder (requires DEqMS package)
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 2)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @return data.frame with results
performDEqMSAnalysis <- function(data, numerator_columns, denominator_columns,
                                 adjustment_method, rowI_col, comparison_name,
                                 min_count = 2, valid_logic = "Each group",
                                 return_format = "complete") {
  tryCatch({
    # Check if DEqMS package is available
    if (!requireNamespace("DEqMS", quietly = TRUE)) {
      if (exists("showNotification", mode = "function")) {
        showNotification("DEqMS package not available. Falling back to Limma analysis.", type = "warning", duration = 5)
      }
      # Fallback to Limma
      return(performLimmaAnalysis(data, numerator_columns, denominator_columns,
                                  adjustment_method, rowI_col, comparison_name,
                                  min_count, valid_logic, return_format))
    }

    cat("DEqMS analysis: Using Limma + DEqMS pipeline\n")

    # Use Limma as base, then apply DEqMS correction
    # This is a simplified implementation - full DEqMS requires PSM count information
    limma_result <- performLimmaAnalysis(data, numerator_columns, denominator_columns,
                                         adjustment_method, rowI_col, comparison_name,
                                         min_count, valid_logic, return_format)

    if (nrow(limma_result) == 0) {
      return(limma_result)
    }

    # Add DEqMS-specific columns (placeholder - would need PSM counts for full implementation)
    if (return_format == "complete") {
      # Placeholder for DEqMS variance correction
      limma_result[[paste0(comparison_name, "_DEqMS_Variance_Prior")]] <- rep(NA_real_, nrow(limma_result))
      limma_result[[paste0(comparison_name, "_DEqMS_Variance_Post")]] <- rep(NA_real_, nrow(limma_result))
    }

    cat("DEqMS analysis completed (using Limma base with DEqMS placeholders)\n")

    return(limma_result)

  }, error = function(e) {
    cat("Error in performDEqMSAnalysis:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("DEqMS analysis failed, falling back to Limma:", e$message), type = "warning", duration = 5)
    }

    # Fallback to Limma
    return(performLimmaAnalysis(data, numerator_columns, denominator_columns,
                                adjustment_method, rowI_col, comparison_name,
                                min_count, valid_logic, return_format))
  })
}

#' Perform Permutation Test analysis - basic implementation
#' @param data input data.frame
#' @param numerator_columns column indices for numerator group
#' @param denominator_columns column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param rowI_col row index column
#' @param comparison_name name for the comparison
#' @param min_count minimum valid values (default: 3)
#' @param valid_logic validation logic (default: "Each group")
#' @param return_format "essential", "extended", or "complete"
#' @param n_permutations number of permutations (default: 1000)
#' @return data.frame with results
performPermutationTest <- function(data, numerator_columns, denominator_columns,
                                   adjustment_method, rowI_col, comparison_name,
                                   min_count = 3, valid_logic = "Each group",
                                   return_format = "complete", n_permutations = 1000) {
  tryCatch({
    if (length(numerator_columns) < 3 || length(denominator_columns) < 3) {
      if (exists("showNotification", mode = "function")) {
        showNotification("Permutation Test: Fewer than three samples selected.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Starting Permutation Test with", length(numerator_columns), "numerator and", length(denominator_columns), "denominator columns\n")
    cat("Using", n_permutations, "permutations\n")

    # Filter the data
    filt <- filterDataForStatistics(data, numerator_columns, denominator_columns,
                                    rowI_col, min_count, valid_logic)

    if (!is.null(filt$error)) {
      stop("Error in data filtering: ", filt$error)
    }

    df <- filt$filtered
    num_names <- filt$num_names
    denom_names <- filt$denom_names

    if (nrow(df) == 0) {
      if (exists("showNotification", mode = "function")) {
        showNotification("No rows with sufficient abundance values found.", type = "error", duration = 5)
      }
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    cat("Filtered to", nrow(df), "rows for Permutation Test analysis\n")

    # Calculate statistics for each row
    results_list <- list()

    for (i in 1:nrow(df)) {
      row <- df[i, ]
      row_index <- row$`Row Index`

      tryCatch({
        # Extract values
        num_vals <- suppressWarnings(as.numeric(row[num_names]))
        denom_vals <- suppressWarnings(as.numeric(row[denom_names]))

        # Remove NA and infinite values
        num_vals <- num_vals[!is.na(num_vals) & is.finite(num_vals)]
        denom_vals <- denom_vals[!is.na(denom_vals) & is.finite(denom_vals)]

        if (length(num_vals) < 3 || length(denom_vals) < 3) {
          results_list[[i]] <- list(
            row_index = row_index,
            abundance_ratio = NA_real_,
            p_value = NA_real_,
            observed_difference = NA_real_,
            mean_numerator = NA_real_,
            mean_denominator = NA_real_
          )
          next
        }

        # Calculate basic statistics
        mean_num <- mean(num_vals)
        mean_denom <- mean(denom_vals)
        abundance_ratio <- if (mean_denom > 0) mean_num / mean_denom else NA_real_

        # Calculate observed difference (log2 scale)
        if (min(c(num_vals, denom_vals)) > 0) {
          log_num_vals <- log2(num_vals)
          log_denom_vals <- log2(denom_vals)
          observed_diff <- mean(log_num_vals) - mean(log_denom_vals)

          # Perform permutation test
          all_vals <- c(log_num_vals, log_denom_vals)
          n_num <- length(log_num_vals)
          n_total <- length(all_vals)

          permuted_diffs <- replicate(n_permutations, {
            permuted_indices <- sample(n_total, n_num)
            permuted_num <- all_vals[permuted_indices]
            permuted_denom <- all_vals[-permuted_indices]
            mean(permuted_num) - mean(permuted_denom)
          })

          # Calculate p-value (two-tailed)
          p_value <- mean(abs(permuted_diffs) >= abs(observed_diff))

        } else {
          observed_diff <- NA_real_
          p_value <- NA_real_
        }

        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = abundance_ratio,
          p_value = p_value,
          observed_difference = observed_diff,
          mean_numerator = mean_num,
          mean_denominator = mean_denom
        )

      }, error = function(e) {
        results_list[[i]] <- list(
          row_index = row_index,
          abundance_ratio = NA_real_,
          p_value = NA_real_,
          observed_difference = NA_real_,
          mean_numerator = NA_real_,
          mean_denominator = NA_real_
        )
      })
    }

    # Convert results to data.frame
    results_df <- data.frame(
      "Row Index" = sapply(results_list, function(x) x$row_index),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add statistics based on return_format
    if (return_format %in% c("complete", "extended")) {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
      results_df[[paste0(comparison_name, "_Observed_Difference")]] <- sapply(results_list, function(x) x$observed_difference)
    }

    if (return_format == "complete") {
      results_df[[paste0(comparison_name, "_Mean_Numerator")]] <- sapply(results_list, function(x) x$mean_numerator)
      results_df[[paste0(comparison_name, "_Mean_Denominator")]] <- sapply(results_list, function(x) x$mean_denominator)
      results_df[[paste0(comparison_name, "_N_Permutations")]] <- rep(n_permutations, nrow(results_df))
    }

    if (return_format == "essential") {
      results_df[[paste0(comparison_name, "_Abundance_Ratio")]] <- sapply(results_list, function(x) x$abundance_ratio)
      results_df[[paste0(comparison_name, "_p_Value")]] <- sapply(results_list, function(x) x$p_value)
    }

    # Adjust p-values
    p_values <- results_df[[paste0(comparison_name, "_p_Value")]]
    adj_p_values <- adjustPvalues(p_values, adjustment_method)
    results_df[[paste0(comparison_name, "_Adj_p_Value")]] <- adj_p_values

    # Set unique rownames
    results_df <- ensure_unique_rownames(results_df)

    cat("Permutation Test completed, returning", nrow(results_df), "results with", ncol(results_df)-1, "analysis columns\n")

    return(results_df)

  }, error = function(e) {
    cat("Error in performPermutationTest:", e$message, "\n")
    if (exists("showNotification", mode = "function")) {
      showNotification(paste("Permutation Test failed:", e$message), type = "error", duration = 5)
    }
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}





# NOTE: safe_parallel_operation was moved back to R/utils.R
# (still has active caller: impute_random_forest)

# Safe helper function to access module reactive values
safe_module_reactive_get <- function(module_out, reactive_name, default_value = NULL, context = "") {
  tryCatch({
    if (is.null(module_out) || !is.list(module_out)) {
      return(default_value)
    }

    if (reactive_name %in% names(module_out)) {
      reactive_func <- module_out[[reactive_name]]
      if (is.function(reactive_func)) {
        result <- reactive_func()
        return(result)
      }
    }
    return(default_value)
  }, error = function(e) {
    debug_log(paste("Error in safe_module_reactive_get for", context, ":", e$message), 1)
    return(default_value)
  })
}
