# Data Wizard/ratios/datawizard_ratios_core.R

#' Ratios Core Statistical Analysis Submodule
#' Contains all statistical analysis methods for ratio calculations

# ========================================
# COMPLETE STATISTICAL ANALYSIS FUNCTIONS
# ========================================

#' ENHANCED: Perform Welch's T-Test analysis
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @return data.frame with essential columns only
perform_welch_t_test_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                          adjustment_method, row_index_column, comparison_name,
                                          metadata_definition = NULL, log_fn = NULL,
                                          valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "Welch's T-Test",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting Welch's T-Test analysis\n")

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 3 || length(denominator_column_indices) < 3) {
          stop("Welch's T-Test requires at least 3 samples per group")
        }

        # Ensure original Row Index exists
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("Welch T-Test Data Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 3,
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("Data filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        if (DEBUG_LEVEL >= 2) cat("Processing", nrow(analysis_data), "rows\n")

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("Welch's T-Test ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # Improved p-value calculations with better numerical stability
        p_values <- apply(analysis_data, 1, function(row) {
          tryCatch({
            # Extract and validate values
            numerator_values <- suppressWarnings(as.numeric(row[numerator_column_names]))
            denominator_values <- suppressWarnings(as.numeric(row[denominator_column_names]))

            # Remove invalid values (NA, negative, zero, infinite)
            clean_num_values <- numerator_values[!is.na(numerator_values) &
                                                   is.finite(numerator_values) &
                                                   numerator_values > 0]
            clean_denom_values <- denominator_values[!is.na(denominator_values) &
                                                       is.finite(denominator_values) &
                                                       denominator_values > 0]

            if (length(clean_num_values) < 3 || length(clean_denom_values) < 3) {
              return(NA_real_)
            }

            # Log2 transformation with numerical stability check
            num_log2 <- log2(clean_num_values)
            denom_log2 <- log2(clean_denom_values)

            # Check for constant values (variance = 0)
            num_var <- var(num_log2)
            denom_var <- var(denom_log2)

            if (is.na(num_var) || is.na(denom_var) ||
                num_var < .Machine$double.eps || denom_var < .Machine$double.eps) {
              return(NA_real_)
            }

            # Perform Welch's T-Test with additional validation
            test_result <- t.test(x = num_log2, y = denom_log2,
                                  var.equal = FALSE, paired = FALSE)

            p_val <- test_result$p.value

            # Validate p-value
            if (is.na(p_val) || !is.finite(p_val) || p_val < 0 || p_val > 1) {
              return(NA_real_)
            }

            return(p_val)

          }, error = function(e) {
            return(NA_real_)
          })
        })

        # P-value adjustment
        adjusted_p_values <- adjust_p_values_safely(p_values, adjustment_method)

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = abundance_ratios,
          p_values = p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = row_indices,
          method_name = "Welch's T-Test"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          valid_count <- sum(!is.na(p_values))
          log_fn("Welch T-Test Success", paste("Completed:", valid_count, "valid results"), "success")
        }

        if (DEBUG_LEVEL >= 2) cat("✓ Welch's T-Test completed successfully\n")
        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("Welch T-Test Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' ENHANCED: Perform Student's T-Test analysis (equal variances assumed)
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @return data.frame with essential columns only
perform_t_test_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                    adjustment_method, row_index_column, comparison_name,
                                    metadata_definition = NULL, log_fn = NULL,
                                    valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "Student's T-Test",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting Student's T-Test analysis\n")

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 3 || length(denominator_column_indices) < 3) {
          stop("Student's T-Test requires at least 3 samples per group")
        }

        # Ensure Row Index
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("Student's T-Test Data Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 3,
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("Data filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        if (DEBUG_LEVEL >= 2) cat("Processing", nrow(analysis_data), "rows\n")

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("Student's T-Test ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # Student's T-Test calculations (assumes equal variances)
        p_values <- apply(analysis_data, 1, function(row) {
          tryCatch({
            # Extract and validate values
            numerator_values <- suppressWarnings(as.numeric(row[numerator_column_names]))
            denominator_values <- suppressWarnings(as.numeric(row[denominator_column_names]))

            # Remove invalid values (NA, negative, zero, infinite)
            clean_num_values <- numerator_values[!is.na(numerator_values) &
                                                   is.finite(numerator_values) &
                                                   numerator_values > 0]
            clean_denom_values <- denominator_values[!is.na(denominator_values) &
                                                       is.finite(denominator_values) &
                                                       denominator_values > 0]

            if (length(clean_num_values) < 3 || length(clean_denom_values) < 3) {
              return(NA_real_)
            }

            # Log2 transformation with numerical stability check
            num_log2 <- log2(clean_num_values)
            denom_log2 <- log2(clean_denom_values)

            # Check for constant values (variance = 0)
            num_var <- var(num_log2)
            denom_var <- var(denom_log2)

            if (is.na(num_var) || is.na(denom_var) ||
                num_var < .Machine$double.eps || denom_var < .Machine$double.eps) {
              return(NA_real_)
            }

            # Perform Student's T-Test (assumes equal variances)
            test_result <- t.test(x = num_log2, y = denom_log2,
                                  var.equal = TRUE, paired = FALSE)

            p_val <- test_result$p.value

            # Validate p-value
            if (is.na(p_val) || !is.finite(p_val) || p_val < 0 || p_val > 1) {
              return(NA_real_)
            }

            return(p_val)

          }, error = function(e) {
            return(NA_real_)
          })
        })

        # P-value adjustment
        adjusted_p_values <- adjust_p_values_safely(p_values, adjustment_method)

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = abundance_ratios,
          p_values = p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = row_indices,
          method_name = "Student's T-Test"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          valid_count <- sum(!is.na(p_values))
          log_fn("Student's T-Test Success", paste("Completed:", valid_count, "valid results"), "success")
        }

        if (DEBUG_LEVEL >= 2) cat("✓ Student's T-Test completed successfully\n")
        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("Student's T-Test Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' CORRECTED: Perform Moderated Welch Test analysis with proper hyperparameter estimation
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @return data.frame with essential columns only
perform_moderated_welch_test_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                                  adjustment_method, row_index_column, comparison_name,
                                                  metadata_definition = NULL, log_fn = NULL,
                                                  valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "Moderated Welch Test",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting Moderated Welch Test analysis\n")

        # Dependency check
        if (!requireNamespace("pracma", quietly = TRUE)) {
          stop("pracma package required for Moderated Welch Test")
        }

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 2 || length(denominator_column_indices) < 2) {
          stop("Moderated Welch Test requires at least 2 samples per group")
        }

        # Ensure Row Index
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("Moderated Welch Test Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 2,
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("Moderated Welch Test filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("Moderated Welch Test Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("Moderated Welch Test: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        n <- nrow(analysis_data)
        if (DEBUG_LEVEL >= 2) cat("Moderated Welch Test: Processing", n, "rows\n")

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("Moderated Welch Test ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # CORRECTED: Moderated Welch Test calculations with proper statistics
        gene_stats <- data.frame(
          mean_diff = numeric(n),
          pooled_se = numeric(n),
          unequal_se = numeric(n),
          df_pooled = numeric(n),
          df_unequal = numeric(n),
          log_var_pooled = numeric(n),
          log_var_unequal = numeric(n),
          valid = logical(n),
          stringsAsFactors = FALSE
        )

        # First pass: calculate statistics for each gene
        for (i in 1:n) {
          row <- analysis_data[i, ]
          num_values <- suppressWarnings(as.numeric(row[numerator_column_names]))
          denom_values <- suppressWarnings(as.numeric(row[denominator_column_names]))

          # Clean values
          clean_num_values <- num_values[!is.na(num_values) & is.finite(num_values) & num_values > 0]
          clean_denom_values <- denom_values[!is.na(denom_values) & is.finite(denom_values) & denom_values > 0]

          if (length(clean_num_values) < 2 || length(clean_denom_values) < 2) {
            gene_stats$valid[i] <- FALSE
            next
          }

          # Log2 transformation
          num_log2 <- log2(clean_num_values)
          denom_log2 <- log2(clean_denom_values)

          # Calculate basic statistics
          n1 <- length(num_log2)
          n2 <- length(denom_log2)
          mean1 <- mean(num_log2)
          mean2 <- mean(denom_log2)
          var1 <- var(num_log2)
          var2 <- var(denom_log2)

          if (is.na(var1) || is.na(var2) || var1 <= 0 || var2 <= 0) {
            gene_stats$valid[i] <- FALSE
            next
          }

          # Calculate statistics for moderation
          mean_diff <- mean1 - mean2

          # Pooled variance approach (for empirical Bayes)
          df_pooled <- n1 + n2 - 2
          pooled_var <- ((n1 - 1) * var1 + (n2 - 1) * var2) / df_pooled
          pooled_se <- sqrt(pooled_var * (1/n1 + 1/n2))

          # Unequal variance approach (Welch)
          unequal_se <- sqrt(var1/n1 + var2/n2)
          df_unequal <- (var1/n1 + var2/n2)^2 / ((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))

          # Store results
          gene_stats$mean_diff[i] <- mean_diff
          gene_stats$pooled_se[i] <- pooled_se
          gene_stats$unequal_se[i] <- unequal_se
          gene_stats$df_pooled[i] <- df_pooled
          gene_stats$df_unequal[i] <- df_unequal
          gene_stats$log_var_pooled[i] <- log(pooled_var)
          gene_stats$log_var_unequal[i] <- log(unequal_se^2)
          gene_stats$valid[i] <- TRUE
        }

        valid_genes <- which(gene_stats$valid)
        if (length(valid_genes) == 0) {
          stop("No valid genes for moderated analysis")
        }

        # CORRECTED: Empirical Bayes variance estimation
        valid_log_vars <- gene_stats$log_var_pooled[valid_genes]
        valid_dfs <- gene_stats$df_pooled[valid_genes]

        if (length(valid_log_vars) < 3) {
          if (DEBUG_LEVEL >= 1) cat("Warning: Too few genes for robust moderation, using unmoderated results\n")
          # Fall back to unmoderated Welch test
          s02 <- NA
          d0 <- NA
        } else {
          # Estimate hyperparameters using method of moments
          hyper_params <- estimate_hyperparameters_corrected(valid_log_vars, mean(valid_dfs))
          s02 <- hyper_params$s02
          d0 <- hyper_params$d0
        }

        # Calculate final p-values
        final_p_values <- numeric(n)

        for (i in 1:n) {
          if (!gene_stats$valid[i]) {
            final_p_values[i] <- NA_real_
            next
          }

          if (is.na(s02) || is.na(d0)) {
            # Use unmoderated Welch test
            t_stat <- gene_stats$mean_diff[i] / gene_stats$unequal_se[i]
            df <- gene_stats$df_unequal[i]
          } else {
            # Use moderated statistics
            moderated_var <- (d0 * s02 + gene_stats$df_pooled[i] * exp(gene_stats$log_var_pooled[i])) /
              (d0 + gene_stats$df_pooled[i])
            moderated_se <- sqrt(moderated_var * (1/length(numerator_column_names) + 1/length(denominator_column_names)))

            t_stat <- gene_stats$mean_diff[i] / moderated_se
            df <- d0 + gene_stats$df_pooled[i]
          }

          if (is.finite(t_stat) && is.finite(df) && df > 0) {
            p_value <- 2 * pt(-abs(t_stat), df = df)
            final_p_values[i] <- if (is.finite(p_value) && p_value >= 0 && p_value <= 1) p_value else NA_real_
          } else {
            final_p_values[i] <- NA_real_
          }
        }

        # P-value adjustment
        adjusted_p_values <- adjust_p_values_safely(final_p_values, adjustment_method)

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = abundance_ratios,
          p_values = final_p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = row_indices,
          method_name = "Moderated Welch Test"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          valid_count <- sum(!is.na(final_p_values))
          log_fn("Moderated Welch Test Success", paste("Completed:", valid_count, "valid results"), "success")
        }

        if (DEBUG_LEVEL >= 2) cat("✓ Moderated Welch Test completed successfully\n")
        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("Moderated Welch Test Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' ENHANCED: Perform Limma analysis
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @return data.frame with essential columns only
perform_limma_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                   adjustment_method, row_index_column, comparison_name,
                                   metadata_definition = NULL, log_fn = NULL,
                                   valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "Limma",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting LIMMA analysis\n")

        # Dependency check
        if (!requireNamespace("limma", quietly = TRUE)) {
          stop("limma package required for LIMMA analysis")
        }

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 2 || length(denominator_column_indices) < 2) {
          stop("LIMMA requires at least 2 samples per group")
        }

        # Ensure Row Index
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("LIMMA Data Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 2,
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("LIMMA data filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("LIMMA Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("LIMMA: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        if (DEBUG_LEVEL >= 2) {
          cat("LIMMA: Processing", nrow(analysis_data), "rows\n")
          cat("Row Index range:", min(row_indices), "to", max(row_indices), "\n")
        }

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("LIMMA ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # Create robust protein mapping
        protein_mapping <- data.frame(
          original_position = seq_len(nrow(analysis_data)),
          row_index = row_indices,
          protein_id = paste0("protein_", row_indices),
          stringsAsFactors = FALSE
        )

        # Extract expression data
        expression_columns <- c(numerator_column_names, denominator_column_names)
        expression_data <- analysis_data[, expression_columns, drop = FALSE]

        # Set rownames with protein identifiers
        rownames(expression_data) <- protein_mapping$protein_id

        # Improved log2 transformation with robust pseudocount
        min_positive_value <- min(expression_data[expression_data > 0], na.rm = TRUE)
        pseudocount <- max(.Machine$double.eps, min_positive_value * 0.01)

        expression_data[expression_data <= 0 | is.na(expression_data)] <- pseudocount
        log2_expression <- log2(expression_data)

        # Remove rows with too many NAs or constant values
        max_na_threshold <- ncol(log2_expression) * 0.5
        valid_protein_indices <- rowSums(is.na(log2_expression)) <= max_na_threshold

        # Also remove rows with no variance
        row_variances <- apply(log2_expression, 1, function(x) var(x, na.rm = TRUE))
        valid_protein_indices <- valid_protein_indices &
          !is.na(row_variances) &
          row_variances > .Machine$double.eps

        log2_expression_clean <- log2_expression[valid_protein_indices, , drop = FALSE]

        # Update mapping for kept rows
        kept_mapping <- protein_mapping[valid_protein_indices, ]
        kept_abundance_ratios <- abundance_ratios[valid_protein_indices]

        if (DEBUG_LEVEL >= 2) {
          cat("LIMMA: Kept", nrow(log2_expression_clean), "rows after filtering\n")
          if (nrow(kept_mapping) > 0) {
            cat("Kept Row Index range:", min(kept_mapping$row_index), "to", max(kept_mapping$row_index), "\n")
          }
        }

        if (nrow(log2_expression_clean) == 0) {
          if (!is.null(log_fn)) {
            log_fn("LIMMA Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("LIMMA: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        # Create design matrix
        condition_labels <- rep("Condition", length(numerator_column_names))
        reference_labels <- rep("Reference", length(denominator_column_names))
        group_factor <- as.factor(c(condition_labels, reference_labels))
        design_matrix <- model.matrix(~0 + group_factor)
        colnames(design_matrix) <- levels(group_factor)

        # Create contrast
        contrast_matrix <- limma::makeContrasts(contrasts = "Condition-Reference", levels = design_matrix)

        # LIMMA fitting
        if (DEBUG_LEVEL >= 2) cat("Running LIMMA model fitting...\n")
        fit1 <- limma::lmFit(log2_expression_clean, design_matrix)
        fit2 <- limma::contrasts.fit(fit1, contrast_matrix)
        fit3 <- limma::eBayes(fit2)

        # Extract results - maintain order
        limma_results <- limma::topTable(fit3, sort = "none", n = Inf)

        if (nrow(limma_results) == 0) {
          stop("LIMMA returned no results")
        }

        if (DEBUG_LEVEL >= 2) cat("LIMMA returned", nrow(limma_results), "results\n")

        # Robust Row Index extraction
        limma_rownames <- rownames(limma_results)
        result_row_indices <- numeric(nrow(limma_results))
        result_abundance_ratios <- numeric(nrow(limma_results))

        for (i in seq_len(nrow(limma_results))) {
          limma_rowname <- limma_rownames[i]

          # Find corresponding Row Index in mapping
          mapping_match <- which(kept_mapping$protein_id == limma_rowname)

          if (length(mapping_match) == 1) {
            result_row_indices[i] <- kept_mapping$row_index[mapping_match]
            result_abundance_ratios[i] <- kept_abundance_ratios[mapping_match]
          } else {
            # Fallback: extract from protein ID
            extracted_id <- as.numeric(gsub("protein_", "", limma_rowname))
            if (!is.na(extracted_id)) {
              result_row_indices[i] <- extracted_id
              # Find abundance ratio
              original_pos <- which(kept_mapping$row_index == extracted_id)
              if (length(original_pos) == 1) {
                result_abundance_ratios[i] <- kept_abundance_ratios[original_pos]
              } else {
                result_abundance_ratios[i] <- NA_real_
              }
            } else {
              result_row_indices[i] <- NA_real_
              result_abundance_ratios[i] <- NA_real_
            }
          }
        }

        # Validation of Row Index extraction
        valid_indices <- !is.na(result_row_indices)
        if (sum(valid_indices) == 0) {
          stop("Failed to extract any valid Row Index from LIMMA results")
        }

        if (sum(valid_indices) < nrow(limma_results) && DEBUG_LEVEL >= 1) {
          cat("Warning: Could only extract", sum(valid_indices), "out of", nrow(limma_results), "Row Index values\n")
        }

        if (DEBUG_LEVEL >= 2) cat("Successfully extracted", sum(valid_indices), "Row Index values\n")

        # Get p-values and adjust
        raw_p_values <- limma_results$P.Value
        adjusted_p_values <- adjust_p_values_safely(raw_p_values, adjustment_method)

        # Filter to valid results
        if (sum(valid_indices) < length(result_row_indices)) {
          result_row_indices <- result_row_indices[valid_indices]
          result_abundance_ratios <- result_abundance_ratios[valid_indices]
          raw_p_values <- raw_p_values[valid_indices]
          adjusted_p_values <- adjusted_p_values[valid_indices]
        }

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = result_abundance_ratios,
          p_values = raw_p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = result_row_indices,
          method_name = "Limma"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          log_fn("LIMMA Success", paste("Completed:", nrow(result), "results"), "success")
        }

        if (DEBUG_LEVEL >= 2) {
          cat("✓ LIMMA completed successfully\n")
          cat("Final result:", nrow(result), "rows, Row Index range:",
              min(result$`Row Index`), "to", max(result$`Row Index`), "\n")
        }

        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("LIMMA Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' ENHANCED: Perform DEqMS analysis with PSM support
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @param metadata_definition metadata for finding PSM columns
#' @return data.frame with essential columns only
perform_deqms_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                   adjustment_method, row_index_column, comparison_name,
                                   metadata_definition = NULL, log_fn = NULL,
                                   valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "DEqMS",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting DEqMS analysis\n")

        # Dependencies check
        if (!requireNamespace("DEqMS", quietly = TRUE)) {
          stop("DEqMS package required")
        }

        if (!requireNamespace("limma", quietly = TRUE)) {
          stop("limma package required for DEqMS")
        }

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 2 || length(denominator_column_indices) < 2) {
          stop("DEqMS requires at least 2 samples per group")
        }

        # Early PSM data check
        psm_column_indices <- find_psm_columns(metadata_definition)

        if (is.null(psm_column_indices) || length(psm_column_indices) == 0) {
          if (DEBUG_LEVEL >= 1) cat("DEqMS analysis aborted: No PSM data found\n")

          if (!is.null(log_fn)) {
            log_fn("DEqMS PSM Check", "No PSM data found, analysis aborted", "warning")
          }

          stop("DEqMS requires PSM data but none was found")
        }

        if (DEBUG_LEVEL >= 2) cat("PSM data found - proceeding with DEqMS\n")

        # Ensure Row Index
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("DEqMS Data Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 2,
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("DEqMS data filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("DEqMS Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("DEqMS: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        if (DEBUG_LEVEL >= 2) cat("DEqMS: Processing", nrow(analysis_data), "rows\n")

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("DEqMS ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # PSM data extraction with improved robustness
        if (DEBUG_LEVEL >= 2) cat("Extracting PSM data from", length(psm_column_indices), "PSM columns\n")

        analysis_row_indices <- analysis_data$`Row Index`
        original_row_positions <- match(analysis_row_indices, data$`Row Index`)

        if (any(is.na(original_row_positions))) {
          stop("Could not match all analysis rows to original data for PSM extraction")
        }

        psm_data <- data[original_row_positions, psm_column_indices, drop = FALSE]

        # Calculate PSM counts per row with improved handling
        psm_counts <- apply(psm_data, 1, function(row) {
          valid_psm_values <- suppressWarnings(as.numeric(row))
          valid_psm_values <- valid_psm_values[!is.na(valid_psm_values) &
                                                 is.finite(valid_psm_values) &
                                                 valid_psm_values >= 1]
          if (length(valid_psm_values) == 0) {
            return(1)  # Default minimum PSM count
          } else {
            return(max(1, round(mean(valid_psm_values))))
          }
        })

        if (DEBUG_LEVEL >= 2) {
          cat("PSM counts summary: min =", min(psm_counts), ", max =", max(psm_counts),
              ", median =", median(psm_counts), "\n")
        }

        # Prepare data for LIMMA/DEqMS (similar to robust LIMMA)
        protein_mapping <- data.frame(
          original_position = seq_len(nrow(analysis_data)),
          row_index = row_indices,
          protein_id = paste0("protein_", row_indices),
          stringsAsFactors = FALSE
        )

        # Extract expression data
        expression_columns <- c(numerator_column_names, denominator_column_names)
        expression_data <- analysis_data[, expression_columns, drop = FALSE]
        rownames(expression_data) <- protein_mapping$protein_id

        # Log2 transformation with robust pseudocount
        min_positive_value <- min(expression_data[expression_data > 0], na.rm = TRUE)
        pseudocount <- max(.Machine$double.eps, min_positive_value * 0.01)

        expression_data[expression_data <= 0 | is.na(expression_data)] <- pseudocount
        log2_expression <- log2(expression_data)

        # Remove rows with too many NAs or insufficient variance
        max_na_threshold <- ncol(log2_expression) * 0.5
        valid_protein_indices <- rowSums(is.na(log2_expression)) <= max_na_threshold

        # Also check for sufficient variance
        row_variances <- apply(log2_expression, 1, function(x) var(x, na.rm = TRUE))
        valid_protein_indices <- valid_protein_indices &
          !is.na(row_variances) &
          row_variances > .Machine$double.eps

        log2_expression_clean <- log2_expression[valid_protein_indices, , drop = FALSE]

        # Update corresponding data
        kept_mapping <- protein_mapping[valid_protein_indices, ]
        kept_abundance_ratios <- abundance_ratios[valid_protein_indices]
        kept_psm_counts <- psm_counts[valid_protein_indices]

        if (DEBUG_LEVEL >= 2) cat("DEqMS: Kept", nrow(log2_expression_clean), "rows after filtering\n")

        # if (nrow(log2_expression_clean) == 0) {
        #   stop("No rows remaining after DEqMS filtering")
        # }

        if (nrow(log2_expression_clean) == 0) {
          if (!is.null(log_fn)) {
            log_fn("DEqMS Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("DEqMS: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        # Design matrix
        condition_labels <- rep("Condition", length(numerator_column_names))
        reference_labels <- rep("Reference", length(denominator_column_names))
        group_factor <- as.factor(c(condition_labels, reference_labels))
        design_matrix <- model.matrix(~0 + group_factor)
        colnames(design_matrix) <- levels(group_factor)

        # Contrast
        contrast_matrix <- limma::makeContrasts(contrasts = "Condition-Reference", levels = design_matrix)

        # LIMMA fitting
        if (DEBUG_LEVEL >= 2) cat("Running LIMMA model for DEqMS...\n")
        fit1 <- limma::lmFit(log2_expression_clean, design_matrix)
        fit2 <- limma::contrasts.fit(fit1, contrast_matrix)
        fit3 <- limma::eBayes(fit2)

        # Add PSM counts and apply DEqMS
        fit3$count <- kept_psm_counts

        if (DEBUG_LEVEL >= 2) cat("Applying DEqMS variance adjustment...\n")
        deqms_fit <- DEqMS::spectraCounteBayes(fit3)

        # Extract DEqMS results
        deqms_results <- DEqMS::outputResult(deqms_fit, coef_col = 1)

        if (nrow(deqms_results) == 0) {
          stop("DEqMS returned no results")
        }

        # Robust Row Index extraction (same as LIMMA)
        deqms_gene_names <- deqms_results$gene
        result_row_indices <- numeric(nrow(deqms_results))
        result_abundance_ratios <- numeric(nrow(deqms_results))

        for (i in seq_len(nrow(deqms_results))) {
          gene_name <- deqms_gene_names[i]

          # Find corresponding Row Index in mapping
          mapping_match <- which(kept_mapping$protein_id == gene_name)

          if (length(mapping_match) == 1) {
            result_row_indices[i] <- kept_mapping$row_index[mapping_match]
            result_abundance_ratios[i] <- kept_abundance_ratios[mapping_match]
          } else {
            # Fallback: extract from gene name
            extracted_id <- as.numeric(gsub("protein_", "", gene_name))
            if (!is.na(extracted_id) && extracted_id %in% kept_mapping$row_index) {
              result_row_indices[i] <- extracted_id
              original_pos <- which(kept_mapping$row_index == extracted_id)
              if (length(original_pos) == 1) {
                result_abundance_ratios[i] <- kept_abundance_ratios[original_pos]
              } else {
                result_abundance_ratios[i] <- NA_real_
              }
            } else {
              result_row_indices[i] <- NA_real_
              result_abundance_ratios[i] <- NA_real_
            }
          }
        }

        # Validation
        valid_indices <- !is.na(result_row_indices)
        if (sum(valid_indices) == 0) {
          stop("Failed to extract any valid Row Index from DEqMS results")
        }

        if (DEBUG_LEVEL >= 2) cat("DEqMS: Successfully extracted", sum(valid_indices), "Row Index values\n")

        # Get p-values - use DEqMS adjusted p-values
        raw_p_values <- deqms_results$sca.P.Value
        adjusted_p_values <- adjust_p_values_safely(raw_p_values, adjustment_method)

        # Filter to valid indices if necessary
        if (sum(valid_indices) < length(result_row_indices)) {
          result_row_indices <- result_row_indices[valid_indices]
          result_abundance_ratios <- result_abundance_ratios[valid_indices]
          raw_p_values <- raw_p_values[valid_indices]
          adjusted_p_values <- adjusted_p_values[valid_indices]
        }

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = result_abundance_ratios,
          p_values = raw_p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = result_row_indices,
          method_name = "DEqMS"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          log_fn("DEqMS Success", paste("Completed with PSM data:", nrow(result), "results"), "success")
        }

        if (DEBUG_LEVEL >= 2) cat("✓ DEqMS completed successfully\n")
        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("DEqMS Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' ENHANCED: Perform Mann-Whitney U Test (Wilcoxon) analysis
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator group
#' @param denominator_column_indices column indices for denominator group
#' @param adjustment_method p-value adjustment method
#' @param row_index_column row index column
#' @param comparison_name name for this comparison
#' @return data.frame with essential columns only
perform_mann_whitney_analysis <- function(data, numerator_column_indices, denominator_column_indices,
                                          adjustment_method, row_index_column, comparison_name,
                                          metadata_definition = NULL, log_fn = NULL,
                                          valid_count = NULL, valid_logic = NULL, content_type = NULL) {

  original_row_indices <- if (!is.null(data) && "Row Index" %in% names(data)) data$`Row Index` else integer(0)

  return(execute_statistical_method_safely(
    method_name = "Mann-Whitney U Test",
    statistical_function = function() {
      tryCatch({
        if (DEBUG_LEVEL >= 2) cat("Starting Mann-Whitney U Test analysis\n")

        if (is.null(data) || nrow(data) == 0) {
          stop("No input data provided")
        }

        if (length(numerator_column_indices) < 3 || length(denominator_column_indices) < 3) {
          stop("Mann-Whitney U Test requires at least 3 samples per group")
        }

        # Ensure Row Index
        data <- ensure_original_row_index(data)

        # Data retransformation
        all_column_indices <- c(numerator_column_indices, denominator_column_indices)
        retransform_result <- check_and_retransform_data(data, all_column_indices, metadata_definition, log_fn = log_fn)
        working_data <- retransform_result$data

        if (!"Row Index" %in% names(working_data)) {
          working_data$`Row Index` <- data$`Row Index`
        }

        if (retransform_result$was_retransformed && !is.null(log_fn) && DEBUG_LEVEL >= 2) {
          log_fn("Mann-Whitney U Test Retransformation", retransform_result$transformation_info, "info")
        }

        # Data filtering
        row_index_column <- which(names(working_data) == "Row Index")[1]

        filter_result <- filter_data_for_analysis_fixed(
          working_data,
          numerator_column_indices,
          denominator_column_indices,
          row_index_column,
          minimum_count = 3,  # Unverändert für Limma
          valid_count = valid_count,  # NEU: aus ratio config
          valid_logic = valid_logic,  # NEU: aus ratio config
          content_type = content_type,  # NEU: aus ratio config
          metadata_definition = metadata_definition
        )

        if (!is.null(filter_result$error)) {
          stop("Mann-Whitney U Test filtering failed: ", filter_result$error)
        }

        analysis_data <- filter_result$filtered_data
        numerator_column_names <- filter_result$numerator_column_names
        denominator_column_names <- filter_result$denominator_column_names

        if (nrow(analysis_data) == 0) {
          if (!is.null(log_fn)) {
            log_fn("Mann-Whitney U Test Filtering", "No rows passed filtering criteria", "warning")
          }
          if (exists("showNotification")) {
            showNotification("Mann-Whitney U Test: No proteins passed filtering criteria. Consider relaxing Valid Count requirements.",
                             type = "warning", duration = 6)
          }
          # Return empty result instead of throwing error
          return(data.frame("Row Index" = integer(), check.names = FALSE))
        }

        row_indices <- analysis_data$`Row Index`
        if (DEBUG_LEVEL >= 2) cat("Mann-Whitney U Test: Processing", nrow(analysis_data), "rows\n")

        # Calculate abundance ratios
        abundance_ratios <- apply(analysis_data, 1, function(row) {
          compute_abundance_ratio_for_row(row, numerator_column_names, denominator_column_names)
        })

        if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
          preview_n <- min(3, nrow(analysis_data))
          preview_rows <- analysis_data[seq_len(preview_n), , drop = FALSE]
          preview_txt <- vapply(seq_len(preview_n), function(i) {
            nr <- suppressWarnings(as.numeric(preview_rows[i, numerator_column_names, drop = TRUE]))
            dr <- suppressWarnings(as.numeric(preview_rows[i, denominator_column_names, drop = TRUE]))
            paste0(
              "RowIndex=", preview_rows$`Row Index`[i],
              " | Num=", paste(signif(nr, 6), collapse = ","),
              " | Den=", paste(signif(dr, 6), collapse = ","),
              " | Ratio=", signif(abundance_ratios[i], 6)
            )
          }, character(1))
          log_fn(paste0("Mann-Whitney U Test ratio preview | ", paste(preview_txt, collapse = " || ")), 2)
        }

        # Improved Mann-Whitney U Test calculations
        p_values <- apply(analysis_data, 1, function(row) {
          tryCatch({
            numerator_values <- suppressWarnings(as.numeric(row[numerator_column_names]))
            denominator_values <- suppressWarnings(as.numeric(row[denominator_column_names]))

            # Clean values
            clean_num_values <- numerator_values[!is.na(numerator_values) &
                                                   is.finite(numerator_values) &
                                                   numerator_values > 0]
            clean_denom_values <- denominator_values[!is.na(denominator_values) &
                                                       is.finite(denominator_values) &
                                                       denominator_values > 0]

            if (length(clean_num_values) < 3 || length(clean_denom_values) < 3) {
              return(NA_real_)
            }

            # Log2 transformation for consistency with other methods
            num_log2 <- log2(clean_num_values)
            denom_log2 <- log2(clean_denom_values)

            # Check for ties (constant values)
            combined_values <- c(num_log2, denom_log2)
            if (length(unique(combined_values)) <= 1) {
              return(NA_real_)
            }

            # Perform Mann-Whitney U test (Wilcoxon rank-sum test)
            # Use exact = FALSE for large samples to avoid warnings
            use_exact <- (length(num_log2) * length(denom_log2)) < 50

            test_result <- wilcox.test(num_log2, denom_log2,
                                       exact = use_exact,
                                       correct = TRUE)

            p_value <- test_result$p.value

            if (is.na(p_value) || !is.finite(p_value) || p_value < 0 || p_value > 1) {
              return(NA_real_)
            }

            return(p_value)

          }, error = function(e) {
            return(NA_real_)
          })
        })

        # P-value adjustment
        adjusted_p_values <- adjust_p_values_safely(p_values, adjustment_method)

        # Create result
        result <- create_robust_ratio_result(
          comparison_name = comparison_name,
          abundance_ratios = abundance_ratios,
          p_values = p_values,
          adjusted_p_values = adjusted_p_values,
          row_indices = row_indices,
          method_name = "Mann-Whitney U Test"
        )

        if (!is.null(log_fn) && DEBUG_LEVEL >= 1) {
          valid_count <- sum(!is.na(p_values))
          log_fn("Mann-Whitney U Test Success", paste("Completed:", valid_count, "valid results"), "success")
        }

        if (DEBUG_LEVEL >= 2) cat("✓ Mann-Whitney U Test completed successfully\n")
        return(result)

      }, error = function(e) {
        if (!is.null(log_fn)) {
          log_fn("Mann-Whitney U Test Error", e$message, "error")
        }
        stop(e$message)
      })
    },
    original_row_indices = original_row_indices
  ))
}

#' CORRECTED: Empirical Bayes hyperparameter estimation
#' @param log_variances vector of log variances
#' @param mean_df mean degrees of freedom
#' @param log_fn optional logging function
#' @return list with s02 and d0 parameters
estimate_hyperparameters_corrected <- function(log_variances, mean_df, log_fn = NULL) {
  tryCatch({
    # Remove any infinite or NA values
    valid_log_vars <- log_variances[is.finite(log_variances)]

    if (length(valid_log_vars) < 3) {
      if (DEBUG_LEVEL >= 1) cat("Warning: Insufficient data for hyperparameter estimation\n")
      return(list(s02 = NA, d0 = NA))
    }

    # Method of moments estimation for empirical Bayes
    # Based on the assumption that log(variance) follows a shifted gamma distribution

    # Calculate sample moments
    mean_log_var <- mean(valid_log_vars)
    var_log_var <- var(valid_log_vars)

    if (is.na(var_log_var) || var_log_var <= 0) {
      if (DEBUG_LEVEL >= 1) cat("Warning: Invalid variance of log variances\n")
      return(list(s02 = NA, d0 = NA))
    }

    # For gamma distribution with shape=a and rate=b:
    # E[log(X)] ≈ digamma(a) - log(b)
    # Var[log(X)] ≈ trigamma(a)

    # Check if pracma package is available for digamma and trigamma
    if (!requireNamespace("pracma", quietly = TRUE)) {
      if (DEBUG_LEVEL >= 1) cat("Warning: pracma package not available, using simplified estimation\n")

      # Simplified estimation without trigamma
      # Use method of moments for inverse gamma

      # Estimate d0 using the relationship: Var[log(s^2)] = trigamma(d0/2)
      # Approximate trigamma(x) ≈ 1/x for large x
      d0_approx <- 2 / var_log_var
      d0_approx <- max(0.1, min(100, d0_approx))  # Reasonable bounds

      # Estimate s02
      # E[log(s^2)] = E[log((d0*s02)/chi^2_d0)] = log(d0*s02) - E[log(chi^2_d0)]
      # E[log(chi^2_d0)] ≈ digamma(d0/2) + log(2)
      # Approximate digamma(x) ≈ log(x) - 1/(2x) for x > 1
      if (d0_approx > 2) {
        expected_log_chi2 <- log(d0_approx) - 1/d0_approx + log(2)
      } else {
        expected_log_chi2 <- log(2)  # Rough approximation
      }

      log_s02_est <- mean_log_var + expected_log_chi2 - log(d0_approx)
      s02_est <- exp(log_s02_est)

      return(list(s02 = s02_est, d0 = d0_approx))
    }

    # Use trigamma to estimate d0
    # Solve: var_log_var = trigamma(d0/2)

    # Define objective function
    objective_func <- function(d0_half) {
      if (d0_half <= 0) return(Inf)
      tryCatch({
        trigamma_val <- pracma::psi(1, d0_half)  # trigamma function
        return((trigamma_val - var_log_var)^2)
      }, error = function(e) {
        return(Inf)
      })
    }

    # Optimize to find d0/2
    opt_result <- tryCatch({
      optimize(objective_func, interval = c(0.1, 50), tol = 1e-6)
    }, error = function(e) {
      if (DEBUG_LEVEL >= 1) cat("Warning: Optimization failed, using fallback\n")
      return(list(minimum = 1))
    })

    d0_est <- 2 * opt_result$minimum
    d0_est <- max(0.1, min(100, d0_est))  # Apply reasonable bounds

    # Estimate s02 using the mean
    # E[log(variance)] = digamma(mean_df/2) - log(mean_df/2) + log(s02) - digamma(d0/2) + log(d0/2)

    tryCatch({
      digamma_mean_df <- pracma::psi(0, mean_df/2)
      digamma_d0 <- pracma::psi(0, d0_est/2)

      log_s02_est <- mean_log_var - digamma_mean_df + log(mean_df/2) + digamma_d0 - log(d0_est/2)
      s02_est <- exp(log_s02_est)

      # Apply reasonable bounds
      s02_est <- max(1e-10, min(1e10, s02_est))

    }, error = function(e) {
      if (DEBUG_LEVEL >= 1) cat("Warning: s02 estimation failed, using sample variance\n")
      s02_est <- exp(mean_log_var)
    })

    if (DEBUG_LEVEL >= 2) {
      cat("Hyperparameters estimated: s02 =", signif(s02_est, 4), "d0 =", signif(d0_est, 4), "\n")
    }

    return(list(s02 = s02_est, d0 = d0_est))

  }, error = function(e) {
    if (DEBUG_LEVEL >= 1) cat("Error in hyperparameter estimation:", e$message, "\n")
    return(list(s02 = NA, d0 = NA))
  })
}

#' Ratios Core Server Submodule
#' @param id namespace id
#' @param utils_functions list of utility functions from utils submodule
#' @export
ratiosCoreServer <- function(id, utils_functions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Make utility functions available in this environment
    ensure_original_row_index <- utils_functions$ensure_original_row_index
    check_and_retransform_data <- utils_functions$check_and_retransform_data
    filter_data_for_analysis_fixed <- utils_functions$filter_data_for_analysis_fixed
    compute_abundance_ratio_for_row <- utils_functions$compute_abundance_ratio_for_row
    adjust_p_values_safely <- utils_functions$adjust_p_values_safely
    create_robust_ratio_result <- utils_functions$create_robust_ratio_result
    execute_statistical_method_safely <- utils_functions$execute_statistical_method_safely
    find_psm_columns <- utils_functions$find_psm_columns

    # Bind analysis function environments to this module scope so utility
    # function aliases above are resolved correctly at runtime.
    environment(perform_welch_t_test_analysis) <- environment()
    environment(perform_t_test_analysis) <- environment()
    environment(perform_moderated_welch_test_analysis) <- environment()
    environment(perform_limma_analysis) <- environment()
    environment(perform_deqms_analysis) <- environment()
    environment(perform_mann_whitney_analysis) <- environment()
    environment(estimate_hyperparameters_corrected) <- environment()

    # Return statistical analysis functions
    return(list(
      perform_welch_t_test_analysis = perform_welch_t_test_analysis,
      perform_t_test_analysis = perform_t_test_analysis,
      perform_moderated_welch_test_analysis = perform_moderated_welch_test_analysis,
      perform_limma_analysis = perform_limma_analysis,
      perform_deqms_analysis = perform_deqms_analysis,
      perform_mann_whitney_analysis = perform_mann_whitney_analysis,
      estimate_hyperparameters_corrected = estimate_hyperparameters_corrected
    ))
  })
}
