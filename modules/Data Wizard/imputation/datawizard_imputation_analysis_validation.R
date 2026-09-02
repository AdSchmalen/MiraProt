# ============================================================================
# MiraProt Imputation Server Fragment: datawizard_imputation_analysis_validation.R
# Purpose: Provides logging, missingness analysis, resource estimation, validation, quality assessment, and metadata helpers.
# Loading: Source in declared order with local = TRUE from modImputationServer().
# Invariant: This fragment shares the active moduleServer frame; do not source standalone.
# ============================================================================

    add_processing_log <- function(operation, status, details = "", duration = NULL) {
      tryCatch({
        log_entry <- list(
          timestamp = Sys.time(),
          operation = operation,
          status = status,
          details = details,
          duration = duration
        )

        current_logs <- processing_history()
        updated_logs <- append(current_logs, list(log_entry))

        # Keep only last 50 entries
        if (length(updated_logs) > 50) {
          updated_logs <- tail(updated_logs, 50)
        }

        processing_history(updated_logs)
        debug_log(paste("Log entry added:", operation, "-", status), 2)

      }, error = function(e) {
        debug_log(paste("Error adding processing log:", e$message), 1)
      })
    }

    # ========================================
    # FIXED: Safe Missing Pattern Analysis
    # ========================================

    analyze_missing_patterns_safe <- function(current_data, selected_columns) {
      tryCatch({
        if (is.null(current_data) || length(selected_columns) == 0) {
          debug_log("No data or columns for missing analysis", 2)
          return(NULL)
        }

        # Get metadata safely without causing circular dependencies
        current_metadata <- tryCatch({
          if (exists("data_def") && is.function(data_def)) {
            data_def()
          } else {
            NULL
          }
        }, error = function(e) {
          debug_log("Could not access metadata for missing analysis", 2)
          return(NULL)
        })

        if (is.null(current_metadata)) {
          debug_log("No metadata available for missing analysis", 2)
          return(NULL)
        }

        # Simple missing rate calculation without complex analysis
        relevant_cols <- c()
        for (content_type in selected_columns) {
          matching_indices <- which(current_metadata$Content == content_type)
          if (length(matching_indices) > 0) {
            col_names <- current_metadata$Column[matching_indices]
            existing_cols <- intersect(col_names, names(current_data))
            relevant_cols <- c(relevant_cols, existing_cols)
          }
        }

        if (length(relevant_cols) == 0) {
          return(NULL)
        }

        # Calculate overall missing rate safely
        total_values <- 0
        total_missing <- 0

        for (col_name in relevant_cols) {
          if (col_name %in% names(current_data)) {
            col_data <- current_data[[col_name]]
            if (!is.null(col_data)) {
              total_values <- total_values + length(col_data)
              total_missing <- total_missing + sum(is.na(col_data))
            }
          }
        }

        overall_rate <- if (total_values > 0) total_missing / total_values else 0

        # Simple mechanism estimation
        mechanism <- if (overall_rate < 0.1) {
          "Likely MCAR"
        } else if (overall_rate > 0.5) {
          "Likely MNAR"
        } else {
          "Likely MAR"
        }

        return(list(
          overall_missing_rate = overall_rate,
          mechanism_estimate = mechanism,
          columns_analyzed = length(relevant_cols)
        ))

      }, error = function(e) {
        debug_log(paste("Error in safe missing analysis:", e$message), 1)
        return(NULL)
      })
    }

    # ========================================
    # FIXED: Safe Memory Estimation
    # ========================================

    estimate_memory_safe <- function(current_data, selected_method, selected_columns) {
      tryCatch({
        if (is.null(current_data)) {
          return(list(estimated_peak_mb = 100, warning_level = "Unknown"))
        }

        # Simple size calculation
        n_rows <- nrow(current_data)
        n_cols <- ncol(current_data)

        # Safe object size estimation
        data_size_mb <- tryCatch({
          as.numeric(object.size(current_data)) / (1024^2)
        }, error = function(e) {
          # Fallback calculation
          n_rows * n_cols * 8 / (1024^2) # 8 bytes per numeric value
        })

        # Method multipliers
        multiplier <- switch(selected_method,
                             "left-censored" = 1.5,
                             "Random forest" = 3.0,
                             "MICE - CART" = 2.5,
                             2.0) # default

        estimated_mb <- data_size_mb * multiplier

        warning_level <- if (estimated_mb > 1000) {
          "High"
        } else if (estimated_mb > 500) {
          "Medium"
        } else {
          "Low"
        }

        return(list(
          estimated_peak_mb = round(estimated_mb, 1),
          warning_level = warning_level
        ))

      }, error = function(e) {
        debug_log(paste("Error in memory estimation:", e$message), 1)
        return(list(estimated_peak_mb = 100, warning_level = "Unknown"))
      })
    }

    # ========================================
    # Enhanced Data Validation
    # ========================================

    validate_data_for_imputation <- function(df, selected_columns, method = "left-censored") {
      validation_results <- list(
        valid = TRUE,
        warnings = character(0),
        errors = character(0),
        info = character(0),
        column_count = 0,
        missing_count = 0
      )

      tryCatch({
        if (is.null(df)) {
          validation_results$valid <- FALSE
          validation_results$errors <- c(validation_results$errors, "No data provided")
          return(validation_results)
        }

        if (nrow(df) == 0) {
          validation_results$valid <- FALSE
          validation_results$errors <- c(validation_results$errors, "Data contains no rows")
          return(validation_results)
        }

        # Get metadata safely
        metadata <- tryCatch({
          data_def()
        }, error = function(e) {
          debug_log(paste("Error accessing metadata in validation:", e$message), 1)
          return(NULL)
        })

        if (is.null(metadata)) {
          validation_results$warnings <- c(validation_results$warnings, "No metadata available")
          return(validation_results)
        }

        total_cols <- 0
        total_missing <- 0

        # Validate each selected column type
        for (content_type in selected_columns) {
          tryCatch({
            content_indices <- which(metadata$Content == content_type)
            if (length(content_indices) == 0) {
              validation_results$warnings <- c(validation_results$warnings,
                                               paste("No columns found for content type:", content_type))
              next
            }

            col_names <- metadata$Column[content_indices]
            existing_cols <- intersect(col_names, names(df))

            if (length(existing_cols) == 0) {
              validation_results$warnings <- c(validation_results$warnings,
                                               paste("No existing columns found for:", content_type))
              next
            }

            total_cols <- total_cols + length(existing_cols)

            # Analyze each column safely
            for (col_name in existing_cols) {
              tryCatch({
                col_data <- df[[col_name]]
                if (is.null(col_data)) next

                missing_count <- sum(is.na(col_data))
                total_missing <- total_missing + missing_count

                # Only perform additional checks on numeric columns
                if (is.numeric(col_data)) {
                  non_missing_data <- col_data[!is.na(col_data)]

                  if (length(non_missing_data) > 1) {
                    # Check for extreme ranges (safely)
                    data_range <- tryCatch({
                      diff(range(non_missing_data, na.rm = TRUE))
                    }, error = function(e) {
                      debug_log(paste("Error calculating range for", col_name), 2)
                      return(0)
                    })

                    data_median <- tryCatch({
                      median(non_missing_data, na.rm = TRUE)
                    }, error = function(e) {
                      debug_log(paste("Error calculating median for", col_name), 2)
                      return(1)
                    })

                    if (is.finite(data_range) && is.finite(data_median) && data_median != 0) {
                      if (data_range > 1000 * abs(data_median)) {
                        validation_results$warnings <- c(validation_results$warnings,
                                                         paste("Very wide data range in", col_name))
                      }
                    }
                  }

                  # Missing value proportion warnings
                  missing_prop <- missing_count / length(col_data)
                  if (missing_prop > 0.9) {
                    validation_results$warnings <- c(validation_results$warnings,
                                                     paste(col_name, "has", round(missing_prop * 100), "% missing values"))
                  }

                  # Check for constant columns
                  unique_values <- length(unique(non_missing_data))
                  if (unique_values <= 1) {
                    validation_results$warnings <- c(validation_results$warnings,
                                                     paste("Column", col_name, "has constant values"))
                  }
                }

              }, error = function(e) {
                debug_log(paste("Error analyzing column", col_name, ":", e$message), 1)
                validation_results$warnings <- c(validation_results$warnings,
                                                 paste("Error analyzing column:", col_name))
              })
            }

          }, error = function(e) {
            debug_log(paste("Error processing content type", content_type, ":", e$message), 1)
            validation_results$warnings <- c(validation_results$warnings,
                                             paste("Error processing content type:", content_type))
          })
        }

        validation_results$column_count <- total_cols
        validation_results$missing_count <- total_missing

        if (total_cols == 0) {
          validation_results$valid <- FALSE
          validation_results$errors <- c(validation_results$errors, "No valid columns found for imputation")
        }

        if (total_missing == 0) {
          validation_results$info <- c(validation_results$info, "No missing values found - imputation not needed")
        }

        debug_log(paste("Enhanced validation completed - Valid:", validation_results$valid,
                        "Columns:", validation_results$column_count,
                        "Missing values:", validation_results$missing_count), 2)

        return(validation_results)

      }, error = function(e) {
        debug_log(paste("Error during validation:", e$message), 1)
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors, paste("Validation error:", e$message))
        return(validation_results)
      })
    }

    # ========================================
    # Enhanced Imputation Tracking
    # ========================================

    calculate_total_imputed <- function(matrix_info) {
      tryCatch({
        # Prefer cached counter
        cached <- suppressWarnings(as.integer(total_imputed_count()))
        if (!is.na(cached) && cached > 0) return(cached)

        # Fall back to matrix_info$total_imputed
        if (!is.null(matrix_info) && is.list(matrix_info) && !is.null(matrix_info$total_imputed)) {
          return(as.integer(matrix_info$total_imputed))
        }

        # Last resort: quick on-demand count from current data
        current_data <- get_current_data()
        if (!is.null(current_data)) {
          imputed_cols <- grep("^Imputed ", names(current_data), value = TRUE)
          if (length(imputed_cols) > 0) {
            return(sum(vapply(imputed_cols, function(cn) sum(!is.na(current_data[[cn]])), integer(1))))
          }
        }
        0L
      }, error = function(e) {
        0L
      })
    }

    assess_imputation_quality <- function(result) {
      tryCatch({
        if (is.null(result)) return(NULL)

        quality_score <- 0.5  # Default neutral score
        values_imputed <- result$total_imputed %||% 0

        # Simple quality assessment based on method and imputed values
        if (values_imputed > 0) {
          quality_score <- 0.7  # Reasonable quality if imputation occurred
        }

        return(list(
          overall_quality_score = quality_score,
          values_imputed = values_imputed,
          assessment_method = "basic"
        ))

      }, error = function(e) {
        debug_log(paste("Error in quality assessment:", e$message), 1)
        return(list(overall_quality_score = 0.4, values_imputed = 0))
      })
    }

    # ========================================
    # ENHANCED: Metadata preservation for imputation
    # ========================================

    preserve_and_extend_metadata <- function(original_metadata, selected_content_types, prefix = "Imputed ") {
      tryCatch({
        if (is.null(original_metadata)) {
          debug_log("No original metadata to preserve", 1)
          return(NULL)
        }
        original_metadata <- datawizard_drop_deprecated_metadata_columns(
          original_metadata)

        debug_log("Preserving and extending metadata", 2)

        # Start with original metadata
        extended_metadata <- original_metadata

        # For each content type that was imputed
        for (content_type in selected_content_types) {
          # Find original rows for this content type
          original_rows <- which(original_metadata$Content == content_type)

          if (length(original_rows) > 0) {
            # Create new metadata entries for imputed columns
            for (row_idx in original_rows) {
              original_row <- original_metadata[row_idx, ]

              # Create new row for imputed column
              new_row <- original_row
              new_row$Column <- paste0(prefix, original_row$Column)
              new_row$Content <- paste0(prefix, original_row$Content)
              if ("Transformation" %in% names(new_row)) new_row$Transformation <- "None"

              # Add to extended metadata
              extended_metadata <- rbind(extended_metadata, new_row)
            }
          }
        }

        # Reset row names
        rownames(extended_metadata) <- NULL

        debug_log(paste("Metadata extended from", nrow(original_metadata), "to", nrow(extended_metadata), "rows"), 2)

        return(extended_metadata)

      }, error = function(e) {
        debug_log(paste("Error preserving metadata:", e$message), 1)
        return(original_metadata) # Return original if extension fails
      })
    }


    merge_unrelated_imputed_columns <- function(updated_data, original_data, selected_content_types, prefix = "Imputed ") {
      tryCatch({
        if (is.null(updated_data) || is.null(original_data)) return(updated_data)

        selected_imputed_cols <- paste0(prefix, selected_content_types)
        existing_imputed_cols <- grep(paste0("^", prefix), names(original_data), value = TRUE)
        preserve_cols <- setdiff(existing_imputed_cols, selected_imputed_cols)

        if (length(preserve_cols) == 0) return(updated_data)

        for (col_name in preserve_cols) {
          if (!col_name %in% names(updated_data) && col_name %in% names(original_data)) {
            updated_data[[col_name]] <- original_data[[col_name]]
            debug_log(paste("Preserved existing imputed column:", col_name), 2)
          }
        }

        updated_data
      }, error = function(e) {
        debug_log(paste("Error preserving unrelated imputed columns:", e$message), 1)
        updated_data
      })
    }

    sync_metadata_to_data_columns <- function(current_metadata, updated_data, prefix = "Imputed ") {
      tryCatch({
        if (is.null(current_metadata) || is.null(updated_data)) return(current_metadata)
        current_metadata <- datawizard_drop_deprecated_metadata_columns(
          current_metadata)

        data_cols <- names(updated_data)
        metadata_cols <- as.character(current_metadata$Column)

        keep_rows <- metadata_cols %in% data_cols
        synced_metadata <- current_metadata[keep_rows, , drop = FALSE]

        # Imputed columns are generated in raw domain and must always be tagged as untransformed.
        if ("Transformation" %in% names(synced_metadata)) {
          imputed_rows <- startsWith(as.character(synced_metadata$Column), prefix)
          synced_metadata$Transformation[imputed_rows] <- "None"
        }

        missing_cols <- setdiff(data_cols, synced_metadata$Column)
        if (length(missing_cols) > 0) {
          for (col_name in missing_cols) {
            if (startsWith(col_name, prefix)) {
              base_col <- sub(paste0("^", prefix), "", col_name)
              base_col <- sub("_dup[0-9]+$", "", base_col)
              src_idx <- which(current_metadata$Column == base_col)[1]
              if (!is.na(src_idx)) {
                new_row <- current_metadata[src_idx, , drop = FALSE]
                new_row$Column <- col_name
                if ("Content" %in% names(new_row)) {
                  source_content <- as.character(new_row$Content)
                  new_row$Content <- if (is.na(source_content) || !nzchar(trimws(source_content))) {
                    "Imputed Data"
                  } else {
                    paste("Imputed", source_content)
                  }
                }
                if ("Sample" %in% names(new_row)) {
                  dup_suffix <- regmatches(col_name, regexpr("_dup[0-9]+$", col_name))
                  if (length(dup_suffix) && !is.na(new_row$Sample) && nzchar(new_row$Sample)) {
                    new_row$Sample <- paste0(new_row$Sample, dup_suffix)
                  }
                }
                if ("Transformation" %in% names(new_row)) new_row$Transformation <- "None"
                synced_metadata <- rbind(synced_metadata, new_row)
              }
            }
          }
        }

        rownames(synced_metadata) <- NULL
        synced_metadata
      }, error = function(e) {
        debug_log(paste("Error syncing metadata to data columns:", e$message), 1)
        current_metadata
      })
    }
