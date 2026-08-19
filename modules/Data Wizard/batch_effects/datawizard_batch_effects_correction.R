# =============================================================================
# File:    datawizard_batch_effects_correction.R
# Purpose: Main batch correction pipeline for the Batch Effects module.
#
# Architecture fit:
#   Sourced into modEnv via source(..., local = modEnv) from the orchestrator
#   (datawizard_batch_effects.R).  Provides a single entry-point function
#   register_batch_correction_handler() that registers the observeEvent on
#   the correction action button.
#
# Structure:
#   register_batch_correction_handler()
#     - Registers observeEvent(input$unBatchButton, ...) containing the
#       full 12-step correction pipeline:
#       Steps 1-3:  Data validation, parameter preparation, batch group
#                   gathering and validation
#       Step  4:    Data subset extraction, numeric conversion
#       Step  5:    Imputation (Left censored / Random Forest / MICE CART)
#       Step  6:    Pre-correction transformation and log2 preparation
#       Step  7:    Batch size validation
#       Step  8:    Batch correction algorithms
#                   (Offset Correction, ComBat, Limma, LOESS, Quantile)
#       Step  9:    Back-transformation
#       Step 10:    Optional removal of imputed values
#       Steps 11-12: Merge, data update, performance recording
#
# Dependencies (via modEnv lexical scope):
#   - Imputation:   performLeftCensoredImputation, impute_random_forest,
#                   impute_mice_cart
#   - Transform:    retransform_data_global
#   - Validation:   validate_batch_data_structure, validate_batch_numeric_columns,
#                   validate_batch_groups, identify_batch_complete_cases,
#                   merge_batch_corrected_data  (all from _utils.R)
#
# Developer guidance:
#   - To add a new correction method, add a branch in the Step 8 if/else chain
#     and update the minSamples switch in Step 7.
#   - The pre-existing return() calls inside tryCatch error handlers only exit
#     the handler closure, not the outer observeEvent.  This is a known
#     limitation preserved from the original code.
#   - All user-facing text must be in English.
# =============================================================================

#' Register the main batch correction observer
#'
#' Sets up the observeEvent on the correction action button.  All reactive
#' dependencies (state, data access) are received via parameters; utility
#' and imputation functions are resolved via modEnv lexical scope.
#'
#' @param input   Shiny input object (moduleServer scope)
#' @param state   Named list of reactiveVals from init_batch_effects_state()
#' @param get_file_data  Closure returning the current data frame
#' @param set_file_data  Closure accepting a data frame to persist
#' @param debug_log      Logging function(message, level)
#' @return  invisible(NULL).  Side-effect: registers one observeEvent.
register_batch_correction_handler <- function(input, state,
                                              get_file_data, set_file_data,
                                              debug_log) {

  # --- local convenience aliases (short names used throughout) ---------------
  batchCounter       <- state$batchCounter
  last_correction_time <- state$last_correction_time
  correction_history <- state$correction_history

  validate_data   <- function(df)
    validate_batch_data_structure(df, debug_log)

  validate_cols   <- function(df, cols)
    validate_batch_numeric_columns(df, cols, debug_log)

  validate_groups <- function(batch_list, method)
    validate_batch_groups(batch_list, method, debug_log)

  find_complete   <- function(data_matrix)
    identify_batch_complete_cases(data_matrix, debug_log)

  merge_corrected <- function(original_data, corrected_data, selected_cols,
                              complete_idx, prefix)
    merge_batch_corrected_data(original_data, corrected_data, selected_cols,
                               complete_idx, prefix, debug_log)

  # ---------------------------------------------------------------------------
  # The 12-step correction pipeline
  # ---------------------------------------------------------------------------
  observeEvent(input$unBatchButton, {
    start_time <- Sys.time()
    debug_log("Starting batch correction process", 1)

    # Show progress
    progress <- shiny::Progress$new()
    progress$set(message = "Preparing batch correction...", value = 0)
    on.exit(progress$close())

    tryCatch({

      # Step 1: Initial data validation
      progress$set(message = "Validating data...", value = 0.1)
      debug_log("Step 1: Initial data validation", 2)

      df_raw <- get_file_data()
      if (is.null(df_raw)) {
        showNotification("No data available for batch correction. Please load data first.", type = "error")
        debug_log("No data available for batch correction", 1)
        return()
      }

      validation <- validate_data(df_raw)
      if (!validation$valid) {
        showNotification(validation$message, type = "error")
        debug_log(paste("Data structure validation failed:", validation$message), 1)
        return()
      }

      # Step 2: Prepare parameters
      progress$set(message = "Preparing parameters...", value = 0.2)
      debug_log("Step 2: Preparing parameters", 2)

      prefix <- "Batch Corrected "
      id_col <- "Row Index"
      orig_order <- names(df_raw)

      # Remove previously corrected columns
      old_cols <- grep(paste0("^", prefix), orig_order, value = TRUE)
      if (length(old_cols) > 0) {
        df_raw <- df_raw[, !names(df_raw) %in% old_cols, drop = FALSE]
        orig_order <- names(df_raw)
        debug_log(paste("Removed", length(old_cols), "previously corrected columns"), 2)
        showNotification(paste("Removed", length(old_cols), "previously corrected columns."), type = "message")
      }

      # Step 3: Gather and validate batch selections
      progress$set(message = "Validating batch groups...", value = 0.3)
      debug_log("Step 3: Gathering and validating batch selections", 2)

      nBatches <- max(batchCounter(), 1)
      batch_list <- lapply(seq_len(nBatches), function(i) {
        input_cols <- input[[paste0("selected_batches_", i)]] %||% character()
        setdiff(input_cols, id_col)
      })

      # Validate batch groups
      batch_validation <- validate_groups(batch_list, input$batch_method)
      if (!batch_validation$valid) {
        showNotification(batch_validation$message, type = "error")
        debug_log(paste("Batch group validation failed:", batch_validation$message), 1)
        return()
      }

      # Use validated batch groups
      batch_list <- batch_validation$groups
      selected_columns <- unlist(batch_list, use.names = FALSE)
      batch_labels <- rep(seq_along(batch_list), times = lengths(batch_list))

      debug_log(paste("Validated batch configuration:", length(batch_list), "groups,",
                      length(selected_columns), "total columns"), 2)

      # Validate selected columns
      col_validation <- validate_cols(df_raw, selected_columns)
      if (!col_validation$valid) {
        showNotification(col_validation$message, type = "error")
        debug_log(paste("Column validation failed:", col_validation$message), 1)
        return()
      }

      # Step 4: Prepare data subset
      progress$set(message = "Preparing data for correction...", value = 0.4)
      debug_log("Step 4: Preparing data subset", 2)

      # Extract selected columns and convert to numeric
      sub_raw <- df_raw[, selected_columns, drop = FALSE]

      # Convert to numeric
      sub_df <- as.data.frame(sub_raw, check.names = FALSE, stringsAsFactors = FALSE)
      conversion_warnings <- character()

      for (col_name in names(sub_df)) {
        if (!is.numeric(sub_df[[col_name]])) {
          original_na_count <- sum(is.na(sub_df[[col_name]]))
          sub_df[[col_name]] <- suppressWarnings(as.numeric(as.character(sub_df[[col_name]])))
          new_na_count <- sum(is.na(sub_df[[col_name]]))

          if (new_na_count > original_na_count) {
            conversion_warnings <- c(conversion_warnings,
                                     paste0(col_name, ": ", new_na_count - original_na_count, " values converted to NA"))
          }
        }
      }

      if (length(conversion_warnings) > 0) {
        debug_log(paste("Data conversion warnings:", paste(conversion_warnings, collapse = "; ")), 2)
        showNotification(paste("Data conversion warnings:", paste(conversion_warnings, collapse = "; ")),
                         type = "warning")
      }

      # Store original data with NAs for potential restoration later
      original_data_with_nas <- sub_df

      # Initialize processing variables
      process_all_data <- FALSE
      complete_cases <- logical(0)

      # Determine processing approach based on imputation method
      imp_method <- input$imputation_method_batch
      debug_log(paste("Selected imputation method:", imp_method), 2)

      if (imp_method == "None") {
        # No imputation: only process complete cases
        complete_cases <- find_complete(sub_df)
        n_complete <- sum(complete_cases)
        n_total <- nrow(sub_df)

        if (n_complete == 0) {
          showNotification("No complete cases found for batch correction. All proteins have missing values.", type = "error")
          debug_log("No complete cases found for batch correction", 1)
          return()
        }

        if (n_complete < n_total) {
          debug_log(paste("Processing", n_complete, "complete proteins out of", n_total, "total proteins"), 2)
          showNotification(paste("Processing", n_complete, "complete proteins out of", n_total, "total proteins.",
                                 n_total - n_complete, "proteins with missing values will be skipped."),
                           type = "message", duration = 5)
        }

        # Extract complete data for correction
        complete_data <- sub_df[complete_cases, , drop = FALSE]
        process_all_data <- FALSE

      } else {
        # With imputation: prepare to process all data after imputation
        complete_cases <- rep(TRUE, nrow(sub_df))  # All rows will be processed
        complete_data <- sub_df
        process_all_data <- TRUE

        debug_log(paste("Imputation selected:", imp_method, "- All", nrow(sub_df), "proteins will be processed"), 2)
        showNotification(paste("Imputation selected:", imp_method, "- All", nrow(sub_df), "proteins will be processed."),
                         type = "message", duration = 4)
      }

      # Step 5: Handle imputation (only if requested)
      progress$set(message = "Handling imputation (if selected)...", value = 0.5)
      debug_log("Step 5: Handling imputation", 2)

      # Store original data for potential NA restoration
      original_complete_data <- complete_data

      if (imp_method != "None") {
        debug_log(paste("Applying imputation method:", imp_method), 2)
        tryCatch({
          if (imp_method == "Left censored") {
            complete_data <- performLeftCensoredImputation(complete_data)
          } else if (imp_method == "Random Forest") {
            complete_data <- impute_random_forest(complete_data)
          } else if (imp_method == "MICE CART") {
            complete_data <- impute_mice_cart(complete_data)
          }

          # After imputation, update processing parameters
          process_all_data <- TRUE
          complete_cases <- rep(TRUE, nrow(complete_data))  # All rows are now complete

          debug_log(paste("Imputation completed using", imp_method, "method"), 2)
          showNotification(paste("Imputation completed using", imp_method, "method. All proteins will now be batch corrected."), type = "message")

        }, error = function(e) {
          debug_log(paste("Error during imputation:", e$message), 1)
          showNotification(paste("Error during imputation:", e$message), type = "error")
          return()
        })
      }

      # Step 6: Prepare for batch correction
      progress$set(message = "Preparing for batch correction...", value = 0.6)
      debug_log("Step 6: Preparing for batch correction", 2)

      # Undo prior transformation if needed
      if (input$transformation_batch != "None") {
        debug_log(paste("Applying transformation:", input$transformation_batch), 2)
        tryCatch({
          mat <- as.matrix(complete_data)
          mat2 <- retransform_data_global(
            mat,
            seq_len(ncol(mat)),
            rep(input$transformation_batch, ncol(mat))
          )
          complete_data <- as.data.frame(mat2, check.names = FALSE, stringsAsFactors = FALSE)
        }, error = function(e) {
          debug_log(paste("Error in data transformation:", e$message), 1)
          showNotification(paste("Error in data transformation:", e$message), type = "error")
          return()
        })
      }

      # Log2 transform for correction (standard approach)
      tryCatch({
        # Check for non-positive values
        min_val <- min(complete_data, na.rm = TRUE)
        if (min_val <= 0) {
          # Add small constant to handle zero/negative values
          offset <- abs(min_val) + 1e-6
          complete_data <- complete_data + offset
          debug_log(paste("Added offset of", signif(offset, 3), "to handle non-positive values"), 2)
          showNotification(paste("Added offset of", signif(offset, 3), "to handle non-positive values."),
                           type = "message")
        }

        data_for_correction <- log2(as.matrix(complete_data))

        # Validate the matrix
        if (nrow(data_for_correction) == 0 || ncol(data_for_correction) == 0) {
          debug_log("No valid data remaining for batch correction", 1)
          showNotification("No valid data remaining for batch correction.", type = "error")
          return()
        }

        # Check for infinite values
        if (any(is.infinite(data_for_correction))) {
          debug_log("Data contains infinite values", 1)
          showNotification("Data contains infinite values. Please check your input data.", type = "error")
          return()
        }

        debug_log(paste("Data prepared for correction:", nrow(data_for_correction), "x",
                        ncol(data_for_correction)), 2)

      }, error = function(e) {
        debug_log(paste("Error preparing data for correction:", e$message), 1)
        showNotification(paste("Error preparing data for correction:", e$message), type = "error")
        return()
      })

      # Step 7: Validate batch sizes for complete data
      progress$set(message = "Validating batch sizes...", value = 0.7)
      debug_log("Step 7: Validating batch sizes", 2)

      unique_batches <- unique(batch_labels)
      minSamples <- switch(input$batch_method,
                           "ComBat" = 3,
                           "Limma" = 2,
                           "Offset Correction" = 2,
                           "LOESS" = 3,
                           "Quantile" = 2,
                           2)

      batch_size_warnings <- character()
      for (b in unique_batches) {
        cnt <- sum(batch_labels == b)
        if (cnt < minSamples) {
          batch_size_warnings <- c(batch_size_warnings,
                                   paste0("Batch ", b, " has only ", cnt, " column(s)"))
        }
      }

      if (length(batch_size_warnings) > 0) {
        debug_log(paste("Batch size warnings:", paste(batch_size_warnings, collapse = "; ")), 2)
        showNotification(paste("Small batch size warning:", paste(batch_size_warnings, collapse = "; "),
                               "- Results may be unreliable."), type = "warning")
      }

      # Step 8: Perform batch correction
      progress$set(message = paste("Applying", input$batch_method, "correction..."), value = 0.8)
      debug_log(paste("Step 8: Performing", input$batch_method, "correction"), 2)

      corrected_data <- NULL

      tryCatch({

        if (input$batch_method == "Offset Correction") {
          debug_log("Applying Offset Correction method", 2)
          global_med <- median(data_for_correction, na.rm = TRUE)
          corrected_matrix <- data_for_correction

          for (i in unique_batches) {
            cols_i <- which(batch_labels == i)
            batch_data <- data_for_correction[, cols_i, drop = FALSE]
            batch_med <- median(batch_data, na.rm = TRUE)

            for (j in cols_i) {
              col_med <- median(data_for_correction[, j], na.rm = TRUE)
              offset <- (global_med - batch_med) + (batch_med - col_med)
              corrected_matrix[, j] <- data_for_correction[, j] + offset
            }
          }

          corrected_data <- corrected_matrix

        } else if (input$batch_method == "ComBat") {
          debug_log("Applying ComBat method", 2)
          # Load required package
          if (!requireNamespace("sva", quietly = TRUE)) {
            debug_log("Package 'sva' not available for ComBat correction", 1)
            showNotification("Package 'sva' is required for ComBat correction.", type = "error")
            return()
          }

          mat <- data_for_correction

          # Filter out constant features
          feature_vars <- apply(mat, 1, var, na.rm = TRUE)
          nonconst <- feature_vars > 1e-8 & !is.na(feature_vars)

          if (!any(nonconst)) {
            debug_log("No features with sufficient variance found", 1)
            showNotification("No features with sufficient variance found.", type = "error")
            return()
          }

          if (sum(nonconst) < nrow(mat)) {
            debug_log(paste("Filtered out", nrow(mat) - sum(nonconst), "constant features"), 2)
            showNotification(paste("Filtered out", nrow(mat) - sum(nonconst), "constant features."),
                             type = "message")
          }

          mat2 <- mat[nonconst, , drop = FALSE]
          batch_fac <- factor(batch_labels)
          mod <- model.matrix(~1, data.frame(batch = batch_fac))

          adj <- sva::ComBat(dat = mat2, batch = batch_fac, mod = mod,
                             par.prior = TRUE, prior.plots = FALSE)

          # Restore corrected values to the full matrix
          corrected_matrix <- mat
          corrected_matrix[nonconst, ] <- adj
          corrected_data <- corrected_matrix

        } else if (input$batch_method == "Limma") {
          debug_log("Applying Limma method", 2)
          # Load required package
          if (!requireNamespace("limma", quietly = TRUE)) {
            debug_log("Package 'limma' not available for Limma correction", 1)
            showNotification("Package 'limma' is required for Limma correction.", type = "error")
            return()
          }

          corrected_data <- limma::removeBatchEffect(data_for_correction, batch = batch_labels)

        } else if (input$batch_method == "LOESS") {
          debug_log("Applying LOESS method", 2)
          mat <- data_for_correction
          out_mat <- matrix(NA, nrow(mat), ncol(mat), dimnames = dimnames(mat))

          for (b in unique_batches) {
            idx_b <- which(batch_labels == b)
            grp <- mat[, idx_b, drop = FALSE]
            ref <- apply(grp, 1, median, na.rm = TRUE)

            for (k in seq_along(idx_b)) {
              tryCatch({
                fit <- loess(grp[, k] ~ ref, span = 0.75, family = "symmetric", degree = 1)
                trend <- predict(fit, newdata = ref)
                out_mat[, idx_b[k]] <- (grp[, k] - trend) + ref
              }, error = function(e) {
                # Fallback: use original values if LOESS fails
                debug_log(paste("LOESS failed for column", idx_b[k], "- using original values"), 2)
                out_mat[, idx_b[k]] <- grp[, k]
              })
            }
          }

          corrected_data <- out_mat

        } else if (input$batch_method == "Quantile") {
          debug_log("Applying Quantile method", 2)
          # Load required package
          if (!requireNamespace("limma", quietly = TRUE)) {
            debug_log("Package 'limma' not available for Quantile normalization", 1)
            showNotification("Package 'limma' is required for Quantile normalization.", type = "error")
            return()
          }

          corrected_matrix <- data_for_correction

          for (i in unique_batches) {
            cols_i <- which(batch_labels == i)
            batch_dat <- data_for_correction[, cols_i, drop = FALSE]

            normed <- if (ncol(batch_dat) > 1) {
              limma::normalizeQuantiles(batch_dat)
            } else {
              batch_dat
            }

            corrected_matrix[, cols_i] <- normed
          }

          corrected_data <- corrected_matrix
        }

        debug_log(paste("Batch correction completed using", input$batch_method, "method"), 2)

      }, error = function(e) {
        debug_log(paste("Error during", input$batch_method, "correction:", e$message), 1)
        showNotification(paste("Error during", input$batch_method, "correction:", e$message), type = "error")
        return()
      })

      if (is.null(corrected_data)) {
        debug_log("Batch correction failed - no corrected data generated", 1)
        showNotification("Batch correction failed. No corrected data generated.", type = "error")
        return()
      }

      # Step 9: Back-transform corrected data
      progress$set(message = "Back-transforming data...", value = 0.9)
      debug_log("Step 9: Back-transforming corrected data", 2)

      tryCatch({
        # Convert back from log2
        corrected_data <- 2^corrected_data

        # Apply user-specified transformation
        tfm <- input$transformation_batch
        if (tfm == "log2") {
          corrected_data <- log2(corrected_data)
        } else if (tfm == "log10") {
          corrected_data <- log10(corrected_data)
        } else if (tfm == "-log10") {
          corrected_data <- -log10(corrected_data)
        }

        debug_log(paste("Back-transformation completed with:", tfm), 2)

      }, error = function(e) {
        debug_log(paste("Error during back-transformation:", e$message), 1)
        showNotification(paste("Error during back-transformation:", e$message), type = "warning")
      })

      # Step 10: Handle imputed values removal if requested
      if (imp_method != "None" && input$remove_imputed_batch) {
        progress$set(message = "Removing imputed values...", value = 0.95)
        debug_log("Step 10: Removing imputed values", 2)

        # Restore original NA pattern for imputed values
        original_na_mask <- is.na(original_complete_data)

        # Apply the mask to corrected data
        for (j in seq_len(ncol(corrected_data))) {
          if (process_all_data) {
            # When all data was processed, mask corresponds to original full dataset
            original_na_mask_full <- is.na(original_data_with_nas[, j])
            corrected_data[original_na_mask_full, j] <- NA
          } else {
            # When only complete cases were processed, mask corresponds to complete cases only
            mask_j <- original_na_mask[, j]
            corrected_data[mask_j, j] <- NA
          }
        }
        debug_log("Imputed values removed from corrected data", 2)
        showNotification("Imputed values removed from corrected data.", type = "message")
      }

      # Step 11: Merge corrected data back into original dataset
      progress$set(message = "Finalizing and updating data...", value = 0.98)
      debug_log("Step 11: Merging corrected data back into original dataset", 2)

      # Convert corrected matrix back to data frame
      corrected_df <- as.data.frame(corrected_data, check.names = FALSE, stringsAsFactors = FALSE)
      colnames(corrected_df) <- selected_columns

      # Merge strategy depends on whether all data was processed or only complete cases
      if (process_all_data) {
        # All data was processed (with imputation) - replace all values
        final_data <- merge_corrected(
          original_data = df_raw,
          corrected_data = corrected_df,
          selected_cols = selected_columns,
          complete_idx = rep(TRUE, nrow(df_raw)),  # All rows were processed
          prefix = prefix
        )

        debug_log(paste("All", nrow(corrected_df), "proteins were batch corrected"), 2)
        showNotification(paste("All", nrow(corrected_df), "proteins were batch corrected."),
                         type = "message", duration = 3)
      } else {
        # Only complete cases were processed (no imputation) - merge selectively
        final_data <- merge_corrected(
          original_data = df_raw,
          corrected_data = corrected_df,
          selected_cols = selected_columns,
          complete_idx = complete_cases,  # Only complete cases were processed
          prefix = prefix
        )

        n_corrected <- sum(complete_cases)
        n_total <- nrow(df_raw)
        debug_log(paste("Batch correction completed for", n_corrected, "out of", n_total, "proteins"), 2)
        showNotification(paste("Batch correction completed for", n_corrected, "out of", n_total, "proteins.",
                               n_total - n_corrected, "proteins with missing values were skipped."),
                         type = "message", duration = 4)
      }

      # Update the data
      success <- set_file_data(final_data)
      if (!success) {
        debug_log("Warning: Could not update data source", 1)
        showNotification("Warning: Could not update data source. Data structure may have changed.", type = "warning")
      }

      # Step 12: Final validation and success reporting
      progress$set(message = "Finalizing...", value = 1.0)
      debug_log("Step 12: Final validation", 2)

      corrected_cols <- grep(paste0("^", prefix), names(final_data), value = TRUE)
      if (length(corrected_cols) == 0) {
        debug_log("No corrected columns were created", 1)
        showNotification("No corrected columns were created. Please check your settings.", type = "error")
        return()
      }

      # Record performance metrics
      total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      last_correction_time(total_duration)

      # Add to correction history
      history_entry <- list(
        timestamp = Sys.time(),
        method = input$batch_method,
        duration = total_duration,
        columns_corrected = length(corrected_cols),
        proteins_processed = if (process_all_data) nrow(final_data) else sum(complete_cases)
      )
      current_history <- correction_history()
      correction_history(c(current_history, list(history_entry)))

      # Success message
      if (process_all_data) {
        success_message <- paste("Batch correction completed successfully!",
                                 "Method:", input$batch_method,
                                 "| Corrected columns:", length(corrected_cols),
                                 "| All", nrow(final_data), "proteins processed",
                                 sprintf("| Duration: %.2fs", total_duration))
      } else {
        n_corrected <- sum(complete_cases)
        n_total <- nrow(final_data)
        success_message <- paste("Batch correction completed successfully!",
                                 "Method:", input$batch_method,
                                 "| Corrected columns:", length(corrected_cols),
                                 "| Proteins processed:", n_corrected, "out of", n_total,
                                 sprintf("| Duration: %.2fs", total_duration))
      }

      debug_log(paste("Batch correction completed successfully:", input$batch_method, "method,",
                      length(corrected_cols), "columns,", sprintf("%.2fs", total_duration)), 1)

      batch_groups_str <- paste(
        vapply(seq_along(batch_list), function(i) {
          batch_cols <- batch_list[[i]]
          paste0("Batch ", i, " (", length(batch_cols), "): [", paste(batch_cols, collapse = ", "), "]")
        }, character(1)),
        collapse = " | "
      )

      debug_log(
        sprintf(
          paste0(
            "Batch correction summary",
            " | Method: %s",
            " | Batch groups: %d",
            " | %s",
            " | Transformation: %s",
            " | Imputation: %s",
            " | Remove imputed values: %s",
            " | Columns corrected: %d",
            " | Proteins processed: %d"
          ),
          as.character(input$batch_method),
          length(batch_list),
          batch_groups_str,
          as.character(input$transformation_batch),
          as.character(imp_method),
          as.character(isTRUE(input$remove_imputed_batch)),
          length(corrected_cols),
          if (process_all_data) nrow(final_data) else sum(complete_cases)
        ),
        level = 0
      )
      showNotification(success_message, type = "message", duration = 6)

    }, error = function(err) {
      error_msg <- paste("Unexpected error during batch correction:", err$message)
      debug_log(error_msg, 1)
      showNotification(error_msg, type = "error")
    })
  })

  invisible(NULL)
}
