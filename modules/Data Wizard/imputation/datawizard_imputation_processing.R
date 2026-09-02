# ============================================================================
# MiraProt Imputation Server Fragment: datawizard_imputation_processing.R
# Purpose: Performs imputation while preserving the established data and metadata mutation order.
# Loading: Source in declared order with local = TRUE from modImputationServer().
# Invariant: This fragment shares the active moduleServer frame; do not source standalone.
# ============================================================================

    # ========================================
    # FIXED: Enhanced Main Imputation Function
    # ========================================

    perform_imputation <- function(max_retries = 1) {
      start_time <- Sys.time()

      # Prevent multiple simultaneous processing
      if (safe_is_true(processing_active())) {
        debug_log("Processing already active - ignoring request", 2)
        showNotification("Imputation already in progress", type = "warning", duration = 3)
        return(FALSE)
      }

      # Set processing state
      processing_active(TRUE)
      current_processing_step("Initializing imputation")

      tryCatch({
        # Get inputs with enhanced validation
        current_data <- get_current_data()
        if (is.null(current_data)) {
          stop("No data available for imputation")
        }

        selected_method <- tryCatch({
          input$imputation_method_select
        }, error = function(e) {
          debug_log("Error accessing method selection, using default", 1)
          return("left-censored")
        })

        selected_columns <- tryCatch({
          input$imputation_column_select
        }, error = function(e) {
          debug_log("Error accessing column selection", 1)
          return(character(0))
        })

        random_seed <- suppressWarnings(as.numeric(input$randomSeed_Imputation)[1])
        if (!is.finite(random_seed) || random_seed < 1 || random_seed > .Machine$integer.max) {
          random_seed <- 12345
        }
        random_seed <- as.integer(random_seed)

        valid_choices <- valid_imputation_choices_cache()
        if (length(valid_choices) == 0) {
          current_metadata <- tryCatch(data_def(), error = function(e) NULL)
          valid_choices <- get_valid_imputation_choices(current_data, current_metadata)
          valid_imputation_choices_cache(valid_choices)
        }
        selected_columns <- intersect_imputation_selections(
          selected_columns,
          valid_choices,
          "apply request"
        )
        updateSelectInput(
          session,
          "imputation_column_select",
          choices = valid_choices,
          selected = selected_columns
        )

        if (length(selected_columns) == 0) {
          last_imputation_columns(character(0))
          stop("No columns selected for imputation")
        }

        debug_log("Starting imputation", 1)
        debug_log(paste("Method:", selected_method), 1)
        debug_log(paste("Columns:", paste(selected_columns, collapse = ", ")), 1)

        # Safe missing analysis
        missing_analysis <- analyze_missing_patterns_safe(current_data, selected_columns)
        if (!is.null(missing_analysis)) {
          missing_analysis_cache(missing_analysis)
        }

        # Safe memory estimation
        memory_est <- estimate_memory_safe(current_data, selected_method, selected_columns)
        if (!is.null(memory_est)) {
          performance_metrics(memory_est)
        }

        # Progress setup
        withProgress(message = 'Performing imputation...', value = 0, {

          current_processing_step("Preparing data")
          setProgress(0.2, detail = 'Preparing data...')

          # Get current data definition safely
          current_data_def <- tryCatch({
            data_def()
          }, error = function(e) {
            debug_log(paste("Error accessing data definition:", e$message), 1)
            return(NULL)
          })

          if (is.null(current_data_def)) {
            stop("No data definition available")
          }

          current_processing_step("Executing imputation")
          setProgress(0.5, detail = paste('Executing', selected_method, 'imputation...'))

          # Perform imputation with method-specific handling
          result <- NULL
          for (attempt in 1:max_retries) {
            tryCatch({

              set.seed(random_seed)

              result <- switch(selected_method,
                               "left-censored" = {
                                 debug_log("Performing left-censored imputation", 2)
                                 performGenericImputation(current_data, current_data_def, selected_columns,
                                                          performLeftCensoredImputation, "Imputed ",
                                                          transform_policy = "raw_no_backtransform",
                                                          random_seed = random_seed)
                               },
                               "Random forest" = {
                                 debug_log("Performing Random Forest imputation", 2)
                                 # Clean up parallel processing
                                 if (exists("clean_open_clusters") && is.function(clean_open_clusters)) {
                                   clean_open_clusters()
                                 }
                                 performGenericImputation(current_data, current_data_def, selected_columns,
                                                          impute_random_forest, "Imputed ",
                                                          transform_policy = "raw_no_backtransform",
                                                          random_seed = random_seed)
                               },
                               "MICE - CART" = {
                                 debug_log("Performing MICE CART imputation", 2)
                                 performGenericImputation(current_data, current_data_def, selected_columns,
                                                          impute_mice_cart, "Imputed ",
                                                          transform_policy = "raw_no_backtransform",
                                                          random_seed = random_seed)
                               },
                               {
                                 stop("Unknown imputation method: ", selected_method)
                               }
              )

              # Break out of retry loop if successful
              break

            }, error = function(e) {
              if (attempt < max_retries) {
                debug_log(paste("Attempt", attempt, "failed:", e$message, "- retrying"), 1)
                next
              } else {
                stop(paste("All retry attempts failed. Last error:", e$message))
              }
            })
          }

          current_processing_step("Processing results")
          setProgress(0.8, detail = 'Processing results...')

          if (is.null(result) || is.null(result$data)) {
            stop("Imputation failed to produce results")
          }

          result$data <- merge_unrelated_imputed_columns(result$data, current_data, selected_columns, "Imputed ")

          debug_log(paste("Imputation successful. New dimensions:", nrow(result$data), "x", ncol(result$data)), 1)
          # Calculate total imputed values correctly
          total_imputed_actual <- if (!is.null(result$total_imputed)) {
            # If the imputation function already provides the correct count, use it
            result$total_imputed
          } else if (!is.null(result$data) && !is.null(current_data)) {
            # Count ACTUAL missing values that were replaced by comparing original vs new data
            imputed_cols <- grep("^Imputed ", names(result$data), value = TRUE)
            if (length(imputed_cols) > 0) {
              total_actual_imputed <- 0

              # For each imputed column, find its corresponding original column and count differences
              for (imputed_col in imputed_cols) {
                # Remove "Imputed " prefix to find original column name
                original_col <- gsub("^Imputed\\s+", "", imputed_col, ignore.case = TRUE)

                if (original_col %in% names(current_data)) {
                  # Count missing values in original column
                  missing_in_original <- sum(is.na(current_data[[original_col]]))
                  # Count missing values in imputed column
                  missing_in_imputed <- sum(is.na(result$data[[imputed_col]]))
                  # The difference is the number of values actually imputed
                  actually_imputed <- missing_in_original - missing_in_imputed
                  total_actual_imputed <- total_actual_imputed + actually_imputed

                  debug_log(paste("Column", original_col, "- Missing before:", missing_in_original,
                                  "Missing after:", missing_in_imputed, "Actually imputed:", actually_imputed), 2)
                } else {
                  # Fallback: if we can't find the original column, count all non-NA values in imputed column
                  # This should rarely happen but provides a safeguard
                  debug_log(paste("Warning: Could not find original column for", imputed_col, "- using fallback count"), 1)
                  total_actual_imputed <- total_actual_imputed + sum(!is.na(result$data[[imputed_col]]))
                }
              }

              total_actual_imputed
            } else {
              0
            }
          } else {
            0
          }

          debug_log(paste("Actual values imputed (corrected):", total_imputed_actual), 1)

          total_imputed_count(as.integer(total_imputed_actual))

          # Preserve/improve imputation_matrix so status can read total_imputed
          if (is.null(result$imputation_matrix)) {
            imputation_matrix(list(total_imputed = as.integer(total_imputed_actual)))
          } else {
            mi <- result$imputation_matrix
            if (is.list(mi) && is.null(mi$total_imputed)) {
              mi$total_imputed <- as.integer(total_imputed_actual)
            }
            imputation_matrix(mi)
          }

          # Update reactive values safely
          current_processing_step("Updating data")
          setProgress(0.9, detail = 'Updating data...')

          # Update data
          data_success <- FALSE
          tryCatch({
            if (!is.null(set_data) && is.function(set_data)) {
              data_success <- set_data(result$data)
              debug_log(paste("Data update via set_data():", if (data_success) "SUCCESS" else "FAILED"), 1)
            } else if (!is.null(data) && is.reactive(data)) {
              # Direct reactive update
              data(result$data)
              data_success <- TRUE
              debug_log("Data updated via data() reactive", 2)
            }

            # Force reactive invalidation
            if (data_success) {
              debug_log(paste("Updated data dimensions:", nrow(result$data), "x", ncol(result$data)), 1)

              # Log imputed columns
              imputed_cols <- grep("^Imputed ", names(result$data), value = TRUE)
              debug_log(paste("Imputed columns added:", length(imputed_cols)), 1)

              debug_log("Metadata extension handled synchronously - no delayed update needed", 2)
            }
          }, error = function(e) {
            debug_log(paste("Error updating data:", e$message), 1)
            data_success <- FALSE
          })

          if (!data_success) {
            stop("Failed to update data - imputation results not saved")
          }

          # Update metadata with enhanced preservation
          metadata_for_sync <- if (!is.null(result$data_def)) result$data_def else current_data_def
          synced_metadata <- sync_metadata_to_data_columns(metadata_for_sync, result$data, "Imputed ")

          if (!is.null(synced_metadata)) {
            result$data_def <- synced_metadata
            tryCatch({
              data_def(synced_metadata)
              debug_log("Metadata synced and updated via data_def()", 2)
            }, error = function(e) {
              debug_log(paste("Metadata sync ready but could not update data_def directly:", e$message), 2)
            })
          }

          debug_log("Metadata handling completed", 2)

          # Update tracking information
          if (!is.null(result$imputation_matrix)) {
            imputation_matrix(result$imputation_matrix)
          }

          # Quality assessment
          current_processing_step("Quality assessment")
          setProgress(0.95, detail = 'Quality assessment...')

          quality_result <- tryCatch({
            assess_imputation_quality(result)
          }, error = function(e) {
            debug_log(paste("Error in quality assessment:", e$message), 1)
            list(overall_quality_score = 0.5, values_imputed = result$total_imputed %||% 0)
          })

          if (!is.null(quality_result)) {
            quality_assessment(quality_result)
            debug_log(paste("Quality assessment completed - Score:", round(quality_result$overall_quality_score, 2),
                            "| Values imputed:", quality_result$values_imputed), 1)
          }

          setProgress(1, detail = 'Completed!')
        })

        # Record success
        last_imputation_method(selected_method)
        last_imputation_columns(selected_columns)
        imputation_applied(TRUE)
        imputation_result(result)
        add_processing_log("imputation", "success", paste("Method:", selected_method))

        end_time <- Sys.time()
        processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        last_processing_time(processing_time)

        debug_log("Imputation completed successfully", 1)
        total_imputed_value <- result$total_imputed
        if (is.null(total_imputed_value) || length(total_imputed_value) == 0 || is.na(total_imputed_value)) total_imputed_value <- "unknown"
        debug_log(paste("Total values imputed:", total_imputed_value), 1)

        imputed_cols <- grep("^Imputed ", names(result$data), value = TRUE)
        source_cols <- sub("^Imputed[[:space:]]+", "", imputed_cols, ignore.case = TRUE)
        source_cols <- source_cols[source_cols %in% names(current_data)]

        values_imputed_total <- 0L
        if (is.function(total_imputed_count)) {
          tic <- tryCatch(total_imputed_count(), error = function(e) NULL)
          if (!is.null(tic) && length(tic) > 0 && !is.na(tic[1])) {
            values_imputed_total <- as.integer(tic[1])
          }
        }

        values_imputed_numeric <- numeric(0)
        if (length(source_cols) > 0) {
          for (src_col in source_cols) {
            imp_col <- paste0("Imputed ", src_col)
            if (!imp_col %in% names(result$data)) next
            original_vec <- suppressWarnings(as.numeric(current_data[[src_col]]))
            imputed_vec  <- suppressWarnings(as.numeric(result$data[[imp_col]]))
            if (length(original_vec) != length(imputed_vec)) next
            idx <- is.na(original_vec) & !is.na(imputed_vec)
            if (any(idx)) values_imputed_numeric <- c(values_imputed_numeric, imputed_vec[idx])
          }
        }

        median_imputed_values <- if (length(values_imputed_numeric) > 0) {
          stats::median(values_imputed_numeric, na.rm = TRUE)
        } else {
          NA_real_
        }

        before_values <- unlist(lapply(source_cols, function(col_name) {
          suppressWarnings(as.numeric(current_data[[col_name]]))
        }), use.names = FALSE)
        after_values <- unlist(lapply(source_cols, function(col_name) {
          imp_col <- paste0("Imputed ", col_name)
          if (!imp_col %in% names(result$data)) return(numeric(0))
          suppressWarnings(as.numeric(result$data[[imp_col]]))
        }), use.names = FALSE)

        median_before <- if (length(before_values) > 0) stats::median(before_values, na.rm = TRUE) else NA_real_
        median_after  <- if (length(after_values) > 0) stats::median(after_values, na.rm = TRUE) else NA_real_

        fmt_num <- function(x) if (is.na(x) || !is.finite(x)) "NA" else format(signif(x, 6), trim = TRUE, scientific = FALSE)

        debug_log(
          sprintf(
            paste0(
              "Imputation summary",
              " | Method: %s",
              " | Content types (%d): [%s]",
              " | Values imputed: %s",
              " | Median of imputed values: %s",
              " | Median before imputation: %s",
              " | Median after imputation: %s",
              " | Dataset size: %d x %d",
              " | Random seed: %s"
            ),
            as.character(selected_method),
            length(selected_columns),
            if (length(selected_columns) == 0) "none" else paste(selected_columns, collapse = ", "),
            format(values_imputed_total, big.mark = ","),
            fmt_num(median_imputed_values),
            fmt_num(median_before),
            fmt_num(median_after),
            nrow(result$data),
            ncol(result$data),
            random_seed
          ),
          level = 0
        )

        showNotification(
          paste("Imputation completed successfully.",
                "Method:", selected_method,
                "Time:", round(processing_time, 1), "seconds"),
          type = "message",
          duration = 5
        )

        # Force metadata refresh if needed
        tryCatch({
          if (!is.null(data_def) && is.reactive(data_def)) {
            # Trigger metadata refresh by accessing it
            current_meta <- data_def()
            debug_log("Metadata refresh triggered", 2)
          }
        }, error = function(e) {
          debug_log("Metadata refresh not needed", 2)
        })

        return(TRUE)

      }, error = function(e) {
        debug_log(paste("Imputation failed:", e$message), 1)
        add_processing_log("imputation", "error", e$message)

        showNotification(
          paste("Imputation failed:", e$message),
          type = "error",
          duration = 10
        )

        return(FALSE)

      }, finally = {
        # Always reset processing state
        processing_active(FALSE)
        current_processing_step("")
      })
    }
