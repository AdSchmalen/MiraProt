# ============================================================================
# MiraProt Imputation Server Fragment: datawizard_imputation_observers_api.R
# Purpose: Registers UI/API observers, renderers, cleanup and restore hooks, and assembles the public return API.
# Loading: Source in declared order with local = TRUE from modImputationServer().
# Invariant: This fragment shares the active moduleServer frame; do not source standalone.
# ============================================================================

    # ========================================
    # Enhanced Reset Function
    # ========================================

    # reset_imputation <- function() {
    #   start_time <- Sys.time()
    #
    #   tryCatch({
    #     debug_log("Starting imputation reset", 1)
    #
    #     current_data <- get_current_data()
    #     if (is.null(current_data)) {
    #       showNotification("No data available to reset", type = "warning", duration = 4)
    #       return(FALSE)
    #     }
    #
    #     # Find imputed columns
    #     imputed_cols <- grep("^Imputed ", names(current_data), value = TRUE)
    #     if (length(imputed_cols) == 0) {
    #       showNotification("No imputed columns found to remove", type = "info", duration = 3)
    #       return(TRUE)
    #     }
    #
    #     # Remove imputed columns
    #     clean_data <- current_data[, !names(current_data) %in% imputed_cols, drop = FALSE]
    #     debug_log(paste("Removing", length(imputed_cols), "imputed columns"), 1)
    #
    #     # Update data
    #     success <- set_current_data(clean_data)
    #     duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    #
    #     if (success) {
    #       # Reset state
    #       imputation_result(NULL)
    #       imputation_matrix(NULL)
    #       imputation_applied(FALSE)
    #       last_imputation_method("None")
    #       last_imputation_columns(character(0))
    #       original_data_backup(NULL)
    #       quality_assessment(NULL)
    #       missing_analysis_cache(NULL)
    #       method_recommendations(NULL)
    #       performance_metrics(NULL)
    #       total_imputed_count(0)
    #
    #       # Clear processing errors
    #       processing_errors(list())
    #
    #       add_processing_log("reset", "success", "Imputation reset completed", duration)
    #
    #       success_msg <- paste("Imputation reset! Removed", length(imputed_cols), "columns.")
    #       debug_log("Imputation reset successful", 1)
    #       showNotification(success_msg, type = "message", duration = 3)
    #     } else {
    #       add_processing_log("reset", "error", "Failed to update data after reset", duration)
    #       showNotification("Failed to update data after reset", type = "error", duration = 6)
    #     }
    #
    #     return(success)
    #
    #   }, error = function(e) {
    #     error_msg <- paste("Error resetting imputation:", e$message)
    #     debug_log(paste("ERROR in reset:", e$message), 1)
    #     add_processing_log("reset", "error", e$message)
    #     showNotification(error_msg, type = "error")
    #     return(FALSE)
    #   })
    # }

    # ========================================
    # Enhanced Status Management
    # ========================================

    get_processing_summary <- reactive({
      tryCatch({
        if (processing_active()) {
          step <- current_processing_step()
          if (nzchar(step)) {
            return(paste("Processing:", step))
          } else {
            return("Processing...")
          }
        } else if (imputation_applied()) {
          matrix_info <- imputation_matrix()
          total_imputed <- as.integer(total_imputed_count())
          quality_result <- quality_assessment()
          quality_text <- if (!is.null(quality_result)) {
            paste("| Quality score:", round(quality_result$overall_quality_score, 2))
          } else {
            ""
          }
          paste("Imputation completed. Method:", last_imputation_method(),
                "| Values imputed:", total_imputed, quality_text)
        } else {
          "No imputation applied"
        }
      }, error = function(e) {
        debug_log(paste("Error in status display:", e$message), 1)
        return("Status unavailable")
      })
    })

    # ========================================
    # Enhanced Event Handlers
    # ========================================

    observeEvent(input$apply_imputation_btn, {
      tryCatch({
        current_click <- as.integer(input$apply_imputation_btn %||% 0L)
        if (!is.finite(current_click)) current_click <- 0L

        # Ignore already handled (or stale queued) click events.
        if (current_click <= last_handled_apply_click()) {
          debug_log("Ignoring stale apply click event", 2)
          return()
        }

        req(input$imputation_method_select != "None")
        debug_log("Apply imputation button triggered", 1)

        # Prevent multiple simultaneous processing
        if (safe_is_true(processing_active())) {
          last_handled_apply_click(max(last_handled_apply_click(), current_click))
          debug_log("Processing already active - ignoring button press", 2)
          showNotification("Processing already in progress", type = "warning", duration = 3)
          return()
        }

        perform_imputation()

        # Mark the latest visible click counter as handled. This collapses any
        # rapid extra clicks that happened while imputation was running.
        latest_click <- as.integer(input$apply_imputation_btn %||% current_click)
        if (!is.finite(latest_click)) latest_click <- current_click
        last_handled_apply_click(max(last_handled_apply_click(), latest_click))

      }, error = function(e) {
        debug_log(paste("Error in apply button handler:", e$message), 1)
        showNotification(paste("Error starting imputation:", e$message), type = "error", duration = 8)
      })
    })

    # observeEvent(input$reset_imputation_btn, {
    #   tryCatch({
    #     debug_log("Reset imputation button triggered", 1)
    #     reset_imputation()
    #   }, error = function(e) {
    #     debug_log(paste("Error in reset button handler:", e$message), 1)
    #     showNotification(paste("Error resetting imputation:", e$message), type = "error", duration = 8)
    #   })
    # })

    # ========================================
    # Output Renderers
    # ========================================

    # # Enhanced data validation status
    # output$data_validation_status <- renderText({
    #   tryCatch({
    #     req(input$imputation_method_select != "None")
    #     current_data <- get_current_data()
    #     selected_cols <- input$imputation_column_select
    #
    #     if (is.null(current_data) || length(selected_cols) == 0) {
    #       return("Please select data types to validate...")
    #     }
    #
    #     validation <- validate_data_for_imputation(current_data, selected_cols)
    #
    #     status_lines <- c(
    #       paste("Data Overview:"),
    #       paste("- Rows:", nrow(current_data)),
    #       paste("- Columns to impute:", validation$column_count),
    #       paste("- Missing values:", validation$missing_count)
    #     )
    #
    #     # Add analysis results if available
    #     missing_analysis <- missing_analysis_cache()
    #     if (!is.null(missing_analysis)) {
    #       status_lines <- c(status_lines, "",
    #                         paste("Missing Data Analysis:"),
    #                         paste("- Overall missing rate:", round(missing_analysis$overall_missing_rate * 100, 1), "%"),
    #                         paste("- Estimated mechanism:", missing_analysis$mechanism_estimate))
    #     }
    #
    #     # Add memory estimates if available
    #     memory_est <- performance_metrics()
    #     if (!is.null(memory_est)) {
    #       status_lines <- c(status_lines, "",
    #                         paste("Resource Estimates:"),
    #                         paste("- Memory usage:", memory_est$estimated_peak_mb, "MB"),
    #                         paste("- Warning level:", memory_est$warning_level))
    #     }
    #
    #     if (length(validation$errors) > 0) {
    #       status_lines <- c(status_lines, "", "ERRORS:")
    #       status_lines <- c(status_lines, paste("X", validation$errors))
    #     }
    #
    #     if (length(validation$warnings) > 0) {
    #       status_lines <- c(status_lines, "", "Warnings:")
    #       status_lines <- c(status_lines, paste("!", validation$warnings))
    #     }
    #
    #     if (length(validation$info) > 0) {
    #       status_lines <- c(status_lines, "", "Info:")
    #       status_lines <- c(status_lines, paste("i", validation$info))
    #     }
    #
    #     if (validation$valid) {
    #       status_lines <- c(status_lines, "", "Ready for imputation")
    #     }
    #
    #     return(paste(status_lines, collapse = "\n"))
    #
    #   }, error = function(e) {
    #     debug_log(paste("Error rendering validation status:", e$message), 1)
    #     return(paste("Error rendering status:", e$message))
    #   })
    # })

    # Enhanced imputation status display
    output$imputation_status_display <- renderText({
      tryCatch({
        status_lines <- character()

        if (imputation_applied()) {
          status_lines <- c(status_lines, "Imputation Applied")
          status_lines <- c(status_lines, paste("Method:", last_imputation_method()))
          status_lines <- c(status_lines, paste("Content Types:", paste(last_imputation_columns(), collapse = ", ")))

          # Calculate statistics
          matrix_info <- imputation_matrix()
          total_imputed <- as.integer(total_imputed_count())
          status_lines <- c(status_lines, paste("Values imputed:", format(total_imputed, big.mark = ",")))

          # Check for new columns
          current_data <- get_current_data()
          if (!is.null(current_data)) {
            imputed_cols <- grep("^Imputed ", names(current_data), value = TRUE)
            if (length(imputed_cols) > 0) {
              status_lines <- c(status_lines, paste("New columns created:", length(imputed_cols)))
            }
          }

          # Quality information
          quality_result <- quality_assessment()
          if (!is.null(quality_result)) {
            status_lines <- c(status_lines, paste("Quality score:", round(quality_result$overall_quality_score, 2)))
          }

          # Processing time
          proc_time <- last_processing_time()
          if (!is.null(proc_time)) {
            status_lines <- c(status_lines, paste("Processing time:", round(proc_time, 1), "seconds"))
          }

        } else {
          status_lines <- c(status_lines, "No imputation applied")

          # Check for existing imputed columns (inconsistent state)
          current_data <- get_current_data()
          if (!is.null(current_data)) {
            imputed_cols <- grep("^Imputed ", names(current_data), value = TRUE)
            if (length(imputed_cols) > 0) {
              status_lines <- c(status_lines, paste("WARNING: Found", length(imputed_cols), "imputed columns but status shows not applied"))
            }
          }
        }

        # Processing status
        if (processing_active()) {
          status_lines <- c(status_lines, "", "Processing Status:")
          step <- current_processing_step()
          if (nzchar(step)) {
            status_lines <- c(status_lines, paste("Current step:", step))
          } else {
            status_lines <- c(status_lines, "Processing...")
          }
        }

        # Error information
        errors <- processing_errors()
        if (length(errors) > 0) {
          status_lines <- c(status_lines, "", "Recent Errors:")
          status_lines <- c(status_lines, paste("Count:", length(errors)))
        }

        return(paste(status_lines, collapse = "\n"))

      }, error = function(e) {
        debug_log(paste("Error rendering imputation status:", e$message), 1)
        return("Error retrieving status")
      })
    })

    # ========================================
    # Enhanced Module Health Check
    # ========================================

    module_health_check <- function() {
      tryCatch({
        health_status <- list(
          module_name = "Imputation",
          status = "OK",
          processing_active = processing_active(),
          imputation_applied = imputation_applied(),
          error_count = length(processing_errors()),
          debug_level = DEBUG_LEVEL,
          last_processing_time = last_processing_time(),
          last_method = last_imputation_method(),
          has_analysis_cache = !is.null(missing_analysis_cache()),
          has_quality_assessment = !is.null(quality_assessment())
        )

        # Check for potential issues
        warnings <- character()

        if (health_status$error_count > 3) {
          warnings <- c(warnings, paste("High error count:", health_status$error_count))
        }

        # Check data availability
        current_data <- get_current_data()
        if (is.null(current_data)) {
          warnings <- c(warnings, "No data available")
        } else {
          imputed_cols <- grep("^Imputed ", names(current_data), value = TRUE)
          if (length(imputed_cols) > 0 && !health_status$imputation_applied) {
            warnings <- c(warnings, "Imputed columns exist but status shows not applied")
          }
        }

        # Check metadata availability
        metadata <- tryCatch({
          data_def()
        }, error = function(e) {
          return(NULL)
        })

        if (is.null(metadata)) {
          warnings <- c(warnings, "No metadata available")
        }

        # Check for memory issues
        memory_est <- performance_metrics()
        if (!is.null(memory_est) && memory_est$warning_level == "High") {
          warnings <- c(warnings, "High memory usage detected")
        }

        health_status$warnings <- warnings
        health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

        debug_log(paste("Module health check - Status:", health_status$overall_health), 2)
        return(health_status)

      }, error = function(e) {
        debug_log(paste("Error in module health check:", e$message), 1)
        return(list(
          module_name = "Imputation",
          status = "ERROR",
          error_message = e$message,
          overall_health = "Critical"
        ))
      })
    }

    # ========================================
    # Session Cleanup
    # ========================================

    # Register cleanup function
    cleanup_manager$register_module("Imputation", function() {
      debug_log("Executing [Imputation] cleanup", 2)

      # Clear all reactive values
      imputation_result(NULL)
      imputation_matrix(NULL)
      imputation_applied(FALSE)
      last_imputation_method("None")
      last_imputation_columns(character(0))
      original_data_backup(NULL)
      processing_active(FALSE)
      current_processing_step("")
      processing_errors(list())
      processing_history(list())
      last_processing_time(NULL)
      missing_analysis_cache(NULL)
      quality_assessment(NULL)
      performance_metrics(NULL)
      method_recommendations(NULL)
      log_container(list())
      performance_log(list())
      error_log(list())

      debug_log("[Imputation] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    imputation_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        imputation_method_select = "selectInput",
        imputation_column_select = "selectInput",
        randomSeed_Imputation = "numericInput"
      ),
      module_label    = "Imputation",
      restore_trigger = session_restore_trigger
    )
    set_imputation_session_state <- function(state) {
      if (is.list(state) && !is.null(state$ui_inputs) && is.list(state$ui_inputs)) {
        if (is.null(state$ui_inputs$randomSeed_Imputation)) state$ui_inputs$randomSeed_Imputation <- 12345
      } else if (is.list(state) && is.null(state$randomSeed_Imputation)) {
        state$randomSeed_Imputation <- 12345
      }
      imputation_session_state$set_session_state(state)
    }

    # ========================================
    # Enhanced Return Interface
    # ========================================

    .imputation_api <- list(
      # Core return structure for integration
      imputation_result = imputation_result,

      # Session-restore bridge
      get_session_state = imputation_session_state$get_session_state,
      set_session_state = set_imputation_session_state,
      imputation_setting = reactive({
        tryCatch({
          list(
            imputation_method_select = input$imputation_method_select,
            imputation_column_select = input$imputation_column_select,
            randomSeed_Imputation = input$randomSeed_Imputation
          )
        }, error = function(e) {
          debug_log(paste("Error getting imputation settings:", e$message), 1)
          list(
            imputation_method_select = "None",
            imputation_column_select = character(0),
            randomSeed_Imputation = 12345
          )
        })
      }),
      imputation_log = reactive({ log_container() }),

      # Enhanced status functions
      processing_active = processing_active,
      current_processing_step = current_processing_step,
      processing_errors = reactive({ processing_errors() }),
      has_results = reactive({ !is.null(imputation_result()) }),
      processing_summary = get_processing_summary,

      # Advanced analysis results
      missing_analysis = reactive({ missing_analysis_cache() }),
      quality_assessment = reactive({ quality_assessment() }),
      performance_metrics = reactive({ performance_metrics() }),
      method_recommendations = reactive({ method_recommendations() }),

      # Control functions
      perform_imputation_function = reactive({ perform_imputation() }),
      # reset_imputation_function = reactive({ reset_imputation() }),

      # Health and diagnostics
      module_health_check = module_health_check,
      get_processing_log = reactive({ processing_history() }),

      # Clear processing errors
      clear_processing_errors = function() {
        tryCatch({
          processing_errors(list())
          debug_log("Processing errors cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing processing errors:", e$message), 1)
        })
      },

      # ========================================
      # UI import/export
      # ========================================

      # Core return structure for integration
      imputation_applied = reactive({ imputation_applied() }),
      last_method = reactive({ last_imputation_method() }),
      last_columns = reactive({ last_imputation_columns() }),
      imputation_matrix = reactive({ imputation_matrix() }),
      imputation_result = reactive({ imputation_result() }),

      # Data functions
      get_data = get_current_data,
      set_data = set_current_data,

      # Processing functions
      perform_imputation = perform_imputation,
      # reset_imputation = reset_imputation,

      # Analysis functions
      get_missing_analysis = reactive({ missing_analysis_cache() }),
      get_quality_assessment = reactive({ quality_assessment() }),
      get_performance_metrics = reactive({ performance_metrics() }),
      get_method_recommendations = reactive({ method_recommendations() }),

      # Status functions
      processing_active = reactive({ processing_active() }),
      current_step = reactive({ current_processing_step() }),
      processing_errors = reactive({ processing_errors() }),
      processing_history = reactive({ processing_history() }),
      last_processing_time = reactive({ last_processing_time() }),

      # Health check
      module_health_check = module_health_check,

      # ========================================
      # NEW: UI CONFIGURATION EXPORT/IMPORT FUNCTIONS
      # These are the key additions for auto-assign integration
      # ========================================

      # Export functions (for datawizard_auto_assign.R)
      get_current_ui_values = get_current_ui_values,
      get_imputation_ui_config_for_export = get_imputation_ui_config_for_export,
      get_current_imputation_state = function() {
        tryCatch({
          # For backward compatibility, return UI values for export if possible
          ui_values <- get_current_ui_values()
          debug_log("Returned UI values for export", 2)
          return(ui_values)
        }, error = function(e) {
          debug_log(paste("Error getting UI values, using internal state:", e$message), 1)
          # Fallback to internal state
          return(list(
            imputation_method_select = last_imputation_method(),
            imputation_column_select = last_imputation_columns()
          ))
        })
      },

      # Enhanced version that prioritizes actual UI values
      get_current_imputation_state_for_export = get_current_imputation_state_for_export,

      # Import functions (for datawizard_integration.R)
      set_imputation_ui_config_from_import = set_imputation_ui_config_from_import,
      apply_ui_config = set_imputation_ui_config_from_import,  # Alias for consistency

      # ========================================
      # UI CONFIGURATION STATUS AND METADATA
      # ========================================

      # UI config status functions
      ui_config_applied = reactive({ !is.null(current_ui_config()) }),
      ui_config_source = reactive({ "module" }),  # Could be enhanced to track source
      current_ui_config = reactive({ current_ui_config() }),
      ui_config_update_active = reactive({ ui_config_update_active() }),

      # Configuration validation
      validate_ui_config = function(config) {
        if (is.null(config)) return(TRUE)
        if (!is.list(config)) return(FALSE)

        # Check required fields
        required_fields <- c("imputation_method_select", "imputation_column_select")
        has_required <- all(required_fields %in% names(config))

        # Validate method if present
        valid_method <- TRUE
        if (!is.null(config$imputation_method_select)) {
          valid_methods <- c("None", "left-censored", "Random forest", "MICE - CART")
          valid_method <- config$imputation_method_select %in% valid_methods
        }

        # Validate columns if present
        valid_columns <- TRUE
        if (!is.null(config$imputation_column_select)) {
          valid_columns <- is.character(config$imputation_column_select) || is.null(config$imputation_column_select)
        }

        return(has_required && valid_method && valid_columns)
      },

      # ========================================
      # ENHANCED STATE ACCESS FOR DEBUGGING
      # ========================================

      # Enhanced state access functions for debugging and monitoring
      get_full_state = function() {
        list(
          ui_state = get_current_ui_values(),
          processing_state = list(
            active = processing_active(),
            step = current_processing_step(),
            errors = length(processing_errors()),
            last_time = last_processing_time()
          ),
          imputation_state = list(
            applied = imputation_applied(),
            method = last_imputation_method(),
            columns = last_imputation_columns(),
            has_results = !is.null(imputation_result())
          ),
          analysis_state = list(
            has_missing_analysis = !is.null(missing_analysis_cache()),
            has_quality_assessment = !is.null(quality_assessment()),
            has_performance_metrics = !is.null(performance_metrics())
          )
        )
      }
    )
