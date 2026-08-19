# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Integration Adapters
# Purpose:
#   Encapsulate Auto-Assign adapter calls to sibling Data Wizard modules
#   (filtering, pivot, merge, imputation, ratios, batch effects, edit, basemean).
# Architectural Role:
#   Integration boundary layer preserving fallback/probing behavior.
# Responsibilities:
#   - Provide stable collect/apply/get adapter functions used by orchestrator.
#   - Keep defensive fallback order and retry semantics unchanged.
# Non-Responsibilities:
#   - Must not own module lifecycle wiring or observer registration.
# ============================================================================

create_auto_assign_integration_adapters <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("create_auto_assign_integration_adapters requires an environment context")
  }

  evalq({
    #' Collect current batch effects UI state for template saving
    collect_batch_effects_ui_state <- function(batch_module_ref = NULL) {
      return(collect_module_ui_state(
        batch_module_ref,
        "batch_effects",
        "get_current_ui_state",
        c("get_batch_state", "get_ui_state"),
        DEBUG_LEVEL
      ))
    }

    #' Collect current pivot UI state for template saving
    collect_pivot_ui_state <- function(pivot_module_ref = NULL) {
      debug_log("Starting pivot UI config collection", 2)

      tryCatch({
        # Try the pivot module reference first
        if (!is.null(pivot_module_ref)) {
          pivot_ref <- get_module_safely(pivot_module_ref, DEBUG_LEVEL)
          if (!is.null(pivot_ref)) {

            # Try the enhanced export function first
            if (!is.null(pivot_ref$get_pivot_ui_config_for_export) &&
                is.function(pivot_ref$get_pivot_ui_config_for_export)) {
              config <- tryCatch({
                pivot_ref$get_pivot_ui_config_for_export()
              }, error = function(e) {
                debug_log(paste("Error calling enhanced pivot export function:", e$message), 1)
                NULL
              })

              if (!is.null(config)) {
                debug_log("Collected pivot UI config via enhanced export function", 2)
                return(config)
              }
            }

            # Fallback to get_current_ui_values
            if (!is.null(pivot_ref$get_current_ui_values) &&
                is.function(pivot_ref$get_current_ui_values)) {
              config <- tryCatch({
                pivot_ref$get_current_ui_values()
              }, error = function(e) {
                debug_log(paste("Error calling pivot get_current_ui_values:", e$message), 1)
                NULL
              })

              if (!is.null(config)) {
                debug_log("Collected pivot UI config via current UI values", 2)
                return(config)
              }
            }

            # Further fallback to existing get_current_ui_state
            if (!is.null(pivot_ref$get_current_ui_state) &&
                is.function(pivot_ref$get_current_ui_state)) {
              config <- tryCatch({
                pivot_ref$get_current_ui_state()
              }, error = function(e) {
                debug_log(paste("Error calling pivot get_current_ui_state:", e$message), 1)
                NULL
              })

              if (!is.null(config)) {
                debug_log("Collected pivot UI config via existing state function", 2)
                return(config)
              }
            }
          }
        }

        debug_log("No pivot UI config could be collected", 2)
        return(NULL)

      }, error = function(e) {
        debug_log(paste("Error collecting pivot UI config:", e$message), 1)
        return(NULL)
      })
    }




    #' Apply pivot UI configuration with robust error handling
    apply_pivot_ui_config <- function(ui_pivot_config, pivot_module_ref = NULL, max_retries = 2) {
      if (is.null(ui_pivot_config)) return(TRUE)

      if (is.null(pivot_module_ref) && !is.null(pivot_module)) {
        pivot_module_ref <- pivot_module
      }

      for (attempt in seq_len(max_retries)) {
        if (attempt > 1) {
          debug_log(paste("Pivot config application retry attempt", attempt), 2)
          Sys.sleep(0.1)  # Brief pause before retry
        }

        result <- tryCatch({
          debug_log("Applying pivot UI configuration", 2)

          # Try the pivot module reference first
          if (!is.null(pivot_module_ref)) {
            pivot_ref <- get_module_safely(pivot_module_ref, DEBUG_LEVEL)
            if (!is.null(pivot_ref)) {

              # Try the enhanced import function first
              if (!is.null(pivot_ref$set_pivot_ui_config_from_import) &&
                  is.function(pivot_ref$set_pivot_ui_config_from_import)) {

                tryCatch({
                  pivot_ref$set_pivot_ui_config_from_import(ui_pivot_config)
                  debug_log("Applied pivot config via enhanced import function", 2)
                  return(TRUE)
                }, error = function(e) {
                  debug_log(paste("Error applying via enhanced import:", e$message), 1)
                  return(FALSE)
                })
              }

              # Fallback to apply_ui_config if available
              if (!is.null(pivot_ref$apply_ui_config) &&
                  is.function(pivot_ref$apply_ui_config)) {

                tryCatch({
                  result <- pivot_ref$apply_ui_config(ui_pivot_config)
                  if (isTRUE(result)) {
                    debug_log("Applied pivot config via apply_ui_config", 2)
                    return(TRUE)
                  } else {
                    debug_log("Apply_ui_config returned false", 2)
                    return(FALSE)
                  }
                }, error = function(e) {
                  debug_log(paste("Error applying via apply_ui_config:", e$message), 1)
                  return(FALSE)
                })
              }
            }
          }

          debug_log("No method available to apply pivot UI config", 2)
          return(FALSE)

        }, error = function(e) {
          debug_log(paste("Attempt", attempt, "failed for pivot config application:", e$message), 1)
          if (attempt == max_retries) {
            add_processing_log("pivot_config", "error", e$message)
            return(FALSE)
          }
        })

        if (result) {
          add_processing_log("pivot_config", "success", "Pivot configuration applied successfully")
          return(TRUE)
        }
      }

      debug_log("All attempts failed for pivot config application", 2)
      add_processing_log("pivot_config", "error", "All retry attempts failed")
      return(FALSE)
    }

    #' Get current pivot state for status display
    get_pivot_state <- function(pivot_module_ref = NULL) {
      tryCatch({
        if (!is.null(pivot_module_ref)) {
          pivot_ref <- get_module_safely(pivot_module_ref, DEBUG_LEVEL)
          if (!is.null(pivot_ref)) {

            # Try get_current_pivot_state_for_export if available (most comprehensive)
            if (!is.null(pivot_ref$get_current_pivot_state_for_export) &&
                is.function(pivot_ref$get_current_pivot_state_for_export)) {
              return(pivot_ref$get_current_pivot_state_for_export())
            }

            # Try get_current_ui_values if available
            if (!is.null(pivot_ref$get_current_ui_values) &&
                is.function(pivot_ref$get_current_ui_values)) {
              return(pivot_ref$get_current_ui_values())
            }

            # Basic state collection as fallback
            state <- list()

            if (!is.null(pivot_ref$pivot_data_dw) && is.reactive(pivot_ref$pivot_data_dw)) {
              tryCatch({
                state$pivot_data_dw <- pivot_ref$pivot_data_dw()
              }, error = function(e) {})
            }

            if (!is.null(pivot_ref$pivot_type_dw) && is.reactive(pivot_ref$pivot_type_dw)) {
              tryCatch({
                state$pivot_type_dw <- pivot_ref$pivot_type_dw()
              }, error = function(e) {})
            }

            if (!is.null(pivot_ref$pivot_options) && is.reactive(pivot_ref$pivot_options)) {
              tryCatch({
                state$pivot_options <- pivot_ref$pivot_options()
              }, error = function(e) {})
            }

            if (length(state) > 0) {
              return(state)
            }
          }
        }

        return(list(
          pivot_data_dw = "primary",
          pivot_type_dw = "wider",
          pivot_options = list()
        ))

      }, error = function(e) {
        debug_log(paste("Error getting pivot state:", e$message), 1)
        return(list(
          pivot_data_dw = "primary",
          pivot_type_dw = "wider",
          pivot_options = list(),
          error = e$message
        ))
      })
    }

    #' Collect current merge UI state for template saving
    collect_merge_ui_state <- function(merge_module_ref = NULL) {
      return(collect_module_ui_state(
        merge_module_ref,
        "merge",
        "get_current_ui_state",
        c("get_merge_state", "get_ui_state"),
        DEBUG_LEVEL
      ))
    }

    #' Collect current filter UI state for template saving
    collect_filter_ui_state <- function(filter_module_ref = NULL) {
      filter_state <- collect_module_ui_state(
        filter_module_ref,
        "filter",
        "get_current_filter_state",
        c("get_current_filter_state_basic", "get_filter_state"),
        DEBUG_LEVEL
      )

      if (is.null(filter_state)) {
        filter_state <- create_empty_structure("filter_state")
        filter_state$collection_status <- "fallback_empty"
      }

      return(filter_state)
    }

    #' Collect current ratio configurations for template saving
    collect_ratio_configurations <- function(ratios_module_ref = NULL) {
      tryCatch({
        ratio_configs <- collect_module_ui_state(
          ratios_module_ref,
          "ratios",
          "ratio_configurations",
          c("get_ratio_configurations", "get_configurations"),
          DEBUG_LEVEL
        )

        if (!is.null(ratio_configs) && is.data.frame(ratio_configs)) {
          required_cols <- c("Title", "Content", "Numerator", "Denominator", "Statistics")
          if (all(required_cols %in% names(ratio_configs))) {
            return(ratio_configs)
          }
        }

        return(create_empty_structure("ratio_configurations"))

      }, error = function(e) {
        debug_log(paste("Error collecting ratio configurations:", e$message), 1)
        return(create_empty_structure("ratio_configurations"))
      })
    }

    collect_basemean_configurations <- function(basemean_module) {
      tryCatch({
        if (is.null(basemean_module)) {
          debug_log("collect_basemean_configurations: Basemean module is NULL", 2)
          return(NULL)
        }

        debug_log("collect_basemean_configurations: Calling get_ui_config...", 2)

        config <- safe_module_call(
          basemean_module$get_ui_config,
          default_return = NULL,
          context = "basemean_ui_export"
        )

        if (!is.null(config) && length(config) > 0) {
          debug_log(paste("collect_basemean_configurations: Exporting", length(config), "fields"), 2)
          debug_log(paste("Basemean export fields:", paste(names(config), collapse = ", ")), 2)
          return(config)
        } else {
          debug_log("collect_basemean_configurations: Returned NULL or empty", 2)
          return(NULL)
        }
      }, error = function(e) {
        debug_log(paste("Error collecting Basemean UI configuration:", e$message), 1)
        return(NULL)
      })
    }



    #' Collect current edit operations for template saving
    collect_edit_operations <- function() {
      tryCatch({
        edit_ops <- collect_module_ui_state(
          edit_module,
          "edit",
          "pending_operations",
          c("get_operations_table", "get_operations"),
          DEBUG_LEVEL
        )

        if (!is.null(edit_ops) && is.data.frame(edit_ops)) {
          required_cols <- c("Operation", "Type", "Columns", "Parameters", "Description")
          if (all(required_cols %in% names(edit_ops))) {
            if (!"Executed" %in% names(edit_ops)) {
              edit_ops$Executed <- FALSE
            }
            return(edit_ops)
          }
        }

        return(create_empty_structure("operations"))

      }, error = function(e) {
        debug_log(paste("Error collecting edit operations:", e$message), 1)
        return(create_empty_structure("operations"))
      })
    }

    #' Collect current imputation UI configuration for export
    collect_imputation_ui_config <- function(imputation_module_ref = NULL) {
      debug_log("Starting imputation UI config collection", 2)

      tryCatch({
        # Debug: Check if we have the imputation module parameter
        if (is.null(imputation_module_ref)) {
          debug_log("No imputation_module_ref provided, checking imputation_module", 2)
          imputation_module_ref <- imputation_module
        }

        if (is.null(imputation_module_ref)) {
          debug_log("No imputation module available for UI config collection", 2)
          return(NULL)
        }

        debug_log("Imputation module reference found, attempting to get module", 2)

        # Get module safely
        imp_ref <- tryCatch({
          if (is.reactive(imputation_module_ref)) {
            imputation_module_ref()
          } else {
            imputation_module_ref
          }
        }, error = function(e) {
          debug_log(paste("Error getting imputation module reference:", e$message), 1)
          return(NULL)
        })

        if (is.null(imp_ref)) {
          debug_log("Imputation module reference is NULL", 2)
          return(NULL)
        }

        if (!is.list(imp_ref)) {
          debug_log("Imputation module reference is not a list", 2)
          return(NULL)
        }

        # Debug: List available functions
        available_functions <- names(imp_ref)[sapply(imp_ref, is.function)]
        debug_log(paste("Available functions in imputation module:", paste(available_functions, collapse = ", ")), 2)

        # Try the enhanced export function first
        if ("get_imputation_ui_config_for_export" %in% names(imp_ref)) {
          debug_log("Found get_imputation_ui_config_for_export function", 2)

          if (is.function(imp_ref$get_imputation_ui_config_for_export)) {
            config <- tryCatch({
              imp_ref$get_imputation_ui_config_for_export()
            }, error = function(e) {
              debug_log(paste("Error calling get_imputation_ui_config_for_export:", e$message), 1)
              return(NULL)
            })

            if (!is.null(config)) {
              debug_log("Successfully collected imputation UI config via enhanced export function", 2)
              return(config)
            } else {
              debug_log("Enhanced export function returned NULL", 2)
            }
          } else {
            debug_log("get_imputation_ui_config_for_export is not a function", 2)
          }
        } else {
          debug_log("get_imputation_ui_config_for_export function not found", 2)
        }

        # Try get_current_ui_values function
        if ("get_current_ui_values" %in% names(imp_ref)) {
          debug_log("Found get_current_ui_values function", 2)

          if (is.function(imp_ref$get_current_ui_values)) {
            config <- tryCatch({
              imp_ref$get_current_ui_values()
            }, error = function(e) {
              debug_log(paste("Error calling get_current_ui_values:", e$message), 1)
              return(NULL)
            })

            if (!is.null(config)) {
              debug_log("Successfully collected imputation UI config via get_current_ui_values", 2)
              return(config)
            } else {
              debug_log("get_current_ui_values function returned NULL", 2)
            }
          } else {
            debug_log("get_current_ui_values is not a function", 2)
          }
        } else {
          debug_log("get_current_ui_values function not found", 2)
        }

        # Try basic state collection as last resort
        debug_log("Attempting basic state collection as fallback", 2)

        basic_config <- list()

        if ("last_method" %in% names(imp_ref) && is.reactive(imp_ref$last_method)) {
          tryCatch({
            basic_config$imputation_method_select <- imp_ref$last_method()
            debug_log(paste("Got method from last_method:", basic_config$imputation_method_select), 2)
          }, error = function(e) {
            debug_log(paste("Error getting last_method:", e$message), 2)
          })
        }

        if ("last_columns" %in% names(imp_ref) && is.reactive(imp_ref$last_columns)) {
          tryCatch({
            basic_config$imputation_column_select <- imp_ref$last_columns()
            debug_log(paste("Got columns from last_columns:", paste(basic_config$imputation_column_select, collapse = ", ")), 2)
          }, error = function(e) {
            debug_log(paste("Error getting last_columns:", e$message), 2)
          })
        }

        if (length(basic_config) > 0) {
          debug_log("Successfully collected basic imputation config", 2)
          return(basic_config)
        }

        # Fallback to UI_config parameter
        if (!is.null(UI_config)) {
          debug_log("Trying UI_config parameter as fallback", 2)

          if (is.reactive(UI_config)) {
            config <- UI_config()
            if (!is.null(config)) {
              debug_log("Successfully collected imputation UI config from reactive parameter", 2)
              return(config)
            }
          } else if (is.list(UI_config)) {
            debug_log("Successfully collected imputation UI config from list parameter", 2)
            return(UI_config)
          }
        }

        debug_log("No imputation UI config could be collected - all methods failed", 2)
        return(NULL)

      }, error = function(e) {
        debug_log(paste("Error collecting imputation UI config:", e$message), 1)
        return(NULL)
      })
    }

    # ========================================
    # Enhanced Application Functions with Retry Logic
    # ========================================

    #' Apply filter template to UI with retry logic
    apply_filter_template <- function(filter_state, filter_module_ref = NULL, max_retries = 2) {
      if (is.null(filter_state)) return(TRUE)

      for (attempt in seq_len(max_retries)) {
        if (attempt > 1) {
          debug_log(paste("Filter template application retry attempt", attempt), 2)
          Sys.sleep(0.1)  # Brief pause before retry
        }

        result <- tryCatch({
          template_loading_in_progress(TRUE)

          if (!is.null(filter_module_ref)) {
            filter_ref <- get_module_safely(filter_module_ref, DEBUG_LEVEL)

            # Try enhanced version first
            if (!is.null(filter_ref) && !is.null(filter_ref$apply_filter_state) && is.function(filter_ref$apply_filter_state)) {
              success <- tryCatch({
                filter_ref$apply_filter_state(filter_state)
              }, error = function(e) {
                debug_log(paste("apply_filter_state error:", e$message), 1)
                return(NULL)
              })

              if (!is.null(success) && length(success) > 0 && safe_is_true(success)) {
                debug_log("Filter template loaded successfully", 2)
                template_loading_in_progress(FALSE)

                last_import_info(list(
                  timestamp = Sys.time(),
                  method = "enhanced_apply_filter_state",
                  status = "success",
                  filter_components = names(filter_state),
                  attempt = attempt
                ))

                return(TRUE)
              }
            }

            # Try basic version as fallback
            if (!is.null(filter_ref) && !is.null(filter_ref$apply_filter_state_basic) && is.function(filter_ref$apply_filter_state_basic)) {
              success <- tryCatch({
                filter_ref$apply_filter_state_basic(filter_state)
              }, error = function(e) {
                debug_log(paste("apply_filter_state_basic error:", e$message), 1)
                return(NULL)
              })

              if (!is.null(success) && length(success) > 0 && safe_is_true(success)) {
                debug_log("Filter template loaded via fallback", 2)
                template_loading_in_progress(FALSE)

                last_import_info(list(
                  timestamp = Sys.time(),
                  method = "basic_apply_filter_state",
                  status = "success",
                  filter_components = names(filter_state),
                  attempt = attempt
                ))

                return(TRUE)
              }
            }
          }

          if (attempt >= max_retries) {
            # Fallback: store for parent module
            template_loading_in_progress(FALSE)

            last_import_info(list(
              timestamp = Sys.time(),
              method = "fallback_storage",
              status = "partial",
              filter_components = names(filter_state),
              attempt = attempt
            ))

            debug_log("Filter template applied via fallback storage", 2)
            return(TRUE)
          }

          return(NULL)  # Signal retry

        }, error = function(e) {
          if (attempt >= max_retries) {
            debug_log(paste("Error applying filter template:", e$message), 1)
            template_loading_in_progress(FALSE)

            last_import_info(list(
              timestamp = Sys.time(),
              method = "error",
              status = "failed",
              error = e$message,
              attempt = attempt
            ))

            return(FALSE)
          }
          return(NULL)  # Signal retry
        })

        if (!is.null(result)) {
          return(result)
        }
      }

      # All retries failed
      template_loading_in_progress(FALSE)
      debug_log("All retry attempts for filter template application failed", 2)
      return(FALSE)
    }

    #' Apply ratio configurations template with enhanced error handling
    apply_ratio_configurations <- function(configurations_table) {
      if (is.null(configurations_table) || nrow(configurations_table) == 0) return(TRUE)

      tryCatch({
        required_cols <- c("Title", "Content", "Numerator", "Denominator", "Statistics")
        missing_cols <- setdiff(required_cols, names(configurations_table))
        if (length(missing_cols) > 0) {
          debug_log(paste("Ratio configurations invalid: missing columns", paste(missing_cols, collapse = ", ")), 1)
          add_processing_log("ratio_configurations", "error", paste("Missing columns:", paste(missing_cols, collapse = ", ")))
          return(FALSE)
        }

        # Try to apply via ratios module
        if (!is.null(ratios_module)) {
          ratios_ref <- get_module_safely(ratios_module, DEBUG_LEVEL)

          if (!is.null(ratios_ref) && !is.null(ratios_ref$load_ratio_configurations) && is.function(ratios_ref$load_ratio_configurations)) {
            success <- ratios_ref$load_ratio_configurations(configurations_table)
            if (safe_is_true(success)) {
              debug_log(paste("Ratio configurations loaded:", nrow(configurations_table), "configurations"), 2)
              add_processing_log("ratio_configurations", "success", paste("Loaded", nrow(configurations_table), "configurations"))
              return(TRUE)
            }
          }
        }

        debug_log(paste("Ratio configurations available but could not be loaded automatically:", nrow(configurations_table), "configurations"), 2)
        add_processing_log("ratio_configurations", "warning", "Could not load automatically")
        return(FALSE)

      }, error = function(e) {
        debug_log(paste("Error applying ratio configurations:", e$message), 1)
        add_processing_log("ratio_configurations", "error", e$message)
        return(FALSE)
      })
    }

    #' Apply edit operations template with enhanced error handling
    apply_edit_operations <- function(operations_table) {
      if (is.null(operations_table) || nrow(operations_table) == 0) return(TRUE)

      tryCatch({
        required_cols <- c("Operation", "Type", "Columns", "Parameters", "Description")
        missing_cols <- setdiff(required_cols, names(operations_table))
        if (length(missing_cols) > 0) {
          debug_log(paste("Edit operations invalid: missing columns", paste(missing_cols, collapse = ", ")), 1)
          add_processing_log("edit_operations", "error", paste("Missing columns:", paste(missing_cols, collapse = ", ")))
          return(FALSE)
        }

        if (!"Executed" %in% names(operations_table)) {
          operations_table$Executed <- FALSE
        } else {
          operations_table$Executed <- FALSE
        }

        if (!is.null(edit_module)) {
          edit_ref <- get_module_safely(edit_module, DEBUG_LEVEL)

          if (!is.null(edit_ref) && !is.null(edit_ref$load_operations_table) && is.function(edit_ref$load_operations_table)) {
            tryCatch({
              success <- edit_ref$load_operations_table(operations_table)
              if (safe_is_true(success)) {
                debug_log(paste("Edit operations loaded:", nrow(operations_table), "operations"), 2)
                add_processing_log("edit_operations", "success", paste("Loaded", nrow(operations_table), "operations"))
                return(TRUE)
              } else {
                debug_log("Edit module rejected operations table", 2)
                add_processing_log("edit_operations", "error", "Module rejected operations table")
                return(FALSE)
              }
            }, error = function(e) {
              debug_log(paste("Error calling edit module load function:", e$message), 1)
              add_processing_log("edit_operations", "error", e$message)
              return(FALSE)
            })
          } else {
            debug_log("Edit module does not provide load_operations_table function", 2)
            add_processing_log("edit_operations", "warning", "No load function available")
          }
        } else {
          debug_log("No edit module configured", 2)
        }

        debug_log(paste("Edit operations available but could not be loaded automatically:", nrow(operations_table), "operations"), 2)
        add_processing_log("edit_operations", "warning", "Could not load automatically")
        return(FALSE)

      }, error = function(e) {
        debug_log(paste("Error applying edit operations:", e$message), 1)
        add_processing_log("edit_operations", "error", e$message)
        return(FALSE)
      })
    }

    #' Apply loaded imputation UI configuration
    apply_imputation_ui_config <- function(ui_imputation_config, imputation_module_ref = NULL, max_retries = 2) {
      if (is.null(ui_imputation_config)) return(TRUE)

      if (is.null(imputation_module_ref) && !is.null(imputation_module)) {
        imputation_module_ref <- imputation_module
      }

      for (attempt in seq_len(max_retries)) {
        if (attempt > 1) {
          debug_log(paste("Imputation config application retry attempt", attempt), 2)
          Sys.sleep(0.1)  # Brief pause before retry
        }

        result <- tryCatch({
          debug_log("Applying imputation UI configuration", 2)

          # Try the imputation module reference first
          if (!is.null(imputation_module_ref)) {
            imp_ref <- get_module_safely(imputation_module_ref, DEBUG_LEVEL)
            if (!is.null(imp_ref)) {

              # Try the new enhanced import function first
              if (!is.null(imp_ref$set_imputation_ui_config_from_import) &&
                  is.function(imp_ref$set_imputation_ui_config_from_import)) {

                tryCatch({
                  imp_ref$set_imputation_ui_config_from_import(ui_imputation_config)
                  debug_log("Applied imputation config via enhanced import function", 2)
                  return(TRUE)
                }, error = function(e) {
                  debug_log(paste("Error applying via enhanced import:", e$message), 1)
                  return(FALSE)
                })
              }

              # Fallback to apply_ui_config if available
              if (!is.null(imp_ref$apply_ui_config) &&
                  is.function(imp_ref$apply_ui_config)) {

                tryCatch({
                  imp_ref$apply_ui_config(ui_imputation_config)
                  debug_log("Applied imputation config via apply_ui_config", 2)
                  return(TRUE)
                }, error = function(e) {
                  debug_log(paste("Error applying via apply_ui_config:", e$message), 1)
                  return(FALSE)
                })
              }
            }
          }

          debug_log("No method available to apply imputation UI config", 2)
          return(FALSE)

        }, error = function(e) {
          debug_log(paste("Attempt", attempt, "failed for imputation config application:", e$message), 1)
          if (attempt == max_retries) {
            add_processing_log("imputation_config", "error", e$message)
            return(FALSE)
          }
        })

        if (result) {
          add_processing_log("imputation_config", "success", "Configuration applied successfully")
          return(TRUE)
        }
      }

      debug_log("All attempts failed for imputation config application", 2)
      add_processing_log("imputation_config", "error", "All retry attempts failed")
      return(FALSE)
    }



    # ========================================
    # ENHANCED: Imputation State Collection Function
    # ========================================

    #' Get current imputation state for status display
    get_imputation_state <- function(imputation_module_ref = NULL) {
      tryCatch({
        if (!is.null(imputation_module_ref)) {
          imp_ref <- get_module_safely(imputation_module_ref, DEBUG_LEVEL)
          if (!is.null(imp_ref)) {

            # Try get_full_state if available (most comprehensive)
            if (!is.null(imp_ref$get_full_state) && is.function(imp_ref$get_full_state)) {
              return(imp_ref$get_full_state())
            }

            # Try get_current_imputation_state_for_export if available
            if (!is.null(imp_ref$get_current_imputation_state_for_export) &&
                is.function(imp_ref$get_current_imputation_state_for_export)) {
              return(imp_ref$get_current_imputation_state_for_export())
            }

            # Basic state collection as fallback
            state <- list()

            if (!is.null(imp_ref$imputation_applied) && is.reactive(imp_ref$imputation_applied)) {
              state$applied <- imp_ref$imputation_applied()
            }

            if (!is.null(imp_ref$last_method) && is.reactive(imp_ref$last_method)) {
              state$method <- imp_ref$last_method()
            }

            if (!is.null(imp_ref$last_columns) && is.reactive(imp_ref$last_columns)) {
              state$columns <- imp_ref$last_columns()
            }

            if (length(state) > 0) {
              return(state)
            }
          }
        }

        return(list(
          applied = FALSE,
          method = "None",
          columns = character(0)
        ))

      }, error = function(e) {
        debug_log(paste("Error getting imputation state:", e$message), 1)
        return(list(
          applied = FALSE,
          method = "None",
          columns = character(0),
          error = e$message
        ))
      })
    }



    list(
      collect_batch_effects_ui_state = collect_batch_effects_ui_state,
      collect_pivot_ui_state = collect_pivot_ui_state,
      apply_pivot_ui_config = apply_pivot_ui_config,
      get_pivot_state = get_pivot_state,
      collect_merge_ui_state = collect_merge_ui_state,
      collect_filter_ui_state = collect_filter_ui_state,
      collect_ratio_configurations = collect_ratio_configurations,
      collect_basemean_configurations = collect_basemean_configurations,
      collect_edit_operations = collect_edit_operations,
      collect_imputation_ui_config = collect_imputation_ui_config,
      apply_filter_template = apply_filter_template,
      apply_ratio_configurations = apply_ratio_configurations,
      apply_edit_operations = apply_edit_operations,
      apply_imputation_ui_config = apply_imputation_ui_config,
      get_imputation_state = get_imputation_state
    )
  }, envir = ctx)
}
