# ============================================================================
# MiraProt Imputation Server Fragment: datawizard_imputation_state_configuration.R
# Purpose: Owns module-local reactive state, data access, UI configuration, and choice refresh wiring.
# Loading: Source in declared order with local = TRUE from modImputationServer().
# Invariant: This fragment shares the active moduleServer frame; do not source standalone.
# ============================================================================

    # ========================================
    # Enhanced Debug Management
    # ========================================

    # Helper function for controlled debug output
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "IMPUTATION", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ IMPUTATION ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Imputation module server starting", 1)

    # ========================================
    # CRITICAL: Load utils.R with proper error handling
    # ========================================

    tryCatch({
      if (file.exists("R/utils.R")) {
        source("R/utils.R", local = TRUE)
        debug_log("utils.R loaded successfully", 2)
      } else {
        debug_log("utils.R not found, trying alternative paths", 1)
        # Try alternative paths
        possible_paths <- c("../R/utils.R", "../../R/utils.R", "./utils.R")
        loaded <- FALSE
        for (path in possible_paths) {
          if (file.exists(path)) {
            source(path, local = TRUE)
            debug_log(paste("utils.R loaded from:", path), 2)
            loaded <- TRUE
            break
          }
        }
        if (!loaded) {
          stop("Could not find utils.R in any expected location")
        }
      }
    }, error = function(e) {
      debug_log(paste("CRITICAL ERROR loading utils.R:", e$message), 1)
      showNotification("Error loading utility functions - imputation may not work correctly", type = "error", duration = 10)
    })

    # ========================================
    # Safe helper function
    # ========================================

    safe_is_true <- function(x) {
      if (is.null(x) || length(x) == 0) return(FALSE)
      if (is.logical(x)) return(isTRUE(x[1]))
      if (is.numeric(x)) return(x[1] > 0)
      if (is.character(x)) return(tolower(x[1]) %in% c("true", "t", "yes", "y", "1"))
      if (is.list(x)) {
        if (!is.null(x$success)) return(safe_is_true(x$success))
        if (!is.null(x$data)) return(TRUE)
        return(length(x) > 0)
      }
      return(FALSE)
    }

    # ========================================
    # Enhanced Reactive Values
    # ========================================

    # Core imputation state
    imputation_result <- reactiveVal(NULL)
    imputation_matrix <- reactiveVal(NULL)
    imputation_applied <- reactiveVal(FALSE)
    last_imputation_method <- reactiveVal("None")
    last_imputation_columns <- reactiveVal(character(0))
    original_data_backup <- reactiveVal(NULL)

    # Processing state management
    processing_active <- reactiveVal(FALSE)
    current_processing_step <- reactiveVal("")
    processing_errors <- reactiveVal(list())
    processing_history <- reactiveVal(list())
    last_processing_time <- reactiveVal(NULL)

    # Advanced analysis caches
    missing_analysis_cache <- reactiveVal(NULL)
    quality_assessment <- reactiveVal(NULL)
    performance_metrics <- reactiveVal(NULL)
    method_recommendations <- reactiveVal(NULL)

    # UI configuration management
    ui_config_update_active <- reactiveVal(FALSE)
    current_ui_config <- reactiveVal(NULL)

    # Logging system
    log_container <- reactiveVal(list())
    performance_log <- reactiveVal(list())
    error_log <- reactiveVal(list())
    total_imputed_count <- reactiveVal(0)

    # Track handled apply-button clicks to avoid replaying rapid queued clicks
    # after a long-running imputation has finished.
    last_handled_apply_click <- reactiveVal(0L)

    # ========================================
    # Enhanced Data Access Functions
    # ========================================

    get_current_data <- function() {
      tryCatch({
        # Priority: get_data function > data reactive
        if (!is.null(get_data) && is.function(get_data)) {
          result <- get_data()
          debug_log(paste("Data retrieved via get_data() - dimensions:",
                          if (is.null(result)) "NULL" else paste(nrow(result), "x", ncol(result))), 2)
          return(result)
        } else if (!is.null(data) && is.reactive(data)) {
          result <- data()
          debug_log(paste("Data retrieved via data() reactive - dimensions:",
                          if (is.null(result)) "NULL" else paste(nrow(result), "x", ncol(result))), 2)
          return(result)
        }
        debug_log("No data source available", 1)
        return(NULL)
      }, error = function(e) {
        debug_log(paste("Error accessing current data:", e$message), 1)
        return(NULL)
      })
    }

    set_current_data <- function(new_data) {
      tryCatch({
        success <- FALSE
        if (!is.null(set_data) && is.function(set_data)) {
          success <- set_data(new_data)
          debug_log(paste("Data update via set_data():", if (safe_is_true(success)) "SUCCESS" else "FAILED"),
                    if (safe_is_true(success)) 2 else 1)
        } else if (!is.null(data) && is.reactive(data)) {
          # Try to update reactive data if possible
          if (!is.null(data)) {
            data(new_data)  # This may fail if data is read-only
            success <- TRUE
            debug_log("Data updated via data() reactive", 2)
          }
        } else {
          debug_log("No data update method available", 1)
        }
        return(safe_is_true(success))
      }, error = function(e) {
        debug_log(paste("Error setting current data:", e$message), 1)
        return(FALSE)
      })
    }

    # ========================================
    # Enhanced UI Configuration Management
    # ========================================

    # Function to safely get UI_config value
    get_ui_config <- reactive({
      tryCatch({
        if (is.null(UI_config)) {
          return(NULL)
        } else if (is.reactive(UI_config)) {
          return(UI_config())
        } else if (is.list(UI_config)) {
          return(UI_config)
        } else {
          debug_log("Invalid UI_config type - ignoring", 1)
          return(NULL)
        }
      }, error = function(e) {
        debug_log(paste("Error accessing UI_config:", e$message), 1)
        return(NULL)
      })
    })

    # Update UI elements based on UI_config changes
    observeEvent(get_ui_config(), {
      tryCatch({
        config <- get_ui_config()
        if (!is.null(config)) {
          debug_log("UI_config update received", 2)

          # Set flag to prevent conflicts
          ui_config_update_active(TRUE)

          # Update imputation method if specified
          if (!is.null(config$imputation_method_select)) {
            method_value <- config$imputation_method_select
            valid_choices <- c("None", "left-censored", "Random forest", "MICE - CART")
            if (method_value %in% valid_choices) {
              updateSelectInput(
                session,
                "imputation_method_select",
                selected = method_value
              )
              debug_log(paste("Method updated to:", method_value), 2)
            }
          }

          # Update column selection if specified
          if (!is.null(config$imputation_column_select)) {
            column_values <- safe_list_check(config$imputation_column_select, character(0))
            valid_choices <- valid_imputation_choices_cache()
            if (length(valid_choices) == 0) {
              current_data <- isolate(get_current_data())
              current_metadata <- tryCatch(data_def(), error = function(e) NULL)
              valid_choices <- get_valid_imputation_choices(current_data, current_metadata)
            }
            valid_column_values <- intersect_imputation_selections(
              column_values,
              valid_choices,
              "UI_config"
            )
            updateSelectInput(
              session,
              "imputation_column_select",
              choices = valid_choices,
              selected = valid_column_values
            )
            config$imputation_column_select <- valid_column_values
            debug_log(
              paste(
                "Columns updated to:",
                if (length(valid_column_values) > 0) paste(valid_column_values, collapse = ", ") else "<none>"
              ),
              2
            )
          }

          # Store current config
          current_ui_config(config)

          # Clear flag after brief delay
          invalidateLater(500, session)
          ui_config_update_active(FALSE)
        }
      }, error = function(e) {
        debug_log(paste("Error updating UI from config:", e$message), 1)
        ui_config_update_active(FALSE)
      })
    }, ignoreInit = TRUE)

    # ========================================
    # Enhanced UI Configuration Export Functions
    # ========================================

    # Safe helper functions for UI value extraction
    safe_character_check <- function(value, default_val = "") {
      if (is.null(value) || length(value) == 0) return(default_val)
      if (is.character(value)) return(value[1])
      return(as.character(value[1]))
    }

    safe_logical_check <- function(value, default_val = FALSE) {
      if (is.null(value) || length(value) == 0) return(default_val)
      if (is.logical(value)) return(value[1])
      return(isTRUE(value))
    }

    safe_list_check <- function(value, default_val = character(0)) {
      if (is.null(value) || length(value) == 0) return(default_val)
      if (is.character(value)) return(value)
      return(as.character(value))
    }

    # Track the current data/metadata column signature so option lists are refreshed
    # when the usable imputation columns change underneath an imported or remembered
    # selection.
    last_imputation_choice_signature <- reactiveVal(NULL)
    valid_imputation_choices_cache <- reactiveVal(character(0))

    build_imputation_column_signature <- function(current_data, current_metadata) {
      revision_sig <- tryCatch(data_revision_signature(), error = function(e) NULL)
      if (!is.null(revision_sig)) {
        return(paste(unlist(revision_sig, use.names = TRUE), collapse = "|"))
      }
      tryCatch({
        data_signature <- if (is.null(current_data)) {
          "no-data"
        } else {
          paste(
            paste(names(current_data), vapply(current_data, function(col) class(col)[1], character(1)), sep = ":"),
            collapse = "|"
          )
        }

        metadata_signature <- if (is.null(current_metadata) ||
                                  !all(c("Column", "Content") %in% names(current_metadata))) {
          "no-metadata"
        } else {
          paste(
            paste(as.character(current_metadata$Column), as.character(current_metadata$Content), sep = ":"),
            collapse = "|"
          )
        }

        paste(data_signature, metadata_signature, sep = "||")
      }, error = function(e) {
        debug_log(paste("Error building imputation column signature:", e$message), 1)
        paste("signature-error", Sys.time())
      })
    }

    get_valid_imputation_choices <- function(current_data = NULL, current_metadata = NULL) {
      tryCatch({
        if (is.null(current_data)) current_data <- get_current_data()
        if (is.null(current_metadata)) current_metadata <- data_def()

        if (is.null(current_metadata) || is.null(current_data) ||
            !all(c("Column", "Content") %in% names(current_metadata))) {
          return(character(0))
        }

        allowed_content_types <- c(
          "Raw Abundance",
          "Normalized Abundance",
          "Batch Corrected Abundance",
          "Imputed Raw Abundance",
          "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
          "Imputed Batch Corrected Abundance",
          "Batch Corrected Normalized Abundance",
          "Batch Corrected Raw Abundance"
        )
        abundance_content_types <- unique(
          current_metadata$Content[current_metadata$Content %in% allowed_content_types]
        )
        valid_content_types <- character()

        for (content_type in abundance_content_types) {
          if (content_type == "" || is.na(content_type)) next

          content_indices <- which(current_metadata$Content == content_type)
          if (length(content_indices) > 0) {
            col_names <- current_metadata$Column[content_indices]
            existing_cols <- intersect(col_names, names(current_data))

            has_numeric <- FALSE
            for (col_name in existing_cols) {
              col_data <- current_data[[col_name]]
              if (is.numeric(col_data)) {
                has_numeric <- TRUE
                break
              }
            }

            if (has_numeric) {
              valid_content_types <- c(valid_content_types, content_type)
            }
          }
        }

        unique(valid_content_types)
      }, error = function(e) {
        debug_log(paste("Error computing valid imputation choices:", e$message), 1)
        character(0)
      })
    }

    intersect_imputation_selections <- function(selections, valid_choices, source = "selection") {
      selections <- safe_list_check(selections, character(0))
      if (length(selections) == 0) return(character(0))

      valid_selections <- intersect(selections, valid_choices)
      dropped_selections <- setdiff(selections, valid_selections)
      if (length(dropped_selections) > 0) {
        debug_log(
          paste(
            "Dropped stale", source, "imputation selection(s):",
            paste(dropped_selections, collapse = ", ")
          ),
          1
        )
      }

      valid_selections
    }

    refresh_current_ui_config_columns <- function(valid_columns) {
      tryCatch({
        config <- current_ui_config()
        if (!is.null(config) && is.list(config) &&
            !identical(safe_list_check(config$imputation_column_select, character(0)), valid_columns)) {
          config$imputation_column_select <- valid_columns
          current_ui_config(config)
        }
      }, error = function(e) {
        debug_log(paste("Error refreshing current_ui_config columns:", e$message), 1)
      })
    }

    # Function to get current UI values directly from inputs (adapted from filtering)
    get_current_ui_values <- function() {
      tryCatch({
        ui_values <- list(
          imputation_method_select = safe_character_check(input$imputation_method_select, "None"),
          imputation_column_select = safe_list_check(input$imputation_column_select, character(0))
        )

        debug_log("Extracted UI values for export", 2)
        return(ui_values)

      }, error = function(e) {
        debug_log(paste("Error extracting UI values:", e$message), 1)

        # Fallback to internal state
        fallback_values <- list(
          imputation_method_select = last_imputation_method(),
          imputation_column_select = last_imputation_columns()
        )

        return(fallback_values)
      })
    }

    # Function to export UI configuration with robust error handling
    get_imputation_ui_config_for_export <- function() {
      tryCatch({
        config <- list(
          # Standard UI settings with robust extraction
          imputation_method_select = safe_character_check(input$imputation_method_select, "None"),
          imputation_column_select = safe_list_check(input$imputation_column_select, character(0)),

          # Metadata for tracking
          export_timestamp = Sys.time(),
          export_version = "enhanced_v1.0",

          # Additional state information
          imputation_applied = imputation_applied(),
          last_method = last_imputation_method(),
          last_columns = last_imputation_columns()
        )

        debug_log("Imputation UI configuration exported successfully", 2)
        return(config)

      }, error = function(e) {
        debug_log(paste("Error exporting imputation UI config:", e$message), 1)

        # Return minimal safe config on error
        return(list(
          imputation_method_select = "None",
          imputation_column_select = character(0),
          export_error = TRUE,
          error_message = e$message
        ))
      })
    }

    # Function to get current imputation state for export with UI priority
    get_current_imputation_state_for_export <- function() {
      # Use isolate to avoid reactive context issues
      isolate({
        tryCatch({
          ui_values <- list(
            method = safe_character_check(input$imputation_method_select, "None"),
            columns = safe_list_check(input$imputation_column_select, character(0)),
            applied = imputation_applied(),
            last_processing_time = last_processing_time(),
            has_results = !is.null(imputation_result())
          )

          debug_log("Exported current imputation state", 2)
          return(ui_values)

        }, error = function(e) {
          debug_log(paste("Error exporting current state:", e$message), 1)

          # Fallback to internal state
          return(list(
            method = last_imputation_method(),
            columns = last_imputation_columns(),
            applied = imputation_applied()
          ))
        })
      })
    }

    # ========================================
    # Enhanced UI Configuration Import Functions
    # ========================================

    # Function to apply UI configuration from import with robust error handling
    set_imputation_ui_config_from_import <- function(config) {
      tryCatch({
        if (is.null(config) || !is.list(config)) {
          debug_log("Invalid configuration for import - using defaults", 1)
          return()
        }

        if (isTRUE(config$export_error)) {
          debug_log("Imported configuration has export errors - using partial import", 2)
        }

        # Safe import with validation for method
        if (!is.null(config$imputation_method_select)) {
          validated_method <- safe_character_check(config$imputation_method_select, "None")
          valid_methods <- c("None", "left-censored", "Random forest", "MICE - CART")

          if (validated_method %in% valid_methods) {
            tryCatch({
              updateSelectInput(session, "imputation_method_select", selected = validated_method)
              debug_log(paste("Updated imputation method to:", validated_method), 2)
            }, error = function(e) {
              debug_log(paste("Error updating imputation method:", e$message), 1)
            })
          } else {
            debug_log(paste("Invalid method in import config:", validated_method), 1)
          }
        }

        # Safe import with validation for columns
        if (!is.null(config$imputation_column_select)) {
          validated_columns <- safe_list_check(config$imputation_column_select, character(0))
          valid_choices <- valid_imputation_choices_cache()
          if (length(valid_choices) == 0) {
            current_data <- get_current_data()
            current_metadata <- tryCatch(data_def(), error = function(e) NULL)
            valid_choices <- get_valid_imputation_choices(current_data, current_metadata)
          }
          valid_columns <- intersect_imputation_selections(
            validated_columns,
            valid_choices,
            "imported UI config"
          )

          tryCatch({
            updateSelectInput(
              session,
              "imputation_column_select",
              choices = valid_choices,
              selected = valid_columns
            )
            refresh_current_ui_config_columns(valid_columns)
            debug_log(
              paste(
                "Updated imputation columns from import:",
                if (length(valid_columns) > 0) paste(valid_columns, collapse = ", ") else "<none>"
              ),
              2
            )
          }, error = function(e) {
            debug_log(paste("Error updating columns from import:", e$message), 1)
          })
        }

        debug_log("Imputation UI configuration imported successfully", 2)

      }, error = function(e) {
        debug_log(paste("Error importing imputation UI config:", e$message), 1)
        showNotification(
          "Some imputation settings could not be imported. Default values will be used.",
          type = "warning",
          duration = 4
        )
      })
    }

    # ========================================
    # Enhanced Observer for UI Config Updates
    # ========================================

    # Update the existing UI config observer to handle import properly
    observeEvent(get_ui_config(), {
      tryCatch({
        config <- get_ui_config()
        if (!is.null(config)) {
          debug_log("UI_config update received", 2)

          # Set flag to prevent conflicts
          ui_config_update_active(TRUE)

          # Use the new import function for consistency
          set_imputation_ui_config_from_import(config)

          # Store current config after dropping stale imported selections.
          if (!is.null(config$imputation_column_select)) {
            valid_choices <- valid_imputation_choices_cache()
            if (length(valid_choices) == 0) {
              current_data <- get_current_data()
              current_metadata <- tryCatch(data_def(), error = function(e) NULL)
              valid_choices <- get_valid_imputation_choices(current_data, current_metadata)
            }
            config$imputation_column_select <- intersect_imputation_selections(
              config$imputation_column_select,
              valid_choices,
              "current_ui_config"
            )
          }
          current_ui_config(config)

          # Reset flag after processing
          ui_config_update_active(FALSE)
        }
      }, error = function(e) {
        debug_log(paste("Error processing UI_config update:", e$message), 1)
        ui_config_update_active(FALSE)
      })
    }, ignoreInit = TRUE)

    # ========================================
    # Enhanced Column Population
    # ========================================

    refresh_imputation_choices <- function() {
      if (!isTRUE(initialized())) return(invisible(FALSE))
      tryCatch({
        current_metadata <- isolate(data_def())
        current_data <- isolate(get_current_data())
        req(is.data.frame(current_data), is.data.frame(current_metadata))
        if (datawizard_metadata_assignment_pending(get0("rv", inherits = TRUE, ifnotfound = NULL)) &&
            !datawizard_metadata_meaningful_ready(get0("rv", inherits = TRUE, ifnotfound = NULL))) {
          debug_log("Metadata assignment pending; deferring imputation choices", 2)
          return()
        }
        current_signature <- build_imputation_column_signature(current_data, current_metadata)
        previous_signature <- last_imputation_choice_signature()
        signature_changed <- is.null(previous_signature) || !identical(previous_signature, current_signature)

        if (is.null(current_metadata) || is.null(current_data)) {
          debug_log("No metadata or data available for column population", 2)
          valid_imputation_choices_cache(character(0))
          last_imputation_choice_signature(current_signature)

          remembered_columns <- safe_list_check(last_imputation_columns(), character(0))
          if (length(remembered_columns) > 0) {
            debug_log(
              paste(
                "Clearing stale remembered imputation selection(s):",
                paste(remembered_columns, collapse = ", ")
              ),
              1
            )
            last_imputation_columns(character(0))
          }

          refresh_current_ui_config_columns(character(0))
          updateSelectInput(session, "imputation_column_select", choices = character(0), selected = character(0))
          return()
        }

        valid_content_types <- get_valid_imputation_choices(current_data, current_metadata)
        valid_imputation_choices_cache(valid_content_types)

        if (signature_changed) {
          last_imputation_choice_signature(current_signature)
          debug_log("Data column signature changed; recomputed imputation choices", 2)
        }

        remembered_columns <- safe_list_check(last_imputation_columns(), character(0))
        valid_remembered_columns <- intersect_imputation_selections(
          remembered_columns,
          valid_content_types,
          "remembered"
        )
        if (!identical(remembered_columns, valid_remembered_columns)) {
          last_imputation_columns(valid_remembered_columns)
        }

        configured_columns <- character(0)
        config <- current_ui_config()
        if (!is.null(config) && !is.null(config$imputation_column_select)) {
          configured_columns <- intersect_imputation_selections(
            config$imputation_column_select,
            valid_content_types,
            "current_ui_config"
          )
          if (!identical(safe_list_check(config$imputation_column_select, character(0)), configured_columns)) {
            refresh_current_ui_config_columns(configured_columns)
          }
        }

        current_input_columns <- tryCatch({
          safe_list_check(input$imputation_column_select, character(0))
        }, error = function(e) character(0))
        valid_input_columns <- intersect_imputation_selections(
          current_input_columns,
          valid_content_types,
          "active UI"
        )

        selected_columns <- if (length(configured_columns) > 0 ||
                                (!is.null(config) && !is.null(config$imputation_column_select))) {
          configured_columns
        } else if (length(valid_input_columns) > 0) {
          valid_input_columns
        } else {
          valid_remembered_columns
        }

        updateSelectInput(
          session,
          "imputation_column_select",
          choices = valid_content_types,
          selected = selected_columns
        )

        if (length(valid_content_types) > 0) {
          debug_log(paste("Updated column choices:", length(valid_content_types), "content types available"), 2)
        } else {
          debug_log("No supported numeric abundance content types found", 2)
        }

      }, error = function(e) {
        debug_log(paste("Error updating column choices:", e$message), 1)
        valid_imputation_choices_cache(character(0))
        updateSelectInput(session, "imputation_column_select", choices = character(0), selected = character(0))
      })
    }

    # Update column choices based on metadata and data column signature.
    observeEvent(data_revision_signature(), {
      refresh_imputation_choices()
    }, ignoreInit = TRUE)

    # A data revision can be released while the collapsible imputation panel is
    # still closed.  That event is deliberately ignored by
    # refresh_imputation_choices(), so opening the panel must perform the first
    # hydration explicitly rather than waiting for another data change.
    observeEvent(initialized(), {
      if (isTRUE(initialized())) refresh_imputation_choices()
    }, ignoreInit = TRUE)

    # ========================================
    # Enhanced Processing Log Functions
    # ========================================
