# modules/Data Wizard/filtering/datawizard_filtering_observers.R
#
# PURPOSE:
#   Contains all observe() and observeEvent() blocks for the Data Wizard
#   filtering module. This file centralizes reactive side-effects (UI updates,
#   input synchronization, filter CRUD operations) so that the orchestrator
#   file (datawizard_filtering.R) stays lean and focused on wiring.
#
# ARCHITECTURE:
#   Called from datawizard_filtering.R via register_filtering_observers().
#   All observers run inside the moduleServer() closure of the orchestrator.
#   Reactive state comes from datawizard_filtering_state.R (via the `state`
#   list and its destructured aliases).  Pure filter logic comes from
#   datawizard_filtering_engine.R.  Utility helpers come from
#   datawizard_filtering_utils.R.  All three are loaded into modEnv before
#   this file is sourced.
#
# STRUCTURE:
#   1. register_filtering_observers()
#      - SOME validation observer
#      - UI config integration observer (auto-assign)
#      - Config reset observer
#      - Category selection observer
#      - Column selection observer (with caching)
#      - Metadata observer
#      - Add filter observer
#      - Clear filters observer
#      - Reset all filters observer
#      - Apply all filters observer
#      - Table update observer
#
# NOTES FOR DEVELOPERS:
#   - Observer registration order is preserved from the original monolith.
#     Changing the order may affect timing of reactive invalidation chains.
#   - Every observer is wrapped in tryCatch for robustness. If you add a
#     new observer, follow the same pattern.
#   - `show_filter_notification` is passed in and must not be called before
#     the session is available.
#   - `perform_filtering` is passed as a closure that already captures
#     data, metadata_def, filter_state, etc.

register_filtering_observers <- function(input, output, session, ns,
                                         filter_state,
                                         apply_filters_trigger,
                                         reset_filters_trigger,
                                         filtered_conditions_dw,
                                         filterChoicesCache,
                                         filterCategoryCache,
                                         config,
                                         config_applied,
                                         metadata_ready,
                                         some_operator,
                                         some_count,
                                         selected_type,
                                         data,
                                         metadata_def,
                                         perform_filtering,
                                         show_filter_notification,
                                         debug_log,
                                         DEBUG_LEVEL,
                                         primary_working_revision_debounced = reactive(NULL),
                                         metadata_revision_debounced = reactive(NULL),
                                         data_revision_signature = reactive(NULL),
                                         filtering_ui_active = reactiveVal(FALSE),
                                         metadata_assignment_pending = reactive(FALSE),
                                         metadata_meaningful_ready = reactive(FALSE)) {


  is_default_or_empty_filter_config <- function(cfg) {
    if (is.null(cfg) || !is.list(cfg)) return(TRUE)

    custom_empty <- is.null(cfg$custom) ||
      (is.data.frame(cfg$custom) && nrow(cfg$custom) == 0)

    conf <- cfg$confidence %||% list()
    conf_numeric_enabled <- isTRUE(conf$numeric_enabled %||% conf$numeric_fdr_dw %||% FALSE)
    conf_string_enabled <- isTRUE(conf$string_enabled %||% conf$string_fdr_dw %||% FALSE)
    conf_numeric_max <- conf$numeric_max %||% conf$numeric_input_dw_max
    conf_numeric_min <- conf$numeric_min %||% conf$numeric_input_dw
    conf_string_input <- conf$string_input %||% conf$string_input_dw %||% ""
    confidence_default <- !conf_numeric_enabled && !conf_string_enabled &&
      is.null(conf_numeric_max) &&
      (is.null(conf_numeric_min) || isTRUE(suppressWarnings(as.numeric(conf_numeric_min) == 0.05))) &&
      !nzchar(as.character(conf_string_input %||% ""))

    valid <- cfg$valid_values %||% list()
    valid_group <- valid$group_selection %||% valid$valid_filtering_group_dw %||% "In total"
    valid_min <- valid$min_count %||% valid$valid_filtering_value_dw %||% 1
    valid_default <- identical(as.character(valid_group), "In total") &&
      isTRUE(suppressWarnings(as.numeric(valid_min) == 1))

    custom_empty && confidence_default && valid_default
  }

  should_silently_accept_startup_defaults <- function(cfg) {
    current_data <- tryCatch(isolate(data()), error = function(e) NULL)
    is.null(current_data) && is_default_or_empty_filter_config(cfg)
  }

  observeEvent(input$filter_tabs_dw, {
    filtering_ui_active(TRUE)
  }, ignoreInit = TRUE)

  # ========================================
  # SOME Validation Observer
  # ========================================

  observe({
    tryCatch({
      current_type <- selected_type()
      current_logic <- safe_character_check(input$filter_logic_dw, "AND")
      logic_choices <- if (identical(current_type, "character")) {
        list("AND" = "AND", "OR" = "OR", "EXCLUDE" = "EXCLUDE")
      } else {
        list("AND" = "AND", "OR" = "OR")
      }

      if (!identical(current_type, "character") && current_logic == "EXCLUDE") {
        current_logic <- "AND"
      }

      updateSelectInput(
        session,
        "filter_logic_dw",
        choices = logic_choices,
        selected = current_logic
      )
    }, error = function(e) {
      debug_log(paste("Error updating filter logic choices:", e$message), 1)
    })
  })

  observe({
    tryCatch({
      req(input$multi_column_logic)

      if (input$multi_column_logic == "SOME") {
        current_columns <- tryCatch({
          length(input$filter_column_dw %||% character(0))
        }, error = function(e) {
          debug_log(paste("Error getting current columns:", e$message), 1)
          0
        })

        current_count <- some_count()

        # Update max value safely
        if (current_columns > 0) {
          tryCatch({
            updateNumericInput(
              session,
              "some_count",
              max = current_columns,
              value = min(current_count, current_columns)
            )
          }, error = function(e) {
            debug_log(paste("Error updating numeric input:", e$message), 1)
          })
        }

        # Display validation message with error handling
        validation_msg <- tryCatch({
          if (current_columns < 2) {
            tags$div(
              style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 5px; border-radius: 3px;",
              HTML("<small><strong>Warning:</strong> SOME logic requires multiple columns. Please select at least 2 columns.</small>")
            )
          } else if (current_count > current_columns) {
            tags$div(
              style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 5px; border-radius: 3px;",
              HTML(paste("<small><strong>Error:</strong> Count cannot exceed", current_columns, "selected columns.</small>"))
            )
          } else {
            tags$div(
              style = "color: #155724; background-color: #d4edda; border: 1px solid #c3e6cb; padding: 5px; border-radius: 3px;",
              HTML(paste("<small><strong>Valid:</strong> Will check", current_count, "out of", current_columns, "columns.</small>"))
            )
          }
        }, error = function(e) {
          debug_log(paste("Error creating validation message:", e$message), 1)
          tags$div(
            style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 5px; border-radius: 3px;",
            HTML("<small><strong>Error:</strong> Validation failed.</small>")
          )
        })

        tryCatch({
          output$some_validation_message <- renderUI(validation_msg)
        }, error = function(e) {
          debug_log(paste("Error rendering validation message:", e$message), 1)
        })
      }

    }, error = function(e) {
      debug_log(paste("Error in SOME validation observer:", e$message), 1)
      tryCatch({
        output$some_validation_message <- renderUI(NULL)
      }, error = function(e2) {
        debug_log(paste("Error clearing validation message:", e2$message), 1)
      })
    })
  })

  # ========================================
  # UI Config Integration for Auto-Assign (Enhanced Timing)
  # ========================================

  observe({
    tryCatch({
      current_config <- config()
      if (!is.null(current_config) && !config_applied()) {
        if (should_silently_accept_startup_defaults(current_config)) {
          isolate({
            config_applied(TRUE)
          })
          return()
        }

        filtering_ui_active(TRUE)
        debug_log("Applying UI configuration from auto-assign", 1)

        # Capture all config values BEFORE onFlushed
        local_config <- isolate(current_config)

        # Use onFlushed to ensure UI elements are rendered before updating
        session$onFlushed(function() {
          tryCatch({
            debug_log("Executing delayed filtering UI updates", 2)

            # Apply confidence settings with freeze to prevent loops
            if (!is.null(local_config$confidence)) {
              conf <- local_config$confidence
              debug_log("Applying confidence config", 2)

              conf_numeric_enabled <- conf$numeric_enabled %||% conf$numeric_fdr_dw
              if (!is.null(conf_numeric_enabled)) {
                updateCheckboxInput(session, "numeric_fdr_dw", value = isTRUE(conf_numeric_enabled))
                debug_log(paste("Updated numeric_fdr_dw to:", isTRUE(conf_numeric_enabled)), 2)
              }

              conf_string_enabled <- conf$string_enabled %||% conf$string_fdr_dw
              if (!is.null(conf_string_enabled)) {
                updateCheckboxInput(session, "string_fdr_dw", value = isTRUE(conf_string_enabled))
                debug_log(paste("Updated string_fdr_dw to:", isTRUE(conf_string_enabled)), 2)
              }

              conf_numeric_max <- conf$numeric_max %||% conf$numeric_input_dw_max
              if (!is.null(conf_numeric_max) && is.numeric(conf_numeric_max)) {
                updateNumericInput(session, "numeric_input_dw_max", value = conf_numeric_max)
                debug_log(paste("Updated numeric_input_dw_max to:", conf_numeric_max), 2)
              }

              conf_numeric_min <- conf$numeric_min %||% conf$numeric_input_dw
              if (!is.null(conf_numeric_min) && is.numeric(conf_numeric_min)) {
                updateNumericInput(session, "numeric_input_dw", value = conf_numeric_min)
                debug_log(paste("Updated numeric_input_dw to:", conf_numeric_min), 2)
              }

              conf_string_input <- conf$string_input %||% conf$string_input_dw
              if (!is.null(conf_string_input) && is.character(conf_string_input)) {
                updateTextInput(session, "string_input_dw", value = conf_string_input)
                debug_log(paste("Updated string_input_dw to:", conf_string_input), 2)
              }

              # Update internal state (outside onFlushed)
              isolate({
                filter_state$confidence <- conf
              })
              debug_log("Confidence settings applied from UI config", 2)
            }

            # Apply valid values settings with freeze to prevent loops
            if (!is.null(local_config$valid_values)) {
              valid <- local_config$valid_values
              debug_log("Applying valid values config", 2)

              valid_group_selection <- valid$group_selection %||% valid$valid_filtering_group_dw
              if (!is.null(valid_group_selection) && is.character(valid_group_selection)) {
                # Ensure the selection is valid
                valid_choices <- c("In total", "One group", "Each group")
                if (valid_group_selection %in% valid_choices) {
                  updateSelectInput(session, "valid_filtering_group_dw", selected = valid_group_selection)
                  debug_log(paste("Updated valid_filtering_group_dw to:", valid_group_selection), 2)
                }
              }

              valid_min_count <- valid$min_count %||% valid$valid_filtering_value_dw
              if (!is.null(valid_min_count) && is.numeric(valid_min_count)) {
                updateNumericInput(session, "valid_filtering_value_dw", value = valid_min_count)
                debug_log(paste("Updated valid_filtering_value_dw to:", valid_min_count), 2)
              }

              # Update internal state (outside onFlushed)
              isolate({
                filter_state$valid_values <- valid
              })
              debug_log("Valid values settings applied from UI config", 2)
            }

            # Apply custom filters (this already works)
            if (!is.null(local_config$custom) && is.data.frame(local_config$custom)) {
              current_data <- isolate(data())
              if (!is.null(current_data) && is.data.frame(current_data)) {
                # Validate and apply custom filters
                valid_filters <- local_config$custom[
                  local_config$custom$Column %in% names(current_data), , drop = FALSE
                ]

                if (nrow(valid_filters) > 0) {
                  isolate({
                    filter_state$custom_conditions <- valid_filters
                  })
                  debug_log(paste("Applied", nrow(valid_filters), "custom filters from UI config"), 2)
                }
              }
            }

            # Mark config as applied (outside onFlushed)
            isolate({
              config_applied(TRUE)
            })
            debug_log("All UI config applied successfully", 1)

          }, error = function(e) {
            debug_log(paste("Error in delayed UI config application:", e$message), 1)
          })
        })
      }
    }, error = function(e) {
      debug_log(paste("Error in observe for UI config:", e$message), 1)
    })
  })

  # Reset config_applied when new config arrives (SAFE VERSION)
  observeEvent(config(), {
    tryCatch({
      if (!is.null(config())) {
        config_applied(FALSE)
        debug_log("New config detected, reset config_applied flag", 2)
      }
    }, error = function(e) {
      debug_log(paste("Error in config reset observer:", e$message), 1)
    })
  }, ignoreInit = TRUE)

  # ========================================
  # Enhanced Column Selection Updates with Abundance Info (SAFE VERSION)
  # ========================================

  filter_category_choice_cache <- reactiveVal(list(signature = NULL, choices = NULL))

  # Fetch each upstream object once for a committed transaction.  The retained
  # value is descriptor-only: no copy of the (potentially large) data frame.
  filtering_refresh_snapshot <- reactiveVal(list(
    key = NULL, columns = character(0), content_to_columns = list(),
    categories = character(0), metadata_ready = FALSE
  ))
  observeEvent(data_revision_signature(), {
    key <- data_revision_signature()
    current_data <- isolate(data())
    md <- isolate(metadata_def())
    columns <- if (is.data.frame(current_data)) names(current_data) else character(0)
    ready <- is.data.frame(md) && all(c("Content", "Column") %in% names(md))
    mapping <- if (ready) split(as.character(md$Column), as.character(md$Content)) else list()
    categories <- names(mapping)
    categories <- categories[!is.na(categories) & nzchar(categories)]
    if ("Additional Information" %in% as.character(md$Content)) {
      categories <- unique(c(categories, "Additional Information"))
    }
    filtering_refresh_snapshot(list(
      key = key, columns = columns, content_to_columns = mapping,
      categories = categories, metadata_ready = ready
    ))
  }, ignoreInit = FALSE, priority = 100)

  observeEvent(data_revision_signature(), {
    if (isTRUE(metadata_assignment_pending()) && !isTRUE(metadata_meaningful_ready())) {
      debug_log("Metadata assignment pending; deferring filtering category choices", 2)
      return()
    }
    tryCatch({
      snapshot <- filtering_refresh_snapshot()
      if (!isTRUE(snapshot$metadata_ready)) {
        updateSelectInput(session, "filter_category_dw", choices = c("No categories available" = ""))
        return()
      }
      category_signature <- paste(unlist(tryCatch(data_revision_signature(), error = function(e) NULL), use.names = TRUE), collapse = "|")
      cached_categories <- filter_category_choice_cache()
      if (!is.null(cached_categories$signature) && identical(cached_categories$signature, category_signature)) {
        cats <- cached_categories$choices
      } else {
        cats <- snapshot$categories
        filter_category_choice_cache(list(signature = category_signature, choices = cats))
      }
      if (length(cats) == 0) {
        updateSelectInput(session, "filter_category_dw", choices = c("No categories found" = ""))
        return()
      }
      updateSelectInput(session, "filter_category_dw",
                        choices = c("All columns..." = "", cats),
                        selected = "")
      debug_log(paste("Updated filter categories:", length(cats), "available"), 2)
    }, error = function(e) {
      debug_log(paste("Error updating filter categories:", e$message), 1)
      updateSelectInput(session, "filter_category_dw", choices = c("Error loading categories" = ""))
    })
  }, ignoreInit = TRUE)

  observeEvent(list(data_revision_signature(), input$filter_category_dw), {
    tryCatch({
      snapshot <- filtering_refresh_snapshot()
      all_cols <- snapshot$columns
      req(length(all_cols) > 0)
      sel_category <- tryCatch({ input$filter_category_dw }, error = function(e) NULL)
      sel_category_chr <- if (is.null(sel_category)) "" else as.character(sel_category)

      use_meta <- isTRUE(snapshot$metadata_ready)

      # Compute choices for the current (maybe empty) category
      choices <- all_cols
      if (use_meta && nzchar(sel_category_chr)) {
        category_columns <- snapshot$content_to_columns[[sel_category_chr]]
        if (length(category_columns) > 0) {
          choices <- intersect(as.character(category_columns), all_cols)
        } else {
          choices <- character(0)
        }
      }

      # Normalize for deterministic comparison
      choices <- unique(as.character(choices))
      choices <- choices[order(choices)]

      # Current user selection (never use %||% here; be explicit)
      prev_selected <- tryCatch({
        if (is.null(input$filter_column_dw)) character(0) else as.character(input$filter_column_dw)
      }, error = function(e) character(0))

      # Keep only valid selections
      valid_selected <- intersect(prev_selected, choices)

      revision_key <- paste(unlist(tryCatch(data_revision_signature(), error = function(e) NULL), use.names = TRUE), collapse = "|")
      choices <- paste(revision_key, choices, sep = "::")
      # ---- Change detection to avoid unnecessary updateSelectizeInput() ----
      last_choices  <- isolate(filterChoicesCache())
      last_category <- isolate(filterCategoryCache())

      category_same <- identical(sel_category_chr, last_category)
      choices_same  <- identical(choices, last_choices)

      if (category_same && choices_same) {
        if (setequal(valid_selected, prev_selected)) {
          return()
        }
      }

      # Prepare what to set as 'selected'
      selected_to_set <- if (setequal(valid_selected, prev_selected)) prev_selected else valid_selected

      # Prevent downstream observers from firing due to programmatic updates
      freezeReactiveValue(input, "filter_column_dw")

      # Push choices + selected; use server=TRUE to reduce client-side churn
      updateSelectizeInput(
        session,
        "filter_column_dw",
        choices = sub("^.*::", "", choices),
        selected = selected_to_set,
        server   = TRUE
      )

      # Update caches AFTER a successful push
      filterChoicesCache(choices)
      filterCategoryCache(sel_category_chr)

      debug_log(
        sprintf("Updated filter columns: %d choices (category='%s'; selected=%d)",
                length(choices),
                ifelse(nzchar(sel_category_chr), sel_category_chr, "<all>"),
                length(selected_to_set)),
        2
      )

    }, error = function(e) {
      debug_log(paste("Error updating filter column choices:", e$message), 1)
      freezeReactiveValue(input, "filter_column_dw")
      updateSelectizeInput(session, "filter_column_dw", choices = character(0), selected = character(0))
      # Reset local caches on error
      filterChoicesCache(character(0))
      filterCategoryCache("")
    })
  }, ignoreInit = TRUE)

  # Update status when metadata changes (SAFE VERSION)
  observeEvent(data_revision_signature(), {
    tryCatch({
      current_data <- tryCatch({ isolate(data()) }, error = function(e) NULL)
      req(is.data.frame(current_data))
      current_metadata <- tryCatch({ isolate(metadata_def()) }, error = function(e) NULL)
      metadata_status <- tryCatch({ metadata_ready() }, error = function(e) FALSE)

      if (!is.null(current_metadata) && metadata_status) {
        abundance_info <- find_abundance_columns_with_priority(current_metadata, metadata_status)
        if (length(abundance_info$columns) > 0) {
          debug_log(paste("Metadata update: Found", length(abundance_info$columns),
                          abundance_info$selected_type, "columns"), 2)
        }
      }
    }, error = function(e) {
      debug_log(paste("Error in metadata observer:", e$message), 1)
    })
  }, ignoreInit = TRUE)

  # ========================================
  # Enhanced Event Observers with Robust Error Handling
  # ========================================

  # Add filter - ENHANCED SAFETY TO PREVENT CRASH
  observeEvent(input$add_filter_dw, {
    tryCatch({
      filtering_ui_active(TRUE)
      debug_log("Add filter button clicked", 2)

      # Robust input validation
      column_valid <- tryCatch({
        !is.null(input$filter_column_dw) && length(input$filter_column_dw) > 0
      }, error = function(e) {
        debug_log(paste("Error checking column validity:", e$message), 1)
        FALSE
      })

      current_data <- data()
      sel_cols <- if (is.null(input$filter_column_dw)) character(0) else as.character(input$filter_column_dw)
      if (!validate_selected_columns_homogeneous(session, sel_cols, current_data)) return()

      if (!column_valid) {
        show_filter_notification("Please select at least one column to filter.", "warning", "Add Filter", session, DEBUG_LEVEL)
        return()
      }

      operator_valid <- tryCatch({
        !is.null(input$filter_operator_dw_1) && nzchar(input$filter_operator_dw_1)
      }, error = function(e) {
        debug_log(paste("Error checking operator validity:", e$message), 1)
        FALSE
      })

      if (!operator_valid) {
        show_filter_notification("Please select an operator for the first condition.", "warning", "Add Filter", session, DEBUG_LEVEL)
        return()
      }

      # Robust input sanitization
      value_1_dw <- tryCatch({
        if (!is.null(input$filter_value_dw_1) && nzchar(trimws(input$filter_value_dw_1))) {
          sanitize_filter_input(trimws(input$filter_value_dw_1), debug_level = DEBUG_LEVEL)
        } else {
          NA_character_
        }
      }, error = function(e) {
        debug_log(paste("Error sanitizing value_1:", e$message), 1)
        NA_character_
      })

      value_2_dw <- tryCatch({
        if (!is.null(input$filter_value_dw_2) && nzchar(trimws(input$filter_value_dw_2))) {
          sanitize_filter_input(trimws(input$filter_value_dw_2), debug_level = DEBUG_LEVEL)
        } else {
          NA_character_
        }
      }, error = function(e) {
        debug_log(paste("Error sanitizing value_2:", e$message), 1)
        NA_character_
      })

      # Safe extraction of other inputs
      operator_2_dw <- safe_character_check(
        if (!is.na(value_2_dw) && !is.null(input$filter_operator_dw_2)) {
          input$filter_operator_dw_2
        } else {
          NA_character_
        },
        NA_character_
      )

      logic_dw <- safe_character_check(
        if (!is.na(value_2_dw) && !is.null(input$filter_logic_dw)) {
          input$filter_logic_dw
        } else {
          NA_character_
        },
        NA_character_
      )

      empty_filter_dw <- safe_character_check(input$filter_empty_dw, "None")
      multi_column_logic_dw <- safe_character_check(input$multi_column_logic, "OR")

      # Robust SOME logic settings
      some_operator_dw <- NA_character_
      some_count_dw <- NA_real_

      if (multi_column_logic_dw == "SOME") {
        some_operator_dw <- tryCatch({
          if (!is.null(input$some_operator)) {
            some_operator()
          } else {
            "at_least"
          }
        }, error = function(e) {
          debug_log(paste("Error getting some_operator:", e$message), 1)
          "at_least"
        })

        some_count_dw <- tryCatch({
          if (!is.null(input$some_count)) {
            some_count()
          } else {
            1
          }
        }, error = function(e) {
          debug_log(paste("Error getting some_count:", e$message), 1)
          1
        })

        # Validate SOME logic settings
        some_validation_ok <- tryCatch({
          column_count <- length(input$filter_column_dw)

          if (column_count < 2) {
            show_filter_notification("SOME logic requires at least 2 columns. Switching to OR logic.", "warning", "Add Filter", session, DEBUG_LEVEL)
            multi_column_logic_dw <<- "OR"
            some_operator_dw <<- NA_character_
            some_count_dw <<- NA_real_
            TRUE
          } else if (some_count_dw > column_count) {
            show_filter_notification(
              paste("Count adjusted to maximum valid count (", column_count, ")."),
              "info", "Add Filter", session, DEBUG_LEVEL
            )
            some_count_dw <<- column_count
            TRUE
          } else {
            TRUE
          }
        }, error = function(e) {
          debug_log(paste("Error validating SOME logic:", e$message), 1)
          multi_column_logic_dw <<- "OR"
          some_operator_dw <<- NA_character_
          some_count_dw <<- NA_real_
          TRUE
        })

        if (!some_validation_ok) {
          return()
        }
      }

      # Validate filter conditions
      condition_validation <- tryCatch({
        has_value_condition <- !is.na(value_1_dw) && nzchar(value_1_dw)
        has_empty_condition <- !is.null(empty_filter_dw) && empty_filter_dw != "None"

        list(
          valid = has_value_condition || has_empty_condition,
          has_value = has_value_condition,
          has_empty = has_empty_condition
        )
      }, error = function(e) {
        debug_log(paste("Error validating conditions:", e$message), 1)
        list(valid = FALSE, has_value = FALSE, has_empty = FALSE)
      })

      if (!condition_validation$valid) {
        show_filter_notification("Please provide at least one filter value or select an empty filter option.", "warning", "Add Filter", session, DEBUG_LEVEL)
        return()
      }

      # Robust data validation
      data_validation <- tryCatch({
        current_data <- data()

        if (is.null(current_data) || !is.data.frame(current_data)) {
          list(valid = FALSE, message = "No data available for filtering.")
        } else {
          missing_columns <- setdiff(input$filter_column_dw, names(current_data))
          if (length(missing_columns) > 0) {
            list(
              valid = FALSE,
              message = paste("The following columns are not found in the data:", paste(missing_columns, collapse = ", "))
            )
          } else {
            list(valid = TRUE, message = NULL)
          }
        }
      }, error = function(e) {
        debug_log(paste("Error validating data:", e$message), 1)
        list(valid = FALSE, message = "Error validating data structure.")
      })

      if (!data_validation$valid) {
        show_filter_notification(data_validation$message, "error", "Add Filter", session, DEBUG_LEVEL)
        return()
      }

      # Create new filter with robust error handling
      filter_creation_result <- tryCatch({
        # Get current filters safely
        current_filters <- tryCatch({
          if (exists("filtered_conditions_dw") && is.reactive(filtered_conditions_dw)) {
            existing_filters <- filtered_conditions_dw()
            if (is.null(existing_filters) || !is.data.frame(existing_filters)) {
              data.frame(stringsAsFactors = FALSE)
            } else {
              existing_filters
            }
          } else {
            data.frame(stringsAsFactors = FALSE)
          }
        }, error = function(e) {
          debug_log(paste("Error getting current filters:", e$message), 1)
          data.frame(stringsAsFactors = FALSE)
        })

        # Create new filter
        new_filter_dw <- data.frame(
          Column = paste(input$filter_column_dw, collapse = "|"),
          Operator_1 = input$filter_operator_dw_1,
          Value_1 = value_1_dw,
          Logic = logic_dw,
          Operator_2 = operator_2_dw,
          Value_2 = value_2_dw,
          Empty_Filter = empty_filter_dw,
          Multi_Column_Logic = multi_column_logic_dw,
          Some_Operator = some_operator_dw,
          Some_Count = some_count_dw,
          stringsAsFactors = FALSE
        )

        # Add SOME columns to existing filters if needed
        if (nrow(current_filters) > 0) {
          if (!"Some_Operator" %in% names(current_filters)) {
            current_filters$Some_Operator <- NA_character_
          }
          if (!"Some_Count" %in% names(current_filters)) {
            current_filters$Some_Count <- NA_real_
          }

          # Ensure all columns exist in new_filter_dw
          missing_in_new <- setdiff(names(current_filters), names(new_filter_dw))
          for (col in missing_in_new) {
            new_filter_dw[[col]] <- NA_character_
          }
        }

        # Combine filters
        updated_filters <- rbind(current_filters, new_filter_dw)

        list(success = TRUE, filters = updated_filters, new_filter = new_filter_dw)

      }, error = function(e) {
        debug_log(paste("Error creating filter:", e$message), 1)
        list(success = FALSE, message = paste("Error creating filter:", e$message))
      })

      if (!filter_creation_result$success) {
        show_filter_notification(filter_creation_result$message, "error", "Add Filter", session, DEBUG_LEVEL)
        return()
      }

      # Update reactive values safely
      tryCatch({
        # Update internal state first
        filter_state$custom_conditions <- filter_creation_result$filters

        debug_log(paste("Updated filter_state with", nrow(filter_creation_result$filters), "total filters"), 2)

      }, error = function(e) {
        debug_log(paste("Error updating filter conditions:", e$message), 1)
        show_filter_notification("Error saving filter condition. Please try again.", "error", "Add Filter", session, DEBUG_LEVEL)
        return()
      })

      # Validate filter table integrity
      validate_filter_table_integrity(filter_state)

      # Force table refresh
      tryCatch({
      }, error = function(e) {
        debug_log(paste("Error forcing table refresh:", e$message), 2)
      })

      # Success notification
      notification_result <- tryCatch({
        if (length(input$filter_column_dw) > 1) {
          if (multi_column_logic_dw == "SOME") {
            safe_sprintf("SOME filter added: %s %d out of %d columns must meet condition.",
                         some_operator_dw,
                         safe_numeric_check(some_count_dw, default_val = 1),
                         safe_numeric_check(length(input$filter_column_dw), default_val = 1))
          } else {
            safe_sprintf("Multi-column filter added for %d columns using %s logic.",
                         safe_numeric_check(length(input$filter_column_dw), default_val = 1),
                         multi_column_logic_dw)
          }
        } else {
          "Filter condition added."
        }
      }, error = function(e) {
        debug_log(paste("Error generating notification:", e$message), 1)
        "Filter condition added."
      })

      show_filter_notification(notification_result, "success", "Add Filter", session, DEBUG_LEVEL)
      debug_log(paste("Added filter for", length(input$filter_column_dw), "columns"), 1)

    }, error = function(e) {
      debug_log(paste("Critical error in add filter observer:", e$message), 1)
      show_filter_notification("Failed to add filter due to an error. Please try again.", "error", "Add Filter", session, DEBUG_LEVEL)
    })
  })

  # Clear filters
  observeEvent(input$clear_filters_dw, {
    tryCatch({
      debug_log("Clear filters button clicked", 2)

      # Reset to empty data frame with all necessary columns
      empty_conditions <- data.frame(
        Column = character(),
        Operator_1 = character(),
        Value_1 = character(),
        Logic = character(),
        Operator_2 = character(),
        Value_2 = character(),
        Empty_Filter = character(),
        Multi_Column_Logic = character(),
        Some_Operator = character(),
        Some_Count = character(),
        stringsAsFactors = FALSE
      )

      filter_state$custom_conditions <- empty_conditions

      show_filter_notification("All custom filters cleared", "success", "Clear Filters", session, DEBUG_LEVEL)
      debug_log("Custom filters cleared successfully", 1)

    }, error = function(e) {
      debug_log(paste("Error clearing filters:", e$message), 1)
      show_filter_notification("Error clearing filters. Please try again.", "error", "Clear Filters", session, DEBUG_LEVEL)
    })
  })

  # Reset all filters - enhanced safety
  observeEvent(input$reset_all_filters, {
    tryCatch({
      # Reset all UI inputs safely
      updateCheckboxInput(session, "numeric_fdr_dw", value = FALSE)
      updateCheckboxInput(session, "string_fdr_dw", value = FALSE)
      updateNumericInput(session, "numeric_input_dw", value = NULL)
      updateNumericInput(session, "numeric_input_dw_max", value = NULL)
      updateTextInput(session, "string_input_dw", value = "")
      updateNumericInput(session, "valid_filtering_value_dw", value = 1)
      updateSelectInput(session, "valid_filtering_group_dw", selected = "In total")

      # Reset state
      filter_state$custom_conditions <- data.frame(
        Column = character(),
        Operator_1 = character(),
        Value_1 = character(),
        Logic = character(),
        Operator_2 = character(),
        Value_2 = character(),
        Empty_Filter = character(),
        Multi_Column_Logic = character(),
        stringsAsFactors = FALSE
      )

      filter_state$confidence <- list(
        numeric_enabled = FALSE,
        string_enabled = FALSE,
        numeric_max = NULL,
        numeric_min = NULL,
        string_input = NULL
      )

      filter_state$valid_values <- list(
        group_selection = "In total",
        min_count = 1
      )

      filter_state$errors <- character()
      filter_state$preview_active <- FALSE

      reset_filters_trigger(reset_filters_trigger() + 1)
      show_filter_notification("All filters have been reset", "message", "", session)
      debug_log("All filters reset", 1)

    }, error = function(e) {
      debug_log(paste("Error resetting filters:", e$message), 1)
      show_filter_notification("Error resetting filters", "error", "", session)
    })
  })

  # Apply filters - enhanced safety
  observeEvent(input$apply_all_filters, {
    tryCatch({
      req_data <- tryCatch({ req(data(), metadata_def()) }, error = function(e) NULL)
      if (is.null(req_data)) {
        debug_log("Data or metadata not available for filtering", 1)
        return()
      }

      filtering_ui_active(TRUE)
      filter_state$processing <- TRUE
      filter_state$errors <- character()

      # Update filter state safely
      filter_state$confidence <- list(
        numeric_enabled = safe_logical_check(input$numeric_fdr_dw),
        string_enabled = safe_logical_check(input$string_fdr_dw),
        numeric_max = input$numeric_input_dw_max,
        numeric_min = input$numeric_input_dw,
        string_input = safe_character_check(input$string_input_dw)
      )

      filter_state$valid_values <- list(
        group_selection = safe_character_check(input$valid_filtering_group_dw, "In total"),
        min_count = as.numeric(input$valid_filtering_value_dw %||% 1)
      )

      result <- perform_filtering(source = "individual")

      if (safe_logical_check(result$success)) {
        show_filter_notification(
          safe_sprintf("Filters applied successfully. Removed %d rows.",
                       safe_numeric_check(result$rows_removed, default_val = 0)),
          "message", "", session
        )
        apply_filters_trigger(apply_filters_trigger() + 1)
      } else {
        filter_state$errors <- result$errors %||% character()
        show_filter_notification("Filter application failed", "error", "", session)
      }

      filter_state$processing <- FALSE

    }, error = function(e) {
      debug_log(paste("Error applying filters:", e$message), 1)
      show_filter_notification("Filter application failed due to an error", "error", "", session)
      filter_state$processing <- FALSE
      filter_state$errors <- c(filter_state$errors, paste("Apply error:", e$message))
    })
  })

  # ========================================
  # Table Update Observer
  # ========================================

  observe({
    tryCatch({
      # Trigger table re-render when conditions change
      current_conditions <- filter_state$custom_conditions
      if ((is.null(current_conditions) || !is.data.frame(current_conditions) || nrow(current_conditions) == 0) &&
          !isTRUE(filtering_ui_active())) {
        return()
      }
      debug_log(paste("Custom conditions changed, table has", nrow(current_conditions), "rows"), 2)

      # Force reactivity by touching the output

    }, error = function(e) {
      debug_log(paste("Error in table update observer:", e$message), 1)
    })
  })

  debug_log("All filtering observers registered", 1)
}
