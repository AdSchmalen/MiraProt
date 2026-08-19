# modules/datawizard_merge.R - Enhanced with Improved Debug Management System

############
# UI (Enhanced with better tooltips and structure)

#' Enhanced Merge Module UI with UI_config Support
#'
#' Creates the user interface for data merging operations with enhanced tooltips and structure
#' @param id module namespace id
#' @export
modMergeUI <- function(id) {
  ns <- NS(id)
  div(
    ## Tooltip at top
    fluidRow(
      column(12,
             div(style = "margin-top: -10px; margin-bottom: 15px; color: #666; font-size: 12px;",
                 HTML("<strong>Merge operations combine two datasets based on common columns (keys).
                      Choose join columns carefully to ensure proper data alignment and avoid data loss.</strong><br/>"))
      )),

    # Hidden readiness output for conditionalPanel (must exist in DOM)
    tags$div(style = "display: none;",
             textOutput(ns("merge_ready"))
    ),

    # Not ready: show large notice (like Pivot)
    conditionalPanel(
      condition = sprintf("output['%s'] !== 'true'", ns("merge_ready")),
      fluidRow(
        column(12,
               div(class = "alert alert-info",
                   style = "margin-top: 10px; font-size: 14px;",
                   "No data available. Please load both primary and secondary data first."
               )
        )
      )
    ),

    # Ready: full Merge UI
    conditionalPanel(
      condition = sprintf("output['%s'] === 'true'", ns("merge_ready")),

    ## Join column selection for File 1
    fluidRow(
      column(12,
             div(
               title = "Select the column from the primary dataset that will be used as the join key for merging.",
               selectInput(
                 ns("file1_col"),
                 "Primary Data Join Column:",
                 choices = NULL,
                 selected = NULL
               )
             )
      )
    ),

    ## Join column selection for File 2
    fluidRow(
      column(12,
             div(
               title = "Select the column from the secondary dataset that will be used as the join key for merging.",
               selectInput(
                 ns("file2_col"),
                 "Secondary Data Join Column:",
                 choices = NULL,
                 selected = NULL
               )
             )
      )
    ),

    ## Additional columns to add from File 2
    fluidRow(
      column(12,
             div(
               title = "Select additional columns from the secondary dataset to include in the merged result. The join column is included automatically.",
               selectInput(
                 ns("file2_add_col"),
                 "Additional Columns from Secondary Data:",
                 choices = NULL,
                 multiple = TRUE,
                 selected = NULL
               )
             )
      )
    ),

    ## Join type selection
    fluidRow(
      column(12,
             div(
               title = "Left Join: Keep all rows from primary data, add matching rows from secondary.
               \nInner Join: Keep only rows that exist in both datasets.
               \nFull Join: Keep all rows from both datasets, filling missing values with NA.",
               selectInput(
                 ns("join_type"),
                 "Join Type:",
                 choices = c(
                   "Left Join (recommended)" = "left",
                   "Inner Join" = "inner",
                   "Full Join" = "full"
                 ),
                 selected = "left"
               )
             )
      )
    ),

    ## Preview section
    fluidRow(
      column(12,
             div(
               title = "Preview the structure of your merged data before applying the operation.",
               h5("Merge Preview"),
               textOutput(ns("merge_preview_dim")),
               DT::DTOutput(ns("merge_preview_table"))
             )
      )
    ),

    br(),

    ## Action buttons
    fluidRow(
      column(6,
             div(
               title = "Apply the merge operation with current settings to combine your datasets.",
               actionButton(ns("apply_merge"), "Apply Merge",
                            width = "100%", class = "btn-success",
                            style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;")
             ))#,
      # column(6,
      #        div(
      #          title = "Preview how the merge operation will combine your datasets without making changes.",
      #          actionButton(ns("preview_merge"), "Preview Merge",
      #                       width = "100%", class = "btn-info")
      #        ))
    ),

    # ## Status information
    # div(style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
    #     h6("Merge Status"),
    #     verbatimTextOutput(ns("merge_status"))
    # )
    )
  )
}

############
# Server - Enhanced with Improved Debug Management System

#' Enhanced Merge Module Server with Improved Debug Management
#'
#' Server logic for data merging operations with comprehensive error handling and monitoring
#' @param id module namespace id
#' @param get_data function to get primary data
#' @param set_data function to set primary data
#' @param get_data2 function to get secondary data
#' @param UI_config reactive containing UI configuration for import (optional)
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modMergeServer <- function(id, get_data = NULL, set_data = NULL, get_data2 = NULL,
                           UI_config = NULL,
                           session_restore_trigger = reactive(NULL),
                           debug_level = 0,
                           primary_working_revision_debounced = reactive(NULL),
                           secondary_revision_debounced = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management System
    # ========================================

    # Helper function for controlled debug output with [MERGE] prefix
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "MERGE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ MERGE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Merge module server starting", 1)

    # ========================================
    # Enhanced Reactive Values with Error Tracking
    # ========================================

    # Processing + logs
    merge_processing_active <- reactiveVal(FALSE)
    last_operation_time <- reactiveVal(NULL)
    operation_history <- reactiveVal(list())
    processing_errors <- reactiveVal(list())
    operation_log <- reactiveVal(list())

    # UI config state
    ui_config_applied <- reactiveVal(FALSE)
    ui_config_source <- reactiveVal("none")
    ui_config_update_in_progress <- reactiveVal(FALSE)
    merge_import_config <- reactiveVal(NULL)
    last_merge_column_signature <- reactiveVal(NULL)

    # ========================================
    # Enhanced Helper Functions with Robust Error Handling
    # ========================================

    #' Add entry to operation log with enhanced error tracking
    add_operation_log <- function(operation, status, message = "", duration = 0) {
      tryCatch({
        current_log <- operation_log()
        new_entry <- list(
          timestamp = Sys.time(),
          operation = operation,
          status = status,
          message = message,
          duration = duration
        )
        operation_log(c(current_log, list(new_entry)))

        # Enhanced debug logging based on status
        if (status == "error") {
          debug_log(paste("ERROR in", operation, ":", message), 1)
          current_errors <- processing_errors()
          processing_errors(c(current_errors, list(new_entry)))
        } else if (status == "warning") {
          debug_log(paste("WARNING in", operation, ":", message), 1)
        } else if (status == "success") {
          debug_log(paste("SUCCESS in", operation, "- duration:", sprintf("%.2fs", duration)), 2)
        } else {
          debug_log(paste("INFO", operation, ":", message), 2)
        }

      }, error = function(e) {
        debug_log(paste("Error adding to operation log:", e$message), 1)
      })
    }

    #' Enhanced boolean check function with robust validation
    safe_is_true <- function(x) {
      if (is.null(x) || length(x) == 0) return(FALSE)
      if (is.logical(x)) return(isTRUE(x[1]))
      if (is.numeric(x)) return(x[1] > 0)
      if (is.character(x)) return(tolower(x[1]) %in% c("true","t","yes","y","1"))
      if (is.list(x)) {
        if (!is.null(x$success)) return(safe_is_true(x$success))
        if (!is.null(x$data)) return(TRUE)
        return(length(x) > 0)
      }
      FALSE
    }

    normalize_join_type <- function(x) {
      if (is.null(x) || length(x) == 0) return(NULL)
      t <- tolower(as.character(x)[1])
      if (t %in% c("left","left_join"))  return("left")
      if (t %in% c("inner","inner_join")) return("inner")
      if (t %in% c("full","full_join","outer","outer_join")) return("full")
      # right/right_join nicht unterstützt -> NULL
      NULL
    }

    get_merge_column_signature <- function(primary_data, secondary_data) {
      primary_cols <- if (is.data.frame(primary_data) && ncol(primary_data) > 0) names(primary_data) else character(0)
      secondary_cols <- if (is.data.frame(secondary_data) && ncol(secondary_data) > 0) names(secondary_data) else character(0)
      paste(
        paste(primary_cols, collapse = "\r"),
        paste(secondary_cols, collapse = "\r"),
        sep = "\n---secondary---\n"
      )
    }

    normalize_merge_config <- function(config) {
      if (!is.null(config$merge) && is.list(config$merge)) config$merge else config
    }

    valid_single_selection <- function(selection, choices) {
      if (!is.null(selection) && length(selection) > 0 && selection[1] %in% choices) {
        as.character(selection)[1]
      } else {
        NULL
      }
    }

    log_dropped_merge_selections <- function(dropped_parts, old_signature, new_signature) {
      if (length(dropped_parts) == 0) return(invisible(FALSE))
      if (!is.null(old_signature) && !identical(old_signature, new_signature)) {
        debug_log(
          paste(
            "Dropped stale merge selections after header-row/data column changes:",
            paste(dropped_parts, collapse = "; ")
          ),
          1
        )
      }
      invisible(TRUE)
    }

    normalize_merged_column <- function(x) {
      if (is.list(x)) {
        if (all(lengths(x) <= 1)) {
          x <- vapply(
            x,
            function(value) {
              if (length(value) == 0 || is.null(value)) return(NA_character_)
              as.character(value[[1]])
            },
            character(1),
            USE.NAMES = FALSE
          )
        } else {
          x <- vapply(
            x,
            function(value) {
              if (length(value) == 0 || is.null(value)) return(NA_character_)
              paste(value, collapse = ";")
            },
            character(1),
            USE.NAMES = FALSE
          )
        }
      }
      type.convert(x, as.is = FALSE)
    }

    ensure_primary_not_shrunk <- function(result, primary_data, join_type) {
      if (!is.data.frame(result) || !is.data.frame(primary_data)) {
        stop("Merge result and primary data must be data frames")
      }
      if (nrow(result) < nrow(primary_data)) {
        stop(paste0(
          "Merge would reduce primary data rows (",
          nrow(primary_data), " -> ", nrow(result),
          "). Primary data must not become smaller. Use a left or full join, or check join keys."
        ))
      }
      if (identical(join_type, "inner")) {
        debug_log(
          "Inner join kept at least the original primary row count; primary data row-count guard passed",
          1
        )
      }
      invisible(TRUE)
    }

    prepare_secondary_for_merge <- function(primary_data, secondary_subset, secondary_join_col,
                                            merge_prefix = "Merged_") {
      use_suffix_fallback <- FALSE
      fallback_reasons <- character(0)

      if (any(is.na(names(primary_data))) || any(names(primary_data) == "")) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "primary data contains missing/empty column names")
      }
      if (any(is.na(names(secondary_subset))) || any(names(secondary_subset) == "")) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "secondary data contains missing/empty column names")
      }
      if (anyDuplicated(names(primary_data)) > 0) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "primary data contains duplicate column names")
      }
      if (anyDuplicated(names(secondary_subset)) > 0) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "secondary data contains duplicate column names")
      }

      secondary_non_join_cols <- setdiff(names(secondary_subset), secondary_join_col)
      prefixed_names <- paste0(merge_prefix, secondary_non_join_cols)
      if (length(prefixed_names) > 0 && anyDuplicated(prefixed_names) > 0) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "prefixed secondary names would duplicate each other")
      }
      if (length(prefixed_names) > 0 && any(prefixed_names %in% names(primary_data))) {
        use_suffix_fallback <- TRUE
        fallback_reasons <- c(fallback_reasons, "prefixed secondary names collide with primary names")
      }

      if (!use_suffix_fallback && length(secondary_non_join_cols) > 0) {
        old_names <- names(secondary_subset)
        names(secondary_subset) <- ifelse(old_names %in% secondary_non_join_cols,
                                          paste0(merge_prefix, old_names), old_names)
      }

      list(
        secondary_subset = secondary_subset,
        used_fallback = use_suffix_fallback,
        fallback_reasons = unique(fallback_reasons),
        prefixed_count = if (use_suffix_fallback) 0 else length(secondary_non_join_cols)
      )
    }

    # ========================================
    # Enhanced Data Access Functions with Error Handling
    # ========================================

    get_primary_data <- function() {
      tryCatch({
        if (!is.null(get_data) && is.function(get_data)) {
          data <- get_data()
          debug_log(paste("Primary data accessed -",
                          if (!is.null(data)) paste(nrow(data), "x", ncol(data)) else "NULL"), 2)
          return(data)
        }
        debug_log("Primary data getter not available", 2)
        return(NULL)
      }, error = function(e) {
        debug_log(paste("Error accessing primary data:", e$message), 1)
        return(NULL)
      })
    }

    set_primary_data <- function(new_data) {
      tryCatch({
        if (!is.null(set_data) && is.function(set_data)) {
          result <- set_data(new_data)
          debug_log(paste("Primary data updated -",
                          if (!is.null(new_data)) paste(nrow(new_data), "x", ncol(new_data)) else "NULL"), 1)
          return(result)
        }
        debug_log("Primary data setter not available", 1)
        return(FALSE)
      }, error = function(e) {
        debug_log(paste("Error setting primary data:", e$message), 1)
        return(FALSE)
      })
    }

    get_secondary_data <- function() {
      tryCatch({
        if (!is.null(get_data2) && is.function(get_data2)) {
          data <- get_data2()
          debug_log(paste("Secondary data accessed -",
                          if (!is.null(data)) paste(nrow(data), "x", ncol(data)) else "NULL"), 2)
          return(data)
        }
        debug_log("Secondary data getter not available", 2)
        return(NULL)
      }, error = function(e) {
        debug_log(paste("Error accessing secondary data:", e$message), 1)
        return(NULL)
      })
    }

    get_primary_data_for_readiness <- function() {
      tryCatch({
        if (!is.null(get_data) && is.function(get_data)) get_data() else NULL
      }, error = function(e) NULL)
    }

    get_secondary_data_for_readiness <- function() {
      tryCatch({
        if (!is.null(get_data2) && is.function(get_data2)) get_data2() else NULL
      }, error = function(e) NULL)
    }

    merge_data_ready <- reactive({
      primary <- get_primary_data_for_readiness()
      secondary <- get_secondary_data_for_readiness()
      is.data.frame(primary) && nrow(primary) > 0 && ncol(primary) > 0 &&
        is.data.frame(secondary) && nrow(secondary) > 0 && ncol(secondary) > 0
    })

    last_merge_data_ready <- reactiveVal(NULL)
    observeEvent(merge_data_ready(), {
      ready <- isTRUE(merge_data_ready())
      previous_ready <- last_merge_data_ready()
      if (!ready && !identical(previous_ready, FALSE)) {
        debug_log("Merge data readiness changed to not-ready; preview and choice observers are paused until data is available", 2)
      }
      last_merge_data_ready(ready)
    }, ignoreInit = FALSE)

    # ========================================
    # IMPORT: apply merge UI config
    # ========================================
    set_merge_ui_config_from_import <- function(config) {
      tryCatch({
        if (is.null(config) || !is.list(config)) return(invisible(FALSE))

        debug_log("Applying merge UI config from import", 1)
        ui_config_source("import")
        ui_config_applied(TRUE)

        # join_type sofort (wenn gültig)
        if (!is.null(config$join_type)) {
          jt <- normalize_join_type(config$join_type)
          if (!is.null(jt) && jt %in% c("left","inner","full")) {
            freezeReactiveValue(input, "join_type")
            updateSelectInput(session, "join_type", selected = jt)
            debug_log(paste("Applied join_type:", jt), 2)
          } else {
            showNotification(paste("Merge: unsupported join_type ->", as.character(config$join_type)[1],
                                   "(supported: left, inner, full)"),
                             type = "warning", duration = 4)
            debug_log(paste("Unsupported join_type in import:", as.character(config$join_type)[1]), 1)
          }
        }

        # Rest puffern; wird gesetzt, sobald Choices vorhanden
        merge_import_config(list(
          file1_col     = if (!is.null(config$file1_col)) as.character(config$file1_col)[1] else NULL,
          file2_col     = if (!is.null(config$file2_col)) as.character(config$file2_col)[1] else NULL,
          file2_add_col = if (!is.null(config$file2_add_col)) as.character(config$file2_add_col) else NULL
        ))
        debug_log("Stored merge columns for deferred application", 2)

        invisible(TRUE)
      }, error = function(e) {
        showNotification(paste("Some merge settings could not be imported:", e$message),
                         type = "warning", duration = 5)
        debug_log(paste("Error in set_merge_ui_config_from_import:", e$message), 1)
        invisible(FALSE)
      })
    }

    # ========================================
    # Enhanced Validation Functions with Comprehensive Checks
    # ========================================

    validate_merge_inputs <- function() {
      tryCatch({
        errors <- character()
        warnings <- character()

        # Check primary data availability and validity
        primary_data <- get_primary_data()
        if (is.null(primary_data)) {
          errors <- c(errors, "Primary data not available")
        } else if (!is.data.frame(primary_data)) {
          errors <- c(errors, "Primary data is not a data frame")
        } else if (nrow(primary_data) == 0) {
          errors <- c(errors, "Primary data is empty")
        } else if (ncol(primary_data) == 0) {
          errors <- c(errors, "Primary data has no columns")
        }

        # Check secondary data availability and validity
        secondary_data <- get_secondary_data()
        if (is.null(secondary_data)) {
          errors <- c(errors, "Secondary data not available")
        } else if (!is.data.frame(secondary_data)) {
          errors <- c(errors, "Secondary data is not a data frame")
        } else if (nrow(secondary_data) == 0) {
          errors <- c(errors, "Secondary data is empty")
        } else if (ncol(secondary_data) == 0) {
          errors <- c(errors, "Secondary data has no columns")
        }

        # Check join column selections
        if (is.null(input$file1_col) || !nzchar(input$file1_col)) {
          errors <- c(errors, "Please select a join column for primary data")
        } else if (!is.null(primary_data) && !input$file1_col %in% names(primary_data)) {
          errors <- c(errors, "Selected join column not found in primary data")
        }

        if (is.null(input$file2_col) || !nzchar(input$file2_col)) {
          errors <- c(errors, "Please select a join column for secondary data")
        } else if (!is.null(secondary_data) && !input$file2_col %in% names(secondary_data)) {
          errors <- c(errors, "Selected join column not found in secondary data")
        }

        # Check additional columns if specified
        if (!is.null(input$file2_add_col) && length(input$file2_add_col) > 0 && !is.null(secondary_data)) {
          missing_cols <- setdiff(input$file2_add_col, names(secondary_data))
          if (length(missing_cols) > 0) {
            errors <- c(errors, paste("Additional columns not found:", paste(missing_cols, collapse = ", ")))
          }
        }

        # Check for potential data quality issues
        if (!is.null(primary_data) && !is.null(secondary_data) &&
            !is.null(input$file1_col) && !is.null(input$file2_col) &&
            input$file1_col %in% names(primary_data) &&
            input$file2_col %in% names(secondary_data)) {

          # Check for duplicate values in join columns
          primary_join_vals <- primary_data[[input$file1_col]]
          secondary_join_vals <- secondary_data[[input$file2_col]]

          if (any(duplicated(primary_join_vals, incomparables = NA))) {
            warnings <- c(warnings, "Primary join column contains duplicate values")
          }

          if (any(duplicated(secondary_join_vals, incomparables = NA))) {
            warnings <- c(warnings, "Secondary join column contains duplicate values")
          }

          # Check for missing values in join columns
          if (any(is.na(primary_join_vals))) {
            warnings <- c(warnings, "Primary join column contains missing values")
          }

          if (any(is.na(secondary_join_vals))) {
            warnings <- c(warnings, "Secondary join column contains missing values")
          }
        }

        debug_log(paste("Input validation - Errors:", length(errors), "Warnings:", length(warnings)), 2)

        return(list(
          valid = length(errors) == 0,
          errors = errors,
          warnings = warnings
        ))

      }, error = function(e) {
        error_msg <- paste("Error during input validation:", e$message)
        debug_log(error_msg, 1)
        return(list(
          valid = FALSE,
          errors = error_msg,
          warnings = character()
        ))
      })
    }

    # ========================================
    # Enhanced UI_config Management with Improved Error Handling
    # ========================================

    #' Apply UI configuration from import with enhanced timing fix
    apply_ui_config <- function(ui_config) {
      if (is.null(ui_config)) {
        debug_log("No UI config to apply", 2)
        return(TRUE)
      }

      if (ui_config_update_in_progress()) {
        debug_log("UI config update already in progress, skipping", 2)
        return(TRUE)
      }

      tryCatch({
        ui_config_update_in_progress(TRUE)
        debug_log("Applying UI config with enhanced timing", 2)

        if (!is.list(ui_config)) {
          debug_log("UI config is not a list", 1)
          ui_config_update_in_progress(FALSE)
          return(FALSE)
        }

        # Use session$onFlushed for delayed updates to ensure proper timing
        session$onFlushed(function() {
          tryCatch({
            debug_log("Executing delayed UI updates", 2)

            # Apply join type first (doesn't depend on data)
            if (!is.null(ui_config$join_type)) {
              valid_types <- c("left", "inner", "full")
              if (ui_config$join_type %in% valid_types) {
                freezeReactiveValue(input, "join_type")
                updateSelectInput(session, "join_type", selected = ui_config$join_type)
                debug_log(paste("Applied join type:", ui_config$join_type), 2)
              }
            }

            # Store column configs for later application when data becomes available
            if (!is.null(ui_config$file1_col)) {
              debug_log(paste("Stored file1_col for later application:", ui_config$file1_col), 2)
            }
            if (!is.null(ui_config$file2_col)) {
              debug_log(paste("Stored file2_col for later application:", ui_config$file2_col), 2)
            }
            if (!is.null(ui_config$file2_add_col)) {
              debug_log(paste("Stored file2_add_col for later application:", length(ui_config$file2_add_col), "columns"), 2)
            }

            ui_config_applied(TRUE)
            ui_config_source("import")
            ui_config_update_in_progress(FALSE)

            showNotification("Merge configuration applied successfully", type = "message", duration = 3)
            debug_log("UI config applied successfully", 1)

          }, error = function(e) {
            debug_log(paste("Error in delayed UI updates:", e$message), 1)
            ui_config_update_in_progress(FALSE)
          })
        }, once = TRUE)

        return(TRUE)

      }, error = function(e) {
        debug_log(paste("Error applying UI config:", e$message), 1)
        ui_config_update_in_progress(FALSE)
        return(FALSE)
      })
    }

    #' Get current UI state for export
    get_current_ui_state <- function() {
      tryCatch({
        ui_state <- list(
          file1_col = input$file1_col,
          file2_col = input$file2_col,
          file2_add_col = input$file2_add_col,
          join_type = input$join_type
        )

        debug_log(paste("Current UI state collected - File1:", ui_state$file1_col,
                        "File2:", ui_state$file2_col,
                        "Additional:", length(ui_state$file2_add_col %||% character(0)),
                        "Join:", ui_state$join_type), 2)

        return(ui_state)

      }, error = function(e) {
        debug_log(paste("Error collecting UI state:", e$message), 1)
        return(NULL)
      })
    }

    # ========================================
    # Enhanced UI_config Observers with Error Handling
    # ========================================

    # Watch for UI_config changes from assign_rules module
    observeEvent(UI_config(), {
      cfg_in <- UI_config()
      if (is.null(cfg_in)) return()
      # Unterstütze beide Varianten: direktes Merge-Objekt oder { merge = ... }
      cfg <- normalize_merge_config(cfg_in)
      set_merge_ui_config_from_import(cfg)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ========================================
    # Column choices and deferred import selections
    # ========================================
    observeEvent(list(primary_working_revision_debounced(), secondary_revision_debounced()), {
      req(merge_data_ready())
      primary_data <- isolate(get_primary_data())
      secondary_data <- isolate(get_secondary_data())
      req(is.data.frame(primary_data), is.data.frame(secondary_data))

      choices1 <- if (is.data.frame(primary_data) && ncol(primary_data) > 0) names(primary_data) else character(0)
      choices2 <- if (is.data.frame(secondary_data) && ncol(secondary_data) > 0) names(secondary_data) else character(0)
      column_signature <- get_merge_column_signature(primary_data, secondary_data)
      previous_signature <- last_merge_column_signature()

      current_selection1 <- isolate(input$file1_col)
      current_selection2 <- isolate(input$file2_col)
      current_selection2_add <- isolate(input$file2_add_col) %||% character(0)

      selected1 <- valid_single_selection(current_selection1, choices1)
      selected2 <- valid_single_selection(current_selection2, choices2)
      selected2_add <- intersect(current_selection2_add, choices2)

      dropped_parts <- character(0)
      if (!is.null(current_selection1) && length(current_selection1) > 0 && !current_selection1[1] %in% choices1) {
        dropped_parts <- c(dropped_parts, paste0("file1_col=", current_selection1[1]))
      }
      if (!is.null(current_selection2) && length(current_selection2) > 0 && !current_selection2[1] %in% choices2) {
        dropped_parts <- c(dropped_parts, paste0("file2_col=", current_selection2[1]))
      }
      dropped_add <- setdiff(current_selection2_add, choices2)
      if (length(dropped_add) > 0) {
        dropped_parts <- c(dropped_parts, paste0("file2_add_col=", paste(dropped_add, collapse = ", ")))
      }
      log_dropped_merge_selections(dropped_parts, previous_signature, column_signature)

      pending <- merge_import_config()
      if (!is.null(pending)) {
        primary_ready <- is.data.frame(primary_data) && ncol(primary_data) > 0
        secondary_ready <- is.data.frame(secondary_data) && ncol(secondary_data) > 0
        pending_resolved <- TRUE
        applied_any <- FALSE

        if (!is.null(pending$file1_col)) {
          if (primary_ready) {
            if (pending$file1_col %in% choices1) {
              selected1 <- pending$file1_col
              applied_any <- TRUE
            } else {
              showNotification(paste("Merge: file1_col not found in primary data:", pending$file1_col),
                               type = "warning", duration = 4)
            }
          } else {
            pending_resolved <- FALSE
          }
        }

        if (!is.null(pending$file2_col)) {
          if (secondary_ready) {
            if (pending$file2_col %in% choices2) {
              selected2 <- pending$file2_col
              applied_any <- TRUE
            } else {
              showNotification(paste("Merge: file2_col not found in secondary data:", pending$file2_col),
                               type = "warning", duration = 4)
            }
          } else {
            pending_resolved <- FALSE
          }
        }

        if (!is.null(pending$file2_add_col) && length(pending$file2_add_col) > 0) {
          if (secondary_ready) {
            valid_add <- intersect(pending$file2_add_col, choices2)
            if (length(valid_add) > 0) {
              selected2_add <- valid_add
              applied_any <- TRUE
            } else {
              selected2_add <- character(0)
              showNotification("Merge: none of file2_add_col found in secondary data",
                               type = "warning", duration = 4)
            }
          } else {
            pending_resolved <- FALSE
          }
        }

        if (applied_any) {
          debug_log("Merge import selections applied", 1)
        }
        if (pending_resolved) {
          merge_import_config(NULL)
        }
      }

      freezeReactiveValue(input, "file1_col")
      updateSelectInput(
        session, "file1_col",
        choices = choices1,
        selected = selected1
      )

      freezeReactiveValue(input, "file2_col")
      updateSelectInput(
        session, "file2_col",
        choices = choices2,
        selected = selected2
      )

      freezeReactiveValue(input, "file2_add_col")
      updateSelectInput(
        session, "file2_add_col",
        choices = choices2,
        selected = if (length(selected2_add) > 0) selected2_add else character(0)
      )

      last_merge_column_signature(column_signature)
    })

    # Update UI config source when user makes manual changes
    observeEvent(list(input$file1_col, input$file2_col, input$file2_add_col, input$join_type), {
      if (ui_config_source() == "import" && !ui_config_update_in_progress()) {
        ui_config_source("user_modified")
        debug_log("UI config source changed to user_modified", 2)
      }
    }, ignoreInit = TRUE)

    # ========================================
    # Enhanced Preview Logic with Robust Error Handling
    # ========================================

    # merge_preview_data <- reactive({
    #   tryCatch({
    #     validation <- validate_merge_inputs()
    #     if (!validation$valid) {
    #       return(list(error = validation$errors[1]))
    #     }
    #
    #     if (length(validation$warnings) > 0) {
    #       debug_log(paste("Preview warnings:", paste(validation$warnings, collapse = "; ")), 2)
    #     }
    #
    #     primary_data <- get_primary_data()
    #     secondary_data <- get_secondary_data()
    #
    #     # Take samples for preview to avoid performance issues
    #     sample_size <- 100
    #     primary_sample <- if (nrow(primary_data) > sample_size) {
    #       primary_data[sample(nrow(primary_data), sample_size), ]
    #     } else {
    #       primary_data
    #     }
    #
    #     secondary_sample <- if (nrow(secondary_data) > sample_size) {
    #       secondary_data[sample(nrow(secondary_data), sample_size), ]
    #     } else {
    #       secondary_data
    #     }
    #
    #     # Prepare columns to include from secondary data
    #     file2_cols <- c(input$file2_col)  # Always include join column
    #     if (!is.null(input$file2_add_col) && length(input$file2_add_col) > 0) {
    #       file2_cols <- c(file2_cols, input$file2_add_col)
    #     }
    #     file2_cols <- unique(file2_cols)
    #
    #     # Check if dplyr is available
    #     if (!requireNamespace("dplyr", quietly = TRUE)) {
    #       return(list(error = "dplyr package required for merge operations"))
    #     }
    #
    #     secondary_subset <- secondary_sample[, file2_cols, drop = FALSE]
    #     join_type <- input$join_type %||% "left"
    #
    #     # Perform merge based on join type
    #     result <- switch(join_type,
    #                      "left" = dplyr::left_join(
    #                        primary_sample,
    #                        secondary_subset,
    #                        by = setNames(input$file2_col, input$file1_col)
    #                      ),
    #                      "inner" = dplyr::inner_join(
    #                        primary_sample,
    #                        secondary_subset,
    #                        by = setNames(input$file2_col, input$file1_col)
    #                      ),
    #                      "full" = dplyr::full_join(
    #                        primary_sample,
    #                        secondary_subset,
    #                        by = setNames(input$file2_col, input$file1_col)
    #                      ),
    #                      stop("Invalid join type")
    #     )
    #
    #     debug_log(paste("Preview generated -", nrow(result), "x", ncol(result), "result"), 2)
    #     return(result)
    #
    #   }, error = function(e) {
    #     debug_log(paste("Preview error:", e$message), 1)
    #     return(list(error = e$message))
    #   })
    # })

    merge_preview_info <- reactive({
      req(merge_data_ready())
      tryCatch({
        # Daten holen
        primary <- get_primary_data()
        secondary <- get_secondary_data()
        if (is.null(primary) || !is.data.frame(primary) || nrow(primary) == 0) {
          return(list(error = "Primary data not available"))
        }
        if (is.null(secondary) || !is.data.frame(secondary) || nrow(secondary) == 0) {
          return(list(error = "Secondary data not available"))
        }
        # Inputs prüfen
        f1 <- input$file1_col
        f2 <- input$file2_col
        add2 <- input$file2_add_col
        jt <- input$join_type %||% "left"
        if (is.null(f1) || !nzchar(f1) || !(f1 %in% names(primary))) {
          return(list(error = "Select a valid primary join column"))
        }
        if (is.null(f2) || !nzchar(f2) || !(f2 %in% names(secondary))) {
          return(list(error = "Select a valid secondary join column"))
        }

        # Gültige Zusatzspalten aus file2
        add2_valid <- intersect(as.character(add2 %||% character(0)), names(secondary))
        # Spaltenzahl (dplyr::join: by-Spalte aus y wird nicht dupliziert)
        ncols_res <- ncol(primary) + length(add2_valid)

        # Zeilenzahl exakt, ohne Full-Join zu materialisieren:
        # - Zähle Duplikate der Schlüssel in secondary
        sec_key <- secondary[[f2]]
        # Zähle NA separat? Join mit NA in dplyr matched NA zu NA -> behandeln wir identisch
        # Erzeuge Count-Tabelle (Basis-R vektorisiert, speicherschonend)
        # Verwende match() für Mapping
        sec_levels <- unique(sec_key)
        # counts für jeden unique key
        # Hinweis: table() ist schnell, aber liefert factor/character; wir nutzen tapply
        sec_counts <- tapply(rep(1L, length(sec_key)), sec_key, sum, simplify = TRUE)
        # Vektor mit counts pro primary-Key (gleiche Namen wie sec_counts)
        idx <- match(primary[[f1]], names(sec_counts))
        nmatch <- as.integer(sec_counts[idx])
        # left/inner/full
        if (jt == "left") {
          nrows_res <- sum(ifelse(is.na(nmatch), 1L, nmatch))
        } else if (jt == "inner") {
          nrows_res <- sum(ifelse(is.na(nmatch), 0L, nmatch))
        } else if (jt == "full") {
          # links wie left
          left_rows <- sum(ifelse(is.na(nmatch), 1L, nmatch))
          # plus alle sec-Keys, die in primary nicht vorkommen (mit ihren counts)
          idx2 <- match(names(sec_counts), primary[[f1]])
          extra <- sum(as.integer(sec_counts[is.na(idx2)]))
          nrows_res <- left_rows + extra
        } else {
          return(list(error = paste("Unsupported join type:", jt)))
        }

        # Kleine Kopf-Tabelle (erste 5 Zeilen) erzeugen:
        # Sekundär-Subset (nur benötigte Spalten)
        sec_subset_cols <- unique(c(f2, add2_valid))
        secondary_subset <- secondary[, sec_subset_cols, drop = FALSE]

        # Primär-Subset bevorzugt mit Keys, die Matches haben, damit Vorschau zeigt, was passiert
        keys_with_matches <- intersect(unique(primary[[f1]]), unique(sec_key))
        if (length(keys_with_matches) > 0) {
          primary_sample <- primary[primary[[f1]] %in% keys_with_matches, , drop = TRUE]
          if (nrow(primary_sample) > 50) primary_sample <- head(primary_sample, 50)
        } else {
          primary_sample <- head(primary, 50)
        }

        # Join für die Vorschau (klein halten)
        if (!requireNamespace("dplyr", quietly = TRUE)) {
          return(list(error = "dplyr package required for merge preview"))
        }
        head_tbl <- tryCatch({
          join_by <- setNames(f2, f1)
          naming_prep <- prepare_secondary_for_merge(primary_sample, secondary_subset, f2)
          secondary_subset <- naming_prep$secondary_subset
          join_with_relationship <- function(join_fun, x, y, by, suffix = c(".x", ".y")) {
            tryCatch(
              join_fun(x, y, by = by, relationship = "many-to-many", suffix = suffix),
              error = function(e) {
                if (grepl("unused argument", conditionMessage(e), fixed = TRUE)) {
                  join_fun(x, y, by = by, suffix = suffix)
                } else {
                  stop(e)
                }
              }
            )
          }
          if (jt == "left") {
            join_with_relationship(dplyr::left_join, primary_sample, secondary_subset, join_by)
          } else if (jt == "inner") {
            join_with_relationship(dplyr::inner_join, primary_sample, secondary_subset, join_by)
          } else { # full
            join_with_relationship(dplyr::full_join, primary_sample, secondary_subset, join_by)
          }
        }, error = function(e) NULL)

        if (!is.null(head_tbl)) {
          head_tbl <- utils::head(head_tbl, 5)
        } else {
          head_tbl <- data.frame(Info = "Preview unavailable due to join error")
        }

        list(dim = c(nrows_res, ncols_res), head = head_tbl)
      }, error = function(e) {
        list(error = paste("Error preparing merge preview:", e$message))
      })
    })

    output$merge_ready <- renderText({
      if (isTRUE(merge_data_ready())) "true" else "false"
    })

    outputOptions(output, "merge_ready", suspendWhenHidden = FALSE)

    # Dimensions-Text rendern
    output$merge_preview_dim <- renderText({
      req(merge_data_ready())
      info <- merge_preview_info()
      if (is.null(info)) return("Configure merge options to see preview...")
      if (!is.null(info$error)) return(info$error)
      paste0("Dimensions: ", info$dim[1], " rows x ", info$dim[2], " columns")
    })

    # Scrollbare Tabelle mit den ersten 5 Zeilen
    output$merge_preview_table <- DT::renderDT({
      normalize_preview_for_dt <- function(x) {
        if (is.null(x) || !is.data.frame(x)) {
          return(data.frame(Info = "Preview unavailable", stringsAsFactors = FALSE))
        }
        out <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
        nms <- names(out)
        nms[is.na(nms) | nms == ""] <- paste0("Unnamed_", seq_len(sum(is.na(nms) | nms == "")))
        names(out) <- make.unique(nms, sep = "_dup_")
        if (ncol(out) == 0) {
          return(data.frame(Info = "Preview has no columns", stringsAsFactors = FALSE))
        }
        for (j in seq_along(out)) {
          if (is.list(out[[j]])) out[[j]] <- vapply(out[[j]], toString, character(1))
        }
        out
      }

      req(merge_data_ready())
      info <- merge_preview_info()
      if (is.null(info) || !is.null(info$error)) {
        return(DT::datatable(
          data.frame(Info = if (!is.null(info$error)) info$error else "Configure merge options to see preview..."),
          options = list(dom = 't', pageLength = 1, scrollX = TRUE, searching = FALSE),
          rownames = FALSE
        ))
      }
      head_tbl <- normalize_preview_for_dt(info$head)
      DT::datatable(
        head_tbl,
        options = list(scrollX = TRUE, pageLength = 5, dom = 't', searching = FALSE),
        rownames = FALSE
      )
    }, server = FALSE)


    # output$merge_preview <- renderText({
    #   tryCatch({
    #     preview_result <- merge_preview_data()
    #
    #     if (is.list(preview_result) && !is.null(preview_result$error)) {
    #       return(paste("Error:", preview_result$error))
    #     }
    #
    #     primary_data <- get_primary_data()
    #     secondary_data <- get_secondary_data()
    #
    #     if (is.null(primary_data) || is.null(secondary_data)) {
    #       return("Load both primary and secondary data to see preview...")
    #     }
    #
    #     validation <- validate_merge_inputs()
    #     if (!validation$valid) {
    #       return(paste("Configure merge settings:", validation$errors[1]))
    #     }
    #
    #     # Format preview information
    #     info_lines <- c(
    #       "Preview (sample):",
    #       paste("Primary data:", nrow(primary_data), "rows x", ncol(primary_data), "columns"),
    #       paste("Secondary data:", nrow(secondary_data), "rows x", ncol(secondary_data), "columns"),
    #       paste("Result:", nrow(preview_result), "rows x", ncol(preview_result), "columns"),
    #       paste("Join type:", input$join_type),
    #       paste("Join columns:", input$file1_col, "⟷", input$file2_col)
    #     )
    #
    #     if (length(validation$warnings) > 0) {
    #       info_lines <- c(info_lines, "", "Warnings:", validation$warnings)
    #     }
    #
    #     return(paste(info_lines, collapse = "\n"))
    #
    #   }, error = function(e) {
    #     debug_log(paste("Error rendering preview:", e$message), 1)
    #     return(paste("Error rendering preview:", e$message))
    #   })
    # })

    # ========================================
    # Enhanced Preview Modal Handler
    # ========================================

    # observeEvent(input$preview_merge, {
    #   tryCatch({
    #     operation_start_time <- Sys.time()
    #     add_operation_log("preview", "starting", "Generating preview modal")
    #
    #     preview_result <- merge_preview_data()
    #
    #     if (is.list(preview_result) && !is.null(preview_result$error)) {
    #       showNotification(paste("Preview error:", preview_result$error),
    #                        type = "error", duration = 6)
    #       add_operation_log("preview", "error", preview_result$error)
    #       return()
    #     }
    #
    #     operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
    #     add_operation_log("preview", "success", "Preview modal generated", operation_duration)
    #
    #     # Show detailed preview modal
    #     showModal(modalDialog(
    #       title = "Merge Preview",
    #       size = "l",
    #       div(
    #         h5("Merge Information:"),
    #         verbatimTextOutput(ns("preview_info")),
    #         br(),
    #         h5("First few rows of merged data:"),
    #         DT::DTOutput(ns("preview_table"))
    #       ),
    #       footer = modalButton("Close")
    #     ))
    #
    #     output$preview_info <- renderText({
    #       primary_data <- get_primary_data()
    #       secondary_data <- get_secondary_data()
    #
    #       info_lines <- c(
    #         paste("Primary data dimensions:", nrow(primary_data), "x", ncol(primary_data)),
    #         paste("Secondary data dimensions:", nrow(secondary_data), "x", ncol(secondary_data)),
    #         paste("Preview result dimensions:", nrow(preview_result), "x", ncol(preview_result)),
    #         paste("Join type:", input$join_type),
    #         paste("Join columns:", input$file1_col, "⟷", input$file2_col),
    #         paste("Additional columns from secondary:",
    #               if (!is.null(input$file2_add_col)) length(input$file2_add_col) else 0)
    #       )
    #
    #       return(paste(info_lines, collapse = "\n"))
    #     })
    #
    #     output$preview_table <- DT::renderDT({
    #       DT::datatable(
    #         head(preview_result, 20),
    #         options = list(
    #           scrollX = TRUE,
    #           pageLength = 10,
    #           dom = 't'
    #         )
    #       )
    #     })
    #
    #   }, error = function(e) {
    #     debug_log(paste("Error in preview modal:", e$message), 1)
    #     showNotification(paste("Error generating preview:", e$message),
    #                      type = "error", duration = 6)
    #     add_operation_log("preview", "error", e$message)
    #   })
    # })

    # ========================================
    # Enhanced Apply Merge Handler with Comprehensive Error Handling
    # ========================================

    observeEvent(input$apply_merge, {
      # Prevent multiple simultaneous operations
      if (merge_processing_active()) {
        debug_log("Merge operation already active - ignoring button press", 2)
        showNotification("Merge operation already in progress", type = "warning", duration = 4)
        return()
      }

      tryCatch({
        operation_start_time <- Sys.time()
        merge_processing_active(TRUE)
        debug_log("Starting merge operation", 1)

        # Enhanced input validation
        validation <- validate_merge_inputs()
        if (!validation$valid) {
          error_msg <- paste("Input validation failed:", paste(validation$errors, collapse = "; "))
          debug_log(error_msg, 1)
          showNotification(error_msg, type = "error", duration = 8)
          add_operation_log("apply_merge", "error", "Validation failed")
          return()
        }

        if (length(validation$warnings) > 0) {
          warning_msg <- paste("Validation warnings:", paste(validation$warnings, collapse = "; "))
          debug_log(warning_msg, 1)
          showNotification(warning_msg, type = "warning", duration = 5)
        }

        add_operation_log("apply_merge", "starting", "Beginning merge operation")

        # Set up progress tracking
        progress <- NULL

        tryCatch({
          progress <- shiny::Progress$new()
          progress$set(message = "Preparing merge operation...", value = 0)
          debug_log("Progress tracking initialized", 2)

          primary_data <- get_primary_data()
          secondary_data <- get_secondary_data()

          progress$set(value = 0.2, message = "Preparing data...")

          # Prepare columns to include from secondary data
          file2_cols <- c(input$file2_col)  # Always include join column
          if (!is.null(input$file2_add_col) && length(input$file2_add_col) > 0) {
            file2_cols <- c(file2_cols, input$file2_add_col)
          }
          file2_cols <- unique(file2_cols)

          secondary_subset <- secondary_data[, file2_cols, drop = FALSE]

          progress$set(value = 0.5, message = "Performing merge...")

          # Check dplyr availability
          if (!requireNamespace("dplyr", quietly = TRUE)) {
            stop("dplyr package required for merge operations")
          }

          join_type <- input$join_type %||% "left"
          debug_log(paste("Executing", join_type, "join"), 2)

          # Perform merge operation with robust naming strategy
          join_by <- setNames(input$file2_col, input$file1_col)
          join_with_relationship <- function(join_fun, x, y, by, suffix = c(".x", ".y")) {
            tryCatch(
              join_fun(x, y, by = by, relationship = "many-to-many", suffix = suffix),
              error = function(e) {
                if (grepl("unused argument", conditionMessage(e), fixed = TRUE)) {
                  join_fun(x, y, by = by, suffix = suffix)
                } else {
                  stop(e)
                }
              }
            )
          }

          # Naming policy for secondary columns:
          # 1) Preferred: rename incoming (y) columns to Merged_<name> before join.
          #    This keeps primary (x) headers untouched (especially for left joins).
          # 2) Fallback: if prefixed names would collide, keep original names and let
          #    dplyr suffix both sides with .x/.y for overlapping names.
          naming_prep <- prepare_secondary_for_merge(primary_data, secondary_subset, input$file2_col)
          secondary_subset <- naming_prep$secondary_subset

          if (!naming_prep$used_fallback && naming_prep$prefixed_count > 0) {
            debug_log(paste("Applied secondary prefix to", naming_prep$prefixed_count, "columns"), 2)
          } else if (naming_prep$used_fallback) {
            debug_log(paste("Using suffix fallback (.x/.y):", paste(naming_prep$fallback_reasons, collapse = "; ")), 1)
            showNotification(
              paste("Merge naming fallback to .x/.y:", paste(naming_prep$fallback_reasons, collapse = "; ")),
              type = "warning",
              duration = 6
            )
          }

          result <- switch(join_type,
                           "left" = join_with_relationship(
                             dplyr::left_join,
                             primary_data,
                             secondary_subset,
                             join_by,
                             suffix = c(".x", ".y")
                           ),
                           "inner" = join_with_relationship(
                             dplyr::inner_join,
                             primary_data,
                             secondary_subset,
                             join_by,
                             suffix = c(".x", ".y")
                           ),
                           "full" = join_with_relationship(
                             dplyr::full_join,
                             primary_data,
                             secondary_subset,
                             join_by,
                             suffix = c(".x", ".y")
                           ),
                           stop("Invalid join type")
          )

          ensure_primary_not_shrunk(result, primary_data, join_type)

          # Normalize new columns: flatten list-cols and type-convert (strings -> numeric when possible)
          new_columns <- setdiff(names(result), names(primary_data))
          if (length(new_columns) > 0) {
            for (col in new_columns) {
              result[[col]] <- normalize_merged_column(result[[col]])
            }
          }

          progress$set(value = 0.8, message = "Processing result...")

          # Added columns are determined after join; naming is handled before join (or via suffix fallback).
          new_columns <- setdiff(names(result), names(primary_data))

          progress$set(value = 0.9, message = "Updating data...")

          # Update the primary data
          success <- set_primary_data(result)

          if (!success) {
            stop("Could not update data after merge operation")
          }

          set_data(result)

          operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
          last_operation_time(operation_duration)

          # Add to operation history
          history_entry <- list(
            timestamp = Sys.time(),
            operation = "merge",
            join_type = join_type,
            original_rows = nrow(primary_data),
            result_rows = nrow(result),
            added_columns = length(new_columns),
            duration = operation_duration,
            success = TRUE
          )
          current_history <- operation_history()
          operation_history(c(current_history, list(history_entry)))

          progress$set(value = 1.0, message = "Complete!")

          add_operation_log("apply_merge", "success",
                            paste("Merge completed -", nrow(primary_data), "→", nrow(result), "rows"),
                            operation_duration)

          # Enhanced success notification
          success_msg <- paste("Merge operation completed successfully!",
                               "Join type:", join_type,
                               sprintf("| Duration: %.2fs", operation_duration),
                               sprintf("| %d → %d rows", nrow(primary_data), nrow(result)),
                               sprintf("| Added %d columns", length(new_columns)))
          debug_log(success_msg, 1)

          # Level-0 merge summary (must be emitted exactly once per merge button click)
          collapse_cols <- function(x) {
            if (is.null(x) || length(x) == 0) return("(none)")
            paste(as.character(x), collapse = ", ")
          }
          added_primary_cols <- setdiff(names(result), names(primary_data))
          merge_summary_lvl0 <- paste0(
            "Merge summary | Primary join column: ", as.character(input$file1_col),
            " | Secondary join column: ", as.character(input$file2_col),
            " | New columns in primary after merge: [", collapse_cols(added_primary_cols), "]",
            " | Final dimensions: ", nrow(result), " x ", ncol(result)
          )
          debug_log(merge_summary_lvl0, level = 0)
          showNotification(success_msg, type = "message", duration = 4)

        }, error = function(e) {
          operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
          err_msg <- conditionMessage(e)
          if (is.null(err_msg) || !nzchar(trimws(err_msg))) {
            err_msg <- paste("Unhandled", class(e)[1], "during merge operation")
          }
          add_operation_log("apply_merge", "error", err_msg, operation_duration)
          debug_log(paste("Merge operation failed:", err_msg), 1)
          showNotification(paste("Error during merge operation:", err_msg), type = "error", duration = 8)
        }, finally = {
          if (!is.null(progress)) {
            progress$close()
            debug_log("Progress dialog closed", 2)
          }
        })

      }, error = function(e) {
        debug_log(paste("Critical error in apply merge handler:", e$message), 1)
        showNotification(paste("Critical error:", e$message), type = "error", duration = 10)
      }, finally = {
        merge_processing_active(FALSE)
        debug_log("Merge operation completed (active flag cleared)", 2)
      })
    })

    # ========================================
    # Enhanced Status Display with Comprehensive Information
    # ========================================

    # output$merge_status <- renderText({
    #   tryCatch({
    #     status_lines <- character()
    #
    #     # Data availability status
    #     primary_data <- get_primary_data()
    #     secondary_data <- get_secondary_data()
    #
    #     if (!is.null(primary_data)) {
    #       status_lines <- c(status_lines, paste("Primary data:", nrow(primary_data), "rows x", ncol(primary_data), "columns"))
    #     } else {
    #       status_lines <- c(status_lines, "Primary data: Not loaded")
    #     }
    #
    #     if (!is.null(secondary_data)) {
    #       status_lines <- c(status_lines, paste("Secondary data:", nrow(secondary_data), "rows x", ncol(secondary_data), "columns"))
    #     } else {
    #       status_lines <- c(status_lines, "Secondary data: Not loaded")
    #     }
    #
    #     # Configuration status
    #     if (!is.null(input$file1_col) && nzchar(input$file1_col)) {
    #       status_lines <- c(status_lines, paste("Primary join column:", input$file1_col))
    #     }
    #
    #     if (!is.null(input$file2_col) && nzchar(input$file2_col)) {
    #       status_lines <- c(status_lines, paste("Secondary join column:", input$file2_col))
    #     }
    #
    #     if (!is.null(input$file2_add_col) && length(input$file2_add_col) > 0) {
    #       status_lines <- c(status_lines, paste("Additional columns:", length(input$file2_add_col)))
    #     }
    #
    #     if (!is.null(input$join_type)) {
    #       status_lines <- c(status_lines, paste("Join type:", input$join_type))
    #     }
    #
    #     # UI config status
    #     if (ui_config_applied()) {
    #       status_lines <- c(status_lines, paste("UI config:", ui_config_source()))
    #     }
    #
    #     # Validation status
    #     validation <- validate_merge_inputs()
    #     if (validation$valid) {
    #       status_lines <- c(status_lines, "✓ Ready to merge")
    #     } else {
    #       status_lines <- c(status_lines, paste("⚠", validation$errors[1]))
    #     }
    #
    #     # Recent operation log entries
    #     log_entries <- operation_log()
    #     if (length(log_entries) > 0) {
    #       status_lines <- c(status_lines, "", "=== RECENT OPERATIONS ===")
    #       recent_entries <- tail(log_entries, 3)
    #       for (entry in recent_entries) {
    #         status_indicator <- switch(entry$status,
    #                                    "success" = "✓",
    #                                    "error" = "✗",
    #                                    "warning" = "⚠",
    #                                    "ℹ")
    #         status_lines <- c(status_lines,
    #                           paste(format(entry$timestamp, "%H:%M:%S"),
    #                                 status_indicator,
    #                                 entry$operation))
    #       }
    #     }
    #
    #     # Show current operation if processing
    #     if (merge_processing_active()) {
    #       status_lines <- c(status_lines, "", "🔄 MERGE IN PROGRESS")
    #     }
    #
    #     # Performance metrics
    #     operation_time <- last_operation_time()
    #     if (!is.null(operation_time)) {
    #       status_lines <- c(status_lines, sprintf("Last operation: %.2fs", operation_time))
    #     }
    #
    #     # Error count
    #     error_count <- length(processing_errors())
    #     if (error_count > 0) {
    #       status_lines <- c(status_lines, paste("⚠ Recent errors:", error_count))
    #     }
    #
    #     return(paste(status_lines, collapse = "\n"))
    #
    #   }, error = function(e) {
    #     debug_log(paste("Error rendering merge status:", e$message), 1)
    #     return(paste("Error rendering status:", e$message))
    #   })
    # })
    #
    # ========================================
    # Enhanced Interface Functions
    # ========================================

    #' Check if merge is configured
    is_merge_configured <- function() {
      tryCatch({
        validation <- validate_merge_inputs()
        return(validation$valid)
      }, error = function(e) {
        debug_log(paste("Error checking merge configuration:", e$message), 1)
        return(FALSE)
      })
    }

    #' Get merge summary
    get_merge_summary <- function() {
      tryCatch({
        if (!is_merge_configured()) {
          return("No merge configured")
        }

        join_type <- input$join_type %||% "left"
        add_cols <- if (!is.null(input$file2_add_col)) length(input$file2_add_col) else 0
        return(paste("Join type:", join_type, "| Additional columns:", add_cols))
      }, error = function(e) {
        debug_log(paste("Error getting merge summary:", e$message), 1)
        return("Error getting summary")
      })
    }

    #' Module health check function
    module_health_check <- function() {
      tryCatch({
        health_status <- list(
          module_name = "Merge",
          status = "OK",
          processing_active = merge_processing_active(),
          error_count = length(processing_errors()),
          debug_level = DEBUG_LEVEL,
          last_operation_time = last_operation_time(),
          ui_config_applied = ui_config_applied(),
          ui_config_source = ui_config_source()
        )

        # Check for potential issues
        warnings <- character()

        validation <- validate_merge_inputs()
        if (!validation$valid) {
          warnings <- c(warnings, "Configuration incomplete")
        }

        if (length(validation$warnings) > 0) {
          warnings <- c(warnings, "Data quality warnings present")
        }

        if (health_status$error_count > 3) {
          warnings <- c(warnings, paste("High error count:", health_status$error_count))
        }

        # Check data availability
        if (is.null(get_primary_data())) {
          warnings <- c(warnings, "Primary data not available")
        }

        if (is.null(get_secondary_data())) {
          warnings <- c(warnings, "Secondary data not available")
        }

        health_status$warnings <- warnings
        health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

        debug_log(paste("Module health check - Status:", health_status$overall_health), 2)
        return(health_status)

      }, error = function(e) {
        debug_log(paste("Error in module health check:", e$message), 1)
        return(list(
          module_name = "Merge",
          status = "ERROR",
          error_message = e$message,
          overall_health = "Critical"
        ))
      })
    }

    # ========================================
    # Session Cleanup with Enhanced Error Handling
    # ========================================

    # Register cleanup function
    cleanup_manager$register_module("Merge", function() {
      debug_log("Executing [Merge] cleanup", 2)

      # Clear all reactive values safely
      merge_processing_active(FALSE)
      last_operation_time(NULL)
      operation_history(list())
      processing_errors(list())
      operation_log(list())
      ui_config_applied(FALSE)
      ui_config_source("none")
      ui_config_update_in_progress(FALSE)

      debug_log("[Merge] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    merge_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        file1_col     = "selectizeInput",
        file2_col     = "selectizeInput",
        file2_add_col = "selectizeInput",
        join_type     = "selectInput"
      ),
      module_label    = "Merge",
      restore_trigger = session_restore_trigger
    )

    # ========================================
    # Enhanced Return Values with Comprehensive Interface
    # ========================================

    return(list(
      # Core functionality
      get_primary_data = get_primary_data,
      set_primary_data = set_primary_data,
      get_secondary_data = get_secondary_data,

      # Session-restore bridge
      get_session_state = merge_session_state$get_session_state,
      set_session_state = merge_session_state$set_session_state,

      # UI_config management
      get_current_ui_state = function() {
        list(
          file1_col = input$file1_col,
          file2_col = input$file2_col,
          file2_add_col = input$file2_add_col,
          join_type = input$join_type
        )
      },
      apply_ui_config = function(cfg) {
        # Backward-compat: akzeptiere alten Weg
        set_merge_ui_config_from_import(cfg)
        invisible(TRUE)
      },
      set_merge_ui_config_from_import = set_merge_ui_config_from_import,
      ui_config_applied = reactive({ ui_config_applied() }),
      ui_config_source  = reactive({ ui_config_source() }),

      # Exponiere Inputs als reactive (einmal!)
      file1_col = reactive({ input$file1_col }),
      file2_col = reactive({ input$file2_col }),
      file2_add_col = reactive({ input$file2_add_col }),
      join_type = reactive({ input$join_type }),

      # Configuration status
      is_merge_configured = is_merge_configured,
      get_merge_summary = get_merge_summary,
      validate_merge_inputs = validate_merge_inputs,

      # Preview functionality
      # merge_preview_data = merge_preview_data,

      # Action triggers
      apply_trigger = reactive({ input$apply_merge }),
      # preview_trigger = reactive({ input$preview_merge }),

      # Enhanced status and monitoring
      merge_processing_active = reactive({ merge_processing_active() }),
      processing_errors = reactive({ processing_errors() }),
      operation_log = reactive({ operation_log() }),
      operation_history = reactive({ operation_history() }),

      # Error management
      get_processing_errors = function() { processing_errors() },
      clear_processing_errors = function() {
        tryCatch({
          processing_errors(list())
          debug_log("Processing errors cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing processing errors:", e$message), 1)
        })
      },

      # Performance monitoring
      get_performance_metrics = reactive({
        tryCatch({
          list(
            last_operation_time = last_operation_time(),
            operation_history = operation_history(),
            debug_level = DEBUG_LEVEL
          )
        }, error = function(e) {
          debug_log(paste("Error getting performance metrics:", e$message), 1)
          list(
            last_operation_time = NULL,
            operation_history = list(),
            debug_level = DEBUG_LEVEL
          )
        })
      }),

      # Module health check
      module_health_check = module_health_check,

      # Debug and testing functions
      test_module_functionality = function() {
        debug_log("=== TESTING MERGE MODULE ===", 1)
        debug_log(paste("Processing active:", merge_processing_active()), 1)
        debug_log(paste("Error count:", length(processing_errors())), 1)
        debug_log(paste("Debug level:", DEBUG_LEVEL), 1)
        debug_log(paste("UI config applied:", ui_config_applied()), 1)
        debug_log(paste("UI config source:", ui_config_source()), 1)
        debug_log(paste("Merge configured:", is_merge_configured()), 1)
        health <- module_health_check()
        debug_log(paste("Module health:", health$overall_health), 1)
        debug_log("=== END TESTING ===", 1)
      }
    ))
  })
}
