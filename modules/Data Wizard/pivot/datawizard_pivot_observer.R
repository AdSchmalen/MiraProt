# ============================================================================
# File: modules/Data Wizard/pivot/datawizard_pivot_observer.R
# Purpose:
#   Register all reactive outputs, observeEvent handlers, session cleanup,
#   and assemble the public API for the Data Wizard Pivot submodule.
#
# Architecture role:
#   Observer/reactive layer for Pivot. This file centralizes all reactive and
#   event-driven wiring so the orchestrator stays slim. Pure computation lives
#   in datawizard_pivot_logic.R; static UI in datawizard_pivot_ui.R; reactive
#   state containers in datawizard_pivot_state.R.
#
# Exported functions (called from the orchestrator):
#   - pivot_register_outputs(ctx)   : wires all renderXxx outputs and reactives;
#                                     returns list(get_current_data, pivot_preview_data)
#   - pivot_register_observers(ctx) : wires observeEvent handlers for UI changes
#                                     and apply/confirm pivot actions
#   - pivot_register_cleanup(ctx)   : registers session cleanup with cleanup_manager
#   - pivot_build_api(ctx)          : constructs and returns the public module API list
#
# Safe maintenance notes:
#   - Keep observer side effects focused; delegate heavy work to logic.R helpers.
#   - Ensure all dependencies are passed explicitly via `ctx`.
#   - Do not add non-observer helper logic here except as local inner functions
#     required by the registered handlers.
# ============================================================================

# ============================================================================
# pivot_register_outputs
# ============================================================================

#' Register all reactive outputs for the Pivot module.
#'
#' Wires get_current_data reactive, all renderXxx output blocks, and
#' outputOptions. Called once from modPivotServer.
#'
#' Required ctx keys: input, output, ns, get_primary_data, get_secondary_data,
#'   add_pivot_error, debug_log, pivot_options_state, preview_error_count,
#'   preview_last_error_time.
#'
#' @param ctx named list of dependencies from the orchestrator
#' @return list(get_current_data = reactive, pivot_preview_data = reactive)
pivot_register_outputs <- function(ctx) {

  # ------------------------------------------------------------------
  # Data selection reactive
  # ------------------------------------------------------------------

  pivot_descriptor_snapshot <- reactive({
    if (identical(ctx$input$pivot_data_dw, "secondary")) {
      ctx$secondary_revision_debounced()
    } else {
      ctx$data_revision_signature()
    }

    tryCatch({
      if (is.null(ctx$input$pivot_data_dw)) {
        return(list(ready = FALSE, columns = character(0), rows = 0L))
      }

      data <- if (ctx$input$pivot_data_dw == "primary") {
        if (is.function(ctx$get_primary_data_silent)) {
          isolate(ctx$get_primary_data_silent())
        } else {
          isolate(ctx$get_primary_data())
        }
      } else if (ctx$input$pivot_data_dw == "secondary") {
        isolate(ctx$get_secondary_data())
      } else {
        ctx$add_pivot_error("data_selection",
                            paste("Unknown data selection:", ctx$input$pivot_data_dw), "warning")
        return(list(ready = FALSE, columns = character(0), rows = 0L))
      }

      list(
        ready = is.data.frame(data),
        columns = if (is.data.frame(data)) setdiff(names(data), "Row Index") else character(0),
        rows = if (is.data.frame(data)) nrow(data) else 0L
      )
    }, error = function(e) {
      list(ready = FALSE, columns = character(0), rows = 0L)
    })
  })

  is_current_data_ready <- reactive(isTRUE(pivot_descriptor_snapshot()$ready))

  get_current_data <- reactive({
    if (!isTRUE(is_current_data_ready())) {
      return(NULL)
    }

    tryCatch({
      if (ctx$input$pivot_data_dw == "primary") {
        return(isolate(ctx$get_primary_data()))
      } else if (ctx$input$pivot_data_dw == "secondary") {
        return(isolate(ctx$get_secondary_data()))
      }

      return(NULL)
    }, error = function(e) {
      ctx$add_pivot_error("data_selection",
                          paste("Error getting current data:", e$message), "error")
      return(NULL)
    })
  })

  # ------------------------------------------------------------------
  # pivot_ready indicator
  # ------------------------------------------------------------------

  ctx$output$pivot_ready <- renderText({
    if (isTRUE(is_current_data_ready())) "true" else "false"
  })

  outputOptions(ctx$output, "pivot_ready", suspendWhenHidden = FALSE)

  # ------------------------------------------------------------------
  # Dynamic options UI
  # ------------------------------------------------------------------

  pivot_column_choice_cache <- reactiveVal(list(signature = NULL, columns = character(0)))
  get_pivot_column_choices <- function(current_data) {
    sig <- tryCatch(ctx$data_revision_signature(), error = function(e) NULL)
    cache_signature <- paste(c(unlist(sig, use.names = TRUE), ctx$input$pivot_data_dw), collapse = "|")
    cached <- pivot_column_choice_cache()
    if (!is.null(cached$signature) && identical(cached$signature, cache_signature)) {
      return(cached$columns)
    }
    column_names <- setdiff(names(current_data), "Row Index")
    pivot_column_choice_cache(list(signature = cache_signature, columns = column_names))
    column_names
  }

  ctx$output$pivot_options <- renderUI({
    req(isTRUE(ctx$initialized()))
    tryCatch({
      if (!isTRUE(is_current_data_ready())) {
        return(div(class = "alert alert-warning",
                   "No data available. Please load data for the selected pivot source."))
      }

      snapshot <- pivot_descriptor_snapshot()
      if (!isTRUE(snapshot$ready)) {
        ctx$add_pivot_error("ui_generation", "Current data is not a data frame", "error")
        return(div(class = "alert alert-danger", "Data format error. Please check your data."))
      }

      if (snapshot$rows == 0) {
        return(div(class = "alert alert-warning", "Data is empty. Please load data with content."))
      }

      if (is.null(ctx$input$pivot_type_dw)) {
        return(div(class = "alert alert-info", "Select a pivot type to see options."))
      }

      # Choice builders share one cheap descriptor snapshot.  Preview and Apply
      # retain their independent full-data trigger through get_current_data().
      column_names <- snapshot$columns
      cfg <- ctx$pivot_options_state()

      ctx$debug_log(paste("Generating UI for", ctx$input$pivot_type_dw,
                          "pivot with", length(column_names), "columns"), 2)

      if (ctx$input$pivot_type_dw == "wider") {
        sel_names_from  <- if (!is.null(cfg$wider_names_from) &&
                               cfg$wider_names_from %in% column_names) cfg$wider_names_from else NULL
        sel_values_from <- if (!is.null(cfg$wider_values_from) &&
                               cfg$wider_values_from %in% column_names) cfg$wider_values_from else NULL
        sel_id_cols     <- if (!is.null(cfg$wider_id_cols))
          intersect(as.character(cfg$wider_id_cols), column_names) else NULL

        tagList(
          div(
            title = "Column containing the names that will become new column headers in the wide format.",
            selectInput(ctx$ns("wider_names_from"),
                        "Names from (column to become headers):",
                        choices = column_names, selected = sel_names_from)
          ),
          div(
            title = "Column(s) containing the values to fill the new columns created from names. You can select multiple value columns.",
            selectInput(ctx$ns("wider_values_from"), "Values from:",
                        choices = column_names, selected = sel_values_from, multiple = TRUE)
          ),
          div(
            title = "Columns to keep as identifier variables (not pivoted). Leave empty to use all other columns.",
            selectInput(ctx$ns("wider_id_cols"), "ID columns (optional):",
                        choices = column_names, multiple = TRUE, selected = sel_id_cols)
          )
        )

      } else if (ctx$input$pivot_type_dw == "longer") {
        sel_cols <- if (!is.null(cfg$longer_cols))
          intersect(as.character(cfg$longer_cols), column_names) else NULL
        val_names_to <- if (!is.null(cfg$longer_names_to) &&
                            nzchar(as.character(cfg$longer_names_to)[1]))
          as.character(cfg$longer_names_to)[1] else "name"
        val_values_to <- if (!is.null(cfg$longer_values_to) &&
                             nzchar(as.character(cfg$longer_values_to)[1]))
          as.character(cfg$longer_values_to)[1] else "value"

        tagList(
          div(
            title = "Columns to pivot into longer format. These columns will be stacked into rows.",
            selectInput(ctx$ns("longer_cols"), "Columns to pivot:",
                        choices = column_names, multiple = TRUE, selected = sel_cols)
          ),
          div(
            title = "Name for the new column that will contain the original column names.",
            textInput(ctx$ns("longer_names_to"), "Names to (new column name):",
                      value = val_names_to, placeholder = "name")
          ),
          div(
            title = "Name for the new column that will contain the values from the pivoted columns.",
            textInput(ctx$ns("longer_values_to"), "Values to (new column name):",
                      value = val_values_to, placeholder = "value")
          )
        )
      }
    }, error = function(e) {
      ctx$add_pivot_error("ui_generation",
                          paste("Error generating pivot options UI:", e$message), "error")
      return(div(class = "alert alert-danger",
                 paste("Error generating options:", e$message)))
    })
  })

  # ------------------------------------------------------------------
  # Preview logic (with error-loop prevention)
  # ------------------------------------------------------------------

  prepare_pivot_preview_info <- function() {
    current_time    <- Sys.time()
    last_error_time <- ctx$preview_last_error_time()

    if (!is.null(last_error_time) &&
        difftime(current_time, last_error_time, units = "secs") < 2 &&
        ctx$preview_error_count() > 3) {
      ctx$debug_log("Preview error loop detected - stopping preview generation", 1)
      return(list(error = "Preview temporarily disabled due to repeated errors"))
    }

    tryCatch({
      if (!isTRUE(is_current_data_ready())) {
        return(list(error = "No data available for preview"))
      }

      current_data <- get_current_data()

      if (is.null(current_data) || !is.data.frame(current_data) || nrow(current_data) == 0) {
        return(list(error = "No valid data available for preview"))
      }

      if (is.null(ctx$input$pivot_type_dw)) {
        return(list(error = "Select a pivot type to see preview"))
      }

      if (!requireNamespace("tidyr", quietly = TRUE)) {
        return(list(error = "tidyr package required for pivot operations"))
      }

      if (ctx$input$pivot_type_dw == "wider") {
        if (is.null(ctx$input$wider_names_from) || is.null(ctx$input$wider_values_from) ||
            length(ctx$input$wider_values_from) == 0) {
          return(list(error = "Select 'Names from' and 'Values from' for preview"))
        }

        if (!ctx$input$wider_names_from %in% names(current_data)) {
          return(list(error = "Selected columns not found in data"))
        }

        invalid_vf <- ctx$input$wider_values_from[!ctx$input$wider_values_from %in% names(current_data)]
        if (length(invalid_vf) > 0) {
          return(list(error = paste("Selected columns not found in data:",
                                    paste(invalid_vf, collapse = ", "))))
        }

        info <- pivot_preview_wider(current_data, ctx$input$wider_names_from,
                                    ctx$input$wider_values_from, ctx$input$wider_id_cols)

        if (!is.null(info$error)) {
          ctx$preview_error_count(ctx$preview_error_count() + 1)
          ctx$preview_last_error_time(Sys.time())
          ctx$add_pivot_error("preview", info$error, "error")
          return(info)
        }

        ctx$debug_log(paste("Wider preview generated:",
                            nrow(info$preview_data), "x", ncol(info$preview_data)), 2)
        ctx$preview_error_count(0)
        return(info)
      }

      if (ctx$input$pivot_type_dw == "longer") {
        if (is.null(ctx$input$longer_cols) || length(ctx$input$longer_cols) == 0) {
          return(list(error = "Select columns to pivot for preview"))
        }

        invalid <- ctx$input$longer_cols[!ctx$input$longer_cols %in% names(current_data)]
        if (length(invalid) > 0) {
          return(list(error = paste("Columns not found in data:", paste(invalid, collapse = ", "))))
        }

        names_to  <- normalize_pivot_text_value(ctx$input$longer_names_to,  "name")
        values_to <- normalize_pivot_text_value(ctx$input$longer_values_to, "value")

        info <- pivot_preview_longer(current_data, ctx$input$longer_cols, names_to, values_to)

        if (!is.null(info$error)) {
          ctx$preview_error_count(ctx$preview_error_count() + 1)
          ctx$preview_last_error_time(Sys.time())
          ctx$add_pivot_error("preview", info$error, "error")
          return(info)
        }

        ctx$debug_log(paste("Longer preview generated:",
                            nrow(info$preview_data), "x", ncol(info$preview_data)), 2)
        ctx$preview_error_count(0)
        return(info)
      }

      if (ctx$input$pivot_type_dw == "transpose") {
        if (ncol(current_data) < 2) {
          return(list(error = "Data must have at least 2 columns to transpose"))
        }

        info <- pivot_preview_transpose(current_data)

        if (!is.null(info$error)) {
          ctx$preview_error_count(ctx$preview_error_count() + 1)
          ctx$preview_last_error_time(Sys.time())
          ctx$add_pivot_error("preview", info$error, "error")
          return(info)
        }

        ctx$debug_log(paste("Transpose preview generated:",
                            nrow(info$preview_data), "x", ncol(info$preview_data)), 2)
        ctx$preview_error_count(0)
        return(info)
      }

      return(list(error = "Unknown pivot type"))
    }, error = function(e) {
      ctx$preview_error_count(ctx$preview_error_count() + 1)
      ctx$preview_last_error_time(Sys.time())
      ctx$add_pivot_error("preview", e$message, "error")
      list(error = paste("Error preparing preview:", e$message))
    })
  }

  # Backward-compatible reactive: returns preview data, an error string, or NULL.
  pivot_preview_data <- reactive({
    info <- prepare_pivot_preview_info()
    if (is.null(info)) return(NULL)
    if (!is.null(info$error)) return(info$error)
    info$preview_data
  })

  pivot_preview_info <- reactive({
    info <- prepare_pivot_preview_info()
    if (is.null(info)) return(list(error = "Configure pivot options to see preview..."))
    if (!is.null(info$error)) return(list(error = info$error))
    list(dim = info$dim, head = info$head)
  })

  ctx$output$pivot_preview_dim <- renderText({
    info <- pivot_preview_info()
    if (is.null(info)) return("Configure pivot options to see preview...")
    if (!is.null(info$error)) return(info$error)
    paste0("Dimensions: ", info$dim[1], " rows x ", info$dim[2], " columns")
  })

  ctx$output$pivot_preview_table <- DT::renderDT({
    info <- pivot_preview_info()
    if (is.null(info) || !is.null(info$error)) {
      return(DT::datatable(
        data.frame(Info = if (!is.null(info$error)) info$error
                         else "Configure pivot options to see preview..."),
        options  = list(dom = "t", pageLength = 1, searching = FALSE),
        rownames = FALSE
      ))
    }
    DT::datatable(info$head,
                  options = list(scrollX = TRUE, pageLength = 5, dom = "t", searching = FALSE),
                  rownames = FALSE)
  }, server = FALSE)


  list(
    get_current_data       = get_current_data,
    is_current_data_ready  = is_current_data_ready,
    pivot_preview_data     = pivot_preview_data
  )
}

# ============================================================================
# pivot_register_observers
# ============================================================================

#' Register observeEvent handlers for the Pivot module.
#'
#' Handles: UI_config reactive changes, apply pivot button, large-pivot
#' confirmation modal. Execution wrappers are defined as inner functions
#' within this scope so they close over ctx naturally.
#'
#' Required ctx keys: input, output, session, ns, UI_config, debug_log,
#'   add_pivot_error, apply_ui_config, get_current_data, set_current_data,
#'   execute_pivot_from_params, pivot_operation_params, last_operation_time,
#'   operation_history.
pivot_register_observers <- function(ctx) {

  # ------------------------------------------------------------------
  # Execution wrappers (inner helpers, not exported)
  # ------------------------------------------------------------------

  # Returns TRUE and emits a warning notification + debug_log(1) when any of
  # the supplied column names is "Row Index". Callers should return() early
  # when this function returns TRUE.
  reject_if_row_index_selected <- function(col_names, operation_label) {
    if ("Row Index" %in% col_names) {
      ctx$debug_log(
        paste0(
          "'Row Index' was found in ", operation_label, " column selections",
          " - it is not allowed as a selectable column; rejecting operation"
        ),
        1
      )
      showNotification(
        "'Row Index' cannot be used as a column selection in pivot operations. Please choose a different column.",
        type = "warning", duration = 8
      )
      return(TRUE)
    }
    FALSE
  }

  perform_wider_pivot <- function(data, names_from, values_from, id_cols, progress = NULL) {
    tryCatch({
      operation_start_time <- Sys.time()
      ctx$debug_log(paste("Starting wider pivot - Names from:", names_from,
                          "Values from:", paste(values_from, collapse = ",")), 1)

      if (!is.null(progress)) progress$set(value = 0.4, message = "Performing wider pivot...")

      result <- pivot_execute_wider(data, names_from, values_from, id_cols, ctx$debug_log)

      if (!is.null(progress)) progress$set(value = 0.9, message = "Updating data...")

      success <- tryCatch({
        ctx$set_current_data(result)
      }, error = function(set_error) {
        stop(paste("Failed to update data:",
                   if (!is.null(set_error$message) && nzchar(set_error$message))
                     set_error$message else "Unknown data update error"))
      })

      operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))

      if (safe_is_true(success)) {
        ctx$debug_log("PIVOT WIDER: Data update delegated to set_current_data adapter", 2)

        if (!is.null(progress)) progress$set(value = 1.0, message = "Complete!")

        success_msg <- paste(
          "Wider pivot completed successfully!",
          "Original:", nrow(data), "x", ncol(data),
          "Result:", nrow(result), "x", ncol(result),
          sprintf("(%.2fs)", operation_duration)
        )
        ctx$debug_log(success_msg, 1)
        showNotification(success_msg, type = "message", duration = 4)

        history_entry <- list(
          timestamp     = Sys.time(),
          operation     = "wider_pivot",
          duration      = operation_duration,
          success       = TRUE,
          original_dims = paste(nrow(data), "x", ncol(data)),
          result_dims   = paste(nrow(result), "x", ncol(result))
        )
        ctx$operation_history(c(ctx$operation_history(), list(history_entry)))

      } else {
        error_msg <- "Could not update data after pivot operation (set_current_data returned FALSE)"
        ctx$add_pivot_error("wider_pivot", error_msg, "error")
        showNotification(paste("Error performing wider pivot:", error_msg), type = "error", duration = 10)
      }

    }, error = function(e) {
      error_details <- if (!is.null(e$message) && nzchar(e$message)) {
        e$message
      } else if (!is.null(e$call)) {
        paste("Error in function call:", deparse(e$call)[1])
      } else {
        "Unknown error during wider pivot execution"
      }
      ctx$add_pivot_error("wider_pivot", paste("Wider pivot execution failed:", error_details), "error")
      showNotification(paste("Error performing wider pivot:", error_details), type = "error", duration = 10)
    })
  }

  perform_longer_pivot <- function(data, cols, names_to, values_to, progress = NULL) {    tryCatch({
      operation_start_time <- Sys.time()
      ctx$debug_log(paste("Starting longer pivot - Cols:", length(cols),
                          "Names to:", names_to, "Values to:", values_to), 1)

      if (!is.null(progress)) progress$set(value = 0.4, message = "Performing longer pivot...")

      result <- pivot_execute_longer(data, cols, names_to, values_to, ctx$debug_log)

      if (isTRUE(attr(result, "mixed_type_coercion"))) {
        ctx$add_pivot_error("longer_pivot",
                            "Mixed value types detected; pivoted values coerced to character.", "warning")
        showNotification(
          "Mixed value types detected during longer pivot. Values were converted to character.",
          type = "warning", duration = 8
        )
      }

      if (!is.null(progress)) progress$set(value = 0.9, message = "Updating data...")

      success <- tryCatch({
        ctx$set_current_data(result)
      }, error = function(set_error) {
        stop(paste("Failed to update data:",
                   if (!is.null(set_error$message) && nzchar(set_error$message))
                     set_error$message else "Unknown data update error"))
      })

      operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))

      if (safe_is_true(success)) {
        if (!is.null(progress)) progress$set(value = 1.0, message = "Complete!")

        success_msg <- paste(
          "Longer pivot completed successfully!",
          "Original:", nrow(data), "x", ncol(data),
          "Result:", nrow(result), "x", ncol(result),
          sprintf("(%.2fs)", operation_duration)
        )
        ctx$debug_log(success_msg, 1)
        showNotification(success_msg, type = "message", duration = 4)

        history_entry <- list(
          timestamp     = Sys.time(),
          operation     = "longer_pivot",
          duration      = operation_duration,
          success       = TRUE,
          original_dims = paste(nrow(data), "x", ncol(data)),
          result_dims   = paste(nrow(result), "x", ncol(result))
        )
        ctx$operation_history(c(ctx$operation_history(), list(history_entry)))

      } else {
        error_msg <- "Could not update data after pivot operation (set_current_data returned FALSE)"
        ctx$add_pivot_error("longer_pivot", error_msg, "error")
        showNotification(paste("Error performing longer pivot:", error_msg), type = "error", duration = 10)
      }

    }, error = function(e) {
      error_details <- if (!is.null(e$message) && nzchar(e$message)) {
        e$message
      } else if (!is.null(e$call)) {
        paste("Error in function call:", deparse(e$call)[1])
      } else {
        "Unknown error during longer pivot execution"
      }
      ctx$add_pivot_error("longer_pivot", paste("Longer pivot execution failed:", error_details), "error")
      showNotification(paste("Error performing longer pivot:", error_details), type = "error", duration = 10)
    })
  }

  perform_transpose_pivot <- function(data, progress = NULL) {
    tryCatch({
      operation_start_time <- Sys.time()
      ctx$debug_log(paste("Starting transpose pivot - Input:", nrow(data), "x", ncol(data)), 1)

      if (!is.null(progress)) progress$set(value = 0.4, message = "Performing transpose...")

      result <- pivot_execute_transpose(data, ctx$debug_log)

      if (isTRUE(attr(result, "mixed_type_coercion"))) {
        ctx$add_pivot_error("transpose",
                            "Mixed value types detected; all transposed values coerced to character.", "warning")
        showNotification(
          "Mixed value types detected during transpose. All values were converted to character.",
          type = "warning", duration = 8
        )
      }

      if (!is.null(progress)) progress$set(value = 0.9, message = "Updating data...")

      success <- tryCatch({
        ctx$set_current_data(result)
      }, error = function(set_error) {
        stop(paste("Failed to update data:",
                   if (!is.null(set_error$message) && nzchar(set_error$message))
                     set_error$message else "Unknown data update error"))
      })

      operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))

      if (safe_is_true(success)) {
        if (!is.null(progress)) progress$set(value = 1.0, message = "Complete!")

        success_msg <- paste(
          "Transpose completed successfully!",
          "Original:", nrow(data), "x", ncol(data),
          "Result:", nrow(result), "x", ncol(result),
          sprintf("(%.2fs)", operation_duration)
        )
        ctx$debug_log(success_msg, 1)
        showNotification(success_msg, type = "message", duration = 4)

        history_entry <- list(
          timestamp     = Sys.time(),
          operation     = "transpose",
          duration      = operation_duration,
          success       = TRUE,
          original_dims = paste(nrow(data), "x", ncol(data)),
          result_dims   = paste(nrow(result), "x", ncol(result))
        )
        ctx$operation_history(c(ctx$operation_history(), list(history_entry)))

      } else {
        error_msg <- "Could not update data after transpose operation (set_current_data returned FALSE)"
        ctx$add_pivot_error("transpose", error_msg, "error")
        showNotification(paste("Error performing transpose:", error_msg), type = "error", duration = 10)
      }

    }, error = function(e) {
      error_details <- if (!is.null(e$message) && nzchar(e$message)) {
        e$message
      } else if (!is.null(e$call)) {
        paste("Error in function call:", deparse(e$call)[1])
      } else {
        "Unknown error during transpose execution"
      }
      ctx$add_pivot_error("transpose", paste("Transpose execution failed:", error_details), "error")
      showNotification(paste("Error performing transpose:", error_details), type = "error", duration = 10)
    })
  }

  execute_pivot_from_params <- function(params, progress = NULL) {
    if (is.null(params) || is.null(params$type)) return()

    if (params$type == "wider") {
      perform_wider_pivot(params$data, params$names_from, params$values_from,
                          params$id_cols, progress)
    } else if (params$type == "longer") {
      perform_longer_pivot(params$data, params$cols, params$names_to,
                           params$values_to, progress)
    } else if (params$type == "transpose") {
      perform_transpose_pivot(params$data, progress)
    }
  }

  # ------------------------------------------------------------------
  # Watch for UI_config changes
  # ------------------------------------------------------------------

  observeEvent(ctx$UI_config(), {
    current_config <- ctx$UI_config()
    if (!is.null(current_config)) {
      ctx$debug_log("Enhanced UI_config update received for pivot", 2)
      success <- ctx$apply_ui_config(current_config)
      if (success) {
        ctx$debug_log("Pivot UI_config applied successfully", 2)
      } else {
        ctx$debug_log("Failed to apply pivot UI_config", 1)
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ------------------------------------------------------------------
  # Apply pivot action
  # ------------------------------------------------------------------

  observeEvent(ctx$input$apply_pivot_dw, {
    tryCatch({
      ctx$debug_log("Apply pivot button triggered", 1)

      if (!isTRUE(ctx$is_current_data_ready())) {
        showNotification("No data available for pivoting.", type = "error", duration = 8)
        return()
      }

      current_data <- ctx$get_current_data()

      if (is.null(current_data)) {
        showNotification("No data available for pivoting.", type = "error", duration = 8)
        return()
      }
      if (!is.data.frame(current_data) || nrow(current_data) == 0) {
        showNotification("Invalid or empty data for pivoting.", type = "error", duration = 8)
        return()
      }
      if (is.null(ctx$input$pivot_type_dw)) {
        showNotification("Please select a pivot type.", type = "error", duration = 6)
        return()
      }

      if (ctx$input$pivot_type_dw == "wider") {
        # "Row Index" must not be used as a column selection in pivot operations.
        if (reject_if_row_index_selected(
          c(ctx$input$wider_names_from, ctx$input$wider_values_from, ctx$input$wider_id_cols),
          "wider pivot"
        )) return()

        params <- pivot_build_wider_params(
          current_data,
          ctx$input$wider_names_from,
          ctx$input$wider_values_from,
          ctx$input$wider_id_cols,
          ctx$debug_log
        )

        if (!is.null(params$error)) {
          showNotification(params$error, type = "error", duration = 6)
          return()
        }

        if (params$estimated_cols > 1000) {
          ctx$pivot_operation_params(params)
          showModal(modalDialog(
            title = "Performance Warning - Wider Pivot",
            div(
              h5("Large Dataset Warning"),
              p(paste("This pivot operation will create approximately", params$estimated_cols, "columns:")),
              tags$ul(
                tags$li(paste("ID columns:", length(params$id_cols))),
                tags$li(paste("New columns from unique values:", params$unique_names)),
                tags$li(paste("Current data:", nrow(current_data), "rows"))
              ),
              br(),
              p("This may significantly impact application performance and memory usage."),
              p("Consider:"),
              tags$ul(
                tags$li("Filtering your data first to reduce rows"),
                tags$li("Using fewer ID columns"),
                tags$li("Grouping/aggregating the names_from column")
              )
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ctx$ns("confirm_large_pivot"), "Continue Anyway", class = "btn-warning")
            ),
            size = "m"
          ))
          return()
        }

        execute_pivot_from_params(params)

      } else if (ctx$input$pivot_type_dw == "longer") {
        # "Row Index" must not be used as a column selection in pivot operations.
        if (reject_if_row_index_selected(ctx$input$longer_cols, "longer pivot")) return()

        params <- pivot_build_longer_params(
          current_data,
          ctx$input$longer_cols,
          normalize_pivot_text_value(ctx$input$longer_names_to,  "name"),
          normalize_pivot_text_value(ctx$input$longer_values_to, "value"),
          ctx$debug_log
        )

        if (!is.null(params$error)) {
          showNotification(params$error, type = "error", duration = 6)
          return()
        }

        if (params$estimated_rows > 500000) {
          ctx$pivot_operation_params(params)
          showModal(modalDialog(
            title = "Performance Warning - Longer Pivot",
            div(
              h5("Large Dataset Warning"),
              p(paste("This pivot operation will create approximately",
                      format(params$estimated_rows, big.mark = ","), "rows:")),
              tags$ul(
                tags$li(paste("Current rows:", format(nrow(current_data), big.mark = ","))),
                tags$li(paste("Columns to pivot:", params$pivot_cols_count)),
                tags$li(paste("Multiplication factor:", params$pivot_cols_count, "x"))
              ),
              br(),
              p("This may significantly impact application performance and memory usage."),
              p("Consider:"),
              tags$ul(
                tags$li("Filtering your data first to reduce rows"),
                tags$li("Selecting fewer columns to pivot"),
                tags$li("Processing in smaller batches")
              )
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ctx$ns("confirm_large_pivot"), "Continue Anyway", class = "btn-warning")
            ),
            size = "m"
          ))
          return()
        }

        execute_pivot_from_params(params)

      } else if (ctx$input$pivot_type_dw == "transpose") {
        params <- pivot_build_transpose_params(current_data, ctx$debug_log)

        if (!is.null(params$error)) {
          showNotification(params$error, type = "error", duration = 6)
          return()
        }

        execute_pivot_from_params(params)
      }
    }, error = function(e) {
      ctx$add_pivot_error("apply_pivot", paste("Error during pivot operation:", e$message), "error")
      showNotification(paste("Error during pivot operation:", e$message), type = "error", duration = 10)
    })
  })

  # ------------------------------------------------------------------
  # Large pivot confirmation
  # ------------------------------------------------------------------

  observeEvent(ctx$input$confirm_large_pivot, {
    tryCatch({
      removeModal()
      params <- ctx$pivot_operation_params()
      if (is.null(params)) {
        showNotification("Error: Pivot parameters not found.", type = "error", duration = 8)
        return()
      }

      operation_start_time <- Sys.time()
      progress <- shiny::Progress$new()
      progress$set(message = paste("Performing large", params$type, "pivot operation..."), value = 0)

      tryCatch({
        if (params$type == "wider") {
          progress$set(value = 0.2, message = "Preparing wider pivot...")
        } else if (params$type == "longer") {
          progress$set(value = 0.2, message = "Preparing longer pivot...")
        }

        execute_pivot_from_params(params, progress)

        operation_duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
        ctx$last_operation_time(operation_duration)

        history_entry <- list(
          timestamp = Sys.time(),
          operation = paste("large", params$type, "pivot"),
          duration  = operation_duration,
          success   = TRUE
        )
        ctx$operation_history(c(ctx$operation_history(), list(history_entry)))

        ctx$pivot_operation_params(NULL)
      }, error = function(e) {
        ctx$add_pivot_error("large_pivot",
                            paste("Error during large pivot operation:", e$message), "error")
        showNotification(paste("Error during large pivot operation:", e$message),
                         type = "error", duration = 10)
      }, finally = {
        progress$close()
      })
    }, error = function(e) {
      ctx$add_pivot_error("confirm_large_pivot",
                          paste("Error in large pivot confirmation:", e$message), "error")
      showNotification(paste("Error in large pivot confirmation:", e$message),
                       type = "error", duration = 10)
      ctx$pivot_operation_params(NULL)
    })
  })
}

# ============================================================================
# pivot_register_cleanup
# ============================================================================

#' Register the Pivot module cleanup function with cleanup_manager.
#'
#' Required ctx keys: debug_log, ui_config_applied, ui_config_source,
#'   ui_config_update_in_progress, pivot_options_state, pivot_errors,
#'   last_operation_time, operation_history, pivot_operation_params,
#'   preview_error_count, preview_last_error_time, session.
pivot_register_cleanup <- function(ctx) {
  cleanup_manager$register_module("Pivot", function() {
    ctx$debug_log("Executing [Pivot] cleanup", 2)

    ctx$ui_config_applied(FALSE)
    ctx$ui_config_source("none")
    ctx$ui_config_update_in_progress(FALSE)
    ctx$pivot_options_state(list())
    ctx$pivot_errors(list())
    ctx$last_operation_time(NULL)
    ctx$operation_history(list())
    ctx$pivot_operation_params(NULL)
    ctx$preview_error_count(0)
    ctx$preview_last_error_time(NULL)

    ctx$debug_log("[Pivot] cleanup completed", 2)
  })

}

# ============================================================================
# pivot_build_api
# ============================================================================

#' Build and return the public API list for modPivotServer.
#'
#' All items in the returned list are functions or reactives. The `ctx` must
#' include every object referenced in the API.
#'
#' Required ctx keys: get_primary_data, set_primary_data, get_secondary_data,
#'   set_secondary_data, get_current_data, set_current_data, get_current_ui_state,
#'   apply_ui_config, ui_config_applied, ui_config_source, is_pivot_configured,
#'   get_pivot_summary, input, pivot_preview_data, pivot_errors, add_pivot_error,
#'   preview_error_count, preview_last_error_time, last_operation_time,
#'   operation_history, DEBUG_LEVEL, debug_log, module_health_check, ui_system,
#'   get_current_ui_values, get_pivot_ui_config_for_export, set_pivot_ui_config_from_import.
pivot_build_api <- function(ctx) {
  list(
    # Core data access
    get_primary_data   = ctx$get_primary_data,
    set_primary_data   = ctx$set_primary_data,
    get_secondary_data = ctx$get_secondary_data,
    set_secondary_data = ctx$set_secondary_data,
    get_current_data   = ctx$get_current_data,
    is_current_data_ready = ctx$is_current_data_ready,
    set_current_data   = ctx$set_current_data,

    # UI config management
    get_current_ui_state = ctx$get_current_ui_state,
    apply_ui_config      = ctx$apply_ui_config,
    ui_config_applied    = reactive({ ctx$ui_config_applied() }),
    ui_config_source     = reactive({ ctx$ui_config_source() }),

    # Configuration status
    is_pivot_configured = ctx$is_pivot_configured,
    get_pivot_summary   = ctx$get_pivot_summary,

    # Individual configuration accessors
    pivot_data_dw = reactive({ ctx$input$pivot_data_dw }),
    pivot_type_dw = reactive({ ctx$input$pivot_type_dw }),
    pivot_options = reactive({
      tryCatch({
        if (ctx$input$pivot_type_dw == "wider") {
          list(
            names_from  = ctx$input$wider_names_from,
            values_from = ctx$input$wider_values_from,
            id_cols     = ctx$input$wider_id_cols
          )
        } else if (ctx$input$pivot_type_dw == "longer") {
          list(
            cols      = ctx$input$longer_cols,
            names_to  = ctx$input$longer_names_to,
            values_to = ctx$input$longer_values_to
          )
        } else {
          list()
        }
      }, error = function(e) {
        ctx$add_pivot_error("pivot_options", paste("Error getting pivot options:", e$message), "error")
        return(list())
      })
    }),

    # Preview functionality
    pivot_preview_data = ctx$pivot_preview_data,

    # Action triggers
    apply_trigger   = reactive({ ctx$input$apply_pivot_dw }),
    # Deprecated placeholder: preview button input is not part of the active UI.
    preview_trigger = reactive({ NULL }),

    # Error tracking
    pivot_errors      = reactive({ ctx$pivot_errors() }),
    get_pivot_errors  = function() { ctx$pivot_errors() },
    clear_pivot_errors = function() {
      tryCatch({
        ctx$pivot_errors(list())
        ctx$preview_error_count(0)
        ctx$preview_last_error_time(NULL)
        ctx$debug_log("Pivot errors cleared", 2)
      }, error = function(e) {
        ctx$debug_log(paste("Error clearing pivot errors:", e$message), 1)
      })
    },

    # Performance monitoring
    get_performance_metrics = reactive({
      tryCatch({
        list(
          last_operation_time = ctx$last_operation_time(),
          operation_history   = ctx$operation_history(),
          debug_level         = ctx$DEBUG_LEVEL
        )
      }, error = function(e) {
        ctx$debug_log(paste("Error getting performance metrics:", e$message), 1)
        list(last_operation_time = NULL, operation_history = list(),
             debug_level = ctx$DEBUG_LEVEL)
      })
    }),

    # Module health check
    module_health_check = ctx$module_health_check,

    # Debug and testing functions
    test_pivot_module = function() {
      ctx$debug_log("=== TESTING PIVOT MODULE ===", 2)
      ctx$debug_log(paste("Debug level:", ctx$DEBUG_LEVEL), 2)
      ctx$debug_log(paste("Error count:", length(ctx$pivot_errors())), 2)
      ctx$debug_log(paste("UI config applied:", ctx$ui_config_applied()), 2)
      ctx$debug_log(paste("UI config source:", ctx$ui_config_source()), 2)
      ctx$debug_log(paste("Pivot configured:", ctx$is_pivot_configured()), 2)
      ctx$debug_log(paste("Current data available:", !is.null(ctx$get_current_data())), 2)
      ctx$debug_log("=== END TESTING ===", 2)
    },
    test_safe_ui_system = function() {
      ctx$debug_log("=== TESTING SAFE UI SYSTEM ===", 2)
      ctx$debug_log(paste("UI system available:", !is.null(ctx$ui_system)), 2)
      ctx$debug_log(paste("Update function available:", !is.null(ctx$ui_system$update_input_safely)), 2)
      ctx$debug_log(paste("Notification function available:",
                          !is.null(ctx$ui_system$show_notification_safely)), 2)
      ctx$debug_log(paste("Execute function available:", !is.null(ctx$ui_system$execute_when_ready)), 2)

      test_config <- list(pivot_data_dw = "primary", pivot_type_dw = "wider")
      result <- ctx$apply_ui_config(test_config)
      ctx$debug_log(paste("Test config application result:", result), 2)
      ctx$debug_log("=== END TEST ===", 2)
      return(result)
    },

    # Export / import functions
    get_current_ui_values           = ctx$get_current_ui_values,
    get_pivot_ui_config_for_export  = ctx$get_pivot_ui_config_for_export,
    set_pivot_ui_config_from_import = ctx$set_pivot_ui_config_from_import,

    # Session-restore bridge
    get_session_state = if (is.function(ctx$get_session_state)) ctx$get_session_state else function() list(),
    set_session_state = if (is.function(ctx$set_session_state)) ctx$set_session_state else function(state) invisible(NULL),

    get_current_pivot_state_for_export = function() {
      isolate({
        tryCatch({
          ui_values <- ctx$get_current_ui_values()
          ctx$debug_log("Returned pivot UI values for export", 2)
          return(ui_values)
        }, error = function(e) {
          ctx$debug_log(paste("Error getting pivot UI values:", e$message), 1)
          return(list(pivot_data_dw = "primary", pivot_type_dw = "wider", pivot_options = list()))
        })
      })
    },

    # UI config validation
    validate_pivot_ui_config = function(config) {
      if (is.null(config)) return(TRUE)
      if (!is.list(config)) return(FALSE)

      required_fields <- c("pivot_data_dw", "pivot_type_dw")
      has_required    <- all(required_fields %in% names(config))

      valid_type <- if (!is.null(config$pivot_type_dw))
        config$pivot_type_dw %in% c("wider", "longer") else TRUE

      valid_data <- if (!is.null(config$pivot_data_dw))
        config$pivot_data_dw %in% c("primary", "secondary") else TRUE

      return(has_required && valid_type && valid_data)
    }
  )
}
