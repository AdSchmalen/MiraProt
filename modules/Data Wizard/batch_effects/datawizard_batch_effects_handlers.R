# ============================================================================
# File: modules/Data Wizard/batch_effects/datawizard_batch_effects_handlers.R
#
# What this file does:
#   Registers all Shiny observers, event handlers, and output renderers for the
#   Batch Effects module. Also contains the UI config import/export logic
#   (apply_ui_config, get_current_ui_state delegate).
#
# How it fits into the module architecture:
#   datawizard_batch_effects.R (orchestrator)
#     -> sources this file into modEnv
#     -> calls register_batch_effects_handlers() inside moduleServer()
#     -> the function registers observers that survive for the session lifetime
#
# File structure:
#   1. register_batch_effects_handlers()  -- the single entry point
#      Internally registers:
#        a. apply_ui_config()             -- import UI configuration
#        b. get_current_ui_state()        -- export current UI state
#        c. UI_config observer            -- watches for imported config
#        d. User-modification observer    -- tracks manual UI changes
#        e. Add/Remove batch group handlers
#        f. Batch inputs sync observer
#        g. batch_ready output            -- readiness gate for conditionalPanel
#        h. batch_columns renderUI        -- dynamic batch group selectors
#        i. outputOptions for the above
#      Returns a list with apply_ui_config and get_current_ui_state so the
#      orchestrator can wire them into the module return value.
#
# What future developers need to know:
#   - This function is called once inside moduleServer(). All observers it
#     creates close over the function arguments (input, output, session, etc.)
#     via normal R lexical scoping.
#   - State is accessed via the state list (state$batchCounter, etc.), not via
#     standalone reactiveVal names.
#   - Utility functions (validate_batch_data_structure, etc.) are called from
#     modEnv and require debug_log as an explicit argument.
#   - The function returns a named list; the orchestrator destructures it.
# ============================================================================

#' Register all observers, event handlers, and outputs for Batch Effects.
#'
#' @param input       Shiny input object.
#' @param output      Shiny output object.
#' @param session     Shiny session object.
#' @param ns          Namespace function (session$ns).
#' @param state       Named list of reactiveVal objects from init_batch_effects_state().
#' @param get_file_data  Function returning the current data.frame (or NULL).
#' @param UI_config   Reactive returning UI configuration for import (or NULL).
#' @param debug_log   Logging function.
#' @return Named list with:
#'   \item{apply_ui_config}{Function(ui_config) -> logical}
#'   \item{get_current_ui_state}{Function() -> list or NULL}
register_batch_effects_handlers <- function(input, output, session, ns,
                                            state, get_file_data,
                                            UI_config, debug_log, primary_working_revision_debounced = reactive(NULL),
                                            data_revision_signature = reactive(NULL),
                                            initialized = reactive(TRUE)) {

  # ========================================
  # UI_config Management
  # ========================================

  #' Apply UI configuration from import with validation and delayed updates.
  apply_ui_config <- function(ui_config) {
    if (is.null(ui_config)) {
      debug_log("No UI config to apply", 2)
      return(TRUE)
    }

    if (state$ui_config_update_in_progress()) {
      debug_log("UI config update already in progress, skipping to prevent circular dependency", 2)
      return(TRUE)
    }

    tryCatch({
      state$ui_config_update_in_progress(TRUE)
      debug_log("Applying UI config from import", 1)

      # Validate UI config structure
      if (!is.list(ui_config)) {
        error_msg <- "UI config is not a list"
        state$ui_config_errors(append(state$ui_config_errors(), error_msg))
        debug_log(error_msg, 1)
        state$ui_config_update_in_progress(FALSE)
        return(FALSE)
      }

      # Use session$onFlushed for delayed updates to prevent timing issues
      session$onFlushed(function() {
        tryCatch({
          debug_log("Executing delayed UI updates with improved timing", 2)

          applied_settings <- character()

          # Apply batch method with validation and freeze
          if (!is.null(ui_config$batch_method)) {
            valid_methods <- c("Offset Correction", "ComBat", "Limma", "LOESS", "Quantile")
            if (ui_config$batch_method %in% valid_methods) {
              freezeReactiveValue(input, "batch_method")
              updateSelectInput(session, "batch_method", selected = ui_config$batch_method)
              applied_settings <- c(applied_settings, paste("Method:", ui_config$batch_method))
              debug_log(paste("Applied batch method:", ui_config$batch_method), 2)
            } else {
              warning_msg <- paste("Invalid batch method:", ui_config$batch_method)
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          # Apply imputation method with validation and freeze
          if (!is.null(ui_config$imputation_method_batch)) {
            valid_imputation <- c("None", "Left censored", "Random Forest", "MICE CART")
            if (ui_config$imputation_method_batch %in% valid_imputation) {
              freezeReactiveValue(input, "imputation_method_batch")
              updateSelectInput(session, "imputation_method_batch", selected = ui_config$imputation_method_batch)
              applied_settings <- c(applied_settings, paste("Imputation:", ui_config$imputation_method_batch))
              debug_log(paste("Applied imputation method:", ui_config$imputation_method_batch), 2)
            } else {
              warning_msg <- paste("Invalid imputation method:", ui_config$imputation_method_batch)
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          # Apply transformation with validation and freeze
          if (!is.null(ui_config$transformation_batch)) {
            valid_transformations <- c("None", "log2", "log10", "-log10")
            if (ui_config$transformation_batch %in% valid_transformations) {
              freezeReactiveValue(input, "transformation_batch")
              updateSelectInput(session, "transformation_batch", selected = ui_config$transformation_batch)
              applied_settings <- c(applied_settings, paste("Transform:", ui_config$transformation_batch))
              debug_log(paste("Applied transformation:", ui_config$transformation_batch), 2)
            } else {
              warning_msg <- paste("Invalid transformation:", ui_config$transformation_batch)
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          # Apply remove imputed flag with validation and freeze
          if (!is.null(ui_config$remove_imputed_batch)) {
            if (is.logical(ui_config$remove_imputed_batch)) {
              freezeReactiveValue(input, "remove_imputed_batch")
              updateCheckboxInput(session, "remove_imputed_batch", value = ui_config$remove_imputed_batch)
              applied_settings <- c(applied_settings, paste("Remove imputed:", ui_config$remove_imputed_batch))
              debug_log(paste("Applied remove imputed flag:", ui_config$remove_imputed_batch), 2)
            } else {
              warning_msg <- paste("Invalid remove imputed flag (not logical):", ui_config$remove_imputed_batch)
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          # Apply batch counter and inputs with validation
          if (!is.null(ui_config$batch_counter) && is.numeric(ui_config$batch_counter)) {
            if (ui_config$batch_counter >= 1 && ui_config$batch_counter <= 10) {
              state$batchCounter(ui_config$batch_counter)
              applied_settings <- c(applied_settings, paste("Groups:", ui_config$batch_counter))
              debug_log(paste("Applied batch counter:", ui_config$batch_counter), 2)
            } else {
              warning_msg <- paste("Invalid batch counter (must be 1-10):", ui_config$batch_counter)
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          if (!is.null(ui_config$batch_inputs) && is.list(ui_config$batch_inputs)) {
            # Validate batch inputs structure
            valid_inputs <- TRUE
            for (input_name in names(ui_config$batch_inputs)) {
              if (!grepl("^selected_batches_\\d+$", input_name)) {
                valid_inputs <- FALSE
                break
              }
            }

            if (valid_inputs) {
              state$batch_inputs(ui_config$batch_inputs)
              applied_settings <- c(applied_settings, paste("Batch inputs:", length(ui_config$batch_inputs)))
              debug_log(paste("Applied batch inputs - groups:", length(ui_config$batch_inputs)), 2)
            } else {
              warning_msg <- "Invalid batch inputs structure"
              state$ui_config_errors(append(state$ui_config_errors(), warning_msg))
              debug_log(warning_msg, 1)
            }
          }

          state$ui_config_applied(TRUE)
          state$ui_config_source("import")
          state$ui_config_update_in_progress(FALSE)

          # Log result
          if (length(applied_settings) > 0) {
            debug_log("UI config applied successfully", 1)
          } else {
            showNotification("Batch effects configuration loaded but no valid settings found", type = "warning", duration = 3)
            debug_log("UI config applied but no valid settings found", 1)
          }

        }, error = function(e) {
          error_msg <- paste("Error in delayed UI updates:", e$message)
          debug_log(error_msg, 1)
          state$ui_config_errors(append(state$ui_config_errors(), error_msg))
          state$ui_config_update_in_progress(FALSE)
          showNotification(paste("Error applying batch effects configuration:", e$message), type = "error", duration = 5)
        })
      }, once = TRUE)  # Critical: only execute once

      return(TRUE)

    }, error = function(e) {
      error_msg <- paste("Error applying UI config:", e$message)
      debug_log(error_msg, 1)
      state$ui_config_errors(append(state$ui_config_errors(), error_msg))
      state$ui_config_update_in_progress(FALSE)
      showNotification(paste("Error applying batch effects configuration:", e$message), type = "error", duration = 5)
      return(FALSE)
    })
  }

  # Delegate to state file
  get_current_ui_state <- function() {
    get_batch_effects_ui_state(input, state, debug_log)
  }

  # ========================================
  # UI_config Watchers
  # ========================================

  # Watch for UI_config changes
  observeEvent(UI_config(), {
    tryCatch({
      current_config <- UI_config()

      if (!is.null(current_config) && !state$ui_config_applied()) {
        debug_log("UI_config detected via observeEvent, applying configuration", 1)
        apply_ui_config(current_config)
      }
    }, error = function(e) {
      debug_log(paste("Error in UI_config observer:", e$message), 1)
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Track when user modifies settings manually (after an import)
  observeEvent(list(input$batch_method, input$imputation_method_batch,
                    input$transformation_batch, input$remove_imputed_batch), {
    tryCatch({
      if (state$ui_config_source() == "import" && !state$ui_config_update_in_progress()) {
        state$ui_config_source("user_modified")
        debug_log("UI config source changed to user_modified", 2)
      }
    }, error = function(e) {
      debug_log(paste("Error updating UI config source:", e$message), 1)
    })
  }, ignoreInit = TRUE)

  # ========================================
  # Batch Group Management Handlers
  # ========================================

  # Add batch group
  observeEvent(input$addBatchButton, {
    tryCatch({
      if (state$batchCounter() >= 10) {
        showNotification("Maximum of 10 batch groups allowed.", type = "warning")
        debug_log("Maximum batch groups reached", 2)
        return()
      }

      state$batchCounter(state$batchCounter() + 1)
      current <- state$batch_inputs()
      newID <- paste0("selected_batches_", state$batchCounter())
      current[[newID]] <- isolate(input[[newID]])
      state$batch_inputs(current)

      debug_log(paste("Added batch group", state$batchCounter()), 2)
      showNotification(paste("Added batch group", state$batchCounter()), type = "message")

    }, error = function(e) {
      debug_log(paste("Error adding batch group:", e$message), 1)
      showNotification("Error adding batch group", type = "error")
    })
  })

  # Remove batch group
  observeEvent(input$removeBatchButton, {
    tryCatch({
      if (state$batchCounter() > 1) {
        remove_id <- paste0("selected_batches_", state$batchCounter())
        current <- state$batch_inputs()
        current[[remove_id]] <- NULL
        state$batchCounter(state$batchCounter() - 1)
        state$batch_inputs(current)

        debug_log(paste("Removed batch group", state$batchCounter() + 1), 2)
        showNotification(paste("Removed batch group", state$batchCounter() + 1), type = "message")
      } else {
        showNotification("At least one batch group is required.", type = "warning")
        debug_log("Cannot remove last batch group", 2)
      }

    }, error = function(e) {
      debug_log(paste("Error removing batch group:", e$message), 1)
      showNotification("Error removing batch group", type = "error")
    })
  })

  # Keep batch_inputs synchronized with UI inputs
  observe({
    req(isTRUE(initialized()))
    tryCatch({
      n <- max(state$batchCounter(), 1)
      current <- state$batch_inputs()

      for (i in seq_len(n)) {
        id <- paste0("selected_batches_", i)
        current[[id]] <- input[[id]]
      }

      state$batch_inputs(current)
      debug_log(paste("Synchronized batch inputs for", n, "groups"), 2)

    }, error = function(e) {
      debug_log(paste("Error synchronizing batch inputs:", e$message), 1)
    })
  })

  # ========================================
  # Outputs: Readiness Gate and Dynamic UI
  # ========================================

  batch_structure_cache <- reactiveVal(list(signature = NULL, validation = NULL, numeric_cols = character(0)))

  get_batch_structure_choices <- function(df) {
    sig <- tryCatch(data_revision_signature(), error = function(e) NULL)
    cache_signature <- paste(unlist(sig, use.names = TRUE), collapse = "|")
    cached <- batch_structure_cache()
    if (!is.null(cached$signature) && identical(cached$signature, cache_signature)) {
      return(cached)
    }

    validation <- validate_batch_data_structure(df, debug_log)
    numeric_cols <- character(0)
    if (isTRUE(validation$valid)) {
      numeric_cols <- names(df)[vapply(df, function(x) {
        is.numeric(x) || !all(is.na(suppressWarnings(as.numeric(as.character(x)))))
      }, logical(1))]
      numeric_cols <- setdiff(numeric_cols, "Row Index")
    }

    result <- list(signature = cache_signature, validation = validation, numeric_cols = numeric_cols)
    batch_structure_cache(result)
    result
  }

  batch_refresh_snapshot <- reactiveVal(list(key = NULL, ready = FALSE, descriptors = NULL))
  observeEvent(data_revision_signature(), {
    key <- data_revision_signature()
    df <- isolate(get_file_data())
    ready <- is.data.frame(df) && nrow(df) > 0 && ncol(df) > 0
    descriptors <- if (ready) get_batch_structure_choices(df) else NULL
    batch_refresh_snapshot(list(key = key, ready = ready, descriptors = descriptors))
  }, ignoreInit = FALSE, priority = 100)

  output$batch_ready <- renderText({
    if (isTRUE(batch_refresh_snapshot()$ready)) "true" else "false"
  })

  # Dynamic UI: render batch column selectors
  output$batch_columns <- renderUI({
    req(isTRUE(initialized()))
    tryCatch({
      snapshot <- batch_refresh_snapshot()
      if (!isTRUE(snapshot$ready)) {
        return(div(class = "alert alert-info", "No data available. Please load data first."))
      }

      structure_choices <- snapshot$descriptors
      validation <- structure_choices$validation
      if (!validation$valid) {
        return(div(class = "alert alert-danger", validation$message))
      }

      nBatches <- max(state$batchCounter(), 1)
      numeric_cols <- structure_choices$numeric_cols

      if (length(numeric_cols) == 0) {
        return(div(class = "alert alert-warning",
                   "No numeric columns found for batch correction. Please ensure your data contains numeric values."))
      }

      # Create batch group selectors
      batchUIs <- lapply(seq_len(nBatches), function(i) {
        inputId <- ns(paste0("selected_batches_", i))
        selectedVal <- isolate(state$batch_inputs()[[paste0("selected_batches_", i)]])

        div(style = "margin-bottom: 15px;",
            selectizeInput(
              inputId = inputId,
              label = paste0("Batch Group ", i, " - Select columns:"),
              choices = numeric_cols,
              multiple = TRUE,
              selected = selectedVal,
              options = list(
                placeholder = "Select columns that belong to the same experimental batch...",
                maxItems = length(numeric_cols)
              )
            )
        )
      })

    }, error = function(e) {
      return(div(class = "alert alert-danger", "Error loading batch column interface. Please refresh the page."))
    })
  })

  outputOptions(output, "batch_ready", suspendWhenHidden = FALSE)

  # ========================================
  # Return callable functions for the orchestrator
  # ========================================

  list(
    apply_ui_config    = apply_ui_config,
    get_current_ui_state = get_current_ui_state
  )
}
