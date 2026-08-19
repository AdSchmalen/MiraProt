# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_assign_rules.R
# Purpose:
#   Server module for assignment-rule workflows in Data Wizard. It coordinates
#   condition-group definition, rule-set import/application, and preparation of
#   downstream UI configurations consumed by other Data Wizard feature modules.
#
# Architectural Role:
#   orchestrator
#
# Responsibilities:
#   - Own assign-rules reactive lifecycle and workflow sequencing.
#   - Coordinate rule-file loading, validation outcomes, and status propagation.
#   - Expose stable module outputs (reactives/functions) for parent integrations.
#
# Non-Responsibilities:
#   - Implement low-level UI layout details beyond module orchestration needs.
#   - Implement generic utility algorithms that belong in assign-rules utilities.
#
# Allowed Dependencies:
#   - modules/Data Wizard/assign rules/datawizard_assign_rules_UI.R
#   - modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R
#   - modules/Data Wizard/assign rules/datawizard_assign_rules_state.R
#   - modules/Data Wizard/assign rules/datawizard_assign_rules_handlers.R
#   - modules/Data Wizard/assign rules/datawizard_assign_rules_adapters.R
#   - Shiny APIs and package functions required for assign-rules orchestration.
#
# Interaction Boundaries:
#   - Inputs: module id, rule-file reactive source, metadata reactive source,
#             optional shared state handle, debug level.
#   - Outputs: condition state reactives, rule-load state, UI-config payload
#              channels, processing/status helper endpoints.
#   - Side Effects: reactive mutations, user notifications, module debug logging.
#
# Stability Guarantees:
#   - Keep exported module server signature and return-list contract stable for
#     callers unless explicit cross-module migration is planned.
#   - Maintain deterministic state-reset behavior on session cleanup.
# ============================================================================


# Source utility functions and UI components
source("modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R", local = modEnv)
source("modules/Data Wizard/assign rules/datawizard_assign_rules_state.R", local = modEnv)
source("modules/Data Wizard/assign rules/datawizard_assign_rules_handlers.R", local = modEnv)
source("modules/Data Wizard/assign rules/datawizard_assign_rules_adapters.R", local = modEnv)
source("modules/Data Wizard/assign rules/datawizard_assign_rules_UI.R", local = modEnv)

############
# Server - Enhanced Robustness and Performance

#' Enhanced Condition Assignment Rules Module Server with Robust Debug Management
#'
#' Server logic for managing condition assignments and sample groupings with comprehensive error handling and debug management
#' @param id module namespace id
#' @param rule_files reactive containing available rule files (optional)
#' @param metadata_current reactive containing current metadata state
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modAssignRulesServer <- function(id, rule_files = reactive(character(0)),
                                 metadata_current = reactive(NULL),
                                 central_rules_available = reactive(FALSE),
                                 manual_rules_available = reactive(FALSE),
                                 rv = NULL,
                                 debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management System
    # ========================================

    # Helper function for controlled debug output with consistent formatting
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "ASSIGN RULES", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ ASSIGN RULES ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Assign Rules module server starting", 2)

    # ========================================
    # Reactive State Container (WP3)
    # ========================================

    assign_rules_state <- create_assign_rules_state()

    # Core state management
    condition_inputs <- assign_rules_state$condition_inputs
    counter_condition <- assign_rules_state$counter_condition
    Options_condition <- assign_rules_state$Options_condition
    selected_rule_file <- assign_rules_state$selected_rule_file
    active_rule_file <- assign_rules_state$active_rule_file
    rule_loading_status <- assign_rules_state$rule_loading_status
    rule_load_pipeline_status <- assign_rules_state$rule_load_pipeline_status

    # UI configuration triggers for main module
    assign_rules_ui_imputation <- assign_rules_state$assign_rules_ui_imputation
    assign_rules_ui_filtering <- assign_rules_state$assign_rules_ui_filtering
    assign_rules_ui_ratios <- assign_rules_state$assign_rules_ui_ratios
    assign_rules_ui_batch_effects <- assign_rules_state$assign_rules_ui_batch_effects
    assign_rules_ui_pivot <- assign_rules_state$assign_rules_ui_pivot
    assign_rules_ui_merge <- assign_rules_state$assign_rules_ui_merge
    assign_rules_ui_edit <- assign_rules_state$assign_rules_ui_edit
    assign_rules_ui_basemean <- assign_rules_state$assign_rules_ui_basemean

    # Trigger for explicit re-application of metadata rules
    apply_metadata_rules_trigger <- assign_rules_state$apply_metadata_rules_trigger

    # Enhanced error tracking and performance monitoring
    processing_errors <- assign_rules_state$processing_errors
    processing_warnings <- assign_rules_state$processing_warnings
    processing_log <- assign_rules_state$processing_log
    ui_config_application_status <- assign_rules_state$ui_config_application_status
    ui_config_sources <- assign_rules_state$ui_config_sources
    ui_config_errors <- assign_rules_state$ui_config_errors
    last_loaded_rule_data <- assign_rules_state$last_loaded_rule_data
    processing_history <- assign_rules_state$processing_history
    last_processing_time <- assign_rules_state$last_processing_time

    # ========================================
    # Enhanced UI Configuration Setters with Retry Logic
    # ========================================

    set_imputation_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_imputation,
        validator = validate_ui_imputation_config,
        config_name = "imputation",
        source_name = "imputation",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_filtering_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_filtering,
        validator = validate_ui_filtering_config,
        config_name = "filtering",
        source_name = "filtering",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_batch_effects_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_batch_effects,
        validator = validate_ui_batch_effects_config,
        config_name = "batch_effects",
        source_name = "batch_effects",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_pivot_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_pivot,
        validator = validate_ui_pivot_config,
        config_name = "pivot",
        source_name = "pivot",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_merge_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_merge,
        validator = validate_ui_merge_config,
        config_name = "merge",
        source_name = "merge",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_edit_ui_config <- function(ui_config) {
      assign_rules_adapter_set_edit_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_edit,
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources,
        metadata_current = metadata_current
      )
    }

    set_ratios_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_ratios,
        validator = validate_ui_ratios_config,
        config_name = "ratios",
        source_name = "ratios",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    set_basemean_ui_config <- function(ui_config) {
      assign_rules_adapter_set_ui_config(
        ui_config = ui_config,
        reactive_setter = assign_rules_ui_basemean,
        validator = validate_ui_basemean_config,
        config_name = "basemean",
        source_name = "basemean",
        debug_log = debug_log,
        ui_config_errors = ui_config_errors,
        ui_config_sources = ui_config_sources
      )
    }

    # ========================================
    # Notification Aggregation
    # ========================================

    rule_load_events <- assign_rules_state$rule_load_events  # Collects single events
    aggregated_notifications_enabled <- assign_rules_state$aggregated_notifications_enabled # Switch

    reset_rule_load_events <- function() {
      rule_load_events(list())
    }

    add_rule_load_event <- function(type, key, message = NULL) {
      # type: "loaded", "pending", "failed", "missing", "info", "error"
      evs <- rule_load_events()
      evs[[length(evs) + 1]] <- list(
        type = type,
        key = key,
        message = message,
        timestamp = Sys.time()
      )
      rule_load_events(evs)
    }

    emit_aggregated_rule_notification <- function(base_success = TRUE) {
      evs <- rule_load_events()
      if (!length(evs)) return(invisible())

      # Group by type
      by_type <- split(evs, vapply(evs, function(x) x$type, character(1)))

      fmt_keys <- function(lst) {
        unique(vapply(lst, function(x) x$key, character(1)))
      }

      loaded   <- if ("loaded"   %in% names(by_type)) fmt_keys(by_type[["loaded"]]) else character(0)
      pending  <- if ("pending"  %in% names(by_type)) fmt_keys(by_type[["pending"]]) else character(0)
      failed   <- if ("failed"   %in% names(by_type)) fmt_keys(by_type[["failed"]]) else character(0)
      missing  <- if ("missing"  %in% names(by_type)) fmt_keys(by_type[["missing"]]) else character(0)
      infos    <- if ("info"     %in% names(by_type)) vapply(by_type[["info"]], function(x) x$message %||% "", character(1)) else character(0)
      errors   <- if ("error"    %in% names(by_type)) vapply(by_type[["error"]], function(x) x$message %||% "", character(1)) else character(0)

      parts <- character()

      if (length(loaded))  parts <- c(parts, paste0("Loaded: ", paste(loaded, collapse = ", ")))
      if (length(pending)) parts <- c(parts, paste0("Pending (metadata not ready): ", paste(pending, collapse = ", ")))
      if (length(failed))  parts <- c(parts, paste0("Failed: ", paste(failed, collapse = ", ")))
      # if (length(missing)) parts <- c(parts, paste0("No data for: ", paste(missing, collapse = ", ")))
      if (length(infos))   parts <- c(parts, paste0(paste(infos, collapse = " | ")))
      if (length(errors))  parts <- c(parts, paste0("Errors: ", paste(errors, collapse = " | ")))

      if (!length(parts)) {
        parts <- c("Rule set processed (no detailed events).")
      }

      final_msg <- paste(parts, collapse = "\n")

      # Define type
      notif_type <- if (length(failed) || length(errors)) "warning" else "message"

      showNotification(final_msg, type = notif_type, duration = if (notif_type == "message") 6 else 10)

      reset_rule_load_events()
    }

    # ========================================
    # Enhanced Rule File Management with Comprehensive Error Handling
    # ========================================
    # Enhanced rule file loading with proper reactive context handling
    register_assign_rules_rule_loading_handler(
      input = input,
      session = session,
      metadata_current = metadata_current,
      aggregated_notifications_enabled = aggregated_notifications_enabled,
      reset_rule_load_events = reset_rule_load_events,
      rule_loading_status = rule_loading_status,
      rule_load_pipeline_status = rule_load_pipeline_status,
      selected_rule_file = selected_rule_file,
      active_rule_file = active_rule_file,
      ui_config_application_status = ui_config_application_status,
      ui_config_errors = ui_config_errors,
      clear_processing_errors = clear_processing_errors,
      processing_errors = processing_errors,
      processing_warnings = processing_warnings,
      debug_log = debug_log,
      last_loaded_rule_data = last_loaded_rule_data,
      set_imputation_ui_config = set_imputation_ui_config,
      set_filtering_ui_config = set_filtering_ui_config,
      set_batch_effects_ui_config = set_batch_effects_ui_config,
      set_pivot_ui_config = set_pivot_ui_config,
      set_merge_ui_config = set_merge_ui_config,
      set_edit_ui_config = set_edit_ui_config,
      set_ratios_ui_config = set_ratios_ui_config,
      set_basemean_ui_config = set_basemean_ui_config,
      add_rule_load_event = add_rule_load_event,
      emit_aggregated_rule_notification = emit_aggregated_rule_notification,
      last_processing_time = last_processing_time,
      assign_rules_ui_imputation = assign_rules_ui_imputation,
      assign_rules_ui_filtering = assign_rules_ui_filtering,
      assign_rules_ui_batch_effects = assign_rules_ui_batch_effects,
      assign_rules_ui_pivot = assign_rules_ui_pivot,
      assign_rules_ui_merge = assign_rules_ui_merge,
      assign_rules_ui_edit = assign_rules_ui_edit,
      assign_rules_ui_ratios = assign_rules_ui_ratios,
      assign_rules_ui_basemean = assign_rules_ui_basemean,
      ui_config_sources = ui_config_sources
    )

    # ========================================
    # Enhanced Condition Management with Error Handling
    # ========================================
    # Add/remove condition handlers
    register_assign_rules_condition_management_handlers(
      input = input,
      counter_condition = counter_condition,
      condition_inputs = condition_inputs,
      Options_condition = Options_condition,
      debug_log = debug_log,
      processing_log = processing_log,
      processing_errors = processing_errors,
      processing_warnings = processing_warnings
    )

    # ========================================
    # Enhanced Dynamic UI Generation
    # ========================================

    # Render condition input textboxes with enhanced error handling
    output$textbox_ui_condition <- renderUI({
      tryCatch({
        n <- max(counter_condition(), 1)

        textboxes <- lapply(seq_len(n), function(i) {
          condition_id <- paste0("Condition_", i)
          current_value <- condition_inputs()[[condition_id]]

          if (is.null(current_value) || current_value == "") {
            current_value <- paste("Condition", i)
          }

          div(
            class = "condition-input-row",
            style = "display: flex; align-items: flex-end; gap: 6px; margin-bottom: 6px;",
            div(
              style = "flex: 1 1 auto; min-width: 0;",
              textInput(
                inputId = ns(paste0("textin", i)),
                label = paste("Condition Group", i, ":"),
                value = current_value,
                placeholder = paste("Enter name for condition", i)
              )
            ),
            actionButton(
              inputId = ns(paste0("remove_condition_", i)),
              label = "×",
              class = "btn-danger btn-xs",
              style = "width: 26px; height: 26px; min-width: 26px; padding: 0; margin-bottom: 15px; line-height: 1; border-radius: 2px; font-weight: bold;",
              title = paste("Remove Condition Group", i)
            )
          )
        })

        condition_blur_input_id <- ns("condition_text_blur")
        condition_input_id_prefix <- ns("")
        condition_blur_event_namespace <- gsub("[^A-Za-z0-9_]", "_", condition_input_id_prefix)

        condition_blur_binding <- tags$script(HTML(sprintf(
          "(function() {
  var selector = 'input[id^=\"%s\"][id*=\"textin\"]';
  var eventName = 'blur.conditionTextBlur_%s';
  $(document).off(eventName, selector).on(eventName, selector, function() {
    Shiny.setInputValue('%s', {id: this.id, value: this.value, nonce: Math.random()}, {priority: 'event'});
  });
})();",
          condition_input_id_prefix,
          condition_blur_event_namespace,
          condition_blur_input_id
        )))

        tagList(condition_blur_binding, do.call(tagList, textboxes))

      }, error = function(e) {
        debug_log(paste("Error rendering condition textboxes:", e$message), 1)
        add_processing_log("ui_rendering", "error", paste("Failed to render textboxes:", e$message),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        div(
          class = "alert alert-danger",
          paste("Error rendering condition inputs:", e$message)
        )
      })
    })

    outputOptions(output, "textbox_ui_condition", suspendWhenHidden = FALSE)

    # Dynamic UI for rule set loading with populated choices
    output$rule_set_ui <- renderUI({
      data_available <- FALSE

      # Use core_values instead of rv - safer access
      tryCatch({
        if (!is.null(metadata_current)) {
          current_metadata <- metadata_current()
          data_available <- !is.null(current_metadata) &&
            is.data.frame(current_metadata) &&
            nrow(current_metadata) > 0
        }
      }, error = function(e) {
        data_available <- FALSE
      })

      if (data_available) {
        # Get rule files for dropdown
        available_files <- c("Select a rule set..." = "")
        tryCatch({
          if (!is.null(rule_files) && is.reactive(rule_files)) {
            files <- rule_files()
            if (length(files) > 0) {
              available_files <- c(available_files, files)
            }
          }
        }, error = function(e) {
          debug_log(paste("Error getting rule files:", e$message), 2)
        })

        # Show rule set loading when metadata is available
        fluidRow(
          column(12,
                 div(
                   title = "Load a pre-configured set of assignment rules to automatically populate condition definitions",
                   selectInput(
                     ns("load_rule_set_dw"),
                     "Load Assignment Rule Set:",
                     choices = available_files,
                     width = "100%"
                   )
                 )
          )
        )
      } else {
        # Show info message when no data is loaded
        fluidRow(
          column(12,
                 div(
                   class = "alert alert-info",
                   style = "margin: 10px 0;",
                   HTML("<strong>Please load data before assigning metadata.</strong>")
                 )
          )
        )
      }
    })

    outputOptions(output, "rule_set_ui", suspendWhenHidden = FALSE)
    # Observer to update dropdown after UI is created (robust version)
    register_assign_rules_rule_files_dropdown_handler(
      rule_files = rule_files,
      input = input,
      session = session,
      debug_log = debug_log,
      add_processing_log = add_processing_log,
      processing_log = processing_log,
      processing_errors = processing_errors,
      processing_warnings = processing_warnings
    )

    # ========================================
    # Enhanced Condition Input Synchronization
    # ========================================
    # Sync condition inputs with reactive storage and options state
    register_assign_rules_condition_sync_handlers(
      input = input,
      counter_condition = counter_condition,
      condition_inputs = condition_inputs,
      Options_condition = Options_condition,
      debug_log = debug_log,
      processing_log = processing_log,
      processing_errors = processing_errors,
      processing_warnings = processing_warnings
    )

    # ========================================
    # Apply Metadata Rules Button Handler
    # ========================================

    observeEvent(input$apply_metadata_rules_btn, {
      debug_log("Apply Metadata Rules button clicked", 1)
      apply_metadata_rules_trigger(isolate(apply_metadata_rules_trigger()) + 1L)
    }, ignoreInit = TRUE)

    # ========================================
    # Enhanced Current Conditions Reactive
    # ========================================

    # Get current condition names with validation
    currentConditions <- reactive({
      tryCatch({
        n <- max(counter_condition(), 1)
        conditions <- sapply(seq_len(n), function(i) {
          input_val <- input[[paste0("textin", i)]]
          if (is.null(input_val) || trimws(input_val) == "") {
            paste("Condition", i)
          } else {
            trimws(input_val)
          }
        })

        return(conditions)

      }, error = function(e) {
        debug_log(paste("Error getting current conditions:", e$message), 1)
        return(character(0))
      })
    })

    # ========================================
    # Enhanced UI Outputs with Status Icons and Comprehensive Information
    # ========================================

    # Readiness output for conditional UI (true only if metadata is available and non-empty)
    output$assign_ready <- renderText({
      ready <- FALSE
      if (!is.null(metadata_current) && is.reactive(metadata_current)) {
        m <- tryCatch(metadata_current(), error = function(e) NULL)
        ready <- (!is.null(m) && is.data.frame(m) && nrow(m) > 0 && ncol(m) > 0)
      }
      if (isTRUE(ready)) "true" else "false"
    })

    outputOptions(output, "assign_ready", suspendWhenHidden = FALSE)

    # Rule loading status output for conditional UI
    output$rule_loading_status_output <- reactive({
      return(rule_loading_status())
    })
    outputOptions(output, "rule_loading_status_output", suspendWhenHidden = FALSE)

    has_structured_rule_tables <- function(x) {
      is.list(x) &&
        (is.data.frame(x$table) || is.data.frame(x$condition) || is.data.frame(x$ratio))
    }

    get_central_rules_available <- reactive({
      val <- FALSE
      tryCatch({
        if (!is.null(central_rules_available) && is.reactive(central_rules_available)) {
          val <- isTRUE(central_rules_available())
        }
      }, error = function(e) {
        debug_log(paste("rules_available_output: central_rules_available() failed:", e$message), 1)
        val <- FALSE
      })
      val
    })

    get_manual_rules_available <- reactive({
      val <- FALSE
      tryCatch({
        if (!is.null(manual_rules_available) && is.reactive(manual_rules_available)) {
          val <- isTRUE(manual_rules_available())
        }
      }, error = function(e) {
        debug_log(paste("rules_available_output: manual_rules_available() failed:", e$message), 1)
        val <- FALSE
      })
      val
    })

    if (!is.null(rv)) {
      observeEvent(rv$session_restore_trigger, {
        central_snapshot_available <- FALSE
        tryCatch({
          central_snapshot_available <- isTRUE(has_structured_rule_tables(rv$central_loaded_rules)) ||
            isTRUE(has_structured_rule_tables(rv$central_rule_file))
        }, error = function(e) {
          central_snapshot_available <- FALSE
        })

        debug_log(paste(
          "AssignRules restore trigger observed:",
          "trigger=", as.character(rv$session_restore_trigger),
          "| last_loaded_rule_data=", if (!is.null(last_loaded_rule_data())) "set" else "NULL",
          "| local_ui_configs_non_null=", sum(c(
            !is.null(assign_rules_ui_imputation()),
            !is.null(assign_rules_ui_filtering()),
            !is.null(assign_rules_ui_batch_effects()),
            !is.null(assign_rules_ui_pivot()),
            !is.null(assign_rules_ui_merge()),
            !is.null(assign_rules_ui_edit()),
            !is.null(assign_rules_ui_ratios()),
            !is.null(assign_rules_ui_basemean())
          )),
          "| central_rules_available_reactive=", get_central_rules_available(),
          "| manual_rules_available_reactive=", get_manual_rules_available(),
          "| rv.central_*_has_tables=", central_snapshot_available
        ), 1)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }

    # Rules availability output for conditional UI (persists after dropdown reset,
    # unlike rule_loading_status which reverts to "idle" when the selectInput is cleared).
    # During session-restore, rule payloads can be rehydrated through module bridges
    # that repopulate UI configs without always restoring last_loaded_rule_data first.
    # Treat either source as "rules available" for Apply Metadata Rules visibility.
    output$rules_available_output <- reactive({
      has_loaded_rule_data <- !is.null(last_loaded_rule_data())
      has_restored_ui_config <- any(c(
        !is.null(assign_rules_ui_imputation()),
        !is.null(assign_rules_ui_filtering()),
        !is.null(assign_rules_ui_batch_effects()),
        !is.null(assign_rules_ui_pivot()),
        !is.null(assign_rules_ui_merge()),
        !is.null(assign_rules_ui_edit()),
        !is.null(assign_rules_ui_ratios()),
        !is.null(assign_rules_ui_basemean())
      ))
      has_central_rules <- isTRUE(get_central_rules_available())
      has_manual_rules <- isTRUE(get_manual_rules_available())

      availability <- isTRUE(has_loaded_rule_data) ||
        isTRUE(has_restored_ui_config) ||
        isTRUE(has_central_rules) ||
        isTRUE(has_manual_rules)

      debug_log(paste(
        "rules_available_output evaluated:",
        "last_loaded_rule_data=", has_loaded_rule_data,
        "| restored_ui_config=", has_restored_ui_config,
        "| central_rules=", has_central_rules,
        "| manual_rules=", has_manual_rules,
        "| result=", if (availability) "true" else "false"
      ), 2)

      if (isTRUE(availability)) "true" else "false"
    })
    outputOptions(output, "rules_available_output", suspendWhenHidden = FALSE)

    # ========================================
    # Enhanced External Interface Functions
    # ========================================

    #' Apply pending edit configuration when metadata becomes available
    apply_pending_edit_config <- function() {
      tryCatch({
        current_sources <- ui_config_sources()
        if (!is.null(current_sources[["edit"]]) && current_sources[["edit"]] == "rule_file_pending") {
          current_config <- assign_rules_ui_edit()
          if (!is.null(current_config)) {
            debug_log("Applying pending edit config - metadata now available", 2)

            # Simply update the source status - the edit module will handle the config through reactive
            current_sources[["edit"]] <- "rule_file"
            ui_config_sources(current_sources)

            add_processing_log("edit_config", "success", "Applied pending edit config",
                               processing_log = processing_log, processing_errors = processing_errors,
                               processing_warnings = processing_warnings, debug_log = debug_log)
            return(TRUE)
          }
        }
        return(FALSE)
      }, error = function(e) {
        debug_log(paste("Error applying pending edit config:", e$message), 1)
        add_processing_log("edit_config", "error", paste("Failed to apply pending config:", e$message),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        return(FALSE)
      })
    }

    #' Apply processing to metadata dataframe with enhanced error handling
    apply_processing <- function(metadata_df) {
      # Resolve a reactive metadata source here, while Shiny's reactive context is
      # available; the processing and eligibility helpers remain pure functions.
      if (shiny::is.reactive(metadata_df)) {
        metadata_df <- metadata_df()
      }

      if (is.null(metadata_df)) {
        debug_log("No metadata to process", 2)
        return(metadata_df)
      }

      if (!is.data.frame(metadata_df)) {
        debug_log("Metadata is not a data frame", 1)
        return(metadata_df)
      }

      required_cols <- c("Column", "Content", "Options")
      missing_cols <- setdiff(required_cols, names(metadata_df))
      if (length(missing_cols) > 0L) {
        debug_log(paste("Metadata is missing required columns:", paste(missing_cols, collapse = ", ")), 1)
        return(metadata_df)
      }

      if (nrow(metadata_df) == 0L) {
        debug_log("No metadata to process", 2)
        return(metadata_df)
      }

      if (!metadata_needs_sample_processing(metadata_df)) {
        debug_log("Metadata has no missing sample assignments; skipping sample processing", 2)

        # Preserve the existing lightweight cleanup that removes Sample values
        # from rows whose Content does not carry a sample assignment.
        if ("Sample" %in% names(metadata_df)) {
          return(postprocess_dataframe(metadata_df))
        }
        return(metadata_df)
      }

      processing_start_time <- Sys.time()

      tryCatch({
        debug_log(paste("Processing metadata with", nrow(metadata_df), "rows"), 2)
        add_processing_log("metadata_processing", "starting", paste("Processing", nrow(metadata_df), "rows"),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)

        processed_data <- process_dataframe(metadata_df)
        postprocessed_data <- postprocess_dataframe(processed_data)

        processing_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))
        add_processing_log("metadata_processing", "success", "Completed", processing_duration,
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        debug_log(paste("Metadata processing completed in", sprintf("%.2fs", processing_duration)), 2)

        return(postprocessed_data)

      }, error = function(e) {
        processing_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))
        debug_log(paste("Error in apply_processing:", e$message), 1)
        add_processing_log("metadata_processing", "error", e$message, processing_duration,
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        showNotification(paste("Error processing metadata:", e$message), type = "error", duration = 6)
        return(metadata_df)
      })
    }

    # Enhanced getter functions for UI configurations
    get_imputation_ui_config <- function() { return(assign_rules_ui_imputation()) }
    get_filtering_ui_config <- function() { return(assign_rules_ui_filtering()) }
    get_ratios_ui_config <- function() { return(assign_rules_ui_ratios()) }
    get_batch_effects_ui_config <- function() { return(assign_rules_ui_batch_effects()) }
    get_pivot_ui_config <- function() { return(assign_rules_ui_pivot()) }
    get_merge_ui_config <- function() { return(assign_rules_ui_merge()) }
    get_edit_ui_config <- function() { return(assign_rules_ui_edit()) }
    get_basemean_ui_config <- function() { return(assign_rules_ui_basemean()) }

    # Enhanced backward compatibility functions
    get_imputation_defaults_function <- function() { return(assign_rules_ui_imputation()) }
    set_imputation_defaults_function <- function(method = "None", columns = character(0)) {
      ui_config <- list(
        imputation_method_select = method,
        imputation_column_select = columns
      )
      return(set_imputation_ui_config(ui_config))
    }

    #' Set conditions externally with enhanced validation
    set_conditions_function <- function(condition_list) {
      if (!is.list(condition_list) || length(condition_list) == 0) {
        debug_log("Invalid condition list provided", 1)
        return(FALSE)
      }

      tryCatch({
        debug_log(paste("Setting", length(condition_list), "conditions externally"), 2)
        add_processing_log("external_conditions", "starting", paste("Setting", length(condition_list), "conditions"),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)

        valid_conditions <- list()
        for (i in seq_along(condition_list)) {
          condition_value <- condition_list[[i]]
          if (!is.null(condition_value) && is.character(condition_value) && nzchar(condition_value)) {
            condition_id <- if (!is.null(names(condition_list)[i])) {
              names(condition_list)[i]
            } else {
              paste0("Condition_", i)
            }
            valid_conditions[[condition_id]] <- condition_value
          }
        }

        if (length(valid_conditions) > 0) {
          condition_inputs(valid_conditions)
          counter_condition(length(valid_conditions))

          all_options <- c(NA_character_, unname(unlist(valid_conditions)))
          Options_condition(all_options)

          # Update UI inputs safely
          session$onFlushed(function() {
            for (i in seq_along(valid_conditions)) {
              input_id <- paste0("textin", i)
              condition_value <- valid_conditions[[i]]

              tryCatch({
                updateTextInput(session, input_id, value = condition_value)
              }, error = function(e) {
                debug_log(paste("Error updating input", input_id, ":", e$message), 2)
              })
            }
          })

          add_processing_log("external_conditions", "success", paste("Set", length(valid_conditions), "conditions"),
                             processing_log = processing_log, processing_errors = processing_errors,
                             processing_warnings = processing_warnings, debug_log = debug_log)
          debug_log(paste("Successfully set", length(valid_conditions), "conditions"), 2)
          return(TRUE)
        }

        add_processing_log("external_conditions", "warning", "No valid conditions provided",
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        return(FALSE)

      }, error = function(e) {
        debug_log(paste("Error in set_conditions_function:", e$message), 1)
        add_processing_log("external_conditions", "error", e$message,
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        return(FALSE)
      })
    }

    #' Merge extra condition labels into the dropdown source AND ensure every
    #' extracted condition gets its own textbox_ui_condition textbox, without
    #' overwriting the user's own already-typed condition-group names.
    #'
    #' Unlike set_conditions_function() (which fully replaces the condition
    #' groups and is used when a rule file is loaded), this appends any
    #' condition values auto-detected/auto-filled by the Auto-Assign engine
    #' (e.g. via "Apply Metadata Rules") that are not already represented by
    #' an existing condition-group textbox: it adds new Condition_N entries
    #' (and grows counter_condition accordingly) for those, and always keeps
    #' Options_condition (the metadata table's condition dropdown) in sync.
    merge_condition_options_function <- function(extra_values) {
      # Auto-detected condition merges intentionally store only condition labels;
      # do not materialize, copy, or write the metadata table from this path.
      if (!is.character(extra_values) || length(extra_values) == 0) {
        return(FALSE)
      }

      tryCatch({
        extra_values <- unique(trimws(extra_values))
        extra_values <- extra_values[nzchar(extra_values)]
        if (length(extra_values) == 0) return(FALSE)

        current_inputs <- condition_inputs()
        if (!is.list(current_inputs)) current_inputs <- list()

        existing_values <- trimws(unname(unlist(current_inputs, use.names = FALSE)))
        existing_values <- existing_values[!is.na(existing_values) & nzchar(existing_values)]

        new_values <- extra_values[!(extra_values %in% existing_values)]

        if (length(new_values) > 0) {
          # counter_condition and length(current_inputs) should normally be
          # identical (condition IDs are always sequential Condition_1..N -
          # see register_assign_rules_condition_management_handlers); take
          # the max defensively so a transient mismatch never causes a
          # duplicate Condition_N id to be assigned.
          current_count <- max(counter_condition(), length(current_inputs), 0)
          new_entries <- list()

          for (value in new_values) {
            current_count <- current_count + 1
            condition_id <- paste0("Condition_", current_count)
            current_inputs[[condition_id]] <- value
            new_entries[[condition_id]] <- value
          }

          condition_inputs(current_inputs)
          counter_condition(current_count)

          # Populate the newly created textboxes with their extracted values
          # once the UI has re-rendered them (renderUI reacts to the
          # counter_condition/condition_inputs changes above). Capture the
          # values in `new_entries` now so later mutations of
          # `current_inputs`/`condition_inputs()` can't change what gets
          # written into the textboxes.
          session$onFlushed(function() {
            for (condition_id in names(new_entries)) {
              idx <- as.integer(sub("^Condition_", "", condition_id))
              input_id <- paste0("textin", idx)
              tryCatch({
                updateTextInput(session, input_id, value = new_entries[[condition_id]])
              }, error = function(e) {
                debug_log(paste("Error updating input", input_id, ":", e$message), 2)
              })
            }
          })

          debug_log(paste("Added", length(new_values), "new condition-group textbox(es) from auto-detected condition(s)"), 2)
        }

        current_options <- Options_condition()
        if (!is.character(current_options)) current_options <- character(0)

        merged <- unique(c(current_options, extra_values))
        # Keep a single NA placeholder ("no selection") at the front.
        merged <- c(NA_character_, merged[!is.na(merged) & nzchar(merged)])

        Options_condition(merged)
        debug_log(paste("Merged", length(extra_values), "auto-detected condition option(s)"), 2)
        TRUE
      }, error = function(e) {
        debug_log(paste("Error merging condition options:", e$message), 1)
        FALSE
      })
    }

    #' Set selected rule file externally with validation
    set_selected_rule_file_function <- function(file_name) {
      if (!is.character(file_name) || !nzchar(file_name)) {
        debug_log("Invalid file name provided", 1)
        return(FALSE)
      }

      tryCatch({
        debug_log(paste("Setting rule file externally:", file_name), 2)
        updateSelectInput(session, "load_rule_set_dw", selected = file_name)
        selected_rule_file(file_name)
        add_processing_log("external_rule_file", "success", paste("Set rule file:", file_name),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        return(TRUE)
      }, error = function(e) {
        debug_log(paste("Error setting rule file:", e$message), 1)
        add_processing_log("external_rule_file", "error", e$message,
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        return(FALSE)
      })
    }

    #' Update rule loading status externally
    update_rule_status_function <- function(status) {
      valid_statuses <- c("idle", "loading", "loaded", "error")
      if (!status %in% valid_statuses) {
        debug_log(paste("Invalid status provided:", status), 1)
        return(FALSE)
      }

      tryCatch({
        rule_loading_status(status)
        add_processing_log("rule_status_update", "info", paste("Status updated to:", status),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        debug_log(paste("Rule status updated to:", status), 2)
        return(TRUE)
      }, error = function(e) {
        debug_log(paste("Error updating rule status:", e$message), 1)
        return(FALSE)
      })
    }

    # ========================================
    # Enhanced Session Cleanup
    # ========================================

    # Register cleanup function
    cleanup_manager$register_module("Assign Rules", function() {
      debug_log("Executing [Assign Rules] cleanup", 2)

      # Clear all reactive values safely
      condition_inputs(list(Condition_1 = "Condition 1"))
      counter_condition(1)
      Options_condition(c(NA_character_, "Condition 1"))
      selected_rule_file("")
      active_rule_file("")
      rule_loading_status("idle")

      # Clear UI configurations
      assign_rules_ui_imputation(NULL)
      assign_rules_ui_filtering(NULL)
      assign_rules_ui_ratios(NULL)
      assign_rules_ui_batch_effects(NULL)
      assign_rules_ui_pivot(NULL)
      assign_rules_ui_merge(NULL)
      assign_rules_ui_edit(NULL)

      # Clear tracking and logs
      processing_errors(list())
      processing_warnings(list())
      processing_log(list())
      ui_config_application_status("idle")
      ui_config_sources(assign_rules_default_ui_config_sources())
      ui_config_errors(list())
      last_loaded_rule_data(NULL)
      processing_history(list())
      last_processing_time(NULL)

      debug_log("[Assign Rules] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    # The assign-rules submodule has two flavors of state to round-trip:
    #   (1) user-visible inputs: `load_rule_set_dw` + per-row textInputs
    #       `textin{i}` (rendered dynamically, one per condition group).
    #   (2) reactive state backing those inputs: `condition_inputs`,
    #       `counter_condition`, `selected_rule_file`. On restore we need
    #       to rehydrate this state first so that renderUI paints the
    #       textInputs with the correct values.
    # We use the shared factory to cover `load_rule_set_dw` and wrap its
    # getters/setters to also capture the reactive state in `extra`.
    assign_rules_session_state_base <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        load_rule_set_dw = "selectInput"
      ),
      module_label    = "AssignRules",
      restore_trigger = if (!is.null(rv)) {
        reactive({ rv$session_restore_trigger })
      } else {
        reactive(NULL)
      }
    )
    pending_dynamic_text_restore <- reactiveVal(NULL)

    get_session_state_fn <- function() {
      base <- assign_rules_session_state_base$get_session_state()
      # Snapshot per-row textInputs that are currently rendered.
      dyn_inputs <- list()
      n <- tryCatch(isolate(counter_condition()), error = function(e) 0L)
      n <- if (is.numeric(n) && n > 0) as.integer(n) else 0L
      if (n > 0L) {
        for (i in seq_len(n)) {
          tid <- paste0("textin", i)
          v <- tryCatch(isolate(input[[tid]]), error = function(e) NULL)
          if (is.character(v) && length(v) == 1L) {
            dyn_inputs[[tid]] <- v
          }
        }
      }
      base$ui_inputs <- c(base$ui_inputs, dyn_inputs)
      saved_condition_count <- tryCatch(length(isolate(condition_inputs())), error = function(e) 0L)
      debug_log(paste("AssignRules save: saved condition count", saved_condition_count), 1)
      base$extra <- list(
        condition_inputs   = tryCatch(isolate(condition_inputs()),
                                      error = function(e) NULL),
        counter_condition  = tryCatch(isolate(counter_condition()),
                                      error = function(e) NULL),
        Options_condition  = tryCatch(isolate(Options_condition()),
                                      error = function(e) NULL),
        selected_rule_file = tryCatch(isolate(selected_rule_file()),
                                      error = function(e) NULL),
        active_rule_file = tryCatch(isolate(active_rule_file()),
                                    error = function(e) NULL),
        rule_loading_status = tryCatch(isolate(rule_loading_status()),
                                       error = function(e) NULL),
        last_loaded_rule_data = tryCatch(isolate(last_loaded_rule_data()),
                                         error = function(e) NULL),
        ui_configs = list(
          imputation    = tryCatch(isolate(assign_rules_ui_imputation()),
                                   error = function(e) NULL),
          filtering     = tryCatch(isolate(assign_rules_ui_filtering()),
                                   error = function(e) NULL),
          ratios        = tryCatch(isolate(assign_rules_ui_ratios()),
                                   error = function(e) NULL),
          batch_effects = tryCatch(isolate(assign_rules_ui_batch_effects()),
                                   error = function(e) NULL),
          pivot         = tryCatch(isolate(assign_rules_ui_pivot()),
                                   error = function(e) NULL),
          merge         = tryCatch(isolate(assign_rules_ui_merge()),
                                   error = function(e) NULL),
          edit          = tryCatch(isolate(assign_rules_ui_edit()),
                                   error = function(e) NULL),
          basemean      = tryCatch(isolate(assign_rules_ui_basemean()),
                                   error = function(e) NULL)
        )
      )
      base
    }

    restore_condition_state_fn <- function(state, restore_inputs = TRUE) {
      if (is.null(state) || !is.list(state)) return(invisible(NULL))
      extra <- state$extra
      restored_count <- 0L
      restored_dynamic_ids <- character(0)
      dropped_dynamic_ids <- character(0)
      dynamic_text_inputs <- list()
      textin_ids <- character(0)
      textin_idx <- integer(0)
      max_textin_index <- 0L

      if (is.list(state$ui_inputs)) {
        textin_ids <- grep("^textin[0-9]+$", names(state$ui_inputs), value = TRUE)
        textin_idx <- suppressWarnings(as.integer(sub("^textin", "", textin_ids)))
        valid_textin <- !is.na(textin_idx)
        textin_ids <- textin_ids[valid_textin]
        textin_idx <- textin_idx[valid_textin]
        if (length(textin_idx) > 0L) {
          max_textin_index <- max(textin_idx, 0L)
        }

        for (i in seq_along(textin_ids)) {
          input_value <- state$ui_inputs[[textin_ids[[i]]]]
          if (is.character(input_value) && length(input_value) == 1L && nzchar(input_value)) {
            dynamic_text_inputs[[textin_ids[[i]]]] <- input_value
          }
        }
      }

      if (is.list(extra)) {
        tryCatch({
          overlaid_condition_inputs <- if (!is.null(extra$condition_inputs) && is.list(extra$condition_inputs)) {
            extra$condition_inputs
          } else {
            list()
          }

          if (length(dynamic_text_inputs) > 0L) {
            for (id in names(dynamic_text_inputs)) {
              idx <- suppressWarnings(as.integer(sub("^textin", "", id)))
              if (!is.na(idx)) {
                overlaid_condition_inputs[[paste0("Condition_", idx)]] <- dynamic_text_inputs[[id]]
              }
            }
          }

          overlaid_values <- unname(unlist(overlaid_condition_inputs, use.names = FALSE))
          overlaid_values <- overlaid_values[!is.na(overlaid_values) & nzchar(overlaid_values)]
          saved_options <- extra$Options_condition
          saved_options_non_empty <- !is.null(saved_options) &&
            length(saved_options) > 0L &&
            any(!is.na(saved_options) & nzchar(as.character(saved_options)))

          condition_inputs(overlaid_condition_inputs)
          restored_count <- length(overlaid_condition_inputs)

          saved_counter <- if (is.numeric(extra$counter_condition) && length(extra$counter_condition) == 1L) {
            as.integer(extra$counter_condition)
          } else {
            0L
          }
          restored_counter <- max(saved_counter, length(overlaid_condition_inputs), max_textin_index, 1L)
          counter_condition(restored_counter)

          if (saved_options_non_empty) {
            Options_condition(saved_options)
          } else {
            Options_condition(c(NA_character_, overlaid_values))
          }

          if (is.character(extra$selected_rule_file) &&
              length(extra$selected_rule_file) == 1L) {
            selected_rule_file(extra$selected_rule_file)
          }
          if (is.character(extra$active_rule_file) &&
              length(extra$active_rule_file) == 1L) {
            active_rule_file(extra$active_rule_file)
          }
          if (is.character(extra$rule_loading_status) &&
              length(extra$rule_loading_status) == 1L) {
            rule_loading_status(extra$rule_loading_status)
          }
          if (!is.null(extra$last_loaded_rule_data)) {
            last_loaded_rule_data(extra$last_loaded_rule_data)
          }
          if (is.list(extra$ui_configs)) {
            uc <- extra$ui_configs
            if (!is.null(uc$imputation))    assign_rules_ui_imputation(uc$imputation)
            if (!is.null(uc$filtering))     assign_rules_ui_filtering(uc$filtering)
            if (!is.null(uc$ratios))        assign_rules_ui_ratios(uc$ratios)
            if (!is.null(uc$batch_effects)) assign_rules_ui_batch_effects(uc$batch_effects)
            if (!is.null(uc$pivot))         assign_rules_ui_pivot(uc$pivot)
            if (!is.null(uc$merge))         assign_rules_ui_merge(uc$merge)
            if (!is.null(uc$edit))          assign_rules_ui_edit(uc$edit)
            if (!is.null(uc$basemean))      assign_rules_ui_basemean(uc$basemean)
          }
        }, error = function(e) {
          debug_log(paste("AssignRules restore: condition-state hydrate failed:", e$message), 1)
        })
      }

      if (is.list(state$ui_inputs)) {
        max_textin <- max(as.integer(counter_condition()), 0L)
        restored_dynamic_ids <- names(dynamic_text_inputs)
        restored_idx <- suppressWarnings(as.integer(sub("^textin", "", restored_dynamic_ids)))
        restored_dynamic_ids <- restored_dynamic_ids[!is.na(restored_idx) & restored_idx <= max_textin]
        dropped_dynamic_ids <- setdiff(textin_ids, restored_dynamic_ids)
        dynamic_text_inputs <- dynamic_text_inputs[restored_dynamic_ids]
      }

      debug_log(paste("AssignRules restore: restored condition count", restored_count), 1)
      debug_log(paste("AssignRules restore: restored Options_condition values",
                      paste(as.character(Options_condition()), collapse = ", ")), 1)
      debug_log(paste("AssignRules restore: dynamic input IDs restored=",
                      paste(restored_dynamic_ids, collapse = ", "),
                      "| dropped=", paste(dropped_dynamic_ids, collapse = ", ")), 1)

      if (isTRUE(restore_inputs)) {
        # `textin{i}` controls are rendered dynamically after condition state is
        # hydrated, so do not send them through the generic bridge (which only
        # knows about static specs such as `load_rule_set_dw`). Once Shiny has
        # flushed the rendered rows, restore each known dynamic text input with
        # the text-input-specific updater.
        if (length(dynamic_text_inputs) > 0L) {
          pending_dynamic_text_restore(dynamic_text_inputs)
          apply_dynamic_text_inputs <- function(pass = "primary") {
            max_textin <- tryCatch(
              max(as.integer(isolate(counter_condition())), 0L),
              error = function(e) 0L
            )
            bound_input_ids <- tryCatch(
              isolate(names(input)),
              error = function(e) character(0)
            )
            unresolved_ids <- character(0)

            for (id in names(dynamic_text_inputs)) {
              idx <- suppressWarnings(as.integer(sub("^textin", "", id)))
              if (!is.na(idx) && idx <= max_textin) {
                updateTextInput(session, id, value = dynamic_text_inputs[[id]])
                if (!id %in% bound_input_ids) {
                  unresolved_ids <- c(unresolved_ids, id)
                }
              }
            }

            if (identical(pass, "retry") && length(unresolved_ids) > 0L) {
              debug_log(paste(
                "AssignRules restore: dynamic text inputs still unbound after retry:",
                paste(unresolved_ids, collapse = ", ")
              ), 1)
            }

            unresolved_ids
          }

          session$onFlushed(once = TRUE, function() {
            unresolved_ids <- apply_dynamic_text_inputs("primary")
            if (length(unresolved_ids) > 0L) {
              session$onFlushed(once = TRUE, function() {
                apply_dynamic_text_inputs("retry")
              })
            }
          })
        }

        static_state <- state
        if (is.list(static_state$ui_inputs)) {
          static_state$ui_inputs[textin_ids] <- NULL
        }
        assign_rules_session_state_base$set_session_state(static_state)
      }
      invisible(NULL)
    }

    set_session_state_fn <- function(state) {
      restore_condition_state_fn(state, restore_inputs = TRUE)
    }

    if (!is.null(rv)) {
      observeEvent(rv$session_restore_trigger, {
        pending <- pending_dynamic_text_restore()
        if (!is.list(pending) || length(pending) == 0L) return(NULL)

        apply_pending_dynamic_restore <- function(pass = "restore_trigger") {
          current_pending <- pending_dynamic_text_restore()
          if (!is.list(current_pending) || length(current_pending) == 0L) return(invisible(NULL))
          max_textin <- tryCatch(
            max(as.integer(isolate(counter_condition())), 0L),
            error = function(e) 0L
          )
          bound_input_ids <- tryCatch(
            isolate(names(input)),
            error = function(e) character(0)
          )
          unresolved_ids <- character(0)
          for (id in names(current_pending)) {
            idx <- suppressWarnings(as.integer(sub("^textin", "", id)))
            if (!is.na(idx) && idx <= max_textin) {
              updateTextInput(session, id, value = current_pending[[id]])
              if (!id %in% bound_input_ids) unresolved_ids <- c(unresolved_ids, id)
            }
          }
          if (length(unresolved_ids) == 0L) {
            pending_dynamic_text_restore(NULL)
          } else if (identical(pass, "retry")) {
            debug_log(paste(
              "AssignRules restore: dynamic text inputs still unbound after restore-trigger retry:",
              paste(unresolved_ids, collapse = ", ")
            ), 1)
          }
          invisible(unresolved_ids)
        }

        session$onFlushed(once = TRUE, function() {
          unresolved_ids <- apply_pending_dynamic_restore("restore_trigger")
          if (length(unresolved_ids) > 0L) {
            session$onFlushed(once = TRUE, function() {
              apply_pending_dynamic_restore("retry")
            })
          }
        })
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }

    # ========================================
    # Enhanced Return Values
    # ========================================

    return(list(
      # Session-restore bridge
      get_session_state = get_session_state_fn,
      set_session_state = set_session_state_fn,
      restore_condition_state = restore_condition_state_fn,

      # Condition management
      current_conditions = currentConditions,
      condition_options = Options_condition,
      condition_count = counter_condition,

      # Rule file management
      selected_rule_file = selected_rule_file,
      active_rule_file = active_rule_file,
      last_applied_rule_file = active_rule_file,
      rule_loading_status = rule_loading_status,
      rule_load_pipeline_status = rule_load_pipeline_status,

      # UI configuration management
      assign_rules_ui_imputation = assign_rules_ui_imputation,
      assign_rules_ui_filtering = assign_rules_ui_filtering,
      assign_rules_ui_ratios = assign_rules_ui_ratios,
      assign_rules_ui_batch_effects = assign_rules_ui_batch_effects,
      assign_rules_ui_pivot = assign_rules_ui_pivot,
      assign_rules_ui_merge = assign_rules_ui_merge,
      assign_rules_ui_edit = assign_rules_ui_edit,

      # Enhanced getter functions
      get_imputation_ui_config = get_imputation_ui_config,
      get_filtering_ui_config = get_filtering_ui_config,
      get_ratios_ui_config = get_ratios_ui_config,
      get_batch_effects_ui_config = get_batch_effects_ui_config,
      get_pivot_ui_config = get_pivot_ui_config,
      get_merge_ui_config = get_merge_ui_config,
      get_edit_ui_config = get_edit_ui_config,
      get_basemean_ui_config = get_basemean_ui_config,

      # Enhanced setter functions
      set_imputation_ui_config = set_imputation_ui_config,
      set_filtering_ui_config = set_filtering_ui_config,
      set_batch_effects_ui_config = set_batch_effects_ui_config,
      set_pivot_ui_config = set_pivot_ui_config,
      set_merge_ui_config = set_merge_ui_config,
      set_ratios_ui_config = set_ratios_ui_config,
      set_edit_ui_config = set_edit_ui_config,

      # Enhanced status tracking
      get_ui_config_status = reactive({
        tryCatch({
          list(
            application_status = ui_config_application_status(),
            selected_rule_file = selected_rule_file(),
            active_rule_file = active_rule_file(),
            sources = ui_config_sources(),
            errors = ui_config_errors(),
            imputation_available = !is.null(assign_rules_ui_imputation()),
            filtering_available = !is.null(assign_rules_ui_filtering()),
            batch_effects_available = !is.null(assign_rules_ui_batch_effects()),
            pivot_available = !is.null(assign_rules_ui_pivot()),
            merge_available = !is.null(assign_rules_ui_merge()),
            edit_available = !is.null(assign_rules_ui_edit()),
            ratios_available = !is.null(assign_rules_ui_ratios()),
            edit_pending = !is.null(ui_config_sources()[["edit"]]) && ui_config_sources()[["edit"]] == "rule_file_pending",
            error_count = length(processing_errors()),
            warning_count = length(processing_warnings())
          )
        }, error = function(e) {
          debug_log(paste("Error getting UI config status:", e$message), 1)
          list(
            application_status = "error",
            error_message = e$message
          )
        })
      }),

      # Enhanced backward compatibility
      get_imputation_defaults = reactive({ get_imputation_defaults_function() }),
      set_imputation_defaults = set_imputation_defaults_function,

      # Enhanced external interface functions
      set_conditions = set_conditions_function,
      merge_condition_options = merge_condition_options_function,
      set_selected_rule_file = set_selected_rule_file_function,
      update_rule_status = update_rule_status_function,
      apply_pending_edit_config = apply_pending_edit_config,
      apply_metadata_rules_trigger = apply_metadata_rules_trigger,

      # Processing functions with error handling
      apply_processing = apply_processing,
      process_dataframe = process_dataframe,
      postprocess_dataframe = postprocess_dataframe,

      # Enhanced management functions
      add_condition = function() {
        tryCatch({
          counter_condition(counter_condition() + 1)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error in add_condition:", e$message), 1)
          return(FALSE)
        })
      },
      remove_condition = function() {
        tryCatch({
          if (counter_condition() > 1) {
            counter_condition(counter_condition() - 1)
            return(TRUE)
          }
          return(FALSE)
        }, error = function(e) {
          debug_log(paste("Error in remove_condition:", e$message), 1)
          return(FALSE)
        })
      },

      # Enhanced status functions
      has_conditions = reactive({
        tryCatch({
          length(currentConditions()) > 0
        }, error = function(e) {
          debug_log(paste("Error checking has_conditions:", e$message), 1)
          return(FALSE)
        })
      }),
      has_rule_file_selected = reactive({
        tryCatch({
          nzchar(selected_rule_file())
        }, error = function(e) {
          debug_log(paste("Error checking has_rule_file_selected:", e$message), 1)
          return(FALSE)
        })
      }),
      get_condition_inputs = reactive({ condition_inputs() }),
      get_last_loaded_rule_data = reactive({ last_loaded_rule_data() }),
      get_active_rule_file = reactive({ active_rule_file() }),
      get_last_applied_rule_file = reactive({ active_rule_file() }),

      # Enhanced utility functions
      has_imputation_ui_config = reactive({ !is.null(assign_rules_ui_imputation()) }),
      has_filtering_ui_config = reactive({ !is.null(assign_rules_ui_filtering()) }),
      has_edit_ui_config = reactive({ !is.null(assign_rules_ui_edit()) }),
      has_pending_edit_config = reactive({
        current_sources <- ui_config_sources()
        !is.null(current_sources[["edit"]]) && current_sources[["edit"]] == "rule_file_pending"
      }),

      # Edit-specific utilities
      get_edit_operations_table = reactive({
        tryCatch({
          edit_config <- assign_rules_ui_edit()
          if (!is.null(edit_config) && !is.null(edit_config$operations_table)) {
            return(edit_config$operations_table)
          }
          return(data.frame(
            Operation = character(0),
            Type = character(0),
            Columns = character(0),
            Parameters = character(0),
            Description = character(0),
            Executed = logical(0),
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          debug_log(paste("Error getting edit operations table:", e$message), 1)
          return(data.frame(
            Operation = character(0),
            Type = character(0),
            Columns = character(0),
            Parameters = character(0),
            Description = character(0),
            Executed = logical(0),
            stringsAsFactors = FALSE
          ))
        })
      }),

      # Enhanced error management
      get_processing_errors = function() { return(processing_errors()) },
      get_processing_warnings = function() { return(processing_warnings()) },
      get_processing_log = function() { return(processing_log()) },
      clear_processing_errors = function() { clear_processing_errors(processing_errors, processing_warnings, debug_log) },

      # Enhanced performance monitoring
      get_performance_metrics = reactive({
        tryCatch({
          list(
            last_processing_time = last_processing_time(),
            processing_history = processing_history(),
            debug_level = DEBUG_LEVEL,
            error_count = length(processing_errors()),
            warning_count = length(processing_warnings()),
            total_operations = length(processing_log())
          )
        }, error = function(e) {
          debug_log(paste("Error getting performance metrics:", e$message), 1)
          list(
            last_processing_time = NULL,
            processing_history = list(),
            debug_level = DEBUG_LEVEL,
            error_count = 0,
            warning_count = 0,
            total_operations = 0
          )
        })
      }),

      # Enhanced summary functions
      get_processing_summary = reactive({
        tryCatch({
          conditions <- currentConditions()
          config_count <- sum(c(
            !is.null(assign_rules_ui_imputation()),
            !is.null(assign_rules_ui_filtering()),
            !is.null(assign_rules_ui_batch_effects()),
            !is.null(assign_rules_ui_pivot()),
            !is.null(assign_rules_ui_merge()),
            !is.null(assign_rules_ui_edit()),
            !is.null(assign_rules_ui_ratios())
          ))

          if (length(conditions) > 0 || config_count > 0) {
            summary_parts <- character()
            if (length(conditions) > 0) {
              summary_parts <- c(summary_parts, paste("Condition groups:", length(conditions)))
            }
            if (config_count > 0) {
              summary_parts <- c(summary_parts, paste("UI configurations:", config_count))
            }

            error_count <- length(processing_errors())
            if (error_count > 0) {
              summary_parts <- c(summary_parts, paste("Errors:", error_count))
            }

            return(paste(summary_parts, collapse = ", "))
          } else {
            return("No processing configured")
          }
        }, error = function(e) {
          debug_log(paste("Error getting processing summary:", e$message), 1)
          return("Error getting processing summary")
        })
      }),

      # Enhanced validation functions
      validate_processing_inputs = function() {
        validate_processing_inputs(rule_files, metadata_current, condition_inputs, debug_log)
      },

      # Enhanced debug and testing functions
      clear_all_ui_configs = function() {
        tryCatch({
          assign_rules_ui_imputation(NULL)
          assign_rules_ui_filtering(NULL)
          assign_rules_ui_batch_effects(NULL)
          assign_rules_ui_pivot(NULL)
          assign_rules_ui_merge(NULL)
          assign_rules_ui_edit(NULL)
          assign_rules_ui_ratios(NULL)
          ui_config_sources(assign_rules_default_ui_config_sources())
          ui_config_errors(list())
          ui_config_application_status("idle")
          debug_log("All UI configs cleared", 2)
          add_processing_log("config_management", "success", "All UI configs cleared",
                             processing_log = processing_log, processing_errors = processing_errors,
                             processing_warnings = processing_warnings, debug_log = debug_log)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error clearing UI configs:", e$message), 1)
          add_processing_log("config_management", "error", paste("Failed to clear configs:", e$message),
                             processing_log = processing_log, processing_errors = processing_errors,
                             processing_warnings = processing_warnings, debug_log = debug_log)
          return(FALSE)
        })
      }
    ))
  })
}
