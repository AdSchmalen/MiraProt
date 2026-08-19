# ============================================================================
# Module/Sub-script: modules/Data Wizard/assign rules/datawizard_assign_rules_handlers.R
# Purpose:
#   Handler registration layer for assign-rules. This script groups observer
#   registration by intent so event-driven workflows are explicit and easier to
#   maintain without changing assign-rules behavior.
#
# Architectural Role:
#   handlers
#
# Responsibilities:
#   - Register rule-load and rule-file-dropdown synchronization observers.
#   - Register condition add/remove observers.
#   - Register condition input synchronization and option-refresh observers.
#
# Non-Responsibilities:
#   - Own source-of-truth state initialization.
#   - Define UI layout or low-level utility algorithms.
#
# Allowed Dependencies:
#   - Shiny observer APIs and assign-rules state/util callbacks passed in.
#
# Interaction Boundaries:
#   - Inputs: `input`, `session`, reactive state handles, and callbacks from
#             orchestrator.
#   - Outputs: registered observers with side effects on provided state handles.
#   - Side Effects: reactive updates, notifications, and processing log entries.
#
# Stability Guarantees:
#   - Keep event timing and user-facing observer behavior equivalent to previous
#     inline orchestrator implementation.
# ============================================================================

register_assign_rules_rule_loading_handler <- function(
  input,
  session,
  metadata_current,
  aggregated_notifications_enabled,
  reset_rule_load_events,
  rule_loading_status,
  rule_load_pipeline_status,
  selected_rule_file,
  active_rule_file,
  ui_config_application_status,
  ui_config_errors,
  clear_processing_errors,
  processing_errors,
  processing_warnings,
  debug_log,
  last_loaded_rule_data,
  set_imputation_ui_config,
  set_filtering_ui_config,
  set_batch_effects_ui_config,
  set_pivot_ui_config,
  set_merge_ui_config,
  set_edit_ui_config,
  set_ratios_ui_config,
  set_basemean_ui_config,
  add_rule_load_event,
  emit_aggregated_rule_notification,
  last_processing_time,
  assign_rules_ui_imputation,
  assign_rules_ui_filtering,
  assign_rules_ui_batch_effects,
  assign_rules_ui_pivot,
  assign_rules_ui_merge,
  assign_rules_ui_edit,
  assign_rules_ui_ratios,
  assign_rules_ui_basemean,
  ui_config_sources
) {
  emit_rule_loading_error <- function(error_key, error_message, notification_prefix = "") {
    final_message <- if (nzchar(notification_prefix)) {
      paste(notification_prefix, error_message)
    } else {
      error_message
    }

    debug_log(final_message, 1)
    if (aggregated_notifications_enabled) {
      add_rule_load_event("error", error_key, final_message)
      emit_aggregated_rule_notification(base_success = FALSE)
    } else {
      showNotification(final_message, type = "error", duration = 8)
    }

    rule_loading_status("error")
    ui_config_application_status("error")
  }

  set_phase <- function(ok, phase, code = if (ok) "ok" else "failed", message = "", details = list()) {
    rule_load_pipeline_status(assign_rules_load_status(ok, phase, code, message, details))
  }

  emit_non_aggregated_rule_result <- function(config_types_loaded, config_types_failed) {
    if (length(config_types_loaded) > 0) {
      showNotification("✅ Rule set loaded successfully!", type = "message", duration = 4)
      if (length(config_types_failed) > 0) {
        warning_msg <- paste("⚠ Some configurations failed:",
                             paste(config_types_failed, collapse = ", "))
        showNotification(warning_msg, type = "warning", duration = 6)
      }
    } else {
      showNotification("ℹ Rule set loaded, but no UI configurations found.", type = "message", duration = 4)
    }
  }

  observeEvent(input$load_rule_set_dw, {
    if (!is.null(input$load_rule_set_dw) && nzchar(input$load_rule_set_dw)) {

      # Reset Event Collection
      if (aggregated_notifications_enabled) reset_rule_load_events()

      processing_start_time <- Sys.time()
      rule_file_name <- isolate(input$load_rule_set_dw)
      rule_loading_status("loading")
      set_phase(TRUE, "read", "started", paste("Reading", rule_file_name))
      ui_config_application_status("loading")
      ui_config_errors(list())
      clear_processing_errors(processing_errors, processing_warnings, debug_log)

      debug_log(paste("Loading rule file:", rule_file_name), 2)

      metadata_ready <- FALSE
      if (!is.null(metadata_current)) {
        tryCatch({
          current_metadata <- metadata_current()
          metadata_ready <- !is.null(current_metadata) &&
            is.data.frame(current_metadata) &&
            nrow(current_metadata) > 0 &&
            all(c("Content", "Column") %in% names(current_metadata))
        }, error = function(e) {
          debug_log(paste("Error checking metadata availability:", e$message), 2)
          metadata_ready <- FALSE
        })
      }

      if (!metadata_ready) {
        debug_log("Metadata not ready - will load rule file without immediate validation", 2)
      }

      invalidateLater(200, session)

      tryCatch({
        rule_file_path <- file.path("AutoAssign", rule_file_name)
        if (!file.exists(rule_file_path)) {
          err <- paste("Rule file not found:", rule_file_name)
          emit_rule_loading_error("file", err)
          return()
        }

        rule_data <- tryCatch({
          readRDS(rule_file_path)
        }, error = function(e) {
          set_phase(FALSE, "read", "read_failed", e$message)
          err <- paste("Error reading rule file:", e$message)
          emit_rule_loading_error("file", err)
          stop(err)
        })

        if (is.null(rule_data)) {
          err <- "Rule file contains no data"
          emit_rule_loading_error("empty", err)
          return()
        }

        prepared <- tryCatch(prepare_assign_rules_rule_envelope(rule_data), error = function(e) {
          status <- e$assign_rules_status
          if (is.null(status)) status <- assign_rules_load_status(FALSE, "validation", "validation_failed", e$message)
          rule_load_pipeline_status(status)
          stop(e)
        })
        set_phase(TRUE, if (prepared$migrated) "migration" else "validation",
                  if (prepared$migrated) "legacy_migrated" else "validated")
        rule_data <- prepared$rules_data

        config_reactives <- list(
          imputation = assign_rules_ui_imputation, filtering = assign_rules_ui_filtering,
          batch_effects = assign_rules_ui_batch_effects, pivot = assign_rules_ui_pivot,
          merge = assign_rules_ui_merge, edit = assign_rules_ui_edit,
          ratios = assign_rules_ui_ratios, basemean = assign_rules_ui_basemean)
        snapshot <- list(last = isolate(last_loaded_rule_data()), selected = isolate(selected_rule_file()),
          active = isolate(active_rule_file()), sources = isolate(ui_config_sources()),
          ui_errors = isolate(ui_config_errors()), configs = lapply(config_reactives, function(x) isolate(x())))
        rollback <- function() {
          last_loaded_rule_data(snapshot$last); selected_rule_file(snapshot$selected)
          active_rule_file(snapshot$active); ui_config_sources(snapshot$sources)
          ui_config_errors(snapshot$ui_errors)
          for (name in names(config_reactives)) config_reactives[[name]](snapshot$configs[[name]])
          identical(isolate(last_loaded_rule_data()), snapshot$last) &&
            identical(isolate(selected_rule_file()), snapshot$selected) &&
            identical(isolate(active_rule_file()), snapshot$active) &&
            identical(isolate(ui_config_sources()), snapshot$sources) &&
            identical(isolate(ui_config_errors()), snapshot$ui_errors) &&
            all(vapply(names(config_reactives), function(name)
              identical(isolate(config_reactives[[name]]()), snapshot$configs[[name]]), logical(1)))
        }

        config_types_loaded <- character()
        config_types_failed <- character()
        has_optional_ui_config <- rule_data_has_optional_ui_config(rule_data)

        ui_configs <- if (has_optional_ui_config) {
          list(
            list(extract_ui_imputation_config(rule_data, debug_log, ui_config_errors), set_imputation_ui_config, "imputation"),
            list(extract_ui_filtering_config(rule_data, debug_log, ui_config_errors), set_filtering_ui_config, "filtering"),
            list(extract_ui_batch_effects_config(rule_data, debug_log, ui_config_errors), set_batch_effects_ui_config, "batch_effects"),
            list(extract_ui_pivot_config(rule_data, debug_log, ui_config_errors), set_pivot_ui_config, "pivot"),
            list(extract_ui_merge_config(rule_data, debug_log, ui_config_errors), set_merge_ui_config, "merge"),
            list(extract_ui_edit_config(rule_data, debug_log, ui_config_errors), set_edit_ui_config, "edit"),
            list(extract_ui_ratios_config(rule_data, debug_log, ui_config_errors), set_ratios_ui_config, "ratios"),
            list(extract_ui_basemean_config(rule_data, debug_log, ui_config_errors), set_basemean_ui_config, "basemean")
          )
        } else {
          debug_log("Rule file contains no optional UI_config section; loading assignment rules only", 2)
          list()
        }

        # Nothing above this point mutates module state.  Capture every value
        # touched by application so a setter failure cannot leave a mixed rule
        # file/UI configuration in the session.
        set_phase(TRUE, "staging", "staged")

        for (config_info in ui_configs) {
          config <- config_info[[1]]
          setter <- config_info[[2]]
          name <- config_info[[3]]

          if (!is.null(config)) {
            if (name == "filtering" && !metadata_ready) {
              debug_log("Metadata not ready - storing filtering config for later application", 2)
              tryCatch({
                setter(config)
                config_types_loaded <- c(config_types_loaded, paste0(name, "_pending"))
                if (aggregated_notifications_enabled) add_rule_load_event("pending", name)
              }, error = function(e) {
                config_types_failed <- c(config_types_failed, name)
                debug_log(paste("Failed to store", name, "config:", e$message), 1)
                if (aggregated_notifications_enabled) add_rule_load_event("failed", name, e$message)
              })
            } else {
              success <- FALSE
              for (attempt in 1:2) {
                tryCatch({
                  if (setter(config)) {
                    config_types_loaded <- c(config_types_loaded, name)
                    success <- TRUE
                    debug_log(paste("Successfully loaded", name, "config on attempt", attempt), 2)
                    break
                  }
                }, error = function(e) {
                  debug_log(paste("Attempt", attempt, "failed for", name, "config:", e$message),
                            if (attempt < 2) 2 else 1)
                  if (attempt < 2) Sys.sleep(0.1)
                })
              }
              if (!success) {
                stop(paste("UI configuration setter failed:", name), call. = FALSE)
              } else {
                if (aggregated_notifications_enabled) add_rule_load_event("loaded", name)
              }
            }
          } else {
            if (aggregated_notifications_enabled) add_rule_load_event("missing", name)
          }
        }

        last_loaded_rule_data(rule_data)
        selected_rule_file(rule_file_name)
        set_phase(TRUE, "application", "applied")
        expected <- isolate(last_loaded_rule_data())
        if (!identical(expected, rule_data)) stop("Applied rule payload could not be verified.", call. = FALSE)
        set_phase(TRUE, "verification", "verified")

        total_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))
        last_processing_time(total_duration)
        rule_loading_status("loaded")
        active_rule_file(rule_file_name)

        if (length(config_types_loaded) > 0) {
          ui_config_application_status("completed")
          debug_log(paste("UI configs loaded for:", paste(config_types_loaded, collapse = ", ")), 2)
        } else {
          ui_config_application_status("no_config")
          if (has_optional_ui_config) {
            debug_log("No valid UI configurations found in rule file", 2)
          }
        }


        if (aggregated_notifications_enabled) {
          base_success <- !has_optional_ui_config || length(config_types_loaded) > 0
          emit_aggregated_rule_notification(base_success = base_success)
        } else {
          emit_non_aggregated_rule_result(config_types_loaded, config_types_failed)
        }

      }, error = function(e) {
        if (exists("rollback", inherits = FALSE)) {
          restored <- tryCatch(rollback(), error = function(rollback_error) rollback_error)
          if (!isTRUE(restored)) {
            message <- if (inherits(restored, "error")) restored$message else "Restored values did not match the snapshot."
            set_phase(FALSE, "rollback", "rollback_verification_failed", message)
          } else if (isTRUE(rule_load_pipeline_status()$ok)) {
            set_phase(FALSE, "application", "application_failed_rolled_back", e$message)
          }
        } else if (isTRUE(rule_load_pipeline_status()$ok)) {
          set_phase(FALSE, "validation", "validation_failed", e$message)
        }
        error_msg <- paste("Error loading rule file:", e$message)
        emit_rule_loading_error("loader", error_msg, notification_prefix = "X")
      })

    } else {

      debug_log("Rule file dropdown selection cleared; preserving loaded rule data and active rule state", 2)
      selected_rule_file("")
    }
  })
}

register_assign_rules_condition_management_handlers <- function(
  input,
  counter_condition,
  condition_inputs,
  Options_condition,
  debug_log,
  processing_log,
  processing_errors,
  processing_warnings
) {
  normalize_condition_inputs <- function(current_inputs, condition_count) {
    if (!is.list(current_inputs)) current_inputs <- list()

    for (condition_index in seq_len(max(condition_count, 1))) {
      condition_id <- paste0("Condition_", condition_index)
      condition_value <- current_inputs[[condition_id]]
      condition_value <- if (length(condition_value) > 0 && !is.na(condition_value[[1]])) {
        as.character(condition_value[[1]])
      } else {
        ""
      }

      if (!nzchar(trimws(condition_value))) {
        current_inputs[[condition_id]] <- paste("Condition", condition_index)
      }
    }

    current_inputs
  }

  build_condition_options <- function(current_inputs) {
    condition_values <- unname(unlist(current_inputs, use.names = FALSE))
    condition_values <- condition_values[!is.na(condition_values) & nzchar(trimws(condition_values))]
    c(NA_character_, condition_values)
  }

  rebuild_condition_options <- function(current_inputs) {
    Options_condition(build_condition_options(current_inputs))
  }

  observeEvent(input$add_btn, {
    tryCatch({
      new_count <- counter_condition() + 1
      counter_condition(new_count)

      current_inputs <- normalize_condition_inputs(condition_inputs(), new_count)
      new_id <- paste0("Condition_", new_count)
      current_inputs[[new_id]] <- paste("Condition", new_count)
      condition_inputs(current_inputs)
      rebuild_condition_options(current_inputs)

      debug_log(paste("Added condition group", new_count), 2)
      add_processing_log("condition_management", "success", paste("Added condition group", new_count),
                         processing_log = processing_log, processing_errors = processing_errors,
                         processing_warnings = processing_warnings, debug_log = debug_log)

      showNotification(paste("Added Condition Group", new_count), type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error adding condition:", e$message), 1)
      add_processing_log("condition_management", "error", paste("Failed to add condition:", e$message),
                         processing_log = processing_log, processing_errors = processing_errors,
                         processing_warnings = processing_warnings, debug_log = debug_log)
      showNotification("Failed to add condition group", type = "error", duration = 5)
    })
  })

  reindex_condition_inputs <- function(current_inputs, fallback_to_default = TRUE) {
    if (!is.list(current_inputs)) current_inputs <- list()

    condition_values <- unname(unlist(current_inputs, use.names = FALSE))
    condition_values <- as.character(condition_values)
    condition_values <- condition_values[!is.na(condition_values) & nzchar(trimws(condition_values))]

    if (length(condition_values) == 0 && isTRUE(fallback_to_default)) {
      condition_values <- "Condition 1"
    }

    stats::setNames(as.list(condition_values), paste0("Condition_", seq_along(condition_values)))
  }

  remove_condition_at <- function(remove_index) {
    tryCatch({
      current_count <- max(counter_condition(), 1)
      if (is.na(remove_index) || remove_index < 1 || remove_index > current_count) {
        return(NULL)
      }

      current_inputs <- normalize_condition_inputs(condition_inputs(), current_count)
      condition_ids <- paste0("Condition_", seq_len(current_count))
      current_inputs <- current_inputs[condition_ids]
      current_inputs[[paste0("Condition_", remove_index)]] <- NULL
      current_inputs <- reindex_condition_inputs(current_inputs)

      condition_inputs(current_inputs)
      counter_condition(length(current_inputs))
      rebuild_condition_options(current_inputs)

      debug_log(paste("Removed condition group", remove_index), 2)
      add_processing_log("condition_management", "success", paste("Removed condition group", remove_index),
                         processing_log = processing_log, processing_errors = processing_errors,
                         processing_warnings = processing_warnings, debug_log = debug_log)

      showNotification(paste("Removed Condition Group", remove_index), type = "message", duration = 3)
    }, error = function(e) {
      debug_log(paste("Error removing condition:", e$message), 1)
      add_processing_log("condition_management", "error", paste("Failed to remove condition:", e$message),
                         processing_log = processing_log, processing_errors = processing_errors,
                         processing_warnings = processing_warnings, debug_log = debug_log)
      showNotification("Failed to remove condition group", type = "error", duration = 5)
    })
  }

  registered_remove_observers <- new.env(parent = emptyenv())

  observe({
    condition_count <- max(counter_condition(), 1)

    for (condition_index in seq_len(condition_count)) {
      remove_id <- paste0("remove_condition_", condition_index)
      if (!isTRUE(registered_remove_observers[[remove_id]])) {
        local({
          local_index <- condition_index
          local_id <- remove_id
          observeEvent(input[[local_id]], {
            remove_condition_at(local_index)
          }, ignoreInit = TRUE)
        })
        registered_remove_observers[[remove_id]] <- TRUE
      }
    }
  })
}


register_assign_rules_rule_files_dropdown_handler <- function(
  rule_files,
  input,
  session,
  debug_log,
  add_processing_log,
  processing_log,
  processing_errors,
  processing_warnings
) {
  observeEvent(rule_files(), {
    if (!is.null(input$load_rule_set_dw)) {
      tryCatch({
        available_files <- character()

        if (!is.null(rule_files)) {
          tryCatch({
            available_files <- rule_files()
            debug_log(paste("Retrieved", length(available_files), "rule files from reactive"), 2)
          }, error = function(e) {
            debug_log(paste("Error accessing rule files reactive:", e$message), 1)
            available_files <- character()
          })
        }

        if (length(available_files) == 0) {
          autoassign_dir <- file.path(".", "AutoAssign")
          if (dir.exists(autoassign_dir)) {
            tryCatch({
              available_files <- list.files(autoassign_dir, pattern = "\\.rds$", full.names = FALSE)
              debug_log(paste("Found", length(available_files), "rule files in AutoAssign directory"), 2)
            }, error = function(e) {
              debug_log(paste("Error scanning AutoAssign directory:", e$message), 1)
            })
          } else {
            debug_log("AutoAssign directory does not exist", 2)
          }
        }

        if (length(available_files) > 0) {
          choices_with_default <- c("Select a rule set..." = "", setNames(available_files, available_files))
          tryCatch({
            updateSelectInput(session, "load_rule_set_dw",
                              choices = choices_with_default,
                              selected = isolate(input$load_rule_set_dw))
            debug_log("Rule file dropdown updated successfully", 2)
          }, error = function(e) {
            debug_log(paste("Error updating rule file dropdown:", e$message), 1)
          })
        } else {
          tryCatch({
            updateSelectInput(session, "load_rule_set_dw",
                              choices = c("No rule files found" = ""),
                              selected = "")
            debug_log("No rule files found - updated dropdown with message", 2)
          }, error = function(e) {
            debug_log(paste("Error updating dropdown with no files message:", e$message), 1)
          })
        }

      }, error = function(e) {
        debug_log(paste("Critical error in rule files observe:", e$message), 1)
        add_processing_log("rule_files_update", "error", e$message,
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
        tryCatch({
          updateSelectInput(session, "load_rule_set_dw",
                            choices = c("Error loading rule files" = ""),
                            selected = "")
        }, error = function(e2) {
          debug_log(paste("Failed to update dropdown even with error message:", e2$message), 1)
        })
      })
    }
  })
}

register_assign_rules_condition_sync_handlers <- function(
  input,
  counter_condition,
  condition_inputs,
  Options_condition,
  debug_log,
  processing_log,
  processing_errors,
  processing_warnings
) {
  rebuild_condition_options <- function(current_inputs) {
    current_conditions <- unname(unlist(current_inputs, use.names = FALSE))
    current_conditions <- current_conditions[!is.na(current_conditions) & nzchar(trimws(current_conditions))]
    Options_condition(c(NA_character_, current_conditions))
  }

  observeEvent(input$condition_text_blur, {
    tryCatch({
      blur_payload <- input$condition_text_blur
      input_id <- blur_payload$id %||% ""
      condition_index <- suppressWarnings(as.integer(sub("^.*textin([0-9]+)$", "\\1", input_id)))

      if (is.na(condition_index) || condition_index < 1 || condition_index > max(counter_condition(), 1)) {
        return(NULL)
      }

      condition_id <- paste0("Condition_", condition_index)
      new_value <- blur_payload$value %||% paste("Condition", condition_index)
      current_inputs <- condition_inputs()
      if (!is.list(current_inputs)) current_inputs <- list()

      if (!identical(current_inputs[[condition_id]], new_value)) {
        current_inputs[[condition_id]] <- new_value
        condition_inputs(current_inputs)
        rebuild_condition_options(current_inputs)

        add_processing_log("condition_sync", "info", paste("Synchronized", condition_id, "on blur"),
                           processing_log = processing_log, processing_errors = processing_errors,
                           processing_warnings = processing_warnings, debug_log = debug_log)
      }
    }, error = function(e) {
      debug_log(paste("Error syncing blurred condition input:", e$message), 1)
      add_processing_log("condition_sync", "error", paste("Failed to sync blurred input:", e$message),
                         processing_log = processing_log, processing_errors = processing_errors,
                         processing_warnings = processing_warnings, debug_log = debug_log)
    })
  })
}
