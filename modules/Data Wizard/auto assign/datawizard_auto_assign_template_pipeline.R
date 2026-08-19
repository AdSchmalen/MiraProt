# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Template Pipeline
# Purpose:
#   Encapsulate template import/loading pipeline behavior used by Auto-Assign.
# Architectural Role:
#   Template processing layer for rule/template/UI application flow.
# Responsibilities:
#   - Provide stable template loading pipeline entrypoints for orchestrator.
#   - Preserve existing loading order, logging, and timing semantics.
# Non-Responsibilities:
#   - Must not own module lifecycle entrypoints or observer registration.
# ============================================================================

create_auto_assign_template_pipeline <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("create_auto_assign_template_pipeline requires an environment context")
  }

  evalq({
    load_rules_directly <- function(rules_data, notify = TRUE) {
      notify <- isTRUE(notify)
      `%||%` <- function(x, y) if (is.null(x)) y else x
      notify_user <- function(...) {
        if (notify) showNotification(...)
      }
      fail_validation <- function(message) {
        debug_log(message, 1)
        add_processing_log("rule_loading", "error", message)
        notify_user(message, type = "error", duration = 6)
        FALSE
      }

      if (!is.list(rules_data)) {
        return(fail_validation("Invalid rules data provided - not a list"))
      }

      canonical_names <- c("table", "condition", "ratio")
      missing_components <- setdiff(canonical_names, names(rules_data))
      if (length(missing_components)) {
        return(fail_validation(paste(
          "Invalid rules data - missing canonical components:",
          paste(missing_components, collapse = ", ")
        )))
      }
      invalid_components <- canonical_names[!vapply(
        rules_data[canonical_names], is.data.frame, logical(1)
      )]
      if (length(invalid_components)) {
        return(fail_validation(paste(
          "Invalid rules data - canonical components must be data frames:",
          paste(invalid_components, collapse = ", ")
        )))
      }

      # Upgrade historical templates before validation; generated identities are
      # persisted immediately and are independent of regex text.
      upgraded <- tryCatch(list(
        table=upgrade_rule_component(rules_data$table, "content"),
        condition=upgrade_rule_component(rules_data$condition, "condition"),
        ratio=upgrade_rule_component(rules_data$ratio, "ratio")), error=identity)
      if (inherits(upgraded, "error")) return(fail_validation(upgraded$message))
      rules_data[names(upgraded)] <- upgraded

      # Legacy schema prefix upgraded here: c("Content", "VariantId", "Priority"
      required_fields <- list(
        table = c("RuleId", "Content", "VariantId", "Priority", "Include", "Exclude", "Transformation"),
        condition = c("RuleId", "Content", "VariantId", "Method", "Before", "After", "Separators", "Pos"),
        ratio = c("RuleId", "Content", "VariantId", "Method", "Separators", "Invert", "NumBefore",
                  "NumAfter", "DenBefore", "DenAfter", "NumPos", "DenPos")
      )
      invalid_schemas <- canonical_names[!vapply(canonical_names, function(component) {
        identical(names(rules_data[[component]]), required_fields[[component]])
      }, logical(1))]
      if (length(invalid_schemas)) {
        return(fail_validation(paste(
          "Invalid rules data - canonical component fields are invalid:",
          paste(invalid_schemas, collapse = ", ")
        )))
      }

      # Rule state is transactional.  Keep the original objects (including
      # column classes and attributes) so rollback is exact rather than a
      # reconstruction of the previous rules.
      rule_snapshot <- list(
        table = isolate(rv_table_rules_autoassign_dw()),
        condition = isolate(rv_condition_rules_autoassign_dw()),
        ratio = isolate(rv_rules_autoassign_dw()),
        loaded = isolate(rules_loaded_centrally()),
        current = isolate(current_loaded_rules()),
        ui_config = if (exists("current_ui_config")) isolate(current_ui_config()) else NULL,
        import_info = if (exists("last_import_info")) isolate(last_import_info()) else NULL,
        envelope = if (exists("rule_envelope")) isolate(rule_envelope()) else NULL,
        provenance = if (exists("provenance_mappings")) isolate(provenance_mappings()) else list(),
        contrasts = if (exists("contrast_mappings")) isolate(contrast_mappings()) else list(),
        selected = list(
          content = if (exists("selected_content_rule")) isolate(selected_content_rule()) else NULL,
          condition = if (exists("selected_condition_rule")) isolate(selected_condition_rule()) else NULL,
          ratio = if (exists("selected_ratio_rule")) isolate(selected_ratio_rule()) else NULL),
        priorities = if (exists("rule_priorities")) isolate(rule_priorities()) else integer(),
        capabilities = if (exists("required_capabilities")) isolate(required_capabilities()) else character()
      )
      restore_rule_snapshot <- function() {
        rv_table_rules_autoassign_dw(rule_snapshot$table)
        rv_condition_rules_autoassign_dw(rule_snapshot$condition)
        rv_rules_autoassign_dw(rule_snapshot$ratio)
        rules_loaded_centrally(rule_snapshot$loaded)
        current_loaded_rules(rule_snapshot$current)
        if (exists("current_ui_config")) current_ui_config(rule_snapshot$ui_config)
        if (exists("last_import_info")) last_import_info(rule_snapshot$import_info)
        if (exists("rule_envelope")) rule_envelope(rule_snapshot$envelope)
        if (exists("provenance_mappings")) provenance_mappings(rule_snapshot$provenance)
        if (exists("contrast_mappings")) contrast_mappings(rule_snapshot$contrasts)
        if (exists("selected_content_rule")) selected_content_rule(rule_snapshot$selected$content)
        if (exists("selected_condition_rule")) selected_condition_rule(rule_snapshot$selected$condition)
        if (exists("selected_ratio_rule")) selected_ratio_rule(rule_snapshot$selected$ratio)
        if (exists("rule_priorities")) rule_priorities(rule_snapshot$priorities)
        if (exists("required_capabilities")) required_capabilities(rule_snapshot$capabilities)
        restored <- identical(isolate(rv_table_rules_autoassign_dw()), rule_snapshot$table) &&
          identical(isolate(rv_condition_rules_autoassign_dw()), rule_snapshot$condition) &&
          identical(isolate(rv_rules_autoassign_dw()), rule_snapshot$ratio) &&
          identical(isolate(rules_loaded_centrally()), rule_snapshot$loaded) &&
          identical(isolate(current_loaded_rules()), rule_snapshot$current) &&
          (!exists("current_ui_config") || identical(isolate(current_ui_config()), rule_snapshot$ui_config)) &&
          (!exists("last_import_info") || identical(isolate(last_import_info()), rule_snapshot$import_info)) &&
          (!exists("rule_envelope") || identical(isolate(rule_envelope()), rule_snapshot$envelope)) &&
          (!exists("provenance_mappings") || identical(isolate(provenance_mappings()), rule_snapshot$provenance)) &&
          (!exists("contrast_mappings") || identical(isolate(contrast_mappings()), rule_snapshot$contrasts)) &&
          (!exists("selected_content_rule") || identical(isolate(selected_content_rule()), rule_snapshot$selected$content)) &&
          (!exists("selected_condition_rule") || identical(isolate(selected_condition_rule()), rule_snapshot$selected$condition)) &&
          (!exists("selected_ratio_rule") || identical(isolate(selected_ratio_rule()), rule_snapshot$selected$ratio)) &&
          (!exists("rule_priorities") || identical(isolate(rule_priorities()), rule_snapshot$priorities)) &&
          (!exists("required_capabilities") || identical(isolate(required_capabilities()), rule_snapshot$capabilities))
        if (!isTRUE(restored)) stop("Auto-Assign rollback verification failed.", call. = FALSE)
        invisible(TRUE)
      }

      processing_start_time <- Sys.time()

      tryCatch({
        success_count <- 0
        total_operations <- 0

        debug_log("Starting input validation for rule loading", 2)

        # Replace every canonical store, including valid zero-row frames.  This
        # prevents omitted/stale rules from surviving a successful transfer.
        rv_table_rules_autoassign_dw(rules_data$table)
        rv_condition_rules_autoassign_dw(rules_data$condition)
        rv_rules_autoassign_dw(rules_data$ratio)
        envelope <- list(schema_version=2L, rules=list(table=rules_data$table,
          condition=rules_data$condition, ratio=rules_data$ratio),
          provenance=rules_data$provenance %||% list(),
          contrasts=rules_data$contrast_mappings %||% list(),
          priorities=stats::setNames(rules_data$table$Priority, rules_data$table$RuleId),
          required_capabilities=unique(as.character(rules_data$required_capabilities %||% character())))
        if (exists("rule_envelope")) rule_envelope(envelope)
        if (exists("provenance_mappings")) provenance_mappings(envelope$provenance)
        if (exists("contrast_mappings")) contrast_mappings(envelope$contrasts)
        if (exists("rule_priorities")) rule_priorities(envelope$priorities)
        if (exists("required_capabilities")) required_capabilities(envelope$required_capabilities)
        selectable_table <- rules_data$table[rules_data$table$Content != "Row Index", , drop = FALSE]
        initial_content_id <- if (nrow(selectable_table))
          resolve_content_rule_id(selectable_table, selectable_table$Content[[1L]]) else
          if (nrow(rules_data$table)) rules_data$table$RuleId[[1L]] else NULL
        if (exists("selected_content_rule")) selected_content_rule(initial_content_id)
        if (exists("selected_condition_rule")) selected_condition_rule(if(nrow(rules_data$condition)) rules_data$condition$RuleId[[1L]] else NULL)
        if (exists("selected_ratio_rule")) selected_ratio_rule(if(nrow(rules_data$ratio)) rules_data$ratio$RuleId[[1L]] else NULL)
        success_count <- sum(vapply(rules_data[canonical_names], nrow, integer(1)) > 0L)
        total_operations <- sum(vapply(rules_data[canonical_names], nrow, integer(1)))

        update_if_present <- function(row, field, update, input_id, argument,
                                      convert = identity) {
          value <- row[[field]]
          if (!is.na(value) && (is.numeric(value) || is.logical(value) || nzchar(value))) {
            args <- list(session = session, inputId = input_id)
            args[[argument]] <- convert(value)
            do.call(update, args)
          }
        }

        # Populate the existing editors from the first rule of each non-empty
        # component.  Unlike the former best-effort updates, an update error is
        # a load failure and therefore rolls the canonical stores back.
        if (nrow(rules_data$table) > 0L) {
          row <- rules_data$table[match(initial_content_id, rules_data$table$RuleId), , drop = FALSE]
          update_if_present(row, "Content", updateSelectInput, "lookup_content_dw", "selected")
          update_if_present(row, "Include", updateTextInput, "string_include_autoassign_dw", "value", regex_to_plain_dw)
          update_if_present(row, "Exclude", updateTextInput, "string_exclude_autoassign_dw", "value", regex_to_plain_dw)
          update_if_present(row, "Transformation", updateSelectInput, "transformation_col_dw", "selected")
        }
        if (nrow(rules_data$condition) > 0L) {
          row <- rules_data$condition[1L, , drop = FALSE]
          update_if_present(row, "Content", updateSelectInput, "cond_content_autoassign_dw", "selected")
          update_if_present(row, "Method", updateSelectInput, "cond_method_autoassign_dw", "selected")
          update_if_present(row, "Before", updateTextInput, "cond_before_autoassign_dw", "value", regex_to_plain_dw)
          update_if_present(row, "After", updateTextInput, "cond_after_autoassign_dw", "value", regex_to_plain_dw)
          update_if_present(row, "Separators", updateCheckboxGroupInput, "cond_sep_chars_autoassign_dw", "selected", function(x) strsplit(x, "\\|")[[1L]])
          update_if_present(row, "Pos", updateNumericInput, "cond_pos_autoassign_dw", "value")
        }
        if (nrow(rules_data$ratio) > 0L) {
          row <- rules_data$ratio[1L, , drop = FALSE]
          update_if_present(row, "Content", updateSelectInput, "new_content_autoassign_dw", "selected")
          update_if_present(row, "Method", updateSelectInput, "new_method_autoassign_dw", "selected")
          update_if_present(row, "Separators", updateCheckboxGroupInput, "new_sep_chars_autoassign_dw", "selected", function(x) strsplit(x, "\\|")[[1L]])
          update_if_present(row, "Invert", updateCheckboxInput, "new_invert_autoassign_dw", "value", as.logical)
          for (spec in list(
            c("NumBefore", "new_num_before_autoassign_dw"), c("NumAfter", "new_num_after_autoassign_dw"),
            c("DenBefore", "new_den_before_autoassign_dw"), c("DenAfter", "new_den_after_autoassign_dw")
          )) update_if_present(row, spec[[1L]], updateTextInput, spec[[2L]], "value", regex_to_plain_dw)
          update_if_present(row, "NumPos", updateNumericInput, "new_num_pos_autoassign_dw", "value")
          update_if_present(row, "DenPos", updateNumericInput, "new_den_pos_autoassign_dw", "value")
        }

        debug_log(paste("Basic rules loading completed:", success_count, "rule types"), 2)

        # Template component loading with comprehensive error handling
        template_operations <- 0

        # Filter template with enhanced validation
        if (!is.null(rules_data$filter_template)) {
          debug_log("Processing filter template", 2)
          if (is.list(rules_data$filter_template) && length(rules_data$filter_template) > 0) {
            tryCatch({
              debug_log("Attempting to apply filter template", 2)
              filter_result <- apply_filter_template(rules_data$filter_template, filter_module)
              if (safe_is_true(filter_result)) {
                template_operations <- template_operations + 1
                debug_log("Filter template applied successfully", 2)
              } else {
                stop("Filter template application returned false/null", call. = FALSE)
              }
            }, error = function(e) {
              debug_log(paste("Error applying filter template:", e$message), 1)
              add_processing_log("filter_template", "error", e$message)
              stop(e)
            })
          } else {
            debug_log("Filter template data is invalid or empty", 2)
          }
        }

        # Edit operations with enhanced validation
        if (!is.null(rules_data$edit_operations)) {
          debug_log("Processing edit operations", 2)
          if (is.data.frame(rules_data$edit_operations) && nrow(rules_data$edit_operations) > 0) {
            tryCatch({
              debug_log(paste("Attempting to apply", nrow(rules_data$edit_operations), "edit operations"), 2)
              edit_result <- apply_edit_operations(rules_data$edit_operations)
              if (safe_is_true(edit_result)) {
                template_operations <- template_operations + 1
                debug_log("Edit operations applied successfully", 2)
              } else {
                stop("Edit operations application returned false/null", call. = FALSE)
              }
            }, error = function(e) {
              debug_log(paste("Error applying edit operations:", e$message), 1)
              add_processing_log("edit_operations", "error", e$message)
              stop(e)
            })
          } else {
            debug_log("Edit operations data is invalid or empty", 2)
          }
        }

        # Ratio configurations with enhanced validation
        if (!is.null(rules_data$ratio_configurations)) {
          debug_log("Processing ratio configurations", 2)
          if (is.data.frame(rules_data$ratio_configurations) && nrow(rules_data$ratio_configurations) > 0) {
            tryCatch({
              debug_log(paste("Attempting to apply", nrow(rules_data$ratio_configurations), "ratio configurations"), 2)
              ratio_result <- apply_ratio_configurations(rules_data$ratio_configurations)
              if (safe_is_true(ratio_result)) {
                template_operations <- template_operations + 1
                debug_log("Ratio configurations applied successfully", 2)
              } else {
                stop("Ratio configurations application returned false/null", call. = FALSE)
              }
            }, error = function(e) {
              debug_log(paste("Error applying ratio configurations:", e$message), 1)
              add_processing_log("ratio_configurations", "error", e$message)
              stop(e)
            })
          } else {
            debug_log("Ratio configurations data is invalid or empty", 2)
          }
        }

        debug_log(paste("Template components processed:", template_operations, "successful"), 2)

        # UI configuration loading with comprehensive validation
        ui_operations <- 0

        if (!is.null(rules_data$UI_config)) {
          debug_log("Processing UI configurations", 2)

          if (!is.null(rules_data$UI_config$UI_imputation)) {
            if (is.list(rules_data$UI_config$UI_imputation) && length(rules_data$UI_config$UI_imputation) > 0) {
              tryCatch({
                debug_log("Attempting to apply imputation UI config", 2)
                imputation_result <- apply_imputation_ui_config(rules_data$UI_config$UI_imputation)
                if (safe_is_true(imputation_result)) {
                  ui_operations <- ui_operations + 1
                  debug_log("Imputation UI config applied successfully", 2)
                } else {
                  stop("Imputation UI config application returned false/null", call. = FALSE)
                }
              }, error = function(e) {
                debug_log(paste("Error applying imputation UI config:", e$message), 1)
                add_processing_log("imputation_ui_config", "error", e$message)
                stop(e)
              })
            } else {
              debug_log("Imputation UI config data is invalid or empty", 2)
            }
          }

          if (!is.null(rules_data$UI_config$filtering)) {
            if (is.list(rules_data$UI_config$filtering) && length(rules_data$UI_config$filtering) > 0) {
              tryCatch({
                debug_log("Attempting to apply filtering UI config", 2)
                filtering_result <- apply_filter_template(rules_data$UI_config$filtering, filter_module)
                if (safe_is_true(filtering_result)) {
                  ui_operations <- ui_operations + 1
                  debug_log("Filtering UI config applied successfully", 2)
                } else {
                  stop("Filtering UI config application returned false/null", call. = FALSE)
                }
              }, error = function(e) {
                debug_log(paste("Error applying filtering UI config:", e$message), 1)
                add_processing_log("filtering_ui_config", "error", e$message)
                stop(e)
              })
            } else {
              debug_log("Filtering UI config data is invalid or empty", 2)
            }
          }
        }

        debug_log(paste("UI configurations processed:", ui_operations, "successful"), 2)

        # CORRECTED: UI state application with proper validation
        if (!is.null(rules_data$ui)) {
          debug_log("Processing UI state data", 2)

          if (is.list(rules_data$ui) && length(rules_data$ui) > 0) {
            tryCatch({
              ui_updates <- 0
              debug_log("Starting UI state application", 2)

              # Enhanced validation and application for each UI element
              ui_elements <- list(
                list("lookup_content_dw", rules_data$ui$lookup_content, updateSelectInput, "selected"),
                list("string_include_autoassign_dw", rules_data$ui$string_include, updateTextInput, "value"),
                list("string_exclude_autoassign_dw", rules_data$ui$string_exclude, updateTextInput, "value"),
                list("transformation_col_dw", rules_data$ui$transformation_col, updateSelectInput, "selected")
              )

              for (element in ui_elements) {
                element_id <- element[[1]]
                element_value <- element[[2]]
                element_function <- element[[3]]
                element_param <- element[[4]]

                tryCatch({
                  if (!is.null(element_value) && length(element_value) > 0 &&
                      !is.na(element_value) && nzchar(as.character(element_value))) {

                    # Create parameter list dynamically
                    params <- list(session = session, inputId = element_id)
                    params[[element_param]] <- element_value

                    # Call the update function
                    do.call(element_function, params)

                    ui_updates <- ui_updates + 1
                    debug_log(paste("Successfully updated", element_id, "with value:", element_value), 2)
                  } else {
                    debug_log(paste("Skipping", element_id, "- invalid value"), 2)
                  }
                }, error = function(e) {
                  debug_log(paste("Error updating", element_id, ":", e$message), 1)
                  stop(e)
                })
              }

              if (ui_updates > 0) {
                ui_operations <- ui_operations + 1
                debug_log(paste("Applied", ui_updates, "UI state updates"), 2)
                add_processing_log("ui_state_application", "success", paste("Applied", ui_updates, "updates"))
              } else {
                debug_log("No UI state updates could be applied", 2)
                add_processing_log("ui_state_application", "warning", "No updates applied")
              }

            }, error = function(e) {
              debug_log(paste("Error in UI state application:", e$message), 1)
              add_processing_log("ui_state_application", "error", e$message)
              stop(e)
            })
          } else {
            debug_log("UI state data is invalid or empty", 2)
          }
        }

        # Update status tracking
        rules_loaded_centrally(TRUE)
        current_loaded_rules(rules_data)

        # Calculate processing time
        processing_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))
        last_processing_time(processing_duration)

        # Enhanced success logging
        add_processing_log("rule_loading", "success",
                           paste("Loaded", success_count, "rule types,", template_operations, "templates,", ui_operations, "UI configs"),
                           processing_duration)

        debug_log(paste("Template loaded successfully:", success_count, "rule types,", template_operations, "templates,", ui_operations, "UI configs",
                        sprintf("(%.2fs)", processing_duration)), 1)

        # IMPORTANT: Force refresh of the tables.
        # `invalidateLater()` requires an active reactive consumer. Session
        # restore may call load_rules_directly() from non-reactive code paths
        # (e.g. set_session_state), where invalidateLater would throw and
        # abort restore. Keep this best-effort and never fail rule loading.
        if (success_count > 0) {
          # Small delay to ensure reactive updates propagate.
          tryCatch({
            invalidateLater(100, session)
          }, error = function(e) {
            debug_log(paste(
              "Skipping invalidateLater in load_rules_directly (non-reactive context):",
              e$message
            ), 2)
          })
          notify_user(paste("Rules loaded successfully:", success_count, "rule types"), type = "message", duration = 4)
        }

        return(TRUE)

      }, error = function(e) {
        restore_error <- tryCatch({
          restore_rule_snapshot()
          NULL
        }, error = identity)
        processing_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))
        debug_log(paste("CRITICAL ERROR in load_rules_directly:", e$message), 1)
        debug_log(paste("Error class:", class(e)), 2)
        debug_log(paste("Error call:", deparse(e$call)), 2)
        add_processing_log("rule_loading", "error", e$message, processing_duration)
        if (inherits(restore_error, "error")) {
          add_processing_log("rule_loading_rollback", "error", restore_error$message, processing_duration)
          debug_log(paste("CRITICAL ERROR restoring rule snapshot:", restore_error$message), 1)
        } else {
          add_processing_log("rule_loading_rollback", "success", "Prior rule state restored and verified", processing_duration)
        }
        module_health_status("Error")
        notify_user(paste("Error loading rules:", e$message), type = "error", duration = 6)
        return(FALSE)
      })
    }



    list(
      load_rules_directly = load_rules_directly
    )
  }, envir = ctx)
}
