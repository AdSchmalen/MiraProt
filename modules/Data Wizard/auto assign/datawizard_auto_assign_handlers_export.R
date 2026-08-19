# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Export Handlers
# Purpose: Register export observers against the shared Auto-Assign context.
# ============================================================================

register_auto_assign_export_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_export_handlers requires an environment context")
  }

  evalq({
    # ========================================
    # Enhanced Export Rules with Comprehensive Error Handling
    # ========================================

    output$export_rules_autoassign_dw <- downloadHandler(
      filename = function() {
        paste0("complete_template_", Sys.Date(), ".rds")
      },
      content = function(file) {
        export_start_time <- Sys.time()
        template_export_status("exporting")

        tryCatch({
          # Base rule set
          rules_list <- list(
            table     = rv_table_rules_autoassign_dw(),
            condition = rv_condition_rules_autoassign_dw(),
            ratio     = rv_rules_autoassign_dw()
          )

          components_exported <- c("assignment_rules")

          # UI State if enabled
          if (safe_is_true(input$save_ui_autoassign_dw)) {
            rules_list$ui <- list(
              lookup_content       = input$lookup_content_dw,
              string_include       = input$string_include_autoassign_dw,
              string_exclude       = input$string_exclude_autoassign_dw,
              transformation_col   = input$transformation_col_dw
            )
            components_exported <- c(components_exported, "ui_state")
            debug_log("UI state included in export", 2)

            # Filter template
            if (safe_is_true(input$include_filtering_config)) {
              filter_template <- collect_filter_ui_state(filter_module)
              if (!is.null(filter_template)) {
                rules_list$filter_template <- filter_template
                components_exported <- c(components_exported, "filter_template")
                debug_log("Filter template included in export", 2)
              }
            }

            # Ratio configurations
            if (safe_is_true(input$include_ratio_configurations)) {
              ratio_configurations <- collect_ratio_configurations(ratios_module)
              if (!is.null(ratio_configurations) && nrow(ratio_configurations) > 0) {
                rules_list$ratio_configurations <- ratio_configurations
                components_exported <- c(components_exported, "ratio_configurations")
                debug_log(paste("Ratio configurations included in export:", nrow(ratio_configurations)), 2)
              }
            }

            # Basemean configurations
            if (safe_is_true(input$include_basemean_ui_config)) {
              debug_log("Collecting Basemean configuration...", 2)

              tryCatch({
                basemean_mod <- if (is.function(basemean_module)) basemean_module() else basemean_module

                basemean_configurations <- collect_basemean_configurations(basemean_mod)

                if (!is.null(basemean_configurations)) {
                  rules_list$basemean_configurations <- basemean_configurations
                  components_exported <- c(components_exported, "basemean_configurations")
                  debug_log("Basemean configuration included in export", 2)
                } else {
                  debug_log("No Basemean configuration found to export", 2)
                }
              }, error = function(e) {
                debug_log(paste("Error collecting Basemean configuration:", e$message), 1)
              })
            }

            # # Ratio configurations
            # if (safe_is_true(input$include_ratio_configurations)) {
            #   ratio_configurations <- collect_ratio_configurations(ratios_module)
            #   if (!is.null(ratio_configurations) && nrow(ratio_configurations) > 0) {
            #     rules_list$ratio_configurations <- ratio_configurations
            #     components_exported <- c(components_exported, "ratio_configurations")
            #     debug_log(paste("Ratio configurations included in export:", nrow(ratio_configurations)), 2)
            #   }
            # }

            # Edit operations
            if (safe_is_true(input$include_edit_operations)) {
              edit_operations <- collect_edit_operations()
              if (!is.null(edit_operations) && nrow(edit_operations) > 0) {
                rules_list$edit_operations <- edit_operations
                components_exported <- c(components_exported, "edit_operations")
                debug_log(paste("Edit operations included in export:", nrow(edit_operations)), 2)
              }
            }

          }

          # UI_config Structure for ALL modules
          if (safe_is_true(input$save_ui_autoassign_dw)) {
            if (is.null(rules_list$UI_config)) {
              rules_list$UI_config <- list()
            }

            # Collect all UI configurations with error handling
            ui_configs <- list(
              list(safe_is_true(input$include_filtering_config), "filtering", collect_filter_ui_state(filter_module)),
              list(safe_is_true(input$include_imputation_config), "UI_imputation", collect_imputation_ui_config()),
              list(safe_is_true(input$include_batch_effects_config), "batch_effects", collect_batch_effects_ui_state(batch_module)),
              list(safe_is_true(input$include_pivot_config), "pivot", collect_pivot_ui_state(pivot_module)),
              list(safe_is_true(input$include_merge_config), "merge", collect_merge_ui_state(merge_module))
            )

            for (config_info in ui_configs) {
              include_flag <- config_info[[1]]
              config_name <- config_info[[2]]
              config_data <- config_info[[3]]

              if (include_flag && !is.null(config_data)) {
                rules_list$UI_config[[config_name]] <- config_data
                components_exported <- c(components_exported, paste0(config_name, "_ui_config"))
                debug_log(paste("UI config included:", config_name), 2)
              }
            }
          }

          # Enhanced debug information
          rules_list$debug_info <- list(
            exported_at = Sys.time(),
            export_options = list(
              save_ui = safe_is_true(input$save_ui_autoassign_dw),
              include_filtering = safe_is_true(input$include_filtering_config),
              include_imputation = safe_is_true(input$include_imputation_config),
              include_batch_effects = safe_is_true(input$include_batch_effects_config),
              include_pivot = safe_is_true(input$include_pivot_config),
              include_merge = safe_is_true(input$include_merge_config),
              include_edit_ops = safe_is_true(input$include_edit_operations),
              include_ratios = safe_is_true(input$include_ratio_configurations)
            ),
            components_exported = components_exported,
            module_versions = list(
              auto_assign = "modular_v1",
              filtering = "enhanced_v2",
              imputation = "enhanced_v1",
              edit_operations = "enhanced_v1",
              batch_effects = "enhanced_v1",
              pivot = "enhanced_v1",
              merge = "enhanced_v1"
            ),
            last_import_info = last_import_info(),
            debug_level = DEBUG_LEVEL,
            processing_history = tail(processing_history(), 5)
          )

          # Save file
          saveRDS(rules_list, file)

          # Calculate export duration
          export_duration <- as.numeric(difftime(Sys.time(), export_start_time, units = "secs"))

          # Store enhanced export info
          last_export_info(list(
            timestamp = Sys.time(),
            filename = basename(file),
            components_exported = components_exported,
            export_size = file.info(file)$size,
            export_duration = export_duration,
            edit_operations_count = if (!is.null(rules_list$edit_operations)) nrow(rules_list$edit_operations) else 0,
            ratio_configurations_count = if (!is.null(rules_list$ratio_configurations)) nrow(rules_list$ratio_configurations) else 0,
            ui_configs_count = length(intersect(components_exported, c("batch_effects_ui_config", "pivot_ui_config", "merge_ui_config", "filtering_ui_config", "UI_imputation_ui_config")))
          ))

          template_export_status("completed")

          # Enhanced success logging
          add_processing_log("template_export", "success",
                             paste("Exported", length(components_exported), "components"), export_duration)

          debug_log(paste("Template exported successfully with:", paste(components_exported, collapse = ", "),
                          sprintf("(%.2fs)", export_duration)), 1)

          showNotification("Template exported successfully", type = "message", duration = 4)

        }, error = function(e) {
          export_duration <- as.numeric(difftime(Sys.time(), export_start_time, units = "secs"))
          template_export_status("error")

          add_processing_log("template_export", "error", e$message, export_duration)
          debug_log(paste("Export failed:", e$message), 1)

          showNotification("Export failed", type = "error", duration = 6)
        })
      }
    )

  }, envir = ctx)

  invisible(TRUE)
}
