# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Export Handlers
# Purpose: Export the three Auto-Assign rule frames as an RDS rule file.
# ============================================================================

register_auto_assign_export_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_export_handlers requires an environment context")
  }

  evalq({
    output$export_rules_autoassign_dw <- downloadHandler(
      filename = function() {
        paste0("auto_assign_rules_", Sys.Date(), ".rds")
      },
      content = function(file) {
        export_start_time <- Sys.time()
        template_export_status("exporting")

        tryCatch({
          # Retain the established top-level rule-frame structure so the file
          # follows the same legacy migration/import path as existing files.
          rules_list <- list(
            table = rv_table_rules_autoassign_dw(),
            condition = rv_condition_rules_autoassign_dw(),
            ratio = rv_rules_autoassign_dw()
          )

          saveRDS(rules_list, file)

          export_duration <- as.numeric(difftime(
            Sys.time(), export_start_time, units = "secs"
          ))
          rule_counts <- vapply(rules_list, nrow, integer(1))

          last_export_info(list(
            timestamp = Sys.time(),
            filename = basename(file),
            components_exported = names(rules_list),
            rule_counts = rule_counts,
            export_size = file.info(file)$size,
            export_duration = export_duration
          ))
          template_export_status("completed")

          add_processing_log(
            "rule_set_export", "success",
            paste("Exported", sum(rule_counts), "Auto-Assign rules"),
            export_duration
          )
          debug_log(sprintf(
            "Auto-Assign rule set exported successfully (%d rules, %.2fs)",
            sum(rule_counts), export_duration
          ), 1)
          showNotification("Auto-Assign rule set exported successfully", type = "message", duration = 4)
        }, error = function(e) {
          export_duration <- as.numeric(difftime(
            Sys.time(), export_start_time, units = "secs"
          ))
          template_export_status("error")
          add_processing_log("rule_set_export", "error", e$message, export_duration)
          debug_log(paste("Rule set export failed:", e$message), 1)
          showNotification("Rule set export failed", type = "error", duration = 6)
        })
      }
    )
  }, envir = ctx)

  invisible(TRUE)
}
