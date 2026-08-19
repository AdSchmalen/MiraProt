# R/server_export.R
# ========================================
# Excel Export: Download Handler & Status
# ========================================
# Sets up the multi-module Excel export download and its status output.
# Called from the server function in app.R.
# Depends on: DEBUG_LEVEL, debug_log() from R/bootstrap.R
#             create_comprehensive_excel() from modules/

setup_export_handlers <- function(output, rv, module_outputs) {

  # ========================================
  # Download Handler
  # ========================================

  output$download_results <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("MiraProt_Results_", timestamp, ".xlsx")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Creating Excel file...', value = 0, {
          incProgress(0.1, detail = "Initializing export...")
          debug_log("Starting Excel export", 1)

          # Use the existing create_comprehensive_excel function
          if (exists("create_comprehensive_excel")) {
            incProgress(0.3, detail = "Creating comprehensive Excel with module data...")

            modules_list <- list()
            for (name in names(module_outputs)) {
              modules_list[[name]] <- module_outputs[[name]]
            }

            wb <- create_comprehensive_excel(
              module_outputs = modules_list,
              rv = rv,
              debug_level = DEBUG_LEVEL
            )

            if (!is.null(wb)) {
              incProgress(0.8, detail = "Finalizing Excel file...")
              openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
              incProgress(1.0, detail = "Export complete!")

              showNotification("Excel export completed.",
                               type = "message", duration = 5)
              debug_log("Excel export completed successfully", 1)
              return()
            } else {
              debug_log("create_comprehensive_excel returned NULL", 1)
            }
          } else {
            debug_log("create_comprehensive_excel function not found", 1)
          }

          # Fallback: Create minimal export if main function fails
          debug_log("Main export failed or returned NULL, creating minimal fallback", 1)
          incProgress(0.5, detail = "Creating minimal export...")

          fallback_wb <- openxlsx::createWorkbook()

          # Add basic data if available
          if (!is.null(rv$data_mod)) {
            openxlsx::addWorksheet(fallback_wb, "Data")
            fallback_data <- rv$data_mod
            if (exists("sanitize_for_excel") && is.function(sanitize_for_excel)) {
              fallback_data <- sanitize_for_excel(fallback_data, "Data", debug_log)
            }
            openxlsx::writeData(fallback_wb, "Data", fallback_data)
          }

          if (!is.null(rv$data_def)) {
            openxlsx::addWorksheet(fallback_wb, "Metadata")
            fallback_meta <- rv$data_def
            if (exists("sanitize_for_excel") && is.function(sanitize_for_excel)) {
              fallback_meta <- sanitize_for_excel(fallback_meta, "Metadata", debug_log)
            }
            openxlsx::writeData(fallback_wb, "Metadata", fallback_meta)
          }

          # Add status sheet
          openxlsx::addWorksheet(fallback_wb, "Export_Status")
          status_data <- data.frame(
            Issue = "Partial Export",
            Details = "Main export function failed, basic data included",
            Timestamp = as.character(Sys.time()),
            Data_Available = !is.null(rv$data_mod),
            Metadata_Available = !is.null(rv$data_def),
            stringsAsFactors = FALSE
          )

          openxlsx::writeData(fallback_wb, "Export_Status", status_data)

          incProgress(0.8, detail = "Saving fallback file...")
          openxlsx::saveWorkbook(fallback_wb, file, overwrite = TRUE)
          incProgress(1.0, detail = "Fallback export complete")

          showNotification("Partial Excel export created. Some module data may be missing.",
                           type = "warning", duration = 8)
          debug_log("Fallback export completed", 1)
        })

      }, error = function(e) {
        debug_log(paste("Excel export failed completely:", e$message), 1)

        # Last resort: Create error file
        tryCatch({
          error_wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(error_wb, "Error")

          error_data <- data.frame(
            Error = "Export Failed",
            Message = e$message,
            Timestamp = as.character(Sys.time()),
            Suggestion = "Check console for details",
            stringsAsFactors = FALSE
          )

          openxlsx::writeData(error_wb, "Error", error_data)
          openxlsx::saveWorkbook(error_wb, file, overwrite = TRUE)

          showNotification(paste("Download failed:", e$message), type = "error", duration = 10)
        }, error = function(e2) {
          showNotification("Complete download failure - check console", type = "error", duration = 10)
        })
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # ========================================
  # Export Status Text
  # ========================================

  output$export_status <- renderText({
    tryCatch({
      available_modules <- sum(!sapply(module_outputs, is.null))
      total_modules <- length(module_outputs)

      # Check which specific modules are available
      mod_status <- character()
      for (name in names(module_outputs)) {
        if (!is.null(module_outputs[[name]])) {
          clean_name <- gsub("_out$", "", name)
          mod_status <- c(mod_status, clean_name)
        }
      }

      if (available_modules == 0) {
        "[WARNING] No modules loaded"
      } else if (available_modules == total_modules) {
        paste("[OK] Ready for export -", available_modules, "modules available:", paste(mod_status, collapse = ", "))
      } else {
        paste("[WARNING] Partial export ready -", available_modules, "of", total_modules, "modules available:", paste(mod_status, collapse = ", "))
      }
    }, error = function(e) {
      debug_log(paste("Error checking export status:", e$message), 1)
      "[WARNING] Export status unknown"
    })
  })

  invisible(NULL)
}
