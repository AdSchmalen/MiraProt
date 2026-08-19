# ==============================================================================
# File: R/export/export_pipeline_finalize.R
#
# Purpose:
#   Finalize workbook export by writing logs/summary and applying sheet
#   width, rename, and ordering rules.
#
# Architectural Role:
#   Terminal stage helper for create_comprehensive_excel() orchestration.
#
# Responsibilities:
#   - Write Log and Export_Summary sheets.
#   - Track final data availability flags in context.
#   - Auto-width columns and enforce rename/reorder sheet policy.
#
# Non-Responsibilities (Must NOT be here):
#   - Module data extraction and main sheet generation stages.
#
# Public API:
#   export_pipeline_run_finalize()
# ==============================================================================

#' Pipeline stage: workbook finalization
#'
#' Purpose:
#'   Run terminal export finalization within the shared context.
#'
#' Inputs/Parameters:
#'   @param ctx Environment-based export context.
#'
#' Outputs:
#'   - Invisibly returns NULL; mutates `ctx` in place.
#'
#' Side effects:
#'   - Writes summary/log worksheets, renames/reorders sheets, updates flags.
#'
#' Failure behavior:
#'   - Preserves localized error handling and logs failures without hard abort.
export_pipeline_run_finalize <- function(ctx) {
  with(ctx, {
    has_original <- FALSE
    has_processed <- FALSE
    has_metadata <- FALSE
    # ========================================
    # Debug Log Sheet (Level 0)
    # ========================================
    debug_log("Creating Log sheet for debug level 0 entries", level = 1)

    tryCatch({
      level0_logs <- extract_level0_debug_entries(rv)
      openxlsx::addWorksheet(wb, "Log")
      level0_logs <- sanitize_for_excel(level0_logs, "Log", debug_log)
      writeData_sanitized(wb, "Log", level0_logs, startRow = 1, startCol = 1)
      if (ncol(level0_logs) > 0) {
        openxlsx::addStyle(wb, "Log", headerStyle, rows = 1, cols = 1:ncol(level0_logs))
      }
      sheets_created <- sheets_created + 1
      debug_log(paste("Log sheet created with", nrow(level0_logs), "level 0 debug entries"), level = 1)
    }, error = function(e) {
      debug_log(paste("Error creating Log sheet:", e$message), level = 1)
    })

    # ========================================
    # Export Summary Sheet
    # ========================================
    debug_log("Creating Export Summary sheet", level = 1)

    tryCatch({
      openxlsx::addWorksheet(wb, "Export_Summary")

      # Collect summary information
      original_rows <- if (!is.null(original_data) && is.data.frame(original_data)) nrow(original_data) else 0L
      processed_rows <- if (!is.null(processed_data) && is.data.frame(processed_data)) nrow(processed_data) else 0L
      metadata_rows <- if (!is.null(metadata) && is.data.frame(metadata)) nrow(metadata) else 0L

      # Fallback: in some code paths metadata is available in rv$data_def but not
      # retained on `ctx$metadata`; use it so Export_Summary reflects actual rows.
      if (metadata_rows == 0L && !is.null(rv$data_def) && is.data.frame(rv$data_def)) {
        metadata_rows <- nrow(rv$data_def)
      }

      has_original <- original_rows > 0L
      has_processed <- processed_rows > 0L
      has_metadata <- metadata_rows > 0L

      export_summary <- data.frame(
        Property = c(
          "Session Timestamp",
          "Session Token",
          "Session PID",
          "MiraProt Version",
          "R Version",
          "Export Date",
          "Export Time",
          "Total Sheets Created",
          "Original Data Available",
          "Processed Data Available",
          "Metadata Available",
          "Original Data Rows",
          "Processed Data Rows",
          "Metadata Rows",
          "Export Status",
          "Data Wizard Status"
        ),
        Value = c(
          as.character(Sys.time()),
          as.character(
            rv$restored_session_token %||%
              get0("MIRAPROT_SESSION_TOKEN", envir = globalenv(), inherits = FALSE, ifnotfound = NA_character_)
          ),
          as.character(Sys.getpid()),
          "1.0.0",
          R.version.string,
          as.character(Sys.Date()),
          format(Sys.time(), "%H:%M:%S"),
          as.character(sheets_created),
          ifelse(has_original, "Yes", "No"),
          ifelse(has_processed, "Yes", "No"),
          ifelse(has_metadata, "Yes", "No"),
          as.character(original_rows),
          as.character(processed_rows),
          as.character(metadata_rows),
          ifelse(has_original || has_processed || has_metadata, "Success", "No Data Available"),
          ifelse(has_original && has_processed && has_metadata, "Complete",
                 ifelse(has_original || has_processed || has_metadata, "Partial", "No Data"))
        ),
        stringsAsFactors = FALSE
      )

      writeData_sanitized(wb, "Export_Summary", export_summary, startRow = 1, startCol = 1)
      openxlsx::addStyle(wb, "Export_Summary", headerStyle, rows = 1, cols = 1:2)

      debug_log("Export summary sheet created", level = 1)

    }, error = function(e) {
      debug_log(paste("Error creating export summary:", e$message), level = 1)
    })

    # Auto-adjust column widths for all sheets
    tryCatch({
      all_sheet_names <- openxlsx::sheets(wb)
      for (sheet in all_sheet_names) {
        openxlsx::setColWidths(wb, sheet, cols = 1:20, widths = "auto")
      }
      debug_log("Column widths adjusted for all sheets", level = 2)
    }, error = function(e) {
      debug_log(paste("Error adjusting column widths:", e$message), level = 2)
    })

    # ========================================
    # Rename and Reorder Worksheets
    # ========================================
    tryCatch({
      current_sheets <- openxlsx::sheets(wb)

      # Resolve naming conflict: the abundance-wizard sheet is called "DotPlot_Data"
      # but "DotPlot_Result" (dot-plot module) must also be renamed to "DotPlot_Data".
      # Rename the abundance-wizard sheet first so the target name is free.
      if ("DotPlot_Data" %in% current_sheets && "DotPlot_Result" %in% current_sheets) {
        openxlsx::renameWorksheet(wb, "DotPlot_Data", "DotPlot_Matrix_Data")
      }

      # Rename sheets (name only; content is unchanged)
      rename_map <- list(
        "Abundance_Summary"  = "Abundance_Data",
        "SampleIDs"          = "SampleIDs_Data",
        "PCA_Analysis"       = "Dimensions_Data",
        "Volcano_Result"     = "Volcano_Data",
        "DotPlot_Result"     = "DotPlot_Data",
        "Venn_UpSet_Results" = "Venn_UpSet_Data"
      )
      for (old_name in names(rename_map)) {
        if (old_name %in% current_sheets) {
          openxlsx::renameWorksheet(wb, old_name, rename_map[[old_name]])
        }
      }

      # Reorder sheets; any sheets not listed here are appended at the end
      desired_order <- c(
        "Processed_Data", "Original_Data", "Metadata",
        "Abundance_Data", "SampleIDs_Data", "Dimensions_Data",
        "Volcano_Data", "DotPlot_Data",
        "GO_Analysis", "GSEA_Analysis",
        "STRING_Result",
        "Venn_UpSet_Data", "Heatmap_Data",
        "Log", "Export_Summary"
      )
      current_sheets   <- openxlsx::sheets(wb)
      ordered_present  <- intersect(desired_order, current_sheets)
      remaining_sheets <- setdiff(current_sheets, desired_order)
      new_order_names  <- c(ordered_present, remaining_sheets)
      openxlsx::worksheetOrder(wb) <- match(new_order_names, current_sheets)

      debug_log("Worksheets renamed and reordered successfully", level = 1)
    }, error = function(e) {
      debug_log(paste("Error renaming/reordering worksheets:", e$message), level = 1)
    })
    ctx$has_original <- has_original
    ctx$has_processed <- has_processed
    ctx$has_metadata <- has_metadata
  })
  invisible(NULL)
}
