# ==============================================================================
# File: R/export/export_orchestration.R
#
# Purpose:
#   Define the public comprehensive workbook export orchestrator used by app.R
#   and server export handlers.
#
# Architectural Role:
#   Thin composition/orchestration entrypoint for staged export pipeline helpers.
#
# Responsibilities:
#   - Keep public create_comprehensive_excel() API stable.
#   - Build shared export context and execute staged helpers in order.
#   - Return workbook on success or NULL on fatal export errors.
#
# Non-Responsibilities (Must NOT be here):
#   - Implement module-specific sheet creation logic inline.
#   - Duplicate utility code already centralized in sibling subscripts.
#
# Public API:
#   create_comprehensive_excel()
# ==============================================================================

#' Create comprehensive Excel workbook with guaranteed Data Wizard sheets
#'
#' Purpose:
#'   Public export entrypoint called by server handlers to build the workbook.
#'
#' Inputs/Parameters:
#'   @param module_outputs list of all module outputs.
#'   @param rv reactive values object.
#'   @param debug_level debug verbosity level.
#'
#' Outputs:
#'   @return openxlsx workbook object or NULL if creation fails.
#'
#' Side effects:
#'   - Reads module/reactive state and writes workbook sheets via stage helpers.
#'
#' Failure behavior:
#'   - Returns NULL on fatal errors while preserving per-stage graceful fallback.
create_comprehensive_excel <- function(module_outputs = NULL, rv = NULL, debug_level = 1) {
  debug_sys <- create_excel_debug_system(debug_level)
  debug_log <- debug_sys$debug_log

  debug_log("Starting comprehensive Excel creation from app.R", level = 1)

  tryCatch({
    ctx <- new.env(parent = environment())

    # Workbook and styles
    ctx$wb <- openxlsx::createWorkbook()
    ctx$sheets_created <- 0
    ctx$headerStyle <- openxlsx::createStyle(
      fontSize = 12,
      fontColour = "white",
      fgFill = "#4F81BD",
      textDecoration = "bold",
      border = "TopBottomLeftRight"
    )

    # Shared state
    ctx$debug_log <- debug_log
    ctx$module_outputs <- module_outputs
    ctx$rv <- rv
    ctx$datawizard_out <- NULL
    if (!is.null(module_outputs) && !is.null(module_outputs$datawizard_out)) {
      ctx$datawizard_out <- module_outputs$datawizard_out
    }
    ctx$processed_data <- NULL
    ctx$original_data <- NULL
    ctx$metadata <- NULL
    ctx$has_original <- FALSE
    ctx$has_processed <- FALSE
    ctx$has_metadata <- FALSE

    # Staged export pipeline
    export_pipeline_run_datawizard_primary(ctx)
    export_pipeline_run_go_gsea(ctx)
    export_pipeline_run_matrix_heatmap(ctx)
    export_pipeline_run_optional_modules(ctx)
    export_pipeline_run_finalize(ctx)

    debug_log(paste("Excel workbook created successfully with", ctx$sheets_created, "sheets"), level = 1)
    debug_log(paste("Data availability - Original:", ctx$has_original,
                    "Processed:", ctx$has_processed,
                    "Metadata:", ctx$has_metadata), level = 1)

    ctx$wb
  }, error = function(e) {
    debug_log(paste("Critical error in Excel creation:", e$message), level = 1)
    NULL
  })
}
