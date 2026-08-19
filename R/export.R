# ============================================================================
# Module Script: R/export.R
# Purpose:
#   Parent loader for the refactored Excel export subsystem.
#
# Architectural Role:
#   composition root
#
# Responsibilities:
#   - Source Excel export sub-scripts into the active module environment.
#   - Keep the public API surface unchanged for callers (app.R, server handlers).
#
# Non-Responsibilities (Must NOT be here):
#   - Implement workbook/sheet export workflows directly.
#   - Contain module-specific export logic.
# ============================================================================

.export_source_dir <- local({
  src_file <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  if (!is.character(src_file) || length(src_file) != 1L || !nzchar(src_file)) {
    return(file.path("R", "export"))
  }
  file.path(dirname(src_file), "export")
})

.export_subscripts <- c(
  "export_debug_utils.R",
  "export_module_helpers.R",
  "export_pipeline_datawizard_primary.R",
  "export_pipeline_go_gsea.R",
  "export_pipeline_matrix_heatmap.R",
  "export_pipeline_optional_modules.R",
  "export_pipeline_finalize.R",
  "export_orchestration.R",
  "export_migration_map.R"
)

for (.export_subscript in .export_subscripts) {
  .export_subscript_path <- file.path(.export_source_dir, .export_subscript)
  if (!file.exists(.export_subscript_path)) {
    stop("Missing export sub-script: ", .export_subscript_path)
  }
  sys.source(.export_subscript_path, envir = environment())
}

rm(.export_source_dir,
   .export_subscripts,
   .export_subscript,
   .export_subscript_path)
