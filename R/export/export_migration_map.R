# ==============================================================================
# File: R/export/export_migration_map.R
#
# Purpose:
#   Document block-level migration from the previous monolithic
#   export_orchestration.R implementation to staged helper scripts.
#
# Architectural Role:
#   Maintenance-only mapping reference for contributors.
#
# Responsibilities:
#   - Record old block -> new helper function/file mapping.
#
# Non-Responsibilities (Must NOT be here):
#   - Runtime workbook logic.
#
# Public API:
#   (none; documentation-only script)
# ==============================================================================

# Migration map (old block -> new helper)
# - Lines 61-739   -> export_pipeline_run_datawizard_primary() in export_pipeline_datawizard_primary.R
# - Lines 740-1121 -> export_pipeline_run_go_gsea() in export_pipeline_go_gsea.R
# - Lines 1122-1642 -> export_pipeline_run_matrix_heatmap() in export_pipeline_matrix_heatmap.R
# - Lines 1643-2123 -> export_pipeline_run_optional_modules() in export_pipeline_optional_modules.R
# - Lines 2124-2253 -> export_pipeline_run_finalize() in export_pipeline_finalize.R
# - Public entrypoint remains create_comprehensive_excel() in export_orchestration.R
