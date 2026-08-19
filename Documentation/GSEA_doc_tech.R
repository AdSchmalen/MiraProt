# ==============================================================================
# File: Documentation/GSEA_doc_tech.R
#
# Purpose:
#   Backward-compatible wrapper for technical GSEA documentation renderers.
#   Canonical technical content is implemented in:
#     - Documentation/GSEA_doc_tech_core.R
#     - Documentation/GSEA_doc_tech_functions.R
# ==============================================================================

# Backward-compatible aliases retained for existing navigation keys.
render_GSEA_tech_overview_content_GSEA <- function() {
  render_GSEA_tech_core_overview_content_GSEA()
}

render_GSEA_tech_functions_content_GSEA <- function() {
  render_GSEA_tech_functions_content_GSEA_v2()
}

render_GSEA_tech_dataproc_content_GSEA <- function() {
  render_GSEA_tech_core_dataproc_content_GSEA()
}

render_GSEA_tech_integration_content_GSEA <- function() {
  render_GSEA_tech_core_integration_content_GSEA()
}
