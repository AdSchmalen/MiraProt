# ./Documentation/datawizard_doc.R
# Datawizard Module Documentation — Orchestrator
#
# This file acts as the single entry point for Datawizard documentation.
# All content has been refactored into focused companion files listed below.
#
# Since app.R loads all Documentation/*.R files into modEnv automatically,
# the companion files are already available when this file is sourced.
# This orchestrator exists to:
#   (1) provide a clear entry point and file map, and
#   (2) re-export modDatawizardDocUI / modDatawizardDocServer so that
#       existing references (e.g. in R/ui.R and R/server_modules.R) continue
#       to resolve without changes.
#
# File map:
#   datawizard_doc_ui.R           — UI layout, server routing (defines
#                                    modDatawizardDocUI / modDatawizardDocServer)
#   datawizard_doc_user.R         — User guide content (13 sections); Auto RegEx
#                                    guidance is in the Auto-Assign user section
#   datawizard_doc_tech_core.R    — Architecture, module loading, data flow, logging
#   datawizard_doc_tech_reactive.R— Auto-assign, assign rules, import/export (RDS);
#                                    Auto RegEx guidance is in the Auto-Assign
#                                    technical section
#   datawizard_doc_tech_helpers.R — Batch effects, pivot, merge modules
#   datawizard_doc_data.R         — Filtering, edit, imputation, ratios, basemean
#
# Loading order does not matter because app.R sources every Documentation/*.R
# file into modEnv and all render_* functions are plain functions (not closures
# that capture load-time state).
