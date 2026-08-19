# ==============================================================================
# File: Documentation/abundances_doc.R
#
# Purpose:
#   Orchestrator for the Abundances documentation module. This file is sourced
#   into modEnv by the bulk loader in app.R. The actual content is split across
#   three sub-files that are also sourced automatically (alphabetical order):
#
#     abundances_doc_tech.R  - Technical documentation content functions
#     abundances_doc_ui.R    - UI layout, server routing, navigation logic
#     abundances_doc_user.R  - User guide content functions
#
# Load Order (via list.files alphabetical):
#   1. abundances_doc.R       <- this file (orchestrator, no function defs)
#   2. abundances_doc_tech.R  <- render_tech_*_content_abundance()
#   3. abundances_doc_ui.R    <- modAbundancesDocUI(), modAbundancesDocServer()
#   4. abundances_doc_user.R  <- render_*_content_abundance() (user guide)
#
# Public API (consumed by R/ui.R and R/server_modules.R):
#   modAbundancesDocUI(id)                  - defined in abundances_doc_ui.R
#   modAbundancesDocServer(id, debug_level) - defined in abundances_doc_ui.R
#
# Content Rendering Functions:
#   User Guide (abundances_doc_user.R):
#     render_overview_content_abundance()
#     render_creating_content_abundance()
#     render_customizing_content_abundance()
#     render_interactive_content_abundance()
#     render_downloading_content_abundance()
#
#   Technical Documentation (abundances_doc_tech.R):
#     render_tech_overview_content_abundance()
#     render_tech_functions_content_abundance()
#     render_tech_data_processing_content_abundance()
#     render_tech_integration_content_abundance()
#
# Notes for future developers:
#   - This file intentionally contains no function definitions. All functions
#     are defined in the three sub-files listed above.
#   - The sub-files are sourced into modEnv automatically by the same
#     list.files() loop in app.R that sources this file. No explicit
#     source() calls are needed here.
#   - To add a new documentation section:
#     1. Add a navigation link in abundances_doc_ui.R (modAbundancesDocUI)
#     2. Add an observeEvent in abundances_doc_ui.R (modAbundancesDocServer)
#     3. Add a render function in the appropriate content file
#     4. Add the case to the switch() in modAbundancesDocServer
# ==============================================================================
