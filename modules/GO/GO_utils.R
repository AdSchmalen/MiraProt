# ==============================================================================
# GO Module - Utility Stub (Backward Compatibility)
# ==============================================================================
#
# Purpose:
#   Backward-compatibility shim. All functions previously defined in this file
#   have been reorganised into:
#     - GO_module_hub.R   (organism mapping and public resolvers)
#     - GO_module_hub_cache.R (persistent SQLite/cache metadata)
#     - GO_module_hub_annotationhub.R (AnnotationHub acquisition)
#     - GO_module_hub_discovery.R (key-type/organism discovery)
#     - GO_module_logic.R (readiness, pairing, identifiers, enrichment)
#     - GO_module_plots.R (p-value formatting and plot constructors)
#     - GO_module_tree.R  (tree construction and selection helpers)
#
#   This file is no longer sourced directly; GO_module.R now sources
#   the GO hub peer files followed by logic, plots, and tree directly.
#
#   This stub is retained to avoid breaking any scripts that may source
#   GO_utils.R independently outside the module loader. If no such scripts
#   exist it can be removed safely.
# ==============================================================================

source("modules/GO/GO_module_hub.R",               local = TRUE)
source("modules/GO/GO_module_hub_cache.R",         local = TRUE)
source("modules/GO/GO_module_hub_annotationhub.R", local = TRUE)
source("modules/GO/GO_module_hub_discovery.R",     local = TRUE)
source("modules/GO/GO_module_logic.R", local = TRUE)
source("modules/GO/GO_module_plots.R", local = TRUE)
source("modules/GO/GO_module_tree.R",  local = TRUE)
