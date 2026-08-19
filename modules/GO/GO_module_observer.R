# ==============================================================================
# GO Module - Ordered Observer Loader
# ==============================================================================
#
# This file is sourced with local = TRUE from the active modGOServer() frame.
# Keep these peer fragments in their historical registration order so helpers,
# reactives, observers, and outputs remain available before their first use.
# ==============================================================================

source("modules/GO/GO_module_observer_data_choices.R", local = TRUE)
source("modules/GO/GO_module_observer_enrichment_cache.R", local = TRUE)
source("modules/GO/GO_module_observer_plot_grid.R", local = TRUE)
source("modules/GO/GO_module_observer_restore_cleanup.R", local = TRUE)
