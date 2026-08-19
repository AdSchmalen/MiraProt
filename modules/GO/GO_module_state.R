# ==============================================================================
# GO Module - Reactive State Definitions
# ==============================================================================
#
# Purpose:
#   Centralizes all reactiveVal() and reactiveValues() definitions for the GO
#   enrichment module. This file is the single authoritative source for all
#   reactive state surface of the module.
#
# Architecture role:
#   Sourced with local = TRUE inside modGOServer() in GO_module.R, and MUST be
#   sourced before GO_module_observer.R. All reactive values created here are
#   available to observers and outputs in GO_module_observer.R through the shared
#   server closure. No logic, no observers, no outputs belong here.
#
# Structure:
#   1. Analysis result state (GO_Result_List, go_data_processed, go_analysis_status,
#      go_tree_structure)
#   2. Import state (import_status_message)
#   3. Plot state (current_plot_object, current_plot_message, current_plot_height,
#      current_plot_width, plot_update_trigger, plot_config)
#   4. Error tracking state (go_errors, last_error_time)
#   5. Column pairing state (last_auto_paired_pval, user_manual_pval_selection)
#   6. Organism / key type loading state (organism_cache, keytype_loading,
#      keytype_last_organism, keytype_timer, go_keytype_initialized)
#   7. Download dimension state (last_manual_input_time)
#   8. Derived data readiness reactive (go_ready)
#
# Future developers:
#   - Add all new module-level reactive state here.
#   - Do not add observers, outputs, or logic here.
#   - This file must be sourced BEFORE GO_module_observer.R.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Analysis result state
# ------------------------------------------------------------------------------

GO_Result_List        <- reactiveVal(NULL)
go_data_processed     <- reactiveVal(NULL)
go_analysis_status    <- reactiveVal("idle")
go_tree_structure     <- reactiveVal(NULL)
go_results_ready_for_fallback <- reactiveVal(FALSE)
go_restore_guard_active <- reactiveVal(FALSE)
go_restore_row_index_skip_logged <- reactiveVal(FALSE)
go_restore_trigger_baseline <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# 2. Import state
# ------------------------------------------------------------------------------

import_status_message <- reactiveVal("")

# ------------------------------------------------------------------------------
# 3. Plot state
# ------------------------------------------------------------------------------

current_plot_object  <- reactiveVal(NULL)
current_plot_message <- reactiveVal("")
current_plot_height  <- reactiveVal(600)
current_plot_width   <- reactiveVal(800)
plot_update_trigger  <- reactiveVal(0)

plot_config <- reactiveValues(
  colors = c("#440154FF", "#31688EFF", "#EFC000FF"),
  theme  = theme_bw(),
  sizes  = list(
    axisTitle   = 12,
    tick        = 10,
    legendText  = 10,
    legendTitle = 11,
    label       = 10
  ),
  plot_dimensions = list(
    height_inch = 8,
    width_inch  = 10
  )
)

# ------------------------------------------------------------------------------
# 4. Error tracking state
# ------------------------------------------------------------------------------

go_errors      <- reactiveVal(list())
last_error_time <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# 5. Column pairing state
# ------------------------------------------------------------------------------

last_auto_paired_pval     <- reactiveVal(NULL)
user_manual_pval_selection <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# 6. Organism / key type loading state
# ------------------------------------------------------------------------------

organism_cache        <- reactiveValues()  # debug-inspection helper only
keytype_timer         <- reactiveVal(NULL)
keytype_loading       <- reactiveVal(FALSE)
go_keytype_initialized <- reactiveVal(FALSE)
keytype_last_organism <- reactiveVal(NULL)

# Session-level OrgDb cache.
# Holds the loaded OrgDb object for the current organism so that repeated
# "Run GO Analysis" clicks within the same session do not re-enter
# load_annotation_hub_with_progress() for the same organism.
# Mirrors the cached_org_db / cached_orgdb_name pattern in the Annotation module.
# Invalidated whenever the organism dropdown changes (OrgDb_GO observer).
cached_go_org_db   <- reactiveVal(NULL)
cached_go_orgdb_name <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# 7. Download dimension state
# ------------------------------------------------------------------------------

last_manual_input_time <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# 8. Derived data readiness reactive
# ------------------------------------------------------------------------------

go_ready <- reactive({
  req(rv$data_mod, rv$data_def)

  if (datawizard_metadata_defer_downstream_choices(rv)) {
    debug_log("Metadata assignment pending; deferring GO readiness check", 2)
    return(FALSE)
  }

  if ((isTRUE(rv$session_restoring) || isTRUE(go_restore_guard_active())) &&
      !is.null(tryCatch(GO_Result_List(), error = function(e) NULL))) {
    restore_readiness <- compute_go_readiness(rv$data_def)
    if (!isTRUE(restore_readiness$ready)) {
      debug_log("GO readiness downgrade skipped during session restore guard", 2)
      return(TRUE)
    }
  }

  data_available <- !is.null(rv$data_mod) && !is.null(rv$data_def)

  if (data_available) {
    return(compute_go_readiness(rv$data_def)$ready)
  }

  FALSE
})
