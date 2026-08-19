# ============================================================================
# Module/Sub-script: modules/Data Wizard/assign rules/datawizard_assign_rules_adapters.R
# Purpose:
#   Defensive adapter functions for assign-rules integration handoff. Adapters
#   normalize validation and failure behavior when applying extracted UI-config
#   payloads to assign-rules state channels.
#
# Architectural Role:
#   adapters
#
# Responsibilities:
#   - Wrap UI-config apply paths with consistent validation and safe fallback.
#   - Keep source-tagging and error-recording semantics consistent.
#   - Isolate optional external-readiness checks from orchestrator flow code.
#
# Non-Responsibilities:
#   - Own reactive state initialization or observer registration.
#   - Define UI rendering or rule extraction logic.
#
# Allowed Dependencies:
#   - assign-rules utility helpers (`set_ui_config_safe`, validators).
#   - Shiny/reactive callbacks supplied by orchestrator.
#
# Interaction Boundaries:
#   - Inputs: UI payloads, validator callbacks, reactive setters, logging/error
#             channels, optional metadata reactive.
#   - Outputs: boolean success/failure outcomes for orchestration handlers.
#   - Side Effects: may update config channels, source tags, and error logs.
#
# Stability Guarantees:
#   - Preserve existing success/failure semantics of assign-rules config setters.
#   - Keep invalid payload handling deterministic and non-crashing.
# ============================================================================

assign_rules_adapter_set_ui_config <- function(
  ui_config,
  reactive_setter,
  validator,
  config_name,
  source_name,
  debug_log,
  ui_config_errors,
  ui_config_sources
) {
  set_ui_config_safe(
    ui_config,
    reactive_setter,
    validator,
    config_name,
    source_name,
    debug_log = debug_log,
    ui_config_errors = ui_config_errors,
    ui_config_sources = ui_config_sources
  )
}

assign_rules_adapter_set_edit_ui_config <- function(
  ui_config,
  reactive_setter,
  debug_log,
  ui_config_errors,
  ui_config_sources,
  metadata_current = NULL
) {
  # Always validate edit payload first
  if (!validate_ui_edit_config(ui_config)) {
    debug_log("Edit config validation failed", 1)
    current_errors <- ui_config_errors()
    ui_config_errors(append(current_errors, "Invalid edit config structure"))
    return(FALSE)
  }

  # Metadata readiness check is intentionally non-blocking; kept for diagnostics.
  if (!is.null(metadata_current)) {
    tryCatch({
      current_metadata <- metadata_current()
      metadata_available <- !is.null(current_metadata) && nrow(current_metadata) > 0
      if (!isTRUE(metadata_available)) {
        debug_log("Edit config stored while metadata is not ready", 2)
      }
    }, error = function(e) {
      debug_log(paste("Error checking metadata availability:", e$message), 2)
    })
  }

  assign_rules_adapter_set_ui_config(
    ui_config = ui_config,
    reactive_setter = reactive_setter,
    validator = validate_ui_edit_config,
    config_name = "edit",
    source_name = "edit",
    debug_log = debug_log,
    ui_config_errors = ui_config_errors,
    ui_config_sources = ui_config_sources
  )
}
