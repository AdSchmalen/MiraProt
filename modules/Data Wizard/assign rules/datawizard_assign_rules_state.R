# ============================================================================
# Module/Sub-script: modules/Data Wizard/assign rules/datawizard_assign_rules_state.R
# Purpose:
#   Reactive state factory for assign-rules workflows. This script centralizes
#   source-of-truth reactive containers and default-state reset values used by
#   the assign-rules orchestrator.
#
# Architectural Role:
#   reactive state
#
# Responsibilities:
#   - Create and return the assign-rules reactive state container.
#   - Provide canonical default UI-config source-map values.
#   - Keep initialization/reset state shape consistent across workflows.
#
# Non-Responsibilities:
#   - Execute module event handlers or workflow orchestration.
#   - Implement UI layout, rule extraction, or data-processing algorithms.
#
# Allowed Dependencies:
#   - Shiny reactive primitives (`reactiveVal`).
#
# Interaction Boundaries:
#   - Inputs: none (factory initializes state with deterministic defaults).
#   - Outputs: named list of reactiveVals and default-map helper function.
#   - Side Effects: allocation of module-local reactive containers.
#
# Stability Guarantees:
#   - Preserve names and state semantics consumed by assign-rules orchestrator.
#   - Keep default-source map keys stable unless module contracts are migrated.
# ============================================================================

assign_rules_default_ui_config_sources <- function() {
  list(
    imputation = "none",
    filtering = "none",
    batch_effects = "none",
    pivot = "none",
    merge = "none",
    edit = "none"
  )
}

create_assign_rules_state <- function() {
  list(
    # Core condition and file state
    condition_inputs = reactiveVal(list(Condition_1 = "Condition 1")),
    counter_condition = reactiveVal(1),
    Options_condition = reactiveVal(c(NA_character_, "Condition 1")),
    selected_rule_file = reactiveVal(""),
    active_rule_file = reactiveVal(""),
    rule_loading_status = reactiveVal("idle"),
    # Do not call a separately sourced helper in reactiveVal's startup promise.
    # The loader will replace this with assign_rules_load_status() records as it
    # advances; this literal has the same public shape for the initial state.
    rule_load_pipeline_status = reactiveVal(structure(
      list(ok = TRUE, phase = "idle", code = "ok", message = "", details = list()),
      class = c("assign_rules_load_status", "list")
    )),

    # UI configuration channels
    assign_rules_ui_imputation = reactiveVal(NULL),
    assign_rules_ui_filtering = reactiveVal(NULL),
    assign_rules_ui_ratios = reactiveVal(NULL),
    assign_rules_ui_batch_effects = reactiveVal(NULL),
    assign_rules_ui_pivot = reactiveVal(NULL),
    assign_rules_ui_merge = reactiveVal(NULL),
    assign_rules_ui_edit = reactiveVal(NULL),
    assign_rules_ui_basemean = reactiveVal(NULL),

    # Processing / diagnostics
    processing_errors = reactiveVal(list()),
    processing_warnings = reactiveVal(list()),
    processing_log = reactiveVal(list()),
    ui_config_application_status = reactiveVal("idle"),
    ui_config_sources = reactiveVal(assign_rules_default_ui_config_sources()),
    ui_config_errors = reactiveVal(list()),
    last_loaded_rule_data = reactiveVal(NULL),
    processing_history = reactiveVal(list()),
    last_processing_time = reactiveVal(NULL),

    # Notification aggregation state
    rule_load_events = reactiveVal(list()),
    aggregated_notifications_enabled = TRUE,

    # Trigger counter for explicit re-application of metadata rules via button
    apply_metadata_rules_trigger = reactiveVal(0L)
  )
}
