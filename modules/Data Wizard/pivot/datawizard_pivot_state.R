# ============================================================================
# File: modules/Data Wizard/pivot/datawizard_pivot_state.R
# Purpose:
#   Define and centralize all reactive state containers used by the Data Wizard
#   Pivot submodule server implementation.
#
# Architecture role:
#   This file is the single source of truth for Pivot reactive state creation.
#   It is sourced by the Pivot orchestrator before server implementation code,
#   and consumed by `modPivotServer()` through a state object.
#
# Structure:
#   - `pivot_create_state()`: factory that initializes and returns all reactive
#     values required by the Pivot module runtime.
#
# Safe maintenance notes:
#   - Add new Pivot runtime reactive values here first, then wire them in the
#     implementation layer.
#   - Keep naming stable to preserve compatibility with existing server logic.
#   - Do not put observers or output rendering in this file; state only.
# ============================================================================

pivot_create_state <- function() {
  list(
    pivot_errors = reactiveVal(list()),
    last_operation_time = reactiveVal(NULL),
    operation_history = reactiveVal(list()),
    ui_config_applied = reactiveVal(FALSE),
    ui_config_source = reactiveVal("none"),
    ui_config_update_in_progress = reactiveVal(FALSE),
    pivot_options_state = reactiveVal(list()),
    preview_error_count = reactiveVal(0),
    preview_last_error_time = reactiveVal(NULL),
    pivot_operation_params = reactiveVal(NULL)
  )
}
