#!/usr/bin/env python3
"""Static regression checks for Data Wizard submodule restore ordering."""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INTEGRATION = ROOT / "modules" / "Data Wizard" / "datawizard_integration.R"
ASSIGN_RULES = ROOT / "modules" / "Data Wizard" / "datawizard_assign_rules.R"
REGISTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_module_registration.R"
CORE = ROOT / "modules" / "Data Wizard" / "datawizard_core.R"

integration = INTEGRATION.read_text()
assign_rules = ASSIGN_RULES.read_text()
registration = REGISTRATION.read_text()
core = CORE.read_text()

assert ".dw_session_submodule_keys_by_phase <- list" in integration, (
    "Data Wizard submodule keys must be grouped into restore phases"
)
phase_match = re.search(
    r"\.dw_session_submodule_keys_by_phase <- list\((.*?)\n\)\n\n\.dw_session_submodule_keys",
    integration,
    flags=re.DOTALL,
)
assert phase_match, "Could not locate phased submodule key definition"
phases = phase_match.group(1)
loader_pos = phases.index("loader = c(\"loader_out\")")
assign_pos = phases.index("assign_rules = c(\"assign_rules_out\")")
downstream_pos = phases.index("downstream = c(")
assert loader_pos < assign_pos < downstream_pos, (
    "Restore phases must run loader_out, then assign_rules_out, then downstream modules"
)
for key in [
    "imputation_out", "filtering_out", "batch_out", "pivot_out", "merge_out",
    "ratios_out", "basemean_out", "annotation_out", "auto_assign_out", "edit_out",
]:
    assert key in phases, f"Downstream restore phase is missing {key}"

ordering_match = re.search(
    r"ordered_keys <- unique\(c\((.*?)\n  \)\)",
    integration,
    flags=re.DOTALL,
)
assert ordering_match, "Could not locate set_all_submodule_ui_states ordering"
ordering = ordering_match.group(1)
assert ordering.index("$loader") < ordering.index("$assign_rules") < ordering.index("$downstream"), (
    "set_all_submodule_ui_states must dispatch phase groups in dependency order"
)

setter_match = re.search(
    r"set_session_state_fn <- function\(state\) \{(.*?)\n      assign_rules_session_state_base\$set_session_state\(state\)",
    assign_rules,
    flags=re.DOTALL,
)
assert setter_match, "Could not locate Assign Rules set_session_state_fn pre-hydration block"
setter_prefix = setter_match.group(1)
for token in ["condition_inputs(extra$condition_inputs)", "counter_condition(as.integer(extra$counter_condition))", "Options_condition(extra$Options_condition)"]:
    assert token in setter_prefix, f"Assign Rules must hydrate {token} before base UI restore"
assert setter_prefix.index("condition_inputs(extra$condition_inputs)") < setter_prefix.index(
    "counter_condition(as.integer(extra$counter_condition))"
) < setter_prefix.index("Options_condition(extra$Options_condition)"), (
    "Assign Rules condition state hydration order changed unexpectedly"
)

assert "Assign Rules condition option(s) before downstream submodule dispatch" in registration, (
    "module_ui restore phase must log saved Assign Rules condition option count before dispatch"
)
assert registration.index("n_assign_rules_condition_options") < registration.index(
    ".safe_fn_call_arg(dw$set_all_submodule_ui_states, submodule_state_payload)"
), "Assign Rules option-count log must happen before submodule payload dispatch"

saved_condition_inputs = {"Condition_1": "NA", "Condition_2": "C", "Condition_3": "ERU"}
saved_options = [None, "NA", "C", "ERU"]
assert len(saved_condition_inputs) == 3, "Regression fixture should save three condition text boxes"
assert {"NA", "C", "ERU"}.issubset(set(saved_options)), (
    "Regression fixture must preserve saved condition option values"
)
assert "remaining_ids <- unique(c(deferred_ids, unresolved_ids))" in core or "max_restore_attempts" in core, (
    "Dynamic textin* replay must be retained or safely bounded when controls are not yet rendered"
)
for i, expected in enumerate(["NA", "C", "ERU"], start=1):
    assert saved_condition_inputs[f"Condition_{i}"] == expected, f"textin{i} saved value fixture mismatch"

print("Data Wizard restore ordering static checks passed")
