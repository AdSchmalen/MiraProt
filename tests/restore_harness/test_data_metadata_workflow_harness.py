#!/usr/bin/env python3
"""Reproducible Data & Metadata session workflow harness.

This harness models the MiraProt Data & Metadata save/restore contract without
starting a Shiny runtime. It uses synthetic Data Wizard data that follows the
user workflow: load data, apply AutoAssign condition groups, save using the
Data & Metadata level, restore into a fresh server state, and assert that only
Data Wizard state is replayed.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
CORE_HELPERS = ROOT / "R" / "session_save_restore" / "session_save_restore_core_helpers.R"
ORCHESTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_orchestration.R"
REGISTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_module_registration.R"

DATA_ONLY = "data_only"
EXPECTED_CONDITIONS = {"C", "ERU"}
DATAWIZARD_SUBMODULES = {
    "loader_out",
    "auto_assign_out",
    "assign_rules_out",
    "filtering_out",
    "imputation_out",
}
ANALYSIS_MODULES = {"pca", "volcano", "heatmap", "dotplot", "go", "gsea", "venn", "string", "grid"}


@dataclass
class ServerState:
    """Small stand-in for the app/server reactiveValues and module side effects."""

    rv: dict[str, Any] = field(default_factory=dict)
    datawizard_ui_payloads: dict[str, dict[str, Any]] = field(default_factory=dict)
    restored_modules: list[str] = field(default_factory=list)
    analysis_restore_calls: list[str] = field(default_factory=list)


@dataclass
class SyntheticWorkflow:
    """Fixture data produced by the Data Wizard + AutoAssign user workflow."""

    data_mod: list[dict[str, Any]]
    data_def: list[dict[str, Any]]
    assign_rules_state: dict[str, Any]
    submodule_ui_states: dict[str, Any]


def create_synthetic_workflow() -> SyntheticWorkflow:
    data_mod = [
        {"Protein": "P001", "Gene": "AAA", "C_1": 10.1, "C_2": 10.4, "ERU_1": 18.9, "ERU_2": 19.2},
        {"Protein": "P002", "Gene": "BBB", "C_1": 8.3, "C_2": 8.1, "ERU_1": 11.7, "ERU_2": 12.0},
        {"Protein": "P003", "Gene": "CCC", "C_1": 14.0, "C_2": 13.8, "ERU_1": 21.4, "ERU_2": 22.1},
    ]
    data_def = [
        {"Column": "Protein", "Content": "Identifier", "Options": "UniProt", "Sample": "", "Options_condition": ""},
        {"Column": "Gene", "Content": "Identifier", "Options": "Gene Symbol", "Sample": "", "Options_condition": ""},
        {"Column": "C_1", "Content": "Normalized Abundance", "Options": "Sample", "Sample": "C_1", "Options_condition": "C"},
        {"Column": "C_2", "Content": "Normalized Abundance", "Options": "Sample", "Sample": "C_2", "Options_condition": "C"},
        {"Column": "ERU_1", "Content": "Normalized Abundance", "Options": "Sample", "Sample": "ERU_1", "Options_condition": "ERU"},
        {"Column": "ERU_2", "Content": "Normalized Abundance", "Options": "Sample", "Sample": "ERU_2", "Options_condition": "ERU"},
    ]
    assign_rules_state = {
        "counter_condition": 2,
        "Options_condition": ["C", "ERU"],
        "text_inputs": {"textin1": "C", "textin2": "ERU"},
    }
    submodule_ui_states = {
        "submodules": {
            "loader_out": {"ui_inputs": {"sheetDropdown": "Synthetic primary", "header_primary": 1}},
            "auto_assign_out": {
                "ui_inputs": {"lookup_col": "Column", "include_pattern": "^(C|ERU)_", "condition_regex": "^(C|ERU)"},
                "extra": {"current_loaded": {"condition_rules": [{"pattern": "^C_", "condition": "C"}, {"pattern": "^ERU_", "condition": "ERU"}]}},
            },
            "assign_rules_out": {
                "ui_inputs": assign_rules_state["text_inputs"],
                "extra": {"counter_condition": 2, "Options_condition": ["C", "ERU"]},
            },
            "filtering_out": {"ui_inputs": {"condition_filter": ["C", "ERU"]}},
            "imputation_out": {"ui_inputs": {"imputation_method": "none"}},
        }
    }
    return SyntheticWorkflow(data_mod, data_def, assign_rules_state, submodule_ui_states)


def colnames(data_mod: list[dict[str, Any]]) -> list[str]:
    return list(data_mod[0].keys()) if data_mod else []


def dimensions(table: list[dict[str, Any]]) -> tuple[int, int]:
    return (len(table), len(table[0]) if table else 0)


def apply_autoassign_rules(workflow: SyntheticWorkflow) -> None:
    """Assert and normalize the fixture like AutoAssign's condition-rule pass."""
    for row in workflow.data_def:
        column = row["Column"]
        if column.startswith("C_"):
            row["Options_condition"] = "C"
        elif column.startswith("ERU_"):
            row["Options_condition"] = "ERU"
    conditions = sorted({row["Options_condition"] for row in workflow.data_def if row["Options_condition"]})
    workflow.assign_rules_state["Options_condition"] = conditions
    workflow.assign_rules_state["counter_condition"] = len(conditions)
    workflow.submodule_ui_states["submodules"]["assign_rules_out"]["extra"] = dict(workflow.assign_rules_state)


def metadata_is_meaningful(data_def: list[dict[str, Any]]) -> bool:
    content_values = {str(row.get("Content", "")).strip().lower() for row in data_def}
    conditions = {str(row.get("Options_condition", "")).strip() for row in data_def}
    return bool(content_values - {"", "row index"}) and EXPECTED_CONDITIONS.issubset(conditions)


def save_data_metadata_snapshot(source: ServerState, workflow: SyntheticWorkflow) -> dict[str, Any]:
    """Build a Data & Metadata envelope with only the Data Wizard snapshot."""
    datawizard_snapshot = {
        "version": "2.0",
        "save_level": DATA_ONLY,
        "primary_data_raw": workflow.data_mod,
        "data_mod": workflow.data_mod,
        "data_def": workflow.data_def,
        "handson_metadata": workflow.data_def,
        "final_processed_data": workflow.data_mod,
        "final_processed_metadata": workflow.data_def,
        "condition_groups": sorted(EXPECTED_CONDITIONS),
        "assign_rules_state": workflow.assign_rules_state,
        "submodule_ui_states": workflow.submodule_ui_states,
        "loader_state": workflow.submodule_ui_states["submodules"]["loader_out"],
    }
    return {
        "version": "3.0.0",
        "save_level": DATA_ONLY,
        "rv_snapshot": {
            "data_mod": workflow.data_mod,
            "data_def": workflow.data_def,
        },
        "module_snapshots": {"datawizard": datawizard_snapshot},
        "plot_data_cache_index": {},
        "manifest": {"module_ids": ["datawizard"], "save_level": DATA_ONLY},
    }


def restore_data_metadata_snapshot(snapshot: dict[str, Any], target: ServerState) -> None:
    modules = snapshot.get("module_snapshots", {})
    for module_id, payload in modules.items():
        target.restored_modules.append(module_id)
        if module_id != "datawizard":
            target.analysis_restore_calls.append(module_id)
            continue
        target.rv["data_mod"] = payload["data_mod"]
        target.rv["data_def"] = payload["data_def"]
        target.rv["assign_rules"] = payload["assign_rules_state"]
        target.datawizard_ui_payloads = payload["submodule_ui_states"]["submodules"]


def assert_data_metadata_workflow_harness() -> None:
    workflow = create_synthetic_workflow()
    original_data_dims = dimensions(workflow.data_mod)
    original_def_dims = dimensions(workflow.data_def)

    apply_autoassign_rules(workflow)

    assert metadata_is_meaningful(workflow.data_def), "metadata must contain non-placeholder content and C/ERU conditions"
    assert EXPECTED_CONDITIONS == set(workflow.assign_rules_state["Options_condition"])
    assert workflow.assign_rules_state["counter_condition"] == 2
    assert [row["Column"] for row in workflow.data_def] == colnames(workflow.data_mod)

    dirty_source = ServerState(rv={"plot_data_cache_pool": {"stale-analysis-cache": object()}})
    snapshot = save_data_metadata_snapshot(dirty_source, workflow)

    module_ids = set(snapshot["module_snapshots"])
    assert "datawizard" in module_ids, "Data Wizard module snapshot must exist"
    assert module_ids.isdisjoint(ANALYSIS_MODULES), "non-Data-Wizard module snapshots must be absent"
    assert "plot_data_cache_pool" not in snapshot or snapshot["plot_data_cache_pool"] in (None, {}), (
        "plot_data_cache_pool must be absent or empty for Data & Metadata saves"
    )
    assert "plot_data_cache_pool" not in snapshot["rv_snapshot"], (
        "stale source plot_data_cache_pool must not be persisted in the restored rv snapshot"
    )

    fresh = ServerState()
    restore_data_metadata_snapshot(snapshot, fresh)

    assert dimensions(fresh.rv["data_mod"]) == original_data_dims, "rv$data_mod dimensions must match original"
    assert dimensions(fresh.rv["data_def"]) == original_def_dims, "rv$data_def dimensions must match original"
    assert [row["Column"] for row in fresh.rv["data_def"]] == colnames(fresh.rv["data_mod"]), (
        "rv$data_def$Column must match names(rv$data_mod)"
    )
    assert fresh.rv["assign_rules"]["counter_condition"] == workflow.assign_rules_state["counter_condition"]
    assert EXPECTED_CONDITIONS.issubset(set(fresh.rv["assign_rules"]["Options_condition"])), (
        "Assign Rules Options_condition must include C and ERU"
    )
    assert DATAWIZARD_SUBMODULES.issubset(set(fresh.datawizard_ui_payloads)), (
        "Data Wizard submodule UI payloads must be restored"
    )
    assert fresh.analysis_restore_calls == [], "no analysis plot/UI module restore may be performed"
    assert fresh.restored_modules == ["datawizard"], "restore should dispatch only Data Wizard for Data & Metadata mode"


def assert_restore_source_contains_data_metadata_contracts() -> None:
    helpers = CORE_HELPERS.read_text()
    orchestration = ORCHESTRATION.read_text()
    registration = REGISTRATION.read_text()

    assert "Data & Metadata (SESSION_SAVE_LEVEL_DATA / \"data_only\")" in helpers
    assert "Save the complete Data Wizard module and submodule UI/settings" in helpers
    assert "Do not save non-Data-Wizard module UI, plots, or visualization state" in helpers
    assert "Do not build plot-data cache pools" in helpers
    assert "if (identical(save_level, SESSION_SAVE_LEVEL_DATA))" in orchestration
    assert "[SaveStage:plot_data_cache_pool] skipped for data-only save level" in orchestration
    assert "if (!identical(save_level, SESSION_SAVE_LEVEL_DATA))" in orchestration
    assert "envelope$plot_data_cache_pool" in orchestration
    assert "qs_preset = \"balanced\"" in helpers
    assert "transport_preset = qs_preset" in helpers
    assert "transport        = if (qs_available) \"qs\" else \"inline_rds\"" in helpers
    assert "message(" in helpers and "using inline RDS fallback transport" in helpers
    assert ".session_rds_compress_for_transport_preset" in helpers
    assert "qs::qserialize(payload, preset = qs_preset)" in helpers
    assert "getOption(\"miraprot.session_qs_preset_data\", \"fast\")" in orchestration
    assert "qs_preset        = session_qs_preset" in orchestration
    assert "rds_compress <- .session_rds_compress_for_transport_preset" in orchestration
    assert "saveRDS(envelope, file = file, compress = rds_compress)" in orchestration
    assert "Data & Metadata saves are intentionally scoped to the Data Wizard" in registration
    assert "state$submodule_ui_states <- set_field(\"submodule_ui_states\"" in registration
    assert ".safe_fn_call(dw$get_all_submodule_ui_states)" in registration
    assert "extract_saved_condition_choices" in registration
    assert "assign_rules_payload$extra$Options_condition" in registration
    assert ".safe_fn_call_arg(dw$set_all_submodule_ui_states, submodule_state_payload)" in registration


if __name__ == "__main__":
    assert_data_metadata_workflow_harness()
    assert_restore_source_contains_data_metadata_contracts()
    print("Data & Metadata workflow harness checks passed")
