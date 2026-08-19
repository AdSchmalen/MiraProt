#!/usr/bin/env python3
"""Restore preprocessing harness for .resolve_plot_data_cache_for_module save levels.

The static/restore harness mirrors the server-side restore preprocessing path that
materializes shared plot-data caches before module restore callbacks run.  It
covers every supported save level with synthetic snapshot shapes and verifies
that a missing or stale module-level plot_data_cache_ref degrades in a controlled
way or resolves through the singleton-pool fallback, never as an upload-level
fatal error.
"""

from __future__ import annotations

from copy import deepcopy
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
CORE_HELPERS = ROOT / "R" / "session_save_restore" / "session_save_restore_core_helpers.R"
ORCHESTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_orchestration.R"

DATA_ONLY = "data_only"
DATA_AND_ANALYSIS = "data_and_analysis"
FULL_SESSION = "full_session"
SHARED_POOL_DEPENDENCY = "shared_plot_data_cache_pool"


def representative_cache_pair() -> dict[str, list[dict[str, Any]]]:
    data_mod = [
        {"Protein": "P001", "Gene": "STAT1", "C_1": 10.1, "ERU_1": 18.9},
        {"Protein": "P002", "Gene": "MYC", "C_1": 8.3, "ERU_1": 11.7},
    ]
    data_def = [
        {"Column": "Protein", "Content": "Identifier", "Options_condition": ""},
        {"Column": "Gene", "Content": "Identifier", "Options_condition": ""},
        {"Column": "C_1", "Content": "Normalized Abundance", "Options_condition": "C"},
        {"Column": "ERU_1", "Content": "Normalized Abundance", "Options_condition": "ERU"},
    ]
    return {"data_mod": data_mod, "data_def": data_def}


def datawizard_state(save_level: str) -> dict[str, Any]:
    pair = representative_cache_pair()
    return {
        "version": "2.0",
        "save_level": save_level,
        "data_mod": pair["data_mod"],
        "data_def": pair["data_def"],
        "submodule_ui_states": {"auto_assign_out": {"ui_inputs": {"condition_regex": "^(C|ERU)_"}}},
    }


def analysis_state(module_id: str) -> dict[str, Any]:
    return {
        "version": "2.0",
        "session_safe": True,
        "module_state": {
            "result_dataframe": [{"ID": f"{module_id.upper()}_TERM", "p.adjust": 0.01}],
            "ui_settings": {"selected_term": f"{module_id.upper()}_TERM"},
        },
    }


def full_module_state(stale_or_missing_ref: str | None) -> dict[str, Any]:
    state = {
        "restore_cache_dependency": SHARED_POOL_DEPENDENCY,
        "ui_settings": {"selected_plot": "PCA scores"},
        "plot_recreation_state": {"plot_kind": "pca"},
    }
    if stale_or_missing_ref is not None:
        state["plot_data_cache_ref"] = stale_or_missing_ref
    return {"version": "2.0", "module_state": state}


def snapshot_shape(save_level: str, *, full_ref: str | None = "stale-cache-ref") -> dict[str, Any]:
    modules: dict[str, Any] = {"datawizard": datawizard_state(save_level)}
    if save_level in {DATA_AND_ANALYSIS, FULL_SESSION}:
        modules.update({"go": analysis_state("go"), "gsea": analysis_state("gsea")})
    if save_level == FULL_SESSION:
        modules["pca"] = full_module_state(full_ref)
    return {
        "version": "3.0.0",
        "save_level": save_level,
        "module_snapshots": modules,
        "plot_data_cache_pool": {"singleton-cache": representative_cache_pair()},
        "manifest": {"module_ids": list(modules), "save_level": save_level},
    }


def is_plot_cache_pair(value: Any) -> bool:
    return isinstance(value, dict) and isinstance(value.get("data_mod"), list) and isinstance(value.get("data_def"), list)


def resolve_plot_data_cache_for_module(module_state: dict[str, Any], pool: dict[str, Any]) -> dict[str, Any]:
    """Python model of .resolve_plot_data_cache_for_module singleton fallback."""
    resolved = deepcopy(module_state)
    ref = resolved.get("plot_data_cache_ref")
    cached = pool.get(ref) if isinstance(ref, str) else None
    if not is_plot_cache_pair(cached) and resolved.get("restore_cache_dependency") == SHARED_POOL_DEPENDENCY:
        if len(pool) == 1:
            pool_id, sole = next(iter(pool.items()))
            if is_plot_cache_pair(sole):
                cached = sole
                resolved["plot_data_cache_ref"] = pool_id
                resolved["restore_cache_resolution_mode"] = "singleton_pool_fallback"
    if is_plot_cache_pair(cached):
        resolved["restore_plot_data_cache"] = cached
        resolved["restore_cache_degraded"] = False
    return resolved


def preprocess_restore_snapshot(snapshot: dict[str, Any]) -> tuple[dict[str, Any], list[str]]:
    processed = deepcopy(snapshot)
    logs: list[str] = []
    pool = processed.get("plot_data_cache_pool") or {}
    try:
        for module_id, payload in processed.get("module_snapshots", {}).items():
            state = payload.get("module_state")
            if not isinstance(state, dict):
                logs.append(f"Restore preprocess [{module_id}]: no module_state cache dependency")
                continue
            if state.get("restore_cache_dependency") != SHARED_POOL_DEPENDENCY:
                logs.append(f"Restore preprocess [{module_id}]: no shared plot-data cache dependency")
                continue
            resolved_state = resolve_plot_data_cache_for_module(state, pool)
            has_cache = is_plot_cache_pair(resolved_state.get("restore_plot_data_cache"))
            if not has_cache:
                resolved_state["restore_cache_degraded"] = True
                resolved_state["restore_cache_degraded_reason"] = "cache_ref_unresolved_no_valid_pool_entry"
                mode = "degraded_cache_ref_unresolved"
                logs.append(f"Restore preprocess [{module_id}] degraded: cache_ref_unresolved_no_valid_pool_entry")
            else:
                mode = resolved_state.get("restore_cache_resolution_mode") or "module_ref"
                logs.append(f"Restore preprocess [{module_id}]: restore_cache_resolution_mode={mode}")
            resolved_state["restore_cache_resolved"] = has_cache
            resolved_state["restore_cache_resolution_mode"] = mode
            payload["module_state"] = resolved_state
    except NameError as exc:  # would indicate the harness called an undefined helper
        logs.append(f"upload_fatal: undefined helper {exc}")
    return processed, logs


def assert_no_upload_level_fatal(logs: list[str]) -> None:
    fatal_logs = [line for line in logs if "upload_fatal" in line or "undefined helper" in line]
    assert not fatal_logs, f"restore preprocessing must not fail at upload level: {fatal_logs}"


def test_resolve_plot_data_cache_for_module_save_levels_static_restore_harness() -> None:
    for save_level in (DATA_ONLY, DATA_AND_ANALYSIS, FULL_SESSION):
        processed, logs = preprocess_restore_snapshot(snapshot_shape(save_level))
        assert_no_upload_level_fatal(logs)
        assert processed["save_level"] == save_level

    data_only = preprocess_restore_snapshot(snapshot_shape(DATA_ONLY))[0]
    assert set(data_only["module_snapshots"]) == {"datawizard"}
    assert "restore_cache_resolved" not in data_only["module_snapshots"]["datawizard"]

    data_and_analysis = preprocess_restore_snapshot(snapshot_shape(DATA_AND_ANALYSIS))[0]
    assert set(data_and_analysis["module_snapshots"]) == {"datawizard", "go", "gsea"}
    assert all(
        "restore_cache_resolved" not in data_and_analysis["module_snapshots"][mid].get("module_state", {})
        for mid in ("go", "gsea")
    )

    full_session, logs = preprocess_restore_snapshot(snapshot_shape(FULL_SESSION, full_ref="mismatched-cache-ref"))
    pca_state = full_session["module_snapshots"]["pca"]["module_state"]
    assert_no_upload_level_fatal(logs)
    assert pca_state["restore_cache_resolved"] is True
    assert pca_state["restore_cache_resolution_mode"] == "singleton_pool_fallback"
    assert pca_state["plot_data_cache_ref"] == "singleton-cache"
    assert is_plot_cache_pair(pca_state["restore_plot_data_cache"])

    missing_ref_session, logs = preprocess_restore_snapshot(snapshot_shape(FULL_SESSION, full_ref=None))
    missing_ref_state = missing_ref_session["module_snapshots"]["pca"]["module_state"]
    assert_no_upload_level_fatal(logs)
    assert missing_ref_state["restore_cache_resolved"] is True
    assert missing_ref_state["restore_cache_resolution_mode"] == "singleton_pool_fallback"


def test_resolve_plot_data_cache_for_module_source_guards_against_undefined_helpers() -> None:
    helpers = CORE_HELPERS.read_text()
    orchestration = ORCHESTRATION.read_text()
    resolver = helpers.split(".resolve_plot_data_cache_for_module <- function", 1)[1].split(
        "# Build a lightweight index describing the plot-data cache pool.", 1
    )[0]
    assert ".resolve_plot_data_cache_for_module(st, plot_data_cache_pool)" in orchestration
    assert ".is_valid_pair" not in resolver, "regression guard: stale undefined helper must not be called"
    assert ".is_plot_cache_pair(cached)" in resolver
    assert "singleton_pool_fallback" in resolver


if __name__ == "__main__":
    test_resolve_plot_data_cache_for_module_save_levels_static_restore_harness()
    test_resolve_plot_data_cache_for_module_source_guards_against_undefined_helpers()
    print(".resolve_plot_data_cache_for_module save-level restore harness checks passed")
