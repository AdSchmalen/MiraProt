#!/usr/bin/env python3
"""Full-session scripted restore harness covering downstream module contracts.

This harness models the complete user workflow without launching Shiny:
representative data/metadata are loaded, AutoAssign creates canonical metadata,
GO and GSEA are restored before visual modules, downstream module plot/session
state is saved without serialized plot objects, and a fresh server state rebuilds
plots from saved settings plus cache-referenced data.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from time import perf_counter
from typing import Any, Callable

ROOT = Path(__file__).resolve().parents[2]
ORCHESTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_orchestration.R"
REGISTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_module_registration.R"
DATAWIZARD_CORE = ROOT / "modules" / "Data Wizard" / "core" / "datawizard_core_submodule_session.R"

FULL_SESSION = "full"
DATAWIZARD = "datawizard"
GO_GSEA = {"go", "gsea"}
DOWNSTREAM = ["abundances", "sampleids", "pca", "volcano", "dotplot", "string", "venn", "heatmap"]
RESTORE_PRIORITIES = {
    DATAWIZARD: 10,
    "go": 30,
    "gsea": 31,
    "abundances": 40,
    "sampleids": 41,
    "pca": 42,
    "volcano": 43,
    "dotplot": 44,
    "string": 45,
    "venn": 46,
    "heatmap": 47,
}
TIMING_BUDGET_MS = {
    DATAWIZARD: 10.0,
    "go": 8.0,
    "gsea": 8.0,
    "abundances": 8.0,
    "sampleids": 8.0,
    "pca": 8.0,
    "volcano": 8.0,
    "dotplot": 8.0,
    "string": 8.0,
    "venn": 8.0,
    "heatmap": 8.0,
}


@dataclass
class TimedCall:
    module_id: str
    phase: str
    elapsed_ms: float


@dataclass
class ModuleAdapter:
    module_id: str
    priority: int
    save_state: Callable[[], dict[str, Any]]
    restore_state: Callable[[dict[str, Any], "FreshServer"], None]


@dataclass
class FreshServer:
    rv: dict[str, Any] = field(default_factory=dict)
    restored_order: list[str] = field(default_factory=list)
    restored_ui_values: dict[str, dict[str, Any]] = field(default_factory=dict)
    restored_labels: dict[str, str] = field(default_factory=dict)
    recreated_plots: dict[str, dict[str, Any]] = field(default_factory=dict)
    restore_poll_counts: dict[str, int] = field(default_factory=dict)
    primary_working: list[dict[str, Any]] | None = None
    metadata_sync_attempts: list[dict[str, Any]] = field(default_factory=list)
    rejected_publications: list[dict[str, Any]] = field(default_factory=list)
    session_restore_phase: str = "restoring_modules"
    timings: list[TimedCall] = field(default_factory=list)


def representative_data() -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    data_mod = [
        {"Protein": "P001", "Gene": "STAT1", "C_1": 10.1, "C_2": 10.4, "ERU_1": 18.9, "ERU_2": 19.2, "logFC": 1.4, "p_adj": 0.001},
        {"Protein": "P002", "Gene": "MYC", "C_1": 8.3, "C_2": 8.1, "ERU_1": 11.7, "ERU_2": 12.0, "logFC": -0.9, "p_adj": 0.018},
        {"Protein": "P003", "Gene": "JUN", "C_1": 14.0, "C_2": 13.8, "ERU_1": 21.4, "ERU_2": 22.1, "logFC": 1.7, "p_adj": 0.006},
    ]
    data_def = [{"Column": col, "Content": "Identifier", "Options_condition": ""} for col in ["Protein", "Gene"]]
    data_def.extend(
        {"Column": col, "Content": "Normalized Abundance", "Options_condition": ""}
        for col in ["C_1", "C_2", "ERU_1", "ERU_2"]
    )
    data_def.extend(
        {"Column": col, "Content": "Statistic", "Options_condition": ""} for col in ["logFC", "p_adj"]
    )
    return data_mod, data_def


def historical_plot_data() -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    """Return a deliberately incompatible, older plotting pair (pair B)."""
    data_mod = [
        {"LegacyID": "OLD-1", "Baseline": 2.0, "Treated": 7.0, "LegacyFC": 1.8, "LegacyQ": 0.04},
        {"LegacyID": "OLD-2", "Baseline": 6.0, "Treated": 3.0, "LegacyFC": -1.0, "LegacyQ": 0.08},
    ]
    data_def = [
        {"Column": "LegacyID", "Content": "Identifier", "Options_condition": ""},
        {"Column": "Baseline", "Content": "Normalized Abundance", "Options_condition": "OLD_C"},
        {"Column": "Treated", "Content": "Normalized Abundance", "Options_condition": "OLD_T"},
        {"Column": "LegacyFC", "Content": "Statistic", "Options_condition": ""},
        {"Column": "LegacyQ", "Content": "Statistic", "Options_condition": ""},
    ]
    return data_mod, data_def


def ordered_columns(data_mod: list[dict[str, Any]]) -> list[str]:
    return list(data_mod[0])


def autoassign_metadata(data_def: list[dict[str, Any]]) -> None:
    for row in data_def:
        if row["Column"].startswith("C_"):
            row["Options_condition"] = "C"
        elif row["Column"].startswith("ERU_"):
            row["Options_condition"] = "ERU"


def cache_id(data_mod: list[dict[str, Any]], data_def: list[dict[str, Any]]) -> str:
    cols = ",".join(data_mod[0])
    return f"cache-{len(data_mod)}x{len(data_mod[0])}-{len(data_def)}-{cols}"


def datawizard_snapshot(data_mod: list[dict[str, Any]], data_def: list[dict[str, Any]]) -> dict[str, Any]:
    return {
        "version": "2.0",
        "save_level": FULL_SESSION,
        "data_mod": data_mod,
        "data_def": data_def,
        "assign_rules_state": {"Options_condition": ["C", "ERU"], "counter_condition": 2},
        "submodule_ui_states": {"auto_assign_out": {"ui_inputs": {"condition_regex": "^(C|ERU)_"}}},
    }


def analysis_snapshot(module_id: str, plot_type: str) -> dict[str, Any]:
    return {
        "version": "2.0",
        "session_safe": True,
        "session_payload_shape": "analysis_result_plus_ui_v1",
        "contains_plot_object": False,
        "module_state": {
            "result_dataframe": [{"ID": f"{module_id.upper()}_TERM", "p.adjust": 0.01}],
            "ui_settings": {f"selected_{module_id}": f"{module_id.upper()}_TERM"},
            "plot_recreation_state": {"plot_type": plot_type, "selected": f"{module_id.upper()}_TERM"},
        },
    }


def downstream_snapshot(module_id: str, ref: str) -> dict[str, Any]:
    selected = {
        "abundances": "Abundance profile",
        "sampleids": "Sample ID overview",
        "pca": "PCA scores",
        "volcano": "ERU vs C volcano",
        "dotplot": "GO dotplot",
        "string": "STRING network",
        "venn": "Regulated overlap",
        "heatmap": "Expression heatmap",
    }[module_id]
    return {
        "version": "2.0",
        "module_state": {
            "restore_cache_dependency": "shared_plot_data_cache_pool",
            "plot_data_cache_ref": ref,
            "ui_settings": {"selected_plot": selected, "condition_a": "ERU", "condition_b": "C"},
            "plot_recreation_state": {"plot_kind": module_id, "selected_plot": selected},
            "labels": {"title": selected, "x": "C", "y": "ERU"},
        },
    }


def assert_exported_downstream_cache_contract(snapshots: dict[str, dict[str, Any]], module_ids: list[str]) -> None:
    forbidden_embedded_fields = {"plot_data_cache_payload", "restore_plot_data_cache", "data_mod", "data_def"}
    for module_id in module_ids:
        module_state = snapshots[module_id]["module_state"]
        assert module_state.get("restore_cache_dependency") == "shared_plot_data_cache_pool"
        assert module_state.get("plot_data_cache_ref"), f"{module_id} must retain plot_data_cache_ref after export"
        embedded = forbidden_embedded_fields.intersection(module_state)
        assert not embedded, f"{module_id} exported embedded data/cache fields: {sorted(embedded)}"

def contains_forbidden_plot_object(x: Any) -> bool:
    if isinstance(x, dict):
        if x.get("class") in {"ggplot", "plotly", "htmlwidget"}:
            return True
        return any(k in {"plot_object", "ggplot", "plotly", "current_plot"} or contains_forbidden_plot_object(v) for k, v in x.items())
    if isinstance(x, list):
        return any(contains_forbidden_plot_object(v) for v in x)
    return False


def collect_snapshots(adapters: list[ModuleAdapter]) -> dict[str, dict[str, Any]]:
    return {a.module_id: a.save_state() for a in sorted(adapters, key=lambda a: (a.priority, a.module_id))}


def restore_snapshots(adapters: list[ModuleAdapter], snapshots: dict[str, dict[str, Any]], server: FreshServer) -> None:
    for adapter in sorted(adapters, key=lambda a: (a.priority, a.module_id)):
        if adapter.module_id not in snapshots:
            continue
        start = perf_counter()
        adapter.restore_state(snapshots[adapter.module_id], server)
        server.timings.append(TimedCall(adapter.module_id, "restore", (perf_counter() - start) * 1000))
        server.restored_order.append(adapter.module_id)
    server.session_restore_phase = "complete"


def restore_datawizard(state: dict[str, Any], server: FreshServer) -> None:
    server.rv["data_mod"] = state["data_mod"]
    server.rv["data_def"] = state["data_def"]
    server.primary_working = state["data_mod"]
    server.rv["canonical_data_ready"] = True
    server.restored_ui_values[DATAWIZARD] = state["submodule_ui_states"]["auto_assign_out"]["ui_inputs"]


def replay_datawizard_publication(candidate_data: list[dict[str, Any]], server: FreshServer) -> None:
    """Model the replay callback's guard before publishing a working dataset.

    A restored downstream cache can wake a Data Wizard callback.  The callback
    may inspect that historical data, but it must not pair it with current
    canonical metadata or publish it as the latest primary working dataset.
    """
    canonical_metadata = server.rv["data_def"]
    candidate_columns = ordered_columns(candidate_data)
    metadata_columns = [row["Column"] for row in canonical_metadata]
    if server.session_restore_phase != "complete" or candidate_columns != metadata_columns:
        server.rejected_publications.append(
            {"data_columns": candidate_columns, "metadata_columns": metadata_columns}
        )
        return

    server.metadata_sync_attempts.append(
        {"data_mod": candidate_data, "data_def": canonical_metadata}
    )
    server.rv["data_mod"] = candidate_data
    server.primary_working = candidate_data


def restore_analysis(module_id: str) -> Callable[[dict[str, Any], FreshServer], None]:
    def _restore(state: dict[str, Any], server: FreshServer) -> None:
        assert server.rv.get("canonical_data_ready") is True, f"{module_id} restored before canonical data"
        module_state = state["module_state"]
        server.rv[f"{module_id}_results"] = module_state["result_dataframe"]
        server.restored_ui_values[module_id] = module_state["ui_settings"]
        server.recreated_plots[module_id] = {
            "source": "settings/data",
            "plot_type": module_state["plot_recreation_state"]["plot_type"],
            "result_count": len(module_state["result_dataframe"]),
        }
    return _restore


def restore_downstream(module_id: str, cache_pool: dict[str, Any]) -> Callable[[dict[str, Any], FreshServer], None]:
    def _restore(state: dict[str, Any], server: FreshServer) -> None:
        assert server.rv.get("canonical_data_ready") is True, f"{module_id} restored before canonical data"
        assert {"go", "gsea"}.issubset(set(server.restored_order)), f"{module_id} restored before GO/GSEA"
        module_state = state["module_state"]
        ref = module_state["plot_data_cache_ref"]
        assert ref in cache_pool, f"{module_id} cache ref must resolve"
        server.restore_poll_counts[module_id] = server.restore_poll_counts.get(module_id, 0) + 1
        server.restored_ui_values[module_id] = module_state["ui_settings"]
        server.restored_labels[module_id] = module_state["labels"]["title"]
        server.recreated_plots[module_id] = {
            "source": "settings/data",
            "plot_kind": module_state["plot_recreation_state"]["plot_kind"],
            "rows": len(cache_pool[ref]["data_mod"]),
            "columns": ordered_columns(cache_pool[ref]["data_mod"]),
            "title": module_state["labels"]["title"],
        }
        # This closely models the restore-triggered Data Wizard replay: the
        # historical pair is observable, then meets the guarded publication
        # boundary before it could synchronize canonical state.
        replay_datawizard_publication(cache_pool[ref]["data_mod"], server)
    return _restore


def test_full_session_restore_contract_for_downstream_modules() -> None:
    canonical_data_a, canonical_metadata_a = representative_data()
    historical_data_b, historical_metadata_b = historical_plot_data()
    autoassign_metadata(canonical_metadata_a)
    assert {r["Options_condition"] for r in canonical_metadata_a if r["Options_condition"]} == {"C", "ERU"}
    assert len(ordered_columns(canonical_data_a)) != len(ordered_columns(historical_data_b))
    assert ordered_columns(canonical_data_a) != ordered_columns(historical_data_b)

    ref = cache_id(historical_data_b, historical_metadata_b)
    cache_pool = {ref: {"data_mod": historical_data_b, "data_def": historical_metadata_b}}
    adapters = [ModuleAdapter(DATAWIZARD, RESTORE_PRIORITIES[DATAWIZARD], lambda: datawizard_snapshot(canonical_data_a, canonical_metadata_a), restore_datawizard)]
    adapters.extend(
        [
            ModuleAdapter("gsea", RESTORE_PRIORITIES["gsea"], lambda: analysis_snapshot("gsea", "ridgeplot"), restore_analysis("gsea")),
            ModuleAdapter("go", RESTORE_PRIORITIES["go"], lambda: analysis_snapshot("go", "dotplot"), restore_analysis("go")),
        ]
    )
    adapters.extend(
        ModuleAdapter(mid, RESTORE_PRIORITIES[mid], lambda mid=mid: downstream_snapshot(mid, ref), restore_downstream(mid, cache_pool))
        for mid in DOWNSTREAM
    )

    snapshots = collect_snapshots(list(reversed(adapters)))
    envelope = {
        "version": "3.0.0",
        "save_level": FULL_SESSION,
        "module_snapshots": snapshots,
        "plot_data_cache_pool": cache_pool,
        "manifest": {"module_ids": list(snapshots), "cache_ref_integrity": {"valid": True, "missing_refs": []}},
    }

    assert list(snapshots) == [DATAWIZARD, "go", "gsea", *DOWNSTREAM]
    assert all(not contains_forbidden_plot_object(snapshot) for snapshot in snapshots.values())
    assert_exported_downstream_cache_contract(snapshots, ["abundances", "sampleids", "pca", "volcano", "dotplot", "venn"])
    assert envelope["manifest"]["cache_ref_integrity"] == {"valid": True, "missing_refs": []}

    fresh = FreshServer()
    restore_snapshots(list(reversed(adapters)), snapshots, fresh)

    assert fresh.restored_order == [DATAWIZARD, "go", "gsea", *DOWNSTREAM]
    assert fresh.restored_order.index(DATAWIZARD) < min(fresh.restored_order.index(mid) for mid in GO_GSEA | set(DOWNSTREAM))
    assert max(fresh.restored_order.index(mid) for mid in GO_GSEA) < min(fresh.restored_order.index(mid) for mid in DOWNSTREAM)
    assert set(fresh.recreated_plots) == {"go", "gsea", *DOWNSTREAM}
    assert all(plot["source"] == "settings/data" for plot in fresh.recreated_plots.values())
    assert fresh.restored_ui_values["volcano"]["selected_plot"] == "ERU vs C volcano"
    assert fresh.restored_ui_values["pca"]["condition_a"] == "ERU"
    assert fresh.restored_labels["heatmap"] == "Expression heatmap"
    assert fresh.restore_poll_counts == {mid: 1 for mid in DOWNSTREAM}
    assert fresh.rv["data_mod"] == canonical_data_a
    assert fresh.rv["data_def"] == canonical_metadata_a
    assert [row["Column"] for row in fresh.rv["data_def"]] == ordered_columns(canonical_data_a)
    assert fresh.primary_working == canonical_data_a
    assert all(
        fresh.recreated_plots[mid]["columns"] == ordered_columns(historical_data_b)
        for mid in DOWNSTREAM
    )
    assert fresh.metadata_sync_attempts == [], "historical B must never be synchronized with canonical A metadata"
    assert len(fresh.rejected_publications) == len(DOWNSTREAM)
    assert all(
        rejection == {
            "data_columns": ordered_columns(historical_data_b),
            "metadata_columns": ordered_columns(canonical_data_a),
        }
        for rejection in fresh.rejected_publications
    )
    for timing in fresh.timings:
        assert timing.elapsed_ms <= TIMING_BUDGET_MS[timing.module_id], (
            f"{timing.module_id} restore took {timing.elapsed_ms:.3f}ms, "
            f"budget is {TIMING_BUDGET_MS[timing.module_id]:.3f}ms"
        )


def test_restore_source_contains_fast_schema_and_bounded_retry_guards() -> None:
    orchestration = ORCHESTRATION.read_text()
    registration = REGISTRATION.read_text()
    datawizard_core = DATAWIZARD_CORE.read_text()

    assert "schema_shallow" in orchestration, "known v2 snapshots should avoid legacy deep sanitizer walks"
    assert ".sanitize_schema_v2_module_snapshot" in orchestration
    assert "[SaveStage:sanitization:module]" in orchestration and "elapsed_ms=" in orchestration
    assert "cache_ref_integrity" in orchestration and "integrity_missing_refs" in orchestration
    assert "priority = 10L" in registration, "Data Wizard must be registered before analysis modules"
    assert "module_id = \"datawizard\"" in registration
    assert "max_restore_attempts = 5L" in datawizard_core
    assert "attempt + 1L >= max_restore_attempts" in datawizard_core
    assert "restore warning: dropping unresolved inputs after " in datawizard_core


if __name__ == "__main__":
    test_full_session_restore_contract_for_downstream_modules()
    test_restore_source_contains_fast_schema_and_bounded_retry_guards()
    print("Full-session downstream restore harness checks passed")
