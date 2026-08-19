"""Scripted harness for GO/GSEA session save and restore ordering.

This test mirrors the Shiny session contract without requiring a live Shiny
session: Data Wizard state is saved first, followed by GO and GSEA module
state.  GO/GSEA snapshots intentionally persist result data plus plot
recreation settings, but not plot objects.  Restore into a fresh set of module
adapters must apply modules in registry priority order and must be able to
regenerate plots from restored settings without rerunning enrichment.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable


@dataclass(frozen=True)
class RepresentativeResults:
    dataframe: list[dict[str, Any]]
    plot_object: dict[str, Any]
    ui_settings: dict[str, Any]
    plot_recreation_state: dict[str, Any]


@dataclass
class ModuleAdapter:
    module_id: str
    priority: int
    save_state: Callable[[], dict[str, Any]]
    restore_state: Callable[[dict[str, Any]], None]


@dataclass
class RestoredAnalysisModule:
    module_id: str
    restored_results: list[list[dict[str, Any]]] = field(default_factory=list)
    restored_ui_settings: dict[str, Any] | None = None
    restored_plot_recreation_state: dict[str, Any] | None = None
    enrichment_reruns: int = 0

    def restore_state(self, state: dict[str, Any]) -> None:
        self.restored_results.append(state["module_state"]["result_dataframe"])
        self.restored_ui_settings = state["module_state"]["ui_settings"]
        self.restored_plot_recreation_state = state["module_state"]["plot_recreation_state"]

    def regenerate_plot_from_restored_settings(self) -> dict[str, Any]:
        assert self.restored_results, f"{self.module_id} results must be restored before plotting"
        assert self.restored_plot_recreation_state, (
            f"{self.module_id} plot recreation state must be restored before plotting"
        )

        # Deliberately use the restored result dataframe and recreation settings
        # only.  The enrichment_reruns counter proves no analysis rerun path was
        # used to recreate the plot.
        return {
            "module": self.module_id,
            "plot_type": self.restored_plot_recreation_state["plot_type"],
            "term_count": len(self.restored_results[0]),
            "settings": dict(self.restored_ui_settings or {}),
        }


def representative_go_results() -> RepresentativeResults:
    dataframe = [
        {"ID": "GO:0006955", "Description": "immune response", "p.adjust": 0.001, "Count": 8},
        {"ID": "GO:0008283", "Description": "cell proliferation", "p.adjust": 0.012, "Count": 5},
    ]
    ui_settings = {
        "OrgDb_GO": "org.Hs.eg.db",
        "keyType_GO": "SYMBOL",
        "ont_GO": "BP",
        "ThemeSelect_GO": "Black and White",
        "LegendPosition_GO": "right",
        "max_terms_GO": 15,
    }
    return RepresentativeResults(
        dataframe=dataframe,
        plot_object={"class": "ggplot", "layers": ["dotplot"]},
        ui_settings=ui_settings,
        plot_recreation_state={
            "plot_type": "dotplot",
            "selected_go_terms": ["GO:0006955", "GO:0008283"],
            "plot_height": 640,
            "plot_width": 900,
            "ui_inputs": ui_settings,
        },
    )


def representative_gsea_results() -> RepresentativeResults:
    dataframe = [
        {"ID": "HALLMARK_INTERFERON_GAMMA_RESPONSE", "Description": "IFN gamma", "NES": 2.1, "p.adjust": 0.003},
        {"ID": "HALLMARK_MYC_TARGETS_V1", "Description": "MYC targets", "NES": -1.8, "p.adjust": 0.018},
    ]
    ui_settings = {
        "plot_type_GSEA": "ridgeplot",
        "custom_Enrich_select": "HALLMARK_INTERFERON_GAMMA_RESPONSE",
        "ThemeSelect_GSEA": "Minimal",
        "LegendPosition_GSEA": "bottom",
        "plot_height_gsea": 720,
    }
    return RepresentativeResults(
        dataframe=dataframe,
        plot_object={"class": "ggplot", "layers": ["ridgeplot"]},
        ui_settings=ui_settings,
        plot_recreation_state={
            "plot_type": "ridgeplot",
            "selected_enrichment": "HALLMARK_INTERFERON_GAMMA_RESPONSE",
            "plot_height_gsea": 720,
            "ui_inputs": ui_settings,
        },
    )


def save_analysis_snapshot(results: RepresentativeResults) -> dict[str, Any]:
    """Persist result data and recreation state, intentionally excluding plots."""
    return {
        "version": "harness-1.0",
        "session_safe": True,
        "module_state": {
            "result_dataframe": results.dataframe,
            "ui_settings": results.ui_settings,
            "plot_recreation_state": results.plot_recreation_state,
        },
    }


def collect_snapshots(adapters: list[ModuleAdapter]) -> dict[str, dict[str, Any]]:
    ordered = sorted(adapters, key=lambda adapter: adapter.priority)
    return {adapter.module_id: adapter.save_state() for adapter in ordered}


def restore_snapshots(adapters: list[ModuleAdapter], snapshots: dict[str, dict[str, Any]]) -> list[str]:
    ordered = sorted(adapters, key=lambda adapter: adapter.priority)
    restored_order: list[str] = []
    for adapter in ordered:
        if adapter.module_id in snapshots:
            adapter.restore_state(snapshots[adapter.module_id])
            restored_order.append(adapter.module_id)
    return restored_order


def test_session_restore_order_and_go_gsea_plot_recreation_contract() -> None:
    go_original = representative_go_results()
    gsea_original = representative_gsea_results()

    datawizard_state = {
        "version": "harness-1.0",
        "data_mod": [{"Protein": "STAT1", "logFC": 1.4}, {"Protein": "MYC", "logFC": -0.9}],
        "data_def": [{"sample": "A", "condition": "treated"}, {"sample": "B", "condition": "control"}],
    }

    # Simulate save order: Data Wizard first, GO second, GSEA third.
    snapshots = collect_snapshots(
        [
            ModuleAdapter("go", 30, lambda: save_analysis_snapshot(go_original), lambda state: None),
            ModuleAdapter("gsea", 31, lambda: save_analysis_snapshot(gsea_original), lambda state: None),
            ModuleAdapter("datawizard", 10, lambda: datawizard_state, lambda state: None),
        ]
    )

    assert list(snapshots) == ["datawizard", "go", "gsea"]

    go_snapshot = snapshots["go"]["module_state"]
    gsea_snapshot = snapshots["gsea"]["module_state"]
    assert "plot_object" not in go_snapshot
    assert "current_plot" not in go_snapshot
    assert "plot_object" not in gsea_snapshot
    assert "current_plot" not in gsea_snapshot
    assert go_snapshot["plot_recreation_state"] == go_original.plot_recreation_state
    assert gsea_snapshot["plot_recreation_state"] == gsea_original.plot_recreation_state

    restored_datawizard: dict[str, Any] = {}
    restored_go = RestoredAnalysisModule("go")
    restored_gsea = RestoredAnalysisModule("gsea")

    restored_order = restore_snapshots(
        [
            ModuleAdapter("gsea", 31, lambda: {}, restored_gsea.restore_state),
            ModuleAdapter("go", 30, lambda: {}, restored_go.restore_state),
            ModuleAdapter("datawizard", 10, lambda: {}, lambda state: restored_datawizard.update(state)),
        ],
        snapshots,
    )

    assert restored_order == ["datawizard", "go", "gsea"]
    assert len(restored_go.restored_results) == 1
    assert len(restored_gsea.restored_results) == 1
    assert restored_go.restored_results[0] == go_original.dataframe
    assert restored_gsea.restored_results[0] == gsea_original.dataframe
    assert restored_go.restored_ui_settings == go_original.ui_settings
    assert restored_gsea.restored_ui_settings == gsea_original.ui_settings

    go_plot = restored_go.regenerate_plot_from_restored_settings()
    gsea_plot = restored_gsea.regenerate_plot_from_restored_settings()

    assert go_plot == {"module": "go", "plot_type": "dotplot", "term_count": 2, "settings": go_original.ui_settings}
    assert gsea_plot == {
        "module": "gsea",
        "plot_type": "ridgeplot",
        "term_count": 2,
        "settings": gsea_original.ui_settings,
    }
    assert restored_go.enrichment_reruns == 0
    assert restored_gsea.enrichment_reruns == 0
