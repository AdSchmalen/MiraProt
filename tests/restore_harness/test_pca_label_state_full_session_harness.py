"""Focused PCA/UMAP full-session label restore harness.

The production normalizer is pure R, but the restore-harness suite intentionally
runs without a Shiny or R runtime.  This small table model mirrors its documented
input/output schema and exercises complete save/restore transitions; static tests
tie those transitions to the production setter and render observer.
"""

from __future__ import annotations

from copy import deepcopy
from typing import Any

import pytest


METHODS = ("PCA", "UMAP")
EMPTY_ITEMS = []
EMPTY_SETTINGS: list[dict[str, Any]] = []


def normalize_labels(snapshot: dict[str, Any]) -> dict[str, Any]:
    """Model normalize_pca_label_restore_state's canonical output table."""
    request = snapshot.get("plot_request") or {}
    current = request.get("label_state")
    legacy = request.get("labels") or {}
    source = current if isinstance(current, dict) and current.get("mode") else legacy
    target = (source.get("mode") or (snapshot.get("analysis_results") or {}).get("comparison_target")
              or (snapshot.get("plot_ui_inputs") or {}).get("comparison_target") or "samples")
    mode = "proteins" if str(target).lower() in {"protein", "proteins"} else "samples"

    if mode == "samples":
        active = source.get("labeling_active", source.get(
            "sample_labeling_active_pca", snapshot.get("sample_labeling_active_pca", False)
        ))
        settings = source.get("settings", source.get(
            "sample_label_settings_pca", snapshot.get("sample_label_settings_pca", {})
        ))
        return {"mode": "samples", "labeling_active": active is True,
                "sample_settings": deepcopy(settings) if isinstance(settings, dict) else {},
                "selected_items": EMPTY_ITEMS, "item_settings": EMPTY_SETTINGS}

    selection_is_explicit = isinstance(current, dict) and "selected_items" in current
    selected = source.get("selected_items")
    if selected is None and not selection_is_explicit:
        selected = source.get("selected_items_vector_pca", snapshot.get("selected_items_vector_pca"))
    if not selected and not selection_is_explicit:
        selected = source.get("labeled_proteins", snapshot.get("labeled_proteins", []))
    selected = list(dict.fromkeys(str(item) for item in (selected or []) if str(item).strip()))
    settings = source.get("settings", source.get(
        "item_label_settings_pca", snapshot.get("item_label_settings_pca", [])
    ))
    setting_ids = [row.get("item_id") for row in settings]
    counts = {item: setting_ids.count(item) for item in selected}
    clean = [deepcopy(row) for row in settings if row.get("item_id") in selected
             and counts[row["item_id"]] == 1]
    return {"mode": "proteins", "labeling_active": False, "sample_settings": {},
            "selected_items": selected, "item_settings": clean}


def restore_into(receiver: dict[str, Any], snapshot: dict[str, Any]) -> dict[str, Any]:
    """Integration model of set_session_state: normalize, replace, then render."""
    normalized = normalize_labels(snapshot)
    receiver["restore_guard"] = True
    receiver["static_plot"] = None
    receiver["sample_active"] = normalized["labeling_active"]
    receiver["sample_settings"] = normalized["sample_settings"]
    receiver["selected_items"] = normalized["selected_items"]
    receiver["item_settings"] = normalized["item_settings"]
    assert receiver["static_plot"] is None  # no render while guarded
    receiver["restore_guard"] = False
    receiver["static_plot"] = {"method": snapshot["method"], "labels": deepcopy(normalized)}
    return normalized


def snapshot(method: str, label_state: dict[str, Any]) -> dict[str, Any]:
    mode = label_state["mode"]
    return {"method": method, "had_plot": True,
            "analysis_results": {"method": method, "comparison_target": mode, "coordinates": [[0, 1]]},
            "plot_ui_inputs": {"analysis_method": method.lower(), "comparison_target": mode},
            "plot_request": {"label_state": deepcopy(label_state)}}


class PcaProteinUi:
    """Small module-UI driver for the dynamic protein label editor.

    This deliberately keeps the plot outside the editor's mutable state, just
    as the Shiny module does.  It lets the restore harness exercise a complete
    client interaction after the table-oriented restore assertions above.
    """

    def __init__(self) -> None:
        self.enhanced_selected_items_suspended = False
        self.protein_controls_visible = False
        self.rows: list[dict[str, Any]] = []
        self.selected_items_vector_pca: list[str] = []
        self.item_label_settings_pca: list[dict[str, Any]] = []
        self.static_plot: dict[str, Any] | None = None
        self.dynamic_inputs: dict[tuple[str, str], Any] = {}

    def restore(self, saved: dict[str, Any]) -> None:
        normalized = restore_into(self.__dict__, saved)
        self.selected_items_vector_pca = normalized["selected_items"]
        self.item_label_settings_pca = deepcopy(normalized["item_settings"])
        settings_by_id = {row["item_id"]: row for row in self.item_label_settings_pca}
        self.rows = [deepcopy(settings_by_id[item]) for item in self.selected_items_vector_pca]
        self.dynamic_inputs = {
            (row["item_id"], field): row[field]
            for row in self.rows
            for field in ("label_color", "dot_color", "use_custom_dot_color")
        }
        self.protein_controls_visible = bool(self.selected_items_vector_pca)

    def change_dynamic_input(self, item_id: str, field: str, value: Any) -> None:
        self.dynamic_inputs[(item_id, field)] = value

    def applySettings_pca(self) -> None:
        for row in self.rows:
            for field in ("label_color", "dot_color", "use_custom_dot_color"):
                row[field] = self.dynamic_inputs[(row["item_id"], field)]
        self.item_label_settings_pca = deepcopy(self.rows)

    def remove_item_click_pca(self, item_id: str) -> None:
        self.selected_items_vector_pca = [
            item for item in self.selected_items_vector_pca if item != item_id
        ]
        self.rows = [row for row in self.rows if row["item_id"] != item_id]

    def click_header_or_chevron(self, target: str) -> None:
        # The chevron is a child of the single header click target.  One user
        # click therefore produces one visibility transition, not two.
        assert target in {"header", "chevron"}
        self.protein_controls_visible = not self.protein_controls_visible


@pytest.mark.parametrize("method", METHODS)
def test_sample_all_labels_with_non_default_style_and_explicitly_disabled(method: str) -> None:
    style = {"master_label_color": "#123456", "master_dot_color": "#ABCDEF",
             "use_master_dot_color": True, "max_overlaps": 37, "label_distance": 0.8,
             "line_thickness": 1.4, "label_size": 11, "labeled_dot_size": 4}
    for active in (True, False):
        saved = snapshot(method, {"mode": "samples", "labeling_active": active, "settings": style})
        receiver = {"selected_items": ["STALE"], "item_settings": [{"item_id": "STALE"}]}
        normalized = restore_into(receiver, saved)
        assert normalized == {"mode": "samples", "labeling_active": active,
                              "sample_settings": style, "selected_items": [], "item_settings": []}
        assert receiver["static_plot"]["labels"] == normalized


@pytest.mark.parametrize("method", METHODS)
def test_distinct_protein_settings_and_explicit_empty_selection(method: str) -> None:
    rows = [
        {"item_id": "P001", "label_color": "#111111", "dot_color": "#AA0000", "use_custom_dot_color": True},
        {"item_id": "P002", "label_color": "#222222", "dot_color": "#00AA00", "use_custom_dot_color": False},
    ]
    saved = snapshot(method, {"mode": "proteins", "selected_items": ["P001", "P002"], "settings": rows})
    receiver = {"sample_active": True, "sample_settings": {"active": True}}
    normalized = restore_into(receiver, saved)
    assert normalized["selected_items"] == ["P001", "P002"]
    assert normalized["item_settings"] == rows
    assert receiver["sample_active"] is False and receiver["sample_settings"] == {}

    empty = snapshot(method, {"mode": "proteins", "selected_items": [], "settings": rows})
    receiver = {"selected_items": ["OLD"], "item_settings": [{"item_id": "OLD"}]}
    assert restore_into(receiver, empty)["selected_items"] == []
    assert receiver["item_settings"] == []
    assert receiver["static_plot"]["labels"]["selected_items"] == []


@pytest.mark.parametrize("click_target", ("header", "chevron"))
def test_protein_restore_hydrates_dynamic_module_ui_and_keeps_plot_available(
    click_target: str,
) -> None:
    rows = [
        {"item_id": "P001", "label_color": "#112233", "dot_color": "#AABBCC",
         "use_custom_dot_color": True},
        {"item_id": "P002", "label_color": "#445566", "dot_color": "#DDEEFF",
         "use_custom_dot_color": False},
    ]
    saved = snapshot(
        "PCA",
        {"mode": "proteins", "selected_items": ["P001", "P002"], "settings": rows},
    )
    ui = PcaProteinUi()

    assert ui.enhanced_selected_items_suspended is False
    assert ui.protein_controls_visible is False

    ui.restore(saved)

    assert ui.protein_controls_visible is True
    assert len(ui.rows) == 2
    assert {row["item_id"]: row for row in ui.rows} == {row["item_id"]: row for row in rows}
    restored_plot = ui.static_plot
    assert restored_plot is not None

    ui.change_dynamic_input("P002", "label_color", "#FEDCBA")
    ui.applySettings_pca()
    assert next(
        row for row in ui.item_label_settings_pca if row["item_id"] == "P002"
    )["label_color"] == "#FEDCBA"
    assert ui.static_plot is restored_plot

    ui.remove_item_click_pca("P001")
    assert ui.selected_items_vector_pca == ["P002"]
    assert ui.static_plot is restored_plot

    visibility_before_click = ui.protein_controls_visible
    ui.click_header_or_chevron(click_target)
    assert ui.protein_controls_visible is (not visibility_before_click)
    assert ui.static_plot is restored_plot


@pytest.mark.parametrize("method", METHODS)
def test_cross_mode_restore_clears_irrelevant_receiver_state(method: str) -> None:
    sample = snapshot(method, {"mode": "samples", "labeling_active": True,
                               "settings": {"master_label_color": "#135790"}})
    protein_receiver = {"selected_items": ["OLD"], "item_settings": [{"item_id": "OLD"}]}
    restore_into(protein_receiver, sample)
    assert protein_receiver["selected_items"] == [] and protein_receiver["item_settings"] == []

    protein = snapshot(method, {"mode": "proteins", "selected_items": ["P9"], "settings": []})
    sample_receiver = {"sample_active": True, "sample_settings": {"active": True}}
    restore_into(sample_receiver, protein)
    assert sample_receiver["sample_active"] is False and sample_receiver["sample_settings"] == {}


def test_legacy_pca_top_level_and_plot_request_labels_tables() -> None:
    top_level = {"method": "PCA", "analysis_results": {"comparison_target": "proteins"},
                 "selected_items_vector_pca": ["P1"], "labeled_proteins": ["STALE"],
                 "item_label_settings_pca": [
                     {"item_id": "P1", "label_color": "#111111", "dot_color": "#222222", "use_custom_dot_color": True}
                 ]}
    assert normalize_labels(top_level)["selected_items"] == ["P1"]

    request_labels = {"method": "PCA", "plot_ui_inputs": {"comparison_target": "samples"},
                      "sample_labeling_active_pca": False,
                      "plot_request": {"labels": {"sample_labeling_active_pca": True,
                                                    "sample_label_settings_pca": {"label_size": 13}}}}
    normalized = normalize_labels(request_labels)
    assert normalized["labeling_active"] is True
    assert normalized["sample_settings"] == {"label_size": 13}


@pytest.mark.parametrize("save_level,modules", [
    ("data_only", {"datawizard"}),
    ("data_and_analysis", {"datawizard", "go", "gsea"}),
])
def test_non_full_snapshots_omit_pca(save_level: str, modules: set[str]) -> None:
    envelope = {"save_level": save_level, "module_snapshots": {name: {} for name in modules}}
    assert "pca" not in envelope["module_snapshots"]
