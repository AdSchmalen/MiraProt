#!/usr/bin/env python3
"""Lightweight restore harness for legacy Data Wizard session snapshots.

This test models the restore invariants that caused legacy sessions with a
55-column raw loader table and a 56-column processed Data Wizard table to lose
processed columns/metadata during restore.  It intentionally avoids launching a
full Shiny session so it can run in minimal CI environments.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_module_registration.R"
LOADER = ROOT / "modules" / "Data Wizard" / "datawizard_file_loader.R"
LOADER_RESTORE = ROOT / "modules" / "Data Wizard" / "file_loader" / "datawizard_file_loader_restore.R"
CORE = ROOT / "modules" / "Data Wizard" / "datawizard_core.R"
CORE_SUBMODULE_SESSION = ROOT / "modules" / "Data Wizard" / "core" / "datawizard_core_submodule_session.R"
UTILS = ROOT / "modules" / "Data Wizard" / "datawizard_utils.R"


@dataclass
class Table:
    names: list[str]

    @property
    def ncol(self) -> int:
        return len(self.names)


@dataclass
class Metadata:
    column: list[str]
    content: list[str]
    options: list[str]
    sample: list[str]
    transformation: list[str]

    @property
    def nrow(self) -> int:
        return len(self.column)


@dataclass
class RestoreRv:
    primary_data_raw: Table | None = None
    data_mod: Table | None = None
    data_def: Metadata | None = None
    session_restore_generation: int = 42


@dataclass
class LoaderRestoreHarness:
    """Pure harness for loader restore dedupe and publish decisions."""

    rv: RestoreRv
    applied_generations: list[int] = field(default_factory=list)
    local_data_fixed: Table | None = None

    def apply(self, staged: dict) -> bool:
        generation = staged.get("restore_generation")
        if generation in self.applied_generations:
            return False
        self.applied_generations.append(generation)
        self.local_data_fixed = staged["data_fixed"]
        if staged.get("restore_skip_publish_working_data"):
            # Loader-local state may refresh the raw mirror, but must not publish
            # the 55-column raw table over the canonical 56-column processed data.
            self.rv.primary_data_raw = staged["data_fixed"]
        else:
            self.rv.data_mod = staged["data_fixed"]
        return True


@dataclass
class DynamicUiReplayHarness:
    """Pure harness for bounded dynamic UI replay and unresolved-value drops."""

    pending: dict[str, str]
    input_values: dict[str, str | None] = field(default_factory=dict)
    choices: dict[str, list[str]] = field(default_factory=dict)
    max_restore_attempts: int = 5
    attempts: int = 0
    warnings: list[str] = field(default_factory=list)

    def tick(self) -> None:
        if not self.pending:
            return
        next_attempt = self.attempts + 1
        assert next_attempt <= self.max_restore_attempts, "attempts must never exceed max_restore_attempts"
        self.attempts = next_attempt
        remaining: dict[str, str] = {}
        for input_id, value in self.pending.items():
            available = self.choices.get(input_id, [])
            if value in available:
                self.input_values[input_id] = value
            else:
                remaining[input_id] = value
        if remaining and self.attempts >= self.max_restore_attempts:
            self.warnings.append(
                f"dropping unresolved inputs after {self.max_restore_attempts} attempt(s): "
                + ",".join(sorted(remaining))
            )
            self.pending = {}
        else:
            self.pending = remaining


def metadata_matches_dataset(meta: Metadata, data: Table) -> bool:
    return meta.nrow == data.ncol and [str(x) for x in meta.column] == [str(x) for x in data.names]


def is_meaningful_metadata(meta: Metadata) -> bool:
    meaningful = [c.strip().lower() for c in meta.content if c and c.strip()]
    return any(c != "row index" for c in meaningful)



def assert_datawizard_data_only_restore_invariants(rv: RestoreRv) -> None:
    assert isinstance(rv.data_mod, Table), "rv.data_mod must be a data frame/table"
    assert isinstance(rv.data_def, Metadata), "rv.data_def must be a metadata table"
    assert hasattr(rv.data_def, "column"), "rv.data_def must contain a Column field"
    assert [str(x) for x in rv.data_def.column] == [str(x) for x in rv.data_mod.names], (
        "rv.data_def$Column must exactly match names(rv.data_mod)"
    )
    assert is_meaningful_metadata(rv.data_def), "rv.data_def must contain meaningful metadata"

def make_legacy_snapshot() -> dict:
    raw_names = ["Row Index"] + [f"Raw_{i:02d}" for i in range(1, 55)]
    data_mod_names = raw_names + ["Basemean"]
    content = ["Row Index", "Identifier", "Gene Symbol"]
    content.extend("Abundance" if i % 2 else "Condition" for i in range(3, 55))
    content.append("Processed Statistic")
    metadata = Metadata(
        column=data_mod_names,
        content=content,
        options=["Index", "UniProt", "Gene", *[f"Sample_{i:02d}" for i in range(3, 55)], "Basemean"],
        sample=["", "", "", *[f"S{i:02d}" for i in range(3, 55)], "derived"],
        transformation=["None"] * 55 + ["Mean abundance"],
    )
    return {
        "primary_data_raw_rv": Table(raw_names),
        "data_mod": Table(data_mod_names),
        "data_def": metadata,
        "loader_state": {
            "data_fixed": Table(raw_names),
            "inputs": {"sheetDropdown": "Legacy primary", "header_primary": 1},
        },
        "submodule_ui_states": {
            "submodules": {
                "basemean_out": {"ui_inputs": {"abundance_cols": "Basemean"}},
                "filtering_out": {"ui_inputs": {"condition_col": "Raw_04"}},
            }
        },
    }


def assert_restore_harness_behaviour() -> None:
    snapshot = make_legacy_snapshot()
    assert snapshot["primary_data_raw_rv"].ncol == 55
    assert snapshot["loader_state"]["data_fixed"].ncol == 55
    assert snapshot["data_mod"].ncol == 56
    assert "Basemean" in snapshot["data_mod"].names
    assert metadata_matches_dataset(snapshot["data_def"], snapshot["data_mod"])
    assert is_meaningful_metadata(snapshot["data_def"])

    rv = RestoreRv(
        primary_data_raw=snapshot["primary_data_raw_rv"],
        data_mod=snapshot["data_mod"],
        data_def=snapshot["data_def"],
    )
    loader = LoaderRestoreHarness(rv)
    staged = dict(snapshot["loader_state"], restore_skip_publish_working_data=True, restore_generation=rv.session_restore_generation)

    assert loader.apply(staged) is True
    assert loader.apply(staged) is False, "same restore generation must be staged exactly once"
    assert loader.applied_generations == [rv.session_restore_generation]
    assert rv.data_mod.ncol == 56, "loader restore must not replace processed data with raw data"
    assert rv.data_mod.names[-1] == "Basemean"
    assert_datawizard_data_only_restore_invariants(rv)

    replay = DynamicUiReplayHarness(pending={"abundance_cols": "Basemean"}, choices={"abundance_cols": []})
    for _ in range(replay.max_restore_attempts + 3):
        replay.tick()
    assert replay.attempts == replay.max_restore_attempts
    assert replay.pending == {}, "unresolved values must be dropped at the bounded retry cap"
    assert replay.warnings == ["dropping unresolved inputs after 5 attempt(s): abundance_cols"]

    replay = DynamicUiReplayHarness(pending={"abundance_cols": "Basemean"}, choices={"abundance_cols": []})
    replay.tick()
    replay.choices["abundance_cols"] = ["Basemean"]
    replay.tick()
    assert replay.pending == {}
    assert replay.input_values["abundance_cols"] == "Basemean"


def assert_restore_source_contains_required_guards() -> None:
    registration = REGISTRATION.read_text()
    loader = LOADER.read_text() + LOADER_RESTORE.read_text()
    core = CORE.read_text() + CORE_SUBMODULE_SESSION.read_text()
    utils = UTILS.read_text()

    assert "assert_datawizard_data_only_restore_invariants <- function(rv, strict = FALSE)" in registration
    assert "rv$datawizard_restore_diagnostics <- diagnostics" in registration
    assert "Data Wizard data-only canonical restore invariant validation failed" in registration
    assert "assert_datawizard_data_only_restore_invariants(rv, strict = FALSE)" in registration
    assert "loader_state$restore_skip_publish_working_data <- TRUE" in registration
    assert "loader_state$restore_generation <- restore_generation" in registration
    assert "last_applied_loader_restore_generation" in loader
    assert "skipping already-applied staged state" in loader
    assert "restored loader-local state without publishing working data" in loader
    assert "rv$primary_data_raw <- st$data_fixed" in loader
    assert "publish_primary_current_sheet(st$data_fixed" in loader
    assert re.search(r"if \(skip_publish_working_data\).*?rv\$primary_data_raw <- st\$data_fixed.*?\} else \{.*?publish_primary_current_sheet\(st\$data_fixed", loader, re.S)
    assert "identical(as.character(meta$Column), as.character(names(data)))" in utils
    assert "any(tolower(content_values) != \"row index\")" in utils
    assert "remaining_ids <- unique(c(deferred_ids, unresolved_ids))" in core
    assert "pending_ui_state(st)" in core
    assert "inputs_unresolved_after_apply" in core
    assert "attempt + 1L >= max_restore_attempts" in core, (
        "dynamic replay must be bounded by max_restore_attempts"
    )
    assert "restore warning: dropping unresolved inputs after " in core, (
        "unresolved dynamic inputs must be dropped with a clear warning at the retry cap"
    )


if __name__ == "__main__":
    assert_restore_harness_behaviour()
    assert_restore_source_contains_required_guards()
    print("Legacy Data Wizard session restore harness checks passed")
