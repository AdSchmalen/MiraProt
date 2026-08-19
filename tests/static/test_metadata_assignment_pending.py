#!/usr/bin/env python3
"""Static checks for Data Wizard metadata assignment pending downstream guards."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
UTILS = ROOT / "R" / "utils.R"
PCA = ROOT / "modules" / "PCA" / "pca_module_server_observers.R"
HEATMAP = ROOT / "modules" / "Heatmap" / "Heatmap_observers.R"
VENN = ROOT / "modules" / "venn" / "venn_observers_data_lists.R"

utils_text = UTILS.read_text()

assert re.search(
    r"datawizard_metadata_defer_downstream_choices <- function\(rv = NULL\) \{\s*"
    r"datawizard_metadata_assignment_pending\(rv\)\s*\}",
    utils_text,
), "Downstream choices must defer for the entire metadata assignment pending window"

ready_match = re.search(
    r"datawizard_metadata_ready_for_abundance_warning <- function\(rv = NULL, data_def = NULL\) \{(.*?)\n\}",
    utils_text,
    flags=re.DOTALL,
)
assert ready_match, "Could not locate abundance warning readiness helper"
ready_body = ready_match.group(1)
assert "datawizard_metadata_committed_ready(rv)" in ready_body, (
    "Abundance warning helper must require committed metadata readiness"
)
assert "Content" not in ready_body and "meaningful_content" not in ready_body, (
    "Abundance warning helper must not infer readiness directly from metadata Content values"
)

pending_match = re.search(
    r"datawizard_metadata_assignment_pending <- function\(rv = NULL\) \{(.*?)\n\}",
    utils_text,
    flags=re.DOTALL,
)
assert pending_match, "Could not locate metadata assignment pending helper"
pending_body = pending_match.group(1)
assert "datawizard_metadata_lifecycle_state" in pending_body, (
    "Pending helper must treat the metadata_assigning lifecycle state as pending"
)
assert "metadata_assigning" in pending_body, (
    "Pending helper must remain true while lifecycle state is metadata_assigning"
)

venn_text = VENN.read_text()
venn_defer_pos = venn_text.find("if (datawizard_metadata_defer_downstream_choices(rv))")
venn_req_pos = venn_text.find("req(rv$data_def)", venn_defer_pos)
venn_update_pos = venn_text.find("Updating central abundance type choices for reference values")
venn_row_index_pos = venn_text.find("Venn reference value choices unavailable: metadata only contains Row Index")
assert venn_defer_pos != -1, "Venn observer must check assignment-pending defer guard"
assert venn_req_pos != -1, "Venn observer must still require metadata after the pending guard"
assert venn_defer_pos < venn_req_pos < venn_update_pos < venn_row_index_pos, (
    "Venn pending guard must run before metadata req, update log, and Row Index-only log"
)
assert "Metadata assignment pending; Venn reference value choices left empty" in venn_text, (
    "Venn observer must keep the low-priority pending message"
)
assert "non_row_index_choices <- content_choices[content_choices != \"Row Index\"]" in venn_text, (
    "Venn observer must keep Row Index excluded from reference choices"
)

for label, path, log_fn in (
    ("PCA", PCA, "debug_log"),
    ("Heatmap", HEATMAP, "heatmap_debug_log"),
):
    text = path.read_text()
    defer_pos = text.find("if (datawizard_metadata_defer_downstream_choices(rv))")
    warning_pos = text.find('No abundance data available. Prompting user.')
    assert defer_pos != -1, f"{label} observer must check assignment-pending defer guard"
    assert warning_pos != -1, f"{label} observer must retain abundance warning branch"
    assert defer_pos < warning_pos, f"{label} defer guard must run before abundance warning branch"
    guard_block = text[defer_pos:warning_pos]
    assert "return()" in guard_block, f"{label} defer guard must return before warning branch"
    assert "metadata not ready yet" in text, f"{label} observer must keep low-verbosity not-ready log"

print("Metadata assignment pending downstream guard checks passed")
