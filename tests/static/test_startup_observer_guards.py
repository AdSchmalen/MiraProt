#!/usr/bin/env python3
"""Static checks for startup-silent observers and readable file-loader logs."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
HEATMAP_OBSERVERS = ROOT / "modules" / "Heatmap" / "Heatmap_observers.R"
FILE_LOADER = ROOT / "modules" / "Data Wizard" / "datawizard_file_loader.R"

heatmap_text = HEATMAP_OBSERVERS.read_text()
go_observer_match = re.search(
    r"# Update GO dropdown only after GO results exist\.(.*?)# Display suggested identifiers",
    heatmap_text,
    flags=re.DOTALL,
)
assert go_observer_match, "Could not locate guarded GO dropdown observer"
go_observer = go_observer_match.group(1)
assert "req(!is.null(GO_res))" in go_observer, "GO observer must wait for GO_res"
assert "req(!is.null(go_results))" in go_observer, "GO observer must wait for non-null GO results"
assert "go_data is not a data.frame" not in go_observer, "GO observer should not log NULL startup data"

loader_text = FILE_LOADER.read_text()
for forbidden in (
    "HEADER_ROW OBSERVER TRIGGERED",
    "HEADER_ROW2 OBSERVER TRIGGERED",
    "FILE_LOADER:",
    "ERROR in",
    "WARNING in",
    "SUCCESS in",
    "HEADER CHANGE:",
    "BLOCKING header_row",
):
    assert forbidden not in loader_text, f"Caps-lock debug text remains: {forbidden}"

primary_observer_match = re.search(
    r"observeEvent\(\{\s*list\(data_fixed\(\), header_primary_debounced\(\)\).*?"
    r"restored_primary_cache <- can_use_restored_sheet_cache_for_header_primary\(\)\s*"
    r"raw_primary_data <- data_fixed\(\)\s*"
    r"if \(!is_valid_data\(raw_primary_data\) && !isTRUE\(restored_primary_cache\)\) \{.*?"
    r"can_header_reprocess_primary\(restored_primary_cache\)",
    loader_text,
    flags=re.DOTALL,
)
assert primary_observer_match, "Primary header observer must require valid data or the narrow restored-cache predicate before firing"

secondary_observer_match = re.search(
    r"observeEvent\(\{\s*list\(data2_fixed\(\), header_additional_debounced\(\)\).*?"
    r"restored_secondary_cache <- can_use_restored_sheet_cache_for_header_secondary\(\)\s*"
    r"raw_secondary_data <- data2_fixed\(\)\s*"
    r"if \(!is_valid_data\(raw_secondary_data\) && !isTRUE\(restored_secondary_cache\)\) \{.*?"
    r"can_header_reprocess_secondary\(restored_secondary_cache\)",
    loader_text,
    flags=re.DOTALL,
)
assert secondary_observer_match, "Secondary header observer must require valid data or the narrow restored-cache predicate before firing"

for predicate in (
    "can_use_restored_sheet_cache_for_header_primary",
    "can_use_restored_sheet_cache_for_header_secondary",
):
    definition_count = len(re.findall(rf"{predicate} <- function", loader_text))
    call_count = len(re.findall(rf"{predicate}\(\)", loader_text))
    assert definition_count == 1, f"{predicate} must be defined exactly once"
    assert call_count == 1, f"{predicate} must only be called by its header-row observer"

restored_cache_guard = re.search(
    r"restored_sheet_cache_header_guards_clear <- function\(\) \{(.*?)\n    \}",
    loader_text,
    flags=re.DOTALL,
)
assert restored_cache_guard, "Restored-cache header guard predicate must be present"
guard_body = restored_cache_guard.group(1)
for required_guard in (
    "!isTRUE(restore_replay_active())",
    "!isTRUE(reset_replay_active())",
    "is.null(pending_loader_state())",
    "!isTRUE(rv$session_restoring)",
):
    assert required_guard in guard_body, f"Restored-cache header guard missing: {required_guard}"

print("Startup observer guard static checks passed")

DOTPLOT_INTERACTION = ROOT / "modules" / "dot" / "dotplot_server_interaction.R"
dotplot_text = DOTPLOT_INTERACTION.read_text()

assert "has_seen_gsea_results_dot <- reactiveVal(FALSE)" in dotplot_text, (
    "Dotplot GSEA observer must track readiness transitions from an initially silent state"
)
assert "has_seen_go_results_dot <- reactiveVal(FALSE)" in dotplot_text, (
    "Dotplot GO observer must track readiness transitions from an initially silent state"
)

for source, dropdown, flag in (
    ("GSEA", "GSEA_dot", "has_seen_gsea_results_dot"),
    ("GO", "GO_dot", "has_seen_go_results_dot"),
):
    null_guard = re.search(
        rf"if \(is\.null\({source.lower()}_results\)\) \{{(.*?)req\(FALSE\)",
        dotplot_text,
        flags=re.DOTALL,
    )
    assert null_guard, f"Could not locate Dotplot {source} NULL-result startup guard"
    guard_body = null_guard.group(1)
    assert f"isTRUE({flag}())" in guard_body, (
        f"Dotplot {source} NULL-result guard must only clear after a ready -> not-ready transition"
    )
    assert f'updateSelectInput(session, "{dropdown}"' in guard_body, (
        f"Dotplot {source} transition guard must clear the dropdown after prior results disappear"
    )
    assert f'{flag}(FALSE)' in guard_body, (
        f"Dotplot {source} transition guard must reset readiness after clearing"
    )
    assert f"No valid {source} data available for dropdown update" not in guard_body, (
        f"Plain startup with no {source} data must not emit the stale Dotplot dropdown log"
    )

print("Dotplot startup dropdown log regression checks passed")
