#!/usr/bin/env python3
"""Ensure restored filtering triggers cannot overwrite canonical Data Wizard data."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INTEGRATION = ROOT / "modules" / "Data Wizard" / "datawizard_integration.R"

source = INTEGRATION.read_text()
function_start = source.index("setup_filter_integration <- function")
function_end = source.index("#' Helper function to safely get reactive values", function_start)
filter_integration = source[function_start:function_end]


def observer_body(trigger: str) -> str:
    """Return the callback text belonging to one filter trigger observer."""
    start = filter_integration.index(f"observeEvent(filter_module${trigger}(), {{")
    next_observer = filter_integration.find("observeEvent(filter_module$", start + 1)
    return filter_integration[start : next_observer if next_observer >= 0 else None]


apply_body = observer_body("apply_filters_trigger")
clear_body = observer_body("clear_filters_trigger")

for trigger, body, first_write, operation in [
    ("apply", apply_body, "primary_data_state$set_filtered_data(", "filter"),
    ("clear", clear_body, "core_values$filtered_data(NULL)", "filter clear"),
]:
    guard = body.index(f'if (!publication_helper(', body.index(f'"{operation}"') - 80)
    early_return = body.index("return(invisible(NULL))", guard)
    write = body.index(first_write)
    assert guard < early_return < write, (
        f"The {trigger} observer must reject replay publication before its first canonical write"
    )

# Keep the narrower callback boundary: metadata validation and publication remain
# intact for genuine interactive filtering after restoration completes.
assert "metadata_matches_dataset" not in filter_integration
assert "primary_data_state$set_metadata_for_current_data(current_metadata)" in apply_body
assert "primary_data_state$set_filtered_data(filter_result$data, source = \"filter\")" in apply_body
assert 'publication_helper(filter_result$data, "filter")' in apply_body
assert 'publication_helper(baseline, "filter clear")' in clear_body

print("Data Wizard filter restore replay guards are correctly scoped")
