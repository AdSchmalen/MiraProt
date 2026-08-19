#!/usr/bin/env python3
"""Static checks for debounced Assign Rules condition text inputs."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
HANDLERS = ROOT / "modules" / "Data Wizard" / "assign rules" / "datawizard_assign_rules_handlers.R"

text = HANDLERS.read_text()
function_match = re.search(
    r"register_assign_rules_condition_sync_handlers <- function\((.*?)\n}\n",
    text,
    flags=re.DOTALL,
)
assert function_match, "Could not locate condition sync handler"
body = function_match.group(1)

assert "condition_text_inputs <- reactive" in body, "Condition text inputs should be collected reactively"
assert "condition_text_inputs_debounced <- debounce(condition_text_inputs, millis = 1500)" in body, (
    "Condition text input sync must be debounced by 1500 ms"
)
assert "observeEvent(condition_text_inputs_debounced()" in body, (
    "Condition text observer must consume the debounced reactive"
)
assert "Options_condition(all_options)" in body, "Debounced sync must update metadata dropdown options"
assert "millis = 500" not in body, "Old 500 ms condition debounce should not remain"

print("Assign Rules text input debounce static checks passed")
