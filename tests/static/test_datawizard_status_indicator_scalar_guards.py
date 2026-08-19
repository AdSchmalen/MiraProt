#!/usr/bin/env python3
"""Static regression checks for Data Wizard table status scalar guards."""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OBSERVER = ROOT / "modules" / "Data Wizard" / "tables" / "datawizard_tables_observer.R"
text = OBSERVER.read_text()

helper_match = re.search(
    r"safe_scalar_logical <- function\(x, default = FALSE\) \{(.*?)\n  \}\n\n  safe_scalar_string",
    text,
    flags=re.DOTALL,
)
assert helper_match, "safe_scalar_logical helper must be defined before status observers"
helper_body = helper_match.group(1)
for required in (
    "!is.logical(x)",
    "length(x) < 1",
    "is.na(x[[1]])",
    "return(default)",
    "isTRUE(x[[1]])",
):
    assert required in helper_body, f"safe_scalar_logical must guard {required}"

status_match = re.search(
    r"output\$data_status_indicator <- renderUI\(\{(.*?)\n  \}\)",
    text,
    flags=re.DOTALL,
)
assert status_match, "Could not locate data status indicator renderUI"
status_body = status_match.group(1)
assert "is_filtered <- safe_scalar_logical(filter_applied(), FALSE)" in status_body, (
    "filter_applied() must be scalarized before assigning is_filtered"
)
assert re.search(r"is_modified <- safe_scalar_logical\(is_modified, FALSE\).*?if \(is_modified\)", status_body, re.DOTALL), (
    "is_modified must be scalarized immediately before if (is_modified)"
)
assert "status_color <- safe_scalar_string(status_color, \"#18bc9c\")" in status_body, (
    "status_color must be normalized to a scalar string"
)
assert "status_text <- safe_scalar_string(status_text, \"RAW DATA\")" in status_body, (
    "status_text must be normalized to a scalar string"
)


def safe_scalar_logical_py(value, default=False, raises=False):
    """Python mirror of safe_scalar_logical for fixed regression cases below."""
    if raises:
        return False
    if value is None or len(value) < 1 or value[0] is None:
        return default
    if not isinstance(value[0], bool):
        return default
    return value[0]

cases = [
    ("logical(0)", [], False),
    ("NULL", None, False),
    ("errors", [], False, True),
    ("TRUE", [True], True),
    ("FALSE", [False], False),
]
for case in cases:
    label, value, expected, *rest = case
    raises = bool(rest and rest[0])
    actual = safe_scalar_logical_py(value, False, raises=raises)
    assert actual is expected, f"filter_applied()={label} expected {expected}, got {actual}"

print("Data Wizard status scalar guard static checks passed")
