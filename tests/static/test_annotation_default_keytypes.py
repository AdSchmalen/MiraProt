#!/usr/bin/env python3
"""Static checks for Annotation default keytype wiring."""

from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
UTILS = ROOT / "modules" / "Data Wizard" / "Annotation" / "datawizard_annotation_utils.R"
ORCHESTRATOR = ROOT / "modules" / "Data Wizard" / "datawizard_annotation.R"

utils_text = UTILS.read_text()
human_match = re.search(
    r'"org\.Hs\.eg\.db"\s*=\s*c\((.*?)\)',
    utils_text,
    flags=re.DOTALL,
)
assert human_match, "Could not locate Homo sapiens default keytypes"

human_defaults = set(re.findall(r'"([^"]+)"', human_match.group(1)))
required_human_keytypes = {"REFSEQ", "GENENAME", "ALIAS"}
missing_human_keytypes = required_human_keytypes - human_defaults
assert not missing_human_keytypes, (
    "Homo sapiens defaults are missing required keytypes: "
    + ", ".join(sorted(missing_human_keytypes))
)

source_lines = ORCHESTRATOR.read_text().splitlines()
utils_source_line = next(
    (idx for idx, line in enumerate(source_lines, start=1)
     if "datawizard_annotation_utils.R" in line),
    None,
)
general_source_line = next(
    (idx for idx, line in enumerate(source_lines, start=1)
     if "datawizard_annotation_observer_general.R" in line),
    None,
)
assert utils_source_line is not None, "Missing datawizard_annotation_utils.R source call"
assert general_source_line is not None, "Missing datawizard_annotation_observer_general.R source call"
assert utils_source_line < general_source_line, (
    "datawizard_annotation_utils.R must be sourced before "
    "datawizard_annotation_observer_general.R"
)

print("Annotation default keytype static checks passed")
