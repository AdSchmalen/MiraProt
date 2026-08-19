#!/usr/bin/env python3
"""Static contract for the safety-critical tables metadata observer unit."""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ENTRY = (ROOT / "modules/Data Wizard/datawizard_tables.R").read_text()
RENDERING = (ROOT / "modules/Data Wizard/tables/datawizard_tables_observer_rendering.R").read_text()
METADATA = (ROOT / "modules/Data Wizard/tables/datawizard_tables_observer_metadata.R").read_text()
METADATA_PARTS = "\n".join(
    (ROOT / f"modules/Data Wizard/tables/datawizard_tables_observer_metadata_{phase}.R").read_text()
    for phase in ("hydration", "sync", "editing")
)
COORDINATOR = (ROOT / "modules/Data Wizard/tables/datawizard_tables_observer.R").read_text()

assert 'source("modules/Data Wizard/tables/datawizard_tables_observer_metadata.R"' in ENTRY
assert len(COORDINATOR.splitlines()) < 1000
assert "register_tables_metadata_hydration <- function(context)" in METADATA_PARTS
assert "register_tables_metadata_editing <- function(context)" in METADATA_PARTS
for phase in ("hydration", "sync", "editing"):
    assert f'datawizard_tables_observer_metadata_{phase}.R"' in METADATA
assert RENDERING.index("register_tables_metadata_hydration(context)") < RENDERING.index(
    "register_tables_metadata_editing(context)"
)

# Singular registrations/callbacks are performance and circular-loop sentinels:
# one render, one edit observer, one canonical callback in each write path, and
# one notification throttle. Duplicate work would change these counts.
for fragment, expected in (
    ("output$metadata_table <- renderRHandsontable({", 1),
    ("observeEvent(input$metadata_table, {", 1),
    ("sync_current_metadata_to_core <- function(", 1),
    ("set_current_metadata <- function(", 1),
    ("elapsed <- as.numeric(difftime(now, metadata_notif_state$last", 1),
    ("session$onFlushed(function()", 1),
    ("later::later(function()", 1),
):
    assert METADATA_PARTS.count(fragment) == expected, (fragment, METADATA_PARTS.count(fragment))

for invariant in (
    "ignoring empty skeleton to preserve existing metadata",
    "skipping stale metadata overwrite",
    "updating from rule-applied metadata",
    "restore_has_valid_canonical_pair",
    "Metadata paste rejected:",
    "Metadata paste contained undefined condition value(s):",
    "metadata_sync_pending(TRUE)",
    "suppress_metadata_edit_echo(TRUE)",
    'freezeReactiveValue(input, "metadata_table")',
):
    assert invariant in METADATA_PARTS, invariant

assert "list(set_current_metadata = set_current_metadata)" in METADATA_PARTS
print("Data Wizard metadata observer extraction contract checks passed")
