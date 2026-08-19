#!/usr/bin/env python3
"""Contract checks for the explicit Apply Metadata Rules integration bridge."""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PARENT = (ROOT / "modules/datawizard_module.R").read_text()
TABLES = (ROOT / "modules/Data Wizard/datawizard_tables.R").read_text()
METADATA = "\n".join(
    (ROOT / f"modules/Data Wizard/tables/datawizard_tables_observer_metadata_{phase}.R").read_text()
    for phase in ("hydration", "sync", "editing")
)
ASSIGN = (ROOT / "modules/Data Wizard/datawizard_assign_rules.R").read_text()
ENGINE = (ROOT / "modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R").read_text()

# The assign-rules public interface and trigger contract remain in their owner.
assert "observeEvent(input$apply_metadata_rules_btn" in ASSIGN
assert "apply_metadata_rules_trigger = apply_metadata_rules_trigger" in ASSIGN
assert "apply_metadata_rules_trigger <- assign_rules_state$apply_metadata_rules_trigger" in ASSIGN

# Tables exposes one owned transactional setter rather than exposing a parent write.
assert "set_current_metadata     = tables_api$set_current_metadata" in TABLES
for fragment in (
    'if (!is.data.frame(metadata))',
    'metadata_aligned_with_primary(metadata, reference)',
    'mark_programmatic_metadata_sync()',
    'set_metadata(metadata)',
    'current_handson_metadata(metadata)',
    'metadata_options_refresh(isolate(metadata_options_refresh()) + 1L)',
    'list(success = TRUE, reason = "metadata committed"',
    'set_metadata(previous_canonical)',
):
    assert fragment in METADATA, fragment

bridge_start = PARENT.index("# Observer: Explicit re-application of metadata rules via button")
bridge_end = PARENT.index('debug_log("Simplified data flow management initialized"', bridge_start)
bridge = PARENT[bridge_start:bridge_end]

# Source precedence covers blank canonical metadata, filled local metadata, and
# paused-sync local edits; snapshotting also makes repeated clicks independent.
positions = [bridge.index(token) for token in (
    "aligned(table_meta)", "aligned(core_meta)", "aligned(canonical_meta)"
)]
assert positions == sorted(positions)
assert "unserialize(serialize(selected, NULL))" in bridge
assert "auto_assign_out$apply_rules(snapshot)" in bridge

# Content, Sample and ratio changes all travel through the same validated frame;
# no-match/no-change still verifies both live mirrors before notification.
for field in ("Column", "fields_changed", "rows_changed", "live_table", "live_canonical"):
    assert field in bridge
assert bridge.index("if (!isTRUE(commit$success))") < bridge.index("extracted_values <-")
assert bridge.index("canonical_ok <-") < bridge.index('if (identical(new_meta, snapshot))')
assert "set_current_metadata(new_meta" not in bridge  # setter is invoked through its public handle
assert "primary_data_state$set_metadata_for_current_data(new_meta)" in bridge  # compatibility only
assert "Tables setter unavailable; using canonical setter fallback" in bridge

# Matching assignments overwrite filled values as promised; unmatched rows are
# not rebuilt and therefore retain every field/value from the input frame.
assert "df_working <- metadata_df" in ENGINE
assert "df$Content[idx] <- content_term" in ENGINE
assert "df$Sample[group_rows[group_needed]] <- samples[group_needed]" in ENGINE
assert "df$Numerator[row_idx] <- components$numerator" in ENGINE
assert "df$Denominator[row_idx] <- components$denominator" in ENGINE

# Lifecycle cleanup is registered before rule execution, covering success,
# no-match, failed commit, and repeated-trigger error paths.
assert bridge.index("on.exit({") < bridge.index("apply_rules(snapshot)")
assert 'set_assignment_state(FALSE, if (ready)' in bridge
assert bridge.count('type = "error"') == 1

print("Apply Metadata Rules bridge contract checks passed")
