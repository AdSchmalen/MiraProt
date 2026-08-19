#!/usr/bin/env python3
"""Static regression checks for Data Wizard restore module-count diagnostics."""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REGISTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_module_registration.R"
CORE_HELPERS = ROOT / "R" / "session_save_restore" / "session_save_restore_core_helpers.R"
ORCHESTRATION = ROOT / "R" / "session_save_restore" / "session_save_restore_orchestration.R"

registration = REGISTRATION.read_text()
core_helpers = CORE_HELPERS.read_text()
orchestration = ORCHESTRATION.read_text()

assert "session_registry$current_restore_snapshot_ids()" in registration
assert '"restored_non_datawizard_module_count=", module_counts$restored' in registration
assert '"registered_non_datawizard_module_count=", module_counts$registered' in registration
assert "non_datawizard_module_restore_count=" not in registration
assert "current_restore_snapshot_ids = function()" in core_helpers
assert "restore_context_snapshots = module_snapshots" in orchestration

# Representative Data & Analysis payload: only snapshots in this payload count
# as restored, regardless of any additional full-session registry participants.
payload = {
    "save_level": "data_and_analysis",
    "module_snapshots": {"datawizard": {}, "go": {}, "gsea": {}},
}
restored_non_datawizard_module_count = len(
    set(payload["module_snapshots"]) - {"datawizard"}
)
assert restored_non_datawizard_module_count == 2

print("Data Wizard restore snapshot count static checks passed")
