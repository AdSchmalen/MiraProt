#!/usr/bin/env python3
"""Regression checks for per-run GSEA worker auto-allocation."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OBSERVER = ROOT / "modules" / "GSEA" / "GSEA_module_observer.R"
STATE = ROOT / "modules" / "GSEA" / "GSEA_module_state.R"

observer_text = OBSERVER.read_text()
state_text = STATE.read_text()

assert "gsea_session_workers" not in observer_text
assert "gsea_session_workers" not in state_text
assert "requested_cores <- NULL" in observer_text
assert "requested_cores  = requested_cores" in observer_text

for diagnostic in ("last_workers_requested", "last_workers_effective"):
    assert f"{diagnostic} <- reactiveVal(NULL)" in state_text
    assert f"{diagnostic}(NULL)" in observer_text

assert "last_workers_effective(as.integer(workers_used))" in observer_text
assert "last_workers_requested(as.integer(workers_used))" not in observer_text

print("GSEA worker allocation remains per-run")
