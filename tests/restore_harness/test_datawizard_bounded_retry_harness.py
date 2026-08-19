#!/usr/bin/env python3
"""Scripted Data Wizard submodule restore harness for bounded dynamic UI replay.

The harness models the target behavior of
modules/Data Wizard/datawizard_core.R::create_submodule_session_state() without
starting a Shiny runtime.  It stages Assign Rules-style dynamic text inputs
(textin1, textin2, ...) plus one hidden input that never binds, then verifies the
bounded retry/drop/dedupe invariants that protect session restore from noisy or
unbounded replay loops.
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "modules" / "Data Wizard" / "datawizard_core.R"


def stable_signature(payload: dict[str, Any]) -> str:
    encoded = repr(sorted(payload.items())).encode("utf-8")
    return hashlib.sha1(encoded).hexdigest()


@dataclass
class BoundedRestoreHarness:
    """Pure restore state machine mirroring the core bounded retry behavior."""

    input_specs: dict[str, str]
    bound_inputs: set[str]
    max_restore_attempts: int = 5
    input_values: dict[str, Any] = field(default_factory=dict)
    pending: dict[str, Any] | None = None
    generation: int = 0
    attempt: int = 0
    last_applied_signature: str | None = None
    warnings: list[str] = field(default_factory=list)
    logs: list[str] = field(default_factory=list)
    apply_counts: dict[str, int] = field(default_factory=dict)
    restore_triggers: int = 0
    queued_post_stabilization_retry: bool = False
    callback_queue: list[str] = field(default_factory=list)

    def stage(self, ui_inputs: dict[str, Any]) -> str:
        self.generation += 1
        self.attempt = 0
        self.pending = dict(ui_inputs)
        signature = self.signature_key(self.generation, self.pending)
        self.logs.append(f"staged generation={self.generation} signature={signature}")
        return signature

    def signature_key(self, generation: int, pending: dict[str, Any] | None) -> str:
        return f"{generation}:{stable_signature(pending or {})}"

    def schedule_retry_once(self) -> None:
        if self.queued_post_stabilization_retry:
            return
        self.queued_post_stabilization_retry = True
        self.callback_queue.append("post_stabilization_retry")
        self.logs.append("scheduled post_stabilization_retry")

    def restore_trigger(self) -> None:
        self.restore_triggers += 1
        self.apply_pending_once("restore_trigger")
        while self.callback_queue:
            reason = self.callback_queue.pop(0)
            if reason == "post_stabilization_retry":
                self.queued_post_stabilization_retry = False
            self.apply_pending_once(reason)

    def apply_pending_once(self, reason: str) -> None:
        if not self.pending:
            return

        signature_key = self.signature_key(self.generation, self.pending)
        if self.last_applied_signature == signature_key:
            self.logs.append(f"skipped already applied {signature_key}")
            self.pending = None
            return

        bound = {k: v for k, v in self.pending.items() if k in self.bound_inputs}
        deferred = {k: v for k, v in self.pending.items() if k not in self.bound_inputs}

        for input_id, value in bound.items():
            self.input_values[input_id] = value
            self.apply_counts[input_id] = self.apply_counts.get(input_id, 0) + 1

        if deferred:
            next_attempt = self.attempt + 1
            assert next_attempt <= self.max_restore_attempts, "attempts must never exceed max_restore_attempts"
            self.attempt = next_attempt
            if self.attempt >= self.max_restore_attempts:
                unresolved = ",".join(sorted(deferred))
                self.warnings.append(
                    f"dropping unresolved inputs after {self.max_restore_attempts} attempt(s): {unresolved}"
                )
                self.pending = None
                return
            self.pending = dict(deferred)
            self.logs.append(
                f"deferred {','.join(sorted(deferred))} reason={reason} attempt={self.attempt}"
            )
            self.schedule_retry_once()
            return

        self.last_applied_signature = signature_key
        self.pending = None


def assert_scripted_datawizard_restore_harness() -> None:
    payload = {
        # Assign Rules dynamically renders these controls as textin1, textin2, ...
        "textin1": "Treatment",
        "textin2": "Control",
        "textin3": "Recovery",
        # Hidden/conditional control that never binds in this fixture.
        "hidden_condition_template": "legacy-hidden-value",
    }
    harness = BoundedRestoreHarness(
        input_specs={
            "textin1": "textInput",
            "textin2": "textInput",
            "textin3": "textInput",
            "hidden_condition_template": "textInput",
        },
        bound_inputs={"textin1", "textin2", "textin3"},
        max_restore_attempts=5,
    )

    harness.stage(payload)
    harness.restore_trigger()

    assert harness.restore_triggers == 1, "Data Wizard restore must need exactly one restore trigger"
    assert harness.attempt == harness.max_restore_attempts, "hidden unresolved input should stop at the configured cap"
    assert harness.attempt <= harness.max_restore_attempts, "retry attempts must never exceed max_restore_attempts"
    assert harness.input_values == {
        "textin1": "Treatment",
        "textin2": "Control",
        "textin3": "Recovery",
    }, "visible Assign Rules textin* inputs must be applied before unresolved hidden inputs are dropped"
    assert harness.pending is None, "restore must complete after unresolved hidden input is dropped"
    assert len(harness.warnings) == 1, "unresolved hidden inputs must be dropped once"
    assert "dropping unresolved inputs after 5 attempt(s)" in harness.warnings[0]
    assert "hidden_condition_template" in harness.warnings[0], "drop warning must identify unresolved hidden input"
    assert harness.logs.count("scheduled post_stabilization_retry") == harness.max_restore_attempts - 1, (
        "post_stabilization_retry must be bounded and must not spam beyond the retry budget"
    )

    applied = BoundedRestoreHarness(
        input_specs={"textin1": "textInput", "textin2": "textInput"},
        bound_inputs={"textin1", "textin2"},
        max_restore_attempts=5,
    )
    applied.stage({"textin1": "Treatment", "textin2": "Control"})
    applied.restore_trigger()
    assert applied.apply_counts == {"textin1": 1, "textin2": 1}

    # Simulate the same generation/signature being observed again; the target
    # implementation should clear it without replaying update*Input calls.
    applied.pending = {"textin1": "Treatment", "textin2": "Control"}
    applied.apply_pending_once("input_bind_change")
    assert applied.apply_counts == {"textin1": 1, "textin2": 1}, (
        "already-applied generation/signature payloads must not be replayed"
    )


def assert_core_contains_bounded_retry_guards() -> None:
    core = CORE.read_text()
    assert "max_restore_attempts = 5L" in core
    assert "attempt + 1L >= max_restore_attempts" in core
    assert "drop_exhausted_restore(st, reason, remaining_ids)" in core
    assert "restore warning: dropping unresolved inputs after " in core
    assert "exhausted_restore_keys" in core, "drop warning should be deduped per generation/signature"
    assert "active_retry_keys" in core, "scheduled retry callbacks should be deduped"
    assert "post_stabilization_retry" in core
    assert "last_applied_signature" in core
    assert "generation/signature already applied" in core
    assert re.search(r"remaining_ids <- unique\(c\(deferred_ids, unresolved_ids\)\).*?st\$attempt <- attempt \+ 1L", core, re.S), (
        "unresolved dynamic inputs must be requeued with a bounded attempt counter"
    )


if __name__ == "__main__":
    assert_scripted_datawizard_restore_harness()
    assert_core_contains_bounded_retry_guards()
    print("Data Wizard bounded restore harness checks passed")
