from pathlib import Path


SOURCE = Path(
    "modules/Data Wizard/core/datawizard_core_lifecycle_observers.R"
).read_text()
REGISTRATION = Path(
    "R/session_save_restore/session_save_restore_module_registration.R"
).read_text()


def test_restore_metadata_sync_defers_mismatched_replay_state() -> None:
    observer_start = SOURCE.index("current_metadata <- core_values$handson_metadata()")
    observer_end = SOURCE.index("if (!core_values$metadata_observer_active())", observer_start)
    observer = SOURCE[observer_start:observer_end]

    guard = observer.index("if (restore_guard_active())")
    alignment = observer.index("!metadata_matches_dataset(current_metadata, current_primary)")
    defer = observer.index("deferring synchronization")
    setter = observer.index("primary_data_state$set_metadata_for_current_data(current_metadata)")

    assert guard < alignment < defer < setter
    assert "return()" in observer[defer:setter]


def test_restore_guard_outlives_restore_trigger_consumers() -> None:
    phase_start = REGISTRATION.index("# Phase 5: notify loader/submodule restore observers")
    phase_end = REGISTRATION.index(
        'debug_log(paste("Data Wizard session state restored; fields:"', phase_start
    )
    phase = REGISTRATION[phase_start:phase_end]

    trigger = phase.index("rv$session_restore_trigger <-")
    deferred_release = phase.index("session$onFlushed(once = TRUE, finalize_restore_guard)")
    release = phase.index("rv$session_restoring <- FALSE")

    assert trigger < release < deferred_release
    assert "deferred guard release until replay consumers flushed" in phase
