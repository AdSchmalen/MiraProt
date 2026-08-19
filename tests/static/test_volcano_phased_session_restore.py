from pathlib import Path


MODULE = Path("modules/volcano_module.R").read_text()
OBSERVER = Path(
    "modules/Volcano/volcano_observers_selection_restore.R"
).read_text()
REGISTRATION = Path(
    "R/session_save_restore/session_save_restore_module_registration.R"
).read_text()


def test_volcano_setter_separates_state_and_plot_phases():
    setter = MODULE.split("set_session_state = function(state, phase = NULL)", 1)[1]
    plot_phase, state_phase = setter.split(
        'if (!is.null(phase) && !identical(phase, "full_module_state")) return()', 1
    )

    assert 'if (identical(phase, "full_module_plots")) {' in plot_phase
    assert "restore_rebuild_requested" in plot_phase
    assert "pending_ui_inputs" not in plot_phase
    assert "current_pairs" not in plot_phase
    assert "volcano_labels(" not in plot_phase
    assert "pending_ui_inputs" in state_phase
    assert "had_static_plots_on_save" in state_phase


def test_missing_cache_warning_is_deferred_to_trigger_and_unique():
    warning = "Volcano plot could not be restored because cached plot data is unavailable."
    setter = MODULE.split("set_session_state = function(state, phase = NULL)", 1)[1]
    trigger = OBSERVER.split("observeEvent(rv$session_restore_trigger, {", 1)[1]

    assert warning not in setter
    assert trigger.count(warning) == 1
    assert ".module_restore_live_contract_compatible" in trigger


def test_registry_tracks_state_apply_and_rebuild_request_separately():
    volcano_registration = REGISTRATION.split("# Volcano Module", 1)[1].split(
        "# PCA Module", 1
    )[0]

    assert "invoke_module_set_session_state(volcano, state$module_state, phase)" in volcano_registration
    assert 'restore_state_applied <- isTRUE(invoked) && identical(phase, "full_module_state")' in volcano_registration
    assert 'rebuild_requested <- isTRUE(invoked) && identical(phase, "full_module_plots")' in volcano_registration
    assert "plot_recreated <- invoke_module_set_session_state" not in volcano_registration
