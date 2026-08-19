"""Contract checks for identifier dropdown hydration during PCA restore."""
from pathlib import Path


PCA_MODULE = Path("modules/pca_module.R")
PCA_PIPELINE = Path("modules/PCA/pca_module_server_pipeline.R")
PCA_STATE = Path("modules/PCA/pca_module_state.R")
PCA_OBSERVERS = Path("modules/PCA/pca_module_server_observers.R")


def test_identifier_restore_marker_is_part_of_pca_state() -> None:
    source = PCA_STATE.read_text()
    assert "restored_identifier_column = reactiveVal(NULL)" in source


def test_restore_stages_authoritative_identifier_with_ui_fallback() -> None:
    source = PCA_MODULE.read_text()
    setter = source.split("set_session_state = function(state, phase = NULL)", 1)[1]
    assert "restored_identifier <- state$analysis_results$identifier_col %||%" in setter
    assert "state$plot_ui_inputs$GeneIdentifierColumn_pca" in setter
    assert "pca_state$restored_identifier_column(restored_identifier)" in setter


def test_identifier_observer_guards_results_and_consumes_only_matching_echo() -> None:
    source = PCA_PIPELINE.read_text()
    observer = source.split("# Automatic Re-Run on Identifier Change", 1)[1]
    observer = observer.split("# Static Plot Rendering", 1)[0]

    guard = observer.index("if (isTRUE(state$restore_in_progress()))")
    first_results_read = observer.index("analysis_results()")
    assert guard < first_results_read
    assert "[PCA] Skipping identifier reanalysis during session restore" in observer

    marker_read = "restored_id <- isolate(state$restored_identifier_column())"
    marker_clear = "state$restored_identifier_column(NULL)"
    matching_echo = "if (identical(restored_id, new_id))"
    assert marker_read in observer
    assert marker_clear in observer
    assert matching_echo in observer
    assert observer.index(marker_clear) < observer.index(matching_echo) < first_results_read


def test_restore_hydrates_identifier_choices_and_selection_atomically() -> None:
    source = PCA_MODULE.read_text()
    restore = source.split("observeEvent(rv$session_restore_trigger, {", 1)[1]
    restore = restore.split('debug_log("[PCA] session restore: identifier', 1)[0]

    assert 'updateSelectInput(session, "GeneIdentifierColumn_pca"' in restore
    assert 'if (identical(id, "GeneIdentifierColumn_pca")) next' in restore
    assert "saved_identifier %in% identifier_choices" in restore
    assert restore.index('updateSelectInput(session, "GeneIdentifierColumn_pca"') < restore.index("for (updater_name")


def test_startup_observer_defers_identifier_default_during_restore() -> None:
    source = PCA_OBSERVERS.read_text()
    identifier_section = source.split("# Observer 2 Logic: Update identifier column choices", 1)[1]
    identifier_section = identifier_section.split("# Observer 3 Logic", 1)[0]

    assert "identifier_restore_pending" in identifier_section
    assert "Deferring identifier choices to session restore updater" in identifier_section


def test_safe_fallback_echo_does_not_rerun_saved_analysis() -> None:
    source = PCA_PIPELINE.read_text()
    observer = source.split("# Automatic Re-Run on Identifier Change", 1)[1]
    observer = observer.split("# Static Plot Rendering", 1)[0]

    expected_guard = "if (!is.null(expected_identifier_echo) && identical(expected_identifier_echo, new_id))"
    assert expected_guard in observer
    assert observer.index(expected_guard) < observer.index("analysis_results()")
