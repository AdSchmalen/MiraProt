"""Static contracts for PCA observer safety during full-session restore."""
from pathlib import Path


OBSERVERS = Path("modules/PCA/pca_module_server_observers.R").read_text()
PIPELINE = Path("modules/PCA/pca_module_server_pipeline.R").read_text()
MODULE = Path("modules/pca_module.R").read_text()
STATE = Path("modules/PCA/pca_module_state.R").read_text()


def test_destructive_target_and_identifier_observers_are_guarded() -> None:
    target = OBSERVERS.split("observeEvent(input$comparison_target", 1)[1]
    target = target.split("}, ignoreInit = TRUE)", 1)[0]
    identifier = PIPELINE.split("observeEvent(input$GeneIdentifierColumn_pca", 1)[1]
    identifier = identifier.split("}, ignoreNULL = TRUE)", 1)[0]
    assert "state$restore_in_progress()" in target
    assert "restored_comparison_target" in target
    assert "state$restore_in_progress()" in identifier
    assert "restored_identifier_column" in identifier


def test_method_and_master_control_echoes_cannot_mutate_restored_state() -> None:
    assert 'consume_restore_echo("analysis_method"' in OBSERVERS
    for input_id in (
        "masterLabelColor_pca",
        "masterDotColor_pca",
        "masterCustomDot_pca",
    ):
        body = OBSERVERS.split(f"observeEvent(input${input_id}", 1)[1]
        body = body.split("})", 1)[0]
        assert "restore_in_progress" in body
        assert f'consume_restore_echo("{input_id}"' in body


def test_restore_stages_hazardous_echoes_and_has_one_release_path() -> None:
    assert "expected_restore_input_echoes = reactiveVal(list())" in STATE
    for input_id in (
        "comparison_target",
        "GeneIdentifierColumn_pca",
        "analysis_method",
        "custom_col_sel_pca",
        "select_samples_pca",
        "masterLabelColor_pca",
    ):
        assert f'"{input_id}"' in MODULE
    trigger = MODULE.split("observeEvent(rv$session_restore_trigger, {", 1)[1]
    trigger = trigger.split("}, ignoreInit = TRUE)", 1)[0]
    assert trigger.count("session$onFlushed(") == 1
    assert trigger.count("pca_state$restore_in_progress(FALSE)") == 1
    assert trigger.count("pca_state$render_nonce(isolate(pca_state$render_nonce()) + 1L)") == 1
    assert "rows_requested" not in OBSERVERS


def test_choice_population_prefers_staged_restore_values() -> None:
    input_observer = OBSERVERS.split("register_pca_input_observers <- function", 1)[1]
    input_observer = input_observer.split("register_pca_protein_selection_observers <- function", 1)[0]
    for input_id in (
        "custom_col_sel_pca",
        "GeneIdentifierColumn_pca",
        "select_samples_pca",
    ):
        assert f"pending_ui_inputs()${input_id}" in input_observer
