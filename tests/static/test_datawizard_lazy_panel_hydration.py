from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PARENT = (ROOT / "modules/datawizard_module.R").read_text()
INTEGRATION = (ROOT / "modules/Data Wizard/datawizard_integration.R").read_text()
PIVOT = (ROOT / "modules/Data Wizard/pivot/datawizard_pivot_observer.R").read_text()
BATCH = (ROOT / "modules/Data Wizard/batch_effects/datawizard_batch_effects_handlers.R").read_text()
IMPUTATION = (ROOT / "modules/Data Wizard/imputation/datawizard_imputation_state_configuration.R").read_text()


def test_toggle_contract_has_distinct_monotonic_opened_and_initialized_state():
    assert "advanced_panel_initialized <- register_datawizard_ui_toggle_handlers(input)" in PARENT
    assert "opened <- stats::setNames(" in INTEGRATION
    assert "initialized <- stats::setNames(" in INTEGRATION
    assert "initialized[[name]](TRUE)" in INTEGRATION
    assert "initialized[[name]](FALSE)" not in INTEGRATION
    assert "list(opened = opened, initialized = initialized)" in INTEGRATION


def test_collapsed_restore_configuration_is_staged_before_hydration():
    # Registration and config bridges remain eager; only expensive UI work is gated.
    assert 'modPivotServer(' in INTEGRATION
    assert 'UI_config = ui_config_functions$create_pivot_ui_config' in INTEGRATION
    assert 'pivot_options_state(clean_opts)' in (ROOT / "modules/Data Wizard/datawizard_pivot.R").read_text()
    assert "req(isTRUE(ctx$initialized()))" in PIVOT
    assert 'outputOptions(ctx$output, "pivot_options", suspendWhenHidden = FALSE)' not in PIVOT


def test_fresh_upload_does_not_hydrate_advanced_choices_until_opened():
    assert 'initialized = panel_initialized("processing")' in INTEGRATION
    assert 'initialized = panel_initialized("imputation")' in INTEGRATION
    assert "req(isTRUE(initialized()))" in BATCH
    assert "if (!isTRUE(initialized())) return(invisible(FALSE))" in IMPUTATION
    assert PIVOT.index("req(isTRUE(ctx$initialized()))") < PIVOT.index('ctx$debug_log(paste("Generating UI for"')
    assert BATCH.index("req(isTRUE(initialized()))") < BATCH.index('debug_log(paste("Synchronized batch inputs for"')
    assert IMPUTATION.index("if (!isTRUE(initialized())) return(invisible(FALSE))") < IMPUTATION.index('debug_log("Data column signature changed; recomputed imputation choices"')


if __name__ == "__main__":
    test_toggle_contract_has_distinct_monotonic_opened_and_initialized_state()
    test_collapsed_restore_configuration_is_staged_before_hydration()
    test_fresh_upload_does_not_hydrate_advanced_choices_until_opened()
