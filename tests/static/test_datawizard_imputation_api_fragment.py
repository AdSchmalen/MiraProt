from pathlib import Path


SOURCE = Path(
    "modules/Data Wizard/imputation/datawizard_imputation_observers_api.R"
).read_text(encoding="utf-8")


def test_imputation_api_fragment_assigns_complete_public_api():
    assert ".imputation_api <- list(" in SOURCE
    assert "get_session_state = imputation_session_state$get_session_state" in SOURCE
    assert "get_current_imputation_state = function()" in SOURCE
    assert "get_full_state = function()" in SOURCE


def test_imputation_state_fallback_returns_its_local_list():
    fallback_start = SOURCE.index("# Fallback to internal state")
    fallback_end = SOURCE.index("# Enhanced version", fallback_start)
    fallback = SOURCE[fallback_start:fallback_end]

    assert "return(list(" in fallback
    assert ".imputation_api <- list(" not in fallback
