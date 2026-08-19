from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / "modules" / "Data Wizard" / "auto regex"


def test_pure_layers_exclude_standalone_runtime_and_shiny_lifecycle():
    text = "\n".join((BASE / name).read_text() for name in (
        "datawizard_auto_regex_utils.R", "datawizard_auto_regex_logic.R"
    ))
    forbidden = ("install.packages(", "observeEvent(", "downloadHandler(",
                 "runApp(", "reactiveValues(", "create_session_logger")
    assert not any(token in text for token in forbidden)


def test_inference_orchestration_and_primary_contract_live_in_logic():
    utils = (BASE / "datawizard_auto_regex_utils.R").read_text()
    logic = (BASE / "datawizard_auto_regex_logic.R").read_text()
    for function in ("infer_content", "infer_conditions", "infer_ratios",
                     "auto_regex_infer_rules"):
        assert f"{function} <- function" in logic
        assert f"{function} <- function" not in utils
    for key in ("rules=", "statuses=", "diagnostics=", "warnings=",
                "errors=", "timings="):
        assert key in logic
