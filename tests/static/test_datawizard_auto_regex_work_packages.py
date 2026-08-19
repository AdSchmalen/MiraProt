from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[2]
AUTO_REGEX = ROOT / "modules" / "Data Wizard" / "auto regex"
UI = (AUTO_REGEX / "datawizard_auto_regex_UI.R").read_text()
HANDLERS = (AUTO_REGEX / "datawizard_auto_regex_handlers.R").read_text()
INTEGRATION = (ROOT / "modules" / "Data Wizard" /
               "datawizard_integration.R").read_text()


def _selector_call_after(marker):
    """Return the balanced selector call immediately following a marker."""
    tail = INTEGRATION[INTEGRATION.index(marker):]
    start = tail.index("select_datawizard_primary_display_data(")
    depth = 0
    for index, char in enumerate(tail[start:], start=start):
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return re.sub(r"\s+", " ", tail[start:index + 1])
    raise AssertionError("unbalanced primary display-data selector call")


def test_tables_and_auto_regex_share_the_exact_display_data_selector():
    tables = _selector_call_after("# Initialize tables module")
    auto_regex = _selector_call_after("auto_regex_working_data <- reactive({")
    assert tables == auto_regex
    assert 'context = "Tables"' in auto_regex
    assert "publish_raw_if_missing = TRUE" in auto_regex


def test_compact_ui_keeps_bootstrap_structure_and_stable_ids():
    assert re.search(
        r'ns\("excel_file"\).*?shiny::column\(\s*6,.*?'
        r'ns\("excel_worksheet_controls"\)', UI, re.S)
    assert "mapping_fields[1:3]" in HANDLERS
    assert "mapping_fields[4:6]" in HANDLERS
    assert "shiny::column(3, mapping_control(field))" in HANDLERS
    assert re.search(r'shiny::fluidRow\(\s*shiny::column\(\s*3,.*?'
                     r'ns\("redundancy"\)', UI, re.S)
    assert re.search(r'shiny::column\(\s*3,\s*offset = 9,.*?'
                     r'ns\("infer_rules"\)', UI, re.S)
    assert 'ns("validation_text")' in UI
    assert 'ns("validation_table")' not in UI
    assert "output$validation_text" in HANDLERS
    assert "output$validation_table" not in HANDLERS


def test_handler_has_one_terminal_notification_and_no_attached_dependency():
    # All expected click outcomes converge on the single on.exit notification.
    assert HANDLERS.count("shiny::showNotification(") == 1
    assert "if (isTRUE(state$processing())) return()" in HANDLERS
    assert "transfer(payload, notify = FALSE)" in HANDLERS
    assert "transfer(previous, notify = FALSE)" in HANDLERS

    auto_regex_sources = "\n".join(
        path.read_text() for path in AUTO_REGEX.glob("*.R")
    )
    assert not re.search(r"(?m)^\s*(library|require)\s*\(", auto_regex_sources)
    assert "install.packages(" not in auto_regex_sources
    assert 'requireNamespace("readxl", quietly = TRUE)' in HANDLERS


def test_both_sources_and_public_auto_assign_loader_remain_wired():
    assert '"Current MiraProt metadata" = "current_metadata"' in UI
    assert '"Excel workbook" = "excel"' in UI
    assert "metadata = auto_regex_metadata" in INTEGRATION
    assert "data = auto_regex_working_data" in INTEGRATION
    assert "modules$tables_out$current_metadata()" in INTEGRATION
    assert "modules$auto_assign_out$load_rules_directly(rules, notify = notify)" in INTEGRATION
