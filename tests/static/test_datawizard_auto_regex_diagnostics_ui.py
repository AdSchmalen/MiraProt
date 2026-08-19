from pathlib import Path

BASE = Path(__file__).resolve().parents[2] / "modules" / "Data Wizard" / "auto regex"
HANDLERS = (BASE / "datawizard_auto_regex_handlers.R").read_text()
UI = (BASE / "datawizard_auto_regex_UI.R").read_text()


def test_all_eight_panels_have_namespaced_lazy_toggle_handlers():
    for name in ("validation", "content_rules", "content_diagnostics",
                 "condition_rules", "condition_diagnostics", "ratio_rules",
                 "ratio_diagnostics", "run_diagnostics"):
        assert f'"{name}"' in HANDLERS
        assert f'"{name}"' in UI
    assert "shiny::req(isTRUE(panel_open[[name]]))" in HANDLERS
    assert 'ns(paste0(name, "_content"))' in HANDLERS
    assert "asis = TRUE" in HANDLERS


def test_tables_are_bounded_paginated_and_render_failures_are_local():
    for token in ("diagnostic_row_limit <- 500L", "pageLength = 10L",
                  "scrollX = TRUE", "deferRender = TRUE", "tryCatch("):
        assert token in HANDLERS
    assert "Rendering error" in HANDLERS
    assert "TP (true positive)" in UI
    assert "coverage is the share" in UI
