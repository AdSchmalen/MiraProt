from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[2]
APP = (ROOT / "app.R").read_text(encoding="utf-8")
MODULE = (ROOT / "modules" / "datawizard_module.R").read_text(encoding="utf-8")
UTILS = (ROOT / "modules" / "Data Wizard" / "auto regex" /
         "datawizard_auto_regex_utils.R").read_text(encoding="utf-8")
RATIOS = (ROOT / "modules" / "Data Wizard" /
          "datawizard_ratios.R").read_text(encoding="utf-8")


RUNTIME_UI_FILES = tuple(
    path
    for source_root in (ROOT / "R", ROOT / "modules", ROOT / "Documentation")
    for path in source_root.rglob("*.R")
)


def test_runtime_ui_has_no_trailing_missing_arguments():
    """Do not pass empty trailing arguments into Shiny/rlang dots collectors."""
    offenders = []
    for path in RUNTIME_UI_FILES:
        source = path.read_text(encoding="utf-8")
        for match in re.finditer(r",\s*\)", source):
            offenders.append(f"{path.relative_to(ROOT)}:{source.count(chr(10), 0, match.start()) + 1}")
    assert not offenders, "trailing missing UI arguments: " + ", ".join(offenders)


def test_required_module_source_errors_are_not_swallowed_before_ui_build():
    """A partial modEnv must never reach build_ui as a missing constructor."""
    source_loop = APP[APP.index('for (f in list.files("modules"'):
                      APP.index('for (f in list.files("R"')]
    assert 'stop(message, call. = FALSE)' in source_loop
    assert 'debug_log(paste("Error loading"' not in source_loop


def test_ratio_contrast_references_are_built_before_constructor_call():
    """Keep the contrast call readable and avoid nested-parenthesis parse failures."""
    assert "reference_columns <- c(" in RATIOS
    assert "!is.na(reference_columns) & nzchar(reference_columns)" in RATIOS
    assert "Filter(nzchar, unique(stats::na.omit(trimws(c(" not in RATIOS


def test_content_upgrade_validates_the_upgraded_frame():
    """The upgrader must not refer to constructor-local variables."""
    start = UTILS.index("upgrade_rule_component <- function")
    end = UTILS.index("canonical_prerequisite_rules <- function", start)
    body = UTILS[start:end]

    assert "out$Transformation" in body
    assert "out$Content" in body
    assert "values$Transformation" not in body
    assert not re.search(r"\bcontent\s*%in%\s*TRANSFORMATION_CONTENT_TYPES", body)
