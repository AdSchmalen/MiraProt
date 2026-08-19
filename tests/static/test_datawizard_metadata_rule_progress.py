from pathlib import Path


ROOT = Path(__file__).parents[2]
ORCHESTRATOR = (ROOT / "modules/datawizard_module.R").read_text()
ENGINE = (ROOT / "modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R").read_text()
AUTO_ASSIGN = (ROOT / "modules/Data Wizard/datawizard_auto_assign.R").read_text()


def ordered(text, labels):
    positions = [text.index(f'update_stage("{label}")') for label in labels]
    assert positions == sorted(positions)


def test_shared_progress_scope_replaces_detail_and_is_exception_safe():
    helper = ORCHESTRATOR[ORCHESTRATOR.index("with_metadata_rule_progress <- function") :]
    assert 'shiny::withProgress(message = "Applying metadata rules"' in helper
    assert "shiny::setProgress(" in helper
    assert "detail = stage" in helper
    assert "metadata_rule_application_in_progress(TRUE)" in ORCHESTRATOR
    assert ORCHESTRATOR.count("on.exit(metadata_rule_application_in_progress(FALSE), add = TRUE)") == 2


def test_automatic_and_manual_orchestration_stages_are_ordered():
    automatic = ORCHESTRATOR[
        ORCHESTRATOR.index("apply_rules_safely_impl <- function") :
        ORCHESTRATOR.index("apply_rules_safely <- function", ORCHESTRATOR.index("apply_rules_safely_impl <- function") + 1)
    ]
    ordered(automatic, ["Reading metadata rule file", "Loading assignment rules"])
    assert 'update_stage("Loading edit operations")' in automatic
    assert 'update_stage("Updating condition groups")' in automatic
    assert 'update_stage("Finalizing metadata")' in automatic

    manual = ORCHESTRATOR[ORCHESTRATOR.index("# Observer: Explicit re-application") :]
    ordered(manual, [
        "Resolving current metadata", "Copying current metadata",
        "Applying metadata rules", "Validating metadata", "Committing metadata",
        "Synchronizing condition groups", "Finalizing state",
    ])


def test_restore_suppresses_progress_and_engine_callback_is_optional():
    assert 'interactive_apply <- identical(source, "interactive") && isTRUE(apply_to_metadata)' in ORCHESTRATOR
    assert "with_metadata_rule_progress(interactive_apply" in ORCHESTRATOR
    assert "progress_callback = function(stage) invisible(NULL)" in ENGINE
    assert "progress_callback = progress_callback" in AUTO_ASSIGN


def test_engine_reports_only_active_phases_in_order():
    phases = [
        "Applying content rules", "Applying sample rules",
        "Applying ratio rules", "Finalizing metadata",
    ]
    positions = [ENGINE.index(f'report_progress("{phase}")') for phase in phases]
    assert positions == sorted(positions)
    assert "Content rules applied" not in ENGINE
