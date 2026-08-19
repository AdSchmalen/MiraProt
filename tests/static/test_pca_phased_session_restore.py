"""Contract checks for PCA's two-phase full-session restore."""
from pathlib import Path


PCA_MODULE = Path("modules/pca_module.R")
PCA_PIPELINE = Path("modules/PCA/pca_module_server_pipeline.R")
REGISTRATION = Path(
    "R/session_save_restore/session_save_restore_module_registration.R"
)


def test_pca_setter_accepts_phase_and_keeps_legacy_default() -> None:
    source = PCA_MODULE.read_text()
    assert "set_session_state = function(state, phase = NULL)" in source
    assert 'if (identical(phase, "full_module_plots")) {' in source
    assert 'if (!is.null(phase) && !identical(phase, "full_module_state")) return()' in source


def test_plot_phase_returns_before_state_and_ui_hydration() -> None:
    source = PCA_MODULE.read_text()
    setter = source.split("set_session_state = function(state, phase = NULL)", 1)[1]
    plot_phase = setter.split('if (identical(phase, "full_module_plots")) {', 1)[1]
    plot_phase, state_phase = plot_phase.split(
        'if (!is.null(phase) && !identical(phase, "full_module_state")) return()', 1
    )

    assert "pca_restore_rebuild_expected" in plot_phase
    assert "return()" in plot_phase
    for state_write in (
        "pending_ui_inputs(state$plot_ui_inputs)",
        "sample_label_settings_pca(normalized_labels$sample_settings)",
        "item_label_settings_pca(normalized_labels$item_settings)",
        "analysis_results(state$analysis_results)",
    ):
        assert state_write not in plot_phase
        assert state_write in state_phase


def test_registration_detects_phase_support_and_preserves_pca_contract() -> None:
    source = REGISTRATION.read_text()
    invoker = source.split(
        "invoke_module_set_session_state <- function(module, module_state, phase)", 1
    )[1].split("log_module_restore_timing <- function", 1)[0]
    assert '"phase" %in% names(set_formals) || "..." %in% names(set_formals)' in invoker
    assert "set_session_state(module_state, phase = phase)" in invoker

    pca_registration = source.split("# PCA Module (priority 50, level: full_session)", 1)[1]
    pca_registration = pca_registration.split("# Heatmap Module", 1)[0]
    assert 'priority   = full_session_participant_priorities[["pca"]]' in pca_registration
    assert "save_level = SESSION_SAVE_LEVEL_FULL" in pca_registration
    assert "invoke_module_set_session_state(pca, state$module_state, phase)" in pca_registration


def test_plot_phase_reports_request_without_claiming_render_completion() -> None:
    module_source = PCA_MODULE.read_text()
    setter = module_source.split("set_session_state = function(state, phase = NULL)", 1)[1]
    plot_phase = setter.split('if (identical(phase, "full_module_plots")) {', 1)[1]
    plot_phase = plot_phase.split(
        'if (!is.null(phase) && !identical(phase, "full_module_state")) return()', 1
    )[0]
    assert 'report$render_status <- if (isTRUE(report$rebuild_requested)) "rebuild_requested"' in plot_phase
    assert "report$plot_recreated <- FALSE" in plot_phase

    registration = REGISTRATION.read_text()
    pca_registration = registration.split("# PCA Module (priority 50, level: full_session)", 1)[1]
    pca_registration = pca_registration.split("# Heatmap Module", 1)[0]
    assert 'rebuild_requested <- isTRUE(invoked) && identical(phase, "full_module_plots")' in pca_registration
    assert 'plot_recreated <- invoke_module_set_session_state' not in pca_registration


def test_static_renderer_owns_pca_restore_completion_marker() -> None:
    source = PCA_PIPELINE.read_text()
    plot_created = source.split("p <- create_static_plot(", 1)[1]
    marker = plot_created.index('update_restore_render_report(render_generation, "render_completed")')
    null_guard = plot_created.index("if (is.null(p))")
    assert null_guard < marker
    assert 'update_restore_render_report(render_generation, "render_failed", e$message)' in source


def test_plot_timeout_is_diagnostic_only_and_generation_guarded() -> None:
    source = PCA_MODULE.read_text()
    callback = source.split("later::later(function() {", 1)[1].split(
        "}, delay = timeout_seconds)", 1
    )[0]

    assert "tryCatch({" in callback
    assert "current <- isolate({" in callback
    assert "timeout_session_generation" in callback
    assert "timeout_pca_generation" in callback
    assert "current_report$render_completed" in callback
    assert "current_report$render_failed" in callback
    assert 'record_restore_report("PCA", current_report)' in callback
    assert "restore timeout diagnostic failed" in callback
    for forbidden_write in (
        "restore_in_progress(",
        "plots_ready(",
        "static_plot_obj(",
        "pending_ui_inputs(",
        "restore_plot_data_cache(",
    ):
        assert forbidden_write not in callback
