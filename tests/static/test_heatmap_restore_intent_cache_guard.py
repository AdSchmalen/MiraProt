"""Static contracts for Heatmap snapshot cache creation."""

from pathlib import Path


HEATMAP_MODULE = Path("modules/Heatmap_module.R").read_text()
PROTEIN_SELECTION_OBSERVERS = Path(
    "modules/Heatmap/Heatmap_observers_protein_selection.R"
).read_text()


def _getter() -> str:
    return HEATMAP_MODULE.split("get_session_state = function()", 1)[1].split(
        "set_session_state = function(state)", 1
    )[0]


def test_cache_artifacts_require_explicit_heatmap_restore_intent() -> None:
    getter = _getter()
    intent = getter.index("has_heatmap_restore_intent <- isTRUE(state$had_heatmap)")
    strategy = getter.index("heatmap_restore_strategy <- if (isTRUE(has_matrix_payload))")
    cache_guard = getter.index(
        'if (identical(heatmap_restore_strategy, "shared_cache")) {'
    )
    candidate = getter.index("cache_candidate <- tryCatch({")
    cache_ref = getter.index("state$plot_data_cache_ref <- tryCatch(")
    cache_payload = getter.index("state$plot_data_cache_payload <- cache_candidate")

    assert intent < strategy < cache_guard < candidate < cache_ref < cache_payload
    assert (
        'if (identical(heatmap_restore_strategy, "shared_cache") &&\n'
        "            !is.null(heatmap_cache_ref))"
        in getter
    )


def test_native_matrix_strategy_excludes_plot_cache_artifacts() -> None:
    getter = _getter()
    native_branch = getter.split(
        'if (identical(heatmap_restore_strategy, "native_matrices")) {', 1
    )[1].split(
        'if (identical(heatmap_restore_strategy, "shared_cache") &&', 1
    )[0]

    assert "state$matrix_payload <- list(" in native_branch
    assert '"plot_data_cache_payload", "plot_data_cache_ref"' in native_branch
    assert '"plot_cache_ref_by_title"' in native_branch


def test_empty_heatmap_snapshot_uses_no_restore_dependency_or_matrix_payload() -> None:
    getter = _getter()
    dependency = getter.split("state$restore_cache_dependency <-", 1)[1].split(
        "# This payload is a transient", 1
    )[0]
    matrix_guard = getter.split("state$matrix_payload <- list(", 1)[0].rsplit(
        "if (", 1
    )[1]

    assert 'native_matrices = "module_matrix_payload"' in dependency
    assert 'shared_cache = "shared_plot_data_cache_pool"' in dependency
    assert '"none"' in dependency
    assert 'identical(heatmap_restore_strategy, "native_matrices")' in matrix_guard


def test_restored_identifier_label_stays_live_without_mutating_search_text() -> None:
    label_start = PROTEIN_SELECTION_OBSERVERS.index(
        "output$search_identifier_label_Heatmap <- renderText({"
    )
    suspension_policy = PROTEIN_SELECTION_OBSERVERS.index(
        'outputOptions(output, "search_identifier_label_Heatmap", '
        "suspendWhenHidden = FALSE)",
        label_start,
    )
    next_output = PROTEIN_SELECTION_OBSERVERS.index(
        "output$geneSymbolList_Heatmap <- renderPrint({", suspension_policy
    )
    label_registration = PROTEIN_SELECTION_OBSERVERS[label_start:next_output]

    assert 'paste0(\n        "Search for ",\n        selected_identifier,\n        ":"' in label_registration
    assert suspension_policy < next_output
    assert "updateTextAreaInput" not in label_registration
    assert 'input$input_Heatmap <-' not in label_registration
