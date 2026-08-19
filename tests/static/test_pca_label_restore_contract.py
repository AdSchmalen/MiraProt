"""Static contracts for PCA/UMAP label-state persistence and rendering."""

from pathlib import Path


PCA_MODULE = Path("modules/pca_module.R").read_text()
PIPELINE = Path("modules/PCA/pca_module_server_pipeline.R").read_text()
STATE = Path("modules/PCA/pca_module_state.R").read_text()


def _getter() -> str:
    return PCA_MODULE.split("get_session_state = function()", 1)[1].split(
        "set_session_state = function(state, phase = NULL)", 1
    )[0]


def _setter() -> str:
    return PCA_MODULE.split("set_session_state = function(state, phase = NULL)", 1)[1]


def test_pca_snapshot_is_data_only_and_has_one_mode_discriminated_label_state() -> None:
    getter = _getter()
    assert "state$plot_request <- list(" in getter
    assert "label_state = label_state" in getter
    assert 'mode = "samples"' in getter
    assert 'mode = "proteins"' in getter
    assert "selected_items = selected_ids" in getter
    assert "searchGene_pca = tryCatch" in getter
    assert "general_controls = current_inputs" in getter
    assert "geometry_controls = current_inputs" in getter
    assert "item_controls = settings" in getter

    # Plot objects may exist in live state, but must never be assigned into the
    # serialized module envelope.
    for forbidden in (
        "state$static_plot_obj",
        "state$interactive_plot_obj",
        "state$ggplot_object_PCATab",
        "state$plotly",
    ):
        assert forbidden not in getter
    assert "intentionally excluded and recreated after restore" in STATE


def test_setter_normalizes_legacy_and_current_labels_before_replacing_state() -> None:
    setter = _setter()
    normalization = setter.index("normalized_labels <- normalize_pca_label_restore_state(")
    guard = setter.index("pca_state$restore_in_progress(TRUE)")
    writes = setter.index('if (identical(normalized_labels$mode, "samples"))')
    assert normalization < guard < writes
    assert "state$plot_request$labels" in setter
    assert "compatibility = state" in setter
    assert "selected_items_vector_pca(character())" in setter
    assert "sample_label_settings_pca(list())" in setter
    assert "item_label_settings_pca(normalized_labels$item_settings)" in setter


def test_static_render_waits_for_guard_and_depends_on_both_normalized_setting_sets() -> None:
    static_observer = PIPELINE.split("output$static_plot <- renderPlot({", 1)[1].split(
        "# Interactive Plot Outputs", 1
    )[0]
    guard = static_observer.index("state$restore_in_progress()")
    sample_dependency = static_observer.index("sample_label_settings_pca()")
    protein_dependency = static_observer.index("item_label_settings_pca()")
    build = static_observer.index("create_static_plot(")
    assert guard < sample_dependency < build
    assert guard < protein_dependency < build
    assert "render_nonce()" in static_observer
    assert "session restore finalized" in PCA_MODULE
