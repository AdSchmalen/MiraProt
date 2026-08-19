"""Static contracts for PCA protein-control restoration."""

from pathlib import Path


PCA_MODULE = Path("modules/pca_module.R").read_text()
PCA_OBSERVERS = Path("modules/PCA/pca_module_server_observers.R").read_text()


def test_dynamic_label_editor_keeps_rendering_while_hidden() -> None:
    render = PCA_OBSERVERS.index("output$enhanced_selectedItems_pca <- renderUI({")
    unsuspend = PCA_OBSERVERS.index(
        'outputOptions(output, "enhanced_selectedItems_pca", suspendWhenHidden = FALSE)'
    )
    assert render < unsuspend


def test_restore_idempotently_reveals_nonempty_protein_controls_before_finalizer() -> None:
    trigger = PCA_MODULE.split("observeEvent(rv$session_restore_trigger, {", 1)[1]
    ordinary_updates = trigger.index("for (updater_name in names(pca_ui_restore_ids))")
    reveal = trigger.index('session$ns("protein_controls_content")')
    schedule = trigger.index("schedule_finalizer(restore_status)")

    assert ordinary_updates < reveal < schedule
    assert 'identical(restored_target, "proteins")' in trigger
    assert "length(restored_selection) > 0L" in trigger
    assert "content.style.display!=='none'" in trigger
    assert "fa-chevron-right" in trigger
    assert "fa-chevron-down" in trigger
    assert "toggle_protein_controls" not in trigger
