"""Static contracts for concise PCA restore lifecycle diagnostics."""
from pathlib import Path


MODULE_SOURCE = Path("modules/pca_module.R").read_text()
PIPELINE_SOURCE = Path("modules/PCA/pca_module_server_pipeline.R").read_text()


def test_restore_boundaries_have_compact_diagnostics() -> None:
    expected_messages = (
        "Data Wizard trigger received (session_generation=%s, pca_generation=%s)",
        "restored identifier choices available (count=%d)",
        "%s identifier selected (%s)",
        "ordinary PCA UI synchronization completed",
        "ordinary PCA UI synchronization failed:",
        "labeling staged (selected_protein_count=%d, per_item_settings_count=%d)",
        "restore guard released (status=%s)",
        "render requested (pca_generation=%s)",
    )
    for message in expected_messages:
        assert message in MODULE_SOURCE

    assert "static plot render completed (pca_generation=%s)" in PIPELINE_SOURCE
    assert "static plot render failed (pca_generation=%s): %s" in PIPELINE_SOURCE


def test_missing_identifier_choices_warn_without_stopping_restore() -> None:
    missing_choices = MODULE_SOURCE.split(
        "if (length(identifier_choices) == 0L) {", 1
    )[1].split("saved_identifier <-", 1)[0]

    assert "report$warnings" in missing_choices
    assert "saved compact PCA result" in missing_choices
    assert "stop(" not in missing_choices
