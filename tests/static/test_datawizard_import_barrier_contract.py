from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def text(path):
    return (ROOT / path).read_text(encoding="utf-8")


def test_canonical_data_contract_survives_import_barrier():
    core = text("modules/Data Wizard/datawizard_core.R")
    loader = text("modules/Data Wizard/datawizard_file_loader.R")
    for token in (
        "rv$data_mod", "rv$data_def", "primary_data_raw",
        'write_registry("primary_original"', 'write_registry("primary_raw"',
        'write_registry("primary_working"', "primary_working_revision",
        "primary_raw_revision", "metadata_revision",
    ):
        assert token in core
    assert "set_raw_imported_data" in loader


def test_phase_and_timing_contract_is_explicit():
    core = text("modules/Data Wizard/datawizard_core.R")
    loader = text("modules/Data Wizard/datawizard_file_loader.R")
    for phase in ("idle", "reading", "publishing_raw", "creating_metadata", "ready"):
        assert f'"{phase}"' in core
    for timing in ("reset", "parsing", "publication", "metadata creation", "downstream release"):
        assert f"IMPORT TIMING: {timing}" in loader


def test_first_guarded_modules_use_compact_barrier_signal():
    for path in (
        "modules/GO/GO_module_observer_data_choices.R",
        "modules/GSEA/GSEA_module_observer.R",
        "modules/PCA/pca_module_server_observers.R",
        "modules/Heatmap/Heatmap_observers.R",
        "modules/Volcano/volcano_observers.R",
        "modules/dot/dotplot_server_config.R",
        "modules/venn/venn_observers_data_lists.R",
    ):
        body = text(path)
        assert "datawizard_import_ready_signature(rv)" in body
        assert "datawizard_import_barrier_active(rv)" in body
