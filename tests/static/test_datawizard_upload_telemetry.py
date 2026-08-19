from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LOADER = (ROOT / "modules/Data Wizard/datawizard_file_loader.R").read_text()
CORE = (ROOT / "modules/Data Wizard/datawizard_core.R").read_text()
TABLES = (ROOT / "modules/Data Wizard/tables/datawizard_tables_observer.R").read_text()


def test_upload_lifecycle_has_correlation_and_monotonic_clock():
    assert "datawizard_upload_correlation_id" in LOADER
    assert 'proc.time()[["elapsed"]]' in LOADER
    assert "correlation_id=%s | elapsed_ms=" in LOADER


def test_required_loader_and_core_markers_are_present():
    text = LOADER + CORE
    for marker in (
        "validation", "sheet_enumeration", "xlsx_parsing", "normalization",
        "reset", "state_transaction", "cache_insertion",
        "placeholder_metadata_creation", "committed_revision_release",
    ):
        assert f'"{marker}"' in text


def test_server_dispatch_and_browser_readiness_are_distinct():
    assert "serialization dispatch queued" in TABLES
    assert "new CustomEvent('datawizard:dt-ready'" in TABLES
    assert "datawizard_primary_table_client_ready" in TABLES
    assert "datawizard_additional_table_client_ready" in TABLES


def test_metrics_privacy_and_duplicate_work_contract():
    assert "visible_dimensions=" in TABLES
    assert "object_mb=" in LOADER
    assert "cache_hit=" in LOADER
    assert "generation=%d" in CORE
    assert "revision_working=" in CORE
    assert "filenames_may_be_logged" in LOADER
    assert "duplicate_work" in LOADER
    assert "DW DUPLICATE WORK" in TABLES
