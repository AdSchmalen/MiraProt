import re
from pathlib import Path


METADATA_SOURCES = (
    Path("R/utils.R"),
    Path("modules/Data Wizard/datawizard_core.R"),
    Path("modules/Data Wizard/datawizard_file_loader.R"),
    Path("modules/Data Wizard/datawizard_imputation.R"),
    Path("modules/Data Wizard/tables/datawizard_tables_observer.R"),
    Path("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R"),
)


def test_deprecated_custom_field_is_never_constructed_or_assigned():
    assignment = re.compile(r"(?:\$Custom\s*<-|\bCustom\s*=)")
    for source_path in METADATA_SOURCES:
        source = source_path.read_text(encoding="utf-8")
        assert not assignment.search(source), source_path


def test_excel_metadata_export_drops_nonpublic_fields():
    source = Path("R/export/export_pipeline_datawizard_primary.R").read_text(
        encoding="utf-8"
    )
    export_block = source[source.index("# Sheet 3: Metadata"):]
    export_block = export_block[:export_block.index("# Sheet 4:")]
    assert 'private_fields <- c("Custom", "ContrastId", "VariantId")' in export_block
    assert "!names(metadata) %in% private_fields" in export_block
