from pathlib import Path


UTILS = Path("modules/Data Wizard/datawizard_utils.R").read_text(encoding="utf-8")
CORE = Path(
    "modules/Data Wizard/core/datawizard_core_metadata_updates.R"
).read_text(encoding="utf-8")
EXPORT = Path("modules/Data Wizard/datawizard_export.R").read_text(encoding="utf-8")
PRIMARY_EXPORT = Path(
    "R/export/export_pipeline_datawizard_primary.R"
).read_text(encoding="utf-8")
TABLES = Path(
    "modules/Data Wizard/tables/datawizard_tables_observer_metadata_editing.R"
).read_text(encoding="utf-8")


def test_public_metadata_drops_private_lineage_columns():
    helper_start = UTILS.index("datawizard_drop_deprecated_metadata_columns <- function")
    helper_end = UTILS.index("\n}", helper_start)
    helper = UTILS[helper_start:helper_end]

    assert 'private_fields <- c("Custom", "ContrastId", "VariantId")' in helper
    assert "!names(metadata) %in% private_fields" in helper


def test_ratio_metadata_rows_keep_triplet_rows_without_private_fields():
    row_builder_start = CORE.index("create_ratio_metadata_rows <- function")
    row_builder_end = CORE.index("update_metadata_for_ratio_columns", row_builder_start)
    row_builder = CORE[row_builder_start:row_builder_end]

    assert 'new_metadata_rows$Content[i] <- "Abundance Ratio"' in row_builder
    assert 'new_metadata_rows$Content[i] <- "Abundance Ratio p-Value"' in row_builder
    assert 'new_metadata_rows$Content[i] <- "Abundance Ratio Adj. p-Value"' in row_builder
    assert "ContrastId =" not in row_builder
    assert "VariantId =" not in row_builder


def test_tables_and_excel_export_filter_private_metadata_fields():
    assert "df_display <- datawizard_drop_deprecated_metadata_columns(df)" in TABLES
    assert (
        "metadata_def <- datawizard_drop_deprecated_metadata_columns(metadata_resolved$data)"
        in EXPORT
    )
    assert 'private_fields <- c("Custom", "ContrastId", "VariantId")' in PRIMARY_EXPORT
    assert "!names(metadata) %in% private_fields" in PRIMARY_EXPORT
