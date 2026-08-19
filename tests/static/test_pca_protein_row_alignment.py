"""Contracts that keep protein PCA coordinates and identifiers one-to-one."""

from pathlib import Path


UTILS = Path("modules/PCA/pca_module_utils.R").read_text()
PIPELINE = Path("modules/PCA/pca_module_server_pipeline.R").read_text()
STATIC = Path("modules/PCA/pca_module_static.R").read_text()


def test_preparation_uses_identifiers_from_the_post_removal_matrix() -> None:
    removal = UTILS.index("df_matrix <- impute_missing_values")
    final_ids = UTILS.index("final_identifiers <- rownames(df_matrix)")
    returned = UTILS.index("identifiers = identifiers_filtered", final_ids)
    assert removal < final_ids < returned


def test_protein_plot_data_has_no_identifier_name_fallback() -> None:
    create_plot_data = UTILS.split("create_plot_data <- function", 1)[1]
    assert "plot_data$Identifier <- plot_data$Name" not in create_plot_data
    assert "Protein plot identifier alignment failed" in create_plot_data


def test_main_pipeline_checks_all_four_counts_before_storage() -> None:
    finalization = PIPELINE.split("# Add metadata to results", 1)[1]
    invariant = finalization.index("invariant_counts <- c(")
    storage = finalization.index("analysis_results(results)")
    assert invariant < storage
    for count in ("analysis_matrix", "coordinates", "point_names", "plot_data"):
        assert count in finalization[invariant:storage]
    assert "coordinate_identifiers" in finalization[invariant:storage]


def test_static_plot_revalidates_protein_mapping() -> None:
    plot_build = STATIC.split("create_static_plot <- function", 1)[1]
    assert 'identical(results$comparison_target, "proteins")' in plot_build
    assert "Protein plot alignment failed" in plot_build
