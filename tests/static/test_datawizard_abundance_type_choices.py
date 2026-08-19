from pathlib import Path


SUPPORTED_TYPES = (
    "Raw Abundance",
    "Normalized Abundance",
    "Batch Corrected Abundance",
    "Imputed Raw Abundance",
    "Imputed Normalized Abundance",
    "Imputed Batch Corrected Abundance",
    "Batch Corrected Normalized Abundance",
    "Batch Corrected Raw Abundance",
    "Imputed Batch Corrected Normalized Abundance",
    "Imputed Batch Corrected Raw Abundance",
)


CHOICE_SOURCES = (
    Path("modules/Data Wizard/imputation/datawizard_imputation_state_configuration.R"),
    Path("modules/Data Wizard/basemean/datawizard_basemean_observer.R"),
    Path("modules/Data Wizard/ratios/datawizard_ratios_UI.R"),
    # Tables consumes the canonical choices helper rather than duplicating it.
    Path("modules/Data Wizard/datawizard_utils.R"),
    Path("modules/Data Wizard/auto assign/datawizard_auto_assign_UI.R"),
)


def test_datawizard_abundance_choices_cover_supported_metadata_content_types():
    for source_path in CHOICE_SOURCES:
        source = source_path.read_text(encoding="utf-8")
        missing = [content_type for content_type in SUPPORTED_TYPES if content_type not in source]
        assert not missing, f"{source_path} is missing choices: {missing}"


def test_dynamic_abundance_dropdowns_are_silent_until_a_revision_changes():
    modules = {
        Path("modules/Data Wizard/imputation/datawizard_imputation_state_configuration.R"): "refresh_imputation_choices",
        Path("modules/Data Wizard/basemean/datawizard_basemean_observer.R"): "refresh_basemean_choices",
        Path("modules/Data Wizard/ratios/datawizard_ratios_UI.R"): "refresh_ratio_choices",
    }
    for source_path, refresh_function in modules.items():
        source = source_path.read_text(encoding="utf-8")
        revision_observer = source.index(
            "observeEvent(list(", source.index(f"{refresh_function} <- function()")
        )
        observer_end = source.index("ignoreInit = TRUE", revision_observer)
        assert refresh_function in source[revision_observer:observer_end], source_path
        next_section = source.find("# ========================================", observer_end)
        if next_section == -1:
            next_section = len(source)
        assert "session$onFlushed" not in source[observer_end:next_section], source_path


DOWNSTREAM_CHOICE_SOURCES = {
    "PCA": (
        Path("modules/PCA/pca_module_UI.R"),
        Path("modules/PCA/pca_module_server_observers.R"),
    ),
    "Heatmap": (
        Path("modules/Heatmap/Heatmap_ui.R"),
        Path("modules/Heatmap/Heatmap_observers.R"),
        Path("modules/Heatmap_module.R"),
    ),
    "GSEA": (
        Path("modules/GSEA/GSEA_module_observer.R"),
        Path("modules/GSEA/GSEA_module_logic.R"),
    ),
    "Abundances": (
        Path("modules/abundances/abundances_ui.R"),
        Path("modules/abundances/abundances_observer.R"),
    ),
    "Venn": (Path("modules/venn/venn_observers_data_lists.R"),),
}

DOWNSTREAM_SUPPORTED_TYPES = tuple(
    content_type
    for content_type in SUPPORTED_TYPES
    if content_type not in {"Batch Corrected Abundance", "Imputed Batch Corrected Abundance"}
)


def test_downstream_abundance_allowlists_cover_supported_metadata_types():
    for module, source_paths in DOWNSTREAM_CHOICE_SOURCES.items():
        for source_path in source_paths:
            source = source_path.read_text(encoding="utf-8")
            missing = [
                content_type
                for content_type in DOWNSTREAM_SUPPORTED_TYPES
                if content_type not in source
            ]
            assert not missing, f"{module}: {source_path} is missing choices: {missing}"


def test_gsea_validation_remains_anchored_and_covers_supported_metadata_types():
    source = Path("modules/GSEA/GSEA_module_logic.R").read_text(encoding="utf-8")
    validation = source[source.index("validate_abundance_type <- function"):]
    validation = validation[:validation.index("\n}")]
    assert '"^(' in validation
    assert ')$"' in validation
    for content_type in DOWNSTREAM_SUPPORTED_TYPES:
        assert content_type in validation
