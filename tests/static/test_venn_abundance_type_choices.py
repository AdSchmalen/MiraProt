from pathlib import Path


SOURCE = Path("modules/venn/venn_observers_data_lists.R").read_text(encoding="utf-8")


def test_venn_reference_selector_uses_only_supported_abundance_types():
    choice_block_start = SOURCE.index("abundance_types <- c(")
    choice_block_end = SOURCE.index("abundance_like <-", choice_block_start)
    choice_block = SOURCE[choice_block_start:choice_block_end]

    supported = (
        "Raw Abundance",
        "Normalized Abundance",
        "Imputed Raw Abundance",
        "Imputed Normalized Abundance",
        "Imputed Batch Corrected Normalized Abundance",
        "Imputed Batch Corrected Raw Abundance",
        "Batch Corrected Raw Abundance",
        "Batch Corrected Normalized Abundance",
    )
    excluded = (
        "Abundance Ratio",
        "Abundance Ratio p-Value",
        "Abundance Ratio Adj. p-Value",
        "# PSMs",
    )

    assert all(f'"{content_type}"' in choice_block for content_type in supported)
    assert all(f'"{content_type}"' not in choice_block for content_type in excluded)
    assert 'grepl("Abundance|Ratio|PSMs|p-Value|P-Value|Intensity|Quant"' not in SOURCE
