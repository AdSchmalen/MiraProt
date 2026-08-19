from pathlib import Path


IMPUTATION = Path("modules/Data Wizard/imputation/datawizard_imputation_state_configuration.R").read_text(encoding="utf-8")
BASEMEAN = Path(
    "modules/Data Wizard/basemean/datawizard_basemean_observer.R"
).read_text(encoding="utf-8")


def test_imputation_choices_refresh_when_lazy_panel_is_initialized():
    assert "observeEvent(initialized(), {" in IMPUTATION
    assert "if (isTRUE(initialized())) refresh_imputation_choices()" in IMPUTATION


def test_basemean_preserves_metadata_sample_order():
    sample_block = BASEMEAN.split("cached_samples <- sample_choice_cache()", 1)[1]
    sample_block = sample_block.split("current_samples <-", 1)[0]

    assert "valid_samples <- unique(na.omit(" in sample_block
    assert "sort(unique(na.omit(" not in sample_block
