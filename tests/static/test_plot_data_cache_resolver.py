"""Static regression checks for plot-data cache resolver helpers."""
from pathlib import Path

TEXT = Path("R/session_save_restore/session_save_restore_core_helpers.R").read_text()


def resolver_block():
    return TEXT.split(".resolve_plot_data_cache_for_module <- function", 1)[1].split(
        "# Build a lightweight index describing the plot-data cache pool.", 1
    )[0]


def test_plot_data_cache_resolver_uses_plot_cache_pair_validator():
    resolver = resolver_block()
    assert ".is_valid_pair" not in resolver
    assert ".is_plot_cache_pair(cached)" in resolver
    assert ".is_plot_cache_pair(sole)" in resolver


if __name__ == "__main__":
    test_plot_data_cache_resolver_uses_plot_cache_pair_validator()
    print("Plot data cache resolver static checks passed")
