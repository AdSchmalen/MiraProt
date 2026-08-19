"""Focused static checks for the app-critical Heatmap download handler source."""
from pathlib import Path

PATH = Path("modules/Heatmap/Heatmap_download.R")
TEXT = PATH.read_text(encoding="utf-8")


def test_heatmap_download_handler_source_file_is_present_and_registered():
    assert PATH.exists(), "Heatmap download handler source file is missing"
    assert "# Heatmap Module - Download Handlers and Grid Export" in TEXT, (
        "Heatmap download handler source should retain its dedicated module header"
    )
    assert "build_heatmap_htlist_for_tab <- function" in TEXT, (
        "Heatmap download source should provide the runtime HeatmapList builder used by downloads"
    )
    assert "output$downloadPlotButton_Heatmaps <- downloadHandler" in TEXT, (
        "App-critical Heatmap plot download handler must remain registered"
    )


def test_heatmap_download_handler_contains_filename_and_content_callbacks():
    handler = TEXT.split("output$downloadPlotButton_Heatmaps <- downloadHandler", 1)[1]
    assert "filename = function()" in handler, "Heatmap download handler needs a filename callback"
    assert "content = function(file)" in handler, "Heatmap download handler needs a content callback"
    for expected in ("png", "pdf", "svg"):
        assert expected in handler, f"Heatmap download handler should support {expected} export"


if __name__ == "__main__":
    test_heatmap_download_handler_source_file_is_present_and_registered()
    test_heatmap_download_handler_contains_filename_and_content_callbacks()
    print("Heatmap download handler source checks passed")
