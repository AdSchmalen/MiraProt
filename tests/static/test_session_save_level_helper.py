"""Static checks for session save-level resolution helper behavior."""
from pathlib import Path

TEXT = Path("R/session_save_restore/session_save_restore_orchestration.R").read_text()


def helper_block():
    return TEXT.split(".resolve_session_save_level <- function", 1)[1].split(
        ".collect_prune_sanitize_shared_rv_snapshot <- function", 1
    )[0]


def download_handler_block():
    return TEXT.split("output$session_download <- downloadHandler", 1)[1].split(
        "shared_snapshot <- .collect_prune_sanitize_shared_rv_snapshot", 1
    )[0]


def test_session_save_level_helper_reads_isolated_input_once():
    helper = helper_block()
    assert "(input)" in helper.split("{", 1)[0]
    assert "save_level <- isolate(input$session_save_level)" in helper
    assert helper.count("isolate(input$session_save_level)") == 1


def test_session_save_level_helper_allows_only_known_levels_and_defaults_full():
    helper = helper_block()
    for expected in (
        "SESSION_SAVE_LEVEL_DATA",
        "SESSION_SAVE_LEVEL_ANALYSIS",
        "SESSION_SAVE_LEVEL_FULL",
    ):
        assert expected in helper
    assert "!is.character(save_level)" in helper
    assert "length(save_level) != 1L" in helper
    assert "save_level <- SESSION_SAVE_LEVEL_FULL" in helper


def test_session_download_uses_save_level_helper_without_inline_isolate():
    handler = download_handler_block()
    assert "save_level <- .resolve_session_save_level(input)" in handler
    assert "isolate(input$session_save_level)" not in handler


if __name__ == "__main__":
    test_session_save_level_helper_reads_isolated_input_once()
    test_session_save_level_helper_allows_only_known_levels_and_defaults_full()
    test_session_download_uses_save_level_helper_without_inline_isolate()
    print("Session save-level helper checks passed")
