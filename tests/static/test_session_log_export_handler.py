"""Static checks for the central session-log copy/download path."""
from pathlib import Path

TEXT = Path("R/session_save_restore/session_save_restore_orchestration.R").read_text()


def test_session_log_copy_download_use_authoritative_export_helper():
    assert "authoritative_session_log_buffer <- function" in TEXT, (
        "Session log serialization should be centralized around the authoritative buffer reader"
    )
    assert "session_log_export_text <- function" in TEXT, (
        "Copy/download handlers should share one central export text helper"
    )

    download = TEXT.split("output$session_log_download <- downloadHandler", 1)[1].split(
        "copy_session_log_to_clipboard <- function", 1
    )[0]
    copy = TEXT.split("observeEvent(input$session_log_copy", 1)[1].split(
        "observeEvent(input$session_deep_cleanup_shutdown", 1
    )[0]

    for name, block in (("download", download), ("copy", copy)):
        assert "session_log_export_text()" in block, f"{name} path must use the central export helper"
        assert "ui_filtered_log_text()" not in block, f"{name} path must not copy rendered UI text"
        assert "session_log_display" not in block, f"{name} path must not read the display output"
        assert "document.getElementById" not in block, f"{name} path must not read log text from the DOM"

    export_helper = TEXT.split("session_log_export_text <- function", 1)[1].split(
        "output$session_log_display <- renderText", 1
    )[0]
    assert export_helper.count("authoritative_session_log_buffer(") == 1, (
        "The export helper should read the authoritative log buffer exactly once"
    )
    assert "authoritative_session_log_text(" not in export_helper, (
        "The export helper should serialize the authoritative buffer directly"
    )
    assert "ui_filtered_log_text()" not in export_helper, (
        "The export helper must bypass rendered Session-tab text"
    )


def test_session_log_dedupe_uses_stable_source_tuple():
    assert "session_log_record_key <- function(buf, source_block)" in TEXT, (
        "Session log dedupe should build a stable key that includes the source block"
    )
    record_key = TEXT.split("session_log_record_key <- function", 1)[1].split(
        "dedupe_session_log_buffer <- function", 1
    )[0]
    for expected in (
        "source_col",
        "module_col",
        "time_col",
        "message_col",
    ):
        assert expected in record_key, f"Missing {expected} from the session log dedupe tuple"

    dedupe_call_block = TEXT.split("authoritative_session_log_buffer <- function", 1)[1].split(
        "# Build one authoritative vector", 1
    )[0]
    assert 'source_block = "restored"' in dedupe_call_block
    assert 'source_block = "live"' in dedupe_call_block
    assert "live_lines <- live_lines[!(live_lines %in% restored_lines)]" not in TEXT, (
        "Line-only cross-buffer dedupe can collapse legitimate records; use tuple dedupe instead"
    )


if __name__ == "__main__":
    test_session_log_copy_download_use_authoritative_export_helper()
    test_session_log_dedupe_uses_stable_source_tuple()
    print("Session log export handler checks passed")
