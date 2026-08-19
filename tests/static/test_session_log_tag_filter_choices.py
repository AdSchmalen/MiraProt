"""Focused checks for the Session log tag-filter choice refresh."""
from pathlib import Path


TEXT = Path("R/session_save_restore/session_save_restore_orchestration.R").read_text()
OBSERVER = TEXT.split("    known_tags <- c(\"MAIN APP\", \"FILE LOADER\")", 1)[1].split(
    "  authoritative_session_log_buffer <- function", 1
)[0]


def test_tag_choices_are_cleaned_deduplicated_and_radix_sorted():
    assert 'buf_tags <- unique(trimws(as.character(buf$tag)))' in OBSERVER
    assert "buf_tags <- buf_tags[!is.na(buf_tags) & nzchar(buf_tags)]" in OBSERVER
    assert 'tags <- sort(unique(c(known_tags, tags)), method = "radix")' in OBSERVER

    # Exercise the intended transformation with deliberately unsorted, duplicated,
    # blank, and missing values so the expected UI contract remains explicit.
    known_tags = ["MAIN APP", "FILE LOADER"]
    discovered_tags = ["Zulu", " Alpha ", "Zulu", "", None, "Beta", "FILE LOADER"]
    cleaned = [tag.strip() for tag in discovered_tags if tag is not None and tag.strip()]
    tags = sorted(dict.fromkeys(known_tags + cleaned))

    choices = [("All debug calls", "")] + [(tag, tag) for tag in tags]
    assert choices == [
        ("All debug calls", ""),
        ("Alpha", "Alpha"),
        ("Beta", "Beta"),
        ("FILE LOADER", "FILE LOADER"),
        ("MAIN APP", "MAIN APP"),
        ("Zulu", "Zulu"),
    ]


def test_choice_refresh_preserves_a_valid_selected_tag():
    assert 'choices <- c("All debug calls" = "", stats::setNames(tags, tags))' in OBSERVER
    assert "selected <- isolate(input$session_log_tag_filter)" in OBSERVER
    assert "!(selected %in% unname(choices))" in OBSERVER
    assert 'selected <- ""' in OBSERVER
    assert "selected = selected" in OBSERVER

    choice_values = ["", "Alpha", "Beta", "FILE LOADER", "MAIN APP", "Zulu"]
    selected = "Beta"
    refreshed_selected = selected if selected in choice_values else ""
    assert refreshed_selected == "Beta"
