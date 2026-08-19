"""Static safety contract for session-upload ownership and cleanup."""

from pathlib import Path


SOURCE = Path(
    "R/session_save_restore/session_save_restore_orchestration.R"
).read_text(encoding="utf-8")


def test_restore_captures_and_normalizes_datapath_once() -> None:
    assert "restore_path <- .normalized_existing_path(file_info$datapath)" in SOURCE
    assert "readRDS(file_info$datapath)" not in SOURCE


def test_cleanup_is_registered_immediately_before_materializing_snapshot() -> None:
    reader = SOURCE[SOURCE.index(".read_restore_snapshot <- function"):]
    reader = reader[: reader.index("set_restored_log_content <- function")]
    assert reader.index("on.exit({") < reader.index("readRDS(path)")
    assert "base::file(" not in reader
    assert "close(" not in reader
    assert "readRDS(con)" not in reader


def test_only_claimed_upload_storage_is_deleted() -> None:
    assert "in_upload_storage <- regular_file" in SOURCE
    assert "!isTRUE(in_protected_storage) && !already_claimed" in SOURCE
    assert "if (isTRUE(owned_temporary_path))" in SOURCE
    assert 'getOption("miraprot.upload.dirs"' in SOURCE
    assert "active_restore_upload_paths" in SOURCE


def test_path_claim_is_released_independently_of_file_ownership() -> None:
    reader = SOURCE[SOURCE.index(".read_restore_snapshot <- function"):]
    reader = reader[: reader.index("set_restored_log_content <- function")]
    owned_block_end = reader.index("claim_error <- tryCatch")
    assert reader.index("if (isTRUE(owned_temporary_path))") < owned_block_end
    assert reader.index("rm(list = path", owned_block_end) > owned_block_end


def test_large_restore_bindings_are_released_without_forced_gc() -> None:
    assert "snapshot$compatibility_upgrade <- NULL" in SOURCE
    assert "plot_data_cache_pool <- NULL" in SOURCE
    restore_section = SOURCE[SOURCE.index("restore_session_from_file <- function"):]
    assert "gc(" not in restore_section
