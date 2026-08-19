from pathlib import Path


def test_grid_restore_retry_runs_in_reactive_context():
    source = Path("modules/Grid_module.R").read_text(encoding="utf-8")

    start = source.index("restore_pending <- function() isolate({")
    retry = source.index("later::later(restore_pending", start)
    finish = source.index("session$onFlushed(restore_pending", retry)

    assert start < retry < finish
    assert "resolved_ids <- names(rv$gridplot_selection)" in source[start:finish]
