"""Static contract for PCA restore-trigger generation idempotence."""
from pathlib import Path


SOURCE = Path("modules/pca_module.R").read_text()


def test_duplicate_generation_returns_before_guard_or_widget_replay() -> None:
    trigger = SOURCE.split("observeEvent(rv$session_restore_trigger, {", 1)[1]
    trigger = trigger.split("}, ignoreInit = TRUE)", 1)[0]

    duplicate_guard = trigger.index(
        "identical(generation_key, pca_finalized_restore_generation)"
    )
    guard_raise = trigger.index("pca_state$restore_in_progress(TRUE)")
    widget_replay = trigger.index(
        'updateSelectInput(session, "GeneIdentifierColumn_pca"'
    )
    assert duplicate_guard < guard_raise < widget_replay


def test_generation_is_finalized_with_the_single_render_invalidation() -> None:
    trigger = SOURCE.split("observeEvent(rv$session_restore_trigger, {", 1)[1]
    trigger = trigger.split("}, ignoreInit = TRUE)", 1)[0]

    nonce = "pca_state$render_nonce(isolate(pca_state$render_nonce()) + 1L)"
    finalized = "pca_finalized_restore_generation <<- generation_key"
    assert trigger.count(nonce) == 1
    assert trigger.count(finalized) == 1
    assert trigger.index(nonce) < trigger.index(finalized)
