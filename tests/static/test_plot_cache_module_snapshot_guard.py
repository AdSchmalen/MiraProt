"""Static regression checks for save-time plot-cache module snapshot integrity."""
from pathlib import Path

TEXT = Path("R/session_save_restore/session_save_restore_orchestration.R").read_text()
PLOT_CACHE_BLOCK = TEXT.split(".build_save_time_plot_data_cache_bundle <- function", 1)[1].split(
    ".build_session_plot_data_cache_bundle <- .build_save_time_plot_data_cache_bundle", 1
)[0]


def test_plot_cache_uses_pre_cache_snapshot_order_and_datawizard_first_guard():
    assert "pre_cache_module_snapshots <-" in PLOT_CACHE_BLOCK
    assert "input_module_ids <- names(pre_cache_module_snapshots)" in PLOT_CACHE_BLOCK
    assert "for (mid in names(pre_cache_module_snapshots))" in PLOT_CACHE_BLOCK

    first_loop = PLOT_CACHE_BLOCK.split('.log_plot_cache_phase("first_pass:start")', 1)[1].split(
        '.log_plot_cache_phase("repair:start")', 1
    )[0]
    assert "for (mid in names(pre_cache_module_snapshots))" in first_loop

    save_orchestration = TEXT.split("module_snapshots <- module_snapshot_bundle$module_snapshots", 1)[1].split(
        "plot_cache_bundle <- .build_save_time_plot_data_cache_bundle", 1
    )[0]
    assert "pre_cache_module_snapshots <- module_snapshots" in save_orchestration
    assert "datawizard" in TEXT, "datawizard must remain represented in snapshot ordering checks"


def test_plot_cache_only_mutates_per_module_state_and_restores_missing_modules():
    assert "result_module_snapshots[[mid]]$module_state <-" in PLOT_CACHE_BLOCK
    assert "result_module_snapshots <<-" not in PLOT_CACHE_BLOCK
    assert "module=<id> optional cache processing skipped" not in PLOT_CACHE_BLOCK
    assert '" optional cache processing skipped: "' in PLOT_CACHE_BLOCK
    assert "setdiff(input_module_ids, names(result_module_snapshots)" in PLOT_CACHE_BLOCK
    assert "result_module_snapshots[[missing_mid]] <<- pre_cache_module_snapshots[[missing_mid]]" in PLOT_CACHE_BLOCK
    assert "disabled_module_snapshot_loss_guard" not in PLOT_CACHE_BLOCK


def test_lazy_module_mutation_expressions_do_not_return_from_bundle_builder():
    mutation_calls = PLOT_CACHE_BLOCK.split(".with_plot_cache_module_mutation(")[2:]
    assert len(mutation_calls) == 3, "first-pass, repair, and heatmap mutations must remain audited"
    for mutation_call in mutation_calls:
        expression = mutation_call.split("})", 1)[0]
        assert "return(" not in expression

    first_pass_expression = mutation_calls[0].split("})", 1)[0]
    assert "if (is.list(mstate) && isTRUE(.uses_shared_plot_data_cache(mstate))) {" in first_pass_expression


def test_payload_hydration_replaces_incompatible_refs_and_defers_payload_removal():
    hydration = PLOT_CACHE_BLOCK.split(
        ".hydrate_missing_cache_ref_from_payload <- function", 1
    )[1].split(".restore_missing_pre_cache_module_snapshots <- function", 1)[0]

    assert "plot_data_cache_ref = NULL" in hydration
    assert "candidate_state[names(contract)] <- contract" in hydration
    for dimension in ("data_mod_nrow", "data_mod_ncol", "data_def_nrow", "data_def_ncol"):
        assert f"candidate_state${dimension}" in hydration
    assert ".cache_ref_contract_compatible(" in hydration
    assert ".validate_plot_cache_ref_by_title(by_title, candidate_pool)" in hydration
    assert "function(unused) cache_id" in hydration

    validation_at = hydration.index(".cache_ref_contract_compatible(")
    payload_retention_at = hydration.index("candidate_state$plot_data_cache_payload <- pair")
    pool_commit_at = hydration.index("plot_data_cache_pool <<- candidate_pool")
    state_commit_at = hydration.index("result_module_snapshots[[mid]] <<- candidate_snapshot")
    assert validation_at < payload_retention_at < pool_commit_at < state_commit_at
    assert "candidate_state$plot_data_cache_payload <- NULL" not in hydration


def test_shared_cache_normalization_uses_one_selected_revision_pair():
    assert "selected_revisions <- list(" in PLOT_CACHE_BLOCK
    assert ".selected_plot_cache_contract <- function" in PLOT_CACHE_BLOCK
    assert ".selected_plot_cache_id <- function" in PLOT_CACHE_BLOCK
    assert "data_mod_revision_id = mstate$data_mod_revision_id" not in PLOT_CACHE_BLOCK
    assert "data_def_revision_id = mstate$data_def_revision_id" not in PLOT_CACHE_BLOCK
    assert "data_mod_revision_id = selected_revisions$data_mod_revision_id" in PLOT_CACHE_BLOCK
    assert "data_def_revision_id = selected_revisions$data_def_revision_id" in PLOT_CACHE_BLOCK


if __name__ == "__main__":
    test_plot_cache_uses_pre_cache_snapshot_order_and_datawizard_first_guard()
    test_plot_cache_only_mutates_per_module_state_and_restores_missing_modules()
    test_lazy_module_mutation_expressions_do_not_return_from_bundle_builder()
    test_payload_hydration_replaces_incompatible_refs_and_defers_payload_removal()
    test_shared_cache_normalization_uses_one_selected_revision_pair()
    print("Plot-cache module snapshot guard static checks passed")
