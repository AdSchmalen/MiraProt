library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_orchestration.R")

.cache_pair <- function(offset = 0) {
  list(
    data_mod = data.frame(Protein = c("P1", "P2"), S1 = c(1, 2) + offset),
    data_def = data.frame(Column = c("Protein", "S1"), Content = c("Identifier", "Abundance"))
  )
}

.cache_rv <- function(pair = .cache_pair()) {
  c(pair, list(data_mod_revision_id = 4L, data_def_revision_id = 9L))
}

.shared_state <- function(pair, pending = TRUE) {
  c(
    list(
      restore_cache_dependency = "shared_plot_data_cache_pool",
      plot_reconstruction_pending = pending
    ),
    .plot_data_cache_ref_contract(4L, 9L, pair$data_mod, pair$data_def)
  )
}

.build_cache_bundle <- function(snapshots, rv, gc_mode = "hard") {
  old <- options(miraprot.session_gc_mode = gc_mode)
  on.exit(options(old), add = TRUE)
  .build_save_time_plot_data_cache_bundle(snapshots, rv, SESSION_SAVE_LEVEL_FULL)
}

.restore_saved_pair <- function(bundle, module_id, rv) {
  materialized <- .materialize_active_dataset_cache_aliases(
    bundle$plot_data_cache_pool, rv
  )
  .resolve_plot_data_cache_for_module(
    bundle$module_snapshots[[module_id]]$module_state, materialized
  )$restore_plot_data_cache
}

test_that("live plotted data has one canonical active alias for all active modules", {
  live <- .cache_pair()
  rv <- .cache_rv(live)
  state <- .shared_state(live)
  warnings <- character()

  withCallingHandlers(
    bundle <- .build_cache_bundle(setNames(lapply(c("pca", "volcano", "venn"), function(id) {
      list(module_state = state)
    }), c("pca", "volcano", "venn")), rv),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(bundle$plot_data_cache_pool, 1L)
  active_id <- names(bundle$plot_data_cache_pool)
  expect_true(.is_canonical_active_dataset_cache_alias(bundle$plot_data_cache_pool[[active_id]]))
  expect_true(all(vapply(bundle$module_snapshots, function(snapshot) {
    identical(snapshot$module_state$plot_data_cache_ref, active_id)
  }, logical(1L))))
  expect_false(any(grepl("downgrade|embedded_recovery|shared cache contract unresolved", warnings)))
  for (id in names(bundle$module_snapshots)) {
    expect_identical(.restore_saved_pair(bundle, id, rv), live, info = id)
  }
})

test_that("historical plotted data remains a normal entry and wins over live data", {
  historical <- .cache_pair()
  live <- .cache_pair(100)
  state <- .shared_state(historical)
  state$plot_data_cache_payload <- historical
  bundle <- .build_cache_bundle(list(pca = list(module_state = state)), .cache_rv(live))
  saved <- bundle$module_snapshots$pca$module_state

  expect_length(bundle$plot_data_cache_pool, 1L)
  expect_false(.is_canonical_active_dataset_cache_alias(
    bundle$plot_data_cache_pool[[saved$plot_data_cache_ref]]
  ))
  expect_identical(bundle$plot_data_cache_pool[[saved$plot_data_cache_ref]], historical)
  expect_identical(.restore_saved_pair(bundle, "pca", .cache_rv(live)), historical)
  expect_false(identical(.restore_saved_pair(bundle, "pca", .cache_rv(live)), live))
})

test_that("unused active entry is removed by active GC", {
  active <- "4|9|active|fp:active"
  pool <- setNames(list(.cache_pair()), active)
  old <- options(miraprot.session_gc_mode = "hard")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(list(), pool, .build_plot_data_cache_index(pool), active)

  expect_length(result$pool, 0L)
  expect_identical(result$report$removed_ids, active)
})

test_that("unused active entry remains unchanged with inactive GC", {
  active <- "4|9|active|fp:active"
  pool <- setNames(list(.cache_pair()), active)
  index <- .build_plot_data_cache_index(pool)
  old <- options(miraprot.session_gc_mode = "off")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(list(), pool, index, active)

  expect_identical(result$pool, pool)
  expect_identical(result$index, index)
  expect_identical(result$report$unused_ids, active)
})

test_that("referenced active entry survives as an alias and restores authoritatively", {
  live <- .cache_pair()
  rv <- .cache_rv(live)
  bundle <- .build_cache_bundle(
    list(dotplot = list(module_state = .shared_state(live))), rv
  )
  ref <- bundle$module_snapshots$dotplot$module_state$plot_data_cache_ref

  expect_identical(names(bundle$plot_data_cache_pool), ref)
  expect_true(.is_canonical_active_dataset_cache_alias(bundle$plot_data_cache_pool[[ref]]))
  expect_false(any(c("data_mod", "data_def") %in% names(bundle$plot_data_cache_pool[[ref]])))
  expect_identical(.restore_saved_pair(bundle, "dotplot", rv), live)
})

test_that("malformed aliases cannot bind to live data", {
  live <- .cache_pair()
  rv <- .cache_rv(live)
  id <- .build_plot_data_cache_id(4L, 9L, live$data_mod, live$data_def)
  alias <- .alias_active_dataset_cache_entries(setNames(list(live), id), id, rv)[[id]]

  for (field in c("data_mod_revision_id", "data_mod_nrow", "fingerprint")) {
    malformed <- alias
    malformed[[field]] <- if (is.integer(malformed[[field]])) malformed[[field]] + 1L else "wrong"
    pool <- setNames(list(malformed), id)
    expect_identical(.materialize_active_dataset_cache_aliases(pool, rv), pool, info = field)
    restored <- .resolve_plot_data_cache_for_module(.shared_state(live), pool)
    expect_null(restored$restore_plot_data_cache, info = field)
  }
})

test_that("inactive module stale reference does not keep an entry alive", {
  stale <- "4|9|stale|fp:stale"
  pool <- setNames(list(.cache_pair()), stale)
  snapshots <- list(pca = list(module_state = list(
    had_plot = FALSE, plot_data_cache_ref = stale
  )))
  old <- options(miraprot.session_gc_mode = "hard")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(
    snapshots, pool, .build_plot_data_cache_index(pool), NA_character_
  )

  expect_length(result$pool, 0L)
  expect_identical(result$report$removed_ids, stale)
})

test_that("native heatmap matrices save no cache payload and emit no invariant warning", {
  live <- .cache_pair()
  warnings <- character()
  withCallingHandlers(
    bundle <- .build_cache_bundle(list(heatmap = list(module_state = list(
      had_heatmap = TRUE,
      restore_cache_dependency = "module_matrix_payload",
      heatmap_expression_matrix = matrix(1:4, nrow = 2L)
    ))), .cache_rv(live)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  state <- bundle$module_snapshots$heatmap$module_state

  expect_length(bundle$plot_data_cache_pool, 0L)
  expect_null(state$plot_data_cache_payload)
  expect_null(state$restore_plot_data_cache)
  expect_length(warnings, 0L)
  expect_identical(state$heatmap_expression_matrix, matrix(1:4, nrow = 2L))
})

test_that("reconstructing heatmap keeps and resolves its historical pair", {
  historical <- .cache_pair()
  live <- .cache_pair(100)
  state <- .shared_state(historical)
  state$had_heatmap <- TRUE
  state$plot_data_cache_payload <- historical
  bundle <- .build_cache_bundle(list(heatmap = list(module_state = state)), .cache_rv(live))
  ref <- bundle$module_snapshots$heatmap$module_state$plot_data_cache_ref

  expect_identical(names(bundle$plot_data_cache_pool), ref)
  expect_identical(bundle$plot_data_cache_pool[[ref]], historical)
  expect_identical(.restore_saved_pair(bundle, "heatmap", .cache_rv(live)), historical)
  expect_false(identical(.restore_saved_pair(bundle, "heatmap", .cache_rv(live)), live))
})
