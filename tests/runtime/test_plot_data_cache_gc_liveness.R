library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")

.gc_entry <- function(value) {
  list(data_mod = data.frame(value = value), data_def = data.frame(kind = "value"))
}

.gc_index <- function(ids) {
  stats::setNames(lapply(ids, function(id) list(fingerprint = paste0("fingerprint-", id))), ids)
}

test_that("hard GC does not give the active dataset implicit liveness", {
  active <- "1|1|active|fp:active"
  referenced <- "1|1|referenced|fp:referenced"
  legacy <- "legacy-entry"
  pool <- stats::setNames(
    list(.gc_entry(1), .gc_entry(2), .gc_entry(3)),
    c(active, referenced, legacy)
  )
  snapshots <- list(plot = list(module_state = list(
    had_plot = TRUE,
    plot_data_cache_ref = referenced
  )))

  old <- options(miraprot.session_gc_mode = "hard")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(
    snapshots, pool, .gc_index(c(active, referenced)), active
  )

  expect_setequal(names(result$pool), c(referenced, legacy))
  expect_identical(result$report$removed_ids, active)
  expect_identical(result$report$active_dataset_kept_entries, 0L)
  expect_identical(result$report$unknown_kept_entries, 1L)
})

test_that("only plot-bearing snapshots make their cache refs live", {
  active <- "1|1|active|fp:active"
  pool <- stats::setNames(list(.gc_entry(1)), active)
  snapshots <- list(plot = list(module_state = list(
    had_plot = FALSE,
    plot_data_cache_ref = active
  )))

  old <- options(miraprot.session_gc_mode = "hard")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(snapshots, pool, .gc_index(active), active)

  expect_length(result$pool, 0L)
  expect_identical(result$report$required_entries, 0L)
  expect_identical(result$report$active_dataset_kept_entries, 0L)
})

test_that("stale scalar refs are ignored while persisted plot titles stay live", {
  stale <- "1|1|stale|fp:stale"
  first <- "1|1|first|fp:first"
  second <- "1|1|second|fp:second"
  snapshots <- list(
    inactive = list(module_state = list(plot_data_cache_ref = stale)),
    gallery = list(module_state = list(
      plot_data_cache_ref = stale,
      plot_cache_ref_by_title = list(Overview = first, Detail = second)
    ))
  )

  required <- .compute_required_cache_ids(snapshots)
  expect_setequal(as.character(required), c(first, second))

  diagnostics <- attr(required, "cache_liveness_diagnostics", exact = TRUE)
  expect_identical(diagnostics[[first]][[1L]]$module_id, "gallery")
  expect_identical(diagnostics[[first]][[1L]]$plot_title, "Overview")
  expect_identical(diagnostics[[second]][[1L]]$plot_title, "Detail")
})

test_that("pending reconstruction makes the scalar cache ref live", {
  ref <- "1|1|pending|fp:pending"
  snapshots <- list(plot = list(module_state = list(
    plot_rebuild_pending = TRUE,
    plot_data_cache_ref = ref
  )))

  required <- .compute_required_cache_ids(snapshots)
  expect_identical(as.character(required), ref)
  diagnostic <- attr(required, "cache_liveness_diagnostics", exact = TRUE)[[ref]][[1L]]
  expect_identical(diagnostic$module_id, "plot")
  expect_identical(diagnostic$plot_title, "<default>")
})

test_that("off GC reports unused entries without changing pool or index", {
  active <- "1|1|active|fp:active"
  referenced <- "1|1|referenced|fp:referenced"
  pool <- stats::setNames(list(.gc_entry(1), .gc_entry(2)), c(active, referenced))
  index <- .gc_index(names(pool))
  snapshots <- list(plot = list(module_state = list(
    plot_reconstruction_pending = TRUE,
    plot_data_cache_ref = referenced
  )))

  old <- options(miraprot.session_gc_mode = "off")
  on.exit(options(old), add = TRUE)
  result <- .gc_plot_data_cache_pool(snapshots, pool, index, active)

  expect_identical(result$pool, pool)
  expect_identical(result$index, index)
  expect_identical(result$report$unused_ids, active)
  expect_identical(result$report$unused_entries, 1L)
  expect_identical(result$report$entries_deleted, 0L)
  expect_identical(result$report$active_dataset_kept_entries, 0L)
})
