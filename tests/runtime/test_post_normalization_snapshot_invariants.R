library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_orchestration.R")

.invariant_pair <- function(offset = 0) {
  list(
    data_mod = data.frame(Protein = c("P1", "P2"), S1 = c(1, 2) + offset),
    data_def = data.frame(Column = c("Protein", "S1"), Content = c("Identifier", "Abundance"))
  )
}

.invariant_state <- function(pair, ref = NULL) {
  contract <- .plot_data_cache_ref_contract(4L, 9L, pair$data_mod, pair$data_def, ref)
  c(list(restore_cache_dependency = "shared_plot_data_cache_pool"), contract)
}

.different_invariant_pairs <- function() {
  list(
    historical_pair = .invariant_pair(0),
    live_pair = .invariant_pair(1000)
  )
}

.registered_plot_module_ids <- c(
  "sampleids", "pca", "volcano", "dotplot", "venn", "abundances", "heatmap"
)

.plot_module_intent_state <- function(module_id, active, pair) {
  intent <- switch(module_id,
    sampleids = list(had_plot = active),
    pca = list(had_plot = active),
    volcano = list(had_static_plots = active),
    dotplot = list(plot_ready = active),
    venn = list(had_plot = active, plot_active = active),
    abundances = list(had_plot = active),
    heatmap = list(had_heatmap = active, plot_request = if (active) list(type = "expression") else NULL)
  )
  intent$restore_cache_dependency <- if (active) "shared_plot_data_cache_pool" else "none"
  if (active) {
    contract <- .plot_data_cache_ref_contract(4L, 9L, pair$data_mod, pair$data_def)
    intent[names(contract)] <- contract
    intent$plot_data_cache_payload <- pair
    intent$plot_cache_ref_by_title <- stats::setNames(
      list(contract$plot_data_cache_ref), paste0(module_id, "::main")
    )
  }
  intent
}

test_that("every registered plot participant is inert when inactive and cache-backed when active", {
  pair <- .invariant_pair()
  rv_list <- c(pair, list(data_mod_revision_id = 4L, data_def_revision_id = 9L))

  for (module_id in .registered_plot_module_ids) {
    for (active in c(FALSE, TRUE)) {
      snapshots <- setNames(list(list(
        module_id = module_id,
        module_state = .plot_module_intent_state(module_id, active, pair)
      )), module_id)
      result <- .build_save_time_plot_data_cache_bundle(
        snapshots, rv_list, SESSION_SAVE_LEVEL_FULL
      )
      state <- result$module_snapshots[[module_id]]$module_state
      expect_identical(state$restore_cache_dependency,
                       if (active) "shared_plot_data_cache_pool" else "none",
                       info = paste(module_id, active))
      expect_length(result$plot_data_cache_pool, if (active) 1L else 0L,
                    info = paste(module_id, active))
      if (active) {
        resolved <- .safe_cache_pool_get(result$plot_data_cache_pool,
                                         state$plot_data_cache_ref)
        expect_identical(resolved, pair, info = module_id)
        expect_true(.cache_ref_contract_compatible(
          .module_cache_ref_contract(state), resolved$data_mod, resolved$data_def
        ), info = module_id)
        expect_true(.validate_plot_cache_ref_by_title(
          state$plot_cache_ref_by_title, result$plot_data_cache_pool
        )$valid, info = module_id)
      } else {
        expect_null(state$plot_cache_ref_by_title, info = module_id)
        expect_null(state$plot_data_cache_ref, info = module_id)
      }
    }
  }
})

test_that("one active plot does not make inactive registered participants join the pool", {
  pair <- .invariant_pair()
  rv_list <- c(pair, list(data_mod_revision_id = 4L, data_def_revision_id = 9L))
  for (active_id in .registered_plot_module_ids) {
    snapshots <- setNames(lapply(.registered_plot_module_ids, function(module_id) list(
      module_id = module_id,
      module_state = .plot_module_intent_state(module_id, identical(module_id, active_id), pair)
    )), .registered_plot_module_ids)
    result <- .build_save_time_plot_data_cache_bundle(
      snapshots, rv_list, SESSION_SAVE_LEVEL_FULL
    )
    expect_length(result$plot_data_cache_pool, 1L, info = active_id)
    expect_length(result$failed_modules, 0L, info = active_id)
    for (inactive_id in setdiff(.registered_plot_module_ids, active_id)) {
      state <- result$module_snapshots[[inactive_id]]$module_state
      expect_identical(state$restore_cache_dependency, "none", info = inactive_id)
      expect_null(state$plot_data_cache_ref, info = inactive_id)
      expect_null(state$plot_cache_ref_by_title, info = inactive_id)
    }
  }
})

test_that("embedded recovery is removed only for a completely resolved contract", {
  pair <- .invariant_pair()
  state <- .invariant_state(pair)
  ref <- state$plot_data_cache_ref
  state$plot_cache_ref_by_title <- list(Main = ref)
  state$plot_data_cache_payload <- pair
  snapshots <- list(pca = list(module_state = state))
  pool <- setNames(list(pair), ref)
  debug_messages <- character()
  recorded <- character()

  expect_no_warning(
    result <- .session_enforce_post_normalization_snapshot_invariants(
      snapshots,
      pool,
      debug_log = function(message, level) {
        debug_messages <<- c(debug_messages, message)
      },
      record_warning = function(mid, message) recorded <<- c(recorded, message)
    )
  )

  expect_null(result$pca$module_state$plot_data_cache_payload)
  expect_null(result$pca$module_state$restore_plot_data_cache)
  expect_match(debug_messages, "action=removed_for_shared_plot_data_cache_pool")
  expect_length(recorded, 0L)
})

test_that("an unresolved title reference is promoted to the recovered canonical pair", {
  pair <- .invariant_pair()
  state <- .invariant_state(pair)
  ref <- state$plot_data_cache_ref
  state$plot_cache_ref_by_title <- list(Main = ref, Missing = "missing")
  state$plot_data_cache_payload <- pair
  state$restore_plot_data_cache <- pair
  snapshots <- list(pca = list(module_state = state))
  pool <- setNames(list(pair), ref)
  recorded <- character()

  expect_no_warning(
    result <- .session_enforce_post_normalization_snapshot_invariants(
      snapshots, pool,
      record_warning = function(mid, message) recorded <<- c(recorded, message)
    )
  )

  expect_null(result$pca$module_state$plot_data_cache_payload)
  expect_null(result$pca$module_state$restore_plot_data_cache)
  expect_true(all(vapply(
    result$pca$module_state$plot_cache_ref_by_title,
    identical, logical(1L), ref
  )))
  expect_length(recorded, 0L)
})

test_that("downgrade diagnostics distinguish cache contract failure classes", {
  pair <- .invariant_pair()
  wrong_pair <- .invariant_pair(100)

  diagnostic_for <- function(state, pool) {
    warnings <- character()
    suppressWarnings(.session_enforce_post_normalization_snapshot_invariants(
      list(venn = list(module_state = state)), pool,
      record_warning = function(mid, message) warnings <<- c(warnings, message)
    ))
    warnings
  }

  primary <- .invariant_state(pair)
  primary$plot_data_cache_ref <- ""
  primary$plot_data_cache_payload <- pair
  expect_match(diagnostic_for(primary, list()), "failure=primary_reference")

  missing <- .invariant_state(pair, "absent")
  missing$plot_data_cache_payload <- pair
  expect_match(diagnostic_for(missing, list()), "failure=missing_pool_entry")

  title <- .invariant_state(pair, "noncanonical-ref")
  title$plot_cache_ref_by_title <- list(Main = "absent")
  title$plot_data_cache_payload <- pair
  expect_match(
    diagnostic_for(title, setNames(list(pair), "noncanonical-ref")),
    "failure=title_reference"
  )

  incompatible <- .invariant_state(pair)
  incompatible$plot_data_cache_payload <- pair
  expect_match(
    diagnostic_for(incompatible, setNames(list(wrong_pair), incompatible$plot_data_cache_ref)),
    "failure=contract_compatibility"
  )

  mismatch <- .invariant_state(pair)
  mismatch$plot_data_cache_payload <- wrong_pair
  expect_match(
    diagnostic_for(mismatch, setNames(list(pair), mismatch$plot_data_cache_ref)),
    "failure=intended_pair_mismatch"
  )
})

test_that("title map values must be scalar cache ids and keys are never cache ids", {
  pair <- .invariant_pair()
  ref <- .build_plot_data_cache_id(data_mod = pair$data_mod, data_def = pair$data_def)
  pool <- setNames(list(pair), ref)

  expect_false(.validate_plot_cache_ref_by_title(list(Main = ""), pool)$valid)
  expect_false(.validate_plot_cache_ref_by_title(list(Main = c(ref, ref)), pool)$valid)
  expect_false(.validate_plot_cache_ref_by_title(setNames(list("missing"), ref), pool)$valid)
  expect_true(.validate_plot_cache_ref_by_title(list(Main = ref), pool)$valid)

  state <- list(plot_cache_ref_by_title = setNames(list("missing"), ref))
  restored <- .resolve_plot_data_cache_for_module(state, pool)
  expect_null(restored$restore_plot_data_cache_by_title)
  expect_false(restored$plot_cache_ref_by_title_valid)
})

test_that("a mismatched contract does not authorize dropping recovery data", {
  pair <- .invariant_pair()
  wrong_pair <- .invariant_pair(100)
  state <- .invariant_state(pair)
  ref <- state$plot_data_cache_ref
  state$plot_data_cache_payload <- pair
  snapshots <- list(pca = list(module_state = state))

  expect_warning(
    result <- .session_enforce_post_normalization_snapshot_invariants(
      snapshots, setNames(list(wrong_pair), ref)
    ),
    "shared cache contract unresolved"
  )
  expect_true(.is_plot_cache_pair(result$pca$module_state$plot_data_cache_payload))
  expect_identical(result$pca$module_state$restore_cache_dependency, "none")
  expect_null(result$pca$module_state$plot_data_cache_ref)
})

test_that("a repairable shared reference is rewritten to the recovered canonical pair", {
  pair <- .invariant_pair()
  canonical <- .plot_data_cache_ref_contract(4L, 9L, pair$data_mod, pair$data_def, NULL)
  state <- .invariant_state(pair, "stale-ref")
  state$plot_cache_ref_by_title <- list(Main = "also-stale")
  state$plot_data_cache_payload <- pair

  result <- .session_enforce_post_normalization_snapshot_invariants(
    list(pca = list(module_state = state)),
    setNames(list(pair), canonical$plot_data_cache_ref),
    data_mod_revision_id = 4L,
    data_def_revision_id = 9L
  )

  repaired <- result$pca$module_state
  expect_identical(repaired$plot_data_cache_ref, canonical$plot_data_cache_ref)
  expect_identical(repaired$plot_cache_ref_by_title$Main, canonical$plot_data_cache_ref)
  expect_null(repaired$plot_data_cache_payload)
  expect_identical(repaired$restore_cache_dependency, "shared_plot_data_cache_pool")
})

test_that("an unresolved shared declaration without recovery aborts the save", {
  pair <- .invariant_pair()
  state <- .invariant_state(pair, "missing")
  snapshots <- list(pca = list(module_state = state))

  expect_error(
    .session_enforce_post_normalization_snapshot_invariants(snapshots, list()),
    "no valid embedded recovery pair"
  )
})

test_that("explicit inactive plot state removes an unresolved shared declaration", {
  state <- list(
    had_plot = FALSE,
    restore_cache_dependency = "shared_plot_data_cache_pool",
    plot_data_cache_ref = "missing",
    plot_cache_ref_by_title = list(Main = "missing")
  )
  debug_messages <- character()

  result <- expect_no_warning(
    .session_enforce_post_normalization_snapshot_invariants(
      list(pca = list(module_state = state)), list(),
      debug_log = function(message, level) debug_messages <<- c(debug_messages, message)
    )
  )$pca$module_state

  expect_identical(result$restore_cache_dependency, "none")
  expect_null(result$plot_data_cache_ref)
  expect_null(result$plot_cache_ref_by_title)
  expect_null(result$plot_data_cache_payload)
  expect_null(result$restore_plot_data_cache)
  expect_null(result$restore_plot_data_cache_by_title)
  expect_match(debug_messages, "action=removed_inactive_shared_cache_declaration")
})

test_that("true or absent plot intent remains strict for unresolved shared declarations", {
  shared_state <- list(
    restore_cache_dependency = "shared_plot_data_cache_pool",
    plot_data_cache_ref = "missing"
  )

  expect_error(
    .session_enforce_post_normalization_snapshot_invariants(
      list(pca = list(module_state = c(list(had_plot = TRUE), shared_state))), list()
    ),
    "no valid embedded recovery pair"
  )
  expect_error(
    .session_enforce_post_normalization_snapshot_invariants(
      list(pca = list(module_state = shared_state)), list()
    ),
    "no valid embedded recovery pair"
  )
})

test_that("valid shared references and title maps are unchanged", {
  pair <- .invariant_pair()
  state <- .invariant_state(pair)
  state$had_plot <- TRUE
  state$plot_cache_ref_by_title <- list(Main = state$plot_data_cache_ref)
  pool <- setNames(list(pair), state$plot_data_cache_ref)

  result <- .session_enforce_post_normalization_snapshot_invariants(
    list(pca = list(module_state = state)), pool
  )$pca$module_state

  expect_identical(result$plot_data_cache_ref, state$plot_data_cache_ref)
  expect_identical(result$plot_cache_ref_by_title, state$plot_cache_ref_by_title)
  expect_identical(result$restore_cache_dependency, state$restore_cache_dependency)
})

test_that("envelope assertion is non-mutating and accepts finalized embedded fallback", {
  pair <- .invariant_pair()
  fallback <- list(
    restore_cache_dependency = "none",
    plot_data_cache_payload = pair,
    restore_cache_degraded_reason = "embedded_recovery"
  )
  snapshots <- list(pca = list(module_state = fallback))
  before <- serialize(snapshots, NULL)

  expect_silent(.session_assert_finalized_snapshot_cache_invariants(snapshots, list()))
  expect_identical(serialize(snapshots, NULL), before)
})

test_that("envelope assertion rejects invalid finalized shared contracts", {
  pair <- .invariant_pair()
  state <- .invariant_state(pair)
  ref <- state$plot_data_cache_ref

  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      list(pca = list(module_state = state)), list()
    ),
    "missing from the pool"
  )

  incompatible <- state
  incompatible$data_mod_revision_id <- 100L
  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      list(pca = list(module_state = incompatible)), setNames(list(pair), ref)
    ),
    "incompatible revision, dimension, or fingerprint"
  )

  other <- .invariant_pair(100)
  other_ref <- .plot_data_cache_ref_contract(
    4L, 9L, other$data_mod, other$data_def, NULL
  )$plot_data_cache_ref
  different <- state
  different$plot_cache_ref_by_title <- list(Other = other_ref)
  pool <- setNames(list(pair, other), c(ref, other_ref))
  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      list(pca = list(module_state = different)), pool
    ),
    "resolve to different data pairs"
  )

  no_reference <- list(restore_cache_dependency = "shared_plot_data_cache_pool")
  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      list(pca = list(module_state = no_reference)), list()
    ),
    "no primary reference"
  )
})

test_that("Venn retains exactly one historical fallback when its shared entry is unavailable", {
  pairs <- .different_invariant_pairs()
  historical <- pairs$historical_pair
  live <- pairs$live_pair
  state <- .invariant_state(historical)
  state$plot_cache_ref_by_title <- list(`Venn diagram` = state$plot_data_cache_ref)
  state$restore_plot_data_cache <- historical
  state$plot_data_cache_payload <- live
  live_contract <- .plot_data_cache_ref_contract(4L, 9L, live$data_mod, live$data_def)
  warnings <- character()

  expect_warning(
    finalized <- .session_enforce_post_normalization_snapshot_invariants(
      list(venn = list(module_state = state)),
      setNames(list(live), live_contract$plot_data_cache_ref),
      record_warning = function(mid, message) warnings <<- c(warnings, message)
    ),
    "keeping one embedded plot-data recovery pair"
  )

  result <- finalized$venn$module_state
  expect_identical(result$restore_cache_dependency, "none")
  expect_identical(result$plot_data_cache_payload, historical)
  expect_null(result$restore_plot_data_cache)
  expect_null(result$plot_data_cache_ref)
  expect_null(result$plot_cache_ref_by_title)
  expect_length(warnings, 1L)
})

test_that("Heatmap cache intent distinguishes empty, native, and rebuildable states", {
  pairs <- .different_invariant_pairs()
  historical <- pairs$historical_pair
  live <- pairs$live_pair
  revisions <- list(data_mod_revision_id = 4L, data_def_revision_id = 9L)
  rv_list <- c(live, revisions)

  empty <- .build_save_time_plot_data_cache_bundle(
    list(heatmap = list(module_state = list(
      had_heatmap = FALSE,
      restore_cache_dependency = "none"
    ))),
    rv_list, SESSION_SAVE_LEVEL_FULL
  )
  expect_length(empty$plot_data_cache_pool, 0L)
  expect_false(any(c("restore_plot_data_cache", "plot_data_cache_payload") %in%
                     names(empty$module_snapshots$heatmap$module_state)))

  native_matrix <- matrix(1:6, nrow = 2L)
  native <- .build_save_time_plot_data_cache_bundle(
    list(heatmap = list(module_state = list(
      had_heatmap = TRUE,
      restore_cache_dependency = "module_matrix_payload",
      heatmap_expression_matrix = native_matrix
    ))),
    rv_list, SESSION_SAVE_LEVEL_FULL
  )
  expect_length(native$plot_data_cache_pool, 0L)
  expect_identical(
    native$module_snapshots$heatmap$module_state$heatmap_expression_matrix,
    native_matrix
  )
  expect_false(any(c("restore_plot_data_cache", "plot_data_cache_payload", "data_mod", "data_def") %in%
                     names(native$module_snapshots$heatmap$module_state)))

  rebuild_state <- .invariant_state(historical)
  rebuild_state$had_heatmap <- TRUE
  rebuild_state$restore_plot_data_cache <- historical
  rebuilt <- .build_save_time_plot_data_cache_bundle(
    list(heatmap = list(module_state = rebuild_state)),
    rv_list, SESSION_SAVE_LEVEL_FULL
  )
  rebuilt_state <- rebuilt$module_snapshots$heatmap$module_state
  expect_length(rebuilt$plot_data_cache_pool, 1L)
  expect_identical(
    .safe_cache_pool_get(rebuilt$plot_data_cache_pool, rebuilt_state$plot_data_cache_ref),
    historical
  )
  expect_false(identical(
    .safe_cache_pool_get(rebuilt$plot_data_cache_pool, rebuilt_state$plot_data_cache_ref),
    live
  ))
  expect_null(rebuilt_state$restore_plot_data_cache)
  expect_null(rebuilt_state$plot_data_cache_payload)
})

test_that("final invariant validation is idempotent and does not duplicate warnings", {
  pairs <- .different_invariant_pairs()
  state <- .invariant_state(pairs$historical_pair)
  state$restore_plot_data_cache <- pairs$historical_pair
  warnings <- character()
  validate <- function(snapshots) {
    .session_enforce_post_normalization_snapshot_invariants(
      snapshots, list(),
      record_warning = function(mid, message) warnings <<- c(warnings, message)
    )
  }

  first <- expect_warning(
    validate(list(pca = list(module_state = state))),
    "keeping one embedded plot-data recovery pair"
  )
  first_bytes <- serialize(first, NULL)
  expect_length(warnings, 1L)
  second <- expect_no_warning(validate(first))

  expect_identical(serialize(second, NULL), first_bytes)
  expect_length(warnings, 1L)
})
