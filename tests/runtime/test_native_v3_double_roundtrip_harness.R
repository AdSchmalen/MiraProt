# Focused regression harness for the native full-session restore/export path.
#
# This deliberately exercises the cache-pool and envelope helpers used by the
# download handler without starting a Shiny client.  Keeping it below the UI
# layer makes failures deterministic and lets the test inspect both exports.

library(testthat)
library(shiny)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_orchestration.R")

.roundtrip_module_ids <- c(
  "datawizard", "abundances", "sampleids", "pca", "volcano", "dotplot",
  "venn", "heatmap", "go", "gsea", "string"
)
.roundtrip_shared_cache_ids <- c(
  "abundances", "sampleids", "pca", "volcano", "dotplot", "venn", "heatmap"
)

.roundtrip_runtime_hits <- function(x, path = "snapshot") {
  hits <- character()
  walk <- function(value, at) {
    classes <- class(value)
    forbidden <- intersect(classes, c("ggplot", "plotly", "htmlwidget", "grob", "gTree"))
    if (length(forbidden)) hits <<- c(hits, paste0(at, ":", forbidden))
    if (is.function(value)) hits <<- c(hits, paste0(at, ":function"))
    if (is.environment(value)) hits <<- c(hits, paste0(at, ":environment"))
    if (is.list(value) && !is.environment(value)) {
      labels <- names(value)
      if (is.null(labels)) labels <- as.character(seq_along(value))
      for (i in seq_along(value)) walk(value[[i]], paste0(at, "$", labels[[i]]))
    }
  }
  walk(x, path)
  unique(hits)
}

.roundtrip_progress <- list(set = function(...) invisible(NULL))

.make_roundtrip_original <- function() {
  historical_data_mod <- data.frame(
    Protein = sprintf("P%03d", seq_len(80L)),
    Abundance = seq_len(80L), stringsAsFactors = FALSE
  )
  historical_data_def <- data.frame(
    Column = names(historical_data_mod), Content = c("Identifier", "Abundance"),
    stringsAsFactors = FALSE
  )
  historical_pair <- list(
    data_mod = historical_data_mod,
    data_def = historical_data_def
  )
  # The live Data Wizard tables deliberately differ from the tables with which
  # the saved plots were built.  A restore/export must not silently rebind old
  # plots to these newer values.
  live_pair <- list(
    data_mod = transform(historical_data_mod, Abundance = Abundance + 1000L),
    data_def = transform(historical_data_def, Content = paste0(Content, " (live)"))
  )
  cache_id <- .build_plot_data_cache_id(
    3L, 5L, historical_pair$data_mod, historical_pair$data_def
  )

  snapshots <- setNames(lapply(.roundtrip_module_ids, function(mid) {
    state <- list(schema_version = "2.0", ui_inputs = list(selected = mid))
    if (mid %in% .roundtrip_shared_cache_ids) {
      state$restore_cache_dependency <- "shared_plot_data_cache_pool"
      state$plot_data_cache_ref <- cache_id
      state$restore_plot_data_cache <- historical_pair
      if (identical(mid, "venn")) {
        state$plot_cache_ref_by_title <- list(`Venn diagram` = cache_id)
      }
    }
    list(module_id = mid, module_state = state)
  }), .roundtrip_module_ids)

  rv <- reactiveValues(
    data_mod = live_pair$data_mod, data_def = live_pair$data_def,
    data_mod_revision_id = 3L, data_def_revision_id = 5L,
    gridplot_order = c("pca_main", "volcano_main"),
    plot_spans = list(pca_main = list(colspan = 2L, rowspan = 1L)),
    plot_margins = list(pca_main = list(top = 4, right = 2)),
    gridplot_selection = list(
      pca_main = list(label = "PCA", source = "pca", include_label = TRUE),
      volcano_main = list(label = "Volcano", source = "volcano", include_label = FALSE)
    )
  )
  list(
    rv = rv, snapshots = snapshots,
    historical_pair = historical_pair, live_pair = live_pair
  )
}

.export_roundtrip <- function(rv, snapshots) {
  sanitize_started <- proc.time()[["elapsed"]]
  shared <- isolate(.collect_sanitized_rv_snapshot_for_save(rv, SESSION_SAVE_LEVEL_FULL))
  sanitize_elapsed <- proc.time()[["elapsed"]] - sanitize_started

  visited <- character()
  predicate <- .module_uses_shared_plot_data_cache
  assign(".module_uses_shared_plot_data_cache", function(module_state = NULL,
                                                          legacy_full_session = FALSE) {
    marker <- if (is.list(module_state)) module_state$ui_inputs$selected else NULL
    if (is.character(marker) && length(marker) == 1L) visited <<- c(visited, marker)
    predicate(module_state, legacy_full_session)
  }, envir = environment(.build_save_time_plot_data_cache_bundle))
  on.exit(assign(".module_uses_shared_plot_data_cache", predicate,
                 envir = environment(.build_save_time_plot_data_cache_bundle)), add = TRUE)
  cache <- .build_save_time_plot_data_cache_bundle(
    snapshots, shared$rv_list, SESSION_SAVE_LEVEL_FULL
  )
  assign(".module_uses_shared_plot_data_cache", predicate,
         envir = environment(.build_save_time_plot_data_cache_bundle))

  built <- .build_session_save_envelope(
    shared$rv_list, cache$module_snapshots, SESSION_SAVE_LEVEL_FULL,
    cache$failed_modules, cache$plot_data_cache_pool,
    cache$plot_data_cache_index, cache$gc_report, .roundtrip_progress
  )$envelope
  list(envelope = built, visited = unique(visited), sanitize_elapsed = sanitize_elapsed,
       cache = cache)
}

.restore_roundtrip <- function(envelope) {
  restored <- upgrade_session_snapshot_to_current_schema(unwrap_snapshot(envelope))
  # Mirror the native restore preprocessor: consumers receive their exact
  # historical pool pair as transient recovery evidence before another save.
  for (mid in names(restored$module_snapshots)) {
    state <- restored$module_snapshots[[mid]]$module_state
    if (.module_uses_shared_plot_data_cache(state, legacy_full_session = FALSE)) {
      restored$module_snapshots[[mid]]$module_state <-
        .resolve_plot_data_cache_for_module(state, restored$plot_data_cache_pool)
    }
  }
  rv_state <- restored$rv_snapshot
  grid <- rv_state$grid_session_payload
  if (is.list(grid) && length(grid)) {
    rv_state$grid_session_payload <- NULL
    rv_state$gridplot_order <- vapply(grid, `[[`, character(1L), "stable_plot_id")
    rv_state$gridplot_selection <- setNames(lapply(grid, function(x) list(
      label = x$display_label, source = x$source_module,
      source_plot_id = x$source_plot_id, include_label = x$include_label
    )), rv_state$gridplot_order)
    rv_state$plot_spans <- setNames(lapply(grid, `[[`, "span"), rv_state$gridplot_order)
    rv_state$plot_margins <- setNames(lapply(grid, `[[`, "margins"), rv_state$gridplot_order)
  }
  rv <- do.call(reactiveValues, rv_state)
  list(snapshot = restored, rv = rv, modules = restored$module_snapshots)
}

test_that("native schema 3 remains bounded and equivalent over two restore/export rounds", {
  original <- .make_roundtrip_original()
  export_a <- .export_roundtrip(original$rv, original$snapshots)
  restore_a <- .restore_roundtrip(export_a$envelope)
  export_b <- .export_roundtrip(restore_a$rv, restore_a$modules)
  restore_b <- .restore_roundtrip(export_b$envelope)

  for (exported in list(export_a$envelope, export_b$envelope)) {
    expect_identical(exported$version, MIRAPROT_SESSION_SCHEMA_VERSION)
    expect_true(.session_restore_is_native_current_schema(unwrap_snapshot(exported)))
    expect_setequal(exported$manifest$module_ids, .roundtrip_module_ids)
    expect_length(exported$manifest$module_ids, 11L)
  }
  expect_setequal(export_a$visited, .roundtrip_module_ids)
  expect_setequal(export_b$visited, .roundtrip_module_ids)
  expect_identical(names(export_a$cache$module_snapshots), .roundtrip_module_ids)
  expect_identical(names(export_b$cache$module_snapshots), .roundtrip_module_ids)
  expect_false(any(grepl("restored missing|missing-module repair", export_a$cache$warnings)))
  expect_false(any(grepl("restored missing|missing-module repair", export_b$cache$warnings)))
  # The first shared-cache module inserts the distinct pair; every subsequent
  # module carrying that pair must reuse the same entry rather than duplicate it.
  expect_length(export_a$cache$plot_data_cache_pool, 1L)
  expect_length(unique(vapply(
    export_a$cache$module_snapshots[.roundtrip_shared_cache_ids],
    function(module) module$module_state$plot_data_cache_ref,
    character(1L)
  )), 1L)

  historical_id <- .build_plot_data_cache_id(
    3L, 5L, original$historical_pair$data_mod, original$historical_pair$data_def
  )
  live_id <- .build_plot_data_cache_id(
    3L, 5L, original$live_pair$data_mod, original$live_pair$data_def
  )
  expect_false(identical(historical_id, live_id))

  for (exported in list(export_a$envelope, export_b$envelope)) {
    payload <- unwrap_snapshot(exported)
    for (mid in .roundtrip_shared_cache_ids) {
      state <- payload$module_snapshots[[mid]]$module_state
      expect_identical(state$restore_cache_dependency, "shared_plot_data_cache_pool", info = mid)
      expect_identical(state$plot_data_cache_ref, historical_id, info = mid)
      resolved <- .safe_cache_pool_get(payload$plot_data_cache_pool, state$plot_data_cache_ref)
      expect_identical(resolved, original$historical_pair, info = mid)
    }
    venn_state <- payload$module_snapshots$venn$module_state
    expect_identical(venn_state$plot_cache_ref_by_title$`Venn diagram`, historical_id)
    expect_identical(
      .safe_cache_pool_get(payload$plot_data_cache_pool,
                           venn_state$plot_cache_ref_by_title$`Venn diagram`),
      original$historical_pair
    )
    expect_false(live_id %in% names(payload$plot_data_cache_pool))
    for (module in payload$module_snapshots) {
      expect_false(any(c("restore_plot_data_cache", "plot_data_cache_payload",
                         "restore_plot_data_cache_by_title") %in%
                         names(module$module_state)))
    }
    expect_equal(.roundtrip_runtime_hits(payload$rv_snapshot), character())
    grid <- payload$rv_snapshot$grid_session_payload
    expect_true(all(vapply(grid, function(x) {
      setequal(names(x), c("stable_plot_id", "source_module", "source_plot_id",
                           "display_label", "include_label", "order", "span", "margins"))
    }, logical(1L))))
    expect_equal(.roundtrip_runtime_hits(grid), character())
  }

  a_payload <- unwrap_snapshot(export_a$envelope)
  b_payload <- unwrap_snapshot(export_b$envelope)
  refs_a <- vapply(a_payload$module_snapshots[.roundtrip_shared_cache_ids],
                   function(x) x$module_state$plot_data_cache_ref, character(1L))
  refs_b <- vapply(b_payload$module_snapshots[.roundtrip_shared_cache_ids],
                   function(x) x$module_state$plot_data_cache_ref, character(1L))
  expect_identical(refs_b, refs_a)
  expect_identical(names(b_payload$plot_data_cache_pool),
                   names(a_payload$plot_data_cache_pool))
  for (cache_id in names(a_payload$plot_data_cache_pool)) {
    expect_identical(b_payload$plot_data_cache_pool[[cache_id]],
                     a_payload$plot_data_cache_pool[[cache_id]], info = cache_id)
  }
  expect_lte(length(names(b_payload$rv_snapshot)), length(names(a_payload$rv_snapshot)))
  expect_lte(as.numeric(object.size(export_b$envelope)),
             as.numeric(object.size(export_a$envelope)) * 1.25 + 4096)
  expect_identical(restore_a$snapshot$compatibility_upgrade$mode, "native_current_schema")
  expect_identical(restore_b$snapshot$compatibility_upgrade$mode, "native_current_schema")
  expect_identical(restore_b$snapshot$rv_snapshot, restore_a$snapshot$rv_snapshot)
  expect_identical(restore_b$snapshot$module_snapshots, restore_a$snapshot$module_snapshots)

  # Generous guard: intended to catch accidental deep walks of runtime graphs,
  # never to benchmark a particular machine.
  expect_lt(export_a$sanitize_elapsed, 10)
  expect_lt(export_b$sanitize_elapsed, 10)
})

test_that("restored transient payloads converge on one stable shared-cache contract", {
  data_mod <- data.frame(
    Protein = c("P001", "P002", "P003"),
    Abundance = c(12.5, 8.25, 19),
    stringsAsFactors = FALSE
  )
  data_def <- data.frame(
    Column = names(data_mod),
    Content = c("Identifier", "Abundance"),
    stringsAsFactors = FALSE
  )
  pair <- list(data_mod = data_mod, data_def = data_def)
  revisions <- list(data_mod_revision_id = 12L, data_def_revision_id = 18L)
  canonical <- .plot_data_cache_ref_contract(
    revisions$data_mod_revision_id, revisions$data_def_revision_id,
    data_mod, data_def
  )

  make_state <- function(ref) {
    c(list(
      restore_cache_dependency = "shared_plot_data_cache_pool",
      plot_data_cache_ref = ref,
      # This is intentionally transient recovery evidence from restore.
      restore_plot_data_cache = pair
    ), revisions)
  }
  snapshots <- list(
    abundances = list(module_id = "abundances", module_state = make_state("0|0|restored-stale")),
    sampleids = list(module_id = "sampleids", module_state = make_state(canonical$plot_data_cache_ref))
  )
  rv_list <- c(pair, revisions)

  # Contract normalization itself must not discard recovery evidence.  Only
  # the final shared-pool resolution performed by the bundle builder may do so.
  staged <- .normalize_module_cache_ref_contract(
    snapshots$abundances$module_state,
    data_mod_revision_id = revisions$data_mod_revision_id,
    data_def_revision_id = revisions$data_def_revision_id,
    drop_embedded_pair = FALSE
  )
  expect_identical(staged$restore_plot_data_cache, pair)

  normalized <- .build_save_time_plot_data_cache_bundle(
    snapshots, rv_list, SESSION_SAVE_LEVEL_FULL
  )
  expect_length(normalized$plot_data_cache_pool, 1L)
  normalized_ids <- vapply(normalized$module_snapshots, function(module) {
    state <- module$module_state
    resolved <- .safe_cache_pool_get(normalized$plot_data_cache_pool, state$plot_data_cache_ref)
    expect_true(.is_plot_cache_pair(resolved), info = module$module_id)
    expect_true(.cache_ref_contract_compatible(
      .module_cache_ref_contract(state), resolved$data_mod, resolved$data_def
    ), info = module$module_id)
    expect_null(state$restore_plot_data_cache, info = module$module_id)
    expect_null(state$plot_data_cache_payload, info = module$module_id)
    state$plot_data_cache_ref
  }, character(1L))
  expect_length(unique(normalized_ids), 1L)
  expect_identical(unname(normalized_ids[[1L]]), canonical$plot_data_cache_ref)

  envelope <- .build_session_save_envelope(
    rv_list, normalized$module_snapshots, SESSION_SAVE_LEVEL_FULL,
    normalized$failed_modules, normalized$plot_data_cache_pool,
    normalized$plot_data_cache_index, normalized$gc_report, .roundtrip_progress
  )$envelope
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(envelope, path)
  restored <- unwrap_snapshot(readRDS(path))
  expect_s3_class(restored$rv_snapshot$data_mod, "data.frame")
  expect_identical(restored$rv_snapshot$data_mod, data_mod)

  normalized_again <- .build_save_time_plot_data_cache_bundle(
    restored$module_snapshots, restored$rv_snapshot, SESSION_SAVE_LEVEL_FULL
  )
  repeated_ids <- vapply(normalized_again$module_snapshots, function(module) {
    module$module_state$plot_data_cache_ref
  }, character(1L))
  expect_identical(repeated_ids, normalized_ids)
  expect_identical(names(normalized_again$plot_data_cache_pool),
                   names(normalized$plot_data_cache_pool))
  expect_length(normalized_again$plot_data_cache_pool, 1L)
  expect_false(any(vapply(normalized_again$module_snapshots, function(module) {
    state <- module$module_state
    any(c("restore_plot_data_cache", "plot_data_cache_payload", "data_mod", "data_def") %in%
          names(state))
  }, logical(1L))))
})
