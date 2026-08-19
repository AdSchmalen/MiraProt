# Runtime export invariants for module session snapshots.
#
# These tests intentionally allow legacy restore code paths to keep readers for
# old plot-object snapshots. The invariant applies to freshly collected session
# envelopes and to new get_session_state() module payloads: exported state must
# contain replay metadata, cache references, and serializable data only.

library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_orchestration.R")

.runtime_export_modules <- c(
  abundances = "modules/abundances_module.R",
  sampleids = "modules/sampleids_module.R",
  pca = "modules/pca_module.R",
  volcano = "modules/volcano_module.R",
  dotplot = "modules/dotplot_module.R",
  venn = "modules/Venn_module.R",
  heatmap = "modules/Heatmap_module.R",
  go = "modules/GO_module.R",
  gsea = "modules/GSEA_module.R",
  string = "modules/STRING_module.R"
)

.runtime_snapshot_roots <- function(exported) {
  if (is.list(exported) && is.list(exported$payload_inline) &&
      is.list(exported$payload_inline$module_snapshots)) {
    return(exported$payload_inline$module_snapshots)
  }
  if (is.list(exported) && is.list(exported$module_snapshots)) {
    return(exported$module_snapshots)
  }
  exported
}

.find_export_runtime_objects <- function(exported, approved_shiny_tag_paths = character(), max_hits = 100L) {
  roots <- .runtime_snapshot_roots(exported)
  hits <- .find_session_runtime_objects(roots, path = "module_snapshots", max_hits = max_hits)
  if (length(approved_shiny_tag_paths) > 0L) {
    approved <- paste0(approved_shiny_tag_paths, ":shiny.tag")
    hits <- setdiff(hits, approved)
  }
  hits
}

.expect_runtime_clean_export <- function(exported, approved_shiny_tag_paths = character()) {
  runtime_hits <- .find_export_runtime_objects(
    exported,
    approved_shiny_tag_paths = approved_shiny_tag_paths
  )
  expect_equal(runtime_hits, character(), info = paste(runtime_hits, collapse = "\n"))
}

test_that("runtime object finder covers plot/widget/proto/environment/tag objects", {
  fake_snapshot <- list(
    gg = structure(list(), class = "ggplot"),
    ply = structure(list(), class = c("plotly", "htmlwidget")),
    widget = structure(list(), class = "htmlwidget"),
    proto = structure(list(), class = "ggproto"),
    env = new.env(parent = emptyenv()),
    tag = structure(list(name = "div"), class = "shiny.tag")
  )

  hits <- .find_export_runtime_objects(list(module_snapshots = list(pca = list(module_state = fake_snapshot))))

  expect_true(any(grepl(":ggplot$", hits)))
  expect_true(any(grepl(":plotly$", hits)))
  expect_true(any(grepl(":htmlwidget$", hits)))
  expect_true(any(grepl(":ggproto$", hits)))
  expect_true(any(grepl(":environment$", hits)))
  expect_true(any(grepl(":shiny.tag$", hits)))
})


test_that("export invariant scanner rejects new snapshots with runtime objects", {
  contaminated <- list(
    payload_inline = list(
      module_snapshots = list(
        volcano = list(
          module_id = "volcano",
          module_state = list(plot = structure(list(), class = "ggplot"))
        )
      )
    )
  )

  expect_error(
    .expect_runtime_clean_export(contaminated),
    regexp = "module_snapshots\\$volcano\\$module_state\\$plot:ggplot",
    fixed = FALSE
  )
})

test_that("shiny.tag objects require explicit path approval", {
  tagged <- list(
    module_snapshots = list(
      go = list(
        module_id = "go",
        module_state = list(ui = structure(list(name = "div"), class = "shiny.tag"))
      )
    )
  )
  approved_path <- "module_snapshots$go$module_state$ui"

  expect_true(any(grepl(":shiny.tag$", .find_export_runtime_objects(tagged))))
  .expect_runtime_clean_export(tagged, approved_shiny_tag_paths = approved_path)
})

test_that("export invariant scanner accepts v3 envelopes and inline snapshots", {
  clean_snapshots <- setNames(lapply(names(.runtime_export_modules), function(module_id) {
    list(
      module_id = module_id,
      module_state = list(
        version = "2.0",
        ui_inputs = list(selected = module_id),
        plot_data_cache_ref = paste0(module_id, "_cache")
      )
    )
  }), names(.runtime_export_modules))

  .expect_runtime_clean_export(list(module_snapshots = clean_snapshots))
  .expect_runtime_clean_export(list(payload_inline = list(module_snapshots = clean_snapshots)))
})

test_that("new get_session_state modules are covered by runtime export invariant", {
  for (module_path in unname(.runtime_export_modules)) {
    expect_true(file.exists(module_path), info = module_path)
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    expect_match(module_text, "get_session_state\\s*=\\s*function", info = module_path)
  }
})

test_that("active plot-data cache entries are aliased and materialized for shared-cache modules", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(10, 20))
  data_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  rv_snapshot <- list(
    data_mod = data_mod,
    data_def = data_def,
    data_mod_revision_id = 7L,
    data_def_revision_id = 11L
  )
  active_id <- .build_plot_data_cache_id(
    data_mod_revision_id = rv_snapshot$data_mod_revision_id,
    data_def_revision_id = rv_snapshot$data_def_revision_id,
    data_mod = rv_snapshot$data_mod,
    data_def = rv_snapshot$data_def
  )
  pool <- setNames(list(list(data_mod = data_mod, data_def = data_def)), active_id)

  aliased <- .alias_active_dataset_cache_entries(pool, active_id, rv_snapshot)
  expect_identical(aliased[[active_id]]$kind, "canonical_active_dataset_ref")
  expect_false("data_mod" %in% names(aliased[[active_id]]))
  expect_false("data_def" %in% names(aliased[[active_id]]))

  materialized <- .materialize_active_dataset_cache_aliases(aliased, rv_snapshot)
  expect_identical(materialized[[active_id]], list(data_mod = data_mod, data_def = data_def))

  modules <- c("abundances", "sampleids", "pca", "volcano", "dotplot", "venn", "heatmap")
  for (module_id in modules) {
    state <- list(
      restore_cache_dependency = "shared_plot_data_cache_pool",
      plot_data_cache_ref = active_id,
      data_mod_revision_id = rv_snapshot$data_mod_revision_id,
      data_def_revision_id = rv_snapshot$data_def_revision_id,
      plot_data_cache_fingerprint = .plot_data_cache_fingerprint(data_mod, data_def)
    )
    restored <- .resolve_plot_data_cache_for_module(state, materialized)
    expect_true(inherits(restored$restore_plot_data_cache$data_mod, "data.frame"), info = module_id)
    expect_true(inherits(restored$restore_plot_data_cache$data_def, "data.frame"), info = module_id)
    expect_identical(restored$restore_plot_data_cache$data_mod, data_mod, info = module_id)
    expect_identical(restored$restore_plot_data_cache$data_def, data_def, info = module_id)
    expect_false(isTRUE(restored$restore_cache_degraded), info = module_id)
  }
})

test_that("non-active or mismatched plot-data cache entries are not aliased", {
  live_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(10, 20))
  live_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  stale_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(100, 200))
  stale_def <- live_def
  rv_snapshot <- list(
    data_mod = live_mod,
    data_def = live_def,
    data_mod_revision_id = 7L,
    data_def_revision_id = 11L
  )
  active_id <- .build_plot_data_cache_id(7L, 11L, live_mod, live_def)
  stale_id <- .build_plot_data_cache_id(7L, 11L, stale_mod, stale_def)
  pool <- list()
  pool[[active_id]] <- list(data_mod = stale_mod, data_def = stale_def)
  pool[[stale_id]] <- list(data_mod = stale_mod, data_def = stale_def)

  aliased <- .alias_active_dataset_cache_entries(pool, active_id, rv_snapshot)
  expect_false(.is_canonical_active_dataset_cache_alias(aliased[[active_id]]))
  expect_false(.is_canonical_active_dataset_cache_alias(aliased[[stale_id]]))
  expect_identical(aliased[[active_id]]$data_mod, stale_mod)
  expect_identical(aliased[[stale_id]]$data_mod, stale_mod)
})

test_that("active-dataset alias resolver rejects every malformed identity field", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(10, 20))
  data_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  revisions <- list(data_mod_revision_id = 7L, data_def_revision_id = 11L)
  rv_snapshot <- c(list(data_mod = data_mod, data_def = data_def), revisions)
  active_id <- .build_plot_data_cache_id(7L, 11L, data_mod, data_def)
  pair <- list(data_mod = data_mod, data_def = data_def)
  alias <- .alias_active_dataset_cache_entries(
    setNames(list(pair), active_id), active_id, rv_snapshot
  )[[active_id]]

  resolve <- function(entry = alias, id = active_id, mod = data_mod) {
    .resolve_plot_data_cache_pool_entry(
      setNames(list(entry), id), id, mod, data_def, 7L, 11L
    )
  }
  expect_identical(resolve(), pair)
  expect_identical(
    .resolve_plot_data_cache_pool_entry(setNames(list(pair), active_id), active_id),
    pair
  )

  for (field in setdiff(names(alias), "kind")) {
    malformed <- alias
    malformed[[field]] <- if (is.integer(malformed[[field]])) {
      malformed[[field]] + 1L
    } else {
      paste0(malformed[[field]], "-stale")
    }
    expect_null(resolve(malformed), info = field)
  }
  missing_field <- alias
  missing_field$fingerprint <- NULL
  expect_null(resolve(missing_field))
  alias_with_payload <- alias
  alias_with_payload$data_mod <- data_mod
  expect_null(resolve(alias_with_payload))
  expect_null(resolve(alias, mod = transform(data_mod, Abundance = Abundance + 100)))
})

test_that("full session snapshots without plots do not demand plot-data pool frames", {
  module_snapshots <- list(
    datawizard = list(module_state = list(data_mod = data.frame(x = 1))),
    pca = list(module_state = list(ui_inputs = list(choice = "none"))),
    heatmap = list(module_state = list(restore_cache_dependency = "module_matrix_payload"))
  )

  expect_false(.session_snapshots_need_plot_pool(module_snapshots))
})

test_that("full session snapshots with plots demand a pool or active-dataset alias", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(1, 2))
  data_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  active_id <- .build_plot_data_cache_id(data_mod = data_mod, data_def = data_def)
  module_snapshots <- list(
    datawizard = list(module_state = list(plot_data_cache_ref = "ignored_datawizard_ref")),
    volcano = list(module_state = list(had_plot = TRUE)),
    pca = list(module_state = list(plot_data_cache_ref = active_id)),
    dotplot = list(module_state = list(plot_cache_ref_by_title = list(Main = active_id)))
  )

  expect_true(.session_snapshots_need_plot_pool(module_snapshots))

  pool <- list()
  pool[[active_id]] <- list(data_mod = data_mod, data_def = data_def)
  aliased <- .alias_active_dataset_cache_entries(
    plot_data_cache_pool = pool,
    active_dataset_id = active_id,
    rv_snapshot = list(data_mod = data_mod, data_def = data_def)
  )
  expect_true(.is_canonical_active_dataset_cache_alias(aliased[[active_id]]))
})

test_that("legacy full snapshots with missing refs still receive canonical restore cache", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(1, 2))
  data_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  legacy <- list(
    version = "2.0.0",
    save_level = SESSION_SAVE_LEVEL_FULL,
    data_mod = data_mod,
    data_def = data_def,
    module_snapshots = list(
      datawizard = list(module_state = list()),
      volcano = list(module_state = list(had_plot = TRUE))
    )
  )

  upgraded <- upgrade_session_snapshot_to_current_schema(legacy)
  expect_true(length(upgraded$plot_data_cache_pool) > 0L)
  st <- upgraded$module_snapshots$volcano$module_state
  expect_identical(st$restore_cache_dependency, "shared_plot_data_cache_pool")
  expect_true(is.character(st$plot_data_cache_ref) && nzchar(st$plot_data_cache_ref))
  expect_identical(st$restore_plot_data_cache$data_mod, data_mod)
  expect_identical(st$restore_plot_data_cache$data_def, data_def)
})

test_that("current inline v3 snapshots do not become legacy when the cache pool is empty", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(1, 2))
  data_def <- data.frame(
    Column = names(data_mod),
    Content = c("Identifier", "Abundance")
  )
  module_snapshots <- list(
    datawizard = list(module_state = list()),
    volcano = list(module_state = list(had_plot = TRUE))
  )
  envelope <- list(
    miraprot_session = TRUE,
    version = MIRAPROT_SESSION_SCHEMA_VERSION,
    save_level = SESSION_SAVE_LEVEL_FULL,
    manifest = list(
      module_ids = names(module_snapshots),
      failed_modules = character(),
      data_dims = as.integer(dim(data_mod)),
      transport = "inline_rds",
      transport_preset = "balanced"
    ),
    payload_inline = list(
      rv_snapshot = list(data_mod = data_mod, data_def = data_def),
      module_snapshots = module_snapshots
    ),
    plot_data_cache_pool = NULL
  )

  upgraded <- upgrade_session_snapshot_to_current_schema(unwrap_snapshot(envelope))

  expect_identical(upgraded$compatibility_upgrade$mode, "native_current_schema")
  expect_identical(upgraded$plot_data_cache_pool, list())
  volcano_state <- upgraded$module_snapshots$volcano$module_state
  expect_null(volcano_state$restore_plot_data_cache)
  expect_null(volcano_state$restore_cache_dependency)
  expect_null(volcano_state$plot_data_cache_ref)
})

test_that("legacy plot bundle writer is disabled for new saves", {
  fake_plot <- structure(list(data = data.frame(x = 1)), class = "ggplot")
  snapshot <- .prepare_plot_for_snapshot(fake_plot)

  expect_identical(snapshot, fake_plot)
  expect_false(inherits(snapshot, "miraprot_plot_bundle"))
})

test_that("runtime scanner flags legacy miraprot_plot_bundle payloads", {
  bundled <- structure(list(kind = "ggplot"), class = "miraprot_plot_bundle")
  hits <- .find_export_runtime_objects(list(module_snapshots = list(dotplot = list(module_state = list(plot = bundled)))))

  expect_true(any(grepl(":miraprot_plot_bundle$", hits)))
})

test_that("singleton plot-data cache pool fallback hydrates restore cache", {
  data_mod <- data.frame(Protein = "P1", Abundance = 10)
  data_def <- data.frame(Sample = "S1", Condition = "A")
  pool <- list(singleton_cache = list(data_mod = data_mod, data_def = data_def))
  state <- list(
    restore_cache_dependency = "shared_plot_data_cache_pool",
    plot_data_cache_ref = "missing_cache_ref"
  )

  expect_error(
    restored <- .resolve_plot_data_cache_for_module(state, pool),
    NA
  )
  expect_identical(restored$restore_plot_data_cache, pool$singleton_cache)
  expect_identical(restored$plot_data_cache_ref, "singleton_cache")
  expect_identical(restored$restore_cache_resolution_mode, "singleton_pool_fallback")
  expect_false(isTRUE(restored$restore_cache_degraded))
})

test_that("envelope assertion validates active-dataset aliases without expanding the serialized pool", {
  data_mod <- data.frame(Protein = c("P1", "P2"), Abundance = c(10, 20))
  data_def <- data.frame(Sample = c("S1", "S2"), Condition = c("A", "B"))
  rv_snapshot <- list(
    data_mod = data_mod,
    data_def = data_def,
    data_mod_revision_id = 7L,
    data_def_revision_id = 11L
  )
  ref <- .build_plot_data_cache_id(7L, 11L, data_mod, data_def)
  pair <- list(data_mod = data_mod, data_def = data_def)
  pool <- setNames(list(pair), ref)
  aliased_pool <- .alias_active_dataset_cache_entries(pool, ref, rv_snapshot)
  contract <- .plot_data_cache_ref_contract(7L, 11L, data_mod, data_def, ref)
  state <- c(list(
    restore_cache_dependency = "shared_plot_data_cache_pool",
    plot_cache_ref_by_title = list(Abundances = ref)
  ), contract)
  snapshots <- list(abundances = list(module_state = state))

  expect_silent(.session_assert_finalized_snapshot_cache_invariants(
    snapshots, aliased_pool, rv_snapshot
  ))
  expect_true(.is_canonical_active_dataset_cache_alias(aliased_pool[[ref]]))
  expect_false(any(c("data_mod", "data_def") %in% names(aliased_pool[[ref]])))

  fingerprint_mismatch <- rv_snapshot
  fingerprint_mismatch$data_mod$Abundance <-
    fingerprint_mismatch$data_mod$Abundance + 1
  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      snapshots, aliased_pool, fingerprint_mismatch
    ),
    "missing from the pool or malformed"
  )

  revision_mismatch <- rv_snapshot
  revision_mismatch$data_mod_revision_id <- 8L
  expect_error(
    .session_assert_finalized_snapshot_cache_invariants(
      snapshots, aliased_pool, revision_mismatch
    ),
    "missing from the pool or malformed"
  )

  historical_mod <- transform(data_mod, Abundance = Abundance + 100)
  historical_ref <- .build_plot_data_cache_id(3L, 5L, historical_mod, data_def)
  historical_pair <- list(data_mod = historical_mod, data_def = data_def)
  historical_contract <- .plot_data_cache_ref_contract(
    3L, 5L, historical_mod, data_def, historical_ref
  )
  historical_state <- c(list(
    restore_cache_dependency = "shared_plot_data_cache_pool"
  ), historical_contract)
  historical_pool <- setNames(list(historical_pair), historical_ref)
  expect_silent(.session_assert_finalized_snapshot_cache_invariants(
    list(abundances = list(module_state = historical_state)),
    historical_pool,
    rv_snapshot
  ))
  expect_identical(historical_pool[[historical_ref]], historical_pair)
})
