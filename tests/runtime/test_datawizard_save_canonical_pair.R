library(testthat)
library(shiny)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_orchestration.R")
source("modules/Data Wizard/datawizard_utils.R")
source("modules/Data Wizard/datawizard_core.R")

modEnv <- environment()
source("R/session_save_restore/session_save_restore_module_registration.R")

.capture_datawizard_save <- function(data_mod, data_def) {
  captured <- new.env(parent = emptyenv())
  registry <- list(
    register = function(module_id, save_fn, restore_fn, ...) {
      if (identical(module_id, "datawizard")) captured$save_fn <- save_fn
      invisible(NULL)
    },
    registered_ids = function() "datawizard",
    current_restore_snapshot_ids = function() "datawizard"
  )
  rv <- reactiveValues(data_mod = data_mod, data_def = data_def)
  outputs <- reactiveValues(datawizard_out = list())
  register_module_session_participants(registry, outputs, rv = rv)
  stopifnot(is.function(captured$save_fn))
  captured$save_fn
}

.canonical_pair <- function(prefix, offset = 0) {
  data <- data.frame(
    setNames(list(c("P1", "P2"), c(1, 2) + offset),
             c(paste0(prefix, "_Protein"), paste0(prefix, "_S1"))),
    check.names = FALSE
  )
  list(
    data_mod = data,
    data_def = data.frame(
      Column = names(data),
      Content = c("Identifier", "Abundance"),
      check.names = FALSE
    )
  )
}

test_that("Data Wizard serializes canonical A unchanged and retains historical B in plot cache", {
  canonical_a <- .canonical_pair("A")
  historical_b <- .canonical_pair("B", 100)
  save_fn <- .capture_datawizard_save(canonical_a$data_mod, canonical_a$data_def)

  datawizard_snapshot <- isolate(save_fn(SESSION_SAVE_LEVEL_ANALYSIS))
  historical_state <- c(
    list(
      restore_cache_dependency = "shared_plot_data_cache_pool",
      plot_reconstruction_pending = TRUE,
      plot_data_cache_payload = historical_b
    ),
    .plot_data_cache_ref_contract(11L, 12L, historical_b$data_mod, historical_b$data_def)
  )
  live_rv <- c(canonical_a, list(data_mod_revision_id = 21L, data_def_revision_id = 22L))
  bundle <- .build_save_time_plot_data_cache_bundle(
    list(
      datawizard = datawizard_snapshot,
      pca = list(module_state = historical_state)
    ),
    live_rv,
    SESSION_SAVE_LEVEL_FULL
  )
  round_tripped <- unserialize(serialize(list(
    module_snapshots = bundle$module_snapshots,
    plot_data_cache_pool = bundle$plot_data_cache_pool
  ), NULL))

  expect_identical(round_tripped$module_snapshots$datawizard$data_mod, canonical_a$data_mod)
  expect_identical(round_tripped$module_snapshots$datawizard$data_def, canonical_a$data_def)
  expect_true(any(vapply(round_tripped$plot_data_cache_pool, function(entry) {
    identical(entry$data_mod, historical_b$data_mod) &&
      identical(entry$data_def, historical_b$data_def)
  }, logical(1L))))
  expect_false(any(vapply(round_tripped$plot_data_cache_pool, function(entry) {
    identical(entry$data_mod, canonical_a$data_mod)
  }, logical(1L))))
})

test_that("Data Wizard rejects an inconsistent canonical pair before serialization", {
  canonical <- .canonical_pair("A")
  inconsistent_metadata <- canonical$data_def[1L, , drop = FALSE]
  save_fn <- .capture_datawizard_save(canonical$data_mod, inconsistent_metadata)
  serialization_started <- FALSE

  expect_error({
    snapshot <- isolate(save_fn(SESSION_SAVE_LEVEL_ANALYSIS))
    serialization_started <- TRUE
    serialize(snapshot, NULL)
  }, paste0(
    "Data Wizard save refused: canonical data and metadata are misaligned; ",
    "data dimensions=2 x 2, metadata dimensions=1 x 2, ",
    "ordered Column values match names\\(data_mod\\)=FALSE"
  ))
  expect_false(serialization_started)
})
