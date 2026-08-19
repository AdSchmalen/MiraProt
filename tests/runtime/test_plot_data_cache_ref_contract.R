library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")

.cache_contract_pair <- function() {
  list(
    data_mod = data.frame(Protein = c("P1", "P2"), S1 = c(1, 2)),
    data_def = data.frame(Column = c("Protein", "S1"), Content = c("Identifier", "Abundance"))
  )
}

.expect_contract_resolves_same_pair <- function(contract, pair) {
  pool <- setNames(list(list(data_mod = pair$data_mod, data_def = pair$data_def)),
                   contract$plot_data_cache_ref)
  resolved <- .safe_cache_pool_get(pool, contract$plot_data_cache_ref)

  expect_identical(resolved$data_mod, pair$data_mod)
  expect_identical(resolved$data_def, pair$data_def)
}

test_that("plot-data cache contracts normalize supplied revisions", {
  pair <- .cache_contract_pair()

  numeric_contract <- .plot_data_cache_ref_contract(
    7L, 11L, pair$data_mod, pair$data_def
  )
  character_contract <- .plot_data_cache_ref_contract(
    "7", "11", pair$data_mod, pair$data_def
  )

  expect_identical(numeric_contract$data_mod_revision_id, 7L)
  expect_identical(numeric_contract$data_def_revision_id, 11L)
  expect_identical(character_contract$data_mod_revision_id, 7L)
  expect_identical(character_contract$data_def_revision_id, 11L)
  expect_identical(character_contract$plot_data_cache_ref,
                   numeric_contract$plot_data_cache_ref)
  .expect_contract_resolves_same_pair(numeric_contract, pair)
})

test_that("plot-data cache contracts normalize missing, invalid, and NA revisions to zero", {
  pair <- .cache_contract_pair()
  revisions <- list(
    missing = list(NULL, NULL),
    empty = list(integer(), character()),
    invalid = list("not-a-revision", list()),
    na = list(NA_integer_, NA_character_)
  )

  for (values in revisions) {
    contract <- .plot_data_cache_ref_contract(
      values[[1L]], values[[2L]], pair$data_mod, pair$data_def
    )
    expect_identical(contract$data_mod_revision_id, 0L)
    expect_identical(contract$data_def_revision_id, 0L)
    expect_match(contract$plot_data_cache_ref, "^0\\|0\\|", perl = TRUE)
    .expect_contract_resolves_same_pair(contract, pair)
  }
})

test_that("plot-data cache contracts preserve supplied refs and generate absent refs", {
  pair <- .cache_contract_pair()
  supplied <- .plot_data_cache_ref_contract(
    3L, 5L, pair$data_mod, pair$data_def, plot_data_cache_ref = "saved-ref"
  )
  generated <- .plot_data_cache_ref_contract(
    3L, 5L, pair$data_mod, pair$data_def
  )

  expect_identical(supplied$plot_data_cache_ref, "saved-ref")
  expect_identical(generated$plot_data_cache_ref, .build_plot_data_cache_id(
    3L, 5L, pair$data_mod, pair$data_def
  ))
  .expect_contract_resolves_same_pair(supplied, pair)
  .expect_contract_resolves_same_pair(generated, pair)
})

test_that("plot-data cache contracts never inherit revision locals", {
  pair <- .cache_contract_pair()
  assign("mod_rev", 901L, envir = .GlobalEnv)
  assign("def_rev", 902L, envir = .GlobalEnv)
  on.exit(rm("mod_rev", "def_rev", envir = .GlobalEnv), add = TRUE)

  contract <- .plot_data_cache_ref_contract(
    data_mod = pair$data_mod,
    data_def = pair$data_def
  )

  expect_identical(contract$data_mod_revision_id, 0L)
  expect_identical(contract$data_def_revision_id, 0L)
  expect_match(contract$plot_data_cache_ref, "^0\\|0\\|", perl = TRUE)
  .expect_contract_resolves_same_pair(contract, pair)
})
