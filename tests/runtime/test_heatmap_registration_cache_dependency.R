library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_module_registration.R")

test_that("Heatmap registration preserves valid module dependency declarations", {
  for (dependency in SESSION_RESTORE_CACHE_DEPENDENCIES) {
    state <- list(
      restore_cache_dependency = dependency,
      heatmap_expression_matrix = matrix(1, nrow = 1L),
      had_heatmap = TRUE
    )
    expect_identical(
      .heatmap_registration_restore_cache_dependency(state),
      dependency
    )
  }
})

test_that("Heatmap registration derives dependency only for invalid declarations", {
  matrix_state <- list(
    restore_cache_dependency = "invalid_dependency",
    matrix_payload = list(expression_matrix = matrix(1:4, nrow = 2L))
  )
  expect_identical(
    .heatmap_registration_restore_cache_dependency(matrix_state),
    "module_matrix_payload"
  )

  expect_identical(
    .heatmap_registration_restore_cache_dependency(list(had_heatmap = TRUE)),
    "shared_plot_data_cache_pool"
  )
  expect_identical(
    .heatmap_registration_restore_cache_dependency(list(plot_request = list(type = "expression"))),
    "shared_plot_data_cache_pool"
  )

  expect_identical(
    .heatmap_registration_restore_cache_dependency(list(had_heatmap = FALSE)),
    "none"
  )
  expect_identical(
    .heatmap_registration_restore_cache_dependency(list()),
    "none"
  )
})
