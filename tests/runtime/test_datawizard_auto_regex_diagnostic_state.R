library(testthat)
library(shiny)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_reactive_state.R")

test_that("diagnostics retain current run provenance and timings", {
  state <- auto_regex_create_state()
  run <- state$begin_run("current_metadata", "source-hash-1")
  tables <- list(content = data.frame(TP = 1L), condition = data.frame(), ratio = data.frame())
  expect_true(state$complete_run(run$run_id, run$source_fingerprint,
    list(table = data.frame()), tables, timings = c(total = 12)))

  record <- state$diagnostics()
  expect_identical(record$run_id, run$run_id)
  expect_identical(record$source_mode, "current_metadata")
  expect_identical(record$source_fingerprint, "source-hash-1")
  expect_identical(record$timings, c(total = 12))
  expect_true(is.numeric(record$elapsed_ms) && record$elapsed_ms >= 0)
  expect_identical(record$tables, tables)
  expect_s3_class(record$started_at, "POSIXct")
  expect_s3_class(record$completed_at, "POSIXct")

  state$mark_stale()
  expect_null(state$diagnostics())
  expect_null(state$completed_run_id())
})
