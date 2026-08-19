library(testthat)
library(shiny)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_reactive_state.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")

test_that("namespace matching and source modes stay isolated", {
  expect_identical(NS("wizard")("auto_regex"), "wizard-auto_regex")
  state <- auto_regex_create_state()
  state$reset_source_specific_state("excel")
  state$reset_workbook_state(list(path = "book.xlsx"), "Sheet1")
  state$reset_worksheet_state("Sheet1", data.frame(Column = "x"))
  expect_identical(isolate(state$source()), "excel")
  state$reset_source_specific_state("current_metadata")
  expect_identical(isolate(state$source()), "current_metadata")
  expect_null(isolate(state$workbook()))
  expect_null(isolate(state$worksheet_data()))
})

test_that("file and worksheet replacement clear stale mappings and results", {
  state <- auto_regex_create_state()
  seed_completed_result <- function(source) {
    state$mapping(c(Column = "Column", Content = "Content"))
    state$validation(data.frame(Check = "ok"))
    run <- state$begin_run(source, "fixture")
    expect_true(state$complete_run(run$run_id, run$source_fingerprint,
      list(table = data.frame()), diagnostics = list(old = data.frame(x = 1)),
      payload = list(old = TRUE)))
    state$payload(list(old = TRUE))
  }

  seed_completed_result("excel")
  state$reset_workbook_state(list(path = "replacement.xlsx"), "New sheet")
  expect_null(state$mapping())
  expect_null(state$validation())
  expect_null(state$candidate_rules())
  expect_null(state$payload())
  expect_null(state$diagnostics())

  seed_completed_result("excel")
  state$reset_worksheet_state("Another sheet", data.frame(Column = "new"))
  expect_null(state$mapping())
  expect_null(state$validation())
  expect_null(state$candidate_rules())
  expect_null(state$payload())
  expect_null(state$diagnostics())
})

test_that("state transitions guard stale processing and retain diagnostics", {
  state <- auto_regex_create_state()
  run1 <- state$begin_run("current_metadata", "one")
  state$mark_stale()
  expect_false(state$complete_run(run1$run_id, run1$source_fingerprint, list(table = data.frame())))
  run2 <- state$begin_run("excel", "two")
  expect_true(state$complete_run(run2$run_id, run2$source_fingerprint,
    list(table = data.frame()), list(content = data.frame(ok = TRUE)),
    warnings = "review", timings = c(total = 1)))
  expect_identical(state$run_status(), "complete")
  expect_identical(state$warnings(), "review")
  expect_identical(state$diagnostics()$source_mode, "excel")
  expect_false(state$complete_transfer(run1$run_id, run1$source_fingerprint))
  expect_true(state$complete_transfer(run2$run_id, run2$source_fingerprint, list(ok = TRUE)))
})

test_that("effective source signatures are bounded and value-sensitive", {
  frame <- data.frame(Column = c("A", "B"), Content = c("Raw Abundance", NA))
  mapping <- c(Column = "Column", Content = "Content")
  first <- auto_regex_source_descriptor("excel", frame, frame,
    list(name = "book.xlsx", size = 10), "Sheet1", mapping, list(revision = 1))
  same <- auto_regex_source_descriptor("excel",
    data.frame(Column = factor(c("A", "B")), Content = c("Raw Abundance", NA)),
    frame, list(name = "book.xlsx", size = 10), "Sheet1", mapping,
    list(revision = 1))
  changed_mapping <- auto_regex_source_descriptor("excel", frame, frame,
    list(name = "book.xlsx", size = 10), "Sheet1", rev(mapping), list(revision = 1))
  changed_value <- auto_regex_source_descriptor("excel",
    transform(frame, Content = c("Found in Sample", NA)), frame,
    list(name = "book.xlsx", size = 10), "Sheet1", mapping, list(revision = 1))

  expect_identical(first$signature, same$signature)
  expect_false(identical(first$signature, changed_mapping$signature))
  expect_false(identical(first$signature, changed_value$signature))
  expect_lt(nchar(first$signature), 40L)
  expect_true(is.data.frame(first$metadata))
})

test_that("effective invalidation reserves stale for completed results", {
  state <- auto_regex_create_state()
  state$validation(data.frame(ok = TRUE))
  auto_regex_invalidate_effective_source(state)
  expect_identical(state$run_status(), "ready")

  run <- state$begin_run("current_metadata", "source")
  expect_true(state$complete_run(run$run_id, run$source_fingerprint,
    list(table = data.frame()), payload = list(ok = TRUE)))
  expect_true(state$complete_transfer(run$run_id, run$source_fingerprint))
  auto_regex_invalidate_effective_source(state)
  expect_identical(state$run_status(), "stale")
  auto_regex_invalidate_effective_source(state)
  expect_identical(state$run_status(), "stale")
})

test_that("failure, notification terminal paths, and cleanup are singular", {
  state <- auto_regex_create_state()
  run <- state$begin_run("current_metadata", "bad")
  expect_true(state$fail_run(run$run_id, run$source_fingerprint, "broken",
    warnings = "warning"))
  expect_identical(state$run_status(), "failed")
  expect_identical(length(state$errors()), 1L)
  expect_identical(length(state$warnings()), 1L)
  state$cleanup()
  expect_identical(state$run_status(), "cleaned")
  expect_false(state$initialized())
  expect_null(state$diagnostics())
  expect_null(state$payload())
  expect_silent(state$cleanup())
  expect_identical(state$run_status(), "cleaned")
})

test_that("imperative state transitions work inside and outside reactive contexts", {
  exercise_transitions <- function(state) {
    state$reset_source_specific_state("current_metadata")
    run <- state$begin_run("current_metadata", "source")
    expect_true(auto_regex_run_is_current(state, run$run_id,
      run$source_fingerprint))
    expect_true(state$complete_run(run$run_id, run$source_fingerprint,
      list(table = data.frame()), payload = list(ok = TRUE)))
    expect_true(state$complete_transfer(run$run_id, run$source_fingerprint))
    state$mark_stale()
    failed_run <- state$begin_run("current_metadata", "source")
    expect_true(state$fail_run(failed_run$run_id,
      failed_run$source_fingerprint, "expected"))
    state$cleanup()
    state$cleanup()
  }

  expect_silent(exercise_transitions(auto_regex_create_state()))
  expect_silent(shiny::isolate(exercise_transitions(auto_regex_create_state())))

  observed <- shiny::reactiveVal(FALSE)
  observer <- shiny::observe({
    exercise_transitions(auto_regex_create_state())
    observed(TRUE)
  })
  on.exit(observer$destroy(), add = TRUE)
  shiny::flushReact()
  expect_true(shiny::isolate(observed()))
})
