restore_helpers_dir <- file.path("R", "session_save_restore")
if (!dir.exists(restore_helpers_dir)) {
  restore_helpers_dir <- file.path("..", "..", restore_helpers_dir)
}

restore_acceptance_env <- new.env(parent = globalenv())
restore_acceptance_env$`%||%` <- function(x, y) if (is.null(x)) y else x
restore_acceptance_env$debug_log <- function(...) invisible(NULL)
sys.source(file.path(restore_helpers_dir, "session_save_restore_core_helpers.R"),
           envir = restore_acceptance_env)
sys.source(file.path(restore_helpers_dir, "session_save_restore_callbacks.R"),
           envir = restore_acceptance_env)

make_restore_transaction <- function(generation = 1L) {
  active_generation <- generation
  callbacks <- list()
  timers <- list()
  reports <- list()
  registry <- restore_acceptance_env$.create_restore_job_registry(
    function() active_generation,
    function(callback, delay) {
      timers[[length(timers) + 1L]] <<- list(callback = callback, delay = delay)
    },
    function(report) reports[[length(reports) + 1L]] <<- report
  )
  registry$start_generation(generation)
  registry$set_phase(generation, "REPLAYING")
  list(
    registry = registry,
    defer = function(callback) callbacks[[length(callbacks) + 1L]] <<- callback,
    flush = function() {
      pending <- callbacks
      callbacks <<- list()
      lapply(pending, function(callback) callback())
      invisible(length(pending))
    },
    pending = function() length(callbacks),
    timers = function() timers,
    reports = function() reports,
    generation = function(value) {
      if (!missing(value)) active_generation <<- value
      active_generation
    }
  )
}

test_that("restore-only imperative helpers run as plain functions", {
  skip_if_not_installed("shiny")
  value <- shiny::reactiveVal(0L)

  # Deliberately invoke the restore boundary from an ordinary function: there
  # is no observer, reactive, render function, or other reactive consumer.
  result <- (function() {
    restore_acceptance_env$.run_session_restore_callback(
      "Data Wizard", "plain imperative replay", 1L, "replay",
      function() value(value() + 1L),
      job_metadata = list(current_generation = function() 1L)
    )
  })()

  expect_true(result)
  expect_identical(shiny::isolate(value()), 1L)
})

test_that("multi-flush restore settles all required visualization owners", {
  skip_if_not_installed("shiny")
  transaction <- make_restore_transaction(21L)
  owners <- c("Data Wizard", "Abundances", "SampleIDs", "PCA", "Volcano",
              "Dotplot", "Heatmap")
  applied <- character()

  for (owner in owners) {
    job_id <- transaction$registry$register_restore_job(
      owner, "multi-flush replay", "replay", timeout = 30
    )
    local({
      captured_owner <- owner
      captured_job <- job_id
      transaction$defer(function() {
        restore_acceptance_env$.run_session_restore_callback(
          captured_owner, "multi-flush replay", 21L, "replay",
          function() applied <<- c(applied, captured_owner),
          job_metadata = list(
            job_id = captured_job,
            resolve_job = transaction$registry$resolve_restore_job,
            current_generation = transaction$generation
          )
        )
      })
    })
    # Each owner is scheduled from a distinct simulated Shiny flush.
    expect_identical(transaction$flush(), 1L)
  }

  transaction$registry$seal_generation(21L)
  expect_setequal(applied, owners)
  expect_length(transaction$reports(), 1L)
  expect_identical(transaction$reports()[[1L]]$state, "SETTLED")
  expect_setequal(vapply(transaction$reports()[[1L]]$jobs, `[[`, character(1), "owner"),
                  owners)
})

test_that("a named deferred callback error is contained and fails settlement", {
  skip_if_not_installed("shiny")
  transaction <- make_restore_transaction(22L)
  job_id <- transaction$registry$register_restore_job(
    "Heatmap", "injected deferred failure", "render", timeout = 30
  )
  process_alive <- TRUE
  transaction$defer(function() {
    restore_acceptance_env$.run_session_restore_callback(
      "Heatmap", "injected deferred failure", 22L, "render",
      function() stop("intentional deferred callback error"),
      job_metadata = list(
        job_id = job_id,
        resolve_job = transaction$registry$resolve_restore_job,
        current_generation = transaction$generation
      )
    )
  })

  expect_silent(transaction$flush())
  # Execution after the callback demonstrates that the R process was not
  # unwound or terminated by the injected condition.
  process_alive <- process_alive && identical(1L + 1L, 2L)
  transaction$registry$seal_generation(22L)
  expect_true(process_alive)
  expect_length(transaction$reports(), 1L)
  expect_true(transaction$reports()[[1L]]$state %in% c("DEGRADED", "FAILED"))
  expect_match(transaction$reports()[[1L]]$errors[[1L]],
               "intentional deferred callback error", fixed = TRUE)
})

test_that("generation N callbacks cannot mutate or release generation N guards", {
  skip_if_not_installed("shiny")
  transaction <- make_restore_transaction(30L)
  old_job <- transaction$registry$register_restore_job(
    "Volcano", "late generation callback", "finalizer", timeout = 30
  )
  mutated <- FALSE
  guard_released <- FALSE
  transaction$defer(function() {
    accepted <- restore_acceptance_env$.run_session_restore_callback(
      "Volcano", "late generation callback", 30L, "finalizer",
      function() mutated <<- TRUE,
      job_metadata = list(
        job_id = old_job,
        resolve_job = function(id, outcome, error = NULL) {
          released <- transaction$registry$resolve_restore_job(id, outcome, error)
          if (isTRUE(released)) guard_released <<- TRUE
          released
        },
        current_generation = transaction$generation
      )
    )
    expect_false(accepted)
  })

  # Generation N+1 begins before the queued generation N callback is run.
  transaction$generation(31L)
  transaction$registry$start_generation(31L)
  expect_silent(transaction$flush())
  expect_false(mutated)
  expect_false(guard_released)
  expect_length(transaction$registry$outstanding_restore_jobs(30L), 1L)
})

test_that("success is withheld until every required job resolves", {
  transaction <- make_restore_transaction(40L)
  jobs <- vapply(c("Data Wizard", "PCA", "Heatmap"), function(owner) {
    transaction$registry$register_restore_job(owner, "required restore", "replay", 30)
  }, character(1))
  transaction$registry$seal_generation(40L)

  expect_true(transaction$registry$resolve_restore_job(jobs[[1L]], "success"))
  expect_true(transaction$registry$resolve_restore_job(jobs[[2L]], "success"))
  expect_empty(transaction$reports())
  expect_length(transaction$registry$outstanding_restore_jobs(40L), 1L)

  expect_true(transaction$registry$resolve_restore_job(jobs[[3L]], "success"))
  expect_length(transaction$reports(), 1L)
  expect_identical(transaction$reports()[[1L]]$state, "SETTLED")
  expect_length(transaction$reports()[[1L]]$jobs, 3L)
})
