core_helpers_file <- file.path("R", "session_save_restore", "session_save_restore_core_helpers.R")
if (!file.exists(core_helpers_file)) core_helpers_file <- file.path("..", "..", core_helpers_file)

test_that("restore jobs resolve exactly once and are generation scoped", {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$logs <- list()
  env$debug_log <- function(message, level) {
    env$logs[[length(env$logs) + 1L]] <- list(message = message, level = level)
  }
  sys.source(core_helpers_file, envir = env)
  generation <- 1L
  timers <- list()
  reports <- list()
  registry <- env$.create_restore_job_registry(
    function() generation,
    function(callback, delay) timers[[length(timers) + 1L]] <<- callback,
    function(report) reports[[length(reports) + 1L]] <<- report
  )

  registry$start_generation(1L)
  registry$set_phase(1L, "REPLAYING")
  first <- registry$register_restore_job("plots", "render wait", "render", 5)
  expect_length(registry$outstanding_restore_jobs(1L), 1L)
  expect_true(registry$resolve_restore_job(first, "completed"))
  expect_false(registry$resolve_restore_job(first, "completed"))
  registry$seal_generation(1L)
  expect_identical(reports[[1L]]$state, "SETTLED")

  generation <- 2L
  registry$start_generation(2L)
  second <- registry$register_restore_job("ui", "replay", "replay", 5)
  expect_false(registry$resolve_restore_job(first, "completed"))
  expect_length(registry$outstanding_restore_jobs(2L), 1L)
  registry$seal_generation(2L)
  timers[[2L]]()
  expect_identical(reports[[2L]]$state, "DEGRADED")
  expect_identical(reports[[2L]]$timeouts, 1L)
  expect_false(registry$resolve_restore_job(second, "completed"))
  expect_true(all(vapply(env$logs, `[[`, integer(1), "level") == 0L))
  messages <- vapply(env$logs, `[[`, character(1), "message")
  required_context <- paste0("generation=2 phase=replay owner=ui reason=replay job_id=", second)
  expect_true(any(grepl(paste0("[RestoreRegistry:timeout] ", required_context),
                        messages, fixed = TRUE)))
  expect_true(any(grepl("[RestoreRegistry:final] generation=2 phase=SETTLEMENT owner=registry reason=restore settlement job_id=all status=DEGRADED",
                        messages, fixed = TRUE)))
  expect_true(any(grepl(paste0("[RestoreRegistry:outstanding] ", required_context,
                               " outcome=timeout"), messages, fixed = TRUE)))
})
