helpers_dir <- file.path("R", "session_save_restore")
if (!dir.exists(helpers_dir)) helpers_dir <- file.path("..", "..", helpers_dir)
volcano_file <- file.path("modules", "Volcano", "volcano_observers_selection_restore.R")
if (!file.exists(volcano_file)) volcano_file <- file.path("..", "..", volcano_file)

volcano_restore_test_env <- new.env(parent = globalenv())
volcano_restore_test_env$`%||%` <- function(x, y) if (is.null(x)) y else x
volcano_restore_test_env$debug_log <- function(...) invisible(NULL)
sys.source(file.path(helpers_dir, "session_save_restore_callbacks.R"),
           envir = volcano_restore_test_env)
sys.source(volcano_file, envir = volcano_restore_test_env)

test_that("Volcano finalizer callback errors degrade restore without escaping", {
  skip_if_not_installed("shiny")
  resolutions <- list()
  resolver <- function(id, outcome, error = NULL) {
    resolutions[[length(resolutions) + 1L]] <<- list(
      id = id, outcome = outcome, error = error
    )
    TRUE
  }

  result <- NULL
  expect_silent({
    result <- volcano_restore_test_env$.run_volcano_restore_finalizer(
      generation = 12L,
      callback = function() stop("injected Volcano finalizer failure"),
      job_id = "restore-12-volcano",
      resolve_job = resolver,
      current_generation = function() 12L
    )
  })

  expect_false(result)
  expect_length(resolutions, 1L)
  expect_identical(resolutions[[1L]]$id, "restore-12-volcano")
  expect_identical(resolutions[[1L]]$outcome, "failure")
  expect_match(resolutions[[1L]]$error, "injected Volcano finalizer failure", fixed = TRUE)
})
