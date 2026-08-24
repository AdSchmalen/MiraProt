helpers_dir <- file.path("R", "session_save_restore")
if (!dir.exists(helpers_dir)) helpers_dir <- file.path("..", "..", helpers_dir)
pca_restore_test_env <- new.env(parent = globalenv())
pca_restore_test_env$`%||%` <- function(x, y) if (is.null(x)) y else x
pca_restore_test_env$debug_log <- function(...) invisible(NULL)
sys.source(file.path(helpers_dir, "session_save_restore_core_helpers.R"), envir = pca_restore_test_env)
sys.source(file.path(helpers_dir, "session_save_restore_callbacks.R"), envir = pca_restore_test_env)

pca_state_file <- file.path("modules", "PCA", "pca_module_state.R")
if (!file.exists(pca_state_file)) pca_state_file <- file.path("..", "..", pca_state_file)
sys.source(pca_state_file, envir = pca_restore_test_env)

make_pca_registry <- function() {
  generation <- 7L
  scheduled <- list()
  registry <- pca_restore_test_env$.create_restore_job_registry(
    function() generation,
    function(callback, delay) scheduled[[length(scheduled) + 1L]] <<- list(callback = callback, delay = delay),
    function(...) invisible(NULL)
  )
  registry$start_generation(generation)
  list(registry = registry, scheduled = function() scheduled,
       generation = function(value) { if (!missing(value)) generation <<- value; generation })
}

test_that("PCA cache hit waits for render completion settlement", {
  fixture <- make_pca_registry()
  job <- fixture$registry$register_restore_job("PCA", "render settlement", "render", 30)
  fixture$registry$seal_generation(7L)
  expect_length(fixture$registry$outstanding_restore_jobs(), 1L) # cache lookup is not completion
  expect_true(fixture$registry$resolve_restore_job(job, "success"))
  expect_length(fixture$registry$outstanding_restore_jobs(), 0L)
})

test_that("PCA cache hit render expectation settles from bounded timeout", {
  fixture <- make_pca_registry()
  fixture$registry$register_restore_job("PCA", "render settlement", "render", 3)
  fixture$registry$seal_generation(7L)
  expect_equal(fixture$scheduled()[[1L]]$delay, 3)
  fixture$scheduled()[[1L]]$callback()
  expect_length(fixture$registry$outstanding_restore_jobs(), 0L)
})

test_that("PCA finalizer rejects a stale session generation", {
  skip_if_not_installed("shiny")
  fixture <- make_pca_registry()
  job <- fixture$registry$register_restore_job("PCA", "restore finalizer", "finalizer", 15)
  fixture$generation(8L)
  mutated <- FALSE
  expect_false(pca_restore_test_env$.run_session_restore_callback(
    "PCA", "restore finalizer", 7L, "finalizer", function() mutated <<- TRUE,
    job_metadata = list(job_id = job, resolve_job = fixture$registry$resolve_restore_job,
                        current_generation = fixture$generation)
  ))
  expect_false(mutated)
})

test_that("PCA finalizer errors are contained and reported as failure", {
  skip_if_not_installed("shiny")
  fixture <- make_pca_registry()
  job <- fixture$registry$register_restore_job("PCA", "restore finalizer", "finalizer", 15)
  expect_false(pca_restore_test_env$.run_session_restore_callback(
    "PCA", "restore finalizer", 7L, "finalizer", function() stop("finalizer boom"),
    job_metadata = list(job_id = job, resolve_job = fixture$registry$resolve_restore_job,
                        current_generation = fixture$generation)
  ))
  fixture$registry$seal_generation(7L)
  expect_length(fixture$registry$outstanding_restore_jobs(), 0L)
})

test_that("PCA saved-plot intent is canonical and legacy-compatible", {
  saved_plot_intent <- pca_restore_test_env$pca_saved_plot_intent

  expect_false(saved_plot_intent(list(had_plot = FALSE, plots_ready = TRUE)))
  expect_true(saved_plot_intent(list(had_plot = TRUE, plots_ready = FALSE)))
  expect_true(saved_plot_intent(list(plots_ready = TRUE)))
  expect_false(saved_plot_intent(list(coordinates = matrix(1, nrow = 1L))))
})
