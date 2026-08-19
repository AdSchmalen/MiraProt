library(testthat)

gsea_env <- new.env(parent = globalenv())
gsea_env$gsea_debug_log <- function(...) invisible(NULL)
sys.source("modules/GSEA/GSEA_module_parallelization.R", envir = gsea_env)

ranking_fixture <- c(G1 = 3, G2 = 2, G3 = 1, G4 = -1, G5 = -2, G6 = -3)
term2gene_fixture <- data.frame(
  term = rep(paste0("set", 1:3), each = 2),
  gene = paste0("G", 1:6),
  stringsAsFactors = FALSE
)

install_fake_parallel_services <- function(env, detected = 12L) {
  state <- new.env(parent = emptyenv())
  state$next_id <- 0L
  state$stops <- integer()

  env$gsea_biocparallel_available <- function() TRUE
  env$gsea_detect_cores <- function() detected
  env$gsea_bp_serial <- function() structure(list(id = "serial", workers = 1L), class = "SerialParam")
  env$gsea_bp_snow <- function(workers, ...) {
    state$next_id <- state$next_id + 1L
    structure(list(id = paste0("snow-", state$next_id), workers = workers), class = "SnowParam")
  }
  env$gsea_bp_start <- identity
  env$gsea_bp_is_up <- function(bp) TRUE
  env$gsea_bp_workers <- function(bp) bp$workers
  env$gsea_bp_stop <- function(bp) {
    state$stops[bp$id] <- ifelse(is.na(state$stops[bp$id]), 1L, state$stops[bp$id] + 1L)
    invisible(bp)
  }
  state
}

install_fake_gsea_services <- function(env, execute) {
  gmt <- tempfile(fileext = ".gmt")
  writeLines("fixture", gmt)
  env$gsea_read_term2gene <- function(path) term2gene_fixture
  env$gsea_execute <- execute
  gmt
}

metadata <- function(result) attr(result, "gsea_execution_metadata")
successful_result <- function(...) data.frame(ID = "set1", pvalue = 0.01)

test_that("a sequential job does not pin worker allocation for a later large job", {
  state <- install_fake_parallel_services(gsea_env)
  job_number <- 0L
  gsea_env$gsea_is_small_job <- function(...) {
    job_number <<- job_number + 1L
    job_number == 1L
  }
  gmt <- install_fake_gsea_services(gsea_env, successful_result)

  small <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)
  large <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)

  expect_identical(attr(small, "gsea_workers_used"), 1L)
  expect_identical(metadata(small)$execution_mode, "sequential")
  expect_identical(attr(large, "gsea_workers_used"), 4L)
  expect_identical(metadata(large)$execution_mode, "parallel")
  expect_identical(unname(state$stops[c("serial", "snow-1")]), c(1L, 1L))
})

test_that("consecutive large jobs use fresh backends stopped exactly once", {
  state <- install_fake_parallel_services(gsea_env)
  gsea_env$gsea_is_small_job <- function(...) FALSE
  observed_ids <- character()
  gmt <- install_fake_gsea_services(gsea_env, function(..., BPPARAM) {
    observed_ids <<- c(observed_ids, BPPARAM$id)
    successful_result()
  })

  first <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)
  second <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)

  expect_identical(observed_ids, c("snow-1", "snow-2"))
  expect_identical(unname(state$stops[observed_ids]), c(1L, 1L))
  expect_identical(c(attr(first, "gsea_workers_used"), attr(second, "gsea_workers_used")), c(4L, 4L))
})

test_that("parallel failure reports successful sequential fallback accurately", {
  state <- install_fake_parallel_services(gsea_env)
  gsea_env$gsea_is_small_job <- function(...) FALSE
  calls <- 0L
  gmt <- install_fake_gsea_services(gsea_env, function(..., BPPARAM) {
    calls <<- calls + 1L
    if (inherits(BPPARAM, "SnowParam")) stop("socket worker failed")
    successful_result()
  })

  result <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)

  expect_identical(calls, 2L)
  expect_identical(attr(result, "gsea_workers_used"), 1L)
  expect_identical(metadata(result)$effective_workers, 1L)
  expect_identical(metadata(result)$execution_mode, "sequential")
  expect_true(metadata(result)$fallback_occurred)
  expect_identical(unname(state$stops["snow-1"]), 1L)
})

test_that("a failed run cleans up and cannot change the next allocation", {
  state <- install_fake_parallel_services(gsea_env)
  gsea_env$gsea_is_small_job <- function(...) FALSE
  fail <- TRUE
  gmt <- install_fake_gsea_services(gsea_env, function(...) {
    if (fail) stop("invalid enrichment input")
    successful_result()
  })

  expect_null(gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10))
  fail <- FALSE
  result <- gsea_env$run_gsea_analysis(ranking_fixture, gmt, num_permutations = 10)

  expect_identical(attr(result, "gsea_workers_used"), 4L)
  expect_identical(metadata(result)$requested_workers, 4L)
  expect_identical(unname(state$stops[c("snow-1", "snow-2")]), c(1L, 1L))
})

test_that("safe core limits and effective worker metadata remain distinct", {
  state <- install_fake_parallel_services(gsea_env, detected = 24L)
  gsea_env$gsea_is_small_job <- function(...) FALSE
  gsea_env$gsea_bp_workers <- function(bp) 1L
  gmt <- install_fake_gsea_services(gsea_env, successful_result)

  expect_identical(gsea_env$gsea_safe_cores(requested_cores = 2L), 2L)
  bp_info <- gsea_env$gsea_get_bpparam(requested_cores = 2L)
  expect_identical(bp_info$requested_workers, 2L)
  expect_identical(bp_info$effective_workers, 1L)
  gsea_env$gsea_bp_stop(bp_info$bp)

  result <- gsea_env$run_gsea_analysis(
    ranking_fixture, gmt, num_permutations = 10, requested_cores = 2L
  )
  expect_identical(metadata(result)$requested_workers, 2L)
  expect_identical(metadata(result)$effective_workers, 1L)
  expect_identical(unname(state$stops[c("snow-1", "snow-2")]), c(1L, 1L))
})
