# Regression coverage for filename-based readRDS compression detection and
# restore-upload ownership cleanup.  Extract the private helper from the setup
# function so this test exercises its actual implementation without a Shiny
# session.

library(testthat)

source("R/session_save_restore/session_save_restore_orchestration.R")

.find_read_restore_snapshot_assignment <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-") ) &&
      identical(expr[[2L]], as.name(".read_restore_snapshot"))) {
    return(expr)
  }
  if (!is.recursive(expr)) return(NULL)
  for (part in as.list(expr)) {
    found <- .find_read_restore_snapshot_assignment(part)
    if (!is.null(found)) return(found)
  }
  NULL
}

.make_restore_snapshot_reader <- function() {
  assignment <- .find_read_restore_snapshot_assignment(body(setup_session_save_restore))
  stopifnot(!is.null(assignment))
  helper_env <- new.env(parent = globalenv())
  helper_env$active_restore_upload_paths <- new.env(parent = emptyenv())
  helper_env$debug_log <- function(...) invisible(NULL)
  eval(assignment, envir = helper_env)
  list(read = helper_env$.read_restore_snapshot,
       claims = helper_env$active_restore_upload_paths)
}

.compression_fixture <- list(
  marker = "MiraProt restore compression fixture",
  values = c(1L, 3L, 5L, 8L),
  nested = list(enabled = TRUE)
)

test_that("read restore snapshot detects every application RDS compression mode", {
  reader <- .make_restore_snapshot_reader()
  # TRUE currently means gzip in base R; gzip and xz are the concrete modes
  # selected by .session_rds_compress_for_transport_preset().
  for (compression in list(TRUE, "gzip", "xz")) {
    path <- tempfile(fileext = ".rds")
    saveRDS(.compression_fixture, path, compress = compression)

    expect_identical(reader$read(path, owned_temporary_path = FALSE),
                     .compression_fixture,
                     info = paste("compression:", compression))
    expect_true(file.exists(path), info = "non-owned fixture must not be deleted")
    unlink(path)
  }
})

test_that("owned temporary restore is deleted only after reading completes", {
  reader <- .make_restore_snapshot_reader()
  path <- tempfile(fileext = ".rds")
  saveRDS(.compression_fixture, path, compress = "gzip")
  assign(path, TRUE, envir = reader$claims)

  restored <- reader$read(path, owned_temporary_path = TRUE)

  expect_identical(restored, .compression_fixture)
  expect_false(file.exists(path))
  expect_false(exists(path, envir = reader$claims, inherits = FALSE))
})

test_that("invalid RDS reports the read error while still performing cleanup", {
  reader <- .make_restore_snapshot_reader()
  path <- tempfile(fileext = ".rds")
  writeLines("not an RDS file", path, useBytes = TRUE)
  assign(path, TRUE, envir = reader$claims)

  read_error <- tryCatch({
    reader$read(path, owned_temporary_path = TRUE)
    NULL
  }, error = identity)

  expect_s3_class(read_error, "error")
  expect_match(conditionMessage(read_error), "unknown input format|read error|error reading")
  expect_false(file.exists(path))
  expect_false(exists(path, envir = reader$claims, inherits = FALSE))
})
