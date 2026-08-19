library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")

canonical_snapshot <- function(...) {
  modifyList(list(
    miraprot_session = TRUE,
    version = MIRAPROT_SESSION_SCHEMA_VERSION,
    rv_snapshot = list(data_mod = data.frame(Protein = "P1"))
  ), list(...))
}

test_that("session-save error markers are rejected before canonical validation", {
  marker <- list(
    miraprot_session = TRUE,
    session_file_type = MIRAPROT_SESSION_ERROR_MARKER,
    version = MIRAPROT_SESSION_SCHEMA_VERSION,
    error = "save failed\nfor a sanitized reason"
  )

  result <- validate_session_snapshot(marker)

  expect_false(result$valid)
  expect_identical(
    result$message,
    paste0(
      "This download contains a session-save error marker: ",
      "save failed for a sanitized reason"
    )
  )
  expect_false(grepl("data_mod", result$message, fixed = TRUE))
})

test_that("an error-marker RDS reports its stored save error", {
  marker <- list(
    miraprot_session = TRUE,
    session_file_type = MIRAPROT_SESSION_ERROR_MARKER,
    version = MIRAPROT_SESSION_SCHEMA_VERSION,
    error = "snapshot serialization failed after cache normalization"
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(marker, path)

  result <- validate_session_snapshot(readRDS(path))

  expect_false(result$valid)
  expect_match(result$message, marker$error, fixed = TRUE)
  expect_false(grepl("data_mod", result$message, fixed = TRUE))
})

test_that("legacy envelopes are not classified by an error field alone", {
  legacy <- canonical_snapshot(error = "legacy diagnostic text")

  result <- validate_session_snapshot(legacy)

  expect_true(result$valid)
})
