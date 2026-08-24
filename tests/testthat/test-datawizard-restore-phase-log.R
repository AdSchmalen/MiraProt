test_that("restore phase module count describes active snapshot membership", {
  root <- if (dir.exists("R/session_save_restore")) "." else "../.."
  registration_file <- file.path(
    root,
    "R/session_save_restore/session_save_restore_module_registration.R"
  )
  source_text <- paste(readLines(registration_file, warn = FALSE), collapse = "\n")
  log_helpers <- sub(
    "(?s).*?(non_datawizard_module_counts <- function\\(\\) \\{.*?)(?=          set_restore_phase <- function)",
    "\\1",
    source_text,
    perl = TRUE
  )

  expect_match(
    log_helpers,
    "snapshot_ids <- tryCatch\\(\\s*session_registry\\$current_restore_snapshot_ids\\(\\)",
    perl = TRUE
  )
  expect_match(
    log_helpers,
    '"snapshot_non_datawizard_module_count=", module_counts\\$snapshot',
    perl = TRUE
  )
  expect_false(grepl("completed", log_helpers, fixed = TRUE))
  expect_false(grepl("restored_non_datawizard_module_count", log_helpers, fixed = TRUE))
})
