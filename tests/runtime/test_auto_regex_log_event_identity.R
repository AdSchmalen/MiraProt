library(testthat)

test_that("central records keep run/event identity and serialize complete lines", {
  env <- new.env(parent = globalenv())
  sys.source("R/bootstrap.R", envir = env)
  recorder <- get(".miraprot_log_record", envir = globalenv())
  join <- get(".miraprot_log_records_to_text", envir = globalenv())

  recorder(0L, "AUTO REGEX", "Auto RegEx run 1 started", "1", "auto-regex:1:start")
  recorder(0L, "AUTO REGEX", "Auto RegEx run 1 completed | 2 content, 1 condition, 1 ratio",
           "1", "auto-regex:1:terminal")
  recorder(0L, "AUTO REGEX", "Auto RegEx run 2 started", "2", "auto-regex:2:start")
  recorder(0L, "AUTO REGEX", "Auto RegEx run 2 completed | 3 content, 2 condition, 1 ratio",
           "2", "auto-regex:2:terminal")

  rows <- get(".miraprot_log_buffers", envir = globalenv())[["0"]]
  expect_identical(rows$run_id, c("1", "1", "2", "2"))
  expect_identical(length(unique(rows$event_id)), 4L)
  expect_identical(sum(grepl(" started$", rows$message)), 2L)
  expect_identical(sum(grepl(" completed \\|", rows$message)), 2L)
  text <- join(rows$line)
  expect_true(grepl("\\n$", text))
  expect_false(grepl("ratio\\[ AUTO REGEX", text, fixed = FALSE))
  expect_identical(length(strsplit(sub("\\n$", "", text), "\\n", fixed = FALSE)[[1L]]), 4L)
})

test_that("handler source exposes reinitialization guard and stable terminal IDs", {
  source_text <- paste(readLines(
    "modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R",
    warn = FALSE), collapse = "\n")
  expect_match(source_text, "auto_regex_handler_tokens", fixed = TRUE)
  expect_match(source_text, "auto-regex:%s:start", fixed = TRUE)
  expect_match(source_text, "auto-regex:%s:terminal", fixed = TRUE)
})
