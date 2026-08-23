core_session_file <- file.path(
  "modules", "Data Wizard", "core", "datawizard_core_submodule_session.R"
)
if (!file.exists(core_session_file)) {
  core_session_file <- file.path("..", "..", core_session_file)
}

test_that("deferred submodule restore callbacks isolate reactive reads", {
  test_env <- new.env(parent = globalenv())
  test_env$messages <- character(0)
  test_env$debug_log <- function(message, level) {
    test_env$messages <- c(test_env$messages, message)
  }
  sys.source(core_session_file, envir = test_env)

  restored <- shiny::reactiveVal("pending")
  callback <- function() {
    restored(paste0(restored(), ":applied"))
  }

  expect_true(test_env$.run_submodule_restore_callback(
    callback, "AutoAssign", "restore_trigger:nested"
  ))
  expect_identical(shiny::isolate(restored()), "pending:applied")
  expect_true(any(grepl(
    "[RestoreCallback:done] module=AutoAssign reason=restore_trigger:nested",
    test_env$messages, fixed = TRUE
  )))
  expect_false(any(grepl(
    "Operation not allowed without an active reactive context",
    test_env$messages, fixed = TRUE
  )))
})

test_that("deferred submodule restore callback errors are named and nonfatal", {
  test_env <- new.env(parent = globalenv())
  test_env$messages <- character(0)
  test_env$debug_log <- function(message, level) {
    test_env$messages <- c(test_env$messages, message)
  }
  sys.source(core_session_file, envir = test_env)

  expect_false(test_env$.run_submodule_restore_callback(
    function() stop("deliberate UI replay failure"),
    "Ratios", "post_stabilization_retry"
  ))
  expect_true(any(grepl(
    paste0("[RestoreCallback:error] module=Ratios ",
           "reason=post_stabilization_retry error=deliberate UI replay failure"),
    test_env$messages, fixed = TRUE
  )))
})

test_that("AutoAssign restore transaction isolates canonical identity reads", {
  autoassign_file <- file.path("modules", "Data Wizard", "datawizard_auto_assign.R")
  if (!file.exists(autoassign_file)) autoassign_file <- file.path("..", "..", autoassign_file)
  source_text <- paste(readLines(autoassign_file, warn = FALSE), collapse = "\n")

  callback_start <- regexpr("apply_extra = function\\(extra\\) \\{", source_text)
  callback_text <- substr(source_text, callback_start, callback_start + 3000L)
  expect_match(callback_text, "shiny::isolate\\(\\{")
  expect_match(callback_text, "frames <- list\\(content=rv_table_rules_autoassign_dw\\(\\)")
  expect_match(callback_text, "selected_content_rule")
  expect_match(callback_text, "selected_condition_rule")
  expect_match(callback_text, "selected_ratio_rule")
})
