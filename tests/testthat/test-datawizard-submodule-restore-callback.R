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

test_that("imperative readiness keeps absence separate from reactive-context conditions", {
  test_env <- new.env(parent = globalenv())
  test_env$messages <- character(0)
  test_env$debug_log <- function(message, level) {
    test_env$messages <- c(test_env$messages, message)
  }
  sys.source(core_session_file, envir = test_env)

  # These are the restore owners whose readiness gates are imperative. Calling
  # each predicate from an ordinary function deliberately provides no reactive
  # consumer, matching an onFlushed/later restore callback.
  owners <- c("Data Wizard", "GSEA", "SampleIDs", "Venn", "PCA", "Volcano", "Dotplot")
  for (owner in owners) {
    unavailable <- (function() {
      test_env$.evaluate_restore_readiness(owner, function() FALSE)
    })()
    expect_false(unavailable$ready, info = owner)
    expect_true(unavailable$retry, info = owner)
    expect_null(unavailable$code, info = owner)

    reactive_source <- shiny::reactiveVal(TRUE)
    violation <- (function() {
      test_env$.evaluate_restore_readiness(owner, function() reactive_source())
    })()
    expect_false(violation$ready, info = owner)
    expect_false(violation$retry, info = owner)
    expect_identical(violation$code, "REACTIVE_CONTEXT_VIOLATION", info = owner)
  }
})

test_that("reactive-context readiness violations settle their named restore job", {
  test_env <- new.env(parent = globalenv())
  test_env$debug_log <- function(message, level) NULL
  sys.source(core_session_file, envir = test_env)
  settlements <- list()
  source <- shiny::reactiveVal(TRUE)

  result <- (function() {
    test_env$.evaluate_restore_readiness(
      "PCA", function() source(),
      list(job_id = "restore-4-2", resolve_job = function(id, outcome, error) {
        settlements[[length(settlements) + 1L]] <<- list(id, outcome, error)
        TRUE
      })
    )
  })()

  expect_false(result$retry)
  expect_identical(settlements[[1L]][[1L]], "restore-4-2")
  expect_identical(settlements[[1L]][[2L]], "failure")
  expect_match(settlements[[1L]][[3L]], "REACTIVE_CONTEXT_VIOLATION", fixed = TRUE)
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
