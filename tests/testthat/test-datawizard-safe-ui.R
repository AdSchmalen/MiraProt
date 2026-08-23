safe_ui_file <- file.path(
  "modules", "Data Wizard", "core", "datawizard_core_safe_ui.R"
)
if (!file.exists(safe_ui_file)) {
  safe_ui_file <- file.path("..", "..", safe_ui_file)
}

test_that("execute_when_ready has an explicit interactive-only contract", {
  test_env <- new.env(parent = globalenv())
  sys.source(safe_ui_file, envir = test_env)

  session <- new.env(parent = emptyenv())
  session$onFlushed <- function(callback, once) {
    session$callback <- callback
    session$once <- once
    invisible(NULL)
  }
  ui_system <- test_env$create_safe_ui_system(session, "test")

  expect_error(
    ui_system$execute_when_ready(identity, "config"),
    "interactive-only",
    fixed = TRUE
  )
  expect_error(
    ui_system$execute_when_ready(identity, "config", caller_intent = "restore"),
    "interactive-only",
    fixed = TRUE
  )

  observed <- NULL
  ui_system$execute_when_ready(
    function(config) observed <<- config,
    "config",
    caller_intent = "interactive"
  )
  expect_null(observed)
  expect_true(session$once)
  session$callback()
  expect_identical(observed, "config")
})

test_that("execute_when_ready contains callback errors after the flush", {
  test_env <- new.env(parent = globalenv())
  sys.source(safe_ui_file, envir = test_env)

  session <- new.env(parent = emptyenv())
  session$onFlushed <- function(callback, once) {
    session$callback <- callback
    invisible(NULL)
  }
  ui_system <- test_env$create_safe_ui_system(session, "test", debug_level = 2)
  ui_system$execute_when_ready(
    function(config) stop("deliberate failure"),
    list(),
    caller_intent = "interactive"
  )

  expect_output(session$callback(),
    "Interactive post-flush update failed: deliberate failure",
    fixed = TRUE
  )
})
