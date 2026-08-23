registration_file <- file.path(
  "R", "session_save_restore", "session_save_restore_module_registration.R"
)
if (!file.exists(registration_file)) {
  registration_file <- file.path("..", "..", registration_file)
}

test_that("deferred Assign Rules restore isolates reactive reads", {
  test_env <- new.env(parent = globalenv())
  test_env$debug_messages <- character(0)
  test_env$debug_log <- function(message, level) {
    test_env$debug_messages <- c(test_env$debug_messages, message)
  }
  sys.source(registration_file, envir = test_env)

  condition_state <- shiny::reactiveVal("before")
  restore_fn <- function(payload) {
    previous <- condition_state()
    condition_state(paste(previous, payload$value, sep = ":"))
  }
  deferred_dispatch <- function() {
    test_env$.restore_assign_rules_payload(
      list(restore_condition_state = restore_fn),
      list(value = "restored"),
      "before downstream submodule UI"
    )
  }

  expect_identical(deferred_dispatch(), "success")
  expect_identical(shiny::isolate(condition_state()), "before:restored")
  expect_false(any(grepl(
    "Operation not allowed without an active reactive context",
    test_env$debug_messages,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "restored Assign Rules condition state before downstream submodule UI",
    test_env$debug_messages,
    fixed = TRUE
  )))
})

test_that("Assign Rules restore errors are nonfatal and not successes", {
  test_env <- new.env(parent = globalenv())
  test_env$debug_messages <- character(0)
  test_env$debug_log <- function(message, level) {
    test_env$debug_messages <- c(test_env$debug_messages, message)
  }
  sys.source(registration_file, envir = test_env)

  result <- test_env$.restore_assign_rules_payload(
    list(restore_condition_state = function(payload) stop("deliberate restore failure")),
    list(value = "ignored"),
    "before downstream submodule UI"
  )

  expect_identical(result, "error")
  expect_true(any(grepl(
    "Assign Rules condition-state restore failed: deliberate restore failure",
    test_env$debug_messages,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "restored Assign Rules condition state",
    test_env$debug_messages,
    fixed = TRUE
  )))
})

test_that("Assign Rules restore reports an unavailable bridge distinctly", {
  test_env <- new.env(parent = globalenv())
  test_env$debug_log <- function(message, level) NULL
  sys.source(registration_file, envir = test_env)

  expect_identical(
    test_env$.restore_assign_rules_payload(list(), list(value = "state"), "suffix"),
    "unavailable"
  )
})
