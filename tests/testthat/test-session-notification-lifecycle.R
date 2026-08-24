session_loader_file <- file.path("R", "session_save_restore.R")
if (!file.exists(session_loader_file)) {
  session_loader_file <- file.path("..", "..", session_loader_file)
}

notification_test_owner <- function() {
  host <- new.env(parent = globalenv())
  sys.source(session_loader_file, envir = host)
  environment(host$setup_session_save_restore)
}

test_that("restore feedback is generation-safe and uses one explicit notification", {
  owner <- notification_test_owner()
  calls <- list()
  logs <- character()
  generation <- 1L
  session <- new.env(parent = emptyenv())
  notify <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  lifecycle <- owner$.make_session_notification_lifecycle(
    session = session,
    current_generation = function(candidate) identical(candidate, generation),
    log_fn = function(message, level) logs <<- c(logs, message),
    notify_fn = notify
  )

  expect_true(lifecycle$restore(
    "Restoring MiraProt session...", phase = "start", generation = 1L
  ))
  expect_true(lifecycle$restore(
    "Session state restored. 11 module(s) loaded. Plots and outputs are rebuilding.",
    phase = "state_applied", generation = 1L
  ))
  # No renderer, plot output, or tab input is involved in state-applied feedback.
  expect_length(calls, 2L)
  expect_match(calls[[2L]][[1L]], "outputs are rebuilding", fixed = TRUE)
  expect_true(all(vapply(calls, function(x) identical(x$id, lifecycle$restore_id), logical(1L))))
  expect_true(all(vapply(calls, function(x) identical(x$session, session), logical(1L))))

  expect_true(lifecycle$restore(
    "Session restoration complete. 11 module(s) restored.",
    phase = "settled", generation = 1L, state = "SETTLED"
  ))
  expect_identical(calls[[3L]]$id, calls[[2L]]$id)
  expect_false(lifecycle$restore(
    "duplicate", phase = "settled", generation = 1L, state = "SETTLED"
  ))

  generation <- 2L
  expect_true(lifecycle$restore(
    "Restoring MiraProt session...", phase = "start", generation = 2L
  ))
  expect_false(lifecycle$restore(
    "stale warning", type = "warning", phase = "settled",
    generation = 1L, state = "DEGRADED"
  ))
  expect_false(any(vapply(calls, function(x) identical(x[[1L]], "stale warning"), logical(1L))))
  expect_true(any(grepl("phase=state_applied", logs, fixed = TRUE)))
})

test_that("restore warning and failure replace the same lifecycle id", {
  owner <- notification_test_owner()
  calls <- list()
  session <- new.env(parent = emptyenv())
  lifecycle <- owner$.make_session_notification_lifecycle(
    session, function(candidate) identical(candidate, 7L),
    log_fn = function(...) invisible(NULL),
    notify_fn = function(...) calls[[length(calls) + 1L]] <<- list(...)
  )

  lifecycle$restore("state restored", phase = "state_applied", generation = 7L)
  lifecycle$restore("Session restored with warnings: PCA timeout.", type = "warning",
                    phase = "settled", generation = 7L, state = "DEGRADED")
  expect_identical(calls[[1L]]$id, calls[[2L]]$id)
  expect_identical(calls[[2L]]$type, "warning")

  lifecycle$restore("Session restoration failed: plot error.", type = "error",
                    phase = "settled", generation = 7L, state = "FAILED")
  expect_identical(calls[[2L]]$id, calls[[3L]]$id)
  expect_identical(calls[[3L]]$type, "error")
})

test_that("save results use the stable id and explicit session", {
  owner <- notification_test_owner()
  calls <- list()
  session <- new.env(parent = emptyenv())
  lifecycle <- owner$.make_session_notification_lifecycle(
    session, function(candidate) TRUE,
    log_fn = function(...) invisible(NULL),
    notify_fn = function(...) calls[[length(calls) + 1L]] <<- list(...)
  )

  cases <- list(
    success = list(result = list(fallback_used = FALSE), failed = character(), type = "message"),
    dropped = list(result = list(fallback_used = FALSE), failed = "PCA", type = "warning"),
    fallback = list(result = list(fallback_used = TRUE, modules_removed_entirely = TRUE,
                                  error = "serialization failed"),
                    failed = character(), type = "warning")
  )
  for (case in cases) {
    owner$.notify_session_save_result(
      case$result, 11L, case$failed, "Full Session State", session,
      notify = lifecycle$save
    )
    expect_identical(calls[[length(calls)]]$id, lifecycle$save_id)
    expect_identical(calls[[length(calls)]]$session, session)
    expect_identical(calls[[length(calls)]]$type, case$type)
  }

  lifecycle$save("Saving MiraProt session...", phase = "start")
  lifecycle$save("Failed to save session", type = "error", phase = "result", state = "FAILED")
  expect_identical(calls[[length(calls) - 1L]]$id, calls[[length(calls)]]$id)
  expect_identical(calls[[length(calls)]]$type, "error")
})
