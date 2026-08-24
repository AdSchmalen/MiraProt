venn_restore_file <- file.path("modules", "venn", "venn_observers_export_restore.R")
if (!file.exists(venn_restore_file)) venn_restore_file <- file.path("..", "..", venn_restore_file)

make_venn_no_plot_fixture <- function(old_poll = FALSE) {
  skip_if_not_installed("shiny")
  handlers <- registrations <- resolutions <- updates <- reports <- list()
  value <- function(initial = NULL) {
    current <- initial
    function(next_value) {
      if (missing(next_value)) return(current)
      current <<- next_value
      invisible(current)
    }
  }

  env <- new.env(parent = globalenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  env$isolate <- function(x) x
  env$downloadHandler <- function(...) structure(list(), class = "mock_download")
  env$observe <- function(...) invisible(NULL)
  env$observeEvent <- function(eventExpr, handlerExpr, ...) {
    if (grepl("session_restore_trigger", paste(deparse(substitute(eventExpr)), collapse = ""))) {
      handlers[["restore"]] <<- list(expr = substitute(handlerExpr), env = parent.frame())
    }
    invisible(NULL)
  }
  env$reactiveVal <- value
  env$debug_log <- function(...) invisible(NULL)
  env$requireNamespace <- function(...) FALSE
  env$updateTextInput <- function(session, id, value) updates[[id]] <<- value
  env$updateTextAreaInput <- function(session, id, value) updates[[id]] <<- value
  env$updateCheckboxInput <- function(session, id, value) updates[[id]] <<- value
  env$updateNumericInput <- function(session, id, value) updates[[id]] <<- value
  env$updateSelectInput <- function(session, id, selected) updates[[id]] <<- selected
  env$updateSelectizeInput <- function(session, id, selected, ...) updates[[id]] <<- selected
  env$output <- new.env(parent = emptyenv())
  env$input <- list()
  env$cleanup_mgr <- NULL
  env$rv <- list(session_restore_trigger = 1L, session_restore_generation = 12L)
  env$state <- new.env(parent = emptyenv())
  env$state$list_count_Venn <- value(2L)
  env$state$list_data_Venn <- list(
    names = list("Restored A", "Restored B"),
    lists = list("A1\nA2", "B1\nB2"), colors = list(NULL, NULL)
  )
  env$state$pending_ui_inputs <- value(list(diagramType_Venn = "Euler"))
  env$state$had_plot_on_save <- value(FALSE)
  env$state$last_restore_report <- value(NULL)
  env$restore_poll_active <- value(isTRUE(old_poll))
  env$restore_poll_attempt <- value(if (old_poll) 4L else 0L)
  env$restore_poll_captured <- value(if (old_poll) list(diagramType_Venn = "Venn") else NULL)
  env$restore_poll_phase <- value(if (old_poll) "choices" else "base")
  env$restore_phase_attempt <- value(if (old_poll) 2L else 0L)
  env$restore_poll_generation <- value(if (old_poll) 11L else NA_integer_)
  env$restore_poll_job <- value(if (old_poll) "venn-old-11" else NULL)
  env$restore_poll_job_settled <- value(!old_poll)
  env$settle_restore_poll <- function(outcome, error = NULL) {
    if (isTRUE(env$restore_poll_job_settled())) return(invisible(FALSE))
    env$restore_poll_job_settled(TRUE)
    env$session$userData$resolve_restore_job(env$restore_poll_job(), outcome, error)
  }
  env$finalize_restore_report <- function(status, reason = NULL, ...) {
    reports[[length(reports) + 1L]] <<- list(status = status, reason = reason)
  }
  env$session <- list(
    userData = list(
      register_restore_job = function(...) {
        registrations[[length(registrations) + 1L]] <<- list(...)
        "unexpected-job"
      },
      resolve_restore_job = function(job, outcome, error = NULL) {
        stale <- !identical(env$restore_poll_generation(), env$rv$session_restore_generation)
        resolutions[[length(resolutions) + 1L]] <<- list(
          job = job, outcome = outcome, error = error, stale = stale
        )
        !stale
      }
    ),
    onFlushed = function(...) stop("no-plot restore must not arm a flush callback")
  )

  source_env <- new.env(parent = env)
  sys.source(venn_restore_file, envir = source_env)
  source_env$register_venn_export_restore_observers(env)
  list(
    run = function() eval(handlers$restore$expr, handlers$restore$env), env = env,
    registrations = function() registrations, resolutions = function() resolutions,
    updates = function() updates, reports = function() reports
  )
}

test_that("Venn no-plot restore replays list and UI state without a restore job", {
  fixture <- make_venn_no_plot_fixture()
  fixture$run()
  expect_length(fixture$registrations(), 0L)
  expect_equal(
    fixture$updates()[c("name1", "name2")],
    list(name1 = "Restored A", name2 = "Restored B")
  )
  expect_equal(fixture$updates()$diagramType_Venn, "Euler")
  expect_null(fixture$env$state$pending_ui_inputs())
  expect_equal(fixture$reports()[[1L]], list(
    status = "restore_skipped", reason = "had_plot_on_save_false"
  ))
})

test_that("Venn no-plot generation settles and clears an older armed poll", {
  fixture <- make_venn_no_plot_fixture(old_poll = TRUE)
  fixture$run()
  expect_length(fixture$registrations(), 0L)
  expect_equal(fixture$resolutions(), list(list(
    job = "venn-old-11", outcome = "skipped", error = "STALE_GENERATION", stale = TRUE
  )))
  expect_true(fixture$env$restore_poll_job_settled())
  expect_null(fixture$env$restore_poll_job())
  expect_true(is.na(fixture$env$restore_poll_generation()))
  expect_false(fixture$env$restore_poll_active())
  expect_null(fixture$env$restore_poll_captured())
  expect_equal(fixture$reports()[[1L]]$status, "restore_skipped")
})
