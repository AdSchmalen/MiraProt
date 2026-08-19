# Regression coverage for preserving the session-restore runtime callback across
# an rv reset and for the Data Wizard's session$userData compatibility fallback.

library(testthat)
library(shiny)

source("R/session_save_restore/session_save_restore_orchestration.R")

.find_assignment <- function(expr, target) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name(target))) {
    return(expr)
  }
  if (!is.recursive(expr)) return(NULL)
  for (part in as.list(expr)) {
    found <- .find_assignment(part, target)
    if (!is.null(found)) return(found)
  }
  NULL
}

.make_reset_helper <- function(rv) {
  helper_env <- new.env(parent = globalenv())
  helper_env$rv <- rv
  helper_env$debug_log <- function(...) invisible(NULL)
  for (target in c(
    "persistent_runtime_bindings",
    "restore_runtime_state_fields",
    "restore_runtime_fields",
    "reset_rv_for_session_restore"
  )) {
    assignment <- .find_assignment(body(setup_session_save_restore), target)
    stopifnot(!is.null(assignment))
    eval(assignment, envir = helper_env)
  }
  helper_env$reset_rv_for_session_restore
}

.make_datawizard_handoff <- function(rv, fallback = NULL) {
  loader_env <- new.env(parent = globalenv())
  sys.source("modules/Data Wizard/datawizard_file_loader.R", envir = loader_env)
  assignment <- .find_assignment(
    body(loader_env$modFileLoaderServer),
    "handoff_session_restore_upload"
  )
  stopifnot(!is.null(assignment))

  helper_env <- new.env(parent = globalenv())
  helper_env$rv <- rv
  helper_env$session <- list(userData = new.env(parent = emptyenv()))
  helper_env$session$userData$handle_session_restore_upload <- fallback
  helper_env$debug_log <- function(...) invisible(NULL)
  helper_env$notifications <- character()
  helper_env$showNotification <- function(message, ...) {
    helper_env$notifications <- c(helper_env$notifications, message)
    invisible(NULL)
  }
  eval(assignment, envir = helper_env)
  helper_env
}

test_that("rv restore callback survives reset and handles a second Data Wizard upload", {
  calls <- list()
  restore_handler <- function(file_info, source_label) {
    calls[[length(calls) + 1L]] <<- list(file_info, source_label)
  }
  rv <- reactiveValues(
    restore_session_from_file = restore_handler,
    stale_before_restore = TRUE
  )
  handoff <- .make_datawizard_handoff(rv)

  isolate(handoff$handoff_session_restore_upload(
    list(name = "first.rds", datapath = tempfile()), "first"
  ))
  expect_identical(.make_reset_helper(rv)(list(snapshot_value = 1L)), character())
  expect_true(isolate(is.function(rv$restore_session_from_file)))
  isolate(handoff$handoff_session_restore_upload(
    list(name = "second.rds", datapath = tempfile()), "second"
  ))

  expect_length(calls, 2L)
  expect_identical(vapply(calls, function(x) x[[1L]]$name, character(1)),
                   c("first.rds", "second.rds"))
  expect_true(all(vapply(calls, function(x) identical(x[[2L]], "Data Wizard file loader"), logical(1))))
  expect_false(any(grepl("Session restore is unavailable", handoff$notifications, fixed = TRUE)))
})

test_that("Data Wizard uses the session userData restore fallback", {
  calls <- list()
  fallback <- function(file_info, source_label) {
    calls[[length(calls) + 1L]] <<- list(file_info, source_label)
  }
  handoff <- .make_datawizard_handoff(reactiveValues(), fallback)

  isolate(handoff$handoff_session_restore_upload(
    list(name = "fallback.rds", datapath = tempfile()), "fallback"
  ))

  expect_length(calls, 1L)
  expect_identical(calls[[1L]][[1L]]$name, "fallback.rds")
  expect_identical(calls[[1L]][[2L]], "Data Wizard file loader")
  expect_false(any(grepl("Session restore is unavailable", handoff$notifications, fixed = TRUE)))
})

test_that("restore runtime allowlist is available to snapshot application", {
  setup_body <- paste(deparse(body(setup_session_save_restore)), collapse = "\n")
  definition <- regexpr("restore_runtime_fields <- unique", setup_body, fixed = TRUE)[[1L]]
  application <- regexpr("field_name %in% restore_runtime_fields", setup_body, fixed = TRUE)[[1L]]

  expect_gt(definition, 0L)
  expect_gt(application, definition)
})

.find_call <- function(expr, target) {
  if (is.call(expr) && identical(expr[[1L]], as.name(target))) return(expr)
  if (!is.recursive(expr)) return(NULL)
  for (part in as.list(expr)) {
    found <- .find_call(part, target)
    if (!is.null(found)) return(found)
  }
  NULL
}

test_that("Data Wizard replay publication cannot replace canonical data with plot cache data", {
  source("R/session_save_restore/session_save_restore_core_helpers.R", local = TRUE)
  integration_env <- new.env(parent = globalenv())
  sys.source("modules/Data Wizard/datawizard_integration.R", envir = integration_env)

  canonical_a <- as.data.frame(setNames(
    replicate(127L, c(1, 2), simplify = FALSE), paste0("A", seq_len(127L))))
  metadata_a <- data.frame(Column = names(canonical_a), Content = "Abundance")
  historical_b <- as.data.frame(setNames(
    replicate(46L, c(10, 20), simplify = FALSE), paste0("B", seq_len(46L))))
  metadata_b <- data.frame(Column = names(historical_b), Content = "Abundance")

  pool <- list(cache_b = list(data_mod = historical_b, data_def = metadata_b))
  reconstructed_plot_state <- .resolve_plot_data_cache_for_module(
    list(plot_data_cache_ref = "cache_b"), pool)
  reconstructed_plot <- reconstructed_plot_state$restore_plot_data_cache
  expect_identical(reconstructed_plot$data_mod, historical_b)
  expect_identical(reconstructed_plot$data_def, metadata_b)

  registry_state <- new.env(parent = emptyenv())
  registry_state$primary_working <- canonical_a
  rv <- reactiveValues(
    data_mod = canonical_a,
    data_def = metadata_a,
    session_restore_generation = 9L,
    session_restore_trigger = 9L,
    session_restore_phase = "datawizard_ui",
    session_restoring = TRUE
  )
  logs <- character()
  primary_data_state <- list(set_modified_data = function(data, operation, metadata = NULL) {
    rv$data_mod <- data
    if (!is.null(metadata)) rv$data_def <- metadata
    registry_state$primary_working <- data
    invisible(TRUE)
  })
  helper_env <- new.env(parent = integration_env)
  helper_env$rv <- rv
  helper_env$primary_data_state <- primary_data_state
  helper_env$debug_log <- function(message, ...) logs <<- c(logs, message)
  eval(.find_assignment(body(integration_env$initialize_submodules),
                        "datawizard_restore_phase_active"), helper_env)
  eval(.find_assignment(body(integration_env$initialize_submodules),
                        "publish_primary_data"), helper_env)

  ratios_call <- .find_call(body(integration_env$initialize_submodules), "modRatiosServer")
  ratios_set_data <- as.list(ratios_call)$set_data
  helper_env$core_values <- list(
    metadata_observer_active = reactiveVal(TRUE),
    filter_applied = reactiveVal(FALSE),
    handson_metadata = reactiveVal(metadata_a)
  )
  helper_env$modification_functions <- list(record_modification = function(...) invisible(NULL))
  helper_env$metadata_functions <- list(update_metadata_for_ratio_columns = function(...) invisible(NULL))
  helper_env$modules <- list(ratios_out = list())
  helper_env$sync_enhanced_metadata_for_current_data <- function(...) FALSE
  helper_env$showNotification <- function(...) invisible(NULL)
  offending_callback <- eval(ratios_set_data, helper_env)

  expect_false(isolate(offending_callback(reconstructed_plot$data_mod)))
  expect_identical(isolate(rv$data_mod), canonical_a)
  expect_identical(isolate(rv$data_def), metadata_a)
  expect_identical(registry_state$primary_working, canonical_a)
  expect_true(any(grepl("operation=integration set_data", logs, fixed = TRUE)))
  expect_true(any(grepl("incoming dimensions=2 x 46", logs, fixed = TRUE)))

  isolate({
    rv$session_restoring <- FALSE
    rv$session_restore_phase <- "complete"
  })
  expect_true(isolate(offending_callback(reconstructed_plot$data_mod)))
  expect_identical(isolate(rv$data_mod), historical_b)
  expect_identical(registry_state$primary_working, historical_b)
})
