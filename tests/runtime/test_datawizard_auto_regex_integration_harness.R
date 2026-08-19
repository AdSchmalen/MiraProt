library(testthat)
library(shiny)

source("modules/Data Wizard/datawizard_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_reactive_state.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")
source("tests/runtime/helpers/datawizard_auto_regex_integration_harness.R")

fixture_metadata <- function() data.frame(
  Column = c("Protein IDs", "Raw A R1", "Raw B R1", "A_B_ratio"),
  Content = c("Protein ID", "Raw Abundance", "Raw Abundance", "Abundance Ratio"),
  Options = c(NA, "A", "B", NA), Transformation = c(NA, "log2", "log2", NA),
  Numerator = c(NA, NA, NA, "A"), Denominator = c(NA, NA, NA, "B"),
  stringsAsFactors = FALSE, check.names = FALSE)

test_that("readiness classifies paired synchronized metadata and data exactly", {
  metadata <- fixture_metadata()
  data <- setNames(data.frame(matrix(seq_len(8), nrow = 2)), metadata$Column)

  cases <- list(
    aligned_and_assigned = list(metadata, data, "ready"),
    aligned_empty_content = list(transform(metadata, Content = ""), data,
                                 "assignments_required"),
    missing_column = list(metadata[names(metadata) != "Column"], data,
                          "missing_column"),
    metadata_absent = list(NULL, data, "metadata_unavailable"),
    reordered_columns = list(metadata[c(2, 1, 3, 4), ], data, "misaligned"),
    missing_column_in_data = list(metadata, data[names(data)[-4]], "misaligned"),
    extra_column_in_data = list(metadata, transform(data, extra = 1), "misaligned")
  )
  for (case in cases) {
    expect_identical(auto_regex_current_readiness(case[[1]], case[[2]]),
                     case[[3]])
  }

  # A synchronized pair becomes stale when only its dataset is replaced.
  expect_identical(auto_regex_current_readiness(metadata, data), "ready")
  replacement <- data
  names(replacement)[2] <- "Raw C R1"
  expect_identical(auto_regex_current_readiness(metadata, replacement),
                   "misaligned")
  synchronized <- metadata
  synchronized$Column <- names(replacement)
  expect_identical(auto_regex_current_readiness(synchronized, replacement),
                   "ready")
})

test_that("aligned skeleton, pending edits, and true misalignment are distinct", {
  data <- setNames(data.frame(matrix(nrow = 1, ncol = 4)), fixture_metadata()$Column)
  skeleton <- data.frame(Column = names(data), Content = NA_character_,
                         stringsAsFactors = FALSE)
  h <- auto_regex_test_harness(skeleton, data)
  expect_identical(h$readiness(), "assignments_required")

  pending <- skeleton
  pending$Content[[1L]] <- "Protein ID"
  h$values$pending(pending)
  expect_identical(h$readiness(), "ready")
  expect_identical(h$effective_metadata(), pending)

  h$values$pending(NULL)
  h$values$metadata(skeleton[-1L, , drop = FALSE])
  expect_identical(h$readiness(), "misaligned")
})

test_that("Excel descriptors track effective changes but ignore type-only invalidation", {
  sheets <- list(Only = fixture_metadata(), Other = transform(fixture_metadata(), Options = "X"))
  loader <- function(workbook, sheet) sheets[[sheet]]
  h <- auto_regex_test_harness(fixture_metadata(), data.frame(), loader = loader)
  workbook <- list(name = "fixture.xlsx", size = 10, revision = 1L)
  exact <- setNames(names(sheets$Only), names(sheets$Only))
  one <- h$excel(workbook, "Only", exact)
  same <- h$excel(workbook, "Only", exact)
  expect_identical(one$signature, same$signature)
  expect_identical(one$metadata, sheets$Only)

  changed_mapping <- exact
  changed_mapping[c("Options", "Transformation")] <- rev(changed_mapping[c("Options", "Transformation")])
  expect_false(identical(one$signature, h$excel(workbook, "Only", changed_mapping)$signature))
  expect_false(identical(one$signature, h$excel(workbook, "Other", exact)$signature))

  typed <- sheets$Only
  typed$Column <- factor(typed$Column)
  sheets$Only <- typed
  expect_identical(one$signature, h$excel(workbook, "Only", exact)$signature)
})

test_that("inference progress is monotonic by phase and standalone payload is unchanged", {
  metadata <- fixture_metadata()
  h <- auto_regex_test_harness(metadata, setNames(data.frame(matrix(nrow = 1,
    ncol = nrow(metadata))), metadata$Column))
  result <- h$infer()
  expected_events <- c("preprocessing_start", "preprocessing_complete",
    "content_start", "content_complete", "condition_start", "condition_complete",
    "ratio_start", "ratio_complete", "payload_validation_start",
    "payload_validation_complete")
  expect_identical(h$progress(), expected_events)
  expect_identical(result$rules, coerce_contract(list(
    table = infer_content(metadata)$table,
    condition = infer_conditions(metadata, "Options")$table,
    ratio = infer_ratios(metadata)$table)))
})

test_that("public transfer rollback and a later failed inference preserve first rules", {
  metadata <- fixture_metadata()
  old <- coerce_contract(list())
  h <- auto_regex_test_harness(metadata, data.frame(), initial_rules = old)
  first <- h$infer()$rules
  expect_true(h$load(first))
  expect_identical(shiny::isolate(h$values$rules()), first)

  prior <- shiny::isolate(h$values$rules())
  failing_transfer <- function(payload) {
    h$values$rules(payload)
    FALSE
  }
  expect_false(failing_transfer(coerce_contract(list())))
  expect_true(h$load(prior))
  expect_identical(shiny::isolate(h$values$rules()), first)

  failed <- h$infer(data.frame(Column = "x"))
  expect_gt(length(failed$errors), 0L)
  expect_identical(shiny::isolate(h$values$rules()), first)
  expect_identical(tail(h$loads(), 1L)[[1L]], first)
})

test_that("processing lock and cleanup transitions are repeatable", {
  h <- auto_regex_test_harness(fixture_metadata(), data.frame())
  expect_false(shiny::isolate(h$state$processing()))
  h$state$processing(TRUE)
  # This is the same guard used by the handler for a second click.
  expect_true(shiny::isolate(h$state$processing()))
  h$state$processing(FALSE)
  expect_silent(h$state$cleanup())
  expect_silent(h$state$cleanup())
  expect_identical(shiny::isolate(h$state$run_status()), "cleaned")
})

test_that("stage outcomes distinguish success, failure, and skipped phases", {
  metadata <- fixture_metadata()
  events <- list()
  log_message <- function(message, level) {
    events[[length(events) + 1L]] <<- list(message=message, level=level)
  }
  content_ok <- function(...) list(table=empty_content(), status=data.frame(),
    metrics=data.frame(), warnings=character())
  condition_ok <- function(...) list(table=empty_condition(), status=data.frame(),
    diagnostics=data.frame(), warnings=character())
  ratio_ok <- function(...) list(table=empty_ratio(), status=data.frame(),
    diagnostics=data.frame(), warnings=character(), timings=c())

  successful <- auto_regex_infer_rules(metadata, debug_log=log_message,
    .stage_functions=list(content=content_ok, condition=condition_ok, ratio=ratio_ok))
  expect_identical(unname(successful$stage_outcomes), rep("success", 4L))
  level_one <- vapply(events, function(x) x$level == 1L, logical(1))
  messages <- vapply(events, `[[`, character(1), "message")
  for (phase in c("Preprocessing and validation", "Content inference",
                  "Condition inference", "Ratio inference")) {
    expect_identical(sum(level_one & messages == paste0(phase, " started.")), 1L)
    expect_identical(sum(level_one & messages == paste0(phase, " completed.")), 1L)
  }
  expect_true(all(vapply(events[grepl("elapsed time", messages, fixed=TRUE)],
    function(x) x$level == 2L, logical(1))))

  events <- list()
  content_error <- function(...) stop("injected content error", call.=FALSE)
  failed <- auto_regex_infer_rules(metadata, debug_log=log_message,
    .stage_functions=list(content=content_error, condition=condition_ok, ratio=ratio_ok))
  expect_identical(failed$stage_outcomes[c("content", "condition", "ratio")],
    c(content="failure", condition="success", ratio="skipped"))
  messages <- vapply(events, `[[`, character(1), "message")
  levels <- vapply(events, `[[`, integer(1), "level")
  expect_true(any(levels == 1L & grepl(
    "Content inference failed: injected content error", messages, fixed=TRUE)))
  expect_false(any(messages == "Content inference completed."))
  expect_true(any(grepl("stage=content | outcome=failure | error=injected content error",
    failed$errors, fixed=TRUE)))
})

test_that("an empty valid optional stage result is successful, not exceptional", {
  metadata <- fixture_metadata()
  ratio_called <- FALSE
  content_ok <- function(...) list(table=empty_content(), status=data.frame(),
    metrics=data.frame(), warnings=character())
  empty_condition_ok <- function(...) list(table=empty_condition(), status=data.frame(),
    diagnostics=data.frame(), warnings=character())
  ratio_ok <- function(...) {
    ratio_called <<- TRUE
    list(table=empty_ratio(), status=data.frame(), diagnostics=data.frame(),
      warnings=character(), timings=c())
  }
  result <- auto_regex_infer_rules(metadata, .stage_functions=list(
    content=content_ok, condition=empty_condition_ok, ratio=ratio_ok))
  expect_identical(result$stage_outcomes[["condition"]], "success")
  expect_true(ratio_called)
  expect_length(result$errors, 0L)
  expect_identical(result$rules$condition, empty_condition())
})
