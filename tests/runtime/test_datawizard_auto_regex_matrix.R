library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")

regex_fixture <- function(name) read.csv(
  file.path("tests/fixtures/regex_metadata_assistant", name),
  stringsAsFactors = FALSE, na.strings = "NA", check.names = FALSE
)

test_that("regex primitives preserve literals, storage, and engine validation", {
  literal <- "a/b (x)+[1]"
  escaped <- regex_escape_literal(literal)
  expect_true(safe_grepl(escaped, literal))
  expect_false(safe_grepl(escaped, toupper(literal)))
  stored <- regex_to_miraprot_storage(escaped, "content")
  expect_identical(regex_from_miraprot_storage(stored, "content"), escaped)
  expect_identical(regex_to_miraprot_storage(stored, "content"), stored)
  expect_true(validate_pcre("(?:raw|norm)[[:digit:]]+")$valid)
  expect_false(validate_pcre("(")$valid)
  expect_true(validate_stringr_pattern("raw\\s+")$valid)
  expect_false(any(safe_grepl("(", c("a", "b"))))
})

test_that("metadata validation reports structural and transformation defects", {
  bad <- data.frame(Column = c("", "x", "x"), Content = "Raw Abundance",
    Transformation = c("sqrt", "log2", "log10"), check.names = FALSE)
  checks <- validate_metadata(bad, c("Column", "Column", "Content"), "")
  expect_true(all(c("column names", "Column values", "duplicates", "Transformation") %in% checks$Check))
  expect_true(any(checks$Severity == "Error"))
  expect_match(condition_content_validation_messages(data.frame(Content = "Protein ID")),
    "not sample-bearing")
})

test_that("structural validation is separate from condition readiness", {
  metadata <- data.frame(
    Column = c("Normalized sample A", "Found sample B"),
    Content = c("Normalized Abundance", "Found in Sample"),
    Options = c("", ""), Transformation = c("None", ""),
    stringsAsFactors = FALSE, check.names = FALSE)

  validation <- validate_metadata(metadata, names(metadata), "Options")
  expect_identical(validation$Severity, "Info")
  expect_identical(validation$Message, "No structural problems detected.")

  readiness <- auto_regex_condition_reference_summary(metadata)
  expect_identical(readiness$target, "Options")
  expect_identical(readiness$applicable_rows, 2L)
  expect_identical(readiness$reference_rows, 0L)
  expect_identical(readiness$unavailable_labels,
    c("Normalized Abundance", "Found in Sample"))
  expect_identical(readiness$status, "warning")

  condition <- infer_conditions(metadata, "Options")
  expect_identical(condition$warnings,
    "Condition inference unavailable for Normalized Abundance and Found in Sample because Options is empty.")
})

test_that("condition readiness is not applicable without sample-bearing content", {
  metadata <- data.frame(Column = "Protein accession", Content = "Identifier",
    Options = "", Transformation = "", stringsAsFactors = FALSE)
  readiness <- auto_regex_condition_reference_summary(metadata)
  expect_identical(readiness$status, "not_applicable")
  expect_identical(readiness$applicable_rows, 0L)
  expect_length(infer_conditions(metadata, "Options")$warnings, 0L)
  expect_length(infer_conditions(metadata[, c("Column", "Content")], "")$warnings, 0L)
})

test_that("equivalent metadata representations have identical readiness", {
  current <- data.frame(Column = c("Raw A", "Raw B"),
    Content = c("Raw Abundance", "Raw Abundance"), Options = c("A", "B"),
    stringsAsFactors = FALSE)
  excel <- data.frame(Column = factor(current$Column), Content = factor(current$Content),
    Options = factor(current$Options), stringsAsFactors = FALSE)
  expect_identical(auto_regex_condition_reference_summary(current),
    auto_regex_condition_reference_summary(excel))
  expect_identical(auto_regex_condition_reference_summary(current)$status, "ready")

  selected <- current
  selected$Options <- ""
  expect_error(auto_regex_verify_condition_reference_transfer(current, selected),
    "lost while transferring")
})

test_that("token inference and deterministic selection are reproducible", {
  values <- c("Raw A R1", "Raw B R2")
  lexed <- tokens(values)
  expect_identical(vapply(split(lexed, lexed$Source), function(z)
    paste0(z$Text, collapse = ""), character(1)), values)
  first <- candidate_fragments(values, c("Normalized A R1"))
  second <- candidate_fragments(rev(values), c("Normalized A R1"))
  expect_identical(first, second)
  score <- score_pattern("^Raw", c(values, "Normalized"), c(TRUE, TRUE, FALSE))
  expect_equal(score$TP, 2L)
  expect_equal(score$FP, 0L)
})

test_that("schemas, coercion, complexity, and fingerprints are stable", {
  rules <- list(table = empty_content(), condition = empty_condition(), ratio = empty_ratio())
  coerced <- coerce_contract(rules)
  expect_identical(names(coerced$table), CONTENT_FIELDS)
  expect_identical(names(coerced$condition), CONDITION_FIELDS)
  expect_identical(names(coerced$ratio), RATIO_FIELDS)
  expect_identical(regex_complexity("a|b(?:c)+"), regex_complexity("a|b(?:c)+"))
  fingerprint <- function(x) paste(sprintf("%02x", as.integer(serialize(x, NULL, version = 2L))), collapse = "")
  expect_identical(fingerprint(coerced), fingerprint(coerce_contract(rules)))
  changed <- coerced
  changed$extra <- 1
  expect_false(identical(fingerprint(coerced), fingerprint(changed)))
})

test_that("integrated inference agrees with every standalone golden fixture", {
  metadata <- Reduce(function(x, y) merge(x, y, all = TRUE),
    lapply(c("content.csv", "condition.csv", "ratio.csv"), regex_fixture))
  integrated <- auto_regex_infer_rules(metadata, condition_target = "Options")
  standalone <- list(
    table = infer_content(metadata)$table,
    condition = infer_conditions(metadata, "Options")$table,
    ratio = infer_ratios(metadata)$table
  )
  expect_identical(integrated$rules, coerce_contract(standalone))
  expect_length(integrated$errors, 0L)
  exported <- build_export_template(integrated$rules)
  expect_length(validate_export(exported, metadata), 0L)
})
