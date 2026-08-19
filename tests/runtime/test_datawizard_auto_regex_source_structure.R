library(testthat)

auto_regex_source_dir <- file.path("modules", "Data Wizard", "auto regex")

legacy_sources <- file.path(auto_regex_source_dir, c(
  "datawizard_auto_regex_utils.R",
  "datawizard_auto_regex_logic.R",
  "datawizard_auto_regex_handlers.R"
))

source_auto_regex_layers <- function() {
  implementation <- new.env(parent = baseenv())
  expected_by_layer <- list(
    c("extract_condition", "apply_content_table", "apply_condition_table",
      "apply_ratio_table", "data_wizard_normalize_rules", "validate_export"),
    c("infer_content", "infer_conditions", "infer_ratios",
      "auto_regex_infer_rules"),
    "auto_regex_register_handlers"
  )

  for (index in seq_along(legacy_sources)) {
    sys.source(legacy_sources[[index]], envir = implementation)
    missing <- expected_by_layer[[index]][!vapply(
      expected_by_layer[[index]], exists, logical(1), envir = implementation,
      mode = "function", inherits = FALSE
    )]
    expect_length(missing, 0L, info = paste("missing after sourcing",
      basename(legacy_sources[[index]])))
  }

  implementation
}

test_that("legacy Auto Regex layers source in order into an isolated environment", {
  implementation <- source_auto_regex_layers()

  expect_identical(environment(implementation$infer_conditions), implementation)
  expect_false(exists("infer_conditions", envir = .GlobalEnv, inherits = FALSE))
  expect_false(exists("auto_regex_register_handlers", envir = .GlobalEnv,
    inherits = FALSE))
})

test_that("condition inference resolves extract_condition in its source environment", {
  implementation <- source_auto_regex_layers()
  inference_environment <- environment(implementation$infer_conditions)
  original <- get("extract_condition", envir = inference_environment,
    inherits = FALSE)
  observed <- FALSE
  replacement <- function(...) {
    observed <<- TRUE
    original(...)
  }

  assign("extract_condition", replacement, envir = inference_environment)
  on.exit(assign("extract_condition", original, envir = inference_environment),
    add = TRUE)

  metadata <- read.csv(
    file.path("tests", "fixtures", "regex_metadata_assistant", "condition.csv"),
    stringsAsFactors = FALSE, na.strings = "NA", check.names = FALSE
  )
  implementation$infer_conditions(metadata, "Options")

  expect_true(observed)
  assign("extract_condition", original, envir = inference_environment)
  expect_identical(get("extract_condition", envir = inference_environment,
    inherits = FALSE), original)
})

test_that("new Auto Regex source files stay within the structural size limit", {
  source_files <- list.files(auto_regex_source_dir, pattern = "[.]R$",
    full.names = TRUE)
  # These are the pre-split files this test precedes. Remove entries as they are
  # split; every existing smaller file and every newly added file is enforced.
  pre_split_files <- file.path(auto_regex_source_dir, c(
    "datawizard_auto_regex_utils.R",
    "datawizard_auto_regex_logic.R",
    "datawizard_auto_regex_handlers.R"
  ))
  enforced_files <- setdiff(source_files, pre_split_files)
  line_counts <- vapply(enforced_files, function(path) {
    length(readLines(path, warn = FALSE))
  }, integer(1))

  expect_true(all(line_counts <= 1000L), info = paste(
    sprintf("%s: %d lines", basename(enforced_files), line_counts),
    collapse = "\n"
  ))
})

test_that("legacy utility loader exposes extracted contract and source identity families", {
  implementation <- new.env(parent = baseenv())
  sys.source(legacy_sources[[1L]], envir = implementation)

  expected_contracts <- c(
    "AUTO_REGEX_METADATA_SCHEMA", "CONTENT_FIELDS", "CONDITION_FIELDS",
    "RATIO_FIELDS", "empty_content", "empty_condition", "empty_ratio",
    "canonical_rule_schemas", "canonical_rule_classes",
    "coerce_rule_component_classes", "stable_variant_ids", "stable_rule_ids",
    "upgrade_rule_component", "canonical_prerequisite_rules",
    "validate_metadata"
  )
  expected_source_identity <- c(
    "auto_regex_normalize_metadata", "auto_regex_column_signature",
    "auto_regex_compact_signature", "auto_regex_source_descriptor",
    "auto_regex_source_change_reason"
  )

  expect_true(all(vapply(c(expected_contracts, expected_source_identity), exists,
    logical(1), envir = implementation, inherits = FALSE)))
  expect_identical(environment(implementation$canonical_rule_schemas),
    implementation)
  expect_identical(environment(implementation$auto_regex_source_descriptor),
    implementation)
})

test_that("utility compatibility loader sources extracted families first", {
  utility <- readLines(legacy_sources[[1L]], warn = FALSE)
  contract_source <- grep("datawizard_auto_regex_contracts[.]R", utility)
  identity_source <- grep("datawizard_auto_regex_source_identity[.]R", utility)
  first_residual <- grep("MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT <-", utility,
    fixed = TRUE)

  expect_length(contract_source, 1L)
  expect_length(identity_source, 1L)
  expect_lt(contract_source, identity_source)
  expect_lt(identity_source, first_residual)
})
