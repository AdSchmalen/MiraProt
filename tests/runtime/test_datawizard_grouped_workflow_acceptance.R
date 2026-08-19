library(testthat)
library(shiny)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")
source("modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R")

workflow_fixture <- read.csv(
  "tests/fixtures/regex_metadata_assistant/grouped_workflows.csv",
  stringsAsFactors = FALSE, na.strings = "NA", check.names = FALSE
)
workflow <- function(name) {
  x <- workflow_fixture[workflow_fixture$Scenario == name, , drop = FALSE]
  x$Scenario <- NULL
  x
}

empty_metadata_skeleton <- function(rows) {
  data.frame(Column = rows$Column, Content = "", Options = NA_character_,
    Transformation = NA_character_, Numerator = NA_character_,
    Denominator = NA_character_, Sample = NA_character_,
    stringsAsFactors = FALSE, check.names = FALSE)
}

normalize_golden_metadata <- function(x) {
  fields <- c("Column", "Content", "Options", "Transformation", "Numerator",
    "Denominator")
  for (field in fields) x[[field]][is.na(x[[field]])] <- ""
  x[, fields, drop = FALSE]
}

production_roundtrip <- function(rows, partition_recovery = auto_regex_partition_recovery) {
  inferred <- auto_regex_infer_rules(rows, condition_target = "Options",
    .partition_recovery = partition_recovery)
  exported <- build_export_template(inferred$rules,
    exported_at = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_length(validate_export(exported, rows), 0L)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(exported, path, version = 2L)
  imported <- prepare_assign_rules_rule_envelope(readRDS(path))$rules_data

  # This is intentionally field-for-field rather than a lossy semantic
  # comparison: identity, precedence and every method-specific operand are
  # part of the persisted rule contract.
  expect_identical(imported$table[CONTENT_FIELDS], inferred$rules$table[CONTENT_FIELDS])
  expect_identical(imported$condition[CONDITION_FIELDS],
    inferred$rules$condition[CONDITION_FIELDS])
  expect_identical(imported$ratio[RATIO_FIELDS], inferred$rules$ratio[RATIO_FIELDS])

  variants <- unique(imported$table[c("Content", "VariantId")])
  for (component in c("condition", "ratio")) {
    dependent <- imported[[component]]
    if (nrow(dependent)) expect_true(all(paste(dependent$Content, dependent$VariantId) %in%
      paste(variants$Content, variants$VariantId)), info = component)
  }

  engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL, add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = reactiveVal(imported$table),
    rv_condition_rules_autoassign_dw = reactiveVal(imported$condition),
    rv_rules_autoassign_dw = reactiveVal(imported$ratio),
    rules_loaded_centrally = reactiveVal(TRUE),
    extractedConds_autoassign_dw = reactiveVal(character())
  )
  applied <- isolate(engine$apply_auto_assign_rules(empty_metadata_skeleton(rows)))
  list(inferred = inferred, exported = exported, imported = imported,
    applied = applied)
}

test_that("the complete grouped golden workflow survives RDS and the production engine", {
  for (scenario in c("classic", "merged", "datawizard", "mixed")) {
    rows <- workflow(scenario)
    roundtrip <- production_roundtrip(rows)
    expect_identical(normalize_golden_metadata(roundtrip$applied),
      normalize_golden_metadata(rows), info = scenario)

    assigned_variant <- attr(roundtrip$applied, "variant_id", exact = TRUE)
    for (component in c("condition", "ratio")) {
      dependent <- roundtrip$imported[[component]]
      for (i in seq_len(nrow(dependent))) {
        touched <- roundtrip$applied$Content == dependent$Content[[i]] &
          assigned_variant == dependent$VariantId[[i]]
        other_variant <- roundtrip$applied$Content == dependent$Content[[i]] &
          assigned_variant != dependent$VariantId[[i]]
        if (component == "condition" && any(touched))
          expect_true(all(nzchar(roundtrip$applied$Options[touched])), info = scenario)
        if (component == "ratio" && any(touched))
          expect_true(all(nzchar(roundtrip$applied$Numerator[touched]) &
            nzchar(roundtrip$applied$Denominator[touched])), info = scenario)
        if (component == "ratio" && any(other_variant))
          expect_true(all((is.na(roundtrip$applied$Numerator[other_variant]) |
            !nzchar(roundtrip$applied$Numerator[other_variant])) &
            (is.na(roundtrip$applied$Denominator[other_variant]) |
            !nzchar(roundtrip$applied$Denominator[other_variant]))), info = scenario)
      }
    }

    search <- roundtrip$inferred$diagnostics$partition_search
    if (nrow(search)) {
      expect_true(all(search$Authoritative), info = scenario)
      expect_true(all(search$CompleteReplay), info = scenario)
      replay <- apply_content_table(rows, roundtrip$imported$table)
      expect_true(all(replay$rows$Match), info = scenario)
      expect_false(any(replay$rows$Conflict), info = scenario)
      expect_identical(nrow(replay$conflicts), 0L, info = scenario)
    }
  }
})

test_that("the established single-rule workflow remains byte-for-byte compatible", {
  rows <- workflow("classic")
  recovery_calls <- 0L
  forbidden_recovery <- function(...) {
    recovery_calls <<- recovery_calls + 1L
    stop("partition recovery must not run for the classic workflow")
  }
  legacy <- list(
    table = infer_content(rows)$table,
    condition = infer_conditions(rows, "Options")$table,
    ratio = infer_ratios(rows)$table
  )
  integrated <- auto_regex_infer_rules(rows, condition_target = "Options",
    .partition_recovery = forbidden_recovery)

  expect_identical(integrated$rules, coerce_contract(legacy))
  expect_identical(sum(integrated$rules$table$Content == "Raw Abundance"), 1L)
  expect_identical(
    integrated$rules$table[integrated$rules$table$Content == "Raw Abundance",
      c("Include", "Exclude", "Transformation"), drop = FALSE],
    legacy$table[legacy$table$Content == "Raw Abundance",
      c("Include", "Exclude", "Transformation"), drop = FALSE]
  )
  expect_identical(integrated$rules$condition, legacy$condition)
  expect_identical(integrated$rules$ratio, legacy$ratio)
  replay <- apply_content_table(rows, integrated$rules$table)
  expect_identical(replay$metadata$Content, rows$Content)
  expect_identical(replay$metadata$Transformation, c("log2", "log2", NA, NA))
  expect_identical(
    apply_condition_table(replay$metadata, integrated$rules$condition)$metadata$Options,
    c("A", "B", "", "")
  )
  expect_length(integrated$diagnostics$partition_search, 0L)
  expect_identical(recovery_calls, 0L)
})

test_that("ratio families are grouped only when heterogeneous and fully replayable", {
  merged <- auto_regex_infer_rules(workflow("merged"))
  datawizard <- auto_regex_infer_rules(workflow("datawizard"))
  mixed <- auto_regex_infer_rules(workflow("mixed"))

  expect_identical(sum(merged$rules$table$Content == "Abundance Ratio"), 1L)
  expect_identical(sum(merged$rules$ratio$Content == "Abundance Ratio"), 1L)
  expect_identical(sum(datawizard$rules$table$Content == "Abundance Ratio"), 1L)
  expect_identical(sum(datawizard$rules$ratio$Content == "Abundance Ratio"), 0L)

  variants <- mixed$rules$table[mixed$rules$table$Content == "Abundance Ratio", , drop = FALSE]
  expect_gte(nrow(variants), 2L)
  expect_identical(anyDuplicated(variants$VariantId), 0L)
  expect_identical(sum(mixed$rules$ratio$Content == "Abundance Ratio"), 1L)
  replay <- apply_content_table(workflow("mixed"), mixed$rules$table)
  expect_true(all(replay$rows$Match))
  expect_false(any(replay$rows$Conflict))
  expect_true(all(mixed$diagnostics$partition_search$CompleteReplay))
  expect_true(all(mixed$diagnostics$partition_search$Authoritative))
})

test_that("intentional overlap is diagnosed and cannot masquerade as safe replay", {
  rows <- workflow("mixed")
  overlap <- rbind(
    canonical_rule_row("content", Content = "Abundance Ratio", VariantId = "broad",
      Priority = 1L, Include = "Ratio", Exclude = "", Transformation = NA_character_),
    canonical_rule_row("content", Content = "Abundance Ratio", VariantId = "merged",
      Priority = 2L, Include = "Merged", Exclude = "", Transformation = NA_character_)
  )
  replay <- apply_content_table(rows, overlap)
  expect_true(any(replay$rows$Conflict))
  expect_gt(nrow(replay$conflicts), 0L)
  expect_false(all(replay$rows$Match))
})

test_that("bounded partition search abstains safely at its resource limit", {
  limited <- function(metadata, label, condition_target, logger) {
    limits <- AUTO_REGEX_PARTITION_LIMITS
    limits$nodes <- 0L
    auto_regex_partition_recovery(metadata, label, condition_target, logger, limits)
  }
  result <- auto_regex_infer_rules(workflow("mixed"), .partition_recovery = limited)
  expect_lte(sum(result$rules$table$Content == "Abundance Ratio"), 1L)
  expect_true(any(grepl("limit exhausted", result$warnings, fixed = TRUE)))
  expect_false(any(result$diagnostics$partition_search$Authoritative %in% TRUE))
})

test_that("the complete 110-row overlapping-condition workbook replays exactly", {
  fixture_path <- "tests/fixtures/regex_metadata_assistant/full_110_case.csv"
  expected_path <- "tests/fixtures/regex_metadata_assistant/full_110_expected.csv"
  source_rows <- read.csv(fixture_path, stringsAsFactors = FALSE,
    na.strings = "NA", check.names = FALSE)
  expected <- read.csv(expected_path, stringsAsFactors = FALSE,
    na.strings = "NA", check.names = FALSE)
  fields <- c("Column", "Content", "Options", "Numerator", "Denominator",
    "Transformation", "Sample")
  normalize_blanks <- function(x) {
    for (field in fields) x[[field]][is.na(x[[field]])] <- ""
    x[, fields, drop = FALSE]
  }
  source_rows <- normalize_blanks(source_rows)
  expected <- normalize_blanks(expected)

  # This full shape reproduces the reported failure in which the Identifier
  # fallback interpreted the suffix of "Capsid" as the token "id" and could
  # consequently steal unresolved ratio headers.
  inferred <- auto_regex_infer_rules(source_rows, condition_target = "Options")
  replay <- apply_content_table(source_rows, inferred$rules$table)$metadata
  replay <- apply_condition_table(replay, inferred$rules$condition)$metadata
  replay <- apply_ratio_table(replay, inferred$rules$ratio,
    source_rows$Numerator, source_rows$Denominator)$metadata
  replay <- normalize_blanks(replay)

  expect_identical(nrow(replay), 110L)
  expect_identical(replay$Column, source_rows$Column)
  expect_identical(replay, expected)

  merged <- startsWith(replay$Column, "Merged_")
  data_wizard <- grepl("^Test(Alpha|Beta)_", replay$Column)
  ratio_headers <- merged | data_wizard
  expect_identical(sum(merged), 45L)
  expect_true(all(replay$Content[merged] %in% c("Abundance Ratio",
    "Abundance Ratio p-Value", "Abundance Ratio adj. p-Value")))
  expect_true(all(nzchar(replay$Numerator[merged])))
  expect_true(all(nzchar(replay$Denominator[merged])))
  expect_identical(replay$Numerator[merged], expected$Numerator[merged])
  expect_identical(replay$Denominator[merged], expected$Denominator[merged])
  expect_identical(sum(data_wizard), 6L)
  expect_true(all(replay$Content[data_wizard] %in% c("Abundance Ratio",
    "Abundance Ratio p-Value", "Abundance Ratio adj. p-Value")))
  expect_true(all(replay$Options[data_wizard] == "Ratio"))
  expect_true(all(replay$Numerator[data_wizard] == ""))
  expect_true(all(replay$Denominator[data_wizard] == ""))
  # The complete authoritative fixture is the release gate: no row with a
  # non-Identifier truth label may be assigned Identifier.
  identifier_false_positives <- replay$Content == "Identifier" &
    source_rows$Content != "Identifier"
  expect_identical(sum(identifier_false_positives), 0L)
  fallback_diagnostic <- inferred$diagnostics$identifier_fallback
  expect_true(all(c("PatternMatched", "ExpectedContent", "PreviouslyUnassigned",
    "OverriddenBySpecificRule", "RejectedByAuthoritativeNonIdentifier") %in%
    names(fallback_diagnostic)))
  expect_true(any(fallback_diagnostic$RejectedByAuthoritativeNonIdentifier))
  expect_true(all(fallback_diagnostic$FallbackRejected))
  expect_identical(replay$Content[replay$Column == "PG.ProteinDescriptions"],
    "Description")
  expect_identical(sum(replay$Content == "Description"), 1L)
})
