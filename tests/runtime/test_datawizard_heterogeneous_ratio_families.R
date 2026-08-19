library(testthat)
library(shiny)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")
source("modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R")

fixture <- read.csv(
  "tests/fixtures/regex_metadata_assistant/heterogeneous_ratio_families.csv",
  stringsAsFactors = FALSE, na.strings = "NA", check.names = FALSE
)

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

test_that("compound identifier tokens do not split family signatures", {
  sigs <- auto_regex_family_signature(fixture, seq_len(nrow(fixture)))
  # Check each ratio Content: Alpha_ and Alpha_2_ must share one family
  for (content in c("Abundance Ratio", "Abundance Ratio p-Value",
      "Abundance Ratio adj. p-Value")) {
    rows <- which(fixture$Content == content)
    content_sigs <- unique(sigs[rows])
    content_sigs <- content_sigs[nzchar(content_sigs)]
    expect_true(length(content_sigs) <= 2L,
      info = paste(content, "should have at most 2 families"))
  }
  # Specifically verify Alpha_ and Alpha_2_ merge
  alpha1 <- which(fixture$Column == "Alpha_Abundance Ratio")
  alpha2 <- which(fixture$Column == "Alpha_2_Abundance Ratio")
  expect_identical(sigs[alpha1], sigs[alpha2])
})

test_that("Abundance Ratio content rule matches merged and DW families", {
  inferred <- auto_regex_infer_rules(fixture, condition_target = "Options")
  expect_true("Abundance Ratio" %in% inferred$rules$table$Content)

  replay <- apply_content_table(fixture, inferred$rules$table)
  ratio_rows <- which(fixture$Content == "Abundance Ratio")
  expect_true(all(replay$rows$Match[ratio_rows]),
    info = "all Abundance Ratio rows match")
})

test_that("merged Abundance Ratio rows get ratio extraction rule", {
  inferred <- auto_regex_infer_rules(fixture, condition_target = "Options")
  ratio_rules <- inferred$rules$ratio[
    inferred$rules$ratio$Content == "Abundance Ratio", , drop = FALSE]
  expect_true(nrow(ratio_rules) >= 1L,
    info = "at least one ratio rule for Abundance Ratio")

  # Replay and verify merged rows
  meta <- apply_content_table(fixture, inferred$rules$table)$metadata
  meta <- apply_condition_table(meta, inferred$rules$condition)$metadata
  meta <- apply_ratio_table(meta, inferred$rules$ratio,
    fixture$Numerator, fixture$Denominator)$metadata

  merged_ratio <- which(startsWith(fixture$Column, "Merged_Ratio_"))
  expect_true(all(nzchar(meta$Numerator[merged_ratio])),
    info = "merged Numerator extracted")
  expect_true(all(nzchar(meta$Denominator[merged_ratio])),
    info = "merged Denominator extracted")
  expect_identical(meta$Numerator[merged_ratio], fixture$Numerator[merged_ratio])
  expect_identical(meta$Denominator[merged_ratio], fixture$Denominator[merged_ratio])
})

test_that("DW ratio rows do not receive fabricated components", {
  inferred <- auto_regex_infer_rules(fixture, condition_target = "Options")
  meta <- apply_content_table(fixture, inferred$rules$table)$metadata
  meta <- apply_condition_table(meta, inferred$rules$condition)$metadata
  meta <- apply_ratio_table(meta, inferred$rules$ratio,
    fixture$Numerator, fixture$Denominator)$metadata

  dw <- grepl("^Alpha(_[0-9]+)?_Abundance", meta$Column)
  expect_true(all(is.na(meta$Numerator[dw]) | !nzchar(meta$Numerator[dw])),
    info = "DW Numerator empty")
  expect_true(all(is.na(meta$Denominator[dw]) | !nzchar(meta$Denominator[dw])),
    info = "DW Denominator empty")
})

test_that("no non-Identifier truth row is assigned Identifier", {
  inferred <- auto_regex_infer_rules(fixture, condition_target = "Options")
  meta <- apply_content_table(fixture, inferred$rules$table)$metadata
  identifier_false_positives <- meta$Content == "Identifier" &
    fixture$Content != "Identifier"
  expect_identical(sum(identifier_false_positives), 0L)
})

test_that("non-extractable sibling does not invalidate extractable sibling", {
  # The core regression: when both extractable (merged) and non-extractable (DW)
  # ratio rows share one Content, the extractable family must still get its
  # ratio extraction rule.  Previously, ratio inference evaluated completeness
  # at the whole-Content level and rejected the merged candidate because the DW
  # rows had no references.
  inferred <- auto_regex_infer_rules(fixture, condition_target = "Options")
  merged_ratio_rules <- inferred$rules$ratio[
    inferred$rules$ratio$Content == "Abundance Ratio", , drop = FALSE]
  expect_true(nrow(merged_ratio_rules) >= 1L,
    info = "at least one ratio rule for Abundance Ratio")
})

test_that("complete but nonrepresentable sibling keeps variant-scoped merged ratio rule", {
  annotated <- fixture

  alpha <- grepl(
    "^Alpha_Abundance Ratio",
    annotated$Column
  )

  alpha2 <- grepl(
    "^Alpha_2_Abundance Ratio",
    annotated$Column
  )

  # Deliberately complete training annotations that are not encoded
  # anywhere in the Data-Wizard column names.
  annotated$Numerator[alpha] <- "4_mock_IFNy, 5_mock_IFNy, 6_mock_IFNy"
  annotated$Denominator[alpha] <- "1_mock, 2_mock, 3_mock"

  annotated$Numerator[alpha2] <- "7_Capsid, 8_Capsid, 9_Capsid"
  annotated$Denominator[alpha2] <- "1_mock, 2_mock, 3_mock"

  inferred <- auto_regex_infer_rules(
    annotated,
    condition_target = "Options"
  )

  expect_length(
    inferred$errors,
    0L,
    info = paste(inferred$errors, collapse = " | ")
  )

  content_replay <- apply_content_table(
    annotated,
    inferred$rules$table
  )

  ratio_content_rules <- inferred$rules$table[
    inferred$rules$table$Content == "Abundance Ratio",
    ,
    drop = FALSE
  ]

  expect_gte(
    nrow(ratio_content_rules),
    2L,
    info = "extractable and non-extractable header families need distinct Content variants"
  )

  expect_equal(
    length(unique(ratio_content_rules$VariantId)),
    nrow(ratio_content_rules)
  )

  variants <- attr(
    content_replay$metadata,
    "variant_id",
    exact = TRUE
  )

  merged <- startsWith(
    annotated$Column,
    "Merged_Ratio_"
  )

  merged_variant <- unique(variants[merged])

  expect_length(merged_variant, 1L)

  ratio_rules <- inferred$rules$ratio[
    inferred$rules$ratio$Content == "Abundance Ratio",
    ,
    drop = FALSE
  ]

  expect_true(
    merged_variant %in% ratio_rules$VariantId,
    info = "extractable merged family must own a variant-scoped ratio rule"
  )

  ratio_truth <- grepl(
    "Abundance Ratio",
    annotated$Content,
    fixed = TRUE
  )

  expect_identical(
    content_replay$metadata$Content[ratio_truth],
    annotated$Content[ratio_truth]
  )

  variants <- attr(
    content_replay$metadata,
    "variant_id",
    exact = TRUE
  )

  expect_false(is.null(variants))

  # Every emitted downstream ratio rule must have a real parent.
  content_keys <- paste(
    inferred$rules$table$Content,
    inferred$rules$table$VariantId,
    sep = "\r"
  )

  ratio_keys <- paste(
    inferred$rules$ratio$Content,
    inferred$rules$ratio$VariantId,
    sep = "\r"
  )

  expect_true(all(ratio_keys %in% content_keys))

  replay <- content_replay$metadata

  replay <- apply_condition_table(
    replay,
    inferred$rules$condition
  )$metadata

  replay <- apply_ratio_table(
    replay,
    inferred$rules$ratio,
    annotated$Numerator,
    annotated$Denominator
  )$metadata

  merged <- startsWith(
    annotated$Column,
    "Merged_"
  )

  expect_identical(
    replay$Numerator[merged],
    annotated$Numerator[merged]
  )

  expect_identical(
    replay$Denominator[merged],
    annotated$Denominator[merged]
  )

  dw <- alpha | alpha2

  dw_variants <- unique(
    variants[
      dw &
        annotated$Content == "Abundance Ratio"
    ]
  )

  expect_false(
    any(dw_variants %in% ratio_rules$VariantId),
    info = paste(
      "complete-but-header-unrepresentable variants must classify as Ratio",
      "without owning a fabricated numerator/denominator extraction rule"
    )
  )

  expect_true(
    all(is.na(replay$Numerator[dw]) | !nzchar(replay$Numerator[dw]))
  )

  expect_true(
    all(is.na(replay$Denominator[dw]) | !nzchar(replay$Denominator[dw]))
  )
})

test_that("ratio inference preserves a non-default parent VariantId during replay", {
  metadata <- data.frame(
    Column = c(
      "Merged_Ratio_AAV2_eGFP_IFNy / mock",
      "Merged_Ratio_Capsid / mock_IFNy",
      "Merged_Ratio_AAV2_eGFP / Capsid"
    ),
    Content = rep("Abundance Ratio", 3L),
    Options = rep("Ratio", 3L),
    Numerator = c(
      "AAV2_eGFP_IFNy",
      "Capsid",
      "AAV2_eGFP"
    ),
    Denominator = c(
      "mock",
      "mock_IFNy",
      "Capsid"
    ),
    Transformation = rep("None", 3L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Deliberately NOT v1. Before the fix, candidate scoring succeeds but
  # apply_ratio_table() synthesizes v1 on the sliced replay data and targets
  # zero rows.
  parent_variant <- "abundance-ratio-v23"

  content_rule <- canonical_rule_row(
    "content",
    Content = "Abundance Ratio",
    VariantId = parent_variant,
    Priority = 1L,
    Include = "^Merged_Ratio_",
    Exclude = "",
    Transformation = "None"
  )

  result <- infer_ratios(
    metadata,
    content_rules = content_rule,
    condition_rules = empty_condition()
  )

  expect_equal(nrow(result$table), 1L)

  expect_identical(
    result$table$VariantId,
    parent_variant
  )

  applied <- apply_content_table(
    metadata,
    content_rule
  )$metadata

  applied <- apply_ratio_table(
    applied,
    result$table,
    metadata$Numerator,
    metadata$Denominator
  )

  expect_true(all(applied$diagnostics$Success))

  expect_identical(
    applied$metadata$Numerator,
    metadata$Numerator
  )

  expect_identical(
    applied$metadata$Denominator,
    metadata$Denominator
  )
})

test_that("partition candidates are atomic structural families", {
  annotated <- fixture

  alpha <- grepl(
    "^Alpha_Abundance Ratio",
    annotated$Column
  )

  alpha2 <- grepl(
    "^Alpha_2_Abundance Ratio",
    annotated$Column
  )

  annotated$Numerator[alpha] <-
    "4_mock_IFNy, 5_mock_IFNy, 6_mock_IFNy"

  annotated$Denominator[alpha] <-
    "1_mock, 2_mock, 3_mock"

  annotated$Numerator[alpha2] <-
    "7_Capsid, 8_Capsid, 9_Capsid"

  annotated$Denominator[alpha2] <-
    "1_mock, 2_mock, 3_mock"

  target <- which(
    annotated$Content ==
      "Abundance Ratio"
  )

  signatures <- auto_regex_family_signature(
    annotated,
    target
  )

  groups <- split(
    target,
    signatures,
    drop = TRUE
  )

  expect_equal(
    length(groups),
    2L
  )

  for (group_rows in groups) {
    selectors <-
      auto_regex_exact_family_selectors(
        annotated,
        group_rows
      )

    expect_gt(
      nrow(selectors),
      0L
    )

    for (i in seq_len(nrow(selectors))) {
      hit <- safe_grepl(
        selectors$Pattern[[i]],
        annotated$Column
      )

      expect_setequal(
        which(hit),
        group_rows,
        info = paste(
          "selector must reproduce exactly one complete structural family:",
          selectors$Pattern[[i]]
        )
      )
    }
  }

  recovered <-
    auto_regex_partition_recovery(
      annotated,
      "Abundance Ratio",
      condition_target = "Options"
    )

  expect_true(
    recovered$ok
  )

  expect_equal(
    nrow(recovered$content),
    2L
  )

  expect_equal(
    length(unique(
      recovered$content$VariantId
    )),
    2L
  )

  expect_equal(
    nrow(recovered$ratio),
    1L
  )

  expect_true(
    recovered$ratio$VariantId %in%
      recovered$content$VariantId
  )
})
