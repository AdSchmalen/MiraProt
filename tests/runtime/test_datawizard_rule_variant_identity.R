library(testthat)
library(shiny)

source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_reactive_state.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")
source("modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R")

content_rules <- data.frame(
  RuleId = c("content-low", "content-high"),
  Content = c("Abundance Ratio", "Abundance Ratio"),
  VariantId = c("ratio-a", "ratio-b"), Priority = c(2L, 9L),
  Include = c("^A_", "^B_"), Exclude = c("skip", "omit"),
  Transformation = c("log2", "none"), stringsAsFactors = FALSE
)

test_that("Content selection retains RuleId and otherwise chooses highest priority", {
  expect_identical(resolve_content_rule_id(content_rules, "Abundance Ratio", "content-low"),
                   "content-low")
  expect_identical(resolve_content_variant_id(content_rules, "Abundance Ratio", "content-low"),
                   "ratio-a")
  expect_identical(resolve_content_rule_id(content_rules, "Abundance Ratio"), "content-high")
  expect_identical(resolve_content_variant_id(content_rules, "Abundance Ratio"), "ratio-b")

  dependent <- data.frame(
    RuleId = c("condition-a", "condition-b"), Content = "Abundance Ratio",
    VariantId = c("ratio-a", "ratio-b"), stringsAsFactors = FALSE
  )
  expect_identical(resolve_variant_rule_id(dependent, "Abundance Ratio", "ratio-a"),
                   "condition-a")
  expect_identical(resolve_variant_rule_id(dependent, "Abundance Ratio", "ratio-b"),
                   "condition-b")
})

test_that("RuleId variants survive independent edit, delete, export and import", {
  edited <- content_rules
  edited$Include[edited$RuleId == "content-low"] <- "^A_EDITED_"
  expect_identical(edited$Include[edited$RuleId == "content-high"], "^B_")
  expect_identical(edited$Exclude[edited$RuleId == "content-high"], "omit")

  exported <- list(
    table = edited,
    condition = create_auto_assign_empty_condition_rules(),
    ratio = create_auto_assign_empty_ratio_rules()
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(exported, path)
  imported <- prepare_assign_rules_rule_envelope(readRDS(path))$rules_data
  expect_identical(imported$table, edited)

  replay_engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL, add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = reactiveVal(imported$table),
    rv_condition_rules_autoassign_dw = reactiveVal(imported$condition),
    rv_rules_autoassign_dw = reactiveVal(imported$ratio),
    rules_loaded_centrally = reactiveVal(TRUE),
    extractedConds_autoassign_dw = reactiveVal(character())
  )
  replay <- data.frame(Column = c("A_EDITED_sample", "B_sample"), Content = "",
    Options = NA_character_, Transformation = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(imported$table))) {
    rule <- imported$table[i, ]
    replay <- replay_engine$apply_rule_autoassign_dw(replay, rule$Content,
      rule$VariantId, rule$Include, rule$Exclude)
  }
  expect_identical(replay$Content, rep("Abundance Ratio", 2L))
  expect_identical(attr(replay, "variant_id"), c("ratio-a", "ratio-b"))

  remaining <- imported$table[imported$table$RuleId != "content-low", , drop = FALSE]
  expect_identical(remaining$RuleId, "content-high")
  expect_identical(remaining$VariantId, "ratio-b")
  expect_identical(remaining$Include, "^B_")
})

test_that("condition and ratio rules only touch their winning VariantId", {
  logs <- list()
  engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL,
    add_processing_log = function(...) logs[[length(logs) + 1L]] <<- list(...),
    rv_table_rules_autoassign_dw = reactiveVal(content_rules),
    rv_condition_rules_autoassign_dw = reactiveVal(create_auto_assign_empty_condition_rules()),
    rv_rules_autoassign_dw = reactiveVal(create_auto_assign_empty_ratio_rules()),
    rules_loaded_centrally = reactiveVal(FALSE),
    extractedConds_autoassign_dw = reactiveVal(character())
  )
  metadata <- data.frame(
    Column = c("A_control_treated", "B_case_reference"), Content = "",
    Options = NA_character_, stringsAsFactors = FALSE
  )
  metadata <- engine$apply_rule_autoassign_dw(
    metadata, "Abundance Ratio", "ratio-a", "^A_"
  )
  metadata <- engine$apply_rule_autoassign_dw(
    metadata, "Abundance Ratio", "ratio-b", "^B_"
  )

  conditioned <- engine$apply_condition_rule(
    metadata, "Abundance Ratio", "ratio-a", position = "whole",
    setter = function(x) NULL
  )
  expect_identical(conditioned$Options[[1L]], "A_control_treated")
  expect_true(is.na(conditioned$Options[[2L]]))

  ratio_rule <- canonical_rule_row(
    "ratio", RuleId = "ratio-rule-b", Content = "Abundance Ratio",
    VariantId = "ratio-b", Method = "Position in String", Separators = "_",
    Invert = FALSE, NumPos = 2L, DenPos = 3L
  )
  ratio_applied <- engine$apply_ratio_rules_fixed(conditioned, ratio_rule)
  expect_true(is.na(ratio_applied$Numerator[[1L]]))
  expect_true(is.na(ratio_applied$Denominator[[1L]]))
  expect_identical(ratio_applied$Numerator[[2L]], "case")
  expect_identical(ratio_applied$Denominator[[2L]], "reference")
})
