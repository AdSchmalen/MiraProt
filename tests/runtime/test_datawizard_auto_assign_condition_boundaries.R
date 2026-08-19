#!/usr/bin/env Rscript

library(testthat)

source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")

create_test_engine <- function() {
  create_auto_assign_rule_engine(
    debug_log = function(...) NULL,
    add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = function() data.frame(),
    rv_condition_rules_autoassign_dw = function() data.frame(),
    rv_rules_autoassign_dw = function() data.frame(),
    rules_loaded_centrally = function() FALSE,
    extractedConds_autoassign_dw = function() character()
  )
}

apply_boundary_rule <- function(columns, position, before = NULL, after = NULL) {
  metadata <- data.frame(
    Column = columns,
    Content = rep("Raw Abundance", length(columns)),
    Options = rep(NA_character_, length(columns)),
    stringsAsFactors = FALSE
  )

  create_test_engine()$apply_condition_rule(
    metadata,
    lookup_col = "Raw Abundance",
    position = position,
    before = before,
    after = after,
    setter = function(...) NULL
  )
}

test_that("start rules do not fall back to a complete nonmatching header", {
  result <- apply_boundary_rule(
    c("control :: abundance", "unmatched complete header"),
    position = "start",
    after = " :: "
  )

  expect_identical(result$Options, c("control", NA_character_))
  expect_false("unmatched complete header" %in% result$Options)
})

test_that("end rules do not fall back to a complete nonmatching header", {
  result <- apply_boundary_rule(
    c("abundance :: treated", "unmatched complete header"),
    position = "end",
    before = " :: "
  )

  expect_identical(result$Options, c("treated", NA_character_))
  expect_false("unmatched complete header" %in% result$Options)
})

test_that("between rules preserve the strict nonmatching-boundary contract", {
  result <- apply_boundary_rule(
    c("abundance [vehicle] replicate", "unmatched complete header"),
    position = "between",
    before = "\\[",
    after = "\\]"
  )

  expect_identical(result$Options, c("vehicle", NA_character_))
  expect_false("unmatched complete header" %in% result$Options)
})
