#!/usr/bin/env Rscript

source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")

debug_entries <- list()
processing_entries <- list()

engine <- create_auto_assign_rule_engine(
  debug_log = function(...) {
    debug_entries[[length(debug_entries) + 1L]] <<- list(...)
  },
  add_processing_log = function(...) {
    processing_entries[[length(processing_entries) + 1L]] <<- list(...)
  },
  rv_table_rules_autoassign_dw = function() data.frame(),
  rv_condition_rules_autoassign_dw = function() data.frame(),
  rv_rules_autoassign_dw = function() data.frame(),
  rules_loaded_centrally = function() FALSE,
  extractedConds_autoassign_dw = function() character()
)

result <- engine$apply_condition_autoassign_dw(
  data.frame(
    Column = "Sample A",
    Content = "Raw Abundance",
    stringsAsFactors = FALSE
  ),
  lookup_col = "Raw Abundance",
  position = "whole",
  setter = function(...) stop("application failed")
)

stopifnot(
  length(debug_entries) == 1L,
  identical(debug_entries[[1L]], list(
    "Error applying condition rule: application failed",
    1
  )),
  length(processing_entries) == 1L,
  identical(processing_entries[[1L]], list(
    "apply_condition_rule",
    "error",
    "application failed"
  )),
  identical(attr(result, "condition_rule_status")$status, "error")
)

cat("condition rule error logging regression check passed\n")
