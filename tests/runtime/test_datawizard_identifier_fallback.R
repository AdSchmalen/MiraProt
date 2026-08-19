#!/usr/bin/env Rscript

library(testthat)
library(shiny)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")

test_that("identifier fallback matches only controlled lexical units", {
  stored <- identifier_fallback_pattern()
  runtime <- regex_from_miraprot_storage(stored, "content")

  positives <- c("PG.ProteinAccessions", "PG.Genes", "PG.ProteinNames", "Gene ID")
  negatives <- c("Capsid", "Capsid_IFNy", "peptide", "identity",
    "identification score")
  expect_true(all(grepl(runtime, positives, perl = TRUE)))
  expect_false(any(grepl(runtime, negatives, perl = TRUE)))
  expect_identical(regex_from_miraprot_storage(
    regex_to_miraprot_storage(runtime, "content"), "content"), runtime)
})

test_that("persisted fallback runs through the production auto-assign engine", {
  stored <- identifier_fallback_pattern()
  persisted <- regex_to_miraprot_storage(
    regex_from_miraprot_storage(stored, "content"), "content")
  rule <- canonical_rule_row("content", Content = "Identifier",
    VariantId = stable_variant_ids("Identifier"), Priority = 0L,
    Include = persisted, Exclude = "", Transformation = NA_character_)

  # Load the actual runtime helpers after constructing the persisted Auto RegEx
  # payload, then exercise the same engine entrypoint used by the module.
  source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
  source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")
  engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL, add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = reactiveVal(rule),
    rv_condition_rules_autoassign_dw = reactiveVal(data.frame()),
    rv_rules_autoassign_dw = reactiveVal(data.frame()),
    rules_loaded_centrally = reactiveVal(FALSE),
    extractedConds_autoassign_dw = reactiveVal(character()))
  metadata <- data.frame(
    Column = c("PG.ProteinAccessions", "PG.Genes", "PG.ProteinNames", "Gene ID",
      "Capsid", "Capsid_IFNy", "peptide", "identity", "identification score"),
    Content = "", Options = "", Transformation = "", Sample = "",
    stringsAsFactors = FALSE, check.names = FALSE)
  assigned <- isolate(engine$apply_auto_assign_rules(metadata))

  expect_identical(assigned$Content[seq_len(4L)], rep("Identifier", 4L))
  expect_identical(assigned$Content[-seq_len(4L)], rep("", 5L))
})
