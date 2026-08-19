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

serialize_bytes <- function(value) {
  serialize(value, connection = NULL, version = 3L)
}

assign_conditions <- function(engine, sample_names) {
  metadata <- data.frame(
    Column = c("batch::control::A", "batch::treated::B", "accession"),
    Content = c("Raw Abundance", "Raw Abundance", "Identifier"),
    Options = c("stale-control", "stale-treated", "UniProt"),
    Sample = sample_names,
    Numerator = c("numerator-a", "numerator-b", "numerator-id"),
    Denominator = c("denominator-a", "denominator-b", "denominator-id"),
    Transformation = c("log2", "log2", "None"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  engine$apply_condition_rule(
    metadata,
    lookup_col = "Raw Abundance",
    position = "between",
    before = "::",
    after = "::",
    setter = function(...) NULL
  )
}

test_that("sample finalization changes only missing Sample cells", {
  engine <- create_test_engine()
  assigned <- assign_conditions(engine, c("curated-control", NA_character_, NA_character_))
  metadata_columns <- c(
    "Column", "Content", "Options", "Numerator", "Denominator",
    "Transformation"
  )

  # This is the phase boundary under test: conditions have populated Options,
  # while sample-name finalization has not run yet.
  metadata_snapshot <- lapply(assigned[metadata_columns], serialize_bytes)
  sample_snapshot <- assigned$Sample

  finalized <- engine$finalize_condition_sample_names(assigned)

  for (column in metadata_columns) {
    expect_identical(
      serialize_bytes(finalized[[column]]),
      metadata_snapshot[[column]],
      info = paste(column, "changed during sample-name finalization")
    )
  }

  changed_cells <- which(
    vapply(
      seq_along(sample_snapshot),
      function(index) !identical(finalized$Sample[[index]], sample_snapshot[[index]]),
      logical(1)
    )
  )
  expect_true(length(changed_cells) > 0L)
  expect_true(all(is.na(sample_snapshot[changed_cells])))
  expect_identical(finalized$Sample[[1L]], sample_snapshot[[1L]])
  expect_false(is.na(finalized$Sample[[2L]]))
  expect_identical(finalized$Sample[[3L]], sample_snapshot[[3L]])
})

test_that("sample finalization is byte-identical when every Sample is populated", {
  engine <- create_test_engine()
  assigned <- assign_conditions(
    engine,
    c("curated-control", "curated-treated", "curated-identifier")
  )
  complete_metadata_snapshot <- serialize_bytes(assigned)

  finalized <- engine$finalize_condition_sample_names(assigned)

  expect_identical(serialize_bytes(finalized), complete_metadata_snapshot)
})
