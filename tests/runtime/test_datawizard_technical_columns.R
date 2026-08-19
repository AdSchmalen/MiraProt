library(testthat)

source("modules/Data Wizard/datawizard_utils.R")
source("modules/Data Wizard/datawizard_file_loader.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")

test_that("canonical Row Index is idempotent and legacy aliases migrate exactly", {
  original <- data.frame(A = letters[1:3], "Row Index" = 11:13,
    check.names = FALSE)
  once <- canonicalize_datawizard_column_names(original)
  twice <- canonicalize_datawizard_column_names(once)
  expect_identical(once, twice)
  expect_identical(names(once), c("A", "Row Index"))
  expect_identical(once[["Row Index"]], 11:13)

  legacy <- data.frame(A = letters[1:3], Row_Index = 11:13,
    check.names = FALSE)
  migrated <- canonicalize_datawizard_column_names(legacy)
  expect_identical(names(migrated), c("A", "Row Index"))
  expect_identical(migrated[["Row Index"]], 11:13)
})

test_that("conflicting alias and legitimate derived columns are preserved", {
  input <- data.frame("Row Index" = 1:2, Row_Index = 8:9,
    "row sample index" = c("x", "y"), Derived = c(3, 4), check.names = FALSE)
  result <- canonicalize_datawizard_column_names(input)
  expect_identical(names(result), names(input))
  expect_identical(result$Derived, input$Derived)
})

test_that("verified metadata aliases fold and duplicate keys fail readiness", {
  data <- data.frame(A = 1:2, "Row Index" = 1:2, check.names = FALSE)
  metadata <- data.frame(Column = c("A", "Row Index", "Row_Index"),
    Content = c("Identifier", "Row Index", "Row Index"), check.names = FALSE)
  migrated <- datawizard_migrate_metadata_technical_keys(metadata)
  expect_identical(migrated$Column, c("A", "Row Index"))
  expect_identical(auto_regex_current_readiness(migrated, data), "ready")

  duplicate <- rbind(migrated, migrated[2, , drop = FALSE])
  expect_identical(auto_regex_current_readiness(duplicate, data), "duplicate_column")
  diagnostic <- auto_regex_source_diagnostic(duplicate, data)
  expect_identical(diagnostic$active_columns, 2L)
  expect_identical(diagnostic$metadata_rows, 3L)
  expect_identical(diagnostic$technical, "Row Index")
  expect_identical(diagnostic$duplicates, "Row Index")
})
