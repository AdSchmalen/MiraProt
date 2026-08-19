library(testthat)
library(shiny)

source("modules/Data Wizard/datawizard_core.R")

# Metadata notifications are an integration concern; keep this logic regression
# test independent of a live Shiny session.
showNotification <- function(...) invisible(NULL)

.metadata_row <- function(column, content) {
  data.frame(
    Column = column,
    Content = content,
    Options = "replicate",
    Numerator = NA_character_,
    Denominator = NA_character_,
    Transformation = "None",
    Sample = column,
    stringsAsFactors = FALSE
  )
}

.exercise_metadata_update <- function(source_content, new_column, update_name) {
  original_column <- "source_column"
  metadata <- reactiveVal(.metadata_row(original_column, source_content))
  final_metadata <- reactiveVal(NULL)
  core_values <- list(
    handson_metadata = metadata,
    final_processed_metadata = final_metadata
  )
  updates <- create_metadata_update_functions(core_values)

  new_data <- data.frame(source = 1, transformed = 2, check.names = FALSE)
  names(new_data) <- c(original_column, new_column)
  isolate(updates[[update_name]](new_data))

  isolate(metadata())$Content[2]
}

test_that("batch correction creates canonical abundance content names", {
  expect_identical(
    .exercise_metadata_update(
      "Raw Abundance", "Batch Corrected source_column",
      "update_metadata_for_batch_corrected_columns"
    ),
    "Batch Corrected Raw Abundance"
  )
  expect_identical(
    .exercise_metadata_update(
      "Normalized Abundance", "Batch Corrected source_column",
      "update_metadata_for_batch_corrected_columns"
    ),
    "Batch Corrected Normalized Abundance"
  )
})

test_that("imputation creates canonical batch-corrected abundance content names", {
  expect_identical(
    .exercise_metadata_update(
      "Batch Corrected Raw Abundance", "Imputed source_column",
      "update_metadata_for_imputed_columns"
    ),
    "Imputed Batch Corrected Raw Abundance"
  )
  expect_identical(
    .exercise_metadata_update(
      "Batch Corrected Normalized Abundance", "Imputed source_column",
      "update_metadata_for_imputed_columns"
    ),
    "Imputed Batch Corrected Normalized Abundance"
  )
})
