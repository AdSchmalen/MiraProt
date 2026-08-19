library(testthat)
library(shiny)

source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_reactive_state.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_template_pipeline.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")

empty_rules <- function() list(
  table = create_auto_assign_empty_table_rules(),
  condition = create_auto_assign_empty_condition_rules(),
  ratio = create_auto_assign_empty_ratio_rules()
)

pipeline_context <- function(fail_update = FALSE) {
  e <- new.env(parent = globalenv())
  e$rv_table_rules_autoassign_dw <- reactiveVal(create_auto_assign_empty_table_rules())
  e$rv_condition_rules_autoassign_dw <- reactiveVal(create_auto_assign_empty_condition_rules())
  e$rv_rules_autoassign_dw <- reactiveVal(create_auto_assign_empty_ratio_rules())
  e$rules_loaded_centrally <- reactiveVal(FALSE)
  e$current_loaded_rules <- reactiveVal(NULL)
  e$session <- list()
  e$debug_log <- function(...) NULL
  e$logs <- list()
  e$add_processing_log <- function(...) e$logs[[length(e$logs) + 1L]] <- list(...)
  e$notifications <- list()
  e$showNotification <- function(...) {
    e$notifications[[length(e$notifications) + 1L]] <- list(...)
    NULL
  }
  updater <- function(...) if (fail_update) stop("editor update failed") else NULL
  for (name in c("updateSelectInput", "updateTextInput", "updateCheckboxGroupInput",
                 "updateNumericInput", "updateCheckboxInput")) e[[name]] <- updater
  e$apply_filter_template <- function(...) TRUE
  e$apply_ratio_configurations <- function(...) TRUE
  e$apply_edit_operations <- function(...) TRUE
  e$apply_imputation_ui_config <- function(...) TRUE
  e$set_basemean_ui_config <- function(...) TRUE
  e$apply_pivot_ui_config <- function(...) TRUE
  e$filter_module <- e$ratios_module <- e$edit_module <- e$imputation_module <-
    e$basemean_module <- e$pivot_module <- NULL
  e$current_ui_config <- reactiveVal(NULL)
  e$last_import_info <- reactiveVal(NULL)
  e$template_loading_in_progress <- reactiveVal(FALSE)
  e$last_processing_time <- reactiveVal(NULL)
  e$module_health_status <- reactiveVal("OK")
  e
}

test_that("transaction accepts success and clears empty components", {
  ctx <- pipeline_context()
  pipeline <- create_auto_assign_template_pipeline(ctx)
  rules <- empty_rules()
  rules$table <- data.frame(Content = "Raw Abundance", Include = "Raw", Exclude = "",
    Transformation = "log2", stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(pipeline$load_rules_directly(rules, notify = FALSE))
  expect_identical(ctx$rv_table_rules_autoassign_dw(), rules$table)
  expect_identical(ctx$rv_condition_rules_autoassign_dw(), rules$condition)
  expect_identical(ctx$rv_rules_autoassign_dw(), rules$ratio)
  expect_true(ctx$rules_loaded_centrally())
  expect_length(ctx$notifications, 0L)
})

test_that("partial failure rolls back exact public reactives", {
  ctx <- pipeline_context(fail_update = TRUE)
  old <- empty_rules()
  old$table <- data.frame(Content = "Protein ID", Include = "Accession", Exclude = "",
    Transformation = NA_character_, stringsAsFactors = FALSE, check.names = FALSE)
  ctx$rv_table_rules_autoassign_dw(old$table)
  ctx$current_loaded_rules(list(marker = "session"))
  incoming <- empty_rules()
  incoming$table <- data.frame(Content = "Raw Abundance", Include = "Raw", Exclude = "",
    Transformation = "None", stringsAsFactors = FALSE, check.names = FALSE)
  pipeline <- create_auto_assign_template_pipeline(ctx)
  expect_false(pipeline$load_rules_directly(incoming, notify = FALSE))
  expect_identical(ctx$rv_table_rules_autoassign_dw(), old$table)
  expect_identical(ctx$current_loaded_rules(), list(marker = "session"))
  expect_false(ctx$rules_loaded_centrally())
  expect_length(ctx$notifications, 0L)
})

test_that("invalid import cannot mutate manual edits or restored session state", {
  ctx <- pipeline_context()
  manual <- empty_rules()
  manual$condition <- data.frame(Content = "Raw Abundance", Method = "whole",
    Before = "", After = "", Separators = "", Pos = 1L,
    stringsAsFactors = FALSE, check.names = FALSE)
  ctx$rv_condition_rules_autoassign_dw(manual$condition)
  ctx$rules_loaded_centrally(TRUE)
  ctx$current_loaded_rules(list(restored = TRUE))
  pipeline <- create_auto_assign_template_pipeline(ctx)
  expect_false(pipeline$load_rules_directly(list(table = data.frame()), notify = FALSE))
  expect_identical(ctx$rv_condition_rules_autoassign_dw(), manual$condition)
  expect_true(ctx$rules_loaded_centrally())
  expect_identical(ctx$current_loaded_rules(), list(restored = TRUE))
})

test_that("canonical frames survive export and import serialization", {
  rules <- empty_rules()
  rules$ratio <- data.frame(Content = "Abundance Ratio", Method = "Position in String",
    Separators = "_", Invert = FALSE, NumBefore = NA_character_, NumAfter = NA_character_,
    DenBefore = NA_character_, DenAfter = NA_character_, NumPos = 1L, DenPos = 2L,
    stringsAsFactors = FALSE, check.names = FALSE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(rules, path)
  restored <- readRDS(path)
  expect_identical(restored, rules)
  ctx <- pipeline_context()
  expect_true(create_auto_assign_template_pipeline(ctx)$load_rules_directly(restored, notify = FALSE))
  expect_identical(ctx$rv_rules_autoassign_dw(), rules$ratio)
})

test_that("validated whitespace boundaries load and extract all referenced conditions", {
  rules <- empty_rules()
  rules$table <- data.frame(Content = "Row Index", Include = "Row Index", Exclude = "",
    Transformation = NA_character_, stringsAsFactors = FALSE, check.names = FALSE)
  rules$condition <- data.frame(
    Content = c("Raw Abundance", "Normalized Abundance"), Method = c("end", "end"),
    Before = c(" ", ", "), After = c("", ""), Separators = c("", ""),
    Pos = c(1L, 1L), stringsAsFactors = FALSE, check.names = FALSE)
  samples <- LETTERS[1:6]
  metadata <- data.frame(
    Column = c(paste("Raw", samples), paste0("Normalized, ", samples)),
    Content = rep(rules$condition$Content, each = length(samples)),
    stringsAsFactors = FALSE, check.names = FALSE)

  applied <- apply_condition_table(metadata, rules$condition, rep(samples, 2L))
  expect_true(all(applied$diagnostics$ExactMatch))
  expect_length(validate_export(rules, metadata), 0L)

  ctx <- pipeline_context()
  expect_true(create_auto_assign_template_pipeline(ctx)$load_rules_directly(rules, notify = FALSE))
  expect_identical(ctx$rv_condition_rules_autoassign_dw(), rules$condition)
  expect_identical(ctx$rv_condition_rules_autoassign_dw()$Before[[1L]], " ")
  loaded <- apply_condition_table(metadata, ctx$rv_condition_rules_autoassign_dw(), rep(samples, 2L))
  expect_identical(loaded$metadata$Options, rep(samples, 2L))
})
