library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")

quiet_logger <- function(...) invisible(NULL)

write_template_fixture <- function(dataset) {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  auto_regex_write_metadata_template(path, dataset, quiet_logger)
  path
}

read_workbook_shape <- function(path) {
  workbook <- openxlsx::loadWorkbook(path)
  list(names = openxlsx::getSheetNames(path),
       visibility = openxlsx::sheetVisibility(workbook))
}

test_that("metadata workbook without Instructions hides only its helper sheet", {
  path <- write_template_fixture(data.frame(Sample_A = 1, check.names = FALSE))
  shape <- read_workbook_shape(path)

  expect_identical(shape$names, c("Metadata", "Content Choices"))
  expect_length(shape$visibility, length(shape$names))
  expect_identical(unname(shape$visibility), c("visible", "hidden"))
})

test_that("metadata workbook with Instructions hides only its helper sheet", {
  dataset <- data.frame(first = 1, second = 2, check.names = FALSE)
  names(dataset) <- c("duplicate", "duplicate")
  path <- write_template_fixture(dataset)
  shape <- read_workbook_shape(path)

  expect_identical(shape$names,
                   c("Metadata", "Instructions", "Content Choices"))
  expect_length(shape$visibility, length(shape$names))
  expect_identical(unname(shape$visibility),
                   c("visible", "visible", "hidden"))
})

test_that("metadata Content validation is written to the saved workbook", {
  path <- write_template_fixture(data.frame(Sample_A = 1, check.names = FALSE))
  extracted <- tempfile("metadata-workbook-")
  dir.create(extracted)
  utils::unzip(path, files = "xl/worksheets/sheet1.xml", exdir = extracted)
  worksheet_xml <- paste(readLines(
    file.path(extracted, "xl/worksheets/sheet1.xml"), warn = FALSE),
    collapse = "")

  expect_match(worksheet_xml, "<dataValidations", fixed = TRUE)
  expect_match(worksheet_xml, "Content Choices", fixed = TRUE)
})

test_that("visibility helper supports older whole-vector replacement behavior", {
  workbook <- new.env(parent = emptyenv())
  workbook$visibility <- stats::setNames(
    c("visible", "visible", "visible"),
    c("Metadata", "Instructions", "Content Choices"))
  submitted_length <- 0L
  older_api <- list(
    get = function(wb) wb$visibility,
    set = function(wb, value) {
      submitted_length <<- length(value)
      wb$visibility <- value
      wb
    }
  )

  result <- auto_regex_set_sheet_visibility(
    workbook, "Content Choices", visibility_api = older_api)

  expect_identical(submitted_length, 3L)
  expect_length(result, 3L)
  expect_identical(unname(workbook$visibility),
                   c("visible", "visible", "hidden"))
})

test_that("metadata writer fails clearly when openxlsx is unavailable", {
  messages <- character()
  logger <- function(message, level) messages <<- c(messages, message)
  path <- tempfile(fileext = ".xlsx")

  expect_error(auto_regex_write_metadata_template(
    path, data.frame(Sample_A = 1), logger, openxlsx_available = FALSE),
    "openxlsx.*unavailable")
  expect_false(file.exists(path))
  expect_true(any(grepl("openxlsx.*unavailable", messages)))
})
