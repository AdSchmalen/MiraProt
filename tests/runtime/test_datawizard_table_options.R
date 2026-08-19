library(testthat)

logic_path <- file.path("modules", "Data Wizard", "tables", "datawizard_tables_logic.R")
source(logic_path, local = TRUE)

test_that("Data Wizard viewers share the intended DataTables policy", {
  callback <- structure("browser-ready", class = "callback")
  options <- build_datawizard_table_options(callback)

  expect_identical(options$pageLength, 50L)
  expect_true(options$paging)
  expect_true(options$searching)
  expect_true(options$scrollX)
  expect_identical(options$scrollY, DATAWIZARD_TABLE_COMPACT_HEIGHT)
  expect_identical(DATAWIZARD_TABLE_COMPACT_HEIGHT, "180px")
  expect_true(options$scrollCollapse)
  expect_true(options$deferRender)
  expect_true(options$processing)
  expect_false(options$ordering)
  expect_true(options$autoWidth)
  expect_false(options$destroy)
  expect_identical(options$dom, "tip")
  expect_identical(options$initComplete, callback)
  expect_false("lengthMenu" %in% names(options))
})

test_that("both table renderers use the shared DataTables policy", {
  observer_path <- file.path(
    "modules", "Data Wizard", "tables", "datawizard_tables_observer.R"
  )
  observer_source <- paste(readLines(observer_path, warn = FALSE), collapse = "\n")

  uses <- gregexpr(
    "options = build_datawizard_table_options(",
    observer_source,
    fixed = TRUE
  )[[1]]
  expect_length(uses[uses > 0], 2L)

  top_filters <- gregexpr(
    'filter = "top"',
    observer_source,
    fixed = TRUE
  )[[1]]
  expect_length(top_filters[top_filters > 0], 2L)

  expect_match(observer_source, 'DTOutput(ns(primary_table_output_id()))', fixed = TRUE)
  expect_match(observer_source, 'DTOutput(ns(additional_table_output_id()))', fixed = TRUE)
  server_modes <- gregexpr("server = TRUE", observer_source, fixed = TRUE)[[1]]
  expect_length(server_modes[server_modes > 0], 2L)

  complete_frames <- gregexpr(
    "ensure_unique_preview_names(snapshot$complete_frame",
    observer_source,
    fixed = TRUE
  )[[1]]
  expect_length(complete_frames[complete_frames > 0], 2L)
})

test_that("metadata styling resolves a titled header within its own row", {
  ui_path <- file.path(
    "modules", "Data Wizard", "tables", "datawizard_tables_ui.R"
  )
  ui_source <- paste(readLines(ui_path, warn = FALSE), collapse = "\n")

  expect_match(ui_source, "var titledHeader = headers.find", fixed = TRUE)
  expect_match(
    ui_source,
    "!header.querySelector('input, select')",
    fixed = TRUE
  )
  expect_match(
    ui_source,
    "titledHeader.parentElement.children, titledHeader",
    fixed = TRUE
  )
})
