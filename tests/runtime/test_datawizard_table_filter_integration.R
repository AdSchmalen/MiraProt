library(testthat)

logic_path <- file.path("modules", "Data Wizard", "tables", "datawizard_tables_logic.R")
if (!file.exists(logic_path)) logic_path <- file.path("..", "..", logic_path)
source(logic_path, local = TRUE)

table_roles <- c(primary = "primary", additional = "secondary")

# DataTables' top filters treat numeric columns as numbers.  Keeping this
# small model in the test makes the expected result sets explicit without
# depending on a browser or reproducing DataTables' implementation in app code.
filter_rows <- function(df, text = NULL, numeric_range = NULL) {
  keep <- rep(TRUE, nrow(df))
  if (!is.null(text)) {
    keep <- keep & grepl(text$value, df[[text$column]], fixed = TRUE)
  }
  if (!is.null(numeric_range)) {
    values <- df[[numeric_range$column]]
    stopifnot(is.numeric(values))
    keep <- keep & values >= numeric_range$lower & values <= numeric_range$upper
  }
  df[keep, , drop = FALSE]
}

test_that("presentation truncation keeps canonical types and searchable suffixes", {
  suffix <- "match-after-visual-boundary"
  canonical <- data.frame(
    amount = c(1.25, 10.5, 100.75),
    count = c(1L, 10L, 100L),
    enabled = c(TRUE, FALSE, TRUE),
    date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    datetime = as.POSIXct(c("2026-01-01", "2026-01-02", "2026-01-03"), tz = "UTC"),
    label = c("short", paste0(strrep("x", 80), suffix), "other"),
    check.names = FALSE
  )

  preview <- build_data_preview(canonical)$data
  expect_identical(preview, canonical)
  expect_identical(truncate_text(canonical), canonical)
  expect_identical(filter_rows(preview,
    numeric_range = list(column = "amount", lower = 10, upper = 11))$amount, 10.5)
  expect_identical(filter_rows(preview,
    text = list(column = "label", value = suffix))$amount, 10.5)

  renderer <- build_datawizard_text_renderer(50L)
  expect_match(renderer, "type!=='display'", fixed = TRUE)
  expect_match(renderer, "return data", fixed = TRUE)
  expect_match(renderer, "textContent=value", fixed = TRUE)
  options <- build_datawizard_table_options(NULL, renderer, 5L)
  expect_identical(options$columnDefs[[1]]$targets, 5L)
  expect_identical(options$columnDefs[[1]]$render, renderer)
})

for (table_name in names(table_roles)) local({
  role <- table_roles[[table_name]]

  test_that(paste(table_name, "120-row table pages and filters the complete client payload"), {
    canonical <- data.frame(
      `Row Index` = seq_len(120),
      amount = seq_len(120),
      label = c(rep("ordinary", 87), "after-page-one", rep("ordinary", 32)),
      check.names = FALSE
    )
    snapshot <- build_table_display_snapshot(canonical, role, 1L)
    options <- build_datawizard_table_options(NULL)

    expect_identical(options$pageLength, DATAWIZARD_TABLE_PAGE_LENGTH)
    expect_equal(nrow(snapshot$visible_slice[seq_len(options$pageLength), , drop = FALSE]), 50L)
    expect_identical(filter_rows(snapshot$complete_frame,
      text = list(column = "label", value = "after-page-one"))$`Row Index`, 88L)

    numeric_result <- filter_rows(snapshot$complete_frame,
      numeric_range = list(column = "amount", lower = 9, upper = 11))
    expect_identical(numeric_result$amount, 9:11)
    expect_false(100L %in% numeric_result$amount)
    expect_identical(snapshot$complete_frame, canonical)
  })

  test_that(paste(table_name, "server-side source searches all rows and is not capped at 100 results"), {
    canonical <- data.frame(
      `Row Index` = seq_len(750),
      amount = seq_len(750),
      label = c(rep("match-many", 149), rep("ordinary", 600), "near-boundary"),
      check.names = FALSE
    )
    snapshot <- build_table_display_snapshot(canonical, role, 2L)
    boundary <- filter_rows(snapshot$complete_frame,
      text = list(column = "label", value = "near-boundary"))
    many <- filter_rows(snapshot$complete_frame,
      text = list(column = "label", value = "match-many"))

    expect_true(snapshot$server_side)
    expect_identical(boundary$`Row Index`, 750L)
    expect_equal(nrow(many), 149L)
    expect_equal(ceiling(nrow(many) / build_datawizard_table_options(NULL)$pageLength), 3)
  })

  test_that(paste(table_name, "large data retains all rows in every display mode and clearing filters is lossless"), {
    canonical <- data.frame(
      `Row Index` = seq_len(900),
      amount = seq_len(900),
      label = c(rep("ordinary", 874), "beyond-preview", rep("ordinary", 25)),
      check.names = FALSE
    )
    bounded <- build_table_display_snapshot(canonical, role, 3L)
    full <- build_table_display_snapshot(canonical, role, 3L, show_full = TRUE)

    expect_identical(bounded$policy, "very_large")
    expect_equal(bounded$slice_rows, DATAWIZARD_TABLE_PAGE_LENGTH)
    expect_true(bounded$row_truncated)
    expect_false(bounded$full_requested)
    expect_true(full$server_side)
    expect_identical(filter_rows(full$visible_slice,
      text = list(column = "label", value = "beyond-preview"))$`Row Index`, 875L)

    # Clearing a top filter means displaying its unchanged source payload.
    bounded_filtered <- filter_rows(bounded$complete_frame,
      numeric_range = list(column = "amount", lower = 10, upper = 20))
    full_filtered <- filter_rows(full$visible_slice,
      numeric_range = list(column = "amount", lower = 10, upper = 20))
    expect_equal(nrow(bounded_filtered), 11L)
    expect_equal(nrow(full_filtered), 11L)
    expect_equal(nrow(bounded$visible_slice), DATAWIZARD_TABLE_PAGE_LENGTH)
    expect_equal(nrow(full$visible_slice), 900L)
    expect_identical(bounded$complete_frame, canonical)
    expect_identical(full$complete_frame, canonical)
  })
})

test_that("Shiny serializes both DT viewers with searchable 50-row pages", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  frames <- list(
    primary = data.frame(id = seq_len(120), label = c(rep("ordinary", 119), "primary-late")),
    additional = data.frame(id = seq_len(120), label = c(rep("ordinary", 119), "additional-late"))
  )

  shiny::testServer(function(input, output, session) {
    for (id in names(frames)) local({
      output_id <- id
      frame <- frames[[id]]
      output[[output_id]] <- DT::renderDT({
        DT::datatable(
          frame,
          rownames = FALSE,
          filter = "top",
          options = build_datawizard_table_options(NULL)
        )
      }, server = FALSE)
    })
  }, {
    primary_json <- output$primary
    additional_json <- output$additional
    expect_match(primary_json, "primary-late", fixed = TRUE)
    expect_match(additional_json, "additional-late", fixed = TRUE)
    expect_match(primary_json, '"pageLength":50', fixed = TRUE)
    expect_match(additional_json, '"pageLength":50', fixed = TRUE)
  })
})

test_that("both renderers retain filters, scrolling, and independent column controls", {
  observer_path <- file.path("modules", "Data Wizard", "tables", "datawizard_tables_observer.R")
  if (!file.exists(observer_path)) observer_path <- file.path("..", "..", observer_path)
  source_text <- paste(readLines(observer_path, warn = FALSE), collapse = "\n")
  occurrences <- function(needle) {
    positions <- gregexpr(needle, source_text, fixed = TRUE)[[1]]
    length(positions[positions > 0])
  }

  expect_identical(occurrences('filter = "top"'), 2L)
  expect_identical(occurrences("options = build_datawizard_table_options("), 2L)
  # Keep the public control IDs and their singular labels in lockstep. The
  # toggle changes the interaction/navigation presentation only; it must not
  # imply that complete-dataset searching is disabled in preview mode.
  for (control_id in c("primary_toggle_full_table", "additional_toggle_full_table")) {
    control_pattern <- paste0(
      'ns("', control_id, '"),\\s*',
      'if \\(snapshot\\$full_requested\\) "Return to preview" else ',
      '"Enable full-table interaction"'
    )
    expect_match(source_text, control_pattern, perl = TRUE)
  }
  expect_identical(occurrences('"Enable full-table interaction"'), 2L)
  expect_identical(occurrences('"Return to preview"'), 2L)

  options <- build_datawizard_table_options(NULL)
  expect_true(options$searching)
  expect_identical(options$pageLength, DATAWIZARD_TABLE_PAGE_LENGTH)
  expect_true(options$scrollX)
  expect_identical(options$scrollY, DATAWIZARD_TABLE_COMPACT_HEIGHT)
  expect_true(options$scrollCollapse)
  expect_true(options$deferRender)
  expect_true(options$processing)
})
