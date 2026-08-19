library(testthat)

logic_path <- file.path("modules", "Data Wizard", "tables", "datawizard_tables_logic.R")
if (!file.exists(logic_path)) logic_path <- file.path("..", "..", logic_path)
source(logic_path, local = TRUE)

snapshot <- function(x, full = FALSE, revision = 1L) {
  build_table_display_snapshot(x, "primary", revision, show_full = full)
}

test_that("snapshots retain canonical rows and bound the initial preview to 50", {
  small <- snapshot(data.frame(Row.Index = 1:10, value = 11:20, check.names = FALSE))
  expect_identical(small$policy, "small")
  expect_identical(small$complete_frame[[2]], 11:20)

  wide <- snapshot(as.data.frame(matrix(1, nrow = 2, ncol = 250)))
  expect_identical(wide$policy, "very_large")
  expect_equal(wide$slice_cols, DATAWIZARD_PREVIEW_MAX_COLS)

  tall <- snapshot(data.frame(`Row Index` = seq_len(50001), value = 1, check.names = FALSE))
  expect_identical(tall$policy, "very_large")
  expect_equal(tall$slice_rows, DATAWIZARD_TABLE_PAGE_LENGTH)
  expect_true(tall$row_truncated)
  expect_true(tall$server_side)

  proteomics <- snapshot(as.data.frame(matrix(0, nrow = 8719, ncol = 104)))
  expect_gt(proteomics$estimated_cells, DATAWIZARD_LARGE_CELL_COUNT)
  expect_identical(proteomics$policy, "very_large")
  expect_equal(dim(proteomics$visible_slice), c(DATAWIZARD_TABLE_PAGE_LENGTH, 104L))
  expect_equal(dim(proteomics$complete_frame), c(8719L, 104L))
})

test_that("serialized size bounds the preview but not the canonical filter source", {
  payload <- data.frame(value = rep(strrep("x", 10000), 1000), stringsAsFactors = FALSE)
  result <- build_table_display_snapshot(
    payload, "primary", 1L, large_cells = Inf, large_serialized_bytes = 1000
  )
  expect_identical(result$policy, "very_large")
  expect_equal(result$slice_rows, DATAWIZARD_TABLE_PAGE_LENGTH)
  expect_true(result$row_truncated)
  expect_equal(nrow(result$complete_frame), 1000L)
  expect_true(result$server_side)
})

test_that("duplicate names are repaired only in the visible slice", {
  original <- data.frame(a = 1:2, b = 3:4)
  names(original) <- c("duplicate", "duplicate")
  result <- snapshot(original)
  expect_identical(names(result$complete_frame), c("duplicate", "duplicate"))
  expect_identical(names(result$visible_slice), c("duplicate", "duplicate_dup_1"))
})

test_that("server-side sources retain canonical DT types and cell values", {
  long_text <- paste(rep("long", 20), collapse = "-")
  original <- data.frame(
    numeric = c(1.5, NA_real_),
    integer = c(1L, NA_integer_),
    logical = c(TRUE, NA),
    date = as.Date(c("2026-01-01", NA)),
    datetime = as.POSIXct(c("2026-01-01 12:00:00", NA), tz = "UTC"),
    character = c(long_text, NA_character_),
    factor = factor(c(long_text, NA_character_)),
    check.names = FALSE
  )

  result <- snapshot(original)
  visible <- result$visible_slice

  expect_type(visible$numeric, "double")
  expect_type(visible$integer, "integer")
  expect_type(visible$logical, "logical")
  expect_s3_class(visible$date, "Date")
  expect_s3_class(visible$datetime, "POSIXct")
  expect_type(visible$character, "character")
  expect_s3_class(visible$factor, "factor")
  expect_identical(visible$character[[1]], long_text)
  expect_identical(as.character(visible$factor[[1]]), long_text)
  expect_true(all(vapply(visible[2, ], is.na, logical(1))))
  expect_identical(result$complete_frame, original)
  expect_true(is.factor(result$complete_frame$factor))
})

test_that("full-table opt-in exposes the complete frame", {
  original <- as.data.frame(matrix(seq_len(900 * 205), nrow = 900, ncol = 205))
  bounded <- snapshot(original)
  full <- snapshot(original, full = TRUE)
  expect_equal(dim(bounded$visible_slice), c(DATAWIZARD_TABLE_PAGE_LENGTH, DATAWIZARD_PREVIEW_MAX_COLS))
  expect_identical(dim(full$visible_slice), dim(original))
  expect_true(full$full_requested)
})

test_that("preview, full, preview transitions remount filtered DT outputs", {
  original <- data.frame(
    `Row Index` = seq_len(900),
    abundance = seq_len(900),
    label = c(rep("ordinary", 899), "late-only-match"),
    check.names = FALSE
  )
  modes <- c(FALSE, TRUE, FALSE)
  snapshots <- lapply(modes, function(full) snapshot(original, full = full))
  output_ids <- Map(datawizard_table_output_id, "primary", modes)

  expect_identical(unlist(output_ids), c(
    "primary_table_preview_bounded",
    "primary_table_preview_full",
    "primary_table_preview_bounded"
  ))
  expect_identical(
    unlist(Map(datawizard_table_output_id, "additional", modes)),
    c("additional_table_preview_bounded", "additional_table_preview_full", "additional_table_preview_bounded")
  )
  expect_equal(vapply(snapshots, `[[`, integer(1), "slice_rows"), c(50L, 900L, 50L))
  expect_true(all(vapply(snapshots, `[[`, logical(1), "server_side")))

  # The bounded display excludes late rows, but every mode retains them in the
  # complete server-side source used by top filters.
  expect_false(any(snapshots[[1]]$visible_slice$label == "late-only-match"))
  expect_equal(which(snapshots[[1]]$complete_frame$label == "late-only-match"), 900L)
  expect_equal(which(snapshots[[2]]$visible_slice$label == "late-only-match"), 900L)
  expect_equal(which(snapshots[[2]]$visible_slice$abundance > 899), 900L)
  expect_false(any(snapshots[[3]]$visible_slice$label == "late-only-match"))

  long_late_label <- paste0("late-only-match-", strrep("x", 100))
  original$label[[900]] <- long_late_label
  expect_identical(snapshot(original, full = TRUE)$visible_slice$label[[900]], long_late_label)

  skip_if_not_installed("DT")
  widgets <- lapply(snapshots, function(x) DT::datatable(x$visible_slice, filter = "top"))
  expect_true(all(vapply(widgets, function(widget) identical(widget$x$filter, "top"), logical(1))))
})

test_that("row removal maps through Row Index and column removal leaves identities", {
  complete <- data.frame(`Row Index` = c(101, 205, 999), value = letters[1:3], check.names = FALSE)
  visible <- complete[c(3, 1), , drop = FALSE]
  expect_identical(resolve_preview_row_position(complete, visible, 1L), 3L)
  reduced <- complete[-resolve_preview_row_position(complete, visible, 1L), , drop = FALSE]
  expect_identical(reduced$`Row Index`, c(101, 205))
  expect_identical(complete[, setdiff(names(complete), "value"), drop = FALSE]$`Row Index`, c(101, 205, 999))
})

test_that("filtered DT selections remove exactly the late canonical row for both tables", {
  exercise_removal <- function(role) {
    complete <- data.frame(
      `Row Index` = sprintf("%s-%03d", role, seq_len(120)),
      value = seq_len(120),
      label = c(rep("ordinary", 87), "filtered-target", rep("ordinary", 32)),
      check.names = FALSE
    )
    for (full_interaction in c(FALSE, TRUE)) {
      snapshot_for_mode <- build_table_display_snapshot(
        complete, role, revision = 1L, show_full = full_interaction
      )
      visible <- snapshot_for_mode$visible_slice

      # A filtered display reports this as its first selected row, although its
      # source-frame position is 88. The browser callback supplies its identity.
      selected_identity <- complete$`Row Index`[[88L]]
      position <- resolve_preview_row_identity_position(complete, visible, selected_identity)
      mode <- if (full_interaction) "full interaction" else "preview"
      expect_identical(position, 88L, info = paste(role, mode))

      reduced <- complete[-position, , drop = FALSE]
      expect_equal(nrow(reduced), 119L, info = paste(role, mode))
      expect_false(selected_identity %in% reduced$`Row Index`, info = paste(role, mode))
      expect_identical(
        setdiff(complete$`Row Index`, reduced$`Row Index`),
        selected_identity,
        info = paste(role, mode)
      )
    }
  }

  exercise_removal("primary")
  exercise_removal("additional")
})

test_that("duplicate Row Index identities are rejected as ambiguous", {
  complete <- data.frame(`Row Index` = c("stable", "duplicate", "duplicate"), check.names = FALSE)
  expect_true(is.na(resolve_preview_row_identity_position(complete, complete, "duplicate")))
})

test_that("canonical selections normalize unordered and duplicate positions", {
  complete <- data.frame(value = seq_len(120))
  expect_identical(resolve_canonical_selected_positions(complete, c(80, 3, 80, 51)), c(3L, 51L, 80L))
  expect_identical(resolve_canonical_selected_positions(complete, integer(0)), integer(0))
})

test_that("canonical selections use durable identities after filtering", {
  complete <- data.frame(`Row Index` = paste0("row-", seq_len(120)), value = seq_len(120), check.names = FALSE)
  expect_identical(
    resolve_canonical_selected_positions(complete, c(2, 1), c("row-88", "row-105")),
    c(88L, 105L)
  )
})

test_that("canonical selections reject every unsafe selection atomically", {
  complete <- data.frame(`Row Index` = c("one", "duplicate", "duplicate"), check.names = FALSE)
  expect_error(resolve_canonical_selected_positions(complete, c(1, 2), c("one", "duplicate")), "ambiguous")
  expect_error(resolve_canonical_selected_positions(complete, 1, "deleted-row"), "missing or ambiguous")
  expect_error(resolve_canonical_selected_positions(complete, c(1, 4), c("one", "one")), "invalid or stale")
  expect_error(resolve_canonical_selected_positions(complete, c(1, NA)), "invalid or stale")
  expect_error(resolve_canonical_selected_positions(complete, c(1, Inf)), "invalid or stale")
  expect_error(resolve_canonical_selected_positions(complete, c(1, 0)), "invalid or stale")
})

test_that("canonical row identity does not depend on the bounded visible slice", {
  complete <- data.frame(`Row Index` = c("visible", "outside-preview"), check.names = FALSE)
  bounded <- complete[1, , drop = FALSE]

  expect_identical(
    resolve_preview_row_identity_position(complete, bounded, "outside-preview"),
    2L
  )
  expect_identical(resolve_canonical_row_identity_position(complete, "outside-preview"), 2L)
  expect_true(is.na(resolve_canonical_row_identity_position(complete, NA_character_)))
})

test_that("late metadata and restored snapshots never replace canonical data", {
  restored <- data.frame(`Row Index` = 1:900, abundance = seq_len(900), check.names = FALSE)
  before_metadata <- snapshot(restored, revision = "restored")
  metadata <- data.frame(Column = names(restored), Content = c("Row Index", "Raw Abundance"))
  expect_equal(nrow(metadata), ncol(before_metadata$complete_frame))
  after_metadata <- snapshot(before_metadata$complete_frame, revision = "restored")
  expect_identical(after_metadata$complete_frame, restored)
  expect_equal(after_metadata$slice_rows, DATAWIZARD_TABLE_PAGE_LENGTH)
})

test_that("table options use 50-row pages and a ten-row scroll viewport", {
  options <- build_datawizard_table_options(
    init_complete = "ready",
    pagination_callback = build_datawizard_pagination_callback(FALSE)
  )
  expect_identical(options$pageLength, 50L)
  expect_identical(options$scrollY, "340px")
  expect_match(options$drawCallback, "toggle\\(false\\|\\|filtered\\)")
  expect_match(build_datawizard_pagination_callback(TRUE), "toggle\\(true\\|\\|filtered\\)")
})
