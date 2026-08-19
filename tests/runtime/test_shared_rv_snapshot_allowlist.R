library(testthat)
library(shiny)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("R/session_save_restore/session_save_restore_orchestration.R")

test_that("full shared snapshots use an explicit allowlist", {
  rendered_plot <- structure(
    list(runtime_environment = new.env(parent = emptyenv())),
    class = "ggplot"
  )
  rv <- reactiveValues(
    data_mod = data.frame(protein = "P1"),
    data_def = data.frame(column = "protein"),
    data_mod_revision_id = 4L,
    gridplot_order = "pca_main",
    plot_spans = list(pca_main = list(colspan = 2L, rowspan = 1L)),
    plot_margins = list(pca_main = list(top = 2)),
    gridplot_selection = list(pca_main = list(
      plot = rendered_plot,
      label = "PCA",
      source = "pca",
      type = "ggplot",
      include_label = FALSE,
      added_at = Sys.time()
    )),
    restore_plot_data_cache = list(large = TRUE),
    restore_reports = list(pca = "runtime only"),
    restore_diagnostics = list(trace = TRUE),
    restore_telemetry = list(count = 1L),
    datawizard_restore_diagnostics = list(trace = TRUE),
    pca_restore_rebuild_expected = TRUE,
    pca_restore_selected_cache_key = "cache-key",
    go_pending_ui_restore = list(input = 1),
    gsea_pending_ui_restore = list(input = 2),
    session_restore_generation = 9L,
    session_restore_phase = "module_restore",
    session_restore_trigger = 3L,
    callback = function() NULL,
    observer = structure(list(), class = "Observer"),
    session = new.env(),
    arbitrary_module_state = list(should_not = "leak")
  )

  snapshot <- isolate(.build_rv_snapshot_for_save_level(rv, "full_session"))

  expect_true(all(c("data_mod", "data_def", "data_mod_revision_id",
                    "grid_session_payload") %in% names(snapshot)))
  expect_false(any(c(
    "restore_plot_data_cache", "restore_reports", "restore_diagnostics",
    "restore_telemetry", "datawizard_restore_diagnostics",
    "pca_restore_rebuild_expected", "pca_restore_selected_cache_key",
    "go_pending_ui_restore", "gsea_pending_ui_restore",
    "session_restore_generation", "session_restore_phase",
    "session_restore_trigger", "callback", "observer", "session",
    "arbitrary_module_state", "gridplot_order", "plot_spans", "plot_margins",
    "gridplot_selection"
  ) %in% names(snapshot)))

  entry <- snapshot$grid_session_payload[[1L]]
  expect_identical(entry$display_label, "PCA")
  expect_false(entry$include_label)
  expect_identical(entry$source_module, "pca")
  expect_identical(entry$stable_plot_id, "pca_main")
  expect_false("plot" %in% names(entry))
  expect_false("added_at" %in% names(entry))
})

test_that("size inventory keeps restore/export runtime state non-dominant", {
  large_runtime_value <- rep.int("runtime-only", 50000L)
  restored_rv <- reactiveValues(
    data_mod = data.frame(protein = sprintf("P%05d", seq_len(2000L))),
    data_def = data.frame(column = "protein"),
    gridplot_order = "pca_main",
    gridplot_selection = list(pca_main = list(
      plot = structure(list(), class = "ggplot"),
      label = "PCA", source = "pca"
    )),
    restore_plot_data_cache = large_runtime_value,
    restore_diagnostics = large_runtime_value,
    datawizard_restore_diagnostics = large_runtime_value
  )

  first_export <- isolate(.build_rv_snapshot_for_save_level(
    restored_rv, "full_session"
  ))
  # Simulate Grid's restore of stable metadata before exporting once more.
  restored_grid <- setNames(lapply(first_export$grid_session_payload, function(x) {
    list(label = x$display_label, source = x$source_module,
         source_plot_id = x$source_plot_id, include_label = x$include_label)
  }), vapply(first_export$grid_session_payload, `[[`, character(1L),
             "stable_plot_id"))
  round_trip_state <- first_export
  round_trip_state$grid_session_payload <- NULL
  round_trip_state$gridplot_order <- names(restored_grid)
  round_trip_state$gridplot_selection <- restored_grid
  round_trip_rv <- do.call(reactiveValues, round_trip_state)
  second_export <- isolate(.build_rv_snapshot_for_save_level(
    round_trip_rv, "full_session"
  ))
  inventory <- .rv_snapshot_size_inventory(second_export)

  forbidden <- c(
    "gridplot_selection", "restore_plot_data_cache", "restore_diagnostics",
    "datawizard_restore_diagnostics"
  )
  expect_false(any(forbidden %in% inventory$field))
  expect_true(all(inventory$contract == "kept"))
  expect_false(any(inventory$contains_runtime_plot))
  expect_identical(inventory$field[[1L]], "data_mod")
  expect_lt(
    inventory$size_bytes[inventory$field == "grid_session_payload"],
    inventory$size_bytes[inventory$field == "data_mod"]
  )
})
