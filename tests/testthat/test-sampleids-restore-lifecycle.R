sampleids_observer_source <- function() {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  paste(readLines(file.path(root, "modules", "sampleids", "sampleids_observer.R"),
                  warn = FALSE), collapse = "\n")
}

testthat::test_that("SampleIDs restore uses cached pairs for dynamic UI choices", {
  source <- sampleids_observer_source()

  testthat::expect_match(source,
    "effective_sampleids_data_pair <- function\\(\\)[\\s\\S]*restore_plot_data_cache\\(\\)",
    perl = TRUE)
  testthat::expect_match(source,
    "effective_sampleids_data_pair <- function\\(\\)[\\s\\S]*restore_context_active\\(\\)",
    perl = TRUE)
  testthat::expect_match(source,
    "plot_creation_cache\\(\\)[\\s\\S]*plot_from_restore_cache\\(\\)", perl = TRUE)
  testthat::expect_match(source,
    "list\\(data_mod = rv\\$data_mod, data_def = rv\\$data_def\\)", perl = TRUE)

  # Readiness plus both dynamic-choice observers share the same resolver.
  calls <- unlist(regmatches(source, gregexpr("effective_sampleids_data_pair\\(\\)",
                                              source, perl = TRUE)))
  testthat::expect_gte(length(calls), 4L)
})

testthat::test_that("SampleIDs restore timeout hierarchy is module-local", {
  source <- sampleids_observer_source()

  testthat::expect_match(source,
    'register_job\\("SampleIDs", "poll and plot rebuild", "render", 30\\)')
  testthat::expect_match(source, "restore_poll_started_at\\(Sys.time\\(\\)\\)")
  testthat::expect_match(source, "difftime\\(Sys.time\\(\\), started_at")
  testthat::expect_match(source, "elapsed_seconds >= 15")
  testthat::expect_false(grepl("attempt > 20L", source, fixed = TRUE))
})

testthat::test_that("cached rebuild provenance survives payload release", {
  source <- sampleids_observer_source()

  testthat::expect_match(source,
    "used_restore_cache <- has_valid_restore_plot_data_cache\\(cache\\)")
  testthat::expect_match(source, paste0(
    "state\\$restore_plot_data_cache\\(NULL\\)[\\s\\S]{0,1600}",
    "state\\$plot_from_restore_cache\\(isTRUE\\(rebuild_ok\\) && isTRUE\\(used_restore_cache\\)\\)"),
    perl = TRUE)
  testthat::expect_match(source,
    "isTRUE\\(state\\$plot_from_restore_cache\\(\\)\\)\\) return\\(\\)")
})
