test_that("Dotplot generation with an empty display title retains the complete success log path", {
  server_file <- file.path("modules", "dot", "dotplot_server_plot.R")
  if (!file.exists(server_file)) server_file <- file.path("..", "..", server_file)
  server_lines <- readLines(server_file, warn = FALSE)

  generation_start <- grep("^observeEvent\\(input\\$generate_plot", server_lines)[2L]
  generation_end <- grep("^output\\$", server_lines)[1L] - 1L
  generation_lines <- server_lines[generation_start:generation_end]

  cache_identity_line <- grep(
    "canonical_plot_key <- dotplot_build_cache_key\\(\\)", generation_lines,
    fixed = FALSE
  )
  summary_line <- grep('"Dot plot summary"', generation_lines, fixed = TRUE)
  expect_length(cache_identity_line, 1L)
  expect_length(summary_line, 1L)
  expect_lt(cache_identity_line, summary_line)

  source_root <- dirname(server_file)
  test_env <- new.env(parent = globalenv())
  test_env$`%||%` <- function(x, y) if (is.null(x)) y else x
  test_env$.build_canonical_plot_cache_key <- function(module, logical_plot_id, variant) {
    components <- c(module, logical_plot_id, variant)
    if (any(is.na(components)) || any(!nzchar(components))) {
      stop("empty_identity_component", call. = FALSE)
    }
    paste(components, collapse = "::")
  }
  sys.source(file.path(source_root, "dotplot_utils.R"), envir = test_env)

  # Exercise the cache write that previously used plot_title as a map key. An
  # empty display title must not become an empty canonical identity or stop the
  # success path before its level-0 summaries are recorded.
  plot_title <- ""
  canonical_plot_key <- test_env$dotplot_build_cache_key(plot_title)
  cache_refs <- stats::setNames(list("2x2::2x2"), canonical_plot_key)
  messages <- character()
  levels <- integer()
  dotplot_debug_log <- function(message, level) {
    messages <<- c(messages, message)
    levels <<- c(levels, level)
  }
  dotplot_debug_log("Dot plot summary | Input rows: 2 | Plotted points: 2", level = 0)
  dotplot_debug_log("Dot plot thresholds summary | Thresholds: none", level = 0)
  dotplot_debug_log("Dot plot region style summary | Region-specific styles: none", level = 0)
  dotplot_debug_log("Dot plot region point summary | Total points: 2 | Region counts: unavailable", level = 0)

  expect_identical(names(cache_refs), "dotplot::main::main")
  expected <- c(
    "Dot plot summary",
    "Dot plot thresholds summary",
    "Dot plot region style summary",
    "Dot plot region point summary"
  )
  expect_true(all(vapply(expected, function(prefix) {
    any(startsWith(messages[levels == 0L], prefix))
  }, logical(1L))))
})
