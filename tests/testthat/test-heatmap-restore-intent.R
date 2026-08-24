heatmap_module_file <- file.path("modules", "Heatmap_module.R")
if (!file.exists(heatmap_module_file)) {
  heatmap_module_file <- file.path("..", "..", heatmap_module_file)
}

find_heatmap_restore_intent <- function(node) {
  if (is.call(node) && identical(node[[1L]], as.name("<-")) &&
      identical(node[[2L]], as.name("had_heatmap")) &&
      grepl("matrix_payload", paste(deparse(node[[3L]]), collapse = " "), fixed = TRUE)) {
    return(node[[3L]])
  }
  if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) return(NULL)
  for (child in as.list(node)) {
    found <- find_heatmap_restore_intent(child)
    if (!is.null(found)) return(found)
  }
  NULL
}

find_named_assignment <- function(node, name) {
  if (is.call(node) && identical(node[[1L]], as.name("<-")) &&
      identical(node[[2L]], as.name(name))) {
    return(node[[3L]])
  }
  if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) return(NULL)
  for (child in as.list(node)) {
    found <- find_named_assignment(child, name)
    if (!is.null(found)) return(found)
  }
  NULL
}

test_that("Heatmap explicit no-plot intent overrides stale matrix residue", {
  intent <- find_heatmap_restore_intent(parse(heatmap_module_file))
  expect_false(is.null(intent))

  evaluate_intent <- function(state) eval(intent, envir = list(state = state))
  residue <- matrix(1, nrow = 1L)

  expect_false(evaluate_intent(list(
    had_heatmap = FALSE,
    heatmap_expression_matrix = residue,
    matrix_payload = list(expression_matrix = residue)
  )))
  expect_true(evaluate_intent(list(had_heatmap = TRUE)))
  expect_true(evaluate_intent(list(matrix_payload = list(expression_matrix = residue))))
})

test_that("restored heatmap survives input echoes until manual Create enables live context", {
  lifecycle_file <- file.path("modules", "Heatmap", "Heatmap_observers_plot_lifecycle.R")
  if (!file.exists(lifecycle_file)) lifecycle_file <- file.path("..", "..", lifecycle_file)
  lifecycle <- parse(lifecycle_file)
  trigger_expr <- find_named_assignment(lifecycle, "trigger_live_heatmap_rebuild")
  expect_false(is.null(trigger_expr))

  input <- new.env(parent = emptyenv())
  input$create_heatmap_btn <- NULL
  input$custom_col_sel_heatmap <- "Normalized Abundance"
  input$select_samples_heatmap <- c("restored-sample")
  restored_expr <- structure(matrix(42, nrow = 1L), context = "restored-matrix")
  plots <- list(expr = restored_expr)
  messages <- character()
  rebuild_sources <- character()

  env <- new.env(parent = globalenv())
  env$input <- input
  env$heatmap_state <- list(restore_in_progress = FALSE)
  env$heatmap_plots <- function(value) {
    if (missing(value)) return(plots)
    plots <<- value
  }
  env$heatmap_debug_log <- function(message, level) messages <<- c(messages, message)
  env$should_log_no_prior_heatmap_skip <- function(...) TRUE
  env$should_disable_live_heatmap_updates <- function() list(disable = FALSE)
  env$run_heatmap_creation <- function(trigger_source) {
    rebuild_sources <<- c(rebuild_sources, trigger_source)
    plots <<- list(expr = structure(matrix(99, nrow = 1L), context = "live"))
  }
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  eval(call("<-", as.name("trigger_live_heatmap_rebuild"), trigger_expr), env)

  # Restore cascades are authoritative even when both later gates would apply.
  env$heatmap_state$restore_in_progress <- TRUE
  expect_false(env$trigger_live_heatmap_rebuild("restore echo"))
  expect_match(tail(messages, 1L), "restore in progress", fixed = TRUE)

  # Once restore completes, an input echo still cannot replace the restored
  # matrix until Create/Refresh has been explicitly used in this session.
  env$heatmap_state$restore_in_progress <- FALSE
  expect_false(env$trigger_live_heatmap_rebuild("input echo"))
  expect_identical(plots$expr, restored_expr)
  expect_length(rebuild_sources, 0L)
  expect_match(tail(messages, 1L), "restored heatmap preserved; live updates require manual Create/Refresh", fixed = TRUE)

  # The genuinely empty plot gate retains its separate diagnostic.
  plots <- list(expr = NULL)
  expect_false(env$trigger_live_heatmap_rebuild("empty state"))
  expect_match(tail(messages, 1L), "no prior heatmap", fixed = TRUE)

  # A manual Create count unlocks subsequent updates, which rebuild from the
  # live path rather than retaining the restored matrix context.
  plots <- list(expr = restored_expr)
  input$create_heatmap_btn <- 1L
  expect_true(env$trigger_live_heatmap_rebuild("post-create input"))
  expect_identical(rebuild_sources, "live:post-create input")
  expect_identical(attr(plots$expr, "context"), "live")
})
