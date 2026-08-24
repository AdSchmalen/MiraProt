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
