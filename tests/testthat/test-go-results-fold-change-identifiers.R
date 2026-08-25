test_that("GO fold changes preserve the filtered input identifier namespace", {
  logic_file <- file.path("modules", "GO", "GO_module_logic.R")
  if (!file.exists(logic_file)) logic_file <- file.path("..", "..", logic_file)

  test_env <- new.env(parent = globalenv())
  test_env$pairwise_termsim <- function(x) x
  sys.source(logic_file, envir = test_env)

  mock_class <- paste0("MockGoResult", sample.int(.Machine$integer.max, 1L))
  methods::setClass(
    mock_class,
    slots = c(
      gene = "character", result = "data.frame", organism = "character",
      ontology = "character", pAdjustMethod = "character",
      pvalueCutoff = "numeric", qvalueCutoff = "numeric", universe = "character"
    )
  )
  edo <- methods::new(
    mock_class,
    gene = c("100", "200"),
    result = data.frame(ID = "GO:1", p.adjust = 0.01),
    organism = "test", ontology = "BP", pAdjustMethod = "BH",
    pvalueCutoff = 0.05, qvalueCutoff = 0.2, universe = c("100", "200")
  )
  go_data <- data.frame(
    Gene = c(NA, "", "   ", " TP53 ", "BRCA1", "TP53", "NANOG"),
    Abundance = c(9, 8, 7, 1.25, NA, 4.5, -2),
    stringsAsFactors = FALSE
  )

  results <- test_env$create_go_results_list_direct(edo, go_data)

  expect_identical(names(results$go_data_FC), c("TP53", "BRCA1", "NANOG"))
  expect_equal(unname(results$go_data_FC), c(1.25, NA, -2))
  expect_identical(results$go_data, go_data)
})
