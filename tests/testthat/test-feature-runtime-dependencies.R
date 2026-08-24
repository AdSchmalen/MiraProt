repo_file <- function(...) {
  path <- file.path(...)
  if (!file.exists(path)) path <- file.path("..", "..", path)
  path
}

read_manifest_vector <- function(path, variable) {
  expressions <- parse(file = path)
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name(variable))) {
      return(eval(expression[[3L]], envir = baseenv()))
    }
  }
  stop("Could not find ", variable, " in ", path)
}

test_that("required runtime manifests have package parity", {
  manifests <- c(
    repo_file("install.R"),
    repo_file("update.R"),
    repo_file("portable", "scripts", "install-packages.R")
  )
  bioc <- lapply(manifests, read_manifest_vector, variable = "bioc_packages")
  cran <- lapply(manifests, read_manifest_vector, variable = "cran_packages")

  expect_true(all(vapply(bioc, function(x) "ComplexHeatmap" %in% x, logical(1))))
  expect_true(all(vapply(cran, function(x) "europepmc" %in% x, logical(1))))
  expect_true(all(vapply(cran, function(x) "qs2" %in% x, logical(1))))
  expect_true(all(vapply(bioc[-1L], setequal, logical(1), bioc[[1L]])))
  expect_true(all(vapply(cran[-1L], setequal, logical(1), cran[[1L]])))
})

test_that("feature dependency checks provide explicit diagnostics", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "feature_dependencies.R"), envir = env)
  unavailable <- function(package, quietly = TRUE) FALSE

  expect_error(
    env$require_feature_dependency(
      "ComplexHeatmap",
      "Heatmap restore requires the 'ComplexHeatmap' package, but it is not installed.",
      unavailable
    ),
    "Heatmap restore requires the 'ComplexHeatmap' package, but it is not installed.",
    fixed = TRUE
  )
  expect_error(
    env$require_pubmed_plot_dependency(unavailable),
    "PubMed citation plots require the 'europepmc' package, but it is not installed.",
    fixed = TRUE
  )
})

test_that("Heatmap creation and restore preflight ComplexHeatmap", {
  env <- new.env(parent = globalenv())
  env$require_feature_dependency <- function(package, message, ...) stop(message, call. = FALSE)
  sys.source(repo_file("modules", "Heatmap", "Heatmap_create_expression.R"), envir = env)

  expect_error(env$heatmap_create_expression_heatmap(silent_restore = FALSE),
               "Heatmap creation requires the 'ComplexHeatmap' package", fixed = TRUE)
  expect_error(env$heatmap_create_expression_heatmap(silent_restore = TRUE),
               "Heatmap restore requires the 'ComplexHeatmap' package", fixed = TRUE)
})

test_that("GO and GSEA PubMed constructors preflight before pmcplot", {
  env <- new.env(parent = globalenv())
  env$require_pubmed_plot_dependency <- function(...) {
    stop("PubMed citation plots require the 'europepmc' package, but it is not installed.",
         call. = FALSE)
  }
  sys.source(repo_file("modules", "GO", "GO_module_plots.R"), envir = env)
  sys.source(repo_file("modules", "GSEA", "GSEA_plots.R"), envir = env)

  expect_error(
    env$create_go_pubmed_plot("GO:0000001", NULL, NULL, NULL),
    "PubMed citation plots require the 'europepmc' package, but it is not installed.",
    fixed = TRUE
  )
  expect_error(
    env$create_pubmed_plot("pathway", NULL, NULL, NULL),
    "PubMed citation plots require the 'europepmc' package, but it is not installed.",
    fixed = TRUE
  )

  expect_identical(env$create_go_pubmed_plot(character(), NULL, NULL, NULL)$message,
                   "Select GO terms for PubMed analysis")
  expect_identical(env$create_pubmed_plot(character(), NULL, NULL, NULL)$message,
                   "Select pathways for PubMed analysis")
})

test_that("feature packages are not attached by bootstrap", {
  bootstrap <- paste(readLines(repo_file("R", "bootstrap.R")), collapse = "\n")
  expect_false(grepl('"ComplexHeatmap"', bootstrap, fixed = TRUE))
  expect_false(grepl('"europepmc"', bootstrap, fixed = TRUE))
})
