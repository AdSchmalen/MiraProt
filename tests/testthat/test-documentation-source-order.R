testthat::test_that("MiraProt renderers exist before its server is registered", {
  loader_env <- new.env(parent = baseenv())
  sys.source(file.path("R", "documentation_loader.R"), envir = loader_env)

  ui_file <- file.path("Documentation", "MiraProt_doc_ui.R")
  required <- loader_env$miraprot_renderer_references(ui_file)
  testthat::expect_gt(length(required), 0L)

  source_order <- character()
  documentation_env <- new.env(parent = globalenv())
  loaded <- loader_env$load_documentation_files(
    envir = documentation_env,
    logger = function(message, level) {
      source_order <<- c(source_order, sub("^Loaded: ", "", message))
    }
  )

  ui_position <- match("MiraProt_doc_ui.R", basename(loaded))
  definition_positions <- match(
    c("MiraProt_doc_user.R", "MiraProt_doc_tech.R"),
    basename(loaded)
  )
  testthat::expect_true(all(definition_positions < ui_position))
  testthat::expect_true(all(vapply(required, function(symbol) {
    exists(symbol, envir = documentation_env, inherits = FALSE) &&
      is.function(get(symbol, envir = documentation_env, inherits = FALSE))
  }, logical(1))))
  testthat::expect_true(all(basename(loaded) %in% source_order))
})

testthat::test_that("missing MiraProt renderers produce an actionable diagnostic", {
  loader_env <- new.env(parent = baseenv())
  sys.source(file.path("R", "documentation_loader.R"), envir = loader_env)

  testthat::expect_error(
    loader_env$assert_miraprot_documentation_renderers(
      file.path("Documentation", "MiraProt_doc_ui.R"),
      new.env(parent = emptyenv())
    ),
    "Cannot register the MiraProt documentation server.*render_.*_content"
  )
})
