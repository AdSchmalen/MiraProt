# Documentation files are not independent: routing modules call content
# renderers defined in their companion user and technical files.  Keep that
# dependency visible rather than relying on list.files()'s alphabetical order.
miraprot_documentation_files <- c(
  "MiraProt_doc_user.R",
  "MiraProt_doc_tech.R"
)

miraprot_documentation_ui <- "MiraProt_doc_ui.R"

miraprot_renderer_references <- function(ui_file) {
  expressions <- parse(ui_file, keep.source = FALSE)
  symbols <- unique(all.names(expressions, functions = TRUE))
  sort(symbols[grepl("^render_.*_content$", symbols)])
}

assert_miraprot_documentation_renderers <- function(ui_file, envir) {
  required <- miraprot_renderer_references(ui_file)
  missing <- required[!vapply(required, function(symbol) {
    exists(symbol, envir = envir, inherits = FALSE) &&
      is.function(get(symbol, envir = envir, inherits = FALSE))
  }, logical(1))]

  if (length(missing) > 0) {
    stop(
      sprintf(
        paste0(
          "Cannot register the MiraProt documentation server; required ",
          "content renderer(s) are missing or are not functions: %s. ",
          "Expected definitions from Documentation/MiraProt_doc_user.R and ",
          "Documentation/MiraProt_doc_tech.R before MiraProt_doc_ui.R."
        ),
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(required)
}

load_documentation_files <- function(directory = "Documentation", envir,
                                     logger = NULL) {
  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  by_name <- stats::setNames(files, basename(files))
  content_files <- unname(by_name[miraprot_documentation_files])
  content_files <- content_files[!is.na(content_files)]
  ui_file <- unname(by_name[miraprot_documentation_ui])
  ui_file <- ui_file[!is.na(ui_file)]
  other_files <- setdiff(files, c(content_files, ui_file))

  source_one <- function(file) {
    sys.source(file, envir = envir)
    if (is.function(logger)) {
      logger(paste("Loaded:", basename(file)), 2)
    }
  }

  for (file in content_files) {
    source_one(file)
  }

  if (length(ui_file) == 1L) {
    assert_miraprot_documentation_renderers(ui_file, envir)
  }

  # All unrelated documentation remains loaded; only the dependency-bearing
  # MiraProt router is deliberately deferred.
  for (file in other_files) {
    source_one(file)
  }
  if (length(ui_file) == 1L) {
    source_one(ui_file)
  }

  invisible(c(content_files, other_files, ui_file))
}
