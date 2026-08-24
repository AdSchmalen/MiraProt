observer_file <- file.path(
  "modules", "Data Wizard", "filtering", "datawizard_filtering_observers.R"
)
if (!file.exists(observer_file)) {
  observer_file <- file.path("..", "..", observer_file)
}

find_observer_handler <- function(marker) {
  expressions <- parse(observer_file, keep.source = FALSE)
  matches <- list()

  walk <- function(node) {
    if (!is.call(node)) return(invisible(NULL))
    if (identical(node[[1]], as.name("observeEvent")) &&
        grepl(marker, paste(deparse(node), collapse = "\n"), fixed = TRUE)) {
      matches[[length(matches) + 1L]] <<- node[[3]]
    }
    lapply(as.list(node)[-1L], walk)
    invisible(NULL)
  }
  lapply(expressions, walk)
  expect_length(matches, 1L)
  matches[[1L]]
}

new_observer_environment <- function() {
  env <- new.env(parent = globalenv())
  env$isolate <- function(value) value
  env$freezeReactiveValue <- function(...) invisible(NULL)
  env$debug_messages <- character(0)
  env$debug_log <- function(message, level) {
    env$debug_messages <- c(env$debug_messages, message)
  }
  env
}

test_that("empty refresh columns preserve the current selection and caches", {
  handler <- find_observer_handler("Error updating filter column choices:")
  env <- new_observer_environment()
  env$input <- list(filter_category_dw = "category", filter_column_dw = "old")
  env$filtering_refresh_snapshot <- function() list(columns = character(0))
  env$data_revision_signature <- function() "revision"
  env$filterChoicesCache <- local({ value <- "cached"; function(x) {
    if (missing(x)) value else value <<- x
  }})
  env$filterCategoryCache <- local({ value <- "cached-category"; function(x) {
    if (missing(x)) value else value <<- x
  }})
  env$updateSelectizeInput <- function(...) stop("must not update")

  expect_null(eval(handler, env))
  expect_identical(env$input$filter_column_dw, "old")
  expect_identical(env$filterChoicesCache(), "cached")
  expect_identical(env$filterCategoryCache(), "cached-category")
  expect_empty(env$debug_messages)
})

test_that("missing data exits the metadata observer without an error log", {
  handler <- find_observer_handler("Error in metadata observer:")
  env <- new_observer_environment()
  env$data <- function() NULL
  env$metadata_def <- function() stop("metadata must not be read")
  env$metadata_ready <- function() TRUE
  env$find_abundance_columns_with_priority <- function(...) stop("must not run")

  expect_null(eval(handler, env))
  expect_empty(env$debug_messages)
})

test_that("genuine column update exceptions retain their condition message", {
  handler <- find_observer_handler("Error updating filter column choices:")
  env <- new_observer_environment()
  env$input <- list(filter_category_dw = "", filter_column_dw = "a")
  env$session <- NULL
  env$filtering_refresh_snapshot <- function() {
    list(columns = "a", metadata_ready = FALSE, content_to_columns = list())
  }
  env$data_revision_signature <- function() "revision"
  env$filterChoicesCache <- local({ value <- "cached"; function(x) {
    if (missing(x)) value else value <<- x
  }})
  env$filterCategoryCache <- local({ value <- "cached-category"; function(x) {
    if (missing(x)) value else value <<- x
  }})
  calls <- 0L
  env$updateSelectizeInput <- function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("deliberately injected failure")
    invisible(NULL)
  }

  expect_null(eval(handler, env))
  expect_true(any(grepl("deliberately injected failure", env$debug_messages,
                        fixed = TRUE)))
  expect_false(any(grepl("Error updating filter column choices: *$",
                         env$debug_messages)))
  expect_identical(env$filterChoicesCache(), character(0))
  expect_identical(env$filterCategoryCache(), "")
})
