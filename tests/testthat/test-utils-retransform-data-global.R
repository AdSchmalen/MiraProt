test_that("global retransformation commits requested columns atomically", {
  utils_file <- file.path("R", "utils.R")
  if (!file.exists(utils_file)) utils_file <- file.path("..", "..", utils_file)

  test_env <- new.env(parent = globalenv())
  notifications <- character()
  test_env$showNotification <- function(message, ...) {
    notifications <<- c(notifications, message)
  }
  sys.source(utils_file, envir = test_env)

  input <- data.frame(safe = c(1, 2), overflow = c(1, 1024), untouched = 3:4)
  result <- test_env$retransform_data_global(
    input,
    c(1, 2),
    c("log2", "log2")
  )

  expect_identical(result, input)
  expect_length(notifications, 1L)
  expect_match(notifications, "overflow", fixed = TRUE)
})

test_that("global retransformation ignores pre-existing non-finite positions", {
  utils_file <- file.path("R", "utils.R")
  if (!file.exists(utils_file)) utils_file <- file.path("..", "..", utils_file)

  test_env <- new.env(parent = globalenv())
  notifications <- character()
  test_env$showNotification <- function(message, ...) {
    notifications <<- c(notifications, message)
  }
  sys.source(utils_file, envir = test_env)

  input <- data.frame(
    log_values = c(1, NA_real_, Inf, NaN),
    unchanged = c(5, 6, 7, 8)
  )
  result <- test_env$retransform_data_global(input, 1, "log2")

  expect_equal(result$log_values[c(1, 3)], c(2, Inf))
  expect_true(is.na(result$log_values[2]))
  expect_false(is.nan(result$log_values[2]))
  expect_true(is.nan(result$log_values[4]))
  expect_identical(result$unchanged, input$unchanged)
  expect_empty(notifications)
})

test_that("global retransformation retains None and unknown behavior", {
  utils_file <- file.path("R", "utils.R")
  if (!file.exists(utils_file)) utils_file <- file.path("..", "..", utils_file)

  test_env <- new.env(parent = globalenv())
  test_env$showNotification <- function(...) stop("unexpected notification")
  sys.source(utils_file, envir = test_env)

  input <- data.frame(none = c(1, NA_real_), unknown = c(Inf, 2))

  expect_identical(
    test_env$retransform_data_global(input, c(1, 2), c("None", "other")),
    input
  )
})
