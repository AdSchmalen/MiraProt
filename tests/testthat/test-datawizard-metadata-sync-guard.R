hydration_file <- file.path(
  "modules", "Data Wizard", "tables",
  "datawizard_tables_observer_metadata_hydration.R"
)
if (!file.exists(hydration_file)) {
  hydration_file <- file.path("..", "..", hydration_file)
}

test_that("an older restore generation cannot release a newer metadata guard", {
  test_env <- new.env(parent = globalenv())
  sys.source(hydration_file, envir = test_env)

  guard <- new.env(parent = emptyenv())
  guard$token <- 1L
  guard$generation <- 10L
  guard$echo <- TRUE
  guard$active <- TRUE

  # Generation N has captured its release callback. Before it runs, generation
  # N+1 raises a fresh guard and advances both pieces of release identity.
  old_release <- function() test_env$.clear_datawizard_metadata_sync_guard(
    scheduled_token = 1L,
    scheduled_generation = 10L,
    current_token = function() guard$token,
    current_generation = function() guard$generation,
    clear_guards = function() {
      guard$echo <- FALSE
      guard$active <- FALSE
    }
  )
  guard$generation <- 11L
  guard$token <- 2L

  expect_false(old_release())
  expect_true(guard$echo)
  expect_true(guard$active)
})

metadata_fixture <- function(columns, content = "Sample", options = NA_character_) {
  data.frame(
    Column = columns,
    Content = rep(content, length(columns)),
    Options = rep(options, length(columns)),
    stringsAsFactors = FALSE
  )
}

test_that("write-back guard suppresses only the exact metadata echo", {
  test_env <- new.env(parent = globalenv())
  sys.source(hydration_file, envir = test_env)

  local <- metadata_fixture(paste0("X", seq_len(46)), options = "old")
  guard <- new.env(parent = emptyenv())
  guard$active <- TRUE

  expect_true(test_env$.consume_datawizard_metadata_write_back_guard(
    guard, local, local
  ))
  expect_false(guard$active)

  # Full-payload comparison is required: matching dimensions and Columns do
  # not make a changed restored assignment an echo of the Tables write-back.
  restored <- local
  restored$Options <- "restored"
  guard$active <- TRUE
  expect_false(test_env$.consume_datawizard_metadata_write_back_guard(
    guard, restored, local
  ))
  expect_false(guard$active)
})

test_that("distinct restored metadata is not swallowed by a stale guard", {
  test_env <- new.env(parent = globalenv())
  sys.source(hydration_file, envir = test_env)

  stale <- metadata_fixture(paste0("B", seq_len(127)), options = "dataset B")
  restored_data <- as.data.frame(setNames(
    replicate(46, numeric(), simplify = FALSE), paste0("A", seq_len(46))
  ))
  restored <- metadata_fixture(names(restored_data), options = "dataset A")
  guard <- new.env(parent = emptyenv())
  guard$active <- TRUE

  suppress_hydration <- test_env$.consume_datawizard_metadata_write_back_guard(
    guard, restored, stale
  )
  editable <- if (suppress_hydration) stale else restored

  expect_false(suppress_hydration)
  expect_false(guard$active)
  expect_identical(editable, restored)
  expect_identical(as.character(editable$Column), names(restored_data))
  expect_false(identical(editable, stale))
})

test_that("restore hydration distinguishes dataset identity and handles empty local state", {
  test_env <- new.env(parent = globalenv())
  sys.source(hydration_file, envir = test_env)
  consume <- test_env$.consume_datawizard_metadata_write_back_guard

  prior <- metadata_fixture(c("old_a", "old_b"), options = "prior")
  restored_names <- metadata_fixture(c("new_a", "new_b"), options = "restored")
  guard <- new.env(parent = emptyenv())
  guard$active <- TRUE
  expect_false(consume(guard, restored_names, prior))

  same_columns_new_values <- prior
  same_columns_new_values$Content <- "Condition"
  guard$active <- TRUE
  expect_false(consume(guard, same_columns_new_values, prior))

  guard$active <- TRUE
  expect_false(consume(guard, restored_names, NULL))
  expect_false(guard$active)
})
