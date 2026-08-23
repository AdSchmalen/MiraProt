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
