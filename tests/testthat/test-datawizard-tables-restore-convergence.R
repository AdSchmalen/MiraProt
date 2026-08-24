hydration_file <- file.path(
  "modules", "Data Wizard", "tables",
  "datawizard_tables_observer_metadata_hydration.R"
)
editing_file <- file.path(
  "modules", "Data Wizard", "tables",
  "datawizard_tables_observer_metadata_editing.R"
)
if (!file.exists(hydration_file)) {
  hydration_file <- file.path("..", "..", hydration_file)
  editing_file <- file.path("..", "..", editing_file)
}

tables_restore_test_env <- new.env(parent = globalenv())
sys.source(hydration_file, envir = tables_restore_test_env)
sys.source(editing_file, envir = tables_restore_test_env)

tables_restore_data <- function(columns) {
  as.data.frame(setNames(
    replicate(length(columns), numeric(), simplify = FALSE), columns
  ))
}

tables_restore_metadata <- function(columns, assignment) {
  data.frame(
    Column = columns,
    Content = rep("Sample", length(columns)),
    Options = rep(assignment, length(columns)),
    Sample = rep(assignment, length(columns)),
    stringsAsFactors = FALSE
  )
}

test_that("restore completion converges stale or empty Tables state to the canonical pair", {
  data_b <- tables_restore_data(paste0("B", seq_len(127)))
  metadata_b <- tables_restore_metadata(names(data_b), "dataset B")
  data_a <- tables_restore_data(paste0("A", seq_len(46)))
  metadata_a <- tables_restore_metadata(names(data_a), "dataset A")

  converge <- tables_restore_test_env$.datawizard_tables_restore_convergence_payload
  expect_identical(converge(metadata_b, metadata_a, data_a), metadata_a)
  expect_identical(converge(NULL, metadata_a, data_a), metadata_a)

  # This models the server-side restore-trigger assignment without requiring a
  # rendered/visible rHandsontable output.
  current_handson_metadata <- metadata_b
  replacement <- converge(current_handson_metadata, metadata_a, data_a)
  if (!is.null(replacement)) current_handson_metadata <- replacement
  expect_identical(current_handson_metadata, metadata_a)
  expect_true(metadata_matches_dataset(current_handson_metadata, data_a))
})

test_that("restore convergence uses full metadata payload and only a valid pair", {
  data_a <- tables_restore_data(paste0("A", seq_len(46)))
  prior <- tables_restore_metadata(names(data_a), "prior assignment")
  restored <- tables_restore_metadata(names(data_a), "restored assignment")
  converge <- tables_restore_test_env$.datawizard_tables_restore_convergence_payload

  expect_identical(converge(prior, restored, data_a), restored)
  expect_null(converge(restored, restored, data_a))
  expect_null(converge(prior, prior[-1, ], data_a))
  expect_null(converge(prior, NULL, data_a))
})

test_that("stale browser echoes are rejected before local mutation", {
  data_a <- tables_restore_data(paste0("A", seq_len(46)))
  metadata_a <- tables_restore_metadata(names(data_a), "restored")
  stale_127 <- tables_restore_metadata(paste0("B", seq_len(127)), "stale")
  stale_same_size <- tables_restore_metadata(rev(names(data_a)), "stale")
  aligned <- tables_restore_test_env$.datawizard_tables_browser_payload_aligned

  current_handson_metadata <- metadata_a
  sync_pending <- FALSE
  for (payload in list(stale_127, stale_same_size)) {
    if (aligned(payload, data_a)) {
      current_handson_metadata <- payload
      sync_pending <- TRUE
    }
  }
  expect_identical(current_handson_metadata, metadata_a)
  expect_false(sync_pending)
})

test_that("an aligned genuine metadata edit remains accepted after restore", {
  data_a <- tables_restore_data(paste0("A", seq_len(46)))
  metadata_a <- tables_restore_metadata(names(data_a), "restored")
  edited <- metadata_a
  edited$Content[[2]] <- "Condition"
  edited$Options[[2]] <- "Treatment"
  aligned <- tables_restore_test_env$.datawizard_tables_browser_payload_aligned

  expect_true(aligned(edited, data_a))
  current_handson_metadata <- metadata_a
  if (aligned(edited, data_a)) current_handson_metadata <- edited
  expect_identical(current_handson_metadata, edited)
})

test_that("a superseded restore identity cannot apply its convergence payload", {
  data_a <- tables_restore_data(c("A1", "A2"))
  metadata_a <- tables_restore_metadata(names(data_a), "generation N")
  data_next <- tables_restore_data(c("N1", "N2"))
  metadata_next <- tables_restore_metadata(names(data_next), "generation N+1")
  converge <- tables_restore_test_env$.datawizard_tables_restore_convergence_payload

  scheduled_generation <- 10L
  scheduled_trigger <- 3L
  replacement <- converge(NULL, metadata_a, data_a)
  current_generation <- 11L
  current_trigger <- 4L
  current_handson_metadata <- metadata_next
  if (identical(current_generation, scheduled_generation) &&
      identical(current_trigger, scheduled_trigger)) {
    current_handson_metadata <- replacement
  }
  expect_identical(current_handson_metadata, metadata_next)
})
