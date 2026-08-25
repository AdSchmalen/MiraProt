testthat::local_edition(3)

`%||%` <- function(x, y) if (!is.null(x)) x else y
.safe_rv_write <- function(rv_fn, value) if (is.function(rv_fn)) rv_fn(value)
source(file.path("modules", "Data Wizard", "datawizard_utils.R"), local = FALSE)
source(file.path("R", "session_save_restore", "session_save_restore_module_registration.R"), local = FALSE)

make_metadata <- function(data, content = NULL) {
  metadata <- .datawizard_safe_metadata_skeleton(data)
  if (!is.null(content)) metadata$Content <- content
  metadata
}

testthat::test_that("aligned blank canonical metadata outranks stale historical metadata", {
  data_a <- data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE)
  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  metadata_a <- make_metadata(data_a, c("Row Index", "Condition"))
  metadata_b <- make_metadata(data_b)
  pca_cache <- list(data = data_a, metadata = metadata_a, cache_key = "A")
  historical_cache_before_restore <- unserialize(serialize(pca_cache, NULL))

  selected <- .select_datawizard_restored_metadata(list(
    data_mod = data_b,
    data_def = metadata_b,
    handson_metadata = metadata_a,
    final_processed_metadata = metadata_a
  ))

  testthat::expect_identical(selected, metadata_b)
  testthat::expect_true(metadata_matches_dataset(selected, data_b))
  testthat::expect_false(is_meaningful_metadata(selected))
  # Data Wizard selection neither reads nor mutates the historical PCA cache.
  testthat::expect_identical(pca_cache, historical_cache_before_restore)
})

testthat::test_that("meaningful canonical metadata remains authoritative", {
  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  metadata_b <- make_metadata(data_b, c("Row Index", "Condition"))

  selected <- .select_datawizard_restored_metadata(list(
    data_mod = data_b,
    data_def = metadata_b,
    handson_metadata = NULL,
    final_processed_metadata = NULL
  ))

  testthat::expect_identical(selected, metadata_b)
  testthat::expect_true(is_meaningful_metadata(selected))
})

testthat::test_that("misaligned restore metadata is replaced by a safe aligned skeleton", {
  data_a <- data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE)
  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  metadata_a <- make_metadata(data_a, c("Row Index", "Condition"))

  selected <- .select_datawizard_restored_metadata(list(
    data_mod = data_b,
    data_def = metadata_a,
    handson_metadata = metadata_a,
    final_processed_metadata = metadata_a
  ))

  testthat::expect_true(metadata_matches_dataset(selected, data_b))
  testthat::expect_false(is_meaningful_metadata(selected))
  testthat::expect_identical(selected$Column, names(data_b))
})

testthat::test_that("canonical transaction publishes blank metadata and rejects stale final metadata", {
  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  metadata_b <- make_metadata(data_b)
  stale_final <- make_metadata(
    data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE),
    c("Row Index", "Condition")
  )
  calls <- new.env(parent = emptyenv())
  calls$metadata <- NULL
  calls$final_metadata <- "not-cleared"
  adapter <- list(
    begin_datawizard_transaction = function(...) NULL,
    set_raw_imported_data = function(...) NULL,
    set_modified_data = function(...) NULL,
    set_metadata_for_current_data = function(metadata) calls$metadata <- metadata,
    set_final_data = function(...) NULL,
    set_metadata_final = function(metadata, ...) calls$final_metadata <- metadata,
    publish_legacy_mirrors = function() NULL,
    commit_datawizard_transaction = function() NULL
  )
  reactive_holder <- function(initial = NULL) {
    value <- initial
    function(new_value) {
      if (missing(new_value)) value else value <<- new_value
    }
  }
  dw <- list(
    primary_data_raw = reactive_holder(),
    handson_metadata = reactive_holder(),
    final_processed_data = reactive_holder(),
    final_processed_metadata = reactive_holder("not-cleared")
  )
  rv <- new.env(parent = emptyenv())
  old_isolate <- get0("isolate", envir = .GlobalEnv, inherits = FALSE)
  assign("isolate", function(expr) eval.parent(substitute(expr)), envir = .GlobalEnv)
  on.exit({
    if (is.null(old_isolate)) rm("isolate", envir = .GlobalEnv) else
      assign("isolate", old_isolate, envir = .GlobalEnv)
  }, add = TRUE)
  old_debug_log <- get0("debug_log", envir = .GlobalEnv, inherits = FALSE)
  assign("debug_log", function(...) NULL, envir = .GlobalEnv)
  on.exit({
    if (is.null(old_debug_log)) rm("debug_log", envir = .GlobalEnv) else
      assign("debug_log", old_debug_log, envir = .GlobalEnv)
  }, add = TRUE)

  result <- .restore_datawizard_canonical_data_transaction(
    adapter, dw, rv,
    state = list(data_mod = data_b, final_processed_data = data_b,
                 final_processed_metadata = stale_final),
    restored_metadata = metadata_b,
    assert_invariants = FALSE
  )

  testthat::expect_identical(calls$metadata, metadata_b)
  testthat::expect_identical(rv$data_def, metadata_b)
  testthat::expect_identical(rv$handson_metadata, metadata_b)
  testthat::expect_identical(dw$handson_metadata(), metadata_b)
  testthat::expect_null(result$final_metadata)
  testthat::expect_null(rv$final_processed_metadata)
  testthat::expect_null(dw$final_processed_metadata())
  testthat::expect_identical(calls$final_metadata, "not-cleared")
})
