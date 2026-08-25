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

testthat::test_that("session restore public handles share the Data Wizard registry", {
  testthat::skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))
  source(file.path("modules", "Data Wizard", "datawizard_core.R"), local = FALSE)

  # Guard the actual module contract: before this regression fix the public
  # return omitted dataset_registry, making the restore adapter fall back to rv.
  module_source <- paste(readLines(file.path("modules", "datawizard_module.R")), collapse = "\n")
  testthat::expect_match(
    module_source,
    "dataset_registry\\s*=\\s*core_values\\$dataset_registry"
  )

  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  metadata_b <- make_metadata(data_b)
  core_values <- create_core_reactive_values()
  public_handles <- list(
    dataset_registry = core_values$dataset_registry,
    primary_data_raw = core_values$primary_data_raw,
    handson_metadata = core_values$handson_metadata,
    final_processed_data = core_values$final_processed_data,
    final_processed_metadata = core_values$final_processed_metadata
  )
  rv <- shiny::reactiveValues()
  restore_adapter <- create_primary_data_state_adapter(
    rv = rv,
    core_values = public_handles
  )
  historical_pca_cache <- list(
    data = data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE),
    metadata = make_metadata(
      data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE),
      c("Row Index", "Condition")
    ),
    cache_key = "A"
  )
  cache_before <- unserialize(serialize(historical_pca_cache, NULL))

  shiny::isolate({
    restore_adapter$set_raw_imported_data(data_b, "session restore regression")
    restore_adapter$set_metadata_for_current_data(metadata_b)
  })

  primary <- shiny::isolate(resolve_datawizard_dataset(
    "primary_working", core_values = core_values, rv = rv
  )$data)
  canonical_metadata <- shiny::isolate(resolve_datawizard_dataset(
    "metadata_working", core_values = core_values, rv = rv
  )$data)
  canonical_resolved <- shiny::isolate(resolve_current_metadata("primary_working"))
  table_metadata <- metadata_b
  core_metadata <- shiny::isolate(core_values$handson_metadata())

  testthat::expect_identical(restore_adapter$registry(), core_values$dataset_registry())
  testthat::expect_identical(primary, data_b)
  testthat::expect_identical(shiny::isolate(rv$data_mod), data_b)
  testthat::expect_identical(canonical_metadata, metadata_b)
  testthat::expect_identical(core_metadata, metadata_b)
  testthat::expect_identical(shiny::isolate(rv$data_def), metadata_b)
  testthat::expect_true(metadata_matches_dataset(table_metadata, primary))
  testthat::expect_true(metadata_matches_dataset(core_metadata, primary))
  testthat::expect_true(metadata_matches_dataset(canonical_metadata, primary))
  testthat::expect_identical(canonical_resolved, metadata_b)
  testthat::expect_true(metadata_matches_dataset(canonical_resolved, primary))
  testthat::expect_true(any(vapply(
    list(table_metadata, core_metadata, canonical_resolved),
    metadata_matches_dataset,
    logical(1),
    dataset = primary
  )))
  testthat::expect_identical(historical_pca_cache, cache_before)
})

testthat::test_that("restore lifecycle ranks alignment ahead of meaningfulness", {
  testthat::skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))
  source(file.path("modules", "Data Wizard", "datawizard_core.R"), local = FALSE)

  data_a <- data.frame(`Row Index` = 1:2, A = 3:4, check.names = FALSE)
  data_b <- data.frame(`Row Index` = 1:2, B = 5:6, check.names = FALSE)
  meaningful_a <- make_metadata(data_a, c("Row Index", "Condition"))
  blank_b <- make_metadata(data_b)
  meaningful_b <- make_metadata(data_b, c("Row Index", "Condition"))

  testthat::expect_identical(
    select_datawizard_restore_lifecycle_metadata(blank_b, meaningful_a, data_b),
    blank_b
  )
  testthat::expect_identical(
    select_datawizard_restore_lifecycle_metadata(blank_b, meaningful_b, data_b),
    meaningful_b
  )
  testthat::expect_identical(
    select_datawizard_restore_lifecycle_metadata(meaningful_b, meaningful_b, data_b),
    meaningful_b
  )
})
