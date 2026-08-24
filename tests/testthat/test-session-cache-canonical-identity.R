core_file <- file.path("R", "session_save_restore", "session_save_restore_core_helpers.R")
if (!file.exists(core_file)) core_file <- file.path("..", "..", core_file)

cache_env <- new.env(parent = globalenv())
cache_env$`%||%` <- function(x, y) if (is.null(x)) y else x
cache_env$debug_log <- function(...) invisible(NULL)
sys.source(file.path(dirname(core_file), "session_save_restore_cache_keys.R"), envir = cache_env)
sys.source(core_file, envir = cache_env)

test_that("full-session visualization cache identities round trip byte-identically", {
  modules <- c("sampleids", "pca", "volcano", "dotplot", "heatmap", "venn", "abundances")
  keys <- vapply(modules, function(module) {
    cache_env$.build_canonical_plot_cache_key(module, "logical plot 1", "main")
  }, character(1L))
  refs <- stats::setNames(as.list(paste0("cache-", seq_along(keys))), keys)
  full_session <- list(
    save_level = "full_session",
    module_snapshots = stats::setNames(lapply(seq_along(modules), function(i) {
      list(module_state = list(plot_cache_ref_by_title = refs[i]))
    }), modules)
  )

  restored <- unserialize(serialize(full_session, NULL, version = 3L))
  restored_keys <- unlist(lapply(restored$module_snapshots, function(snapshot) {
    names(snapshot$module_state$plot_cache_ref_by_title)
  }), use.names = FALSE)
  expect_identical(charToRaw(paste(keys, collapse = "\n")),
                   charToRaw(paste(restored_keys, collapse = "\n")))
  expect_true(all(vapply(restored_keys, function(key) {
    identity <- cache_env$.canonical_plot_cache_identity(key = key)
    isTRUE(identity$valid) && identical(identity$key, key)
  }, logical(1L))))
})

test_that("malformed and NA canonical cache identities have distinct rejection outcomes", {
  malformed <- cache_env$.canonical_plot_cache_identity(key = "dotplot::::main")
  missing <- cache_env$.canonical_plot_cache_identity(key = NA_character_)
  empty <- cache_env$.canonical_plot_cache_identity("dotplot", "", "main")

  expect_false(malformed$valid)
  expect_identical(malformed$outcome, "malformed_separator_layout")
  expect_false(missing$valid)
  expect_identical(missing$outcome, "na_identity")
  expect_false(empty$valid)
  expect_identical(empty$outcome, "empty_identity_component")
  expect_error(cache_env$.build_canonical_plot_cache_key("dotplot", "", "main"),
               "empty_identity_component")
})

test_that("cache map validation distinguishes absent, malformed, and unresolved requirements", {
  absent <- cache_env$.validate_plot_cache_ref_by_title(NULL, list())
  malformed <- cache_env$.validate_plot_cache_ref_by_title(
    stats::setNames(list("cache-1"), "dotplot::::main"), list()
  )
  unresolved <- cache_env$.validate_plot_cache_ref_by_title(
    stats::setNames(list("cache-1"), "dotplot::plot::main"), list()
  )

  expect_true(absent$valid)
  expect_identical(absent$outcome, "absent_cache_requirement")
  expect_identical(malformed$diagnostics[["dotplot::::main"]], "malformed_separator_layout")
  expect_identical(unresolved$diagnostics[["dotplot::plot::main"]], "unresolved_reference")
})

test_that("live fallback requires data, metadata, revision, and dependency compatibility", {
  data_mod <- data.frame(id = "P1", abundance = 1)
  data_def <- data.frame(column = names(data_mod), role = c("id", "measure"))
  contract <- cache_env$.plot_data_cache_ref_contract(7L, 9L, data_mod, data_def)
  contract$restore_cache_dependency <- "shared_plot_data_cache_pool"

  expect_true(cache_env$.cache_ref_contract_compatible(
    contract, data_mod, data_def, 7L, 9L, "shared_plot_data_cache_pool"
  ))
  changed_data <- data_mod
  changed_data$abundance <- 2
  expect_false(cache_env$.cache_ref_contract_compatible(
    contract, changed_data, data_def, 7L, 9L, "shared_plot_data_cache_pool"
  ))
  expect_false(cache_env$.cache_ref_contract_compatible(
    contract, data_mod, data_def, 8L, 9L, "shared_plot_data_cache_pool"
  ))
  changed_metadata <- data_def
  changed_metadata$role[[2L]] <- "ignored"
  expect_false(cache_env$.cache_ref_contract_compatible(
    contract, data_mod, changed_metadata, 7L, 9L, "shared_plot_data_cache_pool"
  ))
})
