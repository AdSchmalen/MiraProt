core_helpers_file <- file.path(
  "R", "session_save_restore", "session_save_restore_core_helpers.R"
)
if (!file.exists(core_helpers_file)) {
  core_helpers_file <- file.path("..", "..", core_helpers_file)
}

session_test_env <- new.env(parent = globalenv())
session_test_env$`%||%` <- function(x, y) if (is.null(x)) y else x
session_test_env$debug_log <- function(...) invisible(NULL)
cache_keys_file <- file.path(dirname(core_helpers_file), "session_save_restore_cache_keys.R")
sys.source(cache_keys_file, envir = session_test_env)
sys.source(core_helpers_file, envir = session_test_env)

minimal_rv_snapshot <- function() {
  list(data_mod = data.frame(protein = "P1", abundance = 1))
}

minimal_payload <- function() {
  list(rv_snapshot = minimal_rv_snapshot(), module_snapshots = list())
}

inline_envelope <- function(version = "4.0.0", marker = "miraprot_session") {
  snapshot <- list(
    version = version,
    app_version = "historical-test",
    created_at = as.POSIXct("2020-01-01", tz = "UTC"),
    save_level = "data_only",
    manifest = list(
      module_ids = character(), failed_modules = character(),
      data_dims = c(1L, 2L), transport = "inline_rds",
      transport_preset = "balanced"
    ),
    payload_inline = minimal_payload()
  )
  snapshot[[marker]] <- TRUE
  snapshot
}

test_that("current MiraProt v4 inline sessions remain valid", {
  snapshot <- inline_envelope()
  expect_true(session_test_env$validate_session_snapshot(snapshot)$valid)

  unwrapped <- session_test_env$unwrap_snapshot(snapshot)
  expect_true(session_test_env$.session_restore_is_native_current_schema(unwrapped))
  expect_s3_class(unwrapped$rv_snapshot$data_mod, "data.frame")
})

test_that("current MiraProt v4 qs2 sessions remain valid", {
  skip_if_not_installed("qs2")
  snapshot <- inline_envelope()
  snapshot$manifest$transport <- "qs2"
  snapshot$payload_qs2 <- qs2::qs_serialize(minimal_payload())
  snapshot$payload_inline <- NULL

  expect_true(session_test_env$validate_session_snapshot(snapshot)$valid)
  expect_s3_class(
    session_test_env$unwrap_snapshot(snapshot)$rv_snapshot$data_mod,
    "data.frame"
  )
})

test_that("MiraProt v3 inline and v1/v2 top-level snapshots remain valid", {
  v3 <- session_test_env$.build_v3_envelope(
    minimal_rv_snapshot(), list(), "data_only"
  )
  expect_identical(v3$manifest$transport, "inline_rds")
  expect_true(is.list(v3$payload_inline))
  expect_true(session_test_env$validate_session_snapshot(v3)$valid)
  expect_s3_class(session_test_env$unwrap_snapshot(v3)$rv_snapshot$data_mod, "data.frame")

  for (version in c("1.0.0", "2.0.0")) {
    legacy <- list(
      miraprot_session = TRUE,
      version = version,
      rv_snapshot = minimal_rv_snapshot(),
      module_snapshots = list()
    )
    expect_true(session_test_env$validate_session_snapshot(legacy)$valid)
  }
})

test_that("historical ShinyProt top-level sessions use legacy upgrade path", {
  historical <- list(
    shinyprot_session = TRUE,
    version = "2.0.0",
    app_version = "1.0",
    created_at = as.POSIXct("2020-01-01", tz = "UTC"),
    save_level = "data_only",
    rv_snapshot = minimal_rv_snapshot(),
    module_snapshots = list()
  )

  expect_true(session_test_env$validate_session_snapshot(historical)$valid)
  expect_false(session_test_env$.session_restore_is_native_current_schema(historical))
  upgraded <- session_test_env$upgrade_session_snapshot_to_current_schema(historical)
  expect_identical(upgraded$compatibility_upgrade$mode, "legacy_upgraded_schema")
  expect_s3_class(upgraded$rv_snapshot$data_mod, "data.frame")
  expect_null(upgraded$miraprot_session)
})

test_that("ShinyProt marker does not relax structural or version validation", {
  malformed <- list(shinyprot_session = TRUE)
  expect_false(session_test_env$validate_session_snapshot(malformed)$valid)

  unsupported <- list(
    shinyprot_session = TRUE,
    version = "99.0.0",
    rv_snapshot = minimal_rv_snapshot()
  )
  expect_false(session_test_env$validate_session_snapshot(unsupported)$valid)

  invalid_payload <- inline_envelope(version = "3.0.0", marker = "shinyprot_session")
  invalid_payload$payload_inline <- raw(1)
  expect_false(session_test_env$validate_session_snapshot(invalid_payload)$valid)
})

test_that("obsolete v3 qs transport is rejected without deserialization", {
  snapshot <- inline_envelope(version = "3.0.0")
  snapshot$manifest$transport <- "qs"
  snapshot$payload_inline <- NULL
  snapshot$payload_qs <- as.raw(c(1, 2, 3))

  result <- session_test_env$validate_session_snapshot(snapshot)
  expect_false(result$valid)
  expect_match(result$message, "Unsupported v3 session transport", fixed = TRUE)
})

test_that("runtime session code has no legacy qs namespace path", {
  source_text <- paste(readLines(core_helpers_file, warn = FALSE), collapse = "\n")
  expect_false(grepl("qs::", source_text, fixed = TRUE))
  expect_false(grepl(".legacy_qs_available", source_text, fixed = TRUE))
  expect_false(grepl("qdeserialize", source_text, fixed = TRUE))
})
