session_loader_file <- file.path("R", "session_save_restore.R")
if (!file.exists(session_loader_file)) {
  session_loader_file <- file.path("..", "..", session_loader_file)
}

source_session_subsystem <- function() {
  host <- new.env(parent = globalenv())
  sys.source(session_loader_file, envir = host)
  host
}

critical_session_runtime <- c(
  ".resolve_session_save_level",
  ".collect_sanitized_rv_snapshot_for_save",
  ".build_rv_snapshot_for_save_level",
  ".collect_sanitized_module_snapshots_for_save",
  ".build_save_time_plot_data_cache_bundle",
  ".build_v4_envelope",
  ".write_session_save_envelope",
  ".write_session_envelope_with_inline_fallback",
  ".notify_session_save_result",
  ".write_session_error_stub",
  ".sanitize_session_error_message",
  "MIRAPROT_SESSION_ERROR_MARKER",
  "MIRAPROT_SESSION_SCHEMA_VERSION",
  "MIRAPROT_SESSION_COMPATIBLE_VERSIONS",
  "MIRAPROT_APP_VERSION",
  "SESSION_SAVE_LEVEL_DATA",
  "SESSION_SAVE_LEVEL_ANALYSIS",
  "SESSION_SAVE_LEVEL_FULL"
)

expect_intact_session_owner <- function(host, disposable_mod_env = NULL) {
  public <- c(
    "setup_session_save_restore",
    "create_session_registry",
    "register_module_session_participants"
  )
  expect_true(all(vapply(public, function(x) is.function(host[[x]]), logical(1))))
  owner <- environment(host$setup_session_save_restore)
  expect_false(identical(owner, disposable_mod_env))
  expect_true(all(vapply(
    critical_session_runtime,
    exists,
    logical(1),
    envir = owner,
    inherits = FALSE
  )))
  expect_true(owner$assert_session_save_restore_runtime_integrity())
  invisible(owner)
}

test_that("pre-existing modEnv can be emptied without invalidating session closures", {
  old_mod_env <- get0("modEnv", envir = globalenv(), inherits = FALSE)
  disposable <- new.env(parent = globalenv())
  assign("modEnv", disposable, envir = globalenv())
  on.exit({
    if (is.environment(old_mod_env)) assign("modEnv", old_mod_env, globalenv())
    else if (exists("modEnv", globalenv(), inherits = FALSE)) rm("modEnv", envir = globalenv())
  }, add = TRUE)

  host <- source_session_subsystem()
  owner <- expect_intact_session_owner(host, disposable)
  rm(list = ls(disposable, all.names = TRUE), envir = disposable)

  expect_identical(environment(host$setup_session_save_restore), owner)
  expect_intact_session_owner(host, disposable)
})

test_that("clean startup and subsequent modEnv creation preserve the session owner", {
  old_mod_env <- get0("modEnv", envir = globalenv(), inherits = FALSE)
  if (exists("modEnv", envir = globalenv(), inherits = FALSE)) {
    rm("modEnv", envir = globalenv())
  }
  on.exit({
    if (is.environment(old_mod_env)) assign("modEnv", old_mod_env, globalenv())
    else if (exists("modEnv", globalenv(), inherits = FALSE)) rm("modEnv", envir = globalenv())
  }, add = TRUE)

  host <- source_session_subsystem()
  owner <- expect_intact_session_owner(host)
  disposable <- new.env(parent = globalenv())
  assign("modEnv", disposable, globalenv())
  expect_identical(expect_intact_session_owner(host, disposable), owner)
})

test_that("repeated source creates stable owners independent of hot-reloaded modEnv", {
  old_mod_env <- get0("modEnv", envir = globalenv(), inherits = FALSE)
  disposable <- new.env(parent = globalenv())
  assign("modEnv", disposable, globalenv())
  on.exit({
    if (is.environment(old_mod_env)) assign("modEnv", old_mod_env, globalenv())
    else if (exists("modEnv", globalenv(), inherits = FALSE)) rm("modEnv", envir = globalenv())
  }, add = TRUE)
  first <- source_session_subsystem()
  first_owner <- expect_intact_session_owner(first, disposable)
  rm(list = ls(disposable, all.names = TRUE), envir = disposable)
  second <- source_session_subsystem()
  second_owner <- expect_intact_session_owner(second, disposable)

  expect_false(identical(first_owner, second_owner))
  expect_intact_session_owner(first, disposable)
})

test_that("save-level resolver preserves all UI values and full-session default", {
  owner <- environment(source_session_subsystem()$setup_session_save_restore)
  resolve <- owner$.resolve_session_save_level
  for (level in c("data_only", "data_and_analysis", "full_session")) {
    expect_identical(resolve(list(session_save_level = level)), level)
  }
  expect_identical(resolve(list(session_save_level = "invalid")), "full_session")
  expect_identical(resolve(list()), "full_session")
})

test_that("error marker and minimal/full envelopes are readable binary RDS", {
  owner <- environment(source_session_subsystem()$setup_session_save_restore)
  error_file <- tempfile(fileext = ".rds")
  owner$.write_session_error_stub(error_file, "controlled save failure")
  expect_gt(file.info(error_file)$size, 0)
  marker <- readRDS(error_file)
  expect_true(marker$miraprot_session)
  expect_identical(marker$session_file_type, owner$MIRAPROT_SESSION_ERROR_MARKER)
  expect_identical(marker$version, owner$MIRAPROT_SESSION_SCHEMA_VERSION)
  expect_identical(marker$app_version, owner$MIRAPROT_APP_VERSION)
  expect_match(marker$error, "controlled save failure", fixed = TRUE)

  cases <- list(
    data_only = list(),
    data_and_analysis = list(analysis = list(result = data.frame(id = "P1"))),
    full_session = list(
      datawizard = list(data_mod = data.frame(id = "P1", value = 1)),
      volcano = list(settings = list(title = "representative"))
    )
  )
  for (level in names(cases)) {
    envelope <- owner$.build_v4_envelope(
      rv_snapshot = list(data_mod = data.frame(id = "P1", value = 1)),
      module_snapshots = cases[[level]],
      save_level = level
    )
    output <- tempfile(fileext = ".rds")
    result <- owner$.write_session_envelope_with_inline_fallback(
      envelope, output, cases[[level]]
    )
    expect_true(result$saved, info = level)
    saved <- readRDS(output)
    expect_true(saved$miraprot_session, info = level)
    expect_identical(saved$version, owner$MIRAPROT_SESSION_SCHEMA_VERSION)
    expect_identical(saved$save_level, level)
    expect_true(is.list(saved$manifest))
    expect_true(saved$manifest$transport %in% c("inline_rds", "qs2"))
  }
})
