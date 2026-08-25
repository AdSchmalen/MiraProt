portable_script <- function() {
  path <- file.path("portable", "scripts", "install-packages-portable.R")
  if (!file.exists(path)) path <- file.path("..", "..", path)
  path
}

load_portable_installer <- function() {
  env <- new.env(parent = globalenv())
  sys.source(portable_script(), envir = env)
  env
}

fake_package <- function(library, package, version) {
  dir.create(file.path(library, package), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(sprintf("Package: %s", package), sprintf("Version: %s", version),
               "Title: Test package", "Description: Test package.",
               "License: MIT", "Author: Test", "Maintainer: Test <test@example.com>"),
             file.path(library, package, "DESCRIPTION"))
}

test_that("lockfile absence and renv failures select the fallback", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  calls <- character()
  env$portable_fallback <- function(reason, fallback, library) { calls <<- c(calls, reason); FALSE }

  expect_false(env$portable_install(lib, root, "fallback.R"))
  expect_match(calls[[1]], "absent")
  writeLines("{", file.path(root, "renv.lock"))
  for (message in c("malformed lockfile", "bootstrap failed", "restore failed",
                    "requires R 4.5.0")) {
    expect_false(env$portable_install(lib, root, "fallback.R",
      restore_missing = function(...) stop(message)))
    expect_identical(tail(calls, 1), message)
  }
})

test_that("missing packages use the full lockfile and preserve existing versions", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "kept", "9.9.9")
  description_before <- readBin(file.path(lib, "kept", "DESCRIPTION"), "raw", 10000)
  lock <- list(R = list(Version = as.character(getRversion())), Packages = list(
    kept = list(Package = "kept", Version = "1.0.0"),
    added = list(Package = "added", Version = "2.0.0")
  ))
  restored <- NULL
  env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) lock,
    runtime_library = tempfile(),
    restore = function(stage, missing, libraries, lockfile) {
      restored <<- list(missing = missing, libraries = libraries, lockfile = lockfile)
      fake_package(stage, "added", "2.0.0")
    }
  )
  expect_identical(restored$missing, "added")
  expect_identical(restored$lockfile, lock_path)
  expect_identical(restored$libraries[[2]], lib)
  expect_false(file.exists(file.path(dirname(restored$libraries[[1]]), "renv.lock")))
  expect_identical(env$portable_installed(lib)[["kept"]], "9.9.9")
  expect_identical(env$portable_installed(lib)[["added"]], "2.0.0")
  expect_identical(readBin(file.path(lib, "kept", "DESCRIPTION"), "raw", 10000),
                   description_before)
})

test_that("an R version mismatch is rejected before restore", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  restored <- FALSE
  expect_error(env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) list(R = list(Version = "0.0.1"),
                                   Packages = list(pkg = list(Version = "1.0.0"))),
    restore = function(...) restored <<- TRUE
  ), "requires R 0.0.1")
  expect_false(restored)
})

test_that("destination and R runtime packages are already satisfied", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); runtime <- tempfile()
  dir.create(root); dir.create(lib); dir.create(runtime)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "destinationPackage", "1.0.0")

  recommended <- rownames(utils::installed.packages(lib.loc = .Library,
                                                     priority = "recommended"))
  skip_if(!length(recommended), "R has no recommended package available")
  runtime_package <- recommended[[1]]
  runtime_version <- as.character(utils::packageVersion(runtime_package, lib.loc = .Library))
  file.copy(file.path(.Library, runtime_package), runtime, recursive = TRUE)
  lock <- list(R = list(Version = as.character(getRversion())), Packages = setNames(list(
    list(Package = "destinationPackage", Version = "1.0.0"),
    list(Package = runtime_package, Version = runtime_version)
  ), c("destinationPackage", runtime_package)))
  restored <- FALSE
  fell_back <- FALSE
  env$portable_fallback <- function(...) { fell_back <<- TRUE; FALSE }

  output <- capture.output(env$portable_install(
    lib, root, "fallback.R", restore_missing = function(...) {
      env$portable_restore_missing(
        lock_path, lib, root, bootstrap = function(...) TRUE,
        read_lock = function(...) lock, runtime_library = runtime,
        restore = function(...) restored <<- TRUE)
    }
  ))
  expect_false(restored)
  expect_false(fell_back)
  expect_match(output, "Installed 0 missing packages", fixed = TRUE)
  expect_false(dir.exists(file.path(lib, runtime_package)))
})

test_that("restore configuration is isolated for bootstrap and actual restore", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  lock <- list(R = list(Version = as.character(getRversion())),
               Packages = list(added = list(Package = "added", Version = "2.0.0")))
  names <- c("RENV_PROJECT", "RENV_PATHS_ROOT", "RENV_CONFIG_CACHE_ENABLED",
             "RENV_CONFIG_SYNCHRONIZED_CHECK", "RENV_CONFIG_STARTUP_QUIET", "R_LIBS_USER")
  old <- Sys.getenv(names, unset = NA_character_)
  on.exit(for (name in names(old)) {
    if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(old[[name]]), name))
  }, add = TRUE)
  Sys.setenv(RENV_PROJECT = "caller's project", RENV_CONFIG_CACHE_ENABLED = "TRUE",
             R_LIBS_USER = "caller's library")
  bootstrap_values <- restore_values <- NULL

  env$portable_restore_missing(
    lock_path, lib, root,
    bootstrap = function(...) bootstrap_values <<- Sys.getenv(names),
    read_lock = function(...) lock, runtime_library = tempfile(),
    restore = function(stage, ...) {
      restore_values <<- Sys.getenv(names)
      fake_package(stage, "added", "2.0.0")
    }
  )
  expect_identical(bootstrap_values[["RENV_CONFIG_SYNCHRONIZED_CHECK"]], "FALSE")
  expect_identical(bootstrap_values[["RENV_CONFIG_STARTUP_QUIET"]], "TRUE")
  expect_identical(restore_values[["RENV_CONFIG_CACHE_ENABLED"]], "FALSE")
  expect_match(restore_values[["RENV_PATHS_ROOT"]], "miraprot-renv-")
  expect_match(restore_values[["R_LIBS_USER"]], "library$")
  expect_identical(Sys.getenv("RENV_PROJECT"), "caller's project")
  expect_identical(Sys.getenv("RENV_CONFIG_CACHE_ENABLED"), "TRUE")
  expect_identical(Sys.getenv("R_LIBS_USER"), "caller's library")
})

test_that("restore failure falls back without changing existing packages", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "kept", "9.9.9")
  before <- readBin(file.path(lib, "kept", "DESCRIPTION"), "raw", 10000)
  fallback_reason <- NULL
  env$portable_fallback <- function(reason, ...) { fallback_reason <<- reason; FALSE }
  result <- env$portable_install(lib, root, "fallback.R", restore_missing = function(...) {
    env$portable_restore_missing(
      lock_path, lib, root, bootstrap = function(...) TRUE,
      read_lock = function(...) list(
        R = list(Version = as.character(getRversion())),
        Packages = list(kept = list(Version = "1.0.0"), added = list(Version = "2.0.0"))
      ), runtime_library = tempfile(), restore = function(...) stop("restore failed"))
  })
  expect_false(result)
  expect_match(fallback_reason, "restore failed")
  expect_identical(readBin(file.path(lib, "kept", "DESCRIPTION"), "raw", 10000), before)
})

test_that("a package appearing in the destination is never overwritten", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  lock <- list(R = list(Version = as.character(getRversion())),
               Packages = list(added = list(Version = "2.0.0")))
  expect_error(env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) lock, runtime_library = tempfile(),
    restore = function(stage, ...) {
      fake_package(stage, "added", "2.0.0")
      fake_package(lib, "added", "unexpected")
    }
  ), "changed during staged restore")
  expect_identical(env$portable_installed(lib)[["added"]], "unexpected")
})

test_that("fallback delegates to the unchanged package-list script", {
  text <- paste(readLines(portable_script()), collapse = "\n")
  expect_match(text, 'system2\\(file.path\\(R.home\\("bin"\\), "Rscript"\\)')
  expect_match(text, '"--vanilla"')
  expect_true(file.exists(sub("install-packages-portable", "install-packages", portable_script())))
})
