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

test_that("valid lockfile installs missing packages without touching existing versions", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "kept", "9.9.9")
  description_before <- readBin(file.path(lib, "kept", "DESCRIPTION"), "raw", 10000)
  lock <- list(R = list(Version = as.character(getRversion())), Packages = list(
    kept = list(Package = "kept", Version = "1.0.0"),
    added = list(Package = "added", Version = "2.0.0")
  ))
  selected <- NULL
  env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) lock,
    write_lock = function(value, ...) { selected <<- value },
    restore = function(stage, missing) fake_package(stage, "added", "2.0.0")
  )
  expect_identical(names(selected$Packages), "added")
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
    write_lock = function(...) NULL,
    restore = function(...) restored <<- TRUE
  ), "requires R 0.0.1")
  expect_false(restored)
})

test_that("fallback delegates to the unchanged package-list script", {
  text <- paste(readLines(portable_script()), collapse = "\n")
  expect_match(text, 'system2\\(file.path\\(R.home\\("bin"\\), "Rscript"\\)')
  expect_match(text, '"--vanilla"')
  expect_true(file.exists(sub("install-packages-portable", "install-packages", portable_script())))
})
