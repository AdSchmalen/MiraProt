portable_script <- function(name) {
  path <- file.path("portable", "scripts", name)
  if (!file.exists(path)) path <- file.path("..", "..", path)
  path
}

load_portable_installer <- function() {
  env <- new.env(parent = globalenv())
  sys.source(portable_script("install-packages-portable.R"), envir = env)
  env
}

fake_package <- function(library, package, version) {
  dir.create(file.path(library, package), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(sprintf("Package: %s", package), sprintf("Version: %s", version),
               "Title: Test", "Description: Test.", "License: MIT",
               "Author: Test", "Maintainer: Test <test@example.com>"),
             file.path(library, package, "DESCRIPTION"))
}

test_that("restore lock contains only genuinely missing packages", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "bit64", "4.8.4")
  before <- readBin(file.path(lib, "bit64", "DESCRIPTION"), "raw", 10000)
  lock <- list(R = list(Version = as.character(getRversion())), Packages = list(
    bit64 = list(Package = "bit64", Version = "4.8.2"),
    missing = list(Package = "missing", Version = "1.0.0")
  ))
  selected <- restored <- NULL

  env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) lock, runtime_library = tempfile(),
    write_lock = function(value, path) { selected <<- value; writeLines("selected", path) },
    restore = function(stage, missing, libraries, lockfile) {
      restored <<- list(missing = missing, lockfile = lockfile)
      fake_package(stage, "missing", "1.0.0")
    }
  )

  expect_identical(names(selected$Packages), "missing")
  expect_identical(restored$missing, "missing")
  expect_false(identical(restored$lockfile, lock_path))
  expect_identical(env$portable_installed(lib)[["bit64"]], "4.8.4")
  expect_identical(readBin(file.path(lib, "bit64", "DESCRIPTION"), "raw", 10000), before)
  expect_identical(env$portable_installed(lib)[["missing"]], "1.0.0")
})

test_that("a satisfied incremental restore is a no-op", {
  env <- load_portable_installer()
  root <- tempfile(); lib <- tempfile(); dir.create(root); dir.create(lib)
  lock_path <- file.path(root, "renv.lock"); writeLines("{}", lock_path)
  fake_package(lib, "present", "9.0.0")
  restored <- wrote <- FALSE
  output <- capture.output(env$portable_restore_missing(
    lock_path, lib, root, bootstrap = function(...) TRUE,
    read_lock = function(...) list(
      R = list(Version = as.character(getRversion())),
      Packages = list(present = list(Package = "present", Version = "1.0.0"))
    ), runtime_library = tempfile(),
    write_lock = function(...) wrote <<- TRUE,
    restore = function(...) restored <<- TRUE
  ))
  expect_false(restored)
  expect_false(wrote)
  expect_match(output, "Installed 0 missing packages", fixed = TRUE)
  expect_identical(env$portable_installed(lib)[["present"]], "9.0.0")
})

test_that("fallback contract includes the human OrgDb runtime package", {
  fallback <- readLines(portable_script("install-packages.R"), warn = FALSE)
  expect_true(any(grepl('"org.Hs.eg.db"', fallback, fixed = TRUE)))
  expect_true(any(grepl("required_all <- unique(c(bioc_packages", fallback, fixed = TRUE)))
})

test_that("cache orchestration delegates semantic target then source validation", {
  cache <- paste(readLines(portable_script("prebuild-cache.R"), warn = FALSE), collapse = "\n")
  target_check <- regexpr("org_db <- load_cached_orgdb\\(cached_sqlite\\)", cache)[[1]]
  source_check <- regexpr("if \\(!go_valid && !is.null\\(source_go_cache\\)\\)", cache)[[1]]
  remote_check <- regexpr("AnnotationHub\\(localHub = FALSE", cache)[[1]]
  expect_gt(target_check, 0)
  expect_gt(source_check, target_check)
  expect_gt(remote_check, source_check)
  expect_match(cache, "unlink(destination, recursive = TRUE, force = TRUE)", fixed = TRUE)
  expect_false(grepl("file.copy(source_ah, ah_cache", cache, fixed = TRUE))
})
