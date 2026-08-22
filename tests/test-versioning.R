#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
root <- normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."))
version_bytes <- readBin(file.path(root, "VERSION"), "raw", n = 100L)
stopifnot(identical(rawToChar(version_bytes), "1.0.0\n"))

cff <- readLines(file.path(root, "CITATION.cff"), warn = FALSE)
cff_version <- sub('^version: "([^"]+)"$', "\\1", grep('^version: "[^"]+"$', cff, value = TRUE))
stopifnot(length(cff_version) == 1L, identical(cff_version, "1.0.0"))

`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path(root, "R", "version_info.R"))
old <- setwd(root)
on.exit(setwd(old), add = TRUE)
source_info <- miraprot_version_info()
expected_sha <- system2("git", c("rev-parse", "--short=7", "HEAD"), stdout = TRUE)
stopifnot(identical(source_info$version, "1.0.0"), identical(source_info$commit, expected_sha))

for (count in c("100", "999")) {
  runtime <- tempfile("miraprot-runtime-")
  dir.create(runtime)
  writeLines("1.0.0", file.path(runtime, "VERSION"))
  writeLines(c(paste0("COMMIT_COUNT=", count), "COMMIT_SHA=abcdef1", "COMMIT_DATE=2026-08-22"),
             file.path(runtime, "BUILD_INFO"))
  setwd(runtime)
  info <- miraprot_version_info()
  stopifnot(identical(info$version, "1.0.0"), identical(info$commit, "abcdef1"),
            identical(info$last_updated, "2026-08-22"))
  setwd(root)
  unlink(runtime, recursive = TRUE)
}

cat("Version architecture checks passed.\n")
