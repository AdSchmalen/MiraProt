#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path("R", "version_info.R"), local = FALSE)

with_working_directory <- function(path, code) {
  previous <- setwd(path)
  on.exit(setwd(previous), add = TRUE)
  force(code)
}

portable_root <- tempfile("miraprot-portable-")
dir.create(portable_root)
writeLines(c(
  "COMMIT_COUNT=123",
  "COMMIT_SHA=1a2b3c4",
  "COMMIT_DATE=2026-08-19"
), file.path(portable_root, "BUILD_INFO"))

original_git_value <- .miraprot_git_value
.miraprot_git_value <- function(args) stop("Git must not be invoked in portable mode")
portable_warnings <- character()
portable <- with_working_directory(portable_root, withCallingHandlers(
  miraprot_version_info(),
  warning = function(warning) {
    portable_warnings <<- c(portable_warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
))
stopifnot(
  file.exists(file.path(portable_root, "BUILD_INFO")),
  !file.exists(file.path(portable_root, ".git")),
  identical(portable$version, "1.0.123"),
  identical(portable$commit, "1a2b3c4"),
  identical(portable$last_updated, "2026-08-19"),
  length(portable_warnings) == 0L
)

checkout_root <- tempfile("miraprot-checkout-")
dir.create(checkout_root)
file.create(file.path(checkout_root, ".git"))
writeLines(c(
  "COMMIT_COUNT=10",
  "COMMIT_SHA=aaaaaaa",
  "COMMIT_DATE=2000-01-01"
), file.path(checkout_root, "BUILD_INFO"))

git_calls <- list()
.miraprot_git_value <- function(args) {
  git_calls[[length(git_calls) + 1L]] <<- args
  if (identical(args[[1L]], "rev-list")) return("456")
  if (identical(args[[1L]], "rev-parse")) return("bcd1234")
  "2026-08-18"
}
checkout <- with_working_directory(checkout_root, miraprot_version_info())
stopifnot(
  identical(checkout$version, "1.0.456"),
  identical(checkout$commit, "bcd1234"),
  identical(checkout$last_updated, "2026-08-18"),
  length(git_calls) == 3L
)

# A failed individual Git query still uses the corresponding build value.
.miraprot_git_value <- function(args) {
  if (identical(args[[1L]], "rev-parse")) return(NA_character_)
  if (identical(args[[1L]], "rev-list")) return("999")
  "2026-08-16"
}
writeLines(c(
  "COMMIT_COUNT=789",
  "COMMIT_SHA=cafe123",
  "COMMIT_DATE=2026-08-17"
), file.path(checkout_root, "BUILD_INFO"))
fallback <- with_working_directory(checkout_root, miraprot_version_info())
stopifnot(
  identical(fallback$version, "1.0.999"),
  identical(fallback$commit, "cafe123"),
  identical(fallback$last_updated, "2026-08-16")
)

# Handled nonzero statuses do not leak system2 warnings.
system2 <- function(...) {
  warning("command had status 128")
  structure(character(), status = 128L)
}
quiet_warnings <- character()
quiet_value <- withCallingHandlers(
  original_git_value(c("rev-list", "--count", "HEAD")),
  warning = function(warning) quiet_warnings <<- c(quiet_warnings, conditionMessage(warning))
)
stopifnot(is.na(quiet_value), length(quiet_warnings) == 0L)
rm(system2)

.miraprot_git_value <- function(args) stop("Git must not be invoked without .git")
malformed_root <- tempfile("miraprot-malformed-")
dir.create(malformed_root)
writeLines(c(
  "COMMIT_COUNT=not-a-count",
  "COMMIT_SHA=invalid",
  "COMMIT_DATE=yesterday"
), file.path(malformed_root, "BUILD_INFO"))
malformed <- with_working_directory(malformed_root, miraprot_version_info())
unlink(file.path(malformed_root, "BUILD_INFO"))
missing <- with_working_directory(malformed_root, miraprot_version_info())
stopifnot(
  identical(malformed, list(
    version = "1.0", commit = "unavailable", last_updated = "unavailable"
  )),
  identical(missing, malformed)
)

cat("Version information runtime checks passed\n")
