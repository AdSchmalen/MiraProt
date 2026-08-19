# Application version and source revision shown in the About tab.
#
# The major/minor version is a product decision.  The patch component is the
# number of commits in the current Git history, so it advances automatically
# whenever a commit is added.  Portable builds do not contain .git; their
# build scripts therefore write the same values to BUILD_INFO.
MIRAPROT_VERSION_BASE <- "1.0"

.miraprot_read_build_info <- function(path = "BUILD_INFO") {
  if (!file.exists(path)) return(list())

  lines <- readLines(path, warn = FALSE)
  fields <- strsplit(lines[grepl("^[A-Z_]+=[^\r\n]*$", lines)], "=", fixed = TRUE)
  if (length(fields) == 0L) return(list())

  values <- vapply(fields, function(field) paste(field[-1L], collapse = "="), character(1L))
  names(values) <- vapply(fields, `[[`, character(1L), 1L)
  as.list(values)
}

.miraprot_git_value <- function(args) {
  value <- tryCatch(
    suppressWarnings(
      system2("git", c("-C", ".", args), stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) character()
  )
  status <- attr(value, "status")
  if ((!is.null(status) && status != 0L) || length(value) == 0L) return(NA_character_)
  trimws(value[[1L]])
}

miraprot_version_info <- function() {
  build <- .miraprot_read_build_info()

  if (file.exists(".git")) {
    commit_count <- .miraprot_git_value(c("rev-list", "--count", "HEAD"))
    commit_sha <- .miraprot_git_value(c("rev-parse", "--short=7", "HEAD"))
    commit_date <- .miraprot_git_value(c("log", "-1", "--format=%cs"))
  } else {
    commit_count <- NA_character_
    commit_sha <- NA_character_
    commit_date <- NA_character_
  }

  if (is.na(commit_count)) commit_count <- build$COMMIT_COUNT %||% NA_character_
  if (is.na(commit_sha)) commit_sha <- build$COMMIT_SHA %||% NA_character_
  if (is.na(commit_date)) commit_date <- build$COMMIT_DATE %||% NA_character_

  valid_count <- !is.na(commit_count) && grepl("^[0-9]+$", commit_count)
  valid_sha <- !is.na(commit_sha) && grepl("^[0-9a-fA-F]{7,40}$", commit_sha)
  valid_date <- !is.na(commit_date) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", commit_date)

  list(
    version = if (valid_count) paste(MIRAPROT_VERSION_BASE, commit_count, sep = ".") else MIRAPROT_VERSION_BASE,
    commit = if (valid_sha) commit_sha else "unavailable",
    last_updated = if (valid_date) commit_date else "unavailable"
  )
}
