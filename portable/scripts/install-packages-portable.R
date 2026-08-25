# Prefer the committed renv lockfile, while retaining install-packages.R as the
# independent recovery installer.  This file intentionally contains no package
# manifest of its own.

portable_installed <- function(library) {
  info <- utils::installed.packages(lib.loc = library, noCache = TRUE)
  if (!nrow(info)) return(setNames(character(), character()))
  setNames(info[, "Version"], rownames(info))
}

portable_fallback <- function(reason, fallback, library) {
  cat("WARNING: renv-based package restore could not be used.\n",
      "Reason: ", reason, "\n",
      "Falling back to MiraProt's package-list installer.\n\n", sep = "")
  cat("Dependency source: package-list fallback\n")
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(fallback), shQuote(library)))
  if (!identical(status, 0L)) stop("Package-list installer failed with exit code ", status)
  invisible(FALSE)
}

portable_bootstrap_renv <- function(project, work) {
  activate <- file.path(project, "renv", "activate.R")
  text <- if (file.exists(activate)) readLines(activate, warn = FALSE) else character()
  if (!length(text) || !any(grepl('version <- "[0-9]+[.][0-9]+[.][0-9]+"', text))) {
    stop("the committed renv bootstrap version could not be determined")
  }
  bootstrap_project <- file.path(work, "bootstrap")
  dir.create(file.path(bootstrap_project, "renv"), recursive = TRUE)
  file.copy(activate, file.path(bootstrap_project, "renv", "activate.R"))
  sys.source(file.path(bootstrap_project, "renv", "activate.R"), envir = globalenv())
  if (!requireNamespace("renv", quietly = TRUE)) stop("the committed renv bootstrap did not load renv")
  invisible(TRUE)
}

portable_restore_missing <- function(lockfile, library, project, bootstrap = portable_bootstrap_renv,
                                     restore = NULL, read_lock = renv::lockfile_read,
                                     write_lock = renv::lockfile_write,
                                     runtime_library = .Library) {
  original_libpaths <- .libPaths()
  on.exit(.libPaths(original_libpaths), add = TRUE)
  before <- portable_installed(library)
  work <- tempfile("miraprot-renv-")
  dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  stage <- file.path(work, "library")
  dir.create(stage)
  bootstrap_project <- file.path(work, "bootstrap")
  environment_names <- c("RENV_PROJECT", "RENV_PATHS_ROOT", "RENV_CONFIG_CACHE_ENABLED",
                         "RENV_CONFIG_SYNCHRONIZED_CHECK", "RENV_CONFIG_STARTUP_QUIET",
                         "R_LIBS_USER")
  old_environment <- Sys.getenv(environment_names, unset = NA_character_)
  on.exit({
    for (name in names(old_environment)) {
      if (is.na(old_environment[[name]])) Sys.unsetenv(name) else {
        do.call(Sys.setenv, setNames(list(old_environment[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(RENV_PROJECT = bootstrap_project,
             RENV_PATHS_ROOT = file.path(work, "renv-root"),
             RENV_CONFIG_CACHE_ENABLED = "FALSE",
             RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE",
             RENV_CONFIG_STARTUP_QUIET = "TRUE",
             R_LIBS_USER = stage)
  bootstrap(project, work)
  lock <- read_lock(lockfile)
  if (!is.list(lock$R) || !is.character(lock$R$Version) || length(lock$R$Version) != 1L ||
      !is.list(lock$Packages) || !length(names(lock$Packages))) {
    stop("renv.lock is missing valid R or Packages sections")
  }
  if (!identical(lock$R$Version, as.character(getRversion()))) {
    stop(sprintf("renv.lock requires R %s but the staged portable runtime is R %s",
                 lock$R$Version, getRversion()))
  }
  runtime <- portable_installed(runtime_library)
  missing <- setdiff(names(lock$Packages), union(names(before), names(runtime)))
  differing <- intersect(names(before), names(lock$Packages))
  differing <- differing[vapply(differing, function(pkg) {
    recorded <- lock$Packages[[pkg]]$Version
    is.character(recorded) && length(recorded) == 1L && !identical(before[[pkg]], recorded)
  }, logical(1))]
  if (length(differing)) {
    cat("Preserving installed packages whose versions differ from renv.lock: ",
        paste(differing, collapse = ", "), "\n", sep = "")
  }
  if (!length(missing)) {
    cat("Dependency source: renv.lock\nInstalled 0 missing packages.\nPreserved ",
        length(before), " existing portable packages.\n", sep = "")
    return(invisible(TRUE))
  }
  cat("Restoring genuinely missing packages: ", paste(missing, collapse = ", "), "\n", sep = "")
  # renv's packages= argument selects requested packages, but restore still
  # synchronizes records from the supplied lockfile. Give it only the records
  # it is permitted to stage so preserved version mismatches cannot become
  # incidental upgrades or downgrades.
  selected <- lock
  selected$Packages <- lock$Packages[missing]
  selected_lock <- file.path(work, "renv.lock")
  write_lock(selected, selected_lock)
  restore_libraries <- unique(c(stage, library, runtime_library))
  if (is.null(restore)) restore <- function(stage, missing, libraries, lockfile) {
    renv::restore(project = work, lockfile = lockfile, library = libraries,
                  packages = missing, clean = FALSE, prompt = FALSE, retry = FALSE)
  }
  # Source builds may load dependencies while installing. The destination is
  # therefore readable, but the explicit staging library remains the only
  # library renv is allowed to write.
  .libPaths(unique(c(restore_libraries, original_libpaths)))
  restore_error <- tryCatch({
    restore(stage, missing, restore_libraries, selected_lock)
    NULL
  }, error = identity)
  if (!identical(portable_installed(library), before)) {
    stop("the portable library changed during staged restore")
  }
  if (inherits(restore_error, "error")) stop(restore_error)
  staged <- portable_installed(stage)
  absent <- setdiff(missing, names(staged))
  if (length(absent)) stop("renv did not install required packages: ", paste(absent, collapse = ", "))
  for (pkg in missing) {
    destination <- file.path(library, pkg)
    if (dir.exists(destination)) stop("package appeared in portable library during restore: ", pkg)
    source <- file.path(stage, pkg)
    if (!file.rename(source, destination)) {
      dir.create(destination)
      entries <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      copied <- length(entries) == 0L || all(file.copy(entries, destination, recursive = TRUE))
      if (!copied) {
        unlink(destination, recursive = TRUE, force = TRUE)
        stop("could not copy restored package into portable library: ", pkg)
      }
    }
  }
  after <- portable_installed(library)
  if (!identical(after[names(before)], before) || any(!names(before) %in% names(after))) {
    stop("an existing portable package was replaced or removed")
  }
  if (length(setdiff(missing, names(after)))) stop("restored packages are missing from portable library")
  cat("Dependency source: renv.lock\nInstalled ", length(missing),
      " missing packages.\nPreserved ", length(before), " existing portable packages.\n", sep = "")
  invisible(TRUE)
}

portable_install <- function(library, project, fallback, restore_missing = portable_restore_missing) {
  lockfile <- file.path(project, "renv.lock")
  if (!file.exists(lockfile)) return(portable_fallback("renv.lock is absent", fallback, library))
  if (is.na(file.info(lockfile)$size) || file.info(lockfile)$size < 3L) {
    return(portable_fallback("renv.lock is empty or truncated", fallback, library))
  }
  result <- tryCatch(restore_missing(lockfile, library, project), error = identity)
  if (inherits(result, "error")) return(portable_fallback(conditionMessage(result), fallback, library))
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) stop("Usage: install-packages-portable.R <target-lib> <project-root> <fallback-script>")
  library <- normalizePath(args[[1L]], mustWork = FALSE)
  dir.create(library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(library, file.path(R.home(), "library")))
  portable_install(library, normalizePath(args[[2L]]), normalizePath(args[[3L]]))
}
