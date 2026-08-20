#!/usr/bin/env Rscript
# prebuild-cache.R — Pre-build AnnotationHub + organism cache for portable distribution
#
# Usage:
#   Rscript prebuild-cache.R <output-cache-dir> [<r-library-path>]
#
# This downloads the AnnotationHub SQLite index and the default organism
# database (org.Hs.eg.db) so the portable app starts instantly without
# needing a network download on first launch.
#
# The output directory structure matches what the Go launcher expects:
#   <output-cache-dir>/
#     annotation_cache/   — AnnotationHub SQLite index + resources
#     go_cache/           — Organism-specific RDS caches

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript prebuild-cache.R <output-cache-dir> [<r-library-path>]")
}

cache_root <- normalizePath(args[1], mustWork = FALSE)
if (length(args) >= 2) {
  lib_path <- normalizePath(args[2], mustWork = TRUE)
  r_base_lib <- file.path(R.home(), "library")
  .libPaths(c(lib_path, r_base_lib))
}

ah_cache <- file.path(cache_root, "annotation_cache")
go_cache <- file.path(cache_root, "go_cache")

dir.create(ah_cache, recursive = TRUE, showWarnings = FALSE)
dir.create(go_cache, recursive = TRUE, showWarnings = FALSE)


annotationhub_binary_available <- function(repos = BiocManager::repositories()) {
  ap <- tryCatch(utils::available.packages(repos = repos, type = "binary"), error = function(e) NULL)
  !is.null(ap) && "AnnotationHub" %in% rownames(ap)
}

ensure_bioc_pkg <- function(pkg, lib = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }

  options(repos = BiocManager::repositories())
  install_args <- list(pkgs = pkg, ask = FALSE, update = FALSE)
  if (!is.null(lib)) install_args$lib <- lib

  old_pkg_type <- getOption("pkgType")
  on.exit(options(pkgType = old_pkg_type), add = TRUE)

  if (!identical(pkg, "AnnotationHub") || annotationhub_binary_available(BiocManager::repositories())) {
    options(pkgType = "binary")
    try(do.call(BiocManager::install, install_args), silent = TRUE)
  }

  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))

  options(pkgType = "source")
  options(install.packages.check.source = "no")
  try(do.call(BiocManager::install, install_args), silent = TRUE)

  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))

  if (!requireNamespace(pkg, quietly = TRUE) && identical(pkg, "AnnotationHub")) {
    src_args <- modifyList(install_args, list(dependencies = TRUE))
    try(do.call(BiocManager::install, src_args), silent = TRUE)
  }

  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    install.packages("pkgbuild")
  }
  has_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))
  if (!has_tools && .Platform$OS.type == "windows") {
    cat("Build tools not detected. Attempting automatic Rtools installation...\n")
    try(pkgbuild::install_build_tools(quiet = TRUE), silent = TRUE)
    has_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))
  }

  if (!has_tools) {
    stop(paste0(pkg, " could not be installed and build tools were not detected."), call. = FALSE)
  }

  stop(paste0("Failed to install required Bioconductor package: ", pkg), call. = FALSE)
}

cat("=== Validating portable AnnotationHub + organism cache ===\n")
cat("AH cache dir: ", ah_cache, "\n")
cat("GO cache dir: ", go_cache, "\n\n")

ensure_bioc_pkg("AnnotationHub")
ensure_bioc_pkg("AnnotationDbi")
suppressPackageStartupMessages({
  library(AnnotationHub)
  library(AnnotationDbi)
})
Sys.setenv(ANNOTATION_HUB_CACHE = ah_cache)

orgdb_name <- "org.Hs.eg.db"
org_cache_dir <- file.path(go_cache, make.names(orgdb_name))
dir.create(org_cache_dir, recursive = TRUE, showWarnings = FALSE)
cached_sqlite <- file.path(org_cache_dir, paste0(make.names(orgdb_name), ".sqlite"))
keytypes_file <- file.path(org_cache_dir, "keytypes.rds")

load_cached_orgdb <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) return(NULL)
  tryCatch(AnnotationDbi::loadDb(path), error = function(e) NULL)
}

org_db <- load_cached_orgdb(cached_sqlite)
go_valid <- !is.null(org_db)
if (go_valid) {
  cat("Existing org.Hs.eg.db GO cache validated - download skipped.\n")
} else {
  cat("org.Hs.eg.db GO cache missing or invalid.\n")
}

# localHub=TRUE is deliberately the first AnnotationHub operation: it cannot
# update the hub over the network. Resolving q[[1]] also verifies that the
# expected resource blob, not merely an index file, is locally usable.
local_ah <- NULL
local_org_db <- NULL
ah_valid <- FALSE
if (dir.exists(ah_cache) && length(list.files(ah_cache, all.files = TRUE, no.. = TRUE)) > 0) {
  local_ah <- tryCatch(
    suppressMessages(suppressWarnings(
      AnnotationHub(localHub = TRUE, ask = FALSE, cache = ah_cache)
    )),
    error = function(e) NULL
  )
  if (!is.null(local_ah) && methods::is(local_ah, "AnnotationHub")) {
    local_query <- tryCatch(query(local_ah, c(orgdb_name, "OrgDb")), error = function(e) NULL)
    if (!is.null(local_query) && length(local_query) > 0) {
      local_org_db <- tryCatch(local_query[[1]], error = function(e) NULL)
      ah_valid <- !is.null(local_org_db)
    }
  }
}

if (ah_valid) {
  cat("Existing AnnotationHub cache validated - remote initialization skipped.\n")
} else {
  cat("AnnotationHub cache missing or invalid - building required portable cache.\n")
}

# A valid local hub can reconstruct a missing GO SQLite cache without network.
if (!go_valid && ah_valid) {
  source_sqlite <- tryCatch(AnnotationDbi::dbfile(local_org_db), error = function(e) NULL)
  if (!is.null(source_sqlite) && file.exists(source_sqlite) &&
      file.copy(source_sqlite, cached_sqlite, overwrite = TRUE)) {
    org_db <- load_cached_orgdb(cached_sqlite)
    go_valid <- !is.null(org_db)
    if (go_valid) cat("Reconstructed org.Hs.eg.db GO cache from local AnnotationHub.\n")
  }
}

# Remote access is the final fallback. A missing/invalid hub is rebuilt as one
# coherent cache; the GO SQLite is only copied when it was genuinely missing.
if (!ah_valid || !go_valid) {
  if (!ah_valid && dir.exists(ah_cache)) {
    unlink(list.files(ah_cache, full.names = TRUE, all.files = TRUE, no.. = TRUE),
           recursive = TRUE, force = TRUE)
  }
  remote_ah <- suppressMessages(suppressWarnings(
    AnnotationHub(localHub = FALSE, ask = FALSE, cache = ah_cache)
  ))
  if (is.null(remote_ah) || !methods::is(remote_ah, "AnnotationHub")) {
    stop("Failed to connect to AnnotationHub")
  }
  remote_query <- query(remote_ah, c(orgdb_name, "OrgDb"))
  if (is.null(remote_query) || length(remote_query) < 1) {
    stop("No database found for org.Hs.eg.db")
  }
  cat("Downloading org.Hs.eg.db to complete the AnnotationHub cache...\n")
  remote_org_db <- remote_query[[1]]
  if (is.null(remote_org_db)) stop("Failed to download org.Hs.eg.db")

  if (!go_valid) {
    source_sqlite <- tryCatch(AnnotationDbi::dbfile(remote_org_db), error = function(e) NULL)
    if (!is.null(source_sqlite) && file.exists(source_sqlite) &&
        file.copy(source_sqlite, cached_sqlite, overwrite = TRUE)) {
      org_db <- load_cached_orgdb(cached_sqlite)
      go_valid <- !is.null(org_db)
    }
    if (!go_valid) stop("Could not create the org.Hs.eg.db SQLite cache")
  }
}

# Missing keytypes are derived from the local SQLite and never cause a download.
keytypes_valid <- FALSE
if (file.exists(keytypes_file) && file.info(keytypes_file)$size > 0) {
  keytypes_valid <- !is.null(tryCatch(readRDS(keytypes_file), error = function(e) NULL))
}
if (!keytypes_valid && go_valid) {
  key_types <- tryCatch(AnnotationDbi::keytypes(org_db), error = function(e) NULL)
  if (!is.null(key_types) && length(key_types) > 0) {
    saveRDS(key_types, keytypes_file)
    cat("Derived and cached ", length(key_types), " key types from local SQLite.\n", sep = "")
  }
}

# Only newly reconstructed caches need portable metadata. Existing copied
# metadata is intentionally left untouched, including developer-machine paths.
metadata_file <- file.path(org_cache_dir, "cache_metadata.rds")
if (!file.exists(metadata_file) && go_valid) {
  now_str <- as.character(Sys.time())
  saveRDS(list(cache_status = "valid", source = "annotationhub",
               sqlite_path = basename(cached_sqlite), created = now_str,
               updated = now_str, ttl_days = 30, orgdb_name = orgdb_name),
          metadata_file)
  writeLines(now_str, file.path(org_cache_dir, "cache_timestamp.txt"))
  saveRDS(list(available = TRUE), file.path(org_cache_dir, "organism_db.rds"))
}

cat("\n=== Cache pre-build complete ===\n")
Sys.unsetenv("ANNOTATION_HUB_CACHE")
