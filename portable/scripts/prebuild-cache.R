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

cat("=== Pre-building AnnotationHub cache ===\n")
cat("AH cache dir: ", ah_cache, "\n")
cat("GO cache dir: ", go_cache, "\n\n")

# Ensure required packages are available
ensure_bioc_pkg("AnnotationHub")
ensure_bioc_pkg("AnnotationDbi")

# Load required packages
suppressPackageStartupMessages({
  library(AnnotationHub)
  library(AnnotationDbi)
})

# --- Step 1: Download AnnotationHub index and org.Hs.eg.db resource ---
cat("--- Downloading AnnotationHub index + org.Hs.eg.db ---\n")

Sys.setenv(ANNOTATION_HUB_CACHE = ah_cache)

ah <- suppressMessages(suppressWarnings(
  AnnotationHub(localHub = FALSE, ask = FALSE, cache = ah_cache)
))

if (is.null(ah) || !methods::is(ah, "AnnotationHub")) {
  stop("Failed to connect to AnnotationHub")
}

cat("Connected to AnnotationHub, querying org.Hs.eg.db...\n")

q <- query(ah, c("org.Hs.eg.db", "OrgDb"))
if (is.null(q) || length(q) < 1) {
  stop("No database found for org.Hs.eg.db")
}

cat("Downloading org.Hs.eg.db...\n")
org_db <- q[[1]]

if (is.null(org_db)) {
  stop("Failed to download org.Hs.eg.db")
}
cat("org.Hs.eg.db downloaded successfully\n\n")

# --- Step 2: Save organism-specific SQLite-backed cache ---
cat("--- Saving organism cache (SQLite-backed) ---\n")

org_cache_dir <- file.path(go_cache, make.names("org.Hs.eg.db"))
dir.create(org_cache_dir, recursive = TRUE, showWarnings = FALSE)

# Resolve the underlying SQLite file from the live OrgDb object and copy it
# into the organism cache directory. This is the same approach used at runtime
# by save_organism_cache() in GO_module_hub.R.
source_sqlite <- tryCatch(AnnotationDbi::dbfile(org_db), error = function(e) NULL)
cached_sqlite <- NULL

if (!is.null(source_sqlite) && file.exists(source_sqlite)) {
  dest_sqlite <- file.path(org_cache_dir, paste0(make.names("org.Hs.eg.db"), ".sqlite"))
  if (file.copy(source_sqlite, dest_sqlite, overwrite = TRUE)) {
    cached_sqlite <- dest_sqlite
    cat("Copied SQLite file:", source_sqlite, "->", dest_sqlite, "\n")
  } else {
    cat("Warning: could not copy SQLite file\n")
  }
} else {
  cat("Warning: could not resolve SQLite path from OrgDb\n")
}

# Write structured cache metadata (new format).
# Use the basename only for sqlite_path so the metadata is portable.
# load_organism_cache() resolves missing absolute paths via the canonical
# fallback (cache_dir + orgdb_name + ".sqlite") and updates the metadata
# on first load.
now_str <- as.character(Sys.time())
cache_meta <- list(
  cache_status = if (!is.null(cached_sqlite)) "valid" else "marker_only",
  source       = "annotationhub",
  sqlite_path  = if (!is.null(cached_sqlite)) basename(cached_sqlite) else "",
  created      = now_str,
  updated      = now_str,
  ttl_days     = 30,
  orgdb_name   = "org.Hs.eg.db"
)
saveRDS(cache_meta, file.path(org_cache_dir, "cache_metadata.rds"))
writeLines(now_str, file.path(org_cache_dir, "cache_timestamp.txt"))

# Also write legacy marker for backward compatibility
saveRDS(list(available = TRUE), file.path(org_cache_dir, "organism_db.rds"))

cat("Saved organism cache to:", org_cache_dir, "\n")

# --- Step 3: Extract and cache key types ---
cat("--- Caching key types ---\n")

key_types <- tryCatch(
  AnnotationDbi::keytypes(org_db),
  error = function(e) {
    cat("Warning: could not extract key types:", e$message, "\n")
    NULL
  }
)

if (!is.null(key_types) && length(key_types) > 0) {
  keytypes_file <- file.path(org_cache_dir, "keytypes.rds")
  saveRDS(key_types, keytypes_file)
  cat("Cached", length(key_types), "key types\n")
}

# --- Done ---
cat("\n=== Cache pre-build complete ===\n")
cat("Contents:\n")
for (f in list.files(cache_root, recursive = TRUE)) {
  full <- file.path(cache_root, f)
  size_kb <- round(file.info(full)$size / 1024, 1)
  cat(sprintf("  %s (%s KB)\n", f, size_kb))
}

Sys.unsetenv("ANNOTATION_HUB_CACHE")
cat("\nDone.\n")
