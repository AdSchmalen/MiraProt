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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

valid_keytypes <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) return(FALSE)
  value <- tryCatch(readRDS(path), error = function(e) NULL)
  is.character(value) && length(value) > 0
}

common_species <- c(
  "Homo sapiens" = "org.Hs.eg.db", "Mus musculus" = "org.Mm.eg.db",
  "Rattus norvegicus" = "org.Rn.eg.db", "Drosophila melanogaster" = "org.Dm.eg.db",
  "Danio rerio" = "org.Dr.eg.db", "Caenorhabditis elegans" = "org.Ce.eg.db",
  "Saccharomyces cerevisiae" = "org.Sc.sgd.db", "Equus caballus" = "org.Ec.eg.db"
)
species_map_file <- file.path(go_cache, "species_map.rds")
species_map <- if (file.exists(species_map_file)) {
  tryCatch(readRDS(species_map_file), error = function(e) NULL)
} else NULL
if (is.null(species_map) || is.null(names(species_map))) species_map <- common_species

species_for_orgdb <- function(name) {
  matches <- names(species_map)[unname(species_map) == name]
  if (length(matches)) return(matches[[1]])
  matches <- names(common_species)[unname(common_species) == name]
  if (length(matches)) matches[[1]] else NA_character_
}

write_portable_metadata <- function(org_dir, name, sqlite) {
  metadata_file <- file.path(org_dir, "cache_metadata.rds")
  meta <- if (file.exists(metadata_file)) {
    tryCatch(readRDS(metadata_file), error = function(e) NULL)
  } else NULL
  if (!is.list(meta)) meta <- list()
  now_str <- as.character(Sys.time())
  meta$cache_status <- "valid"
  meta$source <- meta$source %||% "annotationhub"
  # A basename is deliberately relocation-safe and is resolved relative to
  # this organism directory by the portable runtime.
  meta$sqlite_path <- basename(sqlite)
  meta$created <- meta$created %||% now_str
  meta$updated <- meta$updated %||% now_str
  meta$ttl_days <- meta$ttl_days %||% 30
  meta$orgdb_name <- name
  saveRDS(meta, metadata_file)
  writeLines(meta$updated, file.path(org_dir, "cache_timestamp.txt"))
  saveRDS(list(available = TRUE), file.path(org_dir, "organism_db.rds"))
}

resolve_local_orgdb <- function(cache_dir, name) {
  if (!dir.exists(cache_dir) || !length(list.files(cache_dir, all.files = TRUE, no.. = TRUE))) return(NULL)
  ah <- tryCatch(suppressMessages(suppressWarnings(
    AnnotationHub(localHub = TRUE, ask = FALSE, cache = cache_dir)
  )), error = function(e) NULL)
  if (is.null(ah) || !methods::is(ah, "AnnotationHub")) return(NULL)
  species <- species_for_orgdb(name)
  queries <- if (!is.na(species)) list(c(species, "OrgDb"), c(name, "OrgDb")) else list(c(name, "OrgDb"))
  for (terms in queries) {
    q <- tryCatch(query(ah, terms), error = function(e) NULL)
    if (!is.null(q) && length(q)) {
      value <- tryCatch(q[[1]], error = function(e) NULL)
      if (!is.null(value)) return(value)
    }
  }
  NULL
}

normalize_organism <- function(org_dir) {
  name <- basename(org_dir)
  sqlite <- file.path(org_dir, paste0(make.names(name), ".sqlite"))
  keytypes <- file.path(org_dir, "keytypes.rds")
  db <- load_cached_orgdb(sqlite)
  if (is.null(db)) {
    local_db <- resolve_local_orgdb(file.path(org_dir, "ah_cache"), name)
    source_sqlite <- if (!is.null(local_db)) tryCatch(AnnotationDbi::dbfile(local_db), error = function(e) NULL) else NULL
    if (!is.null(source_sqlite) && file.exists(source_sqlite) &&
        file.copy(source_sqlite, sqlite, overwrite = TRUE)) {
      db <- load_cached_orgdb(sqlite)
      if (!is.null(db)) cat("Normalized ", name, " from its offline nested ah_cache.\n", sep = "")
    }
  } else {
    cat("Validated copied canonical cache for ", name, ".\n", sep = "")
  }
  if (is.null(db)) {
    cat("No usable offline cache for ", name, "; leaving it for runtime fallback.\n", sep = "")
    return(NULL)
  }
  if (!valid_keytypes(keytypes)) {
    values <- tryCatch(AnnotationDbi::keytypes(db), error = function(e) NULL)
    if (is.character(values) && length(values)) {
      saveRDS(values, keytypes)
      cat("Derived ", length(values), " key types for ", name, ".\n", sep = "")
    }
  }
  write_portable_metadata(org_dir, name, sqlite)
  db
}

# Convert each copied source cache independently and strictly offline. Nested
# BiocFileCache directories are opened as units; their indexes are never merged.
organism_dirs <- list.dirs(go_cache, recursive = FALSE, full.names = TRUE)
organism_dirs <- organism_dirs[grepl("^org\\.[A-Za-z0-9]+\\.[A-Za-z0-9]+\\.db$", basename(organism_dirs))]
cat("=== Offline normalization of copied organism caches ===\n")
invisible(lapply(organism_dirs, normalize_organism))

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

# This also rewrites copied absolute source-machine paths as a basename.
if (go_valid) write_portable_metadata(org_cache_dir, orgdb_name, cached_sqlite)

cat("\n=== Cache pre-build complete ===\n")
Sys.unsetenv("ANNOTATION_HUB_CACHE")
