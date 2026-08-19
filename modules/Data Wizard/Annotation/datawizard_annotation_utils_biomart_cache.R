# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_cache.R
#
# Purpose:
#   BioMart disk cache I/O functions.  Manages persistent storage of species
#   lists, per-species keytypes, full mapping tables, and metadata manifests
#   on disk so that Ensembl does not need to be queried on every session start.
#
# Architectural Role:
#   Persistence layer for BioMart metadata and mapping data.
#   Sourced into modEnv via datawizard_annotation.R. All functions are
#   available to observers and other utils in the same environment.
#
# Key Responsibilities:
#   - Determine the cache directory (portable / project / temp fallback).
#   - Save and load species data frames and per-species keytypes as RDS.
#   - Save and load full BioMart mapping tables as RDS (disk persistence for
#     cross-session reuse, analogous to the AnnotationHub SQLite caching).
#   - Provide atomic cache writes to prevent corruption.
#   - Invalidate (delete) the cache for forced refresh.
#   - Warm the session cache by bulk-loading from disk.
#   - Save and load the metadata manifest (timestamps, build status).
#
# Public Functions:
#   1.  get_biomart_cache_dir()                - Cache base directory path
#   2.  save_biomart_species_cache()           - Save species DF to disk
#   3.  load_biomart_species_cache()           - Load species DF from disk
#   4.  save_biomart_keytypes_cache()          - Save per-species keytypes
#   5.  save_biomart_keytypes_cache_atomic()   - Atomic per-species keytype write
#   6.  load_biomart_keytypes_cache()          - Load per-species keytypes
#   7.  invalidate_biomart_cache()             - Delete cache for refresh
#   7a. invalidate_biomart_keytype_cache()     - Delete species/keytypes cache only
#   7b. invalidate_biomart_database_cache()    - Delete mapping tables cache only
#   8.  warm_biomart_session_cache()           - Bulk-load disk keytypes into session
#   9.  save_biomart_metadata_manifest()       - Save cache manifest
#   10. load_biomart_metadata_manifest()       - Load cache manifest
#   11. save_biomart_mapping_table()           - Persist full mapping table to disk
#   12. load_biomart_mapping_table()           - Load mapping table from disk
#   13. get_biomart_mapping_cache_timestamp()  - Read mapping table cache timestamp
#
# Dependencies:
#   - Base R file I/O (readRDS, saveRDS, file.path, dir.create, unlink)
#
# Integration Points:
#   - Called by build utils to persist fetched metadata.
#   - Called by observers to load cached data on session start.
#   - get_biomart_cache_dir() used by all cache-related functions.
#   - save/load_biomart_mapping_table() called by biomart_map_ids() to persist
#     full mapping tables across sessions.
#
# Guidance for Future Developers:
#   - Cache format changes require incrementing a version marker or
#     invalidating existing caches to avoid deserialization errors.
#   - Atomic writes (temp + rename) prevent partial-read corruption.
#   - The mapping_tables/ subdirectory uses cache_key as the subdirectory name
#     (colons replaced by underscores for filesystem compatibility).
# ==============================================================================

#' Return the BioMart disk cache base directory.
#'
#' Follows the same location priority as the GO module organism cache:
#' \enumerate{
#'   \item \code{MIRAPROT_GO_CACHE} environment variable root (portable mode).
#'   \item \code{cache/BioMart_Cache/} relative to \code{getwd()}.
#'   \item \code{tempdir()/MiraProt_BioMart_Cache} if project path is not writable.
#' }
#'
#' @return Character path to the BioMart cache base directory.
get_biomart_cache_dir <- function() {
  portable_root <- Sys.getenv("MIRAPROT_GO_CACHE", "")
  if (nzchar(portable_root)) {
    return(file.path(portable_root, "BioMart_Cache"))
  }

  project_path <- file.path(getwd(), "cache", "BioMart_Cache")
  cache_parent <- dirname(project_path)
  if (dir.exists(cache_parent) ||
      isTRUE(dir.create(cache_parent, recursive = TRUE, showWarnings = FALSE))) {
    return(project_path)
  }

  file.path(tempdir(), "MiraProt_BioMart_Cache")
}


#' Save BioMart species data frame to disk cache.
#'
#' Writes \code{species_list.rds} and \code{species_timestamp.txt} to the
#' \code{species/} subdirectory of the BioMart cache.  Mirrors
#' \code{save_organism_cache()} from \code{GO_module_hub.R}.
#'
#' @param species_df \code{data.frame} with columns \code{scientific_name} and
#'   \code{dataset}.
#' @param debug_log Logging function with signature (message, level).
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
save_biomart_species_cache <- function(species_df,
                                       debug_log = function(m, l = 1) {}) {
  if (is.null(species_df) || !is.data.frame(species_df) || nrow(species_df) == 0) {
    return(FALSE)
  }

  tryCatch({
    cache_dir   <- file.path(get_biomart_cache_dir(), "species")
    cache_file  <- file.path(cache_dir, "species_list.rds")
    ts_file     <- file.path(cache_dir, "species_timestamp.txt")

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    saveRDS(species_df, cache_file)
    writeLines(as.character(Sys.time()), ts_file)

    debug_log(sprintf("save_biomart_species_cache: saved %d species to %s",
                      nrow(species_df), cache_file), 1)
    TRUE
  }, error = function(e) {
    debug_log(sprintf("save_biomart_species_cache: failed (%s)", e$message), 1)
    FALSE
  })
}


#' Load BioMart species data frame from disk cache.
#'
#' Returns the cached \code{data.frame} if the cache file exists and is valid.
#' No age-based expiration is applied: the BioMart cache is persistent and only
#' refreshed when explicitly requested by the user (Update Organisms) or when
#' the cache is missing entirely.  Timestamps are retained for reporting/audit
#' purposes only.
#'
#' @param debug_log Logging function with signature (message, level).
#' @param ... Ignored. Accepts (and silently drops) legacy arguments such as
#'   \code{max_cache_age_days} for backward compatibility with callers that
#'   have not yet been updated.
#' @return \code{data.frame} on cache hit, \code{NULL} on miss.
load_biomart_species_cache <- function(debug_log = function(m, l = 1) {}, ...) {
  tryCatch({
    cache_dir  <- file.path(get_biomart_cache_dir(), "species")
    cache_file <- file.path(cache_dir, "species_list.rds")
    ts_file    <- file.path(cache_dir, "species_timestamp.txt")

    if (!file.exists(cache_file) || !file.exists(ts_file)) {
      debug_log("load_biomart_species_cache: no cache found", 2)
      return(NULL)
    }

    timestamp   <- readLines(ts_file)[1]
    cache_time  <- as.POSIXct(timestamp)
    age_days    <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))

    species_df <- readRDS(cache_file)

    if (!is.data.frame(species_df) || nrow(species_df) == 0) {
      debug_log("load_biomart_species_cache: cache file is invalid", 1)
      return(NULL)
    }

    debug_log(sprintf(
      "load_biomart_species_cache: loaded %d species from cache (age: %.1f days, persistent/no TTL)",
      nrow(species_df), age_days), 1)

    species_df
  }, error = function(e) {
    debug_log(sprintf("load_biomart_species_cache: error (%s)", e$message), 1)
    NULL
  })
}


#' Save BioMart keytypes for a species to disk cache.
#'
#' Writes \code{keytypes.rds} and \code{keytypes_timestamp.txt} to a
#' per-species subdirectory under \code{keytypes/} in the BioMart cache.
#' Species name is sanitized (spaces replaced by underscores) for use as a
#' directory name.  Mirrors \code{save_keytypes_to_cache()} from
#' \code{GO_module_hub.R}.
#'
#' @param species_name Character scientific name (e.g. "Mus musculus").
#' @param keytypes Character vector of OrgDb-style key type names.
#' @param debug_log Logging function with signature (message, level).
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
save_biomart_keytypes_cache <- function(species_name, keytypes,
                                        debug_log = function(m, l = 1) {}) {
  if (is.null(species_name) || is.null(keytypes) || length(keytypes) == 0) {
    return(FALSE)
  }

  tryCatch({
    safe_name  <- gsub(" ", "_", species_name, fixed = TRUE)
    cache_dir  <- file.path(get_biomart_cache_dir(), "keytypes", safe_name)
    cache_file <- file.path(cache_dir, "keytypes.rds")
    ts_file    <- file.path(cache_dir, "keytypes_timestamp.txt")

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    saveRDS(keytypes, cache_file)
    writeLines(as.character(Sys.time()), ts_file)

    debug_log(sprintf("save_biomart_keytypes_cache: saved %d keytypes for %s",
                      length(keytypes), species_name), 1)
    TRUE
  }, error = function(e) {
    debug_log(sprintf("save_biomart_keytypes_cache: failed for %s (%s)",
                      species_name, e$message), 1)
    FALSE
  })
}


#' Atomically save BioMart keytypes for a species to disk cache.
#'
#' Writes keytypes to a temporary staging file, validates the write, then
#' renames the staging file into the active cache location. This ensures
#' that an interrupted write never corrupts the active cache entry.
#'
#' @param species_name Character scientific name (e.g. "Mus musculus").
#' @param keytypes Character vector of OrgDb-style key type names.
#' @param debug_log Logging function with signature (message, level).
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
save_biomart_keytypes_cache_atomic <- function(species_name, keytypes,
                                                debug_log = function(m, l = 1) {}) {
  if (is.null(species_name) || is.null(keytypes) || length(keytypes) == 0) {
    return(FALSE)
  }

  tryCatch({
    safe_name  <- gsub(" ", "_", species_name, fixed = TRUE)
    cache_dir  <- file.path(get_biomart_cache_dir(), "keytypes", safe_name)
    cache_file <- file.path(cache_dir, "keytypes.rds")
    ts_file    <- file.path(cache_dir, "keytypes_timestamp.txt")

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Write to temporary staging files first
    tmp_cache_file <- paste0(cache_file, ".tmp")
    tmp_ts_file    <- paste0(ts_file, ".tmp")

    saveRDS(keytypes, tmp_cache_file)

    # Validate the staging file is readable and matches
    staged <- tryCatch(readRDS(tmp_cache_file), error = function(e) NULL)
    if (is.null(staged) || !is.character(staged) || length(staged) != length(keytypes)) {
      debug_log(sprintf(
        "save_biomart_keytypes_cache_atomic: staging validation failed for %s",
        species_name), 1)
      tryCatch(unlink(tmp_cache_file), error = function(e) NULL)
      return(FALSE)
    }

    writeLines(as.character(Sys.time()), tmp_ts_file)

    # Atomic rename: swap staging files into active location
    file.rename(tmp_cache_file, cache_file)
    file.rename(tmp_ts_file, ts_file)

    # Post-rename validation
    if (!file.exists(cache_file)) {
      debug_log(sprintf(
        "save_biomart_keytypes_cache_atomic: post-rename validation failed for %s",
        species_name), 1)
      return(FALSE)
    }

    debug_log(sprintf(
      "save_biomart_keytypes_cache_atomic: saved %d keytypes for %s (atomic)",
      length(keytypes), species_name), 2)
    TRUE
  }, error = function(e) {
    debug_log(sprintf(
      "save_biomart_keytypes_cache_atomic: failed for %s (%s)",
      species_name, e$message), 1)
    # Clean up staging files on error
    tryCatch({
      safe_name  <- gsub(" ", "_", species_name, fixed = TRUE)
      cache_dir  <- file.path(get_biomart_cache_dir(), "keytypes", safe_name)
      tmp_cache  <- file.path(cache_dir, "keytypes.rds.tmp")
      tmp_ts     <- file.path(cache_dir, "keytypes_timestamp.txt.tmp")
      if (file.exists(tmp_cache)) unlink(tmp_cache)
      if (file.exists(tmp_ts))    unlink(tmp_ts)
    }, error = function(e2) NULL)
    FALSE
  })
}


#' Load BioMart keytypes for a species from disk cache.
#'
#' Returns the cached keytypes vector if valid.  No age-based expiration is
#' applied: the BioMart keytype cache is persistent and only refreshed when
#' explicitly requested by the user or when the cache is missing entirely.
#' Timestamps are retained for reporting/audit purposes only.
#'
#' @param species_name Character scientific name (e.g. "Mus musculus").
#' @param debug_log Logging function with signature (message, level).
#' @param ... Ignored. Accepts (and silently drops) legacy arguments such as
#'   \code{max_cache_age_days} for backward compatibility with callers that
#'   have not yet been updated.
#' @return Character vector of key types on cache hit, \code{NULL} otherwise.
load_biomart_keytypes_cache <- function(species_name,
                                        debug_log = function(m, l = 1) {}, ...) {
  if (is.null(species_name) || !nzchar(species_name)) return(NULL)

  tryCatch({
    safe_name  <- gsub(" ", "_", species_name, fixed = TRUE)
    cache_dir  <- file.path(get_biomart_cache_dir(), "keytypes", safe_name)
    cache_file <- file.path(cache_dir, "keytypes.rds")
    ts_file    <- file.path(cache_dir, "keytypes_timestamp.txt")

    if (!file.exists(cache_file) || !file.exists(ts_file)) {
      debug_log(sprintf("load_biomart_keytypes_cache: no cache for %s", species_name), 2)
      return(NULL)
    }

    timestamp   <- readLines(ts_file)[1]
    cache_time  <- as.POSIXct(timestamp)
    age_days    <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))

    keytypes <- readRDS(cache_file)
    if (!is.character(keytypes) || length(keytypes) == 0) {
      debug_log(sprintf("load_biomart_keytypes_cache: invalid cache for %s", species_name), 1)
      return(NULL)
    }

    debug_log(sprintf(
      "load_biomart_keytypes_cache: loaded %d keytypes for %s (age: %.1f days, persistent/no TTL)",
      length(keytypes), species_name, age_days), 1)

    keytypes
  }, error = function(e) {
    debug_log(sprintf("load_biomart_keytypes_cache: error for %s (%s)",
                      species_name, e$message), 1)
    NULL
  })
}


#' Invalidate the BioMart disk cache.
#'
#' Deletes the \code{species/}, \code{keytypes/}, and \code{mapping_tables/}
#' subdirectories inside the BioMart cache directory.  Called by
#' "Update Organisms" in BioMart mode to force a fresh fetch on the next
#' activation.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return Invisible \code{NULL}.
invalidate_biomart_cache <- function(debug_log = function(m, l = 1) {}) {
  tryCatch({
    base_dir <- get_biomart_cache_dir()
    species_dir   <- file.path(base_dir, "species")
    keytypes_dir  <- file.path(base_dir, "keytypes")
    mapping_dir   <- file.path(base_dir, "mapping_tables")

    if (dir.exists(species_dir)) {
      unlink(species_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_cache: removed %s", species_dir), 1)
    }
    if (dir.exists(keytypes_dir)) {
      unlink(keytypes_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_cache: removed %s", keytypes_dir), 1)
    }
    if (dir.exists(mapping_dir)) {
      unlink(mapping_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_cache: removed %s", mapping_dir), 1)
    }
  }, error = function(e) {
    debug_log(sprintf("invalidate_biomart_cache: error (%s)", e$message), 1)
  })
  invisible(NULL)
}


#' Invalidate only the BioMart species and keytypes disk cache.
#'
#' Deletes the \code{species/} and \code{keytypes/} subdirectories inside the
#' BioMart cache directory while preserving the \code{mapping_tables/}
#' directory.  The next operation that requires species or keytype metadata
#' will re-download it from BioMart.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return Invisible \code{NULL}.
invalidate_biomart_keytype_cache <- function(debug_log = function(m, l = 1) {}) {
  tryCatch({
    base_dir <- get_biomart_cache_dir()
    species_dir  <- file.path(base_dir, "species")
    keytypes_dir <- file.path(base_dir, "keytypes")

    if (dir.exists(species_dir)) {
      unlink(species_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_keytype_cache: removed %s", species_dir), 1)
    }
    if (dir.exists(keytypes_dir)) {
      unlink(keytypes_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_keytype_cache: removed %s", keytypes_dir), 1)
    }
  }, error = function(e) {
    debug_log(sprintf("invalidate_biomart_keytype_cache: error (%s)", e$message), 1)
  })
  invisible(NULL)
}


#' Invalidate only the BioMart mapping tables disk cache.
#'
#' Deletes the \code{mapping_tables/} subdirectory inside the BioMart cache
#' directory while preserving the \code{species/} and \code{keytypes/}
#' directories.  The next ID mapping operation will need to download the
#' required mapping tables from BioMart again.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return Invisible \code{NULL}.
invalidate_biomart_database_cache <- function(debug_log = function(m, l = 1) {}) {
  tryCatch({
    base_dir <- get_biomart_cache_dir()
    mapping_dir <- file.path(base_dir, "mapping_tables")

    if (dir.exists(mapping_dir)) {
      unlink(mapping_dir, recursive = TRUE)
      debug_log(sprintf("invalidate_biomart_database_cache: removed %s", mapping_dir), 1)
    }
  }, error = function(e) {
    debug_log(sprintf("invalidate_biomart_database_cache: error (%s)", e$message), 1)
  })
  invisible(NULL)
}


#' Warm BioMart session keytype cache from disk for a set of species.
#'
#' Reads per-species keytype RDS files from disk cache into a named list
#' suitable for storing in \code{cached_biomart_keytypes()}.  Species with
#' no valid disk cache are silently skipped (they will be fetched on demand
#' when selected).
#'
#' This is intended to be called once during the cross-species toggle-ON
#' sequence to prepopulate the session cache and make subsequent species
#' switches instant (session cache lookup only, no disk I/O).
#'
#' @section BioMart keytype caching strategy:
#' The module implements a \strong{full persistent cache} strategy:
#' \itemize{
#'   \item A complete BioMart metadata cache containing all discoverable species
#'         and their keytypes is built and persisted to disk.
#'   \item The cache does not auto-expire.  Refresh occurs only when:
#'         (1) cache is missing globally or for a specific species, or
#'         (2) the user explicitly clicks Update Organisms in BioMart mode.
#'   \item On first cross-species activation, all available species keytypes
#'         are bulk-loaded from disk cache into the session-level cache.
#'   \item Timestamps are stored for reporting/auditing but are not used
#'         for expiration logic.
#' }
#'
#' @param species Character vector of species names to warm up.
#'   If NULL, loads all species found in the disk cache.
#' @param existing_cache Named list of species -> keytypes already in session.
#' @param debug_log Logging function with signature (message, level).
#' @param ... Ignored. Accepts (and silently drops) legacy arguments such as
#'   \code{max_cache_age_days} for backward compatibility with callers that
#'   have not yet been updated.
#' @return Named list merging \code{existing_cache} with any newly loaded
#'   species keytypes.
warm_biomart_session_cache <- function(species = NULL,
                                       existing_cache = list(),
                                       debug_log = function(m, l = 1) {}, ...) {
  loaded <- 0L

  # If no species list provided, discover all cached species from disk
  if (is.null(species)) {
    keytypes_dir <- file.path(get_biomart_cache_dir(), "keytypes")
    if (dir.exists(keytypes_dir)) {
      dirs <- list.dirs(keytypes_dir, recursive = FALSE, full.names = FALSE)
      species <- gsub("_", " ", dirs, fixed = TRUE)
      debug_log(sprintf("warm_biomart_session_cache: discovered %d species in disk cache", length(species)), 2)
    } else {
      species <- character(0)
    }
  }

  for (sp in species) {
    if (is.null(existing_cache[[sp]])) {
      kt <- load_biomart_keytypes_cache(sp, debug_log = debug_log)
      if (!is.null(kt) && length(kt) > 0) {
        existing_cache[[sp]] <- kt
        loaded <- loaded + 1L
      }
    }
  }
  debug_log(sprintf("warm_biomart_session_cache: loaded %d/%d species from disk cache",
                    loaded, length(species)), 2)
  existing_cache
}


#' Save the BioMart cache manifest (timestamps and status metadata).
#'
#' The manifest tracks when the full cache was created, last updated, which
#' species were successfully cached, and any partial failure information.
#' The manifest is stored as \code{cache_manifest.rds} in the BioMart cache
#' base directory.
#'
#' @param manifest Named list with fields: created_at, updated_at,
#'   species_count, keytypes_cached_count, missing_species, status.
#' @param debug_log Logging function with signature (message, level).
#' @return Logical TRUE on success, FALSE on failure.
save_biomart_metadata_manifest <- function(manifest,
                                            debug_log = function(m, l = 1) {}) {
  if (is.null(manifest) || !is.list(manifest)) return(FALSE)

  tryCatch({
    base_dir <- get_biomart_cache_dir()
    if (!dir.exists(base_dir)) {
      dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    }
    manifest_file <- file.path(base_dir, "cache_manifest.rds")
    saveRDS(manifest, manifest_file)
    debug_log(sprintf(
      "save_biomart_metadata_manifest: saved manifest (species=%d, keytypes_cached=%d, status=%s)",
      manifest$species_count %||% 0L,
      manifest$keytypes_cached_count %||% 0L,
      manifest$status %||% "unknown"), 1)
    TRUE
  }, error = function(e) {
    debug_log(sprintf("save_biomart_metadata_manifest: failed (%s)", e$message), 1)
    FALSE
  })
}


#' Load the BioMart cache manifest from disk.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return Named list on success, NULL if no manifest exists.
load_biomart_metadata_manifest <- function(debug_log = function(m, l = 1) {}) {
  tryCatch({
    manifest_file <- file.path(get_biomart_cache_dir(), "cache_manifest.rds")
    if (!file.exists(manifest_file)) {
      debug_log("load_biomart_metadata_manifest: no manifest found", 2)
      return(NULL)
    }
    manifest <- readRDS(manifest_file)
    if (!is.list(manifest)) {
      debug_log("load_biomart_metadata_manifest: invalid manifest format", 1)
      return(NULL)
    }
    debug_log(sprintf(
      "load_biomart_metadata_manifest: loaded (status=%s, species=%d, updated=%s)",
      manifest$status %||% "unknown",
      manifest$species_count %||% 0L,
      manifest$updated_at %||% "unknown"), 1)
    manifest
  }, error = function(e) {
    debug_log(sprintf("load_biomart_metadata_manifest: error (%s)", e$message), 1)
    NULL
  })
}


# =============================================================================
# Mapping table disk cache
# =============================================================================

#' Persist a full BioMart mapping table to disk.
#'
#' Saves the data.frame returned by an unfiltered \code{biomaRt::getBM()} call
#' (the complete source->target attribute mapping table) to the
#' \code{mapping_tables/} subdirectory of the BioMart cache.  A companion
#' timestamp file is written alongside the data so that the UI can report when
#' the local mapping database was last refreshed.
#'
#' The on-disk layout mirrors the keytype cache structure:
#' \preformatted{
#'   <cache_base>/mapping_tables/<safe_key>/mapping_table.rds
#'   <cache_base>/mapping_tables/<safe_key>/mapping_timestamp.txt
#' }
#'
#' where \code{safe_key} is the session cache key with colons replaced by
#' double underscores (e.g. \code{hsapiens_gene_ensembl__ensembl_gene_id__mmusculus_homolog_ensembl_gene}).
#'
#' @param cache_key Character cache key (e.g.
#'   \code{"hsapiens_gene_ensembl:ensembl_gene_id:mmusculus_homolog_ensembl_gene"}).
#' @param table A \code{data.frame} with at least two columns.
#' @param debug_log Logging function with signature (message, level).
#' @return Logical \code{TRUE} on success, \code{FALSE} on failure.
save_biomart_mapping_table <- function(cache_key, table,
                                       debug_log = function(m, l = 1) {}) {
  if (is.null(cache_key) || !nzchar(cache_key) ||
      is.null(table) || !is.data.frame(table) || nrow(table) == 0) {
    return(FALSE)
  }

  tryCatch({
    safe_key   <- gsub(":", "__", cache_key, fixed = TRUE)
    cache_dir  <- file.path(get_biomart_cache_dir(), "mapping_tables", safe_key)
    cache_file <- file.path(cache_dir, "mapping_table.rds")
    ts_file    <- file.path(cache_dir, "mapping_timestamp.txt")

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Atomic write: temp + rename
    tmp_file <- paste0(cache_file, ".tmp")
    saveRDS(table, tmp_file)
    file.rename(tmp_file, cache_file)

    writeLines(as.character(Sys.time()), ts_file)

    debug_log(sprintf(
      "save_biomart_mapping_table: saved %d rows for key '%s'",
      nrow(table), cache_key), 1)
    TRUE
  }, error = function(e) {
    debug_log(sprintf(
      "save_biomart_mapping_table: failed for key '%s' (%s)",
      cache_key, e$message), 1)
    FALSE
  })
}


#' Load a full BioMart mapping table from disk cache.
#'
#' Returns the previously cached data.frame if it exists and is valid.
#' No age-based expiration is applied (the mapping table cache is persistent,
#' matching the keytype cache strategy).  The cache is only refreshed when
#' explicitly invalidated by the user via Update Organisms / Clear Cache.
#'
#' @param cache_key Character cache key.
#' @param debug_log Logging function with signature (message, level).
#' @return A \code{data.frame} on cache hit, \code{NULL} on miss.
load_biomart_mapping_table <- function(cache_key,
                                       debug_log = function(m, l = 1) {}) {
  if (is.null(cache_key) || !nzchar(cache_key)) return(NULL)

  tryCatch({
    safe_key   <- gsub(":", "__", cache_key, fixed = TRUE)
    cache_dir  <- file.path(get_biomart_cache_dir(), "mapping_tables", safe_key)
    cache_file <- file.path(cache_dir, "mapping_table.rds")
    ts_file    <- file.path(cache_dir, "mapping_timestamp.txt")

    if (!file.exists(cache_file) || !file.exists(ts_file)) {
      debug_log(sprintf(
        "load_biomart_mapping_table: no disk cache for key '%s'", cache_key), 2)
      return(NULL)
    }

    table <- readRDS(cache_file)

    if (!is.data.frame(table) || ncol(table) < 2) {
      debug_log(sprintf(
        "load_biomart_mapping_table: invalid data for key '%s'", cache_key), 1)
      return(NULL)
    }

    timestamp  <- readLines(ts_file)[1]
    cache_time <- as.POSIXct(timestamp)
    age_days   <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))

    debug_log(sprintf(
      "load_biomart_mapping_table: loaded %d rows for key '%s' (age: %.1f days)",
      nrow(table), cache_key, age_days), 1)

    table
  }, error = function(e) {
    debug_log(sprintf(
      "load_biomart_mapping_table: error for key '%s' (%s)",
      cache_key, e$message), 1)
    NULL
  })
}


#' Read the timestamp of a cached BioMart mapping table.
#'
#' Returns the formatted date string from the mapping table timestamp file.
#' Used by the status display to report when the local mapping database was
#' last downloaded.  If no mapping table has been cached yet, returns
#' \code{NULL}.
#'
#' @param cache_key Character cache key.  If \code{NULL}, scans the
#'   \code{mapping_tables/} directory for the most recently updated timestamp
#'   and returns that.
#' @param debug_log Logging function with signature (message, level).
#' @return Character formatted date string (e.g. "2025-06-15 14:32"), or
#'   \code{NULL} if no cached mapping table exists.
get_biomart_mapping_cache_timestamp <- function(cache_key = NULL,
                                                 debug_log = function(m, l = 1) {}) {
  tryCatch({
    mapping_dir <- file.path(get_biomart_cache_dir(), "mapping_tables")

    if (!is.null(cache_key) && nzchar(cache_key)) {
      safe_key <- gsub(":", "__", cache_key, fixed = TRUE)
      ts_file  <- file.path(mapping_dir, safe_key, "mapping_timestamp.txt")
      if (file.exists(ts_file)) {
        ts_val <- tryCatch(as.POSIXct(readLines(ts_file, n = 1)),
                           error = function(e) NA)
        if (!is.na(ts_val)) {
          return(format(ts_val, "%Y-%m-%d %H:%M"))
        }
      }
      return(NULL)
    }

    # No specific key: find the most recent timestamp across all mapping tables
    if (!dir.exists(mapping_dir)) return(NULL)

    ts_files <- list.files(mapping_dir, pattern = "^mapping_timestamp\\.txt$",
                           recursive = TRUE, full.names = TRUE)
    if (length(ts_files) == 0) return(NULL)

    latest <- NULL
    for (tf in ts_files) {
      ts_val <- tryCatch(as.POSIXct(readLines(tf, n = 1)),
                         error = function(e) NA)
      if (!is.na(ts_val) && (is.null(latest) || ts_val > latest)) {
        latest <- ts_val
      }
    }

    if (!is.null(latest)) {
      return(format(latest, "%Y-%m-%d %H:%M"))
    }
    NULL
  }, error = function(e) {
    debug_log(sprintf("get_biomart_mapping_cache_timestamp: error (%s)", e$message), 2)
    NULL
  })
}
