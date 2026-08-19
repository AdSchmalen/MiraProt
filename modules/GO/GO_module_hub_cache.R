# ==============================================================================
# GO Module Hub - Persistent SQLite and Cache Metadata
# ==============================================================================
#
# This peer file is sourced into the same environment as GO_module_hub.R.
# Function bodies are kept here to separate hub responsibilities without
# changing resolver behavior.
# ==============================================================================

# 4. Organism-specific disk cache (SQLite-backed)
# ==============================================================================
#
# Design:
#   OrgDb objects are SQLite-backed AnnotationDbi instances. They hold a live
#   DBI connection to an underlying .sqlite file and cannot be serialized with
#   saveRDS/readRDS across R sessions (attempting this triggers infinite
#   recursion or returns a broken handle).
#
#   This cache layer avoids that problem entirely:
#     1. After a successful AnnotationHub download, the path to the actual
#        .sqlite file is resolved (from the OrgDb's internal dbfile() accessor
#        or from the AnnotationHub/BiocFileCache download directory).
#     2. A copy of that .sqlite file is placed in the organism cache directory.
#     3. On subsequent loads, the OrgDb is reconstructed in-process via
#        AnnotationDbi::loadDb() from the local copy -- no network access, no
#        serialized R object, and deterministic reload behaviour.
#
#   Cache metadata is stored in cache_metadata.rds as a structured list with:
#     - cache_status:  "valid" or "stale"
#     - source:        "annotationhub", "local_sqlite", or "package"
#     - sqlite_path:   absolute path to the cached .sqlite copy
#     - created:       ISO-8601 timestamp of initial cache creation
#     - updated:       ISO-8601 timestamp of last successful refresh
#     - ttl_days:      numeric TTL used for freshness validation
#     - orgdb_name:    OrgDb package name (e.g. "org.Hs.eg.db")
#
#   Migration: If an old marker-only layout is detected (organism_db.rds +
#   cache_timestamp.txt without cache_metadata.rds), the load path falls back
#   to the AnnotationHub re-download strategy, and a successful reload
#   automatically upgrades the cache to the new metadata format.
# ==============================================================================

# Default cache TTL in days; overridden by individual function arguments.
.ORGANISM_CACHE_TTL_DAYS <- 30

#' Get organism-specific cache directory
#'
#' @param orgdb_name organism database name
#' @return character path to organism cache directory
get_organism_cache_dir <- function(orgdb_name) {

  # Use a project-relative cache so annotation databases persist across sessions
  # and are portable when the project folder is moved to another machine.
  # Falls back to tempdir() only if the project path is not writable.
  #
  # The MIRAPROT_GO_CACHE environment variable (set by the portable launcher)
  # redirects this cache to a persistent location outside the app directory.
  env_cache <- Sys.getenv("MIRAPROT_GO_CACHE", unset = "")
  if (nzchar(env_cache)) {
    base_cache <- env_cache
  } else {
    base_cache <- file.path("cache", "GO_Cache")
  }
  tryCatch({
    if (!dir.exists(base_cache)) {
      dir.create(base_cache, recursive = TRUE)
    }
  }, error = function(e) {
    # Fall back to tempdir() if project dir is not writable (e.g. read-only mount)
    base_cache <<- file.path(tempdir(), "MiraProt_GO_Cache")
    if (!dir.exists(base_cache)) {
      dir.create(base_cache, recursive = TRUE)
    }
  })

  # Create organism-specific subdirectory
  org_cache <- file.path(base_cache, make.names(orgdb_name))
  if (!dir.exists(org_cache)) {
    dir.create(org_cache, recursive = TRUE)
  }

  return(org_cache)
}

#' Get persistent AnnotationHub cache directory for an organism
#'
#' In portable mode this returns the directory pointed to by the
#' ANNOTATION_HUB_CACHE environment variable (set by the launcher).
#' Otherwise it returns a persistent subdirectory of the organism cache dir
#' so that downloaded SQLite files are reused across R sessions.
#'
#' @param orgdb_name organism database name (e.g., "org.Hs.eg.db")
#' @return character path to the AH cache directory (created if absent)
get_organism_ah_cache_dir <- function(orgdb_name) {
  if (.has_persistent_ah_cache()) {
    return(Sys.getenv("ANNOTATION_HUB_CACHE"))
  }
  ah_dir <- file.path(get_organism_cache_dir(orgdb_name), "ah_cache")
  if (!dir.exists(ah_dir)) {
    dir.create(ah_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ah_dir
}

# --------------------------------------------------------------------------
# Cache metadata helpers
# --------------------------------------------------------------------------

#' Build a cache metadata list for an organism.
#'
#' @param orgdb_name OrgDb package name.
#' @param sqlite_path Absolute path to the cached .sqlite file.
#' @param source Character describing the source ("annotationhub", "local_sqlite", "package").
#' @param ttl_days Numeric TTL in days.
#' @return Named list with cache metadata fields.
.build_cache_metadata <- function(orgdb_name, sqlite_path, source = "annotationhub",
                                  ttl_days = .ORGANISM_CACHE_TTL_DAYS) {
  now <- as.character(Sys.time())
  list(
    cache_status = "valid",
    source       = source,
    sqlite_path  = normalizePath(sqlite_path, mustWork = FALSE),
    created      = now,
    updated      = now,
    ttl_days     = ttl_days,
    orgdb_name   = orgdb_name
  )
}

#' Read cache metadata from disk.
#'
#' @param orgdb_name OrgDb package name.
#' @param debug_log Logging function.
#' @return Cache metadata list, or NULL if absent or unreadable.
.read_cache_metadata <- function(orgdb_name,
                                 debug_log = function(message, level = 1) {}) {
  cache_dir <- get_organism_cache_dir(orgdb_name)
  meta_file <- file.path(cache_dir, "cache_metadata.rds")
  if (!file.exists(meta_file)) return(NULL)
  tryCatch({
    meta <- readRDS(meta_file)
    if (!is.list(meta) || is.null(meta$cache_status)) return(NULL)
    meta
  }, error = function(e) {
    debug_log(paste("Failed to read cache metadata for", orgdb_name, ":", e$message), 1)
    NULL
  })
}

#' Write cache metadata to disk.
#'
#' @param orgdb_name OrgDb package name.
#' @param meta Cache metadata list (from .build_cache_metadata).
#' @param debug_log Logging function.
#' @return TRUE on success, FALSE on failure.
.write_cache_metadata <- function(orgdb_name, meta,
                                  debug_log = function(message, level = 1) {}) {
  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)
    meta_file <- file.path(cache_dir, "cache_metadata.rds")
    saveRDS(meta, meta_file)

    # Also write cache_timestamp.txt for backward compatibility with any
    # external code that reads the timestamp file directly.
    ts_file <- file.path(cache_dir, "cache_timestamp.txt")
    writeLines(meta$updated, ts_file)

    TRUE
  }, error = function(e) {
    debug_log(paste("Failed to write cache metadata for", orgdb_name, ":", e$message), 1)
    FALSE
  })
}

# --------------------------------------------------------------------------
# SQLite path resolution and OrgDb reconstruction
# --------------------------------------------------------------------------

#' Resolve the underlying .sqlite file path from a live OrgDb object.
#'
#' @param org_db A live OrgDb object (AnnotationDbi-based).
#' @param debug_log Logging function.
#' @return Character path to the .sqlite file, or NULL if not resolvable.
.resolve_orgdb_sqlite_path <- function(org_db,
                                       debug_log = function(message, level = 1) {}) {
  tryCatch({
    db_path <- AnnotationDbi::dbfile(org_db)
    if (!is.null(db_path) && nzchar(db_path) && file.exists(db_path)) {
      debug_log(paste("Resolved OrgDb sqlite path:", db_path), 2)
      return(db_path)
    }
    debug_log("dbfile() returned no valid path", 2)
    NULL
  }, error = function(e) {
    debug_log(paste("Failed to resolve OrgDb sqlite path:", e$message), 2)
    NULL
  })
}

#' Copy the OrgDb .sqlite file into the organism cache directory.
#'
#' @param orgdb_name OrgDb package name.
#' @param source_sqlite_path Path to the source .sqlite file.
#' @param debug_log Logging function.
#' @return Path to the cached copy, or NULL on failure.
.cache_sqlite_file <- function(orgdb_name, source_sqlite_path,
                               debug_log = function(message, level = 1) {}) {
  if (is.null(source_sqlite_path) || !file.exists(source_sqlite_path)) {
    debug_log("Source sqlite path is NULL or does not exist", 1)
    return(NULL)
  }

  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)
    dest_path <- file.path(cache_dir, paste0(make.names(orgdb_name), ".sqlite"))

    # Skip copy if destination already exists and is the same file
    if (file.exists(dest_path) &&
        normalizePath(source_sqlite_path, mustWork = FALSE) ==
        normalizePath(dest_path, mustWork = FALSE)) {
      debug_log(paste("SQLite file already at cache destination:", dest_path), 2)
      return(dest_path)
    }

    copied <- file.copy(source_sqlite_path, dest_path, overwrite = TRUE)
    if (copied) {
      debug_log(paste("Cached sqlite file for", orgdb_name, "->", dest_path), 1)
      return(dest_path)
    }

    debug_log(paste("file.copy failed for", orgdb_name), 1)
    NULL
  }, error = function(e) {
    debug_log(paste("Failed to cache sqlite file for", orgdb_name, ":", e$message), 1)
    NULL
  })
}

#' Reconstruct an OrgDb object from a cached .sqlite file.
#'
#' Uses AnnotationDbi::loadDb() to open the .sqlite file and return a live
#' OrgDb handle. This avoids serializing the R object and works
#' deterministically across sessions.
#'
#' @param sqlite_path Path to the .sqlite file.
#' @param orgdb_name OrgDb name (for logging only).
#' @param debug_log Logging function.
#' @return A live OrgDb object, or NULL on failure.
reconstruct_orgdb_from_sqlite <- function(sqlite_path, orgdb_name = "",
                                          debug_log = function(message, level = 1) {}) {
  if (is.null(sqlite_path) || !file.exists(sqlite_path)) {
    debug_log(paste("SQLite file not found for reconstruction:", sqlite_path), 1)
    return(NULL)
  }

  tryCatch({
    org_db <- AnnotationDbi::loadDb(sqlite_path)
    if (!is.null(org_db)) {
      debug_log(paste("Reconstructed OrgDb from sqlite:", orgdb_name, "->", sqlite_path), 1)
    }
    org_db
  }, error = function(e) {
    debug_log(paste("Failed to reconstruct OrgDb from sqlite for", orgdb_name, ":", e$message), 1)
    NULL
  })
}

# --------------------------------------------------------------------------
# Save / load / validate organism cache
# --------------------------------------------------------------------------

#' Save organism database to the SQLite-backed cache.
#'
#' Resolves the .sqlite path from the live OrgDb handle, copies it into the
#' organism cache directory, and writes structured cache metadata. Does NOT
#' serialize the OrgDb R object itself (see module header for rationale).
#'
#' @param orgdb_name organism database name
#' @param org_db live organism database object
#' @param debug_log logging function (default: no-op)
#' @return TRUE on success, FALSE on failure
save_organism_cache <- function(orgdb_name, org_db,
                                debug_log = function(message, level = 1) {}) {

  if (is.null(org_db)) {
    return(FALSE)
  }

  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)

    # Resolve the sqlite file from the live OrgDb object
    source_sqlite <- .resolve_orgdb_sqlite_path(org_db, debug_log = debug_log)

    if (!is.null(source_sqlite)) {
      # Copy sqlite into the organism cache directory
      cached_sqlite <- .cache_sqlite_file(orgdb_name, source_sqlite,
                                          debug_log = debug_log)

      if (!is.null(cached_sqlite)) {
        meta <- .build_cache_metadata(orgdb_name, cached_sqlite,
                                      source = "annotationhub")
        .write_cache_metadata(orgdb_name, meta, debug_log = debug_log)

        debug_log(paste("Saved SQLite-backed cache for", orgdb_name,
                        "- source:", cached_sqlite), 1)
        return(TRUE)
      }
    }

    # Fallback: sqlite path not resolvable (unusual). Write marker-style
    # metadata so that freshness checks still work.  On next load the
    # AnnotationHub re-download path will be used.
    debug_log(paste("Could not resolve sqlite for", orgdb_name,
                    "- writing marker-only metadata"), 1)
    meta <- .build_cache_metadata(orgdb_name, sqlite_path = "",
                                  source = "annotationhub")
    meta$cache_status <- "marker_only"
    .write_cache_metadata(orgdb_name, meta, debug_log = debug_log)

    # Also write the legacy marker file for backward compatibility
    legacy_marker <- file.path(cache_dir, "organism_db.rds")
    saveRDS(list(available = TRUE), legacy_marker)

    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Failed to save organism cache for", orgdb_name,
                    ":", e$message), 1)
    return(FALSE)
  })
}

#' Get the age of the organism cache in days.
#'
#' Returns the cache age as a numeric value (days since last update), or
#' \code{NULL} if no cache exists for the given organism. Used to decide
#' whether a stale-cache confirmation modal should be shown.
#'
#' @param orgdb_name organism database name (e.g. "org.Hs.eg.db")
#' @param debug_log logging function (default: no-op)
#' @return Numeric age in days, or NULL if no cache.
get_organism_cache_age_days <- function(orgdb_name,
                                        debug_log = function(message, level = 1) {}) {
  tryCatch({
    # --- Try new metadata-based cache first ---
    meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)
    if (!is.null(meta)) {
      updated_time <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
      if (!is.na(updated_time)) {
        return(as.numeric(difftime(Sys.time(), updated_time, units = "days")))
      }
    }

    # --- Legacy fallback ---
    cache_dir <- get_organism_cache_dir(orgdb_name)
    timestamp_file <- file.path(cache_dir, "cache_timestamp.txt")
    if (file.exists(timestamp_file)) {
      cache_time <- tryCatch(as.POSIXct(readLines(timestamp_file)[1]),
                             error = function(e) NA)
      if (!is.na(cache_time)) {
        return(as.numeric(difftime(Sys.time(), cache_time, units = "days")))
      }
    }

    NULL
  }, error = function(e) {
    debug_log(sprintf("get_organism_cache_age_days: error (%s)", e$message), 2)
    NULL
  })
}


#' Load organism database from the SQLite-backed cache.
#'
#' Attempts to reconstruct the OrgDb from the cached .sqlite file. Falls back
#' to returning NULL (triggering an AnnotationHub re-download in the caller)
#' if the cache is missing, expired, or the .sqlite file is unusable.
#'
#' Migration: if the cache directory contains an old marker-only layout
#' (organism_db.rds + cache_timestamp.txt but no cache_metadata.rds), the
#' function reads the legacy timestamp for freshness and returns NULL so that
#' the caller re-downloads from AnnotationHub. A successful re-download will
#' upgrade the cache to the new metadata format.
#'
#' @param orgdb_name organism database name
#' @param max_cache_age_days maximum cache age in days
#' @param ignore_ttl logical; when TRUE the cache is returned regardless of
#'   age (used when the user has explicitly chosen to keep using a stale cache).
#'   Defaults to FALSE.
#' @param debug_log logging function (default: no-op)
#' @return organism database object or NULL
load_organism_cache <- function(orgdb_name, max_cache_age_days = 30,
                                ignore_ttl = FALSE,
                                debug_log = function(message, level = 1) {}) {

  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)
    is_portable <- nzchar(Sys.getenv("MIRAPROT_GO_CACHE", ""))

    # --- Try new metadata-based cache first ---
    meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)

    if (!is.null(meta)) {
      updated_time <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
      age_days <- if (!is.na(updated_time)) {
        as.numeric(difftime(Sys.time(), updated_time, units = "days"))
      } else {
        Inf
      }

      # Check TTL (skip in portable mode or when caller says ignore_ttl)
      if (!ignore_ttl && !is_portable && age_days > max_cache_age_days) {
        debug_log(paste("SQLite cache expired for", orgdb_name,
                        "- age:", round(age_days, 1), "days (max:", max_cache_age_days, ")"), 1)
        return(NULL)
      }

      # Try to reconstruct from cached sqlite
      sqlite_path <- meta$sqlite_path

      # Portable relocation fallback: if the stored absolute path does not
      # exist (e.g. the prebuilt portable distribution was moved to a
      # different machine or directory), try the canonical path inside the
      # current cache directory.  This handles both prebuild and user-
      # initiated relocations transparently.
      if (!is.null(sqlite_path) && nzchar(sqlite_path) && !file.exists(sqlite_path)) {
        canonical_path <- file.path(cache_dir, paste0(make.names(orgdb_name), ".sqlite"))
        if (file.exists(canonical_path)) {
          debug_log(paste("Stored sqlite_path missing; using canonical fallback:",
                          canonical_path), 1)
          sqlite_path <- canonical_path

          # Update the metadata so subsequent loads do not repeat the fallback
          meta$sqlite_path <- normalizePath(canonical_path, mustWork = FALSE)
          .write_cache_metadata(orgdb_name, meta, debug_log = debug_log)
        }
      }

      if (!is.null(sqlite_path) && nzchar(sqlite_path) && file.exists(sqlite_path)) {
        org_db <- reconstruct_orgdb_from_sqlite(sqlite_path, orgdb_name,
                                                debug_log = debug_log)
        if (!is.null(org_db)) {
          resolver_source <- if (is_portable) "portable cache" else "SQLite cache"
          debug_log(sprintf(
            "Cache hit: loaded %s from SQLite cache (age: %s days, source: %s%s)",
            orgdb_name,
            round(age_days, 1),
            meta$source %||% "unknown",
            if (is_portable) ", portable mode" else ""
          ), 1)
          return(.tag_go_annotation_resolver_source(org_db, resolver_source))
        }
        debug_log(paste("SQLite reconstruction failed for", orgdb_name,
                        "- will re-download"), 1)
      } else if (identical(meta$cache_status, "marker_only")) {
        debug_log(paste("Marker-only metadata for", orgdb_name,
                        "(age:", round(age_days, 1), "days)",
                        "- will load from AnnotationHub cache"), 1)
      } else {
        debug_log(paste("SQLite file missing for", orgdb_name,
                        "- will re-download"), 1)
      }

      return(NULL)
    }

    # --- Legacy migration path: old marker-only layout ---
    cache_file <- file.path(cache_dir, "organism_db.rds")
    timestamp_file <- file.path(cache_dir, "cache_timestamp.txt")

    if (!file.exists(timestamp_file)) {
      debug_log(paste("No organism cache found for", orgdb_name), 2)
      return(NULL)
    }

    timestamp <- readLines(timestamp_file)[1]
    cache_time <- tryCatch(as.POSIXct(timestamp), error = function(e) NA)
    if (is.na(cache_time)) {
      debug_log(paste("Invalid timestamp in legacy cache for", orgdb_name), 1)
      return(NULL)
    }

    age_days <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))

    if (!ignore_ttl && !is_portable && age_days > max_cache_age_days) {
      debug_log(paste("Legacy cache expired for", orgdb_name,
                      "- age:", round(age_days, 1), "days"), 1)
      return(NULL)
    }

    debug_log(paste("Legacy marker-only cache detected for", orgdb_name,
                    "(age:", round(age_days, 1), "days)",
                    "- will re-download and upgrade to SQLite cache"), 1)

    # Return NULL so the caller triggers a fresh download.
    # The save path will then write the new metadata format.
    return(NULL)

  }, error = function(e) {
    debug_log(paste("Error loading organism cache for", orgdb_name,
                    ":", e$message), 1)
    return(NULL)
  })
}

#' Check if organism cache exists and is valid
#'
#' Uses the new cache metadata when available. Falls back to legacy timestamp
#' file for backward compatibility with old cache layouts.
#'
#' @param orgdb_name organism database name
#' @param max_cache_age_days maximum cache age in days
#' @param ignore_ttl logical; when TRUE the cache is considered valid regardless
#'   of age (used when the user has explicitly chosen to keep using a stale
#'   cache). Defaults to FALSE.
#' @param debug_log logging function (default: no-op)
#' @return logical indicating if valid cache exists
has_valid_organism_cache <- function(orgdb_name, max_cache_age_days = 10,
                                     ignore_ttl = FALSE,
                                     debug_log = function(message, level = 1) {}) {

  tryCatch({
    is_portable <- nzchar(Sys.getenv("MIRAPROT_GO_CACHE", ""))

    # --- New metadata-based check ---
    meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)
    if (!is.null(meta)) {
      updated_time <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
      if (is.na(updated_time)) return(FALSE)

      age_days <- as.numeric(difftime(Sys.time(), updated_time, units = "days"))

      if (!ignore_ttl && !is_portable && age_days > max_cache_age_days) {
        debug_log(paste("Cache metadata expired for", orgdb_name,
                        "- age:", round(age_days, 1), "days"), 2)
        return(FALSE)
      }

      debug_log(paste("Valid cache metadata for", orgdb_name,
                      "- age:", round(age_days, 1), "days",
                      "(status:", meta$cache_status, ")"), 2)
      return(TRUE)
    }

    # --- Legacy fallback ---
    cache_dir <- get_organism_cache_dir(orgdb_name)
    timestamp_file <- file.path(cache_dir, "cache_timestamp.txt")

    if (!file.exists(timestamp_file)) {
      return(FALSE)
    }

    timestamp <- readLines(timestamp_file)[1]
    cache_time <- tryCatch(as.POSIXct(timestamp), error = function(e) NA)
    if (is.na(cache_time)) return(FALSE)

    age_days <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))

    if (!ignore_ttl && !is_portable && age_days > max_cache_age_days) {
      debug_log(paste("Legacy cache expired for", orgdb_name,
                      "- age:", round(age_days, 1), "days"), 2)
      return(FALSE)
    }

    debug_log(paste("Valid legacy cache for", orgdb_name,
                    "- age:", round(age_days, 1), "days"), 2)
    return(TRUE)

  }, error = function(e) {
    return(FALSE)
  })
}


#' Return organism-specific static keytype defaults.
#'
#' Shared by GO and Annotation keytype resolution when a fresh keytype cache is
#' unavailable but an organism cache exists, or when a final conservative
#' fallback is needed.
#'
#' @param orgdb_name organism database name
#' @param minimal logical; when TRUE, return only the minimal common keytypes
#' @return character vector of keytypes
