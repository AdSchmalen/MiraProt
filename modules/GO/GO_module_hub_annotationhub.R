# ==============================================================================
# GO Module Hub - AnnotationHub Acquisition
# ==============================================================================
#
# This peer file is sourced into the same environment as GO_module_hub.R.
# Function bodies are kept here to separate hub responsibilities without
# changing resolver behavior.
# ==============================================================================

# 2. AnnotationHub loading functions
# ==============================================================================

# Helper: check if running in portable mode with a persistent AnnotationHub cache.
# When TRUE, fresh-download functions reuse the persistent cache instead of
# creating throwaway temp directories, so the AnnotationHub index persists
# across sessions and downloads are faster on subsequent runs.
.has_persistent_ah_cache <- function() {
  nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", "")) &&
    nzchar(Sys.getenv("ANNOTATION_HUB_CACHE", ""))
}

# Helper: centralize AnnotationHub timeout settings.  AnnotationHub resource
# retrieval accepts curl options via `[[..., config=list(...)]` in current
# Bioconductor releases; `options(timeout=...)` is retained for base-R download
# paths and older dependencies.
.annotationhub_timeout_seconds <- function() {
  timeout <- suppressWarnings(as.numeric(Sys.getenv("MIRAPROT_ANNOTATIONHUB_TIMEOUT", "300")))
  if (is.na(timeout) || timeout < 10) timeout <- 300
  timeout
}

.annotationhub_curl_config <- function() {
  timeout <- .annotationhub_timeout_seconds()
  list(
    timeout = timeout,
    connecttimeout = timeout,
    low_speed_limit = 0,
    low_speed_time = timeout
  )
}

.with_annotationhub_timeout <- function(expr,
                                        debug_log = function(message, level = 1) {}) {
  timeout <- .annotationhub_timeout_seconds()

  old_timeout <- getOption("timeout")
  options(timeout = max(timeout, if (is.null(old_timeout)) 60 else old_timeout))
  debug_log(paste("AnnotationHub timeout set to", getOption("timeout"), "seconds"), 2)

  on.exit({
    if (is.null(old_timeout)) {
      options(timeout = NULL)
    } else {
      options(timeout = old_timeout)
    }
  }, add = TRUE)

  force(expr)
}

.annotationhub_fetch_record <- function(hub_subset, index = 1L,
                                        debug_log = function(message, level = 1) {}) {
  .with_annotationhub_timeout({
    tryCatch({
      hub_subset[[index, config = .annotationhub_curl_config()]]
    }, error = function(e) {
      if (grepl("unused argument|formal argument", conditionMessage(e), ignore.case = TRUE)) {
        debug_log("AnnotationHub record retrieval does not support curl config; retrying without config", 2)
        hub_subset[[index]]
      } else {
        stop(e)
      }
    })
  }, debug_log = debug_log)
}

#' Load Annotation Hub - RECURSION-FREE VERSION
#'
#' @param orgdb_name organism database name
#' @param debug_log logging function (default: no-op)
#' @param max_cache_age_days maximum age of cache in days (default: 10)
#' @return annotation object or NULL if failed
load_annotation_hub <- function(orgdb_name,
                                debug_log = function(message, level = 1) {},
                                max_cache_age_days = 10) {

  # Input validation
  if (missing(orgdb_name) || is.null(orgdb_name) || !is.character(orgdb_name) || length(orgdb_name) == 0) {
    debug_log("Invalid orgdb_name provided, using default", 1)
    orgdb_name <- "org.Hs.eg.db"
  }

  debug_log(paste("Loading organism database:", orgdb_name), 1)

  # DON'T use complex caching - just load fresh each time for safety
  # This eliminates all recursion risk

  tryCatch({
    # Try direct package loading first (fastest and safest)
    if (orgdb_name == "org.Hs.eg.db") {
      if (require("org.Hs.eg.db", quietly = TRUE)) {
        debug_log("Loaded org.Hs.eg.db from package", 1)
        return(org.Hs.eg.db::org.Hs.eg.db)
      }
    }

    # Fallback to AnnotationHub
    debug_log("Loading from AnnotationHub", 1)

    ah <- .with_annotationhub_timeout(suppressMessages(suppressWarnings({
      AnnotationHub(localHub = FALSE, ask = FALSE)
    })), debug_log = debug_log)

    if (is.null(ah) || !methods::is(ah, "AnnotationHub")) {
      debug_log("Failed to connect to AnnotationHub", 1)
      return(NULL)
    }

    # Query and download (use organism name for reliable AnnotationHub matching)
    q <- .query_orgdb_records(ah, orgdb_name)

    if (is.null(q) || length(q) < 1) {
      debug_log(paste("No database found for:", orgdb_name), 1)
      return(NULL)
    }

    debug_log(paste("Loading", orgdb_name, "from AnnotationHub"), 1)
    org_db <- .annotationhub_fetch_record(q, 1L, debug_log = debug_log)

    if (!is.null(org_db)) {
      debug_log(paste("Successfully loaded", orgdb_name), 1)
      return(org_db)
    }

  }, error = function(e) {
    debug_log(paste("Error loading", orgdb_name, ":", e$message), 1)
  })

  return(NULL)
}

#' Load annotation hub with fresh download - SAME METHOD AS FORCE REFRESH
#'
#' @param orgdb_name organism database name (e.g., "org.Hs.eg.db")
#' @param debug_log logging function (default: no-op)
#' @return annotation object or NULL if failed
load_annotation_hub_fresh <- function(orgdb_name,
                                      debug_log = function(message, level = 1) {}) {

  tryCatch({

    debug_log(paste("Fresh download for", orgdb_name, "using force refresh method"), 1)

    # Use a persistent per-organism AnnotationHub cache directory so that
    # the downloaded SQLite file survives across calls.  In portable mode the
    # directory is taken from ANNOTATION_HUB_CACHE; otherwise it lives inside
    # the organism cache dir and is reused on every subsequent load.
    cache_dir <- get_organism_ah_cache_dir(orgdb_name)
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (.has_persistent_ah_cache()) {
      debug_log(paste("Portable mode: using persistent AH cache:", cache_dir), 1)
    } else {
      debug_log(paste("Using persistent organism AH cache:", cache_dir), 1)
    }

    debug_log("Creating AnnotationHub with cache directory...", 1)

    # Store original environment variable
    old_cache_dir <- Sys.getenv("ANNOTATION_HUB_CACHE")

    org_db <- NULL

    tryCatch({

      Sys.setenv(ANNOTATION_HUB_CACHE = cache_dir)

      debug_log("Forcing fresh download with clean cache...", 1)

      ah <- .with_annotationhub_timeout(suppressMessages(suppressWarnings({
        AnnotationHub(localHub = FALSE, ask = FALSE, cache = cache_dir)
      })), debug_log = debug_log)

      if (is.null(ah) || !methods::is(ah, "AnnotationHub")) {
        debug_log("Failed to connect to AnnotationHub", 1)
        return(NULL)
      }

      debug_log("Connected successfully, downloading fresh data...", 1)

      q <- .query_orgdb_records(ah, orgdb_name)

      if (is.null(q) || length(q) < 1) {
        # Fallback: some AnnotationHub/Bioc version combinations can return
        # zero hits for OrgDb query terms (e.g., after switching to a git
        # AnnotationHub build on a fresh machine). In that case, prefer a
        # locally installed OrgDb package if available.
        debug_log(paste("No database found for:", orgdb_name,
                        "- trying installed OrgDb package fallback"), 1)
        org_db <- .load_installed_orgdb_fallback(orgdb_name, debug_log = debug_log)
        if (!is.null(org_db)) return(org_db)
        return(NULL)
      }

      debug_log("Downloading fresh annotation database...", 1)

      download_error <- NULL
      org_db <- tryCatch(.annotationhub_fetch_record(q, 1L, debug_log = debug_log), error = function(e) {
        download_error <<- e$message
        debug_log(paste("AnnotationHub download failed for", orgdb_name, ":", e$message), 1)
        .load_installed_orgdb_fallback(orgdb_name, debug_log = debug_log)
      })

      if (!is.null(org_db)) {
        debug_log(paste("Fresh database downloaded for", orgdb_name), 1)
      } else {
        debug_log("Database download failed", 1)
      }

    }, finally = {

      # Always restore original environment
      if (nzchar(old_cache_dir)) {
        Sys.setenv(ANNOTATION_HUB_CACHE = old_cache_dir)
      } else {
        Sys.unsetenv("ANNOTATION_HUB_CACHE")
      }

      # cache_dir is a persistent organism-specific directory — do NOT delete it.
      gc()
    })

    return(org_db)

  }, error = function(e) {
    debug_log(paste("Error in fresh download for", orgdb_name, ":", e$message), 1)
    return(NULL)
  })
}

#' Load Annotation Hub with Progress Bar - SQLite Cache Version
#'
#' Wrapper function that shows progress in Shiny UI and uses organism-specific
#' SQLite-backed cache. The load strategy is:
#'   1. Try to reconstruct OrgDb from cached .sqlite file (fastest path).
#'   2. If sqlite cache miss but metadata/marker is fresh, try localHub load
#'      from persistent AH cache (no network).
#'   3. If no valid cache, download fresh from AnnotationHub.
#'   4. On successful load, save the .sqlite file to the organism cache.
#'
#' @param organism_display organism display name (e.g., "Homo sapiens")
#' @param debug_log logging function (default: no-op)
#' @param max_cache_age_days maximum age of cache in days (default: 30)
#' @param ignore_ttl logical; when TRUE the cache is served regardless of age
#'   (used when the user has explicitly chosen to keep using a stale cache).
#'   Defaults to FALSE.
#' @return annotation object or NULL if failed
# 3. Force refresh and cache management
# ==============================================================================

#' Force Refresh AnnotationHub Cache - SQLite cache version
#'
#' Clears the existing organism cache (including SQLite artifact and metadata),
#' downloads a fresh OrgDb from AnnotationHub, and saves the .sqlite file plus
#' keytypes to the organism cache. Returns the live OrgDb object.
#'
#' @param orgdb_name organism database name
#' @param debug_log logging function (default: no-op)
#' @return list with success status and result
force_refresh_safe <- function(orgdb_name = "org.Hs.eg.db",
                               debug_log = function(message, level = 1) {}) {

  result <- list(
    success = FALSE,
    data = NULL,
    error = NULL,
    orgdb_name = orgdb_name
  )

  tryCatch({

    # Validate inputs
    if (missing(orgdb_name) || is.null(orgdb_name) || !is.character(orgdb_name) ||
        length(orgdb_name) == 0 || is.na(orgdb_name) || !nzchar(orgdb_name)) {
      orgdb_name <- "org.Hs.eg.db"
      result$orgdb_name <- orgdb_name
    }

    debug_log("=== FORCE REFRESH STARTED ===", 1)
    debug_log(paste("Target organism:", orgdb_name), 1)

    # NOTE: Old cache is NOT deleted upfront.  We download fresh first and
    # only clear the previous cache after the new data has been successfully
    # saved.  This prevents data loss when the download fails.

    # In portable mode with a persistent AH cache, reuse it
    use_persistent <- .has_persistent_ah_cache()

    if (use_persistent) {
      cache_dir <- Sys.getenv("ANNOTATION_HUB_CACHE")
      if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

      # Keep the launcher-managed AnnotationHub cache intact until a replacement
      # OrgDb has been downloaded and saved.  Deleting it upfront can turn a
      # transient AnnotationHub HTTP 403 into loss of the portable offline cache.
      clean_corrupt_annotationhub_cache(debug_log = debug_log)

      debug_log(paste("Portable mode: using persistent AH cache:", cache_dir), 1)
    } else {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      random_id <- sample(1000:9999, 1)
      cache_dir <- file.path(tempdir(), paste0("force_refresh_", timestamp, "_", random_id))

      # Clean up any existing temp directories first
      temp_base <- dirname(cache_dir)
      existing_temps <- list.files(temp_base, pattern = "force_refresh_", full.names = TRUE)
      for (temp_dir in existing_temps) {
        if (dir.exists(temp_dir)) {
          unlink(temp_dir, recursive = TRUE, force = TRUE)
        }
      }
      dir.create(cache_dir, recursive = TRUE)
    }

    debug_log("Downloading fresh OrgDb from AnnotationHub...", 1)

    # Store original environment variables
    old_cache_dir <- Sys.getenv("ANNOTATION_HUB_CACHE")

    tryCatch({

      Sys.setenv(ANNOTATION_HUB_CACHE = cache_dir)

      ah <- .with_annotationhub_timeout(suppressMessages(suppressWarnings({
        AnnotationHub(localHub = FALSE, ask = FALSE, cache = cache_dir)
      })), debug_log = debug_log)

      if (is.null(ah) || !methods::is(ah, "AnnotationHub")) {
        result$error <- "Failed to connect to AnnotationHub"
        return(result)
      }

      debug_log("Connected to AnnotationHub, downloading fresh database...", 1)

      q <- .query_orgdb_records(ah, orgdb_name)

      if (is.null(q) || length(q) < 1) {
        debug_log(paste("No AnnotationHub record for", orgdb_name,
                        "- trying installed OrgDb package fallback"), 1)
        org_db <- .load_installed_orgdb_fallback(orgdb_name, debug_log = debug_log)
        if (!is.null(org_db)) {
          removed_files <- clear_organism_cache(orgdb_name, debug_log = debug_log)
          debug_log(paste("Cleared old organism cache:", removed_files, "files removed"), 1)
          cache_success <- save_organism_cache(orgdb_name, org_db, debug_log = debug_log)
          result$success <- TRUE
          result$data <- org_db
          debug_log(paste("Loaded", orgdb_name,
                          "from installed package fallback (sqlite:",
                          if (cache_success) "saved" else "marker only", ")"), 1)
          return(result)
        }
        result$error <- paste("No database found for:", orgdb_name)
        return(result)
      }

      download_error <- NULL
      org_db <- tryCatch(.annotationhub_fetch_record(q, 1L, debug_log = debug_log), error = function(e) {
        download_error <<- e$message
        debug_log(paste("AnnotationHub download failed for", orgdb_name, ":", e$message), 1)
        .load_installed_orgdb_fallback(orgdb_name, debug_log = debug_log)
      })

      if (!is.null(org_db)) {

        # Download succeeded -- now clear the old cache before saving new data.
        # This ensures we never lose a cache when the download fails.
        removed_files <- clear_organism_cache(orgdb_name, debug_log = debug_log)
        debug_log(paste("Cleared old organism cache:", removed_files, "files removed"), 1)

        # Save the .sqlite file and write cache metadata
        cache_success <- save_organism_cache(orgdb_name, org_db,
                                             debug_log = debug_log)

        # Also extract and cache key types
        tryCatch({
          key_types <- AnnotationDbi::keytypes(org_db)
          if (!is.null(key_types) && length(key_types) > 0) {
            keytypes_success <- save_keytypes_to_cache(orgdb_name, key_types,
                                                       debug_log = debug_log)
            if (keytypes_success) {
              debug_log(paste("Cached", length(key_types),
                              "key types for", orgdb_name), 1)
            }
          }
        }, error = function(e) {
          debug_log(paste("Could not extract key types:", e$message), 1)
        })

        result$success <- TRUE
        result$data <- org_db
        debug_log("=== FORCE REFRESH COMPLETED ===", 1)
        debug_log(paste("Fresh database downloaded and cached for", orgdb_name,
                        "(sqlite:", if (cache_success) "saved" else "marker only",
                        ")"), 1)
      } else {
        result$error <- paste(
          "AnnotationHub resource download failed for", orgdb_name,
          "and no installed OrgDb fallback was available.",
          "This usually means the redirected AnnotationHub storage endpoint is unreachable from this network.",
          "The existing cache was preserved. For broad species support, retry from a network that can reach",
          "the Bioconductor AnnotationHub storage backend or use the BioMart annotation mode.",
          if (!is.null(download_error)) paste("Details:", download_error) else ""
        )
      }

    }, finally = {

      # Always restore original environment
      if (nzchar(old_cache_dir)) {
        Sys.setenv(ANNOTATION_HUB_CACHE = old_cache_dir)
      } else {
        Sys.unsetenv("ANNOTATION_HUB_CACHE")
      }

      # Only delete if it was a throwaway temp dir
      if (!use_persistent && dir.exists(cache_dir)) {
        tryCatch({
          unlink(cache_dir, recursive = TRUE, force = TRUE)
        }, error = function(e) {
          debug_log(paste("Temp cache cleanup warning:", e$message), 2)
        })
      }

      gc()
    })

  }, error = function(e) {
    result$error <- paste("Critical error:", e$message)
    debug_log("=== FORCE REFRESH FAILED ===", 1)
    debug_log(paste("Error:", e$message), 1)
  })

  return(result)
}

#' Clean up all temporary force refresh cache directories
#'
#' @param debug_log logging function (default: no-op)
cleanup_temp_caches <- function(debug_log = function(message, level = 1) {}) {

  tryCatch({
    temp_base <- tempdir()
    # Cover all GO temp directory patterns: go_analysis_*, force_refresh_*, keytypes_download_*
    existing_temps <- list.files(temp_base,
                                 pattern = "^(go_analysis_|force_refresh_|keytypes_download_)",
                                 full.names = TRUE)

    if (length(existing_temps) > 0) {
      removed_count <- 0
      for (temp_dir in existing_temps) {
        if (dir.exists(temp_dir)) {
          tryCatch({
            unlink(temp_dir, recursive = TRUE, force = TRUE)
            removed_count <- removed_count + 1
          }, error = function(e) {
            # Ignore errors
          })
        }
      }

      debug_log(paste("Cleaned up", removed_count, "temporary cache directories"), 1)
    }

  }, error = function(e) {
    # Ignore cleanup errors
  })
}

#' Clean corrupt AnnotationHub cache files
#'
#' @param debug_log logging function (default: no-op)
clean_corrupt_annotationhub_cache <- function(debug_log = function(message, level = 1) {}) {

  debug_log("Cleaning corrupt AnnotationHub cache files...", 1)

  # In portable mode, only inspect the launcher-managed AH cache.
  # In script/RStudio mode, inspect standard system directories.
  if (nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", ""))) {
    portable_ah <- Sys.getenv("ANNOTATION_HUB_CACHE", "")
    ah_cache_dirs <- if (nzchar(portable_ah)) portable_ah else character(0)
  } else {
    ah_cache_dirs <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "AnnotationHub"),
      file.path(path.expand("~"), ".cache", "R", "AnnotationHub"),
      file.path(Sys.getenv("HOME"), ".cache", "R", "AnnotationHub")
    )
  }

  total_removed <- 0

  for (ah_cache_dir in ah_cache_dirs) {
    if (dir.exists(ah_cache_dir)) {

      # Remove index file
      index_file <- file.path(ah_cache_dir, "annotationhub.index.rds")
      if (file.exists(index_file)) {
        if (file.remove(index_file)) {
          total_removed <- total_removed + 1
        }
      }

      # Remove corrupt BiocFileCache SQLite database
      sqlite_file <- file.path(ah_cache_dir, "annotationhub.sqlite3")
      if (file.exists(sqlite_file)) {
        tryCatch({
          con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
          tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
        }, error = function(e) {
          # SQLite could not open it => corrupt => remove
          if (file.remove(sqlite_file)) {
            total_removed <- total_removed + 1
            debug_log(paste("Removed corrupt annotationhub.sqlite3 from", ah_cache_dir), 1)
          }
        })
      }

      # Remove duplicate/corrupt files
      duplicate_files <- list.files(ah_cache_dir, pattern = "(86d42a8a4759_126275|86d43749115b_126275)", full.names = TRUE)
      if (length(duplicate_files) > 0) {
        removed_dupes <- sum(file.remove(duplicate_files))
        total_removed <- total_removed + removed_dupes
        debug_log(paste("Removed", removed_dupes, "corrupt cache files from", ah_cache_dir), 1)
      }
    }
  }

  debug_log(paste("Total corrupt cache files removed:", total_removed), 1)
  return(total_removed)
}

#' Clear AnnotationHub Cache - Like working version
#'
#' Clears the AnnotationHub cache to force fresh download
#' @param debug_log logging function (default: no-op)
#' @return logical success status
clear_annotationhub_cache <- function(debug_log = function(message, level = 1) {}) {

  tryCatch({

    debug_log("Clearing AnnotationHub cache...", 1)

    # In portable mode, clear only the launcher-managed ANNOTATION_HUB_CACHE.
    # In script/RStudio mode, clear the standard system AH directories.
    if (nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", ""))) {
      portable_ah <- Sys.getenv("ANNOTATION_HUB_CACHE", "")
      if (nzchar(portable_ah) && dir.exists(portable_ah)) {
        cache_files <- list.files(portable_ah, full.names = TRUE, recursive = TRUE)
        if (length(cache_files) > 0) {
          removed_count <- sum(file.remove(cache_files))
          debug_log(paste("Cleared", removed_count, "files from portable AH cache"), 1)
          return(TRUE)
        }
      }
      debug_log("No portable AH cache files found to clear", 1)
      return(TRUE)
    }

    # Script/RStudio mode: clear standard system directories
    cache_dirs <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "AnnotationHub"),
      file.path(path.expand("~"), ".cache", "R", "AnnotationHub"),
      file.path(Sys.getenv("HOME"), ".cache", "R", "AnnotationHub")
    )

    cleared_any <- FALSE

    for (cache_dir in cache_dirs) {
      if (dir.exists(cache_dir)) {
        debug_log(paste("Clearing cache directory:", cache_dir), 2)

        # Remove all files in cache directory
        cache_files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
        if (length(cache_files) > 0) {
          removed_count <- sum(file.remove(cache_files))
          debug_log(paste("Removed", removed_count, "cache files"), 2)
          cleared_any <- TRUE
        }
      }
    }

    if (cleared_any) {
      debug_log("AnnotationHub cache cleared successfully", 1)
      return(TRUE)
    } else {
      debug_log("No cache files found to clear", 1)
      return(TRUE)
    }

  }, error = function(e) {
    debug_log(paste("Cache clearing failed:", e$message), 1)
    return(FALSE)
  })
}

#' Clear organism-specific cache - SQLite-aware version
#'
#' Removes all cache artifacts for the specified organism, including:
#' - cache_metadata.rds (new metadata format)
#' - cached .sqlite file (OrgDb database copy)
#' - keytypes.rds
#' - cache_timestamp.txt (legacy format)
#' - organism_db.rds (legacy marker)
#' - ah_cache/ subdirectory
#'
#' @param orgdb_name organism database name
#' @param debug_log logging function (default: no-op)
#' @return number of cache files removed
clear_organism_cache <- function(orgdb_name = NULL,
                                 debug_log = function(message, level = 1) {}) {

  removed_count <- 0

  tryCatch({

    # Clear our custom organism cache (includes sqlite, metadata, keytypes)
    if (!is.null(orgdb_name)) {
      org_cache <- get_organism_cache_dir(orgdb_name)
      if (dir.exists(org_cache)) {
        cache_files <- list.files(org_cache, full.names = TRUE, recursive = TRUE)
        if (length(cache_files) > 0) {
          removed_count <- removed_count + sum(file.remove(cache_files))
          unlink(org_cache, recursive = TRUE)
          debug_log(paste("Cleared organism cache directory for", orgdb_name,
                          ":", org_cache), 1)
        }
      }
    }

    # In portable mode the AnnotationHub cache lives in the launcher-managed
    # ANNOTATION_HUB_CACHE directory — never touch system-wide AH directories.
    # In script/RStudio mode, clear the standard system AH cache locations.
    if (!nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", ""))) {
      ah_cache_dirs <- c(
        file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "AnnotationHub"),
        file.path(path.expand("~"), ".cache", "R", "AnnotationHub"),
        file.path(Sys.getenv("HOME"), ".cache", "R", "AnnotationHub")
      )

      for (ah_cache_dir in ah_cache_dirs) {
        if (dir.exists(ah_cache_dir)) {

          # Remove index file - forces AnnotationHub to refresh
          index_file <- file.path(ah_cache_dir, "annotationhub.index.rds")
          if (file.exists(index_file)) {
            tryCatch({
              if (file.remove(index_file)) {
                removed_count <- removed_count + 1
              }
            }, error = function(e) {
              # Ignore permission errors
            })
          }

          # Try to remove organism files (ignore if locked)
          if (!is.null(orgdb_name)) {
            cache_files <- list.files(ah_cache_dir, full.names = TRUE, recursive = FALSE)
            org_patterns <- c(orgdb_name, "126275")

            for (pattern in org_patterns) {
              org_files <- grep(pattern, cache_files, value = TRUE)
              for (org_file in org_files) {
                tryCatch({
                  if (file.remove(org_file)) {
                    removed_count <- removed_count + 1
                  }
                }, error = function(e) {
                  # Ignore permission errors - we'll bypass with temp cache
                })
              }
            }
          }
        }
      }
    }

    debug_log(paste("Cleared cache for", orgdb_name, "-", removed_count, "files"), 1)

  }, error = function(e) {
    debug_log(paste("Cache clearing error:", e$message), 1)
  })

  return(removed_count)
}

# ==============================================================================
