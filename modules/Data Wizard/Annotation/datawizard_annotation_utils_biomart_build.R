# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_build.R
#
# Purpose:
#   BioMart cache building functions.  Orchestrates full, selective, and
#   incremental downloads of species metadata and keytypes from Ensembl
#   BioMart, storing results via the cache I/O layer.
#
# Architectural Role:
#   Cache construction layer for BioMart metadata.
#   Sourced into modEnv via datawizard_annotation.R. All functions are
#   available to observers and other utils in the same environment.
#
# Key Responsibilities:
#   - Build a complete cache of all species and their keytypes.
#   - Build a selective cache for specific source/target species only.
#   - Build an incremental cache for species not yet cached.
#   - Handle per-species retry/backoff, mirror rotation, and abort signals.
#   - Record build results in the metadata manifest.
#
# Public Functions:
#   1.  build_full_biomart_cache()       - Full cache build (all species)
#   2.  build_selective_biomart_cache()   - Selective refresh for specific species
#   3.  build_missing_biomart_cache()     - Download keytypes for uncached species
#   4.  build_mapping_tables_for_pair()   - Download mapping tables for a species pair
#   5.  build_preset_mapping_tables()     - Download mapping tables for all preset pairs
#   6.  build_missing_preset_mapping_tables() - Download tables only for uncached preset pairs
#
# Dependencies:
#   - BioMart species utils (fetch_biomart_species_with_scientific_names,
#     fetch_keytypes_with_retry, species_to_biomart_dataset)
#   - BioMart cache I/O utils (save/load functions, manifest, cache dir)
#   - interruptible_sleep() from general utils
#
# Integration Points:
#   - Called by annotation observers to populate or refresh the BioMart cache.
#   - Uses progress_callback and abort_flag for UI integration.
#
# Guidance for Future Developers:
#   - The file-based lock (.build_lock) prevents concurrent full builds.
#   - Partial failures are tolerated; failed species are recorded in manifest.
#   - The sticky mirror index reduces wasted time on unavailable mirrors.
# ==============================================================================

#' Build a complete BioMart metadata cache (all species + all keytypes).
#'
#' Fetches the full species list from Ensembl BioMart, then iterates over every
#' species to fetch and cache its keytypes using retry/backoff/mirror rotation.
#' Partial failures are tolerated: species that fail are recorded in the
#' manifest but do not prevent other species from being cached.  If the full
#' build fails entirely, the previous valid cache is preserved.
#'
#' Resilience per species mirrors \code{biomart_map_ids()}: retries with
#' jittered exponential backoff, mirror rotation on transport/server errors,
#' and non-retryable error classification for schema issues.  A sticky mirror
#' index is carried across species so that consecutive fetches reuse the last
#' successful endpoint, reducing wasted time on unavailable mirrors.
#'
#' A file-based lock (\code{.build_lock} in the cache directory) prevents
#' concurrent full-cache builds from overlapping.
#'
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(value, detail) for UI progress
#'   updates.  Ignored if NULL.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible builds.
#' @param max_retries_per_species Integer maximum fetch attempts per species
#'   (default 3).
#' @return Named list with: success (logical), species_count (integer),
#'   keytypes_cached (integer), missing_species (character vector),
#'   failed_species (character vector), duration_secs (numeric),
#'   manifest (the saved manifest list).
build_full_biomart_cache <- function(debug_log = function(m, l = 1) {},
                                       progress_callback = NULL,
                                       abort_flag = NULL,
                                       max_retries_per_species = 3L) {
  t0 <- proc.time()[["elapsed"]]
  started_at <- as.character(Sys.time())
  session_id <- paste0("full_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                       sample.int(9999, 1))
  result <- list(success = FALSE, species_count = 0L,
                 keytypes_cached = 0L, missing_species = character(0),
                 failed_species = character(0),
                 duration_secs = 0, manifest = NULL)

  report_progress <- function(value, detail) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(value, detail), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  debug_log(sprintf(
    "build_full_biomart_cache: [START] full BioMart metadata cache build (session=%s)",
    session_id), 1)

  # -- File-based lock to prevent concurrent builds --
  lock_file <- file.path(get_biomart_cache_dir(), ".build_lock")
  tryCatch({
    base_dir <- get_biomart_cache_dir()
    if (!dir.exists(base_dir)) {
      dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }, error = function(e) NULL)

  if (file.exists(lock_file)) {
    lock_age <- tryCatch({
      as.numeric(difftime(Sys.time(),
                          file.info(lock_file)$mtime, units = "mins"))
    }, error = function(e) Inf)
    if (lock_age < 30) {
      debug_log(sprintf(
        "build_full_biomart_cache: concurrent build detected (lock age %.1f min) -- skipping",
        lock_age), 1)
      result$duration_secs <- proc.time()[["elapsed"]] - t0
      return(result)
    }
    debug_log(sprintf(
      "build_full_biomart_cache: stale lock (%.1f min) -- removing and proceeding",
      lock_age), 1)
  }
  tryCatch(writeLines(as.character(Sys.time()), lock_file),
           error = function(e) NULL)
  on.exit(tryCatch(unlink(lock_file), error = function(e) NULL), add = TRUE)

  # NOTE: Existing cache is NOT deleted upfront.  Each species entry is replaced
  # atomically after successful download/validation.  If refresh is interrupted,
  # previously cached species remain usable.

  # 1. Fetch species list
  report_progress(0.05, "Fetching BioMart species list...")
  biomart_df <- tryCatch(
    fetch_biomart_species_with_scientific_names(debug_log = debug_log),
    error = function(e) {
      debug_log(sprintf("build_full_biomart_cache: species fetch failed (%s)", e$message), 1)
      NULL
    }
  )

  if (identical(attr(biomart_df, "biomart_source"), "fallback")) {
    existing_species_df <- load_biomart_species_cache(debug_log = debug_log)
    if (!is.null(existing_species_df) && is.data.frame(existing_species_df) &&
        nrow(existing_species_df) > nrow(biomart_df)) {
      debug_log("build_full_biomart_cache: live species fetch fell back to static list -- keeping previous full species cache", 1)
      biomart_df <- existing_species_df
    }
  }

  if (is.null(biomart_df) || !is.data.frame(biomart_df) || nrow(biomart_df) == 0) {
    debug_log("build_full_biomart_cache: species fetch returned empty -- aborting build, keeping previous cache", 1)
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # Save species list to disk only after a usable live result, or when no
  # richer previous cache exists. This avoids replacing a full species cache
  # with the static fallback during a transient BioMart outage.
  report_progress(0.10, sprintf("Saving %d species to cache...", nrow(biomart_df)))
  save_biomart_species_cache(biomart_df, debug_log = debug_log)
  result$species_count <- nrow(biomart_df)

  # 2. Fetch keytypes for every species with retry/backoff/mirror rotation.
  #    Each species uses atomic write (temp -> validate -> rename) so that
  #    interruption leaves the previous cache entry intact.
  all_species <- unique(biomart_df$scientific_name)
  n_species   <- length(all_species)
  cached_count <- 0L
  missing      <- character(0)
  failed       <- character(0)
  per_species_status <- list()
  # Sticky mirror: start from mirror 1, carry last successful mirror forward
  current_mirror_idx <- 1L

  debug_log(sprintf("build_full_biomart_cache: planned keytypes fetch for %d unique species (max_retries=%d)",
                    n_species, max_retries_per_species), 1)

  for (i in seq_along(all_species)) {
    sp <- all_species[i]
    elapsed <- proc.time()[["elapsed"]] - t0

    # Abort check between species
    if (is_aborted()) {
      debug_log(sprintf("build_full_biomart_cache: abort detected at species %d/%d -- stopping",
                        i, n_species), 1)
      # Mark remaining species as skipped
      for (j in i:n_species) {
        per_species_status[[all_species[j]]] <- list(
          status = "skipped", error_class = "abort",
          error_message = "build aborted by user")
      }
      break
    }

    pct <- 0.10 + 0.85 * (i / n_species)
    report_progress(pct, sprintf(
      "Keytypes for %s (%d/%d) [elapsed %.0fs, mirror %d]",
      sp, i, n_species, elapsed, current_mirror_idx))

    # Use resilient fetch with retry/backoff/mirror rotation
    fetch_res <- fetch_keytypes_with_retry(
      species_name      = sp,
      max_retries       = max_retries_per_species,
      start_mirror_idx  = current_mirror_idx,
      abort_flag        = abort_flag,
      debug_log         = debug_log
    )

    # Carry successful mirror forward for next species
    current_mirror_idx <- fetch_res$mirror_idx

    if (identical(fetch_res$status, "aborted")) {
      debug_log(sprintf("build_full_biomart_cache: abort detected during fetch for %s -- stopping", sp), 1)
      per_species_status[[sp]] <- list(
        status = "aborted", error_class = "abort",
        error_message = "aborted by user", attempts = fetch_res$attempts,
        mirror_used = fetch_res$mirror_idx)
      # Mark remaining species as skipped
      if ((i + 1L) <= n_species) {
        for (j in (i + 1L):n_species) {
          per_species_status[[all_species[j]]] <- list(
            status = "skipped", error_class = "abort",
            error_message = "build aborted by user")
        }
      }
      break
    }

    kt <- fetch_res$keytypes

    if (identical(fetch_res$status, "success") && !is.null(kt) && length(kt) > 0) {
      # Atomic save: write to staging file, validate, then rename into active cache
      write_ok <- save_biomart_keytypes_cache_atomic(sp, kt, debug_log = debug_log)
      safe_name  <- gsub(" ", "_", sp, fixed = TRUE)
      cache_file <- file.path(get_biomart_cache_dir(), "keytypes", safe_name, "keytypes.rds")
      disk_ok    <- file.exists(cache_file)

      if (isTRUE(write_ok) && disk_ok) {
        cached_count <- cached_count + 1L
        per_species_status[[sp]] <- list(
          status = "success", keytypes_count = length(kt),
          attempts = fetch_res$attempts, mirror_used = fetch_res$mirror_idx,
          updated_at = as.character(Sys.time()))
        debug_log(sprintf("build_full_biomart_cache: [OK] %s -> %d keytypes written to disk (attempt %d, mirror %d)",
                          sp, length(kt), fetch_res$attempts, fetch_res$mirror_idx), 2)
      } else {
        failed <- c(failed, sp)
        per_species_status[[sp]] <- list(
          status = "write_failed", error_class = "disk",
          error_message = "disk write or validation failed",
          attempts = fetch_res$attempts, mirror_used = fetch_res$mirror_idx)
        debug_log(sprintf("build_full_biomart_cache: [WRITE-FAIL] %s -> save returned %s, disk exists: %s",
                          sp, as.character(write_ok), as.character(disk_ok)), 1)
      }
    } else if (identical(fetch_res$status, "empty")) {
      # Connected but species has no matching OrgDb-compatible attributes
      missing <- c(missing, sp)
      per_species_status[[sp]] <- list(
        status = "missing", error_class = fetch_res$error_class,
        error_message = fetch_res$error_message,
        attempts = fetch_res$attempts, mirror_used = fetch_res$mirror_idx)
      debug_log(sprintf("build_full_biomart_cache: [MISSING] %s -> no matching keytypes (attempt %d)",
                        sp, fetch_res$attempts), 1)
    } else if (identical(fetch_res$status, "no_dataset")) {
      # Species has no BioMart dataset mapping
      missing <- c(missing, sp)
      per_species_status[[sp]] <- list(
        status = "no_dataset", error_class = "schema",
        error_message = fetch_res$error_message,
        attempts = fetch_res$attempts, mirror_used = fetch_res$mirror_idx)
      debug_log(sprintf("build_full_biomart_cache: [NO-DATASET] %s -> %s",
                        sp, fetch_res$error_message), 1)
    } else {
      # Fetch failed after all retries
      failed <- c(failed, sp)
      per_species_status[[sp]] <- list(
        status = "failed", error_class = fetch_res$error_class,
        error_message = fetch_res$error_message,
        attempts = fetch_res$attempts, mirror_used = fetch_res$mirror_idx)
      debug_log(sprintf("build_full_biomart_cache: [FAILED] %s -> %s [%s] after %d attempts",
                        sp, fetch_res$error_message %||% "unknown error",
                        fetch_res$error_class %||% "unknown",
                        fetch_res$attempts), 1)
    }

    # Rate limiter: small pause between species to avoid overwhelming BioMart
    if (i < n_species && !is_aborted()) {
      Sys.sleep(0.2)
    }

    # Log progress periodically
    if (i %% 25 == 0 || i == n_species) {
      debug_log(sprintf(
        "build_full_biomart_cache: progress %d/%d species (cached=%d, failed=%d, missing=%d, elapsed=%.0fs)",
        i, n_species, cached_count, length(failed), length(missing),
        proc.time()[["elapsed"]] - t0), 1)
    }
  }

  # 3. Post-build integrity validation: count actual keytype directories on disk
  keytypes_dir <- file.path(get_biomart_cache_dir(), "keytypes")
  on_disk_count <- 0L
  if (dir.exists(keytypes_dir)) {
    on_disk_dirs <- list.dirs(keytypes_dir, recursive = FALSE, full.names = TRUE)
    on_disk_count <- sum(vapply(on_disk_dirs, function(d) {
      file.exists(file.path(d, "keytypes.rds"))
    }, logical(1)))
  }
  debug_log(sprintf(
    "build_full_biomart_cache: on-disk verification: %d keytype directories with keytypes.rds (expected >= %d from build)",
    on_disk_count, cached_count), 1)

  # Integrity check: with atomic replacement (no upfront deletion), on_disk_count
  # should be >= cached_count because pre-existing entries from prior builds are
  # preserved.  Warn if on_disk_count is unexpectedly lower.
  if (on_disk_count < cached_count) {
    debug_log(sprintf(
      "build_full_biomart_cache: [INTEGRITY WARNING] on-disk count (%d) is lower than build count (%d) -- some atomic writes may have failed silently",
      on_disk_count, cached_count), 1)
  }

  # 4. Save manifest with per-species detail and extended versioning metadata
  now_ts <- as.character(Sys.time())
  existing_manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
  created_at <- if (!is.null(existing_manifest) && !is.null(existing_manifest$created_at)) {
    existing_manifest$created_at
  } else {
    now_ts
  }

  # Determine final status enum.
  build_status <- if (is_aborted()) {
    "aborted"
  } else if (cached_count == 0L) {
    "failed"
  } else if (cached_count <= 1L) {
    "failed_insufficient"
  } else if (length(missing) == 0 && length(failed) == 0) {
    "complete"
  } else {
    "partial"
  }

  manifest <- list(
    created_at                    = created_at,
    started_at                    = started_at,
    updated_at                    = now_ts,
    species_discovered            = as.integer(n_species),
    species_count                 = as.integer(n_species),
    keytypes_cached_count         = as.integer(cached_count),
    keytypes_cached_species_count = as.integer(cached_count),
    failed_species                = failed,
    missing_species               = missing,
    per_species_status            = per_species_status,
    on_disk_count                 = as.integer(on_disk_count),
    status                        = build_status,
    build_duration_secs           = proc.time()[["elapsed"]] - t0,
    refresh_session_id            = session_id,
    refresh_mode                  = "full_biomart_refresh",
    interrupted                   = is_aborted()
  )

  report_progress(0.97, "Saving cache manifest...")
  save_biomart_metadata_manifest(manifest, debug_log = debug_log)

  # Do not mark full build as successful if only one species keytype was cached
  result$success           <- cached_count > 1L && !is_aborted()
  result$species_count     <- as.integer(n_species)
  result$keytypes_cached   <- as.integer(cached_count)
  result$missing_species   <- missing
  result$failed_species    <- failed
  result$duration_secs     <- proc.time()[["elapsed"]] - t0
  result$manifest          <- manifest

  debug_log(sprintf(
    "build_full_biomart_cache: [END] completed in %.1fs -- %d species discovered, %d keytypes cached, %d failed, %d missing, on-disk=%d, status=%s (session=%s)",
    result$duration_secs, n_species, cached_count, length(failed), length(missing),
    on_disk_count, build_status, session_id), 1)

  if (!result$success && cached_count > 0L) {
    debug_log(sprintf(
      "build_full_biomart_cache: [WARNING] build NOT marked successful -- only %d species cached (minimum 2 required) or build was aborted",
      cached_count), 1)
  }

  result
}


#' Selective BioMart cache refresh for specific species only.
#'
#' Refreshes keytypes only for the given source and target species using
#' atomic per-species cache replacement.  All other cached species are left
#' untouched.  Species metadata is refreshed only when the species data frame
#' is not already cached on disk.
#'
#' @param source_species Character scientific name of the source species.
#' @param target_species Character scientific name of the target species.
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(value, detail) for UI progress.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible builds.
#' @param max_retries_per_species Integer maximum fetch attempts per species.
#' @return Named list with: success, species_refreshed (character vector),
#'   failed_species, duration_secs.
build_selective_biomart_cache <- function(source_species, target_species,
                                          debug_log = function(m, l = 1) {},
                                          progress_callback = NULL,
                                          abort_flag = NULL,
                                          max_retries_per_species = 3L) {
  t0 <- proc.time()[["elapsed"]]
  started_at <- as.character(Sys.time())
  session_id <- paste0("selective_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                       sample.int(9999, 1))

  result <- list(success = FALSE, species_refreshed = character(0),
                 failed_species = character(0), skipped_species = character(0),
                 duration_secs = 0)

  report_progress <- function(value, detail) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(value, detail), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  # Deduplicate if source == target
  target_list <- unique(c(source_species, target_species))
  n_species <- length(target_list)

  debug_log(sprintf(
    "build_selective_biomart_cache: [START] selective refresh for %d species: %s (session=%s)",
    n_species, paste(target_list, collapse = ", "), session_id), 1)

  # Ensure species metadata is available (refresh only if missing)
  report_progress(0.05, "Checking species metadata...")
  existing_species_df <- load_biomart_species_cache(debug_log = debug_log)
  if (is.null(existing_species_df)) {
    report_progress(0.10, "Fetching BioMart species list (needed for species resolution)...")
    biomart_df <- tryCatch(
      fetch_biomart_species_with_scientific_names(debug_log = debug_log),
      error = function(e) {
        debug_log(sprintf(
          "build_selective_biomart_cache: species metadata fetch failed (%s)",
          e$message), 1)
        NULL
      }
    )
    if (!is.null(biomart_df) && is.data.frame(biomart_df) && nrow(biomart_df) > 0) {
      save_biomart_species_cache(biomart_df, debug_log = debug_log)
    }
  }

  # Refresh keytypes for each target species with atomic writes
  cached_count  <- 0L
  failed        <- character(0)
  refreshed     <- character(0)
  current_mirror_idx <- 1L

  for (i in seq_along(target_list)) {
    sp <- target_list[i]
    elapsed <- proc.time()[["elapsed"]] - t0

    if (is_aborted()) {
      debug_log(sprintf(
        "build_selective_biomart_cache: abort detected at species %d/%d -- stopping",
        i, n_species), 1)
      break
    }

    pct <- 0.10 + 0.80 * (i / n_species)
    report_progress(pct, sprintf(
      "Refreshing keytypes for %s (%d/%d) [elapsed %.0fs]",
      sp, i, n_species, elapsed))

    fetch_res <- fetch_keytypes_with_retry(
      species_name      = sp,
      max_retries       = max_retries_per_species,
      start_mirror_idx  = current_mirror_idx,
      abort_flag        = abort_flag,
      debug_log         = debug_log
    )

    current_mirror_idx <- fetch_res$mirror_idx

    if (identical(fetch_res$status, "aborted")) {
      debug_log(sprintf(
        "build_selective_biomart_cache: abort during fetch for %s -- stopping",
        sp), 1)
      break
    }

    kt <- fetch_res$keytypes

    if (identical(fetch_res$status, "success") && !is.null(kt) && length(kt) > 0) {
      write_ok <- save_biomart_keytypes_cache_atomic(sp, kt, debug_log = debug_log)
      if (isTRUE(write_ok)) {
        cached_count <- cached_count + 1L
        refreshed <- c(refreshed, sp)
        debug_log(sprintf(
          "build_selective_biomart_cache: [OK] %s -> %d keytypes (atomic write, attempt %d)",
          sp, length(kt), fetch_res$attempts), 1)
      } else {
        failed <- c(failed, sp)
        debug_log(sprintf(
          "build_selective_biomart_cache: [WRITE-FAIL] %s -> atomic write failed",
          sp), 1)
      }
    } else {
      failed <- c(failed, sp)
      debug_log(sprintf(
        "build_selective_biomart_cache: [FAILED] %s -> status=%s, error=%s",
        sp, fetch_res$status %||% "unknown",
        fetch_res$error_message %||% "unknown"), 1)
    }
  }

  # Update manifest with selective refresh metadata
  report_progress(0.95, "Updating cache manifest...")
  existing_manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
  if (!is.null(existing_manifest)) {
    now_ts <- as.character(Sys.time())
    pss <- existing_manifest$per_species_status %||% list()
    for (sp in refreshed) {
      pss[[sp]] <- list(
        status = "success",
        keytypes_count = length(
          tryCatch(load_biomart_keytypes_cache(sp, debug_log = debug_log),
                   error = function(e) character(0))),
        updated_at = now_ts)
    }
    existing_manifest$per_species_status <- pss
    existing_manifest$updated_at <- now_ts
    existing_manifest$last_selective_refresh <- list(
      session_id   = session_id,
      species      = target_list,
      refreshed    = refreshed,
      failed       = failed,
      started_at   = started_at,
      completed_at = now_ts,
      duration_secs = proc.time()[["elapsed"]] - t0
    )
    existing_manifest$refresh_session_id <- session_id
    existing_manifest$refresh_mode <- "selective_biomart_refresh"
    existing_manifest$interrupted <- is_aborted()
    save_biomart_metadata_manifest(existing_manifest, debug_log = debug_log)
  }

  result$success           <- cached_count > 0L && !is_aborted()
  result$species_refreshed <- refreshed
  result$failed_species    <- failed
  result$duration_secs     <- proc.time()[["elapsed"]] - t0

  debug_log(sprintf(
    "build_selective_biomart_cache: [END] completed in %.1fs -- %d/%d species refreshed, %d failed (session=%s)",
    result$duration_secs, cached_count, n_species, length(failed), session_id), 1)

  result
}


#' Load missing BioMart species keytypes.
#'
#' Identifies species from the current species list that do not yet have a
#' keytypes entry on disk, then downloads only those species using atomic
#' per-species cache writes.  Already-cached species are never touched.
#'
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(value, detail) for UI progress.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible builds.
#' @param max_retries_per_species Integer maximum fetch attempts per species.
#' @return Named list with: success, species_downloaded (character vector),
#'   species_skipped (integer count of already-cached species),
#'   failed_species, duration_secs.
build_missing_biomart_cache <- function(debug_log = function(m, l = 1) {},
                                         progress_callback = NULL,
                                         abort_flag = NULL,
                                         max_retries_per_species = 3L) {
  t0 <- proc.time()[["elapsed"]]
  started_at <- as.character(Sys.time())
  session_id <- paste0("missing_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                       sample.int(9999, 1))

  result <- list(success = FALSE, species_downloaded = character(0),
                 species_skipped = 0L, failed_species = character(0),
                 duration_secs = 0)

  report_progress <- function(value, detail) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(value, detail), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  debug_log(sprintf(
    "build_missing_biomart_cache: [START] loading missing species keytypes (session=%s)",
    session_id), 1)

  # 1. Ensure we have an up-to-date species list
  report_progress(0.05, "Loading species list...")
  species_df <- load_biomart_species_cache(debug_log = debug_log)
  if (is.null(species_df)) {
    report_progress(0.08, "Fetching BioMart species list...")
    species_df <- tryCatch(
      fetch_biomart_species_with_scientific_names(debug_log = debug_log),
      error = function(e) {
        debug_log(sprintf(
          "build_missing_biomart_cache: species list fetch failed (%s)",
          e$message), 1)
        NULL
      }
    )
    if (!is.null(species_df) && is.data.frame(species_df) && nrow(species_df) > 0) {
      save_biomart_species_cache(species_df, debug_log = debug_log)
    } else {
      debug_log("build_missing_biomart_cache: no species list available -- aborting", 1)
      result$duration_secs <- proc.time()[["elapsed"]] - t0
      return(result)
    }
  }

  all_species <- unique(species_df$scientific_name)

  # 2. Identify species that are missing from the keytypes cache on disk
  missing_species <- character(0)
  cached_species  <- character(0)
  keytypes_dir <- file.path(get_biomart_cache_dir(), "keytypes")

  for (sp in all_species) {
    safe_name  <- gsub(" ", "_", sp, fixed = TRUE)
    cache_file <- file.path(keytypes_dir, safe_name, "keytypes.rds")
    if (file.exists(cache_file)) {
      cached_species <- c(cached_species, sp)
    } else {
      missing_species <- c(missing_species, sp)
    }
  }

  result$species_skipped <- length(cached_species)
  n_missing <- length(missing_species)

  debug_log(sprintf(
    "build_missing_biomart_cache: %d species total, %d already cached, %d missing",
    length(all_species), length(cached_species), n_missing), 1)

  if (n_missing == 0L) {
    debug_log("build_missing_biomart_cache: no missing species -- nothing to do", 1)
    report_progress(1.0, "All species already cached -- nothing to download.")
    result$success <- TRUE
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  report_progress(0.10, sprintf("Downloading keytypes for %d missing species...", n_missing))

  # 3. Download keytypes for missing species only (atomic writes)
  downloaded    <- character(0)
  failed        <- character(0)
  current_mirror_idx <- 1L

  for (i in seq_along(missing_species)) {
    sp <- missing_species[i]
    elapsed <- proc.time()[["elapsed"]] - t0

    if (is_aborted()) {
      debug_log(sprintf(
        "build_missing_biomart_cache: abort detected at species %d/%d -- stopping",
        i, n_missing), 1)
      break
    }

    pct <- 0.10 + 0.85 * (i / n_missing)
    report_progress(pct, sprintf(
      "Keytypes for %s (%d/%d missing) [elapsed %.0fs, mirror %d]",
      sp, i, n_missing, elapsed, current_mirror_idx))

    fetch_res <- fetch_keytypes_with_retry(
      species_name      = sp,
      max_retries       = max_retries_per_species,
      start_mirror_idx  = current_mirror_idx,
      abort_flag        = abort_flag,
      debug_log         = debug_log
    )

    current_mirror_idx <- fetch_res$mirror_idx

    if (identical(fetch_res$status, "aborted")) {
      debug_log(sprintf(
        "build_missing_biomart_cache: abort during fetch for %s -- stopping",
        sp), 1)
      break
    }

    kt <- fetch_res$keytypes

    if (identical(fetch_res$status, "success") && !is.null(kt) && length(kt) > 0) {
      write_ok <- save_biomart_keytypes_cache_atomic(sp, kt, debug_log = debug_log)
      if (isTRUE(write_ok)) {
        downloaded <- c(downloaded, sp)
        debug_log(sprintf(
          "build_missing_biomart_cache: [OK] %s -> %d keytypes (atomic write, attempt %d)",
          sp, length(kt), fetch_res$attempts), 2)
      } else {
        failed <- c(failed, sp)
        debug_log(sprintf(
          "build_missing_biomart_cache: [WRITE-FAIL] %s -> atomic write failed",
          sp), 1)
      }
    } else if (identical(fetch_res$status, "empty") ||
               identical(fetch_res$status, "no_dataset")) {
      # Species has no keytypes -- not a failure, just nothing to cache
      debug_log(sprintf(
        "build_missing_biomart_cache: [NO-DATA] %s -> %s",
        sp, fetch_res$error_message %||% "no keytypes available"), 2)
    } else {
      failed <- c(failed, sp)
      debug_log(sprintf(
        "build_missing_biomart_cache: [FAILED] %s -> status=%s, error=%s",
        sp, fetch_res$status %||% "unknown",
        fetch_res$error_message %||% "unknown"), 1)
    }

    # Rate limiter
    if (i < n_missing && !is_aborted()) {
      Sys.sleep(0.2)
    }

    if (i %% 25 == 0 || i == n_missing) {
      debug_log(sprintf(
        "build_missing_biomart_cache: progress %d/%d missing (downloaded=%d, failed=%d, elapsed=%.0fs)",
        i, n_missing, length(downloaded), length(failed),
        proc.time()[["elapsed"]] - t0), 1)
    }
  }

  # 4. Update manifest
  report_progress(0.97, "Updating cache manifest...")
  existing_manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
  if (!is.null(existing_manifest)) {
    now_ts <- as.character(Sys.time())
    pss <- existing_manifest$per_species_status %||% list()
    for (sp in downloaded) {
      pss[[sp]] <- list(
        status = "success",
        keytypes_count = length(
          tryCatch(load_biomart_keytypes_cache(sp, debug_log = debug_log),
                   error = function(e) character(0))),
        updated_at = now_ts)
    }
    existing_manifest$per_species_status <- pss
    existing_manifest$updated_at <- now_ts
    existing_manifest$last_missing_refresh <- list(
      session_id     = session_id,
      missing_count  = n_missing,
      downloaded     = downloaded,
      failed         = failed,
      started_at     = started_at,
      completed_at   = now_ts,
      duration_secs  = proc.time()[["elapsed"]] - t0
    )
    existing_manifest$refresh_session_id <- session_id
    existing_manifest$refresh_mode <- "missing_biomart_refresh"
    existing_manifest$interrupted <- is_aborted()

    # Update on_disk_count
    on_disk_count <- 0L
    if (dir.exists(keytypes_dir)) {
      on_disk_dirs <- list.dirs(keytypes_dir, recursive = FALSE, full.names = TRUE)
      on_disk_count <- sum(vapply(on_disk_dirs, function(d) {
        file.exists(file.path(d, "keytypes.rds"))
      }, logical(1)))
    }
    existing_manifest$on_disk_count <- as.integer(on_disk_count)

    save_biomart_metadata_manifest(existing_manifest, debug_log = debug_log)
  }

  result$success            <- length(downloaded) > 0L || n_missing == 0L
  result$species_downloaded <- downloaded
  result$failed_species     <- failed
  result$duration_secs      <- proc.time()[["elapsed"]] - t0

  debug_log(sprintf(
    "build_missing_biomart_cache: [END] completed in %.1fs -- %d downloaded, %d failed, %d skipped (already cached) (session=%s)",
    result$duration_secs, length(downloaded), length(failed),
    length(cached_species), session_id), 1)

  result
}


#' Download BioMart mapping tables for a single species pair.
#'
#' For cross-species pairs, downloads full mapping tables for all
#' (source_attr, homolog_attr) combinations via \code{biomaRt::getBM()} on the
#' source dataset.  For same-species pairs, downloads all (source_attr,
#' target_attr) combinations where source differs from target.
#'
#' Only tables that are not already cached on disk are downloaded.  Each
#' downloaded table is persisted via \code{save_biomart_mapping_table()} so that
#' subsequent mapping calls in \code{biomart_map_ids()} get instant cache hits.
#'
#' @param source_species Character scientific name of the source species.
#' @param target_species Character scientific name of the target species.
#' @param start_mirror_idx Integer mirror index to start from (default 1).
#' @param max_retries Integer maximum download attempts per table (default 3).
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(message, value) for progress.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible downloads.
#' @return Named list with: success (logical), tables_downloaded (integer),
#'   tables_skipped (integer), tables_failed (integer), tables_total (integer),
#'   mirror_idx (integer), duration_secs (numeric).
build_mapping_tables_for_pair <- function(source_species, target_species,
                                          start_mirror_idx = 1L,
                                          max_retries = 3L,
                                          debug_log = function(m, l = 1) {},
                                          progress_callback = NULL,
                                          abort_flag = NULL) {

  t0 <- proc.time()[["elapsed"]]

  result <- list(success = FALSE, tables_downloaded = 0L, tables_skipped = 0L,
                 tables_failed = 0L, tables_total = 0L,
                 mirror_idx = start_mirror_idx, duration_secs = 0)

  report_progress <- function(msg, value = NULL) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(msg, value), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  # -- Resolve datasets --------------------------------------------------------
  source_dataset <- species_to_biomart_dataset(source_species)
  target_dataset <- species_to_biomart_dataset(target_species)

  if (is.null(source_dataset) || is.null(target_dataset)) {
    debug_log(sprintf(
      "build_mapping_tables_for_pair: cannot resolve dataset for %s or %s",
      source_species, target_species), 1)
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # -- All BioMart-compatible source attributes --------------------------------
  all_biomart_attrs <- c(
    "external_gene_name", "ensembl_gene_id", "entrezgene_id",
    "uniprot_gn_id", "refseq_mrna", "description",
    "mgi_id", "ensembl_peptide_id", "ensembl_transcript_id"
  )

  # -- Build list of (source_attr, target_attr, cache_key) tuples --------------
  is_cross <- (source_dataset != target_dataset)
  pairs <- list()

  if (is_cross) {
    target_prefix <- sub("_gene_ensembl$", "", target_dataset)
    homolog_targets <- c("ensembl_gene_id", "external_gene_name",
                         "ensembl_peptide_id")
    for (src_attr in all_biomart_attrs) {
      for (tgt_attr in homolog_targets) {
        hom_attr <- resolve_homolog_attribute(tgt_attr, target_prefix)
        if (!is.null(hom_attr)) {
          pairs[[length(pairs) + 1L]] <- list(
            source_attr = src_attr, target_attr = hom_attr,
            cache_key = paste(source_dataset, src_attr, hom_attr, sep = ":"))
        }
      }
    }
  } else {
    for (src_attr in all_biomart_attrs) {
      for (tgt_attr in all_biomart_attrs) {
        if (src_attr != tgt_attr) {
          pairs[[length(pairs) + 1L]] <- list(
            source_attr = src_attr, target_attr = tgt_attr,
            cache_key = paste(source_dataset, src_attr, tgt_attr, sep = ":"))
        }
      }
    }
  }

  result$tables_total <- length(pairs)

  if (length(pairs) == 0L) {
    debug_log(sprintf(
      "build_mapping_tables_for_pair: no attribute pairs for %s -> %s",
      source_species, target_species), 1)
    result$success <- TRUE
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # -- Filter out already-cached pairs ----------------------------------------
  report_progress("Checking cache...", 0)
  uncached_idx <- which(vapply(pairs, function(p) {
    is.null(load_biomart_mapping_table(p$cache_key, debug_log = debug_log))
  }, logical(1)))

  result$tables_skipped <- length(pairs) - length(uncached_idx)

  if (length(uncached_idx) == 0L) {
    debug_log(sprintf(
      "build_mapping_tables_for_pair: all %d tables already cached for %s -> %s",
      length(pairs), source_species, target_species), 1)
    result$success <- TRUE
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  debug_log(sprintf(
    "build_mapping_tables_for_pair: %d/%d tables to download for %s -> %s",
    length(uncached_idx), length(pairs), source_species, target_species), 1)

  # -- Connect to source dataset -----------------------------------------------
  report_progress(sprintf("Connecting to BioMart for %s...", source_species), 0)
  conn <- connect_ensembl_with_mirrors(source_dataset, start_mirror_idx,
                                        debug_log, abort_flag = abort_flag)
  source_mart <- conn$mart
  result$mirror_idx <- conn$mirror_idx

  if (isTRUE(conn$aborted) || is.null(source_mart)) {
    debug_log(sprintf(
      "build_mapping_tables_for_pair: connection failed for %s",
      source_dataset), 1)
    result$tables_failed <- length(uncached_idx)
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    return(result)
  }

  # -- Download each uncached table --------------------------------------------
  downloaded <- 0L
  failed <- 0L
  total_uncached <- length(uncached_idx)

  for (iter in seq_along(uncached_idx)) {
    if (is_aborted()) {
      debug_log("build_mapping_tables_for_pair: abort detected", 1)
      failed <- failed + (total_uncached - iter + 1L)
      break
    }

    idx <- uncached_idx[iter]
    p   <- pairs[[idx]]
    pct <- iter / total_uncached
    report_progress(
      sprintf("Table %d / %d : %s \u2192 %s",
              iter, total_uncached, p$source_attr, p$target_attr),
      pct * 0.95)

    # Try download with retries
    table_data <- NULL
    for (attempt in seq_len(max_retries)) {
      if (is_aborted()) break
      table_data <- tryCatch({
        .with_biomart_timeout({
          biomaRt::getBM(
            attributes = c(p$source_attr, p$target_attr),
            mart       = source_mart
          )
        }, debug_log = debug_log)
      }, error = function(e) {
        debug_log(sprintf(
          "  getBM failed for %s (attempt %d/%d): %s",
          p$cache_key, attempt, max_retries, e$message), 2)
        NULL
      })

      if (!is.null(table_data) && is.data.frame(table_data)) break
      if (attempt < max_retries) interruptible_sleep(1 + attempt, abort_flag)
    }

    if (!is.null(table_data) && is.data.frame(table_data) && nrow(table_data) > 0) {
      save_biomart_mapping_table(p$cache_key, table_data, debug_log = debug_log)
      downloaded <- downloaded + 1L
      debug_log(sprintf(
        "  [OK] %s -> %d rows cached", p$cache_key, nrow(table_data)), 2)
    } else {
      failed <- failed + 1L
      debug_log(sprintf("  [FAIL] %s -> no data or download error", p$cache_key), 1)
    }

    # Rate limiter between downloads
    if (iter < total_uncached && !is_aborted()) {
      interruptible_sleep(0.3, abort_flag)
    }
  }

  result$tables_downloaded <- downloaded
  result$tables_failed     <- failed
  result$success           <- downloaded > 0L && !is_aborted()
  result$duration_secs     <- proc.time()[["elapsed"]] - t0

  debug_log(sprintf(
    "build_mapping_tables_for_pair: [END] %s -> %s in %.1fs: %d downloaded, %d skipped, %d failed",
    source_species, target_species, result$duration_secs,
    downloaded, result$tables_skipped, failed), 1)

  result
}


#' Preload BioMart mapping tables for all default preset species pairs.
#'
#' Iterates over all ordered pairs (including same-species) of
#' \code{ANNOTATION_DEFAULT_SPECIES_PRESET} and downloads the full mapping
#' tables for every BioMart-compatible key type combination.  This pre-warms
#' the disk cache so that all subsequent annotation mapping operations for
#' these species complete instantly from cache without any network requests.
#'
#' Warning: this downloads a large number of tables (thousands) and may take
#' several hours to complete depending on network speed and Ensembl server
#' load.  The function supports abort signals for early termination.
#'
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(value, detail) for UI progress.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible downloads.
#' @param max_retries Integer maximum download attempts per table (default 3).
#' @return Named list with: success (logical), pairs_processed (integer),
#'   pairs_total (integer), total_downloaded (integer), total_skipped (integer),
#'   total_failed (integer), duration_secs (numeric).
build_preset_mapping_tables <- function(debug_log = function(m, l = 1) {},
                                         progress_callback = NULL,
                                         abort_flag = NULL,
                                         max_retries = 3L) {

  t0 <- proc.time()[["elapsed"]]
  session_id <- paste0("preset_mt_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                       sample.int(9999, 1))

  result <- list(success = FALSE, pairs_processed = 0L, pairs_total = 0L,
                 total_downloaded = 0L, total_skipped = 0L, total_failed = 0L,
                 duration_secs = 0)

  report_progress <- function(value, detail) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(value, detail), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  preset_species <- ANNOTATION_DEFAULT_SPECIES_PRESET
  n_species <- length(preset_species)

  # Build all ordered pairs (including same-species for intra-species mapping)
  all_pairs <- list()
  for (src in preset_species) {
    for (tgt in preset_species) {
      all_pairs[[length(all_pairs) + 1L]] <- list(source = src, target = tgt)
    }
  }
  result$pairs_total <- length(all_pairs)

  debug_log(sprintf(
    "build_preset_mapping_tables: [START] %d species, %d pairs (session=%s)",
    n_species, length(all_pairs), session_id), 1)

  report_progress(0.01, sprintf(
    "Preloading mapping tables for %d species pairs...",
    length(all_pairs)))

  # Group by source species for connection reuse
  current_mirror_idx <- 1L
  pairs_done <- 0L

  for (pair_idx in seq_along(all_pairs)) {
    if (is_aborted()) {
      debug_log("build_preset_mapping_tables: abort detected", 1)
      break
    }

    pair <- all_pairs[[pair_idx]]

    # Absolute progress window for this pair (0.02 .. 0.97)
    pair_start <- 0.02 + 0.95 * ((pair_idx - 1L) / length(all_pairs))
    pair_end   <- 0.02 + 0.95 * (pair_idx / length(all_pairs))

    report_progress(pair_start, sprintf(
      "Pair %d / %d :  %s \u2192 %s  [elapsed %.0fs]",
      pair_idx, length(all_pairs),
      pair$source, pair$target,
      proc.time()[["elapsed"]] - t0))

    # Sub-progress callback maps pair-internal progress into the overall window
    sub_progress <- function(msg, value) {
      frac <- max(0, min(1, if (is.numeric(value)) value else 0))
      pct  <- pair_start + (pair_end - pair_start) * frac
      report_progress(pct, sprintf(
        "Pair %d / %d  (%s \u2192 %s):  %s",
        pair_idx, length(all_pairs),
        pair$source, pair$target, msg))
    }

    # Wrap each pair in tryCatch so one bad pair cannot crash the loop
    pair_result <- tryCatch(
      build_mapping_tables_for_pair(
        source_species    = pair$source,
        target_species    = pair$target,
        start_mirror_idx  = current_mirror_idx,
        max_retries       = max_retries,
        debug_log         = debug_log,
        progress_callback = sub_progress,
        abort_flag        = abort_flag
      ),
      error = function(e) {
        debug_log(sprintf(
          "build_preset_mapping_tables: ERROR on pair %d (%s -> %s): %s",
          pair_idx, pair$source, pair$target, e$message), 1)
        list(mirror_idx = current_mirror_idx,
             tables_downloaded = 0L, tables_skipped = 0L,
             tables_failed = 1L, success = FALSE)
      }
    )

    current_mirror_idx      <- pair_result$mirror_idx
    result$total_downloaded <- result$total_downloaded + pair_result$tables_downloaded
    result$total_skipped    <- result$total_skipped + pair_result$tables_skipped
    result$total_failed     <- result$total_failed + pair_result$tables_failed
    pairs_done              <- pairs_done + 1L

    # Log progress periodically
    if (pair_idx %% 10 == 0 || pair_idx == length(all_pairs)) {
      debug_log(sprintf(
        "build_preset_mapping_tables: progress %d/%d pairs (downloaded=%d, skipped=%d, failed=%d, elapsed=%.0fs)",
        pair_idx, length(all_pairs), result$total_downloaded,
        result$total_skipped, result$total_failed,
        proc.time()[["elapsed"]] - t0), 1)
    }

    # Brief pause between pairs to avoid overwhelming BioMart
    if (pair_idx < length(all_pairs) && !is_aborted()) {
      interruptible_sleep(0.5, abort_flag)
    }
  }

  result$pairs_processed <- pairs_done
  result$success         <- pairs_done > 0L && !is_aborted()
  result$duration_secs   <- proc.time()[["elapsed"]] - t0

  debug_log(sprintf(
    "build_preset_mapping_tables: [END] completed in %.1fs -- %d/%d pairs, %d tables downloaded, %d skipped, %d failed (session=%s)",
    result$duration_secs, pairs_done, length(all_pairs),
    result$total_downloaded, result$total_skipped, result$total_failed,
    session_id), 1)

  result
}


#' Preload BioMart mapping tables only for uncached default preset species pairs.
#'
#' Similar to \code{build_preset_mapping_tables()}, but skips any species pair
#' that already has at least one cached mapping table on disk.  This is useful
#' for filling in gaps after a partial or interrupted preload without
#' re-checking every individual table of already-started pairs.
#'
#' @param debug_log Logging function with signature (message, level).
#' @param progress_callback Optional function(value, detail) for UI progress.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible downloads.
#' @param max_retries Integer maximum download attempts per table (default 3).
#' @return Named list with: success (logical), pairs_processed (integer),
#'   pairs_total (integer), pairs_skipped (integer), total_downloaded (integer),
#'   total_skipped (integer), total_failed (integer), duration_secs (numeric).
build_missing_preset_mapping_tables <- function(debug_log = function(m, l = 1) {},
                                                 progress_callback = NULL,
                                                 abort_flag = NULL,
                                                 max_retries = 3L) {

  t0 <- proc.time()[["elapsed"]]
  session_id <- paste0("missing_mt_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                       sample.int(9999, 1))

  result <- list(success = FALSE, pairs_processed = 0L, pairs_total = 0L,
                 pairs_skipped = 0L, total_downloaded = 0L, total_skipped = 0L,
                 total_failed = 0L, duration_secs = 0)

  report_progress <- function(value, detail) {
    if (is.function(progress_callback)) {
      tryCatch(progress_callback(value, detail), error = function(e) NULL)
    }
  }

  is_aborted <- function() {
    !is.null(abort_flag) &&
      isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  preset_species <- ANNOTATION_DEFAULT_SPECIES_PRESET
  n_species <- length(preset_species)

  # Build all ordered pairs (including same-species for intra-species mapping)
  all_pairs <- list()
  for (src in preset_species) {
    for (tgt in preset_species) {
      all_pairs[[length(all_pairs) + 1L]] <- list(source = src, target = tgt)
    }
  }
  result$pairs_total <- length(all_pairs)

  # Determine which pairs already have at least one cached mapping table
  report_progress(0.01, "Checking which pairs have cached mapping tables...")
  uncached_pairs <- list()
  mapping_dir <- file.path(get_biomart_cache_dir(), "mapping_tables")

  for (pair in all_pairs) {
    src_ds <- species_to_biomart_dataset(pair$source)
    tgt_ds <- species_to_biomart_dataset(pair$target)
    if (is.null(src_ds) || is.null(tgt_ds)) {
      uncached_pairs[[length(uncached_pairs) + 1L]] <- pair
      next
    }
    # Check if any cached table exists for this pair by probing a representative
    # cache key.  For cross-species pairs we check the first homolog attribute;
    # for intra-species pairs we check external_gene_name -> ensembl_gene_id.
    has_any <- FALSE
    if (dir.exists(mapping_dir)) {
      is_cross <- (src_ds != tgt_ds)
      if (is_cross) {
        tgt_prefix <- sub("_gene_ensembl$", "", tgt_ds)
        sample_attr <- resolve_homolog_attribute("ensembl_gene_id", tgt_prefix)
        if (!is.null(sample_attr)) {
          sample_key <- paste(src_ds, "ensembl_gene_id", sample_attr, sep = ":")
          safe_key <- gsub(":", "__", sample_key, fixed = TRUE)
          has_any <- dir.exists(file.path(mapping_dir, safe_key))
        }
      } else {
        sample_key <- paste(src_ds, "external_gene_name", "ensembl_gene_id",
                            sep = ":")
        safe_key <- gsub(":", "__", sample_key, fixed = TRUE)
        has_any <- dir.exists(file.path(mapping_dir, safe_key))
      }
    }
    if (!has_any) {
      uncached_pairs[[length(uncached_pairs) + 1L]] <- pair
    }
  }

  result$pairs_skipped <- length(all_pairs) - length(uncached_pairs)

  debug_log(sprintf(
    "build_missing_preset_mapping_tables: [START] %d species, %d total pairs, %d uncached, %d skipped (session=%s)",
    n_species, length(all_pairs), length(uncached_pairs), result$pairs_skipped,
    session_id), 1)

  if (length(uncached_pairs) == 0L) {
    result$success <- TRUE
    result$duration_secs <- proc.time()[["elapsed"]] - t0
    debug_log(sprintf(
      "build_missing_preset_mapping_tables: all pairs already have cached tables (session=%s)",
      session_id), 1)
    return(result)
  }

  report_progress(0.02, sprintf(
    "Downloading mapping tables for %d uncached pairs (%d already cached)...",
    length(uncached_pairs), result$pairs_skipped))

  current_mirror_idx <- 1L
  pairs_done <- 0L

  for (pair_idx in seq_along(uncached_pairs)) {
    if (is_aborted()) {
      debug_log("build_missing_preset_mapping_tables: abort detected", 1)
      break
    }

    pair <- uncached_pairs[[pair_idx]]

    pair_start <- 0.02 + 0.95 * ((pair_idx - 1L) / length(uncached_pairs))
    pair_end   <- 0.02 + 0.95 * (pair_idx / length(uncached_pairs))

    report_progress(pair_start, sprintf(
      "Pair %d / %d :  %s -> %s  [elapsed %.0fs]",
      pair_idx, length(uncached_pairs),
      pair$source, pair$target,
      proc.time()[["elapsed"]] - t0))

    sub_progress <- function(msg, value) {
      frac <- max(0, min(1, if (is.numeric(value)) value else 0))
      pct  <- pair_start + (pair_end - pair_start) * frac
      report_progress(pct, sprintf(
        "Pair %d / %d  (%s -> %s):  %s",
        pair_idx, length(uncached_pairs),
        pair$source, pair$target, msg))
    }

    pair_result <- tryCatch(
      build_mapping_tables_for_pair(
        source_species    = pair$source,
        target_species    = pair$target,
        start_mirror_idx  = current_mirror_idx,
        max_retries       = max_retries,
        debug_log         = debug_log,
        progress_callback = sub_progress,
        abort_flag        = abort_flag
      ),
      error = function(e) {
        debug_log(sprintf(
          "build_missing_preset_mapping_tables: ERROR on pair %d (%s -> %s): %s",
          pair_idx, pair$source, pair$target, e$message), 1)
        list(mirror_idx = current_mirror_idx,
             tables_downloaded = 0L, tables_skipped = 0L,
             tables_failed = 1L, success = FALSE)
      }
    )

    current_mirror_idx      <- pair_result$mirror_idx
    result$total_downloaded <- result$total_downloaded + pair_result$tables_downloaded
    result$total_skipped    <- result$total_skipped + pair_result$tables_skipped
    result$total_failed     <- result$total_failed + pair_result$tables_failed
    pairs_done              <- pairs_done + 1L

    if (pair_idx %% 10 == 0 || pair_idx == length(uncached_pairs)) {
      debug_log(sprintf(
        "build_missing_preset_mapping_tables: progress %d/%d pairs (downloaded=%d, skipped=%d, failed=%d, elapsed=%.0fs)",
        pair_idx, length(uncached_pairs), result$total_downloaded,
        result$total_skipped, result$total_failed,
        proc.time()[["elapsed"]] - t0), 1)
    }

    if (pair_idx < length(uncached_pairs) && !is_aborted()) {
      interruptible_sleep(0.5, abort_flag)
    }
  }

  result$pairs_processed <- pairs_done
  result$success         <- pairs_done > 0L && !is_aborted()
  result$duration_secs   <- proc.time()[["elapsed"]] - t0

  debug_log(sprintf(
    "build_missing_preset_mapping_tables: [END] completed in %.1fs -- %d/%d uncached pairs, %d tables downloaded, %d skipped, %d failed (session=%s)",
    result$duration_secs, pairs_done, length(uncached_pairs),
    result$total_downloaded, result$total_skipped, result$total_failed,
    session_id), 1)

  result
}
