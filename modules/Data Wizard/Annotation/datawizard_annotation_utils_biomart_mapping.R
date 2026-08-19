# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_mapping.R
#
# Purpose:
#   BioMart connection, ID mapping, and cross-species ortholog functions.
#   Handles mirror-aware connections to Ensembl, full-table getBM downloads
#   with session-level and disk-level caching, and the two-step cross-species
#   mapping pipeline.
#
# Architectural Role:
#   Query and mapping layer for BioMart-based annotation.
#   Sourced into modEnv via datawizard_annotation.R. All functions are
#   available to observers and other utils in the same environment.
#
# Key Responsibilities:
#   - Validate species/keytype compatibility before BioMart queries.
#   - Classify BioMart errors for diagnostic and failover decisions.
#   - Connect to Ensembl with automatic mirror rotation.
#   - Download the complete source->target mapping table in a single BioMart
#     request and match requested IDs locally.
#   - Cache the downloaded table in session memory (table_cache env) for
#     instant reuse within the same session, AND on disk (via
#     save_biomart_mapping_table) for reuse across sessions.
#   - Fall back to chunked queries if the full-table download fails.
#   - Orchestrate two-step mapping (BioMart orthologs + OrgDb conversion).
#
# Public Functions:
#   1.  validate_biomart_compatibility()   - Pre-query compatibility check
#   2.  classify_biomart_error()           - Error classification for failover
#   3.  connect_ensembl_with_mirrors()     - BioMart connection with mirror fallback
#   4.  biomart_map_ids()                  - Full-table BioMart mapping with session + disk cache
#   5.  map_ids_crossspecies()             - Full cross-species pipeline (BioMart + OrgDb)
#
# Dependencies:
#   - biomaRt (useEnsembl, getLDS, getBM)
#   - BioMart species utils (species_to_biomart_dataset, etc.)
#   - General utils (interruptible_sleep, collapse_mapping_results)
#   - BioMart cache utils (load_biomart_keytypes_cache,
#     save_biomart_mapping_table, load_biomart_mapping_table)
#
# Integration Points:
#   - Called by annotation observers for cross-species ID conversion.
#   - biomart_map_ids() is the core engine used by map_ids_crossspecies().
#   - validate_biomart_compatibility() is called before any BioMart query.
#   - table_cache (biomart_table_env from reactive state) is passed in from
#     the observer to enable session-level full-table caching.
#   - Disk persistence via save/load_biomart_mapping_table() from
#     datawizard_annotation_utils_biomart_cache.R enables cross-session reuse.
#
# Caching hierarchy (checked in order):
#   1. Session cache (biomart_table_env environment) - instant, in-memory
#   2. Disk cache (mapping_tables/ subdirectory)      - fast, cross-session
#   3. Full-table BioMart download (getBM, no filter) - single network request
#   4. Chunked BioMart queries (getBM/getLDS)          - fallback on download failure
#
# Guidance for Future Developers:
#   - The full-table approach (getBM without values filter) reduces BioMart
#     network round-trips to a single request per source/target combination.
#   - Disk-persisted tables are stored as RDS with companion timestamp files
#     under mapping_tables/ inside the BioMart cache directory.
#   - Chunked fallback logic (floor 25 IDs for getLDS, 50 for getBM) is retained
#     for cases where the full-table download fails or is not applicable (getLDS).
#   - Mirror rotation is used on connection; the full-table download itself does
#     not rotate mirrors on failure (it falls back to chunked instead).
# ==============================================================================

#' Validate that species and key type selections are compatible with biomaRt.
#'
#' Performs all pre-query checks for cross-species mapping:
#' \itemize{
#'   \item Source species can be converted to a biomaRt dataset
#'   \item Target species can be converted to a biomaRt dataset
#'   \item Source key type has a valid biomaRt attribute mapping
#' }
#'
#' @param source_species Character display name of source species.
#' @param target_species Character display name of target species.
#' @param source_keytype Character OrgDb key type for source identifiers.
#' @param target_attr Character biomaRt attribute for target (from UI dropdown).
#' @param debug_log Logging function.
#' @return Named list with:
#'   \describe{
#'     \item{valid}{Logical: TRUE if all checks pass.}
#'     \item{error}{Character error message, or NULL if valid.}
#'     \item{source_attr}{Character converted biomaRt attribute for source, or NULL.}
#'     \item{source_dataset}{Character biomaRt dataset for source, or NULL.}
#'     \item{target_dataset}{Character biomaRt dataset for target, or NULL.}
#'   }
validate_biomart_compatibility <- function(source_species, target_species,
                                            source_keytype, target_attr,
                                            debug_log = function(m, l = 1) {}) {

  result <- list(valid = FALSE, error = NULL, source_attr = NULL,
                  source_dataset = NULL, target_dataset = NULL)

  # Check source species
  source_dataset <- species_to_biomart_dataset(source_species)
  if (is.null(source_dataset)) {
    result$error <- paste0("Source species '", source_species,
                            "' cannot be mapped to a BioMart dataset. ",
                            "Cross-species mapping is not available for this species.")
    debug_log(paste("BioMart validation failed:", result$error), 1)
    return(result)
  }
  result$source_dataset <- source_dataset

  # Check target species
  target_dataset <- species_to_biomart_dataset(target_species)
  if (is.null(target_dataset)) {
    result$error <- paste0("Target species '", target_species,
                            "' cannot be mapped to a BioMart dataset. ",
                            "Cross-species mapping is not available for this species.")
    debug_log(paste("BioMart validation failed:", result$error), 1)
    return(result)
  }
  result$target_dataset <- target_dataset

  # Check source key type conversion
  source_attr <- orgdb_keytype_to_biomart_attr(source_keytype)
  if (is.null(source_attr)) {
    result$error <- paste0("Source ID type '", source_keytype,
                            "' is not compatible with BioMart cross-species mapping. ",
                            "Please select one of: SYMBOL, ENSEMBL, ENTREZID, UNIPROT, ",
                            "REFSEQ, GENENAME, MGI, ENSEMBLPROT, or ENSEMBLTRANS.")
    debug_log(paste("BioMart validation failed:", result$error), 1)
    return(result)
  }
  result$source_attr <- source_attr

  result$valid <- TRUE
  debug_log(sprintf("BioMart validation passed: %s [%s] -> %s [%s]",
                     source_dataset, source_attr, target_dataset, target_attr), 2)
  result
}


#' Classify a BioMart error message for diagnostic and failover decisions.
#'
#' Returns one of: "transport" (MySQL, connection, timeout), "server"
#' (HTTP 500/502/503), "schema" (invalid attribute/filter), or "unknown".
#'
#' @param msg Character error message string.
#' @return Character error class tag.
classify_biomart_error <- function(msg) {
  msg_lower <- tolower(msg)
  if (grepl("mysql|connection reset|broken pipe|timed out|timeout|eof|refused", msg_lower)) {
    return("transport")
  }
  if (grepl("http 500|internal server|503|502|bad gateway|service unavailable", msg_lower)) {
    return("server")
  }
  if (grepl("unknown filter|invalid|attribute.*not found|no attribute", msg_lower)) {
    return("schema")
  }
  "unknown"
}

.biomart_timeout_seconds <- function() {
  timeout <- suppressWarnings(as.numeric(Sys.getenv("MIRAPROT_BIOMART_TIMEOUT", "300")))
  if (is.na(timeout) || timeout < 10) timeout <- 300
  timeout
}

.with_biomart_timeout <- function(expr,
                                  debug_log = function(m, l = 1) {}) {
  timeout <- .biomart_timeout_seconds()
  old_timeout <- getOption("timeout")
  options(timeout = max(timeout, if (is.null(old_timeout)) 60 else old_timeout))
  debug_log(sprintf("BioMart timeout set to %s seconds", getOption("timeout")), 2)

  on.exit({
    if (is.null(old_timeout)) {
      options(timeout = NULL)
    } else {
      options(timeout = old_timeout)
    }
  }, add = TRUE)

  force(expr)
}



#' Connect to Ensembl BioMart with mirror fallback.
#'
#' Tries multiple Ensembl mirrors starting from \code{start_mirror_idx}.
#' Returns a list with the mart and the mirror index that succeeded, or
#' \code{list(mart = NULL, mirror_idx = start_mirror_idx)} if all fail.
#'
#' @param dataset Character biomaRt dataset name (e.g. "hsapiens_gene_ensembl").
#'   If NULL, connects without specifying a dataset (for listDatasets etc.).
#' @param start_mirror_idx Integer mirror index to start from (1-based,
#'   wraps around). Default 1.
#' @param debug_log Logging function with signature (message, level).
#' @return Named list with \code{mart} (Mart or NULL) and
#'   \code{mirror_idx} (integer index of successful mirror).
connect_ensembl_with_mirrors <- function(dataset = NULL,
                                          start_mirror_idx = 1L,
                                          debug_log = function(m, l = 1) {},
                                          abort_flag = NULL) {
  mirrors <- c(
    "https://www.ensembl.org",
    "https://useast.ensembl.org",
    "https://asia.ensembl.org"
  )
  n_mirrors <- length(mirrors)

  for (offset in 0:(n_mirrors - 1L)) {
    # Check abort between mirror attempts
    if (!is.null(abort_flag) && isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))) {
      debug_log("BioMart connection aborted by user", 1)
      return(list(mart = NULL, mirror_idx = start_mirror_idx, aborted = TRUE))
    }

    idx <- ((start_mirror_idx - 1L + offset) %% n_mirrors) + 1L
    host <- mirrors[idx]
    mart <- tryCatch(.with_biomart_timeout({
      if (!is.null(dataset)) {
        biomaRt::useEnsembl("genes", dataset = dataset, host = host)
      } else {
        biomaRt::useEnsembl("genes", host = host)
      }
    }, debug_log = debug_log), error = function(e) NULL)
    if (!is.null(mart)) {
      ds_label <- if (!is.null(dataset)) dataset else "(no dataset)"
      debug_log(sprintf("BioMart connected via %s for %s", host, ds_label), 1)
      return(list(mart = mart, mirror_idx = idx, aborted = FALSE))
    }
    debug_log(sprintf("BioMart mirror %s failed for %s", host,
                      if (!is.null(dataset)) dataset else "(no dataset)"), 2)
  }

  debug_log(sprintf("All BioMart mirrors failed for dataset %s",
                    if (!is.null(dataset)) dataset else "(none)"), 1)
  list(mart = NULL, mirror_idx = start_mirror_idx, aborted = FALSE)
}


#' Map identifiers between species via Ensembl BioMart (full-table download with local lookup).
#'
#' Uses a two-strategy approach with a four-level caching hierarchy:
#' \enumerate{
#'   \item Session cache (in-memory environment) - instant lookup.
#'   \item Disk cache (RDS file in mapping_tables/ directory) - cross-session reuse.
#'   \item Full-table BioMart download (single \code{getBM()} call without values filter).
#'   \item Chunked BioMart queries (getBM/getLDS with retry, mirror rotation, adaptive sizing).
#' }
#'
#' \describe{
#'   \item{Strategy A (preferred)}{Downloads the complete source->homolog mapping
#'     table from the source dataset via \code{biomaRt::getBM()} in a single
#'     request (no ID filter), stores it in \code{table_cache} (session) and
#'     on disk (via \code{save_biomart_mapping_table}), and matches the
#'     requested identifiers locally.  On subsequent calls the cached table is
#'     reused (session or disk) without any BioMart round-trip.  Falls back to
#'     the original per-chunk getBM loop if the full-table download fails.
#'     Only available when the target attribute has a direct homolog equivalent.}
#'   \item{Strategy B (fallback)}{For same-species queries, applies the same
#'     full-table download + disk persistence approach using \code{getBM()}.
#'     For cross-species queries where Strategy A is not applicable, uses
#'     \code{biomaRt::getLDS()} in chunks (chunked approach is retained for
#'     getLDS because an unfiltered cross-species download may be very large).}
#' }
#'
#' @param source_ids Character vector of source identifiers.
#' @param source_dataset Character biomaRt source dataset name.
#' @param source_attr Character biomaRt attribute/filter for the source.
#' @param target_dataset Character biomaRt target dataset name.
#' @param target_attr Character biomaRt attribute for the target.
#' @param chunk_size_a Integer chunk size for the Strategy A chunked fallback (default 200).
#' @param chunk_size_b Integer chunk size for the Strategy B getLDS chunked path (default 200).
#'   The adaptive reduction logic (halving down to a minimum of 25 on consecutive
#'   failures) provides a safety net for unstable connections.
#' @param max_retries Integer maximum retries per chunk in fallback paths (default 4).
#' @param abort_flag A reactiveVal or NULL.  Checked before/during/after downloads.
#' @param progress_callback Optional function(message, value) for progress
#'   updates; value is 0-1 or NULL for detail-only updates.
#' @param table_cache An environment used as a session-level cache for full
#'   BioMart mapping tables.  When provided, the first call checks session
#'   memory, then disk cache, then downloads the full table from BioMart and
#'   stores it in both session memory and on disk.  Subsequent calls reuse the
#'   cached table for local lookup without network requests.
#'   Pass \code{NULL} to disable session caching (disk cache is still checked).
#' @param debug_log Logging function with signature (message, level).
#' @return A data.frame with columns \code{source_id} and \code{target_id},
#'   or an empty data.frame with those columns if mapping fails entirely.
biomart_map_ids <- function(source_ids, source_dataset, source_attr,
                            target_dataset, target_attr,
                            chunk_size_a = 200L, chunk_size_b = 200L,
                            # chunk_size_b is aligned with chunk_size_a: getLDS() has no
                            # hard per-chunk payload limit at 200 IDs.  The adaptive
                            # halving logic (floor 25) handles genuinely unstable sessions.
                            max_retries = 4L,
                            abort_flag = NULL,
                            progress_callback = NULL,
                            table_cache = NULL,
                            debug_log = function(m, l = 1) {}) {

  empty_result <- data.frame(source_id = character(0),
                             target_id = character(0),
                             stringsAsFactors = FALSE)

  # --- Input validation & deduplication ---
  source_ids <- unique(source_ids[!is.na(source_ids) & nzchar(source_ids)])
  if (length(source_ids) == 0) {
    debug_log("biomart_map_ids: no valid source IDs after deduplication", 1)
    return(empty_result)
  }

  debug_log(sprintf("biomart_map_ids: %d unique IDs, %s [%s] -> %s [%s]",
                    length(source_ids), source_dataset, source_attr,
                    target_dataset, target_attr), 1)

  # --- Progress helper ---
  report_progress <- function(msg, value = NULL) {
    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(msg, value), error = function(e) NULL)
    }
  }

  # --- Helper: query a single chunk with retry + jittered exponential backoff ---
  # Returns list(result = df_or_null, last_error = msg_or_null, error_class = class_or_null)
  query_chunk_with_retry <- function(query_fn, chunk, chunk_idx, n_chunks,
                                     strategy_label) {
    backoff <- 1
    last_error_msg <- NULL
    last_error_class <- NULL

    for (attempt in seq_len(max_retries)) {
      # Abort check before each attempt
      if (!is.null(abort_flag) && isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))) {
        debug_log(sprintf("  [%s] Chunk %d/%d: abort detected before attempt %d",
                          strategy_label, chunk_idx, n_chunks, attempt), 1)
        return(list(result = NULL, last_error = "aborted", error_class = "abort"))
      }

      report_progress(
        sprintf("Strategy %s: chunk %d/%d (attempt %d/%d)",
                strategy_label, chunk_idx, n_chunks, attempt, max_retries),
        (chunk_idx - 1 + (attempt - 1) / max_retries) / n_chunks
      )

      result <- tryCatch(
        .with_biomart_timeout(query_fn(chunk), debug_log = debug_log),
        error = function(e) e
      )

      # Abort check after request returns
      if (!is.null(abort_flag) && isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))) {
        if (is.data.frame(result)) {
          debug_log(sprintf("  [%s] Chunk %d/%d: abort after successful request, keeping result",
                            strategy_label, chunk_idx, n_chunks), 1)
          return(list(result = result, last_error = NULL, error_class = NULL))
        }
        return(list(result = NULL, last_error = "aborted", error_class = "abort"))
      }

      if (is.data.frame(result)) {
        debug_log(sprintf("  [%s] Chunk %d/%d: %d mappings returned",
                          strategy_label, chunk_idx, n_chunks, nrow(result)), 2)
        return(list(result = result, last_error = NULL, error_class = NULL))
      }

      last_error_msg <- conditionMessage(result)
      last_error_class <- classify_biomart_error(last_error_msg)

      if (attempt < max_retries) {
        jittered_backoff <- backoff * (1 + stats::runif(1, 0, 0.5))
        debug_log(sprintf(
          "  [%s] Chunk %d/%d attempt %d/%d failed [%s]: %s  (retry in %.1fs)",
          strategy_label, chunk_idx, n_chunks, attempt, max_retries,
          toupper(last_error_class), last_error_msg, jittered_backoff), 1)

        report_progress(
          sprintf("Strategy %s: chunk %d/%d - retry backoff %.0fs",
                  strategy_label, chunk_idx, n_chunks, jittered_backoff),
          NULL
        )

        # Interruptible backoff wait
        if (interruptible_sleep(jittered_backoff, abort_flag, interval = 0.25)) {
          debug_log(sprintf("  [%s] Abort detected during backoff wait", strategy_label), 1)
          return(list(result = NULL, last_error = "aborted", error_class = "abort"))
        }
        backoff <- backoff * 2
      } else {
        debug_log(sprintf(
          "  [%s] Chunk %d/%d FAILED after %d attempts [%s]: %s",
          strategy_label, chunk_idx, n_chunks, max_retries,
          toupper(last_error_class), last_error_msg), 1)
      }
    }
    list(result = NULL, last_error = last_error_msg, error_class = last_error_class)
  }

  # --- Helper: standardise getBM / getLDS result to source_id, target_id ---
  standardise_result <- function(df) {
    if (is.null(df) || ncol(df) < 2) return(empty_result)
    out <- data.frame(source_id = as.character(df[[1]]),
                      target_id = as.character(df[[2]]),
                      stringsAsFactors = FALSE)
    out <- out[!is.na(out$target_id) & nzchar(out$target_id), , drop = FALSE]
    out
  }

  # --- Helper: split IDs into chunks ---
  make_chunks <- function(ids, size) {
    split(ids, ceiling(seq_along(ids) / size))
  }

  # --- Helper: check abort flag ---
  is_aborted <- function() {
    !is.null(abort_flag) && isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  # =========================================================================
  # Strategy A: getBM() with homolog attributes on the source dataset
  # =========================================================================

  strategy_a_result <- NULL
  strategy_a_aborted <- FALSE

  target_prefix <- sub("_gene_ensembl$", "", target_dataset)
  homolog_attr  <- resolve_homolog_attribute(target_attr, target_prefix)

  # Cache key for the full mapping table (session-level cache).
  # Keyed by source_dataset:source_attr:homolog_attr because homolog_attr
  # already encodes the target species (e.g. mmusculus_homolog_ensembl_gene).
  cache_key_a <- if (!is.null(table_cache) && !is.null(homolog_attr)) {
    paste(source_dataset, source_attr, homolog_attr, sep = ":")
  } else {
    NULL
  }

  # Track mirror index for rotation across chunks
  current_mirror_idx <- 1L

  if (!is.null(homolog_attr) && source_dataset != target_dataset) {
    debug_log(sprintf("Strategy A: getBM on %s, attrs = [%s, %s]",
                      source_dataset, source_attr, homolog_attr), 1)

    # --- 1. Session cache hit: filter the previously downloaded full table locally ---
    if (!is.null(cache_key_a) &&
        exists(cache_key_a, envir = table_cache, inherits = FALSE)) {
      cached_full_a <- get(cache_key_a, envir = table_cache, inherits = FALSE)
      debug_log(sprintf("Strategy A: using session-cached full table (%d rows)",
                        nrow(cached_full_a)), 1)
      report_progress("Strategy A: matching from session cache...", 0.5)
      filtered <- cached_full_a[cached_full_a[[source_attr]] %in% source_ids, , drop = FALSE]
      strategy_a_result <- standardise_result(filtered)
      debug_log(sprintf("Strategy A (session cache): %d/%d IDs matched locally",
                        nrow(strategy_a_result), length(source_ids)), 1)

    # --- 2. Disk cache hit: load full table from disk, promote to session cache ---
    } else if (!is.null(cache_key_a)) {
      disk_table_a <- load_biomart_mapping_table(cache_key_a, debug_log = debug_log)
      if (!is.null(disk_table_a) && is.data.frame(disk_table_a) && nrow(disk_table_a) > 0) {
        debug_log(sprintf("Strategy A: loaded full table from disk cache (%d rows)",
                          nrow(disk_table_a)), 1)
        report_progress("Strategy A: matching from disk cache...", 0.5)

        # Promote to session cache for instant reuse within this session
        if (!is.null(table_cache)) {
          assign(cache_key_a, disk_table_a, envir = table_cache)
          debug_log("Strategy A: disk table promoted to session cache", 2)
        }

        filtered <- disk_table_a[disk_table_a[[source_attr]] %in% source_ids, , drop = FALSE]
        strategy_a_result <- standardise_result(filtered)
        debug_log(sprintf("Strategy A (disk cache): %d/%d IDs matched locally",
                          nrow(strategy_a_result), length(source_ids)), 1)
      }
    }

    # --- 3. No cache hit: download from BioMart ---
    if (is.null(strategy_a_result)) {
      report_progress("Strategy A: connecting to BioMart...", 0)

      conn <- connect_ensembl_with_mirrors(source_dataset, current_mirror_idx, debug_log,
                                            abort_flag = abort_flag)
      source_mart <- conn$mart
      current_mirror_idx <- conn$mirror_idx

      # Abort check after connection (may have been clicked during blocking HTTP)
      if (isTRUE(conn$aborted) || is_aborted()) {
        debug_log("Strategy A: aborted during or after connection", 1)
        attr(empty_result, "aborted") <- TRUE
        return(empty_result)
      }

      if (!is.null(source_mart)) {
        # --- Full-table download: fetch all source->homolog mappings in one request ---
        # The downloaded table is cached both in the session environment AND on
        # disk so that repeated "Map IDs" clicks (same session) and new sessions
        # both reuse the local copy without any further BioMart requests.
        # If the full-table download fails the code falls back to the chunked
        # approach.
        report_progress("Strategy A: downloading full mapping table...", 0.1)
        debug_log(sprintf(
          "Strategy A: downloading full table %s [%s -> %s] (no ID filter)",
          source_dataset, source_attr, homolog_attr), 1)

        full_table_a <- tryCatch({
          .with_biomart_timeout(biomaRt::getBM(
            attributes = c(source_attr, homolog_attr),
            mart       = source_mart
          ), debug_log = debug_log)
        }, error = function(e) {
          debug_log(sprintf(
            "Strategy A: full table download failed (%s) - falling back to chunked queries",
            e$message), 1)
          NULL
        })

        # Abort check after the potentially long blocking download
        if (is_aborted()) {
          debug_log("Strategy A: abort detected after full table download", 1)
          attr(empty_result, "aborted") <- TRUE
          return(empty_result)
        }

        if (!is.null(full_table_a) && is.data.frame(full_table_a) && nrow(full_table_a) > 0) {
          debug_log(sprintf("Strategy A: downloaded full table (%d rows)",
                            nrow(full_table_a)), 1)

          # Store in session cache for subsequent mapping runs within this session
          if (!is.null(table_cache) && !is.null(cache_key_a)) {
            assign(cache_key_a, full_table_a, envir = table_cache)
            debug_log("Strategy A: full table stored in session cache", 2)
          }

          # Persist to disk for cross-session reuse
          if (!is.null(cache_key_a)) {
            save_biomart_mapping_table(cache_key_a, full_table_a, debug_log = debug_log)
          }

          # Filter locally - no further BioMart requests needed
          filtered <- full_table_a[full_table_a[[source_attr]] %in% source_ids, , drop = FALSE]
          strategy_a_result <- standardise_result(filtered)
          debug_log(sprintf("Strategy A (full table): %d/%d IDs matched locally",
                            nrow(strategy_a_result), length(source_ids)), 1)
        } else {
          # Full-table download returned NULL or an empty frame; fall back to
          # the original chunked approach so that partial network issues do not
          # silently produce zero results.
          debug_log("Strategy A: full table unavailable, falling back to chunked queries", 1)

          current_chunk_size_a <- chunk_size_a
          chunks    <- make_chunks(source_ids, current_chunk_size_a)
          n_chunks  <- length(chunks)
          debug_log(sprintf("Strategy A (chunked fallback): %d chunks of up to %d IDs",
                            n_chunks, current_chunk_size_a), 1)

          chunk_results <- list()
          n_success <- 0L
          n_fail    <- 0L
          total_rows <- 0L
          consecutive_chunk_failures <- 0L

          i <- 1L
          while (i <= length(chunks)) {
            # Abort check
            if (is_aborted()) {
              debug_log(sprintf("Mapping aborted by user after %d chunks (user request)", i - 1), 1)
              strategy_a_aborted <- TRUE
              break
            }

            query_fn <- function(ids) {
              biomaRt::getBM(
                attributes = c(source_attr, homolog_attr),
                filters    = source_attr,
                values     = ids,
                mart       = source_mart
              )
            }

            retry_result <- query_chunk_with_retry(query_fn, chunks[[i]], i, length(chunks), "A")

            # Handle abort during retry
            if (identical(retry_result$error_class, "abort")) {
              strategy_a_aborted <- TRUE
              break
            }

            if (!is.null(retry_result$result)) {
              std_res <- standardise_result(retry_result$result)
              chunk_results[[length(chunk_results) + 1L]] <- std_res
              n_success <- n_success + 1L
              total_rows <- total_rows + nrow(std_res)
              consecutive_chunk_failures <- 0L
              i <- i + 1L
            } else {
              n_fail <- n_fail + 1L
              consecutive_chunk_failures <- consecutive_chunk_failures + 1L

              # Mirror rotation on transport/server errors
              if (retry_result$error_class %in% c("transport", "server")) {
                new_mirror <- (current_mirror_idx %% 3L) + 1L
                debug_log(sprintf("Strategy A: rotating to mirror %d after %s error",
                                  new_mirror, toupper(retry_result$error_class)), 1)
                conn <- connect_ensembl_with_mirrors(source_dataset, new_mirror, debug_log,
                                                      abort_flag = abort_flag)
                if (isTRUE(conn$aborted)) {
                  strategy_a_aborted <- TRUE
                  break
                }
                if (!is.null(conn$mart)) {
                  source_mart <- conn$mart
                  current_mirror_idx <- conn$mirror_idx
                }
              }

              # Adaptive chunk size reduction after 2 consecutive chunk failures
              if (consecutive_chunk_failures >= 2L && current_chunk_size_a > 50L) {
                current_chunk_size_a <- max(50L, current_chunk_size_a %/% 2L)
                # Re-chunk remaining unprocessed IDs
                remaining_ids <- unlist(chunks[i:length(chunks)], use.names = FALSE)
                chunks <- c(chunks[seq_len(i - 1L)], make_chunks(remaining_ids, current_chunk_size_a))
                debug_log(sprintf("Strategy A: adaptive chunk reduction to %d IDs, %d chunks remaining",
                                  current_chunk_size_a, length(chunks) - i + 1L), 1)
                consecutive_chunk_failures <- 0L
              }

              i <- i + 1L
            }
          }

          combined <- do.call(rbind, chunk_results)
          if (!is.null(combined) && nrow(combined) > 0) {
            strategy_a_result <- combined
            debug_log(sprintf("Strategy A (chunked): %d/%d chunks succeeded, %d total mappings",
                              n_success, n_success + n_fail, nrow(combined)), 1)
            if (strategy_a_aborted) {
              attr(strategy_a_result, "aborted") <- TRUE
            }
          } else {
            debug_log("Strategy A (chunked): all chunks returned empty results", 1)
          }
        }
      } else {
        debug_log("Strategy A: connection failed on all mirrors", 1)
      }
    }
  } else {
    if (source_dataset == target_dataset) {
      debug_log("Same-species query: skipping Strategy A, using Strategy B (getBM)", 1)
    } else {
      debug_log(sprintf("Strategy A not available for target_attr '%s': no homolog equivalent",
                        target_attr), 1)
    }
  }

  # Return Strategy A results if we got any
  if (!is.null(strategy_a_result) && nrow(strategy_a_result) > 0) {
    debug_log(sprintf("Cross-species backend: BioMart Strategy A"), 1)
    debug_log(sprintf("biomart_map_ids: returning %d mappings from Strategy A",
                      nrow(strategy_a_result)), 1)
    report_progress("Strategy A: complete", 1)
    return(strategy_a_result)
  }

  # If user aborted during Strategy A, return partial results
  if (strategy_a_aborted) {
    attr(empty_result, "aborted") <- TRUE
    return(empty_result)
  }

  # =========================================================================
  # Strategy B: getLDS() / getBM() in small chunks (fallback)
  # =========================================================================

  # Abort check before Strategy B (user may have clicked during Strategy A processing)
  if (is_aborted()) {
    debug_log("Mapping aborted before Strategy B", 1)
    attr(empty_result, "aborted") <- TRUE
    return(empty_result)
  }

  debug_log("Strategy B (fallback): chunked queries", 1)
  report_progress("Strategy B: connecting to BioMart...", 0)

  is_cross_species <- (source_dataset != target_dataset)
  source_mart <- NULL
  target_mart <- NULL
  source_mirror_idx <- current_mirror_idx
  target_mirror_idx <- current_mirror_idx

  if (is_cross_species) {
    conn_src <- connect_ensembl_with_mirrors(source_dataset, source_mirror_idx, debug_log,
                                              abort_flag = abort_flag)
    source_mart <- conn_src$mart
    source_mirror_idx <- conn_src$mirror_idx

    if (isTRUE(conn_src$aborted) || is_aborted()) {
      debug_log("Strategy B: aborted during source connection", 1)
      attr(empty_result, "aborted") <- TRUE
      return(empty_result)
    }

    conn_tgt <- connect_ensembl_with_mirrors(target_dataset, target_mirror_idx, debug_log,
                                              abort_flag = abort_flag)
    target_mart <- conn_tgt$mart
    target_mirror_idx <- conn_tgt$mirror_idx

    if (isTRUE(conn_tgt$aborted) || is_aborted()) {
      debug_log("Strategy B: aborted during target connection", 1)
      attr(empty_result, "aborted") <- TRUE
      return(empty_result)
    }

    if (is.null(source_mart) || is.null(target_mart)) {
      debug_log("Strategy B: could not connect to one or both marts", 1)
      failed_ds <- if (is.null(source_mart)) source_dataset else target_dataset
      attr(empty_result, "biomart_error") <- paste0(
        "Could not connect to BioMart dataset '", failed_ds,
        "'. The dataset may not exist on Ensembl or the server may be unavailable. ",
        "Try a different species or try again later."
      )
      return(empty_result)
    }
  } else {
    conn_src <- connect_ensembl_with_mirrors(source_dataset, source_mirror_idx, debug_log,
                                              abort_flag = abort_flag)
    source_mart <- conn_src$mart
    source_mirror_idx <- conn_src$mirror_idx

    if (isTRUE(conn_src$aborted) || is_aborted()) {
      debug_log("Strategy B: aborted during connection", 1)
      attr(empty_result, "aborted") <- TRUE
      return(empty_result)
    }
    if (is.null(source_mart)) return(empty_result)
  }

  # --- Strategy B full-table approach for same-species getBM queries ---
  # For same-species lookups (getBM without homolog attributes) download the
  # complete source->target attribute table in one request, cache it in both
  # session memory and on disk, and match the requested IDs locally.
  # The cross-species getLDS path is left unchanged (chunked) because
  # getLDS without a values filter may return a very large payload.
  if (!is_cross_species && !is.null(source_mart)) {
    cache_key_b <- if (!is.null(table_cache)) {
      paste(source_dataset, source_attr, target_attr, sep = ":")
    } else {
      NULL
    }

    # 1. Session cache hit: filter locally without any BioMart call
    if (!is.null(cache_key_b) &&
        exists(cache_key_b, envir = table_cache, inherits = FALSE)) {
      cached_full_b <- get(cache_key_b, envir = table_cache, inherits = FALSE)
      debug_log(sprintf("Strategy B: using session-cached full table (%d rows)",
                        nrow(cached_full_b)), 1)
      report_progress("Strategy B: matching from session cache...", 0.5)
      filtered <- cached_full_b[cached_full_b[[source_attr]] %in% source_ids, , drop = FALSE]
      result_b <- standardise_result(filtered)
      debug_log(sprintf("Strategy B (session cache): %d/%d IDs matched locally",
                        nrow(result_b), length(source_ids)), 1)
      report_progress("Strategy B: complete (from session cache)", 1)
      return(result_b)
    }

    # 2. Disk cache hit: load full table, promote to session cache
    if (!is.null(cache_key_b)) {
      disk_table_b <- load_biomart_mapping_table(cache_key_b, debug_log = debug_log)
      if (!is.null(disk_table_b) && is.data.frame(disk_table_b) && nrow(disk_table_b) > 0) {
        debug_log(sprintf("Strategy B: loaded full table from disk cache (%d rows)",
                          nrow(disk_table_b)), 1)
        report_progress("Strategy B: matching from disk cache...", 0.5)

        # Promote to session cache
        if (!is.null(table_cache)) {
          assign(cache_key_b, disk_table_b, envir = table_cache)
          debug_log("Strategy B: disk table promoted to session cache", 2)
        }

        filtered <- disk_table_b[disk_table_b[[source_attr]] %in% source_ids, , drop = FALSE]
        result_b <- standardise_result(filtered)
        debug_log(sprintf("Strategy B (disk cache): %d/%d IDs matched locally",
                          nrow(result_b), length(source_ids)), 1)
        report_progress("Strategy B: complete (from disk cache)", 1)
        return(result_b)
      }
    }

    # 3. Download the full same-species mapping table in one request
    report_progress("Strategy B: downloading full mapping table...", 0.1)
    debug_log(sprintf(
      "Strategy B: downloading full table %s [%s -> %s] (no ID filter)",
      source_dataset, source_attr, target_attr), 1)

    full_table_b <- tryCatch({
      .with_biomart_timeout(biomaRt::getBM(
        attributes = c(source_attr, target_attr),
        mart       = source_mart
      ), debug_log = debug_log)
    }, error = function(e) {
      debug_log(sprintf(
        "Strategy B: full table download failed (%s) - falling back to chunked queries",
        e$message), 1)
      NULL
    })

    # Abort check after the potentially long blocking download
    if (is_aborted()) {
      debug_log("Strategy B: abort detected after full table download", 1)
      attr(empty_result, "aborted") <- TRUE
      return(empty_result)
    }

    if (!is.null(full_table_b) && is.data.frame(full_table_b) && nrow(full_table_b) > 0) {
      debug_log(sprintf("Strategy B: downloaded full table (%d rows)",
                        nrow(full_table_b)), 1)

      # Store in session cache for subsequent mapping runs within this session
      if (!is.null(table_cache) && !is.null(cache_key_b)) {
        assign(cache_key_b, full_table_b, envir = table_cache)
        debug_log("Strategy B: full table stored in session cache", 2)
      }

      # Persist to disk for cross-session reuse
      if (!is.null(cache_key_b)) {
        save_biomart_mapping_table(cache_key_b, full_table_b, debug_log = debug_log)
      }

      # Filter locally
      filtered <- full_table_b[full_table_b[[source_attr]] %in% source_ids, , drop = FALSE]
      result_b <- standardise_result(filtered)
      debug_log(sprintf("Strategy B (full table): %d/%d IDs matched locally",
                        nrow(result_b), length(source_ids)), 1)
      report_progress("Strategy B: complete", 1)
      return(result_b)
    }

    # Full-table download returned NULL or empty; fall through to chunked loop
    debug_log("Strategy B: full table unavailable, falling back to chunked queries", 1)
  }

  current_chunk_size_b <- chunk_size_b
  chunks   <- make_chunks(source_ids, current_chunk_size_b)
  n_chunks <- length(chunks)
  debug_log(sprintf("Strategy B: %d chunks of up to %d IDs, cross_species=%s",
                    n_chunks, current_chunk_size_b, is_cross_species), 1)

  chunk_results <- list()
  n_success <- 0L
  n_fail    <- 0L
  total_rows <- 0L
  strategy_b_aborted <- FALSE
  consecutive_chunk_failures <- 0L

  i <- 1L
  while (i <= length(chunks)) {
    # Abort check
    if (is_aborted()) {
      debug_log(sprintf("Mapping aborted by user after %d chunks (user request)", i - 1), 1)
      strategy_b_aborted <- TRUE
      break
    }

    if (is_cross_species) {
      query_fn <- function(ids) {
        biomaRt::getLDS(
          attributes  = source_attr,
          filters     = source_attr,
          values      = ids,
          mart        = source_mart,
          attributesL = target_attr,
          martL       = target_mart
        )
      }
    } else {
      query_fn <- function(ids) {
        biomaRt::getBM(
          attributes = c(source_attr, target_attr),
          filters    = source_attr,
          values     = ids,
          mart       = source_mart
        )
      }
    }

    retry_result <- query_chunk_with_retry(query_fn, chunks[[i]], i, length(chunks), "B")

    # Handle abort during retry
    if (identical(retry_result$error_class, "abort")) {
      strategy_b_aborted <- TRUE
      break
    }

    if (!is.null(retry_result$result)) {
      std_res <- standardise_result(retry_result$result)
      chunk_results[[length(chunk_results) + 1L]] <- std_res
      n_success <- n_success + 1L
      total_rows <- total_rows + nrow(std_res)
      consecutive_chunk_failures <- 0L
      i <- i + 1L
    } else {
      n_fail <- n_fail + 1L
      consecutive_chunk_failures <- consecutive_chunk_failures + 1L

      # Mirror rotation on transport/server errors
      if (retry_result$error_class %in% c("transport", "server")) {
        new_src_mirror <- (source_mirror_idx %% 3L) + 1L
        debug_log(sprintf("Strategy B: rotating mirrors after %s error",
                          toupper(retry_result$error_class)), 1)

        conn_src <- connect_ensembl_with_mirrors(source_dataset, new_src_mirror, debug_log,
                                                  abort_flag = abort_flag)
        if (isTRUE(conn_src$aborted)) {
          strategy_b_aborted <- TRUE
          break
        }
        if (!is.null(conn_src$mart)) {
          source_mart <- conn_src$mart
          source_mirror_idx <- conn_src$mirror_idx
        }
        if (is_cross_species) {
          new_tgt_mirror <- (target_mirror_idx %% 3L) + 1L
          conn_tgt <- connect_ensembl_with_mirrors(target_dataset, new_tgt_mirror, debug_log,
                                                    abort_flag = abort_flag)
          if (isTRUE(conn_tgt$aborted)) {
            strategy_b_aborted <- TRUE
            break
          }
          if (!is.null(conn_tgt$mart)) {
            target_mart <- conn_tgt$mart
            target_mirror_idx <- conn_tgt$mirror_idx
          }
        }
      }

      # Adaptive chunk size reduction after 2 consecutive chunk failures
      if (consecutive_chunk_failures >= 2L && current_chunk_size_b > 25L) {
        current_chunk_size_b <- max(25L, current_chunk_size_b %/% 2L)
        remaining_ids <- unlist(chunks[i:length(chunks)], use.names = FALSE)
        chunks <- c(chunks[seq_len(i - 1L)], make_chunks(remaining_ids, current_chunk_size_b))
        debug_log(sprintf("Strategy B: adaptive chunk reduction to %d IDs, %d chunks remaining",
                          current_chunk_size_b, length(chunks) - i + 1L), 1)
        consecutive_chunk_failures <- 0L
      }

      i <- i + 1L
    }
  }

  combined <- do.call(rbind, chunk_results)
  if (is.null(combined)) combined <- empty_result

  debug_log(sprintf("Strategy B: %d/%d chunks succeeded, %d total mappings",
                    n_success, n_success + n_fail, nrow(combined)), 1)
  if (!strategy_b_aborted) {
    debug_log(sprintf("Cross-species backend: BioMart Strategy B"), 1)
  }
  debug_log(sprintf("biomart_map_ids: returning %d mappings", nrow(combined)), 1)
  report_progress("Strategy B: complete", 1)

  if (strategy_b_aborted) {
    attr(combined, "aborted") <- TRUE
  }

  combined
}



#' Perform cross-species ortholog mapping via BioMart with optional
#' intra-species conversion to the final target key type.
#'
#' Prefers direct BioMart output when the target key type has a known
#' BioMart attribute equivalent AND a Strategy A route (homolog equivalent
#' on the source dataset).  For cross-species queries where the target
#' attribute has no homolog equivalent, skips direct BioMart mapping
#' (which would rely on the fragile getLDS endpoint) and uses the two-step
#' approach instead: ensembl_gene_id intermediate via Strategy A + OrgDb
#' conversion to the final target key type.
#'
#' @param ids Character vector of source identifiers.
#' @param source_species Character source species display name.
#' @param target_species Character target species display name.
#' @param source_attr Character biomaRt attribute/filter for source.
#' @param target_keytype Character OrgDb key type for the desired output
#'   (e.g. "SYMBOL", "ENSEMBL", "UNIPROT").
#' @param collapse_strategy Character "first" or "semicolon".
#' @param abort_flag A reactiveVal or NULL.  Checked before and during mapping.
#' @param progress_callback Optional function(message, value) for progress
#'   updates passed through to \code{biomart_map_ids()}.
#' @param table_cache An environment used as a session-level cache for full
#'   BioMart mapping tables.  When provided, \code{biomart_map_ids()} stores
#'   the downloaded full table on first use and reuses it on subsequent calls
#'   within the same session, eliminating repeated BioMart round-trips.
#'   Pass \code{NULL} to disable caching (default).
#' @param debug_log Logging function.
#' @return Named character vector: names = input IDs, values = mapped IDs or NA.
#'   Attributes:
#'   \describe{
#'     \item{step1_mapped}{Integer count of IDs with orthologs found in step 1.}
#'     \item{step2_input}{Integer count of unique intermediate IDs entering step 2.}
#'     \item{step2_mapped}{Integer count of IDs successfully converted in step 2.}
#'     \item{two_step}{Logical TRUE if step 2 was performed.}
#'     \item{direct_biomart}{Logical TRUE if BioMart provided the final target
#'       type directly (no OrgDb step 2).}
#'   }
map_ids_crossspecies <- function(ids, source_species, target_species,
                                  source_attr, target_keytype,
                                  collapse_strategy = "first",
                                  abort_flag = NULL,
                                  progress_callback = NULL,
                                  table_cache = NULL,
                                  ignore_ttl = FALSE,
                                  debug_log = function(m, l = 1) {}) {
  if (length(ids) == 0) {
    debug_log("map_ids_crossspecies: no IDs provided", 1)
    return(character(0))
  }

  debug_log(sprintf("Cross-species mapping: %d IDs, %s (%s) -> %s (%s), strategy: %s",
                    length(ids), source_species, source_attr,
                    target_species, target_keytype, collapse_strategy), 1)

  # --- Progress helper ---
  report_progress <- function(msg, value = NULL) {
    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(msg, value), error = function(e) NULL)
    }
  }

  # --- Helper: collapse raw_result data.frame to named vector ---
  collapse_raw <- function(raw_result) {
    grouped   <- split(raw_result$target_id, raw_result$source_id)
    collapsed <- collapse_mapping_results(grouped, collapse_strategy)

    result  <- stats::setNames(rep(NA_character_, length(ids)), ids)
    matched <- intersect(names(collapsed), ids)
    result[matched] <- collapsed[matched]
    result
  }

  # --- Helper: check abort flag ---
  is_aborted <- function() {
    !is.null(abort_flag) && isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))
  }

  # --- Helper: build an aborted-result object with consistent attributes ---
  make_aborted_result <- function(base_result = NULL, step1_mapped = NULL) {
    if (is.null(base_result)) {
      base_result <- stats::setNames(rep(NA_character_, length(ids)), ids)
    }
    attr(base_result, "aborted")        <- TRUE
    attr(base_result, "two_step")       <- FALSE
    attr(base_result, "direct_biomart") <- FALSE
    if (!is.null(step1_mapped)) {
      attr(base_result, "step1_mapped") <- step1_mapped
    }
    base_result
  }

  # =======================================================================
  # Determine BioMart target attribute
  # =======================================================================
  source_dataset <- species_to_biomart_dataset(source_species)
  target_dataset <- species_to_biomart_dataset(target_species)

  if (is.null(source_dataset) || is.null(target_dataset)) {
    debug_log("Cross-species mapping failed: unrecognized species", 1)
    result <- stats::setNames(rep(NA_character_, length(ids)), ids)
    attr(result, "biomart_error") <- paste0(
      "No BioMart dataset available for ",
      if (is.null(source_dataset)) source_species else target_species,
      ". Try using a species from the initial list or check Ensembl BioMart availability."
    )
    return(result)
  }

  # Check if BioMart can directly provide the requested target keytype
  target_biomart_attr <- orgdb_keytype_to_biomart_attr(target_keytype)
  can_direct_biomart <- !is.null(target_biomart_attr)

  # For cross-species queries, only attempt direct mapping when a Strategy A
  # route exists (homolog equivalent attribute on the source dataset).
  # Without a homolog equivalent, biomart_map_ids would fall back to getLDS
  # (Strategy B), which is fragile and slow.  The two-step path
  # (ensembl_gene_id intermediate via Strategy A + OrgDb conversion) is more
  # reliable and avoids getLDS entirely.
  is_cross <- (source_dataset != target_dataset)
  if (can_direct_biomart && is_cross) {
    target_prefix <- sub("_gene_ensembl$", "", target_dataset)
    has_strategy_a <- !is.null(resolve_homolog_attribute(target_biomart_attr,
                                                         target_prefix))
    if (!has_strategy_a) {
      debug_log(sprintf(
        "Direct BioMart skipped for %s: no homolog equivalent for '%s', using two-step instead",
        target_keytype, target_biomart_attr), 1)
      can_direct_biomart <- FALSE
    }
  }

  if (can_direct_biomart) {
    debug_log(sprintf("Direct BioMart output possible: %s -> %s",
                      target_keytype, target_biomart_attr), 1)
  } else if (!is.null(target_biomart_attr)) {
    debug_log(sprintf("No direct BioMart route for %s; will use ensembl_gene_id intermediate",
                      target_keytype), 1)
  } else {
    debug_log(sprintf("No BioMart equivalent for %s; will use ensembl_gene_id intermediate",
                      target_keytype), 1)
  }

  # =======================================================================
  # Attempt 1: Direct BioMart mapping to final target attribute
  # =======================================================================
  if (can_direct_biomart) {
    report_progress("Step 1: BioMart direct mapping...", 0)
    debug_log(sprintf("Direct mapping: %s [%s] -> %s [%s]",
                      source_dataset, source_attr, target_dataset, target_biomart_attr), 1)

    direct_result <- tryCatch({
      biomart_map_ids(
        source_ids        = as.character(ids),
        source_dataset    = source_dataset,
        source_attr       = source_attr,
        target_dataset    = target_dataset,
        target_attr       = target_biomart_attr,
        abort_flag        = abort_flag,
        progress_callback = progress_callback,
        table_cache       = table_cache,
        debug_log         = debug_log
      )
    }, error = function(e) {
      debug_log(paste("Direct BioMart mapping failed:", e$message), 1)
      NULL
    })

    if (!is.null(direct_result) && nrow(direct_result) > 0) {
      was_aborted <- isTRUE(attr(direct_result, "aborted"))
      collapsed <- collapse_raw(direct_result)
      n_mapped <- sum(!is.na(collapsed))
      n_pct <- round(100 * n_mapped / length(ids), 1)
      debug_log(sprintf("Direct BioMart mapping complete: %d / %d mapped (%.1f%%)",
                        n_mapped, length(ids), n_pct), 1)
      debug_log("Cross-species backend: BioMart (direct, single step)", 1)
      report_progress("Direct BioMart mapping complete", 1)

      if (was_aborted) attr(collapsed, "aborted") <- TRUE
      attr(collapsed, "two_step") <- FALSE
      attr(collapsed, "direct_biomart") <- TRUE
      attr(collapsed, "step1_mapped") <- n_mapped
      return(collapsed)
    }

    # Direct mapping returned no results or failed; check if aborted
    if (!is.null(direct_result) && isTRUE(attr(direct_result, "aborted"))) {
      result <- stats::setNames(rep(NA_character_, length(ids)), ids)
      attr(result, "aborted") <- TRUE
      attr(result, "two_step") <- FALSE
      attr(result, "direct_biomart") <- TRUE
      attr(result, "step1_mapped") <- 0L
      return(result)
    }

    debug_log("Direct BioMart mapping returned no results; falling back to two-step", 1)
  }

  # Abort check before entering the two-step fallback path
  if (is_aborted()) {
    debug_log("Cross-species mapping aborted before two-step fallback", 1)
    return(make_aborted_result())
  }

  # =======================================================================
  # Fallback: Two-step approach (ensembl_gene_id intermediate + OrgDb)
  # =======================================================================
  intermediate_attr <- "ensembl_gene_id"
  report_progress("Step 1: BioMart ortholog mapping (ensembl intermediate)...", 0.05)
  debug_log(sprintf("Step 1 (fallback): BioMart ortholog mapping %s [%s] -> %s [%s]",
                    source_dataset, source_attr, target_dataset, intermediate_attr), 1)

  raw_result <- tryCatch({
    biomart_map_ids(
      source_ids        = as.character(ids),
      source_dataset    = source_dataset,
      source_attr       = source_attr,
      target_dataset    = target_dataset,
      target_attr       = intermediate_attr,
      abort_flag        = abort_flag,
      progress_callback = progress_callback,
      table_cache       = table_cache,
      debug_log         = debug_log
    )
  }, error = function(e) {
    debug_log(paste("Step 1 BioMart mapping failed:", e$message), 1)
    NULL
  })

  if (is.null(raw_result) || nrow(raw_result) == 0) {
    debug_log("Step 1: cross-species mapping returned no results", 1)
    result <- stats::setNames(rep(NA_character_, length(ids)), ids)
    if (!is.null(raw_result)) {
      biomart_err <- attr(raw_result, "biomart_error")
      if (!is.null(biomart_err)) attr(result, "biomart_error") <- biomart_err
      if (isTRUE(attr(raw_result, "aborted"))) attr(result, "aborted") <- TRUE
    }
    attr(result, "two_step") <- FALSE
    attr(result, "direct_biomart") <- FALSE
    return(result)
  }

  was_aborted <- isTRUE(attr(raw_result, "aborted"))
  step1_result <- collapse_raw(raw_result)
  step1_mapped <- sum(!is.na(step1_result))
  step1_pct    <- round(100 * step1_mapped / length(ids), 1)
  debug_log(sprintf("Step 1 complete: %d / %d orthologs found (%.1f%%)",
                    step1_mapped, length(ids), step1_pct), 1)

  # =======================================================================
  # Step 2: Intra-species conversion if target_keytype != "ENSEMBL"
  # =======================================================================
  needs_step2 <- (target_keytype != "ENSEMBL")

  if (!needs_step2 || step1_mapped == 0) {
    debug_log(if (!needs_step2) "Step 2 not needed: target is ENSEMBL"
              else "Step 2 skipped: no orthologs from step 1", 1)
    debug_log("Cross-species backend: BioMart (single step, ensembl intermediate)", 1)
    report_progress("Mapping complete (single step)", 1)
    if (was_aborted) attr(step1_result, "aborted") <- TRUE
    attr(step1_result, "two_step") <- FALSE
    attr(step1_result, "direct_biomart") <- FALSE
    attr(step1_result, "step1_mapped") <- step1_mapped
    return(step1_result)
  }

  # Collect unique intermediate Ensembl IDs for conversion
  intermediate_ids <- unique(na.omit(step1_result))

  # Abort check between step 1 and step 2
  if (is_aborted()) {
    debug_log("Cross-species mapping aborted between step 1 and step 2", 1)
    return(make_aborted_result(step1_result, step1_mapped))
  }

  report_progress("Step 2: OrgDb conversion...", 0.85)
  debug_log(sprintf("Step 2: converting %d unique Ensembl IDs -> %s in %s",
                    length(intermediate_ids), target_keytype, target_species), 1)

  # Load target species OrgDb
  target_orgdb_name <- organism_to_orgdb(target_species)
  target_orgdb <- tryCatch({
    load_organism_cache(target_orgdb_name, max_cache_age_days = 10,
                         ignore_ttl = ignore_ttl,
                         debug_log = debug_log)
  }, error = function(e) {
    debug_log(paste("Step 2: OrgDb cache load failed:", e$message), 1)
    NULL
  })

  if (is.null(target_orgdb)) {
    target_orgdb <- tryCatch({
      load_annotation_hub_with_progress(target_species, debug_log = debug_log,
                                        ignore_ttl = ignore_ttl)
    }, error = function(e) {
      debug_log(paste("Step 2: OrgDb download failed:", e$message), 1)
      NULL
    })
    if (!is.null(target_orgdb)) {
      tryCatch(save_organism_cache(target_orgdb_name, target_orgdb,
                                    debug_log = debug_log),
               error = function(e) NULL)
    }
  }

  # Abort check after OrgDb load (which may have been slow)
  if (is_aborted()) {
    debug_log("Cross-species mapping aborted after step 2 OrgDb load", 1)
    return(make_aborted_result(step1_result, step1_mapped))
  }

  if (is.null(target_orgdb)) {
    debug_log("Step 2 failed: could not load target species OrgDb; returning Ensembl IDs", 1)
    if (was_aborted) attr(step1_result, "aborted") <- TRUE
    attr(step1_result, "two_step") <- FALSE
    attr(step1_result, "direct_biomart") <- FALSE
    attr(step1_result, "step1_mapped") <- step1_mapped
    attr(step1_result, "step2_error") <- paste0(
      "Could not load annotation database for ", target_species,
      ". Returning Ensembl gene IDs from the ortholog mapping step instead of ",
      target_keytype, ".")
    return(step1_result)
  }

  # Perform intra-species mapping: ENSEMBL -> target_keytype
  step2_mapped_vec <- map_ids_intraspecies(
    ids               = intermediate_ids,
    org_db            = target_orgdb,
    from_keytype      = "ENSEMBL",
    to_keytype        = target_keytype,
    collapse_strategy = collapse_strategy,
    debug_log         = debug_log
  )

  step2_input  <- length(intermediate_ids)
  step2_mapped <- sum(!is.na(step2_mapped_vec))
  step2_pct    <- round(100 * step2_mapped / step2_input, 1)
  debug_log(sprintf("Step 2 complete: %d / %d Ensembl IDs converted to %s (%.1f%%)",
                    step2_mapped, step2_input, target_keytype, step2_pct), 1)

  # Compose: for each source ID, chain step1 (ensembl) -> step2 (final)
  final_result <- stats::setNames(rep(NA_character_, length(ids)), ids)
  for (src_id in ids) {
    ensembl_id <- step1_result[[src_id]]
    if (!is.na(ensembl_id)) {
      sub_ids <- strsplit(ensembl_id, ";")[[1]]
      converted <- step2_mapped_vec[sub_ids]
      converted <- converted[!is.na(converted)]
      if (length(converted) > 0) {
        if (collapse_strategy == "semicolon") {
          final_result[[src_id]] <- paste(unique(converted), collapse = ";")
        } else {
          final_result[[src_id]] <- converted[1]
        }
      }
    }
  }

  n_final <- sum(!is.na(final_result))
  debug_log(sprintf("Final result: %d / %d source IDs mapped end-to-end",
                    n_final, length(ids)), 1)
  debug_log("Cross-species backend: BioMart + OrgDb (two-step fallback)", 1)
  report_progress("Two-step mapping complete", 1)

  if (was_aborted) attr(final_result, "aborted") <- TRUE
  attr(final_result, "two_step")       <- TRUE
  attr(final_result, "direct_biomart") <- FALSE
  attr(final_result, "step1_mapped")   <- step1_mapped
  attr(final_result, "step2_input")    <- step2_input
  attr(final_result, "step2_mapped")   <- step2_mapped

  final_result
}
