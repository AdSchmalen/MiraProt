# ==============================================================================
# GO Module Hub - Key Types and Available Organism Discovery
# ==============================================================================
#
# This peer file is sourced into the same environment as GO_module_hub.R.
# Function bodies are kept here to separate hub responsibilities without
# changing resolver behavior.
# ==============================================================================

# 5. Key type cache helpers
# ==============================================================================

#' Load key types from organism cache
#'
#' Uses cache metadata for freshness validation when available; falls back to
#' the legacy cache_timestamp.txt file for backward compatibility.
#'
#' @param orgdb_name organism database name (e.g., "org.Hs.eg.db")
#' @param max_cache_age_days maximum cache age in days
#' @param ignore_ttl logical; when TRUE the cache is returned regardless of
#'   age. Defaults to FALSE.
#' @param debug_log logging function (default: no-op)
#' @return vector of key types or NULL
load_keytypes_from_cache <- function(orgdb_name, max_cache_age_days = 10,
                                     ignore_ttl = FALSE,
                                     debug_log = function(message, level = 1) {}) {

  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)
    keytypes_file <- file.path(cache_dir, "keytypes.rds")

    if (!file.exists(keytypes_file)) {
      debug_log(paste("No keytypes cache found for", orgdb_name), 2)
      return(NULL)
    }

    is_portable <- nzchar(Sys.getenv("MIRAPROT_GO_CACHE", ""))

    # Determine cache age from metadata or legacy timestamp
    age_days <- Inf
    meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)
    if (!is.null(meta)) {
      updated_time <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
      if (!is.na(updated_time)) {
        age_days <- as.numeric(difftime(Sys.time(), updated_time, units = "days"))
      }
    } else {
      # Legacy fallback
      timestamp_file <- file.path(cache_dir, "cache_timestamp.txt")
      if (file.exists(timestamp_file)) {
        cache_time <- tryCatch(as.POSIXct(readLines(timestamp_file)[1]),
                               error = function(e) NA)
        if (!is.na(cache_time)) {
          age_days <- as.numeric(difftime(Sys.time(), cache_time, units = "days"))
        }
      }
    }

    if (!ignore_ttl && !is_portable && age_days > max_cache_age_days) {
      debug_log(paste("Keytypes cache expired for", orgdb_name,
                      "- age:", round(age_days, 1), "days"), 1)
      return(NULL)
    }

    # Load key types from cache
    key_types <- readRDS(keytypes_file)

    debug_log(paste("Loaded", length(key_types), "key types from cache for", orgdb_name,
                    "(age:", round(age_days, 1), "days)"), 1)

    return(key_types)

  }, error = function(e) {
    debug_log(paste("Failed to load keytypes cache for", orgdb_name, ":", e$message), 1)
    return(NULL)
  })
}

#' Save key types to organism cache
#'
#' Saves the keytypes vector alongside the organism cache metadata. If cache
#' metadata already exists, updates the `updated` timestamp. Also writes the
#' legacy cache_timestamp.txt for backward compatibility.
#'
#' @param orgdb_name organism database name
#' @param key_types vector of key types
#' @param debug_log logging function (default: no-op)
save_keytypes_to_cache <- function(orgdb_name, key_types,
                                   debug_log = function(message, level = 1) {}) {

  tryCatch({
    cache_dir <- get_organism_cache_dir(orgdb_name)
    keytypes_file <- file.path(cache_dir, "keytypes.rds")

    # Save key types
    saveRDS(key_types, keytypes_file)

    # Update the metadata timestamp (or write legacy timestamp if no metadata)
    meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)
    if (!is.null(meta)) {
      meta$updated <- as.character(Sys.time())
      .write_cache_metadata(orgdb_name, meta, debug_log = debug_log)
    } else {
      # Legacy path: write timestamp file
      timestamp_file <- file.path(cache_dir, "cache_timestamp.txt")
      writeLines(as.character(Sys.time()), timestamp_file)
    }

    debug_log(paste("Cached", length(key_types), "key types for", orgdb_name), 1)

    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Failed to cache keytypes for", orgdb_name, ":", e$message), 1)
    return(FALSE)
  })
}

#' Load key types with download - uses same structure as force refresh
#'
#' @param orgdb_name organism database name
#' @param debug_log logging function (default: no-op)
#' @return vector of key types or NULL
load_keytypes_with_download <- function(orgdb_name,
                                        debug_log = function(message, level = 1) {}) {

  tryCatch({

    debug_log(paste("Downloading key types for", orgdb_name), 1)

    # In portable mode with a persistent AH cache, reuse it
    use_persistent <- .has_persistent_ah_cache()

    if (use_persistent) {
      cache_dir <- Sys.getenv("ANNOTATION_HUB_CACHE")
      if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
      debug_log(paste("Portable mode: using persistent AH cache:", cache_dir), 1)
    } else {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      random_id <- sample(1000:9999, 1)
      cache_dir <- file.path(tempdir(), paste0("keytypes_download_", timestamp, "_", random_id))
      dir.create(cache_dir, recursive = TRUE)
    }

    # Store original environment
    old_cache_dir <- Sys.getenv("ANNOTATION_HUB_CACHE")

    key_types <- NULL

    tryCatch({

      Sys.setenv(ANNOTATION_HUB_CACHE = cache_dir)

      ah <- suppressMessages(suppressWarnings({
        .with_annotationhub_timeout(
          AnnotationHub(localHub = FALSE, ask = FALSE, cache = cache_dir),
          debug_log = debug_log
        )
      }))

      if (!is.null(ah) && methods::is(ah, "AnnotationHub")) {

        q <- .query_orgdb_records(ah, orgdb_name)

        if (!is.null(q) && length(q) > 0) {

          debug_log(paste("Loading organism database for", orgdb_name), 1)

          org_db <- tryCatch(.annotationhub_fetch_record(q, 1L, debug_log = debug_log), error = function(e) {
            debug_log(paste("AnnotationHub keytype download failed for", orgdb_name, ":", e$message), 1)
            .load_installed_orgdb_fallback(orgdb_name, debug_log = debug_log)
          })

          if (!is.null(org_db)) {
            key_types <- AnnotationDbi::keytypes(org_db)

            if (!is.null(key_types) && length(key_types) > 0) {
              save_keytypes_to_cache(orgdb_name, key_types, debug_log = debug_log)
              debug_log(paste("Successfully downloaded and cached", length(key_types), "key types"), 1)
            }
          }
        }
      }

    }, finally = {

      # Restore original environment
      if (nzchar(old_cache_dir)) {
        Sys.setenv(ANNOTATION_HUB_CACHE = old_cache_dir)
      } else {
        Sys.unsetenv("ANNOTATION_HUB_CACHE")
      }

      # Only delete if it was a throwaway temp dir
      if (!use_persistent && dir.exists(cache_dir)) {
        unlink(cache_dir, recursive = TRUE, force = TRUE)
      }

      gc()
    })

    return(key_types)

  }, error = function(e) {
    debug_log(paste("Error downloading key types for", orgdb_name, ":", e$message), 1)
    return(NULL)
  })
}

# ==============================================================================
# 6. Organism discovery
# ==============================================================================

#' Update Available Organisms - COMPLETELY REWRITTEN (BULLETPROOF)
#'
#' Loads all available organisms from AnnotationHub - GUARANTEES returning a list
#' @param debug_log logging function (default: no-op)
#' @return list with success status and organism choices - ALWAYS A LIST
update_available_organisms_safe <- function(debug_log = function(message, level = 1) {}) {

  # STEP 1: Initialize result - THIS GUARANTEES we return a list
  result <- list(
    success = FALSE,
    organism_choices = NULL,
    organism_count = 0,
    error = NULL
  )

  debug_log("=== UPDATE ORGANISMS STARTED ===", 1)

  # STEP 2: Connect to AnnotationHub - NO EARLY RETURNS
  debug_log("Forcing fresh AnnotationHub connection...", 1)

  ah <- NULL
  ah_error <- NULL

  tryCatch({
    debug_log("Connecting to remote AnnotationHub (bypassing local cache)", 2)
    ah <- .with_annotationhub_timeout(suppressMessages(suppressWarnings({
      AnnotationHub(localHub = FALSE, ask = FALSE)
    })), debug_log = debug_log)
  }, error = function(e) {
    ah_error <<- paste("Connection failed:", e$message)
    debug_log(paste("Remote AnnotationHub connection failed:", e$message), 1)
  })

  if (is.null(ah) || !methods::is(ah, "AnnotationHub")) {
    result$error <- ah_error %||% "Failed to connect to AnnotationHub"
    debug_log("Failed to create AnnotationHub", 1)
    return(result)  # Early return with result list
  }

  debug_log("Successfully connected to AnnotationHub", 1)

  # STEP 3: Query annotations - NO EARLY RETURNS
  debug_log("Querying OrgDb annotations using working pattern...", 1)

  annotations <- NULL
  query_error <- NULL

  tryCatch({
    annotations <- query(ah, pattern = "OrgDb")
  }, error = function(e) {
    query_error <<- paste("Query failed:", e$message)
    debug_log(paste("Pattern query failed:", e$message), 1)
  })

  if (is.null(annotations) || length(annotations) == 0) {
    result$error <- query_error %||% "No OrgDb annotations found"
    debug_log("No annotations found", 1)
    return(result)  # Early return with result list
  }

  debug_log(paste("Found", length(annotations), "OrgDb annotations"), 1)

  # STEP 4: Extract species - NO EARLY RETURNS
  debug_log("Extracting species names...", 1)

  processed_species <- NULL
  species_error <- NULL

  tryCatch({
    debug_log("Accessing annotations$species directly", 2)

    # Check if species column exists
    if (!"species" %in% colnames(mcols(annotations))) {
      species_error <- "Species column not available"
      debug_log("WARNING: No 'species' column found", 1)
    } else {
      unique_species <- unique(annotations$species)
      debug_log(paste("Found", length(unique_species), "unique species"), 2)

      # Apply filters
      sorted_species <- sort(unique_species)
      filtered_species <- sorted_species[grepl("^[A-Za-z0-9 ]+$", sorted_species)]
      debug_log(paste("After alphanumeric filter:", length(filtered_species), "species"), 2)

      filtered_species <- filtered_species[!grepl("^[a-z]", filtered_species, ignore.case = FALSE)]
      debug_log(paste("After lowercase filter:", length(filtered_species), "species"), 2)

      filtered_species <- filtered_species[!is.na(filtered_species) & nzchar(filtered_species)]
      debug_log(paste("After cleanup:", length(filtered_species), "species"), 2)

      if (length(filtered_species) > 0) {
        sample_species <- head(filtered_species, 5)
        debug_log(paste("Sample species found:", paste(sample_species, collapse = ", ")), 1)
        processed_species <- filtered_species
      } else {
        species_error <- "No valid species after filtering"
      }
    }
  }, error = function(e) {
    species_error <<- paste("Species extraction failed:", e$message)
    debug_log(paste("Species extraction failed:", e$message), 1)
  })

  if (is.null(processed_species) || length(processed_species) == 0) {
    result$error <- species_error %||% "Failed to extract species"
    debug_log("No valid species extracted", 1)
    return(result)  # Early return with result list
  }

  # STEP 4b: Build and persist the species <-> orgdb map so that
  # orgdb_to_organism() / organism_to_orgdb() work for every organism.
  tryCatch({
    md <- mcols(annotations)
    if ("title" %in% colnames(md) && "species" %in% colnames(md)) {
      ah_titles  <- as.character(md$title)
      ah_species <- as.character(md$species)

      # Keep only rows that look like valid orgdb entries
      valid <- !is.na(ah_titles) & !is.na(ah_species) &
               nzchar(ah_titles) & nzchar(ah_species) &
               grepl("^org\\.", ah_titles)

      if (any(valid)) {
        # For species with multiple AH entries pick the last one in the hub
        # listing (AnnotationHub appends newer versions at the end).
        pairs <- data.frame(species = ah_species[valid],
                            orgdb   = ah_titles[valid],
                            stringsAsFactors = FALSE)
        pairs <- pairs[!duplicated(pairs$species, fromLast = TRUE), ]

        species_to_orgdb <- setNames(pairs$orgdb, pairs$species)
        save_species_map(species_to_orgdb, debug_log = debug_log)
      }
    } else {
      debug_log("Species map: 'title' or 'species' column missing from AH metadata", 1)
    }
  }, error = function(e) {
    debug_log(paste("Species map build failed (non-fatal):", e$message), 1)
  })

  # STEP 5: Create choices - NO EARLY RETURNS
  debug_log("Creating organism choices...", 1)

  organism_choices <- NULL
  choice_error <- NULL

  tryCatch({
    common_organisms <- c(
      "Homo sapiens", "Mus musculus", "Rattus norvegicus", "Drosophila melanogaster",
      "Caenorhabditis elegans", "Saccharomyces cerevisiae", "Arabidopsis thaliana", "Danio rerio"
    )

    # Order species: common first, then others
    common_found <- intersect(common_organisms, processed_species)
    other_species <- setdiff(processed_species, common_organisms)

    ordered_species <- if (length(common_found) > 0) {
      c(common_found, other_species)
    } else {
      processed_species
    }

    # Create named vector
    organism_choices <- setNames(ordered_species, ordered_species)

    debug_log(paste("Created", length(organism_choices), "organism choices"), 2)

  }, error = function(e) {
    choice_error <<- paste("Choice creation failed:", e$message)
    debug_log(paste("Choice creation failed:", e$message), 1)
  })

  if (is.null(organism_choices) || length(organism_choices) == 0) {
    result$error <- choice_error %||% "Failed to create choices"
    debug_log("No organism choices created", 1)
    return(result)  # Early return with result list
  }

  # STEP 6: SUCCESS - Set all result fields
  result$success <- TRUE
  result$organism_choices <- organism_choices
  result$organism_count <- length(organism_choices)
  result$error <- NULL

  debug_log("=== UPDATE ORGANISMS COMPLETED ===", 1)
  debug_log(paste("Successfully loaded", result$organism_count, "organisms"), 1)

  # GUARANTEE: Always return result list
  return(result)
}

#' Update Organisms with Cache Clear - GUARANTEED FRESH (FIXED)
#'
#' Clears cache first, then loads organisms (like working version)
#' @param debug_log logging function (default: no-op)
#' @return list with success status and organism choices
update_organisms_with_fresh_cache <- function(debug_log = function(message, level = 1) {}) {

  # Initialize result structure to GUARANTEE it's always a list
  result <- list(
    success = FALSE,
    organism_choices = NULL,
    organism_count = 0,
    error = "Function not completed"
  )

  tryCatch({

    debug_log("=== FRESH ORGANISM UPDATE STARTED ===", 1)

    # Step 1: In portable mode, preserve the launcher-managed cache and only
    # remove files known to be corrupt.  Clearing the entire AnnotationHub cache
    # before contacting the server can break offline refreshes if a transient
    # HTTP error (for example 403 Forbidden) occurs during re-download.
    clean_success <- tryCatch({
      clean_corrupt_annotationhub_cache(debug_log = debug_log)
      TRUE
    }, error = function(e) {
      debug_log(paste("Cache cleanup error:", e$message), 1)
      FALSE
    })

    if (!clean_success) {
      debug_log("Warning: Cache cleanup failed, continuing anyway", 1)
    }

    # Step 2: Force fresh connection
    debug_log("Forcing fresh AnnotationHub connection...", 1)

    # Step 3: Run the organism update - GUARANTEE it returns a list
    update_result <- update_available_organisms_safe(debug_log = debug_log)

    # CRITICAL: Validate that we got a proper list back
    if (!is.list(update_result)) {
      debug_log(paste("CRITICAL: update_result is not a list, type:", typeof(update_result)), 1)
      result$error <- "Update function returned invalid type"
      return(result)
    }

    # Copy the result
    result$success <- isTRUE(update_result$success)
    result$organism_choices <- update_result$organism_choices
    result$organism_count <- update_result$organism_count %||% 0
    result$error <- update_result$error

    debug_log("=== FRESH ORGANISM UPDATE COMPLETED ===", 1)

  }, error = function(e) {
    result$error <- paste("Critical error in fresh update:", e$message)
    debug_log("=== FRESH ORGANISM UPDATE FAILED ===", 1)
    debug_log(paste("Critical error:", e$message), 1)
  })

  # GUARANTEE: Always return a proper list
  return(result)
}

#' Load key types for a given organism from AnnotationHub
#'
#' @param organism_name organism name to query
#' @param debug_log logging function (default: no-op)
#' @return vector of key types or NULL
load_organism_keytypes <- function(organism_name,
                                   debug_log = function(message, level = 1) {}) {

  debug_log(paste("Loading key types for:", organism_name), 2)

  tryCatch({

    # Connect to AnnotationHub (prefer local cache)
    ah <- tryCatch({
      .with_annotationhub_timeout(suppressMessages(suppressWarnings({
        AnnotationHub(localHub = TRUE, ask = FALSE)
      })), debug_log = debug_log)
    }, error = function(e) {
      debug_log("Local hub failed, trying remote", 2)
      .with_annotationhub_timeout(suppressMessages(suppressWarnings({
        AnnotationHub(localHub = FALSE, ask = FALSE)
      })), debug_log = debug_log)
    })

    if (is.null(ah)) {
      debug_log("Failed to connect to AnnotationHub", 1)
      return(NULL)
    }

    # Query for the specific organism
    annotations <- tryCatch({
      query(ah, c(organism_name, "OrgDb"))
    }, error = function(e) {
      debug_log(paste("Query failed for", organism_name, ":", e$message), 1)
      return(NULL)
    })

    if (is.null(annotations) || length(annotations) == 0) {
      debug_log(paste("No annotations found for:", organism_name), 1)
      return(NULL)
    }

    # Load the first matching annotation
    org_db <- tryCatch({
      annotations[[1]]
    }, error = function(e) {
      debug_log(paste("Failed to load annotation:", e$message), 1)
      return(NULL)
    })

    if (is.null(org_db)) {
      return(NULL)
    }

    # Get available key types
    key_types <- tryCatch({
      AnnotationDbi::keytypes(org_db)
    }, error = function(e) {
      debug_log(paste("Failed to get key types:", e$message), 1)
      return(NULL)
    })

    if (!is.null(key_types)) {
      debug_log(paste("Found", length(key_types), "key types for", organism_name), 2)
      debug_log(paste("Available types:", paste(head(key_types, 8), collapse = ", ")), 2)
    }

    return(key_types)

  }, error = function(e) {
    debug_log(paste("Critical error loading key types:", e$message), 1)
    return(NULL)
  })
}
