# ==============================================================================
# GO Module - Organism Mapping and Public Annotation Resolvers
# ==============================================================================
#
# Purpose:
#   Defines organism normalization/mapping and the stable public resolver entry
#   points shared by GO enrichment and the Data Wizard Annotation submodule.
#   Acquisition, persistence, and discovery helpers live in peer hub files.
#
# Cache architecture:
#   OrgDb objects are SQLite-backed AnnotationDbi instances and cannot be
#   reliably serialized with saveRDS/readRDS across R sessions. This module
#   therefore uses a SQLite-artifact cache strategy:
#
#     1. After a successful AnnotationHub download, the path to the underlying
#        .sqlite file is resolved via AnnotationDbi::dbfile().
#     2. A copy of the .sqlite file is placed in the organism cache directory.
#     3. On subsequent loads, the OrgDb is reconstructed in-process via
#        AnnotationDbi::loadDb() -- no network access, no serialized R object.
#     4. Structured cache metadata (cache_metadata.rds) tracks status, source,
#        sqlite path, created/updated timestamps, and TTL.
#
#   Migration: Old marker-only layouts (organism_db.rds + cache_timestamp.txt)
#   are detected and upgraded transparently on the next successful download.
#
# Architecture role:
#   This file is sourced with local = TRUE inside modGOServer() in GO_module.R.
#   It depends on debug_log being passed explicitly to all functions.
#   It has no dependency on GO_module_state.R, GO_module_observer.R, or
#   GO_module_logic.R.
#
# Structure:
#   1. Organism name mapping (organism_to_orgdb, orgdb_to_organism, .ah_query_terms,
#      save_species_map, .load_species_map -- dynamic species map cache)
#   2. AnnotationHub loading functions (load_annotation_hub, load_annotation_hub_fresh,
#      load_annotation_hub_with_progress)
#   3. Force refresh and cache management (force_refresh_safe, cleanup_temp_caches,
#      clean_corrupt_annotationhub_cache, clear_annotationhub_cache, clear_organism_cache)
#   4. Organism-specific SQLite-backed cache (get_organism_cache_dir,
#      get_organism_ah_cache_dir, cache metadata helpers, SQLite resolution,
#      save_organism_cache, load_organism_cache, has_valid_organism_cache)
#   5. Key type cache helpers (load_keytypes_from_cache, save_keytypes_to_cache,
#      load_keytypes_with_download)
#   6. Organism discovery (update_available_organisms_safe,
#      update_organisms_with_fresh_cache, load_organism_keytypes)
#
# Future developers:
#   - All network I/O and file-based caching belongs in this file.
#   - Every function that performs logging must receive debug_log as a parameter.
#   - Do not add Shiny reactive logic here; this file is usable outside server context.
#   - The %||% null-coalescing operator is defined in GO_module_logic.R and is
#     available via closure when this file is sourced inside modGOServer.
#   - Do not serialize OrgDb objects with saveRDS. Use the SQLite cache path instead.
# ==============================================================================

# Default no-op logger used when debug_log is not provided by the caller.
.go_hub_null_log <- function(message, level = 1) {}


#' Tag an OrgDb object with the cache/network source used by the GO resolver.
#'
#' @param org_db OrgDb object or NULL
#' @param source Resolver source label
#' @return org_db with a go_annotation_resolver_source attribute when non-NULL
.tag_go_annotation_resolver_source <- function(org_db, source) {
  if (!is.null(org_db)) {
    attr(org_db, "go_annotation_resolver_source") <- source
  }
  org_db
}

# ==============================================================================
# 1. Organism name mapping
# ==============================================================================

# --------------------------------------------------------------------------
# Dynamic species map cache
#
# When update_available_organisms_safe() discovers all OrgDb entries from
# AnnotationHub it persists a bidirectional mapping (species <-> orgdb title)
# to disk.  orgdb_to_organism() and organism_to_orgdb() consult this map so
# that every species advertised by AnnotationHub is queryable -- not only the
# handful of organisms in the hardcoded fallback table.
# --------------------------------------------------------------------------

#' Return the path to the cached species map RDS file.
#' @return character scalar -- file path (may or may not exist yet)
.species_map_path <- function() {
  env_cache <- Sys.getenv("MIRAPROT_GO_CACHE", unset = "")
  base <- if (nzchar(env_cache)) env_cache else file.path("cache", "GO_Cache")
  file.path(base, "species_map.rds")
}

#' Save a species map to disk.
#'
#' @param species_to_orgdb named character vector: names = species display
#'   names, values = orgdb package names (as found in the AH title column).
#' @param debug_log logging function
#' @return TRUE on success, FALSE on error (never throws)
save_species_map <- function(species_to_orgdb,
                             debug_log = function(message, level = 1) {}) {
  tryCatch({
    path <- .species_map_path()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(species_to_orgdb, path)
    debug_log(paste("Saved species map with",
                    length(species_to_orgdb), "entries to", path), 1)
    TRUE
  }, error = function(e) {
    debug_log(paste("Failed to save species map:", e$message), 1)
    FALSE
  })
}

#' Load the cached species map from disk.
#' @return named character vector (species -> orgdb) or NULL
.load_species_map <- function() {
  path <- .species_map_path()
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

#' Common organism display-name to OrgDb mappings.
#' @return named character vector (canonical organism display name -> OrgDb)
.common_organism_orgdb_mappings <- function() {
  c(
    "Homo sapiens" = "org.Hs.eg.db",
    "Mus musculus" = "org.Mm.eg.db",
    "Rattus norvegicus" = "org.Rn.eg.db",
    "Drosophila melanogaster" = "org.Dm.eg.db",
    "Caenorhabditis elegans" = "org.Ce.eg.db",
    "Saccharomyces cerevisiae" = "org.Sc.sgd.db",
    "Danio rerio" = "org.Dr.eg.db",
    "Gallus gallus" = "org.Gg.eg.db",
    "Sus scrofa" = "org.Ss.eg.db",
    "Bos taurus" = "org.Bt.eg.db",
    "Equus caballus" = "org.Ec.eg.db"
  )
}

#' Normalize an organism label to the canonical display name used for cache keys/logging.
#'
#' Converts equivalent labels such as "Homo.sapiens" and "Homo sapiens" to
#' one canonical representation before callers resolve OrgDb package/cache keys.
#'
#' @param organism_name display name (e.g., "Homo sapiens" or "Homo.sapiens")
#' @return canonical organism display name when known, otherwise a cleaned label
normalize_organism_name <- function(organism_name) {
  if (is.null(organism_name) || !is.character(organism_name) || length(organism_name) == 0) {
    return("Homo sapiens")
  }

  organism_clean <- trimws(as.character(organism_name[[1]]))
  organism_clean <- gsub("[._]+", " ", organism_clean)
  organism_clean <- gsub("\\s+", " ", organism_clean)
  organism_clean <- trimws(organism_clean)

  if (!nzchar(organism_clean)) {
    return("Homo sapiens")
  }

  mappings <- .common_organism_orgdb_mappings()
  idx <- match(tolower(organism_clean), tolower(names(mappings)))
  if (!is.na(idx)) {
    return(names(mappings)[[idx]])
  }

  dyn_map <- .load_species_map()
  if (!is.null(dyn_map) && length(dyn_map) > 0) {
    dyn_idx <- match(tolower(organism_clean), tolower(names(dyn_map)))
    if (!is.na(dyn_idx)) {
      return(names(dyn_map)[[dyn_idx]])
    }
  }

  organism_clean
}

#' Convert organism display name to orgdb format.
#'
#' Consults, in order: (1) a hardcoded table of common model organisms,
#' (2) the dynamic species map built by update_available_organisms_safe(),
#' (3) a heuristic that constructs the name from genus/species initials.
#'
#' @param organism_name display name (e.g., "Homo sapiens" or "Homo.sapiens")
#' @return orgdb name (e.g., "org.Hs.eg.db")
organism_to_orgdb <- function(organism_name) {

  organism_clean <- normalize_organism_name(organism_name)

  # Common organism mappings (hardcoded offline fallback)
  mappings <- .common_organism_orgdb_mappings()

  # Check exact matches first
  if (organism_clean %in% names(mappings)) {
    return(mappings[[organism_clean]])
  }

  # Try case-insensitive matching
  for (org_name in names(mappings)) {
    if (tolower(organism_clean) == tolower(org_name)) {
      return(mappings[[org_name]])
    }
  }

  # Consult the dynamic species map (built by update_available_organisms_safe)
  dyn_map <- .load_species_map()
  if (!is.null(dyn_map)) {
    # Exact match
    if (organism_clean %in% names(dyn_map)) {
      return(dyn_map[[organism_clean]])
    }
    # Case-insensitive match
    idx <- match(tolower(organism_clean), tolower(names(dyn_map)))
    if (!is.na(idx)) {
      return(dyn_map[[idx]])
    }
  }

  # Try to construct orgdb name from species name (heuristic fallback)
  parts <- strsplit(organism_clean, " ")[[1]]
  if (length(parts) >= 2) {
    genus_abbrev <- substring(parts[1], 1, 1)
    species_abbrev <- substring(parts[2], 1, 1)
    constructed <- paste0("org.", genus_abbrev, species_abbrev, ".eg.db")
    return(constructed)
  }

  # Fallback
  return("org.Hs.eg.db")
}

#' Reverse-map an orgdb package name back to the organism display name.
#'
#' Used to build AnnotationHub queries that match the species metadata field,
#' because non-model organisms (e.g. Equus caballus) do not carry the
#' abbreviated "org.Xx.eg.db" string in their AnnotationHub record.
#'
#' Consults (1) a hardcoded table of common model organisms and (2) the
#' dynamic species map persisted by update_available_organisms_safe().
#'
#' @param orgdb_name orgdb identifier (e.g., "org.Ec.eg.db")
#' @return organism display name (e.g., "Equus caballus") or NULL if unknown
orgdb_to_organism <- function(orgdb_name) {

  if (is.null(orgdb_name) || !is.character(orgdb_name) || length(orgdb_name) == 0) {
    return(NULL)
  }

  # Hardcoded offline fallback
  reverse_map <- list(
    "org.Hs.eg.db" = "Homo sapiens",
    "org.Mm.eg.db" = "Mus musculus",
    "org.Rn.eg.db" = "Rattus norvegicus",
    "org.Dm.eg.db" = "Drosophila melanogaster",
    "org.Ce.eg.db" = "Caenorhabditis elegans",
    "org.Sc.sgd.db" = "Saccharomyces cerevisiae",
    "org.Dr.eg.db" = "Danio rerio",
    "org.Gg.eg.db" = "Gallus gallus",
    "org.Ss.eg.db" = "Sus scrofa",
    "org.Bt.eg.db" = "Bos taurus",
    "org.Ec.eg.db" = "Equus caballus"
  )

  if (orgdb_name %in% names(reverse_map)) {
    return(reverse_map[[orgdb_name]])
  }

  # Consult the dynamic species map (inverted: orgdb -> species)
  dyn_map <- .load_species_map()
  if (!is.null(dyn_map) && length(dyn_map) > 0) {
    idx <- match(orgdb_name, dyn_map)
    if (!is.na(idx)) {
      return(names(dyn_map)[[idx]])
    }
  }

  return(NULL)
}

#' Build AnnotationHub query terms for an OrgDb package.
#'
#' Uses the full organism name when available (matches the species metadata
#' field in AnnotationHub for all organisms, including non-model ones).
#' Falls back to the orgdb_name for unknown packages.
#'
#' @param orgdb_name orgdb identifier (e.g., "org.Ec.eg.db")
#' @return character vector suitable for \code{AnnotationHub::query()}
.ah_query_terms <- function(orgdb_name) {
  organism <- orgdb_to_organism(orgdb_name)
  search_term <- if (!is.null(organism)) organism else orgdb_name
  c(search_term, "OrgDb")
}

#' Query AnnotationHub for OrgDb records using resilient fallback terms.
#'
#' Some AnnotationHub/Bioc combinations can return 0 hits for one term style
#' (full organism name vs package name). This helper tries both and returns the
#' first non-empty result.
#'
#' @param ah AnnotationHub object
#' @param orgdb_name OrgDb package name (e.g. "org.Hs.eg.db")
#' @return AnnotationHub subset (possibly length 0)
.query_orgdb_records <- function(ah, orgdb_name) {
  terms_primary <- .ah_query_terms(orgdb_name)
  q <- tryCatch(query(ah, terms_primary), error = function(e) NULL)
  if (!is.null(q) && length(q) > 0) return(q)

  terms_fallback <- unique(c(orgdb_name, "OrgDb"))
  q2 <- tryCatch(query(ah, terms_fallback), error = function(e) NULL)
  if (!is.null(q2) && length(q2) > 0) return(q2)

  q
}


#' Load an installed OrgDb package as an AnnotationHub download fallback.
#'
#' AnnotationHub can occasionally return HTTP 403 for individual fetch URLs
#' (seen in portable Windows builds).  When the matching OrgDb package is
#' already installed, use that local package so cache refreshes can still
#' complete and preserve offline portability.
#'
#' @param orgdb_name organism database package name (e.g. "org.Hs.eg.db")
#' @param debug_log logging function
#' @return OrgDb object or NULL
.load_installed_orgdb_fallback <- function(orgdb_name,
                                           debug_log = function(message, level = 1) {}) {
  if (is.null(orgdb_name) || !nzchar(orgdb_name)) return(NULL)

  pkg_loaded <- tryCatch(
    require(orgdb_name, character.only = TRUE, quietly = TRUE),
    error = function(e) FALSE
  )

  if (!isTRUE(pkg_loaded)) {
    debug_log(paste("Installed OrgDb fallback not available for", orgdb_name), 2)
    return(NULL)
  }

  org_db <- tryCatch(
    get(orgdb_name, envir = asNamespace(orgdb_name)),
    error = function(e) {
      debug_log(paste("Installed OrgDb fallback lookup failed for", orgdb_name, ":", e$message), 1)
      NULL
    }
  )

  if (!is.null(org_db)) {
    debug_log(paste("Using installed OrgDb fallback for", orgdb_name), 1)
  }

  org_db
}

# ==============================================================================
load_annotation_hub_with_progress <- function(organism_display,
                                              debug_log = function(message, level = 1) {},
                                              max_cache_age_days = 30,
                                              ignore_ttl = FALSE) {

  # Input validation and conversion
  if (missing(organism_display) || is.null(organism_display) || !is.character(organism_display) || length(organism_display) == 0) {
    organism_display <- "Homo sapiens"
  }

  # Convert to orgdb format
  orgdb_name <- organism_to_orgdb(organism_display)

  debug_log(paste("Loading annotations for:", organism_display, "->", orgdb_name), 1)

  # --- Shared load logic (used by both Shiny and non-Shiny paths) ---
  .do_load <- function(show_progress = FALSE) {
    .inc <- if (show_progress) shiny::incProgress else function(...) invisible(NULL)

    .inc(0.1, detail = "Checking SQLite cache")

    # Step 1: Try SQLite-backed cache (reconstructs OrgDb from .sqlite file)
    org_db <- load_organism_cache(orgdb_name, max_cache_age_days,
                                  ignore_ttl = ignore_ttl,
                                  debug_log = debug_log)
    if (!is.null(org_db)) {
      resolver_source <- attr(org_db, "go_annotation_resolver_source") %||% "SQLite cache"
      debug_log(paste("GO annotation resolver used", resolver_source, "for", orgdb_name), 1)
      .inc(0.8, detail = paste("Loaded from", resolver_source))
      return(org_db)
    }

    # Step 2: Determine fallback strategy
    cache_fresh <- has_valid_organism_cache(orgdb_name,
                                            max_cache_age_days = max_cache_age_days,
                                            ignore_ttl = ignore_ttl,
                                            debug_log = debug_log)

    if (cache_fresh) {
      debug_log(paste("Cache metadata fresh for", orgdb_name,
                      "- trying local AnnotationHub cache"), 1)
      .inc(0.2, detail = "Loading from local AnnotationHub cache")
    } else {
      debug_log(paste("No valid cache for", orgdb_name,
                      "- downloading from AnnotationHub"), 1)
      .inc(0.2, detail = "Downloading annotation database")
    }

    # Step 3: Load via AnnotationHub
    .inc(0.3, detail = "Loading annotation database")
    ah_cache_dir <- get_organism_ah_cache_dir(orgdb_name)

    if (cache_fresh) {
      org_db <- tryCatch({
        ah <- .with_annotationhub_timeout(suppressMessages(suppressWarnings(
          AnnotationHub(localHub = TRUE, ask = FALSE, cache = ah_cache_dir)
        )), debug_log = debug_log)
        q <- .query_orgdb_records(ah, orgdb_name)
        if (length(q) > 0) .annotationhub_fetch_record(q, 1L, debug_log = debug_log) else NULL
      }, error = function(e) {
        debug_log(paste("localHub load failed for", orgdb_name, ":", e$message), 1)
        NULL
      })

      if (is.null(org_db)) {
        debug_log(paste("Local AH cache miss for", orgdb_name,
                        "- falling back to controlled re-download"), 1)
        org_db <- load_annotation_hub_fresh(orgdb_name, debug_log = debug_log)
        org_db <- .tag_go_annotation_resolver_source(org_db, "AnnotationHub/network")
      } else {
        resolver_source <- if (.has_persistent_ah_cache() || nzchar(Sys.getenv("MIRAPROT_GO_CACHE", ""))) {
          "portable cache"
        } else {
          "AnnotationHub/network"
        }
        org_db <- .tag_go_annotation_resolver_source(org_db, resolver_source)
      }
    } else {
      org_db <- load_annotation_hub_fresh(orgdb_name, debug_log = debug_log)
      org_db <- .tag_go_annotation_resolver_source(org_db, "AnnotationHub/network")
    }

    # Step 4: Save to SQLite cache on success
    if (!is.null(org_db)) {
      resolver_source <- attr(org_db, "go_annotation_resolver_source") %||% "AnnotationHub/network"
      debug_log(paste("GO annotation resolver used", resolver_source, "for", orgdb_name), 1)
      .inc(0.7, detail = "Saving to SQLite cache")
      cache_saved <- save_organism_cache(orgdb_name, org_db,
                                         debug_log = debug_log)
      .inc(0.9, detail = "Loading complete")
      if (cache_saved) {
        debug_log(paste("Saved", orgdb_name, "to SQLite-backed cache"), 1)
      } else {
        debug_log(paste("Loaded", orgdb_name,
                        "(cache save failed - session cache only)"), 1)
      }
    } else {
      .inc(0.9, detail = "Load failed")
      debug_log(paste("Failed to load", orgdb_name), 1)
    }

    return(org_db)
  }

  # Check if we are in a Shiny context
  in_shiny <- tryCatch(!is.null(shiny::getDefaultReactiveDomain()),
                        error = function(e) FALSE)

  if (in_shiny) {
    tryCatch({
      shiny::withProgress(
        message = paste("Loading annotations for", organism_display),
        value = 0,
        .do_load(show_progress = TRUE)
      )
    }, error = function(e) {
      debug_log(paste("Progress wrapper error:", e$message), 1)
      .do_load(show_progress = FALSE)
    })
  } else {
    .do_load(show_progress = FALSE)
  }
}

# ==============================================================================
static_orgdb_keytypes <- function(orgdb_name, minimal = FALSE) {
  minimal_types <- c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")
  if (isTRUE(minimal)) return(minimal_types)

  if (grepl("org.Hs", orgdb_name)) {
    c(minimal_types, "REFSEQ", "GENENAME", "ALIAS")
  } else if (grepl("org.Mm", orgdb_name)) {
    c(minimal_types, "REFSEQ", "MGI", "ALIAS")
  } else if (grepl("org.Rn", orgdb_name)) {
    c(minimal_types, "REFSEQ", "RGD", "ALIAS")
  } else {
    c(minimal_types, "REFSEQ", "ALIAS")
  }
}

#' Resolve OrgDb keytypes through shared session, disk, default, and download paths.
#'
#' The returned structure is intentionally UI-agnostic so GO and Annotation can
#' share the cache/default/download decision tree while preserving their own
#' dropdown update and stale-request handling behavior.
#'
#' @param orgdb_name organism database name (e.g. "org.Hs.eg.db")
#' @param mode one of startup, user_change, or force_refresh
#' @param session_cache optional named list used as an in-session shared cache
#' @param max_keytype_cache_age_days strict TTL for keytypes.rds
#' @param max_organism_cache_age_days TTL for using static defaults from an OrgDb cache
#' @param ignore_ttl logical; when TRUE disk cache TTL checks are bypassed
#' @param debug_log logging function
#' @return list with keytypes, source, age_days, should_update_ui, and message
resolve_orgdb_keytypes <- function(orgdb_name,
                                   mode = c("startup", "user_change", "force_refresh"),
                                   session_cache = NULL,
                                   max_keytype_cache_age_days = 10,
                                   max_organism_cache_age_days = 30,
                                   ignore_ttl = FALSE,
                                   debug_log = function(message, level = 1) {}) {
  mode <- match.arg(mode)
  cache <- if (is.list(session_cache)) session_cache else list()
  age_days <- get_organism_cache_age_days(orgdb_name, debug_log = debug_log)
  make_result <- function(keytypes, source, should_update_ui = TRUE, message = NULL) {
    list(
      keytypes = sort(unique(as.character(keytypes %||% character(0)))),
      source = source,
      age_days = age_days,
      should_update_ui = isTRUE(should_update_ui),
      message = message %||% sprintf("Resolved keytypes for %s from %s", orgdb_name, source)
    )
  }

  if (!identical(mode, "force_refresh")) {
    cached_session_types <- cache[[orgdb_name]]
    if (!is.null(cached_session_types) && length(cached_session_types) > 0) {
      debug_log(sprintf("KeyType resolver: session cache hit for %s (%d types)", orgdb_name, length(cached_session_types)), 1)
      return(make_result(cached_session_types, "cache", TRUE,
                         sprintf("Loaded %d key types from session cache", length(cached_session_types))))
    }
  }

  if (identical(mode, "startup") && identical(orgdb_name, "org.Hs.eg.db")) {
    keytypes <- static_orgdb_keytypes(orgdb_name, minimal = TRUE)
    debug_log(sprintf("KeyType resolver: startup static defaults for %s", orgdb_name), 1)
    return(make_result(keytypes, "static_default", FALSE,
                       "Using startup static defaults without cache or download"))
  }

  if (!identical(mode, "force_refresh")) {
    cached_types <- load_keytypes_from_cache(
      orgdb_name,
      max_cache_age_days = max_keytype_cache_age_days,
      ignore_ttl = ignore_ttl,
      debug_log = debug_log
    )
    if (!is.null(cached_types) && length(cached_types) > 0) {
      return(make_result(cached_types, "cache", TRUE,
                         sprintf("Loaded %d key types from disk cache", length(cached_types))))
    }

    if (has_valid_organism_cache(orgdb_name,
                                 max_cache_age_days = max_organism_cache_age_days,
                                 ignore_ttl = ignore_ttl,
                                 debug_log = debug_log)) {
      keytypes <- static_orgdb_keytypes(orgdb_name)
      return(make_result(keytypes, "static_default", TRUE,
                         "Using static keytype defaults from valid organism cache"))
    }
  }

  keytypes <- load_keytypes_with_download(orgdb_name, debug_log = debug_log)
  if (!is.null(keytypes) && length(keytypes) > 0) {
    save_keytypes_to_cache(orgdb_name, keytypes, debug_log = debug_log)
    return(make_result(keytypes, "download", TRUE,
                       sprintf("Downloaded %d key types", length(keytypes))))
  }

  keytypes <- static_orgdb_keytypes(orgdb_name, minimal = if (!identical(mode, "force_refresh")) FALSE else TRUE)
  source <- if (length(keytypes) > 0 && !identical(mode, "force_refresh")) "static_default" else "minimal"
  if (identical(mode, "force_refresh")) source <- "minimal"
  make_result(keytypes, source, TRUE,
              sprintf("Using %s keytype fallback", gsub("_", " ", source)))
}

# ==============================================================================
