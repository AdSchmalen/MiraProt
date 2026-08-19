# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_species.R
#
# Purpose:
#   BioMart species and key-type mapping functions.  Translates between
#   display names, biomaRt dataset identifiers, homolog attribute prefixes,
#   and OrgDb key types.  Also provides live and fallback species/keytype
#   fetching from Ensembl.
#
# Architectural Role:
#   Species/keytype translation layer for cross-species annotation.
#   Sourced into modEnv via datawizard_annotation.R. All functions are
#   available to observers and other utils in the same environment.
#
# Key Responsibilities:
#   - Map species display names to biomaRt datasets and homolog prefixes.
#   - Convert OrgDb key types to biomaRt attribute names.
#   - Fetch live species and keytype lists from Ensembl with retry logic.
#   - Provide static fallback species/keytype data when Ensembl is unavailable.
#
# Public Functions:
#   1.  species_to_biomart_dataset()                  - Display name to biomaRt dataset
#   2.  species_to_homolog_prefix()                   - Display name to homolog attr prefix
#   3.  resolve_homolog_attribute()                    - Target attr to homolog attr name
#   4.  orgdb_keytype_to_biomart_attr()               - OrgDb keytype to biomaRt attribute
#   5.  get_biomart_compatible_keytypes()              - Subset of keytypes valid for BioMart
#   6.  fetch_biomart_species()                       - Live species list from Ensembl (legacy)
#   7.  fetch_biomart_keytypes_for_species()           - Live keytypes from Ensembl per species
#   8.  fetch_biomart_keytypes_live()                  - Low-level keytypes fetch
#   9.  fetch_keytypes_with_retry()                    - Resilient wrapper with retry/backoff
#   10. get_biomart_species_list()                     - Static fallback species list
#   11. ANNOTATION_DEFAULT_SPECIES_PRESET              - Default species subset constant
#   12. BIOMART_SPECIES_FALLBACK                       - Static fallback data frame
#   13. fetch_biomart_species_with_scientific_names()   - Live species with scientific names
#
# Dependencies:
#   - biomaRt (listDatasets, listAttributes, useEnsembl)
#
# Integration Points:
#   - Used by BioMart cache, build, and mapping utils for species resolution.
#   - Used by annotation observers for UI population of species/keytype lists.
#
# Guidance for Future Developers:
#   - When adding new species, update both species_to_biomart_dataset() and
#     species_to_homolog_prefix() mapping tables.
#   - The static fallback lists should be refreshed periodically.
# ==============================================================================

#' Map species display name to biomaRt dataset name.
#'
#' @param species_name Character display name (e.g. "Homo sapiens").
#' @return Character biomaRt dataset name (e.g. "hsapiens_gene_ensembl"),
#'   or NULL if the species is not recognized.
species_to_biomart_dataset <- function(species_name) {
  if (is.null(species_name) || !is.character(species_name)) return(NULL)

  mappings <- list(
    "Homo sapiens"               = "hsapiens_gene_ensembl",
    "Mus musculus"               = "mmusculus_gene_ensembl",
    "Rattus norvegicus"          = "rnorvegicus_gene_ensembl",
    "Danio rerio"                = "drerio_gene_ensembl",
    "Drosophila melanogaster"    = "dmelanogaster_gene_ensembl",
    "Caenorhabditis elegans"     = "celegans_gene_ensembl",
    "Saccharomyces cerevisiae"   = "scerevisiae_gene_ensembl",
    "Bos taurus"                 = "btaurus_gene_ensembl",
    "Gallus gallus"              = "ggallus_gene_ensembl",
    "Sus scrofa"                 = "sscrofa_gene_ensembl",
    "Equus caballus"             = "ecaballus_gene_ensembl"
  )

  dataset <- mappings[[species_name]]
  if (is.null(dataset)) {
    # Try to construct from species name
    parts <- strsplit(tolower(species_name), " ")[[1]]
    if (length(parts) >= 2) {
      dataset <- paste0(substring(parts[1], 1, 1), parts[2], "_gene_ensembl")
    }
  }
  dataset
}


#' Map species display name to homolog attribute prefix used by Ensembl.
#'
#' Ensembl exposes ortholog data as attributes on the source dataset, e.g.
#' \code{mmusculus_homolog_ensembl_gene} on the human dataset.  This function
#' returns the species-specific prefix needed to construct those attribute names.
#'
#' @param species_name Character display name (e.g. "Mus musculus").
#' @return Character prefix (e.g. "mmusculus"), or NULL if unrecognized.
species_to_homolog_prefix <- function(species_name) {
  if (is.null(species_name) || !is.character(species_name)) return(NULL)

  prefixes <- list(
    "Homo sapiens"               = "hsapiens",
    "Mus musculus"               = "mmusculus",
    "Rattus norvegicus"          = "rnorvegicus",
    "Danio rerio"                = "drerio",
    "Drosophila melanogaster"    = "dmelanogaster",
    "Caenorhabditis elegans"     = "celegans",
    "Saccharomyces cerevisiae"   = "scerevisiae",
    "Bos taurus"                 = "btaurus",
    "Gallus gallus"              = "ggallus",
    "Sus scrofa"                 = "sscrofa"
  )

  prefix <- prefixes[[species_name]]
  if (is.null(prefix)) {
    parts <- strsplit(tolower(species_name), " ")[[1]]
    if (length(parts) >= 2) {
      prefix <- paste0(substring(parts[1], 1, 1), parts[2])
    }
  }
  prefix
}



#' Resolve a biomaRt target attribute to its homolog attribute name.
#'
#' \code{ensembl_gene_id}, \code{external_gene_name}, and
#' \code{ensembl_peptide_id} have direct homolog equivalents in Ensembl.
#' For other attributes this function returns NULL, signalling that
#' Strategy A (getBM with homolog attributes) is not applicable.
#' Callers (e.g. \code{map_ids_crossspecies}) use this to decide whether
#' to attempt direct mapping or fall back to a two-step approach.
#'
#' @param target_attr Character biomaRt attribute (e.g. "ensembl_gene_id").
#' @param homolog_prefix Character prefix from \code{species_to_homolog_prefix()}.
#' @return Character homolog attribute name, or NULL if not available.
resolve_homolog_attribute <- function(target_attr, homolog_prefix) {
  if (is.null(target_attr) || is.null(homolog_prefix)) return(NULL)

  mapping <- list(
    "ensembl_gene_id"    = paste0(homolog_prefix, "_homolog_ensembl_gene"),
    "external_gene_name" = paste0(homolog_prefix, "_homolog_associated_gene_name"),
    "ensembl_peptide_id" = paste0(homolog_prefix, "_homolog_ensembl_peptide")
  )

  mapping[[target_attr]]
}


#' Convert an OrgDb key type to the corresponding biomaRt attribute name.
#'
#' AnnotationHub/OrgDb uses uppercase key type names (e.g. "SYMBOL",
#' "ENSEMBL") while biomaRt uses lowercase attribute names (e.g.
#' "external_gene_name", "ensembl_gene_id").  This function provides the
#' deterministic mapping.  Returns NULL for key types that have no valid
#' biomaRt equivalent (e.g. "ALIAS").
#'
#' @param keytype Character OrgDb key type name.
#' @return Character biomaRt attribute name, or NULL if not mappable.
orgdb_keytype_to_biomart_attr <- function(keytype) {
  if (is.null(keytype) || !is.character(keytype) || length(keytype) != 1) return(NULL)

  mapping <- c(
    "SYMBOL"       = "external_gene_name",
    "ENSEMBL"      = "ensembl_gene_id",
    "ENTREZID"     = "entrezgene_id",
    "UNIPROT"      = "uniprot_gn_id",
    "REFSEQ"       = "refseq_mrna",
    "GENENAME"     = "description",
    "MGI"          = "mgi_id",
    "ENSEMBLPROT"  = "ensembl_peptide_id",
    "ENSEMBLTRANS" = "ensembl_transcript_id"
  )

  result <- mapping[keytype]
  if (is.na(result)) NULL else unname(result)
}


#' Return the subset of OrgDb key types that have valid biomaRt equivalents.
#'
#' Used to filter the source key type dropdown when cross-species mode is
#' enabled, ensuring users can only select key types that will produce
#' valid biomaRt queries.
#'
#' @param available_keytypes Character vector of OrgDb key type names
#'   (e.g. from the current dropdown choices or cached key types).
#'   If NULL, returns the core set of always-compatible key types.
#' @return Character vector of key types that have valid biomaRt mappings.
get_biomart_compatible_keytypes <- function(available_keytypes = NULL) {
  # All OrgDb keytypes that have a known biomaRt mapping
  compatible <- c("SYMBOL", "ENSEMBL", "ENTREZID", "UNIPROT",
                   "REFSEQ", "GENENAME", "MGI", "ENSEMBLPROT", "ENSEMBLTRANS")

  if (is.null(available_keytypes)) {
    return(compatible)
  }

  intersect(available_keytypes, compatible)
}


#' Fetch BioMart-compatible species list from Ensembl.
#'
#' Queries \code{biomaRt::listDatasets()} on a live Ensembl connection to
#' obtain all available gene datasets, then extracts species display names.
#' Falls back to a hardcoded list of common model organisms if the network
#' call fails.
#'
#' @param debug_log Logging function with signature (message, level).
#' @return Named character vector suitable for \code{selectInput()} choices,
#'   where names and values are species display names (e.g. "Homo sapiens").
fetch_biomart_species <- function(debug_log = function(m, l = 1) {}) {

  # Hardcoded fallback list (common model organisms)
  fallback <- c(
    "Homo sapiens", "Mus musculus", "Rattus norvegicus", "Danio rerio",
    "Drosophila melanogaster", "Caenorhabditis elegans",
    "Saccharomyces cerevisiae", "Bos taurus", "Gallus gallus", "Sus scrofa"
  )

  species_list <- tryCatch({
    # Connect to Ensembl and list all gene datasets
    ensembl <- .with_biomart_timeout(
      biomaRt::useEnsembl("genes", host = "https://www.ensembl.org"),
      debug_log = debug_log)
    datasets <- .with_biomart_timeout(biomaRt::listDatasets(ensembl),
                                      debug_log = debug_log)

    if (is.null(datasets) || nrow(datasets) == 0) {
      debug_log("fetch_biomart_species: listDatasets returned empty, trying US-East mirror", 1)
      ensembl <- .with_biomart_timeout(
        biomaRt::useEnsembl("genes", host = "https://useast.ensembl.org"),
        debug_log = debug_log)
      datasets <- .with_biomart_timeout(biomaRt::listDatasets(ensembl),
                                        debug_log = debug_log)
    }

    if (!is.null(datasets) && nrow(datasets) > 0 && "description" %in% names(datasets)) {
      # Extract species names from the description column.
      # BioMart descriptions follow the pattern "Species name genes (GRCxxx)"
      raw_desc <- datasets$description
      species_names <- trimws(sub("\\s+genes\\s*\\(.*$", "", raw_desc, ignore.case = TRUE))
      # Filter to non-empty, unique, and alphabetically sorted
      species_names <- sort(unique(species_names[nzchar(species_names)]))
      debug_log(sprintf("fetch_biomart_species: loaded %d species from Ensembl",
                        length(species_names)), 1)
      species_names
    } else {
      debug_log("fetch_biomart_species: could not parse datasets, using fallback", 1)
      fallback
    }
  }, error = function(e) {
    debug_log(sprintf("fetch_biomart_species: network call failed (%s), using fallback",
                      e$message), 1)
    fallback
  })

  stats::setNames(species_list, species_list)
}


#' Fetch BioMart-available keytypes (attributes) for a given species.
#'
#' Connects to the species' Ensembl dataset and queries
#' \code{biomaRt::listAttributes()} to determine which OrgDb-equivalent
#' key types are available. Falls back to the static
#' \code{get_biomart_compatible_keytypes()} set if the network call fails.
#'
#' @param species_name Character display name (e.g. "Homo sapiens").
#' @param debug_log Logging function with signature (message, level).
#' @return Character vector of OrgDb-style key type names (e.g. "SYMBOL",
#'   "ENSEMBL") that are available as BioMart attributes for this species.
fetch_biomart_keytypes_for_species <- function(species_name,
                                                debug_log = function(m, l = 1) {}) {

  # Reverse mapping: biomaRt attribute -> OrgDb keytype
  attr_to_keytype <- c(
    "external_gene_name"      = "SYMBOL",
    "ensembl_gene_id"         = "ENSEMBL",
    "entrezgene_id"           = "ENTREZID",
    "uniprot_gn_id"           = "UNIPROT",
    "refseq_mrna"             = "REFSEQ",
    "description"             = "GENENAME",
    "mgi_id"                  = "MGI",
    "ensembl_peptide_id"      = "ENSEMBLPROT",
    "ensembl_transcript_id"   = "ENSEMBLTRANS"
  )

  fallback <- get_biomart_compatible_keytypes(NULL)

  dataset <- species_to_biomart_dataset(species_name)
  if (is.null(dataset)) {
    debug_log(sprintf("fetch_biomart_keytypes: no dataset for '%s', using fallback",
                      species_name), 1)
    return(fallback)
  }

  result <- tryCatch({
    conn_info <- connect_ensembl_with_mirrors(dataset, debug_log = debug_log)
    if (is.null(conn_info$mart)) {
      debug_log("fetch_biomart_keytypes: connection failed, using fallback", 1)
      return(fallback)
    }

    attrs <- .with_biomart_timeout(biomaRt::listAttributes(conn_info$mart),
                                   debug_log = debug_log)
    if (is.null(attrs) || nrow(attrs) == 0 || !"name" %in% names(attrs)) {
      debug_log("fetch_biomart_keytypes: listAttributes empty, using fallback", 1)
      return(fallback)
    }

    available_attrs <- attrs$name
    # Find which of our known BioMart attributes are present
    present <- names(attr_to_keytype)[names(attr_to_keytype) %in% available_attrs]
    keytypes <- unname(attr_to_keytype[present])

    if (length(keytypes) == 0) {
      debug_log("fetch_biomart_keytypes: no matching attributes found, using fallback", 1)
      return(fallback)
    }

    debug_log(sprintf("fetch_biomart_keytypes: %d keytypes available for %s: %s",
                      length(keytypes), species_name,
                      paste(keytypes, collapse = ", ")), 2)
    keytypes
  }, error = function(e) {
    debug_log(sprintf("fetch_biomart_keytypes: error for '%s' (%s), using fallback",
                      species_name, e$message), 1)
    fallback
  })

  result
}


#' Fetch BioMart keytypes for a species (low-level, no static fallback).
#'
#' Like \code{fetch_biomart_keytypes_for_species()} but does not silently
#' return a static fallback on failure.  Mirror rotation is still used via
#' \code{connect_ensembl_with_mirrors()}, but if all mirrors fail or
#' \code{listAttributes()} returns no matching keytypes, the function returns
#' an error or empty result so the caller can decide retry strategy.
#'
#' @param species_name Character scientific name (e.g. "Homo sapiens").
#' @param start_mirror_idx Integer mirror index to start from (1-based).
#' @param debug_log Logging function with signature (message, level).
#' @return A list with \code{keytypes} (character vector or NULL),
#'   \code{mirror_idx} (integer index of the mirror that succeeded),
#'   and \code{error} (character error message or NULL on success).
fetch_biomart_keytypes_live <- function(species_name,
                                        start_mirror_idx = 1L,
                                        debug_log = function(m, l = 1) {}) {

  attr_to_keytype <- c(
    "external_gene_name"      = "SYMBOL",
    "ensembl_gene_id"         = "ENSEMBL",
    "entrezgene_id"           = "ENTREZID",
    "uniprot_gn_id"           = "UNIPROT",
    "refseq_mrna"             = "REFSEQ",
    "description"             = "GENENAME",
    "mgi_id"                  = "MGI",
    "ensembl_peptide_id"      = "ENSEMBLPROT",
    "ensembl_transcript_id"   = "ENSEMBLTRANS"
  )

  dataset <- species_to_biomart_dataset(species_name)
  if (is.null(dataset)) {
    return(list(keytypes = NULL, mirror_idx = start_mirror_idx,
                error = sprintf("no BioMart dataset mapping for '%s'", species_name)))
  }

  conn_info <- connect_ensembl_with_mirrors(dataset,
                                             start_mirror_idx = start_mirror_idx,
                                             debug_log = debug_log)
  if (is.null(conn_info$mart)) {
    return(list(keytypes = NULL, mirror_idx = conn_info$mirror_idx,
                error = sprintf("all mirrors failed for dataset '%s'", dataset)))
  }

  attrs <- tryCatch(
    .with_biomart_timeout(biomaRt::listAttributes(conn_info$mart),
                          debug_log = debug_log),
    error = function(e) {
      debug_log(sprintf("fetch_biomart_keytypes_live: listAttributes error for %s: %s",
                        species_name, e$message), 1)
      NULL
    }
  )

  if (is.null(attrs) || !is.data.frame(attrs) || nrow(attrs) == 0 ||
      !"name" %in% names(attrs)) {
    return(list(keytypes = NULL, mirror_idx = conn_info$mirror_idx,
                error = "listAttributes returned empty or invalid"))
  }

  available_attrs <- attrs$name
  present <- names(attr_to_keytype)[names(attr_to_keytype) %in% available_attrs]
  keytypes <- unname(attr_to_keytype[present])

  list(keytypes  = if (length(keytypes) > 0) sort(keytypes) else character(0),
       mirror_idx = conn_info$mirror_idx,
       error      = NULL)
}


#' Fetch keytypes for a species with retry, backoff, and mirror rotation.
#'
#' Wraps \code{fetch_biomart_keytypes_live()} with resilient retry logic
#' mirroring the strategy used in \code{biomart_map_ids()}.  On retryable
#' errors (transport/server) the function backs off exponentially with jitter
#' and rotates to the next BioMart mirror.  Non-retryable errors (schema)
#' cause immediate failure.
#'
#' @param species_name Character scientific name (e.g. "Homo sapiens").
#' @param max_retries Integer maximum attempts (default 3).
#' @param start_mirror_idx Integer mirror index to start from (1-based).
#' @param abort_flag Reactive or function returning TRUE when abort requested,
#'   or NULL for non-interruptible.
#' @param debug_log Logging function with signature (message, level).
#' @return A named list:
#'   \describe{
#'     \item{keytypes}{Character vector on success, NULL on failure.}
#'     \item{status}{"success", "failed", "empty" (connected but no keytypes),
#'       "no_dataset", or "aborted".}
#'     \item{error_class}{Error class tag or NULL.}
#'     \item{error_message}{Last error message or NULL.}
#'     \item{attempts}{Integer number of attempts made.}
#'     \item{mirror_idx}{Integer index of last mirror used.}
#'   }
fetch_keytypes_with_retry <- function(species_name,
                                       max_retries = 3L,
                                       start_mirror_idx = 1L,
                                       abort_flag = NULL,
                                       debug_log = function(m, l = 1) {}) {
  n_mirrors <- 3L
  mirror_idx <- start_mirror_idx
  backoff <- 1
  last_error_msg <- NULL
  last_error_class <- NULL

  for (attempt in seq_len(max_retries)) {
    # Abort check
    if (!is.null(abort_flag) &&
        isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))) {
      return(list(keytypes = NULL, status = "aborted",
                  error_class = "abort", error_message = "aborted by user",
                  attempts = attempt, mirror_idx = mirror_idx))
    }

    res <- tryCatch(
      fetch_biomart_keytypes_live(species_name,
                                  start_mirror_idx = mirror_idx,
                                  debug_log = debug_log),
      error = function(e) {
        list(keytypes = NULL, mirror_idx = mirror_idx,
             error = e$message)
      }
    )

    # Success case
    if (is.null(res$error)) {
      if (length(res$keytypes) > 0) {
        return(list(keytypes = res$keytypes, status = "success",
                    error_class = NULL, error_message = NULL,
                    attempts = attempt, mirror_idx = res$mirror_idx))
      } else {
        # Connected but species has no matching attributes
        return(list(keytypes = character(0), status = "empty",
                    error_class = NULL, error_message = "no matching OrgDb-compatible attributes",
                    attempts = attempt, mirror_idx = res$mirror_idx))
      }
    }

    # Failure case: classify error
    last_error_msg <- res$error
    last_error_class <- classify_biomart_error(last_error_msg)

    # Schema errors are non-retryable (dataset doesn't exist, attribute invalid)
    if (identical(last_error_class, "schema")) {
      debug_log(sprintf("fetch_keytypes_with_retry: %s attempt %d/%d SCHEMA error (non-retryable): %s",
                        species_name, attempt, max_retries, last_error_msg), 1)
      return(list(keytypes = NULL, status = "failed",
                  error_class = last_error_class, error_message = last_error_msg,
                  attempts = attempt, mirror_idx = mirror_idx))
    }

    # No dataset mapping: non-retryable
    if (grepl("no BioMart dataset mapping", last_error_msg, fixed = TRUE)) {
      return(list(keytypes = NULL, status = "no_dataset",
                  error_class = "schema", error_message = last_error_msg,
                  attempts = attempt, mirror_idx = mirror_idx))
    }

    # Retryable: rotate mirror and back off
    mirror_idx <- (mirror_idx %% n_mirrors) + 1L

    if (attempt < max_retries) {
      jittered_backoff <- backoff * (1 + stats::runif(1, 0, 0.5))
      debug_log(sprintf(
        "fetch_keytypes_with_retry: %s attempt %d/%d failed [%s]: %s  (retry in %.1fs, next mirror %d)",
        species_name, attempt, max_retries,
        toupper(last_error_class), last_error_msg, jittered_backoff, mirror_idx), 1)

      if (interruptible_sleep(jittered_backoff, abort_flag, interval = 0.25)) {
        return(list(keytypes = NULL, status = "aborted",
                    error_class = "abort", error_message = "aborted during backoff",
                    attempts = attempt, mirror_idx = mirror_idx))
      }
      backoff <- backoff * 2
    } else {
      debug_log(sprintf(
        "fetch_keytypes_with_retry: %s FAILED after %d attempts [%s]: %s",
        species_name, max_retries, toupper(last_error_class), last_error_msg), 1)
    }
  }

  list(keytypes = NULL, status = "failed",
       error_class = last_error_class, error_message = last_error_msg,
       attempts = max_retries, mirror_idx = mirror_idx)
}


#' Get BioMart-compatible species list (static fallback).
#'
#' Returns the hardcoded list of species that have known BioMart dataset
#' mappings. Used as a synchronous fallback when the live
#' \code{fetch_biomart_species()} call is not desired or has failed.
#'
#' @return Named character vector of species display names.
get_biomart_species_list <- function() {
  species <- c(
    "Homo sapiens", "Mus musculus", "Rattus norvegicus", "Danio rerio",
    "Drosophila melanogaster", "Caenorhabditis elegans",
    "Saccharomyces cerevisiae", "Bos taurus", "Gallus gallus", "Sus scrofa"
  )
  stats::setNames(species, species)
}


# ------------------------------------------------------------------------------
# Static fallback data frame for BioMart species (used when network is
# unavailable and no disk cache exists).  Scientific names are authoritative;
# dataset names are the Ensembl gene dataset identifiers.
# ------------------------------------------------------------------------------

# Default species preset shown in species dropdowns before "Update Organisms" is
# clicked. This subset is used both in AnnotationHub mode (UI initial choices) and
# in BioMart/cross-species mode (initial choices on toggle-ON). The full backend
# list is only shown after the user explicitly clicks "Update Organisms".
ANNOTATION_DEFAULT_SPECIES_PRESET <- c(
  "Homo sapiens", "Mus musculus", "Rattus norvegicus",
  "Drosophila melanogaster", "Caenorhabditis elegans", "Saccharomyces cerevisiae",
  "Bos taurus", "Sus scrofa", "Equus caballus"
)

BIOMART_SPECIES_FALLBACK <- data.frame(
  scientific_name = c(
    "Homo sapiens", "Mus musculus", "Rattus norvegicus",
    "Drosophila melanogaster", "Caenorhabditis elegans",
    "Saccharomyces cerevisiae", "Bos taurus", "Sus scrofa",
    "Equus caballus"
  ),
  dataset = c(
    "hsapiens_gene_ensembl", "mmusculus_gene_ensembl", "rnorvegicus_gene_ensembl",
    "dmelanogaster_gene_ensembl",
    "celegans_gene_ensembl", "scerevisiae_gene_ensembl",
    "btaurus_gene_ensembl", "sscrofa_gene_ensembl",
    "ecaballus_gene_ensembl"
  ),
  stringsAsFactors = FALSE
)

.biomart_fallback_species_df <- function() {
  df <- BIOMART_SPECIES_FALLBACK
  attr(df, "biomart_source") <- "fallback"
  df
}


#' Fetch BioMart species list with scientific names.
#'
#' Queries \code{biomaRt::listDatasets()} for available gene datasets and
#' cross-references with the Ensembl REST API (\code{/info/species}) to derive
#' authoritative scientific names (e.g. "Mus musculus", "Equus caballus").
#' Falls back to abbreviated names derived from the dataset prefix if the REST
#' call fails, and ultimately to \code{BIOMART_SPECIES_FALLBACK} if BioMart
#' itself is unreachable.
#'
#' Matching logic: for each BioMart dataset prefix (e.g. "mmusculus"), the
#' genus initial and species epithet are compared against the first character of
#' the REST genus and the last underscore-separated part of the REST name
#' respectively.  This handles both simple binomials and subspecies entries
#' (e.g. "cfamiliaris" matches "canis_lupus_familiaris").
#'
#' @param debug_log Logging function with signature (message, level).
#' @return A \code{data.frame} with columns \code{scientific_name} and
#'   \code{dataset}, sorted by \code{scientific_name}.  Never NULL; falls back
#'   to \code{BIOMART_SPECIES_FALLBACK} on complete failure.
fetch_biomart_species_with_scientific_names <- function(
    debug_log = function(m, l = 1) {}) {

  # -- Step 1: Fetch BioMart dataset list --------------------------------------
  datasets <- tryCatch({
    ensembl <- .with_biomart_timeout(
      biomaRt::useEnsembl("genes", host = "https://www.ensembl.org"),
      debug_log = debug_log)
    ds <- .with_biomart_timeout(biomaRt::listDatasets(ensembl),
                                debug_log = debug_log)
    if (is.null(ds) || nrow(ds) == 0) {
      debug_log("fetch_biomart_species_with_scientific_names: www empty, trying US-East", 1)
      ensembl <- .with_biomart_timeout(
        biomaRt::useEnsembl("genes", host = "https://useast.ensembl.org"),
        debug_log = debug_log)
      ds <- .with_biomart_timeout(biomaRt::listDatasets(ensembl),
                                  debug_log = debug_log)
    }
    ds
  }, error = function(e) {
    debug_log(sprintf(
      "fetch_biomart_species_with_scientific_names: BioMart call failed (%s), using fallback",
      e$message), 1)
    NULL
  })

  if (is.null(datasets) || nrow(datasets) == 0 || !"dataset" %in% names(datasets)) {
    debug_log("fetch_biomart_species_with_scientific_names: no datasets available, using BIOMART_SPECIES_FALLBACK", 1)
    return(.biomart_fallback_species_df())
  }

  # Filter to gene datasets only
  gene_mask <- grepl("_gene_ensembl$", datasets$dataset)
  gene_datasets <- datasets[gene_mask, , drop = FALSE]
  if (nrow(gene_datasets) == 0) {
    return(.biomart_fallback_species_df())
  }

  # -- Step 2: Fetch Ensembl REST species list for scientific names ------------
  rest_species <- tryCatch({
    resp <- jsonlite::fromJSON(
      "https://rest.ensembl.org/info/species?content-type=application/json",
      simplifyDataFrame = TRUE
    )
    if (is.data.frame(resp$species) && "name" %in% names(resp$species)) {
      resp$species
    } else {
      NULL
    }
  }, error = function(e) {
    debug_log(sprintf(
      "fetch_biomart_species_with_scientific_names: REST API failed (%s), deriving names from dataset",
      e$message), 1)
    NULL
  })

  # Helper: derive scientific name from a REST species name string
  # e.g. "homo_sapiens" -> "Homo sapiens", "canis_lupus_familiaris" -> "Canis lupus familiaris"
  rest_name_to_scientific <- function(name) {
    parts <- strsplit(name, "_")[[1]]
    parts[1] <- paste0(toupper(substr(parts[1], 1, 1)),
                       substr(parts[1], 2, nchar(parts[1])))
    paste(parts, collapse = " ")
  }

  # Helper: derive abbreviated fallback name from BioMart dataset prefix
  # e.g. "hsapiens" -> "H. sapiens"
  prefix_to_abbreviated <- function(prefix) {
    if (nchar(prefix) < 2) return(prefix)
    paste0(toupper(substr(prefix, 1, 1)), ". ", substr(prefix, 2, nchar(prefix)))
  }

  # -- Step 3: Match each BioMart dataset to a scientific name -----------------
  result_rows <- lapply(seq_len(nrow(gene_datasets)), function(i) {
    ds_name  <- gene_datasets$dataset[i]
    prefix   <- sub("_gene_ensembl$", "", ds_name)  # e.g. "hsapiens"
    if (nchar(prefix) < 2) return(NULL)

    genus_initial   <- tolower(substr(prefix, 1, 1))
    species_epithet <- tolower(substr(prefix, 2, nchar(prefix)))

    scientific_name <- NA_character_

    if (!is.null(rest_species)) {
      # Match: genus initial and last part of REST name == species epithet
      for (j in seq_len(nrow(rest_species))) {
        rest_name  <- tolower(rest_species$name[j])
        rest_parts <- strsplit(rest_name, "_")[[1]]
        if (length(rest_parts) < 2) next
        rest_g_initial <- substr(rest_parts[1], 1, 1)
        rest_sp_last   <- rest_parts[length(rest_parts)]
        if (rest_g_initial == genus_initial && rest_sp_last == species_epithet) {
          scientific_name <- rest_name_to_scientific(rest_species$name[j])
          break
        }
      }
    }

    # Fallback: abbreviated form from dataset prefix
    if (is.na(scientific_name)) {
      scientific_name <- prefix_to_abbreviated(prefix)
    }

    data.frame(scientific_name = scientific_name, dataset = ds_name,
               stringsAsFactors = FALSE)
  })

  result_df <- do.call(rbind, Filter(Negate(is.null), result_rows))

  if (is.null(result_df) || nrow(result_df) == 0) {
    debug_log("fetch_biomart_species_with_scientific_names: matching produced no rows, using BIOMART_SPECIES_FALLBACK", 1)
    return(.biomart_fallback_species_df())
  }

  result_df <- result_df[order(result_df$scientific_name), ]
  rownames(result_df) <- NULL

  debug_log(sprintf(
    "fetch_biomart_species_with_scientific_names: %d species resolved (REST match: %s)",
    nrow(result_df),
    if (!is.null(rest_species)) "yes" else "no (abbreviated fallback)"), 1)

  result_df
}
