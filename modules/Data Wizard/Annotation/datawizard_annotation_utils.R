# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils.R
#
# Purpose:
#   General-purpose annotation utility functions that are not specific to
#   BioMart.  Includes organism defaults, ID collapse helpers, intra-species
#   mapping, column management, species abbreviation, and cache date queries.
#
# Architectural Role:
#   Core utils layer of the annotation module.
#   Sourced into modEnv via datawizard_annotation.R. All functions are
#   available to observers and other utils in the same environment.
#
# Key Responsibilities:
#   - Provide default key types per organism for immediate UI population.
#   - Collapse multi-mapped ID results (first / semicolon strategies).
#   - Perform intra-species ID mapping through OrgDb.
#   - Safely add annotation columns to data frames.
#   - Build deterministic column names and retrieve cache timestamps.
#
# Public Functions:
#   1.  get_default_keytypes_for_organism()  - Default key types per species
#   2.  interruptible_sleep()                - Abort-aware sleep utility
#   3.  collapse_mapping_results()           - Collapse multi-mapped results
#   4.  map_ids_intraspecies()               - Within-species ID mapping via OrgDb
#   5.  add_annotation_column()              - Safe column addition with make.unique
#   6.  abbreviate_species()                 - Species name to short token (e.g. "Hs")
#   7.  build_annotation_col_name()          - Deterministic column name builder
#   8.  get_annotation_cache_date()          - Read cache timestamp for active mode
#   9.  split_identifier_cell()              - Split one cell into individual ID tokens
#
# Dependencies:
#   - AnnotationDbi (mapIds for intra-species mapping)
#   - BioMart cache utils (for get_annotation_cache_date)
#
# Integration Points:
#   - Called by datawizard_annotation_observer.R for mapping and column ops.
#   - collapse_mapping_results() used by both intra- and cross-species paths.
#   - interruptible_sleep() used by BioMart retry/backoff logic.
#
# Guidance for Future Developers:
#   - Keep this file free of BioMart-specific logic; BioMart helpers live in
#     the datawizard_annotation_utils_biomart_*.R companion files.
#   - All functions must remain pure (no Shiny dependency) for testability.
# ==============================================================================


#' Get default key types for an organism (fallback when cache is empty).
#'
#' Mirrors the defaults used in GO_module_observer.R so that key type
#' dropdowns are immediately usable without any download.
#'
#' @param orgdb_name Character OrgDb package name (e.g. "org.Hs.eg.db").
#' @return Character vector of default key type names.
get_default_keytypes_for_organism <- function(orgdb_name) {
  if (is.null(orgdb_name) || !is.character(orgdb_name)) {
    return(c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT"))
  }

  switch(orgdb_name,
    "org.Hs.eg.db" = c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT",
                        "REFSEQ", "GENENAME", "ALIAS"),
    "org.Mm.eg.db" = c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT",
                        "REFSEQ", "MGI", "ALIAS"),
    "org.Rn.eg.db" = c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT",
                        "REFSEQ", "RGD", "ALIAS"),
    # All other organisms
    c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT", "REFSEQ", "ALIAS")
  )
}


#' Sleep that can be interrupted by an abort flag.
#'
#' Splits a long sleep into short intervals, checking the abort flag between
#' each one.  Returns immediately with \code{TRUE} if abort is detected.
#'
#' @param seconds Numeric total seconds to sleep.
#' @param abort_flag A reactive or function returning TRUE when abort is
#'   requested, or NULL for non-interruptible sleep.
#' @param interval Numeric poll interval in seconds (default 0.2).
#' @return Logical TRUE if sleep was interrupted by abort, FALSE if completed.
interruptible_sleep <- function(seconds, abort_flag = NULL, interval = 0.2) {
  if (is.null(abort_flag)) {
    Sys.sleep(seconds)
    return(FALSE)
  }
  elapsed <- 0
  while (elapsed < seconds) {
    sleep_time <- min(interval, seconds - elapsed)
    Sys.sleep(sleep_time)
    elapsed <- elapsed + sleep_time
    if (isTRUE(tryCatch(abort_flag(), error = function(e) FALSE))) {
      return(TRUE)
    }
  }
  FALSE
}


#' Collapse multi-mapped results to a single value per input ID.
#'
#' @param mapped_list Named list where each element is a character vector
#'   of mapped values (possibly length > 1).
#' @param strategy Character: "first" (take first match) or
#'   "semicolon" (concatenate with ";").
#' @return Named character vector with one value per input ID (or NA).
collapse_mapping_results <- function(mapped_list, strategy = "first") {
  if (length(mapped_list) == 0) return(character(0))

  vapply(mapped_list, function(vals) {
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) return(NA_character_)
    if (strategy == "semicolon") {
      paste(unique(vals), collapse = ";")
    } else {
      vals[1]
    }
  }, character(1))
}



#' Perform intra-species ID mapping via an OrgDb object.
#'
#' Uses AnnotationDbi::mapIds with multiVals = "list" and collapses
#' results according to the chosen strategy.
#'
#' @param ids Character vector of source identifiers.
#' @param org_db An OrgDb object (loaded from GO cache).
#' @param from_keytype Character source key type (e.g. "SYMBOL").
#' @param to_keytype Character target key type (e.g. "ENSEMBL").
#' @param collapse_strategy Character "first" or "semicolon".
#' @param debug_log Logging function.
#' @return Named character vector: names = input IDs, values = mapped IDs or NA.
map_ids_intraspecies <- function(ids, org_db, from_keytype, to_keytype,
                                 collapse_strategy = "first", debug_log = function(m, l = 1) {}) {
  if (length(ids) == 0) {
    debug_log("map_ids_intraspecies: no IDs provided", 1)
    return(character(0))
  }

  debug_log(sprintf("Mapping %d IDs: %s -> %s (strategy: %s)",
                    length(ids), from_keytype, to_keytype, collapse_strategy), 1)

  tryCatch({
    mapped_list <- AnnotationDbi::mapIds(
      org_db,
      keys     = as.character(ids),
      keytype  = from_keytype,
      column   = to_keytype,
      multiVals = "list"
    )

    result <- collapse_mapping_results(mapped_list, collapse_strategy)

    n_mapped   <- sum(!is.na(result))
    n_unmapped <- sum(is.na(result))
    debug_log(sprintf("Intra-species mapping complete: %d mapped, %d unmapped",
                      n_mapped, n_unmapped), 1)

    result

  }, error = function(e) {
    debug_log(paste("Intra-species mapping failed:", e$message), 1)
    stats::setNames(rep(NA_character_, length(ids)), ids)
  })
}

#' Split a single identifier cell into individual ID tokens.
#'
#' Cells may contain one or more identifiers separated by semicolons or
#' commas, optionally surrounded by whitespace (e.g. "P12345; P67890" or
#' "GENE1,GENE2").  Each token is trimmed of leading/trailing whitespace,
#' and empty tokens produced by consecutive separators or trailing
#' separators are silently dropped.
#'
#' The function is deliberately conservative: it ONLY splits on explicit
#' `;` or `,` characters.  Space-only separation is NOT performed to avoid
#' false splits in gene descriptions or other multi-word identifiers.
#'
#' @param x Character scalar (one cell value).  NA and empty/whitespace-only
#'   strings return \code{character(0)}.
#' @return Character vector of non-empty, trimmed identifier tokens.
#'   A cell with no separator yields a length-1 vector (the original trimmed
#'   value), preserving identical behaviour for single-ID cells.
split_identifier_cell <- function(x) {
  if (is.na(x)) return(character(0))
  x <- trimws(x)
  if (!nzchar(x)) return(character(0))
  tokens <- strsplit(x, "\\s*[;,]\\s*", perl = TRUE)[[1L]]
  tokens <- trimws(tokens)
  tokens[nzchar(tokens)]
}


#' Add a mapped column to the data frame, handling name collisions.
#'
#' Maps the named result vector back to the data frame rows by matching
#' against the source column values. Uses make.unique() if the desired
#' column name already exists.
#'
#' Cells that contain multiple identifiers separated by \code{;} or \code{,}
#' (with optional surrounding whitespace) are handled transparently: each
#' token is looked up individually and the results are combined using
#' \code{collapse_strategy}.  Single-ID cells behave exactly as before.
#'
#' @param data Data frame.
#' @param source_col Character name of the source column in data.
#' @param mapped_values Named character vector from mapping functions.
#' @param target_col_name Character desired new column name.
#' @param collapse_strategy Character \code{"first"} or \code{"semicolon"}.
#'   Controls how multiple mapping hits for a multi-ID cell are combined.
#'   Defaults to \code{"first"} for backward compatibility.
#' @param debug_log Logging function.
#' @return List with data (updated data frame) and new_col_name (actual name used).
add_annotation_column <- function(data, source_col, mapped_values,
                                   target_col_name,
                                   collapse_strategy = "first",
                                   debug_log = function(m, l = 1) {}) {
  if (is.null(data) || !source_col %in% names(data)) {
    debug_log("add_annotation_column: invalid data or source column", 1)
    return(NULL)
  }

  # Handle column name collision
  actual_col_name <- target_col_name
  if (target_col_name %in% names(data)) {
    candidate_names <- make.unique(c(names(data), target_col_name))
    actual_col_name <- candidate_names[length(candidate_names)]
    debug_log(sprintf("Column '%s' exists, using '%s' instead",
                      target_col_name, actual_col_name), 1)
  }

  # Map values back to rows, handling cells that contain multiple identifiers
  # separated by semicolons or commas.  For single-ID cells the behaviour is
  # identical to the previous direct-lookup approach.
  source_ids <- as.character(data[[source_col]])
  new_values <- vapply(source_ids, function(cell) {
    tokens <- split_identifier_cell(cell)
    if (length(tokens) == 0L) return(NA_character_)

    # Look up every token; subscripting with an unknown name yields NA,
    # which is filtered out below without a warning.
    hits <- mapped_values[tokens]
    hits <- hits[!is.na(hits)]
    if (length(hits) == 0L) return(NA_character_)

    if (collapse_strategy == "semicolon") {
      # Each hit may itself be semicolon-collapsed already (from a prior
      # semicolon mapping step); flatten, de-duplicate, and re-join.
      all_vals <- unlist(strsplit(as.character(hits), ";", fixed = TRUE))
      all_vals <- unique(all_vals[nzchar(trimws(all_vals))])
      if (length(all_vals) == 0L) return(NA_character_)
      paste(all_vals, collapse = ";")
    } else {
      # "first": return the first non-NA mapped value as-is.
      as.character(hits[[1L]])
    }
  }, character(1L))
  names(new_values) <- NULL

  data[[actual_col_name]] <- new_values

  debug_log(sprintf("Added column '%s' with %d non-NA values out of %d rows",
                    actual_col_name, sum(!is.na(new_values)), nrow(data)), 1)

  list(data = data, new_col_name = actual_col_name)
}


#' Abbreviate a species display name to a short deterministic token.
#'
#' Takes the first letter of the genus (uppercase) and the first letter of the
#' species epithet (lowercase), e.g. "Homo sapiens" -> "Hs",
#' "Mus musculus" -> "Mm".  Falls back to the first two characters of the
#' input (sanitized, capitalized consistently) when the name does not
#' contain a space.  Returns "NA" for NULL or empty input.
#'
#' @param species_name Character display name (e.g. "Homo sapiens").
#' @return Character short token (e.g. "Hs").
abbreviate_species <- function(species_name) {
  if (is.null(species_name) || !nzchar(species_name)) return("NA")
  parts <- strsplit(trimws(species_name), "\\s+")[[1]]
  if (length(parts) >= 2) {
    paste0(toupper(substring(parts[1], 1, 1)), tolower(substring(parts[2], 1, 1)))
  } else {
    # Single-word name: take first two chars, capitalize first
    raw <- gsub("[^A-Za-z0-9]", "", species_name)
    paste0(toupper(substring(raw, 1, 1)), tolower(substring(raw, 2, 2)))
  }
}


#' Build a deterministic, compact annotation column name.
#'
#' Encodes mapping mode, source species, source key type, target species
#' (cross-species only), and target key type into a single machine-safe
#' column name.  Tokens are separated by underscores.
#'
#' Format:
#'   Intraspecies:  \code{Intra_<SrcSp>_<FromKey>_to_<ToKey>}
#'   Cross-species: \code{Cross_<SrcSp>_<FromKey>_to_<TgtSp>_<ToKey>}
#'
#' @param mode Character: "intra" or "cross".
#' @param source_species Character source species display name.
#' @param from_keytype Character source key type (e.g. "SYMBOL").
#' @param to_keytype Character target key type (e.g. "ENSEMBL").
#' @param target_species Character target species display name (required
#'   when mode is "cross"; ignored for "intra").
#' @return Character sanitized column name.
build_annotation_col_name <- function(mode, source_species, from_keytype,
                                       to_keytype, target_species = NULL) {
  sanitize <- function(x) gsub("[^A-Za-z0-9_]", "", gsub("\\s+", "_", trimws(x)))

  src_sp  <- abbreviate_species(source_species)
  from_kt <- sanitize(from_keytype)
  to_kt   <- sanitize(to_keytype)

  if (identical(mode, "cross") && !is.null(target_species)) {
    tgt_sp <- abbreviate_species(target_species)
    paste0("Cross_", src_sp, "_", from_kt, "_to_", tgt_sp, "_", to_kt)
  } else {
    paste0("Intra_", src_sp, "_", from_kt, "_to_", to_kt)
  }
}


#' Read the cache timestamp for the active annotation mode.
#'
#' For intra-species (OrgDb/AnnotationHub) mode, reads the timestamp from
#' the organism cache metadata (new format) or the legacy cache_timestamp.txt.
#' For cross-species (BioMart) mode, reads the BioMart species cache timestamp.
#'
#' @param mode Character: "intra" or "cross".
#' @param species Character source species display name (used for OrgDb path).
#' @param debug_log Logging function.
#' @return Character formatted date string, or "cache date unavailable".
get_annotation_cache_date <- function(mode, species = NULL,
                                       debug_log = function(m, l = 1) {}) {
  fallback <- "cache date unavailable"
  tryCatch({
    if (identical(mode, "intra") && !is.null(species)) {
      orgdb_name <- organism_to_orgdb(species)

      # Try new metadata format first
      meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)
      if (!is.null(meta) && !is.null(meta$updated)) {
        ts_val <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
        if (!is.na(ts_val)) {
          return(format(ts_val, "%Y-%m-%d %H:%M"))
        }
      }

      # Legacy fallback
      cache_dir  <- get_organism_cache_dir(orgdb_name)
      ts_file    <- file.path(cache_dir, "cache_timestamp.txt")
      if (file.exists(ts_file)) {
        ts_val <- tryCatch(as.POSIXct(readLines(ts_file, n = 1)),
                           error = function(e) NA)
        if (!is.na(ts_val)) {
          return(format(ts_val, "%Y-%m-%d %H:%M"))
        }
      }
    } else if (identical(mode, "cross")) {
      cache_dir <- file.path(get_biomart_cache_dir(), "species")
      ts_file   <- file.path(cache_dir, "species_timestamp.txt")
      if (file.exists(ts_file)) {
        ts_val <- tryCatch(as.POSIXct(readLines(ts_file, n = 1)),
                           error = function(e) NA)
        if (!is.na(ts_val)) {
          return(format(ts_val, "%Y-%m-%d %H:%M"))
        }
      }
    }
    fallback
  }, error = function(e) {
    debug_log(sprintf("get_annotation_cache_date: error (%s)", e$message), 2)
    fallback
  })
}


#' Read the mapping table cache timestamp for BioMart mode.
#'
#' Returns the formatted date string from the most recently cached BioMart
#' mapping table.  This is the second of the two BioMart timestamps displayed
#' in the annotation status output (the first being the keytype cache date
#' returned by \code{get_annotation_cache_date()}).
#'
#' @param debug_log Logging function.
#' @return Character formatted date string, or \code{NULL} if no mapping
#'   table has been cached to disk yet.
get_annotation_mapping_cache_date <- function(debug_log = function(m, l = 1) {}) {
  tryCatch({
    get_biomart_mapping_cache_timestamp(cache_key = NULL, debug_log = debug_log)
  }, error = function(e) {
    debug_log(sprintf("get_annotation_mapping_cache_date: error (%s)", e$message), 2)
    NULL
  })
}
