# GSEA_module_Gene_Sets.R
#
# Purpose:
#   Handles all gene set loading, preparation, validation, and GSEA result
#   persistence for the GSEA module.
#
# Architecture:
#   This file is sourced by GSEA_module.R and provides stateless helper
#   functions. It has no dependency on Shiny reactives. Functions here cover:
#     - Null coalescing operator
#     - UI choice builders (identifiers, samples, ratios, p-values)
#     - GMT file helpers and ranking method registry
#     - GSEA result save/load (RDS format)
#     - Excel Sheet 5 format detection and import
#
# Structure:
#   1. Null coalescing operator (%||%)
#   2. Ranking method registry
#   3. UI choice builder functions
#   4. Gene set file helpers (listing, validation)
#   5. GSEA result persistence (save_res_GSEA, load_res_GSEA)
#   6. Excel file helpers (is_single_sheet_gsea, read_xlsx_preserve_names)
#   7. Sheet 5 import (import_sheet5_gsea)
#
# Developer notes:
#   - All logging uses gsea_debug_log(message, level, DEBUG_LEVEL) from
#     GSEA_module_logic.R, or a debug_log function parameter where provided.
#   - is_single_sheet_gsea() and read_xlsx_preserve_names() are defined here;
#     they are used by the file validation observer in GSEA_module_observer.R.
#   - import_sheet5_gsea() reconstructs a list-based gseaResult substitute
#     from the chunked column format written by the Excel export system.

# ============================================================
# Null Coalescing Operator
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# Ranking Method Registry
# ============================================================

#' Return the named vector of available GSEA ranking methods
#'
#' @return Named character vector mapping display names to method codes.
gsea_get_rank_methods <- function() {
  c(
    "Signal-to-Noise"                              = "S2N",
    "T-Test"                                       = "ttest",
    "Ratio"                                        = "ratio",
    "Difference of Expression Means Between Classes" = "diff",
    "log2 Ratio"                                   = "log2_ratio",
    "Sum of Ranks"                                 = "SoR",
    "Baumgartner-Weiss-Schinder"                   = "BWS",
    "Weighted Average Difference"                  = "WAD",
    "Fold Change Rank Ordering Statistics"         = "FCROS",
    "MWT"                                          = "MWT",
    "Minimum Significant Difference"               = "MSD"
  )
}

# ============================================================
# UI Choice Builder Functions
# ============================================================

#' Extract identifier column choices from the data definition
#'
#' @param def Data frame; the data definition (metadata).
#' @return Character vector of identifier options.
gsea_get_identifier_choices <- function(def) {
  if (is.null(def) || nrow(def) == 0) return(character(0))
  def_filtered <- def[def$Content == "Identifier", ]
  id_options   <- unique(def_filtered$Options[!is.na(def_filtered$Options) & def_filtered$Options != ""])
  id_options
}

#' Extract sample name choices for a given reference value from the data definition
#'
#' @param def Data frame; the data definition.
#' @param ref_val Character; the selected reference / abundance type.
#' @return Character vector of sample names.
gsea_get_sample_choices <- function(def, ref_val) {
  if (is.null(def) || nrow(def) == 0 || is.null(ref_val)) return(character(0))
  ref_indices  <- which(def$Content == ref_val)
  sample_names <- unique(def$Sample[ref_indices])
  sample_names <- sample_names[!is.na(sample_names) & sample_names != ""]
  sample_names
}

#' Extract abundance ratio column choices from the data definition
#'
#' @param def Data frame; the data definition.
#' @return Character vector of ratio column names.
gsea_get_ratio_choices <- function(def) {
  if (is.null(def) || nrow(def) == 0) return(character(0))
  ratio_indices <- grep("Abundance Ratio", def$Content, ignore.case = TRUE)
  ratio_columns <- unique(def$Column[ratio_indices])
  ratio_columns <- ratio_columns[!is.na(ratio_columns) & ratio_columns != ""]
  ratio_columns
}

#' Extract p-value column choices that match a given abundance ratio column
#'
#' Matches by comparing the numerator/denominator annotations of the ratio
#' column against those of available p-value columns.
#'
#' @param def Data frame; the data definition.
#' @param ratio_col Character; the selected ratio column name.
#' @return Character vector of matching p-value column names.
gsea_get_pvalue_choices <- function(def, ratio_col) {
  if (is.null(def) || nrow(def) == 0 || is.null(ratio_col)) return(character(0))

  required_cols <- c("Content", "Column", "Numerator", "Denominator")
  if (!all(required_cols %in% colnames(def))) return(character(0))

  def_content     <- as.character(def$Content)
  def_column      <- as.character(def$Column)
  def_numerator   <- as.character(def$Numerator)
  def_denominator <- as.character(def$Denominator)

  ratio_row <- which(def_column == ratio_col)[1]
  if (is.na(ratio_row)) return(character(0))

  numerator   <- def_numerator[ratio_row]
  denominator <- def_denominator[ratio_row]

  if (is.na(numerator) || is.na(denominator) || numerator == "" || denominator == "") {
    return(character(0))
  }

  pval_indices <- grep("p.value|pvalue|p_value", def_content, ignore.case = TRUE)
  if (length(pval_indices) == 0) return(character(0))

  matching_pvals <- character(0)
  for (idx in pval_indices) {
    if (!is.na(def_numerator[idx])   && !is.na(def_denominator[idx]) &&
        def_numerator[idx]   == numerator &&
        def_denominator[idx] == denominator) {
      col_name <- def_column[idx]
      if (!is.na(col_name) && nzchar(col_name)) {
        matching_pvals <- c(matching_pvals, col_name)
      }
    }
  }

  unique(matching_pvals)
}

# ============================================================
# Gene Set File Helpers
# ============================================================

# GMT file list cache shared by all GSEA sessions in this R process.
#
# The GSEA directory is normally static while the app is running, so avoid
# hitting the filesystem on every observer invalidation.  Entries are keyed by
# normalized directory path and refreshed only when forced or when the
# directory mtime changes.
.gsea_gmt_file_cache <- new.env(parent = emptyenv())

#' List available GMT files in the GSEA directory
#'
#' Results are cached per directory and invalidated by directory mtime. Use
#' `force_refresh = TRUE` for explicit user-triggered refreshes after adding
#' GMT files while the app is running.
#'
#' @param gsea_dir Character; path to the directory containing .gmt files.
#' @param force_refresh Logical; bypass the cache and rescan the directory.
#' @return Character vector of file names (not full paths).
gsea_list_gmt_files <- function(gsea_dir = "./GSEA", force_refresh = FALSE) {
  cache_key <- normalizePath(gsea_dir, winslash = "/", mustWork = FALSE)

  if (!dir.exists(gsea_dir)) {
    .gsea_gmt_file_cache[[cache_key]] <- list(mtime = NA_real_, files = character(0))
    return(character(0))
  }

  dir_mtime <- file.info(gsea_dir)$mtime
  cached    <- .gsea_gmt_file_cache[[cache_key]]

  if (!force_refresh &&
      !is.null(cached) &&
      identical(cached$mtime, dir_mtime)) {
    return(cached$files)
  }

  files <- list.files(gsea_dir, pattern = "\\.gmt$", full.names = FALSE)
  .gsea_gmt_file_cache[[cache_key]] <- list(mtime = dir_mtime, files = files)
  files
}

# ============================================================
# GSEA Result Persistence (RDS)
# ============================================================

#' Save a GSEA result list to an RDS file
#'
#' @param res_GSEA List; the complete GSEA result wrapper including Results,
#'   GeneList, analysis_metadata, etc.
#' @param path Character; destination file path.
#' @param debug_log Optional function(message, level); logging function.
#' @return Invisibly NULL. Throws an error on failure.
save_res_GSEA <- function(res_GSEA, path, debug_log = NULL) {
  log <- if (is.function(debug_log)) debug_log else function(msg, lvl = 1) gsea_debug_log(msg, lvl, 1)

  if (missing(res_GSEA) || is.null(res_GSEA)) {
    stop("No res_GSEA object found to save.")
  }

  tryCatch({
    saveRDS(res_GSEA, file = path)
    log(paste("Saved res_GSEA to:", normalizePath(path, winslash = "/")), 1)
  }, error = function(e) {
    stop("[GSEA EXPORT] Error while saving res_GSEA: ", e$message)
  })

  invisible(NULL)
}

#' Load a GSEA result list from an RDS file
#'
#' Validates the structure and ensures required fields exist. Sets
#' analysis_metadata$analysis_type to "imported_GSEA_result".
#'
#' @param file Character; path to the RDS file.
#' @param debug_log Optional function(message, level); logging function.
#' @return The loaded and validated res_GSEA list.
load_res_GSEA <- function(file, debug_log = NULL) {
  log <- if (is.function(debug_log)) debug_log else function(msg, lvl = 1) gsea_debug_log(msg, lvl, 1)

  log(paste("Loading GSEA result file:", file), 1)

  if (!file.exists(file)) {
    stop("[GSEA IMPORT] File does not exist: ", file)
  }

  suppressPackageStartupMessages({
    if (!requireNamespace("DOSE", quietly = TRUE)) {
      stop("Package 'DOSE' is required but not installed.")
    }
    if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
      stop("Package 'clusterProfiler' is required but not installed.")
    }
  })

  res <- tryCatch({
    readRDS(file)
  }, error = function(e) {
    log("Direct readRDS failed, attempting binary stream.", 1)
    con <- file(file, "rb")
    on.exit(close(con))
    readRDS(con)
  })

  if (!is.list(res)) {
    stop("[GSEA IMPORT] Loaded object is not a list - invalid GSEA structure.")
  }

  if (!"Results" %in% names(res)) {
    stop("[GSEA IMPORT] No 'Results' element found in the loaded structure.")
  }

  valid_results <- inherits(res$Results, "gseaResult") || inherits(res$Results, "enrichResult")
  if (!valid_results) {
    warning("[GSEA IMPORT] res_GSEA$Results is not a valid clusterProfiler object. Structure kept as-is.")
  } else {
    log(paste("Results class:", class(res$Results)[1]), 1)
  }

  if (!"analysis_metadata" %in% names(res)) {
    log("analysis_metadata missing - creating empty.", 1)
    res$analysis_metadata <- list()
  }
  res$analysis_metadata$analysis_type <- "imported_GSEA_result"

  required_slots <- c("GeneList", "GeneList_FC", "source")
  missing_slots  <- setdiff(required_slots, names(res))
  if (length(missing_slots) > 0) {
    warning("[GSEA IMPORT] Missing fields will be set to NULL: ",
            paste(missing_slots, collapse = ", "))
    for (m in missing_slots) res[[m]] <- NULL
  }

  log(paste("Loaded successfully. Components:", paste(names(res), collapse = ", ")), 1)
  res
}

# ============================================================
# Excel File Helpers
# ============================================================

#' Read an Excel worksheet while preserving original column names
#'
#' Wraps openxlsx::read.xlsx with check.names = FALSE to prevent R from
#' mangling column names that contain special characters or spaces.
#'
#' @param path Character; path to the Excel file.
#' @param sheet Character or integer; sheet name or index.
#' @param startRow Integer; row to start reading from (default 1).
#' @return Data frame with original column names preserved.
read_xlsx_preserve_names <- function(path, sheet = 1, startRow = 1) {
  openxlsx::read.xlsx(path, sheet = sheet, startRow = startRow, check.names = FALSE)
}

#' Test whether an Excel file contains a valid GSEA Sheet 5 format
#'
#' Checks for the presence of the "GSEA_Analysis" sheet and verifies that
#' it contains at least two of the three known signature columns
#' ("GSEA Params", "GSEA Slots", "Integrity Digests") or chunked ranking /
#' gene set columns produced by the export system.
#'
#' @param file_path Character; path to the Excel file.
#' @param sheet_name Character; sheet name to look for (default
#'   "GSEA_Analysis").
#' @return Logical; TRUE if this file appears to be a Sheet 5 GSEA export.
is_single_sheet_gsea <- function(file_path, sheet_name = "GSEA_Analysis") {
  tryCatch({
    sheet_names <- openxlsx::getSheetNames(file_path)
    if (!sheet_name %in% sheet_names) return(FALSE)

    sheet_data <- read_xlsx_preserve_names(file_path, sheet = sheet_name, startRow = 1)
    col_names  <- names(sheet_data)

    known_signatures <- c("GSEA Params", "GSEA Slots", "Integrity Digests")
    found_signatures <- sum(known_signatures %in% col_names)

    has_chunked <- any(grepl("^Ranking Vector \\(\\d+\\)$", col_names)) ||
                   any(grepl("^All Gene Sets \\(\\d+\\)$", col_names))

    (found_signatures >= 2) || has_chunked
  }, error = function(e) {
    FALSE
  })
}

# ============================================================
# Sheet 5 Import
# ============================================================

#' Import a GSEA result from the Sheet 5 Excel format
#'
#' Reconstructs results, ranking vector, fold changes, and gene sets from the
#' chunked column format written by the GSEA export system.
#'
#' @param file_path Character; path to the Excel file.
#' @param debug_log Optional function(message, level); logging function.
#' @return Named list with elements: gsea_results, GeneList, GeneList_FC,
#'   import_time. Returns NULL on error.
import_sheet5_gsea <- function(file_path, debug_log = NULL) {
  log <- if (is.function(debug_log)) {
    debug_log
  } else {
    function(msg, lvl = 1) gsea_debug_log(msg, lvl, 1)
  }

  log("Starting Sheet 5 GSEA import", 1)

  tryCatch({
    raw_data <- openxlsx::read.xlsx(file_path, sheet = "GSEA_Analysis", check.names = FALSE)

    core_columns <- c("Pathway ID", "Description", "Set Size", "Enrichment Score",
                      "NES", "P Value", "Adjusted P Value", "Q Value", "Rank",
                      "Leading Edge", "Core Enrichment Genes")
    results_df   <- raw_data[, intersect(core_columns, names(raw_data)), drop = FALSE]

    rename_back <- c(
      "Pathway ID"           = "ID",
      "Description"          = "Description",
      "Set Size"             = "setSize",
      "Enrichment Score"     = "enrichmentScore",
      "NES"                  = "NES",
      "P Value"              = "pvalue",
      "Adjusted P Value"     = "p.adjust",
      "Q Value"              = "qvalue",
      "Rank"                 = "rank",
      "Leading Edge"         = "leading_edge",
      "Core Enrichment Genes" = "core_enrichment"
    )
    for (old_name in names(rename_back)) {
      if (old_name %in% names(results_df)) {
        names(results_df)[names(results_df) == old_name] <- rename_back[[old_name]]
      }
    }

    # Reconstruct ranking vector from chunked columns
    ranking_vector <- NULL
    ranking_cols   <- grep("^Ranking Vector", names(raw_data), value = TRUE)
    if (length(ranking_cols) > 0) {
      ranking_string <- paste(raw_data[1, ranking_cols], collapse = "")
      if (nzchar(ranking_string)) {
        pairs <- strsplit(ranking_string, ";")[[1]]
        ranking_vector <- setNames(
          as.numeric(sapply(pairs, function(x) strsplit(x, "=")[[1]][2])),
          sapply(pairs,             function(x) strsplit(x, "=")[[1]][1])
        )
      }
    }

    # Reconstruct fold change vector from chunked columns
    fc_vector <- NULL
    fc_cols   <- grep("^Fold Change Vector", names(raw_data), value = TRUE)
    if (length(fc_cols) > 0) {
      fc_string <- paste(raw_data[1, fc_cols], collapse = "")
      if (nzchar(fc_string)) {
        pairs <- strsplit(fc_string, ";")[[1]]
        fc_vector <- setNames(
          as.numeric(sapply(pairs, function(x) strsplit(x, "=")[[1]][2])),
          sapply(pairs,             function(x) strsplit(x, "=")[[1]][1])
        )
      }
    }

    # Reconstruct gene sets from chunked columns
    gene_sets     <- list()
    genesets_cols <- grep("^All Gene Sets", names(raw_data), value = TRUE)
    if (length(genesets_cols) > 0) {
      genesets_string <- paste(raw_data[1, genesets_cols], collapse = "")
      if (nzchar(genesets_string)) {
        pathway_blocks <- strsplit(genesets_string, "\\|\\|")[[1]]
        for (block in pathway_blocks) {
          if (nzchar(block)) {
            parts <- strsplit(block, "\\|")[[1]]
            if (length(parts) == 2) {
              gene_sets[[parts[1]]] <- strsplit(parts[2], ",")[[1]]
            }
          }
        }
      }
    }

    # Parse metadata from JSON columns
    slots_info  <- list()
    params_info <- list()
    if ("GSEA Slots" %in% names(raw_data) && nzchar(raw_data[1, "GSEA Slots"])) {
      slots_info <- tryCatch(jsonlite::fromJSON(raw_data[1, "GSEA Slots"]), error = function(e) list())
    }
    if ("GSEA Params" %in% names(raw_data) && nzchar(raw_data[1, "GSEA Params"])) {
      params_info <- tryCatch(jsonlite::fromJSON(raw_data[1, "GSEA Params"]), error = function(e) list())
    }

    gsea_result <- list(
      result      = results_df,
      organism    = slots_info$organism  %||% "UNKNOWN",
      setType     = slots_info$setType   %||% "UNKNOWN",
      geneSets    = gene_sets,
      geneList    = ranking_vector %||% setNames(numeric(0), character(0)),
      keytype     = slots_info$keytype   %||% "SYMBOL",
      permScores  = list(),
      params      = params_info,
      gene2Symbol = character(0),
      readable    = slots_info$readable  %||% FALSE,
      termsim     = matrix(0, 0, 0),
      method      = slots_info$method    %||% "GSEA",
      dr          = list()
    )
    class(gsea_result)             <- c("gseaResult", "class")
    attr(gsea_result, "package")   <- "DOSE"

    log("Sheet 5 GSEA import completed successfully", 1)

    list(
      gsea_results = gsea_result,
      GeneList     = ranking_vector,
      GeneList_FC  = fc_vector,
      import_time  = Sys.time()
    )
  }, error = function(e) {
    log(paste("Sheet 5 import error:", e$message), 1)
    NULL
  })
}
