# ==============================================================================
# GO Module - Pure Logic and Helper Functions
# ==============================================================================
#
# Purpose:
#   Provides readiness, column pairing, identifier preparation, enrichment
#   execution, result construction, and reusable logic helpers for the GO module.
#   No Shiny reactive logic, AnnotationHub access, or file-based caching belongs here.
#
# Architecture role:
#   This file is sourced with local = TRUE inside modGOServer() in GO_module.R,
#   after GO_module_hub.R. All functions are available to GO_module_state.R and
#   GO_module_observer.R through the shared server closure.
#
# Structure:
#   1. Utility helpers (%||%, safe_any, get_color_limits)
#   2. Data validation (check_go_data_readiness, check_pairing_prerequisites)
#   3. Column choice and pairing helpers
#   4. GO analysis conversion helpers (convert_ontology_input, convert_padjust_method)
#   5. GO enrichment execution (perform_go_enrichment, create_go_results_list_direct)
#
# Future developers:
#   - All functions that log must receive debug_log as an explicit parameter with
#     a default no-op: debug_log = function(message, level = 1) {}
#   - Do not add AnnotationHub/network/cache logic here; that belongs in GO_module_hub.R.
#   - Do not add Shiny reactive logic here; that belongs in GO_module_observer.R.
#   - The %||% operator defined here is used by GO_module_hub.R via closure.
# ==============================================================================

# ==============================================================================
# 1. Utility Helpers
# ==============================================================================

#' Null-Coalescing Operator
#'
#' Returns right-hand side if left-hand side is NULL
#' @param lhs left-hand side value
#' @param rhs right-hand side value (default)
#' @return lhs if not NULL, otherwise rhs
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0) lhs else rhs
}

#' Safe Any Function with NA Handling
#'
#' Wrapper around any() that properly handles NA values
#' @param logical_vector logical vector that may contain NA
#' @return logical value (never NA)
safe_any <- function(logical_vector) {
  if (length(logical_vector) == 0) {
    return(FALSE)
  }

  clean_vector <- logical_vector[!is.na(logical_vector)]

  if (length(clean_vector) == 0) {
    return(FALSE)
  }

  result <- any(clean_vector)

  if (is.na(result)) {
    return(FALSE)
  }

  return(result)
}


#' Compute GO Data Readiness
#'
#' Pure helper that summarizes whether data definition metadata contains the
#' abundance-ratio and p-value annotations required by GO analysis.
#' @param data_def data definition dataframe
#' @return list with ready, has_abundance, has_pvalue, abundance_count, pvalue_count
compute_go_readiness <- function(data_def) {
  result <- list(
    ready = FALSE,
    has_abundance = FALSE,
    has_pvalue = FALSE,
    abundance_count = 0L,
    pvalue_count = 0L
  )

  if (is.null(data_def) || !is.data.frame(data_def) || nrow(data_def) == 0 ||
      !"Content" %in% names(data_def)) {
    return(result)
  }

  content_col <- data_def$Content
  valid_content <- content_col[!is.na(content_col)]

  if (length(valid_content) == 0) {
    return(result)
  }

  abundance_matches <- grepl("Abundance Ratio", valid_content, ignore.case = TRUE)
  pvalue_matches <- grepl("p-Value|p-value", valid_content, ignore.case = TRUE)

  result$abundance_count <- sum(abundance_matches, na.rm = TRUE)
  result$pvalue_count <- sum(pvalue_matches, na.rm = TRUE)
  result$has_abundance <- result$abundance_count > 0L
  result$has_pvalue <- result$pvalue_count > 0L
  result$ready <- result$has_abundance && result$has_pvalue

  result
}

#' Get Color Limits
#'
#' Calculate appropriate color scale limits for plots
#' @param values numeric vector of values
#' @return vector with min, mid, max values
get_color_limits <- function(values) {
  if (length(values) == 0) return(c(0, 0.5, 1))
  vals <- as.numeric(values)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(c(0, 0.5, 1))
  return(c(min(vals), median(vals), max(vals)))
}

# ==============================================================================
# 2. Data Validation
# ==============================================================================

#' Check Prerequisites for Column Pairing (FIXED)
#'
#' Validates that all required data is available for automatic pairing
#' @param rv reactive values object
#' @param debug_log logging function
#' @return logical indicating if pairing can proceed
check_pairing_prerequisites <- function(rv, debug_log = function(message, level = 1) {}) {

  debug_log("Starting pairing prerequisites check", 2)

  if (is.null(rv)) {
    debug_log("Reactive values object is NULL", 1)
    return(FALSE)
  }

  if (is.null(rv$data_mod)) {
    debug_log("No data_mod available", 2)
    return(FALSE)
  }

  if (is.null(rv$data_def)) {
    debug_log("No data_def available", 2)
    return(FALSE)
  }

  if (!is.data.frame(rv$data_def)) {
    debug_log("data_def is not a data frame", 1)
    return(FALSE)
  }

  if (!"Content" %in% names(rv$data_def)) {
    debug_log("data_def missing Content column", 1)
    return(FALSE)
  }

  if (nrow(rv$data_def) == 0) {
    debug_log("data_def is empty", 2)
    return(FALSE)
  }

  content_column <- rv$data_def$Content
  valid_content <- content_column[!is.na(content_column)]

  debug_log(paste("Total Content entries:", length(content_column),
                  "Valid entries:", length(valid_content)), 2)

  if (length(valid_content) == 0) {
    debug_log("No valid Content entries found (all NA)", 2)
    return(FALSE)
  }

  abundance_matches <- valid_content == "Abundance Ratio"
  has_abundance <- length(abundance_matches) > 0 && any(abundance_matches)

  if (is.na(has_abundance)) {
    has_abundance <- FALSE
  }

  if (!has_abundance) {
    debug_log("No abundance ratio columns found", 2)
    return(FALSE)
  }

  pvalue_types <- c("Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value")
  pvalue_matches <- valid_content %in% pvalue_types
  has_pvalue <- length(pvalue_matches) > 0 && any(pvalue_matches)

  if (is.na(has_pvalue)) {
    has_pvalue <- FALSE
  }

  if (!has_pvalue) {
    debug_log("No p-value columns found", 2)
    return(FALSE)
  }

  debug_log("All prerequisites met for pairing", 1)
  return(TRUE)
}

#' Enhanced Data Readiness Check
#'
#' Comprehensive check if GO module data is ready for analysis
#' @param rv reactive values object
#' @param debug_log logging function
#' @return logical indicating data readiness
check_go_data_readiness <- function(rv, debug_log = function(message, level = 1) {}) {

  debug_log("Checking GO data readiness", 3)

  if (is.null(rv)) {
    debug_log("Reactive values object is NULL", 1)
    return(FALSE)
  }

  if (datawizard_metadata_defer_downstream_choices(rv)) {
    debug_log("Metadata assignment pending; deferring GO data readiness check", 2)
    return(FALSE)
  }

  data_available <- !is.null(rv$data_mod) && !is.null(rv$data_def)

  if (!data_available) {
    debug_log("Required data (data_mod or data_def) not available", 2)
    return(FALSE)
  }

  if (!is.data.frame(rv$data_mod) || !is.data.frame(rv$data_def)) {
    debug_log("Data structures are not data frames", 1)
    return(FALSE)
  }

  if (nrow(rv$data_mod) == 0 || nrow(rv$data_def) == 0) {
    debug_log("Empty data detected", 1)
    return(FALSE)
  }

  readiness <- compute_go_readiness(rv$data_def)

  if (!readiness$has_abundance && !readiness$has_pvalue) {
    debug_log("No GO abundance ratio or p-value content types found", 1)
  } else if (!readiness$has_abundance) {
    debug_log("No GO abundance ratio content types found", 1)
  } else if (!readiness$has_pvalue) {
    debug_log("No GO p-value content types found", 1)
  }

  return(readiness$ready)
}

# ==============================================================================
# 3. Column Choice and Pairing Helpers
# ==============================================================================

#' Enhanced Column Choices Retrieval with Content Filtering
#'
#' Gets available column choices based on content type with robust error handling
#' @param data_def data definition dataframe
#' @param content_pattern content pattern to match
#' @param debug_log logging function
#' @param exact_match whether to require an exact, case-sensitive content match
#' @return named character vector of column choices
get_column_choices_by_content <- function(data_def, content_pattern,
                                          debug_log = function(message, level = 1) {},
                                          exact_match = FALSE) {

  debug_log(paste("Getting column choices for content pattern:", content_pattern), 3)

  if (is.null(data_def) || !is.data.frame(data_def)) {
    debug_log("Invalid data_def input", 1)
    return(character(0))
  }

  if (is.null(content_pattern) || !nzchar(content_pattern)) {
    debug_log("Invalid content_pattern", 1)
    return(character(0))
  }

  required_cols <- c("Content", "Column")
  missing_cols <- required_cols[!required_cols %in% names(data_def)]

  if (length(missing_cols) > 0) {
    debug_log(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), 1)
    return(character(0))
  }

  content_col <- data_def$Content
  column_col <- data_def$Column

  valid_rows <- which(!is.na(content_col) & !is.na(column_col))

  if (length(valid_rows) == 0) {
    debug_log("No valid rows with both Content and Column values", 1)
    return(character(0))
  }

  content_matches <- if (isTRUE(exact_match)) {
    content_col[valid_rows] == content_pattern
  } else {
    grepl(content_pattern, content_col[valid_rows], ignore.case = TRUE)
  }
  matching_indices <- valid_rows[content_matches]

  if (length(matching_indices) == 0) {
    debug_log(paste("No columns found matching pattern:", content_pattern), 3)
    return(character(0))
  }

  matching_columns <- column_col[matching_indices]
  valid_columns <- matching_columns[!is.na(matching_columns) & nzchar(trimws(matching_columns))]

  if (length(valid_columns) == 0) {
    debug_log("No valid column names found for matching content", 1)
    return(character(0))
  }

  choices <- setNames(valid_columns, valid_columns)

  debug_log(paste("Found", length(choices), "column choices"), 3)

  return(choices)
}

#' Enhanced Gene Identifier Search with Robust NA Handling
#'
#' Searches for available gene identifier columns in metadata
#' @param data_def data definition dataframe
#' @param debug_log logging function
#' @return named character vector of identifier choices
get_gene_identifier_choices <- function(data_def, debug_log = function(message, level = 1) {}) {

  debug_log("Searching for gene identifier columns", 2)

  if (is.null(data_def) || !is.data.frame(data_def)) {
    debug_log("Invalid data_def input", 1)
    return(character(0))
  }

  required_cols <- c("Content", "Options", "Column")
  missing_cols <- required_cols[!required_cols %in% names(data_def)]

  if (length(missing_cols) > 0) {
    debug_log(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), 1)
    return(character(0))
  }

  if (nrow(data_def) == 0) {
    debug_log("Empty data_def", 2)
    return(character(0))
  }

  content_col <- data_def$Content
  valid_content_indices <- which(!is.na(content_col))

  if (length(valid_content_indices) == 0) {
    debug_log("All Content values are NA", 1)
    return(character(0))
  }

  identifier_indices <- valid_content_indices[
    grepl("Identifier", content_col[valid_content_indices], ignore.case = TRUE)
  ]

  if (length(identifier_indices) == 0) {
    debug_log("No identifier rows found", 1)
    return(character(0))
  }

  identifier_rows <- data_def[identifier_indices, ]

  options_col <- identifier_rows$Options
  valid_options <- options_col[!is.na(options_col) & nzchar(trimws(options_col))]

  if (length(valid_options) == 0) {
    debug_log("No valid Options found in identifier rows", 1)
    return(character(0))
  }

  choices <- unique(valid_options)

  named_choices <- character(length(choices))
  names(named_choices) <- choices

  for (i in seq_along(choices)) {
    matching_rows <- identifier_rows[
      !is.na(identifier_rows$Options) & identifier_rows$Options == choices[i],
    ]

    if (nrow(matching_rows) > 0) {
      named_choices[i] <- matching_rows$Column[1]
    } else {
      debug_log(paste("No matching column found for option:", choices[i]), 2)
      named_choices[i] <- choices[i]
    }
  }

  debug_log(paste("Found", length(named_choices), "gene identifier options"), 1)

  return(named_choices)
}

#' Find Best P-Value Partner for Abundance Ratio Column
#'
#' Uses volcano module pairing logic to find the most likely p-value partner
#' @param selected_ratio_col selected abundance ratio column name
#' @param pval_type_selection selected p-value type
#' @param data_def data definition dataframe
#' @param debug_log logging function
#' @return best matching p-value column name or NULL
find_best_pvalue_partner <- function(selected_ratio_col, pval_type_selection, data_def,
                                     debug_log = function(message, level = 1) {}) {

  diagnostics_enabled <- isTRUE(getOption("miraprot.go.pairing_diagnostics", FALSE))

  debug_log(paste("Selected abundance column:", selected_ratio_col), 1)

  ratio_idx <- which(data_def$Column == selected_ratio_col & data_def$Content == "Abundance Ratio")
  if (length(ratio_idx) == 0) {
    debug_log("Selected ratio column not found in metadata", 1)
    return(NULL)
  }

  ratio_idx <- ratio_idx[1]

  pval_indices <- which(data_def$Content == pval_type_selection)
  debug_log(paste("P-value candidate count:", length(pval_indices), "for type:", pval_type_selection), 1)

  if (length(pval_indices) == 0) {
    debug_log(paste("No p-value columns found for type:", pval_type_selection), 1)
    return(NULL)
  }

  if (length(pval_indices) == 1) {
    best_col <- data_def$Column[pval_indices[1]]
    debug_log(paste("Selected best match:", best_col), 1)
    debug_log("Best similarity score: 1", 1)
    return(best_col)
  }

  best_match <- NULL
  best_similarity <- 0

  ratio_col_name <- data_def$Column[ratio_idx]

  for (pval_idx in pval_indices) {
    pval_col_name <- data_def$Column[pval_idx]

    similarity <- calculate_go_column_similarity(ratio_col_name, pval_col_name, ratio_idx, pval_idx,
                                                 data_def, debug_log = debug_log)

    if (diagnostics_enabled) {
      debug_log(paste("Similarity between", ratio_col_name, "and", pval_col_name, ":", round(similarity, 3)), 2)
    }

    if (similarity > best_similarity) {
      best_similarity <- similarity
      best_match <- pval_col_name
    }
  }

  if (best_similarity > 0.3) {
    debug_log(paste("Selected best match:", best_match), 1)
    debug_log(paste("Best similarity score:", round(best_similarity, 3)), 1)
    return(best_match)
  }

  debug_log("Selected best match: none", 1)
  debug_log(paste("Best similarity score:", round(best_similarity, 3)), 1)
  return(NULL)
}

#' Calculate Column Similarity for GO Pairing
#'
#' Adapted similarity calculation specifically for GO module pairing
#' @param ratio_name ratio column name
#' @param pval_name p-value column name
#' @param ratio_idx ratio column index in data_def
#' @param pval_idx p-value column index in data_def
#' @param data_def data definition dataframe
#' @param debug_log logging function
#' @return similarity score (0-1)
calculate_go_column_similarity <- function(ratio_name, pval_name, ratio_idx, pval_idx, data_def,
                                           debug_log = function(message, level = 1) {}) {

  similarity <- 0

  if ("Numerator" %in% names(data_def) && "Denominator" %in% names(data_def)) {
    ratio_num <- as.character(data_def$Numerator[ratio_idx])
    ratio_den <- as.character(data_def$Denominator[ratio_idx])
    pval_num <- as.character(data_def$Numerator[pval_idx])
    pval_den <- as.character(data_def$Denominator[pval_idx])

    ratio_num <- if (is.na(ratio_num) || ratio_num == "") "unknown" else trimws(ratio_num)
    ratio_den <- if (is.na(ratio_den) || ratio_den == "") "unknown" else trimws(ratio_den)
    pval_num <- if (is.na(pval_num) || pval_num == "") "unknown" else trimws(pval_num)
    pval_den <- if (is.na(pval_den) || pval_den == "") "unknown" else trimws(pval_den)

    if (ratio_num == pval_num && ratio_den == pval_den) {
      similarity <- similarity + 0.7
      if (isTRUE(getOption("miraprot.go.pairing_diagnostics", FALSE))) {
        debug_log("Numerator/denominator match bonus: +0.7", 2)
      }
    }
  }

  name_sim <- calculate_go_name_similarity(ratio_name, pval_name)
  similarity <- similarity + (name_sim * 0.3)

  if (isTRUE(getOption("miraprot.go.pairing_diagnostics", FALSE))) {
    debug_log(paste("Name similarity:", round(name_sim, 3), "weighted:", round(name_sim * 0.3, 3)), 2)
  }

  return(min(1.0, similarity))
}

#' Calculate Name-Based Similarity for GO Columns
#'
#' Calculates similarity based on column naming patterns
#' @param ratio_name ratio column name
#' @param pval_name p-value column name
#' @return similarity score (0-1)
calculate_go_name_similarity <- function(ratio_name, pval_name) {

  norm_ratio <- tolower(trimws(ratio_name))
  norm_pval <- tolower(trimws(pval_name))

  ratio_parts <- extract_go_name_parts(norm_ratio)
  pval_parts <- extract_go_name_parts(norm_pval)

  prefix_sim <- calculate_go_string_similarity(ratio_parts$prefix, pval_parts$prefix)
  core_sim <- calculate_go_string_similarity(ratio_parts$core, pval_parts$core)
  suffix_sim <- calculate_go_string_similarity(ratio_parts$suffix, pval_parts$suffix)

  total_sim <- (prefix_sim * 0.4) + (core_sim * 0.4) + (suffix_sim * 0.2)

  keyword_bonus <- detect_go_related_keywords(norm_ratio, norm_pval)
  total_sim <- total_sim + keyword_bonus

  return(min(1.0, total_sim))
}

#' Extract Name Parts for GO Column Analysis
#'
#' Extracts prefix, core, and suffix parts from column names
#' @param name column name (normalized)
#' @return list with prefix, core, suffix components
extract_go_name_parts <- function(name) {
  parts <- unlist(strsplit(name, "[_\\-\\s\\.\\(\\)\\[\\]:]+"))
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) {
    return(list(prefix = "", core = name, suffix = ""))
  }

  if (length(parts) == 1) {
    return(list(prefix = "", core = parts[1], suffix = ""))
  }

  if (length(parts) == 2) {
    return(list(prefix = parts[1], core = parts[2], suffix = ""))
  }

  prefix <- parts[1]
  suffix <- parts[length(parts)]
  core <- paste(parts[2:(length(parts)-1)], collapse = "_")

  return(list(prefix = prefix, core = core, suffix = suffix))
}

#' Calculate String Similarity for GO Pairing
#'
#' Simple string similarity calculation
#' @param str1 first string
#' @param str2 second string
#' @return similarity score (0-1)
calculate_go_string_similarity <- function(str1, str2) {
  if (is.null(str1) || is.null(str2) || !nzchar(str1) || !nzchar(str2)) {
    return(0)
  }

  str1 <- tolower(trimws(str1))
  str2 <- tolower(trimws(str2))

  if (str1 == str2) return(1.0)

  max_len <- max(nchar(str1), nchar(str2))
  if (max_len == 0) return(0)

  distance <- adist(str1, str2)[1,1]
  similarity <- 1 - (distance / max_len)

  return(max(0, similarity))
}

#' Detect Related Keywords for GO Pairing
#'
#' Detects related statistical keywords in column names
#' @param ratio_name ratio column name (normalized)
#' @param pval_name p-value column name (normalized)
#' @return keyword bonus score
detect_go_related_keywords <- function(ratio_name, pval_name) {

  ratio_keywords <- c("ratio", "fold", "fc", "abundance", "r")
  pval_keywords <- c("pval", "pvalue", "p-val", "p-value", "p", "sig", "significance")
  adjpval_keywords <- c("adjp", "adj-p", "adjpval", "adj-pval", "adjusted", "fdr", "q", "qval")

  contains_ratio <- any(sapply(ratio_keywords, function(kw) grepl(kw, ratio_name, fixed = TRUE)))
  contains_pval <- any(sapply(pval_keywords, function(kw) grepl(kw, pval_name, fixed = TRUE)))
  contains_adjpval <- any(sapply(adjpval_keywords, function(kw) grepl(kw, pval_name, fixed = TRUE)))

  if (contains_ratio && (contains_pval || contains_adjpval)) {
    return(0.2)
  }

  return(0.0)
}

# ==============================================================================
# 4. GO Analysis Conversion Helpers
# ==============================================================================

#' Convert Ontology Input
#'
#' Convert UI ontology selection to clusterProfiler format
#' @param ont_input ontology input from UI
#' @return ontology code for clusterProfiler
convert_ontology_input <- function(ont_input) {
  switch(ont_input,
         "Molecular Function" = "MF",
         "Cellular Component" = "CC",
         "Biological Process" = "BP",
         "All" = "ALL",
         "BP" # Default
  )
}

#' Convert P-adjust Method
#'
#' Convert UI p-adjustment method to clusterProfiler format
#' @param method_input method input from UI
#' @return method code for clusterProfiler
convert_padjust_method <- function(method_input) {
  switch(method_input,
         "Holm" = "holm",
         "Hommel" = "hommel",
         "Benjamini & Hochberg" = "BH",
         "Benjamini & Yekutieli" = "BY",
         "Bonferroni" = "bonferroni",
         "FDR" = "fdr",
         "None" = "none",
         "BH" # Default
  )
}


#' Clean GO Identifier Vector
#'
#' Removes missing/blank identifiers and returns unique values while preserving
#' first occurrence order. Identifier case is intentionally preserved because
#' AnnotationDbi key types can be case-sensitive.
#' @param identifiers vector of identifiers
#' @return cleaned character vector
clean_go_identifiers <- function(identifiers) {
  if (is.null(identifiers) || length(identifiers) == 0) {
    return(character(0))
  }

  identifiers <- as.character(identifiers)
  identifiers <- trimws(identifiers)
  identifiers <- identifiers[!is.na(identifiers) & nzchar(identifiers)]
  unique(identifiers)
}

#' Parse GO Custom Universe Text
#'
#' Parses user-provided background identifiers from the GO universe text area.
#' Supported separators are newlines, commas, semicolons, and tabs. Whitespace at
#' identifier boundaries is trimmed, but internal spaces are preserved.
#' @param universe_text text area input
#' @return cleaned character vector
parse_go_custom_universe <- function(universe_text) {
  if (is.null(universe_text) || length(universe_text) == 0) {
    return(character(0))
  }

  universe_text <- paste(as.character(universe_text), collapse = "\n")
  if (!nzchar(trimws(universe_text))) {
    return(character(0))
  }

  universe_text <- gsub("\r\n?", "\n", universe_text)
  identifiers <- unlist(strsplit(universe_text, "[,;\t\n]+", perl = TRUE), use.names = FALSE)
  identifiers <- trimws(identifiers)
  identifiers <- gsub("^(['\"])(.*)\\1$", "\\2", identifiers)
  clean_go_identifiers(identifiers)
}

#' Prepare GO Gene and Universe Inputs
#'
#' Builds a consistent enrichment input list from the filtered GO genes, the
#' dataset identifiers, and optional custom universe text. If a custom universe is
#' provided, input genes outside that universe are removed instead of silently
#' expanding the user-defined background.
#' @param selected_genes filtered genes to test for enrichment
#' @param dataset_universe identifiers from the selected identifier column
#' @param custom_universe_text optional text area input with background genes
#' @param debug_log logging function
#' @return list with genes, universe, custom_universe_used, removed_genes,
#'   universe_not_in_dataset, error, and message
prepare_go_enrichment_inputs <- function(selected_genes, dataset_universe,
                                         custom_universe_text = NULL,
                                         debug_log = function(message, level = 1) {}) {
  selected_genes <- clean_go_identifiers(selected_genes)
  dataset_universe <- clean_go_identifiers(dataset_universe)
  custom_universe <- parse_go_custom_universe(custom_universe_text)
  custom_universe_used <- length(custom_universe) > 0

  universe <- if (custom_universe_used) custom_universe else dataset_universe

  if (length(selected_genes) == 0) {
    return(list(
      genes = character(0), universe = universe,
      custom_universe_used = custom_universe_used,
      removed_genes = character(0), universe_not_in_dataset = character(0),
      error = TRUE, message = "No valid gene identifiers are available for GO enrichment."
    ))
  }

  if (length(universe) == 0) {
    return(list(
      genes = character(0), universe = character(0),
      custom_universe_used = custom_universe_used,
      removed_genes = selected_genes, universe_not_in_dataset = character(0),
      error = TRUE, message = "The GO universe is empty after removing blank identifiers."
    ))
  }

  removed_genes <- setdiff(selected_genes, universe)
  genes <- selected_genes[selected_genes %in% universe]
  universe_not_in_dataset <- if (custom_universe_used) setdiff(universe, dataset_universe) else character(0)

  debug_log(paste(
    "Prepared GO enrichment inputs - genes:", length(genes),
    "universe:", length(universe),
    "custom universe:", custom_universe_used,
    "removed genes:", length(removed_genes)
  ), 1)

  if (length(genes) == 0) {
    return(list(
      genes = genes, universe = universe,
      custom_universe_used = custom_universe_used,
      removed_genes = removed_genes, universe_not_in_dataset = universe_not_in_dataset,
      error = TRUE,
      message = "None of the filtered genes are present in the selected GO universe. Please check the custom universe and identifier key type."
    ))
  }

  list(
    genes = genes,
    universe = universe,
    custom_universe_used = custom_universe_used,
    removed_genes = removed_genes,
    universe_not_in_dataset = universe_not_in_dataset,
    error = FALSE,
    message = NULL
  )
}

# ==============================================================================
# 5. GO Enrichment Execution
# ==============================================================================

#' Perform GO Enrichment Analysis with Enhanced Error Handling
#'
#' Direct approach with robust error handling
#' @param genes vector of gene identifiers
#' @param annotations organism annotation object
#' @param keyType key type for gene identifiers
#' @param ont ontology (BP, MF, CC, or All)
#' @param pAdjustMethod p-value adjustment method
#' @param pvalueCutoff p-value cutoff
#' @param qvalueCutoff q-value cutoff
#' @param minGSSize minimum gene set size
#' @param maxGSSize maximum gene set size
#' @param universe background universe of genes
#' @param debug_log logging function
#' @return enrichResult object or NULL
perform_go_enrichment <- function(genes, annotations, keyType, ont, pAdjustMethod,
                                  pvalueCutoff, qvalueCutoff, minGSSize, maxGSSize,
                                  universe, debug_log = function(message, level = 1) {}) {

  tryCatch({
    debug_log(paste("Starting enrichGO with", length(genes), "genes"), 1)
    debug_log(paste("Universe size:", length(universe)), 2)
    debug_log(paste("keyType:", keyType), 2)

    genes <- as.character(genes)
    genes <- genes[!is.na(genes) & genes != "" & nzchar(genes)]
    genes <- unique(genes)

    universe <- as.character(universe)
    universe <- universe[!is.na(universe) & universe != "" & nzchar(universe)]
    universe <- unique(universe)

    debug_log(paste("Clean genes count:", length(genes)), 1)

    enrichGO(
      gene = genes,
      OrgDb = annotations,
      keyType = keyType,
      ont = ont,
      pAdjustMethod = pAdjustMethod,
      pvalueCutoff = pvalueCutoff,
      qvalueCutoff = qvalueCutoff,
      minGSSize = minGSSize,
      maxGSSize = maxGSSize,
      universe = universe
    )
  }, error = function(e) {
    debug_log(paste("enrichGO failed:", e$message), 1)
    return(NULL)
  })
}

#' Create GO Results List
#'
#' Create results list from GO enrichment output and fold-change input data
#' @param edo enrichGO result object (used as-is)
#' @param go_data filtered GO input data
#' @param annotations organism annotation object
#' @param keyType key type for gene identifiers
#' @param debug_log logging function
#' @return list with results and processed data
create_go_results_list_direct <- function(edo, go_data, annotations = NULL, keyType = "SYMBOL",
                                          debug_log = function(message, level = 1) {}) {

  debug_log("Creating GO results list", 1)

  # go_data is already the filtered enrichment input and its Gene values remain
  # in the user-selected keyType namespace.  edo@gene is not a safe filter here:
  # enrichResult internals can use a different identifier namespace.  Preserve
  # input order so duplicate identifiers resolve deterministically to the first
  # filtered occurrence, without dropping measured (including NA) abundances.
  fc_source <- go_data
  fc_source$Gene <- trimws(as.character(fc_source$Gene))
  fc_source <- fc_source[!is.na(fc_source$Gene) & nzchar(fc_source$Gene), , drop = FALSE]
  fc_source_dedup <- fc_source[!duplicated(fc_source$Gene), , drop = FALSE]

  fc_vec <- setNames(fc_source_dedup$Abundance, fc_source_dedup$Gene)
  debug_log(paste("Created fold change vector directly from filtered GO input with", length(fc_vec), "unique gene entries"), 2)
  debug_log(paste("GO fold-change source rows:", nrow(fc_source), "unique genes:", length(unique(fc_source$Gene))), 1)

  edox <- if (!is.null(annotations)) {
    tryCatch({
      debug_log("Applying setReadable", 2)
      setReadable(edo, annotations, keyType)
    }, error = function(e) {
      debug_log(paste("setReadable failed:", e$message), 1)
      edo
    })
  } else {
    debug_log("No annotations provided, using original edo", 2)
    edo
  }

  edop <- tryCatch({
    debug_log("Applying pairwise_termsim", 2)
    pairwise_termsim(edo)
  }, error = function(e) {
    debug_log(paste("pairwise_termsim failed:", e$message), 1)
    edo
  })

  reduced_edo_for_plotting <- tryCatch({
    # This is a deliberate size-reduced plotting copy, not an error fallback.
    plotting_edo <- edo
    if (nrow(edo@result) > 20) {
      plotting_edo@result <- edo@result[order(edo@result$p.adjust), ][1:20, ]
      debug_log("Created reduced GO result object for plotting with top 20 terms", 2)
    }
    plotting_edo
  }, error = function(e) {
    debug_log(paste("Reduced plotting object creation failed:", e$message), 1)
    edo
  })

  results_list <- list(
    Edo_GO = edo,
    Edo_GO_safe = reduced_edo_for_plotting,
    Edox_GO = edox,
    Edop_GO = edop,
    go_data_FC = fc_vec,
    go_data = go_data
  )

  debug_log("GO results list created successfully", 1)

  # Level-0 logs: reproduction-grade record of GO enrichment run
  n_input_rows_l0 <- tryCatch(nrow(go_data),                  error = function(e) NA_integer_)
  n_input_l0      <- tryCatch(length(unique(go_data$Gene)),    error = function(e) NA_integer_)
  org_l0        <- tryCatch(as.character(edo@organism),      error = function(e) NA_character_)
  ont_l0        <- tryCatch(as.character(edo@ontology),      error = function(e) NA_character_)
  padj_l0       <- tryCatch(as.character(edo@pAdjustMethod), error = function(e) NA_character_)
  pval_cut_l0   <- tryCatch(as.character(edo@pvalueCutoff),  error = function(e) NA_character_)
  qval_cut_l0   <- tryCatch(as.character(edo@qvalueCutoff),  error = function(e) NA_character_)
  mingss_l0     <- tryCatch(as.character(safe_go_enrich_slot(edo, "minGSSize", default = NA)),     error = function(e) NA_character_)
  maxgss_l0     <- tryCatch(as.character(safe_go_enrich_slot(edo, "maxGSSize", default = NA)),     error = function(e) NA_character_)
  debug_log(
    sprintf(
      paste0(
        "GO analysis inputs",
        " | Organism: %s",
        " | Ontology: %s",
        " | Key type: %s",
        " | p-value cutoff: %s",
        " | q-value cutoff: %s",
        " | p-adjust method: %s",
        " | Min GS size: %s",
        " | Max GS size: %s",
        " | Input protein rows: %d",
        " | Unique input proteins: %d"
      ),
      org_l0, ont_l0, keyType,
      pval_cut_l0, qval_cut_l0, padj_l0,
      mingss_l0, maxgss_l0,
      n_input_rows_l0, n_input_l0
    ),
    level = 0
  )
  n_sig_l0      <- tryCatch(nrow(edo@result),     error = function(e) NA_integer_)
  n_enrich_l0   <- tryCatch(length(edo@gene),     error = function(e) NA_integer_)
  n_universe_l0 <- tryCatch(length(edo@universe), error = function(e) NA_integer_)
  debug_log(
    sprintf(
      paste0(
        "GO analysis outcome",
        " | Protein rows passed filter: %d",
        " | Unique proteins passed filter: %d",
        " | Proteins in enrichment: %d",
        " | Universe size: %d",
        " | Significant terms: %d"
      ),
      n_input_rows_l0, n_input_l0, n_enrich_l0, n_universe_l0, n_sig_l0
    ),
    level = 0
  )

  return(results_list)
}
