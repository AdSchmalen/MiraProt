# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_merge.R
#
# Purpose:
#   Provides helper functions for the Identifier Merging feature of the
#   Annotation submodule.  These are pure-logic utilities with no Shiny
#   dependencies (no UI, no reactive state, no observers).
#
# Architectural Role:
#   Utility layer for the merge workflow.  Sourced into modEnv via the
#   orchestrator (datawizard_annotation.R) before the observer files.
#   Called by merge observers in datawizard_annotation_utils_observer.R.
#
# Functions:
#   get_identifier_columns()   - Extract identifier columns from metadata.
#   merge_identifiers()        - Row-wise merge of identifier columns.
#   build_merge_col_name()     - Deterministic column name for merge output.
#   validate_merge_inputs()    - Pre-merge input validation.
#
# Notes:
#   - All functions are pure (no side effects beyond logging).
#   - Designed for testability: no Shiny-specific objects required.
# ==============================================================================


#' Extract columns flagged as "Identifier" from metadata.
#'
#' Reads the metadata data frame and returns the column names whose
#' Content field is exactly "Identifier", preserving their original order.
#'
#' @param meta  Data frame with at least columns "Column" and "Content".
#' @param debug_log  Logging function with signature (message, level).
#' @return Character vector of identifier column names (possibly empty).
get_identifier_columns <- function(meta,
                                   debug_log = function(m, l = 1) {}) {
  if (is.null(meta) || !is.data.frame(meta)) {
    debug_log("get_identifier_columns: metadata is NULL or not a data frame", 1)
    return(character(0))
  }

  if (!all(c("Column", "Content") %in% names(meta))) {
    debug_log("get_identifier_columns: metadata missing 'Column' or 'Content' field", 1)
    return(character(0))
  }

  trimmed_content <- trimws(meta$Content)
  id_cols <- meta$Column[!is.na(trimmed_content) & trimmed_content == "Identifier"]
  id_cols <- id_cols[!is.na(id_cols) & nzchar(trimws(id_cols))]

  debug_log(sprintf("get_identifier_columns: found %d identifier column(s): %s",
                     length(id_cols), paste(id_cols, collapse = ", ")), 2)
  id_cols
}


#' Validate inputs before performing a merge operation.
#'
#' Checks that the selected identifier columns exist in the data and that
#' at least one column is selected.
#'
#' @param data          Data frame to merge within.
#' @param id_columns    Character vector of selected identifier column names
#'                      (in merge-priority order).
#' @param merge_mode    One of "first_non_empty" or "concatenate_all".
#' @param debug_log     Logging function.
#' @return A list with:
#'   \item{valid}{Logical.  TRUE if inputs are acceptable.}
#'   \item{error}{Character message if not valid, NULL otherwise.}
#'   \item{missing_cols}{Character vector of columns not found in data.}
validate_merge_inputs <- function(data, id_columns, merge_mode,
                                  debug_log = function(m, l = 1) {}) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(list(valid = FALSE, error = "No data available for merging.",
                missing_cols = character(0)))
  }

  if (is.null(id_columns) || length(id_columns) == 0) {
    return(list(valid = FALSE,
                error = "No identifier columns selected. Please add at least one column to the merge list.",
                missing_cols = character(0)))
  }

  valid_modes <- c("first_non_empty", "concatenate_all")
  if (is.null(merge_mode) || !merge_mode %in% valid_modes) {
    return(list(valid = FALSE,
                error = sprintf("Invalid merge mode '%s'. Must be one of: %s.",
                                as.character(merge_mode),
                                paste(valid_modes, collapse = ", ")),
                missing_cols = character(0)))
  }

  missing <- setdiff(id_columns, names(data))
  if (length(missing) > 0) {
    debug_log(sprintf("validate_merge_inputs: %d column(s) not found in data: %s",
                       length(missing), paste(missing, collapse = ", ")), 1)
    return(list(valid = FALSE,
                error = sprintf("The following identifier column(s) are not in the data: %s.",
                                paste(missing, collapse = ", ")),
                missing_cols = missing))
  }

  list(valid = TRUE, error = NULL, missing_cols = character(0))
}


#' Perform row-wise identifier merging.
#'
#' Iterates over each row and merges selected identifier columns according
#' to the chosen mode, respecting the column order provided.
#'
#' @param data        Data frame containing the identifier columns.
#' @param id_columns  Character vector of column names in merge-priority order.
#' @param merge_mode  One of:
#'   \describe{
#'     \item{"first_non_empty"}{Keep the first non-empty/non-NA value per row.}
#'     \item{"concatenate_all"}{Comma-join all non-empty/non-NA values per row.}
#'   }
#' @param debug_log   Logging function.
#' @return A list with:
#'   \item{values}{Character vector of length \code{nrow(data)}.}
#'   \item{n_merged}{Integer count of rows with a non-empty result.}
#'   \item{n_empty}{Integer count of rows with no result (all columns empty/NA).}
merge_identifiers <- function(data, id_columns, merge_mode,
                              debug_log = function(m, l = 1) {}) {

  n <- nrow(data)
  result <- character(n)

  # Pre-extract columns as character vectors for performance
  col_values <- lapply(id_columns, function(col) {
    vals <- as.character(data[[col]])
    vals[is.na(vals)] <- ""
    trimws(vals)
  })

  if (identical(merge_mode, "first_non_empty")) {
    debug_log(sprintf("merge_identifiers: mode=first_non_empty, %d column(s), %d row(s)",
                       length(id_columns), n), 1)
    for (i in seq_len(n)) {
      val <- ""
      for (j in seq_along(col_values)) {
        candidate <- col_values[[j]][i]
        if (nzchar(candidate)) {
          val <- candidate
          break
        }
      }
      result[i] <- val
    }

  } else if (identical(merge_mode, "concatenate_all")) {
    debug_log(sprintf("merge_identifiers: mode=concatenate_all, %d column(s), %d row(s)",
                       length(id_columns), n), 1)
    for (i in seq_len(n)) {
      parts <- character(0)
      for (j in seq_along(col_values)) {
        candidate <- col_values[[j]][i]
        if (nzchar(candidate)) {
          parts <- c(parts, candidate)
        }
      }
      result[i] <- paste(parts, collapse = ",")
    }
  }

  # Replace empty strings with NA for consistency with annotation output
  result[!nzchar(result)] <- NA_character_

  n_merged <- sum(!is.na(result))
  n_empty  <- n - n_merged

  debug_log(sprintf("merge_identifiers: %d/%d rows produced a value, %d empty",
                     n_merged, n, n_empty), 1)

  list(values = result, n_merged = n_merged, n_empty = n_empty)
}


#' Build a deterministic column name for the merged identifier output.
#'
#' @param id_columns  Character vector of column names used in the merge.
#' @param merge_mode  Merge mode string ("first_non_empty" or "concatenate_all").
#' @return A character string, e.g. "Merged_ID_first_3cols".
build_merge_col_name <- function(id_columns, merge_mode) {
  mode_tag <- if (identical(merge_mode, "first_non_empty")) "first" else "concat"
  n_cols   <- length(id_columns)
  paste0("Merged_ID_", mode_tag, "_", n_cols, "cols")
}


#' Build a publication-ready methods-section text for an identifier merge.
#'
#' Produces a concise paragraph suitable for copy-pasting into the methods
#' section of a scientific manuscript.  Handles edge cases:
#'   - 0 columns (should not happen after validation, but returns empty string)
#'   - 1 column (describes simple extraction rather than a merge)
#'   - 2+ columns (describes the merge with priority order)
#'
#' @param id_columns    Character vector of column names in merge-priority order.
#' @param merge_mode    One of "first_non_empty" or "concatenate_all".
#' @param new_col_name  Name of the output column that was added to the data.
#' @param n_total       Total number of rows in the data.
#' @param n_merged      Number of rows where a non-empty value was produced.
#' @param n_empty       Number of rows with no result (all sources empty/NA).
#' @return A single character string containing the methods text.
build_merge_methods_text <- function(id_columns, merge_mode, new_col_name,
                                     n_total, n_merged, n_empty) {

  n_cols <- length(id_columns)

  # Edge case: no columns (defensive; should not reach here after validation)
  if (n_cols == 0L) return("")

  # Format the column list with quoting
  quoted <- paste0("\"", id_columns, "\"")

  if (n_cols == 1L) {
    col_list_text <- quoted
  } else if (n_cols == 2L) {
    col_list_text <- paste(quoted, collapse = " and ")
  } else {
    col_list_text <- paste0(
      paste(quoted[-n_cols], collapse = ", "),
      ", and ", quoted[n_cols]
    )
  }

  # Build mode-specific description
  if (identical(merge_mode, "first_non_empty")) {
    if (n_cols == 1L) {
      mode_desc <- paste0(
        "Protein identifiers were extracted from the column ", col_list_text,
        ". Non-empty, non-missing values were retained; rows with empty or ",
        "missing entries were set to NA."
      )
    } else {
      mode_desc <- paste0(
        "Protein identifiers from multiple identifier columns were merged ",
        "into a single column, retaining one identifier per row. ",
        "For each row, the first non-empty, non-missing value was selected ",
        "with decreasing priority from the following columns: ",
        col_list_text, "."
      )
    }
  } else {
    # concatenate_all
    if (n_cols == 1L) {
      mode_desc <- paste0(
        "Protein identifiers were extracted from the column ", col_list_text,
        ". Non-empty, non-missing values were retained; rows with empty or ",
        "missing entries were set to NA."
      )
    } else {
      mode_desc <- paste0(
        "Protein identifiers from multiple identifier columns were merged ",
        "into a single column. For each row, all non-empty, non-missing ",
        "values were concatenated (comma-separated) in the following column ",
        "order: ", col_list_text, "."
      )
    }
  }

  # Append result summary
  pct_merged <- if (n_total > 0) round(n_merged / n_total * 100, 1) else 0
  result_desc <- sprintf(
    " The resulting column \"%s\" contains identifiers for %d of %d rows (%.1f%%); %d %s without a valid identifier.",
    new_col_name, n_merged, n_total, pct_merged,
    n_empty,
    if (n_empty == 1L) "row remained" else "rows remained"
  )

  paste0(mode_desc, result_desc)
}
