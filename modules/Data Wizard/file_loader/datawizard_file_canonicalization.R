# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_canonicalization.R
# Purpose:
#   Provide the file canonicalization portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard file canonicalization and metadata utilities.

# Exact technical-column policy. `Row_Index` was written by old session
# snapshots; no pattern matching is used because similarly named user columns
# are legitimate data.
DATAWIZARD_ROW_INDEX <- "Row Index"
DATAWIZARD_LEGACY_ROW_INDEX <- "Row_Index"

#' Canonicalize Data Wizard data column names and ensure Row Index.
#'
#' Converts missing column names to blanks, assigns stable `Unnamed_*` labels
#' to blanks, migrates the exact legacy `Row_Index` alias, makes names unique
#' with `_dup_`, and preserves or creates exactly one canonical `Row Index`.
#'
#' @param df A data.frame (or NULL).
#' @return A data.frame with canonical names and a single Row Index column.
#' @export
canonicalize_datawizard_column_names <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(data.frame("Row Index" = integer(0), check.names = FALSE))
  }

  df <- as.data.frame(df, check.names = FALSE)

  cn <- names(df)
  if (is.null(cn)) {
    cn <- rep("", ncol(df))
  }
  cn <- as.character(cn)
  cn[is.na(cn)] <- ""

  canonical <- which(cn == DATAWIZARD_ROW_INDEX)
  legacy <- which(cn == DATAWIZARD_LEGACY_ROW_INDEX)

  # Migrate only the documented legacy spelling.  When both spellings exist,
  # it is an alias only when its values equal the canonical column; otherwise
  # retain it as user data and let metadata expose the unresolved distinction.
  if (!length(canonical) && length(legacy) == 1L) {
    cn[legacy] <- DATAWIZARD_ROW_INDEX
    canonical <- legacy
    legacy <- integer()
  } else if (length(canonical) && length(legacy)) {
    equivalent <- vapply(legacy, function(i) {
      identical(as.character(df[[i]]), as.character(df[[canonical[[1L]]]]))
    }, logical(1))
    if (any(equivalent)) {
      keep <- rep(TRUE, ncol(df)); keep[legacy[equivalent]] <- FALSE
      df <- df[, keep, drop = FALSE]; cn <- cn[keep]
      canonical <- which(cn == DATAWIZARD_ROW_INDEX)
    }
  }

  # Duplicate exact canonical keys are invalid metadata identities. Preserve
  # their values under explicit unique names rather than silently deleting data.
  if (length(canonical) > 1L) {
    cn[canonical[-1L]] <- paste0(DATAWIZARD_ROW_INDEX, "_dup_", seq_len(length(canonical) - 1L))
    canonical <- canonical[[1L]]
  }

  empty_names <- !nzchar(cn)
  if (any(empty_names)) {
    cn[empty_names] <- paste0("Unnamed_", seq_len(sum(empty_names)))
  }

  cn <- make.unique(cn, sep = "_dup_")
  names(df) <- cn
  if (!length(canonical)) df[[DATAWIZARD_ROW_INDEX]] <- seq_len(nrow(df))

  data.frame(df, check.names = FALSE)
}

#' Clean data cells, canonicalize column names, then append a "Row Index" column.
#'
#' @param df A data.frame (or NULL).
#' @return A cleaned data.frame with canonical Data Wizard column names.
#' @export
clean_and_index <- function(df) {
  # Enhanced input validation
  if (is.null(df) || !is.data.frame(df)) {
    return(data.frame("Row Index" = integer(0), check.names = FALSE))
  }

  # Keep all input columns through cell cleanup. Column-name repair and any
  # incoming Row Index removal are centralized in
  # canonicalize_datawizard_column_names() below.

  # Trim trailing rows that are fully empty/blank (common with Excel used-range artifacts)
  # Fast path: compute last non-empty row column-wise (vectorized) to avoid expensive row-wise scans.
  if (nrow(df) > 0 && ncol(df) > 0) {
    last_non_empty <- 0L

    for (col in df) {
      non_empty <- if (is.list(col)) {
        vapply(col, function(cell) {
          if (is.null(cell) || length(cell) == 0L || all(is.na(cell))) return(FALSE)
          vals <- trimws(as.character(cell))
          any(!is.na(vals) & vals != "")
        }, logical(1), USE.NAMES = FALSE)
      } else if (is.character(col) || is.factor(col)) {
        vals <- trimws(as.character(col))
        !is.na(vals) & vals != ""
      } else {
        !is.na(col)
      }

      idx <- which(non_empty)
      if (length(idx) > 0L) {
        last_non_empty <- max(last_non_empty, idx[[length(idx)]])
      }
    }

    if (last_non_empty > 0L) {
      df <- df[seq_len(last_non_empty), , drop = FALSE]
    } else {
      df <- df[0, , drop = FALSE]
    }
  }

  df <- canonicalize_datawizard_column_names(df)

  # Normalize text-like cells early to keep Excel-safe payloads across pipeline.
  excel_cell_limit <- 32767L
  text_cols <- which(vapply(df, function(col) is.character(col) || is.factor(col) || is.list(col), logical(1)))
  if (length(text_cols) > 0L) {
    for (j in text_cols) {
      col <- df[[j]]
      x <- if (is.list(col)) {
        vapply(col, function(cell) {
          if (length(cell) == 0L || all(is.na(cell))) return(NA_character_)
          paste(as.character(cell), collapse = " | ")
        }, character(1), USE.NAMES = FALSE)
      } else {
        as.character(col)
      }
      x <- trimws(x)
      ok <- !is.na(x)
      too_long <- ok & nchar(x, type = "chars", allowNA = FALSE, keepNA = FALSE) > excel_cell_limit
      if (any(too_long)) {
        x[too_long] <- substr(x[too_long], 1L, excel_cell_limit)
      }
      df[[j]] <- x
    }
  }

  return(data.frame(df, check.names = FALSE))
}

#' Process data with optional header-row promotion.
#'
#' Applies the same clean_and_index() path used by direct file loads after
#' promoting the selected header row, so primary and secondary callers receive
#' identical canonical column names (including duplicate/blank repair) and a
#' single rebuilt Row Index column.
#'
#' @param raw_data Raw data.frame to process.
#' @param header_row_input 1-based header row selection.
#' @param operation_name Human-readable operation name for debug messages.
#' @param debug_log Optional function accepting message and level.
#' @return List with success/data or success/error.
#' @export
process_data_with_header_dw <- function(raw_data, header_row_input, operation_name = "data processing",
                                        debug_log = NULL) {
  tryCatch({
    if (is.null(raw_data) || !is.data.frame(raw_data) || nrow(raw_data) == 0L || ncol(raw_data) == 0L) {
      return(list(success = FALSE, error = "Invalid or empty input data"))
    }

    hr <- tryCatch({
      as.integer(header_row_input)
    }, error = function(e) {
      if (is.function(debug_log)) {
        debug_log(paste("Invalid header row input for", operation_name, ":", e$message), 2)
      }
      return(1L)
    })

    if (is.na(hr) || hr < 1L || hr > nrow(raw_data)) {
      hr <- 1L
    }

    if (hr == 1L) {
      df_trim <- raw_data
    } else {
      raw_names <- tryCatch({
        as.character(raw_data[hr - 1L, , drop = TRUE])
      }, error = function(e) {
        if (is.function(debug_log)) {
          debug_log(paste("Error extracting header names for", operation_name, ":", e$message), 2)
        }
        return(colnames(raw_data))
      })

      df_trim <- raw_data[-seq_len(hr - 1L), , drop = FALSE]
      colnames(df_trim) <- raw_names
    }

    processed_data <- clean_and_index(df_trim)
    if (is.function(debug_log)) {
      debug_log(paste(
        "Data processing completed for", operation_name, "-",
        nrow(processed_data), "rows x", ncol(processed_data), "columns"
      ), 2)
    }

    return(list(success = TRUE, data = processed_data))

  }, error = function(e) {
    error_msg <- paste("Error in data processing for", operation_name, ":", e$message)
    if (is.function(debug_log)) {
      debug_log(error_msg, 1)
    }
    return(list(success = FALSE, error = error_msg))
  })
}

#' Main file loader with enhanced features (backward compatible interface).
#'
#' @param file A Shiny file object.
#' @param sheet Optional sheet name for Excel files.
#' @param header Logical. Use first line as column names.
#' @return List with data and type (maintains original interface).
#' @export
load_file_dw <- function(file, sheet = NULL, header = TRUE) {
  # Use enhanced loading with recovery
  return(load_file_with_recovery_dw(file, sheet, header))
}

#' Build a skeleton metadata table suitable for rhandsontable.
#'
#' @param data A data.frame.
#' @param header_row Header row number (should be 1 for processed data).
#' @return A metadata skeleton data.frame.
#' @export
init_handson_table_dw <- function(data, header_row = 1) {
  # Enhanced input validation
  if (is.null(data) || !is.data.frame(data)) {
    stop("Data must be a non-NULL data.frame")
  }

  if (nrow(data) == 0) {
    warning("Input data is empty")
    return(data.frame(
      Column          = "Row Index",
      Content         = "Row Index",
      Options         = NA_character_,
      Numerator       = NA_character_,
      Denominator     = NA_character_,
      Transformation  = NA_character_,
      Sample          = NA_character_,
      stringsAsFactors = FALSE,
      check.names     = FALSE
    ))
  }

  # Validate header_row
  hr <- tryCatch({
    as.integer(header_row)
  }, error = function(e) {
    warning("Invalid header_row value, defaulting to 1")
    return(1L)
  })

  if (is.na(hr) || hr < 1 || hr > nrow(data)) {
    hr <- 1
  }

  # Collect column names
  if (hr == 1) {
    cols <- colnames(data)
  } else {
    raw_names <- tryCatch({
      as.character(data[hr - 1, , drop = TRUE])
    }, error = function(e) {
      warning("Error extracting header names from specified row, using column names")
      return(colnames(data))
    })

    header_frame <- as.data.frame(matrix(nrow = 0L, ncol = length(raw_names)),
                                  check.names = FALSE)
    colnames(header_frame) <- raw_names
    cols <- names(canonicalize_datawizard_column_names(header_frame))
  }

  # Build metadata skeleton
  df_handson <- tryCatch({
    data.frame(
      Column          = cols,
      Content         = NA_character_,
      Options         = NA_character_,
      Numerator       = NA_character_,
      Denominator     = NA_character_,
      Transformation  = NA_character_,
      Sample          = NA_character_,
      stringsAsFactors = FALSE,
      check.names     = FALSE
    )
  }, error = function(e) {
    stop("Error creating metadata skeleton: ", e$message)
  })

  # Ensure "Row Index" entry exists
  if ("Row Index" %in% colnames(data)) {
    idx <- which(df_handson$Column == "Row Index")
    if (length(idx) == 1) {
      df_handson$Content[idx] <- "Row Index"
    }
  } else {
    if (nrow(df_handson) > 0) {
      last <- nrow(df_handson)
      df_handson$Column[last]  <- "Row Index"
      df_handson$Content[last] <- "Row Index"
    }
  }

  return(df_handson)
}
