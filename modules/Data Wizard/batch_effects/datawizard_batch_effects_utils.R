# ============================================================================
# File: modules/Data Wizard/batch_effects/datawizard_batch_effects_utils.R
#
# What this file does:
#   Contains pure/near-pure utility functions used by the Batch Effects module:
#   data access wrappers, validators, and data-merge helpers. None of these
#   functions reference Shiny input/output/session or reactive state directly;
#   all dependencies are passed as arguments.
#
# How it fits into the module architecture:
#   datawizard_batch_effects.R (orchestrator)
#     -> sources this file into modEnv
#     -> the orchestrator, handlers file, and correction file all call
#        these utility functions
#
# File structure:
#   1. Data access wrappers
#      - make_get_file_data()  -- factory returning a safe getter closure
#      - make_set_file_data()  -- factory returning a safe setter closure
#   2. Validation functions
#      - validate_batch_data_structure()  -- checks data frame shape
#      - validate_batch_numeric_columns() -- checks column types
#      - validate_batch_groups()          -- checks group configuration
#   3. Data helpers
#      - identify_batch_complete_cases()  -- finds rows without NAs
#      - merge_batch_corrected_data()     -- merges corrected columns back
#
# What future developers need to know:
#   - Every function receives a debug_log function as its last argument
#     (except the factories which capture it in the closure).
#   - Validation functions return list(valid, message[, extra fields]).
#   - Data helpers are designed to be safe: they return sensible defaults
#     on NULL/empty input and never throw unhandled errors.
#   - If you add a new utility, keep it free of Shiny reactive dependencies.
# ============================================================================

# ========================================
# Data Access Wrappers
# ========================================

#' Create a safe data-getter closure.
#'
#' @param get_data  The raw getter function (or NULL) passed into the module.
#' @param debug_log Logging function.
#' @return A function() that returns a data.frame or NULL.
make_get_file_data <- function(get_data, debug_log) {
  function() {
    tryCatch({
      if (!is.null(get_data) && is.function(get_data)) {
        data <- get_data()
        if (!is.null(data)) {
          debug_log(paste("Retrieved data with dimensions:", nrow(data), "x", ncol(data)), 2)
        }
        return(data)
      }
      debug_log("No get_data function available", 1)
      return(NULL)
    }, error = function(e) {
      debug_log(paste("Error getting file data:", e$message), 1)
      return(NULL)
    })
  }
}

#' Create a safe data-setter closure.
#'
#' @param set_data  The raw setter function (or NULL) passed into the module.
#' @param debug_log Logging function.
#' @return A function(new_data) that returns logical success.
make_set_file_data <- function(set_data, debug_log) {
  function(new_data) {
    tryCatch({
      if (!is.null(set_data) && is.function(set_data)) {
        success <- set_data(new_data)
        if (success && !is.null(new_data)) {
          debug_log(paste("Successfully updated data with dimensions:", nrow(new_data), "x", ncol(new_data)), 2)
        } else if (!success) {
          debug_log("Failed to update data via set_data function", 1)
        }
        return(success)
      }
      debug_log("No set_data function available", 1)
      return(FALSE)
    }, error = function(e) {
      debug_log(paste("Error setting file data:", e$message), 1)
      return(FALSE)
    })
  }
}

# ========================================
# Validation Functions
# ========================================

#' Validate basic data frame structure for batch correction.
#'
#' @param df        A data.frame (or NULL).
#' @param debug_log Logging function.
#' @return list(valid = logical, message = character)
validate_batch_data_structure <- function(df, debug_log) {
  tryCatch({
    if (is.null(df) || nrow(df) == 0) {
      return(list(valid = FALSE, message = "No data available for batch correction."))
    }
    if (ncol(df) < 2) {
      return(list(valid = FALSE, message = "Data must have at least 2 columns for batch correction."))
    }

    debug_log(paste("Data structure validation passed:", nrow(df), "rows,", ncol(df), "columns"), 2)
    return(list(valid = TRUE, message = ""))

  }, error = function(e) {
    error_msg <- paste("Error validating data structure:", e$message)
    debug_log(error_msg, 1)
    return(list(valid = FALSE, message = error_msg))
  })
}

#' Validate that selected columns exist and are numeric (or convertible).
#'
#' @param df            A data.frame.
#' @param selected_cols Character vector of column names to check.
#' @param debug_log     Logging function.
#' @return list(valid = logical, message = character)
validate_batch_numeric_columns <- function(df, selected_cols, debug_log) {
  tryCatch({
    if (length(selected_cols) == 0) {
      return(list(valid = FALSE, message = "Please select at least one column for batch correction."))
    }

    # Check if columns exist
    missing_cols <- setdiff(selected_cols, names(df))
    if (length(missing_cols) > 0) {
      debug_log(paste("Missing columns detected:", paste(missing_cols, collapse = ", ")), 1)
      return(list(valid = FALSE, message = paste("Column(s) not found:", paste(missing_cols, collapse = ", "))))
    }

    # Check if columns are numeric or can be converted
    non_numeric <- character()
    conversion_warnings <- character()

    for (col in selected_cols) {
      if (!is.numeric(df[[col]])) {
        test_convert <- suppressWarnings(as.numeric(as.character(df[[col]])))
        if (all(is.na(test_convert))) {
          non_numeric <- c(non_numeric, col)
        } else {
          conversion_warnings <- c(conversion_warnings, col)
        }
      }
    }

    if (length(non_numeric) > 0) {
      debug_log(paste("Non-numeric columns detected:", paste(non_numeric, collapse = ", ")), 1)
      return(list(valid = FALSE,
                  message = paste("Column(s) contain non-numeric data that cannot be converted:",
                                  paste(non_numeric, collapse = ", "))))
    }

    if (length(conversion_warnings) > 0) {
      debug_log(paste("Columns requiring conversion:", paste(conversion_warnings, collapse = ", ")), 2)
    }

    debug_log(paste("Numeric column validation passed for", length(selected_cols), "columns"), 2)
    return(list(valid = TRUE, message = ""))

  }, error = function(e) {
    error_msg <- paste("Error validating numeric columns:", e$message)
    debug_log(error_msg, 1)
    return(list(valid = FALSE, message = error_msg))
  })
}

#' Validate batch group configuration (non-empty, no overlap, method-specific minimums).
#'
#' @param batch_list List of character vectors (one per batch group).
#' @param method     Character string: the selected correction method.
#' @param debug_log  Logging function.
#' @return list(valid = logical, message = character, groups = list) where
#'         \code{groups} contains only non-empty groups (present when valid).
validate_batch_groups <- function(batch_list, method, debug_log) {
  tryCatch({
    # Remove empty groups
    non_empty_groups <- batch_list[lengths(batch_list) > 0]

    if (length(non_empty_groups) < 1) {
      return(list(valid = FALSE, message = "Please select columns for at least one batch group."))
    }

    if (method == "ComBat" && length(non_empty_groups) < 2) {
      return(list(valid = FALSE, message = "ComBat requires at least 2 different batch groups."))
    }

    if (method %in% c("Offset Correction", "Limma", "LOESS", "Quantile") &&
        length(non_empty_groups) < 2) {
      return(list(valid = FALSE, message = paste(method, "requires at least 2 different batch groups for meaningful correction.")))
    }

    # Check for overlapping columns between groups
    all_cols <- unlist(non_empty_groups)
    if (length(all_cols) != length(unique(all_cols))) {
      return(list(valid = FALSE, message = "Columns cannot be assigned to multiple batch groups."))
    }

    debug_log(paste("Batch group validation passed:", length(non_empty_groups), "groups with", length(all_cols), "total columns"), 2)
    return(list(valid = TRUE, message = "", groups = non_empty_groups))

  }, error = function(e) {
    error_msg <- paste("Error validating batch groups:", e$message)
    debug_log(error_msg, 1)
    return(list(valid = FALSE, message = error_msg))
  })
}

# ========================================
# Data Helpers
# ========================================

#' Identify rows with complete data (no NAs) in a numeric matrix/data.frame.
#'
#' @param data_matrix A data.frame or matrix.
#' @param debug_log   Logging function.
#' @return Logical vector of length nrow(data_matrix), or logical(0) on error.
identify_batch_complete_cases <- function(data_matrix, debug_log) {
  tryCatch({
    if (is.null(data_matrix) || nrow(data_matrix) == 0) {
      debug_log("No data available for complete case identification", 2)
      return(logical(0))
    }

    complete_rows <- apply(data_matrix, 1, function(x) !any(is.na(x)))

    complete_count <- sum(complete_rows)
    total_count <- length(complete_rows)
    debug_log(paste("Complete cases identified:", complete_count, "out of", total_count, "total"), 2)

    return(complete_rows)

  }, error = function(e) {
    debug_log(paste("Error identifying complete cases:", e$message), 1)
    return(logical(0))
  })
}

#' Merge corrected data columns back into the original data frame.
#'
#' Creates new columns with a prefix (e.g. "Batch Corrected ") and fills them
#' only for rows indicated by complete_idx.
#'
#' @param original_data  The full original data.frame.
#' @param corrected_data A data.frame with corrected values (same column names as selected_cols).
#' @param selected_cols  Character vector of original column names that were corrected.
#' @param complete_idx   Logical vector indicating which rows were corrected.
#' @param prefix         Character prefix for new column names.
#' @param debug_log      Logging function.
#' @return The original data.frame augmented with prefixed corrected columns.
merge_batch_corrected_data <- function(original_data, corrected_data, selected_cols, complete_idx, prefix, debug_log) {
  tryCatch({
    if (is.null(original_data) || is.null(corrected_data)) {
      debug_log("Cannot merge corrected data: missing input data", 1)
      return(original_data)
    }

    result_data <- original_data

    if (nrow(corrected_data) == 0) {
      debug_log("No corrected data to merge", 2)
      return(result_data)
    }

    # Create new column names with prefix
    new_col_names <- paste0(prefix, selected_cols)
    debug_log(paste("Creating", length(new_col_names), "new corrected columns"), 2)

    # Initialize new columns with NAs
    for (new_col in new_col_names) {
      result_data[[new_col]] <- NA_real_
    }

    # Fill in corrected values only for complete cases
    if (sum(complete_idx) > 0) {
      for (i in seq_along(selected_cols)) {
        original_col <- selected_cols[i]
        new_col <- new_col_names[i]

        result_data[[new_col]][complete_idx] <- corrected_data[, original_col]
      }
      debug_log(paste("Merged corrected data for", sum(complete_idx), "complete cases"), 2)
    } else {
      debug_log("No complete cases to merge", 1)
    }

    return(result_data)

  }, error = function(e) {
    debug_log(paste("Error merging corrected data:", e$message), 1)
    return(original_data)
  })
}
