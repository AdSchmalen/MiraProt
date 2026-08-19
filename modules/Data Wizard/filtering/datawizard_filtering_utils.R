# ==============================================================================
# File: modules/Data Wizard/filtering/datawizard_filtering_utils.R
#
# Purpose:
#   Provides shared utility functions for the filtering module: safe type
#   coercion helpers, metadata availability checks, column type analysis,
#   input extraction, filter table validation, and a robust sprintf wrapper.
#   These are small, general-purpose helpers that do not contain domain-
#   specific filter logic.
#
# Architectural Role:
#   Utility layer. Consumed by the engine (datawizard_filtering_engine.R),
#   state factory, observers, UI rendering, and the orchestrator. Sourced
#   into modEnv via datawizard_filtering.R before all other filtering files.
#
# Structure:
#   1. Null-coalescing operator (%||%)
#   2. Safe type coercion helpers (logical, numeric, character, sprintf)
#   3. Safe input extraction with optional validator
#   4. Column type normalization and homogeneity check
#   5. Metadata availability check
#   6. Filter table integrity validation
#
# Dependencies:
#   - debug_log must exist in the calling environment.
#   - showNotification (Shiny) is used by validate_selected_columns_homogeneous
#     for user-facing error messages.
#
# Notes for future developers:
#   - validate_filter_table_integrity receives a reactiveValues object and
#     may mutate it (auto-repair of SOME rows). This is intentional.
#   - validate_selected_columns_homogeneous is the only function here that
#     calls showNotification. It receives session implicitly via Shiny context.
#   - Do not duplicate functions already in rlang or base R. The %||% operator
#     is defined here as a convenience for environments where rlang is not loaded.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Null-coalescing operator
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x


# ------------------------------------------------------------------------------
# 2. Safe type coercion helpers
# ------------------------------------------------------------------------------

#' Robust sprintf wrapper with fallback to paste.
safe_sprintf <- function(fmt, ...) {
  tryCatch({
    sprintf(fmt, ...)
  }, error = function(e) {
    debug_log(paste("sprintf error:", e$message), 1)
    # Fallback: simple text concatenation
    args <- list(...)
    if (length(args) > 0) {
      paste(fmt, ":", paste(args, collapse = ", "))
    } else {
      fmt
    }
  })
}

#' Safe logical check with fallback.
#' @param value Value to check.
#' @param default Fallback value if check fails (default FALSE).
safe_logical_check <- function(value, default = FALSE) {
  tryCatch({
    if (is.null(value) || length(value) == 0 || is.na(value) || !is.logical(value)) {
      return(default)
    }
    return(value[1])
  }, error = function(e) {
    return(default)
  })
}

#' Safe numeric validation with bounds checking.
#' @param value Value to validate.
#' @param min_val Minimum bound (default 1).
#' @param max_val Maximum bound (NULL = no upper bound).
#' @param default_val Fallback value (default 1).
safe_numeric_check <- function(value, min_val = 1, max_val = NULL, default_val = 1) {
  tryCatch({
    if (is.null(value) || is.na(value)) return(default_val)
    if (!is.numeric(value)) {
      value <- as.numeric(value)
      if (is.na(value)) return(default_val)
    }
    value <- as.integer(round(value))
    if (!is.null(max_val)) value <- min(value, max_val)
    value <- max(value, min_val)
    return(value)
  }, error = function(e) {
    return(default_val)
  })
}

#' Safe character check with fallback.
#' @param value Value to check.
#' @param default Fallback value if check fails (default "").
safe_character_check <- function(value, default = "") {
  tryCatch({
    if (is.null(value) || length(value) == 0 || is.na(value)) return(default)
    if (!is.character(value)) value <- as.character(value)
    return(value[1])
  }, error = function(e) {
    return(default)
  })
}


# ------------------------------------------------------------------------------
# 3. Safe input extraction with optional validator
# ------------------------------------------------------------------------------

#' Safely extract and validate a value with a fallback default.
#' Optionally applies a validator function; returns default if validation fails.
#'
#' @param input_value The value to extract.
#' @param default_value Fallback value if input is NULL or validation fails.
#' @param validator Optional function(x) -> logical. If it returns FALSE,
#'   default_value is used.
#' @return The validated input_value, or default_value.
safe_input_extract <- function(input_value, default_value, validator = NULL) {
  tryCatch({
    if (is.null(input_value)) return(default_value)
    if (!is.null(validator) && is.function(validator)) {
      if (!validator(input_value)) {
        debug_log(paste("Input validation failed, using default:", default_value), 2)
        return(default_value)
      }
    }
    return(input_value)
  }, error = function(e) {
    debug_log(paste("Error extracting input value:", e$message,
                    "- using default:", default_value), 1)
    return(default_value)
  })
}


# ------------------------------------------------------------------------------
# 4. Column type normalization and homogeneity check
# ------------------------------------------------------------------------------

#' Normalize an R vector's class to one of:
#' "numeric", "character", "datetime", "logical", "other".
#'
#' @param x An R vector or column.
#' @return A single character string with the normalized type.
normalize_col_type <- function(x) {
  if (is.null(x)) return("other")
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("datetime")
  if (is.factor(x) || is.character(x)) return("character")
  "other"
}

#' Compute a named vector of normalized types for a set of columns.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to check.
#' @return A named character vector (names = column names, values = types).
column_types_for <- function(df, cols) {
  if (is.null(df) || !is.data.frame(df) || length(cols) == 0) {
    return(setNames(character(0), character(0)))
  }
  cols_in_df <- intersect(cols, names(df))
  if (length(cols_in_df) == 0) return(setNames(character(0), character(0)))
  vapply(cols_in_df, function(nm) normalize_col_type(df[[nm]]), FUN.VALUE = character(1))
}

#' Validate that all selected columns share the same normalized type.
#' Shows a Shiny notification on mixed types and returns FALSE.
#'
#' @param session Shiny session (for showNotification context).
#' @param selected_cols Character vector of selected column names.
#' @param df The data frame containing the columns.
#' @return TRUE if homogeneous (or empty selection), FALSE on mixed types.
validate_selected_columns_homogeneous <- function(session, selected_cols, df) {
  if (is.null(selected_cols) || length(selected_cols) == 0) return(TRUE)
  if (is.null(df) || !is.data.frame(df)) {
    showNotification("No data is available to validate selected columns.",
                     type = "error", duration = 5)
    return(FALSE)
  }
  cols_in_df <- intersect(as.character(selected_cols), names(df))
  if (length(cols_in_df) == 0) {
    showNotification("The selected columns do not exist in the current dataset.",
                     type = "error", duration = 5)
    return(FALSE)
  }

  typs <- column_types_for(df, cols_in_df)
  unique_types <- unique(unname(typs))
  if (length(unique_types) <= 1) return(TRUE)

  showNotification("Cannot add filter: selected columns contain mixed data types.",
                   type = "error", duration = 6)
  FALSE
}


# ------------------------------------------------------------------------------
# 5. Metadata availability check
# ------------------------------------------------------------------------------

#' Check whether the metadata Content column has meaningful assignments.
#' @param metadata_def Metadata data frame (or reactive wrapping one).
#' @return TRUE if Content column has at least one non-empty value.
check_metadata_content_available <- function(metadata_def) {
  tryCatch({
    if (is.null(metadata_def)) return(FALSE)

    current_metadata <- if (is.reactive(metadata_def)) metadata_def() else metadata_def

    if (is.null(current_metadata) || !is.data.frame(current_metadata)) return(FALSE)
    if (nrow(current_metadata) == 0) return(FALSE)
    if (!all(c("Content", "Column") %in% names(current_metadata))) return(FALSE)

    content_values <- current_metadata$Content
    valid_content <- content_values[!is.na(content_values) & nzchar(trimws(content_values))]

    if (length(valid_content) == 0) {
      debug_log("Content column exists but contains no meaningful assignments", 2)
      return(FALSE)
    }

    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error checking metadata content availability:", e$message), 1)
    return(FALSE)
  })
}


# ------------------------------------------------------------------------------
# 6. Filter table integrity validation
# ------------------------------------------------------------------------------

#' Validate and auto-repair the custom conditions data frame.
#' Checks required columns and repairs incomplete SOME-logic rows.
#'
#' @param fs A reactiveValues object containing $custom_conditions.
#' @return TRUE if valid (possibly after repair), FALSE if structurally broken.
validate_filter_table_integrity <- function(fs) {
  tryCatch({
    current_conditions <- fs$custom_conditions

    if (nrow(current_conditions) == 0) {
      debug_log("Filter table validation: No filters to validate", 2)
      return(TRUE)
    }

    required_cols <- c("Column", "Operator_1", "Value_1", "Multi_Column_Logic")
    missing_cols <- setdiff(required_cols, names(current_conditions))

    if (length(missing_cols) > 0) {
      debug_log(paste("Filter table validation failed: Missing columns:",
                      paste(missing_cols, collapse = ", ")), 1)
      return(FALSE)
    }

    # Auto-repair SOME rows with missing settings
    some_rows <- which(current_conditions$Multi_Column_Logic == "SOME")
    if (length(some_rows) > 0) {
      for (row in some_rows) {
        if (is.na(current_conditions$Some_Operator[row]) ||
            is.na(current_conditions$Some_Count[row])) {
          debug_log(paste("Filter table validation: SOME row", row,
                          "has missing settings, auto-repairing"), 1)
          current_conditions$Some_Operator[row] <- "at_least"
          current_conditions$Some_Count[row] <- 1
        }
      }
      fs$custom_conditions <- current_conditions
    }

    debug_log("Filter table validation passed", 2)
    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error in filter table validation:", e$message), 1)
    return(FALSE)
  })
}
