# ============================================================================
# File: modules/Data Wizard/pivot/datawizard_pivot_logic.R
# Purpose:
#   Provide reusable pure logic functions for the Data Wizard Pivot submodule.
#
# Architecture role:
#   This file contains only stateless, reusable functions that implement pivot
#   operations. These functions have no Shiny dependencies and receive all
#   required dependencies (including debug_log) as explicit parameters.
#   They are called from the orchestrator (datawizard_pivot.R) and observers.
#
# Structure:
#   - Utility helpers: safe_is_true, normalize_pivot_text_value
#   - Core pivot execution: pivot_execute_wider, pivot_execute_longer
#   - Preview computation: pivot_preview_wider, pivot_preview_longer
#
# Safe maintenance notes:
#   - Do not add Shiny dependencies (input, output, session, reactive, etc.).
#   - Do not add server functions here.
#   - Functions that need logging must accept debug_log as a parameter.
#   - Keep function signatures stable; the orchestrator depends on them.
# ============================================================================

# ============================================================================
# Utility helpers
# ============================================================================

# The reserved metadata column added by clean_and_index (file loader). All
# pivot operations strip this column before executing and regenerate it
# afterwards as a fresh sequential integer index.
PIVOT_ROW_INDEX_COL <- "Row Index"

# Boolean-safe check that handles NULL, empty, numeric, and character truthy values.
safe_is_true <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (is.logical(x)) return(isTRUE(x[1]))
  if (is.numeric(x)) return(x[1] > 0)
  if (is.character(x)) return(tolower(x[1]) %in% c("true", "t", "yes", "y", "1"))
  return(FALSE)
}

# Normalize an optional text input value to a non-empty string or a default.
normalize_pivot_text_value <- function(value, default_value) {
  if (is.null(value) || !nzchar(value)) {
    return(default_value)
  }
  value
}

# ============================================================================
# Core pivot execution functions
#
# These functions validate their inputs, execute the tidyr operation, and
# return a result data frame. They stop() with an informative message on
# failure. They have no Shiny side effects; callers are responsible for UI
# notifications and reactive state updates.
# ============================================================================

#' Execute a wider pivot on a data frame.
#'
#' @param data data.frame to pivot
#' @param names_from character column name to spread into new column headers
#' @param values_from character vector of column names containing values
#' @param id_cols character vector of id columns; auto-derived when NULL or empty
#' @param debug_log optional logging function accepting (message, level)
#' @return result data.frame
pivot_execute_wider <- function(data, names_from, values_from, id_cols,
                                debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(data) || !is.data.frame(data)) {
    stop("Input data is NULL or not a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data is empty (0 rows)")
  }

  # Strip "Row Index" from data and all column-role assignments before pivoting.
  # It is always regenerated as a fresh sequential index after the operation,
  # matching the convention used by clean_and_index in the file loader.
  row_index_col <- PIVOT_ROW_INDEX_COL
  input_nrow <- nrow(data)
  input_ncol <- ncol(data)
  if (row_index_col %in% names(data)) {
    data <- data[, names(data) != row_index_col, drop = FALSE]
    debug_log("Removed 'Row Index' column before wider pivot; it will be regenerated after", 2)
  }
  values_from <- values_from[values_from != row_index_col]
  if (!is.null(id_cols)) id_cols <- id_cols[id_cols != row_index_col]

  if (is.null(names_from) || !names_from %in% names(data)) {
    stop(paste("Names from column not found in data:", names_from))
  }
  if (is.null(values_from) || length(values_from) == 0) {
    stop("No 'Values from' column(s) selected")
  }

  missing_values_from <- values_from[!values_from %in% names(data)]
  if (length(missing_values_from) > 0) {
    stop(paste(
      "Values from column(s) not found in data:",
      paste(missing_values_from, collapse = ", ")
    ))
  }

  if (!is.null(id_cols) && length(id_cols) > 0) {
    missing_id_cols <- id_cols[!id_cols %in% names(data)]
    if (length(missing_id_cols) > 0) {
      stop(paste("ID columns not found in data:", paste(missing_id_cols, collapse = ", ")))
    }
  } else {
    id_cols <- setdiff(names(data), c(names_from, values_from))
    debug_log(paste("Auto-generated ID columns:", paste(id_cols, collapse = ", ")), 2)
  }

  if (length(id_cols) == 0) {
    stop("No valid ID columns available for pivot (all columns are used for names_from or values_from)")
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package required for pivot operations but not available")
  }

  debug_log(paste("Executing tidyr::pivot_wider with", length(id_cols), "ID columns"), 2)

  result <- tryCatch({
    tidyr::pivot_wider(
      data,
      id_cols = tidyr::all_of(id_cols),
      names_from = names_from,
      values_from = tidyr::all_of(values_from),
      names_sep = "_",
      names_vary = "fastest",
      values_fn = list
    )
  }, error = function(pivot_error) {
    raw_msg <- if (!is.null(pivot_error$message) && nzchar(pivot_error$message)) {
      pivot_error$message
    } else {
      "Unknown tidyr error"
    }
    stop(paste("tidyr::pivot_wider failed:", raw_msg))
  })

  if (is.null(result) || !is.data.frame(result)) {
    stop(paste("Pivot operation returned invalid result type:", class(result)[1]))
  }

  debug_log(paste("Wider pivot successful - result:", nrow(result), "x", ncol(result)), 2)

  # Add single-values_from prefix for consistent column naming.
  # For multiple values_from, tidyr creates columns like "<value>_<name>" using
  # names_sep. For a single values_from, tidyr uses only names_from values.
  # Adding the prefix makes naming consistent regardless of values_from count.
  if (length(values_from) == 1) {
    value_prefix <- paste0(values_from, "_")
    new_columns <- setdiff(names(result), id_cols)
    if (length(new_columns) > 0) {
      new_names <- names(result)
      for (col in new_columns) {
        col_idx <- which(names(result) == col)
        new_names[col_idx] <- paste0(value_prefix, col)
      }
      names(result) <- new_names
      debug_log("Added single-value prefix to wider pivot columns for consistent naming", 2)
    }
  }

  # Prepend a fresh sequential "Row Index" column to the result, matching the
  # convention used by clean_and_index in the file loader.
  result <- data.frame(
    `Row Index` = seq_len(nrow(result)),
    result,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  debug_log("Prepended fresh sequential 'Row Index' to wider pivot result", 2)

  id_cols_display <- if (!is.null(id_cols) && length(id_cols) > 0) {
    paste(id_cols, collapse = ", ")
  } else {
    "(none selected)"
  }

  debug_log(
    sprintf(
      paste0(
        "Pivot summary | Pivot type: wider",
        " | Names from: %s",
        " | Values from (%d): [%s]",
        " | ID columns (%d): [%s]",
        " | Input dimensions: %d x %d",
        " | Final dimensions: %d x %d"
      ),
      as.character(names_from),
      length(values_from),
      paste(values_from, collapse = ", "),
      length(id_cols),
      id_cols_display,
      input_nrow, input_ncol,
      nrow(result), ncol(result)
    ),
    level = 0
  )

  result
}

#' Execute a longer pivot on a data frame.
#'
#' When the selected columns contain mixed value types and tidyr raises a
#' "Can't combine" error, the function retries with values coerced to character
#' and sets the attribute "mixed_type_coercion" = TRUE on the result so that
#' callers can surface a user-facing warning notification.
#'
#' @param data data.frame to pivot
#' @param cols character vector of column names to pivot to rows
#' @param names_to character name for the new names column
#' @param values_to character name for the new values column
#' @param debug_log optional logging function accepting (message, level)
#' @return result data.frame (may carry attr "mixed_type_coercion" = TRUE)
pivot_execute_longer <- function(data, cols, names_to, values_to,
                                 debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(data) || !is.data.frame(data)) {
    stop("Input data is NULL or not a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data is empty (0 rows)")
  }

  # Strip "Row Index" from data and cols before pivoting.
  # It is always regenerated as a fresh sequential index after the operation.
  row_index_col <- PIVOT_ROW_INDEX_COL
  input_nrow <- nrow(data)
  input_ncol <- ncol(data)
  if (row_index_col %in% names(data)) {
    data <- data[, names(data) != row_index_col, drop = FALSE]
    debug_log("Removed 'Row Index' column before longer pivot; it will be regenerated after", 2)
  }
  cols <- cols[cols != row_index_col]

  if (is.null(cols) || length(cols) == 0) {
    stop("No columns specified for pivoting")
  }

  missing_cols <- cols[!cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
  }

  if (is.null(names_to) || !is.character(names_to) || !nzchar(names_to)) {
    names_to <- "name"
    debug_log("Using default 'name' for names_to parameter", 2)
  }
  if (is.null(values_to) || !is.character(values_to) || !nzchar(values_to)) {
    values_to <- "value"
    debug_log("Using default 'value' for values_to parameter", 2)
  }

  if (names_to %in% names(data) && !names_to %in% cols) {
    stop(paste0(
      "Column name conflict: 'names_to' parameter '",
      names_to, "' already exists in data"
    ))
  }
  if (values_to %in% names(data) && !values_to %in% cols) {
    stop(paste0(
      "Column name conflict: 'values_to' parameter '",
      values_to, "' already exists in data"
    ))
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package required for pivot operations but not available")
  }

  debug_log(paste("Executing tidyr::pivot_longer with", length(cols), "columns"), 2)

  result <- tryCatch({
    tidyr::pivot_longer(
      data,
      cols = tidyr::all_of(cols),
      names_to = names_to,
      values_to = values_to
    )
  }, error = function(pivot_error) {
    raw_msg <- if (!is.null(pivot_error$message) && nzchar(pivot_error$message)) {
      pivot_error$message
    } else {
      "Unknown tidyr error"
    }

    # Robust fallback for mixed column types (e.g. <double> with <list>).
    # Mark the result with an attribute so callers can surface a warning to the user.
    if (grepl("Can't combine", raw_msg, fixed = TRUE)) {
      coerced_transform <- stats::setNames(list(as.character), values_to)
      r <- tidyr::pivot_longer(
        data,
        cols = tidyr::all_of(cols),
        names_to = names_to,
        values_to = values_to,
        values_transform = coerced_transform
      )
      attr(r, "mixed_type_coercion") <- TRUE
      return(r)
    }

    stop(paste("tidyr::pivot_longer failed:", raw_msg))
  })

  if (is.null(result) || !is.data.frame(result)) {
    stop(paste("Pivot operation returned invalid result type:", class(result)[1]))
  }

  # Prepend a fresh sequential "Row Index" column to the result, preserving any
  # mixed_type_coercion flag set during the tryCatch above.
  mixed_coercion_flag <- isTRUE(attr(result, "mixed_type_coercion"))
  result <- data.frame(
    `Row Index` = seq_len(nrow(result)),
    result,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  if (mixed_coercion_flag) attr(result, "mixed_type_coercion") <- TRUE
  debug_log("Prepended fresh sequential 'Row Index' to longer pivot result", 2)

  # Log after tryCatch to avoid relying on parent-scope mutation inside the handler.
  if (isTRUE(attr(result, "mixed_type_coercion"))) {
    debug_log("Longer pivot: mixed types detected, pivoted values coerced to character", 1)
  }

  debug_log(paste("Longer pivot successful - result:", nrow(result), "x", ncol(result)), 2)

  debug_log(
    sprintf(
      paste0(
        "Pivot summary | Pivot type: longer",
        " | Columns to pivot (%d): [%s]",
        " | Names to: %s",
        " | Values to: %s",
        " | Mixed-type coercion: %s",
        " | Input dimensions: %d x %d",
        " | Final dimensions: %d x %d"
      ),
      length(cols),
      paste(cols, collapse = ", "),
      as.character(names_to),
      as.character(values_to),
      as.character(isTRUE(attr(result, "mixed_type_coercion"))),
      input_nrow, input_ncol,
      nrow(result), ncol(result)
    ),
    level = 0
  )

  result
}

# ============================================================================
# Preview computation functions
#
# These functions build a small preview of the pivot result for display.
# Return value on success: list(dim = c(rows, cols), head = data.frame,
#                               preview_data = data.frame)
# Return value on error:   list(error = "message string")
# They have no Shiny side effects.
# ============================================================================

#' Build a wider-pivot preview from a data sample.
#'
#' @param data data.frame
#' @param names_from character column name
#' @param values_from character vector of column names
#' @param id_cols character vector of id column names (may be NULL or empty)
#' @return named list with dim/head/preview_data or error
pivot_preview_wider <- function(data, names_from, values_from, id_cols) {
  tryCatch({
    # Strip "Row Index" before preview so estimated dimensions and the preview
    # table are consistent with what pivot_execute_wider produces.
    row_index_col <- PIVOT_ROW_INDEX_COL
    if (row_index_col %in% names(data)) {
      data <- data[, names(data) != row_index_col, drop = FALSE]
    }
    if (!is.null(id_cols)) id_cols <- id_cols[id_cols != row_index_col]

    id_cols_used <- if (!is.null(id_cols) && length(id_cols) > 0) {
      valid <- id_cols[id_cols %in% names(data)]
      if (length(valid) == 0) {
        setdiff(names(data), c(names_from, values_from))
      } else {
        valid
      }
    } else {
      setdiff(names(data), c(names_from, values_from))
    }

    if (length(id_cols_used) == 0) {
      return(list(error = "No valid ID columns for pivot"))
    }

    unique_names <- length(unique(data[[names_from]][!is.na(data[[names_from]])]))
    n_value_cols <- length(values_from)
    # +1 accounts for the "Row Index" column that pivot_execute_wider prepends.
    ncols_res <- length(id_cols_used) + (unique_names * n_value_cols) + 1L
    nrows_res <- nrow(unique(data[, id_cols_used, drop = FALSE]))

    subset_n <- min(200, nrow(data))
    subset_df <- data[seq_len(subset_n), , drop = FALSE]

    preview_data <- tidyr::pivot_wider(
      subset_df,
      id_cols = tidyr::all_of(id_cols_used),
      names_from = names_from,
      values_from = tidyr::all_of(values_from),
      names_sep = "_",
      names_vary = "fastest"
    )

    # Prepend Row Index to preview data to match the actual execute result.
    preview_data <- data.frame(
      `Row Index` = seq_len(nrow(preview_data)),
      preview_data,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    list(
      dim = c(nrows_res, ncols_res),
      head = utils::head(preview_data, 5),
      preview_data = preview_data
    )
  }, error = function(e) {
    list(error = paste("Error preparing wider preview:", e$message))
  })
}

#' Build a longer-pivot preview from a data sample.
#'
#' @param data data.frame
#' @param cols character vector of column names to pivot
#' @param names_to character name for the names column
#' @param values_to character name for the values column
#' @return named list with dim/head/preview_data or error
pivot_preview_longer <- function(data, cols, names_to, values_to) {
  tryCatch({
    # Strip "Row Index" before preview so estimated dimensions and the preview
    # table are consistent with what pivot_execute_longer produces.
    row_index_col <- PIVOT_ROW_INDEX_COL
    if (row_index_col %in% names(data)) {
      data <- data[, names(data) != row_index_col, drop = FALSE]
    }
    cols <- cols[cols != row_index_col]
    if (length(cols) == 0) {
      return(list(error = "No columns left to pivot after removing 'Row Index'"))
    }

    k <- length(cols)
    nrows_res <- nrow(data) * k
    # +1 accounts for the "Row Index" column that pivot_execute_longer prepends.
    ncols_res <- (ncol(data) - k) + 2 + 1L
    needed_src_rows <- max(1L, ceiling(5 / k))
    subset_n <- min(nrow(data), max(needed_src_rows, 10))
    subset_df <- data[seq_len(subset_n), , drop = FALSE]

    preview_data <- tidyr::pivot_longer(
      subset_df,
      cols = tidyr::all_of(cols),
      names_to = names_to,
      values_to = values_to
    )

    # Prepend Row Index to preview data to match the actual execute result.
    preview_data <- data.frame(
      `Row Index` = seq_len(nrow(preview_data)),
      preview_data,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    list(
      dim = c(nrows_res, ncols_res),
      head = utils::head(preview_data, 5),
      preview_data = preview_data
    )
  }, error = function(e) {
    list(error = paste("Error preparing longer preview:", e$message))
  })
}

# ============================================================================
# Transpose execution and preview
#
# Transpose swaps rows and columns. The column named "Row Index" (the
# metadata row identifier used throughout the application) is removed before
# transposition and its values are used as new column headers. A fresh
# sequential integer "Row Index" column (1, 2, 3, …) is then prepended to
# the transposed result, matching the convention used by pivot_wider and
# pivot_longer (and the initial clean_and_index step in the file loader).
# If no "Row Index" column is present in the input, sequential row numbers
# are used as the new column headers and a fresh sequential "Row Index" is
# still prepended to the result.
#
# Note: base::t() coerces a data.frame to a matrix before transposing, which
# promotes all values to the most general type. When the input contains both
# numeric and character columns, all output values become character. A
# "mixed_type_coercion" attribute is set on the result in this case.
# ============================================================================

#' Execute a transpose on a data frame.
#'
#' Removes the "Row Index" column (if present), transposes the remaining
#' columns, and prepends a new "Row Index" column with sequential integers
#' (1, 2, 3, …) — one per row of the transposed result. This matches the
#' Row Index convention used by pivot_wider, pivot_longer, and the file
#' loader's clean_and_index step.
#'
#' @param data data.frame to transpose
#' @param debug_log optional logging function accepting (message, level)
#' @return result data.frame (may carry attr "mixed_type_coercion" = TRUE)
pivot_execute_transpose <- function(data, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(data) || !is.data.frame(data)) {
    stop("Input data is NULL or not a data frame")
  }
  if (nrow(data) == 0) {
    stop("Input data is empty (0 rows)")
  }
  if (ncol(data) < 2) {
    stop("Data must have at least 2 columns to transpose")
  }

  row_index_col <- PIVOT_ROW_INDEX_COL
  has_row_index <- row_index_col %in% names(data)

  if (has_row_index) {
    new_col_names    <- as.character(data[[row_index_col]])
    data_to_transpose <- data[, names(data) != row_index_col, drop = FALSE]
    debug_log(paste("Transpose: 'Row Index' column found; using its values as new column names"), 2)
  } else {
    new_col_names    <- as.character(seq_len(nrow(data)))
    data_to_transpose <- data
    debug_log("Transpose: no 'Row Index' column found; using row numbers as new column names", 2)
  }

  # Reject list-columns: t() cannot sensibly handle them.
  list_cols <- names(data_to_transpose)[vapply(data_to_transpose, is.list, logical(1))]
  if (length(list_cols) > 0) {
    stop(paste(
      "Cannot transpose: the following columns contain list values:",
      paste(list_cols, collapse = ", ")
    ))
  }

  # Detect type coercion before transposing.
  col_types     <- vapply(data_to_transpose, function(x) class(x)[1], character(1))
  unique_types  <- unique(col_types)
  mixed_types   <- length(unique_types) > 1

  # t() on a data.frame coerces to matrix first.
  transposed <- as.data.frame(t(data_to_transpose), stringsAsFactors = FALSE)

  if (ncol(transposed) != length(new_col_names)) {
    stop(paste(
      "Transpose dimension mismatch: expected", length(new_col_names),
      "columns but got", ncol(transposed)
    ))
  }

  names(transposed) <- new_col_names

  if (has_row_index) {
    promoted_headers <- c(
      rownames(transposed)[1],
      as.character(unlist(transposed[1, , drop = TRUE], use.names = FALSE))
    )

    transposed_body <- transposed[-1, , drop = FALSE]

    result <- data.frame(
      `Row Index`      = seq_len(nrow(transposed_body)),
      `.header_source` = rownames(transposed_body),
      transposed_body,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
    names(result)    <- c("Row Index", promoted_headers)
    rownames(result) <- NULL
    debug_log("Transpose: preserved original header row and measurement headers", 2)
  } else {
    result <- data.frame(
      `Row Index` = seq_len(nrow(transposed)),
      transposed,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  }

  if (mixed_types) {
    attr(result, "mixed_type_coercion") <- TRUE
    debug_log(paste(
      "Transpose: mixed column types detected (",
      paste(unique_types, collapse = ", "),
      "); all values coerced to character"
    ), 1)
  }

  debug_log(
    sprintf(
      paste0(
        "Pivot summary | Pivot type: transpose",
        " | Row Index removed: %s",
        " | Mixed-type coercion: %s",
        " | Input dimensions: %d x %d",
        " | Final dimensions: %d x %d"
      ),
      as.character(has_row_index),
      as.character(isTRUE(attr(result, "mixed_type_coercion"))),
      nrow(data), ncol(data),
      nrow(result), ncol(result)
    ),
    level = 0
  )

  result
}

#' Build a transpose-pivot preview from a data sample.
#'
#' Computes the expected full-result dimensions from the entire dataset and
#' produces a display head by transposing only the first few rows (which
#' become the first few columns of the result). This avoids materialising a
#' result with potentially thousands of columns.
#'
#' @param data data.frame
#' @return named list with dim/head/preview_data or error
pivot_preview_transpose <- function(data) {
  tryCatch({
    row_index_col <- PIVOT_ROW_INDEX_COL
    has_row_index <- row_index_col %in% names(data)

    nrows_res <- if (has_row_index) max(ncol(data) - 2L, 0L) else ncol(data)
    ncols_res <- if (has_row_index) nrow(data) + 2L else nrow(data)

    # Build a preview by transposing a small row subset (becomes a small
    # column subset of the result, keeping the table compact).
    n_preview_rows <- min(5L, nrow(data))
    subset_df      <- data[seq_len(n_preview_rows), , drop = FALSE]
    preview_data   <- pivot_execute_transpose(subset_df)

    list(
      dim          = c(nrows_res, ncols_res),
      head         = utils::head(preview_data, 5),
      preview_data = preview_data
    )
  }, error = function(e) {
    list(error = paste("Error preparing transpose preview:", e$message))
  })
}

#' Build parameter list for a transpose apply request.
#'
#' @param current_data data.frame to transpose
#' @param debug_log optional logging function accepting (message, level)
#' @return named list with type/data or list(error = "message")
pivot_build_transpose_params <- function(current_data, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(current_data) || !is.data.frame(current_data) || nrow(current_data) == 0) {
    return(list(error = "No valid data available for transpose."))
  }
  if (ncol(current_data) < 2) {
    return(list(error = "Data must have at least 2 columns to transpose."))
  }

  list(
    type = "transpose",
    data = current_data
  )
}

# ============================================================================
# Safe UI system factory
#
# Creates the list of helper closures used by the orchestrator to interact
# with Shiny UI inputs safely. Returned as a named list; all closures capture
# `session` and `debug_log` explicitly, making this function testable and
# keeping the orchestrator free of 50+ lines of boilerplate.
# ============================================================================

#' Create the safe UI interaction system for the Pivot module.
#'
#' @param session Shiny session object
#' @param provided_system optional pre-built system supplied by the parent module; when NULL, the pivot module creates its own local system as the normal standalone path
#' @param debug_log logging function accepting (message, level)
#' @return named list with update_input_safely, show_notification_safely, execute_when_ready
pivot_create_ui_system <- function(session, provided_system = NULL, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (!is.null(provided_system)) {
    debug_log("Using provided safe UI system", 2)
    return(provided_system)
  }

  debug_log("Creating local safe UI system", 2)

  list(
    update_input_safely = function(input_id, value, input_type = "selectInput") {
      tryCatch({
        switch(input_type,
               "selectInput"   = updateSelectInput(session, input_id, selected = value),
               "textInput"     = updateTextInput(session, input_id, value = value),
               "numericInput"  = updateNumericInput(session, input_id, value = value),
               "checkboxInput" = updateCheckboxInput(session, input_id, value = value),
               updateSelectInput(session, input_id, selected = value)
        )
        debug_log(paste("Successfully updated", input_id), 2)
        return(TRUE)
      }, error = function(e) {
        debug_log(paste("Error updating", input_id, ":", e$message), 1)
        return(FALSE)
      })
    },

    show_notification_safely = function(message, type = "message", duration = 3) {
      tryCatch({
        showNotification(message, type = type, duration = duration)
      }, error = function(e) {
        debug_log(paste("Notification fallback:", message), 2)
      })
    },

    execute_when_ready = function(update_function, config) {
      in_reactive_context <- tryCatch({
        isolate(session$userData$test <- TRUE)
        TRUE
      }, error = function(e) FALSE)

      if (in_reactive_context) {
        debug_log("Using delayed UI updates (reactive context)", 2)
        session$onFlushed(function() {
          update_function(config)
        }, once = TRUE)
      } else {
        debug_log("Using immediate UI updates (non-reactive context)", 2)
        tryCatch({
          update_function(config)
        }, error = function(e) {
          debug_log(paste("Immediate update failed:", e$message), 1)
        })
      }
    }
  )
}

# ============================================================================
# Apply-request parameter builders
#
# These functions extract and validate user-supplied column selections from
# explicit parameter values and return either a parameter list ready for
# execution, or list(error = "human-readable message") when validation fails.
# They have no Shiny side effects; callers are responsible for notifications.
# ============================================================================

#' Build parameter list for a wider pivot apply request.
#'
#' @param current_data data.frame to pivot
#' @param names_from character column name (from input$wider_names_from)
#' @param values_from character vector of column names (from input$wider_values_from)
#' @param id_cols_input character vector or NULL (from input$wider_id_cols)
#' @param debug_log optional logging function accepting (message, level)
#' @return named list with type/data/names_from/... or list(error = "message")
pivot_build_wider_params <- function(current_data, names_from, values_from,
                                     id_cols_input, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(names_from) || is.null(values_from)) {
    return(list(error = "Please select columns for 'Names from' and 'Values from'."))
  }

  if (!names_from %in% names(current_data)) {
    return(list(error = "Selected 'Names from' column was not found in the current dataset."))
  }

  missing_vf <- values_from[!values_from %in% names(current_data)]
  if (length(missing_vf) > 0) {
    return(list(error = "One or more selected 'Values from' columns were not found in the current dataset."))
  }

  names_column <- current_data[[names_from]]
  unique_names <- length(unique(names_column[!is.na(names_column)]))

  id_cols <- if (!is.null(id_cols_input) && length(id_cols_input) > 0) {
    id_cols_input
  } else {
    setdiff(names(current_data), c(names_from, values_from))
  }

  n_value_cols   <- length(values_from)
  estimated_cols <- length(id_cols) + (unique_names * n_value_cols)

  debug_log(
    paste(
      "Wider pivot estimation - ID cols:", length(id_cols),
      "Unique names:", unique_names,
      "Estimated total cols:", estimated_cols
    ),
    2
  )

  list(
    type           = "wider",
    data           = current_data,
    names_from     = names_from,
    values_from    = values_from,
    id_cols        = id_cols,
    estimated_cols = estimated_cols,
    unique_names   = unique_names
  )
}

#' Build parameter list for a longer pivot apply request.
#'
#' @param current_data data.frame to pivot
#' @param cols character vector of column names to pivot (from input$longer_cols)
#' @param names_to character name for new names column
#' @param values_to character name for new values column
#' @param debug_log optional logging function accepting (message, level)
#' @return named list with type/data/cols/... or list(error = "message")
pivot_build_longer_params <- function(current_data, cols, names_to, values_to,
                                      debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (is.null(cols) || length(cols) == 0) {
    return(list(error = "Please select columns to pivot."))
  }

  invalid_cols <- cols[!cols %in% names(current_data)]
  if (length(invalid_cols) > 0) {
    return(list(error = "One or more selected pivot columns were not found in the current dataset."))
  }

  pivot_cols_count <- length(cols)
  estimated_rows   <- nrow(current_data) * pivot_cols_count

  debug_log(
    paste(
      "Longer pivot estimation - Pivot cols:", pivot_cols_count,
      "Current rows:", nrow(current_data),
      "Estimated rows:", estimated_rows
    ),
    2
  )

  list(
    type             = "longer",
    data             = current_data,
    cols             = cols,
    names_to         = names_to,
    values_to        = values_to,
    estimated_rows   = estimated_rows,
    pivot_cols_count = pivot_cols_count
  )
}
