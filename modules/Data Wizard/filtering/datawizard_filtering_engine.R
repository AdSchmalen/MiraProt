# ==============================================================================
# File: modules/Data Wizard/filtering/datawizard_filtering_engine.R
#
# Purpose:
#   Contains all pure data-transformation functions for the filtering module.
#   These functions receive data frames and configuration as input and return
#   filtered data or logical vectors. They do NOT depend on Shiny reactivity.
#
# Architectural Role:
#   Engine layer of the filtering module. Called by observers and the
#   perform_filtering bridge function inside modFilteringServer.
#   Sourced into modEnv via datawizard_filtering.R.
#
# Structure:
#   1. Value classification helpers (is_valid_abundance_value_dw, is_empty_like_dw)
#   2. Metadata query helpers (find_abundance_columns_with_priority,
#      find_confidence_columns_typed)
#   3. Input sanitization (sanitize_filter_input)
#   4. Operator-level filter application (apply_filter_operator)
#   5. Single-column filter application (apply_single_custom_filter)
#   6. Valid-value filter (apply_valid_value_filter)
#   7. Multi-column filter with SOME logic (apply_enhanced_multi_column_filter)
#   8. Batch custom filter application (apply_custom_filters_batch)
#   9. Top-level orchestrator (apply_all_filters_function_improved)
#
# Dependencies:
#   - Utility functions from datawizard_filtering_utils.R: safe_character_check,
#     safe_logical_check, safe_numeric_check, safe_sprintf.
#   - debug_log must exist in the calling environment (defined at modEnv level
#     by datawizard_core.R, or overridden inside moduleServer for the
#     [FILTERING] prefix variant).
#
# Notes for future developers:
#   - Every function in this file must remain Shiny-free (no input, output,
#     session, reactive, observe). This keeps them unit-testable.
#   - The one exception is find_confidence_columns_typed /
#     find_abundance_columns_with_priority which accept a metadata argument
#     that MAY be a reactive. They handle both cases internally via
#     is.reactive() checks. This is intentional for backward compatibility.
#   - If you add a new filter type, add it as a function here and wire it
#     from the observers file.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Value classification helpers
# ------------------------------------------------------------------------------

#' Check whether a single value qualifies as a valid abundance value.
#' A value is valid if it is non-NA, non-empty, non-zero, and finite.
#' Character representations of numbers are converted and checked.
#'
#' @param value A single scalar value (numeric or character).
#' @return TRUE if the value is considered valid, FALSE otherwise.
is_valid_abundance_value_dw <- function(value) {
  if (length(value) == 0 || is.null(value) || is.na(value)) {
    return(FALSE)
  }

  if (is.factor(value)) {
    value <- as.character(value)
  }

  if (is.character(value)) {
    trimmed <- trimws(value)
    if (trimmed == "" || tolower(trimmed) %in% c("na", "n/a", "null", "none", "-", "\u2014")) {
      return(FALSE)
    }

    numeric_value <- suppressWarnings(as.numeric(trimmed))
    if (is.na(numeric_value) || !is.finite(numeric_value)) {
      return(FALSE)
    }

    return(numeric_value != 0)
  }

  if (!is.numeric(value) || !is.finite(value)) {
    return(FALSE)
  }

  value != 0
}

#' Detect "empty-like" values in a vector.
#' Covers NA, empty strings, and common placeholder strings.
#'
#' @param x A vector (character, factor, numeric, or logical).
#' @return A logical vector of the same length, TRUE where x is empty-like.
is_empty_like_dw <- function(x) {
  out <- is.na(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    t <- trimws(x)
    out <- out | (t == "") | (tolower(t) %in% c("na", "n/a", "null", "none", "-", "\u2014"))
  }
  out
}


# ------------------------------------------------------------------------------
# 2. Metadata query helpers
# ------------------------------------------------------------------------------

#' Find abundance columns in metadata using a priority order.
#' Priority: Raw Abundance > Normalized Abundance > Batch Corrected Abundance.
#'
#' @param metadata_def Metadata data frame (or reactive wrapping one).
#' @param metadata_ready Logical flag; if FALSE, returns empty immediately.
#' @return A list with elements: columns, types, groups, selected_type.
find_abundance_columns_with_priority <- function(metadata_def, metadata_ready = TRUE) {
  empty_result <- list(columns = character(0), types = character(0),
                       groups = character(0), selected_type = "")
  if (!metadata_ready) return(empty_result)

  tryCatch({
    current_metadata <- if (is.reactive(metadata_def)) metadata_def() else metadata_def

    if (is.null(current_metadata) || !is.data.frame(current_metadata) || nrow(current_metadata) == 0) {
      return(empty_result)
    }

    priority_types <- c("Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance")

    for (priority_type in priority_types) {
      matching_rows <- which(grepl(paste0("^", priority_type, "$"),
                                   current_metadata$Content, ignore.case = TRUE))

      if (length(matching_rows) > 0) {
        columns <- current_metadata$Column[matching_rows]
        columns <- columns[!is.na(columns) & nzchar(columns)]

        if (length(columns) > 0) {
          debug_log(paste("Selected abundance type:", priority_type,
                          "with", length(columns), "columns"), 2)

          groups <- if ("Options" %in% names(current_metadata)) {
            options_values <- current_metadata$Options[matching_rows[seq_along(columns)]]
            ifelse(is.na(options_values) | options_values == "", "No_Group", options_values)
          } else {
            rep("No_Group", length(columns))
          }

          return(list(
            columns = columns,
            types = rep(priority_type, length(columns)),
            groups = groups,
            selected_type = priority_type
          ))
        }
      }
    }

    debug_log("No abundance columns found in metadata", 2)
    return(empty_result)

  }, error = function(e) {
    debug_log(paste("Error finding abundance columns:", e$message), 1)
    return(empty_result)
  })
}

#' Find confidence columns in metadata, split into numeric and character types.
#' Uses the Content == "Protein Confidence" convention in the metadata.
#'
#' @param metadata_def Metadata data frame (or reactive).
#' @param data_df Optional data frame to determine actual column types.
#' @param metadata_ready Logical flag.
#' @param min_numeric_fraction Minimum fraction of parseable numeric values
#'   to classify a character column as numeric.
#' @return A list with elements: numeric, character, all (each a character vector).
find_confidence_columns_typed <- function(metadata_def, data_df = NULL, metadata_ready = TRUE,
                                          min_numeric_fraction = 0.7) {
  empty_result <- list(numeric = character(0), character = character(0), all = character(0))
  if (!metadata_ready) return(empty_result)

  safe_debug <- function(msg) {
    if (exists("debug_log")) try(debug_log(msg, 2), silent = TRUE)
  }

  tryCatch({
    current_metadata <- if (is.reactive(metadata_def)) metadata_def() else metadata_def
    if (is.null(current_metadata) || !is.data.frame(current_metadata) || nrow(current_metadata) == 0) {
      return(empty_result)
    }

    # Confidence columns are identified via Content == "Protein Confidence"
    candidate_cols <- character(0)
    if ("Content" %in% names(current_metadata) && "Column" %in% names(current_metadata)) {
      matches <- current_metadata$Content == "Protein Confidence"
      if (any(matches, na.rm = TRUE)) {
        candidate_cols <- current_metadata$Column[matches]
      }
    }

    candidate_cols <- unique(candidate_cols)
    candidate_cols <- candidate_cols[!is.na(candidate_cols) & nzchar(candidate_cols)]
    if (length(candidate_cols) == 0) return(empty_result)

    # Without data, conservatively classify everything as character
    if (is.null(data_df) || !is.data.frame(data_df)) {
      return(list(numeric = character(0), character = candidate_cols, all = candidate_cols))
    }

    numeric_cols <- character(0)
    character_cols <- character(0)

    for (col in candidate_cols) {
      if (!col %in% names(data_df)) next
      v <- data_df[[col]]

      if (is.numeric(v)) {
        numeric_cols <- c(numeric_cols, col)
        next
      }

      # Attempt numeric interpretation of character column
      suppressWarnings({ vn <- as.numeric(v) })
      non_na_total <- sum(!is.na(v))
      non_na_numeric <- sum(!is.na(vn))
      frac <- ifelse(non_na_total == 0, 0, non_na_numeric / non_na_total)

      if (frac >= min_numeric_fraction && non_na_total > 0) {
        numeric_cols <- c(numeric_cols, col)
      } else {
        character_cols <- c(character_cols, col)
      }
    }

    all_cols <- unique(c(numeric_cols, character_cols))
    safe_debug(paste("Confidence detection -> numeric:", paste(numeric_cols, collapse = ","),
                     "| character:", paste(character_cols, collapse = ",")))

    list(numeric = unique(numeric_cols),
         character = unique(character_cols),
         all = unique(all_cols))

  }, error = function(e) {
    safe_debug(paste("Error in confidence detection:", e$message))
    empty_result
  })
}


# ------------------------------------------------------------------------------
# 3. Input sanitization
# ------------------------------------------------------------------------------

#' Sanitize a filter input value: convert to character, trim, truncate,
#' and remove potentially dangerous characters.
#'
#' @param input_value The raw input value.
#' @param max_length Maximum allowed character length (default 1000).
#' @param debug_level Debug verbosity level passed to debug_log.
#' @return A sanitized character string, or NA_character_ if input is invalid.
sanitize_filter_input <- function(input_value, max_length = 1000, debug_level = 0) {
  tryCatch({
    if (is.null(max_length) || !is.numeric(max_length)) max_length <- 1000

    if (is.null(input_value)) {
      return(NA_character_)
    }

    # Convert to character
    if (!is.character(input_value)) {
      input_value <- tryCatch(as.character(input_value), error = function(e) NA_character_)
    }

    if (length(input_value) == 0 || all(is.na(input_value))) return(NA_character_)
    if (length(input_value) > 1) input_value <- input_value[1]
    if (is.na(input_value)) return(NA_character_)

    # Truncate
    if (nchar(input_value) > max_length) {
      debug_log(paste("Input truncated to", max_length, "characters"), 1)
      input_value <- substr(input_value, 1, max_length)
    }

    # Remove potentially dangerous characters
    input_value <- gsub("[`'\";\\\\]", "", input_value)
    input_value <- trimws(input_value)

    if (nchar(input_value) == 0) return(NA_character_)

    return(input_value)

  }, error = function(e) {
    debug_log(paste("Critical error in input sanitization:", e$message), 1)
    return(NA_character_)
  })
}


# ------------------------------------------------------------------------------
# 4. Operator-level filter application
# ------------------------------------------------------------------------------

#' Apply a comparison operator to a column vector.
#' Supports numeric operators (>, >=, <, <=, ==, !=) and string operators
#' (contains, not_contains, equals, starts, ends).
#'
#' @param col_data Column vector from the data frame.
#' @param operator Character string identifying the operator.
#' @param value The comparison value (character; converted to numeric when needed).
#' @return A logical vector of the same length as col_data.
apply_filter_operator <- function(col_data, operator, value) {
  tryCatch({
    if (operator %in% c(">", ">=", "<", "<=", "==", "!=")) {
      numeric_col <- suppressWarnings(as.numeric(col_data))
      numeric_value <- suppressWarnings(as.numeric(value))

      if (!is.na(numeric_value)) {
        switch(operator,
               ">"  = !is.na(numeric_col) & numeric_col >  numeric_value,
               ">=" = !is.na(numeric_col) & numeric_col >= numeric_value,
               "<"  = !is.na(numeric_col) & numeric_col <  numeric_value,
               "<=" = !is.na(numeric_col) & numeric_col <= numeric_value,
               "==" = !is.na(numeric_col) & numeric_col == numeric_value,
               "!=" = is.na(numeric_col) | numeric_col != numeric_value)
      } else {
        # Fallback to string comparison for == and !=
        char_col <- as.character(col_data)
        switch(operator,
               "==" = !is.na(char_col) & char_col == value,
               "!=" = is.na(char_col) | char_col != value,
               rep(FALSE, length(col_data)))
      }
    } else {
      # String operations
      char_col <- as.character(col_data)
      switch(operator,
             "contains"     = !is.na(char_col) & grepl(value, char_col, fixed = TRUE),
             "not_contains" = is.na(char_col) | !grepl(value, char_col, fixed = TRUE),
             "equals"       = !is.na(char_col) & char_col == value,
             "starts"       = !is.na(char_col) & startsWith(char_col, value),
             "ends"         = !is.na(char_col) & endsWith(char_col, value),
             rep(FALSE, length(col_data)))
    }

  }, error = function(e) {
    debug_log(paste("Error in filter operator:", e$message), 1)
    return(rep(FALSE, length(col_data)))
  })
}


# ------------------------------------------------------------------------------
# 5. Single-column filter application
# ------------------------------------------------------------------------------

#' Apply a single custom filter condition to one column.
#' Handles empty-value filtering and up to two chained operator conditions.
#'
#' @param data Data frame.
#' @param column_name Name of the column to filter.
#' @param condition A single-row data frame describing the filter condition.
#' @return A logical vector (TRUE = keep row).
apply_single_custom_filter <- function(data, column_name, condition) {
  tryCatch({
    if (!column_name %in% names(data)) {
      return(rep(TRUE, nrow(data)))
    }

    col_data <- data[[column_name]]
    rows_to_keep <- rep(TRUE, nrow(data))

    # Handle empty filter first
    empty_filter <- safe_character_check(condition$Empty_Filter, "None")
    if (empty_filter != "None") {
      empties <- is_empty_like_dw(col_data)
      rows_to_keep <- rows_to_keep & if (empty_filter == "Remove Empty") !empties else empties
    }

    # Apply first condition
    value_1 <- safe_character_check(condition$Value_1)
    operator_1 <- safe_character_check(condition$Operator_1, "==")

    if (nchar(value_1) > 0) {
      condition_1_result <- apply_filter_operator(col_data, operator_1, value_1)

      # Apply second condition if present
      value_2 <- safe_character_check(condition$Value_2)
      if (nchar(value_2) > 0) {
        operator_2 <- safe_character_check(condition$Operator_2, "==")
        logic <- safe_character_check(condition$Logic, "AND")
        condition_2_result <- apply_filter_operator(col_data, operator_2, value_2)

        string_operators <- c("contains", "not_contains", "equals", "starts", "ends")
        final_condition <- if (
          logic == "EXCLUDE" &&
            operator_1 %in% string_operators &&
            operator_2 %in% string_operators
        ) {
          if (operator_1 == "not_contains") {
            condition_1_result | condition_2_result
          } else {
            condition_1_result & !condition_2_result
          }
        } else if (logic == "AND") {
          condition_1_result & condition_2_result
        } else {
          condition_1_result | condition_2_result
        }
      } else {
        final_condition <- condition_1_result
      }

      rows_to_keep <- rows_to_keep & final_condition
    }

    return(rows_to_keep)

  }, error = function(e) {
    debug_log(paste("Error in single custom filter:", e$message), 1)
    return(rep(TRUE, nrow(data)))
  })
}


# ------------------------------------------------------------------------------
# 6. Valid-value filter
# ------------------------------------------------------------------------------

#' Filter rows based on valid abundance value counts.
#' Supports three modes: "In total", "One group", "Each group".
#'
#' @param data Data frame to filter.
#' @param abundance_info List from find_abundance_columns_with_priority.
#' @param filter_settings List containing $valid_values$min_count and
#'   $valid_values$group_selection.
#' @return Filtered data frame.
apply_valid_value_filter <- function(data, abundance_info, filter_settings) {
  tryCatch({
    if (length(abundance_info$columns) == 0) {
      debug_log("No abundance columns available for valid value filtering", 1)
      return(data)
    }

    min_count <- as.numeric(filter_settings$valid_values$min_count)
    group_selection <- filter_settings$valid_values$group_selection

    if (is.na(min_count) || min_count <= 0) {
      debug_log("Invalid min_count for valid value filtering", 1)
      return(data)
    }

    debug_log(paste("Applying valid value filter:", group_selection,
                    "mode, min count:", min_count), 1)

    # Only use columns that exist in the data
    available_columns <- abundance_info$columns[abundance_info$columns %in% names(data)]
    available_groups <- abundance_info$groups[abundance_info$columns %in% names(data)]

    if (length(available_columns) == 0) {
      debug_log("No abundance columns exist in current data", 1)
      return(data)
    }

    rows_to_keep <- rep(TRUE, nrow(data))

    if (group_selection == "In total") {
      for (i in seq_len(nrow(data))) {
        valid_count <- 0
        for (col in available_columns) {
          if (is_valid_abundance_value_dw(data[i, col])) valid_count <- valid_count + 1
        }
        rows_to_keep[i] <- valid_count >= min_count
      }
      debug_log(paste("In total mode: keeping", sum(rows_to_keep),
                      "of", length(rows_to_keep), "rows"), 2)

    } else if (group_selection == "One group") {
      unique_groups <- unique(available_groups)
      for (i in seq_len(nrow(data))) {
        passes_any_group <- FALSE
        for (group in unique_groups) {
          group_columns <- available_columns[available_groups == group]
          valid_count <- 0
          for (col in group_columns) {
            if (is_valid_abundance_value_dw(data[i, col])) valid_count <- valid_count + 1
          }
          if (valid_count >= min_count) { passes_any_group <- TRUE; break }
        }
        rows_to_keep[i] <- passes_any_group
      }
      debug_log(paste("One group mode: keeping", sum(rows_to_keep),
                      "of", length(rows_to_keep), "rows"), 2)

    } else if (group_selection == "Each group") {
      unique_groups <- unique(available_groups)
      for (i in seq_len(nrow(data))) {
        passes_all_groups <- TRUE
        for (group in unique_groups) {
          group_columns <- available_columns[available_groups == group]
          valid_count <- 0
          for (col in group_columns) {
            if (is_valid_abundance_value_dw(data[i, col])) valid_count <- valid_count + 1
          }
          if (valid_count < min_count) { passes_all_groups <- FALSE; break }
        }
        rows_to_keep[i] <- passes_all_groups
      }
      debug_log(paste("Each group mode: keeping", sum(rows_to_keep),
                      "of", length(rows_to_keep), "rows"), 2)
    }

    filtered_data <- data[rows_to_keep, , drop = FALSE]
    debug_log(paste("Valid value filter removed", nrow(data) - nrow(filtered_data), "rows"), 1)
    return(filtered_data)

  }, error = function(e) {
    debug_log(paste("Error in valid value filter:", e$message), 1)
    return(data)
  })
}


# ------------------------------------------------------------------------------
# 7. Multi-column filter with SOME logic
# ------------------------------------------------------------------------------

#' Apply a filter condition across multiple columns with OR / AND / SOME logic.
#'
#' SOME logic allows specifying "at least N", "less than N", or "exactly N"
#' columns that must meet the condition.
#'
#' @param data_df Data frame to filter.
#' @param column_names Character vector of column names.
#' @param condition A single-row data frame with the filter condition,
#'   including Multi_Column_Logic, Some_Operator, Some_Count fields.
#' @param debug_level Debug verbosity level.
#' @return Filtered data frame.
apply_enhanced_multi_column_filter <- function(data_df, column_names, condition,
                                               debug_level = 0) {
  tryCatch({
    debug_log(paste("Applying multi-column filter to", length(column_names), "columns"), 2)

    if (is.null(data_df) || !is.data.frame(data_df) || nrow(data_df) == 0) {
      debug_log("Invalid or empty input data for multi-column filter", 1)
      return(data_df)
    }
    if (is.null(column_names) || length(column_names) == 0) {
      debug_log("No column names provided for multi-column filter", 1)
      return(data_df)
    }

    available_columns <- column_names[column_names %in% names(data_df)]
    if (length(available_columns) == 0) {
      debug_log("No available columns found", 1)
      return(data_df)
    }
    if (length(available_columns) < length(column_names)) {
      debug_log(paste("Missing columns:",
                      paste(setdiff(column_names, available_columns), collapse = ", ")), 2)
    }

    multi_logic <- safe_character_check(condition$Multi_Column_Logic, "OR")
    if (multi_logic == "SOME" && length(available_columns) < 2) {
      debug_log("SOME logic requires multiple columns, falling back to OR", 1)
      multi_logic <- "OR"
    }

    # Collect per-column boolean results
    column_conditions <- list()
    for (col_name in available_columns) {
      col_result <- tryCatch(
        apply_single_custom_filter(data_df, col_name, condition),
        error = function(e) {
          debug_log(paste("Error processing column", col_name, ":", e$message), 1)
          NULL
        }
      )
      if (!is.null(col_result) && is.logical(col_result) && length(col_result) == nrow(data_df)) {
        column_conditions[[col_name]] <- col_result
      }
    }

    if (length(column_conditions) == 0) {
      debug_log("No valid column conditions generated", 1)
      return(data_df)
    }

    # Combine per-column results according to the chosen logic
    final_condition <- tryCatch({
      if (multi_logic == "OR") {
        debug_log("Applied OR logic across columns", 2)
        Reduce("|", column_conditions)

      } else if (multi_logic == "AND") {
        debug_log("Applied AND logic across columns", 2)
        Reduce("&", column_conditions)

      } else if (multi_logic == "SOME") {
        some_operator_val <- safe_character_check(condition$Some_Operator, "at_least")
        some_count_val <- safe_numeric_check(condition$Some_Count,
                                             min_val = 1,
                                             max_val = length(column_conditions),
                                             default_val = 1)
        if (!some_operator_val %in% c("at_least", "less_than", "exactly")) {
          debug_log(paste("Invalid SOME operator:", some_operator_val, "- using 'at_least'"), 1)
          some_operator_val <- "at_least"
        }

        condition_matrix <- tryCatch(
          do.call(cbind, column_conditions),
          error = function(e) {
            debug_log(paste("Error creating condition matrix:", e$message), 1)
            mat <- matrix(FALSE, nrow = nrow(data_df), ncol = length(column_conditions))
            for (i in seq_along(column_conditions)) {
              if (length(column_conditions[[i]]) == nrow(data_df)) {
                mat[, i] <- column_conditions[[i]]
              }
            }
            mat
          }
        )

        if (is.null(condition_matrix)) {
          debug_log("Failed to create condition matrix, using OR logic", 1)
          Reduce("|", column_conditions)
        } else {
          columns_meeting <- tryCatch(rowSums(condition_matrix, na.rm = TRUE),
                                      error = function(e) rep(0, nrow(data_df)))
          result_cond <- switch(
            some_operator_val,
            "at_least"  = columns_meeting >= some_count_val,
            "less_than" = columns_meeting <  some_count_val,
            "exactly"   = columns_meeting == some_count_val,
            Reduce("|", column_conditions)
          )
          debug_log(paste("Applied SOME logic:", some_operator_val,
                          some_count_val, "columns"), 2)
          result_cond
        }

      } else {
        debug_log(paste("Unknown multi-column logic:", multi_logic, "- using OR"), 1)
        Reduce("|", column_conditions)
      }
    }, error = function(e) {
      debug_log(paste("Critical error in logic application:", e$message), 1)
      tryCatch(Reduce("|", column_conditions),
               error = function(e2) rep(TRUE, nrow(data_df)))
    })

    # Apply the combined condition
    if (!is.null(final_condition) && is.logical(final_condition) &&
        length(final_condition) == nrow(data_df)) {
      final_condition[is.na(final_condition)] <- FALSE
      filtered_result <- data_df[final_condition, , drop = FALSE]
      debug_log(paste("Multi-column filter removed",
                      nrow(data_df) - nrow(filtered_result), "rows"), 1)
      return(filtered_result)
    } else {
      debug_log("Invalid final condition, returning original data", 1)
      return(data_df)
    }

  }, error = function(e) {
    debug_log(paste("Critical error in multi-column filter:", e$message), 1)
    return(data_df)
  })
}


# ------------------------------------------------------------------------------
# 8. Batch custom filter application
# ------------------------------------------------------------------------------

#' Apply all custom filter conditions sequentially to a data frame.
#' Each condition is parsed, validated, and applied. Multi-column conditions
#' are routed through apply_enhanced_multi_column_filter.
#'
#' @param data_df Data frame to filter.
#' @param custom_conditions Data frame of filter conditions (one row per condition).
#' @param debug_level Debug verbosity level.
#' @return A list with: data, success, rows_removed, errors, warnings, debug_info.
apply_custom_filters_batch <- function(data_df, custom_conditions, debug_level = 0) {
  result <- list(
    data = data_df,
    success = TRUE,
    rows_removed = 0,
    errors = character(),
    warnings = character(),
    debug_info = character()
  )

  tryCatch({
    if (is.null(data_df) || !is.data.frame(data_df)) {
      result$errors <- c(result$errors, "Invalid input data for custom filters")
      result$success <- FALSE
      debug_log("Custom filters: Invalid input data", 1)
      return(result)
    }
    if (nrow(data_df) == 0) {
      result$warnings <- c(result$warnings, "Empty input data - returning as is")
      return(result)
    }
    if (is.null(custom_conditions) || !is.data.frame(custom_conditions) ||
        nrow(custom_conditions) == 0) {
      return(result)
    }

    required_cols <- c("Column", "Operator_1", "Value_1")
    missing_cols <- setdiff(required_cols, names(custom_conditions))
    if (length(missing_cols) > 0) {
      result$errors <- c(result$errors,
                         paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      result$success <- FALSE
      return(result)
    }

    initial_rows <- nrow(data_df)
    filtered_data <- data_df
    debug_log(paste("Applying", nrow(custom_conditions),
                    "custom filter conditions to", initial_rows, "rows"), 1)

    for (i in seq_len(nrow(custom_conditions))) {
      condition_result <- tryCatch({
        condition_row <- custom_conditions[i, , drop = FALSE]

        if (is.na(condition_row$Column) || !nzchar(condition_row$Column)) {
          list(success = FALSE,
               warning = paste("Skipping row", i, ": empty column name"),
               data = filtered_data)
        } else {
          # Sanitize filter values
          condition_row$Value_1 <- sanitize_filter_input(condition_row$Value_1,
                                                         debug_level = debug_level)
          condition_row$Value_2 <- sanitize_filter_input(condition_row$Value_2,
                                                         debug_level = debug_level)

          # Parse pipe-separated column names
          column_names <- trimws(strsplit(condition_row$Column, "\\|")[[1]])

          if (length(column_names) == 0) {
            list(success = FALSE,
                 error = paste("Could not parse column names for condition", i),
                 data = filtered_data)
          } else {
            missing_columns <- setdiff(column_names, names(filtered_data))
            available_cols <- intersect(column_names, names(filtered_data))

            if (length(available_cols) == 0) {
              list(success = FALSE,
                   error = paste("No available columns for condition", i),
                   data = filtered_data)
            } else {
              warn_msg <- if (length(missing_columns) > 0) {
                paste("Missing columns for condition", i, ":",
                      paste(missing_columns, collapse = ", "))
              } else NULL

              list(success = TRUE, data = filtered_data,
                   columns = available_cols, condition = condition_row,
                   warning = warn_msg)
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Critical error processing condition", i, ":", e$message), 1)
        list(success = FALSE,
             error = paste("Critical error in condition", i, ":", e$message),
             data = filtered_data)
      })

      # Collect warnings / errors from condition parsing
      if (!is.null(condition_result$warning)) {
        result$warnings <- c(result$warnings, condition_result$warning)
      }
      if (!condition_result$success) {
        if (!is.null(condition_result$error)) {
          result$errors <- c(result$errors, condition_result$error)
        }
        next
      }

      # Apply filter (multi-column or single-column)
      filter_result <- tryCatch({
        filtered_condition_data <- if (length(condition_result$columns) > 1) {
          apply_enhanced_multi_column_filter(
            condition_result$data,
            condition_result$columns,
            condition_result$condition,
            debug_level
          )
        } else {
          keep_rows <- apply_single_custom_filter(
            condition_result$data,
            condition_result$columns[[1]],
            condition_result$condition
          )
          
          if (!is.logical(keep_rows) ||
              length(keep_rows) != nrow(condition_result$data)) {
            stop("Single-column filter returned an invalid keep vector")
          }
          
          keep_rows[is.na(keep_rows)] <- FALSE
          condition_result$data[keep_rows, , drop = FALSE]
        }
        
        list(
          success = is.data.frame(filtered_condition_data),
          data = if (is.data.frame(filtered_condition_data)) {
            filtered_condition_data
          } else {
            condition_result$data
          },
          warnings = character(),
          errors = if (is.data.frame(filtered_condition_data)) {
            character()
          } else {
            "Custom filter failed"
          }
        )
        
      }, error = function(e) {
        debug_log(paste("Error in custom filter", i, ":", e$message), 1)
        list(
          success = FALSE,
          data = condition_result$data,
          warnings = character(),
          errors = paste("Custom filter error:", e$message)
        )
      })

      if (filter_result$success && !is.null(filter_result$data) &&
          is.data.frame(filter_result$data)) {
        rows_before <- nrow(filtered_data)
        filtered_data <- filter_result$data
        rows_removed_cond <- rows_before - nrow(filtered_data)
        result$warnings <- c(result$warnings, filter_result$warnings)
        if (rows_removed_cond > 0) {
          result$debug_info <- c(result$debug_info,
                                 paste("Filter", i, "removed", rows_removed_cond, "rows"))
        }
      } else {
        result$errors <- c(result$errors, filter_result$errors)
        result$warnings <- c(result$warnings,
                             paste("Filter", i, "failed - data unchanged"))
      }
    }

    result$data <- filtered_data
    result$rows_removed <- initial_rows - nrow(filtered_data)
    result$success <- TRUE
    debug_log(paste("Custom filters completed: removed", result$rows_removed, "rows total"), 1)

  }, error = function(e) {
    result$errors <- c(result$errors, paste("Critical custom filter error:", e$message))
    result$data <- data_df
    result$success <- TRUE
    result$warnings <- c(result$warnings, "Critical error occurred - returning original data")
    debug_log(paste("Critical custom filter error:", e$message), 1)
  })

  return(result)
}


# ------------------------------------------------------------------------------
# 9. Top-level filter orchestrator
# ------------------------------------------------------------------------------

#' Apply all filter stages (confidence, valid values, custom) in sequence.
#'
#' @param data_input Data frame to filter.
#' @param metadata_def Metadata data frame (or reactive).
#' @param filter_settings Reactive or plain list with $confidence and $valid_values.
#' @param custom_conditions Data frame of custom filter conditions.
#' @param metadata_ready_status Logical flag.
#' @param debug_level Debug verbosity level.
#' @return A list with: data, success, rows_original, rows_filtered, rows_removed,
#'   errors, warnings, processing_time.
apply_all_filters_function_improved <- function(data_input, metadata_def, filter_settings,
                                                custom_conditions,
                                                metadata_ready_status = TRUE,
                                                debug_level = 0,
                                                debug_log = NULL) {
  if (is.null(debug_log)) {
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "FILTERING", message)
      } else if (is.numeric(debug_level) && debug_level >= level) {
        cat(paste0("[ FILTERING ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }
  }
  result <- list(
    data = data_input,
    success = FALSE,
    rows_original = 0,
    rows_filtered = 0,
    rows_removed = 0,
    errors = character(),
    warnings = character(),
    processing_time = 0
  )

  processing_start_time <- Sys.time()

  tryCatch({
    if (!is.data.frame(data_input)) {
      result$errors <- c(result$errors, "Input data is not a data frame")
      return(result)
    }
    if (nrow(data_input) == 0) {
      result$errors <- c(result$errors, "Input data is empty")
      return(result)
    }

    filtered_data <- data_input
    initial_rows <- nrow(filtered_data)
    result$rows_original <- initial_rows
    debug_log(paste("Starting filter application with", initial_rows, "rows"), 1)

    # --- Step 1: Confidence filters ---
    confidence_numeric_enabled <- safe_logical_check(filter_settings$confidence$numeric_enabled)
    confidence_string_enabled  <- safe_logical_check(filter_settings$confidence$string_enabled)
    confidence_applied         <- FALSE
    confidence_rows_before_tab <- nrow(filtered_data)
    confidence_numeric_cols    <- character()
    confidence_string_cols     <- character()
    confidence_numeric_min     <- NA_real_
    confidence_numeric_max     <- NA_real_
    confidence_string_input    <- ""

    if (confidence_numeric_enabled || confidence_string_enabled) {

      debug_log("Applying confidence filters", 1)
      current_metadata <- if (is.reactive(metadata_def)) metadata_def() else metadata_def
      confidence_info <- find_confidence_columns_typed(
        metadata_def = current_metadata,
        data_df = filtered_data,
        metadata_ready = metadata_ready_status
      )

      if (length(confidence_info$all) > 0) {
        debug_log(paste("Confidence columns detected -> numeric:",
                        paste(confidence_info$numeric, collapse = ","),
                        "| character:",
                        paste(confidence_info$character, collapse = ",")), 1)
      }

      # String confidence filter (only on character confidence columns)
      if (confidence_string_enabled && length(confidence_info$character) > 0) {
        string_input <- safe_character_check(filter_settings$confidence$string_input)
        confidence_string_input <- string_input
        if (nzchar(string_input)) {
          for (col in confidence_info$character) {
            if (!col %in% names(filtered_data)) next
            col_data <- as.character(filtered_data[[col]])
            rows_before <- nrow(filtered_data)
            rows_to_keep <- !grepl(string_input, col_data, fixed = TRUE)
            filtered_data <- filtered_data[rows_to_keep, , drop = FALSE]
            confidence_string_cols <- c(confidence_string_cols, col)
            confidence_applied <- TRUE
            debug_log(paste("String confidence filter on", col, "removed",
                            rows_before - nrow(filtered_data), "rows"), 2)
          }
        }
      }

      # Numeric confidence filter (only on numeric confidence columns)
      if (confidence_numeric_enabled && length(confidence_info$numeric) > 0) {
        min_thr <- filter_settings$confidence$numeric_min
        max_thr <- filter_settings$confidence$numeric_max
        confidence_numeric_min <- suppressWarnings(as.numeric(min_thr))
        confidence_numeric_max <- suppressWarnings(as.numeric(max_thr))
        for (col in confidence_info$numeric) {
          if (!col %in% names(filtered_data)) next
          col_raw <- filtered_data[[col]]
          col_num <- if (!is.numeric(col_raw)) {
            suppressWarnings(as.numeric(col_raw))
          } else {
            col_raw
          }
          if (all(is.na(col_num))) {
            debug_log(paste("Skipping numeric confidence filter on", col,
                            ": no numeric values"), 2)
            next
          }
          rows_to_keep <- rep(TRUE, nrow(filtered_data))
          # "Minimum Threshold" means: keep values <= threshold
          if (!is.null(min_thr) && !is.na(min_thr)) {
            rows_to_keep <- rows_to_keep & (is.na(col_num) | col_num <= min_thr)
          }
          if (!is.null(max_thr) && !is.na(max_thr)) {
            rows_to_keep <- rows_to_keep & (is.na(col_num) | col_num <= max_thr)
          }
          rows_before <- nrow(filtered_data)
          filtered_data <- filtered_data[rows_to_keep, , drop = FALSE]
          confidence_numeric_cols <- c(confidence_numeric_cols, col)
          confidence_applied <- TRUE
          debug_log(paste("Numeric confidence filter on", col, "removed",
                          rows_before - nrow(filtered_data), "rows"), 2)
        }
      }
    }

    if (confidence_applied) {
      confidence_removed <- confidence_rows_before_tab - nrow(filtered_data)
      active_modes <- character(0)
      if (isTRUE(confidence_numeric_enabled) && length(confidence_numeric_cols) > 0) {
        active_modes <- c(
          active_modes,
          sprintf(
            "Numeric (min: %s, max: %s, columns: %s)",
            if (is.na(confidence_numeric_min)) "NA" else as.character(confidence_numeric_min),
            if (is.na(confidence_numeric_max)) "NA" else as.character(confidence_numeric_max),
            paste(confidence_numeric_cols, collapse = ", ")
          )
        )
      }
      if (isTRUE(confidence_string_enabled) && nzchar(confidence_string_input) && length(confidence_string_cols) > 0) {
        active_modes <- c(
          active_modes,
          sprintf(
            "String (pattern: '%s', columns: %s)",
            confidence_string_input,
            paste(confidence_string_cols, collapse = ", ")
          )
        )
      }

      if (length(active_modes) == 0) {
        debug_log("Confidence filter summary | No confidence filter applied", level = 0)
      } else {
        debug_log(
          sprintf(
            "Confidence filter summary | Active modes: %s | Rows removed: %d | Rows remaining: %d",
            paste(active_modes, collapse = " + "),
            confidence_removed,
            nrow(filtered_data)
          ),
          level = 0
        )
      }
    }

    # --- Step 2: Valid values filter ---
    valid_group <- safe_character_check(filter_settings$valid_values$group_selection, "In total")
    valid_min_count <- as.numeric(filter_settings$valid_values$min_count %||% 1)
    valid_values_applied <- FALSE
    valid_rows_before_tab <- nrow(filtered_data)
    valid_abundance_cols  <- character()
    valid_selected_type   <- NA_character_

    if (!is.na(valid_min_count) && is.finite(valid_min_count) && valid_min_count > 0) {
      debug_log(paste("Applying valid values filter: min", valid_min_count,
                      "values", valid_group), 1)
      current_metadata <- if (is.reactive(metadata_def)) metadata_def() else metadata_def
      abundance_info <- find_abundance_columns_with_priority(current_metadata,
                                                             metadata_ready_status)
      if (length(abundance_info$columns) > 0) {
        rows_before <- nrow(filtered_data)
        filtered_data <- apply_valid_value_filter(filtered_data, abundance_info, filter_settings)
        valid_abundance_cols <- abundance_info$columns
        valid_selected_type  <- as.character(abundance_info$selected_type %||% NA_character_)
        valid_values_applied <- TRUE
        debug_log(paste("Valid values filter removed",
                        rows_before - nrow(filtered_data), "rows"), 2)
      } else {
        result$warnings <- c(result$warnings,
                             "No abundance columns found for valid value filtering")
        debug_log("No abundance columns available for valid value filtering", 1)
      }
    }

    if (valid_values_applied) {
      valid_removed <- valid_rows_before_tab - nrow(filtered_data)
      debug_log(
        sprintf(
          paste0(
            "Valid values filter summary",
            " | Minimum valid values: %s",
            " | Group mode: %s",
            " | Abundance type: %s",
            " | Abundance columns (%d): %s",
            " | Rows removed: %d",
            " | Rows remaining: %d"
          ),
          as.character(valid_min_count),
          as.character(valid_group),
          as.character(valid_selected_type),
          length(valid_abundance_cols),
          if (length(valid_abundance_cols) == 0) "none" else paste(valid_abundance_cols, collapse = ", "),
          valid_removed,
          nrow(filtered_data)
        ),
        level = 0
      )
    }

    # --- Step 3: Custom filters ---
    custom_rows_before_tab <- nrow(filtered_data)
    custom_applied         <- FALSE
    custom_condition_descs <- character()

    if (!is.null(custom_conditions) && is.data.frame(custom_conditions) &&
        nrow(custom_conditions) > 0) {
      debug_log(paste("Applying", nrow(custom_conditions), "custom filter conditions"), 1)

      for (i in seq_len(nrow(custom_conditions))) {
        condition <- custom_conditions[i, ]
        column_names <- trimws(strsplit(condition$Column, "\\|")[[1]])
        available_columns <- column_names[column_names %in% names(filtered_data)]

        if (length(available_columns) == 0) {
          result$warnings <- c(result$warnings,
                               paste("No available columns for condition", i))
          next
        }

        rows_before <- nrow(filtered_data)
        condition_results <- list()
        for (col in available_columns) {
          condition_results[[col]] <- apply_single_custom_filter(filtered_data, col, condition)
        }

        multi_logic <- safe_character_check(condition$Multi_Column_Logic, "OR")
        if (length(condition_results) > 0) {
          final_rows <- if (multi_logic == "AND") {
            Reduce("&", condition_results)
          } else {
            Reduce("|", condition_results)
          }
          filtered_data <- filtered_data[final_rows, , drop = FALSE]

          operator_1 <- safe_character_check(condition$Operator_1, "")
          value_1    <- safe_character_check(condition$Value_1, "")
          operator_2 <- safe_character_check(condition$Operator_2, "")
          value_2    <- safe_character_check(condition$Value_2, "")
          logic_12   <- safe_character_check(condition$Logic, "AND")
          empty_str  <- safe_character_check(condition$Empty_Filter, "None")

          col_classes <- vapply(available_columns, function(col_name) {
            if (!col_name %in% names(data_input)) return("unknown")
            col_vec <- data_input[[col_name]]
            if (is.numeric(col_vec)) "numeric" else "character"
          }, character(1))
          unique_classes <- unique(col_classes)
          column_type_label <- if (length(unique_classes) == 1) unique_classes else paste(unique_classes, collapse = "+")

          second_condition_active <- nzchar(value_2)
          second_condition_desc <- if (second_condition_active) {
            sprintf("active (%s '%s' %s)", operator_2, value_2, logic_12)
          } else {
            "inactive (Value_2 empty)"
          }

          condition_summary <- sprintf(
            paste0(
              "#%d | Columns (%d): [%s]",
              " | Column type: %s",
              " | Condition 1: %s '%s'",
              " | Condition 2: %s",
              " | Empty handling: %s",
              " | Rows removed: %d",
              " | Rows remaining: %d"
            ),
            i,
            length(available_columns),
            paste(available_columns, collapse = ", "),
            column_type_label,
            if (nzchar(operator_1)) operator_1 else "(none)",
            if (nzchar(value_1)) value_1 else "(none)",
            second_condition_desc,
            empty_str,
            rows_before - nrow(filtered_data),
            nrow(filtered_data)
          )

          custom_condition_descs <- c(custom_condition_descs, condition_summary)
          custom_applied <- TRUE
          debug_log(paste("Custom filter summary |", condition_summary), level = 0)
        }
      }
    }

    if (custom_applied) {
      custom_removed <- custom_rows_before_tab - nrow(filtered_data)
      debug_log(
        sprintf(
          "Custom filters total | Conditions applied: %d | Rows removed: %d | Rows remaining: %d",
          length(custom_condition_descs),
          custom_removed,
          nrow(filtered_data)
        ),
        level = 0
      )
    }

    # Final results
    result$data <- filtered_data
    result$rows_filtered <- nrow(filtered_data)
    result$rows_removed <- initial_rows - nrow(filtered_data)
    result$success <- TRUE
    result$processing_time <- as.numeric(difftime(Sys.time(), processing_start_time,
                                                  units = "secs"))
    debug_log(paste("Filter application completed. Removed",
                    result$rows_removed, "rows"), 1)

  }, error = function(e) {
    debug_log(paste("Error in filter application:", e$message), 1)
    result$errors <- c(result$errors, paste("Filter application error:", e$message))
    result$processing_time <- as.numeric(difftime(Sys.time(), processing_start_time,
                                                  units = "secs"))
  })

  return(result)
}
