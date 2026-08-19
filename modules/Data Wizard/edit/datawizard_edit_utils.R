# ============================================================================
# Sub-Script: Data Wizard Edit Utilities
#
# Purpose:
#   Pure (non-reactive) helper functions used by the Edit module.
#   None of these functions access input, output, session, or reactive
#   values — they operate on plain R objects and return plain R objects.
#
# Architectural Role:
#   Stateless computation layer. Called by handlers, the API, and outputs
#   but never calls back into them. This makes every function here
#   independently testable without a Shiny session.
#
# Functions:
#   detect_column_type(data_vector)
#     -> "numeric" | "character" | "mixed" | "unknown"
#
#   detect_multi_column_types(data, columns, debug_log)
#     -> list(overall_type, individual_types, type_summary,
#             compatible, existing_columns)
#
#   serialize_parameters(params)
#     -> JSON string via jsonlite::toJSON
#
#   deserialize_parameters(params_string)
#     -> R list via jsonlite::fromJSON
#
#   validate_operations_table(ops_table, available_columns,
#                             reset_executed, debug_log)
#     -> list(success, operations, warnings, removed_count)
#
#   apply_single_operation(data, operation, debug_log)
#     -> list(success, data, message)
#
# Dependencies:
#   jsonlite (serialize/deserialize)
# ============================================================================

############
# Column Type Detection and Validation

#' Detect column data type for appropriate UI rendering
#'
#' @param data_vector vector to analyze
#' @return character indicating "numeric", "character", or "mixed"
detect_column_type <- function(data_vector) {
  if (is.null(data_vector) || length(data_vector) == 0) {
    return("unknown")
  }

  tryCatch({
    # Limit sample size for performance on large datasets
    sample_size <- min(1000, length(data_vector))
    if (length(data_vector) > sample_size) {
      sample_indices <- sample(length(data_vector), sample_size)
      data_sample <- data_vector[sample_indices]
    } else {
      data_sample <- data_vector
    }

    # Remove NA values for type detection
    non_na_data <- data_sample[!is.na(data_sample)]

    if (length(non_na_data) == 0) {
      return("unknown")
    }

    # Check if already numeric
    if (is.numeric(data_sample)) {
      return("numeric")
    }

    # Try numeric conversion for character/factor data
    if (is.character(data_sample) || is.factor(data_sample)) {
      # Remove empty strings and whitespace-only strings
      non_empty_data <- non_na_data[nzchar(trimws(as.character(non_na_data)))]

      if (length(non_empty_data) == 0) {
        return("character")
      }

      numeric_conversion <- suppressWarnings(as.numeric(as.character(non_empty_data)))
      conversion_rate <- sum(!is.na(numeric_conversion)) / length(non_empty_data)

      # If more than 90% can be converted to numeric, treat as numeric
      if (conversion_rate > 0.9) {
        return("numeric")
      } else {
        return("character")
      }
    }

    # Default fallback
    return("character")

  }, error = function(e) {
    return("unknown")
  })
}

#' Detect column types for multiple columns and determine overall type compatibility
#'
#' @param data_df data.frame containing the columns
#' @param column_names character vector of column names to analyze
#' @param debug_log function for debug output
#' @return list with overall_type, individual_types, and type_summary
detect_multi_column_types <- function(data_df, column_names, debug_log = function(...) {}) {
  if (is.null(data_df) || length(column_names) == 0) {
    return(list(
      overall_type = "unknown",
      individual_types = character(0),
      type_summary = "No columns selected",
      compatible = FALSE
    ))
  }

  tryCatch({
    # Check which columns exist in the data
    existing_columns <- intersect(column_names, names(data_df))

    if (length(existing_columns) == 0) {
      debug_log("No matching columns found in data", 2)
      return(list(
        overall_type = "unknown",
        individual_types = character(0),
        type_summary = "No matching columns found in data",
        compatible = FALSE
      ))
    }

    # Performance optimization: limit to first 10 columns for very large selections
    if (length(existing_columns) > 10) {
      debug_log(paste("Large column selection detected. Analyzing first 10 of", length(existing_columns), "columns"), 2)
      columns_to_analyze <- existing_columns[1:10]
      is_large_selection <- TRUE
    } else {
      columns_to_analyze <- existing_columns
      is_large_selection <- FALSE
    }

    # Detect type for each column
    individual_types <- character(length(columns_to_analyze))
    names(individual_types) <- columns_to_analyze

    for (col_name in columns_to_analyze) {
      individual_types[col_name] <- detect_column_type(data_df[[col_name]])
    }

    # Determine overall compatibility
    unique_types <- unique(individual_types)
    unique_types <- unique_types[unique_types != "unknown"]

    if (length(unique_types) == 0) {
      overall_type <- "unknown"
      compatible <- FALSE
      type_summary <- "All columns have unknown type"
    } else if (length(unique_types) == 1) {
      overall_type <- unique_types[1]
      compatible <- TRUE
      if (is_large_selection) {
        type_summary <- paste0("First 10 columns are ", overall_type, " (", length(existing_columns), " total)")
      } else {
        type_summary <- paste0("All ", length(existing_columns), " columns are ", overall_type)
      }
    } else {
      overall_type <- "mixed"
      compatible <- FALSE
      type_counts <- table(individual_types)
      type_summary <- paste0("Mixed types in first ", length(columns_to_analyze), " columns: ",
                             paste(names(type_counts), " (", type_counts, ")", collapse = ", "))
    }

    debug_log(paste("Column type detection:", type_summary), 2)

    return(list(
      overall_type = overall_type,
      individual_types = individual_types,
      type_summary = type_summary,
      compatible = compatible,
      existing_columns = existing_columns,
      analyzed_columns = columns_to_analyze,
      is_large_selection = is_large_selection
    ))

  }, error = function(e) {
    debug_log(paste("Error in multi-column type detection:", e$message), 1)
    return(list(
      overall_type = "unknown",
      individual_types = character(0),
      type_summary = "Type detection failed",
      compatible = FALSE
    ))
  })
}

############
# Parameter Serialization

#' Robust parameter serialization with escape handling
#'
#' @param params list of parameters to serialize
#' @return character string with serialized parameters
serialize_parameters <- function(params) {
  tryCatch({
    param_strings <- character()

    for (name in names(params)) {
      value <- params[[name]]

      # Handle different value types with proper escaping
      if (is.null(value)) {
        value_str <- "NULL"
      } else if (is.logical(value)) {
        value_str <- as.character(value)
      } else if (is.numeric(value)) {
        value_str <- as.character(value)
      } else {
        # Escape special characters in strings
        value_str <- gsub(":", "\\:", as.character(value), fixed = TRUE)
        value_str <- gsub(";", "\\;", value_str, fixed = TRUE)
      }

      param_strings <- c(param_strings, paste0(name, ":", value_str))
    }

    return(paste(param_strings, collapse = ";"))

  }, error = function(e) {
    return("")
  })
}

#' Robust parameter deserialization with escape handling
#'
#' @param param_string character string with serialized parameters
#' @return list of deserialized parameters
deserialize_parameters <- function(param_string) {
  tryCatch({
    if (is.null(param_string) || !nzchar(param_string)) {
      return(list())
    }

    # Split by unescaped semicolons
    param_pairs <- strsplit(param_string, "(?<!\\\\);", perl = TRUE)[[1]]
    params <- list()

    for (pair in param_pairs) {
      # Split on the first unescaped colon. Avoid strsplit() here because R
      # drops trailing empty fields (e.g. "replacement:"), which is a valid
      # empty replacement value.
      separator <- regexpr("(?<!\\\\):", pair, perl = TRUE)

      if (separator > 0) {
        param_name <- substr(pair, 1, separator - 1)
        param_value <- substr(pair, separator + attr(separator, "match.length"), nchar(pair))

        # Unescape special characters
        param_value <- gsub("\\\\:", ":", param_value, fixed = TRUE)
        param_value <- gsub("\\\\;", ";", param_value, fixed = TRUE)

        # Convert value back to appropriate type
        if (param_value == "NULL") {
          params[[param_name]] <- NULL
        } else if (param_value == "TRUE") {
          params[[param_name]] <- TRUE
        } else if (param_value == "FALSE") {
          params[[param_name]] <- FALSE
        } else if (!is.na(suppressWarnings(as.numeric(param_value)))) {
          params[[param_name]] <- as.numeric(param_value)
        } else {
          params[[param_name]] <- param_value
        }
      }
    }

    return(params)

  }, error = function(e) {
    return(list())
  })
}

############
# Operations Table Validation

#' Validate operations table for robustness with import support
#'
#' @param operations_df data.frame with operations
#' @param available_columns character vector of available column names
#' @param reset_executed logical whether to reset Executed column to FALSE
#' @param debug_log function for debug output
#' @return list with cleaned operations and validation summary
validate_operations_table <- function(operations_df, available_columns, reset_executed = TRUE, debug_log = function(...) {}) {
  if (is.null(operations_df) || nrow(operations_df) == 0) {
    debug_log("Empty operations table - returning default structure", 2)
    return(list(
      operations = data.frame(
        Operation = character(),
        Type = character(),
        Columns = character(),
        Parameters = character(),
        Description = character(),
        Executed = logical(),
        stringsAsFactors = FALSE
      ),
      success = TRUE,
      removed_count = 0,
      warnings = character(0)
    ))
  }

  tryCatch({
    debug_log(paste("Validating operations table with", nrow(operations_df), "operations"), 2)

    # Check required columns
    required_cols <- c("Operation", "Type", "Columns", "Parameters", "Description")
    missing_cols <- setdiff(required_cols, names(operations_df))

    if (length(missing_cols) > 0) {
      error_msg <- paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      debug_log(error_msg, 1)
      return(list(
        operations = data.frame(
          Operation = character(),
          Type = character(),
          Columns = character(),
          Parameters = character(),
          Description = character(),
          Executed = logical(),
          stringsAsFactors = FALSE
        ),
        success = FALSE,
        removed_count = nrow(operations_df),
        warnings = error_msg
      ))
    }

    # Add Executed column if missing (for backwards compatibility)
    if (!"Executed" %in% names(operations_df)) {
      operations_df$Executed <- FALSE
      debug_log("Added Executed column to imported operations", 2)
    }

    # Reset Executed column to FALSE if requested
    if (reset_executed) {
      operations_df$Executed <- FALSE
      debug_log("Reset all Executed flags to FALSE", 2)
    }

    # Validate each operation with enhanced error tracking
    valid_operations <- list()
    removed_count <- 0
    warnings <- character()
    validation_details <- list()

    for (i in seq_len(nrow(operations_df))) {
      operation <- operations_df[i, ]
      operation_valid <- TRUE
      operation_warnings <- character()

      # Parse columns from operation
      columns_str <- operation$Columns
      if (is.na(columns_str) || !nzchar(columns_str)) {
        removed_count <- removed_count + 1
        warning_msg <- paste("Operation", i, "removed: no columns specified")
        warnings <- c(warnings, warning_msg)
        debug_log(warning_msg, 2)
        operation_valid <- FALSE
      } else {
        # Split columns and check availability
        operation_columns <- strsplit(columns_str, "\\|")[[1]]
        existing_columns <- intersect(operation_columns, available_columns)
        missing_columns <- setdiff(operation_columns, available_columns)

        if (length(existing_columns) == 0) {
          # All columns missing - remove operation
          removed_count <- removed_count + 1
          warning_msg <- paste("Operation", i, "removed: all columns missing -", paste(missing_columns, collapse = ", "))
          warnings <- c(warnings, warning_msg)
          debug_log(warning_msg, 2)
          operation_valid <- FALSE
        } else if (length(missing_columns) > 0) {
          # Some columns missing - update operation to only include existing columns
          operation$Columns <- paste(existing_columns, collapse = "|")
          operation$Description <- paste0(operation$Description,
                                          " [Updated: removed missing columns: ",
                                          paste(missing_columns, collapse = ", "), "]")
          operation_warnings <- c(operation_warnings,
                                  paste("Removed missing columns:", paste(missing_columns, collapse = ", ")))
        }
      }

      # Validate operation type and parameters if operation is still valid
      if (operation_valid) {
        if (!operation$Operation %in% c("Replace", "Edit")) {
          removed_count <- removed_count + 1
          warning_msg <- paste("Operation", i, "removed: invalid operation type -", operation$Operation)
          warnings <- c(warnings, warning_msg)
          debug_log(warning_msg, 2)
          operation_valid <- FALSE
        }

        if (!operation$Type %in% c("character", "numeric")) {
          removed_count <- removed_count + 1
          warning_msg <- paste("Operation", i, "removed: invalid type -", operation$Type)
          warnings <- c(warnings, warning_msg)
          debug_log(warning_msg, 2)
          operation_valid <- FALSE
        }

        # Test parameter deserialization
        if (operation_valid) {
          test_params <- deserialize_parameters(operation$Parameters)
          if (length(test_params) == 0 && nzchar(operation$Parameters)) {
            operation_warnings <- c(operation_warnings, "Parameters may be malformed")
          }
        }
      }

      # Store validation details for debugging
      validation_details[[i]] <- list(
        valid = operation_valid,
        warnings = operation_warnings
      )

      if (operation_valid) {
        # Ensure Executed column is logical
        operation$Executed <- as.logical(operation$Executed)
        if (is.na(operation$Executed)) operation$Executed <- FALSE

        # Add warnings to operation warnings list
        if (length(operation_warnings) > 0) {
          warnings <- c(warnings, paste("Operation", i, "warnings:", paste(operation_warnings, collapse = "; ")))
        }

        # Operation passed validation
        valid_operations[[length(valid_operations) + 1]] <- operation
      }
    }

    # Reconstruct data.frame
    if (length(valid_operations) > 0) {
      validated_df <- do.call(rbind, valid_operations)
      rownames(validated_df) <- NULL
    } else {
      validated_df <- data.frame(
        Operation = character(),
        Type = character(),
        Columns = character(),
        Parameters = character(),
        Description = character(),
        Executed = logical(),
        stringsAsFactors = FALSE
      )
    }

    debug_log(paste("Validation complete:", nrow(validated_df), "valid operations,", removed_count, "removed"), 1)

    return(list(
      operations = validated_df,
      success = TRUE,
      removed_count = removed_count,
      warnings = warnings,
      validation_details = validation_details
    ))

  }, error = function(e) {
    error_msg <- paste("Validation error:", e$message)
    debug_log(error_msg, 1)
    return(list(
      operations = data.frame(
        Operation = character(),
        Type = character(),
        Columns = character(),
        Parameters = character(),
        Description = character(),
        Executed = logical(),
        stringsAsFactors = FALSE
      ),
      success = FALSE,
      removed_count = 0,
      warnings = error_msg
    ))
  })
}

############
# Data Manipulation Functions

#' Apply string-based replacement operations
#'
#' @param data_vector character vector to process
#' @param search_type character: "is equal", "starts with", "ends with", "contains"
#' @param search_term character: term to search for
#' @param replace_type character: "Replace cell", "Replace substring", or "Clear cell"
#' @param replacement character: replacement text
#' @return list with modified data and operation details
apply_string_replacement <- function(data_vector, search_type, search_term, replace_type, replacement) {
  result <- list(
    data = data_vector,
    success = FALSE,
    matches_found = 0,
    operation_description = ""
  )

  tryCatch({
    # Convert to character if needed
    char_data <- as.character(data_vector)

    # Find matching positions based on search type
    matches <- switch(search_type,
                      "is equal" = char_data == search_term,
                      "starts with" = startsWith(char_data, search_term),
                      "ends with" = endsWith(char_data, search_term),
                      "contains" = grepl(search_term, char_data, fixed = TRUE),
                      rep(FALSE, length(char_data))
    )

    # Handle NA values in matches
    matches[is.na(matches)] <- FALSE
    matches_count <- sum(matches, na.rm = TRUE)

    if (matches_count > 0) {
      if (replace_type == "Replace cell") {
        # Replace entire cell content
        char_data[matches] <- replacement
      } else if (replace_type == "Clear cell") {
        # Clear the entire cell content without requiring replacement text
        char_data[matches] <- ""
      } else if (replace_type == "Replace substring") {
        # Replace only the substring
        if (search_type == "contains") {
          char_data[matches] <- gsub(search_term, replacement, char_data[matches], fixed = TRUE)
        } else {
          # For other search types, replace the matching part
          char_data[matches] <- switch(search_type,
                                       "is equal" = replacement,
                                       "starts with" = paste0(replacement, substring(char_data[matches], nchar(search_term) + 1)),
                                       "ends with" = paste0(substring(char_data[matches], 1, nchar(char_data[matches]) - nchar(search_term)), replacement),
                                       char_data[matches]
          )
        }
      }
    }

    result$data <- char_data
    result$success <- TRUE
    result$matches_found <- matches_count
    replacement_description <- if (replace_type == "Clear cell") {
      "<empty>"
    } else {
      paste0("'", replacement, "'")
    }
    result$operation_description <- paste0(
      "String replacement: ", search_type, " '", search_term, "' → ", replacement_description,
      " (", replace_type, ") - ", matches_count, " matches"
    )

  }, error = function(e) {
    result$operation_description <- paste("String replacement failed:", e$message)
  })

  return(result)
}

#' Apply numeric-based replacement operations with robust error handling
#'
#' @param data_vector numeric vector to process
#' @param operator character: comparison operator
#' @param threshold numeric: comparison value
#' @param replace_with character: "NA" or "Numeric"
#' @param replacement_value numeric: value to replace with (if replace_with is "Numeric")
#' @return list with modified data and operation details
apply_numeric_replacement <- function(data_vector, operator, threshold, replace_with, replacement_value = NULL) {
  result <- list(
    data = data_vector,
    success = FALSE,
    matches_found = 0,
    operation_description = ""
  )

  tryCatch({
    # Ensure numeric data
    conversion_result <- safe_numeric_conversion(data_vector)
    if (!conversion_result$success) {
      result$operation_description <- paste("Numeric replacement failed:", conversion_result$errors)
      return(result)
    }

    numeric_data <- conversion_result$data

    # Operator mapping - UI sends "=" but we need "=="
    corrected_operator <- switch(operator,
                                 "=" = "==",
                                 "!=" = "!=",
                                 "<" = "<",
                                 "<=" = "<=",
                                 ">" = ">",
                                 ">=" = ">=",
                                 operator  # fallback to original
    )

    # Apply comparison with corrected operator
    matches <- switch(corrected_operator,
                      "<" = numeric_data < threshold,
                      "<=" = numeric_data <= threshold,
                      "==" = numeric_data == threshold,
                      "!=" = numeric_data != threshold,
                      ">=" = numeric_data >= threshold,
                      ">" = numeric_data > threshold,
                      rep(FALSE, length(numeric_data))
    )

    # Handle NA values in matches
    matches[is.na(matches)] <- FALSE
    matches_count <- sum(matches, na.rm = TRUE)

    if (matches_count > 0) {
      if (replace_with == "NA") {
        numeric_data[matches] <- NA
      } else if (replace_with == "Numeric" && !is.null(replacement_value)) {
        numeric_data[matches] <- replacement_value
      }
    }

    result$data <- numeric_data
    result$success <- TRUE
    result$matches_found <- matches_count
    result$operation_description <- paste0(
      "Numeric replacement: values ", operator, " ", threshold, " → ",
      ifelse(replace_with == "NA", "NA", replacement_value),
      " - ", matches_count, " matches"
    )

  }, error = function(e) {
    result$operation_description <- paste("Numeric replacement failed:", e$message)
  })

  return(result)
}

#' Apply string editing operations (prefix/suffix)
#'
#' @param data_vector character vector to process
#' @param edit_text character: text to add
#' @param position character: "Before" or "After"
#' @return list with modified data and operation details
apply_string_edit <- function(data_vector, edit_text, position) {
  result <- list(
    data = data_vector,
    success = FALSE,
    operation_description = ""
  )

  tryCatch({
    char_data <- as.character(data_vector)

    if (position == "Before") {
      modified_data <- paste0(edit_text, char_data)
    } else if (position == "After") {
      modified_data <- paste0(char_data, edit_text)
    } else {
      result$operation_description <- "Invalid position specified for string edit"
      return(result)
    }

    result$data <- modified_data
    result$success <- TRUE
    result$operation_description <- paste0(
      "String edit: added '", edit_text, "' ", tolower(position), " existing values"
    )

  }, error = function(e) {
    result$operation_description <- paste("String edit failed:", e$message)
  })

  return(result)
}

#' Apply mathematical operations to numeric data with robust error handling
#'
#' @param data_vector numeric vector to process
#' @param operation character: mathematical operation
#' @param value numeric: operation value
#' @param base numeric: base for logarithmic operations (optional)
#' @return list with modified data and operation details
apply_numeric_edit <- function(data_vector, operation, value = NULL, base = NULL) {
  result <- list(
    data = data_vector,
    success = FALSE,
    operation_description = ""
  )

  tryCatch({
    # Ensure numeric data
    conversion_result <- safe_numeric_conversion(data_vector)
    if (!conversion_result$success) {
      result$operation_description <- paste("Numeric edit failed:", conversion_result$errors)
      return(result)
    }

    numeric_data <- conversion_result$data
    original_na_count <- sum(is.na(numeric_data))

    # Apply mathematical operation
    modified_data <- switch(operation,
                            "Add" = {
                              if (is.null(value)) stop("Value required for addition")
                              numeric_data + value
                            },
                            "Subtract" = {
                              if (is.null(value)) stop("Value required for subtraction")
                              numeric_data - value
                            },
                            "Multiply" = {
                              if (is.null(value)) stop("Value required for multiplication")
                              numeric_data * value
                            },
                            "Divide" = {
                              if (is.null(value)) stop("Value required for division")
                              if (value == 0) stop("Cannot divide by zero")
                              numeric_data / value
                            },
                            "log" = {
                              if (is.null(base)) base <- exp(1)  # Natural log by default
                              if (base <= 0 || base == 1) stop("Invalid logarithm base")
                              # Handle non-positive values
                              ifelse(numeric_data > 0, log(numeric_data, base = base), NA)
                            },
                            "-log" = {
                              if (is.null(base)) base <- exp(1)  # Natural log by default
                              if (base <= 0 || base == 1) stop("Invalid logarithm base")
                              # Handle non-positive values
                              ifelse(numeric_data > 0, -log(numeric_data, base = base), NA)
                            },
                            "raise to the power of" = {
                              if (is.null(value)) stop("Exponent required for power operation")
                              # Handle potential overflow/underflow
                              result_power <- numeric_data^value
                              # Check for infinite or very large values
                              ifelse(is.finite(result_power), result_power, NA)
                            },
                            {
                              stop("Unknown operation: ", operation)
                            }
    )

    # Check for new infinite or NaN values
    new_invalid_count <- sum(!is.finite(modified_data)) - original_na_count

    result$data <- modified_data
    result$success <- TRUE

    # Create operation description
    op_desc <- switch(operation,
                      "Add" = paste0("added ", value),
                      "Subtract" = paste0("subtracted ", value),
                      "Multiply" = paste0("multiplied by ", value),
                      "Divide" = paste0("divided by ", value),
                      "log" = paste0("log base ", base),
                      "-log" = paste0("negative log base ", base),
                      "raise to the power of" = paste0("raised to power ", value),
                      operation
    )

    result$operation_description <- paste0("Numeric edit: ", op_desc)

    if (new_invalid_count > 0) {
      result$operation_description <- paste0(
        result$operation_description,
        " (", new_invalid_count, " values became invalid)"
      )
    }

  }, error = function(e) {
    result$operation_description <- paste("Numeric edit failed:", e$message)
  })

  return(result)
}

#' Apply operations to multiple columns
#'
#' @param data_df data.frame containing the columns
#' @param column_names character vector of column names to process
#' @param operation_function function to apply to each column
#' @param ... additional arguments passed to operation_function
#' @return list with updated data.frame and operation summary
apply_multi_column_operation <- function(data_df, column_names, operation_function, ...) {
  result <- list(
    data = data_df,
    success = FALSE,
    operation_summaries = list(),
    overall_summary = ""
  )

  tryCatch({
    updated_data <- data_df
    successful_operations <- 0
    failed_operations <- 0
    total_matches <- 0

    # Apply operation to each column
    for (col_name in column_names) {
      if (!col_name %in% names(updated_data)) {
        result$operation_summaries[[col_name]] <- paste("Column", col_name, "not found in data")
        failed_operations <- failed_operations + 1
        next
      }

      # Apply operation to this column
      col_result <- operation_function(updated_data[[col_name]], ...)

      if (col_result$success) {
        updated_data[[col_name]] <- col_result$data
        result$operation_summaries[[col_name]] <- col_result$operation_description
        successful_operations <- successful_operations + 1

        # Add matches if available
        if (!is.null(col_result$matches_found)) {
          total_matches <- total_matches + col_result$matches_found
        }
      } else {
        result$operation_summaries[[col_name]] <- col_result$operation_description
        failed_operations <- failed_operations + 1
      }
    }

    result$data <- updated_data
    result$success <- successful_operations > 0

    # Create overall summary
    if (successful_operations > 0) {
      result$overall_summary <- paste0(
        successful_operations, " of ", length(column_names), " columns processed"
      )

      if (total_matches > 0) {
        result$overall_summary <- paste0(result$overall_summary, " (", total_matches, " total matches)")
      }

      if (failed_operations > 0) {
        result$overall_summary <- paste0(result$overall_summary, ", ", failed_operations, " failed")
      }
    } else {
      result$overall_summary <- paste0("All operations failed on ", length(column_names), " columns")
    }

  }, error = function(e) {
    result$overall_summary <- paste("Multi-column operation error:", e$message)
  })

  return(result)
}

#' Apply a single operation from the operations table to data
#'
#' @param data_df current dataset
#' @param operation single row from operations table
#' @param debug_log function for debug output
#' @return list with success status, modified data, and message
apply_single_operation <- function(data_df, operation, debug_log = function(...) {}) {
  result <- list(
    success = FALSE,
    data = data_df,
    message = ""
  )

  tryCatch({
    debug_log(paste("Applying operation:", operation$Operation, operation$Type), 2)

    # Enhanced parameter parsing
    params <- deserialize_parameters(operation$Parameters)

    if (length(params) == 0 && nzchar(operation$Parameters)) {
      # Fallback to old parsing method for backwards compatibility
      param_pairs <- strsplit(operation$Parameters, ";")[[1]]
      for (pair in param_pairs) {
        parts <- strsplit(pair, ":")[[1]]
        if (length(parts) == 2) {
          param_name <- parts[1]
          param_value <- parts[2]

          # Convert value back to appropriate type
          if (param_value == "NULL") {
            params[[param_name]] <- NULL
          } else if (param_value == "TRUE") {
            params[[param_name]] <- TRUE
          } else if (param_value == "FALSE") {
            params[[param_name]] <- FALSE
          } else if (!is.na(suppressWarnings(as.numeric(param_value)))) {
            params[[param_name]] <- as.numeric(param_value)
          } else {
            params[[param_name]] <- param_value
          }
        }
      }
    }

    # Parse columns
    columns <- strsplit(operation$Columns, "\\|")[[1]]
    debug_log(paste("Target columns:", length(columns)), 2)

    # Check if columns exist
    missing_columns <- setdiff(columns, names(data_df))
    if (length(missing_columns) > 0) {
      result$message <- paste("Missing columns:", paste(missing_columns, collapse = ", "))
      debug_log(result$message, 1)
      return(result)
    }

    # Apply operation based on type with enhanced error handling
    if (operation$Operation == "Replace") {
      if (operation$Type == "character") {
        # Validate required parameters
        replace_type <- params$replace_type
        required_params <- c("search_type", "search_term", "replace_type")
        if (!identical(replace_type, "Clear cell")) {
          required_params <- c(required_params, "replacement")
        }
        missing_params <- setdiff(required_params, names(params))
        if (length(missing_params) > 0) {
          result$message <- paste("Missing parameters for character replacement:", paste(missing_params, collapse = ", "))
          return(result)
        }
        if (!("replacement" %in% names(params))) {
          params$replacement <- ""
        }

        operation_result <- apply_multi_column_operation(
          data_df, columns, apply_string_replacement,
          params$search_type, params$search_term, params$replace_type, params$replacement
        )
      } else if (operation$Type == "numeric") {
        # Validate required parameters
        required_params <- c("operator", "threshold", "replace_with")
        missing_params <- setdiff(required_params, names(params))
        if (length(missing_params) > 0) {
          result$message <- paste("Missing parameters for numeric replacement:", paste(missing_params, collapse = ", "))
          return(result)
        }

        operation_result <- apply_multi_column_operation(
          data_df, columns, apply_numeric_replacement,
          params$operator, params$threshold, params$replace_with, params$replacement_value
        )
      } else {
        result$message <- paste("Unknown replacement type:", operation$Type)
        debug_log(result$message, 1)
        return(result)
      }
    } else if (operation$Operation == "Edit") {
      if (operation$Type == "character") {
        # Validate required parameters
        required_params <- c("edit_text", "position")
        missing_params <- setdiff(required_params, names(params))
        if (length(missing_params) > 0) {
          result$message <- paste("Missing parameters for character edit:", paste(missing_params, collapse = ", "))
          return(result)
        }

        operation_result <- apply_multi_column_operation(
          data_df, columns, apply_string_edit,
          params$edit_text, params$position
        )
      } else if (operation$Type == "numeric") {
        # Validate required parameters for numeric edit
        if (!"operation" %in% names(params)) {
          result$message <- "Missing 'operation' parameter for numeric edit"
          return(result)
        }

        operation_result <- apply_multi_column_operation(
          data_df, columns, apply_numeric_edit,
          params$operation, params$value, params$base
        )
      } else {
        result$message <- paste("Unknown edit type:", operation$Type)
        debug_log(result$message, 1)
        return(result)
      }
    } else {
      result$message <- paste("Unknown operation:", operation$Operation)
      debug_log(result$message, 1)
      return(result)
    }

    # Check operation result
    if (operation_result$success) {
      result$success <- TRUE
      result$data <- operation_result$data
      result$message <- operation_result$overall_summary
      debug_log(paste("Operation completed:", result$message), 2)
    } else {
      result$message <- operation_result$overall_summary
      debug_log(paste("Operation failed:", result$message), 1)
    }

    return(result)

  }, error = function(e) {
    result$message <- paste("Error applying operation:", e$message)
    debug_log(result$message, 1)
    return(result)
  })
}

#' Safely convert data to numeric with error reporting
#'
#' @param data_vector vector to convert
#' @return list with converted data and success status
safe_numeric_conversion <- function(data_vector) {
  result <- list(
    data = data_vector,
    success = FALSE,
    errors = "",
    conversion_rate = 0
  )

  tryCatch({
    if (is.numeric(data_vector)) {
      result$data <- data_vector
      result$success <- TRUE
      result$conversion_rate <- 1.0
    } else {
      original_length <- length(data_vector)
      non_na_count <- sum(!is.na(data_vector))

      converted <- suppressWarnings(as.numeric(as.character(data_vector)))
      successful_conversions <- sum(!is.na(converted) & !is.na(data_vector))

      result$data <- converted
      result$success <- TRUE
      result$conversion_rate <- if (non_na_count > 0) successful_conversions / non_na_count else 0

      if (result$conversion_rate < 0.8 && non_na_count > 0) {
        result$errors <- paste("Low conversion rate:", round(result$conversion_rate * 100, 1), "%")
      }
    }

    return(result)

  }, error = function(e) {
    result$errors <- e$message
    return(result)
  })
}
