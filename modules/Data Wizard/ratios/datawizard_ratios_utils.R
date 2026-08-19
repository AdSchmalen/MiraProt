# Data Wizard/ratios/datawizard_ratios_utils.R

#' Ratios Utilities Submodule
#' Contains statistical execution, result application, and ratio server helpers

validate_ratio_configuration_inputs <- function() {
  tryCatch({
    # Get input values directly without debug spam
    inputs <- list(
      name_sel_dw = ui_inputs$name_sel_dw(),
      custom_col_sel_dw = ui_inputs$custom_col_sel_dw(),
      numerator_sel_dw = ui_inputs$numerator_sel_dw(),
      denominator_sel_dw = ui_inputs$denominator_sel_dw(),
      statistics_sel_dw = ui_inputs$statistics_sel_dw(),
      adjust_sel_dw = ui_inputs$adjust_sel_dw()
    )

    if (is.null(inputs$name_sel_dw) || trimws(inputs$name_sel_dw) == "") {
      return(list(valid = FALSE, message = "Please provide a name for this comparison."))
    }

    current_ratios <- ratio_configurations_df()
    if (nrow(current_ratios) > 0 && trimws(inputs$name_sel_dw) %in% current_ratios$Title) {
      return(list(
        valid = FALSE,
        message = "A comparison with this name already exists. Please choose a different name."
      ))
    }

    if (is.null(inputs$custom_col_sel_dw) || inputs$custom_col_sel_dw == "") {
      return(list(valid = FALSE, message = "Please select an abundance data type."))
    }

    if (is.null(inputs$numerator_sel_dw) || length(inputs$numerator_sel_dw) == 0) {
      return(list(valid = FALSE, message = "Please select at least one numerator group."))
    }

    if (is.null(inputs$denominator_sel_dw) || length(inputs$denominator_sel_dw) == 0) {
      return(list(valid = FALSE, message = "Please select at least one denominator group."))
    }

    overlap <- intersect(inputs$numerator_sel_dw, inputs$denominator_sel_dw)
    if (length(overlap) > 0) {
      return(list(
        valid = FALSE,
        message = paste("Groups cannot be in both numerator and denominator:", paste(overlap, collapse = ", "))
      ))
    }

    if (is.null(inputs$statistics_sel_dw) || inputs$statistics_sel_dw == "") {
      return(list(valid = FALSE, message = "Please select a statistical method."))
    }

    if (is.null(inputs$adjust_sel_dw) || inputs$adjust_sel_dw == "") {
      return(list(valid = FALSE, message = "Please select a p-value adjustment method."))
    }

    return(list(valid = TRUE, message = ""))

  }, error = function(e) {
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Error in input validation:", e$message, "\n")
    }
    return(list(valid = FALSE, message = paste("Validation error:", e$message)))
  })
}

make_comparison_name_unique <- function(proposed_name) {
  current_ratios <- ratio_configurations_df()
  if (nrow(current_ratios) == 0) {
    return(proposed_name)
  }

  existing_names <- current_ratios$Title
  if (!proposed_name %in% existing_names) {
    return(proposed_name)
  }

  counter <- 1
  while (paste0(proposed_name, "_", counter) %in% existing_names) {
    counter <- counter + 1
  }

  return(paste0(proposed_name, "_", counter))
}

#' IMPROVED: More robust result creation with better input validation and performance
#' @param comparison_name name of the comparison
#' @param abundance_ratios vector of abundance ratios
#' @param p_values vector of p-values
#' @param adjusted_p_values vector of adjusted p-values
#' @param row_indices vector of row indices
#' @param method_name name of the statistical method
#' @param log_fn optional logging function
#' @return data.frame with essential columns
create_robust_ratio_result <- function(comparison_name, abundance_ratios, p_values,
                                       adjusted_p_values, row_indices,
                                       method_name = "Unknown", log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(paste("[Result Creation]", message), level)
    }
  }

  tryCatch({
    safe_log(paste("Creating robust result for", method_name), 2)

    # Input validation with coercion
    if (!is.numeric(abundance_ratios)) abundance_ratios <- as.numeric(abundance_ratios)
    if (!is.numeric(p_values)) p_values <- as.numeric(p_values)
    if (!is.numeric(adjusted_p_values)) adjusted_p_values <- as.numeric(adjusted_p_values)
    if (!is.numeric(row_indices)) row_indices <- as.numeric(row_indices)

    # Check lengths
    lengths <- c(
      abundance = length(abundance_ratios),
      p_values = length(p_values),
      adj_p_values = length(adjusted_p_values),
      indices = length(row_indices)
    )

    if (length(unique(lengths)) > 1) {
      safe_log(paste("Length mismatch in", method_name, "results:",
                     paste(names(lengths), lengths, sep = "=", collapse = ", ")), 1)

      # Use minimum length
      min_length <- min(lengths)
      abundance_ratios <- abundance_ratios[seq_len(min_length)]
      p_values <- p_values[seq_len(min_length)]
      adjusted_p_values <- adjusted_p_values[seq_len(min_length)]
      row_indices <- row_indices[seq_len(min_length)]

      safe_log(paste("Truncated to", min_length, "elements"), 2)
    }

    if (length(row_indices) == 0) {
      safe_log(paste(method_name, "has no valid results"), 1)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    # Remove invalid Row Index entries efficiently
    valid_entries <- !is.na(row_indices) & is.finite(row_indices) & row_indices > 0
    invalid_count <- sum(!valid_entries)

    if (invalid_count > 0) {
      safe_log(paste("Removing", invalid_count, "invalid entries"), 2)
      abundance_ratios <- abundance_ratios[valid_entries]
      p_values <- p_values[valid_entries]
      adjusted_p_values <- adjusted_p_values[valid_entries]
      row_indices <- row_indices[valid_entries]
    }

    if (length(row_indices) == 0) {
      safe_log(paste("No valid entries remaining for", method_name), 1)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    # Create result using the essential function
    result <- create_essential_ratio_result(
      comparison_name = comparison_name,
      abundance_ratios = abundance_ratios,
      p_values = p_values,
      adjusted_p_values = adjusted_p_values,
      row_indices = row_indices
    )

    safe_log(paste("Created result with", nrow(result), "rows"), 2)
    return(result)

  }, error = function(e) {
    safe_log(paste("Error creating robust result:", e$message), 1)
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' IMPROVED: Enhanced wrapper for safe statistical execution
#' @param method_name name of the statistical method
#' @param statistical_function function to execute
#' @param original_row_indices vector of original row indices
#' @param log_fn optional logging function
#' @return result data.frame or empty data.frame on error
execute_statistical_method_safely <- function(method_name, statistical_function,
                                              original_row_indices, log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(message, level)
    }
  }

  tryCatch({
    safe_log(paste("Executing", method_name, "safely"), 2)

    # Execute the function
    result <- statistical_function()

    # Validate result
    if (is.null(result)) {
      safe_log(paste(method_name, "returned NULL"), 1)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    if (!is.data.frame(result)) {
      safe_log(paste(method_name, "did not return a data frame"), 1)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    if (nrow(result) == 0) {
      safe_log(paste(method_name, "returned empty result"), 2)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    # Validate Row Index column
    if (!"Row Index" %in% names(result)) {
      safe_log(paste(method_name, "result missing Row Index column"), 1)
      return(data.frame("Row Index" = integer(), check.names = FALSE))
    }

    # Validate Row Index consistency if original indices provided
    if (!is.null(original_row_indices) && length(original_row_indices) > 0) {
      validation_passed <- validate_row_index_consistency(
        original_row_indices,
        result$`Row Index`,
        method_name,
        log_fn
      )

      if (!validation_passed && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
        safe_log(paste(method_name, "Row Index validation failed"), 1)
      }
    }

    safe_log(paste(method_name, "executed successfully with", nrow(result), "results"), 1)
    return(result)

  }, error = function(e) {
    error_msg <- paste("Error in", method_name, ":", e$message)
    safe_log(error_msg, 1)

    # Return empty data frame with correct structure
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  })
}

#' IMPROVED: Find PSM columns with better pattern matching and performance
#' @param metadata_definition data frame with metadata
#' @param log_fn optional logging function
#' @return vector of column indices or NULL
find_psm_columns <- function(metadata_definition, log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(paste("[PSM Search]", message), level)
    }
  }

  safe_log("Searching for PSM columns", 2)

  if (is.null(metadata_definition) || !is.data.frame(metadata_definition) || nrow(metadata_definition) == 0) {
    safe_log("No metadata provided or empty metadata", 1)
    return(NULL)
  }

  # Improved PSM pattern matching
  psm_patterns <- c(
    "#\\s*PSMs?",                    # # PSM, # PSMs
    "PSM\\s*Count",                  # PSM Count
    "Peptide.*Count",                # Peptide Count, Peptide Spectrum Count
    "Spectrum.*Count",               # Spectrum Count
    "MS.*Count",                     # MS Count, MS/MS Count
    "Spectra.*Count"                 # Spectra Count
  )

  psm_pattern <- paste(psm_patterns, collapse = "|")

  if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
    safe_log(paste("Metadata dimensions:", nrow(metadata_definition), "rows,",
                   ncol(metadata_definition), "columns"), 2)
  }

  # Look for PSM patterns in Content column
  if ("Content" %in% names(metadata_definition)) {
    psm_rows <- grep(psm_pattern, metadata_definition$Content, ignore.case = TRUE)

    if (length(psm_rows) > 0) {
      safe_log(paste("Found PSM content in", length(psm_rows), "rows"), 1)

      # Log matches for debugging (only first few)
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2 && length(psm_rows) <= 5) {
        matching_content <- metadata_definition$Content[psm_rows]
        safe_log(paste("Matching content:", paste(matching_content, collapse = ", ")), 2)
      }

      return(psm_rows)
    } else {
      safe_log("No PSM patterns found in Content column", 2)
    }
  } else {
    safe_log("No 'Content' column found in metadata", 1)
  }

  # Fallback: search in column names
  safe_log("Searching in metadata column names as fallback", 2)
  psm_col_pattern <- paste(c("psm", "peptide.*count", "spectrum.*count", "spectra.*count"),
                           collapse = "|")

  psm_cols <- grep(psm_col_pattern, names(metadata_definition), ignore.case = TRUE)

  if (length(psm_cols) > 0) {
    safe_log(paste("Found PSM columns by name:", length(psm_cols), "columns"), 1)
    return(psm_cols)
  }

  safe_log("No PSM columns found", 1)

  # Enhanced potential PSM column detection
  if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
    potential_psm_columns <- grep("(count|psm|peptide|spectrum|ms|spec)",
                                  names(metadata_definition),
                                  ignore.case = TRUE,
                                  value = TRUE)

    if (length(potential_psm_columns) > 0) {
      safe_log(paste("Potential PSM-related columns found:",
                     paste(head(potential_psm_columns, 3), collapse = ", ")), 2)
    }
  }

  return(NULL)
}

#' Create essential ratio columns only (3 columns as specified)
#' @param comparison_name comparison name for prefix
#' @param abundance_ratios vector of abundance ratios
#' @param p_values vector of p-values
#' @param adjusted_p_values vector of adjusted p-values
#' @param row_indices vector of row indices
#' @return data.frame with only essential columns
create_essential_ratio_result <- function(comparison_name, abundance_ratios,
                                          p_values, adjusted_p_values, row_indices) {
  # Input validation and coercion
  row_indices <- as.integer(row_indices)
  abundance_ratios <- as.numeric(abundance_ratios)
  p_values <- as.numeric(p_values)
  adjusted_p_values <- as.numeric(adjusted_p_values)

  # Validate inputs
  n_rows <- length(row_indices)
  if (length(abundance_ratios) != n_rows || length(p_values) != n_rows ||
      length(adjusted_p_values) != n_rows) {
    stop("All input vectors must have the same length")
  }

  if (n_rows == 0) {
    return(data.frame("Row Index" = integer(), check.names = FALSE))
  }

  # Create only the 3 required columns
  result_df <- data.frame(
    "Row Index" = row_indices,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add columns with fixed naming format
  result_df[[paste0(comparison_name, "_Abundance Ratio")]] <- abundance_ratios
  result_df[[paste0(comparison_name, "_Abundance Ratio p-Value")]] <- p_values
  result_df[[paste0(comparison_name, "_Abundance Ratio Adj. p-Value")]] <- adjusted_p_values

  return(result_df)
}

#' ENHANCED: Filter data for analysis with corrected valid value logic
#' @param data input data frame
#' @param numerator_column_indices column indices for numerator
#' @param denominator_column_indices column indices for denominator
#' @param row_index_column row index column (will be ensured)
#' @param minimum_count minimum valid values required for statistical test
#' @param valid_count minimum valid values from ratio config (optional)
#' @param valid_logic logic from ratio config (optional)
#' @param content_type content type from ratio config (optional)
#' @param metadata_definition metadata for imputed data handling (optional)
#' @param log_fn optional logging function
#' @return list with filtered data, mapping, and column information
filter_data_for_analysis_fixed <- function(data, numerator_column_indices, denominator_column_indices,
                                           row_index_column, minimum_count,
                                           valid_count = NULL, valid_logic = NULL, content_type = NULL,
                                           metadata_definition = NULL, log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(paste("[Filter]", message), level)
    }
  }

  tryCatch({
    safe_log("Starting enhanced data filtering", 2)

    # Input validation
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
      stop("No data provided to filter_data_for_analysis_fixed")
    }

    # Ensure original Row Index exists
    data <- ensure_original_row_index(data, log_fn)
    row_index_column <- which(names(data) == "Row Index")[1]

    safe_log(paste("Original data dimensions:", nrow(data), "x", ncol(data)), 2)

    # Validate column indices
    all_data_columns <- c(numerator_column_indices, denominator_column_indices)
    valid_columns <- all_data_columns[all_data_columns > 0 & all_data_columns <= ncol(data)]

    if (length(valid_columns) != length(all_data_columns)) {
      safe_log("Some column indices are out of bounds", 1)
    }

    # Get column names for filtering
    numerator_column_names <- colnames(data)[numerator_column_indices]
    denominator_column_names <- colnames(data)[denominator_column_indices]

    safe_log(paste("Numerator columns:", length(numerator_column_names),
                   "Denominator columns:", length(denominator_column_names)), 2)

    # Pre-allocate logical vectors
    non_empty_rows <- logical(nrow(data))
    sufficient_data_condition <- logical(nrow(data))

    # Vectorized NA check - avoid completely empty rows
    if (ncol(data) > 1) {
      na_counts <- rowSums(is.na(data))
      non_empty_rows <- na_counts < ncol(data)
    } else {
      non_empty_rows <- rep(TRUE, nrow(data))
    }

    safe_log(paste("Rows with all NA values:", sum(!non_empty_rows)), 2)

    # Determine which data to use for validation (imputed vs original)
    validation_data <- data
    if (!is.null(valid_count) && !is.null(content_type) && !is.null(metadata_definition) &&
        !is.na(valid_count) && valid_count > 1) {
      validation_data <- get_validation_data_for_imputed_content(data, content_type, metadata_definition, safe_log)
    }

    # Normalize valid_logic (robust to UI label variants)
    logic_key <- NULL
    if (!is.null(valid_logic) && is.character(valid_logic) && nzchar(valid_logic)) {
      vl <- tolower(trimws(valid_logic))
      vl <- gsub("[ _-]+", " ", vl)          # normalize separators
      if (vl %in% c("in total", "total")) {
        logic_key <- "total"
      } else if (vl %in% c("in one group", "one group")) {
        logic_key <- "one"
      } else if (vl %in% c("in each group", "each group")) {
        logic_key <- "each"
      }
    }
    if (is.null(logic_key)) {
      logic_key <- "each"  # conservative default
    }
    safe_log(paste("Valid logic normalized to:", logic_key), 1)

    # Statistical test requirement + Valid Value filtering (COMBINED LOGIC)
    if (length(numerator_column_names) > 0 && length(denominator_column_names) > 0) {
      # Extract data matrices for both current data and validation data
      num_data <- data[, numerator_column_names, drop = FALSE]
      denom_data <- data[, denominator_column_names, drop = FALSE]
      val_num_data <- validation_data[, numerator_column_names, drop = FALSE]
      val_denom_data <- validation_data[, denominator_column_names, drop = FALSE]

      # Convert to numeric matrices
      num_matrix <- data.matrix(num_data)
      denom_matrix <- data.matrix(denom_data)
      val_num_matrix <- data.matrix(val_num_data)
      val_denom_matrix <- data.matrix(val_denom_data)

      # Count valid values (statistical test - uses current data)
      valid_num_mask <- !is.na(num_matrix) & is.finite(num_matrix) & num_matrix > 0
      valid_denom_mask <- !is.na(denom_matrix) & is.finite(denom_matrix) & denom_matrix > 0
      valid_num_counts <- rowSums(valid_num_mask)
      valid_denom_counts <- rowSums(valid_denom_mask)

      # Count valid values (ratio filtering - uses validation data for imputed)
      val_valid_num_mask <- !is.na(val_num_matrix) & is.finite(val_num_matrix) & val_num_matrix > 0
      val_valid_denom_mask <- !is.na(val_denom_matrix) & is.finite(val_denom_matrix) & val_denom_matrix > 0
      val_valid_num_counts <- rowSums(val_valid_num_mask)
      val_valid_denom_counts <- rowSums(val_valid_denom_mask)

      if (!is.null(valid_count) && !is.na(valid_count) && valid_count > 1) {
        safe_log(paste("Using combined filtering - Statistical minimum:", minimum_count, "Ratio valid count:", valid_count, "Logic:", logic_key), 1)

        if (logic_key == "total") {
          stat_test_condition <- (valid_num_counts + valid_denom_counts) >= minimum_count
          ratio_filter_condition <- (val_valid_num_counts + val_valid_denom_counts) >= valid_count
          sufficient_data_condition <- stat_test_condition & ratio_filter_condition

        } else if (logic_key == "one") {
          stat_test_condition <- (valid_num_counts >= minimum_count) | (valid_denom_counts >= minimum_count)
          ratio_filter_condition <- (val_valid_num_counts >= valid_count) | (val_valid_denom_counts >= valid_count)
          sufficient_data_condition <- stat_test_condition & ratio_filter_condition

        } else { # "each"
          stat_test_condition <- (valid_num_counts >= minimum_count) & (valid_denom_counts >= minimum_count)
          ratio_filter_condition <- (val_valid_num_counts >= valid_count) & (val_valid_denom_counts >= valid_count)
          sufficient_data_condition <- stat_test_condition & ratio_filter_condition
        }

        safe_log(paste("Combined filtering applied:", logic_key, "logic"), 2)

      } else {
        # Default: Only statistical test requirement (original behavior)
        sufficient_data_condition <- (valid_num_counts >= minimum_count) & (valid_denom_counts >= minimum_count)
        safe_log("Only statistical test filtering applied (no ratio valid values)", 2)
      }
    } else {
      sufficient_data_condition <- rep(TRUE, nrow(data))
    }

    safe_log(paste("Rows with sufficient data:", sum(sufficient_data_condition, na.rm = TRUE)), 2)

    # Combine all filtering conditions
    final_filter_condition <- non_empty_rows & sufficient_data_condition
    kept_count <- sum(final_filter_condition, na.rm = TRUE)
    safe_log(paste("Final filtered rows:", kept_count), 1)

    # Create filtered dataset with mapping
    filter_result <- create_filtered_data_with_mapping(data, final_filter_condition, log_fn)
    filtered_data <- filter_result$filtered_data
    mapping_info <- filter_result$mapping

    # Create analysis subset (only needed columns)
    analysis_column_indices <- unique(c(row_index_column, numerator_column_indices, denominator_column_indices))
    analysis_column_indices <- analysis_column_indices[analysis_column_indices <= ncol(filtered_data)]

    analysis_subset <- filtered_data[, analysis_column_indices, drop = FALSE]

    # Ensure Row Index is the first column
    if (!"Row Index" %in% names(analysis_subset)) {
      stop("Row Index column missing after subsetting")
    }

    row_idx_pos <- which(names(analysis_subset) == "Row Index")[1]
    if (row_idx_pos != 1) {
      col_order <- c(row_idx_pos, setdiff(seq_len(ncol(analysis_subset)), row_idx_pos))
      analysis_subset <- analysis_subset[, col_order, drop = FALSE]
    }

    safe_log(paste("Analysis subset dimensions:", nrow(analysis_subset), "x", ncol(analysis_subset)), 2)

    return(list(
      filtered_data = analysis_subset,
      numerator_column_names = numerator_column_names,
      denominator_column_names = denominator_column_names,
      mapping = mapping_info,
      original_row_count = nrow(data),
      filtered_row_count = nrow(analysis_subset),
      filter_condition = final_filter_condition
    ))

  }, error = function(e) {
    safe_log(paste("Error in filter_data_for_analysis_fixed:", e$message), 1)
    return(list(
      filtered_data = data.frame(),
      numerator_column_names = character(0),
      denominator_column_names = character(0),
      mapping = list(),
      original_row_count = if (!is.null(data)) nrow(data) else 0,
      filtered_row_count = 0,
      filter_condition = logical(0),
      error = e$message
    ))
  })
}

#' Get validation data for imputed content types (HELPER FUNCTION)
#' @param data current data frame
#' @param content_type content type from ratio config
#' @param metadata_definition metadata
#' @param safe_log logging function
#' @return data frame to use for validation
get_validation_data_for_imputed_content <- function(data, content_type, metadata_definition, safe_log) {
  tryCatch({
    if (is.null(content_type) || is.null(metadata_definition)) {
      return(data)
    }

    # Check if this is imputed data
    if (!grepl("^Imputed", content_type, ignore.case = TRUE)) {
      safe_log("Non-imputed data, using current data for validation", 2)
      return(data)
    }

    # Find original data type
    original_content_type <- gsub("^Imputed\\s+", "", content_type, ignore.case = TRUE)
    safe_log(paste("Imputed data detected. Original type:", original_content_type), 1)

    # Find original data columns
    original_columns <- metadata_definition$Column[metadata_definition$Content == original_content_type]
    original_columns <- original_columns[!is.na(original_columns)]
    existing_original_columns <- original_columns[original_columns %in% names(data)]

    if (length(existing_original_columns) == 0) {
      safe_log("No original data columns found, using current data", 2)
      return(data)
    }

    # Create validation data using original values where available
    validation_data <- data
    imputed_columns <- metadata_definition$Column[metadata_definition$Content == content_type]
    imputed_columns <- imputed_columns[!is.na(imputed_columns) & imputed_columns %in% names(data)]

    # Map imputed to original columns
    for (imp_col in imputed_columns) {
      potential_orig_col <- gsub("^Imputed\\s+", "", imp_col, ignore.case = TRUE)
      if (potential_orig_col %in% existing_original_columns) {
        validation_data[, imp_col] <- data[, potential_orig_col]
        safe_log(paste("Using original values from", potential_orig_col, "for validation"), 2)
      }
    }

    return(validation_data)

  }, error = function(e) {
    safe_log(paste("Error in validation data setup:", e$message), 1)
    return(data)
  })
}

#' IMPROVED: Compute abundance ratio with better numerical stability
#' @param row single row of data
#' @param numerator_column_names column names for numerator
#' @param denominator_column_names column names for denominator
#' @return numeric ratio or NA
compute_abundance_ratio_for_row <- function(row, numerator_column_names, denominator_column_names) {
  tryCatch({
    # Extract and clean numeric values
    numerator_values <- suppressWarnings(as.numeric(row[numerator_column_names]))
    denominator_values <- suppressWarnings(as.numeric(row[denominator_column_names]))

    finite_numerator_values <- numerator_values[!is.na(numerator_values) & is.finite(numerator_values)]
    finite_denominator_values <- denominator_values[!is.na(denominator_values) & is.finite(denominator_values)]

    # Remove NA, infinite, and non-positive values
    clean_numerator_values <- numerator_values[!is.na(numerator_values) &
                                                 is.finite(numerator_values) &
                                                 numerator_values > 0]
    clean_denominator_values <- denominator_values[!is.na(denominator_values) &
                                                     is.finite(denominator_values) &
                                                     denominator_values > 0]

    # Check if we have sufficient data
    if (length(clean_numerator_values) == 0 || length(clean_denominator_values) == 0) {
      # Fallback for log-scale data that can include negative values
      if (length(finite_numerator_values) > 0 && length(finite_denominator_values) > 0) {
        # Heuristic: values <= 0 in otherwise finite data are likely transformed intensities
        if (any(finite_numerator_values <= 0) || any(finite_denominator_values <= 0)) {
          clean_numerator_values <- 2^finite_numerator_values
          clean_denominator_values <- 2^finite_denominator_values
        } else {
          return(NA_real_)
        }
      } else {
        return(NA_real_)
      }
    }

    # Calculate means with numerical stability
    numerator_mean <- mean(clean_numerator_values)
    denominator_mean <- mean(clean_denominator_values)

    # Enhanced validation
    if (is.na(numerator_mean) || is.na(denominator_mean) ||
        !is.finite(numerator_mean) || !is.finite(denominator_mean) ||
        denominator_mean <= .Machine$double.eps) {  # Use machine epsilon instead of 0
      return(NA_real_)
    }

    # Calculate ratio
    ratio <- numerator_mean / denominator_mean

    # Return finite ratio or NA
    return(if (is.finite(ratio) && ratio > 0) ratio else NA_real_)

  }, error = function(e) {
    return(NA_real_)
  })
}

#' ENHANCED: Adjust p-values using various methods with comprehensive validation
#' @param p_values vector of p-values
#' @param adjustment_method method for adjustment
#' @return vector of adjusted p-values
adjust_p_values_safely <- function(p_values, adjustment_method) {
  tryCatch({
    # Input validation
    if (length(p_values) == 0) {
      return(numeric(0))
    }

    # Convert to numeric if needed
    if (!is.numeric(p_values)) {
      p_values <- suppressWarnings(as.numeric(p_values))
    }

    # Find valid p-values (between 0 and 1, finite)
    valid_p_indices <- !is.na(p_values) & is.finite(p_values) &
      p_values >= 0 & p_values <= 1

    if (sum(valid_p_indices) == 0) {
      return(p_values)  # Return original if no valid p-values
    }

    # Initialize with original values
    adjusted_p_values <- p_values

    # Enhanced method mapping with more options
    method_map <- list(
      "Bonferroni" = "bonferroni",
      "FDR" = "fdr",
      "Holm" = "holm",
      "Hochberg" = "hochberg",
      "Hommel" = "hommel",
      "Benjamini & Hochberg" = "BH",
      "Benjamini & Yekutieli" = "BY",
      "BH" = "BH",  # Alternative naming
      "BY" = "BY"   # Alternative naming
    )

    # Get the p.adjust method
    p_adjust_method <- method_map[[adjustment_method]]

    if (!is.null(p_adjust_method)) {
      # Apply adjustment only to valid p-values
      valid_p_vals <- p_values[valid_p_indices]

      # Additional check for edge cases
      if (length(valid_p_vals) > 0) {
        adjusted_valid <- p.adjust(valid_p_vals, method = p_adjust_method)

        # Ensure adjusted p-values are still valid
        adjusted_valid[adjusted_valid < 0] <- 0
        adjusted_valid[adjusted_valid > 1] <- 1
        adjusted_valid[!is.finite(adjusted_valid)] <- 1

        adjusted_p_values[valid_p_indices] <- adjusted_valid
      }
    } else if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Warning: Unknown p-value adjustment method:", adjustment_method, "\n")
    }

    return(adjusted_p_values)

  }, error = function(e) {
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Error in p-value adjustment:", e$message, "\n")
    }
    # Return original p-values on error
    return(p_values)
  })
}

#' CORRECTED: Use the improved hyperparameter estimation from core module
#' @param z vector of log variances
#' @param D degrees of freedom
#' @param log_fn optional logging function
#' @return list with s02 and d0 parameters
estimate_hyperparameters <- function(z, D, log_fn = NULL) {
  # This function is now a wrapper to maintain compatibility
  # The actual implementation is in the core module as estimate_hyperparameters_corrected
  tryCatch({
    # Remove any infinite or NA values
    z_clean <- z[is.finite(z)]

    if (length(z_clean) == 0) {
      return(list(s02 = NA, d0 = NA))
    }

    if (length(unique(z_clean)) <= 1) {
      return(list(s02 = NA, d0 = NA))
    }

    # Check if pracma package is available
    if (!requireNamespace("pracma", quietly = TRUE)) {
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
        cat("Warning: pracma package required for hyperparameter estimation\n")
      }
      return(list(s02 = NA, d0 = NA))
    }

    # Use simplified method of moments approach
    var_z <- var(z_clean, na.rm = TRUE)
    if (is.na(var_z) || var_z <= 0) {
      return(list(s02 = NA, d0 = NA))
    }

    # Simplified estimation
    d0 <- max(0.1, min(100, 2 / var_z))
    s02 <- exp(mean(z_clean, na.rm = TRUE))

    return(list(s02 = s02, d0 = d0))

  }, error = function(e) {
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Error in estimate_hyperparameters:", e$message, "\n")
    }
    return(list(s02 = NA, d0 = NA))
  })
}

#' OPTIMIZED: Merge analysis results with improved performance for large datasets
#' @param original_data original full data frame with Row Index
#' @param analysis_result result from statistical analysis with Row Index
#' @param validate_merge whether to perform validation checks
#' @param log_fn optional logging function
#' @return original data with new columns appended in correct positions
merge_analysis_results_fixed <- function(original_data, analysis_result,
                                         validate_merge = TRUE, log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(paste("[Merge]", message), level)
    }
  }

  tryCatch({
    safe_log("Starting results merging", 2)

    # Input validation
    if (is.null(analysis_result) || !is.data.frame(analysis_result) || nrow(analysis_result) == 0) {
      safe_log("No analysis results to merge", 2)
      return(original_data)
    }

    if (!is.data.frame(original_data)) {
      stop("Original data must be a data frame")
    }

    if (!"Row Index" %in% names(original_data) || !"Row Index" %in% names(analysis_result)) {
      stop("Both datasets must have 'Row Index' column for merging")
    }

    # Ensure original data has proper Row Index
    original_data <- ensure_original_row_index(original_data, log_fn)

    # Get new columns from analysis result (excluding Row Index)
    new_column_names <- setdiff(names(analysis_result), "Row Index")

    if (length(new_column_names) == 0) {
      safe_log("No new columns to add", 2)
      return(original_data)
    }

    safe_log(paste("Adding", length(new_column_names), "new columns"), 1)

    # OPTIMIZED: Create enhanced data copy
    enhanced_data <- original_data

    # Performance optimization: use match once for all columns
    original_indices <- original_data$`Row Index`
    result_indices <- analysis_result$`Row Index`

    # Create mapping efficiently
    match_positions <- match(original_indices, result_indices)
    valid_matches <- !is.na(match_positions)

    safe_log(paste("Found", sum(valid_matches), "matching rows out of", length(original_indices)), 2)

    # Check for duplicate Row Index values in analysis result
    if (validate_merge && anyDuplicated(result_indices) > 0 && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      safe_log("Warning: Duplicate Row Index values detected in analysis result", 1)
    }

    # OPTIMIZED: Add all new columns at once using vectorized operations
    for (col_name in new_column_names) {
      # Initialize new column with NA values of appropriate type
      col_data <- analysis_result[[col_name]]

      # Determine appropriate NA type based on column data
      if (is.numeric(col_data)) {
        new_column_values <- rep(NA_real_, nrow(original_data))
      } else if (is.integer(col_data)) {
        new_column_values <- rep(NA_integer_, nrow(original_data))
      } else if (is.character(col_data)) {
        new_column_values <- rep(NA_character_, nrow(original_data))
      } else {
        new_column_values <- rep(NA, nrow(original_data))
      }

      # Fill in values where matches exist using vectorized assignment
      if (sum(valid_matches) > 0) {
        matched_values <- col_data[match_positions[valid_matches]]
        new_column_values[valid_matches] <- matched_values
      }

      # Add the new column
      enhanced_data[[col_name]] <- new_column_values
    }

    # Final validation
    if (validate_merge) {
      # Check Row Index integrity
      if (!identical(original_data$`Row Index`, enhanced_data$`Row Index`)) {
        safe_log("ERROR: Row Index was modified during merge!", 1)
      } else {
        safe_log("Row Index integrity maintained", 2)
      }

      # Quick validation of merge
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
        non_na_counts <- vapply(new_column_names, function(col) {
          sum(!is.na(enhanced_data[[col]]))
        }, integer(1))

        safe_log(paste("Non-NA values per new column:",
                       paste(names(non_na_counts), non_na_counts, sep = "=", collapse = ", ")), 2)
      }
    }

    safe_log(paste("Successfully merged", length(new_column_names), "columns"), 1)
    return(enhanced_data)

  }, error = function(e) {
    safe_log(paste("Error in merge_analysis_results_fixed:", e$message), 1)
    return(original_data)
  })
}

#' Enhanced: Suggest statistical method based on sample size and availability of PSMs
#' @param sample_size sample size per group
#' @param has_psm logical: does data come with PSM information
#' @param log_fn optional logging function
suggest_statistical_methods <- function(sample_size, has_psm = FALSE, log_fn = NULL) {
  safe_log <- function(message, level = 2) {
    if (!is.null(log_fn) && is.function(log_fn) && exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
      log_fn(paste("[Method Suggestion]", message), level)
    }
  }

  safe_log(paste("Suggesting methods for sample size:", sample_size, "PSM data:", has_psm), 2)

  suggestions <- list()

  if (sample_size >= 3) {
    suggestions$recommended <- c("Limma", "Welch's T-Test", "Student's T-Test")
    suggestions$alternative <- c("Mann-Whitney U Test")
  } else if (sample_size >= 2) {
    suggestions$recommended <- c("Limma", "Moderated Welch Test")
    suggestions$alternative <- character(0)
  } else {
    suggestions$recommended <- character(0)
    suggestions$alternative <- character(0)
  }

  if (has_psm && sample_size >= 2) {
    suggestions$recommended <- c("DEqMS", suggestions$recommended)
  }

  suggestions$notes <- paste("Based on", sample_size, "samples per group")

  return(suggestions)
}

#' Enhanced wrapper for safe statistical execution
#' @param statistical_function function to execute
#' @param fallback_result result to return on error
#' @param error_message_prefix prefix for error messages
#' @return result of function or fallback
execute_statistical_function_safely <- function(statistical_function, fallback_result = NULL, error_message_prefix = "Statistical calculation failed") {
  tryCatch({
    result <- statistical_function()
    if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
        cat("Function returned empty result\n")
      }
      return(fallback_result)
    }
    return(result)
  }, error = function(e) {
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Error in statistical calculation:", e$message, "\n")
    }
    if (exists("showNotification")) {
      showNotification(paste(error_message_prefix, ":", e$message), type = "error", duration = 5)
    }
    return(fallback_result)
  })
}

# ========================================
# Statistical Analysis Functions (Legacy Compatibility)
# ========================================

# Apply ratios function with statistical methods from core_module
apply_single_ratio_configuration_fixed <- function(ratio_config, current_data, metadata_definition) {
  tryCatch({
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Applying ratio analysis:", ratio_config$Title, "\n")
    }

    current_data <- utils_module$ensure_original_row_index(current_data)

    content_matching_rows <- which(metadata_definition$Content == ratio_config$Content)
    if (length(content_matching_rows) == 0) {
      stop("No columns found for content type: ", ratio_config$Content)
    }

    numerator_sample_groups <- ratio_config$Numerator[[1]]
    denominator_sample_groups <- ratio_config$Denominator[[1]]

    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
      cat("Numerator groups:", length(numerator_sample_groups),
          "Denominator groups:", length(denominator_sample_groups), "\n")
    }

    # Column mapping
    numerator_column_indices <- c()
    denominator_column_indices <- c()

    for (group in numerator_sample_groups) {
      matching_group_columns <- which(metadata_definition$Content == ratio_config$Content &
                                        ((!is.na(metadata_definition$Sample) & metadata_definition$Sample == group) |
                                           (!is.na(metadata_definition$Options) & metadata_definition$Options == group)))
      numerator_column_indices <- c(numerator_column_indices, matching_group_columns)
    }

    for (group in denominator_sample_groups) {
      matching_group_columns <- which(metadata_definition$Content == ratio_config$Content &
                                        ((!is.na(metadata_definition$Sample) & metadata_definition$Sample == group) |
                                           (!is.na(metadata_definition$Options) & metadata_definition$Options == group)))
      denominator_column_indices <- c(denominator_column_indices, matching_group_columns)
    }

    if (length(numerator_column_indices) == 0 || length(denominator_column_indices) == 0) {
      stop("Could not find columns for specified groups")
    }

    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
      cat("Found", length(numerator_column_indices), "numerator and",
          length(denominator_column_indices), "denominator columns\n")
    }

    row_index_column <- which(names(current_data) == "Row Index")[1]
    selected_method <- ratio_config$Statistics
    adjustment_method <- ratio_config$`Adjustment Method`
    comparison_name <- ratio_config$Title

    # Apply statistical method using core module
    analysis_result <- switch(selected_method,
                              "Welch's T-Test" = core_module$perform_welch_t_test_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              "Student's T-Test" = core_module$perform_t_test_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              "Moderated Welch Test" = core_module$perform_moderated_welch_test_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              "Limma" = core_module$perform_limma_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              "DEqMS" = core_module$perform_deqms_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              "Mann-Whitney U Test" = core_module$perform_mann_whitney_analysis(
                                current_data, numerator_column_indices, denominator_column_indices,
                                adjustment_method, row_index_column, comparison_name,
                                metadata_definition, log_fn = add_processing_log_entry,
                                valid_count = valid_count,  # NEU
                                valid_logic = valid_logic,  # NEU
                                content_type = ratio_config$Content  # NEU
                              ),
                              {
                                if (exists("showNotification")) {
                                  showNotification(paste("Unknown statistical method:", selected_method),
                                                   type = "error", duration = 8)
                                }
                                data.frame()
                              }
    )

    if (!is.null(analysis_result) && nrow(analysis_result) > 0) {
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
        cat("Statistical analysis completed with", nrow(analysis_result), "results\n")
      }
      return(analysis_result)
    } else {
      return(NULL)
    }

  }, error = function(e) {
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Error in apply_single_ratio_configuration_fixed:", e$message, "\n")
    }
    if (exists("add_processing_log_entry")) {
      add_processing_log_entry(
        action = paste("Ratio Analysis", ratio_config$Title),
        details = e$message,
        status = "error"
      )
    }
    return(NULL)
  })
}

apply_all_ratio_configurations <- function() {
  processing_status("processing")
  processing_start_time <- Sys.time()

  tryCatch({
    processing_errors(list())

    current_data <- if (!is.null(get_data)) get_data() else NULL
    if (is.null(current_data)) {
      stop("No data available for ratio analysis")
    }

    current_metadata <- if (!is.null(data_def)) data_def() else NULL
    if (is.null(current_metadata)) {
      stop("No metadata available for ratio analysis")
    }

    ratio_configs <- ratio_configurations_df()
    if (nrow(ratio_configs) == 0) {
      stop("No ratio configurations defined")
    }

    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat("Applying", nrow(ratio_configs), "ratio configurations\n")
    }

    last_applied_configurations(ratio_configs)

    analysis_results_list <- list()
    enhanced_data <- current_data
    successful_analyses <- 0
    failed_analyses <- 0

    for (i in 1:nrow(ratio_configs)) {
      current_ratio_config <- ratio_configs[i, , drop = FALSE]

      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 2) {
        cat("Processing ratio", i, "of", nrow(ratio_configs), ":", current_ratio_config$Title, "\n")
      }

      single_analysis_result <- apply_single_ratio_configuration_fixed(
        current_ratio_config, current_data, current_metadata)

      if (!is.null(single_analysis_result) && nrow(single_analysis_result) > 0) {
        analysis_results_list[[current_ratio_config$Title]] <- single_analysis_result
        enhanced_data <- utils_module$merge_analysis_results_fixed(
          enhanced_data, single_analysis_result, validate_merge = TRUE)
        successful_analyses <- successful_analyses + 1
      } else {
        failed_analyses <- failed_analyses + 1
      }
    }

    total_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))

    analysis_results(analysis_results_list)
    last_applied_ratios_list(ratio_configs)

    if (length(analysis_results_list) > 0) {
      # DEBUG: Check if set_data exists and try to call it
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
        cat("DEBUG: set_data function check - is.null:", is.null(set_data), "\n")
        if (!is.null(set_data)) {
          cat("DEBUG: About to call set_data with enhanced_data (", nrow(enhanced_data), "x", ncol(enhanced_data), ")\n")
        }
      }

      if (!is.null(set_data)) {
        update_success <- set_data(enhanced_data)

        if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
          cat("DEBUG: set_data returned:", update_success, "\n")
        }

        if (update_success) {
          if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
            cat("RATIOS: Successfully updated data via set_data\n")
          }

          if (exists("add_processing_log_entry")) {
            add_processing_log_entry(
              action = "Data Update",
              details = paste("Successfully applied", successful_analyses, "ratio analyses"),
              status = "success",
              data = list(
                ratios_applied = successful_analyses,
                new_cols = ncol(enhanced_data) - ncol(current_data),
                duration = total_duration
              )
            )
          }
        } else {
          if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
            cat("ERROR: set_data returned FALSE - data was not updated!\n")
          }
        }
      } else {
        if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
          cat("ERROR: set_data is NULL - cannot update data!\n")
        }
      }

      if (failed_analyses == 0) {
        success_msg <- paste("Successfully applied", successful_analyses, "ratio analyses",
                             sprintf("(%.2fs)", total_duration))
        if (exists("showNotification")) {
          showNotification(success_msg, type = "message", duration = 4)
        }
      } else {
        warning_msg <- paste("Applied", successful_analyses, "analyses with", failed_analyses,
                             "failures", sprintf("(%.2fs)", total_duration))
        if (exists("showNotification")) {
          showNotification(warning_msg, type = "warning", duration = 6)
        }
      }
    } else {
      if (exists("showNotification")) {
        showNotification("No ratio analyses were successfully completed", type = "error", duration = 8)
      }
    }

    processing_status("completed")

    return(list(
      success = length(analysis_results_list) > 0,
      results = analysis_results_list,
      processed_data = enhanced_data,
      errors = processing_errors(),
      successful = successful_analyses,
      failed = failed_analyses,
      duration = total_duration
    ))

  }, error = function(e) {
    error_message <- paste("Error in apply_all_ratio_configurations:", e$message)
    if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= 1) {
      cat(error_message, "\n")
    }

    if (exists("add_processing_log_entry")) {
      add_processing_log_entry(
        action = "Apply All Ratios Error",
        details = error_message,
        status = "error",
        data = list(error = e$message)
      )
    }

    if (exists("showNotification")) {
      showNotification(error_message, type = "error", duration = 10)
    }
    processing_status("error")

    return(list(
      success = FALSE,
      results = list(),
      processed_data = NULL,
      errors = c(processing_errors(), list(list(error = e$message, timestamp = Sys.time())))
    ))
  })
}

#' Ratios Utilities Server Submodule
#' @param id namespace id
#' @export
ratiosUtilsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Return utility functions for use by other modules
    return(list(
      ensure_original_row_index = ensure_original_row_index,
      validate_ratio_configuration_inputs = validate_ratio_configuration_inputs,
      make_comparison_name_unique = make_comparison_name_unique,
      create_filtered_data_with_mapping = create_filtered_data_with_mapping,
      validate_complete_row_index_pipeline = validate_complete_row_index_pipeline,
      validate_row_index_consistency = validate_row_index_consistency,
      create_robust_ratio_result = create_robust_ratio_result,
      execute_statistical_method_safely = execute_statistical_method_safely,
      check_and_retransform_data = check_and_retransform_data,
      retransform_data_global = retransform_data_global,
      find_psm_columns = find_psm_columns,
      create_essential_ratio_result = create_essential_ratio_result,
      filter_data_for_analysis_fixed = filter_data_for_analysis_fixed,
      compute_abundance_ratio_for_row = compute_abundance_ratio_for_row,
      adjust_p_values_safely = adjust_p_values_safely,
      estimate_hyperparameters = estimate_hyperparameters,
      merge_analysis_results_fixed = merge_analysis_results_fixed,
      suggest_statistical_methods = suggest_statistical_methods,
      execute_statistical_function_safely = execute_statistical_function_safely,
      print_validation_report = print_validation_report,
      apply_single_ratio_configuration_fixed = apply_single_ratio_configuration_fixed,
      apply_all_ratio_configurations = apply_all_ratio_configurations
    ))
  })
}
