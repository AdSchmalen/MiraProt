# ==============================================================================
# File: modules/PCA/pca_module_utils.R
#
# Purpose:
#   Pure utility functions for the PCA/Dimension Reduction module. Contains
#   data preparation, validation, coordinate extraction, color helpers, axis
#   management, and protein search/filter logic.
#
# Architectural Role:
#   All functions in this file are stateless and Shiny-free. They accept plain
#   R objects as arguments and return plain R objects. This makes them
#   independently testable and reusable across the analysis pipeline and the
#   rendering pipeline. debug_log is accepted as an explicit parameter by
#   every function that performs logging.
#
# Structure:
#   1. Data validation and preprocessing
#      - validate_dimension_reduction_data()
#      - prepare_pca_analysis_data()
#   2. Analysis execution
#      - run_pca_analysis()
#      - run_umap_analysis()
#   3. Plot data construction
#      - create_plot_data()
#      - get_plot_coordinates()
#   4. Axis management
#      - manage_axis_choices()
#   5. Color utilities
#      - get_default_colors_for_items_pca()
#      - (palette helpers)
#   6. Protein search / filter
#      - get_filter_string_pca()
#
# Notes for future developers:
#   - Do not import Shiny or add reactive dependencies to this file.
#   - All functions that log must receive debug_log explicitly; do not use
#     cat() directly.
#   - If this file grows beyond ~600 lines, consider splitting by domain
#     (e.g., separate files for analysis, plot data, colors) while keeping
#     the same no-additional-files constraint in mind.
# ==============================================================================

# ========================================
# Data Preprocessing Functions
# ========================================

# Helper: find Options/Option column
get_option_col <- function(metadata) {
  cn <- colnames(metadata)
  hit <- which(tolower(cn) %in% c("options", "option"))
  if (length(hit) >= 1) return(cn[hit[1]])
  return(NULL)
}

# Helper: case-insensitive column resolver
get_col <- function(df, candidates) {
  cn <- colnames(df)
  for (cand in candidates) {
    hit <- which(tolower(cn) == tolower(cand))
    if (length(hit) >= 1) return(cn[hit[1]])
  }
  return(NULL)
}

# Helper: sanitize sample names for safe use as row names
sanitize_rownames <- function(x, prefix = "Row_") {
  x <- as.character(x)
  na_or_empty <- is.na(x) | x == ""
  if (any(na_or_empty)) {
    x[na_or_empty] <- paste0(prefix, which(na_or_empty))
  }
  make.unique(x)
}

# Extract conditions for samples based on metadata and selected data type.
# - Accepts either "prepared" metadata (has Sample + Condition) OR raw data_def (Content + Sample/Column + Options/Option)
# - Returns factor vector aligned with the provided sample_names
extract_conditions_for_samples <- function(metadata, sample_names, selected_data_type = NULL) {
  if (is.null(metadata) || length(sample_names) == 0) {
    return(factor(rep(NA_character_, length(sample_names))))
  }

  # 1) Preferred: prepared metadata with Condition + Sample
  cond_col   <- get_col(metadata, c("Condition"))
  sample_col <- get_col(metadata, c("Sample"))
  if (!is.null(cond_col) && !is.null(sample_col)) {
    idx  <- match(sample_names, metadata[[sample_col]])
    cond <- rep(NA_character_, length(sample_names))
    hit  <- !is.na(idx)
    if (any(hit)) cond[hit] <- as.character(metadata[[cond_col]][idx[hit]])
    return(factor(cond))
  }

  # 2) Raw data_def: filter by Content and map Sample/Column -> Options/Option
  opt_col     <- get_col(metadata, c("Options", "Option"))
  content_col <- get_col(metadata, c("Content"))
  key_col     <- get_col(metadata, c("Sample", "Column"))

  if (is.null(opt_col) || is.null(key_col)) {
    return(factor(rep(NA_character_, length(sample_names))))
  }

  md <- metadata
  if (!is.null(content_col) && !is.null(selected_data_type) && nzchar(selected_data_type)) {
    md <- md[trimws(md[[content_col]]) == trimws(selected_data_type), , drop = FALSE]
  }
  if (nrow(md) == 0) {
    return(factor(rep(NA_character_, length(sample_names))))
  }

  md_map <- md[, c(key_col, opt_col), drop = FALSE]
  md_map <- md_map[!is.na(md_map[[key_col]]) & md_map[[key_col]] != "", , drop = FALSE]
  md_map <- md_map[!duplicated(md_map[[key_col]]), , drop = FALSE]

  cond_map <- setNames(as.character(md_map[[opt_col]]), as.character(md_map[[key_col]]))
  cond <- unname(cond_map[sample_names])
  return(factor(as.character(cond)))
}


# Attach restored plot-data cache to a compact PCA/UMAP result object.
# The restored data_def/data_mod pair must be authoritative for restored plots;
# live rv$data_def is only a fallback when no valid restore cache is available.
pca_attach_cached_restore_metadata <- function(results, restore_cache) {
  if (!is.list(results)) return(results)
  if (!is.list(restore_cache) ||
      !inherits(restore_cache$data_mod, "data.frame") ||
      !inherits(restore_cache$data_def, "data.frame")) {
    return(results)
  }

  raw_metadata <- restore_cache$data_def
  selected_data_type <- results$selected_data_type %||% NULL
  selected_samples <- results$selected_samples %||% character()

  content_col <- get_col(raw_metadata, c("Content"))
  sample_col <- get_col(raw_metadata, c("Sample"))
  option_col <- get_col(raw_metadata, c("Options", "Option"))

  abundance_indices <- integer(0)
  if (!is.null(content_col) && !is.null(sample_col) &&
      !is.null(selected_data_type) && nzchar(as.character(selected_data_type)[1])) {
    if (length(selected_samples) > 0) {
      abundance_indices <- which(
        raw_metadata[[content_col]] == selected_data_type &
          raw_metadata[[sample_col]] %in% selected_samples &
          !is.na(raw_metadata[[sample_col]])
      )
    } else {
      abundance_indices <- which(
        raw_metadata[[content_col]] == selected_data_type &
          !is.na(raw_metadata[[sample_col]])
      )
    }
  }

  if (length(selected_samples) == 0 && !is.null(sample_col) && length(abundance_indices) > 0) {
    selected_samples <- as.character(raw_metadata[[sample_col]][abundance_indices])
  }

  sample_names <- results$point_names %||% NULL
  if ((is.null(sample_names) || length(sample_names) == 0) && !is.null(results$coordinates)) {
    sample_names <- rownames(results$coordinates)
  }
  if (is.null(sample_names) || length(sample_names) == 0) {
    sample_names <- selected_samples
  }

  restored_metadata <- results$metadata
  if (identical(results$comparison_target, "samples")) {
    existing_condition_count <- if (is.data.frame(restored_metadata) && "Condition" %in% names(restored_metadata)) {
      sum(!is.na(restored_metadata$Condition) & nzchar(as.character(restored_metadata$Condition)))
    } else 0L
    rebuilt_metadata <- data.frame(
      Sample = as.character(sample_names),
      DataType = as.character(selected_data_type %||% NA_character_)[1],
      stringsAsFactors = FALSE
    )

    # Match prepare_pca_analysis_data(): Condition comes from data_def$Options
    # at the abundance column indices selected by Content/Sample.
    if (length(abundance_indices) > 0 && !is.null(option_col)) {
      condition_info <- raw_metadata[[option_col]][abundance_indices]
      condition_map <- setNames(as.character(condition_info), as.character(raw_metadata[[sample_col]][abundance_indices]))
      rebuilt_metadata$Condition <- unname(condition_map[rebuilt_metadata$Sample])
    } else {
      rebuilt_metadata$Condition <- as.character(extract_conditions_for_samples(
        metadata = raw_metadata,
        sample_names = rebuilt_metadata$Sample,
        selected_data_type = selected_data_type
      ))
    }
    rebuilt_condition_count <- sum(!is.na(rebuilt_metadata$Condition) & nzchar(as.character(rebuilt_metadata$Condition)))
    # Compact PCA results already persist prepared metadata with Condition. If
    # the cached raw metadata cannot reconstruct those conditions, keep the more
    # informative saved metadata while still attaching cached raw_metadata/full_data.
    restored_metadata <- if (existing_condition_count > rebuilt_condition_count) restored_metadata else rebuilt_metadata
  }

  results$full_data <- restore_cache$data_mod
  results$raw_metadata <- raw_metadata
  results$metadata <- restored_metadata
  results$selected_data_type <- selected_data_type
  results$selected_samples <- selected_samples
  results
}

find_condition_column <- function(metadata) {
  opt_col <- get_option_col(metadata)
  if (is.null(opt_col)) return(NULL)
  if (!"Content" %in% colnames(metadata)) return(opt_col)

  sample_rows <- grepl(get_sample_content_pattern(), metadata$Content, ignore.case = FALSE)
  uniq <- unique(stats::na.omit(metadata[[opt_col]][sample_rows]))
  if (length(uniq) >= 1) return(opt_col)
  return(NULL)
}

#' Impute missing values in data matrix (FIXED VERSION)
#' @param data_matrix Numeric matrix with possible NAs
#' @param method Imputation method
#' @return Matrix with imputed values
impute_missing_values <- function(data_matrix, method = "remove", debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(...) invisible(NULL)
  if (method == "remove") {
    debug_log("Starting missing-value removal", 2)
  } else {
    debug_log("Starting missing value imputation", 2)
  }

  # ========================================
  # CRITICAL FIX: Ensure data is fully numeric
  # ========================================

  # Convert to matrix if not already
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }

  # Check data type and convert to numeric if needed
  if (!is.numeric(data_matrix)) {
    debug_log("Data matrix contains non-numeric data, attempting conversion", 1)
    original_rownames <- rownames(data_matrix)
    original_colnames <- colnames(data_matrix)

    # Try to convert to numeric
    data_matrix_numeric <- tryCatch({
      # First check if it's character numbers
      if (is.character(data_matrix)) {
        matrix(as.numeric(data_matrix), nrow = nrow(data_matrix), ncol = ncol(data_matrix))
      } else {
        # For factors or other types
        matrix(as.numeric(as.character(data_matrix)), nrow = nrow(data_matrix), ncol = ncol(data_matrix))
      }
    }, error = function(e) {
      debug_log(paste("Failed to convert to numeric:", e$message), 1)
      stop("Data matrix contains non-convertible non-numeric data")
    })

    # Check if conversion introduced NAs where there were none before
    original_na_count <- sum(is.na(data_matrix))
    new_na_count <- sum(is.na(data_matrix_numeric))

    if (new_na_count > original_na_count) {
      debug_log(paste("Numeric conversion introduced", new_na_count - original_na_count, "additional NAs"), 1)
    }

    data_matrix <- data_matrix_numeric

    # Restore the names captured before constructing the numeric matrix.
    if (!is.null(original_rownames)) rownames(data_matrix) <- original_rownames
    if (!is.null(original_colnames)) colnames(data_matrix) <- original_colnames
  }

  # Double-check that data is now numeric
  if (!is.numeric(data_matrix)) {
    stop("Failed to convert data to numeric format")
  }

  # ========================================
  # PROCEED WITH IMPUTATION IF NAs EXIST
  # ========================================

  if (!any(is.na(data_matrix))) {
    debug_log("No missing values found, returning original data", 2)
    return(data_matrix)
  }

  missing_value_count <- sum(is.na(data_matrix))
  if (method == "remove") {
    debug_log(paste("Handling", missing_value_count, "missing values using remove method"), 2)
  } else {
    debug_log(paste("Imputing", missing_value_count, "missing values using", method, "method"), 2)
  }

  if (method == "mean") {
    # Row-wise mean imputation with enhanced error handling
    for (i in 1:nrow(data_matrix)) {
      na_idx <- is.na(data_matrix[i, ])
      if (any(na_idx)) {
        # Calculate row mean excluding NAs
        non_na_values <- data_matrix[i, !na_idx]

        if (length(non_na_values) > 0) {
          row_mean <- mean(non_na_values, na.rm = TRUE)

          if (!is.na(row_mean) && is.finite(row_mean)) {
            data_matrix[i, na_idx] <- row_mean
          } else {
            # If row mean is invalid, use global mean
            global_mean <- mean(data_matrix, na.rm = TRUE)
            if (!is.na(global_mean) && is.finite(global_mean)) {
              data_matrix[i, na_idx] <- global_mean
            } else {
              # Last resort: use minimum positive value
              min_val <- min(data_matrix[data_matrix > 0], na.rm = TRUE)
              data_matrix[i, na_idx] <- ifelse(is.finite(min_val), min_val * 0.1, 0.1)
            }
          }
        } else {
          # If entire row is NA, use global mean
          global_mean <- mean(data_matrix, na.rm = TRUE)
          if (!is.na(global_mean) && is.finite(global_mean)) {
            data_matrix[i, na_idx] <- global_mean
          } else {
            data_matrix[i, na_idx] <- 0.1  # Minimal positive value
          }
        }
      }
    }
  } else if (method == "median") {
    # Row-wise median imputation
    for (i in 1:nrow(data_matrix)) {
      na_idx <- is.na(data_matrix[i, ])
      if (any(na_idx)) {
        non_na_values <- data_matrix[i, !na_idx]

        if (length(non_na_values) > 0) {
          row_median <- median(non_na_values, na.rm = TRUE)

          if (!is.na(row_median) && is.finite(row_median)) {
            data_matrix[i, na_idx] <- row_median
          } else {
            global_median <- median(data_matrix, na.rm = TRUE)
            data_matrix[i, na_idx] <- ifelse(is.finite(global_median), global_median, 0.1)
          }
        } else {
          global_median <- median(data_matrix, na.rm = TRUE)
          data_matrix[i, na_idx] <- ifelse(is.finite(global_median), global_median, 0.1)
        }
      }
    }
  } else if (method == "min") {
    # Minimum value imputation (common for proteomics)
    global_min <- min(data_matrix, na.rm = TRUE)
    if (is.finite(global_min)) {
      data_matrix[is.na(data_matrix)] <- global_min * 0.8
    } else {
      data_matrix[is.na(data_matrix)] <- 0.1
    }
  } else if (method == "remove") {
    # Remove rows/columns with any NA values
    debug_log("Removing rows and columns with missing values", 2)
    rows_before <- nrow(data_matrix)
    cols_before <- ncol(data_matrix)

    # Remove rows with any NA
    complete_rows <- complete.cases(data_matrix)
    if (sum(complete_rows) < 2) {
      stop("Too few complete rows remain after removing missing values")
    }
    data_matrix <- data_matrix[complete_rows, , drop = FALSE]

    # Remove columns with any NA
    complete_cols <- apply(data_matrix, 2, function(x) !any(is.na(x)))
    if (sum(complete_cols) < 2) {
      stop("Too few complete columns remain after removing missing values")
    }
    data_matrix <- data_matrix[, complete_cols, drop = FALSE]
    rows_after <- nrow(data_matrix)
    cols_after <- ncol(data_matrix)

    debug_log(paste0(
      "Removed incomplete rows/columns | rows: ", rows_before, " -> ", rows_after,
      " | columns: ", cols_before, " -> ", cols_after
    ), 2)
  }

  # Final validation
  if (any(is.na(data_matrix))) {
    debug_log(paste("Warning:", sum(is.na(data_matrix)), "NAs still remain after", ifelse(method == "remove", "removal", "imputation")), 1)
  }

  if (any(is.infinite(data_matrix))) {
    debug_log("Warning: Infinite values detected after missing-value handling", 1)
    data_matrix[is.infinite(data_matrix)] <- 0.1
  }

  if (method == "remove") {
    debug_log("Missing-value removal completed", 2)
  } else {
    debug_log("Missing value imputation completed", 2)
  }
  return(data_matrix)
}

# ========================================
# Validation Functions
# ========================================

#' Validate input data for dimension reduction
#' @param data Data frame or matrix
#' @param min_features Minimum number of features required
#' @param min_samples Minimum number of samples required
#' @return List with validation status and messages
validate_dimension_reduction_data <- function(data, min_features = 10, min_samples = 3) {
  messages <- list()
  is_valid <- TRUE

  # Check if data exists
  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    messages$error <- "Data is empty or NULL"
    return(list(valid = FALSE, messages = messages))
  }

  # Find numeric columns
  numeric_cols <- which(sapply(data, is.numeric))

  if (length(numeric_cols) < min_samples) {
    messages$error <- paste("Insufficient numeric columns. Need at least",
                            min_samples, "samples")
    return(list(valid = FALSE, messages = messages))
  }

  # Check number of features
  if (nrow(data) < min_features) {
    messages$error <- paste("Insufficient features. Need at least",
                            min_features, "proteins/genes")
    return(list(valid = FALSE, messages = messages))
  }

  # Check for variance
  numeric_data <- data[, numeric_cols]
  row_vars <- apply(numeric_data, 1, var, na.rm = TRUE)

  if (sum(row_vars > 0, na.rm = TRUE) < min_features) {
    messages$warning <- "Many features have zero variance"
  }

  # Check missing values
  na_proportion <- sum(is.na(numeric_data)) / (nrow(numeric_data) * ncol(numeric_data))
  if (na_proportion > 0.5) {
    messages$error <- "More than 50% missing values"
    return(list(valid = FALSE, messages = messages))
  } else if (na_proportion > 0.2) {
    messages$warning <- paste0("Data contains ", round(na_proportion * 100, 1),
                               "% missing values")
  }

  # Check for infinite values
  if (any(is.infinite(as.matrix(numeric_data)))) {
    messages$error <- "Data contains infinite values"
    return(list(valid = FALSE, messages = messages))
  }

  return(list(valid = is_valid, messages = messages))
}


# ========================================
# Protein Input Parsing Function
# ========================================

get_filter_string_pca <- function(input_text, selected_identifier, debug_log) {
  debug_log("Parsing protein input text for Heatmap module", 2)

  lines <- unlist(strsplit(input_text, "\n"))
  lines <- trimws(lines[lines != ""])
  num_lines <- length(lines)

  if (num_lines == 0) {
    return(data.frame())
  }

  df <- data.frame(matrix(nrow = num_lines, ncol = 1))
  colnames(df) <- c(selected_identifier)

  for (i in 1:num_lines) {
    line <- unlist(strsplit(lines[i], "[,\\s]+"))
    df[i, selected_identifier] <- line[1]  # Takes first element if comma/space separated
  }

  debug_log(paste("Parsed", num_lines, "protein identifiers"), 2)
  return(df)
}

#' Get abundance column indices for PCA analysis (Updated to work with individual sample selection)
#' @param data_def data definition dataframe
#' @param selected_data_type selected abundance data type
#' @param selected_samples vector of selected sample names
#' @param debug_log logging function
#' @return vector of column indices
get_pca_abundance_columns <- function(data_def, selected_data_type, selected_samples, debug_log = NULL) {
  tryCatch({
    if (is.null(data_def) || is.null(selected_data_type) || is.null(selected_samples)) {
      debug_log("Missing required parameters for abundance column selection", 1)
      return(integer(0))
    }

    # Find rows that match both the data type AND the selected samples
    matching_rows <- which(data_def$Content == selected_data_type &
                             data_def$Sample %in% selected_samples &
                             !is.na(data_def$Sample))

    debug_log(paste("Found", length(matching_rows), "matching abundance columns for data type:",
                    selected_data_type, "and", length(selected_samples), "selected samples"), 2)

    return(matching_rows)

  }, error = function(e) {
    debug_log(paste("Error getting abundance columns:", e$message), 1)
    return(integer(0))
  })
}

#' Retransform data using metadata (user-provided function)
#' @param df data frame
#' @param index column indices
#' @param transformation_df transformation vector
#' @return retransformed data frame
retransform_data_pca <- function(df, index, transformation_df) {
  for (i in seq_along(index)) {
    ci <- index[i]; tr <- transformation_df[i]
    orig <- df[, ci]
    new <- switch(tr,
                  "log2"    = 2^orig,
                  "-log10"  = 10^(-orig),
                  "log10"   = 10^orig,
                  orig)
    if (any(is.infinite(new))) {
      showNotification(paste("Retransformation produces infinite values in column:", colnames(df)[ci]), type="error", duration=5)
      df[, ci] <- orig
    } else {
      df[, ci] <- new
    }
  }
  df
}

# ========================================
# Z-Score Normalization (Vectorized)
# ========================================

# This function replaces the z-score section in prepare_pca_analysis_data
apply_zscore_normalization <- function(log_data, debug_log) {
  debug_log("Calculating z-scores (protein-wise normalization)", 2)

  # Vectorized z-score calculation using scale with MARGIN=1 (row-wise)
  df_zscore <- t(scale(t(log_data), center = TRUE, scale = TRUE))

  # Handle proteins with zero standard deviation (scale returns NaN)
  df_zscore[is.na(df_zscore)] <- 0

  # Verification
  protein_means_check <- rowMeans(df_zscore, na.rm = TRUE)
  protein_sds_check <- apply(df_zscore, 1, sd, na.rm = TRUE)

  debug_log(paste("Z-score verification - Protein means range:",
                  round(min(protein_means_check, na.rm = TRUE), 4), "to",
                  round(max(protein_means_check, na.rm = TRUE), 4)), 2)
  debug_log(paste("Z-score verification - Protein SDs range:",
                  round(min(protein_sds_check, na.rm = TRUE), 4), "to",
                  round(max(protein_sds_check, na.rm = TRUE), 4)), 2)

  debug_log(paste("Z-score normalization completed. Final range:",
                  round(min(df_zscore, na.rm = TRUE), 2), "to",
                  round(max(df_zscore, na.rm = TRUE), 2)), 2)

  return(df_zscore)
}

#' Complete corrected prepare_pca_analysis_data function
#' @param data data matrix
#' @param metadata data definition
#' @param selected_data_type selected abundance type
#' @param selected_samples vector of selected sample names
#' @param selected_identifier identifier column name
#' @param debug_log logging function
#' @return list with prepared data and metadata
prepare_pca_analysis_data <- function(data, metadata, selected_data_type, selected_samples, selected_identifier, debug_log = NULL) {
  tryCatch({

    debug_log("=== COMPLETE PCA DATA PREPARATION START ===", 2)
    transformations_applied <- character(0)
    retransform_succeeded <- FALSE

    # ========================================
    # GET COLUMN INDICES
    # ========================================

    abundance_indices <- which(metadata$Content == selected_data_type &
                                 metadata$Sample %in% selected_samples &
                                 !is.na(metadata$Sample))

    if (length(abundance_indices) == 0) {
      debug_log("No matching abundance columns found", 1)
      showNotification("No matching data found for selected samples", type = "error", duration = 5)
      return(NULL)
    }

    identifier_indices <- which(grepl(selected_identifier, metadata$Options, ignore.case = TRUE))
    if (length(identifier_indices) == 0) {
      debug_log("No matching identifier found", 1)
      showNotification("No matching identifier found", type = "error", duration = 5)
      return(NULL)
    }

    debug_log(paste("Found", length(abundance_indices), "abundance columns and", length(identifier_indices), "identifier columns"), 2)

    # ========================================
    # RETRANSFORM DATA USING CORRECT FUNCTION
    # ========================================

    transformation_df <- metadata$Transformation[abundance_indices]
    debug_log(paste("Metadata retransformation needed:", paste(unique(transformation_df), collapse = ", ")), 2)

    # Check if any transformations need to be applied before doing expensive work.
    transformation_values <- trimws(as.character(transformation_df))
    needs_retransform <- any(!is.na(transformation_values) &
                               transformation_values != "" &
                               tolower(transformation_values) != "none")

    if (!needs_retransform) {
      data_retransform <- data
      debug_log("No PCA retransformation needed; all selected columns marked None", 2)
    } else {
      # Prefer shared global retransformation utility for consistency with other modules
      retransform_fn <- get0("retransform_data_global", mode = "function", inherits = TRUE)
      data_retransform <- tryCatch({
        retransform_succeeded <<- TRUE
        if (is.function(retransform_fn)) {
          debug_log("Calling retransform_data_global", 2)
          retransform_fn(data, abundance_indices, transformation_df)
        } else {
          debug_log("retransform_data_global not found, falling back to retransform_data_pca", 1)
          retransform_data_pca(data, abundance_indices, transformation_df)
        }
      }, error = function(e) {
        retransform_succeeded <<- FALSE
        debug_log(paste("Retransformation failed:", e$message), 1)
        showNotification("Retransformation failed, using original data", type = "warning", duration = 3)
        data
      })

      # Validate retransformation result
      if (!is.data.frame(data_retransform)) {
        debug_log(paste("retransform_data_pca returned:", class(data_retransform)), 2)
        if (is.list(data_retransform) && "data" %in% names(data_retransform)) {
          data_retransform <- data_retransform$data
        } else {
          data_retransform <- as.data.frame(data_retransform)
        }
      }

      # Verify retransformation actually worked
      original_range <- range(data[, abundance_indices], na.rm = TRUE)
      retrans_range <- range(data_retransform[, abundance_indices], na.rm = TRUE)
      debug_log(paste("Original range:", round(original_range[1], 2), "to", round(original_range[2], 2)), 2)
      debug_log(paste("Retransformed range:", round(retrans_range[1], 2), "to", round(retrans_range[2], 2)), 2)

      if (isTRUE(all.equal(original_range, retrans_range))) {
        debug_log("WARNING: Retransformation did not change data - using original", 1)
        retransform_succeeded <- FALSE
      } else {
        debug_log("Retransformation successful", 2)
      }

      if (retransform_succeeded) {
        transformations_applied <- c(transformations_applied, "retransform")
      }
    }

    # ========================================
    # TRANSFORMATION SUMMARY
    # ========================================

    transformation_labels <- trimws(as.character(transformation_df))
    transformation_labels[is.na(transformation_labels) | transformation_labels == ""] <- "<missing>"
    transformation_counts <- table(transformation_labels)
    transformation_summary <- paste(
      paste0(names(transformation_counts), "=", as.integer(transformation_counts)),
      collapse = ", "
    )
    debug_log(paste0(
      "PCA transformation summary | columns=", length(abundance_indices),
      " | transformations={", transformation_summary, "}",
      " | retransformation_needed=", needs_retransform
    ), 2)

    if (isTRUE(getOption("miraprot.pca.transformation_diagnostics", FALSE))) {
      debug_log("=== TRANSFORMATION DIAGNOSTIC ===", 2)

      # Show what transformations are expected
      debug_log(paste("Abundance indices:", paste(abundance_indices, collapse = ", ")), 2)
      debug_log(paste("Number of abundance columns:", length(abundance_indices)), 2)

      # Check transformation values for each abundance column
      debug_log("Transformation analysis per column:", 2)
      for (i in 1:min(10, length(abundance_indices))) {
        col_idx <- abundance_indices[i]
        transformation <- transformation_df[i]
        sample_name <- metadata$Sample[col_idx]
        content <- metadata$Content[col_idx]

        debug_log(paste("Column", col_idx, "- Sample:", sample_name,
                        "| Content:", content,
                        "| Transformation:", transformation), 2)
      }

      # Check unique transformation values
      unique_transforms <- unique(transformation_df)
      debug_log(paste("Unique transformation values found:", paste(unique_transforms, collapse = ", ")), 2)

      # Count each transformation type
      for (transform_type in unique_transforms) {
        count <- sum(transformation_df == transform_type, na.rm = TRUE)
        debug_log(paste("Transformation '", transform_type, "':", count, "columns"), 2)
      }

      debug_log("=== TRANSFORMATION DIAGNOSTIC COMPLETE ===", 2)
    }

    # ========================================
    # CREATE DATA MATRIX
    # ========================================

    df <- data_retransform[, abundance_indices, drop = FALSE]

    if (ncol(df) == 0 || nrow(df) == 0) {
      debug_log("No data available after processing", 1)
      showNotification("No data available for analysis", type = "error", duration = 5)
      return(NULL)
    }

    # Set column names (sample names)
    sample_names <- metadata$Sample[abundance_indices]
    if (length(sample_names) == ncol(df)) {
      colnames(df) <- sample_names
    } else {
      colnames(df) <- paste0("Sample_", seq_len(ncol(df)))
      debug_log("Sample names length mismatch, using generic names", 1)
    }

    # Set row names (protein identifiers)
    row_identifiers <- data[, identifier_indices[1]]
    row_identifiers <- sanitize_rownames(row_identifiers, prefix = "Protein_")
    if (any(duplicated(row_identifiers))) {
      debug_log("Duplicate identifiers detected, making unique", 1)
      row_identifiers <- make.unique(as.character(row_identifiers), sep = "_")
    }

    if (length(row_identifiers) == nrow(df)) {
      rownames(df) <- row_identifiers
    } else {
      stop(sprintf(
        paste0("Protein identifier alignment failed after retransformation: ",
               "abundance rows=%d, identifier rows=%d"),
        nrow(df), length(row_identifiers)
      ))
    }

    # ========================================
    # INITIAL FILTERING
    # ========================================

    debug_log("Initial data filtering", 2)
    initial_proteins <- nrow(df)

    # Remove rows with too many missing values (>50%)
    missing_threshold <- 0.5
    row_missing <- rowSums(is.na(df)) / ncol(df)
    keep_rows <- row_missing <= missing_threshold

    df_filtered <- df[keep_rows, , drop = FALSE]
    identifiers_filtered <- row_identifiers[keep_rows]

    debug_log(paste("Filtered out", initial_proteins - nrow(df_filtered), "proteins with >50% missing values"), 2)
    debug_log(paste("Remaining proteins:", nrow(df_filtered)), 2)

    if (nrow(df_filtered) == 0) {
      debug_log("No valid data remaining after filtering", 1)
      showNotification("No valid data for analysis after filtering", type = "error", duration = 5)
      return(NULL)
    }

    # ========================================
    # MATRIX CONVERSION (Fix for list problem)
    # ========================================

    missing_value_method <- "remove"
    debug_log(paste0(
      "PCA preprocessing | metadata_retransform=", toupper(as.character(needs_retransform)),
      " | missing_value_method=", missing_value_method,
      " | log2=TRUE",
      " | zscore=TRUE",
      " | prcomp_scale=TRUE"
    ), 2)
    debug_log("Converting to matrix for arithmetic operations", 2)

    df_matrix <- tryCatch({
      as.matrix(df_filtered)
    }, error = function(e) {
      debug_log(paste("Matrix conversion failed:", e$message), 1)

      # Column-by-column conversion fallback
      df_numeric <- data.frame(row.names = rownames(df_filtered))
      for (i in 1:ncol(df_filtered)) {
        col_name <- colnames(df_filtered)[i]
        col_data <- df_filtered[, i]

        numeric_col <- tryCatch({
          if (is.list(col_data)) {
            as.numeric(unlist(col_data))
          } else {
            as.numeric(col_data)
          }
        }, error = function(e) {
          debug_log(paste("Failed to convert column", i, "to numeric:", e$message), 1)
          rep(NA, nrow(df_filtered))
        })

        df_numeric[[col_name]] <- numeric_col
      }
      as.matrix(df_numeric)
    })

    # Restore names
    rownames(df_matrix) <- sanitize_rownames(rownames(df_filtered), prefix = "Protein_")
    colnames(df_matrix) <- colnames(df_filtered)

    debug_log("Matrix conversion completed", 2)

    # ========================================
    # MISSING VALUE IMPUTATION
    # ========================================

    na_count <- sum(is.na(df_matrix))
    if (na_count > 0) {
      # Use your existing impute_missing_values function with the corrected version
      df_matrix <- impute_missing_values(df_matrix, method = missing_value_method, debug_log = debug_log)
      if (missing_value_method == "remove") {
        transformations_applied <- c(transformations_applied, "missing_value_removal")
      } else if (missing_value_method %in% c("mean", "median", "min")) {
        transformations_applied <- c(transformations_applied, paste0("missing_value_imputation_", missing_value_method))
      }

      # Verify missing-value removal worked
      remaining_na <- sum(is.na(df_matrix))
      if (remaining_na > 0) {
        debug_log(paste("WARNING:", remaining_na, "missing values remain after removal"), 1)
        # Force removal of remaining NAs
        df_matrix[is.na(df_matrix)] <- 0
      } else {
        debug_log("No missing values remain after removal", 2)
      }
    }

    # `remove` can change the row set a second time (after the >50% filter).
    # The matrix row names are the authoritative, unique identifiers assigned
    # above, so derive the returned identifiers from the matrix that actually
    # continues through the analysis rather than from the earlier filter mask.
    final_identifiers <- rownames(df_matrix)
    if (is.null(final_identifiers) || length(final_identifiers) != nrow(df_matrix)) {
      stop(sprintf(
        "Protein identifier alignment failed during preparation: matrix rows=%d, identifiers=%d",
        nrow(df_matrix), length(final_identifiers %||% character())
      ))
    }
    identifiers_filtered <- as.character(final_identifiers)

    # ========================================
    # LOG2 TRANSFORMATION
    # ========================================

    debug_log("Applying log2 transformation", 2)

    # Check for zero/negative values
    min_val <- min(df_matrix, na.rm = TRUE)
    if (min_val <= 0) {
      debug_log(paste("Found zero/negative values (min:", min_val, "), using pseudocount"), 1)
      pseudocount <- abs(min_val) + 1
    } else {
      pseudocount <- 1
    }

    df_log <- log2(df_matrix + pseudocount)
    transformations_applied <- c(transformations_applied, "log2")

    # Handle any infinite values
    if (any(is.infinite(df_log))) {
      debug_log("Replacing infinite values after log2 transformation", 1)
      df_log[is.infinite(df_log)] <- 0
    }

    debug_log(paste("Log2 transformation completed. Range:",
                    round(min(df_log, na.rm = TRUE), 2), "to",
                    round(max(df_log, na.rm = TRUE), 2)), 2)

    # ========================================
    # Z-SCORE NORMALIZATION (CORRECTED)
    # ========================================

    df_zscore <- apply_zscore_normalization(df_log, debug_log)
    transformations_applied <- c(transformations_applied, "zscore")

    # Handle any remaining NAs
    df_zscore[is.na(df_zscore)] <- 0


    # Verification of z-score normalization
    protein_means_check <- rowMeans(df_zscore, na.rm = TRUE)
    protein_sds_check <- apply(df_zscore, 1, sd, na.rm = TRUE)

    # ========================================
    # CREATE SAMPLE METADATA
    # ========================================

    sample_metadata <- data.frame(
      Sample = colnames(df_zscore),
      DataType = selected_data_type,
      stringsAsFactors = FALSE
    )

    # Add condition information if available
    condition_info <- metadata$Options[abundance_indices]
    if (length(condition_info) == ncol(df_zscore)) {
      sample_metadata$Condition <- condition_info
      debug_log(paste("Added conditions:", paste(unique(condition_info), collapse = ", ")), 2)
    } else {
      debug_log("No condition information available", 2)
    }

    # ========================================
    # FINAL VALIDATION
    # ========================================

    debug_log("Final data validation", 2)

    # Check for any remaining problematic values
    if (any(is.na(df_zscore))) {
      debug_log(paste("WARNING:", sum(is.na(df_zscore)), "NAs remain in final data"), 1)
    }

    if (any(is.infinite(df_zscore))) {
      debug_log(paste("WARNING:", sum(is.infinite(df_zscore)), "infinite values in final data"), 1)
    }

    # Check variance
    total_variance <- var(as.vector(df_zscore), na.rm = TRUE)
    debug_log(paste("Total data variance:", round(total_variance, 4)), 2)

    if (total_variance < 0.01) {
      debug_log("WARNING: Very low data variance - PCA may not be informative", 1)
    }

    debug_log("=== COMPLETE PCA DATA PREPARATION END ===", 2)
    debug_log(paste("Final processed data:", nrow(df_zscore), "proteins x", ncol(df_zscore), "samples"), 1)

    # ========================================
    # RETURN RESULTS
    # ========================================

    return(list(
      data = df_zscore,                        # Z-score normalized, log2-transformed data
      raw_data = df_matrix,                    # Raw retransformed data
      log2_data = df_log,                      # Log2-transformed data
      samples = colnames(df_zscore),
      identifiers = identifiers_filtered,
      metadata = sample_metadata,
      abundance_indices = abundance_indices,
      identifier_indices = identifier_indices[1],
      selected_data_type = selected_data_type,
      selected_samples = selected_samples,
      transformations_applied = transformations_applied,
      data_quality = list(
        original_proteins = initial_proteins,
        final_proteins = nrow(df_zscore),
        missing_values_imputed = na_count,
        z_score_mean_range = range(protein_means_check, na.rm = TRUE),
        z_score_sd_range = range(protein_sds_check, na.rm = TRUE),
        total_variance = total_variance
      )
    ))

  }, error = function(e) {
    debug_log(paste("Error in data preparation:", e$message), 1)
    debug_log(paste("Error traceback:", paste(traceback(), collapse = "\n")), 1)
    showNotification("Error preparing data for analysis", type = "error", duration = 5)
    return(NULL)
  })
}

# Return selected samples whose abundance metadata has conflicting, non-empty
# biological-group assignments. Duplicate rows with the same group are benign.
pca_conflicting_group_samples <- function(metadata, selected_data_type, selected_samples) {
  if (!inherits(metadata, "data.frame")) return(character())
  content_col <- get_col(metadata, c("Content"))
  sample_col <- get_col(metadata, c("Sample", "Column"))
  option_col <- get_col(metadata, c("Options", "Option"))
  if (any(vapply(list(content_col, sample_col, option_col), is.null, logical(1)))) {
    return(character())
  }

  rows <- !is.na(metadata[[content_col]]) &
    trimws(as.character(metadata[[content_col]])) == trimws(as.character(selected_data_type))[1] &
    !is.na(metadata[[sample_col]]) & metadata[[sample_col]] %in% selected_samples
  groups <- split(as.character(metadata[[option_col]][rows]), as.character(metadata[[sample_col]][rows]))
  conflicts <- names(groups)[vapply(groups, function(values) {
    values <- trimws(values[!is.na(values)])
    length(unique(values[nzchar(values)])) > 1L
  }, logical(1))]
  selected_samples[selected_samples %in% conflicts]
}

#' Create convex hull data for polygon plotting by condition (Enhanced Debug Version)
#' @param plot_data Data frame with x, y coordinates and Condition column
#' @param debug_log Logging function
#' @return Data frame with convex hull points for each condition
create_convex_hull_data <- function(plot_data, debug_log = NULL) {
  tryCatch({

    # Basic validation
    if (is.null(plot_data) || nrow(plot_data) == 0 || !"Condition" %in% colnames(plot_data)) {
      debug_log("No valid data for polygon creation", 1)
      return(NULL)
    }

    if (!"x" %in% colnames(plot_data) || !"y" %in% colnames(plot_data)) {
      debug_log("Missing coordinate columns for polygon creation", 1)
      return(NULL)
    }

    # Filter valid conditions
    valid_data <- plot_data[!is.na(plot_data$Condition) & plot_data$Condition != "", ]

    if (nrow(valid_data) == 0) {
      debug_log("No valid conditions for polygon creation", 1)
      return(NULL)
    }

    # Create hulls for each condition
    hull_list <- list()
    conditions <- unique(valid_data$Condition)

    for (condition in conditions) {
      condition_data <- valid_data[valid_data$Condition == condition, ]

      if (nrow(condition_data) >= 3) {
        hull_indices <- chull(condition_data$x, condition_data$y)
        hull_points <- condition_data[hull_indices, ]
        hull_list[[condition]] <- hull_points
        debug_log(paste("Created hull for", condition, "with", nrow(hull_points), "points"), 2)
      }
    }

    # Combine all hulls
    if (length(hull_list) > 0) {
      hull_data <- do.call(rbind, hull_list)
      debug_log(paste("Combined hull data:", nrow(hull_data), "points for", length(hull_list), "conditions"), 2)
      return(hull_data)
    } else {
      debug_log("No conditions had enough points for polygon creation", 1)
      return(NULL)
    }

  }, error = function(e) {
    debug_log(paste("Error creating convex hull data:", e$message), 1)
    return(NULL)
  })
}

#' Get plot coordinates for specified axes (Fixed version)
#' @param results Analysis results
#' @param x_axis X axis name (e.g., "PC1", "PC2")
#' @param y_axis Y axis name (e.g., "PC1", "PC2")
#' @return data frame with x, y coordinates
get_plot_coordinates <- function(results, x_axis, y_axis) {
  if (is.null(results$coordinates) || nrow(results$coordinates) == 0) {
    stop("No coordinates available in analysis results")
  }

  coords <- results$coordinates
  method <- results$method %||% ""

  # Default to first two columns
  x_col <- 1
  y_col <- min(2, ncol(coords))

  # Method-specific coordinate mapping
  if (method == "pca") {
    x_num <- suppressWarnings(as.numeric(gsub("PC", "", x_axis)))
    y_num <- suppressWarnings(as.numeric(gsub("PC", "", y_axis)))
    if (!is.na(x_num) && x_num <= ncol(coords)) x_col <- x_num
    if (!is.na(y_num) && y_num <= ncol(coords)) y_col <- y_num

  }

  # Validate indices
  if (x_col < 1 || x_col > ncol(coords) || y_col < 1 || y_col > ncol(coords)) {
    stop(paste("Invalid axis selection:", x_axis, "vs", y_axis, "for", ncol(coords), "available components"))
  }

  return(data.frame(
    x = coords[, x_col],
    y = coords[, y_col],
    stringsAsFactors = FALSE
  ))
}

# ========================================
# PCA Implementation
# ========================================

perform_pca <- function(data_matrix, params, debug_log) {
  tryCatch({
    debug_log("Starting PCA analysis", 2)

    # Use common validation
    validation <- validate_analysis_input(data_matrix, "pca", params)
    if (!validation$valid) {
      stop(paste("Validation failed:", paste(validation$errors, collapse = "; ")))
    }

    # Remove constant columns
    col_vars <- apply(data_matrix, 2, var, na.rm = TRUE)
    constant_cols <- which(col_vars == 0 | is.na(col_vars))
    if (length(constant_cols) > 0) {
      debug_log(paste("Removing", length(constant_cols), "constant columns"), 2)
      data_matrix <- data_matrix[, -constant_cols, drop = FALSE]

      if (ncol(data_matrix) < 2) {
        stop("Too few variable columns remain after filtering")
      }
    }

    # Handle missing values
    if (any(is.na(data_matrix))) {
      debug_log("Imputing missing values", 2)
      data_matrix <- impute_missing_values(data_matrix, method = "remove", debug_log = debug_log)
    }
    scale_data <- params$pca_scale %||% 1
    scale_columns <- isTRUE(as.numeric(scale_data)[1] >= 0.5)
    data_scaled <- scale(data_matrix, center = TRUE, scale = scale_columns)
    if (scale_columns) {
      debug_log("Applied centering and scaling", 2)
    } else {
      debug_log("Applied centering without scaling", 2)
    }

    # Check for NaN after scaling
    if (any(is.nan(data_scaled))) {
      debug_log("NaN values after scaling, retrying with centering only", 1)
      data_scaled <- scale(data_matrix, center = TRUE, scale = FALSE)
    }

    # Perform PCA on the already centered/scaled matrix.
    pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE,
                         rank. = min(dim(data_scaled)) - 1)

    if (nrow(pca_result$x) != nrow(data_matrix)) {
      stop(sprintf(
        "PCA row alignment failed: analysis matrix rows=%d, coordinate rows=%d",
        nrow(data_matrix), nrow(pca_result$x)
      ))
    }

    # Calculate variance explained
    var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
    cumvar_explained <- cumsum(var_explained)

    # Prepare results
    results <- list(
      method = "pca",
      coordinates = pca_result$x,
      loadings = pca_result$rotation,
      sdev = pca_result$sdev,
      var_explained = var_explained,
      cumvar_explained = cumvar_explained,
      n_components = ncol(pca_result$x),
      center = attr(data_scaled, "scaled:center"),
      scale = attr(data_scaled, "scaled:scale"),
      raw_result = pca_result
    )

    debug_log(paste("PCA completed with", results$n_components, "components"), 1)
    return(results)

  }, error = function(e) {
    debug_log(paste("PCA error:", e$message), 1)
    return(NULL)
  })
}

validate_analysis_input <- function(data_matrix, method = "pca", params = list()) {
  errors <- character()
  warnings <- character()

  # Basic data validation
  if (is.null(data_matrix)) {
    errors <- c(errors, "Data matrix is NULL")
  } else {
    if (!is.matrix(data_matrix) && !is.data.frame(data_matrix)) {
      errors <- c(errors, "Input must be a matrix or data frame")
    }

    if (nrow(data_matrix) < 2 || ncol(data_matrix) < 2) {
      errors <- c(errors, "Data matrix must have at least 2 rows and 2 columns")
    }
  }

  # Method-specific validation
  if (method == "umap") {
    if (nrow(data_matrix) < 2) errors <- c(errors, "UMAP requires at least 2 samples")
  }

  return(list(valid = length(errors) == 0, errors = errors, warnings = warnings))
}

# ========================================
# UMAP Implementation
# ========================================

perform_umap <- function(data_matrix, params, debug_log) {
  tryCatch({
    debug_log("Starting UMAP analysis", 2)

    # Check if umap is available
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop("umap package is required for UMAP analysis. Please install it with: install.packages('umap')")
    }

    # Validate input
    if (!is.matrix(data_matrix) && !is.data.frame(data_matrix)) {
      stop("Input must be a matrix or data frame")
    }

    if (nrow(data_matrix) < 2) {
      stop("UMAP requires at least 2 samples")
    }

    # Get parameters with validation
    n_neighbors <- params$umap_neighbors %||% 15
    min_dist <- params$umap_min_dist %||% 0.1
    metric <- params$umap_metric %||% "euclidean"

    # Validate n_neighbors
    n_samples <- nrow(data_matrix)
    if (n_neighbors >= n_samples) {
      old_neighbors <- n_neighbors
      n_neighbors <- max(2, floor(n_samples / 2))
      debug_log(paste("Adjusted n_neighbors from", old_neighbors, "to", n_neighbors, "based on sample size"), 1)
    }

    # Validate min_dist
    min_dist <- max(0.001, min(0.99, min_dist))

    # Handle missing values
    if (any(is.na(data_matrix))) {
      debug_log("Imputing missing values", 2)
      data_matrix <- impute_missing_values(data_matrix, method = "remove", debug_log = debug_log)
    }
    col_vars <- apply(data_matrix, 2, var, na.rm = TRUE)
    if (any(col_vars == 0 | is.na(col_vars))) {
      constant_cols <- which(col_vars == 0 | is.na(col_vars))
      debug_log(paste("Removing", length(constant_cols), "constant columns"), 2)
      data_matrix <- data_matrix[, -constant_cols, drop = FALSE]
    }

    # Configure UMAP
    umap_config <- umap::umap.defaults
    umap_config$n_neighbors <- n_neighbors
    umap_config$min_dist <- min_dist
    umap_config$metric <- metric
    umap_config$n_components <- 2
    umap_config$random_state <- params$umap_seed %||% 42

    # Additional stability parameters
    umap_config$n_epochs <- 500  # More epochs for stability
    umap_config$negative_sample_rate <- 5
    umap_config$init <- "spectral"  # More stable initialization

    # Perform UMAP with error handling
    umap_result <- tryCatch({
      umap::umap(data_matrix, config = umap_config)
    }, error = function(e) {
      if (grepl("n_neighbors", e$message, ignore.case = TRUE)) {
        # Try with lower n_neighbors
        debug_log("Retrying with lower n_neighbors", 1)
        umap_config$n_neighbors <- max(2, floor(n_neighbors / 2))
        umap::umap(data_matrix, config = umap_config)
      } else if (grepl("spectral", e$message, ignore.case = TRUE)) {
        # Try with random initialization
        debug_log("Retrying with random initialization", 1)
        umap_config$init <- "random"
        umap::umap(data_matrix, config = umap_config)
      } else {
        stop(e$message)
      }
    })

    # Prepare results
    results <- list(
      method = "umap",
      coordinates = umap_result$layout,
      n_components = 2,
      n_neighbors = n_neighbors,
      min_dist = min_dist,
      metric = metric,
      config = umap_config,
      raw_result = umap_result
    )

    # Add dimension names
    colnames(results$coordinates) <- c("Dim1", "Dim2")

    debug_log(paste("UMAP completed with", n_neighbors, "neighbors"), 1)
    return(results)

  }, error = function(e) {
    debug_log(paste("UMAP error:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Helper Functions
# ========================================

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Get axis labels with variance explained
get_axis_labels <- function(results, x_axis, y_axis, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(...) invisible(NULL)
  tryCatch({
    if (results$method == "pca") {
      x_pc <- as.numeric(gsub("PC", "", x_axis))
      y_pc <- as.numeric(gsub("PC", "", y_axis))

      # Validate indices
      if (!is.na(x_pc) && x_pc <= length(results$var_explained)) {
        x_var <- round(results$var_explained[x_pc], 1)
        x_label <- paste0(x_axis, " (", x_var, "%)")
      } else {
        x_label <- x_axis
      }

      if (!is.na(y_pc) && y_pc <= length(results$var_explained)) {
        y_var <- round(results$var_explained[y_pc], 1)
        y_label <- paste0(y_axis, " (", y_var, "%)")
      } else {
        y_label <- y_axis
      }
    } else if (results$method == "umap") {
      x_label <- "UMAP 1"
      y_label <- "UMAP 2"
    } else {
      x_label <- x_axis
      y_label <- y_axis
    }

    return(list(x = x_label, y = y_label))

  }, error = function(e) {
    debug_log(paste("Error getting axis labels:", e$message), 1)
    return(list(x = x_axis, y = y_axis))
  })
}

# Enhanced create plot data with strict condition handling for repository metadata
# - Uses only 'Options'/'Option' from rows where 'Content' matches the required pattern
# - Maps by Name -> metadata$Sample, then (if needed) Name -> metadata$Column
# - No positional matching, no heuristic fallbacks
# @param results Analysis results
# @param metadata Metadata data frame
# @param identifier_col Identifier column name
# @return Data frame with plot coordinates and condition information
create_plot_data <- function(results, metadata = NULL, identifier_col = NULL, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(...) invisible(NULL)
  tryCatch({
    if (is.null(results$coordinates) || nrow(results$coordinates) == 0) {
      stop("No coordinates available in results.")
    }

    # Coordinates for requested axes (caller may reassign later)
    coords <- get_plot_coordinates(
      results,
      results$x_axis %||% "PC1",
      results$y_axis %||% "PC2"
    )

    # Protein coordinates must always use the identifiers persisted with the
    # analysis. Coordinate row names are not an independent fallback because
    # they can conceal an upstream row-filtering mismatch.
    if (identical(results$comparison_target, "proteins")) {
      sample_names <- results$point_names %||% character()
      if (length(sample_names) != nrow(results$coordinates)) {
        stop(sprintf(
          "Protein plot identifier alignment failed: coordinate rows=%d, point_names=%d",
          nrow(results$coordinates), length(sample_names)
        ))
      }
    } else {
      sample_names <- rownames(results$coordinates)
      if (is.null(sample_names) || length(sample_names) != nrow(results$coordinates)) {
        sample_names <- results$point_names %||% paste0("Point_", seq_len(nrow(results$coordinates)))
      }
    }

    plot_data <- data.frame(
      x = coords$x,
      y = coords$y,
      Name = sample_names,
      stringsAsFactors = FALSE
    )

    # Assign conditions for sample comparison
    if (!is.null(metadata) && results$comparison_target == "samples") {
      cond <- extract_conditions_for_samples(
        metadata = metadata,
        sample_names = sample_names,
        selected_data_type = results$selected_data_type %||% NULL
      )
      plot_data$Condition <- cond
      debug_log(paste("Condition assignment:", sum(!is.na(cond)), "matches of", length(cond)), 3)
    }

    # Proteins: attach identifiers with strict length safety
    if (results$comparison_target == "proteins" && !is.null(identifier_col)) {
      point_ids <- results$point_names %||% character()
      if (length(point_ids) != nrow(plot_data)) {
        stop(sprintf(
          "Protein plot identifier alignment failed: plot rows=%d, point_names=%d",
          nrow(plot_data), length(point_ids)
        ))
      }
      plot_data$Identifier <- point_ids
    }

    return(plot_data)

  }, error = function(e) {
    debug_log(paste("Error creating plot data (strict):", e$message), 1)
    if (identical(results$comparison_target, "proteins")) stop(e$message, call. = FALSE)
    n <- if (!is.null(results$coordinates)) nrow(results$coordinates) else 0
    if (n > 0) {
      coords2 <- try(get_plot_coordinates(results, results$x_axis %||% "PC1", results$y_axis %||% "PC2"), silent = TRUE)
      x <- if (!inherits(coords2, "try-error")) coords2$x else rep(NA_real_, n)
      y <- if (!inherits(coords2, "try-error")) coords2$y else rep(NA_real_, n)
      Name <- rownames(results$coordinates) %||% paste0("Point_", seq_len(n))
      return(data.frame(x = x, y = y, Name = Name, stringsAsFactors = FALSE))
    } else {
      return(data.frame(x = numeric(0), y = numeric(0), Name = character(0), stringsAsFactors = FALSE))
    }
  })
}

# ========================================
# Centralized Axis Choice Management
# ========================================

manage_axis_choices <- function(method, results, input) {
  choices_x <- character()
  choices_y <- character()
  selected_x <- NULL
  selected_y <- NULL

  if (!is.null(results) && results$method == method) {
    # Use actual results
    if (method == "pca") {
      n_comp <- min(results$n_components, 20)
      choices_x <- choices_y <- paste0("PC", 1:n_comp)
      selected_x <- input$axis_x %||% "PC1"
      selected_y <- input$axis_y %||% "PC2"
    } else if (method %in% c("umap")) {
      choices_x <- choices_y <- c("Dim1", "Dim2")
      selected_x <- "Dim1"
      selected_y <- "Dim2"
    }
  } else {
    # Pre-populate choices
    if (method == "pca") {
      choices_x <- choices_y <- paste0("PC", 1:10)
      selected_x <- "PC1"
      selected_y <- "PC2"
    } else if (method %in% c("umap")) {
      choices_x <- choices_y <- c("Dim1", "Dim2")
      selected_x <- "Dim1"
      selected_y <- "Dim2"
    }
  }

  return(list(
    choices_x = choices_x,
    choices_y = choices_y,
    selected_x = selected_x,
    selected_y = selected_y
  ))
}

# ========================================
# Labeling Utility Functions
# ========================================

get_default_colors_for_items_pca <- function(items, comparison_target) {
  tryCatch({
    # Different default colors based on what we're analyzing
    if (comparison_target == "proteins") {
      default_color <- "#E0E0E0"  # Standard protein color
    } else {
      default_color <- "#2E86AB"  # Standard sample color
    }

    default_colors <- rep(default_color, length(items))
    return(default_colors)

  }, error = function(e) {
    return(rep("#E0E0E0", length(items)))
  })
}

create_pca_label_data <- function(items_to_label, plot_data, item_settings = NULL, comparison_target = "samples") {
  tryCatch({
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    # Filter plot_data for items to label
    label_data <- plot_data[plot_data$Name %in% items_to_label, ]

    if (nrow(label_data) == 0) {
      return(NULL)
    }

    # Add label colors
    label_data$LabelColor <- "#000000"  # Default
    label_data$CustomDotColor <- "#E0E0E0"  # Default
    label_data$UseCustomDotColor <- FALSE

    # Apply individual settings if provided
    if (!is.null(item_settings) && nrow(item_settings) > 0) {
      for (i in 1:nrow(label_data)) {
        item_name <- label_data$Name[i]
        setting_row <- item_settings[item_settings$item_id == item_name, ]

        if (nrow(setting_row) > 0) {
          label_data$LabelColor[i] <- setting_row$label_color[1]
          label_data$CustomDotColor[i] <- setting_row$dot_color[1]
          label_data$UseCustomDotColor[i] <- setting_row$use_custom_dot_color[1]
        }
      }
    }

    return(label_data)

  }, error = function(e) {
    return(NULL)
  })
}

apply_labels_to_pca_plot <- function(base_plot, label_data, input, labeled_dot_size = 2, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(...) invisible(NULL)
  if (is.null(label_data) || nrow(label_data) == 0) {
    return(base_plot)
  }

  # Get labeling settings
  max_overlaps <- input$maxOverlaps_pca %||% 10
  label_distance <- input$labelDistance_pca %||% 0.25
  line_thickness <- input$lineThickness_pca %||% 0.5
  label_size <- as.numeric(input$labelSize_pca %||% 8)

  debug_log(paste("Applying PCA labels with dot size:", labeled_dot_size), 2)

  # Add custom colored dots if enabled
  custom_dots <- label_data[label_data$UseCustomDotColor == TRUE & !is.na(label_data$CustomDotColor), ]
  if (nrow(custom_dots) > 0) {
    debug_log(paste("Adding", nrow(custom_dots), "custom colored dots with size", labeled_dot_size), 2)
    base_plot <- base_plot +
      geom_point(
        data = custom_dots,
        aes(x = x, y = y),
        color = custom_dots$CustomDotColor,
        size = labeled_dot_size,  # Use the parameter instead of hardcoded value
        alpha = 0.8
      )
  }

  # Also add default colored dots for labeled proteins without custom colors
  default_dots <- label_data[label_data$UseCustomDotColor == FALSE | is.na(label_data$CustomDotColor), ]
  if (nrow(default_dots) > 0) {
    debug_log(paste("Adding", nrow(default_dots), "default colored dots with size", labeled_dot_size), 2)
    base_plot <- base_plot +
      geom_point(
        data = default_dots,
        aes(x = x, y = y),
        color = "#E0E0E0",  # Default dot color
        size = labeled_dot_size,
        alpha = 0.8
      )
  }

  # Add labels with individual colors
  labeled_plot <- base_plot +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(x = x, y = y, label = Name, color = I(LabelColor)),
      size = label_size,
      max.overlaps = max_overlaps,
      nudge_x = label_distance,
      nudge_y = label_distance,
      min.segment.length = 0,
      segment.size = line_thickness,
      show.legend = FALSE
    )

  return(labeled_plot)
}
