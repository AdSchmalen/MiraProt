# ==============================================================================

# volcano_data_processing.R
# ==============================================================================
#
# PURPOSE:
#   All data-layer functions for the volcano module: column pairing (metadata-
#   first with pattern-based fallback), safe data preparation with
#   transformation handling, axis range calculation, plot title generation,
#   and protein search/filter parsing.
#
# ARCHITECTURAL ROLE:
#   Data Processing -- pure functions with no Shiny dependencies. Every
#   function receives data as arguments and returns results without accessing
#   reactive state, input, output, or session.
#
# RESPONSIBILITIES:
#   - Intelligent column pairing (find_ratio_pvalue_pairs_smart and helpers)
#   - Safe data preparation with transformation metadata
#     (prepare_volcano_plot_data_safe)
#   - Plot title generation from column pair metadata
#     (generate_plot_title_from_pair)
#   - Axis range and tick spacing calculation
#     (calculate_optimal_ranges, calculate_nice_tick_spacing)
#   - Protein search/filter string parsing (get_filter_string_Volcano)
#   - Column query helpers for UI (get_ratio_columns, get_pvalue_columns)
#   - Pairing validation (validate_pairing)
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Plot creation or styling (those are in volcano_plot_static.R)
#   - Direct access to reactive state, input, output, or session
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - None (self-contained data processing)
#   External packages:
#     - base: adist (string distance for similarity matching)
#
# INTERACTIONS:
#   Called by:
#     - volcano_module.R: pairing, title generation, optimal ranges, search
#     - volcano_plot_static.R: prepare_volcano_plot_data_safe
#     - volcano_plot_interactive.R: prepare_volcano_plot_data_safe
#   Calls into:
#     - None
#   Data flow:
#     - IN:  raw data frames, data_def metadata, input parameters
#     - OUT: column pairs, transformed data frames, axis ranges, titles
#
# LAST UPDATED: 2026-03-10
# ==============================================================================

# Compact, deterministic signature of the canonical input pair used to build
# Volcano plots.  Keep this shared between plot generation and session capture:
# the latter may only fall back to live data when it is still the plotted data.
.volcano_data_signature <- function(data, data_def) {
  if (!is.data.frame(data) || !is.data.frame(data_def)) return(NA_character_)
  small_fp <- function(df, n = 5L) {
    nr <- min(nrow(df), n)
    nc <- min(ncol(df), n)
    if (nr == 0L || nc == 0L) return("empty")
    sl <- df[seq_len(nr), seq_len(nc), drop = FALSE]
    raw <- serialize(sl, NULL, version = 2)
    as.character(sum(as.integer(raw)) %% 1000000007L)
  }
  paste0(
    nrow(data), "x", ncol(data), ":",
    nrow(data_def), "x", ncol(data_def), ":",
    paste(colnames(data), collapse = "|"), "::",
    paste(colnames(data_def), collapse = "|"), "::",
    small_fp(data), ":", small_fp(data_def)
  )
}


# ============================================================================
# SECTION 1: Plot Data Preparation
# ============================================================================

# ========================================
# Safe Data Preparation (Canonical Implementation)
# ========================================

prepare_volcano_plot_data_safe <- function(data, data_def, ratio_idx, pval_idx, debug_log) {

  tryCatch({
    ratio_idx <- suppressWarnings(as.integer(ratio_idx[1]))
    pval_idx <- suppressWarnings(as.integer(pval_idx[1]))
    if (!is.data.frame(data) || !is.data.frame(data_def) ||
        !is.finite(ratio_idx) || !is.finite(pval_idx) ||
        ratio_idx < 1L || pval_idx < 1L ||
        ratio_idx > ncol(data) || pval_idx > ncol(data) ||
        ratio_idx > nrow(data_def) || pval_idx > nrow(data_def)) {
      debug_log(paste(
        "Skipping volcano data preparation: ratio/p-value indices do not match cached data dimensions",
        "ratio_idx=", ratio_idx, "pval_idx=", pval_idx,
        "data_cols=", if (is.data.frame(data)) ncol(data) else NA_integer_,
        "metadata_rows=", if (is.data.frame(data_def)) nrow(data_def) else NA_integer_
      ), 1)
      return(NULL)
    }

    # Extract raw data
    ratio_values <- data[, ratio_idx]
    pval_values <- data[, pval_idx]

    # Create plot dataframe with row indices for later mapping
    plot_df <- data.frame(
      x = ratio_values,
      y = pval_values,
      row_idx = seq_len(nrow(data)),
      stringsAsFactors = FALSE
    )

    # Robust numeric conversion
    if (!is.numeric(plot_df$x)) {
      debug_log("Ratio column is not numeric - attempting conversion", 2)
      x_numeric <- suppressWarnings(as.numeric(plot_df$x))
      na_introduced <- sum(is.na(x_numeric) & !is.na(plot_df$x))
      if (na_introduced > 0) {
        debug_log(paste("Conversion of ratio column introduced", na_introduced, "NA values"), 2)
      }
      plot_df$x <- x_numeric
    }

    if (!is.numeric(plot_df$y)) {
      debug_log("P-value column is not numeric - attempting conversion", 2)
      y_numeric <- suppressWarnings(as.numeric(plot_df$y))
      na_introduced <- sum(is.na(y_numeric) & !is.na(plot_df$y))
      if (na_introduced > 0) {
        debug_log(paste("Conversion of p-value column introduced", na_introduced, "NA values"), 2)
      }
      plot_df$y <- y_numeric
    }

    # If everything is NA, abort cleanly
    if (all(is.na(plot_df$x)) || all(is.na(plot_df$y))) {
      debug_log("All x or y values are NA after numeric conversion - skipping this plot", 1)
      return(NULL)
    }

    # Handle transformations safely
    if ("Transformation" %in% names(data_def) && nrow(data_def) >= max(ratio_idx, pval_idx)) {

      # Ratio transformation to log2
      ratio_transform <- data_def$Transformation[ratio_idx]
      if (!is.na(ratio_transform) && nzchar(ratio_transform) && ratio_transform != "none") {
        if (ratio_transform != "log2") {
          plot_df$x <- switch(ratio_transform,
                              "log10" = 10^plot_df$x,
                              "-log10" = 10^(-plot_df$x),
                              "-log2" = 2^(-plot_df$x),
                              plot_df$x)
          plot_df$x <- suppressWarnings(log2(plot_df$x))
        }
      } else {
        finite_x <- plot_df$x[is.finite(plot_df$x)]
        if (length(finite_x) > 0 && any(finite_x <= 0, na.rm = TRUE)) {
          debug_log("Ratio values include <=0 with 'none'/missing transform; treating as already log-scale", 1)
        } else {
          plot_df$x <- suppressWarnings(log2(plot_df$x))
        }
      }

      # P-value transformation to -log10
      pval_transform <- data_def$Transformation[pval_idx]
      if (!is.na(pval_transform) && nzchar(pval_transform) && pval_transform != "none") {
        if (pval_transform != "-log10") {
          plot_df$y <- switch(pval_transform,
                              "log10" = 10^plot_df$y,
                              "log2" = 2^plot_df$y,
                              "-log2" = 2^(-plot_df$y),
                              plot_df$y)
          plot_df$y <- suppressWarnings(-log10(plot_df$y))
        }
      } else {
        finite_y <- plot_df$y[is.finite(plot_df$y)]
        if (length(finite_y) > 0 && max(finite_y, na.rm = TRUE) > 1) {
          debug_log("P-values exceed 1 with 'none'/missing transform; treating as already -log10 scale", 1)
        } else {
          plot_df$y <- suppressWarnings(-log10(plot_df$y))
        }
      }
    } else {
      # Default transformations with heuristics for already-transformed inputs
      finite_x <- plot_df$x[is.finite(plot_df$x)]
      if (length(finite_x) > 0 && any(finite_x <= 0, na.rm = TRUE)) {
        debug_log("No transformation metadata and ratio has <=0 values; treating x as already log-scale", 1)
      } else {
        plot_df$x <- suppressWarnings(log2(plot_df$x))
      }

      finite_y <- plot_df$y[is.finite(plot_df$y)]
      if (length(finite_y) > 0 && max(finite_y, na.rm = TRUE) > 1) {
        debug_log("No transformation metadata and p-values >1; treating y as already -log10 scale", 1)
      } else {
        plot_df$y <- suppressWarnings(-log10(plot_df$y))
      }
    }

    # Remove non-finite values
    valid_rows <- is.finite(plot_df$x) & is.finite(plot_df$y)
    plot_df <- plot_df[valid_rows, ]

    debug_log(paste("Removed", sum(!valid_rows), "invalid rows, remaining:", nrow(plot_df)), 2)

    return(plot_df)

  }, error = function(e) {
    debug_log(paste("Error in data preparation:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# Plot Title Generation
# ========================================

generate_plot_title_from_pair <- function(pair) {
  title <- gsub("^Abundance Ratio:\\s*", "", pair$ratio_col)
  title <- trimws(title)
  if (!nzchar(title)) title <- "Volcano Plot"
  return(title)
}

has_valid_cached_pairing_result <- function(pairing_result, source_data_signature = NULL) {
  if (!is.list(pairing_result) ||
      is.null(pairing_result$pairs) ||
      length(pairing_result$pairs) == 0) {
    return(FALSE)
  }

  if (!is.null(source_data_signature) &&
      is.character(source_data_signature) &&
      nzchar(source_data_signature)) {
    cached_signature <- if (!is.null(pairing_result$source_data_signature)) {
      pairing_result$source_data_signature
    } else {
      NA_character_
    }
    if (!is.character(cached_signature) ||
        !nzchar(cached_signature) ||
        !identical(cached_signature, source_data_signature)) {
      return(FALSE)
    }
  }

  TRUE
}

# ========================================
# Axis Range and Tick Calculation
# ========================================

calculate_optimal_ranges <- function(data, data_def, debug_log, pairs = NULL) {

  debug_log("Calculating optimal axis ranges and tick spacing from data", 2)

  tryCatch({
    abundance_cols <- which(grepl("^Abundance Ratio$", data_def$Content))

    if (length(abundance_cols) == 0) {
      debug_log("No abundance ratio columns found for range calculation", 1)
      stop("No abundance ratio columns")
    }

    all_x <- c()
    all_y <- c()

    valid_pairs <- list()
    if (!is.null(pairs) && is.list(pairs) && length(pairs) > 0) {
      valid_pairs <- Filter(function(pair) {
        if (!is.list(pair) || is.null(pair$ratio_idx) || is.null(pair$pval_idx)) return(FALSE)
        ratio_idx <- suppressWarnings(as.integer(pair$ratio_idx[1]))
        pval_idx <- suppressWarnings(as.integer(pair$pval_idx[1]))
        is.finite(ratio_idx) && is.finite(pval_idx) &&
          ratio_idx >= 1 && pval_idx >= 1 &&
          ratio_idx <= ncol(data) && pval_idx <= ncol(data) &&
          ratio_idx <= nrow(data_def) && pval_idx <= nrow(data_def)
      }, pairs)
    }

    if (length(valid_pairs) > 0) {
      debug_log(paste("Using", length(valid_pairs), "validated metadata pairs for range calculation"), 2)

      for (pair in valid_pairs) {
        ratio_idx <- as.integer(pair$ratio_idx[1])
        pval_idx <- as.integer(pair$pval_idx[1])

        plot_df <- prepare_volcano_plot_data_safe(data, data_def, ratio_idx, pval_idx, debug_log)

        if (!is.null(plot_df) && nrow(plot_df) > 0) {
          all_x <- c(all_x, plot_df$x)
          all_y <- c(all_y, plot_df$y)
        }
      }
    } else {
      debug_log("No validated metadata pairs available for range calculation; using positional fallback", 2)

      pval_cols_adj <- which(grepl("Adj.*p-Value$", data_def$Content))
      pval_cols_raw <- which(grepl("p-Value$", data_def$Content) & !grepl("Adj", data_def$Content))

      for (i in seq_along(abundance_cols)) {
        ratio_idx <- abundance_cols[i]

        if (length(pval_cols_adj) >= i) {
          pval_idx <- pval_cols_adj[i]
        } else if (length(pval_cols_raw) >= i) {
          pval_idx <- pval_cols_raw[i]
        } else {
          next
        }

        plot_df <- prepare_volcano_plot_data_safe(data, data_def, ratio_idx, pval_idx, debug_log)

        if (!is.null(plot_df) && nrow(plot_df) > 0) {
          all_x <- c(all_x, plot_df$x)
          all_y <- c(all_y, plot_df$y)
        }
      }
    }

    if (length(all_x) == 0 || length(all_y) == 0) {
      debug_log("No valid data points found for range calculation", 1)
      stop("No valid points")
    }

    all_x <- all_x[is.finite(all_x)]
    all_y <- all_y[is.finite(all_y)]

    if (length(all_x) == 0 || length(all_y) == 0) {
      debug_log("No finite x/y values after cleaning", 1)
      stop("No finite points")
    }

    # X-axis: symmetric around 0 (log2 fold change)
    x_abs_max <- max(abs(all_x))
    x_padding <- max(0.5, x_abs_max * 0.08)
    x_limit <- ceiling(x_abs_max + x_padding)
    if (x_limit < 1) x_limit <- 1

    x_min <- -x_limit
    x_max <-  x_limit

    if (x_abs_max <= 2) {
      x_tick_optimal <- 0.5
    } else if (x_abs_max <= 6) {
      x_tick_optimal <- 1
    } else {
      x_tick_optimal <- 2
    }

    debug_log(paste("X-axis symmetric range -", x_min, "to", x_max,
                    "(padding:", round(x_padding, 3), ") with tick", x_tick_optimal), 2)

    # Y-axis: 0 to max (-log10 p)
    y_min <- 0
    y_max_raw <- max(all_y)
    y_padding <- max(0.5, y_max_raw * 0.08)
    y_max <- ceiling(y_max_raw + y_padding)
    if (y_max < 2) y_max <- 2

    y_span <- y_max - y_min
    y_tick_optimal <- calculate_nice_tick_spacing(y_span, target_ticks = 6)

    debug_log(paste("Y-axis range -", y_min, "to", y_max,
                    "(padding:", round(y_padding, 3), ") span", y_span, "tick", y_tick_optimal), 2)

    optimal_ranges <- list(
      x_range = c(x_min, x_max),
      y_range = c(y_min, y_max),
      x_tick = x_tick_optimal,
      y_tick = y_tick_optimal
    )

    debug_log(paste("Calculated optimal ticks - X:", optimal_ranges$x_tick,
                    "Y:", optimal_ranges$y_tick), 1)

    return(optimal_ranges)

  }, error = function(e) {
    debug_log(paste("Error calculating optimal ranges:", e$message), 1)
    return(list(x_range = c(-8, 8), y_range = c(0, 18), x_tick = 2, y_tick = 2))
  })
}

calculate_nice_tick_spacing <- function(range_span, target_ticks = 6) {
  raw_spacing <- range_span / target_ticks
  if (raw_spacing <= 0.5) return(0.5)
  if (raw_spacing <= 1) return(1)
  if (raw_spacing <= 2) return(2)
  if (raw_spacing <= 5) return(5)
  if (raw_spacing <= 10) return(10)
  return(round(raw_spacing / 5) * 5)
}

# ========================================
# Protein Search and Filter Parsing
# ========================================

get_filter_string_Volcano <- function(input_text, selected_identifier, debug_log) {
  debug_log("Parsing protein input text", 2)

  lines <- unlist(strsplit(input_text, "\n"))
  lines <- trimws(lines[lines != ""])
  num_lines <- length(lines)

  if (num_lines == 0) return(data.frame())

  df <- data.frame(matrix(nrow = num_lines, ncol = 1))
  colnames(df) <- c(selected_identifier)

  for (i in 1:num_lines) {
    line <- unlist(strsplit(lines[i], "[,\\s]+"))
    df[i, selected_identifier] <- line[1]
  }

  debug_log(paste("Parsed", num_lines, "protein identifiers"), 2)
  return(df)
}


# ============================================================================
# SECTION 2: Column Pairing (Metadata-First with Pattern Fallback)
# ============================================================================

# ========================================
# Main Pairing Function
# ========================================

find_ratio_pvalue_pairs_smart <- function(data_def, pval_type_selection, debug_log) {

  debug_log("Starting metadata-based column pairing", 1)

  # Primary approach: Use metadata content and numerator/denominator
  metadata_result <- find_pairs_by_metadata(data_def, pval_type_selection, debug_log)

  if (metadata_result$success && length(metadata_result$pairs) > 0) {
    debug_log(paste("Metadata pairing successful:", length(metadata_result$pairs), "pairs"), 1)
    metadata_result$has_ambiguous <- any(sapply(metadata_result$pairs, function(p) isTRUE(p$ambiguous)))
    return(metadata_result)
  }

  # Fallback: Pattern-based detection
  debug_log("Metadata pairing failed, trying pattern-based approach", 1)
  pattern_result <- find_pairs_by_pattern(data_def, pval_type_selection, debug_log)

  if (pattern_result$success) {
    debug_log(paste("Pattern pairing successful:", length(pattern_result$pairs), "pairs"), 1)
    pattern_result$has_ambiguous <- any(sapply(pattern_result$pairs, function(p) isTRUE(p$ambiguous)))
    return(pattern_result)
  }

  debug_log("All pairing methods failed", 1)
  return(list(pairs = list(), success = FALSE, has_ambiguous = FALSE))
}

# ========================================
# Metadata-Based Pairing (Primary Method)
# ========================================

find_pairs_by_metadata <- function(data_def, pval_type_selection, debug_log) {

  debug_log("Analyzing metadata structure", 2)

  ratio_indices <- which(data_def$Content == "Abundance Ratio")

  if (length(ratio_indices) == 0) {
    debug_log("No abundance ratio columns in metadata", 1)
    return(list(pairs = list(), success = FALSE))
  }

  pval_type_fallback <- FALSE

  if (pval_type_selection == "Adjusted p-value") {
    pval_content_type <- "Abundance Ratio Adj. p-Value"
    pval_indices <- which(data_def$Content == pval_content_type)

    # If no adj. p-value columns, fall back to raw p-value
    if (length(pval_indices) == 0) {
      debug_log("No Abundance Ratio Adj. p-Value columns found - using p-Value", 1)
      pval_content_type <- "Abundance Ratio p-Value"
      pval_indices <- which(data_def$Content == pval_content_type)
      pval_type_fallback <- TRUE
    }
  } else {
    pval_content_type <- "Abundance Ratio p-Value"
    pval_indices <- which(data_def$Content == pval_content_type)

    # If no raw p-value columns, fall back to adj. p-value
    if (length(pval_indices) == 0) {
      debug_log("No Abundance Ratio p-Value columns found - using Adj. p-Value", 1)
      pval_content_type <- "Abundance Ratio Adj. p-Value"
      pval_indices <- which(data_def$Content == pval_content_type)
      pval_type_fallback <- TRUE
    }
  }

  debug_log(paste("Found", length(ratio_indices), "ratio columns,", length(pval_indices), "p-value columns"), 2)

  if (length(pval_indices) == 0) {
    debug_log(paste("No", pval_content_type, "columns found"), 1)
    return(list(pairs = list(), success = FALSE))
  }

  ratio_groups <- group_by_numerator_denominator(ratio_indices, data_def, debug_log, "ratio")
  pval_groups <- group_by_numerator_denominator(pval_indices, data_def, debug_log, "p-value")

  pairs <- match_metadata_groups(ratio_groups, pval_groups, data_def, pval_type_selection, debug_log)

  debug_log(paste("Created", length(pairs), "metadata-based pairs"), 1)

  return(list(pairs = pairs, success = length(pairs) > 0, pval_type_fallback = pval_type_fallback))
}

group_by_numerator_denominator <- function(indices, data_def, debug_log, label = "columns") {
  groups <- list()

  for (idx in indices) {
    numerator <- if ("Numerator" %in% names(data_def)) data_def$Numerator[idx] else ""
    denominator <- if ("Denominator" %in% names(data_def)) data_def$Denominator[idx] else ""

    numerator <- trimws(as.character(numerator))
    denominator <- trimws(as.character(denominator))

    if (is.na(numerator) || numerator == "") numerator <- "unknown"
    if (is.na(denominator) || denominator == "") denominator <- "unknown"

    group_key <- paste(numerator, denominator, sep = "_vs_")

    if (group_key %in% names(groups)) {
      groups[[group_key]] <- c(groups[[group_key]], idx)
    } else {
      groups[[group_key]] <- idx
    }
  }

  debug_log(paste("Created", length(groups), label, "numerator/denominator groups"), 2)
  return(groups)
}

match_metadata_groups <- function(ratio_groups, pval_groups, data_def, pval_type_selection, debug_log) {
  pairs <- list()

  for (group_key in names(ratio_groups)) {
    ratio_indices <- ratio_groups[[group_key]]

    if (group_key %in% names(pval_groups)) {
      pval_indices <- pval_groups[[group_key]]

      debug_log(paste("Matching group", group_key, ":", length(ratio_indices), "ratios,", length(pval_indices), "p-values"), 2)

      group_pairs <- create_pairs_within_group(ratio_indices, pval_indices, data_def, pval_type_selection, debug_log)
      pairs <- c(pairs, group_pairs)
    } else {
      debug_log(paste("No matching p-value group for", group_key), 2)
    }
  }

  return(pairs)
}

create_pairs_within_group <- function(ratio_indices, pval_indices, data_def, pval_type_selection, debug_log) {
  pairs <- list()

  # Direct pairing for single ratio/pval
  if (length(ratio_indices) == 1 && length(pval_indices) == 1) {
    ratio_idx <- ratio_indices[1]
    pval_idx <- pval_indices[1]

    pair <- list(
      ratio_idx = ratio_idx,
      pval_idx = pval_idx,
      ratio_col = data_def$Column[ratio_idx],
      pval_col = data_def$Column[pval_idx],
      pval_type = if (pval_type_selection == "Adjusted p-value") "Adjusted p-value" else "p-value",
      confidence = 1.0
    )

    pairs[[length(pairs) + 1]] <- pair
    debug_log(paste("Direct pair:", pair$ratio_col, "with", pair$pval_col), 2)
    return(pairs)
  }

  # Single ratio with multiple p-value columns (likely mislabeled metadata)
  if (length(ratio_indices) == 1 && length(pval_indices) > 1) {
    ratio_idx <- ratio_indices[1]
    pval_idx <- pval_indices[1]  # pick the first one

    pair <- list(
      ratio_idx = ratio_idx,
      pval_idx = pval_idx,
      ratio_col = data_def$Column[ratio_idx],
      pval_col = data_def$Column[pval_idx],
      pval_type = if (pval_type_selection == "Adjusted p-value") "Adjusted p-value" else "p-value",
      confidence = 0.5,
      ambiguous = TRUE
    )

    pairs[[length(pairs) + 1]] <- pair
    debug_log(paste("AMBIGUOUS: Multiple p-value columns for ratio",
                    data_def$Column[ratio_idx], "- using first:",
                    data_def$Column[pval_idx],
                    ". Please check metadata assignment."), 1)
    return(pairs)
  }

  # For multiple columns, use similarity-based matching
  all_indices <- c(ratio_indices, pval_indices)
  all_columns <- data_def$Column[all_indices]

  n_cols <- length(all_columns)
  similarity_matrix <- matrix(0, nrow = n_cols, ncol = n_cols)

  for (i in 1:(n_cols-1)) {
    for (j in (i+1):n_cols) {
      similarity <- calculate_metadata_similarity(all_columns[i], all_columns[j])
      similarity_matrix[i, j] <- similarity_matrix[j, i] <- similarity
    }
  }

  used_pval_indices <- numeric()

  for (ratio_pos in seq_along(ratio_indices)) {
    ratio_idx <- ratio_indices[ratio_pos]
    ratio_col_name <- data_def$Column[ratio_idx]

    best_match <- NULL
    best_similarity <- 0
    candidate_count <- 0
    ambiguity_threshold <- 0.05  # similarities within this range are considered tied

    # First pass: find the best similarity score
    for (pval_pos in seq_along(pval_indices)) {
      pval_idx <- pval_indices[pval_pos]
      if (pval_idx %in% used_pval_indices) next

      similarity <- similarity_matrix[ratio_pos, length(ratio_indices) + pval_pos]

      if (similarity > best_similarity) {
        best_similarity <- similarity
        best_match <- list(
          index = pval_idx,
          column = data_def$Column[pval_idx],
          confidence = similarity
        )
      }
    }

    # Second pass: count how many candidates are equally likely (within threshold)
    if (!is.null(best_match)) {
      for (pval_pos in seq_along(pval_indices)) {
        pval_idx <- pval_indices[pval_pos]
        if (pval_idx %in% used_pval_indices) next
        similarity <- similarity_matrix[ratio_pos, length(ratio_indices) + pval_pos]
        if (abs(similarity - best_similarity) <= ambiguity_threshold) {
          candidate_count <- candidate_count + 1
        }
      }
    }

    ambiguous <- candidate_count > 1

    if (!is.null(best_match) && best_similarity > 0.3) {
      pair <- list(
        ratio_idx = ratio_idx,
        pval_idx = best_match$index,
        ratio_col = ratio_col_name,
        pval_col = best_match$column,
        pval_type = if (pval_type_selection == "Adjusted p-value") "Adjusted p-value" else "p-value",
        confidence = best_match$confidence,
        ambiguous = ambiguous
      )

      pairs[[length(pairs) + 1]] <- pair
      used_pval_indices <- c(used_pval_indices, best_match$index)

      if (ambiguous) {
        debug_log(paste("AMBIGUOUS pair (", candidate_count, "equally likely candidates):",
                        ratio_col_name, "with", best_match$column,
                        "- using first match. Check metadata for correctness."), 1)
      } else {
        debug_log(paste("Intelligent pair:", ratio_col_name, "with", best_match$column,
                        "similarity:", round(best_similarity, 3)), 2)
      }
    }
  }

  return(pairs)
}

# ========================================
# Column Name Similarity Calculation
# ========================================

calculate_metadata_similarity <- function(ratio_name, pval_name) {
  ratio_id <- extract_column_identifier(ratio_name, "Abundance Ratio")
  pval_id <- extract_column_identifier(pval_name, "Abundance Ratio.*p-Value")

  if (ratio_id == pval_id && ratio_id != ratio_name) return(1.0)

  ratio_struct <- analyze_column_structure(ratio_name, 1)
  pval_struct <- analyze_column_structure(pval_name, 2)

  structural_sim <- calculate_structural_similarity(ratio_struct, pval_struct)
  pattern_bonus <- detect_related_patterns(ratio_name, pval_name)

  return(min(1.0, structural_sim + pattern_bonus))
}

extract_column_identifier <- function(column_name, prefix_pattern) {
  identifier <- sub(paste0("^", prefix_pattern, ":\\s*"), "", column_name)
  identifier <- trimws(identifier)

  if (identifier == column_name) {
    identifier <- sub(paste0("^", prefix_pattern, "\\s+"), "", column_name)
    identifier <- trimws(identifier)
  }

  return(identifier)
}

detect_related_patterns <- function(name1, name2) {
  norm1 <- tolower(name1)
  norm2 <- tolower(name2)

  prefix_bonus <- detect_same_prefix_pattern(norm1, norm2)
  if (prefix_bonus > 0) return(prefix_bonus)

  suffix_bonus <- detect_same_suffix_pattern(norm1, norm2)
  if (suffix_bonus > 0) return(suffix_bonus)

  keyword_bonus <- detect_related_keywords(norm1, norm2)
  if (keyword_bonus > 0) return(keyword_bonus)

  return(0.0)
}

detect_same_prefix_pattern <- function(name1, name2) {
  prefix1 <- sub("^([^\\-_\\s\\.]+).*", "\\1", name1)
  prefix2 <- sub("^([^\\-_\\s\\.]+).*", "\\1", name2)
  if (prefix1 == prefix2 && nchar(prefix1) >= 2) return(0.3)
  return(0.0)
}

detect_same_suffix_pattern <- function(name1, name2) {
  suffix1 <- sub(".*[\\-_\\s\\.]([^\\-_\\s\\.]+)$", "\\1", name1)
  suffix2 <- sub(".*[\\-_\\s\\.]([^\\-_\\s\\.]+)$", "\\1", name2)
  if (suffix1 == suffix2 && nchar(suffix1) >= 2) return(0.3)
  return(0.0)
}

detect_related_keywords <- function(name1, name2) {
  ratio_keywords <- c("ratio", "fold", "fc", "abundance", "r")
  pval_keywords <- c("pval", "pvalue", "p-val", "p-value", "p", "sig", "significance")
  adjpval_keywords <- c("adjp", "adj-p", "adjpval", "adj-pval", "adjusted", "fdr", "q", "qval")

  contains_ratio <- any(sapply(ratio_keywords, function(kw) grepl(kw, name1, fixed = TRUE)))
  contains_pval <- any(sapply(pval_keywords, function(kw) grepl(kw, name2, fixed = TRUE)))
  contains_adjpval <- any(sapply(adjpval_keywords, function(kw) grepl(kw, name2, fixed = TRUE)))

  if (contains_ratio && (contains_pval || contains_adjpval)) return(0.2)
  if (contains_pval && contains_adjpval) return(0.15)
  return(0.0)
}

# ========================================
# Pattern-Based Pairing (Fallback Method)
# ========================================

find_pairs_by_pattern <- function(data_def, pval_type_selection, debug_log) {
  debug_log("Using pattern-based detection as fallback (flexible metadata matching)", 2)

  all_columns <- data_def$Column
  content <- data_def$Content
  n_cols <- length(all_columns)

  if (n_cols < 2) return(list(pairs = list(), success = FALSE))

  # Step 1: Classify columns using data_def$Content with flexible grepl matching
  ratio_indices <- which(grepl("^Abundance Ratio$", content))
  adj_pval_indices <- which(grepl("Adj.*p-Value", content, ignore.case = TRUE))
  raw_pval_indices <- which(grepl("p-Value", content, ignore.case = TRUE) &
                              !grepl("Adj", content, ignore.case = TRUE))
  all_pval_indices <- which(grepl("p-Value", content, ignore.case = TRUE))

  debug_log(paste("Flexible metadata classification:", length(ratio_indices), "ratio,",
                  length(raw_pval_indices), "raw p-value,",
                  length(adj_pval_indices), "adj p-value columns"), 1)

  if (length(ratio_indices) == 0 || length(all_pval_indices) == 0) {
    debug_log("Could not identify both ratio and p-value columns in metadata Content", 1)
    return(list(pairs = list(), success = FALSE))
  }

  # Step 2: Select which p-value columns to use based on user selection
  if (pval_type_selection == "Adjusted p-value" && length(adj_pval_indices) > 0) {
    target_pval_indices <- adj_pval_indices
    pval_type_label <- "Adjusted p-value"
  } else if (length(raw_pval_indices) > 0) {
    target_pval_indices <- raw_pval_indices
    pval_type_label <- "p-value"
  } else {
    target_pval_indices <- all_pval_indices
    pval_type_label <- "p-value"
  }

  debug_log(paste("Using", length(target_pval_indices), pval_type_label, "columns for pairing"), 2)

  # Step 3: Match ratio columns to p-value columns using column name similarity
  pairs <- find_pattern_pairs(ratio_indices, target_pval_indices, all_columns,
                              pval_type_label, debug_log)

  debug_log(paste("Pattern detection found", length(pairs), "pairs"), 2)

  return(list(pairs = pairs, success = length(pairs) > 0))
}

find_pattern_pairs <- function(ratio_indices, pval_indices, all_columns,
                               pval_type_label, debug_log) {
  pairs <- list()

  # Build similarity matrix between ratio and p-value columns
  n_ratio <- length(ratio_indices)
  n_pval <- length(pval_indices)

  # Extract the "identifier" part from each column name for matching
  # e.g. "Abundance Ratio: Treatment / Control" -> "Treatment / Control"
  # e.g. "Abundance Ratio p-Value: Treatment / Control" -> "Treatment / Control"
  ratio_ids <- sapply(ratio_indices, function(i) extract_comparison_identifier(all_columns[i]))
  pval_ids <- sapply(pval_indices, function(i) extract_comparison_identifier(all_columns[i]))

  used_pval <- logical(n_pval)

  for (ri in seq_len(n_ratio)) {
    best_pval_pos <- NULL
    best_similarity <- -1
    candidate_count <- 0
    ambiguity_threshold <- 0.05

    for (pi in seq_len(n_pval)) {
      if (used_pval[pi]) next

      # Compare the identifier parts
      sim <- calculate_string_similarity(ratio_ids[ri], pval_ids[pi])

      if (sim > best_similarity) {
        best_similarity <- sim
        best_pval_pos <- pi
      }
    }

    # Count ambiguous candidates
    if (!is.null(best_pval_pos)) {
      for (pi in seq_len(n_pval)) {
        if (used_pval[pi]) next
        sim <- calculate_string_similarity(ratio_ids[ri], pval_ids[pi])
        if (abs(sim - best_similarity) <= ambiguity_threshold) {
          candidate_count <- candidate_count + 1
        }
      }
    }

    ambiguous <- candidate_count > 1

    # Require reasonable similarity (> 0.3) for a valid pair
    if (!is.null(best_pval_pos) && best_similarity > 0.3) {
      pair <- list(
        ratio_idx = ratio_indices[ri],
        pval_idx = pval_indices[best_pval_pos],
        ratio_col = all_columns[ratio_indices[ri]],
        pval_col = all_columns[pval_indices[best_pval_pos]],
        pval_type = pval_type_label,
        confidence = best_similarity,
        ambiguous = ambiguous
      )

      pairs[[length(pairs) + 1]] <- pair
      used_pval[best_pval_pos] <- TRUE

      if (ambiguous) {
        debug_log(paste("AMBIGUOUS pattern pair (", candidate_count, "candidates):",
                        pair$ratio_col, "with", pair$pval_col), 1)
      } else {
        debug_log(paste("Pattern pair:", pair$ratio_col, "with", pair$pval_col,
                        "similarity:", round(best_similarity, 3)), 2)
      }
    } else {
      debug_log(paste("No matching p-value column for ratio:",
                      all_columns[ratio_indices[ri]]), 1)
    }
  }

  return(pairs)
}

# Extract the comparison identifier from a column name
# Strips common prefixes like "Abundance Ratio:", "Abundance Ratio p-Value:" etc.
extract_comparison_identifier <- function(column_name) {
  id <- column_name

  # Remove common prefixes (order matters - try most specific first)
  prefixes <- c(
    "Abundance Ratio Adj\\. p-Value:\\s*",
    "Abundance Ratio Adj\\.p-Value:\\s*",
    "Abundance Ratio p-Value:\\s*",
    "Abundance Ratio:\\s*",
    "Log2 Fold Change:\\s*",
    "Fold Change:\\s*"
  )

  for (prefix in prefixes) {
    stripped <- sub(paste0("^", prefix), "", id, ignore.case = TRUE)
    if (stripped != id) {
      id <- stripped
      break
    }
  }

  trimws(id)
}

# ========================================
# Structural Analysis for Pattern Detection
# ========================================

analyze_column_structure <- function(column_name, index) {
  normalized <- tolower(column_name)

  list(
    original = column_name,
    index = index,
    normalized = normalized,
    length = nchar(column_name),
    words = extract_word_segments(column_name),
    prefix = extract_prefix(column_name),
    suffix = extract_suffix(column_name),
    core = extract_core_part(column_name)
  )
}

extract_word_segments <- function(text) {
  segments <- unlist(strsplit(text, "[_\\-\\s\\.\\(\\)\\[\\]:]+"))
  segments[nzchar(segments)]
}

extract_prefix <- function(text) {
  if (grepl("[_\\-\\s\\.]", text)) {
    return(sub("([^_\\-\\s\\.]+).*", "\\1", text))
  }
  substr(text, 1, min(3, nchar(text)))
}

extract_suffix <- function(text) {
  if (grepl("[_\\-\\s\\.]", text)) {
    return(sub(".*[_\\-\\s\\.]([^_\\-\\s\\.]+)$", "\\1", text))
  }
  substr(text, max(1, nchar(text)-2), nchar(text))
}

extract_core_part <- function(text) {
  segments <- extract_word_segments(text)
  if (length(segments) >= 2) {
    middle_start <- max(1, floor(length(segments)/3))
    middle_end <- min(length(segments), ceiling(2*length(segments)/3))
    return(paste(segments[middle_start:middle_end], collapse = "_"))
  }
  text
}

calculate_structural_similarity <- function(struct1, struct2) {
  prefix_sim <- calculate_string_similarity(struct1$prefix, struct2$prefix)
  core_sim <- calculate_string_similarity(struct1$core, struct2$core)
  word_overlap <- calculate_word_overlap(struct1$words, struct2$words)

  prefix_sim * 0.4 + core_sim * 0.3 + word_overlap * 0.3
}

calculate_string_similarity <- function(str1, str2) {
  if (is.null(str1) || is.null(str2) || !nzchar(str1) || !nzchar(str2)) return(0)

  str1 <- tolower(trimws(str1))
  str2 <- tolower(trimws(str2))

  if (str1 == str2) return(1.0)

  max_len <- max(nchar(str1), nchar(str2))
  if (max_len == 0) return(0)

  distance <- adist(str1, str2)[1,1]
  max(0, 1 - (distance / max_len))
}

calculate_word_overlap <- function(words1, words2) {
  if (length(words1) == 0 || length(words2) == 0) return(0)

  words1 <- tolower(words1)
  words2 <- tolower(words2)

  intersection <- length(intersect(words1, words2))
  union_size <- length(union(words1, words2))

  if (union_size == 0) return(0)
  intersection / union_size
}


# ============================================================================
# SECTION 3: UI Helper and Validation Functions
# ============================================================================

# ========================================
# UI Column Query Helpers
# ========================================

get_ratio_columns <- function(data_def) {
  if (is.null(data_def) || nrow(data_def) == 0) return(character())

  ratio_idx <- which(data_def$Content == "Abundance Ratio")
  if (length(ratio_idx) == 0) return(character())

  cols <- data_def$Column[ratio_idx]
  names(cols) <- cols
  cols
}

get_pvalue_columns <- function(data_def, type = "all") {
  if (is.null(data_def) || nrow(data_def) == 0) return(character())

  if (type == "regular") {
    pval_idx <- which(data_def$Content == "Abundance Ratio p-Value")
  } else if (type == "adjusted") {
    pval_idx <- which(data_def$Content == "Abundance Ratio Adj. p-Value")
  } else {
    pval_idx <- which(data_def$Content %in% c("Abundance Ratio p-Value",
                                              "Abundance Ratio Adj. p-Value"))
  }

  if (length(pval_idx) == 0) return(character())

  cols <- data_def$Column[pval_idx]
  names(cols) <- cols
  cols
}

# ========================================
# Pairing Validation
# ========================================

validate_pairing <- function(ratio_col, pval_col, data, data_def, debug_log) {
  if (!ratio_col %in% colnames(data) || !pval_col %in% colnames(data)) {
    debug_log("Selected columns not found in data", 1)
    return(FALSE)
  }

  ratio_idx <- which(data_def$Column == ratio_col)
  pval_idx <- which(data_def$Column == pval_col)

  if (length(ratio_idx) == 0 || length(pval_idx) == 0) {
    debug_log("Columns not found in metadata", 1)
    return(FALSE)
  }

  ratio_data <- data[[ratio_col]]
  pval_data <- data[[pval_col]]

  if (!is.numeric(ratio_data) || !is.numeric(pval_data)) {
    debug_log("Non-numeric data in selected columns", 1)
    return(FALSE)
  }

  valid_ratio <- sum(is.finite(ratio_data))
  valid_pval <- sum(is.finite(pval_data))

  if (valid_ratio < 10 || valid_pval < 10) {
    debug_log("Insufficient valid data points", 1)
    return(FALSE)
  }

  pval_range <- range(pval_data[is.finite(pval_data)])
  if (pval_range[1] < 0 || pval_range[2] > 1) {
    if (pval_range[1] > 1) {
      debug_log("P-values appear to be -log10 transformed", 2)
    } else {
      debug_log("P-values outside expected range [0,1]", 1)
    }
  }

  return(TRUE)
}
