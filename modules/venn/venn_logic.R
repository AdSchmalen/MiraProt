# =============================================================================
# modules/venn/venn_logic.R
#
# Purpose:
#   Contains all pure functions and helper logic for the Venn module. No Shiny
#   reactivity, no observers, and no server logic are present in this file.
#   Functions here accept reactive values as arguments; they do not access the
#   reactive graph directly.
#
# Architectural role:
#   Logic layer of the Venn module. Sourced by Venn_module.R. Functions are
#   called from venn_observer.R (inside reactive contexts) and from download
#   handlers in venn_observer.R.
#
# File structure:
#   1. Package availability checks
#   2. Plot-container sizing helpers  -- calculate_expected_intersections(),
#                                        calculate_upset_plot_width(),
#                                        needs_horizontal_scroll(),
#                                        convert_pixels_to_inches()
#   3. Sample-choice helper          -- venn_get_sample_choices()
#   4. Input-collection helper       -- collect_input_lists()
#   5. Protein extraction helper     -- extract_sample_proteins()
#   6. Excel export helper           -- build_venn_intersection_workbook()
#   7. NULL-coalescing operator      -- `%||%`
#   8. Venn diagram creation         -- create_venn_diagram()
#   9. UpSet plot entry point        -- create_upset_plot()
#  10. UpSet plot variants           -- create_standard_upset(),
#                                        create_upset_with_abundances(),
#                                        create_upset_with_ratios()
#  11. UpSet data helpers            -- prepare_upset_plot_data(),
#                                        get_upset_panel_theme()
#  12. Data-preparation helpers      -- prepare_abundance_data(),
#                                        prepare_ratio_data()
#  13. Theme and file-save helpers   -- get_upset_theme(), save_plot_file()
#  14. Grid integration helper       -- convert_venn_to_ggplot()
#
# Notes for future developers:
#   - All functions that require logging accept debug_log as a final parameter
#     (default NULL). Pass the debug_log closure from the module server to
#     enable logging; omit it for silent use.
#   - Functions that build ComplexUpset plots read Shiny input values from the
#     passed input object; they must be called from within a reactive context.
#   - create_upset_with_abundances() and create_upset_with_ratios() accept
#     num_intersections_export as a parameter and call it as a side effect to
#     track the intersection count for dynamic plot sizing.
#   - Intersection computation for Venn-type plots is NOT done in
#     create_venn_diagram(); it is performed in venn_data_cache (Phase 1 in
#     venn_observer.R) so it only runs on button press, not on styling changes.
# =============================================================================

if (!require("VennDiagram", quietly = TRUE)) {
  stop("VennDiagram package is required but not installed")
}
if (!require("ComplexUpset", quietly = TRUE)) {
  stop("ComplexUpset package is required but not installed")
}
if (!require("ggupset", quietly = TRUE)) {
  stop("ggupset package is required but not installed")
}
if (!require("grid", quietly = TRUE)) {
  stop("grid package is required but not installed")
}
if (!require("ggplot2", quietly = TRUE)) {
  stop("ggplot2 package is required but not installed")
}

VENN_SESSION_SCHEMA_VERSION <- "2.0"


# =============================================================================
# Plot-Container Sizing
# =============================================================================

#' Calculate expected number of intersections for a given number of lists
#'
#' Used to estimate the required plot width before the plot is generated.
#' For 2-3 lists exact counts are returned; for larger sets a conservative
#' practical estimate capped at 25 is used.
#'
#' @param num_lists Number of protein lists
#' @return Estimated integer intersection count
calculate_expected_intersections <- function(num_lists) {
  if (num_lists <= 1) return(1)
  if (num_lists <= 2) return(3)
  if (num_lists <= 3) return(7)

  practical_intersections <- num_lists + (num_lists * 2)
  practical_intersections <- min(practical_intersections, 25)
  return(practical_intersections)
}

#' Calculate optimal UpSet plot width in pixels
#'
#' Guarantees a consistent per-bar width by computing the total width from
#' the number of intersections and adding fixed overhead for set sizes,
#' margins, and label overflow. Result is clamped between min and max.
#'
#' @param num_intersections Number of intersections in the plot
#' @param desired_bar_width Target width per intersection bar in pixels
#' @param set_sizes_width Width of the set-sizes panel (left side)
#' @param margins_width Total margins and spacing allowance
#' @param label_overhead Additional width for label variation
#' @param min_total_width Minimum total width in pixels
#' @param max_total_width Maximum total width in pixels
#' @return Integer total plot width in pixels
calculate_upset_plot_width <- function(num_intersections,
                                       desired_bar_width  = 40,
                                       set_sizes_width    = 200,
                                       margins_width      = 100,
                                       label_overhead     = 50,
                                       min_total_width    = 800,
                                       max_total_width    = 3000) {
  bar_spacing             <- 8
  intersection_bars_width <- (num_intersections * desired_bar_width) +
                              ((num_intersections - 1) * bar_spacing)
  calculated_width        <- set_sizes_width + intersection_bars_width +
                              margins_width + label_overhead
  max(min_total_width, min(max_total_width, calculated_width))
}

#' Return TRUE when the plot requires a horizontal scrollbar
#'
#' @param plot_width Calculated plot width in pixels
#' @param container_width Viewport threshold in pixels
#' @return Logical
needs_horizontal_scroll <- function(plot_width, container_width = 1200) {
  plot_width > container_width
}

#' Convert pixels to inches (96 DPI standard)
#'
#' @param width_pixels Width in pixels
#' @param dpi Dots per inch (default: 96)
#' @return Numeric width in inches
convert_pixels_to_inches <- function(width_pixels, dpi = 96) {
  width_pixels / dpi
}

#' Estimate additional top padding needed for UpSet plots
#'
#' @param num_sets Number of sets in the UpSet plot
#' @param annotation_count Number of active annotation panels
#' @param title_size_pt Plot title size in points
#' @return List with top_padding_px and warning flag
calculate_upset_top_padding <- function(num_sets,
                                        annotation_count = 1L,
                                        title_size_pt = 14) {
  safe_min_padding <- 90
  computed_padding <- 55 + (as.integer(num_sets) * 5) +
    (as.integer(annotation_count) * 24) +
    as.integer(round(title_size_pt * 1.4))
  list(
    top_padding_px = max(safe_min_padding, computed_padding),
    below_safe_threshold = computed_padding < safe_min_padding,
    safe_min_padding = safe_min_padding
  )
}

#' Calculate restored UpSet dimensions from rebuilt cache characteristics
#'
#' @param num_intersections Number of intersections in rebuilt data
#' @param num_sets Number of sets in rebuilt data
#' @param annotation_count Number of active annotation panels
#' @param title_size_pt Plot title size in points
#' @return List with plot_width_px, plot_height_px and top padding diagnostics
calculate_upset_plot_dimensions <- function(num_intersections,
                                            num_sets,
                                            annotation_count = 1L,
                                            title_size_pt = 14) {
  width_px <- calculate_upset_plot_width(max(1L, as.integer(num_intersections)))
  top_info <- calculate_upset_top_padding(
    num_sets = max(1L, as.integer(num_sets)),
    annotation_count = max(1L, as.integer(annotation_count)),
    title_size_pt = title_size_pt
  )
  height_px <- 620 + (max(1L, as.integer(num_sets)) * 24) +
    (max(1L, as.integer(annotation_count)) * 200) +
    top_info$top_padding_px

  list(
    plot_width_px = width_px,
    plot_height_px = height_px,
    top_padding_px = top_info$top_padding_px,
    top_padding_below_safe = top_info$below_safe_threshold,
    safe_top_padding_px = top_info$safe_min_padding
  )
}


# =============================================================================
# Sample-Choice Helper
# =============================================================================

#' Get sample names for a given abundance type from the data definition
#'
#' Looks up rows in def whose Content column exactly matches ref_val and
#' returns the unique non-empty Sample values.
#'
#' @param def Data-definition data frame
#' @param ref_val Selected abundance type string
#' @return Character vector of sample names (may be empty)
venn_get_sample_choices <- function(def, ref_val) {
  if (is.null(def) || nrow(def) == 0 || is.null(ref_val)) return(character(0))

  ref_indices  <- which(def$Content == ref_val)
  sample_names <- unique(def$Sample[ref_indices])
  sample_names <- sample_names[!is.na(sample_names) & sample_names != ""]
  return(sample_names)
}


# =============================================================================
# Input-Collection Helper
# =============================================================================

#' Collect non-empty protein lists entered by the user in the UI
#'
#' Reads text-area inputs list1..listN, splits on newlines, trims whitespace,
#' and discards empty strings. Empty lists are removed before returning.
#'
#' @param list_count Number of protein lists to collect
#' @param input Shiny input object
#' @param ns Namespace function
#' @return Named list of character vectors (empty lists excluded)
collect_input_lists <- function(list_count, input, ns) {
  inputLists <- list()

  for (i in 1:list_count) {
    list_content <- input[[paste0("list", i)]]
    if (!is.null(list_content) && list_content != "") {
      proteins <- trimws(unlist(strsplit(list_content, "\n")))
      proteins <- proteins[proteins != ""]
      if (length(proteins) > 0) {
        inputLists[[i]] <- proteins
      }
    }
  }

  inputLists[lengths(inputLists) > 0]
}


# =============================================================================
# Protein Extraction from Sample Data
# =============================================================================

#' Extract protein identifiers for a set of selected samples
#'
#' Locates abundance columns for the selected samples (optionally filtered by
#' abundance type), then returns the identifiers for rows that have at least
#' one non-NA, non-zero abundance value.
#'
#' @param data_modified Modified data frame
#' @param df_data_definition_post_mod Data-definition frame
#' @param selected_samples Character vector of selected sample names
#' @param reference_value Optional abundance type string for precise filtering
#' @param selected_identifier Optional protein-identifier column name
#' @return Character vector of protein identifiers (may be empty)
extract_sample_proteins <- function(data_modified, df_data_definition_post_mod,
                                    selected_samples, reference_value = NULL,
                                    selected_identifier = NULL) {
  if (is.null(data_modified) || is.null(df_data_definition_post_mod) ||
      length(selected_samples) == 0) {
    return(character())
  }

  data_def <- df_data_definition_post_mod
  data     <- data_modified

  identifier_col_index <- 1
  if (!is.null(selected_identifier) && selected_identifier != "") {
    identifier_indices <- which(data_def$Options == selected_identifier)
    if (length(identifier_indices) > 0) {
      identifier_col_index <- identifier_indices[1]
    }
  } else {
    identifier_indices <- which(grepl("Identifier", data_def$Content))
    if (length(identifier_indices) > 0) {
      identifier_col_index <- identifier_indices[1]
    }
  }

  if (!is.null(reference_value) && reference_value != "") {
    matching_indices <- which(
      data_def$Content == reference_value &
        data_def$Sample %in% selected_samples
    )
  } else {
    matching_indices <- which(data_def$Sample %in% selected_samples)
  }

  if (length(matching_indices) > 0) {
    abundance_data <- data[, matching_indices, drop = FALSE]

    valid_rows <- apply(abundance_data, 1, function(row) {
      any(!is.na(row) & row != 0)
    })

    if (any(valid_rows)) {
      proteins_sample <- data[valid_rows, identifier_col_index]
      proteins_sample <- proteins_sample[!is.na(proteins_sample) & proteins_sample != ""]
      return(proteins_sample)
    }
  }

  return(character())
}


# =============================================================================
# Excel Export Helper
# =============================================================================

#' Build an openxlsx workbook containing Venn intersection data
#'
#' Creates a single-sheet workbook with three sections stacked vertically:
#'   1. Input Lists               -- one row per list with name, count, proteins
#'   2. Intersections             -- all non-empty subset intersections
#'   3. Exclusive Intersections   -- proteins unique to each combination
#'
#' This function is pure (no Shiny reactivity). Input collection from the UI
#' happens in the download handler in venn_observer.R before calling here.
#'
#' @param list_names Character vector of list names (length == length(input_lists))
#' @param input_lists List of character vectors, one per protein list
#' @return An openxlsx workbook object ready for saveWorkbook()
build_venn_intersection_workbook <- function(list_names, input_lists,
                                             value_data = NULL,
                                             value_column_name = NULL) {

  excel_cell_char_limit <- 32767L

  truncate_for_excel_cell <- function(text_value) {
    if (is.null(text_value) || is.na(text_value) || !nzchar(text_value)) {
      return(text_value)
    }
    if (nchar(text_value, type = "chars") <= excel_cell_char_limit) {
      return(text_value)
    }
    substr(text_value, 1L, excel_cell_char_limit)
  }

  pack_items_into_excel_cells <- function(items, separator = ", ",
                                           column_prefix = "Proteins") {
    if (is.null(items)) items <- character()
    items <- as.character(items)
    items <- items[!is.na(items) & nzchar(items)]
    if (length(items) == 0) {
      return(stats::setNames(list(NA_character_), column_prefix))
    }

    cell_values <- character()
    current_cell <- ""

    for (item in items) {
      item <- truncate_for_excel_cell(item)
      candidate <- if (nzchar(current_cell)) {
        paste0(current_cell, separator, item)
      } else {
        item
      }

      if (nchar(candidate, type = "chars") <= excel_cell_char_limit) {
        current_cell <- candidate
      } else {
        if (nzchar(current_cell)) {
          cell_values <- c(cell_values, current_cell)
          current_cell <- item
        } else {
          cell_values <- c(cell_values, truncate_for_excel_cell(item))
          current_cell <- ""
        }
      }
    }

    if (nzchar(current_cell)) {
      cell_values <- c(cell_values, current_cell)
    }

    col_names <- if (length(cell_values) == 1L) {
      column_prefix
    } else {
      paste0(column_prefix, " ", seq_along(cell_values))
    }
    stats::setNames(as.list(cell_values), col_names)
  }

  bind_rows_fill <- function(rows) {
    if (length(rows) == 0) return(NULL)
    all_cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
    aligned <- lapply(rows, function(r) {
      missing <- setdiff(all_cols, names(r))
      if (length(missing) > 0) {
        for (m in missing) r[[m]] <- NA
      }
      as.data.frame(r[all_cols], stringsAsFactors = FALSE, check.names = FALSE)
    })
    do.call(rbind, aligned)
  }

  value_lookup <- NULL
  include_values <- !is.null(value_data) &&
    is.data.frame(value_data) &&
    all(c("Protein", "Value") %in% names(value_data)) &&
    !is.null(value_column_name) && nzchar(value_column_name)
  if (include_values) {
    value_lookup <- stats::setNames(value_data$Value, value_data$Protein)
  }

  build_rows <- function(labels, data_list) {
    rows <- lapply(labels, function(nm) {
      proteins <- unique(data_list[[nm]])
      proteins <- proteins[nzchar(proteins)]

      row <- list(
        Intersection = nm,
        Count = length(proteins)
      )
      row <- c(row, pack_items_into_excel_cells(proteins, separator = ", ", column_prefix = "Proteins"))

      if (include_values) {
        protein_values <- unname(value_lookup[proteins])
        protein_values <- protein_values[is.finite(protein_values)]
        row[[paste0("Mean ", value_column_name)]] <- if (length(protein_values) > 0) mean(protein_values) else NA_real_
        row[[paste0("Median ", value_column_name)]] <- if (length(protein_values) > 0) stats::median(protein_values) else NA_real_

        value_pairs <- if (length(proteins) > 0) {
          paste0(proteins, "=", round(unname(value_lookup[proteins]), 4))
        } else {
          character()
        }
        row <- c(row, pack_items_into_excel_cells(value_pairs, separator = "; ",
                                                   column_prefix = paste0(value_column_name, " by Protein")))
      }
      row
    })
    bind_rows_fill(rows)
  }

  list_rows <- lapply(seq_along(list_names), function(i) {
    proteins <- input_lists[[i]]
    row <- list(
      List = list_names[[i]],
      Count = length(proteins)
    )
    c(row, pack_items_into_excel_cells(proteins, separator = ", ", column_prefix = "Proteins"))
  })
  list_df <- bind_rows_fill(list_rows)
  if (is.null(list_df)) {
    list_df <- data.frame(List = character(), Count = integer(),
                          Proteins = character(), stringsAsFactors = FALSE)
  }

  intersection_data <- list()
  if (length(input_lists) > 0 && length(list_names) == length(input_lists)) {
    names(input_lists) <- list_names
    all_combinations <- unlist(
      lapply(seq_along(input_lists), function(m) {
        combn(names(input_lists), m, simplify = FALSE)
      }), recursive = FALSE
    )
    for (comb in all_combinations) {
      proteins_in_comb <- Reduce(intersect, input_lists[comb])
      proteins_in_comb <- proteins_in_comb[nzchar(proteins_in_comb)]
      intersection_data[[paste(comb, collapse = " & ")]] <- proteins_in_comb
    }
    intersection_data <- Filter(function(x) length(x) > 0, intersection_data)
  }

  intersection_df <- build_rows(names(intersection_data), intersection_data)
  if (is.null(intersection_df)) {
    intersection_df <- data.frame(Intersection = character(), Count = integer(),
                                  Proteins = character(), stringsAsFactors = FALSE)
  }

  exclusive_rows <- list()
  if (length(input_lists) > 0 && length(list_names) == length(input_lists)) {
    all_combinations <- unlist(
      lapply(seq_along(input_lists), function(m) {
        combn(names(input_lists), m, simplify = FALSE)
      }), recursive = FALSE
    )
    for (comb in all_combinations) {
      proteins_in_comb <- Reduce(intersect, input_lists[comb])
      proteins_in_comb <- proteins_in_comb[nzchar(proteins_in_comb)]
      other_lists <- setdiff(names(input_lists), comb)
      excluded_proteins <- if (length(other_lists) > 0) unique(unlist(input_lists[other_lists])) else character()
      proteins_exclusive <- setdiff(proteins_in_comb, excluded_proteins)
      proteins_exclusive <- proteins_exclusive[nzchar(proteins_exclusive)]

      if (length(proteins_exclusive) > 0) {
        without_label <- if (length(other_lists) > 0) {
          paste0(" without ", paste(other_lists, collapse = " & "))
        } else ""
        exclusive_label <- paste0(paste(comb, collapse = " & "), without_label)
        exclusive_rows[[length(exclusive_rows) + 1]] <- list(
          label = exclusive_label,
          proteins = proteins_exclusive
        )
      }
    }
  }

  exclusive_intersection_df <- if (length(exclusive_rows) > 0) {
    exclusive_list <- lapply(exclusive_rows, function(x) x$proteins)
    names(exclusive_list) <- vapply(exclusive_rows, function(x) x$label, character(1))
    build_rows(names(exclusive_list), exclusive_list)
  } else {
    NULL
  }
  if (is.null(exclusive_intersection_df)) {
    exclusive_intersection_df <- data.frame(Intersection = character(), Count = integer(),
                                            Proteins = character(), stringsAsFactors = FALSE)
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Venn_Export")

  current_row <- 1
  openxlsx::writeData(wb, "Venn_Export", "Input Lists", startRow = current_row, startCol = 1)
  current_row <- current_row + 1
  openxlsx::writeData(wb, "Venn_Export", list_df, startRow = current_row, startCol = 1)
  current_row <- current_row + max(nrow(list_df), 1) + 2

  openxlsx::writeData(wb, "Venn_Export", "Intersections", startRow = current_row, startCol = 1)
  current_row <- current_row + 1
  openxlsx::writeData(wb, "Venn_Export", intersection_df, startRow = current_row, startCol = 1)
  current_row <- current_row + max(nrow(intersection_df), 1) + 2

  openxlsx::writeData(wb, "Venn_Export", "Exclusive Intersections", startRow = current_row, startCol = 1)
  current_row <- current_row + 1
  openxlsx::writeData(wb, "Venn_Export", exclusive_intersection_df, startRow = current_row, startCol = 1)

  max_cols <- max(ncol(list_df), ncol(intersection_df), ncol(exclusive_intersection_df))
  openxlsx::setColWidths(wb, "Venn_Export", cols = seq_len(max_cols), widths = "auto")

  return(wb)
}



# =============================================================================
# NULL-Coalescing Operator
# =============================================================================

#' Return y when x is NULL, otherwise return x
#'
#' @param x Value to test
#' @param y Fallback value
`%||%` <- function(x, y) if (is.null(x)) y else x


# =============================================================================
# Identifier Validation Helper
# =============================================================================

#' Check whether user-provided protein identifiers exist in the selected column
#'
#' Looks up the identifier column in data_def by its Options label and checks
#' whether at least one value from user_proteins appears in that column of
#' data_mod. Returns TRUE when matches are found or when validation cannot be
#' performed (missing inputs), so that the calling code only blocks on a
#' confirmed mismatch.
#'
#' @param user_proteins Character vector of identifiers entered by the user
#' @param data_mod Modified data frame (rv$data_mod)
#' @param data_def Data-definition frame (rv$data_def)
#' @param identifier_column Selected identifier column name from UI
#' @return Logical: TRUE if at least one match found or validation not possible
validate_identifier_match <- function(user_proteins, data_mod, data_def,
                                      identifier_column) {
  if (is.null(data_mod) || is.null(data_def) || length(user_proteins) == 0) {
    return(TRUE)
  }
  identifier_column <- as.character(identifier_column %||% "")[1]
  if (!nzchar(identifier_column)) {
    return(TRUE)
  }

  id_idx <- integer(0)
  if ("Options" %in% names(data_def)) {
    id_idx <- which(as.character(data_def$Options) == identifier_column)
  }
  if (length(id_idx) == 0 && "Column" %in% names(data_def)) {
    id_idx <- which(as.character(data_def$Column) == identifier_column)
  }
  if (length(id_idx) == 0 && identifier_column %in% names(data_mod)) {
    col_values <- data_mod[[identifier_column]]
  } else if (length(id_idx) > 0) {
    col_name <- if ("Column" %in% names(data_def)) as.character(data_def$Column[id_idx[1]]) else NULL
    if (!is.null(col_name) && nzchar(col_name) && col_name %in% names(data_mod)) {
      col_values <- data_mod[[col_name]]
    } else {
      col_values <- data_mod[, id_idx[1]]
    }
  } else {
    return(TRUE)
  }

  normalize_id <- function(x) trimws(as.character(x))
  any(normalize_id(user_proteins) %in% normalize_id(col_values))
}


# =============================================================================
# Venn Diagram Creation
# =============================================================================

#' Create Venn diagram
#'
#' Builds a VennDiagram grob list. Supports 2-5 input lists.
#' Intersection data must be pre-computed and stored in state before calling
#' this function (handled by venn_data_cache in venn_observer.R).
#'
#' @param inputLists Named list of protein vectors
#' @param colors Character vector of hex colours, one per list
#' @param input Shiny input object (used for display options)
#' @param debug_log Logging function from the module server (optional)
#' @return VennDiagram grob list
create_venn_diagram <- function(inputLists, colors, input, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  if (length(inputLists) > 5) {
    stop("Venn diagrams support maximum 5 sets. Use UpSet plot for more sets.")
  }

  debug_log("Starting Venn diagram creation", 2)

  total_counts <- sapply(inputLists, length)

  # Safely read display parameters
  show_titles <- tryCatch(isTRUE(input$showListTitles_Venn),   error = function(e) FALSE)

  overlap_cex_val <- tryCatch({
    val <- as.numeric(input$overlapNumberSize_Venn %||% 1.5)
    if (!is.finite(val) || is.na(val)) 1.5 else val
  }, error = function(e) 1.5)

  list_title_size_val <- tryCatch({
    val <- as.numeric(input$listTitleSize_Venn %||% 1.5)
    if (!is.finite(val) || is.na(val)) 1.5 else val
  }, error = function(e) 1.5)

  cat_labels_val <- if (show_titles) names(inputLists) else rep(NA, length(inputLists))
  cat_cex_val    <- if (show_titles) list_title_size_val else 0

  list_title_distance_val <- tryCatch({
    val <- as.numeric(input$listTitleDistance_Venn %||% 0.05)
    if (!is.finite(val) || is.na(val)) 0.05 else val
  }, error = function(e) 0.05)

  font_family_val    <- tryCatch(input$font_family_Venn %||% "sans",    error = function(e) "sans")
  cat_font_val       <- tryCatch(input$catFont_Venn %||% "sans",        error = function(e) "sans")
  font_style_val     <- tryCatch(input$fontStyle_Venn %||% "plain",     error = function(e) "plain")
  cat_font_style_val <- tryCatch(input$cat_FontStyle_Venn %||% "plain", error = function(e) "plain")
  show_percentages   <- tryCatch(isTRUE(input$showPercentages_Venn),    error = function(e) FALSE)

  venn.plot <- venn.diagram(
    disable.logging = TRUE,
    x              = inputLists,
    filename       = NULL,
    cat.labels     = cat_labels_val,
    fill           = colors,
    alpha          = 0.5,
    cex            = overlap_cex_val,
    cat.cex        = cat_cex_val,
    cat.dist       = list_title_distance_val,
    fontfamily     = font_family_val,
    cat.fontfamily = cat_font_val,
    fontface       = font_style_val,
    cat.fontface   = cat_font_style_val,
    margin         = 0.1
  )

  if (show_percentages) {
    for (i in seq_along(venn.plot)) {
      if (!is.null(venn.plot[[i]]$label)) {
        label <- venn.plot[[i]]$label
        count <- suppressWarnings(as.numeric(label))
        if (!is.na(count) && count > 0) {
          percentage <- round((count / sum(total_counts)) * 100, 1)
          venn.plot[[i]]$label <- paste0(label, "\n(", percentage, "%)")
        }
      }
    }
  }

  return(venn.plot)
}


# =============================================================================
# UpSet Plot Variants
# =============================================================================

#' Create standard UpSet plot
#'
#' @param upset_data Data frame with binary membership matrix
#' @param inputLists Named list of protein vectors
#' @param input Shiny input object
#' @param num_intersections_export reactiveVal to store intersection count
#' @return ggplot/upset object

create_plain_upset_fallback <- function(upset_data, inputLists, input,
                                        num_intersections_export,
                                        debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  fallback_plot <- create_standard_upset(
    upset_data = upset_data,
    inputLists = inputLists,
    input = input,
    num_intersections_export = num_intersections_export,
    debug_log = debug_log
  )

  collect_classes <- function(x) {
    out <- class(x)
    if (is.list(x)) {
      for (i in seq_along(x)) out <- c(out, collect_classes(x[[i]]))
    }
    unique(out)
  }
  layer_classes <- collect_classes(fallback_plot)
  forbidden_patterns <- c("stat_summary", "geom_boxplot", "aes")
  forbidden_count <- sum(vapply(
    forbidden_patterns,
    function(pat) sum(grepl(pat, layer_classes, ignore.case = TRUE)),
    integer(1)
  ))
  fallback_layers_ok <- identical(forbidden_count, 0L)

  debug_log(
    paste0(
      "fallback_plain_upset_layers_ok=", if (isTRUE(fallback_layers_ok)) "TRUE" else "FALSE",
      " layer_class_count=", length(layer_classes),
      " forbidden_class_hits=", forbidden_count
    ),
    if (isTRUE(fallback_layers_ok)) 2 else 1
  )

  fallback_plot
}

create_standard_upset <- function(upset_data, inputLists, input,
                                  num_intersections_export,
                                  debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  num_intersections_export(0L)

  width_ratio       <- 0.3
  selected_theme    <- get_upset_theme(input$ThemeSelect_Upset)
  panel_style_theme <- get_upset_panel_theme(input$ThemeSelect_Upset)
  panel_grid_theme  <- if (identical(input$ThemeSelect_Upset, "Black and White")) {
    theme(panel.grid = element_blank())
  } else {
    theme()
  }

  set_cols <- names(inputLists)
  membership_counts <- rowSums(upset_data[, set_cols, drop = FALSE])
  valid_membership <- membership_counts > 0
  intersection_labels <- apply(
    upset_data[valid_membership, set_cols, drop = FALSE],
    1,
    function(x) paste0(as.integer(x), collapse = "")
  )
  max_count <- if (length(intersection_labels) > 0) {
    max(table(intersection_labels))
  } else {
    0L
  }
  expand_mult_top <- 0.08
  ylim_max <- if (max_count > 0) max_count * 1.08 else 1
  panel_spacing_y <- "unchanged"

  debug_log(
    paste0(
      "[UpSet intersection bars] max_count=", max_count,
      " ylim_max=", signif(ylim_max, 6),
      " expand_mult_top=", expand_mult_top,
      " panel_spacing_y=", panel_spacing_y
    ),
    2
  )

  intersection_size_annotation <- intersection_size(
    counts = TRUE,
    text   = list(size = input$label_text_size_Venn, size.unit = "pt")
  ) +
    scale_y_continuous(
      limits = c(0, ylim_max),
      expand = expansion(mult = c(0, expand_mult_top))
    ) +
    coord_cartesian(clip = "off")

  upset_plot <- suppressMessages(
    ComplexUpset::upset(
      upset_data,
      intersect   = names(inputLists),
      name        = "Groups",
      width_ratio = width_ratio,
      set_sizes   = upset_set_size(
        geom = geom_bar(
          aes(x = group, y = after_stat(count)), stat = "count", width = 0.7
        )
      ),
      base_annotations = list("Intersection size" = intersection_size_annotation),

      themes = list(
        "default" = panel_style_theme + panel_grid_theme,
        "intersections_matrix" = panel_style_theme + panel_grid_theme + theme(
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn),
          axis.text.x  = element_blank(),
          axis.title.y  = element_blank()
        ),
        "overall_sizes" = panel_style_theme + panel_grid_theme + theme(
          axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank()
        ),
        "Intersection size" = panel_style_theme + panel_grid_theme + theme(
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      )
    )
  )

  # (upset_plot + selected_theme + panel_grid_theme) &
  #   theme(
  #     axis.title = element_text(size = input$axis_title_size_Venn),
  #     axis.text  = element_text(size = input$axis_text_size_Venn)
  #   )
}


validate_upset_y_column <- function(plot_data, plot_variant, debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)
  y_column_name <- "Value"
  has_y_column <- is.data.frame(plot_data) && y_column_name %in% names(plot_data)
  y_values <- if (isTRUE(has_y_column)) suppressWarnings(as.numeric(plot_data[[y_column_name]])) else numeric(0)
  n_finite_y <- if (isTRUE(has_y_column)) sum(is.finite(y_values), na.rm = TRUE) else 0L
  debug_log(paste0("[Venn] plot build diagnostics: has_y_column=", has_y_column,
                   "; y_column_name=", y_column_name,
                   "; n_finite_y=", n_finite_y,
                   "; plot_variant=", plot_variant), 1)
  list(ok = isTRUE(has_y_column) && n_finite_y > 0L, y_column_name = y_column_name)
}

compute_boxplot_y_limits <- function(plot_data, y_column = "Value") {
  if (!is.data.frame(plot_data) || !y_column %in% names(plot_data)) {
    return(list(y_min = NA_real_, y_max = NA_real_))
  }
  y <- suppressWarnings(as.numeric(plot_data[[y_column]]))
  y <- y[is.finite(y)]
  if (length(y) == 0L) return(list(y_min = NA_real_, y_max = NA_real_))

  q <- stats::quantile(y, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- q[2] - q[1]
  if (!is.finite(iqr) || iqr <= 0) {
    y_min <- min(y, na.rm = TRUE)
    y_max <- max(y, na.rm = TRUE)
  } else {
    lower <- q[1] - 1.5 * iqr
    upper <- q[2] + 1.5 * iqr
    y_clamped <- y[y >= lower & y <= upper]
    if (length(y_clamped) == 0L) y_clamped <- y
    y_min <- min(y_clamped, na.rm = TRUE)
    y_max <- max(y_clamped, na.rm = TRUE)
  }

  if (!is.finite(y_min) || !is.finite(y_max)) return(list(y_min = NA_real_, y_max = NA_real_))
  span <- y_max - y_min
  if (!is.finite(span) || span <= 0) span <- max(abs(y_max), 1) * 0.1
  pad <- span * 0.08
  list(y_min = y_min - pad, y_max = y_max + pad)
}

compute_boxplot_upper_expand_mult <- function(y_min, y_max) {
  if (!is.finite(y_min) || !is.finite(y_max)) return(0.08)
  span <- y_max - y_min
  if (!is.finite(span) || span < 0) return(0.08)

  # Dynamic headroom in the requested 6-10% band:
  # tighter expansion for larger spans, more room for compact ranges.
  span_ratio <- span / max(abs(y_max), 1)
  span_ratio <- max(0, min(1, span_ratio))
  0.10 - (0.04 * span_ratio)
}

#' Create UpSet plot with mean-abundance boxplots
#'
#' @param upset_data Data frame with binary membership matrix
#' @param inputLists Named list of protein vectors
#' @param input Shiny input object
#' @param data_modified Reactive returning modified data frame (ignored when
#'   pre_prepared_data is supplied)
#' @param df_data_definition_post_mod Reactive returning data-definition frame
#'   (ignored when pre_prepared_data is supplied)
#' @param num_intersections_export reactiveVal to store intersection count
#' @param debug_log Logging function from the module server (optional)
#' @param pre_prepared_data Optional pre-computed data frame from
#'   prepare_abundance_data(). When supplied the data-loading step is skipped
#'   so that only the styling layer is re-evaluated on UI changes.
#' @return ggplot/upset object
create_upset_with_abundances <- function(upset_data, inputLists, input,
                                         data_modified = NULL,
                                         df_data_definition_post_mod = NULL,
                                         num_intersections_export, debug_log = NULL,
                                         pre_prepared_data = NULL,
                                         source_mode = "live",
                                         metadata_source = "live",
                                         show_dots_override = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)
  strict_cached_contract <- isTRUE(getOption("miraprot.venn.strict_cached_contract", FALSE))

  if (!is.null(pre_prepared_data)) {
    plot_data <- pre_prepared_data
    debug_log("Using pre-prepared abundance data", 2)
  } else {
    data_def            <- df_data_definition_post_mod()
    data                <- data_modified()
    identifier_selected <- input$GeneIdentifierColumn_Venn
    abundance_selected  <- input$ReferenceValues_Venn

    debug_log(paste("Identifier selected:", identifier_selected), 2)
    debug_log(paste("Abundance selected (from ReferenceValues):", abundance_selected), 2)

    if (is.null(abundance_selected) || abundance_selected == "") {
      stop("Please select a valid abundance type in 'Select abundance type for sample filtering'.")
    }

    if (isTRUE(strict_cached_contract) && identical(source_mode, "cached")) {
      notify_once <- local({
        fired <- FALSE
        function(msg) {
          if (!fired) {
            showNotification(msg, type = "warning")
            fired <<- TRUE
          }
        }
      })
      if (!"Options" %in% names(data_def) ||
          !identifier_selected %in% as.character(data_def$Options)) {
        notify_once("Cached restore id column missing; rendering plain UpSet.")
        return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
      }
      abundance_idx_cached <- which(as.character(data_def$Content) == as.character(abundance_selected))
      if (length(abundance_idx_cached) == 0L) {
        notify_once("Cached restore abundance type not found; rendering plain UpSet.")
        return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
      }
      selected_samples_cached <- as.character(input$data_abundance_Mean_Venn %||% character())
      selected_samples_cached <- selected_samples_cached[nzchar(selected_samples_cached)]
      sample_labels_cached <- as.character(data_def$Sample[abundance_idx_cached])
      mapped_cols <- which(!is.na(sample_labels_cached) & sample_labels_cached %in% selected_samples_cached)
      if (length(mapped_cols) == 0L) {
        notify_once("Cached restore selected samples do not map to abundance columns; rendering plain UpSet.")
        return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
      }
    }

    plot_data <- prepare_abundance_data(
      data, data_def, identifier_selected, abundance_selected,
      input$data_abundance_Mean_Venn, upset_data, inputLists, debug_log,
      strict_sample_mapping = isTRUE(strict_cached_contract) && identical(source_mode, "cached")
    )
  }

  set_cols  <- names(inputLists)
  plot_data <- prepare_upset_plot_data(plot_data, set_cols)

  ensure_intersection_labels <- function(df, sets, fallback_upset_data = NULL, fallback_inputLists = NULL) {
    if (!is.data.frame(df) || length(sets) == 0L) return(df)
    if (!all(sets %in% names(df))) {
      if (!is.null(fallback_upset_data) && !is.null(fallback_inputLists) &&
          !is.null(data_modified) && !is.null(df_data_definition_post_mod)) {
        debug_log("Intersection labels missing in live mode; regenerating from upset_data + inputLists", 1)
        regenerated <- prepare_abundance_data(
          data = data_modified(),
          data_def = df_data_definition_post_mod(),
          identifier_selected = input$GeneIdentifierColumn_Venn,
          abundance_selected = input$ReferenceValues_Venn,
          samples_mean = input$data_abundance_Mean_Venn,
          upset_data = fallback_upset_data,
          inputLists = fallback_inputLists,
          debug_log = debug_log,
          strict_sample_mapping = FALSE
        )
        df <- prepare_upset_plot_data(regenerated, sets)
      }
      return(df)
    }

    if (!("intersection_label" %in% names(df)) || all(is.na(df$intersection_label)) || all(df$intersection_label == "")) {
      set_matrix <- as.data.frame(lapply(df[, sets, drop = FALSE], function(col) as.integer(as.logical(col))))
      membership_counts <- rowSums(set_matrix, na.rm = TRUE)
      valid_rows <- membership_counts > 0
      labels <- rep(NA_character_, nrow(df))
      if (any(valid_rows)) {
        labels[valid_rows] <- apply(set_matrix[valid_rows, , drop = FALSE], 1, function(x) paste0(x, collapse = ""))
      }
      df$intersection_label <- labels
    }

    df
  }

  if (!("intersection_label" %in% names(plot_data)) && identical(source_mode, "live")) {
    plot_data <- ensure_intersection_labels(
      plot_data,
      set_cols,
      fallback_upset_data = upset_data,
      fallback_inputLists = inputLists
    )
  } else {
    plot_data <- ensure_intersection_labels(plot_data, set_cols)
  }

  valid_intersection_labels <- unique(stats::na.omit(plot_data$intersection_label))
  num_intersections <- length(valid_intersection_labels)

  non_empty_lists <- length(inputLists) > 0 && any(vapply(inputLists, length, integer(1)) > 0)
  guard_cached_restore <- isTRUE(strict_cached_contract) && identical(source_mode, "cached") && identical(metadata_source, "restore")
  if (isTRUE(guard_cached_restore) && identical(num_intersections, 0L) && isTRUE(non_empty_lists)) {
    warn_msg <- paste0(
      "Data-contract error: computed 0 intersections from post-filter plotting table ",
      "while input lists are non-empty during cached restore. Rendering plain UpSet fallback."
    )
    debug_log(warn_msg, 1)
    showNotification(warn_msg, type = "warning")
    return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
  }

  debug_log(paste("Detected", num_intersections,
                  "intersections for abundance plot width calculation"), 2)
  num_intersections_export(num_intersections)

  width_ratio       <- min(0.3, (300 / (1 + ((num_intersections - 5) * 0.1))) / 1000)
  selected_theme    <- get_upset_theme(input$ThemeSelect_Upset)
  panel_style_theme <- get_upset_panel_theme(input$ThemeSelect_Upset)
  panel_grid_theme  <- if (identical(input$ThemeSelect_Upset, "Black and White")) {
    theme(panel.grid = element_blank())
  } else {
    theme()
  }

  y_check <- validate_upset_y_column(plot_data, "abundance_boxplot", debug_log)
  y_limits <- compute_boxplot_y_limits(plot_data, y_check$y_column_name)
  y_expand_top <- compute_boxplot_upper_expand_mult(y_limits$y_min, y_limits$y_max)
  panel_spacing_y <- "unchanged"
  debug_log(paste0("[UpSet boxplot] source_mode=", source_mode,
                   " metadata_source=", metadata_source,
                   " y_min=", signif(y_limits$y_min, 6),
                   " y_max=", signif(y_limits$y_max, 6),
                   " y_expand_top=", signif(y_expand_top, 4),
                   " panel_spacing_y=", panel_spacing_y), 1)
  boxplot_show_dots <- if (!is.null(show_dots_override)) isTRUE(show_dots_override) else isTRUE(input$showDotsInBoxplot_Venn %||% TRUE)
  boxplot_annotation <- if (isTRUE(y_check$ok)) {
    p_boxplot <- ggplot(mapping = aes(x = intersection)) +
      geom_boxplot(aes(y = .data[[y_check$y_column_name]]), outlier.shape = NA)
    if (boxplot_show_dots) {
      p_boxplot <- p_boxplot + geom_jitter(aes(y = .data[[y_check$y_column_name]]), width = 0.15, alpha = 0.5)
    }
    list(
      "Boxplot" = p_boxplot +
        scale_y_continuous(
          limits = c(y_limits$y_min, y_limits$y_max),
          expand = expansion(mult = c(0.02, y_expand_top))
        ) +
        coord_cartesian(clip = "off") +
        labs(y = expression(log[2](Abundances)))
    )
  } else {
    showNotification("Y-values unavailable for abundance boxplot. Rendering plain UpSet.", type = "warning")
    return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
  }

  upset_plot <- suppressMessages(ComplexUpset::upset(
    plot_data,
    intersect   = names(inputLists),
    name        = "Conditions",
    width_ratio = width_ratio,
    base_annotations = boxplot_annotation,
    set_sizes = upset_set_size(
      geom = geom_bar(
        aes(x = group, y = after_stat(count)), stat = "count", width = 0.7
      )
    ),
    themes = c(
      list(
        "default" = panel_style_theme + panel_grid_theme + theme(
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn)
        ),
        "intersections_matrix" = panel_style_theme + panel_grid_theme + theme(
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn),
          axis.text.x  = element_blank(),
          axis.title.y  = element_blank()
        ),
        "overall_sizes" = panel_style_theme + panel_grid_theme + theme(
          axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.title = element_text(size = input$axis_title_size_Venn),
          axis.text  = element_text(size = input$axis_text_size_Venn),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      ),
      if (isTRUE(y_check$ok)) list(
        "Boxplot" = panel_style_theme + panel_grid_theme + theme(
          axis.title.y = element_text(size = input$axis_title_size_Venn),
          axis.text.y  = element_text(size = input$axis_text_size_Venn),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      ) else list()
    )
  ))

  # (upset_plot + selected_theme + panel_grid_theme) &
  #   theme(
  #     axis.title = element_text(size = input$axis_title_size_Venn),
  #     axis.text  = element_text(size = input$axis_text_size_Venn)
  #   )
}

#' Create UpSet plot with log2 abundance-ratio boxplots
#'
#' @param upset_data Data frame with binary membership matrix
#' @param inputLists Named list of protein vectors
#' @param input Shiny input object
#' @param data_modified Reactive returning modified data frame (ignored when
#'   pre_prepared_data is supplied)
#' @param df_data_definition_post_mod Reactive returning data-definition frame
#'   (ignored when pre_prepared_data is supplied)
#' @param num_intersections_export reactiveVal to store intersection count
#' @param debug_log Logging function from the module server (optional)
#' @param pre_prepared_data Optional pre-computed data frame from
#'   prepare_ratio_data(). When supplied the data-loading step is skipped
#'   so that only the styling layer is re-evaluated on UI changes.
#' @return ggplot/upset object
create_upset_with_ratios <- function(upset_data, inputLists, input,
                                     data_modified = NULL,
                                     df_data_definition_post_mod = NULL,
                                     num_intersections_export, debug_log = NULL,
                                     pre_prepared_data = NULL,
                                     source_mode = "live",
                                     metadata_source = "live",
                                     show_dots_override = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)
  strict_cached_contract <- isTRUE(getOption("miraprot.venn.strict_cached_contract", FALSE))

  if (!is.null(pre_prepared_data)) {
    plot_data <- pre_prepared_data
    debug_log("Using pre-prepared ratio data", 2)
  } else {
    data_def            <- df_data_definition_post_mod()
    data                <- data_modified()
    identifier_selected <- input$GeneIdentifierColumn_Venn
    abundance_selected  <- input$ReferenceValues_Venn

    debug_log(paste("Identifier selected:", identifier_selected), 2)
    debug_log(paste("Abundance selected (from ReferenceValues):", abundance_selected), 2)
    debug_log(paste("Numerator samples:", paste(input$data_abundance_ratio_num_Venn, collapse = ", ")), 2)
    debug_log(paste("Denominator samples:", paste(input$data_abundance_ratio_denom_Venn, collapse = ", ")), 2)

    plot_data <- prepare_ratio_data(
      data, data_def, identifier_selected, abundance_selected,
      input$data_abundance_ratio_num_Venn, input$data_abundance_ratio_denom_Venn,
      upset_data, inputLists, debug_log
    )
  }

  set_cols  <- names(inputLists)
  plot_data <- prepare_upset_plot_data(plot_data, set_cols)

  ensure_intersection_labels <- function(df, sets, fallback_upset_data = NULL, fallback_inputLists = NULL) {
    if (!is.data.frame(df) || length(sets) == 0L) return(df)
    if (!all(sets %in% names(df))) {
      if (!is.null(fallback_upset_data) && !is.null(fallback_inputLists) &&
          !is.null(data_modified) && !is.null(df_data_definition_post_mod)) {
        debug_log("Intersection labels missing in live mode; regenerating from upset_data + inputLists", 1)
        regenerated <- prepare_abundance_data(
          data = data_modified(),
          data_def = df_data_definition_post_mod(),
          identifier_selected = input$GeneIdentifierColumn_Venn,
          abundance_selected = input$ReferenceValues_Venn,
          samples_mean = input$data_abundance_Mean_Venn,
          upset_data = fallback_upset_data,
          inputLists = fallback_inputLists,
          debug_log = debug_log,
          strict_sample_mapping = FALSE
        )
        df <- prepare_upset_plot_data(regenerated, sets)
      }
      return(df)
    }

    if (!("intersection_label" %in% names(df)) || all(is.na(df$intersection_label)) || all(df$intersection_label == "")) {
      set_matrix <- as.data.frame(lapply(df[, sets, drop = FALSE], function(col) as.integer(as.logical(col))))
      membership_counts <- rowSums(set_matrix, na.rm = TRUE)
      valid_rows <- membership_counts > 0
      labels <- rep(NA_character_, nrow(df))
      if (any(valid_rows)) {
        labels[valid_rows] <- apply(set_matrix[valid_rows, , drop = FALSE], 1, function(x) paste0(x, collapse = ""))
      }
      df$intersection_label <- labels
    }

    df
  }

  if (!("intersection_label" %in% names(plot_data)) && identical(source_mode, "live")) {
    plot_data <- ensure_intersection_labels(
      plot_data,
      set_cols,
      fallback_upset_data = upset_data,
      fallback_inputLists = inputLists
    )
  } else {
    plot_data <- ensure_intersection_labels(plot_data, set_cols)
  }

  valid_intersection_labels <- unique(stats::na.omit(plot_data$intersection_label))
  num_intersections <- length(valid_intersection_labels)


  non_empty_lists <- length(inputLists) > 0 && any(vapply(inputLists, length, integer(1)) > 0)
  guard_cached_restore <- isTRUE(strict_cached_contract) && identical(source_mode, "cached") && identical(metadata_source, "restore")
  if (isTRUE(guard_cached_restore) && identical(num_intersections, 0L) && isTRUE(non_empty_lists)) {
    warn_msg <- paste0(
      "Data-contract error: computed 0 intersections from post-filter plotting table ",
      "while input lists are non-empty during cached restore. Rendering plain UpSet fallback."
    )
    debug_log(warn_msg, 1)
    showNotification(warn_msg, type = "warning")
    return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
  }

  debug_log(paste("Detected", num_intersections,
                  "intersections for ratio plot width calculation"), 2)
  num_intersections_export(num_intersections)

  width_ratio       <- min(0.3, (300 / (1 + ((num_intersections - 5) * 0.1))) / 1000)
  selected_theme    <- get_upset_theme(input$ThemeSelect_Upset)
  panel_style_theme <- get_upset_panel_theme(input$ThemeSelect_Upset)
  panel_grid_theme  <- if (identical(input$ThemeSelect_Upset, "Black and White")) {
    theme(panel.grid = element_blank())
  } else {
    theme()
  }

  upset_plot <- suppressMessages(
    ComplexUpset::upset(
      plot_data,
      intersect   = names(inputLists),
      name        = "Conditions",
      width_ratio = width_ratio,
      base_annotations = {
        y_check <- validate_upset_y_column(plot_data, "ratio_boxplot", debug_log)
        y_limits <- compute_boxplot_y_limits(plot_data, y_check$y_column_name)
        y_expand_top <- compute_boxplot_upper_expand_mult(y_limits$y_min, y_limits$y_max)
        panel_spacing_y <- "unchanged"
        debug_log(paste0("[UpSet boxplot] source_mode=", source_mode,
                         " metadata_source=", metadata_source,
                         " y_min=", signif(y_limits$y_min, 6),
                         " y_max=", signif(y_limits$y_max, 6),
                         " y_expand_top=", signif(y_expand_top, 4),
                         " panel_spacing_y=", panel_spacing_y), 1)
        if (isTRUE(y_check$ok)) {
          boxplot_show_dots <- if (!is.null(show_dots_override)) isTRUE(show_dots_override) else isTRUE(input$showDotsInBoxplot_Venn %||% TRUE)
          p_boxplot <- ggplot(mapping = aes(x = intersection)) +
            geom_boxplot(aes(y = .data[[y_check$y_column_name]]), outlier.shape = NA)
          if (boxplot_show_dots) {
            p_boxplot <- p_boxplot + geom_jitter(aes(y = .data[[y_check$y_column_name]]), width = 0.15, alpha = 0.5)
          }
          list(
            "Boxplot" =
              p_boxplot +
              scale_y_continuous(
                limits = c(y_limits$y_min, y_limits$y_max),
                expand = expansion(mult = c(0.02, y_expand_top))
              ) +
              coord_cartesian(clip = "off") +
              labs(y = expression(log[2](Abundance~Ratio)))
          )
        } else {
          showNotification("Y-values unavailable for ratio boxplot. Rendering plain UpSet.", type = "warning")
          return(create_plain_upset_fallback(upset_data, inputLists, input, num_intersections_export, debug_log))
        }
      },
      set_sizes = upset_set_size(
        geom = geom_bar(
          aes(x = group, y = after_stat(count)), stat = "count", width = 0.7
        )
      ),
      themes = c(
        list(
          "default" = panel_style_theme + panel_grid_theme,
          "intersections_matrix" = panel_style_theme + panel_grid_theme + theme(
            axis.title = element_text(size = input$axis_title_size_Venn),
            axis.text  = element_text(size = input$axis_text_size_Venn),
            axis.text.x  = element_blank(),
            axis.title.y  = element_blank()
          ),
          "overall_sizes" = panel_style_theme + panel_grid_theme + theme(
            axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title = element_text(size = input$axis_title_size_Venn),
            axis.text  = element_text(size = input$axis_text_size_Venn),
            axis.text.y  = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        ),
        if (exists("y_check") && isTRUE(y_check$ok)) list(
          "Boxplot" = panel_style_theme + panel_grid_theme + theme(
            axis.title.y = element_text(size = input$axis_title_size_Venn),
            axis.text.y  = element_text(size = input$axis_text_size_Venn),
            axis.text.x  = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank()
          )
        ) else list()
      )
    )
  )

  # (upset_plot + selected_theme + panel_grid_theme) &
  #   theme(
  #     axis.title = element_text(size = input$axis_title_size_Venn),
  #     axis.text  = element_text(size = input$axis_text_size_Venn)
  #   )
}


# =============================================================================
# UpSet Data Helpers
# =============================================================================

#' Map UI theme-name string to a ggplot2 panel theme object
#'
#' @param theme_name Character string matching a UI selectInput choice
#' @return ggplot2 theme object
get_upset_panel_theme <- function(theme_name) {
  switch(
    theme_name,
    "Gray" = theme(
      panel.background = element_rect(fill = "#EBEBEB", colour = NA),
      panel.grid.major = element_line(colour = "white", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "white", linewidth = 0.15),
      axis.text  = element_text(colour = "#4D4D4D"),
      axis.title = element_text(colour = "#4D4D4D")
    ),
    "Black and White" = theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey92", linewidth = 0.15),
      axis.text  = element_text(colour = "black"),
      axis.title = element_text(colour = "black")
    ),
    "Linedraw" = theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      panel.grid.major = element_line(colour = "black", linewidth = 0.25),
      panel.grid.minor = element_line(colour = "grey70", linewidth = 0.15)
    ),
    "Light" = theme(
      panel.background = element_rect(fill = "white", colour = "grey85"),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey95", linewidth = 0.15)
    ),
    "Dark" = theme(
      panel.background = element_rect(fill = "grey20", colour = NA),
      panel.grid.major = element_line(colour = "grey35", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey30", linewidth = 0.15),
      axis.text  = element_text(colour = "grey90"),
      axis.title = element_text(colour = "grey95")
    ),
    "Minimal" = theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    ),
    "Classic" = theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid       = element_blank(),
      axis.line        = element_line(colour = "black")
    ),
    "Void" = theme(
      panel.background = element_blank(),
      panel.grid       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank()
    ),
    theme()
  )
}

# Backward-compatibility no-op helper retained for stale call sites
prepare_upset_plot_data <- function(data, set_cols = NULL) {
  data
}


# =============================================================================
# Data-Preparation Helpers
# =============================================================================

#' Prepare long-format abundance data for UpSet abundance plots
#'
#' Joins per-protein mean log2 abundances to the binary membership matrix.
#'
#' @param data Modified data frame
#' @param data_def Data-definition frame
#' @param identifier_selected Selected protein-identifier column name
#' @param abundance_selected Selected abundance type (Content value)
#' @param samples_mean Optional character vector of sample names to average
#' @param upset_data Binary membership data frame (with Protein column)
#' @param inputLists Named list of protein vectors
#' @param debug_log Logging function from the module server (optional)
#' @return Data frame suitable for ComplexUpset with a Value column
prepare_abundance_data <- function(data, data_def, identifier_selected,
                                   abundance_selected, samples_mean,
                                   upset_data, inputLists, debug_log = NULL,
                                   strict_sample_mapping = FALSE) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  available_content_types <- trimws(as.character(data_def$Content))
  available_content_types <- available_content_types[
    !is.na(available_content_types) &
      available_content_types != "" &
      available_content_types != "NA"
  ]
  available_content_types <- unique(available_content_types)

  debug_log(paste("Preparing abundance data with type:", abundance_selected), 2)
  debug_log(paste("Available Content types:",
                  paste(available_content_types, collapse = ", ")), 2)

  identifier_col_index <- which(data_def$Options == identifier_selected)
  if (length(identifier_col_index) == 0) {
    stop("Selected identifier column not found in data definition")
  }

  abundance_indices <- which(data_def$Content == abundance_selected)
  debug_log(paste("Found", length(abundance_indices), "abundance indices"), 2)

  if (length(abundance_indices) == 0) {
    stop(paste(
      "No abundance data found for selected type:", abundance_selected,
      ". Available types:", paste(available_content_types, collapse = ", ")
    ))
  }

  abundance_data  <- data[, abundance_indices, drop = FALSE]
  identifier_data <- data[, identifier_col_index]

  normalize_sample_label <- function(x) {
    x <- trimws(as.character(x))
    x <- tolower(x)
    x
  }
  normalize_alias_label <- function(x) {
    x <- normalize_sample_label(x)
    gsub("[_\\-]+", " ", x)
  }

  if (!is.null(samples_mean) && length(samples_mean) > 0) {
    debug_log(paste("Filtering for specific samples:",
                    paste(samples_mean, collapse = ", ")), 2)

    abundance_labels <- data_def$Sample[abundance_indices]
    canonical_tbl <- data.frame(
      label = abundance_labels,
      col_idx = seq_along(abundance_indices),
      stringsAsFactors = FALSE
    )
    canonical_tbl <- canonical_tbl[!is.na(canonical_tbl$label) & canonical_tbl$label != "", , drop = FALSE]

    direct_lookup <- setNames(canonical_tbl$col_idx, normalize_sample_label(canonical_tbl$label))
    selected_norm <- normalize_sample_label(samples_mean)
    direct_matches <- unname(direct_lookup[selected_norm])
    direct_matches <- direct_matches[!is.na(direct_matches)]

    sample_indices_in_abundance <- unique(as.integer(direct_matches))

    if (length(sample_indices_in_abundance) == 0) {
      alias_lookup <- setNames(canonical_tbl$col_idx, normalize_alias_label(canonical_tbl$label))
      selected_alias <- normalize_alias_label(samples_mean)
      alias_matches <- unname(alias_lookup[selected_alias])
      alias_matches <- alias_matches[!is.na(alias_matches)]
      sample_indices_in_abundance <- unique(as.integer(alias_matches))
      debug_log(paste("Direct sample matches: 0; alias matches:",
                      length(sample_indices_in_abundance)), 2)
    } else {
      debug_log(paste("Direct sample matches:",
                      length(sample_indices_in_abundance)), 2)
    }

    if (length(sample_indices_in_abundance) > 0) {
      abundance_data <- abundance_data[, sample_indices_in_abundance, drop = FALSE]
    } else {
      debug_log("cached_sample_resolution_failed", 1)
      if (isTRUE(strict_sample_mapping)) {
        stop("No selected samples matched abundance columns for cached restore")
      }
      debug_log("No cached sample-label matches; forcing non-boxplot fallback (plain UpSet)", 1)
      abundance_data <- abundance_data[, 0, drop = FALSE]
    }
  } else if (isTRUE(strict_sample_mapping)) {
    stop("Cached restore requires selected samples for abundance boxplot")
  }

  mean_abundance <- log2(rowMeans(abundance_data, na.rm = TRUE))
  mean_abundance[is.infinite(mean_abundance)] <- NA

  plot_data <- merge(
    upset_data,
    data.frame(Protein = identifier_data, Value = mean_abundance),
    by = "Protein"
  )

  debug_log(paste("Final plot_data has", nrow(plot_data), "rows"), 2)
  return(plot_data)
}

#' Prepare long-format log2-ratio data for UpSet ratio plots
#'
#' Computes log2(numerator mean / denominator mean) per protein and joins to
#' the binary membership matrix.
#'
#' @param data Modified data frame
#' @param data_def Data-definition frame
#' @param identifier_selected Selected protein-identifier column name
#' @param abundance_selected Selected abundance type (Content value)
#' @param numerator_samples Character vector of numerator sample names
#' @param denominator_samples Character vector of denominator sample names
#' @param upset_data Binary membership data frame (with Protein column)
#' @param inputLists Named list of protein vectors
#' @param debug_log Logging function from the module server (optional)
#' @return Data frame suitable for ComplexUpset with a Value column
prepare_ratio_data <- function(data, data_def, identifier_selected,
                                abundance_selected, numerator_samples,
                                denominator_samples, upset_data, inputLists,
                                debug_log = NULL) {
  if (is.null(debug_log)) debug_log <- function(msg, level = 1) invisible(NULL)

  debug_log("Preparing ratio data", 2)
  debug_log(paste("Numerator samples:", paste(numerator_samples, collapse = ", ")), 2)
  debug_log(paste("Denominator samples:", paste(denominator_samples, collapse = ", ")), 2)
  debug_log(paste("Abundance selected:", abundance_selected), 2)

  if (is.null(numerator_samples) || is.null(denominator_samples) ||
      length(numerator_samples) == 0 || length(denominator_samples) == 0) {
    stop("Both numerator and denominator samples must be selected for ratio calculation")
  }

  identifier_col_index <- which(data_def$Options == identifier_selected)
  if (length(identifier_col_index) == 0) {
    stop("Selected identifier column not found in data definition")
  }

  abundance_indices <- which(data_def$Content == abundance_selected)
  debug_log(paste("Found", length(abundance_indices),
                  "abundance indices for type:", abundance_selected), 2)

  if (length(abundance_indices) == 0) {
    stop(paste(
      "No abundance data found for selected type:", abundance_selected,
      ". Available types:", paste(unique(data_def$Content), collapse = ", ")
    ))
  }

  num_indices   <- which(data_def$Sample[abundance_indices] %in% numerator_samples)
  denom_indices <- which(data_def$Sample[abundance_indices] %in% denominator_samples)

  debug_log(paste("Found", length(num_indices), "numerator indices and",
                  length(denom_indices), "denominator indices"), 2)

  if (length(num_indices) == 0 || length(denom_indices) == 0) {
    available_samples <- unique(data_def$Sample[abundance_indices])
    available_samples <- available_samples[!is.na(available_samples) & available_samples != ""]
    debug_log(paste("Available samples for abundance type:",
                    paste(available_samples, collapse = ", ")), 1)

    # Restore/runtime fallback: if the selected abundance type does not expose
    # sample-level columns (e.g. legacy snapshots with "Row Index"), resolve
    # ratio columns directly by sample names across the full definition table.
    fallback_abundance_indices <- which(!is.na(data_def$Sample) & data_def$Sample != "" &
                                          data_def$Sample %in% c(numerator_samples, denominator_samples))
    if (length(fallback_abundance_indices) > 0) {
      fallback_samples <- data_def$Sample[fallback_abundance_indices]
      fallback_num_indices <- which(fallback_samples %in% numerator_samples)
      fallback_denom_indices <- which(fallback_samples %in% denominator_samples)
      if (length(fallback_num_indices) > 0 && length(fallback_denom_indices) > 0) {
        debug_log(paste0("Ratio fallback activated: abundance type '", abundance_selected,
                         "' has no matching sample columns; using sample-name lookup across data_def"), 1)
        abundance_indices <- fallback_abundance_indices
        num_indices <- fallback_num_indices
        denom_indices <- fallback_denom_indices
      }
    }

    if (length(num_indices) == 0 || length(denom_indices) == 0) {
      stop(paste(
        "Could not find matching samples for ratio calculation.",
        "Numerator found:", length(num_indices), "samples.",
        "Denominator found:", length(denom_indices), "samples.",
        "Available samples for", abundance_selected, ":",
        paste(available_samples, collapse = ", ")
      ))
    }
  }

  abundance_data  <- data[, abundance_indices, drop = FALSE]
  identifier_data <- data[, identifier_col_index]
  abundance_transformations <- data_def$Transformation[abundance_indices]

  # Ratio calculations must operate on original-scale abundances when source
  # values are transformed. Apply global retransformation only for columns that
  # explicitly declare a known transformation type.
  if (!is.null(abundance_transformations) && length(abundance_transformations) == ncol(abundance_data)) {
    retransform_idx <- which(abundance_transformations %in% c("log2", "log10", "-log10"))
    if (length(retransform_idx) > 0) {
      abundance_data <- retransform_data_global(
        abundance_data,
        index = retransform_idx,
        transformation_df = abundance_transformations[retransform_idx]
      )
      debug_log(paste("Retransformed", length(retransform_idx),
                      "abundance columns before ratio calculation"), 2)
    }
  }

  num_mean   <- rowMeans(abundance_data[, num_indices,   drop = FALSE], na.rm = TRUE)
  denom_mean <- rowMeans(abundance_data[, denom_indices, drop = FALSE], na.rm = TRUE)

  ratio_values <- log2(num_mean / denom_mean)
  ratio_values[is.infinite(ratio_values)] <- NA

  plot_data <- merge(
    upset_data,
    data.frame(Protein = identifier_data, Value = ratio_values),
    by = "Protein"
  )

  # Remove rows with non-finite ratio values (NA, NaN, Inf, -Inf).
  # rowMeans(na.rm = TRUE) returns NaN for all-NA rows, and log2(NaN/x) = NaN,
  # which is not caught by is.infinite().  Filtering here prevents ggplot from
  # emitting repeated stat_boxplot / geom_point warnings on every render.
  plot_data <- plot_data[is.finite(plot_data$Value), ]

  debug_log(paste("Final ratio plot_data has", nrow(plot_data), "rows"), 2)
  return(plot_data)
}


# =============================================================================
# Theme and File-Save Helpers
# =============================================================================

#' Map UI theme-name string to a ggplot2 theme object
#'
#' @param theme_name Character string matching a UI selectInput choice
#' @return ggplot2 theme object
get_upset_theme <- function(theme_name) {
  switch(
    theme_name,
    "Gray"            = theme_gray(),
    "Black and White" = theme_bw(),
    "Linedraw"        = theme_linedraw(),
    "Light"           = theme_light(),
    "Dark"            = theme_dark(),
    "Minimal"         = theme_minimal(),
    "Classic"         = theme_classic(),
    "Void"            = theme_void(),
    theme_minimal()
  )
}

#' Save a plot object to a file in the requested format
#'
#' Opens the appropriate graphics device, renders the plot, and closes the
#' device. Supports raster (png, jpeg, tiff) and vector (svg, pdf) formats.
#'
#' @param file Destination file path (provided by downloadHandler)
#' @param plot_obj Plot object (ggplot, VennDiagram grob list, or UpSet plot)
#' @param format Character string: "png", "jpeg", "tiff", "svg", or "pdf"
#' @param width Width in inches
#' @param height Height in inches
#' @param ppi Resolution in pixels per inch (used for raster formats only)
save_plot_file <- function(file, plot_obj, format, width, height, ppi,
                           cached_mode = FALSE) {
  effective_height <- height
  if (isTRUE(cached_mode) && inherits(plot_obj, "ggplot")) {
    effective_height <- max(height, height + 1.2)
  }
  switch(format,
    "png"  = png( file, width = width * ppi, height = effective_height * ppi, res = ppi),
    "jpeg" = jpeg(file, width = width * ppi, height = effective_height * ppi, res = ppi),
    "tiff" = tiff(file, width = width * ppi, height = effective_height * ppi, res = ppi),
    "svg"  = svg( file, width = width, height = effective_height),
    "pdf"  = pdf( file, width = width, height = effective_height)
  )
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  if (inherits(plot_obj, "ggplot")) {
    print(plot_obj)
  } else {
    if (format %in% c("png", "jpeg", "tiff", "svg", "pdf")) {
      grid.newpage()
      if (is.list(plot_obj) && length(plot_obj) > 0) {
        grid.draw(plot_obj)
      } else {
        print(plot_obj)
      }
    }
  }
}


# =============================================================================
# Grid Integration Helper
# =============================================================================

#' Convert a Venn diagram or UpSet plot to a ggplot for Grid tab
#'
#' UpSet plots that are already ggplot objects are returned as-is. Venn
#' diagrams (grid grob lists) are rasterised to a temporary PNG and wrapped
#' in a ggplot annotation_raster. A fallback placeholder is returned if
#' conversion fails.
#'
#' @param plot_obj Plot object to convert
#' @param plot_type Character string: "Venn" or an UpSet variant name
#' @param debug_log Logging function from the module server
#' @return ggplot object
convert_venn_to_ggplot <- function(plot_obj, plot_type, debug_log, cached_mode = FALSE) {
  debug_log(paste("Converting", plot_type, "plot to ggplot for grid compatibility"), 1)

  if (plot_type != "Venn" && inherits(plot_obj, "ggplot")) {
    debug_log(paste("Using existing ggplot for", plot_type, "plot"), 2)
    return(plot_obj)
  }

  tryCatch({
    temp_file <- tempfile(fileext = ".png")

    export_height <- if (isTRUE(cached_mode) && plot_type != "Venn") 1100 else 900
    png(temp_file, width = 1200, height = export_height, res = 150, type = "cairo")
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)

    if (plot_type == "Venn") {
      if (is.list(plot_obj) && length(plot_obj) > 0) {
        grid::grid.newpage()
        grid::grid.draw(plot_obj)
      } else {
        print(plot_obj)
      }
    } else {
      print(plot_obj)
    }

    dev.off()
    on.exit(NULL)

    if (requireNamespace("png", quietly = TRUE)) {
      img <- png::readPNG(temp_file)
      unlink(temp_file)

      ggplot_diagram <- ggplot() +
        annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
        xlim(0, 1) + ylim(0, 1) +
        theme_void() +
        ggtitle(paste(plot_type, "Diagram")) +
        theme(plot.title = element_text(hjust = 0.5, size = 14))

      debug_log(paste("Successfully converted", plot_type, "plot to ggplot"), 1)
      return(ggplot_diagram)
    } else {
      unlink(temp_file)
      stop("png package not available for grid conversion")
    }
  }, error = function(e) {
    debug_log(paste("Error converting", plot_type, "to ggplot:", e$message), 1)

    if (exists("temp_file")) unlink(temp_file)

    fallback_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste(plot_type, "Diagram\n(Conversion Error)"),
               size = 6, hjust = 0.5, vjust = 0.5) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      ggtitle(paste(plot_type, "Diagram")) +
      theme(
        plot.title       = element_text(hjust = 0.5, size = 14),
        panel.background = element_rect(fill = "lightgray", color = "black")
      )

    debug_log(paste("Using fallback ggplot for", plot_type), 1)
    return(fallback_plot)
  })
}
