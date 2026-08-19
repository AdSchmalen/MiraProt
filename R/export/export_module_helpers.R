# ==============================================================================
# File: R/export/export_module_helpers.R
#
# Purpose:
#   Module-specific helper functions used by the comprehensive Excel export
#   workflow (safe extraction and enhanced sheet helpers).
#
# Architectural Role:
#   Helper layer for module adapters consumed by export orchestration.
#
# Structure:
#   1. Safe module data extraction helper
#   2. Volcano matrix assembly helper
#   3. Enhanced Heatmap sheet helper
#   4. Enhanced GSEA sheet helper
#
# Return Interface (public API):
#   safe_extract_module_data()
#   create_volcano_matrix_from_plots()
#   create_enhanced_heatmap_excel_sheet()
#   create_enhanced_gsea_excel_sheet()
#
# Notes for future developers:
#   - Keep function signatures backward-compatible with existing callers.
#   - Preserve defensive tryCatch behavior and debug_log semantics.
#   - Avoid Shiny observer/reactive side-effects in this helper layer.
# ==============================================================================

# ========================================
# Safe Module Data Extraction (Helper Function)
# ========================================

#' Safely extract data from module output (helper for other modules)
#' @param module_out module output object
#' @param function_name name of the function to call
#' @param module_name name of the module for logging
#' @param debug_log debug logging function
#' @return data frame or NULL if extraction fails
safe_extract_module_data <- function(module_out, function_name, module_name, debug_log) {
  tryCatch({
    if (is.null(module_out)) {
      debug_log(paste("Module", module_name, "not available"), level = 2)
      return(NULL)
    }

    # Handle different module output types
    if (is.function(module_out)) {
      debug_log(paste("Module", module_name, "is a function, cannot extract data directly"), level = 2)
      return(NULL)
    }

    # Check if the function exists in the module
    if (is.list(module_out) && function_name %in% names(module_out)) {
      func <- module_out[[function_name]]

      if (is.function(func)) {
        debug_log(paste("Calling", function_name, "from", module_name), level = 2)

        # Try to call the function
        data_result <- func()

        # Handle reactive values
        if (is.reactive(data_result)) {
          data_result <- data_result()
        }

        if (!is.null(data_result) && is.data.frame(data_result) && nrow(data_result) > 0) {
          debug_log(paste("Successfully extracted", nrow(data_result), "rows from", module_name), level = 1)
          return(data_result)
        } else {
          debug_log(paste("No valid data returned from", module_name, function_name), level = 2)
          return(NULL)
        }
      } else {
        debug_log(paste("Function", function_name, "is not callable in", module_name), level = 2)
        return(NULL)
      }
    } else {
      debug_log(paste("Function", function_name, "not found in", module_name), level = 2)
      return(NULL)
    }
  }, error = function(e) {
    debug_log(paste("Error extracting data from", module_name, ":", e$message), level = 1)
    return(NULL)
  })
}

create_volcano_matrix_from_plots <- function(plot_data_list, debug_log) {
  # Get all unique identifiers
  all_identifiers <- character(0)
  identifier_col <- NULL

  for (plot_data in plot_data_list) {
    possible_id_cols <- c("Identifier", "Gene", "Protein", "ID", "identifier", "gene", "protein", "id")
    id_col <- intersect(possible_id_cols, names(plot_data))

    if (length(id_col) > 0) {
      identifier_col <- id_col[1]
      all_identifiers <- c(all_identifiers, as.character(plot_data[[identifier_col]]))
    }
  }

  if (is.null(identifier_col) || length(all_identifiers) == 0) {
    debug_log("No identifier column found in volcano plots", level = 1)
    return(NULL)
  }

  unique_identifiers <- unique(all_identifiers)
  debug_log(paste("Creating volcano matrix with", length(unique_identifiers), "identifiers"), level = 1)

  # Initialize matrix
  volcano_matrix <- data.frame(
    Identifier = unique_identifiers,
    stringsAsFactors = FALSE
  )

  # Add columns for each plot
  for (plot_name in names(plot_data_list)) {
    plot_data <- plot_data_list[[plot_name]]

    # Find X and Y columns
    x_possible <- c("x", "LogFC", "log2FC", "logFC", "FoldChange", "fold_change")
    y_possible <- c("y", "PValue", "p.value", "pvalue", "P.Value", "neg_log10_p")

    x_col <- intersect(x_possible, names(plot_data))
    y_col <- intersect(y_possible, names(plot_data))

    if (length(x_col) > 0 && length(y_col) > 0) {
      x_col <- x_col[1]
      y_col <- y_col[1]

      # Create unique column names
      x_col_name <- make.unique(c(names(volcano_matrix), paste0(plot_name, "_LogFC")))[length(names(volcano_matrix)) + 1]
      y_col_name <- make.unique(c(names(volcano_matrix), paste0(plot_name, "_PValue")))[length(names(volcano_matrix)) + 2]

      # Initialize columns
      volcano_matrix[[x_col_name]] <- NA_real_
      volcano_matrix[[y_col_name]] <- NA_real_

      # Fill data
      for (i in seq_len(nrow(volcano_matrix))) {
        identifier <- volcano_matrix$Identifier[i]
        matching_row <- which(as.character(plot_data[[identifier_col]]) == identifier)

        if (length(matching_row) > 0) {
          matching_row <- matching_row[1]
          volcano_matrix[[x_col_name]][i] <- plot_data[[x_col]][matching_row]
          volcano_matrix[[y_col_name]][i] <- plot_data[[y_col]][matching_row]
        }
      }

      debug_log(paste("Added volcano columns:", x_col_name, ",", y_col_name), level = 2)
    }
  }

  return(volcano_matrix)
}

#' Enhanced Heatmap Data Export for Excel
#' This function should be integrated into the create_comprehensive_excel function
#' in the section where heatmap data is processed
create_enhanced_heatmap_excel_sheet <- function(wb, module_outputs, debug_log, headerStyle) {

  debug_log("Starting enhanced heatmap Excel export", 1)

  tryCatch({

    # Check if heatmap module exists
    if (is.null(module_outputs$heatmap_out)) {
      debug_log("No heatmap module output found", 1)
      return(FALSE)
    }

    heatmap_module <- module_outputs$heatmap_out

    # Create the heatmap data sheet
    sheet_name <- "Heatmap_Data"
    openxlsx::addWorksheet(wb, sheet_name)

    current_row <- 1
    sheets_data_found <- FALSE

    # ========================================
    # 1. Expression Heatmap z-scores
    # ========================================
    debug_log("Processing Expression Heatmap z-scores", 1)

    if ("get_expression_heatmap_for_export" %in% names(heatmap_module)) {
      expr_heatmap_reactive <- heatmap_module$get_expression_heatmap_for_export
      if (is.function(expr_heatmap_reactive)) {
        expr_heatmap <- expr_heatmap_reactive()

        if (!is.null(expr_heatmap) && (is.matrix(expr_heatmap) || is.data.frame(expr_heatmap))) {
          debug_log(paste("Found expression heatmap z-scores:", nrow(expr_heatmap), "x", ncol(expr_heatmap)), 1)

          # Write section header
          writeData_sanitized(wb, sheet_name,
                              "Expression Heatmap z-scores",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert to data frame with protein names
          if (is.matrix(expr_heatmap)) {
            expr_df <- as.data.frame(expr_heatmap)
            if (!is.null(rownames(expr_heatmap))) {
              expr_df$Protein <- rownames(expr_heatmap)
              expr_df <- expr_df[, c("Protein", setdiff(names(expr_df), "Protein")), drop = FALSE]
            }
          } else {
            expr_df <- expr_heatmap
          }

          # Write data
          writeData_sanitized(wb, sheet_name, expr_df, startRow = current_row, startCol = 1)
          if (ncol(expr_df) > 0) {
            openxlsx::addStyle(wb, sheet_name, headerStyle,
                               rows = current_row, cols = 1:ncol(expr_df))
          }
          current_row <- current_row + nrow(expr_df) + 3
          sheets_data_found <- TRUE
        }
      }
    }

    # ========================================
    # 2. Pairwise Correlation Heatmap Proteins Pearson r
    # ========================================
    debug_log("Processing Protein Correlation Heatmap", 1)

    if ("get_protein_correlation_for_export" %in% names(heatmap_module)) {
      protein_corr_reactive <- heatmap_module$get_protein_correlation_for_export
      if (is.function(protein_corr_reactive)) {
        protein_corr <- protein_corr_reactive()

        if (!is.null(protein_corr) && (is.matrix(protein_corr) || is.data.frame(protein_corr))) {
          debug_log(paste("Found protein correlation matrix:", nrow(protein_corr), "x", ncol(protein_corr)), 1)

          # Write section header
          writeData_sanitized(wb, sheet_name,
                              "Pairwise Correlation Heatmap Proteins Pearson r",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert to data frame with protein names
          if (is.matrix(protein_corr)) {
            protein_corr_df <- as.data.frame(protein_corr)
            if (!is.null(rownames(protein_corr))) {
              protein_corr_df$Protein <- rownames(protein_corr)
              protein_corr_df <- protein_corr_df[, c("Protein", setdiff(names(protein_corr_df), "Protein")), drop = FALSE]
            }
          } else {
            protein_corr_df <- protein_corr
          }

          # Write data
          writeData_sanitized(wb, sheet_name, protein_corr_df, startRow = current_row, startCol = 1)
          if (ncol(protein_corr_df) > 0) {
            openxlsx::addStyle(wb, sheet_name, headerStyle,
                               rows = current_row, cols = 1:ncol(protein_corr_df))
          }
          current_row <- current_row + nrow(protein_corr_df) + 3
          sheets_data_found <- TRUE
        }
      }
    }

    # ========================================
    # 3. Pairwise Correlation Heatmap Samples Pearson r
    # ========================================
    debug_log("Processing Sample Correlation Heatmap", 1)

    if ("get_sample_correlation_for_export" %in% names(heatmap_module)) {
      sample_corr_reactive <- heatmap_module$get_sample_correlation_for_export
      if (is.function(sample_corr_reactive)) {
        sample_corr <- sample_corr_reactive()

        if (!is.null(sample_corr) && (is.matrix(sample_corr) || is.data.frame(sample_corr))) {
          debug_log(paste("Found sample correlation matrix:", nrow(sample_corr), "x", ncol(sample_corr)), 1)

          # Write section header
          writeData_sanitized(wb, sheet_name,
                              "Pairwise Correlation Heatmap Samples Pearson r",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert to data frame with sample names
          if (is.matrix(sample_corr)) {
            sample_corr_df <- as.data.frame(sample_corr)
            if (!is.null(rownames(sample_corr))) {
              sample_corr_df$Sample <- rownames(sample_corr)
              sample_corr_df <- sample_corr_df[, c("Sample", setdiff(names(sample_corr_df), "Sample")), drop = FALSE]
            }
          } else {
            sample_corr_df <- sample_corr
          }

          # Write data
          writeData_sanitized(wb, sheet_name, sample_corr_df, startRow = current_row, startCol = 1)
          if (ncol(sample_corr_df) > 0) {
            openxlsx::addStyle(wb, sheet_name, headerStyle,
                               rows = current_row, cols = 1:ncol(sample_corr_df))
          }
          current_row <- current_row + nrow(sample_corr_df) + 3
          sheets_data_found <- TRUE
        }
      }
    }

    # ========================================
    # 4. Basemean Heatmap
    # ========================================
    debug_log("Processing Basemean Heatmap", 1)

    if ("get_basemean_heatmap_for_export" %in% names(heatmap_module)) {
      basemean_reactive <- heatmap_module$get_basemean_heatmap_for_export
      if (is.function(basemean_reactive)) {
        basemean_data <- basemean_reactive()

        if (!is.null(basemean_data) && (is.matrix(basemean_data) || is.data.frame(basemean_data) || is.numeric(basemean_data))) {
          debug_log(paste("Found basemean data, length/nrow:",
                          if(is.matrix(basemean_data) || is.data.frame(basemean_data)) nrow(basemean_data) else length(basemean_data)), 1)

          # Write section header
          writeData_sanitized(wb, sheet_name,
                              "Basemean Heatmap",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert to data frame
          if (is.matrix(basemean_data)) {
            basemean_df <- as.data.frame(basemean_data)
            if (!is.null(rownames(basemean_data))) {
              basemean_df$Protein <- rownames(basemean_data)
              basemean_df <- basemean_df[, c("Protein", setdiff(names(basemean_df), "Protein")), drop = FALSE]
            }
          } else if (is.numeric(basemean_data) && !is.null(names(basemean_data))) {
            basemean_df <- data.frame(
              Protein = names(basemean_data),
              log2_Basemean = basemean_data,
              stringsAsFactors = FALSE
            )
          } else {
            basemean_df <- basemean_data
          }

          # Write data
          writeData_sanitized(wb, sheet_name, basemean_df, startRow = current_row, startCol = 1)
          if (ncol(basemean_df) > 0) {
            openxlsx::addStyle(wb, sheet_name, headerStyle,
                               rows = current_row, cols = 1:ncol(basemean_df))
          }
          current_row <- current_row + nrow(basemean_df) + 3
          sheets_data_found <- TRUE
        }
      }
    }

    # ========================================
    # 5. Abundance Ratio Heatmap
    # ========================================
    debug_log("Processing Abundance Ratio Heatmap", 1)

    if ("get_abundance_ratio_heatmap_for_export" %in% names(heatmap_module)) {
      ratio_reactive <- heatmap_module$get_abundance_ratio_heatmap_for_export
      if (is.function(ratio_reactive)) {
        ratio_data <- ratio_reactive()

        if (!is.null(ratio_data) && (is.matrix(ratio_data) || is.data.frame(ratio_data) || is.numeric(ratio_data))) {
          debug_log(paste("Found abundance ratio data, length/nrow:",
                          if(is.matrix(ratio_data) || is.data.frame(ratio_data)) nrow(ratio_data) else length(ratio_data)), 1)

          # Write section header
          writeData_sanitized(wb, sheet_name,
                              "Abundance Ratio Heatmap",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert to data frame
          if (is.matrix(ratio_data)) {
            ratio_df <- as.data.frame(ratio_data)
            if (!is.null(rownames(ratio_data))) {
              ratio_df$Protein <- rownames(ratio_data)
              ratio_df <- ratio_df[, c("Protein", setdiff(names(ratio_df), "Protein")), drop = FALSE]
            }
          } else if (is.numeric(ratio_data) && !is.null(names(ratio_data))) {
            ratio_df <- data.frame(
              Protein = names(ratio_data),
              log2_Abundance_Ratio = ratio_data,
              stringsAsFactors = FALSE
            )
          } else {
            ratio_df <- ratio_data
          }

          # Write data
          writeData_sanitized(wb, sheet_name, ratio_df, startRow = current_row, startCol = 1)
          if (ncol(ratio_df) > 0) {
            openxlsx::addStyle(wb, sheet_name, headerStyle,
                               rows = current_row, cols = 1:ncol(ratio_df))
          }
          current_row <- current_row + nrow(ratio_df) + 3
          sheets_data_found <- TRUE
        }
      }
    }

    # ========================================
    # Add Export Summary at the end
    # ========================================
    if ("get_export_summary" %in% names(heatmap_module)) {
      summary_reactive <- heatmap_module$get_export_summary
      if (is.function(summary_reactive)) {
        summary_data <- summary_reactive()

        if (!is.null(summary_data)) {
          debug_log("Adding heatmap export summary", 2)

          writeData_sanitized(wb, sheet_name,
                              "Heatmap Export Summary",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Convert summary to data frame
          summary_df <- data.frame(
            Property = names(summary_data),
            Value = as.character(unlist(summary_data)),
            stringsAsFactors = FALSE
          )

          writeData_sanitized(wb, sheet_name, summary_df, startRow = current_row, startCol = 1)
          openxlsx::addStyle(wb, sheet_name, headerStyle,
                             rows = current_row, cols = 1:2)
        }
      }
    }

    if (!sheets_data_found) {
      debug_log("No heatmap data found for export", 1)
      # Write placeholder message
      writeData_sanitized(wb, sheet_name,
                          "No heatmap data available - please generate heatmaps first",
                          startRow = 1, startCol = 1)
      return(FALSE)
    }

    debug_log("Enhanced heatmap Excel sheet created successfully", 1)
    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error creating enhanced heatmap Excel sheet:", e$message), 1)
    return(FALSE)
  })
}

#' Create enhanced GSEA Excel sheet with complete S4 reconstruction data
#' @param wb Workbook object
#' @param gsea_data GSEA results data frame
#' @param headerStyle Header style object
#' @param debug_log Debug logging function
#' @param original_s4_obj Original gseaResult S4 object for comparison
#' @param gene_list Gene rankings list
#' @param gene_sets Gene sets list
#' @param fc_vec Fold change vector
#' @return List with success status and export details
create_enhanced_gsea_excel_sheet <- function(wb, gsea_data, headerStyle, debug_log,
                                             original_s4_obj = NULL, gene_list = NULL,
                                             gene_sets = NULL, fc_vec = NULL) {
  result <- list(success = FALSE, gene_list_exported = FALSE, gene_sets_exported = FALSE)

  tryCatch({
    if (is.null(gsea_data) || !is.data.frame(gsea_data) || nrow(gsea_data) == 0) {
      debug_log("No valid GSEA data for enhanced Excel export", level = 2)
      return(result)
    }

    debug_log("=== CREATING ENHANCED GSEA EXCEL EXPORT ===", level = 1)
    debug_log(paste("Input GSEA data columns:", paste(colnames(gsea_data), collapse = ", ")), level = 1)
    debug_log(paste("Input GSEA data rows:", nrow(gsea_data)), level = 1)

    # Start with the original data and rename columns to Excel format
    enhanced_gsea <- gsea_data

    # Column mapping: internal_name -> Excel_name (ORIGINAL NAMES)
    col_mapping <- list(
      "ID" = "Pathway ID",
      "Description" = "Description",
      "setSize" = "Set Size",
      "enrichmentScore" = "Enrichment Score",
      "NES" = "NES",
      "pvalue" = "P Value",
      "p.adjust" = "Adjusted P Value",
      "qvalue" = "Q Value",
      "rank" = "Rank",
      "leading_edge" = "Leading Edge",
      "core_enrichment" = "Core Enrichment Genes"
    )

    # Rename existing columns to Excel format
    for (internal_col in names(col_mapping)) {
      excel_col <- col_mapping[[internal_col]]
      if (internal_col %in% colnames(enhanced_gsea)) {
        colnames(enhanced_gsea)[colnames(enhanced_gsea) == internal_col] <- excel_col
        debug_log(paste("Renamed", internal_col, "->", excel_col), level = 2)
      }
    }

    # Add Gene Set Members column from S4 object gene sets
    if (!is.null(gene_sets) && length(gene_sets) > 0) {
      debug_log("Adding Gene Set Members column from S4 object", level = 1)
      enhanced_gsea[["Gene Set Members"]] <- character(nrow(enhanced_gsea))

      for (i in 1:nrow(enhanced_gsea)) {
        tryCatch({
          pathway_id <- as.character(enhanced_gsea[["Pathway ID"]][i])
          debug_log(paste("Processing pathway", i, ":", pathway_id), level = 2)

          if (!is.na(pathway_id) && nzchar(pathway_id) && pathway_id %in% names(gene_sets)) {
            genes <- gene_sets[[pathway_id]]
            if (length(genes) > 0) {
              enhanced_gsea[["Gene Set Members"]][i] <- paste(genes, collapse = "/")
              debug_log(paste("Added", length(genes), "genes for pathway:", pathway_id), level = 2)
            }
          } else {
            debug_log(paste("No gene set found for pathway:", pathway_id), level = 2)
          }
        }, error = function(e) {
          debug_log(paste("Error processing pathway", i, ":", e$message), level = 1)
        })
      }
      result$gene_sets_exported <- TRUE
    }

    # Add comprehensive gene ranking data
    if (!is.null(gene_list) && length(gene_list) > 0) {
      debug_log(paste("Adding comprehensive ranking vector data with", length(gene_list), "genes"), level = 1)

      # Create ranking vector string (gene=rank format)
      ranking_pairs <- paste(names(gene_list), gene_list, sep = "=")
      ranking_vector_str <- paste(ranking_pairs, collapse = ";")

      debug_log(paste("Ranking vector string length:", nchar(ranking_vector_str)), level = 1)

      # Split into chunks to avoid Excel cell limits (32,767 characters)
      max_chars <- 30000
      n_chunks <- ceiling(nchar(ranking_vector_str) / max_chars)
      debug_log(paste("Splitting ranking vector into", n_chunks, "chunks"), level = 1)

      for (chunk in 1:n_chunks) {
        col_name <- if (chunk == 1) "Ranking Vector" else paste0("Ranking Vector (", chunk, ")")
        start_pos <- (chunk - 1) * max_chars + 1
        end_pos <- min(chunk * max_chars, nchar(ranking_vector_str))
        chunk_str <- substr(ranking_vector_str, start_pos, end_pos)

        # Initialize column with correct length
        enhanced_gsea[[col_name]] <- character(nrow(enhanced_gsea))
        # Add data to first row only
        enhanced_gsea[[col_name]][1] <- chunk_str

        debug_log(paste("Added ranking chunk", chunk, "with", nchar(chunk_str), "characters"), level = 2)
      }

      result$gene_list_exported <- TRUE
    }

    # Add fold change vector data
    if (!is.null(fc_vec) && length(fc_vec) > 0) {
      debug_log(paste("Adding fold change vector data with", length(fc_vec), "genes"), level = 1)

      # Create fold change vector string
      fc_pairs <- paste(names(fc_vec), fc_vec, sep = "=")
      fc_vector_str <- paste(fc_pairs, collapse = ";")

      debug_log(paste("FC vector string length:", nchar(fc_vector_str)), level = 1)

      # Split into chunks
      max_chars <- 30000
      n_chunks <- ceiling(nchar(fc_vector_str) / max_chars)
      debug_log(paste("Splitting FC vector into", n_chunks, "chunks"), level = 1)

      for (chunk in 1:n_chunks) {
        col_name <- if (chunk == 1) "Fold Change Vector" else paste0("Fold Change Vector (", chunk, ")")
        start_pos <- (chunk - 1) * max_chars + 1
        end_pos <- min(chunk * max_chars, nchar(fc_vector_str))
        chunk_str <- substr(fc_vector_str, start_pos, end_pos)

        # Initialize column with correct length
        enhanced_gsea[[col_name]] <- character(nrow(enhanced_gsea))
        # Add data to first row only
        enhanced_gsea[[col_name]][1] <- chunk_str

        debug_log(paste("Added FC chunk", chunk, "with", nchar(chunk_str), "characters"), level = 2)
      }
    }

    # Sort by NES (descending) for better readability
    if ("NES" %in% colnames(enhanced_gsea)) {
      enhanced_gsea <- enhanced_gsea[order(as.numeric(enhanced_gsea[["NES"]]), decreasing = TRUE, na.last = TRUE), ]
      debug_log("Sorted by NES column", level = 1)
    }

    debug_log(paste("Enhanced GSEA data prepared:", nrow(enhanced_gsea), "rows,", ncol(enhanced_gsea), "columns"), level = 1)
    debug_log(paste("Final columns:", paste(colnames(enhanced_gsea), collapse = ", ")), level = 1)

    # Write to Excel
    openxlsx::addWorksheet(wb, "GSEA_Analysis")
    writeData_sanitized(wb, "GSEA_Analysis", enhanced_gsea, startRow = 1, startCol = 1)

    # Apply header style
    if (ncol(enhanced_gsea) > 0) {
      openxlsx::addStyle(wb, "GSEA_Analysis", headerStyle, rows = 1, cols = 1:ncol(enhanced_gsea))
    }

    # Debug what was exported
    debug_log("=== EXCEL EXPORT SUMMARY ===", level = 1)
    debug_log(paste("Successfully exported", nrow(enhanced_gsea), "pathways"), level = 1)
    debug_log(paste("Total columns exported:", ncol(enhanced_gsea)), level = 1)

    result$success <- TRUE

    return(result)

  }, error = function(e) {
    debug_log(paste("Error creating enhanced GSEA Excel sheet:", e$message), level = 1)
    debug_log(paste("Error traceback:", toString(e)), level = 1)
    return(result)
  })
}
