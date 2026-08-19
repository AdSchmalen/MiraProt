# ==============================================================================
# File: R/export/export_pipeline_matrix_heatmap.R
#
# Purpose:
#   Execute matrix-format exports and comprehensive heatmap export staging.
#
# Architectural Role:
#   Stage helper for create_comprehensive_excel() pipeline orchestration.
#
# Responsibilities:
#   - Export volcano matrix/dotplot matrix/venn-upset sheets.
#   - Export consolidated Heatmap_Data sheet sections.
#   - Maintain shared counters and debug logs in context.
#
# Non-Responsibilities (Must NOT be here):
#   - Base sheet creation, optional module fallbacks, finalization logic.
#
# Public API:
#   export_pipeline_run_matrix_heatmap()
# ==============================================================================

#' Pipeline stage: matrix + heatmap sheets
#'
#' Purpose:
#'   Run volcano/dot/venn matrix exports and heatmap staging.
#'
#' Inputs/Parameters:
#'   @param ctx Environment-based export context.
#'
#' Outputs:
#'   - Invisibly returns NULL; mutates `ctx` in place.
#'
#' Side effects:
#'   - Adds worksheets and writes workbook content.
#'
#' Failure behavior:
#'   - Uses existing localized tryCatch guards to keep pipeline robust.
export_pipeline_run_matrix_heatmap <- function(ctx) {
  with(ctx, {
    # ========================================
    # Sheet 9: Volcano Plot Data (MATRIX FORMAT - One Row per Identifier)
    # ========================================
    debug_log("Checking for Volcano plot data via CORRECTED access", level = 1)

    volcano_matrix_data <- NULL
    if (!is.null(module_outputs) && !is.null(module_outputs$volcano_out)) {
      tryCatch({
        volcano_module <- module_outputs$volcano_out
        debug_log(paste("CORRECTED Volcano module names:", paste(names(volcano_module), collapse = ", ")), level = 1)

        if (is.list(volcano_module)) {
          # Try correct reactive name: "plots" instead of "volcano_state$static_plots"
          if ("plots" %in% names(volcano_module)) {
            plots_reactive <- volcano_module$plots
            if (is.function(plots_reactive)) {
              plots_data <- plots_reactive()

              if (!is.null(plots_data) && is.list(plots_data)) {
                debug_log(paste("Found volcano plots:", length(plots_data)), level = 1)

                # Process each plot
                plot_data_list <- list()

                for (i in seq_along(plots_data)) {
                  plot_obj <- plots_data[[i]]
                  if (!is.null(plot_obj) && inherits(plot_obj, "ggplot") && !is.null(plot_obj$data)) {
                    plot_title <- names(plots_data)[i] %||% paste("Volcano", i)
                    plot_data_list[[plot_title]] <- plot_obj$data
                    debug_log(paste("Extracted volcano data:", plot_title, "with", nrow(plot_obj$data), "points"), level = 2)
                  }
                }

                # Create matrix format as before...
                if (length(plot_data_list) > 0) {
                  # [Your existing matrix creation logic here]
                  volcano_matrix_data <- create_volcano_matrix_from_plots(plot_data_list, debug_log)
                }
              }
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Corrected volcano access failed:", e$message), level = 2)
      })
    }

    # ========================================
    # Sheet 10: Dot Plot Data (MATRIX FORMAT - One Row per Identifier)
    # ========================================
    debug_log("Checking for Dot Plot data via direct access", level = 1)

    dotplot_matrix_data <- NULL
    if (!is.null(module_outputs) && !is.null(module_outputs$dotplot_out)) {
      tryCatch({
        dotplot_module <- module_outputs$dotplot_out

        # Try to get plot object data
        if (is.list(dotplot_module)) {
          plot_obj <- NULL

          # Try different possible reactive names
          possible_plot_reactives <- c("current_plot_object", "plot_object", "ggplot_object")

          for (reactive_name in possible_plot_reactives) {
            if (reactive_name %in% names(dotplot_module)) {
              plot_reactive <- dotplot_module[[reactive_name]]
              if (is.function(plot_reactive)) {
                tryCatch({
                  plot_obj <- plot_reactive()
                  if (!is.null(plot_obj) && inherits(plot_obj, "ggplot")) {
                    debug_log(paste("Found dot plot object via", reactive_name), level = 2)
                    break
                  }
                }, error = function(e) {
                  debug_log(paste("Error accessing", reactive_name, ":", e$message), level = 2)
                })
              }
            }
          }

          # Extract data from plot object
          if (!is.null(plot_obj) && !is.null(plot_obj$data)) {
            plot_data <- plot_obj$data
            debug_log(paste("Extracted dot plot data with", nrow(plot_data), "rows and", ncol(plot_data), "columns"), level = 2)

            # Find identifier column
            possible_id_cols <- c("Identifier", "Gene", "Protein", "ID", "identifier", "gene", "protein", "id")
            id_col <- intersect(possible_id_cols, names(plot_data))

            if (length(id_col) > 0) {
              identifier_col <- id_col[1]

              # Find X and Y axis columns
              x_col <- NULL
              y_col <- NULL

              # Common axis column patterns
              axis_cols <- setdiff(names(plot_data), identifier_col)

              # Try to identify X and Y columns
              if (length(axis_cols) >= 2) {
                # Use first two non-identifier columns as X and Y
                x_col <- axis_cols[1]
                y_col <- axis_cols[2]
              } else if ("x" %in% names(plot_data) && "y" %in% names(plot_data)) {
                x_col <- "x"
                y_col <- "y"
              }

              if (!is.null(x_col) && !is.null(y_col)) {
                # Create matrix format
                dotplot_matrix_data <- data.frame(
                  Identifier = as.character(plot_data[[identifier_col]]),
                  stringsAsFactors = FALSE
                )

                # Apply make.unique to column names
                x_col_name <- make.unique(c("Identifier", x_col))[2]
                y_col_name <- make.unique(c("Identifier", x_col, y_col))[3]

                dotplot_matrix_data[[x_col_name]] <- plot_data[[x_col]]
                dotplot_matrix_data[[y_col_name]] <- plot_data[[y_col]]

                # Remove duplicates based on Identifier
                dotplot_matrix_data <- dotplot_matrix_data[!duplicated(dotplot_matrix_data$Identifier), ]

                debug_log(paste("Created dot plot matrix with", nrow(dotplot_matrix_data), "unique identifiers"), level = 1)
                debug_log(paste("Columns: Identifier,", x_col_name, ",", y_col_name), level = 2)
              }
            }
          }
        }

      }, error = function(e) {
        debug_log(paste("Direct dot plot access failed:", e$message), level = 2)
      })
    }

    if (!is.null(dotplot_matrix_data) && nrow(dotplot_matrix_data) > 0) {
      tryCatch({
        openxlsx::addWorksheet(wb, "DotPlot_Data")
        writeData_sanitized(wb, "DotPlot_Data", dotplot_matrix_data, startRow = 1, startCol = 1)

        if (ncol(dotplot_matrix_data) > 0) {
          openxlsx::addStyle(wb, "DotPlot_Data", headerStyle, rows = 1, cols = 1:ncol(dotplot_matrix_data))
        }

        sheets_created <- sheets_created + 1
        debug_log("Dot Plot Data sheet created successfully in matrix format", level = 1)
      }, error = function(e) {
        debug_log(paste("Error creating Dot Plot Data sheet:", e$message), level = 1)
      })
    } else {
      debug_log("No Dot Plot data available for Excel export", level = 2)
    }

    # ========================================
    # Sheet 12: Venn / UpSet Results
    # ========================================
    debug_log("Checking for Venn/UpSet export data", level = 1)

    if (!is.null(module_outputs) && !is.null(module_outputs$venn_out)) {
      tryCatch({
        venn_module <- module_outputs$venn_out
        debug_log(paste("Venn module functions:", paste(names(venn_module), collapse = ", ")), level = 1)

        venn_sheet_written <- FALSE

        if (is.list(venn_module) && "get_venn_export_workbook" %in% names(venn_module) &&
            is.function(venn_module$get_venn_export_workbook)) {
          venn_wb <- venn_module$get_venn_export_workbook()
          if (!is.null(venn_wb)) {
            temp_venn <- tempfile(fileext = ".xlsx")
            openxlsx::saveWorkbook(venn_wb, temp_venn, overwrite = TRUE)
            venn_df <- tryCatch(openxlsx::readWorkbook(temp_venn, sheet = 1), error = function(e) NULL)
            unlink(temp_venn)

            if (!is.null(venn_df) && is.data.frame(venn_df) && nrow(venn_df) > 0) {
              # The venn workbook's first row is a section label ("Input Lists"), which
              # readWorkbook uses as column names → "Input.Lists", "X2", "X3", etc.
              # The actual column headers (List, Count, Proteins, ...) are in the first
              # data row. Promote them to column names and drop that row.
              actual_names <- as.character(unlist(venn_df[1, ]))
              keep <- !is.na(actual_names) & nzchar(trimws(actual_names))
              actual_names[!keep] <- colnames(venn_df)[!keep]
              if (length(actual_names) > 0 && trimws(actual_names[1]) == "List") {
                actual_names[1] <- "Input Lists"
              }
              colnames(venn_df) <- actual_names
              venn_df <- venn_df[-1, , drop = FALSE]
              rownames(venn_df) <- NULL

              openxlsx::addWorksheet(wb, "Venn_UpSet_Results")
              writeData_sanitized(wb, "Venn_UpSet_Results", venn_df, startRow = 1, startCol = 1)
              if (ncol(venn_df) > 0) {
                openxlsx::addStyle(wb, "Venn_UpSet_Results", headerStyle, rows = 1, cols = 1:ncol(venn_df))
              }
              sheets_created <- sheets_created + 1
              venn_sheet_written <- TRUE
              debug_log("Venn/UpSet results sheet created via get_venn_export_workbook", level = 1)
            }
          }
        }

        if (!venn_sheet_written && is.list(venn_module) && "get_intersection_list" %in% names(venn_module) &&
            is.function(venn_module$get_intersection_list)) {
          intersections <- venn_module$get_intersection_list()
          if (!is.null(intersections) && length(intersections) > 0) {
            overlap_list <- list()
            for (intersection_name in names(intersections)) {
              proteins <- intersections[[intersection_name]]
              if (length(proteins) > 0) {
                overlap_list[[intersection_name]] <- data.frame(
                  Intersection = intersection_name,
                  Protein_Count = length(proteins),
                  Proteins = paste(proteins, collapse = "; "),
                  stringsAsFactors = FALSE
                )
              }
            }

            if (length(overlap_list) > 0) {
              venn_overlap_data <- do.call(rbind, overlap_list)
              rownames(venn_overlap_data) <- NULL
              openxlsx::addWorksheet(wb, "Venn_UpSet_Results")
              writeData_sanitized(wb, "Venn_UpSet_Results", venn_overlap_data, startRow = 1, startCol = 1)
              if (ncol(venn_overlap_data) > 0) {
                openxlsx::addStyle(wb, "Venn_UpSet_Results", headerStyle, rows = 1, cols = 1:ncol(venn_overlap_data))
              }
              sheets_created <- sheets_created + 1
              debug_log("Venn/UpSet results sheet created via intersection fallback", level = 1)
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Venn/UpSet sheet creation failed:", e$message), level = 1)
      })
    }

    # ========================================
    # Sheet 14: Heatmap Data (ALL TYPES IN ONE SHEET WITH COLORS)
    # ========================================
    debug_log("Creating comprehensive heatmap data sheet", 1)

    if (!is.null(module_outputs) && !is.null(module_outputs$heatmap_out)) {
      tryCatch({
        heatmap_module <- module_outputs$heatmap_out
        debug_log(paste("Heatmap module detected, available functions:", paste(names(heatmap_module), collapse = ", ")), 1)

        # Check if we have any heatmap data to export
        has_any_data <- FALSE
        data_check_results <- list()

        # Check each data type with enhanced debugging

        # 1. Expression data
        if ("get_expression_matrix" %in% names(heatmap_module)) {
          expr_data <- tryCatch({
            expr_reactive <- heatmap_module$get_expression_matrix
            if (is.function(expr_reactive)) expr_reactive() else NULL
          }, error = function(e) NULL)
          data_check_results$expression <- !is.null(expr_data) && (is.matrix(expr_data) || is.data.frame(expr_data)) && nrow(expr_data) > 0
          if (data_check_results$expression) has_any_data <- TRUE
          debug_log(paste("Expression check: has data =", data_check_results$expression), 1)
        }

        # 2. Protein correlation data
        if ("get_protein_correlation" %in% names(heatmap_module)) {
          prot_data <- tryCatch({
            prot_reactive <- heatmap_module$get_protein_correlation
            if (is.function(prot_reactive)) prot_reactive() else NULL
          }, error = function(e) NULL)
          data_check_results$protein_correlation <- !is.null(prot_data) && (is.matrix(prot_data) || is.data.frame(prot_data)) && nrow(prot_data) > 0
          if (data_check_results$protein_correlation) has_any_data <- TRUE
          debug_log(paste("Protein correlation check: has data =", data_check_results$protein_correlation), 1)
        }

        # 3. Sample correlation data (improved check)
        if ("get_sample_correlation" %in% names(heatmap_module)) {
          sample_data <- tryCatch({
            sample_reactive <- heatmap_module$get_sample_correlation
            if (is.function(sample_reactive)) sample_reactive() else NULL
          }, error = function(e) NULL)
          data_check_results$sample_correlation <- !is.null(sample_data) && (is.matrix(sample_data) || is.data.frame(sample_data)) && nrow(sample_data) > 0
          if (data_check_results$sample_correlation) has_any_data <- TRUE
          debug_log(paste("Sample correlation check: has data =", data_check_results$sample_correlation), 1)
        }

        # 4. Basemean data (NEW)
        if ("get_basemean_heatmap_for_export" %in% names(heatmap_module)) {
          basemean_data <- tryCatch({
            basemean_reactive <- heatmap_module$get_basemean_heatmap_for_export
            if (is.function(basemean_reactive)) basemean_reactive() else NULL
          }, error = function(e) NULL)
          data_check_results$basemean <- !is.null(basemean_data) && (is.matrix(basemean_data) || is.data.frame(basemean_data) || (is.numeric(basemean_data) && length(basemean_data) > 0))
          if (data_check_results$basemean) has_any_data <- TRUE
          debug_log(paste("Basemean check: has data =", data_check_results$basemean), 1)
        } else {
          debug_log("get_basemean_heatmap_for_export function not found", 1)
        }

        # 5. Abundance ratio data (NEW)
        if ("get_abundance_ratio_heatmap_for_export" %in% names(heatmap_module)) {
          ratio_data <- tryCatch({
            ratio_reactive <- heatmap_module$get_abundance_ratio_heatmap_for_export
            if (is.function(ratio_reactive)) ratio_reactive() else NULL
          }, error = function(e) NULL)
          data_check_results$abundance_ratio <- !is.null(ratio_data) && (is.matrix(ratio_data) || is.data.frame(ratio_data) || (is.numeric(ratio_data) && length(ratio_data) > 0))
          if (data_check_results$abundance_ratio) has_any_data <- TRUE
          debug_log(paste("Abundance ratio check: has data =", data_check_results$abundance_ratio), 1)
        } else {
          debug_log("get_abundance_ratio_heatmap_for_export function not found", 1)
        }

        debug_log(paste("Final heatmap data availability check - has_any_data:", has_any_data), 1)

        # Only create sheet if we have data to export
        if (has_any_data) {
          debug_log("Creating heatmap data sheet with available data", 1)

          openxlsx::addWorksheet(wb, "Heatmap_Data")
          current_row <- 1
          exported_sections <- 0

          # ========================================
          # 1. Expression Heatmap z-scores
          # ========================================
          if (data_check_results$expression) {
            debug_log("Exporting Expression Heatmap z-scores", 1)

            expr_reactive <- heatmap_module$get_expression_matrix
            expr_matrix <- expr_reactive()

            writeData_sanitized(wb, "Heatmap_Data",
                                "Expression Heatmap z-scores",
                                startRow = current_row, startCol = 1)
            openxlsx::addStyle(wb, "Heatmap_Data", headerStyle, rows = current_row, cols = 1)
            current_row <- current_row + 2

            # Convert to data frame with protein names
            expr_df <- as.data.frame(expr_matrix)
            if (!is.null(rownames(expr_matrix))) {
              expr_df$Protein <- rownames(expr_matrix)
              expr_df <- expr_df[, c("Protein", setdiff(names(expr_df), "Protein")), drop = FALSE]
            }

            writeData_sanitized(wb, "Heatmap_Data", expr_df, startRow = current_row, startCol = 1)
            if (ncol(expr_df) > 0) {
              openxlsx::addStyle(wb, "Heatmap_Data", headerStyle,
                                 rows = current_row, cols = 1:ncol(expr_df))
            }
            current_row <- current_row + nrow(expr_df) + 3
            exported_sections <- exported_sections + 1
            debug_log(paste("Expression matrix exported:", nrow(expr_df), "proteins x", ncol(expr_df)-1, "samples"), 1)
          }

          # ========================================
          # 2. Pairwise Correlation Heatmap Proteins Pearson r
          # ========================================
          if (data_check_results$protein_correlation) {
            debug_log("Exporting Protein Correlation Heatmap", 1)

            protein_corr_reactive <- heatmap_module$get_protein_correlation
            corr_matrix <- protein_corr_reactive()

            writeData_sanitized(wb, "Heatmap_Data",
                                "Pairwise Correlation Heatmap Proteins Pearson r",
                                startRow = current_row, startCol = 1)
            openxlsx::addStyle(wb, "Heatmap_Data", headerStyle, rows = current_row, cols = 1)
            current_row <- current_row + 2

            # Convert to data frame with protein names
            corr_df <- as.data.frame(corr_matrix)
            if (!is.null(rownames(corr_matrix))) {
              corr_df$Protein <- rownames(corr_matrix)
              corr_df <- corr_df[, c("Protein", setdiff(names(corr_df), "Protein")), drop = FALSE]
            }

            writeData_sanitized(wb, "Heatmap_Data", corr_df, startRow = current_row, startCol = 1)
            if (ncol(corr_df) > 0) {
              openxlsx::addStyle(wb, "Heatmap_Data", headerStyle,
                                 rows = current_row, cols = 1:ncol(corr_df))
            }
            current_row <- current_row + nrow(corr_df) + 3
            exported_sections <- exported_sections + 1
            debug_log(paste("Protein correlation matrix exported:", nrow(corr_df), "x", ncol(corr_df)-1), 1)
          }

          # ========================================
          # 3. Pairwise Correlation Heatmap Samples Pearson r
          # ========================================
          if (data_check_results$sample_correlation) {
            debug_log("Exporting Sample Correlation Heatmap", 1)

            sample_corr_reactive <- heatmap_module$get_sample_correlation
            sample_corr_matrix <- sample_corr_reactive()

            writeData_sanitized(wb, "Heatmap_Data",
                                "Pairwise Correlation Heatmap Samples Pearson r",
                                startRow = current_row, startCol = 1)
            openxlsx::addStyle(wb, "Heatmap_Data", headerStyle, rows = current_row, cols = 1)
            current_row <- current_row + 2

            # Convert to data frame with sample names
            sample_corr_df <- as.data.frame(sample_corr_matrix)
            if (!is.null(rownames(sample_corr_matrix))) {
              sample_corr_df$Sample <- rownames(sample_corr_matrix)
              sample_corr_df <- sample_corr_df[, c("Sample", setdiff(names(sample_corr_df), "Sample")), drop = FALSE]
            }

            writeData_sanitized(wb, "Heatmap_Data", sample_corr_df, startRow = current_row, startCol = 1)
            if (ncol(sample_corr_df) > 0) {
              openxlsx::addStyle(wb, "Heatmap_Data", headerStyle,
                                 rows = current_row, cols = 1:ncol(sample_corr_df))
            }
            current_row <- current_row + nrow(sample_corr_df) + 3
            exported_sections <- exported_sections + 1
            debug_log(paste("Sample correlation matrix exported:", nrow(sample_corr_df), "x", ncol(sample_corr_df)-1), 1)
          }

          # ========================================
          # 4. Basemean Heatmap
          # ========================================
          if (data_check_results$basemean) {
            debug_log("Exporting Basemean Heatmap", 1)

            basemean_reactive <- heatmap_module$get_basemean_heatmap_for_export
            basemean_data <- basemean_reactive()

            writeData_sanitized(wb, "Heatmap_Data",
                                "Basemean Heatmap",
                                startRow = current_row, startCol = 1)
            openxlsx::addStyle(wb, "Heatmap_Data", headerStyle, rows = current_row, cols = 1)
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
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            } else {
              basemean_df <- basemean_data
            }

            writeData_sanitized(wb, "Heatmap_Data", basemean_df, startRow = current_row, startCol = 1)
            if (ncol(basemean_df) > 0) {
              openxlsx::addStyle(wb, "Heatmap_Data", headerStyle,
                                 rows = current_row, cols = 1:ncol(basemean_df))
            }
            current_row <- current_row + nrow(basemean_df) + 3
            exported_sections <- exported_sections + 1
            debug_log(paste("Basemean data exported:", nrow(basemean_df), "proteins"), 1)
          }

          # ========================================
          # 5. Abundance Ratio Heatmap
          # ========================================
          if (data_check_results$abundance_ratio) {
            debug_log("Exporting Abundance Ratio Heatmap", 1)

            ratio_reactive <- heatmap_module$get_abundance_ratio_heatmap_for_export
            ratio_data <- ratio_reactive()

            writeData_sanitized(wb, "Heatmap_Data",
                                "Abundance Ratio Heatmap",
                                startRow = current_row, startCol = 1)
            openxlsx::addStyle(wb, "Heatmap_Data", headerStyle, rows = current_row, cols = 1)
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
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            } else {
              ratio_df <- ratio_data
            }

            writeData_sanitized(wb, "Heatmap_Data", ratio_df, startRow = current_row, startCol = 1)
            if (ncol(ratio_df) > 0) {
              openxlsx::addStyle(wb, "Heatmap_Data", headerStyle,
                                 rows = current_row, cols = 1:ncol(ratio_df))
            }
            current_row <- current_row + nrow(ratio_df) + 3
            exported_sections <- exported_sections + 1
            debug_log(paste("Abundance ratio data exported:", nrow(ratio_df), "proteins"), 1)
          }

          sheets_created <- sheets_created + 1
          debug_log(paste("Comprehensive heatmap data sheet created successfully with", exported_sections, "sections"), 1)

        } else {
          debug_log("No heatmap data available for export - skipping heatmap sheet creation", 1)
        }

      }, error = function(e) {
        debug_log(paste("Error creating heatmap data sheet:", e$message), 1)
      })
    } else {
      debug_log("No heatmap module output available", 1)
    }

  })
  invisible(NULL)
}
