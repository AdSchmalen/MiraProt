# ==============================================================================
# File: R/export/export_pipeline_datawizard_primary.R
#
# Purpose:
#   Execute the early workbook export pipeline stages for Data Wizard and
#   primary analytical sheets prior to GO/GSEA extraction.
#
# Architectural Role:
#   Stage helper for create_comprehensive_excel() pipeline orchestration.
#
# Responsibilities:
#   - Export Processed_Data, Original_Data, Metadata sheets.
#   - Export Abundance, SampleIDs, and robust PCA sheet content.
#   - Maintain sheet counters and data state in shared export context.
#
# Non-Responsibilities (Must NOT be here):
#   - GO/GSEA staging, matrix/heatmap staging, optional module sheets.
#   - Final workbook summary, log, rename, reorder, and return handling.
#
# Public API:
#   export_pipeline_run_datawizard_primary()
# ==============================================================================

#' Pipeline stage: Data Wizard + primary pre-GO sheets
#'
#' Purpose:
#'   Run the first staged export segment using the shared export context.
#'
#' Inputs/Parameters:
#'   @param ctx Environment-based export context containing workbook, styles,
#'     module outputs, rv state, debug logger, and mutable counters/state.
#'
#' Outputs:
#'   - Invisibly returns NULL; mutates `ctx` in place.
#'
#' Side effects:
#'   - Adds worksheets and writes workbook data; updates `ctx$sheets_created`
#'     and data references (`processed_data`, `original_data`, `metadata`).
#'
#' Failure behavior:
#'   - Relies on localized tryCatch blocks in stage code; logs and degrades
#'     gracefully without aborting the overall export pipeline.

#' Resolve the live Data Wizard metadata table and color mapper for Excel styling.
#'
#' The on-screen table is styled from the Tables submodule's current metadata, so
#' prefer that live source before falling back to the broader Data Wizard export
#' metadata accessors.
resolve_processed_data_coloring_inputs <- function(rv, datawizard_out, debug_log) {
  metadata <- NULL
  color_mapper <- NULL

  if (!is.null(datawizard_out) && is.list(datawizard_out)) {
    tables_out <- datawizard_out$tables_out
    if (is.list(tables_out)) {
      if (is.function(tables_out$create_content_color_mapping)) {
        color_mapper <- tables_out$create_content_color_mapping
      }

      for (metadata_accessor in c("current_metadata", "current_handson_metadata")) {
        if (!is.null(metadata)) break
        accessor <- tables_out[[metadata_accessor]]
        if (is.function(accessor)) {
          metadata <- tryCatch(accessor(), error = function(e) NULL)
          if (!is.null(metadata) && is.data.frame(metadata) && nrow(metadata) > 0) {
            debug_log(paste("Using Tables submodule", metadata_accessor, "for Processed_Data coloring"), level = 2)
            break
          }
          metadata <- NULL
        }
      }
    }

    for (metadata_accessor in c("working_metadata", "final_processed_metadata", "handson_metadata", "metadata_def", "data_def")) {
      if (!is.null(metadata)) break
      accessor <- datawizard_out[[metadata_accessor]]
      if (is.function(accessor)) {
        metadata <- tryCatch(accessor(), error = function(e) NULL)
        if (!is.null(metadata) && is.data.frame(metadata) && nrow(metadata) > 0) {
          debug_log(paste("Using Data Wizard", metadata_accessor, "for Processed_Data coloring"), level = 2)
          break
        }
        metadata <- NULL
      }
    }
  }

  if (is.null(metadata)) {
    metadata <- access_data_for_excel(rv, datawizard_out, "metadata", debug_log)
  }

  if (is.null(color_mapper)) {
    color_mapper <- tryCatch(get("create_content_color_mapping", mode = "function", inherits = TRUE),
                             error = function(e) NULL)
  }

  list(metadata = metadata, color_mapper = color_mapper)
}

#' Apply Data Wizard metadata-based column fills to the Processed_Data sheet.
#'
#' Mirrors the Data Wizard data table column coloring in Excel by resolving the
#' same Content/Options metadata through create_content_color_mapping().
apply_processed_data_metadata_coloring <- function(wb, sheet, processed_data, metadata, color_mapper, debug_log) {
  tryCatch({
    if (is.null(processed_data) || !is.data.frame(processed_data) || ncol(processed_data) == 0) {
      return(invisible(FALSE))
    }
    if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0) {
      debug_log("Processed_Data metadata coloring skipped: no metadata available", level = 2)
      return(invisible(FALSE))
    }
    if (!all(c("Column", "Content") %in% names(metadata))) {
      debug_log("Processed_Data metadata coloring skipped: metadata lacks Column/Content fields", level = 2)
      return(invisible(FALSE))
    }
    if (!"Options" %in% names(metadata)) {
      metadata$Options <- ""
    }
    if (!is.function(color_mapper)) {
      debug_log("Processed_Data metadata coloring skipped: create_content_color_mapping() is unavailable", level = 2)
      return(invisible(FALSE))
    }

    metadata <- metadata[metadata$Column %in% names(processed_data), , drop = FALSE]
    if (nrow(metadata) == 0) {
      debug_log("Processed_Data metadata coloring skipped: metadata columns do not match processed data", level = 2)
      return(invisible(FALSE))
    }

    color_mapping <- color_mapper(unique(metadata$Content), metadata)
    color_mapping <- color_mapping[names(color_mapping) %in% names(processed_data)]
    color_mapping <- color_mapping[!is.na(color_mapping) & nzchar(color_mapping)]
    if (length(color_mapping) == 0) {
      debug_log("Processed_Data metadata coloring skipped: no column color mapping could be resolved", level = 2)
      return(invisible(FALSE))
    }

    styled_rows <- seq_len(nrow(processed_data)) + 1L

    for (fill_color in unique(unname(color_mapping))) {
      styled_cols <- which(names(processed_data) %in% names(color_mapping)[color_mapping == fill_color])
      if (length(styled_cols) == 0) next
      fill_style <- openxlsx::createStyle(fgFill = fill_color)
      openxlsx::addStyle(
        wb, sheet, fill_style,
        rows = styled_rows, cols = styled_cols,
        gridExpand = TRUE, stack = TRUE
      )
    }

    debug_log(paste("Applied metadata-based coloring to", length(color_mapping), "Processed_Data columns"), level = 1)
    invisible(TRUE)
  }, error = function(e) {
    debug_log(paste("Processed_Data metadata coloring failed:", e$message), level = 1)
    invisible(FALSE)
  })
}

export_pipeline_run_datawizard_primary <- function(ctx) {
  with(ctx, {
    # ========================================
    # Sheet 1: Processed Data
    # ========================================
    debug_log("Creating Sheet 1: Processed Data", level = 1)

    coloring_inputs <- resolve_processed_data_coloring_inputs(rv, datawizard_out, debug_log)
    metadata_for_coloring <- coloring_inputs$metadata
    processed_data <- access_data_for_excel(rv, datawizard_out, "processed", debug_log)

    if (!is.null(processed_data)) {
      openxlsx::addWorksheet(wb, "Processed_Data")
      processed_data <- sanitize_for_excel(processed_data, "Processed_Data", debug_log)
      writeData_sanitized(wb, "Processed_Data", processed_data, startRow = 1, startCol = 1)
      if (ncol(processed_data) > 0) {
        openxlsx::addStyle(wb, "Processed_Data", headerStyle, rows = 1, cols = 1:ncol(processed_data))
        apply_processed_data_metadata_coloring(
          wb, "Processed_Data", processed_data, metadata_for_coloring,
          coloring_inputs$color_mapper, debug_log
        )
      }
      sheets_created <- sheets_created + 1
      debug_log(paste("Sheet 1 created with", nrow(processed_data), "rows and", ncol(processed_data), "columns"), level = 1)
    } else {
      debug_log("No processed data found - creating placeholder sheet", level = 1)
      openxlsx::addWorksheet(wb, "Processed_Data")
      placeholder_data <- data.frame(
        Info = "No processed data available",
        Note = "Data processing may not be complete",
        Suggestion = "Process data in Data Wizard first",
        stringsAsFactors = FALSE
      )
      writeData_sanitized(wb, "Processed_Data", placeholder_data)
      sheets_created <- sheets_created + 1
    }

    # ========================================
    # Sheet 2: Original Data
    # ========================================
    debug_log("Creating Sheet 2: Original Data", level = 1)

    original_data <- access_data_for_excel(rv, datawizard_out, "original", debug_log)

    if (!is.null(original_data)) {
      openxlsx::addWorksheet(wb, "Original_Data")
      original_data <- sanitize_for_excel(original_data, "Original_Data", debug_log)
      writeData_sanitized(wb, "Original_Data", original_data, startRow = 1, startCol = 1)
      if (ncol(original_data) > 0) {
        openxlsx::addStyle(wb, "Original_Data", headerStyle, rows = 1, cols = 1:ncol(original_data))
      }
      sheets_created <- sheets_created + 1
      debug_log(paste("Sheet 2 created with", nrow(original_data), "rows and", ncol(original_data), "columns"), level = 1)
    } else {
      debug_log("No original data found - creating placeholder sheet", level = 1)
      openxlsx::addWorksheet(wb, "Original_Data")
      placeholder_data <- data.frame(
        Info = "No original data available",
        Note = "Data may not have been loaded yet",
        Suggestion = "Load data in Data Wizard first",
        stringsAsFactors = FALSE
      )
      writeData_sanitized(wb, "Original_Data", placeholder_data)
      sheets_created <- sheets_created + 1
    }

    # ========================================
    # Sheet 3: Metadata (data_def)
    # ========================================
    debug_log("Creating Sheet 3: Metadata", level = 1)

    metadata <- metadata_for_coloring
    if (is.null(metadata)) {
      metadata <- access_data_for_excel(rv, datawizard_out, "metadata", debug_log)
    }

    if (!is.null(metadata)) {
      openxlsx::addWorksheet(wb, "Metadata")
      # Saved sessions may still carry fields that are not part of the public
      # Excel metadata schema. Keep private contrast lineage out of exports.
      if (is.data.frame(metadata)) {
        private_fields <- c("Custom", "ContrastId", "VariantId")
        metadata <- metadata[, !names(metadata) %in% private_fields, drop = FALSE]
      }
      metadata <- sanitize_for_excel(metadata, "Metadata", debug_log)
      writeData_sanitized(wb, "Metadata", metadata, startRow = 1, startCol = 1)
      if (ncol(metadata) > 0) {
        openxlsx::addStyle(wb, "Metadata", headerStyle, rows = 1, cols = 1:ncol(metadata))
      }
      sheets_created <- sheets_created + 1
      debug_log(paste("Sheet 3 created with", nrow(metadata), "rows and", ncol(metadata), "columns"), level = 1)
    } else {
      debug_log("No metadata found - creating placeholder sheet", level = 1)
      openxlsx::addWorksheet(wb, "Metadata")
      placeholder_data <- data.frame(
        Info = "No metadata available",
        Note = "Metadata may not have been defined yet",
        Suggestion = "Define column metadata in Data Wizard first",
        stringsAsFactors = FALSE
      )
      writeData_sanitized(wb, "Metadata", placeholder_data)
      sheets_created <- sheets_created + 1
    }

    # ========================================
    # Sheet 4: Abundance Plot Summary (STATISTICAL SUMMARY)
    # ========================================
    debug_log("Checking for Abundance plot data via direct access", level = 1)

    abundance_summary_data <- NULL
    if (!is.null(module_outputs) && !is.null(module_outputs$abundance_out)) {
      tryCatch({
        abundance_module <- module_outputs$abundance_out
        if (is.list(abundance_module) && "ggplot_object_abundanceTab" %in% names(abundance_module)) {
          plot_reactive <- abundance_module$ggplot_object_abundanceTab
          if (is.function(plot_reactive)) {
            plot_obj <- plot_reactive()
            if (!is.null(plot_obj) && inherits(plot_obj, "ggplot") && !is.null(plot_obj$data)) {
              plot_data <- plot_obj$data
              debug_log(paste("Found abundance plot data with", nrow(plot_data), "points"), level = 2)

              # Extract the Variable and Value columns (typical for abundance boxplots)
              if ("Variable" %in% names(plot_data) && "Value" %in% names(plot_data)) {

                # Calculate summary statistics for each variable (sample)
                summary_list <- list()
                unique_variables <- unique(plot_data$Variable)

                for (variable in unique_variables) {
                  variable_data <- plot_data$Value[plot_data$Variable == variable]
                  variable_data <- variable_data[is.finite(variable_data)]  # Remove NAs and infinite values

                  if (length(variable_data) > 0) {
                    # Calculate comprehensive boxplot statistics
                    q1 <- quantile(variable_data, 0.25, na.rm = TRUE)
                    median_val <- median(variable_data, na.rm = TRUE)
                    q3 <- quantile(variable_data, 0.75, na.rm = TRUE)
                    min_val <- min(variable_data, na.rm = TRUE)
                    max_val <- max(variable_data, na.rm = TRUE)
                    mean_val <- mean(variable_data, na.rm = TRUE)
                    iqr_val <- q3 - q1

                    # Outlier detection (using 1.5 * IQR rule)
                    lower_fence <- q1 - 1.5 * iqr_val
                    upper_fence <- q3 + 1.5 * iqr_val
                    outliers <- variable_data[variable_data < lower_fence | variable_data > upper_fence]

                    summary_list[[as.character(variable)]] <- data.frame(
                      Sample = as.character(variable),
                      Count = length(variable_data),
                      Mean = round(mean_val, 4),
                      Median = round(median_val, 4),
                      Q1 = round(q1, 4),
                      Q3 = round(q3, 4),
                      Min = round(min_val, 4),
                      Max = round(max_val, 4),
                      IQR = round(iqr_val, 4),
                      Outliers_Count = length(outliers),
                      Outliers_Percent = round((length(outliers) / length(variable_data)) * 100, 2),
                      stringsAsFactors = FALSE
                    )
                  }
                }

                if (length(summary_list) > 0) {
                  abundance_summary_data <- do.call(rbind, summary_list)
                  rownames(abundance_summary_data) <- NULL
                  debug_log(paste("Created abundance summary for", nrow(abundance_summary_data), "samples"), level = 1)
                }
              }
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Direct abundance access failed:", e$message), level = 2)
      })
    }

    if (!is.null(abundance_summary_data) && nrow(abundance_summary_data) > 0) {
      tryCatch({
        openxlsx::addWorksheet(wb, "Abundance_Summary")

        # Add descriptive header
        writeData_sanitized(wb, "Abundance_Summary",
                            "Abundance Plot Statistical Summary",
                            startRow = 1, startCol = 1)
        writeData_sanitized(wb, "Abundance_Summary",
                            paste("Boxplot statistics for", nrow(abundance_summary_data), "samples"),
                            startRow = 2, startCol = 1)

        # Write summary data starting from row 4
        writeData_sanitized(wb, "Abundance_Summary", abundance_summary_data, startRow = 4, startCol = 1)

        # Style the headers
        if (ncol(abundance_summary_data) > 0) {
          openxlsx::addStyle(wb, "Abundance_Summary", headerStyle, rows = 4, cols = 1:ncol(abundance_summary_data))
        }

        sheets_created <- sheets_created + 1
        debug_log("Abundance Summary sheet created successfully", level = 1)
      }, error = function(e) {
        debug_log(paste("Error creating Abundance Summary sheet:", e$message), level = 1)
      })
    } else {
      debug_log("No Abundance plot data available for Excel export", level = 2)
    }

    # ========================================
    # Sheet 5: Sample IDs Summary (UNIFIED SHEET)
    # ========================================
    debug_log("Processing Sample IDs plot data with enhanced debugging", level = 1)

    sampleid_summary_data <- NULL
    sampleid_summary_numeric <- NULL

    if (!is.null(module_outputs) && !is.null(module_outputs$sampleid_out)) {
      tryCatch({
        sampleid_module <- module_outputs$sampleid_out
        debug_log(paste("SampleIDs module structure:", paste(names(sampleid_module), collapse = ", ")), level = 1)

        if (is.list(sampleid_module) && "ggplot_object_SampleIDTab" %in% names(sampleid_module)) {
          plot_reactive <- sampleid_module$ggplot_object_SampleIDTab
          if (is.function(plot_reactive)) {
            plot_obj <- plot_reactive()

            if (!is.null(plot_obj) && inherits(plot_obj, "ggplot") && !is.null(plot_obj$data)) {
              plot_data <- plot_obj$data
              debug_log(paste("SampleIDs raw data dimensions:", nrow(plot_data), "rows x", ncol(plot_data), "cols"), level = 1)
              debug_log(paste("SampleIDs column names:", paste(names(plot_data), collapse = ", ")), level = 1)

              # Determine the correct column names for Variable and Value
              possible_variable_cols <- c("Variable", "Sample", "Group", "x", "variable")
              possible_value_cols <- c("value", "Value", "y", "ID_Value", "Sample_ID")

              variable_col <- intersect(possible_variable_cols, names(plot_data))
              value_col <- intersect(possible_value_cols, names(plot_data))

              if (length(variable_col) > 0 && length(value_col) > 0) {
                variable_col <- variable_col[1]
                value_col <- value_col[1]

                debug_log(paste("Using columns - Variable:", variable_col, "Value:", value_col), level = 1)

                # Extract the relevant data
                sample_values <- plot_data[[value_col]]
                sample_variables <- plot_data[[variable_col]]

                # Determine if values are character/categorical or numeric
                is_numeric_data <- is.numeric(sample_values) ||
                  (is.character(sample_values) &&
                     all(grepl("^[0-9\\.\\-]+$", sample_values[!is.na(sample_values) & nzchar(sample_values)])))

                debug_log(paste("Data type detected:", ifelse(is_numeric_data, "Numeric", "Character/Categorical")), level = 1)

                if (!is_numeric_data) {
                  # CHARACTER DATA: Create contingency table for stacked bars
                  debug_log("Processing character/categorical Sample IDs data for stacked bar counts", level = 1)

                  # Remove NA values and empty strings
                  valid_data <- !is.na(sample_values) & !is.na(sample_variables) &
                    nzchar(as.character(sample_values)) & nzchar(as.character(sample_variables))

                  if (sum(valid_data) > 0) {
                    clean_values <- sample_values[valid_data]
                    clean_variables <- sample_variables[valid_data]

                    # Create contingency table: Character values as rows, Samples as columns
                    contingency_table <- table(clean_values, clean_variables)

                    if (nrow(contingency_table) > 0 && ncol(contingency_table) > 0) {
                      # Convert to data frame with proper structure
                      sampleid_summary_data <- as.data.frame.matrix(contingency_table)
                      sampleid_summary_data$Character_Value <- rownames(contingency_table)

                      # Reorder columns: Character_Value first, then samples
                      sampleid_summary_data <- sampleid_summary_data[, c("Character_Value", setdiff(names(sampleid_summary_data), "Character_Value")), drop = FALSE]

                      # Add row totals
                      sampleid_summary_data$Total_Count <- rowSums(sampleid_summary_data[, -1, drop = FALSE])

                      # Add TOTAL ROW
                      sample_columns <- setdiff(names(sampleid_summary_data), c("Character_Value", "Total_Count"))
                      total_row <- data.frame(
                        Character_Value = "TOTAL",
                        stringsAsFactors = FALSE
                      )

                      # Calculate column totals
                      for (col in sample_columns) {
                        total_row[[col]] <- sum(sampleid_summary_data[[col]], na.rm = TRUE)
                      }
                      total_row$Total_Count <- sum(sampleid_summary_data$Total_Count, na.rm = TRUE)

                      # Append total row
                      sampleid_summary_data <- rbind(sampleid_summary_data, total_row)

                      debug_log(paste("Created character contingency table with total row:", nrow(sampleid_summary_data), "rows x", ncol(sampleid_summary_data), "cols"), level = 1)
                    }
                  }

                } else {
                  # NUMERIC DATA: Create summary statistics per sample
                  debug_log("Processing numeric Sample IDs data", level = 1)

                  # Convert to numeric and remove invalid values
                  numeric_values <- suppressWarnings(as.numeric(sample_values))
                  valid_numeric <- is.finite(numeric_values) & !is.na(sample_variables) & nzchar(as.character(sample_variables))

                  if (sum(valid_numeric) > 0) {
                    clean_numeric_values <- numeric_values[valid_numeric]
                    clean_variables <- sample_variables[valid_numeric]

                    # Calculate summary statistics for each sample
                    summary_list <- list()
                    unique_samples <- unique(clean_variables)

                    for (sample in unique_samples) {
                      sample_data <- clean_numeric_values[clean_variables == sample]

                      if (length(sample_data) > 0) {
                        summary_list[[as.character(sample)]] <- data.frame(
                          Sample = as.character(sample),
                          Count = length(sample_data),
                          Mean = round(mean(sample_data, na.rm = TRUE), 4),
                          Median = round(median(sample_data, na.rm = TRUE), 4),
                          Min = round(min(sample_data, na.rm = TRUE), 4),
                          Max = round(max(sample_data, na.rm = TRUE), 4),
                          SD = round(sd(sample_data, na.rm = TRUE), 4),
                          Unique_Values = length(unique(sample_data)),
                          stringsAsFactors = FALSE
                        )
                      }
                    }

                    if (length(summary_list) > 0) {
                      sampleid_summary_numeric <- do.call(rbind, summary_list)
                      rownames(sampleid_summary_numeric) <- NULL
                      debug_log(paste("Created numeric summary for", nrow(sampleid_summary_numeric), "samples"), level = 1)
                    }
                  }
                }
              }
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Error processing SampleIDs data:", e$message), level = 1)
      })
    }

    # ========================================
    # Create Unified SampleIDs Sheet
    # ========================================
    if (!is.null(sampleid_summary_data) || !is.null(sampleid_summary_numeric)) {
      tryCatch({
        openxlsx::addWorksheet(wb, "SampleIDs")
        current_row <- 1

        # Main sheet header
        writeData_sanitized(wb, "SampleIDs",
                            "Sample IDs Analysis Summary",
                            startRow = current_row, startCol = 1)
        openxlsx::addStyle(wb, "SampleIDs", headerStyle, rows = current_row, cols = 1)
        current_row <- current_row + 3

        # ========================================
        # CHARACTER DATA SECTION (if available)
        # ========================================
        if (!is.null(sampleid_summary_data) && nrow(sampleid_summary_data) > 0) {
          # Section header
          writeData_sanitized(wb, "SampleIDs",
                              "CHARACTER DATA - Stacked Bar Counts",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 1

          # Description
          writeData_sanitized(wb, "SampleIDs",
                              paste("Protein counts per character value across", ncol(sampleid_summary_data)-2, "samples"),
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 1

          writeData_sanitized(wb, "SampleIDs",
                              "Each number represents how many proteins have that character value in each sample",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Write character data
          writeData_sanitized(wb, "SampleIDs", sampleid_summary_data, startRow = current_row, startCol = 1)

          # Style the headers
          if (ncol(sampleid_summary_data) > 0) {
            openxlsx::addStyle(wb, "SampleIDs", headerStyle, rows = current_row, cols = 1:ncol(sampleid_summary_data))
          }

          # Highlight the total row
          total_row_number <- current_row + nrow(sampleid_summary_data)
          if (ncol(sampleid_summary_data) > 0) {
            total_style <- openxlsx::createStyle(fgFill = "#E0E0E0", textDecoration = "bold", border = "TopBottomLeftRight")
            openxlsx::addStyle(wb, "SampleIDs", total_style, rows = total_row_number, cols = 1:ncol(sampleid_summary_data))
          }

          current_row <- current_row + nrow(sampleid_summary_data) + 3
          debug_log("Character data section added to unified SampleIDs sheet", level = 1)
        }

        # ========================================
        # NUMERIC DATA SECTION (if available)
        # ========================================
        if (!is.null(sampleid_summary_numeric) && nrow(sampleid_summary_numeric) > 0) {
          # Section header
          writeData_sanitized(wb, "SampleIDs",
                              "NUMERIC DATA - Statistical Summary",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 1

          # Description
          writeData_sanitized(wb, "SampleIDs",
                              paste("Statistical summary for", nrow(sampleid_summary_numeric), "samples"),
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 1

          writeData_sanitized(wb, "SampleIDs",
                              "Each row shows statistical measures for numeric Sample ID values per sample",
                              startRow = current_row, startCol = 1)
          current_row <- current_row + 2

          # Write numeric data
          writeData_sanitized(wb, "SampleIDs", sampleid_summary_numeric, startRow = current_row, startCol = 1)

          # Style the headers
          if (ncol(sampleid_summary_numeric) > 0) {
            openxlsx::addStyle(wb, "SampleIDs", headerStyle, rows = current_row, cols = 1:ncol(sampleid_summary_numeric))
          }

          debug_log("Numeric data section added to unified SampleIDs sheet", level = 1)
        }

        sheets_created <- sheets_created + 1
        debug_log("Unified SampleIDs sheet created successfully", level = 1)

      }, error = function(e) {
        debug_log(paste("Error creating unified SampleIDs sheet:", e$message), level = 1)
      })
    } else {
      debug_log("No SampleIDs data available for Excel export", level = 1)
    }

    # ========================================
    # Debug Summary
    # ========================================
    if (is.null(sampleid_summary_data) && is.null(sampleid_summary_numeric)) {
      debug_log("No Sample IDs data was successfully processed for Excel export", level = 1)
    } else {
      debug_log(paste("Sample IDs export completed - Character:", !is.null(sampleid_summary_data),
                      "Numeric:", !is.null(sampleid_summary_numeric)), level = 1)
    }

    # ========================================
    # Sheet 6: PCA Analysis (DYNAMIC SPACING - Exact Fit)
    # ========================================
    debug_log("Checking for ALL PCA/UMAP results via robust access", level = 1)

    pca_data_available <- FALSE

    if (!is.null(module_outputs) && !is.null(module_outputs$pca_out)) {
      tryCatch({
        pca_module <- module_outputs$pca_out
        debug_log("Enhanced PCA module interface available", level = 1)

        if (is.list(pca_module)) {
          # Get all available results (using the robust method from before)
          all_results <- list()

          if ("get_all_results" %in% names(pca_module) && is.function(pca_module$get_all_results)) {
            all_results <- pca_module$get_all_results()
          }

          # [Include the fallback methods from the previous solution]
          if (length(all_results) == 0) {
            # Fallback methods as before...
          }

          debug_log(paste("FINAL: Total results found:", length(all_results)), level = 1)

          if (length(all_results) > 0) {
            # Create unified PCA sheet
            openxlsx::addWorksheet(wb, "PCA_Analysis")

            # Color schemes
            color_schemes <- list(
              sample_pca = "#E8F4FD", protein_pca = "#E8F5E8",
              sample_umap = "#FDF2E8", protein_umap = "#F0E8FD",
              samples_pca = "#E8F4FD", proteins_pca = "#E8F5E8",
              samples_umap = "#FDF2E8", proteins_umap = "#F0E8FD"
            )

            # Create styles
            styles <- list()
            for (type in names(color_schemes)) {
              styles[[type]] <- openxlsx::createStyle(
                fgFill = color_schemes[[type]],
                textDecoration = "bold",
                border = "TopBottomLeftRight"
              )
            }

            # ========================================
            # STEP 1: Calculate actual width for each analysis
            # ========================================
            analysis_widths <- list()
            analysis_names <- names(all_results)

            for (analysis_type in analysis_names) {
              results <- all_results[[analysis_type]]
              if (!is.null(results)) {
                max_width <- 5  # Minimum for headers

                # Check coordinates width
                if (!is.null(results$coordinates)) {
                  coords_width <- ncol(results$coordinates) + 1  # +1 for identifier column
                  max_width <- max(max_width, coords_width)
                  debug_log(paste(analysis_type, "- Coordinates width:", coords_width), level = 2)
                }

                # Check variance explained width (PCA only)
                if (results$method == "pca" && !is.null(results$var_explained)) {
                  variance_width <- 3  # Component, Variance_Explained, Cumulative_Variance
                  max_width <- max(max_width, variance_width)
                  debug_log(paste(analysis_type, "- Variance width:", variance_width), level = 2)
                }

                # Check loadings width (PCA only)
                if (results$method == "pca" && !is.null(results$loadings)) {
                  loadings_width <- ncol(results$loadings) + 1  # +1 for identifier column
                  max_width <- max(max_width, loadings_width)
                  debug_log(paste(analysis_type, "- Loadings width:", loadings_width), level = 2)
                }

                analysis_widths[[analysis_type]] <- max_width
                debug_log(paste("CALCULATED:", analysis_type, "needs", max_width, "columns"), level = 1)
              }
            }

            # ========================================
            # STEP 2: Calculate positions with exact spacing
            # ========================================
            col_spacing <- 2  # Exactly 2 columns between analyses
            start_row <- 1
            start_col <- 1

            # Calculate positions
            analysis_positions <- list()
            current_col <- start_col

            for (i in seq_along(analysis_names)) {
              analysis_type <- analysis_names[i]
              analysis_positions[[analysis_type]] <- current_col

              if (i < length(analysis_names)) {
                # Move to next position: current + width + spacing
                current_col <- current_col + analysis_widths[[analysis_type]] + col_spacing
              }

              debug_log(paste("POSITION:", analysis_type, "starts at column", analysis_positions[[analysis_type]]), level = 1)
            }

            # ========================================
            # STEP 3: Write data using calculated positions
            # ========================================

            # Main header
            writeData_sanitized(wb, "PCA_Analysis",
                                "Comprehensive Dimension Reduction Analysis",
                                startRow = start_row, startCol = start_col)
            openxlsx::addStyle(wb, "PCA_Analysis", headerStyle, rows = start_row, cols = start_col)

            current_row_start <- start_row + 3

            for (analysis_type in analysis_names) {
              results <- all_results[[analysis_type]]
              actual_col <- analysis_positions[[analysis_type]]

              if (!is.null(results)) {
                debug_log(paste("WRITING:", analysis_type, "at column", actual_col), level = 2)

                # Determine analysis details
                method <- results$method %||% "unknown"
                target <- results$comparison_target %||% "unknown"
                title <- paste(toupper(method), "-", toupper(target))

                # Get style
                style_key <- paste(target, method, sep = "_")
                current_style <- styles[[style_key]] %||% styles[[analysis_type]] %||% headerStyle

                current_write_row <- current_row_start

                # Section header
                writeData_sanitized(wb, "PCA_Analysis", title,
                                    startRow = current_write_row, startCol = actual_col)
                openxlsx::addStyle(wb, "PCA_Analysis", current_style,
                                   rows = current_write_row, cols = actual_col:(actual_col + 4))
                current_write_row <- current_write_row + 2

                # 1. COORDINATES
                if (!is.null(results$coordinates)) {
                  coords_df <- as.data.frame(results$coordinates)

                  # Add identifier column
                  if (target == "samples" || grepl("sample", analysis_type, ignore.case = TRUE)) {
                    coords_df$Sample <- rownames(results$coordinates) %||%
                      paste0("Sample_", seq_len(nrow(coords_df)))
                    coords_df <- coords_df[, c("Sample", setdiff(names(coords_df), "Sample")), drop = FALSE]
                  } else {
                    coords_df$Protein <- rownames(results$coordinates) %||%
                      paste0("Protein_", seq_len(nrow(coords_df)))
                    coords_df <- coords_df[, c("Protein", setdiff(names(coords_df), "Protein")), drop = FALSE]
                  }

                  # Write coordinates
                  writeData_sanitized(wb, "PCA_Analysis",
                                      paste("Coordinates (", nrow(coords_df), "points)"),
                                      startRow = current_write_row, startCol = actual_col)
                  current_write_row <- current_write_row + 1

                  writeData_sanitized(wb, "PCA_Analysis", coords_df,
                                      startRow = current_write_row, startCol = actual_col)
                  if (ncol(coords_df) > 0) {
                    openxlsx::addStyle(wb, "PCA_Analysis", current_style,
                                       rows = current_write_row,
                                       cols = actual_col:(actual_col + ncol(coords_df) - 1))
                  }
                  current_write_row <- current_write_row + nrow(coords_df) + 2
                  pca_data_available <- TRUE
                }

                # 2. VARIANCE EXPLAINED (PCA only)
                if (method == "pca" && !is.null(results$var_explained)) {
                  scree_data <- data.frame(
                    Component = paste0("PC", 1:length(results$var_explained)),
                    Variance_Explained = round(results$var_explained, 4),
                    Cumulative_Variance = round(cumsum(results$var_explained), 4)
                  )

                  writeData_sanitized(wb, "PCA_Analysis", "Variance Explained",
                                      startRow = current_write_row, startCol = actual_col)
                  current_write_row <- current_write_row + 1

                  writeData_sanitized(wb, "PCA_Analysis", scree_data,
                                      startRow = current_write_row, startCol = actual_col)
                  openxlsx::addStyle(wb, "PCA_Analysis", current_style,
                                     rows = current_write_row, cols = actual_col:(actual_col + 2))
                  current_write_row <- current_write_row + nrow(scree_data) + 2
                  pca_data_available <- TRUE
                }

                # 3. LOADINGS (PCA only)
                if (method == "pca" && !is.null(results$loadings)) {
                  loadings_df <- as.data.frame(results$loadings)

                  if (target == "samples" || grepl("sample", analysis_type, ignore.case = TRUE)) {
                    loadings_df$Feature <- rownames(loadings_df) %||%
                      paste0("Feature_", seq_len(nrow(loadings_df)))
                    loadings_df <- loadings_df[, c("Feature", setdiff(names(loadings_df), "Feature")), drop = FALSE]
                  } else {
                    loadings_df$Sample <- rownames(loadings_df) %||%
                      paste0("Sample_", seq_len(nrow(loadings_df)))
                    loadings_df <- loadings_df[, c("Sample", setdiff(names(loadings_df), "Sample")), drop = FALSE]
                  }

                  writeData_sanitized(wb, "PCA_Analysis", "Loadings",
                                      startRow = current_write_row, startCol = actual_col)
                  current_write_row <- current_write_row + 1

                  writeData_sanitized(wb, "PCA_Analysis", loadings_df,
                                      startRow = current_write_row, startCol = actual_col)
                  if (ncol(loadings_df) > 0) {
                    openxlsx::addStyle(wb, "PCA_Analysis", current_style,
                                       rows = current_write_row,
                                       cols = actual_col:(actual_col + ncol(loadings_df) - 1))
                  }
                  pca_data_available <- TRUE
                }
              }
            }

            if (pca_data_available) {
              sheets_created <- sheets_created + 1
              debug_log("Dynamic-spacing PCA/UMAP Analysis sheet created with exact fit", level = 1)
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Error in dynamic-spacing PCA analysis:", e$message), level = 2)
      })
    }

  })
  invisible(NULL)
}
