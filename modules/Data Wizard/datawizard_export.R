# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_export.R
# Purpose:
#   Provide Data Wizard export and configuration serialization/import helpers,
#   including Excel workbook assembly and export-readiness checks.
#
# Architectural Role:
#   export
#
# Responsibilities:
#   - Build result workbooks from canonical data and metadata state.
#   - Expose configuration export/import helper factories for integrated modules.
#   - Provide defensive export validation and user-facing error signaling.
#
# Non-Responsibilities (Must NOT be here):
#   - Drive top-level orchestration or lifecycle observer registration.
#   - Implement processing logic that belongs to out-of-scope feature modules.
#
# Allowed Dependencies:
#   - In-scope utility layer (`datawizard_utils.R`).
#   - Caller-provided core/module references and existing workbook dependencies.
#
# Interaction Boundaries:
#   - Inputs:
#     Loader outputs, core state handles, module output lists, ui config values.
#   - Outputs:
#     Export helper function lists for Excel generation and config data transfer.
#   - Out-of-Scope Integrations:
#     Consumes black-box module outputs only through existing exposed getters/setters.
#
# Stability Guarantees:
#   - Preserve exported helper names and expected return structures.
#   - Preserve workbook content intent and defensive error behavior.
#   - Keep backward compatibility for downstream download/config workflows.
# ============================================================================
# modules/Data Wizard/datawizard_export.R
# Data Wizard Export Operations - Excel Export and Configuration Management

# Source required utilities
source("modules/Data Wizard/datawizard_utils.R", local = TRUE)



sanitize_for_excel_export_dw <- function(df, sheet_name = "", max_chars = 32767L) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) return(df)

  text_cols <- which(vapply(df, function(col) is.character(col) || is.factor(col) || is.list(col), logical(1)))
  if (length(text_cols) == 0L) return(df)

  trimmed_cells <- 0L
  truncated_cells <- 0L
  for (j in text_cols) {
    col <- df[[j]]
    x <- if (is.list(col)) {
      vapply(col, function(cell) {
        if (length(cell) == 0L || all(is.na(cell))) return(NA_character_)
        paste(as.character(cell), collapse = " | ")
      }, character(1), USE.NAMES = FALSE)
    } else {
      as.character(col)
    }

    before_trim <- x
    x <- trimws(x)
    trimmed_cells <- trimmed_cells + sum(!is.na(before_trim) & !is.na(x) & before_trim != x)

    ok <- !is.na(x)
    if (!any(ok)) next

    too_long <- ok & nchar(x, type = "chars", allowNA = FALSE, keepNA = FALSE) > max_chars
    if (any(too_long)) {
      x[too_long] <- substr(x[too_long], 1L, max_chars)
      truncated_cells <- truncated_cells + sum(too_long)
    }
    df[[j]] <- x
  }

  if (trimmed_cells > 0L) {
    debug_log(paste0("Excel export sanitizer: trimmed whitespace in ", trimmed_cells,
                     " over-limit cells for sheet '", sheet_name, "'."), level = 2)
  }
  if (truncated_cells > 0L) {
    debug_log(paste0("Excel export sanitizer: truncated ", truncated_cells,
                     " cells to ", max_chars, " chars for sheet '", sheet_name, "'."), level = 1)
  }

  df
}

writeData_sanitized_dw <- function(wb, sheet, x, startRow = 1, startCol = 1) {
  withCallingHandlers(
    writeData(wb, sheet, x, startRow = startRow, startCol = startCol),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("32767|exeed the limit of 32767|exceed the limit of 32767", msg, ignore.case = TRUE)) {
        debug_log(paste0("Suppressed openxlsx 32767-char warning for sheet '", sheet, "' after sanitization."), level = 2)
        invokeRestart("muffleWarning")
      }
    }
  )
}
#' Create Excel export functionality
#' @param loader_out file loader module output
#' @param core_values core reactive values
#' @param modification_functions modification tracking functions
#' @return list of Excel export functions
create_excel_export_functions <- function(loader_out, core_values, modification_functions, modules_list = NULL) {

  list(
    create_results_excel = function() {
      tryCatch({
        debug_log("Creating results Excel workbook", level = 1)

        read_export_reactive <- function(value, default = NULL) {
          tryCatch({
            if (is.function(value)) value() else if (!is.null(value)) value else default
          }, error = function(e) default)
        }

        dataset_dimensions_label <- function(data) {
          if (is.null(data) || !is.data.frame(data)) return("Not available")
          paste(nrow(data), "x", ncol(data))
        }

        dataset_revision_label <- function(resolved_dataset) {
          revision <- resolved_dataset$revision
          if (is.null(revision) || length(revision) == 0L || is.na(revision)) "legacy/unknown" else as.character(revision)
        }

        sheet_sources <- list()
        record_sheet_source <- function(sheet_name, resolved_dataset) {
          sheet_sources[[sheet_name]] <<- data.frame(
            Sheet = sheet_name,
            Dataset_Role = resolved_dataset$resolved_role,
            Requested_Role = resolved_dataset$role,
            Revision = dataset_revision_label(resolved_dataset),
            Source = resolved_dataset$source,
            Dimensions = dataset_dimensions_label(resolved_dataset$data),
            stringsAsFactors = FALSE
          )
        }

        # Collect all required data
        loader_primary <- validate_reactive_value(loader_out$primary, "primary_for_excel")
        legacy_primary_fallback <- if (!is.null(loader_primary)) {
          loader_primary
        } else {
          read_export_reactive(core_values$primary_data_raw, NULL)
        }
        loader_additional <- validate_reactive_value(loader_out$additional, "additional_for_excel")
        primary_original_resolved <- resolve_datawizard_dataset(
          role = "primary_original",
          core_values = core_values,
          fallback_roles = c("primary_raw", "raw"),
          fallback_data = legacy_primary_fallback,
          fallback_label = "loader primary/current raw fallback"
        )
        additional_resolved <- resolve_datawizard_dataset(
          role = "secondary_working",
          core_values = core_values,
          fallback_data = loader_additional,
          fallback_label = "loader additional fallback"
        )
        modified_role <- if (isTRUE(read_export_reactive(core_values$filter_applied, FALSE))) "primary_filtered" else "primary_working"
        modified_resolved <- resolve_datawizard_dataset(
          role = modified_role,
          core_values = core_values,
          fallback_data = if (identical(modified_role, "primary_filtered")) {
            read_export_reactive(core_values$filtered_data, NULL)
          } else {
            read_export_reactive(core_values$primary_data_raw, NULL)
          },
          fallback_label = "current primary fallback"
        )
        export_final <- isTRUE(read_export_reactive(core_values$apply_triggered, FALSE))
        final_resolved <- if (export_final) {
          resolve_datawizard_dataset(
            role = "primary_final",
            core_values = core_values,
            fallback_data = read_export_reactive(core_values$final_processed_data, NULL),
            fallback_label = "final processed data fallback"
          )
        } else {
          NULL
        }
        metadata_role <- if (export_final) "metadata_final" else "metadata_working"
        metadata_resolved <- resolve_datawizard_dataset(
          role = metadata_role,
          core_values = core_values,
          fallback_data = if (export_final) {
            read_export_reactive(core_values$final_processed_metadata, NULL)
          } else {
            read_export_reactive(core_values$handson_metadata, NULL)
          },
          fallback_label = "current metadata fallback"
        )

        primary_raw <- primary_original_resolved$data
        additional_data_df <- additional_resolved$data
        modified_data <- modified_resolved$data
        metadata_def <- datawizard_drop_deprecated_metadata_columns(metadata_resolved$data)

        if (is.null(modified_data)) {
          modified_data <- primary_raw
          modified_resolved$data <- modified_data
          modified_resolved$resolved_role <- primary_original_resolved$resolved_role
          modified_resolved$revision <- primary_original_resolved$revision
          modified_resolved$source <- "primary original fallback"
        }

        # Validate data
        if (is.null(primary_raw) || nrow(primary_raw) == 0) {
          stop("No primary data available for export")
        }

        if (is.null(metadata_def) || nrow(metadata_def) == 0) {
          stop("No metadata defined for export")
        }

        debug_log(paste("Excel export data validation passed -",
                        "Primary:", nrow(primary_raw), "x", ncol(primary_raw),
                        "Modified:", nrow(modified_data), "x", ncol(modified_data)), level = 2)

        # Create workbook
        wb <- createWorkbook()

        # Sheet 1: Primary Data (Original)
        addWorksheet(wb, "Primary_Data_Original")
        primary_raw <- sanitize_for_excel_export_dw(primary_raw, "Primary_Data_Original")
        writeData_sanitized_dw(wb, "Primary_Data_Original", primary_raw, startRow = 1, startCol = 1)
        primary_original_resolved$data <- primary_raw
        record_sheet_source("Primary_Data_Original", primary_original_resolved)

        # Header formatting
        headerStyle <- createStyle(
          fontSize = 12,
          fontColour = "white",
          fgFill = "#4F81BD",
          textDecoration = "bold",
          border = "TopBottomLeftRight"
        )
        addStyle(wb, "Primary_Data_Original", headerStyle, rows = 1, cols = 1:ncol(primary_raw))

        # Sheet 2: Additional Data (if available)
        sheet_count <- 1
        if (!is.null(additional_data_df) && is.data.frame(additional_data_df) && nrow(additional_data_df) > 0) {
          addWorksheet(wb, "Additional_Data")
          additional_data_df <- sanitize_for_excel_export_dw(additional_data_df, "Additional_Data")
          writeData_sanitized_dw(wb, "Additional_Data", additional_data_df, startRow = 1, startCol = 1)
          additional_resolved$data <- additional_data_df
          record_sheet_source("Additional_Data", additional_resolved)
          addStyle(wb, "Additional_Data", headerStyle, rows = 1, cols = 1:ncol(additional_data_df))
          sheet_count <- sheet_count + 1
          debug_log(paste("Additional data added:", nrow(additional_data_df), "x", ncol(additional_data_df)), level = 2)
        }

        # Sheet 3: Modified Data
        data_status <- "Processed_Data"
        status_details <- character()

        if (core_values$filter_applied()) status_details <- c(status_details, "Filtered")

        if (!is.null(modified_data) && !identical(modified_data, primary_raw)) {
          if (any(grepl("^Imputed ", names(modified_data)))) {
            status_details <- c(status_details, "Imputed")
          }
          if (any(grepl("^Batch Corrected ", names(modified_data)))) {
            status_details <- c(status_details, "BatchCorrected")
          }
          if (any(grepl("^Ratio_|_Abundance Ratio", names(modified_data)))) {
            status_details <- c(status_details, "Ratios")
          }
        }

        if (length(status_details) > 0) {
          data_status <- paste0("Data_", paste(status_details, collapse = "_"))
        }

        addWorksheet(wb, data_status)
        modified_data <- sanitize_for_excel_export_dw(modified_data, data_status)
        writeData_sanitized_dw(wb, data_status, modified_data, startRow = 1, startCol = 1)
        modified_resolved$data <- modified_data
        record_sheet_source(data_status, modified_resolved)

        modifiedStyle <- createStyle(
          fontSize = 12,
          fontColour = "white",
          fgFill = "#70AD47",
          textDecoration = "bold",
          border = "TopBottomLeftRight"
        )
        addStyle(wb, data_status, modifiedStyle, rows = 1, cols = 1:ncol(modified_data))

        # Optional final processed primary data when the apply pipeline has run.
        if (export_final && !is.null(final_resolved$data) && is.data.frame(final_resolved$data) && nrow(final_resolved$data) > 0) {
          addWorksheet(wb, "Final_Data")
          final_data <- sanitize_for_excel_export_dw(final_resolved$data, "Final_Data")
          writeData_sanitized_dw(wb, "Final_Data", final_data, startRow = 1, startCol = 1)
          final_resolved$data <- final_data
          record_sheet_source("Final_Data", final_resolved)
          addStyle(wb, "Final_Data", modifiedStyle, rows = 1, cols = 1:ncol(final_data))
        }

        # Sheet 4: Metadata
        addWorksheet(wb, "Metadata_Definition")
        metadata_def <- sanitize_for_excel_export_dw(metadata_def, "Metadata_Definition")
        writeData_sanitized_dw(wb, "Metadata_Definition", metadata_def, startRow = 1, startCol = 1)
        metadata_resolved$data <- metadata_def
        record_sheet_source("Metadata_Definition", metadata_resolved)

        metadataStyle <- createStyle(
          fontSize = 12,
          fontColour = "white",
          fgFill = "#E7E6E6",
          textDecoration = "bold",
          border = "TopBottomLeftRight"
        )
        addStyle(wb, "Metadata_Definition", metadataStyle, rows = 1, cols = 1:ncol(metadata_def))

        # Optional module sheet: Venn / UpSet intersections
        if (!is.null(modules_list$venn_out) && is.function(modules_list$venn_out$get_venn_export_workbook)) {
          venn_wb <- tryCatch(modules_list$venn_out$get_venn_export_workbook(), error = function(e) NULL)
          if (!is.null(venn_wb)) {
            temp_venn <- tempfile(fileext = ".xlsx")
            openxlsx::saveWorkbook(venn_wb, temp_venn, overwrite = TRUE)
            venn_df <- tryCatch(openxlsx::readWorkbook(temp_venn, sheet = 1), error = function(e) NULL)
            unlink(temp_venn)
            if (!is.null(venn_df) && is.data.frame(venn_df)) {
              addWorksheet(wb, "Venn_UpSet_Results")
              venn_df <- sanitize_for_excel_export_dw(venn_df, "Venn_UpSet_Results")
              writeData_sanitized_dw(wb, "Venn_UpSet_Results", venn_df, startRow = 1, startCol = 1)
              addStyle(wb, "Venn_UpSet_Results", metadataStyle, rows = 1, cols = 1:ncol(venn_df))
              debug_log("Added Venn/UpSet results sheet to overall export", level = 2)
            }
          }
        }

        # Sheet 5: Export Information
        addWorksheet(wb, "Export_Info")

        info_data <- data.frame(
          Property = c(
            "Export Date",
            "Export Time",
            "Primary Data Dimensions (Original)",
            "Modified Data Dimensions",
            "Metadata Rows",
            "Data Status",
            "Filter Applied",
            "Processing Details"
          ),
          Value = c(
            as.character(Sys.Date()),
            format(Sys.time(), "%H:%M:%S"),
            paste(nrow(primary_raw), "x", ncol(primary_raw)),
            paste(nrow(modified_data), "x", ncol(modified_data)),
            as.character(nrow(metadata_def)),
            ifelse(length(status_details) > 0, paste(status_details, collapse = ", "), "Raw Data"),
            as.character(core_values$filter_applied()),
            ifelse(length(status_details) > 0,
                   paste("Data has been processed with:", paste(status_details, collapse = ", ")),
                   "No processing applied")
          ),
          stringsAsFactors = FALSE
        )

        sheet_source_data <- if (length(sheet_sources) > 0L) {
          do.call(rbind, sheet_sources)
        } else {
          data.frame(
            Sheet = character(),
            Dataset_Role = character(),
            Requested_Role = character(),
            Revision = character(),
            Source = character(),
            Dimensions = character(),
            stringsAsFactors = FALSE
          )
        }
        sheet_source_info <- data.frame(
          Property = paste0("Sheet Source - ", sheet_source_data$Sheet),
          Value = paste0(
            sheet_source_data$Dataset_Role,
            " (revision ", sheet_source_data$Revision,
            ", source ", sheet_source_data$Source,
            ", dimensions ", sheet_source_data$Dimensions,
            ")"
          ),
          stringsAsFactors = FALSE
        )
        info_data <- rbind(info_data, sheet_source_info)

        info_data <- sanitize_for_excel_export_dw(info_data, "Export_Info")
        writeData_sanitized_dw(wb, "Export_Info", info_data, startRow = 1, startCol = 1)

        infoStyle <- createStyle(
          fontSize = 12,
          fontColour = "white",
          fgFill = "#9BBDF0",
          textDecoration = "bold",
          border = "TopBottomLeftRight"
        )
        addStyle(wb, "Export_Info", infoStyle, rows = 1, cols = 1:2)

        # Auto-adjust column widths
        sheet_names <- names(wb)
        for (sheet in sheet_names) {
          setColWidths(wb, sheet, cols = 1:20, widths = "auto")
        }

        debug_log(paste("Excel workbook created successfully with", length(sheet_names), "sheets"), level = 1)

        return(wb)

      }, error = function(e) {
        debug_log(paste("Error creating Excel:", e$message), level = 1)
        showNotification(paste("Error creating Excel file:", e$message), type = "error")
        return(NULL)
      })
    },

    get_excel_download_data = function() {
      wb <- this$create_results_excel()
      if (is.null(wb)) return(NULL)

      # Write to temporary file
      temp_file <- tempfile(fileext = ".xlsx")
      saveWorkbook(wb, temp_file, overwrite = TRUE)

      return(temp_file)
    },

    is_export_ready = reactive({
      has_primary <- !is.null(core_values$primary_data_raw()) && nrow(core_values$primary_data_raw()) > 0
      has_metadata <- !is.null(core_values$handson_metadata()) && nrow(core_values$handson_metadata()) > 0

      return(has_primary && has_metadata)
    })
  )
}

#' Create configuration export functions for all modules
#' @param modules_list list of initialized modules
#' @param ui_config_values UI configuration values
#' @param core_values core reactive values
#' @return list of configuration export functions
create_config_export_functions <- function(modules_list, ui_config_values, core_values) {

  list(
    get_ratio_configurations_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$ratios_out)) {
          current_configs <- safe_module_call(modules_list$ratios_out$ratio_configurations,
                                              default_return = NULL,
                                              context = "ratio_configurations")

          if (!is.null(current_configs) && is.data.frame(current_configs) && nrow(current_configs) > 0) {
            debug_log(paste("Exporting", nrow(current_configs), "ratio configurations"), level = 2)
            return(current_configs)
          }

          export_configs <- safe_module_call(modules_list$ratios_out$get_ratio_configurations_for_export,
                                             default_return = NULL,
                                             context = "ratio_export")
          if (!is.null(export_configs) && is.data.frame(export_configs)) {
            return(export_configs)
          }
        }

        return(data.frame(
          Title = character(),
          Content = character(),
          Numerator = I(list()),
          Denominator = I(list()),
          Statistics = character(),
          "Adjustment Method" = character(),
          "Valid Count" = numeric(),
          "Valid Logic" = character(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))

      }, error = function(e) {
        debug_log(paste("Error getting ratio configurations:", e$message), level = 1)
        return(data.frame())
      })
    },

    get_imputation_ui_config_for_export = function() {
      tryCatch({
        current_settings <- core_values$imputation_setting()
        if (!is.null(current_settings) &&
            !is.null(current_settings$imputation_method_select) &&
            !is.null(current_settings$imputation_column_select)) {
          return(current_settings)
        }

        central_config <- ui_config_values$central_imputation_ui_config()
        if (!is.null(central_config)) {
          return(central_config)
        }

        return(list(
          imputation_method_select = "None",
          imputation_column_select = character(0)
        ))

      }, error = function(e) {
        debug_log(paste("Error getting imputation UI config:", e$message), level = 1)
        core_values$ui_config_errors(append(core_values$ui_config_errors(), paste("Export imputation config error:", e$message)))
        return(NULL)
      })
    },

    get_filtering_ui_config_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$filtering_out)) {
          filter_state <- safe_module_call(modules_list$filtering_out$get_current_filter_state,
                                           default_return = NULL,
                                           context = "filter_state")
          if (!is.null(filter_state)) {
            return(filter_state)
          }
        }

        central_config <- ui_config_values$central_filtering_ui_config()
        if (!is.null(central_config)) {
          return(central_config)
        }

        current_filtering <- list()
        confidence <- core_values$filtering_confidence()
        valid_values <- core_values$filtering_valid_values()
        conditions <- core_values$filtered_conditions()

        if (!is.null(confidence)) current_filtering$confidence <- confidence
        if (!is.null(valid_values)) current_filtering$valid_values <- valid_values
        if (!is.null(conditions)) current_filtering$custom <- conditions

        if (length(current_filtering) > 0) {
          return(current_filtering)
        }

        return(list(
          confidence = list(
            numeric_fdr_dw = FALSE,
            string_fdr_dw = FALSE,
            numeric_input_dw_max = NULL,
            numeric_input_dw = NULL,
            string_input_dw = ""
          ),
          valid_values = list(
            valid_filtering_group_dw = "In total",
            valid_filtering_value_dw = 1
          ),
          custom = data.frame(
            Column = character(),
            Operator_1 = character(),
            Value_1 = character(),
            Logic = character(),
            Operator_2 = character(),
            Value_2 = character(),
            Empty_Filter = character(),
            Multi_Column_Logic = character(),
            stringsAsFactors = FALSE
          )
        ))

      }, error = function(e) {
        debug_log(paste("Error getting filtering UI config:", e$message), level = 1)
        core_values$filtering_config_errors(append(core_values$filtering_config_errors(), paste("Export filtering config error:", e$message)))
        return(NULL)
      })
    },

    get_ratios_ui_config_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$ratios_out)) {
          current_settings <- safe_module_call(modules_list$ratios_out$get_current_ui_state,
                                               default_return = NULL,
                                               context = "ratios_ui_state")
          if (!is.null(current_settings)) return(current_settings)
        }

        central_config <- ui_config_values$central_ratios_ui_config()
        if (!is.null(central_config)) return(central_config)

        return(list(
          ratio_settings = list(
            custom_col_sel = "Normalized Abundance",
            statistics_sel = "Limma",
            adjust_sel = "FDR",
            column_prefix = "Ratio_"
          ),
          ratio_configurations = data.frame()
        ))
      }, error = function(e) {
        debug_log(paste("Error getting ratios UI config:", e$message), level = 1)
        return(NULL)
      })
    },

    get_batch_effects_ui_config_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$batch_out)) {
          current_settings <- safe_module_call(modules_list$batch_out$get_current_ui_state,
                                               default_return = NULL,
                                               context = "batch_ui_state")
          if (!is.null(current_settings)) return(current_settings)
        }

        central_config <- ui_config_values$central_batch_effects_ui_config()
        if (!is.null(central_config)) return(central_config)

        return(list(
          batch_method = "ComBat",
          imputation_method_batch = "None",
          transformation_batch = "None",
          remove_imputed_batch = FALSE,
          batch_counter = 2,
          batch_inputs = list()
        ))
      }, error = function(e) {
        debug_log(paste("Error getting batch effects UI config:", e$message), level = 1)
        return(NULL)
      })
    },

    get_pivot_ui_config_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$pivot_out)) {
          current_settings <- safe_module_call(modules_list$pivot_out$get_current_ui_state,
                                               default_return = NULL,
                                               context = "pivot_ui_state")
          if (!is.null(current_settings)) return(current_settings)
        }

        central_config <- ui_config_values$central_pivot_ui_config()
        if (!is.null(central_config)) return(central_config)

        return(list(
          pivot_data_dw = "primary",
          pivot_type_dw = "wider",
          pivot_options = list()
        ))
      }, error = function(e) {
        debug_log(paste("Error getting pivot UI config:", e$message), level = 1)
        return(NULL)
      })
    },

    get_merge_ui_config_for_export = function() {
      tryCatch({
        if (!is.null(modules_list$merge_out)) {
          current_settings <- safe_module_call(modules_list$merge_out$get_current_ui_state,
                                               default_return = NULL,
                                               context = "merge_ui_state")
          if (!is.null(current_settings)) return(current_settings)
        }

        central_config <- ui_config_values$central_merge_ui_config()
        if (!is.null(central_config)) return(central_config)

        return(list(
          file1_col = NULL,
          file2_col = NULL,
          file2_add_col = character(0),
          join_type = "left"
        ))
      }, error = function(e) {
        debug_log(paste("Error getting merge UI config:", e$message), level = 1)
        return(NULL)
      })
    },

    get_edit_operations_for_export = function() {
      if (!is.null(modules_list$edit_out)) {
        pending_ops <- safe_module_call(modules_list$edit_out$pending_operations,
                                        default_return = NULL,
                                        context = "edit_operations_export")
        if (!is.null(pending_ops) && is.data.frame(pending_ops)) {
          return(pending_ops)
        }
      }

      return(data.frame(
        Operation = character(),
        Type = character(),
        Columns = character(),
        Parameters = character(),
        Description = character(),
        stringsAsFactors = FALSE
      ))
    }
  )
}

#' Create status and processing functions
#' @param core_values core reactive values
#' @param modules_list list of initialized modules
#' @param modification_functions modification tracking functions
#' @return list of status and processing functions
create_status_functions <- function(core_values, modules_list, modification_functions) {

  list(
    processing_status = reactive({
      status_parts <- list()

      tryCatch({
        # Basic data status
        current_data <- core_values$primary_data_raw()
        status_parts$data_loaded <- !is.null(current_data) && nrow(current_data) > 0
        if (status_parts$data_loaded) {
          status_parts$data_rows <- nrow(current_data)
          status_parts$data_cols <- ncol(current_data)
        }

        # Metadata status
        current_meta <- core_values$handson_metadata()
        status_parts$metadata_defined <- !is.null(current_meta) && nrow(current_meta) > 0
        if (status_parts$metadata_defined) {
          defined_content <- sum(!is.na(current_meta$Content))
          status_parts$metadata_completeness <- defined_content / nrow(current_meta)
        }

        # Processing status
        status_parts$final_processing_complete <- core_values$apply_triggered() &&
          !is.null(core_values$final_processed_data()) &&
          !is.null(core_values$final_processed_metadata())

        # Filter status
        status_parts$filter_applied <- core_values$filter_applied()
        if (status_parts$filter_applied) {
          filtered <- core_values$filtered_data()
          status_parts$filtered_rows <- if (!is.null(filtered)) nrow(filtered) else 0
        }

        # Data modification status
        status_parts$data_modified <- modification_functions$is_data_modified()
        status_parts$modification_history <- core_values$modification_history()

        # Module-specific statuses
        if (!is.null(modules_list$imputation_out)) {
          status_parts$imputation_applied <- safe_module_call(modules_list$imputation_out$imputation_applied,
                                                              default_return = FALSE,
                                                              context = "imputation_applied")
        }

        if (!is.null(modules_list$ratios_out)) {
          status_parts$ratios_configured <- safe_module_call(modules_list$ratios_out$has_ratios,
                                                             default_return = FALSE,
                                                             context = "ratios_has_ratios")
        }

        if (!is.null(modules_list$batch_out)) {
          tryCatch({
            if (!is.null(modules_list$batch_out$is_batch_correction_configured)) {
              status_parts$batch_effects_configured <- safe_module_call(
                modules_list$batch_out$is_batch_correction_configured,
                default_return = FALSE,
                context = "batch_is_configured_status",
                max_retries = 1
              )
            } else {
              status_parts$batch_effects_configured <- FALSE
            }
          }, error = function(e) {
            debug_log(paste("Error getting batch effects status:", e$message), level = 1)
            status_parts$batch_effects_configured <- FALSE
          })
        }

        # Processing pipeline readiness
        status_parts$ready_for_analysis <- status_parts$data_loaded &&
          status_parts$metadata_defined &&
          (status_parts$metadata_completeness > 0.5)

        status_parts$has_active_processing <- status_parts$filter_applied ||
          status_parts$imputation_applied ||
          (length(core_values$filtering_log()) > 0)

      }, error = function(e) {
        debug_log(paste("Error in processing_status:", e$message), level = 1)
        # Error fallbacks
        status_parts$data_loaded <- FALSE
        status_parts$metadata_defined <- FALSE
        status_parts$final_processing_complete <- FALSE
        status_parts$filter_applied <- FALSE
        status_parts$imputation_applied <- FALSE
        status_parts$data_modified <- FALSE
        status_parts$ready_for_analysis <- FALSE
        status_parts$has_active_processing <- FALSE
        status_parts$error_occurred <- TRUE
        status_parts$error_message <- e$message
      })

      return(status_parts)
    }),

    get_export_preview = function() {
      edit_ops <- this$get_edit_operations_for_export()
      ratio_configs <- this$get_ratio_configurations_for_export()

      list(
        filter_config = if (!is.null(ui_config_values$central_filtering_ui_config())) "Available" else "Not Available",
        imputation_config = if (!is.null(ui_config_values$central_imputation_ui_config())) "Available" else "Not Available",
        edit_operations = if (!is.null(edit_ops)) nrow(edit_ops) else 0,
        ratio_configurations = if (!is.null(ratio_configs)) nrow(ratio_configs) else 0,
        edit_operations_pending = if (!is.null(edit_ops) && "Executed" %in% names(edit_ops)) sum(!edit_ops$Executed, na.rm = TRUE) else 0,
        edit_operations_executed = if (!is.null(edit_ops) && "Executed" %in% names(edit_ops)) sum(edit_ops$Executed, na.rm = TRUE) else 0,
        ratio_configurations_methods = if (!is.null(ratio_configs) && "Statistics" %in% names(ratio_configs)) table(ratio_configs$Statistics) else NULL
      )
    }
  )
}

#' Create consolidated export/config/status function bundle for Data Wizard orchestration
#' @param loader_out file loader module output
#' @param core_values core reactive values
#' @param modification_functions modification tracking functions
#' @param modules_list initialized module outputs
#' @param ui_config_values UI configuration values
#' @return named list with excel, config_export, and status functions
create_datawizard_export_bundle <- function(loader_out, core_values, modification_functions,
                                            modules_list, ui_config_values) {
  list(
    excel_functions = create_excel_export_functions(loader_out, core_values, modification_functions, modules_list),
    config_export_functions = create_config_export_functions(modules_list, ui_config_values, core_values),
    status_functions = create_status_functions(core_values, modules_list, modification_functions)
  )
}
