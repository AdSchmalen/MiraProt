# Data Wizard/datawizard_ratios.R
# Main Coordinator for Custom Ratios Module

# Load submodules
source("modules/Data Wizard/ratios/datawizard_ratios_UI.R", local = modEnv)
source("modules/Data Wizard/ratios/datawizard_ratios_validation.R", local = modEnv)
source("modules/Data Wizard/ratios/datawizard_ratios_utils.R", local = modEnv)
source("modules/Data Wizard/ratios/datawizard_ratios_core.R", local = modEnv)

#' Custom Ratios Module UI - Main Coordinator
#' @param id module namespace id
#' @export
modRatiosUI <- function(id) {
  ns <- NS(id)
  ratiosUISubmodule(ns("ui"))
}

#' Custom Ratios Module Server - Main Coordinator
#' @param id module namespace id
#' @param data_def reactive containing metadata definition
#' @param get_data function to get current data
#' @param set_data function to set modified data
#' @param available_samples reactive containing available sample names
#' @param UI_config reactive containing UI configuration
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modRatiosServer <- function(id, data_def = NULL, get_data = NULL, set_data = NULL,
                            available_samples = NULL, UI_config = reactive(NULL),
                            session_restore_trigger = reactive(NULL),
                            metadata_revision_debounced = reactive(NULL),
                            primary_working_revision_debounced = reactive(NULL),
                            debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "RATIOS", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ RATIOS ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Custom contrasts module server starting", 1)

    # Initialize Submodules
    utils_module <- ratiosUtilsServer("utils")
    core_module <- ratiosCoreServer("core", utils_module)

    # Reactive Values
    ratio_configurations_df <- reactiveVal(
      data.frame(
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
      )
    )

    processing_status <- reactiveVal("idle")
    processing_log_entries <- reactiveVal(list())
    last_applied_configurations <- reactiveVal(NULL)
    analysis_results <- reactiveVal(list())
    processing_errors <- reactiveVal(list())
    ui_config_applied <- reactiveVal(FALSE)
    ui_config_source_info <- reactiveVal("none")
    last_applied_ratios_list <- reactiveVal(NULL)
    # Keep reactiveVal's promise free of cross-script constructor calls during
    # module startup. Shiny evaluates that promise inside rlang::dots_list(); a
    # partially sourced/masked constructor otherwise surfaces only as the opaque
    # "attempt to apply non-function" startup error.
    contrast_mapping_collection <- reactiveVal(
      list(version = DATAWIZARD_CONTRAST_MAPPING_VERSION, mappings = list())
    )

    # Enhanced Logging Functions
    add_processing_log_entry <- function(action, details, status = "success", data = NULL) {
      tryCatch({
        new_entry <- list(
          timestamp = Sys.time(),
          action = action,
          details = details,
          status = status,
          data = data
        )

        current_log <- processing_log_entries()
        updated_log <- c(current_log, list(new_entry))

        # Keep only last 20 entries
        if (length(updated_log) > 20) {
          updated_log <- tail(updated_log, 20)
        }

        processing_log_entries(updated_log)

        # Enhanced debug logging based on status
        if (status == "error") {
          debug_log(paste("ERROR in", action, ":", details), 1)
          current_errors <- processing_errors()
          processing_errors(c(current_errors, list(new_entry)))
        } else if (status == "warning") {
          debug_log(paste("WARNING in", action, ":", details), 1)
        } else if (status == "success") {
          debug_log(paste("SUCCESS in", action), 2)
        } else {
          debug_log(paste("INFO", action, ":", details), 2)
        }

      }, error = function(e) {
        debug_log(paste("Error adding to processing log:", e$message), 1)
      })
    }

    # Initialize UI with connected reactive values - WICHTIG: Namespace anpassen!
    ui_inputs <- ratiosUIServer(
      "ui",  # KEIN ns() hier, da moduleServer das bereits handhabt
      ratio_configurations_df,
      processing_status,
      analysis_results,
      processing_log_entries,
      ui_config_applied,
      ui_config_source_info,
      data_def = data_def,              # Pass reactive, nicht den Wert!
      available_samples = available_samples,
      get_data = get_data,
      UI_config = UI_config,
      parent_session = session,  # Pass parent session für updateTextInput
      session_restore_trigger = session_restore_trigger,
      metadata_revision_debounced = metadata_revision_debounced,
      primary_working_revision_debounced = primary_working_revision_debounced
    )

    # Validation Functions
    validate_ratio_configuration_inputs <- function() {
      tryCatch({
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
          return(list(valid = FALSE, message = "A comparison with this name already exists."))
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
        debug_log(paste("Error in input validation:", e$message), 1)
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

      # Make unique by adding number suffix
      counter <- 1
      while (paste0(proposed_name, "_", counter) %in% existing_names) {
        counter <- counter + 1
      }

      return(paste0(proposed_name, "_", counter))
    }

    # Apply ratios function with statistical methods from core_module
    apply_single_ratio_configuration_fixed <- function(ratio_config, current_data, metadata_definition) {
      tryCatch({
        debug_log(paste("Applying contrast analysis:", ratio_config$Title), 1)

        # Ensure current data has proper Row Index
        current_data <- utils_module$ensure_original_row_index(current_data)

        # Find columns for the content type
        content_matching_rows <- which(metadata_definition$Content == ratio_config$Content)
        if (length(content_matching_rows) == 0) {
          stop("No columns found for content type: ", ratio_config$Content)
        }

        # Map sample groups to column indices
        numerator_sample_groups <- ratio_config$Numerator[[1]]
        denominator_sample_groups <- ratio_config$Denominator[[1]]

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

        # Get row index column
        row_index_column <- which(names(current_data) == "Row Index")[1]

        # Apply the appropriate statistical method
        selected_method <- ratio_config$Statistics
        adjustment_method <- ratio_config$`Adjustment Method`
        comparison_name <- ratio_config$Title

        # Extract valid value filtering parameters from ratio config
        valid_count <- if (is.na(ratio_config$`Valid Count`)) NULL else as.numeric(ratio_config$`Valid Count`)
        valid_logic <- if (is.na(ratio_config$`Valid Logic`)) NULL else ratio_config$`Valid Logic`

        debug_log(paste("Valid value filtering - Count:", valid_count, "Logic:", valid_logic), 2)

        ratio_log_fn <- function(action, details = "", status = "info", data = NULL) {
          # Compatibility path: some utility loggers call log_fn(message, level)
          if (is.numeric(details) && length(details) == 1 && identical(status, "info") && is.null(data)) {
            debug_log(paste0("[", comparison_name, "] ", action), as.integer(details))
            return(invisible(NULL))
          }

          detail_text <- if (!is.null(details) && nzchar(as.character(details))) paste0(" | ", as.character(details)) else ""
          debug_log(paste0("[", comparison_name, "] ", action, detail_text), 2)
          add_processing_log_entry(
            action = paste("Ratio Analysis", comparison_name, action),
            details = as.character(details),
            status = status,
            data = data
          )
        }

        # Use the core module's statistical methods based on selection
        analysis_result <- switch(selected_method,
                                  "Welch's T-Test" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_welch_t_test_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "Welch's T-Test failed"
                                  ),
                                  "Student's T-Test" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_t_test_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "Student's T-Test failed"
                                  ),
                                  "Moderated Welch Test" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_moderated_welch_test_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "Moderated Welch Test failed"
                                  ),
                                  "Limma" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_limma_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "Limma analysis failed"
                                  ),
                                  "DEqMS" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_deqms_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "DEqMS analysis failed"
                                  ),
                                  "Mann-Whitney U Test" = utils_module$execute_statistical_function_safely(
                                    function() core_module$perform_mann_whitney_analysis(
                                      current_data, numerator_column_indices, denominator_column_indices,
                                      adjustment_method, row_index_column, comparison_name,
                                      metadata_definition, log_fn = ratio_log_fn,
                                      valid_count = valid_count,  # NEU
                                      valid_logic = valid_logic,  # NEU
                                      content_type = ratio_config$Content  # NEU
                                    ),
                                    fallback_result = data.frame(),
                                    error_message_prefix = "Mann-Whitney U Test failed"
                                  ),
                                  # Fallback
                                  {
                                    showNotification(paste("Unknown statistical method:", selected_method), type = "error", duration = 8)
                                    data.frame()
                                  }
        )

        if (!is.null(analysis_result) && nrow(analysis_result) > 0) {
          debug_log(paste("Statistical analysis completed with", nrow(analysis_result), "results"), 1)
          return(analysis_result)
        } else {
          return(NULL)
        }

      }, error = function(e) {
        debug_log(paste("Error in apply_single_ratio_configuration_fixed:", e$message), 1)
        add_processing_log_entry(
          action = paste("Ratio Analysis", ratio_config$Title),
          details = e$message,
          status = "error"
        )
        return(NULL)
      })
    }

    apply_all_ratio_configurations <- function() {
      processing_status("processing")
      processing_start_time <- Sys.time()

      tryCatch({
        # Clear previous errors
        processing_errors(list())

        # Get current data and metadata
        current_data <- if (!is.null(get_data)) get_data() else NULL
        current_metadata <- if (!is.null(data_def)) data_def() else NULL

        if (is.null(current_data)) {
          stop("No data available for ratio analysis")
        }

        if (is.null(current_metadata)) {
          stop("No metadata available for ratio analysis")
        }

        ratio_configs <- ratio_configurations_df()
        if (nrow(ratio_configs) == 0) {
          stop("No ratio configurations defined")
        }

        debug_log(paste("Applying", nrow(ratio_configs), "contrast configurations"), 1)

        # Store configurations for metadata updates
        last_applied_configurations(ratio_configs)

        # Process each ratio configuration
        analysis_results_list <- list()
        generated_mappings <- list()
        enhanced_data <- current_data
        successful_analyses <- 0
        failed_analyses <- 0

        for (i in 1:nrow(ratio_configs)) {
          current_ratio_config <- ratio_configs[i, , drop = FALSE]

          debug_log(paste("Processing contrast", i, "of", nrow(ratio_configs), ":", current_ratio_config$Title), 2)

          single_analysis_result <- apply_single_ratio_configuration_fixed(current_ratio_config, current_data, current_metadata)

          if (!is.null(single_analysis_result) && nrow(single_analysis_result) > 0) {
            analysis_results_list[[current_ratio_config$Title]] <- single_analysis_result
            enhanced_data <- utils_module$merge_analysis_results_fixed(enhanced_data, single_analysis_result, validate_merge = TRUE)
            successful_analyses <- successful_analyses + 1
            suffixes <- c(ratio = "_Abundance Ratio", p_value = "_Abundance Ratio p-Value",
                          adjusted_p_value = "_Abundance Ratio Adj. p-Value")
            result_columns <- setdiff(names(single_analysis_result), "Row Index")
            generated_columns <- vapply(suffixes, function(suffix) {
              hit <- result_columns[endsWith(result_columns, suffix)]
              if (length(hit) == 1L) hit else NA_character_
            }, character(1))
            generated_columns <- generated_columns[!is.na(generated_columns)]
            # set_data publishes the enhanced frame as the next working-data
            # revision.  Bind the mapping to that output revision, not to the
            # input snapshot from which the statistics were calculated.
            source_revision <- as.character(as.integer(
              primary_working_revision_debounced() %||% 0L) + 1L)
            contrast_id <- paste0("dw-contrast-", i, "-r", source_revision)
            reference_columns <- c(
              as.character(current_metadata$Sample),
              as.character(current_metadata$Options)
            )
            reference_columns <- trimws(reference_columns)
            reference_columns <- unique(reference_columns[
              !is.na(reference_columns) & nzchar(reference_columns)
            ])
            generated_mappings[[length(generated_mappings) + 1L]] <- create_datawizard_contrast_mapping(
              contrast_id, generated_columns, current_ratio_config$Numerator[[1]],
              current_ratio_config$Denominator[[1]], source_revision,
              reference_columns,
              partial = length(generated_columns) < length(suffixes),
              missing_variants = setdiff(names(suffixes), names(generated_columns)))
          } else {
            failed_analyses <- failed_analyses + 1
          }
        }

        # Calculate processing time
        total_duration <- as.numeric(difftime(Sys.time(), processing_start_time, units = "secs"))

        # Store results
        analysis_results(analysis_results_list)
        last_applied_ratios_list(ratio_configs)
        contrast_mapping_collection(create_datawizard_contrast_mapping_collection(generated_mappings))

        # Update data if we have results
        if (length(analysis_results_list) > 0) {
          if (!is.null(set_data)) {
            update_success <- set_data(enhanced_data)
            if (update_success) {
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
            } else {
              add_processing_log_entry(
                action = "Data Update",
                details = "Failed to update data after ratio analysis",
                status = "error"
              )
            }
          }

          # Enhanced completion notification (ORIGINAL CODE)
          if (failed_analyses == 0) {
            success_msg <- paste("Successfully applied", successful_analyses, "ratio analyses",
                                 sprintf("(%.2fs)", total_duration))
            showNotification(success_msg, type = "message", duration = 4)
          } else {
            warning_msg <- paste("Applied", successful_analyses, "analyses with", failed_analyses, "failures",
                                 sprintf("(%.2fs)", total_duration))
            showNotification(warning_msg, type = "warning", duration = 6)
          }
        } else {
          showNotification("No ratio analyses were successfully completed", type = "error", duration = 8)
        }

        for (i in 1:nrow(ratio_configs)) {
          rc <- ratio_configs[i, , drop = FALSE]
          numerator_str   <- paste(rc$Numerator[[1]],   collapse = ", ")
          denominator_str <- paste(rc$Denominator[[1]], collapse = ", ")
          debug_log(paste0(
            "Performed Ratios calculation with ", rc$Statistics, " statistics",
            " | Content: ", rc$Content,
            " | Numerator: ", numerator_str,
            " | Denominator: ", denominator_str,
            " | Adjustment Method: ", rc$`Adjustment Method`,
            " | Valid Count: ", rc$`Valid Count`,
            " | Valid Logic: ", rc$`Valid Logic`
          ), level = 0)
        }

        debug_log(paste("Contrast calculation completed:",
                        ncol(enhanced_data) - ncol(current_data), "ratio columns added"), level = 0)

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
        debug_log(error_message, 1)

        add_processing_log_entry(
          action = "Apply All Ratios Error",
          details = error_message,
          status = "error",
          data = list(error = e$message)
        )

        showNotification(error_message, type = "error", duration = 10)
        processing_status("error")

        return(list(
          success = FALSE,
          results = list(),
          processed_data = NULL,
          errors = c(processing_errors(), list(list(error = e$message, timestamp = Sys.time())))
        ))
      })
    }

    # Event handlers for UI buttons - Direkt die reactive values verwenden
    observeEvent(ui_inputs$add_ratio_clicked(), {
      debug_log("Add contrast button clicked", 1)

      validation_result <- validate_ratio_configuration_inputs()
      if (!validation_result$valid) {
        showNotification(validation_result$message, type = "error", duration = 8)
        return()
      }

      final_comparison_name <- make_comparison_name_unique(trimws(ui_inputs$name_sel_dw()))

      if (final_comparison_name != trimws(ui_inputs$name_sel_dw())) {
        showNotification(
          paste("Name changed to", final_comparison_name, "to ensure uniqueness."),
          type = "message", duration = 3
        )
      }

      new_ratio_config <- data.frame(
        Title = final_comparison_name,
        Content = ui_inputs$custom_col_sel_dw(),
        Numerator = I(list(ui_inputs$numerator_sel_dw())),
        Denominator = I(list(ui_inputs$denominator_sel_dw())),
        Statistics = ui_inputs$statistics_sel_dw(),
        "Adjustment Method" = ui_inputs$adjust_sel_dw(),
        "Valid Count" = ui_inputs$valid_comparison_sel_dw(),
        "Valid Logic" = ui_inputs$valid_compgroup_sel_dw(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      current_ratio_configs <- ratio_configurations_df()
      updated_ratio_configs <- rbind(current_ratio_configs, new_ratio_config)
      ratio_configurations_df(updated_ratio_configs)

      add_processing_log_entry(
        action = "Add Ratio Configuration",
        details = paste("Added ratio configuration:", final_comparison_name),
        status = "success",
        data = list(
          name = final_comparison_name,
          numerator = ui_inputs$numerator_sel_dw,
          denominator = ui_inputs$denominator_sel_dw,
          method = ui_inputs$statistics_sel_dw
        )
      )

      # Reset only the name field
      ui_inputs$reset_name_field()

      showNotification(paste("Custom ratio '", final_comparison_name, "' added successfully."),
                       type = "message", duration = 3)
    }, ignoreInit = TRUE)

    observeEvent(ui_inputs$apply_ratios_clicked(), {
      debug_log("Apply contrasts button clicked", 1)

      if (nrow(ratio_configurations_df()) == 0) {
        showNotification("No ratio configurations to apply.", type = "warning", duration = 4)
        return()
      }

      application_result <- apply_all_ratio_configurations()
    }, ignoreInit = TRUE)

    observeEvent(ui_inputs$clear_ratio_clicked(), {
      debug_log("Clear contrasts button clicked", 1)

      ratio_configurations_df(data.frame(
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

      analysis_results(list())
      processing_errors(list())
      last_applied_ratios_list(NULL)
      last_applied_configurations(NULL)

      add_processing_log_entry(
        action = "Clear All Ratios",
        details = "All ratio configurations cleared",
        status = "success"
      )

      showNotification("All custom ratios cleared.", type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # UI_config Integration Observer
    observeEvent(UI_config(), {
      if (!is.null(UI_config)) {
        ui_config_data <- UI_config()

        if (!is.null(ui_config_data) && !ui_config_applied()) {
          tryCatch({
            debug_log("Processing UI configuration in parent module", 1)
            if (!is.null(ui_config_data$contrast_mapping_collection)) {
              validate_datawizard_contrast_mapping_collection(ui_config_data$contrast_mapping_collection)
              contrast_mapping_collection(ui_config_data$contrast_mapping_collection)
            }

            # Load ratio configurations if available
            if (!is.null(ui_config_data$ratio_configurations)) {
              configs <- ui_config_data$ratio_configurations

              if (is.data.frame(configs) && nrow(configs) > 0) {
                debug_log(paste("Loading", nrow(configs), "contrast configurations from UI_config"), 1)

                # Validate structure
                required_cols <- c("Title", "Content", "Numerator", "Denominator", "Statistics")
                missing_cols <- setdiff(required_cols, names(configs))

                if (length(missing_cols) == 0) {
                  # Ensure proper column types
                  if (!is.list(configs$Numerator)) {
                    configs$Numerator <- I(as.list(configs$Numerator))
                  }
                  if (!is.list(configs$Denominator)) {
                    configs$Denominator <- I(as.list(configs$Denominator))
                  }

                  # Ensure required columns exist with defaults
                  if (!"Adjustment Method" %in% names(configs)) {
                    configs$`Adjustment Method` <- "FDR"
                  }
                  if (!"Valid Count" %in% names(configs)) {
                    configs$`Valid Count` <- 1
                  }
                  if (!"Valid Logic" %in% names(configs)) {
                    configs$`Valid Logic` <- "In total"
                  }

                  # Make names unique to avoid conflicts
                  for (i in seq_len(nrow(configs))) {
                    original_name <- configs$Title[i]
                    unique_name <- make_comparison_name_unique(original_name)
                    if (unique_name != original_name) {
                      configs$Title[i] <- unique_name
                      debug_log(paste("Renamed configuration to ensure uniqueness:", unique_name), 2)
                    }
                  }

                  # Set the configurations
                  ratio_configurations_df(configs)

                  add_processing_log_entry(
                    action = "Load UI_config Ratios",
                    details = paste("Loaded", nrow(configs), "ratio configurations from UI_config"),
                    status = "success",
                    data = list(configurations_loaded = nrow(configs))
                  )

                  ui_config_applied(TRUE)
                  ui_config_source_info("rule_file_import")

                  # showNotification(
                  #   paste("Successfully loaded", nrow(configs), "ratio configurations from template"),
                  #   type = "message", duration = 4
                  # )
                }
              }
            }

          }, error = function(e) {
            debug_log(paste("Error applying UI_config:", e$message), 1)
            showNotification(
              paste("Error applying ratios configuration:", e$message),
              type = "error", duration = 8
            )
          })
        }
      }
    }, ignoreInit = TRUE)

    # Helper functions
    module_health_check <- function() {
      tryCatch({
        health_status <- list(
          module_name = "Custom Ratios",
          status = "OK",
          processing_status = processing_status(),
          configuration_count = nrow(ratio_configurations_df()),
          error_count = length(processing_errors()),
          debug_level = DEBUG_LEVEL,
          results_count = length(analysis_results())
        )

        warnings <- character()

        if (health_status$error_count > 5) {
          warnings <- c(warnings, paste("High error count:", health_status$error_count))
        }

        missing_packages <- character()
        if (!requireNamespace("limma", quietly = TRUE)) {
          missing_packages <- c(missing_packages, "limma")
        }
        if (!requireNamespace("DEqMS", quietly = TRUE)) {
          missing_packages <- c(missing_packages, "DEqMS")
        }
        if (!requireNamespace("pracma", quietly = TRUE)) {
          missing_packages <- c(missing_packages, "pracma")
        }

        if (length(missing_packages) > 0) {
          warnings <- c(warnings, paste("Missing packages:", paste(missing_packages, collapse = ", ")))
        }

        health_status$warnings <- warnings
        health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

        return(health_status)

      }, error = function(e) {
        return(list(
          module_name = "Custom Ratios",
          status = "ERROR",
          error_message = e$message,
          overall_health = "Critical"
        ))
      })
    }

    # Register cleanup function
    cleanup_manager$register_module("ratios", function() {
      debug_log("Executing [Contrasts] cleanup", 2)

      # Clear reactive values
      ratio_configurations_df(data.frame(
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
      last_applied_configurations(NULL)
      processing_status("idle")
      processing_log_entries(list())
      last_applied_ratios_list(NULL)
      ui_config_applied(FALSE)
      ui_config_source_info("none")
      analysis_results(list())
      processing_errors(list())

      debug_log("[Contrasts] cleanup completed", 2)
    })

    # # Session cleanup
    # session$onSessionEnded(function() {
    #   debug_log("Custom contrasts module session cleanup", 1)
    #   tryCatch({
    #     ratio_configurations_df(data.frame(
    #       Title = character(),
    #       Content = character(),
    #       Numerator = I(list()),
    #       Denominator = I(list()),
    #       Statistics = character(),
    #       "Adjustment Method" = character(),
    #       "Valid Count" = numeric(),
    #       "Valid Logic" = character(),
    #       stringsAsFactors = FALSE,
    #       check.names = FALSE
    #     ))
    #     last_applied_configurations(NULL)
    #     processing_status("idle")
    #     processing_log_entries(list())
    #     last_applied_ratios_list(NULL)
    #     ui_config_applied(FALSE)
    #     ui_config_source_info("none")
    #     analysis_results(list())
    #     processing_errors(list())
    #
    #     debug_log("Session cleanup completed", 1)
    #   }, error = function(e) {
    #     debug_log(paste("Error during session cleanup:", e$message), 1)
    #   })
    # })

    # Return values (matching legacy module exactly)
    return(list(
      # Main data access
      ratio_configurations = ratio_configurations_df,

      # Processing functions
      apply_all_ratios = apply_all_ratio_configurations,

      # Method recommendation functions
      get_method_suggestions = reactive({
        num_groups <- length(ui_inputs$numerator_sel_dw() %||% character(0))
        denom_groups <- length(ui_inputs$denominator_sel_dw() %||% character(0))

        if (num_groups > 0 && denom_groups > 0) {
          sample_size <- min(num_groups, denom_groups)
          has_psm <- !is.null(utils_module$find_psm_columns(data_def()))
          return(utils_module$suggest_statistical_methods(sample_size, has_psm))
        }
        return(NULL)
      }),

      # Enhanced status information
      get_processing_summary = reactive({
        list(
          configured = nrow(ratio_configurations_df()),
          applied = length(analysis_results()),
          errors = length(processing_errors()),
          status = processing_status(),
          log_entries = length(processing_log_entries())
        )
      }),
      processing_log = processing_log_entries,

      # Configuration status
      has_ratios = reactive({ nrow(ratio_configurations_df()) > 0 }),
      ratio_count = reactive({ nrow(ratio_configurations_df()) }),

      # Processing status
      processing_status = processing_status,

      # Results access
      ratio_results = analysis_results,

      # Enhanced error tracking
      processing_errors = reactive({ processing_errors() }),
      get_processing_errors = function() { processing_errors() },
      clear_processing_errors = function() {
        tryCatch({
          processing_errors(list())
          debug_log("Processing errors cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing processing errors:", e$message), 1)
        })
      },

      # Metadata integration functions
      get_last_applied_configs = function() { last_applied_configurations() },
      get_contrast_mapping_collection = function() { contrast_mapping_collection() },
      load_contrast_mapping_collection = function(collection) {
        validate_datawizard_contrast_mapping_collection(collection)
        contrast_mapping_collection(collection)
        TRUE
      },
      last_applied_ratio_configs = last_applied_configurations,

      # Central Apply integration functions
      is_configured = reactive({ nrow(ratio_configurations_df()) > 0 }),

      # Get current UI state for export
      get_current_ui_state = function() {
        list(
          ratio_settings = list(
            custom_col_sel = ui_inputs$custom_col_sel_dw(),
            statistics_sel = ui_inputs$statistics_sel_dw(),
            adjust_sel = ui_inputs$adjust_sel_dw()
          ),
          ratio_configurations = ratio_configurations_df(),
          contrast_mapping_collection = contrast_mapping_collection()
        )
      },

      # Session-restore bridge (delegates to inner ratiosUIServer which
      # owns the namespaced inputs). get_session_state() returns a flat
      # closure-free input snapshot; set_session_state() stages it and an
      # internal observer pushes update*Input after the next flush.
      get_session_state = if (is.function(ui_inputs$get_session_state)) {
        ui_inputs$get_session_state
      } else {
        function() list()
      },
      set_session_state = if (is.function(ui_inputs$set_session_state)) {
        ui_inputs$set_session_state
      } else {
        function(state) invisible(NULL)
      },

      # Helper function for uniqueness
      make_comparison_name_unique = make_comparison_name_unique,

      # Available statistical methods information
      get_available_statistical_methods = function() {
        available_methods <- c("ANOVA", "Welch's T-Test")

        if (requireNamespace("limma", quietly = TRUE)) {
          available_methods <- c(available_methods, "Limma")
        }

        if (requireNamespace("limma", quietly = TRUE) && requireNamespace("DEqMS", quietly = TRUE)) {
          available_methods <- c(available_methods, "DEqMS")
        }

        if (requireNamespace("pracma", quietly = TRUE)) {
          available_methods <- c(available_methods, "Moderated Welch Test")
        }

        available_methods <- c(available_methods, "Mann-Whitney U Test")

        return(sort(available_methods))
      },

      # Check if specific method is available
      is_method_available = function(method_name) {
        switch(method_name,
               "ANOVA" = TRUE,
               "Welch's T-Test" = TRUE,
               "Mann-Whitney U Test" = TRUE,
               "Limma" = requireNamespace("limma", quietly = TRUE),
               "DEqMS" = requireNamespace("limma", quietly = TRUE) && requireNamespace("DEqMS", quietly = TRUE),
               "Moderated Welch Test" = requireNamespace("pracma", quietly = TRUE),
               FALSE)
      },

      # Module health check
      module_health_check = module_health_check,

      # Row index validation function
      validate_row_index_after_merge = function(original_data, merged_data, new_column_names) {
        tryCatch({
          if (!identical(original_data$`Row Index`, merged_data$`Row Index`)) {
            stop("CRITICAL: Row Index modified during merge!")
          }

          if (nrow(original_data) != nrow(merged_data)) {
            stop("CRITICAL: Row count changed during merge!")
          }

          if (!all(new_column_names %in% names(merged_data))) {
            warning("Some expected columns missing after merge")
          }

          debug_log("Row Index validation PASSED", 2)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Row Index validation FAILED:", e$message), 1)
          return(FALSE)
        })
      },

      # Get missing dependencies for methods
      get_missing_dependencies = function() {
        missing <- list()

        if (!requireNamespace("limma", quietly = TRUE)) {
          missing[["limma"]] <- c("Limma", "DEqMS")
        }

        if (!requireNamespace("DEqMS", quietly = TRUE)) {
          missing[["DEqMS"]] <- "DEqMS"
        }

        if (!requireNamespace("pracma", quietly = TRUE)) {
          missing[["pracma"]] <- "Moderated Welch Test"
        }

        return(missing)
      },

      # Enhanced configuration export
      get_ratio_configurations_for_export = function() {
        tryCatch({
          current_configs <- ratio_configurations_df()
          if (!is.null(current_configs) && nrow(current_configs) > 0) {
            debug_log(paste("Exporting", nrow(current_configs), "contrast configurations"), 2)
            return(current_configs)
          } else {
            debug_log("No contrast configurations to export", 2)
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
          }
        }, error = function(e) {
          debug_log(paste("Error getting contrast configurations for export:", e$message), 1)
          return(data.frame())
        })
      },

      # Enhanced configuration loading
      load_ratio_configurations = function(configurations_table) {
        if (is.null(configurations_table) || nrow(configurations_table) == 0) {
          debug_log("No contrast configurations to load", 2)
          return(TRUE)
        }

        tryCatch({
          debug_log(paste("Loading", nrow(configurations_table), "contrast configurations"), 1)

          # Validate structure
          required_cols <- c("Title", "Content", "Numerator", "Denominator", "Statistics")
          missing_cols <- setdiff(required_cols, names(configurations_table))
          if (length(missing_cols) > 0) {
            debug_log(paste("ERROR: Missing required columns:", paste(missing_cols, collapse = ", ")), 1)
            showNotification(
              paste("Invalid ratio configurations: missing columns", paste(missing_cols, collapse = ", ")),
              type = "error", duration = 8
            )
            return(FALSE)
          }

          # Ensure proper column types
          if (nrow(configurations_table) > 0) {
            if (!is.list(configurations_table$Numerator)) {
              configurations_table$Numerator <- I(as.list(configurations_table$Numerator))
            }
            if (!is.list(configurations_table$Denominator)) {
              configurations_table$Denominator <- I(as.list(configurations_table$Denominator))
            }

            if (!"Adjustment Method" %in% names(configurations_table)) {
              configurations_table$`Adjustment Method` <- "FDR"
            }
            if (!"Valid Count" %in% names(configurations_table)) {
              configurations_table$`Valid Count` <- 1
            }
            if (!"Valid Logic" %in% names(configurations_table)) {
              configurations_table$`Valid Logic` <- "In total"
            }
          }

          # Make configuration names unique
          if (nrow(ratio_configurations_df()) > 0) {
            for (i in seq_len(nrow(configurations_table))) {
              original_name <- configurations_table$Title[i]
              unique_name <- make_comparison_name_unique(original_name)
              if (unique_name != original_name) {
                configurations_table$Title[i] <- unique_name
                debug_log(paste("Renamed configuration to avoid conflicts:", unique_name), 2)
              }
            }
          }

          # Append to existing configurations
          current_configs <- ratio_configurations_df()
          combined_configs <- rbind(current_configs, configurations_table)
          ratio_configurations_df(combined_configs)

          add_processing_log_entry(
            action = "Load Configurations Template",
            details = paste("Loaded", nrow(configurations_table), "ratio configurations from template"),
            status = "success",
            data = list(
              configurations_loaded = nrow(configurations_table),
              total_configurations = nrow(combined_configs),
              configuration_names = configurations_table$Title
            )
          )

          # showNotification(
          #   paste("Successfully loaded", nrow(configurations_table), "ratio configurations"),
          #   type = "message", duration = 4
          # )

          return(TRUE)

        }, error = function(e) {
          debug_log(paste("Error loading contrast configurations:", e$message), 1)

          add_processing_log_entry(
            action = "Load Configurations Error",
            details = paste("Failed to load ratio configurations:", e$message),
            status = "error"
          )

          showNotification(
            paste("Error loading ratio configurations:", e$message),
            type = "error", duration = 8
          )
          return(FALSE)
        })
      },

      # Enhanced configuration management
      clear_all_configurations = function() {
        tryCatch({
          ratio_configurations_df(data.frame(
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

          analysis_results(list())
          processing_errors(list())
          last_applied_ratios_list(NULL)
          last_applied_configurations(NULL)

          add_processing_log_entry(
            action = "Clear All Configurations",
            details = "All ratio configurations cleared via template loading",
            status = "success"
          )

          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error clearing configurations:", e$message), 1)
          return(FALSE)
        })
      },

      # Configuration validation
      validate_configuration_compatibility = function(configuration) {
        tryCatch({
          if (!is.null(data_def)) {
            current_metadata <- data_def()
            if (!is.null(current_metadata)) {
              available_content_types <- unique(current_metadata$Content)
              if (!configuration$Content %in% available_content_types) {
                return(list(
                  valid = FALSE,
                  message = paste("Content type", configuration$Content, "not available in current data")
                ))
              }

              content_rows <- which(current_metadata$Content == configuration$Content)
              if (length(content_rows) > 0) {
                available_samples <- character()
                if ("Sample" %in% names(current_metadata)) {
                  available_samples <- unique(current_metadata$Sample[content_rows])
                  available_samples <- available_samples[!is.na(available_samples) & nzchar(available_samples)]
                }
                if (length(available_samples) == 0 && "Options" %in% names(current_metadata)) {
                  available_samples <- unique(current_metadata$Options[content_rows])
                  available_samples <- available_samples[!is.na(available_samples) & nzchar(available_samples)]
                }

                if (length(available_samples) > 0) {
                  missing_numerator <- setdiff(configuration$Numerator[[1]], available_samples)
                  missing_denominator <- setdiff(configuration$Denominator[[1]], available_samples)

                  if (length(missing_numerator) > 0 || length(missing_denominator) > 0) {
                    return(list(
                      valid = FALSE,
                      message = paste("Sample groups not available:",
                                      if (length(missing_numerator) > 0) paste("Numerator:", paste(missing_numerator, collapse = ", ")),
                                      if (length(missing_denominator) > 0) paste("Denominator:", paste(missing_denominator, collapse = ", ")))
                    ))
                  }
                }
              }
            }
          }

          return(list(valid = TRUE, message = "Configuration is compatible"))
        }, error = function(e) {
          return(list(valid = FALSE, message = paste("Validation error:", e$message)))
        })
      },

      # Template compatibility check
      check_template_compatibility = function(template_configurations) {
        if (is.null(template_configurations) || nrow(template_configurations) == 0) {
          return(list(compatible = TRUE, message = "No configurations to check"))
        }

        compatibility_issues <- list()

        for (i in seq_len(nrow(template_configurations))) {
          config <- template_configurations[i, ]
          validation_result <- validate_configuration_compatibility(config)
          if (!validation_result$valid) {
            compatibility_issues[[config$Title]] <- validation_result$message
          }
        }

        if (length(compatibility_issues) == 0) {
          return(list(
            compatible = TRUE,
            message = paste("All", nrow(template_configurations), "configurations are compatible")
          ))
        } else {
          return(list(
            compatible = FALSE,
            message = paste("Compatibility issues found in", length(compatibility_issues), "configurations"),
            issues = compatibility_issues
          ))
        }
      },

      # Enhanced status functions for template integration
      get_configurations_summary = reactive({
        current_configs <- ratio_configurations_df()
        if (nrow(current_configs) == 0) {
          return(list(
            count = 0,
            methods = character(),
            content_types = character(),
            status = "No configurations"
          ))
        }

        return(list(
          count = nrow(current_configs),
          methods = if ("Statistics" %in% names(current_configs)) table(current_configs$Statistics) else character(),
          content_types = if ("Content" %in% names(current_configs)) table(current_configs$Content) else character(),
          names = current_configs$Title,
          status = "Configured"
        ))
      }),

      # Test module functionality
      test_module_processing = function() {
        debug_log("=== TESTING MODULE PROCESSING ===", 1)
        debug_log(paste("Processing status:", processing_status()), 1)
        debug_log(paste("Configuration count:", nrow(ratio_configurations_df())), 1)
        debug_log(paste("Error count:", length(processing_errors())), 1)
        debug_log(paste("Results count:", length(analysis_results())), 1)
        debug_log(paste("Debug level:", DEBUG_LEVEL), 1)

        missing_deps <- get_missing_dependencies()
        if (length(missing_deps) > 0) {
          debug_log("Missing dependencies:", 1)
          for (pkg in names(missing_deps)) {
            debug_log(paste("  -", pkg, "for:", paste(missing_deps[[pkg]], collapse = ", ")), 1)
          }
        }

        debug_log("=== END TESTING ===", 1)
      }
    ))
  })
}
