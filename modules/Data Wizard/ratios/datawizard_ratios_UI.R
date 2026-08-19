# Data Wizard/ratios/datawizard_ratios_UI.R

#' Ratios UI Submodule
#' @param id namespace id
#' @export
ratiosUISubmodule <- function(id) {
  ns <- NS(id)

  div(
    h4("Custom Ratios & Statistical Analysis"),

    div(
      class = "custom-ratios-container",

      # Data source selection
      fluidRow(
        column(12,
               div(
                 title = "Choose which type of abundance data to use for ratio calculations. Imputed data includes filled-in missing values.",
                 selectizeInput(
                   ns("custom_col_sel_dw"),
                   "Select abundance data:",
                   choices = c(
                     "Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance",
                     "Imputed Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
                     "Imputed Batch Corrected Abundance",
                     "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance"
                   ),
                   selected = "Normalized Abundance"
                 )
               )
        )
      ),

      # Sample group selection
      fluidRow(
        column(6,
               div(
                 title = "Select sample groups that will be in the numerator (top) of the ratio calculation",
                 selectizeInput(
                   ns("numerator_sel_dw"),
                   "Select numerator groups:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Choose numerator groups...")
                 )
               )),
        column(6,
               div(
                 title = "Select sample groups that will be in the denominator (bottom) of the ratio calculation",
                 selectizeInput(
                   ns("denominator_sel_dw"),
                   "Select denominator groups:",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Choose denominator groups...")
                 )
               ))
      ),

      # Validation criteria
      fluidRow(
        column(6,
               div(
                 title = "Minimum number of valid (non-missing) measurements required for a protein to be included in the analysis",
                 numericInput(
                   ns("valid_comparison_sel_dw"),
                   "Minimum valid values:",
                   value = 1,
                   min = 0
                 )
               )),
        column(6,
               div(
                 title = "How to apply the minimum valid values requirement across treatment groups",
                 selectInput(
                   ns("valid_compgroup_sel_dw"),
                   "Valid values required in:",
                   choices = c("In total", "One group", "Each group"),
                   selected = "In total"
                 )
               ))
      ),

      # Statistical analysis options
      fluidRow(
        column(6,
               div(
                 title = "Statistical method to use for calculating p-values and significance testing",
                 selectizeInput(
                   ns("statistics_sel_dw"),
                   "Statistical method:",
                   choices = c(
                     "Student's T-Test", "Welch's T-Test", "Moderated Welch Test", "Limma", "DEqMS",
                     "Mann-Whitney U Test"
                   ),
                   selected = "Limma"
                 )
               )),
        column(6,
               div(
                 title = "Method for adjusting p-values to control for multiple comparisons",
                 selectizeInput(
                   ns("adjust_sel_dw"),
                   "p-value adjustment:",
                   choices = c(
                     "Bonferroni", "FDR", "Holm", "Hochberg", "Hommel",
                     "Benjamini & Hochberg", "Benjamini & Yekutieli"
                   ),
                   selected = "FDR"
                 )
               ))
      ),

      # Comparison name
      fluidRow(
        column(12,
               div(
                 title = "Provide a unique name for this comparison. This will be used as prefix for the new columns",
                 textInput(
                   ns("name_sel_dw"),
                   "Comparison name (must be unique):",
                   placeholder = "e.g., Treatment_vs_Control"
                 )
               )
        )
      ),

      # Action buttons
      fluidRow(
        column(6,
               actionButton(
                 ns("add_ratio_dw"),
                 "Add Ratio",
                 width = "100%",
                 class = "btn-primary"
               )),

        column(6,
               actionButton(
                 ns("clear_ratio_dw"),
                 "Clear Queue",
                 width = "100%",
                 class = "btn-default",
                 style = "background-color: #3498db; border-color: #3498db; color: #fff;"
               ))
      ),
      fluidRow(
        column(12,
               actionButton(
                 ns("apply_ratios_dw"),
                 "Apply Queue",
                 width = "100%",
                 class = "btn-success",
                 style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;"
               ))
      ),

      # Current ratios table
      uiOutput(ns("ratios_queue_block")),

      br()
      #
      # # Status output
      # div(
      #   class = "well",
      #   h6("Ratio Analysis Status"),
      #   verbatimTextOutput(ns("ratio_status"))
      # )
    )
  )
}

#' Ratios UI Server Submodule
#' @param id namespace id
#' @param ratio_configs reactive configurations data frame
#' @param processing_status reactive processing status
#' @param analysis_results reactive analysis results
#' @param processing_log_entries reactive log entries
#' @param ui_config_applied reactive UI config status
#' @param ui_config_source_info reactive UI config source
#' @param data_def reactive metadata definition
#' @param available_samples reactive available samples
#' @param UI_config reactive UI configuration
#' @param parent_session parent session for updateTextInput
#' @export
ratiosUIServer <- function(id, ratio_configs, processing_status, analysis_results,
                           processing_log_entries, ui_config_applied, ui_config_source_info,
                           data_def = NULL, available_samples = NULL, UI_config = NULL,
                           get_data = NULL,
                           parent_session = NULL,
                           session_restore_trigger = reactive(NULL),
                           metadata_revision_debounced = reactive(NULL),
                           primary_working_revision_debounced = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic UI Updates based on metadata
    refresh_ratio_choices <- function() {
      if (!is.null(data_def)) {
        loaded_data <- if (is.function(get_data)) isolate(get_data()) else NULL
        if (!is.data.frame(loaded_data)) {
          return()
        }

        metadata <- isolate(data_def())
        if (!is.data.frame(metadata) || !("Content" %in% names(metadata))) {
          return()
        }

        tryCatch({
          content_types <- unique(metadata$Content)

          # ========================================
          # PART 1: Data Type Choices
          # ========================================

          # Supported abundance types, in display order
          valid_choices <- c(
            "Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance",
            "Imputed Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
            "Imputed Batch Corrected Abundance",
            "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance"
          )
          available_choices <- valid_choices[valid_choices %in% content_types]
          if (length(available_choices) == 0) return()

          # Preserve current selection
          current_data_selection <- isolate(input$custom_col_sel_dw)
          final_data_selection <- if (!is.null(current_data_selection) && current_data_selection %in% available_choices) {
            current_data_selection
          } else {
            available_choices[1]
          }

          # ========================================
          # PART 2: Sample Choices
          # ========================================

          content_rows <- which(metadata$Content == final_data_selection)
          unique_samples <- character(0)

          if (length(content_rows) > 0) {
            if ("Sample" %in% names(metadata)) {
              sample_names <- metadata$Sample[content_rows]
              sample_names <- sample_names[!is.na(sample_names) & nzchar(trimws(sample_names))]
              if (length(sample_names) > 0) {
                unique_samples <- unique(trimws(sample_names))
              }
            }

            # Fallback to Options
            if (length(unique_samples) == 0 && "Options" %in% names(metadata)) {
              option_names <- metadata$Options[content_rows]
              option_names <- option_names[!is.na(option_names) & nzchar(trimws(option_names))]
              if (length(option_names) > 0) {
                unique_samples <- unique(trimws(option_names))
              }
            }
          }

          # available_samples fallback
          if (length(unique_samples) == 0 && !is.null(available_samples)) {
            fallback_samples <- tryCatch(available_samples(), error = function(e) character(0))
            if (length(fallback_samples) > 0) {
              unique_samples <- fallback_samples
            }
          }

          # Preserve sample selections
          current_numerator <- isolate(input$numerator_sel_dw)
          current_denominator <- isolate(input$denominator_sel_dw)

          preserved_numerator <- intersect(current_numerator %||% character(0), unique_samples)
          preserved_denominator <- intersect(current_denominator %||% character(0), unique_samples)

          # ========================================
          # ALL UPDATES AT ONCE
          # ========================================

          updateSelectizeInput(session, "custom_col_sel_dw",
                               choices = available_choices,
                               selected = final_data_selection,
                               server = TRUE)

          updateSelectizeInput(session, "numerator_sel_dw",
                               choices = unique_samples,
                               selected = preserved_numerator,
                               server = TRUE)

          updateSelectizeInput(session, "denominator_sel_dw",
                               choices = unique_samples,
                               selected = preserved_denominator,
                               server = TRUE)

        }, error = function(e) {
          message <- conditionMessage(e)
          if (!nzchar(message) || inherits(e, "shiny.silent.error")) {
            return()
          }
          debug_log(paste("[Ratios] Error updating UI choices:", message), level = 1)
          showNotification(paste("Error updating UI choices:", message), type = "error", duration = 8)
        })
      }
    }

    observeEvent(list(metadata_revision_debounced(), primary_working_revision_debounced()), {
      refresh_ratio_choices()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Update sample choices when data type selection changes
    observeEvent(input$custom_col_sel_dw, {
      if (!is.null(data_def) && !is.null(input$custom_col_sel_dw)) {
        metadata <- data_def()
        if (!is.null(metadata)) {
          content_rows <- which(metadata$Content == input$custom_col_sel_dw)
          unique_samples <- character(0)

          if (length(content_rows) > 0) {
            if ("Sample" %in% names(metadata)) {
              sample_names <- metadata$Sample[content_rows]
              sample_names <- sample_names[!is.na(sample_names) & nzchar(trimws(sample_names))]
              if (length(sample_names) > 0) {
                unique_samples <- unique(trimws(sample_names))
              }
            }

            if (length(unique_samples) == 0 && "Options" %in% names(metadata)) {
              option_names <- metadata$Options[content_rows]
              option_names <- option_names[!is.na(option_names) & nzchar(trimws(option_names))]
              if (length(option_names) > 0) {
                unique_samples <- unique(trimws(option_names))
              }
            }
          }

          if (length(unique_samples) == 0 && !is.null(available_samples)) {
            fallback_samples <- tryCatch(available_samples(), error = function(e) character(0))
            if (length(fallback_samples) > 0) {
              unique_samples <- fallback_samples
            }
          }

          updateSelectizeInput(session, "numerator_sel_dw", choices = unique_samples, server = TRUE)
          updateSelectizeInput(session, "denominator_sel_dw", choices = unique_samples, server = TRUE)
        }
      }
    }, ignoreInit = TRUE)

    # UI_config Integration Observer
    #
    # Guard every updateSelectizeInput selection with is_applyable(): if the
    # saved UI config carries a closure or any non-atomic leaf (e.g. a stale
    # reactive snapshot), skip it with a debug_log entry instead of letting
    # it reach updateSelectizeInput() -> "cannot coerce type 'closure' to
    # vector of type 'character'".
    is_applyable <- function(v) {
      !is.function(v) && (is.null(v) || is.atomic(v))
    }
    observeEvent(UI_config(), {
      if (!is.null(UI_config)) {
        ui_config_data <- UI_config()

        if (!is.null(ui_config_data) && !ui_config_applied()) {
          tryCatch({
            # Apply ratio settings if available
            if (!is.null(ui_config_data$ratio_settings)) {
              settings <- ui_config_data$ratio_settings

              if (!is.null(settings$custom_col_sel)) {
                if (is_applyable(settings$custom_col_sel)) {
                  updateSelectizeInput(session, "custom_col_sel_dw",
                                       selected = settings$custom_col_sel)
                } else {
                  debug_log("[Ratios] restore: skipping non-scalar field custom_col_sel", level = 1)
                }
              }

              if (!is.null(settings$statistics_sel)) {
                if (is_applyable(settings$statistics_sel)) {
                  updateSelectizeInput(session, "statistics_sel_dw",
                                       selected = settings$statistics_sel)
                } else {
                  debug_log("[Ratios] restore: skipping non-scalar field statistics_sel", level = 1)
                }
              }

              if (!is.null(settings$adjust_sel)) {
                if (is_applyable(settings$adjust_sel)) {
                  updateSelectizeInput(session, "adjust_sel_dw",
                                       selected = settings$adjust_sel)
                } else {
                  debug_log("[Ratios] restore: skipping non-scalar field adjust_sel", level = 1)
                }
              }
            }

          }, error = function(e) {
            debug_log(paste("[Ratios] Error applying UI configuration:", e$message), level = 1)
            showNotification(
              paste("Error applying UI configuration:", e$message),
              type = "error", duration = 8
            )
          })
        }
      }
    }, ignoreInit = TRUE)

    # Render custom ratios table
    output$custom_RatioTable_dw <- DT::renderDT({
      tryCatch({
        configuration_df <- ratio_configs()

        if (is.null(configuration_df) || !is.data.frame(configuration_df) || nrow(configuration_df) == 0) {
          # Do not render an empty table; the container is also not created
          return(NULL)
        }

        # Create display version
        display_df <- configuration_df
        display_df$Numerator <- sapply(configuration_df$Numerator, function(x) {
          if (is.null(x) || length(x) == 0) "" else paste(x, collapse = ", ")
        })
        display_df$Denominator <- sapply(configuration_df$Denominator, function(x) {
          if (is.null(x) || length(x) == 0) "" else paste(x, collapse = ", ")
        })

        DT::datatable(
          display_df,
          options = list(
            dom = 't',
            pageLength = 10,
            scrollX = TRUE,
            scrollY = "200px",
            paging = FALSE,
            info = FALSE,
            searching = FALSE
          ),
          rownames = FALSE
        )

      }, error = function(e) {
        error_df <- data.frame(Error = "Table rendering failed")
        return(DT::datatable(error_df, options = list(dom = 't'), rownames = FALSE))
      })
    })

    # Build the queued ratios block (heading + table) only when there is content
    output$ratios_queue_block <- renderUI({
      tryCatch({
        configuration_df <- ratio_configs()
        if (is.null(configuration_df) || !is.data.frame(configuration_df) || nrow(configuration_df) == 0) {
          return(NULL)  # no block, no reserved space
        }
        tagList(
          h5("Queued Custom Ratios:"),
          DT::DTOutput(ns("custom_RatioTable_dw"), width = "100%", height = "200px")
        )
      }, error = function(e) {
        debug_log(paste("Error building ratios_queue_block:", e$message), 1)
        NULL
      })
    })


    # # Render status output
    # output$ratio_status <- renderText({
    #   tryCatch({
    #     status_lines <- character()
    #
    #     config_count <- nrow(ratio_configs())
    #     status_lines <- c(status_lines, paste("Configured ratios:", config_count))
    #
    #     if (config_count > 0) {
    #       ratio_names <- ratio_configs()$Title
    #       status_lines <- c(status_lines, paste("Names:", paste(ratio_names, collapse = ", ")))
    #     }
    #
    #     current_status <- processing_status()
    #     if (current_status != "idle") {
    #       status_lines <- c(status_lines, paste("Processing status:", current_status))
    #     }
    #
    #     current_results <- analysis_results()
    #     if (length(current_results) > 0) {
    #       status_lines <- c(status_lines, paste("Applied analyses:", length(current_results)))
    #     }
    #
    #     if (ui_config_applied()) {
    #       status_lines <- c(status_lines, paste("UI config applied from:", ui_config_source_info()))
    #     }
    #
    #     return(paste(status_lines, collapse = "\n"))
    #
    #   }, error = function(e) {
    #     return(paste("Status display error:", e$message))
    #   })
    # })

    session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        custom_col_sel_dw       = "selectizeInput",
        numerator_sel_dw        = "selectizeInput",
        denominator_sel_dw      = "selectizeInput",
        valid_comparison_sel_dw = "selectizeInput",
        valid_compgroup_sel_dw  = "selectizeInput",
        statistics_sel_dw       = "selectizeInput",
        adjust_sel_dw           = "selectizeInput",
        name_sel_dw             = "textInput"
      ),
      module_label = "Ratios",
      get_extra = function() {
        list(ratio_configurations = tryCatch(isolate(ratio_configs()),
                                             error = function(e) NULL))
      },
      apply_extra = function(extra) {
        if (!is.list(extra) || !is.data.frame(extra$ratio_configurations)) {
          return(invisible(NULL))
        }
        df <- extra$ratio_configurations
        tryCatch({
          if (!is.null(df$Numerator) && !is.list(df$Numerator)) {
            df$Numerator <- I(as.list(df$Numerator))
          }
          if (!is.null(df$Denominator) && !is.list(df$Denominator)) {
            df$Denominator <- I(as.list(df$Denominator))
          }
          ratio_configs(df)
          debug_log(paste("[Ratios] restored ratio_configurations queue with",
                          nrow(df), "rows"), level = 2)
        }, error = function(e) {
          debug_log(paste("[Ratios] apply_extra failed:", e$message), level = 1)
        })
      },
      restore_trigger = session_restore_trigger
    )

    # Return list with reactive values instead of functions
    list(
      # Direct reactive values for inputs
      custom_col_sel_dw = reactive(input$custom_col_sel_dw),
      numerator_sel_dw = reactive(input$numerator_sel_dw),
      denominator_sel_dw = reactive(input$denominator_sel_dw),
      valid_comparison_sel_dw = reactive(input$valid_comparison_sel_dw),
      valid_compgroup_sel_dw = reactive(input$valid_compgroup_sel_dw),
      statistics_sel_dw = reactive(input$statistics_sel_dw),
      adjust_sel_dw = reactive(input$adjust_sel_dw),
      name_sel_dw = reactive(input$name_sel_dw),

      # Button click events as reactive values
      add_ratio_clicked = reactive(input$add_ratio_dw),
      apply_ratios_clicked = reactive(input$apply_ratios_dw),
      clear_ratio_clicked = reactive(input$clear_ratio_dw),

      # Function to reset name field
      reset_name_field = function() {
        updateTextInput(session, "name_sel_dw", value = "")
      },

      # Session-restore bridge
      get_session_state = session_state$get_session_state,
      set_session_state = session_state$set_session_state
    )
  })
}
