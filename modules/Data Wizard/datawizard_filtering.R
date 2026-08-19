# modules/Data Wizard/datawizard_filtering.R
#
# PURPOSE:
#   Orchestrator for the Data Wizard filtering module. Sources all sub-files,
#   defines the UI (modFilteringUI), wires the server (modFilteringServer),
#   and exposes the module's return interface to the rest of the application.
#
# ARCHITECTURE:
#   This file delegates concerns to five sub-files inside filtering/:
#     - datawizard_filtering_utils.R     : Shared utility / validation helpers
#     - datawizard_filtering_engine.R    : Pure filter logic (no Shiny dependency)
#     - datawizard_filtering_state.R     : Central reactive state factory
#     - datawizard_filtering_observers.R : All observe() / observeEvent() blocks
#     - datawizard_filtering_ui.R        : Dynamic UI outputs (renderUI, renderRHandsontable)
#                                          and show_filter_notification() helper
#
# STRUCTURE:
#   1. Source sub-files into modEnv
#   2. modFilteringUI()       - Full tab UI definition (static)
#   3. modFilteringServer()   - Server function:
#      a. Debug setup
#      b. State initialization (via create_filtering_state)
#      c. apply_imported_some_settings() helper
#      d. UI output registration (via register_filtering_ui_outputs)
#      e. perform_filtering() helper
#      f. Observer registration (via register_filtering_observers)
#      g. State export helpers (get_current_ui_values, get_current_filter_state_for_export)
#      h. Return interface
#
# RETURN INTERFACE (public API):
#   Triggers:  apply_filters_trigger, reset_filters_trigger
#   Functions: perform_filtering, apply_filter_state
#   Getters:   get_current_filter_state (primary), get_current_ui_values,
#              get_current_filter_state_for_export
#   Aliases:   get_current_filter_state_basic, get_filter_state
#              (probed by collect_module_ui_state in auto_assign adapters)
#
# NOTES FOR DEVELOPERS:
#   - perform_filtering() bridges reactive state and the pure engine; it is
#     passed to register_filtering_observers so observers can invoke it.
#   - show_filter_notification() lives in datawizard_filtering_ui.R and is
#     available via modEnv to both observers and the orchestrator.
#   - apply_imported_some_settings() handles SOME-logic import and is used
#     both internally and in the return interface's apply_filter_state().
#   - UI config export/import for SOME logic lives in datawizard_core.R, not here.

# Source utility functions, filter engine, reactive state, and observers
source("modules/Data Wizard/filtering/datawizard_filtering_utils.R", local = modEnv)
source("modules/Data Wizard/filtering/datawizard_filtering_engine.R", local = modEnv)
source("modules/Data Wizard/filtering/datawizard_filtering_state.R", local = modEnv)
source("modules/Data Wizard/filtering/datawizard_filtering_observers.R", local = modEnv)
source("modules/Data Wizard/filtering/datawizard_filtering_ui.R", local = modEnv)

############
# UI Module - Original Tab Structure Restored
############

modFilteringUI <- function(id) {
  ns <- NS(id)

  div(
    # Error display area
    uiOutput(ns("filter_errors")),

    tabsetPanel(
      id = ns("filter_tabs_dw"),

      tabPanel(
        title = "Confidence",
        br(),
        fluidRow(
          column(12,
                 div(
                   title = "Filter proteins based on their confidence scores. Use numeric filtering for FDR values or string filtering for confidence categories.",
                   h5("Protein Confidence Filtering")
                 ),

                 div(
                   title = "Enable to filter proteins based on numeric confidence values (typically FDR or p-values)",
                   checkboxInput(ns("numeric_fdr_dw"),
                                 "Enable Numeric Confidence Filtering",
                                 value = FALSE)
                 ),

                 conditionalPanel(
                   condition = paste0("input['", ns("numeric_fdr_dw"), "']"),
                   fluidRow(
                     column(6,
                            div(
                              title = "Minimum confidence threshold - proteins with values below this will be kept",
                              numericInput(ns("numeric_input_dw"),
                                           "Minimum Confidence Threshold:",
                                           value = 0.05,
                                           min = 0,
                                           max = 1,
                                           step = 0.001)
                            )
                     ),
                     column(6,
                            div(
                              title = "Maximum confidence threshold - proteins with values above this will be removed (optional)",
                              numericInput(ns("numeric_input_dw_max"),
                                           "Maximum Confidence Threshold:",
                                           value = NULL,
                                           min = 0,
                                           max = 1,
                                           step = 0.001)
                            )
                     )
                   )
                 ),

                 hr(),

                 div(
                   title = "Enable to filter proteins that contain specific text in their confidence columns",
                   checkboxInput(ns("string_fdr_dw"),
                                 "Enable String Confidence Filtering",
                                 value = FALSE)
                 ),

                 conditionalPanel(
                   condition = paste0("input['", ns("string_fdr_dw"), "']"),
                   div(
                     title = "Text pattern to search for and remove from the dataset",
                     textInput(ns("string_input_dw"),
                               "String to Remove:",
                               value = "",
                               placeholder = "Enter text pattern to filter out")
                   )
                 )
          )
        )
      ),

      tabPanel(
        title = "Valid Values",
        br(),
        fluidRow(
          column(12,
                 div(
                   title = "Filter proteins based on the number of valid (non-missing) abundance values they have across samples",
                   h5("Valid Value Filtering")
                 ),

                 div(
                   title = "Choose whether to require valid values in each group separately, at least one group, or across the entire dataset",
                   selectInput(ns("valid_filtering_group_dw"),
                               "Look for valid abundance values in:",
                               choices = c("In total" = "In total",
                                           "One group" = "One group",
                                           "Each group" = "Each group"),
                               selected = "In total",
                               width = "100%")
                 ),

                 # Add explanation for the modes
                 div(
                   style = "font-size: 0.9em; color: #666; margin-top: 5px;",
                   tags$strong("In total:"), " At least X valid values across all abundance columns", br(),
                   tags$strong("One group:"), " At least X valid values in at least one group", br(),
                   tags$strong("Each group:"), " At least X valid values in every group"
                 ),

                 div(
                   title = "Minimum number of valid (non-missing) values required for a protein to pass the filter",
                   numericInput(ns("valid_filtering_value_dw"),
                                "Minimum valid values:",
                                value = 1,
                                min = 0)
                 )
          )
        )
      ),

      tabPanel(
        title = "Custom",
        br(),
        fluidRow(
          column(12,
                 div(
                   title = "Create custom filters for any column in your dataset using comparison operators and logical combinations.",
                   h5("Custom Column Filtering")
                 ),

                 div(
                   fluidRow(
                     column(
                       6,
                       div(
                         title = "Select a content category from metadata to filter available columns",
                         selectInput(
                           ns("filter_category_dw"),
                           "Content Category:",
                           choices = NULL,
                           width = "100%"
                         )
                       )
                     ),
                     column(
                       6,
                       div(
                         title = "Select one or more columns within the chosen category (or all if no category selected)",
                         selectizeInput(
                           ns("filter_column_dw"),
                           "Columns:",
                           choices = NULL,
                           multiple = TRUE,
                           width = "100%",
                           options = list(
                             closeAfterSelect = FALSE,
                             plugins = list("remove_button")
                           )
                         )
                       )
                     )
                   )
                 ),

                 conditionalPanel(
                   condition = safe_sprintf(
                     "input['%s'] && input['%s'].length > 1",
                     ns("filter_column_dw"), ns("filter_column_dw")
                   ),
                   div(
                     conditionalPanel(
                       condition = safe_sprintf("input['%s'] && input['%s'].length > 1", ns("filter_column_dw"), ns("filter_column_dw")),
                       div(
                         style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; border-radius: 6px; margin: 8px 0;",
                         div(
                           HTML("<strong>Column Combination Logic:</strong>"),
                           style = "font-weight: bold; display: block; margin-bottom: 8px;"
                         ),
                         radioButtons(
                           ns("multi_column_logic"),
                           label = NULL,
                           choices = list(
                             "ANY column meets condition (OR logic)" = "OR",
                             "ALL columns meet condition (AND logic)" = "AND",
                             "SOME columns meet condition (Custom count)" = "SOME"
                           ),
                           selected = "OR",
                           inline = FALSE
                         ),

                         conditionalPanel(
                           condition = safe_sprintf("input['%s'] == 'SOME'", ns("multi_column_logic")),
                           div(
                             style = "background-color: #e3f2fd; border: 1px solid #90caf9; padding: 10px; border-radius: 4px; margin-top: 8px;",
                             h6("Custom Count Settings:", style = "margin-bottom: 8px; font-weight: bold;"),
                             fluidRow(
                               column(6,
                                      selectInput(
                                        ns("some_operator"),
                                        "Condition:",
                                        choices = list(
                                          "At least" = "at_least",
                                          "Less than" = "less_than",
                                          "Exactly" = "exactly"
                                        ),
                                        selected = "at_least"
                                      )
                               ),
                               column(6,
                                      numericInput(
                                        ns("some_count"),
                                        "Number of columns:",
                                        value = 1,
                                        min = 1,
                                        step = 1
                                      )
                               )
                             ),
                             div(id = ns("some_validation_message"), style = "margin-top: 5px;"),
                             tags$small(
                               HTML("<strong>Note:</strong> Determines how many selected columns must meet the filter condition.")
                             )
                           )
                         ),

                         conditionalPanel(
                           condition = safe_sprintf("input['%s'] != 'SOME'", ns("multi_column_logic")),
                           tags$small(
                             HTML(
                               "<strong>OR logic:</strong> Keep rows where at least one selected column meets the condition<br>
             <strong>AND logic:</strong> Keep rows where all selected columns meet the condition"
                             )
                           )
                         )
                       )
                     )
                   )
                 ),

                 ## 🔹 First Operator + First Value in ONE row
                 fluidRow(
                   column(6,
                          div(
                            title = "Choose comparison operator for the first condition",
                            uiOutput(ns("filter_operator_ui_dw_1"))
                          )
                   ),
                   column(6,
                          div(
                            title = "Enter the value to compare against for the first condition",
                            uiOutput(ns("filter_value_ui_dw_1"))
                          )
                   )
                 ),

                 br(),

                 ## 🔹 Combination Logic in its OWN row
                 div(
                   title = "Choose how to combine the first and second condition (if both are provided). EXCLUDE is available for string filters and keeps first-condition matches except rows matching the second condition.",
                   selectInput(ns("filter_logic_dw"),
                               "Combination Logic:",
                               choices = list("AND" = "AND", "OR" = "OR"),
                               selected = "AND")
                 ),
                 tags$small(
                   HTML(
                     "<strong>EXCLUDE:</strong> For string filters, keep rows matching the first condition and remove rows matching the second condition. With <em>Does not contain</em>, the second condition is treated as an exception to keep."
                   )
                 ),

                 br(),

                 ## 🔹 Second Operator + Second Value together in ONE row
                 fluidRow(
                   column(6,
                          div(
                            title = "Choose comparison operator for the second condition (optional)",
                            uiOutput(ns("filter_operator_ui_dw_2"))
                          )
                   ),
                   column(6,
                          div(
                            title = "Enter the value to compare against for the second condition (optional)",
                            uiOutput(ns("filter_value_ui_dw_2"))
                          )
                   )
                 ),

                 br(),

                 div(
                   title = "Choose how to handle empty or missing values in the selected columns",
                   selectInput(ns("filter_empty_dw"),
                               "Empty Value Handling:",
                               choices = list(
                                 "None" = "None",
                                 "Remove Empty" = "Remove Empty",
                                 "Keep Only Empty" = "Keep Only Empty"
                               ),
                               selected = "None")
                 ),

                 br(),

                 fluidRow(
                   column(6,
                          div(
                            title = "Add the current filter condition to your custom filter list",
                            actionButton(ns("add_filter_dw"),
                                         "Add to Queue",
                                         class = "btn-primary",
                                         width = "100%")
                          )
                   ),
                   column(6,
                          div(
                            title = "Remove all custom filter conditions",
                            actionButton(ns("clear_filters_dw"),
                                         "Clear Queue",
                                         class = "btn-default",
                                         style = "background-color: #3498db; border-color: #3498db; color: #fff;",
                                         width = "100%")
                          )
                   )
                 ),

                 br(),

                 uiOutput(ns("custom_filters_block"))
          )
        )
      )
    ),

    hr(),

    # Apply and Reset Section
    fluidRow(
      column(12,
             div(
               title = "Apply all configured filters to the data",
               actionButton(ns("apply_all_filters"),
                            "Apply All Filters",
                            class = "btn-success",
                            style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                            width = "100%")
             )
      )
    )
  )
}

############
# Server Module
############

modFilteringServer <- function(id, data, metadata_def, init_meta = NULL, UI_config = reactive(NULL),
                               metadata_ready_status = reactive(TRUE),
                               session_restore_trigger = reactive(NULL),
                               debug_level = 0,
                               primary_working_revision_debounced = reactive(NULL),
                               metadata_revision_debounced = reactive(NULL),
                               data_revision_signature = reactive(NULL),
                               metadata_assignment_pending = reactive(FALSE),
                               metadata_meaningful_ready = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management
    # ========================================

    # Helper function for controlled debug output
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "FILTERING", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ FILTERING ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Filtering module server starting", 1)

    # ========================================
    # Reactive State Initialization
    # (All state is created centrally in datawizard_filtering_state.R)
    # ========================================

    state <- create_filtering_state(
      input                = input,
      session              = session,
      data                 = data,
      metadata_def         = metadata_def,
      metadata_ready_status = metadata_ready_status,
      UI_config            = UI_config,
      debug_log            = debug_log,
      DEBUG_LEVEL          = DEBUG_LEVEL
    )

    # Local aliases for backward compatibility within this file.
    # These allow existing code to reference the same names as before.
    filter_state            <- state$filter_state
    apply_filters_trigger   <- state$apply_filters_trigger
    reset_filters_trigger   <- state$reset_filters_trigger
    filterChoicesCache      <- state$filterChoicesCache
    filterCategoryCache     <- state$filterCategoryCache
    rv_filter               <- state$rv_filter
    config                  <- state$config
    config_applied          <- state$config_applied
    filtering_ui_active     <- state$filtering_ui_active
    selected_filter_columns <- state$selected_filter_columns
    metadata_ready          <- state$metadata_ready
    filtered_conditions_dw  <- state$filtered_conditions_dw
    some_operator           <- state$some_operator
    some_count              <- state$some_count
    selected_type           <- state$selected_type

    # Function to handle SOME settings import:
    apply_imported_some_settings <- function(some_settings) {
      tryCatch({
        if (is.null(some_settings) || !is.list(some_settings)) {
          debug_log("No SOME settings to import", 2)
          return()
        }

        debug_log("Importing SOME settings", 2)

        # Apply multi-column logic
        if (!is.null(some_settings$multi_column_logic)) {
          validated_logic <- safe_character_check(some_settings$multi_column_logic, "OR")
          if (validated_logic %in% c("OR", "AND", "SOME")) {
            updateRadioButtons(session, "multi_column_logic", selected = validated_logic)
            debug_log(paste("Imported multi_column_logic:", validated_logic), 2)
          }
        }

        # Apply SOME operator
        if (!is.null(some_settings$some_operator)) {
          validated_operator <- safe_character_check(some_settings$some_operator, "at_least")
          if (validated_operator %in% c("at_least", "less_than", "exactly")) {
            updateSelectInput(session, "some_operator", selected = validated_operator)
            debug_log(paste("Imported some_operator:", validated_operator), 2)
          }
        }

        # Apply SOME count
        if (!is.null(some_settings$some_count)) {
          validated_count <- safe_numeric_check(some_settings$some_count, min_val = 1, default_val = 1)
          updateNumericInput(session, "some_count", value = validated_count)
          debug_log(paste("Imported some_count:", validated_count), 2)
        }

        debug_log("SOME settings imported successfully", 1)

      }, error = function(e) {
        debug_log(paste("Error importing SOME settings:", e$message), 1)
      })
    }

    # ========================================
    # Register all dynamic UI outputs
    # (Defined in datawizard_filtering_ui.R)
    # ========================================

    register_filtering_ui_outputs(
      input         = input,
      output        = output,
      session       = session,
      ns            = ns,
      filter_state  = filter_state,
      selected_type = selected_type,
      debug_log     = debug_log,
      filtering_ui_active = filtering_ui_active
    )

    # ========================================
    # Helper Functions
    # ========================================

    # Enhanced filtering function with safety checks
    perform_filtering <- function(source = "central") {
      tryCatch({
        current_data <- tryCatch({ data() }, error = function(e) NULL)
        current_metadata <- tryCatch({ metadata_def() }, error = function(e) NULL)

        if (is.null(current_data) || is.null(current_metadata)) {
          return(list(success = FALSE, errors = "No data or metadata available"))
        }

        metadata_status <- tryCatch({ metadata_ready() }, error = function(e) TRUE)

        result <- apply_all_filters_function_improved(
          current_data,
          current_metadata,
          filter_state,
          filter_state$custom_conditions,
          metadata_status,
          DEBUG_LEVEL,
          debug_log = debug_log
        )

        filter_state$filter_results <- list(
          success = safe_logical_check(result$success),
          rows_original = as.numeric(result$rows_original %||% 0),
          rows_filtered = as.numeric(result$rows_filtered %||% 0),
          rows_removed = as.numeric(result$rows_removed %||% 0),
          errors = result$errors %||% character(),
          warnings = result$warnings %||% character()
        )

        # Per-tab level-0 logs (Confidence, Valid Values, Custom) are emitted
        # inside apply_all_filters_function_improved(); keep the aggregate log
        # at level 1 so it stays in the verbose stream rather than doubling up
        # the Essential channel.
        if (isTRUE(filter_state$filter_results$success)) {
          debug_log(paste("Filter run completed: removed",
                          filter_state$filter_results$rows_removed, "rows \u2013",
                          filter_state$filter_results$rows_filtered,
                          "remain. Source =", source), 1)
        }

        return(result)

      }, error = function(e) {
        debug_log(paste("Critical error in perform_filtering:", e$message), 1)
        return(list(
          success = FALSE,
          errors = paste("Critical error:", e$message)
        ))
      })
    }

    # ========================================
    # Register all observers
    # (Defined in datawizard_filtering_observers.R)
    # ========================================

    register_filtering_observers(
      input                = input,
      output               = output,
      session              = session,
      ns                   = ns,
      filter_state         = filter_state,
      apply_filters_trigger = apply_filters_trigger,
      reset_filters_trigger = reset_filters_trigger,
      filtered_conditions_dw = filtered_conditions_dw,
      filterChoicesCache   = filterChoicesCache,
      filterCategoryCache  = filterCategoryCache,
      config               = config,
      config_applied       = config_applied,
      metadata_ready       = metadata_ready,
      some_operator        = some_operator,
      some_count           = some_count,
      selected_type        = selected_type,
      data                 = data,
      metadata_def         = metadata_def,
      perform_filtering    = perform_filtering,
      show_filter_notification = show_filter_notification,
      debug_log            = debug_log,
      DEBUG_LEVEL          = DEBUG_LEVEL,
      filtering_ui_active = filtering_ui_active,
      primary_working_revision_debounced = primary_working_revision_debounced,
      metadata_revision_debounced = metadata_revision_debounced,
      data_revision_signature = data_revision_signature,
      metadata_assignment_pending = metadata_assignment_pending,
      metadata_meaningful_ready = metadata_meaningful_ready
    )

    # ========================================
    # State Export Helpers
    # ========================================

    # Get current UI input values (includes SOME settings)
    get_current_ui_values <- function() {
      tryCatch({
        isolate({
          ui_values <- list(
            confidence = list(
              numeric_enabled = safe_logical_check(input$numeric_fdr_dw),
              string_enabled = safe_logical_check(input$string_fdr_dw),
              numeric_max = input$numeric_input_dw_max,
              numeric_min = input$numeric_input_dw,
              string_input = safe_character_check(input$string_input_dw)
            ),
            valid_values = list(
              group_selection = safe_character_check(input$valid_filtering_group_dw, "In total"),
              min_count = as.numeric(input$valid_filtering_value_dw %||% 1)
            ),
            custom = filter_state$custom_conditions,
            some_settings = list(
              multi_column_logic = safe_character_check(input$multi_column_logic, "OR"),
              some_operator = safe_character_check(input$some_operator, "at_least"),
              some_count = safe_numeric_check(input$some_count, min_val = 1, default_val = 1)
            )
          )

          debug_log("Exported UI values including SOME settings", 2)
          return(ui_values)
        })
      }, error = function(e) {
        debug_log(paste("Error exporting UI values:", e$message), 1)

        return(list(
          confidence = filter_state$confidence,
          valid_values = filter_state$valid_values,
          custom = filter_state$custom_conditions,
          some_settings = list(
            multi_column_logic = "OR",
            some_operator = "at_least",
            some_count = 1
          )
        ))
      })
    }

    # Get filter state for export (without SOME settings -- used by auto-assign)
    get_current_filter_state_for_export <- function() {
      state <- get_current_ui_values()
      state$some_settings <- NULL
      return(state)
    }

    # ========================================
    # Session Cleanup
    # ========================================

    cleanup_manager$register_module("Filtering", function() {
      debug_log("Executing [Filtering] cleanup", 2)

      filter_state$confidence <- list(
        numeric_enabled = FALSE,
        string_enabled  = FALSE,
        numeric_max     = NULL,
        numeric_min     = NULL,
        string_input    = NULL
      )
      filter_state$valid_values <- list(
        group_selection = "In total",
        min_count       = 1
      )
      filter_state$custom_conditions <- data.frame(
        Column             = character(),
        Operator_1         = character(),
        Value_1            = character(),
        Logic              = character(),
        Operator_2         = character(),
        Value_2            = character(),
        Empty_Filter       = character(),
        Multi_Column_Logic = character(),
        stringsAsFactors   = FALSE
      )
      filter_state$filter_results <- list(
        success       = FALSE,
        rows_original = 0,
        rows_filtered = 0,
        rows_removed  = 0,
        errors        = character(),
        warnings      = character()
      )
      filter_state$errors        <- character()
      filter_state$preview_active <- FALSE
      filter_state$processing    <- FALSE

      apply_filters_trigger(0)
      reset_filters_trigger(0)
      filterChoicesCache(character(0))
      filterCategoryCache("")
      rv_filter$last_choices  <- character(0)
      rv_filter$last_selected <- character(0)
      rv_filter$last_category <- ""
      config_applied(FALSE)
      filtering_ui_active(FALSE)

      debug_log("[Filtering] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    filtering_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        numeric_fdr_dw           = "checkboxInput",
        string_fdr_dw            = "checkboxInput",
        numeric_input_dw         = "numericInput",
        numeric_input_dw_max     = "numericInput",
        string_input_dw          = "selectizeInput",
        valid_filtering_group_dw = "selectInput",
        valid_filtering_value_dw = "numericInput",
        filter_category_dw       = "selectInput",
        filter_column_dw         = "selectInput",
        multi_column_logic       = "radioButtons"
      ),
      module_label = "Filtering",
      # Persist the custom-conditions queue (the DT-backed table the user
      # builds up before hitting Apply) so a session save+restore round-trip
      # leaves the queue intact.
      get_extra = function() {
        list(custom_conditions = tryCatch(isolate(filter_state$custom_conditions),
                                           error = function(e) NULL))
      },
      apply_extra = function(extra) {
        if (is.list(extra) && is.data.frame(extra$custom_conditions)) {
          filtering_ui_active(TRUE)
          filter_state$custom_conditions <- extra$custom_conditions
          debug_log(paste("[Filtering] restored custom_conditions queue with",
                          nrow(extra$custom_conditions), "rows"), 2)
        }
      },
      restore_trigger = session_restore_trigger
    )

    # ========================================
    # Return Interface
    # ========================================

    return(list(
      # Triggers
      apply_filters_trigger = apply_filters_trigger,
      reset_filters_trigger = reset_filters_trigger,

      # Session-restore bridge
      get_session_state = filtering_session_state$get_session_state,
      set_session_state = filtering_session_state$set_session_state,

      # Core function
      perform_filtering = perform_filtering,

      # State export -- primary API used by datawizard_export.R and auto_assign
      get_current_filter_state = function() {
        tryCatch({
          ui_values <- get_current_ui_values()
          debug_log("Returned UI values for export", 2)
          return(ui_values)
        }, error = function(e) {
          debug_log(paste("Error getting UI values, using internal state:", e$message), 1)
          return(isolate(list(
            confidence = filter_state$confidence,
            valid_values = filter_state$valid_values,
            custom = filter_state$custom_conditions
          )))
        })
      },

      get_current_ui_values = get_current_ui_values,
      get_current_filter_state_for_export = get_current_filter_state_for_export,

      # Fallback aliases probed by collect_module_ui_state() in auto_assign adapters
      get_current_filter_state_basic = get_current_ui_values,
      get_filter_state = get_current_ui_values,

      # State import -- used by auto_assign to restore saved templates
      apply_filter_state = function(new_state) {
        tryCatch({
          debug_log("Applying imported filter state", 1)
          filtering_ui_active(TRUE)

          if (!is.null(new_state$confidence)) {
            debug_log("Applying confidence state", 2)
            filter_state$confidence <- new_state$confidence

            session$onFlushed(function() {
              tryCatch({
                conf <- new_state$confidence

                if (!is.null(conf$numeric_enabled)) {
                  updateCheckboxInput(session, "numeric_fdr_dw", value = isTRUE(conf$numeric_enabled))
                }
                if (!is.null(conf$string_enabled)) {
                  updateCheckboxInput(session, "string_fdr_dw", value = isTRUE(conf$string_enabled))
                }
                if (!is.null(conf$numeric_max) && is.numeric(conf$numeric_max)) {
                  updateNumericInput(session, "numeric_input_dw_max", value = conf$numeric_max)
                }
                if (!is.null(conf$numeric_min) && is.numeric(conf$numeric_min)) {
                  updateNumericInput(session, "numeric_input_dw", value = conf$numeric_min)
                }
                if (!is.null(conf$string_input) && is.character(conf$string_input)) {
                  updateTextInput(session, "string_input_dw", value = conf$string_input)
                }
              }, error = function(e) {
                debug_log(paste("Error applying confidence UI updates:", e$message), 1)
              })
            })
          }

          if (!is.null(new_state$valid_values)) {
            debug_log("Applying valid values state", 2)
            filter_state$valid_values <- new_state$valid_values

            session$onFlushed(function() {
              tryCatch({
                valid <- new_state$valid_values

                if (!is.null(valid$group_selection) && is.character(valid$group_selection)) {
                  valid_choices <- c("In total", "One group", "Each group")
                  if (valid$group_selection %in% valid_choices) {
                    updateSelectInput(session, "valid_filtering_group_dw", selected = valid$group_selection)
                  }
                }
                if (!is.null(valid$min_count) && is.numeric(valid$min_count)) {
                  updateNumericInput(session, "valid_filtering_value_dw", value = valid$min_count)
                }
              }, error = function(e) {
                debug_log(paste("Error applying valid values UI updates:", e$message), 1)
              })
            })
          }

          if (!is.null(new_state$custom) && is.data.frame(new_state$custom)) {
            debug_log(paste("Applying", nrow(new_state$custom), "custom filters"), 2)

            custom_filters <- new_state$custom
            if (!"Some_Operator" %in% names(custom_filters)) {
              custom_filters$Some_Operator <- NA_character_
            }
            if (!"Some_Count" %in% names(custom_filters)) {
              custom_filters$Some_Count <- NA_real_
            }

            filter_state$custom_conditions <- custom_filters
          }

          if (!is.null(new_state$some_settings)) {
            debug_log("Applying SOME settings", 2)
            session$onFlushed(function() {
              apply_imported_some_settings(new_state$some_settings)
            })
          }

          debug_log("Filter state applied successfully", 1)
          return(TRUE)

        }, error = function(e) {
          debug_log(paste("Error applying filter state:", e$message), 1)
          show_filter_notification("Error applying imported filter settings", "error", "Import", session, DEBUG_LEVEL)
          return(FALSE)
        })
      }
    ))
  })
}
