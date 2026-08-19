# ==============================================================================
# File: modules/Data Wizard/filtering/datawizard_filtering_state.R
#
# Purpose:
#   Centralizes ALL reactive state for the filtering module in one place.
#   This includes reactiveValues containers, reactiveVal triggers and caches,
#   derived reactives (debounced column selection, column type detection,
#   metadata readiness, SOME logic reactives, config unwrapping).
#
# Architectural Role:
#   State layer of the filtering module. Called once from modFilteringServer
#   during initialization. Returns a named list that observers, UI renderers,
#   and the return interface all reference.
#
# Structure:
#   1. create_filtering_state() -- factory function that builds all reactive
#      containers and derived reactives inside the caller's moduleServer closure.
#      Parameters provide access to input, session, and module arguments.
#
# Dependencies:
#   - Shiny (reactiveValues, reactiveVal, reactive, debounce).
#   - Utility functions from datawizard_filtering_utils.R: safe_character_check,
#     safe_numeric_check.
#   - debug_log must exist in the calling environment.
#
# Notes for future developers:
#   - This file is purely declarative. It contains NO observers, NO renderUI,
#     NO side effects. It only creates reactive containers and wires derived
#     reactives.
#   - The returned list is the single source of truth for all filtering state.
#     Do not create additional reactive state outside this file.
#   - Ordering of reactive declarations matters: derived reactives must be
#     defined after the reactives they depend on.
#   - The function must be called INSIDE moduleServer() so that `input` and
#     `session` are available in the closure.
# ==============================================================================


#' Create all reactive state for the filtering module.
#'
#' @param input Shiny input object (from moduleServer closure).
#' @param session Shiny session object.
#' @param data Reactive expression returning the current data frame.
#' @param metadata_def Reactive expression returning the metadata data frame.
#' @param metadata_ready_status Reactive or logical indicating metadata readiness.
#' @param UI_config Reactive or value providing auto-assign UI configuration.
#' @param debug_log Logging function with signature (message, level).
#' @param DEBUG_LEVEL Numeric debug verbosity level.
#' @return A named list of all reactive containers and derived reactives.
create_filtering_state <- function(input, session, data, metadata_def,
                                   metadata_ready_status, UI_config,
                                   debug_log, DEBUG_LEVEL) {

  # --------------------------------------------------------------------------
  # Primary state container
  # --------------------------------------------------------------------------

  filter_state <- shiny::reactiveValues(
    # Confidence filter configuration
    confidence = list(
      numeric_enabled = FALSE,
      string_enabled = FALSE,
      numeric_max = NULL,
      numeric_min = NULL,
      string_input = NULL
    ),
    # Valid-values filter configuration
    valid_values = list(
      group_selection = "In total",
      min_count = 1
    ),
    # Custom filter conditions queue
    custom_conditions = data.frame(
      Column = character(),
      Operator_1 = character(),
      Value_1 = character(),
      Logic = character(),
      Operator_2 = character(),
      Value_2 = character(),
      Empty_Filter = character(),
      Multi_Column_Logic = character(),
      stringsAsFactors = FALSE
    ),
    # Last filter run results
    filter_results = list(
      success = FALSE,
      rows_original = 0,
      rows_filtered = 0,
      rows_removed = 0,
      errors = character(),
      warnings = character()
    ),
    # UI state
    errors = character(),
    preview_active = FALSE,
    processing = FALSE
  )

  # --------------------------------------------------------------------------
  # Triggers (incremented to signal downstream observers)
  # --------------------------------------------------------------------------

  apply_filters_trigger <- shiny::reactiveVal(0)
  reset_filters_trigger <- shiny::reactiveVal(0)

  # --------------------------------------------------------------------------
  # UI caches (avoid redundant selectizeInput updates)
  # --------------------------------------------------------------------------

  filterChoicesCache  <- shiny::reactiveVal(character(0))
  filterCategoryCache <- shiny::reactiveVal("")

  rv_filter <- shiny::reactiveValues(
    last_choices  = character(0),
    last_selected = character(0),
    last_category = ""
  )

  # --------------------------------------------------------------------------
  # Config state (auto-assign integration)
  # --------------------------------------------------------------------------

  config <- shiny::reactive({
    tryCatch({
      if (is.reactive(UI_config)) UI_config() else UI_config
    }, error = function(e) {
      debug_log(paste("Error accessing UI_config:", e$message), 1)
      NULL
    })
  })

  config_applied <- shiny::reactiveVal(FALSE)
  filtering_ui_active <- shiny::reactiveVal(FALSE)

  # --------------------------------------------------------------------------
  # Derived reactives: column selection (debounced)
  # --------------------------------------------------------------------------

  filter_columns_raw <- shiny::reactive({
    input$filter_column_dw
  })

  filter_columns_debounced <- shiny::debounce(filter_columns_raw, millis = 700)

  selected_filter_columns <- shiny::reactive({
    filter_columns_debounced()
  })

  # --------------------------------------------------------------------------
  # Derived reactives: metadata readiness
  # --------------------------------------------------------------------------

  metadata_ready <- shiny::reactive({
    tryCatch({
      if (is.reactive(metadata_ready_status)) {
        isTRUE(metadata_ready_status())
      } else {
        isTRUE(metadata_ready_status)
      }
    }, error = function(e) {
      debug_log(paste("Error accessing metadata ready status:", e$message), 1)
      TRUE
    })
  })

  # --------------------------------------------------------------------------
  # Derived reactives: custom conditions shortcut
  # --------------------------------------------------------------------------

  filtered_conditions_dw <- shiny::reactive({
    filter_state$custom_conditions
  })

  # --------------------------------------------------------------------------
  # Derived reactives: SOME logic
  # --------------------------------------------------------------------------

  some_operator <- shiny::reactive({
    tryCatch({
      val <- safe_character_check(input$some_operator, "at_least")
      if (!val %in% c("at_least", "less_than", "exactly")) {
        debug_log(paste("Invalid some_operator value:", val, "- using default"), 1)
        return("at_least")
      }
      val
    }, error = function(e) {
      debug_log(paste("Error in some_operator reactive:", e$message), 1)
      "at_least"
    })
  })

  some_count <- shiny::reactive({
    tryCatch({
      cols <- tryCatch(selected_filter_columns(), error = function(e) character(0))
      current_columns <- tryCatch(length(cols), error = function(e) 0)
      if (is.na(current_columns) || current_columns <= 0) current_columns <- 1
      current_columns
    }, error = function(e) {
      debug_log(paste("Error in some_count reactive:", e$message), 1)
      1
    })
  })

  # --------------------------------------------------------------------------
  # Derived reactives: column type detection for dynamic operator UI
  # --------------------------------------------------------------------------

  selected_type <- shiny::reactive({
    log_column_type_error <- function(e) {
      if (inherits(e, c("shiny.silent.error", "validation"))) {
        return()
      }

      message <- conditionMessage(e)
      if (nzchar(message)) {
        debug_log(paste("Error determining column type:", message), 1)
      }
    }

    tryCatch({
      cols <- tryCatch(
        selected_filter_columns(),
        error = function(e) {
          log_column_type_error(e)
          NULL
        }
      )
      if (is.null(cols) || length(cols) == 0) return("character")

      current_data <- tryCatch(
        data(),
        error = function(e) {
          log_column_type_error(e)
          NULL
        }
      )
      if (is.null(current_data) ||
          !is.data.frame(current_data) ||
          is.null(names(current_data)) ||
          length(names(current_data)) == 0) {
        return("character")
      }

      first_col <- cols[1]
      if (!first_col %in% names(current_data)) {
        return("character")
      }

      if (is.numeric(current_data[[first_col]])) return("numeric")
      "character"
    }, error = function(e) {
      log_column_type_error(e)
      "character"
    })
  })

  # --------------------------------------------------------------------------
  # Return all state as a named list
  # --------------------------------------------------------------------------

  list(
    # Primary state container
    filter_state            = filter_state,

    # Triggers
    apply_filters_trigger   = apply_filters_trigger,
    reset_filters_trigger   = reset_filters_trigger,

    # UI caches
    filterChoicesCache      = filterChoicesCache,
    filterCategoryCache     = filterCategoryCache,
    rv_filter               = rv_filter,

    # Config (auto-assign integration)
    config                  = config,
    config_applied          = config_applied,
    filtering_ui_active     = filtering_ui_active,

    # Derived reactives: column selection
    filter_columns_raw      = filter_columns_raw,
    filter_columns_debounced = filter_columns_debounced,
    selected_filter_columns = selected_filter_columns,

    # Derived reactives: metadata
    metadata_ready          = metadata_ready,

    # Derived reactives: custom conditions shortcut
    filtered_conditions_dw  = filtered_conditions_dw,

    # Derived reactives: SOME logic
    some_operator           = some_operator,
    some_count              = some_count,

    # Derived reactives: column type
    selected_type           = selected_type
  )
}
