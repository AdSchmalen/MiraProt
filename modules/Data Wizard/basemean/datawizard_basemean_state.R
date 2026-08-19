# ==============================================================================
# File: modules/Data Wizard/basemean/datawizard_basemean_state.R
#
# Purpose:
#   Centralizes all reactive state for the Basemean submodule of the
#   Data Wizard in a single factory function. This is the single source of
#   truth for all basemean reactive containers.
#
# Architectural Role:
#   State layer of the basemean module. Called once from modBasemeanServer()
#   during initialization. The returned list is passed to
#   register_basemean_observers() and to apply_ui_config_basemean() so that
#   all parts of the module share the same reactive instances.
#
# Structure:
#   1. create_basemean_state() - Factory function that creates and returns:
#      - current_data              reactive: validated data frame wrapper
#      - current_meta              reactive: validated metadata wrapper
#      - ui_config_update_active   reactiveVal: guard flag for import loops
#      - current_ui_config         reactiveVal: snapshot of the last applied config
#      - get_ui_config             reactive: unwraps UI_config regardless of type
#
# Notes for future developers:
#   - This file is purely declarative. It must contain no observers, no
#     renderUI, and no side effects. It only creates reactive containers and
#     wires derived reactives.
#   - The factory must be called INSIDE moduleServer() so that `input` and
#     `session` are available in the calling closure.
#   - debug_log must be defined in the calling environment before
#     create_basemean_state() is invoked.
#   - current_data and current_meta use validate() so that downstream
#     reactive chains fail gracefully when data is not yet loaded.
# ==============================================================================


#' Create all reactive state for the basemean module.
#'
#' @param get_data  Function (not reactive); returns the current data frame.
#' @param data_def  Reactive expression; returns the current metadata data frame.
#' @param UI_config Reactive, reactiveVal, list, or NULL providing UI configuration.
#' @param debug_log Logging function with signature (message, level).
#' @return A named list of all reactive containers and derived reactives.
create_basemean_state <- function(get_data, data_def, UI_config, debug_log) {

  # --------------------------------------------------------------------------
  # Validated data wrappers
  # --------------------------------------------------------------------------

  current_data <- reactive({
    df <- get_data()
    validate(need(!is.null(df), "No data available"))
    df
  })

  current_meta <- reactive({
    def <- data_def()
    validate(need(!is.null(def), "No metadata available"))
    def
  })

  # --------------------------------------------------------------------------
  # UI update guard: prevents re-entrant or looping UI config imports
  # --------------------------------------------------------------------------

  ui_config_update_active <- reactiveVal(FALSE)

  # --------------------------------------------------------------------------
  # Snapshot of the last applied configuration (used for export)
  # --------------------------------------------------------------------------

  current_ui_config <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # Unwrap UI_config regardless of whether it is a reactiveVal, a reactive
  # expression, a static list, or NULL
  # --------------------------------------------------------------------------

  get_ui_config <- reactive({
    tryCatch({
      if (is.null(UI_config)) {
        return(NULL)
      } else if (is.reactive(UI_config)) {
        return(UI_config())
      } else if (is.list(UI_config)) {
        return(UI_config)
      } else {
        debug_log("get_ui_config: unrecognized UI_config type — returning NULL", 2)
        return(NULL)
      }
    }, error = function(e) {
      debug_log(paste("get_ui_config: error accessing UI_config:", e$message), 1)
      return(NULL)
    })
  })

  # --------------------------------------------------------------------------
  # Return all state as a named list
  # --------------------------------------------------------------------------

  list(
    current_data            = current_data,
    current_meta            = current_meta,
    ui_config_update_active = ui_config_update_active,
    current_ui_config       = current_ui_config,
    get_ui_config           = get_ui_config
  )
}
