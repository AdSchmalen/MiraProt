# ==============================================================================
# File: modules/Data Wizard/datawizard_basemean.R
#
# Purpose:
#   Orchestrator for the Data Wizard Basemean submodule. Sources all sub-files,
#   defines the UI wrapper (modBasemeanUI), and wires the server function
#   (modBasemeanServer). This is the only file in the module that contains a
#   server function.
#
# Architectural Role:
#   This file delegates concerns to four sub-files inside basemean/:
#     - datawizard_basemean_logic.R    : Pure logic functions (no Shiny dependency)
#     - datawizard_basemean_state.R    : Reactive state factory (create_basemean_state)
#     - datawizard_basemean_observer.R : All observe() / observeEvent() blocks
#     - datawizard_basemean_ui.R       : Static UI layout and input widgets
#
# Structure:
#   1. Source sub-files into modEnv
#   2. modBasemeanUI()    - UI wrapper calling datawizard_basemean_UI()
#   3. modBasemeanServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_basemean_state()
#      c. apply_ui_config_basemean() helper (UI import application)
#      d. Observer registration via register_basemean_observers()
#      e. Return interface
#
# Return Interface (public API):
#   get_ui_config            : reactive returning the current UI_config value
#   apply_ui_config_basemean : function(cfg) to programmatically apply a config
#
# Notes for future developers:
#   - No server logic lives in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_basemean_observers.
#   - debug_log is defined here and passed explicitly to any function that logs.
#   - The apply_ui_config_basemean() helper is defined here (not in the observer
#     file) because it is part of the return interface and requires access to
#     both state and session, which are only available inside moduleServer().
# ==============================================================================

source("modules/Data Wizard/basemean/datawizard_basemean_logic.R",    local = modEnv)
source("modules/Data Wizard/basemean/datawizard_basemean_state.R",    local = modEnv)
source("modules/Data Wizard/basemean/datawizard_basemean_observer.R", local = modEnv)
source("modules/Data Wizard/basemean/datawizard_basemean_ui.R",       local = modEnv)


# ==============================================================================
# UI
# ==============================================================================

modBasemeanUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    datawizard_basemean_UI(ns)
  )
}


# ==============================================================================
# SERVER
# ==============================================================================

modBasemeanServer <- function(id, data_def, get_data, set_data,
                               available_samples, UI_config,
                               session_restore_trigger = reactive(NULL),
                               debug_level = 0,
                               metadata_revision_debounced = reactive(NULL),
                               primary_working_revision_debounced = reactive(NULL),
                               data_revision_signature = reactive(NULL),
                               metadata_assignment_pending = reactive(FALSE),
                               metadata_meaningful_ready = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "BASEMEAN", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ BASEMEAN ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Basemean module server starting", 1)

    # --------------------------------------------------------------------------
    # State initialization
    # --------------------------------------------------------------------------

    state <- create_basemean_state(get_data, data_def, UI_config, debug_log)

    ui_config_update_active <- state$ui_config_update_active
    current_ui_config       <- state$current_ui_config
    get_ui_config           <- state$get_ui_config

    # --------------------------------------------------------------------------
    # apply_ui_config_basemean()
    # Applies an imported configuration object to the module UI controls.
    # Defined here because it is part of the public return interface and
    # requires direct access to session, input, and state.
    # --------------------------------------------------------------------------

    apply_ui_config_basemean <- function(cfg) {
      debug_log("Applying imported Basemean UI configuration", 1)

      if (ui_config_update_active()) {
        debug_log("apply_ui_config_basemean: update already active — skipping", 2)
        return(FALSE)
      }

      ui_config_update_active(TRUE)
      on.exit(ui_config_update_active(FALSE), add = TRUE)

      session$userData$apply_basemean_after_sample_update <- cfg

      tryCatch({
        isolate({
          if (!is.null(cfg$abundance_type)) {
            updateSelectInput(session, "abundance_type_basemean",
                              selected = cfg$abundance_type)
          }
          if (!is.null(cfg$suffix)) {
            updateTextInput(session, "suffix_basemean", value = cfg$suffix)
          }
        })

        session$userData$last_basemean_config <- cfg
        debug_log("Basemean UI configuration applied successfully", 1)
        TRUE

      }, error = function(e) {
        debug_log(paste("apply_ui_config_basemean failed:", e$message), 1)
        FALSE
      })
    }

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------

    register_basemean_observers(
      input          = input,
      output         = output,
      session        = session,
      ns             = ns,
      state          = state,
      get_data       = get_data,
      set_data       = set_data,
      data_def       = data_def,
      UI_config      = UI_config,
      apply_ui_config = apply_ui_config_basemean,
      debug_log      = debug_log,
      DEBUG_LEVEL    = DEBUG_LEVEL,
      metadata_revision_debounced = metadata_revision_debounced,
      primary_working_revision_debounced = primary_working_revision_debounced,
      data_revision_signature = data_revision_signature,
      metadata_assignment_pending = metadata_assignment_pending,
      metadata_meaningful_ready = metadata_meaningful_ready
    )

    debug_log("Basemean module initialized successfully", 1)

    # --------------------------------------------------------------------------
    # Session Cleanup
    # --------------------------------------------------------------------------

    cleanup_manager$register_module("Basemean", function() {
      debug_log("Executing [Basemean] cleanup", 2)

      ui_config_update_active(FALSE)
      current_ui_config(NULL)

      debug_log("[Basemean] cleanup completed", 2)
    })

    # --------------------------------------------------------------------------
    # Session-restore bridge
    # --------------------------------------------------------------------------
    basemean_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        abundance_type_basemean  = "selectInput",
        sample_selection_basemean = "selectizeInput",
        suffix_basemean           = "textInput"
      ),
      module_label    = "Basemean",
      restore_trigger = session_restore_trigger
    )

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    list(
      get_ui_config            = get_ui_config,
      apply_ui_config_basemean = apply_ui_config_basemean,
      get_session_state        = basemean_session_state$get_session_state,
      set_session_state        = basemean_session_state$set_session_state
    )

  })
}
