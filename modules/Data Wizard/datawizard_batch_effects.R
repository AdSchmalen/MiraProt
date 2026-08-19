# =============================================================================
# File:    datawizard_batch_effects.R
# Purpose: Orchestrator for the Batch Effects correction module.
#
# Architecture fit:
#   Sourced into modEnv by the parent module (datawizard_module.R) via
#   source("modules/Data Wizard/datawizard_batch_effects.R", local = modEnv).
#   Exposes modBatchEffectsUI() and modBatchEffectsServer() which the parent
#   calls to embed the batch-effects UI tab and server logic.
#
# Structure:
#   1. modEnv imports   – pull shared helpers into local scope
#   2. Sub-file sourcing – load the 5 decomposed files from batch_effects/
#   3. modBatchEffectsUI – thin wrapper delegating to _UI.R
#   4. modBatchEffectsServer – moduleServer orchestrator:
#      a. Debug setup (debug_log)
#      b. State initialisation (from _state.R)
#      c. Data-access wrappers (from _utils.R)
#      d. Handler/observer registration (from _handlers.R)
#      e. Correction pipeline registration (from _correction.R)
#      f. Interface function wiring (delegates to _state.R query functions)
#      g. Session cleanup registration
#      h. Return list (reactives + closures for parent module)
#
# Sub-files (batch_effects/ directory):
#   _UI.R         – build_batch_effects_ui(): pure UI construction
#   _state.R      – init_batch_effects_state(), cleanup, state query functions
#   _utils.R      – data-access factories, validation helpers, merge logic
#   _handlers.R   – register_batch_effects_handlers(): observers, UI_config,
#                   event handlers, outputs
#   _correction.R – register_batch_correction_handler(): 12-step correction
#                   pipeline with 5 algorithms
#
# Developer guidance:
#   - This file should remain orchestration-only.  Business logic belongs in
#     the sub-files.
#   - All sub-files are sourced with local = modEnv, so functions defined in
#     them are available here via lexical scoping.
#   - The state aliases (batchCounter, batch_inputs, ...) exist solely for
#     the return list at the bottom.  Sub-files receive the state list object.
#   - To add a new sub-file, add a source() call in the sourcing block and
#     call its registration function inside moduleServer.
#   - All code, comments, and user-facing text must be in English.
# =============================================================================

#— Import helper functions from modEnv —#
clean_open_clusters                  <- modEnv$clean_open_clusters
performLeftCensoredImputation        <- modEnv$performLeftCensoredImputation
performGroupedLeftCensoredImputation <- modEnv$performGroupedLeftCensoredImputation
impute_random_forest                 <- modEnv$impute_random_forest
performRandomForestImputation        <- modEnv$performRandomForestImputation
impute_mice_cart                     <- modEnv$impute_mice_cart
performMICECartImputation            <- modEnv$performMICECartImputation
retransform_data_global              <- modEnv$retransform_data_global

# Source sub-module files
source("modules/Data Wizard/batch_effects/datawizard_batch_effects_UI.R", local = modEnv)
source("modules/Data Wizard/batch_effects/datawizard_batch_effects_state.R", local = modEnv)
source("modules/Data Wizard/batch_effects/datawizard_batch_effects_utils.R", local = modEnv)
source("modules/Data Wizard/batch_effects/datawizard_batch_effects_handlers.R", local = modEnv)
source("modules/Data Wizard/batch_effects/datawizard_batch_effects_correction.R", local = modEnv)

############
# UI

#' Batch Effects Module UI
#'
#' Creates the user interface for batch effects correction in proteomics data.
#' Delegates to build_batch_effects_ui() in batch_effects/datawizard_batch_effects_UI.R.
#'
#' @param id module id
#' @export
modBatchEffectsUI <- function(id) {
  build_batch_effects_ui(id)
}

############
# Server

#' Enhanced Batch Effects Module Server with Improved Debug Management
#'
#' Server logic for batch effects correction with UI configuration import/export support
#' and enhanced debug management following the pattern from datawizard_edit.
#'
#' @param id module id
#' @param get_data function to get current data
#' @param set_data function to set updated data
#' @param init_meta function to regenerate metadata skeleton (e.g. init_handson_table_dw)
#' @param header_primary reactive returning current header_row integer
#' @param UI_config reactive containing UI configuration for import (optional)
#' @param debug_level numeric debug level (0=none, 1=critical, 2=verbose)
#' @export
modBatchEffectsServer <- function(id, get_data = NULL, set_data = NULL, init_meta = NULL,
                                  header_primary = NULL, UI_config = NULL,
                                  session_restore_trigger = reactive(NULL),
                                  debug_level = 0,
                                  primary_working_revision_debounced = reactive(NULL),
                                  data_revision_signature = reactive(NULL),
                                  initialized = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management
    # ========================================

    # Helper function for controlled debug output
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "BATCH", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ BATCH ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Batch effects module server starting", 1)

    # ========================================
    # Reactive State (centralized in batch_effects/datawizard_batch_effects_state.R)
    # ========================================

    state <- init_batch_effects_state()

    # Local aliases for backward compatibility within this file.
    # Sub-files (handlers, correction) receive the state list directly.
    batchCounter             <- state$batchCounter
    batch_inputs             <- state$batch_inputs
    ui_config_applied        <- state$ui_config_applied
    ui_config_source         <- state$ui_config_source
    ui_config_update_in_progress <- state$ui_config_update_in_progress
    ui_config_errors         <- state$ui_config_errors
    last_correction_time     <- state$last_correction_time
    correction_history       <- state$correction_history

    # ========================================
    # Data Access Wrappers (from batch_effects/datawizard_batch_effects_utils.R)
    # ========================================

    get_file_data <- make_get_file_data(get_data, debug_log)
    set_file_data <- make_set_file_data(set_data, debug_log)

    # ========================================
    # Register handlers, observers, and outputs
    # (from batch_effects/datawizard_batch_effects_handlers.R)
    # ========================================

    handler_fns <- register_batch_effects_handlers(
      input, output, session, ns, state, get_file_data, UI_config, debug_log,
      primary_working_revision_debounced, data_revision_signature, initialized
    )
    apply_ui_config    <- handler_fns$apply_ui_config
    get_current_ui_state <- handler_fns$get_current_ui_state

    # ========================================
    # Register batch correction pipeline
    # (from batch_effects/datawizard_batch_effects_correction.R)
    # ========================================

    register_batch_correction_handler(input, state, get_file_data, set_file_data, debug_log)

    # ========================================
    # Interface Functions (delegated to state file)
    # ========================================

    apply_batch_correction <- function() {
      apply_batch_correction_check(input, state, get_file_data, debug_log)
    }

    is_batch_correction_configured <- function() {
      check_batch_correction_configured(input, state, debug_log)
    }

    get_batch_summary <- function() {
      get_batch_effects_summary(input, state, debug_log)
    }

    # Health check (delegated to state file)
    module_health_check <- function() {
      run_batch_effects_health_check(input, state, debug_log, DEBUG_LEVEL)
    }

    # ========================================
    # Session Cleanup (delegated to state file)
    # ========================================

    cleanup_manager$register_module("Batch effects", function() {
      cleanup_batch_effects_state(state, debug_log)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    batch_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        batch_method            = "selectInput",
        imputation_method_batch = "selectInput",
        transformation_batch    = "selectInput",
        remove_imputed_batch    = "checkboxInput"
      ),
      module_label    = "BatchEffects",
      restore_trigger = session_restore_trigger
    )

    # ========================================
    # Enhanced Return Values with UI_config Integration
    # ========================================

    return(list(
      # Core functionality
      processing_complete = reactive({ TRUE }),
      get_data = get_file_data,
      set_data = set_file_data,

      # Session-restore bridge
      get_session_state = batch_session_state$get_session_state,
      set_session_state = batch_session_state$set_session_state,

      # Enhanced interface for central control
      apply_batch_correction = apply_batch_correction,
      is_batch_correction_configured = is_batch_correction_configured,
      get_batch_summary = get_batch_summary,

      # Trigger for manual apply
      apply_trigger = reactive({ input$unBatchButton }),

      # Enhanced: UI_config management functions
      get_current_ui_state = get_current_ui_state,
      apply_ui_config = apply_ui_config,
      ui_config_applied = reactive({ ui_config_applied() }),
      ui_config_source = reactive({ ui_config_source() }),

      # Enhanced: Individual configuration accessors
      batch_method = reactive({
        tryCatch({
          input$batch_method
        }, error = function(e) {
          debug_log(paste("Error accessing batch_method reactive:", e$message), 1)
          "ComBat"
        })
      }),
      imputation_method_batch = reactive({
        tryCatch({
          input$imputation_method_batch
        }, error = function(e) {
          debug_log(paste("Error accessing imputation_method_batch reactive:", e$message), 1)
          "None"
        })
      }),
      transformation_batch = reactive({
        tryCatch({
          input$transformation_batch
        }, error = function(e) {
          debug_log(paste("Error accessing transformation_batch reactive:", e$message), 1)
          "None"
        })
      }),
      remove_imputed_batch = reactive({
        tryCatch({
          input$remove_imputed_batch
        }, error = function(e) {
          debug_log(paste("Error accessing remove_imputed_batch reactive:", e$message), 1)
          FALSE
        })
      }),
      batch_counter = batchCounter,
      batch_inputs = batch_inputs,

      # Enhanced: Error handling and debugging
      get_ui_config_errors = reactive({ ui_config_errors() }),
      clear_ui_config_errors = function() {
        tryCatch({
          ui_config_errors(list())
          debug_log("UI config errors cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing UI config errors:", e$message), 1)
        })
      },

      # Enhanced: Performance monitoring
      get_performance_metrics = reactive({
        tryCatch({
          list(
            last_correction_time = last_correction_time(),
            correction_history = correction_history(),
            debug_level = DEBUG_LEVEL
          )
        }, error = function(e) {
          debug_log(paste("Error getting performance metrics:", e$message), 1)
          list(
            last_correction_time = NULL,
            correction_history = list(),
            debug_level = DEBUG_LEVEL
          )
        })
      }),

      # Enhanced: Module health check
      module_health_check = module_health_check,

      # Enhanced: Debug and testing functions
      test_ui_config_loading = function() {
        debug_log("=== TESTING UI CONFIG LOADING ===", 1)
        debug_log(paste("UI config applied:", ui_config_applied()), 1)
        debug_log(paste("UI config source:", ui_config_source()), 1)
        debug_log(paste("UI config errors:", length(ui_config_errors())), 1)
        debug_log(paste("Update in progress:", ui_config_update_in_progress()), 1)
        debug_log(paste("Batch counter:", batchCounter()), 1)
        debug_log(paste("Batch inputs count:", length(batch_inputs())), 1)
        debug_log("=== END TESTING ===", 1)
      }
    ))
  })
}
