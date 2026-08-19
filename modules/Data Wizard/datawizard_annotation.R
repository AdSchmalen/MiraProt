# ==============================================================================
# File: modules/Data Wizard/datawizard_annotation.R
#
# Purpose:
#   Orchestrator for the Data Wizard Annotation submodule. Sources all sub-files,
#   defines the UI wrapper (modAnnotationUI), and wires the server function
#   (modAnnotationServer). This is the only file in the module that contains a
#   server function.
#
# Architectural Role:
#   This file delegates concerns to sub-files inside Annotation/:
#
#   Utils layer (sourced first, in dependency order):
#     - datawizard_annotation_utils.R                    : Core logic (defaults, collapse, intra-species mapping)
#     - datawizard_annotation_utils_biomart_cache.R      : BioMart disk cache I/O (save/load/invalidate/manifest)
#     - datawizard_annotation_utils_biomart_species.R    : BioMart species lookups, keytype fetch, retry logic
#     - datawizard_annotation_utils_biomart_build.R      : Full/selective/missing BioMart cache builders
#     - datawizard_annotation_utils_biomart_mapping.R    : Cross-species ID mapping via BioMart/Ensembl
#
#   Reactive & observer layer:
#     - datawizard_annotation_reactive.R           : Reactive state factory
#     - datawizard_annotation_observer_general.R   : General observers (a, b, c, e, f)
#     - datawizard_annotation_observer_biomart.R   : BioMart observers (c1b, c1c, c1d)
#     - datawizard_annotation_observer_cache.R     : Cache observers (c2, c3, c3a-c3c, c4)
#     - datawizard_annotation_observer_mapping.R   : Mapping observers (d0, d)
#     - datawizard_annotation_observer.R           : Thin observer entrypoint
#
#   UI layer:
#     - datawizard_annotation_ui.R                 : Static UI layout
#
# Structure:
#   1. Source sub-files into modEnv
#   2. modAnnotationUI()    - UI wrapper calling datawizard_annotation_UI()
#   3. modAnnotationServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_annotation_state()
#      c. apply_ui_config_annotation() helper
#      d. Observer registration via register_annotation_observers()
#      e. Return interface
#
# Return Interface (public API):
#   get_ui_config                : reactive returning the current UI_config value
#   apply_ui_config_annotation   : function(cfg) to programmatically apply a config
#
# Notes for future developers:
#   - No server logic lives in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_annotation_observers.
#   - debug_log is defined here and passed explicitly to any function that logs.
#   - The apply_ui_config_annotation() helper is defined here (not in the observer
#     file) because it is part of the return interface and requires access to
#     both state and session, which are only available inside moduleServer().
# ==============================================================================

source("modules/Data Wizard/Annotation/datawizard_annotation_utils.R",                local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_cache.R",  local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_species.R", local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_build.R",  local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_mapping.R", local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_merge.R",          local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_reactive.R",             local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_observer_general.R",     local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_observer_biomart.R",     local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_observer_cache.R",       local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_observer_mapping.R",     local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_utils_observer.R",       local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_observer.R",             local = modEnv)
source("modules/Data Wizard/Annotation/datawizard_annotation_ui.R",                   local = modEnv)


# ==============================================================================
# UI
# ==============================================================================

modAnnotationUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    datawizard_annotation_UI(ns)
  )
}


# ==============================================================================
# SERVER
# ==============================================================================

modAnnotationServer <- function(id, data_def, get_data, set_data,
                                 available_samples, UI_config,
                                 session_restore_trigger = reactive(NULL),
                                 debug_level = 0) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "ANNOTATION", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ ANNOTATION ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Annotation module server starting", 1)

    # --------------------------------------------------------------------------
    # State initialization
    # --------------------------------------------------------------------------

    state <- create_annotation_state(get_data, data_def, UI_config, debug_log)

    ui_config_update_active <- state$ui_config_update_active
    current_ui_config       <- state$current_ui_config
    get_ui_config           <- state$get_ui_config

    # --------------------------------------------------------------------------
    # apply_ui_config_annotation()
    # Applies an imported configuration object to the module UI controls.
    # Defined here because it is part of the public return interface and
    # requires direct access to session, input, and state.
    # --------------------------------------------------------------------------

    apply_ui_config_annotation <- function(cfg) {
      if (is.null(cfg) || !is.list(cfg)) {
        debug_log("apply_ui_config_annotation: NULL or non-list config - skipping", 2)
        return(invisible(FALSE))
      }

      tryCatch({
        ui_config_update_active(TRUE)
        on.exit(ui_config_update_active(FALSE), add = TRUE)

        debug_log("Applying annotation UI configuration", 1)

        if (!is.null(cfg$species)) {
          updateSelectInput(session, "species_annotation", selected = cfg$species)
        }
        if (!is.null(cfg$from_keytype)) {
          updateSelectInput(session, "from_keytype_annotation", selected = cfg$from_keytype)
        }
        # Unified target keytype: accept both to_keytype and legacy target_keytype
        target_kt <- cfg$to_keytype %||% cfg$target_keytype
        if (!is.null(target_kt)) {
          updateSelectInput(session, "to_keytype_annotation", selected = target_kt)
        }
        # Strategy dropdown: accept new annotation_strategy value, or map
        # legacy cross_species boolean to the appropriate strategy
        if (!is.null(cfg$annotation_strategy)) {
          updateSelectInput(session, "annotation_strategy", selected = cfg$annotation_strategy)
        } else if (!is.null(cfg$cross_species)) {
          # Legacy config migration: cross_species TRUE -> biomart, FALSE -> annothub
          strategy_val <- if (isTRUE(cfg$cross_species)) "biomart" else "annothub"
          updateSelectInput(session, "annotation_strategy", selected = strategy_val)
        }
        if (!is.null(cfg$target_species)) {
          updateSelectInput(session, "target_species_annotation", selected = cfg$target_species)
        }
        if (!is.null(cfg$collapse_strategy)) {
          updateSelectInput(session, "collapse_strategy_annotation", selected = cfg$collapse_strategy)
        }

        current_ui_config(cfg)
        debug_log("Annotation UI configuration applied successfully", 1)
        invisible(TRUE)

      }, error = function(e) {
        debug_log(paste("Error applying UI config:", e$message), 1)
        invisible(FALSE)
      })
    }

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------

    register_annotation_observers(
      input, output, session, ns, state, get_data, set_data,
      data_def, UI_config, apply_ui_config_annotation,
      debug_log, DEBUG_LEVEL
    )

    # --------------------------------------------------------------------------
    # Cleanup
    # --------------------------------------------------------------------------

    if (exists("cleanup_manager", envir = parent.env(environment()))) {
      tryCatch({
        cleanup_manager$register_module("Annotation", function() {
          debug_log("Executing [Annotation] cleanup", 2)
          state$last_mapping_result(NULL)
          state$cached_org_db(NULL)
          state$cached_orgdb_name(NULL)
          state$cached_keytypes(NULL)
          state$keytype_loading(FALSE)
          state$keytype_last_organism(NULL)
          state$keytype_applied_orgdb(NULL)
          state$keytype_choices_applied(list())
          state$cached_biomart_species(NULL)
          state$cached_biomart_keytypes(list())
          state$biomart_cache_manifest(NULL)
          state$pre_crossspecies_species(NULL)
          state$pre_crossspecies_source_choices(
            c("Homo sapiens", "Mus musculus", "Rattus norvegicus", "Drosophila melanogaster")
          )
          state$biomart_build_active(FALSE)
          state$abort_flag(FALSE)
          state$source_update_token(0L)
          state$target_update_token(0L)
          state$keytype_status_message(NULL)
          state$merge_identifier_list(character(0))
          state$merge_default_identifiers(character(0))
          rm(list = ls(envir = state$biomart_table_env), envir = state$biomart_table_env)
          debug_log("[Annotation] cleanup completed", 2)
        })
      }, error = function(e) {
        debug_log(paste("Could not register cleanup:", e$message), 2)
      })
    }

    # --------------------------------------------------------------------------
    # Session-restore bridge
    # --------------------------------------------------------------------------
    annotation_session_state <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        source_column_annotation  = "selectInput",
        species_annotation        = "selectInput",
        from_keytype_annotation   = "selectInput",
        to_keytype_annotation     = "selectInput",
        annotation_strategy       = "radioButtons",
        target_species_annotation = "selectInput",
        collapse_strategy_annotation = "selectInput",
        merge_behavior            = "radioButtons"
      ),
      module_label    = "Annotation",
      restore_trigger = session_restore_trigger
    )

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    list(
      get_ui_config              = get_ui_config,
      apply_ui_config_annotation = apply_ui_config_annotation,
      get_session_state          = annotation_session_state$get_session_state,
      set_session_state          = annotation_session_state$set_session_state
    )
  })
}
