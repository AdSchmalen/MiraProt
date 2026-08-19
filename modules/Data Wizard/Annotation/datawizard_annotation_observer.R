# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_observer.R
#
# Purpose:
#   Thin entrypoint for Annotation observer registration.  Creates shared
#   debounced reactives and delegates to four concern-based observer files:
#     - datawizard_annotation_observer_general.R  (a, b, c, e, f)
#     - datawizard_annotation_observer_biomart.R  (c1b, c1c, c1d)
#     - datawizard_annotation_observer_cache.R    (c2, c3, c3a-c3c, c4)
#     - datawizard_annotation_observer_mapping.R  (d0, d)
#
# Architectural Role:
#   Observer entrypoint for the annotation module.  Called from
#   modAnnotationServer() via register_annotation_observers() after state
#   is initialised.  This file owns the debounced species reactives that
#   are shared across sub-files and the single public entry function.
#   No observer logic lives here -- all observers are in the sub-files.
#
# Responsibilities:
#   - Define register_annotation_observers() with the original signature.
#   - Create debounced species reactives (50 ms).
#   - Delegate to the four sub-registration functions.
#
# Integration Points / Dependencies:
#   - The four observer sub-files must be sourced into modEnv before this
#     file (or at least before register_annotation_observers() is called).
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via the `state` list.
#
# Maintenance Guidance:
#   - Do NOT add observer logic here.  Place new observers in the
#     appropriate concern-based sub-file.
#   - If a new concern area is needed, create a new sub-file and add the
#     delegation call below.
# ==============================================================================


#' Register all observers for the annotation module.
#'
#' Thin entrypoint that creates shared debounced reactives and delegates
#' to four concern-based sub-registration functions.
#'
#' @param input              Shiny input object from moduleServer closure.
#' @param output             Shiny output object from moduleServer closure.
#' @param session            Shiny session object.
#' @param ns                 Namespace function for this module.
#' @param state              Named list from create_annotation_state().
#' @param get_data           Function returning the current data frame.
#' @param set_data           Function to write back a modified data frame.
#' @param data_def           Reactive expression returning the metadata data frame.
#' @param UI_config          Reactive, reactiveVal, list, or NULL; UI configuration.
#' @param apply_ui_config    Function apply_ui_config_annotation(cfg) from orchestrator.
#' @param debug_log          Logging function with signature (message, level).
#' @param DEBUG_LEVEL        Numeric debug verbosity level.
register_annotation_observers <- function(input, output, session, ns,
                                           state, get_data, set_data, data_def,
                                           UI_config, apply_ui_config,
                                           debug_log, DEBUG_LEVEL) {

  # --------------------------------------------------------------------------
  # Debounced species reactives (shared across sub-files)
  #
  # Coalesce rapid species switches into a single "latest-only" value.
  # When the user clicks through multiple species quickly, only the final
  # selection triggers expensive keytype loading (cache lookup / BioMart
  # fetch).  The debounce window (50 ms) is kept short because the full
  # persistent BioMart cache makes session lookups near-instant, so the
  # primary goal is to catch rapid click bursts without adding perceptible
  # delay for deliberate selections.
  #
  # Both the intra-species observer (b) and the cross-species observers
  # (c1c, c1d) observe these debounced values instead of the raw inputs.
  # --------------------------------------------------------------------------

  species_src_reactive  <- reactive({ input$species_annotation })
  species_src_debounced <- debounce(species_src_reactive, millis = 50)

  species_tgt_reactive  <- reactive({ input$target_species_annotation })
  species_tgt_debounced <- debounce(species_tgt_reactive, millis = 50)

  # --------------------------------------------------------------------------
  # Delegate to concern-based observer files
  # --------------------------------------------------------------------------

  register_annotation_observers_general(
    input = input, output = output, session = session, ns = ns,
    state = state, get_data = get_data, data_def = data_def,
    apply_ui_config = apply_ui_config,
    debug_log = debug_log, DEBUG_LEVEL = DEBUG_LEVEL,
    species_src_debounced = species_src_debounced,
    species_tgt_debounced = species_tgt_debounced
  )

  register_annotation_observers_biomart(
    input = input, output = output, session = session, ns = ns,
    state = state,
    debug_log = debug_log, DEBUG_LEVEL = DEBUG_LEVEL,
    species_src_debounced = species_src_debounced,
    species_tgt_debounced = species_tgt_debounced
  )

  register_annotation_observers_cache(
    input = input, output = output, session = session, ns = ns,
    state = state,
    debug_log = debug_log, DEBUG_LEVEL = DEBUG_LEVEL
  )

  register_annotation_observers_mapping(
    input = input, output = output, session = session, ns = ns,
    state = state, get_data = get_data, set_data = set_data,
    data_def = data_def,
    debug_log = debug_log, DEBUG_LEVEL = DEBUG_LEVEL
  )

  register_annotation_observers_strategy(
    input = input, output = output, session = session, ns = ns,
    state = state, get_data = get_data, set_data = set_data,
    data_def = data_def,
    debug_log = debug_log, DEBUG_LEVEL = DEBUG_LEVEL
  )
}
