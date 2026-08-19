# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_reactive.R
#
# Purpose:
#   Centralizes all reactive state for the Annotation submodule of the
#   Data Wizard in a single factory function.
#
# Architectural Role:
#   State layer of the annotation module. Called once from modAnnotationServer()
#   during initialization. The returned list is passed to
#   register_annotation_observers() so that all parts of the module share
#   the same reactive instances.
#
# Structure:
#   1. create_annotation_state() - Factory function that creates and returns:
#      - current_data              reactive: validated data frame wrapper
#      - current_meta              reactive: validated metadata wrapper
#      - ui_config_update_active   reactiveVal: guard flag for import loops
#      - current_ui_config         reactiveVal: snapshot of last applied config
#      - get_ui_config             reactive: unwraps UI_config regardless of type
#      - last_mapping_result       reactiveVal: summary of last mapping operation
#      - cached_org_db             reactiveVal: session-level OrgDb cache
#      - cached_orgdb_name         reactiveVal: tracks which OrgDb is cached
#      - cached_keytypes           reactiveVal: session-level keytypes cache
#      - keytype_loading           reactiveVal: guard flag preventing concurrent loads
#      - keytype_last_organism     reactiveVal: tracks organism with active keytype work
#      - keytype_applied_orgdb     reactiveVal: tracks OrgDb whose keytype choices were applied
#      - keytype_choices_applied   reactiveVal: tracks last applied keytype choices by OrgDb
#      - abort_flag                reactiveVal: signals abort request to running mapping
#      - cached_biomart_species    reactiveVal: cached species data frame from Ensembl BioMart
#      - cached_biomart_keytypes   reactiveVal: session-level keytypes per BioMart species (named list)
#      - biomart_cache_manifest    reactiveVal: BioMart cache metadata (timestamps, status, counts)
#      - pre_crossspecies_species  reactiveVal: species choices snapshot before cross-species toggle
#      - source_update_token       reactiveVal: monotonic counter for source species request versioning
#      - target_update_token       reactiveVal: monotonic counter for target species request versioning
#      - keytype_status_message    reactiveVal: immediate status message for keytype loading progress
#      - biomart_table_env         environment: mutable session-level cache for full BioMart mapping
#                                               tables; keyed by source_dataset:source_attr:target_attr
#
# Notes:
#   - Purely declarative: no observers, no renderUI, no side effects.
#   - Must be called INSIDE moduleServer() so that session is available.
# ==============================================================================


#' Create all reactive state for the annotation module.
#'
#' @param get_data  Function (not reactive); returns the current data frame.
#' @param data_def  Reactive expression; returns the current metadata data frame.
#' @param UI_config Reactive, reactiveVal, list, or NULL providing UI configuration.
#' @param debug_log Logging function with signature (message, level).
#' @return A named list of all reactive containers and derived reactives.
create_annotation_state <- function(get_data, data_def, UI_config, debug_log) {

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
        debug_log("get_ui_config: unrecognized UI_config type - returning NULL", 2)
        return(NULL)
      }
    }, error = function(e) {
      debug_log(paste("get_ui_config: error accessing UI_config:", e$message), 1)
      return(NULL)
    })
  })

  # --------------------------------------------------------------------------
  # Annotation-specific state
  # --------------------------------------------------------------------------

  # Summary of last mapping operation (for status display)
  last_mapping_result <- reactiveVal(NULL)

  # Session-level OrgDb cache to avoid repeated disk reads

  cached_org_db <- reactiveVal(NULL)

  # Tracks which OrgDb name is currently cached
  cached_orgdb_name <- reactiveVal(NULL)

  # Session-level keytypes cache
  cached_keytypes <- reactiveVal(NULL)

  # Loading guard: prevents concurrent keytype loads (mirrors GO module pattern)
  keytype_loading <- reactiveVal(FALSE)

  # Tracks the organism currently/most recently scheduled for keytype work.
  # This is only an in-flight duplicate guard; it is not proof that the UI
  # dropdown choices were actually updated.
  keytype_last_organism <- reactiveVal(NULL)

  # Tracks the OrgDb whose Annotation keytype choices have been applied to the
  # source/target dropdowns.  This is separate from keytype_last_organism so a
  # same-organism event can still refresh/expand startup defaults when no real
  # dropdown update has been applied yet.
  keytype_applied_orgdb <- reactiveVal(NULL)

  # Named list of applied source/target choice vectors by OrgDb.  Used for
  # diagnostics and for distinguishing expanded choices from startup defaults.
  keytype_choices_applied <- reactiveVal(list())

  # Startup guard: allows the Annotation UI's default keytype selections to be
  # accepted on the initial default-species event without forcing an immediate
  # cache lookup/download. Later user species changes still use the full loader.
  annotation_keytype_initialized <- reactiveVal(FALSE)

  # Abort flag: set to TRUE when the user clicks "Abort Mapping"
  abort_flag <- reactiveVal(FALSE)

  # Cached BioMart species data frame (scientific_name + dataset columns).
  # Populated from disk cache or live fetch; NULL triggers a fresh load.
  cached_biomart_species <- reactiveVal(NULL)

  # Session-level keytype cache per BioMart species.
  # Named list: species_name -> character vector of keytypes.
  # With the full persistent cache strategy, this is bulk-loaded from disk
  # on first cross-species activation and provides near-instant lookups.
  cached_biomart_keytypes <- reactiveVal(list())

  # BioMart cache manifest: tracks metadata about the full persistent cache
  # (created_at, updated_at, species_count, keytypes_cached_count,
  # missing_species, status).  Loaded from disk on init and updated after
  # each full cache rebuild.  Timestamps are informational/audit only.
  biomart_cache_manifest <- reactiveVal(NULL)

  # Snapshot of species dropdown choices before entering cross-species mode
  # (used to restore the full AnnotationHub/default list on toggle-off)
  pre_crossspecies_species <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # Request versioning tokens for latest-selection-wins logic
  # --------------------------------------------------------------------------

  # Monotonically increasing counter for source species keytype update requests.
  # Each new request increments the counter; stale responses are discarded by
  # comparing the captured token to the current value after an async/slow step.
  source_update_token <- reactiveVal(0L)

  # Same mechanism for target species keytype update requests.
  target_update_token <- reactiveVal(0L)

  # Immediate status message for keytype loading progress.
  # Updated instantly on species change (before debounce fires); cleared when
  # the debounced worker finishes.  Rendered in the annotation_status output
  # when no mapping result is present to give the user immediate feedback
  # during the scheduling/cache-lookup/fetch pipeline.
  keytype_status_message <- reactiveVal(NULL)

  # Choices for source species dropdown before cross-species mode was entered;
  # initialized to the four UI defaults and updated by Update Organisms (in
  # Annotation mode) so that toggling off cross-species restores the
  # most-recently-loaded AnnotationHub list.
  pre_crossspecies_source_choices <- reactiveVal(
    c("Homo sapiens", "Mus musculus", "Rattus norvegicus", "Drosophila melanogaster")
  )

  # Guard flag preventing concurrent full BioMart cache builds.  Set to TRUE
  # when a build is in progress, FALSE when idle.  Checked by Refresh Cache
  # and Update Organisms observers to avoid overlapping builds.
  biomart_build_active <- reactiveVal(FALSE)

  # Session-level cache for full BioMart mapping tables downloaded during ID
  # mapping.  Stored as a plain R environment (mutable by reference) so that
  # biomart_map_ids() can read/write entries without Shiny reactive overhead.
  #
  # Each entry is keyed by the combination of BioMart parameters used for the
  # query (source_dataset:source_attr:target_attr) and stores the complete
  # downloaded data.frame of all source-to-target mappings.  On the first
  # "Map IDs" button click the full table is fetched from Ensembl in a single
  # request; subsequent button clicks reuse the cached table for local lookup,
  # eliminating repeated BioMart network calls.
  biomart_table_env <- new.env(parent = emptyenv())

  # --------------------------------------------------------------------------
  # Stale cache decision state
  # --------------------------------------------------------------------------

  # Tracks user decisions for stale (> 30 days) AnnotationHub caches.
  # Named list: orgdb_name -> "use_old" (keep stale cache) or "replace"
  # (download fresh).  Checked by the species-change observer and the
  # mapping observer to honour the user's earlier choice in the session.
  stale_cache_accepted <- reactiveVal(list())

  # Stores the orgdb_name + organism_display that triggered a stale-cache
  # modal so that the modal-button observers can resume the keytype load.
  pending_stale_cache_organism <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # Identifier Merging state
  # --------------------------------------------------------------------------

  # Current list of identifier columns selected for merging (in user-defined
  # order).  Updated by the merge UI (drag-and-drop reorder, remove, reset).
  merge_identifier_list <- reactiveVal(character(0))

  # Default/original identifier columns from metadata, used for reset.
  merge_default_identifiers <- reactiveVal(character(0))

  # --------------------------------------------------------------------------
  # Return all state as a named list
  # --------------------------------------------------------------------------

  list(
    current_data                   = current_data,
    current_meta                   = current_meta,
    ui_config_update_active        = ui_config_update_active,
    current_ui_config              = current_ui_config,
    get_ui_config                  = get_ui_config,
    last_mapping_result            = last_mapping_result,
    cached_org_db                  = cached_org_db,
    cached_orgdb_name              = cached_orgdb_name,
    cached_keytypes                = cached_keytypes,
    keytype_loading                = keytype_loading,
    keytype_last_organism          = keytype_last_organism,
    keytype_applied_orgdb          = keytype_applied_orgdb,
    keytype_choices_applied        = keytype_choices_applied,
    annotation_keytype_initialized = annotation_keytype_initialized,
    abort_flag                     = abort_flag,
    cached_biomart_species         = cached_biomart_species,
    cached_biomart_keytypes        = cached_biomart_keytypes,
    biomart_cache_manifest         = biomart_cache_manifest,
    pre_crossspecies_species       = pre_crossspecies_species,
    pre_crossspecies_source_choices = pre_crossspecies_source_choices,
    biomart_build_active           = biomart_build_active,
    merge_identifier_list          = merge_identifier_list,
    merge_default_identifiers      = merge_default_identifiers,
    source_update_token            = source_update_token,
    target_update_token            = target_update_token,
    keytype_status_message         = keytype_status_message,
    biomart_table_env              = biomart_table_env,
    stale_cache_accepted           = stale_cache_accepted,
    pending_stale_cache_organism   = pending_stale_cache_organism
  )
}
