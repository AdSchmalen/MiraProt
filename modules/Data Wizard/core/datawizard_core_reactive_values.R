# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_reactive_values.R
# Purpose:
#   Provide the core reactive values portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard core reactive value factories

#' Create core reactive values for data wizard state management
#' @return list of reactive values for core functionality
create_core_reactive_values <- function() {
  import_phase <- reactiveVal("idle")
  import_ready_revision <- reactiveVal(0L)
  import_generation_started <- reactiveVal(0L)
  import_generation_committed <- reactiveVal(0L)
  primary_working_revision <- reactiveVal(0L)
  primary_raw_revision <- reactiveVal(0L)
  primary_filtered_revision <- reactiveVal(0L)
  metadata_revision <- reactiveVal(0L)
  metadata_content_signature <- reactiveVal("")
  identifier_choices <- reactiveVal(stats::setNames(character(0), character(0)))
  metadata_lifecycle_state <- reactiveVal("raw_loaded")
  metadata_assignment_pending <- reactiveVal(FALSE)
  metadata_meaningful_ready <- reactiveVal(FALSE)
  secondary_revision <- reactiveVal(0L)

  # One transaction token for expensive consumers.  Keeping this as a compact
  # list (rather than a digest of the data) makes it cheap to compare and, more
  # importantly, publishes all parts of an import in one reactive flush.
  committed_snapshot_key <- reactive({
    list(
      import_generation = as.integer(import_generation_committed() %||% 0L),
      primary_working_revision = as.integer(primary_working_revision() %||% 0L),
      metadata_revision = as.integer(metadata_revision() %||% 0L),
      metadata_content_signature = as.character(metadata_content_signature() %||% ""),
      lifecycle_ready = isTRUE(metadata_meaningful_ready()) &&
        !isTRUE(metadata_assignment_pending()) &&
        identical(import_phase(), "ready")
    )
  })
  committed_snapshot_key_debounced <- debounce(committed_snapshot_key, 500)

  list(
    # Upload publication barrier. Consumers observe the ready revision together
    # with data/metadata revision IDs, never the full data frame.
    import_phase = import_phase,
    import_ready_revision = import_ready_revision,
    import_generation_started = import_generation_started,
    import_generation_committed = import_generation_committed,
    # Primary data state management
    dataset_registry = reactiveVal(create_datawizard_dataset_registry()),
    primary_data_raw = reactiveVal(NULL),
    handson_metadata = reactiveVal(NULL),
    final_processed_data = reactiveVal(NULL),
    final_processed_metadata = reactiveVal(NULL),
    apply_triggered = reactiveVal(FALSE),
    filter_applied = reactiveVal(FALSE),
    filtered_data = reactiveVal(NULL),

    # Lightweight dataset revision signals for downstream recalculation triggers.
    primary_working_revision = primary_working_revision,
    primary_raw_revision = primary_raw_revision,
    primary_filtered_revision = primary_filtered_revision,
    metadata_revision = metadata_revision,
    metadata_content_signature = metadata_content_signature,
    identifier_choices = identifier_choices,
    metadata_lifecycle_state = metadata_lifecycle_state,
    metadata_assignment_pending = metadata_assignment_pending,
    metadata_meaningful_ready = metadata_meaningful_ready,
    secondary_revision = secondary_revision,

    # Preferred downstream invalidation signal.  The individual revision
    # reactives below remain part of the public API for compatibility.
    committed_snapshot_key = committed_snapshot_key,
    committed_snapshot_key_debounced = committed_snapshot_key_debounced,

    primary_working_revision_debounced = debounce(reactive({
      primary_working_revision()
    }), 500),
    metadata_revision_debounced = debounce(reactive({
      metadata_revision()
    }), 500),
    metadata_content_signature_debounced = debounce(reactive({
      metadata_content_signature()
    }), 500),
    identifier_choices_debounced = debounce(reactive({
      identifier_choices()
    }), 500),
    secondary_revision_debounced = debounce(reactive({
      secondary_revision()
    }), 500),

    # Data modification tracking
    data_modified = reactiveVal(FALSE),
    modification_history = reactiveVal(list()),

    # Enhanced logging and settings storage
    imputation_log = reactiveVal(NULL),
    imputation_setting = reactiveVal(NULL),
    filtering_confidence = reactiveVal(NULL),
    filtering_valid_values = reactiveVal(NULL),
    filtered_conditions = reactiveVal(NULL),
    filtering_log = reactiveVal(NULL),

    # Error tracking
    ui_config_errors = reactiveVal(list()),
    filtering_config_errors = reactiveVal(list()),
    last_config_application_time = reactiveVal(NULL),

    # Central rule management
    central_rule_file = reactiveVal(""),
    central_loaded_rules = reactiveVal(NULL),
    rule_application_state = reactiveVal("idle"),

    metadata_observer_active = reactiveVal(TRUE)
  )
}

#' Create UI configuration reactive values for all modules
#' @return list of UI config reactive values
create_ui_config_reactive_values <- function() {
  list(
    # Imputation UI config
    central_imputation_ui_config = reactiveVal(NULL),
    ui_config_source = reactiveVal("none"),
    ui_config_update_in_progress = reactiveVal(FALSE),

    # Filtering UI config
    central_filtering_ui_config = reactiveVal(NULL),
    filtering_ui_config_source = reactiveVal("none"),
    filtering_update_in_progress = reactiveVal(FALSE),

    # Batch effects UI config
    central_batch_effects_ui_config = reactiveVal(NULL),
    batch_effects_ui_config_source = reactiveVal("none"),
    batch_effects_update_in_progress = reactiveVal(FALSE),

    # Pivot UI config
    central_pivot_ui_config = reactiveVal(NULL),
    pivot_ui_config_source = reactiveVal("none"),
    pivot_update_in_progress = reactiveVal(FALSE),

    # Merge UI config
    central_merge_ui_config = reactiveVal(NULL),
    merge_ui_config_source = reactiveVal("none"),
    merge_update_in_progress = reactiveVal(FALSE),

    # Ratios UI config
    central_ratios_ui_config = reactiveVal(NULL),
    ratios_ui_config_source = reactiveVal("none"),
    ratios_update_in_progress = reactiveVal(FALSE),

    # Basemean UI config
    central_basemean_ui_config = reactiveVal(NULL),
    basemean_ui_config_source = reactiveVal("none"),
    basemean_update_in_progress = reactiveVal(FALSE)
  )
}
