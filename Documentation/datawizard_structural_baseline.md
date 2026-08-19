# Data Wizard structural migration baseline

This is a review inventory, not a migration plan.  The source files named below
remain the runtime authority.  Updating a contract in this document and its
static test is required when a reviewed move intentionally changes the
inventory.

## A. Boundary

The baseline covers `datawizard_core.R`, `datawizard_integration.R`,
`datawizard_utils.R`, the File Loader and Tables compatibility entry points,
and the Auto-Assign integration adapter.  No runtime file is part of this
change.

## B. Exact top-level symbol inventory

The symbols below are assignments at physical column zero (nested helpers are
deliberately excluded).

* `datawizard_core.R`: `create_metadata_content_signature_dw`,
  `create_datawizard_identifier_choices`, `create_primary_data_state_adapter`,
  `create_core_reactive_values`, `create_ui_config_reactive_values`,
  `create_data_access_functions`, `create_modification_tracking_functions`,
  `create_ui_config_management_functions`, `create_metadata_content_status`,
  `create_metadata_update_functions`, `create_safe_ui_system`,
  `create_submodule_session_state`, `register_core_data_lifecycle_observers`.
* `datawizard_integration.R`: `resolve_optional_module_api`,
  `call_optional_module_api`, `initialize_submodules`,
  `get_all_submodule_ui_states`, `set_all_submodule_ui_states`,
  `setup_auto_assign_integration`, `setup_filter_integration`,
  `safe_reactive_get`, `setup_ui_config_triggers`,
  `register_datawizard_ui_toggle_handlers`,
  `register_datawizard_state_bridge_observers`,
  `register_assign_rules_ui_config_observers`,
  `register_datawizard_session_cleanup`.
* `datawizard_utils.R`: `datawizard_metadata_content_choices`,
  `datawizard_drop_deprecated_metadata_columns`, `debug_log`,
  `datawizard_is_displayable_primary_data`, `resolve_datawizard_dataset`,
  `.datawizard_read_value`, `.datawizard_lookup_state`,
  `.datawizard_resolve_registry`, `.datawizard_safe_metadata_skeleton`,
  `metadata_matches_dataset`, `datawizard_normalize_technical_pair`,
  `datawizard_migrate_metadata_technical_keys`, `is_meaningful_metadata`,
  `restore_has_valid_canonical_pair`, `rebuild_metadata_for_dataset`,
  `set_metadata_for_dataset`, `resolve_current_metadata`,
  `select_datawizard_primary_display_data`, `validate_data_frame`,
  `validate_reactive_value`, `safe_module_call`,
  `initialize_datawizard_module_safely`, `validate_column_type`,
  `get_required_data_type`, `perform_metadata_safety_check`,
  `clean_metadata_for_filtering`, `validate_filtering_config_structure`,
  `test_filtering_manually`, `reset_rv_for_primary_data`,
  `reset_core_metadata_for_new_data`, `set_ratio_or_identifier_options`.
* `datawizard_file_loader.R`: `is_supported_datawizard_upload_dw`,
  `check_csv_separator_dw`, `validate_file_size_dw`,
  `detect_and_handle_encoding_dw`, `manage_memory_after_loading_dw`,
  `robust_read_table`, `load_file_with_recovery_dw`,
  `canonicalize_datawizard_column_names`, `clean_and_index`,
  `process_data_with_header_dw`, `load_file_dw`, `init_handson_table_dw`,
  `modFileLoaderUI`, `modFileLoaderServer`.
* `datawizard_tables.R`: `modDataTablesUI`, `modDataTablesServer`.

## C. Compatibility callers and paths

`modules/Data Wizard/datawizard_file_loader.R` is the File Loader
compatibility/orchestration entrypoint. `modules/Data Wizard/file_loader/` owns its UI,
shared context, observer families, restore, and diagnostics, and continues to
own the reading and canonicalization primitives.

The main coordinator continues to source core, integration, export, and utils
by their historical paths.  File Loader and the coordinator call
`create_primary_data_state_adapter`; the coordinator also calls every public
core factory.  Integration and session restore remain direct callers of the
adapter.  Submodules continue to call `create_submodule_session_state` through
the core compatibility source path.  The executable static contract contains
the complete caller/path matrix so additions remain allowed while removal of a
known compatibility route is reviewed.

## D. Adapter surface

`create_auto_assign_integration_adapters()` returns exactly:
`collect_batch_effects_ui_state`, `collect_pivot_ui_state`,
`apply_pivot_ui_config`, `get_pivot_state`, `collect_merge_ui_state`,
`collect_filter_ui_state`, `collect_ratio_configurations`,
`collect_basemean_configurations`, `collect_edit_operations`,
`collect_imputation_ui_config`, `apply_filter_template`,
`apply_ratio_configurations`, `apply_edit_operations`,
`apply_imputation_ui_config`, and `get_imputation_state`.

## E. Public module return names

File Loader returns: `primary`, `additional`, `data_fixed`, `data2_fixed`,
`header_primary`, `header_additional`, `init_meta`, `loading_errors`,
`loading_active`, `current_operation`, `loading_history`, `has_primary_data`,
`has_additional_data`, `file_cache`, `module_initialized`, `loader_mode`,
`can_rebuild_metadata`, `header_reprocess_active`, `load_file_enhanced`,
`module_health_check`, `set_additional_working_data`, `clear_cache`,
`get_session_state`, `get_minimal_session_state`, `set_session_state`, and
`get_loading_summary`.

Tables returns: `current_metadata`, `current_handson_metadata`,
`set_current_metadata`, `has_metadata`, `has_final_metadata`,
`is_data_modified`, and `create_content_color_mapping`.

## F. Physical line-count policy

Physical lines include blank and comment lines and are counted as
`splitlines()`, independent of the final newline. The historical counts were:
core 2,941; integration 1,856; utils 1,083; File Loader 3,658; Tables 238; and
Auto-Assign integration adapter 878. After extraction, the Section B inventory
is validated across each compatibility loader and its owned implementation
files. Every focused Core, File Loader, and Tables R subscript must remain at or
below 1,000 physical lines; a move must preserve the exact symbol set and avoid
duplicate ownership.

## G. Intended acyclic source DAG

For the scoped files the intended edges are:

* `datawizard_module.R` -> provenance, File Loader, batch effects, pivot,
  merge, Tables, filtering, auto regex, auto assign, assign rules, imputation,
  edit, ratios, basemean, annotation, core, integration, export, utils;
* core -> utils, dataset registry, then the focused Core implementation units;
* integration -> utils;
* File Loader compatibility/orchestration entrypoint -> reading,
  canonicalization, UI, shared context, observer families, restore, and
  diagnostics owned by `modules/Data Wizard/file_loader/`;
* Tables -> tables logic, state, the single observer-context implementation,
  mutation and metadata compatibility loaders, rendering, observer, and UI;
* Tables metadata compatibility loader -> hydration, sync-rendering, editing.

The contract rejects duplicate edges and cycles in this scoped graph. The
Auto-Assign integration adapter remains a leaf here. Dynamic sourcing outside
this migration boundary is intentionally not inferred. Production callers
continue to use the historical compatibility paths; direct-source compatibility
for focused implementations assumes the same prerequisite order.

## H. Deferred semantic/runtime probes

Later moving commits must add or retain probes for: exact revision increments;
one evaluation per committed snapshot; primary/additional file-read counts;
metadata regeneration count and identity; DT widget serialization; restore
application count and idempotence.  Each probe must record elapsed time and its
counter values before thresholds are changed.  The present bounded-snapshot
harness captures its existing upper bounds; this baseline introduces no new
performance threshold.
