"""Review contracts for the pre-move Data Wizard source layout."""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[2]

FILES = {
    "core": Path("modules/Data Wizard/datawizard_core.R"),
    "integration": Path("modules/Data Wizard/datawizard_integration.R"),
    "utils": Path("modules/Data Wizard/datawizard_utils.R"),
    "loader": Path("modules/Data Wizard/datawizard_file_loader.R"),
    "tables": Path("modules/Data Wizard/datawizard_tables.R"),
    "adapter": Path("modules/Data Wizard/auto assign/datawizard_auto_assign_integration_adapters.R"),
}

SYMBOLS = {
    "core": "create_metadata_content_signature_dw create_datawizard_identifier_choices create_primary_data_state_adapter create_core_reactive_values create_ui_config_reactive_values create_data_access_functions create_modification_tracking_functions create_ui_config_management_functions create_metadata_content_status create_metadata_update_functions create_safe_ui_system create_submodule_session_state register_core_data_lifecycle_observers".split(),
    "integration": "resolve_optional_module_api call_optional_module_api initialize_submodules get_all_submodule_ui_states set_all_submodule_ui_states setup_auto_assign_integration setup_filter_integration safe_reactive_get setup_ui_config_triggers register_datawizard_ui_toggle_handlers register_datawizard_state_bridge_observers register_assign_rules_ui_config_observers register_datawizard_session_cleanup".split(),
    "utils": "datawizard_metadata_content_choices datawizard_drop_deprecated_metadata_columns debug_log datawizard_is_displayable_primary_data resolve_datawizard_dataset .datawizard_read_value .datawizard_lookup_state .datawizard_resolve_registry .datawizard_safe_metadata_skeleton metadata_matches_dataset datawizard_normalize_technical_pair datawizard_migrate_metadata_technical_keys is_meaningful_metadata restore_has_valid_canonical_pair rebuild_metadata_for_dataset set_metadata_for_dataset resolve_current_metadata select_datawizard_primary_display_data validate_data_frame validate_reactive_value safe_module_call initialize_datawizard_module_safely validate_column_type get_required_data_type perform_metadata_safety_check clean_metadata_for_filtering validate_filtering_config_structure test_filtering_manually reset_rv_for_primary_data reset_core_metadata_for_new_data set_ratio_or_identifier_options".split(),
    "loader": "is_supported_datawizard_upload_dw check_csv_separator_dw validate_file_size_dw detect_and_handle_encoding_dw manage_memory_after_loading_dw robust_read_table load_file_with_recovery_dw canonicalize_datawizard_column_names clean_and_index process_data_with_header_dw load_file_dw init_handson_table_dw modFileLoaderUI modFileLoaderServer".split(),
    "tables": ["modDataTablesUI", "modDataTablesServer"],
    "adapter": ["create_auto_assign_integration_adapters"],
}

LINE_COUNTS = {"core": 2941, "integration": 1856, "utils": 1083,
               "loader": 3658, "tables": 238, "adapter": 878}

IMPLEMENTATION_FILES = {
    "core": [Path(f"modules/Data Wizard/core/datawizard_core_{name}.R") for name in (
        "projections state_adapter reactive_values access_tracking ui_configuration "
        "metadata_updates safe_ui submodule_session lifecycle_observers"
    ).split()],
    "loader": [
        Path("modules/Data Wizard/file_loader/datawizard_file_reading.R"),
        Path("modules/Data Wizard/file_loader/datawizard_file_canonicalization.R"),
        Path("modules/Data Wizard/file_loader/datawizard_file_loader_ui.R"),
        FILES["loader"],
    ],
    "tables": [FILES["tables"]],
}

ADAPTER_METHODS = "collect_batch_effects_ui_state collect_pivot_ui_state apply_pivot_ui_config get_pivot_state collect_merge_ui_state collect_filter_ui_state collect_ratio_configurations collect_basemean_configurations collect_edit_operations collect_imputation_ui_config apply_filter_template apply_ratio_configurations apply_edit_operations apply_imputation_ui_config get_imputation_state".split()
LOADER_RETURNS = "primary additional data_fixed data2_fixed header_primary header_additional init_meta loading_errors loading_active current_operation loading_history has_primary_data has_additional_data file_cache module_initialized loader_mode can_rebuild_metadata header_reprocess_active load_file_enhanced module_health_check set_additional_working_data clear_cache get_session_state get_minimal_session_state set_session_state get_loading_summary".split()
TABLE_RETURNS = "current_metadata current_handson_metadata set_current_metadata has_metadata has_final_metadata is_data_modified create_content_color_mapping".split()
SOURCE_TARGETS = {
    "modules/datawizard_module.R": [
        f"modules/Data Wizard/datawizard_{name}.R" for name in
        "provenance file_loader batch_effects pivot merge tables filtering auto_regex auto_assign assign_rules imputation edit ratios basemean annotation module_ui core integration export utils".split()
    ],
    FILES["core"].as_posix(): [
        "modules/Data Wizard/datawizard_utils.R",
        "modules/Data Wizard/datawizard_dataset_registry.R",
        "modules/Data Wizard/core/datawizard_core_projections.R",
        "modules/Data Wizard/core/datawizard_core_state_adapter.R",
        "modules/Data Wizard/core/datawizard_core_reactive_values.R",
        "modules/Data Wizard/core/datawizard_core_access_tracking.R",
        "modules/Data Wizard/core/datawizard_core_ui_configuration.R",
        "modules/Data Wizard/core/datawizard_core_metadata_updates.R",
        "modules/Data Wizard/core/datawizard_core_safe_ui.R",
        "modules/Data Wizard/core/datawizard_core_submodule_session.R",
        "modules/Data Wizard/core/datawizard_core_lifecycle_observers.R",
    ],
    FILES["integration"].as_posix(): ["modules/Data Wizard/datawizard_utils.R"],
    FILES["tables"].as_posix(): [
        "modules/Data Wizard/tables/datawizard_tables_logic.R",
        "modules/Data Wizard/tables/datawizard_tables_state.R",
        "modules/Data Wizard/tables/datawizard_tables_observer_context.R",
        "modules/Data Wizard/tables/datawizard_tables_observer_mutations.R",
        "modules/Data Wizard/tables/datawizard_tables_observer_metadata.R",
        "modules/Data Wizard/tables/datawizard_tables_observer_rendering.R",
        "modules/Data Wizard/tables/datawizard_tables_observer.R",
        "modules/Data Wizard/tables/datawizard_tables_ui.R",
    ],
    FILES["utils"].as_posix(): [],
    FILES["loader"].as_posix(): [
        "modules/Data Wizard/file_loader/datawizard_file_reading.R",
        "modules/Data Wizard/file_loader/datawizard_file_canonicalization.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_ui.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_interactive.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_header_reset.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_restore.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_diagnostics.R",
        "modules/Data Wizard/file_loader/datawizard_file_loader_context.R",
    ],
    FILES["adapter"].as_posix(): [],
}


def text(key):
    return (ROOT / FILES[key]).read_text(encoding="utf-8")


def top_level_functions(source):
    return re.findall(r"(?m)^([.]?[A-Za-z][A-Za-z0-9._]*)\s*<-\s*function\s*\(", source)


def named_entries(source, marker):
    """Read top-level named arguments from the first list following marker."""
    start = source.index("list(", source.index(marker)) + len("list(")
    depth, quote, escaped, entries = 1, None, False, []
    for line in source[start:].splitlines():
        if depth == 1 and (match := re.match(r"\s*([A-Za-z][A-Za-z0-9._]*)\s*=", line)):
            entries.append(match.group(1))
        for char in line:
            if quote:
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == quote:
                    quote = None
            elif char in "'\"":
                quote = char
            elif char == "#":
                break
            elif char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
        if depth == 0:
            break
    return entries


def test_exact_top_level_symbols_and_physical_line_counts():
    for key, expected in SYMBOLS.items():
        units = IMPLEMENTATION_FILES.get(key, [FILES[key]])
        actual = [symbol for unit in units for symbol in top_level_functions(
            (ROOT / unit).read_text(encoding="utf-8")
        )]
        assert len(actual) == len(set(actual)), key
        assert sorted(actual) == sorted(expected), key
    focused = [
        *Path("modules/Data Wizard/core").glob("*.R"),
        *Path("modules/Data Wizard/file_loader").glob("*.R"),
        *Path("modules/Data Wizard/tables").glob("*.R"),
        *Path("modules/Data Wizard").glob("datawizard_file_loader_*.R"),
    ]
    for path in focused:
        assert len((ROOT / path).read_text(encoding="utf-8").splitlines()) <= 1000, path


def test_public_adapter_and_module_return_names():
    assert named_entries(text("adapter"), "    list(") == ADAPTER_METHODS
    assert named_entries(text("loader"), "# Return Interface (unchanged for compatibility)") == LOADER_RETURNS
    assert named_entries(text("tables"), "# Return interface") == TABLE_RETURNS


def test_compatibility_sources_and_current_callers_remain_present():
    coordinator = (ROOT / "modules/datawizard_module.R").read_text(encoding="utf-8")
    for path in ("datawizard_core.R", "datawizard_integration.R", "datawizard_export.R", "datawizard_utils.R"):
        assert f'source("modules/Data Wizard/{path}", local = TRUE)' in coordinator
    callers = {
        "modules/datawizard_module.R": ["create_primary_data_state_adapter(", "create_data_access_functions(", "create_modification_tracking_functions(", "create_metadata_update_functions(", "create_ui_config_management_functions(", "create_metadata_content_status(", "register_core_data_lifecycle_observers("],
        "modules/Data Wizard/datawizard_file_loader.R": ["create_primary_data_state_adapter("],
        "modules/Data Wizard/datawizard_integration.R": ["create_primary_data_state_adapter(", "create_safe_ui_system("],
        "R/session_save_restore/session_save_restore_module_registration.R": ["create_primary_data_state_adapter("],
        "modules/Data Wizard/datawizard_auto_assign.R": ["create_auto_assign_integration_adapters("],
    }
    for path, calls in callers.items():
        source = (ROOT / path).read_text(encoding="utf-8")
        for call in calls:
            assert call in source, (path, call)


def test_scoped_source_graph_is_exact_and_acyclic():
    scoped = [Path("modules/datawizard_module.R"), *FILES.values()]
    edges = []
    for path in scoped:
        source = (ROOT / path).read_text(encoding="utf-8")
        targets = re.findall(r'^source\("([^"]+)"', source, flags=re.MULTILINE)
        assert len(targets) == len(set(targets)), path
        assert targets == SOURCE_TARGETS[path.as_posix()], path
        edges.extend((path.as_posix(), target) for target in targets)
    graph = {}
    nodes = {path.as_posix() for path in scoped}
    for source, target in edges:
        if target in nodes:
            graph.setdefault(source, set()).add(target)
    visiting, visited = set(), set()
    def visit(node):
        assert node not in visiting, f"source cycle at {node}"
        if node in visited:
            return
        visiting.add(node)
        for target in graph.get(node, ()):
            visit(target)
        visiting.remove(node)
        visited.add(node)
    for node in nodes:
        visit(node)
