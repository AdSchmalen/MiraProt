# Regex Metadata Assistant migration baseline

**Historical status:** This read-only document preserves the standalone implementation as a behavioral oracle and migration inventory. The integrated implementation and the current in-app technical documentation are authoritative for present wiring and runtime architecture.

## Migration boundary and exact standalone inventory

“Shared” means the function is eligible to migrate (with tests and a recorded destination); “standalone-only” means it owns launching, private application state, standalone package installation, standalone UI, or its diagnostic harness and **must not migrate**. Names beginning with a dot remain private even if shared implementation is extracted. Line ranges extend to the next top-level function, and thus include constants/comments owned by the preceding definition.

| Function | Current source | Disposition |
|---|---|---|
| `.miraprot_noop_logger` | [`Regex_Metadata_Assistant.R:L36-L36`](../../Regex_Metadata_Assistant.R#L36-L36) | **Standalone-only; do not migrate** |
| `.miraprot_safe_value` | [`Regex_Metadata_Assistant.R:L37-L41`](../../Regex_Metadata_Assistant.R#L37-L41) | **Standalone-only; do not migrate** |
| `.miraprot_examples` | [`Regex_Metadata_Assistant.R:L42-L42`](../../Regex_Metadata_Assistant.R#L42-L42) | **Standalone-only; do not migrate** |
| `.miraprot_timed` | [`Regex_Metadata_Assistant.R:L43-L51`](../../Regex_Metadata_Assistant.R#L43-L51) | **Standalone-only; do not migrate** |
| `.miraprot_effective_debug_level` | [`Regex_Metadata_Assistant.R:L52-L60`](../../Regex_Metadata_Assistant.R#L52-L60) | **Standalone-only; do not migrate** |
| `.miraprot_console_log` | [`Regex_Metadata_Assistant.R:L61-L72`](../../Regex_Metadata_Assistant.R#L61-L72) | **Standalone-only; do not migrate** |
| `.miraprot_bootstrap_log` | [`Regex_Metadata_Assistant.R:L73-L92`](../../Regex_Metadata_Assistant.R#L73-L92) | **Standalone-only; do not migrate** |
| `ensure_packages` | [`Regex_Metadata_Assistant.R:L93-L317`](../../Regex_Metadata_Assistant.R#L93-L317) | **Standalone-only; do not migrate** |
| `is_sample_bearing_content` | [`Regex_Metadata_Assistant.R:L318-L320`](../../Regex_Metadata_Assistant.R#L318-L320) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `invalid_condition_content` | [`Regex_Metadata_Assistant.R:L321-L324`](../../Regex_Metadata_Assistant.R#L321-L324) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `condition_content_validation_messages` | [`Regex_Metadata_Assistant.R:L325-L345`](../../Regex_Metadata_Assistant.R#L325-L345) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `empty_content` | [`Regex_Metadata_Assistant.R:L346-L346`](../../Regex_Metadata_Assistant.R#L346-L346) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `empty_condition` | [`Regex_Metadata_Assistant.R:L347-L347`](../../Regex_Metadata_Assistant.R#L347-L347) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `empty_ratio` | [`Regex_Metadata_Assistant.R:L348-L349`](../../Regex_Metadata_Assistant.R#L348-L349) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `chr` | [`Regex_Metadata_Assistant.R:L350-L353`](../../Regex_Metadata_Assistant.R#L350-L353) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_escape_literal` | [`Regex_Metadata_Assistant.R:L354-L358`](../../Regex_Metadata_Assistant.R#L354-L358) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_atom_for_token` | [`Regex_Metadata_Assistant.R:L359-L378`](../../Regex_Metadata_Assistant.R#L359-L378) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_join_atoms` | [`Regex_Metadata_Assistant.R:L379-L386`](../../Regex_Metadata_Assistant.R#L379-L386) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_map_slashes` | [`Regex_Metadata_Assistant.R:L387-L403`](../../Regex_Metadata_Assistant.R#L387-L403) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_to_miraprot_storage` | [`Regex_Metadata_Assistant.R:L404-L407`](../../Regex_Metadata_Assistant.R#L404-L407) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_from_miraprot_storage` | [`Regex_Metadata_Assistant.R:L408-L412`](../../Regex_Metadata_Assistant.R#L408-L412) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_validation_result` | [`Regex_Metadata_Assistant.R:L413-L418`](../../Regex_Metadata_Assistant.R#L413-L418) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `validate_pcre` | [`Regex_Metadata_Assistant.R:L419-L426`](../../Regex_Metadata_Assistant.R#L419-L426) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `validate_stringr_pattern` | [`Regex_Metadata_Assistant.R:L427-L439`](../../Regex_Metadata_Assistant.R#L427-L439) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `safe_grepl` | [`Regex_Metadata_Assistant.R:L440-L448`](../../Regex_Metadata_Assistant.R#L440-L448) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `content_transformation_details` | [`Regex_Metadata_Assistant.R:L449-L490`](../../Regex_Metadata_Assistant.R#L449-L490) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `infer_content_transformation` | [`Regex_Metadata_Assistant.R:L491-L503`](../../Regex_Metadata_Assistant.R#L491-L503) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `normalize_transformation_values` | [`Regex_Metadata_Assistant.R:L504-L515`](../../Regex_Metadata_Assistant.R#L504-L515) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `validate_metadata` | [`Regex_Metadata_Assistant.R:L516-L571`](../../Regex_Metadata_Assistant.R#L516-L571) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.format_special_character` | [`Regex_Metadata_Assistant.R:L572-L582`](../../Regex_Metadata_Assistant.R#L572-L582) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.token_base_shape` | [`Regex_Metadata_Assistant.R:L583-L590`](../../Regex_Metadata_Assistant.R#L583-L590) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `tokens` | [`Regex_Metadata_Assistant.R:L591-L676`](../../Regex_Metadata_Assistant.R#L591-L676) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `common_values` | [`Regex_Metadata_Assistant.R:L677-L678`](../../Regex_Metadata_Assistant.R#L677-L678) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.candidate_token_rows` | [`Regex_Metadata_Assistant.R:L679-L684`](../../Regex_Metadata_Assistant.R#L679-L684) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.shape_atom` | [`Regex_Metadata_Assistant.R:L685-L695`](../../Regex_Metadata_Assistant.R#L685-L695) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.protect_literal` | [`Regex_Metadata_Assistant.R:L696-L703`](../../Regex_Metadata_Assistant.R#L696-L703) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.candidate_family_builders` | [`Regex_Metadata_Assistant.R:L704-L790`](../../Regex_Metadata_Assistant.R#L704-L790) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `candidate_fragments` | [`Regex_Metadata_Assistant.R:L791-L815`](../../Regex_Metadata_Assistant.R#L791-L815) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.regex_constraint_count` | [`Regex_Metadata_Assistant.R:L816-L824`](../../Regex_Metadata_Assistant.R#L816-L824) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `score_pattern` | [`Regex_Metadata_Assistant.R:L825-L849`](../../Regex_Metadata_Assistant.R#L825-L849) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.refinement_tier` | [`Regex_Metadata_Assistant.R:L850-L856`](../../Regex_Metadata_Assistant.R#L850-L856) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.refinement_order` | [`Regex_Metadata_Assistant.R:L857-L864`](../../Regex_Metadata_Assistant.R#L857-L864) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `.refinement_mutations` | [`Regex_Metadata_Assistant.R:L865-L908`](../../Regex_Metadata_Assistant.R#L865-L908) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `refine_pattern_search` | [`Regex_Metadata_Assistant.R:L909-L950`](../../Regex_Metadata_Assistant.R#L909-L950) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `infer_anchors` | [`Regex_Metadata_Assistant.R:L951-L979`](../../Regex_Metadata_Assistant.R#L951-L979) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `infer_content` | [`Regex_Metadata_Assistant.R:L980-L1151`](../../Regex_Metadata_Assistant.R#L980-L1151) | **Migrated:** [`datawizard_auto_regex_logic.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_logic.R) |
| `extract_condition` | [`Regex_Metadata_Assistant.R:L1152-L1209`](../../Regex_Metadata_Assistant.R#L1152-L1209) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `condition_separators` | [`Regex_Metadata_Assistant.R:L1210-L1238`](../../Regex_Metadata_Assistant.R#L1210-L1238) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `condition_contexts` | [`Regex_Metadata_Assistant.R:L1239-L1273`](../../Regex_Metadata_Assistant.R#L1239-L1273) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `infer_conditions` | [`Regex_Metadata_Assistant.R:L1274-L1416`](../../Regex_Metadata_Assistant.R#L1274-L1416) | **Migrated:** [`datawizard_auto_regex_logic.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_logic.R) |
| `ratio_between` | [`Regex_Metadata_Assistant.R:L1417-L1437`](../../Regex_Metadata_Assistant.R#L1417-L1437) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `ratio_normalize_boundaries` | [`Regex_Metadata_Assistant.R:L1438-L1445`](../../Regex_Metadata_Assistant.R#L1438-L1445) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `ratio_tokenize_exact` | [`Regex_Metadata_Assistant.R:L1446-L1468`](../../Regex_Metadata_Assistant.R#L1446-L1468) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `ratio_extract` | [`Regex_Metadata_Assistant.R:L1469-L1491`](../../Regex_Metadata_Assistant.R#L1469-L1491) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `known_samples_after_conditions` | [`Regex_Metadata_Assistant.R:L1492-L1500`](../../Regex_Metadata_Assistant.R#L1492-L1500) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `ratio_diagnostics` | [`Regex_Metadata_Assistant.R:L1501-L1516`](../../Regex_Metadata_Assistant.R#L1501-L1516) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `infer_ratios` | [`Regex_Metadata_Assistant.R:L1517-L1705`](../../Regex_Metadata_Assistant.R#L1517-L1705) | **Migrated:** [`datawizard_auto_regex_logic.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_logic.R) |
| `apply_content_table` | [`Regex_Metadata_Assistant.R:L1706-L1749`](../../Regex_Metadata_Assistant.R#L1706-L1749) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `content_assignment_summary` | [`Regex_Metadata_Assistant.R:L1750-L1767`](../../Regex_Metadata_Assistant.R#L1750-L1767) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `apply_condition_table` | [`Regex_Metadata_Assistant.R:L1768-L1799`](../../Regex_Metadata_Assistant.R#L1768-L1799) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `apply_ratio_table` | [`Regex_Metadata_Assistant.R:L1800-L1813`](../../Regex_Metadata_Assistant.R#L1800-L1813) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `test_rules` | [`Regex_Metadata_Assistant.R:L1814-L1817`](../../Regex_Metadata_Assistant.R#L1814-L1817) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `coerce_contract` | [`Regex_Metadata_Assistant.R:L1818-L1832`](../../Regex_Metadata_Assistant.R#L1818-L1832) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `data_wizard_normalize_rules` | [`Regex_Metadata_Assistant.R:L1833-L1844`](../../Regex_Metadata_Assistant.R#L1833-L1844) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `build_export_template` | [`Regex_Metadata_Assistant.R:L1845-L1865`](../../Regex_Metadata_Assistant.R#L1845-L1865) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `regex_complexity` | [`Regex_Metadata_Assistant.R:L1866-L1872`](../../Regex_Metadata_Assistant.R#L1866-L1872) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `validate_export` | [`Regex_Metadata_Assistant.R:L1873-L1980`](../../Regex_Metadata_Assistant.R#L1873-L1980) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `roundtrip_export` | [`Regex_Metadata_Assistant.R:L1981-L1994`](../../Regex_Metadata_Assistant.R#L1981-L1994) | Shared migration candidate |
| `signature_hash` | [`Regex_Metadata_Assistant.R:L1995-L2003`](../../Regex_Metadata_Assistant.R#L1995-L2003) | **Standalone-only; do not migrate** |
| `prepare_export_inputs` | [`Regex_Metadata_Assistant.R:L2004-L2062`](../../Regex_Metadata_Assistant.R#L2004-L2062) | **Migrated:** [`datawizard_auto_regex_utils.R`](../modules/Data%20Wizard/auto%20regex/datawizard_auto_regex_utils.R) |
| `export_signature` | [`Regex_Metadata_Assistant.R:L2063-L2067`](../../Regex_Metadata_Assistant.R#L2063-L2067) | **Standalone-only; do not migrate** |
| `compare_export_objects` | [`Regex_Metadata_Assistant.R:L2068-L2084`](../../Regex_Metadata_Assistant.R#L2068-L2084) | **Standalone-only; do not migrate** |
| `prepare_export_artifact` | [`Regex_Metadata_Assistant.R:L2085-L2120`](../../Regex_Metadata_Assistant.R#L2085-L2120) | **Standalone-only; do not migrate** |
| `write_cached_rds` | [`Regex_Metadata_Assistant.R:L2121-L2131`](../../Regex_Metadata_Assistant.R#L2121-L2131) | **Standalone-only; do not migrate** |
| `table_section` | [`Regex_Metadata_Assistant.R:L2132-L2144`](../../Regex_Metadata_Assistant.R#L2132-L2144) | **Standalone-only; do not migrate** |
| `inference_notes` | [`Regex_Metadata_Assistant.R:L2145-L2153`](../../Regex_Metadata_Assistant.R#L2145-L2153) | **Standalone-only; do not migrate** |
| `build_ui` | [`Regex_Metadata_Assistant.R:L2154-L2223`](../../Regex_Metadata_Assistant.R#L2154-L2223) | **Standalone-only; do not migrate** |
| `create_session_logger` | [`Regex_Metadata_Assistant.R:L2224-L2306`](../../Regex_Metadata_Assistant.R#L2224-L2306) | **Standalone-only; do not migrate** |
| `filter_session_log_entries` | [`Regex_Metadata_Assistant.R:L2307-L2317`](../../Regex_Metadata_Assistant.R#L2307-L2317) | **Standalone-only; do not migrate** |
| `session_log_frame` | [`Regex_Metadata_Assistant.R:L2318-L2346`](../../Regex_Metadata_Assistant.R#L2318-L2346) | **Standalone-only; do not migrate** |
| `cancel_application_callbacks` | [`Regex_Metadata_Assistant.R:L2347-L2358`](../../Regex_Metadata_Assistant.R#L2347-L2358) | **Standalone-only; do not migrate** |
| `cleanup_application_references` | [`Regex_Metadata_Assistant.R:L2359-L2374`](../../Regex_Metadata_Assistant.R#L2359-L2374) | **Standalone-only; do not migrate** |
| `shutdown_application` | [`Regex_Metadata_Assistant.R:L2375-L2406`](../../Regex_Metadata_Assistant.R#L2375-L2406) | **Standalone-only; do not migrate** |
| `end_application_session` | [`Regex_Metadata_Assistant.R:L2407-L2429`](../../Regex_Metadata_Assistant.R#L2407-L2429) | **Standalone-only; do not migrate** |
| `server` | [`Regex_Metadata_Assistant.R:L2430-L2639`](../../Regex_Metadata_Assistant.R#L2430-L2639) | **Standalone-only; do not migrate** |
| `run_self_tests` | [`Regex_Metadata_Assistant.R:L2640-L3583`](../../Regex_Metadata_Assistant.R#L2640-L3583) | **Standalone-only; do not migrate** |
| `browser_launch_enabled` | [`Regex_Metadata_Assistant.R:L3584-L3588`](../../Regex_Metadata_Assistant.R#L3584-L3588) | **Standalone-only; do not migrate** |
| `browser_launcher` | [`Regex_Metadata_Assistant.R:L3589-L3606`](../../Regex_Metadata_Assistant.R#L3589-L3606) | **Standalone-only; do not migrate** |
| `keep_server_available` | [`Regex_Metadata_Assistant.R:L3607-L3611`](../../Regex_Metadata_Assistant.R#L3607-L3611) | **Standalone-only; do not migrate** |
| `application_endpoint` | [`Regex_Metadata_Assistant.R:L3612-L3625`](../../Regex_Metadata_Assistant.R#L3612-L3625) | **Standalone-only; do not migrate** |
| `run_application` | [`Regex_Metadata_Assistant.R:L3626-L3632`](../../Regex_Metadata_Assistant.R#L3626-L3632) | **Standalone-only; do not migrate** |
| `launch_regex_metadata_assistant` | [`Regex_Metadata_Assistant.R:L3633-L3666`](../../Regex_Metadata_Assistant.R#L3633-L3666) | **Standalone-only; do not migrate** |
| `is_command_line_invocation` | [`Regex_Metadata_Assistant.R:L3667-L3676`](../../Regex_Metadata_Assistant.R#L3667-L3676) | **Standalone-only; do not migrate** |
| `self_test_requested` | [`Regex_Metadata_Assistant.R:L3677-L3678`](../../Regex_Metadata_Assistant.R#L3677-L3678) | **Standalone-only; do not migrate** |
| `dispatch_startup` | [`Regex_Metadata_Assistant.R:L3679-L3689`](../../Regex_Metadata_Assistant.R#L3679-L3689) | **Standalone-only; do not migrate** |
| `announce_startup` | [`Regex_Metadata_Assistant.R:L3690-L3699`](../../Regex_Metadata_Assistant.R#L3690-L3699) | **Standalone-only; do not migrate** |
| `startup_lifecycle_self_tests` | [`Regex_Metadata_Assistant.R:L3700-L3762`](../../Regex_Metadata_Assistant.R#L3700-L3762) | **Standalone-only; do not migrate** |

No function is considered migrated merely because Data Wizard has a similarly named helper. A migration change must add its destination reference beside the source reference in this table; until then, the standalone remains the behavioral oracle. The standalone-only set above is explicit: host logging replaces its logger, the app bootstrap owns package loading, Data Wizard owns UI/reactivity, and artifact download caching is a standalone workflow rather than rule semantics.

## Data Wizard metadata resolver

`resolve_current_metadata(reference_dataset_role = "primary_working")` is the authoritative resolver. Its exact precedence is: resolve the reference data through `resolve_datawizard_dataset`; use `rv$data_mod` only if that fails; accept aligned `rv$data_def`; then aligned registry `metadata_working`, then `metadata_final`; then aligned `core_values$handson_metadata()`; otherwise rebuild and publish a skeleton. Alignment is exact ordered equality between metadata `Column` and dataset names. A Row-Index-only skeleton is not meaningful metadata. The resolver and predicates are defined at [`modules/Data Wizard/datawizard_utils.R:L62-L379`](../../modules/Data%20Wizard/datawizard_utils.R#L62-L379); integration consumers use it rather than choosing metadata aliases themselves at [`modules/Data Wizard/datawizard_integration.R:L128-L143`](../../modules/Data%20Wizard/datawizard_integration.R#L128-L143).

## Auto-Assign public API

The only parent entrypoints are `modAutoAssignUI(id)` and `modAutoAssignServer(...)`, defined at [`modules/Data Wizard/datawizard_auto_assign.R:L64-L101`](../../modules/Data%20Wizard/datawizard_auto_assign.R#L64-L101). Server namespace authority is `session$ns`, not a reconstructed `NS(id)`, at [`modules/Data Wizard/datawizard_auto_assign.R:L102-L105`](../../modules/Data%20Wizard/datawizard_auto_assign.R#L102-L105).

The returned public list (and therefore compatibility surface) begins at [`modules/Data Wizard/datawizard_auto_assign.R:L413-L570`](../../modules/Data%20Wizard/datawizard_auto_assign.R#L413-L570): session get/set; imputation collect/apply/state; enhanced/basic template status; direct rule loading; extracted-condition reactive; rule application; content/condition/ratio rule reactives; extracted conditions; filtering collect/apply/state; edit collect/apply/state; ratio collect/apply/state; current UI config get/set; `has_rules`; central-load state; processing/health/error/history state (continued in the same return list). Internal factory products are not public unless present in this returned list.

## Canonical persisted schemas

The canonical constants and allowlists are at [`Regex_Metadata_Assistant.R:L266-L345`](../../Regex_Metadata_Assistant.R#L266-L345), constructors/coercion at [`Regex_Metadata_Assistant.R:L346-L348`](../../Regex_Metadata_Assistant.R#L346-L348) and [`Regex_Metadata_Assistant.R:L1818-L1844`](../../Regex_Metadata_Assistant.R#L1818-L1844), and validation at [`Regex_Metadata_Assistant.R:L1873-L1980`](../../Regex_Metadata_Assistant.R#L1873-L1980).

* `table`: `Content`, `Include`, `Exclude`, `Transformation`, all character; one canonical literal Row Index rule; unsupported content has `NA` transformation.
* `condition`: `Content`, `Method`, `Before`, `After`, `Separators` (character), `Pos` (integer). Allowed methods: `between`, `start`, `end`, `whole`, `phrase_position`, `pattern_detect`.
* `ratio`: `Content`, `Method`, `Separators` (character), `Invert` (logical), `NumBefore`, `NumAfter`, `DenBefore`, `DenAfter` (character), `NumPos`, `DenPos` (integer), preserving method-specific `NA`s.
* Container: first named components are `table`, `condition`, `ratio`; optional `debug_info` and loader-compatible configuration payloads follow. The observed shipped-file details are recorded beside the code at [`Regex_Metadata_Assistant.R:L220-L265`](../../Regex_Metadata_Assistant.R#L220-L265).

## Logging behavior

The host recorder always stores an event, fans it into buffers for the selected detail level and above, caps each FIFO buffer at 5,000, increments the version token, and gates only console output using live `DEBUG_LEVEL`; changing level never replays old console records. This is implemented at [`R/bootstrap.R:L139-L261`](../../R/bootstrap.R#L139-L261). Data Wizard and Auto-Assign call that recorder with tags and only fall back to leveled console output when unavailable at [`modules/Data Wizard/datawizard_utils.R:L34-L50`](../../modules/Data%20Wizard/datawizard_utils.R#L34-L50) and [`modules/Data Wizard/datawizard_auto_assign.R:L108-L125`](../../modules/Data%20Wizard/datawizard_auto_assign.R#L108-L125). The standalone logger is bounded, structured, severity/detail-filtered, and observability-only; its contract begins at [`Regex_Metadata_Assistant.R:L9-L92`](../../Regex_Metadata_Assistant.R#L9-L92) and its session implementation is [`Regex_Metadata_Assistant.R:L2224-L2346`](../../Regex_Metadata_Assistant.R#L2224-L2346). Do not migrate the standalone logger into the host.

## Namespace derivation and collapsible panels

Module UI roots derive `ns <- NS(id)`; server logic derives `ns <- session$ns`. Auto-Assign demonstrates both at [`modules/Data Wizard/auto assign/datawizard_auto_assign_UI.R:L705-L706`](../../modules/Data%20Wizard/auto%20assign/datawizard_auto_assign_UI.R#L705-L706) and [`modules/Data Wizard/datawizard_auto_assign.R:L102-L105`](../../modules/Data%20Wizard/datawizard_auto_assign.R#L102-L105). Never concatenate a module id to recreate nested namespaces.

The integrated rendering and lifecycle owner is `modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_render.R`; the explicit coordinator loads it and calls it last. This extraction changes neither the proposed grouped-rule schema nor runtime behavior.

Data Wizard’s current collapsible pattern pairs `toggle_<name>` with `<name>_content` and `<name>_icon`; the observer calls `shinyjs::toggle`, then switches `fa-chevron-right`/`fa-chevron-down` from click parity. The canonical handler and supported section names are [`modules/Data Wizard/datawizard_integration.R:L1496-L1523`](../../modules/Data%20Wizard/datawizard_integration.R#L1496-L1523). Use this pattern for migrated panels rather than introducing `shinyBS`; `shinyBS` remains an app dependency only because bootstrap currently loads it.

## Package availability

Standalone requires exactly `shiny`, `readxl`, `DT`, `openxlsx`, and `stringr`, checks namespaces, installs missing CRAN packages, rechecks, and emits actionable failures at [`Regex_Metadata_Assistant.R:L9-L15`](../../Regex_Metadata_Assistant.R#L9-L15) and [`Regex_Metadata_Assistant.R:L93-L139`](../../Regex_Metadata_Assistant.R#L93-L139). The host already requires all five in its fail-fast CRAN bootstrap list, along with `shinyjs` for panel toggles, at [`R/bootstrap.R:L43-L116`](../../R/bootstrap.R#L43-L116). Migrated code must not install packages or wrap imports; it relies on bootstrap and may use `requireNamespace` only for genuinely optional behavior.

## Golden Excel comparison

The text-only golden sources are [`content.csv`](../../tests/fixtures/regex_metadata_assistant/content.csv), [`condition.csv`](../../tests/fixtures/regex_metadata_assistant/condition.csv), and [`ratio.csv`](../../tests/fixtures/regex_metadata_assistant/ratio.csv). They define the `content`, `condition`, and `ratio` sheets covering transformation preservation, sample-bearing condition inference plus standalone-only/non-applicable content, and a one-row ratio. `run_self_tests()` generates a temporary Excel workbook with `openxlsx`, reads it through `readxl`, asserts sheet names and current outcomes, replays condition rules, and verifies reliable/applicable counts at [`Regex_Metadata_Assistant.R:L2991-L3033`](../../Regex_Metadata_Assistant.R#L2991-L3033). Thus the committed fixtures remain reviewable text while still exercising the real Excel boundary. These assertions—not a prose paraphrase—are the golden result. Update the CSV sources and assertions in the same reviewed change if behavior intentionally changes.

Current golden expectations are: `log2`, `log10`, and `-log10` are retained for their three transformation-capable content rows; only `Raw Abundance` receives the condition rule while Description, Identifier, Row Index, and # PSMs are `not_applicable`; condition replay preserves every expected `Options` value; and the ratio sheet produces one reliable rule with one applicable and one successful row.

## Decision checklist

Before implementation, cite: the source function row above; resolver precedence; returned Auto-Assign API member (if externally visible); exact schema and NA semantics; host logger/tag/level; `NS(id)` or `session$ns` according to side; bootstrap availability; and the existing toggle pattern. If repository behavior changes, update this baseline and the golden assertion in the same commit.
