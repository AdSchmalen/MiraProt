# ./Documentation/datawizard_doc_data.R
# Datawizard Documentation — Data Processing Modules Technical Documentation
#
# Contains content rendering functions for data processing submodules:
#   - Filtering (confidence, valid values, custom column filters)
#   - Data Editing (replace, edit operations queue)
#   - Missing Data Handling / Imputation
#   - Ratios & Statistics
#   - Basemean Calculation
#
# These functions are called by the documentation server in datawizard_doc_ui.R.

############
# Filtering Module

render_tech_filtering_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Technical Documentation — Filtering Module"),
    tags$p(
      "The Filtering module removes low-quality rows from the Primary dataset prior to downstream analysis. ",
      "It implements three tabs with complementary responsibilities: ",
      tags$b("Confidence"), " (numeric or string-based confidence filtering), ",
      tags$b("Valid Values"), " (row-wise completeness rules), and ",
      tags$b("Custom"), " (ad-hoc column predicates with AND/OR logic and multi-column aggregation including SOME). ",
      "The module is resilient to partial UI state, validates inputs, and supports robust export/import of UI configuration ",
      "for RDS-based workflows."
    ),

    tags$h3("Module Contract"),
    tags$p("UI & Server signatures and the primary integration surface:"),
    tags$pre(class = "r", paste0(
      'modFilteringUI <- function(id) { ... }\n\n',
      'modFilteringServer <- function(\n',
      '  id,\n',
      '  data,                     # reactive: data.frame (Primary data to filter)\n',
      '  metadata_def,             # reactive: conditions/measurement column mapping\n',
      '  init_meta = NULL,         # optional\n',
      '  UI_config = reactive(NULL),        # optional incoming UI config (RDS import)\n',
      '  metadata_ready_status = reactive(TRUE),\n',
      '  debug_level = 1\n',
      ') {\n',
      '  # returns a list of helpers/reactives (see below)\n',
      '}\n'
    )),
    tags$p("Returned interface (selected):"),
    tags$ul(
      tags$li(tags$code("get_filter_state()"), " — reactive list of current filter state (confidence, valid_values, custom, some_settings)."),
      tags$li(tags$code("get_custom_conditions()"), " — reactive ", tags$code("data.frame"), " representing the Custom rules table."),
      tags$li(tags$code("get_filter_results()"), " — reactive results list (filtered data, rows_removed, warnings, errors, debug_info)."),
      tags$li(tags$code("is_processing()"), " / ", tags$code("has_errors()"), " — reactives for status/health."),
      tags$li(tags$code("perform_filtering()"), " — executes filtering with current inputs."),
      tags$li(tags$code("apply_filter_state(new_state)"), " — programmatically applies a state bundle (used after RDS import)."),
      tags$li(tags$code("force_ui_update(ui_config)"), " — force-synchronizes UI to a config payload (guarded safe updates)."),
      tags$li(tags$code("get_current_ui_values()"), " — snapshot of actual UI inputs (preferred export for RDS)."),
      tags$li(tags$code("get_current_filter_state_for_export()"), " — consolidated export bundle for Auto-Assign/RDS pipelines.")
    ),

    tags$h3("Data & Metadata Contracts"),
    tags$ul(
      tags$li(tags$b("Input data:"), " ", tags$code("data()"), " must return a ", tags$code("data.frame"),
              " that includes measurement columns and the confidence column(s) referenced by the UI."),
      tags$li(tags$b("Metadata:"), " ", tags$code("metadata_def()"), " supplies group/condition definitions and identifies measurement columns (used by Valid Values and as defaults for Custom)."),
      tags$li(tags$b("Ready signal:"), " ", tags$code("metadata_ready_status()"), " blocks execution until metadata is ready, preventing premature filtering.")
    ),

    tags$h3("Tabs & Logic"),
    tags$h4("1) Confidence"),
    tags$p(
      "Purpose: remove low-confidence proteins based on numeric and/or string criteria. ",
      "If both numeric and string filters are enabled, the row must pass ", tags$b("both"), " (logical AND)."
    ),
    tags$p("Key inputs (module-scoped via ", tags$code("ns()"), "):"),
    tags$ul(
      tags$li(tags$code("numeric_fdr_dw"), " — enable numeric confidence filtering."),
      tags$li(tags$code("numeric_input_dw"), " — minimum acceptable value (keep rows with value ≥ min)."),
      tags$li(tags$code("numeric_input_dw_max"), " — optional maximum (keep rows with value ≤ max)."),
      tags$li(tags$code("string_fdr_dw"), " — enable character confidence filtering."),
      tags$li(tags$code("string_input_dw"), " — substring/pattern to match (case-insensitive).")
    ),
    tags$p("Behavior:"),
    tags$ul(
      tags$li("Numeric enabled → keep rows with values within [min, max] (max optional)."),
      tags$li("String enabled → keep rows whose confidence label contains the provided token."),
      tags$li("Both enabled → numeric AND string must pass.")
    ),
    tags$p(tags$i("Edge cases:"), " if the numeric column is missing/non-numeric, numeric filtering is skipped with a warning; empty string tokens disable string filtering."),

    tags$h4("2) Valid Values"),
    tags$p(
      "Purpose: ensure sufficient measured values (non-missing) per row to support downstream analyses. ",
      "Measurement columns are derived from ", tags$code("metadata_def()"), "."
    ),
    tags$p("Key inputs:"),
    tags$ul(
      tags$li(tags$code("valid_filtering_group_dw"), " — scope for counting valid values: ",
              tags$b("In total"), " (across all measures), ",
              tags$b("One group"), " (any group passes), ",
              tags$b("Each group"), " (every group passes)."),
      tags$li(tags$code("valid_filtering_value_dw"), " — minimal number of valid (non-missing) values required within the chosen scope.")
    ),
    tags$p("Behavior:"),
    tags$ul(
      tags$li(tags$b("In total:"), " keep if ", tags$code("rowSums(!is.na(measures)) >= min_count"), "."),
      tags$li(tags$b("One group:"), " keep if any group's non-missing count ≥ min_count."),
      tags$li(tags$b("Each group:"), " keep if every group's non-missing count ≥ min_count.")
    ),
    tags$p(tags$i("Edge cases:"), " no measurement columns → skip with warning; non-positive thresholds effectively disable the filter."),

    tags$h4("3) Custom"),
    tags$p(
      "Purpose: enable power users to define arbitrary row predicates per column and combine them across columns. ",
      "Two per-column conditions can be combined with ", tags$b("AND / OR"), "; across columns use ", tags$b("OR / AND / SOME"),
      " where SOME supports at-least/less-than/exactly N columns matching."
    ),
    tags$p("Key inputs & UI:"),
    tags$ul(
      tags$li(tags$code("filter_column_dw"), " — one or more columns to test."),
      tags$li("Per-column conditions: ",
              tags$code("filter_operator_ui_dw_1"), ", ", tags$code("filter_value_ui_dw_1"),
              " and optionally ",
              tags$code("filter_operator_ui_dw_2"), ", ", tags$code("filter_value_ui_dw_2"), "."),
      tags$li(tags$code("filter_logic_dw"), " — combine Condition 1 & 2 via AND / OR."),
      tags$li(tags$code("multi_column_logic"), " — OR (ANY), AND (ALL), SOME."),
      tags$li(tags$code("some_operator"), " — for SOME: ", tags$code("at_least"), " / ", tags$code("less_than"), " / ", tags$code("exactly"), "."),
      tags$li(tags$code("some_count"), " — integer N for SOME."),
      tags$li(tags$code("filter_empty_dw"), " — policy for NA/empty values (treated explicitly before evaluation).")
    ),
    tags$p("Behavior:"),
    tags$ul(
      tags$li("Build per-column predicate from up to two conditions (numeric: <, ≤, ==, ≥, >, !=; string: contains/startsWith/endsWith/equality; plus emptiness checks)."),
      tags$li("Combine the two conditions via AND / OR (if both provided)."),
      tags$li("Aggregate across selected columns using OR / AND / SOME:"),
      tags$ul(
        tags$li(tags$b("OR:"), " keep if any selected column satisfies the predicate."),
        tags$li(tags$b("AND:"), " keep if all selected columns satisfy the predicate."),
        tags$li(tags$b("SOME:"), " compute match_count across selected columns and compare via ", tags$code("some_operator"), " to ", tags$code("some_count"), ".")
      )
    ),
    tags$p(tags$i("Validation:"), " the rule table is validated (required columns present, operators recognized); unknown columns/operators are logged and skipped with notifications."),

    tags$h3("State & Export/Import"),
    tags$p("Internal state is tracked in a structured list. Export favors real UI values; import updates UI safely and synchronizes internals."),
    tags$pre(class = "r", paste0(
      "filter_state <- list(\n",
      "  confidence = list(\n",
      "    numeric_enabled = FALSE, numeric_min = 0.05, numeric_max = NA,\n",
      "    string_enabled = FALSE, string_input = \"\"\n",
      "  ),\n",
      "  valid_values = list(\n",
      "    group_selection = \"In total\",  # or \"One group\" / \"Each group\"\n",
      "    min_count = 1\n",
      "  ),\n",
      "  custom_conditions = data.frame(),  # Column, Operator_1, Value_1, Operator_2, Value_2, Multi_Column_Logic, Some_Operator, Some_Count\n",
      "  some_settings = list(\n",
      "    multi_column_logic = \"OR\",\n",
      "    some_operator = \"at_least\",\n",
      "    some_count = 1\n",
      "  )\n",
      ")\n\n",
      "# Preferred export/import helpers\n",
      "filt$get_current_ui_values()                # exact UI snapshot for RDS export\n",
      "filt$get_current_filter_state_for_export()  # consolidated bundle for Auto-Assign\n",
      "filt$apply_filter_state(bundle)             # apply on import\n"
    )),

    tags$h3("Error Handling, Validation & Notifications"),
    tags$ul(
      tags$li(tags$code("datawizard_filtering_utils.R"), " provides: ",
              tags$code("validate_filter_table_integrity()"), ", ",
              tags$code("sanitize_filter_input()"), " and a family of safe-check helpers."),
      tags$li("UI/data issues are wrapped with ", tags$code("tryCatch"), " and logged via ", tags$code("debug_log(msg, level)"),
              " (filtered by ", tags$code("debug_level"), ")."),
      tags$li("User-facing messages use a centralized notifier, e.g. ",
              tags$code("show_filter_notification(message, type, context, session, debug_level)"), ".")
    ),

    tags$h3("Performance & Safety"),
    tags$ul(
      tags$li("All filters operate row-wise over the current ", tags$code("data()"), "."),
      tags$li("Valid Values and Custom (SOME) aggregate vectorized predicates efficiently; NA handling is explicit."),
      tags$li("Empty data frames are handled gracefully (warnings, no hard failure).")
    ),

    tags$h3("Integration Guide (Orchestrator / Core)"),
    tags$p("Typical wiring in the datawizard integration layer:"),
    tags$pre(class = "r", paste0(
      "# Server wiring\n",
      "filt <- modFilteringServer(\n",
      "  id = \"filtering\",\n",
      "  data = core$get_primary_data(),\n",
      "  metadata_def = core$get_metadata_def(),\n",
      "  UI_config = autoassign$get_ui_config_for(\"filtering\"),\n",
      "  metadata_ready_status = core$metadata_ready_status,\n",
      "  debug_level = 1\n",
      ")\n\n",
      "# UI\n",
      "modFilteringUI(\"filtering\")\n\n",
      "# Export current filtering state into RDS bundle\n",
      "onExportConfig(function() {\n",
      "  list(filtering = filt$get_current_filter_state_for_export())\n",
      "})\n\n",
      "# Apply imported state after RDS import\n",
      "observeEvent(imported_config$filtering, ignoreInit = TRUE, {\n",
      "  filt$apply_filter_state(imported_config$filtering)\n",
      "})\n"
    )),
    tags$p(tags$i("Structural changes:"), " if upstream transformations (e.g., Pivot/Merge) alter the data shape, ",
           "Integration should reset or re-run Filtering (e.g., via a module-level reset trigger) to avoid stale predicates."),

    tags$h3("Extension Points"),
    tags$ul(
      tags$li("Add new comparison operators by extending the operator UI factories and the predicate evaluator mapping."),
      tags$li(
        "Support additional confidence sources by exposing/selecting the appropriate columns via ",
        tags$code("metadata_def()"),
        " and augmenting the Confidence tab if multiple columns are available."
      ),
      tags$li(
        "If downstream modules require kept row indices, extend ",
        tags$code("get_filter_results()"),
        " to expose ",
        tags$code("kept_idx"),
        "."
      )
    )
  )
}

############
# Edit Module

render_tech_edit_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Technical Documentation — Edit Module"),
    tags$p(
      "The Edit module provides queued data replacement and editing operations on selected columns. ",
      "Operations can be added via UI controls, imported as an operations table, executed in sequence, ",
      "and audited via a log. The module exposes a clear contract for integration with the core data bus ",
      "and metadata definition."
    ),

    tags$h3("Module Contract"),
    tags$p("UI & Server signatures and parameters:"),
    tags$pre(
      class = "r",
      paste0(
        'modEditUI <- function(id) { ... }\n\n',
        'modEditServer <- function(\n',
        '  id,\n',
        '  get_data,                 # function: returns current data.frame\n',
        '  set_data,                 # function: accepts updated data.frame\n',
        '  metadata_def,             # reactive: metadata definition (expects columns Content, Column)\n',
        '  operations_table = reactive(NULL),  # reactive: optional imported operations (see schema below)\n',
        '  debug_level = 1\n',
        ') { ... }\n'
      )
    ),
    tags$p("Returned helpers (selected):"),
    tags$ul(
      tags$li(tags$code("get_current_data()"), " — reactive wrapper around ", tags$code("get_data()"), "."),
      tags$li(tags$code("has_original_data()"), " — reactive flag for reset availability."),
      tags$li(tags$code("get_pending_operations()"), " — reactive pending operations data.frame."),
      tags$li(tags$code("get_operation_log()"), " — reactive character vector with log entries.")
    ),

    tags$h3("Data & Metadata Contracts"),
    tags$ul(
      tags$li(
        tags$b("Input data:"), " ", tags$code("get_data()"),
        " must return a ", tags$code("data.frame"), " whose columns match the names referenced in operations."
      ),
      tags$li(
        tags$b("Metadata:"), " ", tags$code("metadata_def()"),
        " is used to populate the UI selectors. The code reads ",
        tags$code("metadata$Content"), " (category) and ", tags$code("metadata$Column"), " (column names)."
      ),
      tags$li(
        tags$b("Column availability:"), " the module intersects the selected metadata columns with ",
        "current data column names to ensure only existing columns are offered."
      )
    ),

    tags$h3("UI Structure"),
    tags$ul(
      tags$li(tags$code("template_status_display"), " — compact status line (queued, pending, executed, source, warnings, progress)."),
      tags$li(tags$code("category_select"), " — select a Content category from ", tags$code("metadata_def()$Content"), "."),
      tags$li(tags$code("column_select"), " — multi-select columns (filtered by the chosen category and existing in data)."),
      tags$li(tags$code("replace_controls"), " — context-aware controls for replacements (character or numeric columns)."),
      tags$li(tags$code("edit_controls"), " — context-aware controls for edits (character or numeric columns)."),
      tags$li(tags$code("add_replace"), " / ", tags$code("add_edit"), " — add an operation to the queue."),
      tags$li(tags$code("operations_table"), " — ", tags$code("rHandsontableOutput"), " of queued operations."),
      tags$li(tags$code("apply_all_operations"), " — execute all non-executed queued operations."),
      tags$li(tags$code("clear_operations"), " — clear the queue."),
      tags$li(tags$code("reset_edits"), " — restore original data snapshot."),
      tags$li(tags$code("show_log"), " + ", tags$code("operation_log"), " — toggle and display the operation log.")
    ),

    tags$h3("Operations Table Schema"),
    tags$p("Queued operations are stored in a data.frame with the following columns:"),
    tags$pre(
      class = "r",
      paste0(
        "data.frame(\n",
        "  Operation   = c(\"Replace\" | \"Edit\"),\n",
        "  Type        = c(\"character\" | \"numeric\"),\n",
        "  Columns     = \"col1|col2|...\",   # pipe-separated column names\n",
        "  Parameters  = \"key:value;key2:value2\",   # serialized parameter string\n",
        "  Description = \"human-readable summary\",\n",
        "  Executed    = logical()\n",
        ")\n"
      )
    ),
    tags$p(
      "The module validates imported tables and ensures required columns exist. ",
      "If ", tags$code("Executed"), " is missing in imported data, it is added and set to ", tags$code("FALSE"),
      " for backwards compatibility."
    ),

    tags$h3("Adding Operations (UI)"),
    tags$h4("Character Columns"),
    tags$p("Replacement controls (", tags$code("replace_controls"), "):"),
    tags$ul(
      tags$li(tags$code("string_search_type"), " — one of ", tags$code("contains / is equal / starts with / ends with"), "."),
      tags$li(tags$code("string_search_term"), " — term to find."),
      tags$li(tags$code("string_replace_type"), " — ", tags$code("Replace cell"), ", ", tags$code("Replace substring"), " or ", tags$code("Clear cell"), "."),
      tags$li(tags$code("string_replacement"), " — replacement string; unused by ", tags$code("Clear cell"), ".")
    ),
    tags$p("Edit controls (", tags$code("edit_controls"), "):"),
    tags$ul(
      tags$li(tags$code("string_edit_text"), " — text to add."),
      tags$li(tags$code("string_edit_position"), " — ", tags$code("Before"), " or ", tags$code("After"), ".")
    ),

    tags$h4("Numeric Columns"),
    tags$p("Replacement controls (", tags$code("replace_controls"), "):"),
    tags$ul(
      tags$li(tags$code("numeric_operator"), " — comparison operator ", tags$code("<, <=, ==, !=, >=, >"), "."),
      tags$li(tags$code("numeric_threshold"), " — comparison threshold."),
      tags$li(tags$code("numeric_replace_with"), " — ", tags$code("NA"), " or ", tags$code("Numeric"), "."),
      tags$li(tags$code("numeric_replacement_value"), " — numeric replacement value (required if ", tags$code("Numeric"), ").")
    ),
    tags$p("Edit controls (", tags$code("edit_controls"), "):"),
    tags$ul(
      tags$li(tags$code("numeric_operation"), " — one of ", tags$code("Add, Subtract, Multiply, Divide, log, -log, raise to the power of"), "."),
      tags$li(tags$code("numeric_operation_value"), " — value for Add/Subtract/Multiply/Divide/Power (required per operation)."),
      tags$li(tags$code("numeric_base"), " — base for ", tags$code("log"), " / ", tags$code("-log"), " (must be > 0 and != 1)."),
      tags$li(tags$code("numeric_exponent"), " — exponent for ", tags$code("raise to the power of"), ".")
    ),

    tags$h3("Operation Execution"),
    tags$p(
      "Clicking ", tags$code("Apply Queue"), " (", tags$code("apply_all_operations"), ") executes all non-executed operations in order. ",
      "Each operation is applied via ", tags$code("apply_single_operation()"), " and, for multi-column targets, ",
      tags$code("apply_multi_column_operation()"), ". Successful applications update the working data and mark the operation as executed."
    ),
    tags$ul(
      tags$li("Character replacements use ", tags$code("apply_string_replacement()"), "."),
      tags$li("Numeric replacements use ", tags$code("apply_numeric_replacement()"), " with ", tags$code("safe_numeric_conversion()"), "."),
      tags$li("Character edits use ", tags$code("apply_string_edit()"), "."),
      tags$li("Numeric edits use ", tags$code("apply_numeric_edit()"), ".")
    ),

    tags$h3("Importing an Operations Table"),
    tags$p(
      "If the optional ", tags$code("operations_table"), " reactive is provided, the server validates and loads it into the queue. ",
      "Validation is performed by ", tags$code("validate_operations_table(operations_df, available_columns, reset_executed = TRUE)"),
      ". Missing or non-existent target columns are removed or updated; warnings are collected. The Executed flags are reset to ",
      tags$code("FALSE"), " by default."
    ),

    tags$h3("Parameters Serialization"),
    tags$p(
      "Operations persist their configuration in the ", tags$code("Parameters"), " column using key-value pairs. ",
      "Utilities ", tags$code("serialize_parameters(list)"), " and ", tags$code("deserialize_parameters(string)"),
      " handle escaping of special characters and robust parsing."
    ),

    tags$h3("Logging & Status"),
    tags$ul(
      tags$li("A textual log is maintained and exposed via ", tags$code("operation_log"), " (UI) and ", tags$code("get_operation_log()"), "."),
      tags$li("The status banner aggregates counts (total, pending, executed), source information for imports, warnings, and in-progress state."),
      tags$li("Progress feedback is shown while applying the queue.")
    ),

    tags$h3("Reset & Data Integrity"),
    tags$p(
      "On initialization, the module snapshots the original dataset along with an MD5 hash. ",
      "When ", tags$code("reset_edits"), " is triggered, the original snapshot is restored via ", tags$code("set_data()"), ". ",
      "If the data structure changes (e.g., column count or names), the original snapshot is refreshed."
    ),

    tags$h3("Error Handling & Validation"),
    tags$ul(
      tags$li("All critical sections are wrapped in ", tags$code("tryCatch"), " with debug output gated by ", tags$code("debug_level"), "."),
      tags$li("Numeric paths use ", tags$code("safe_numeric_conversion()"), " and guard against invalid operations (division by zero, invalid log base, non-positive values for log)."),
      tags$li("Import validation ensures required columns exist and that only supported ", tags$code("Operation"), " and ", tags$code("Type"), " values are accepted.")
    ),

    tags$h3("Function Reference (Utilities)"),
    tags$ul(
      tags$li(tags$code("detect_column_type(vector) → \"numeric\" | \"character\" | \"mixed\" | \"unknown\"")),
      tags$li(tags$code("detect_multi_column_types(data_df, column_names) → list(overall_type, individual_types, type_summary, compatible, existing_columns)")),
      tags$li(tags$code("serialize_parameters(list) → character")),
      tags$li(tags$code("deserialize_parameters(character) → list")),
      tags$li(tags$code("validate_operations_table(df, available_columns, reset_executed, debug_log) → list(operations, success, removed_count, warnings)")),
      tags$li(tags$code("apply_string_replacement(data, search_type, search_term, replace_type, replacement) → list(data, success, matches_found, operation_description)"), " where ", tags$code("replace_type"), " also supports ", tags$code("Clear cell"), "."),
      tags$li(tags$code("apply_numeric_replacement(data, operator, threshold, replace_with, replacement_value) → list(data, success, matches_found, operation_description)")),
      tags$li(tags$code("apply_string_edit(data, edit_text, position) → list(data, success, operation_description)")),
      tags$li(tags$code("apply_numeric_edit(data, operation, value, base) → list(data, success, operation_description)")),
      tags$li(tags$code("apply_multi_column_operation(data_df, column_names, operation_function, ...) → list(success, data, overall_summary, operation_summaries, errors)")),
      tags$li(tags$code("apply_single_operation(data_df, operation, debug_log) → list(success, data, message)")),
      tags$li(tags$code("safe_numeric_conversion(vector) → list(data, success, errors, conversion_rate)"))
    )
  )
}

############
# Imputation Module

render_tech_imputation_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Technical Documentation — Missing Data Handling"),
    tags$p(
      "This module implements missing-value handling for the Primary dataset. ",
      "It lets users choose an imputation method, select data types (from metadata Content) to impute, ",
      "run the imputation, inspect validation and analysis summaries, and reset imputed results."
    ),

    tags$h3("Module Contract"),
    tags$pre(class = "r", paste0(
      "modImputationUI <- function(id) { ... }\n\n",
      "modImputationServer <- function(\n",
      "  id,\n",
      "  data,                       # reactive: data.frame to read from if get_data is not supplied\n",
      "  data_def,                   # reactive: metadata definition (expects columns Content, Options, Sample, Transformation, Column)\n",
      "  get_data = NULL,            # function(): data.frame, preferred read path\n",
      "  set_data = NULL,            # function(updated_df): bool, preferred write path\n",
      "  UI_config = NULL,           # reactive or list with UI values for import\n",
      "  metadata_ready_status = reactive({ FALSE }),\n",
      "  debug_level = 1\n",
      ") { ... }\n"
    )),
    tags$p("Selected server-side helpers exposed from within the module:"),
    tags$ul(
      tags$li(tags$code("get_current_ui_values()"), " — returns ", tags$code("list(imputation_method_select, imputation_column_select)"), "."),
      tags$li(tags$code("get_current_imputation_state_for_export()"), " — returns method, selected Content types, flags and timestamps for export."),
      tags$li(tags$code("get_imputation_ui_config_for_export()"), " — returns a minimal, importable UI configuration."),
      tags$li(tags$code("set_imputation_ui_config_from_import(config)"), " — applies an imported UI configuration safely."),
      tags$li(tags$code("module_health_check()"), " — returns a list with module health indicators.")
    ),

    tags$h3("UI Structure"),
    tags$ul(
      tags$li(
        tags$code("imputation_method_select"), " (", tags$code("selectInput"), "): ",
        "method selector with choices ",
        tags$code("None"), ", ", tags$code("left-censored"), ", ", tags$code("Random forest"), ", ", tags$code("MICE - CART"), "."
      ),
      tags$li(
        tags$code("imputation_column_select"), " (", tags$code("selectInput, multiple"), "): ",
        "selects metadata ", tags$code("Content"), " types (\"Data Types to Impute\"). Choices are populated from ", tags$code("data_def()"), "."
      ),
      tags$li(
        tags$code("apply_imputation_btn"), " / ", tags$code("reset_imputation_btn"),
        " (", tags$code("actionButton"), "): run imputation or remove imputed columns."
      ),
      tags$li(
        tags$code("data_validation_status"), " (", tags$code("renderText"), "): ",
        "summary of validation (row/column counts, missing values, warnings/errors, resource estimates, missingness analysis)."
      ),
      tags$li(
        tags$code("imputation_status_display"), " (", tags$code("renderText"), "): ",
        "status after execution (method, affected Content types, number of values imputed, presence of imputed columns, quality score, processing time, processing state)."
      ),
      tags$li("Informational ", tags$code("conditionalPanel"), " sections describe the selected method.")
    ),

    tags$h3("Imputation Methods"),
    tags$p("The server dispatches by the selected method (", tags$code("input$imputation_method_select"), ") using a common wrapper."),
    tags$ul(
      tags$li(
        tags$b("Left-censored:"), " calls ",
        tags$code("performGenericImputation(current_data, current_data_def, selected_columns, performLeftCensoredImputation, \"Imputed \")"),
        ". The column-wise algorithm is defined in ", tags$code("performLeftCensoredImputation()"), " (see ", tags$code("utils.R"), ")."
      ),
      tags$li(
        tags$b("Random forest:"), " cleans parallel workers if available (", tags$code("clean_open_clusters()"), "), then calls ",
        tags$code("performGenericImputation(..., impute_random_forest, \"Imputed \")"),
        ". The RF routine is defined in ", tags$code("impute_random_forest()"), " (", tags$code("utils.R"), ")."
      ),
      tags$li(
        tags$b("MICE - CART:"), " calls ",
        tags$code("performGenericImputation(..., impute_mice_cart, \"Imputed \")"),
        ". The CART routine is defined in ", tags$code("impute_mice_cart()"), " (", tags$code("utils.R"), ")."
      ),
      tags$li(
        tags$b("None:"), " imputation is not run; validation/status renderers still operate if a method is selected."
      )
    ),

    tags$h3("Column/Metadata Resolution"),
    tags$p(
      "The module expects ", tags$code("imputation_column_select"), " to contain metadata ", tags$code("Content"), " values. ",
      "The utility ", tags$code("performGenericImputation()"), " (", tags$code("utils.R"), ") maps each selected Content ",
      "to concrete column names using ", tags$code("data_def$Options"), " and appends new columns with prefix ", tags$code("\"Imputed \""), ". ",
      "It also appends corresponding rows to ", tags$code("data_def"), " (", tags$code("Content = paste0(\"Imputed \", Content)"), ")."
    ),

    tags$h3("Validation & Analysis"),
    tags$ul(
      tags$li(
        tags$code("validate_data_for_imputation(df, selected_columns, method)"),
        ": checks presence of data, counts missingness, and adds warnings for wide ranges, very high missingness (>", tags$code("90%"), "), and constant columns. ",
        "Returns ", tags$code("list(valid, warnings, errors, info, column_count, missing_count)"), "."
      ),
      tags$li(
        tags$code("analyze_missing_patterns_safe(current_data, selected_columns)"),
        ": computes ", tags$code("overall_missing_rate"), " and a heuristic ", tags$code("mechanism_estimate"),
        " (", tags$code("Likely MCAR/MAR/MNAR"), ")."
      ),
      tags$li(
        tags$code("estimate_memory_safe(current_data, selected_method, selected_columns)"),
        ": estimates peak memory usage and a warning level based on data size and method."
      )
    ),

    tags$h3("Execution Flow"),
    tags$ol(
      tags$li("Guard against concurrent runs via ", tags$code("processing_active()"), " and update ", tags$code("current_processing_step()"), "."),
      tags$li("Read inputs using ", tags$code("get_current_data()"), " and ", tags$code("input$imputation_column_select"), "."),
      tags$li("Run validation (", tags$code("validate_data_for_imputation()"), "), missingness analysis (cache), and memory estimate (cache)."),
      tags$li(
        "Dispatch to the selected method via ",
        tags$code("performGenericImputation(data, data_def, imputation_columns, impute_fun, prefix = \"Imputed \")"),
        "."
      ),
      tags$li(
        "On success: ",
        tags$code("set_current_data(result$data)"),
        ", set ", tags$code("imputation_applied(TRUE)"), ", update method/columns/time, store ",
        tags$code("imputation_matrix()"), " if provided, update ", tags$code("total_imputed_count"), " from ",
        tags$code("result$total_imputed")
      ),
      tags$li(
        "If ", tags$code("result$data_def"), " is returned, it is made available for the parent to integrate (metadata handling is not forced in this module)."
      ),
      tags$li("Quality assessment via ", tags$code("assess_imputation_quality(result)"), " (returns a simple score and count)."),
      tags$li("Append a processing log entry via ", tags$code("add_processing_log(operation, status, details, duration)"), ".")
    ),

    tags$h3("Reset"),
    tags$p(
      tags$code("reset_imputation()"), " removes all columns matching prefix ", tags$code("^Imputed "), " from the current dataset, ",
      "resets module state (result, matrix, flags, caches, counters), and logs the action."
    ),

    tags$h3("Export / Import"),
    tags$ul(
      tags$li(
        tags$code("get_current_imputation_state_for_export()"),
        " returns a compact state including ", tags$code("method"), ", ", tags$code("columns"), ", ",
        tags$code("applied"), ", ", tags$code("last_processing_time"), ", ", tags$code("has_results"), "."
      ),
      tags$li(
        tags$code("get_imputation_ui_config_for_export()"),
        " returns UI configuration and recent method/columns for RDS pipelines."
      ),
      tags$li(
        tags$code("set_imputation_ui_config_from_import(config)"),
        " validates and applies ", tags$code("imputation_method_select"), " and ", tags$code("imputation_column_select"),
        " using ", tags$code("updateSelectInput"), ". Invalid values are ignored with debug logging."
      )
    ),

    tags$h3("Outputs & Status"),
    tags$ul(
      tags$li(
        tags$code("data_validation_status"),
        ": shows row count, number of columns selected for imputation, total missing values, missingness analysis, resource estimate, and collected validation warnings/errors."
      ),
      tags$li(
        tags$code("imputation_status_display"),
        ": after a successful run, shows method, Content types, values imputed (from ", tags$code("total_imputed_count"), "), ",
        "presence of imputed columns in the data, quality score, processing time, and current processing step; ",
        "otherwise indicates that no imputation has been applied."
      )
    ),

    tags$h3("Dependencies and Utilities"),
    tags$ul(
      tags$li(
        tags$code("utils.R"),
        ": provides ", tags$code("performGenericImputation()"), ", ", tags$code("performLeftCensoredImputation()"),
        ", ", tags$code("impute_random_forest()"), ", ", tags$code("impute_mice_cart()"), ", ",
        tags$code("safe_parallel_operation()"), " and parallel cleanup helpers."
      ),
      tags$li(
        "If present, ", tags$code("R/session_management.R"), " is sourced by ", tags$code("utils.R"),
        " and used indirectly for cluster cleanup."
      )
    ),

    tags$h3("Error Handling & Logging"),
    tags$ul(
      tags$li("All critical paths are wrapped in ", tags$code("tryCatch"), " with ", tags$code("debug_log()"), " gated by ", tags$code("debug_level"), "."),
      tags$li(
        "User-facing errors and warnings use ", tags$code("showNotification(...)"), " (e.g., invalid inputs, ongoing processing, reset outcome)."
      ),
      tags$li(
        "Processing steps and outcomes are recorded via ", tags$code("add_processing_log()"),
        " into a bounded in-memory history (last 50 entries)."
      )
    ),

    tags$h3("Performance Considerations"),
    tags$ul(
      tags$li(
        "Random forest and MICE paths restrict operations to numeric columns inside the method implementations (", tags$code("utils.R"), ")."
      ),
      tags$li(
        "Parallel resources are cleaned before running Random Forest when ", tags$code("clean_open_clusters()"), " exists."
      ),
      tags$li(
        "The memory estimator ", tags$code("estimate_memory_safe()"), " computes a coarse peak estimate and warning level for display."
      )
    ),

    tags$h3("Developer Good Practices"),
    tags$ul(
      tags$li(
        "When integrating into the orchestrator, prefer providing ", tags$code("get_data()"), " and ", tags$code("set_data()"),
        " so that data updates are explicit. The fallback attempt to write through a reactive ",
        tags$code("data()"), " is best avoided if the reactive is read-only."
      ),
      tags$li(
        "Ensure ", tags$code("data_def()"), " is up to date and contains correctly aligned ",
        tags$code("Content / Options / Column"), " mappings before imputation. ",
        "The mapping is used by ", tags$code("performGenericImputation()"), " to determine concrete columns."
      ),
      tags$li(
        "If metadata updates are required downstream, consume ", tags$code("result$data_def"),
        " from ", tags$code("performGenericImputation()"), " at the integration layer to persist the extended metadata."
      ),
      tags$li(
        "Avoid concurrent runs: the module guards via ", tags$code("processing_active()"),
        " but the parent should also prevent overlapping long-running operations."
      )
    )
  )
}

############
# Ratios & Statistics Module

render_tech_ratios_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Technical Documentation — Ratios and Statistics"),
    tags$p(
      "This module computes abundance ratios between user-defined sample groups and performs statistical testing. ",
      "It reads group membership from metadata (", tags$code("data_def()"), ") and adds result columns to the working dataset. ",
      "Configuration is captured as ratio definitions and can be applied in batch."
    ),

    tags$h3("Module Contract"),
    tags$pre(class = "r", paste0(
      "modRatiosUI <- function(id) { ratiosUISubmodule(NS(id, \"ui\")) }\n\n",
      "modRatiosServer <- function(\n",
      "  id,\n",
      "  data_def = NULL,                # reactive: metadata with columns Content, Sample and/or Options\n",
      "  get_data = NULL,                # function(): current data.frame\n",
      "  set_data = NULL,                # function(df): persist updated data.frame\n",
      "  available_samples = NULL,       # reactive: optional explicit sample names\n",
      "  UI_config = reactive(NULL),     # reactive: optional UI/config import\n",
      "  debug_level = 1\n",
      ") { ... }\n"
    )),
    tags$p("Selected return values:"),
    tags$ul(
      tags$li(tags$code("ratio_configurations()"), " — reactive ", tags$code("data.frame"), " of configured ratios."),
      tags$li(tags$code("apply_all_ratios()"), " — runs all configured ratio analyses; updates data via ", tags$code("set_data()"), "."),
      tags$li(tags$code("clear_all_configurations()"), " — removes all ratio definitions."),
      tags$li(tags$code("is_method_available(method_name)"), " — checks runtime availability (e.g., ", tags$code("limma"), ", ", tags$code("DEqMS"), ")."),
      tags$li(tags$code("get_current_ui_state() / set_current_ui_state(x)"), " — import/export of UI and configurations."),
      tags$li(tags$code("module_health_check()"), " — basic health summary.")
    ),

    tags$h3("UI Structure"),
    tags$ul(
      tags$li(
        tags$code("custom_col_sel_dw"), " (", tags$code("selectizeInput"), "): ",
        "select Content type to analyze; drives the set of measurable columns."
      ),
      tags$li(
        tags$code("numerator_sel_dw"), " / ", tags$code("denominator_sel_dw"), " (", tags$code("selectizeInput, multiple"), "): ",
        "select sample groups for numerator and denominator. Choices are derived from rows in ", tags$code("data_def()"),
        " where ", tags$code("Content == custom_col_sel_dw"), " (sources: ", tags$code("Sample"), " and/or ", tags$code("Options"), ")."
      ),
      tags$li(
        tags$code("valid_comparison_sel_dw"), " (", tags$code("numericInput"), "): ",
        "Minimum valid values (non-missing) threshold applied to numerator and denominator groups (see below)."
      ),
      tags$li(
        tags$code("valid_compgroup_sel_dw"), " (", tags$code("selectInput"), "): ",
        "Scope of the valid-value rule: ", tags$b("In total"), ", ", tags$b("One group"), ", ", tags$b("Each group"), "."
      ),
      tags$li(
        tags$code("statistics_sel_dw"), " (", tags$code("selectizeInput"), "): ",
        "statistical method: ",
        tags$code("Student's T-Test"), ", ", tags$code("Welch's T-Test"), ", ", tags$code("Moderated Welch Test"),
        ", ", tags$code("Limma"), ", ", tags$code("DEqMS"), ", ", tags$code("Mann-Whitney U Test"), "."
      ),
      tags$li(
        tags$code("adjust_sel_dw"), " (", tags$code("selectizeInput"), "): ",
        "p-value adjustment: ",
        tags$code("Bonferroni"), ", ", tags$code("FDR"), ", ", tags$code("Holm"), ", ", tags$code("Hochberg"),
        ", ", tags$code("Hommel"), ", ", tags$code("Benjamini & Hochberg"), ", ", tags$code("Benjamini & Yekutieli"), "."
      ),
      tags$li(
        tags$code("name_sel_dw"), " (", tags$code("textInput"), "): comparison name; used as prefix for new columns."
      ),
      tags$li(
        tags$code("add_ratio_dw"), " / ", tags$code("apply_ratios_dw"), " / ", tags$code("clear_ratio_dw"), " (", tags$code("actionButton"), "): ",
        "add configuration, run all analyses, or clear all configurations."
      ),
      tags$li(
        tags$code("custom_RatioTable_dw"), " (", tags$code("DT::DTOutput"), "): current configurations."
      ),
      tags$li(
        tags$code("ratio_status"), " (", tags$code("verbatimTextOutput"), "): processing status and last-run diagnostics."
      )
    ),

    tags$h3("Data & Metadata Contracts"),
    tags$ul(
      tags$li(
        tags$b("Input data:"), " provided by ", tags$code("get_data()"), ". The module expects a stable ", tags$code("Row Index"),
        " column (created/validated internally when needed)."
      ),
      tags$li(
        tags$b("Metadata:"), " ", tags$code("data_def()"), " must contain ",
        tags$code("Content"), " and at least one of ", tags$code("Sample"), " or ", tags$code("Options"),
        " for mapping groups to concrete columns."
      )
    ),

    tags$h3("Minimum Valid Values (Per-Group Filtering)"),
    tags$p(
      "Minimum valid values are enforced ", tags$b("within the selected numerator and denominator groups"),
      " and not across all columns of a Content type. The settings are:"
    ),
    tags$ul(
      tags$li(
        tags$code("valid_comparison_sel_dw"), ": minimum count of non-missing values required."
      ),
      tags$li(
        tags$code("valid_compgroup_sel_dw"), ": scope — ",
        tags$b("In total"), " (across all selected samples), ",
        tags$b("One group"), " (any selected group reaches the threshold), ",
        tags$b("Each group"), " (every selected group reaches the threshold)."
      )
    ),
    tags$p(
      "At runtime, these values are passed to ",
      tags$code("filter_data_for_analysis_fixed(...)"),
      " which computes per-row counts separately for numerator and denominator columns and applies the rule. ",
      "For imputed Content types (names starting with ", tags$code("\"Imputed\""), "), validation can reference original columns via ",
      tags$code("get_validation_data_for_imputed_content(...)"),
      " before the counts are computed."
    ),

    tags$h3("Statistical Methods"),
    tags$ul(
      tags$li(
        tags$b("Student's T-Test"), ": ",
        tags$code("perform_t_test_analysis(...)"),
        " — equal variances assumed; requires at least 3 samples per group; supports p-value adjustment."
      ),
      tags$li(
        tags$b("Welch's T-Test"), ": ",
        tags$code("perform_welch_t_test_analysis(...)"),
        " — unequal variances; requires at least 3 samples per group; supports p-value adjustment."
      ),
      tags$li(
        tags$b("Moderated Welch Test"), ": ",
        tags$code("perform_moderated_welch_test_analysis(...)"),
        " — computes Welch-like statistics with moderated variance components; minimum group size set to 2 in the implementation; supports p-value adjustment."
      ),
      tags$li(
        tags$b("Limma"), ": ",
        tags$code("perform_limma_analysis(...)"),
        " — requires ", tags$code("limma"), "; minimum 2 samples per group; design/fit done on the selected columns; supports p-value adjustment."
      ),
      tags$li(
        tags$b("DEqMS"), ": ",
        tags$code("perform_deqms_analysis(...)"),
        " — requires ", tags$code("limma"), " and ", tags$code("DEqMS"),
        "; searches PSM-related metadata via ", tags$code("find_psm_columns(...)"),
        " and aborts if none found."
      ),
      tags$li(
        tags$b("Mann-Whitney U Test"), ": ",
        tags$code("perform_mann_whitney_analysis(...)"),
        " — non-parametric; requires at least 3 samples per group; supports p-value adjustment."
      )
    ),
    tags$p(
      "All methods follow a shared workflow: ensure ", tags$code("Row Index"), ", retransform data if indicated by metadata via ",
      tags$code("check_and_retransform_data(...)"), ", filter rows using ", tags$code("filter_data_for_analysis_fixed(...)"),
      ", compute per-row ratios (", tags$code("compute_abundance_ratio_for_row(...)"), "), calculate p-values, adjust p-values with ",
      tags$code("adjust_p_values_safely(...)"), " and assemble results via ", tags$code("create_robust_ratio_result(...)"), "."
    ),

    tags$h3("Execution Flow"),
    tags$ol(
      tags$li("User defines one or more ratio configurations via the UI and adds them to the configuration table."),
      tags$li(
        "On ", tags$code("apply_all_ratios()"), ": the server reads ", tags$code("get_data()"), " and ", tags$code("data_def()"),
        ", then iterates configurations. For each configuration the module maps group labels to column indices using ",
        tags$code("Content"), " × (", tags$code("Sample"), " or ", tags$code("Options"), ")."
      ),
      tags$li(
        "The chosen statistical function is executed with ", tags$code("valid_count"), " and ", tags$code("valid_logic"),
        " passed through to ", tags$code("filter_data_for_analysis_fixed(...)"), "."
      ),
      tags$li(
        "Results are merged into the dataset (new columns with the comparison name prefix); ",
        tags$code("set_data(updated_df)"), " is called on success."
      )
    ),

    tags$h3("Result Columns"),
    tags$p(
      "Per configuration, the module creates columns prefixed by ", tags$code("name_sel_dw"),
      " (e.g., ratio, p-value, adjusted p-value). Exact column names are assembled in ",
      tags$code("create_robust_ratio_result(...)"), " and preserved on merge."
    ),

    tags$h3("Export / Import of Configurations"),
    tags$ul(
      tags$li(
        tags$code("get_current_ui_state()"), " returns both UI selections (", tags$code("ratio_settings"), ") ",
        "and the current ", tags$code("ratio_configurations()"), "."
      ),
      tags$li(
        tags$code("set_current_ui_state(x)"), " validates and applies a previously saved state; ",
        "invalid values are ignored with debug logging."
      )
    ),

    tags$h3("Error Handling & Logging"),
    tags$ul(
      tags$li("All critical code paths are wrapped in ", tags$code("tryCatch"), "; messages are routed through a module-level debug logger (", tags$code("debug_level"), ")."),
      tags$li(
        "User notifications are raised on missing inputs, unavailable packages, invalid configurations, or empty results."
      ),
      tags$li(
        "Processing steps are appended to an in-memory processing log and surfaced in the UI status output."
      )
    ),

    tags$h3("Dependencies & Availability"),
    tags$ul(
      tags$li(tags$code("limma"), " (required for ", tags$code("Limma"), " and ", tags$code("DEqMS"), ")."),
      tags$li(tags$code("DEqMS"), " (required for ", tags$code("DEqMS"), ")."),
      tags$li(tags$code("pracma"), " (required for ", tags$code("Moderated Welch Test"), ")."),
      tags$li(
        "Availability checks are exposed via ", tags$code("is_method_available(method_name)"),
        " and used to gate execution."
      )
    ),

    tags$h3("Developer Good Practices"),
    tags$ul(
      tags$li(
        "Provide both ", tags$code("get_data()"), " and ", tags$code("set_data()"),
        " so that successful analyses can persist results reliably."
      ),
      tags$li(
        "Keep ", tags$code("data_def()"), " synchronized with the dataset. Group-to-column mapping depends on ",
        tags$code("Content"), " and ", tags$code("Sample/Options"), " consistency."
      ),
      tags$li(
        "Use the minimum valid values controls to avoid using rows where numerator or denominator groups are supported only by imputed values. ",
        "For imputed Content types, the validator may reference original columns before counting."
      ),
      tags$li(
        "Check method availability with ", tags$code("is_method_available()"),
        " when pre-selecting defaults or re-applying saved configurations."
      )
    )
  )
}

############
# Basemean Module

render_tech_basemean_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Technical Documentation — Basemean Calculation"),
    tags$p(
      "This module computes a mean-abundance column (\"Basemean\") across selected sample columns for a chosen abundance type. ",
      "It updates the working dataset and extends metadata with a corresponding row. ",
      "UI state can be imported/exported via a small configuration object."
    ),

    tags$h3("Module Contract"),
    tags$pre(class = "r", paste0(
      "modBasemeanUI <- function(id) { datawizard_basemean_UI(NS(id)) }\n\n",
      "modBasemeanServer <- function(\n",
      "  id,\n",
      "  data_def,              # reactive: metadata; used to resolve columns by Content/Sample\n",
      "  get_data,              # function(): returns current data.frame\n",
      "  set_data,              # function(df): persists updated data.frame\n",
      "  available_samples,     # reactive (not required in current code path)\n",
      "  UI_config,             # reactive or reactiveVal or list; supports import/export of UI state\n",
      "  debug_level = 1\n",
      ") { ... }\n"
    )),
    tags$p("Selected return values:"),
    tags$ul(
      tags$li(tags$code("get_ui_config()"), " — returns the current UI configuration source (reactive/list)."),
      tags$li(tags$code("apply_ui_config_basemean(cfg)"), " — applies ", tags$code("list(abundance_type, samples, suffix)"), " to the UI.")
    ),

    tags$h3("UI Structure"),
    tags$ul(
      tags$li(tags$code("abundance_type_basemean"), " (", tags$code("selectInput"), "): selects the abundance type."),
      tags$li(tags$code("sample_selection_basemean"), " (", tags$code("selectizeInput, multiple"), "): selects samples (columns) belonging to the chosen type."),
      tags$li(tags$code("suffix_basemean"), " (", tags$code("textInput"), "): optional suffix for the Basemean column name."),
      tags$li(tags$code("add_basemean"), " (", tags$code("actionButton"), "): computes and writes the Basemean column."),
      tags$li(tags$code("clear_basemean"), " (", tags$code("actionButton"), "): removes all Basemean columns and associated metadata rows."),
      tags$li(tags$code("basemean_status"), " (", tags$code("verbatimTextOutput"), "): status placeholder (no server render in the provided code).")
    ),

    tags$h3("Data & Metadata Contracts"),
    tags$ul(
      tags$li(
        tags$b("Input data:"), " provided via ", tags$code("get_data()"), ". ",
        "The Basemean is computed as a row-wise mean over the resolved sample columns."
      ),
      tags$li(
        tags$b("Metadata:"), " ", tags$code("data_def()"), " must contain at least ",
        tags$code("Content"), " and ", tags$code("Sample"), " to resolve columns. ",
        "Column selection uses: ", tags$code('idx <- which(data_def$Content == abundance_type & data_def$Sample %in% samples)') , "."
      ),
      tags$li(
        tags$b("Result metadata row:"), " a new row is appended with ",
        tags$code('Content = "Basemean"'), ", ",
        tags$code("Column = <new column name>"), ", ",
        tags$code('Sample = paste(selected_samples, collapse = ", ")'),
        " and ", tags$code('Type = "calculated"'), " (if those fields exist in ", tags$code("data_def"), ")."
      )
    ),

    tags$h3("Valid Abundance Types"),
    tags$p("The server populates the abundance-type selector from the following vector:"),
    tags$pre(class = "r", paste0(
      'c(\n',
      '  "Raw Abundance",\n',
      '  "Normalized Abundance",\n',
      '  "Imputed Raw Abundance",\n',
      '  "Imputed Normalized Abundance",\n',
      '  "Batch Corrected Abundance",\n',
      '  "Imputed Batch Corrected Abundance"\n',
      ')\n'
    )),

    tags$h3("Computation"),
    tags$p(
      "On ", tags$code("input$add_basemean"), ", the server validates inputs and resolves matching columns with ",
      tags$code("data_def()"), ". The new column name is ",
      tags$code('"Basemean"'), " or ", tags$code('paste0("Basemean_", suffix)'), ". ",
      "Values are computed as ", tags$code("rowMeans(data[, idx, drop = FALSE], na.rm = TRUE)"), ". ",
      "If the column already exists, it is overwritten."
    ),
    tags$p(
      "After computing values, a metadata row is appended (see above), and ", tags$code("set_data(updated_df)"), " is called. ",
      "Notifications are displayed on success or failure."
    ),

    tags$h3("Clear Operation"),
    tags$p(
      "On ", tags$code("input$clear_basemean"), ", all columns with names matching ", tags$code('^Basemean'), " are removed from the dataset. ",
      "Associated metadata rows are removed via ", tags$code('data_def[data_def$Content != "Basemean", ]'), ". ",
      tags$code("set_data(updated_df)"), " is called with the reduced data."
    ),

    tags$h3("UI State Import / Export"),
    tags$ul(
      tags$li(
        tags$code("apply_ui_config_basemean(cfg)"), " updates inputs using ", tags$code("updateSelectInput"), ", ",
        tags$code("updateSelectizeInput"), " and ", tags$code("updateTextInput"),
        " with fields ", tags$code("abundance_type"), ", ", tags$code("samples"), ", ", tags$code("suffix"), ". ",
        "A guard reactive (", tags$code("ui_config_update_active()"), ") prevents update loops."
      ),
      tags$li(
        "When the abundance type changes, the module recalculates the valid sample choices and applies any deferred sample selection from ",
        tags$code("session$userData$apply_basemean_after_sample_update"), "."
      ),
      tags$li(
        "A background observer assembles the current UI state into ",
        tags$code("list(abundance_type, samples, suffix)"),
        " and writes it back to ", tags$code("UI_config"), " when ", tags$code("UI_config") , " is a ", tags$code("reactiveVal")
      )
    ),

    tags$h3("Utilities (basemean_utils.R)"),
    tags$ul(
      tags$li(
        tags$code("compute_basemean(data_mod, data_def, abundance_type, samples, suffix)"),
        " → returns ", tags$code("list(data_mod, new_col)"),
        " and creates or overwrites the Basemean column."
      ),
      tags$li(
        tags$code("update_basemean_metadata(data_def, data_mod, new_col, selected_type, selected_samples)"),
        " → appends a metadata row for the Basemean column; tolerates size mismatches between data and metadata."
      ),
      tags$li(
        tags$code("clear_basemean_columns(data_mod, data_def)"),
        " → removes all Basemean columns and prunes metadata (", tags$code('Content != "Basemean"'), ")."
      ),
      tags$li(
        "The functions above are registered on ", tags$code("metadata_functions"),
        " as ", tags$code("$compute_basemean"), ", ", tags$code("$update_basemean_metadata"), ", ",
        tags$code("$clear_basemean_columns"), "."
      )
    ),

    tags$h3("Observations from the Provided Code"),
    tags$ul(
      tags$li(
        "The server uses ", tags$code("input$suffix"), " when computing the new column name during the add operation, ",
        "while the UI defines ", tags$code("suffix_basemean"), ". ",
        "Elsewhere, the module reads and writes ", tags$code("suffix_basemean"),
        " (e.g., in the UI-config observer)."
      ),
      tags$li(
        tags$code("basemean_status"), " is defined in the UI but no output is rendered in the server."
      ),
      tags$li(
        tags$code("available_samples"), " is present in the server signature; the current code derives valid samples from ",
        tags$code("data_def()$Sample"), " filtered by the selected ", tags$code("Content"), "."
      )
    ),

    tags$h3("Error Handling & Logging"),
    tags$ul(
      tags$li(
        "All critical paths use ", tags$code("tryCatch"), " with a module-local ", tags$code("debug_log(message, level)"),
        " gated by ", tags$code("debug_level"), "."
      ),
      tags$li(
        "User-facing feedback is provided via ", tags$code("showNotification(...)"),
        " on validation failures (e.g., no columns matched), update failures (", tags$code("set_data()"), "), and clear operations."
      )
    ),

    tags$h3("Developer Good Practices"),
    tags$ul(
      tags$li(
        "Keep ", tags$code("data_def()"), " synchronized with the dataset. ",
        "Column resolution relies on ", tags$code("Content"), " and ", tags$code("Sample"), " consistency."
      ),
      tags$li(
        "Prefer passing explicit ", tags$code("get_data()"), " and ", tags$code("set_data()"),
        " from the integration layer so updates are applied atomically."
      ),
      tags$li(
        "When importing UI state, call ", tags$code("apply_ui_config_basemean(cfg)"),
        " once the metadata-driven sample choices are available; the module supports a deferred sample re-apply hook."
      ),
      tags$li(
        "If you use the utilities directly (", tags$code("compute_basemean"), "/", tags$code("update_basemean_metadata"),
        "/", tags$code("clear_basemean_columns"), "), re-run downstream modules that depend on metadata or column schema."
      )
    )
  )
}
