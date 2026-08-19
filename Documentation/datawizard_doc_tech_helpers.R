# ./Documentation/datawizard_doc_tech_helpers.R
# Datawizard Documentation — Transformation Modules Technical Documentation
#
# Contains content rendering functions for data transformation submodules:
#   - Batch Effect Correction
#   - Pivot (wide/long reshaping)
#   - Merge (Primary + Additional data joining)
#
# These functions are called by the documentation server in datawizard_doc_ui.R.

############
# Batch Effect Correction

render_tech_batch_effects_content <- function() {
  div(
    h2("Technical Documentation — Batch Effects Module"),
    hr(),

    h3("Purpose & Scope"),
    p("The Batch Effects module performs column-wise batch correction on quantitative data. ",
      "It supports multiple correction methods, optional pre-/post-transformations, and optional imputation. ",
      "Corrected values are added as new columns with a fixed prefix so original data remain intact."),

    h3("File & Entrypoints"),
    tags$ul(
      tags$li(code("modules/Data Wizard/datawizard_batch_effects.R")),
      tags$li(strong("UI:"), code("modBatchEffectsUI(id)")),
      tags$li(strong("Server:"), code("modBatchEffectsServer(id, get_data, set_data, init_meta, header_primary, UI_config, debug_level)"))
    ),

    h3("Integration Contract (Server Arguments & Returns)"),
    p("The server follows the project-wide module contract:"),
    tags$ul(
      tags$li(code("get_data: function()")),
      tags$li(code("set_data: function(df) -> logical")),
      tags$li(code("init_meta: function(...)") , " (accepted but not used for core correction)"),
      tags$li(code("header_primary: reactive()") , " (accepted; not used in correction logic)"),
      tags$li(code("UI_config: reactive()") , " (used to apply imported UI configuration)"),
      tags$li(code("debug_level: numeric") , " (0=none, 1=essential, 2=verbose)")
    ),
    p("The server returns a list including:"),
    tags$ul(
      tags$li(code("processing_complete: reactive({ TRUE })")),
      tags$li(code("get_data, set_data") , " (wrapped accessors used by Integration)"),
      tags$li(code("apply_batch_correction()")),
      tags$li(code("is_batch_correction_configured() -> logical")),
      tags$li(code("get_batch_summary() -> character")),
      tags$li(code("apply_trigger: reactive({ input$unBatchButton })")),
      tags$li(code("get_current_ui_state()") , " (for export/RDS)"),
      tags$li(code("batch_method, imputation_method_batch, transformation_batch, remove_imputed_batch: reactive(...)")),
      tags$li(code("batch_counter: reactiveVal")),
      tags$li(code("batch_inputs: reactiveVal(list(...))")),
      tags$li(code("get_ui_config_errors(), clear_ui_config_errors()")),
      tags$li(code("get_performance_metrics()")),
      tags$li(code("module_health_check()")),
      tags$li(code("test_ui_config_loading()"))
    ),

    h3("UI Summary"),
    tags$ul(
      tags$li(strong("Readiness flag:"), code("output$batch_ready == 'true'") , " gates the main controls."),
      tags$li(strong("Method:"), code("input$batch_method"), " with choices ",
              code("c('Offset Correction','ComBat','Limma','LOESS','Quantile')"), ", default ", code("'ComBat'")),
      tags$li(strong("Imputation:"), code("input$imputation_method_batch"), " with choices ",
              code("c('None','Left censored','Random Forest','MICE CART')"), ", default ", code("'None'")),
      tags$li(strong("Transformation:"), code("input$transformation_batch"), " with choices ",
              code("c('None','log2','log10','-log10')"), ", default ", code("'None'")),
      tags$li(strong("Remove imputed after correction:"), code("input$remove_imputed_batch: logical")),
      tags$li(strong("Batch groups:"), " dynamic selectors ", code("selected_batches_<i>"),
              " (default 2 groups; add/remove buttons; max 10 groups)"),
      tags$li(strong("Action:"), code("input$unBatchButton"), " triggers correction")
    ),

    h3("Data Access & Validation"),
    tags$ul(
      tags$li(strong("Data getter:"), " ", code("get_file_data()"),
              " calls the injected ", code("get_data()"), " and logs dimensions (level 2)."),
      tags$li(strong("Numeric selection:"), " only numeric (or coercible) columns are offered; ",
              code("'Row Index'"), " is excluded as an ID column."),
      tags$li(strong("Structure checks:"), " ", code("validate_data_structure(df)"),
              " (rows>0, cols≥2), ", code("validate_numeric_columns(df, selected_cols)"),
              ", ", code("validate_batch_groups(list_of_groups, method)"),
              " (≥2 non-empty groups, disjoint column sets).")
    ),

    h3("Algorithm (Button Handler)"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'on unBatchButton:
  1) Validate data (dims, numeric columns), gather selections
  2) Prepare parameters:
       • prefix <- "Batch Corrected "
       • remove old "^Batch Corrected " columns from raw
       • sub_df <- selected numeric columns
       • original_data_with_nas <- sub_df (for NA restoration later)
  3) Determine imputation path:
       • None: use complete cases only
       • Left censored / Random Forest / MICE CART: impute → process_all_data <- TRUE
  4) Optional pre-transform (per UI): retransform_data_global() on complete_data
  5) Standardize domain for correction: log2(complete_data) with offset if non-positive
  6) Apply method:
       • Offset Correction: median-centering per batch (column offsets)
       • ComBat: sva::ComBat on non-constant features; then restored to full matrix
       • Limma: limma::removeBatchEffect(batch = labels)
       • LOESS: trend removal per batch/column (loess against batch median)
       • Quantile: limma::normalizeQuantiles per batch
  7) Back-transformation step according to UI selection (if any)
  8) Optional NA restoration:
       • If imputation used and remove_imputed_batch = TRUE → restore original NA pattern
  9) Merge results:
       • Creates new columns "Batch Corrected <col>"
       • Writes corrected values (complete cases only if no imputation)
 10) set_data(final_data) → Integration handles metadata refresh and any filter reset
'
      )
    ),

    h3("Column Strategy & Side Effects"),
    tags$ul(
      tags$li(strong("Prefix:"), " New columns are added as ", code("'Batch Corrected <original>'"),
              ". The module removes older columns with that prefix before writing new ones."),
      tags$li(strong("Original columns:"), " remain untouched."),
      tags$li(strong("Filters/metadata:"), " the module itself only writes data via ", code("set_data()"),
              "; Integration/Core handle metadata refresh and possible filter resets after structure changes.")
    ),

    h3("UI Config Import (RDS)"),
    p("The module consumes imported UI configuration through ", code("UI_config()"),
      " and applies it safely with timing guards:"),
    tags$ul(
      tags$li("Observed via ", code("observeEvent(UI_config())")),
      tags$li("Delay-critical updates run in ", code("session$onFlushed(...)"), " to avoid timing races"),
      tags$li("Inputs updated with ", code("freezeReactiveValue()"), " + ", code("updateSelectInput()/updateCheckboxInput()")),
      tags$li("Accepted keys: ", code("batch_method"), ", ", code("imputation_method_batch"), ", ",
              code("transformation_batch"), ", ", code("remove_imputed_batch"), ", ",
              code("batch_counter"), ", ", code("batch_inputs")),
      tags$li("Invalid values are rejected and recorded in ", code("ui_config_errors()"), ", source flagged as ", code("'import'")),
      tags$li("Any manual user change flips source to ", code("'user_modified'"))
    ),

    h3("Outputs for Integration & Export"),
    tags$ul(
      tags$li(strong("apply_trigger:"), " reactive on ", code("input$unBatchButton"), " (Integration can observe this)"),
      tags$li(strong("get_current_ui_state():"),
              " returns a list containing ", code("batch_method, imputation_method_batch, transformation_batch, remove_imputed_batch, batch_counter, batch_inputs"),
              " (used in RDS/export)"),
      tags$li(strong("get_performance_metrics():"), " exposes ", code("last_correction_time"),
              " and ", code("correction_history"), " plus the active ", code("DEBUG_LEVEL")),
      tags$li(strong("module_health_check():"), " returns a list with health details (status/messages/timestamps)")
    ),

    h3("Dependencies"),
    tags$ul(
      tags$li(strong("CRAN:"), code("sva") , " (ComBat), ", code("limma") , " (removeBatchEffect, normalizeQuantiles)"),
      tags$li(strong("Base R:"), code("stats::loess")),
      tags$li(strong("Provided by modEnv:"),
              code("performLeftCensoredImputation"), ", ",
              code("performGroupedLeftCensoredImputation"), ", ",
              code("impute_random_forest"), ", ",
              code("performRandomForestImputation"), ", ",
              code("impute_mice_cart"), ", ",
              code("performMICECartImputation"), ", ",
              code("retransform_data_global")),
      tags$li(strong("Cleanup integration:"),
              "the module registers a cleanup callback via ", code("cleanup_manager$register_module('Batch effects', fn)"),
              " if ", code("cleanup_manager"), " is available in scope.")
    ),

    h3("Logging"),
    p("A module-scoped ", code("debug_log(message, level)"), " prints to console with a prefix ",
      code("[ BATCH <time> ]"), ". The ", code("debug_level"), " is injected by the caller."),
    tags$ul(
      tags$li(strong("Level 1 (essential):"), " start/end, validation failures, package availability, exceptions"),
      tags$li(strong("Level 2 (verbose):"), " dimensions, chosen methods, offsets added, group counts, post-merge stats")
    ),

    h3("Gotchas & Best Practices"),
    tags$ul(
      tags$li("Ensure at least two non-empty, disjoint batch groups."),
      tags$li("Only numeric (or coercible) columns are valid; ID column ", code("'Row Index'"), " is excluded."),
      tags$li("If input contains zeros/negatives, an offset is added before ", code("log2"), " to avoid ", code("±Inf"), "."),
      tags$li("ComBat/limma methods require their packages; the module will notify and bail out if missing."),
      tags$li("Set ", code("remove_imputed_batch = TRUE"), " to restore original NA pattern after using imputation."),
      tags$li("Corrected columns are always added with the ", code("'Batch Corrected '"), " prefix; previous prefixed columns are removed before writing."),
      tags$li("Metadata/table refresh after new columns is handled by Integration/Core, not here.")
    )
  )
}

############
# Pivot Module

render_tech_pivot_content <- function() {
  div(
    h2("Technical Documentation — Pivot Module"),
    hr(),

    h3("Purpose & Scope"),
    p("The Pivot module reshapes a data.frame between wide and long layouts using ",
      code("tidyr::pivot_wider"), " and ", code("tidyr::pivot_longer"), ". ",
      "It writes the reshaped result back to the selected dataset (Primary or Secondary) via injected setters. ",
      "The module does not edit metadata itself; Integration/Core refresh metadata and (if needed) reset filters after structural changes."),

    h3("Files & Entrypoints"),
    tags$ul(
      tags$li(code("modules/Data Wizard/datawizard_pivot.R")),
      tags$li(strong("UI:"), " ", code("modPivotUI(id)")),
      tags$li(strong("Server:"), " ", code("modPivotServer("),
              code("id, get_data, set_data, get_data2, set_data2, init_meta, UI_config, debug_level, safe_ui_system"), ")")
    ),

    h3("Integration Contract (Server Arguments)"),
    p("The server follows the project-wide module contract, with a few Pivot-specific additions:"),
    tags$ul(
      tags$li(code("get_data(): data.frame | NULL"), " — returns current Primary data."),
      tags$li(code("set_data(df): logical"), " — writes updated Primary data."),
      tags$li(code("get_data2(): data.frame | NULL"), " — returns current Secondary (additional) data."),
      tags$li(code("set_data2(df): logical"), " — writes updated Secondary data."),
      tags$li(code("init_meta(...)"), " — accepted, not used directly for pivot."),
      tags$li(code("UI_config(): list | NULL"), " — imported UI configuration (from RDS)."),
      tags$li(code("debug_level: numeric"), " — 0=none, 1=essential, 2=verbose."),
      tags$li(code("safe_ui_system: list | NULL"), " — optional safe UI wrapper with ",
              code("$update_input_safely() / $show_notification_safely()"), "; a local fallback is created if missing.")
    ),

    h3("Return Value (Server → Integration)"),
    p("The server returns a list of accessors/hooks:"),
    tags$ul(
      tags$li(code("get_primary_data(), set_primary_data(), get_secondary_data(), set_secondary_data()")),
      tags$li(code("get_current_data()"), " — reactive that picks Primary/Secondary based on UI selection."),
      tags$li(code("set_current_data(df)"), " — writes to Primary or Secondary based on UI selection."),
      tags$li(code("get_current_ui_state()"), " — serializable state for RDS: ",
              code("list(pivot_data_dw, pivot_type_dw, pivot_options)")),
      tags$li(code("apply_ui_config(config)"), " — applies imported UI_config safely."),
      tags$li(code("ui_config_applied(), ui_config_source()"), " — reactives for import bookkeeping."),
      tags$li(code("get_pivot_errors(), clear_pivot_errors()"), " — error store helpers."),
      tags$li(code("apply_trigger()"), " — reactive on ", code("input$apply_pivot_dw")),
      tags$li(code("get_performance_metrics()"), " — returns ", code("last_operation_time"), ", ",
              code("operation_history"), ", ", code("debug_level")),
      tags$li(code("module_health_check()"), " — sanity snapshot for diagnostics")
    ),

    h3("UI Controls (as implemented)"),
    tags$ul(
      tags$li(code("input$pivot_data_dw"), " — Select dataset: ",
              code("c('primary','secondary')"), " (labels: “Primary Data” / “Secondary Data”)."),
      tags$li(code("input$pivot_type_dw"), " — Operation: ",
              code("c('wider','longer','transpose')"), "."),
      tags$li(strong("If "), code("wider"), strong(":")),
      tags$ul(
        tags$li(code("input$wider_names_from"), " — source column for new column names."),
        tags$li(code("input$wider_values_from"), " — source column for values."),
        tags$li(code("input$wider_id_cols[]"), " — ID columns kept as identifiers (multi-select; auto-generated if none).")
      )
    ),
    tags$li(strong("If "), code("longer"), strong(":")),
    tags$ul(
      tags$li(code("input$longer_cols[]"), " — columns to stack."),
      tags$li(code("input$longer_names_to"), " — new column for former column names (default “name”)."),
      tags$li(code("input$longer_values_to"), " — new column for values (default “value”).")
    ),
    tags$li(code("output$pivot_ready"), " — string “true”/“false” used in a ",
            code("conditionalPanel"), " to gate the UI (data exists & has rows/cols)."),
    tags$li(code("output$pivot_options"), " — dynamically renders controls for the chosen mode."),
    tags$li(code("output$pivot_preview_dim"), " + ", code("output$pivot_preview_table"),
            " — inline preview (dimensions + DT head of a sampled pivot)."),
    tags$li(code("input$apply_pivot_dw"), " — Apply button."),

    h3("UI_config (Import/Export)"),
    p("The module consumes and emits the following keys under ", code("UI_config$pivot"), ":"),
    tags$ul(
      tags$li(code("pivot_data_dw: 'primary' | 'secondary'")),
      tags$li(code("pivot_type_dw: 'wider' | 'longer' | 'transpose'")),
      tags$li(code("pivot_options: list(...)"), " — mode-specific options:",
              tags$ul(
                tags$li(strong("wider:"), " ", code("wider_names_from, wider_values_from, wider_id_cols[]")),
                tags$li(strong("longer:"), " ", code("longer_cols[], longer_names_to, longer_values_to")),
                tags$li(strong("transpose:"), " (empty list — no additional parameters)")
              )
      )
    ),
    p("On import, ", code("apply_ui_config()"), " delegates to ",
      code("set_pivot_ui_config_from_import(config)"), " which:",
      tags$ul(
        tags$li("Updates ", code("pivot_data_dw"), " and ", code("pivot_type_dw"), " via ", code("updateSelectInput")),
        tags$li("Stores ", code("pivot_options"), " in an internal state (", code("pivot_options_state()"),
                ") so the next dynamic UI render preselects them"),
        tags$li("Marks ", code("ui_config_applied(TRUE)"), " and ", code("ui_config_source('import')"))
      )
    ),
    p("For export, ", code("get_current_ui_state()"), " collects the active type and options and returns the structure above."),

    h3("Preview & Performance Estimation"),
    p("The inline preview computes expected output size and shows a small sample of the pivot result:"),
    tags$ul(
      tags$li(code("pivot_preview_info()"), " — computes ",
              code("c(nrows_res, ncols_res)"), " and a sample head. ",
              "For Wider/Longer uses ", code("tidyr::pivot_wider/longer"), ". ",
              "For Transpose: operates on ", code("min(5, nrow(data))"), " source rows to keep the preview compact; ",
              "if ", code("Row Index"), " is present, expected dimensions follow the current header-preserving layout ",
              code("(rows = max(ncol(data)-2, 0), cols = nrow(data)+2)"), ". ",
              "Errors are caught and shown as text."),
      tags$li(strong("Large-job warnings:"), " the Apply handler estimates workload and asks for confirmation if: ",
              code("wider: estimated_cols > 1000"), ", ",
              code("longer: estimated_rows > 500000"), ". ",
              "Transpose has no size guard because cell count is unchanged. ",
              "Confirmed parameters are cached in ",
              code("pivot_operation_params()"), " and executed after ", code("input$confirm_large_pivot"), ".")
    ),

    h3("Pivot Execution (Apply Handler)"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        '1) Read current table via get_current_data() — respects pivot_data_dw (primary/secondary)
2) Validate inputs by mode (required columns exist, id_cols derivation if needed)
3) If large: show modal, store params, wait for confirm_large_pivot
   (large guard applies to wider/longer only; transpose has no size guard)
4) Call:
     Wider    : tidyr::pivot_wider(data, id_cols = all_of(id_cols),
                                   names_from = wider_names_from,
                                   values_from = wider_values_from)
     Longer   : tidyr::pivot_longer(data, cols = all_of(longer_cols),
                                    names_to   = longer_names_to,
                                    values_to  = longer_values_to)
     Transpose: remove "Row Index" helper column (if present); base::t() on remainder.
                In Row-Index mode, build headers from the first transposed row:
                  c(rownames(transposed)[1], transposed[1, ]).
                Drop that first transposed row from the body, prepend a fresh
                sequential "Row Index", and keep former transposed rownames as
                the first semantic column (e.g. Protein IDs).
                Mixed-type coercion flagged via attr(result, "mixed_type_coercion").
5) Wider only: add a prefix to new columns
6) set_current_data(result) → writes to Primary or Secondary (based on selection)
   (Integration/Core: metadata refresh via update_metadata_for_pivoted_data() & filter reset)
7) Record timings in last_operation_time() and append to operation_history()'
      )
    ),

    h3("Data Routing & Side Effects"),
    tags$ul(
      tags$li("The module writes the result to the dataset currently selected by ",
              code("input$pivot_data_dw"), " via ", code("set_current_data()"), "."),
      tags$li("On ", code("wider"), ", new columns get a stable prefix ",
              code("'Pivoted_<names_from>_'"), " to distinguish them."),
      tags$li("Original columns are not dropped unless the chosen pivot implies it (e.g., long → stacked)."),
      tags$li("Metadata refresh and filter resets are outside this module and are handled by Integration/Core."),
      tags$li("For convenience (and only if ", code("rv"), " exists), the module also sets ",
              code("rv$data_mod <- result"), " after successful apply.")
    ),

    h3("Error Handling & Logging"),
    tags$ul(
      tags$li("All critical paths are wrapped in ", code("tryCatch()"), " with user notifications via ",
              code("showNotification(...)")),
      tags$li("Module-scoped ", code("debug_log(msg, level)"), " prints timestamped messages if ",
              code("debug_level >= level"), "."),
      tags$li("Structured store ", code("pivot_errors()"), " collects errors (", code("get_pivot_errors()/clear_pivot_errors()"), ")."),
      tags$li("Health snapshot via ", code("module_health_check()"), " exposes status, error count, last operation time, and import flags.")
    ),

    h3("Safe UI System"),
    p("If a ", code("safe_ui_system"), " is provided, the module uses its helpers: ",
      code("$update_input_safely(id, value, type)"), " and ",
      code("$show_notification_safely(msg, type, duration)"), ". ",
      "Otherwise, a local fallback implements safe updates with ", code("updateSelectInput/updateTextInput"), " and guards."),

    h3("Cleanup"),
    p("If a ", code("cleanup_manager"), " exists, the module registers a cleanup callback: ",
      code('cleanup_manager$register_module("Pivot", function() { ... })'),
      " that clears reactive flags and transient state upon session end."),

    h3("UI_config Example (RDS)"),
    pre(
      style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
      'UI_config$pivot <- list(
  pivot_data_dw = "primary",
  pivot_type_dw = "wider",
  pivot_options = list(
    wider_names_from  = "Protein FDR Confidence: Combined",
    wider_values_from = "Protein FDR Confidence: Combined",
    wider_id_cols     = c("Accession", "Gene")
  ),
  export_timestamp = Sys.time(),
  export_version   = "enhanced_v1.0"
)'
    ),

    h3("Notes & Best Practices"),
    tags$ul(
      tags$li("Validate that required columns exist before applying."),
      tags$li("Keep the pivot UI state minimal: just ", code("pivot_type_dw"), " and the mode-specific ",
              code("pivot_options"), "."),
      tags$li("Expect Integration/Core to update metadata and react to column layout changes."),
      tags$li("Beware of very large long pivots (rows explode): use filters or fewer columns first."),
      tags$li("When extending options, also extend the import/export helpers so RDS round-trips remain stable.")
    )
  )
}

############
# Merge Module

render_tech_merge_content <- function() {
  div(
    h2("Technical Documentation — Merge Module"),
    hr(),

    h3("Purpose & Scope"),
    p("The Merge module combines the Primary and Secondary datasets using join operations from ",
      code("dplyr"), ". Developers can choose the join keys and add extra columns from the Secondary dataset. ",
      "Newly merged columns are prefixed to keep lineage clear; Integration/Core handle metadata refresh and filter resets after structural changes."),

    h3("Files & Entrypoints"),
    tags$ul(
      tags$li(code("modules/Data Wizard/datawizard_merge.R")),
      tags$li(strong("UI:"), " ", code("modMergeUI(id)")),
      tags$li(strong("Server:"), " ", code("modMergeServer("),
              code("id, get_data, set_data, get_data2, UI_config, debug_level)"), ")")
    ),

    h3("Integration Contract (Server Arguments)"),
    p("The server follows the project-wide module contract and expects:"),
    tags$ul(
      tags$li(code("get_data(): data.frame | NULL"), " — returns current Primary data."),
      tags$li(code("set_data(df): logical"), " — writes updated Primary data."),
      tags$li(code("get_data2(): data.frame | NULL"), " — returns current Secondary (Additional) data."),
      tags$li(code("UI_config(): list | NULL"), " — imported UI configuration (from RDS; may be a flat list or nested under ", code("$merge"), ")."),
      tags$li(code("debug_level: numeric"), " — 0=none, 1=essential, 2=verbose; used by the module-scoped ", code("debug_log"), " closure.")
    ),

    h3("Return Value (Server → Integration)"),
    p("The server returns a small list of accessors/hooks used by Integration and for export/testing:"),
    tags$ul(
      tags$li(code("get_primary_data(), set_primary_data()"), " — thin wrappers over the injected getters/setters."),
      tags$li(code("get_secondary_data()"), " — convenience wrapper (Secondary)."),
      tags$li(code("apply_trigger()"), " — reactive tied to ", code("input$apply_merge"), "."),
      tags$li(code("get_current_ui_state()"), " — serializable state for RDS (see UI_config keys below)."),
      tags$li(code("get_performance_metrics()"), " — includes ", code("last_operation_time"), " and ", code("operation_history"), "."),
      tags$li(code("module_health_check()"), " — snapshot with status/messages/timestamps."),
      tags$li(code("test_module_functionality()"), " — internal smoke test that writes key diagnostics via ", code("debug_log"), ".")
    ),

    h3("UI Controls (as implemented)"),
    tags$ul(
      tags$li(code("output$merge_ready"), " — “true/false” sentinel to gate UI; requires both Primary and Secondary to be present and non-empty."),
      tags$li(code("selectInput(ns('file1_col'))"), " — Primary join column."),
      tags$li(code("selectInput(ns('file2_col'))"), " — Secondary join column."),
      tags$li(code("selectInput(ns('file2_add_col'), multiple = TRUE)"), " — extra Secondary columns to carry along (join column is always included)."),
      tags$li(code("selectInput(ns('join_type'))"), " — join type: ", code("left | inner | full"), " (right is not exposed in the UI)."),
      tags$li(code("actionButton(ns('apply_merge'))"), " — apply the merge."),
      tags$li(code("output$merge_preview_dim"), " + ", code("output$merge_preview_table"),
              " — live preview (dimensions + sampled DT head) based on current selections.")
    ),

    h3("UI_config (Import/Export)"),
    p("The module consumes and emits the following keys under ", code("UI_config$merge"), " (or as a flat list if emitted directly):"),
    tags$ul(
      tags$li(code("file1_col: character(1)"), " — Primary join column."),
      tags$li(code("file2_col: character(1)"), " — Secondary join column."),
      tags$li(code("file2_add_col: character()"), " — optional extra columns from Secondary."),
      tags$li(code("join_type: 'left'|'inner'|'full'"), " — join type.")
    ),
    p("On import, the module observes ", code("UI_config()"), " and applies a config block via ",
      code("set_merge_ui_config_from_import(cfg)"), " (provided by Core/Integration). ",
      "Both a nested ", code("list(merge = <cfg>)"), " and a flat ", code("<cfg>"), " are supported. ",
      "Input updates use ", code("freezeReactiveValue"), " + ", code("updateSelectInput"), " and respect current column availability; invalid keys are ignored with a user notification. ",
      "UI-config bookkeeping is tracked via ", code("ui_config_applied()"), " and ", code("ui_config_source()"), "."),

    h3("Validation"),
    p("Before merge, the module performs thorough input/data validation:"),
    tags$ul(
      tags$li(strong("Primary/Secondary existence & shape:"), " both must be data.frames with ≥1 row and ≥1 col."),
      tags$li(strong("Join columns:"), " ", code("file1_col ∈ names(Primary)"), ", ", code("file2_col ∈ names(Secondary)"), "."),
      tags$li(strong("Additional columns:"), " if provided, must exist in Secondary (duplicates are deduped)."),
      tags$li(strong("Join type:"), " must be one of ", code("left/inner/full"), ".")
    ),
    p("Validation returns a structure with ", code("$valid"), ", ", code("$errors"), ", and ", code("$warnings"),
      " (warnings are displayed but do not block)."),

    h3("Preview"),
    p("The preview renders dimensions and a small DT head for the prospective result using the current selections:"),
    tags$ul(
      tags$li(code("output$merge_preview_dim"), " — text like ", code("'Rows x Cols'"), "."),
      tags$li(code("output$merge_preview_table"), " — head of the merged result on a reduced sample; errors are caught and displayed.")
    ),

    h3("Execution (Apply Handler)"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        '1) Guard concurrent runs via merge_processing_active()
2) Validate inputs → show aggregated warnings (non-blocking) and stop on errors
3) Open a shiny::Progress; fetch primary/secondary via get_*()
4) Compute Secondary subset = file2_col + file2_add_col (unique)
5) Perform join (dplyr), mapping key names:
     result <- switch(join_type,
       "left"  = dplyr::left_join (primary, secondary_subset, by = setNames(file2_col, file1_col)),
       "inner" = dplyr::inner_join(primary, secondary_subset, by = setNames(file2_col, file1_col)),
       "full"  = dplyr::full_join (primary, secondary_subset, by = setNames(file2_col, file1_col))
     )
6) Prefix only the newly introduced columns:
     new_cols <- setdiff(names(result), names(primary))
     for (col in new_cols) names(result)[names(result) == col] <- paste0("Merged_", col)
7) set_data(result) — Integration/Core refresh metadata and may reset filters
8) Log/notify success; append to operation_history and operation_log; close progress; clear active flag'
      )
    ),
    p("Only new columns from Secondary are prefixed (", code("'Merged_'"), "); existing Primary columns keep their names."),

    h3("Logging & Diagnostics"),
    p("The module defines a local ", code("debug_log(msg, level)"), " that prints to console when ",
      code("debug_level >= level"), ". Notable logs:"),
    tags$ul(
      tags$li(strong("Level 1:"), " start/end, validation failures, errors during join/progress, success summary (rows/cols added)."),
      tags$li(strong("Level 2:"), " chosen join type, selected keys, additional columns, intermediate dimensions, progress milestones.")
    ),
    p("Operational telemetry is stored in:"),
    tags$ul(
      tags$li(code("last_operation_time: reactiveVal()")),
      tags$li(code("operation_history: reactiveVal(list())")),
      tags$li(code("processing_errors: reactiveVal(list())")),
      tags$li(code("operation_log: reactiveVal(list())"))
    ),

    h3("Error Handling & User Feedback"),
    tags$ul(
      tags$li("All critical paths are wrapped in ", code("tryCatch()"), " and user-facing ", code("showNotification()"), " calls."),
      tags$li("Errors are logged (level 1) and appended to ", code("processing_errors()"), " and ", code("operation_log()"), "."),
      tags$li("A progress dialog (", code("shiny::Progress"), ") covers long operations and is always closed in ", code("finally"), ".")
    ),

    h3("Health Snapshot"),
    p("The module exposes ", code("module_health_check()"), " and a test helper ",
      code("test_module_functionality()"), " that print current flags (processing, error count, debug_level, UI-config state) and evaluate overall health."),

    h3("Best Practices"),
    tags$ul(
      tags$li("Use stable identifiers for join columns (avoid columns that are later edited/renamed by Edit/Pivot)."),
      tags$li("Be explicit with ", code("file2_add_col"), " to avoid bringing unnecessary columns into the Primary table."),
      tags$li("Expect Integration/Core to refresh metadata and potentially reset active filters after new columns are added."),
      tags$li("When extending the module with new join types, wire them through the same validation/preview/apply path and extend UI_config import/export accordingly.")
    )
  )
}
