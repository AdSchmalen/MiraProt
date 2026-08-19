# ./Documentation/datawizard_doc_tech_core.R
# Canonical repository path: Documentation/datawizard_doc_tech_core.R
# Datawizard Documentation — Technical Core Documentation
#
# Contains content rendering functions for core architectural documentation:
#   - Architecture Overview
#   - Module Loading & Parameter Wiring
#   - Data Flow & State Management
#   - Logging & Debugging
#
# These functions are called by the documentation server in datawizard_doc_ui.R.

############
# Architecture Overview

render_dwdocs_tech_overview_content <- function() {
  div(
    h2("Technical Documentation — Architecture Overview"),
    hr(),

    h3("What Datawizard Is"),
    p("Datawizard is a modular Shiny system for loading quantitative proteomics data, defining metadata, transforming data through submodules, and exporting results. A parent module initializes shared state (Core), delegates submodule wiring to an Integration layer, and exposes a cross-module bus (rv) so modules always see the freshest data and metadata."),

    h3("High-Level Layers"),
    tags$ul(
      tags$li(
        strong("Parent (datawizard_module.R): "),
        "Creates Core, starts the File Loader, invokes Integration to instantiate submodules, and exposes convenience APIs (status, exports)."
      ),
      tags$li(
        strong("Core (datawizard_core.R): "),
        "Central reactive stores for data and metadata (including a raw snapshot for resets), a central UI-config store, typed UI-config setters, metadata update functions, modification tracking, and safe-UI helpers."
      ),
      tags$li(
        strong("Integration (datawizard_integration.R): "),
        "Instantiates submodules with a consistent parameter contract (data/metadata getters, update callbacks, per-module safe UI system, readiness), and forwards Assign Rules → Core UI-config triggers."
      ),
      tags$li(
        strong("Utilities: "),
        code("datawizard_utils.R"), " (validation, debug, helpers), ",
        code("datawizard_tables.R"), " (display helpers for data and metadata)."
      ),
      tags$li(
        strong("Exports: "),
        code("modules/Data Wizard/datawizard_export.R"), " (in-app Excel export), and ",
        code("R/export.R"), " (standalone Excel orchestrator at project root)."
      )
    ),

    h3("Data Lifecycle — From Load to Export"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'Parent (datawizard_module.R)
  ├─ Core (central reactives, UI-config, metadata utils; keeps raw snapshot for resets)
  ├─ File Loader (Primary data; Additional data exposed for Merge; sheet & header-row selection)
  └─ Integration.initialize_submodules()
       ├─ Assign Rules (RDS load → UI-config triggers, condition options)
       ├─ Auto-Assign  (regex rule frames owner; export/apply; safe updates inside module)
       ├─ Batch Effects / Pivot / Merge        (operate without metadata upfront; update metadata afterwards)
       ├─ Filtering / Edit / Imputation / Ratios / Basemean  (require metadata; wired with data_def)
       └─ Per-module safe UI systems and cross-module observers

file load -> registry/core/rv mirrors -> submodule processing -> tables/export/session save -> restore/reset

Central registry + adapter
  ├─ primary_original / primary_raw / primary_working / primary_filtered / primary_final
  ├─ secondary_original / secondary_working
  ├─ metadata_working / metadata_final
  ├─ core reactiveVal fields and debounced revision signals
  └─ legacy rv mirrors for compatibility (rv$data_mod, rv$data_def, rv$primary_data_raw, ...)

Export (Excel) and session save/restore
  ├─ resolve display/export data through registry/core state
  ├─ collect loader state and submodule UI states
  └─ restore loader data, registry entries, metadata snapshots, and guarded UI state'
      )
    ),

    h3("1) Loading Data (Primary & Additional)"),
    tags$ul(
      tags$li(
        strong("File Loader (parent server): "),
        code("modFileLoaderServer('loader', rv, core_values = core_values, ...)"),
        " reads Primary data (universally available to submodules) and optionally Additional data. ",
        "Excel sheets and the header row are selectable. ",
        "Additional data is exposed to submodules via Integration only where relevant (e.g., Merge gets ", code("get_data2"), ")."
      ),
      tags$li(
        strong("Core stores: "),
        code("core_values$primary_data_raw"), " (canonical raw snapshot used for fallbacks/resets), and ",
        code("core_values$handson_metadata"), " (working metadata table)."
      ),
      tags$li(
        strong("Readiness gate: "),
        code("create_metadata_content_status(core_values)"),
        " ensures modules don’t act before data/metadata exist."
      )
    ),

    h3("2) Defining Metadata"),
    tags$ul(
      tags$li(
        strong("Auto-Assign (datawizard_auto_assign.R + auto assign/): "),
        code("datawizard_auto_assign.R"),
        " is the public orchestration entrypoint. ",
        "Authoritative rule state, UI composition, rule execution, handlers, integration adapters, template loading, and output registration are delegated to focused files under ",
        code("modules/Data Wizard/auto assign/"),
        ". The canonical rule model uses stable RuleId/VariantId identity, with Priority on Content rules. ",
        "Auto RegEx infers candidate frames but transfers them through Auto-Assign's public transactional loader rather than owning another rule store."
      ),
      tags$li(
        strong("Assign Rules (datawizard_assign_rules.R): "),
        "Loads .RDS with ", code("readRDS"), ", extracts per-module UI-config blocks and dedicated payloads, ",
        "emits reactive triggers for Core’s typed setters, and exposes current condition options. ",
        "It does not apply regex frames itself."
      ),
      tags$li(
        strong("Core metadata store: "),
        code("core_values$handson_metadata"), " is the single source of truth; ",
        code("datawizard_tables.R"), " contains display helpers only."
      )
    ),

    h3("3) Transforming Data (Overview; ordered by dependency)"),
    p("Integration wires a consistent contract: data getter (prefer ", code("rv$data_mod"), " → filtered → raw), metadata getter (prefer ", code("rv$data_def"), "), update callback (write back, refresh metadata, reset filters on structural change), per-module safe UI system, and debug level."),
    tags$ul(
      tags$li(strong("Batch Effects:"), " corrects numeric intensities without needing metadata upfront; updates metadata afterwards."),
      tags$li(strong("Pivot:"), " reshapes data; updates metadata for pivoted columns."),
      tags$li(strong("Merge:"), " joins Primary with Additional using configured keys; updates metadata."),
      tags$li(strong("Filtering:"), " requires metadata; sets ", code("core_values$filter_applied"), " and updates ", code("core_values$filtered_data"), "."),
      tags$li(strong("Edit:"), " requires metadata; applies tabular edit operations with modification history."),
      tags$li(strong("Imputation:"), " requires metadata; fills missing values using configured strategies."),
      tags$li(strong("Ratios & Statistics:"), " requires metadata; creates ratio columns and runs selected statistics."),
      tags$li(strong("Basemean:"), " requires metadata; computes basemean per selected samples/abundance type.")
    ),
    p("When structure changes (new/renamed columns), Integration refreshes metadata and may reset filters to the raw snapshot."),

    h3("4) State Propagation & Contracts"),
    tags$ul(
      tags$li(strong("Data getter:"), " prefer ", code("rv$data_mod"), " → ", code("core_values$filtered_data"), " → ", code("core_values$primary_data_raw"), "."),
      tags$li(strong("Metadata getter:"), " prefer ", code("rv$data_def"), " → ", code("core_values$handson_metadata"), "."),
      tags$li(strong("Update callback:"), " write into ", code("rv$data_mod"), " and Core; call metadata update helpers; reset filters when structure changes."),
      tags$li(strong("UI-config pipeline:"), " Assign Rules emits triggers → Integration applies via Core’s typed setters."),
      tags$li(strong("Safe UI systems:"), " created per module in Integration (not only Auto-Assign).")
    ),

    h3("5) Exporting Results (Excel)"),
    tags$ul(
      tags$li(
        strong("In-app: "),
        code("datawizard_export.R::create_excel_export_functions(...)"),
        " builds a workbook with **Primary (original)**, **Additional (if any)**, **Modified (current processed)**, **Metadata**, plus an **Export_Info** sheet describing processing status."
      ),
      tags$li(
        strong("Standalone: "),
        code("R/export.R::create_comprehensive_excel(...)"),
        " lives outside the modules folder and can aggregate outputs from Datawizard and external modules (e.g., GO results) when provided."
      ),
      tags$li(
        strong("Validation & provenance: "),
        "primary data and metadata are validated; status annotations and debug logging included."
      )
    ),

    h3("Summary"),
    p("Datawizard centralizes state in Core, loads Primary/Additional via the Loader (with sheet and header-row control), wires submodules through Integration with consistent contracts, defines metadata via Auto-Assign + Assign Rules, keeps the freshest state on the ", code("rv"), " bus, and exports a reproducible Excel package. Tables are for visualization only; safe UI systems are per-module; ", code("primary_data_raw"), " remains the reset baseline.")
  )
}

############
# Module Loading & Parameter Wiring

render_tech_module_loading_content <- function() {
  div(
    h2("Technical Documentation — Module Loading & Parameter Wiring"),
    hr(),

    h3("Purpose"),
    p("This document explains how the parent Data Wizard module loads all submodules and wires their parameters.
       It covers the boot sequence, the cross-module contracts passed into each submodule,
       the central reactive stores and UI-config bridges provided by Core, and the Integration layer that coordinates updates."),

    h3("Files & Responsibilities"),
    tags$ul(
      tags$li(
        strong("datawizard_module.R (parent): "),
        "UI layout (left: Loader & Assign Rules, center: Tables & Auto-Assign, right: processing panels). ",
        "Server creates central state (Core), starts the File Loader, initializes submodules via ",
        code("initialize_submodules(...)"),
        ", and registers export/import/status helpers."
      ),
      tags$li(
        strong("datawizard_core.R (Core): "),
        code("create_core_reactive_values()"), ", ",
        code("create_ui_config_reactive_values()"), ", ",
        code("create_data_access_functions()"), ", ",
        code("create_modification_tracking_functions()"), ", ",
        code("create_metadata_update_functions()"), ", ",
        code("create_metadata_content_status()"), ", ",
        code("create_ui_config_management_functions()"), " (typed UI setters), ",
        code("create_safe_ui_system()"), " (defensive UI updates)."
      ),
      tags$li(
        strong("datawizard_integration.R (Integration): "),
        code("initialize_submodules(...)"), " wires all submodules with consistent parameter contracts, ",
        "creates per-module ", code("safe_ui_systems"), ", sets up cross-module observers (filters, basemean, ratios, edits, batch, pivot, merge), ",
        "and forwards Assign Rules UI-config triggers to Core via typed setters (",
        code("setup_ui_config_triggers(...)"), ")."
      )
    ),

    h3("Compatibility loaders, implementation ownership, and source order"),
    p(
      "The historical files ", code("datawizard_core.R"), ", ",
      code("datawizard_file_loader.R"), ", and ", code("datawizard_tables.R"),
      " remain compatibility loaders and the only application-facing source paths. In particular, ",
      code("modules/Data Wizard/datawizard_file_loader.R"),
      " is the File Loader compatibility/orchestration entrypoint. Focused files under ",
      code("core/"), ", ", code("modules/Data Wizard/file_loader/"), ", and ", code("tables/"),
      " own their named implementations; moving a function does not move or duplicate its public contract."
    ),
    tags$ul(
      tags$li("Core sources utilities and the dataset registry before projections, adapters, reactive factories, UI configuration, metadata updates, safe UI, submodule-session, and lifecycle implementations."),
      tags$li("modules/Data Wizard/file_loader/ owns File Loader UI, shared context, observer families, restore, and diagnostics, and continues to own its reading and canonicalization primitives. The compatibility entrypoint sources them in the established order before exposing the historical module functions."),
      tags$li("Tables sources logic and state first, creates exactly one shared observer context, then registers hydration, rendering/mutation, and metadata-editing phases in order. A session must never create a second Loader context or a second Tables context."),
      tags$li("Focused implementation files may be directly sourced for compatibility tests only when their documented prerequisites have already been sourced; production callers use the compatibility loaders."),
      tags$li("The public factory names, module arguments, returned names, reactive keys, callbacks, session payloads, and restore behavior are unchanged by this structural split.")
    ),

    h3("Boot Sequence (Server)"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'modDataWizardServer(id, rv) {
  # 1) Core state & helpers
  core_values        <- create_core_reactive_values()
  ui_config_values   <- create_ui_config_reactive_values()
  data_access        <- create_data_access_functions(core_values)
  modifications      <- create_modification_tracking_functions(core_values)
  metadata_fx        <- create_metadata_update_functions(core_values)
  ui_config_fx       <- create_ui_config_management_functions(ui_config_values, core_values)
  meta_ready         <- create_metadata_content_status(core_values)

  # 2) File loader (primary data, headers, init metadata)
  loader_out <- modFileLoaderServer("loader", rv, core_values = core_values, debug_level = DEBUG_LEVEL)

  # 3) Submodules (Integration orchestrates parameter wiring)
  modules <- initialize_submodules(
               session, loader_out, core_values, ui_config_values,
               data_access, modifications, metadata_fx, ui_config_fx, meta_ready, rv
             )

  # 4) Export/status helpers (parent convenience API)
  excel_fx   <- create_excel_export_functions(loader_out, core_values, modifications)
  cfg_export <- create_config_export_functions(modules, ui_config_values, core_values)

  # 5) Assign Rules → Core UI-config setters (typed) via reactive triggers
  setup_ui_config_triggers(modules$assign_rules_out, ui_config_fx)
}'
      )
    ),

    h3("Core Reactive Stores & UI-Config Bridge"),
    tags$ul(
      tags$li(
        strong("Central data/metadata (Core): "),
        code("core_values$primary_data_raw"), ", ",
        code("core_values$handson_metadata"), ", ",
        code("core_values$filtered_data"), ", ",
        code("core_values$filter_applied"), ", ",
        code("core_values$modification_history"), " and status flags."
      ),
      tags$li(
        strong("Central UI-config store (Core): "),
        code("ui_config_values$central_*_ui_config"), " plus per-module flags (e.g., ",
        code("filtering_update_in_progress"), ")."
      ),
      tags$li(
        strong("Typed UI-config setters (Core): "),
        code("set_filtering_ui_config_from_import()"), ", ",
        code("set_imputation_ui_config_from_import()"), ", ",
        code("set_batch_effects_ui_config_from_import()"), ", ",
        code("set_pivot_ui_config_from_import()"), ", ",
        code("set_merge_ui_config_from_import()"), ", ",
        code("set_ratios_ui_config_from_import()"), ", ",
        code("set_basemean_ui_config_from_import()"), "."
      ),
      tags$li(
        strong("Safe UI System (per module): "),
        code("create_safe_ui_system(session, name, DEBUG_LEVEL)"),
        " provides ", code("update_input_safely()"), ", ",
        code("show_notification_safely()"), ", ",
        code("execute_when_ready()"), " to avoid timing/race issues."
      ),
      tags$li(
        strong("Metadata readiness: "),
        code("create_metadata_content_status(core_values)"), " emits a reactive readiness gate that submodules can use."
      )
    ),

    h3("The Cross-Module Data Bus (rv)"),
    p("The parent receives and passes an external reactive bus ", code("rv"), " holding transient, session-level artifacts:"),
    tags$ul(
      tags$li(code("rv$data_mod"), ": the latest processed/derived data (submodules write here after transformations)."),
      tags$li(code("rv$data_def"), ": the latest enhanced metadata (synced after structural changes).")
    ),
    p("Integration prefers ", code("rv$data_mod"), " / ", code("rv$data_def"), " when present. Fallbacks are Core’s raw data and metadata."),

    h3("How Submodules Are Instantiated (Integration)"),
    p("Integration consistently wires each submodule with:"),
    tags$ul(
      tags$li("A data getter that prioritizes ", code("rv$data_mod"), " → filtered data → raw data."),
      tags$li("A metadata getter that prioritizes ", code("rv$data_def"), " → Core metadata."),
      tags$li("A callback to push updates back into ", code("rv"), " and Core, with appropriate metadata refresh and filter resets."),
      tags$li("A module-scoped UI_config value built via Core’s ", code("ui_config_functions$create_*_ui_config(assign_rules_out)"), "."),
      tags$li("Auxiliary signals like ", code("metadata_ready_status"), " and ", code("debug_level"), ".")
    ),

    h3("Parameter Contracts by Submodule (selected)"),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Submodule (Server)"),
          tags$th("Key inputs"),
          tags$th("Key outputs / callbacks")
        )
      ),
      tags$tbody(
        # Assign Rules
        tags$tr(
          tags$td(code("modAssignRulesServer('assign_rules', ...)")),
          tags$td(
            span(code("rule_files"), ", "),
            span(code("metadata_current"), " (", code("core_values$handson_metadata"), "), "),
            span(code("rv"), " (bus), "),
            span(code("debug_level"))
          ),
          tags$td("Reactive UI-config triggers for other modules; current condition names; aggregated notifications.")
        ),
        # Auto-Assign
        tags$tr(
          tags$td(code("modAutoAssignServer('auto_assign', ...)")),
          tags$td(
            span(code("metadata_skeleton"), " (init via ", code("loader_out$init_meta"), "), "),
            span(code("rule_files"), ", "),
            span("module refs: ", code("filter_module"), ", ", code("edit_module"), ", ", code("ratios_module"), ", ", code("batch_module"), ", ", code("pivot_module"), ", ", code("merge_module"), ", ", code("imputation_module"), ", ", code("basemean_module"), "), "),
            span(code("UI_config"), " (for export packaging), "),
            span(code("filter_applied"), ", "),
            span(code("data_modified"), ", "),
            span(code("modules_list"), ", "),
            span(code("parent_session"), ", "),
            span(code("rv"))
          ),
          tags$td("Applies rule frames; may apply ratio/filter/edit payloads; syncs UI preview; updates metadata when needed.")
        ),
        # Filtering
        tags$tr(
          tags$td(code("modFilteringServer('filtering_ui', ...)")),
          tags$td(
            span(code("data"), " & ", code("get_data"), " (rv → filtered → raw), "),
            span(code("metadata_def"), " (rv → Core + cleaning), "),
            span(code("init_meta = loader_out$init_meta"), ", "),
            span(code("UI_config = ui_config_functions$create_filtering_ui_config(assign_rules_out)"), ", "),
            span(code("metadata_ready_status"), ", "),
            span(code("debug_level"))
          ),
          tags$td("Reactive trigger to apply filters; updates ", code("core_values$filtered_data"), " + flags; records modifications.")
        ),
        # Imputation
        tags$tr(
          tags$td(code("modImputationServer('imputation', ...)")),
          tags$td(
            span(code("get_data"), " (rv → filtered → raw), "),
            span(code("update_data"), " (write back to rv/Core), "),
            span(code("UI_config = ui_config_functions$create_imputation_ui_config(assign_rules_out)"), ", "),
            span(code("debug_level"))
          ),
          tags$td("Applies imputation; updates data; may trigger downstream refresh via Integration.")
        ),
        # Ratios
        tags$tr(
          tags$td(code("modRatiosServer('ratios', ...)")),
          tags$td(
            span(code("data_def"), " (rv → Core metadata), "),
            span(code("get_data"), " (rv → filtered → raw), "),
            span("plus UI-config and rule frame access via Auto-Assign/Assign Rules where relevant")
          ),
          tags$td("Applies ratio columns; updates data; Integration refreshes metadata for new columns.")
        ),
        # Basemean
        tags$tr(
          tags$td(code("modBasemeanServer('basemean', ...)")),
          tags$td(
            span(code("data_def"), ", ", code("get_data"), ", "),
            span(code("available_samples = assign_rules_out$current_conditions()"), ", "),
            span(code("UI_config = ui_config_functions$create_basemean_ui_config(assign_rules_out)"))
          ),
          tags$td("Creates basemean columns; updates data & metadata; resets filters if necessary.")
        ),
        # Batch Effects
        tags$tr(
          tags$td(code("modBatchEffectsServer('batch', ...)")),
          tags$td(
            span(code("get_data"), ", ", code("update_data"), ", "),
            span(code("init_meta = loader_out$init_meta"), ", "),
            span(code("header_primary = loader_out$header_primary"), ", "),
            span(code("UI_config = ui_config_functions$create_batch_effects_ui_config(assign_rules_out)"))
          ),
          tags$td("Applies batch correction; updates data & metadata; filter reset on structure change.")
        ),
        # Pivot, Merge, Edit, Tables (similar)
        tags$tr(
          tags$td("Pivot / Merge / Edit / Tables"),
          tags$td("Each receives data/metadata getters, UI_config (where applicable), and debug level."),
          tags$td("Each pushes updates into ", code("rv"), " and Core; Integration handles metadata sync and UI safety.")
        )
      )
    ),

    h3("Assign Rules → Core UI-Config Application"),
    p("Assign Rules does not own regex frames; it loads an .RDS, extracts per-module UI-config blocks and dedicated payloads, and emits them as reactive triggers. Integration listens and applies them via Core’s typed setters:"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'setup_ui_config_triggers(assign_rules_out, ui_config_functions)
# Internally:
# safe_ui_config_observer(assign_rules_out$assign_rules_ui_filtering,
#                         ui_config_functions$set_filtering_ui_config_from_import, "Filtering")
# ... similarly for Imputation, Batch Effects, Pivot, Merge, Ratios, Basemean'
      )
    ),

    h3("Readiness, Error Handling & Notifications"),
    tags$ul(
      tags$li("Submodules are wrapped in ", code("tryCatch"), " on initialization with user-friendly ", code("showNotification(...)"), "."),
      tags$li("Sensitive UI operations use per-module ", code("safe_ui_systems"), " (", code("update_input_safely"), ", ", code("execute_when_ready"), ")."),
      tags$li("When data structure changes (e.g., Basemean, Ratios, Batch), Integration ensures metadata refresh and may reset filters."),
      tags$li("Assign Rules aggregates import notifications; Auto-Assign logs rule application; Core records ", code("modification_history"), ".")
    ),

    h3("Adding a New Submodule (pattern)"),
    tags$ol(
      tags$li("Implement the new module’s UI/Server with a parameter contract matching existing modules: ",
              code("get_data"), "/", code("data"), ", ", code("data_def"), ", ", code("update_data"), ", optional ", code("UI_config"), "."),
      tags$li("Add sourcing to the parent (UI code already sources with a local environment)."),
      tags$li("In ", code("initialize_submodules(...)"), ", instantiate your module with the standard getters/setters and ",
              "provide a ", code("ui_config_functions$create_*_ui_config(assign_rules_out)"), " if you need UI-config."),
      tags$li("If the module should respond to imported UI-configs, expose a reactive trigger in Assign Rules and add it to ",
              code("setup_ui_config_triggers(...)"), " pointing to a typed Core setter (add one in ",
              code("create_ui_config_management_functions(...)"), ")."),
      tags$li("On structural changes, call Core metadata update helpers (e.g., for new columns) and reset filters if needed.")
    ),

    h3("Documentation Assessment"),
    tags$ul(
      tags$li("Describes the actual boot order from ", code("datawizard_module.R"), " (Core → Loader → Integration → Export/Import helpers)."),
      tags$li("Explains how ", code("initialize_submodules(...)"), " wires consistent parameter contracts and safe UI systems."),
      tags$li("Clarifies roles: Auto-Assign owns rule frames & export/apply; Assign Rules loads files and emits UI-config triggers; Core stores and applies configs; Integration coordinates."),
      tags$li("Reflects concrete function names used in the codebase (no placeholder APIs).")
    ),

    h3("Summary"),
    p("The parent module centralizes state in Core, lets the Loader initialize data and metadata, and then relies on Integration to instantiate and wire each submodule with consistent contracts. Assign Rules emits UI-config triggers that Core applies via typed setters, while Auto-Assign owns rule frames and the export/apply path for imported bundles. The cross-module bus ", code("rv"), " carries the latest processed data and metadata across submodules.")
  )
}

############
# Data Flow & State Management

render_tech_data_flow_content <- function() {
  div(
    h2("Technical Documentation — Data Flow & State Management"),
    hr(),

    h3("Data Flow Overview"),
    pre(
      style = "background-color:#f8f9fa; padding:14px; border-radius:6px; font-family:monospace;",
      '1) LOAD
   File(s) → File Loader
     • Select Excel sheet (if applicable)
     • Select header row
   → core_values$primary_data_raw   (canonical raw snapshot)
   → core_values$handson_metadata   (initial skeleton)

2) METADATA DEFINITION
   Auto-Assign (regex rule frames: table/condition/ratio) → apply to headers → update metadata
   Assign Rules (import .RDS, emit UI-config triggers) → Core typed setters → update metadata
   → rv$data_def (latest enhanced metadata)

3) TRANSFORM
   (order by dependency)
   No metadata required upfront:
     • Batch Effects → data
     • Pivot → data
     • Merge (Primary + Additional) → data
   Metadata required:
     • Filtering → core_values$filtered_data + filter_applied
     • Edit → data
     • Imputation → data
     • Ratios & Statistics → data
     • Basemean → data
   → rv$data_mod (latest processed data)

4) PREVIEW & TABLES
   get_data():    rv$data_mod  ▷  core_values$filtered_data  ▷  core_values$primary_data_raw
   get_metadata(): rv$data_def ▷  core_values$handson_metadata
   (Tables render only; no mutation)

5) EXPORT
   datawizard_export (in-app) / R/export.R (standalone):
     • Primary (original), Additional (if any), Modified (current), Metadata
     • Export_Info/status sheets
'
    ),

    h3("Core State, Dataset Registry & Compatibility Mirrors"),

    h4("Central dataset roles"),
    p("The registry is the canonical role/revision store. Use role-aware resolution instead of reaching into loader-local values or legacy rv fields."),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(tags$tr(tags$th("Role group"), tags$th("Registry roles"), tags$th("Purpose"))),
      tags$tbody(
        tags$tr(tags$td("Original"), tags$td(code("primary_original"), ", ", code("secondary_original")), tags$td("First loaded snapshots; treated as reset baselines and protected except explicit load/restore updates.")),
        tags$tr(tags$td("Raw"), tags$td(code("primary_raw")), tags$td("Primary data as imported/currently selected before downstream processing.")),
        tags$tr(tags$td("Working"), tags$td(code("primary_working"), ", ", code("secondary_working")), tags$td("Current editable/processable data used by Datawizard modules.")),
        tags$tr(tags$td("Filtered"), tags$td(code("primary_filtered")), tags$td("Filtering output used by previews/export when filters are active and final processing is not applied.")),
        tags$tr(tags$td("Final"), tags$td(code("primary_final")), tags$td("Final processed data preferred by export once apply/finalization has run.")),
        tags$tr(tags$td("Metadata"), tags$td(code("metadata_working"), ", ", code("metadata_final")), tags$td("Current and final metadata snapshots aligned to Primary columns."))
      )
    ),
    p("Each registry entry records dataset identity, role, revision, dimensions, column signature, source metadata, data or lazy reference, and creation time."),

    h4("Core reactiveVal fields"),
    tags$ul(
      tags$li(code("core_values$dataset_registry"), " — central role/revision registry."),
      tags$li(code("core_values$primary_data_raw"), " — raw Primary data mirror for core/legacy callers."),
      tags$li(code("core_values$handson_metadata"), " — working metadata table."),
      tags$li(code("core_values$filtered_data"), " and ", code("core_values$filter_applied"), " — filtered output and activation flag."),
      tags$li(code("core_values$final_processed_data"), ", ", code("core_values$final_processed_metadata"), ", ", code("core_values$apply_triggered"), " — final output and apply state."),
      tags$li("Revision signals: ", code("primary_working_revision"), ", ", code("primary_raw_revision"), ", ", code("primary_filtered_revision"), ", ", code("metadata_revision"), ", ", code("secondary_revision"), "; expensive observers should prefer the debounced variants."),
      tags$li(code("metadata_content_signature"), " — compact Column/Content/Options projection for consumers such as Primary table coloring that do not need the entire metadata table."),
      tags$li(code("ui_config_values$central_*_ui_config"), " — per-module UI-config store applied through typed setters.")
    ),

    h4("Legacy rv compatibility fields"),
    tags$ul(
      tags$li(code("rv$data_mod"), " — current Primary working data mirror."),
      tags$li(code("rv$data_def"), " — current working metadata mirror."),
      tags$li(code("rv$primary_data_raw"), ", ", code("rv$primary_data_original"), ", ", code("rv$primary_data_source"), ", ", code("rv$primary_data_operation"), " — Primary compatibility/provenance mirrors."),
      tags$li(code("rv$secondary_data_original"), " — Secondary reset/restore fallback."),
      tags$li(code("rv$filter_applied"), ", ", code("rv$filtered_data"), ", ", code("rv$final_processed_data"), ", ", code("rv$final_processed_metadata"), ", ", code("rv$data_modified"), " — compatibility state for existing observers.")
    ),
    div(
      class = "alert alert-warning",
      strong("Maintenance rule: "),
      "new Data Wizard modules must use the central resolver/adapter and must not write directly to ",
      code("rv$data_mod"), " or ", code("rv$data_def"),
      " except through compatibility APIs owned by the data adapter."
    ),

    div(
      class = "well",
      h4("Role-aware resolver/adapter contract"),
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'resolve_datawizard_dataset(role = "original" | "raw" | "working" | "filtered" | "final" | "display" | "export")

Adapter writes:
  set_raw_imported_data()  -> primary_original + primary_raw + primary_working + rv mirrors
  set_modified_data()      -> primary_working + optional metadata_working + rv mirrors
  set_filtered_data()      -> primary_filtered + core filtered_data
  set_final_data()         -> primary_final + core final_processed_data
  set_metadata_final()     -> metadata_final + core final_processed_metadata'
      )
    ),

    h3("Loader-local fields (Primary & Additional)"),
    p("Loader-local reactive values are implementation details. Other modules should consume injected getters, the central resolver, or adapter-published state."),
    tags$ul(
      tags$li(code("data_fixed()"), " and ", code("data2_fixed()"), " — current Primary and Additional sheet/data frames after header and type handling."),
      tags$li(code("primary_data_original()"), " and ", code("secondary_data_original()"), " — loader-captured original snapshots."),
      tags$li(code("sheet_cache_primary"), " and ", code("sheet_cache_secondary"), " — workbook caches; session snapshots omit full caches and restore a one-sheet runtime cache."),
      tags$li("Interactive Primary sheet changes are allowed even when the previous sheet had derived columns. The loader treats a sheet switch as a new Primary data context, republishes raw/working data, clears filtered/final/modified processing state, and keeps the reset snapshots untouched."),
      tags$li(code("primary_file_meta()"), " and ", code("secondary_file_meta()"), " — file/sheet metadata."),
      tags$li("Inputs such as ", code("sheetDropdown"), ", ", code("sheetDropdown2"), ", ", code("header_row"), ", ", code("header_row2"), ", and ", code("data_source_type"), " are saved/restored through loader session-state hooks."),
      tags$li("Restore-mode guards prevent restored header/sheet choices from being interpreted as new interactive loads.")
    ),

    h3("Metadata creation & updates"),
    tags$ul(
      tags$li(strong("Auto-Assign:"), " owns & applies regex rule frames (", code("table"), "/", code("condition"), "/", code("ratio"), ") to classify columns, extract samples, and prepare ratio parsing. It also exports .RDS bundles and applies imported rule frames (", code("load_rules_directly"), "). Safe input updates are used inside this module."),
      tags$li(strong("Assign Rules:"), " loads .RDS (", code("readRDS"), "), extracts per-module UI-configs/dedicated blocks (e.g., ", code("ratio_configurations"), ", ", code("basemean_configurations"), "), emits reactive triggers, and Integration applies them via Core’s typed setters. Assign Rules does not apply regex frames."),
      tags$li(strong("Core:"), " applies UI-configs via ", code("set_*_ui_config_from_import"), "; metadata stays in ", code("core_values$handson_metadata"), " and the freshest copy is mirrored to ", code("rv$data_def"), "."),
      tags$li(strong("Tables manual edits:"), " update local ", code("current_handson_metadata"), " immediately. When ", code("input$pause_metadata_sync"), " is inactive, edits still write through the injected ", code("set_metadata()"), " callback. In pause mode, edits set a pending flag and skip canonical write-back until ", code("sync_metadata_now"), " is clicked or automatic sync is re-enabled. The final write still goes through ", code("set_metadata()"), " / ", code("primary_data_state$set_metadata_for_current_data()"), ", so ", code("core_values$handson_metadata"), ", the metadata registry, revisions/signatures, and ", code("rv$data_def"), " remain synchronized through the existing path.")
    ),

    h3("Transformation pipeline (ordered by dependency)"),
    p("Integration wires each submodule with injected data/metadata reactives, adapter-backed update callbacks, a per-module safe UI system, session-state hooks, and the debug level. Structural changes refresh metadata and may reset filters to the original/raw baseline."),
    tags$ol(
      tags$li(
        strong("Batch Effects"), " — numeric corrections; no metadata required upfront; updates metadata after structure changes."
      ),
      tags$li(
        strong("Pivot"), " — reshape (Wider, Longer) or Transpose; updates metadata to reflect the new column set via ",
        code("update_metadata_for_pivoted_data"), ". ",
        "Transpose: removes the ", code("\"Row Index\""), " column before calling ",
        code("base::t()"), ", promotes former column names to the new Row Index, and promotes former Row Index values to new column headers. ",
        "Mixed-type coercion (character promotion) is detected and flagged on the result via the ",
        code("mixed_type_coercion"), " attribute."
      ),
      tags$li(
        strong("Merge"), " — combine Primary and Additional; updates metadata."
      ),
      tags$li(
        strong("Filtering"), " — requires metadata; sets ", code("core_values$filter_applied"), " and ", code("core_values$filtered_data"), "."
      ),
      tags$li(
        strong("Edit"), " — requires metadata; tabular operations with audit in ", code("core_values$modification_history"), "."
      ),
      tags$li(
        strong("Imputation"), " — requires metadata; fills missing values as configured."
      ),
      tags$li(
        strong("Ratios & Statistics"), " — requires metadata; adds ratio/stat columns."
      ),
      tags$li(
        strong("Basemean"), " — requires metadata; computes basemean on selected samples/abundance types."
      )
    ),

    h3("Reactive dependencies, observers & debounce strategy"),
    tags$ul(
      tags$li(code("core_values$primary_working_revision_debounced()"), " → expensive recalculation from the current Primary working data."),
      tags$li(code("core_values$metadata_revision_debounced()"), " → metadata-driven validation and UI refresh."),
      tags$li(code("core_values$metadata_content_signature_debounced()"), " → content/option-only refreshes such as Primary table color mapping; ignores metadata fields irrelevant to those consumers."),
      tags$li(code("core_values$secondary_revision_debounced()"), " → Secondary-data-dependent modules."),
      tags$li(code("rv$data_def / core_values$handson_metadata"), " → Filtering, Edit, Imputation, Ratios, Basemean."),
      tags$li("Condition options from Assign Rules → metadata dropdowns and dependent configuration UIs; text input synchronization is debounced before condition dropdown sources are refreshed."),
      tags$li("During restore, guard observers so replayed inputs do not regenerate metadata or re-apply rules destructively.")
    ),

    h3("Excel export and session save/restore sources"),
    tags$ul(
      tags$li(strong("Excel export:"), " resolves semantic export data from registry/core state: final data when apply is triggered, filtered data when filters are active, otherwise working/raw data. Module configuration sheets come from module export helpers rather than module internals."),
      tags$li(strong("Session save:"), " combines loader session state, aggregated submodule UI states, core data snapshots, and compatibility mirrors exposed by the Datawizard return object."),
      tags$li(strong("Session restore order:"), " restore loader state, rebuild current sheet caches, republish original/working registry entries, restore rules without overwriting metadata, reassert metadata/submodule UI snapshots, then release restore guards.")
    ),

    h3("Reset behavior, validation & recovery"),
    tags$ul(
      tags$li(strong("Primary reset:"), " resolves ", code("primary_original"), " first, falls back to ", code("rv$primary_data_original"), ", then republishes ", code("primary_raw"), ", ", code("primary_working"), ", core raw data, and rv mirrors."),
      tags$li(strong("Secondary reset:"), " resolves ", code("secondary_original"), ", falls back to ", code("rv$secondary_data_original"), ", then republishes ", code("secondary_working"), "."),
      tags$li(strong("Processing reset:"), " clears filtered/final roles, metadata/final core values, apply/filter flags, modification history, imputation/filter logs, rule/config state, and compatibility mirrors without mutating original snapshots."),
      tags$li(strong("Validation:"), " loaders validate shapes and headers; modules guard inputs via validators and typed setters; imports produce aggregated notifications (loaded/pending/failed)."),
      tags$li(strong("Provenance:"), " status flags (e.g., ", code("filter_applied"), "), registry revisions, ", code("last_import_info"), ", and ", code("components_exported"), " in .RDS/exports.")
    ),

    h3("Migration rules for future modules"),
    tags$ol(
      tags$li("Use ", code("resolve_datawizard_dataset()"), " or injected reactives for reads; do not infer current data from globals or rv chains."),
      tags$li("Use adapter-backed writes for Primary, Filtered, Final, Secondary, and Metadata changes."),
      tags$li("Do not write directly to ", code("rv$data_mod"), " or ", code("rv$data_def"), " except in central compatibility APIs."),
      tags$li("Keep module-local ", code("reactiveVal"), " fields in module state files and expose them through explicit return values."),
      tags$li("Add ", code("get_session_state"), "/", code("set_session_state"), " hooks for UI state that affects processing or export."),
      tags$li("Prefer debounced revision signals for heavy recalculation."),
      tags$li("When adding a dataset role, update the registry roles, resolver mapping, adapter revision-signal mapping, export/session code, and this technical documentation together."),
      tags$li("Preserve original snapshots; transformations create working/filtered/final revisions instead of overwriting originals.")
    ),

    h3("Memory & performance notes"),
    tags$ul(
      tags$li("Prefer module getters over storing large copies; write back via the provided update callback to keep a single flow of truth."),
      tags$li("Avoid eager full-table recomputation; most modules already do focused updates and rely on Integration for metadata refresh."),
      tags$li("Clear transient objects in long-running operations; keep exports on demand.")
    ),

    h3("Debugging checklist"),
    div(
      class = "alert alert-info",
      tags$ol(
        tags$li("Confirm getter precedence returns what you expect (", code("rv$data_mod ▷ filtered ▷ raw"), ")."),
        tags$li("Inspect ", code("core_values$handson_metadata"), " vs. ", code("rv$data_def"), " if a module ‘doesn’t see’ metadata."),
        tags$li("Check ", code("core_values$filter_applied"), " and ", code("core_values$filtered_data"), " when previews look off."),
        tags$li("Review ", code("modification_history"), " and ", code("processing_history"), " for recent structural changes."),
        tags$li("For imports: review aggregated notifications and ", code("last_import_info"), "."),
        tags$li("Use ", code("debug_log"), " traces in Loader/Integration when timing/race conditions are suspected.")
      )
    )
  )
}

############
# Logging & Debugging

render_tech_logging_content <- function() {
  div(
    h2("Technical Documentation — Logging & Debugging"),
    hr(),

    h3("Logging Model (as implemented)"),
    p("Each module defines a local ", code("debug_log(message, level = 1)"), " function inside its server. ",
      "That function closes over a numeric ", code("debug_level"), " argument passed into the module server. ",
      "Log lines are printed to stdout (console) only when ", code("debug_level >= level"), ". ",
      "This keeps verbosity per-module and avoids global logging state."),

    h3("Where the debug level comes from"),
    tags$ul(
      tags$li("The parent/Integration calls each module server with a numeric ", code("debug_level"), "."),
      tags$li("Inside the module server, a small ", code("debug_log"), " closure is defined and uses that ", code("debug_level"), "."),
      tags$li("Example pattern (taken from your docs module): a timestamped line with a fixed module label is written via ", code("cat()"), ".")
    ),

    h3("Minimal in-module pattern"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'moduleServer(id, function(input, output, session) {
  # debug_level is provided by the caller (parent/integration)
  debug_log <- function(message, level = 1) {
    if (debug_level >= level) {
      ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      line <- sprintf("[%s] DATAWIZARD_<MODULE>: %s", ts, message)
      cat(line, "\n")
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) rec(line)
    }
  }

  debug_log("Module initialized", 1)
  debug_log(sprintf("dims=%d x %d", nrow(df), ncol(df)), 2)
})'
      )
    ),

    h3("Levels & usage"),
    tags$ul(
      tags$li(strong("Level 1 — Essential:"), " module init/shutdown; file load success/failure; start/end of import/export; ",
              "structure-changing actions (pivot, merge, batch, ratios, basemean); filter on/off; caught errors."),
      tags$li(strong("Level 2 — Diagnostic:"), " shapes (rows×cols), chosen methods/parameters, retry attempts (e.g., applying templates), ",
              "notes around potentially slow steps.")
    ),

    h3("Typical log points by component"),
    tags$ul(
      tags$li(strong("File Loader:"), " file name(s), selected Excel sheet (if applicable), header row, parsed dimensions."),
      tags$li(strong("Auto-Assign:"), " export start/end; which rule frames are present (table/condition/ratio); application of imported frames; ",
              "counts of matched/unmatched headers; optional application of ratio configurations or edit steps."),
      tags$li(strong("Assign Rules:"), " .RDS read status; which UI-config blocks/dedicated payloads were found; ",
              "a single aggregated summary of loaded/pending/failed items (shown to the user)."),
      tags$li(strong("Integration/Core:"), " filter state toggles; metadata refresh after structure changes; ",
              "calls to typed UI-config setters accepted/rejected; entries appended to modification history."),
      tags$li(strong("Export:"), " workbook build start/end; which sheets were included (Primary, Additional if any, Modified, Metadata, Export_Info).")
    ),

    h3("Practical checks"),
    div(
      class = "well",
      tags$ul(
        tags$li("At module start, log the effective debug level once: ",
                code('debug_log(sprintf("debug_level=%d", debug_level), 1)')),
        tags$li("If nothing prints, ensure the parent/Integration actually passes a non-NULL ", code("debug_level"), " and your messages use level ≤ that value."),
        tags$li("Prefer the local ", code("debug_log"), " over raw ", code("cat()/print()"), " so verbosity is respected."),
        tags$li("When output views look wrong, log your data/metadata source selection: ",
                code("rv$data_mod ▷ core_values$filtered_data ▷ core_values$primary_data_raw"), " and the value of ",
                code("core_values$filter_applied"), "."),
        tags$li("On import issues, print the aggregated summary emitted by Assign Rules and inspect the per-part outcomes.")
      )
    ),

    h3("Concrete snippets"),
    pre(
      style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
      '# Loader
debug_log(sprintf("Loading: %s (sheet=%s, header_row=%d)", file, sheet, header_row), 1)
debug_log(sprintf("Parsed dims: %d×%d", nrow(df), ncol(df)), 2)

# Auto-Assign apply
debug_log("Applying imported rule frames: table/condition/ratio", 1)
debug_log(sprintf("ratio_configurations present: %s", !is.null(rules$ratio_configurations)), 2)

# Assign Rules import summary (already aggregated for the user)
debug_log(sprintf("Import summary — loaded=%d pending=%d failed=%d", n_loaded, n_pending, n_failed), 1)

# Integration after structure change
debug_log("Structure changed → refreshing metadata; resetting filters if necessary", 1)

# Error handling example
tryCatch({
  apply_filter_template(tpl, max_retries = 2)
}, error = function(e) {
  debug_log(sprintf("apply_filter_template error: %s", e$message), 1)
})'
    ),

    h3("Best practices (grounded in current code)"),
    div(
      class = "alert alert-info",
      tags$ul(
        tags$li("Keep logging module-scoped; pass ", code("debug_level"), " from the parent and close over it."),
        tags$li("Use concise, grep-friendly single-line messages with context (module tag, action, shape, key options)."),
        tags$li("Reserve level 1 for lifecycle/errors/structural changes; put dimensions/parameters/timings at level 2."),
        tags$li("Avoid inventing global loggers or file sinks unless you explicitly add them; current implementation logs to console.")
      )
    ),

    h3("Shared identifier projections"),
    tags$p(
      "The Data Wizard now publishes compact identifier projections derived from metadata rows whose ",
      code("Content"),
      " equals ",
      code("Identifier"),
      ". ",
      code("core_values$identifier_choices"),
      " / ",
      code("rv$datawizard_identifier_choices"),
      " expose display labels from ",
      code("Options"),
      " mapped to data columns from ",
      code("Column"),
      ", while ",
      code("rv$datawizard_identifier_option_choices"),
      " exposes option-label choices for legacy modules that expect ",
      code("Options"),
      " values. Modules should prefer these projections for identifier dropdown updates instead of rescanning the full metadata table."
    ),

    h3("Troubleshooting"),
    tags$dl(
      tags$dt("No logs appear"),
      tags$dd("Confirm the module is called with a non-NULL ", code("debug_level"), " and that your messages use a level ≤ it."),

      tags$dt("Too verbose"),
      tags$dd("Lower the ", code("debug_level"), " at the call site (parent/Integration) or raise the message level to 2."),

      tags$dt("Logs don’t reflect the right state"),
      tags$dd("Log which data/metadata source you read (", code("rv$data_mod/filtered/raw"), ", ", code("rv$data_def/handson_metadata"),
              ") and check ", code("core_values$filter_applied"), ".")
    )
  )
}
