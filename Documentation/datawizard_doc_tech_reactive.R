# ./Documentation/datawizard_doc_tech_reactive.R
# Datawizard Documentation — Reactive Systems Technical Documentation
#
# Contains content rendering functions for reactive-heavy subsystems:
#   - Auto-Assign System (regex rule frames, export/apply)
#   - Assign Rules System (import, UI-config routing, conditions)
#   - Import/Export System (RDS bundle structure and lifecycle)
#
# These functions are called by the documentation server in datawizard_doc_ui.R.

############
# Auto-Assign System

render_tech_auto_assign_content <- function() {
  div(
    h2("Technical Documentation — Auto-Assign System"),
    hr(),

    h3("Purpose & Scope"),
    p("Auto-Assign generates metadata for columns by applying rule frames to column names.
       It is the owner of regex rule frames and the export bundle, and it is the central apply-path when a template (.RDS) is imported.
       Auto-Assign updates reactive rule stores and synchronizes UI previews; it does not perform the Assign Rules UI for manual condition grouping."),

    h3("Reactive implementation loading contract"),
    p(
      "Core, File Loader, and Tables retain their historical compatibility-loader paths while focused sub-files own implementation details. Source order is explicit rather than incidental: prerequisites load before dependent registrars, and production code sources the compatibility entry point rather than assembling a private subset."
    ),
    tags$ul(
      tags$li("modules/Data Wizard/datawizard_file_loader.R is the compatibility/orchestration entrypoint; modules/Data Wizard/file_loader/ owns UI, shared context, observer families, restore, and diagnostics, and continues to own reading and canonicalization primitives."),
      tags$li("Each File Loader module session constructs one loader context; all loader observer families close over that context rather than allocating competing state."),
      tags$li("Each Tables module session constructs one Tables observer context. Metadata hydration registers before general rendering and mutation, and metadata editing registers last against that same context."),
      tags$li("Implementation files own their focused functions, but canonical datasets, metadata, and restore authority remain with the existing registry/Core adapters and injected setters."),
      tags$li("Direct-source compatibility is retained for structural tests and isolated maintenance, subject to the same prerequisite order used by the loader."),
      tags$li("No public module signature, returned reactive/API name, state key, template format, or session/restore contract changes as a result of the extraction.")
    ),

    h3("Clear separation: Auto-Assign vs Assign Rules"),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(tags$th("Concern"), tags$th("Auto-Assign"), tags$th("Assign Rules"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Ownership of regex rule frames"),
          tags$td("Owns and applies ", code("table"), ", ", code("condition"), ", ", code("ratio"), " frames"),
          tags$td("Does not own regex frames; does not apply them directly")
        ),
        tags$tr(
          tags$td("Export (.RDS)"),
          tags$td("Implements export via ", code("output$export_rules_autoassign_dw"), " → ", code("saveRDS")),
          tags$td("No export; loads files chosen by the user (", code("readRDS"), ") and emits configs")
        ),
        tags$tr(
          tags$td("Import application"),
          tags$td("Applies loaded bundle via ", code("load_rules_directly(rules_data)")),
          tags$td("Parses bundle; extracts per-module configs; Integration forwards to Core setters")
        ),
        tags$tr(
          tags$td("UI responsibility"),
          tags$td("Configuration UI for rule frames, live preview table; export options"),
          tags$td("UI for selecting/announcing templates; condition options list; notifications")
        ),
        tags$tr(
          tags$td("Notifications / aggregation"),
          tags$td("Debug logging and small toasts (no aggregation)"),
          tags$td("Aggregated import notifications via ", code("emit_aggregated_rule_notification"))
        )
      )
    ),

    h3("Auto-Assign internal structure and rule identity"),
    p(
      code("datawizard_auto_assign.R"),
      " is now the public orchestrator rather than the owner of every implementation detail. ",
      "It creates the shared Auto-Assign context and delegates UI composition, reactive state, rule execution, observers, external-module adapters, template loading, and output rendering to focused sub-files."
    ),
    tags$ul(
      tags$li(
        code("auto assign/datawizard_auto_assign_UI.R"),
        " — declarative modal and rule-editor UI."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_reactive_state.R"),
        " — authoritative reactive rule stores, selections, envelope/provenance state, template status, and reset behavior."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_rule_engine.R"),
        " — authoritative Content, condition, ratio, and aggregate metadata application."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_handlers_content.R"),
        " — Content rule handler registration."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_handlers_conditions.R"),
        " — Condition rule handler registration."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_handlers_ratios.R"),
        " — Ratio rule handler registration."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_handlers_export.R"),
        " — Export handler registration."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_handlers.R"),
        " — rule editing/removal observers and input-driven state mutation."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_integration_adapters.R"),
        " — adapters to other Data Wizard submodules and configuration collectors/appliers."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_template_pipeline.R"),
        " — canonical rule migration, validation, transactional loading, envelope construction, and rollback."
      ),
      tags$li(
        code("auto assign/datawizard_auto_assign_outputs.R"),
        " — Rule ID displays and rule-table/status renderers."
      )
    ),
    p(
      strong("Identity contract. "),
      code("RuleId"),
      " identifies one editable rule. ",
      code("VariantId"),
      " identifies the Content application variant shared by its dependent condition/ratio rules. ",
      "Content rules additionally carry ",
      code("Priority"),
      ". Several rules may therefore share the same Content label without sharing rule identity."
    ),

    h3("Auto RegEx inference subsystem"),
    p("Auto RegEx is a candidate-rule inference subsystem inside Auto-Assign; it is not a second rule owner. Auto RegEx infers candidate rule frames. Auto-Assign owns the authoritative ", code("table"), ", ", code("condition"), ", and ", code("ratio"), " frames, their editing UI, application to metadata, and export. Assign Rules owns template intake and condition-option management."),

    h4("Layers and public entry points"),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(tags$tr(tags$th("File"), tags$th("Responsibility"), tags$th("Boundary"))),
      tags$tbody(
        tags$tr(tags$td(code("modules/Data Wizard/datawizard_auto_regex.R")), tags$td("Public composition root, dependency order, host-logging adapter, and dependency injection."), tags$td("No inference, private state writes, or independent logger.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_UI.R")), tags$td("Namespaced controls and diagnostic presentation."), tags$td("No observers or state mutation.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_reactive_state.R")), tags$td("Session-local state and run-transition invariants."), tags$td("No source I/O, inference, or authoritative writes.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_logic.R")), tags$td("Pure inference orchestration and result contract."), tags$td("No Shiny lifecycle or authoritative mutation.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_utils.R")), tags$td("Canonical schemas, validation, tokenization, candidate/rule primitives, replay, and semantic spans."), tags$td("No I/O, package installation, or application lifecycle.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_handlers.R")), tags$td("Compatibility loader and explicit coordinator for focused handler registrars; rendering is registered last."), tags$td("No direct output, observer, cleanup, inference, or state ownership.")),
        tags$tr(tags$td(code("auto regex/datawizard_auto_regex_handlers_render.R")), tags$td("Readiness/status and diagnostic rendering, metadata-template download, bounded tables, suspension policy, panel toggles, and singular session cleanup."), tags$td("No source/run/transfer events, inference, or state allocation."))
      )
    ),
    p("The only application-facing entry points are ", code("modAutoRegexUI(id)"), " and ", code("modAutoRegexServer(id, metadata, data, revision, transfer, rule_state, provenance)"), ". The injected readers freeze metadata, working data, and revision; ", code("transfer"), " adapts the public Auto-Assign loader, and ", code("rule_state"), " exposes the three public readers used for snapshot verification."),

    h4("Host wiring and source injection"),
    tags$ul(
      tags$li("Auto-Assign nests the UI as ", code('modAutoRegexUI(ns("auto_regex"))'), ". Integration initializes the server in ", code("datawizard_integration.R"), " with ID ", code('"auto_assign-auto_regex"'), ". Under the parent Data Wizard namespace these are equivalent: the hyphenated server ID represents ", code('ns("auto_assign")'), " followed by the child ", code('ns("auto_regex")'), ". Do not reconstruct nested namespaces in Auto RegEx; use ", code("session$ns"), "."),
      tags$li("For the current-metadata source, table-local current metadata takes precedence only when it aligns with the resolved primary working dataset; canonical current metadata is the fallback. Working data and metadata must describe the same columns, and a merely aligned but empty/skeletal metadata frame is not meaningful inference evidence."),
      tags$li("Excel mode owns the uploaded workbook descriptor, worksheet selection/read, and mapping of ", code("Column"), ", ", code("Content"), ", ", code("Options"), ", ", code("Transformation"), ", ", code("Numerator"), ", and ", code("Denominator"), ". Switching source, workbook, worksheet, or mapping invalidates source-specific results. Missing ", code("readxl"), " is reported; packages are never installed at runtime.")
    ),

    h4("Run data flow"),
    pre(style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;", "source snapshot
  → validation
  → pass-one content inference
  → condition inference
  → ratio inference
  → semantic-span analysis
  → targeted content refinement
  → Identifier fallback
  → downstream replay
  → canonical validation
  → transactional Auto-Assign transfer"),

    h4("Inference semantics"),
    p(
      strong("Content inference. "),
      "For each Content label, its annotated headers are positives and other headers are negatives. ",
      "Candidate selectors are scored against the complete metadata frame and must preserve canonical persisted replay. ",
      "For a Content represented by exactly one row, the explicit singleton fallback can begin from the exact whole-header selector; later compaction may shorten it only when complete-table Content/VariantId/Transformation/conflict state remains identical."
    ),
    p(
      strong("Structural-family recovery. "),
      "One Content label may be partitioned into several ",
      code("VariantId"),
      " values when the source rows form independently supported header structures that cannot safely share one downstream rule scope. ",
      "Recovery proves each family against the complete metadata frame and reruns applicable condition/ratio inference inside that isolated family. ",
      "Canonical ratio-role siblings may provide structural evidence for otherwise singleton ratio families, but evidence rows do not broaden the selector's target rows."
    ),
    p(
      strong("Ratio representability. "),
      "Complete Numerator/Denominator annotations are not automatically extraction obligations. ",
      "A row is ",
      code("required"),
      " only when both references are represented as distinct header spans. Complete annotations not encoded by the header are ",
      code("nonrepresentable"),
      " and must remain blank under an extraction rule. Partial references remain unsafe. ",
      "A candidate ratio rule is rejected if it starts extracting arbitrary values from a nonrepresentable sibling sharing its current Content/VariantId."
    ),
    p(
      strong("Semantic-span refinement. "),
      "Condition, numerator, and denominator locations are recorded after downstream extraction. ",
      code("ExtractionConfirmed"),
      " means a span is known biological/experimental information and therefore contributes negative evidence during Content minimization. ",
      code("SafeToGeneralize"),
      " is narrower: it additionally permits replacing the literal by a validated structural atom. ",
      "Token-aligned semantic values can use shape-derived atoms such as ",
      code("[[:alpha:]]+"),
      ", ",
      code("[[:digit:]]+"),
      ", or combinations preserving punctuation. ",
      "Prospective shape-compatible substitutions and complete downstream replay must succeed before a generalized Content rule is retained."
    ),
    p(
      strong("Content compaction and redundancy. "),
      "The compactor decomposes the pre-compaction selected/generalized Include regex into top-level edge atoms and explores contiguous subranges. ",
      "Every state must reproduce the complete baseline Content/VariantId/Transformation/conflict assignment. ",
      "Redundancy 0 selects the preferred minimal exact-replay state, prioritizing non-semantic and natural-edge representations before length. ",
      "Higher redundancy values walk to neighbouring states by restoring safe edge atoms that existed in that pre-compaction regex. ",
      "The ladder does not reconstruct source-header text that is absent from the pre-compaction regex. ",
      "Restoration is blocked when the added context is predominantly semantic; requested levels therefore may saturate at a lower effective level."
    ),
    p(
      strong("Cached redundancy rebuild. "),
      "The completed inference stores immutable per-RuleId redundancy ladders. ",
      "The global redundancy value supplies the default and per-rule overrides may replace it. ",
      "Changing these controls rebuilds only the Content representations from cache, then validates the rebuilt canonical payload; condition and ratio inference are not rerun."
    ),
    p(
      strong("Identifier fallback. "),
      "After ordinary inference and refinement, currently unassigned identifier-like columns may be covered by the centralized Identifier fallback at lower precedence. ",
      "The fallback is rejected if it changes an already reliable normal assignment."
    ),

    h4("Conditions, Samples, and ratios"),
    tags$ul(
      tags$li(
        strong("Ratios: "),
        "Supported extraction methods remain ",
        code("Regular Expressions"),
        ", ",
        code("Pattern Recognition"),
        ", and ",
        code("Position in String"),
        ". Auto RegEx learns only from rows that have a real extraction obligation and rejects rules that extract arbitrary values from nonrepresentable siblings. ",
        "Auto-Assign's post-application pairing layer then groups related Ratio / p-Value / adjusted-p-Value rows using authoritative contrast identity when available, otherwise complete literal-header stems, then conservative structural triplet matching. ",
        "A real Numerator/Denominator pair is propagated to blank siblings in a proven family but is never overwritten. If no real pair exists for the family, one deterministic shared surrogate pair is generated so downstream modules can keep the three result columns associated."
      )
      ),

    h4("Canonical rule contract"),
    p(
      "The three authoritative frames use stable rule and variant identity. ",
      "Field names and order are canonical after ",
      code("upgrade_rule_component()"),
      ":"
    ),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Component"),
          tags$th("Canonical fields")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(code("table")),
          tags$td(
            code(
              "RuleId, Content, VariantId, Priority, Include, Exclude, Transformation"
            )
          )
        ),
        tags$tr(
          tags$td(code("condition")),
          tags$td(
            code(
              "RuleId, Content, VariantId, Method, Before, After, Separators, Pos"
            )
          )
        ),
        tags$tr(
          tags$td(code("ratio")),
          tags$td(
            code(
              "RuleId, Content, VariantId, Method, Separators, Invert, NumBefore, NumAfter, DenBefore, DenAfter, NumPos, DenPos"
            )
          )
        )
      )
    ),
    tags$ul(
      tags$li(
        code("RuleId"),
        " is globally stable editor identity. User-created rules use deterministic non-colliding IDs such as ",
        code("user-content-r1"),
        ", ",
        code("user-condition-r1"),
        ", and ",
        code("user-ratio-r1"),
        "."
      ),
      tags$li(
        code("VariantId"),
        " scopes dependent condition/ratio rules to one Content variant. A child rule must have a matching Content + VariantId parent."
      ),
      tags$li(
        code("Priority"),
        " belongs to Content rules and preserves deterministic application order."
      ),
      tags$li(
        "Legacy frames are upgraded before validation; generated identities are persisted independently of regex text."
      )
    ),
    p(
      "Canonical validation also enforces supported methods, required values, regex validity, component-specific NA rules, foreign-key identity, and application replay. Candidate diagnostics never make a frame authoritative."
    ),

    h4("State, identity, progress, and logging"),
    tags$ul(
      tags$li("Lifecycle states are ", code("idle"), "/", code("ready"), " before work, ", code("running"), ", ", code("complete"), " for staged validated candidates, ", code("transferred"), " after verified commit, ", code("failed"), ", ", code("stale"), " after invalidation, and ", code("cleaned"), " after session cleanup. A source may be ready in the UI while the stored run status remains idle."),
      tags$li("A source descriptor records mode, workbook/worksheet/mapping where applicable, normalized metadata signature, working-column signature, and injected revisions. Compact signatures identify effective source values; they are equality guards, not cryptographic durable IDs. Monotonic run IDs identify attempts, while a separate monotonically increasing source-fingerprint generation invalidates captured runs. Before staging and before/after transfer, handlers require the run ID, generation, status, and frozen source signature to remain current, preventing stale commits."),
      tags$li("Pure logic reports only named phase-boundary events: preprocessing, content, condition, ratio, semantic refinement, and payload validation start/complete. The handler maps those callbacks to Shiny progress and adds Auto-Assign transfer boundaries. Keeping progress as an injected callback leaves core inference testable outside Shiny."),
      tags$li("All messages use the single ", code("AUTO REGEX"), " tag through central ", code(".miraprot_log_record"), " and the application's central ", code("DEBUG_LEVEL"), ". The run ID appears only in the start record and terminal summary. Level 2 trace data is bounded to representative values, row IDs, candidate counts, pruning/scoring summaries, and timings; it must not dump complete source frames or create an independent retained logger.")
    ),

    h4("Transactional transfer, diagnostics, and maintenance"),
    p("The handler validates the canonical candidate payload, snapshots all three public Auto-Assign frames through ", code("rule_state"), ", calls the public ", code("load_rules_directly(..., notify = FALSE)"), " adapter quietly, and verifies all three frames with exact ", code("identical()"), " equality. A loader error, false return, mismatch, or run invalidation triggers rollback through that same public quiet loader followed by exact verification; Auto RegEx never writes Auto-Assign's private reactives. The handler emits one aggregate terminal notification, rather than loader and per-component toasts."),
    p("Auto RegEx owns inference/run diagnostics and their presentation; Auto-Assign owns authoritative rule-table previews and application diagnostics. Diagnostic tables are bounded to 500 displayed rows and rendered lazily only after their panel is opened. Candidate families, context widths, combination frontiers, and refinement depth are bounded; tokenization is cached; ratio inference reuses content/condition prerequisites; semantic refinement is targeted rather than a complete second content pass."),
    tags$ul(
      tags$li("Never write private Auto-Assign state; transfer and rollback only through the public loader and verify public readers."),
      tags$li("Do not add a second exporter, independent logger, runtime package installation, or duplicate rule engine."),
      tags$li("Keep pure inference and rule utilities testable outside Shiny; handlers alone adapt Shiny lifecycle, I/O, progress, and notification effects."),
      tags$li("Any source identity, injected wiring, schema, method, precedence, or transfer-contract change requires tests and corresponding maintenance-documentation updates.")
    ),

    h3("Key files & entry points"),
    tags$ul(
      tags$li(
        strong("Module file: "), code("datawizard_auto_assign.R"),
        " — ", code("modAutoAssignUI()"), ", ", code("modAutoAssignServer()"),
        "; export handler ", code("output$export_rules_autoassign_dw"),
        "; central import apply ", code("load_rules_directly(rules_data)"), "."
      ),
      tags$li(
        strong("Utilities: "), code("auto assign/datawizard_auto_assign_utils.R"),
        " — core rule identity/state helpers and compatibility loading; ratio pairing, sample construction, module-safe regex/extraction/UI adapters, and ratio parsing live respectively in ",
        code("datawizard_auto_assign_ratio_pairing.R"), ", ", code("datawizard_auto_assign_sample_helpers.R"), ", ",
        code("datawizard_auto_assign_module_adapters.R"), ", and ", code("datawizard_auto_assign_ratio_parsing.R"), "."
      ),
      tags$li(
        strong("Integration touchpoints: "),
        "Integration observes Assign Rules outputs and calls Core setters; it also injects the aligned metadata/data source and public Auto-Assign transfer/readers into Auto RegEx. Auto-Assign remains focused on authoritative rule frames and export/apply."
      )
    ),

    h3("Auto-Assign rule stores, selection, and UI sync"),
    tags$ul(
      tags$li(
        code("rv_table_rules_autoassign_dw"),
        " owns the canonical Content frame: ",
        code("RuleId, Content, VariantId, Priority, Include, Exclude, Transformation"),
        "."
      ),
      tags$li(
        code("rv_condition_rules_autoassign_dw"),
        " owns condition extraction rules keyed by RuleId and linked to a Content VariantId."
      ),
      tags$li(
        code("rv_rules_autoassign_dw"),
        " owns ratio extraction rules keyed by RuleId and linked to a Content VariantId."
      ),
      tags$li(
        code("selected_content_rule"),
        ", ",
        code("selected_condition_rule"),
        ", and ",
        code("selected_ratio_rule"),
        " store the exact RuleId currently being edited. Content labels are intentionally not unique identities."
      ),
      tags$li(
        code("populate_content_rule_ui(rule_id)"),
        ", ",
        code("populate_condition_rule_ui(rule_id)"),
        ", and ",
        code("populate_ratio_rule_ui(rule_id)"),
        " populate the editors from one exact rule."
      ),
      tags$li(
        strong("Modify actions: "),
        "require an existing selected RuleId, replace that row in place, and preserve RuleId/VariantId."
      ),
      tags$li(
        strong("Add New Content: "),
        "creates a globally unique user RuleId and a fresh Content VariantId."
      ),
      tags$li(
        strong("Add New Condition / Ratio: "),
        "creates a globally unique user RuleId while retaining the selected parent Content VariantId."
      ),
      tags$li(
        strong("Remove: "),
        "deletes one exact RuleId. Both the selected-rule controls and row-level remove buttons operate on RuleId rather than Content label."
      ),
      tags$li(
        "The rule tables use single-row selection and display the complete canonical frame plus an exact-row remove control."
      )
    ),

    h3("Export pipeline (what gets written)"),
    tags$ol(
      tags$li("User triggers ", code("output$export_rules_autoassign_dw"), " in the UI."),
      tags$li("The handler reads the canonical ", code("table"), ", ", code("condition"),
              ", and ", code("ratio"), " frames from their rule reactives."),
      tags$li("Only those three frames are saved via ", code("saveRDS(rules_list, file)"), ".")
    ),
    p(
      "Auto-Assign rule-file export does not serialize Data Wizard UI or module configuration. ",
      "Application session save/restore remains a separate subsystem and continues to capture UI state."
    ),

    h3("Import application and transactional rule loading"),
    tags$ol(
      tags$li(
        "Assign Rules reads the selected RDS bundle and forwards the Auto-Assign payload through the public ",
        code("load_rules_directly(rules_data)"),
        " interface."
      ),
      tags$li(
        code("load_rules_directly()"),
        " requires the canonical ",
        code("table"),
        ", ",
        code("condition"),
        ", and ",
        code("ratio"),
        " components. Historical frames are first migrated through ",
        code("upgrade_rule_component()"),
        "."
      ),
      tags$li(
        "After migration, each component must have the exact canonical field set and order. Invalid component structure rejects the load before authoritative replacement."
      ),
      tags$li(
        "The loader snapshots all three rule stores plus loaded-template state, rule envelope, provenance, contrast mappings, selected Rule IDs, priorities, and required capabilities."
      ),
      tags$li(
        "A successful load replaces all three canonical frames, including valid zero-row frames. This prevents rules from an older template surviving when the new template intentionally contains no rules of that type."
      ),
      tags$li(
        "The loader builds the schema-v2 rule envelope containing canonical rules, provenance, contrast mappings, priorities, and required capabilities."
      ),
      tags$li(
        "Visible editor selections are initialized from stable Rule IDs rather than from Content labels alone."
      ),
      tags$li(
        "Optional template blocks such as filtering, edit operations, ratio configurations, and UI configuration are then applied through their existing adapters."
      ),
      tags$li(
        "If any authoritative loading/application step fails, the captured rule/template state is restored and exact rollback is verified. A failed rollback is itself treated as an error."
      ),
      tags$li(
        "Per-module UI configurations that belong to Core continue through the Assign Rules → Integration → typed Core setter path rather than becoming part of rule execution."
      )
    ),

    h3("Core logic used by Auto-Assign"),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'apply_auto_assign_rules <- function(metadata, table_rules, condition_rules) { ... }
apply_rule_autoassign_dw     <- function(column_names, include, exclude, transformation) { ... }
apply_condition_autoassign_dw<- function(column_names, method, before, after, separators, pos) { ... }
extract_sample_from_column_name <- function(column_name, before, after, separators, pos) { ... }
apply_ratio_rules_fixed      <- function(metadata, ratio_rules) { ... }
apply_filter_template        <- function(filter_template, max_retries = 2) { ... }'
      )
    ),
    p("Regex composition helpers (AND/OR, escaping) and column parsing for authoritative application live in ",
      code("datawizard_auto_assign_utils.R"), ". Auto RegEx candidate generation and pure replay primitives live in its private logic/utils layers and converge on the same canonical frame contract; they do not replace Auto-Assign's application engine."),

    h3("Regex evaluation & precedence"),
    tags$ul(
      tags$li("Rules are applied in deterministic order based on the configured table rules."),
      tags$li("Patterns can be combined with prebuilt helpers (", code("make_and_regex_autoassign_dw"), ", ", code("make_or_regex_autoassign_dw"), ")."),
      tags$li("Invalid patterns are skipped with debug logs; columns without a match remain unchanged and can be surfaced by the preview.")
    ),

    h3("Error handling & resilience"),
    tags$ul(
      tags$li("Defensive ", code("tryCatch"), " sections around export and apply steps; errors are logged via ", code("debug_log"), "."),
      tags$li("Short, bounded retries for UI-sensitive operations (e.g., applying filter templates)."),
      tags$li("Partial imports are supported — missing optional sections are ignored with messages; required frames are validated."),
      tags$li("Auto RegEx failures leave candidates non-authoritative; transfer failures restore the public three-frame snapshot through the same quiet public loader and report whether exact rollback verification succeeded.")
    ),

    h3("Performance notes"),
    tags$ul(
      tags$li("Vectorized matching for column classification and sample extraction."),
      tags$li("Safe UI updates (", code("safe_update_input"), ") prevent excessive redraws."),
      tags$li("Ratio application and edit operations use focused apply helpers to avoid full-table recomputation where possible."),
      tags$li("Auto RegEx bounds candidate search and diagnostic rows, caches tokenization, reuses prerequisite inference for ratios, and performs only targeted semantic content refinement.")
    ),

    h3("Developer guidelines"),
    tags$ul(
      tags$li("Keep exported rule files limited to the three canonical rule frames; do not add module-specific UI state."),
      tags$li("Keep legacy UI/config import support separate from the current rule-file export path."),
      tags$li("When evolving the regex schema, preserve clear migration and validation fallbacks for existing rule files.")
    ),

    h3("Examples"),
    div(
      class = "well",
      pre(
        style = "background-color:#f5f5f5; padding:10px; border-radius:6px; font-family:monospace;",
        '# Export from Auto-Assign
# (inside server) output$export_rules_autoassign_dw

# Apply a loaded rules bundle (called by Assign Rules after readRDS)
rules <- readRDS("AutoAssign/my_auto_assign_rules.rds")
ok <- load_rules_directly(rules)

# Extra configuration fields in older rule files remain accepted by the legacy import path.'
      )
    ),

    h3("Documentation assessment (what changed vs. earlier docs)"),
    tags$ul(
      tags$li("Auto-Assign is now clearly documented as owner of rule frames and the export handler; Assign Rules owns file selection, extraction and notifications."),
      tags$li("Removed non-existent API calls (e.g., generic ", code("update_auto_assign_regex"), "); replaced with actual ", code("load_rules_directly"), " and apply helpers."),
      tags$li("Current exports contain rule frames only; older files with UI/config payloads remain readable through the legacy import path."),
      tags$li("Function names reflect the codebase: ",
              code("apply_auto_assign_rules"), ", ", code("apply_rule_autoassign_dw"), ", ",
              code("apply_condition_autoassign_dw"), ", ",
              code("extract_sample_from_column_name"), ", ",
              code("apply_ratio_rules_fixed"), ", ",
              code("apply_filter_template"), ", ",
              code("collect_*"), ".")
    ),

    h3("Summary"),
    p("Auto-Assign owns authoritative regex rule frames, editing, export, and the central apply path for imported bundles. Auto RegEx infers and transactionally transfers validated candidate frames through Auto-Assign's public loader, without becoming an owner or exporter. Assign Rules handles template intake, condition options, config extraction, and import notifications; Integration supplies Auto RegEx sources and forwards Assign Rules configs to Core setters.")
  )
}

############
# Assign Rules System

render_tech_assign_rules_content <- function() {
  div(
    h2("Technical Documentation — Assign Rules System"),
    hr(),

    h3("Purpose & Scope"),
    p("The Assign Rules module provides:"),
    tags$ul(
      tags$li("A focused UI to define and maintain condition groups (\"conditions\")."),
      tags$li("A resilient import path for rule bundles (.RDS) produced by Auto-Assign export."),
      tags$li("Routing and application of per-module UI configurations to Core via typed setters."),
      tags$li("Status/notification orchestration and provenance logging for imports.")
    ),
    p("This module does not parse regex or generate ratio patterns — those are owned by Auto-Assign. It does, however, expose condition options downstream and apply imported UI/config payloads when present."),

    h3("Files & Key Functions"),
    tags$ul(
      tags$li(
        strong("Module file: "), code("datawizard_assign_rules.R"),
        " — server logic (", code("modAssignRulesServer()"), "), import controls, notifications, condition UI state."
      ),
      tags$li(
        strong("UI components: "), code("assign rules/datawizard_assign_rules_UI.R"),
        " — ", code("modAssignRulesUI()"), " (buttons, condition inputs, rule file picker, readiness gates)."
      ),
      tags$li(
        strong("Helpers: "), code("assign rules/datawizard_assign_rules_utils.R"),
        " — validators (", code("validate_ui_*"), "), extractors (", code("extract_ui_*"), "), and a generic ",
        code("extract_ui_config()"), " with retry logic and fallbacks."
      )
    ),

    h3("What lives where: UI_config vs. dedicated blocks"),
    p("Imports carry generic UI trees under ", code("UI_config"), " for several modules, but some payloads are module-specific and live outside that tree. The Assign Rules import path supports both:"),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(tags$th("Module"), tags$th("Primary source in .RDS"), tags$th("Importer / Setter"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Filtering"),
          tags$td(span(code("UI_config$filtering"), " or ", code("filter_template"))),
          tags$td(code("extract_ui_filtering_config()"), " → ", code("set_filtering_ui_config_from_import()"), " and optionally ", code("apply_filter_template(..., max_retries = 2)"))
        ),
        tags$tr(
          tags$td("Imputation"),
          tags$td(code("UI_config$UI_imputation"), " (legacy: ", code("imputation_defaults"), ")"),
          tags$td(code("extract_ui_imputation_config()"), " → ", code("set_imputation_ui_config_from_import()"))
        ),
        tags$tr(
          tags$td("Batch Effects"),
          tags$td(code("UI_config$batch_effects")),
          tags$td(code("extract_ui_batch_effects_config()"), " → ", code("set_batch_effects_ui_config_from_import()"))
        ),
        tags$tr(
          tags$td("Pivot"),
          tags$td(code("UI_config$pivot")),
          tags$td(code("extract_ui_pivot_config()"), " → ", code("set_pivot_ui_config_from_import()"))
        ),
        tags$tr(
          tags$td("Merge"),
          tags$td(code("UI_config$merge")),
          tags$td(code("extract_ui_merge_config()"), " → ", code("set_merge_ui_config_from_import()"))
        ),
        tags$tr(
          tags$td("Ratios"),
          tags$td(span(code("UI_config$ratios"), " or ", code("ratio_configurations"))),
          tags$td(code("extract_ui_ratios_config()"), " → ", code("set_ratios_ui_config_from_import()"), " (and module helpers such as ", code("apply_ratio_rules_fixed"), " when relevant)")
        ),
        tags$tr(
          tags$td("Basemean"),
          tags$td(code("basemean_configurations"), " (preferred)"),
          tags$td(code("extract_ui_basemean_config()"), " → ", code("set_basemean_ui_config_from_import()"))
        ),
        tags$tr(
          tags$td("Edit Operations"),
          tags$td(code("edit_operations")),
          tags$td(code("extract_ui_edit_config()"), " → ", code("set_edit_ui_config()"))
        )
      )
    ),
    p("Auto-Assign rule frames (", code("table"), ", ", code("condition"), ", ", code("ratio"), ") are not owned by Assign Rules and are applied within Auto-Assign."),

    h3("User Interface & Reactive State"),
    tags$ul(
      tags$li("Add/Remove condition groups buttons feed a reactive list ", code("condition_inputs"), "."),
      tags$li("A derived reactive ", code("Options_condition"), " mirrors current condition names and is used by downstream modules for choices."),
      tags$li("Readiness gate ", code("output$assign_ready"), " prevents rendering when metadata is not available."),
      tags$li("Rule file picker scans the Auto-Assign directory and updates ", code("input$load_rule_set_dw"), " with available exports.")
    ),

    h3("Import flow (Assign Rules)"),
    tags$ol(
      tags$li("User selects an exported template (.RDS)."),
      tags$li("Module loads via ", code("readRDS(path)"), " with ", code("tryCatch"), " and error notifications."),
      tags$li("For each supported component, an extractor attempts the primary path (e.g., ", code("UI_config$filtering"),
              ") and falls back to legacy locations (e.g., ", code("filter_template"), ")."),
      tags$li("Validated configs are applied through Core’s typed setters (e.g., ", code("set_filtering_ui_config_from_import()"),
              "), with short retries where the module requires metadata readiness."),
      tags$li("Optional templates (", code("filter_template"), ") are applied with bounded retries: ", code("apply_filter_template(..., max_retries = 2)"), "."),
      tags$li("Module-specific payloads (", code("ratio_configurations"), ", ", code("basemean_configurations"), ", ", code("edit_operations"),
              ") are routed to their corresponding bridges/setters."),
      tags$li("A consolidated notification summarizes loaded/pending/failed parts and logs are appended to ", code("processing_history"), " / ", code("last_import_info"), ".")
    ),

    h3("Status, Notifications, Provenance"),
    tags$ul(
      tags$li("State flags: ", code("rule_loading_status"), ", ", code("ui_config_application_status"), ", ", code("template_loading_in_progress"), "."),
      tags$li("Aggregated notifications: ", code("emit_aggregated_rule_notification()"), " groups events (loaded/pending/failed/missing)."),
      tags$li("Error & warning capture: ", code("ui_config_errors"), ", ", code("processing_errors"), ", ", code("processing_warnings"), "."),
      tags$li("Provenance and support details: ", code("processing_history"), ", ", code("last_import_info"), " (timestamp, method, status, affected parts).")
    ),

    h3("Validation & Fallbacks"),
    tags$ul(
      tags$li("Each extractor uses ", code("extract_ui_config()"), " with a validator (e.g., ", code("validate_ui_filtering_config()"), ")."),
      tags$li("Legacy paths are supported where applicable (e.g., ", code("imputation_defaults"), " → converted into current structure)."),
      tags$li("Partial imports are fine — missing sections are logged and skipped."),
      tags$li("Module/UI updates are guarded and retried briefly when metadata isn’t ready.")
    ),

    h3("Developer Guidelines"),
    tags$ul(
      tags$li("When adding a new module: implement a validator, an extractor (primary path under ", code("UI_config"),
              " if appropriate), and a typed Core setter to apply it."),
      tags$li("Prefer Core setters (", code("set_*_ui_config_from_import()"), ") over direct input mutation; they centralize validation, error tracking, and source tagging."),
      tags$li("Keep module-specific payloads outside ", code("UI_config"), " where this improves clarity (e.g., ", code("basemean_configurations"), ", ", code("ratio_configurations"), ")."),
      tags$li("Emit concise, aggregated notifications instead of multiple toasts during imports.")
    ),

    h3("Examples"),
    div(
      class = "well",
      pre(
        style = "background-color:#f5f5f5; padding:10px; border-radius:6px; font-family:monospace;",
        '# Load a rules bundle chosen by the user
rules <- readRDS("AutoAssign/my_auto_assign_rules.rds")

# Extract with primary-then-legacy fallbacks
cfg_filtering  <- extract_ui_filtering_config(rules, debug_log, ui_config_errors)
cfg_imputation <- extract_ui_imputation_config(rules, debug_log, ui_config_errors)
cfg_ratios     <- extract_ui_ratios_config(rules, debug_log, ui_config_errors)
cfg_basemean   <- extract_ui_basemean_config(rules, debug_log, ui_config_errors)

# Apply via Core setters and module helpers
if (!is.null(cfg_filtering))  set_filtering_ui_config_from_import(cfg_filtering)
if (!is.null(cfg_imputation)) set_imputation_ui_config_from_import(cfg_imputation)
if (!is.null(cfg_ratios))     set_ratios_ui_config_from_import(cfg_ratios)
if (!is.null(cfg_basemean))   set_basemean_ui_config_from_import(cfg_basemean)

# Optional filtering template with bounded retries
if (!is.null(rules$filter_template)) apply_filter_template(rules$filter_template, max_retries = 2)'
      )
    ),

    h3("Documentation Assessment"),
    tags$ul(
      tags$li("Export ownership clarified: Assign Rules imports and applies; Auto-Assign handles exporting rule bundles."),
      tags$li("UI_config coverage corrected: not all modules serialize inside ", code("UI_config"),
              " — Ratios and Basemean primarily use ", code("ratio_configurations"), " / ", code("basemean_configurations"), "."),
      tags$li("Replicate numbering and JSON parsing claims removed (not present in this module’s code)."),
      tags$li("Function names aligned with code: ", code("extract_ui_*"), ", ", code("validate_ui_*"),
              ", ", code("set_*_ui_config_from_import"), ", ", code("apply_filter_template"), ", ", code("emit_aggregated_rule_notification"), ".")
    ),

    h3("Summary"),
    p("Assign Rules is the intake and application point for Auto-Assign exports. It manages condition group UI, validates and applies per-module configurations with Core setters, supports legacy paths, and communicates status through aggregated notifications. Ratios and Basemean rely on their dedicated top-level blocks rather than the generic ", code("UI_config"), ".")
  )
}


############
# Import/Export System (RDS)

render_tech_import_export_content <- function() {
  div(
    h2("Technical Documentation — Import/Export System (RDS)"),
    hr(),

    h3("Overview"),
    p("The Import/Export system bundles rule frames, optional UI state, and per-module configurations into a single .RDS file to enable reproducibility and transfer between sessions or machines."),
    p("Export is initiated in the Auto-Assign module; import is triggered from Assign Rules and then delegated back to Auto-Assign for application. Core exposes typed setters to apply UI configurations, and Integration coordinates cross-module updates and safe UI refresh."),

    h3("Files & Ownership"),
    tags$ul(
      tags$li(
        strong("datawizard_auto_assign.R: "),
        "Owns export (Shiny download handler) and the central import apply path (",
        code("load_rules_directly"), "). Collects optional payloads and applies rules/UI/configs."
      ),
      tags$li(
        strong("datawizard_assign_rules.R: "),
        "Loads an .RDS file (", code("readRDS"), ") and triggers application while handling user notifications and status."
      ),
      tags$li(
        strong("datawizard_core.R: "),
        "Holds central reactive stores and typed UI setters: ",
        code("set_filtering_ui_config_from_import"), ", ",
        code("set_imputation_ui_config_from_import"), ", ",
        code("set_batch_effects_ui_config_from_import"), ", ",
        code("set_pivot_ui_config_from_import"), ", ",
        code("set_merge_ui_config_from_import"), ", ",
        code("set_ratios_ui_config_from_import"), ", ",
        code("set_basemean_ui_config_from_import"), "."
      ),
      tags$li(
        strong("datawizard_integration.R: "),
        "Coordinates module interplay (metadata sync, safe UI guard) and performs post-load refreshes where required."
      )
    ),

    h3("What lives where: UI_config vs dedicated module payloads"),
    p("Not every module serializes into the generic ", code("UI_config"), " tree. Some modules use dedicated top-level blocks. The system supports both patterns; the current export path uses:"),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Module"),
          tags$th("Primary export location"),
          tags$th("Typical import application")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Filtering"),
          tags$td(code("UI_config$filtering"), " and ", code("filter_template")),
          tags$td(code("set_filtering_ui_config_from_import"), " + ", code("apply_filter_template"))
        ),
        tags$tr(
          tags$td("Imputation"),
          tags$td(code("UI_config$UI_imputation")),
          tags$td(code("apply_imputation_ui_config"), " (module setter first; Core fallback)")
        ),
        tags$tr(
          tags$td("Batch Effects"),
          tags$td(code("UI_config$batch_effects")),
          tags$td("Applied via Core setter (module-aware helpers if present)")
        ),
        tags$tr(
          tags$td("Pivot"),
          tags$td(code("UI_config$pivot")),
          tags$td(code("apply_pivot_ui_config"), " / Core setter and Integration sync")
        ),
        tags$tr(
          tags$td("Merge"),
          tags$td(code("UI_config$merge")),
          tags$td("Core setter + consistency checks")
        ),
        tags$tr(
          tags$td("Ratios"),
          tags$td(code("ratio"), " (regex rules) and ", code("ratio_configurations")),
          tags$td(code("apply_ratio_rules_fixed"), " and ", code("apply_ratio_configurations"))
        ),
        tags$tr(
          tags$td("Basemean"),
          tags$td(code("basemean_configurations")),
          tags$td("Applied through module/Core helpers; not serialized under ", code("UI_config"), " in the current export path")
        ),
        tags$tr(
          tags$td("Edit Operations"),
          tags$td(code("edit_operations")),
          tags$td(code("apply_edit_operations"))
        )
      )
    ),

    h3("RDS bundle structure"),
    p("New Auto-Assign exports contain only the three required rule frames."),
    tags$ul(
      tags$li(code("table"), ": data.frame — Content Assignment rules."),
      tags$li(code("condition"), ": data.frame — Condition Extraction rules."),
      tags$li(code("ratio"), ": data.frame — Ratio Analysis rules.")
    ),
    p("The importer still recognizes historical bundles containing optional UI/configuration fields, but new exports do not write those fields."),
    div(
      class = "well",
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'list(
  table     = <Content Assignment rule data frame>,
  condition = <Condition Extraction rule data frame>,
  ratio     = <Ratio Analysis rule data frame>
)'
      )
    ),

    h3("Export flow"),
    tags$ol(
      tags$li("User triggers export in Auto-Assign (", code("output$export_rules_autoassign_dw"), ")."),
      tags$li("Export collects required rule frames (", code("table"), ", ", code("condition"), ", ", code("ratio"), ")."),
      tags$li("No Data Wizard UI or module configuration is added to the rule file."),
      tags$li("The three-frame rule list is saved via ", code("saveRDS(rules_list, file)"), ".")
    ),

    h3("Import flow"),
    tags$ol(
      tags$li("Assign Rules loads the file via ", code("rules <- readRDS(path)"), " with error handling and status notifications."),
      tags$li("Auto-Assign applies the bundle using ", code("load_rules_directly(rules)"), "."),
      tags$li("Rule frames are committed to reactives: ", code("rv_table_rules_autoassign_dw"), ", ", code("rv_condition_rules_autoassign_dw"), ", ", code("rv_rules_autoassign_dw"), ". ",
              code("load_rules_directly"), " also calls ", code("updateSelectInput"), " for each tab's dropdown; however these calls are no-ops when the modal is closed. ",
              "Actual field population is handled by the modal-open observer (", code("observeEvent(input$open_auto_assign_modal)"), "), which calls the ",
              code("populate_*_rule_ui"), " helpers for the first loaded rule in each tab when the modal is shown."),
      tags$li("If present, ", code("filter_template"), " is applied with short retries: ", code("apply_filter_template(..., max_retries = 2)"), "."),
      tags$li("If present, ", code("UI_config"), " components are routed through Core setters (Filtering, Imputation, Batch Effects, Pivot, Merge)."),
      tags$li("Module-specific payloads are applied through dedicated helpers: ",
              code("apply_ratio_rules_fixed"), ", ",
              code("apply_ratio_configurations"), ", ",
              code("apply_edit_operations"),
              "; Basemean uses ", code("basemean_configurations"), " and is applied via its module/Core bridge."),
      tags$li("Integration performs follow-up synchronization (metadata refresh, safe UI guard, targeted refresh).")
    ),

    h3("Error handling & status"),
    tags$ul(
      tags$li("Defensive ", code("tryCatch"), " around all major steps with user notifications and a running processing log."),
      tags$li("State flags and provenance: ", code("template_loading_in_progress"), ", ", code("rule_loading_status"), ", ",
              code("template_export_status"), ", ", code("processing_history"), ", ", code("last_import_info"), "."),
      tags$li("Partial imports are supported — missing sections are logged and skipped without aborting the whole load."),
      tags$li("Version hints via ", code("debug_info$module_versions"), "; compatibility is handled through robust setters and fallbacks.")
    ),

    h3("Developer notes"),
    tags$ul(
      tags$li("Prefer typed Core setters (", code("set_*_ui_config_from_import"), ") and module-level apply helpers over raw input updates."),
      tags$li("Apply in a stable order: commit rules → sync visible UI → apply templates/configs → run Integration sync."),
      tags$li("Respect module-specific serialization: Ratios and Basemean primarily travel via ", code("ratio_configurations"), " and ", code("basemean_configurations"), " rather than the generic ", code("UI_config"), "."),
      tags$li("Keep export options aligned with the actual payloads; ensure ", code("components_exported"), " reflects what was included."),
      tags$li("Modules that depend on reactive metadata (e.g., Basemean) must delay UI import application until sample lists are built; use flags such as ", code("applying_import_config"), " and ", code("samples_ready"), " to coordinate update order.")
    ),

    h3("Examples"),
    div(
      class = "well",
      pre(
        style = "background-color:#f5f5f5; padding:10px; border-radius:6px; font-family:monospace;",
        '# Minimal rules-only import
rules <- readRDS("rules_only.rds")
load_rules_directly(rules)

# Full bundle import (rules + filtering template + UI_config + ratios/basemean/edit ops)
rules <- readRDS("project_state.rds")
ok <- load_rules_directly(rules)
if (isTRUE(ok)) message("Rules and configurations applied.")'
      )
    ),

    # h3("Short assessment of this documentation"),
    # tags$ul(
    #   tags$li("Adjusted to reflect module-specific export paths: Basemean and Ratios are documented as dedicated top-level blocks, not generic ", code("UI_config"), "."),
    #   tags$li("Function names match the code (e.g., ", code("collect_filter_ui_state"), " vs. a generic “create_*” placeholder)."),
    #   tags$li("Flows and status reporting mirror the implementation (export handler in Auto-Assign; import via ", code("readRDS"), " + ", code("load_rules_directly"), ")."),
    #   tags$li("RDS shape and debug metadata align with the actual bundle, including ", code("components_exported"), " and option flags.")
    # ),

    h3("Summary"),
    p("Use the rule frames for core semantics (table/condition/ratio), the generic ", code("UI_config"), " for modules that serialize UI trees (Filtering, Imputation, Batch Effects, Pivot, Merge), and the dedicated blocks for module-specific payloads such as ",
      code("ratio_configurations"), " and ", code("basemean_configurations"), ". The import path is resilient, modular, and designed for partial availability of payloads.")
  )
}
