# Data Wizard Auto RegEx architecture

This document is the deep architecture reference for the integrated Auto RegEx subsystem. Auto RegEx infers candidate rules inside Auto-Assign; it does not own a second authoritative rule store. Auto-Assign remains responsible for the `table`, `condition`, and `ratio` frames, editing, application, and export.

## Public boundary and integration

The application-facing entry points are `modAutoRegexUI(id)` and `modAutoRegexServer(id, metadata, data, revision, transfer, rule_state, provenance)`. The public composition root is `modules/Data Wizard/datawizard_auto_regex.R`; it loads the private compatibility/coordinator layer, adapts centralized host logging, and injects dependencies. The handler coordinator then sources its focused registrars and invokes the render/lifecycle registrar last.

The UI is nested by Auto-Assign as `modAutoRegexUI(ns("auto_regex"))`. The server is initialized by `modules/Data Wizard/datawizard_integration.R` with the equivalent composite ID `"auto_assign-auto_regex"`. Private server code derives its namespace from `session$ns` rather than reconstructing nested IDs.

Integration injects the resolved primary working data, current metadata, revision evidence, the public `load_rules_directly` adapter, and public readers for all three rule frames. For the current-metadata source, live table-local metadata takes precedence when it aligns with the resolved working dataset; canonical current metadata is the fallback. An aligned but empty or skeletal metadata frame is not meaningful inference evidence.

## Run flow and inference semantics

Each run freezes its effective source and proceeds through validation, content inference, condition inference, ratio inference, semantic-span analysis, targeted content refinement, Identifier fallback, downstream replay, canonical validation, and transactional transfer.

After the unchanged content, condition, and ratio passes complete, a per-Content
diagnostic records the applicable rows that failed each stage. It separates
ordinary rule failures from missing references, transformation conflicts,
contradictory labels for identical source text, invalid persisted regex/replay
instability, and exhausted search limits. A grouped fallback is eligible only
when independently discriminating safe subsets demonstrate multiple structural
families; every terminal or contradictory failure class vetoes grouping.

Content candidates record five distinct evidence families: `structural`
(shared token/delimiter organization), `shape` (shared identifier shapes),
`partial` (a shared part of the structure), `concrete_token` (one or more
literal tokens), and `whole_header_literal` (a complete source-name literal).
Zero cross-content false positives and exact persisted/downstream replay remain
mandatory, but are not evidence by themselves. For an automatically
transferable variant, grouped-inference release 1 additionally requires at
least two supporting source rows and two distinct source names. A complete
header literal never satisfies that threshold. Literal-only singleton variants
are retained only as non-authoritative diagnostic suggestions; they are not
written to the candidate rule table. Any future singleton-transfer mode must
label such a rule `low-evidence`, require stronger downstream replay, and show
that status prominently to the user.

Per-variant diagnostics report supporting source-row and distinct source-name
counts, candidate family, representative covered and uncovered examples,
cross-content false positives, downstream exact matches, and whether the rule
remains valid without memorizing one complete source header. They also expose
the threshold decision and authoritative/suggestion status so an exact replay
cannot be mistaken for adequate generalization evidence.

- **Semantic refinement:** verified, unique condition and ratio spans may be generalized in affected content rules. The refined result is replayed, and the first-pass rule is restored if refinement changes a previously reachable downstream assignment or extraction.
- **Identifier fallback:** if ordinary inference and refinement produce no Identifier rule, a case-insensitive rule over the centralized Identifier vocabulary may be added at lower precedence. It applies only to rows left unassigned by ordinary rules and is rejected if it changes a reliable normal assignment.
- **Conditions and Samples:** condition rules write `Options`; `Sample` is generated later by Auto-Assign's application path. Sample names combine the extracted condition with minimal source-derived structural identifiers and are made unique within each Content type. Collisions use the shortest distinguishing source substring, with a deterministic row suffix only for indistinguishable sources.
- **Ratios:** ratio inference reuses successful content and condition prerequisites and accepts candidates only when exact component replay matches the annotated numerator and denominator.

The pure logic layer accepts an injected progress callback and reports named phase boundaries. The handler translates these events into Shiny progress and adds transfer boundaries, keeping inference testable without Shiny.

## Source identity and lifecycle

A canonical descriptor captures the **effective** source: source mode; workbook, worksheet, and mapping where applicable; compact normalized-metadata and working-column signatures; and injected revision evidence. These bounded signatures are equality guards for the values that can affect a run, not cryptographic or durable identifiers. Full serialized source values are not retained as hexadecimal fingerprints.

Monotonic run IDs distinguish attempts. A separate monotonically increasing source-fingerprint generation invalidates captured runs. Before staging and around transfer, the handler requires the run ID, generation, lifecycle status, and frozen effective-source signature to remain current, preventing stale results from becoming authoritative.

Lifecycle states cover idle/ready presentation, running work, completed candidates, verified transfer, failure, staleness, and cleanup. Source, upload, worksheet, or mapping changes invalidate source-specific results. The session-end callback invalidates work, clears uploaded workbook state and diagnostics, and marks state cleaned so late callbacks cannot commit; Shiny owns teardown of session-scoped observers and `withProgress` resources.

## Logging and transactional transfer

All subsystem messages use the `AUTO REGEX` tag through the central `.miraprot_log_record` recorder and application `DEBUG_LEVEL`. A console-only fallback is used if the recorder is unavailable; Auto RegEx does not retain a separate log. Detailed logging is bounded to representative evidence, counts, pruning/scoring summaries, and timings rather than complete source frames.

The handler validates and stages the canonical candidate frames, snapshots all three public Auto-Assign frames through `rule_state`, then calls the public `load_rules_directly(..., notify = FALSE)` transfer adapter. It verifies the three public readers with exact `identical()` comparisons. A loader error, false return, mismatch, or run invalidation triggers rollback through the same quiet public loader and another exact verification. Neither transfer nor rollback replaces private or public reactives directly. Candidate diagnostics remain non-authoritative.

## Responsibility matrix

| File | Exclusive responsibility | Explicit exclusions |
|---|---|---|
| `modules/Data Wizard/datawizard_auto_regex.R` | Public composition root, dependency order, centralized host-logging adapter, and dependency injection | No inference, handlers, independent logger, or rule writes |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_UI.R` | Namespaced controls and diagnostic presentation | No observers or state mutation |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R` | Compatibility loader and explicit registrar coordinator; invokes rendering last | No output, observer, cleanup, inference, or state ownership |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_render.R` | Readiness/status, download, validation/rule/diagnostic and redundancy rendering; bounded tables; suspension policy; panel toggles; singular session cleanup registration | No source/run/transfer events, inference, or state allocation |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_source.R`, `datawizard_auto_regex_handlers_redundancy.R`, `datawizard_auto_regex_handlers_run.R`, and `datawizard_auto_regex_handlers_transfer.R` | Focused source, redundancy, run, and transactional transfer event registration | No rendering or session cleanup registration |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R` | Pure inference orchestration, progress phase callbacks, and result contract | No Shiny lifecycle, I/O, or authoritative mutation |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_reactive_state.R` | Session-local state allocation and run-transition invariants | No source I/O, inference, or Auto-Assign writes |
| `modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R` | Canonical schemas, validation, tokenization, candidate and replay primitives, and semantic spans | No runtime installation, I/O, logger, or application lifecycle |

## Architectural constraints and limitations

- Packages are never installed at runtime. Excel input reports when host-provided `readxl` support is unavailable.
- Auto RegEx owns only its metadata-template download handler; it has no Session tab, application startup/shutdown behavior, private Auto-Assign access, or duplicated authoritative rule engine.
- Exact verification intentionally treats type and attribute differences as transfer failure.
- Diagnostics and inference search are bounded for predictable runtime; optional `stringr` and base PCRE validation may differ for engine-specific syntax.
- Changes to source identity, injected wiring, schema, precedence, lifecycle, or transfer contracts require corresponding tests and documentation maintenance.
- The normative next-revision design for stable rule/variant identity,
  explicit priority and transformation ownership, deterministic legacy
  migration, capability gating, and complete-payload rollback is
  [`grouped_rule_schema_design.md`](grouped_rule_schema_design.md). The current
  runtime must not emit that grouped format until all conformance gates in the
  design are implemented.

## Documentation ownership

- User workflow: `Documentation/datawizard_doc_user.R`, in the Auto-Assign user section.
- In-app developer guide: `Documentation/datawizard_doc_tech_reactive.R`, in the Auto-Assign technical section.
- Deep architecture reference: `Documentation/datawizard_auto_regex_architecture.md` (this document).
- Historical migration oracle: `Documentation/regex_metadata_assistant_migration_baseline.md`.
