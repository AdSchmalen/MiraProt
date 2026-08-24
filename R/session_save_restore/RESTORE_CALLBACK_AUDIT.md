# Restore callback audit

`restore_callback_audit_manifest.csv` is the checked-in, reviewable inventory of
imperative boundaries that can be reached while restoring a session.  It covers
the restore framework and the Grid, GO, GSEA, STRING, Heatmap, Abundances, and
Data Wizard module trees (including every Data Wizard submodule).

## Classification

* **Category 1** — imperative deferred replay (`session$onFlushed()` or
  `later::later()`), promise continuations, and custom timer/retry callbacks. Every
  restore-specific occurrence must declare exactly one approved contract: a direct
  call to `.run_session_restore_callback()`, a named module wrapper which delegates
  to that runner, or a captured-values-only implementation. Captured-values-only
  means that neither the callback body nor any helper it invokes reads `reactiveVal`,
  `reactiveValues`, `input`, or a reactive expression.
* **Category 2** — a reactive scheduler (`invalidateLater()`, `debounce()`, or
  `throttle()`). These boundaries are safe only as part of a reactive consumer;
  `invalidateLater()` outside one is never implicitly approved. Genuine `observe`,
  `observeEvent`, `reactive`, and `render*` consumers remain Category 2 rather than
  being mislabeled as imperative restore callbacks.
* **Category 3** — teardown/finalization (`cleanup_manager` registration,
  session cleanup, `on.exit()`, or a `tryCatch(..., finally=)` clause).

Each CSV row represents one reviewed source occurrence. `boundary_id` is stable
across ordinary line movement: it consists of file, mechanism, and occurrence
number. `reviewed_line` is navigational metadata, not its identity. The remaining
columns record owner, trigger, direct/transitive reactive-read contract,
isolation, catch semantics, generation/staleness guard, and failure behavior.
`restore_specific` identifies the Category 1 occurrences to which the strict restore
rule applies, `callback_contract` is their explicit linkage, and
`reactive_consumer` prevents a real reactive consumer from being conflated with an
imperative callback. Non-restore deferred UI work remains inventoried but is marked
`restore_specific=no` rather than being granted a restore contract.

## Enforcement

`test-restore-callback-audit.R` scans the complete scope above. A newly added raw
boundary must either be replaced by an approved helper call or receive an
explicitly reviewed manifest row. Calls routed through helpers do not need a row
at every call site; the helper's raw scheduling boundary does. The sole structural
exception is `invalidateLater()` whose parsed call is nested in a Shiny reactive
consumer (`reactive`, `observe`, `observeEvent`, or a `render*` call).

The audit intentionally inventories lexical finalizers as well as asynchronous
callbacks: restore failures frequently exercise cleanup paths, so omitting them
would leave restore-reachable failure behavior unaudited.


## Readiness and error classification

Restore readiness is not allowed to use `tryCatch(x(), error = function(e) NULL)`,
`tryCatch(x(), error = function(e) FALSE)`, or `try(x(), silent = TRUE)` as a
blanket availability probe. Such a probe can conceal a missing Shiny reactive
context and turn a deterministic defect into polling or partial restore. Readiness
predicates use `.evaluate_restore_readiness()`; specialized availability handlers
must call `.is_shiny_context_error()` and rethrow a matching condition. The shared
callback boundary then records it as `REACTIVE_CONTEXT_VIOLATION`, never ordinary
unavailability.

Review includes callback bodies and transitively invoked restore helpers. An
`isolate()` annotation by itself is not an audit contract: Category 1 must have one
of the three explicit contracts above. Promise continuations and named custom
timer/retry callbacks are subject to the same rule as `onFlushed()` and `later()`.
