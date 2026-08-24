# Restore callback audit

`restore_callback_audit_manifest.csv` is the checked-in, reviewable inventory of
imperative boundaries that can be reached while restoring a session.  It covers
the restore framework and the Grid, GO, GSEA, STRING, Heatmap, Abundances, and
Data Wizard module trees (including every Data Wizard submodule).

## Classification

* **Category 1** — imperative deferred replay (`session$onFlushed()` or
  `later::later()`). Reactive values read by these callbacks must be isolated,
  and restore work should use the shared guarded callback/job helpers.
* **Category 2** — a reactive scheduler (`invalidateLater()`, `debounce()`, or
  `throttle()`). These boundaries are safe only as part of a reactive consumer;
  `invalidateLater()` outside one is never implicitly approved.
* **Category 3** — teardown/finalization (`cleanup_manager` registration,
  session cleanup, `on.exit()`, or a `tryCatch(..., finally=)` clause).

Each CSV row represents one reviewed source occurrence. `boundary_id` is stable
across ordinary line movement: it consists of file, mechanism, and occurrence
number. `reviewed_line` is navigational metadata, not its identity. The remaining
columns record owner, trigger, direct/transitive reactive-read contract,
isolation, catch semantics, generation/staleness guard, and failure behavior.

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
