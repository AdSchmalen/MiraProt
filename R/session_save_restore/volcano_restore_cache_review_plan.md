# Volcano Session-Restore Cache Review and Reference Plan

## 1) Requirement Understanding

The required behavior is:

1. Plot creation uses the currently loaded `data_mod` + `data_def`.
2. At plot creation time, the module captures:
   - plot source data (`data_mod`),
   - plot source metadata (`data_def`),
   - module UI settings used for creation.
3. Each captured data/metadata pair should be cache-addressable via a label/id so:
   - switching datasets later does not lose access to plot source pair,
   - multiple modules and plots can reference distinct source pairs,
   - duplicate pairs can reuse existing cache entries.
4. Session save must persist both:
   - currently loaded data,
   - all cached source pairs and their references.
5. Session restore must rebuild plots from cached source pair + cached plot-time UI,
   not from currently loaded data.

This is distinct from the normal "Create Plot" path:
- Create Plot should continue to use *live* current dataset.
- Restore replay should use *cached* source dataset by label/reference.

## 2) Review of Current Volcano Implementation

## Implemented (present)

### A. Plot-time cache capture
- `volcano_state$plot_creation_cache <- list(data_mod = data, data_def = data_def)`
- `volcano_state$plot_ui_cache <- list(...)` (replay-relevant UI values)

### B. Save/restore contract wiring
- `get_session_state()` exports:
  - `restore_plot_data_cache` from `plot_creation_cache`
  - `plot_ui_inputs` from `plot_ui_cache`
- `set_session_state()` stages:
  - `restore_plot_data_cache`
  - `plot_ui_cache`
  - seeds `plot_creation_cache` from restore cache

### C. Restore fallback replay
- If serialized ggplot objects are missing, fallback regeneration uses:
  - cached `data_mod`/`data_def` pair,
  - cached UI merged into input-like object.

### D. Dataset-switch safety guards
- `compute_data_signature()` and mismatch checks prevent implicit data-refresh
  from silently rebuilding plots on a different dataset.
- Auto-range fallback uses cached plot-creation data when mismatch exists.

## Partially implemented / to verify

### E. Explicit cache labels per plot/pair
- Current volcano implementation stores module-level cache objects.
- Global snapshot orchestration assigns `plot_data_cache_ref` labels in the
  central `plot_data_cache_pool`.
- What still needs formal verification:
  - one-to-many mapping for multiple volcano plots created from different source
    datasets in the same session,
  - whether per-plot cache labels are persisted (current behavior appears
    module-level, not per-plot-key map).

## 3) Gap Analysis Against Requirement

Requirement asks for explicit cache-label semantics enabling multiple switches
across modules/plots. Current implementation is close, but a strict per-plot
cache-label map in volcano should be made explicit if multiple source pairs can
coexist in one module instance.

Suggested hardening:

1. Store `plot_cache_ref_by_title` (or pair-id key) in module state.
2. On plot creation, resolve/create cache ref and bind it to plot title.
3. On restore replay, select the proper cache ref for the selected plot title.

## 4) Reference Blueprint for Dotplot (same strategy)

Dotplot should mirror the proven volcano restore model:

1. Capture at create-time:
   - source `data_mod`/`data_def`,
   - plot-time UI settings,
   - cache ref binding.
2. Keep live Create Plot unchanged.
3. On restore-only replay:
   - resolve cache by ref,
   - rebuild from cached pair + cached UI,
   - avoid using newly loaded dataset.
4. Guard against dataset-switch auto-refresh mutating restored plots.

## 5) Minimal-invasive Implementation Principle

- No behavior changes for regular Create Plot flow.
- Restore path receives strict cache sourcing.
- Existing observers remain intact, only guarded where they would overwrite
  restored state from a mismatched active dataset.

## 6) Update: Gap Closed

The previously identified gap (explicit per-plot cache labels) is now implemented
in the restore infrastructure and Volcano wiring:

- Snapshot save normalizes optional `plot_cache_ref_by_title` entries to the
  resolved `plot_data_cache_ref` id.
- Restore helper resolves `plot_cache_ref_by_title` into
  `restore_plot_data_cache_by_title` payloads.
- Volcano session contract now persists/restores `plot_cache_ref_by_title` and
  `restore_plot_data_cache_by_title`.
- Volcano restore regeneration prefers title-specific cached pairs first, then
  falls back to module-level cached pair.

This enables explicit title-to-cache binding during replay and closes the
label-level mapping gap documented above.
