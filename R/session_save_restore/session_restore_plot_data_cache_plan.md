# Session Restore Plot Data Cache Concept and Implementation Plan

## Concept
- Introduce a **session-restore-only plot data cache** to prevent restored plot recreation from reading mutated `rv$data_mod`/`rv$data_def`.
- Each plotting module (`abundances`, `sampleids`, `pca`, `volcano`, `dotplot`, `venn`, `heatmap`) stores:
  - `plot_data_cache_id`
  - `plot_data_cache_ref` (module -> cache id)
- Snapshot stores a centralized cache pool:
  - `plot_data_cache_pool[[cache_id]] = list(data_mod, data_def)`
- Deduplication is id-based (no full dataframe comparison at save time).

## Id Strategy
- Add monotonic revision ids in Data Wizard for both canonical tables:
  - `rv$data_mod_revision_id`
  - `rv$data_def_revision_id`
- Cache id format: `<mod_rev>|<def_rev>|<nrow_mod>x<ncol_mod>|<nrow_def>x<ncol_def>`.
- Modules capture the current composite id at plot generation time and persist it in their module session state.

## Restore Behavior
- During session restore:
  - Module `set_session_state()` stages the cache ref id.
  - On `rv$session_restore_trigger`, module resolves cache by id and uses cached frames exclusively for recreation.
  - After restore-trigger recreation completes, module clears staged cache ref and returns to regular live-data mode.

## Prioritized Implementation Plan
1. **Core plumbing (highest priority)**
   - Extend snapshot schema to include `plot_data_cache_pool` and cache metadata.
   - Add helper APIs in `session_save_restore_core_helpers.R`.
2. **Data revision source (high)**
   - Add/maintain both `rv$data_mod_revision_id` and `rv$data_def_revision_id` in Data Wizard at all mutation boundaries.
   - Define update rules:
     - increment only `data_mod_revision_id` when only abundance matrix changes,
     - increment only `data_def_revision_id` when only metadata changes,
     - increment both when both are regenerated.
3. **Module contract update (high)**
   - Add `plot_data_cache_ref` support in get/set session state for target plotting modules.
4. **Restore-only data resolution (high)**
   - In each target module, use staged cached data only inside restore-trigger regeneration path.
5. **Cleanup + compatibility (medium)**
   - Backward compatibility with snapshots lacking cache fields.
6. **Validation & tests (medium)**
   - Add regression tests for dataset-switch-then-save scenario.

## Risk Assessment
- **High risk**: Missing updates to either revision id (`data_mod` or `data_def`) can cause stale cache re-use.
- **High risk**: Module-specific restore observers may bypass staged cache unless individually wired.
- **Medium risk**: Snapshot size can increase when either table changes often, because cache invalidation becomes more frequent.
- **Low risk**: Backward compatibility if optional fields are guarded.

## Impact of Adding `data_def` to Cache Scope
- **Scope increase**: every module cache entry and restore resolver must treat `data_mod` and `data_def` as an inseparable pair.
- **Core helper impact**:
  - cache registration helper must validate that both objects are present and data-frame typed,
  - deduplication key must include both revision ids,
  - cache pool payload shape becomes fixed: `list(data_mod = ..., data_def = ...)`.
- **Module impact**:
  - each module `get_session_state()` must save the pair reference (not just `data_mod`),
  - each module restore-trigger must resolve and pass both tables into generation code.
- **Test impact**:
  - add regression case where `data_mod` is unchanged but `data_def` changes before save,
  - add inverse case where `data_def` is unchanged but `data_mod` changes,
  - verify restored plot rebuild uses the exact saved pair in both cases.

## Senior R Shiny Review (Simulated)
- Feedback:
  1. Keep cache resolution inside each module’s restore-trigger observer; avoid global temporary overrides of `rv$data_mod`.
  2. Do not hash dataframes by default; revision-id ownership for both `data_mod` and `data_def` belongs to Data Wizard pipeline.
  3. Guard every new field with `%||%` fallbacks for old snapshots.
  4. Ensure restore-only cache path is one-shot and cleared after replay.
- Plan improvements applied:
  - Explicit one-shot staged cache ref per module.
  - No runtime dataframe deep-compare.
  - Optional-field tolerant schema evolution.
