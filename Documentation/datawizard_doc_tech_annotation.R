# ==============================================================================
# File: Documentation/datawizard_doc_tech_annotation.R
#
# Purpose:
#   Technical documentation for the Data Wizard Annotation submodule.
#   Loaded into modEnv by app.R alongside other documentation files.
#
# Defines:
#   render_tech_annotation_content() - Returns div() with architecture docs.
# ==============================================================================


#' Render technical documentation for the Annotation module.
#' @return A Shiny div tag with the full technical documentation content.
render_tech_annotation_content <- function() {
  div(
    class = "dw-guide-wrap",

    h3("Annotation / ID Mapping - Technical Documentation"),

    # --------------------------------------------------------------------------
    # Architecture overview
    # --------------------------------------------------------------------------
    h4("Module Architecture"),

    tags$p(
      "The Annotation submodule follows the same four-layer decomposition as ",
      "Basemean and Ratios. It lives as a third tab inside the 'Data expansion' ",
      "wellPanel and is wired through the standard integration layer."
    ),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
      "modules/Data Wizard/
  datawizard_annotation.R                              # Orchestrator (server-only)
  Annotation/
    datawizard_annotation_ui.R                         # Static UI (tab content only)
    datawizard_annotation_reactive.R                   # Reactive state factory
    datawizard_annotation_utils.R                      # Core logic (defaults, collapse, intra-species)
    datawizard_annotation_utils_biomart_cache.R        # BioMart disk cache I/O
    datawizard_annotation_utils_biomart_species.R      # BioMart species lookups, keytype fetch
    datawizard_annotation_utils_biomart_build.R        # Full/selective/missing cache builders
    datawizard_annotation_utils_biomart_mapping.R      # Cross-species ID mapping via Ensembl
    datawizard_annotation_utils_merge.R                # Identifier merging helpers
    datawizard_annotation_observer.R                   # Thin observer entrypoint
    datawizard_annotation_observer_general.R           # General observers (a, b, c, e, f)
    datawizard_annotation_observer_biomart.R           # BioMart mode (c1b, c1c, c1d)
    datawizard_annotation_observer_cache.R             # Cache management (c2, c3, c3a-c3c, c4)
    datawizard_annotation_observer_mapping.R           # Mapping execution (d0, d)
    datawizard_annotation_utils_observer.R             # Strategy switching + merge observers (s1, m1, m2)"
    ),

    # --------------------------------------------------------------------------
    # Sourcing and wiring
    # --------------------------------------------------------------------------
    h4("Sourcing & Wiring"),

    tags$ul(
      tags$li(tags$code("datawizard_annotation.R"), " is sourced into ",
              tags$code("modEnv"), " by ", tags$code("datawizard_module.R"),
              " alongside ratios and basemean."),
      tags$li("The orchestrator sources all 12 sub-files into ", tags$code("modEnv"),
              " in dependency order: core utils, BioMart utils (cache, species, build, mapping), ",
              "reactive state, four observer concern files, observer entrypoint, then UI."),
      tags$li(tags$code("modAnnotationUI()"), " is called in the existing ",
              tags$code("tabsetPanel"), " as the third ", tags$code("tabPanel"),
              "."),
      tags$li(tags$code("modAnnotationServer()"), " is initialized in ",
              tags$code("datawizard_integration.R"), " via ",
              tags$code("initialize_module_safely()"), ".")
    ),

    # --------------------------------------------------------------------------
    # Data flow
    # --------------------------------------------------------------------------
    h4("Data Flow"),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
"User selects: source column, source species, key types, strategy
       |
       v
Observer validates inputs, shows Abort button, wraps in withProgress()
       |
       v
Intra-species path:
  Load OrgDb from SQLite cache (session -> sqlite -> AH download)
  AnnotationDbi::mapIds(org_db, ...)

Cross-species path (direct BioMart preference with two-step fallback, BioMart strategy):
  Attempt 1 - Direct BioMart output:
    - Translate target keytype to BioMart attr via orgdb_keytype_to_biomart_attr()
    - If translatable: call biomart_map_ids() with target attr directly
    - If successful: return result (direct_biomart = TRUE, single step)
  Attempt 2 - Two-step fallback (if direct fails or target not translatable):
    Step 1: BioMart ortholog mapping -> ensembl_gene_id in target species
      - Strategy A: getBM() with homolog attributes (chunks of 200)
      - Strategy B: getLDS() in chunks of 200 (fallback; same as Strategy A)
      - Mirror rotation per chunk on transport/server errors
      - Adaptive chunk-size reduction after 2 consecutive failures
      - Jittered exponential backoff (up to 4 retries per chunk)
      - Interruptible sleep for abort responsiveness
      - Progress callback reports chunk-level progress
    Step 2 (if target key type != ENSEMBL):
      - Load target species OrgDb
      - AnnotationDbi::mapIds() from ENSEMBL to target key type
      - Reports step 2 unmapped count separately
       |
       v
collapse_mapping_results() -> one value per ID
       |
       v
add_annotation_column(data, ...) with make.unique()
       |
       v
set_data(new_data), hide Abort button
  -> rv$data_mod updated
  -> update_metadata_for_annotation_columns()
  -> rv$data_def synced

Identifier Merging path (merge strategy):
  get_identifier_columns(metadata) -> columns tagged as 'Identifier'
  validate_merge_inputs(data, id_columns, merge_mode)
  merge_identifiers(data, id_columns, merge_mode)
    - first_non_empty: first non-empty/non-NA value per row in list order
    - concatenate_all: comma-join all non-empty/non-NA values in list order
  build_merge_col_name(id_columns, merge_mode) with make.unique()
  data[[new_col]] <- merged_values
  set_data(new_data)
  -> rv$data_mod updated"
    ),

    # --------------------------------------------------------------------------
    # Cache reuse
    # --------------------------------------------------------------------------
    h4("Cache Reuse Strategy (SQLite-Backed)"),

    tags$p(
      "The annotation module reuses the GO module's cache infrastructure from ",
      tags$code("modules/GO/GO_module_hub_cache.R"), " (available via ", tags$code("modEnv"), "). ",
      "OrgDb objects are SQLite-backed AnnotationDbi instances and cannot be ",
      "serialized with saveRDS/readRDS across R sessions. The cache layer avoids ",
      "this by storing a copy of the underlying .sqlite file and reconstructing ",
      "the OrgDb in-process via ", tags$code("AnnotationDbi::loadDb()"), " on load."
    ),

    tags$h5("Cache metadata model"),
    tags$p(
      "Each organism cache directory contains a ", tags$code("cache_metadata.rds"),
      " file with the following fields:"
    ),
    tags$ul(
      tags$li(tags$code("cache_status"), " - 'valid' (sqlite present) or 'marker_only' (legacy/fallback)"),
      tags$li(tags$code("source"), " - 'annotationhub', 'local_sqlite', or 'package'"),
      tags$li(tags$code("sqlite_path"), " - absolute path to the cached .sqlite copy"),
      tags$li(tags$code("created"), " - ISO timestamp of initial cache creation"),
      tags$li(tags$code("updated"), " - ISO timestamp of last successful refresh"),
      tags$li(tags$code("ttl_days"), " - numeric TTL used for freshness validation (default 30)"),
      tags$li(tags$code("orgdb_name"), " - OrgDb package name (e.g. 'org.Hs.eg.db')")
    ),

    tags$h5("Cache functions (from modules/GO/GO_module_hub_cache.R and its GO hub peers)"),
    tags$ul(
      tags$li(tags$code("organism_to_orgdb()"), " - Species name to OrgDb package name"),
      tags$li(tags$code("load_organism_cache()"),
              " - Reads cache metadata, reconstructs OrgDb from cached .sqlite via",
              " AnnotationDbi::loadDb(). Falls back to legacy marker detection for",
              " backward compatibility with pre-SQLite cache layouts."),
      tags$li(tags$code("save_organism_cache()"),
              " - Resolves the .sqlite path from the live OrgDb via dbfile(),",
              " copies it into the organism cache directory, and writes structured",
              " cache metadata. Does NOT serialize the OrgDb R object."),
      tags$li(tags$code("has_valid_organism_cache()"),
              " - TRUE when cache metadata or legacy timestamp is within the TTL window."),
      tags$li(tags$code("load_keytypes_from_cache()"), " - Load cached key types (metadata-aware freshness)"),
      tags$li(tags$code("save_keytypes_to_cache()"), " - Save key types alongside cache metadata"),
      tags$li(tags$code("load_keytypes_with_download()"), " - Download key types via AnnotationHub"),
      tags$li(tags$code("load_annotation_hub_with_progress()"),
              " - Three-tier load strategy: (1) reconstruct from cached .sqlite,",
              " (2) fresh metadata => load from local AH cache, (3) stale/missing =>",
              " download from AnnotationHub. Saves .sqlite and metadata on success."),
      tags$li(tags$code("force_refresh_safe()"),
              " - Clears organism cache (sqlite artifact, metadata, keytypes),",
              " re-downloads OrgDb, saves .sqlite file and new metadata."),
      tags$li(tags$code("clear_organism_cache()"),
              " - Removes all cache artifacts for an organism (sqlite, metadata,",
              " keytypes, legacy markers, ah_cache subdirectory)."),
      tags$li(tags$code("invalidate_biomart_cache()"),
              " - Deletes BioMart species and keytype disk cache directories."),
      tags$li(tags$code("save_biomart_keytypes_cache_atomic()"),
              " - Atomic per-species keytype cache write: writes to temp staging file,",
              " validates by reading back, then renames into active location."),
      tags$li(tags$code("build_selective_biomart_cache()"),
              " - Selective BioMart refresh for specific source + target species only.",
              " Uses atomic writes; does not touch other cached species."),
      tags$li(tags$code("build_missing_biomart_cache()"),
              " - Downloads keytypes for species in the species list that do not yet",
              " have a cache entry on disk. Already-cached species are not touched.")
    ),

    tags$h5("Why OrgDb handles are not RDS-serialized"),
    tags$p(
      "OrgDb objects hold a live DBI connection to an underlying .sqlite file. ",
      "R's serializer walks the connection object graph and triggers infinite recursion ",
      "or returns a broken handle that fails on deserialization. The SQLite-backed ",
      "cache avoids this entirely by storing only the .sqlite file and reconstructing ",
      "the OrgDb with ", tags$code("AnnotationDbi::loadDb()"), " on each load."
    ),

    tags$h5("Refresh / Clear / Update behaviour"),
    tags$ul(
      tags$li(tags$b("Refresh Cache"), " - Mode-aware: in Annotation Hub mode ",
              "calls force_refresh_safe(): clears the organism SQLite cache and metadata, ",
              "re-downloads from AnnotationHub, saves new .sqlite and keytypes. Exactly one ",
              "controlled rebuild. In BioMart mode opens a cache management ",
              "modal with two sections. Section 1 (Species and Keytypes Cache): ",
              "(1) Refresh All Species and Keytypes -- full_biomart_refresh via ",
              tags$code("build_full_biomart_cache()"), "; ",
              "(2) Refresh Current Source and Target Keytypes -- selective_biomart_refresh via ",
              tags$code("build_selective_biomart_cache()"), "; ",
              "(3) Load Missing Species Keytypes -- missing_biomart_refresh via ",
              tags$code("build_missing_biomart_cache()"),
              ". Section 2 (Mapping Database Cache): ",
              "(4) Load Mapping Database for Current Pair via ",
              tags$code("build_mapping_tables_for_pair()"),
              " -- downloads all mapping tables for selected source/target pair; ",
              "(5) Preload All Default Species Databases via ",
              tags$code("build_preset_mapping_tables()"),
              " -- downloads mapping tables for all 9 default species combinations. ",
              "All BioMart refresh paths use atomic per-species replacement ",
              "(write to temp, validate, rename). The existing cache is never deleted upfront. ",
              "Protected by biomart_build_active concurrency guard. ",
              "Not applicable in merge mode (cache buttons are hidden)."),
      tags$li(tags$b("Clear Cache"), " - Removes all organism cache artifacts (intra-species)",
              " and BioMart cache. Invalidates session reactiveVals.",
              " Next operation triggers a fresh download."),
      tags$li(tags$b("Update Organisms"), " - Mode-aware: in Annotation mode fetches full",
              " organism list from AnnotationHub (with 30-day TTL refresh cycle); ",
              "in BioMart mode rebuilds the full persistent metadata cache (all species + all ",
              "keytypes for every species) via build_full_biomart_cache() with atomic writes. ",
              "BioMart cache is never automatically refreshed based on age -- only rebuilt on ",
              "explicit action or when data is missing. The existing cache is preserved during ",
              "the build. Protected by biomart_build_active concurrency guard.")
    ),

    tags$h5("TTL / timestamp semantics"),
    tags$p(
      "For OrgDb (Annotation mode), cache freshness is determined by the 'updated' timestamp ",
      "in cache_metadata.rds (or cache_timestamp.txt for legacy layouts). The organism cache TTL ",
      "is 30 days. Keytype caches use a stricter 10-day TTL; keytypes older than 10 days are ",
      "not loaded directly. On the startup default path, the module treats 14-day keytypes as ",
      "deferred/stale for direct loading and keeps static defaults without downloading. On later ",
      "species changes, a strict keytype miss can still use organism-specific static defaults ",
      "when the organism cache is within 30 days, avoiding a download. In portable mode ",
      "(MIRAPROT_GO_CACHE set), the age check is skipped because the bundled cache is always trusted."
    ),
    tags$p(
      "For BioMart data (BioMart mode), the cache is fully persistent with no age-based ",
      "auto-expiration. Timestamps (created_at, updated_at) are stored in the cache manifest ",
      "(cache_manifest.rds) for reporting and auditing purposes only. Refresh occurs only when: ",
      "(1) cache data is missing globally or for a specific species, or ",
      "(2) the user explicitly clicks 'Update Organisms' while in BioMart mode. ",
      "This prevents unnecessary network traffic and ensures the app remains usable without ",
      "a network connection."
    ),

    tags$h5("BioMart cache rationale (non-SQLite)"),
    tags$p(
      "BioMart data (species lists, keytypes per species) are plain R data frames and ",
      "character vectors -- not SQLite-backed database handles. They serialize cleanly ",
      "with saveRDS/readRDS and do not suffer from the broken-handle problem that ",
      "affects OrgDb objects. Therefore, BioMart data continues to use structured ",
      "RDS + timestamp caching. The cache is persistent (no TTL-based auto-expiration); ",
      "a cache_manifest.rds file tracks metadata (created_at, updated_at, species_discovered, ",
      "species_count, keytypes_cached_count, failed_species, missing_species, ",
      "per_species_status, on_disk_count, status, build_duration_secs, ",
      "refresh_session_id, refresh_mode, interrupted) for diagnostics ",
      "and status display."
    ),

    tags$h5("Full BioMart cache build (build_full_biomart_cache)"),
    tags$p(
      "The ", tags$code("build_full_biomart_cache()"), " function is a pure data/cache ",
      "function (no UI side effects). It fetches all discoverable species from Ensembl, ",
      "then iterates over every species to fetch and persist keytypes using atomic ",
      "per-species replacement. The function is independent from the currently selected ",
      "source/target species. Key guarantees:"
    ),
    tags$ul(
      tags$li("Iterates ALL species returned by BioMart, not just the currently selected species."),
      tags$li("The existing cache is NOT deleted upfront. Each species entry is replaced ",
              "atomically after successful download and validation via ",
              tags$code("save_biomart_keytypes_cache_atomic()"), "."),
      tags$li("If the build is interrupted, already-completed species retain their new data; ",
              "incomplete species keep their old cached version."),
      tags$li("Each per-species keytype write uses a staging file (keytypes.rds.tmp), which ",
              "is validated by reading it back and comparing, then renamed into the active ",
              "location. No partial or corrupt entry ever becomes active."),
      tags$li("The build is NOT marked successful if only one species keytype set was cached ",
              "(minimum two required). This prevents false success when only the selected species was cached."),
      tags$li("The manifest includes per-species success/failure/missing status, per-species ",
              "update timestamps, refresh session ID, refresh mode, and interrupted flag."),
      tags$li("Partial failures are tolerated: species that fail are recorded in the manifest ",
              "but do not prevent other species from being cached.")
    ),

    tags$h5("Selective BioMart cache refresh (build_selective_biomart_cache)"),
    tags$p(
      "The ", tags$code("build_selective_biomart_cache()"), " function refreshes keytypes ",
      "for only the specified source and target species. It uses atomic per-species ",
      "replacement and does not touch any other cached species. If both source and target ",
      "are the same species, only one fetch is performed (deduplication). Species metadata ",
      "is fetched only if the species list is not already cached on disk. The manifest is ",
      "updated with selective refresh metadata (session ID, refreshed species, duration) ",
      "without overwriting unrelated manifest fields."
    ),

    tags$h5("Missing species cache build (build_missing_biomart_cache)"),
    tags$p(
      "The ", tags$code("build_missing_biomart_cache()"), " function identifies species ",
      "from the current species list that do not yet have a keytypes entry on disk, then ",
      "downloads only those species using atomic per-species cache writes. Already-cached ",
      "species are never touched. If all species are already cached, the function returns ",
      "immediately with success. The manifest is updated with missing-refresh metadata ",
      "(download count, skip count, duration) without overwriting unrelated fields."
    ),

    tags$h5("Atomic cache write (save_biomart_keytypes_cache_atomic)"),
    tags$p(
      "The ", tags$code("save_biomart_keytypes_cache_atomic()"), " function provides ",
      "transaction-like safety for per-species keytype cache writes. It: ",
      "(1) writes keytypes to a temporary staging file (keytypes.rds.tmp); ",
      "(2) reads the staging file back and validates it matches the original data; ",
      "(3) only after successful validation, atomically renames the staging file into ",
      "the active cache location. If any step fails, the previous cache entry remains ",
      "intact and staging files are cleaned up."
    ),

    tags$h5("Manifest semantics (cache_manifest.rds)"),
    tags$p(
      "The manifest is stored as ", tags$code("cache_manifest.rds"), " in the BioMart cache ",
      "base directory and contains:"
    ),
    tags$ul(
      tags$li(tags$code("created_at"), " - ISO timestamp of the first build."),
      tags$li(tags$code("started_at"), " - ISO timestamp of the most recent build start."),
      tags$li(tags$code("updated_at"), " - ISO timestamp of the most recent build completion."),
      tags$li(tags$code("species_discovered"), " - Total species returned by BioMart species fetch."),
      tags$li(tags$code("species_count"), " - Same as species_discovered (for backward compatibility)."),
      tags$li(tags$code("keytypes_cached_count"), " - Number of species with keytypes successfully persisted."),
      tags$li(tags$code("failed_species"), " - Character vector of species where keytypes fetch or disk write failed."),
      tags$li(tags$code("missing_species"), " - Character vector of species returning no keytypes (empty result)."),
      tags$li(tags$code("per_species_status"), " - Named list (species -> list(status, keytypes_count/error, updated_at)) with per-species outcome and last successful update timestamp."),
      tags$li(tags$code("on_disk_count"), " - Actual count of keytype directories containing keytypes.rds on disk (post-build verification)."),
      tags$li(tags$code("status"), " - Overall build status: 'complete' (all species cached), 'partial' (some failed/missing), ",
              "'failed' (zero cached), 'failed_insufficient' (only one species cached), or 'aborted'."),
      tags$li(tags$code("build_duration_secs"), " - Wall-clock time of the build in seconds."),
      tags$li(tags$code("refresh_session_id"), " - Unique identifier for the most recent refresh session ",
              "(format: mode_YYYYMMDD_HHMMSS_random)."),
      tags$li(tags$code("refresh_mode"), " - Type of the most recent refresh: 'full_biomart_refresh', ",
              "'selective_biomart_refresh', or 'missing_biomart_refresh'."),
      tags$li(tags$code("interrupted"), " - Logical flag indicating whether the most recent refresh ",
              "was interrupted/aborted before completion."),
      tags$li(tags$code("last_selective_refresh"), " - (Optional) Named list with details of the most ",
              "recent selective refresh: session_id, species, refreshed, failed, timestamps, duration."),
      tags$li(tags$code("last_missing_refresh"), " - (Optional) Named list with details of the most ",
              "recent missing-species refresh: session_id, missing_count, downloaded, failed, timestamps, duration.")
    ),
    tags$p(
      "Timestamps are informational/audit only and are NOT used for auto-expiration. ",
      "The manifest is updated after every build (full, selective, or missing) and loaded ",
      "at session init for status display. On restart after an interrupted refresh, the ",
      "cache loads consistently without manual repair because each species entry is ",
      "committed individually via atomic replacement."
    ),

    tags$h5("Migration from legacy cache layout"),
    tags$p(
      "Existing installations with the old marker-only layout (organism_db.rds + ",
      "cache_timestamp.txt, no cache_metadata.rds) are detected automatically. ",
      "The legacy timestamp is read for freshness validation, and load_organism_cache() ",
      "returns NULL (triggering a re-download from AnnotationHub). On successful ",
      "re-download, save_organism_cache() writes the new metadata format, upgrading ",
      "the cache transparently."
    ),

    tags$p(
      "OrgDb objects are kept alive in a session-level reactiveVal (",
      tags$code("cached_org_db"), ") so that within a session each species is loaded",
      " at most once. Across sessions, the cached .sqlite file provides fast",
      " subsequent loads without any network access."
    ),

    # --------------------------------------------------------------------------
    # Species & key type synchronization
    # --------------------------------------------------------------------------
    h4("Species & Key Type Synchronization"),

    tags$p(
      "The species dropdown is populated with four common organisms at startup ",
      "for fast initial load. Clicking ", tags$b("'Update Organisms'"),
      " is mode-aware: in Annotation Hub mode it triggers ",
      tags$code("update_organisms_with_fresh_cache()"), " to fetch the full organism ",
      "list from AnnotationHub and repopulates both ", tags$code("species_annotation"),
      " (source) and ", tags$code("target_species_annotation"), " (BioMart target) ",
      "with the complete set, and also updates ", tags$code("pre_crossspecies_source_choices"),
      " so that switching away from BioMart mode later restores this list. In BioMart mode ",
      "it calls ", tags$code("build_full_biomart_cache()"), " to ",
      "rebuild the complete persistent BioMart metadata cache -- fetching all discoverable ",
      "species and all keytypes for every species using atomic per-species replacement. ",
      "The existing cache is preserved during the build. The results are saved to disk and ",
      "loaded into ", tags$code("cached_biomart_species"), " and ",
      tags$code("cached_biomart_keytypes"), " for near-instant session-level lookups. ",
      "The ", tags$b("'Refresh Cache'"), " button is mode-aware: ",
      "in Annotation Hub mode it calls ", tags$code("force_refresh_safe()"),
      " to re-download the OrgDb and key types for the currently selected species. ",
      "In BioMart mode it opens a cache management modal with two sections: ",
      "Section 1 (Species and Keytypes Cache) offers: ",
      "(1) Refresh All Species and Keytypes (", tags$code("build_full_biomart_cache()"), "), ",
      "(2) Refresh Current Source and Target Keytypes (",
      tags$code("build_selective_biomart_cache()"), "), and ",
      "(3) Load Missing Species Keytypes (", tags$code("build_missing_biomart_cache()"),
      "). Section 2 (Mapping Database Cache) offers: ",
      "(4) Load Mapping Database for Current Pair (",
      tags$code("build_mapping_tables_for_pair()"), ") and ",
      "(5) Preload All Default Species Databases (",
      tags$code("build_preset_mapping_tables()"),
      "). All BioMart refresh paths use atomic per-species replacement and are protected ",
      "by ", tags$code("biomart_build_active"), " to prevent concurrent builds. ",
      "Cache buttons are hidden in Identifier Merging mode."
    ),

    tags$p(
      "When the source species changes, an observer updates both key type dropdowns ",
      "via a prioritised fallback chain:"
    ),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
"Key type load order (species change observer - intra-species / AnnotationDB mode only):
  Observer b is SKIPPED when BioMart or merge mode is active; observer c1d handles
  source keytype updates in BioMart mode to avoid AnnotationDB values overriding
  BioMart keytypes.

1. load_keytypes_from_cache()            # keytypes.rds present and <= 10 days old
2. has_valid_organism_cache() + defaults # strict keytype miss, organism cache <= 30 days:
                                         # use organism-specific hardcoded defaults
                                         # (keytypes.rds absent/stale but organism cache is fresh)
3. load_keytypes_with_download()         # no marker or marker too old: fresh AH download
4. get_default_keytypes_for_organism()   # organism-specific hardcoded defaults on error
5. Minimal fallback                      # SYMBOL, ENTREZID, ENSEMBL, UNIPROT

Key type load order (c1d - source species change in BioMart mode):
1. cached_biomart_keytypes()[[species]]  # session-level in-memory cache (instant)
2. load_biomart_keytypes_cache()         # per-species keytypes.rds from disk (persistent, no TTL)
3. fetch_biomart_keytypes_for_species()  # live BioMart fetch; result saved to disk + session cache
4. get_biomart_compatible_keytypes(NULL) # static fallback if BioMart unreachable

Key type load order (c1b - BioMart mode selected, source species):
  Same cache-first order as c1d above (load_biomart_keytypes_cache ->
  fetch_biomart_keytypes_for_species -> static fallback).
  Falls back to get_biomart_compatible_keytypes(cached_keytypes()) only when
  BioMart is completely unreachable.

OrgDb load order (Map IDs action - intra-species):
1. cached_org_db()                       # session-level reactiveVal (same species)
2. load_organism_cache()                 # disk marker check; returns NULL for markers
   - marker fresh (<= TTL):
     load_annotation_hub()               # system BiocFileCache, no re-download
     -> fallback: load_annotation_hub_fresh() if BiocFileCache miss
   - stale/missing marker:
     load_annotation_hub_fresh()         # forced re-download into clean temp dir
3. force_refresh_safe()                  # only on explicit 'Refresh Cache' button"
    ),

    tags$p(
      "A guard reactive (", tags$code("keytype_loading"), ") prevents concurrent ",
      "loads; a tracker reactive (", tags$code("keytype_last_organism"), ") skips ",
      "redundant loads when the species has not changed."
    ),

    # --------------------------------------------------------------------------
    # Latest-selection-wins / stale request discard
    # --------------------------------------------------------------------------
    h4("Latest-Selection-Wins (Stale Request Discard)"),

    tags$p(
      "Species-triggered keytype updates can be slow (especially BioMart network fetches ",
      "on cache miss). If the user changes species rapidly while a previous update is ",
      "in flight, the older request's results must not overwrite the current UI state. ",
      "A two-layer defence prevents this: input debouncing and request-versioning tokens."
    ),

    tags$h5("Input debouncing"),
    tags$p(
      "Both the source and target species inputs are observed through ",
      tags$code("shiny::debounce()"), " reactives with a 50 ms window. With the full ",
      "persistent BioMart cache, session-level lookups are near-instant so the debounce ",
      "is kept short (just enough to coalesce rapid click bursts). When the user ",
      "clicks through multiple species quickly, only the final stable value after 50 ms ",
      "of inactivity triggers the keytype loading pipeline. Intermediate ",
      "selections are coalesced and never processed. The debounced reactives ",
      "(", tags$code("species_src_debounced"), ", ", tags$code("species_tgt_debounced"),
      ") are created at the top of ", tags$code("register_annotation_observers()"),
      " and shared by all species keytype observers (b, c1c, c1d)."
    ),

    tags$h5("Request versioning tokens"),
    tags$p(
      "Two monotonically increasing counters (", tags$code("source_update_token"),
      " and ", tags$code("target_update_token"), ") in the reactive state track the ",
      "latest request for each selector path. When an observer starts processing a ",
      "species change, it increments the counter and captures the token value. After ",
      "each potentially slow step (cache read, network fetch), the observer re-checks ",
      "whether its token still matches the current counter value. If a newer request ",
      "has incremented the counter, the observer returns immediately, discarding its ",
      "stale results without applying them to the UI."
    ),

    tags$h5("Immediate progress feedback"),
    tags$p(
      "Raw (non-debounced) ", tags$code("observeEvent()"), " handlers on the species ",
      "inputs update ", tags$code("keytype_status_message"), " immediately when the user ",
      "selects a new species, before the debounce window completes. This status message ",
      "is rendered in the ", tags$code("annotation_status"), " output area and shows ",
      "prefetch-stage feedback: scheduling request, checking session/disk cache, ",
      "fetching from BioMart/Annotation backend, and applying results. If a request is ",
      "discarded as stale, the status updates with a clear discard reason."
    ),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
"Stale-check points in each observer:
  Observer b (intra-species, debounced):
    1. After load_keytypes_from_cache()
    2. Before updateSelectInput (cache path)
    3. After load_keytypes_with_download()
    4. Before updateSelectInput (fetch path)

  Observer c1c (target species, debounced):
    1. After session cache check
    2. After load_biomart_keytypes_cache()
    3. After fetch_biomart_keytypes_for_species()
    4. Before updateSelectInput

  Observer c1d (source species, BioMart mode, debounced):
    Same pattern as c1c

  Observer c1b (BioMart mode selected):
    1. After source keytype fetch
    2. After species list fetch
    3. Before target keytype UI apply"
    ),

    tags$p(
      "Stale discards are logged with the request token values for auditability. ",
      "Example log: ", tags$code("KeyType [src-token=3]: STALE after download (current=5) -- discarding"), ". ",
      "The strategy change observer (c1b) bumps both source and target tokens ",
      "when entering BioMart mode, ensuring that any in-flight AnnotationDB ",
      "keytype load from observer b is also discarded."
    ),

    tags$h5("Progress indicator for species/keytype updates"),
    tags$p(
      "Each keytype update workflow is wrapped in ", tags$code("withProgress()"),
      " to provide real-time feedback. The progress detail text always describes the ",
      "current in-progress step (checking session cache, checking disk cache, fetching from ",
      "BioMart/Annotation backend, updating keytype dropdown, finalizing UI state). ",
      "Progress is closed automatically on success, stale-discard, or error. ",
      "Additionally, ", tags$code("keytype_status_message"), " is displayed in the ",
      "annotation_status output area for immediate, pre-progress feedback that appears ",
      "before the withProgress modal (which only shows once the debounced observer fires)."
    ),

    tags$h5("UI selection stability guarantees"),
    tags$p(
      "All programmatic ", tags$code("updateSelectInput()"), " calls on species dropdowns ",
      "follow a strict selection preservation protocol to prevent rollback:"
    ),
    tags$ul(
      tags$li(
        tags$b("NULL-selected pattern:"), " When updating dropdown choices and the user's ",
        "current selection is valid in the new choice set, ", tags$code("selected"),
        " is omitted (passed as NULL). This lets the Shiny client preserve whatever the ",
        "user currently has selected, preventing race conditions where a server-side ",
        "update overwrites a concurrent user change."
      ),
      tags$li(
        tags$b("Explicit fallback only when invalid:"), " A specific ", tags$code("selected"),
        " value is only sent when the user's current selection is NOT present in the new ",
        "choice set. In that case a fallback (e.g. Homo sapiens for source, Mus musculus ",
        "for target) is applied and logged."
      ),
      tags$li(
        tags$b("Guard logging:"), " Every programmatic species selection change is logged ",
        "with a GUARD tag indicating the observer source (e.g. c1b-toggle-ON, c2-biomart, ",
        "c2-annotation) and whether the selection was preserved or forced to a fallback."
      ),
      tags$li(
        tags$b("User notification on fallback:"), " When a selection is forced to a fallback ",
        "value because the previous selection became invalid, the user is informed via a ",
        "notification."
      )
    ),

    tags$h5("Separation of bulk cache build and UI updates"),
    tags$p(
      "The full BioMart cache builder (", tags$code("build_full_biomart_cache()"), ") is a ",
      "pure data/cache function with no UI side effects. It never calls ",
      tags$code("updateSelectInput()"), " or modifies session-level reactiveVals directly. ",
      "UI updates happen only AFTER the build completes, in the observer that invoked ",
      "the build. This ensures that background cache warming never overwrites current ",
      "input selections."
    ),

    tags$p(
      "When BioMart mode is selected, observer c1b performs several ",
      "synchronization steps: ",
      "(1) ", tags$code("from_keytype_annotation"), " is updated with BioMart keytypes ",
      "for the currently selected source species -- loaded from the per-species disk ",
      "cache (persistent, no TTL) via ", tags$code("load_biomart_keytypes_cache()"), ", or fetched ",
      "live via ", tags$code("fetch_biomart_keytypes_for_species()"), " on cache miss, ",
      "and then filtered to BioMart-compatible filter types via ",
      tags$code("get_biomart_compatible_keytypes()"), "; a fallback to the ",
      "AnnotationDB-filtered subset is used only when BioMart is unreachable; ",
      "(2) both ", tags$code("species_annotation"), " (source) and ",
      tags$code("target_species_annotation"), " (target) are set to the default preset ",
      "(", tags$code("ANNOTATION_DEFAULT_SPECIES_PRESET"), ") on strategy change, using the ",
      "NULL-selected pattern to preserve the user's current selection if valid; the full BioMart ",
      "list (from persistent disk cache or live via ",
      tags$code("fetch_biomart_species_with_scientific_names()"), " on cache miss) is only ",
      "shown after clicking Update Organisms ",
      "(falling back to ", tags$code("BIOMART_SPECIES_FALLBACK"), " on network failure); ",
      "(3) the shared ", tags$code("to_keytype_annotation"), " dropdown is updated to ",
      "BioMart-compatible key types for the target species, loaded from the per-species ",
      "keytype disk cache or fetched live on cache miss. ",
      "Observer b (AnnotationDB keytype load on species change) is guarded to skip ",
      "entirely when BioMart or merge mode is active so that BioMart keytypes are never ",
      "overwritten by AnnotationDB values. ",
      "Observer c1d keeps source keytypes in sync when the source species changes ",
      "while BioMart mode is already active, using the same cache-first pattern. ",
      "Observer c1c keeps target keytypes in sync when the target species changes. ",
      "When BioMart mode is deactivated (switching to another strategy), ", tags$code("species_annotation"),
      " is restored using the NULL-selected pattern: the user's current selection is ",
      "preserved if valid in the restored choices; otherwise the pre-BioMart ",
      "snapshot from ", tags$code("pre_crossspecies_source_choices"),
      " and ", tags$code("pre_crossspecies_species"), " is used; ",
      "both keytype dropdowns are restored to their full OrgDb/AnnotationHub sets. ",
      "The BioMart species data frame is cached in the session-level reactiveVal ",
      tags$code("cached_biomart_species"), " (holds the data frame) and per-species ",
      "keytypes are cached in ", tags$code("cached_biomart_keytypes"),
      " (named list: species -> keytypes vector)."
    ),

    # --------------------------------------------------------------------------
    # BioMart species and keytype cache
    # --------------------------------------------------------------------------
    h4("BioMart Species and Keytype Cache"),

    tags$p(
      "BioMart species and per-species keytypes are persisted to a timestamped disk ",
      "cache (", tags$code("cache/BioMart_Cache/"), ") following the same architecture ",
      "as the GO module OrgDb cache in ", tags$code("modules/GO/GO_module_hub_cache.R"), "."
    ),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
"cache/BioMart_Cache/
  species/
    species_list.rds         # data.frame(scientific_name, dataset)
    species_timestamp.txt    # as.character(Sys.time())
  keytypes/
    Homo_sapiens/
      keytypes.rds           # character vector of OrgDb-style keytypes
      keytypes_timestamp.txt
    Mus_musculus/
      keytypes.rds
      keytypes_timestamp.txt
    ..."
    ),

    tags$p(
      "Cache location priority (identical to GO module): ",
      "(1) ", tags$code("MIRAPROT_GO_CACHE"), " env var root (portable mode); ",
      "(2) ", tags$code("cache/BioMart_Cache/"), " relative to ", tags$code("getwd()"),
      "; (3) ", tags$code("tempdir()/MiraProt_BioMart_Cache"), " if project path is not writable."
    ),

    tags$p(tags$b("Scientific name derivation:"), " ",
      tags$code("fetch_biomart_species_with_scientific_names()"), " cross-references ",
      "the ", tags$code("biomaRt::listDatasets()"), " dataset column with the Ensembl ",
      "REST API endpoint ", tags$code("/info/species"), ". For each BioMart dataset ",
      "prefix (e.g. \"mmusculus\"), the genus initial and species epithet are matched ",
      "against the REST ", tags$code("name"), " field (e.g. \"mus_musculus\") using: ",
      tags$code("genus_initial(REST) == first_char(prefix)"),
      " AND ", tags$code("last_part(REST name) == remainder(prefix)"),
      ". This handles subspecies entries (e.g. \"cfamiliaris\" matches ",
      "\"canis_lupus_familiaris\"). Scientific names are constructed by capitalizing ",
      "the genus (e.g. \"mus_musculus\" -> \"Mus musculus\"). If the REST call fails, ",
      "abbreviated names are derived from the dataset prefix (e.g. \"H. sapiens\"). ",
      "If BioMart is unreachable, ", tags$code("BIOMART_SPECIES_FALLBACK"),
      " is used (11 common species with correct scientific names)."
    ),

    tags$p(tags$b("Cache invalidation policy:"),
      tags$ul(
        tags$li(
          "Persistent: the BioMart cache has no age-based auto-expiration. ",
          "Timestamps are stored for audit/reporting purposes only."
        ),
        tags$li(
          "Missing-only fetch: when BioMart mode is activated and the disk cache is ",
          "absent for a species, only that species is fetched on demand."
        ),
        tags$li(
          "Manual rebuild: clicking 'Update Organisms' or 'Refresh Cache' in BioMart mode ",
          "calls ", tags$code("invalidate_biomart_cache()"), " (deletes ", tags$code("species/"),
          " and ", tags$code("keytypes/"), " subdirectories) and rebuilds the full cache ",
          "for all species via ", tags$code("build_full_biomart_cache()"), ". Both buttons ",
          "are protected by the ", tags$code("biomart_build_active"), " concurrency guard."
        ),
        tags$li(
          "Portable mode: when ", tags$code("MIRAPROT_GO_CACHE"),
          " is set, the bundled cache is always trusted."
        ),
        tags$li(
          "Fallback hierarchy: disk cache -> live fetch -> ",
          tags$code("BIOMART_SPECIES_FALLBACK"), " (stored as session state only; ",
          "not written to disk as a fallback marker)."
        )
      )
    ),

    tags$p(tags$b("Cache functions (in datawizard_annotation_utils_biomart_cache.R):")),
    tags$ul(
      tags$li(tags$code("get_biomart_cache_dir()"), " - Returns cache base directory"),
      tags$li(tags$code("save_biomart_species_cache(species_df, debug_log)"),
              " - Saves species data frame + timestamp"),
      tags$li(tags$code("load_biomart_species_cache(debug_log)"),
              " - Loads species data frame from disk; NULL on miss (persistent, no TTL)"),
      tags$li(tags$code("save_biomart_keytypes_cache(species_name, keytypes, debug_log)"),
              " - Saves per-species keytypes + timestamp"),
      tags$li(tags$code("load_biomart_keytypes_cache(species_name, debug_log)"),
              " - Loads per-species keytypes from disk; NULL on miss (persistent, no TTL)"),
      tags$li(tags$code("invalidate_biomart_cache(debug_log)"),
              " - Deletes species/ and keytypes/ subdirectories"),
      tags$li(tags$code("warm_biomart_session_cache(species, existing_cache, debug_log)"),
              " - Bulk-loads per-species keytypes from disk into session cache"),
      tags$li(tags$code("fetch_biomart_keytypes_live(species_name, start_mirror_idx, debug_log)"),
              " - Low-level keytype fetch that propagates errors (no silent fallback); ",
              "returns list(keytypes, mirror_idx, error)"),
      tags$li(tags$code("fetch_keytypes_with_retry(species_name, max_retries, start_mirror_idx, abort_flag, debug_log)"),
              " - Resilient wrapper around fetch_biomart_keytypes_live with retry, jittered ",
              "exponential backoff, mirror rotation; returns detailed status ",
              "(keytypes, status, error_class, error_message, attempts, mirror_idx)"),
      tags$li(tags$code("build_full_biomart_cache(debug_log, progress_callback, abort_flag, max_retries_per_species)"),
              " - Fetches keytypes for ALL species using resilient per-species fetch, ",
              "persists to disk, validates integrity; returns manifest"),
      tags$li(tags$code("save_biomart_metadata_manifest(manifest, debug_log)"),
              " - Saves cache manifest with per-species status to cache_manifest.rds"),
      tags$li(tags$code("load_biomart_metadata_manifest(debug_log)"),
              " - Loads cache manifest from disk; NULL if not found")
    ),

    tags$h5("BioMart keytype caching strategy"),
    tags$p(
      "The module implements a ", tags$b("full persistent cache with on-demand fallback"),
      " strategy:"
    ),
    tags$ul(
      tags$li(
        tags$b("Full cache build:"), " When the user clicks Update Organisms or Refresh Cache ",
        "in BioMart mode, ", tags$code("build_full_biomart_cache()"), " fetches keytypes for ",
        "ALL discoverable species (typically 200+) and persists each to disk. This is a pure ",
        "data/cache operation with no UI side effects. The build is independent from the ",
        "currently selected source/target species."
      ),
      tags$li(
        tags$b("Resilient per-species fetch:"), " Each species keytype fetch uses ",
        tags$code("fetch_keytypes_with_retry()"), " which provides: up to 3 retries per species, ",
        "jittered exponential backoff (1s, 2s, 4s base), mirror rotation across 3 Ensembl endpoints ",
        "on transport/server errors, error classification (transport/server = retryable, ",
        "schema = non-retryable), and per-species outcome tracking (success/failed/empty/no_dataset/skipped). ",
        "A sticky mirror index is carried across species so consecutive fetches reuse the ",
        "last successful endpoint."
      ),
      tags$li(
        tags$b("Abort support:"), " The build checks an abort flag between species and during ",
        "retry backoff waits. On abort, remaining species are marked 'skipped' and partial results ",
        "are preserved."
      ),
      tags$li(
        tags$b("Concurrency guard:"), " A file-based lock (.build_lock in the cache directory) ",
        "prevents concurrent full-cache builds. The lock auto-expires after 30 minutes to handle ",
        "stale locks from crashed sessions. Additionally, the ", tags$code("biomart_build_active"),
        " reactiveVal prevents both Refresh Cache and Update Organisms from triggering overlapping ",
        "builds within the same session."
      ),
      tags$li(
        tags$b("Session cache warm-up:"), " After a full build or on first BioMart ",
        "activation, ", tags$code("warm_biomart_session_cache()"), " bulk-reads ALL disk-cached ",
        "keytypes into the session-level reactiveVal. Subsequent species switches hit the ",
        "session cache instantly (no disk I/O)."
      ),
      tags$li(
        tags$b("Three-tier lookup:"), " Observers c1c and c1d check: (1) session cache, ",
        "(2) disk cache, (3) BioMart live fetch -- in that order. Results from any tier ",
        "are stored in both session and disk cache for future lookups."
      ),
      tags$li(
        tags$b("Missing-species on-demand:"), " If a single species keytype cache is missing ",
        "(e.g. new species added to BioMart), it is fetched on demand when selected. Only the ",
        "missing species is fetched, not the entire cache."
      ),
      tags$li(
        tags$b("Cache policy:"), " Persistent (no TTL-based auto-expiration). ",
        "Session cache: valid for session lifetime. Manual rebuild via ",
        "'Update Organisms' or 'Refresh Cache' in BioMart mode. Manual invalidation ",
        "via 'Clear Cache'. Timestamps are informational/audit only."
      )
    ),

    tags$h5("Cache manifest and partial failure semantics"),
    tags$p(
      "The cache manifest (", tags$code("cache_manifest.rds"), ") is saved after every full ",
      "build and contains comprehensive metadata:"
    ),
    tags$ul(
      tags$li(tags$code("started_at"), " / ", tags$code("updated_at"),
              " - ISO timestamps for build start and completion"),
      tags$li(tags$code("species_discovered"), " - total species count from BioMart"),
      tags$li(tags$code("keytypes_cached_count"), " / ", tags$code("keytypes_cached_species_count"),
              " - number of species with successfully cached keytypes"),
      tags$li(tags$code("failed_species"), " - character vector of species that failed after all retries"),
      tags$li(tags$code("missing_species"), " - species that connected but had no matching keytypes or no dataset"),
      tags$li(tags$code("per_species_status"), " - named list: species -> list(status, error_class, ",
              "error_message, attempts, mirror_used, keytypes_count)"),
      tags$li(tags$code("on_disk_count"), " - post-build verification count of actual keytype directories on disk"),
      tags$li(tags$code("status"), " - final status enum: 'complete' (all cached), 'partial' (some failures), ",
              "'failed' (no species cached), 'failed_insufficient' (only 1 species), 'aborted' (user cancelled)"),
      tags$li(tags$code("build_duration_secs"), " - total wall-clock build time")
    ),
    tags$p(
      "A build is NOT marked successful if only one species keytype was cached (minimum 2 required). ",
      "The post-build integrity check compares the on-disk keytype directory count against the in-memory ",
      "build count and logs a warning if they differ. Partial builds are still usable: species with ",
      "successfully cached keytypes work normally; failed species fall back to the static keytype set ",
      "on demand."
    ),

    # --------------------------------------------------------------------------
    # AnnotationHub <-> BioMart compatibility
    # --------------------------------------------------------------------------
    h4("AnnotationHub \u2194 BioMart Compatibility"),

    tags$p(
      "OrgDb key type names (AnnotationHub convention) and BioMart attribute names ",
      "are different namespaces. Three functions manage the translation:"
    ),

    tags$ul(
      tags$li(
        tags$code("orgdb_keytype_to_biomart_attr(keytype)"),
        " - Returns the BioMart attribute name for a given OrgDb key type, or ",
        tags$code("NULL"), " if no mapping exists (e.g. ALIAS has no BioMart equivalent)."
      ),
      tags$li(
        tags$code("get_biomart_compatible_keytypes(available_keytypes)"),
        " - Filters a vector of OrgDb key types to those with a BioMart mapping. ",
        "Used to populate the source key type dropdown in BioMart mode."
      ),
      tags$li(
        tags$code("validate_biomart_compatibility(source_species, target_species, source_keytype, target_attr)"),
        " - Pre-flight validation executed before every BioMart query. Checks that ",
        "both species resolve to known BioMart datasets and that the source key type ",
        "has a BioMart mapping. Returns a list with ", tags$code("$valid"),
        " (logical), ", tags$code("$error"), " (message if invalid), and resolved ",
        tags$code("$source_attr"), ", ", tags$code("$source_dataset"), ", ",
        tags$code("$target_dataset"), "."
      )
    ),

    tags$p(
      "The same-species edge case: when source and target datasets are identical, ",
      "BioMart Strategy A is skipped and Strategy B (", tags$code("getBM()"),
      " within a single mart) is used. Results may differ from an OrgDb-based ",
      "intra-species query because the underlying database is different."
    ),

    # --------------------------------------------------------------------------
    # BioMart mapping strategy
    # --------------------------------------------------------------------------
    h4("BioMart Inter-/Intra-species Mapping Strategy"),

    tags$p(
      "BioMart mode mapping is implemented in ",
      tags$code("map_ids_crossspecies()"), " and prefers direct BioMart output ",
      "when the target key type has a known BioMart attribute equivalent. ",
      "A two-step fallback is used when direct output is not available."
    ),

    tags$table(
      class = "table table-bordered table-sm",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Approach"), tags$th("Backend"), tags$th("When Used"), tags$th("Purpose")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Direct (preferred)"),
          tags$td("BioMart"),
          tags$td("Target keytype has a BioMart equivalent (e.g. SYMBOL, ENSEMBL, UNIPROT)"),
          tags$td("Maps source IDs directly to the final target attribute in one step")
        ),
        tags$tr(
          tags$td("Two-step (fallback)"),
          tags$td("BioMart + OrgDb"),
          tags$td("Direct mapping returns no results or no BioMart equivalent exists"),
          tags$td("Step 1: maps to ensembl_gene_id; Step 2: OrgDb converts to final type")
        )
      )
    ),

    tags$p(
      "The direct approach first calls ", tags$code("orgdb_keytype_to_biomart_attr()"),
      " to determine if BioMart can provide the requested target type. If yes, ",
      tags$code("biomart_map_ids()"), " is called with that attribute. If it ",
      "succeeds, results are returned with ", tags$code('attr(result, "direct_biomart") <- TRUE'),
      " and no OrgDb step is needed."
    ),

    tags$p(
      "If the direct approach fails or is not applicable, the two-step fallback ",
      "queries BioMart for ", tags$code("ensembl_gene_id"), " as an intermediate, ",
      "then loads the target species OrgDb and uses ",
      tags$code("AnnotationDbi::mapIds()"), " to convert ENSEMBL to the final key type. ",
      "Mapping statistics for both steps are attached as attributes on the result."
    ),

    tags$p(
      "BioMart Strategy A uses ", tags$code("getBM()"), " with homolog attributes; ",
      "Strategy B falls back to ", tags$code("getLDS()"), ". ",
      "Both strategies use the same default chunk size of 200 IDs. ",
      "getLDS() has no hard per-chunk payload limit at 200 IDs; the adaptive ",
      "reduction logic provides the necessary safety net for unstable connections. ",
      "Failed chunks are retried up to 4 times with jittered exponential backoff. ",
      "Connections use ", tags$code("connect_ensembl_with_mirrors()"),
      " which rotates through three Ensembl mirrors (www.ensembl.org, ",
      "useast.ensembl.org, asia.ensembl.org)."
    ),

    tags$p(
      "Connection stability improvements: (1) mirror failover per chunk on ",
      "transport/server errors; (2) adaptive chunk-size reduction after 2 ",
      "consecutive failures (halving down to a minimum of 50 for Strategy A, ",
      "25 for Strategy B -- Strategy B keeps the lower floor because ",
      tags$code("getLDS()"), " is more fragile under extreme load); ",
      "(3) error classification (", tags$code("classify_biomart_error()"),
      ") tags errors as TRANSPORT, SERVER, SCHEMA, or UNKNOWN for diagnostic ",
      "logging; (4) connection reinitialization on stale/broken handles."
    ),

    tags$p(
      "A real-time progress callback (", tags$code("progress_callback"),
      " parameter) is threaded through ", tags$code("map_ids_crossspecies()"),
      " and ", tags$code("biomart_map_ids()"), " to ",
      tags$code("query_chunk_with_retry()"), ". The observer wraps the call in ",
      tags$code("withProgress()"), " and provides a closure that calls ",
      tags$code("setProgress()"), "/", tags$code("incProgress()"),
      ". Progress messages report the active strategy, chunk index and total, ",
      "attempt index and max, and backoff wait state."
    ),

    h4("Observer Implementation Notes"),

    tags$p(tags$b("withProgress scoping constraint:"), " ",
      "All variable assignments inside the ", tags$code("withProgress()"), " expression ",
      "MUST use ", tags$code("<-"), " (plain assignment), NOT ", tags$code("<<-"),
      " (super-assignment). Shiny evaluates the ", tags$code("withProgress"), " body via ",
      tags$code("eval(substitute(expr), envir = parent.frame())"), ", which makes the ",
      "calling frame (the observer's local environment) the current environment. ",
      tags$code("<-"), " therefore writes directly to the observer's local frame. ",
      tags$code("<<-"), " searches the ", tags$em("parent"), " of that frame, skipping the ",
      "local declarations entirely, and leaves the variables at their pre-",
      tags$code("withProgress"), " values (typically ", tags$code("NULL"), "). ",
      "This was the root cause of the cross-species mapping regression in PR 312: ",
      tags$code("mapped_values"), " and ", tags$code("target_col_name"),
      " were assigned with ", tags$code("<<-"), " and remained ", tags$code("NULL"),
      " after the progress block returned."
    ),

    tags$p(tags$b("Early-exit via mapping_cancelled flag:"), " ",
      tags$code("return()"), " called inside the ", tags$code("withProgress"), " body ",
      "only exits the ", tags$code("eval()"), " context (i.e. exits ",
      tags$code("withProgress"), " itself), not the enclosing observer handler. ",
      "Pre-mapping validation failures (missing target species, failed BioMart ",
      "compatibility check, missing OrgDb) are therefore signalled via ",
      tags$code("mapping_cancelled <- TRUE"), " (using ", tags$code("<-"), " so the ",
      "assignment lands in the observer frame). Successful mapping paths are wrapped ",
      "in the corresponding ", tags$code("else"), " branch. After ",
      tags$code("withProgress"), " returns, the first post-block statement is: ",
      tags$code("if (isTRUE(mapping_cancelled)) return(NULL)"),
      ", which correctly exits the observer."
    ),

    h4("Abort Mechanism"),

    tags$p(
      "An abort button (", tags$code("abort_annotation"), ") is visible ",
      "while a mapping is running. Clicking it sets ",
      tags$code("abort_flag(TRUE)"), " and shows an immediate status notification. ",
      "The flag is checked at multiple points: before each chunk, before each ",
      "retry attempt, after each BioMart request returns, and during backoff ",
      "waits. Backoff sleeps use ", tags$code("interruptible_sleep()"),
      " which polls the flag every 0.2 seconds, ensuring abort takes effect ",
      "within that interval."
    ),

    tags$p(
      "On abort, partial results collected so far are returned with ",
      tags$code('attr(result, "aborted") <- TRUE'), " and a distinct status ",
      "message is shown. An early abort heuristic also fires automatically: if ",
      "more than half the chunks have been processed and zero result rows were ",
      "returned, iteration stops."
    ),

    # --------------------------------------------------------------------------
    # Mapping & collapsing
    # --------------------------------------------------------------------------
    h4("Mapping Ambiguity & Deterministic Collapsing"),

    tags$p(
      "Identifier mappings can be one-to-many, many-to-one, or many-to-many. ",
      "All mappings produce exactly one cell per row. No row expansion occurs. ",
      "The collapse strategy determines how multiple targets are reduced:"
    ),

    tags$table(
      class = "table table-bordered table-sm",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Strategy"), tags$th("Behavior"), tags$th("Example")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("First match"),
          tags$td("Takes the first mapped value; deterministic"),
          tags$td("TP53 -> ENSG00000141510")
        ),
        tags$tr(
          tags$td("Semicolon-separated"),
          tags$td("Concatenates all unique mappings with ';'"),
          tags$td("TP53 -> ENSG00000141510;ENSG00000272440")
        )
      )
    ),

    tags$p(
      "Cells that contain multiple identifiers separated by ", tags$code(";"),
      " or ", tags$code(","), " (with optional surrounding whitespace, e.g. ",
      tags$code("\"P12345; P67890\""), ") are split automatically by ",
      tags$code("split_identifier_cell()"), " before lookup. Each token is queried ",
      "individually and the results are combined using the chosen strategy. ",
      "Empty tokens produced by consecutive or trailing separators are silently dropped. ",
      "Single-identifier cells are handled identically to before (no splitting occurs)."
    ),

    # --------------------------------------------------------------------------
    # Metadata update
    # --------------------------------------------------------------------------
    h4("Metadata Update"),

    tags$p(
      "New annotation columns receive metadata with ",
      tags$code("Content = 'Identifier'"), ", ",
      tags$code("Transformation = 'ID Mapping'"), ". ",
      "The update follows the same pattern as basemean: ",
      tags$code("setdiff() -> rbind() -> handson_metadata()"), "."
    ),

    # --------------------------------------------------------------------------
    # Column naming
    # --------------------------------------------------------------------------
    h4("Column Naming Convention"),

    tags$p(
      "All annotation column names are built by the deterministic helper ",
      tags$code("build_annotation_col_name(mode, source_species, from_keytype, to_keytype, target_species)"),
      ". Species names are abbreviated to a two-letter token via ",
      tags$code("abbreviate_species()"), " (genus initial uppercase + species initial lowercase, ",
      "e.g. 'Homo sapiens' -> 'Hs', 'Mus musculus' -> 'Mm'). ",
      "Tokens are separated by underscores and sanitized for machine safety."
    ),

    tags$p(
      "Intra-species format: ",
      tags$code("Intra_<SrcSp>_<FromKey>_to_<ToKey>"),
      " (e.g. ", tags$code("Intra_Hs_SYMBOL_to_ENSEMBL"), "). ",
      "Cross-species format: ",
      tags$code("Cross_<SrcSp>_<FromKey>_to_<TgtSp>_<ToKey>"),
      " (e.g. ", tags$code("Cross_Hs_SYMBOL_to_Mm_UNIPROT"), "). ",
      "If a column name already exists, ", tags$code("make.unique()"),
      " appends a numeric suffix."
    ),

    tags$p(
      "The status report includes a 'Cache date' row showing the timestamp of the ",
      "database cache used for the mapping run. In intra-species mode this is the ",
      "OrgDb/AnnotationHub cache timestamp; in BioMart mode it is the BioMart ",
      "species cache timestamp. If no cache metadata is available, the fallback text ",
      "'cache date unavailable' is shown. The cache date is read by ",
      tags$code("get_annotation_cache_date(mode, species, debug_log)"), "."
    ),

    # --------------------------------------------------------------------------
    # Function signatures
    # --------------------------------------------------------------------------
    h4("Key Function Signatures"),

    tags$pre(
      style = "background: #f5f5f5; padding: 12px; border-radius: 6px; font-size: 12px;",
"# Utils (pure logic, no Shiny)
get_default_keytypes_for_organism(orgdb_name)
orgdb_keytype_to_biomart_attr(keytype)
get_biomart_compatible_keytypes(available_keytypes)
fetch_biomart_species(debug_log)                          # legacy; returns named char vector
fetch_biomart_species_with_scientific_names(debug_log)    # returns data.frame(scientific_name, dataset)
fetch_biomart_keytypes_for_species(species_name, debug_log)
get_biomart_species_list()
ANNOTATION_DEFAULT_SPECIES_PRESET                         # character vector (9 species), initial UI preset
BIOMART_SPECIES_FALLBACK                                  # data.frame constant (9 species)
get_biomart_cache_dir()
save_biomart_species_cache(species_df, debug_log)
load_biomart_species_cache(max_cache_age_days = 7, debug_log)
save_biomart_keytypes_cache(species_name, keytypes, debug_log)
load_biomart_keytypes_cache(species_name, max_cache_age_days = 7, debug_log)
invalidate_biomart_cache(debug_log)
validate_biomart_compatibility(source_species, target_species, source_keytype, target_attr,
                               debug_log)
species_to_biomart_dataset(species_name)
species_to_homolog_prefix(species_name)
resolve_homolog_attribute(target_attr, homolog_prefix)
classify_biomart_error(msg)
interruptible_sleep(seconds, abort_flag, interval = 0.2)
connect_ensembl_with_mirrors(dataset = NULL, start_mirror_idx = 1, debug_log)
  # Returns: list(mart, mirror_idx)
biomart_map_ids(source_ids, source_dataset, source_attr, target_dataset, target_attr,
                chunk_size_a = 200, chunk_size_b = 200, max_retries = 4,
                abort_flag, debug_log, progress_callback = NULL)
collapse_mapping_results(mapped_list, strategy)
map_ids_intraspecies(ids, org_db, from_keytype, to_keytype, collapse_strategy, debug_log)
map_ids_crossspecies(ids, source_species, target_species, source_attr, target_keytype,
                     collapse_strategy, abort_flag, debug_log,
                     progress_callback = NULL)
  # Returns: mapped vector with attr(, 'direct_biomart') = TRUE/FALSE
split_identifier_cell(x)
  # Splits one cell on ';' or ',' (absorbing surrounding whitespace); trims tokens;
  # drops empty tokens; returns character(0) for NA / whitespace-only input.
  # Space-only separation is intentionally NOT performed to avoid false splits
  # in multi-word identifiers (e.g. gene descriptions).
add_annotation_column(data, source_col, mapped_values, target_col_name,
                      collapse_strategy = \"first\", debug_log)
  # Splits each source cell with split_identifier_cell(), looks up every token
  # in mapped_values, and combines hits using collapse_strategy.
  # Handles column name collisions via make.unique().
abbreviate_species(species_name)
build_annotation_col_name(mode, source_species, from_keytype, to_keytype, target_species)
get_annotation_cache_date(mode, species, debug_log)

# State factory
create_annotation_state(get_data, data_def, UI_config, debug_log)
  # Returns: ..., abort_flag,
  #          cached_biomart_species,          # data.frame(scientific_name, dataset) or NULL
  #          cached_biomart_keytypes,         # named list: species -> keytypes vector
  #          pre_crossspecies_species,        # selected value before BioMart mode
  #          pre_crossspecies_source_choices, # choices vector before BioMart mode
  #          merge_identifier_list,          # current identifier columns for merge (ordered)
  #          merge_default_identifiers       # original identifier columns for reset

# Observer registration (thin entrypoint delegates to five sub-files)
register_annotation_observers(input, output, session, ns, state, get_data, set_data,
                               data_def, UI_config, apply_ui_config, debug_log, DEBUG_LEVEL)
  # Sub-files:
  #   datawizard_annotation_observer_general.R:  a, b, c, e, f
  #   datawizard_annotation_observer_biomart.R:  c1b, c1c, c1d
  #   datawizard_annotation_observer_cache.R:    c2, c3, c3a-c3c, c4
  #   datawizard_annotation_observer_mapping.R:  d0, d
  #   datawizard_annotation_utils_observer.R:    s1, m1, m2

# Merge helpers (datawizard_annotation_utils_merge.R)
get_identifier_columns(meta, debug_log)
validate_merge_inputs(data, id_columns, merge_mode, debug_log)
merge_identifiers(data, id_columns, merge_mode, debug_log)
build_merge_col_name(id_columns, merge_mode)

# Orchestrator
modAnnotationUI(id)
modAnnotationServer(id, data_def, get_data, set_data, available_samples, UI_config, debug_level)"
    )
  )
}
