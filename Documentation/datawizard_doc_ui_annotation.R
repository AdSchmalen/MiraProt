# ==============================================================================
# File: Documentation/datawizard_doc_ui_annotation.R
#
# Purpose:
#   UI documentation for the Data Wizard Annotation submodule.
#   Describes all input widgets, their IDs, types, and behavior.
#   Loaded into modEnv by app.R alongside other documentation files.
#
# Defines:
#   render_annotation_ui_doc_content() - Returns div() with UI docs.
# ==============================================================================


#' Render UI documentation for the Annotation module.
#' @return A Shiny div tag with the UI documentation content.
render_annotation_ui_doc_content <- function() {
  div(
    class = "dw-guide-wrap",

    h3("Annotation Module - UI Documentation"),

    # --------------------------------------------------------------------------
    # Integration
    # --------------------------------------------------------------------------
    h4("UI Integration"),

    tags$p(
      "The Annotation tab is the third tab inside the existing 'Data expansion' ",
      "wellPanel, alongside 'Ratios & Statistics' and 'Basemean'. The UI function ",
      tags$code("datawizard_annotation_UI(ns)"), " returns a ", tags$code("tagList"),
      " that is rendered inside a ", tags$code("tabPanel"), ". It does not create ",
      "its own wellPanel or tabsetPanel."
    ),

    # --------------------------------------------------------------------------
    # Widget inventory
    # --------------------------------------------------------------------------
    h4("Input Widget Inventory"),

    tags$table(
      class = "table table-bordered table-sm",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Input ID"), tags$th("Type"), tags$th("Purpose"),
          tags$th("Populated By")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$code("source_column_annotation")),
          tags$td("selectInput"),
          tags$td("Column containing identifiers to convert"),
          tags$td("Observer: includes character/factor columns plus columns with ",
                  "metadata Content = 'Identifier'")
        ),
        tags$tr(
          tags$td(tags$code("annotation_strategy")),
          tags$td("selectInput"),
          tags$td("Annotation strategy selector (replaces the former cross-species checkbox)"),
          tags$td("Static: 'annothub' (default), 'biomart', or 'merge'. ",
                  "Controls which UI controls are visible and which mapping/merge logic is used. ",
                  "annothub = Annotation Hub Intraspecies Mapping, ",
                  "biomart = BioMart Intra-/Inter-species Mapping, ",
                  "merge = Identifier Merging.")
        ),
        tags$tr(
          tags$td(tags$code("species_annotation")),
          tags$td("selectInput"),
          tags$td("Source organism species"),
          tags$td("Initially the 9-species default preset (", tags$code("ANNOTATION_DEFAULT_SPECIES_PRESET"),
                  "); in Annotation Hub mode expanded to the full AnnotationHub list by ",
                  tags$code("update_organisms_annotation"),
                  "; in BioMart mode set to the default preset on strategy change ",
                  "and expanded to the full BioMart scientific-name species list on button press; ",
                  "restored from pre-BioMart snapshot on strategy change away from BioMart. ",
                  "Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("refresh_cache_annotation")),
          tags$td("actionButton"),
          tags$td("Mode-aware cache refresh: in Annotation Hub mode ",
                  "refreshes the OrgDb for the selected species. In BioMart mode ",
                  "opens a cache management modal with two sections: ",
                  "(1) Species and Keytypes Cache -- Refresh All Species and Keytypes ",
                  "(full refresh with atomic per-species replacement); ",
                  "Refresh Current Source and Target Keytypes (selective refresh); ",
                  "Load Missing Species Keytypes (download only uncached species). ",
                  "(2) Mapping Database Cache -- Load Mapping Database for Current Pair ",
                  "(downloads all mapping tables for the selected source/target species); ",
                  "Preload All Default Species Databases (downloads mapping tables for all ",
                  "combinations of the 9 default species). ",
                  "The existing cache is never deleted upfront; each entry ",
                  "is replaced atomically after successful download and validation. ",
                  "Hidden in merge mode."),
          tags$td("N/A (button)")
        ),
        tags$tr(
          tags$td(tags$code("clear_cache_annotation")),
          tags$td("actionButton"),
          tags$td("Mode-aware cache clear: in Annotation Hub mode removes ",
                  "the OrgDb disk cache for the selected species and invalidates the ",
                  "session-level OrgDb/keytype cache; in BioMart mode ",
                  "removes the BioMart species and keytype disk cache and invalidates the ",
                  "session-level BioMart caches. Fails gracefully with a warning ",
                  "notification when the cache directory is not writable. Hidden in merge mode."),
          tags$td("N/A (button)")
        ),
        tags$tr(
          tags$td(tags$code("update_organisms_annotation")),
          tags$td("actionButton"),
          tags$td("Mode-aware organism refresh: in Annotation Hub mode ",
                  "fetches from AnnotationHub and repopulates both ",
                  tags$code("species_annotation"), " and ",
                  tags$code("target_species_annotation"), " with the full AnnotationHub list; ",
                  "in BioMart mode rebuilds the full persistent BioMart ",
                  "metadata cache (all species + all keytypes) and ",
                  "repopulates both dropdowns with BioMart scientific-name species. ",
                  "Hidden in merge mode."),
          tags$td("N/A (button)")
        ),
        tags$tr(
          tags$td(tags$code("from_keytype_annotation")),
          tags$td("selectInput"),
          tags$td("Current identifier type of the source data"),
          tags$td("Observer: updated from GO cache or defaults on species change; ",
                  "restricted to BioMart-compatible types when BioMart mode is active. ",
                  "Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("to_keytype_annotation")),
          tags$td("selectInput"),
          tags$td("Target identifier type to convert to (shared for intra- and BioMart modes)"),
          tags$td("Observer: in Annotation Hub mode, shows full OrgDb keytypes for source species. ",
                  "In BioMart mode, shows BioMart-compatible keytypes for the target species ",
                  "loaded from the persistent BioMart disk cache (no TTL auto-expiration) or fetched live via ",
                  tags$code("fetch_biomart_keytypes_for_species()"), " on cache miss; ",
                  "falls back to static compatible set on network failure. ",
                  "Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("target_species_annotation")),
          tags$td("selectInput"),
          tags$td("Target species for ortholog mapping (displays scientific names in BioMart mode)"),
          tags$td("Initially the 9-species default preset (", tags$code("ANNOTATION_DEFAULT_SPECIES_PRESET"),
                  "); when BioMart mode is selected, set to the default preset; ",
                  "expanded to the full BioMart list (from persistent disk cache or live via ",
                  tags$code("fetch_biomart_species_with_scientific_names()"), " on cache miss) ",
                  "only after clicking Update Organisms; ",
                  "falls back to ", tags$code("BIOMART_SPECIES_FALLBACK"),
                  " (9 common species with scientific names) on network failure. ",
                  "Shown only in BioMart mode via shinyjs.")
        ),
        tags$tr(
          tags$td(tags$code("collapse_strategy_annotation")),
          tags$td("selectInput"),
          tags$td("How to collapse one-to-many mappings"),
          tags$td("Static: 'first' or 'semicolon'. Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("run_annotation")),
          tags$td("actionButton"),
          tags$td("Execute identifier mapping"),
          tags$td("N/A (button). Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("abort_annotation")),
          tags$td("actionButton"),
          tags$td("Abort a running mapping operation; returns partial results"),
          tags$td("N/A (button); hidden by default, shown via shinyjs while mapping is active. ",
                  "Hidden in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("merge_controls_panel")),
          tags$td("div (container)"),
          tags$td("Container for all merge-mode controls; hidden by default, shown in merge mode"),
          tags$td("N/A (container)")
        ),
        tags$tr(
          tags$td(tags$code("merge_identifier_list_ui")),
          tags$td("uiOutput"),
          tags$td("Dynamic list of identifier columns with drag-and-drop reorder and per-row remove"),
          tags$td("Observer: populated from metadata columns tagged as 'Identifier'. ",
                  "Supports drag-and-drop reordering via Sortable.js. ",
                  "Each row has an 'x' button to remove that column from the merge.")
        ),
        tags$tr(
          tags$td(tags$code("merge_reset_list")),
          tags$td("actionButton"),
          tags$td("Reset the identifier list to the original default order"),
          tags$td("N/A (button). Only shown in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("merge_behavior")),
          tags$td("selectInput"),
          tags$td("Merge behavior: 'First non-empty only' or 'Concatenate all'"),
          tags$td("Static: 'first_non_empty' (default) or 'concatenate_all'. Only shown in merge mode.")
        ),
        tags$tr(
          tags$td(tags$code("merge_run")),
          tags$td("actionButton"),
          tags$td("Execute identifier merge"),
          tags$td("N/A (button). Only shown in merge mode.")
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Strategy-driven visibility
    # --------------------------------------------------------------------------
    h4("Strategy-Driven Visibility"),

    tags$p(
      "The ", tags$code("annotation_strategy"), " dropdown controls which UI ",
      "elements are visible. The target species panel (",
      tags$code("target_species_panel"), ") is shown only when BioMart mode is ",
      "selected. All mapping-specific controls (species, keytypes, cache buttons, ",
      "Map IDs) are hidden in merge mode and replaced by the merge controls panel. ",
      "The visibility is managed via ", tags$code("shinyjs::show/hide"), " calls ",
      "in the strategy observer (observer s1)."
    ),

    tags$p(
      "Selecting BioMart mode triggers several ",
      "dropdown updates: (1) ", tags$code("from_keytype_annotation"), " is restricted ",
      "to the BioMart-compatible subset returned by ",
      tags$code("get_biomart_compatible_keytypes()"),
      " (e.g. SYMBOL, ENSEMBL, ENTREZID, UNIPROT); (2) both ",
      tags$code("species_annotation"), " (source) and ",
      tags$code("target_species_annotation"), " (target) are populated with scientific ",
      "names (e.g. 'Mus musculus') loaded from the BioMart species disk cache ",
      "(persistent, no TTL) or fetched live via ",
      tags$code("fetch_biomart_species_with_scientific_names()"), " on cache miss, ",
      "falling back to ", tags$code("BIOMART_SPECIES_FALLBACK"),
      " on complete network failure; (3) the shared ", tags$code("to_keytype_annotation"),
      " dropdown is updated to BioMart-compatible key types for the target species, ",
      "loaded from the per-species keytype disk cache or fetched live on cache miss. ",
      "All species dropdown updates use the NULL-selected pattern to preserve the ",
      "user's current selection if valid in the new choice set. ",
      "When switching away from BioMart mode, ", tags$code("species_annotation"),
      " is restored from the pre-BioMart snapshot (",
      tags$code("pre_crossspecies_source_choices"), "), preferring the user's current ",
      "selection if valid, and both keytype dropdowns ",
      "are restored to their full OrgDb/AnnotationHub sets."
    ),

    tags$p(
      "Additionally, in BioMart mode, when the ", tags$b("source"), " species ",
      "changes (observer c1d), ", tags$code("from_keytype_annotation"), " is updated ",
      "with BioMart-compatible keytypes for the new source species, loaded from the ",
      "per-species keytype disk cache or fetched live on cache miss. This ensures ",
      "that source and target keytype dropdowns always reflect their respective ",
      "species' actual BioMart capabilities."
    ),

    # --------------------------------------------------------------------------
    # BioMart species/keytype disk cache
    # --------------------------------------------------------------------------
    h4("BioMart Species/Keytype Cache"),

    tags$p(
      "In BioMart mode, species and keytype data are persisted to disk so ",
      "that repeated sessions do not require a network round-trip to Ensembl. ",
      "The cache resides in ", tags$code("cache/BioMart_Cache/"), " relative to the ",
      "project root (or a portable/temp equivalent). Two subdirectories are used:"
    ),

    tags$ul(
      tags$li(
        tags$code("species/"), " - A single ", tags$code("species_list.rds"),
        " file (data frame with ", tags$code("scientific_name"), " and ",
        tags$code("dataset"), " columns) plus ", tags$code("species_timestamp.txt"),
        " (ISO timestamp)."
      ),
      tags$li(
        tags$code("keytypes/{Species_Name}/"), " - Per-species ",
        tags$code("keytypes.rds"), " (character vector) and ",
        tags$code("keytypes_timestamp.txt"), " for each species that has been queried."
      )
    ),

    tags$p(
      "Cache policy: the BioMart species list and per-species keytypes are persistent ",
      "and do not auto-expire based on age. Timestamps are stored for audit/reporting only. ",
      "When BioMart mode is activated and the disk cache is absent for a species, ",
      "only that species is fetched on demand. ",
      "Clicking ", tags$b("'Update Organisms'"), " in BioMart mode calls ",
      tags$code("build_full_biomart_cache()"), " to rebuild the entire cache for all species ",
      "(after deleting the previous cache via ", tags$code("invalidate_biomart_cache()"), "). ",
      "The full build iterates all discoverable BioMart species and caches keytypes for each, ",
      "independent of the currently selected species. ",
      "Each species keytype fetch uses ", tags$code("fetch_keytypes_with_retry()"), " which ",
      "provides retry with jittered exponential backoff and mirror rotation on transport/server ",
      "errors, mirroring the resilience strategy used in BioMart identifier mapping. ",
      "A sticky mirror index is carried across species so that consecutive fetches reuse the ",
      "last successful endpoint. ",
      "Portable mode (when the ",
      tags$code("MIRAPROT_GO_CACHE"), " environment variable is set) always trusts the ",
      "bundled cache. If a live refresh fails, the ",
      "most recent valid cache is used; if no cache exists and the network is ",
      "unavailable, the static ", tags$code("BIOMART_SPECIES_FALLBACK"),
      " (11 common model organisms with scientific names) is used as the final fallback."
    ),

    tags$p(
      "The ", tags$b("Refresh Cache"), " button (", tags$code("refresh_cache_annotation"),
      ") is mode-aware. In Annotation Hub mode it refreshes the ",
      "OrgDb/AnnotationHub cache for the currently selected species. ",
      "In BioMart mode it opens a cache management modal dialog ",
      "with two sections:"
    ),
    tags$h5("Section 1: Species and Keytypes Cache"),
    tags$p(
      "Controls what species and key types appear in the dropdown menus. ",
      "Refreshing this section does not download any mapping data."
    ),
    tags$ul(
      tags$li(tags$b("Refresh All Species and Keytypes"), " -- Start a full BioMart refresh (all species + all keytypes). ",
              "Estimated runtime is approximately 3 hours. The existing cache is NOT ",
              "deleted upfront; each species is replaced atomically after successful ",
              "download and validation via ", tags$code("save_biomart_keytypes_cache_atomic()"),
              ". If the refresh is interrupted, already-completed species remain committed ",
              "and incomplete species keep their old cached version."),
      tags$li(tags$b("Refresh Current Source and Target Keytypes"), " -- Selective refresh using ",
              tags$code("build_selective_biomart_cache()"), ". Refreshes keytypes only for ",
              "the currently selected source and target species. All other cached species ",
              "remain unchanged. Completes in seconds."),
      tags$li(tags$b("Load Missing Species Keytypes"), " -- Downloads keytypes only for ",
              "species that exist in the species list but do not yet have a keytypes entry ",
              "on disk, via ", tags$code("build_missing_biomart_cache()"),
              ". Already-cached species are never touched.")
    ),
    tags$h5("Section 2: Mapping Database Cache"),
    tags$p(
      "Pre-downloads BioMart mapping tables so that subsequent ID mapping ",
      "operations complete instantly from cache without network requests."
    ),
    tags$ul(
      tags$li(tags$b("Load Mapping Database for Current Pair"), " -- Downloads all mapping ",
              "tables for the currently selected source and target species with all ",
              "available key type combinations via ", tags$code("build_mapping_tables_for_pair()"),
              ". Tables already cached on disk are skipped."),
      tags$li(tags$b("Preload All Default Species Databases"), " -- Downloads mapping tables ",
              "for ALL pair combinations of the 9 default species via ",
              tags$code("build_preset_mapping_tables()"),
              ". This is a long-running operation (potentially several hours). ",
              "Already-cached tables are skipped."),
      tags$li(tags$b("Cancel"), " -- Closes the modal without taking any action.")
    ),
    tags$p(
      "All BioMart refresh paths use ", tags$code("biomart_build_active"), " as a ",
      "concurrency guard to prevent overlapping builds. The modal confirmation flow ",
      "applies only in BioMart mode; Annotation mode refresh behavior remains unchanged."
    ),

    tags$p(
      "The ", tags$b("Clear Cache"), " button (", tags$code("clear_cache_annotation"),
      ") is mode-aware. In Annotation Hub mode it calls ",
      tags$code("clear_organism_cache()"), " to delete the OrgDb disk cache files ",
      "for the selected species and invalidates the session-level OrgDb/keytype ",
      "cache. In BioMart mode it calls ",
      tags$code("invalidate_biomart_cache()"), " to delete the BioMart species and ",
      "keytype cache directories. In both paths the operation is wrapped in ",
      tags$code("tryCatch"), " so a permission error produces a warning notification ",
      "rather than crashing the session. The three cache action buttons appear in a ",
      "single row in the order: Refresh Cache, Clear Cache, Update Organisms."
    ),

    tags$p(
      "The ", tags$code("abort_annotation"), " button is hidden by default. ",
      "It becomes visible (via ", tags$code("shinyjs::show()"), ") when the ",
      tags$code("run_annotation"), " observer starts a mapping, and is hidden ",
      "again (via ", tags$code("shinyjs::hide()"), " in ", tags$code("on.exit()"),
      ") when the mapping completes or is aborted. Abort is responsive during ",
      "chunk processing, retry backoff waits, and between BioMart requests, ",
      "using interruptible sleep intervals."
    ),

    # --------------------------------------------------------------------------
    # Column naming
    # --------------------------------------------------------------------------
    h4("Column Naming & Metadata"),

    tags$p(
      "All annotation column names are generated by the deterministic helper ",
      tags$code("build_annotation_col_name()"), " which encodes mapping context ",
      "into a compact, machine-safe name. Species are abbreviated to a two-letter ",
      "token (genus initial + species initial, e.g. 'Hs' for Homo sapiens) via ",
      tags$code("abbreviate_species()"), "."
    ),

    tags$p(
      "Intra-species column name format: ",
      tags$code("Intra_<SrcSp>_<FromKey>_to_<ToKey>"),
      " (e.g. ", tags$code("Intra_Hs_SYMBOL_to_ENSEMBL"), "). ",
      "BioMart cross-species column name format: ",
      tags$code("Cross_<SrcSp>_<FromKey>_to_<TgtSp>_<ToKey>"),
      " (e.g. ", tags$code("Cross_Hs_SYMBOL_to_Mm_UNIPROT"), "). ",
      "Identifier merge column name format: ",
      tags$code("Merged_ID_<mode>_<N>cols"),
      " (e.g. ", tags$code("Merged_ID_first_3cols"), " or ",
      tags$code("Merged_ID_concat_3cols"), "). ",
      "If a column name already exists, ", tags$code("make.unique()"),
      " appends a numeric suffix. ",
      "New annotation columns receive the metadata label ",
      tags$code("Content = 'Identifier'"), "."
    ),

    # --------------------------------------------------------------------------
    # Output
    # --------------------------------------------------------------------------
    h4("Output Elements"),

    tags$table(
      class = "table table-bordered table-sm",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Output ID"), tags$th("Type"), tags$th("Content")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$code("annotation_status")),
          tags$td("uiOutput / renderUI"),
          tags$td("Summary table of last mapping/merge result: source/target columns, ",
                  "mapping types, mode (Annotation Hub, BioMart, or Identifier Merging), strategy used, ",
                  "mapped/unmapped or merged/empty counts, and cache date of the database used for the run")
        )
      )
    ),

    # --------------------------------------------------------------------------
    # Progress indicator for species/keytype updates
    # --------------------------------------------------------------------------
    h4("Progress Indicator for Species/Keytype Updates"),

    tags$p(
      "When the user changes the source or target species, a Shiny progress bar ",
      "is displayed during the keytype update workflow. The progress text always ",
      "describes the current in-progress action (not the last completed action). ",
      "Typical progress steps include:"
    ),

    tags$ul(
      tags$li("Checking cache..."),
      tags$li("Loading from cache..."),
      tags$li("Fetching from BioMart / Annotation backend for <species>"),
      tags$li("Updating keytype dropdown..."),
      tags$li("Finalizing UI state")
    ),

    tags$p(
      "The progress bar is shown for all species/keytype update paths: ",
      "observer b (intra-species keytype load), observer c1c (target species change ",
      "in BioMart mode), and observer c1d (source species change in BioMart ",
      "mode). Progress is closed automatically on completion, stale-discard, or error."
    ),

    # --------------------------------------------------------------------------
    # Latest-selection-wins behavior
    # --------------------------------------------------------------------------
    h4("Latest-Selection-Wins (Stale Request Discard)"),

    tags$p(
      "When the user changes species rapidly (e.g. clicking through several species ",
      "in quick succession), only the final selection is processed. Two mechanisms ",
      "enforce this:"
    ),

    tags$ul(
      tags$li(
        tags$b("Input debouncing (50 ms):"), " Species input changes are passed through ",
        "a debounce filter. The keytype loading observer only fires after the user has ",
        "stopped changing species for 50 ms, ensuring intermediate clicks are coalesced ",
        "and never trigger expensive cache/network operations. The debounce is kept short ",
        "because the full persistent BioMart cache makes session lookups near-instant."
      ),
      tags$li(
        tags$b("Request versioning tokens:"), " Each species selector path (source and ",
        "target) has a monotonically increasing token counter (", tags$code("source_update_token"),
        " and ", tags$code("target_update_token"), "). When a new update request starts, ",
        "it increments the token and captures the value. After each slow step ",
        "(cache read, network fetch), the observer re-checks whether its captured token ",
        "still matches the current counter. If a newer request has since incremented the ",
        "counter, the observer discards its results without applying them to the UI."
      )
    ),

    tags$p(
      "This applies to all species-triggered keytype update paths, including the ",
      "strategy change observer (c1b) which bumps both source and target tokens ",
      "when entering BioMart mode. Stale requests are logged with their token ",
      "values for diagnostics."
    ),

    tags$p(
      "A status message appears immediately in the annotation status area when a ",
      "new species is selected, confirming that the selection was registered. The ",
      "full progress bar appears once the debounced observer begins processing. ",
      "If a request is discarded as stale, the status updates instantly with a ",
      "clear explanation."
    )
  )
}
