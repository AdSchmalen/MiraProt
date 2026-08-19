# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_observer_cache.R
#
# Purpose:
#   Contains cache-management observers for the Annotation submodule: the
#   Update Organisms button, the Refresh Cache button (with its BioMart
#   modal sub-handlers), and the Clear Cache button.
#
# Architectural Role:
#   One of four concern-based observer files that together replace the former
#   monolithic datawizard_annotation_observer.R.  This file covers all
#   observers that create, refresh, or clear the persistent caches used by
#   both AnnotationDB and BioMart modes.
#
#   Called indirectly: register_annotation_observers() (the thin entrypoint in
#   datawizard_annotation_observer.R) delegates to
#   register_annotation_observers_cache() defined here.
#
# Responsibilities:
#   - Observer c2:  Update Organisms button (mode-aware).
#   - Observer c3:  Refresh Cache button (mode-aware; BioMart modal).
#   - Observer c3a: Full BioMart refresh (modal "Refresh All Species and Keytypes").
#   - Observer c3b: Selective BioMart refresh (modal "Refresh Current Source and Target Keytypes").
#   - Observer c3c: Load missing species/keytypes (modal "Load Missing Species Keytypes").
#   - Observer c3d: Load mapping database for current pair (modal "Load Mapping Database for Current Pair").
#   - Observer c3e: Preload all default species mapping databases (modal "Preload All Default Species Databases").
#   - Observer c3f: Download missing default pairs only (modal "Download Missing Default Pairs Only").
#   - Observer c3g: Clear keytype/species cache (confirmation modal).
#   - Observer c3h: Clear database cache (confirmation modal).
#   - Observer c3i: Clear all BioMart cache (confirmation modal).
#   - Observer c4:  Clear Cache button (mode-aware, AnnotationDB only).
#
# Integration Points / Dependencies:
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via destructured handles.
#   - BioMart build functions: build_full_biomart_cache(),
#     build_selective_biomart_cache(), build_missing_biomart_cache().
#   - GO cache helpers: update_organisms_with_fresh_cache(),
#     force_refresh_safe(), clear_organism_cache(), cleanup_temp_caches(),
#     clean_corrupt_annotationhub_cache(), load_keytypes_from_cache(),
#     get_default_keytypes_for_organism().
#   - BioMart cache helpers: load_biomart_species_cache(),
#     save_biomart_species_cache(), warm_biomart_session_cache(),
#     load_biomart_metadata_manifest(), invalidate_biomart_cache(),
#     invalidate_biomart_keytype_cache(), invalidate_biomart_database_cache(),
#     load_biomart_keytypes_cache().
#   - BioMart build helpers: build_full_biomart_cache(),
#     build_selective_biomart_cache(), build_missing_biomart_cache(),
#     build_mapping_tables_for_pair(), build_preset_mapping_tables(),
#     build_missing_preset_mapping_tables().
#
# Maintenance Guidance:
#   - Keep this file focused on cache lifecycle buttons.
#   - If a new cache-related button or modal handler is added, place it here.
#   - Target: stay below 1200 lines.
# ==============================================================================


#' Register cache-management annotation observers.
#'
#' @param input,output,session,ns  Shiny module objects.
#' @param state              Named list from create_annotation_state().
#' @param debug_log          Logging function.
#' @param DEBUG_LEVEL        Numeric debug verbosity.
register_annotation_observers_cache <- function(input, output, session, ns,
                                                 state,
                                                 debug_log, DEBUG_LEVEL) {

  # -- Destructure state handles -----------------------------------------------
  cached_org_db            <- state$cached_org_db
  cached_orgdb_name        <- state$cached_orgdb_name
  cached_keytypes          <- state$cached_keytypes
  keytype_last_organism    <- state$keytype_last_organism
  keytype_applied_orgdb    <- state$keytype_applied_orgdb
  keytype_choices_applied  <- state$keytype_choices_applied
  abort_flag               <- state$abort_flag
  cached_biomart_species          <- state$cached_biomart_species
  cached_biomart_keytypes         <- state$cached_biomart_keytypes
  biomart_cache_manifest          <- state$biomart_cache_manifest
  pre_crossspecies_source_choices <- state$pre_crossspecies_source_choices
  biomart_build_active            <- state$biomart_build_active

  set_progress_window <- function(value, detail = NULL, start = 0, end = 0.90) {
    if (!is.numeric(value) || length(value) == 0 || is.na(value[1])) {
      value <- 0
    }
    value <- max(0, min(1, value[1]))
    tryCatch(
      setProgress(value = start + (end - start) * value, detail = detail),
      error = function(e) NULL
    )
  }

  # --------------------------------------------------------------------------
  # c2. Update Organisms button observer
  #     Mode-aware: in Annotation Hub mode fetches from AnnotationHub;
  #     in BioMart mode rebuilds the full persistent BioMart metadata cache
  #     (all species + all keytypes).
  # --------------------------------------------------------------------------

  observeEvent(input$update_organisms_annotation, {
    is_cross <- identical(isolate(input$annotation_strategy), "biomart")

    if (!is_cross) {
      # Annotation DB mode: load from AnnotationHub for both dropdowns.
      withProgress(message = "Updating organisms from AnnotationHub...", value = 0, {
        incProgress(0.2, detail = "Fetching organism list from AnnotationHub")
        result <- update_organisms_with_fresh_cache(debug_log = debug_log)

        incProgress(0.8, detail = "Updating UI")
        debug_log(paste("AnnotationHub result success:", result$success,
                        "- Count:", result$organism_count), 1)

        if (is.list(result) && isTRUE(result$success)) {
          # Preserve user's current source species selection if valid in new choices
          current_src_sp <- isolate(input$species_annotation)
          src_in_choices <- !is.null(current_src_sp) &&
            (current_src_sp %in% result$organism_choices ||
             current_src_sp %in% names(result$organism_choices))
          if (src_in_choices) {
            debug_log(sprintf("GUARD [c2-annotation]: preserving source species '%s' (valid in new choices)", current_src_sp), 2)
            updateSelectInput(session, "species_annotation",
                              choices = result$organism_choices)
          } else {
            src_fallback <- if ("Homo sapiens" %in% names(result$organism_choices)) "Homo sapiens" else result$organism_choices[1]
            debug_log(sprintf("GUARD [c2-annotation]: source species fallback '%s' -> '%s' (previous not in new choices)",
                              current_src_sp, src_fallback), 1)
            updateSelectInput(session, "species_annotation",
                              choices  = result$organism_choices,
                              selected = src_fallback)
          }
          # Keep snapshot current so toggle-off can restore this list.
          pre_crossspecies_source_choices(result$organism_choices)

          # Preserve user's current target species selection if valid in new choices
          current_target_sp <- isolate(input$target_species_annotation)
          tgt_in_choices <- !is.null(current_target_sp) &&
            (current_target_sp %in% result$organism_choices ||
             current_target_sp %in% names(result$organism_choices))
          if (tgt_in_choices) {
            debug_log(sprintf("GUARD [c2-annotation]: preserving target species '%s' (valid in new choices)", current_target_sp), 2)
            updateSelectInput(session, "target_species_annotation",
                              choices = result$organism_choices)
          } else {
            target_fallback <- if ("Mus musculus" %in% names(result$organism_choices)) "Mus musculus" else result$organism_choices[1]
            debug_log(sprintf("GUARD [c2-annotation]: target species fallback '%s' -> '%s' (previous not in new choices)",
                              current_target_sp, target_fallback), 1)
            updateSelectInput(session, "target_species_annotation",
                              choices  = result$organism_choices,
                              selected = target_fallback)
          }

          showNotification(
            paste("Loaded", result$organism_count, "organisms from AnnotationHub."),
            type = "message", duration = 5
          )
        } else {
          showNotification(
            paste("Failed:", result$error),
            type = "error", duration = 5
          )
        }
      })

    } else {
      # BioMart mode: rebuild the full persistent BioMart metadata cache
      # (all species + all keytypes) and update UI.

      if (isTRUE(isolate(biomart_build_active()))) {
        showNotification(
          "A BioMart cache build is already in progress. Please wait.",
          type = "warning", duration = 5)
        return()
      }
      biomart_build_active(TRUE)
      on.exit(biomart_build_active(FALSE), add = TRUE)

      withProgress(message = "Building full BioMart metadata cache...", value = 0, {

        # NOTE: Existing cache is NOT deleted upfront.  build_full_biomart_cache()
        # uses atomic per-species replacement so interruption cannot leave the
        # cache empty or broken.
        incProgress(0.05, detail = "Starting full BioMart cache build")

        debug_log("Update Organisms (BioMart mode): starting full cache rebuild", 1)

        build_result <- build_full_biomart_cache(
          debug_log = debug_log,
          progress_callback = function(value, detail) {
            tryCatch(set_progress_window(value, detail, start = 0.05, end = 0.89),
                     error = function(e) debug_log(sprintf("c2 progress callback error: %s", e$message), 2))
          },
          abort_flag = NULL  # Cache operations are not user-abortable
        )

        used_fallback <- FALSE
        used_existing_cache <- FALSE
        if (isTRUE(build_result$success) && build_result$species_count > 0) {
          # Reload species from the freshly written cache
          biomart_df <- load_biomart_species_cache(debug_log = debug_log)
          if (is.null(biomart_df)) {
            biomart_df <- BIOMART_SPECIES_FALLBACK
            used_fallback <- TRUE
          }
        } else {
          biomart_df <- load_biomart_species_cache(debug_log = debug_log)
          if (is.null(biomart_df)) {
            biomart_df <- BIOMART_SPECIES_FALLBACK
            used_fallback <- TRUE
          } else {
            used_existing_cache <- TRUE
          }
          showNotification(
            "Could not fully rebuild BioMart cache. Keeping existing species cache when available.",
            type = "warning", duration = 5
          )
        }

        cached_biomart_species(biomart_df)

        sci_names      <- sort(unique(biomart_df$scientific_name))
        biomart_choices <- stats::setNames(sci_names, sci_names)

        setProgress(value = 0.90, detail = "Updating UI")

        # Preserve source species selection if valid; use NULL-selected pattern
        # to prevent overwriting concurrent user changes (no rollback).
        current_src_sp <- isolate(input$species_annotation)
        if (!is.null(current_src_sp) && current_src_sp %in% biomart_choices) {
          debug_log(sprintf("GUARD [c2-biomart]: preserving source species '%s' (valid in new choices)", current_src_sp), 2)
          updateSelectInput(session, "species_annotation",
                            choices = biomart_choices)
        } else {
          src_fallback <- if ("Homo sapiens" %in% biomart_choices) "Homo sapiens" else biomart_choices[1]
          debug_log(sprintf("GUARD [c2-biomart]: source species fallback '%s' -> '%s' (not in new choices)",
                            current_src_sp, src_fallback), 1)
          updateSelectInput(session, "species_annotation",
                            choices = biomart_choices, selected = src_fallback)
        }

        # Preserve target species selection if valid
        current_target_sp <- isolate(input$target_species_annotation)
        if (!is.null(current_target_sp) && current_target_sp %in% biomart_choices) {
          debug_log(sprintf("GUARD [c2-biomart]: preserving target species '%s' (valid in new choices)", current_target_sp), 2)
          updateSelectInput(session, "target_species_annotation",
                            choices = biomart_choices)
        } else {
          tgt_fallback <- if ("Mus musculus" %in% biomart_choices) "Mus musculus" else biomart_choices[1]
          debug_log(sprintf("GUARD [c2-biomart]: target species fallback '%s' -> '%s' (not in new choices)",
                            current_target_sp, tgt_fallback), 1)
          updateSelectInput(session, "target_species_annotation",
                            choices = biomart_choices, selected = tgt_fallback)
        }

        # Warm up the session cache from the freshly built disk cache
        setProgress(value = 0.95, detail = "Loading keytypes into session cache")
        kt_cache <- warm_biomart_session_cache(
          species        = NULL,
          existing_cache = list(),
          debug_log      = debug_log
        )
        cached_biomart_keytypes(kt_cache)

        # Store manifest in session for status display
        manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
        if (!is.null(manifest)) {
          biomart_cache_manifest(manifest)
        }

        duration_str <- sprintf("%.1f", build_result$duration_secs)
        missing_count <- length(build_result$missing_species)
        failed_count  <- length(build_result$failed_species)
        status_msg <- if (used_fallback) {
          sprintf("BioMart cache rebuild failed. Using built-in fallback (%d species).",
                  length(biomart_choices))
        } else if (used_existing_cache) {
          sprintf("BioMart cache rebuild failed. Keeping existing cache (%d species).",
                  length(biomart_choices))
        } else if (missing_count > 0 || failed_count > 0) {
          sprintf("BioMart cache rebuilt: %d species, %d keytypes cached, %d failed, %d missing (%.1fs).",
                  build_result$species_count, build_result$keytypes_cached,
                  failed_count, missing_count, build_result$duration_secs)
        } else {
          sprintf("BioMart cache rebuilt: %d species, all keytypes cached (%.1fs).",
                  build_result$species_count, build_result$duration_secs)
        }

        debug_log(sprintf("Update Organisms (BioMart mode): %s", status_msg), 1)
        showNotification(status_msg,
                         type = if (used_fallback || used_existing_cache) "warning" else "message",
                         duration = 7)
      })
    }
  })

  # --------------------------------------------------------------------------
  # c3. Refresh Cache button observer
  #     Mode-aware: in Annotation Hub mode mirrors GO module's refresh
  #     behavior (clears and re-downloads OrgDb for the currently selected
  #     species).  In BioMart mode opens a confirmation modal with refresh
  #     options split into two sections:
  #       Section 1 -- Species and Keytypes Cache (dropdown metadata):
  #         1) Full refresh (all species + all keytypes)
  #         2) Refresh only current source and target species keytypes
  #         3) Load missing species keytypes
  #       Section 2 -- Mapping Database Cache (BioMart mapping tables):
  #         4) Load mapping database for current source/target pair
  #         5) Preload all default species mapping databases
  # --------------------------------------------------------------------------

  observeEvent(input$refresh_cache_annotation, {
    is_cross <- identical(isolate(input$annotation_strategy), "biomart")

    if (!is_cross) {
      # -- Annotation DB mode: refresh OrgDb for the selected species --
      species <- input$species_annotation
      req(species)

      orgdb_name <- organism_to_orgdb(species)
      debug_log(paste("Refresh cache (AnnotationDB mode) requested for:", species, "->", orgdb_name), 1)

      withProgress(message = paste("Refreshing AnnotationDB cache for", species, "..."), value = 0, {

        incProgress(0.1, detail = "Cleaning temporary caches")
        tryCatch(cleanup_temp_caches(debug_log = debug_log), error = function(e) {
          debug_log(paste("cleanup_temp_caches failed:", e$message), 2)
        })

        incProgress(0.2, detail = "Checking AnnotationHub integrity")
        tryCatch(clean_corrupt_annotationhub_cache(debug_log = debug_log), error = function(e) {
          debug_log(paste("clean_corrupt_annotationhub_cache failed:", e$message), 2)
        })

        incProgress(0.3, detail = "Force refreshing organism database")
        refresh_result <- tryCatch({
          force_refresh_safe(orgdb_name, debug_log = debug_log)
        }, error = function(e) {
          debug_log(paste("force_refresh_safe failed:", e$message), 1)
          list(success = FALSE, error = e$message)
        })

        if (is.list(refresh_result) && isTRUE(refresh_result$success)) {
          # Invalidate session cache only after a successful replacement was
          # downloaded or an installed OrgDb fallback was loaded.  On failure,
          # keep the previous in-memory/cache state usable.
          cached_org_db(NULL)
          cached_orgdb_name(NULL)
          cached_keytypes(NULL)
          keytype_last_organism(NULL)
          keytype_applied_orgdb(NULL)
          keytype_choices_applied(list())

          incProgress(0.8, detail = "Reloading key types")

          shared_keytype_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
          shared_keytype_cache[[orgdb_name]] <- NULL
          session$userData$orgdb_keytypes_session_cache <- shared_keytype_cache

          # Reload key types with force-refresh semantics so explicit refresh
          # buttons bypass any shared in-session result and update disk caches.
          keytype_result <- tryCatch({
            resolve_orgdb_keytypes(orgdb_name,
                                   mode = "force_refresh",
                                   session_cache = session$userData$orgdb_keytypes_session_cache,
                                   max_keytype_cache_age_days = 10,
                                   debug_log = debug_log)
          }, error = function(e) {
            debug_log(paste("Post-refresh keytypes resolve failed:", e$message), 2)
            NULL
          })

          key_types <- keytype_result$keytypes %||% character(0)
          if (length(key_types) == 0) {
            key_types <- get_default_keytypes_for_organism(orgdb_name)
          }

          shared_keytype_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
          shared_keytype_cache[[orgdb_name]] <- key_types
          session$userData$orgdb_keytypes_session_cache <- shared_keytype_cache
          cached_keytypes(key_types)
          cached_orgdb_name(orgdb_name)

          updateSelectInput(session, "from_keytype_annotation",
                            choices  = key_types,
                            selected = if ("SYMBOL" %in% key_types) "SYMBOL" else key_types[1])
          updateSelectInput(session, "to_keytype_annotation",
                            choices  = key_types,
                            selected = if ("ENSEMBL" %in% key_types) "ENSEMBL" else key_types[1])
          keytype_applied_orgdb(orgdb_name)
          applied_keytype_choices <- keytype_choices_applied()
          applied_keytype_choices[[orgdb_name]] <- list(
            from = key_types,
            to = key_types
          )
          keytype_choices_applied(applied_keytype_choices)

          incProgress(1.0, detail = "Done")

          showNotification(
            paste("AnnotationDB cache refreshed for", species),
            type = "message", duration = 5
          )
        } else {
          incProgress(1.0, detail = "Refresh failed; keeping existing cache")
          error_msg <- refresh_result$error %||% "Unknown AnnotationHub error"
          showNotification(
            paste("AnnotationDB cache refresh failed for", species, "-", error_msg),
            type = "error", duration = 10
          )
        }
      })

    } else {
      # -- BioMart mode: show confirmation modal with refresh options --
      debug_log("Refresh cache (BioMart mode): showing confirmation modal", 1)

      if (isTRUE(isolate(biomart_build_active()))) {
        showNotification(
          "A BioMart cache build is already in progress. Please wait.",
          type = "warning", duration = 5)
        return()
      }

      src_sp  <- isolate(input$species_annotation)
      tgt_sp  <- isolate(input$target_species_annotation)

      source_label <- src_sp %||% "none"
      target_label <- tgt_sp %||% "none"
      selected_species_label <- if (identical(src_sp, tgt_sp) && !is.null(src_sp)) {
        tagList(tags$b("Selected species: "), src_sp)
      } else {
        tagList(tags$b("Selected species: "), source_label, " \u2192 ", target_label)
      }
      current_pair_label <- tagList(source_label, " \u2192 ", target_label)

      showModal(modalDialog(
        title = "BioMart Cache Management",
        tags$p(
          tags$b("BioMart uses two local caches:"),
          " one for available species and ID types, and one for ID-mapping tables. ",
          "Choose what you want to update below."
        ),
        tabsetPanel(
          tabPanel(
            "Species & ID Types",
            tags$h4("Species & ID Types"),
            tags$p(
              "Controls which species and identifier types are available in the Annotation dropdowns. ",
              "This does ", tags$b("not"), " download ID-mapping tables."
            ),
            tags$ul(
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Refresh All Species & ID Types:"), tags$br(),
                      "Reload species and ID types for all BioMart species. ",
                      tags$span(class = "text-muted", "May take several hours; existing entries remain until replacements download successfully.")),
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Refresh Selected Species:"), tags$br(),
                      "Refresh ID types only for the current selection. ",
                      selected_species_label, tags$span(class = "text-muted", " Usually completes quickly.")),
              tags$li(tags$b("Load Missing ID Types:"), tags$br(),
                      "Download ID types only for species that are missing them. ",
                      tags$span(class = "text-muted", "Already cached species are left unchanged."))
            ),
            fluidRow(
              style = "margin-bottom: 15px;",
              column(6, actionButton(ns("biomart_refresh_full"),
                                     "Refresh All Species & ID Types",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                                     width = "100%")),
              column(6, actionButton(ns("biomart_refresh_selective"),
                                     "Refresh Selected Species",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;",
                                     width = "100%"))
            ),
            fluidRow(
              column(6, actionButton(ns("biomart_refresh_missing"),
                                     "Load Missing ID Types",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #3498db; border-color: #3498db; color: #fff;",
                                     width = "100%")),
              column(6)
            )
          ),
          tabPanel(
            "Mapping Tables",
            tags$h4("Mapping Tables"),
            tags$p(
              "Stores BioMart ID-conversion tables locally. Cached mappings can be reused without another network request. ",
              "This does ", tags$b("not"), " change which species or ID types appear in the dropdowns."
            ),
            div(
              class = "well well-sm",
              style = "margin-bottom: 15px;",
              tags$b("Current pair"), tags$br(), current_pair_label
            ),
            tags$ul(
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Download Mapping Tables for Current Pair:"), tags$br(),
                      "Download every available ID-type combination for this pair. ",
                      tags$span(class = "text-muted", "Already cached tables are skipped; usually a few minutes.")),
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Preload All Default Species:"), tags$br(),
                      "Download mapping tables for every pair of the 9 default species. ",
                      tags$span(class = "text-muted", "Large download; may take several hours; cached tables are skipped.")),
              tags$li(tags$b("Fill Missing Default Species Pairs:"), tags$br(),
                      "Download only pairs with ",
                      tags$b("no mapping tables cached yet"), ". ",
                      tags$span(class = "text-muted", "Partially cached pairs are left unchanged."))
            ),
            fluidRow(
              style = "margin-bottom: 15px;",
              column(6, actionButton(ns("biomart_preload_all_defaults"),
                                     "Preload All Default Species",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                                     width = "100%")),
              column(6, actionButton(ns("biomart_load_pair_db"),
                                     "Download Tables for Current Pair",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;",
                                     width = "100%"))
            ),
            fluidRow(
              column(6, actionButton(ns("biomart_preload_missing_defaults"),
                                     "Fill Missing Default Species Pairs",
                                     class = "btn-default btn-sm",
                                     style = "background-color: #3498db; border-color: #3498db; color: #fff;",
                                     width = "100%")),
              column(6)
            )
          ),
          tabPanel(
            "Clear Cache",
            tags$h4("Clear Cache"),
            tags$p("Delete one cache or both. Each action asks for confirmation before anything is removed."),
            tags$ul(
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Clear Species & ID Type Cache:"), tags$br(),
                      "Deletes the species list and ID types. Mapping tables are not affected."),
              tags$li(style = "margin-bottom: 8px;",
                      tags$b("Clear Mapping Table Cache:"), tags$br(),
                      "Deletes all downloaded mapping tables. Species and ID-type lists are not affected."),
              tags$li(tags$b("Clear All BioMart Cache:"), tags$br(),
                      "Deletes both caches. Everything will be downloaded again when needed.")),
            fluidRow(
              style = "margin-bottom: 15px;",
              column(6, actionButton(ns("biomart_clear_keytype_cache"),
                                     "Clear Species & ID Type Cache",
                                     icon = icon("trash"),
                                     class = "btn-default btn-sm",
                                     style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;",
                                     width = "100%")),
              column(6, actionButton(ns("biomart_clear_database_cache"),
                                     "Clear Mapping Table Cache",
                                     icon = icon("trash"),
                                     class = "btn-default btn-sm",
                                     style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;",
                                     width = "100%"))
            ),
            fluidRow(
              column(6, actionButton(ns("biomart_clear_all_cache"),
                                     "Clear All BioMart Cache",
                                     icon = icon("trash"),
                                     class = "btn-default btn-sm",
                                     style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;",
                                     width = "100%")),
              column(6)
            )
          )
        ),

        footer = tagList(
          modalButton("Close")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }
  })

  # --------------------------------------------------------------------------
  # c3a. Full BioMart refresh (confirmed via modal "Yes")
  #      Atomic per-species replacement; existing cache NOT deleted upfront.
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_refresh_full, {
    removeModal()
    debug_log("Refresh cache (BioMart mode): user confirmed full refresh", 1)

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(message = "Full BioMart cache refresh (all species + keytypes)...", value = 0, {

      build_result <- build_full_biomart_cache(
        debug_log = debug_log,
        progress_callback = function(value, detail) {
          tryCatch(set_progress_window(value, detail, start = 0.02, end = 0.89),
                   error = function(e) debug_log(sprintf("c3a progress callback error: %s", e$message), 2))
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      used_fallback <- FALSE
      used_existing_cache <- FALSE
      if (isTRUE(build_result$success) && build_result$species_count > 0) {
        biomart_df <- load_biomart_species_cache(debug_log = debug_log)
        if (is.null(biomart_df)) {
          biomart_df <- BIOMART_SPECIES_FALLBACK
          used_fallback <- TRUE
        }
      } else {
        biomart_df <- load_biomart_species_cache(debug_log = debug_log)
        if (is.null(biomart_df)) {
          biomart_df <- BIOMART_SPECIES_FALLBACK
          used_fallback <- TRUE
        } else {
          used_existing_cache <- TRUE
        }
        showNotification(
          "Could not fully rebuild BioMart cache. Keeping existing species cache when available.",
          type = "warning", duration = 5
        )
      }

      cached_biomart_species(biomart_df)

      sci_names      <- sort(unique(biomart_df$scientific_name))
      biomart_choices <- stats::setNames(sci_names, sci_names)

      setProgress(value = 0.90, detail = "Updating UI")

      # Preserve source species selection if valid (NULL-selected pattern)
      current_src_sp <- isolate(input$species_annotation)
      if (!is.null(current_src_sp) && current_src_sp %in% biomart_choices) {
        debug_log(sprintf("GUARD [c3a-biomart-refresh]: preserving source species '%s'", current_src_sp), 2)
        updateSelectInput(session, "species_annotation", choices = biomart_choices)
      } else {
        src_fallback <- if ("Homo sapiens" %in% biomart_choices) "Homo sapiens" else biomart_choices[1]
        debug_log(sprintf("GUARD [c3a-biomart-refresh]: source species fallback '%s' -> '%s'",
                          current_src_sp, src_fallback), 1)
        updateSelectInput(session, "species_annotation",
                          choices = biomart_choices, selected = src_fallback)
      }

      # Preserve target species selection if valid
      current_target_sp <- isolate(input$target_species_annotation)
      if (!is.null(current_target_sp) && current_target_sp %in% biomart_choices) {
        debug_log(sprintf("GUARD [c3a-biomart-refresh]: preserving target species '%s'", current_target_sp), 2)
        updateSelectInput(session, "target_species_annotation", choices = biomart_choices)
      } else {
        tgt_fallback <- if ("Mus musculus" %in% biomart_choices) "Mus musculus" else biomart_choices[1]
        debug_log(sprintf("GUARD [c3a-biomart-refresh]: target species fallback '%s' -> '%s'",
                          current_target_sp, tgt_fallback), 1)
        updateSelectInput(session, "target_species_annotation",
                          choices = biomart_choices, selected = tgt_fallback)
      }

      # Warm session cache from freshly built disk cache
      setProgress(value = 0.95, detail = "Loading keytypes into session cache")
      kt_cache <- warm_biomart_session_cache(
        species        = NULL,
        existing_cache = list(),
        debug_log      = debug_log
      )
      cached_biomart_keytypes(kt_cache)

      # Store manifest for status display
      manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
      if (!is.null(manifest)) {
        biomart_cache_manifest(manifest)
      }

      missing_count <- length(build_result$missing_species)
      failed_count  <- length(build_result$failed_species)
      status_msg <- if (used_fallback) {
        sprintf("Full BioMart cache refresh failed. Using built-in fallback (%d species).",
                length(biomart_choices))
      } else if (used_existing_cache) {
        sprintf("Full BioMart cache refresh failed. Keeping existing cache (%d species).",
                length(biomart_choices))
      } else if (missing_count > 0 || failed_count > 0) {
        sprintf("Full BioMart cache refresh: %d species, %d keytypes cached, %d failed, %d missing (%.1fs).",
                build_result$species_count, build_result$keytypes_cached,
                failed_count, missing_count, build_result$duration_secs)
      } else {
        sprintf("Full BioMart cache refresh: %d species, all keytypes cached (%.1fs).",
                build_result$species_count, build_result$duration_secs)
      }

      debug_log(sprintf("c3a full refresh: %s", status_msg), 1)
      showNotification(status_msg,
                       type = if (used_fallback || used_existing_cache) "warning" else "message",
                       duration = 7)
    })
  })

  # --------------------------------------------------------------------------
  # c3b. Selective BioMart refresh (source + target species only)
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_refresh_selective, {
    removeModal()

    src_sp <- isolate(input$species_annotation)
    tgt_sp <- isolate(input$target_species_annotation)
    debug_log(sprintf(
      "Refresh cache (BioMart mode): user chose selective refresh for '%s' and '%s'",
      src_sp %||% "NULL", tgt_sp %||% "NULL"), 1)

    if (is.null(src_sp) && is.null(tgt_sp)) {
      showNotification("No source or target species selected.", type = "warning", duration = 5)
      return()
    }

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(message = "Selective BioMart refresh (source + target species)...", value = 0, {

      sel_result <- build_selective_biomart_cache(
        source_species  = src_sp,
        target_species  = tgt_sp,
        debug_log       = debug_log,
        progress_callback = function(value, detail) {
          tryCatch(set_progress_window(value, detail, start = 0.02, end = 0.89),
                   error = function(e) debug_log(sprintf("c3b progress callback error: %s", e$message), 2))
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      # Update session cache for the refreshed species
      setProgress(value = 0.90, detail = "Updating session cache")
      current_kt_cache <- isolate(cached_biomart_keytypes())
      for (sp in sel_result$species_refreshed) {
        kt <- load_biomart_keytypes_cache(sp, debug_log = debug_log)
        if (!is.null(kt) && length(kt) > 0) {
          current_kt_cache[[sp]] <- kt
        }
      }
      cached_biomart_keytypes(current_kt_cache)

      # Update manifest in session
      manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
      if (!is.null(manifest)) {
        biomart_cache_manifest(manifest)
      }

      refreshed_str <- paste(sel_result$species_refreshed, collapse = ", ")
      failed_str    <- paste(sel_result$failed_species, collapse = ", ")

      if (isTRUE(sel_result$success)) {
        status_msg <- sprintf(
          "Selective refresh complete: %d species refreshed (%s) in %.1fs.",
          length(sel_result$species_refreshed), refreshed_str,
          sel_result$duration_secs)
        showNotification(status_msg, type = "message", duration = 7)
      } else {
        status_msg <- sprintf(
          "Selective refresh: %d refreshed, %d failed (%s) in %.1fs.",
          length(sel_result$species_refreshed), length(sel_result$failed_species),
          failed_str, sel_result$duration_secs)
        showNotification(status_msg, type = "warning", duration = 7)
      }

      debug_log(sprintf("c3b selective refresh: %s", status_msg), 1)
    })
  })

  # --------------------------------------------------------------------------
  # c3c. Load missing species/keytypes (download only uncached species)
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_refresh_missing, {
    removeModal()
    debug_log("Refresh cache (BioMart mode): user chose load missing species/keytypes", 1)

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(message = "Loading missing BioMart species/keytypes...", value = 0, {

      miss_result <- build_missing_biomart_cache(
        debug_log       = debug_log,
        progress_callback = function(value, detail) {
          tryCatch(set_progress_window(value, detail, start = 0.02, end = 0.89),
                   error = function(e) debug_log(sprintf("c3c progress callback error: %s", e$message), 2))
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      # Warm session cache with newly downloaded species
      setProgress(value = 0.90, detail = "Updating session cache")
      current_kt_cache <- isolate(cached_biomart_keytypes())
      for (sp in miss_result$species_downloaded) {
        kt <- load_biomart_keytypes_cache(sp, debug_log = debug_log)
        if (!is.null(kt) && length(kt) > 0) {
          current_kt_cache[[sp]] <- kt
        }
      }
      cached_biomart_keytypes(current_kt_cache)

      # Update manifest in session
      manifest <- load_biomart_metadata_manifest(debug_log = debug_log)
      if (!is.null(manifest)) {
        biomart_cache_manifest(manifest)
      }

      n_downloaded <- length(miss_result$species_downloaded)
      n_failed     <- length(miss_result$failed_species)
      n_skipped    <- miss_result$species_skipped

      if (n_downloaded == 0L && n_failed == 0L) {
        status_msg <- sprintf(
          "All %d species already cached. Nothing to download.",
          n_skipped)
        showNotification(status_msg, type = "message", duration = 5)
      } else if (isTRUE(miss_result$success)) {
        status_msg <- sprintf(
          "Missing species loaded: %d downloaded, %d already cached, %d failed (%.1fs).",
          n_downloaded, n_skipped, n_failed, miss_result$duration_secs)
        showNotification(status_msg, type = "message", duration = 7)
      } else {
        status_msg <- sprintf(
          "Missing species load: %d downloaded, %d already cached, %d failed (%.1fs).",
          n_downloaded, n_skipped, n_failed, miss_result$duration_secs)
        showNotification(status_msg, type = "warning", duration = 7)
      }

      debug_log(sprintf("c3c missing species load: %s", status_msg), 1)
    })
  })

  # --------------------------------------------------------------------------
  # c3d. Load BioMart mapping database for current source/target pair
  #      Downloads all mapping tables (all key type combinations) for the
  #      currently selected source and target species so that subsequent
  #      mapping operations complete instantly from cache.
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_load_pair_db, {
    removeModal()

    src_sp <- isolate(input$species_annotation)
    tgt_sp <- isolate(input$target_species_annotation)
    debug_log(sprintf(
      "c3d: user chose Load Mapping Database for pair '%s' -> '%s'",
      src_sp %||% "NULL", tgt_sp %||% "NULL"), 1)

    if (is.null(src_sp) || is.null(tgt_sp)) {
      showNotification("No source or target species selected.", type = "warning", duration = 5)
      return()
    }

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(
      message = sprintf("Loading mapping database for %s -> %s...", src_sp, tgt_sp),
      value = 0, {

      pair_result <- build_mapping_tables_for_pair(
        source_species   = src_sp,
        target_species   = tgt_sp,
        debug_log        = debug_log,
        # build_mapping_tables_for_pair reports value in [0, 0.95];
        # scale by 0.9 to cap at ~0.85 and reserve the final 15% for the "Done" step.
        progress_callback = function(msg, value) {
          tryCatch(setProgress(value = if (is.numeric(value)) value * 0.9 else 0,
                               detail = msg),
                   error = function(e) NULL)
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      setProgress(value = 0.95, detail = "Done")

      if (pair_result$tables_downloaded == 0L && pair_result$tables_skipped > 0L) {
        status_msg <- sprintf(
          "All %d mapping tables already cached for %s -> %s. Nothing to download.",
          pair_result$tables_skipped, src_sp, tgt_sp)
        showNotification(status_msg, type = "message", duration = 5)
      } else if (isTRUE(pair_result$success)) {
        status_msg <- sprintf(
          "Mapping database loaded for %s -> %s: %d tables downloaded, %d already cached, %d failed (%.1fs).",
          src_sp, tgt_sp, pair_result$tables_downloaded,
          pair_result$tables_skipped, pair_result$tables_failed,
          pair_result$duration_secs)
        showNotification(status_msg, type = "message", duration = 7)
      } else {
        status_msg <- sprintf(
          "Mapping database load for %s -> %s: %d downloaded, %d cached, %d failed (%.1fs).",
          src_sp, tgt_sp, pair_result$tables_downloaded,
          pair_result$tables_skipped, pair_result$tables_failed,
          pair_result$duration_secs)
        showNotification(status_msg, type = "warning", duration = 7)
      }

      debug_log(sprintf("c3d load pair mapping db: %s", status_msg), 1)
    })
  })

  # --------------------------------------------------------------------------
  # c3e. Preload mapping databases for all default preset species pairs
  #      Downloads all mapping tables for all ordered combinations of the 11
  #      default preset species.  This is a long-running operation.
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_preload_all_defaults, {
    removeModal()
    debug_log("c3e: user chose Preload All Default Species Databases", 1)

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(
      message = "Preloading mapping tables for all default species pairs...",
      value = 0, {

      preset_result <- build_preset_mapping_tables(
        debug_log       = debug_log,
        # build_preset_mapping_tables already produces values in [0.01, 0.97];
        # no additional scaling needed — values map directly to the progress bar.
        progress_callback = function(value, detail) {
          tryCatch(setProgress(value = if (is.numeric(value)) value else 0,
                               detail = detail),
                   error = function(e) NULL)
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      setProgress(value = 0.98, detail = "Done")

      if (preset_result$total_downloaded == 0L && preset_result$total_skipped > 0L) {
        status_msg <- sprintf(
          "All mapping tables already cached for %d default species pairs (%d tables). Nothing to download.",
          preset_result$pairs_total, preset_result$total_skipped)
        showNotification(status_msg, type = "message", duration = 5)
      } else if (isTRUE(preset_result$success)) {
        status_msg <- sprintf(
          "Default species preload complete: %d/%d pairs processed, %d tables downloaded, %d cached, %d failed (%.1fs).",
          preset_result$pairs_processed, preset_result$pairs_total,
          preset_result$total_downloaded, preset_result$total_skipped,
          preset_result$total_failed, preset_result$duration_secs)
        showNotification(status_msg, type = "message", duration = 10)
      } else {
        status_msg <- sprintf(
          "Default species preload: %d/%d pairs, %d downloaded, %d cached, %d failed (%.1fs).",
          preset_result$pairs_processed, preset_result$pairs_total,
          preset_result$total_downloaded, preset_result$total_skipped,
          preset_result$total_failed, preset_result$duration_secs)
        showNotification(status_msg, type = "warning", duration = 10)
      }

      debug_log(sprintf("c3e preload all defaults: %s", status_msg), 1)
    })
  })

  # --------------------------------------------------------------------------
  # c3f. Download Missing Default Pairs Only
  #      Similar to c3e but skips species pairs that already have at least one
  #      cached mapping table on disk. Useful for filling gaps after a partial
  #      or interrupted preload.
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_preload_missing_defaults, {
    removeModal()
    debug_log("c3f: user chose Download Missing Default Pairs Only", 1)

    if (isTRUE(isolate(biomart_build_active()))) {
      showNotification(
        "A BioMart cache build is already in progress. Please wait.",
        type = "warning", duration = 5)
      return()
    }
    biomart_build_active(TRUE)
    on.exit(biomart_build_active(FALSE), add = TRUE)

    withProgress(
      message = "Downloading mapping tables for missing default species pairs...",
      value = 0, {

      missing_result <- build_missing_preset_mapping_tables(
        debug_log       = debug_log,
        progress_callback = function(value, detail) {
          tryCatch(setProgress(value = if (is.numeric(value)) value else 0,
                               detail = detail),
                   error = function(e) NULL)
        },
        abort_flag = NULL  # Cache operations are not user-abortable
      )

      setProgress(value = 0.98, detail = "Done")

      if (missing_result$pairs_skipped == missing_result$pairs_total) {
        status_msg <- sprintf(
          "All %d default species pairs already have cached mapping tables. Nothing to download.",
          missing_result$pairs_total)
        showNotification(status_msg, type = "message", duration = 5)
      } else if (missing_result$total_downloaded == 0L) {
        status_msg <- sprintf(
          "All mapping tables already cached for remaining %d pairs (%d pairs skipped entirely). Nothing to download.",
          missing_result$pairs_processed, missing_result$pairs_skipped)
        showNotification(status_msg, type = "message", duration = 5)
      } else if (isTRUE(missing_result$success)) {
        status_msg <- sprintf(
          "Missing pairs download complete: %d pairs processed, %d pairs skipped, %d tables downloaded, %d cached, %d failed (%.1fs).",
          missing_result$pairs_processed, missing_result$pairs_skipped,
          missing_result$total_downloaded, missing_result$total_skipped,
          missing_result$total_failed, missing_result$duration_secs)
        showNotification(status_msg, type = "message", duration = 10)
      } else {
        status_msg <- sprintf(
          "Missing pairs download: %d pairs processed, %d pairs skipped, %d downloaded, %d cached, %d failed (%.1fs).",
          missing_result$pairs_processed, missing_result$pairs_skipped,
          missing_result$total_downloaded, missing_result$total_skipped,
          missing_result$total_failed, missing_result$duration_secs)
        showNotification(status_msg, type = "warning", duration = 10)
      }

      debug_log(sprintf("c3f missing default pairs: %s", status_msg), 1)
    })
  })

  # --------------------------------------------------------------------------
  # c3g. Clear Keytype/Species Cache -- shows confirmation modal
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_clear_keytype_cache, {
    removeModal()
    showModal(modalDialog(
      title = "Confirm: Clear Species & ID Type Cache",
      tags$p(
        "This will delete the locally stored species list and all ID-type entries ",
        "from disk. The species and ID type dropdown menus will revert to the ",
        "built-in defaults until the cache is rebuilt."
      ),
      tags$p(
        "Mapping tables will ", tags$b("not"), " be affected."
      ),
      tags$p("Do you want to proceed?"),
      footer = tagList(
        actionButton(ns("biomart_clear_keytype_cache_confirm"),
                     "Yes, Clear Species & ID Type Cache",
                     class = "btn-default",
                     style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$biomart_clear_keytype_cache_confirm, {
    removeModal()
    debug_log("c3g: user confirmed clear keytype/species cache", 1)

    withProgress(message = "Clearing BioMart keytype/species cache...", value = 0, {
      incProgress(0.3, detail = "Removing species and keytype disk cache")
      tryCatch({
        invalidate_biomart_keytype_cache(debug_log = debug_log)
      }, error = function(e) {
        debug_log(paste("invalidate_biomart_keytype_cache failed:", e$message), 1)
        showNotification(
          paste("Could not clear keytype/species cache:", e$message),
          type = "error", duration = 7)
        return()
      })

      incProgress(0.7, detail = "Invalidating session cache")
      cached_biomart_species(NULL)
      cached_biomart_keytypes(list())

      incProgress(1.0, detail = "Done")

      showNotification(
        "BioMart species and keytype cache cleared. Mapping tables are intact.",
        type = "message", duration = 5)
    })
  })

  # --------------------------------------------------------------------------
  # c3h. Clear Database Cache -- shows confirmation modal
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_clear_database_cache, {
    removeModal()
    showModal(modalDialog(
      title = "Confirm: Clear Mapping Table Cache",
      tags$p(
        "This will delete all locally stored BioMart mapping tables from disk. ",
        "Any future ID mapping operation will need to re-download the required ",
        "tables from BioMart, which can take several minutes per species pair."
      ),
      tags$p(
        "The species and ID-type lists will ", tags$b("not"), " be affected."
      ),
      tags$p("Do you want to proceed?"),
      footer = tagList(
        actionButton(ns("biomart_clear_database_cache_confirm"),
                     "Yes, Clear Mapping Table Cache",
                     class = "btn-default",
                     style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$biomart_clear_database_cache_confirm, {
    removeModal()
    debug_log("c3h: user confirmed clear database cache", 1)

    withProgress(message = "Clearing BioMart mapping database cache...", value = 0, {
      incProgress(0.3, detail = "Removing mapping tables from disk")
      tryCatch({
        invalidate_biomart_database_cache(debug_log = debug_log)
      }, error = function(e) {
        debug_log(paste("invalidate_biomart_database_cache failed:", e$message), 1)
        showNotification(
          paste("Could not clear database cache:", e$message),
          type = "error", duration = 7)
        return()
      })

      incProgress(1.0, detail = "Done")

      showNotification(
        "BioMart mapping database cache cleared. Species and keytype cache are intact.",
        type = "message", duration = 5)
    })
  })

  # --------------------------------------------------------------------------
  # c3i. Clear All Cache -- shows confirmation modal
  # --------------------------------------------------------------------------

  observeEvent(input$biomart_clear_all_cache, {
    removeModal()
    showModal(modalDialog(
      title = "Confirm: Clear All BioMart Cache",
      tags$p(
        "This will delete the ", tags$b("entire"), " BioMart cache including:"
      ),
      tags$ul(
        tags$li("The species list and all keytype entries"),
        tags$li("All pre-downloaded mapping tables")
      ),
      tags$p(
        "Everything will need to be re-downloaded from scratch. ",
        "Depending on the amount of cached data, rebuilding the full cache ",
        "may take several hours."
      ),
      tags$p(tags$b("Do you really want to clear all cache?")),
      footer = tagList(
        actionButton(ns("biomart_clear_all_cache_confirm"),
                     "Yes, Clear All Cache",
                     class = "btn-default",
                     style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$biomart_clear_all_cache_confirm, {
    removeModal()
    debug_log("c3i: user confirmed clear all BioMart cache", 1)

    withProgress(message = "Clearing all BioMart cache...", value = 0, {
      incProgress(0.3, detail = "Removing all BioMart disk cache")
      tryCatch({
        invalidate_biomart_cache(debug_log = debug_log)
      }, error = function(e) {
        debug_log(paste("invalidate_biomart_cache failed:", e$message), 1)
        showNotification(
          paste("Could not clear BioMart cache:", e$message),
          type = "error", duration = 7)
        return()
      })

      incProgress(0.7, detail = "Invalidating session cache")
      cached_biomart_species(NULL)
      cached_biomart_keytypes(list())

      incProgress(1.0, detail = "Done")

      showNotification(
        "All BioMart cache cleared (species, keytypes, and mapping tables).",
        type = "message", duration = 5)
    })
  })

  # --------------------------------------------------------------------------
  # c4. Clear Cache button observer
  #     Mode-aware: in Annotation Hub mode clears the OrgDb disk cache for
  #     the currently selected species; in BioMart mode clears the BioMart
  #     species/keytype disk cache.  Session-level caches are invalidated
  #     in both paths so the next operation re-downloads or re-fetches as
  #     needed.
  # --------------------------------------------------------------------------

  observeEvent(input$clear_cache_annotation, {
    is_cross <- identical(isolate(input$annotation_strategy), "biomart")

    if (!is_cross) {
      # Annotation DB mode: clear the OrgDb disk cache for the selected species.
      species <- isolate(input$species_annotation)
      if (is.null(species) || !nzchar(species)) {
        showNotification("No species selected; nothing to clear.", type = "warning", duration = 5)
        return()
      }

      orgdb_name <- organism_to_orgdb(species)
      debug_log(paste("Clear cache requested for:", species, "->", orgdb_name), 1)

      withProgress(message = paste("Clearing cache for", species, "..."), value = 0, {

        incProgress(0.3, detail = "Removing organism disk cache")
        removed_count <- tryCatch({
          clear_organism_cache(orgdb_name, debug_log = debug_log)
        }, error = function(e) {
          debug_log(paste("clear_organism_cache failed:", e$message), 1)
          -1L
        })

        incProgress(0.7, detail = "Invalidating session cache")
        cached_org_db(NULL)
        cached_orgdb_name(NULL)
        cached_keytypes(NULL)
        keytype_last_organism(NULL)
        keytype_applied_orgdb(NULL)
        keytype_choices_applied(list())

        incProgress(1.0, detail = "Done")

        if (identical(removed_count, -1L)) {
          showNotification(
            paste("Could not clear cache for", species,
                  "(permission denied or unexpected error)."),
            type = "error", duration = 7
          )
        } else if (removed_count == 0) {
          showNotification(
            paste("No cache found for", species, "(nothing to clear)."),
            type = "warning", duration = 5
          )
        } else {
          showNotification(
            paste("Cache cleared for", species,
                  sprintf("(%d file(s) removed).", removed_count)),
            type = "message", duration = 5
          )
        }
      })

    } else {
      # BioMart mode: clear the BioMart species and keytype disk cache.
      debug_log("Clear cache requested for BioMart (cross-species mode)", 1)

      withProgress(message = "Clearing BioMart cache...", value = 0, {

        incProgress(0.3, detail = "Removing BioMart disk cache")
        tryCatch({
          invalidate_biomart_cache(debug_log = debug_log)
        }, error = function(e) {
          debug_log(paste("invalidate_biomart_cache failed:", e$message), 1)
          showNotification(
            paste("Could not clear BioMart cache (permission denied or unexpected error):",
                  e$message),
            type = "error", duration = 7
          )
          return()
        })

        incProgress(0.7, detail = "Invalidating session cache")
        cached_biomart_species(NULL)
        cached_biomart_keytypes(list())

        incProgress(1.0, detail = "Done")

        showNotification(
          "BioMart species and keytype cache cleared.",
          type = "message", duration = 5
        )
      })
    }
  })
}
