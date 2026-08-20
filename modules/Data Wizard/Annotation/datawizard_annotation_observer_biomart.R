# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_observer_biomart.R
#
# Purpose:
#   Contains cross-species / BioMart-specific observers for the Annotation
#   submodule: the cross-species toggle, target species keytype loading, and
#   source species keytype loading in BioMart mode.
#
# Architectural Role:
#   One of four concern-based observer files that together replace the former
#   monolithic datawizard_annotation_observer.R.  This file covers all
#   observers that manage the BioMart cross-species mode lifecycle: entering/
#   leaving cross-species mode and keeping source/target keytype dropdowns
#   synchronised with the BioMart cache.
#
#   Called indirectly: register_annotation_observers() (the thin entrypoint in
#   datawizard_annotation_observer.R) delegates to
#   register_annotation_observers_biomart() defined here.
#
# Responsibilities:
#   - Observer c1b: Cross-species toggle (entering/leaving BioMart mode).
#   - Observer c1c: Target species change (debounced, cross-species mode).
#   - Observer c1d: Source species change (debounced, cross-species mode).
#
# Integration Points / Dependencies:
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via destructured handles.
#   - Debounced species reactives created in the entrypoint and passed in.
#   - BioMart cache helpers from datawizard_annotation_utils_biomart_cache.R
#     and datawizard_annotation_utils_biomart_species.R.
#   - get_biomart_compatible_keytypes(), warm_biomart_session_cache() from
#     datawizard_annotation_utils.R.
#
# Maintenance Guidance:
#   - Keep this file focused on cross-species / BioMart mode observers.
#   - Cache management buttons (Update Organisms, Refresh, Clear) belong in
#     datawizard_annotation_observer_cache.R.
#   - Target: stay below 1000 lines.
# ==============================================================================


#' Register BioMart cross-species observers.
#'
#' @param input,output,session,ns  Shiny module objects.
#' @param state              Named list from create_annotation_state().
#' @param debug_log          Logging function.
#' @param DEBUG_LEVEL        Numeric debug verbosity.
#' @param species_src_debounced  Debounced reactive for source species.
#' @param species_tgt_debounced  Debounced reactive for target species.
register_annotation_observers_biomart <- function(input, output, session, ns,
                                                   state,
                                                   debug_log, DEBUG_LEVEL,
                                                   species_src_debounced,
                                                   species_tgt_debounced) {

  # -- Destructure state handles -----------------------------------------------
  cached_keytypes                 <- state$cached_keytypes
  cached_biomart_species          <- state$cached_biomart_species
  cached_biomart_keytypes         <- state$cached_biomart_keytypes
  pre_crossspecies_species        <- state$pre_crossspecies_species
  pre_crossspecies_source_choices <- state$pre_crossspecies_source_choices
  source_update_token             <- state$source_update_token
  target_update_token             <- state$target_update_token
  keytype_status_message          <- state$keytype_status_message

  # --------------------------------------------------------------------------
  # c1b. Strategy change observer (BioMart mode entry/exit)
  #      When BioMart strategy is selected: restricts source key types to
  #      BioMart-compatible subset, updates target species to BioMart-
  #      compatible species, and updates the shared target key type dropdown
  #      (to_keytype_annotation) with BioMart-compatible types for the
  #      target species. When another strategy is selected, restores all
  #      dropdowns to their full OrgDb/AnnotationHub sets.
  # --------------------------------------------------------------------------

  observeEvent(input$annotation_strategy, {
    tryCatch({
      is_cross <- identical(input$annotation_strategy, "biomart")
      current_from <- isolate(input$from_keytype_annotation)
      current_to   <- isolate(input$to_keytype_annotation)

      if (is_cross) {
        # --- Entering BioMart mode ---
        biomart_entry_t0 <- proc.time()[["elapsed"]]

        # Bump both tokens: this toggle triggers source and target keytype updates
        src_token <- isolate(source_update_token()) + 1L
        source_update_token(src_token)
        tgt_token <- isolate(target_update_token()) + 1L
        target_update_token(tgt_token)
        debug_log(sprintf("Cross-species toggle ON [src-token=%d, tgt-token=%d]",
                          src_token, tgt_token), 1)

        # Populate the session cache from the persistent cache before resolving
        # either dropdown.  This only reads local RDS files; it never refreshes
        # or rebuilds the disk cache.
        kt_cache <- warm_biomart_session_cache(
          species        = NULL,
          existing_cache = cached_biomart_keytypes(),
          debug_log      = debug_log
        )
        cached_biomart_keytypes(kt_cache)

        # 1. Save current species selection so we can restore on toggle-off
        pre_crossspecies_species(isolate(input$species_annotation))

        # 2. Load BioMart keytypes for the current source species (cache-first).
        #    Do NOT use AnnotationDB/OrgDb cached_keytypes() here: in BioMart
        #    mode the dropdown must reflect actual BioMart attributes, not a
        #    filtered subset of OrgDb keys.
        src_species    <- isolate(input$species_annotation)
        src_keytypes   <- NULL

        if (!is.null(src_species) && nzchar(src_species)) {
          debug_log(paste("Cross-species toggle ON: loading BioMart keytypes for source species:", src_species), 1)

          # Mode entry is deliberately network-free: session -> disk -> fallback.
          src_keytypes <- cached_biomart_keytypes()[[src_species]]
          if (!is.null(src_keytypes) && length(src_keytypes) > 0) {
            debug_log("BioMart mode entry: source keytypes session cache HIT", 1)
          } else {
            src_keytypes <- load_biomart_keytypes_cache(src_species,
                                                        debug_log = debug_log)
            if (!is.null(src_keytypes) && length(src_keytypes) > 0) {
              debug_log("BioMart mode entry: source keytypes disk cache HIT", 1)
            } else {
              debug_log("BioMart mode entry: source keytypes cache MISS, using fallback (no live fetch)", 1)
            }
          }

          # Update session keytype cache
          if (!is.null(src_keytypes) && length(src_keytypes) > 0) {
            kt_cache <- cached_biomart_keytypes()
            kt_cache[[src_species]] <- src_keytypes
            cached_biomart_keytypes(kt_cache)
          }
        }

        # Fall back to BioMart-compatible subset of OrgDb keytypes when BioMart
        # fetch failed (offline / species not in BioMart).
        if (is.null(src_keytypes) || length(src_keytypes) == 0) {
          debug_log("Cross-species toggle ON: using OrgDb-compatible fallback for source keytypes", 1)
          full_keytypes <- cached_keytypes()
          if (is.null(full_keytypes) || length(full_keytypes) == 0) {
            full_keytypes <- c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")
          }
          src_keytypes <- get_biomart_compatible_keytypes(full_keytypes)
        }

        compatible <- get_biomart_compatible_keytypes(src_keytypes)
        if (length(compatible) == 0) {
          compatible <- c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")
        }
        selected_from <- if (!is.null(current_from) && current_from %in% compatible) {
          current_from
        } else if ("SYMBOL" %in% compatible) {
          "SYMBOL"
        } else {
          compatible[1]
        }
        updateSelectInput(session, "from_keytype_annotation",
                          choices = compatible, selected = selected_from)

        # 3. Load BioMart species list (session -> disk -> static fallback).
        #    Cache is persistent (no TTL) -- only refreshed on explicit Update Organisms.
        biomart_df <- cached_biomart_species()

        if (!is.null(biomart_df) && is.data.frame(biomart_df) &&
            nrow(biomart_df) > 0 &&
            all(c("scientific_name", "dataset") %in% names(biomart_df))) {
          debug_log("BioMart mode entry: species session cache HIT", 1)
        } else {
          # Try disk cache first (persistent, no TTL)
          biomart_df <- load_biomart_species_cache(debug_log = debug_log)
          if (!is.null(biomart_df) && is.data.frame(biomart_df) &&
              nrow(biomart_df) > 0 &&
              all(c("scientific_name", "dataset") %in% names(biomart_df))) {
            debug_log("BioMart mode entry: species disk cache HIT", 1)
          } else {
            biomart_df <- BIOMART_SPECIES_FALLBACK
            debug_log("BioMart mode entry: species cache MISS, using built-in fallback (no live fetch)", 1)
          }
        }

        # Store in session cache
        cached_biomart_species(biomart_df)

        # -- Stale check before applying the locally resolved species list --
        if (!identical(input$annotation_strategy, "biomart") ||
            !identical(isolate(source_update_token()), src_token)) {
          debug_log(sprintf("BioMart mode [src-token=%d]: Stale after species cache lookup, discarding",
                            src_token), 1)
          return()
        }

        # On toggle-ON show only the default preset subset so that both BioMart
        # and AnnotationHub modes start with the same species list.  The full
        # BioMart list is only exposed after the user clicks "Update Organisms".
        # Filter the preset to species confirmed present in the fetched data;
        # fall back to the full preset when the fetch returned the static fallback.
        available_names <- unique(biomart_df$scientific_name)
        preset_available <- ANNOTATION_DEFAULT_SPECIES_PRESET[
          ANNOTATION_DEFAULT_SPECIES_PRESET %in% available_names
        ]
        display_species <- if (length(preset_available) > 0) {
          preset_available
        } else {
          ANNOTATION_DEFAULT_SPECIES_PRESET
        }
        biomart_choices <- stats::setNames(display_species, display_species)

        current_target_sp <- isolate(input$target_species_annotation)
        if (!is.null(current_target_sp) && current_target_sp %in% biomart_choices) {
          target_sp_selected <- current_target_sp
          debug_log(sprintf("GUARD [c1b-toggle-ON]: preserving target species '%s' (valid in BioMart preset)", current_target_sp), 2)
          updateSelectInput(session, "target_species_annotation",
                            choices = biomart_choices)
        } else {
          target_sp_selected <- if ("Mus musculus" %in% biomart_choices) {
            "Mus musculus"
          } else {
            biomart_choices[1]
          }
          debug_log(sprintf("GUARD [c1b-toggle-ON]: target species fallback '%s' -> '%s' (not in BioMart preset)",
                            current_target_sp, target_sp_selected), 1)
          updateSelectInput(session, "target_species_annotation",
                            choices = biomart_choices, selected = target_sp_selected)
        }

        # 3b. Update SOURCE species dropdown with the same list so both pools
        #     are identical in cross-species mode.
        current_src_sp <- isolate(input$species_annotation)
        if (!is.null(current_src_sp) && current_src_sp %in% biomart_choices) {
          src_sp_selected <- current_src_sp
          debug_log(sprintf("GUARD [c1b-toggle-ON]: preserving source species '%s' (valid in BioMart preset)", current_src_sp), 2)
          updateSelectInput(session, "species_annotation",
                            choices = biomart_choices)
        } else {
          src_sp_selected <- if ("Homo sapiens" %in% biomart_choices) {
            "Homo sapiens"
          } else {
            biomart_choices[1]
          }
          debug_log(sprintf("GUARD [c1b-toggle-ON]: source species fallback '%s' -> '%s' (not in BioMart preset)",
                            current_src_sp, src_sp_selected), 1)
          updateSelectInput(session, "species_annotation",
                            choices = biomart_choices, selected = src_sp_selected)
        }

        # 4. Resolve target keytypes locally (session -> disk -> fallback).
        target_keytypes <- cached_biomart_keytypes()[[target_sp_selected]]
        if (!is.null(target_keytypes) && length(target_keytypes) > 0) {
          debug_log("BioMart mode entry: target keytypes session cache HIT", 1)
        } else {
          target_keytypes <- load_biomart_keytypes_cache(target_sp_selected,
                                                         debug_log = debug_log)
          if (!is.null(target_keytypes) && length(target_keytypes) > 0) {
            debug_log("BioMart mode entry: target keytypes disk cache HIT", 1)
          } else {
            debug_log("BioMart mode entry: target keytypes cache MISS, using fallback (no live fetch)", 1)
          }
        }
        if (is.null(target_keytypes) || length(target_keytypes) == 0) {
          target_keytypes <- get_biomart_compatible_keytypes(NULL)
        }

        # -- Stale check before applying target keytypes --
        if (!identical(input$annotation_strategy, "biomart") ||
            !identical(isolate(target_update_token()), tgt_token)) {
          debug_log(sprintf("BioMart mode [tgt-token=%d]: Stale before target keytype apply, discarding",
                            tgt_token), 1)
          return()
        }

        # Update session keytype cache
        kt_cache <- cached_biomart_keytypes()
        kt_cache[[target_sp_selected]] <- target_keytypes
        cached_biomart_keytypes(kt_cache)

        selected_to <- if (!is.null(current_to) && current_to %in% target_keytypes) {
          current_to
        } else if ("ENSEMBL" %in% target_keytypes) {
          "ENSEMBL"
        } else {
          target_keytypes[1]
        }
        updateSelectInput(session, "to_keytype_annotation",
                          choices = target_keytypes, selected = selected_to)

        debug_log(sprintf(
          "Cross-species enabled: %d species (scientific names), %d source keytypes, %d target keytypes",
          length(biomart_choices), length(compatible), length(target_keytypes)), 2)
        debug_log(sprintf("BioMart mode entry completed in %.3fs (network-free)",
                          proc.time()[["elapsed"]] - biomart_entry_t0), 1)

      } else {
        # --- Leaving BioMart mode ---

        # Restore full OrgDb keytypes for both source and target
        full_keytypes <- cached_keytypes()
        if (!is.null(full_keytypes) && length(full_keytypes) > 0) {
          selected <- if (!is.null(current_from) && current_from %in% full_keytypes) {
            current_from
          } else if ("SYMBOL" %in% full_keytypes) {
            "SYMBOL"
          } else {
            full_keytypes[1]
          }
          updateSelectInput(session, "from_keytype_annotation",
                            choices = full_keytypes, selected = selected)

          selected_to <- if (!is.null(current_to) && current_to %in% full_keytypes) {
            current_to
          } else if ("ENSEMBL" %in% full_keytypes) {
            "ENSEMBL"
          } else {
            full_keytypes[1]
          }
          updateSelectInput(session, "to_keytype_annotation",
                            choices = full_keytypes, selected = selected_to)

          debug_log(sprintf("BioMart mode disabled: restored %d full OrgDb keytypes",
                            length(full_keytypes)), 2)
        }

        # Restore source species dropdown from pre-BioMart snapshot.
        # Prefer user's current selection if valid in the restored choices.
        saved_choices  <- pre_crossspecies_source_choices()
        saved_selected <- pre_crossspecies_species()
        if (!is.null(saved_choices) && length(saved_choices) > 0) {
          current_src_sp <- isolate(input$species_annotation)
          if (!is.null(current_src_sp) && current_src_sp %in% saved_choices) {
            # User's current selection is valid in restored choices - preserve it
            debug_log(sprintf("GUARD [c1b-strategy-off]: preserving user's current source species '%s'", current_src_sp), 2)
            updateSelectInput(session, "species_annotation",
                              choices = saved_choices)
          } else {
            # Fall back to pre-BioMart snapshot or default
            restore_selected <- if (!is.null(saved_selected) && saved_selected %in% saved_choices) {
              saved_selected
            } else if ("Homo sapiens" %in% saved_choices) {
              "Homo sapiens"
            } else {
              saved_choices[1]
            }
            debug_log(sprintf("GUARD [c1b-strategy-off]: source species fallback '%s' -> '%s' (not in restored choices)",
                              current_src_sp, restore_selected), 1)
            updateSelectInput(session, "species_annotation",
                              choices = saved_choices, selected = restore_selected)
          }
          debug_log(sprintf("BioMart mode disabled: restored %d source species choices",
                            length(saved_choices)), 2)
        }
      }
    }, error = function(e) {
      debug_log(paste("Error in strategy change observer:", e$message), 1)
      showNotification(
        paste("Error updating annotation strategy:", e$message),
        type = "error", duration = 5
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # c1c. Target species change observer (debounced, BioMart mode)
  #      When the target species changes, updates the shared
  #      to_keytype_annotation dropdown with BioMart-compatible key types
  #      available for that target species.
  #      Uses debounced input to coalesce rapid switches and session-level
  #      cache to avoid redundant disk/network I/O.
  # --------------------------------------------------------------------------

  observe({
    target_species <- species_tgt_debounced()
    req(target_species, nzchar(target_species))

    isolate({
    if (!identical(input$annotation_strategy, "biomart")) return()

    # -- Request versioning: increment target token --
    my_token <- target_update_token() + 1L
    target_update_token(my_token)
    t0 <- proc.time()[["elapsed"]]

    tryCatch({
      debug_log(sprintf("Target species changed (debounced) [tgt-token=%d]: %s",
                        my_token, target_species), 1)
      keytype_status_message(paste("Loading target keytypes for", target_species, "..."))

      withProgress(message = paste("Updating target key types for", target_species), value = 0.1, {

        # 1. Session cache lookup (fastest)
        incProgress(0.1, detail = "Checking session cache...")
        kt_session <- cached_biomart_keytypes()
        target_keytypes <- kt_session[[target_species]]

        if (!is.null(target_keytypes) && length(target_keytypes) > 0) {
          debug_log(sprintf("Target keytypes [tgt-token=%d]: Session cache HIT for %s (%d types, elapsed=%.3fs)",
                            my_token, target_species, length(target_keytypes),
                            proc.time()[["elapsed"]] - t0), 1)
        } else {
          # 2. Disk cache lookup (persistent, no TTL)
          incProgress(0.1, detail = "Checking disk cache...")
          keytype_status_message(paste("Checking disk cache for", target_species, "..."))
          target_keytypes <- load_biomart_keytypes_cache(target_species,
                                                           debug_log = debug_log)

          # -- Stale check after disk cache read --
          if (!identical(target_update_token(), my_token)) {
            debug_log(sprintf("Target keytypes [tgt-token=%d]: STALE after cache check (current=%d) -- discarding",
                              my_token, target_update_token()), 1)
            keytype_status_message(sprintf("Discarded stale target request for %s", target_species))
            return()
          }

          if (!is.null(target_keytypes)) {
            debug_log(sprintf("Target keytypes [tgt-token=%d]: Disk cache HIT for %s (%d types)",
                              my_token, target_species, length(target_keytypes)), 1)
          } else {
            # 3. BioMart live fetch
            incProgress(0.2, detail = paste("Fetching from BioMart for", target_species))
            keytype_status_message(paste("Fetching keytypes from BioMart for", target_species, "..."))
            t_fetch <- proc.time()[["elapsed"]]
            target_keytypes <- tryCatch(
              fetch_biomart_keytypes_for_species(target_species, debug_log = debug_log),
              error = function(e) {
                debug_log(sprintf("Live keytype fetch failed for %s: %s",
                                  target_species, e$message), 1)
                get_biomart_compatible_keytypes(NULL)
              }
            )
            debug_log(sprintf("Target keytypes [tgt-token=%d]: BioMart fetch took %.3fs",
                              my_token, proc.time()[["elapsed"]] - t_fetch), 2)

            # -- Stale check after fetch --
            if (!identical(target_update_token(), my_token)) {
              debug_log(sprintf("Target keytypes [tgt-token=%d]: STALE after fetch (current=%d) -- discarding",
                                my_token, target_update_token()), 1)
              keytype_status_message(sprintf("Discarded stale target request for %s", target_species))
              return()
            }

            if (!is.null(target_keytypes) && length(target_keytypes) > 0) {
              save_biomart_keytypes_cache(target_species, target_keytypes,
                                           debug_log = debug_log)
            }
          }
        }

        if (is.null(target_keytypes) || length(target_keytypes) == 0) {
          target_keytypes <- get_biomart_compatible_keytypes(NULL)
        }

        # -- Final stale check before UI apply --
        if (!identical(target_update_token(), my_token) ||
            !identical(input$target_species_annotation, target_species)) {
          debug_log(sprintf("Target keytypes [tgt-token=%d]: STALE before UI apply -- discarding",
                            my_token), 1)
          keytype_status_message(sprintf("Discarded stale target request for %s", target_species))
          return()
        }

        # Update session keytype cache
        kt_cache <- cached_biomart_keytypes()
        kt_cache[[target_species]] <- target_keytypes
        cached_biomart_keytypes(kt_cache)

        incProgress(0.3, detail = "Updating keytype dropdown...")
        keytype_status_message(paste("Applying target keytypes for", target_species, "..."))

        current_to <- input$to_keytype_annotation
        selected <- if (!is.null(current_to) && current_to %in% target_keytypes) {
          current_to
        } else if ("ENSEMBL" %in% target_keytypes) {
          "ENSEMBL"
        } else {
          target_keytypes[1]
        }

        updateSelectInput(session, "to_keytype_annotation",
                          choices = target_keytypes, selected = selected)

        incProgress(0.1, detail = "Finalizing UI state")
        keytype_status_message(NULL)
        debug_log(sprintf("Target keytypes [tgt-token=%d]: Applied %d types for %s (total=%.3fs)",
                          my_token, length(target_keytypes), target_species,
                          proc.time()[["elapsed"]] - t0), 1)

      }) # end withProgress

    }, error = function(e) {
      debug_log(paste("Error updating target keytypes:", e$message), 1)
      keytype_status_message(NULL)
    })
    }) # end isolate
  })

  # --------------------------------------------------------------------------
  # c1d. Source species change observer (debounced, BioMart mode)
  #      When the source species changes while BioMart mode is active, updates
  #      from_keytype_annotation with the BioMart-compatible keytypes available
  #      for that source species.  Keytypes are loaded from session cache,
  #      disk cache, or BioMart live fetch (in that order).
  #      Uses debounced input to coalesce rapid switches.
  # --------------------------------------------------------------------------

  observe({
    src_species <- species_src_debounced()
    req(src_species, nzchar(src_species))

    isolate({
    if (!identical(input$annotation_strategy, "biomart")) return()

    # -- Request versioning: increment source token --
    my_token <- source_update_token() + 1L
    source_update_token(my_token)
    t0 <- proc.time()[["elapsed"]]

    tryCatch({
      debug_log(sprintf("Source species changed (debounced, cross-species) [src-token=%d]: %s",
                        my_token, src_species), 1)
      keytype_status_message(paste("Loading source keytypes for", src_species, "..."))

      withProgress(message = paste("Updating source key types for", src_species), value = 0.1, {

        # 1. Session cache lookup (fastest)
        incProgress(0.1, detail = "Checking session cache...")
        kt_session <- cached_biomart_keytypes()
        src_keytypes <- kt_session[[src_species]]

        if (!is.null(src_keytypes) && length(src_keytypes) > 0) {
          debug_log(sprintf("Source keytypes (cross) [src-token=%d]: Session cache HIT for %s (%d types, elapsed=%.3fs)",
                            my_token, src_species, length(src_keytypes),
                            proc.time()[["elapsed"]] - t0), 1)
        } else {
          # 2. Disk cache lookup (persistent, no TTL)
          incProgress(0.1, detail = "Checking disk cache...")
          keytype_status_message(paste("Checking disk cache for", src_species, "..."))
          src_keytypes <- load_biomart_keytypes_cache(src_species,
                                                       debug_log = debug_log)

          # -- Stale check after disk cache read --
          if (!identical(source_update_token(), my_token)) {
            debug_log(sprintf("Source keytypes (cross) [src-token=%d]: STALE after cache check (current=%d) -- discarding",
                              my_token, source_update_token()), 1)
            keytype_status_message(sprintf("Discarded stale source request for %s", src_species))
            return()
          }

          if (!is.null(src_keytypes)) {
            debug_log(sprintf("Source keytypes (cross) [src-token=%d]: Disk cache HIT for %s (%d types)",
                              my_token, src_species, length(src_keytypes)), 1)
          } else {
            # 3. BioMart live fetch
            incProgress(0.2, detail = paste("Fetching from BioMart for", src_species))
            keytype_status_message(paste("Fetching keytypes from BioMart for", src_species, "..."))
            t_fetch <- proc.time()[["elapsed"]]
            src_keytypes <- tryCatch(
              fetch_biomart_keytypes_for_species(src_species, debug_log = debug_log),
              error = function(e) {
                debug_log(sprintf("Live keytype fetch failed for source %s: %s",
                                  src_species, e$message), 1)
                get_biomart_compatible_keytypes(NULL)
              }
            )
            debug_log(sprintf("Source keytypes (cross) [src-token=%d]: BioMart fetch took %.3fs",
                              my_token, proc.time()[["elapsed"]] - t_fetch), 2)

            # -- Stale check after fetch --
            if (!identical(source_update_token(), my_token)) {
              debug_log(sprintf("Source keytypes (cross) [src-token=%d]: STALE after fetch (current=%d) -- discarding",
                                my_token, source_update_token()), 1)
              keytype_status_message(sprintf("Discarded stale source request for %s", src_species))
              return()
            }

            if (!is.null(src_keytypes) && length(src_keytypes) > 0) {
              save_biomart_keytypes_cache(src_species, src_keytypes, debug_log = debug_log)
            }
          }
        }

        if (is.null(src_keytypes) || length(src_keytypes) == 0) {
          src_keytypes <- get_biomart_compatible_keytypes(NULL)
        }

        # Restrict to BioMart-compatible filter types
        compatible <- get_biomart_compatible_keytypes(src_keytypes)
        if (length(compatible) == 0) {
          compatible <- c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")
        }

        # -- Final stale check before UI apply --
        if (!identical(source_update_token(), my_token) ||
            !identical(input$species_annotation, src_species)) {
          debug_log(sprintf("Source keytypes (cross) [src-token=%d]: STALE before UI apply -- discarding",
                            my_token), 1)
          keytype_status_message(sprintf("Discarded stale source request for %s", src_species))
          return()
        }

        # Update session keytype cache
        kt_cache <- cached_biomart_keytypes()
        kt_cache[[src_species]] <- src_keytypes
        cached_biomart_keytypes(kt_cache)

        incProgress(0.3, detail = "Updating keytype dropdown...")
        keytype_status_message(paste("Applying source keytypes for", src_species, "..."))

        current_from <- input$from_keytype_annotation
        selected <- if (!is.null(current_from) && current_from %in% compatible) {
          current_from
        } else if ("SYMBOL" %in% compatible) {
          "SYMBOL"
        } else {
          compatible[1]
        }

        updateSelectInput(session, "from_keytype_annotation",
                          choices = compatible, selected = selected)

        incProgress(0.1, detail = "Finalizing UI state")
        keytype_status_message(NULL)
        debug_log(sprintf("Source keytypes (cross) [src-token=%d]: Applied %d BioMart-compatible types for %s (total=%.3fs)",
                          my_token, length(compatible), src_species,
                          proc.time()[["elapsed"]] - t0), 1)

      }) # end withProgress

    }, error = function(e) {
      debug_log(paste("Error updating source keytypes (cross):", e$message), 1)
      keytype_status_message(NULL)
    })
    }) # end isolate
  })
}
