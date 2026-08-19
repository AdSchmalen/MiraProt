# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_observer_mapping.R
#
# Purpose:
#   Contains the ID-mapping execution observer for the Annotation submodule:
#   the run-annotation (Map IDs) button handler.
#
# Architectural Role:
#   One of four concern-based observer files that together replace the former
#   monolithic datawizard_annotation_observer.R.  This file covers the core
#   mapping workflow: input validation, intra-species and cross-species
#   mapping dispatch, result column creation, and data write-back.
#
#   Called indirectly: register_annotation_observers() (the thin entrypoint in
#   datawizard_annotation_observer.R) delegates to
#   register_annotation_observers_mapping() defined here.
#
# Responsibilities:
#   - Observer d:  Run annotation / Map IDs button (validates inputs,
#     dispatches to map_ids_intraspecies or map_ids_crossspecies, creates
#     the result column, writes back via set_data, and stores the mapping
#     summary in last_mapping_result).
#
# Integration Points / Dependencies:
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via destructured handles.
#   - get_data / set_data / data_def callbacks from the orchestrator.
#   - Mapping helpers from datawizard_annotation_utils.R:
#     map_ids_intraspecies(), add_annotation_column(),
#     build_annotation_col_name(), get_annotation_cache_date().
#   - Cross-species helpers from datawizard_annotation_utils_biomart_mapping.R:
#     map_ids_crossspecies(), validate_biomart_compatibility(),
#     orgdb_keytype_to_biomart_attr().
#   - OrgDb loading: load_annotation_hub_with_progress(), organism_to_orgdb(),
#     get_organism_cache_dir(), .read_cache_metadata().
#
# Maintenance Guidance:
#   - Keep this file focused on the actual mapping execution path.
#   - If new mapping strategies are added, extend observer d here.
#   - Target: stay below 1000 lines.
#
# withProgress Scoping Constraint:
#   All variable assignments inside withProgress() MUST use <- (plain
#   assignment), NOT <<-.  See the inline SCOPING NOTE in observer d for
#   the full explanation.  This was the root cause of the cross-species
#   mapping regression in PR 312.
# ==============================================================================


#' Register mapping-execution annotation observers.
#'
#' @param input,output,session,ns  Shiny module objects.
#' @param state              Named list from create_annotation_state().
#' @param get_data           Function returning the current data frame.
#' @param set_data           Function to write back a modified data frame.
#' @param data_def           Reactive returning the metadata data frame.
#' @param debug_log          Logging function.
#' @param DEBUG_LEVEL        Numeric debug verbosity.
register_annotation_observers_mapping <- function(input, output, session, ns,
                                                   state, get_data, set_data,
                                                   data_def,
                                                   debug_log, DEBUG_LEVEL) {

  # -- Destructure state handles -----------------------------------------------
  last_mapping_result      <- state$last_mapping_result
  cached_org_db            <- state$cached_org_db
  cached_orgdb_name        <- state$cached_orgdb_name
  biomart_table_env        <- state$biomart_table_env
  stale_cache_accepted     <- state$stale_cache_accepted


  # --------------------------------------------------------------------------
  # d. Run annotation button observer
  #    Validates inputs, performs ID mapping, adds new column, and writes
  #    back via set_data().
  # --------------------------------------------------------------------------

  observeEvent(input$run_annotation, {
    debug_log("Map IDs button clicked", 1)

    tryCatch({
      data <- tryCatch(get_data(), error = function(e) NULL)
      def  <- tryCatch(data_def(),  error = function(e) NULL)
      req(!is.null(data), !is.null(def))

      source_col        <- input$source_column_annotation
      species           <- input$species_annotation
      from_keytype      <- input$from_keytype_annotation
      to_keytype        <- input$to_keytype_annotation
      cross_species     <- identical(input$annotation_strategy, "biomart")
      collapse_strategy <- input$collapse_strategy_annotation

      # Validate inputs
      if (is.null(source_col) || !source_col %in% names(data)) {
        showNotification("Please select a valid source column.", type = "error")
        return(NULL)
      }

      if (is.null(from_keytype) || is.null(to_keytype)) {
        showNotification("Please select source and target ID types.", type = "error")
        return(NULL)
      }

      if (!cross_species && from_keytype == to_keytype) {
        showNotification("Source and target ID types are the same.", type = "warning")
        return(NULL)
      }

      # Extract unique source IDs.
      # Cells may contain multiple identifiers separated by ";" or ","
      # (with optional surrounding whitespace).  Each cell is split into
      # individual tokens so that every token is queried independently.
      # Single-ID cells produce a length-1 token vector, preserving the
      # existing behaviour exactly.
      raw_cell_values <- na.omit(as.character(data[[source_col]]))
      source_ids <- unique(unlist(lapply(raw_cell_values, split_identifier_cell)))
      source_ids <- source_ids[nzchar(source_ids)]

      if (length(source_ids) == 0) {
        showNotification("Source column contains no valid identifiers.", type = "error")
        return(NULL)
      }

      debug_log(sprintf("Starting mapping: %d unique IDs from '%s'",
                        length(source_ids), source_col), 1)

      # Perform mapping inside withProgress for real-time progress bars.
      #
      # SCOPING NOTE: all assignments inside the withProgress expression MUST
      # use <- (plain assignment), NOT <<-.  Shiny evaluates withProgress via
      # eval(substitute(expr), envir = parent.frame()), which makes the calling
      # frame the current environment.  <- therefore writes directly to this
      # observer's local frame.  <<- would search the *parent* of that frame,
      # skipping the local declarations here, leaving mapped_values and
      # target_col_name NULL after withProgress returns (regression from PR 312).
      #
      # EARLY-EXIT NOTE: return() inside withProgress only exits the withProgress
      # eval context, not this observer.  Pre-mapping validation failures are
      # therefore signalled via mapping_cancelled <- TRUE instead of return(),
      # and checked explicitly after withProgress.
      mapped_values     <- NULL
      target_col_name   <- NULL
      mapping_cancelled <- FALSE

      withProgress(
        message = sprintf("Mapping %d identifiers...", length(source_ids)),
        value = 0, {

        # Progress callback for BioMart strategies
        progress_cb <- function(msg, value) {
          if (!is.null(value)) {
            setProgress(value = value, detail = msg)
          } else {
            incProgress(0, detail = msg)
          }
        }

        if (cross_species) {
          # Cross-species via biomaRt
          target_species <- input$target_species_annotation

          if (is.null(target_species) || is.null(to_keytype)) {
            showNotification("Please select target species and target ID type.", type = "error")
            mapping_cancelled <- TRUE
          } else {
            # Validate compatibility before querying BioMart.
            target_biomart_attr    <- orgdb_keytype_to_biomart_attr(to_keytype)
            validation_target_attr <- if (!is.null(target_biomart_attr)) target_biomart_attr
                                      else "ensembl_gene_id"

            incProgress(0.02, detail = "Validating BioMart compatibility...")

            validation <- validate_biomart_compatibility(
              source_species = species,
              target_species = target_species,
              source_keytype = from_keytype,
              target_attr    = validation_target_attr,
              debug_log      = debug_log
            )

            if (!validation$valid) {
              showNotification(validation$error, type = "error", duration = 8)
              mapping_cancelled <- TRUE
            } else {
              source_attr <- validation$source_attr

              debug_log(sprintf(
                "Cross-species BioMart mapping: %d IDs, %s -> %s, source_attr: %s, target_keytype: %s",
                length(source_ids), species, target_species, source_attr, to_keytype), 1)

              mapped_values <- map_ids_crossspecies(
                ids               = source_ids,
                source_species    = species,
                target_species    = target_species,
                source_attr       = source_attr,
                target_keytype    = to_keytype,
                collapse_strategy = collapse_strategy,
                abort_flag        = NULL,
                progress_callback = progress_cb,
                table_cache       = biomart_table_env,
                ignore_ttl        = identical(
                  isolate(stale_cache_accepted())[[organism_to_orgdb(target_species)]], "use_old"
                ),
                debug_log         = debug_log
              )

              target_col_name <- build_annotation_col_name(
                mode           = "cross",
                source_species = species,
                from_keytype   = from_keytype,
                to_keytype     = to_keytype,
                target_species = target_species
              )

              debug_log(sprintf(
                "Cross-species mapping result assigned: length %d (%d non-NA), target_col_name: '%s'",
                length(mapped_values), sum(!is.na(mapped_values)), target_col_name), 1)
            }
          }

        } else {
          # Intra-species via OrgDb
          orgdb_name <- organism_to_orgdb(species)

          incProgress(0.05, detail = "Loading organism database...")

          # Cache decision summary: log the state of every cache layer before
          # the load sequence starts so that the chosen action is traceable.
          tryCatch({
            cache_dir   <- get_organism_cache_dir(orgdb_name)

            # Read cache metadata (new format) or fall back to legacy files
            meta <- .read_cache_metadata(orgdb_name, debug_log = debug_log)

            cache_age_str <- if (!is.null(meta) && !is.null(meta$updated)) {
              ts_val <- tryCatch(as.POSIXct(meta$updated), error = function(e) NA)
              if (!is.na(ts_val))
                sprintf("%.1f days", as.numeric(difftime(Sys.time(), ts_val, units = "days")))
              else "unreadable"
            } else {
              ts_file <- file.path(cache_dir, "cache_timestamp.txt")
              if (file.exists(ts_file)) {
                ts_val <- tryCatch(as.POSIXct(readLines(ts_file)[1]), error = function(e) NA)
                if (!is.na(ts_val))
                  sprintf("%.1f days (legacy)", as.numeric(difftime(Sys.time(), ts_val, units = "days")))
                else "unreadable"
              } else {
                "absent"
              }
            }

            cache_format <- if (!is.null(meta)) {
              paste0(meta$cache_status, " (sqlite: ",
                     if (!is.null(meta$sqlite_path) && nzchar(meta$sqlite_path) &&
                         file.exists(meta$sqlite_path)) "present" else "missing",
                     ")")
            } else {
              marker_file <- file.path(cache_dir, "organism_db.rds")
              if (file.exists(marker_file)) "legacy_marker" else "absent"
            }

            in_session <- !is.null(cached_org_db()) && identical(cached_orgdb_name(), orgdb_name)

            chosen_action <- if (in_session) {
              "use_session_cache"
            } else if (!is.null(meta) && identical(meta$cache_status, "valid") &&
                       !is.null(meta$sqlite_path) && file.exists(meta$sqlite_path)) {
              "reconstruct_from_sqlite"
            } else if (cache_format == "absent") {
              "first_download"
            } else {
              "load_from_annotationhub"
            }

            debug_log(sprintf(
              "[OrgDb cache decision] orgdb=%s | age=%s | format=%s | session_cache=%s | action=%s",
              orgdb_name, cache_age_str, cache_format, in_session, chosen_action), 1)
          }, error = function(e) {
            debug_log(paste("Cache decision summary error:", e$message), 2)
          })

          # Load OrgDb (from session cache or disk cache)
          org_db <- cached_org_db()

          if (is.null(org_db) || !identical(cached_orgdb_name(), orgdb_name)) {
            tryCatch({
              # load_annotation_hub_with_progress handles organism cache check,
              # marker routing, and download internally -- no need to pre-call
              # load_organism_cache() here (that would be a redundant second call).
              # Honour any stale-cache decision the user made in this session.
              user_ignore_ttl <- identical(
                isolate(stale_cache_accepted())[[orgdb_name]], "use_old"
              )
              org_db <- load_annotation_hub_with_progress(
                species, debug_log = debug_log,
                ignore_ttl = user_ignore_ttl
              )
            }, error = function(e) {
              debug_log(paste("AnnotationHub download failed:", e$message), 1)
            })

            if (!is.null(org_db)) {
              cached_org_db(org_db)
              cached_orgdb_name(orgdb_name)
            }
          }

          if (is.null(org_db)) {
            showNotification(
              "Could not load organism database. Try using the GO module first to cache the database.",
              type = "error", duration = 10
            )
            mapping_cancelled <- TRUE
          } else {
            incProgress(0.1, detail = "Performing intra-species mapping...")

            mapped_values <- map_ids_intraspecies(
              ids               = source_ids,
              org_db            = org_db,
              from_keytype      = from_keytype,
              to_keytype        = to_keytype,
              collapse_strategy = collapse_strategy,
              debug_log         = debug_log
            )

            target_col_name <- build_annotation_col_name(
              mode           = "intra",
              source_species = species,
              from_keytype   = from_keytype,
              to_keytype     = to_keytype
            )
          }
        }

        if (!isTRUE(mapping_cancelled)) {
          setProgress(1, detail = "Mapping complete")
        }
      })

      # Exit cleanly when a pre-mapping check cancelled the operation.
      if (isTRUE(mapping_cancelled)) {
        return(NULL)
      }

      if (is.null(mapped_values) || length(mapped_values) == 0) {
        showNotification("Mapping returned no results.", type = "warning")
        return(NULL)
      }

      # Check for BioMart-specific connection/dataset errors
      biomart_err <- attr(mapped_values, "biomart_error")
      if (!is.null(biomart_err)) {
        showNotification(biomart_err, type = "error", duration = 10)
        return(NULL)
      }

      # Check for step 2 OrgDb load error (returns Ensembl IDs as fallback)
      step2_err <- attr(mapped_values, "step2_error")
      if (!is.null(step2_err)) {
        showNotification(step2_err, type = "warning", duration = 10)
      }

      # If no identifier was matched at all, update the result reactive but
      # skip adding a column (adding an all-NA column would not be useful).
      if (all(is.na(mapped_values))) {
        debug_log("Mapping complete: 0 identifiers matched - no column added", 1)

        n_total    <- length(mapped_values)
        cache_mode <- if (cross_species) "cross" else "intra"
        cache_date <- get_annotation_cache_date(
          mode      = cache_mode,
          species   = species,
          debug_log = debug_log
        )
        mapping_cache_date <- if (cross_species) {
          get_annotation_mapping_cache_date(debug_log = debug_log)
        } else {
          NULL
        }

        mapping_summary <- list(
          source_col      = source_col,
          new_col         = NULL,
          from_keytype    = from_keytype,
          to_keytype      = to_keytype,
          cross_species   = cross_species,
          source_species  = species,
          target_species  = if (cross_species) input$target_species_annotation else NULL,
          strategy        = collapse_strategy,
          n_total         = n_total,
          n_mapped        = 0L,
          n_unmapped      = n_total,
          two_step        = isTRUE(attr(mapped_values, "two_step")),
          direct_biomart  = isTRUE(attr(mapped_values, "direct_biomart")),
          step1_mapped    = attr(mapped_values, "step1_mapped"),
          step2_input     = attr(mapped_values, "step2_input"),
          step2_mapped    = attr(mapped_values, "step2_mapped"),
          cache_date      = cache_date,
          mapping_cache_date = mapping_cache_date,
          timestamp       = Sys.time()
        )
        last_mapping_result(mapping_summary)

        showNotification(
          sprintf("No identifiers could be mapped (0/%d matched). No column was added.", n_total),
          type = "warning", duration = 7
        )
        return(NULL)
      }

      # Add column to data
      debug_log(sprintf(
        "Pre-add: mapped_values length %d (%d non-NA), target_col_name: '%s'",
        length(mapped_values), sum(!is.na(mapped_values)), target_col_name), 1)

      result <- add_annotation_column(
        data              = data,
        source_col        = source_col,
        mapped_values     = mapped_values,
        target_col_name   = target_col_name,
        collapse_strategy = collapse_strategy,
        debug_log         = debug_log
      )

      debug_log(sprintf("add_annotation_column: %s",
                        if (is.null(result)) "returned NULL (failed)"
                        else sprintf("new_col='%s', %d rows",
                                     result$new_col_name, nrow(result$data))), 1)

      if (is.null(result)) {
        showNotification("Failed to add annotation column.", type = "error")
        return(NULL)
      }

      # Write back via set_data
      debug_log(sprintf("Calling set_data() with updated data (%d cols, new col: '%s')",
                        ncol(result$data), result$new_col_name), 1)

      success <- tryCatch({
        set_data(result$data)
      }, error = function(e) {
        debug_log(paste("set_data() failed:", e$message), 1)
        showNotification(paste("Error updating data:", e$message), type = "error")
        FALSE
      })

      debug_log(sprintf("set_data() returned: %s",
                        if (isTRUE(success)) "TRUE (success)" else "FALSE or NULL (failed)"), 1)

      if (isTRUE(success)) {
        n_mapped   <- sum(!is.na(result$data[[result$new_col_name]]))
        n_total    <- nrow(result$data)
        n_unmapped <- n_total - n_mapped

        # Resolve cache date for the active mode
        cache_mode <- if (cross_species) "cross" else "intra"
        cache_date <- get_annotation_cache_date(
          mode      = cache_mode,
          species   = species,
          debug_log = debug_log
        )
        mapping_cache_date <- if (cross_species) {
          get_annotation_mapping_cache_date(debug_log = debug_log)
        } else {
          NULL
        }

        # Debug log: resolved column naming components
        target_sp_label <- if (cross_species) input$target_species_annotation else NA_character_
        debug_log(sprintf(
          "[Column naming] mode=%s | src_species=%s | src_key=%s | tgt_species=%s | tgt_key=%s | col_name=%s",
          cache_mode, species, from_keytype,
          if (is.na(target_sp_label)) "(same)" else target_sp_label,
          to_keytype, result$new_col_name), 1)

        mapping_summary <- list(
          source_col      = source_col,
          new_col         = result$new_col_name,
          from_keytype    = from_keytype,
          to_keytype      = to_keytype,
          cross_species   = cross_species,
          source_species  = species,
          target_species  = if (cross_species) input$target_species_annotation else NULL,
          strategy        = collapse_strategy,
          n_total         = n_total,
          n_mapped        = n_mapped,
          n_unmapped      = n_unmapped,
          two_step        = isTRUE(attr(mapped_values, "two_step")),
          direct_biomart  = isTRUE(attr(mapped_values, "direct_biomart")),
          step1_mapped    = attr(mapped_values, "step1_mapped"),
          step2_input     = attr(mapped_values, "step2_input"),
          step2_mapped    = attr(mapped_values, "step2_mapped"),
          cache_date      = cache_date,
          mapping_cache_date = mapping_cache_date,
          timestamp       = Sys.time()
        )
        last_mapping_result(mapping_summary)

        debug_log(sprintf("Annotation column '%s' added: %d/%d mapped",
                          result$new_col_name, n_mapped, n_total), 1)

        target_species_label <- if (cross_species) {
          as.character(input$target_species_annotation)
        } else {
          as.character(species)
        }

        cache_date_str <- if (!is.null(cache_date) && nzchar(as.character(cache_date))) {
          as.character(cache_date)
        } else {
          "n/a"
        }

        mapping_cache_date_str <- if (!is.null(mapping_cache_date) && nzchar(as.character(mapping_cache_date))) {
          as.character(mapping_cache_date)
        } else {
          "n/a"
        }

        annotation_mode <- if (identical(input$annotation_strategy, "biomart")) {
          if (isTRUE(cross_species)) "BioMart annotation (cross-species)" else "BioMart annotation"
        } else {
          "AnnotationHub annotation"
        }

        mapping_mode_flags <- c(
          if (isTRUE(attr(mapped_values, "two_step"))) "two_step"
          else if (isTRUE(attr(mapped_values, "direct_biomart"))) "direct_biomart"
          else NULL
        )
        mapping_mode_label <- if (length(mapping_mode_flags) == 0) "standard" else paste(mapping_mode_flags, collapse = ",")

        debug_log(
          sprintf(
            paste0(
              "Annotation mapping summary",
              " | Mode: %s",
              " | Source species: %s",
              " | Target species: %s",
              " | Source keytype: %s",
              " | Target keytype: %s",
              " | Source column: %s",
              " | New column created: %s",
              " | Collapse strategy: %s",
              " | Unique source IDs: %d",
              " | IDs mapped: %d",
              " | IDs unmapped: %d",
              " | Mapping route: %s",
              " | OrgDb cache date: %s",
              " | Mapping cache date: %s"
            ),
            annotation_mode,
            as.character(species),
            target_species_label,
            as.character(from_keytype),
            as.character(to_keytype),
            as.character(source_col),
            as.character(result$new_col_name),
            as.character(collapse_strategy),
            n_total,
            n_mapped,
            n_unmapped,
            mapping_mode_label,
            cache_date_str,
            mapping_cache_date_str
          ),
          level = 0
        )
        debug_log("Metadata update triggered via set_data()", 1)

        showNotification(
          sprintf("Column '%s' added: %d/%d identifiers mapped.",
                  result$new_col_name, n_mapped, n_total),
          type = "message", duration = 5
        )
      } else {
        debug_log("set_data() returned FALSE after annotation mapping", 1)
        showNotification("Failed to apply annotation update.", type = "error")
      }

    }, error = function(e) {
      debug_log(paste("Error in run_annotation handler:", e$message), 1)
      showNotification(paste("Error during ID mapping:", e$message),
                       type = "error", duration = 8)
    })
  })
}
