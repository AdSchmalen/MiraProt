# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_integration.R
# Purpose:
#   Initialize and coordinate Data Wizard submodule integrations while preserving
#   stable contracts between orchestration state and black-box submodules.
#
# Architectural Role:
#   integration
#
# Responsibilities:
#   - Initialize in-app Data Wizard submodules with defensive wrappers.
#   - Bridge canonical core state to module-specific reactive interfaces.
#   - Register integration-level synchronization and fallback behavior.
#
# Non-Responsibilities (Must NOT be here):
#   - Own top-level module orchestration policy or UI composition.
#   - Change implementation details of out-of-scope black-box submodules.
#
# Allowed Dependencies:
#   - In-scope utility layer (`datawizard_utils.R`) and caller-provided core helpers.
#   - Out-of-scope Data Wizard modules through existing stable entry points only.
#
# Interaction Boundaries:
#   - Inputs:
#     Session context, loader outputs, core/ui config helper lists, shared `rv`.
#   - Outputs:
#     Initialized module output list and integration bridge helper functions.
#   - Out-of-Scope Integrations:
#     filtering, ratios, basemean, merge, pivot, batch effects, assign rules,
#     auto-assign, imputation, edit, tables, file loader (black boxes).
#
# Stability Guarantees:
#   - Preserve initialization contracts and fallback behavior expected by orchestration.
#   - Preserve adapter semantics for external module outputs.
#   - Keep failures isolated to prevent full module crash where recoverable.
# ============================================================================
# modules/Data Wizard/datawizard_integration.R
# Data Wizard Integration Layer - Module Coordination and Filter Integration

# Source required utilities
source("modules/Data Wizard/datawizard_utils.R", local = TRUE)

#' Resolve optional module API function from a module output object
#' @param module_out module output object (typically a list)
#' @param api_name API method name to resolve
#' @param context log context
#' @return function or NULL
resolve_optional_module_api <- function(module_out, api_name, context = "module_api") {
  if (is.null(module_out) || is.null(api_name) || !nzchar(api_name)) {
    return(NULL)
  }

  candidate <- tryCatch(module_out[[api_name]], error = function(e) NULL)
  if (!is.function(candidate)) {
    debug_log(paste("Optional API", api_name, "not available in", context), level = 2)
    return(NULL)
  }

  candidate
}

#' Invoke optional module API with safe fallback
#' @param module_out module output object
#' @param api_name API method name
#' @param default_return fallback value when API is unavailable/fails
#' @param context log context
#' @param ... arguments forwarded to the API function
#' @return API result or default_return
call_optional_module_api <- function(module_out, api_name, default_return = NULL, context = "module_api", ...) {
  api_func <- resolve_optional_module_api(module_out, api_name, context = context)
  if (is.null(api_func)) {
    return(default_return)
  }

  tryCatch(
    api_func(...),
    error = function(e) {
      debug_log(paste("Error in optional API", api_name, "for", context, ":", e$message), level = 1)
      default_return
    }
  )
}

#' Initialize all submodules with error handling wrapper
#' @param session Shiny session object
#' @param loader_out file loader module output
#' @param core_values core reactive values
#' @param ui_config_values UI configuration values
#' @param data_access_functions data access helper functions
#' @param modification_functions modification tracking functions
#' @param metadata_functions metadata update functions
#' @param ui_config_functions UI config management functions
#' @param metadata_content_ready metadata content readiness reactive
#' @return list of initialized modules
initialize_submodules <- function(session, loader_out, core_values, ui_config_values,
                                  data_access_functions, modification_functions,
                                  metadata_functions, ui_config_functions,
                                  metadata_content_ready, rv,
                                  advanced_panel_initialized = NULL,
                                  metadata_rule_progress = function(stage) invisible(NULL)) {

  panel_initialized <- function(name) {
    if (is.null(advanced_panel_initialized) ||
        is.null(advanced_panel_initialized$initialized) ||
        is.null(advanced_panel_initialized$initialized[[name]])) return(reactive(TRUE))
    advanced_panel_initialized$initialized[[name]]
  }

  primary_data_state <- create_primary_data_state_adapter(
    rv = rv,
    core_values = core_values,
    debug_log_fn = debug_log
  )

  # === NEUE ZEILEN HINZUFÜGEN ===
  # Create safe UI systems for all modules
  safe_ui_systems <- list(
    pivot = create_safe_ui_system(session, "PIVOT", DEBUG_LEVEL),
    filtering = create_safe_ui_system(session, "FILTERING", DEBUG_LEVEL),
    ratios = create_safe_ui_system(session, "RATIOS", DEBUG_LEVEL),
    basemean = create_safe_ui_system(session, "BASEMEAN", DEBUG_LEVEL),
    batch_effects = create_safe_ui_system(session, "BATCH_EFFECTS", DEBUG_LEVEL),
    merge = create_safe_ui_system(session, "MERGE", DEBUG_LEVEL),
    edit = create_safe_ui_system(session, "EDIT", DEBUG_LEVEL)
  )

  # Helper functions for data access (existing code)
  get_file_data <- function() {
    if (core_values$filter_applied() && !is.null(core_values$filtered_data())) {
      return(core_values$filtered_data())
    } else {
      return(core_values$primary_data_raw())
    }
  }

  current_metadata_for <- function(reference_data = NULL, context = "") {
    resolved <- resolve_current_metadata("primary_working")
    if (nzchar(context)) {
      source_label <- if (!is.null(rv) && metadata_matches_dataset(tryCatch(rv$data_def, error = function(e) NULL), reference_data)) {
        "rv$data_def"
      } else {
        "resolver fallback"
      }
      debug_log(paste(context, ": Using metadata from", source_label), level = 2)
    }
    resolved
  }

  metadata_aligned_with_data <- function(metadata, data, context = "metadata") {
    aligned <- metadata_matches_dataset(metadata, data)
    if (!aligned) {
      debug_log(paste(context, ": Metadata alignment failed or missing"), level = 1)
    }
    aligned
  }


  empty_primary_data_debug_logged <- FALSE
  last_data_source_log <- new.env(parent = emptyenv())

  has_primary_or_processed_data <- function() {
    processed <- tryCatch(!is.null(rv) && !is.null(rv$data_mod), error = function(e) FALSE)
    filtered <- tryCatch(isTRUE(core_values$filter_applied()) && !is.null(core_values$filtered_data()), error = function(e) FALSE)
    raw <- tryCatch(!is.null(core_values$primary_data_raw()), error = function(e) FALSE)
    processed || filtered || raw
  }

  get_primary_or_processed_data <- function(context = "DATA WIZARD", log_empty = TRUE) {
    log_data_source_once <- function(source_key, message) {
      previous_source_key <- last_data_source_log[[context]]
      if (!identical(previous_source_key, source_key)) {
        debug_log(message, level = 2)
        last_data_source_log[[context]] <- source_key
      }
    }

    if (!has_primary_or_processed_data()) {
      log_data_source_once(
        "none",
        paste0(context, ": get_data found no primary/processed data")
      )
      if (isTRUE(log_empty) && !isTRUE(empty_primary_data_debug_logged)) {
        debug_log("Data Wizard: primary/processed data unavailable during startup; skipping data access", level = 2)
        empty_primary_data_debug_logged <<- TRUE
      }
      return(NULL)
    }

    if (!is.null(rv) && !is.null(rv$data_mod)) {
      log_data_source_once(
        "rv$data_mod",
        paste0(context, ": get_data using rv$data_mod (processed data)")
      )
      return(rv$data_mod)
    } else if (core_values$filter_applied() && !is.null(core_values$filtered_data())) {
      log_data_source_once(
        "filtered_data",
        paste0(context, ": get_data using filtered data")
      )
      return(core_values$filtered_data())
    }

    log_data_source_once(
      "raw_data",
      paste0(context, ": get_data using raw data (fallback)")
    )
    core_values$primary_data_raw()
  }

  datawizard_restore_phase_active <- function(phases = NULL) {
    if (is.null(rv)) return(FALSE)
    phase <- rv$session_restore_phase %||% rv$restore_phase %||% NULL
    isTRUE(rv$session_restoring) ||
      (!is.null(phase) && !identical(phase, "complete") &&
         (is.null(phases) || phase %in% phases))
  }

  # Publication boundary for callbacks that can be fired by restored submodule UI.
  # A restore generation plus an active restore/replay phase proves provenance;
  # ordinary callbacks are forwarded to the canonical adapter unchanged.
  publish_primary_data <- function(new_data, operation, metadata = NULL) {
    restore_generation <- if (!is.null(rv)) {
      tryCatch(shiny::isolate(rv$session_restore_generation), error = function(e) NULL)
    } else {
      NULL
    }
    restore_replay <- !is.null(restore_generation) &&
      (isTRUE(tryCatch(shiny::isolate(rv$session_restoring), error = function(e) FALSE)) ||
         datawizard_restore_phase_active())

    if (isTRUE(restore_replay)) {
      incoming_dimensions <- if (is.data.frame(new_data) || is.matrix(new_data)) {
        paste0(nrow(new_data), " x ", ncol(new_data))
      } else {
        "not tabular"
      }
      debug_log(paste0(
        "Data Wizard publication rejected: operation=", operation,
        " | incoming dimensions=", incoming_dimensions,
        " | restore generation=", as.character(restore_generation)[1]
      ), level = 1)
      return(FALSE)
    }

    primary_data_state$set_modified_data(new_data, operation, metadata = metadata)
    TRUE
  }

  sync_enhanced_metadata_for_current_data <- function(new_data, operation_label) {
    if (is.null(core_values$handson_metadata()) || datawizard_restore_phase_active()) {
      return(FALSE)
    }

    enhanced_metadata <- core_values$handson_metadata()
    if (!metadata_aligned_with_data(enhanced_metadata, new_data, operation_label)) {
      debug_log(
        paste(operation_label, ": Skipping rv$data_def sync so the lifecycle observer can rebuild aligned metadata if needed"),
        level = 1
      )
      return(FALSE)
    }

    primary_data_state$set_metadata_for_current_data(enhanced_metadata)
    debug_log(paste(operation_label, ": Synced enhanced metadata to rv$data_def"), level = 1)
    TRUE
  }

  get_file_data2 <- function() {
    return(validate_reactive_value(loader_out$additional, "loader_additional"))
  }

  update_secondary_dataset_revision <- function(data, source = "secondary") {
    if (is.null(data) || !is.data.frame(data)) {
      return(invisible(NULL))
    }
    entry <- NULL
    registry <- if (!is.null(core_values$dataset_registry) && is.function(core_values$dataset_registry)) {
      core_values$dataset_registry()
    } else {
      NULL
    }
    if (!is.null(registry) && is.function(registry$set)) {
      entry <- registry$set("secondary_working", data, source_metadata = list(source = source))
    }
    if (!is.null(core_values$secondary_revision) && is.function(core_values$secondary_revision)) {
      next_revision <- if (!is.null(entry) && !is.null(entry$revision)) {
        entry$revision
      } else {
        isolate(core_values$secondary_revision()) + 1L
      }
      core_values$secondary_revision(as.integer(next_revision))
    }
    invisible(entry)
  }

  set_file_data2 <- function(new_data) {
    if (!is.null(loader_out$additional)) {
      loader_out$additional(new_data)
      return(TRUE)
    }
    return(FALSE)
  }


  observeEvent(loader_out$additional(), {
    update_secondary_dataset_revision(loader_out$additional(), source = "loader_additional")
  }, ignoreInit = TRUE)

  # Available rule files
  available_rule_files <- reactive({
    tryCatch({
      if (dir.exists("AutoAssign")) {
        files <- list.files("AutoAssign", pattern = "\\.rds$", full.names = FALSE)
        if (length(files) > 0) {
          return(setNames(files, files))
        }
      }
      return(character(0))
    }, error = function(e) {
      debug_log(paste("Error reading rule files:", e$message), level = 1)
      return(character(0))
    })
  })

  modules <- list()


  data_revision_signature <- reactive({
    # Compatibility name, transaction semantics: consumers now observe one
    # debounced committed key rather than independently debounced revisions.
    core_values$committed_snapshot_key_debounced()
  })

  # Reactive accessor for the orchestrator-bumped restore trigger. Passed
  # into every DW submodule below so the create_submodule_session_state()
  # factory can drain its staged pending_ui_state once Phase 2 has flushed
  # and dynamic choices have rebuilt.
  session_restore_trigger <- reactive({
    if (!is.null(rv)) rv$session_restore_trigger else NULL
  })

  has_rule_tables <- function(x) {
    is.list(x) &&
      (is.data.frame(x$table) || is.data.frame(x$condition) || is.data.frame(x$ratio))
  }

  central_rules_available <- reactive({
    tryCatch({
      isTRUE(has_rule_tables(core_values$central_loaded_rules())) ||
        isTRUE(has_rule_tables(core_values$central_rule_file()))
    }, error = function(e) {
      debug_log(paste("Error evaluating central_rules_available:", e$message), level = 1)
      FALSE
    })
  })


  manual_rules_available <- reactive({
    tryCatch({
      auto_assign_out <- modules$auto_assign_out
      if (!is.list(auto_assign_out)) {
        return(FALSE)
      }

      has_rows <- function(x) is.data.frame(x) && nrow(x) > 0
      has_direct_rules <- any(c(
        has_rows(if (is.function(auto_assign_out$table_rules)) auto_assign_out$table_rules() else NULL),
        has_rows(if (is.function(auto_assign_out$condition_rules)) auto_assign_out$condition_rules() else NULL),
        has_rows(if (is.function(auto_assign_out$ratio_rules)) auto_assign_out$ratio_rules() else NULL)
      ))

      has_direct_rules ||
        (!is.null(auto_assign_out$has_rules) &&
           is.reactive(auto_assign_out$has_rules) &&
           isTRUE(auto_assign_out$has_rules()))
    }, error = function(e) {
      debug_log(paste("Error evaluating manual_rules_available:", e$message), level = 1)
      FALSE
    })
  })

  # Initialize assign_rules module
  modules$assign_rules_out <- initialize_datawizard_module_safely("Assign Rules", function() {
    modAssignRulesServer(
      "assign_rules",
      rule_files = available_rule_files,
      metadata_current = reactive({ resolve_current_metadata("primary_working") }),
      central_rules_available = central_rules_available,
      manual_rules_available = manual_rules_available,
      rv = rv,  # Add rv parameter
      debug_level = DEBUG_LEVEL
    )
  })

  # Initialize enhanced filtering module with defensive initialization
  modules$filtering_out <- tryCatch({
    modFilteringServer(
      id = "filtering_ui",
      data = reactive({
        return(get_primary_or_processed_data("FILTERING"))
      }),
      metadata_def = reactive({
        raw_metadata <- resolve_current_metadata("primary_working")
        debug_log("FILTERING: Using resolver metadata", level = 2)

        if (!is.null(raw_metadata)) {
          clean_metadata_for_filtering(raw_metadata)
        } else {
          raw_metadata
        }
      }),
      init_meta = loader_out$init_meta,
      UI_config = ui_config_functions$create_filtering_ui_config(modules$assign_rules_out),
      metadata_ready_status = metadata_content_ready,
      session_restore_trigger = session_restore_trigger,
      debug_level = DEBUG_LEVEL,
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      metadata_revision_debounced = core_values$metadata_revision_debounced,
      data_revision_signature = data_revision_signature,
      metadata_assignment_pending = reactive({ isTRUE(core_values$metadata_assignment_pending()) }),
      metadata_meaningful_ready = reactive({ isTRUE(core_values$metadata_meaningful_ready()) })
    )
  }, error = function(e) {
    debug_log(paste("Error initializing enhanced filtering module:", e$message), 1)
    core_values$filtering_config_errors(append(core_values$filtering_config_errors(),
                                               paste("Filter module initialization error:", e$message)))

    # Fallback to basic filtering with rv$data_mod support
    tryCatch({
      modFilteringServer(
        id = "filtering_ui",
        data = reactive({
          return(get_primary_or_processed_data("FILTERING"))
        }),
        metadata_def = reactive({
          resolve_current_metadata("primary_working")
        }),
        init_meta = loader_out$init_meta,
        metadata_ready_status = metadata_content_ready,
        session_restore_trigger = session_restore_trigger,
        metadata_assignment_pending = reactive({ isTRUE(core_values$metadata_assignment_pending()) }),
        metadata_meaningful_ready = reactive({ isTRUE(core_values$metadata_meaningful_ready()) })
      )
    }, error = function(e2) {
      debug_log(paste("Error initializing basic filtering module:", e2$message), 1)
      NULL
    })
  })

  # Initialize ratios module
  modules$ratios_out <- initialize_datawizard_module_safely("Ratios", function() {
    modRatiosServer(
      "ratios",
      data_def = reactive({
        debug_log("RATIOS: Using resolver metadata", level = 2)
        resolve_current_metadata("primary_working")
      }),
      get_data = function() {
        return(get_primary_or_processed_data("RATIOS"))
      },
      set_data = function(new_data) {
        # Keep existing enhanced set_data function
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("RATIOS: Suppressed metadata observer during processing", level = 1)

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("RATIOS: Updated rv$data_mod directly", level = 1)

            modification_functions$record_modification("Ratio Analysis", "Statistical ratio calculations applied")
          }

          metadata_functions$update_metadata_for_ratio_columns(new_data, modules$ratios_out)
          debug_log("RATIOS: Metadata updated for new ratio columns", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "RATIOS")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from ratio analysis.", type = "message", duration = 4)
          }

          debug_log("Ratios: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error updating data from ratio analysis:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },
      available_samples = reactive({
        call_optional_module_api(
          modules$assign_rules_out,
          "current_conditions",
          default_return = character(0),
          context = "ratios_available_samples"
        )
      }),
      UI_config = ui_config_functions$create_ratios_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger,
      metadata_revision_debounced = core_values$metadata_revision_debounced,
      primary_working_revision_debounced = core_values$primary_working_revision_debounced
    )
  })

  modules$basemean_out <- initialize_datawizard_module_safely("Basemean", function() {
    modBasemeanServer(
      "basemean",

      data_def = reactive({
        # Always resolve metadata against the currently-loaded data so a
        # stale rv$data_def from a previous sheet cannot shadow the current
        # handson metadata (which is rebuilt on every sheet switch).
        reference_data <- if (!is.null(rv) && !is.null(rv$data_mod)) {
          rv$data_mod
        } else if (core_values$filter_applied() && !is.null(core_values$filtered_data())) {
          core_values$filtered_data()
        } else {
          core_values$primary_data_raw()
        }
        current_metadata_for(reference_data, context = "BASEMEAN")
      }),

      get_data = function() {
        return(get_primary_or_processed_data("BASEMEAN"))
      },

      set_data = function(new_data) {
        # Schreibe Datenänderungen in rv$data_mod und synchronisiere Metadaten
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("BASEMEAN: Suppressed metadata observer during processing", level = 1)

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("BASEMEAN: Updated rv$data_mod directly", level = 1)

            modification_functions$record_modification("Basemean Calculation", "Basemean columns created or updated")
          }

          if (is.function(metadata_functions$update_metadata_for_basemean_columns)) {
            metadata_functions$update_metadata_for_basemean_columns(new_data, modules$basemean_out)
          } else {
            debug_log("BASEMEAN: metadata_functions$update_metadata_for_basemean_columns not callable — skipping metadata sync", level = 1)
          }
          debug_log("BASEMEAN: Metadata updated for new basemean columns", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "BASEMEAN")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from Basemean module.", type = "message", duration = 4)
          }

          debug_log("BASEMEAN: Data and metadata updated successfully", level = 2)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error updating data from Basemean module:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },

      available_samples = reactive({
        call_optional_module_api(
          modules$assign_rules_out,
          "current_conditions",
          default_return = character(0),
          context = "ratios_available_samples"
        )
      }),

      UI_config = ui_config_functions$create_basemean_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger,
      metadata_revision_debounced = core_values$metadata_revision_debounced,
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      metadata_assignment_pending = reactive({ isTRUE(core_values$metadata_assignment_pending()) }),
      metadata_meaningful_ready = reactive({ isTRUE(core_values$metadata_meaningful_ready()) }),
      data_revision_signature = data_revision_signature
    )
  })

  # Initialize annotation module
  modules$annotation_out <- initialize_datawizard_module_safely("Annotation", function() {
    modAnnotationServer(
      "annotation",

      data_def = reactive({
        debug_log("ANNOTATION: Using resolver metadata", level = 2)
        resolve_current_metadata("primary_working")
      }),

      get_data = function() {
        return(get_primary_or_processed_data("ANNOTATION"))
      },

      set_data = function(new_data) {
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("ANNOTATION: Suppressed metadata observer during processing", level = 1)

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("ANNOTATION: Updated rv$data_mod directly", level = 1)

            modification_functions$record_modification("ID Annotation", "Identifier mapping applied")
          }

          if (is.function(metadata_functions$update_metadata_for_annotation_columns)) {
            metadata_functions$update_metadata_for_annotation_columns(new_data)
          } else {
            debug_log("ANNOTATION: metadata_functions$update_metadata_for_annotation_columns not callable - skipping metadata sync", level = 1)
          }
          debug_log("ANNOTATION: Metadata updated for new annotation columns", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "ANNOTATION")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from Annotation module.", type = "message", duration = 4)
          }

          debug_log("ANNOTATION: Data and metadata updated successfully", level = 2)
          return(TRUE)
        }, error = function(e) {
          debug_log(paste("Error updating data from Annotation module:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },

      available_samples = reactive({
        call_optional_module_api(
          modules$assign_rules_out,
          "current_conditions",
          default_return = character(0),
          context = "annotation_available_samples"
        )
      }),

      UI_config = ui_config_functions$create_annotation_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger
    )
  })

  # Initialize batch effects module
  modules$batch_out <- initialize_datawizard_module_safely("Batch Effects", function() {
    modBatchEffectsServer(
      "batch",
      get_data = function() {
        return(get_primary_or_processed_data("BATCH EFFECTS"))
      },
      set_data = function(new_data) {
        # Keep existing enhanced set_data function
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("BATCH EFFECTS: Suppressed metadata observer during processing", level = 1)

          current_data <- if (!is.null(rv) && !is.null(rv$data_mod)) {
            rv$data_mod
          } else {
            core_values$primary_data_raw()
          }

          if (!is.null(current_data) && !is.null(new_data)) {
            old_cols <- names(current_data)
            new_cols <- names(new_data)
            added_cols <- setdiff(new_cols, old_cols)
            batch_corrected_cols <- grep("^Batch Corrected ", added_cols, value = TRUE)
            if (length(batch_corrected_cols) > 0) {
              modification_functions$record_modification("Batch Correction", paste("Added", length(batch_corrected_cols), "batch corrected columns"))
            } else {
              modification_functions$record_modification("Batch Correction", "Batch effects correction applied")
            }
          }

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("BATCH EFFECTS: Updated rv$data_mod directly", level = 1)
            debug_log(paste("BATCH EFFECTS: rv$data_mod updated to dimensions:", nrow(new_data), "x", ncol(new_data)), level = 2)
          }

          metadata_functions$update_metadata_for_batch_corrected_columns(new_data)
          debug_log("BATCH EFFECTS: Metadata updated for new batch corrected columns", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "BATCH EFFECTS")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from batch correction.", type = "message", duration = 4)
          }

          debug_log("Batch effects: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
          return(TRUE)

        }, error = function(e) {
          debug_log(paste("Error updating data from batch correction:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },
      init_meta = loader_out$init_meta,
      header_primary = loader_out$header_primary,
      UI_config = ui_config_functions$create_batch_effects_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger,
      debug_level = DEBUG_LEVEL,
      initialized = panel_initialized("processing"),
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      data_revision_signature = data_revision_signature
    )
  })

  # Initialize pivot module
  modules$pivot_out <- initialize_datawizard_module_safely("Pivot", function() {
    modPivotServer(
      "pivot",
      get_data = function() {
        return(get_primary_or_processed_data("PIVOT"))
      },
      set_data = function(new_data) {
        # Keep existing enhanced set_data function
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("PIVOT: Suppressed metadata observer during processing", level = 1)

          current_data <- if (!is.null(rv) && !is.null(rv$data_mod)) {
            rv$data_mod
          } else {
            core_values$primary_data_raw()
          }

          if (!is.null(current_data) && !is.null(new_data)) {
            old_cols <- names(current_data)
            new_cols <- names(new_data)
            added_cols <- setdiff(new_cols, old_cols)
            pivoted_cols <- grep("^Pivoted_", added_cols, value = TRUE)
            if (length(pivoted_cols) > 0) {
              modification_functions$record_modification("Data Pivot", paste("Added", length(pivoted_cols), "pivoted columns"))
            } else {
              modification_functions$record_modification("Data Pivot", "Data pivot operation applied")
            }
          }

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("PIVOT: Updated rv$data_mod directly", level = 1)
            debug_log(paste("PIVOT: rv$data_mod updated to dimensions:", nrow(new_data), "x", ncol(new_data)), level = 2)
          }

          metadata_functions$update_metadata_for_pivoted_data(new_data)
          debug_log("PIVOT: Metadata updated for pivoted data", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "PIVOT")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from pivot operation.", type = "message", duration = 4)
          }

          debug_log("Pivot: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
          return(TRUE)

        }, error = function(e) {
          debug_log(paste("Error updating data from pivot operation:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },
      get_data2 = get_file_data2,
      set_data2 = set_file_data2,
      init_meta = loader_out$init_meta,
      UI_config = ui_config_functions$create_pivot_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger,
      debug_level = DEBUG_LEVEL,
      safe_ui_system = safe_ui_systems$pivot,
      initialized = panel_initialized("processing"),
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      secondary_revision_debounced = core_values$secondary_revision_debounced,
      data_revision_signature = data_revision_signature
    )
  })

  # Initialize merge module
  modules$merge_out <- initialize_datawizard_module_safely("Merge", function() {
    modMergeServer(
      "merge",
      get_data = function() {
        return(get_primary_or_processed_data("MERGE"))
      },
      set_data = function(new_data) {
        # Keep existing enhanced set_data function
        tryCatch({
          core_values$metadata_observer_active(FALSE)
          on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
          debug_log("MERGE: Suppressed metadata observer during processing", level = 1)

          current_data <- if (!is.null(rv) && !is.null(rv$data_mod)) {
            rv$data_mod
          } else {
            core_values$primary_data_raw()
          }

          if (!is.null(current_data) && !is.null(new_data)) {
            old_cols <- names(current_data)
            new_cols <- names(new_data)
            added_cols <- setdiff(new_cols, old_cols)
            merged_cols <- grep("^Merged_", added_cols, value = TRUE)
            if (length(merged_cols) > 0) {
              modification_functions$record_modification("Data Merge", paste("Added", length(merged_cols), "merged columns"))
            } else {
              modification_functions$record_modification("Data Merge", "Data merge operation applied")
            }
          }

          if (!is.null(rv) && !is.null(new_data)) {
            if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
            debug_log("MERGE: Updated rv$data_mod directly", level = 1)
            debug_log(paste("MERGE: rv$data_mod updated to dimensions:", nrow(new_data), "x", ncol(new_data)), level = 2)
          }

          # ZURÜCK ZUR URSPRÜNGLICH FUNKTIONIERENDEN FUNKTION
          metadata_functions$update_metadata_for_new_columns(new_data)
          debug_log("MERGE: Metadata updated for new columns", level = 1)
          sync_enhanced_metadata_for_current_data(new_data, "MERGE")

          if (core_values$filter_applied()) {
            core_values$filtered_data(NULL)
            core_values$filter_applied(FALSE)
            showNotification("Filters reset due to data update from merge operation.", type = "message", duration = 4)
          }

          debug_log("Merge: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
          return(TRUE)

        }, error = function(e) {
          debug_log(paste("Error updating data from merge operation:", e$message), level = 1)
          showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
          return(FALSE)
        })
      },
      get_data2 = get_file_data2,
      UI_config = ui_config_functions$create_merge_ui_config(modules$assign_rules_out),
      session_restore_trigger = session_restore_trigger,
      debug_level = DEBUG_LEVEL,
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      secondary_revision_debounced = core_values$secondary_revision_debounced
    )
  })

  # Initialize auto assign module
  modules$auto_assign_out <- modAutoAssignServer(
    "auto_assign",
    metadata_skeleton = reactive({
      resolve_current_metadata("primary_working")
    }),
    current_data = reactive({
      resolve_datawizard_dataset("primary_working", core_values = core_values,
                                 rv = rv)$data
    }),
    metadata_revision = core_values$metadata_revision,
    rule_files = available_rule_files,
    filter_module = reactive({ modules$filtering_out }),
    edit_module = reactive({ modules$edit_out }),
    ratios_module = reactive({ modules$ratios_out }),
    batch_module = reactive({ modules$batch_out }),
    pivot_module = reactive({ modules$pivot_out }),
    merge_module = reactive({ modules$merge_out }),
    imputation_module = reactive({ modules$imputation_out }),
    basemean_module = reactive({ modules$basemean_out }),
    UI_config = reactive({ ui_config_functions$get_imputation_ui_config_for_export() }),
    session_restore_trigger = session_restore_trigger,
    progress_callback = metadata_rule_progress,
    debug_level = DEBUG_LEVEL
  )

  # Initialize tables module
  modules$tables_out <- modDataTablesServer(
    "tables",
    primary_data = reactive({
      select_datawizard_primary_display_data(
        core_values = core_values,
        rv = rv,
        context = "Tables",
        debug_log_fn = debug_log,
        publish_raw_if_missing = TRUE
      )
    }),
    additional_data = loader_out$additional,
    metadata_skeleton = reactive({ resolve_current_metadata("primary_working") }),
    # metadata_final = reactive({
    #   if (core_values$apply_triggered()) {
    #     core_values$final_processed_metadata()
    #   } else {
    #     NULL
    #   }
    # }),
    metadata_final = reactive({
      if (isTRUE(core_values$apply_triggered())) {
        core_values$final_processed_metadata()
      } else {
        NULL
      }
    }),
    filter_applied = core_values$filter_applied,
    original_rows = reactive({
      current_data <- core_values$primary_data_raw()
      if (!is.null(current_data)) nrow(current_data) else 0
    }),
    data_modified = modification_functions$is_data_modified,
    modules_list = modules,
    parent_session = session, # Parent session for Metadata propagation
    rv = rv,
    set_metadata = function(new_meta) {
      if (!is.null(new_meta) && is.data.frame(new_meta)) {
        primary_data_state$set_metadata_for_current_data(new_meta)
      }
    },
    set_primary_data = function(new_data, operation = "table edit", metadata = NULL) {
      if (!is.null(new_data) && is.data.frame(new_data)) {
        publish_primary_data(new_data, operation, metadata = metadata)
      }
    },
    set_additional_data = function(new_data) {
      if (!is.null(loader_out$set_additional_working_data) &&
          is.function(loader_out$set_additional_working_data)) {
        loader_out$set_additional_working_data(new_data)
      } else if (!is.null(loader_out$additional) && is.function(loader_out$additional)) {
        loader_out$additional(new_data)
      }
    },
    record_modification = modification_functions$record_modification,
    primary_working_revision_debounced = core_values$primary_working_revision_debounced,
    metadata_revision_debounced = core_values$metadata_revision_debounced,
    metadata_content_signature_debounced = core_values$metadata_content_signature_debounced,
    secondary_revision_debounced = core_values$secondary_revision_debounced
  )

  # Bind the child UI rendered inside Auto-Assign from the parent Data Wizard
  # namespace.  The composite id is equivalent to ns("auto_assign") followed
  # by the nested UI's ns("auto_regex").
  auto_regex_working_data <- reactive({
    # Keep Current MiraProt metadata mode on the exact snapshot and selection
    # policy used by the Tables module above.  In particular, this preserves
    # filtered/modified/raw precedence and ordered column identity.
    selected <- select_datawizard_primary_display_data(
      core_values = core_values,
      rv = rv,
      context = "Tables",
      debug_log_fn = debug_log,
      publish_raw_if_missing = TRUE
    )
    datawizard_normalize_technical_pair(selected)$data
  })
  # Keep the editable Tables buffer separate from the committed canonical
  # value.  The former is useful for explaining why a run is not ready, but it
  # is never an inference source: an aligned buffer can still contain edits
  # which have not passed through Synchronize metadata.
  auto_regex_editable_metadata <- reactive({
    modules$tables_out$current_metadata()
  })
  auto_regex_metadata <- reactive({
    working_data <- auto_regex_working_data()
    committed_metadata <- datawizard_migrate_metadata_technical_keys(
      resolve_current_metadata("primary_working"))
    # Return the canonical value even when it is stale.  The handler validates
    # it against this same Tables display snapshot and must not mask a mismatch
    # with the editable buffer.
    if (metadata_matches_dataset(committed_metadata, working_data)) {
      return(committed_metadata)
    }
    committed_metadata
  })
  auto_regex_revision <- reactive({
    # Canonical metadata already participates directly in Auto RegEx source
    # identity. Do not also use metadata_revision as an authority: a no-op
    # metadata commit may increment that counter without changing inference
    # input and must therefore not make a valid candidate stale.
    list(
      canonical_metadata = auto_regex_metadata(),
      # Readiness-only buffer. The handler removes this member from the frozen
      # committed tuple after comparing it with canonical metadata.
      editable_metadata = auto_regex_editable_metadata()
    )
  })
  modules$auto_regex_out <- modAutoRegexServer(
    "auto_assign-auto_regex",
    metadata = auto_regex_metadata,
    data = auto_regex_working_data,
    revision = auto_regex_revision,
    provenance = reactive({
      collection <- if (!is.null(modules$ratios_out$get_contrast_mapping_collection))
        tryCatch(modules$ratios_out$get_contrast_mapping_collection(), error = function(e) NULL)
      else NULL
      list(
        source_revision = as.character(core_values$primary_working_revision_debounced() %||% 0L),
        contrast_mapping_collection = collection
      )
    }),
    transfer = function(rules, notify = TRUE) {
      isTRUE(modules$auto_assign_out$load_rules_directly(rules, notify = notify))
    },
    rule_state = list(
      table = modules$auto_assign_out$table_rules,
      condition = modules$auto_assign_out$condition_rules,
      ratio = modules$auto_assign_out$ratio_rules
    )
  )

  # Initialize edit module
  modules$edit_out <- modEditServer(
    "edit",
    get_data = function() {
      return(get_primary_or_processed_data("EDIT"))
    },
    has_data = function() {
      has_primary_or_processed_data()
    },
    set_data = function(new_data) {
      tryCatch({
        # Step 1: Suppress metadata observer during processing
        core_values$metadata_observer_active(FALSE)
        on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
        debug_log("EDIT: Suppressed metadata observer during processing", level = 1)

        # Step 2: Track modifications for logging
        current_data <- core_values$primary_data_raw()
        if (!is.null(current_data) && !is.null(new_data)) {
          modification_functions$record_modification("Data Editing", "Manual data editing operations applied")
        }

        # Step 3: Update rv$data_mod for table rendering
        if (!is.null(rv) && !is.null(new_data)) {
          if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
          debug_log("EDIT: Updated rv$data_mod directly", level = 1)
          debug_log(paste("EDIT: rv$data_mod updated to dimensions:", nrow(new_data), "x", ncol(new_data)), level = 2)
        }

        # Step 4: Update metadata for any new columns (less common with editing, but for consistency)
        metadata_functions$update_metadata_for_new_columns(new_data)
        debug_log("EDIT: Metadata checked for new columns", level = 2)

        # Step 5: Sync metadata to rv$data_def for other modules
        sync_enhanced_metadata_for_current_data(new_data, "EDIT")

        if (core_values$filter_applied()) {
          core_values$filtered_data(NULL)
          core_values$filter_applied(FALSE)
          showNotification("Filters reset due to data update from editing operations.", type = "message", duration = 4)
        }

        debug_log("Edit: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
        return(TRUE)
      }, error = function(e) {
        debug_log(paste("Error updating data from edit operations:", e$message), level = 1)
        showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
        return(FALSE)
      })
    },
    metadata_def = reactive({ resolve_current_metadata("primary_working") }),
    session_restore_trigger = session_restore_trigger,
    debug_level = DEBUG_LEVEL,
    data_revision_signature = data_revision_signature
  )

  # Initialize imputation module
  modules$imputation_out <- modImputationServer(
    "imputation",
    data = reactive({
      return(get_primary_or_processed_data("IMPUTATION"))
    }),
    data_def = reactive({ resolve_current_metadata("primary_working") }),
    get_data = function() {
      return(get_primary_or_processed_data("IMPUTATION"))
    },
    set_data = function(new_data) {
      tryCatch({
        # Step 1: Suppress metadata observer during processing
        core_values$metadata_observer_active(FALSE)
        on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
        debug_log("IMPUTATION: Suppressed metadata observer during processing", level = 1)

        # Step 2: Track modifications for logging
        current_data <- core_values$primary_data_raw()
        is_modification <- FALSE
        if (!is.null(current_data) && !is.null(new_data)) {
          old_cols <- names(current_data)
          new_cols <- names(new_data)
          added_cols <- setdiff(new_cols, old_cols)
          if (length(added_cols) > 0) {
            is_modification <- TRUE
            imputed_cols <- grep("^Imputed ", added_cols, value = TRUE)
            if (length(imputed_cols) > 0) {
              modification_functions$record_modification("Imputation", paste("Added", length(imputed_cols), "imputed columns"))
            }
          }
        }

        # Step 3: Update rv$data_mod for table rendering
        if (!is.null(rv) && !is.null(new_data)) {
          if (!publish_primary_data(new_data, "integration set_data")) return(FALSE)
          debug_log("IMPUTATION: Updated rv$data_mod directly", level = 1)
          debug_log(paste("IMPUTATION: rv$data_mod updated to dimensions:", nrow(new_data), "x", ncol(new_data)), level = 2)
        }

        # Step 4: Update metadata (existing function already works correctly)
        metadata_functions$update_metadata_for_imputed_columns(new_data)
        debug_log("IMPUTATION: Metadata updated for new imputed columns", level = 1)

        # Step 5: Sync enhanced metadata to rv$data_def for other modules
        sync_enhanced_metadata_for_current_data(new_data, "IMPUTATION")

        if (core_values$filter_applied()) {
          core_values$filtered_data(NULL)
          core_values$filter_applied(FALSE)
          showNotification("Filters reset due to data update from imputation.", type = "message", duration = 4)
        }

        debug_log("Imputation: Data and metadata updated successfully, observer restored after scoped suppression", level = 2)
        return(TRUE)
      }, error = function(e) {
        debug_log(paste("Error updating data from imputation:", e$message), level = 1)
        showNotification(paste("Error updating data:", e$message), type = "error", duration = 8)
        return(FALSE)
      })
    },
    UI_config = ui_config_functions$create_imputation_ui_config(modules$assign_rules_out),
    session_restore_trigger = session_restore_trigger,
    debug_level = DEBUG_LEVEL,
    primary_working_revision_debounced = core_values$primary_working_revision_debounced,
    metadata_revision_debounced = core_values$metadata_revision_debounced,
    data_revision_signature = data_revision_signature,
    initialized = panel_initialized("imputation")
  )

  # Enhanced auto-assign integration with timing control
  tryCatch({
    if (!is.null(modules$auto_assign_out) && !is.null(modules$assign_rules_out)) {
      debug_log("Both auto_assign and assign_rules modules available", level = 2)

      # Setup with timing controller if available
      shiny::isolate({
        setup_auto_assign_integration(modules$auto_assign_out, modules$assign_rules_out,
                                      core_values, metadata_functions)
      })

      debug_log("Enhanced auto-assign integration with timing control setup initiated", level = 2)
    } else {
      debug_log("One or both modules missing for auto-assign integration", level = 2)
      if (is.null(modules$auto_assign_out)) debug_log("auto_assign_out is NULL", level = 2)
      if (is.null(modules$assign_rules_out)) debug_log("assign_rules_out is NULL", level = 2)
    }
  }, error = function(e) {
    debug_log(paste("Error in enhanced integration setup:", e$message), level = 1)
  })

  modules$.publish_primary_data <- publish_primary_data
  return(modules)
}

# ============================================================================
# Session-state fan-out across Data Wizard submodules
# ============================================================================
# Submodules export get_session_state/set_session_state pairs via the
# create_submodule_session_state() factory in datawizard_core.R. These
# helpers aggregate/dispatch those hooks, keeping the per-submodule contract
# identical while giving the session-save/restore layer one call site.
#
# Tables and auto_assign tables/DT state is NOT round-tripped here - only
# user-visible form inputs. Edit operations and ratio configurations keep
# their existing dedicated save paths.
#
# The key name under which each submodule's payload is nested in the
# aggregate is stable and versioned so that save/restore stays
# forward-compatible.

#' Names of submodule entries in modules_list that expose session-state hooks
#' @keywords internal
.dw_session_submodule_keys_by_phase <- list(
  # Keep this save aggregation order stable for backward compatibility with
  # previously persisted session payloads and downstream consumers that may
  # inspect serialized submodule order. Restore dispatch uses the dependency-
  # aware .dw_session_restore_submodule_phases below instead.
  loader = c("loader_out"),
  assign_rules = c("assign_rules_out"),
  downstream = c(
    "imputation_out", "filtering_out", "batch_out", "pivot_out",
    "merge_out", "ratios_out", "basemean_out", "annotation_out",
    "auto_assign_out", "edit_out"
  )
)

.dw_session_submodule_keys <- unname(unlist(.dw_session_submodule_keys_by_phase, use.names = FALSE))

#' Dependency-aware restore phases for Data Wizard submodule session state
#' @keywords internal
.dw_session_restore_submodule_phases <- list(
  # Loader state must exist before downstream modules read data/cache inputs.
  loader = c("loader_out"),

  # Assign Rules rebuilds condition choices consumed by later modules.
  assign_rules = c("assign_rules_out"),

  # Auto Assign depends on Assign Rules, but should restore before modules that
  # replay condition-dependent transformation controls.
  auto_assign = c("auto_assign_out"),

  # Transformation modules replay after condition providers are restored.
  condition_dependent_transformations = c(
    "filtering_out", "ratios_out", "basemean_out", "batch_out",
    "pivot_out", "merge_out", "imputation_out", "annotation_out",
    "edit_out"
  )
)

#' Aggregate get_session_state across all Data Wizard submodules
#' @param modules_list list returned by initialize_submodules
#' @return list of \code{<key> -> submodule_state} entries (unexposed
#'   submodules are skipped silently).
get_all_submodule_ui_states <- function(modules_list) {
  if (is.null(modules_list) || !is.list(modules_list)) return(list())
  out <- list()
  for (key in .dw_session_submodule_keys) {
    mod <- modules_list[[key]]
    if (is.null(mod)) next
    getter <- tryCatch(mod[["get_session_state"]], error = function(e) NULL)
    if (!is.function(getter)) next
    # Use single-bracket assignment so a submodule that returns NULL is still
    # represented in the aggregate payload. With [[<- NULL, R removes the entry,
    # which made restore logs look like only a subset of Data Wizard submodules
    # existed and prevented their setters from being considered.
    submodule_state <- tryCatch(getter(), error = function(e) {
      debug_log(paste0("get_session_state failed for ", key, ": ", e$message), 1)
      NULL
    })
    out[key] <- list(submodule_state)
    if (identical(key, "loader_out") && !is.null(out[[key]])) {
      # Data & Metadata snapshots keep the authoritative full loader payload in
      # state$loader_state. The aggregate submodule payload stores only a small
      # reference so serialized snapshots do not contain two physical copies of
      # data_fixed/data2_fixed, original data, sheet manifests, cache tags, and
      # selected sheet/header inputs. Restore dispatch resolves this reference
      # before calling the loader setter.
      out[key] <- list(list(version = "1.0", ref = "loader_state"))
    }
  }
  list(
    version    = "1.0",
    submodules = out
  )
}

#' Dispatch set_session_state to every Data Wizard submodule
#' @param modules_list list returned by initialize_submodules
#' @param state aggregate state (as produced by get_all_submodule_ui_states)
#'   or a flat \code{<key> -> payload} list.
set_all_submodule_ui_states <- function(modules_list, state) {
  if (is.null(modules_list) || !is.list(modules_list)) return(invisible(FALSE))
  if (is.null(state) || !is.list(state)) return(invisible(FALSE))
  payload <- if (!is.null(state$submodules)) state$submodules else state
  if (!is.list(payload)) return(invisible(FALSE))
  if (is.list(payload$loader_out) && identical(payload$loader_out$ref, "loader_state")) {
    root_loader_state <- state$loader_state %||% attr(state, "loader_state", exact = TRUE)
    if (!is.null(root_loader_state)) {
      payload$loader_out <- root_loader_state
    } else {
      payload$loader_out <- NULL
      debug_log("set_all_submodule_ui_states: loader_out reference has no loader_state payload; skipping loader dispatch", 1)
    }
  }
  payload_keys <- names(payload) %||% character(0)
  ordered_keys <- unique(c(
    .dw_session_restore_submodule_phases$loader,
    .dw_session_restore_submodule_phases$assign_rules,
    .dw_session_restore_submodule_phases$auto_assign,
    .dw_session_submodule_keys_by_phase$downstream,
    payload_keys
  ))
  restore_phase_keys <- unname(unlist(.dw_session_restore_submodule_phases, use.names = FALSE))
  restore_groups <- .dw_session_restore_submodule_phases
  legacy_payload_keys <- payload_keys[!payload_keys %in% restore_phase_keys]
  if (length(legacy_payload_keys) > 0) {
    restore_groups$legacy_payload = legacy_payload_keys
  }

  all_dispatched <- character(0)
  all_skipped <- character(0)

  for (phase_name in names(restore_groups)) {
    phase_payload_keys <- restore_groups[[phase_name]]
    phase_dispatched <- character(0)
    phase_skipped <- character(0)

    for (key in phase_payload_keys) {
      if (!key %in% payload_keys) next
      mod <- modules_list[[key]]
      if (is.null(mod)) {
        phase_skipped <- c(phase_skipped, paste0(key, ":module_missing"))
        next
      }
      setter <- tryCatch(mod[["set_session_state"]], error = function(e) NULL)
      if (!is.function(setter)) {
        phase_skipped <- c(phase_skipped, paste0(key, ":setter_missing"))
        next
      }
      ok <- TRUE
      tryCatch(setter(payload[[key]]), error = function(e) {
        debug_log(paste0("set_session_state failed for ", key, ": ", e$message), 1)
        phase_skipped <<- c(phase_skipped, paste0(key, ":error"))
        ok <<- FALSE
      })
      if (isTRUE(ok)) {
        phase_dispatched <- c(phase_dispatched, key)
      }
    }

    all_dispatched <- c(all_dispatched, phase_dispatched)
    all_skipped <- c(all_skipped, phase_skipped)
    debug_log(paste0(
      "set_all_submodule_ui_states: phase=", phase_name,
      " payload_keys=", paste(payload_keys, collapse = ","),
      " dispatched_keys=", paste(phase_dispatched, collapse = ","),
      " skipped_keys=", paste(phase_skipped, collapse = ",")
    ), 2)
  }

  debug_log(paste0(
    "set_all_submodule_ui_states: phase=summary",
    " payload_keys=", paste(payload_keys, collapse = ","),
    " dispatched_keys=", paste(all_dispatched, collapse = ","),
    " skipped_keys=", paste(all_skipped, collapse = ",")
  ), 2)
  invisible(TRUE)
}

#' Setup simplified auto-assign integration
#' @param auto_assign_out auto-assign module output
#' @param assign_rules_out assign rules module output
#' @param core_values core reactive values
#' @param metadata_functions metadata update functions
setup_auto_assign_integration <- function(auto_assign_out, assign_rules_out, core_values, metadata_functions) {
  if (is.null(auto_assign_out) || is.null(assign_rules_out)) {
    debug_log("Auto-assign or assign-rules module not available for integration", level = 1)
    return()
  }

  # Simplified integration - just trigger the central observer
  tryCatch({
    debug_log("Setting up simplified auto-assign integration", level = 2)

    # Check if assign_rules reactive values are available
    if (!is.null(assign_rules_out$selected_rule_file)) {
      debug_log("Found assign_rules selected_rule_file reactive", level = 2)

      # Simple observer that just logs the selection
      # The actual rule loading is handled by Observer 4 in datawizard_module.R
      observeEvent(assign_rules_out$selected_rule_file(), {
        tryCatch({
          selected_file <- assign_rules_out$selected_rule_file()
          if (!is.character(selected_file) || length(selected_file) != 1L || !nzchar(selected_file)) {
            return(invisible(NULL))
          }
          debug_log(paste("Rule file selected via enhanced integration:", selected_file), level = 1)

          # The central observer in datawizard_module.R will handle the actual loading
          # This observer just logs the selection for debugging

        }, error = function(e) {
          debug_log(paste("Error getting selected rule file:", e$message), level = 1)
        })
      }, ignoreInit = TRUE)
    } else {
      debug_log("assign_rules selected_rule_file not available", level = 2)
    }

    debug_log("Simplified auto-assign integration setup completed", level = 2)

  }, error = function(e) {
    debug_log(paste("Error setting up simplified auto-assign integration:", e$message), level = 1)
  })
}

# Enhanced filtering integration with rv$data_mod support
#' @param primary_data_state Optional adapter for primary data state writes.
setup_filter_integration <- function(filter_module, core_values, modification_functions, module_name,
                                     rv = NULL, primary_data_state = NULL,
                                     publication_helper = NULL) {
  if (is.null(primary_data_state)) {
    primary_data_state <- create_primary_data_state_adapter(
      rv = rv,
      core_values = core_values,
      debug_log_fn = debug_log
    )
  }
  if (!is.function(publication_helper)) {
    publication_helper <- function(new_data, operation, metadata = NULL) {
      primary_data_state$set_modified_data(new_data, operation, metadata = metadata)
      TRUE
    }
  }

  if (is.null(filter_module)) {
    debug_log(paste("Filter module", module_name, "is NULL - skipping integration"), level = 2)
    return()
  }

  datawizard_restore_phase_active <- function(phases = NULL) {
    if (is.null(rv)) return(FALSE)
    phase <- rv$session_restore_phase %||% rv$restore_phase %||% NULL
    isTRUE(rv$session_restoring) ||
      (!is.null(phase) && !identical(phase, "complete") &&
         (is.null(phases) || phase %in% phases))
  }

  # Apply filters observer with enhanced error handling and rv sync
  if (!is.null(filter_module$apply_filters_trigger)) {
    observeEvent(filter_module$apply_filters_trigger(), {
      # Restoring a filtering submodule replays its saved trigger.  The module's
      # plot-data cache is historical plotting input, not a new interactive
      # filtering result, so it must not replace the canonical restore pair.
      if (datawizard_restore_phase_active()) {
        debug_log(paste("Ignoring filter apply replay during session restore for", module_name), level = 2)
        return(invisible(NULL))
      }

      # Get current working data (processed if available, otherwise raw)
      current_data <- if (!is.null(rv) && !is.null(rv$data_mod)) {
        debug_log("FILTERING: Starting with rv$data_mod (processed data)", level = 1)
        rv$data_mod
      } else {
        debug_log("FILTERING: Starting with raw data (fallback)", level = 1)
        core_values$primary_data_raw()
      }

      # Get current metadata (enhanced if available)
      current_metadata <- resolve_current_metadata("primary_working")
      debug_log("FILTERING: Using resolver metadata", level = 2)

      req(current_data, current_metadata)

      debug_log(paste("Filter apply triggered from", module_name), level = 1)

      tryCatch({
        # Call the filtering function with proper error handling
        if (!is.null(filter_module$perform_filtering)) {
          filter_result <- safe_module_call(
            function() filter_module$perform_filtering(source = "individual"),
            default_return = list(success = FALSE, message = "Filter function failed"),
            context = paste("apply_filters", module_name, sep = "_")
          )

          if (!is.null(filter_result) && is.list(filter_result)) {
            if (isTRUE(filter_result$success)) {
              # Step 1: Suppress metadata observer during filtering
              if (!is.null(core_values$metadata_observer_active)) {
                core_values$metadata_observer_active(FALSE)
                on.exit(core_values$metadata_observer_active(TRUE), add = TRUE)
                debug_log("FILTERING: Suppressed metadata observer during processing", level = 1)
              }

              # Step 2: Update filtered data in core_values
              if (!is.null(filter_result$data)) {
                if (!publication_helper(filter_result$data, "filter")) return(invisible(NULL))
                primary_data_state$set_filtered_data(filter_result$data, source = "filter")
                core_values$filter_applied(TRUE)

                # Step 3: CRITICAL - Update rv$data_mod for table rendering (if rv available)
                if (!is.null(rv)) {
                  debug_log("FILTERING: Updated rv$data_mod with filtered data", level = 1)
                  debug_log(paste("FILTERING: rv$data_mod updated to dimensions:",
                                  nrow(filter_result$data), "x", ncol(filter_result$data)), level = 2)
                }

                # Step 4: Sync metadata to rv$data_def (if rv available)
                if (!is.null(rv) && !is.null(current_metadata) && !datawizard_restore_phase_active()) {
                  primary_data_state$set_metadata_for_current_data(current_metadata)
                  debug_log("FILTERING: Synced metadata to rv$data_def", level = 2)
                }

                # Record modification
                rows_removed <- if (!is.null(filter_result$rows_removed)) {
                  filter_result$rows_removed
                } else {
                  nrow(current_data) - nrow(filter_result$data)
                }

                modification_functions$record_modification("Data Filtering", paste("Removed", rows_removed, "rows"))

                showNotification(
                  paste("Filters applied successfully. Removed", rows_removed, "rows."),
                  type = "message", duration = 4
                )

                debug_log(paste("Filtering completed - Removed", rows_removed, "rows, observer restored after scoped suppression"), level = 1)
              }
            } else {
              debug_log(paste("Filter application failed:", filter_result$message), level = 1)
              showNotification(paste("Filter failed:", filter_result$message), type = "error", duration = 6)
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Error in filter integration:", e$message), level = 1)
        showNotification(paste("Filter error:", e$message), type = "error", duration = 6)
      })
    }, ignoreInit = TRUE)
  }

  # Enhanced clear filters observer with rv sync
  if (!is.null(filter_module$clear_filters_trigger)) {
    observeEvent(filter_module$clear_filters_trigger(), {
      debug_log(paste("Filter clear triggered from", module_name), level = 1)
      tryCatch({
        # Bevorzugt den verarbeiteten Baseline-Zustand (falls existiert), sonst Rohdaten
        baseline <- NULL
        if (!is.null(core_values$processed_baseline)) {
          baseline <- core_values$processed_baseline()
        }
        if (is.null(baseline)) {
          baseline <- core_values$primary_data_raw()
        }

        if (!publication_helper(baseline, "filter clear")) return(invisible(NULL))
        core_values$filtered_data(NULL)
        core_values$filter_applied(FALSE)

        if (!is.null(rv)) {
          debug_log("FILTERING: Reset rv$data_mod to baseline (unfiltered)", level = 1)
        } else {
          debug_log("FILTERING: rv fehlt beim Reset – kein rv$data_mod gesetzt", level = 2)
        }

        if (!is.null(core_values$metadata_observer_active)) {
          core_values$metadata_observer_active(TRUE)
        }

        modification_functions$record_modification("Filter Clear", "All filters removed")
        showNotification("Filters cleared - showing unfiltered data", type = "message", duration = 3)
      }, error = function(e) {
        debug_log(paste("Error clearing filters:", e$message), level = 1)
        showNotification(paste("Error clearing filters:", e$message), type = "error", duration = 6)
      })
    }, ignoreInit = TRUE)
  }
}

#' Helper function to safely get reactive values
#' @param reactive_val reactive value or function
#' @param default_value default value if reactive fails
#' @param context context for error logging
#' @return value or default
safe_reactive_get <- function(reactive_val, default_value = NULL, context = "") {
  tryCatch({
    if (is.null(reactive_val)) {
      return(default_value)
    }

    # Check if it's a function (reactive)
    if (is.function(reactive_val)) {
      # Try to call it as a reactive
      result <- reactive_val()
      return(result)
    } else {
      # If it's not a function, return it directly
      return(reactive_val)
    }
  }, error = function(e) {
    debug_log(paste("Error in safe_reactive_get for", context, ":", e$message), level = 1)
    return(default_value)
  })
}

#' Setup UI config trigger integration with assign_rules module
#' @param assign_rules_out assign rules module output
#' @param ui_config_functions UI config management functions
setup_ui_config_triggers <- function(assign_rules_out, ui_config_functions) {

  # Safe observer pattern for UI config triggers
  safe_ui_config_observer <- function(trigger_reactive, setter_func, config_name) {
    if (!is.null(trigger_reactive)) {
      observeEvent(trigger_reactive(), {
        ui_config <- trigger_reactive()
        debug_log(paste("UI config trigger from assign_rules for", config_name), level = 2)

        if (!is.null(ui_config)) {
          success <- setter_func(ui_config)
          if (success) {
            showNotification(paste(config_name, "configuration applied from rule file."),
                             type = "message", duration = 4)
          }
        } else {
          setter_func(NULL)
        }
      }, ignoreInit = TRUE)
    }
  }

  # Set up UI config observers
  if (!is.null(assign_rules_out)) {
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_imputation,
                            ui_config_functions$set_imputation_ui_config_from_import, "Imputation")
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_filtering,
                            ui_config_functions$set_filtering_ui_config_from_import, "Filtering")
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_ratios,
                            ui_config_functions$set_ratios_ui_config_from_import, "Ratios")
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_batch_effects,
                            ui_config_functions$set_batch_effects_ui_config_from_import, "Batch Effects")
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_pivot,
                            ui_config_functions$set_pivot_ui_config_from_import, "Pivot")
    safe_ui_config_observer(assign_rules_out$assign_rules_ui_merge,
                            ui_config_functions$set_merge_ui_config_from_import, "Merge")
  }
}

#' Register collapsible panel toggle handlers for Data Wizard sidebar sections
#' @param input shiny input object
register_datawizard_ui_toggle_handlers <- function(input) {
  toggle_handlers <- list(
    processing = "processing",
    filtering = "filtering",
    edit = "edit",
    imputation = "imputation",
    ratios = "ratios"
  )

  initialized <- stats::setNames(
    lapply(names(toggle_handlers), function(name) reactiveVal(FALSE)),
    names(toggle_handlers)
  )
  opened <- stats::setNames(
    lapply(names(toggle_handlers), function(name) reactiveVal(FALSE)),
    names(toggle_handlers)
  )

  lapply(names(toggle_handlers), function(name) {
    observeEvent(input[[paste0("toggle_", name)]], {
      content_id <- paste0(name, "_content")
      icon_id <- paste0(name, "_icon")

      shinyjs::toggle(content_id)
      is_open <- !isTRUE(opened[[name]]())
      opened[[name]](is_open)

      if (is_open) {
        # Monotonic by design: collapsing must not discard pending or restored
        # configuration after the UI has been hydrated once.
        initialized[[name]](TRUE)
        shinyjs::removeClass(icon_id, "fa-chevron-right")
        shinyjs::addClass(icon_id, "fa-chevron-down")
      } else {
        shinyjs::removeClass(icon_id, "fa-chevron-down")
        shinyjs::addClass(icon_id, "fa-chevron-right")
      }
    })
  })

  list(opened = opened, initialized = initialized)
}

#' Register integration observers that bridge filtering/imputation module state
#' @param modules_list initialized module outputs
#' @param core_values core reactive values list
#' @param ui_config_values UI config reactive values list
#' @param modification_functions modification tracking helpers
#' @param rv shared app reactiveValues
#' @param primary_data_state Optional adapter for primary data state writes.
register_datawizard_state_bridge_observers <- function(modules_list, core_values, ui_config_values,
                                                       modification_functions, rv,
                                                       primary_data_state = NULL) {
  if (is.null(primary_data_state)) {
    primary_data_state <- create_primary_data_state_adapter(
      rv = rv,
      core_values = core_values,
      debug_log_fn = debug_log
    )
  }

  if (!is.null(modules_list$filtering_out)) {
    setup_filter_integration(
      modules_list$filtering_out,
      core_values,
      modification_functions,
      "enhanced",
      rv = rv,
      primary_data_state = primary_data_state,
      publication_helper = modules_list$.publish_primary_data
    )
    debug_log("Enhanced filtering integration setup completed", level = 2)
  } else {
    debug_log("Enhanced filtering module not available for integration", level = 1)
  }

  if (!is.null(modules_list$filtering_out)) {
    safe_filtering_observer <- function(reactive_func, setter_func, name) {
      if (!is.null(reactive_func)) {
        observeEvent(reactive_func(), {
          data <- reactive_func()
          if (!is.null(data)) {
            setter_func(data)
            debug_log(paste("Filtering", name, "settings updated"), level = 2)
          }
        }, ignoreInit = TRUE)
      }
    }

    safe_filtering_observer(modules_list$filtering_out$filtering_confidence, core_values$filtering_confidence, "confidence")
    safe_filtering_observer(modules_list$filtering_out$filtering_valid_values, core_values$filtering_valid_values, "valid values")
    safe_filtering_observer(modules_list$filtering_out$filtered_conditions, core_values$filtered_conditions, "conditions")
    safe_filtering_observer(modules_list$filtering_out$filtering_log, core_values$filtering_log, "log")
  }

  if (!is.null(modules_list$imputation_out)) {
    imputation_log_api <- resolve_optional_module_api(
      modules_list$imputation_out,
      "imputation_log",
      context = "state_bridge_imputation"
    )

    if (!is.null(imputation_log_api)) {
      observeEvent(imputation_log_api(), {
        log_data <- call_optional_module_api(
          modules_list$imputation_out,
          "imputation_log",
          default_return = NULL,
          context = "state_bridge_imputation"
        )
        if (!is.null(log_data)) {
          core_values$imputation_log(log_data)
          debug_log(paste("Imputation log updated - entries:", length(log_data)), level = 2)
        }
      }, ignoreInit = TRUE)
    }

    imputation_setting_api <- resolve_optional_module_api(
      modules_list$imputation_out,
      "imputation_setting",
      context = "state_bridge_imputation"
    )

    if (!is.null(imputation_setting_api)) {
      observeEvent(imputation_setting_api(), {
        setting_data <- call_optional_module_api(
          modules_list$imputation_out,
          "imputation_setting",
          default_return = NULL,
          context = "state_bridge_imputation"
        )
        if (!is.null(setting_data)) {
          core_values$imputation_setting(setting_data)
          debug_log(paste("Imputation settings updated:", setting_data$imputation_method_select), level = 2)

          if (ui_config_values$ui_config_source() %in% c("import", "rules")) {
            ui_config_values$ui_config_source("user_modified")
          }
        }
      }, ignoreInit = TRUE)
    }
  }
}

#' Register assign-rules driven UI configuration observers
#' @param modules_list initialized module outputs
#' @param ui_config_functions UI config setter helpers
#' @param add_agg_event aggregation callback used by rule application summary
register_assign_rules_ui_config_observers <- function(modules_list, ui_config_functions, add_agg_event) {
  safe_ui_config_observer <- function(trigger_reactive, setter_func, config_name) {
    if (is.null(trigger_reactive)) return(invisible())

    observeEvent(trigger_reactive(), {
      cfg_res <- tryCatch(
        list(ok = TRUE, value = trigger_reactive(), error = NULL),
        error = function(e) list(ok = FALSE, value = NULL, error = e$message)
      )

      if (!cfg_res$ok) {
        add_agg_event("failed", config_name, list(error = cfg_res$error))
        return(invisible())
      }

      ui_config <- cfg_res$value

      if (is.null(ui_config)) {
        add_agg_event("missing", config_name)
        tryCatch(setter_func(NULL), error = function(e) {
          debug_log(paste("Reset failed for", config_name, ":", e$message), level = 2)
        })
        return(invisible())
      }

      apply_res <- tryCatch(
        list(ok = TRUE, value = setter_func(ui_config), error = NULL),
        error = function(e) list(ok = FALSE, value = FALSE, error = e$message)
      )

      if (isTRUE(apply_res$ok) && isTRUE(apply_res$value)) {
        add_agg_event("applied", config_name)
      } else if (!apply_res$ok) {
        add_agg_event("failed", config_name, list(error = apply_res$error))
      } else {
        add_agg_event("failed", config_name, list(error = "Setter returned FALSE"))
      }
    }, ignoreInit = TRUE)
  }

  if (!is.null(modules_list$assign_rules_out)) {
    assign_rules_out <- modules_list$assign_rules_out

    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_imputation", context = "assign_rules_ui_config"),
      ui_config_functions$set_imputation_ui_config_from_import,
      "Imputation"
    )
    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_filtering", context = "assign_rules_ui_config"),
      ui_config_functions$set_filtering_ui_config_from_import,
      "Filtering"
    )
    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_ratios", context = "assign_rules_ui_config"),
      ui_config_functions$set_ratios_ui_config_from_import,
      "Ratios"
    )
    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_batch_effects", context = "assign_rules_ui_config"),
      ui_config_functions$set_batch_effects_ui_config_from_import,
      "Batch Effects"
    )
    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_pivot", context = "assign_rules_ui_config"),
      ui_config_functions$set_pivot_ui_config_from_import,
      "Pivot"
    )
    safe_ui_config_observer(
      resolve_optional_module_api(assign_rules_out, "assign_rules_ui_merge", context = "assign_rules_ui_config"),
      ui_config_functions$set_merge_ui_config_from_import,
      "Merge"
    )
  }
}

#' Register standardized Data Wizard session cleanup callback
#' @param core_values core reactive values list
#' @param ui_config_values UI config reactive values list
#' @param modification_functions modification tracking helpers
register_datawizard_session_cleanup <- function(core_values, ui_config_values, modification_functions) {
  cleanup_manager$register_module("DataWizard", function() {
    debug_log("Executing [DataWizard] cleanup", level = 1)

    cleanup_data_state <- create_primary_data_state_adapter(
      core_values = core_values,
      debug_log_fn = debug_log
    )
    cleanup_data_state$reset_to_original(NULL)
    core_values$handson_metadata(NULL)
    core_values$final_processed_data(NULL)
    core_values$final_processed_metadata(NULL)
    core_values$filtered_data(NULL)
    core_values$filter_applied(FALSE)
    core_values$apply_triggered(FALSE)

    modification_functions$reset_modification_tracking()

    core_values$central_rule_file("")
    core_values$central_loaded_rules(NULL)
    core_values$rule_application_state("idle")

    core_values$imputation_log(NULL)
    core_values$imputation_setting(NULL)
    core_values$filtering_confidence(NULL)
    core_values$filtering_valid_values(NULL)
    core_values$filtered_conditions(NULL)
    core_values$filtering_log(NULL)

    ui_config_values$central_imputation_ui_config(NULL)
    ui_config_values$ui_config_source("none")
    ui_config_values$central_filtering_ui_config(NULL)
    ui_config_values$filtering_ui_config_source("none")
    ui_config_values$central_batch_effects_ui_config(NULL)
    ui_config_values$batch_effects_ui_config_source("none")
    ui_config_values$central_pivot_ui_config(NULL)
    ui_config_values$pivot_ui_config_source("none")
    ui_config_values$central_merge_ui_config(NULL)
    ui_config_values$merge_ui_config_source("none")
    ui_config_values$central_ratios_ui_config(NULL)
    ui_config_values$ratios_ui_config_source("none")

    core_values$ui_config_errors(list())
    core_values$filtering_config_errors(list())
    ui_config_values$ui_config_update_in_progress(FALSE)
    ui_config_values$filtering_update_in_progress(FALSE)
    ui_config_values$ratios_update_in_progress(FALSE)
    ui_config_values$batch_effects_update_in_progress(FALSE)
    ui_config_values$pivot_update_in_progress(FALSE)
    ui_config_values$merge_update_in_progress(FALSE)
    core_values$last_config_application_time(NULL)

    debug_log("[DataWizard] cleanup completed", level = 1)
  })
}
