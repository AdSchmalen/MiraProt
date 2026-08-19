# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_state_adapter.R
# Purpose:
#   Provide the core state adapter portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

#' Create a primary data state adapter
#'
#' Centralizes writes to the current primary working data while preserving the
#' legacy rv fields that downstream Data Wizard modules still read. Raw import
#' operations update both canonical raw data and the current working copy;
#' modification operations only replace the working copy.
#'
#' @param rv Shared app reactiveValues, or NULL.
#' @param core_values Core reactive values container, or NULL.
#' @param debug_log_fn Optional logger function(message, level).
#' @return list of explicit primary data state operations.
create_primary_data_state_adapter <- function(rv = NULL, core_values = NULL, debug_log_fn = NULL) {
  telemetry_now <- function() unname(proc.time()[["elapsed"]])
  duplicate_counts <- new.env(parent = emptyenv())
  count_duplicate_work <- function(operation) {
    count <- (duplicate_counts[[operation]] %||% 0L) + 1L
    duplicate_counts[[operation]] <- count
    # Verbose-only: normal log levels discard level 2 diagnostics.
    log_state_update(sprintf("DW DUPLICATE WORK | module=core | operation=%s | count=%d",
                             operation, count), 2)
  }
  telemetry_context <- function() {
    id <- tryCatch(shiny::isolate(rv$datawizard_upload_correlation_id), error = function(e) NULL)
    started <- tryCatch(shiny::isolate(rv$datawizard_upload_monotonic_started), error = function(e) NULL)
    list(id = as.character(id %||% "no-upload"), started = as.numeric(started %||% telemetry_now()))
  }
  log_lifecycle <- function(marker, phase, details = "", level = 1) {
    context <- telemetry_context()
    suffix <- if (nzchar(details)) paste0(" | ", details) else ""
    log_state_update(sprintf(
      "DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=%s | phase=%s%s",
      context$id, 1000 * (telemetry_now() - context$started), marker, phase, suffix
    ), level)
  }
  log_state_update <- function(message, level = 2) {
    if (is.function(debug_log_fn)) {
      debug_log_fn(message, level)
    }
  }

  set_import_phase <- function(phase, started_at = NULL, timing_label = NULL) {
    allowed <- c("idle", "reading", "publishing_raw", "creating_metadata", "ready")
    if (!phase %in% allowed) stop("Unknown Data Wizard import phase: ", phase)
    previous <- if (!is.null(core_values) && is.function(core_values$import_phase)) {
      tryCatch(shiny::isolate(core_values$import_phase()), error = function(e) "idle")
    } else "idle"
    if (!is.null(core_values) && is.function(core_values$import_phase)) core_values$import_phase(phase)
    # This small scalar mirror is intentionally the only import-barrier state in
    # shared rv; the canonical phase remains the lightweight reactiveVal above.
    if (!is.null(rv)) rv$datawizard_import_phase <- phase
    if (identical(phase, "ready") && !identical(previous, "ready")) {
      if (!is.null(core_values) && is.function(core_values$import_ready_revision)) {
        revision <- as.integer(shiny::isolate(core_values$import_ready_revision()) %||% 0L) + 1L
        core_values$import_ready_revision(revision)
        if (!is.null(rv)) rv$datawizard_import_ready_revision <- revision
      }
    }
    if (!is.null(started_at)) {
      elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
      log_state_update(sprintf("IMPORT TIMING: %s %.3fs | phase %s -> %s",
                               timing_label %||% phase, elapsed, previous, phase), 1)
    }
    invisible(phase)
  }

  # A generation is deliberately separate from the phase/revision signals.  The
  # started value changes before reset publishes its many compatibility clears;
  # the committed value changes only once data and its placeholder metadata are
  # a valid pair.  Consumers can therefore discard every intermediate flush.
  begin_import_generation <- function() {
    committed <- if (!is.null(core_values) && is.function(core_values$import_generation_committed))
      shiny::isolate(core_values$import_generation_committed()) else
      tryCatch(rv$datawizard_import_generation_committed, error = function(e) 0L)
    started <- as.integer(committed %||% 0L) + 1L
    if (!is.null(core_values) && is.function(core_values$import_generation_started))
      core_values$import_generation_started(started)
    if (!is.null(rv)) rv$datawizard_import_generation_started <- started
    log_lifecycle("start", "import_generation", sprintf("generation=%d", started))
    invisible(started)
  }

  commit_import_generation <- function(data, metadata) {
    log_lifecycle("start", "committed_revision_release")
    if (!is.data.frame(data) || !metadata_matches_dataset(metadata, data)) {
      stop("Cannot commit Data Wizard import generation without a matching data/metadata pair")
    }
    started <- if (!is.null(core_values) && is.function(core_values$import_generation_started))
      shiny::isolate(core_values$import_generation_started()) else
      tryCatch(rv$datawizard_import_generation_started, error = function(e) 0L)
    if (!is.null(core_values) && is.function(core_values$import_generation_committed))
      core_values$import_generation_committed(as.integer(started))
    if (!is.null(rv)) rv$datawizard_import_generation_committed <- as.integer(started)
    registry <- resolve_dataset_registry()
    expected_roles <- c("primary_original", "primary_raw", "primary_working")
    role_matches <- vapply(expected_roles, function(role) {
      entry <- tryCatch(registry$get_latest_entry(role), error = function(e) NULL)
      !is.null(entry) && isTRUE(all.equal(entry$data, data, check.attributes = FALSE))
    }, logical(1))
    mirror_matches <- if (is.null(rv)) TRUE else all(vapply(
      c("data_raw", "data_mod", "primary_data_raw", "primary_data_original"),
      function(field) isTRUE(all.equal(rv[[field]], data, check.attributes = FALSE)),
      logical(1)
    ))
    core_raw_matches <- is.null(core_values) || !is.function(core_values$primary_data_raw) ||
      isTRUE(all.equal(shiny::isolate(core_values$primary_data_raw()), data, check.attributes = FALSE))
    if (!all(role_matches) || !mirror_matches || !core_raw_matches) {
      stop("Committed Data Wizard import does not match all canonical and compatibility holders")
    }
    set_import_phase("ready")
    revisions <- c(
      working = tryCatch(shiny::isolate(core_values$primary_working_revision()), error = function(e) NA_integer_),
      metadata = tryCatch(shiny::isolate(core_values$metadata_revision()), error = function(e) NA_integer_),
      ready = tryCatch(shiny::isolate(core_values$import_ready_revision()), error = function(e) NA_integer_)
    )
    log_lifecycle("end", "committed_revision_release", sprintf(
      "generation=%d | revision_working=%s | revision_metadata=%s | revision_ready=%s",
      started, revisions[[1]], revisions[[2]], revisions[[3]]
    ))
    invisible(started)
  }

  abort_import_generation <- function() {
    committed <- if (!is.null(core_values) && is.function(core_values$import_generation_committed))
      shiny::isolate(core_values$import_generation_committed()) else
      tryCatch(rv$datawizard_import_generation_committed, error = function(e) 0L)
    if (!is.null(core_values) && is.function(core_values$import_generation_started))
      core_values$import_generation_started(as.integer(committed %||% 0L))
    if (!is.null(rv)) rv$datawizard_import_generation_started <- as.integer(committed %||% 0L)
    set_import_phase("idle")
    invisible(committed)
  }

  refresh_metadata_content_signature <- function(metadata) {
    if (!is.null(core_values) && is.function(core_values$metadata_content_signature)) {
      new_signature <- create_metadata_content_signature_dw(metadata)
      old_signature <- tryCatch(shiny::isolate(core_values$metadata_content_signature()),
                                error = function(e) "")
      if (!identical(old_signature, new_signature)) {
        core_values$metadata_content_signature(new_signature)
      }
    }
  }

  resolve_dataset_registry <- function() {
    if (!is.null(core_values) && !is.null(core_values$dataset_registry)) {
      if (is.function(core_values$dataset_registry)) {
        registry <- core_values$dataset_registry()
        if (is.null(registry)) {
          registry <- create_datawizard_dataset_registry()
          core_values$dataset_registry(registry)
        }
        return(registry)
      }
      return(core_values$dataset_registry)
    }

    if (!is.null(rv)) {
      if (is.null(rv$dataset_registry)) {
        rv$dataset_registry <- create_datawizard_dataset_registry()
      }
      return(rv$dataset_registry)
    }

    create_datawizard_dataset_registry()
  }

  revision_signal_for_role <- function(role) {
    switch(
      role,
      primary_working = "primary_working_revision",
      primary_raw = "primary_raw_revision",
      primary_filtered = "primary_filtered_revision",
      metadata_working = "metadata_revision",
      metadata_final = "metadata_revision",
      secondary_working = "secondary_revision",
      secondary_original = "secondary_revision",
      NULL
    )
  }

  transaction <- new.env(parent = emptyenv())
  transaction$active <- FALSE
  transaction$reason <- NULL
  transaction$changed_roles <- character(0)
  transaction$entries <- list()

  bump_revision_once <- function(signal_name) {
    if (!is.null(signal_name) &&
        !is.null(core_values) &&
        is.function(core_values[[signal_name]])) {
      current_revision <- isolate(core_values[[signal_name]]())
      if (is.null(current_revision) || is.na(current_revision)) {
        current_revision <- 0L
      }
      core_values[[signal_name]](as.integer(current_revision) + 1L)
      if (!is.null(rv)) {
        mirror_name <- switch(signal_name,
          primary_working_revision = "datawizard_data_revision_id",
          primary_raw_revision = "datawizard_raw_revision_id",
          primary_filtered_revision = "datawizard_filtered_revision_id",
          metadata_revision = "datawizard_metadata_revision_id",
          secondary_revision = "datawizard_secondary_revision_id",
          NULL
        )
        if (!is.null(mirror_name)) rv[[mirror_name]] <- as.integer(current_revision) + 1L
      }
    }
  }

  begin_datawizard_transaction <- function(reason = "unspecified") {
    count_duplicate_work("state_transaction")
    if (!isTRUE(transaction$active)) {
      transaction$active <- TRUE
      transaction$reason <- reason
      transaction$changed_roles <- character(0)
      transaction$entries <- list()
      log_state_update(paste("DATA STATE: Transaction started:", reason), 2)
      transaction$started_monotonic <- telemetry_now()
      log_lifecycle("start", "state_transaction", paste0("reason=", reason), 2)
    }
    invisible(TRUE)
  }

  commit_datawizard_transaction <- function() {
    if (!isTRUE(transaction$active)) {
      return(invisible(FALSE))
    }

    changed_roles <- unique(transaction$changed_roles)
    changed_signals <- unique(unlist(lapply(changed_roles, revision_signal_for_role), use.names = FALSE))
    changed_signals <- changed_signals[nzchar(changed_signals)]
    for (signal_name in changed_signals) {
      bump_revision_once(signal_name)
    }

    log_state_update(
      paste(
        "DATA STATE: Transaction committed:",
        transaction$reason %||% "unspecified",
        "| roles:",
        paste(changed_roles, collapse = ", "),
        "| signals:",
        paste(changed_signals, collapse = ", ")
      ),
      2
    )
    log_lifecycle("end", "state_transaction", paste0(
      "reason=", transaction$reason %||% "unspecified",
      " | revisions=", paste(changed_signals, collapse = ","),
      " | duration_ms=", sprintf("%.3f", 1000 * (telemetry_now() - (transaction$started_monotonic %||% telemetry_now())))
    ), 2)

    transaction$active <- FALSE
    transaction$reason <- NULL
    transaction$changed_roles <- character(0)
    transaction$entries <- list()
    transaction$started_monotonic <- NULL
    invisible(TRUE)
  }

  with_datawizard_transaction <- function(reason, expr) {
    started_here <- !isTRUE(transaction$active)
    if (started_here) {
      begin_datawizard_transaction(reason)
    }
    result <- force(expr)
    if (started_here) {
      commit_datawizard_transaction()
    }
    result
  }

  write_registry <- function(role, data, source_metadata = NULL, lazy_ref = NULL, allow_original_update = FALSE) {
    registry <- resolve_dataset_registry()
    entry <- NULL
    if (!is.null(registry) && is.function(registry$set)) {
      entry <- registry$set(
        role = role,
        data = data,
        source_metadata = source_metadata,
        lazy_ref = lazy_ref,
        allow_original_update = allow_original_update
      )
    }

    if (!isTRUE(transaction$active)) {
      begin_datawizard_transaction(paste("implicit", role, "update"))
      transaction$changed_roles <- unique(c(transaction$changed_roles, role))
      transaction$entries[[role]] <- entry
      commit_datawizard_transaction()
    } else {
      transaction$changed_roles <- unique(c(transaction$changed_roles, role))
      transaction$entries[[role]] <- entry
    }

    invisible(entry)
  }

  write_core_primary_raw <- function(data) {
    if (!is.null(core_values) && is.function(core_values$primary_data_raw)) {
      core_values$primary_data_raw(data)
    }
  }

  write_legacy_raw <- function(data) {
    if (!is.null(rv)) {
      rv$primary_data_raw <- data
    }
  }

  write_legacy_working <- function(data) {
    if (!is.null(rv)) {
      rv$data_mod <- data
    }
  }


  metadata_has_meaningful_content <- function(metadata) {
    is_meaningful_metadata(metadata)
  }

  restore_metadata_write_allowed <- function(metadata, context = "metadata update") {
    if (is.null(rv) || !isTRUE(rv$session_restoring)) {
      return(TRUE)
    }

    current_data <- tryCatch(rv$data_mod, error = function(e) NULL)
    current_meta <- tryCatch(rv$data_def, error = function(e) NULL)
    if (!restore_has_valid_canonical_pair(current_data, current_meta)) {
      return(TRUE)
    }

    if (!metadata_matches_dataset(metadata, current_data)) {
      log_state_update(paste("DATA STATE:", context, "rejected during restore; metadata does not match current rv$data_mod"), 1)
      return(FALSE)
    }

    if (!is_meaningful_metadata(metadata) && is_meaningful_metadata(current_meta)) {
      log_state_update(paste("DATA STATE:", context, "rejected during restore; Row-Index-only metadata cannot replace meaningful rv$data_def"), 1)
      return(FALSE)
    }

    TRUE
  }

  mark_metadata_synchronized <- function(metadata) {
    ready <- metadata_has_meaningful_content(metadata)
    if (!ready) {
      return(invisible(FALSE))
    }
    if (!is.null(core_values) && is.function(core_values$metadata_meaningful_ready)) {
      core_values$metadata_meaningful_ready(TRUE)
    }
    if (!is.null(rv)) {
      rv$datawizard_metadata_meaningful_ready <- TRUE
    }
    if (!is.null(core_values) && is.function(core_values$metadata_lifecycle_state)) {
      core_values$metadata_lifecycle_state("metadata_ready")
    }
    if (!is.null(rv)) {
      rv$datawizard_metadata_lifecycle_state <- "metadata_ready"
    }
    if (!is.null(core_values) && is.function(core_values$metadata_assignment_pending)) {
      core_values$metadata_assignment_pending(FALSE)
    }
    if (!is.null(rv)) {
      rv$datawizard_metadata_assignment_pending <- FALSE
    }
    invisible(TRUE)
  }

  set_metadata_for_current_data <- function(metadata) {
    metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
    registry <-
      resolve_dataset_registry()

    primary_entry <-
      tryCatch(
        registry$get_latest_entry(
          "primary_working"
        ),
        error = function(e) NULL
      )

    current_primary <-
      if (!is.null(primary_entry) &&
          is.data.frame(primary_entry$data)) {

        primary_entry$data

      } else if (!is.null(rv) &&
                 is.data.frame(
                   tryCatch(
                     rv$data_mod,
                     error = function(e) NULL
                   )
                 )) {

        rv$data_mod

      } else {

        NULL
      }

    if (is.data.frame(current_primary) &&
        !metadata_matches_dataset(
          metadata,
          current_primary
        )) {

      stop(
        sprintf(
          paste0(
            "Metadata synchronization rejected: ",
            "current primary data has %d columns but metadata has %d rows ",
            "or a different Column order."
          ),
          ncol(current_primary),
          if (is.data.frame(metadata)) {
            nrow(metadata)
          } else {
            0L
          }
        ),
        call. = FALSE
      )
    }
    if (!restore_metadata_write_allowed(metadata, "set_metadata_for_current_data")) {
      return(invisible(NULL))
    }

    ready <- metadata_has_meaningful_content(metadata)
    if (ready) {
      if (!is.null(core_values) && is.function(core_values$metadata_assignment_pending)) {
        core_values$metadata_assignment_pending(TRUE)
      }
      if (!is.null(core_values) && is.function(core_values$metadata_lifecycle_state)) {
        core_values$metadata_lifecycle_state("metadata_assigning")
      }
      if (!is.null(rv)) {
        rv$datawizard_metadata_assignment_pending <- TRUE
        rv$datawizard_metadata_lifecycle_state <- "metadata_assigning"
      }
    }

    with_datawizard_transaction("metadata update", {
      write_registry("metadata_working", metadata)
      if (!is.null(core_values) && is.function(core_values$handson_metadata)) {
        core_values$handson_metadata(metadata)
      }
      if (!is.null(rv)) {
        rv$data_def <- metadata
      }
      refresh_metadata_content_signature(metadata)
    })
    mark_metadata_synchronized(metadata)
    log_state_update("DATA STATE: Metadata synchronized for current primary data", 2)
    invisible(metadata)
  }

  list(
    set_import_phase = set_import_phase,
    begin_import_generation = begin_import_generation,
    commit_import_generation = commit_import_generation,
    abort_import_generation = abort_import_generation,
    begin_datawizard_transaction = begin_datawizard_transaction,
    commit_datawizard_transaction = commit_datawizard_transaction,
    set_dataset = function(role, data, source, metadata = NULL, allow_original_update = FALSE) {
      with_datawizard_transaction(paste("set_dataset", role, source), {
        write_registry(role, data, source_metadata = list(source = source, metadata = metadata), allow_original_update = allow_original_update)
      })
      invisible(data)
    },
    set_metadata = function(role, metadata, source) {
      metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
      if (!restore_metadata_write_allowed(metadata, paste("set_metadata", role, source))) {
        return(invisible(NULL))
      }
      with_datawizard_transaction(paste("set_metadata", role, source), {
        write_registry(role, metadata, source_metadata = list(source = source))
      })
      invisible(metadata)
    },
    publish_legacy_mirrors = function() {
      # Legacy mirrors are published eagerly by the adapter's setters. This API
      # exists as the explicit transaction boundary for future callers.
      invisible(TRUE)
    },

    set_raw_imported_data = function(data, source) {
      with_datawizard_transaction(paste("raw import", source), {
        source_metadata <- list(source = source)
        write_registry("primary_original", data, source_metadata = source_metadata, allow_original_update = TRUE)
        write_registry("primary_raw", data, source_metadata = source_metadata)
        write_registry("primary_working", data, source_metadata = source_metadata)
        write_core_primary_raw(data)
        write_legacy_raw(data)
        write_legacy_working(data)
        if (!is.null(rv)) {
          rv$data_raw <- data
          rv$primary_data_original <- data
          rv$primary_data_source <- source
        }
        if (!is.null(core_values) && is.function(core_values$metadata_lifecycle_state)) {
          core_values$metadata_lifecycle_state("raw_loaded")
        }
        if (!is.null(core_values) && is.function(core_values$metadata_assignment_pending)) {
          core_values$metadata_assignment_pending(FALSE)
        }
        if (!is.null(core_values) && is.function(core_values$metadata_meaningful_ready)) {
          core_values$metadata_meaningful_ready(FALSE)
        }
        if (!is.null(rv)) {
          rv$datawizard_metadata_lifecycle_state <- "raw_loaded"
          rv$datawizard_metadata_assignment_pending <- FALSE
          rv$datawizard_metadata_meaningful_ready <- FALSE
        }
      })
      log_state_update(paste("DATA STATE: Raw primary data imported from", source), 1)
      invisible(data)
    },

    set_current_primary_sheet = function(data, source) {
      with_datawizard_transaction(paste("current primary sheet", source), {
        write_registry("primary_raw", data, source_metadata = list(source = source))
        write_registry("primary_working", data, source_metadata = list(source = source))
        write_core_primary_raw(data)
        write_legacy_raw(data)
        write_legacy_working(data)
        if (!is.null(rv)) {
          rv$data_raw <- data
          rv$primary_data_source <- source
        }
      })
      invisible(data)
    },

    set_modified_data = function(data, operation, metadata = NULL) {
      metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
      with_datawizard_transaction(paste("modified data", operation), {
        write_registry("primary_working", data, source_metadata = list(operation = operation))
        write_legacy_working(data)
        if (!is.null(rv)) {
          rv$primary_data_operation <- operation
        }
        if (!is.null(metadata) && restore_metadata_write_allowed(metadata, paste("set_modified_data", operation))) {
          write_registry("metadata_working", metadata, source_metadata = list(operation = operation))
          if (!is.null(core_values) && is.function(core_values$handson_metadata)) {
            core_values$handson_metadata(metadata)
          }
          if (!is.null(rv)) {
            rv$data_def <- metadata
          }
          refresh_metadata_content_signature(metadata)
        }
      })
      log_state_update(paste("DATA STATE: Working primary data updated by", operation), 1)
      invisible(data)
    },

    reset_primary_to_original = function(data = NULL) {
      registry <- resolve_dataset_registry()
      original_entry <- if (!is.null(registry) && is.function(registry$get_latest_entry)) {
        registry$get_latest_entry("primary_original")
      } else {
        NULL
      }
      data <- (if (!is.null(original_entry)) original_entry$data else NULL) %||% data
      if (is.null(data) && !is.null(rv)) {
        data <- rv$primary_data_original
      }
      if (is.null(data)) {
        return(invisible(NULL))
      }

      with_datawizard_transaction("reset_primary_to_original", {
        write_registry("primary_raw", data, source_metadata = list(operation = "reset_primary_to_original"))
        write_registry("primary_working", data, source_metadata = list(operation = "reset_primary_to_original"))
        write_core_primary_raw(data)
        write_legacy_raw(data)
        write_legacy_working(data)
        if (!is.null(rv)) {
          rv$primary_data_original <- data
          rv$primary_data_operation <- "reset_primary_to_original"
        }
      })
      log_state_update("DATA STATE: Primary data reset to immutable original", 1)
      invisible(data)
    },

    reset_secondary_to_original = function(data = NULL) {
      registry <- resolve_dataset_registry()
      original_entry <- if (!is.null(registry) && is.function(registry$get_latest_entry)) {
        registry$get_latest_entry("secondary_original")
      } else {
        NULL
      }
      data <- (if (!is.null(original_entry)) original_entry$data else NULL) %||% data
      if (is.null(data) && !is.null(rv)) {
        data <- rv$secondary_data_original
      }
      if (is.null(data)) {
        return(invisible(NULL))
      }

      with_datawizard_transaction("reset_secondary_to_original", {
        write_registry("secondary_working", data, source_metadata = list(operation = "reset_secondary_to_original"))
        if (!is.null(rv)) {
          rv$secondary_data_original <- data
        }
      })
      log_state_update("DATA STATE: Secondary data reset to immutable original", 1)
      invisible(data)
    },

    reset_processing_state_for_primary = function() {
      with_datawizard_transaction("reset_processing_state_for_primary", {
        write_registry("primary_filtered", NULL, source_metadata = list(operation = "reset_processing_state_for_primary"))
        write_registry("primary_final", NULL, source_metadata = list(operation = "reset_processing_state_for_primary"))
        write_registry("metadata_working", NULL, source_metadata = list(operation = "reset_processing_state_for_primary"))
        write_registry("metadata_final", NULL, source_metadata = list(operation = "reset_processing_state_for_primary"))

        if (!is.null(core_values)) {
          if (is.function(core_values$filtered_data)) core_values$filtered_data(NULL)
          if (is.function(core_values$filter_applied)) core_values$filter_applied(FALSE)
          if (is.function(core_values$metadata_observer_active)) core_values$metadata_observer_active(TRUE)
          if (is.function(core_values$final_processed_data)) core_values$final_processed_data(NULL)
          if (is.function(core_values$final_processed_metadata)) core_values$final_processed_metadata(NULL)
          if (is.function(core_values$processed_baseline)) core_values$processed_baseline(NULL)
          if (is.function(core_values$apply_triggered)) core_values$apply_triggered(FALSE)
        }

        if (!is.null(rv)) {
          rv$data_def <- NULL
          rv$data_raw <- NULL
          rv$handson_metadata <- NULL
          rv$metadata_content_ready <- FALSE
          rv$metadata_skeleton <- NULL
          rv$init_metadata <- NULL
          rv$current_metadata <- NULL
          rv$filter_applied <- FALSE
          rv$filtered_data <- NULL
          rv$filtered_dataset_log <- NULL
          rv$apply_triggered <- FALSE
          rv$final_processed_data <- NULL
          rv$final_processed_metadata <- NULL
          rv$data_modified <- FALSE
          rv$modification_history <- list()
          rv$imputation_log <- NULL
          rv$imputation_setting <- NULL
          rv$filtering_confidence <- NULL
          rv$filtering_valid_values <- NULL
          rv$filtered_conditions <- NULL
          rv$filtering_log <- NULL
          rv$ab_validate <- FALSE
          rv$ui_config_errors <- list()
          rv$filtering_config_errors <- list()
          rv$last_config_application_time <- NULL
          rv$central_rule_file <- ""
          rv$central_loaded_rules <- NULL
          rv$rule_application_state <- "idle"
          rv$tables_metadata <- NULL
          rv$filtering_metadata <- NULL
          rv$edit_metadata <- NULL
        }
      })
      log_state_update("DATA STATE: Primary processing state cleared", 1)
      invisible(TRUE)
    },

    reset_to_original = function(data = NULL) {
      # Backward-compatible wrapper; reset uses the immutable original snapshot.
      reset_primary_to_original(data)
    },

    set_filtered_data = function(data, source = "filter") {
      write_registry("primary_filtered", data, source_metadata = list(source = source))
      if (!is.null(core_values) && is.function(core_values$filtered_data)) {
        core_values$filtered_data(data)
      }
      invisible(data)
    },

    set_final_data = function(data, source = "final") {
      write_registry("primary_final", data, source_metadata = list(source = source))
      if (!is.null(core_values) && is.function(core_values$final_processed_data)) {
        core_values$final_processed_data(data)
      }
      invisible(data)
    },

    set_metadata_final = function(metadata, source = "final") {
      write_registry("metadata_final", metadata, source_metadata = list(source = source))
      if (!is.null(core_values) && is.function(core_values$final_processed_metadata)) {
        core_values$final_processed_metadata(metadata)
      }
      invisible(metadata)
    },

    registry = resolve_dataset_registry,
    set_metadata_for_current_data = set_metadata_for_current_data
  )
}
