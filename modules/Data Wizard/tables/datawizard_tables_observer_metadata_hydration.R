# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_metadata_hydration.R
# Purpose:
#   Provide the tables observer metadata hydration portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Safety-critical metadata observer unit for Data Wizard tables.
#
# This file is a mechanical extraction from datawizard_tables_observer_rendering.R.
# Hydration is registered before general rendering, while editing is registered
# after table mutation observers, preserving the established observer phases.

.clear_datawizard_metadata_sync_guard <- function(
  scheduled_token, scheduled_generation, current_token, current_generation,
  clear_guards) {
  if (!identical(current_token(), scheduled_token) ||
      !identical(current_generation(), scheduled_generation)) {
    return(invisible(FALSE))
  }
  clear_guards()
  invisible(TRUE)
}

.consume_datawizard_metadata_write_back_guard <- function(
  guard, incoming_metadata, local_metadata) {
  if (!isTRUE(guard$active)) return(FALSE)

  # The guard is one-shot, but only the payload written by Tables is its echo.
  # A distinct canonical payload (notably one published by session restore)
  # must continue through normal canonical-to-Tables hydration.
  guard$active <- FALSE
  isTRUE(all.equal(
    incoming_metadata,
    local_metadata,
    check.attributes = FALSE
  ))
}

.datawizard_tables_restore_convergence_payload <- function(
  local_metadata, canonical_metadata, canonical_data) {
  if (!is.data.frame(canonical_data) ||
      !is.data.frame(canonical_metadata) ||
      !metadata_matches_dataset(canonical_metadata, canonical_data)) {
    return(NULL)
  }
  if (is.data.frame(local_metadata) && isTRUE(all.equal(
    local_metadata, canonical_metadata, check.attributes = FALSE
  ))) {
    return(NULL)
  }
  canonical_metadata
}

register_tables_metadata_hydration <- function(context) {
  with(context, {
  current_handson_metadata <- context$current_handson_metadata
  metadata_options_refresh <- context$metadata_options_refresh
  metadata_write_back_guard <- context$metadata_write_back_guard
  suppress_metadata_edit_echo <- context$suppress_metadata_edit_echo
  programmatic_metadata_update_active <- context$programmatic_metadata_update_active
  suppress_next_final_metadata_sync <- context$suppress_next_final_metadata_sync
  metadata_sync_pending <- context$metadata_sync_pending
  metadata_sync_guard_token <- context$metadata_sync_guard_token

  set_metadata_sync_state <- function(paused = isTRUE(input$pause_metadata_sync),
                                      pending = isTRUE(metadata_sync_pending())) {
    if (is.null(rv)) return(invisible(FALSE))
    tryCatch(isolate({
      rv$datawizard_metadata_sync_paused <- isTRUE(paused)
      rv$datawizard_metadata_sync_pending <- isTRUE(pending)
    }), error = function(e) {
      debug_log(paste("Metadata sync state mirror update failed:", e$message), 2)
    })
    invisible(TRUE)
  }
  mark_programmatic_metadata_sync <- function() {
    # rHandsontable can echo a change event while Shiny is replacing/redrawing
    # the widget from reactive metadata state. Keep a programmatic-update guard
    # active across the flush that sends the redraw and the following flush that
    # receives any load/render echo from the browser. Manual edits should only
    # be processed after this update cycle has fully settled.
    restore_snapshot <- shiny::isolate(list(
      generation = if (!is.null(rv)) rv$session_restore_generation else NULL,
      restoring = if (!is.null(rv)) isTRUE(rv$session_restoring) else FALSE,
      session_phase = if (!is.null(rv)) rv$session_restore_phase else NULL,
      legacy_phase = if (!is.null(rv)) rv$restore_phase else NULL
    ))
    restore_phase <- restore_snapshot$session_phase %||% restore_snapshot$legacy_phase
    invoked_during_restore <- !is.null(restore_snapshot$generation) &&
      (isTRUE(restore_snapshot$restoring) ||
         (!is.null(restore_phase) && !identical(restore_phase, "complete")))
    scheduled_generation <- restore_snapshot$generation
    scheduled_token <- shiny::isolate(metadata_sync_guard_token()) + 1L

    metadata_sync_guard_token(scheduled_token)
    suppress_metadata_edit_echo(TRUE)
    programmatic_metadata_update_active(TRUE)

    current_generation <- function() shiny::isolate(
      if (!is.null(rv)) rv$session_restore_generation else NULL
    )
    release_guards <- function() {
      .clear_datawizard_metadata_sync_guard(
        scheduled_token = scheduled_token,
        scheduled_generation = scheduled_generation,
        current_token = function() metadata_sync_guard_token(),
        current_generation = current_generation,
        clear_guards = function() {
          suppress_metadata_edit_echo(FALSE)
          programmatic_metadata_update_active(FALSE)
        }
      )
    }
    session$onFlushed(function() {
      later::later(function() {
        if (isTRUE(invoked_during_restore)) {
          .run_session_restore_callback(
            owner = "Data Wizard tables",
            reason = "release programmatic metadata guard",
            generation = scheduled_generation,
            phase = restore_phase %||% "metadata",
            job_metadata = list(current_generation = current_generation),
            callback = release_guards
          )
        } else {
          tryCatch(
            shiny::isolate(release_guards()),
            error = function(e) debug_log(paste0(
              "[Data Wizard tables] metadata guard release failed: ",
              conditionMessage(e)
            ), 1L)
          )
        }
      }, delay = 0.25)
    }, once = TRUE)
  }

  metadata_aligned_with_primary <- function(meta, df) {
    metadata_matches_dataset(meta, df)
  }

  metadata_sync_alignment_error <- function(metadata) {

    reference <-
      tryCatch(
        metadata_reference_df(),
        error = function(e) NULL
      )

    if (!is.data.frame(reference)) {
      return(
        "current primary data is unavailable"
      )
    }

    if (!is.data.frame(metadata)) {
      return(
        "metadata is not a data frame"
      )
    }

    if (!"Column" %in% names(metadata)) {
      return(
        "metadata is missing the Column field"
      )
    }

    expected_rows <- ncol(reference)
    received_rows <- nrow(metadata)

    if (!identical(
      received_rows,
      expected_rows
    )) {
      return(
        sprintf(
          paste0(
            "metadata has %d rows but the current data has %d columns. ",
            "If you pasted metadata from a spreadsheet, paste the data rows ",
            "without the spreadsheet header row."
          ),
          received_rows,
          expected_rows
        )
      )
    }

    if (!identical(
      as.character(metadata$Column),
      as.character(names(reference))
    )) {
      return(
        paste(
          "metadata Column values do not match the current data columns",
          "in the same order"
        )
      )
    }

    ""
  }

  metadata_paste_contains_header_row <- function(
    event_source,
    raw_changes) {

    event_source <-
      if (length(event_source)) {
        tolower(
          as.character(
            event_source[[1L]]
          )
        )
      } else {
        ""
      }

    if (!grepl(
      "paste",
      event_source,
      fixed = TRUE
    )) {
      return(FALSE)
    }

    if (is.null(raw_changes) ||
        !length(raw_changes)) {
      return(FALSE)
    }

    header_fields <- c(
      "Content",
      "Options",
      "Numerator",
      "Denominator",
      "Transformation",
      "Sample"
    )

    first_row_values <- character()

    for (change in raw_changes) {

      if (length(change) < 4L) {
        next
      }

      row_index <-
        suppressWarnings(
          as.integer(
            change[[1L]]
          )
        )

      if (is.na(row_index) ||
          row_index != 0L) {
        next
      }

      value <- change[[4L]]

      if (is.null(value) ||
          !length(value) ||
          is.na(value[[1L]])) {
        next
      }

      first_row_values <- c(
        first_row_values,
        as.character(
          value[[1L]]
        )
      )
    }

    matched_fields <- unique(
      first_row_values[
        first_row_values %in%
          header_fields
      ]
    )

    "Content" %in%
      matched_fields &&
      length(matched_fields) >= 3L
  }

  metadata_reference_df <- function() {
    ref_df <- tryCatch({
      if (!is.null(rv) && !is.null(rv$data_mod) && is.data.frame(rv$data_mod)) rv$data_mod else NULL
    }, error = function(e) NULL)
    if (!is.null(ref_df)) return(ref_df)
    tryCatch(primary_data(), error = function(e) NULL)
  }

  # Revisions may be consumed while a restore transaction is still publishing.
  # Its existing completion trigger is the deterministic point at which the
  # valid canonical pair may replace stale table-local presentation state.
  if (!is.null(rv)) {
    observeEvent(rv$session_restore_trigger, {
      trigger <- isolate(rv$session_restore_trigger)
      generation <- isolate(rv$session_restore_generation)
      canonical_data <- isolate(tryCatch(rv$data_mod, error = function(e) NULL))
      canonical_metadata <- isolate(tryCatch(metadata_skeleton(), error = function(e) NULL))
      local_metadata <- isolate(tryCatch(current_handson_metadata(), error = function(e) NULL))
      replacement <- .datawizard_tables_restore_convergence_payload(
        local_metadata, canonical_metadata, canonical_data
      )

      # No deferred work is scheduled. Recheck both existing restore identities
      # before mutation so generation N cannot overwrite generation N+1.
      if (is.null(replacement) ||
          !identical(isolate(rv$session_restore_generation), generation) ||
          !identical(isolate(rv$session_restore_trigger), trigger)) {
        return()
      }

      debug_log(sprintf(
        "[TablesMetadata] generation=%s source=restore-convergence canonical_data_cols=%d canonical_metadata_rows=%d local_metadata_rows=%d action=hydrate_local",
        as.character(generation %||% NA_integer_), ncol(canonical_data),
        nrow(canonical_metadata), if (is.data.frame(local_metadata)) nrow(local_metadata) else 0L
      ), 1)
      mark_programmatic_metadata_sync()
      current_handson_metadata(replacement)
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(paused = FALSE, pending = FALSE)
      freezeReactiveValue(input, "metadata_table")
      metadata_options_refresh(isolate(metadata_options_refresh()) + 1L)
    }, ignoreInit = TRUE, priority = -10)
  }
  # --------------------------------------------------------------------------
  # a. Metadata skeleton initialization observer
  #    Populates current_handson_metadata when the parent module provides a
  #    fresh skeleton (e.g. after a file is loaded or after a data manipulation
  #    extends the metadata). Guarded: when the change originated from a manual
  #    edit write-back (metadata_write_back_guard), skip the overwrite to
  #    prevent feedback loops.
  # --------------------------------------------------------------------------

  observeEvent(list(metadata_revision_debounced(), primary_working_revision_debounced()), {
    incoming_skeleton <- req(isolate(metadata_skeleton()))
    incoming_skeleton <- datawizard_drop_deprecated_metadata_columns(incoming_skeleton)
    primary_df <- metadata_reference_df()
    req(is.data.frame(primary_df))
    current_local_meta <- tryCatch(current_handson_metadata(), error = function(e) NULL)
    incoming_has_rows <- is.data.frame(incoming_skeleton) && nrow(incoming_skeleton) > 0

    # Suppress only the canonical echo of the exact local payload we wrote.
    # A restore may publish a different payload while this one-shot guard is
    # still active; that authoritative payload must hydrate the Tables buffer.
    write_back_guard_was_active <- isTRUE(metadata_write_back_guard$active)
    if (.consume_datawizard_metadata_write_back_guard(
      metadata_write_back_guard, incoming_skeleton, current_local_meta
    )) {
      debug_log("Skeleton observer: suppressed expected write-back echo", 2)
      return()
    }
    if (write_back_guard_was_active) {
      debug_log("Skeleton observer: stale write-back guard cleared; accepting distinct canonical metadata", 1)
    }

    if (!incoming_has_rows) {
      if (!is.null(current_local_meta) && is.data.frame(current_local_meta) && nrow(current_local_meta) > 0) {
        debug_log("Skeleton observer: ignoring empty skeleton to preserve existing metadata", 1)
      }
      return()
    }

    # Do not block first-time/restore hydration. When local metadata is empty,
    # accept incoming skeleton even if data/metadata replay ordering is transiently misaligned.
    if (is.null(current_local_meta) || !is.data.frame(current_local_meta) || nrow(current_local_meta) == 0) {
      mark_programmatic_metadata_sync()
      current_handson_metadata(incoming_skeleton)
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(pending = FALSE)
      return()
    }

    if (!metadata_aligned_with_primary(incoming_skeleton, primary_df)) {
      debug_log("Skeleton observer: skipping stale metadata overwrite (not aligned with primary columns)", 1)
      return()
    }

    mark_programmatic_metadata_sync()
    current_handson_metadata(incoming_skeleton)
    metadata_sync_pending(FALSE)
    set_metadata_sync_state(pending = FALSE)
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # a2. Metadata final observer
  #     Updates current_handson_metadata when processing modules (ratios, batch
  #     effects, imputation, etc.) apply rules and produce updated metadata via
  #     metadata_final(). Without this observer the rule-applied metadata never
  #     reaches the rHandsontable, causing stale Content/Options values and

  #     incorrect column colors in the primary data preview.
  # --------------------------------------------------------------------------

  observeEvent(list(metadata_revision_debounced(), primary_working_revision_debounced()), {
    final_meta <- isolate(metadata_final())
    final_meta <- datawizard_drop_deprecated_metadata_columns(final_meta)
    req(final_meta)
    req(nrow(final_meta) > 0)
    primary_df <- metadata_reference_df()
    req(is.data.frame(primary_df))

    if (isTRUE(suppress_next_final_metadata_sync())) {
      debug_log("Metadata final observer: skipping one-cycle overwrite after manual metadata sync", 2)
      suppress_next_final_metadata_sync(FALSE)
      return()
    }

    if (!metadata_aligned_with_primary(final_meta, primary_df)) {
      debug_log("Metadata final observer: skipping stale overwrite (final metadata not aligned with primary columns)", 1)
      return()
    }

    current <- current_handson_metadata()

    # As above, a pending write-back guard does not own the next unrelated
    # canonical metadata revision.
    write_back_guard_was_active <- isTRUE(metadata_write_back_guard$active)
    if (.consume_datawizard_metadata_write_back_guard(
      metadata_write_back_guard, final_meta, current
    )) {
      debug_log("Metadata final observer: suppressed expected write-back echo", 2)
      return()
    }
    if (write_back_guard_was_active) {
      debug_log("Metadata final observer: stale write-back guard cleared; accepting distinct canonical metadata", 1)
    }

    # Only update when the metadata actually differs
    if (!is.null(current) && isTRUE(all.equal(final_meta, current, check.attributes = FALSE))) {
      return()
    }

    debug_log("Metadata final observer: updating from rule-applied metadata", 1)
    mark_programmatic_metadata_sync()
    current_handson_metadata(final_meta)
    metadata_sync_pending(FALSE)
    set_metadata_sync_state(pending = FALSE)

    # Synchronize core state with guard to prevent skeleton observer feedback loop
    if (is.function(set_metadata)) {
      metadata_write_back_guard$active <- TRUE
      set_metadata(final_meta)
    }
  })

  # --------------------------------------------------------------------------
  # b. Condition change observer (assign_rules integration)
  #    Watches for updated condition lists from the assign_rules module and
  #    forces the metadata table to re-render with the new dropdown options.
  #    Wrapped in tryCatch because modules_list may not always be present.
  # --------------------------------------------------------------------------

  if (!is.null(modules_list) && !is.null(modules_list$assign_rules_out)) {
    tryCatch({
      observeEvent(modules_list$assign_rules_out$condition_options(), {
        dynamic_conditions <- modules_list$assign_rules_out$condition_options()
        if (!is.null(dynamic_conditions) && length(dynamic_conditions) > 0) {
          debug_log(paste("Conditions updated:", paste(dynamic_conditions, collapse = ", ")), 1)
        }
        # Increment a dedicated refresh counter so metadata_table invalidates
        # and re-renders with the new condition_options dropdown source. The
        # freeze only suppresses the transient input echo from the replaced
        # widget; it does not block renderRHandsontable() from rebuilding the
        # dropdown choices with the new condition labels.
        mark_programmatic_metadata_sync()
        freezeReactiveValue(input, "metadata_table")
        metadata_options_refresh(metadata_options_refresh() + 1L)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }, error = function(e) {
      debug_log(paste("Error setting up condition observer:", e$message), 1)
    })
  } else {
    debug_log("Condition observer not registered: assign_rules_out unavailable", 2)
  }

  context$set_metadata_sync_state <- set_metadata_sync_state
  context$mark_programmatic_metadata_sync <- mark_programmatic_metadata_sync
  context$metadata_aligned_with_primary <- metadata_aligned_with_primary
  context$metadata_sync_alignment_error <- metadata_sync_alignment_error
  context$metadata_paste_contains_header_row <- metadata_paste_contains_header_row
  context$metadata_reference_df <- metadata_reference_df
  invisible(NULL)
  })
}
