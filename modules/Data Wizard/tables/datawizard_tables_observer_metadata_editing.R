# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_metadata_editing.R
# Purpose:
#   Provide the tables observer metadata editing portion of the Data Wizard without changing public behavior.
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

.datawizard_tables_browser_payload_aligned <- function(table_data, primary_data) {
  is.data.frame(primary_data) &&
    is.data.frame(table_data) &&
    metadata_matches_dataset(table_data, primary_data)
}

register_tables_metadata_editing <- function(context, request_primary_preview_rerender = NULL) {
  with(context, {
  current_handson_metadata <- context$current_handson_metadata
  metadata_options_refresh <- context$metadata_options_refresh
  metadata_notif_state <- context$metadata_notif_state
  metadata_write_back_guard <- context$metadata_write_back_guard
  suppress_metadata_edit_echo <- context$suppress_metadata_edit_echo
  programmatic_metadata_update_active <- context$programmatic_metadata_update_active
  metadata_sync_pending <- context$metadata_sync_pending
  metadata_sync_last_error <- context$metadata_sync_last_error
  set_metadata_sync_state <- context$set_metadata_sync_state
  mark_programmatic_metadata_sync <- context$mark_programmatic_metadata_sync
  metadata_aligned_with_primary <- context$metadata_aligned_with_primary
  metadata_sync_alignment_error <- context$metadata_sync_alignment_error
  metadata_paste_contains_header_row <- context$metadata_paste_contains_header_row
  metadata_reference_df <- context$metadata_reference_df

  # Debounced metadata source for non-mutating health checks. This avoids
  # expensive re-evaluation while the user is rapidly editing the table and,
  # because it only reads current_handson_metadata(), cannot trigger metadata
  # write-back loops.
  metadata_healthcheck_data <- debounce(reactive({
    metadata_revision_debounced()
    isolate(current_handson_metadata())
  }), millis = 800)

  # --------------------------------------------------------------------------
  # i. output$metadata_healthcheck
  #    Shows a debounced, read-only consistency check above the editable
  #    metadata table. The health check never writes metadata, which prevents
  #    recursive invalidation/write-back problems.
  # --------------------------------------------------------------------------

  output$metadata_healthcheck <- renderUI({
    metadata <- metadata_healthcheck_data()
    result <- tryCatch(
      build_metadata_healthcheck(metadata),
      error = function(e) {
        debug_log(paste("Metadata health check failed:", e$message), 1)
        list(
          status = "warning",
          summary = "Metadata health check could not be completed.",
          issues = "Please continue editing or reload the metadata table; details were written to the log."
        )
      }
    )

    status <- result$status
    alert_class <- if (identical(status, "ok")) {
      "alert alert-success"
    } else if (identical(status, "info")) {
      "alert alert-info"
    } else {
      "alert alert-warning"
    }
    icon_class <- if (identical(status, "ok")) {
      "fa fa-check-circle"
    } else if (identical(status, "info")) {
      "fa fa-info-circle"
    } else {
      "fa fa-exclamation-triangle"
    }

    if (length(result$issues) == 0) {
      return(div(
        class = alert_class,
        style = "padding: 8px 12px; margin-bottom: 10px;",
        tags$i(class = icon_class),
        tags$strong(style = "margin-left: 6px;", result$summary)
      ))
    }

    div(
      class = alert_class,
      style = "padding: 8px 12px; margin-bottom: 10px;",
      tags$i(class = icon_class),
      tags$strong(style = "margin-left: 6px;", result$summary),
      tags$ul(
        style = "margin: 6px 0 0 18px; padding-left: 0;",
        lapply(result$issues, tags$li)
      )
    )
  })

  # --------------------------------------------------------------------------
  # j. output$metadata_table
  #    Renders the rHandsontable for metadata editing. Dropdown choices for
  #    conditions are populated dynamically from the assign_rules module when
  #    available, with a static fallback otherwise.
  # --------------------------------------------------------------------------

  output$metadata_table <- renderRHandsontable({
    metadata_revision_debounced()
    metadata_options_refresh()
    df <- req(current_handson_metadata())

    tryCatch({
      # Old sessions may still contain the long-deprecated Custom field. Never
      # expose or perpetuate it in the canonical editable metadata table.
      df_display <- datawizard_drop_deprecated_metadata_columns(df)

      content_options <- datawizard_metadata_content_choices()

      transformation_options <- c(NA_character_, "None", "log2", "log10", "-log10")

      # Dynamic conditions from assign_rules module; static fallback if unavailable
      condition_options <- c(NA_character_, "Treatment_1", "Treatment_2", "Control")
      tryCatch({
        if (!is.null(modules_list) && !is.null(modules_list$assign_rules_out) &&
            !is.null(modules_list$assign_rules_out$condition_options) &&
            is.function(modules_list$assign_rules_out$condition_options)) {
          dynamic_conditions <- modules_list$assign_rules_out$condition_options()
          if (!is.null(dynamic_conditions) && length(dynamic_conditions) > 0) {
            condition_options <- dynamic_conditions
            debug_log(paste("Metadata table: using dynamic conditions:",
                            paste(dynamic_conditions, collapse = ", ")), 2)
          } else {
            debug_log("Metadata table: no dynamic conditions, using fallback", 2)
          }
        } else {
          debug_log("Metadata table: assign_rules_out unavailable, using fallback", 2)
        }
      }, error = function(e) {
        debug_log(paste("Metadata table: error getting dynamic conditions:", e$message), 1)
      })

      rhandsontable(df_display,
                    stretchH       = "all",
                    selectCallback = TRUE,
                    width          = "100%",
                    height         = 600) %>%
        hot_col("Column",         readOnly = TRUE) %>%
        hot_col("Content",        type = "dropdown", source = content_options) %>%
        hot_col("Options",        allowInvalid = FALSE, type = "dropdown",
                source = condition_options, stringsAsFactors = FALSE,
                readOnly = FALSE) %>%
        hot_col("Numerator",      type = "dropdown", source = condition_options,
                stringsAsFactors = FALSE, readOnly = FALSE) %>%
        hot_col("Denominator",    type = "dropdown", source = condition_options,
                stringsAsFactors = FALSE, readOnly = FALSE) %>%
        hot_col("Transformation", type = "dropdown",
                source = transformation_options) %>%
        hot_col("Sample",         type = "dropdown", source = NULL)

    }, error = function(e) {
      debug_log(paste("Error rendering metadata table:", e$message), 1)
      showNotification(paste("Error creating metadata table:", e$message),
                       type = "error")
      return(NULL)
    })
  })

  sync_current_metadata_to_core <- function(
    table_data,
    source = "manual metadata edit") {

    metadata_sync_last_error("")

    if (!is.function(set_metadata)) {

      metadata_sync_last_error(
        "the canonical metadata setter is unavailable"
      )

      return(FALSE)
    }

    if (!is.data.frame(table_data) ||
        nrow(table_data) == 0L) {

      metadata_sync_last_error(
        "the metadata table is empty"
      )

      return(FALSE)
    }

    alignment_error <-
      metadata_sync_alignment_error(
        table_data
      )

    if (nzchar(alignment_error)) {

      metadata_write_back_guard$active <- FALSE

      metadata_sync_last_error(
        alignment_error
      )

      debug_log(
        paste(
          "Metadata synchronization rejected from",
          source,
          ":",
          alignment_error
        ),
        1
      )

      return(FALSE)
    }

    tryCatch({

      metadata_write_back_guard$active <- TRUE

      set_metadata(
        table_data
      )

      canonical_after <-
        tryCatch(
          isolate(
            metadata_skeleton()
          ),
          error = function(e) NULL
        )

      if (!is.data.frame(canonical_after) ||
          !isTRUE(
            all.equal(
              canonical_after,
              table_data,
              check.attributes = FALSE
            )
          )) {

        stop(
          paste(
            "canonical metadata did not retain",
            "the synchronized table"
          ),
          call. = FALSE
        )
      }

      if (!is.null(session$userData)) {

        session$userData$current_metadata <-
          table_data

        session$userData$metadata_last_updated <-
          Sys.time()
      }

      metadata_sync_last_error("")

      debug_log(
        paste(
          "Metadata synced to core state from",
          source
        ),
        1
      )

      TRUE

    }, error = function(e) {

      metadata_write_back_guard$active <- FALSE

      reason <-
        conditionMessage(e)

      metadata_sync_last_error(
        reason
      )

      debug_log(
        paste(
          "Error syncing metadata to core state from",
          source,
          ":",
          reason
        ),
        1
      )

      FALSE
    })
  }

  # Transactional public entry point for programmatic metadata changes. This
  # keeps external callers out of the table's private reactive state.
  set_current_metadata <- function(metadata, source = "external update") {
    fail <- function(reason) list(success = FALSE, reason = reason,
                                  table_committed = FALSE, canonical_committed = FALSE)
    if (!is.data.frame(metadata)) return(fail("metadata is not a data frame"))
    metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
    if (nrow(metadata) == 0L) return(fail("metadata has no rows"))
    reference <- metadata_reference_df()
    if (!is.data.frame(reference) || !metadata_aligned_with_primary(metadata, reference)) {
      return(fail("metadata is not aligned with the current primary data"))
    }
    if (!is.function(set_metadata)) return(fail("canonical metadata setter is unavailable"))

    previous_table <- isolate(current_handson_metadata())
    previous_canonical <- tryCatch(isolate(metadata_skeleton()), error = function(e) NULL)
    mark_programmatic_metadata_sync()
    tryCatch({
      metadata_write_back_guard$active <- TRUE
      set_metadata(metadata)
      canonical_after <- tryCatch(isolate(metadata_skeleton()), error = function(e) NULL)
      if (!is.data.frame(canonical_after) ||
          !isTRUE(all.equal(canonical_after, metadata, check.attributes = FALSE))) {
        stop("canonical metadata did not accept the update")
      }
      current_handson_metadata(metadata)
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(paused = isTRUE(input$pause_metadata_sync), pending = FALSE)
      if (!is.null(session$userData)) {
        session$userData$current_metadata <- metadata
        session$userData$metadata_last_updated <- Sys.time()
      }
      freezeReactiveValue(input, "metadata_table")
      metadata_options_refresh(isolate(metadata_options_refresh()) + 1L)
      debug_log(paste("Programmatic metadata committed from", source), 1)
      list(success = TRUE, reason = "metadata committed",
           table_committed = TRUE, canonical_committed = TRUE)
    }, error = function(e) {
      try(current_handson_metadata(previous_table), silent = TRUE)
      if (is.data.frame(previous_canonical)) try(set_metadata(previous_canonical), silent = TRUE)
      metadata_write_back_guard$active <- FALSE
      debug_log(paste("Programmatic metadata commit failed from", source, ":", e$message), 1)
      fail(paste("commit failed:", e$message))
    })
  }

  # Install an already-committed session payload in the editable presentation
  # buffer. Canonical state is deliberately not written here: the Data Wizard
  # restore transaction owns that commit and passes this API the same payload.
  hydrate_restored_metadata <- function(metadata, reference_data,
                                        generation = NULL,
                                        source = "restored_metadata") {
    result <- function(success, action, reason = "") {
      list(success = success, action = action, reason = reason)
    }
    generation_label <- as.character(generation %||% NA_integer_)
    log_result <- function(action, rows = 0L) {
      debug_log(sprintf(
        "[TablesMetadata] session payload hydration: generation=%s rows=%d source=%s action=%s",
        generation_label, rows, source, action
      ), 1)
    }

    if (!is.data.frame(metadata) || !is.data.frame(reference_data)) {
      log_result("rejected_invalid_input", if (is.data.frame(metadata)) nrow(metadata) else 0L)
      return(result(FALSE, "rejected_invalid_input",
                    "metadata and reference_data must be data frames"))
    }
    metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
    if (!isTRUE(metadata_matches_dataset(metadata, reference_data))) {
      log_result("rejected_unaligned", nrow(metadata))
      return(result(FALSE, "rejected_unaligned",
                    "metadata is not aligned with the restored data"))
    }
    generation_is_current <- function() {
      is.null(generation) || is.null(rv) || identical(
        isolate(rv$session_restore_generation), generation
      )
    }
    if (!generation_is_current()) {
      log_result("rejected_stale_generation", nrow(metadata))
      return(result(FALSE, "rejected_stale_generation",
                    "restore generation is no longer current"))
    }

    current <- isolate(current_handson_metadata())
    if (is.data.frame(current) && isTRUE(all.equal(
      current, metadata, check.attributes = FALSE
    ))) {
      metadata_write_back_guard$active <- FALSE
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(paused = FALSE, pending = FALSE)
      log_result("noop_already_equal", nrow(metadata))
      return(result(TRUE, "noop_already_equal"))
    }
    if (!generation_is_current()) {
      log_result("rejected_stale_generation", nrow(metadata))
      return(result(FALSE, "rejected_stale_generation",
                    "restore generation is no longer current"))
    }

    mark_programmatic_metadata_sync()
    metadata_write_back_guard$active <- FALSE
    current_handson_metadata(metadata)
    metadata_sync_pending(FALSE)
    set_metadata_sync_state(paused = FALSE, pending = FALSE)
    freezeReactiveValue(input, "metadata_table")
    metadata_options_refresh(isolate(metadata_options_refresh()) + 1L)
    log_result("hydrated", nrow(metadata))
    result(TRUE, "hydrated")
  }

  observeEvent(input$pause_metadata_sync, {
    set_metadata_sync_state(paused = isTRUE(input$pause_metadata_sync),
                            pending = isTRUE(metadata_sync_pending()))
    if (isTRUE(input$pause_metadata_sync)) return()
    if (!isTRUE(metadata_sync_pending())) return()

    table_data <- current_handson_metadata()
    metadata_synced <- sync_current_metadata_to_core(
      table_data,
      source = "metadata sync re-enabled"
    )

    if (isTRUE(metadata_synced)) {
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(paused = FALSE, pending = FALSE)
      showNotification(
        "Metadata synchronized; automatic synchronization re-enabled.",
        type = "message",
        duration = 3
      )
    } else {

      metadata_sync_pending(TRUE)

      set_metadata_sync_state(
        paused = FALSE,
        pending = TRUE
      )

      reason <-
        isolate(
          metadata_sync_last_error()
        )

      message <-
        if (nzchar(reason)) {
          paste(
            "Metadata could not be synchronized:",
            reason
          )
        } else {
          paste(
            "Metadata could not be synchronized;",
            "automatic synchronization remains pending."
          )
        }

      showNotification(
        message,
        type = "warning",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$sync_metadata_now, {
    table_data <- current_handson_metadata()

    if (!is.data.frame(table_data) || nrow(table_data) == 0) {
      showNotification(
        "Metadata could not be synchronized. Please check the log.",
        type = "warning",
        duration = 3
      )
      return()
    }

    metadata_synced <- sync_current_metadata_to_core(
      table_data,
      source = "manual sync button"
    )

    if (isTRUE(metadata_synced)) {
      metadata_sync_pending(FALSE)
      set_metadata_sync_state(pending = FALSE)
      if (is.function(request_primary_preview_rerender)) {
        request_primary_preview_rerender(source = "manual Synchronize metadata")
      }
      showNotification("Metadata synchronized.", type = "message", duration = 3)
    } else {

      reason <-
        isolate(
          metadata_sync_last_error()
        )

      message <-
        if (nzchar(reason)) {
          paste(
            "Metadata could not be synchronized:",
            reason
          )
        } else {
          "Metadata could not be synchronized. Please check the log."
        }

      showNotification(
        message,
        type = "warning",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # k. Metadata edit handler
  #    Converts handsontable edits back to a data frame, applies business rules
  #    (ratio rows, identifier rows), syncs to rv$data_def, and throttles
  #    user notifications to avoid spam during rapid edits.
  # --------------------------------------------------------------------------

  observeEvent(input$metadata_table, {
    if (isTRUE(programmatic_metadata_update_active())) {
      return()
    }

    if (isTRUE(suppress_metadata_edit_echo())) {
      debug_log("Metadata edit handler: ignoring programmatic table echo event", 2)
      return()
    }

    evt <- input$metadata_table
    if (is.null(evt$changes)) return()

    # handsontable emits many non-user change sources during render/load cycles.
    # Only "edit-like" sources should feed manual write-back logic.
    event_source <- tryCatch(evt$changes$source, error = function(e) NULL)
    if (!is.null(event_source)) {
      event_source <- tolower(as.character(event_source)[1])
      # Handsontable paste/autofill sources can come in extended forms
      # (e.g. "copypaste.paste", "autofill.fill"). Keep this conservative:
      # allow direct edit and edit-like sources, but continue to reject
      # render/load/programmatic sources.
      is_edit_like <- identical(event_source, "edit") ||
        grepl("paste", event_source, fixed = TRUE) ||
        grepl("autofill", event_source, fixed = TRUE)
      if (!is_edit_like) {
        debug_log(paste("Metadata edit handler: ignoring non-user source:", event_source), 2)
        return()
      }
    }

    # Additional hard guard: only proceed when at least one actual value delta
    # exists in the reported cell changes.
    raw_changes <- tryCatch(evt$changes$changes, error = function(e) NULL)
    has_real_delta <- FALSE
    if (!is.null(raw_changes) && length(raw_changes) > 0) {
      for (change in raw_changes) {
        if (length(change) < 4) next
        old_val <- change[[3]]
        new_val <- change[[4]]
        old_norm <- if (is.null(old_val) || (length(old_val) == 1 && is.na(old_val))) "" else as.character(old_val)
        new_norm <- if (is.null(new_val) || (length(new_val) == 1 && is.na(new_val))) "" else as.character(new_val)
        if (!identical(old_norm, new_norm)) {
          has_real_delta <- TRUE
          break
        }
      }
    }
    if (!has_real_delta) {
      debug_log("Metadata edit handler: no real cell delta detected, skipping", 2)
      return()
    }

    # Warn when a paste contains condition values that are not currently
    # registered in Assign Rules. Handsontable rejects such dropdown values,
    # so without this warning the pasted value can appear to disappear.
    if (!is.null(event_source) &&
        grepl("paste", event_source, fixed = TRUE) &&
        !is.null(raw_changes) &&
        length(raw_changes) > 0L) {

      configured_conditions <- tryCatch({
        if (!is.null(modules_list) &&
            !is.null(modules_list$assign_rules_out) &&
            is.function(modules_list$assign_rules_out$condition_options)) {

          values <-
            modules_list$assign_rules_out$condition_options()

          values <- trimws(
            as.character(values)
          )

          unique(
            values[
              !is.na(values) &
                nzchar(values)
            ]
          )

        } else {
          character(0)
        }
      }, error = function(e) {
        character(0)
      })

      if (length(configured_conditions) > 0L) {

        metadata_now <- tryCatch(
          isolate(current_handson_metadata()),
          error = function(e) NULL
        )

        metadata_fields <-
          if (is.data.frame(metadata_now)) {
            names(metadata_now)
          } else {
            character(0)
          }

        condition_fields <- c(
          "Options",
          "Numerator",
          "Denominator"
        )

        ratio_contents <- c(
          "Abundance Ratio",
          "Abundance Ratio p-Value",
          "Abundance Ratio Adj. p-Value"
        )

        invalid_values <- character(0)

        for (change in raw_changes) {
          if (length(change) < 4L) {
            next
          }

          property <- change[[2L]]

          field_name <-
            if (length(property) == 1L &&
                as.character(property) %in% metadata_fields) {

              as.character(property)

            } else {

              property_index <-
                suppressWarnings(
                  as.integer(property)
                )

              if (!is.na(property_index) &&
                  property_index >= 0L &&
                  property_index < length(metadata_fields)) {

                metadata_fields[[property_index + 1L]]

              } else {
                as.character(property)
              }
            }

          if (!field_name %in% condition_fields) {
            next
          }

          new_value <- change[[4L]]

          if (is.null(new_value) ||
              length(new_value) == 0L ||
              is.na(new_value[[1L]])) {
            next
          }

          new_value <-
            trimws(
              as.character(
                new_value[[1L]]
              )
            )

          if (!nzchar(new_value)) {
            next
          }

          # Options has two legitimate non-condition uses:
          # Identifier rows use the source column name, and ratio rows use
          # the fixed value "Ratio". Do not warn for those rows.
          if (identical(field_name, "Options") &&
              is.data.frame(metadata_now)) {

            row_index <-
              suppressWarnings(
                as.integer(
                  change[[1L]]
                )
              ) + 1L

            if (!is.na(row_index) &&
                row_index >= 1L &&
                row_index <= nrow(metadata_now)) {

              row_content <-
                as.character(
                  metadata_now$Content[[row_index]]
                )

              if (identical(row_content, "Identifier") ||
                  row_content %in% ratio_contents) {
                next
              }
            }
          }

          if (!new_value %in% configured_conditions) {
            invalid_values <- c(
              invalid_values,
              new_value
            )
          }
        }

        invalid_values <-
          unique(
            invalid_values
          )

        if (length(invalid_values) > 0L) {

          shown_values <-
            paste(
              utils::head(
                invalid_values,
                5L
              ),
              collapse = ", "
            )

          if (length(invalid_values) > 5L) {
            shown_values <-
              paste0(
                shown_values,
                ", ..."
              )
          }

          showNotification(
            paste0(
              "Some pasted condition values are not defined in Condition Groups ",
              "and cannot be accepted by the metadata table: ",
              shown_values,
              ". Add these conditions under Conditions & Metadata Loader first, ",
              "then paste the metadata again."
            ),
            type = "warning",
            duration = 5
          )

          debug_log(
            paste(
              "Metadata paste contained undefined condition value(s):",
              paste(
                invalid_values,
                collapse = ", "
              )
            ),
            1
          )
        }
      }
    }

    if (metadata_paste_contains_header_row(
      event_source,
      raw_changes
    )) {

      debug_log(
        paste(
          "Metadata paste rejected:",
          "a spreadsheet header row was detected in the pasted values."
        ),
        1
      )

      # The authoritative local reactive has not been changed yet.
      # Force the browser widget to redraw from that unchanged state so the
      # rejected client-side paste does not remain visible.
      mark_programmatic_metadata_sync()

      freezeReactiveValue(
        input,
        "metadata_table"
      )

      metadata_options_refresh(
        isolate(
          metadata_options_refresh()
        ) + 1L
      )

      showNotification(
        paste(
          "Metadata was not pasted because the clipboard includes",
          "the metadata header row. Copy and paste only the data rows."
        ),
        type = "warning",
        duration = 5
      )

      return()
    }

    tryCatch({
      table_data <- hot_to_r(evt)

      table_data <- table_data %>%
        mutate(
          Content        = ifelse(Content == "", NA_character_, Content),
          Options        = ifelse(is.na(Content), NA_character_, Options),
          Numerator      = ifelse(is.na(Content), NA_character_, Numerator),
          Denominator    = ifelse(is.na(Content), NA_character_, Denominator),
          Transformation = ifelse(is.na(Content), NA_character_, Transformation),
          Sample         = ifelse(is.na(Content), NA_character_, Sample)
        )

      # Ratio content types: force Options = "Ratio" and default Transformation = "None"
      ratio_terms <- c(
        "Abundance Ratio",
        "Abundance Ratio p-Value",
        "Abundance Ratio Adj. p-Value"
      )
      ratio_rows <- !is.na(table_data$Content) & table_data$Content %in% ratio_terms
      table_data$Options[ratio_rows] <- "Ratio"

      transform_empty <- is.na(table_data$Transformation) |
        table_data$Transformation == ""
      table_data$Transformation[ratio_rows & transform_empty] <- "None"

      # Identifier rows: force Options = Column value
      id_rows <- !is.na(table_data$Content) & table_data$Content == "Identifier"
      table_data$Options[id_rows] <- table_data$Column[id_rows]

      # A hidden/replaced Handsontable can echo its old browser value after a
      # restore installed a new canonical dataset. Reject it before mutation.
      # Genuine edits preserve the current dataset's exact Column identity.
      current_primary <- tryCatch(metadata_reference_df(), error = function(e) NULL)
      if (!.datawizard_tables_browser_payload_aligned(table_data, current_primary)) {
        debug_log(sprintf(
          "[TablesMetadata] stale table payload ignored metadata_rows=%d current_data_cols=%d",
          if (is.data.frame(table_data)) nrow(table_data) else 0L,
          if (is.data.frame(current_primary)) ncol(current_primary) else 0L
        ), 1)
        mark_programmatic_metadata_sync()
        freezeReactiveValue(input, "metadata_table")
        metadata_options_refresh(isolate(metadata_options_refresh()) + 1L)
        return()
      }

      existing_table <- tryCatch(current_handson_metadata(), error = function(e) NULL)
      if (!is.null(existing_table) &&
          isTRUE(all.equal(table_data, existing_table, check.attributes = FALSE))) {
        debug_log("Metadata edit handler: no effective metadata diff, skipping", 2)
        return()
      }

      if (!is.null(rv) && isTRUE(rv$session_restoring) &&
          restore_has_valid_canonical_pair(tryCatch(rv$data_mod, error = function(e) NULL),
                                           tryCatch(rv$data_def, error = function(e) NULL))) {
        current_data_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
        current_data_def <- tryCatch(rv$data_def, error = function(e) NULL)
        if (!metadata_matches_dataset(table_data, current_data_mod) ||
            (!is_meaningful_metadata(table_data) && is_meaningful_metadata(current_data_def))) {
          debug_log("Metadata edit handler: rejected restore-time metadata write that would replace the valid canonical pair", 1)
          return()
        }
      }

      current_handson_metadata(table_data)

      # Determine whether this change is a genuine user edit (not a
      # programmatic re-render echoing rule-applied metadata back).
      # Compare against both skeleton and final metadata sources.
      core_meta <- tryCatch(metadata_skeleton(), error = function(e) NULL)
      metadata_changed <- is.null(core_meta) ||
        !isTRUE(all.equal(table_data, core_meta, check.attributes = FALSE))

      if (metadata_changed) {
        final_meta <- tryCatch(metadata_final(), error = function(e) NULL)
        if (!is.null(final_meta)) {
          metadata_changed <- !isTRUE(all.equal(table_data, final_meta, check.attributes = FALSE))
        }
      }
      metadata_written <- FALSE
      metadata_sync_paused <- metadata_changed && isTRUE(input$pause_metadata_sync)

      # Write-back to canonical core state so that subsequent data
      # manipulations (imputation, batch correction, etc.) use the
      # user-customized metadata as their base, not stale auto-assigned values.
      # The guard flag is set before write-back and cleared by the skeleton
      # observer after it skips the resulting invalidation.
      # Only set the guard when the change is a genuine user edit. When pause
      # mode is active, keep the local table state above but defer canonical
      # core synchronization until the user explicitly resumes/syncs.
      if (metadata_changed) {
        if (metadata_sync_paused) {
          metadata_sync_pending(TRUE)
          set_metadata_sync_state(paused = TRUE, pending = TRUE)
          debug_log("Metadata edit handler: metadata edit pending synchronization", 1)
        } else {
          metadata_sync_pending(FALSE)
          set_metadata_sync_state(paused = FALSE, pending = FALSE)
          metadata_written <- sync_current_metadata_to_core(table_data, "manual metadata edit")
        }
      }

      # Sync changes to rv$data_def for other modules
      tryCatch({
        if (!is.null(rv) &&
            !isTRUE(rv$session_restoring) &&
            (is.null(rv$restore_phase %||% NULL) || identical(rv$restore_phase %||% NULL, "complete"))) {
          if (metadata_changed && !metadata_sync_paused && !isTRUE(metadata_written)) {
            metadata_written <- sync_current_metadata_to_core(table_data, "manual metadata edit fallback")
          }
          if (metadata_sync_paused) {
            debug_log("Manual metadata changes kept local while synchronization is paused", 1)
          } else {
            debug_log("Manual metadata changes propagated through state adapter", 1)
          }

          if (!is.null(evt$changes$changes)) {
            for (change in evt$changes$changes) {
              if (length(change) >= 4) {
                debug_log(paste("Manual edit: row", change[[1]], "->", change[[4]]), 2)
              }
            }
          }

          # Only notify when the change is a genuine user edit, not a
          # programmatic re-render echoing back rule-applied metadata
          if (metadata_changed && !metadata_sync_paused) {
            now     <- Sys.time()
            elapsed <- as.numeric(difftime(now, metadata_notif_state$last, units = "secs"))

            if (elapsed >= 3) {
              msg <- "Metadata updated"
              if (metadata_notif_state$suppressed > 0) {
                total <- metadata_notif_state$suppressed + 1L
                msg   <- paste0("Metadata updated (", total, " rapid edits)")
                metadata_notif_state$suppressed <- 0L
              }
              showNotification(msg, type = "message", duration = 3)
              metadata_notif_state$last <- now
            } else {
              metadata_notif_state$suppressed <- metadata_notif_state$suppressed + 1L
            }
          }

        } else {
          debug_log("rv not available for metadata sync", 1)
        }
      }, error = function(e) {
        debug_log(paste("Error syncing metadata to rv:", e$message), 1)
      })

    }, error = function(e) {
      warning("Handsontable to R conversion failed: ", conditionMessage(e))
      showNotification("Error updating metadata table. Please check your entries.",
                       type = "warning")
    })
  })

  list(
    set_current_metadata = set_current_metadata,
    hydrate_restored_metadata = hydrate_restored_metadata
  )

  })
}
