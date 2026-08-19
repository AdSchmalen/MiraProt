auto_regex_register_run_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  shiny::observeEvent(input$infer_rules, {
    if (!identical(session$userData$auto_regex_handler_tokens[[handler_key]], handler_token))
      return()
    if (isTRUE(state$processing())) return()
    state$processing(TRUE)
    state$current_processing_stage("Preparing inference")
    outcome <- NULL
    active_run_id <- NULL
    terminal_event_id <- ""
    set_outcome <- function(value, stage, message, notification_type,
                            counts = list(), previous_rules_preserved = TRUE) {
      outcome <<- list(
        outcome = value,
        stage = stage,
        message = message,
        notification_type = notification_type,
        counts = counts,
        previous_rules_preserved = isTRUE(previous_rules_preserved)
      )
      invisible(outcome)
    }
    on.exit({
      state$processing(FALSE)
      state$current_processing_stage(NULL)
      # A missing outcome is reserved for defects outside the guarded stage
      # boundaries. Every expected terminal branch sets its own precise result.
      if (is.null(outcome)) {
        set_outcome("inference_failure", "unexpected_error",
          "Auto RegEx failed | unexpected internal error | previous rules preserved",
          "error")
        state$run_status("failed")
      }
      terminal_message <- outcome$message
      if (!is.null(active_run_id)) {
        terminal_event_id <- sprintf("auto-regex:%s:terminal", active_run_id)
        detail <- sub(
          "^Auto RegEx (failed|discarded|completed with warnings|succeeded) \\| ?",
          "", outcome$message)
        terminal_status <- switch(outcome$outcome,
          partial = "completed with warnings",
          success = "completed",
          stale = "discarded",
          "failed")
        if (outcome$outcome %in% c("partial", "success")) {
          terminal_message <- sprintf("Auto RegEx run %s %s | %s",
            active_run_id, terminal_status, detail)
        } else {
          terminal_message <- sprintf("Auto RegEx run %s %s | stage: %s | %s",
            active_run_id, terminal_status, outcome$stage, detail)
        }
        terminal_message <- sprintf("%s | run-id: %s | event-id: %s",
          terminal_message, active_run_id, terminal_event_id)
      }
      logger(terminal_message, 0L, run_id = active_run_id %||% "",
        event_id = terminal_event_id)
      shiny::showNotification(outcome$message, type = outcome$notification_type,
        duration = 6)
    }, add = TRUE)

    shiny::withProgress(message = "Inferring assignment rules", value = 0, {
      phase_values <- c(
        preprocessing_start = 0.02, preprocessing_complete = 0.10,
        content_start = 0.12, content_complete = 0.45,
        condition_start = 0.47, condition_complete = 0.68,
        ratio_start = 0.70, ratio_complete = 0.84,
        semantic_refinement_start = 0.85, semantic_refinement_complete = 0.90,
        payload_validation_start = 0.91, payload_validation_complete = 0.94,
        auto_assign_transfer_start = 0.94, auto_assign_transfer_complete = 1.00
      )
      phase_labels <- c(
        preprocessing_start = "Preprocessing and validation",
        preprocessing_complete = "Preprocessing and validation complete",
        content_start = "Inferring content rules",
        content_complete = "Content inference complete",
        condition_start = "Inferring condition rules",
        condition_complete = "Condition inference complete",
        ratio_start = "Inferring ratio rules",
        ratio_complete = "Ratio inference complete",
        semantic_refinement_start = "Generalizing dataset-specific labels",
        semantic_refinement_complete = "Dataset-specific label generalization complete",
        payload_validation_start = "Validating inferred payload",
        payload_validation_complete = "Payload validation complete",
        auto_assign_transfer_start = "Transferring rules to Auto-Assign",
        auto_assign_transfer_complete = "Auto-Assign transfer complete"
      )
      last_progress <- 0
      report_progress <- function(event) {
        event <- as.character(event)[[1L]]
        value <- unname(phase_values[event])
        if (!length(value) || is.na(value)) return(invisible(NULL))
        value <- max(last_progress, value)
        last_progress <<- value
        stage <- unname(phase_labels[event])
        state$current_processing_stage(stage)
        shiny::setProgress(value = value, detail = stage)
        invisible(NULL)
      }

      tryCatch({
      frozen_validation <- tryCatch({
        # Read the latest injected data/metadata/revision tuple exactly once.
        # Validation and inference below use only this frozen tuple.
        current <- if (identical(isolate(input$source %||% "current_metadata"),
                                 "current_metadata")) current_snapshot() else NULL
        frozen <- source_snapshot(current)
        if (identical(frozen$identity$mode, "current_metadata")) {
          readiness <- current$readiness
          if (!identical(readiness, "ready")) {
            detail <- switch(readiness,
              no_active_dataset = "no active dataset is available; load or select a dataset, then retry",
              metadata_unavailable = paste0("metadata is unavailable; open the metadata table and press ",
                "Synchronize metadata, then retry"),
              missing_column = paste0("metadata is missing the required Column field; open the metadata table ",
                "and press Synchronize metadata, then retry"),
              misaligned = paste0("metadata does not align with the active dataset; open the metadata table ",
                "and press Synchronize metadata, then retry"),
              duplicate_column = paste0("metadata contains duplicate Column values; resolve duplicate canonical ",
                "keys and synchronize metadata before inference"),
              pending_synchronization = paste0("metadata table changes are pending synchronization; press ",
                "Synchronize metadata before inference"),
              assignments_required = paste0("metadata has no meaningful Content assignments; assign Content ",
                "values, synchronize metadata, and retry"),
              "current metadata is not ready")
            diagnostic <- paste0("Current metadata readiness: ", detail, ".")
            state$errors(diagnostic)
            state$run_status("failed")
            set_outcome("validation_failure", "source_validation",
              sprintf("Auto RegEx failed | %s | previous rules preserved", detail),
              "error", list(validation_errors = 1L))
            return(NULL)
          }
        } else {
          mapping <- isolate(state$mapping())
          if (!is.data.frame(frozen$metadata) || is.null(mapping)) {
            state$errors("Select a worksheet and map its fields before inference.")
            state$run_status("failed")
            set_outcome("validation_failure", "source_validation",
              "Auto RegEx failed | select a worksheet and map its fields | previous rules preserved",
              "error", list(validation_errors = 1L))
            return(NULL)
          }
        }
        validation <- validate_metadata(frozen$metadata, names(frozen$metadata),
          if ("Options" %in% names(frozen$metadata)) "Options" else "")
        state$validation(validation)
        validation_errors <- validation[
          !is.na(validation$Severity) & tolower(validation$Severity) == "error", , drop = FALSE]
        if (nrow(validation_errors)) {
          messages <- as.character(validation_errors$Message)
          state$errors(messages)
          state$run_status("failed")
          set_outcome("validation_failure", "metadata_validation", sprintf(
            "Auto RegEx failed | metadata validation: %s%s | previous rules preserved",
            messages[[1L]], if (nrow(validation_errors) > 1L)
              sprintf(" (%d errors total)", nrow(validation_errors)) else ""),
            "error", list(validation_errors = nrow(validation_errors)))
          return(NULL)
        }
        list(frozen = frozen, validation = validation)
      }, error = function(e) {
        state$errors(conditionMessage(e)); state$run_status("failed")
        set_outcome("validation_failure", "source_validation",
          sprintf("Auto RegEx failed | source validation: %s | previous rules preserved",
            conditionMessage(e)), "error", list(validation_errors = 1L))
        NULL
      })
      if (is.null(frozen_validation)) return()

      frozen <-
        frozen_validation$frozen

      validation <-
        frozen_validation$validation

      # Read UI overrides and the expensive cache BEFORE begin_run() clears the
      # currently rendered candidate.
      current_candidate <-
        isolate(
          state$candidate_rules()
        )

      requested_overrides <-
        collect_content_redundancy_overrides(
          current_candidate
        )

      state$redundancy_overrides(
        requested_overrides
      )

      cached_redundancy_base <-
        isolate(
          state$redundancy_base()
        )

      reuse_redundancy_base <-
        is.list(
          cached_redundancy_base
        ) &&
        is.list(
          cached_redundancy_base$base$analysis_cache
        ) &&
        identical(
          cached_redundancy_base$
            source_signature,
          frozen$signature
        ) &&
        is.list(
          cached_redundancy_base$base
        ) &&
        is.list(
          cached_redundancy_base$result
        )

      run <- state$begin_run(
        frozen$identity$mode,
        frozen$signature
      )
      active_run_id <- run$run_id
      source_label <- if (identical(frozen$identity$mode, "excel"))
        "Excel metadata workbook" else "Current MiraProt metadata"
      start_event_id <- sprintf("auto-regex:%s:start", run$run_id)
      logger(sprintf("Auto RegEx run %s started | source: %s | run-id: %s | event-id: %s",
        run$run_id, source_label, run$run_id, start_event_id), 0L,
        run_id = run$run_id, event_id = start_event_id)
      run_logger <- function(message, level = 1L) {
        logger(message, level)
      }
      source_diag <- auto_regex_source_diagnostic(frozen$metadata, frozen$data)
      run_logger(sprintf(
        "Source diagnostic | active columns: %d | synchronized metadata rows: %d | recognized technical columns: %d [%s] | duplicate canonical metadata keys: %d [%s]",
        source_diag$active_columns, source_diag$metadata_rows,
        length(source_diag$technical), paste(source_diag$technical, collapse = ", "),
        length(source_diag$duplicates), paste(source_diag$duplicates, collapse = ", ")), 1L)
      reference_summary <- auto_regex_condition_reference_summary(
        frozen$metadata, "Options")
      run_logger(sprintf(
        "Condition inference readiness | target: %s | sample-bearing content labels: %s%s | applicable rows: %d | rows with nonempty references: %d | labels with no references: %s%s | status: %s",
        reference_summary$target, reference_summary$labels,
        if (reference_summary$labels_omitted > 0L)
          sprintf(" (+%d omitted)", reference_summary$labels_omitted) else "",
        reference_summary$applicable_rows,
        reference_summary$reference_rows,
        reference_summary$unavailable_labels_display,
        if (reference_summary$unavailable_labels_omitted > 0L)
          sprintf(" (+%d omitted)", reference_summary$unavailable_labels_omitted) else "",
        reference_summary$status), 2L)
      canonical_metadata <- if (identical(frozen$identity$mode, "current_metadata"))
        current$metadata else frozen$metadata
      transfer_check <- tryCatch({
        auto_regex_verify_condition_reference_transfer(
          canonical_metadata, frozen$metadata, reference_summary$target)
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(transfer_check)) {
        state$fail_run(run$run_id, run$source_fingerprint, transfer_check)
        state$errors(transfer_check)
        set_outcome("inference_failure", "source_consistency", sprintf(
          "Auto RegEx failed | source-transfer defect: %s | previous rules preserved",
          transfer_check), "error", list(inference_errors = 1L))
        return()
      }
      frozen_provenance <- auto_regex_source_provenance(
        frozen$identity$mode, frozen$metadata, frozen$data,
        if (identical(frozen$identity$mode, "current_metadata"))
          shared$provenance else NULL)
      result <- tryCatch({

        requested_redundancy <-
          isolate(
            input$redundancy %||%
              0L
          )

        requested_redundancy <-
          auto_regex_redundancy_value(
            requested_redundancy,
            fallback =
              shiny::isolate(
                state$global_redundancy()
              )
          )

        state$global_redundancy(
          requested_redundancy
        )

        run_logger(
          sprintf(
            paste0(
              "Regex redundancy request | global=%d | ",
              "per-rule overrides=%d | cached full inference=%s."
            ),
            requested_redundancy,
            length(requested_overrides),
            if (reuse_redundancy_base) {
              "yes"
            } else {
              "no"
            }
          ),
          2L
        )

        if (isTRUE(
          reuse_redundancy_base
        )) {

          report_progress(
            "semantic_refinement_start"
          )

          state$current_processing_stage(
            "Rebuilding Regex redundancy"
          )

          run_logger(
            paste(
              "Regex redundancy rebuild: loading cached RuleId redundancy ladders."
            ),
            1L
          )

          rebuild_started <-
            proc.time()[["elapsed"]]

          base <-
            cached_redundancy_base$base

          rebuilt <-
            auto_regex_apply_cached_content_redundancy(
              metadata =
                cached_redundancy_base$metadata,
              table =
                base$rules$table,
              cache =
                base$analysis_cache,
              redundancy =
                requested_redundancy,
              redundancy_overrides =
                requested_overrides
            )

          # Reuse the successful expensive inference result. Only Content-rule
          # representation and its redundancy diagnostics are replaced.
          refreshed <-
            cached_redundancy_base$
            result

          refreshed$rules <-
            base$rules

          refreshed$rules$table <-
            rebuilt$table

          refreshed$diagnostics$
            content_redundancy <-
            rebuilt$lineage

          refreshed$
            redundancy_base <-
            base

          refreshed$errors <-
            character()

          if (is.null(
            refreshed$timings
          )) {
            refreshed$timings <-
              numeric()
          }

          refreshed$timings[["redundancy_rebuild"]] <-
            (
              proc.time()[["elapsed"]] -
                rebuild_started
            ) * 1000

          report_progress(
            "semantic_refinement_complete"
          )

          run_logger(
            sprintf(
              paste0(
                "Cached Regex redundancy ",
                "rebuild completed in %.1f ms."
              ),
              refreshed$timings[["redundancy_rebuild"]]
            ),
            1L
          )

          refreshed

        } else {

          auto_regex_infer_rules(
            frozen$metadata,
            condition_target =
              if ("Options" %in%
                  names(frozen$metadata)) {
                "Options"
              } else {
                ""
              },
            redundancy =
              requested_redundancy,
            redundancy_overrides =
              requested_overrides,
            debug_log =
              run_logger,
            progress =
              report_progress,
            provenance =
              frozen_provenance
          )
        }

      }, error = function(e) {

        state$fail_run(
          run$run_id,
          run$source_fingerprint,
          conditionMessage(e)
        )

        set_outcome(
          "inference_failure",
          "inference_pipeline",
          sprintf(
            paste0(
              "Auto RegEx failed | inference: %s | ",
              "previous rules preserved"
            ),
            conditionMessage(e)
          ),
          "error",
          list(
            inference_errors = 1L
          )
        )

        NULL
      })
      if (is.null(result)) return()

      now <- source_snapshot()
      if (!identical(now$signature, frozen$signature)) {
        reason <- auto_regex_source_change_reason(frozen, now)
        auto_regex_invalidate_effective_source(state)
        state$errors(sprintf("The %s during inference; only this run was discarded.", reason))
        set_outcome("stale", "source_recheck", sprintf(
          "Auto RegEx discarded | %s | previous rules preserved",
          paste0(toupper(substr(reason, 1L, 1L)), substring(reason, 2L))),
          "warning")
        return()
      }
      if (length(result$errors)) {
        state$fail_run(run$run_id, run$source_fingerprint, result$errors,
          result$diagnostics, result$warnings, result$timings)
        set_outcome("inference_failure", "inference_pipeline", sprintf(
          "Auto RegEx failed | inference: %s%s | previous rules preserved",
          result$errors[[1L]], if (length(result$errors) > 1L)
            sprintf(" (%d errors total)", length(result$errors)) else ""),
          "error", list(inference_errors = length(result$errors)))
        return()
      }
      if (length(result$warnings)) run_logger(sprintf(
        "Inference produced %d warning(s): %s%s", length(result$warnings),
        paste(head(result$warnings, 3L), collapse = " | "),
        if (length(result$warnings) > 3L) " | additional warnings omitted" else ""), 1L)

      redundancy_cache <- NULL

      if (isTRUE(
        reuse_redundancy_base
      )) {

        redundancy_cache <-
          cached_redundancy_base

        # Preserve the new display/result diagnostics while keeping the exact
        # same immutable full-inference base.
        redundancy_cache$result <-
          result

      } else if (is.list(
        result$redundancy_base
      )) {

        redundancy_cache <- list(
          source_signature =
            frozen$signature,

          # Exact replay of alternative redundancy settings requires the same
          # frozen metadata that produced the base rules.
          metadata =
            frozen$metadata,

          # Pre-compaction Content table + final condition/ratio rules +
          # semantic spans.
          base =
            result$redundancy_base,

          # Expensive statuses/diagnostics/warnings are reused by later fast
          # rebuilds.
          result =
            result
        )
      }

      payload <- result$rules
      run_logger("Payload validation started.", 1L)
      report_progress("payload_validation_start")
      payload_validation_started <- proc.time()[["elapsed"]]
      payload_errors <- tryCatch(validate_export(payload, frozen$metadata),
        error = function(e) conditionMessage(e))
      run_logger(sprintf("Payload validation phase completed in %.1f ms.",
        (proc.time()[["elapsed"]] - payload_validation_started) * 1000), 2L)
      report_progress("payload_validation_complete")
      run_logger("Payload validation completed.", 1L)
      if (length(payload_errors)) {
        # Retain successful inference diagnostics as a candidate even though
        # its canonical payload is unsafe to transfer.
        state$complete_run(
          run$run_id,
          run$source_fingerprint,
          result$rules,
          result$diagnostics,
          result$warnings,
          result$timings,
          payload,
          validation,
          redundancy_cache
        )
        state$errors(unique(c(state$errors(), payload_errors)))
        state$run_status("failed")
        set_outcome("transfer_failure", "payload_validation", sprintf(
          "Auto RegEx failed | payload validation: %s%s | previous rules preserved",
          payload_errors[[1L]], if (length(payload_errors) > 1L)
            sprintf(" (%d errors total)", length(payload_errors)) else ""),
          "error", list(payload_errors = length(payload_errors)))
        return()
      }
      committed <- state$complete_run(
        run$run_id,
        run$source_fingerprint,
        result$rules,
        result$diagnostics,
        result$warnings,
        result$timings,
        payload,
        validation,
        redundancy_cache
      )
      if (!isTRUE(committed)) {
        set_outcome("stale", "candidate_commit",
          "Auto RegEx discarded | inference run was superseded | previous rules preserved",
          "warning")
        return()
      }

      transfer <- if (is.list(shared)) shared$transfer else NULL
      rule_state <- if (is.list(shared)) shared$rule_state else NULL
      if (!is.function(transfer) || !is.list(rule_state) ||
          !all(vapply(rule_state[c("table", "condition", "ratio")], is.function, logical(1)))) {
        state$errors("Auto-Assign rule transfer is unavailable.")
        state$run_status("failed")
        set_outcome("transfer_failure", "auto_assign_transfer",
          "Auto RegEx failed | Auto-Assign rule transfer is unavailable | previous rules preserved",
          "error")
        return()
      }

      # Recheck the complete frozen tuple immediately before the transactional
      # public transfer; inference and payload validation may have yielded long
      # enough for the active dataset, canonical commit, or revision to change.
      immediately_before_transfer <- source_snapshot()
      if (!identical(immediately_before_transfer$signature, frozen$signature)) {
        reason <- auto_regex_source_change_reason(frozen, immediately_before_transfer)
        auto_regex_invalidate_effective_source(state)
        state$errors(sprintf("The %s before transfer; only this run was discarded.", reason))
        set_outcome("stale", "pre_transfer_recheck", sprintf(
          "Auto RegEx discarded | %s | previous rules preserved",
          paste0(toupper(substr(reason, 1L, 1L)), substring(reason, 2L))), "warning")
        return()
      }

      # Snapshot and restore exclusively through Auto-Assign's public return
      # interface. The quiet loader remains responsible for its private state.
      previous <- lapply(rule_state[c("table", "condition", "ratio")],
        function(value) isolate(value()))
      matches <- function(expected) all(vapply(names(expected), function(component) {
        identical(isolate(rule_state[[component]]()), expected[[component]])
      }, logical(1)))
      transfer_error <- NULL
      report_progress("auto_assign_transfer_start")
      run_logger("Auto-Assign transfer started.", 1L)
      transfer_started <- proc.time()[["elapsed"]]
      loaded <- tryCatch(isTRUE(transfer(payload, notify = FALSE)), error = function(e) {
        transfer_error <<- conditionMessage(e); FALSE
      })
      verified <- loaded && matches(payload)
      if (!verified) {
        rollback_error <- NULL
        restored <- tryCatch(isTRUE(transfer(previous, notify = FALSE)), error = function(e) {
          rollback_error <<- conditionMessage(e); FALSE
        })
        restored <- restored && matches(previous)
        detail <- transfer_error %||% "inferred rules could not be loaded or verified"
        if (!restored) {
          rollback_detail <- rollback_error %||% "restored payload did not verify"
          run_logger(sprintf("Rollback failed: %s", rollback_detail), 1L)
          state$errors(unique(c(state$errors(), detail,
            paste("Auto-Assign rollback failed:", rollback_detail))))
          message <- sprintf(
            "Auto RegEx failed | transfer: %s | rollback failed: %s | previous rules NOT verified",
            detail, rollback_detail)
          preserved <- FALSE
        } else {
          state$errors(unique(c(state$errors(), detail,
            "Previous Auto-Assign rules were restored and verified.")))
          message <- sprintf(
            "Auto RegEx failed | transfer: %s | previous rules preserved", detail)
          preserved <- TRUE
        }
        state$run_status("failed")
        set_outcome("transfer_failure", "auto_assign_transfer", message, "error",
          list(content = nrow(payload$table), condition = nrow(payload$condition),
            ratio = nrow(payload$ratio)), preserved)
        return()
      }

      if (!isTRUE(state$complete_transfer(run$run_id, run$source_fingerprint, payload))) {
        # The source was superseded after loading. Restore the public snapshot
        # rather than allowing a stale candidate to become authoritative.
        rollback_error <- NULL
        restored <- tryCatch(isTRUE(transfer(previous, notify = FALSE)),
          error = function(e) {
            rollback_error <<- conditionMessage(e)
            FALSE
          })
        restored <- restored && matches(previous)
        if (!restored) run_logger(sprintf("Rollback failed: %s",
          rollback_error %||% "restored payload did not verify"), 1L)
        state$run_status(if (restored) "stale" else "failed")
        set_outcome("stale", "transfer_commit",
          if (restored)
            "Auto RegEx discarded | source changed before transfer commit | previous rules preserved" else
            "Auto RegEx failed | stale transfer rollback failed | previous rules NOT verified",
          if (restored) "warning" else "error",
          previous_rules_preserved = restored)
        return()
      }
      run_logger(sprintf("Auto-Assign transfer phase completed in %.1f ms.",
        (proc.time()[["elapsed"]] - transfer_started) * 1000), 2L)
      run_logger(sprintf("Transfer completed: %d content, %d condition, %d ratio rules.",
        nrow(payload$table), nrow(payload$condition), nrow(payload$ratio)), 1L)
      report_progress("auto_assign_transfer_complete")
      counts <- list(content = nrow(payload$table), condition = nrow(payload$condition),
        ratio = nrow(payload$ratio))
      refinement_counts <- result$diagnostics$refinement_counts
      if (is.data.frame(refinement_counts) && nrow(refinement_counts))
        counts$refinements <- refinement_counts$AcceptedRefinements[[1L]] else
        counts$refinements <- 0L
      partial <- length(result$warnings) > 0L
      set_outcome(if (partial) "partial" else "success", "complete", sprintf(
        "Auto RegEx %s | %d content, %d condition, and %d ratio rules transferred | %d semantic refinements accepted",
        if (partial) "completed with warnings" else "succeeded",
        counts$content, counts$condition, counts$ratio, counts$refinements),
        if (partial) "warning" else "message", counts,
        previous_rules_preserved = FALSE)
    }, error = function(e) {
      state$errors(unique(c(state$errors(), conditionMessage(e))))
      state$run_status("failed")
      set_outcome("inference_failure", "unexpected_error", sprintf(
        "Auto RegEx failed | unexpected error: %s | previous rules preserved",
        conditionMessage(e)), "error")
      })
    })
  }, ignoreInit = TRUE)

  invisible(NULL)
}
