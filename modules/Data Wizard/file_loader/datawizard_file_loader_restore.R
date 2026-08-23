# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_restore.R
# Purpose:
#   Provide the file loader restore portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Mechanical observer/output-family extraction from datawizard_file_loader.R.
register_datawizard_file_loader_restore <- function(loader_environment = parent.frame()) {
  evalq({
    # ========================================
    # Session Save/Restore Bridge
    # ========================================
    # Writes the staged `pending_loader_state` payload into the loader's
    # reactiveVals and applies the saved input selections after the flush.
    #
    # Primary trigger: rv$session_restore_trigger, bumped at the end of
    # Phase 2 of the restore pipeline (see R/session_save_restore.R).
    #
    # Race hardening: if the trigger fires before set_session_state() stages
    # pending_loader_state (or if staging happens after the trigger callback),
    # apply the state as soon as staging exists and session_restoring is FALSE.
    apply_pending_loader_state <- function() {
      staged <- pending_loader_state()
      if (is.null(staged) || !is.list(staged)) return()
      if (isTRUE(applying_loader_restore)) return()

      staged_generation <- staged$restore_generation %||% NULL
      staged_signature <- staged$restore_signature %||% compute_loader_restore_signature(staged)
      same_generation <- !is.null(staged_generation) &&
        !is.null(last_applied_loader_restore_generation) &&
        identical(staged_generation, last_applied_loader_restore_generation)
      same_signature <- is.null(staged_generation) &&
        !is.null(staged_signature) &&
        !is.null(last_applied_loader_restore_signature) &&
        identical(staged_signature, last_applied_loader_restore_signature)
      if (isTRUE(same_generation || same_signature)) {
        debug_log(paste0(
          "Loader restore[", format_loader_restore_id(staged_generation, staged_signature),
          "]: skipping already-applied staged state"
        ), 2)
        pending_loader_state(NULL)
        return()
      }

      # A replay is one immutable, generation/signature-scoped settlement job.
      # Capture the list and its identity before any asynchronous flush is queued;
      # R's copy-on-write semantics then keep callbacks detached from later staging.
      st <- staged
      captured_generation <- staged_generation
      captured_signature <- staged_signature
      captured_payload_signature <- compute_loader_restore_signature(st)
      restore_job_api <- session$userData$restore_jobs %||% NULL
      restore_job_id <- if (!is.null(captured_generation) &&
          is.list(restore_job_api) && is.function(restore_job_api$register_restore_job)) {
        tryCatch(
          restore_job_api$register_restore_job(
            "Data Wizard file loader", "staged loader replay", "REPLAYING", 15
          ),
          error = function(e) NULL
        )
      } else NULL

      pending_loader_state(NULL)
      applying_loader_restore <<- TRUE
      settled <- FALSE

      identity_is_current <- function() {
        current_generation <- get_loader_restore_generation()
        generation_ok <- is.null(captured_generation) ||
          identical(as.integer(current_generation)[1L], as.integer(captured_generation)[1L])
        queued <- tryCatch(shiny::isolate(pending_loader_state()), error = function(e) NULL)
        queued_signature <- if (is.list(queued)) {
          queued$restore_signature %||% compute_loader_restore_signature(queued)
        } else captured_signature
        # The captured payload signature is immutable even though the private
        # working copy is normalized during replay. A newly staged signature
        # invalidates every callback belonging to this job.
        signature_ok <- identical(captured_signature, captured_payload_signature) &&
          identical(queued_signature, captured_signature)
        isTRUE(generation_ok && signature_ok)
      }

      # This is the sole owner of both the loader lock and restore-job outcome.
      # All success, stale, and failure paths converge here exactly once.
      settle_loader_restore <- function(outcome, applied = FALSE, error = NULL) {
        if (isTRUE(settled)) return(invisible(FALSE))
        # Validate the identity on every terminal path before unlocking or
        # asking the transaction registry to resolve this job.
        identity_current <- identity_is_current()
        settled <<- TRUE
        if (isTRUE(applied) && identity_current) {
          last_applied_loader_restore_generation <<- captured_generation
          last_applied_loader_restore_signature <<- captured_signature
        }
        applying_loader_restore <<- FALSE
        if (!is.null(restore_job_id) && is.function(restore_job_api$resolve_restore_job)) {
          restore_job_api$resolve_restore_job(restore_job_id, outcome, error)
        }
        invisible(TRUE)
      }

      run_restore_step <- function(reason, phase, callback) {
        if (!identity_is_current()) {
          settle_loader_restore("skipped", FALSE, "STALE_LOADER_RESTORE_IDENTITY")
          return(invisible(FALSE))
        }
        ok <- .run_session_restore_callback(
          owner = "Data Wizard file loader", reason = reason,
          generation = captured_generation %||% get_loader_restore_generation() %||% 0L,
          phase = phase, callback = callback,
          job_metadata = list(current_generation = function() {
            get_loader_restore_generation() %||% captured_generation %||% 0L
          })
        )
        if (!isTRUE(ok)) {
          # The shared runner explicitly records REACTIVE_CONTEXT_VIOLATION;
          # ordinary invalid-sheet/header/choice errors remain CALLBACK_ERROR.
          settle_loader_restore("failure", FALSE, paste0(reason, " callback failed"))
        }
        invisible(ok)
      }

      schedule_restore_flush <- function(reason, callback) {
        if (is.function(session$onFlushed)) {
          session$onFlushed(once = TRUE, function() {
            run_restore_step(reason, "flush", callback)
          })
        } else {
          run_restore_step(paste0(reason, " (fallback)"), "fallback", callback)
        }
      }

      tryCatch({
        if (!identity_is_current()) {
          settle_loader_restore("skipped", FALSE, "STALE_LOADER_RESTORE_IDENTITY")
          return(invisible(FALSE))
        }
        set_loader_mode("restore_replay", "apply pending loader state")
        restore_replay_active(TRUE)
        skip_next_restore_header_reprocess_primary(TRUE)
        skip_next_restore_header_reprocess_secondary(TRUE)
        # Backward-compatible defaults for older snapshots.
        st_inputs <- st$inputs %||% list()

        if (is_labels_only_loader_state(st)) {
          log_loader_restore_mode(st, "labels_only", TRUE)
          debug_log(paste0(
            "Loader restore[", format_loader_restore_id(staged_generation, staged_signature),
            "]: applying labels-only state"
          ), 2)
          primary_file_meta(st$primary_file_meta)
          secondary_file_meta(st$secondary_file_meta)
          selected_sheet_primary(st_inputs$sheetDropdown %||% NULL)
          selected_sheet_secondary(st_inputs$sheetDropdown2 %||% NULL)

          apply_labels_only_inputs <- function() {
            tryCatch({
              header1 <- suppressWarnings(as.integer(st_inputs$header_row %||% 1L))
              if (is.na(header1) || header1 < 1L) header1 <- 1L
              header2 <- suppressWarnings(as.integer(st_inputs$header_row2 %||% 1L))
              if (is.na(header2) || header2 < 1L) header2 <- 1L

              sheets1 <- get_sheet_choices_from_state(list(), st$primary_file_meta %||% list())
              selected1 <- st_inputs$sheetDropdown
              if (length(sheets1) > 0) {
                if (is.null(selected1) || !selected1 %in% sheets1) selected1 <- sheets1[[1]]
                # Labels-only restore must only replay UI labels/selections.
                # Arm the programmatic sheet skip as a fallback in case the
                # client update is delivered after restore_replay_active() is
                # released; do not enter cached-sheet parsing.
                skip_next_sheet_change_primary(selected1)
                updateSelectInput(session, "sheetDropdown", choices = sheets1, selected = selected1)
              }

              sheets2 <- get_sheet_choices_from_state(list(), st$secondary_file_meta %||% list())
              selected2 <- st_inputs$sheetDropdown2
              if (length(sheets2) > 0) {
                if (is.null(selected2) || !selected2 %in% sheets2) selected2 <- sheets2[[1]]
                skip_next_sheet_change_secondary(selected2)
                updateSelectInput(session, "sheetDropdown2", choices = sheets2, selected = selected2)
              }

              updateNumericInput(session, "header_row",  value = header1)
              updateNumericInput(session, "header_row2", value = header2)
              if (!is.null(st_inputs$data_source_type)) {
                updateRadioButtons(session, "data_source_type", selected = st_inputs$data_source_type)
              }

              schedule_restore_flush("labels-only settlement", function() {
                if (!identity_is_current()) {
                  settle_loader_restore("skipped", FALSE, "STALE_LOADER_RESTORE_IDENTITY")
                  return(invisible(FALSE))
                }
                restore_replay_active(FALSE)
                set_loader_mode("interactive_load", "labels-only restore settled")
                settle_loader_restore("success", applied = TRUE)
              })
            }, error = function(e) {
              if (identical(datawizard_condition_class(e), "reactive_context_violation")) stop(e)
              if (identity_is_current()) {
                restore_replay_active(FALSE)
                set_loader_mode("interactive_load", "labels-only restore failed")
              }
              settle_loader_restore("failure", applied = FALSE, error = e$message)
              debug_log(paste0(
                "Error applying labels-only loader inputs [",
                format_loader_restore_id(staged_generation, staged_signature),
                "]: ", e$message
              ), 1)
            })
          }

          # Preserve browser ordering: one flush rebuilds UI, the second
          # applies selections. Both callbacks cross the shared restore runner.
          schedule_restore_flush("labels-only UI rebuild", function() {
            schedule_restore_flush("labels-only input replay", apply_labels_only_inputs)
          })
          return(invisible(TRUE))
        }

        st$data_fixed <- st$data_fixed %||% st$primary_data_original
        st$data2_fixed <- st$data2_fixed %||% st$secondary_data_original
        st$sheet_cache_primary <- st$sheet_cache_primary %||% st$all_sheets_primary %||% list()
        st$sheet_cache_secondary <- st$sheet_cache_secondary %||% st$all_sheets_secondary %||% list()
        st$sheet_cache_primary <- normalize_sheet_cache(st$sheet_cache_primary, st$primary_file_meta$sheet_names %||% character(0))
        st$sheet_cache_secondary <- normalize_sheet_cache(st$sheet_cache_secondary, st$secondary_file_meta$sheet_names %||% character(0))
        debug_log(paste0(
          "Loader restore[", format_loader_restore_id(staged_generation, staged_signature),
          "]: applying staged state (primary sheets=",
          length(loaded_sheet_names(st$sheet_cache_primary)),
          ", secondary sheets=",
          length(loaded_sheet_names(st$sheet_cache_secondary)),
          ", has_secondary_data=",
          !is.null(st$data2_fixed %||% st$secondary_data_original),
          ")"
        ), 2)
        selected_sheet_primary(st_inputs$sheetDropdown %||% NULL)
        selected_sheet_secondary(st_inputs$sheetDropdown2 %||% NULL)

        # If metadata was not captured (older snapshots), infer a minimal
        # shape from sheet-cache presence to ensure dropdown UI can render.
        if (is.null(st$primary_file_meta) && length(get_sheet_choices_from_state(st$sheet_cache_primary, list())) > 0) {
          st$primary_file_meta <- list(
            name = NA_character_,
            ext = "xlsx",
            n_sheets = length(get_sheet_choices_from_state(st$sheet_cache_primary, list())),
            sheet_names = get_sheet_choices_from_state(st$sheet_cache_primary, list())
          )
        }
        if (is.null(st$secondary_file_meta) && length(get_sheet_choices_from_state(st$sheet_cache_secondary, list())) > 0) {
          st$secondary_file_meta <- list(
            name = NA_character_,
            ext = "xlsx",
            n_sheets = length(get_sheet_choices_from_state(st$sheet_cache_secondary, list())),
            sheet_names = get_sheet_choices_from_state(st$sheet_cache_secondary, list())
          )
        }

        if (!is.null(st$primary_file_meta) && is.null(st$primary_file_meta$sheet_names) &&
            length(get_sheet_choices_from_state(st$sheet_cache_primary, list())) > 0) {
          st$primary_file_meta$sheet_names <- get_sheet_choices_from_state(st$sheet_cache_primary, list())
          st$primary_file_meta$n_sheets <- length(st$primary_file_meta$sheet_names)
        }
        if (!is.null(st$secondary_file_meta) && is.null(st$secondary_file_meta$sheet_names) &&
            length(get_sheet_choices_from_state(st$sheet_cache_secondary, list())) > 0) {
          st$secondary_file_meta$sheet_names <- get_sheet_choices_from_state(st$sheet_cache_secondary, list())
          st$secondary_file_meta$n_sheets <- length(st$secondary_file_meta$sheet_names)
        }

        # Backward compatibility: older snapshots may only persist
        # data_fixed/data2_fixed. Rebuild a one-sheet runtime cache in that
        # case; current full Data Wizard snapshots carry all loaded workbook
        # sheets so restored sessions can switch sheets without a re-upload.
        restored_primary_sheet <- st_inputs$sheetDropdown %||% selected_sheet_primary()
        if (length(loaded_sheet_names(st$sheet_cache_primary)) == 0 && !is.null(restored_primary_sheet) &&
            !is.null(st$data_fixed)) {
          tmp_cache <- normalize_sheet_cache(st$sheet_cache_primary, st$primary_file_meta$sheet_names %||% restored_primary_sheet)
          tmp_reactive <- reactiveVal(tmp_cache)
          cache_loaded_sheet(tmp_reactive, restored_primary_sheet, st$data_fixed, "Primary restore")
          st$sheet_cache_primary <- tmp_reactive()
        }
        restored_secondary_sheet <- st_inputs$sheetDropdown2 %||% selected_sheet_secondary()
        if (length(loaded_sheet_names(st$sheet_cache_secondary)) == 0 && !is.null(restored_secondary_sheet) &&
            !is.null(st$data2_fixed)) {
          tmp_cache2 <- normalize_sheet_cache(st$sheet_cache_secondary, st$secondary_file_meta$sheet_names %||% restored_secondary_sheet)
          tmp_reactive2 <- reactiveVal(tmp_cache2)
          cache_loaded_sheet(tmp_reactive2, restored_secondary_sheet, st$data2_fixed, "Additional restore")
          st$sheet_cache_secondary <- tmp_reactive2()
        }

        skip_publish_working_data <- isTRUE(st$restore_skip_publish_working_data) ||
          !is.null(st$restore_generation) ||
          (!is.null(rv) && !is.null(rv$session_restore_generation))
        log_loader_restore_mode(st, "full", skip_publish_working_data)

        data_fixed(st$data_fixed)
        data2_fixed(st$data2_fixed)
        if (!is.null(st$data_fixed))  data_primary(st$data_fixed)
        if (!is.null(st$data2_fixed)) data_additional(st$data2_fixed)
        if (skip_publish_working_data) {
          debug_log(paste0(
            "Loader restore[", format_loader_restore_id(staged_generation, staged_signature),
            "]: restored loader-local state without publishing working data"
          ), 1)
          if (!is.null(rv) && !is.null(st$data_fixed)) {
            rv$primary_data_raw <- st$data_fixed
          }
          if (!is.null(core_values) &&
              is.function(core_values$primary_data_raw) &&
              !is.null(st$data_fixed)) {
            core_values$primary_data_raw(st$data_fixed)
          }
        } else {
          if (!is.null(st$data_fixed)) {
            publish_primary_current_sheet(st$data_fixed, "session restore current primary sheet")
          }
          if (!is.null(st$data2_fixed)) {
            publish_secondary_current_sheet(st$data2_fixed, "session restore current secondary sheet")
          }
        }
        if (!is.null(st$primary_data_original)) {
          primary_data_state$set_dataset("primary_original", st$primary_data_original, source = "session restore original primary sheet", allow_original_update = TRUE)
        }
        if (!is.null(st$secondary_data_original)) {
          primary_data_state$set_dataset("secondary_original", st$secondary_data_original, source = "session restore original secondary sheet", allow_original_update = TRUE)
        }
        sheet_cache_primary(st$sheet_cache_primary)
        sheet_cache_secondary(st$sheet_cache_secondary)
        primary_data_original(st$primary_data_original)
        secondary_data_original(st$secondary_data_original)
        primary_file_meta(st$primary_file_meta)
        secondary_file_meta(st$secondary_file_meta)

        # Expose originals on rv so the rest of the app (and a post-restore
        # Reset click) can find them without needing the loader handle.
        if (!is.null(rv)) {
          if (!is.null(st$primary_data_original)) {
            rv$primary_data_original <- st$primary_data_original
          }
          if (!is.null(st$secondary_data_original)) {
            rv$secondary_data_original <- st$secondary_data_original
          }
        }

        if (!is.null(st$data_fixed)) {
          shinyjs::show(id = ns("header_sheet_1"), asis = TRUE)
        }
        if (!is.null(st$data2_fixed)) {
          shinyjs::show(id = ns("header_sheet_2"), asis = TRUE)
        }

        # Apply input selections after the renderUI outputs rebuild the
        # dropdowns against the new cached choices. Nested onFlushed: the
        # outer one lets the reactiveVal writes above propagate through
        # the sheetDropdown / sheetDropdown2 renderUI; the inner one delays
        # update*Input until after the rebuilt UI block has been pushed to
        # the client so updateSelectInput(selected=...) lands on a dropdown
        # whose choices already include the saved value. Without the
        # second flush Shiny silently discards the selected sheet and the
        # header_row update races the renderUI rebuild.
        apply_inputs <- function() {
          tryCatch({
            inputs <- st_inputs
          header1 <- suppressWarnings(as.integer(inputs$header_row %||% 1L))
          if (is.na(header1) || header1 < 1L) header1 <- 1L
          header2 <- suppressWarnings(as.integer(inputs$header_row2 %||% 1L))
          if (is.na(header2) || header2 < 1L) header2 <- 1L

          sheets1 <- get_sheet_choices_from_state(st$sheet_cache_primary, st$primary_file_meta)
          selected1 <- inputs$sheetDropdown
          if (length(sheets1) > 0) {
            if (is.null(selected1) || !selected1 %in% sheets1) selected1 <- sheets1[[1]]
            skip_next_cached_sheet_apply_primary(TRUE)
            updateSelectInput(session, "sheetDropdown",
                              choices  = sheets1,
                              selected = selected1)
          }
          sheets2 <- get_sheet_choices_from_state(st$sheet_cache_secondary, st$secondary_file_meta)
          selected2 <- inputs$sheetDropdown2
          if (length(sheets2) > 0) {
            if (is.null(selected2) || !selected2 %in% sheets2) selected2 <- sheets2[[1]]
            skip_next_cached_sheet_apply_secondary(TRUE)
            updateSelectInput(session, "sheetDropdown2",
                              choices  = sheets2,
                              selected = selected2)
          }
          updateNumericInput(session, "header_row",  value = header1)
          updateNumericInput(session, "header_row2", value = header2)
          # Keep restore guard active through at least one additional flush so
          # observer cascades triggered by the restored header inputs cannot
          # re-enter process_data_with_header()/clean_and_index and rebuild
          # Row Index (which would reset metadata on restore).
          schedule_restore_flush("loader replay settlement", function() {
            if (!identity_is_current()) {
              settle_loader_restore("skipped", FALSE, "STALE_LOADER_RESTORE_IDENTITY")
              return(invisible(FALSE))
            }
            restore_replay_active(FALSE)
            set_loader_mode("interactive_load", "restore replay settled")
            settle_loader_restore("success", applied = TRUE)
          })
            debug_log(paste0(
              "Loader restore[", format_loader_restore_id(staged_generation, staged_signature),
              "]: session state applied: header_row=",
              header1,
              " sheetDropdown=",
              (selected1 %||% "NA"),
              " (choices=", length(sheets1), ")",
              " header_row2=", header2,
              " sheetDropdown2=",
              (selected2 %||% "NA"),
              " (choices=", length(sheets2), ")"
            ), 2)
          }, error = function(e) {
            if (identical(datawizard_condition_class(e), "reactive_context_violation")) stop(e)
            if (identity_is_current()) {
              restore_replay_active(FALSE)
              set_loader_mode("interactive_load", "restore replay failed")
            }
            settle_loader_restore("failure", applied = FALSE, error = e$message)
            debug_log(paste0(
              "Error applying loader session inputs [",
              format_loader_restore_id(staged_generation, staged_signature),
              "]: ", e$message
            ), 1)
          })
        }
        schedule_restore_flush("loader UI rebuild", function() {
          schedule_restore_flush("loader input replay", apply_inputs)
        })
      }, error = function(e) {
        if (identical(datawizard_condition_class(e), "reactive_context_violation")) {
          debug_log(paste0("Loader restore reactive-context violation: ", e$message), 1)
        }
        if (identity_is_current()) restore_replay_active(FALSE)
        settle_loader_restore("failure", applied = FALSE, error = e$message)
        debug_log(paste0(
          "Error applying loader session state [",
          format_loader_restore_id(staged_generation, staged_signature),
          "]: ", e$message
        ), 1)
      })
    }

    observeEvent(rv$session_restore_trigger, {
      apply_pending_loader_state()
    }, ignoreInit = TRUE)

    observe({
      st <- pending_loader_state()
      if (is.null(st) || !is.list(st)) return()
      if (is.null(rv) || isTRUE(rv$session_restoring) || restore_phase_active()) return()
      apply_pending_loader_state()
    })
  }, envir = loader_environment)
  invisible(NULL)
}
