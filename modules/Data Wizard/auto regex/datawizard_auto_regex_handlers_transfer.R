auto_regex_register_transfer_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  shiny::observeEvent(
    input$transfer_rules,
    {

      if (!identical(
        session$userData$auto_regex_handler_tokens[[handler_key]],
        handler_token
      )) {
        return()
      }

      if (isTRUE(
        state$processing()
      )) {
        return()
      }

      payload <-
        shiny::isolate(
          state$candidate_payload()
        )

      if (!is.list(payload)) {

        shiny::showNotification(
          "There are no Auto RegEx rules to transfer.",
          type = "warning",
          duration = 5
        )

        return()
      }

      run_id <-
        shiny::isolate(
          state$completed_run_id()
        )

      source_fingerprint <-
        shiny::isolate(
          state$run_source_fingerprint()
        )

      context <-
        shiny::isolate(
          state$run_context()
        )

      if (is.null(run_id) ||
          is.null(source_fingerprint) ||
          is.null(context)) {

        shiny::showNotification(
          "The Auto RegEx candidate has no valid completed run context.",
          type = "error",
          duration = 6
        )

        return()
      }

      current_source <-
        tryCatch(
          source_snapshot(),
          error = function(e) NULL
        )

      if (is.null(current_source) ||
          !identical(
            current_source$signature,
            context$source_fingerprint
          )) {

        auto_regex_invalidate_effective_source(
          state
        )

        logger(
          "Manual rule transfer rejected because the Auto RegEx source changed.",
          1L
        )

        shiny::showNotification(
          "The metadata source changed. Infer rules again before transferring.",
          type = "warning",
          duration = 6
        )

        return()
      }

      payload_errors <-
        tryCatch(
          validate_export(
            payload,
            current_source$metadata
          ),
          error = function(e) {
            conditionMessage(e)
          }
        )

      if (length(payload_errors)) {

        logger(
          sprintf(
            "Manual rule transfer validation failed: %s",
            payload_errors[[1L]]
          ),
          1L
        )

        shiny::showNotification(
          paste(
            "Rule transfer validation failed:",
            payload_errors[[1L]]
          ),
          type = "error",
          duration = 8
        )

        return()
      }

      transfer <-
        if (is.list(shared)) {
          shared$transfer
        } else {
          NULL
        }

      rule_state <-
        if (is.list(shared)) {
          shared$rule_state
        } else {
          NULL
        }

      required_components <-
        c(
          "table",
          "condition",
          "ratio"
        )

      if (!is.function(transfer) ||
          !is.list(rule_state) ||
          !all(
            vapply(
              rule_state[required_components],
              is.function,
              logical(1)
            )
          )) {

        shiny::showNotification(
          "Auto-Assign rule transfer is unavailable.",
          type = "error",
          duration = 6
        )

        return()
      }

      state$processing(TRUE)
      state$current_processing_stage(
        "Transferring rules to Auto-Assign"
      )

      on.exit(
        {
          state$processing(FALSE)
          state$current_processing_stage(NULL)
        },
        add = TRUE
      )

      previous <-
        lapply(
          rule_state[required_components],
          function(value) {
            shiny::isolate(
              value()
            )
          }
        )

      matches <- function(expected) {

        all(
          vapply(
            names(expected),
            function(component) {

              identical(
                shiny::isolate(
                  rule_state[[component]]()
                ),
                expected[[component]]
              )
            },
            logical(1)
          )
        )
      }

      logger(
        sprintf(
          paste0(
            "Manual Auto RegEx transfer started | ",
            "%d content, %d condition, %d ratio rules."
          ),
          nrow(payload$table),
          nrow(payload$condition),
          nrow(payload$ratio)
        ),
        1L
      )

      transfer_error <- NULL

      loaded <-
        tryCatch(
          isTRUE(
            transfer(
              payload,
              notify = FALSE
            )
          ),
          error = function(e) {

            transfer_error <<-
              conditionMessage(e)

            FALSE
          }
        )

      verified <-
        loaded &&
        matches(payload)

      if (!verified) {

        rollback_error <- NULL

        restored <-
          tryCatch(
            isTRUE(
              transfer(
                previous,
                notify = FALSE
              )
            ),
            error = function(e) {

              rollback_error <<-
                conditionMessage(e)

              FALSE
            }
          )

        restored <-
          restored &&
          matches(previous)

        detail <-
          transfer_error %||%
          "candidate rules could not be loaded or verified"

        if (restored) {

          logger(
            sprintf(
              paste0(
                "Manual Auto RegEx transfer failed: %s | ",
                "previous Auto-Assign rules restored."
              ),
              detail
            ),
            1L
          )

          shiny::showNotification(
            paste(
              "Transfer failed; previous Auto-Assign rules were restored:",
              detail
            ),
            type = "error",
            duration = 8
          )

        } else {

          rollback_detail <-
            rollback_error %||%
            "restored payload did not verify"

          logger(
            sprintf(
              paste0(
                "Manual Auto RegEx transfer failed: %s | ",
                "rollback failed: %s."
              ),
              detail,
              rollback_detail
            ),
            1L
          )

          shiny::showNotification(
            paste(
              "Transfer and rollback failed:",
              detail
            ),
            type = "error",
            duration = 8
          )
        }

        return()
      }

      committed <-
        state$complete_transfer(
          run_id,
          source_fingerprint,
          payload
        )

      if (!isTRUE(committed)) {

        rollback_error <- NULL

        restored <-
          tryCatch(
            isTRUE(
              transfer(
                previous,
                notify = FALSE
              )
            ),
            error = function(e) {

              rollback_error <<-
                conditionMessage(e)

              FALSE
            }
          )

        restored <-
          restored &&
          matches(previous)

        if (!restored) {

          logger(
            sprintf(
              "Manual transfer commit failed and rollback failed: %s.",
              rollback_error %||%
                "restored payload did not verify"
            ),
            1L
          )
        }

        shiny::showNotification(
          if (restored) {
            "The Auto RegEx source became stale; previous Auto-Assign rules were restored."
          } else {
            "The Auto RegEx source became stale and rollback could not be verified."
          },
          type = "error",
          duration = 8
        )

        return()
      }

      logger(
        sprintf(
          paste0(
            "Manual Auto RegEx transfer completed | ",
            "%d content, %d condition, %d ratio rules."
          ),
          nrow(payload$table),
          nrow(payload$condition),
          nrow(payload$ratio)
        ),
        1L
      )

      shiny::showNotification(
        "Current Auto RegEx rules transferred to Auto-Assign.",
        type = "message",
        duration = 5
      )
    },
    ignoreInit = TRUE
  )
  invisible(NULL)
}
