# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Condition Rule Handlers
# Purpose: Register condition rule observers against the shared Auto-Assign context.
# ============================================================================

register_auto_assign_condition_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_condition_handlers requires an environment context")
  }

  evalq({
    observeEvent(input$cond_method_autoassign_dw, {
      tryCatch({
        if (!(input$cond_method_autoassign_dw %in% c("between", "end"))) {
          updateTextInput(session, "cond_before_autoassign_dw", value = "")
        }
        if (!(input$cond_method_autoassign_dw %in% c("between", "start"))) {
          updateTextInput(session, "cond_after_autoassign_dw", value = "")
        }
        if (!(input$cond_method_autoassign_dw %in% c("phrase_position", "pattern_detect"))) {
          updateCheckboxGroupInput(session, "cond_sep_chars_autoassign_dw", selected = character(0))
          updateNumericInput(session, "cond_pos_autoassign_dw", value = 1)
        }
      }, error = function(e) {
        debug_log(paste("Error in method change handler:", e$message), 1)
      })
    })
    # Modify selected condition rule
    observeEvent(
      input$add_condition_rule_autoassign_dw,
      {

        if (!can_process_click("add_condition")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_condition_rules_autoassign_dw(),
              "condition"
            )

          selected_id <-
            selected_condition_rule()

          if (is.null(selected_id) ||
              length(selected_id) != 1L ||
              !nzchar(selected_id)) {

            showNotification(
              "Select an existing condition rule to modify.",
              type = "warning",
              duration = 4
            )

            return()
          }

          target_row <- match(
            selected_id,
            rules$RuleId
          )

          if (is.na(target_row)) {

            showNotification(
              "The selected condition RuleId no longer exists.",
              type = "warning",
              duration = 4
            )

            return()
          }

          selected_row <- rules[
            target_row,
            ,
            drop = FALSE
          ]

          if (!identical(
            as.character(
              selected_row$Content[[1L]]
            ),
            as.character(
              input$cond_content_autoassign_dw
            )
          )) {

            showNotification(
              paste(
                "The selected condition RuleId does not",
                "belong to the displayed Content type."
              ),
              type = "warning",
              duration = 4
            )

            return()
          }

          modified_rule <-
            build_condition_rule_from_inputs(
              rule_id = selected_id,
              variant_id =
                selected_row$VariantId[[1L]]
            )

          rules[
            target_row,
            names(rules)
          ] <-
            modified_rule[
              1L,
              names(rules),
              drop = FALSE
            ]

          rv_condition_rules_autoassign_dw(
            upgrade_rule_component(
              rules,
              "condition"
            )
          )

          selected_condition_rule(
            selected_id
          )

          processing_duration <-
            as.numeric(
              difftime(
                Sys.time(),
                processing_start_time,
                units = "secs"
              )
            )

          add_processing_log(
            "modify_condition_rule",
            "success",
            paste(
              "Modified condition RuleId",
              selected_id
            ),
            processing_duration
          )

          showNotification(
            "Condition rule modified",
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error modifying condition rule:",
              e$message
            ),
            1
          )

          showNotification(
            "Error modifying condition rule",
            type = "error",
            duration = 5
          )
        })
      }
    )


    # Add genuinely new condition rule
    observeEvent(
      input$add_new_condition_rule_autoassign_dw,
      {

        if (!can_process_click("add_condition")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_condition_rules_autoassign_dw(),
              "condition"
            )

          variant_id <-
            selected_content_variant(
              input$cond_content_autoassign_dw
            )

          if (is.null(variant_id) ||
              !nzchar(variant_id)) {

            stop(
              paste(
                "Select a content rule variant before",
                "adding a condition rule."
              )
            )
          }

          next_rule_id <-
            next_user_auto_assign_rule_id(
              kind = "condition",
              existing_rule_ids =
                all_auto_assign_rule_ids()
            )

          new_rule <-
            build_condition_rule_from_inputs(
              rule_id = next_rule_id,
              variant_id = variant_id
            )

          rv_condition_rules_autoassign_dw(
            upgrade_rule_component(
              dplyr::bind_rows(
                rules,
                new_rule
              ),
              "condition"
            )
          )

          selected_condition_rule(
            next_rule_id
          )

          processing_duration <-
            as.numeric(
              difftime(
                Sys.time(),
                processing_start_time,
                units = "secs"
              )
            )

          add_processing_log(
            "add_new_condition_rule",
            "success",
            paste(
              "Added new user condition RuleId",
              next_rule_id,
              "for VariantId",
              variant_id
            ),
            processing_duration
          )

          showNotification(
            paste(
              "New condition rule added:",
              next_rule_id
            ),
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error adding new condition rule:",
              e$message
            ),
            1
          )

          showNotification(
            "Error adding new condition rule",
            type = "error",
            duration = 5
          )
        })
      }
    )

    # Enhanced remove condition rule
    observeEvent(input$remove_condition_rule_autoassign_dw, {

      tryCatch({

        df <- rv_condition_rules_autoassign_dw()

        selected_id <- selected_condition_rule()

        if (is.null(selected_id) ||
            length(selected_id) != 1L ||
            !nzchar(selected_id)) {

          showNotification(
            "Select a condition rule to remove.",
            type = "warning",
            duration = 4
          )

          return()
        }

        target_row <- match(
          selected_id,
          df$RuleId
        )

        if (is.na(target_row)) {

          showNotification(
            "The selected condition RuleId no longer exists.",
            type = "warning",
            duration = 4
          )

          return()
        }

        removed_rule <- df[
          target_row,
          ,
          drop = FALSE
        ]

        df <- df[
          -target_row,
          ,
          drop = FALSE
        ]

        rv_condition_rules_autoassign_dw(
          df
        )

        next_id <- resolve_variant_rule_id(
          df,
          removed_rule$Content[[1L]],
          removed_rule$VariantId[[1L]]
        )

        selected_condition_rule(
          next_id
        )

        if (!is.null(next_id)) {
          populate_condition_rule_ui(
            next_id
          )
        }

        add_processing_log(
          "remove_condition_rule",
          "success",
          paste(
            "Removed condition RuleId",
            selected_id,
            "for",
            removed_rule$Content[[1L]]
          )
        )

        showNotification(
          paste(
            "Removed condition rule",
            selected_id
          ),
          type = "message",
          duration = 3
        )

      }, error = function(e) {

        debug_log(
          paste(
            "Error removing condition rule:",
            e$message
          ),
          1
        )

        add_processing_log(
          "remove_condition_rule",
          "error",
          e$message
        )

        showNotification(
          "Error removing sample rule",
          type = "error",
          duration = 5
        )
      })
    })

    observeEvent(input$remove_condition_rule_row_click_autoassign_dw, {
      tryCatch({
        click_data <- input$remove_condition_rule_row_click_autoassign_dw
        rule_id <- as.character(click_data$ruleId)
        if (length(rule_id) != 1L || !nzchar(rule_id)) return()

        df <- rv_condition_rules_autoassign_dw()
        row_idx <- match(rule_id, df$RuleId)
        if (is.na(row_idx)) return()

        removed_content <- df$Content[[row_idx]]
        removed_variant <- df$VariantId[[row_idx]]
        df <- df[df$RuleId != rule_id, , drop = FALSE]
        rv_condition_rules_autoassign_dw(df)

        if (identical(
          selected_condition_rule(),
          rule_id
        )) {

          next_id <- resolve_variant_rule_id(
            df,
            removed_content,
            removed_variant
          )

          selected_condition_rule(
            next_id
          )

          if (!is.null(next_id)) {
            populate_condition_rule_ui(
              next_id
            )
          }
        }

        add_processing_log("remove_condition_rule_row", "success",
                           paste("Removed condition rule", rule_id, "for", removed_content))
        showNotification("Condition rule row removed", type = "message", duration = 3)
      }, error = function(e) {
        debug_log(paste("Error removing condition rule row:", e$message), 1)
        add_processing_log("remove_condition_rule_row", "error", e$message)
        showNotification("Error removing condition rule row", type = "error", duration = 5)
      })
    }, ignoreNULL = TRUE)

    # Update UI inputs when selecting existing sample rule
    observeEvent(input$cond_content_autoassign_dw, {
      df <- rv_condition_rules_autoassign_dw()
      current_id <- selected_condition_rule()
      current_row <- df[df$RuleId == current_id & df$Content == input$cond_content_autoassign_dw,
                        , drop = FALSE]
      variant_id <- if (nrow(current_row) == 1L) current_row$VariantId[[1L]] else
        selected_content_variant(input$cond_content_autoassign_dw)
      id <- resolve_variant_rule_id(df, input$cond_content_autoassign_dw, variant_id,
                                    current_id)
      selected_condition_rule(id)
      if (!is.null(id)) populate_condition_rule_ui(id)
      debug_log(paste("Updated UI for condition content type:", input$cond_content_autoassign_dw), 2)
    })


  }, envir = ctx)

  invisible(TRUE)
}
