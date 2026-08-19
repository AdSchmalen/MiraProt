# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Ratio Rule Handlers
# Purpose: Register ratio rule observers against the shared Auto-Assign context.
# ============================================================================

register_auto_assign_ratio_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_ratio_handlers requires an environment context")
  }

  evalq({
    # Modify selected ratio rule
    observeEvent(
      input$add_rule_autoassign_dw,
      {

        if (!can_process_click("add_ratio")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_rules_autoassign_dw(),
              "ratio"
            )

          selected_id <-
            selected_ratio_rule()

          if (is.null(selected_id) ||
              length(selected_id) != 1L ||
              !nzchar(selected_id)) {

            showNotification(
              "Select an existing ratio rule to modify.",
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
              "The selected ratio RuleId no longer exists.",
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
              input$new_content_autoassign_dw
            )
          )) {

            showNotification(
              paste(
                "The selected ratio RuleId does not",
                "belong to the displayed Content type."
              ),
              type = "warning",
              duration = 4
            )

            return()
          }

          modified_rule <-
            build_ratio_rule_from_inputs(
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

          rv_rules_autoassign_dw(
            upgrade_rule_component(
              rules,
              "ratio"
            )
          )

          selected_ratio_rule(
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
            "modify_ratio_rule",
            "success",
            paste(
              "Modified ratio RuleId",
              selected_id
            ),
            processing_duration
          )

          showNotification(
            "Ratio rule modified",
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error modifying ratio rule:",
              e$message
            ),
            1
          )

          showNotification(
            "Error modifying ratio rule",
            type = "error",
            duration = 5
          )
        })
      }
    )


    # Add genuinely new ratio rule
    observeEvent(
      input$add_new_ratio_rule_autoassign_dw,
      {

        if (!can_process_click("add_ratio")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_rules_autoassign_dw(),
              "ratio"
            )

          variant_id <-
            selected_content_variant(
              input$new_content_autoassign_dw
            )

          if (is.null(variant_id) ||
              !nzchar(variant_id)) {

            stop(
              paste(
                "Select a content rule variant before",
                "adding a ratio rule."
              )
            )
          }

          next_rule_id <-
            next_user_auto_assign_rule_id(
              kind = "ratio",
              existing_rule_ids =
                all_auto_assign_rule_ids()
            )

          new_rule <-
            build_ratio_rule_from_inputs(
              rule_id = next_rule_id,
              variant_id = variant_id
            )

          rv_rules_autoassign_dw(
            upgrade_rule_component(
              dplyr::bind_rows(
                rules,
                new_rule
              ),
              "ratio"
            )
          )

          selected_ratio_rule(
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
            "add_new_ratio_rule",
            "success",
            paste(
              "Added new user ratio RuleId",
              next_rule_id,
              "for VariantId",
              variant_id
            ),
            processing_duration
          )

          showNotification(
            paste(
              "New ratio rule added:",
              next_rule_id
            ),
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error adding new ratio rule:",
              e$message
            ),
            1
          )

          showNotification(
            "Error adding new ratio rule",
            type = "error",
            duration = 5
          )
        })
      }
    )

    # Enhanced remove ratio rule
    observeEvent(input$remove_rule_autoassign_dw, {

      tryCatch({

        df <- rv_rules_autoassign_dw()

        selected_id <- selected_ratio_rule()

        if (is.null(selected_id) ||
            length(selected_id) != 1L ||
            !nzchar(selected_id)) {

          showNotification(
            "Select a ratio rule to remove.",
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
            "The selected ratio RuleId no longer exists.",
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

        rv_rules_autoassign_dw(
          df
        )

        next_id <- resolve_variant_rule_id(
          df,
          removed_rule$Content[[1L]],
          removed_rule$VariantId[[1L]]
        )

        selected_ratio_rule(
          next_id
        )

        if (!is.null(next_id)) {
          populate_ratio_rule_ui(
            next_id
          )
        }

        add_processing_log(
          "remove_ratio_rule",
          "success",
          paste(
            "Removed ratio RuleId",
            selected_id,
            "for",
            removed_rule$Content[[1L]]
          )
        )

        showNotification(
          paste(
            "Removed ratio rule",
            selected_id
          ),
          type = "message",
          duration = 3
        )

      }, error = function(e) {

        debug_log(
          paste(
            "Error removing ratio rule:",
            e$message
          ),
          1
        )

        add_processing_log(
          "remove_ratio_rule",
          "error",
          e$message
        )

        showNotification(
          "Error removing ratio rule",
          type = "error",
          duration = 5
        )
      })
    })

    observeEvent(input$remove_ratio_rule_row_click_autoassign_dw, {
      tryCatch({
        click_data <- input$remove_ratio_rule_row_click_autoassign_dw
        rule_id <- as.character(click_data$ruleId)
        if (length(rule_id) != 1L || !nzchar(rule_id)) return()

        df <- rv_rules_autoassign_dw()
        row_idx <- match(rule_id, df$RuleId)
        if (is.na(row_idx)) return()

        removed_content <- df$Content[[row_idx]]
        removed_variant <- df$VariantId[[row_idx]]
        df <- df[df$RuleId != rule_id, , drop = FALSE]
        if (identical(
          selected_ratio_rule(),
          rule_id
        )) {

          next_id <- resolve_variant_rule_id(
            df,
            removed_content,
            removed_variant
          )

          selected_ratio_rule(
            next_id
          )

          if (!is.null(next_id)) {
            populate_ratio_rule_ui(
              next_id
            )
          }
        }

        add_processing_log("remove_ratio_rule_row", "success",
                           paste("Removed ratio rule", rule_id, "for", removed_content))
        showNotification("Ratio rule row removed", type = "message", duration = 3)
      }, error = function(e) {
        debug_log(paste("Error removing ratio rule row:", e$message), 1)
        add_processing_log("remove_ratio_rule_row", "error", e$message)
        showNotification("Error removing ratio rule row", type = "error", duration = 5)
      })
    }, ignoreNULL = TRUE)

    # Update UI inputs when selecting existing ratio rule
    observeEvent(input$new_content_autoassign_dw, {
      df <- rv_rules_autoassign_dw()
      current_id <- selected_ratio_rule()
      current_row <- df[df$RuleId == current_id & df$Content == input$new_content_autoassign_dw,
                        , drop = FALSE]
      variant_id <- if (nrow(current_row) == 1L) current_row$VariantId[[1L]] else
        selected_content_variant(input$new_content_autoassign_dw)
      id <- resolve_variant_rule_id(df, input$new_content_autoassign_dw, variant_id,
                                    current_id)
      selected_ratio_rule(id)
      if (!is.null(id)) populate_ratio_rule_ui(id)
      debug_log(paste("Updated UI for ratio content type:", input$new_content_autoassign_dw), 2)
    })


  }, envir = ctx)

  invisible(TRUE)
}
