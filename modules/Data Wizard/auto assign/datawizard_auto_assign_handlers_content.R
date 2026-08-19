# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Content Rule Handlers
# Purpose: Register content rule observers against the shared Auto-Assign context.
# ============================================================================

register_auto_assign_content_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_content_handlers requires an environment context")
  }

  evalq({
    # Modify selected content rule
    observeEvent(
      input$add_table_rule_autoassign_dw,
      {

        if (!can_process_click("add_table")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_table_rules_autoassign_dw(),
              "content"
            )

          selected_id <-
            selected_content_rule()

          if (is.null(selected_id) ||
              length(selected_id) != 1L ||
              !nzchar(selected_id)) {

            showNotification(
              "Select an existing content rule to modify.",
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
              "The selected content RuleId no longer exists.",
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

          if (identical(
            selected_row$Content[[1L]],
            "Row Index"
          )) {

            showNotification(
              "The Row Index rule cannot be modified here.",
              type = "warning",
              duration = 4
            )

            return()
          }

          if (!identical(
            as.character(
              selected_row$Content[[1L]]
            ),
            as.character(
              input$lookup_content_dw
            )
          )) {

            showNotification(
              paste(
                "The selected RuleId does not belong to",
                "the displayed Content type."
              ),
              type = "warning",
              duration = 4
            )

            return()
          }

          modified_rule <-
            build_content_rule_from_inputs(
              rule_id = selected_id,
              variant_id =
                selected_row$VariantId[[1L]],
              priority =
                selected_row$Priority[[1L]]
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

          rv_table_rules_autoassign_dw(
            upgrade_rule_component(
              rules,
              "content"
            )
          )

          selected_content_rule(
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
            "modify_table_rule",
            "success",
            paste(
              "Modified content RuleId",
              selected_id
            ),
            processing_duration
          )

          showNotification(
            "Content rule modified",
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error modifying content rule:",
              e$message
            ),
            1
          )

          add_processing_log(
            "modify_table_rule",
            "error",
            e$message
          )

          showNotification(
            "Error modifying content rule",
            type = "error",
            duration = 5
          )
        })
      }
    )


    # Add a genuinely new content rule
    observeEvent(
      input$add_new_content_rule_autoassign_dw,
      {

        if (!can_process_click("add_table")) {
          return()
        }

        tryCatch({

          processing_start_time <- Sys.time()

          rules <-
            upgrade_rule_component(
              rv_table_rules_autoassign_dw(),
              "content"
            )

          next_variant_id <-
            tail(
              stable_variant_ids(
                c(
                  as.character(
                    rules$Content
                  ),
                  input$lookup_content_dw
                ),
                c(
                  as.character(
                    rules$VariantId
                  ),
                  ""
                )
              ),
              1L
            )

          next_rule_id <-
            next_user_auto_assign_rule_id(
              kind = "content",
              existing_rule_ids =
                all_auto_assign_rule_ids()
            )

          next_priority <-
            max(
              c(
                0L,
                suppressWarnings(
                  as.integer(
                    rules$Priority
                  )
                )
              ),
              na.rm = TRUE
            ) + 1L

          new_rule <-
            build_content_rule_from_inputs(
              rule_id = next_rule_id,
              variant_id = next_variant_id,
              priority = next_priority
            )

          next_rules <-
            dplyr::bind_rows(
              rules,
              new_rule
            )

          rv_table_rules_autoassign_dw(
            upgrade_rule_component(
              next_rules,
              "content"
            )
          )

          selected_content_rule(
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
            "add_new_table_rule",
            "success",
            paste(
              "Added new user content RuleId",
              next_rule_id,
              "with VariantId",
              next_variant_id
            ),
            processing_duration
          )

          showNotification(
            paste(
              "New content rule added:",
              next_rule_id
            ),
            type = "message",
            duration = 3
          )

        }, error = function(e) {

          debug_log(
            paste(
              "Error adding new content rule:",
              e$message
            ),
            1
          )

          add_processing_log(
            "add_new_table_rule",
            "error",
            e$message
          )

          showNotification(
            "Error adding new content rule",
            type = "error",
            duration = 5
          )
        })
      }
    )

    # Enhanced remove table rule
    observeEvent(input$remove_table_rule_autoassign_dw, {

      tryCatch({

        df <- rv_table_rules_autoassign_dw()

        selected_id <- selected_content_rule()

        if (is.null(selected_id) ||
            length(selected_id) != 1L ||
            !nzchar(selected_id)) {

          showNotification(
            "Select a content rule to remove.",
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
            "The selected content RuleId no longer exists.",
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

        rv_table_rules_autoassign_dw(df)

        next_id <- resolve_content_rule_id(
          df,
          removed_rule$Content[[1L]]
        )

        selected_content_rule(
          next_id
        )

        if (!is.null(next_id)) {
          populate_content_rule_ui(
            next_id
          )
        }

        add_processing_log(
          "remove_table_rule",
          "success",
          paste(
            "Removed content RuleId",
            selected_id,
            "for",
            removed_rule$Content[[1L]]
          )
        )

        showNotification(
          paste(
            "Removed content rule",
            selected_id
          ),
          type = "message",
          duration = 3
        )

      }, error = function(e) {

        debug_log(
          paste(
            "Error removing table rule:",
            e$message
          ),
          1
        )

        add_processing_log(
          "remove_table_rule",
          "error",
          e$message
        )

        showNotification(
          "Error removing content rule",
          type = "error",
          duration = 5
        )
      })
    })

    observeEvent(input$remove_content_rule_row_click_autoassign_dw, {
      tryCatch({
        click_data <- input$remove_content_rule_row_click_autoassign_dw
        rule_id <- as.character(click_data$ruleId)
        if (length(rule_id) != 1L || !nzchar(rule_id)) return()

        df <- rv_table_rules_autoassign_dw()
        row_idx <- match(rule_id, df$RuleId)
        if (is.na(row_idx)) return()

        removed_content <- df$Content[row_idx]
        df <- df[df$RuleId != rule_id, , drop = FALSE]
        rv_table_rules_autoassign_dw(df)
        if (identical(selected_content_rule(), rule_id)) {
          next_id <- resolve_content_rule_id(df, removed_content)
          selected_content_rule(next_id)
          if (!is.null(next_id)) populate_content_rule_ui(next_id)
        }

        add_processing_log("remove_table_rule_row", "success",
                           paste("Removed content rule", rule_id, "for", removed_content))
        showNotification("Content rule row removed", type = "message", duration = 3)
      }, error = function(e) {
        debug_log(paste("Error removing content rule row:", e$message), 1)
        add_processing_log("remove_table_rule_row", "error", e$message)
        showNotification("Error removing content rule row", type = "error", duration = 5)
      })
    }, ignoreNULL = TRUE)

    # Update UI inputs when selecting existing table rule
    observeEvent(input$lookup_content_dw, {
      df <- rv_table_rules_autoassign_dw()
      id <- resolve_content_rule_id(df, input$lookup_content_dw, selected_content_rule())
      selected_content_rule(id)
      if (!is.null(id)) populate_content_rule_ui(id)
      debug_log(paste("Updated UI for content type:", input$lookup_content_dw), 2)
    })


  }, envir = ctx)

  invisible(TRUE)
}
