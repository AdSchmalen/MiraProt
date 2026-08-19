# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Outputs
# Purpose:
#   Register output renderers for Auto-Assign status and rules tables.
# Architectural Role:
#   Output registration layer for orchestrator-delegated server rendering.
# Responsibilities:
#   - Register renderText/renderDT outputs in module server context.
#   - Preserve output IDs and rendering behavior.
# Non-Responsibilities:
#   - Must not own module lifecycle or observer event registration.
# ============================================================================

register_auto_assign_outputs <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_outputs requires an environment context")
  }

  evalq({
    make_remove_button_html <- function(input_id, rule_id) {
      sprintf(
        paste0(
          "<button class='btn btn-danger btn-xs' style='padding:1px 6px; font-size:11px; line-height:1.4;' ",
          "onclick=\"Shiny.setInputValue('%s', {ruleId:'%s', nonce:Date.now()}, {priority:'event'});\">",
          "<i class='fa fa-times'></i></button>"
        ),
        input_id,
        htmltools::htmlEscape(rule_id, attribute = TRUE)
      )
    }

    make_rule_id_display <- function(rule_id) {

      value <-
        if (is.null(rule_id) ||
            length(rule_id) != 1L ||
            is.na(rule_id)) {
          ""
        } else {
          as.character(rule_id)
        }

      shiny::div(
        class = "form-group",
        shiny::tags$label(
          "Rule ID:"
        ),
        shiny::tags$input(
          type = "text",
          class = "form-control",
          value = value,
          readonly = "readonly",
          style = paste(
            "font-family: monospace;",
            "background-color: #f5f5f5;"
          )
        )
      )
    }


    output$content_rule_id_display_autoassign_dw <-
      shiny::renderUI({

        make_rule_id_display(
          selected_content_rule()
        )
      })


    output$condition_rule_id_display_autoassign_dw <-
      shiny::renderUI({

        make_rule_id_display(
          selected_condition_rule()
        )
      })


    output$ratio_rule_id_display_autoassign_dw <-
      shiny::renderUI({

        make_rule_id_display(
          selected_ratio_rule()
        )
      })

    output$template_status <- renderText({
      tryCatch({
        status_lines <- character()

        n_table <- nrow(rv_table_rules_autoassign_dw())
        n_condition <- nrow(rv_condition_rules_autoassign_dw())
        n_ratio <- nrow(rv_rules_autoassign_dw())

        status_lines <- c(status_lines, "Assignment Rules:")
        status_lines <- c(status_lines, paste("- Content rules:", n_table))
        status_lines <- c(status_lines, paste("- Sample rules:", n_condition))
        status_lines <- c(status_lines, paste("- Ratio rules:", n_ratio))

        return(paste(status_lines, collapse = "\n"))

      }, error = function(e) {
        debug_log(paste("Error rendering template status:", e$message), 1)
        return(paste("Error rendering status:", e$message))
      })
    })

    output$table_rules_content_dw <- DT::renderDT({
      tryCatch({
        df <- rv_table_rules_autoassign_dw()
        df$Remove <- vapply(
          seq_len(nrow(df)),
          function(i) make_remove_button_html(ns("remove_content_rule_row_click_autoassign_dw"), df$RuleId[[i]]),
          character(1)
        )
        df
      }, error = function(e) {
        debug_log(paste("Error rendering content rules table:", e$message), 1)
        data.frame(Error = "Could not load content rules table")
      })
    },
    rownames = FALSE,
    escape = FALSE,
    selection = 'single',
    options = list(
      dom = 't',
      scrollY = "150px",
      scrollCollapse = TRUE,
      paging = FALSE
    ))

    output$condition_rules_table_autoassign_dw <- DT::renderDT({
      tryCatch({
        df <- rv_condition_rules_autoassign_dw()
        df$Remove <- vapply(
          seq_len(nrow(df)),
          function(i) make_remove_button_html(ns("remove_condition_rule_row_click_autoassign_dw"), df$RuleId[[i]]),
          character(1)
        )
        df
      }, error = function(e) {
        debug_log(paste("Error rendering condition rules table:", e$message), 1)
        data.frame(Error = "Could not load condition rules table")
      })
    },
    rownames = FALSE,
    escape = FALSE,
    selection = 'single',
    options = list(
      dom = 't',
      scrollY = "150px",
      scrollCollapse = TRUE,
      paging = FALSE
    ))

    output$rules_table_ratio_autoassign_dw <- DT::renderDT({
      tryCatch({
        df <- rv_rules_autoassign_dw()
        df$Remove <- vapply(
          seq_len(nrow(df)),
          function(i) make_remove_button_html(ns("remove_ratio_rule_row_click_autoassign_dw"), df$RuleId[[i]]),
          character(1)
        )
        df
      }, error = function(e) {
        debug_log(paste("Error rendering ratio rules table:", e$message), 1)
        data.frame(Error = "Could not load ratio rules table")
      })
    },
    rownames = FALSE,
    escape = FALSE,
    selection = 'single',
    options = list(
      dom = 't',
      scrollY = "150px",
      scrollCollapse = TRUE,
      paging = FALSE
    ))

    # These outputs live inside a modal whose DOM is removed when the modal is
    # closed. Keep the cheap authoritative-state views active so reopening the
    # modal reconnects to current server state instead of a suspended snapshot.
    modal_state_outputs <- c(
      "content_rule_id_display_autoassign_dw",
      "condition_rule_id_display_autoassign_dw",
      "ratio_rule_id_display_autoassign_dw",
      "template_status",
      "table_rules_content_dw",
      "condition_rules_table_autoassign_dw",
      "rules_table_ratio_autoassign_dw"
    )

    for (output_id in modal_state_outputs) {
      shiny::outputOptions(
        output,
        output_id,
        suspendWhenHidden = FALSE
      )
    }

  }, envir = ctx)

  invisible(TRUE)
}
