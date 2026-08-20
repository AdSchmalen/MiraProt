# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Handlers
# Purpose:
#   Own registration of Auto-Assign observer/download handlers and keep
#   input-driven reactive mutation logic outside of orchestrator lifecycle code.
# Architectural Role:
#   Event wiring layer for Auto-Assign internals.
# Responsibilities:
#   - Register observeEvent/download handlers in module server context.
#   - Preserve existing event timing and side-effect behavior.
# Non-Responsibilities:
#   - Must not own module entrypoint lifecycle (moduleServer/UI exports).
# ============================================================================

register_auto_assign_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_auto_assign_handlers requires an environment context")
  }

  evalq({

    # Prevent multiple rapid clicks
    button_click_tracking <- reactiveValues(
      last_add_table = NULL,
      last_add_condition = NULL,
      last_add_ratio = NULL
    )

    # Check if enough time has passed since last click
    can_process_click <- function(button_name, min_interval = 0.5) {
      current_time <- Sys.time()
      last_time <- button_click_tracking[[paste0("last_", button_name)]]

      if (is.null(last_time)) {
        button_click_tracking[[paste0("last_", button_name)]] <- current_time
        return(TRUE)
      }

      time_diff <- as.numeric(difftime(current_time, last_time, units = "secs"))
      if (time_diff >= min_interval) {
        button_click_tracking[[paste0("last_", button_name)]] <- current_time
        return(TRUE)
      }

      return(FALSE)
    }

    # ----------------------------------------
    # Populate helpers: update all UI fields for a given content value
    # Called from dropdown observers, modal-open observer, and row-selection observers
    # ----------------------------------------

    populate_content_rule_ui <- function(rule_id) {
      tryCatch({
        df <- rv_table_rules_autoassign_dw()
        match_rows <- df[df$RuleId == rule_id, , drop = FALSE]
        if (nrow(match_rows) == 1) {
          match_row <- match_rows[1, , drop = FALSE]
          auto_convert <- get_auto_convert_state(input, "auto_convert_content_regex_dw", TRUE)
          inc <- if (isTRUE(auto_convert)) regex_to_plain_dw(match_row$Include) else match_row$Include
          exc <- if (isTRUE(auto_convert)) regex_to_plain_dw(match_row$Exclude) else match_row$Exclude
          updateTextInput(session, "string_include_autoassign_dw", value = if (!is.na(inc)) inc else "")
          updateTextInput(session, "string_exclude_autoassign_dw", value = if (!is.na(exc)) exc else "")
          updateSelectInput(session, "transformation_col_dw", selected = match_row$Transformation)
        }
      }, error = function(e) {
        debug_log(paste("Error in populate_content_rule_ui:", e$message), 1)
      })
    }

    populate_condition_rule_ui <- function(rule_id) {
      tryCatch({
        df <- rv_condition_rules_autoassign_dw()
        match_row <- df[df$RuleId == rule_id, , drop = FALSE]
        if (nrow(match_row) == 1) {
          updateSelectInput(session, "cond_method_autoassign_dw", selected = match_row$Method)
          auto_convert <- get_auto_convert_state(input, "auto_convert_sample_regex_dw", TRUE)
          before <- if (isTRUE(auto_convert)) regex_to_plain_dw(match_row$Before) else match_row$Before
          after  <- if (isTRUE(auto_convert)) regex_to_plain_dw(match_row$After)  else match_row$After
          updateTextInput(session, "cond_before_autoassign_dw", value = if (!is.na(before)) before else "")
          updateTextInput(session, "cond_after_autoassign_dw",  value = if (!is.na(after))  after  else "")
          seps <- if (!is.na(match_row$Separators) && nzchar(match_row$Separators))
                    strsplit(match_row$Separators, "\\|")[[1]] else character(0)
          updateCheckboxGroupInput(session, "cond_sep_chars_autoassign_dw", selected = seps)
          updateNumericInput(session, "cond_pos_autoassign_dw", value = match_row$Pos)
        }
      }, error = function(e) {
        debug_log(paste("Error in populate_condition_rule_ui:", e$message), 1)
      })
    }

    populate_ratio_rule_ui <- function(rule_id) {
      tryCatch({
        df <- rv_rules_autoassign_dw()
        match_row <- df[df$RuleId == rule_id, , drop = FALSE]
        if (nrow(match_row) == 1) {
          updateSelectInput(session, "new_method_autoassign_dw", selected = match_row$Method)
          seps <- if (!is.na(match_row$Separators)) strsplit(match_row$Separators, "\\|")[[1]] else character(0)
          updateCheckboxGroupInput(session, "new_sep_chars_autoassign_dw", selected = seps)
          updateCheckboxInput(session, "new_invert_autoassign_dw", value = as.logical(match_row$Invert))
          auto_convert <- get_auto_convert_state(input, "auto_convert_regex_dw", TRUE)
          method_val   <- as.character(match_row$Method)
          if (isTRUE(auto_convert) && grepl("^Regular Expression", method_val, ignore.case = TRUE)) {
            nb  <- regex_to_plain_dw(match_row$NumBefore)
            na_ <- regex_to_plain_dw(match_row$NumAfter)
            db  <- regex_to_plain_dw(match_row$DenBefore)
            da  <- regex_to_plain_dw(match_row$DenAfter)
          } else {
            nb  <- match_row$NumBefore
            na_ <- match_row$NumAfter
            db  <- match_row$DenBefore
            da  <- match_row$DenAfter
          }
          updateTextInput(session, "new_num_before_autoassign_dw", value = ifelse(is.na(nb),  "", nb))
          updateTextInput(session, "new_num_after_autoassign_dw",  value = ifelse(is.na(na_), "", na_))
          updateTextInput(session, "new_den_before_autoassign_dw", value = ifelse(is.na(db),  "", db))
          updateTextInput(session, "new_den_after_autoassign_dw",  value = ifelse(is.na(da),  "", da))
          updateNumericInput(session, "new_num_pos_autoassign_dw", value = match_row$NumPos)
          updateNumericInput(session, "new_den_pos_autoassign_dw", value = match_row$DenPos)
        }
      }, error = function(e) {
        debug_log(paste("Error in populate_ratio_rule_ui:", e$message), 1)
      })
    }

    selected_content_variant <- function(content, preferred_rule_id = NULL) {
      resolve_content_variant_id(
        rv_table_rules_autoassign_dw(), content,
        if (is.null(preferred_rule_id)) selected_content_rule() else preferred_rule_id
      )
    }

    all_auto_assign_rule_ids <- function() {

      unique(c(
        as.character(
          rv_table_rules_autoassign_dw()$RuleId
        ),
        as.character(
          rv_condition_rules_autoassign_dw()$RuleId
        ),
        as.character(
          rv_rules_autoassign_dw()$RuleId
        )
      ))
    }

    build_content_rule_from_inputs <- function(
    rule_id,
    variant_id,
    priority) {

      req(
        input$lookup_content_dw,
        input$string_include_autoassign_dw
      )

      auto_convert <- get_auto_convert_state(
        input,
        "auto_convert_content_regex_dw",
        TRUE
      )

      inc_pat <- input$string_include_autoassign_dw
      exc_pat <- input$string_exclude_autoassign_dw

      if (is.null(exc_pat) ||
          is.na(exc_pat)) {
        exc_pat <- ""
      }

      normalize_content_regex <- function(x) {

        if (is.null(x) ||
            is.na(x) ||
            !nzchar(x)) {
          return(x)
        }

        x <- gsub(
          "\\^",
          "^",
          x,
          fixed = TRUE
        )

        x <- gsub(
          "\\$",
          "$",
          x,
          fixed = TRUE
        )

        x <- gsub(
          "/",
          "\\\\/",
          x,
          perl = TRUE
        )

        x
      }

      if (isTRUE(auto_convert)) {

        if (grepl("\\|", inc_pat)) {

          inc_pat <-
            make_or_regex_autoassign_dw(
              inc_pat
            )

        } else if (grepl(
          "&",
          inc_pat,
          fixed = TRUE
        )) {

          inc_pat <-
            make_and_regex_autoassign_dw(
              inc_pat
            )

        } else {

          inc_pat <-
            escape_regex_autoassign_dw(
              inc_pat
            )
        }

        if (nzchar(exc_pat)) {

          if (grepl("\\|", exc_pat)) {

            exc_pat <-
              make_or_regex_autoassign_dw(
                exc_pat
              )

          } else if (grepl(
            "&",
            exc_pat,
            fixed = TRUE
          )) {

            exc_pat <-
              make_and_regex_autoassign_dw(
                exc_pat
              )

          } else {

            exc_pat <-
              escape_regex_autoassign_dw(
                exc_pat
              )
          }

        } else {

          exc_pat <- ""
        }

        inc_pat <-
          normalize_content_regex(
            inc_pat
          )

        exc_pat <-
          normalize_content_regex(
            exc_pat
          )

        debug_log(
          paste(
            "Applied regex conversion to content patterns",
            "(flow -> regex, anchors kept)"
          ),
          2
        )

      } else {

        debug_log(
          "Using raw regex patterns directly for content rules",
          2
        )
      }

      trans_val <-
        if (input$lookup_content_dw %in%
            TRANSFORMATION_CONTENT_TYPES) {

          input$transformation_col_dw

        } else {

          NA_character_
        }

      canonical_rule_row(
        "content",
        RuleId = rule_id,
        Content = input$lookup_content_dw,
        VariantId = variant_id,
        Priority = as.integer(priority),
        Include = inc_pat,
        Exclude = exc_pat,
        Transformation = trans_val
      )
    }

    build_condition_rule_from_inputs <- function(
    rule_id,
    variant_id) {

      req(
        input$cond_content_autoassign_dw,
        input$cond_method_autoassign_dw
      )

      auto_convert <- get_auto_convert_state(
        input,
        "auto_convert_sample_regex_dw",
        TRUE
      )

      convert_flow_sample_to_regex <- function(x) {

        if (is.null(x) ||
            is.na(x) ||
            !nzchar(x)) {
          return(x)
        }

        value <-
          escape_regex_autoassign_dw(
            x
          )

        value <- gsub(
          "\\^",
          "^",
          value,
          fixed = TRUE
        )

        value <- gsub(
          "\\$",
          "$",
          value,
          fixed = TRUE
        )

        value <- gsub(
          "/",
          "\\\\/",
          value,
          perl = TRUE
        )

        value
      }

      if (isTRUE(auto_convert)) {

        raw_before <-
          convert_flow_sample_to_regex(
            input$cond_before_autoassign_dw
          )

        raw_after <-
          convert_flow_sample_to_regex(
            input$cond_after_autoassign_dw
          )

      } else {

        raw_before <-
          input$cond_before_autoassign_dw

        raw_after <-
          input$cond_after_autoassign_dw
      }

      canonical_rule_row(
        "condition",
        RuleId = rule_id,
        Content =
          input$cond_content_autoassign_dw,
        VariantId = variant_id,
        Method =
          input$cond_method_autoassign_dw,
        Before = raw_before,
        After = raw_after,
        Separators = paste(
          input$cond_sep_chars_autoassign_dw,
          collapse = "|"
        ),
        Pos =
          input$cond_pos_autoassign_dw
      )
    }

    build_ratio_rule_from_inputs <- function(
    rule_id,
    variant_id) {

      req(
        input$new_content_autoassign_dw,
        input$new_method_autoassign_dw
      )

      if (identical(
        input$new_method_autoassign_dw,
        "Regular Expressions"
      )) {

        auto_convert <- get_auto_convert_state(
          input,
          "auto_convert_regex_dw",
          TRUE
        )

        convert_flow_ratio_to_regex <- function(x) {

          if (is.null(x) ||
              is.na(x) ||
              !nzchar(x)) {
            return(x)
          }

          value <-
            escape_regex_autoassign_dw(
              x
            )

          value <- gsub(
            "\\^",
            "^",
            value,
            fixed = TRUE
          )

          value <- gsub(
            "\\$",
            "$",
            value,
            fixed = TRUE
          )

          value <- gsub(
            "/",
            "\\\\/",
            value,
            perl = TRUE
          )

          value
        }

        if (isTRUE(auto_convert)) {

          raw_nb <-
            convert_flow_ratio_to_regex(
              input$new_num_before_autoassign_dw
            )

          raw_na <-
            convert_flow_ratio_to_regex(
              input$new_num_after_autoassign_dw
            )

          raw_db <-
            convert_flow_ratio_to_regex(
              input$new_den_before_autoassign_dw
            )

          raw_da <-
            convert_flow_ratio_to_regex(
              input$new_den_after_autoassign_dw
            )

        } else {

          raw_nb <-
            input$new_num_before_autoassign_dw

          raw_na <-
            input$new_num_after_autoassign_dw

          raw_db <-
            input$new_den_before_autoassign_dw

          raw_da <-
            input$new_den_after_autoassign_dw
        }

        return(
          canonical_rule_row(
            "ratio",
            RuleId = rule_id,
            Content =
              input$new_content_autoassign_dw,
            VariantId = variant_id,
            Method = "Regular Expressions",
            Separators = NA_character_,
            Invert = FALSE,
            NumBefore = raw_nb,
            NumAfter = raw_na,
            DenBefore = raw_db,
            DenAfter = raw_da,
            NumPos = NA_integer_,
            DenPos = NA_integer_
          )
        )
      }

      req(
        input$new_sep_chars_autoassign_dw
      )

      if (identical(
        input$new_method_autoassign_dw,
        "Position in String"
      )) {

        req(
          input$new_num_pos_autoassign_dw,
          input$new_den_pos_autoassign_dw
        )

        return(
          canonical_rule_row(
            "ratio",
            RuleId = rule_id,
            Content =
              input$new_content_autoassign_dw,
            VariantId = variant_id,
            Method = "Position in String",
            Separators = paste(
              input$new_sep_chars_autoassign_dw,
              collapse = "|"
            ),
            Invert = FALSE,
            NumBefore = NA_character_,
            NumAfter = NA_character_,
            DenBefore = NA_character_,
            DenAfter = NA_character_,
            NumPos =
              input$new_num_pos_autoassign_dw,
            DenPos =
              input$new_den_pos_autoassign_dw
          )
        )
      }

      canonical_rule_row(
        "ratio",
        RuleId = rule_id,
        Content =
          input$new_content_autoassign_dw,
        VariantId = variant_id,
        Method =
          input$new_method_autoassign_dw,
        Separators = paste(
          input$new_sep_chars_autoassign_dw,
          collapse = "|"
        ),
        Invert =
          isTRUE(input$new_invert_autoassign_dw),
        NumBefore = NA_character_,
        NumAfter = NA_character_,
        DenBefore = NA_character_,
        DenAfter = NA_character_,
        NumPos = NA_integer_,
        DenPos = NA_integer_
      )
    }

    # ----------------------------------------

    choose_existing_modal_rule_id <- function(
    frame,
    selected_id) {

      if (!is.data.frame(frame) ||
          !nrow(frame) ||
          !"RuleId" %in% names(frame)) {
        return(NULL)
      }

      ids <- as.character(
        frame$RuleId
      )

      keep <-
        !is.na(ids) &
        nzchar(ids)

      ids <- ids[keep]

      if (!length(ids)) {
        return(NULL)
      }

      selected_id <- as.character(
        selected_id
      )

      if (length(selected_id) == 1L &&
          !is.na(selected_id) &&
          nzchar(selected_id) &&
          selected_id %in% ids) {
        return(selected_id)
      }

      ids[[1L]]
    }

    hydrate_auto_assign_modal <- function() {

      tryCatch({

        df_table <-
          rv_table_rules_autoassign_dw()

        valid_table <- df_table[
          !is.na(df_table$Content) &
            df_table$Content != "Row Index",
          ,
          drop = FALSE
        ]

        content_rule_id <-
          choose_existing_modal_rule_id(
            valid_table,
            isolate(
              selected_content_rule()
            )
          )

        if (!is.null(content_rule_id)) {

          row <- match(
            content_rule_id,
            valid_table$RuleId
          )

          content <- as.character(
            valid_table$Content[[row]]
          )

          updateSelectInput(
            session,
            "lookup_content_dw",
            selected = content
          )

          selected_content_rule(
            content_rule_id
          )

          populate_content_rule_ui(
            content_rule_id
          )
        }

        df_cond <-
          rv_condition_rules_autoassign_dw()

        condition_rule_id <-
          choose_existing_modal_rule_id(
            df_cond,
            isolate(
              selected_condition_rule()
            )
          )

        if (!is.null(condition_rule_id)) {

          row <- match(
            condition_rule_id,
            df_cond$RuleId
          )

          content <- as.character(
            df_cond$Content[[row]]
          )

          updateSelectInput(
            session,
            "cond_content_autoassign_dw",
            selected = content
          )

          selected_condition_rule(
            condition_rule_id
          )

          populate_condition_rule_ui(
            condition_rule_id
          )
        }

        df_ratio <-
          rv_rules_autoassign_dw()

        ratio_rule_id <-
          choose_existing_modal_rule_id(
            df_ratio,
            isolate(
              selected_ratio_rule()
            )
          )

        if (!is.null(ratio_rule_id)) {

          row <- match(
            ratio_rule_id,
            df_ratio$RuleId
          )

          content <- as.character(
            df_ratio$Content[[row]]
          )

          updateSelectInput(
            session,
            "new_content_autoassign_dw",
            selected = content
          )

          selected_ratio_rule(
            ratio_rule_id
          )

          populate_ratio_rule_ui(
            ratio_rule_id
          )
        }

        debug_log(
          sprintf(
            paste0(
              "Auto-Assign modal hydrated | ",
              "Content rules: %d | ",
              "Condition rules: %d | ",
              "Ratio rules: %d"
            ),
            nrow(df_table),
            nrow(df_cond),
            nrow(df_ratio)
          ),
          2
        )

      }, error = function(e) {

        debug_log(
          paste(
            "Error hydrating Auto-Assign modal:",
            e$message
          ),
          1
        )
      })
    }

    observeEvent(
      input$open_auto_assign_modal,
      {

        showModal(
          build_auto_assign_modal_ui(
            ns
          )
        )

        # The modal is created dynamically. Send input restoration only after
        # the modal HTML has been flushed to the browser so its input bindings
        # exist before updateSelectInput()/updateTextInput() messages arrive.
        session$onFlushed(
          function() {
            hydrate_auto_assign_modal()
          },
          once = TRUE
        )
      },
      ignoreInit = TRUE
    )

  }, envir = ctx)

  # Register rule observer groups against this shared context. The helpers use
  # the state, click guard, and population helpers created above.
  register_auto_assign_content_handlers(ctx)
  register_auto_assign_condition_handlers(ctx)
  register_auto_assign_ratio_handlers(ctx)

  evalq({
    # ========================================
    # Table Row Selection Synchronization
    # Select a rule in the table -> update dropdown + populate all UI fields
    # Both updateSelectInput and the populate helper are called: updateSelectInput
    # handles the visual dropdown update, while the populate helper guarantees
    # the text fields are filled even when the dropdown value does not change.
    # ========================================

    observeEvent(input$table_rules_content_dw_rows_selected, {
      row_idx <- input$table_rules_content_dw_rows_selected
      if (!is.null(row_idx) && length(row_idx) > 0) {
        tryCatch({
          df <- rv_table_rules_autoassign_dw()
          if (row_idx <= nrow(df)) {
            rule_id <- df$RuleId[row_idx]
            selected_content_rule(rule_id)
            updateSelectInput(session, "lookup_content_dw", selected = df$Content[row_idx])
            populate_content_rule_ui(rule_id)
          }
        }, error = function(e) {
          debug_log(paste("Error handling content table row selection:", e$message), 1)
        })
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$condition_rules_table_autoassign_dw_rows_selected, {
      row_idx <- input$condition_rules_table_autoassign_dw_rows_selected
      if (!is.null(row_idx) && length(row_idx) > 0) {
        tryCatch({
          df <- rv_condition_rules_autoassign_dw()
          if (row_idx <= nrow(df)) {
            rule_id <- df$RuleId[row_idx]
            selected_condition_rule(rule_id)
            content_rules <- rv_table_rules_autoassign_dw()
            content_row <- which(content_rules$VariantId == df$VariantId[row_idx])
            if (length(content_row)) selected_content_rule(content_rules$RuleId[content_row[[1L]]])
            updateSelectInput(session, "cond_content_autoassign_dw", selected = df$Content[row_idx])
            populate_condition_rule_ui(rule_id)
          }
        }, error = function(e) {
          debug_log(paste("Error handling condition table row selection:", e$message), 1)
        })
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$rules_table_ratio_autoassign_dw_rows_selected, {
      row_idx <- input$rules_table_ratio_autoassign_dw_rows_selected
      if (!is.null(row_idx) && length(row_idx) > 0) {
        tryCatch({
          df <- rv_rules_autoassign_dw()
          if (row_idx <= nrow(df)) {
            rule_id <- df$RuleId[row_idx]
            selected_ratio_rule(rule_id)
            content_rules <- rv_table_rules_autoassign_dw()
            content_row <- which(content_rules$VariantId == df$VariantId[row_idx])
            if (length(content_row)) selected_content_rule(content_rules$RuleId[content_row[[1L]]])
            updateSelectInput(session, "new_content_autoassign_dw", selected = df$Content[row_idx])
            populate_ratio_rule_ui(rule_id)
          }
        }, error = function(e) {
          debug_log(paste("Error handling ratio table row selection:", e$message), 1)
        })
      }
    }, ignoreNULL = TRUE)


  }, envir = ctx)

  # Export registration intentionally follows table-row synchronization.
  register_auto_assign_export_handlers(ctx)

  invisible(TRUE)
}
