# modules/Data Wizard/filtering/datawizard_filtering_ui.R
#
# PURPOSE:
#   Contains all dynamic server-side UI output renderers for the Data Wizard
#   filtering module. This file centralizes renderUI, renderRHandsontable,
#   and renderText outputs so that the orchestrator file stays lean.
#
# ARCHITECTURE:
#   Called from datawizard_filtering.R via register_filtering_ui_outputs().
#   All renderers run inside the moduleServer() closure of the orchestrator.
#   Reactive state comes from datawizard_filtering_state.R.
#   The show_filter_notification() helper is also defined here since it is
#   a UI-facing utility used by both this file and the observers file.
#
# STRUCTURE:
#   1. show_filter_notification()  - Notification helper
#   2. register_filtering_ui_outputs()
#      - filter_errors             (renderUI)
#      - filter_operator_ui_dw_1/2 (renderUI)
#      - filter_value_ui_dw_1/2    (renderUI)
#      - custom_filters_block      (renderUI)
#      - filter_table_dw           (renderRHandsontable)
#      - has_custom_filters_dw     (renderText)
#
# NOTES FOR DEVELOPERS:
#   - Operator/value renderers depend on selected_type() from the state file.
#   - The filter table uses rhandsontable and formats SOME logic columns for
#     display while hiding the raw Some_Operator / Some_Count fields.
#   - custom_filters_block is a wrapper that only renders the heading and
#     table output when at least one custom filter condition exists.

#' Enhanced notification function with optional debug_level parameter
show_filter_notification <- function(message, type = "message", context = "", session = NULL, debug_level = NULL) {

  tryCatch({
    # Use module DEBUG_LEVEL if not provided
    if (is.null(debug_level)) {
      debug_level <- DEBUG_LEVEL
    }

    # Robust parameter validation
    if (is.null(debug_level) || !is.numeric(debug_level)) {
      debug_level <- 1
    }

    if (is.null(message) || !is.character(message)) {
      message <- "Invalid notification message"
    }

    if (is.null(type) || !is.character(type)) {
      type <- "message"
    }

    if (is.null(context) || !is.character(context)) {
      context <- ""
    }

    # Map types to valid Shiny notification types with fallback
    shiny_type <- tryCatch({
      switch(type,
             "success" = "message",
             "info" = "message",
             "error" = "error",
             "warning" = "warning",
             "message" = "message",
             "default" = "default",
             "message")  # Default fallback
    }, error = function(e) {
      debug_log(paste("Error mapping notification type:", e$message), 1)
      "message"
    })

    # Duration based on type with fallback
    duration <- tryCatch({
      switch(type,
             "success" = 4,
             "info" = 6,
             "error" = 10,
             "warning" = 8,
             "message" = 5,
             5)  # Default duration
    }, error = function(e) {
      debug_log(paste("Error determining duration:", e$message), 1)
      5
    })

    # Format message with context safely
    full_message <- tryCatch({
      if (nzchar(context)) {
        paste0("[", context, "] ", message)
      } else {
        message
      }
    }, error = function(e) {
      debug_log(paste("Error formatting message:", e$message), 1)
      message
    })

    # Log the notification
    debug_log(paste("Notification:", type, "-", full_message), 2)

    # Show notification if session available
    if (!is.null(session)) {
        showNotification(
          full_message,
          type = shiny_type,
          duration = duration,
          session = session
        )
    } else {
      debug_log("No session available for notification", 2)
    }

    return(full_message)

  }, error = function(e) {
    debug_log(paste("Critical error in notification system:", e$message), 1)
    return(paste("Notification error:", e$message))
  })
}


register_filtering_ui_outputs <- function(input, output, session, ns,
                                          filter_state,
                                          selected_type,
                                          debug_log,
                                          filtering_ui_active = reactiveVal(TRUE)) {

  # ========================================
  # Enhanced Error Display
  # ========================================

  output$filter_errors <- renderUI({
    errors <- filter_state$errors
    if (length(errors) > 0) {
      div(
        class = "alert alert-danger alert-dismissible",
        tags$button(type = "button", class = "close", `data-dismiss` = "alert", "\u00d7"),
        h6("Filter Errors:"),
        tags$ul(
          lapply(errors, function(e) tags$li(e))
        )
      )
    }
  })

  # ========================================
  # Operator and Value UI Rendering
  # ========================================

  numeric_ops <- list(">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=", "==" = "==", "!=" = "!=")
  string_ops <- list("Contains" = "contains", "Equals" = "equals", "Starts with" = "starts",
                     "Ends with" = "ends", "Does not contain" = "not_contains")

  output$filter_operator_ui_dw_1 <- renderUI({
    tryCatch({
      choices <- switch(selected_type(),
                        numeric = numeric_ops,
                        character = string_ops,
                        c(numeric_ops, string_ops))
      selectInput(
        inputId = ns("filter_operator_dw_1"),
        label = "First Operator:",
        choices = choices,
        width = "100%"
      )
    }, error = function(e) {
      debug_log(paste("Error rendering operator UI 1:", e$message), 1)
      return(NULL)
    })
  })

  output$filter_operator_ui_dw_2 <- renderUI({
    tryCatch({
      choices <- switch(selected_type(),
                        numeric = numeric_ops,
                        character = string_ops,
                        c(numeric_ops, string_ops))
      selectInput(
        inputId = ns("filter_operator_dw_2"),
        label = "Second Operator:",
        choices = choices,
        width = "100%"
      )
    }, error = function(e) {
      debug_log(paste("Error rendering operator UI 2:", e$message), 1)
      return(NULL)
    })
  })

  output$filter_value_ui_dw_1 <- renderUI({
    tryCatch({
      if (selected_type() == "numeric") {
        numericInput(ns("filter_value_dw_1"), "First Value:", value = NULL)
      } else {
        textInput(ns("filter_value_dw_1"), "First Value:", value = "", placeholder = "Enter comparison value")
      }
    }, error = function(e) {
      debug_log(paste("Error rendering value UI 1:", e$message), 1)
      return(NULL)
    })
  })

  output$filter_value_ui_dw_2 <- renderUI({
    tryCatch({
      if (selected_type() == "numeric") {
        numericInput(ns("filter_value_dw_2"), "Second Value (optional):", value = NULL)
      } else {
        textInput(ns("filter_value_dw_2"), "Second Value (optional):", value = "", placeholder = "Enter second value (optional)")
      }
    }, error = function(e) {
      debug_log(paste("Error rendering value UI 2:", e$message), 1)
      return(NULL)
    })
  })

  # ========================================
  # Filter Table Display
  # ========================================

  # Build the Custom queue block only when rows exist (prevents reserving space & delayed paint)
  output$custom_filters_block <- renderUI({
    tryCatch({
      df <- filter_state$custom_conditions
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      # Create the heading + table output only now (visible state),
      # so Handsontable can initialize with correct sizes immediately.
      tagList(
        h6("Current Custom Filter Conditions:"),
        rHandsontableOutput(ns("filter_table_dw"))
      )
    }, error = function(e) {
      debug_log(paste("Error building custom_filters_block:", e$message), 1)
      return(NULL)
    })
  })

  output$filter_table_dw <- renderRHandsontable({
    tryCatch({
      # Get current conditions from state
      df <- filter_state$custom_conditions

      if ((is.null(df) || !is.data.frame(df) || nrow(df) == 0) && !isTRUE(filtering_ui_active())) {
        return(NULL)
      }

      debug_log(paste("Rendering filter table with", nrow(df), "conditions"), 2)

      if (nrow(df) == 0) {
        # Do not render an empty table
        debug_log("No filter conditions yet - suppressing table render", 2)
        return(NULL)
      } else {
        # Ensure all SOME columns exist in display
        if (!"Some_Operator" %in% names(df)) {
          df$Some_Operator <- NA_character_
        }
        if (!"Some_Count" %in% names(df)) {
          df$Some_Count <- NA_real_
        }

        # Clean display values
        df_display <- df
        df_display[is.na(df_display)] <- ""

        # Convert all to character for display
        df_display[] <- lapply(df_display, as.character)

        # Format SOME columns for better display
        for (i in 1:nrow(df_display)) {
          if (df_display$Multi_Column_Logic[i] == "SOME") {
            if (df_display$Some_Operator[i] != "" && df_display$Some_Count[i] != "") {
              df_display$Multi_Column_Logic[i] <- paste("SOME:", df_display$Some_Operator[i], df_display$Some_Count[i])
            }
          }
        }

        # Select columns for display (hide internal SOME columns)
        display_columns <- c("Column", "Operator_1", "Value_1", "Logic", "Operator_2", "Value_2", "Empty_Filter", "Multi_Column_Logic")
        df <- df_display[, display_columns, drop = FALSE]
      }

      # Create handsontable with proper sizing
      ht <- rhandsontable(df,
                          readOnly = TRUE,
                          height = 200,
                          stretchH = "all",
                          rowHeaders = FALSE) %>%
        hot_cols(colWidths = c(150, 100, 100, 80, 100, 100, 120, 180)) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)

      debug_log("Filter table rendered successfully", 2)
      return(ht)

    }, error = function(e) {
      debug_log(paste("Error rendering filter table:", e$message), 1)

      # Fallback table on error
      fallback_df <- data.frame(
        Message = "Error displaying filters",
        Details = e$message,
        stringsAsFactors = FALSE
      )

      return(rhandsontable(fallback_df, readOnly = TRUE, height = 100))
    })
  })

  # Flag: table only visible if at least one condition exists
  output$has_custom_filters_dw <- renderText({
    tryCatch({
      df <- filter_state$custom_conditions
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) "true" else "false"
    }, error = function(e) {
      debug_log(paste("Error computing has_custom_filters_dw:", e$message), 1)
      "false"
    })
  })
  outputOptions(output, "has_custom_filters_dw", suspendWhenHidden = FALSE)

  debug_log("All filtering UI outputs registered", 1)
}
