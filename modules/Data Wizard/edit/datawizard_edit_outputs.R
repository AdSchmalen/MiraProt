# ============================================================================
# Sub-Script: Data Wizard Edit Outputs
#
# Purpose:
#   Register all Shiny output renderers for the Edit module. This covers
#   dynamic UI panels (replace controls, edit controls) and the operations
#   queue table display.
#
# Architectural Role:
#   Presentation layer. Reads reactive state and renders output bindings.
#   Must not mutate state, register observers, or define external API.
#
# Structure:
#   register_edit_outputs(ctx) is called once from the orchestrator with
#   the moduleServer environment. It uses evalq() so that output, input,
#   ns, and all reactive state variables are accessible directly.
#
#   Registered outputs:
#     - output$replace_controls     : renderUI for value replacement panel
#     - output$edit_controls        : renderUI for math/string edit panel
#     - output$operations_table     : renderRHandsontable for the queue
#     - output$has_pending_operations : reactive boolean for conditionalPanel
#
# Dependencies (from parent environment):
#   output, input, ns, session
#   selected_columns_info, pending_operations  (reactiveVals from state)
#   debug_log                                  (logging helper)
#
# Future Developer Notes:
#   - If you add a new output$ binding for the Edit module, place it here.
#   - Keep render logic read-only. Side effects belong in the handlers file.
#   - The rhandsontable color-coding loop in operations_table is a known
#     simplification; it may not scale well beyond ~50 operations.
# ============================================================================


register_edit_outputs <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_edit_outputs requires an environment context")
  }

  evalq({

    # ------------------------------------------------------------------
    # Replace controls: dynamic UI based on selected column type
    # ------------------------------------------------------------------
    output$replace_controls <- renderUI({
      tryCatch({
        column_info <- selected_columns_info()

        if (is.null(input$column_select) || length(input$column_select) == 0 || column_info$overall_type == "unknown") {
          return(div(
            class = "alert alert-info",
            "Please select one or more columns to configure replacement options."
          ))
        }

        if (!column_info$compatible) {
          return(div(
            class = "alert alert-warning",
            HTML(paste0(
              "<strong>Mixed column types detected:</strong><br>",
              column_info$type_summary, "<br><br>",
              "Please select columns of the same type for batch operations, or process them separately."
            ))
          ))
        }

        if (column_info$overall_type == "character") {
          # String replacement controls
          tagList(
            div(
              class = "alert alert-info",
              HTML(paste0(
                "<strong>Character columns detected (", length(column_info$existing_columns), " columns).</strong><br>",
                "Configure string-based replacement operations that will be added to the queue."
              ))
            ),
            fluidRow(
              column(6,
                     selectInput(
                       ns("string_search_type"),
                       "Search Type:",
                       choices = c("is equal", "starts with", "ends with", "contains"),
                       selected = "contains"
                     )),
              column(6,
                     textInput(
                       ns("string_search_term"),
                       "Search Term:",
                       placeholder = "Enter text to find"
                     ))
            ),
            fluidRow(
              column(6,
                     selectInput(
                       ns("string_replace_type"),
                       "Replacement Type:",
                       choices = c("Replace cell", "Replace substring", "Clear cell"),
                       selected = "Replace substring"
                     )),
              column(6,
                     conditionalPanel(
                       condition = "input.string_replace_type != 'Clear cell'",
                       ns = ns,
                       textInput(
                         ns("string_replacement"),
                         "Replace With:",
                         placeholder = "Enter replacement text"
                       )
                     ))
            )
          )
        } else if (column_info$overall_type == "numeric") {
          # Numeric replacement controls
          tagList(
            div(
              class = "alert alert-info",
              HTML(paste0(
                "<strong>Numeric columns detected (", length(column_info$existing_columns), " columns).</strong><br>",
                "Configure value-based replacement operations that will be added to the queue."
              ))
            ),
            fluidRow(
              column(4,
                     selectInput(
                       ns("numeric_operator"),
                       "Operator:",
                       choices = c("<", "\u2264" = "<=", "=", "\u2260" = "!=", "\u2265" = ">=", ">"),
                       selected = ">"
                     )),
              column(4,
                     numericInput(
                       ns("numeric_threshold"),
                       "Threshold:",
                       value = 0,
                       step = 0.001
                     )),
              column(4,
                     selectInput(
                       ns("numeric_replace_with"),
                       "Replace with:",
                       choices = c("NA", "Numeric"),
                       selected = "NA"
                     ))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Numeric'", ns("numeric_replace_with")),
              numericInput(
                ns("numeric_replacement_value"),
                "Replacement Value:",
                value = 0,
                step = 0.001
              )
            )
          )
        }
      }, error = function(e) {
        debug_log(paste("Error creating replace controls:", e$message), 1)
        return(div(
          class = "alert alert-danger",
          "Error creating replacement controls. Please refresh the page."
        ))
      })
    })

    # ------------------------------------------------------------------
    # Edit controls: dynamic UI based on selected column type
    # ------------------------------------------------------------------
    output$edit_controls <- renderUI({
      tryCatch({
        column_info <- selected_columns_info()

        if (is.null(input$column_select) || length(input$column_select) == 0 || column_info$overall_type == "unknown") {
          return(div(
            class = "alert alert-info",
            "Please select one or more columns to configure edit options."
          ))
        }

        if (!column_info$compatible) {
          return(div(
            class = "alert alert-warning",
            HTML(paste0(
              "<strong>Mixed column types detected:</strong><br>",
              column_info$type_summary, "<br><br>",
              "Please select columns of the same type for batch operations, or process them separately."
            ))
          ))
        }

        if (column_info$overall_type == "character") {
          # String editing controls
          tagList(
            div(
              class = "alert alert-info",
              HTML(paste0(
                "<strong>Character columns detected (", length(column_info$existing_columns), " columns).</strong><br>",
                "Configure text prefix/suffix operations that will be added to the queue."
              ))
            ),
            fluidRow(
              column(6,
                     textInput(
                       ns("string_edit_text"),
                       "Text to Add:",
                       placeholder = "Enter text to append"
                     )),
              column(6,
                     selectInput(
                       ns("string_edit_position"),
                       "Position:",
                       choices = c("Before", "After"),
                       selected = "After"
                     ))
            )
          )
        } else if (column_info$overall_type == "numeric") {
          # Numeric editing controls
          tagList(
            div(
              class = "alert alert-info",
              HTML(paste0(
                "<strong>Numeric columns detected (", length(column_info$existing_columns), " columns).</strong><br>",
                "Configure mathematical operations that will be added to the queue."
              ))
            ),
            selectInput(
              ns("numeric_operation"),
              "Mathematical Operation:",
              choices = c("Add", "Subtract", "Multiply", "Divide", "log", "-log", "raise to the power of"),
              selected = "Multiply"
            ),

            # Conditional inputs based on operation type
            conditionalPanel(
              condition = sprintf("['Add', 'Subtract', 'Multiply', 'Divide'].indexOf(input['%s']) >= 0", ns("numeric_operation")),
              numericInput(
                ns("numeric_operation_value"),
                "Operation Value:",
                value = 1,
                step = 0.001
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'raise to the power of'", ns("numeric_operation")),
              numericInput(
                ns("numeric_exponent"),
                "Exponent:",
                value = 2,
                step = 0.1
              )
            ),

            conditionalPanel(
              condition = sprintf("['log', '-log'].indexOf(input['%s']) >= 0", ns("numeric_operation")),
              numericInput(
                ns("numeric_base"),
                "Logarithm Base:",
                value = exp(1),
                min = 0.001,
                step = 0.1
              )
            )
          )
        }
      }, error = function(e) {
        debug_log(paste("Error creating edit controls:", e$message), 1)
        return(div(
          class = "alert alert-danger",
          "Error creating edit controls. Please refresh the page."
        ))
      })
    })

    # ------------------------------------------------------------------
    # Operations queue table (rhandsontable)
    # ------------------------------------------------------------------
    output$operations_table <- renderRHandsontable({
      tryCatch({
        df <- pending_operations()

        if (nrow(df) == 0) {
          return(NULL)
        }

        df_display <- df
        df_display[is.na(df_display)] <- ""

        # Ensure all columns are character for display except Executed
        df_display$Operation   <- as.character(df_display$Operation)
        df_display$Type        <- as.character(df_display$Type)
        df_display$Columns     <- as.character(df_display$Columns)
        df_display$Parameters  <- as.character(df_display$Parameters)
        df_display$Description <- as.character(df_display$Description)
        df_display$Executed    <- as.logical(df_display$Executed)

        hot <- rhandsontable(df_display,
                             rowHeaders = FALSE,
                             stretchH = "all",
                             readOnly = TRUE) %>%
          hot_col("Operation", readOnly = TRUE) %>%
          hot_col("Type", readOnly = TRUE) %>%
          hot_col("Columns", readOnly = TRUE) %>%
          hot_col("Parameters", readOnly = TRUE) %>%
          hot_col("Description", readOnly = TRUE) %>%
          hot_col("Executed", readOnly = TRUE)

        # Add color coding for executed operations
        if (nrow(df_display) > 0) {
          for (i in seq_len(nrow(df_display))) {
            if (df_display$Executed[i]) {
              hot <- hot %>%
                hot_table(customOpts = list(
                  cells = list(
                    list(row = i - 1, col = 0:5,
                         className = "executed-operation")
                  )
                ))
            }
          }
        }

        return(hot)
      }, error = function(e) {
        debug_log(paste("Error rendering operations table:", e$message), 1)
        return(NULL)
      })
    })

    # ------------------------------------------------------------------
    # Boolean output for conditionalPanel visibility
    # ------------------------------------------------------------------
    output$has_pending_operations <- reactive({
      tryCatch({
        df <- pending_operations()
        is.data.frame(df) && nrow(df) > 0
      }, error = function(e) {
        debug_log(paste("Error computing has_pending_operations:", e$message), 1)
        FALSE
      })
    })
    outputOptions(output, "has_pending_operations", suspendWhenHidden = FALSE)

  }, envir = ctx)
}
