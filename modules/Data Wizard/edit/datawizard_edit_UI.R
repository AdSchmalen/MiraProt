# ============================================================================
# Sub-Script: Data Wizard Edit UI
#
# Purpose:
#   Define the Shiny UI for the Edit module (modEditUI).
#   Contains all layout, input widgets, CSS, and conditional panels.
#
# Architectural Role:
#   Presentation layer. Produces the HTML/widget tree consumed by
#   moduleServer. Does not contain any server-side logic.
#
# Exports:
#   modEditUI(id)  – returns a Shiny div() tree
#
# Dependencies:
#   shiny, DT (for dataTableOutput)
# ============================================================================

#' Data Editing Module UI - Enhanced with Template Integration
#'
#' Creates UI components for data replacement and editing operations with template support
#' @param id module namespace id
#' @export
modEditUI <- function(id) {
  ns <- NS(id)

  div(
    # Add CSS for executed operations styling
    tags$head(
      tags$style(HTML("
        .executed-operation {
          background-color: #f8f9fa !important;
          color: #6c757d !important;
          font-style: italic;
        }
        .executed-operation:hover {
          background-color: #e9ecef !important;
        }
        .template-status {
          background-color: #e7f3ff;
          border: 1px solid #b8daff;
          padding: 6px 10px;
          border-radius: 4px;
          margin: 5px 0;
          font-size: 0.9em;
        }
      "))
    ),

    #  h4("Data Editing and Replacement"),

    # # Template Status Display
    # div(
    #   class = "template-status",
    #   uiOutput(ns("template_status_display"))
    # ),

    # Column selection controls
    fluidRow(
      column(6,
             div(
               title = "Select a content category to filter available columns",
               selectInput(
                 ns("category_select"),
                 "Content Category:",
                 choices = NULL,
                 width = "100%"
               )
             )),
      column(6,
             div(
               title = "Select one or more columns within the chosen category",
               selectInput(
                 ns("column_select"),
                 "Columns:",
                 choices = NULL,
                 multiple = TRUE,
                 width = "100%"
               )
             ))
    ),

    # # Column information display
    # verbatimTextOutput(ns("column_info")),

    br(),

    # Main editing interface with tabs
    tabsetPanel(
      id = ns("edit_tabs"),
      type = "tabs",

      # Replace Tab
      tabPanel(
        title = "Replace",
        br(),

        # Dynamic UI based on column type
        uiOutput(ns("replace_controls")),

        br(),
        div(
          title = "Add replacement operation to the queue",
          actionButton(
            ns("add_replace"),
            "Add to Queue",
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      # Edit Tab
      tabPanel(
        title = "Edit",
        br(),

        # Dynamic UI based on column type
        uiOutput(ns("edit_controls")),

        br(),
        div(
          title = "Add edit operation to the queue",
          actionButton(
            ns("add_edit"),
            "Add to Queue",
            class = "btn-primary",
            width = "100%"
          )
        )
      )
    ),

    br(),

    # Pending operations table
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("has_pending_operations")),
      div(
        title = "View and manage your queued operations",
        h6("Queued Operations:"),
        rHandsontableOutput(ns("operations_table"), width = "100%", height = 200)
      )
    ),

    hr(),

    # Control buttons
    fluidRow(
      column(6,
             div(
               title = "Apply all pending (not executed) operations to the data",
               actionButton(
                 ns("apply_all_operations"),
                 "Apply Queue",
                 class = "btn-success",
                 style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                 width = "100%"
               )
             )),
      column(6,
             div(
               title = "Clear all operations from the queue",
               actionButton(
                 ns("clear_operations"),
                 "Clear Queue",
                 class = "btn-default",
                 style = "background-color: #3498db; border-color: #3498db; color: #fff;",
                 width = "100%"
               )
             ))#,
      # column(4,
      #        div(
      #          title = "Reset all modifications and restore original data",
      #          actionButton(
      #            ns("reset_edits"),
      #            "Reset",
      #            class = "btn-danger",
      #            width = "100%"
      #          )
      #        ))
    ),

    br(),

    # # Operation log display toggle
    # fluidRow(
    #   column(6,
    #          div(
    #            title = "Show/hide detailed operation log",
    #            checkboxInput(
    #              ns("show_log"),
    #              "Show Operation Log",
    #              value = FALSE
    #            )
    #          ))
    # ),
    #
    # # Operation log display
    # conditionalPanel(
    #   condition = sprintf("input['%s'] == true", ns("show_log")),
    #   br(),
    #   div(
    #     style = "border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
    #     h6("Operation Log"),
    #     verbatimTextOutput(ns("operation_log"))
    #   )
    # )
  )
}
