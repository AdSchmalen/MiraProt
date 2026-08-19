# ============================================================================
# File: modules/Data Wizard/pivot/datawizard_pivot_ui.R
# Purpose:
#   Define the Pivot submodule user interface for Data Wizard.
#
# Architecture role:
#   UI layer for the Pivot submodule. This file only contains UI composition
#   and does not include server-side business logic.
#
# Structure:
#   - `modPivotUI()`: complete Pivot tab UI layout and controls.
#
# Safe maintenance notes:
#   - Keep input IDs stable because server observers and helpers depend on them.
#   - Keep conditional panel wiring (`pivot_ready`) consistent with server outputs.
#   - Do not add server-side logic to this file.
# ============================================================================

# UI (Unchanged - preserving existing interface)

#' Enhanced Pivot Module UI with UI_config Support
#'
#' Creates the user interface for data pivoting operations with enhanced tooltips and structure
#' @param id module namespace id
#' @export
modPivotUI <- function(id) {
  ns <- NS(id)
  div(
    ## Tooltip at top
    fluidRow(
      column(12,
             div(style = "margin-top: -10px; margin-bottom: 15px; color = #666; font-size: 12px;",
                 HTML("<strong>Pivot operations reshape your data between wide and long formats.
                      Wide format has multiple columns for measurements, while long format stacks
                      measurements in a single column with identifier variables.</strong><br/>"))
      )),

    ## Data selection
    fluidRow(
      column(12,
             div(
               title = "Primary Data: Main experimental data to be pivoted.
               \nSecondary Data: Additional data file that can be used for reference or merging.",
               selectInput(
                 ns("pivot_data_dw"),
                 "Select Data to Pivot:",
                 choices = c("Primary Data" = "primary", "Secondary Data" = "secondary"),
                 selected = "primary"
               )
             )
      )
    ),

    ## Pivot type selection
    fluidRow(
      column(12,
             div(
               title = "Wider: Convert from long to wide format (multiple rows become multiple columns).
               \nLonger: Convert from wide to long format (multiple columns become multiple rows).",
               selectInput(
                 ns("pivot_type_dw"),
                 "Pivot Operation:",
                 choices = c("Wider" = "wider", "Longer" = "longer", "Transpose" = "transpose"),
                 selected = "wider"
               )
             )
      )
    ),

    # Hidden readiness output used by conditionalPanel (must be in DOM)
    tags$div(style = "display: none;", textOutput(ns("pivot_ready"))),

    # Not ready: show large notice
    conditionalPanel(
      condition = sprintf("output['%s'] !== 'true'", ns("pivot_ready")),
      fluidRow(
        column(12,
               div(class = "alert alert-info",
                   style = "margin-top: 10px; font-size: 14px;",
                   "No data available. Please load data first."
               )
        )
      )
    ),

    # Ready: full Pivot UI
    conditionalPanel(
      condition = sprintf("output['%s'] === 'true'", ns("pivot_ready")),

      ## Dynamic pivot options based on type
      fluidRow(
        column(12, uiOutput(ns("pivot_options")))
      ),

      ## Preview section
      fluidRow(
        column(12,
               div(
                 title = "Preview the structure of your pivoted data before applying the operation.",
                 h5("Pivot Preview"),
                 textOutput(ns("pivot_preview_dim")),
                 DT::DTOutput(ns("pivot_preview_table"))
               )
        )
      ),

      br(),

      ## Action buttons
      fluidRow(
        column(6,
               div(
                 title = "Apply the pivot operation with current settings to transform your data.",
                 actionButton(ns("apply_pivot_dw"), "Apply Pivot",
                              width = "100%", class = "btn-success",
                              style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;")
               ))
      )
    )
  )
}
