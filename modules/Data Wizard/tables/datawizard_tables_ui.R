# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_ui.R
# Purpose:
#   Provide the tables ui portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# ==============================================================================
# File: modules/Data Wizard/tables/datawizard_tables_ui.R
#
# Purpose:
#   Defines the static UI layout for the Tables submodule of the Data Wizard.
#   Contains only layout declarations and widget placeholders.
#
# Architectural Role:
#   UI layer of the tables module. Sourced into modEnv via
#   datawizard_tables.R and called from modDataTablesUI() to build the panel
#   content. This file has no knowledge of server-side state or reactivity.
#
# Structure:
#   1. datawizard_tables_UI() - Returns the complete div() hierarchy:
#      a. Tabbed Primary and Additional Data Preview well panel
#      b. Metadata Definition well panel (rHandsontableOutput)
#
# Notes for future developers:
#   - All input and output IDs are namespaced via the `ns` function argument.
#   - Dynamic content (data status badge, stable table output, table rows) is populated at runtime
#     by output handlers in datawizard_tables_observer.R.
#   - Do not add server logic, reactive expressions, or observers here.
#   - The additional_data_section div is hidden by default via shinyjs::hidden().
#     Visibility is controlled by an observer in datawizard_tables_observer.R.
#   - Commented-out UI blocks for metadata details are kept for reference; they
#     were intentionally disabled and should only be re-enabled after confirming
#     that the corresponding output handlers are active.
# ==============================================================================


#' Build the static UI for the Data Tables submodule.
#'
#' @param ns Namespace function from the parent modDataTablesUI() call.
#' @return A div() containing all table panels.
datawizard_tables_UI <- function(ns) {
  div(
    tags$style(HTML(sprintf(
      "#%s > li:nth-child(2) { display: none; }
       .datawizard-table-preview .dataTables_wrapper,
       .datawizard-table-preview table.dataTable,
       .datawizard-table-preview table.dataTable thead th,
       .datawizard-table-preview table.dataTable tbody td { background-color: #fff; }",
      ns("data_viewer_tabs")
    ))),
    # Primary and Additional Data Previews
    wellPanel(
      tabsetPanel(
        id = ns("data_viewer_tabs"),
        tabPanel(
          "Primary Data",
          value = "primary_data",
          tags$p(
            tags$strong("Primary data"),
            " are used for analysis throughout MiraProt. Together with the defined metadata, they are available to the different analysis features."
          ),
          fluidRow(
            column(12, uiOutput(ns("data_status_indicator")))
          ),
          fluidRow(
            column(8, verbatimTextOutput(ns("primary_table_info"))),
            column(4, uiOutput(ns("primary_table_display_controls")))
          ),
          fluidRow(
            column(12, div(
              id = ns("primary_table_preview_container"),
              class = "datawizard-table-preview",
              uiOutput(ns("primary_table_preview_ui"))
            ))
          ),
          fluidRow(
            column(6, selectInput(ns("primary_remove_col"), "Remove primary column", choices = character(0))),
            column(2, br(), actionButton(ns("primary_remove_col_btn"), "Remove column", class = "btn-default",
                                      style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;")),
            column(2, br(), actionButton(ns("primary_remove_row_btn"), "Remove selected rows", class = "btn-default",
                                      style = "background-color: #3498db; border-color: #3498db; color: #fff;")),
            column(2, br(), tags$small("Ctrl/Cmd-click to select multiple rows."))
          )
        ),
        tabPanel(
          "Secondary Data",
          value = "secondary_data",
          # Additional Data Preview (hidden until data is available)
          hidden(
            div(
              id = ns("additional_data_section"),
              tags$p(
                tags$strong("Secondary data"),
                " are not available directly to MiraProt analysis modules. Use them to extend the primary data, for example by merging or pivot merging."
              ),
              fluidRow(
                column(8, verbatimTextOutput(ns("additional_table_info"))),
                column(4, uiOutput(ns("additional_table_display_controls")))
              ),
              fluidRow(
                column(12, div(
                  id = ns("additional_table_preview_container"),
                  class = "datawizard-table-preview",
                  uiOutput(ns("additional_table_preview_ui"))
                ))
              ),
              fluidRow(
                column(6, selectInput(ns("additional_remove_col"), "Remove secondary column", choices = character(0))),
                column(2, br(), actionButton(ns("additional_remove_col_btn"), "Remove column", class = "btn-default",
                                          style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;")),
                column(2, br(), actionButton(ns("additional_remove_row_btn"), "Remove selected rows", class = "btn-default",
                                          style = "background-color: #3498db; border-color: #3498db; color: #fff;")),
                column(2, br(), tags$small("Ctrl/Cmd-click to select multiple rows."))
              )
            )
          )
        )
      )
    ),

    br(),

    # Metadata Definition Table
    wellPanel(
      h4("Define Metadata"),
      fluidRow(
        column(12, uiOutput(ns("metadata_healthcheck")))
      ),
      fluidRow(
        column(
          9,
          checkboxInput(
            ns("pause_metadata_sync"),
            "Pause metadata live synchronization",
            value = TRUE
          ),
          uiOutput(ns("metadata_sync_help"))
        ),
        column(
          3,
          uiOutput(ns("metadata_sync_controls"))
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput(ns("metadata_sync_status"))
        )
      ),
      fluidRow(
        column(
          12,
          rHandsontableOutput(
            ns("metadata_table"),
            width = "100%"
          )
        )
      )
    )
  )
}
