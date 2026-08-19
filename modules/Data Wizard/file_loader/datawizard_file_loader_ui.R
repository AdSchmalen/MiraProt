# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_ui.R
# Purpose:
#   Provide the file loader ui portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# modules/Data Wizard/file_loader/datawizard_file_loader_ui.R

#' File Loader Module UI
#' @param id module namespace id
#' @export
modFileLoaderUI <- function(id) {
  ns <- NS(id)
  datawizard_supported_upload_extensions <- paste0(".", c(datawizard_table_extensions_dw, datawizard_session_extensions_dw))

  tagList(
    tags$style(HTML("
  .selectize-control.single .selectize-input {
    height: 34px !important;
    line-height: 34px !important;
    white-space: nowrap !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    display: flex !important;
    align-items: center !important;
    padding: 0 8px !important;
  }

  .selectize-control.single .selectize-input > div {
    max-width: 100% !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    white-space: nowrap !important;
  }
")),

    tags$script(HTML("
  Shiny.addCustomMessageHandler('setSelectizeTitle', function(message) {
    var selector = '#' + message.inputId + '+ .selectize-control .selectize-input';
    function setTitle() {
      var input = $(selector);
      if (input.length) {
        input.attr('title', message.label);
      } else {
        setTimeout(setTitle, 50); // erneut versuchen
      }
    }
    setTitle();
  });
"))
    ,
    tags$script(HTML("
  $(document).on('change', '.selectized', function() {
    var input = $(this).next('.selectize-control').find('.selectize-input');
    var text = input.text();
    input.attr('title', text);
  });
")),
    #     tags$script(HTML("
    #   function updateSelectizeTitle(el) {
    #     var text = $(el).text();
    #     $(el).attr('title', text);
    #   }
    #
    #   // Beobachte neue Selectize Controls
    #   $(document).on('DOMNodeInserted', function(e) {
    #     if ($(e.target).hasClass('selectize-control')) {
    #       var input = $(e.target).find('.selectize-input');
    #
    #       // Warten, bis Text gesetzt ist (z.B. initialer Wert)
    #       setTimeout(function() {
    #         updateSelectizeTitle(input);
    #       }, 100); // kleine Verzögerung reicht aus
    #     }
    #   });
    #
    #   // Aktualisiere Tooltip bei jeder Änderung
    #   $(document).on('change', '.selectized', function() {
    #     var input = $(this).next('.selectize-control').find('.selectize-input');
    #     updateSelectizeTitle(input);
    #   });
    # ")),
    #     tags$script(HTML("
    #   Shiny.addCustomMessageHandler('setSelectizeTitle', function(message) {
    #     var selector = '#' + message.inputId + '+ .selectize-control .selectize-input';
    #     var input = $(selector);
    #     input.attr('title', message.label);
    #   });
    # ")),


    wellPanel(
      tabsetPanel(
        id   = ns("data_upload_tab"),
        type = "tabs",

        # ---- Tab 1: Primary Data ----
        tabPanel(
          title = "Primary Data",
          br(),
          fileInput(ns("file"), "Choose primary data file", accept = datawizard_supported_upload_extensions),
          conditionalPanel(
            condition = "output.show_primary_header_row === 'true'",
            ns = ns,
            div(
              id = ns("header_sheet_1"),
              fluidRow(
                column(6, numericInput(ns("header_row"),  "Header Row", value = 1, min = 1)),
                column(6, uiOutput(ns("sheetDropdown")))
              )
            )
          )
        ),

        # ---- Tab 2: Additional Data ----
        tabPanel(
          title = "Secondary Data",
          br(),
          fileInput(ns("file2"), "Choose additional data file", accept = datawizard_supported_upload_extensions),
          conditionalPanel(
            condition = "output.show_secondary_header_row === 'true'",
            ns = ns,
            div(
              id = ns("header_sheet_2"),
              fluidRow(
                column(6, numericInput(ns("header_row2"), "Header Row", value = 1, min = 1)),
                column(6, uiOutput(ns("sheetDropdown2")))
              )
            )
          )
        )
      ),

      # ---- Reset Button ----
      fluidRow(
        column(6, br()),
        column(6, actionButton(ns("reset_btn_dw"), "Reset", width = "100%", class = "btn-default",
                               style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;"))
      )
    )
  )
}
