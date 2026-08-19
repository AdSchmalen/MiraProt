# ==============================================================================
# File: modules/Data Wizard/basemean/datawizard_basemean_ui.R
#
# Purpose:
#   Defines the static UI component for the Basemean submodule of the
#   Data Wizard. Contains only layout and input widget declarations.
#
# Architectural Role:
#   UI layer of the basemean module. Sourced into modEnv via
#   datawizard_basemean.R and called from modBasemeanUI() to build the tab
#   panel content. This file has no knowledge of server-side state.
#
# Structure:
#   1. datawizard_basemean_UI() - Returns a tagList of all UI controls:
#      - Abundance type selector (selectInput)
#      - Sample selector (selectizeInput, multiple)
#      - Suffix text input
#      - Add Basemean button (success)
#      - Clear Basemeans button (warning)
#
# Notes for future developers:
#   - All input IDs are namespaced via the `ns` function argument.
#   - Choices for the dropdowns are populated at runtime by observers in
#     datawizard_basemean_observer.R, not here.
#   - Do not add server logic, reactive expressions, or observers here.
#   - If you add a new input widget, register the corresponding ID in
#     datawizard_basemean_observer.R and export it from create_basemean_state()
#     if it requires reactive state.
# ==============================================================================

datawizard_basemean_UI <- function(ns) {
  tagList(
    shinyjs::useShinyjs(),
    h4("Basemean Calculation"),
    fluidRow(
      column(12,
             selectInput(
               ns("abundance_type_basemean"),
               "Select Abundance Type:",
               choices = NULL,
               width = "100%"
             )
      )
    ),
    fluidRow(
      column(12,
             selectizeInput(
               ns("sample_selection_basemean"),
               "Select Samples:",
               choices = NULL,
               multiple = TRUE,
               width = "100%"
             )
      )
    ),
    fluidRow(
      column(12,
             textInput(
               ns("suffix_basemean"),
               "Suffix (optional):",
               value = "",
               width = "100%"
             )
      )
    ),
    fluidRow(
      column(6,
             actionButton(
               ns("add_basemean"),
               "Add Basemean",
               width = "100%",
               class = "btn-success",
               style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;"
             )
      ),
      column(6,
             actionButton(
               ns("clear_basemean"),
               "Clear Basemeans",
               width = "100%",
               class = "btn-default",
               style = "background-color: #3498db; border-color: #3498db; color: #fff;"
             )
      )
    )#,
    # hr(),
    # verbatimTextOutput(ns("basemean_status"))
  )
}
