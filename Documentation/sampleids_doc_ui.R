# ==============================================================================
# File: Documentation/sampleids_doc_ui.R
#
# Purpose:
#   Describes the user interface components of the Sample IDs documentation
#   module. Defines the UI layout (modSampleIDsDocUI) and server logic
#   (modSampleIDsDocServer) for navigating between the user guide and
#   technical documentation.
#
# Sourced by:
#   Documentation/sampleids_doc.R (orchestrator)
#
# Dependencies:
#   - Documentation/sampleids_doc_user.R (user guide content functions)
#   - Documentation/sampleids_doc_tech.R (technical documentation content functions)
# ==============================================================================


# ==============================================================================
# CSS Styles
# ==============================================================================

#' Shared CSS styles for the documentation module.
#'
#' Uses the same design tokens as the datawizard documentation (white card,
#' gray border, soft shadow) for a consistent look across all documentation
#' pages.
#' @return A tags$head element containing CSS rules.
sampleids_doc_styles <- function() {
  tags$head(
    tags$style(HTML("
:root{
  --dw-bg:#f5f7fb;
  --dw-surface:#ffffff;
  --dw-border:#e6e8ef;
  --dw-text:#1f2633;
  --dw-text-soft:#56607a;
  --dw-brand:#3c7bf1;
  --dw-brand-10:rgba(60,123,241,.10);
  --dw-shadow:0 8px 22px rgba(31,38,51,.10);
}
html, body { background: var(--dw-bg); }
html { scroll-behavior: smooth; }

/* Main content card identical to datawizard */
.dw-guide-wrap {
  background: var(--dw-surface);
  border: 1px solid var(--dw-border);
  border-radius: 14px;
  box-shadow: var(--dw-shadow);
  padding: 24px;
}
.dw-guide-wrap .alert{
  border:1px solid var(--dw-border);
  border-radius:12px;
}
"))
  )
}


# ==============================================================================
# UI Function
# ==============================================================================

#' Sample IDs Documentation Module UI
#'
#' Creates the user interface for the Sample IDs documentation module.
#' The layout consists of a left sidebar (3 columns) for navigation and a
#' main content area (9 columns) that renders documentation content
#' dynamically.
#'
#' Navigation is split into two documentation types:
#'
#' User Guide sections:
#'   - Overview
#'   - Data Selection and Plotting
#'   - Customization
#'   - Interactivity
#'   - Save Plots
#'
#' Technical Documentation sections:
#'   - Technical Overview
#'   - Functions Reference
#'   - Data Processing
#'   - Integration Details
#'
#' @param id Module namespace id.
#' @export
modSampleIDsDocUI <- function(id) {
  ns <- NS(id)

  tagList(
    sampleids_doc_styles(),

    fluidRow(
      # Left sidebar for navigation (3 columns)
      column(3,
             wellPanel(
               style = "background-color: #f8f9fa; position: sticky; top: 20px;",
               h4("Navigation", style = "margin-bottom: 20px;"),

               # Documentation type selector
               radioButtons(ns("_sampleIDs_doc_type"),
                            "Documentation Type:",
                            choices = c("User Guide" = "user",
                                        "Technical Documentation" = "technical"),
                            selected = "user"),

               hr(),

               # Dynamic navigation based on selected type
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'user'", ns("_sampleIDs_doc_type")),
                 h5("User Guide"),
                 div(
                   class = "list-group",
                   actionLink(ns("nav_overview_sampleIDs"), "Overview",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_data_selection_plotting_sampleIDs"), "Data Selection and Plotting",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_customizing_sampleIDs"), "Customization",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_interactive_sampleIDs"), "Interactivity",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_downloading_sampleIDs"), "Save Plots",
                              class = "list-group-item list-group-item-action")
                 )
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'technical'", ns("_sampleIDs_doc_type")),
                 h5("Technical Documentation"),
                 div(
                   class = "list-group",
                   actionLink(ns("nav_tech_overview_sampleIDs"), "Technical Overview",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_functions_sampleIDs"), "Functions Reference",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_data_processing_sampleIDs"), "Data Processing",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_integration_sampleIDs"), "Integration Details",
                              class = "list-group-item list-group-item-action")
                 )
               )
             )
      ),

      # Main content area (9 columns)
      column(9,
             div(class = "dw-guide-wrap",
                 uiOutput(ns("_sampleIDs_doc_content"))
             )
      )
    )
  )
}


# ==============================================================================
# Server Function
# ==============================================================================

#' Sample IDs Documentation Module Server
#'
#' Server logic for displaying documentation content. Manages navigation
#' state via a single reactiveVal (current_section) and renders the
#' appropriate content function based on the selected section.
#'
#' Content rendering is delegated to functions defined in:
#'   - sampleids_doc_user.R (user guide sections)
#'   - sampleids_doc_tech.R (technical documentation sections)
#'
#' @param id Module namespace id.
#' @param debug_level Debug level for logging (1 = debug, 2 = verbose).
#' @export
modSampleIDsDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debug logging function (following project standards)
    debug_log <- function(message, level = 1) {
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("[%s] SAMPLEIDS_DOC: %s", timestamp, message)
        cat(line, "\n")
        rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
        if (is.function(rec)) rec(line)
      }
    }

    debug_log("Sample IDs documentation module initialized", 1)

    # Reactive value to track current section
    current_section <- reactiveVal("overview")

    # ------------------------------------------------------------------
    # Navigation observers - User Guide
    # ------------------------------------------------------------------

    observeEvent(input$`nav_overview_sampleIDs`, {
      current_section("overview")
      debug_log("Navigated to Overview section", 2)
    })

    observeEvent(input$`nav_data_selection_plotting_sampleIDs`, {
      current_section("data_selection_plotting")
      debug_log("Navigated to Data Selection and Plotting section", 2)
    })

    observeEvent(input$`nav_customizing_sampleIDs`, {
      current_section("customizing")
      debug_log("Navigated to Customization section", 2)
    })

    observeEvent(input$`nav_interactive_sampleIDs`, {
      current_section("interactive")
      debug_log("Navigated to Interactivity section", 2)
    })

    observeEvent(input$`nav_downloading_sampleIDs`, {
      current_section("downloading")
      debug_log("Navigated to Save Plots section", 2)
    })

    # ------------------------------------------------------------------
    # Navigation observers - Technical Documentation
    # ------------------------------------------------------------------

    observeEvent(input$`nav_tech_overview_sampleIDs`, {
      current_section("tech_overview")
      debug_log("Navigated to Technical Overview section", 2)
    })

    observeEvent(input$`nav_tech_functions_sampleIDs`, {
      current_section("tech_functions")
      debug_log("Navigated to Functions Reference section", 2)
    })

    observeEvent(input$`nav_tech_data_processing_sampleIDs`, {
      current_section("tech_data_processing")
      debug_log("Navigated to Data Processing section", 2)
    })

    observeEvent(input$`nav_tech_integration_sampleIDs`, {
      current_section("tech_integration")
      debug_log("Navigated to Integration Details section", 2)
    })

    # ------------------------------------------------------------------
    # Reset section when switching documentation type
    # ------------------------------------------------------------------

    observeEvent(input$`_sampleIDs_doc_type`, {
      if (input$`_sampleIDs_doc_type` == "user") {
        current_section("overview")
        debug_log("Switched to User Guide documentation", 1)
      } else {
        current_section("tech_overview")
        debug_log("Switched to Technical documentation", 1)
      }
    })

    # ------------------------------------------------------------------
    # Content rendering
    # ------------------------------------------------------------------

    output$`_sampleIDs_doc_content` <- renderUI({
      debug_log(sprintf("Rendering content for section: %s", current_section()), 2)

      switch(current_section(),
        # User Guide
        "overview"                = render_overview_content(),
        "data_selection_plotting" = render_data_selection_plotting_content_sampleids(),
        "customizing"             = render_customizing_content(),
        "interactive"             = render_interactive_content_sampleids(),
        "downloading"             = render_downloading_content_sampleids(),

        # Technical Documentation
        "tech_overview"           = render_tech_overview_content(),
        "tech_functions"          = render_tech_functions_content(),
        "tech_data_processing"    = render_tech_data_processing_content(),
        "tech_integration"        = render_tech_integration_content(),

        # Default fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
