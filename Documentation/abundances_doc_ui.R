# ==============================================================================
# File: Documentation/abundances_doc_ui.R
#
# Purpose:
#   Defines the user interface and server routing for the Abundances
#   documentation module. Contains the navigation sidebar, conditional panels,
#   shared CSS, and the dynamic content output area. Content-rendering functions
#   are sourced from abundances_doc_user.R and abundances_doc_tech.R.
#
# Structure:
#   1. modAbundancesDocUI(id) - Returns the full documentation layout:
#      - Left column (width 3): navigation sidebar with section links
#      - Right column (width 9): dynamic content area (doc_content)
#   2. modAbundancesDocServer(id, debug_level) - Server function:
#      - Navigation observers for user guide and technical sections
#      - Documentation type switcher
#      - Content routing via renderUI
#
# Navigation Sections:
#   User Guide:
#     - Overview
#     - Data Selection and Plotting
#     - Customization
#     - Interactivity
#     - Save Plots
#   Technical Documentation:
#     - Technical Overview
#     - Functions Reference
#     - Data Processing
#     - Integration Details
#
# Notes for future developers:
#   - All input IDs are namespaced via NS(id).
#   - Content-rendering functions are defined in abundances_doc_user.R
#     (user guide) and abundances_doc_tech.R (technical documentation).
#   - To add a new documentation section, add a navigation link in the UI
#     below, a matching observeEvent in the server, and a content-rendering
#     function in the appropriate doc file.
# ==============================================================================


# ==============================================================================
# UI
# ==============================================================================

#' Abundances Documentation Module UI
#'
#' Creates the user interface for the Abundances documentation module.
#' Provides a navigation sidebar and a content area that switches between
#' user guide and technical documentation sections.
#'
#' @param id Character. Module namespace identifier.
#' @return A tagList containing the full documentation layout.
#' @export
modAbundancesDocUI <- function(id) {
  ns <- NS(id)

  tagList(
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
    ),
    fluidRow(
      # ----- Left sidebar: navigation (3 columns) -----
      column(3,
        wellPanel(
          style = "background-color: #f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),

          # Documentation type selector
          radioButtons(ns("doc_type"),
                       "Documentation Type:",
                       choices = c("User Guide" = "user",
                                   "Technical Documentation" = "technical"),
                       selected = "user"),

          hr(),

          # User Guide navigation links
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_abundance"),
                         "Overview",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_data_selection_plotting_abundance"),
                         "Data Selection and Plotting",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customizing_abundance"),
                         "Customization",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_interactive_abundance"),
                         "Interactivity",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_downloading_abundance"),
                         "Save Plots",
                         class = "list-group-item list-group-item-action")
            )
          ),

          # Technical Documentation navigation links
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_abundance"),
                         "Technical Overview",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_abundance"),
                         "Functions Reference",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_data_processing_abundance"),
                         "Data Processing",
                         class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_abundance"),
                         "Integration Details",
                         class = "list-group-item list-group-item-action")
            )
          )
        )
      ),

      # ----- Right area: dynamic content (9 columns) -----
      column(9,
        div(class = "dw-guide-wrap", uiOutput(ns("doc_content")))
      )
    )
  )
}


# ==============================================================================
# SERVER
# ==============================================================================

#' Abundances Documentation Module Server
#'
#' Server logic for navigating and displaying documentation content.
#' Routes navigation events to content-rendering functions defined in
#' abundances_doc_user.R and abundances_doc_tech.R.
#'
#' @param id Character. Module namespace identifier.
#' @param debug_level Integer. Logging verbosity (1 = standard, 2 = verbose).
#' @export
modAbundancesDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debug logging (follows project convention)
    debug_log <- function(message, level = 1) {
      if (exists("DEBUG_LEVEL") && DEBUG_LEVEL >= level) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("[%s] ABUNDANCES_DOC: %s", timestamp, message)
        cat(line, "\n")
        rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
        if (is.function(rec)) rec(line)
      }
    }

    debug_log("Abundances documentation module initialized", 1)

    # Reactive value to track current section
    current_section <- reactiveVal("overview")

    # ------ Navigation observers: User Guide ------
    observeEvent(input$nav_overview_abundance, {
      current_section("overview")
      debug_log("Navigated to Overview section", 2)
    })

    observeEvent(input$nav_data_selection_plotting_abundance, {
      current_section("creating")
      debug_log("Navigated to Data Selection and Plotting section", 2)
    })

    observeEvent(input$nav_customizing_abundance, {
      current_section("customizing")
      debug_log("Navigated to Customization section", 2)
    })

    observeEvent(input$nav_interactive_abundance, {
      current_section("interactive")
      debug_log("Navigated to Interactivity section", 2)
    })

    observeEvent(input$nav_downloading_abundance, {
      current_section("downloading")
      debug_log("Navigated to Save Plots section", 2)
    })

    # ------ Navigation observers: Technical Documentation ------
    observeEvent(input$nav_tech_overview_abundance, {
      current_section("tech_overview")
      debug_log("Navigated to Technical Overview section", 2)
    })

    observeEvent(input$nav_tech_functions_abundance, {
      current_section("tech_functions")
      debug_log("Navigated to Functions Reference section", 2)
    })

    observeEvent(input$nav_tech_data_processing_abundance, {
      current_section("tech_data_processing")
      debug_log("Navigated to Data Processing section", 2)
    })

    observeEvent(input$nav_tech_integration_abundance, {
      current_section("tech_integration")
      debug_log("Navigated to Integration Details section", 2)
    })

    # ------ Documentation type switcher ------
    observeEvent(input$doc_type, {
      if (input$doc_type == "user") {
        current_section("overview")
        debug_log("Switched to User Guide documentation", 1)
      } else {
        current_section("tech_overview")
        debug_log("Switched to Technical documentation", 1)
      }
    })

    # ------ Content routing ------
    output$doc_content <- renderUI({
      debug_log(sprintf("Rendering content for section: %s", current_section()), 2)

      switch(current_section(),
        # User Guide
        "overview"    = render_overview_content_abundance(),
        "creating"    = render_creating_content_abundance(),
        "customizing" = render_customizing_content_abundance(),
        "interactive" = render_interactive_content_abundance(),
        "downloading" = render_downloading_content_abundance(),
        # Technical Documentation
        "tech_overview"        = render_tech_overview_content_abundance(),
        "tech_functions"       = render_tech_functions_content_abundance(),
        "tech_data_processing" = render_tech_data_processing_content_abundance(),
        "tech_integration"     = render_tech_integration_content_abundance(),
        # Fallback
        div(class = "alert alert-info",
            "Please select a section from the navigation menu.")
      )
    })
  })
}
