# ==============================================================================
# File: Documentation/pca_doc_ui.R
#
# Purpose:
#   Defines the user interface and server routing for the Dimensionality
#   Reduction documentation module. Contains the navigation sidebar,
#   conditional panels, shared CSS, and the dynamic content output area.
#   Content-rendering functions are sourced from pca_doc_user.R and
#   pca_doc_tech.R.
#
# Note:
#   The orchestrator file pca_doc.R has been removed. This file, together with
#   pca_doc_tech.R and pca_doc_user.R, forms the complete documentation module.
#   All three files are sourced into modEnv automatically by the alphabetical
#   list.files() loop in app.R. No explicit source() calls are needed.
#
# Public API (consumed by R/ui.R and R/server_modules.R):
#   modDimRedDocUI(id)                      - defined below
#   modDimRedDocServer(id, debug_level = 1) - defined below
#
# Structure:
#   1. modDimRedDocUI(id) - Returns the full documentation layout:
#      - Left column (width 3): navigation sidebar with section links
#      - Right column (width 9): dynamic content area (doc_content_dimred)
#   2. modDimRedDocServer(id, debug_level) - Server function:
#      - Navigation observers for user guide and technical sections
#      - Documentation type switcher
#      - Content routing via renderUI
#
# Navigation Sections:
#   User Guide:
#     - Overview
#     - PCA
#     - UMAP
#     - Customization
#     - Interactive Features
#     - Save Plots
#   Technical Documentation:
#     - Technical Overview
#     - Functions Reference
#     - Data Processing
#     - Integration Details
# ==============================================================================

#' Dimensionality Reduction Documentation Module UI
#'
#' Creates the user interface for the Dimensionality Reduction documentation
#' module.
#'
#' @param id Character. Module namespace identifier.
#' @return A tagList containing the full documentation layout.
#' @export
modDimRedDocUI <- function(id) {
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
      column(3,
        wellPanel(
          style = "background-color: #f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),
          radioButtons(
            ns("doc_type_dimred"),
            "Documentation Type:",
            choices = c(
              "User Guide" = "user",
              "Technical Documentation" = "technical"
            ),
            selected = "user"
          ),
          hr(),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("doc_type_dimred")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(
                ns("nav_overview_dimred"),
                "Overview",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_pca_dimred"),
                "PCA",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_umap_dimred"),
                "UMAP",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_customizing_dimred"),
                "Customization",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_interactive_dimred"),
                "Interactive Features",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_downloading_dimred"),
                "Save Plots",
                class = "list-group-item list-group-item-action"
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("doc_type_dimred")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(
                ns("nav_tech_overview_dimred"),
                "Technical Overview",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_tech_functions_dimred"),
                "Functions Reference",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_tech_data_processing_dimred"),
                "Data Processing",
                class = "list-group-item list-group-item-action"
              ),
              actionLink(
                ns("nav_tech_integration_dimred"),
                "Integration Details",
                class = "list-group-item list-group-item-action"
              )
            )
          )
        )
      ),
      column(9,
        div(class = "dw-guide-wrap", uiOutput(ns("doc_content_dimred")))
      )
    )
  )
}

#' Dimensionality Reduction Documentation Module Server
#'
#' Server logic for displaying documentation content.
#'
#' @param id Character. Module namespace identifier.
#' @param debug_level Integer. Logging verbosity (1 = standard, 2 = verbose).
#' @export
modDimRedDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "DIMRED_DOC", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level) {
          timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          cat(sprintf("[%s] DIMRED_DOC: %s", timestamp, message), "\n")
        }
      }
    }
    debug_log("Dimensionality Reduction documentation module initialized", 1)

    current_section <- reactiveVal("overview")

    observeEvent(input$nav_overview_dimred, {
      current_section("overview")
      debug_log("Navigated to Overview", 2)
    })

    observeEvent(input$nav_pca_dimred, {
      current_section("pca")
      debug_log("Navigated to PCA", 2)
    })

    observeEvent(input$nav_umap_dimred, {
      current_section("umap")
      debug_log("Navigated to UMAP", 2)
    })

    observeEvent(input$nav_customizing_dimred, {
      current_section("customizing")
      debug_log("Navigated to Customization", 2)
    })

    observeEvent(input$nav_interactive_dimred, {
      current_section("interactive")
      debug_log("Navigated to Interactive Features", 2)
    })

    observeEvent(input$nav_downloading_dimred, {
      current_section("downloading")
      debug_log("Navigated to Save Plots", 2)
    })

    observeEvent(input$nav_tech_overview_dimred, {
      current_section("tech_overview")
      debug_log("Navigated to Technical Overview", 2)
    })

    observeEvent(input$nav_tech_functions_dimred, {
      current_section("tech_functions")
      debug_log("Navigated to Functions Reference", 2)
    })

    observeEvent(input$nav_tech_data_processing_dimred, {
      current_section("tech_data_processing")
      debug_log("Navigated to Data Processing", 2)
    })

    observeEvent(input$nav_tech_integration_dimred, {
      current_section("tech_integration")
      debug_log("Navigated to Integration Details", 2)
    })

    observeEvent(input$doc_type_dimred, {
      if (input$doc_type_dimred == "user") {
        current_section("overview")
        debug_log("Switched to User Guide", 1)
      } else {
        current_section("tech_overview")
        debug_log("Switched to Technical documentation", 1)
      }
    })

    output$doc_content_dimred <- renderUI({
      debug_log(sprintf("Rendering content for section: %s", current_section()), 2)

      switch(
        current_section(),
        "overview" = render_dimred_overview_content(),
        "pca" = render_dimred_pca_content(),
        "umap" = render_dimred_umap_content(),
        "customizing" = render_dimred_customizing_content(),
        "interactive" = render_dimred_interactive_content(),
        "downloading" = render_downloading_content_dimred(),
        "tech_overview" = render_dimred_tech_overview_content(),
        "tech_functions" = render_dimred_tech_functions_content(),
        "tech_data_processing" = render_dimred_tech_data_processing_content(),
        "tech_integration" = render_dimred_tech_integration_content(),
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
