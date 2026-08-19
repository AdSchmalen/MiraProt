# ==============================================================================
# File: Documentation/GSEA_doc_ui.R
#
# Audience:
#   MiraProt developers and maintainers working on documentation navigation.
#
# Purpose:
#   Defines the UI layout and server-side routing for the GSEA documentation
#   module. It provides a shared navigation sidebar, documentation-type switch,
#   and section-to-render-function mapping for both user and technical content.
#   Content bodies are defined in GSEA_doc_user.R, GSEA_doc_tech_core.R, and GSEA_doc_tech_functions.R.
# ==============================================================================

modGSEADocUI <- function(id) {
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
      column(
        3,
        wellPanel(
          style = "background-color:#f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),
          radioButtons(
            ns("_gsea_doc_type"),
            "Documentation Type:",
            choices = c("User Guide" = "user",
                        "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          # User Guide navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("_gsea_doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_GSEA"),    "Overview",                                class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_dataplot_GSEA"),    "Data Selection, Ranking, and Plotting",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customize_GSEA"),   "Customization",                           class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_plottypes_GSEA"),   "Plot Types",                              class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_saveplots_GSEA"),   "Save Plots",                              class = "list-group-item list-group-item-action")
            )
          ),
          # Technical Documentation navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("_gsea_doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_GSEA"),    "Technical Overview",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_GSEA"),   "Functions Reference",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_dataproc_GSEA"),    "Data Processing",      class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_GSEA"), "Integration Details",  class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("_gsea_doc_content")))
      )
    )
  )
}

############
# Server

modGSEADocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section_GSEA <- reactiveVal("overview")

    # User Guide navigation
    observeEvent(input$nav_overview_GSEA,    { current_section_GSEA("overview") })
    observeEvent(input$nav_dataplot_GSEA,    { current_section_GSEA("dataplot") })
    observeEvent(input$nav_customize_GSEA,   { current_section_GSEA("customize") })
    observeEvent(input$nav_plottypes_GSEA,   { current_section_GSEA("plottypes") })
    observeEvent(input$nav_saveplots_GSEA,   { current_section_GSEA("saveplots") })

    # Technical navigation
    observeEvent(input$nav_tech_overview_GSEA,    { current_section_GSEA("tech_overview") })
    observeEvent(input$nav_tech_functions_GSEA,   { current_section_GSEA("tech_functions") })
    observeEvent(input$nav_tech_dataproc_GSEA,    { current_section_GSEA("tech_dataproc") })
    observeEvent(input$nav_tech_integration_GSEA, { current_section_GSEA("tech_integration") })

    # Switch between user / technical default sections
    observeEvent(input$`_gsea_doc_type`, {
      if (isTRUE(input$`_gsea_doc_type` == "user")) {
        current_section_GSEA("overview")
      } else {
        current_section_GSEA("tech_overview")
      }
    })

    output$`_gsea_doc_content` <- renderUI({
      switch(
        current_section_GSEA(),
        # User Guide sections
        "overview"        = render_GSEA_overview_content_GSEA(),
        "dataplot"        = render_GSEA_dataselection_plotting_content_GSEA(),
        "customize"       = render_GSEA_customizing_content_GSEA(),
        "plottypes"       = render_GSEA_plottypes_content_GSEA(),
        "saveplots"       = render_GSEA_saveplots_content_GSEA(),
        # Technical sections
        "tech_overview"   = render_GSEA_tech_core_overview_content_GSEA(),
        "tech_functions"  = render_GSEA_tech_functions_content_GSEA_v2(),
        "tech_dataproc"   = render_GSEA_tech_core_dataproc_content_GSEA(),
        "tech_integration"= render_GSEA_tech_core_integration_content_GSEA(),
        # Fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
