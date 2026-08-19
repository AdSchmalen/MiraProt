# ./Documentation/heatmap_doc_ui.R
# Heatmap Documentation Module — UI and Server (Navigation/Router)
#
# This file owns the Shiny documentation UI and routing/server logic for the
# Heatmap documentation module:
#   - modHeatmapDocUI(id)
#   - modHeatmapDocServer(id, debug_level = 1)
#
# Content-rendering providers are defined in companion files:
#   - Documentation/heatmap_doc_user.R
#   - Documentation/heatmap_doc_tech.R
#
# Note: There is no single heatmap_doc.R orchestrator file. This file and the
# companion content-provider files are sourced from Documentation/*.R in app.R.

############
# UI

modHeatmapDocUI <- function(id) {
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
            ns("_heatmap_doc_type"),
            "Documentation Type:",
            choices = c("User Guide" = "user",
                        "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          # User Guide navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("_heatmap_doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_heatmap"),        "Overview",                     class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_proteinplot_heatmap"),     "Protein selection and plotting", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_plottypes_heatmap"),       "Plot Types",                  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customize_heatmap"),       "Customization",               class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_saveplots_heatmap"),       "Save plots",                  class = "list-group-item list-group-item-action")
            )
          ),
          # Technical Documentation navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("_heatmap_doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_heatmap"),    "Technical Overview",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_heatmap"),   "Functions Reference",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_dataproc_heatmap"),    "Data processing",      class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_heatmap"), "Integration Details",  class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("_heatmap_doc_content")))
      )
    )
  )
}

############
# Server

modHeatmapDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section_heatmap <- reactiveVal("overview")

    # User Guide navigation
    observeEvent(input$nav_overview_heatmap,    { current_section_heatmap("overview") })
    observeEvent(input$nav_proteinplot_heatmap, { current_section_heatmap("proteinplot") })
    observeEvent(input$nav_customize_heatmap,   { current_section_heatmap("customize") })
    observeEvent(input$nav_plottypes_heatmap,   { current_section_heatmap("plottypes") })
    observeEvent(input$nav_saveplots_heatmap,   { current_section_heatmap("saveplots") })

    # Technical navigation
    observeEvent(input$nav_tech_overview_heatmap,    { current_section_heatmap("tech_overview") })
    observeEvent(input$nav_tech_functions_heatmap,   { current_section_heatmap("tech_functions") })
    observeEvent(input$nav_tech_dataproc_heatmap,    { current_section_heatmap("tech_dataproc") })
    observeEvent(input$nav_tech_integration_heatmap, { current_section_heatmap("tech_integration") })

    # Switch between user / technical default sections
    observeEvent(input$`_heatmap_doc_type`, {
      if (isTRUE(input$`_heatmap_doc_type` == "user")) {
        current_section_heatmap("overview")
      } else {
        current_section_heatmap("tech_overview")
      }
    })

    output$`_heatmap_doc_content` <- renderUI({
      switch(
        current_section_heatmap(),
        # User Guide sections
        "overview"        = render_heatmap_overview_content_heatmap(),
        "proteinplot"     = render_heatmap_proteinselection_plotting_content_heatmap(),
        "customize"       = render_heatmap_customizing_content_heatmap(),
        "plottypes"       = render_heatmap_plottypes_content_heatmap(),
        "saveplots"       = render_heatmap_saveplots_content_heatmap(),
        # Technical sections
        "tech_overview"   = render_heatmap_tech_overview_content_heatmap(),
        "tech_functions"  = render_heatmap_tech_functions_content_heatmap(),
        "tech_dataproc"   = render_heatmap_tech_dataproc_content_heatmap(),
        "tech_integration"= render_heatmap_tech_integration_content_heatmap(),
        # Fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
