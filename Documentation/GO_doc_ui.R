# ./Documentation/GO_doc.R
# GO Module Documentation
# Provides user guide and technical documentation for the GO module

############
# UI

modGODocUI <- function(id) {
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
            ns("_go_doc_type"),
            "Documentation Type:",
            choices = c("User Guide" = "user",
                        "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          # User Guide navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("_go_doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_GO"),    "Overview",                       class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_dataplot_GO"),    "Data selection and Plotting",    class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customize_GO"),   "Customization",                  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_plottypes_GO"),   "Plot Types",                     class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_saveplots_GO"),   "Save Plots",                     class = "list-group-item list-group-item-action")
            )
          ),
          # Technical Documentation navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("_go_doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_GO"),    "Technical Overview",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_GO"),   "Functions Reference",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_dataproc_GO"),    "Data processing",      class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_GO"), "Integration Details",  class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("_go_doc_content")))
      )
    )
  )
}

############
# Server

modGODocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section_GO <- reactiveVal("overview")

    # User Guide navigation
    observeEvent(input$nav_overview_GO,    { current_section_GO("overview") })
    observeEvent(input$nav_dataplot_GO,    { current_section_GO("dataplot") })
    observeEvent(input$nav_customize_GO,   { current_section_GO("customize") })
    observeEvent(input$nav_plottypes_GO,   { current_section_GO("plottypes") })
    observeEvent(input$nav_saveplots_GO,   { current_section_GO("saveplots") })

    # Technical navigation
    observeEvent(input$nav_tech_overview_GO,    { current_section_GO("tech_overview") })
    observeEvent(input$nav_tech_functions_GO,   { current_section_GO("tech_functions") })
    observeEvent(input$nav_tech_dataproc_GO,    { current_section_GO("tech_dataproc") })
    observeEvent(input$nav_tech_integration_GO, { current_section_GO("tech_integration") })

    # Switch between user / technical default sections
    observeEvent(input$`_go_doc_type`, {
      if (isTRUE(input$`_go_doc_type` == "user")) {
        current_section_GO("overview")
      } else {
        current_section_GO("tech_overview")
      }
    })

    output$`_go_doc_content` <- renderUI({
      switch(
        current_section_GO(),
        # User Guide sections
        "overview"        = render_GO_overview_content_GO(),
        "dataplot"        = render_GO_dataselection_plotting_content_GO(),
        "customize"       = render_GO_customizing_content_GO(),
        "plottypes"       = render_GO_plottypes_content_GO(),
        "saveplots"       = render_GO_saveplots_content_GO(),
        # Technical sections
        "tech_overview"   = render_GO_tech_overview_content_GO(),
        "tech_functions"  = render_GO_tech_functions_content_GO(),
        "tech_dataproc"   = render_GO_tech_dataproc_content_GO(),
        "tech_integration"= render_GO_tech_integration_content_GO(),
        # Fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
