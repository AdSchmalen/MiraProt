# ./Documentation/grid_doc.R
# Plot Grid Module Documentation
# Provides user guide and technical documentation for the Plot Grid module

############
# UI

modGridDocUI <- function(id) {
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
            ns("_grid_doc_type"),
            "Documentation Type:",
            choices = c("User Guide" = "user",
                        "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          # User Guide navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("_grid_doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_grid"),     "Overview",        class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_selection_grid"),    "Plot Selection and Management",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customize_grid"),    "Customization and Layout",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_saveplots_grid"),    "Save and Export",      class = "list-group-item list-group-item-action")
            )
          ),
          # Technical Documentation navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("_grid_doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_grid"),    "Technical Overview",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_grid"),   "Functions Reference",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_processing_grid"),  "Data Flow and Reactivity",      class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_grid"), "Integration Details",  class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("_grid_doc_content")))
      )
    )
  )
}

############
# Server

modGridDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section_grid <- reactiveVal("overview")

    # User Guide navigation
    observeEvent(input$nav_overview_grid,    { current_section_grid("overview") })
    observeEvent(input$nav_selection_grid,   { current_section_grid("selection") })
    observeEvent(input$nav_customize_grid,   { current_section_grid("customize") })
    observeEvent(input$nav_saveplots_grid,   { current_section_grid("saveplots") })

    # Technical navigation
    observeEvent(input$nav_tech_overview_grid,    { current_section_grid("tech_overview") })
    observeEvent(input$nav_tech_functions_grid,   { current_section_grid("tech_functions") })
    observeEvent(input$nav_tech_processing_grid,  { current_section_grid("tech_processing") })
    observeEvent(input$nav_tech_integration_grid, { current_section_grid("tech_integration") })

    # Switch between user / technical default sections
    observeEvent(input$`_grid_doc_type`, {
      if (isTRUE(input$`_grid_doc_type` == "user")) {
        current_section_grid("overview")
      } else {
        current_section_grid("tech_overview")
      }
    })

    output$`_grid_doc_content` <- renderUI({
      switch(
        current_section_grid(),
        # User Guide sections
        "overview"       = render_grid_overview_content_grid(),
        "selection"      = render_grid_selection_content_grid(),
        "customize"      = render_grid_customizing_content_grid(),
        "saveplots"      = render_grid_saveplots_content_grid(),
        # Technical sections
        "tech_overview"   = render_grid_tech_overview_content_grid(),
        "tech_functions"  = render_grid_tech_functions_content_grid(),
        "tech_processing" = render_grid_tech_processing_content_grid(),
        "tech_integration"= render_grid_tech_integration_content_grid(),
        # Fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}
