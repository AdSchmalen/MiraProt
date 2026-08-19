# ./Documentation/dotplot_doc.R
# Dot Plot Module Documentation
# Provides user guide and technical documentation for the Dot Plot module

############
# UI

modDotplotDocUI <- function(id) {
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
               style = "background-color:#f8f9fa; position: sticky; top: 20px;",
               h4("Navigation", style = "margin-bottom: 20px;"),
               radioButtons(ns("_dp_doc_type"),
                            "Documentation Type:",
                            choices = c("User Guide" = "user",
                                        "Technical Documentation" = "technical"),
                            selected = "user"),
               hr(),
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'user'", ns("_dp_doc_type")),
                 h5("User Guide"),
                 div(class = "list-group",
                     actionLink(ns("nav_overview"), "Overview", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_dataplot"), "Data Selection and Plotting", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_customize"), "Customization", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_interactive"), "Interactivity", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_labeling"), "Labeling", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_saveplots"), "Save and Export", class = "list-group-item list-group-item-action")
                 )
               ),
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'technical'", ns("_dp_doc_type")),
                 h5("Technical Documentation"),
                 div(class = "list-group",
                     actionLink(ns("nav_tech_overview"), "Technical Overview", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_tech_functions"), "Functions Reference", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_tech_dataproc"), "Data processing", class = "list-group-item list-group-item-action"),
                     actionLink(ns("nav_tech_integration"), "Integration Details", class = "list-group-item list-group-item-action")
                 )
               )
             )
      ),
      column(9, div(class = "dw-guide-wrap", uiOutput(ns("_dp_doc_content"))))
    )
  )
}

############
# Server

modDotplotDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section <- reactiveVal("overview")

    observeEvent(input$nav_overview,  { current_section("overview") })
    observeEvent(input$nav_dataplot,  { current_section("dataplot") })
    observeEvent(input$nav_customize, { current_section("customize") })
    observeEvent(input$nav_interactive, { current_section("interactive") })
    observeEvent(input$nav_labeling,  { current_section("labeling") })
    observeEvent(input$nav_saveplots, { current_section("saveplots") })

    observeEvent(input$nav_tech_overview,  { current_section("tech_overview") })
    observeEvent(input$nav_tech_functions, { current_section("tech_functions") })
    observeEvent(input$nav_tech_dataproc,  { current_section("tech_dataproc") })
    observeEvent(input$nav_tech_integration,{ current_section("tech_integration") })

    observeEvent(input$`_dp_doc_type`, {
      current_section(if (input$`_dp_doc_type` == "user") "overview" else "tech_overview")
    })

    output$`_dp_doc_content` <- renderUI({
      switch(current_section(),
             "overview"        = render_dotplot_overview_content(),
             "dataplot"        = render_dotplot_dataselection_plotting_content(),
             "customize"       = render_dotplot_customizing_content(),
             "interactive"     = render_dotplot_interactive_content(),
             "labeling"        = render_dotplot_labeling_content(),
             "saveplots"       = render_dotplot_saveplots_content(),
             "tech_overview"   = render_dotplot_tech_overview_content(),
             "tech_functions"  = render_dotplot_tech_functions_content(),
             "tech_dataproc"   = render_dotplot_tech_dataproc_content(),
             "tech_integration"= render_dotplot_tech_integration_content(),
             div(class = "alert alert-info", "Please select a section from the navigation menu.")
      )
    })
  })
}
