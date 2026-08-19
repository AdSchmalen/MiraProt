# ./Documentation/tables_modified_doc.R
# Tables Modified Module Documentation (Layout aligned with Datawizard + English text)

############
# UI Function

#' Tables Modified Documentation Module UI
#'
#' @param id module namespace id
#' @export
modTablesModifiedDocUI <- function(id) {
  ns <- NS(id)

  tagList(

    # --- Styles: same look as datawizard ---
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
      # Sidebar UI - unchanged styling
      column(
        width = 3,
        wellPanel(
          style = "background-color: #f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),

          radioButtons(
            ns("tables_doc_type"),
            "Documentation Type:",
            choices = list("User Guide" = "user", "Technical Documentation" = "technical"),
            selected = "user"
          ),

          tags$hr(),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("tables_doc_type")),
            h5("User Guide"),
            actionLink(ns("nav_tables_user"), "Tables Modified — User Guide",
                       class = "list-group-item list-group-item-action")
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("tables_doc_type")),
            h5("Technical Documentation"),
            actionLink(ns("nav_tables_technical"), "Technical Documentation",
                       class = "list-group-item list-group-item-action")
          )
        )
      ),

      column(
        width = 9,
        div(class = "dw-guide-wrap",
            uiOutput(ns("tables_doc_content"))
        )
      )
    )
  )
}

############
# Server Function

#' Tables Modified Documentation Module Server
#'
#' @param id module namespace id
#' @param debug_level Debug level (1=debug, 2=verbose)
#' @export
modTablesModifiedDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_section <- reactiveVal("tables_user")

    observeEvent(input$nav_tables_user, {
      current_section("tables_user")
    })
    observeEvent(input$nav_tables_technical, {
      current_section("tables_technical")
    })
    observeEvent(input$tables_doc_type, {
      current_section(if (input$tables_doc_type == "user") "tables_user" else "tables_technical")
    })

    output$tables_doc_content <- renderUI({
      if (input$tables_doc_type == "user") {
        render_tables_user_guide_content()
      } else {
        render_tables_technical_content()
      }
    })
  })
}

############
# Content Builders (UI fragments)

# -- Simplified English User Guide --
render_tables_user_guide_content <- function() {
  div(
    h2("Tables Modified — User Guide"),
    tags$hr(),

    div(
      class = "alert alert-info",
      h4("What this module does"),
      p("A read-only, interactive view of your processed data with column filters, scrolling, and optional export features.")
    ),

    h3("Key Points"),
    tags$ul(
      tags$li(strong("Read-only:"), " no data manipulation inside this module."),
      tags$li(strong("Interactive table:"), " filtering, scrolling, pagination, export if enabled."),
      tags$li(strong("Use upstream processing:"), " use ", code("datawizard"), " or other upstream modules to prepare the table before viewing here.")
    )
  )
}

# -- Technical Documentation (unchanged content) --
render_tables_technical_content <- function() {
  div(
    h2("Tables Modified — Technical Documentation"),
    tags$hr(),

    h3("Inputs"),
    tags$ul(
      tags$li(code("rv$data_mod"), " — prepared data.frame/tibble to display")
    ),

    h3("Outputs"),
    tags$ul(
      tags$li("Rendered ", code("DT::datatable"), " with optional buttons and column filters")
    ),

    h3("Dependencies"),
    tags$ul(
      tags$li(code("DT")),
      tags$li(code("shiny")),
      tags$li(code("shinyWidgets"), " (optional)")
    )
  )
}
