# ==============================================================================
# File: Documentation/venn_doc.R
#
# Purpose:
#   Orchestrator for the Venn / UpSet documentation module. This file keeps the
#   documentation shell, navigation, and section routing in one place while the
#   actual prose content lives in:
#
#     venn_doc_tech.R  - developer-facing technical documentation
#     venn_doc_user.R  - end-user guide for scientific users
#
# Public API:
#   modVennDocUI(id)
#   modVennDocServer(id, debug_level = 1)
#
# Notes for future developers:
#   - This file should not contain section-level documentation prose.
#   - Add or remove content sections in venn_doc_tech.R / venn_doc_user.R and
#     update the navigation vectors below to keep the UI in sync.
#   - The documentation loader sources files in Documentation/ automatically,
#     so no explicit source() calls are required here.
# ==============================================================================

venn_doc_user_sections <- list(
  overview        = list(id = "nav_overview_venn",        label = "Overview"),
  diagram_types   = list(id = "nav_diagram_types_venn",   label = "Diagram Types"),
  creating_lists  = list(id = "nav_creating_lists_venn",  label = "Creating Protein Lists"),
  venn_diagrams   = list(id = "nav_venn_diagrams_venn",   label = "Venn Diagrams"),
  upset_plots     = list(id = "nav_upset_plots_venn",     label = "UpSet Plots"),
  customization   = list(id = "nav_customization_venn",   label = "Customization"),
  downloading     = list(id = "nav_downloading_venn",     label = "Save and Export")
)

venn_doc_tech_sections <- list(
  tech_overview         = list(id = "nav_tech_overview_venn",         label = "Technical Overview"),
  tech_functions        = list(id = "nav_tech_functions_venn",        label = "Functions and API"),
  tech_data_processing  = list(id = "nav_tech_data_processing_venn",  label = "Reactive Data Flow"),
  tech_algorithms       = list(id = "nav_tech_algorithms_venn",       label = "Plot Logic"),
  tech_integration      = list(id = "nav_tech_integration_venn",      label = "Integration Details")
)

render_venn_doc_nav_group <- function(ns, sections) {
  nav_links <- lapply(names(sections), function(section_key) {
    section <- sections[[section_key]]
    actionLink(
      ns(section$id),
      section$label,
      class = "list-group-item list-group-item-action"
    )
  })

  div(class = "list-group", do.call(tagList, nav_links))
}

render_venn_doc_content <- function(section) {
  switch(
    section,
    overview             = render_overview_content_venn(),
    diagram_types        = render_diagram_types_content_venn(),
    creating_lists       = render_creating_lists_content_venn(),
    venn_diagrams        = render_venn_diagrams_content_venn(),
    upset_plots          = render_upset_plots_content_venn(),
    customization        = render_customization_content_venn(),
    downloading          = render_downloading_content_venn(),
    tech_overview        = render_tech_overview_content_venn(),
    tech_functions       = render_tech_functions_content_venn(),
    tech_data_processing = render_tech_data_processing_content_venn(),
    tech_algorithms      = render_tech_algorithms_content_venn(),
    tech_integration     = render_tech_integration_content_venn(),
    render_overview_content_venn()
  )
}

#' Venn documentation UI
#' @param id Module namespace ID
modVennDocUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
:root{
  --dw-bg:#f5f7fb;
  --dw-surface:#ffffff;
  --dw-border:#e6e8ef;
  --dw-text:#1f2633;
  --dw-brand:#3c7bf1;
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
.dw-guide-wrap .alert {
  border: 1px solid var(--dw-border);
  border-radius: 12px;
}
"))
    ),
    fluidRow(
      column(
        3,
        wellPanel(
          style = "background-color: #f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),
          radioButtons(
            ns("doc_type_venn"),
            "Documentation Type:",
            choices = c("User Guide" = "user", "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("doc_type_venn")),
            h5("User Guide"),
            render_venn_doc_nav_group(ns, venn_doc_user_sections)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("doc_type_venn")),
            h5("Technical Documentation"),
            render_venn_doc_nav_group(ns, venn_doc_tech_sections)
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("doc_content_venn")))
      )
    )
  )
}

#' Venn documentation server
#' @param id Module namespace ID
#' @param debug_level Debug verbosity level
modVennDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    debug_log <- function(message, level = 1) {
      if (isTRUE(debug_level >= level)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("[%s] VENN_DOC: %s", timestamp, message)
        cat(line, "\n")
        rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
        if (is.function(rec)) rec(line)
      }
    }

    current_section_venn <- reactiveVal("overview")

    lapply(names(venn_doc_user_sections), function(section_key) {
      observeEvent(input[[venn_doc_user_sections[[section_key]]$id]], {
        current_section_venn(section_key)
        debug_log(sprintf("Navigated to user section: %s", section_key), 2)
      }, ignoreNULL = TRUE)
    })

    lapply(names(venn_doc_tech_sections), function(section_key) {
      observeEvent(input[[venn_doc_tech_sections[[section_key]]$id]], {
        current_section_venn(section_key)
        debug_log(sprintf("Navigated to technical section: %s", section_key), 2)
      }, ignoreNULL = TRUE)
    })

    observeEvent(input$doc_type_venn, {
      next_section <- if (identical(input$doc_type_venn, "technical")) {
        "tech_overview"
      } else {
        "overview"
      }
      current_section_venn(next_section)
      debug_log(sprintf("Switched documentation type to: %s", input$doc_type_venn), 1)
    }, ignoreNULL = FALSE)

    output$doc_content_venn <- renderUI({
      debug_log(sprintf("Rendering documentation section: %s", current_section_venn()), 2)
      render_venn_doc_content(current_section_venn())
    })
  })
}
