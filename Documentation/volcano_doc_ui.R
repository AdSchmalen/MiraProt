# ./Documentation/volcano_doc_ui.R
# Volcano documentation module UI, shared helpers, and navigation

volcano_doc_callout <- function(title, ..., type = c("info", "success", "warning")) {
  type <- match.arg(type)
  class_name <- switch(
    type,
    info = "alert alert-info",
    success = "alert alert-success",
    warning = "alert alert-warning"
  )

  div(
    class = class_name,
    h4(title),
    ...
  )
}

volcano_doc_sections <- function() {
  list(
    user = list(
      label = "User Guide",
      default = "user_overview",
      sections = list(
        list(id = "user_overview", label = "Overview", renderer = "render_volcano_user_overview_content"),
        list(id = "user_data", label = "Data Requirements", renderer = "render_volcano_user_data_content"),
        list(id = "user_workflow", label = "Using the Interface", renderer = "render_volcano_user_workflow_content"),
        list(id = "user_interpretation", label = "Interpretation", renderer = "render_volcano_user_interpretation_content"),
        list(id = "user_customization", label = "Customization", renderer = "render_volcano_user_customization_content"),
        list(id = "user_export", label = "Save and Export", renderer = "render_volcano_user_export_content")
      )
    ),
    technical = list(
      label = "Technical Documentation",
      default = "tech_architecture",
      sections = list(
        list(id = "tech_architecture", label = "Architecture", renderer = "render_volcano_tech_architecture_content"),
        list(id = "tech_io", label = "Inputs and Outputs", renderer = "render_volcano_tech_io_content"),
        list(id = "tech_reactivity", label = "Reactive Logic", renderer = "render_volcano_tech_reactivity_content"),
        list(id = "tech_functions", label = "Key Functions", renderer = "render_volcano_tech_functions_content"),
        list(id = "tech_integration", label = "Integration Details", renderer = "render_volcano_tech_integration_content")
      )
    )
  )
}

volcano_doc_nav_ui <- function(ns, section_group) {
  div(
    class = "list-group",
    lapply(section_group$sections, function(section) {
      actionLink(
        ns(paste0("nav_", section$id)),
        section$label,
        class = "list-group-item list-group-item-action"
      )
    })
  )
}

#' Volcano Documentation Module UI
#'
#' Creates the documentation user interface for the Volcano module.
#' @param id Module namespace id.
#' @export
modVolcanoDocUI <- function(id) {
  ns <- NS(id)
  section_registry <- volcano_doc_sections()

  tagList(
    tags$head(
      tags$style(HTML("\n:root{\n  --dw-bg:#f5f7fb;\n  --dw-surface:#ffffff;\n  --dw-border:#e6e8ef;\n  --dw-text:#1f2633;\n  --dw-brand:#3c7bf1;\n  --dw-shadow:0 8px 22px rgba(31,38,51,.10);\n}\nhtml, body { background: var(--dw-bg); }\nhtml { scroll-behavior: smooth; }\n.dw-guide-wrap {\n  background: var(--dw-surface);\n  border: 1px solid var(--dw-border);\n  border-radius: 14px;\n  box-shadow: var(--dw-shadow);\n  padding: 24px;\n}\n.dw-guide-wrap .alert, .dw-guide-wrap .well{\n  border:1px solid var(--dw-border);\n  border-radius:12px;\n}\n.dw-doc-meta { color:#56607a; margin-bottom: 16px; }\n"))
    ),
    fluidRow(
      column(
        3,
        wellPanel(
          style = "background-color: #f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),
          radioButtons(
            ns("doc_type"),
            "Documentation Type:",
            choices = setNames(
              c("user", "technical"),
              c(
                section_registry$user$label,
                section_registry$technical$label
              )
            ),
            selected = "user"
          ),
          hr(),
          uiOutput(ns("doc_nav"))
        )
      ),
      column(
        9,
        div(
          class = "dw-guide-wrap",
          uiOutput(ns("doc_content"))
        )
      )
    )
  )
}

#' Volcano Documentation Module Server
#'
#' Server logic for the Volcano documentation module.
#' @param id Module namespace id.
#' @param debug_level Debug level for logging.
#' @export
modVolcanoDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    sections <- volcano_doc_sections()

    debug_log <- function(message, level = 1) {
      if (is.numeric(debug_level) && debug_level >= level) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("[%s] VOLCANO_DOC: %s", timestamp, message)
        cat(line, "\n")
        rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
        if (is.function(rec)) rec(line)
      }
    }

    current_doc_type <- reactiveVal("user")
    current_section <- reactiveVal(sections$user$default)

    observeEvent(input$doc_type, {
      req(input$doc_type)
      current_doc_type(input$doc_type)
      current_section(sections[[input$doc_type]]$default)
      debug_log(sprintf("Switched documentation type to %s", input$doc_type), 2)
    }, ignoreInit = TRUE)

    lapply(names(sections), function(doc_type) {
      lapply(sections[[doc_type]]$sections, function(section) {
        observeEvent(input[[paste0("nav_", section$id)]], {
          current_doc_type(doc_type)
          current_section(section$id)
          debug_log(sprintf("Navigated to %s", section$id), 2)
        }, ignoreInit = TRUE)
      })
    })

    output$doc_nav <- renderUI({
      volcano_doc_nav_ui(session$ns, sections[[current_doc_type()]])
    })

    output$doc_content <- renderUI({
      section_group <- sections[[current_doc_type()]]
      section_meta <- Filter(function(x) identical(x$id, current_section()), section_group$sections)

      if (length(section_meta) != 1) {
        return(div(class = "alert alert-warning", "Documentation section not available."))
      }

      renderer_name <- section_meta[[1]]$renderer
      renderer <- get0(renderer_name, ifnotfound = NULL)

      if (!is.function(renderer)) {
        debug_log(sprintf("Missing renderer: %s", renderer_name), 1)
        return(div(class = "alert alert-warning", "Documentation content is unavailable."))
      }

      renderer()
    })
  })
}
