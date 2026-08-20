# ==============================================================================
# File: Documentation/MiraProt_doc_ui.R
#
# Purpose:
#   Defines the MiraProt documentation module UI and server routing.
#   This file contains only layout, navigation, and section dispatch.
#   Content renderers are implemented in:
#     - Documentation/MiraProt_doc_user.R
#     - Documentation/MiraProt_doc_tech.R
#   Maintainer ownership contract:
#     - User-facing content belongs in Documentation/MiraProt_doc_user.R.
#     - Developer-facing content belongs in Documentation/MiraProt_doc_tech.R.
#     - Documentation/MiraProt_doc_ui.R remains routing-only (no audience prose).
#
# Synchronization rule:
#   Every navigation item must map to exactly one concrete renderer function with
#   non-empty, audience-specific content (user or technical).
#
# Public API:
#   - modMiraProtDocUI(id)
#   - modMiraProtDocServer(id, debug_level = 1)
# ==============================================================================

#' MiraProt Documentation Module UI
#'
#' @param id Module namespace identifier.
#' @export
modMiraProtDocUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
:root{
  --sp-bg:#f5f7fb;
  --sp-surface:#ffffff;
  --sp-border:#e6e8ef;
  --sp-text:#1f2633;
  --sp-text-soft:#56607a;
  --sp-brand:#3c7bf1;
  --sp-shadow:0 8px 22px rgba(31,38,51,.10);
}
.sp-doc-wrap {
  background: var(--sp-surface);
  border: 1px solid var(--sp-border);
  border-radius: 14px;
  box-shadow: var(--sp-shadow);
  padding: 24px;
}
.sp-doc-wrap .alert{
  border:1px solid var(--sp-border);
  border-radius:12px;
}
.sp-code-panel{
  background:#f3f4f6;
  border:1px solid #d9dde5;
  border-radius:8px;
  padding:12px 14px;
  margin:10px 0 14px 0;
}
.sp-code-panel pre{
  margin:0;
  background:transparent;
  border:0;
  padding:0;
  color:#1f2633;
  font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;
  white-space: pre-wrap;
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
            ns("doc_type"),
            "Documentation Type:",
            choices = c(
              "User Guide" = "user",
              "Technical Documentation" = "technical"
            ),
            selected = "user"
          ),
          hr(),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_user_overview"), "Overview and workflow", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_user_local"), "Run with local R", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_user_portable"), "Use portable edition", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_user_session"), "Session tab guide", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_user_compile"), "Build your own distributions", class = "list-group-item list-group-item-action")
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_architecture"), "Architecture and modules", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_reactive"), "Reactive data flow", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_local"), "Local startup workflow", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_portable"), "Portable build and runtime", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_session"), "Session save/restore internals", class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "sp-doc-wrap", uiOutput(ns("doc_content")))
      )
    )
  )
}

#' MiraProt Documentation Module Server
#'
#' @param id Module namespace identifier.
#' @param debug_level Logging verbosity (1 = standard, 2 = verbose).
#' @export
modMiraProtDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    debug_log_local <- function(message, level = 1) {
      if (debug_level >= level) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        cat(sprintf("[%s] MIRAPROT_DOC: %s\n", timestamp, message))
      }
    }

    debug_log_local("MiraProt documentation module initialized", 1)
    current_section <- reactiveVal("user_overview")

    observeEvent(input$nav_user_overview, { current_section("user_overview") })
    observeEvent(input$nav_user_local, { current_section("user_local") })
    observeEvent(input$nav_user_portable, { current_section("user_portable") })
    observeEvent(input$nav_user_session, { current_section("user_session") })
    observeEvent(input$nav_user_compile, { current_section("user_compile") })

    observeEvent(input$nav_tech_architecture, { current_section("tech_architecture") })
    observeEvent(input$nav_tech_reactive, { current_section("tech_reactive") })
    observeEvent(input$nav_tech_local, { current_section("tech_local") })
    observeEvent(input$nav_tech_portable, { current_section("tech_portable") })
    observeEvent(input$nav_tech_session, { current_section("tech_session") })

    observeEvent(input$doc_type, {
      if (identical(input$doc_type, "user")) {
        current_section("user_overview")
        debug_log_local("Switched to user guide", 1)
      } else {
        current_section("tech_architecture")
        debug_log_local("Switched to technical documentation", 1)
      }
    })

    output$doc_content <- renderUI({
      section <- current_section()
      debug_log_local(sprintf("Rendering section: %s", section), 2)

      switch(
        section,
        user_overview = render_user_miraprot_overview_content(),
        user_local = render_user_miraprot_local_content(),
        user_portable = render_user_miraprot_portable_content(),
        user_session = render_user_miraprot_session_content(),
        user_compile = render_user_miraprot_build_content(),
        tech_architecture = render_tech_miraprot_architecture_content(),
        tech_reactive = render_tech_miraprot_reactive_content(),
        tech_local = render_tech_miraprot_local_content(),
        tech_portable = render_tech_miraprot_portable_content(),
        tech_session = render_tech_miraprot_session_content(),
        div(class = "alert alert-info", "Please select a section from the navigation menu.")
      )
    })
  })
}
