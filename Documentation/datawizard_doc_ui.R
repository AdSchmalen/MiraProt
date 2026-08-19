# ./Documentation/datawizard_doc_ui.R
# Datawizard Documentation Module — UI and Server (Navigation Shell)
#
# This file contains the Shiny module UI and server functions that provide
# the navigation sidebar, documentation type switching, and content routing.
# Content rendering functions are sourced from companion files:
#   - datawizard_doc_user.R          (user guide sections)
#   - datawizard_doc_tech_core.R     (architecture, module loading, data flow, logging)
#   - datawizard_doc_tech_reactive.R (auto-assign, assign rules, import/export)
#   - datawizard_doc_tech_helpers.R  (batch effects, pivot, merge)
#   - datawizard_doc_data.R          (filtering, edit, imputation, ratios, basemean)

# Note: Companion content files are loaded into modEnv by app.R (loads all
# Documentation/*.R files). No explicit source() calls needed here.
# Content rendering functions are defined in:
#   - datawizard_doc_user.R
#   - datawizard_doc_tech_core.R
#   - datawizard_doc_tech_reactive.R
#   - datawizard_doc_tech_helpers.R
#   - datawizard_doc_data.R

############
# UI Function

#' Datawizard Documentation Module UI
#'
#' Creates the user interface for Datawizard documentation with navigation sidebar.
#' The sidebar provides two documentation types (User Guide and Technical Documentation)
#' and section-level navigation within each type.
#' @param id module namespace id
#' @export
modDatawizardDocUI <- function(id) {
  ns <- NS(id)

  tagList(

    tags$head(
      tags$style(HTML("
/* ===== Datawizard User Guide — Spacious Step UI (MiraProt-like) ===== */
:root{
  --dw-bg:#f7f9fc; --dw-surface:#fff; --dw-brand:#3c7bf1; --dw-brand-10:rgba(60,123,241,.10);
  --dw-border:#e6e8ef; --dw-text:#1f2633; --dw-text-soft:#56607a;
  --dw-shadow:0 6px 18px rgba(31,38,51,.08);
}
.dw-guide-wrap{background:var(--dw-bg);padding:24px;border-radius:16px;}
.dw-section-title{font-size:22px;line-height:1.25;margin:8px 0 18px;color:var(--dw-text);font-weight:700;}
.dw-steps-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;}
@media (max-width:1200px){.dw-steps-grid{grid-template-columns:repeat(2,1fr);} }
@media (max-width:700px){.dw-steps-grid{grid-template-columns:1fr;} }
.dw-step{
  background:var(--dw-surface);border:1px solid var(--dw-border);border-left:5px solid var(--dw-brand);
  border-radius:14px;box-shadow:var(--dw-shadow);padding:22px 22px 18px;
  transition:transform .2s ease, box-shadow .2s ease, border-color .2s ease;
}
.dw-step:hover{transform:translateY(-2px);box-shadow:0 10px 26px rgba(31,38,51,.12);border-left-color:#2f66cc;}
.dw-step-head{display:flex;align-items:center;gap:12px;margin-bottom:10px;}
.dw-step-num{display:inline-flex;align-items:center;justify-content:center;width:36px;height:36px;border-radius:10px;background:var(--dw-brand-10);color:var(--dw-brand);font-weight:800;font-size:16px;}
.dw-step-title{font-size:18px;font-weight:700;color:var(--dw-text);margin:0;}
.dw-step-body{color:var(--dw-text-soft);font-size:14.5px;line-height:1.55;}
.dw-step-actions{display:flex;gap:10px;margin-top:14px;}
.dw-btn{display:inline-block;padding:8px 12px;border-radius:10px;border:1px solid var(--dw-border);background:#fff;text-decoration:none;font-weight:600;color:var(--dw-text);}
.dw-btn:focus,.dw-btn:hover{border-color:var(--dw-brand);outline:none;}
.dw-side{background:#fff;border:1px solid var(--dw-border);border-radius:14px;padding:16px;box-shadow:var(--dw-shadow);}

/* Data Wizard documentation uses neutral information panels.
   Bootstrap's yellow warning style is intentionally avoided here. */
.doc-panel .alert-warning{
  background:#f5f7fa !important;
  border-color:#d8dee8 !important;
  color:var(--dw-text) !important;
}
.doc-panel .panel-warning{
  border-color:#d8dee8 !important;
}
.doc-panel .panel-warning > .panel-heading{
  background:#eef2f7 !important;
  border-color:#d8dee8 !important;
  color:var(--dw-text) !important;
}

html{scroll-behavior:smooth;}
"))
    ),
    fluidRow(
      # Left sidebar for navigation (3 columns)
      column(3,
             wellPanel(
               style = "background-color: #f8f9fa; position: sticky; top: 20px;",
               h4("Navigation", style = "margin-bottom: 20px;"),

               # Documentation type selector
               radioButtons(ns("doc_type"),
                            "Documentation Type:",
                            choices = c("User Guide" = "user",
                                        "Technical Documentation" = "technical"),
                            selected = "user"),

               hr(),

               # Dynamic navigation based on selected type
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'user'", ns("doc_type")),
                 h5("User Guide Sections"),
                 div(
                   class = "list-group",
                   actionLink(ns("nav_overview"), "Overview",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_file_loader"), "1. File Loader",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_data_tables"), "2. Data Tables",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_assign_rules"), "3. Conditions & Metadata",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_auto_assign"), "4. Auto-Assign Assistant",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_filtering"), "5. Data Filtering",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_editing"), "6. Data Editing",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_imputation"), "7. Missing Data Handling",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_ratios"), "8. Custom Ratios & Statistics",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_batch"), "9. Batch Effects",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_pivot"), "10. Pivot Operations",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_merge"), "11. Merge Operations",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_basemean"), "12. Basemean Operations",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_annotation"), "13. ID Annotation",
                              class = "list-group-item list-group-item-action")
                 )
               ),

               conditionalPanel(
                 condition = sprintf("input['%s'] == 'technical'", ns("doc_type")),
                 h5("Technical Sections"),
                 div(
                   class = "list-group",
                   actionLink(ns("nav_tech_overview"), "Architecture Overview",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_module_loading"), "Module Loading & Wiring",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_auto_assign"), "Auto-Assign System",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_assign_rules"), "Assign Rules System",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_import_export"), "Import/Export System (RDS)",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_data_flow"), "Data Flow & State Management",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_logging"), "Logging & Debugging",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_batch_effects"), "Batch Effect Correction",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_pivot"), "Pivot Data",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_merge_module"), "Merge Primary/Additional Data",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_filtering_module"), "Filtering",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_edit_module"), "Data Editing",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_imputation_module"), "Missing Data Handling",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_ratios_module"), "Ratios & Statistics",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_basemean_module"), "Basemean Calculation",
                              class = "list-group-item list-group-item-action"),
                   actionLink(ns("nav_tech_annotation_module"), "Annotation / ID Mapping",
                              class = "list-group-item list-group-item-action")
                 )
               )
             )
      ),

      # Main content area (9 columns)
      column(9,
             wellPanel(class = "doc-panel",
                       uiOutput(ns("doc_content"))
             )
      )
    )
  )
}

############
# Server Function

#' Datawizard Documentation Module Server
#'
#' Server logic for displaying documentation content based on navigation.
#' Routes section selections to the appropriate content rendering function
#' defined in companion files.
#' @param id module namespace id
#' @param debug_level Debug level for logging (1=debug, 2=verbose)
#' @export
modDatawizardDocServer <- function(id, debug_level = 2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debug logging function (following project standards)
    debug_log <- function(message, level = 1) {
      if (debug_level >= level) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("[%s] DATAWIZARD_DOC: %s", timestamp, message)
        cat(line, "\n")
        rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
        if (is.function(rec)) rec(line)
      }
    }

    debug_log("Datawizard documentation module initialized", 1)

    # Reactive value to track current section
    current_section <- reactiveVal("overview")

    # ========================================
    # Navigation Observers — User Guide
    # ========================================
    observeEvent(input$nav_overview,    { current_section("overview") })
    observeEvent(input$nav_file_loader, { current_section("file_loader") })
    observeEvent(input$nav_data_tables, { current_section("data_tables") })
    observeEvent(input$nav_assign_rules,{ current_section("assign_rules") })
    observeEvent(input$nav_auto_assign, { current_section("auto_assign") })
    observeEvent(input$nav_filtering,   { current_section("filtering") })
    observeEvent(input$nav_editing,     { current_section("editing") })
    observeEvent(input$nav_imputation,  { current_section("imputation") })
    observeEvent(input$nav_ratios,      { current_section("ratios") })
    observeEvent(input$nav_batch,       { current_section("batch") })
    observeEvent(input$nav_pivot,       { current_section("pivot") })
    observeEvent(input$nav_merge,       { current_section("merge") })
    observeEvent(input$nav_basemean,    { current_section("basemean") })
    observeEvent(input$nav_annotation, { current_section("annotation") })

    # ========================================
    # Navigation Observers — Technical Documentation
    # ========================================
    observeEvent(input$nav_tech_overview,          { current_section("tech_overview") })
    observeEvent(input$nav_tech_module_loading,    { current_section("tech_module_loading") })
    observeEvent(input$nav_tech_auto_assign,       { current_section("tech_auto_assign") })
    observeEvent(input$nav_tech_assign_rules,      { current_section("tech_assign_rules") })
    observeEvent(input$nav_tech_import_export,     { current_section("tech_import_export") })
    observeEvent(input$nav_tech_data_flow,         { current_section("tech_data_flow") })
    observeEvent(input$nav_tech_logging,           { current_section("tech_logging") })
    observeEvent(input$nav_tech_batch_effects,     { current_section("tech_batch_effects") })
    observeEvent(input$nav_tech_pivot,             { current_section("tech_pivot") })
    observeEvent(input$nav_tech_merge_module,      { current_section("tech_merge_module") })
    observeEvent(input$nav_tech_filtering_module,  { current_section("tech_filtering_module") })
    observeEvent(input$nav_tech_edit_module,       { current_section("tech_edit_module") })
    observeEvent(input$nav_tech_imputation_module, { current_section("tech_imputation_module") })
    observeEvent(input$nav_tech_ratios_module,     { current_section("tech_ratios_module") })
    observeEvent(input$nav_tech_basemean_module,   { current_section("tech_basemean_module") })
    observeEvent(input$nav_tech_annotation_module, { current_section("tech_annotation_module") })

    # ========================================
    # Documentation Type Switching
    # ========================================
    observeEvent(input$doc_type, {
      if (input$doc_type == "user") {
        current_section("overview")
        debug_log("Switched to User Guide documentation", 1)
      } else {
        current_section("tech_overview")
        debug_log("Switched to Technical documentation", 1)
      }
    })

    # ========================================
    # Content Routing
    # ========================================

    # Dispatch table mapping section IDs to rendering functions.
    # User Guide functions are defined in datawizard_doc_user.R.
    # Technical functions are split across datawizard_doc_tech_core.R,
    # datawizard_doc_tech_reactive.R, datawizard_doc_tech_helpers.R,
    # and datawizard_doc_data.R.
    section_renderers <- list(
      # User Guide
      overview       = render_overview_content_dw,
      file_loader    = render_file_loader_content,
      data_tables    = render_data_tables_content,
      assign_rules   = render_assign_rules_content,
      auto_assign    = render_auto_assign_content,
      filtering      = render_filtering_content_dw,
      editing        = render_editing_content,
      imputation     = render_imputation_content,
      ratios         = render_ratios_content,
      batch          = render_batch_content,
      pivot          = render_pivot_content,
      merge          = render_merge_content,
      basemean       = render_basemean_content,
      annotation     = render_annotation_content,
      # Technical Documentation
      tech_overview          = render_dwdocs_tech_overview_content,
      tech_module_loading    = render_tech_module_loading_content,
      tech_auto_assign       = render_tech_auto_assign_content,
      tech_assign_rules      = render_tech_assign_rules_content,
      tech_import_export     = render_tech_import_export_content,
      tech_data_flow         = render_tech_data_flow_content,
      tech_logging           = render_tech_logging_content,
      tech_batch_effects     = render_tech_batch_effects_content,
      tech_pivot             = render_tech_pivot_content,
      tech_merge_module      = render_tech_merge_content,
      tech_filtering_module  = render_tech_filtering_content,
      tech_edit_module       = render_tech_edit_content,
      tech_imputation_module = render_tech_imputation_content,
      tech_ratios_module     = render_tech_ratios_content,
      tech_basemean_module   = render_tech_basemean_content,
      tech_annotation_module = render_tech_annotation_content
    )

    output$doc_content <- renderUI({
      section <- current_section()
      debug_log(sprintf("Rendering content for section: %s", section), 2)

      renderer <- section_renderers[[section]]
      if (!is.null(renderer)) {
        return(renderer())
      }

      # Default fallback
      return(div(
        class = "alert alert-info",
        "Please select a section from the navigation menu."
      ))
    })
  })
}
