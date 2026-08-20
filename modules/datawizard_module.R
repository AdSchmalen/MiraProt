# ============================================================================
# Module/Sub-script: modules/datawizard_module.R
# Purpose:
#   Provide the Data Wizard Shiny module entry points (UI and server) and orchestrate
#   the lifecycle of in-scope coordination, integration, export, and utility layers.
#
# Architectural Role:
#   orchestration
#
# Responsibilities:
#   - Compose Data Wizard UI sections and bind server entry points.
#   - Wire core state, integration bridges, export endpoints, and shared observers.
#   - Expose a backward-compatible module API to the rest of the application.
#
# Non-Responsibilities (Must NOT be here):
#   - Implement business logic of out-of-scope processing submodules.
#   - Redesign or alter black-box module contracts sourced from Data Wizard sub-scripts.
#
# Allowed Dependencies:
#   - In-scope scripts: datawizard_core.R, datawizard_integration.R, datawizard_export.R,
#     datawizard_utils.R.
#   - Out-of-scope Data Wizard modules only via stable, existing interfaces.
#   - Shiny ecosystem and existing project utility dependencies.
#
# Interaction Boundaries:
#   - Inputs:
#     Shiny input/session context, shared reactiveValues `rv`, and module outputs from
#     sourced Data Wizard submodules.
#   - Outputs:
#     Module UI/server endpoints and a backward-compatible list of reactive helper
#     functions used by downstream application components.
#   - Out-of-Scope Integrations:
#     datawizard_file_loader, batch_effects, pivot, merge, tables, filtering,
#     auto_assign, assign_rules, imputation, edit, ratios, basemean (black boxes).
#
# Stability Guarantees:
#   - Preserve public module behavior and returned server API shape.
#   - Preserve integration behavior with out-of-scope modules.
#   - Keep reactive orchestration robust with defensive error handling and fallbacks.
# ============================================================================
# modules/datawizard_module.R
# Main Data Wizard Module - Refactored Parent Coordinator

# Source all required submodules
source("modules/Data Wizard/datawizard_provenance.R", local = modEnv)
source("modules/Data Wizard/datawizard_file_loader.R", local = modEnv)
source("modules/Data Wizard/datawizard_batch_effects.R", local = modEnv)
source("modules/Data Wizard/datawizard_pivot.R", local = modEnv)
source("modules/Data Wizard/datawizard_merge.R", local = modEnv)
source("modules/Data Wizard/datawizard_tables.R", local = modEnv)
source("modules/Data Wizard/datawizard_filtering.R", local = modEnv)
source("modules/Data Wizard/datawizard_auto_regex.R", local = modEnv)
source("modules/Data Wizard/datawizard_auto_assign.R", local = modEnv)
source("modules/Data Wizard/datawizard_assign_rules.R", local = modEnv)
source("modules/Data Wizard/datawizard_imputation.R", local = modEnv)
source("modules/Data Wizard/datawizard_edit.R", local = modEnv)
source("modules/Data Wizard/datawizard_ratios.R", local = modEnv)
source("modules/Data Wizard/datawizard_basemean.R", local = modEnv)
source("modules/Data Wizard/datawizard_annotation.R", local = modEnv)
source("modules/Data Wizard/datawizard_module_ui.R", local = modEnv)

# Source refactored core functionality
source("modules/Data Wizard/datawizard_core.R", local = TRUE)
source("modules/Data Wizard/datawizard_integration.R", local = TRUE)
source("modules/Data Wizard/datawizard_export.R", local = TRUE)
source("modules/Data Wizard/datawizard_utils.R", local = TRUE)

############
# Server - Refactored with Modular Architecture
modDataWizardServer <- function(id, rv, debug_level = 0) {
  DEBUG_LEVEL <- suppressWarnings(as.integer(debug_level))[1]
  if (length(DEBUG_LEVEL) == 0 || !is.finite(DEBUG_LEVEL)) DEBUG_LEVEL <- 0
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # UI Toggle Event Handlers
    # ========================================
    # Keep every server registered for API/restore compatibility, but hydrate
    # optional tool choices and previews only after its panel is first opened.
    advanced_panel_initialized <- register_datawizard_ui_toggle_handlers(input)

    observeEvent(input$open_data_loading_guide, {
      showModal(modalDialog(
        title = div(
          style = "display:flex; align-items:center; gap:8px;",
          icon("compass"),
          span("Guide to Preparing Your Data")
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(modalButton("Close")),
        div(
          style = "max-height: 75vh; overflow-y: auto; padding-right: 8px; font-size: 14px; line-height: 1.5;",
          tags$style(HTML("
            .dw-guide-card {border: 1px solid #d7e3f4; border-radius: 8px; padding: 12px; margin-bottom: 14px; background: #fcfdff;}
            .dw-guide-title {font-size: 16px; font-weight: 700; color: #1f4e8c; margin-bottom: 6px;}
            .dw-guide-sub {font-size: 13px; color: #2f3b4a; margin-bottom: 8px;}
            .dw-guide-note {font-size: 12px; color: #3f4d5d;}
            .dw-guide-table th {background: #eaf2fd; color: #1f4e8c; font-weight: 700; text-align: center;}
            .dw-guide-table td, .dw-guide-table th {border: 1px solid #c9d9ee !important; padding: 5px 8px !important; font-size: 12px;}
            .dw-row-1 {background:#f3f8ff;} .dw-row-2 {background:#e8f2ff;} .dw-row-3 {background:#deebff;} .dw-row-4 {background:#d3e5ff;} .dw-row-5 {background:#c8deff;}
            .dw-badge-bad {background:#ffe9e9; color:#9d1f1f; border:1px solid #f1b7b7; border-radius:12px; padding:2px 8px; font-size:11px; font-weight:700;}
            .dw-badge-good {background:#e8f8ee; color:#196b35; border:1px solid #b4e1c2; border-radius:12px; padding:2px 8px; font-size:11px; font-weight:700;}
            .dw-pipeline {font-size: 15px; font-weight: 700; color: #1f4e8c; text-align:center; margin: 6px 0;}
          ")),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "1) Core rule: one protein per row"),
              div(class = "dw-guide-sub", "MiraProt requires every row to be a unique protein entry. Keep each protein on its own line."),
              tags$table(class = "table table-condensed dw-guide-table",
                         tags$thead(tags$tr(tags$th("ID"), tags$th("Measure A"), tags$th("Measure B"))),
                         tags$tbody(
                           tags$tr(class="dw-row-1", tags$td("Protein 1"), tags$td("12.4"), tags$td("0.67")),
                           tags$tr(class="dw-row-2", tags$td("Protein 2"), tags$td("9.8"), tags$td("0.43")),
                           tags$tr(class="dw-row-3", tags$td("Protein 3"), tags$td("15.1"), tags$td("0.88")),
                           tags$tr(class="dw-row-4", tags$td("Protein 4"), tags$td("7.6"), tags$td("0.31")),
                           tags$tr(class="dw-row-5", tags$td("Protein 5"), tags$td("11.2"), tags$td("0.72"))
                         )
              ),
              div(class = "dw-guide-note", "Tip: if your data table looks different, MiraProt offers a broad range of tools to convert it into the correct format. Just follow this guide.")
          ),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "2) If proteins are in columns instead of rows"),
              div(class = "dw-guide-sub", "Problem: proteins are spread across columns. Use the Pivot transpose function to flip the table before continuing."),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Click Advanced Processing"), " (right panel), then open the ", tags$strong("Pivot"), " tab."),
                tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
                tags$li("Set ", tags$strong("Pivot Operation"), " = Transpose."),
                tags$li("Click ", tags$strong("Apply Pivot"), ". The row/column orientation is now switched."),
                tags$li("If needed, run a second pivot step after transposing to place ", tags$strong("one Protein ID per row"), ".")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Before (proteins in columns)"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Sample"), tags$th("Protein 1"), tags$th("Protein 2"), tags$th("Protein 3"))),
                                  tags$tbody(tags$tr(tags$td("S1"), tags$td("8.1"), tags$td("7.2"), tags$td("6.8")))
                       )
                ),
                column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Transpose")),
                column(5, span(class="dw-badge-good", "After transpose"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Sample"), tags$th("S1"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("Protein 1"), tags$td("8.1")),
                                    tags$tr(class="dw-row-3", tags$td("Protein 2"), tags$td("7.2")),
                                    tags$tr(class="dw-row-4", tags$td("Protein 3"), tags$td("6.8"))
                                  )
                       )
                )
              )
          ),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "3) If your data is split across files"),
              div(class = "dw-guide-sub", "Use Merge to attach columns from a secondary table to your primary table using a shared ID column."),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Open Advanced Processing"), " and select the ", tags$strong("Merge"), " tab."),
                tags$li("Set ", tags$strong("Primary Data Join Column"), " to your ID in the main table (for example: Protein ID)."),
                tags$li("Set ", tags$strong("Secondary Data Join Column"), " to the matching ID column in the second table."),
                tags$li("Choose ", tags$strong("Additional Columns from Secondary Data"), " (for example: Gene Name, Pathway, p-value)."),
                tags$li("Set ", tags$strong("Join Type"), " = Left Join (recommended) to keep all primary proteins."),
                tags$li("Check ", tags$strong("Merge Preview"), " and click ", tags$strong("Apply Merge"), ".")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Primary Data"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P001"), tags$td("12.4")),
                                    tags$tr(tags$td("P002"), tags$td("9.8"))
                                  )
                       )
                ),
                column(2, div(class="dw-pipeline", style="margin-top: 55px;", "+")),
                column(5, span(class="dw-badge-bad", "Secondary Data"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Gene"), tags$th("Pathway"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P001"), tags$td("TP53"), tags$td("DNA repair")),
                                    tags$tr(tags$td("P002"), tags$td("EGFR"), tags$td("Signaling"))
                                  )
                       )
                )
              ),
              fluidRow(
                column(12, div(class="dw-pipeline", "⟱ Merge (Left Join) ⟱"))
              ),
              fluidRow(
                column(3),
                column(6, span(class="dw-badge-good", "After Merge"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"), tags$th("Gene"), tags$th("Pathway"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P001"), tags$td("12.4"), tags$td("TP53"), tags$td("DNA repair")),
                                    tags$tr(class="dw-row-3", tags$td("P002"), tags$td("9.8"), tags$td("EGFR"), tags$td("Signaling"))
                                  )
                       )
                ),
                column(3)
              ),
              div(class = "dw-guide-note", "Tip: if row count drops unexpectedly, check that both join columns use the same ID format.")
          ),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "4) If proteins appear multiple times (conditions/contrasts)"),
              div(class = "dw-guide-sub", "If one Protein ID repeats by Condition/Contrast, use Pivot to convert repeated rows into one row per protein."),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Open Advanced Processing"), " and switch to the ", tags$strong("Pivot"), " tab."),
                tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
                tags$li("Set ", tags$strong("Pivot Operation"), " = Wider."),
                tags$li("Set ", tags$strong("Names from"), " = Contrast (this creates one column per contrast)."),
                tags$li("Set ", tags$strong("Values from"), " = Value."),
                tags$li("Set ", tags$strong("ID columns"), " = ID (keeps one row per protein)."),
                tags$li("Click ", tags$strong("Apply Pivot"), " and verify the result in the preview/table.")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Before Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("ID"), tags$th("Contrast"), tags$th("Value"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P1"), tags$td("A_vs_B"), tags$td("1.3")),
                                    tags$tr(tags$td("P1"), tags$td("A_vs_C"), tags$td("0.8")),
                                    tags$tr(tags$td("P2"), tags$td("A_vs_B"), tags$td("1.1"))
                                  )
                       )
                ),
                column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Wider)")),
                column(5, span(class="dw-badge-good", "After Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P1"), tags$td("1.3"), tags$td("0.8")),
                                    tags$tr(class="dw-row-3", tags$td("P2"), tags$td("1.1"), tags$td("NA"))
                                  )
                       )
                )
              )
          ),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "5) Reshape data tables for third party analysis tools"),
              div(class = "dw-guide-sub", "Your table can start with one protein per row, while condition measurements are spread across multiple columns (for example Intensity_Control and Intensity_Treated). Pivot Longer reshapes this if needed."),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Open Advanced Processing"), " and go to the ", tags$strong("Pivot"), " tab."),
                tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
                tags$li("Set ", tags$strong("Pivot Operation"), " = Longer."),
                tags$li("Set ", tags$strong("Columns to pivot"), " = Intensity_Control, Intensity_Treated."),
                tags$li("Set ", tags$strong("Names to"), " = Condition."),
                tags$li("Set ", tags$strong("Values to"), " = Intensity."),
                tags$li("Click ", tags$strong("Apply Pivot"), " to create one value column with a condition label column.")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Before Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Intensity_Control"), tags$th("Intensity_Treated"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P001"), tags$td("10.2"), tags$td("14.1")),
                                    tags$tr(tags$td("P002"), tags$td("8.7"), tags$td("9.4"))
                                  )
                       )
                ),
                column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Longer)")),
                column(5, span(class="dw-badge-good", "After Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Condition"), tags$th("Intensity"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P001"), tags$td("Intensity_Control"), tags$td("10.2")),
                                    tags$tr(class="dw-row-3", tags$td("P001"), tags$td("Intensity_Treated"), tags$td("14.1")),
                                    tags$tr(class="dw-row-4", tags$td("P002"), tags$td("Intensity_Control"), tags$td("8.7"))
                                  )
                       )
                )
              ),
              div(class = "dw-guide-note", "Tip: Pivot Longer is usually a temporary preparation step (i.e. for other proteomic tools). If you need one protein per row again for downstream modules, pivot back to wider afterward or reset your data.")
          ),

          div(class = "dw-guide-card",
              div(class = "dw-guide-title", "6) Combine as needed"),
              div(class = "dw-guide-sub", "For complex data (for example Spectronaut-style data exports), combine Pivot and Merge."),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Open Advanced Processing"), " and switch to the ", tags$strong("Pivot"), " tab."),
                tags$li("Set ", tags$strong("Select Data to Pivot"), " = Secondary Data."),
                tags$li("Set ", tags$strong("Pivot Operation"), " = Wider."),
                tags$li("Set ", tags$strong("Names from"), " = Contrast (creates one column per contrast)."),
                tags$li("Set ", tags$strong("Values from"), " = Value (or Ratio)."),
                tags$li("Set ", tags$strong("ID columns"), " = Protein ID."),
                tags$li("Click ", tags$strong("Apply Pivot"), " and verify one row per protein in Secondary Data.")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Secondary Data Before Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Contrast"), tags$th("Value"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P001"), tags$td("A_vs_B"), tags$td("1.30")),
                                    tags$tr(tags$td("P001"), tags$td("A_vs_C"), tags$td("0.82")),
                                    tags$tr(tags$td("P002"), tags$td("A_vs_B"), tags$td("1.12"))
                                  )
                       )
                ),
                column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Wider)")),
                column(5, span(class="dw-badge-good", "Secondary Data After Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P001"), tags$td("1.30"), tags$td("0.82")),
                                    tags$tr(class="dw-row-3", tags$td("P002"), tags$td("1.12"), tags$td("NA"))
                                  )
                       )
                )
              ),
              tags$hr(style = "margin: 12px 0;"),
              tags$ul(
                style = "margin-bottom: 10px; padding-left: 20px;",
                tags$li(tags$strong("Open Advanced Processing"), " and select the ", tags$strong("Merge"), " tab."),
                tags$li("Set ", tags$strong("Primary Data Join Column"), " = Protein ID in the primary table."),
                tags$li("Set ", tags$strong("Secondary Data Join Column"), " = Protein ID in ", tags$strong("Secondary Data After Pivot"), "."),
                tags$li("Choose ", tags$strong("Additional Columns from Secondary Data"), " = Value_A_vs_B, Value_A_vs_C."),
                tags$li("Set ", tags$strong("Join Type"), " = Left Join (recommended) to keep all primary proteins."),
                tags$li("Check ", tags$strong("Merge Preview"), " and click ", tags$strong("Apply Merge"), ".")
              ),
              fluidRow(
                column(5, span(class="dw-badge-bad", "Primary Data"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"))),
                                  tags$tbody(
                                    tags$tr(tags$td("P001"), tags$td("12.4")),
                                    tags$tr(tags$td("P002"), tags$td("9.8"))
                                  )
                       )
                ),
                column(2, div(class="dw-pipeline", style="margin-top: 40px;", "+")),
                column(5, span(class="dw-badge-good", "Secondary Data After Pivot"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P001"), tags$td("1.30"), tags$td("0.82")),
                                    tags$tr(class="dw-row-3", tags$td("P002"), tags$td("1.12"), tags$td("NA"))
                                  )
                       )
                )
              ),
              fluidRow(
                column(12, div(class="dw-pipeline", "⟱ Merge (Left Join) ⟱"))
              ),
              fluidRow(
                column(2),
                column(8, span(class="dw-badge-good", "After Merge"),
                       tags$table(class="table table-condensed dw-guide-table",
                                  tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                                  tags$tbody(
                                    tags$tr(class="dw-row-2", tags$td("P001"), tags$td("12.4"), tags$td("1.30"), tags$td("0.82")),
                                    tags$tr(class="dw-row-3", tags$td("P002"), tags$td("9.8"), tags$td("1.12"), tags$td("NA"))
                                  )
                       )
                ),
                column(2)
              ),
              div(class = "dw-guide-note", "Tip: keep Protein ID names consistent across Primary Data and Secondary Data before Merge.")
          )
        )
      ))
    })


    # ========================================
    # Initialize Core Components
    # ========================================

    # Create core reactive values
    core_values <- create_core_reactive_values()
    ui_config_values <- create_ui_config_reactive_values()

    primary_data_state <- create_primary_data_state_adapter(
      rv = rv,
      core_values = core_values,
      debug_log_fn = debug_log
    )

    # Create helper functions
    data_access_functions <- create_data_access_functions(core_values)
    modification_functions <- create_modification_tracking_functions(core_values)
    metadata_functions <- create_metadata_update_functions(core_values)
    ui_config_functions <- create_ui_config_management_functions(ui_config_values, core_values)

    # Create metadata content readiness
    metadata_content_ready <- create_metadata_content_status(core_values)

    observeEvent(core_values$handson_metadata(), {
      metadata <- core_values$handson_metadata()
      meaningful_ready <- isTRUE(metadata_content_ready())
      assigning <- tryCatch(isTRUE(isolate(core_values$metadata_assignment_pending())),
                            error = function(e) FALSE)
      lifecycle_state <- if (meaningful_ready) {
        "metadata_ready"
      } else if (assigning) {
        "metadata_assigning"
      } else if (is.data.frame(metadata) && nrow(metadata) > 0) {
        "metadata_placeholder"
      } else {
        "raw_loaded"
      }
      published_meaningful_ready <- meaningful_ready && !assigning
      published_lifecycle_state <- if (assigning && meaningful_ready) "metadata_assigning" else lifecycle_state
      if (is.function(core_values$metadata_meaningful_ready)) {
        core_values$metadata_meaningful_ready(published_meaningful_ready)
      }
      if (is.function(core_values$metadata_lifecycle_state)) {
        core_values$metadata_lifecycle_state(published_lifecycle_state)
      }
      rv$datawizard_metadata_meaningful_ready <- published_meaningful_ready
      rv$datawizard_metadata_lifecycle_state <- published_lifecycle_state
      rv$datawizard_metadata_assignment_pending <- assigning

      if (!is.function(core_values$metadata_content_signature)) return()
      signature <- create_metadata_content_signature_dw(metadata)
      old_signature <- tryCatch(isolate(core_values$metadata_content_signature()),
                                error = function(e) "")
      if (!identical(old_signature, signature)) {
        core_values$metadata_content_signature(signature)
        debug_log("Metadata content signature updated", level = 2)
      }

      if (is.function(core_values$identifier_choices)) {
        choices <- create_datawizard_identifier_choices(metadata)
        old_choices <- tryCatch(isolate(core_values$identifier_choices()),
                                error = function(e) stats::setNames(character(0), character(0)))
        if (!identical(old_choices, choices)) {
          core_values$identifier_choices(choices)
          rv$datawizard_identifier_choices <- choices
          option_choices <- names(choices)
          option_choices <- option_choices[nzchar(option_choices)]
          names(option_choices) <- option_choices
          rv$datawizard_identifier_option_choices <- option_choices
          debug_log(paste("Central identifier choices updated:", length(choices)), level = 2)
        }
      }
    }, ignoreInit = FALSE)

    # Initialize file loader module
    loader_out <- modFileLoaderServer(
      "loader",
      rv,
      core_values = core_values,  # HINZUFÜGEN
      debug_level = DEBUG_LEVEL
    )

    # Initialize all submodules
    metadata_rule_application_in_progress <- reactiveVal(FALSE)
    metadata_rule_progress_context <- new.env(parent = emptyenv())
    metadata_rule_progress_context$callback <- function(stage) invisible(NULL)

    # Own the progress lifetime here, where all metadata-rule stages are known.
    # setProgress(detail=...) replaces the active detail instead of retaining a
    # list of already completed stages. withProgress guarantees close on every
    # return and error path.
    with_metadata_rule_progress <- function(show, operation) {
      if (!isTRUE(show)) return(operation(function(stage) invisible(NULL)))
      shiny::withProgress(message = "Applying metadata rules", value = 0, {
        stage_number <- 0L
        update_stage <- function(stage) {
          stage_number <<- stage_number + 1L
          shiny::setProgress(value = min(0.95, stage_number / 10), detail = stage)
          invisible(NULL)
        }
        operation(update_stage)
      })
    }

    metadata_rule_progress <- function(stage) {
      metadata_rule_progress_context$callback(stage)
    }

    modules_list <- initialize_submodules(
      session, loader_out, core_values, ui_config_values,
      data_access_functions, modification_functions,
      metadata_functions, ui_config_functions,
      metadata_content_ready,
      rv,
      advanced_panel_initialized,
      metadata_rule_progress = metadata_rule_progress
    )

    # Expose the file loader under `loader_out` so the session-restore
    # dispatcher (see .dw_session_submodule_keys) can find its
    # get_session_state / set_session_state hooks alongside the other
    # submodules.
    modules_list$loader_out <- loader_out

    # Create module wrapper functions
    # wrapper_functions <- create_module_wrapper_functions(modules_list, data_access_functions)

    # # Initialize apply module with wrappers
    # apply_out <- initialize_module_safely("Apply", function() {
    #   modApplyServer(
    #     "apply",
    #     primary_data = reactive({ data_access_functions$get_file_data() }),
    #     metadata_final = reactive({ core_values$handson_metadata() }),
    #     filtering_functions = modules_list$filtering_out,
    #     imputation_functions = modules_list$imputation_out,
    #     ratio_configurations = reactive({
    #       if (!is.null(modules_list$ratios_out$ratio_configurations)) {
    #         safe_module_call(modules_list$ratios_out$ratio_configurations, default_return = data.frame(),
    #                          context = "apply_ratio_configs")
    #       } else {
    #         data.frame()
    #       }
    #     }),
    #     ratios_functions = wrapper_functions$ratios_functions,
    #     treatment_functions = modules_list$assign_rules_out,
    #     batch_functions = wrapper_functions$batch_functions,
    #     edit_functions = wrapper_functions$edit_functions,
    #     debug_level = DEBUG_LEVEL
    #   )
    # })

    # Create export and status functions
    export_bundle <- create_datawizard_export_bundle(
      loader_out = loader_out,
      core_values = core_values,
      modification_functions = modification_functions,
      modules_list = modules_list,
      ui_config_values = ui_config_values
    )
    excel_functions <- export_bundle$excel_functions
    config_export_functions <- export_bundle$config_export_functions
    status_functions <- export_bundle$status_functions

    # ========================================
    # Data Flow Management
    # ========================================

    # Simple state tracking for rule loading during data loading
    rule_loading_blocked <- reactiveVal(FALSE)
    data_loading_timestamp <- reactiveVal(NULL)

    register_core_data_lifecycle_observers(
      loader_out = loader_out,
      core_values = core_values,
      rv = rv,
      data_loading_timestamp = data_loading_timestamp
    )

    # During session restore we must restore rule tables/UI without
    # re-applying rules onto already-restored metadata. The restore pipeline
    # is therefore:
    #   1) data loader state
    #   2) regex rule tables (from selected rule file)
    #   3) metadata snapshot (re-asserted by session restore layer)
    # This flag marks the next rule-load event as restore-driven so metadata
    # overwrite is skipped.
    restore_rule_load_pending <- reactiveVal(FALSE)
    if (!is.null(rv)) {
      observeEvent(rv$session_restore_trigger, {
        if (!is.null(rv$session_restore_trigger)) {
          restore_rule_load_pending(TRUE)
          debug_log("Session restore detected: next rule load will skip metadata overwrite", level = 2)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }

    # Observer 4: Single rule application point (simplified)
    observeEvent(modules_list$assign_rules_out$selected_rule_file(), {
      selected_file <- validate_reactive_value(modules_list$assign_rules_out$selected_rule_file, "selected_rule_file")

      if (!is.null(selected_file) && nzchar(selected_file)) {
        restore_mode <- isTRUE(restore_rule_load_pending())
        if (restore_mode) {
          restore_rule_load_pending(FALSE)
        }

        # Simple time-based protection: wait if data was just loaded
        if (rule_loading_blocked()) {
          debug_log("Rule application blocked - will retry in 3 seconds", level = 1)

          later::later(function() {
            apply_rules_safely(
              selected_file,
              apply_to_metadata = !restore_mode,
              source = if (restore_mode) "session_restore" else "interactive"
            )
          }, delay = 3)
        } else {
          debug_log("Applying rules immediately", level = 1)
          apply_rules_safely(
            selected_file,
            apply_to_metadata = !restore_mode,
            source = if (restore_mode) "session_restore" else "interactive"
          )
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Helper: extract condition-group labels currently implied by Auto-Assign's
    # Condition Extraction Rules. Works purely off metadata skeleton/rule
    # data - never the full loaded dataset.
    extract_auto_assign_condition_values <- function(modules_list, add_agg_event, debug_log) {
      if (is.null(modules_list$auto_assign_out$extract_conditions_from_rules)) {
        debug_log("extract_conditions_from_rules not available", level = 2)
        return(character(0))
      }

      debug_log("Condition groups fallback extraction from rules/skeleton", level = 2)

      cond_res <- tryCatch(
        list(ok = TRUE,
             values = modules_list$auto_assign_out$extract_conditions_from_rules(),
             error = NULL),
        error = function(e) list(ok = FALSE, values = character(0), error = e$message)
      )

      if (!cond_res$ok) {
        add_agg_event("failed", "ConditionGroups", list(error = cond_res$error))
        debug_log(paste("Error extracting conditions:", cond_res$error), level = 1)
        return(character(0))
      }

      if (length(cond_res$values) == 0) {
        debug_log("No conditions extracted", level = 3)
      }

      cond_res$values
    }

    # Helper: derive condition-group labels from already-updated metadata.
    # This intentionally reads only Auto-Assign's condition rules plus the
    # metadata table returned by apply_rules(); it does not inspect the
    # primary data or copy large loaded data tables.
    extract_auto_assign_condition_values_from_metadata <- function(metadata_df, modules_list, add_agg_event, debug_log) {
      if (is.null(metadata_df) || !is.data.frame(metadata_df) || nrow(metadata_df) == 0) {
        debug_log("No metadata available for condition extraction", level = 3)
        return(character(0))
      }

      if (!("Content" %in% names(metadata_df)) || !("Options" %in% names(metadata_df))) {
        debug_log("Metadata missing Content or Options column for condition extraction", level = 2)
        return(character(0))
      }

      if (is.null(modules_list$auto_assign_out$condition_rules) ||
          !is.function(modules_list$auto_assign_out$condition_rules)) {
        debug_log("auto_assign_out$condition_rules not available", level = 2)
        return(character(0))
      }

      rules_res <- tryCatch(
        list(ok = TRUE, rules = modules_list$auto_assign_out$condition_rules(), error = NULL),
        error = function(e) list(ok = FALSE, rules = NULL, error = e$message)
      )

      if (!rules_res$ok) {
        add_agg_event("failed", "ConditionGroups", list(error = rules_res$error))
        debug_log(paste("Error reading condition rules:", rules_res$error), level = 1)
        return(character(0))
      }

      condition_rules <- rules_res$rules
      if (is.null(condition_rules) || !is.data.frame(condition_rules) ||
          nrow(condition_rules) == 0 || !("Content" %in% names(condition_rules))) {
        debug_log("No condition rules available for metadata condition extraction", level = 3)
        return(character(0))
      }

      values <- character(0)
      metadata_content <- metadata_df$Content
      metadata_options <- metadata_df$Options
      for (i in seq_len(nrow(condition_rules))) {
        content_value <- condition_rules$Content[[i]]
        if (is.na(content_value) || !nzchar(trimws(as.character(content_value)))) next

        matching_rows <- which(metadata_content == content_value)
        if (length(matching_rows) == 0) next

        row_values <- metadata_options[matching_rows]
        row_values <- trimws(as.character(row_values))
        row_values <- row_values[!is.na(row_values) & nzchar(row_values)]
        if (length(row_values) > 0) values <- c(values, row_values)
      }

      values <- unique(trimws(values))
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) > 0) {
        debug_log("Condition groups extracted from applied metadata", level = 2)
      }
      values
    }

    extract_ratio_group_values_from_metadata <- function(
    metadata_df,
    debug_log) {

      if (is.null(metadata_df) ||
          !is.data.frame(metadata_df) ||
          nrow(metadata_df) == 0L) {
        return(character())
      }

      available <- intersect(
        c("Numerator", "Denominator"),
        names(metadata_df)
      )

      if (!length(available)) {
        return(character())
      }

      values <- unlist(
        metadata_df[
          available
        ],
        use.names = FALSE
      )

      values <- trimws(
        as.character(values)
      )

      values <- values[
        !is.na(values) &
          nzchar(values)
      ]

      values <- unique(values)

      if (length(values)) {
        debug_log(
          paste(
            "Ratio numerator/denominator values available for Condition Groups:",
            length(values)
          ),
          level = 2
        )
      }

      values
    }

    # Helper: push extracted condition-group labels into the Assign Rules
    # module so the textbox_ui_condition boxes and the metadata table's
    # condition dropdown (Options_condition, consumed by
    # datawizard_tables.R) stay in sync. This fully replaces the current
    # condition groups/textboxes, matching the historical rule-file-load
    # behavior of set_conditions().
    sync_condition_groups_from_auto_assign <- function(modules_list, add_agg_event, debug_log, extracted_values = NULL) {
      if (is.null(extracted_values)) {
        extracted_values <- extract_auto_assign_condition_values(modules_list, add_agg_event, debug_log)
      }
      if (length(extracted_values) == 0) return(invisible(FALSE))

      # In Zielstruktur umwandeln
      condition_list <- setNames(as.list(extracted_values),
                                 paste0("Condition_", seq_along(extracted_values)))
      if (is.null(modules_list$assign_rules_out$set_conditions)) {
        add_agg_event("missing", "ConditionGroups")
        debug_log("set_conditions function not available", level = 2)
        return(invisible(FALSE))
      }

      set_cond_res <- tryCatch(
        list(ok = TRUE, success = isTRUE(modules_list$assign_rules_out$set_conditions(condition_list)), error = NULL),
        error = function(e) list(ok = FALSE, success = FALSE, error = e$message)
      )

      if (!set_cond_res$ok) {
        add_agg_event("failed", "ConditionGroups", list(error = set_cond_res$error))
        debug_log(paste("Error setting conditions:", set_cond_res$error), level = 1)
        return(invisible(FALSE))
      }

      if (isTRUE(set_cond_res$success)) {
        add_agg_event("conditions", "ConditionGroups", list(vals = extracted_values))
        debug_log(paste("Set", length(extracted_values), "conditions"), level = 2)
        return(invisible(TRUE))
      }

      add_agg_event("failed", "ConditionGroups", list(error = "set_conditions returned FALSE"))
      debug_log("set_conditions returned FALSE", level = 2)
      invisible(FALSE)
    }

    # Helper: merge extracted condition-group labels into the metadata
    # table's condition dropdown source, adding a new condition-group
    # textbox for any extracted condition not already represented by an
    # existing textbox, without overwriting the user's own already-typed
    # condition-group names. Used for re-application paths (e.g. the
    # "Apply Metadata Rules" button) where wiping user-entered condition
    # names would be surprising/destructive.
    merge_condition_groups_from_auto_assign <- function(modules_list, add_agg_event, debug_log, extracted_values = NULL) {
      if (is.null(extracted_values)) {
        extracted_values <- extract_auto_assign_condition_values(modules_list, add_agg_event, debug_log)
      }
      if (length(extracted_values) == 0) return(invisible(FALSE))

      if (is.null(modules_list$assign_rules_out$merge_condition_options)) {
        add_agg_event("missing", "ConditionGroups")
        debug_log("merge_condition_options function not available", level = 2)
        return(invisible(FALSE))
      }

      merge_res <- tryCatch(
        list(ok = TRUE, success = isTRUE(modules_list$assign_rules_out$merge_condition_options(extracted_values)), error = NULL),
        error = function(e) list(ok = FALSE, success = FALSE, error = e$message)
      )

      if (!merge_res$ok) {
        add_agg_event("failed", "ConditionGroups", list(error = merge_res$error))
        debug_log(paste("Error merging conditions:", merge_res$error), level = 1)
        return(invisible(FALSE))
      }

      if (isTRUE(merge_res$success)) {
        add_agg_event("conditions", "ConditionGroups", list(vals = extracted_values))
        debug_log(paste("Merged", length(extracted_values), "condition option(s)"), level = 2)
        return(invisible(TRUE))
      }

      debug_log("merge_condition_options returned FALSE", level = 3)
      invisible(FALSE)
    }

    # Helper function for safe rule application
    apply_rules_safely_impl <- function(file_name, apply_to_metadata = TRUE,
                                        source = "interactive",
                                        update_stage = function(stage) invisible(NULL)) {
      applied_metadata <- NULL
      metadata_commit_succeeded <- FALSE
      applied_condition_values <- character(0)
      set_metadata_assignment_state <- function(pending, state = NULL) {
        if (is.function(core_values$metadata_assignment_pending)) {
          core_values$metadata_assignment_pending(isTRUE(pending))
        }
        if (is.function(core_values$metadata_lifecycle_state) && !is.null(state)) {
          core_values$metadata_lifecycle_state(state)
        }
        rv$datawizard_metadata_assignment_pending <- isTRUE(pending)
        if (!is.null(state)) rv$datawizard_metadata_lifecycle_state <- state
      }

      debug_log(paste("apply_rules_safely invoked for:", file_name,
                      "(source:", source,
                      "| apply_to_metadata:", isTRUE(apply_to_metadata), ")"), level = 2)

      if (is.null(file_name) || !nzchar(file_name)) {
        debug_log("apply_rules_safely: empty file_name -> abort", level = 2)
        add_agg_event("failed", "Rule File", list(error = "Empty rule file name"))
        return(invisible(FALSE))
      }
      set_metadata_assignment_state(TRUE, "metadata_assigning")
      on.exit({
        ready <- isTRUE(metadata_content_ready())
        # Auto-assign and assign-rules updates write directly to handson_metadata().
        # Do not require rv$data_def to match here: the Tables module's
        # pause_metadata_sync checkbox only defers manual metadata_table
        # write-back, and must not keep rule-applied metadata in an assigning
        # state.
        if (is.function(core_values$metadata_meaningful_ready)) core_values$metadata_meaningful_ready(ready)
        set_metadata_assignment_state(FALSE, if (ready) "metadata_ready" else "metadata_failed_or_manual")
        rv$datawizard_metadata_meaningful_ready <- ready
      }, add = TRUE)

      # 1. Datei laden -----------------------------------------------------------
      update_stage("Reading metadata rule file")
      file_path <- file.path("AutoAssign", file_name)
      if (!file.exists(file_path)) {
        msg <- paste("Rule file not found:", file_name)
        debug_log(msg, level = 1)
        add_agg_event("failed", "Rule File", list(error = msg))
        return(invisible(FALSE))
      }

      rules_data_res <- tryCatch(
        list(ok = TRUE, value = readRDS(file_path), error = NULL),
        error = function(e) list(ok = FALSE, value = NULL, error = e$message)
      )
      if (!rules_data_res$ok) {
        debug_log(paste("Failed reading rule file:", rules_data_res$error), level = 1)
        add_agg_event("failed", "Rule File", list(error = rules_data_res$error))
        return(invisible(FALSE))
      }
      rules_data <- rules_data_res$value
      debug_log("Rule data loaded from file", level = 2)

      # 2. Auto-Assign Regeln laden ----------------------------------------------
      update_stage("Loading assignment rules")
      if (!is.null(modules_list$auto_assign_out$load_rules_directly)) {
        load_res <- tryCatch(
          list(ok = TRUE,
               success = isTRUE(modules_list$auto_assign_out$load_rules_directly(rules_data)),
               error = NULL),
          error = function(e) list(ok = FALSE, success = FALSE, error = e$message)
        )

        if (!load_res$ok) {
          debug_log(paste("Error in load_rules_directly:", load_res$error), level = 1)
          add_agg_event("failed", "Auto Assign Rules", list(error = load_res$error))
        } else if (isTRUE(load_res$success)) {
          add_agg_event("applied", "Auto Assign Rules")
          debug_log("Auto-assign rules loaded: TRUE", level = 2)

          # 3. Regeln auf Metadata anwenden --------------------------------------
          if (!isTRUE(apply_to_metadata)) {
            debug_log("Skipping metadata apply (session restore path)", level = 2)
          } else if (!is.null(modules_list$auto_assign_out$apply_rules)) {
            current_metadata <- core_values$handson_metadata()
            if (!is.null(current_metadata)) {
              apply_res <- tryCatch(
                {
                  metadata_rule_progress_context$callback <- update_stage
                  on.exit(metadata_rule_progress_context$callback <- function(stage) invisible(NULL), add = TRUE)
                  new_meta <- modules_list$auto_assign_out$apply_rules(current_metadata)
                  list(ok = TRUE, new_meta = new_meta, error = NULL)
                },
                error = function(e) list(ok = FALSE, new_meta = NULL, error = e$message)
              )

              if (!apply_res$ok) {
                debug_log(paste("Error applying rules to metadata:", apply_res$error), level = 1)
                add_agg_event("failed", "Auto Assign Rules", list(error = apply_res$error))
              } else {
                applied_metadata <- apply_res$new_meta

                if (!identical(apply_res$new_meta, current_metadata)) {
                  core_values$handson_metadata(apply_res$new_meta)
                  debug_log("Metadata updated by auto-assign rules", level = 1)
                }
                metadata_commit_succeeded <- isTRUE(all.equal(
                  isolate(core_values$handson_metadata()), applied_metadata,
                  check.attributes = FALSE
                ))

                applied_condition_values <- unique(c(
                  extract_auto_assign_condition_values_from_metadata(
                    applied_metadata,
                    modules_list,
                    add_agg_event,
                    debug_log
                  ),
                  extract_ratio_group_values_from_metadata(
                    applied_metadata,
                    debug_log
                  )
                ))
                if (length(applied_condition_values) > 0) {
                  update_stage("Updating condition groups")
                  condition_list <- setNames(as.list(applied_condition_values),
                                             paste0("Condition_", seq_along(applied_condition_values)))
                  if (is.null(modules_list$assign_rules_out$set_conditions)) {
                    add_agg_event("missing", "ConditionGroups")
                    debug_log("set_conditions function not available", level = 2)
                  } else {
                    set_cond_res <- tryCatch(
                      list(ok = TRUE,
                           success = isTRUE(modules_list$assign_rules_out$set_conditions(condition_list)),
                           error = NULL),
                      error = function(e) list(ok = FALSE, success = FALSE, error = e$message)
                    )

                    if (!set_cond_res$ok) {
                      add_agg_event("failed", "ConditionGroups", list(error = set_cond_res$error))
                      debug_log(paste("Error setting conditions:", set_cond_res$error), level = 1)
                    } else if (isTRUE(set_cond_res$success)) {
                      add_agg_event("conditions", "ConditionGroups", list(vals = applied_condition_values))
                      debug_log(paste("Set", length(applied_condition_values), "conditions from applied metadata"), level = 2)
                    } else {
                      add_agg_event("failed", "ConditionGroups", list(error = "set_conditions returned FALSE"))
                      debug_log("set_conditions returned FALSE", level = 2)
                    }
                  }
                }
              }
            } else {
              debug_log("No metadata present to apply rules to.", level = 2)
            }
          }
        } else {
          add_agg_event("failed", "Auto Assign Rules", list(error = "load_rules_directly returned FALSE"))
          debug_log("Auto-assign rules loaded: FALSE", level = 2)
        }
      } else {
        debug_log("auto_assign_out$load_rules_directly not available", level = 2)
        add_agg_event("missing", "Auto Assign Rules")
      }

      # 4. Edit-Operationen laden ------------------------------------------------
      if (!is.null(rules_data$edit_operations)) {
        update_stage("Loading edit operations")
        debug_log("Found edit_operations in rule file", level = 2)
        edit_loader <- modules_list$edit_out$load_operations_table
        if (is.function(edit_loader)) {
          edit_res <- tryCatch(
            list(ok = TRUE,
                 success = isTRUE(edit_loader(rules_data$edit_operations)),
                 error = NULL),
            error = function(e) list(ok = FALSE, success = FALSE, error = e$message)
          )
          if (!edit_res$ok) {
            debug_log(paste("Error loading edit operations:", edit_res$error), level = 1)
            add_agg_event("failed", "Edit", list(error = edit_res$error))
          } else if (isTRUE(edit_res$success)) {
            add_agg_event("applied", "Edit")
            debug_log("Edit operations loaded successfully", level = 2)
          } else {
            add_agg_event("failed", "Edit", list(error = "load_operations_table returned FALSE"))
            debug_log("Edit operations import returned FALSE", level = 2)
          }
        } else {
          add_agg_event("missing", "Edit")
          debug_log("edit_out$load_operations_table not available", level = 2)
        }
      } else {
        debug_log("No edit_operations in rule file", level = 3)
      }

      # 5. Conditions extrahieren ------------------------------------------------
      # Fallback for paths where metadata was not applied above (for example,
      # session restore) or where applied metadata did not yield condition
      # labels. Avoid unconditional stale skeleton extraction after successful
      # metadata-based syncing because it can overwrite the applied result.
      if (is.null(applied_metadata) || length(applied_condition_values) == 0) {
        update_stage("Updating condition groups")
        sync_condition_groups_from_auto_assign(modules_list, add_agg_event, debug_log)
      }

      # 6. Rule Status aktualisieren --------------------------------------------
      if (!is.null(modules_list$assign_rules_out$update_rule_status)) {
        tryCatch(
          modules_list$assign_rules_out$update_rule_status("loaded"),
          error = function(e) debug_log(paste("update_rule_status failed:", e$message), level = 2)
        )
      }

      # 7. SelectInput reset -----------------------------------------------------
      # (Keine Notification hier – Aggregation übernimmt)
      tryCatch({
        updateSelectInput(session, "assign_rules-load_rule_set_dw", selected = "")
      }, error = function(e) {
        debug_log(paste("Primary select reset failed:", e$message), level = 2)
        # Fallback Versuch mit vollem Namespace
        tryCatch({
          updateSelectInput(session, "datawizard_out-assign_rules-load_rule_set_dw", selected = "")
        }, error = function(e2) {
          debug_log(paste("Fallback reset also failed:", e2$message), level = 2)
        })
      })

      update_stage("Finalizing metadata")
      if (isTRUE(apply_to_metadata) && isTRUE(metadata_commit_succeeded) &&
          is.function(modules_list$tables_out$refresh_primary_table_style)) {
        modules_list$tables_out$refresh_primary_table_style(
          applied_metadata,
          source = "rule-set application"
        )
      }
      debug_log("apply_rules_safely finished", level = 2)
      invisible(TRUE)
    }

    apply_rules_safely <- function(file_name, apply_to_metadata = TRUE, source = "interactive") {
      interactive_apply <- identical(source, "interactive") && isTRUE(apply_to_metadata)
      if (interactive_apply && isTRUE(metadata_rule_application_in_progress())) {
        debug_log("Metadata-rule application already in progress; ignoring automatic request", level = 1)
        return(invisible(FALSE))
      }
      if (interactive_apply) {
        metadata_rule_application_in_progress(TRUE)
        on.exit(metadata_rule_application_in_progress(FALSE), add = TRUE)
      }
      with_metadata_rule_progress(interactive_apply, function(update_stage) {
        apply_rules_safely_impl(file_name, apply_to_metadata, source, update_stage)
      })
    }

    # Observer: Explicit re-application of metadata rules via button
    observeEvent(modules_list$assign_rules_out$apply_metadata_rules_trigger(), {
      debug_log("Apply Metadata Rules trigger received", level = 1)

      if (isTRUE(metadata_rule_application_in_progress())) {
        debug_log("Metadata-rule application already in progress; ignoring button trigger", level = 1)
        showNotification("Metadata rules are already being applied.", type = "warning", duration = 3)
        return(invisible(NULL))
      }
      metadata_rule_application_in_progress(TRUE)
      on.exit(metadata_rule_application_in_progress(FALSE), add = TRUE)

      with_metadata_rule_progress(TRUE, function(update_stage) {

      set_assignment_state <- function(pending, state) {
        if (is.function(core_values$metadata_assignment_pending)) core_values$metadata_assignment_pending(pending)
        if (is.function(core_values$metadata_lifecycle_state)) core_values$metadata_lifecycle_state(state)
        rv$datawizard_metadata_assignment_pending <- pending
        rv$datawizard_metadata_lifecycle_state <- state
      }
      set_assignment_state(TRUE, "metadata_assigning")
      error_reported <- FALSE
      on.exit({
        ready <- isTRUE(metadata_content_ready())
        if (is.function(core_values$metadata_meaningful_ready)) core_values$metadata_meaningful_ready(ready)
        set_assignment_state(FALSE, if (ready) "metadata_ready" else "metadata_failed_or_manual")
        rv$datawizard_metadata_meaningful_ready <- ready
      }, add = TRUE)

      fail_once <- function(message) {
        if (!error_reported) showNotification(message, type = "error", duration = 6)
        error_reported <<- TRUE
      }

      tryCatch({
        update_stage("Resolving current metadata")
        if (!is.function(modules_list$auto_assign_out$apply_rules)) stop("rule engine not available")
        primary <- resolve_datawizard_dataset("primary_working", core_values = core_values, rv = rv)$data
        aligned <- function(x) is.data.frame(x) && nrow(x) > 0L && metadata_matches_dataset(x, primary)

        table_meta <- tryCatch(isolate(modules_list$tables_out$current_metadata()), error = function(e) NULL)
        core_meta <- tryCatch(isolate(core_values$handson_metadata()), error = function(e) NULL)
        canonical_meta <- tryCatch(isolate(resolve_current_metadata("primary_working")), error = function(e) NULL)
        if (aligned(table_meta)) {
          source_name <- "tables_out$current_metadata"
          selected <- table_meta
        } else if (aligned(core_meta)) {
          source_name <- "core_values$handson_metadata"
          selected <- core_meta
        } else if (aligned(canonical_meta)) {
          source_name <- "canonical resolver"
          selected <- canonical_meta
        } else {
          stop("no aligned metadata is available")
        }
        debug_log(paste("Apply Metadata Rules selected source:", source_name), level = 2)
        debug_log(paste("Apply Metadata Rules source/canonical equality:",
                        isTRUE(all.equal(selected, canonical_meta, check.attributes = FALSE))), level = 2)

        # A serialized copy prevents rule code or later reactive invalidations
        # from mutating the snapshot selected above.
        update_stage("Copying current metadata")
        snapshot <- unserialize(serialize(selected, NULL))
        update_stage("Applying metadata rules")
        metadata_rule_progress_context$callback <- update_stage
        on.exit(metadata_rule_progress_context$callback <- function(stage) invisible(NULL), add = TRUE)
        new_meta <- modules_list$auto_assign_out$apply_rules(snapshot)
        update_stage("Validating metadata")
        if (!is.data.frame(new_meta)) stop("rule engine did not return a data frame")
        if (nrow(new_meta) != nrow(snapshot)) stop("rule engine changed the metadata row count")
        if (!"Column" %in% names(new_meta) || !identical(new_meta$Column, snapshot$Column)) {
          stop("rule engine changed metadata Column values or order")
        }
        missing_fields <- setdiff(names(snapshot), names(new_meta))
        if (length(missing_fields)) stop(paste("rule engine removed metadata fields:", paste(missing_fields, collapse = ", ")))

        changed_cells <- Map(function(before, after) {
          !(is.na(before) & is.na(after)) & (is.na(before) != is.na(after) | as.character(before) != as.character(after))
        }, snapshot, new_meta[names(snapshot)])
        fields_changed <- names(snapshot)[vapply(changed_cells, any, logical(1), na.rm = TRUE)]
        rows_changed <- if (length(changed_cells)) sum(Reduce(`|`, changed_cells)) else 0L
        debug_log(paste("Apply Metadata Rules rows changed:", rows_changed), level = 2)
        debug_log(paste("Apply Metadata Rules fields changed:", paste(fields_changed, collapse = ", ")), level = 2)

        setter <- modules_list$tables_out$set_current_metadata
        update_stage("Committing metadata")
        if (is.function(setter)) {
          commit <- setter(new_meta, source = "Apply Metadata Rules")
        } else {
          warning("Tables metadata setter unavailable; using canonical compatibility fallback")
          debug_log("Tables setter unavailable; using canonical setter fallback", level = 1)
          commit <- tryCatch({
            primary_data_state$set_metadata_for_current_data(new_meta)
            list(success = TRUE, reason = "canonical fallback committed",
                 table_committed = FALSE, canonical_committed = TRUE)
          }, error = function(e) list(success = FALSE, reason = e$message,
                                      table_committed = FALSE, canonical_committed = FALSE))
        }
        debug_log(paste("Apply Metadata Rules table commit result:", isTRUE(commit$table_committed), commit$reason), level = 2)
        debug_log(paste("Apply Metadata Rules canonical commit result:", isTRUE(commit$canonical_committed), commit$reason), level = 2)
        if (!isTRUE(commit$success)) stop(commit$reason %||% "metadata commit failed")

        live_table <- tryCatch(isolate(modules_list$tables_out$current_metadata()), error = function(e) NULL)
        live_canonical <- tryCatch(isolate(resolve_current_metadata("primary_working")), error = function(e) NULL)
        table_ok <- !is.function(setter) || isTRUE(all.equal(live_table, new_meta, check.attributes = FALSE))
        canonical_ok <- isTRUE(all.equal(live_canonical, new_meta, check.attributes = FALSE))
        if (!table_ok || !canonical_ok) stop("committed metadata mirrors did not agree")
        refresher <- modules_list$tables_out$refresh_primary_table_style
        if (is.function(refresher)) {
          refresher(new_meta, source = "Apply Metadata Rules")
        }

        extracted_values <- unique(c(
          extract_auto_assign_condition_values_from_metadata(
            new_meta,
            modules_list,
            add_agg_event,
            debug_log
          ),
          extract_ratio_group_values_from_metadata(
            new_meta,
            debug_log
          )
        ))
        update_stage("Synchronizing condition groups")
        merge_condition_groups_from_auto_assign(
          modules_list, add_agg_event, debug_log,
          if (length(extracted_values) > 0L) extracted_values else NULL
        )

        update_stage("Finalizing state")
        if (identical(new_meta, snapshot)) {
          showNotification("Rules applied. No changes to metadata.", type = "message", duration = 3)
        } else {
          showNotification("Metadata rules applied successfully.", type = "message", duration = 4)
        }
      }, error = function(e) {
        debug_log(paste("Error during explicit rule re-application:", e$message), level = 1)
        fail_once(paste("Error applying rules:", e$message))
      })
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    debug_log("Simplified data flow management initialized", level = 1)

    # ========================================
    # Integration and Observers
    # ========================================

    # Setup filter and state bridge observers
    register_datawizard_state_bridge_observers(
      modules_list = modules_list,
      core_values = core_values,
      ui_config_values = ui_config_values,
      modification_functions = modification_functions,
      rv = rv,
      primary_data_state = primary_data_state
    )

    # # Reset modification tracking when returning to truly raw data
    # observe({
    #   current_data <- core_values$primary_data_raw()
    #   if (!is.null(current_data)) {
    #     has_processed_cols <- any(grepl("^Imputed |^Batch Corrected |^Pivoted |^Merged ", names(current_data)))
    #     if (!has_processed_cols && !core_values$filter_applied() && core_values$data_modified()) {
    #       modification_functions$reset_modification_tracking()
    #     }
    #   }
    # })

    # --- Notification-Aggregator ---
    agg_env <- local({
      e <- new.env(parent = emptyenv())
      e$events <- list()
      e$scheduled <- FALSE
      e$enabled <- TRUE
      # Neu: Zustand je Modul (applied|failed|missing)
      e$module_state <- new.env(parent = emptyenv())  # key -> state
      e
    })
    agg_env$events <- list()
    agg_env$scheduled <- FALSE
    agg_env$enabled <- TRUE

    # Mapping interne Keys -> sprechende Namen
    AGG_FRIENDLY_NAMES <- c(
      filtering      = "Filtering",
      batch_effects  = "Batch Effects",
      pivot          = "Pivot",
      merge          = "Merge",
      ratios         = "Ratios",
      imputation     = "Imputation",
      edit           = "Edit",
      conditiongroups = "Condition Groups"
    )

    # Normalize Modulenames
    normalize_event_name <- function(x) {
      if (is.null(x) || !nzchar(x)) return("unknown")
      x2 <- tolower(trimws(x))
      # Ersetze Leer- und Sonderzeichen durch Unterstrich
      x2 <- gsub("[^a-z0-9]+", "_", x2)
      # Mehrfache Unterstriche entfernen
      x2 <- gsub("_+", "_", x2)
      # führende/trailing underscores weg
      x2 <- gsub("^_|_$", "", x2)
      if (!nzchar(x2)) x2 <- "unknown"
      x2
    }

    title_fallback <- function(key, original = NULL) {
      if (!is.null(original) && nzchar(original)) {
        # Wenn das Original schon "schön" aussieht (Leerzeichen, Großbuchstaben), nimm es
        if (grepl("[A-Z]", substr(original, 1, 2)) || grepl(" ", original)) {
          return(original)
        }
      }
      parts <- strsplit(key, "_", fixed = TRUE)[[1]]
      parts <- paste0(toupper(substring(parts, 1, 1)), substring(parts, 2))
      paste(parts, collapse = " ")
    }

    # Anzeigename für Event-Key bestimmen
    display_name_for <- function(key, original = NULL) {
      if (key %in% names(AGG_FRIENDLY_NAMES)) {
        return(AGG_FRIENDLY_NAMES[[key]])
      }
      title_fallback(key, original)
    }

    # Sicheres Hinzufügen eines Events (niemals Crash)
    add_agg_event <- function(kind, name, extra = NULL) {
      kind <- tolower(kind)
      key  <- normalize_event_name(name)
      entry <- list(
        kind = kind,
        key = key,
        original = name,
        extra = extra,
        ts = Sys.time()
      )
      # Anhängen
      agg_env$events[[length(agg_env$events) + 1]] <- entry

      # Flush nur einmal je Zyklus planen
      if (!isTRUE(agg_env$scheduled)) {
        agg_env$scheduled <- TRUE
        session$onFlush(function() {
          # Alles in TryCatch damit niemals abstürzt
          tryCatch({
            events <- agg_env$events
            agg_env$events <- list()
            agg_env$scheduled <- FALSE

            if (!length(events) || !isTRUE(agg_env$enabled)) {
              if (!length(events)) return(invisible())
              debug_log(paste("Aggregation disabled –", length(events), "Events verworfen"), level = 2)
              return(invisible())
            }

            # Gruppieren nach kind
            by_kind <- split(events, vapply(events, function(e) e$kind, character(1)))
            extract_unique_keys <- function(kind) {
              if (!kind %in% names(by_kind)) return(character(0))
              unique(vapply(by_kind[[kind]], function(e) e$key, character(1)))
            }
            keys_applied <- extract_unique_keys("applied")
            keys_failed  <- extract_unique_keys("failed")
            keys_pending <- extract_unique_keys("pending")
            keys_missing <- extract_unique_keys("missing")

            # Ratios-Konfigurationen zählen (falls extra$n)
            ratio_n <- NA_integer_
            if ("applied" %in% names(by_kind)) {
              ratio_events <- Filter(function(x) x$key == "ratios" && !is.null(x$extra) && !is.null(x$extra$n),
                                     by_kind[["applied"]])
              if (length(ratio_events)) {
                ratio_n <- ratio_events[[length(ratio_events)]]$extra$n
              }
            }

            # Conditions behandeln
            cond_line <- NULL
            if ("conditions" %in% names(by_kind)) {
              cond_events <- by_kind[["conditions"]]
              # Nehme erstes Event mit vals
              first_with_vals <- NULL
              for (ce in cond_events) {
                if (!is.null(ce$extra) && !is.null(ce$extra$vals)) {
                  first_with_vals <- ce
                  break
                }
              }
              if (!is.null(first_with_vals)) {
                vals <- first_with_vals$extra$vals
                if (is.null(vals)) vals <- character()
                cond_line <- paste0("Conditions: ", length(vals),
                                    if (length(vals) > 0)
                                      paste0(" (", paste(head(vals, 5), collapse = ", "),
                                             if (length(vals) > 5) ", …", ")")
                                    else "")
              } else {
                cond_line <- "Conditions: 0"
              }
            }

            # Funktion zum Erzeugen formatierten Textes
            format_key_list <- function(keys) {
              if (!length(keys)) return(NULL)
              disp <- vapply(keys, function(k) display_name_for(k), character(1))
              paste(disp, collapse = ", ")
            }

            line_applied <- NULL
            if (length(keys_applied)) {
              # Ratios evtl. detail anhängen
              applied_disp <- vapply(keys_applied, function(k) {
                base <- display_name_for(k)
                if (k == "ratios" && !is.na(ratio_n)) {
                  paste0(base, " (", ratio_n, " configuration", if (ratio_n != 1) "s" else "", ")")
                } else base
              }, character(1))
              line_applied <- paste0("Loaded: ", paste(applied_disp, collapse = ", "))
            }
            line_failed  <- if (length(keys_failed))  paste0("Failed: ",  format_key_list(keys_failed))  else NULL
            line_pending <- if (length(keys_pending)) paste0("Pending: ", format_key_list(keys_pending)) else NULL
            line_missing <- if (length(keys_missing)) paste0("No data for: ", format_key_list(keys_missing)) else NULL

            lines <- c(line_applied, line_pending, line_failed, line_missing, cond_line)
            lines <- lines[!is.null(lines) & nzchar(lines)]
            if (!length(lines)) lines <- "No changes."

            notif_type <- if (length(keys_failed)) "warning" else "message"
            msg <- paste("Rule Set Application Summary:\n", paste(lines, collapse = "\n"))

            # tryCatch({
            #    showNotification(msg, type = notif_type, duration = if (notif_type == "message") 8 else 10)
            # }, error = function(e2) {
            #   debug_log(paste("showNotification failed:", e2$message), level = 1)
            #   cat("[AGG Fallback]", msg, "\n")
            # })

          }, error = function(e) {
            # Letzter Rettungsanker
            debug_log(paste("Aggregator failure (caught, prevented crash):", e$message), level = 1)
            cat("[AGG ERROR - FALLBACK] Events lost safely.\n")
            agg_env$events <- list()
            agg_env$scheduled <- FALSE
          })
        }, once = TRUE)
      }
    }

    # UI Config Trigger Integration
    register_assign_rules_ui_config_observers(
      modules_list = modules_list,
      ui_config_functions = ui_config_functions,
      add_agg_event = add_agg_event
    )

    # ========================================
    # Legacy Compatibility
    # ========================================

     # create_legacy_compatibility(rv, core_values, modules_list, modification_functions, session)

    # ========================================
    # Filter Status Summary Output
    # ========================================

    # output$filter_status_summary <- renderText({
    #   filter_status <- core_values$filter_applied()
    #   raw_rows <- if (!is.null(core_values$primary_data_raw())) nrow(core_values$primary_data_raw()) else 0
    #   filtered_rows <- if (!is.null(core_values$filtered_data())) nrow(core_values$filtered_data()) else 0
    #
    #   status_lines <- character()
    #
    #   # # Central processing status
    #   # if (!is.null(apply_out)) {
    #   #   is_central_active <- safe_module_call(apply_out$central_processing_active,
    #   #                                         default_return = FALSE,
    #   #                                         context = "central_active_check")
    #   #   if (is_central_active) {
    #   #     current_step <- safe_module_call(apply_out$current_processing_step,
    #   #                                      default_return = "Processing...",
    #   #                                      context = "current_step")
    #   #     status_lines <- c(status_lines, paste("🔄 CENTRAL PROCESSING:", current_step), "")
    #   #   }
    #   # }
    #
    #   # Data status indicator
    #   if (modification_functions$is_data_modified()) {
    #     status_lines <- c(status_lines, "📊 Modified Data")
    #     history <- core_values$modification_history()
    #     if (length(history) > 0) {
    #       recent_ops <- sapply(tail(history, 3), function(x) x$operation)
    #       status_lines <- c(status_lines, paste("Recent:", paste(recent_ops, collapse = ", ")))
    #     }
    #   } else {
    #     status_lines <- c(status_lines, "📄 Raw Data")
    #   }
    #
    #   status_lines <- c(status_lines, paste("Original rows:", raw_rows))
    #
    #   if (filter_status) {
    #     status_lines <- c(status_lines, paste("Filtered rows:", filtered_rows))
    #     status_lines <- c(status_lines, paste("Removed:", raw_rows - filtered_rows))
    #   } else {
    #     status_lines <- c(status_lines, "No filters applied")
    #   }
    #
    #   # # Show enabled modules from central control
    #   # if (!is.null(apply_out)) {
    #   #   enabled_modules <- safe_module_call(apply_out$get_enabled_modules,
    #   #                                       default_return = list(),
    #   #                                       context = "enabled_modules")
    #   #   if (length(enabled_modules) > 0) {
    #   #     enabled_names <- names(enabled_modules)[sapply(enabled_modules, isTRUE)]
    #   #     if (length(enabled_names) > 0) {
    #   #       status_lines <- c(status_lines, "", "Central Control Enabled Modules:")
    #   #       status_lines <- c(status_lines, paste("-", paste(enabled_names, collapse = ", ")))
    #   #     }
    #   #   }
    #   # }
    #
    #   return(paste(status_lines, collapse = "\n"))
    # })

    # ========================================
    # Enhanced Return Interface
    # ========================================

    return(list(
      # Phase 1: Raw Data Access
      primary_data_raw = core_values$primary_data_raw,
      import_phase = core_values$import_phase,
      import_ready_revision = core_values$import_ready_revision,

      # Phase 2: Handson Metadata Access
      handson_metadata = core_values$handson_metadata,

      # Phase 3: Final Processed Data Access
      final_processed_data = core_values$final_processed_data,
      final_processed_metadata = core_values$final_processed_metadata,
      apply_triggered = core_values$apply_triggered,

      # Filter management
      filtered_data = core_values$filtered_data,
      filter_applied = core_values$filter_applied,
      primary_working_revision = core_values$primary_working_revision,
      primary_raw_revision = core_values$primary_raw_revision,
      primary_filtered_revision = core_values$primary_filtered_revision,
      metadata_revision = core_values$metadata_revision,
      metadata_content_signature = core_values$metadata_content_signature,
      identifier_choices = core_values$identifier_choices,
      secondary_revision = core_values$secondary_revision,
      primary_working_revision_debounced = core_values$primary_working_revision_debounced,
      metadata_revision_debounced = core_values$metadata_revision_debounced,
      metadata_content_signature_debounced = core_values$metadata_content_signature_debounced,
      identifier_choices_debounced = core_values$identifier_choices_debounced,
      secondary_revision_debounced = core_values$secondary_revision_debounced,
      working_data_with_filters = reactive({
        if (core_values$filter_applied() && !is.null(core_values$filtered_data())) {
          core_values$filtered_data()
        } else {
          core_values$primary_data_raw()
        }
      }),

      # Central metadata status management
      metadata_content_ready = metadata_content_ready,
      get_metadata_status_for_modules = reactive({
        list(
          ready = metadata_content_ready(),
          timestamp = Sys.time(),
          source = "central_management"
        )
      }),

      # Enhanced filtering module access
      filtering_confidence = core_values$filtering_confidence,
      filtering_valid_values = core_values$filtering_valid_values,
      filtered_conditions = core_values$filtered_conditions,
      filtering_log = core_values$filtering_log,

      # Filtering UI_config access
      central_filtering_ui_config = ui_config_values$central_filtering_ui_config,
      filtering_ui_config_source = reactive({ ui_config_values$filtering_ui_config_source() }),
      get_filtering_ui_config_for_export = config_export_functions$get_filtering_ui_config_for_export,
      set_filtering_ui_config_from_import = ui_config_functions$set_filtering_ui_config_from_import,
      create_filtering_ui_config = ui_config_functions$create_filtering_ui_config,

      # Ratio configurations management
      get_ratio_configurations_for_export = config_export_functions$get_ratio_configurations_for_export,

      # Data modification tracking
      data_modified = modification_functions$is_data_modified,
      modification_history = core_values$modification_history,
      record_modification = modification_functions$record_modification,
      reset_modification_tracking = modification_functions$reset_modification_tracking,

      # Central rule management
      central_rule_file = core_values$central_rule_file,
      central_loaded_rules = core_values$central_loaded_rules,
      rule_application_state = core_values$rule_application_state,

      # Enhanced imputation access
      imputation_out = modules_list$imputation_out,
      imputation_applied = reactive({
        safe_module_call(modules_list$imputation_out$imputation_applied, default_return = FALSE,
                         context = "imputation_applied_return")
      }),
      imputation_status = reactive({
        applied <- safe_module_call(modules_list$imputation_out$imputation_applied, default_return = FALSE,
                                    context = "imputation_status_applied")
        if (applied) {
          list(
            applied = TRUE,
            method = safe_module_call(modules_list$imputation_out$last_method, default_return = "Unknown",
                                      context = "imputation_status_method"),
            columns = safe_module_call(modules_list$imputation_out$last_columns, default_return = character(0),
                                       context = "imputation_status_columns"),
            matrix = safe_module_call(modules_list$imputation_out$imputation_matrix, default_return = NULL,
                                      context = "imputation_status_matrix")
          )
        } else {
          list(applied = FALSE)
        }
      }),

      # Ratio configurations access
      ratio_configurations = reactive({
        if (!is.null(modules_list$ratios_out)) {
          safe_module_call(modules_list$ratios_out$ratio_configurations, default_return = data.frame(),
                           context = "ratio_configurations_return")
        } else {
          data.frame()
        }
      }),

      # Imputation logging and settings access
      imputation_log = core_values$imputation_log,
      imputation_setting = core_values$imputation_setting,

      # UI_config management functions
      central_imputation_ui_config = ui_config_values$central_imputation_ui_config,
      ui_config_source = reactive({ ui_config_values$ui_config_source() }),
      get_imputation_ui_config_for_export = config_export_functions$get_imputation_ui_config_for_export,
      set_imputation_ui_config_from_import = ui_config_functions$set_imputation_ui_config_from_import,
      create_imputation_ui_config = ui_config_functions$create_imputation_ui_config,

      # All other UI configs
      central_batch_effects_ui_config = ui_config_values$central_batch_effects_ui_config,
      batch_effects_ui_config_source = reactive({ ui_config_values$batch_effects_ui_config_source() }),
      get_batch_effects_ui_config_for_export = config_export_functions$get_batch_effects_ui_config_for_export,
      set_batch_effects_ui_config_from_import = ui_config_functions$set_batch_effects_ui_config_from_import,
      create_batch_effects_ui_config = ui_config_functions$create_batch_effects_ui_config,

      central_ratios_ui_config = ui_config_values$central_ratios_ui_config,
      ratios_ui_config_source = reactive({ ui_config_values$ratios_ui_config_source() }),
      get_ratios_ui_config_for_export = config_export_functions$get_ratios_ui_config_for_export,
      set_ratios_ui_config_from_import = ui_config_functions$set_ratios_ui_config_from_import,
      create_ratios_ui_config = ui_config_functions$create_ratios_ui_config,

      central_pivot_ui_config = ui_config_values$central_pivot_ui_config,
      pivot_ui_config_source = reactive({ ui_config_values$pivot_ui_config_source() }),
      get_pivot_ui_config_for_export = config_export_functions$get_pivot_ui_config_for_export,
      set_pivot_ui_config_from_import = ui_config_functions$set_pivot_ui_config_from_import,
      create_pivot_ui_config = ui_config_functions$create_pivot_ui_config,

      central_merge_ui_config = ui_config_values$central_merge_ui_config,
      merge_ui_config_source = reactive({ ui_config_values$merge_ui_config_source() }),
      get_merge_ui_config_for_export = config_export_functions$get_merge_ui_config_for_export,
      set_merge_ui_config_from_import = ui_config_functions$set_merge_ui_config_from_import,
      create_merge_ui_config = ui_config_functions$create_merge_ui_config,

      # Basemean UI config
      central_basemean_ui_config = ui_config_values$central_basemean_ui_config,
      basemean_ui_config_source = reactive({ ui_config_values$basemean_ui_config_source() }),
      get_basemean_ui_config_for_export = config_export_functions$get_basemean_ui_config_for_export,
      set_basemean_ui_config_from_import = ui_config_functions$set_basemean_ui_config_from_import,
      create_basemean_ui_config = ui_config_functions$create_basemean_ui_config,

      # Edit operations access
      edit_operations = reactive({
        config_export_functions$get_edit_operations_for_export()
      }),
      get_edit_operations_for_export = config_export_functions$get_edit_operations_for_export,

      # Legacy compatibility
      primary_data = reactive({
        if (core_values$apply_triggered()) core_values$final_processed_data() else core_values$primary_data_raw()
      }),
      working_data = reactive({
        if (core_values$apply_triggered()) core_values$final_processed_data() else data_access_functions$get_file_data()
      }),
      working_metadata = reactive({
        if (core_values$apply_triggered()) core_values$final_processed_metadata() else core_values$handson_metadata()
      }),
      processed_data = reactive({
        if (core_values$apply_triggered()) core_values$final_processed_data() else NULL
      }),

      # Module outputs
      loader_out = loader_out,
      filtering_out = modules_list$filtering_out,
      imputation_out = modules_list$imputation_out,
      edit_out = modules_list$edit_out,
      ratios_out = modules_list$ratios_out,
      batch_out = modules_list$batch_out,
      # apply_out = apply_out,
      assign_rules_out = modules_list$assign_rules_out,
      auto_assign_out = modules_list$auto_assign_out,
      tables_out = modules_list$tables_out,
      pivot_out = modules_list$pivot_out,
      merge_out = modules_list$merge_out,

      # Session-restore fan-out: aggregate getter/setter across all
      # Data Wizard submodules. Used by R/session_save_restore.R to
      # round-trip per-submodule UI input state on save/restore.
      get_loader_session_state = function() {
        if (is.list(loader_out) && is.function(loader_out$get_session_state)) {
          return(loader_out$get_session_state())
        }
        NULL
      },
      set_loader_session_state = function(state) {
        if (is.list(loader_out) && is.function(loader_out$set_session_state)) {
          return(loader_out$set_session_state(state))
        }
        invisible(FALSE)
      },
      get_all_submodule_ui_states = function() {
        get_all_submodule_ui_states(modules_list)
      },
      set_all_submodule_ui_states = function(state) {
        set_all_submodule_ui_states(modules_list, state)
      },

      # # Module wrapper functions
      # ratios_functions = wrapper_functions$ratios_functions,
      # batch_functions = wrapper_functions$batch_functions,
      # edit_functions = wrapper_functions$edit_functions,

      # Error tracking and status management
      get_ui_config_errors = reactive({ core_values$ui_config_errors() }),
      get_filtering_config_errors = reactive({ core_values$filtering_config_errors() }),

      # Excel export functions
      create_results_excel = excel_functions$create_results_excel,
      get_excel_download_data = excel_functions$get_excel_download_data,
      is_export_ready = excel_functions$is_export_ready,

      # Status and processing functions
      processing_status = status_functions$processing_status,
      get_export_preview = status_functions$get_export_preview,

      # Enhanced working data with modification awareness
      enhanced_working_data = reactive({
        data <- data_access_functions$get_file_data()

        # Add modification metadata if available
        if (!is.null(data)) {
          attr(data, "is_modified") <- modification_functions$is_data_modified()
          attr(data, "modification_history") <- core_values$modification_history()

          # Add imputation information
          if (!is.null(modules_list$imputation_out)) {
            imp_matrix <- safe_module_call(modules_list$imputation_out$imputation_matrix,
                                           default_return = NULL,
                                           context = "enhanced_working_data_matrix")
            if (!is.null(imp_matrix)) {
              attr(data, "imputation_matrix") <- imp_matrix
              attr(data, "imputation_applied") <- safe_module_call(modules_list$imputation_out$imputation_applied,
                                                                   default_return = FALSE,
                                                                   context = "enhanced_working_data_applied")
              attr(data, "imputation_method") <- safe_module_call(modules_list$imputation_out$last_method,
                                                                  default_return = "Unknown",
                                                                  context = "enhanced_working_data_method")
            }
          }

          # Add logging information
          current_imp_log <- core_values$imputation_log()
          if (!is.null(current_imp_log)) attr(data, "imputation_log") <- current_imp_log

          current_imp_setting <- core_values$imputation_setting()
          if (!is.null(current_imp_setting)) attr(data, "imputation_setting") <- current_imp_setting

          # Add UI config information
          ui_config <- ui_config_values$central_imputation_ui_config()
          if (!is.null(ui_config)) {
            attr(data, "imputation_ui_config") <- ui_config
            attr(data, "ui_config_source") <- ui_config_values$ui_config_source()
          }

          # Enhanced filtering information
          confidence <- core_values$filtering_confidence()
          if (!is.null(confidence)) attr(data, "filtering_confidence") <- confidence

          valid_values <- core_values$filtering_valid_values()
          if (!is.null(valid_values)) attr(data, "filtering_valid_values") <- valid_values

          conditions <- core_values$filtered_conditions()
          if (!is.null(conditions)) attr(data, "filtered_conditions") <- conditions

          filter_log <- core_values$filtering_log()
          if (!is.null(filter_log)) attr(data, "filtering_log") <- filter_log

          # Filtering UI config information
          filter_ui_config <- ui_config_values$central_filtering_ui_config()
          if (!is.null(filter_ui_config)) {
            attr(data, "filtering_ui_config") <- filter_ui_config
            attr(data, "filtering_ui_config_source") <- ui_config_values$filtering_ui_config_source()
          }

          # Error tracking information
          ui_errors <- core_values$ui_config_errors()
          if (length(ui_errors) > 0) attr(data, "ui_config_errors") <- ui_errors

          filter_errors <- core_values$filtering_config_errors()
          if (length(filter_errors) > 0) attr(data, "filtering_config_errors") <- filter_errors
        }

        return(data)
      }),

      # # Central processing functions
      # trigger_central_processing = function() {
      #   if (!is.null(apply_out)) {
      #     safe_module_call(apply_out$perform_central_processing, default_return = FALSE,
      #                      context = "trigger_central_processing")
      #   }
      # },

      # get_enabled_modules = function() {
      #   if (!is.null(apply_out)) {
      #     return(safe_module_call(apply_out$get_enabled_modules, default_return = list(),
      #                             context = "get_enabled_modules"))
      #   }
      #   return(list())
      # },

      # check_module_availability = function() {
      #   if (!is.null(apply_out)) {
      #     return(safe_module_call(apply_out$check_module_availability, default_return = list(),
      #                             context = "check_module_availability"))
      #   }
      #   return(list())
      # },

      # Debug and testing functions
      test_filtering_manually = function() {
        test_filtering_manually(
          core_values$primary_data_raw,
          core_values$handson_metadata,
          core_values$filtering_valid_values,
          modules_list$filtering_out
        )
      },

      clear_all_config_errors = function() {
        core_values$ui_config_errors(list())
        core_values$filtering_config_errors(list())
        debug_log("All configuration errors cleared", level = 2)
      },

      reset_config_update_flags = function() {
        ui_config_values$ui_config_update_in_progress(FALSE)
        ui_config_values$filtering_update_in_progress(FALSE)
        ui_config_values$ratios_update_in_progress(FALSE)
        ui_config_values$batch_effects_update_in_progress(FALSE)
        ui_config_values$pivot_update_in_progress(FALSE)
        ui_config_values$merge_update_in_progress(FALSE)
        debug_log("Configuration update flags reset", level = 2)
      },

      clear_all_ui_configs = function() {
        ui_config_values$central_imputation_ui_config(NULL)
        ui_config_values$central_filtering_ui_config(NULL)
        ui_config_values$central_batch_effects_ui_config(NULL)
        ui_config_values$central_pivot_ui_config(NULL)
        ui_config_values$central_merge_ui_config(NULL)
        ui_config_values$central_ratios_ui_config(NULL)

        ui_config_values$ui_config_source("none")
        ui_config_values$filtering_ui_config_source("none")
        ui_config_values$batch_effects_ui_config_source("none")
        ui_config_values$pivot_ui_config_source("none")
        ui_config_values$merge_ui_config_source("none")
        ui_config_values$ratios_ui_config_source("none")

        core_values$ui_config_errors(list())
        core_values$filtering_config_errors(list())
        debug_log("All UI configs cleared", level = 2)
      },

      test_config_loading = function() {
        debug_log("=== TESTING CONFIGURATION LOADING ===", level = 1)
        debug_log(paste("UI config update in progress:", ui_config_values$ui_config_update_in_progress()), level = 1)
        debug_log(paste("Filtering update in progress:", ui_config_values$filtering_update_in_progress()), level = 1)
        debug_log(paste("UI config source:", ui_config_values$ui_config_source()), level = 1)
        debug_log(paste("Filtering UI config source:", ui_config_values$filtering_ui_config_source()), level = 1)
        debug_log(paste("UI config errors:", length(core_values$ui_config_errors())), level = 1)
        debug_log(paste("Filtering config errors:", length(core_values$filtering_config_errors())), level = 1)
        debug_log(paste("Last config application time:", core_values$last_config_application_time()), level = 1)
      },

      test_ratio_configurations_collection = function() {
        debug_log("=== TESTING RATIO CONFIGURATIONS COLLECTION ===", level = 1)
        result <- config_export_functions$get_ratio_configurations_for_export()
        debug_log("Collection result structure:", level = 1)
        str(result)
        return(result)
      },

      get_config_update_status = reactive({
        list(
          ui_config_update_in_progress = ui_config_values$ui_config_update_in_progress(),
          filtering_update_in_progress = ui_config_values$filtering_update_in_progress(),
          ratios_update_in_progress = ui_config_values$ratios_update_in_progress(),
          batch_effects_update_in_progress = ui_config_values$batch_effects_update_in_progress(),
          pivot_update_in_progress = ui_config_values$pivot_update_in_progress(),
          merge_update_in_progress = ui_config_values$merge_update_in_progress(),
          last_application_time = core_values$last_config_application_time()
        )
      })
    ))

    # ========================================
    # Session Cleanup
    # ========================================

    register_datawizard_session_cleanup(
      core_values = core_values,
      ui_config_values = ui_config_values,
      modification_functions = modification_functions
    )

  })
}
