# ============================================================================
# Module/Sub-script: modules/Data Wizard/assign rules/datawizard_assign_rules_UI.R
# Purpose:
#   UI composition for assign-rules interactions: rule-set selection, readiness
#   feedback, and condition-group controls used by the assign-rules server.
#
# Architectural Role:
#   UI composition
#
# Responsibilities:
#   - Define stable namespaced UI controls consumed by module server handlers.
#   - Present assignment-rule controls with readiness- and status-driven display.
#   - Keep layout semantics clear for condition and metadata-assignment tasks.
#
# Non-Responsibilities:
#   - Perform rule parsing, validation, or data mutation.
#   - Own reactive orchestration or cross-module integration logic.
#
# Allowed Dependencies:
#   - Shiny UI primitives (`NS`, `div`, `wellPanel`, `conditionalPanel`, etc.).
#   - Module server output ids defined by `datawizard_assign_rules.R`.
#
# Interaction Boundaries:
#   - Inputs: namespaced module id and server-rendered output values.
#   - Outputs: assign-rules UI tree with stable input/output ids.
#   - Side Effects: none beyond declarative UI rendering.
#
# Stability Guarantees:
#   - Preserve existing input/output ids to avoid breaking server observers.
#   - Preserve conditional visibility semantics for readiness/loading indicators.
# ============================================================================


############
# Main UI Function

#' Condition Assignment Rules Module UI
#'
#' Creates UI components for managing condition assignments and sample groupings
#' @param id module namespace id
#' @export
modAssignRulesUI <- function(id) {
  ns <- NS(id)

  div(
    wellPanel(
      class = "well-panel",
      h4("Conditions & Metadata Loader"),

      # Hidden readiness output used by conditionalPanel
      tags$div(style = "display: none;", textOutput(ns("assign_ready"))),

      # Conditional rule set loading (only show when data is loaded)
      uiOutput(ns("rule_set_ui")),

      # Loading indicator
      conditionalPanel(
        condition = sprintf("output['%s'] === 'loading'", ns("rule_loading_status_output")),
        div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          HTML("<strong>Loading rule set...</strong>")
        )
      ),

      # Re-apply metadata rules button (visible only after rules have been loaded).
      # Uses rules_available_output (based on last_loaded_rule_data) instead of
      # rule_loading_status_output because rule_loading_status resets to "idle"
      # when the selectInput is programmatically cleared after rule loading.
      conditionalPanel(
        condition = sprintf("output['%s'] === 'true'", ns("rules_available_output")),
        div(
          style = "margin: 5px 0 10px 0;",
          actionButton(
            ns("apply_metadata_rules_btn"),
            "Apply Metadata Rules",
            width = "100%",
            class = "btn-default",
            title = "Re-apply the loaded metadata assignment rules to the current metadata. This will overwrite Content, Sample, and Ratio assignments for all columns matching the rule patterns."
          )
        )
      ),

      br(),

      # Show everything below ONLY when data/files are available
      conditionalPanel(
        condition = sprintf("output['%s'] === 'true'", ns("assign_ready")),

        # Condition definition section
        fluidRow(
          column(12,
                 div(
                   title = "Define condition groups for your experimental design",
                   h5("Define Condition Groups")
                 )
          )
        ),

        # Add condition button; individual condition rows render their own remove buttons.
        fluidRow(
          column(12,
                 div(
                   title = "Add a new condition group to your experimental design",
                   actionButton(
                     ns("add_btn"),
                     "Add",
                     width = "100%",
                     class = "btn-primary"
                   )
                 ))
        ),

        br(),

        # Dynamic condition input boxes
        fluidRow(
          column(12,
                 div(
                   title = "Enter names for each condition group. These will be used to categorize your samples.",
                   uiOutput(ns("textbox_ui_condition"))
                 )
          )
        )
      )
    )
  )
}
