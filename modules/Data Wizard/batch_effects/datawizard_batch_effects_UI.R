# ============================================================================
# File: modules/Data Wizard/batch_effects/datawizard_batch_effects_UI.R
#
# What this file does:
#   Builds the complete Shiny UI for the Batch Effects correction sub-module.
#   It returns a single tag tree that the orchestrator
#   (datawizard_batch_effects.R) embeds via modBatchEffectsUI().
#
# How it fits into the module architecture:
#   datawizard_batch_effects.R (orchestrator)
#     -> sources this file into modEnv
#     -> modBatchEffectsUI(id) calls build_batch_effects_ui(id)
#
# File structure:
#   1. build_batch_effects_ui(id) -- the single exported builder function.
#      Internally it constructs:
#        - Info tooltip
#        - Readiness gate (conditionalPanel pair)
#        - Method, imputation, transformation selectors
#        - Remove-imputed checkbox
#        - Dynamic batch column area (renderUI placeholder)
#        - Add/Remove batch group buttons
#        - Main "Correct Batch Effects" action button
#
# What future developers need to know:
#   - The function receives a raw module id, creates its own NS internally.
#   - The hidden textOutput "batch_ready" drives two conditionalPanels and
#     must match the output registered in the server (handlers file).
#   - The uiOutput "batch_columns" is populated server-side by renderUI.
#   - If new UI controls are added, corresponding server logic belongs in
#     the handlers or correction files, not here.
# ============================================================================

#' Build the Batch Effects module UI
#'
#' @param id Character. The Shiny module namespace id.
#' @return A Shiny tag object containing the full Batch Effects UI.
build_batch_effects_ui <- function(id) {
  ns <- NS(id)

  div(
    ## Tooltip at top
    fluidRow(
      column(12,
             div(style = "margin-top: -10px; margin-bottom: 15px; color: #666; font-size: 12px;",
                 HTML("<strong>Batch effect correction removes technical variability
                      introduced when samples are processed in different batches,
                      so that observed differences in protein levels reflect real biological changes
                      rather than experimental artifacts.</strong><br/>"))
      )),

    # Hidden readiness output for conditionalPanel (must exist in DOM)
    tags$div(style = "display: none;", textOutput(ns("batch_ready"))),

    # Not ready: show large notice
    conditionalPanel(
      condition = sprintf("output['%s'] !== 'true'", ns("batch_ready")),
      fluidRow(
        column(12,
               div(class = "alert alert-info",
                   style = "margin-top: 10px; font-size: 14px;",
                   "No data available. Please load data first."
               )
        )
      )
    ),

    # Ready: full Batch Effects UI
    conditionalPanel(
      condition = sprintf("output['%s'] === 'true'", ns("batch_ready")),

      ## Method selection for batch correction
      fluidRow(
        column(12,
               div(
                 title = "ComBat: Most robust method for complex batch effects using empirical Bayes.
               \nLimma: Basic regression-based batch correction without design matrix.
               \nOffset Correction: Simple median centering, fast but basic.
               \nLOESS: Non-linear smoothing for non-linear batch trends.
               \nQuantile: Makes data distributions identical within each batch separately.",
                 selectInput(
                   ns("batch_method"),
                   "Choose Method for Batch Correction",
                   choices = c("Offset Correction", "ComBat", "Limma", "LOESS", "Quantile"),
                   selected = "ComBat"
                 )
               )
        )
      ),

      ## Imputation method selection
      fluidRow(
        column(12,
               div(
                 title = "None: Skip proteins with missing values during correction, keeping original NAs.
\nLeft censored: Replace missing with small values, assumes values below detection limit.
\nRandom Forest: Machine learning-based prediction of missing values from other proteins.
\nMICE CART: Multiple imputation using decision trees, handles uncertainty in predictions.",
                 selectInput(
                   ns("imputation_method_batch"),
                   "Imputation Method",
                   choices = c("None", "Left censored", "Random Forest", "MICE CART"),
                   selected = "None"
                 )
               )
        )
      ),

      ## Data transformation selection
      fluidRow(
        column(12,
               div(
                 title = "None: Use original data scale.
               \nlog2: Logarithm base 2 (common for proteomics fold changes).
               \nlog10: Logarithm base 10 (general scientific notation).
               \n-log10: Negative log10 (for p-values or similar).",
                 selectInput(
                   ns("transformation_batch"),
                   "Data Transformation",
                   choices = c("None", "log2", "log10", "-log10"),
                   selected = "None"
                 )
               )
        )
      ),

      ## Option to remove imputed values after correction
      fluidRow(
        column(12,
               div(
                 title = "Check this to restore missing values after batch correction. Recommended for conservative analysis where you want to maintain the original missing data pattern.",
                 checkboxInput(ns("remove_imputed_batch"),
                               "Remove imputed values after correction",
                               value = FALSE)
               )
        )
      ),

      ## Dynamic batch column selectors
      fluidRow(
        column(12, uiOutput(ns("batch_columns")))
      ),

      ## Buttons to manage batch groups
      fluidRow(
        column(6,
               div(
                 title = "Add a new batch group to organize more columns from different experimental batches.",
                 actionButton(ns("addBatchButton"), "Add Batch Group",
                              width = "100%", class = "btn-primary")
               )),
        column(6,
               div(
                 title = "Remove the last batch group. At least one batch group must remain.",
                 actionButton(ns("removeBatchButton"), "Remove Batch Group",
                              width = "100%", class = "btn-default",
                              style = "background-color: #3498db; border-color: #3498db; color: #fff;")
               ))
      ),

      # Information about batch groups
      div(style = "margin-top: 10px; margin-bottom: 15px; color: #666; font-size: 12px;",
          "Each batch group should contain columns from the same experimental batch. You need at least 2 groups for meaningful batch correction. Missing values will be preserved during correction."),

      br(),

      ## Main action button for batch correction
      fluidRow(
        column(12,
               div(
                 title = "Start the batch effects correction process using the selected method and parameters. Make sure you have configured at least 2 batch groups.",
                 actionButton(ns("unBatchButton"), "Correct Batch Effects",
                              width = "100%", class = "btn-success",
                              style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;")
               )
        )
      )
    )
  )
}
