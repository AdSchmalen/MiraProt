modDataWizardUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Left Panel: Core Controls
    column(
      width = 3,
      modFileLoaderUI(ns("loader")),
      br(),
      div(
        actionButton(
          ns("open_data_loading_guide"),
          label = "Data Loading Guide",
          icon = icon("book-open"),
          width = "100%",
          class = "btn-info"
        )
      ),
      br(),
      modAssignRulesUI(ns("assign_rules")),
      br(),
      modAutoAssignUI(ns("auto_assign")),
      br(),
      div(
        class = "alert alert-info",
        style = "padding: 10px; margin-bottom: 15px;",
        h6("Processing Instructions:", style = "margin-top: 0;"),
        tags$ul(
          style = "margin: 0; font-size: 12px;",
          tags$li("Load your primary and additional data files using the file loader above"),
          tags$li("Define conditions manually or load a regex rule set/UI settings"),
          tags$li("Configure processing options in the processing panels (right)"),
          tags$li("Apply changes and review results in tables")
        )
      )
    ),
    column(
      width = 6,
      modDataTablesUI(ns("tables"))
    ),
    # Right Panel: Advanced Processing Options
    column(
      width = 3,
      # Advanced Processing Options
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
          style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_processing"), "', Math.random())"),
          h5(style = "margin: 0; display: inline-block;", "Advanced Processing"),
          tags$i(id = ns("processing_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
        ),
        div(
          id = ns("processing_content"),
          style = "display: none;",
          tabsetPanel(
            id = ns("processing_options"),
            type = "tabs",
            tabPanel("Merge", br(), modMergeUI(ns("merge"))),
            tabPanel("Pivot", br(), modPivotUI(ns("pivot"))),
            tabPanel("Batch Effects", br(), modBatchEffectsUI(ns("batch")))
          )
        )
      ),

      # Data Filtering
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
          style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_filtering"), "', Math.random())"),
          h5(style = "margin: 0; display: inline-block;", "Data Filtering"),
          tags$i(id = ns("filtering_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
        ),
        div(
          id = ns("filtering_content"),
          style = "display: none;",
          modFilteringUI(ns("filtering_ui")) # ,
          # br(),
          # div(
          #   style = "border: 1px solid #d4dae1; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
          #   h6("Filter Status"),
          #   verbatimTextOutput(ns("filter_status_summary"))
          # )
        )
      ),

      # Data Editing
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
          style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_edit"), "', Math.random())"),
          h5(style = "margin: 0; display: inline-block;", "Data Editing"),
          tags$i(id = ns("edit_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
        ),
        div(
          id = ns("edit_content"),
          style = "display: none;",
          modEditUI(ns("edit"))
        )
      ),

      # Missing Data Handling
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
          style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_imputation"), "', Math.random())"),
          h5(style = "margin: 0; display: inline-block;", "Missing Data Handling"),
          tags$i(id = ns("imputation_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
        ),
        div(
          id = ns("imputation_content"),
          style = "display: none;",
          modImputationUI(ns("imputation"))
        )
      ),

      # Custom Ratios & Statistics
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
          style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_ratios"), "', Math.random())"),
          h5(style = "margin: 0; display: inline-block;", "Data expansion"),
          tags$i(id = ns("ratios_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
        ),
        div(
          id = ns("ratios_content"),
          style = "display: none;",
          tabsetPanel(
            tabPanel("Contrasts", modRatiosUI(ns("ratios"))),
            tabPanel("Basemean", modBasemeanUI(ns("basemean"))),
            tabPanel("Annotation", modAnnotationUI(ns("annotation")))
          )
        )
      )
    )
  )
}
