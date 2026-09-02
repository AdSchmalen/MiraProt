# modules/GO/GO_ui.R
# GO Enrichment Analysis User Interface Module
# Complete UI implementation for GO analysis with all plot types and configuration options

############
# Main UI Function
############

#' GO Module User Interface
#'
#' Creates complete UI for GO enrichment analysis with all controls and plot types
#' @param id module namespace identifier
#' @export
GO_UI <- function(ns) {

  fluidRow(
    # Left Panel - GO Term Selection Tree & Plot Output
    column(width = 9,
           tagList(



             # GO Tree Selection
             div(
               tags$label(`for` = ns("goTree"), "Select GO terms:",
                          style = "font-weight:bold; font-weight:600; margin-bottom:5px; display:block;"),
               shinyTree(ns("goTree"),
                         checkbox = TRUE,
                         theme = "default",
                         multiple = TRUE,
                         animation = FALSE,
                         search = FALSE,
                         themeIcons = FALSE)
             ),
             br(),

             # Plot Type Selector + Button
             fluidRow(
               column(width = 12,
                      selectInput(ns("custom_EnrichPlot_select_GO"),
                                  label = "Plot Type",
                                  choices = c("Enrichment score dotplot",
                                              "Cnet plot (log2FC)",
                                              "Enrichment map",
                                              "Pubmed citations"),
                                  selected = "Enrichment score dotplot",
                                  width = "100%")
               )
             ),

             # Create plot button and height control (matching GSEA layout)
             fluidRow(
               column(
                 width = 3,
                 actionButton(
                   ns("create_go_plot"),
                   "Create Plot",
                   icon = icon("chart-line"),
                   class = "btn btn-primary btn-block",
                   width = "100%"
                 )
               )
             ),
             br(),

             # Plot Customization Panel (conditional, matching GSEA structure)
             conditionalPanel(
               condition = paste0("input['", ns("custom_EnrichPlot_select_GO"), "'] != ''"),

               wellPanel(
                 h5("Plot Customization"),

                 # Color inputs for gradient plots (conditional like GSEA)
                 conditionalPanel(
                   condition = paste0("['Cnet plot (log2FC)', 'Enrichment map', 'Enrichment score dotplot', 'Pubmed citations'].indexOf(input['", ns("custom_EnrichPlot_select_GO"), "']) >= 0"),
                   fluidRow(
                     column(width = 4,
                            colourpicker::colourInput(ns("GOColorInput_down"), "Color 1 (Low)", value = '#440154FF')),
                     column(width = 4,
                            colourpicker::colourInput(ns("GOColorInput_zero"), "Color 2 (Medium)", value = '#31688EFF')),
                     column(width = 4,
                            colourpicker::colourInput(ns("GOColorInput_up"), "Color 3 (High)", value = '#EFC000FF'))
                   )
                 ),

                 # Font size controls (matching GSEA exactly)
                 fluidRow(
                   column(width = 3,
                          numericInput(ns("AxisTitleSize_GO"), "Axis Title Size",
                                       value = 12, min = 8, max = 24, width = "100%")),
                   column(width = 3,
                          numericInput(ns("tickSize_GO"), "Tick Size",
                                       value = 10, min = 6, max = 20, width = "100%")),
                   column(width = 3,
                          numericInput(ns("LegendTitleSize_GO"), "Legend Title Size",
                                       value = 12, min = 8, max = 24, width = "100%")),
                   column(width = 3,
                          numericInput(ns("LegendTextSize_GO"), "Legend Text Size",
                                       value = 10, min = 6, max = 20, width = "100%"))
                 ),

                 # Theme selection and Label Size (matching GSEA layout)
                 fluidRow(
                   column(width = 3,
                          selectInput(ns("ThemeSelect_GO"), "Plot Theme",
                                      choices = c("Gray", "Black and White", "Linedraw",
                                                  "Light", "Dark", "Minimal", "Classic", "Void"),
                                      selected = "Black and White", width = "100%")),
                   column(width = 3,
                          selectInput(ns("LegendPosition_GO"), "Legend Position",
                                      choices = c("Right" = "right",
                                                  "Left" = "left",
                                                  "Top" = "top",
                                                  "Bottom" = "bottom",
                                                  "None" = "none"),
                                      selected = "right", width = "100%")),
                   column(
                     width = 3,
                     # Plot height control (like GSEA)
                     numericInput(
                       ns("plot_height_go"),
                       "Height (px):",
                       value = 600,
                       min = 300,
                       max = 1200,
                       step = 50,
                       width = "100%"
                     )
                   ),
                   column(
                     width = 3,
                     numericInput(ns("max_terms_GO"),
                                  "Maximum terms to display:",
                                  value = 10,
                                  min = 1,
                                  max = 100,
                                  step = 1)
                   )
                 )
               )
             ),

             # Main Plot Area
             # div(id = ns("plot_area"),
             #     style = "min-height: 400px; border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-top: 10px;",
             #     uiOutput(ns("GOplot_container"))
             # ),

             # Initial Plot
             div(
               style = "margin-top: 30px;",
               # plotOutput(ns("GOplot_1"))
               uiOutput(ns("GOplot_1"))
             ),

             # Download Panel
             wellPanel(
               h4("Download Plot"),
               fluidRow(
                 column(width = 3,
                        numericInput(ns("resolution_DPI_GO"),
                                     label = "DPI (where applicable)",
                                     value = 300,
                                     min = 72,
                                     max = 1200,
                                     step = 50,
                                     width = "100%")
                 ),
                 column(width = 3,
                        numericInput(ns("plotWidthInch_GO"),
                                     label = "Width (inches)",
                                     value = 10,
                                     min = 2,
                                     max = 50,
                                     step = 0.5,
                                     width = "100%")
                 ),
                 column(width = 3,
                        numericInput(ns("plotHeightInch_GO"),
                                     label = "Height (inches)",
                                     value = 8,
                                     min = 2,
                                     max = 50,
                                     step = 0.5,
                                     width = "100%")
                 ),
                 column(width = 3,
                        selectInput(ns("downloadFormat_GO"),
                                    label = "Download format",
                                    choices = c("PNG" = "png",
                                                "PDF" = "pdf",
                                                "SVG" = "svg",
                                                "JPEG" = "jpeg",
                                                "TIFF" = "tiff"),
                                    selected = "png",
                                    width = "100%")
                 )
               ),
               br(),
               fluidRow(
                 column(width = 3,
                        div(class = "input-align-bottom",
                            downloadButton(ns("downloadPlotButton_GO"),
                                           "Download",
                                           width = "100%",
                                           icon = icon("download"))
                        )
                 ),
                 column(width = 3,
                        br()
                 ),
                 column(width = 3,
                        textInput(ns("grid_label"), "Label (optional)", value = "",
                                  width = "100%", placeholder = "Grid label")
                 ),
                 column(width = 3,
                        div(class = "input-align-bottom",
                            actionButton(ns("add_to_grid"), "Add Current Tab to Grid",
                                         class = "btn btn-primary",
                                         width = "100%",
                                         icon = icon("plus"))
                        )
                 )
               ),
               br(),
               div(style = "font-size: 12px; color: #666;",
                   textOutput(ns("download_info_GO"))
               )
             )
           ),
           br(),
           wellPanel(
             h4("Export or Import GO Results"),
             p("Save your current GO analysis as an RDS file or import previously saved results.",
               style = "font-size: 0.9em; color: #666; margin-bottom: 15px;"),

             fluidRow(
               column(
                 width = 6,
                 div(
                   class = "form-group",
                   tags$label("Export current GO results:"),
                   br(),
                   uiOutput(ns("download_res_go_ui"))
                 )
               ),
               column(
                 width = 6,
                 div(
                   class = "form-group",
                   tags$label("Import saved GO results:"),
                   fileInput(
                     ns("go_import_rds"),
                     label = NULL,
                     accept = c(".rds"),
                     buttonLabel = "Browse...",
                     placeholder = "No file selected",
                     width = "100%"
                   )
                 )
               )
             ),

             fluidRow(
               column(
                 width = 12,
                 uiOutput(ns("go_import_rds_status"))
               )
             )
           )
    ),

    # Right Panel - Controls and Configuration
    column(width = 3,
           wellPanel(
             # Analysis Button
             fluidRow(
               column(width = 12,
                      actionButton(ns("createGO_button"),
                                   "Run GO Analysis",
                                   width = "100%",
                                   class = "btn-success",
                                   style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                                   icon = icon("chart-line"))
                      )
             )
           ),
           br(),
           # GO Analysis Parameters Panel
           wellPanel(
             h4("GO Analysis Parameters"),

             # Data Selection
             div(class = "form-group",
                 tags$label("Gene Identifier Column:"),
                 selectizeInput(ns("GeneIdentifierColumn_GO"),
                                label = NULL,
                                choices = NULL,
                                options = list(placeholder = "Select column..."))
             ),

             div(class = "form-group",
                 tags$label("Abundance Column:"),
                 selectizeInput(ns("AbundanceCol_GO"),
                                label = NULL,
                                choices = NULL,
                                options = list(placeholder = "Select column..."))
             ),

             div(class = "form-group",
                 tags$label("P-Value Type:"),
                 selectizeInput(ns("pValType_GO"),
                                label = NULL,
                                choices = NULL,
                                options = list(placeholder = "Select p-value type..."))
             ),

             div(class = "form-group",
                 tags$label("P-Value Column:"),
                 selectizeInput(ns("pValCol_GO"),
                                label = NULL,
                                choices = NULL,
                                options = list(placeholder = "Select p-value column..."))
             ),

             # Filter Criteria
             checkboxInput(
               ns("useMaxLog2FC_GO"),
               tagList("Use maximum log", tags$sub("2"), "(fold change)"),
               value = FALSE
             ),
             fluidRow(
               column(6,
                      conditionalPanel(
                        condition = sprintf("!input['%s']", ns("useMaxLog2FC_GO")),
                        tags$label(
                          `for` = ns("AbundanceInput_GO"),
                          tagList("Min. log", tags$sub("2"), "(FC):")
                        )
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s']", ns("useMaxLog2FC_GO")),
                        tags$label(
                          `for` = ns("AbundanceInput_GO"),
                          tagList("Max. log", tags$sub("2"), "(FC):")
                        )
                      ),
                      numericInput(ns("AbundanceInput_GO"),
                                   label = NULL,
                                   value = 1,
                                   step = 0.1)
               ),
               column(6,
                      numericInput(ns("pvalueInput_GO"),
                                   "Max. p-value:",
                                   value = 0.05,
                                   min = 0,
                                   max = 1,
                                   step = 0.001)
               )
             ),

             # GO Analysis Parameters
             selectInput(ns("ont_GO"),
                         "Ontology:",
                         choices = c("Molecular Function", "Cellular Component", "Biological Process", "All"),
                         selected = "All"),

             selectInput(ns("padjustMethod_GO"),
                         "P-value adjustment:",
                         choices = c("Benjamini & Hochberg", "Holm", "Hommel", "Benjamini & Yekutieli", "Bonferroni", "FDR", "None"),
                         selected = "Benjamini & Hochberg"),

             fluidRow(
               column(6,
                      numericInput(ns("pvalueCutoff_GO"),
                                   "P-value cutoff:",
                                   value = 0.05,
                                   min = 0,
                                   max = 1,
                                   step = 0.001)
               ),
               column(6,
                      numericInput(ns("qvalueCutoff_GO"),
                                   "Q-value cutoff:",
                                   value = 0.2,
                                   min = 0,
                                   max = 1,
                                   step = 0.01)
               )
             ),

             fluidRow(
               column(6,
                      numericInput(ns("minGSSize_GO"),
                                   "Min. gene set size:",
                                   value = 10,
                                   min = 1,
                                   step = 1)
               ),
               column(6,
                      numericInput(ns("maxGSSize_GO"),
                                   "Max. gene set size:",
                                   value = 500,
                                   min = 1,
                                   step = 10)
               )
             ),

             numericInput(ns("randomSeed_GO"),
                          "Random seed:",
                          value = 12345,
                          min = 1,
                          max = 2147483647,
                          step = 1,
                          width = "100%"),

             # Organism and Key Type
             selectInput(ns("keyType_GO"),
                         "Key type:",
                         choices = c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT"),
                         selected = "SYMBOL"),

             fluidRow(
               column(width = 6,
                      selectInput(ns("OrgDb_GO"),
                                  "Organism database:",
                                  choices = c("Homo sapiens"           = "Homo.sapiens",
                                              "Mus musculus"            = "Mus.musculus",
                                              "Rattus norvegicus"       = "Rattus.norvegicus",
                                              "Drosophila melanogaster" = "Drosophila.melanogaster",
                                              "Caenorhabditis elegans"  = "Caenorhabditis.elegans",
                                              "Saccharomyces cerevisiae" = "Saccharomyces.cerevisiae",
                                              "Bos taurus"              = "Bos.taurus",
                                              "Sus scrofa"              = "Sus.scrofa",
                                              "Equus caballus"          = "Equus.caballus"),
                                  selected = "Homo.sapiens")
                      )),
             fluidRow(
               column(width = 4,
                      actionButton(ns("refresh_cache"), "Refresh Cache",
                                   icon = icon("refresh"), class = "btn-default btn-sm",
                                   style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                                   width = "100%")
                      ),
               column(width = 4,
                      actionButton(ns("clear_cache_go"), "Clear Cache",
                                   icon = icon("trash"), class = "btn-default btn-sm",
                                   style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;",
                                   width = "100%")
                      ),
               column(width = 4,
                      actionButton(ns("update_organisms"), "Update Organisms",
                                   icon = icon("globe"), class = "btn-info btn-sm",
                                   width = "100%"))
             ),

             # Universe genes (optional)
             textAreaInput(ns("universe_GO"),
                           "Universe genes (optional):",
                           placeholder = "Enter identifiers matching the selected key type. Use one per line, or separate with commas/semicolons.",
                           rows = 3)


           ),
           br(),
           actionButton(ns("resetButton_GO"),
                        label = "Reset to Defaults",
                        width = "100%",
                        class = "btn-default",
                        icon = icon("undo"))

           # # Plot Styling Panel
           # wellPanel(
           #   h4("Plot Styling"),
           #
           #   # Color Configuration
           #   div(class = "form-group",
           #       tags$label("Color Palette:"),
           #       fluidRow(
           #         column(4, colourpicker::colourInput(ns("color1_GO"), "Low", value = "#440154FF")),
           #         column(4, colourpicker::colourInput(ns("color2_GO"), "Mid", value = "#31688EFF")),
           #         column(4, colourpicker::colourInput(ns("color3_GO"), "High", value = "#EFC000FF"))
           #       )
           #   ),
           #
           #   # Text Size Configuration (from monolithic)
           #   tags$label("Text Sizes:"),
           #   fluidRow(
           #     column(6, selectInput(ns("AxisTitleSize_GO"), "Axis Title Size:",
           #                           choices = seq(2, 64, by = 2),
           #                           selected = 20, multiple = FALSE)),
           #     column(6, selectInput(ns("tickSize_GO"), "Tick Size:",
           #                           choices = seq(2, 64, by = 2),
           #                           selected = 16, multiple = FALSE))
           #   ),
           #   fluidRow(
           #     column(6, selectInput(ns("LegendTextSize_GO"), "Legend Text Size:",
           #                           choices = seq(2, 64, by = 2),
           #                           selected = 18, multiple = FALSE)),
           #     column(6, selectInput(ns("LegendTitleSize_GO"), "Legend Title Size:",
           #                           choices = seq(2, 64, by = 2),
           #                           selected = 20, multiple = FALSE))
           #   ),
           #
           #   # Theme Selection (from monolithic)
           #   selectInput(ns("ThemeSelect_GO"),  # MUST be "ThemeSelect_GO" not "theme_GO"
           #               "Theme:",
           #               choices = c("Gray" = "Gray",
           #                           "Black and White" = "Black and White",
           #                           "Linedraw" = "Linedraw",
           #                           "Light" = "Light",
           #                           "Dark" = "Dark",
           #                           "Minimal" = "Minimal",
           #                           "Classic" = "Classic",
           #                           "Void" = "Void"),
           #               selected = "Black and White"),
           #
           #   # Plot Dimensions
           #   tags$label("Plot Dimensions:"),
           #   fluidRow(
           #     column(6, numericInput(ns("plotWidthInch_GO"), "Width (inch):", value = 10, min = 2, max = 20, step = 0.5)),
           #     column(6, numericInput(ns("plotHeightInch_GO"), "Height (inch):", value = 8, min = 2, max = 20, step = 0.5))
           #   )
           # )
    )
  )
}

############
# Additional UI Helper Functions
############

#' Create GO Tree Structure UI Element
#'
#' Creates the hierarchical tree structure for GO term selection
#' @param ns namespace function
#' @param tree_data hierarchical tree data structure
create_go_tree_ui <- function(ns, tree_data = NULL) {
  if (is.null(tree_data)) {
    # Empty tree placeholder
    div(
      style = "min-height: 200px; border: 1px dashed #ccc; padding: 20px; text-align: center; color: #666;",
      icon("hourglass-half"),
      br(), br(),
      "GO analysis results will appear here after running the analysis."
    )
  } else {
    shinyTree(ns("goTree"),
              checkbox = TRUE,
              theme = "default",
              multiple = TRUE,
              animation = TRUE,
              search = TRUE,
              themeIcons = FALSE)
  }
}

#' Create Plot Area UI
#'
#' Creates the main plot display area with loading states
#' @param ns namespace function
create_plot_area_ui <- function(ns) {
  div(
    id = ns("plot_area"),
    style = "min-height: 400px; border: 1px solid #ddd; border-radius: 4px; padding: 10px;",

    # Loading state
    conditionalPanel(
      condition = sprintf("output['%s'] == 'running'", ns("analysis_status")),
      div(
        style = "text-align: center; padding: 50px;",
        icon("spinner", class = "fa-spin fa-2x"),
        br(), br(),
        h4("Running GO Analysis..."),
        p("This may take a few moments.")
      )
    ),

    # Ready state (no results yet)
    conditionalPanel(
      condition = sprintf("output['%s'] == 'idle'", ns("analysis_status")),
      div(
        style = "text-align: center; padding: 50px; color: #666;",
        icon("search", class = "fa-2x"),
        br(), br(),
        h4("Ready for GO Analysis"),
        p("Configure parameters and click 'Run GO Analysis' to begin.")
      )
    ),

    # # Results display
    # conditionalPanel(
    #   condition = sprintf("output['%s'] == 'completed'", ns("analysis_status")),
    #   uiOutput(ns("GOplot_container"))
    # ),

    # Error state
    conditionalPanel(
      condition = sprintf("output['%s'] == 'error'", ns("analysis_status")),
      div(
        class = "alert alert-danger",
        style = "margin: 20px;",
        icon("exclamation-triangle"),
        h4("Analysis Error"),
        p("There was an error during GO analysis. Check the parameters and try again.")
      )
    )
  )
}

############
# Dynamic UI Updates
############

#' Update GO Tree with Results
#'
#' Updates the shinyTree with GO enrichment results
#' @param session shiny session
#' @param tree_structure hierarchical tree structure
update_go_tree_display <- function(session, tree_structure) {
  updateTree(session, "goTree", tree_structure)
}

#' Update Plot Type Choices
#'
#' Updates available plot types based on results
#' @param session shiny session
#' @param available_plots vector of available plot types
update_plot_type_choices <- function(session, available_plots) {
  updateSelectInput(session, "GO_result_plot_GO", choices = available_plots)
}

#' Update Integration Choices
#'
#' Updates choices for integration with other modules
#' @param session shiny session
#' @param go_terms vector of GO term descriptions
update_integration_choices <- function(session, go_terms) {
  updateSelectizeInput(session, "GO_PCA", choices = go_terms)
  updateSelectizeInput(session, "GO_Volcano", choices = go_terms)
  updateSelectizeInput(session, "GO_STRING", choices = go_terms)
}
