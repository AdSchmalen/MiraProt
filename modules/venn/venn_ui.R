# =============================================================================
# modules/venn/venn_ui.R
#
# Purpose:
#   Defines the static and dynamic UI components for the Venn module. No
#   server logic, reactive values, or observers are present in this file.
#
# Architectural role:
#   UI layer of the Venn module. Sourced by Venn_module.R. Exports two UI
#   builder functions:
#     - create_venn_ui()   builds the full module layout (called from modVennUI)
#     - create_list_ui()   builds one protein-list input panel (called from the
#                          dynamicLists_Venn renderUI in venn_observer.R)
#
# File structure:
#   1. create_venn_ui()  -- full-page layout with plot area, controls, and
#                           configuration sidebar
#   2. create_list_ui()  -- single protein-list card with name, textarea,
#                           colour picker, and import selects
#
# Notes for future developers:
#   - create_list_ui() uses isolate() to read initial values from
#     list_data_Venn reactiveValues without creating reactive dependencies.
#     This is intentional: the dynamic list UI is re-rendered on demand by
#     the renderUI block in venn_observer.R, not on every reactive change.
#   - All input IDs use the ns namespace function passed in from the parent
#     server. Do not hard-code IDs.
# =============================================================================


#' Create Venn Module UI
#' @param ns Namespace function from parent module
create_venn_ui <- function(ns) {
  fluidRow(
    tags$style(HTML("
      .venn-inline-download {
        display: flex;
        align-items: flex-start;
        height: 100%;
      }

      .venn-inline-download .btn {
        width: 100%;
        margin-top: 30px;
      }

      .venn-remove-list-wrap {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        padding-top: 22px;
      }

      .venn-remove-list-btn {
        width: 38px;
        height: 38px;
        padding: 0;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
    ")),
    column(width = 9,
           # ========================================
           # Main Plot Area
           # ========================================
           fluidRow(column(width = 12,
                           uiOutput(ns("plotContainer_Venn"))
           )),

           # ========================================
           # Dynamic Lists Area
           # ========================================
           fluidRow(column(width = 12,
                           uiOutput(ns("dynamicLists_Venn")))),

           # ========================================
           # Control Buttons
           # ========================================
           fluidRow(column(width = 12,
                           actionButton(ns("add_proteins_Venn"), "Add Protein List"),
                           actionButton(ns("remove_proteins_Venn"), "Remove Protein List"),
                           actionButton(ns("fill_random_proteins_Venn"), "Fill Lists with Random Proteins")
           )),
           br(),

           # ========================================
           # Intersection Selection
           # ========================================
           fluidRow(
             column(width = 8,
                    selectInput(ns("intersection_dropdown"), "Select Intersection to View Proteins:",
                                choices = NULL, width = "100%")),
             column(width = 4,
                    div(class = "venn-inline-download",
                        downloadButton(ns("download_intersections_xlsx"), "Download Excel")))
           ),

           # ========================================
           # Intersection Results Display
           # ========================================
           fluidRow(column(width = 12,
                           textAreaInput(ns("selectedIntersection"), "Intersection Results:",
                                         width = "100%", height = "150px", value = ""),
                           div(style = "text-align: right; margin-top: 5px;",
                               actionButton(ns("copy_intersection_Venn"),
                                            "Copy to Clipboard",
                                            icon = icon("clipboard"))),
                           tags$script(HTML(paste0("
                            $(document).ready(function() {
                              $('#", ns("selectedIntersection"), "').prop('readonly', true);
                            });
                          ")))
           )),

           # ========================================
           # Download Section
           # ========================================
           fluidRow(column(width = 12, br())),
           fluidRow(column(width = 12,
                           wellPanel(
                             h4("Download Venn/Upset Plot"),
                             fluidRow(
                               column(width = 3,
                                      numericInput(ns("ppi_plot_Venn"), label = "DPI (where applicable)",
                                                   value = 600, width = "100%")
                               ),
                               column(width = 3,
                                      numericInput(ns("width_plot_Venn"), label = "Width (inches)",
                                                   value = 14, width = "100%")
                               ),
                               column(width = 3,
                                      numericInput(ns("height_plot_Venn"), label = "Height (inches)",
                                                   value = 900/96, width = "100%")
                               ),
                               column(width = 3,
                                      selectInput(ns("format_file_Venn"), label = "Download format",
                                                  choices = c("png", "jpeg", "tiff", "svg", "pdf"),
                                                  width = "100%")
                               )
                             ),
                             br(),
                             fluidRow(
                               column(
                                 width = 3,
                                 div(class = "input-align-bottom",
                                     downloadButton(ns("downloadPlot_Venn"), "Download",
                                                    width = "100%",
                                                    icon = icon("download"))
                                 )
                               ),
                               column(
                                 width = 3,
                                 br()
                               ),
                               column(
                                 width = 3,
                                 textInput(ns("grid_label"), "Label (optional)", value = "",
                                           width = "100%", placeholder = "Grid label")
                               ),
                               column(
                                 width = 3,
                                 div(class = "input-align-bottom",
                                     actionButton(ns("add_to_grid"), "Add Current Tab to Grid",
                                                  class = "btn btn-primary",
                                                  width = "100%",
                                                  icon = icon("plus"))
                                 )
                               )
                             )
                           )
           ))
    ),

    # ========================================
    # Configuration Panel
    # ========================================
    column(width = 3,
           wellPanel(
             fluidRow(
               column(width = 12,
                      actionButton(ns("create_plot_Venn"),
                                   "Create Plot",
                                   width = "100%",
                                   class = "btn-primary",
                                   icon = icon("chart-line"))
               )
             )
           ),
           br(),
           # ========================================
           # General Options
           # ========================================
           wellPanel(
             h4("General options"),
             selectInput(ns("diagramType_Venn"), "Select Diagram Type:",
                         choices = c("Venn", "UpSet", "UpSet with Abundances", "UpSet with Abundance Ratios")),

             # Reference value selection for sample filtering
             selectInput(ns("ReferenceValues_Venn"), "Select abundance type for sample filtering:",
                         choices = NULL, multiple = FALSE),

             # Always visible protein identifier selection
             selectInput(ns("GeneIdentifierColumn_Venn"), label = "Select protein identifier",
                         choices = NULL, multiple = FALSE),

             # ========================================
             # UpSet with Abundances Specific Options
             # ========================================
             conditionalPanel(
               condition = paste0("input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundances'"),
               selectizeInput(ns("data_abundance_Mean_Venn"), label = "Samples for mean abundance (default: all)",
                              choices = NULL, multiple = TRUE)
             ),

             # ========================================
             # UpSet with Abundance Ratios Specific Options
             # ========================================
             conditionalPanel(
               condition = paste0("input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundance Ratios'"),
               fluidRow(
                 column(width = 6,
                        selectizeInput(ns("data_abundance_ratio_num_Venn"), label = "Numerator:",
                                       choices = NULL, multiple = TRUE)
                 ),
                 column(width = 6,
                        selectizeInput(ns("data_abundance_ratio_denom_Venn"), label = "Denominator:",
                                       choices = NULL, multiple = TRUE)
                 )
               )
             )
           ),
           br(),

           # ========================================
           # Customization Panel
           # ========================================
           wellPanel(
             h4("Customization"),

             # ========================================
             # Venn Diagram Specific Options
             # ========================================
             conditionalPanel(
               condition = paste0("input['", ns("diagramType_Venn"), "'] == 'Venn'"),
               checkboxInput(ns("showPercentages_Venn"), "Show Percentages in Venn Diagram", value = FALSE),
               checkboxInput(ns("showListTitles_Venn"), "Show List Titles in Venn Diagram", value = TRUE),

               fluidRow(
                 column(width = 6,
                        numericInput(ns("overlapNumberSize_Venn"), "Size for Overlap Numbers:",
                                     value = 1.5, min = 0.1, max = 6, step = 0.1)
                 ),
                 column(width = 6,
                        numericInput(ns("listTitleSize_Venn"), "Size for List Titles:",
                                     value = 1.5, min = 0.1, max = 6, step = 0.1)
                 )
               ),

               numericInput(ns("listTitleDistance_Venn"), "Distance of List Titles from Venn Center:",
                            value = 0.05, min = -1, max = 2, step = 0.01),

               # Font options for list names
               fluidRow(
                 column(width = 6,
                        selectInput(ns("catFont_Venn"), "Font for List Names:",
                                    choices = c("sans", "serif", "mono"))
                 ),
                 column(width = 6,
                        selectInput(ns("cat_FontStyle_Venn"), "Style for List Names:",
                                    choices = c("plain", "bold", "italic"))
                 )
               ),

               # Font options for numbers
               fluidRow(
                 column(width = 6,
                        selectInput(ns("font_family_Venn"), "Font for Numbers:",
                                    choices = c("sans", "serif", "mono"))
                 ),
                 column(width = 6,
                        selectInput(ns("fontStyle_Venn"), "Style for Numbers:",
                                    choices = c("plain", "bold", "italic"))
                 )
               )
             ),

             # ========================================
             # UpSet Specific Options
             # ========================================
             conditionalPanel(
               condition = paste0("input['", ns("diagramType_Venn"), "'] == 'UpSet' ||
                                input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundances' ||
                                input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundance Ratios'"),

               conditionalPanel(
                 condition = paste0("input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundances' ||
                                  input['", ns("diagramType_Venn"), "'] == 'UpSet with Abundance Ratios'"),
                 checkboxInput(ns("showDotsInBoxplot_Venn"), "Show dots in boxplot", value = TRUE)
               ),

               # Theme selection
               fluidRow(
                 column(width = 12,
                        selectInput(ns("ThemeSelect_Upset"), label = "Select plot layout",
                                    choices = c("Gray", "Black and White", "Linedraw", "Light", "Minimal", "Classic", "Void"),
                                    selected = "Minimal", width = "100%")
                 )
               ),

               # Text size options
               fluidRow(
                 column(width = 6,
                       numericInput(ns("axis_title_size_Venn"), "Size axis title:",
                                     value = 20, min = 1, max = 60)
                 ),
                 column(width = 6,
                       numericInput(ns("axis_text_size_Venn"), "Size axis label:",
                                     value = 18, min = 1, max = 60)
                 )
               ),

               fluidRow(
                 column(width = 6,
                        numericInput(ns("label_text_size_Venn"), "Size bar label:",
                                     value = 10, min = 1, max = 60)
                 )
               )
             )
           ),
           br(),
           actionButton(ns("resetButton_Venn"),
                        label = "Reset to Defaults",
                        width = "100%",
                        class = "btn-default",
                        icon = icon("undo"))
    )
  )
}


#' Create individual protein list UI component
#' @param ns Namespace function
#' @param i List index
#' @param list_data_Venn Reactive values containing list data
create_list_ui <- function(ns, i, list_data_Venn) {
  safe_list_value <- function(x, i, default) {
    values <- tryCatch(isolate(x), error = function(e) NULL)

    if (is.null(i) || is.na(i) || i < 1L ||
        is.null(values) || length(values) < i || is.null(values[[i]])) {
      return(default)
    }

    values[[i]]
  }

  wellPanel(
    fluidRow(
      column(6,
             # Left column with name and protein inputs
             fluidRow(
               column(12, textInput(ns(paste0("name", i)), paste("List", i, "Name"),
                                    value = safe_list_value(list_data_Venn$names, i, paste("List", i)), width = "100%"))
             ),
             fluidRow(
               column(12, textAreaInput(ns(paste0("list", i)), paste("Protein List", i),
                                        value = safe_list_value(list_data_Venn$lists, i, ""),
                                        placeholder = "Enter each protein on a new line",
                                        width = "100%", height = "150px"))
             )
      ),
      column(6,
             # Right column with color picker and import options
             fluidRow(
               column(8, colourpicker::colourInput(ns(paste0("color", i)), paste("Color for List", i),
                                                   value = safe_list_value(list_data_Venn$colors, i, "#868686FF"), width = "100%")),
               column(4,
                      div(class = "venn-remove-list-wrap",
                          actionButton(ns(paste0("remove_list_VENN_", i)),
                                       label = NULL,
                                       icon = icon("times"),
                                       class = "btn-danger venn-remove-list-btn")
                      )
               )
             ),
             fluidRow(
               column(12, selectInput(ns(paste0("GSEA_SELECT_", i)),
                                      label = paste("Import proteins for List", i, "from enriched GSEA gene sets"),
                                      choices = NULL, multiple = TRUE, width = "100%"))
             ),
             fluidRow(
               column(12, selectInput(ns(paste0("GO_SELECT_", i)),
                                      label = paste("Import proteins for List", i, "from enriched GO terms"),
                                      choices = NULL, multiple = TRUE, width = "100%"))
             ),
             fluidRow(
               column(12, selectizeInput(ns(paste0("Sample_SELECT_", i)),
                                         label = paste("Import proteins for List", i, "from Sample"),
                                         choices = NULL, multiple = TRUE, width = "100%"))
             ),
             fluidRow(
               column(6, checkboxInput(ns(paste0("CoreEnriched_VENN_", i)),
                                       label = "Core enriched (GSEA)",
                                       value = isTRUE(safe_list_value(list_data_Venn$core_enriched, i, FALSE)))),
               column(6, actionButton(ns(paste0("copy_button_VENN_", i)),
                                      label = "Add enriched proteins", width = "100%"))
             )
      )
    )
  )
}
