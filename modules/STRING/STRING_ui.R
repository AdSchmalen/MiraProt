# ==============================================================================
# STRING Module - User Interface
# ==============================================================================
#
# Purpose:
#   Defines the complete UI layout for the STRING database network module.
#   The single exported function create_STRING_ui(ns) returns a tagList
#   consumed by modSTRINGUI in STRING_module.R.
#
# Architecture role:
#   Sourced at the module level (outside the server function) by STRING_module.R.
#   All UI IDs use the provided ns() namespace function. No server logic.
#
# File structure:
#   Panels are structured as a sidebar-main layout with:
#   - Protein input and selection panel
#   - Network creation controls
#   - Visualization network output
#   - Network customization panel (nodes, edges, labels, layout)
#   - Export controls
#
# Future developers:
#   - All input/output IDs must match those referenced in the server files.
#   - No reactive logic or server calls belong in this file.
# ==============================================================================

#' Create STRING UI
#' @param ns Namespace function
create_STRING_ui <- function(ns) {
  tabPanel("STRING DB",
           fluidRow(
             # Main area (9 columns)
             column(width = 9,

                    # Protein selection panel
                    fluidRow(
                      column(width = 12,
                             tags$textarea(id = ns("hiddenText_STRING"),
                                           style = "display:none;"),
                             wellPanel(
                               # Identifier selection
                               fluidRow(
                                 column(width = 6,
                                        selectInput(ns("identifier_type_STRING"),
                                                    label = "Select identifier",
                                                    choices = NULL, multiple = FALSE)
                                 )
                               ),

                               # Row 1: Search textarea + Suggested Identifiers (left 50%) + GSEA/GO dropdowns (right 50%)
                               fluidRow(
                                 column(width = 6,
                                        fluidRow(
                                          column(width = 6,
                                                 h6(
                                                   textOutput(
                                                     ns("search_identifier_label_STRING"),
                                                     inline = TRUE
                                                   )
                                                 ),
                                                 textAreaInput(
                                                   ns("input_STRING"),
                                                   label = NULL,
                                                   rows = 5,
                                                   width = "100%"
                                                 ),
                                                 tags$script(
                                                   HTML(
                                                     sprintf(
                                                       paste0(
                                                         "$(document)",
                                                         ".off('input.stringSuggestions', '#%s')",
                                                         ".on('input.stringSuggestions', '#%s', function() {",
                                                         "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
                                                         "});"
                                                       ),
                                                       ns("input_STRING"),
                                                       ns("input_STRING"),
                                                       ns("input_STRING")
                                                     )
                                                   )
                                                 )
                                          ),
                                          column(width = 6,
                                                 h6("Suggested Identifiers:"),
                                                 verbatimTextOutput(ns("geneSymbolList_STRING")),
                                                 tags$style(type = "text/css",
                                                            paste0("#", ns("geneSymbolList_STRING"),
                                                                   " {height: 120px; overflow-y: scroll;}"))
                                          )
                                        ),
                                        tags$style(type = "text/css",
                                                   paste0("#", ns("input_STRING"),
                                                          " {height: 120px !important; resize: vertical;}"))
                                 ),
                                 # PATHWAY SELECTION
                                 column(width = 6,
                                        h6("Import proteins of enriched GSEA gene sets:"),
                                        selectInput(ns("GSEA_STRING"),
                                                    label = NULL,
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    width = "100%"),
                                        h6("Import proteins of enriched GO terms:"),
                                        selectInput(ns("GO_STRING"),
                                                    label = NULL,
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    width = "100%")
                                 )
                               ),
                               # Shared button row across both columns
                               fluidRow(
                                 column(width = 2,
                                        actionButton(ns("transferButton_STRING"),
                                                     "Add",
                                                     icon = icon("plus"),
                                                     width = '100%')
                                 ),
                                 column(width = 2,
                                        actionButton(ns("clearButton_STRING"),
                                                     "Clear",
                                                     icon = icon("times-circle"),
                                                     width = '100%')
                                 ),
                                 column(width = 2,
                                        actionButton(ns("copyBtn_STRING"),
                                                     "Copy to Clipboard",
                                                     icon = icon("clipboard"),
                                                     width = '100%')
                                 ),
                                 column(width = 2,
                                        actionButton(ns("Protein_Input_STRING"),
                                                     "Add enriched proteins")
                                 ),
                                 column(width = 2,
                                        checkboxInput(ns("Intersect_STRING"),
                                                      label = "Intersecting proteins",
                                                      value = FALSE)
                                 ),
                                 column(width = 2,
                                        checkboxInput(ns("CoreEnriched_STRING"),
                                                      label = "Core enriched (GSEA)",
                                                      value = TRUE)
                                 )
                               ),
                               br(),

                               # Row 4: Selected Proteins - full width
                               fluidRow(
                                 column(width = 12,
                                        h5("Selected Proteins:"),
                                        uiOutput(ns("selectedGene_STRING"))
                                 )
                               )

                             )
                      )
                    ),

                    # Network visualization
                    fluidRow(
                      column(width = 12,
                             visNetworkOutput(ns("String_plot"),
                                              height = "900px")
                      )
                    ),

                    # Download panel
                    wellPanel(
                      h4("Download Network Plot"),
                      fluidRow(
                        column(width = 3,
                               numericInput(ns("resolution_DPI_STRING"),
                                            label = "DPI (where applicable)",
                                            value = 600,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotWidthInch_STRING"),
                                            label = "Width (inches)",
                                            value = 14,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotHeightInch_STRING"),
                                            label = "Height (inches)",
                                            value = 12,
                                            width = "100%")
                        ),
                        column(width = 3,
                               selectInput(ns("downloadFormat_STRING"),
                                           label = "Download format",
                                           choices = c("png", "jpeg", "tiff", "svg", "pdf"),
                                           width = "100%")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(
                          width = 3,
                          div(class = "input-align-bottom",
                              downloadButton(ns("downloadPlotButton_STRING"), "Download",
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
             ),

             # Sidebar (3 columns)
             column(width = 3,
                    wellPanel(
                      fluidRow(
                        column(width = 12,
                               actionButton(ns("create_STRING"),
                                            "Create STRING network",
                                            width = "100%",
                                            class = "btn-primary",
                                            icon = icon("chart-line"))
                        )
                      )
                    ),
                    br(),

                    # General options
                    wellPanel(
                      h4("General options"),
                      fluidRow(
                        column(width = 6,
                               selectInput(ns("version_STRING"),
                                           label = "STRING DB version",
                                           choices = c("3.0", "4.0", "5.1", "6.0", "6.2", "6.3",
                                                       "7.0", "7.1", "8.0", "8.1", "8.2", "8.3",
                                                       "9.0", "9.05", "9.1", "10.0a", "10.0",
                                                       "10.5", "11.0", "11.0b", "11.5", "12.0"),
                                           selected = "12.0",
                                           multiple = FALSE)
                        ),
                        column(width = 6,
                               numericInput(ns("score_STRING"),
                                            label = "Score threshold",
                                            min = 1,
                                            max = 1000,
                                            value = 500,
                                            step = 1)
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               selectInput(ns("edgetype_STRING"),
                                           label = "Displayed interactions",
                                           choices = c("Functional", "Physical", "Full"),
                                           selected = "Full",
                                           multiple = FALSE)
                        ),
                        column(width = 6,
                               numericInput(ns("neighbor_count_STRING"),
                                            label = "Include neighbours (0 = off)",
                                            min = 0,
                                            max = 500,
                                            value = 0,
                                            step = 1)
                        )
                      ),
                      fluidRow(
                        column(width = 8,
                               selectInput(ns("organism_STRING"),
                                           label = "Organism:",
                                           choices = c("Homo sapiens" = "9606",
                                                       "Mus musculus" = "10090",
                                                       "Rattus norvegicus" = "10116",
                                                       "Drosophila melanogaster" = "7227",
                                                       "Caenorhabditis elegans" = "6239",
                                                       "Saccharomyces cerevisiae" = "4932",
                                                       "Bos taurus" = "9913",
                                                       "Sus scrofa" = "9823",
                                                       "Equus caballus" = "9796"),
                                           selected = "9606",
                                           multiple = FALSE)
                        ),
                        column(width = 4,
                               tags$label(HTML("&nbsp;")),
                               actionButton(ns("update_organisms_STRING"),
                                            "Update Organisms",
                                            icon = icon("globe"),
                                            class = "btn-info btn-sm",
                                            width = "100%")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               selectInput(ns("neighbor_strategy_STRING"),
                                           label = "Choose neighbours with",
                                           choices = c("Most edges" = "most_edges",
                                                       "Connecting clusters" = "connecting_clusters"),
                                           selected = "most_edges",
                                           multiple = FALSE)
                        )
                      ),

                      fluidRow(
                        column(width = 12,
                               selectInput(ns("label_variant_STRING"),
                                           label = "Node label source",
                                           choices = NULL,
                                           multiple = FALSE)
                        )
                      )
                    ),
                    br(),

                    # Customization panel
                    wellPanel(
                      h4("Customization"),

                      # Layout
                      fluidRow(
                        column(width = 12,
                               selectInput(ns("Layout_STRING"),
                                           label = "Select layout type",
                                           choices = c("Fruchterman-Reingold" = "fr",
                                                       "Kamada-Kawai" = "kk",
                                                       "Random" = "random",
                                                       "Circle" = "circle",
                                                       "Star" = "star"),
                                           selected = "fr")
                        )
                      ),

                      # Degree filter
                      fluidRow(
                        column(width = 12,
                               numericInput(ns("EdgeNum_STRING"),
                                            label = "Minimum degree of freedom (number of edges)",
                                            min = 0,
                                            value = 0,
                                            step = 1)
                        )
                      ),

                      # fluidRow(
                      #   column(width = 12,
                      #          checkboxInput(ns("include_neighbors_STRING"),
                      #                        "Include neighbouring nodes",
                      #                        value = FALSE)
                      #   )
                      # ),

                      # Node colors
                      fluidRow(
                        column(width = 6,
                               colourpicker::colourInput(ns("Color_1_STRING"),
                                                         "Select node color",
                                                         value = '#BEBEBE',
                                                         allowTransparent = TRUE)
                        ),
                        column(width = 6,
                               colourpicker::colourInput(ns("Color_2_STRING"),
                                                         "Select frame color",
                                                         value = '#BEBEBE')
                        )
                      ),

                      # Node shape
                      fluidRow(
                        column(width = 6,
                               selectInput(ns("Shape_STRING"),
                                           "Select node shape",
                                           choices = c("Circle", "Box"),
                                           selected = "Circle")
                        )
                      ),

                      # Node size and frame
                      fluidRow(
                        column(width = 6,
                               numericInput(ns("Size_1_STRING"),
                                            "Select node size",
                                            min = 5,
                                            value = 50,
                                            max = 200,
                                            step = 5)
                        ),
                        column(width = 6,
                               numericInput(ns("Frame_STRING"),
                                            "Select frame width",
                                            min = 1,
                                            value = 2,
                                            max = 100,
                                            step = 1)
                        )
                      ),

                      # Edge properties
                      fluidRow(
                        column(width = 6,
                               colourpicker::colourInput(ns("EdgeColor_STRING"),
                                                         "Select edge color",
                                                         value = '#BEBEBE')
                        ),
                        column(width = 6,
                               selectInput(ns("EdgeType_STRING"),
                                           label = "Select edge type",
                                           choices = c("Solid", "Dashed", "Dotted", "Double"),
                                           selected = "Solid")
                        )
                      ),

                      fluidRow(
                        column(width = 6,
                               numericInput(ns("EdgeWidth_STRING"),
                                            label = "Select edge width",
                                            min = 1,
                                            max = 20,
                                            value = 2,
                                            step = 1)
                        )
                      ),

                      # Font properties
                      fluidRow(
                        column(width = 6,
                               selectInput(ns("FontType_STRING"),
                                           label = "Select font type",
                                           choices = c("Arial", "Times New Roman",
                                                       "Courier New", "Georgia"),
                                           selected = "Arial")
                        ),
                        column(width = 6,
                               selectInput(ns("FontStyle_STRING"),
                                           label = "Select font style",
                                           choices = c("Plain text", "Bold",
                                                       "Italic", "Bold and italic"),
                                           selected = "Plain text")
                        )
                      ),

                      fluidRow(
                        column(width = 6,
                               numericInput(ns("TextSize_STRING"),
                                            label = "Select text size",
                                            min = 1,
                                            max = 100,
                                            value = 12,
                                            step = 1)
                        )
                      ),

                      # Cluster selection (conditionally shown after network creation)
                      uiOutput(ns("cluster_section_STRING"))
                    ),
                    br(),
                    actionButton(ns("resetButton_STRING"),
                                 label = "Reset to Defaults",
                                 width = "100%",
                                 class = "btn-default",
                                 icon = icon("undo"))
             )
           )
  )
}
