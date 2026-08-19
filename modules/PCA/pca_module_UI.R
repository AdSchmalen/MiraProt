# ==============================================================================
# File: modules/PCA/pca_module_UI.R
#
# Purpose:
#   Defines the complete UI layout for the PCA/Dimension Reduction module.
#   Exports create_pca_ui(id), which is called from modPCAUI() in
#   pca_module.R.
#
# Architectural Role:
#   This file is purely declarative. It defines static widget trees using
#   Shiny and shinydashboardPlus components. No server logic, reactives, or
#   observers live here. Dynamic choices (sample lists, identifier columns,
#   pathway lists) are populated at runtime by the observer registration
#   functions in pca_module_server_observers.R.
#
# Structure:
#   1. create_pca_ui(id) - top-level function; creates the namespaced UI
#      a. Left sidebar panel: analysis controls, sample/data selection
#      b. Main plot panel: static and interactive plot outputs, scree plot
#      c. Right sidebar: protein/item selection, label controls, export
#
# Notes for future developers:
#   - All input IDs must be wrapped with ns() to respect module namespacing.
#   - Choices for dynamic inputs (e.g., selectInput for samples or identifiers)
#     are set to placeholder values here and updated at runtime via
#     register_pca_input_observers and register_pca_ui_state_observers.
#   - Do not add Shiny logic (observe, reactive, renderXxx) to this file.
# ==============================================================================

create_pca_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Custom CSS and JavaScript
    tags$head(
      # Enhanced clipboard functionality
      tags$script(HTML("
        // Modern clipboard API with fallback
        window.copyToClipboard = function(text, elementId) {
          const copyFunction = async () => {
            try {
              await navigator.clipboard.writeText(text);
              return true;
            } catch (err) {
              // Fallback for older browsers
              const textArea = document.createElement('textarea');
              textArea.value = text;
              textArea.style.position = 'fixed';
              textArea.style.left = '-999999px';
              textArea.style.top = '-999999px';
              document.body.appendChild(textArea);
              textArea.focus();
              textArea.select();
              try {
                const successful = document.execCommand('copy');
                document.body.removeChild(textArea);
                return successful;
              } catch (err) {
                document.body.removeChild(textArea);
                return false;
              }
            }
          };

          copyFunction().then(success => {
            if (success && elementId) {
              Shiny.setInputValue(elementId, Math.random());
            }
          });
        };
      ")),

      # Custom styles
      tags$style(HTML("
        .method-description {
          font-size: 12px;
          color: #666;
          margin-top: 10px;
          padding: 8px;
          border-radius: 4px;
          background-color: #f8f9fa;
          border-left: 3px solid #007bff;
        }

        .parameter-panel {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
          margin-top: 10px;
        }

        .results-panel {
          background-color: #fff;
          padding: 15px;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          margin-top: 10px;
        }

        .protein-search-results {
          max-height: 200px;
          overflow-y: auto;
          border: 1px solid #ced4da;
          border-radius: 4px;
          padding: 10px;
          background-color: #fff;
          font-family: monospace;
          font-size: 12px;
        }

        .selected-proteins-table {
          max-height: 200px;
          overflow-y: auto;
        }

        .input-align-bottom {
          display: flex;
          align-items: flex-start;
          height: 100%;
        }

        .input-align-bottom .btn {
          width: 100%;
          margin-top: 30px;
        }
      "))
    ),

    # Main layout
    fluidRow(
      # Left side - Plot area
      column(width = 9,
             fluidRow(

               column(width = 3,
                      checkboxInput(ns("interactive_plot"),
                                    label = "Interactive Plot",
                                    value = FALSE)
               ),
               column(width = 9,
                      div())
             ),

             # Plot output area
             conditionalPanel(
               condition = "output.plots_ready",
               ns = ns,

               # Tab panel for different plot views
               tabsetPanel(id = ns("plot_tabs"),

                           # Main plot tab
                           tabPanel("Main Plot",
                                    br(),
                                    # Static plot
                                    conditionalPanel(
                                      condition = "!input.interactive_plot",
                                      ns = ns,
                                      plotOutput(ns("static_plot"), height = "600px", click = ns("plot_click"))
                                    ),

                                    # Interactive plot
                                    conditionalPanel(
                                      condition = "input.interactive_plot",
                                      ns = ns,
                                      plotlyOutput(ns("interactive_plot_output"), height = "600px")
                                    )
                           ),

                           # Scree plot tab
                           tabPanel("Scree Plot", value = "scree_tab",
                                    br(),
                                    conditionalPanel(
                                      condition = "input.analysis_method == 'pca' && input.show_scree",
                                      ns = ns,
                                      plotOutput(ns("scree_plot"), height = "400px")
                                    )
                           )

                           )
               ),

             # COMBINED PROTEIN SELECTION AND LABEL CONTROLS PANEL - USING DATAWIZARD PATTERN
             conditionalPanel(
               condition = sprintf("!input['%s'] && output['%s'] && input['%s'] == 'proteins'",
                                   ns("interactive_plot"),
                                   ns("plots_ready"),
                                   ns("comparison_target")),

               # Protein Selection & Labeling wellPanel with DataWizard pattern
               wellPanel(
                 style = "margin-bottom: 10px;",
                 tags$div(
                   id = ns("protein_controls_header"),
                   style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
                   onclick = paste0("Shiny.setInputValue('", ns("toggle_protein_controls"), "', Math.random())"),
                   h4(style = "margin: 0; display: inline-block;", "Protein Selection & Labeling"),
                   tags$i(id = ns("protein_controls_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
                 ),
                 div(
                   id = ns("protein_controls_content"),
                   style = "display: none;",

                   # PROTEIN SELECTION SECTION
                   h5("Protein Selection"),

                   # Row 1: Search textarea (left) + Suggested Identifiers (right), equal height
                   fluidRow(
                     column(width = 6,
                            fluidRow(
                              column(
                                width = 6,
                                h6(
                                  textOutput(
                                    ns("search_identifier_label_pca"),
                                    inline = TRUE
                                  )
                                ),
                                textAreaInput(
                                  ns("searchGene_pca"),
                                  label = NULL,
                                  rows = 5,
                                  width = "100%"
                                ),
                                tags$script(
                                  HTML(
                                    paste0(
                                      "$(document)",
                                      ".off('input.pcaSuggestions', '#",
                                      ns("searchGene_pca"),
                                      "')",
                                      ".on('input.pcaSuggestions', '#",
                                      ns("searchGene_pca"),
                                      "', function() {",
                                      "Shiny.setInputValue('",
                                      ns("searchGene_pca"),
                                      "', this.value, {priority: 'event'});",
                                      "});"
                                    )
                                  )
                                )
                              ),
                              column(width = 6,
                                     h6("Suggested Identifiers:"),
                                     verbatimTextOutput(ns("geneSymbolList_pca")),
                                     tags$style(type = "text/css",
                                                paste0("#", ns("geneSymbolList_pca"),
                                                       " {height: 120px; overflow-y: scroll;}"))
                              )
                            ),
                            tags$style(type = "text/css",
                                       paste0("#", ns("searchGene_pca"),
                                              " {height: 120px !important; resize: vertical;}"))
                     ),
                     # PATHWAY SELECTION
                     column(width = 6,
                            h6("Import proteins of enriched GSEA gene sets:"),
                            selectInput(ns("GSEA_pca"),
                                        label = NULL,
                                        choices = NULL,
                                        multiple = TRUE,
                                        width = "100%"),
                            h6("Import proteins of enriched GO terms:"),
                            selectInput(ns("GO_pca"),
                                        label = NULL,
                                        choices = NULL,
                                        multiple = TRUE,
                                        width = "100%")
                     )
                   ),
                   # Shared button row across both columns
                   fluidRow(
                     column(width = 2,
                            actionButton(ns("transferButton_pca"),
                                         "Add",
                                         icon = icon("plus"),
                                         width = '100%')
                     ),
                     column(width = 2,
                            actionButton(ns("clearButton_pca"),
                                         "Clear",
                                         icon = icon("times-circle"),
                                         width = '100%')
                     ),
                     column(width = 2,
                            actionButton(ns("copyBtn_pca"),
                                         "Copy to Clipboard",
                                         icon = icon("clipboard"),
                                         width = '100%')
                     ),
                     column(width = 2,
                            actionButton(ns("Protein_Input_pca"),
                                         "Add enriched proteins")
                     ),
                     column(width = 2,
                            checkboxInput(ns("Intersect_pca"),
                                          label = "Intersecting proteins",
                                          value = FALSE)
                     ),
                     column(width = 2,
                            checkboxInput(ns("CoreEnriched_pca"),
                                          label = "Core enriched (GSEA)",
                                          value = TRUE)
                     )
                   ),
                   br(),

                   # Action buttons for labeling
                   fluidRow(
                     column(width = 4,
                            actionButton(ns("applySettings_pca"),
                                         "Apply Labeling",
                                         icon = icon("check"),
                                         width = "100%",
                                         class = "btn-primary")
                     ),
                     column(width = 4,
                            actionButton(ns("resetColors_pca"),
                                         "Reset Colors",
                                         icon = icon("refresh"),
                                         width = "100%",
                                         class = "btn-warning")
                     ),
                     column(width = 4,
                            actionButton(ns("clearLabels_pca"),
                                         "Clear All Labels",
                                         icon = icon("eraser"),
                                         width = "100%",
                                         class = "btn-secondary")
                     )
                   ),
                   br(),

                   # Row 4: Selected Proteins - full width
                   fluidRow(
                     column(width = 12,
                            h5("Selected Proteins & Label Controls:"),
                            div(
                              style = "width: 100%;",
                              uiOutput(ns("enhanced_selectedItems_pca"))
                            )
                     )
                   ),

                   # HORIZONTAL SEPARATOR
                   hr(style = "border-top: 2px solid #dee2e6; margin: 20px 0;"),

                   # LABEL CONTROLS SECTION
                   h5("Label Controls"),

                   # Master Controls
                   fluidRow(
                     column(width = 12,
                            h6("Master Controls:", style = "margin-bottom: 10px; color: #2c3e50;")
                     )
                   ),
                   fluidRow(
                     column(width = 4,
                            colourInput(ns("masterLabelColor_pca"),
                                        "Master Label Color:",
                                        value = "#000000")
                     ),
                     column(width = 4,
                            colourInput(ns("masterDotColor_pca"),
                                        "Master Dot Color:",
                                        value = "#E0E0E0")
                     ),
                     column(width = 4,
                            div(style = "padding-top: 25px;",
                                checkboxInput(ns("masterCustomDot_pca"),
                                              "Enable All Custom Dot Colors",
                                              value = FALSE)
                            )
                     )
                   ),

                   # Labeling Settings
                   br(),
                   fluidRow(
                     column(width = 12,
                            h6("Labeling Settings")
                     )
                   ),
                   fluidRow(
                     column(width = 2,
                            numericInput(ns("maxOverlaps_pca"),
                                         "Max Overlaps:",
                                         value = 10,
                                         min = 1,
                                         max = 1000,
                                         step = 1)
                     ),
                     column(width = 2,
                            numericInput(ns("labelDistance_pca"),
                                         "Label Distance:",
                                         value = 0.25,
                                         min = 0.1,
                                         max = 2.0,
                                         step = 0.05)
                     ),
                     column(width = 2,
                            numericInput(ns("lineThickness_pca"),
                                         "Line Thickness:",
                                         value = 0.5,
                                         min = 0.1,
                                         max = 2.0,
                                         step = 0.1)
                     ),
                     column(width = 2,
                            numericInput(ns("labelSize_pca"),
                                         "Label Size:",
                                         value = 8,
                                         min = 6,
                                         max = 16,
                                         step = 1)
                     ),
                     column(width = 2,
                            numericInput(ns("dotSizeLabeled_pca"),
                                         "Dot Size:",
                                         value = 2,
                                         min = 0.1,
                                         max = 10,
                                         step = 0.1)
                     )
                   )
                 )
               )
             ),

             # Interactive selection panel - ONLY FOR PROTEIN ANALYSIS
             conditionalPanel(
               condition = "input.interactive_plot && input.comparison_target == 'proteins'",
               ns = ns,
               wellPanel(
                 h4(icon("mouse-pointer"), "Interactive Protein Selection"),

                 fluidRow(
                   column(width = 8,
                          h5("Selected Proteins:"),
                          verbatimTextOutput(ns("selected_items_list")),
                          tags$style(type = "text/css", paste0("#", ns("selected_items_list"),
                                                               " {max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; background-color: #f9f9f9;}")),
                          br(),
                          textOutput(ns("selected_items_display"))
                   ),
                   column(width = 4,
                          h5("Actions:"),
                          actionButton(ns("copy_selection"),
                                       "Copy to Clipboard",
                                       icon = icon("copy"),
                                       width = "100%",
                                       class = "btn-outline-primary"),
                          br(), br(),

                          actionButton(ns("clear_selection"),
                                       "Clear Selection",
                                       icon = icon("eraser"),
                                       width = "100%",
                                       class = "btn-outline-secondary")
                   )
                 ),

                 hr(),
                 div(class = "text-muted small",
                     HTML("<strong>Selection Methods:</strong><br/>
                          • <strong>Click:</strong> Select individual protein<br/>
                          • <strong>Box/Lasso tools:</strong> Use toolbar to drag and select multiple<br/>
                          • <strong>Ctrl+Drag:</strong> Hold Ctrl while dragging to add to existing selection"))
               )
             ),

             # SAMPLES MODE LABEL CONTROLS - USING DATAWIZARD PATTERN
             conditionalPanel(
               condition = sprintf("output['%s'] && !input['%s'] && input['%s'] == 'samples'",
                                   ns("plots_ready"),
                                   ns("interactive_plot"),
                                   ns("comparison_target")),

               # Sample Label Controls wellPanel with DataWizard pattern
               wellPanel(
                 style = "margin-bottom: 10px;",
                 tags$div(
                   id = ns("sample_controls_header"),
                   style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
                   onclick = paste0("Shiny.setInputValue('", ns("toggle_sample_controls"), "', Math.random())"),
                   h4(style = "margin: 0; display: inline-block;", "Label Controls"),
                   tags$i(id = ns("sample_controls_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
                 ),
                 div(
                   id = ns("sample_controls_content"),
                   style = "display: none;",

                   # SAMPLES MODE - Global Sample Labeling Controls
                   fluidRow(
                     column(width = 12,
                            h5("Global Sample Labeling Controls:"),
                            p("Settings will be applied to ALL samples in the plot",
                              style = "color: #666; font-style: italic; margin-bottom: 15px;"),

                            fluidRow(
                              column(width = 4,
                                     colourInput(ns("masterLabelColor_samples"),
                                                 "Label Color:",
                                                 value = "#000000")
                              ),
                              column(width = 4,
                                     colourInput(ns("masterDotColor_samples"),
                                                 "Dot Color:",
                                                 value = "#E0E0E0")
                              ),
                              column(width = 4,
                                     div(style = "padding-top: 25px;",
                                         checkboxInput(ns("masterCustomDot_samples"),
                                                       "Enable All Custom Dot Colors",
                                                       value = FALSE)
                                     )
                              )
                            )
                     )
                   ),

                   # Labeling Settings
                   hr(),
                   fluidRow(
                     column(width = 12,
                            h5("Labeling Settings")
                     )
                   ),
                   fluidRow(
                     column(width = 2,
                            numericInput(ns("maxOverlaps_samples"),
                                         "Max Overlaps:",
                                         value = 10,
                                         min = 1,
                                         max = 1000,
                                         step = 1)
                     ),
                     column(width = 2,
                            numericInput(ns("labelDistance_samples"),
                                         "Label Distance:",
                                         value = 0.25,
                                         min = 0.1,
                                         max = 2.0,
                                         step = 0.05)
                     ),
                     column(width = 2,
                            numericInput(ns("lineThickness_samples"),
                                         "Line Thickness:",
                                         value = 0.5,
                                         min = 0.1,
                                         max = 2.0,
                                         step = 0.1)
                     ),
                     column(width = 2,
                            numericInput(ns("labelSize_samples"),
                                         "Label Size:",
                                         value = 8,
                                         min = 6,
                                         max = 16,
                                         step = 1)
                     ),
                     column(width = 2,
                            numericInput(ns("dotSizeLabeled_samples"),
                                         "Dot Size:",
                                         value = 2,
                                         min = 0.1,
                                         max = 10,
                                         step = 0.1)
                     )
                   ),

                   # Action buttons for labeling - SAMPLES MODE
                   hr(),
                   fluidRow(
                     column(width = 4,
                            actionButton(ns("applySampleLabeling_pca"),
                                         "Apply Sample Labeling",
                                         icon = icon("check"),
                                         width = "100%",
                                         class = "btn-primary")
                     ),
                     column(width = 4,
                            actionButton(ns("resetLabelSettings_pca"),
                                         "Reset Label Settings",
                                         icon = icon("refresh"),
                                         width = "100%",
                                         class = "btn-warning")
                     ),
                     column(width = 4,
                            actionButton(ns("clearSampleLabels_pca"),
                                         "Clear Labels",
                                         icon = icon("eraser"),
                                         width = "100%",
                                         class = "btn-secondary")
                     )
                   )
                 )
               )
             ),

             br(),
             wellPanel(
               h4("Download Plot"),
               fluidRow(
                 column(width = 3,
                        numericInput(ns("resolution_DPI_PCATab"),
                                     label = "DPI (where applicable)",
                                     value = 600,
                                     min = 72,
                                     max = 1200,
                                     step = 50,
                                     width = "100%")
                 ),
                 column(width = 3,
                        numericInput(ns("plotWidthInch_PCATab"),
                                     label = "Width (inches)",
                                     value = 12,
                                     min = 4,
                                     max = 20,
                                     step = 0.5,
                                     width = "100%")
                 ),
                 column(width = 3,
                        numericInput(ns("plotHeightInch_PCATab"),
                                     label = "Height (inches)",
                                     value = 8,
                                     min = 4,
                                     max = 20,
                                     step = 0.5,
                                     width = "100%")
                 ),
                 column(width = 3,
                        selectInput(ns("downloadFormat_PCATab"),
                                    label = "Download format",
                                    choices = c("PNG" = "png",
                                                "JPEG" = "jpeg",
                                                "TIFF" = "tiff",
                                                "SVG" = "svg",
                                                "PDF" = "pdf"),
                                    selected = "png",
                                    width = "100%")
                 )
               ),
               br(),
               fluidRow(
                 column(width = 3,
                        div(class = "input-align-bottom",
                            downloadButton(ns("downloadPlotButton_PCATab"), "Download Plot",
                                           width = "100%",
                                           icon = icon("download"))
                        )
                 ),
                 column(width = 3,
                        selectInput(ns("plot_type_grid"),
                                    "Selected plot",
                                    choices = list(
                                      "Main Plot" = "main",
                                      "Scree Plot" = "scree"
                                    ),
                                    selected = "main",
                                    width = "100%")
                 ),
                 column(width = 3,
                        textInput(ns("grid_label"), "Label (optional)", value = "",
                                  width = "100%", placeholder = "Grid label")
                 ),
                 column(width = 3,
                        div(class = "input-align-bottom",
                            actionButton(ns("add_to_grid"), "Add Current Plot to Grid",
                                         class = "btn btn-primary",
                                         width = "100%",
                                         icon = icon("plus"))
                        )
                 )
               ),
               br(),
               div(
                 style = "font-size: 0.9em; color: #6c757d; margin-top: 10px;",
                 textOutput(ns("download_info_PCA"))
               )
             )
      ),

      # Right side - Control panels
      column(width = 3,

             wellPanel(
               fluidRow(
                 column(width = 12,
                        actionButton(ns("create_plot"),
                                     label = "Create Plot",
                                     width = "100%",
                                     class = "btn-primary",
                                     icon = icon("chart-line"))
                 )
               )
             ),
             br(),

             # Data Selection Panel
             wellPanel(
               h4("Data Selection"),

               fluidRow(
                 column(width = 12,
                        selectizeInput(ns("custom_col_sel_pca"),
                                       label = "Data type:",
                                       choices = c(
                                         "Raw Abundance",
                                         "Normalized Abundance",
                                         "Imputed Raw Abundance",
                                         "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
                                         "Batch Corrected Raw Abundance",
                                         "Batch Corrected Normalized Abundance"
                                       ),
                                       selected = "Normalized Abundance",
                                       multiple = FALSE,
                                       options = list(
                                         placeholder = "Select abundance type..."
                                       )
                        )
                 )),
               fluidRow(
                 column(width = 12,
                        selectizeInput(ns("select_samples_pca"),
                                       label = "Select samples:",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = "Select samples for analysis..."
                                       )
                        )
                 )
               ),
               fluidRow(
                 column(width = 12,
                        selectInput(ns("GeneIdentifierColumn_pca"),
                                    label = "Identifier Column:",
                                    choices = NULL,
                                    selected = NULL
                        )
                 )
               )
             ),
             br(),

             # Analysis Configuration Panel
             wellPanel(
               h4("Analysis Configuration"),

               radioButtons(ns("comparison_target"),
                            label = "Compare:",
                            choices = c("Samples" = "samples",
                                        "Proteins" = "proteins"),
                            selected = "samples",
                            inline = TRUE),

               selectInput(ns("analysis_method"),
                           label = "Analysis Method:",
                           choices = c("PCA" = "pca",
                                       "UMAP" = "umap"),
                           selected = "pca",
                           width = "100%"),

               # Method descriptions
               conditionalPanel(
                 condition = "input.analysis_method == 'pca'",
                 ns = ns,
                 div(class = "method-description",
                     HTML("<b>PCA (Principal Component Analysis)</b><br>
                      Linear dimensionality reduction that identifies directions of maximum variance.
                      Best for exploring overall data structure and identifying outliers."))
               ),
               conditionalPanel(
                 condition = "input.analysis_method == 'umap'",
                 ns = ns,
                 div(class = "method-description",
                     HTML("<b>UMAP (Uniform Manifold Approximation and Projection)</b><br>
                      Non-linear method that preserves both local and global structure.
                      Often faster and better at preserving global relationships."))
               )
             ),

             br(),

             # Method-specific parameters

             # PCA Parameters
             conditionalPanel(
               condition = "input.analysis_method == 'pca'",
               ns = ns,
               div(class = "parameter-panel",
                   h5("PCA Parameters"),
                   fluidRow(
                     column(width = 6,
                            selectInput(ns("axis_x"),
                                        label = "X-Axis:",
                                        choices = paste0("PC", 1:10),
                                        selected = "PC1")
                     ),
                     column(width = 6,
                            selectInput(ns("axis_y"),
                                        label = "Y-Axis:",
                                        choices = paste0("PC", 1:10),
                                        selected = "PC2")
                     )
                   ),
                   checkboxInput(ns("show_scree"),
                                 label = "Show Scree Plot",
                                 value = TRUE),
                   checkboxInput(ns("pca_scale"),
                                 label = "Scale data before PCA",
                                 value = TRUE),
                   helpText("When this is ON, very large abundance values are toned down so they do not dominate the plot. For sample PCA, proteins are made more comparable with each other; for protein PCA, selected samples are made more comparable with each other. Turn it OFF if you want the original size differences in your data to influence the PCA more strongly.")
               )
             ),

             # UMAP Parameters
             conditionalPanel(
               condition = "input.analysis_method == 'umap'",
               ns = ns,
               div(class = "parameter-panel",
                   h5("UMAP Parameters"),
                   numericInput(ns("umap_neighbors"),
                                label = "Number of Neighbors:",
                                value = 15,
                                min = 2,
                                max = 100,
                                step = 1),
                   helpText("How many nearby points UMAP looks at. Higher values show broader trends; lower values focus more on small local groups."),
                   br(),
                   numericInput(ns("umap_min_dist"),
                                label = "Minimum Distance:",
                                value = 0.1,
                                min = 0.001,
                                max = 0.99,
                                step = 0.01),
                   helpText("How tightly points may be placed together. Lower values make tighter clusters; higher values spread points out."),
                   br(),
                   selectInput(ns("umap_metric"),
                               label = "Distance Metric:",
                               choices = c("euclidean", "manhattan", "cosine"),
                               selected = "euclidean"),
                   helpText("How UMAP measures similarity between points. Euclidean is a good default; try another metric if groups look unclear.")
               )
             ),
             br(),

             # Plot Styling Panel
             wellPanel(
               h4("Plot Styling"),
               conditionalPanel(
                 condition = "input.comparison_target == 'samples'",
                 ns = ns,
                 selectInput(ns("color_palette"),
                             label = "Color Palette:",
                             choices = c("Set1", "Set2", "Set3", "Dark2",
                                         "Paired", "Accent", "Pastel1", "Pastel2",
                                         "Viridis", "Plasma", "Inferno", "Magma"),
                             selected = "Viridis"),
                 checkboxInput(ns("reverse_colors"),
                               label = "Reverse Colors",
                               value = TRUE)
               ),
               conditionalPanel(
                 condition = "input.comparison_target == 'proteins'",
                 ns = ns,
               fluidRow(
                 column(width = 12,
                        colourInput(ns("defaultProteinColor_pca"),
                                    "Default Protein Color:",
                                    value = "#3182bd")
                 )
               )
               ),
               selectInput(ns("plot_theme"),
                           label = "Plot Theme:",
                           choices = c("Classic" = "theme_classic",
                                       "Minimal" = "theme_minimal",
                                       "Gray" = "theme_gray",
                                       "Black & White" = "theme_bw",
                                       "Light" = "theme_light"),
                           selected = "theme_classic"),
               selectInput(ns("legend_position"),
            label = "Legend Position:",
            choices = c("Right" = "right",
                        "Left" = "left",
                        "Top" = "top",
                        "Bottom" = "bottom",
                        "None" = "none"),
            selected = "right"),
               sliderInput(ns("point_size"),
                           label = "Point Size:",
                           min = 0.5,
                           max = 10,
                           value = 3,
                           step = 0.5),
               fluidRow(
                 column(6,
                       numericInput(ns("AxisTitleSize_PCATab"),
                                    "Size: Axis Title",
                                    value = 20, min = 1, max = 64, step = 1, width = "100%")
                 ),
                 column(6,
                       numericInput(ns("tickSize_PCATab"),
                                    "Size: Ticks",
                                    value = 18, min = 1, max = 64, step = 1, width = "100%")
                 )
               ),
               fluidRow(
                 column(6,
                       numericInput(ns("LegendTitleSize_PCATab"),
                                    "Size: Legend Title",
                                    value = 20, min = 1, max = 64, step = 1, width = "100%")
                 ),
                 column(6,
                       numericInput(ns("LegendTextSize_PCATab"),
                                    "Size: Legend Text",
                                    value = 18, min = 1, max = 64, step = 1, width = "100%")
                 )
               )
             ),
             br(),
             actionButton(ns("resetButton_PCATab"),
                          label = "Reset to Defaults",
                          width = "100%",
                          class = "btn-default",
                          icon = icon("undo"))
      )
    ),

    # Hidden output to trigger conditional panels
    conditionalPanel(
      condition = "false",
      textOutput(ns("plots_ready"))
    ),

    # JavaScript toggle for both labeling panels (pure client-side, no server round-trip)
    pca_panels_toggle_js(ns)
  )
}

# ------------------------------------------------------------------------------
# pca_panels_toggle_js
# Purpose: Inject JavaScript that toggles the protein controls panel and the
#   sample controls panel independently, keeping chevron icons in sync.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny tag containing a script block.
# ------------------------------------------------------------------------------
pca_panels_toggle_js <- function(ns) {
  tags$script(HTML(sprintf("
    (function() {
      // Protein controls panel toggle
      $(document).on('click', '#%s', function(event) {
          event.preventDefault();
          event.stopPropagation();
          var content = $('#%s');
          var icon    = $('#%s');
          if (content.is(':visible')) {
            content.slideUp();
            icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
          } else {
            content.slideDown();
            icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
          }
        });
      // Sample controls panel toggle
      $(document).on('click', '#%s', function(event) {
          event.preventDefault();
          event.stopPropagation();
          var content = $('#%s');
          var icon    = $('#%s');
          if (content.is(':visible')) {
            content.slideUp();
            icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
          } else {
            content.slideDown();
            icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
          }
        });
    })();
  ",
  ns("protein_controls_header"), ns("protein_controls_content"),
  ns("protein_controls_icon"),
  ns("sample_controls_header"),
  ns("sample_controls_content"), ns("sample_controls_icon")
  )))
}
