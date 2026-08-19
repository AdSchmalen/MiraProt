# ========================================
# Heatmap Module - UI Components
# ComplexHeatmap 2x2 Grid Implementation
# ========================================

# All text is in English to comply with the logging & language policy.

create_heatmap_ui <- function(ns) {
  tagList(
    # CSS for layout and small tabs styling
    tags$head(
      tags$style(HTML("
        .heatmap-container { padding: 15px; }
        .heatmap-controls { background-color: #f8f9fa; border-radius: 5px; padding: 15px; margin-bottom: 20px; }
        .heatmap-sidebar { background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; }
        .section-header { font-weight: bold; color: #495057; margin-bottom: 10px; padding-bottom: 5px; border-bottom: 2px solid #dee2e6; }

        /* Small tabs styling */
        .heatmap-tabs .nav-tabs {
          border-bottom: 2px solid #dee2e6;
          margin-bottom: 15px;
        }
        .heatmap-tabs .nav-tabs .nav-link {
          font-size: 12px;
          padding: 6px 12px;
          margin-right: 3px;
          border: 1px solid transparent;
          border-radius: 4px 4px 0 0;
          background-color: #f8f9fa;
          color: #495057;
          transition: all 0.2s ease;
        }
        .heatmap-tabs .nav-tabs .nav-link:hover {
          background-color: #e9ecef;
          border-color: #dee2e6;
        }
        .heatmap-tabs .nav-tabs .nav-link.active {
          background-color: #ffffff;
          border-color: #dee2e6 #dee2e6 #ffffff;
          color: #495057;
          font-weight: 500;
        }
        .heatmap-tabs .tab-content {
          border: none;
          padding: 0;
        }
        .heatmap-plot-container {
          background-color: #ffffff;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          padding: 15px;
          min-height: 700px;
        }
        .heatmap-download-panel {
  background-color: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 5px;
  padding: 0;
}

.heatmap-download-panel .well {
  background-color: #ffffff;
  border: none;
  margin-bottom: 0;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}

.heatmap-download-panel h4 {
  color: #495057;
  font-weight: 600;
  border-bottom: 2px solid #dee2e6;
  padding-bottom: 8px;
  margin-bottom: 15px;
}

#download_status {
  background-color: #e3f2fd;
  border: 1px solid #bbdefb;
  border-radius: 4px;
  color: #1565c0;
  font-size: 0.9em;
  font-weight: 500;
}

.btn-success {
  background-color: #28a745;
  border-color: #28a745;
  transition: all 0.2s ease;
}

.btn-success:hover {
  background-color: #218838;
  border-color: #1e7e34;
  transform: translateY(-1px);
}

/* Colorpicker-Fix: letzter in einer Reihe öffnet nach links */
    .col-sm-4:last-child .colourpicker-panel,
    .col-md-4:last-child .colourpicker-panel {
      right: 0 !important;
      left: auto !important;
    }
      ")),


      tags$script(HTML("
    // Fallback: Colorpicker-Panel bei Überlauf nach links verschieben
    $(document).on('click', '.colourpicker-btn', function() {
      var btn = $(this);
      setTimeout(function() {
        var panel = btn.closest('.colourpicker-container').find('.colourpicker-panel');
        if (panel.length > 0) {
          var offset = panel.offset();
          var panelWidth = panel.outerWidth();
          var windowWidth = $(window).width();
          if (offset.left + panelWidth > windowWidth - 10) {
            panel.css({
              'left': 'auto',
              'right': '0'
            });
          }
        }
      }, 15);
    });
  ")),
      heatmap_protein_panel_toggle_js(ns)
    ),

    fluidRow(
      # ========================================
      # Main Panel (9 columns) - Heatmap Grid
      # ========================================
      column(width = 9,
             div(class = "heatmap-container",

                 div(
                   div(class = "heatmap-tabs",
                       tabsetPanel(
                         id = ns("heatmap_unified_tabs"),
                         type = "tabs",

                         # Abundance + Correlation Grid
                         tabPanel(
                           title = "Abundance + Protein Correlation Grid",
                           value = "grid_expr_corr",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("grid_expr_corr_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("heatmap_grid"), height = "600px", width = "900px")
                               )
                           )
                         ),

                         # Abundance + Sample Grid
                         tabPanel(
                           title = "Abundance + Sample Correlation Grid",
                           value = "grid_expr_sample",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("grid_expr_sample_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("heatmap_expr_sample_grid"), height = "700px", width = "800px")
                               )
                           )
                         ),

                         # Abundance Only
                         tabPanel(
                           title = "Abundance Heatmap",
                           value = "expression_tab",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("expression_only_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("test_expression_only"), height = "600px", width = "800px")
                               )
                           )
                         ),

                         # Protein Correlation Only
                         tabPanel(
                           title = "Protein Correlation Heatmap",
                           value = "protein_cor_tab",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("protein_cor_only_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("test_protein_cor_only"), height = "600px", width = "800px")
                               )
                           )
                         ),

                         # Sample Correlation Only
                         tabPanel(
                           title = "Sample Correlation Heatmap",
                           value = "sample_cor_tab",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("sample_cor_only_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("test_sample_cor_only"), height = "600px", width = "800px")
                               )
                           )
                         ),

                         # Basemean Only
                         tabPanel(
                           title = "Basemean Heatmap",
                           value = "basemean_tab",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("basemean_only_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("test_basemean_only"), height = "600px", width = "800px")
                               )
                           )
                         ),

                         # Abundance Ratio Only
                         tabPanel(
                           title = "Abundance Ratio Heatmap",
                           value = "abundance_ratio_tab",
                           div(class = "heatmap-plot-container",
                               uiOutput(ns("abundance_ratio_only_title")),  # CHANGED: von h4 zu uiOutput
                               div(style = "overflow: auto; max-height: 700px; max-width: 100%;",
                                   plotOutput(ns("test_abundance_ratio_only"), height = "600px", width = "800px")
                               )
                           )
                         )
                       )
                   ),

                   br(),

                   # Protein selection panel
                   fluidRow(
                     column(width = 12,
                            tags$textarea(id = ns("hiddenText_Heatmap"),
                                          style = "display:none;"),
                            wellPanel(
                              style = "margin-bottom: 10px;",
                              tags$div(
                                style = "cursor: pointer; background-color: #f5f5f5; padding: 8px; margin: -15px -15px 10px -15px; border-bottom: 1px solid #ddd;",
                                onclick = paste0("Shiny.setInputValue('", ns("toggle_protein_controls_heatmap"), "', Math.random())"),
                                h4(style = "margin: 0; display: inline-block;", "Protein Selection & Labeling"),
                                tags$i(id = ns("protein_controls_heatmap_icon"), class = "fa fa-chevron-right", style = "float: right; margin-top: 3px;")
                              ),
                              div(
                                id = ns("protein_controls_heatmap_content"),
                                style = "display: none;",

                                # Row 1: Search textarea + Suggested Identifiers (left 50%) + GSEA/GO dropdowns (right 50%)
                                fluidRow(
                                  column(width = 6,
                                         fluidRow(
                                           column(
                                             width = 6,
                                             h6(
                                               textOutput(
                                                 ns("search_identifier_label_Heatmap"),
                                                 inline = TRUE
                                               )
                                             ),
                                             textAreaInput(
                                               ns("input_Heatmap"),
                                               label = NULL,
                                               rows = 5,
                                               width = "100%"
                                             ),
                                             tags$script(
                                               HTML(
                                                 paste0(
                                                   "$(document)",
                                                   ".off('input.heatmapSuggestions', '#",
                                                   ns("input_Heatmap"),
                                                   "')",
                                                   ".on('input.heatmapSuggestions', '#",
                                                   ns("input_Heatmap"),
                                                   "', function() {",
                                                   "Shiny.setInputValue('",
                                                   ns("input_Heatmap"),
                                                   "', this.value, {priority: 'event'});",
                                                   "});"
                                                 )
                                               )
                                             )
                                           ),
                                           column(width = 6,
                                                  h6("Suggested Identifiers:"),
                                                  verbatimTextOutput(ns("geneSymbolList_Heatmap")),
                                                  tags$style(type = "text/css",
                                                             paste0("#", ns("geneSymbolList_Heatmap"),
                                                                    " {height: 120px; overflow-y: scroll;}"))
                                           )
                                         ),
                                         tags$style(type = "text/css",
                                                    paste0("#", ns("input_Heatmap"),
                                                           " {height: 120px !important; resize: vertical;}"))
                                  ),
                                  # PATHWAY SELECTION
                                  column(width = 6,
                                         h6("Import proteins of enriched GSEA gene sets:"),
                                         selectInput(ns("GSEA_Heatmap"),
                                                     label = NULL,
                                                     choices = NULL,
                                                     multiple = TRUE,
                                                     width = "100%"),
                                         h6("Import proteins of enriched GO terms:"),
                                         selectInput(ns("GO_Heatmap"),
                                                     label = NULL,
                                                     choices = NULL,
                                                     multiple = TRUE,
                                                     width = "100%")
                                  )
                                ),
                                # Shared button row across both columns
                                fluidRow(
                                  column(width = 2,
                                         actionButton(ns("transferButton_Heatmap"),
                                                      "Add",
                                                      icon = icon("plus"),
                                                      width = '100%')
                                  ),
                                  column(width = 2,
                                         actionButton(ns("clearButton_Heatmap"),
                                                      "Clear",
                                                      icon = icon("times-circle"),
                                                      width = '100%')
                                  ),
                                  column(width = 2,
                                         actionButton(ns("copyBtn_Heatmap"),
                                                      "Copy to Clipboard",
                                                      icon = icon("clipboard"),
                                                      width = '100%')
                                  ),
                                  column(width = 2,
                                         actionButton(ns("Protein_Input_Heatmap"),
                                                      "Add enriched proteins")
                                  ),
                                  column(width = 2,
                                         checkboxInput(ns("Intersect_Heatmap"),
                                                       label = "Intersecting proteins",
                                                       value = FALSE)
                                  ),
                                  column(width = 2,
                                         checkboxInput(ns("CoreEnriched_Heatmap"),
                                                       label = "Core enriched (GSEA)",
                                                       value = TRUE)
                                  )
                                ),
                                br(),

                                # Action buttons for highlighting - placed above Selected Proteins table
                                fluidRow(
                                  column(width = 3,
                                         actionButton(ns("highlightProteins_Heatmap"),
                                                      "Highlight Proteins",
                                                      icon = icon("bookmark"),
                                                      width = '100%',
                                                      class = "btn-info")
                                  ),
                                  column(width = 3,
                                         actionButton(ns("undoHighlight_Heatmap"),
                                                      "Undo",
                                                      icon = icon("undo"),
                                                      width = '100%',
                                                      class = "btn-secondary")
                                  )
                                ),
                                br(),

                                # Row 4: Selected Proteins - full width
                                fluidRow(
                                  column(width = 12,
                                         h5("Selected Proteins:"),
                                         uiOutput(ns("selectedGene_Heatmap"))
                                  )
                                ),
                                br(),

                                # Legend/label gap - below Selected Proteins, left-aligned
                                fluidRow(
                                  column(width = 3,
                                         numericInput(
                                           ns("legend_plot_gap_heatmap"),
                                           label = "Legend/label gap:",
                                           value = 5,
                                           min = 0,
                                           max = 80,
                                           step = 1,
                                           width = "100%"
                                         )
                                  )
                                )
                              )
                            )
                     )
                   ),


                   br(),

                   # Bottom: Conditional single-plot tabset
                   conditionalPanel(
                     condition = sprintf("output['%s'] == 'true'", ns("singleTabsVisible")),
                     uiOutput(ns("single_plots_tabs"))
                   )
                 ),
                 # Download Panel (layout aligned with Sample IDs download panel)
                 div(style = "margin-top: 20px;",
                     wellPanel(
                       h4("Download Current Tab Plot"),
                       div(
                         style = "color: #6c757d; margin-top: -5px; margin-bottom: 15px; font-size: 0.9em;",
                         textOutput(ns("current_tab_info"), inline = TRUE)
                       ),
                       fluidRow(
                         column(width = 3,
                                numericInput(ns("resolution_DPI_Heatmaps"),
                                             label = "DPI (where applicable)",
                                             value = 300, min = 72, max = 600, step = 50,
                                             width = "100%")
                         ),
                         column(width = 3,
                                numericInput(ns("plotWidthInch_Heatmaps"),
                                             label = "Width (inches)",
                                             value = 12, min = 4, max = 20, step = 0.5,
                                             width = "100%")
                         ),
                         column(width = 3,
                                numericInput(ns("plotHeightInch_Heatmaps"),
                                             label = "Height (inches)",
                                             value = 10, min = 4, max = 20, step = 0.5,
                                             width = "100%")
                         ),
                         column(width = 3,
                                selectInput(ns("downloadFormat_Heatmaps"),
                                            label = "Download format",
                                            choices = c("PDF" = "pdf",
                                                        "PNG" = "png",
                                                        "SVG" = "svg",
                                                        "JPEG" = "jpeg",
                                                        "TIFF" = "tiff"),
                                            selected = "pdf",
                                            width = "100%")
                         )
                       ),
                       br(),
                       fluidRow(
                         column(width = 3,
                                div(class = "input-align-bottom",
                                    downloadButton(ns("downloadPlotButton_Heatmaps"),
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
                         ),
                         column(width = 3)
                       )
                     )
                 )
             )
      ),
      # ========================================
      # Sidebar (3 columns) - Settings
      # ========================================
      column(width = 3,
             div(#class = "heatmap-sidebar",
               wellPanel(
                 fluidRow(
                   column(width = 12,
                          actionButton(ns("create_heatmap_btn"),
                                       "Create Plot",
                                       class = "btn-primary",
                                       width = "100%",
                                       icon = icon("chart-line"))
                   )
                 )
               ),
               br(),

               # ========================================
               # Data Selection
               # ========================================
               wellPanel(
                 div(class = "section-header", "Data Selection & Sorting"),

                 # KEEP EXISTING ID: custom_col_sel_heatmap
                 selectizeInput(ns("custom_col_sel_heatmap"),
                                label = "Select data type:",
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
                 ),

                 # Select Samples
                 selectizeInput(ns("select_samples_heatmap"),
                                label = "Select samples:",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(
                                  placeholder = "Select samples for analysis..."
                                )
                 ),


                 selectInput(ns("GeneIdentifierColumn_Heatmap"),
                             label = "Gene identifier column:",
                             choices = NULL,
                             selected = NULL
                 ),

                 # Sample ordering (independent from protein ordering)
                 selectInput(ns("sort_samples_by"),
                             label = "Sort samples by:",
                             choices = c(
                               "None (keep selection order)"           = "none",
                               "Alphabetical (A → Z)"                  = "alpha_asc",
                               "Alphabetical (Z → A)"                  = "alpha_desc",
                               "Pearson clustering (1 - r, average linkage)" = "pearson_cluster",
                               "Distance clustering (Euclidean)" = "distance_cluster",
                               "Principial Component 1 (ascending)"      = "pca1_asc",
                               "Principial Component 1 (descending)"     = "pca1_desc",
                               "Principial Component 2 (ascending)"      = "pca2_asc",
                               "Principial Component 2 (descending)"     = "pca2_desc"
                             ),
                             selected = "distance_cluster"
                 ),

                 selectInput(ns("sort_proteins_by"),
                             label = "Sort proteins by:",
                             choices = c(
                               "Z-Score" = "z_score",
                               "Pearson r" = "pearson_r",
                               "Custom" = "custom"
                             ),
                             selected = "z_score",
                             multiple = FALSE
                 ),

                 # Dynamic info field explaining the sorting method
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'z_score'", ns("sort_proteins_by")),
                   div(
                     style = "background-color: #e8f4fd; border: 1px solid #b8daff; border-radius: 0.25rem; padding: 8px; margin: 5px 0; font-size: 0.85em;",
                     HTML("<i class='fa fa-info-circle'></i> <strong>Z-Score Sorting:</strong> Proteins are ordered based on hierarchical clustering of their normalized abundance values (Z-scores). Similar abundance patterns are grouped together.")
                   )
                 ),

                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'pearson_r'", ns("sort_proteins_by")),
                   div(
                     style = "background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 0.25rem; padding: 8px; margin: 5px 0; font-size: 0.85em;",
                     HTML("<i class='fa fa-info-circle'></i> <strong>Pearson r Sorting:</strong> Proteins are ordered based on hierarchical clustering of their correlation coefficients. Proteins with similar correlation profiles across samples are grouped together.")
                   )
                 ),

                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'custom'", ns("sort_proteins_by")),
                   textAreaInput(ns("custom_protein_order"),
                                 label = "Custom protein order (comma/line separated):",
                                 placeholder = "Protein1, Protein2\nProtein3",
                                 rows = 4,
                                 width = "100%"),
                   selectInput(ns("custom_protein_fallback_sort"),
                               label = "Fallback sorting for remaining proteins",
                               choices = c(
                                 "Z-score" = "z_score",
                                 "Pearson r" = "pearson_r"
                               ),
                               selected = "z_score",
                               multiple = FALSE),
                   div(
                     style = "background-color: #e8f4fd; border: 1px solid #b8daff; border-radius: 0.25rem; padding: 8px; margin: 5px 0; font-size: 0.85em;",
                     HTML("<strong>Custom Sorting:</strong> Matching proteins are placed first in the entered order; remaining proteins use the selected fallback sorting method.")
                   )
                 ),

                 checkboxInput(ns("skip_log_transform_heatmap"),
                               label = "Skip log2 transform before Z-score",
                               value = FALSE
                 ),
                 div(
                   style = "color: #6c757d; font-size: 0.85em; margin-top: -6px; margin-bottom: 8px;",
                   "Uses metadata re-transformation first, then calculates Z-scores on the unlogged abundance scale."
                 )
               ),

               br(),

               # ========================================
               # Filtering Options
               # ========================================
               wellPanel(
                 div(class = "section-header", "Filtering Options"),

                 # Data Quality
                 div(class = "section-header", "Data Quality"),

                 # NA filter checkbox with informative label
                 checkboxInput(ns("remove_na_abundance_heatmap"),
                               label = HTML("Remove proteins with missing abundance values<br><small style='color: #6c757d;'>Removes rows where any selected sample has NA/missing data</small>"),
                               value = FALSE  # Default enabled for better data quality
                 ),

                 # Missing value tile color (only relevant when NA rows are retained)
                 conditionalPanel(
                   condition = sprintf("input['%s'] == false", ns("remove_na_abundance_heatmap")),
                   colourpicker::colourInput(
                     ns("missing_value_color_heatmap"),
                     "Missing value tile color:",
                     value = "#E0E0E0"
                   )
                 ),

                 numericInput(ns("min_abundance_values_per_row_heatmap"),
                              label = "Filter rows with less abundance values than:",
                              value = 1,
                              min = 1,
                              max = 1,
                              step = 1
                 ),

                 # Optional: Show NA count information
                 conditionalPanel(
                   condition = sprintf("input['%s'] == true", ns("remove_na_abundance_heatmap")),
                   div(
                     id = ns("na_filter_info"),
                     style = "background-color: #e7f3ff; border: 1px solid #b8daff; border-radius: 0.25rem; padding: 8px; margin: 5px 0; font-size: 0.9em;",
                     HTML("<i class='fa fa-info-circle'></i> <strong>Info:</strong> Proteins with missing abundance data in any selected sample will be excluded before other filters are applied.")
                   )
                 ),

                 br(),

                 div(class = "section-header", "Statistical Filter"),

                 # P-value filter enable/disable
                 div(
                   id = ns("pvalue_filter_container"),
                   checkboxInput(ns("enable_pvalue_filter_heatmap"),
                                 label = "Enable p-value filtering",
                                 value = TRUE  # Default enabled, will be auto-disabled if no p-values
                   ),

                   # Status message when no p-values available
                   div(
                     id = ns("pvalue_unavailable_message"),
                     style = "display: none; color: #dc3545; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 0.25rem; padding: 8px; margin-top: 5px; margin-bottom: 10px; font-size: 0.9em;",
                     HTML("<strong>⚠ No P-values available.</strong> Statistical filtering is disabled. Proteins will be selected randomly.")
                   )
                 ),

                 # P-Value Type selection (existing, but now in conditional panel)
                 conditionalPanel(
                   condition = sprintf("input['%s'] == true", ns("enable_pvalue_filter_heatmap")),
                   selectInput(ns("pval_type_heatmap"),
                               label = "P-Value type:",
                               choices = NULL,
                               selected = NULL
                   ),

                   # P-Value Column selection (existing, but now in conditional panel)
                   selectInput(ns("pval_col_heatmap"),
                               label = "P-Value column:",
                               choices = NULL,
                               selected = NULL
                   ),

                   # P-Value threshold (existing, but now in conditional panel)
                   numericInput(ns("pval_threshold_heatmap"),
                                label = "P-Value threshold (≤):",
                                value = 0.05,
                                min = 0.0000000000001,
                                max = 1
                   )
                 ),

                 br(),

                 # Ratio Filter Section
                 div(class = "section-header", "Ratio Filter"),

                 # Abundance Ratio Column selection
                 selectInput(ns("abundance_ratio_col_heatmap"),
                             label = "Abundance Ratio column:",
                             choices = NULL,
                             selected = NULL
                 ),

                 # Enhanced ratio filter container with dynamic state management
                 div(
                   id = ns("ratio_filter_container"),

                   # Main checkbox for enabling ratio filter
                   checkboxInput(ns("enable_ratio_filter_heatmap"),
                                 label = "Enable ratio filter",
                                 value = FALSE
                   ),

                   # Warning message when no ratio data is available (initially hidden)
                   div(
                     id = ns("ratio_unavailable_message"),
                     style = "display: none; color: #dc3545; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 0.25rem; padding: 8px; margin-top: 5px; margin-bottom: 10px; font-size: 0.9em;",
                     HTML("<strong>No Abundance Ratio available.</strong> Please review your data and ensure ratio columns are properly configured.")
                   ),

                   # Conditional panel for ratio filter controls (only shown when enabled)
                   conditionalPanel(
                     condition = sprintf("input['%s'] == true", ns("enable_ratio_filter_heatmap")),

                     # Enhanced filter mode selection with clear descriptions
                     selectInput(ns("ratio_filter_mode_heatmap"),
                                 label = "Filter mode:",
                                 choices = c(
                                   "Show proteins with large changes: |log₂(ratio)| > threshold" = "abs_log2_gt",
                                   "Show proteins with small changes: |log₂(ratio)| < threshold" = "abs_log2_lt"
                                 ),
                                 selected = "abs_log2_gt"
                     ),

                     # Threshold input with helpful constraints
                     numericInput(ns("ratio_threshold_heatmap"),
                                  label = "log₂ ratio threshold:",
                                  value = 1.0,
                                  min = 0,
                                  max = 10,
                                  step = 0.1
                     )
                   )
                 ),

                 br(),

                 div(class = "section-header", "Identifier Filter"),

                 textAreaInput(ns("custom_proteins_filter"),
                               label = "Enter proteins (comma/line separated):",
                               placeholder = "Protein1, Protein2\nProtein3",
                               rows = 4,
                               width = "100%"
                 ),

                 selectInput(ns("GSEA_IdentifierFilter_Heatmap"),
                             label = "Enriched GSEA gene sets",
                             choices = NULL, multiple = TRUE, width = "100%"),
                 selectInput(ns("GO_IdentifierFilter_Heatmap"),
                             label = "Enriched GO terms",
                             choices = NULL, multiple = TRUE, width = "100%"),
                 fluidRow(
                   column(width = 6,
                          checkboxInput(ns("Intersect_IdentifierFilter_Heatmap"),
                                        "Intersecting proteins", FALSE)),
                   column(width = 6,
                          checkboxInput(ns("CoreEnriched_IdentifierFilter_Heatmap"),
                                        "Core enriched (GSEA)", TRUE))
                 ),
                 fluidRow(
                   column(width = 12,
                          actionButton(ns("Protein_Input_IdentifierFilter_Heatmap"),
                                       "Add enriched proteins", width = "100%"))
                 ),

                 br(),

                 # Enhanced Max proteins input with better description
                 numericInput(ns("max_proteins_heatmap"),
                              label = HTML("Max proteins to display:<br><small style='color: #6c757d;'>Random selection if no p-values available</small>"),
                              value = 50,
                              min = 10,
                              max = 500,
                              step = 10
                 )
               ),

               br(),

               # ========================================
               # Styling
               # ========================================
               wellPanel(
                 div(class = "section-header", "Styling"),

                 # Color Settings
                 div(class = "section-header", "Color Palette"),

                 fluidRow(
                   column(width = 4,
                          colourpicker::colourInput(ns("Heatmap_ColorInput_1"),
                                                    "Low",
                                                    value = "#440154FF")
                   ),
                   column(width = 4,
                          colourpicker::colourInput(ns("Heatmap_ColorInput_2"),
                                                    "Mid",
                                                    value = "white")
                   ),
                   column(width = 4,
                          colourpicker::colourInput(ns("Heatmap_ColorInput_3"),
                                                    "High",
                                                    value = "#EFC000FF")
                   )
                 ),

                 br(),

                 # Contrast Setting
                 div(class = "section-header", "Contrast Setting"),
                 checkboxInput(ns("correlation_enhanced_contrast"),
                               "Enhanced contrast for correlation",
                               value = TRUE
                 ),

                 br(),

                 # Abundance Heatmap
                 div(class = "section-header", "Abundance Heatmap Labels"),
                 fluidRow(
                   column(width = 6,
                          checkboxInput(ns("show_expr_row_labels"),
                                        "Show Row Labels",
                                        value = TRUE
                          )
                   ),
                   column(width = 6,
                          checkboxInput(ns("show_expr_col_labels"),
                                        "Show Column Labels",
                                        value = TRUE
                          )
                   )
                 ),

                 # Correlation Heatmap
                 div(class = "section-header", "Protein Correlation Labels"),
                 fluidRow(
                   column(width = 6,
                          checkboxInput(ns("show_corr_row_labels"),
                                        "Show Row Labels",
                                        value = FALSE
                          )
                   ),
                   column(width = 6,
                          checkboxInput(ns("show_corr_col_labels"),
                                        "Show Column Labels",
                                        value = TRUE
                          )
                   )
                 ),

                 # Sample Correlation
                 div(class = "section-header", "Sample Correlation Labels"),
                 fluidRow(
                   column(width = 6,
                          checkboxInput(ns("show_sample_row_labels"),
                                        "Show Row Labels",
                                        value = TRUE
                          )
                   ),
                   column(width = 6,
                          checkboxInput(ns("show_sample_col_labels"),
                                        "Show Column Labels",
                                        value = TRUE
                          )
                   )
                 ),

                 # Dendrograms
                 div(class = "section-header", "Dendrograms"),
                 fluidRow(
                   column(width = 6,
                          checkboxInput(ns("show_row_dendrogram"),
                                        "Show row dendrogram",
                                        value = TRUE
                          )
                   ),
                   column(width = 6,
                          checkboxInput(ns("show_column_dendrogram"),
                                        "Show column dendrogram",
                                        value = TRUE
                          )
                   )
                 ),

                 br(),

                 # Fonts
                 div(class = "section-header", "Fonts"),
                 fluidRow(
                   column(width = 6,
                          numericInput(ns("row_font_size"),
                                       "Row font size:",
                                       value = 10,
                                       min = 1,
                                       max = 64,
                                       step = 1
                          )
                   ),
                   column(width = 6,
                          numericInput(ns("col_font_size"),
                                       "Column font size:",
                                       value = 10,
                                       min = 1,
                                       max = 64,
                                       step = 1
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 6,
                          numericInput(ns("legend_title_font_size"),
                                       "Legend title size:",
                                       value = 11, min = 6, max = 64, step = 1)),
                   column(width = 6,
                          numericInput(ns("legend_text_font_size"),
                                       "Legend text size:",
                                       value = 9, min = 6, max = 64, step = 1))
                 ),
                 checkboxInput(ns("hideTitle_Heatmap"),
                               "Hide Title",
                               value = TRUE),
                 br(),
                 # Legend Position
                 div(class = "section-header", "Legend Position"),
                 fluidRow(
                   column(width = 12,
                          selectInput(ns("legend_position"),
                                      "Legend position for all plots:",
                                      choices = list(
                                        "Right" = "right",
                                        "Left" = "left",
                                        "Top" = "top",
                                        "Bottom" = "bottom"
                                      ),
                                      selected = "right",
                                      width = "100%"
                          )
                   )
                 )
               ),

               br(),

               # ========================================
               # Extensions
               # ========================================
               wellPanel(
                 div(class = "section-header", "Extensions"),

                 # Correlation diagonal line options
                 checkboxInput(ns("show_correlation_diagonal"),
                               label = "Show diagonal line in correlation heatmaps",
                               value = TRUE
                 ),

                 # Conditional panel for diagonal line controls
                 conditionalPanel(
                   condition = sprintf("input['%s'] == true", ns("show_correlation_diagonal")),

                   div(style = "margin-left: 20px; margin-top: 10px;",

                       # Color picker for diagonal line
                       fluidRow(
                         column(width = 4,
                                colourpicker::colourInput(ns("diagonal_line_color"),
                                                          "Line color:",
                                                          value = "#7A7A7A"
                                )
                         ),
                         column(width = 4,
                                # Line width control
                                numericInput(ns("diagonal_line_width"),
                                             label = "Line width:",
                                             value = 2,
                                             min = 0.5,
                                             max = 10,
                                             step = 0.5
                                )
                         ),
                         column(width = 4,
                                checkboxInput(ns("diagonal_rotate"),
                                              label = "Rotate",
                                              value = FALSE
                                )
                         )
                       ),

                       # Helper text
                       div(
                         style = "color: #6c757d; font-size: 0.85em; margin-top: 5px;",
                         "Highlights the main diagonal where proteins/samples correlate with themselves (r = 1.0)"
                       )
                   )
                 ),

                 br(),

                 # Basemean Heatmap
                 checkboxInput(ns("show_basemean_heatmap"),
                               "Show Basemean Heatmap",
                               value = FALSE
                 ),

                 # Conditional basemean label controls
                 conditionalPanel(
                   condition = paste0("input['", ns("show_basemean_heatmap"), "']"),
                   div(style = "margin-left: 20px; margin-top: 5px; margin-bottom: 10px;",
                       checkboxInput(ns("show_basemean_row_labels"),
                                     "Show Basemean Row Labels",
                                     value = FALSE
                       ),
                       checkboxInput(ns("show_basemean_col_labels"),
                                     "Show Basemean Column Labels",
                                     value = TRUE
                       )
                   )
                 ),

                 # Enhanced abundance ratio heatmap with dynamic disabling
                 div(
                   id = ns("abundance_ratio_heatmap_container"),
                   checkboxInput(
                     ns("show_abundance_ratio_heatmap"),
                     HTML("Show log<sub>2</sub> Abundance Ratio Heatmap"),
                     value = FALSE
                   ),

                   # Warning message when no abundance ratio available
                   div(
                     id = ns("abundance_ratio_heatmap_unavailable_message"),
                     style = "display: none; color: #dc3545; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 0.25rem; padding: 8px; margin-top: 5px; margin-bottom: 10px; font-size: 0.9em;",
                     HTML("<strong>⚠ No Abundance Ratio available.</strong> Cannot display ratio heatmap. Please review your data.")
                   )
                 ),

                 # Conditional abundance ratio label controls
                 conditionalPanel(
                   condition = paste0(
                     "input['",
                     ns("show_abundance_ratio_heatmap"),
                     "']"
                   ),
                   div(
                     style = "margin-left: 20px; margin-top: 5px; margin-bottom: 10px;",

                     selectInput(
                       ns("abundance_ratio_col_extension_heatmap"),
                       "Abundance Ratio column for heatmap:",
                       choices = NULL,
                       selected = NULL,
                       width = "100%"
                     ),

                     checkboxInput(
                       ns("show_abundance_ratio_row_labels"),
                       "Show Abundance Ratio Row Labels",
                       value = FALSE
                     ),

                     checkboxInput(
                       ns("show_abundance_ratio_col_labels"),
                       "Show Abundance Ratio Column Labels",
                       value = TRUE
                     )
                   )
                 )
               )
             ),
             br(),
             actionButton(ns("reset_heatmap_btn"),
                          label = "Reset to Defaults",
                          width = "100%",
                          class = "btn-default",
                          icon = icon("undo"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# heatmap_protein_panel_toggle_js
# Purpose: Toggle the Heatmap protein selection and labeling panel using the
#   same expandable DataWizard-style pattern used in the PCA module.
# ------------------------------------------------------------------------------
heatmap_protein_panel_toggle_js <- function(ns) {
  tags$script(HTML(sprintf("
    (function() {
      $(document).on('click',
        '#%s, div[onclick*=\"%s\"]',
        function() {
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
  ns("protein_controls_heatmap_icon"), ns("toggle_protein_controls_heatmap"),
  ns("protein_controls_heatmap_content"), ns("protein_controls_heatmap_icon")
  )))
}
