# GSEA_ui.R
# Enhanced UI definition for GSEA module

gsea_gene_set_file_control_ui <- function(ns, files, selected = NULL) {
  if (length(files) > 0L) {
    return(selectInput(
      ns("fileSelector_GSEA"),
      "Select Gene Set File:",
      choices = files,
      selected = selected,
      width = "100%"
    ))
  }

  div(
    class = "alert alert-info",
    role = "status",
    tags$strong("No gene set files are available."),
    tags$p(
      "GSEA requires one or more files in GMT (Gene Matrix Transposed) format. ",
      "Create your own GMT files or download gene set collections from ",
      tags$a(
        "MSigDB",
        href = "https://www.gsea-msigdb.org/gsea/msigdb/",
        target = "_blank",
        rel = "noopener noreferrer"
      ),
      "."
    ),
    tags$p(
      "Place the .gmt files in ", tags$code("./GSEA/"),
      " for a standard installation, or in ",
      tags$code("./shiny-app/GSEA/"), " for the portable version."
    ),
    tags$p("After adding the files, click “Refresh Gene Sets” below.")
  )
}

GSEA_ui_definition <- function(ns) {
  div(
    fluidRow(
      # Left Panel: Configuration
      column(
        width = 9,
        # -------------------------------------------------------------------------
        # GSEA Export / Import Panel (aligned with GO module style)
        # -------------------------------------------------------------------------



          # Multi-pathway selection
          selectizeInput(
            ns("custom_Enrich_select"),
            "Select Gene Sets:",
            choices = NULL,
            multiple = TRUE,  # Enable multiple selection
            width = "100%",
            options = list(
              plugins = list('remove_button'),
              closeAfterSelect = FALSE,
              placeholder = "Select one or more gene sets..."
            )
          ),

          # Plot type selection
          selectInput(
            ns("plot_type_GSEA"),
            "Select Plot Type:",
            choices = c(
              "General Running Score Plot",
              "Enrichment score dotplot",
              "Cnet plot (log2FC)",
              "Cnet plot (Ranking Metrics)",
              "Enrichment map",
              "Heatmap (log2FC)",
              "Heatmap (Ranking Metrics)",
              "Ridgeline plot",
              "Running score plot",
              "Pubmed citations"
            ),
            selected = "General Running Score Plot",
            width = "100%"
          ),

          # Create plot button
          fluidRow(
            column(
              width = 3,
              actionButton(
                ns("create_gsea_plot"),
                "Create Plot",
                icon = icon("chart-line"),
                class = "btn btn-primary btn-block",
                width = "100%"
              )
            )
          ),
        br(),

        # Plot Customization Panel (conditional)
        conditionalPanel(
          condition = paste0("input['", ns("plot_type_GSEA"), "'] != ''"),

          wellPanel(
            h5("Plot Customization"),

            # Color inputs for gradient plots - ADD 'Enrichment score dotplot' to the condition
            conditionalPanel(
              condition = paste0("['Cnet plot (log2FC)', 'Heatmap (Ranking Metrics)', 'Cnet plot (Ranking Metrics)', 'Ridgeline plot','Heatmap (log2FC)', 'Enrichment map', 'Enrichment score dotplot', 'Running score plot', 'Pubmed citations'].indexOf(input['", ns("plot_type_GSEA"), "']) >= 0"),
              fluidRow(
                column(width = 4,
                       colourpicker::colourInput(ns("GSEAColorInput_down"), "Color 1 (Low)", value = '#440154FF')),
                column(width = 4,
                       colourpicker::colourInput(ns("GSEAColorInput_zero"), "Color 2 (Medium)", value = '#31688EFF')),
                column(width = 4,
                       colourpicker::colourInput(ns("GSEAColorInput_up"), "Color 3 (High)", value = '#EFC000FF'))
              )
            ),

            # Font size controls
            fluidRow(
              column(width = 3,
                     numericInput(ns("AxisTitleSize_GSEA"), "Axis Title Size",
                                  value = 12, min = 8, max = 24, width = "100%")),
              column(width = 3,
                     numericInput(ns("tickSize_GSEA"), "Tick Size",
                                  value = 10, min = 6, max = 20, width = "100%")),
              column(width = 3,
                     numericInput(ns("LegendTextSize_GSEA"), "Legend Text Size",
                                  value = 10, min = 6, max = 20, width = "100%")),
              column(width = 3,
                     numericInput(ns("LegendTitleSize_GSEA"), "Legend Title Size",
                                  value = 12, min = 8, max = 24, width = "100%"))
            ),

            # Theme selection
            fluidRow(
              column(width = 3,
                     selectInput(ns("ThemeSelect_GSEA"), "Plot Theme",
                                 choices = c("Gray", "Black and White", "Linedraw",
                                             "Light", "Dark", "Minimal", "Classic", "Void"),
                                 selected = "Black and White", width = "100%")),
              column(width = 3,
                     selectInput(
                       ns("LegendPosition_GSEA"),
                       "Legend Position",
                       choices = c("Right" = "right",
                                   "Left" = "left",
                                   "Top" = "top",
                                   "Bottom" = "bottom",
                                   "None" = "none"),
                       selected = "right",
                       width = "100%"
                     )),
              column(width = 3,
                     numericInput(ns("LabelSize_GSEA"), "Label Size",
                                  value = 12, min = 8, max = 20, width = "100%")),
              column(
                width = 3,
                # Plot height control
                numericInput(
                  ns("plot_height_gsea"),
                  "Height (px):",
                  value = 600,
                  min = 300,
                  max = 1200,
                  step = 50,
                  width = "100%"
                )
              )
            ),

            # Plot-specific customizations
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_GSEA"), "'] === 'Cnet plot (log2FC)' || input['", ns("plot_type_GSEA"), "'] === 'Cnet plot (Ranking Metrics)'"),
              fluidRow(
                column(width = 6,
                       numericInput(ns("cnet_node_size"), "Node Size",
                                    value = 5, min = 1, max = 15, width = "100%")),
                column(width = 6,
                       numericInput(ns("cnet_layout_method"), "Layout Spread",
                                    value = 1, min = 0.5, max = 3, step = 0.1, width = "100%"))
              )
            ),

            conditionalPanel(
              condition = paste0("input['", ns("plot_type_GSEA"), "'] === 'Enrichment map'"),
              fluidRow(
                column(width = 6,
                       numericInput(ns("emap_node_size"), "Node Size",
                                    value = 8, min = 2, max = 20, width = "100%")),
                column(width = 6,
                       numericInput(ns("emap_layout"), "Layout Parameter",
                                    value = 0.7, min = 0.1, max = 1.5, step = 0.1, width = "100%"))
              )
            ),

            conditionalPanel(
              condition = paste0("input['", ns("plot_type_GSEA"), "'] === 'Enrichment score dotplot'"),
              fluidRow(
                column(
                  width = 6,
                  checkboxInput(
                    ns("dotplot_swap_panels"),
                    "Positively Enriched first",
                    value = FALSE
                  )
                ),
                column(
                  width = 6,
                  checkboxInput(
                    ns("dotplot_y_ticks_right"),
                    "Show Y-axis tick labels on the right",
                    value = FALSE
                  )
                )
              )
            )
          )
        ),

        # Plot Display Area
        div(
          id = ns("plot_display_area"),
          uiOutput(ns("GSEAplot_container"))
        ),

        br(),

        # Results tabs
        conditionalPanel(
          condition = paste0("output['", ns("res_GSEA_ready"), "'] == true"),

          tabsetPanel(
            id = ns("gsea_results_tabs"),
            type = "tabs",

            # Enrichment Plot Tab
            tabPanel(
              title = "Enrichment Plot",
              value = "enrichment_plot",

              wellPanel(
                h4("Download GSEA Enrichment Plot"),

                # # Plot output
                # plotOutput(
                #   ns("gsea_enrichment_plot"),
                #   height = "auto"
                # ),

                br(),

                # Download controls
                fluidRow(
                  column(width = 3,
                         numericInput(ns("resolution_DPI_GSEA"),
                                      label = "DPI (where applicable)",
                                      value = 300,
                                      min = 72,
                                      max = 1200,
                                      step = 50,
                                      width = "100%")
                  ),
                  column(width = 3,
                         numericInput(ns("plotWidthInch_GSEA"),
                                      label = "Width (inches)",
                                      value = 10,
                                      min = 2,
                                      max = 50,
                                      step = 0.5,
                                      width = "100%")
                  ),
                  column(width = 3,
                         numericInput(ns("plotHeightInch_GSEA"),
                                      label = "Height (inches)",
                                      value = 8,
                                      min = 2,
                                      max = 50,
                                      step = 0.5,
                                      width = "100%")
                  ),
                  column(width = 3,
                         selectInput(ns("downloadFormat_GSEA"),
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
                             downloadButton(ns("downloadPlotButton_GSEA"),
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
                # Download information display
                fluidRow(
                  column(width = 12,
                         div(style = "font-size: 12px; color: #666;",
                             textOutput(ns("download_info_GSEA")))
                  )
                )
                # column(
                #   width = 3,
                #   downloadButton(
                #     ns("download_enrichment_plot_png"),
                #     "Download PNG",
                #     icon = icon("download"),
                #     class = "btn-secondary",
                #     width = "100%"
                #   )
                # ),
                # column(
                #   width = 3,
                #   downloadButton(
                #     ns("download_enrichment_plot_pdf"),
                #     "Download PDF",
                #     icon = icon("download"),
                #     class = "btn-secondary",
                #     width = "100%"
                #   )
                # ),
                # column(
                #   width = 3,
                #   downloadButton(
                #     ns("download_enrichment_plot_svg"),
                #     "Download SVG",
                #     icon = icon("download"),
                #     class = "btn-secondary",
                #     width = "100%"
                #   )
                # )

              )
            ),

            # Results Table Tab
            tabPanel(
              title = "Results Table",
              value = "results_table",

              wellPanel(
                h4("Download GSEA Results Table"),

                # Table output
                DT::DTOutput(ns("gsea_results_table")),

                br(),

                # Download buttons
                fluidRow(
                  column(
                    width = 3,
                    downloadButton(
                      ns("download_results_csv"),
                      "Download CSV",
                      icon = icon("download"),
                      class = "btn-secondary",
                      width = "100%"
                    )
                  ),
                  column(
                    width = 3,
                    downloadButton(
                      ns("download_results_xlsx"),
                      "Download XLSX",
                      icon = icon("download"),
                      class = "btn-secondary",
                      width = "100%"
                    )
                  )
                )
              )
            )#,

            # # Gene List Tab
            # tabPanel(
            #   title = "Gene Rankings",
            #   value = "gene_rankings",
            #
            #   wellPanel(
            #     h4("Download Gene Ranking List"),
            #
            #     helpText("Top 50 genes by ranking score:"),
            #
            #     # Gene rankings table
            #     DT::DTOutput(ns("gene_rankings_table")),
            #
            #     br(),
            #
            #     downloadButton(
            #       ns("download_gene_rankings"),
            #       "Download Full Rankings",
            #       icon = icon("download"),
            #       class = "btn-secondary"
            #     )
            #   )
            # ),
            #
            # # Leading Edge Tab
            # tabPanel(
            #   title = "Leading Edge",
            #   value = "leading_edge",
            #
            #   wellPanel(
            #     h4("Download Leading Edge Genes"),
            #
            #     helpText("Genes contributing most to the enrichment signal:"),
            #
            #     # Leading edge genes
            #     DT::DTOutput(ns("leading_edge_table")),
            #
            #     br(),
            #
            #     downloadButton(
            #       ns("download_leading_edge"),
            #       "Download Leading Edge",
            #       icon = icon("download"),
            #       class = "btn-secondary"
            #     )
            #   )
            # )
          )
        ),
        # Results Selection
        conditionalPanel(
          condition = paste0("input['", ns("createGSEA"), "'] > 0")
        ),

        br(),

        wellPanel(
          h4("Export or Import GSEA Results"),
          p("Save your current GSEA analysis as an RDS file or import previously saved results.",
            style = "font-size: 0.9em; color: #666; margin-bottom: 15px;"),

          # --- Controls row: Download + File input ---
          fluidRow(
            column(
              width = 6,
              div(
                class = "form-group",
                tags$label("Export current GSEA results:"),
                br(),
                uiOutput(ns("download_res_gsea_ui"))
              )
            ),
            column(
              width = 6,
              div(
                class = "form-group",
                tags$label("Import saved GSEA results:"),
                fileInput(
                  ns("gsea_import_file"),
                  label = NULL,
                  accept = c(".rds"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected",
                  width = "100%"
                )
              )
            )
          ),

          # --- Description texts row: aligned perfectly ---
          fluidRow(
            column(
              width = 6,
              p("Exports the complete GSEA object, including all results and metadata.",
                style = "font-size: 0.8em; color: #777; margin-top: 5px; margin-bottom: 10px;")
            ),
            column(
              width = 6,
              p("Select an exported .rds file to restore a previous GSEA session.",
                style = "font-size: 0.8em; color: #777; margin-top: 5px; margin-bottom: 10px;")
            )
          ),

          # --- Status area below both sections ---
          fluidRow(
            column(
              width = 12,
              uiOutput(ns("gsea_import_controls"))
            )
          )
        )
      ),

      # Right panel
      column(
        width = 3,

        wellPanel(
          fluidRow(
            column(width = 12,
                   actionButton(
                     ns("createGSEA"),
                     "Run GSEA Analysis",
                     width = "100%",
                     class = "btn-success",
                     style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
                     icon = icon("chart-line"
                     )
                   )
            )
          )
        ),
        br(),

        wellPanel(
          h3("GSEA Configuration"),

          # Gene Set Selection
          uiOutput(ns("geneSetFileControl_GSEA")),

          actionButton(
            ns("refresh_GeneSets"),
            "Refresh Gene Sets",
            icon = icon("refresh"),
            width = "100%",
            class = "btn-secondary"
          ),

          br(), br(),

          helpText("Gene set collections:",
                   br(),
                   strong("C1:"), "Positional gene sets",
                   br(),
                   strong("C2:"), "Curated gene sets",
                   br(),
                   strong("C3:"), "Regulatory target gene sets",
                   br(),
                   strong("C4:"), "Computational gene sets",
                   br(),
                   strong("C5:"), "Ontology gene sets",
                   br(),
                   strong("C6:"), "Oncogenic signature gene sets",
                   br(),
                   strong("C7:"), "Immunologic signature gene sets",
                   br(),
                   strong("C8:"), "Cell type signature gene sets",
                   br(),
                   strong("H:"), "Hallmark gene sets"),

          hr(),

          # Ranking Type Selection
          radioButtons(
            ns("GSEA_type_select"),
            "Ranking Method:",
            choices = c("Custom Ranking", "Precalculated Ranking"),
            selected = "Custom Ranking"
          ),

          hr(),

          # Identifier Selection
          selectInput(
            ns("Identifier_GSEA"),
            "Gene Identifier Column:",
            choices = NULL,
            width = "100%"
          ),

          # Validation Messages
          verbatimTextOutput(ns("insufficientColumns")),

          hr(),

          # Analysis Parameters
          h4("Analysis Parameters"),

          numericInput(
            ns("numPermutations_GSEA"),
            "Number of Permutations:",
            value = 1000,
            min = 100,
            max = 10000,
            step = 100,
            width = "100%"
          )
        ),
        br(),
        # Custom Ranking Parameters
        conditionalPanel(
          condition = paste0("input['", ns("GSEA_type_select"), "'] == 'Custom Ranking'"),

          wellPanel(
            h4("Custom Ranking Parameters"),

            # Reference Values Selection
            selectInput(
              ns("RefenceValues_GSEA"),
              "Reference Values:",
              choices = NULL,
              width = "100%"
            ),

            # Ranking Method Selection
            selectInput(
              ns("RankinkMethod_GSEA"),
              "Ranking Method:",
              choices = c(
                "Signal-to-Noise" = "Signal-to-Noise",
                "T-Test" = "T-Test",
                "Ratio" = "Ratio",
                "Difference of Expression Means Between Classes" = "Difference of Expression Means Between Classes",
                "log2 Ratio" = "log2 Ratio",
                "Sum of Ranks" = "Sum of Ranks",
                "Baumgartner-Weiss-Schinder" = "Baumgartner-Weiss-Schinder",
                "Weighted Average Difference" = "Weighted Average Difference",
                "Fold Change Rank Ordering Statistics" = "Fold Change Rank Ordering Statistics",
                "MWT" = "MWT",
                "Minimum Significant Difference" = "Minimum Significant Difference"
              ),
              selected = "MWT",
              width = "100%"
            ),

            hr(),

            # Sample Group Selection
            h5("Sample Groups"),

            selectizeInput(
              ns("numeratorSel_GSEA"),
              "Group 1 Samples (Numerator):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select samples for group 1",
                plugins = list('remove_button')
              )
            ),

            selectizeInput(
              ns("denominatorSel_GSEA"),
              "Group 2 Samples (Denominator):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select samples for group 2",
                plugins = list('remove_button')
              )
            ),

            hr(),

            # Advanced Options
            h5("Advanced Options"),

            fluidRow(
              column(
                width = 6,
                checkboxInput(
                  ns("absolute_GSEA"),
                  "Use Absolute Values",
                  value = FALSE
                )
              ),
              column(
                width = 6,
                checkboxInput(
                  ns("ties_GSEA"),
                  "Break Ties Randomly",
                  value = FALSE
                )
              )
            ),

            checkboxInput(
              ns("PADOG_GSEA"),
              "PADOG: Down-weight Common Genes",
              value = FALSE
            ),

            # Validation Options
            h5("Data Validation"),

            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = ns("gsea_min_valid_values"),
                  label = "Min valid values",
                  value = 1,
                  min = 0,
                  step = 1
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = ns("gsea_validation_rule"),
                  label = "Validation rule",
                  choices = c("In total", "In one group", "In each group"),
                  selected = "In total"
                )
              )
            )
          )
        ),

        # Precalculated Ranking Parameters
        conditionalPanel(
          condition = paste0("input['", ns("GSEA_type_select"), "'] == 'Precalculated Ranking'"),

          wellPanel(
            h4("Precalculated Ranking Parameters"),

            # Ratio Column Selection
            selectInput(
              ns("AbundanceRatio_GSEA_precalc"),
              "Abundance Ratio Column:",
              choices = NULL,
              width = "100%"
            ),

            # P-value Column Selection
            selectInput(
              ns("pVal_GSEA_precalc"),
              "P-value Column:",
              choices = NULL,
              width = "100%"
            ),

            # Ranking Metric
            selectInput(
              ns("RankingMetric_GSEA_precalc"),
              "Ranking Metric:",
              choices = c(
                "log2(FC)" = "log2(FC)",
                "log2(FC) x -log10(p)" = "log2(FC) x -log10(p)",
                "-log10(p)" = "-log10(p)"
              ),
              selected = "log2(FC) x -log10(p)",
              width = "100%"
            ),

            hr(),

            # Advanced Options
            h5("Advanced Options"),

            fluidRow(
              column(
                width = 6,
                checkboxInput(
                  ns("absolute_GSEA_precalc"),
                  "Use Absolute Values",
                  value = FALSE
                )
              ),
              column(
                width = 6,
                checkboxInput(
                  ns("ties_GSEA_precalc"),
                  "Break Ties Randomly",
                  value = FALSE
                )
              )
            ),

            checkboxInput(
              ns("PADOG_GSEA_precalc"),
              "PADOG: Down-weight Common Genes",
              value = FALSE
            )
          )
        ),
        br(),
        actionButton(ns("resetButton_GSEA"),
                     label = "Reset to Defaults",
                     width = "100%",
                     class = "btn-default",
                     icon = icon("undo"))
      )
    )
  )
}
