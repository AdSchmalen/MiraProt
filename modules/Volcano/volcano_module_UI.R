# volcano_module_UI.R - User Interface Components for Volcano Module

# ========================================
# Main UI Function
# ========================================

volcano_UI <- function(ns) {
  tagList(
    # CSS for custom styling
    tags$head(
      tags$style(HTML(sprintf("
        #%s .well {
          padding: 10px;
          margin-bottom: 10px;
        }
        #%s .control-label {
          font-weight: bold;
          margin-bottom: 5px;
        }
        #%s .selectize-input {
          min-height: 34px;
        }
        #%s .btn-group-vertical {
          width: 100%%;
        }
        #%s .plot-container {
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          background-color: #f9f9f9;
        }
        #%s textarea {
          font-family: monospace;
          font-size: 12px;
        }
      ", ns(""), ns(""), ns(""), ns(""), ns(""), ns(""))))
    ),

    fluidRow(
      # Main plot area
      column(width = 9,
             volcano_plot_panel(ns),
             br(),
             volcano_protein_labeling_panel(ns),
             br(),

             # Download panel
             volcano_download_panel(ns)
      ),

      # Control panel
      column(width = 3,
             volcano_control_panel(ns)#,
             #volcano_enhanced_plot_selection_UI(ns("plot_selection"))
      )
    ),
    protein_panel_toggle_js("volcano_module-")
  )
}

# ========================================
# Plot Panel
# ========================================

volcano_plot_panel <- function(ns) {
  div(
    class = "plot-container",

    # Plot type selector
    fluidRow(
      column(width = 6,
             checkboxInput(ns("cechbox_interactive_Volcano"),
                           "Interactive Plot",
                           value = FALSE)
      ),
      column(width = 6,
             conditionalPanel(
               condition = sprintf("output['%s'] !== null", ns("plot_count")),
               selectInput(ns("PlotSelect_Volcano"),
                           "Select Plot:",
                           choices = NULL,
                           width = "100%")
             )
      )
    ),

    # Static plot output
    conditionalPanel(
      condition = sprintf("!input['%s']", ns("cechbox_interactive_Volcano")),
      plotOutput(ns("volcanoPlot"),
                 height = "600px",
                 width = "100%")
    ),

    # Interactive plot output
    conditionalPanel(
      condition = sprintf("input['%s']", ns("cechbox_interactive_Volcano")),
      plotlyOutput(ns("volcanoPlotly"),
                   height = "600px",
                   width = "100%")
    )
  )
}

# ========================================
# Selection Panel
# ========================================
volcano_protein_labeling_panel <- function(ns) {
  tagList(
    conditionalPanel(
      condition = sprintf("input['%s']", ns("cechbox_interactive_Volcano")),

      wellPanel(
        h4(icon("mouse-pointer"), "Interactive Protein Selection"),

        fluidRow(
          column(width = 8,
                 h5("Selected Proteins:"),
                 verbatimTextOutput(ns("selected_items_list_Volcano")),
                 tags$style(type = "text/css", paste0("#", ns("selected_items_list_Volcano"),
                                                      " {max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; background-color: #f9f9f9;}")),
                 br(),
                 textOutput(ns("selected_items_display_Volcano"))
          ),
          column(width = 4,
                 h5("Actions:"),
                 actionButton(ns("copy_selection_Volcano"),
                              "Copy to Clipboard",
                              icon = icon("copy"),
                              width = "100%",
                              class = "btn-outline-primary"),
                 br(), br(),

                 actionButton(ns("clear_selection_Volcano"),
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


    conditionalPanel(
      condition = sprintf("!input['%s']", ns("cechbox_interactive_Volcano")),
      # Protein Selection & Labeling wellPanel with DataWizard pattern
      wellPanel(
        style = "margin-bottom: 10px;",
        tags$div(
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

          # Row 1: Search textarea + Suggested Identifiers (left 50%) + GSEA/GO dropdowns (right 50%)
          fluidRow(
            column(width = 6,
                   fluidRow(
                     column(
                       width = 6,
                       h6(
                         textOutput(
                           ns("search_identifier_label_Volcano"),
                           inline = TRUE
                         )
                       ),
                       textAreaInput(
                         ns("searchGene_Volcano"),
                         label = NULL,
                         rows = 5,
                         width = "100%"
                       ),
                       tags$script(
                         HTML(
                           paste0(
                             "$(document)",
                             ".off('input.volcanoSuggestions', '#",
                             ns("searchGene_Volcano"),
                             "')",
                             ".on('input.volcanoSuggestions', '#",
                             ns("searchGene_Volcano"),
                             "', function() {",
                             "Shiny.setInputValue('",
                             ns("searchGene_Volcano"),
                             "', this.value, {priority: 'event'});",
                             "});"
                           )
                         )
                       )
                     ),
                     column(width = 6,
                            h6("Suggested Identifiers:"),
                            verbatimTextOutput(ns("geneSymbolList_Volcano")),
                            tags$style(type = "text/css",
                                       paste0("#", ns("geneSymbolList_Volcano"),
                                              " {height: 120px; overflow-y: scroll;}"))
                     )
                   ),
                   tags$style(type = "text/css",
                              paste0("#", ns("searchGene_Volcano"),
                                     " {height: 120px !important; resize: vertical;}"))
            ),
            # PATHWAY SELECTION
            column(width = 6,
                   fluidRow(
                     column(width = 6,
                            actionButton(ns("significantMoreButton_Volcano"),
                                         "Significantly more abundant",
                                         icon = icon("arrow-up"),
                                         width = "100%")
                     ),
                     column(width = 6,
                            actionButton(ns("significantLessButton_Volcano"),
                                         "Significantly less abundant",
                                         icon = icon("arrow-down"),
                                         width = "100%")
                     )
                   ),
                   h6("Import proteins of enriched GSEA gene sets:"),
                   selectInput(ns("GSEA_Volcano"),
                               label = NULL,
                               choices = NULL,
                               multiple = TRUE,
                               width = "100%"),
                   h6("Import proteins of enriched GO terms:"),
                   selectInput(ns("GO_Volcano"),
                               label = NULL,
                               choices = NULL,
                               multiple = TRUE,
                               width = "100%")
            )
          ),
          # Shared button row across both columns
          fluidRow(
            column(width = 2,
                   actionButton(ns("transferButton_Volcano"),
                                "Add",
                                icon = icon("plus"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("clearButton_Volcano"),
                                "Clear",
                                icon = icon("times-circle"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("copyBtn_Volcano"),
                                "Copy to Clipboard",
                                icon = icon("clipboard"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("Protein_Input_Volcano"),
                                "Add enriched proteins")
            ),
            column(width = 2,
                   checkboxInput(ns("Intersect_Volcano"),
                                 label = "Intersecting proteins",
                                 value = FALSE)
            ),
            column(width = 2,
                   checkboxInput(ns("CoreEnriched_Volcano"),
                                 label = "Core enriched (GSEA)",
                                 value = TRUE)
            )
          ),
          br(),

          # Action buttons for labeling
          fluidRow(
            column(width = 4,
                   actionButton(ns("applySettings_Volcano"),
                                "Apply Labeling",
                                icon = icon("check"),
                                width = "100%",
                                class = "btn-primary")
            ),
            column(width = 4,
                   actionButton(ns("resetColors_Volcano"),
                                "Reset Colors",
                                icon = icon("refresh"),
                                width = "100%",
                                class = "btn-warning")
            ),
            column(width = 4,
                   actionButton(ns("clearLabels_Volcano"),
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
                     uiOutput(ns("enhanced_selectedProteins_Volcano"))
                   )
            )
          ),

          # Master Controls
          br(),
          fluidRow(
            column(width = 12,
                   h5("Master Controls:", style = "margin-bottom: 10px; color: #2c3e50;")
            )
          ),
          fluidRow(
            column(width = 4,
                   colourInput(ns("masterLabelColor_Volcano"),
                               "Master Label Color:",
                               value = "#000000")
            ),
            column(width = 4,
                   colourInput(ns("masterDotColor_Volcano"),
                               "Master Dot Color:",
                               value = "#E0E0E0")
            ),
            column(width = 4,
                   div(style = "padding-top: 25px;",
                       checkboxInput(ns("masterCustomDot_Volcano"),
                                     "Enable All Custom Dot Colors",
                                     value = FALSE)
                   )
            )
          ),

          # Labeling Settings
          br(),
          fluidRow(
            column(width = 12,
                   h5("Labeling Settings:", style = "margin-bottom: 10px; color: #2c3e50;")
            )
          ),
          fluidRow(
            column(width = 2,
                   numericInput(ns("maxOverlaps_Volcano"),
                                "Max Overlaps:",
                                value = 10,
                                min = 1,
                                max = 1000,
                                step = 1)
            ),
            column(width = 2,
                   numericInput(ns("labelDistance_Volcano"),
                                "Label Distance:",
                                value = 0.25,
                                min = 0.1,
                                max = 2.0,
                                step = 0.05)
            ),
            column(width = 2,
                   numericInput(ns("lineThickness_Volcano_Label"),
                                "Line Thickness:",
                                value = 0.5,
                                min = 0.1,
                                max = 2.0,
                                step = 0.1)
            ),
            column(width = 2,
                   numericInput(ns("labelSize_Volcano"),
                                "Label Size:",
                                value = 8,
                                min = 1,
                                max = 64,
                                step = 0.5)
            ),
            column(width = 2,
                   numericInput(ns("dotSizeLabeled_Volcano"),
                                "Dot Size:",
                                value = 1,
                                min = 0.1,
                                max = 10,
                                step = 0.1)
            )
          )
        )
      )
    )
  )
}

# ========================================
# Search Tab
# ========================================

volcano_search_tab <- function(ns) {
  tagList(
    fluidRow(
      column(width = 8,
             textAreaInput(ns("searchGene_Volcano"),
                           "Search Proteins (one per line):",
                           rows = 5,
                           placeholder = "Enter protein identifiers...",
                           width = "100%")
      ),
      column(width = 4,
             br(),
             actionButton(ns("transferButton_Volcano"),
                          "Add Labels",
                          icon = icon("plus"),
                          class = "btn-primary btn-block"),
             br(),
             actionButton(ns("clearButton_Volcano"),
                          "Clear All",
                          icon = icon("times"),
                          class = "btn-danger btn-block")
      )
    ),

    hr(),

    fluidRow(
      column(width = 12,
             h5("Matching Proteins:"),
             verbatimTextOutput(ns("geneSymbolList_Volcano"))
      )
    )
  )
}

# ========================================
# Threshold Tab
# ========================================

volcano_threshold_tab <- function(ns) {
  tagList(
    fluidRow(
      column(width = 6,
             actionButton(ns("upregButton_Volcano"),
                          "Select Upregulated",
                          icon = icon("arrow-up"),
                          class = "btn-warning btn-block",
                          style = "margin-bottom: 10px;"),
             actionButton(ns("downButton_Volcano"),
                          "Select Downregulated",
                          icon = icon("arrow-down"),
                          class = "btn-info btn-block")
      ),
      column(width = 6,
             checkboxInput(ns("Intersect_Volcano"),
                           "Apply current thresholds",
                           value = TRUE),
             br(),
             p("Selects proteins based on the p-value and fold change thresholds set in the plot parameters.",
               style = "font-size: 12px; color: #666;")
      )
    )
  )
}

# ========================================
# Interactive Selection Tab
# ========================================

volcano_interactive_tab <- function(ns) {
  tagList(
    conditionalPanel(
      condition = sprintf("!input['%s']", ns("cechbox_interactive_Volcano")),
      p("Switch to Interactive Plot Mode to enable selection.",
        class = "text-muted")
    ),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("cechbox_interactive_Volcano")),
      p("Click on individual points or drag to select multiple proteins.",
        class = "text-info"),
      br(),
      uiOutput(ns("selectedInfo_Volcano")),
      br(),
      actionButton(ns("copySelected_Volcano"),
                   "Copy Selected to Clipboard",
                   icon = icon("clipboard"),
                   class = "btn-success btn-block")
    )
  )
}

# ========================================
# Control Panel
# ========================================

volcano_control_panel <- function(ns) {
  tagList(
    # Update button
    wellPanel(
      fluidRow(
        column(width = 12,
               actionButton(ns("update_Volcano"),
                            "Create Plot",
                            width = "100%",
                            class = "btn-primary",
                            icon = icon("chart-line"))
               )
      )
    ),

    br(),

    # Plot settings
    wellPanel(
      h4("Plot Settings"),
      volcano_plot_settings(ns)
    ),

    br(),

    # Appearance settings
    wellPanel(
      h4("Appearance"),
      volcano_appearance_settings(ns)
    ),

    br(),

    # Threshold settings
    wellPanel(
      h4("Thresholds"),
      volcano_threshold_settings(ns)
    ),

    br(),

    # Axis settings
    wellPanel(
      h4("Axes"),
      volcano_axis_settings(ns)
    ),

    br(),

    # Reset button
    actionButton(ns("resetButton_Volcano"),
                 "Reset to Defaults",
                 icon = icon("undo"),
                 width = "100%",
                 class = "btn-default")
  )
}

# ========================================
# Plot Settings
# ========================================

volcano_plot_settings <- function(ns) {
  tagList(
    textInput(ns("plotTitle_Volcano"),
              "Plot Title:",
              placeholder = "Leave empty for auto-generated title"),
    checkboxInput(ns("hideTitle_Volcano"), "Hide Title", value = FALSE),

    fluidRow(
      column(width = 6,
             numericInput(ns("plotTitleSize_Volcano"),
                          "Title Size:",
                          value = 20, min = 1, max = 64, step = 1)
      )
      ),
    fluidRow(
      column(width = 6,
             numericInput(ns("AxisTitleSize_Volcano"),
                          "Axis Title Size:",
                          value = 20, min = 1, max = 64, step = 1)
      ),
      column(width = 6,
             numericInput(ns("tickSize_Volcano"),
                          "Tick Size:",
                          value = 18, min = 1, max = 64, step = 1)
      )
    ),

    selectInput(ns("ThemeSelect_Volcano"),
                "Plot Theme:",
                choices = c("Gray", "Black and White", "Linedraw",
                            "Light", "Dark", "Minimal", "Classic", "Void"),
                selected = "Classic"),

    selectInput(ns("pValueSel_Volcano"),
                "P-value Type:",
                choices = c("Adjusted p-value", "p-value (not recommended)"),
                selected = "Adjusted p-value"),
    fluidRow(
      column(width = 12,
             selectInput(ns("Identifier_Volcano"),
                         label = "Identifier Column:",
                         choices = NULL,
                         selected = NULL
             )
      )
    )
  )
}

# ========================================
# Appearance Settings
# ========================================

volcano_appearance_settings <- function(ns) {
  tagList(
    h5("Non-significant Points"),
    fluidRow(
      column(width = 6,
             sliderInput(ns("dotSizeInput_Volcano"),
                         "Size:",
                         min = 0.1, max = 5,
                         value = 1, step = 0.1)
      ),
      column(width = 6,
             colourInput(ns("dotColorInput_Volcano"),
                         "Color:",
                         value = "#E0E0E0")
      )
    ),

    h5("Upregulated Points"),
    fluidRow(
      column(width = 6,
             sliderInput(ns("dotSizeInputUp_Volcano"),
                         "Size:",
                         min = 0.1, max = 5,
                         value = 1.5, step = 0.1)
      ),
      column(width = 6,
             colourInput(ns("dotColorInputUp_Volcano"),
                         "Color:",
                         value = "#EFC000FF")
      )
    ),

    h5("Downregulated Points"),
    fluidRow(
      column(width = 6,
             sliderInput(ns("dotSizeInputDown_Volcano"),
                         "Size:",
                         min = 0.1, max = 5,
                         value = 1.5, step = 0.1)
      ),
      column(width = 6,
             colourInput(ns("dotColorInputDown_Volcano"),
                         "Color:",
                         value = "#440154FF")
      )
    )
  )
}

# ========================================
# Threshold Settings
# ========================================

volcano_threshold_settings <- function(ns) {
  tagList(
    numericInput(ns("pvalueInput_Volcano"),  # This should match plot_params$pval_threshold
                 "P-value Threshold:",
                 value = 0.05,
                 min = 0, max = 1,
                 step = 0.01),

    numericInput(ns("AbundanceInput_Volcano"),  # This should match plot_params$fold_threshold
                 "Fold Change Threshold (log2):",
                 value = 1,
                 min = 0,
                 step = 0.1)
  )
}

# ========================================
# Axis Settings
# ========================================

volcano_axis_settings <- function(ns) {
  tagList(
    h5("X-Axis (Log2 Fold Change)"),
    sliderInput(ns("xLimInput_Volcano"),
                "Range:",
                min = -10, max = 10,
                value = c(-8, 8),
                step = 0.5),

    numericInput(ns("xTick_Volcano"),
                 "Tick Interval:",
                 value = 2,
                 min = 0.1,
                 step = 0.1),

    h5("Y-Axis (-Log10 P-value)"),
    sliderInput(ns("yLimInput_Volcano"),
                "Range:",
                min = 0, max = 50,
                value = c(0, 18),
                step = 1),

    numericInput(ns("yTick_Volcano"),
                 "Tick Interval:",
                 value = 2,
                 min = 0.1,
                 step = 0.1)
  )
}

# ========================================
# Download Panel
# ========================================

volcano_download_panel <- function(ns) {
  conditionalPanel(
    condition = sprintf("!input['%s']", ns("cechbox_interactive_Volcano")),
    wellPanel(
      h4("Download Plot"),
      fluidRow(
        column(width = 3,
               numericInput(ns("resolution_DPI"),
                            label = "DPI (where applicable)",
                            value = 300,
                            min = 72,
                            max = 600,
                            width = "100%")
        ),
        column(width = 3,
               numericInput(ns("plotWidthInch_Volcano"),
                            label = "Width (inches)",
                            value = 8,
                            min = 1,
                            max = 20,
                            width = "100%")
        ),
        column(width = 3,
               numericInput(ns("plotHeightInch_Volcano"),
                            label = "Height (inches)",
                            value = 6,
                            min = 1,
                            max = 20,
                            width = "100%")
        ),
        column(width = 3,
               selectInput(ns("downloadFormat_Volcano"),
                           label = "Download format",
                           choices = c("png", "pdf", "svg", "tiff", "jpeg"),
                           selected = "png",
                           width = "100%")
        )
      ),
      br(),
      fluidRow(
        column(width = 3,
               div(class = "input-align-bottom",
                   downloadButton(ns("downloadPlotButton_Volcano"), "Download",
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

      # Hidden controls for plot sizing
      conditionalPanel(
        condition = "false",
        numericInput(ns("plotWidth_Volcano"), "Width", value = 800),
        numericInput(ns("plotHeight_Volcano"), "Height", value = 600)
      )
    )
  )
}

# ========================================
# Selected Genes for labeling
# ========================================

# Replace the existing selectedGene_Volcano table section with:

enhanced_protein_labeling_ui <- function(ns) {
  tagList(
    fluidRow(
      column(width = 12,
             h4("Selected Proteins & Label Controls:"),
             div(
               style = "width: 100%;",
               uiOutput(ns("enhanced_selectedProteins_Volcano"))
             ),
             fluidRow(
               column(width = 12,
                      h5("Master Controls:", style = "margin-top: 15px; margin-bottom: 10px; color: #2c3e50;")
               )
             ),
             fluidRow(
               column(width = 4,
                      colourInput(ns("masterLabelColor_Volcano"),
                                  "Master Label Color:",
                                  value = "#000000")
               ),
               column(width = 4,
                      colourInput(ns("masterDotColor_Volcano"),
                                  "Master Dot Color:",
                                  value = "#E0E0E0")
               ),
               column(width = 4,
                      div(style = "padding-top: 25px;",
                          checkboxInput(ns("masterCustomDot_Volcano"),
                                        "Enable All Custom Dot Colors",
                                        value = FALSE)
                      )
               )
             )
      )
    )
  )
}

# ========================================
# JavaScript for Clipboard
# ========================================

volcano_clipboard_js <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
      if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(function() {
          console.log('Copied to clipboard');
        }).catch(function(err) {
          console.error('Could not copy text: ', err);
          fallbackCopyTextToClipboard(text);
        });
      } else {
        fallbackCopyTextToClipboard(text);
      }
    });

    function fallbackCopyTextToClipboard(text) {
      var textArea = document.createElement('textarea');
      textArea.value = text;
      textArea.style.position = 'fixed';
      textArea.style.top = 0;
      textArea.style.left = 0;
      textArea.style.width = '2em';
      textArea.style.height = '2em';
      textArea.style.padding = 0;
      textArea.style.border = 'none';
      textArea.style.outline = 'none';
      textArea.style.boxShadow = 'none';
      textArea.style.background = 'transparent';
      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();

      try {
        var successful = document.execCommand('copy');
        console.log('Fallback: Copying text command was ' + (successful ? 'successful' : 'unsuccessful'));
      } catch (err) {
        console.error('Fallback: Unable to copy', err);
      }

      document.body.removeChild(textArea);
    }
  "))
}

protein_panel_toggle_js <- function(ns_prefix) {
  tags$script(HTML(sprintf("
    (function() {
      var prefix = '%s';
      // Delegated click auf den Header-DIV mit dem Icon
      $(document).on('click', '#'+prefix+'protein_controls_icon, div[onclick*=\"'+prefix+'toggle_protein_controls\"]', function() {
        var content = $('#'+prefix+'protein_controls_content');
        var icon    = $('#'+prefix+'protein_controls_icon');
        if (content.is(':visible')) {
          content.slideUp();
          icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
        } else {
          content.slideDown();
            icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
        }
      });
    })();
  ", ns_prefix)))
}
