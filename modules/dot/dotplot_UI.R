# ==============================================================================
# dotplot_UI.R - Dotplot module user interface definitions
#
# Purpose: Defines all UI panels and controls for dotplot configuration,
# plotting, thresholds, regions, and protein labeling interactions.
#
# Structure:
#   - Main layout: Top-level module layout and panel composition
#   - Configuration panels: Axis, transformation, threshold, and style controls
#   - Plot panels: Static/interactive plot containers and selection displays
#   - Region and labeling UI: Region styling and protein labeling components
#   - UI scripting: JavaScript helper for panel toggling behavior
#
# Dependencies: shiny, shinyWidgets, colourpicker, fontawesome
# Called by: modules/dotplot_module.R via dotplot_UI()
# ==============================================================================

# ========================================
# Main UI Function - CORRECTED
# ========================================

# ------------------------------------------------------------------------------
# dotplot_UI
# Purpose: Build the top-level Dotplot module layout by composing the plot panel,
#   configuration panel, and module-specific UI styling/script helpers.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list containing the complete Dotplot UI.
# ------------------------------------------------------------------------------
dotplot_UI <- function(ns) {
  tagList(
    # CSS for custom styling - FIXED sprintf issue
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
        #%s .threshold-panel {
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          margin-bottom: 10px;
          background-color: #f9f9f9;
        }
        #%s .preset-button {
          margin-bottom: 5px;
          width: 100%%;
        }
        #%s .axis-config {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 4px;
          padding: 15px;
          margin-bottom: 15px;
        }
      ", ns(""), ns(""), ns(""), ns(""), ns(""))))
    ),

    fluidRow(
      # Left Panel - Configuration
      # Right Panel - Plot Area
      column(width = 9,
             dotplot_plot_panel(ns)
      ),

      # Right Panel - Plot Area
      column(width = 3,
             dotplot_config_panel(ns)
      )
    ),
    protein_panel_toggle_js(ns(""))
  )
}

# ========================================
# Configuration Panel - CORRECTED
# ========================================

# ------------------------------------------------------------------------------
# dotplot_config_panel
# Purpose: Create the right-side configuration controls for plot generation,
#   presets, axis selection, transformations, thresholds, and region styling.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list with all configuration controls.
# ------------------------------------------------------------------------------
dotplot_config_panel <- function(ns) {
  tagList(
    # Generate Button - FIXED ICON
    wellPanel(
      actionButton(ns("generate_plot"),
                   "Create Plot",
                   width = "100%",
                   class = "btn-primary",
                   icon = icon("chart-line"))
    ),
    br(),


    wellPanel(
      h4("Quick Presets"),
      fluidRow(
        column(
          width = 6,
          actionButton(ns("apply_volcano_preset"), "Volcano Plot",
                       class = "btn-info preset-button", icon = icon("mountain"), width = "100%")
        ),
        column(
          width = 6,
          actionButton(ns("apply_ma_preset"), "MA Plot",
                       class = "btn-info preset-button", icon = icon("chart-area"), width = "100%")
        )
      ),
      hr(),
      p("Apply standard plot configurations with predefined settings.",
        style = "font-size: 12px; color: #666;")
    ),
    br(),

    # Data Selection
    wellPanel(
      h4("Data Selection"),
      div(class = "axis-config",
          dotplot_axis_config(ns)
      )
    ),
    br(),

    # Plot Settings
    wellPanel(
      h4("Plot Settings"),
      dotplot_plot_settings(ns)
    ),

    br(),

    # Transformations
    wellPanel(
      h4("Data Transformations"),
      dotplot_transformation_config(ns)
    ),
    br(),

    # Threshold Management
    wellPanel(
      h4("Threshold Lines"),
      dotplot_threshold_config(ns)
    ),
    br(),

    # Region-specific Styling - NEW
    dotplot_region_selector_UI(ns)

  )
}

# ========================================
# Axis Configuration
# ========================================

# ------------------------------------------------------------------------------
# dotplot_axis_config
# Purpose: Render axis column selectors and user-editable axis labels, including
#   the identifier column selector used by labeling and interaction features.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list for axis and identifier configuration controls.
# ------------------------------------------------------------------------------
dotplot_axis_config <- function(ns) {
  tagList(
    # X-Axis Configuration
    div(
      h5("X-Axis", style = "margin-bottom: 10px; color: #2c3e50;"),
      selectInput(ns("x_axis_column"),
                  "Data Column:",
                  choices = c("Loading..." = ""),
                  width = "100%"),
      textInput(ns("x_axis_label"),
                "Axis Label:",
                width = "100%")
    ),

    br(),

    # Y-Axis Configuration
    div(
      h5("Y-Axis", style = "margin-bottom: 10px; color: #2c3e50;"),
      selectInput(ns("y_axis_column"),
                  "Data Column:",
                  choices = c("Loading..." = ""),
                  width = "100%"),
      textInput(ns("y_axis_label"),
                "Axis Label:",
                width = "100%")
    ),
    br(),
    div(
      h5("Protein Identifier", style = "margin-bottom: 10px; color: #2c3e50;"),
    selectInput(ns("GeneIdentifierColumn_dot"),
                "Select Identifier Column:",
                choices = c("Loading..." = ""),
                selected = "",
                width = "100%")
    )


  )
}

# ========================================
# Transformation Configuration
# ========================================

# ------------------------------------------------------------------------------
# dotplot_transformation_config
# Purpose: Provide X/Y transformation controls and explanatory guidance for how
#   value transforms are applied before plotting.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list with transformation radio button groups.
# ------------------------------------------------------------------------------
dotplot_transformation_config <- function(ns) {
  tagList(
    fluidRow(
      column(6,
             h5("X-Axis Transform"),
             radioButtons(ns("x_transform"),
                          label = NULL,
                          choices = c("Raw" = "raw",
                                      "log2" = "log2",
                                      "log10" = "log10",
                                      "-log10" = "neg_log10"),
                          selected = "raw")
      ),
      column(6,
             h5("Y-Axis Transform"),
             radioButtons(ns("y_transform"),
                          label = NULL,
                          choices = c("Raw" = "raw",
                                      "log2" = "log2",
                                      "log10" = "log10",
                                      "-log10" = "neg_log10"),
                          selected = "raw")
      )
    ),

    div(
      class = "alert alert-info",
      style = "margin-top: 10px; padding: 8px 12px; font-size: 12px;",
      icon("info-circle"),
      " Transformations are applied before plotting. Choose appropriate transforms for your data type."
    )
  )
}

# ========================================
# Threshold Configuration
# ========================================

# ------------------------------------------------------------------------------
# dotplot_threshold_config
# Purpose: Build threshold management controls for adding/removing thresholds and
#   viewing configured threshold rows.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list containing threshold action and table UI.
# ------------------------------------------------------------------------------
dotplot_threshold_config <- function(ns) {
  tagList(
    # Add Threshold Button
    fluidRow(
      column(6,
             actionButton(ns("add_threshold"), "Add Threshold",
                          icon = icon("plus"), class = "btn-primary", width = "100%")
      ),
      column(6,
             actionButton(ns("remove_threshold"), "Remove",
                          icon = icon("trash"), class = "btn-default", width = "100%",
                          style = "background-color: #3498db; border-color: #3498db; color: #fff;")
      )
    ),

    br(),

    # Threshold Table
    div(
      style = "max-height: 200px; overflow-y: auto;",
      DT::dataTableOutput(ns("threshold_table"))
    ),

    # Enhanced Info Text
    p("Click a row to select, then use Remove button. Double-click a row to edit the threshold. Thresholds are shown as lines on the plot with customizable color, style, and thickness.",
      style = "font-size: 11px; color: #666; margin-top: 10px;")
  )
}

# ========================================
# Plot Settings
# ========================================

# ------------------------------------------------------------------------------
# dotplot_plot_settings
# Purpose: Render plot appearance controls including title/theme, text sizes,
#   axis ranges, and tick interval settings.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list with plot settings inputs.
# ------------------------------------------------------------------------------
dotplot_plot_settings <- function(ns) {
  tagList(
    textInput(ns("plot_title"),
              "Plot Title:",
              placeholder = "Dot Plot"),
    checkboxInput(ns("hide_title"), "Hide Title", value = TRUE),

    fluidRow(
      column(6,
             selectInput(ns("theme_select"),
                         "Theme:",
                         choices = c("Gray", "Black and White", "Linedraw",
                                     "Light", "Dark", "Minimal", "Classic", "Void"),
                         selected = "Classic")
      )
    ),

    # NEW: Text Size Controls
    h5("Text Sizes", style = "margin-top: 15px; color: #2c3e50;"),
    fluidRow(
      column(4,
             numericInput(ns("title_size"),
                          "Title Size:",
                          value = 20, min = 1, max = 64, step = 1)
      ),
      column(4,
             numericInput(ns("axis_title_size"),
                          "Axis Title Size:",
                          value = 20, min = 1, max = 64, step = 1)
      ),
      column(4,
             numericInput(ns("tick_size"),
                          "Tick Size:",
                          value = 18, min = 1, max = 64, step = 1)
      )
    ),

    # NEW: Transformed Axis Range Controls
    h5("Axis Ranges", style = "margin-top: 20px; color: #2c3e50;"),

    # X-Axis Range with Dynamic Label
    div(
      h6(textOutput(ns("x_range_label")), style = "margin-bottom: 5px;"),
      sliderInput(ns("x_axis_range"),
                  label = NULL,
                  min = -10, max = 10,
                  value = c(-5, 5),
                  step = 0.1,
                  width = "100%"),
      fluidRow(
        column(6, div()),  # Spacer
        column(6,
               numericInput(ns("x_tick_interval"),
                            "X Tick Interval:",
                            value = 2, min = 0.1, step = 0.1,
                            width = "100%")
        )
      )
    ),

    # Y-Axis Range
    div(
      h6(textOutput(ns("y_range_label")), style = "margin-bottom: 5px;"),
      sliderInput(ns("y_axis_range"),
                  label = NULL,
                  min = -10, max = 10,
                  value = c(-5, 5),
                  step = 0.1,
                  width = "100%"),
      fluidRow(
        column(6, div()),  # Spacer
        column(6,
               numericInput(ns("y_tick_interval"),
                            "Y Tick Interval:",
                            value = 2, min = 0.1, step = 0.1,
                            width = "100%")
        )
      )
    ),

    # Simple Reset
    fluidRow(
      column(12,
             actionButton(ns("reset_ranges"),
                          "Reset Ranges",
                          icon = icon("refresh"),
                          class = "btn-secondary",
                          width = "100%")
      )
    )
  )
}

# ========================================
# Plot Panel
# ========================================

# ------------------------------------------------------------------------------
# dotplot_plot_panel
# Purpose: Build the main plot area including static/interactive plot outputs,
#   selection panels, export controls, and protein labeling workspace.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list containing all plot-side UI elements.
# ------------------------------------------------------------------------------
dotplot_plot_panel <- function(ns) {
  tagList(

    # Plot Output Area
    fluidRow(
      column(3,
             checkboxInput(ns("interactive_mode"),
                           "Interactive Plot",
                           value = FALSE)
      )
    ),
    conditionalPanel(
      condition = sprintf("!input['%s']", ns("interactive_mode")),
      plotOutput(ns("dotplot_main"),
                 height = "600px",
                 brush = brushOpts(ns("plot_brush"), resetOnNew = TRUE),
                 click = clickOpts(ns("plot_click")))
    ),

    conditionalPanel(
      condition = sprintf("input['%s']", ns("interactive_mode")),
      plotlyOutput(ns("dotplot_interactive"), height = "600px")
    ),

    br(),

    fluidRow(
      column(width = 12,
             dotplot_protein_labeling_panel(ns)
      )
    ),

    br(),

    # Selection Info Panel
    conditionalPanel(
      condition = sprintf("input['%s']", ns("interactive_mode")),
      wellPanel(
        h5("Interactive Features"),
        p("Hover over points for details. Click and drag to zoom. Double-click to reset zoom.",
          class = "text-muted"),
        uiOutput(ns("selection_info"))
      )
    ),

    br(),
    wellPanel(
      h4("Download Plot"),
      fluidRow(
        column(width = 3,
               numericInput(ns("resolution_DPI_Dotplot"),
                            label = "DPI (where applicable)",
                            value = 600,
                            width = "100%")
        ),
        column(width = 3,
               numericInput(ns("plotWidthInch_Dotplot"),
                            label = "Width (inches)",
                            value = 14,
                            width = "100%")
        ),
        column(width = 3,
               numericInput(ns("plotHeightInch_Dotplot"),
                            label = "Height (inches)",
                            value = 12,
                            width = "100%")
        ),
        column(width = 3,
               selectInput(ns("downloadFormat_Dotplot"),
                           label = "Download format",
                           choices = c("png", "jpeg", "tiff", "svg", "pdf"),
                           selected = "png",
                           width = "100%")
        )
      ),
      br(),
      fluidRow(
        column(width = 3,
               div(class = "input-align-bottom",
                   downloadButton(ns("downloadPlotButton_Dotplot"), "Download",
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
      )
    )
  )
}

# ========================================
# Region Selector UI
# ========================================

# ------------------------------------------------------------------------------
# dotplot_region_selector_UI
# Purpose: Render region selection and styling controls used to apply per-region
#   visual properties to points.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list containing region selector and styling inputs.
# ------------------------------------------------------------------------------
dotplot_region_selector_UI <- function(ns) {
  tagList(
    # Enable shinyjs for dynamic updates
    shinyjs::useShinyjs(),
    # Region Selector Panel
    wellPanel(
      h4("Region-specific Styling"),

      # Enhanced Button Matrix (rendered server-side)
      div(id = ns("region_matrix_container"),
          style = "text-align: center; margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
          uiOutput(ns("region_button_matrix"))
      ),

      # Info about selected region
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
        h5("Selected Region:", style = "margin: 0;"),
        textOutput(ns("selected_region_info"), inline = TRUE)
      ),

      # Styling options for selected region
      conditionalPanel(
        condition = sprintf("output['%s'] != ''", ns("selected_region_info")),

        h5("Point Properties for Selected Region:"),

        fluidRow(
          column(6,
                 colourpicker::colourInput(ns("region_point_color"),
                                           "Color:",
                                           value = "#E0E0E0",
                                           closeOnClick = TRUE)
          ),
          column(6,
                 numericInput(ns("region_point_size"),
                              "Size:",
                              value = 1,
                              min = 0.1,
                              max = 5,
                              step = 0.1)
          )
        ),

        fluidRow(
          column(6,
                 numericInput(ns("region_point_alpha"),
                              "Transparency:",
                              value = 0.7,
                              min = 0.1,
                              max = 1,
                              step = 0.1)
          ),
          column(6,
                 selectInput(ns("region_point_shape"),
                             "Shape:",
                             choices = list("Circle" = 19,
                                            "Square" = 15,
                                            "Triangle" = 17,
                                            "Plus" = 3,
                                            "X" = 4),
                             selected = 19)
          )
        ),

        # Reset all region-specific styling
        fluidRow(
          column(12,
                 actionButton(ns("reset_region_styling"),
                              "Reset all region styling",
                              class = "btn-default btn-block",
                              style = "background-color: #95a5a6; border-color: #95a5a6; color: #fff;",
                              icon = icon("undo"))
          )
        )
      ),

      # Info text
      div(
        class = "alert alert-info",
        style = "margin-top: 15px; padding: 8px 12px; font-size: 12px;",
        icon("info-circle"),
        " The matrix shows regions based on your threshold lines. Each region can be styled individually."
      )
    )
  )
}

# ------------------------------------------------------------------------------
# dotplot_protein_labeling_panel
# Purpose: Provide protein search, pathway import, selected-protein management,
#   and advanced labeling control widgets.
# Parameters:
#   - ns: function - Namespace helper returned by NS(id).
# Returns: shiny.tag.list with protein labeling controls and tables.
# ------------------------------------------------------------------------------
dotplot_protein_labeling_panel <- function(ns) {
  tagList(

    conditionalPanel(
      condition = sprintf("input['%s']", ns("interactive_mode")),
      wellPanel(
        h4(icon("mouse-pointer"), "Interactive Protein Selection"),

        fluidRow(
          column(width = 8,
                 h5("Selected Proteins:"),
                 verbatimTextOutput(ns("selected_items_list_dot")),
                 tags$style(type = "text/css", paste0("#", ns("selected_items_list_dot"),
                                                      " {max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; background-color: #f9f9f9;}")),
                 br(),
                 textOutput(ns("selected_items_display_dot"))
          ),
          column(width = 4,
                 h5("Actions:"),
                 actionButton(ns("copy_selection_dot"),
                              "Copy to Clipboard",
                              icon = icon("copy"),
                              width = "100%",
                              class = "btn-outline-primary"),
                 br(), br(),

                 actionButton(ns("clear_selection_dot"),
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
      condition = sprintf("!input['%s']", ns("interactive_mode")),
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
                           ns("search_identifier_label_dot"),
                           inline = TRUE
                         )
                       ),
                       textAreaInput(
                         ns("searchGene_dot"),
                         label = NULL,
                         rows = 5,
                         width = "100%"
                       ),
                       tags$script(
                         HTML(
                           paste0(
                             "$(document)",
                             ".off('input.dotSuggestions', '#",
                             ns("searchGene_dot"),
                             "')",
                             ".on('input.dotSuggestions', '#",
                             ns("searchGene_dot"),
                             "', function() {",
                             "Shiny.setInputValue('",
                             ns("searchGene_dot"),
                             "', this.value, {priority: 'event'});",
                             "});"
                           )
                         )
                       )
                     ),
                     column(width = 6,
                            h6("Suggested Identifiers:"),
                            verbatimTextOutput(ns("geneSymbolList_dot")),
                            tags$style(type = "text/css",
                                       paste0("#", ns("geneSymbolList_dot"),
                                              " {height: 120px; overflow-y: scroll;}"))
                     )
                   ),
                   tags$style(type = "text/css",
                              paste0("#", ns("searchGene_dot"),
                                     " {height: 120px !important; resize: vertical;}"))
            ),
            # PATHWAY SELECTION
            column(width = 6,
                   h6("Import proteins of enriched GSEA gene sets:"),
                   selectInput(ns("GSEA_dot"),
                               label = NULL,
                               choices = NULL,
                               multiple = TRUE,
                               width = "100%"),
                   h6("Import proteins of enriched GO terms:"),
                   selectInput(ns("GO_dot"),
                               label = NULL,
                               choices = NULL,
                               multiple = TRUE,
                               width = "100%")
            )
          ),
          # Shared button row across both columns
          fluidRow(
            column(width = 2,
                   actionButton(ns("transferButton_dot"),
                                "Add",
                                icon = icon("plus"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("clearButton_dot"),
                                "Clear",
                                icon = icon("times-circle"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("copyBtn_dot"),
                                "Copy to Clipboard",
                                icon = icon("clipboard"),
                                width = '100%')
            ),
            column(width = 2,
                   actionButton(ns("Protein_Input_dot"),
                                "Add enriched proteins")
            ),
            column(width = 2,
                   checkboxInput(ns("Intersect_dot"),
                                 label = "Intersecting proteins",
                                 value = FALSE)
            ),
            column(width = 2,
                   checkboxInput(ns("CoreEnriched_dot"),
                                 label = "Core enriched (GSEA)",
                                 value = TRUE)
            )
          ),
          br(),

          # Action buttons for labeling
          fluidRow(
            column(width = 4,
                   actionButton(ns("applySettings_dot"),
                                "Apply Labeling",
                                icon = icon("check"),
                                width = "100%",
                                class = "btn-primary")
            ),
            column(width = 4,
                   actionButton(ns("resetColors_dot"),
                                "Reset Colors",
                                icon = icon("refresh"),
                                width = "100%",
                                class = "btn-warning")
            ),
            column(width = 4,
                   actionButton(ns("clearLabels_dot"),
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
                     uiOutput(ns("enhanced_selectedProteins_dot"))
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
                   colourInput(ns("masterLabelColor_dot"),
                               "Master Label Color:",
                               value = "#000000")
            ),
            column(width = 4,
                   colourInput(ns("masterDotColor_dot"),
                               "Master Dot Color:",
                               value = "#E0E0E0")
            ),
            column(width = 4,
                   div(style = "padding-top: 25px;",
                       checkboxInput(ns("masterCustomDot_dot"),
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
                   numericInput(ns("maxOverlaps_dot"),
                                "Max Overlaps:",
                                value = 10,
                                min = 1,
                                max = 1000,
                                step = 1)
            ),
            column(width = 2,
                   numericInput(ns("labelDistance_dot"),
                                "Label Distance:",
                                value = 0.25,
                                min = 0.1,
                                max = 2.0,
                                step = 0.05)
            ),
            column(width = 2,
                   numericInput(ns("lineThickness_dot"),
                                "Line Thickness:",
                                value = 0.5,
                                min = 0.1,
                                max = 2.0,
                                step = 0.1)
            ),
            column(width = 2,
                   numericInput(ns("labelSize_dot"),
                                "Label Size:",
                                value = 8,
                                min = 6,
                                max = 16,
                                step = 1)
            ),
            column(width = 2,
                   numericInput(ns("dotSizeLabeled_dot"),
                                "Dot Size:",
                                value = 2,
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

# ------------------------------------------------------------------------------
# protein_panel_toggle_js
# Purpose: Inject JavaScript that toggles the protein controls panel and keeps
#   the chevron icon synchronized with collapsed/expanded state.
# Parameters:
#   - ns_prefix: character - Namespace prefix used to target module DOM ids.
# Returns: shiny tag containing a script block.
# ------------------------------------------------------------------------------
protein_panel_toggle_js <- function(ns_prefix) {
  tags$script(HTML(sprintf("
    (function() {
      var prefix = '%s';
      // Delegated click on the header div with the icon
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
