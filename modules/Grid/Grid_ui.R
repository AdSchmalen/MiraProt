# modules/Grid/Grid_ui.R
#
# Purpose:
#   Defines the user interface for the Grid Composer module.
#
# Architecture:
#   Provides a single exported function grid_UI(ns) called from modGridUI()
#   in Grid_module.R.  Contains only layout declarations; no logic or
#   reactive code lives here.
#
# Structure:
#   - 9-column left panel: plot preview and download controls
#   - 3-column right panel: grid options, plot options, and selection list
#
# Future developers:
#   - All input IDs are namespaced via the ns() argument provided by the
#     server's moduleServer call.
#   - Input IDs used in the server (Grid_module.R) must stay in sync with
#     what is declared here.
#   - Do not add reactive or server logic here.

grid_UI <- function(ns) {
  fluidRow(
    # Left: Preview and Download (9 columns)
    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          fluidRow(
            column(
              width = 6,
              checkboxInput(ns("auto_update"), "Auto-update preview", value = TRUE)
            ),
            column(width = 6)
          ),
          h4("Plot grid preview"),
          plotOutput(ns("preview"), height = "600px")
        )
      ),
      fluidRow(
        column(
          width = 12,
          br(),
          wellPanel(
            h4("Download plot"),
            fluidRow(
              column(3,
                     numericInput(ns("resolution_DPI"), "PPI (where applicable)", value = 600)
              ),
              column(3,
                     numericInput(ns("plotWidthInch"), "Width (inches)", value = 14)
              ),
              column(3,
                     numericInput(ns("plotHeightInch"), "Height (inches)", value = 10)
              ),
              column(3,
                     selectInput(ns("downloadFormat"), "Download format",
                                 choices = c("png", "jpeg", "tiff", "svg", "pdf"))
              )
            ),
            br(),
            downloadButton(ns("download"), "Download")
          )
        )
      )
    ),

    # Right: Customization and Selection (3 columns)
    column(
      width = 3,
      wellPanel(
        fluidRow(
          column(width = 12,
                 actionButton(ns("create_plot"),
                              "Create Plot",
                              width = "100%",
                              class = "btn-primary",
                              icon = icon("chart-line"))
          )
        )
      ),
      br(),
      wellPanel(
        h4("Grid options"),
        fluidRow(
          column(6, numericInput(ns("nrow"), "Rows", value = 2, min = 1)),
          column(6, numericInput(ns("ncol"), "Columns", value = 2, min = 1))
        ),
        selectInput(ns("align"), "Align", choices = c("none", "h", "v", "hv"), selected = "hv"),
        selectInput(ns("labels_mode"), "Labels",
                    choices = c("none", "auto_letters", "auto_numbers", "custom"), selected = "auto_letters"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("labels_mode")),
          textInput(ns("labels_custom"), "Custom labels (comma-separated)", value = "")
        ),
        numericInput(ns("label_size"), "Label size", value = 18, min = 6, max = 64, step = 1),
        checkboxInput(ns("hide_titles"), "Hide plot titles", value = TRUE)

      ),
      br(),

      wellPanel(
        h4("Plot Options"),

        # NEW: Legend Position Control
        div(
          h5("Legend Control", style = "margin-top: 15px; color: #2c3e50;"),
          selectInput(ns("force_legend_position"), "Legend Position",
                      choices = list(
                        "Preserve original (recommended)" = "preserve",
                        "Force to bottom" = "bottom",
                        "Force to top" = "top",
                        "Force to right" = "right",
                        "Force to left" = "left",
                        "Hide all legends" = "none"
                      ),
                      selected = "preserve"
          ),

          # Helpful explanation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'preserve'", ns("force_legend_position")),
            div(class = "alert alert-info", style = "font-size: 11px; margin: 5px 0;",
                icon("info-circle"), " Legends keep their original positions from individual modules.")
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] != 'preserve'", ns("force_legend_position")),
            div(class = "alert alert-warning", style = "font-size: 11px; margin: 5px 0;",
                icon("exclamation-triangle"), " This will override all individual legend settings.")
          )
        ),
      br(),

        # NEW: Margin Controls
        div(
          h5("Plot Margins", style = "margin-top: 15px; color: #2c3e50;"),
          fluidRow(
            column(3, numericInput(ns("margin_top"), "Top", value = 10, min = 0, max = 100, step = 5, width = "80px")),
            column(3, numericInput(ns("margin_right"), "Right", value = 10, min = 0, max = 100, step = 5, width = "80px")),
            column(3, numericInput(ns("margin_bottom"), "Bottom", value = 10, min = 0, max = 100, step = 5, width = "80px")),
            column(3, numericInput(ns("margin_left"), "Left", value = 10, min = 0, max = 100, step = 5, width = "80px"))
          )
        ),
        # Span Controls
        div(
          h5("Layout Controls", style = "margin-top: 15px; color: #2c3e50;"),
          div(
            class = "alert alert-info",
            style = "font-size: 12px; margin: 10px 0;",
            "Configure how many columns/rows each plot occupies in the grid."
          )
        )
      ),
      br(),
      wellPanel(
        h4("Selection"),
        fluidRow(
          column(width = 4,
                 actionButton(ns("add_blank"), HTML("Add<br>blank"), width = "100%", class = "btn btn-outline-primary")
          ),
          column(width = 4,
                 actionButton(ns("clear"), "Clear selection", width = "100%", class = "btn btn-outline-secondary")
          ),
          column(width = 4,
                 actionButton(ns("optimize_grid"), "Optimize grid layout", width = "100%", class = "btn btn-outline-primary")
          )
        ),
        br(),
        checkboxInput(ns("opt_compact_order"), "Compact order on optimize", value = FALSE),
        uiOutput(ns("selection"))
      )
    )
  )
}
