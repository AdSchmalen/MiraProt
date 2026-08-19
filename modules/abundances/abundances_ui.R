# ==============================================================================
# File: modules/abundances/abundances_ui.R
#
# Purpose:
#   Defines the static UI layout for the Abundances module. Contains only
#   input widget declarations and structural layout code.
#
# Architectural Role:
#   UI layer of the Abundances module. Sourced into modEnv via
#   abundances_module.R and called from modAbundancesUI() to build the tab
#   panel content. This file has no knowledge of server-side state.
#
# Structure:
#   1. abundances_UI(ns) - Returns a fluidRow containing:
#      - Left column (width 9): plot area, download panel, grid export controls
#      - Right column (width 3): create plot button, general options, plot options,
#                                reset button
#
# Notes for future developers:
#   - All input IDs are namespaced via the `ns` function argument.
#   - Choices for the data selector are populated at runtime by an observer in
#     abundances_observer.R; the values listed here are fallback defaults only.
#   - Do not add server logic, reactive expressions, or observers here.
#   - If you add a new input widget, add the corresponding handler in
#     abundances_observer.R.
# ==============================================================================

abundances_UI <- function(ns) {
  fluidRow(
    column(width = 9,
           fluidRow(
             column(width = 3,
                    checkboxInput(ns("checkbox_interactive_AbundanceTab"),
                                  label = "Interactive Plot", value = FALSE)
             ),
             br()
           ),
           fluidRow(
             column(width = 12,
                    uiOutput(ns("plot_abundanceUI"))
             )
           ),
           fluidRow(
             column(width = 12, br(),
                    wellPanel(
                      h4("Download Plot"),
                      fluidRow(
                        column(width = 3,
                               numericInput(ns("resolution_DPI_Abundances"),
                                            label = "DPI (where applicable)",
                                            value = 600,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotWidthInch_Abundances"),
                                            label = "Width (inches)",
                                            value = 14,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotHeightInch_Abundances"),
                                            label = "Height (inches)",
                                            value = 12,
                                            width = "100%")
                        ),
                        column(width = 3,
                               selectInput(ns("downloadFormat_Abundances"),
                                           label = "Download format",
                                           choices = c("png", "jpeg", "tiff", "svg", "pdf"),
                                           width = "100%")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(width = 3,
                               div(class = "input-align-bottom",
                                   downloadButton(ns("downloadPlotButton_Abundances"), "Download",
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
           )
    ),  # end column(9)

    column(width = 3,
           wellPanel(
             fluidRow(
               column(width = 12,
                      actionButton(ns("refresh_abundanceTab"),
                                   label = "Create Plot",
                                   width = "100%",
                                   class = "btn-primary",
                                   icon = icon("chart-line"))
               )
             )
           ),
           br(),
           wellPanel(
             h4("General options"),
             fluidRow(
               column(width = 6,
                      selectInput(ns("data_abundanceTab"),
                                  label = "Data type",
                                  choices = c("Raw Abundance", "Normalized Abundance",
                                              "Imputed Raw Abundance",
                                              "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
                                              "Batch Corrected Raw Abundance",
                                              "Batch Corrected Normalized Abundance"),
                                  selected = "Raw Abundance", width = "100%")
               ),
               column(width = 6,
                      selectInput(ns("label_abundanceTab"),
                                  label = "Displayed label",
                                  choices = c("Column name", "Sample name"),
                                  selected = "Sample name", width = "100%")
               )
             )
           ),
           br(),
           wellPanel(
             h4("Plot options"),
             fluidRow(
               column(width = 6,
                      selectInput(ns("col_abundanceTab"),
                                  label = "Color palette",
                                  choices = c("Magma", "Inferno", "Plasma", "Viridis",
                                              "Cividis", "Rocket", "Mako", "Turbo",
                                              "Black and White"),
                                  selected = "Viridis", width = "100%")
               ),
               column(width = 6,
                      selectInput(ns("ThemeSelect_Abundance"),
                                  label = "Plot layout",
                                  choices = c("Gray", "Black and White", "Linedraw",
                                              "Light", "Dark", "Minimal", "Classic", "Void"),
                                  selected = "Classic", width = "100%")
               )
             ),
             fluidRow(
               column(width = 12,
                      textInput(ns("plotTitle_Abundance"),
                                label = "Plot Title", value = ""),
                      checkboxInput(ns("hideTitle_Abundance"),
                                    "Hide Title", value = TRUE)
               )
             ),
             fluidRow(
               column(width = 6,
                      numericInput(ns("PlotTitleSize_Abundances"),
                                   label = "Size: Plot Title",
                                   value = 20, min = 1, max = 64, step = 1)
               )
             ),
             fluidRow(
               column(width = 6,
                      numericInput(ns("AxisTitleSize_Abundances"),
                                   label = "Size: Axis Title",
                                   value = 20, min = 1, max = 64, step = 1)
               ),
               column(width = 6,
                      numericInput(ns("tickSize_Abundance"),
                                   label = "Size: Ticks",
                                   value = 18, min = 1, max = 64, step = 1)
               )
             )
           ),
           br(),
           actionButton(ns("resetButton_Abundances"),
                        label = "Reset to Defaults",
                        width = "100%",
                        class = "btn-default",
                        icon = icon("undo"))
    )  # end column(3)
  )  # end fluidRow
}
