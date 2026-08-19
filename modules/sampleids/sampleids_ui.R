# ==============================================================================
# File: modules/sampleids/sampleids_ui.R
#
# Purpose:
#   Defines the static UI layout for the Sample IDs module. Contains only
#   input widget declarations and structural layout code.
#
# Architectural Role:
#   UI layer of the Sample IDs module. Sourced into modEnv via
#   sampleids_module.R and called from modSampleIDsUI() to build the tab panel
#   content. This file has no knowledge of server-side state.
#
# Structure:
#   1. sampleids_UI(ns) - Returns a fluidRow containing:
#      - Left column (width 9): plot area, download panel, grid export controls
#      - Right column (width 3): create plot button, general options, plot options,
#                                reset button
#
# Notes for future developers:
#   - All input IDs are namespaced via the `ns` function argument.
#   - The choices for FileSample_SampleIDTab and data_SampleIDTab are populated at
#     runtime by an observer in sampleids_observer.R; the NULL defaults here are
#     intentional.
#   - Do not add server logic, reactive expressions, or observers here.
#   - If you add a new input widget, add the corresponding handler in
#     sampleids_observer.R.
# ==============================================================================


#' Build the UI layout for the Sample IDs module.
#'
#' @param ns Namespace function for this module instance.
#' @return A fluidRow Shiny tag object.
sampleids_UI <- function(ns) {
  fluidRow(
    column(width = 9,
           fluidRow(
             column(width = 3,
                    checkboxInput(ns("checkbox_interactive_SampleIDTab"),
                                  label = "Interactive Plot", value = FALSE)
             )
           ),
           fluidRow(
             column(width = 12,
                    uiOutput(ns("UI_SampleIDTab"), height = "1000px")
             )
           ),
           fluidRow(
             column(width = 12, br(),
                    wellPanel(
                      h4("Download Plot"),
                      fluidRow(
                        column(width = 3,
                               numericInput(ns("resolution_DPI_SampleIDTab"),
                                            label = "DPI (where applicable)",
                                            value = 600,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotWidthInch_SampleIDTab"),
                                            label = "Width (inches)",
                                            value = 14,
                                            width = "100%")
                        ),
                        column(width = 3,
                               numericInput(ns("plotHeightInch_SampleIDTab"),
                                            label = "Height (inches)",
                                            value = 12,
                                            width = "100%")
                        ),
                        column(width = 3,
                               selectInput(ns("downloadFormat_SampleIDTab"),
                                           label = "Download format",
                                           choices = c("png", "jpeg", "tiff", "svg", "pdf"),
                                           width = "100%")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(width = 3,
                               div(class = "input-align-bottom",
                                   downloadButton(ns("downloadPlotButton_SampleIDTab"), "Download",
                                                  width = "100%",
                                                  icon = icon("download"))
                               )
                        ),
                        column(width = 3,
                               br()
                        ),
                        column(width = 3,
                               textInput(ns("grid_label"), "Label (optional)",
                                         value = "", width = "100%", placeholder = "Grid label")
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
                      actionButton(ns("refresh_SampleIDTab"),
                                   "Create Plot",
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
                      selectInput(ns("FileSample_SampleIDTab"),
                                  "'Found in' data", choices = NULL, width = "100%")
               ),
               column(width = 6,
                      selectInput(ns("data_SampleIDTab"),
                                  "Data type", choices = NULL, width = "100%")
               )
             ),
             fluidRow(
               column(width = 6,
                      selectInput(ns("label_SampleIDTab"),
                                  "Displayed label",
                                  choices = c("Column name", "Sample name"),
                                  selected = "Sample name", width = "100%")
               )
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] == 'Character strings'", ns("data_SampleIDTab")),
               fluidRow(
                 column(width = 12,
                        selectizeInput(ns("Sort_SampleIDTab"),
                                       "Select character strings",
                                       choices = NULL, multiple = TRUE,
                                       width = "100%")
                 )
               ),
               fluidRow(
                 column(width = 12,
                        selectInput(ns("AbsRel_SampleIDTab"),
                                    "Display mode",
                                    choices = c("Absolute", "Relative"),
                                    selected = "Absolute", width = "100%")
                 )
               )
             ),
             conditionalPanel(
               condition = sprintf("input['%s'] != 'Character strings'", ns("data_SampleIDTab")),
               fluidRow(
                 column(width = 6,
                        selectInput(ns("NumericPlotType_SampleIDTab"),
                                    "Plot type",
                                    choices = c("Boxplot", "Violinplot", "Barplot"),
                                    selected = "Barplot", width = "100%")
                 ),
                 column(width = 6,
                        selectInput(ns("Transform_SampleIDTab"),
                                    "Transformation",
                                    choices = list("Untransformed",
                                                   "log\u2082"  = "log2",
                                                   "log\u2081\u2080" = "log10",
                                                   "-log\u2081\u2080" = "-log10"),
                                    selected = "Untransformed", width = "100%")
                 )
               )
             )
           ),
           br(),
           wellPanel(
             h4("Plot options"),
             fluidRow(
               column(width = 6,
                      selectInput(ns("ThemeSelect_SampleIDTab"),
                                  "Plot layout",
                                  choices = c("Gray", "Black and White", "Linedraw", "Light",
                                              "Dark", "Minimal", "Classic", "Void"),
                                  selected = "Classic", width = "100%")
               ),
               column(width = 6,
                      selectInput(ns("col_SampleIDTab"),
                                  "Color palette",
                                  choices = c("Magma", "Inferno", "Plasma", "Viridis", "Cividis",
                                              "Rocket", "Mako", "Turbo", "Gray"),
                                  selected = "Viridis", width = "100%")
               )
             ),
             checkboxInput(ns("col_reverse_SampleIDTab"),
                           "Invert color", value = TRUE, width = "100%"),
             textInput(ns("plotTitle_SampleIDTab"),
                       "Plot Title", value = "Sample IDs", width = "100%"),
             checkboxInput(ns("hideTitle_SampleIDTab"),
                           "Hide Title", value = TRUE, width = "100%"),
             fluidRow(
               column(width = 6,
                      numericInput(ns("TitleSize_SampleIDTab"),
                                   "Size: Title",
                                   value = 20, min = 1, max = 64, step = 1, width = "100%")
               ),
               column(width = 6,
                      numericInput(ns("AxisTitleSize_SampleIDTab"),
                                   "Size: Axis Title",
                                   value = 20, min = 1, max = 64, step = 1, width = "100%")
               )
             ),
             fluidRow(
               column(width = 6,
                      numericInput(ns("tickSize_SampleIDTab"),
                                   "Size: Ticks",
                                   value = 18, min = 1, max = 64, step = 1, width = "100%")
               ),
               column(width = 6,
                      br()
               )
             ),
             fluidRow(
               column(width = 6,
                      numericInput(ns("LegendTitleSize_SampleIDTab"),
                                   "Size: Legend Title",
                                   value = 20, min = 1, max = 64, step = 1, width = "100%")
               ),
               column(width = 6,
                      numericInput(ns("LegendTextSize_SampleIDTab"),
                                   "Size: Legend Text",
                                   value = 18, min = 1, max = 64, step = 1, width = "100%")
               )
             ),
             fluidRow(
               column(width = 12,
                      selectInput(
                        ns("sampleIDs_legend_position"),
                        label = "Legend position:",
                        choices = c("None" = "none", "Right" = "right",
                                    "Left" = "left", "Top" = "top", "Bottom" = "bottom"),
                        selected = "none"
                      )
               )
             )
           ),
           br(),
           actionButton(ns("resetButton_SampleIDTab"),
                        label = "Reset to Defaults",
                        width = "100%",
                        class = "btn-default",
                        icon = icon("undo"))
    )  # end column(3)
  )  # end fluidRow
}
