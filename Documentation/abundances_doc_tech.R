# ==============================================================================
# File: Documentation/abundances_doc_tech.R
#
# Purpose:
#   Contains all content-rendering functions for the technical (developer-facing)
#   documentation of the Abundances module. Each function returns a Shiny tag
#   list that describes one section of the technical reference.
#
# Functions:
#   1. render_tech_overview_content_abundance()        - Architecture, data flow,
#                                                        dependencies, state
#   2. render_tech_functions_content_abundance()        - Function signatures,
#                                                        parameters, return values
#   3. render_tech_data_processing_content_abundance()  - Step-by-step data pipeline
#   4. render_tech_integration_content_abundance()      - Integration points, rv
#                                                        contract, wiring
#
# Notes for future developers:
#   - These functions are called from modAbundancesDocServer() in
#     abundances_doc_ui.R via the content routing switch.
#   - Each function must return a single Shiny tag (typically a div()).
#   - Keep the language precise and targeted at developers who need to
#     maintain or extend the module. Document architecture, contracts,
#     and non-obvious design decisions.
#   - When the module implementation changes, update this file to match.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Technical Overview
# ------------------------------------------------------------------------------

render_tech_overview_content_abundance <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Module Architecture"),
    p("The Abundances module follows a four-layer architecture. An ",
      "orchestrator file sources four sub-files, each with a single ",
      "responsibility:"),

    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "modules/abundances_module.R          # Orchestrator: sources sub-files,
                                    # defines modAbundancesUI/Server
  modules/abundances/
    abundances_ui.R                 # UI layer: static layout and input widgets
    abundances_logic.R              # Logic layer: pure functions (no Shiny deps)
    abundances_state.R              # State layer: reactive value factory
    abundances_observer.R           # Observer layer: all observe/render blocks"
    ),

    h4("Layer Responsibilities"),
    tags$dl(
      tags$dt("Orchestrator (abundances_module.R)"),
      tags$dd("Sources sub-files into modEnv. Defines the public API: ",
              "modAbundancesUI() wraps abundances_UI(), and ",
              "modAbundancesServer() initializes debug logging, creates ",
              "state, registers observers, and returns the reactive interface."),

      tags$dt("UI (abundances_ui.R)"),
      tags$dd("Pure UI declaration. Returns a fluidRow with a 9-column plot ",
              "area (interactive checkbox, plot output, download panel, grid ",
              "controls) and a 3-column options sidebar (Create Plot button, ",
              "General options, Plot options). No server logic."),

      tags$dt("Logic (abundances_logic.R)"),
      tags$dd("Contains Shiny-free functions: sanitize_plot_id() for safe ID ",
              "generation, and build_abundances_plot() which constructs ",
              "ggplot2 and Plotly objects from long-format data and user ",
              "options. Receives debug_log as a parameter to enable logging ",
              "without Shiny dependency."),

      tags$dt("State (abundances_state.R)"),
      tags$dd("Factory function create_abundances_state() that returns a ",
              "named list of reactiveVal containers. Currently holds ",
              "plot_object_abundanceTab (Plotly) and ",
              "ggplot_object_abundanceTab (ggplot2)."),

      tags$dt("Observer (abundances_observer.R)"),
      tags$dd("register_abundances_observers() contains all observe(), ",
              "observeEvent(), renderUI, renderPlot, renderPlotly, and ",
              "downloadHandler blocks. Delegates computation to ",
              "build_abundances_plot(). Thin observers: validate inputs, ",
              "call logic, update state, notify user.")
    ),

    h3("Data Flow"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px;",
      "rv$data_def (metadata)   rv$data_mod (data)
        |                           |
        +------ Dropdown observer --+
        |       Filters available abundance types from metadata
        |
        +------ Refresh observer (on Create Plot click) ------+
        |  1. Read input$data_abundanceTab (selected type)     |
        |  2. Identify matching columns via grepl on Content   |
        |  3. Retransform data: retransform_data_global()      |
        |  4. Subset to abundance columns                      |
        |  5. Apply label choice (Column or Sample)            |
        |  6. Reshape to long format via melt()                |
        |  7. Remove non-finite values                         |
        |  8. Call build_abundances_plot()                      |
        |     -> ggplot2 object + Plotly object                |
        |  9. Store in state reactiveVals                      |
        |                                                      |
        +------ renderUI switches plotlyOutput / plotOutput ----+
        +------ renderPlotly / renderPlot read from state ------+
        +------ downloadHandler exports ggplot2 object ---------+"
    ),

    h3("Dependencies"),
    tags$ul(
      tags$li(strong("ggplot2:"), " Core plotting library for box plots"),
      tags$li(strong("plotly:"), " Interactive plot conversion via ggplotly()"),
      tags$li(strong("viridis:"), " Color palette generation (viridis() function)"),
      tags$li(strong("reshape2:"), " Data transformation from wide to long ",
              "format (melt())")
    ),

    h3("Module State"),
    p("The module maintains minimal state through two reactiveVal containers ",
      "created by create_abundances_state():"),
    tags$ul(
      tags$li(strong("ggplot_object_abundanceTab:"), " Stores the current ",
              "ggplot2 object. Used for static rendering and download export."),
      tags$li(strong("plot_object_abundanceTab:"), " Stores the current ",
              "Plotly object. Used for interactive rendering.")
    ),
    p("Both values are set to NULL initially and updated only when the user ",
      "clicks Create Plot. No persistent state is maintained between sessions.")
  )
}


# ------------------------------------------------------------------------------
# 2. Functions Reference
# ------------------------------------------------------------------------------

render_tech_functions_content_abundance <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    # -- abundances_module.R --
    h3("Orchestrator (abundances_module.R)"),

    h4("modAbundancesUI(id)"),
    p("Public UI wrapper. Calls abundances_UI(ns) from the UI sub-file."),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "modAbundancesUI <- function(id) {
  ns <- NS(id)
  abundances_UI(ns)
}"
    ),

    h4("modAbundancesServer(id, rv, debug_level)"),
    p("Public server function. Initializes debug logging, creates module ",
      "state, registers observers, and returns the reactive interface."),
    tags$dl(
      tags$dt("Parameters"),
      tags$dd(
        tags$ul(
          tags$li(strong("id"), " -- Module namespace identifier"),
          tags$li(strong("rv"), " -- Global reactiveValues (must contain ",
                  "data_def, data_mod, height_px, px_ratio)"),
          tags$li(strong("debug_level"), " -- Integer, logging verbosity ",
                  "(default: 1)")
        )
      ),
      tags$dt("Returns"),
      tags$dd("Named list with ggplot_object_abundanceTab and ",
              "plot_object_abundanceTab (both reactiveVal)")
    ),

    hr(),

    # -- abundances_logic.R --
    h3("Logic Layer (abundances_logic.R)"),

    h4("sanitize_plot_id(x)"),
    p("Replaces non-alphanumeric characters with underscores to produce a ",
      "safe identifier for plot grid entries."),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "sanitize_plot_id <- function(x) {
  gsub(\"[^[:alnum:]_]+\", \"_\", x)
}"
    ),

    h4("build_abundances_plot(...)"),
    p("Core plotting function. Builds both a ggplot2 and a Plotly version of ",
      "the abundance box plot from long-format data. Returns NULL if data is ",
      "empty."),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "build_abundances_plot(
  data_long,        # Long-format data frame (Variable, Value columns)
  data_abundance,   # Character: selected abundance type label
  abundance_title,  # Character: plot title
  color_input,      # Character: palette name or \"Black and White\"
  selected_layout,  # Character: ggplot2 theme name
  plot_title_size,  # Numeric: title font size
  axis_title_size,  # Numeric: axis title font size
  tick_size,        # Numeric: tick label font size
  debug_log         # Function: logging closure from server
)
# Returns: list(ggplot_object, plotly_object) or NULL"
    ),

    p("Key implementation details:"),
    tags$ul(
      tags$li("Two separate ggplot objects are built: one for static ",
              "rendering (with legend suppressed) and one for Plotly ",
              "conversion (with legend visible)"),
      tags$li("Y-axis uses log2 transformation via aes(y = log2(Value))"),
      tags$li("Y-axis label uses bquote() for the static plot and ",
              "HTML subscript notation for the Plotly version"),
      tags$li("Color palettes are resolved via viridis() with the palette ",
              "name converted to lowercase as the option parameter"),
      tags$li("Theme selection uses a switch() statement over the eight ",
              "standard ggplot2 themes"),
      tags$li("Plotly conversion is wrapped in tryCatch(); returns NULL ",
              "for the Plotly object on failure")
    ),

    hr(),

    # -- abundances_state.R --
    h3("State Layer (abundances_state.R)"),

    h4("create_abundances_state()"),
    p("Factory function that creates all reactive containers for the module. ",
      "Must be called inside moduleServer()."),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "create_abundances_state <- function() {
  list(
    plot_object_abundanceTab   = reactiveVal(NULL),
    ggplot_object_abundanceTab = reactiveVal(NULL)
  )
}"
    ),

    hr(),

    # -- abundances_observer.R --
    h3("Observer Layer (abundances_observer.R)"),

    h4("register_abundances_observers(input, output, session, ns, state, rv, debug_log)"),
    p("Registers all reactive side-effects for the module. Called once from ",
      "modAbundancesServer() after state initialization."),

    tags$dl(
      tags$dt("Parameters"),
      tags$dd(
        tags$ul(
          tags$li(strong("input, output, session"), " -- Standard Shiny ",
                  "objects from moduleServer closure"),
          tags$li(strong("ns"), " -- Namespace function"),
          tags$li(strong("state"), " -- Named list from create_abundances_state()"),
          tags$li(strong("rv"), " -- Global reactiveValues"),
          tags$li(strong("debug_log"), " -- Logging closure")
        )
      )
    ),

    p("Contains the following blocks:"),
    tags$ol(
      tags$li(strong("Data-type dropdown observer:"), " Filters the data type ",
              "selectInput to show only abundance types present in ",
              "rv$data_def$Content"),
      tags$li(strong("Plot-title sync observer:"), " Mirrors the selected ",
              "data type into the plot title text input"),
      tags$li(strong("Refresh plot observer:"), " Validates inputs, reshapes ",
              "data, calls build_abundances_plot(), stores result in state"),
      tags$li(strong("Dynamic plot UI output:"), " Switches between ",
              "plotlyOutput and plotOutput based on checkbox state; uses ",
              "rv$height_px and rv$px_ratio for sizing"),
      tags$li(strong("Plotly output:"), " renderPlotly reading from state"),
      tags$li(strong("Static plot output:"), " renderPlot reading from state"),
      tags$li(strong("Download handler:"), " Exports ggplot2 object using ",
              "device-specific functions (png/jpeg/tiff/svg/pdf)"),
      tags$li(strong("Add-to-grid observer:"), " Sends the current ggplot2 ",
              "object to the Plot Grid via modEnv$add_to_grid()")
    )
  )
}


# ------------------------------------------------------------------------------
# 3. Data Processing
# ------------------------------------------------------------------------------

render_tech_data_processing_content_abundance <- function() {
  div(
    h2("Data Processing"),
    hr(),

    h3("Data Processing Pipeline"),
    p("The following steps are executed inside the refresh plot observer ",
      "when the user clicks Create Plot:"),

    h4("Step 1: Input Collection"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# All inputs are read inside isolate() to prevent reactive cascading.
data_abundance  <- as.character(input$data_abundanceTab)[1]
abundance_title <- as.character(input$plotTitle_Abundance)
selected_input  <- paste0(\"^\", input$data_abundanceTab, \"$\")
selected_layout <- input$ThemeSelect_Abundance
label_input     <- input$label_abundanceTab
color_input     <- input$col_abundanceTab

data_def <- rv$data_def
data     <- rv$data_mod"
    ),

    h4("Step 2: Column Identification"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Find abundance column indices by matching data_def$Content
# against the selected type using a regex anchor.
abundance_index <- which(grepl(selected_input, data_def$Content))

# If no columns match, clear the stored plots and return early.
if (length(abundance_index) == 0) {
  plot_object_abundanceTab(NULL)
  ggplot_object_abundanceTab(NULL)
  return()
}"
    ),

    h4("Step 3: Data Retransformation"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Reverse any log-transformations stored in data_def$Transformation
# so that the abundance values are on the original (linear) scale
# before applying log2 in the plot.
transformation_df <- data_def$Transformation[abundance_index]
data <- retransform_data_global(data, abundance_index, transformation_df)"
    ),
    p("retransform_data_global() is a shared utility defined outside this ",
      "module. It reverses transformations (e.g., log2, log10) that were ",
      "applied during data import or normalization."),

    h4("Step 4: Subsetting and Labeling"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "df <- data[, abundance_index, drop = FALSE]

# Set column labels based on user choice
if (label_input == \"Column name\") {
  new_labels <- data_def[abundance_index, ][[\"Column\"]]
} else {
  new_labels <- data_def[abundance_index, ][[\"Sample\"]]
}
colnames(df) <- new_labels"
    ),

    h4("Step 5: Reshaping and Cleaning"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Reshape from wide to long format using reshape2::melt
df <- melt(df, variable.name = \"Variable\", value.name = \"Value\")

# Remove non-finite values (NA, NaN, Inf, -Inf) to prevent
# plotting errors
df <- df[is.finite(df$Value), ]"
    ),

    h4("Step 6: Plot Construction"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Delegate to the pure logic function
result <- build_abundances_plot(
  data_long       = df,
  data_abundance  = data_abundance,
  abundance_title = abundance_title,
  color_input     = color_input,
  selected_layout = selected_layout,
  plot_title_size = as.numeric(input$PlotTitleSize_Abundances),
  axis_title_size = as.numeric(input$AxisTitleSize_Abundances),
  tick_size       = as.numeric(input$tickSize_Abundance),
  debug_log       = debug_log
)

# Store results in state
if (!is.null(result)) {
  plot_object_abundanceTab(result$plotly_object)
  ggplot_object_abundanceTab(result$ggplot_object)
}"
    ),

    h3("Download Pipeline"),
    p("The download handler uses device-specific R functions rather than ",
      "ggsave():"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# For raster formats (png, jpeg, tiff):
dev_fun <- match.fun(fmt)
dev_fun(file, width = width, height = height, units = \"in\", res = dpi)

# For vector formats:
svg(file, width = width, height = height)   # SVG
pdf(file, width = width, height = height)   # PDF

# In all cases:
on.exit(try(dev.off(), silent = TRUE), add = TRUE)
print(plt)"
    ),
    p("This approach gives explicit control over device parameters and ",
      "avoids ggsave()'s default behavior.")
  )
}


# ------------------------------------------------------------------------------
# 4. Integration Details
# ------------------------------------------------------------------------------

render_tech_integration_content_abundance <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Required Reactive Values"),
    p("The module expects the following fields on the global rv object:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "rv <- reactiveValues(
  data_mod  = NULL,   # Data frame: processed proteomics data
  data_def  = NULL,   # Data frame: metadata with columns Content,
                      #   Column, Sample, Transformation, ...
  height_px = NULL,   # Numeric: viewport height in pixels
  px_ratio  = NULL    # Numeric: pixel ratio for responsive sizing
)"
    ),

    h4("Metadata Contract (rv$data_def)"),
    p("The data_def data frame must have at least these columns:"),
    tags$ul(
      tags$li(strong("Content:"), " Character. Data type classification. ",
              "The module filters for values matching one of: ",
              "\"Raw Abundance\", \"Normalized Abundance\", ",
              "\"Batch Corrected Abundance\", \"Imputed Raw Abundance\", ",
              "\"Imputed Normalized Abundance\""),
      tags$li(strong("Column:"), " Character. Original column name in the ",
              "data file"),
      tags$li(strong("Sample:"), " Character. Human-readable sample name"),
      tags$li(strong("Transformation:"), " Character. Transformation applied ",
              "during import (e.g., \"log2\", \"log10\", \"none\"). Used by ",
              "retransform_data_global() to reverse the transformation before ",
              "plotting.")
    ),

    h3("UI Integration"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# In the main UI definition
tabPanel(\"Abundances\",
  modAbundancesUI(\"abundance\")
)"
    ),

    h3("Server Integration"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# In the main server function
initialize_module_safely(\"Abundance\", \"modAbundancesServer\",
                         \"abundance\", rv)"
    ),
    p("initialize_module_safely() is a project-level utility that wraps ",
      "moduleServer calls with error handling and debug logging."),

    h3("Plot Grid Integration"),
    p("The add-to-grid observer relies on modEnv$add_to_grid(), a function ",
      "registered in the global module environment by the Plot Grid module. ",
      "The call signature is:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "modEnv$add_to_grid(
  rv,                     # Global reactiveValues
  id     = plot_id,       # Character: unique plot identifier
  plot   = ggplot_obj,    # ggplot2 object
  label  = display_label, # Character: human-readable label
  source = \"Abundances\"   # Character: source module name
)"
    ),
    p("Only ggplot2 objects can be added to the grid. Interactive Plotly ",
      "objects are not supported."),

    h3("Return Interface"),
    p("modAbundancesServer() returns a named list exposing the module's ",
      "reactive state. Other modules can read (but should not write) these:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "list(
  ggplot_object_abundanceTab = state$ggplot_object_abundanceTab,
  plot_object_abundanceTab   = state$plot_object_abundanceTab
)"
    ),

    h3("External Dependencies"),
    p("Functions called from outside the module:"),
    tags$ul(
      tags$li(strong("retransform_data_global(data, indices, transformations)"),
              " -- Reverses column-level transformations; defined in the ",
              "shared utilities"),
      tags$li(strong("initialize_module_safely(name, server_fn, id, rv)"),
              " -- Wraps module initialization with error handling"),
      tags$li(strong("modEnv$add_to_grid(rv, id, plot, label, source)"),
              " -- Registers a plot with the Plot Grid module")
    )
  )
}
