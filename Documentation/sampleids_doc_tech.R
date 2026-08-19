# ==============================================================================
# File: Documentation/sampleids_doc_tech.R
#
# Purpose:
#   Contains the technical documentation for the Sample IDs module, targeted
#   at developers who need to understand, maintain, or extend the module.
#
# Sourced by:
#   Documentation/sampleids_doc.R (orchestrator)
#
# Sections:
#   1. Technical Overview     - render_tech_overview_content()
#   2. Functions Reference    - render_tech_functions_content()
#   3. Data Processing        - render_tech_data_processing_content()
#   4. Integration Details    - render_tech_integration_content()
# ==============================================================================


# ==============================================================================
# 1. Technical Overview
# ==============================================================================

render_tech_overview_content <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Module Architecture"),
    p("The Sample IDs module follows a four-layer clean architecture pattern. The orchestrator file sources four sub-files, each responsible for a single concern:"),

    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "modules/sampleids_module.R          # Orchestrator: sources sub-files, defines UI/Server
modules/sampleids/
  sampleids_ui.R                     # UI layer: static layout and input widgets
  sampleids_logic.R                  # Logic layer: pure plotting functions (Shiny-free)
  sampleids_state.R                  # State layer: reactive value factory
  sampleids_observer.R               # Observer layer: reactive side-effects, outputs"
    ),

    div(
      class = "alert alert-info",
      p(strong("Design principle:"), " No server logic lives in any sub-file. Sub-files define functions that are called from the orchestrator or from register_sampleids_observers().")
    ),

    h3("Data Flow"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px;",
      "Main App (rv$data_mod, rv$data_def)
     |
     v
Column detection observer
  -> Detects character/numeric column indices
  -> Updates FileSample and data type dropdowns
     |
     v
User clicks 'Create Plot'
     |
     v
Refresh plot observer
  -> Validates inputs and determines column indices
  -> Intersects content type (Found in Sample/File)
     with data type (character/numeric)
  -> Character path: build_sampleids_char_plot()
  -> Numeric path: retransform -> user transform -> build_sampleids_num_plot()
     |
     v
State update (ggplot_object, plotly_object)
     |
     v
Render output (static plot or interactive plotly)"
    ),

    h3("Reactive State"),
    p("All reactive state is centralized in ", code("create_sampleids_state()"), ":"),
    tags$ul(
      tags$li(strong("character_indices:"), " reactiveVal holding integer vector of character column indices from data_mod"),
      tags$li(strong("numeric_indices:"), " reactiveVal holding integer vector of numeric column indices from data_mod"),
      tags$li(strong("ggplot_object_SampleIDTab:"), " reactiveVal holding the current ggplot2 object (or NULL)"),
      tags$li(strong("plotly_object_SampleIDTab:"), " reactiveVal holding the current plotly object (or NULL)")
    ),

    h3("Return Interface"),
    p("The module returns a single-element list for use by other modules:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "list(
  ggplot_object_SampleIDTab = state$ggplot_object_SampleIDTab
)"
    ),

    h3("Dependencies"),
    tags$ul(
      tags$li(strong("shiny:"), " Module framework (UI, server, reactives)"),
      tags$li(strong("ggplot2:"), " Static plot generation and theming"),
      tags$li(strong("plotly:"), " Interactive plot conversion via ggplotly()"),
      tags$li(strong("dplyr:"), " Data manipulation (filter, group_by, summarise, mutate, left_join)"),
      tags$li(strong("reshape2:"), " Data reshaping (melt)"),
      tags$li(strong("viridis:"), " Color palettes"),
      tags$li(strong("scales:"), " Percentage formatting (scales::percent)"),
      tags$li(strong("grid:"), " Unit specifications for theme spacing (grid::unit)")
    ),

    h3("Session Cleanup"),
    p("No explicit session cleanup is registered. The module holds only session-local reactive values that are garbage-collected automatically when the session ends. If persistent external resources are added in the future, register cleanup via the centralized cleanup_manager.")
  )
}


# ==============================================================================
# 2. Functions Reference
# ==============================================================================

render_tech_functions_content <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    # --- sampleids_module.R ---
    h3("sampleids_module.R"),

    h4("modSampleIDsUI(id)"),
    p("UI wrapper. Calls ", code("sampleids_UI(ns)"), " from sampleids_ui.R."),

    h4("modSampleIDsServer(id, rv, debug_level)"),
    p("Server function. Initializes debug logging, creates reactive state via ", code("create_sampleids_state()"), ", registers observers via ", code("register_sampleids_observers()"), ", and returns the public API."),
    tags$ul(
      tags$li(strong("rv:"), " Global reactive values object (required: rv$data_mod, rv$data_def, rv$height_px, rv$px_ratio)"),
      tags$li(strong("debug_level:"), " Integer controlling log verbosity (default: 1)")
    ),

    hr(),

    # --- sampleids_ui.R ---
    h3("sampleids_ui.R"),

    h4("sampleids_UI(ns)"),
    p("Returns a ", code("fluidRow"), " containing the full module layout:"),
    tags$ul(
      tags$li(strong("Left column (width 9):"), " Interactive checkbox, plot output area (dynamic height), download panel (PPI, width, height, format, download button), and grid export controls (label input, add-to-grid button)"),
      tags$li(strong("Right column (width 3):"), " Create Plot button, general options panel (data source, data type, label type, conditional character/numeric options), plot options panel (theme, color palette, invert, title, text sizes, legend position), and reset button")
    ),
    p("All input IDs use the ", code("_SampleIDTab"), " suffix. Dropdown choices for FileSample_SampleIDTab and data_SampleIDTab are NULL by default and populated at runtime by the column detection observer."),

    hr(),

    # --- sampleids_logic.R ---
    h3("sampleids_logic.R"),
    p("Pure logic layer. All functions are Shiny-free and unit-testable."),

    h4("sanitize_plot_id(x)"),
    p("Replaces non-alphanumeric characters with underscores. Used to generate safe plot identifiers for grid integration."),

    h4("get_legend_side(legend_position)"),
    p("Validates a legend position string. Returns one of: ", code('"none"'), ", ", code('"left"'), ", ", code('"right"'), ", ", code('"top"'), ", ", code('"bottom"'), ". Falls back to ", code('"none"'), " on invalid input."),

    h4("get_legend_direction(legend_position)"),
    p("Derives legend direction from position. Returns ", code('"horizontal"'), " for top/bottom, ", code('"vertical"'), " otherwise."),

    h4("apply_sampleids_theme(plot, theme_name, title_size, axis_title_size, tick_size, legend_title_size, legend_text_size, legend_side, legend_dir)"),
    p("Applies a ggplot2 theme (selected via ", code("switch()"), "), sets text sizes, configures legend position and direction, and adds horizontal legend spacing when applicable. Falls back to ", code("theme_minimal()"), " for unrecognized theme names."),

    h4("build_sampleids_char_plot(df, col_indices, data_def, sort_values, abs_rel, label_type, color_input, col_reverse, theme_name, title, ..., debug_log)"),
    p("Builds a bar chart for character string data. Contains two branches:"),
    tags$ul(
      tags$li(strong("More than 10 unique values:"), " Creates a unique-count bar chart with one bar per sample showing the number of distinct string values. Supports Absolute (raw counts) and Relative (proportion) modes."),
      tags$li(strong("Up to 10 unique values:"), " Creates a stacked bar chart grouped by category per sample. Generates custom hover text for plotly with sample name, category, count, total, and proportion.")
    ),
    p("Returns a named list: ", code("list(ggplot_object, plotly_object)"), ". The plotly conversion is wrapped in ", code("tryCatch"), "; returns NULL for the plotly object on failure."),

    h4("build_sampleids_num_plot(df, col_indices, data_def, plot_type, label_type, color_input, col_reverse, theme_name, title, ..., debug_log)"),
    p("Builds a plot for numeric data. Supports three plot types:"),
    tags$ul(
      tags$li(strong("Boxplot:"), " Standard box plot with fill by sample."),
      tags$li(strong("Violinplot:"), " Violin plot with an overlaid box plot (width 0.4, no outlier shapes)."),
      tags$li(strong("Barplot:"), " Bar chart counting positive values only (values > 0).")
    ),
    p("Returns a named list: ", code("list(ggplot_object, plotly_object)"), ", or NULL if plot_type is not recognized."),

    hr(),

    # --- sampleids_state.R ---
    h3("sampleids_state.R"),

    h4("create_sampleids_state()"),
    p("Factory function returning a named list of four reactiveVal containers. Must be called inside ", code("moduleServer()"), " so that the reactive graph context is available."),

    hr(),

    # --- sampleids_observer.R ---
    h3("sampleids_observer.R"),

    h4("register_sampleids_observers(input, output, session, ns, state, rv, debug_log)"),
    p("Registers all observers and outputs for the module. Contains eight blocks:"),

    div(
      class = "well",
      tags$dl(
        tags$dt("a. Column detection observer"),
        tags$dd("Runs on rv$data_mod or rv$data_def changes. Detects character vs. numeric column indices via ", code("sapply(data, is.character/is.numeric)"), ". Updates FileSample_SampleIDTab and data_SampleIDTab dropdowns."),

        tags$dt("b. Character level observer"),
        tags$dd("Triggered by changes to FileSample_SampleIDTab or data_SampleIDTab. Extracts unique character values and updates Sort_SampleIDTab. Limits to 10 unique values; shows error notification if exceeded."),

        tags$dt("c. Dynamic plot UI output"),
        tags$dd("Renders either ", code("plotlyOutput"), " or ", code("plotOutput"), " based on the interactive checkbox. Height is computed from ", code("rv$height_px * rv$px_ratio"), "."),

        tags$dt("d. Static plot output"),
        tags$dd("renderPlot pulling from ", code("state$ggplot_object_SampleIDTab()"), "."),

        tags$dt("e. Plotly output"),
        tags$dd("renderPlotly pulling from ", code("state$plotly_object_SampleIDTab()"), "."),

        tags$dt("f. Refresh plot observer"),
        tags$dd("Core logic. Validates inputs, determines column indices by intersecting content type with data type, delegates to ", code("build_sampleids_char_plot()"), " or ", code("build_sampleids_num_plot()"), ". For numeric data, first calls ", code("retransform_data_global()"), " to undo stored transformations, then applies the user-selected transformation."),

        tags$dt("g. Download handler"),
        tags$dd("Exports the ggplot2 object using ", code("switch()"), " to select the graphics device (png, jpeg, tiff, svg, pdf). Uses on.exit for device cleanup."),

        tags$dt("h. Add-to-grid observer"),
        tags$dd("Validates that a ggplot object exists, generates a sanitized plot ID, and calls ", code("modEnv$add_to_grid(rv, id, plot, label, source)"), ". Shows success or error notification.")
      )
    )
  )
}


# ==============================================================================
# 3. Data Processing
# ==============================================================================

render_tech_data_processing_content <- function() {
  div(
    h2("Data Processing"),
    hr(),

    h3("Column Detection"),
    p("The column detection observer runs whenever ", code("rv$data_mod"), " or ", code("rv$data_def"), " changes:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Detect column types from the data
char_indices <- which(sapply(data, is.character))
num_indices  <- which(sapply(data, is.numeric))

# Identify content types from metadata
found_in_sample_idx <- which(grepl(\"^Found in Sample$\", data_def$Content))
found_in_file_idx   <- which(grepl(\"^Found in File$\",   data_def$Content))

# Determine available data source and type combinations
sample_char_idx <- intersect(char_indices, found_in_sample_idx)
sample_num_idx  <- intersect(num_indices,  found_in_sample_idx)
file_char_idx   <- intersect(char_indices, found_in_file_idx)"
    ),

    h3("Plot Generation Pipeline"),

    h4("Step 1: Input Validation"),
    p("The refresh observer validates:"),
    tags$ul(
      tags$li(code("rv$data_mod"), " and ", code("rv$data_def"), " must be available"),
      tags$li("FileSample selection must be 'Found in Sample' or 'Found in File'"),
      tags$li("Data type must be 'Character strings' or 'Numeric values'"),
      tags$li("The intersection of content indices and type indices must be non-empty"),
      tags$li("The extracted data frame must have at least one column")
    ),

    h4("Step 2: Column Extraction"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Determine which columns to plot
foundin_indices <- which(grepl(foundin_pattern, data_def$Content))
col_indices     <- intersect(foundin_indices, relevant_indices)

# Extract the data subset
df <- data[, col_indices, drop = FALSE]"
    ),

    h4("Step 3a: Character Data Processing"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# Melt to long format
df_long <- melt(df, id.vars = NULL, variable.name = \"Sample\",
                value.name = \"Value\")

# Filter by user-selected sort values (if any)
if (!is.null(sort_values) && length(sort_values) > 0) {
  df_long_filtered <- df_long %>% filter(Value %in% sort_values)
  df_long_filtered$Value <- factor(df_long_filtered$Value,
                                    levels = sort_values)
}

# Branch based on unique value count:
#   > 10 unique values -> unique-count bar chart
#   <= 10 unique values -> stacked bar chart with hover text"
    ),

    h4("Step 3b: Numeric Data Processing"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# 1. Reverse any stored transformation
transformation_df <- data_def$Transformation[col_indices]
data_rt <- retransform_data_global(data, col_indices, transformation_df)
df      <- data_rt[, col_indices, drop = FALSE]

# 2. Apply user-selected transformation
if (transform_sel == \"log2\")   df <- log2(df)
if (transform_sel == \"log10\")  df <- log10(df)
if (transform_sel == \"-log10\") df <- -log10(df)

# 3. Melt and remove NAs
df_long <- melt(df, id.vars = NULL, variable.name = \"Sample\",
                value.name = \"Value\")
df_long <- df_long[!is.na(df_long$Value), ]

# 4. Build plot based on type (Boxplot / Violinplot / Barplot)"
    ),

    h3("Label Resolution"),
    p("Axis labels are determined by the label_type selection:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "sample_labels <- if (label_type == \"Sample name\") {
  labels <- data_def$Sample[col_indices]
  if (is.null(labels) || length(labels) == 0)
    data_def$Column[col_indices]
  else
    labels
} else {
  data_def$Column[col_indices]
}"
    ),

    h3("Color Palette Generation"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "color_palette <- if (color_input == \"gray\") {
  gray.colors(n, start = 0.2, end = 0.8)
} else {
  viridis(n, option = color_input)
}
if (col_reverse) color_palette <- rev(color_palette)"
    ),

    h3("Error Handling"),
    tags$ul(
      tags$li("Input validation uses ", code("req()"), " for required reactive values and ", code("return()"), " with ", code("showNotification()"), " for user-facing errors"),
      tags$li("ggplotly conversion is wrapped in ", code("tryCatch"), "; failures log the error and return NULL for the plotly object"),
      tags$li("The download handler uses ", code("on.exit(try(dev.off(), silent = TRUE))"), " for safe device cleanup"),
      tags$li("The add-to-grid observer validates that the stored object is a ggplot instance via ", code("inherits(p, 'ggplot')"))
    )
  )
}


# ==============================================================================
# 4. Integration Details
# ==============================================================================

render_tech_integration_content <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Required Reactive Values"),
    p("The main application must provide the following via the ", code("rv"), " reactive values object:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "rv <- reactiveValues(
  data_mod  = NULL,  # Processed data frame (character and/or numeric columns)
  data_def  = NULL,  # Metadata data frame (see structure below)
  height_px = NULL,  # Window height in pixels (for responsive plot sizing)
  px_ratio  = NULL   # Device pixel ratio
)"
    ),

    h3("Metadata Structure (data_def)"),
    p("The metadata data frame must contain at minimum:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "data_def <- data.frame(
  Column         = c(...),  # Column names matching data_mod
  Content        = c(...),  # Content type: 'Found in Sample' or 'Found in File'
  Sample         = c(...),  # Sample names for label display (optional)
  Transformation = c(...)   # Stored transformation to reverse before plotting
)"
    ),

    h3("UI Integration"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# In the main UI (e.g., R/ui.R)
tabPanel(
  title = \"Sample IDs\",
  value = \"sampleid\",
  modEnv$modSampleIDsUI(\"sampleid\")
)"
    ),

    h3("Server Integration"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "# In the main server (e.g., R/server_modules.R)
module_outputs$sampleid_out <- modSampleIDsServer(\"sampleid\", rv = rv)"
    ),

    h3("Grid Integration"),
    p("The add-to-grid observer expects ", code("modEnv$add_to_grid"), " to be a function with the signature:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "modEnv$add_to_grid(rv, id, plot, label, source)
# rv     : Global reactive values
# id     : Sanitized plot identifier (e.g., 'sampleid-SampleIDs_default')
# plot   : ggplot2 object
# label  : Display label (user-provided or 'Sample IDs')
# source : Fixed string 'SampleIDs'"
    ),

    h3("External Dependencies"),
    p("The module calls one external function during numeric data processing:"),
    tags$ul(
      tags$li(strong("retransform_data_global(data, col_indices, transformation_df):"), " Reverses previously stored transformations on selected columns before applying the user-selected transformation. Defined outside the module (typically in a shared utilities file).")
    ),

    h3("Module Dependencies"),
    p("Ensure these packages are loaded:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)  # melt()
library(viridis)   # Color palettes
library(scales)    # percent()
library(grid)      # grid::unit()
library(plotly)    # ggplotly() for interactive plots"
    ),

    h3("Common Integration Issues"),
    tags$dl(
      tags$dt("No data sources available in dropdowns:"),
      tags$dd(
        tags$ul(
          tags$li("Verify that data_def$Content contains 'Found in Sample' or 'Found in File' (exact match required)"),
          tags$li("Confirm that column indices in data_def align with data_mod columns"),
          tags$li("Check that rv$data_mod and rv$data_def are both set before the module initializes")
        )
      ),

      tags$dt("Plot not generating after clicking Create Plot:"),
      tags$dd(
        tags$ul(
          tags$li("Verify that the intersection of content type indices and data type indices is non-empty"),
          tags$li("Check the browser console or R console for error notifications"),
          tags$li("For numeric data, ensure retransform_data_global() is available in the global scope")
        )
      ),

      tags$dt("Incorrect axis labels:"),
      tags$dd(
        tags$ul(
          tags$li("When using 'Sample name' labels, data_def$Sample must contain values at the matching indices"),
          tags$li("If Sample values are NULL or empty, the module falls back to Column names")
        )
      ),

      tags$dt("Interactive plot not displaying:"),
      tags$dd(
        tags$ul(
          tags$li("Ensure rv$height_px and rv$px_ratio are available and positive"),
          tags$li("Check that the plotly package is installed and loaded"),
          tags$li("Plotly conversion failures are caught silently; check the debug log for error messages")
        )
      )
    )
  )
}
