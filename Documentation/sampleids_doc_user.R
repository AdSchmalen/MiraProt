# ==============================================================================
# File: Documentation/sampleids_doc_user.R
#
# Purpose:
#   Provides user-oriented documentation for the Sample IDs module.
#   Each function returns a Shiny tagList or div containing the rendered
#   content for one section of the user guide.
#
# Sourced by:
#   Documentation/sampleids_doc.R (orchestrator)
#
# Sections:
#   1. Overview              - render_overview_content()
#   2. Data Selection/Plotting - render_data_selection_plotting_content_sampleids()
#      (internally calls render_data_types_content and render_creating_content)
#   3. Customization         - render_customizing_content()
#   4. Interactivity         - render_interactive_content_sampleids()
#   5. Save Plots            - render_downloading_content_sampleids()
# ==============================================================================


# ==============================================================================
# 1. Overview
# ==============================================================================

render_overview_content <- function() {
  div(
    h2("Sample IDs Module - Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p("The Sample IDs module visualizes the distribution of protein identifications across samples or files, helping you understand data completeness.")
    ),

    h3("Key Features"),
    tags$ul(
      tags$li(strong("Distribution visualization:"), " Stacked bar plots showing protein counts or percentages"),
      tags$li(strong("Dual data sources:"), " Analyze 'Found in Sample' or 'Found in File' data"),
      tags$li(strong("Data type flexibility:"), " Handle both character strings and numeric values"),
      tags$li(strong("Multiple plot types:"), " Boxplot, violin plot, or bar plot for numeric data"),
      tags$li(strong("Sorting and filtering:"), " Select and reorder categories for character data"),
      tags$li(strong("Data transformations:"), " Apply log2, log10, or -log10 transformations to numeric data"),
      tags$li(strong("Absolute/Relative views:"), " Switch between count and percentage displays for character data"),
      tags$li(strong("Flexible labeling:"), " Choose between column names or sample names as axis labels")
    ),

    h3("Common Use Cases"),
    tags$ul(
      tags$li("Assess sample coverage across experiments"),
      tags$li("Identify samples with missing proteins"),
      tags$li("Compare protein identification rates"),
      tags$li("Quality control for mass spectrometry runs"),
      tags$li("Visualize replicate consistency")
    ),

    h3("Quick Start"),
    div(
      class = "alert alert-success",
      p(strong("To create your first Sample ID visualization:")),
      tags$ol(
        tags$li("Select a data source ('Found in Sample' or 'Found in File')"),
        tags$li("Choose a data type ('Character strings' or 'Numeric values')"),
        tags$li("Click ", strong("'Create Plot'"), " to generate the visualization"),
        tags$li("Adjust display and styling options as needed"),
        tags$li("Click ", strong("'Create Plot'"), " again to apply changes"),
        tags$li("Download the plot or add it to the plot grid")
      )
    )
  )
}


# ==============================================================================
# 2. Data Selection and Plotting (composite section)
# ==============================================================================

#' @describeIn render_data_types_content
#'   Wrapper that combines the Data Types and Creating Visualizations
#'   subsections into a single page.
render_data_selection_plotting_content_sampleids <- function() {
  div(
    h2("Data Selection and Plotting"),
    hr(),
    div(
      class = "alert alert-info",
      p("This section covers how to select data sources and data types, and how to create and interpret visualizations.")
    ),
    render_data_types_content(),
    tags$hr(),
    render_creating_content()
  )
}

render_data_types_content <- function() {
  div(
    h3("Data Sources"),

    h4("Found in Sample"),
    p("This option analyzes protein identification at the sample level:"),
    tags$ul(
      tags$li("Shows which samples contain each protein"),
      tags$li("Useful for biological replicate analysis"),
      tags$li("Helps identify sample-specific proteins"),
      tags$li("Reveals patterns in experimental groups")
    ),

    h4("Found in File"),
    p("This option analyzes protein identification at the file level:"),
    tags$ul(
      tags$li("Shows which raw data files contain each protein"),
      tags$li("Useful for technical replicate assessment"),
      tags$li("Helps identify file-specific issues"),
      tags$li("Reveals batch effects or instrument problems")
    ),

    h3("Value Types"),

    h4("Character Strings"),
    p("When your data contains text values:"),
    tags$ul(
      tags$li("Sample names (e.g., 'Control_1', 'Treatment_2')"),
      tags$li("Categorical labels (e.g., 'Present', 'Absent')"),
      tags$li("Grouped categories for visualization"),
      tags$li("e.g. 'High', 'Medium', 'Low', 'Peak Found'")
    ),
    div(
      class = "alert alert-info",
      p(strong("Note:"), " Character data is displayed as stacked bar plots. When there are more than 10 unique values, the module switches to a unique-count bar chart showing the number of distinct strings per sample.")
    ),

    h4("Numeric Values"),
    p("When your data contains numerical values:"),
    tags$ul(
      tags$li("Protein counts per sample"),
      tags$li("Identification scores"),
      tags$li("Quantitative measurements"),
      tags$li("Statistical values")
    ),
    div(
      class = "alert alert-info",
      p(
        tags$strong("Note:"),
        " Numeric values can be visualized using three plot types: boxplots, violin plots, or bar plots.",
        " You can apply data transformations (log2, log10, -log10) from the selection menu.",
        " Bar plots display counts of identified proteins, excluding values equal to zero or missing values."
      )
    )
  )
}

render_creating_content <- function() {
  div(
    h3("Creating Visualizations"),

    h4("1. Select Data Source"),
    tags$ul(
      tags$li("Choose between 'Found in Sample' or 'Found in File'"),
      tags$li("Only sources present in the loaded data are shown"),
      tags$li("Selection updates the available data types automatically")
    ),

    h4("2. Choose Data Type"),
    tags$ul(
      tags$li("Select 'Character strings' for categorical data"),
      tags$li("Select 'Numeric values' for quantitative data"),
      tags$li("The module detects available types automatically based on column content")
    ),

    h4("3. Select Labels"),
    tags$ul(
      tags$li(strong("Column name:"), " Use original column headers as axis labels"),
      tags$li(strong("Sample name:"), " Use sample names extracted from metadata")
    ),

    h4("4. Configure Data-Specific Options"),

    div(
      class = "well",
      h5("For Character Data:"),
      tags$ul(
        tags$li("Use the multi-select dropdown to choose which character values to include"),
        tags$li("If there are 10 or fewer unique values, all are selected by default"),
        tags$li("If there are more than 10 unique values, the selection is cleared and a notification is shown"),
        tags$li("Switch between ", strong("Absolute"), " (counts) and ", strong("Relative"), " (percentages) display modes")
      ),

      h5("For Numeric Data:"),
      tags$ul(
        tags$li("Select a plot type: ", strong("Boxplot"), ", ", strong("Violinplot"), ", or ", strong("Barplot")),
        tags$li(HTML("Choose a data transformation: Untransformed, log<sub>2</sub>, log<sub>10</sub>, or -log<sub>10</sub>")),
        tags$li("The default plot type is Boxplot; the default transformation is Untransformed")
      )
    ),

    h4("5. Generate the Plot"),
    p("Click ", strong("'Create Plot'"), " to generate the visualization. The plot must be regenerated after changing any settings."),

    div(
      class = "alert alert-info",
      h4("Preparing an incompatible table"),
      p(
        "Sample IDs expects the table prepared in Datawizard to retain one protein per row and correctly classified Found in Sample or Found in File columns. If those values are spread across files, columns, or repeated rows, open ",
        strong("Guide to Preparing Your Data"), " in Datawizard before plotting."
      ),
      tags$ul(
        tags$li("Transpose a table when proteins are stored in columns rather than rows."),
        tags$li("Pivot wider when each protein appears once per sample, condition, or contrast."),
        tags$li("Merge secondary annotations or identification results by a shared protein identifier."),
        tags$li("Inspect the transformed preview, then confirm the identification columns' metadata assignments before returning to Sample IDs.")
      )
    ),

    h3("Interpreting Visualizations"),

    h4("Expected Patterns"),
    tags$ul(
      tags$li(tags$strong("Uniform distributions:"), " indicate good consistency across samples."),
      tags$li(tags$strong("High-confidence IDs:"), " should typically be the most abundant identification type."),
      tags$li(tags$strong("Distinct group patterns:"), " may reflect underlying experimental conditions.")
    ),

    h4("Warning Patterns"),
    tags$ul(
      tags$li(tags$strong("Missing bars:"), " indicate samples with no or failed identifications."),
      tags$li(tags$strong("Uneven bar heights:"), " suggest potential issues with sample quality or loading."),
      tags$li(tags$strong("Bimodal distributions:"), " can indicate batch effects or mixed populations.")
    )
  )
}


# ==============================================================================
# 3. Customization
# ==============================================================================

render_customizing_content <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Visual Customization Options"),

    h4("Color Palettes"),
    p("Choose from various color schemes:"),
    tags$dl(
      tags$dt("Viridis Family (Magma, Inferno, Plasma, Viridis, Cividis):"),
      tags$dd("Perceptually uniform, colorblind-friendly palettes"),

      tags$dt("Rocket:"),
      tags$dd("Red to purple gradient"),

      tags$dt("Mako:"),
      tags$dd("Blue-green gradient"),

      tags$dt("Turbo:"),
      tags$dd("Rainbow spectrum"),

      tags$dt("Gray:"),
      tags$dd("Grayscale")
    ),

    p("Use the 'Invert color' checkbox to reverse any palette."),

    h4("Theme Selection"),
    p("Apply different plot themes:"),
    tags$ul(
      tags$li(strong("Gray:"), " Gray panel background with grid lines and axis lines."),
      tags$li(strong("Black and White:"), " White background with black lines; no panel shading."),
      tags$li(strong("Linedraw:"), " White background with line-drawn elements and clear outlines."),
      tags$li(strong("Light:"), " Light background with subtle grid lines and axes."),
      tags$li(strong("Dark:"), " Dark background with light grid lines and axes."),
      tags$li(strong("Minimal:"), " Minimal background elements; light grid lines and axes only."),
      tags$li(strong("Classic:"), " White background with axis ticks and borders in a traditional layout (default)."),
      tags$li(strong("Void:"), " No axes, ticks, text, or background elements (empty canvas).")
    ),

    h4("Legend Position"),
    p("Control where the legend appears on the plot:"),
    tags$ul(
      tags$li(strong("None:"), " No legend displayed (default)."),
      tags$li(strong("Right / Left:"), " Legend placed vertically beside the plot."),
      tags$li(strong("Top / Bottom:"), " Legend placed horizontally above or below the plot.")
    ),

    h3("Text Customization"),

    h4("Plot Title"),
    tags$ul(
      tags$li("Enter a custom title in the text field"),
      tags$li("Default title: 'Sample IDs'"),
      tags$li("Leave empty for no title")
    ),

    h4("Text Size Controls"),
    div(
      class = "well",
      tags$dl(
        tags$dt("Title Size (2-64):"),
        tags$dd("Main plot title size (default: 14)"),

        tags$dt("Axis Title Size (2-64):"),
        tags$dd("X and Y axis label sizes (default: 12)"),

        tags$dt("Tick Size (2-64):"),
        tags$dd("Axis tick label sizes (default: 10)"),

        tags$dt("Legend Title Size (2-64):"),
        tags$dd("Legend header size (default: 12)"),

        tags$dt("Legend Text Size (2-64):"),
        tags$dd("Legend item sizes (default: 10)")
      )
    ),

    h3("Reset to Defaults"),
    p("Click the ", strong("'Reset to Defaults'"), " button in the sidebar to restore the Plot options controls to their default values. General options are not changed."),

    div(
      class = "alert alert-info",
      h4("Best Practices"),
      tags$ul(
        tags$li("Match color scheme to your presentation or publication style"),
        tags$li("Increase text sizes for posters and presentations"),
        tags$li("Use grayscale for black-and-white printing"),
        tags$li("Keep consistent styling across all figures"),
        tags$li("Test readability at actual display size")
      )
    )
  )
}


# ==============================================================================
# 4. Interactivity
# ==============================================================================

render_interactive_content_sampleids <- function() {
  div(
    h2("Interactivity"),
    hr(),

    h3("Static vs Interactive Plots"),

    h4("Static Plots (Default)"),
    p("Traditional plot display using ggplot2:"),
    tags$ul(
      tags$li("Fixed image display"),
      tags$li("Faster rendering"),
      tags$li("Lower memory usage"),
      tags$li("Available for download via the Download panel")
    ),

    h4("Interactive Plots"),
    p("Enable by checking the 'Interactive Plot' checkbox:"),
    tags$ul(
      tags$li("Powered by the Plotly library"),
      tags$li("Dynamic zoom and pan"),
      tags$li("Hover tooltips with values"),
      tags$li("Interactive legend toggling"),
      tags$li("Best suited for data exploration")
    ),

    h3("Interactive Plot Controls"),
    div(
      class = "well",
      h4("Available Interactions:"),
      tags$dl(
        tags$dt("Hover:"),
        tags$dd("Display exact values, sample names, categories, and counts"),

        tags$dt("Zoom:"),
        tags$dd("Click and drag to zoom into specific regions"),

        tags$dt("Pan:"),
        tags$dd("Drag while holding shift to move around"),

        tags$dt("Reset:"),
        tags$dd("Double-click to reset the view"),

        tags$dt("Box select:"),
        tags$dd("Draw a rectangle to focus on specific samples"),

        tags$dt("Download:"),
        tags$dd("Use the camera icon in the toolbar to save the current view as PNG")
      )
    ),

    h3("Hover Information"),
    p("When hovering over stacked bar chart elements (character data):"),
    tags$ul(
      tags$li("Sample name"),
      tags$li("Category"),
      tags$li("Count for that category"),
      tags$li("Total identifications for the sample"),
      tags$li("Proportion (when in Relative mode)")
    ),

    h3("Performance Considerations"),
    div(
      style = paste(
        "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
        "padding: 15px; border-radius: 4px; margin-bottom: 20px;"
      ),
      h4("Large Datasets"),
      tags$ul(
        tags$li("Interactive plots may be noticeably slower with datasets ",
                "containing more than 100 samples."),
        tags$li("Consider using static plots for an initial overview, then ",
                "switch to interactive mode for targeted exploration."),
        tags$li("In interactive mode, zoom into regions of interest for ",
                "closer inspection.")
      )
    )
  )
}


# ==============================================================================
# 5. Save Plots
# ==============================================================================

render_downloading_content_sampleids <- function() {
  div(
    h2("Save Plots"),
    hr(),

    h3("File Formats"),

    h4("Vector Formats:"),
    tags$dl(
      tags$dt("PDF:"),
      tags$dd(
        tags$ul(
          tags$li("Scalable without quality loss"),
          tags$li("Preserves all plot elements"),
          tags$li("Larger file sizes")
        )
      ),

      tags$dt("SVG:"),
      tags$dd(
        tags$ul(
          tags$li("Editable in vector graphics software"),
          tags$li("Web-friendly vector format"),
          tags$li("Compatible with Illustrator/Inkscape")
        )
      )
    ),

    h4("Raster Formats:"),
    tags$dl(
      tags$dt("PNG:"),
      tags$dd(
        tags$ul(
          tags$li("Lossless compression"),
          tags$li("Transparent background support"),
          tags$li("Good quality-to-size ratio")
        )
      ),

      tags$dt("JPEG:"),
      tags$dd(
        tags$ul(
          tags$li("Smallest file sizes"),
          tags$li("Lossy compression"),
          tags$li("No transparency support")
        )
      ),

      tags$dt("TIFF:"),
      tags$dd(
        tags$ul(
          tags$li("Lossless, uncompressed option"),
          tags$li("Required by some journals"),
          tags$li("Large file sizes"),
          tags$li("Professional printing quality")
        )
      )
    ),

    h3("Resolution Settings"),

    h4("PPI (Pixels Per Inch):"),
    tags$ul(
      tags$li(strong("72-96 PPI:"), " Screen display, web use"),
      tags$li(strong("300 PPI:"), " Standard print quality"),
      tags$li(strong("600 PPI:"), " High-quality printing (default)"),
      tags$li(strong("1200 PPI:"), " Professional printing")
    ),

    div(
      class = "alert alert-info",
      p(strong("Note:"), " PPI only affects raster formats (PNG, JPEG, TIFF). Vector formats (PDF, SVG) are resolution-independent.")
    ),

    h3("Size Settings"),

    h4("Dimensions in Inches:"),
    tags$dl(
      tags$dt("Width (default: 14 inches):"),
      tags$dd("Adjust based on number of samples and target use"),

      tags$dt("Height (default: 12 inches):"),
      tags$dd("Maintain aspect ratio for best appearance")
    ),

    h4("Size Guidelines:"),
    tags$ul(
      tags$li(strong("Journal figures:"), " Often 3.5\" (single column) or 7\" (double column) wide"),
      tags$li(strong("Posters:"), " 10-20 inches wide"),
      tags$li(strong("Slides:"), " 10 inches wide x 7.5 inches tall"),
      tags$li(strong("Full page:"), " 7.5 x 10 inches")
    ),

    h3("Download Process"),
    div(
      class = "well",
      h4("Steps to Download:"),
      tags$ol(
        tags$li("Create and customize your plot"),
        tags$li("Select the desired format from the dropdown"),
        tags$li("Adjust PPI for raster formats"),
        tags$li("Set width and height in inches"),
        tags$li("Click ", strong("'Download'"), ""),
        tags$li("Choose a save location and filename")
      )
    ),

    h3("Troubleshooting Downloads"),
    tags$dl(
      tags$dt("Download button not working:"),
      tags$dd("Ensure a plot has been created first; check browser download settings"),

      tags$dt("File too large:"),
      tags$dd("Reduce dimensions or PPI, or use JPEG format"),

      tags$dt("Poor quality when printed:"),
      tags$dd("Increase PPI to 600 or use a vector format (PDF/SVG)"),

      tags$dt("Text too small/large:"),
      tags$dd("Adjust text sizes in the Plot options panel before downloading, not image dimensions")
    ),

    p(
      em(
        "Note: The interactive Plotly view can only be saved via the Plotly toolbar. ",
        "The Download panel exports the static ggplot2 plot."
      )
    ),

    hr(),

    h3("Add to Plot Grid"),
    p(
      "In addition to downloading individual plots, you can store the current plot ",
      "and combine it later with other plots in a multi-panel figure."
    ),
    tags$ul(
      tags$li(
        "Click ", tags$b("Add current plot to Grid"), " to store the currently displayed plot."
      ),
      tags$li(
        "Optionally enter a short label to identify the plot in the grid."
      ),
      tags$li(
        "Stored plots can be accessed and arranged in the ", tags$b("Plot Grid module"),
        " to create composite figures."
      ),
      tags$li(
        "Plots from other modules can be added to the same grid."
      )
    )
  )
}
