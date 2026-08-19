# ==============================================================================
# File: Documentation/abundances_doc_user.R
#
# Purpose:
#   Contains all content-rendering functions for the user-facing guide of the
#   Abundances documentation module. Each function returns a Shiny tag list
#   that describes one section of the user guide.
#
# Functions:
#   1. render_overview_content_abundance()   - Module purpose, features, quick start
#   2. render_creating_content_abundance()   - Data selection, label types, plotting
#   3. render_customizing_content_abundance() - Palettes, themes, text options
#   4. render_interactive_content_abundance() - Static vs interactive mode
#   5. render_downloading_content_abundance() - Export formats, resolution, grid
#
# Notes for future developers:
#   - These functions are called from modAbundancesDocServer() in
#     abundances_doc_ui.R via the content routing switch.
#   - Each function must return a single Shiny tag (typically a div()).
#   - Keep language accessible but scientifically accurate. Avoid jargon
#     where possible; define technical terms when first introduced.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Overview
# ------------------------------------------------------------------------------

render_overview_content_abundance <- function() {
  div(
    h2("Abundances Module - Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p("The Abundances module creates box plot visualizations of protein ",
        "abundance data across samples. It is primarily used to assess data ",
        "distribution, sample quality, and variation patterns at different ",
        "stages of data processing.")
    ),

    h3("Key Features"),
    tags$ul(
      tags$li(strong("Box plots:"), " Visualize abundance distributions ",
              "across all samples in the dataset"),
      tags$li(strong("Multiple data types:"), " Support for raw, normalized, ",
              "batch-corrected, and imputed abundance data"),
      tags$li(strong("Interactive plots:"), " Toggle between static (ggplot2) ",
              "and interactive (Plotly) visualizations"),
      tags$li(strong("Customizable appearance:"), " Color palettes, themes, ",
              "and font sizes can be adjusted"),
      tags$li(strong("High-quality export:"), " Download publication-ready ",
              "figures in vector and raster formats"),
      tags$li(strong("Plot Grid integration:"), " Store plots and combine ",
              "them with outputs from other modules")
    ),

    h3("Typical Use Cases"),
    tags$ul(
      tags$li("Quality control of raw abundance data before processing"),
      tags$li("Identifying outlier samples with unusual distributions"),
      tags$li("Assessing the effectiveness of normalization"),
      tags$li("Comparing abundance distributions before and after ",
              "batch correction or imputation")
    ),

    h3("Quick Start"),
    div(
      class = "alert alert-success",
      p(strong("To create an abundance plot:")),
      tags$ol(
        tags$li("Select a data type under ", strong("General options"),
                " (e.g., Raw Abundance, Normalized Abundance)"),
        tags$li("Choose a label type: ", strong("Column name"), " or ",
                strong("Sample name")),
        tags$li("Click the ", strong("Create Plot"), " button"),
        tags$li("Adjust appearance under ", strong("Plot options"),
                " if desired"),
        tags$li("Click ", strong("Create Plot"), " again to apply changes"),
        tags$li("Download the plot or add it to the Plot Grid")
      )
    ),

    div(
      style = "background-color: #3498db; border-color: #3498db; color: #fff;",
      p(strong("Note:"), " The plot title is automatically set to the ",
        "selected data type. You can overwrite it in the Plot Title field ",
        "before clicking Create Plot.")
    )
  )
}


# ------------------------------------------------------------------------------
# 2. Data Selection and Plotting
# ------------------------------------------------------------------------------

render_creating_content_abundance <- function() {
  div(
    h2("Data Selection and Plotting"),
    hr(),

    h3("Step-by-Step Guide"),

    h4("1. Select Data Type"),
    p("Choose from the available abundance data types. Only types that are ",
      "present in the currently loaded dataset appear in the dropdown:"),
    tags$ul(
      tags$li(p(strong("Raw Abundance:"), " Original, unprocessed values as ",
                "imported from the input file")),
      tags$li(p(strong("Normalized Abundance:"), " Data after normalization ",
                "(e.g., median centering, quantile normalization)")),
      tags$li(p(strong("Batch Corrected Abundance:"), " Data after batch ",
                "effect correction")),
      tags$li(p(strong("Imputed Raw Abundance:"), " Raw data with missing ",
                "values replaced by imputed estimates")),
      tags$li(p(strong("Imputed Normalized Abundance:"), " Normalized data ",
                "with imputed missing values"))
    ),

    div(
      class = "alert alert-info",
      p(strong("Note:"), " Only data types that exist in your dataset will ",
        "be shown. If a type is missing, it has not been generated in the ",
        "Data Wizard module yet.")
    ),

    h4("2. Choose Label Type"),
    p("Controls how sample names appear on the x-axis:"),
    tags$ul(
      tags$li(strong("Column name:"), " Uses the original column headers ",
              "from the data file"),
      tags$li(strong("Sample name:"), " Uses extracted sample names from ",
              "the metadata (default)")
    ),

    h4("3. Generate the Plot"),
    p("Click the ", strong("Create Plot"), " button to render the box plot. ",
      "The module reshapes the selected abundance columns into long format, ",
      "removes non-finite values, and displays one box per sample."),

    h3("Understanding Box Plots"),
    div(
      class = "well",
      h4("Box Plot Components"),
      tags$ul(
        tags$li(strong("Box:"), " Interquartile range (IQR) -- the middle ",
                "50% of the data"),
        tags$li(strong("Median line:"), " The middle value of the ",
                "distribution (50th percentile)"),
        tags$li(strong("Whiskers:"), " Extend to the most extreme data ",
                "point within 1.5 x IQR from the box edges"),
        tags$li(strong("Outliers:"), " Individual points beyond the whiskers"),
        tags$li(strong("Y-axis:"), " Log2-transformed abundance values"),
        tags$li(strong("X-axis:"), " Sample identifiers (column names or ",
                "sample names, depending on label type)")
      )
    ),

    h3("Data Requirements"),
    tags$ul(
      tags$li("Data must be loaded and processed through the Data Wizard module"),
      tags$li("The metadata must contain columns classified as one of the ",
              "five abundance types listed above"),
      tags$li("At least one abundance data type must be available for the ",
              "module to produce output")
    ),

    div(
      class = "alert alert-info",
      h4("Preparing an incompatible table"),
      p(
        "If samples or proteins are arranged in the wrong orientation, return to Datawizard and open ",
        strong("Guide to Preparing Your Data"), ". Use Pivot or Merge there before assigning metadata: the downstream target is one protein per row with abundance measurements in separate columns."
      ),
      tags$ul(
        tags$li("Use Pivot > Transpose when proteins are stored across columns."),
        tags$li("Use Pivot > Wider when a protein repeats in separate condition or contrast rows."),
        tags$li("Use Merge when abundance values and protein annotations are split between tables."),
        tags$li("Recheck the Datawizard preview and metadata types before returning to Abundances.")
      )
    ),

    h3("Interpreting Results"),

    h4("Quality Indicators"),
    tags$dl(
      tags$dt("Consistent medians across samples"),
      tags$dd("Suggests successful normalization or comparable sample ",
              "loading"),

      tags$dt("Similar box sizes"),
      tags$dd("Indicates comparable variance across samples"),

      tags$dt("Few outliers"),
      tags$dd("Indicates good data quality with fewer technical artifacts"),

      tags$dt("Shifted distributions"),
      tags$dd("May indicate batch effects, loading differences, or sample ",
              "degradation -- consider batch correction")
    )
  )
}


# ------------------------------------------------------------------------------
# 3. Customization
# ------------------------------------------------------------------------------

render_customizing_content_abundance <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Color Palettes"),
    p("Select a color scheme from the ", strong("Select color palette"),
      " dropdown. Each sample receives a distinct color from the chosen ",
      "palette:"),

    h4("Available Palettes"),
    tags$dl(
      tags$dt("Viridis family (Magma, Inferno, Plasma, Viridis, Cividis)"),
      tags$dd("Perceptually uniform, colorblind-friendly gradients. ",
              "Recommended for publications."),

      tags$dt("Rocket"),
      tags$dd("Red-to-purple gradient suitable for highlighting extremes"),

      tags$dt("Mako"),
      tags$dd("Blue-green gradient with a calm appearance"),

      tags$dt("Turbo"),
      tags$dd("Rainbow-like spectrum with high color variation"),

      tags$dt("Black and White"),
      tags$dd("Monochrome option -- white fill with black outlines. ",
              "Suitable for grayscale printing.")
    ),

    h3("Plot Themes"),
    p("Choose a ggplot2 theme from the ", strong("Select plot layout"),
      " dropdown to control the overall appearance of the plot:"),

    div(
      class = "well",
      h4("Theme Options"),
      tags$ul(
        tags$li(strong("Gray:"), " Gray panel background with white grid ",
                "lines (ggplot2 default)"),
        tags$li(strong("Black and White:"), " White background with black ",
                "border; no panel shading"),
        tags$li(strong("Linedraw:"), " White background with line-drawn ",
                "elements and clear outlines"),
        tags$li(strong("Light:"), " Light background with subtle grid lines"),
        tags$li(strong("Dark:"), " Dark background with light grid lines"),
        tags$li(strong("Minimal:"), " Minimal decoration; light grid lines only"),
        tags$li(strong("Classic:"), " White background with axis lines in a ",
                "traditional layout (default selection)"),
        tags$li(strong("Void:"), " No axes, grid lines, or background -- ",
                "blank canvas")
      )
    ),

    h3("Text Customization"),

    h4("Plot Title"),
    tags$ul(
      tags$li("The title field auto-fills with the selected data type"),
      tags$li("Overwrite the field with a custom title, or clear it ",
              "for no title"),
      tags$li("Adjust title font size separately (default: 14 pt)")
    ),

    h4("Font Sizes"),
    tags$dl(
      tags$dt("Plot Title Size"),
      tags$dd("Controls the main title text size (default: 14, range: 2-64)"),

      tags$dt("Axis Title Size"),
      tags$dd("Controls the x-axis and y-axis label sizes (default: 12)"),

      tags$dt("Tick Size"),
      tags$dd("Controls the axis tick label sizes (default: 10)")
    ),

    h3("Reset to Defaults"),
    p("Click the ", strong("'Reset to Defaults'"), " button in the sidebar to restore the Plot options controls to their default values. General options are not changed."),

    h3("Applying Changes"),
    p("All customization changes require clicking ", strong("Create Plot"),
      " again to take effect. The module does not update the plot ",
      "automatically when options change.")
  )
}


# ------------------------------------------------------------------------------
# 4. Interactivity
# ------------------------------------------------------------------------------

render_interactive_content_abundance <- function() {
  div(
    h2("Interactivity"),
    hr(),

    h3("Static vs Interactive Plots"),

    h4("Static Plots (Default)"),
    p("Standard display rendered with ggplot2:"),
    tags$ul(
      tags$li("Fixed image output"),
      tags$li("Faster rendering, lower memory usage"),
      tags$li("Exportable via the Download panel in all supported formats")
    ),

    h4("Interactive Plots"),
    p("Enable by checking the ", strong("Interactive Plot"), " checkbox ",
      "above the plot area:"),
    tags$ul(
      tags$li("Rendered using the Plotly library"),
      tags$li("Supports dynamic zoom, pan, and hover tooltips"),
      tags$li("Interactive legend for toggling visibility"),
      tags$li("Best suited for exploratory analysis")
    ),

    h3("Interactive Plot Controls"),
    div(
      class = "well",
      h4("Available Interactions"),
      tags$dl(
        tags$dt("Hover"),
        tags$dd("Displays exact values and sample information for the ",
                "hovered element"),

        tags$dt("Zoom"),
        tags$dd("Click and drag to zoom into a specific region"),

        tags$dt("Pan"),
        tags$dd("Hold Shift and drag to pan across the plot"),

        tags$dt("Reset"),
        tags$dd("Double-click to reset the view to the original extent"),

        tags$dt("Box select"),
        tags$dd("Draw a rectangle to focus on a subset of samples"),

        tags$dt("Screenshot"),
        tags$dd("Use the camera icon in the Plotly toolbar to save the ",
                "current view as a PNG file")
      )
    ),

    h3("Hover Information"),
    p("When hovering over box plot elements, the tooltip displays:"),
    tags$ul(
      tags$li("Sample name"),
      tags$li("Median value"),
      tags$li("Quartile values (Q1, Q3)"),
      tags$li("Minimum and maximum values within the whiskers")
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
                "switch to interactive mode for targeted exploration."
        )
      )
    ),

    div(
      style = paste(
        "background-color: #3498db; border-color: #3498db; color: #fff;",
        "padding: 15px; border-radius: 4px; margin-bottom: 20px;"
      ),
      p(strong("Note:"), " The Download panel exports the static (ggplot2) ",
        "version of the plot regardless of whether interactive mode is ",
        "enabled. To save the interactive view, use the Plotly toolbar's ",
        "camera icon.")
    )
  )
}


# ------------------------------------------------------------------------------
# 5. Save Plots
# ------------------------------------------------------------------------------

render_downloading_content_abundance <- function() {
  div(
    h2("Save Plots"),
    hr(),

    h3("File Formats"),

    h4("Vector Formats"),
    tags$dl(
      tags$dt("PDF"),
      tags$dd(
        tags$ul(
          tags$li("Scalable without quality loss"),
          tags$li("Preserves all plot elements and fonts"),
          tags$li("Recommended for journal submissions")
        )
      ),

      tags$dt("SVG"),
      tags$dd(
        tags$ul(
          tags$li("Editable in vector graphics software ",
                  "(e.g., Illustrator, Inkscape)"),
          tags$li("Web-friendly vector format"),
          tags$li("Good for further post-processing of individual elements")
        )
      )
    ),

    h4("Raster Formats"),
    tags$dl(
      tags$dt("PNG"),
      tags$dd(
        tags$ul(
          tags$li("Lossless compression"),
          tags$li("Supports transparent backgrounds"),
          tags$li("Good balance of quality and file size")
        )
      ),

      tags$dt("JPEG"),
      tags$dd(
        tags$ul(
          tags$li("Lossy compression -- smallest file sizes"),
          tags$li("No transparency support"),
          tags$li("Suitable for presentations or web use")
        )
      ),

      tags$dt("TIFF"),
      tags$dd(
        tags$ul(
          tags$li("Lossless, uncompressed option"),
          tags$li("Required by some journals"),
          tags$li("Largest file sizes; professional print quality")
        )
      )
    ),

    h3("Resolution Settings"),

    h4("PPI (Pixels Per Inch)"),
    p("Controls the pixel density of raster exports (PNG, JPEG, TIFF). ",
      "Has no effect on vector formats (PDF, SVG)."),
    tags$ul(
      tags$li(strong("72-96 PPI:"), " Screen display and web use"),
      tags$li(strong("300 PPI:"), " Standard print quality"),
      tags$li(strong("600 PPI:"), " High-quality printing (default)"),
      tags$li(strong("1200 PPI:"), " Professional printing")
    ),

    h3("Size Settings"),

    h4("Dimensions (in inches)"),
    tags$dl(
      tags$dt("Width (default: 14 inches)"),
      tags$dd("Adjust based on the number of samples and the target medium"),

      tags$dt("Height (default: 12 inches)"),
      tags$dd("Maintain a suitable aspect ratio for readability")
    ),

    h4("Size Guidelines"),
    tags$ul(
      tags$li(strong("Journal figures:"), " Typically 3.5 inches (single ",
              "column) or 7 inches (double column) wide"),
      tags$li(strong("Posters:"), " 10-20 inches wide"),
      tags$li(strong("Slides:"), " Approximately 10 x 7.5 inches"),
      tags$li(strong("Full page:"), " Approximately 7.5 x 10 inches")
    ),

    h3("Download Process"),
    div(
      class = "well",
      h4("Steps"),
      tags$ol(
        tags$li("Create and customize the plot"),
        tags$li("Select the desired format from the dropdown"),
        tags$li("Adjust PPI if using a raster format"),
        tags$li("Set width and height in inches"),
        tags$li("Click the ", strong("Download"), " button"),
        tags$li("Choose a save location and file name in the browser dialog")
      )
    ),

    h3("Troubleshooting"),
    tags$dl(
      tags$dt("Download button does not respond"),
      tags$dd("Ensure that a plot has been created first. Check browser ",
              "download settings if the dialog does not appear."),

      tags$dt("File too large"),
      tags$dd("Reduce dimensions or PPI. Consider using JPEG for smaller ",
              "file sizes."),

      tags$dt("Poor quality when printed"),
      tags$dd("Increase PPI to at least 300, or use a vector format ",
              "(PDF or SVG)."),

      tags$dt("Text appears too small or too large"),
      tags$dd("Adjust font sizes under Plot options before downloading. ",
              "Changing image dimensions alone does not rescale text.")
    ),

    hr(),

    h3("Add to Plot Grid"),
    p("In addition to downloading individual plots, you can store the ",
      "current plot and combine it with other module outputs later."),
    tags$ul(
      tags$li("Click ", strong("Add current plot to Grid"), " to store the ",
              "currently displayed plot together with an optional label"),
      tags$li("Labels are optional; if left blank, the label defaults to ",
              "\"Abundances\""),
      tags$li("Stored plots can be arranged and combined in the ",
              strong("Plot Grid module"), " to create multi-panel figures"),
      tags$li("The Plot Grid accepts plots from all visualization modules, ",
              "not just Abundances")
    ),

    div(
      class = "alert alert-info",
      p(strong("Note:"), " Only the static (ggplot2) version of the plot ",
        "is stored in the grid. Interactive Plotly plots cannot be added.")
    )
  )
}
