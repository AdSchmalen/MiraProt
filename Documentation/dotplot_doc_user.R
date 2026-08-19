############
# User Guide — Content

render_dotplot_overview_content <- function() {
  div(
    h2("Dot Plot Module — Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p(
        "Use the Dot Plot module to compare two quantitative variables at protein level in one figure. ",
        "A dot plot helps you detect trends, outliers, and groups that satisfy biological cutoffs."
      )
    ),

    h3("What a dot plot represents in proteomics and enrichment analysis"),
    tags$ul(
      tags$li(strong("Proteomics context:"), " each dot can represent one protein (or peptide/protein group, depending on your table). Common choices are abundance ratio, intensity, and p-value-derived metrics."),
      tags$li(strong("Enrichment context:"), " dot plots are often used to summarize pathways/terms, where axes and encodings may represent gene ratio, significance, and count."),
      tags$li(strong("In this module:"), " the two axes always come from columns you select in your dataset; point color/size can be globally adjusted and region-specific styling can be used to emphasize cutoffs.")
    ),

    h3("How to read axes and visual encoding"),
    tags$ul(
      tags$li(strong("X- and Y-axes:"), " values from your selected columns after optional transformations (Raw, log2, log10, -log10)."),
      tags$li(strong("Dot position:"), " the primary information: where a protein lies relative to both variables."),
      tags$li(strong("Dot size:"), " in this module, size is a visual emphasis setting (global or region-specific), not an automatic significance metric."),
      tags$li(strong("Dot color gradients:"), " can be meaningful in other dot-plot workflows, but here color is user-defined styling unless you assign semantic meaning through thresholds/regions."),
      tags$li(strong("Ordering/ranking:"), " this is a scatter representation, not a sorted list; interpretation should focus on coordinates and threshold-defined quadrants/regions.")
    ),

    h3("Key Features"),
    tags$ul(
      tags$li(strong("Flexible axes:"), " choose any two columns and set clear axis labels."),
      tags$li(strong("Per-axis transformations:"), " transform each axis independently before plotting."),
      tags$li(strong("Threshold lines:"), " add vertical/horizontal cutoffs with custom style and optional labels."),
      tags$li(strong("Region styling:"), " style threshold-defined regions with custom color, shape, size, and transparency."),
      tags$li(strong("Interactive exploration:"), " use Plotly hover and selection tools to inspect and collect proteins."),
      tags$li(strong("Labeling workflow:"), " annotate selected proteins on the static plot with master and per-protein styling."),
      tags$li(strong("Export and Plot Grid:"), " download high-quality static plots or add the current plot to the Plot Grid.")
    ),

    h3("Typical use cases"),
    tags$ul(
      tags$li("Summarize differential abundance results with fold-change-like and p-value-like axes."),
      tags$li("Highlight proteins in biologically relevant regions such as high ratio + high significance."),
      tags$li("Compare pathway-related proteins imported from GSEA/GO with the global protein distribution."),
      tags$li("Prepare publication figures with consistent thresholds, labels, and export settings.")
    ),

    h3("Common interpretation pitfalls"),
    tags$ul(
      tags$li(strong("Size vs significance confusion:"), " larger points do not automatically mean stronger evidence unless you intentionally encode that meaning."),
      tags$li(strong("Color over-interpretation:"), " custom colors are visual aids; avoid claiming statistical meaning if color was not mapped to a statistic."),
      tags$li(strong("Overplotting:"), " dense clouds can hide points; use transparency, region styling, and interactive zoom/selection."),
      tags$li(strong("Ranking bias:"), " focusing only on extreme points may ignore biologically relevant mid-range patterns."),
      tags$li(strong("Transformation mismatch:"), " always interpret axis values in the transformed scale displayed on the plot.")
    ),

    h3("Quick Start"),
    div(
      class = "alert alert-success",
      tags$ol(
        tags$li("Load data and metadata in Data Wizard."),
        tags$li("Select X and Y columns and confirm axis labels."),
        tags$li("Apply optional axis transformations."),
        tags$li("Define thresholds and optional region styling."),
        tags$li("Create the plot and optionally use interactive selection."),
        tags$li("Apply labels on the static plot and export or add to Plot Grid.")
      )
    )
  )
}

render_dotplot_dataselection_plotting_content <- function() {
  div(
    h2("Data Selection and Plotting"),
    hr(),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      div(
        class = "panel panel-primary",
        div(class = "panel-heading", h4("Step 1 — Load data and metadata (Data Wizard)")),
        div(
          class = "panel-body",
          p("Load your quantitative table and metadata definitions."),
          tags$ul(
            tags$li("Use a stable identifier column (for example gene symbol or accession) if you plan to label proteins."),
            tags$li("Confirm that candidate X/Y columns are numeric.")
          )
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 2 — Choose X/Y columns and axis labels")),
        div(
          class = "panel-body",
          tags$ul(
            tags$li(strong("X-Axis and Y-Axis:"), " choose one column per axis."),
            tags$li(strong("Axis Label:"), " rename to biologically clear text, for example Log2 abundance ratio and -log10 adjusted p-value.")
          )
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 3 — Apply optional transformations")),
        div(
          class = "panel-body",
          p("Choose Raw, log2, log10, or -log10 independently for each axis."),
          div(class = "alert alert-info", "Use -log10 for p-value-like columns when you want larger values to represent stronger statistical evidence.")
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 4 — Configure visual settings")),
        div(
          class = "panel-body",
          tags$ul(
            tags$li("Set plot title and theme."),
            tags$li("Adjust title, axis-title, and tick-label sizes."),
            tags$li("Tune point size and transparency to handle dense regions.")
          )
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5 — Set axis range and tick interval")),
        div(
          class = "panel-body",
          p("Use range sliders to focus on biologically relevant intervals and set tick spacing for readability.")
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 6 — Add and manage thresholds")),
        div(
          class = "panel-body",
          tags$ul(
            tags$li("Add vertical (X) and/or horizontal (Y) thresholds."),
            tags$li("Set line color, style, thickness, and optional label text."),
            tags$li("Select threshold rows in the table to remove or edit them.")
          )
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 7 — Optional region-specific styling")),
        div(
          class = "panel-body",
          p("Thresholds partition the plotting space into regions. Select a region tile and override point color, size, transparency, and shape.")
        )
      ),

      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 8 — Create and review the plot")),
        div(
          class = "panel-body",
          p("Click Create Plot to render the static view. Then enable Interactive Plot for selection tasks if needed.")
        )
      )
    ),

    h3("Interpretation checklist"),
    tags$ul(
      tags$li("Confirm axis transformations before interpreting values."),
      tags$li("Check whether threshold lines reflect your analysis criteria."),
      tags$li("Use region colors consistently across plots to avoid ambiguity."),
      tags$li("Inspect dense areas interactively to reduce overplotting bias.")
    )
  )
}

render_dotplot_customizing_content <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Title, Labels, and Theme"),
    tags$ul(
      tags$li("Set an informative plot title or hide it if the figure caption will carry the context."),
      tags$li("Use explicit axis labels that describe both quantity and scale."),
      tags$li("Choose a theme consistent with your manuscript or presentation style.")
    ),

    h3("Text and Point Appearance"),
    tags$ul(
      tags$li("Adjust title, axis-title, and tick text sizes for final output dimensions."),
      tags$li("Set global point size and transparency before applying region-specific overrides."),
      tags$li("For crowded data, prefer smaller points and increased transparency.")
    ),

    h3("Axis Controls"),
    tags$ul(
      tags$li("Range sliders control visible bounds and can focus attention on relevant value intervals."),
      tags$li("Tick intervals should balance readability and precision."),
      tags$li("Reset Ranges restores automatic range behavior after manual adjustments.")
    ),

    h3("Threshold Lines"),
    tags$ol(
      tags$li("Add a threshold."),
      tags$li("Choose orientation (vertical or horizontal)."),
      tags$li("Provide the threshold value."),
      tags$li("Style line color, type, and thickness; optionally add a label.")
    ),
    div(
      class = "alert alert-info",
      "Threshold values are interpreted in the axis scale displayed in the plot."
    ),

    h3("Region-specific Styling"),
    tags$ul(
      tags$li("Region tiles appear when thresholds are present."),
      tags$li("Each region can override color, shape, size, and transparency."),
      tags$li("Use region styling to emphasize biological decision areas (for example, high ratio + low p-value)."),
      tags$li("Reset all region styling to return to global point aesthetics.")
    )
  )
}


render_dotplot_interactive_content <- function() {
  div(
    h2("Interactivity"),
    hr(),

    h3("Enable interactive mode"),
    p("Activate Interactive Plot to switch from static ggplot rendering to Plotly rendering."),

    h3("Hover and selection tools"),
    tags$ul(
      tags$li(strong("Hover:"), " inspect identifier and transformed X/Y values."),
      tags$li(strong("Click:"), " select a single point."),
      tags$li(strong("Box/Lasso:"), " select groups of points."),
      tags$li(strong("Zoom and pan:"), " inspect dense clusters in detail.")
    ),

    h3("Identifier handling"),
    p(
      "Interactive selections use the active Identifier Column. ",
      "If you change the identifier column, make a new selection to keep names consistent."
    ),

    h3("Selection management"),
    tags$ul(
      tags$li("Selected items are shown in the interactive selection panel."),
      tags$li("Copy to Clipboard exports selected identifiers as a line-separated list."),
      tags$li("Clear Selection removes the current interactive selection.")
    ),

    div(
      class = "alert alert-success",
      p(
        strong("Recommended workflow:"),
        " select proteins in interactive mode, then return to static mode for final label placement and export."
      )
    )
  )
}


render_dotplot_labeling_content <- function() {
  div(
    h2("Labeling"),
    hr(),

    h3("Goal"),
    p("Labeling annotates selected proteins on the static plot to create readable, publication-ready figures."),

    h3("Select identifier and build label list"),
    tags$ul(
      tags$li("Choose the Identifier Column used for search, selection, and labels."),
      tags$li("Add identifiers by typing/pasting one per line."),
      tags$li("Optionally import proteins from selected GSEA/GO pathways."),
      tags$li("Use intersection/core-enriched options when pathway-focused selection is needed.")
    ),

    h3("Style labels and labeled points"),
    tags$ul(
      tags$li("Master controls define default label and dot colors for the full selection."),
      tags$li("Per-protein controls override label color and optional custom dot color."),
      tags$li("Adjust label size, line thickness, dot size for labeled points, max overlaps, and label distance.")
    ),

    h3("Apply and maintenance actions"),
    tags$ul(
      tags$li(strong("Apply Settings & Label:"), " writes labels to the static plot."),
      tags$li(strong("Reset Colors:"), " restores default color settings."),
      tags$li(strong("Clear All Labels:"), " removes labels while preserving the main plot."),
      tags$li(strong("Clear Selection:"), " clears selected identifiers.")
    ),

    div(
      class = "alert alert-info",
      "Labeling is applied to the static plot. Interactive mode is intended for selection and exploration."
    )
  )
}

render_dotplot_saveplots_content <- function() {
  div(
    h2("Save and Export"),
    hr(),

    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download:"), " saves the current static dot plot."),
      tags$li(tags$b("Add current plot to Grid:"), " sends the current static plot to the Plot Grid for multi-panel figure assembly.")
    ),

    h3("Plot file formats"),
    p("The download panel supports the following formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats that depend on the selected DPI value."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats that remain sharp when resized and are suitable for post-editing.")
    ),

    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " define the physical size of the exported figure in inches."),
      tags$li(tags$b("DPI (PPI):"), " controls raster resolution for PNG/JPEG/TIFF output."),
      tags$li("Higher DPI is preferred for print-quality raster output; vector formats are resolution-independent.")
    ),

    div(
      class = "alert alert-info",
      strong("Practical tip: "),
      "Choose SVG or PDF when you expect downstream editing (for example in Illustrator or Inkscape). ",
      "Use TIFF or PNG at high DPI when a raster workflow is required by journals or slide tools."
    ),

    h3("Add the current plot to the Plot Grid"),
    tags$ul(
      tags$li("Use the optional label field to define a panel title in the grid."),
      tags$li("Add only finalized plot states, because the grid stores the plot as rendered at insertion time."),
      tags$li("Keep titles, threshold logic, color semantics, and axis scales consistent before assembling multi-panel figures."),
      tags$li("Combine dot plots with complementary modules in the same Plot Grid layout for integrated result storytelling.")
    ),

    h3("Good scientific practice"),
    tags$ul(
      tags$li("Report which columns and transformations were used on each axis."),
      tags$li("Document threshold values and biological/statistical rationale for those cutoffs."),
      tags$li("Do not assign inferential meaning to visual styles (size/color) unless that mapping is explicitly defined."),
      tags$li("Preserve the identifier system across exported plots and downstream tables for reproducibility.")
    ),

    div(
      class = "alert alert-info",
      strong("Note: "),
      "The download panel exports the static plot. Interactive Plotly toolbar export is separate and does not replace the controlled static export workflow."
    )
  )
}
