# ==============================================================================
# File: Documentation/venn_doc_user.R
#
# Purpose:
#   User-facing guide for the Venn / UpSet module. The target audience is
#   scientific end users who need clear explanations of the module's workflow
#   without requiring knowledge of the internal Shiny implementation.
# ==============================================================================

venn_doc_callout <- function(title, ..., type = c("info", "success", "warning")) {
  type <- match.arg(type)
  class_name <- switch(
    type,
    info = "alert alert-info",
    success = "alert alert-success",
    warning = "alert alert-warning"
  )

  div(
    class = class_name,
    h4(title),
    ...
  )
}

render_overview_content_venn <- function() {
  div(
    h2("Venn and UpSet Module - Overview"),
    hr(),
    div(
      class = "alert alert-info",
      h4("What this module is for"),
      p(
        "Use this module to compare groups of proteins and identify what is ",
        "shared or unique between them. It is useful when you want to compare ",
        "different experimental conditions, pathway-derived protein sets, GO-term ",
        "protein sets, or proteins detected in selected samples."
      )
    ),
    h3("What you can do"),
    tags$ul(
      tags$li("Enter your own protein lists manually."),
      tags$li("Import proteins from GSEA pathways, GO terms, or selected samples."),
      tags$li("Visualize overlaps as a classic Venn diagram or as an UpSet plot."),
      tags$li("Inspect the proteins in any computed intersection using the intersection selector."),
      tags$li("Download the current figure or export all intersections to Excel."),
      tags$li("Send the current figure to the Plot Grid for multi-panel figure assembly.")
    ),
    h3("When to choose each plot type"),
    tags$dl(
      tags$dt(tags$b("Venn")),
      tags$dd("Best for small comparisons when you want an intuitive visual view of overlaps. The current module supports up to 5 lists for Venn diagrams."),
      tags$dt(tags$b("UpSet")),
      tags$dd("Best when you compare many lists or when you want to read intersection sizes more precisely."),
      tags$dt(tags$b("UpSet with Abundances")),
      tags$dd("Useful when you want to compare overlap structure and also inspect the abundance distribution of proteins in each intersection."),
      tags$dt(tags$b("UpSet with Abundance Ratios")),
      tags$dd("Useful when you want to compare overlap structure together with log2 abundance ratios between two groups of selected samples.")
    ),
    h3("Suggested workflow"),
    tags$ol(
      tags$li("Choose the plot type that fits the number of lists and the question you want to answer."),
      tags$li("Name each list clearly and provide proteins manually or through one of the import options."),
      tags$li("For abundance-based UpSet views, choose the abundance type, identifier column, and relevant samples."),
      tags$li("Click Create Plot."),
      tags$li("Use the intersection selector to inspect the proteins behind a specific overlap."),
      tags$li("Adjust the appearance if needed, then download the figure or export the intersections.")
    )
  )
}

render_diagram_types_content_venn <- function() {
  div(
    h2("Diagram Types"),
    hr(),
    h3("Venn diagrams"),
    p(
      "A Venn diagram shows overlap using partially overlapping shapes. It is ",
      "easy to understand and works well when the comparison is small enough to ",
      "remain visually clear."
    ),
    tags$ul(
      tags$li("Recommended for 2 to 3 lists, and still supported up to 5 lists."),
      tags$li("Most useful when the audience needs an intuitive overview rather than the most precise intersection matrix."),
      tags$li("Good for figures where the main message is that certain groups overlap strongly or contain unique proteins.")
    ),
    h3("UpSet plots"),
    p(
      "An UpSet plot replaces overlapping shapes with a matrix-based view. This ",
      "is often easier to read when the number of lists or intersections becomes ",
      "too large for a Venn diagram."
    ),
    tags$ul(
      tags$li("Better suited than a Venn diagram for larger comparisons."),
      tags$li("Shows intersection size as bars, which makes relative sizes easier to compare."),
      tags$li("Supports extra quantitative information in the abundance-based variants.")
    ),
    h3("Choosing scientifically appropriate visualizations"),
    tags$ul(
      tags$li(tags$b("Use Venn"), " when you want a simple overlap figure and the number of sets is modest."),
      tags$li(tags$b("Use standard UpSet"), " when the main goal is to compare many intersections clearly."),
      tags$li(tags$b("Use UpSet with Abundances"), " when you want overlap structure and abundance distributions in the same figure."),
      tags$li(tags$b("Use UpSet with Abundance Ratios"), " when the biological question depends on comparing two sample groups through log2 abundance ratios.")
    ),
    h3("Quick decision checklist"),
    tags$ul(
      tags$li(tags$b("Number of lists: "), "for 2 to 3 lists (and up to 5 maximum), Venn is usually intuitive; for more complex comparisons, prefer UpSet."),
      tags$li(tags$b("Precise intersection-size comparison: "), "if exact relative intersection sizes are important, UpSet is usually more accurate and easier to compare visually."),
      tags$li(tags$b("Need quantitative abundance or ratio context: "), "if you want overlaps plus abundance distributions or log2 abundance-ratio interpretation, use an abundance-based UpSet variant."),
      tags$li(tags$b("Publication or presentation readability: "), "choose the format that your audience can read quickly at final figure size; Venn is often simpler for small comparisons, while UpSet scales better for dense intersections.")
    ),
    p(
      "Recommendation pattern: start with a Venn diagram for small, intuitive ",
      "comparisons where the overlap story is simple. As soon as the number of ",
      "lists, intersections, or precision needs increase, switch to UpSet; move ",
      "to abundance-based UpSet views when quantitative abundance or ratio ",
      "interpretation is part of the scientific question."
    )
  )
}

render_creating_lists_content_venn <- function() {
  div(
    h2("Creating Protein Lists"),
    hr(),
    h3("Manual list entry"),
    p(
      "Each list card contains a name field and a protein text area. Enter one ",
      "protein identifier per line. Clear naming is important because the list ",
      "names are used in the final figure and in the exported intersections."
    ),
    tags$ul(
      tags$li("Use one identifier type consistently within a plot."),
      tags$li("Choose the matching identifier column in the module settings when you use abundance-based UpSet views."),
      tags$li("Avoid mixing gene symbols, UniProt accessions, and other identifier systems in the same comparison unless you know they refer to the same identifier field in the dataset.")
    ),
    h3("Importing proteins from other module results"),
    p("Each list can also be filled from three selector types:"),
    tags$dl(
      tags$dt(tags$b("GSEA pathways")),
      tags$dd("Select one or more enriched pathways and copy their proteins into the current list."),
      tags$dt(tags$b("GO terms")),
      tags$dd("Choose GO terms from available GO results. Protein import works only when those results include term-member protein data in your current session. Always check the imported proteins in the list box before plotting."),
      tags$dt(tags$b("Samples")),
      tags$dd("Select one or more samples to extract proteins with non-missing, non-zero abundance values for the chosen abundance type.")
    ),
    h3("Managing the number of lists"),
    tags$ul(
      tags$li("Use Add Protein List to create another list card."),
      tags$li("Use Remove Protein List to remove the last list card."),
      tags$li("Use Fill Lists with Random Proteins only as a demonstration or testing aid, not for scientific interpretation.")
    ),
    h3("Practical advice"),
    tags$ul(
      tags$li("If you plan to create a Venn diagram, keep the number of lists at 5 or fewer."),
      tags$li("If you need more lists, switch to an UpSet-based view."),
      tags$li("Check that imported proteins use the same identifier system as your manually entered proteins before mixing them in one plot.")
    )
  )
}

render_venn_diagrams_content_venn <- function() {
  div(
    h2("Venn Diagrams"),
    hr(),
    h3("How to create a Venn diagram"),
    tags$ol(
      tags$li("Select ", strong("Venn"), " as the diagram type."),
      tags$li("Prepare between 1 and 5 protein lists. In practice, 2 to 3 lists are usually easiest to read."),
      tags$li("Assign meaningful names and colours to the lists."),
      tags$li("Click Create Plot."),
      tags$li("Use the intersection selector below the plot to inspect the proteins in each overlap.")
    ),
    h3("What the labels mean"),
    tags$ul(
      tags$li("Each region label is the number of proteins in that overlap."),
      tags$li("If Show Percentages is enabled, the module adds percentages to those overlap labels."),
      tags$li("Percentage labels are computed from total counts across all lists (sum of list sizes), not the union size."),
      tags$li("List titles can be shown or hidden depending on the figure style you need.")
    ),
    h3("How to interpret a Venn diagram scientifically"),
    tags$ul(
      tags$li(tags$b("Unique regions"), " highlight proteins found only in one list."),
      tags$li(tags$b("Pairwise or higher-order overlaps"), " highlight proteins shared by several biological conditions, analyses, or annotation sets."),
      tags$li("A large shared region can indicate common biology, shared pathway membership, or a broadly conserved response."),
      tags$li("A large unique region can indicate condition-specific regulation, selective pathway enrichment, or dataset-specific composition.")
    ),
    h3("Limitations"),
    tags$ul(
      tags$li("Venn diagrams become harder to interpret as the number of lists grows."),
      tags$li("Area perception is not always as precise as a bar-based comparison."),
      tags$li("In heavily overlapping data, percentage labels can be non-intuitive because they are based on summed list sizes rather than the union."),
      tags$li("For larger or more complex comparisons, an UpSet plot is usually the better choice.")
    ),
    p(
      strong("Interpretation tip: "),
      "For rigorous reporting, rely on intersection counts and the exported protein lists."
    )
  )
}

render_upset_plots_content_venn <- function() {
  div(
    h2("UpSet Plots"),
    hr(),
    h3("Why use an UpSet plot"),
    p(
      "An UpSet plot is designed for clear comparison of intersections across ",
      "multiple sets. Instead of relying on overlapping shapes, it shows ",
      "intersections as a structured matrix and a set of bars."
    ),
    h3("Standard UpSet plot"),
    tags$ul(
      tags$li("Intersection bars show how many proteins belong to each combination of sets."),
      tags$li("The matrix below the bars shows which lists are involved in that combination."),
      tags$li("Set-size bars show the overall size of each list.")
    ),
    h3("UpSet with Abundances"),
    tags$ul(
      tags$li("Adds abundance information to the intersection view."),
      tags$li("The plotted values are based on log2-transformed mean abundances."),
      tags$li("You can restrict the mean calculation to selected samples or leave it at the default, which uses all samples available for the chosen abundance type.")
    ),
    h3("UpSet with Abundance Ratios"),
    tags$ul(
      tags$li("Adds quantitative comparison between a numerator sample group and a denominator sample group."),
      tags$li("The displayed values are log2 abundance ratios."),
      tags$li("Positive values indicate higher abundance in the numerator group relative to the denominator group; negative values indicate the opposite.")
    ),
    h3("Important preparation steps for abundance-based UpSet views"),
    tags$ol(
      tags$li("Choose the abundance type that corresponds to the data you want to interpret."),
      tags$li("Choose the protein identifier column that matches the identifiers in your lists."),
      tags$li("For abundance ratios, select biologically meaningful numerator and denominator sample groups."),
      tags$li("Only compare values that are scientifically comparable, for example the same normalization level and appropriate replicate groups.")
    ),
    h3("How to read the results"),
    tags$ul(
      tags$li("Look first at which intersections are large or small."),
      tags$li("Then inspect whether specific intersections show distinct abundance distributions or ratio shifts."),
      tags$li("Use the intersection selector to retrieve the underlying proteins for follow-up analysis.")
    )
  )
}

render_customization_content_venn <- function() {
  div(
    h2("Customization"),
    hr(),
    h3("List-level settings"),
    tags$ul(
      tags$li("Rename each list so the biological meaning is clear in the figure and export."),
      tags$li("Choose colours that remain distinguishable in print and on screen."),
      tags$li("When comparing experimental groups, use colours consistently with the rest of your analysis workflow if possible.")
    ),
    h3("Venn-specific appearance options"),
    tags$ul(
      tags$li("Show or hide list titles."),
      tags$li("Adjust the size of overlap numbers and list titles."),
      tags$li("Change the distance of list titles from the Venn centre."),
      tags$li("Select fonts and font styles for list names and overlap numbers."),
      tags$li("Optionally add percentages to the overlap labels.")
    ),
    h3("UpSet-specific appearance options"),
    tags$ul(
      tags$li("Choose a plot theme for the overall layout."),
      tags$li("Adjust axis-title size, axis-label size, and bar-label size."),
      tags$li("Be careful not to make labels too small for publication figures or too large for comparisons with many intersections.")
    ),
    h3("Practical presentation advice"),
    tags$ul(
      tags$li("For presentations, prioritize readability over compactness."),
      tags$li("For manuscripts, make sure list names, axis labels, and legends remain legible at the final figure size."),
      tags$li("If the figure becomes crowded, simplify the comparison or switch from Venn to UpSet.")
    )
  )
}

render_downloading_content_venn <- function() {
  div(
    h2("Save and Export"),
    hr(),
    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download Plot:"), " saves the current Venn or UpSet plot, respectively."),
      tags$li(tags$b("Add current plot to Grid:"), " sends the current plot to the plot grid for later multi-panel assembly.")
    ),
    h3("Plot file formats"),
    p("The plot download panel supports the following file formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats that depend on the chosen DPI setting."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats that remain sharp when resized and are often preferable for publication editing.")
    ),
    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " control the physical size of the exported figure."),
      tags$li("For wide UpSet plots, the module updates the suggested width automatically to match the expected number of intersections."),
      tags$li(tags$b("DPI:"), " controls output resolution for raster formats such as PNG, JPEG, and TIFF."),
      tags$li("Higher DPI is useful for print-quality raster output, whereas SVG and PDF are resolution-independent.")
    ),
    venn_doc_callout(
      "Practical tip",
      p(
        "If you plan to edit the figure in Illustrator, Inkscape, or another ",
        "vector editor, choose SVG or PDF. If a journal or presentation workflow ",
        "requires a raster image, TIFF at high DPI is often a good choice."
      ),
      type = "info"
    ),
    h3("Export intersections to Excel"),
    tags$ul(
      tags$li("The Excel export contains the input lists, cumulative intersections, and exclusive intersections."),
      tags$li("This is useful when you want to review the exact proteins behind each overlap or continue the analysis outside the app."),
      tags$li("Use this export when a figure alone is not sufficient for reporting or downstream statistics.")
    ),
    h3("Add the current plot to the Plot Grid"),
    tags$ul(
      tags$li("Use the optional label field if you want a specific panel title in the Plot Grid."),
      tags$li("This is useful for building composite figures that combine Venn or UpSet views with other MiraProt plots."),
      tags$li("Make sure the current plot is final before adding it to the grid, because the grid stores the current rendered state."),
      tags$li("Check that axis ranges, colour conventions, and titles are consistent before assembling a multi-panel figure."),
      tags$li("Use per-plot margins to reduce white space around Venn plots.")
    ),
    h3("Good scientific practice"),
    tags$ul(
      tags$li("Keep the exported identifier system consistent with your downstream analysis tools."),
      tags$li("When exporting abundance-based plots, record which abundance type and which sample groups were used."),
      tags$li("For ratio plots, clearly state which samples were assigned to the numerator and denominator groups.")
    )
  )
}
