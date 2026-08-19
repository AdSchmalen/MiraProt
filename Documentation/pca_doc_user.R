# ==============================================================================
# File: Documentation/pca_doc_user.R
#
# Purpose:
#   Contains all content-rendering functions for the user-facing guide of the
#   Dimensionality Reduction documentation module. Each function returns a
#   Shiny tag list that describes one section of the user guide.
#
# Note:
#   The orchestrator file pca_doc.R has been removed. This file and
#   pca_doc_tech.R provide all documentation content. pca_doc_ui.R provides
#   the navigation UI and server routing. All three files are sourced into
#   modEnv automatically by the alphabetical list.files() loop in app.R.
#
# Functions:
#   1. render_dimred_overview_content()      - Overview and quick start
#   2. render_dimred_pca_content()           - PCA concepts and interpretation
#   3. render_dimred_umap_content()          - UMAP concepts and interpretation
#   4. render_dimred_customizing_content()   - Visual and labeling options
#   5. render_dimred_interactive_content()   - Interactive exploration features
#   6. render_downloading_content_dimred()   - Save/export guidance
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Overview
# ------------------------------------------------------------------------------

render_dimred_overview_content <- function() {
  div(
    h2("Dimensionality Reduction Module - Overview"),
    hr(),
    div(
      class = "alert alert-info",
      h4("What this module is for"),
      p(
        "Use this module to ask a simple scientific question: which samples or ",
        "which proteins behave similarly in your dataset? The module offers two ",
        "ways to summarize many abundance measurements into a two-dimensional ",
        "view: Principal Component Analysis (PCA) and UMAP."
      )
    ),
    h3("Two analysis perspectives"),
    p(
      "Before creating a plot, choose what each point should represent. The ",
      "same dataset can be viewed from two complementary perspectives."
    ),
    tags$dl(
      tags$dt(tags$b("Samples")),
      tags$dd(
        tags$ul(
          tags$li("Each point is one sample."),
          tags$li("The analysis matrix is transposed so that samples become rows and proteins become columns. Each sample is then positioned based on the overall protein abundance pattern it contains."),
          tags$li("Useful for checking whether replicates cluster together, whether conditions separate, and whether any samples look like outliers."),
          tags$li("When more than one experimental condition is present, the plot automatically draws convex-hull polygons around the samples belonging to each condition, making group separation easier to assess visually."),
          tags$li("A good first view when you want to assess data quality or broad biological structure.")
        )
      ),
      tags$dt(tags$b("Proteins")),
      tags$dd(
        tags$ul(
          tags$li("Each point is one protein."),
          tags$li("The analysis matrix is used as-is, with proteins as rows and samples as columns. Each protein is positioned based on its abundance pattern across the selected samples."),
          tags$li("Useful for finding proteins with similar abundance patterns, which may point to shared regulation or pathway membership."),
          tags$li("Helpful when you want to build a candidate list for labeling, follow-up analysis, or pathway interpretation.")
        )
      )
    ),
    div(
      class = "alert alert-success",
      h4("When to use PCA and when to use UMAP"),
      tags$ul(
        tags$li(
          tags$b("Start with PCA"),
          ": PCA is usually the most straightforward first plot. It summarizes the dominant sources of variation and tells you how much variance each component explains. Use it when you want an interpretable overview, a scree plot, and a clear sense of the strongest trends in the data."
        ),
        tags$li(
          tags$b("Use UMAP when structure may be more complex"),
          ": UMAP is often useful when groups overlap in PCA or when you suspect local subclusters, gradients, or non-linear relationships. It can reveal neighborhood structure more clearly, but its axes do not have a direct variance-based interpretation."
        )
      )
    ),
    h3("What you can do in this module"),
    tags$ul(
      tags$li("Choose samples or proteins as the plotted entities."),
      tags$li("Run PCA or UMAP on the currently selected abundance table and samples."),
      tags$li("Display the main scatter plot and, for PCA, an optional scree plot."),
      tags$li("See automatic condition-group boundaries (convex-hull polygons) in sample plots when multiple conditions are present."),
      tags$li("Adjust colors, themes, legends, point size, and text size."),
      tags$li("Label all samples or selected proteins in the static plot, with per-protein color customization."),
      tags$li("Use the interactive Plotly view to inspect points and select proteins."),
      tags$li("Download the main plot, download the scree plot, export analysis tables, or send plots to the Plot Grid."),
      tags$li("Clear stored results to reset the module state when starting a new analysis.")
    ),
    h3("Suggested workflow"),
    tags$ol(
      tags$li("Select the abundance table and the samples you want to include."),
      tags$li("Choose whether you want to compare samples or proteins."),
      tags$li("Start with PCA to see the dominant structure in the data."),
      tags$li("If needed, try UMAP to inspect local clustering or subtle subgroup structure."),
      tags$li("Switch on the interactive plot when you want to hover, zoom, or select proteins."),
      tags$li("Return to the static plot when you want to refine labels and prepare a figure for export.")
    )
  )
}

# ------------------------------------------------------------------------------
# 2. PCA
# ------------------------------------------------------------------------------

render_dimred_pca_content <- function() {
  div(
    h2("PCA"),
    hr(),
    h3("What PCA shows"),
    p(
      "PCA summarizes the strongest coordinated changes in the data into a set ",
      "of principal components. In practice, this helps you see whether the ",
      "largest differences in protein abundance are associated with biology, ",
      "batch, sample quality, or a smaller number of unusual observations."
    ),
    h3("Main PCA scatter plot"),
    tags$ul(
      tags$li("Each point is either a sample or a protein, depending on the selected comparison mode."),
      tags$li("Points close together have similar abundance patterns across the features used in the PCA."),
      tags$li("A clear separation between groups suggests that the selected principal components capture biologically or technically important differences."),
      tags$li("A distant point may indicate an outlier sample, a distinct subgroup, or a protein with an unusual pattern across samples."),
      tags$li("In sample mode, points are automatically colored by experimental condition. When more than one condition is present, convex-hull polygons are drawn around each condition group to make group boundaries easier to see."),
      tags$li("You can change the X and Y axes to view different pairs of components, for example PC1 vs PC2 or PC2 vs PC3.")
    ),
    h4("How to read the axes"),
    tags$ul(
      tags$li("PC1 explains the largest fraction of variance captured by the PCA, PC2 the next largest, and so on."),
      tags$li("Axis labels are therefore ordered by captured variance, not by biological importance."),
      tags$li("If a known biological grouping appears on PC2 or PC3 rather than PC1, that can still be scientifically meaningful.")
    ),
    h3("Scree plot"),
    p(
      "The scree plot shows how much variance is explained by each principal ",
      "component. This helps you judge whether the first few components capture ",
      "most of the structure, or whether the variance is spread across many ",
      "components."
    ),
    tags$ul(
      tags$li("Tall first bars mean the leading components capture a large part of the signal."),
      tags$li("A rapid drop from PC1 to later components suggests that the main structure is concentrated in the first few PCs."),
      tags$li("A flatter scree plot suggests that the dataset is more complex or that no single axis dominates."),
      tags$li("Use the scree plot to decide whether it is worth inspecting component pairs beyond PC1 and PC2.")
    ),
    h3("About loadings"),
    p(
      "PCA loadings describe how strongly individual proteins contribute to each ",
      "principal component. In the current module, loadings are available in the ",
      "exported data tables, but there is no separate loadings plot exposed in ",
      "the user interface."
    ),
    h3("Practical examples"),
    tags$ul(
      tags$li(tags$b("Sample PCA:"), " replicates clustering tightly is reassuring; separation by condition suggests a strong biological effect; separation by run order or batch may indicate a technical effect."),
      tags$li(tags$b("Protein PCA:"), " clusters of proteins suggest shared behavior across samples and can help you choose proteins to label or export for downstream interpretation."),
      tags$li(tags$b("Outliers:"), " always check whether an isolated point reflects true biology, sample preparation issues, missingness, or annotation problems.")
    )
  )
}

# ------------------------------------------------------------------------------
# 3. UMAP
# ------------------------------------------------------------------------------

render_dimred_umap_content <- function() {
  div(
    h2("UMAP"),
    hr(),
    h3("What UMAP shows"),
    p(
      "UMAP places points so that nearby points in the high-dimensional data ",
      "tend to remain nearby in two dimensions. It is often helpful when you ",
      "want to inspect local neighborhoods, subclusters, or gradual transitions ",
      "that are not easy to see in PCA."
    ),
    h3("How UMAP differs from PCA"),
    tags$ul(
      tags$li("PCA emphasizes the strongest global variance structure and provides variance percentages for each principal component. It is a linear method: the axes are linear combinations of the original features."),
      tags$li("UMAP emphasizes neighborhood relationships and can make local grouping easier to see. It is a non-linear method that can capture structure PCA may miss."),
      tags$li("UMAP axes are coordinates of the embedding only. They are not percentages, they do not represent explained variance, and they should not be interpreted like PC1 or PC2."),
      tags$li("Distances in a UMAP plot are most informative at the local level: nearby points are usually meaningful, but the exact spacing between distant clusters should not be over-interpreted. Two clusters far apart in a UMAP plot are not necessarily more different than two clusters that are moderately separated."),
      tags$li("UMAP results can vary with parameter settings (neighbors, minimum distance, metric). Running UMAP with different parameters and comparing the layouts is good practice when the structure is ambiguous.")
    ),
    h3("Main UMAP settings"),
    h4("Number of neighbors"),
    p(
      "This setting controls how many nearby points are used when UMAP builds ",
      "the map. Smaller values focus more on local structure and can separate ",
      "small clusters. Larger values give a smoother, more global view and can ",
      "reduce over-fragmentation."
    ),
    h4("Minimum distance"),
    p(
      "This setting controls how tightly points are allowed to pack together in ",
      "the final two-dimensional layout. Lower values produce tighter clusters ",
      "and more empty space between groups. Higher values produce a more spread ",
      "out map."
    ),
    h4("Distance metric"),
    p(
      "The metric defines what it means for two samples or proteins to be ",
      "similar. Euclidean uses straight-line distance, Manhattan sums absolute ",
      "differences across dimensions, and Cosine emphasizes pattern similarity ",
      "more than absolute magnitude. Different metrics can highlight different ",
      "aspects of the same dataset."
    ),
    h3("How to interpret a UMAP plot"),
    tags$ul(
      tags$li("Tight groups suggest similar abundance profiles within those samples or proteins."),
      tags$li("Bridges or gradients between groups can suggest continuous biological change rather than sharply separated classes."),
      tags$li("If the picture changes strongly after tuning neighbors, minimum distance, or metric, treat the pattern as exploratory rather than definitive."),
      tags$li("For publication-style interpretation, it is often useful to compare the UMAP result back to PCA and to known sample annotations.")
    )
  )
}

# ------------------------------------------------------------------------------
# 4. Customization
# ------------------------------------------------------------------------------

render_dimred_customizing_content <- function() {
  div(
    h2("Customization"),
    hr(),
    h3("Data and analysis controls"),
    tags$ul(
      tags$li(tags$b("Select data type:"), " choose which abundance table to analyze, for example normalized, batch-corrected, or imputed data."),
      tags$li(tags$b("Select samples:"), " define which samples contribute to the analysis."),
      tags$li(tags$b("Identifier Column:"), " choose the protein or gene identifier field used for protein-level labeling and exports."),
      tags$li(tags$b("Compare:"), " switch between sample-centered and protein-centered views."),
      tags$li(tags$b("Analysis Method:"), " choose PCA or UMAP."),
      tags$li(tags$b("Interactive Plot:"), " switch between a static figure for labeling/export and a Plotly view for exploration and protein selection.")
    ),
    h3("PCA-specific controls"),
    tags$ul(
      tags$li(tags$b("X-Axis and Y-Axis:"), " choose which principal components are shown in the main scatter plot."),
      tags$li(tags$b("Show Scree Plot:"), " display the scree plot tab when you are using PCA."),
      tags$li(tags$b("Scale data before PCA:"), " switch scaling on or off before PCA is calculated.")
    ),
    p(
      "When scaling is on, very large abundance values are toned down so they ",
      "do not dominate the PCA plot. For sample PCA, proteins are made more ",
      "comparable with each other; for protein PCA, selected samples are made ",
      "more comparable with each other. Turn scaling off if you want the ",
      "original size differences in your data to influence the PCA more strongly."
    ),
    h3("UMAP-specific controls"),
    tags$ul(
      tags$li(tags$b("Number of Neighbors:"), " tunes how local versus global the UMAP structure should be."),
      tags$li(tags$b("Minimum Distance:"), " tunes how compact the clusters may appear."),
      tags$li(tags$b("Distance Metric:"), " chooses how similarity is measured: euclidean, manhattan, or cosine.")
    ),
    h3("Plot styling controls"),
    tags$ul(
      tags$li(tags$b("Color Palette and Reverse Colors:"), " available for sample plots to control group colors. Condition groups are colored automatically and, when more than one condition is present, convex-hull polygons are drawn around each group."),
      tags$li(tags$b("Default Protein Color:"), " sets the baseline color for protein plots before individual protein highlighting."),
      tags$li(tags$b("Plot Theme:"), " choose among Classic, Minimal, Gray, Black & White, and Light."),
      tags$li(tags$b("Legend Position:"), " place the legend on the right, left, top, bottom, or hide it."),
      tags$li(tags$b("Point Size:"), " adjust marker size for dense versus sparse plots."),
      tags$li(tags$b("Axis Title Size, Tick Size, Legend Title Size, Legend Text Size:"), " refine text readability for screen viewing or export.")
    ),
    h3("Module management"),
    tags$ul(
      tags$li(tags$b("Reset to Defaults:"), " restores the right-side control panels to their default UI values without removing stored analysis results or plot objects.")
    ),
    h3("Label controls"),
    p(
      "Label controls are available in the static plot view. Protein labeling ",
      "and sample labeling are handled separately because they support ",
      "different workflows."
    ),
    h4("Protein labeling controls"),
    tags$ul(
      tags$li("Search for proteins by gene symbol, or import proteins from enriched GSEA or GO results."),
      tags$li("Add proteins to a selected list, copy that list to the clipboard, and reuse it later."),
      tags$li("Adjust label color, dot color, dot size, label size, label distance, line thickness, and allowed overlap before applying labels."),
      tags$li("Each protein can receive its own label color and dot color, allowing visual emphasis of specific proteins of interest."),
      tags$li("Reset colors, clear labels, or clear the current protein selection when needed.")
    ),
    h4("Sample labeling controls"),
    tags$ul(
      tags$li("Apply labels to all displayed samples in the static plot."),
      tags$li("Adjust the same core appearance settings: label color, dot color, dot size, label size, line thickness, label distance, and overlap allowance."),
      tags$li("Reset label settings or clear labels if the plot becomes too crowded.")
    )
  )
}

# ------------------------------------------------------------------------------
# 5. Interactive Features
# ------------------------------------------------------------------------------

render_dimred_interactive_content <- function() {
  div(
    h2("Interactive Features"),
    hr(),
    div(
      class = "alert alert-info",
      p(
        "The interactive Plotly view is mainly designed for protein selection ",
        "and protein labeling workflows. It is useful for exploration, but the ",
        "main figure export workflow is based on the static plot."
      )
    ),
    h3("Hover and zoom"),
    tags$ul(
      tags$li("Hover over a point to see its identity and plotted coordinates."),
      tags$li("Use zoom to inspect dense regions or closely spaced proteins."),
      tags$li("Pan or reset the view from the Plotly toolbar when you want to navigate around the plot.")
    ),
    h3("Protein selection workflows"),
    p(
      "Interactive selection is available in protein comparison mode. This is ",
      "the main way to collect proteins directly from the scatter plot for ",
      "later labeling or reporting. Selection is not available in sample mode ",
      "because sample plots typically contain few points that can be labeled ",
      "directly using the sample labeling controls."
    ),
    tags$ul(
      tags$li(tags$b("Click:"), " select an individual protein."),
      tags$li(tags$b("Box select:"), " drag a rectangle to select multiple proteins in one region."),
      tags$li(tags$b("Lasso select:"), " draw a free-form selection around an irregular cluster."),
      tags$li(tags$b("Ctrl + drag:"), " add another group of proteins to the current selection instead of replacing it.")
    ),
    h3("Clipboard export and label reuse"),
    tags$ul(
      tags$li("Use Copy to Clipboard to export the current protein selection as a text list."),
      tags$li("This list can be reused in the protein search/selection panel of the static workflow."),
      tags$li("A practical approach is to explore interactively first, copy the proteins of interest, then return to the static plot to apply labels and finalize the figure.")
    ),
    h3("Selection management"),
    tags$ul(
      tags$li("The selected proteins are shown in a dedicated selection panel."),
      tags$li("Clear Selection removes the current Plotly selection if you want to start again."),
      tags$li("Interactive exploration is especially useful when many proteins overlap and cannot be separated easily in a static figure.")
    )
  )
}

# ------------------------------------------------------------------------------
# 6. Save Plots
# ------------------------------------------------------------------------------

render_downloading_content_dimred <- function() {
  div(
    h2("Save Plots and Export Results"),
    hr(),
    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download Main Plot:"), " saves the current static PCA or UMAP scatter plot."),
      tags$li(tags$b("Download Scree Plot:"), " saves the scree plot when the current analysis is PCA."),
      tags$li(tags$b("Download Data:"), " exports the analysis tables as an Excel workbook, including coordinates and method-specific results. For PCA, loadings are also included in the workbook.")
    ),
    h3("Plot file formats"),
    p("The plot download panel supports the following file formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats that depend on the chosen PPI value."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats that stay sharp when resized and are often preferable for publication editing.")
    ),
    h3("Dimensions and PPI"),
    tags$ul(
      tags$li(tags$b("Width (inches) and Height (inches):"), " control the physical size of the exported figure."),
      tags$li(tags$b("PPI:"), " controls output resolution for raster formats such as PNG, JPEG, and TIFF."),
      tags$li("Higher PPI is useful for print-quality raster output, whereas SVG and PDF are resolution-independent.")
    ),
    div(
      class = "alert alert-info",
      p(
        strong("Practical tip:"),
        " if you plan to edit the figure in Illustrator, Inkscape, or another ",
        " vector editor, choose SVG or PDF. If a journal requests a raster ",
        " figure, TIFF at high PPI is often a good choice."
      )
    ),
    h3("Plot Grid integration"),
    p(
      "Instead of downloading immediately, you can send the current plot to the ",
      " Plot Grid and combine it with figures from this or other modules."
    ),
    tags$ul(
      tags$li("Choose whether to send the Main Plot or the Scree Plot."),
      tags$li("Optionally enter a label so the plot is easy to recognize in the grid."),
      tags$li("Click Add current plot to Grid to store it for later multi-panel assembly."),
      tags$li("The scree plot can only be added when a PCA result is available.")
    ),
    h3("Notes about interactive plots"),
    p(
      "The main module download workflow exports the static ggplot-based views. ",
      "Use the static plot when you need a reproducible, labeled figure for ",
      "saving or for Plot Grid assembly."
    )
  )
}
