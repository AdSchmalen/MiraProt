# ./Documentation/volcano_doc_user.R
# Volcano documentation for scientific end users

render_volcano_user_overview_content <- function() {
  div(
    h2("Volcano Module User Guide"),
    hr(),
    volcano_doc_callout(
      "Scientific purpose",
      p(
        "A volcano plot combines effect size and statistical evidence in a single view. ",
        "In MiraProt, the x-axis shows log2 fold change and the y-axis shows either ",
        "-log10 adjusted p-values or -log10 raw p-values. Proteins that lie far from ",
        "zero and high on the plot are the strongest candidates for condition-dependent ",
        "changes in abundance."
      ),
      type = "info"
    ),
    h3("What the module helps you do"),
    tags$ul(
      tags$li("Compare one or more experimental contrasts from the same dataset."),
      tags$li("Identify proteins with both large abundance changes and statistical support."),
      tags$li("Inspect individual proteins interactively before deciding which ones to highlight."),
      tags$li("Produce static figures for reporting and reuse them in the MiraProt plot grid.")
    ),
    h3("How the interface is organised"),
    tags$ul(
      tags$li(strong("Main workspace:"), " the large left-side area where the selected volcano plot, annotation tools, and saving controls appear."),
      tags$li(strong("Control panel:"), " the right-side column where you choose identifiers, p-value display, thresholds, appearance, axis ranges, and reset behaviour."),
      tags$li(strong("Mode-dependent panels:"), " some controls are shown only in static mode or only in interactive mode, depending on whether Interactive Plot is enabled.")
    ),
    h3("How significance is shown"),
    tags$ul(
      tags$li(strong("Increased:"), " proteins above the positive fold-change threshold and above the p-value threshold line."),
      tags$li(strong("Decreased:"), " proteins below the negative fold-change threshold and above the p-value threshold line."),
      tags$li(strong("Not highlighted:"), " proteins that do not pass both criteria at the same time.")
    )
  )
}

render_volcano_user_data_content <- function() {
  div(
    h2("Data Requirements"),
    hr(),
    h3("Required inputs"),
    tags$ul(
      tags$li(strong("Processed data table:"), " one row per protein or feature."),
      tags$li(strong("Abundance ratio columns:"), " columns describing the comparison of interest."),
      tags$li(strong("P-value or adjusted p-value columns:"), " statistical evidence for the same comparison."),
      tags$li(strong("Metadata annotations:"), " the uploaded metadata should clearly identify which columns contain ratios, p-values, and identifiers. Information about numerator and denominator groups helps the module pair the correct columns automatically."),
      tags$li(strong("Identifier column:"), " a column suitable for labels, hover text, and search, such as a gene symbol or protein name.")
    ),
    h3("Transformation handling"),
    p(
      "The module can work with columns that are already transformed, as long as the ",
      "metadata correctly describes the transformation. Internally, the values are ",
      "brought into a consistent plotting space so that different datasets behave in ",
      "a comparable way."
    ),
    h3("Practical preparation advice"),
    tags$ul(
      tags$li("Prefer adjusted p-values when your aim is confirmatory interpretation rather than exploratory screening."),
      tags$li("Use identifiers that are easy to recognize scientifically, especially if you plan to annotate proteins in the final figure."),
      tags$li("Check the metadata assignments in the Data Wizard if an expected comparison is missing or paired incorrectly."),
      tags$li("Interpret fold changes and p-values in the context of the underlying statistical workflow, sample size, and data quality.")
    )
  )
}

render_volcano_user_workflow_content <- function() {
  div(
    h2("Using the Interface"),
    hr(),
    h3("Recommended workflow"),
    tags$ol(
      tags$li("Load the dataset and metadata in the upstream modules so that abundance ratios, p-values, and identifiers are available."),
      tags$li("Open the Volcano module and confirm the correct identifier column and p-value display."),
      tags$li("Click Create Plot to detect available comparisons and generate one volcano plot for each matched ratio and p-value pair."),
      tags$li("If several comparisons are available, choose the one you want to inspect in Select Plot."),
      tags$li("Set fold-change and p-value thresholds that match the scientific stringency of your question."),
      tags$li("Only after the thresholds are satisfactory, refine the visual appearance of the plot."),
      tags$li("Use the interactive plot for rapid inspection of outliers, clusters, or candidate proteins."),
      tags$li("If you import proteins from pathway analyses, transfer them into the search field and then add only the exact matches present in the dataset."),
      tags$li("Move the final shortlist into the static annotation workflow and apply labels."),
      tags$li("Save the figure or send it to the plot grid for multi-panel figure assembly.")
    ),
    volcano_doc_callout(
      "Important behavior",
      p(
        "Once at least one plot has been created, changing colours, sizes, theme, ",
        "title, or axis settings updates the appearance without rebuilding the ",
        "underlying comparison. Changing the p-value display or significance ",
        "thresholds recalculates the plotted classification."
      ),
      type = "success"
    ),
    h3("Main UI areas"),
    tags$ul(
      tags$li(strong("Plot workspace:"), " displays the selected comparison and, once plots exist, lets you switch between detected ratio and p-value pairs."),
      tags$li(strong("Static view:"), " shows the ggplot-based figure together with the panels for annotation, saving, and sending the selected plot to the grid."),
      tags$li(strong("Interactive view:"), " shows the plotly-based figure for hover inspection, click selection, and box or lasso selection."),
      tags$li(strong("Control panel:"), " groups plot settings, appearance, thresholds, axis controls, and reset options into one settings column."),
      tags$li(strong("Pathway import area in static mode:"), " lets you load proteins from selected enrichment results into the search field, optionally keeping only proteins shared across selected pathways.")
    ),
    h3("Static versus interactive mode"),
    tags$ul(
      tags$li(strong("Interactive Plot unchecked:"), " use this mode when you want to annotate proteins, save the figure, or add the selected plot to the grid."),
      tags$li(strong("Interactive Plot checked:"), " use this mode when you want to inspect points dynamically and collect candidate proteins directly from the plot."),
      tags$li("Interactive selections support discovery, but they are not transferred as labels automatically.")
    ),
    h3("Control panel settings that matter most"),
    tags$ul(
      tags$li(strong("P-value display:"), " switches between adjusted and raw p-values and therefore changes the interpretation of the y-axis."),
      tags$li(strong("Identifier column:"), " defines which names are used for hover text, search suggestions, exact matching, and labels."),
      tags$li(strong("Fold-change and p-value thresholds:"), " define which proteins are highlighted as changed with statistical support."),
      tags$li(strong("Theme, title, text sizes, point sizes, and colours:"), " control the visual presentation after the comparison has been created."),
      tags$li(strong("Axis controls and Reset to Defaults:"), " let you tune the visible x and y range and restore the built-in defaults with recalculated ranges when data are available.")
    ),
    h3("Search and exact matching"),
    tags$ul(
      tags$li("Suggested identifiers show both partial and exact matches from the dataset."),
      tags$li("Significantly more abundant and Significantly less abundant add the matching proteins from the selected plot to the search field using the current thresholds and identifier column."),
      tags$li("Add transfers only exact matches into the selected-protein list used for static annotation."),
      tags$li("Importing pathway proteins fills the search field, but it does not annotate proteins by itself."),
      tags$li("This approach reduces the risk of highlighting the wrong protein when names are similar.")
    )
  )
}

render_volcano_user_interpretation_content <- function() {
  div(
    h2("Interpretation"),
    hr(),
    h3("How to read the axes"),
    tags$ul(
      tags$li(strong("X-axis:"), " log2 fold change. Positive values indicate higher abundance in the numerator condition, whereas negative values indicate lower abundance relative to that condition."),
      tags$li(strong("Y-axis:"), " -log10 p-value. Larger values indicate stronger statistical evidence against no difference between the compared groups."),
      tags$li(strong("Dashed guide lines:"), " show the active fold-change and p-value thresholds used to highlight proteins.")
    ),
    h3("Method-selection guidance"),
    p("Use volcano plots as one view within a broader analysis strategy, and choose the method that best matches the scientific question."),
    h4("When volcano plots are appropriate"),
    tags$ul(
      tags$li("Candidate prioritization when you need effect size and statistical significance in the same view for one or more contrasts."),
      tags$li("Rapid triage of proteins for follow-up experiments, annotation, or reporting based on combined magnitude and evidence."),
      tags$li("Comparison-focused interpretation where the primary goal is to rank differential abundance candidates rather than summarize global sample geometry.")
    ),
    h4("When other methods may be preferable"),
    tags$ul(
      tags$li(strong("PCA:"), " preferred when the main question is global sample structure, grouping, or outlier detection across samples."),
      tags$li(strong("Heatmaps:"), " preferred when you want to visualize expression or abundance patterns across many samples and feature sets."),
      tags$li(strong("Pathway analyses:"), " preferred when you want to detect coordinated biological shifts where each individual protein change may be modest.")
    ),
    h3("How to interpret biological patterns"),
    tags$ul(
      tags$li("Proteins high on the left or right combine a larger abundance difference with stronger statistical support and are often the main candidates for follow-up interpretation."),
      tags$li("A dense cloud around the centre usually reflects proteins with small effect sizes, weak statistical support, or both."),
      tags$li("Broad asymmetry between the left and right sides can reflect genuine biology, but may also arise from sample heterogeneity, missing values, normalization choices, or batch effects."),
      tags$li("A statistically supported change is not automatically a biologically important change. Consider effect size, pathway context, prior knowledge, and experimental design together.")
    ),
    h3("Choosing thresholds"),
    tags$ul(
      tags$li(strong("Exploratory vs confirmatory use:"), " exploratory screening often uses less strict settings to avoid missing potentially interesting proteins, while confirmatory interpretation usually applies stricter settings to reduce false positives."),
      tags$li(strong("Adjusted vs raw p-values:"), " adjusted p-values are usually preferred when many proteins are tested and you want stronger control of false discoveries; raw p-values can be useful for early-stage hypothesis generation."),
      tags$li(strong("Fold-change trade-off:"), " increasing the fold-change threshold improves specificity (fewer likely false positives) but lowers sensitivity (more true changes may be missed), whereas lowering it does the opposite."),
      tags$li("There is no universal cutoff that fits every experiment. Choose thresholds in the context of sample size, measurement variability, and your study objective."),
      tags$li("Treat proteins close to any threshold line with caution, because small data or preprocessing differences may change their classification.")
    ),
    h3("Good scientific practice when reading volcano plots"),
    tags$ul(
      tags$li("Use adjusted p-values when controlling for multiple testing is important for your study design."),
      tags$li("Be cautious with proteins close to the threshold lines, because small preprocessing or modeling changes may move them across the cutoff."),
      tags$li("Very large fold changes with limited statistical support can occur when measurements are variable or based on few observations."),
      tags$li("Very small p-values with tiny fold changes may be statistically convincing but biologically modest."),
      tags$li("Whenever possible, interpret candidate proteins together with pathway analyses, replicate behaviour, and orthogonal validation.")
    ),
    h3("Interactive selection workflow"),
    tags$ol(
      tags$li("Enable Interactive Plot to switch from the static figure to the interactive plotly view."),
      tags$li("Inspect proteins by hovering or select them by clicking, box-selecting, or lasso-selecting points."),
      tags$li("Review the selected identifiers in the interactive protein selection panel."),
      tags$li("Copy the identifiers and transfer the proteins you want to annotate into the static workflow.")
    ),
    volcano_doc_callout(
      "Interactive selection and final annotation",
      p(
        "Interactive selection and static annotation use different interface panels ",
        "and different internal selection states. A practical workflow is to use ",
        "interactive mode for discovery and static mode for the final labeled figure."
      ),
      type = "info"
    )
  )
}

render_volcano_user_customization_content <- function() {
  div(
    h2("Customization"),
    hr(),
    h3("Annotation workflow in static mode"),
    tags$ul(
      tags$li("The annotation area contains manual search, pathway-based import, identifier suggestions, and controls for the appearance of highlighted proteins."),
      tags$li("Selected proteins are managed as an explicit list so you can review and adjust them before applying labels."),
      tags$li("Apply Settings & Label updates the displayed static plot."),
      tags$li("Clear All Labels removes labels from the selected plot only, while Reset Colors restores the annotation colour settings.")
    ),
    h3("Text customization"),
    tags$ul(
      tags$li("Adjust label size to balance readability against crowding in dense regions."),
      tags$li("Modify axis title size and tick-label size so the figure remains legible on screen and after export."),
      tags$li("Use title settings to create a clear figure heading that matches the selected comparison."),
      tags$li("Choose label distances and overlap settings carefully to keep annotations readable without misplacing them too far from their points.")
    ),
    h3("Colour and point customization"),
    tags$ul(
      tags$li("Set different colours for increased, decreased, and non-highlighted proteins to make the biological contrast easy to recognize."),
      tags$li("Use per-protein label and point colours when you want to emphasize a small number of biologically important candidates."),
      tags$li("Adjust point size according to plot density: smaller points often help for large datasets, whereas larger points can improve visibility in sparse plots."),
      tags$li("Keep colour choices consistent across related figures if you compare several contrasts in the same study.")
    ),
    h3("Layout and axis customization"),
    tags$ul(
      tags$li("Choose a plot theme that matches the style of your report, poster, or manuscript."),
      tags$li("Refine the visible x-axis and y-axis ranges when you want to focus on the main cloud of points or preserve comparability across plots."),
      tags$li("Reset to Defaults is useful if repeated adjustments make the plot difficult to interpret."),
      tags$li("When comparing several volcano plots side by side, use similar axis ranges and styling so visual differences reflect the data rather than formatting.")
    )
  )
}

render_volcano_user_export_content <- function() {
  div(
    h2("Save and Export"),
    hr(),
    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download Plot:"), " saves the selected static volcano plot."),
      tags$li(tags$b("Add current plot to Grid:"), " sends the selected static volcano plot to the plot grid for later multi-panel assembly."),
      tags$li("Volcano supports plot export and Plot Grid integration from the static view."),
      tags$li("There is no dedicated table-export button in the Volcano module user interface."),
      tags$li("The interactive plot is intended for exploration; for reproducible figure export, return to the static plot.")
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
      tags$li(tags$b("DPI:"), " controls output resolution for raster formats such as PNG, JPEG, and TIFF."),
      tags$li("Higher DPI is useful for print-quality raster output, whereas SVG and PDF are resolution-independent.")
    ),
    volcano_doc_callout(
      "Practical tip",
      p(
        "If you plan to edit the figure in Illustrator, Inkscape, or another ",
        "vector editor, choose SVG or PDF. If a journal or presentation workflow ",
        "requires a raster image, TIFF at high DPI is often a good choice."
      ),
      type = "info"
    ),
    h3("Add the current plot to the Plot Grid"),
    tags$ul(
      tags$li("Use the optional label field if you want a specific panel title in the Plot Grid."),
      tags$li("This is useful for building composite figures that combine volcano plots with other MiraProt plots."),
      tags$li("Make sure the current plot is final before adding it to the grid, because the grid stores the current rendered state."),
      tags$li("Check that axis ranges, colour conventions, and titles are consistent before assembling a multi-panel figure.")
    ),
    h3("Good scientific practice"),
    tags$ul(
      tags$li("Use adjusted p-values for confirmatory reporting unless your analysis plan requires raw p-values."),
      tags$li("Record the active fold-change and p-value thresholds together with exported figures."),
      tags$li("Keep identifier naming, colour meaning, and axis scaling consistent across plots in the same study."),
      tags$li("Column pairing and plot selection are calculated when you click Create Plot, so metadata edits during file setup should not trigger volcano calculations before you request a plot.")
    )
  )
}
