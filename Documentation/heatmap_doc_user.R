############
# User Guide — Content (initial stubs)

render_heatmap_overview_content_heatmap <- function() {
  div(
    h2("Heatmap Module — Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p(
        "The Heatmap module creates publication-ready heatmaps to explore how selected proteins behave across samples ",
        "and how strongly they correlate with each other and with the samples. ",
        "It combines abundance heatmaps with protein–protein and sample–sample correlation views, ",
        "and can optionally show additional one-column heatmaps for average abundance (basemean) ",
        "and log2 abundance ratios. ",
        "All views share the same protein set so that patterns are directly comparable."
      )
    ),

    h3("General — What the Heatmap Module Does in MiraProt"),

    h4("What is shown in the Heatmap module?"),
    p(
      "The Heatmap module focuses on a user‑defined subset of proteins and a chosen set of samples. ",
      "It extracts the corresponding quantitative values from the processed data, ",
      "applies a log2 transformation and computes a per‑protein z‑score across the selected samples. ",
      "These z‑scores form the basis of the abundance heatmap and the downstream clustering."
    ),
    p(
      "In addition to the abundance heatmap, the module can derive:",
      tags$ul(
        tags$li("a protein–protein correlation heatmap based on the selected proteins,"),
        tags$li("a sample–sample correlation heatmap based on the selected samples,"),
        tags$li("a one‑column heatmap of log2 mean abundance across the selected samples (basemean),"),
        tags$li("a one‑column heatmap of log2 abundance ratios when such a column is available in the data.")
      ),
      "All matrices are aligned so that the same protein order is used across views. ",
      "This makes it easier to see whether clusters of co‑regulated proteins also form clear correlation patterns."
    ),

    h4("How proteins and samples are chosen"),
    p(
      "The module does not work on the full dataset by default. Instead, it performs a careful selection step ",
      "before any heatmap is drawn:"
    ),
    tags$ul(
      tags$li(
        strong("Data type and samples: "),
        "You first choose which abundance type to use (for example raw, normalized, batch‑corrected or imputed values) ",
        "and then select one or more samples. Only these samples are used to calculate z‑scores and correlations."
      ),
      tags$li(
        strong("Protein subset: "),
        "Proteins can be supplied in several ways: you can paste or type identifiers directly, ",
        "import proteins from enriched GSEA or GO pathways listed in the app, or work with proteins that pass p‑value ",
        "and ratio filters defined in the Heatmap sidebar. All selection methods are combined into a single consistent ",
        "protein set for plotting."
      ),
      tags$li(
        strong("Data quality filter: "),
        "If enabled, proteins with missing abundance values in any of the selected samples are removed before all other filters. ",
        "This ensures that every protein shown in the heatmap has a complete profile across the chosen samples."
      ),
      tags$li(
        strong("Statistical and ratio filters: "),
        "When p‑values and abundance ratios exist in the dataset and are selected in the sidebar, ",
        "proteins can be restricted to those that pass a p‑value threshold and/or show sufficiently large (or small) ",
        "absolute log2 ratios. If these columns are not available or contain no valid data, filtering is skipped and the module reports this in the interface."
      ),
      tags$li(
        strong("Maximum number of proteins: "),
        "If more proteins pass the filters than the configured maximum, the module automatically reduces the set. ",
        "When p‑values are in use, proteins with the most favourable p‑values are kept; otherwise a random subset is taken. ",
        "If too few proteins remain at any stage, the module stops and shows an explanatory message instead of drawing a misleading heatmap."
      )
    ),

    h4("When the Heatmap module is helpful"),
    tags$ul(
      tags$li("You want to inspect how a focused set of proteins behaves across selected samples using a log2‑scaled, z‑scored abundance heatmap."),
      tags$li("You want to see whether proteins that differ significantly according to p‑values also form coherent correlation clusters."),
      tags$li("You want to compare abundance patterns with additional summaries such as average abundance or log2 abundance ratios using aligned side heatmaps."),
      tags$li("You need a combined view where abundance and correlation heatmaps share the same protein order, making it easier to interpret multi‑panel figures.")
    ),

    h4("Key Features"),
    tags$ul(
      tags$li(
        strong("Sample‑aware abundance heatmaps:"),
        " Abundance values are always taken from the subset of samples you explicitly select. ",
        "The data are re‑transformed to their original scale when necessary, converted to log2, and then z‑scored per protein. ",
        "This keeps the colour scale comparable across proteins while respecting the chosen sample set."
      ),
      tags$li(
        strong("Consistent protein selection pipeline:"),
        " Before plotting, the module applies a fixed sequence of filters: optional removal of proteins with missing values in the selected samples, ",
        "optional p‑value and log2 ratio filters, optional restriction to manually entered identifiers, and finally a limit on the total number of proteins. ",
        "At each step it checks whether enough proteins remain and stops early with a clear message if not."
      ),
      tags$li(
        strong("Correlation heatmaps with optional diagonal line:"),
        " Protein–protein and sample–sample correlation matrices are visualized as separate heatmaps that can be combined with abundance in one view. ",
        "An optional diagonal line can be overlaid in correlation views, with configurable colour, width and orientation, ",
        "to highlight the main or anti‑diagonal."
      ),
      tags$li(
        strong("Additional one‑column summaries:"),
        " When enabled, the module computes log2 mean abundance across the selected samples (basemean) and extracts log2 abundance ratios from dedicated columns. ",
        "Each is drawn as a single‑column heatmap aligned with the abundance matrix, using the same protein order."
      ),
      tags$li(
        strong("Flexible legends and styling:"),
        " Colour gradients are derived from three user‑chosen colours. ",
        "Legends can be placed on any side, and font sizes for labels and legends can be adjusted. ",
        "Special options are available to enhance contrast in correlation heatmaps without affecting the underlying values."
      ),
      tags$li(
        strong("Grid and single‑view tabs:"),
        " The main interface offers combined layouts (abundance with protein correlation, or abundance with sample correlation) ",
        "as well as individual tabs for abundance, each correlation type, and the one‑column basemean and ratio heatmaps. ",
        "All tabs work from the same filtered protein set, so switching views never changes which proteins are shown."
      ),
      tags$li(
        strong("Robust handling of missing or inconsistent metadata:"),
        " The module checks for the presence and validity of p‑value and ratio columns before using them. ",
        "If such information is missing or unusable, it falls back to simpler selection rules and informs you in the UI instead of failing silently."
      )
    ),

    h3("Common Use Cases"),
    tags$ul(
      tags$li(
        "Explore a handful of proteins coming from GSEA or GO enrichment results across a chosen panel of samples, ",
        "using abundance and correlation heatmaps to see whether they form coherent patterns."
      ),
      tags$li(
        "Visualise proteins that pass a p‑value threshold and show strong log2 abundance ratio changes, ",
        "while automatically limiting the view to a manageable number of proteins."
      ),
      tags$li(
        "Create a multi‑panel figure that combines abundance, protein correlation, sample correlation, basemean and log2 abundance ratio, ",
        "all aligned by protein order for use in reports or publications."
      )
    ),

    h3("Limitations of the Heatmap Module in MiraProt"),
    tags$ul(
      tags$li(
        strong("Dependence on available metadata: "),
        "Advanced filters rely on valid p‑value and abundance ratio information in the loaded dataset. ",
        "If these columns are missing or contain no usable values, the module automatically skips the corresponding filters ",
        "and may fall back to random selection when limiting the number of proteins."
      ),
      tags$li(
        strong("Data completeness requirement (when enabled): "),
        "If the option to remove proteins with missing abundance values is active and many selected samples contain gaps, ",
        "a large fraction of proteins may be excluded before heatmap creation. ",
        "In extreme cases this can lead to early termination with an error message instead of a plot."
      ),
      tags$li(
        strong("Visual complexity with many proteins: "),
        "Although the module enforces an upper limit on the number of proteins shown, ",
        "very dense heatmaps can still be difficult to read. ",
        "Interpreting subtle patterns is easier when working with moderate protein sets rather than several hundred proteins at once."
      ),
      tags$li(
        strong("Interpretation of correlation patterns: "),
        "Correlation heatmaps summarise pairwise relationships based on the selected samples only. ",
        "They are sensitive to outliers and to the exact sample subset. ",
        "They help to generate hypotheses about co‑regulated proteins or similar samples but do not provide causal evidence."
      )
    ),

    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Heatmap module lets you build a controlled, multi‑panel view of a selected protein set across chosen samples. ",
      "It applies a structured filtering pipeline, ensures that abundance and correlation views share the same protein order, ",
      "and offers optional one‑column summaries for mean abundance and log2 abundance ratios. ",
      "This makes it a central place in MiraProt for visually exploring patterns in quantitative proteomics data."
    )
  )
}

render_heatmap_proteinselection_plotting_content_heatmap <- function() {
  div(
    h2("Protein selection and plotting"),
    hr(),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      # Step 1 — Load data and choose identifier
      div(
        class = "panel panel-primary",
        div(class = "panel-heading", h4("Step 1 — Load data and choose identifier")),
        div(
          class = "panel-body",
          p(
            "Use the ", strong("Data Wizard"), " to load your dataset and define metadata. ",
            "The Heatmap module expects at least one identifier column and abundance columns linked to samples."
          ),
          tags$ul(
            tags$li(
              strong("Identifier column:"),
              " In the Heatmap sidebar, choose the identifier in ",
              em("Gene identifier column"),
              ". ",
              "This column is used for row labels in the heatmap and for matching any identifier input."
            ),
            tags$li(
              strong("Abundance type:"),
              " In ",
              em("Select data type"),
              " pick the abundance type to display (for example normalised, batch‑corrected, or imputed abundance). ",
              "The choices are taken directly from the metadata and only show types that exist."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Note:</strong> If these selectors are empty, check in the Data Wizard that identifier and abundance content types ",
            "were defined for your columns."
          )
        )
      ),

      # Step 2 — Choose samples
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 2 — Choose samples for the heatmap")),
        div(
          class = "panel-body",
          p(
            "The Heatmap module always works with the samples you select explicitly."
          ),
          tags$ul(
            tags$li(
              strong("Select samples:"),
              " In ",
              em("Select samples"),
              " choose one or more samples. ",
              "Only these samples are used when computing log2 values, z‑scores and all correlation matrices."
            ),
            tags$li(
              strong("Metadata link:"),
              " Sample names come from the metadata. ",
              "A sample is only listed if the selected abundance type is available for that sample."
            )
          ),
          p(
            "If no samples are selected or if no abundance columns match your selection, the module shows an error message instead of plotting."
          )
        )
      ),

      # Step 3 — Configure filters (Data Quality, Statistical, Ratio, Identifier Filter, Max proteins)
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 3 — Configure filters for protein selection")),
        div(
          class = "panel-body",
          p(
            "Before calculating the heatmaps, the module applies several filters from top to bottom in the sidebar. ",
            "These filters determine which proteins finally enter the heatmap."
          ),

          tags$ul(
            tags$li(
              strong("Data Quality — missing values filter (default ON):"),
              " The option ",
              em("Remove proteins with missing abundance values"),
              " removes any protein that has at least one missing abundance value among the selected samples. ",
              "This happens before other filters. ",
              "If you enable this and many samples have gaps, large parts of the data may be removed."
            ),
            tags$li(
              strong("Statistical Filter — p-value (optional):"),
              " When ",
              em("Enable p-value filtering"),
              " is checked and valid p‑value columns exist in the metadata, you can choose:",
              tags$ul(
                tags$li(
                  em("P-Value type"),
                  " — which content type to use (for example overall p‑value, FDR, or ratio‑specific p‑values)."
                ),
                tags$li(
                  em("P-Value column"),
                  " — the specific comparison (numerator vs denominator) you want to use."
                ),
                tags$li(
                  em("P-Value threshold (≤)"),
                  " — only proteins with p‑values at or below this threshold are kept."
                )
              ),
              "The module checks that at least one selected column contains numeric values between 0 and 1. ",
              "If no valid p‑values are found, p‑value filtering is automatically disabled and a red warning box is shown."
            ),
            tags$li(
              strong("Ratio Filter — abundance ratio (optional):"),
              " When a column with ",
              em("Abundance Ratio"),
              " content exists, the module offers a selector for that column and enables:",
              tags$ul(
                tags$li(
                  em("Enable ratio filter"),
                  " — switches the ratio filter on."
                ),
                tags$li(
                  em("Filter mode"),
                  " — keep proteins with large absolute log2 ratios (>",
                  " threshold) or small absolute log2 ratios (< threshold)."
                ),
                tags$li(
                  em("log₂ ratio threshold"),
                  " — the cut‑off applied to |log2(ratio)|."
                )
              ),
              "If no valid positive values are found in any ratio column, the ratio filter and the ratio heatmap option are disabled, ",
              "and a warning panel explains why."
            ),
            tags$li(
              strong("Identifier Filter (optional):"),
              " The text area ",
              em("Enter proteins (comma/line separated)"),
              " lets you restrict the analysis to a custom list of identifiers. ",
              "Only proteins whose identifier matches entries in this field (based on the chosen identifier column) pass this step."
            ),
            tags$li(
              strong("Max proteins to display:"),
              " ",
              em("Max proteins to display"),
              " sets an upper limit on how many proteins are used to build the heatmaps. ",
              "If fewer proteins pass the filters than this number, all of them are kept. ",
              "If more proteins pass, the set is reduced as follows:"
            ),
            tags$ul(
              tags$li(
                "If p‑value filtering is active and a valid p‑value column was used, proteins are ranked by p‑value and the best ones ",
                "are kept up to the maximum."
              ),
              tags$li(
                "If no usable p‑values are available (p‑value filtering disabled or invalid), proteins are randomly sampled from those ",
                "that passed the preceding filters."
              )
            ),
            tags$li(
              strong("Early stop when nothing remains:"),
              " After each major filter (missing values, p‑value, ratio, Identifier Filter), the module checks how many proteins remain. ",
              "If zero remain, it stops immediately and shows a clear notification instead of drawing an empty heatmap."
            )
          )
        ),
        div(
          class = "alert alert-success",
          HTML(
            "<strong>Tip:</strong> The Protein Selection Panel is mainly used to find and highlight proteins in your heatmaps. ",
            "You can also use it to search for identifiers and copy them to the clipboard. ",
            "This makes it easy to paste curated protein lists into the Identifier Filter textarea."
          )
        )
      ),

      # Step 4 — Create the heatmaps
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 4 — Create the heatmaps")),
        div(
          class = "panel-body",
          p(
            "Click ",
            strong("Create Plot"),
            " in the Heatmap sidebar to run the filter pipeline and build the plots."
          ),
          tags$ul(
            tags$li(
              "The module applies all active filters to the dataset and determines the final set of proteins."
            ),
            tags$li(
              "For the selected samples and data type, it extracts the corresponding abundance values, ",
              "converts them to log2 scale (with a small offset to handle zeros) and computes a z‑score per protein across samples."
            ),
            tags$li(
              "From this z‑score matrix, it constructs the ",
              strong("Abundance"),
              " heatmap."
            ),
            tags$li(
              "Using the same z‑score matrix, it calculates:",
              tags$ul(
                tags$li("a protein–protein correlation matrix for the ", strong("Protein Correlation")," heatmap,"),
                tags$li("a sample–sample correlation matrix for the ", strong("Sample Correlation"), " heatmap.")
              )
            ),
            tags$li(
              "If enabled and supported by the data, it also computes:",
              tags$ul(
                tags$li("log2 mean abundance across the selected samples (", strong("Basemean Heatmap"), "),"),
                tags$li("log2 abundance ratios from a selected Abundance Ratio column (", strong("Abundance Ratio Heatmap"), ").")
              )
            )
          ),
          p(
            "The combined tabs (",
            em("Abundance + Correlation Grid Heatmap"),
            " and ",
            em("Abundance + Sample Grid Heatmap"),
            ") show aligned multi‑panel views. ",
            "Single tabs display each component separately but always use the same filtered protein set and sample order."
          )
        )
      ),

      # Step 5 — Use the Protein Selection Panel for labels and highlighting
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5 — Use the Protein Selection Panel for labels and highlighting")),
        div(
          class = "panel-body",
          p(
            "After you have a working heatmap, the ",
            strong("Protein Selection Panel"),
            " becomes useful for inspecting and highlighting proteins."
          ),
          tags$ul(
            tags$li(
              strong("Search and suggestion:"),
              " Enter patterns into ",
              em("Search for Gene Symbols"),
              " and use ",
              em("Suggested Identifiers"),
              " to see all matching identifiers from your chosen identifier column. ",
              "You can copy these suggestions for use in the Identifier Filter or for external documentation."
            ),
            tags$li(
              strong("Pathway‑based lists:"),
              " Use ",
              em("Import proteins of enriched GSEA pathways"),
              " or ",
              em("Import proteins of enriched GO pathways"),
              " plus ",
              em("Add Pathway proteins"),
              " to write pathway‑based protein lists into the search field. ",
              "You can then refine these lists, copy them, or use them as input to the Identifier Filter."
            ),
            tags$li(
              strong("Highlighting in the heatmap:"),
              " ",
              em("Highlight Proteins"),
              " marks the proteins from the internal list in the abundance heatmap (for example with annotations on the side). ",
              em("Undo"),
              " clears this visual emphasis. ",
              "This does not change the underlying filter results or ordering; it is purely for interpretation and reporting."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Tip:</strong> A practical workflow is to first obtain a stable heatmap using the sidebar filters, ",
            "and then use the Protein Selection Panel to highlight specific proteins or pathway members directly in that heatmap."
          )
        )
      ),

      # Step 6 — Basic customisation
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 6 — Basic customisation of the plots")),
        div(
          class = "panel-body",
          p(
            "Several styling options let you adapt the heatmaps for better readability and export."
          ),
          tags$ul(
            tags$li(
              strong("Colour palette:"),
              " Choose three colours (Low, Mid, High) for the heatmap scale. ",
              "Abundance heatmaps use a symmetric scale around zero for z‑scores. ",
              "Correlation heatmaps can optionally use an enhanced contrast mode for clearer extremes."
            ),
            tags$li(
              strong("Labels and dendrograms:"),
              " Toggle row and column labels separately for abundance, correlation and sample correlation heatmaps. ",
              "You can also show or hide row and column dendrograms; these affect the appearance but the underlying order is kept fixed when required."
            ),
            tags$li(
              strong("Fonts and legends:"),
              " Adjust row and column font sizes, legend title and legend text sizes. ",
              "The module automatically adapts legend spacing for horizontal vs vertical legends."
            ),
            tags$li(
              strong("Legend position:"),
              " Use ",
              em("Legend position for all plots"),
              " to place legends on the right, left, top or bottom. ",
              "This setting affects both combined grids and single plots."
            )
          )
        )
      )
    ),

    h3("Data Requirements"),
    tags$ul(
      tags$li("Processed data with at least one identifier column and abundance columns for your samples."),
      tags$li("Metadata that defines Content (abundance types, p‑values, abundance ratios) and Sample names."),
      tags$li("Optional: p‑value columns for Statistical Filter and for ranking when limiting the number of proteins."),
      tags$li("Optional: Abundance Ratio columns for Ratio Filter and the Abundance Ratio Heatmap.")
    ),

    h3("Quick troubleshooting"),
    tags$ul(
      tags$li(
        strong("No heatmap after clicking Create Plot:"),
        " Check the notification in the app. ",
        "Typical causes are: no samples selected, no abundance columns matching the selected type and samples, ",
        "or all proteins removed by the filters."
      ),
      tags$li(
        strong("Zero or very few proteins:"),
        " Relax one filter at a time (disable missing‑value removal, increase the p‑value threshold, disable the ratio filter, ",
        "or clear the Identifier Filter) and try again."
      ),
      tags$li(
        strong("Too many proteins and unreadable labels:"),
        " lower ",
        em("Max proteins to display"),
        ", enable p‑value and/or ratio filtering, or restrict the Identifier Filter to a smaller list."
      )
    )
  )
}

render_heatmap_customizing_content_heatmap <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Colors"),
    p(
      "Colour settings control how abundance values and correlations are mapped to colours. ",
      "They apply consistently across all heatmap tabs in this module."
    ),

    div(
      class = "well",
      h4("Colour palette"),
      p(
        "The Heatmap module uses three base colours (Low, Mid, High) chosen in the sidebar. ",
        "These are combined into gradients for abundance, correlation, basemean and abundance ratio heatmaps."
      ),
      tags$dl(
        tags$dt("Low colour"),
        tags$dd(
          "Used for the low end of the scale. ",
          "For abundance heatmaps this corresponds to low z‑scores (values well below the mean after log2 and z‑score transformation). ",
          "For correlation heatmaps it corresponds to strongly negative correlations (close to −1)."
        ),
        tags$dt("Mid colour"),
        tags$dd(
          "Used around zero. ",
          "For abundance, this represents values close to the mean; ",
          "for correlation it represents correlations near zero."
        ),
        tags$dt("High colour"),
        tags$dd(
          "Used for the high end of the scale. ",
          "For abundance heatmaps this corresponds to high positive z‑scores; ",
          "for correlation heatmaps it corresponds to strong positive correlations (close to +1)."
        )
      ),
      p(
        strong("Abundance Heatmaps: "),
        "use a symmetric gradient around zero based on the observed z‑score range. ",
        "Negative, neutral and positive deviations are mapped from Low → Mid → High."
      ),
      p(
        strong("Correlation Heatmaps: "),
        "use a fixed numeric range from −1 to +1. ",
        "Two mapping modes are available:"
      ),
      tags$ul(
        tags$li(
          strong("Standard (linear) mode:"),
          " colours change linearly from Low at −1, over Mid at 0, to High at +1."
        ),
        tags$li(
          strong("Enhanced contrast for correlation:"),
          " when this checkbox is enabled, the module applies a sigmoid (tanh‑based) transformation to values between −1 and 1 ",
          "before mapping them to colours. ",
          "This compresses values near zero and stretches the extremes. ",
          "Visually, correlations close to 0 become more similar in colour, while strong positive or negative correlations ",
          "are emphasised with more distinct colours."
        )
      ),
      div(
        class = "alert alert-success",
        HTML(
          "<strong>Tip:</strong> For abundance heatmaps, choose a palette where the middle colour clearly separates low and high values ",
          "(for example blue–white–yellow). For correlation heatmaps, enable ",
          "<em>Enhanced contrast for correlation</em> when you want to highlight very strong correlations and de‑emphasise weak ones."
        )
      )
    ),

    h3("Sorting of Proteins"),
    p(
      "The ",
      em("Sort proteins by"),
      " option determines how rows (proteins) and, in part, columns (samples) are ordered in the heatmaps."
    ),

    div(
      class = "well",
      style = "border-left: 4px solid #0d6efd;",
      h4("Sorting methods: Z-Score, Pearson r and Custom"),
      tags$dl(
        tags$dt("Z-Score (default)"),
        tags$dd(
          "Proteins are ordered based on clustering of their z‑scored abundance profiles across the selected samples. ",
          "The module clusters the abundance matrix and uses that order for the Abundance Heatmap and aligned views."
        ),
        tags$dt("Pearson r"),
        tags$dd(
          "Proteins are ordered based on clustering of the protein–protein correlation matrix. ",
          "First, correlations between protein abundance profiles are calculated; then a clustering on this correlation matrix ",
          "provides the row order used across abundance and correlation heatmaps."
        ),
        tags$dt("Custom"),
        tags$dd(
          "Enter protein identifiers as a comma- or line-separated priority list. ",
          "Matching proteins are placed first in the entered order; missing identifiers are ignored and duplicates use the first occurrence. ",
          "Remaining proteins are appended using the selected fallback sorting method (Z-Score by default, or Pearson r)."
        )
      ),
      p(
        strong("Practical differences:"),
        tags$ul(
          tags$li(
            strong("Z-Score sorting:"),
            " groups proteins with similar abundance patterns across the selected samples, ",
            "even if the overall correlation structure is complex."
          ),
          tags$li(
            strong("Pearson r sorting:"),
            " emphasises clusters of proteins that are strongly correlated with each other. ",
            "This can make block structures in the protein correlation heatmap particularly clear."
          ),
          tags$li(
            strong("Custom sorting:"),
            " keeps manually selected proteins at the top while still using Z-Score or Pearson r ordering for all remaining proteins."
          )
        )
      ),
      p(
        strong("When to use which:"),
        tags$ul(
          tags$li(
            "Use ", em("Z-Score"), " when you mainly want to interpret abundance patterns across samples (for example up/down patterns per condition)."
          ),
          tags$li(
            "Use ", em("Pearson r"), " when your focus is on co‑regulated protein clusters and you want the protein correlation heatmap ",
            "to drive the row ordering."
          ),
          tags$li(
            "Use ", em("Custom"), " when a curated subset should appear first, while incomplete lists should still keep a reproducible fallback order."
          )
        )
      )
    ),

    h3("Sorting of Samples"),
    p(
      "The order of samples along the horizontal axis is controlled independently from protein ordering."
    ),

    div(
      class = "well",
      style = "border-left: 4px solid #0d6efd;",
      h4("Sample sorting via ", em("Sort samples by")),
      p(
        "Use the ",
        em("Sort samples by"),
        " option in the sidebar to choose how selected samples are arranged in all heatmaps."
      ),
      tags$dl(
        tags$dt("None"),
        tags$dd(
          "Keeps the manual order from the ",
          em("Select samples"),
          " control. ",
          "This is useful when you want to enforce a specific biological or experimental sequence (for example time course or dose levels) by hand."
        ),
        tags$dt("Alphabetical (A→Z / Z→A)"),
        tags$dd(
          "Sorts samples by their names in ascending or descending alphabetical order. ",
          "This is a simple, reproducible order that does not depend on the underlying data."
        ),
        tags$dt("Pearson clustering (distance: 1 − Pearson correlation; linkage: average linkage)"),
        tags$dd(
          "Groups samples by how similar their overall abundance patterns are. ",
          "The module computes pairwise Pearson correlations between samples, converts them into a distance (1 − Pearson correlation), ",
          "and performs hierarchical clustering with average linkage. ",
          "Samples with similar abundance profiles are placed next to each other."
        ),
        tags$dt("Distance clustering (distance: Euclidean; linkage: Ward.D2)"),
        tags$dd(
          "Clusters samples based on Euclidean distances in log2-transformed abundance values with Ward.D2 linkage. ",
          "This emphasises gradual trends or trajectories across samples and is less focused on correlation alone."
        ),
        tags$dt("PCA1 (ascending / descending)"),
        tags$dd(
          "Orders samples by their score on the first principal component (PC1) of the data. ",
          "PC1 often captures the dominant source of variation, such as treatment vs control or major batch effects. ",
          "The ascending option places lower PC1 scores on the left and higher scores on the right; ",
          "the descending option uses the same axis but reverses the direction."
        ),
        tags$dt("PCA2 (ascending / descending)"),
        tags$dd(
          "Orders samples by their score on the second principal component (PC2). ",
          "PC2 captures the second orthogonal axis of variation, meaning it describes an independent pattern after PC1. ",
          "This can reveal biologically meaningful sample structure that is not visible when sorting by PC1 alone. ",
          "As with PCA1 sorting, ascending places lower PC2 scores on the left and higher scores on the right, while descending reverses that direction."
        )
      ),
      p(
        "The chosen sample order is applied consistently to the Abundance Heatmap and the Sample Correlation Heatmap. ",
        "Protein ordering continues to be controlled separately by the ",
        em("Sort proteins by"),
        " setting."
      )
    ),

    h3("Text and Legend Sizes"),
    p(
      "Font sizes control the readability of labels and legends in all Heatmap views."
    ),
    tags$dl(
      tags$dt("Row font size"),
      tags$dd("Controls the font size of protein (row) labels in all heatmaps where row labels are shown."),
      tags$dt("Column font size"),
      tags$dd("Controls the font size of sample (column) labels and correlation axis labels where they are shown."),
      tags$dt("Legend title size"),
      tags$dd("Controls the font size of legend titles (for example Abundance Heatmap, Pairwise Correlation, log2(Basemean))."),
      tags$dt("Legend text size"),
      tags$dd("Controls the font size of legend tick labels (numeric values on the colour bars).")
    ),
    p(
      "All size controls accept numeric values. ",
      "Increase them for presentations and posters; reduce them for compact displays or when many items are shown."
    ),

    h3("Labels and Dendrograms"),
    p(
      "You can independently show or hide labels and dendrograms for different heatmap types."
    ),
    tags$ul(
      tags$li(
        strong("Abundance Heatmap Labels:"),
        " ",
        em("Show Row Labels"),
        " toggles protein names on the abundance heatmap; ",
        em("Show Column Labels"),
        " toggles sample names."
      ),
      tags$li(
        strong("Protein Correlation Labels:"),
        " ",
        em("Show Row Labels"),
        " and ",
        em("Show Column Labels"),
        " control whether protein names are shown in the protein correlation heatmap."
      ),
      tags$li(
        strong("Sample Correlation Labels:"),
        " ",
        em("Show Row Labels"),
        " and ",
        em("Show Column Labels"),
        " control whether sample names are shown in the sample correlation heatmap."
      ),
      tags$li(
        strong("Basemean / Abundance Ratio Labels:"),
        " separate checkboxes let you show or hide row labels and the single column label for the log2(Basemean) ",
        "and log2(Abundance Ratio) side heatmaps."
      ),
      tags$li(
        strong("Dendrograms:"),
        " ",
        em("Show row dendrogram"),
        " and ",
        em("Show column dendrogram"),
        " control whether clustering trees are drawn for rows and columns. ",
        "The actual order of proteins and samples is determined by the chosen sorting method; ",
        "dendrograms are visual aids only."
      )
    ),

    h3("Legend Position"),
    p(
      "The legend position setting applies to all Heatmap plots in this module."
    ),
    tags$ul(
      tags$li("Right – vertical legends to the right of the plots."),
      tags$li("Left – vertical legends to the left."),
      tags$li("Top – horizontal legends above the plots."),
      tags$li("Bottom – horizontal legends below the plots.")
    ),
    p(
      "The module automatically adapts legend orientation (vertical vs horizontal) and spacing based on this choice. ",
      "This helps keep multi‑panel layouts readable."
    ),

    h3("Correlation Diagonal Line"),
    p(
      "For correlation heatmaps, you can overlay a diagonal line to emphasise self‑correlations."
    ),
    tags$ul(
      tags$li(
        strong("Show diagonal line in correlation heatmaps:"),
        " toggles drawing a line across diagonal cells."
      ),
      tags$li(
        strong("Line color / Line width:"),
        " control the appearance of this line."
      ),
      tags$li(
        strong("Rotate:"),
        " flips the direction from the main diagonal (top‑left to bottom‑right) to the anti‑diagonal (bottom‑left to top‑right)."
      )
    ),

    h3("Basemean and Abundance Ratio Views"),
    p(
      "Two optional single‑column heatmaps can be added alongside the main abundance and correlation views."
    ),
    tags$ul(
      tags$li(
        strong("Show Basemean Heatmap:"),
        " enables a log2(Basemean) heatmap for the proteins used in the abundance heatmap. ",
        "Additional options allow you to show or hide row labels and the column label."
      ),
      tags$li(
        strong("Show log2 Abundance Ratio Heatmap:"),
        " enables a log2 abundance ratio heatmap when a valid abundance ratio column is available. ",
        "If no usable ratio data exist, this checkbox and its label options are disabled and a warning is shown in the sidebar."
      )
    ),
    p(
      "Both side heatmaps share the same colour palette, font sizes and legend position as the main heatmaps."
    ),

    h3("Applying and Iterating Changes"),
    p(
      "Most customisation settings are applied when a heatmap is created or redrawn. ",
      "Changes to colours, sorting, fonts, labels, dendrograms, legend position and diagonal lines affect subsequent plots."
    ),
    tags$ul(
      tags$li(
        "After changing data‑related settings or filters, click ",
        strong("Create Plot"),
        " to rebuild the heatmaps with the new configuration."
      ),
      tags$li(
        "After changing styling options only (colour palette, sorting method, font sizes, labels, dendrograms, legends, diagonal line), ",
        "recreating the plot ensures that all panels use the updated appearance."
      ),
      tags$li(
        "When exporting or adding heatmaps to the Plot Grid, the current styling settings are respected."
      )
    )
  )
}

render_heatmap_plottypes_content_heatmap <- function() {
  div(
    h2("Plot Types"),
    hr(),

    h3("Heatmap Plot Types in MiraProt"),
    p(
      "The Heatmap module offers several related plot types. ",
      "All are based on the same filtered protein set and the same selected samples, ",
      "but each highlights a different aspect of the data. ",
      "This section explains what each plot shows, how to read it, and when it is useful."
    ),
    h3("How to interpret heatmap patterns"),
    tags$ul(
      tags$li(
        strong("What colours mean after transformation: "),
        "In the abundance heatmap, colours show relative values after log2 and z‑score scaling. ",
        "A warm colour means higher than that protein’s average across the selected samples, ",
        "a cool colour means lower, and mid colour means close to average. ",
        "So colours show pattern, not absolute amount."
      ),
      tags$li(
        strong("Row vs column clustering: "),
        "Row clustering groups proteins with similar behaviour across your selected samples. ",
        "Column clustering groups samples with similar overall protein patterns. ",
        "These are two different questions: protein similarity vs sample similarity."
      ),
      tags$li(
        strong("Selected samples change the result: "),
        "Clustering and correlations are calculated only from the samples you selected. ",
        "If you add or remove samples, groupings can change, sometimes strongly."
      ),
      tags$li(
        strong("Filtering also changes the result: "),
        "Removing proteins with missing values, applying p‑value or ratio thresholds, ",
        "or limiting to a maximum number of proteins changes which proteins are included. ",
        "Because the input set changes, clusters and correlation blocks can change too."
      ),
      tags$li(
        strong("Important caution: "),
        "Clustering is descriptive and hypothesis‑generating. ",
        "It helps you find interesting patterns, but it does not prove biological mechanism or causality on its own."
      )
    ),

    # Hinweis zur gemeinsamen Protein- und Sample-Ordnung
    div(
      class = "alert alert-info",
      strong("Consistent ordering across plot types: "),
      "All heatmap types share the same order of proteins (rows) and the same order of samples (columns). ",
      "Protein ordering is controlled by the ",
      em("Sort proteins by"),
      " option, while sample ordering is controlled by the ",
      em("Sort samples by"),
      " option and your explicit sample selection. ",
      "Once chosen, these orders are applied consistently to the Abundance Heatmap, ",
      "the Protein Correlation Heatmap and the Sample Correlation Heatmap so that groupings ",
      "and patterns are directly comparable between plot types."
    ),

    # Abundance Heatmap

    # Abundance Heatmap
    h3("Abundance Heatmap"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A matrix of proteins (rows) versus selected samples (columns)."),
      tags$li("Each cell shows a z‑scored, log2‑transformed abundance value for that protein in that sample."),
      tags$li("Colours range from low abundance (below that protein’s mean) through mid (around the mean) to high abundance (above that protein’s mean).")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Each row is one protein; each column is one of the samples you selected in the sidebar."),
      tags$li("Colours show relative abundance within each protein across samples (after log2 and z‑score transformation)."),
      tags$li("Rows and columns are ordered according to the chosen sorting method (Z‑Score, Pearson r or Custom with its fallback sorting method)."),
      tags$li("Optional dendrograms indicate clustering structure but do not change the order of proteins or samples.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Identifying clusters of proteins with similar abundance patterns across the selected samples."),
      tags$li("Comparing abundance profiles between sample groups or conditions."),
      tags$li("Spotting outlier samples or proteins with unusual profiles.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("P‑values or statistical significance directly on the plot (these are only used upstream for filtering)."),
      tags$li("Information about protein–protein or sample–sample correlations (use the correlation heatmaps for that).")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Abundance Heatmap is the main view of how selected proteins behave across your chosen samples."
    ),

    # Protein Correlation Heatmap
    h3("Protein Correlation Heatmap"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A square matrix of pairwise correlations between proteins."),
      tags$li("Rows and columns represent the same set of proteins as in the Abundance Heatmap."),
      tags$li("Each cell shows the Pearson correlation between two protein abundance profiles across the selected samples.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Diagonal cells compare a protein with itself (correlation 1)."),
      tags$li("Off‑diagonal cells show how similarly two proteins vary across the selected samples (from −1 to +1)."),
      tags$li("Colour indicates correlation strength and sign, using the same Low/Mid/High palette as other plots."),
      tags$li(
        "If enabled, a diagonal line is drawn across the main or anti‑diagonal to highlight self‑correlations; ",
        "its colour, width and orientation are configurable."
      ),
      tags$li("Protein order is aligned with the Abundance Heatmap for direct comparison.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Finding groups of proteins that are strongly co‑regulated across the selected samples."),
      tags$li("Checking whether abundance clusters also form clear correlation blocks."),
      tags$li("Identifying proteins with broadly similar or opposite behaviour.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Absolute abundance levels or fold changes (only similarity patterns)."),
      tags$li("Direct information about sample–sample relationships (use the Sample Correlation Heatmap).")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Protein Correlation Heatmap shows how strongly proteins correlate with each other based on their abundance profiles."
    ),

    # Sample Correlation Heatmap
    h3("Sample Correlation Heatmap"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A square matrix of pairwise correlations between samples."),
      tags$li("Rows and columns represent the same selected samples used in the Abundance Heatmap."),
      tags$li("Each cell shows the Pearson correlation between two samples across the selected proteins.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Diagonal cells compare each sample with itself (correlation 1)."),
      tags$li("Off‑diagonal cells show how similar two samples are with respect to the selected proteins."),
      tags$li("Colour encodes correlation strength and sign over the fixed range −1 to +1."),
      tags$li(
        "If enabled, a diagonal line is drawn across the main or anti‑diagonal; its colour, width and orientation ",
        "are controlled by the same diagonal line settings as for the Protein Correlation Heatmap."
      )
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Checking whether samples cluster as expected by group or condition."),
      tags$li("Identifying outlier samples that correlate poorly with all others."),
      tags$li("Comparing similarity between technical or biological replicates.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Information about which specific proteins cause similarities or differences (use the Abundance Heatmap)."),
      tags$li("Correlation to external variables not present in the data.")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Sample Correlation Heatmap shows how similar your selected samples are to each other across the chosen proteins."
    ),

    # Basemean Single Column Heatmap
    h3("Basemean Single Column Heatmap"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A single‑column heatmap with one value per protein."),
      tags$li("Each cell shows the log2 mean abundance of that protein across the selected samples.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Rows correspond to the same protein order as in the Abundance Heatmap."),
      tags$li("The single column represents log2(mean abundance) for each protein."),
      tags$li("Colour encodes relative magnitude, using the same Low/Mid/High palette.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Comparing typical abundance levels of proteins within the selected set."),
      tags$li("Adding context to abundance and correlation patterns (for example very low vs highly abundant proteins).")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Information about how abundance differs between individual samples (only the average is shown)."),
      tags$li("Any correlation structure (use correlation heatmaps for that).")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Basemean Heatmap adds a compact view of average protein abundance aligned with the main abundance view."
    ),

    # Abundance Ratio Single Column Heatmap
    h3("Abundance Ratio Single Column Heatmap"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A single‑column heatmap with one log2 abundance ratio per protein."),
      tags$li("Values come from a specific Abundance Ratio column defined in the metadata and selected in the sidebar.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Rows share the same protein order as the Abundance Heatmap and Basemean Heatmap."),
      tags$li("The single column represents log2(ratio) for each protein (or uses the logged values directly if already log2‑scaled)."),
      tags$li("Colours reflect the sign and magnitude of the log2 ratio (for example negative = lower, positive = higher in the chosen comparison).")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Overlaying a chosen comparison (e.g. condition A vs B) on top of abundance and correlation patterns."),
      tags$li("Highlighting proteins with strong positive or negative changes in a single, aligned column.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Per‑sample values (it summarises one comparison per protein)."),
      tags$li("Statistical support (p‑values); those are only used in the filtering step.")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Abundance Ratio Heatmap displays log2 ratios for a selected comparison, aligned with the proteins in the Abundance Heatmap."
    ),

    # Combined Grids
    h3("Combined Grid Views"),
    p(
      "The Heatmap module offers two combined grid views that align several heatmaps for the same proteins and samples."
    ),

    h3("Abundance + Protein Correlation Grid Heatmap"),
    h4("What this view shows"),
    tags$ul(
      tags$li("A horizontal combination of the Abundance Heatmap and the Protein Correlation Heatmap."),
      tags$li("Optional single column side heatmaps for log2(Basemean) and log2(Abundance Ratio) can be added to the right.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Rows are proteins; columns on the left are samples (abundance), on the right protein–protein correlations and optional single columns."),
      tags$li("All components share the same protein order, making it possible to directly compare abundance, co‑regulation and summary measures."),
      tags$li("Legends and label settings apply consistently across all panels in the grid.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Inspecting abundance and protein–protein correlation patterns side by side."),
      tags$li("Adding basemean and ratio context to the same set of proteins in a single aligned figure."),
      tags$li("Preparing multi‑panel figures where protein ordering must be identical across panels.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Sample–sample relationships (use the Abundance + Sample Correlation Grid for that).")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Abundance + Protein Correlation Grid aligns abundance, protein correlation and optional basemean/ratio information for the same protein set.",
      "It helps to align protein rows across mutliple different heatmaps."
    ),

    h3("Abundance + Sample Correlation Grid Heatmap"),
    h4("What this view shows"),
    tags$ul(
      tags$li("A vertical combination of the Abundance Heatmap on top of the Sample Correlation Heatmap."),
      tags$li("The sample order is shared between both panels.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("The upper panel shows protein abundance per sample; the lower panel shows sample–sample correlations across those proteins."),
      tags$li("Column ordering of the Abundance Heatmap and both axes of the Sample Correlation Heatmap are aligned."),
      tags$li("Changes in sample order (via the sorting method) affect both panels together.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Comparing abundance patterns with overall sample similarity in a single figure."),
      tags$li("Checking whether observed abundance differences are reflected in the sample correlation structure."),
      tags$li("Communicating both per‑sample abundance and between‑sample relationships in an aligned layout.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Basemean or ratio information (these are available in the Abundance + Correlation Grid instead).")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Abundance + Sample Correlation Grid combines abundance and sample correlation views with a shared sample order.",
      "It helps to align sample columns across multiple heatmaps."
    )
  )
}

render_heatmap_saveplots_content_heatmap <- function() {
  div(
    h2("Save and Export"),
    hr(),

    h3("What can be exported"),
    tags$ul(
      tags$li(
        tags$b("Download plot:"),
        " saves the currently rendered heatmap view from the active tab ",
        "(for example abundance heatmap, protein correlation heatmap, sample correlation heatmap, ",
        "or a one-column basemean or log2 ratio heatmap)."
      ),
      tags$li(
        tags$b("Add to Plot Grid:"),
        " stores the current heatmap view so you can assemble it later with other panels."
      )
    ),

    h3("Plot file formats"),
    p("The download panel supports both raster and vector formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats.")
    ),

    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " define the physical output size of the exported figure."),
      tags$li(tags$b("DPI:"), " applies to raster formats (PNG, JPEG, TIFF) and controls pixel density."),
      tags$li("SVG and PDF are vector outputs and are resolution-independent.")
    ),

    div(
      class = "alert alert-info",
      h4("Practical tip"),
      p(
        "For manuscripts, choose vector output (SVG or PDF) when you want sharp text and shapes that can be resized or edited. ",
        "For slides or posters, raster output can be fully appropriate when fixed-image workflows are required; ",
        "use sufficient DPI and dimensions for the final display or print size."
      )
    ),

    h3("Add current plot to Plot Grid"),
    tags$ul(
      tags$li("The label field is optional; if provided, the label is used as the panel title in the Plot Grid."),
      tags$li(
        "The stored entry reflects exactly the currently rendered tab and settings at the moment you click ",
        tags$b("Add to Plot Grid"), "."
      )
    ),

    h3("Good scientific practice"),
    tags$ul(
      tags$li(
        "In figure legends or methods, record the key heatmap settings used for interpretation ",
        "(for example transformation, scaling and row/column sorting choices)."
      ),
      tags$li(
        "Avoid overinterpreting small color differences without checking the underlying quantitative values ",
        "or correlation coefficients."
      )
    )
  )
}
