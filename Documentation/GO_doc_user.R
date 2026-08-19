############
# User Guide — Content (dummy stubs)

render_GO_overview_content_GO <- function() {
  div(
    h2("GO Module — Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p(
        "The GO module performs Gene Ontology (GO) enrichment analysis for the currently loaded dataset. ",
        "You select a gene identifier column, an abundance ratio, and matching p-values; the module then runs GO enrichment ",
        "for the chosen ontology and organism and provides several plots to summarise and visualise enriched GO terms."
      )
    ),

    h3("GO vs GSEA: which method to use"),
    p(
      "Both methods help you move from single proteins to biology, but they answer slightly different questions. ",
      "Use this quick guide to choose the method that best matches your data and study goal."
    ),
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Comparison point"),
          tags$th("GO over-representation (thresholded list)"),
          tags$th("GSEA (ranked full list)")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Conceptual basis"),
          tags$td("Tests whether a GO term appears more often than expected in a selected protein list."),
          tags$td("Tests whether proteins from a gene set cluster near the top or bottom of a ranked list.")
        ),
        tags$tr(
          tags$td("Input requirement"),
          tags$td("Needs a thresholded list (for example, proteins passing fold-change and p-value cut-offs)."),
          tags$td("Needs a ranked full list, ideally including all quantified proteins with a meaningful score.")
        ),
        tags$tr(
          tags$td("Sensitivity profile"),
          tags$td("Good for strong, clear effects; may miss broad but modest coordinated shifts."),
          tags$td("Good for subtle, coordinated pathway-level trends spread across many proteins.")
        ),
        tags$tr(
          tags$td("Interpretation style"),
          tags$td("Reads as 'which terms are over-represented in my significant hits?'"),
          tags$td("Reads as 'which pathways are enriched toward up/down ends of my ranking?'")
        ),
        tags$tr(
          tags$td("Typical failure modes"),
          tags$td("Unstable results if the hit list is too small, too large, or strongly driven by arbitrary thresholds."),
          tags$td("Unstable results if ranking is noisy, identifier mapping is poor, or many ties reduce ordering quality.")
        ),
        tags$tr(
          tags$td("Proteomics-specific strengths / weaknesses"),
          tags$td("Simple and intuitive when proteome coverage is sparse, but sensitive to missing values and cut-off choices."),
          tags$td("Uses more of the measured proteome and can recover coordinated weak signals, but depends on robust quantification and ranking design.")
        ),
        tags$tr(
          tags$td("Practical selection guidance"),
          tags$td("Start with GO when you already have a confident 'significant proteins' list and need a fast biological summary."),
          tags$td("Start with GSEA when you trust your ranking and want to avoid hard thresholds or capture subtle pathway shifts.")
        )
      )
    ),
    p(
      strong("Practical tip: "),
      "In many projects, running both methods is useful: GO gives a clear summary of strong signals, ",
      "while GSEA can reveal coordinated biology that does not survive strict cut-offs."
    ),

    h3("General — What GO Analysis Does in MiraProt"),

    h4("What is Gene Ontology (GO)?"),
    p(
      "Gene Ontology (GO) is a collection of terms that describe what genes and proteins do, ",
      "which processes they are involved in, and where in the cell they are active."
    ),
    tags$ul(
      tags$li(strong("Biological Process (BP):"), " processes and pathways."),
      tags$li(strong("Molecular Function (MF):"), " activities of individual molecules."),
      tags$li(strong("Cellular Component (CC):"), " cellular locations.")
    ),
    p(
      "In the GO module, you first define a set of interesting proteins using abundance and p‑value filters. ",
      "The module then compares this filtered set to all measured proteins in your table and identifies GO terms ",
      "that contain more of your filtered proteins than expected by chance."
    ),

    h4("When GO Analysis is Helpful"),
    tags$ul(
      tags$li("You have many significantly changing proteins and want to group them into biological themes."),
      tags$li("You want to describe your experiment in terms of processes and functions, not only individual proteins."),
      tags$li("You want to pass selected GO terms to other MiraProt modules such as Volcano, PCA, Venn, Heatmap or STRING.")
    ),

    h4("Limitations"),
    tags$ul(
      tags$li(
        "Only proteins that pass your chosen thresholds for abundance and p‑value enter the GO analysis. ",
        "The remaining measured proteins are treated as background but are not analysed in detail."
      ),
      tags$li(
        "Your filter settings are user‑defined and can bias the result. ",
        "Very strict filters can remove relevant proteins; very loose filters can introduce noise."
      ),
      tags$li(
        "GO terms that contain many proteins can be statistically significant while remaining biologically unspecific."
      ),
      tags$li(
        "GO analysis in MiraProt supports interpretation and hypothesis generation. ",
        "It does not by itself prove causal mechanisms."
      )
    ),

    h3("Key Features"),
    tags$ul(
      tags$li(
        strong("Data-aware setup:"),
        " Uses the app’s metadata to offer suitable choices for Gene Identifier, Abundance, and P-Value columns, ",
        "so you can directly reuse the columns used in other modules."
      ),
      tags$li(
        strong("Configurable enrichment settings:"),
        " Control the GO analysis via ontology selection (Molecular Function, Cellular Component, Biological Process, or All), ",
        "p-value adjustment method, p- and q-value cutoffs, and minimum/maximum gene set size."
      ),
      tags$li(
        strong("Organism and identifier control:"),
        " Choose the organism database and key type (e.g. SYMBOL, ENTREZID, ENSEMBL, UNIPROT). ",
        "\"Refresh Cache\", \"Clear Cache\", and \"Update Organisms\" help keep annotation databases and organism lists in sync with your installation. ",
        "\"Clear Cache\" removes the organism disk cache so the next run downloads a fresh copy; it reports how many files were removed (or warns when none were found or a permission error occurred)."
      ),
      tags$li(
        strong("Interactive GO term tree:"),
        " After a successful analysis, enriched GO terms are organised into a hierarchical tree. ",
        "You can select individual terms or entire branches to define which terms are shown in the plots."
      ),
      tags$li(
        strong("Multiple visualisations:"),
        " Create different views of the same GO result: an enrichment score dotplot, a cnet plot with log2 fold change, ",
        "an enrichment map based on term similarities, and a PubMed citation view combining proportional and absolute citation trends."
      ),
      tags$li(
        strong("Customisable appearance and export:"),
        " Adjust colour gradients (low / mid / high), legend position, font sizes and plot height. ",
        "Download figures in several formats (PNG, PDF, SVG, JPEG, TIFF) with user-defined size and DPI, ",
        "or send the current plot to the global plot grid."
      ),
      tags$li(
        strong("Reuse of previous analyses (RDS):"),
        " Export GO results as a lossless .rds (same pattern as the GSEA module) and import them later to restore plots (dotplot, Cnet, enrichment map, PubMed) without rerunning enrichment. ",
        "The Excel export remains available for human-readable tables only and is not used to recreate plots."
      )
    ),

    h3("Common Use Cases"),
    tags$ul(
      tags$li(
        "Summarise enriched GO terms for a list of filtered or differentially abundant genes or proteins using the dotplot."
      ),
      tags$li(
        "Inspect which genes drive specific GO terms and how terms overlap by using the cnet plot and enrichment map."
      ),
      tags$li(
        "Generate GO figures with consistent layout, theme, and resolution for reports, slide decks, or publications."
      ),
      tags$li(
        "Explore literature trends for selected GO terms with the PubMed citation view."
      )
    ),

    h3("Limitations of Gene Ontology Analysis"),
    tags$ul(
      tags$li(
        strong("Data requirements:"),
        " The module needs at least one abundance ratio and one p-value column in the current dataset. ",
        "If these are missing, GO analysis cannot be started."
      ),
      tags$li(
        strong("Dependence on filtering:"),
        " GO enrichment is run only on genes that pass the Min. log2(FC) and Max. p-value filters. ",
        "If no genes pass, or only very few (e.g. less than 10), the module warns that results may be limited ",
        "or that no significant GO terms were found at the current thresholds."
      ),
      tags$li(
        strong("Organism and key type consistency:"),
        " The analysis relies on a matching combination of organism database and key type. ",
        "If annotations cannot be loaded for the selected organism/key type, the module stops the analysis and shows an error notification."
      ),
      tags$li(
        strong("Lossless restore requires RDS:"),
        " Use the GO .rds export/import to fully restore plots. ",
        "The Excel export is for human-readable tables only and cannot reconstruct the plot-ready enrichResult."
      )
    ),

    h3("Quick Start"),
    div(
      class = "alert alert-success",
      p(strong("To run a GO enrichment analysis in this module:")),
      tags$ol(
        tags$li(
          "In the \"GO Analysis Parameters\" panel, select the Gene Identifier Column, Abundance Column, ",
          "P-Value Type, and P-Value Column that correspond to your contrast of interest."
        ),
        tags$li(
          "Set the filters for Min. log2(FC) and Max. p-value to decide which genes enter the analysis. ",
          "Optionally adjust ontology, p-value adjustment method, p- and q-value cutoffs, and gene set size limits."
        ),
        tags$li(
          "Choose the Key type and Organism database, and, if needed, use \"Refresh Cache\", \"Clear Cache\", or \"Update Organisms\" ",
          "to ensure the annotation databases are up to date. \"Clear Cache\" removes the organism disk cache for the selected species; ",
          "use it when the cache is stale or corrupted."
        ),
        tags$li(
          "Click ", strong("\"Run GO Analysis\""), " to perform the enrichment. ",
          "Once the analysis has finished successfully, the GO term tree on the left is populated."
        ),
        tags$li(
          "Use the GO term tree to select individual terms or branches, then choose a Plot Type ",
          "(Enrichment score dotplot, Cnet plot (log2FC), Enrichment map, or Pubmed citations) and click ",
          strong("\"Create Plot\""), " to generate the figure."
        ),
        tags$li(
          "Fine-tune the visual appearance by adjusting colours, legend position, font sizes, and plot height ",
          "until the figure matches your needs."
        ),
        tags$li(
          "Download the finished plot via ", strong("\"Download Plot\""),
          " or add it to the global plot grid with ", strong("\"Add current plot to Grid\""),
          " for later comparison with other figures. ",
          "To fully restore GO results for plotting, use \"Download res_GO (.rds)\" and later \"Import saved GO results (.rds)\" (lossless, same pattern as GSEA). ",
          "The Excel export is only for human-readable tables."
        )
      )
    )
  )
}

render_GO_dataselection_plotting_content_GO <- function() {
  div(
    h2("Data selection and Plotting"),
    hr(),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      # Step 1 — Load data and define metadata (Data Wizard)
      div(
        class = "panel panel-primary",
        div(
          class = "panel-heading",
          h4("Step 1 — Load data and define metadata (Data Wizard)")
        ),
        div(
          class = "panel-body",
          p(
            "Use the ", strong("Data Wizard"), " to load your proteomics dataset and define metadata ",
            "(e.g., identifier column and content tags). ",
            "See the ", em("Data Wizard documentation"), " for detailed instructions."
          ),
          tags$ul(
            tags$li(
              strong("Tip:"),
              " keep a dedicated identifier column (e.g. ",
              code("Gene symbol"), ", ", code("ENTREZID"), ", ", code("UNIPROT"), ")."
            ),
            tags$li(
              strong("If using Excel:"),
              " choose the correct sheet and header row in the Data Wizard so that column names and metadata match."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Metadata:</strong> For GO analysis, it is important that your metadata ",
            "tags abundance ratios as <em>'Abundance Ratio'</em> and p‑values as ",
            "<em>'Abundance Ratio p-Value'</em> or <em>'Abundance Ratio Adj. p-Value'</em>. ",
            "This enables automatic column suggestions in the GO module."
          )
        )
      ),

      # Step 2 — Select columns for GO analysis
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 2 — Select columns for GO analysis")
        ),
        div(
          class = "panel-body",
          p(
            "In the GO module, you link your processed data to the enrichment analysis by selecting: "
          ),
          tags$ul(
            tags$li(
              strong("Gene identifier column:"),
              " choose the column that contains stable gene/protein IDs (e.g. ",
              code("SYMBOL"), ", ", code("ENTREZID"), ", ", code("ENSEMBL"), ", ", code("UNIPROT"), "). ",
              "This column must match the chosen ", em("Key type"), " and the selected organism."
            ),
            tags$li(
              strong("Abundance ratio column:"),
              " choose the column that represents the change between conditions. ",
              "The GO module converts these values to log2 scale before filtering."
            ),
            tags$li(
              strong("P-value column:"),
              " first select the ", em("p-value type"), " (raw or adjusted), then select the matching column. ",
              "This column defines which genes are considered statistically significant."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Note:</strong> When you change the selected abundance ratio column, ",
            "the GO module automatically proposes a matching p-value column based on your metadata. ",
            "You can always override this proposal by manually selecting a different p-value column."
          )
        )
      ),

      # Step 3 — Define filters for significant genes
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 3 — Define filters for significant genes")
        ),
        div(
          class = "panel-body",
          p(
            "Use the GO-specific filters to decide which genes enter the enrichment:"
          ),
          tags$ul(
            tags$li(
              strong("Abundance threshold:"),
              " keep only genes with an abundance change above a chosen log2 threshold."
            ),
            tags$li(
              strong("P-value threshold:"),
              " keep only genes with a p‑value below the selected cutoff."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Note:</strong> If no genes pass the current thresholds, the analysis stops and a warning is shown. ",
            "If very few genes pass, the module will continue but results can be limited."
          )
        )
      ),

      # Step 4 — Choose organism, key type and GO ontology
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 4 — Choose organism, key type and GO ontology")
        ),
        div(
          class = "panel-body",
          p(
            "The GO module uses organism-specific annotation databases via AnnotationHub. ",
            "Configure the following options:"
          ),
          tags$ul(
            tags$li(
              strong("Organism (OrgDb):"),
              " select the species that matches your experiment (e.g. ",
              em("Homo sapiens"), "). ",
              "The module downloads or reuses the corresponding OrgDb package."
            ),
            tags$li(
              strong("Key type:"),
              " select the identifier type that matches your gene identifier column (e.g. ",
              code("SYMBOL"), ", ", code("ENTREZID"), ", ", code("ENSEMBL"), ", ", code("UNIPROT"), "). ",
              "Available key types are loaded from the chosen OrgDb or from a cached list."
            ),
            tags$li(
              strong("GO ontology:"),
              " choose whether to analyse ",
              em("Biological Process (BP)"), ", ",
              em("Molecular Function (MF)"), " or ",
              em("Cellular Component (CC)"), "."
            ),
            tags$li(
              strong("P-value and q-value cutoffs:"),
              " set the statistical thresholds for term significance after multiple testing correction."
            ),
            tags$li(
              strong("Gene set size limits:"),
              " set minimum and maximum term sizes (number of genes per GO term). ",
              "Very small sets may be unstable, very large sets can be unspecific."
            )
          )
        )
      ),

      # Step 5 — Run the GO analysis
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 5 — Run the GO analysis")
        ),
        div(
          class = "panel-body",
          p(
            "Click the GO analysis action button to start the enrichment. Internally the module will:"
          ),
          tags$ul(
            tags$li("extract gene identifiers, abundance values and p‑values from the selected columns,"),
            tags$li("apply your abundance and p‑value filters to define the input gene list,"),
            tags$li("load the selected organism database and key types from AnnotationHub or cache,"),
            tags$li("run GO over‑representation analysis for the chosen ontology,"),
            tags$li("apply your p‑value / q‑value cutoffs and gene set size limits to select significant terms.")
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Note:</strong> If no significant GO terms are found, the module suggests relaxing the p-value or q-value cutoffs. ",
            "If significant terms are available, they are stored and made available for plotting and export."
          )
        )
      ),

      # Step 6 — Select GO terms for plotting
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 6 — Select GO terms for plotting")
        ),
        div(
          class = "panel-body",
          p(
            "After a successful analysis, significant GO terms are organised into a hierarchical tree."
          ),
          tags$ul(
            tags$li("browse the GO tree and select terms at different levels of the hierarchy,"),
            tags$li("limit the number of plotted terms using the ", em("maximum number of terms"), " setting,"),
            tags$li("if no terms are selected, the module automatically falls back to the most significant terms.")
          )
        ),
        div(
          class = "alert alert-success",
          HTML(
            "<strong>Tip:</strong> The set of selected GO terms is shared with other modules (e.g. Volcano, PCA, Venn, Heatmap, STRING). ",
            "A focused selection keeps plots clearer and facilitates cross-module interpretation."
          )
        )
      ),

      # Step 7 — Choose plot type and appearance
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 7 — Choose plot type and appearance")
        ),
        div(
          class = "panel-body",
          p("Use the plot type selector to choose how the GO results are visualised, e.g.:"),
          tags$ul(
            tags$li(
              strong("Enrichment score dotplot:"),
              " compact overview of the most significant GO terms."
            ),
            tags$li(
              strong("Cnet plot (log2FC):"),
              " network of GO terms and their genes, coloured by log2 fold change."
            ),
            tags$li(
              strong("Enrichment map:"),
              " network of GO terms connected by shared genes."
            ),
            tags$li(
              strong("PubMed citations:"),
              " visual overview of literature support for selected GO terms."
            )
          ),
          p("Then refine the appearance of the plots:"),
          tags$ul(
            tags$li("set colour scales for down‑, neutral‑ and up‑regulated genes in compatible plot types,"),
            tags$li("adjust text sizes for titles, axis labels, ticks and legends,"),
            tags$li("choose a ggplot2 theme (e.g. ", code("Black and White"), ", ", code("Gray"), ", ", code("Classic"), ", ", code("Minimal"), ", ", code("Dark"), "),"),
            tags$li("move the legend (e.g. ", em("right"), ", ", em("bottom"), "),"),
            tags$li("set plot height to balance detail and readability on your screen.")
          )
        ),
        div(
          class = "alert alert-success",
          HTML(
            "<strong>Tip:</strong> After clicking the GO plot creation button, the resulting plot becomes the ",
            "current GO plot. It is reused for downloading and for adding the figure to the Plot Grid."
          )
        )
      ),

      # Step 8 — Export and integrate with other modules
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          h4("Step 8 — Export or combine GO plots")
        ),
        div(
          class = "panel-body",
          tags$ul(
            tags$li(
              strong("Download:"),
              " use the GO download controls to choose width (inches), height (inches), resolution (DPI) ",
              "and file format (PNG, JPEG, TIFF, SVG, PDF), then download the current plot."
            ),
            tags$li(
              strong("Add to Grid:"),
              " provide an optional label and click ",
              em("Add current plot to Grid"),
              " to collect GO plots together with figures from other modules in the Plot Grid."
            )
          )
        )
      )
    ),

    # Understanding section
    h3("Understanding the GO analysis in MiraProt"),
    div(
      class = "well",
      tags$ul(
        tags$li(
          strong("Input gene list:"),
          " the module starts from the filtered set of genes that pass your abundance and p‑value thresholds."
        ),
        tags$li(
          strong("Background universe:"),
          " by default, all genes present in your processed table are used as the universe for the enrichment test."
        ),
        tags$li(
          strong("Enrichment statistics:"),
          " the GO analysis performs standard over‑representation testing with multiple testing correction ",
          "controlled by your p‑value and q‑value settings."
        ),
        tags$li(
          strong("Ontology structure:"),
          " GO terms are hierarchically organised; the tree view helps you navigate from broad parent terms ",
          "to more specific child terms."
        )
      )
    ),

    # Data requirements
    h3("Data Requirements"),
    tags$ul(
      tags$li("A processed data frame containing gene/protein identifiers, abundance ratios and p‑values."),
      tags$li("A metadata table that correctly annotates each column (Content and Column)."),
      tags$li("A suitable organism database (OrgDb) and matching key type for your identifiers.")
    ),

    h3("Import and Re‑use Previous GO Analyses"),
    "Use the GO RDS export/import to bring GO results back into the app:",
    tags$ul(
      tags$li("Export with \"Download res_GO (.rds)\" to save the full GO result (enrichResult, similarity, fold changes)."),
      tags$li("Import with \"Import saved GO results (.rds)\" to restore the tree and all plot types without rerunning enrichment."),
      tags$li("Excel export remains for human-readable tables only; it cannot rebuild the plot-ready result.")
    ),
    div(
      class = "alert alert-success",
      HTML(
        "<strong>Tip:</strong> RDS-imported GO results behave like freshly computed results: ",
        "you can customize colors, themes, legend position and plot height in the same way as after a new GO run. "
      )
    )
  )
}

render_GO_customizing_content_GO <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Colors"),
    p("Use the color controls to make GO plots easy to read and consistent with your figures."),

    div(
      class = "well",
      h4("Gradient Colors for GO Plots"),
      p(
        "For GO dotplots, Cnet plots and enrichment maps the module uses a 3‑color gradient. ",
        "These are controlled by:"
      ),
      tags$dl(
        tags$dt(code("Color 1 (Low)")),
        tags$dd("Color for the lowest values in the scale (e.g. low log2 fold changes or high adjusted p‑values)."),
        tags$dt(code("Color 2 (Medium)")),
        tags$dd("Color for intermediate values in the scale."),
        tags$dt(code("Color 3 (High)")),
        tags$dd("Color for the highest values in the scale (e.g. high log2 fold changes or low adjusted p‑values).")
      ),
      p(
        "These three colors are used to create gradients for:",
        tags$ul(
          tags$li("the color scale of the enrichment score dotplot (adjusted p‑values),"),
          tags$li("the fold‑change coloring in the Cnet plot,"),
          tags$li("the term coloring in the enrichment map,"),
          tags$li("the GO term lines in the PubMed citation plots (each term gets a distinct color from the gradient).")
        )
      ),
      div(
        class = "alert alert-success",
        HTML(
          "<strong>Tip:</strong> Choose colors with good contrast and avoid very light tones as extremes. ",
          "This helps distinguish low/medium/high values across all GO plot types."
        )
      )
    ),

    h3("Text and Legend Sizes"),
    p("Adjust text sizes to keep GO plots readable in the app and in exported figures."),

    tags$dl(
      tags$dt(code("Axis Title Size")),
      tags$dd("Controls the font size of axis titles in GO dotplots and PubMed plots (e.g. ", em("Gene Ratio"), ", ", em("Year"), ")."),
      tags$dt(code("Tick Size")),
      tags$dd("Controls the font size of axis tick labels (numbers on the axes)."),
      tags$dt(code("Legend Title Size")),
      tags$dd("Controls the font size of legend titles (e.g. ", em("Gene Count"), ", ", em("Adjusted p‑Value"), ", ", em("log2(FC)"), ")."),
      tags$dt(code("Legend Text Size")),
      tags$dd("Controls the font size of legend entries (e.g. value ranges or term names in legends).")
    ),
    p(
      "All text size controls are numeric: increase sizes for presentations and posters, decrease them for dense plots or small panels."
    ),

    h3("Plot Themes"),
    p("Select a ggplot2 theme that matches your style and output needs:"),
    div(
      class = "well",
      tags$ul(
        tags$li(strong("Gray:"), " gray background with grid lines and axes."),
        tags$li(strong("Black and White:"), " white background with black lines, no panel shading."),
        tags$li(strong("Linedraw:"), " clean line‑drawn style with clear outlines."),
        tags$li(strong("Light:"), " light background with subtle grid lines."),
        tags$li(strong("Dark:"), " dark background with light grid and axes."),
        tags$li(strong("Minimal:"), " minimal decorations; focus on data, few lines."),
        tags$li(strong("Classic:"), " traditional look with border and ticks."),
        tags$li(strong("Void:"), " empty canvas (no axes, ticks or text).")
      ),
      div(
        class = "alert alert-info",
        HTML(
          "<strong>Note:</strong> The selected theme is applied consistently to GO dotplots, Cnet plots, enrichment maps ",
          "and PubMed citation plots."
        )
      )
    ),

    h3("Legend Position"),
    p("Control where legends appear in your GO plots:"),
    tags$ul(
      tags$li(code("Right"), " – default vertical legend to the right of the plot."),
      tags$li(code("Left"), " – vertical legend to the left of the plot."),
      tags$li(code("Top"), " – horizontal legend above the plot."),
      tags$li(code("Bottom"), " – horizontal legend below the plot."),
      tags$li(code("None"), " – hide legends completely.")
    ),
    p(
      "Legend position affects all GO plot types created via the GO module (dotplot, Cnet, enrichment map and PubMed plots)."
    ),
    div(
      class = "alert alert-success",
      HTML(
        "<strong>Tip:</strong> For multi‑panel figures or when space is limited, place legends at the top or bottom ",
        "so the plot area remains wide."
      )
    ),

    h3("Number of Terms and Plot Height"),
    p("Use these options to control how many GO terms are shown and how much space the plot occupies."),

    tags$dl(
      tags$dt(code("Maximum terms to display")),
      tags$dd(
        "Limits how many GO terms are plotted at once. The module keeps the most significant terms (lowest adjusted p‑values). ",
        "This affects all GO plots that display selected terms."
      ),
      tags$dt(code("Height (px)")),
      tags$dd(
        "Sets the vertical size of the main GO plot in pixels. ",
        "Higher values leave more room for many terms and long labels; lower values make the plot more compact."
      )
    ),
    div(
      class = "alert alert-info",
      HTML(
        "<strong>Note:</strong> If more terms are selected than allowed by ",
        "<em>Maximum terms to display</em>, the GO module automatically keeps the most significant ones."
      )
    ),

    h3("Applying Changes"),
    p(
      "Most customization settings are applied when you create or recreate a GO plot. ",
      "After changing plot type or appearance settings, click ",
      strong("Create Plot"),
      " to update the visualization with the new options."
    )
  )
}

render_GO_plottypes_content_GO <- function() {
  div(
    h2("Plot Types"),
    hr(),

    h3("GO Plot Types in MiraProt"),
    p(
      "The GO module offers four plot types in the ", em("Plot Type"), " selector: ",
      strong("Enrichment score dotplot"), ", ",
      strong("Cnet plot (log2FC)"), ", ",
      strong("Enrichment map"), " and ",
      strong("PubMed citations"), "."
    ),

    # 1) Enrichment score dotplot
    h3("Enrichment Score Dotplot"),

    h4("What This Plot Shows"),
    tags$ul(
      tags$li(
        strong("Y‑axis:"),
        " GO terms selected from the current GO result (for example via the GO tree or the maximum number of terms setting)."
      ),
      tags$li(
        strong("X‑axis (GeneRatio):"),
        " for each GO term, the fraction of filtered proteins that belong to this term ",
        "(number of filtered proteins in the term divided by the total number of filtered proteins)."
      ),
      tags$li(
        strong("Point size (Count):"),
        " the number of filtered proteins assigned to each GO term."
      ),
      tags$li(
        strong("Point color (adjusted p‑value):"),
        " a transformed form of the adjusted p‑value for the term. ",
        "The three plot colors configured in the GO customization panel define the low, medium and high values of this color scale."
      )
    ),

    h4("How to Read It"),
    tags$ul(
      tags$li("Terms further to the right contain a larger fraction of your filtered proteins."),
      tags$li("Larger points correspond to terms that include more of your filtered proteins."),
      tags$li("Color helps to compare the strength of enrichment between terms in the same plot.")
    ),

    h4("What It Is Useful For"),
    tags$ul(
      tags$li("Identifying the most enriched GO terms in a compact overview."),
      tags$li("Comparing term size and enrichment strength within one result."),
      tags$li("Choosing a subset of terms for more detailed network or literature views.")
    ),

    h4("What It Does Not Provide"),
    tags$ul(
      tags$li("It does not show which individual proteins belong to each term."),
      tags$li("It does not display how terms are related to each other or how much they overlap.")
    ),

    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The enrichment score dotplot condenses your GO result into a ranked view of enriched terms, ",
      "showing how many filtered proteins each term contains and how strong the enrichment is."
    ),

    # 2) Cnet plot (log2FC)
    h3("Cnet Plot (log2FC)"),

    h4("What This Plot Shows"),
    p(
      "The Cnet plot visualises selected GO terms together with the proteins that belong to them."
    ),
    tags$ul(
      tags$li(
        strong("GO term nodes:"),
        " one node per selected GO term."
      ),
      tags$li(
        strong("Protein nodes:"),
        " nodes for proteins that appear in at least one of the selected GO terms."
      ),
      tags$li(
        strong("Edges:"),
        " connections between a protein node and a GO term node if the protein is annotated to that term."
      ),
      tags$li(
        strong("Protein node color:"),
        " log2 fold change values taken from your selected abundance column, ",
        "mapped to the three plot colors from the GO customization panel."
      )
    ),

    h4("How to Read It"),
    tags$ul(
      tags$li("Proteins connected to several GO terms can link different biological themes in your data."),
      tags$li("Protein node colors show whether these linking proteins are more increased or decreased in your contrast."),
      tags$li("Dense parts of the network help to see which terms share many of the same proteins.")
    ),

    h4("What It Is Useful For"),
    tags$ul(
      tags$li("Connecting enriched GO terms back to individual proteins and their fold changes."),
      tags$li("Highlighting proteins that support more than one enriched term.")
    ),

    h4("What It Does Not Provide"),
    tags$ul(
      tags$li("It does not show exact adjusted p‑values for GO terms."),
      tags$li("For many terms and proteins at once, the layout can become visually dense.")
    ),

    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The Cnet plot reveals which proteins support your selected GO terms and how their log2 fold changes relate to multiple terms."
    ),

    # 3) Enrichment map
    h3("Enrichment Map"),

    h4("What This Plot Shows"),
    p(
      "The enrichment map focuses on how selected GO terms overlap based on shared proteins."
    ),
    tags$ul(
      tags$li(
        strong("Nodes:"),
        " one node for each selected GO term."
      ),
      tags$li(
        strong("Edges:"),
        " connections between terms that share proteins in the GO result, based on term similarity."
      ),
      tags$li(
        strong("Node color:"),
        " adjusted p‑values mapped to the three plot colors from the GO customization panel."
      )
    ),

    h4("How to Read It"),
    tags$ul(
      tags$li("Groups of strongly connected terms usually represent related biological themes."),
      tags$li("Within such a group, node color helps to see which terms have lower adjusted p‑values."),
      tags$li("Isolated terms can point to more specific or separate processes.")
    ),

    h4("What It Is Useful For"),
    tags$ul(
      tags$li("Reducing long GO term lists to a smaller number of term clusters."),
      tags$li("Selecting representative terms from clusters for reporting and downstream analysis.")
    ),

    h4("What It Does Not Provide"),
    tags$ul(
      tags$li("It does not show individual proteins; only terms and their overlap."),
      tags$li("It does not directly display fold changes of proteins.")
    ),

    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The enrichment map groups your selected GO terms into overlapping clusters, making broad biological themes easier to see."
    ),

    # 4) PubMed citations
    h3("PubMed Citations"),

    h4("What These Plots Show"),
    p(
      "The PubMed plots summarise how often the selected GO terms appear in the scientific literature over recent years."
    ),
    tags$ul(
      tags$li(
        strong("X‑axis:"),
        " publication year."
      ),
      tags$li(
        strong("Y‑axis (upper panel):"),
        " proportion of publications for each term across all years in the shown range."
      ),
      tags$li(
        strong("Y‑axis (lower panel):"),
        " absolute number of publications for each term and year."
      ),
      tags$li(
        strong("Line color:"),
        " each GO term is assigned a color derived from the three GO plot colors."
      )
    ),

    h4("How to Read It"),
    tags$ul(
      tags$li("Compare how frequently different GO terms appear in PubMed over time."),
      tags$li("Use the proportion plot to see relative changes within your selected set of terms.")
    ),

    h4("What It Is Useful For"),
    tags$ul(
      tags$li("Getting a quick impression of how strongly different GO terms are represented in recent literature."),
      tags$li("Supporting the choice of terms for manual literature review.")
    ),

    h4("What It Does Not Provide"),
    tags$ul(
      tags$li("It does not assess the quality or content of individual publications."),
      tags$li("Low citation numbers do not necessarily mean that a process is biologically unimportant.")
    ),

    div(
      class = "alert alert-info",
      strong("Summary: "),
      "The PubMed citation plots show how strongly and how recently your selected GO terms appear in published literature."
    ),

    h3("Overall Summary"),
    p(
      "In the GO module, all plots are based on your filtered set of proteins and the resulting enriched GO terms:"
    ),
    tags$ul(
      tags$li(strong("Enrichment score dotplot:"), " compares enriched terms by size, fraction of filtered proteins and adjusted p‑value."),
      tags$li(strong("Cnet plot:"), " links selected terms to individual proteins and their fold changes."),
      tags$li(strong("Enrichment map:"), " shows how selected terms overlap and form clusters."),
      tags$li(strong("PubMed citations:"), " visualises how often selected terms appear in recent publications.")
    ),
    p(
      "Combining these views helps you move from many individual proteins to a structured biological interpretation, ",
      "while keeping in mind that the analysis depends on your chosen filters and available GO annotations."
    )
  )
}

render_GO_saveplots_content_GO <- function() {
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
      tags$li(strong("Slides:"), " 10 inches wide × 7.5 inches tall"),
      tags$li(strong("Full page:"), " 7.5 × 10 inches")
    ),

    h3("Download Process"),
    div(
      class = "well",
      h4("Steps to Download:"),
      tags$ol(
        tags$li("Create and customize your plot"),
        tags$li("Select desired format from dropdown"),
        tags$li("Adjust PPI for raster formats"),
        tags$li("Set width and height in inches"),
        tags$li("Click ", strong("'Download'"), " button"),
        tags$li("Choose save location and filename")
      )
    ),

    h3("Troubleshooting Downloads"),
    tags$dl(
      tags$dt("Download button not working:"),
      tags$dd("Ensure plot is created first, check browser download settings"),

      tags$dt("File too large:"),
      tags$dd("Reduce dimensions or PPI, use JPEG format"),

      tags$dt("Poor quality when printed:"),
      tags$dd("Increase PPI to 600 or use vector format (PDF/SVG)"),

      tags$dt("Text too small/large:"),
      tags$dd("Adjust text sizes before downloading, not image dimensions")
    ),

    p(
      em(
        "Note: The interactive Plotly view can only be saved via the Plotly toolbar. ",
        "The Download panel exports static plots."
      )
    ),

    hr(),

    h4("Add to Plot Grid"),
    p(
      "In addition to downloading individual plots, you can temporarily store the current plot ",
      "and combine it later with other plots."
    ),
    tags$ul(
      tags$li(
        "Click ", tags$b("Add to Grid"), " to store the currently displayed plot together with a short label."
      ),
      tags$li(
        "Labels are optional."
      ),
      tags$li(
        "Stored plots can be accessed and combined in the ", tags$b("Plot Grid module"),
        " to create a multi-panel figure."
      ),
      tags$li(
        "You can add plots from other submodules as well; the Plot Grid works across submodules."
      )
    )
  )
}
