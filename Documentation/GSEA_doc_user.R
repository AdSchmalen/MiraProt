# ==============================================================================
# File: Documentation/GSEA_doc_user.R
#
# Audience:
#   End users running GSEA analyses in the MiraProt application.
#
# Purpose:
#   Provides user-facing GSEA documentation content: conceptual overview,
#   step-by-step workflow, customization guidance, plot interpretation, and
#   export instructions. Section titles are aligned with the navigation labels
#   defined in GSEA_doc_ui.R.
# ==============================================================================

render_GSEA_overview_content_GSEA <- function() {
  div(
    h2("GSEA Module — Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p(
        "The GSEA module performs Gene Set Enrichment Analysis (GSEA) for the dataset loaded in the active analysis context. ",
        "Instead of starting from a filtered gene list, GSEA works on a ranked list of genes ",
        "and tests whether predefined gene sets are enriched at the top or bottom of that ranking. ",
        "The module offers both advanced custom rankings and rankings based on existing columns, ",
        "and provides plots to summarise and visualise the enrichment results."
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

    h3("What it is"),
    p(
      "GSEA tests whether predefined gene sets are concentrated near the top or bottom of an ordered gene list. ",
      "In MiraProt, this module runs the enrichment analysis, stores the results, and provides multiple GSEA-specific plots for interpretation and export."
    ),
    p(
      strong("Leading edge (core enriched genes): "),
      "This is the subset of genes in a set that drives the enrichment peak (or trough). ",
      "It is often the best starting point for biological follow-up."
    ),

    h3("When to use"),
    tags$ul(
      tags$li("You want pathway-level signals rather than only single-gene hits."),
      tags$li("You expect modest but coordinated changes across many genes."),
      tags$li("You want to compare enrichment patterns between contrasts, conditions, or time points."),
      tags$li("You want a method that uses an ordered gene list instead of a strict hit/non-hit list.")
    ),

    h3("How to Configure"),
    div(
      class = "alert alert-info",
      p(
        strong("Choose one ranking mode: "),
        "Custom Ranking computes scores from abundance values using established metrics; ",
        "Pre-calculated Ranking uses existing abundance-ratio and/or p-value columns."
      )
    ),

    h3("Quick Steps — Custom Ranking"),
    div(
      class = "alert alert-success",
      p(strong("Use this when you want MiraProt to compute an advanced ranking from abundance values.")),
      tags$ol(
        tags$li("Load data in the Data Wizard (identifier column + abundance values)."),
        tags$li("Select the matching identifier column in the GSEA tab (e.g. SYMBOL, ENTREZID, UNIPROT)."),
        tags$li("Choose Custom Ranking and pick a ranking method (e.g. Signal‑to‑Noise, Ratio, Fold Change Rank Ordering Statistics)."),
        tags$li("Select numerator (group 1) and denominator (group 2) samples."),
        tags$li("Optionally enable: Absolute values, Break ties randomly, Down‑weight common genes (PADOG)."),
        tags$li("Select a gene set file from the", code("./GSEA/"), "folder (click Refresh Gene Sets after adding new files)."),
        tags$li("Set the number of permutations."),
        tags$li("Significance statistics are computed automatically during the run."),
        tags$li("Click", strong("Run GSEA"), "and wait until results appear."),
        tags$li("Select enriched gene sets or start with the top ones; choose a plot type and click", strong("Create Plot"), "."),
        tags$li("Adjust colours, text sizes, theme, legend; for raster export (PNG/JPEG/TIFF), set ", code("resolution_DPI_GSEA"), " appropriately, or use PDF/SVG for resolution-independent output.")
      )
    ),

    h3("Quick Steps — Pre-calculated Ranking"),
    div(
      class = "alert alert-success",
      p(strong("Use this when ranking can be built directly from existing abundance ratio and p‑value columns.")),
      tags$ol(
        tags$li("Load data (identifier column + abundance ratio + p‑value column) in the Data Wizard."),
        tags$li("Select the matching identifier column."),
        tags$li("Choose Pre‑calculated Ranking and specify one of:"),
        tags$ul(
          tags$li(strong("Abundance ratio:"), "auto log2 transform used as ranking metric."),
          tags$li(strong("P‑value:"), "converted to a score (e.g. −log10)."),
          tags$li(strong("Combined:"), "log2FC × −log10(p‑value) for effect + confidence.")
        ),
        tags$li("Use only valid abundance ratio / p‑value columns (avoid unrelated columns)."),
        tags$li("Select a gene set file from", code("./GSEA/"), "and Refresh if newly added."),
        tags$li("Set the number of permutations."),
        tags$li("Significance statistics are computed automatically during the run."),
        tags$li("Click", strong("Run GSEA"), "; if no sets are reported, review ranking inputs, identifier matching, and gene set content."),
        tags$li("Select enriched sets or use top results; choose a plot type and click", strong("Create Plot"), "."),
        tags$li("Refine appearance and export or add to the Plot Grid; remember raster output (PNG/JPEG/TIFF) depends on ", code("resolution_DPI_GSEA"), " while PDF/SVG are resolution-independent.")
      )
    ),

    h3("How to Interpret"),
    tags$ul(
      tags$li("Positive enrichment means a gene set is concentrated toward the high end of the ordered list; negative enrichment means concentration toward the low end."),
      tags$li("Use NES and FDR together: NES summarizes enrichment strength and direction, while FDR reflects multiple-testing adjusted confidence."),
      tags$li("Use leading-edge genes to identify which members of a pathway drive the result."),
      tags$li("Compare related pathways and shared leading-edge genes before drawing biological conclusions.")
    ),

    h3("Common Pitfalls"),
    tags$ul(
      tags$li("Ranking quality is critical: noisy inputs or unsuitable columns lead to unstable enrichment."),
      tags$li("Identifier mismatches between your table and gene set files reduce usable set size and can hide real signals."),
      tags$li("Very small or very large gene sets are often less informative; use sensible size constraints."),
      tags$li("GSEA supports biological interpretation and hypothesis generation, but it does not prove causality.")
    ),

    h3("Gene Set Files and MSigDB"),
    p(
      "The GSEA module uses external gene set collections that you provide as files. ",
      "These files must be available to the app so that they can be listed and used in the analysis."
    ),
    tags$ul(
      tags$li(
        "Place gene set files into the ", code("./GSEA/"), " folder of your MiraProt installation. ",
        "When the app starts, it scans this folder and lists all supported gene set files in the GSEA module."
      ),
      tags$li(
        "If you add or update gene set files while the app is running, click ",
        em("Refresh Gene Sets"),
        " in the GSEA module to re‑scan the ", code("./GSEA/"), " folder and load the new files."
      ),
      tags$li(
        "You can obtain curated gene sets from the Molecular Signatures Database (MSigDB). ",
        "They are available on the official website at ",
        a(href = "https://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp",
          "MSigDB Gene Sets", target = "_blank"),
        ". Download the sets that match your organism and identifier type and copy the files into the ",
        code("./GSEA/"), " folder to use them in the app."
      )
    ),

    h3("Terminology Cross-link to Technical Documentation"),
    p(
      "To keep user and technical language consistent, this guide uses the same control names as the app UI and technical docs: ",
      code("Identifier_GSEA"), ", ", code("RefenceValues_GSEA"), ", ", code("RankinkMethod_GSEA"), ", ",
      code("fileSelector_GSEA"), ", ", code("createGSEA"), ", ", code("custom_Enrich_select"), ", and ",
      code("create_gsea_plot"), "."
    ),
    p(
      "Likewise, result objects and outputs are referred to consistently as ",
      code("res_GSEA"), ", ", code("current_rankings"), ", ", code("output$GSEAplot_custom"), ", and ",
      code("output$res_GSEA_ready"),
      " across user and technical sections."
    ),

    h3("References"),
    p("The GSEA module in MiraProt builds on established methods, ranking metrics and R packages. Important references include:"),
    tags$ul(
      tags$li(
        strong("GSEA method:"),
        " Subramanian A, Tamayo P, Mootha VK, et al. (2005). ",
        em("Gene set enrichment analysis: A knowledge-based approach for interpreting genome-wide expression profiles."),
        " Proceedings of the National Academy of Sciences 102(43):15545–15550."
      ),
      tags$li(
        strong("Ranking metrics for GSEA:"),
        " Zyla J, Marczyk M, Weiner J, Polanska J (2017). ",
        em("Ranking metrics in gene set enrichment analysis: do they matter?"),
        " BMC Bioinformatics 18:256. PMID: 28499413, PMCID: PMC5427619, DOI: 10.1186/s12859-017-1674-0. ",
        "The custom ranking mode in MiraProt is conceptually based on these ranking metric considerations."
      ),
      tags$li(
        strong("Molecular Signatures Database (MSigDB):"),
        " Liberzon A, Subramanian A, Pinchback R, et al. (2011). ",
        em("Molecular signatures database (MSigDB) 3.0."),
        " Bioinformatics 27(12):1739–1740. ",
        "Gene sets are available from ",
        a(
          href = "https://www.gsea-msigdb.org/gsea/msigdb/genesets.jsp",
          "MSigDB", target = "_blank"
        ),
        "."
      ),
      tags$li(
        strong("clusterProfiler (R package for enrichment analysis):"),
        " Yu G, Wang L‑G, Han Y, He Q‑Y (2012). ",
        em("clusterProfiler: an R package for comparing biological themes among gene clusters."),
        " OMICS: A Journal of Integrative Biology 16(5):284–287."
      )
    ),

    p(
      "If you use the GSEA module in MiraProt for published work, please cite MiraProt itself and the relevant GSEA‑related references above ",
      "in addition to any other resources you use."
    )
  )
}

render_GSEA_dataselection_plotting_content_GSEA <- function() {
  div(
    h2("Data Selection, Ranking, and Plotting"),
    hr(),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      # Step 1 — Load data and define metadata (Data Wizard)
      div(
        class = "panel panel-primary",
        div(class = "panel-heading", h4("Step 1 — Load data and define metadata (Data Wizard)")),
        div(
          class = "panel-body",
          p(
            "Use the ", strong("Data Wizard"), " to load your dataset and define metadata ",
            "(identifier column, abundance/ranking columns and content tags). Accurate metadata ensures the GSEA selectors show the right choices."
          ),
          tags$ul(
            tags$li(strong("Tip:"), " keep a dedicated identifier column (e.g. ", code("Gene symbol"), ", ", code("ENTREZID"), ", ", code("UNIPROT"), ")."),
            tags$li(strong("If using Excel:"), " choose the correct sheet and header row so column names and metadata match.")
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Metadata:</strong> Mark columns that can serve as ranking metrics (for example log2 fold change or custom scores) so the GSEA module can offer them in the selector.")
        )
      ),

      # Step 2 — Select identifier and ranking
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 2 — Select identifier and ranking")),
        div(
          class = "panel-body",
          p("Choose how genes will be ordered for GSEA:"),
          tags$ul(
            tags$li(strong("Identifier column:"), " choose the ID type that matches your gene sets (for example SYMBOL or ENTREZID)."),
            tags$li(strong("Ranking source — two options:"),
                    tags$ul(
                      tags$li(strong("Custom ranking:"), " the app computes advanced ranking scores from your abundance values (recommended when you provide raw abundances). These scores use established ranking metrics from the literature to combine effect size and significance for a more informed ordering."),
                      tags$li(strong("Pre‑calculated ranking:"), " select existing columns from your table. The module supports three sensible pre‑calculated options:"),
                      tags$ul(
                        tags$li(strong("Abundance ratio:"), " an abundance ratio column will be converted to log2 and used as the ranking metric."),
                        tags$li(strong("P‑value:"), " a chosen p‑value column can be used (the module transforms it to a score)."),
                        tags$li(strong("Combined score:"), " when both an abundance ratio and a p‑value are selected, the module can combine them into a single ranking score (log2FC × −log10(p‑value))).")
                      ),
                      tags$li("Important: arbitrary unrelated columns are not treated as valid ranking metrics unless they were explicitly prepared as ranking scores in the Data Wizard. When you choose a 'pre‑calculated' option, pick an abundance ratio or p‑value column (or both) so the module interprets them correctly.")
                    )
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Note:</strong> Custom ranking derives scores from abundance values using literature‑informed metrics; pre‑calculated ranking uses the specific columns you select (abundance ratio and/or p‑value) and optionally their combined score.")
        )
      ),

      # Step 3 — Configure gene sets and parameters
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 3 — Choose gene sets and GSEA parameters")),
        div(
          class = "panel-body",
          p("Select which gene sets to test and configure the analysis:"),
          tags$ul(
            tags$li(strong("Gene set collection:"), " pick a collection available in the app (the app lists files placed in the ", code("./GSEA/"), " folder)."),
            tags$li(strong("Supported file types:"), " use common GSEA formats (for example GMT from MSigDB) so sets map correctly to your identifiers."),
            tags$li(strong("Refresh Gene Sets:"), " if you add files to ", code("./GSEA/"), " while the app runs, click the refresh control to detect them without restarting."),
            tags$li(strong("Number of permutations:"), " controls permutation-based significance estimation (more permutations usually improve stability but increase runtime)."),
            tags$li(strong("Significance statistics:"), " computed automatically from the permutation-based GSEA run (including p-value/FDR reporting).")
          )
        )
      ),

      # Step 4 — Run GSEA
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 4 — Run GSEA")),
        div(
          class = "panel-body",
          p("Run the analysis and interpret immediate outcomes:"),
          tags$ul(
            tags$li("The module builds the ranked list according to your choice (custom metrics from abundance values or the selected pre‑calculated column(s))."),
            tags$li("Each gene set is tested for enrichment across the ranked list using permutation-based significance estimation."),
            tags$li("Reported statistics include enrichment scores, normalized enrichment scores (NES) and corrected p/FDR values."),
            tags$li("If no sets are reported, try increasing permutations and verify ranking inputs, identifier compatibility, and gene set quality.")
          )
        )
      ),

      # Step 5 — Select sets for plotting
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5 — Select gene sets for plotting")),
        div(
          class = "panel-body",
          p("After the run, inspect and choose sets to visualise:"),
          tags$ul(
            tags$li("Use the gene set list to select individual sets or groups of sets."),
            tags$li("Limit the number of displayed sets with the maximum-sets control to keep plots readable."),
            tags$li("When no selection is made, the module can present the top significant sets as a starting point.")
          )
        ),
        div(
          class = "alert alert-success",
          HTML("<strong>Tip:</strong> Selected sets (and their leading-edge genes) can be exported or used by other modules (Volcano, PCA, Heatmap, STRING) to highlight drivers.")
        )
      ),

      # Step 6 — Plot type and appearance
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 6 — Choose plot type and appearance")),
        div(
          class = "panel-body",
          p("Common GSEA visualisations:"),
          tags$ul(
            tags$li(strong("Enrichment score curves:"), " show the running enrichment score and mark positions of set members in the ranked list."),
            tags$li(strong("Ranked value distributions:"), " show ranking metric values (e.g. log2FC or score) for genes in a set."),
            tags$li(strong("Summary plots:"), " compare NES, p‑value or FDR across several sets.")
          ),
          p("Use colour, text size, theme and legend controls to refine the figure; then click Create Plot to render.")
        ),
        div(
          class = "alert alert-success",
          HTML("<strong>Tip:</strong> After creating a plot, that rendered GSEA plot is available for download or for adding to the global Plot Grid.")
        )
      ),

      # Step 7 — Export and integration
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 7 — Export or combine GSEA plots")),
        div(
          class = "panel-body",
          tags$ul(
            tags$li(strong("Download:"), " export the rendered plot in PNG, JPEG, TIFF, PDF, or SVG; set Width/Height and ", code("resolution_DPI_GSEA"), " for raster outputs."),
            tags$li(strong("Add to Grid:"), " include the plot (with optional label) in the Plot Grid to assemble multi‑panel figures across modules.")
          )
        )
      )
    ),

    # Understanding section
    h3("Understanding GSEA in MiraProt"),
    div(
      class = "well",
      tags$ul(
        tags$li(strong("Ranked input:"), " GSEA examines coordinated enrichment across the entire ranked gene list rather than relying on a hard cutoff."),
        tags$li(strong("Custom ranking:"), " when selected, the app computes ranking scores from abundance values using ranking metrics informed by the literature (see Zyla et al., 2017). Custom rankings often combine effect size and significance information and are generally more powerful than a single-column fallback."),
        tags$li(strong("Pre‑calculated ranking:"), " uses columns that already exist in your table: an abundance ratio (converted to log2), a p‑value column, or a combined score derived from both (log2FC × −log10(p‑value))."),
        tags$li(strong("Gene sets:"), " only genes present in both your data and the selected set contribute to that set's enrichment; ensure identifiers match the gene set format."),
        tags$li(strong("Significance:"), " permutation-based p-values and multiple-testing correction (FDR) are used to flag enriched sets.")
      )
    ),

    # Data requirements
    h3("Data Requirements"),
    tags$ul(
      tags$li("A processed table with one identifier column and either abundance values (for custom ranking) or a pre‑calculated ranking column (abundance ratio, p‑value, or both)."),
      tags$li("Metadata that correctly tags identifier and ranking-capable columns so they appear in the GSEA selectors."),
      tags$li("Gene set files in the ", code("./GSEA/"), " folder (GMT is recommended for MSigDB collections) and matching identifier types.")
    ),

    # --- New Chapter: Ranking Methods (Custom Ranking) ---
    h3("Ranking Methods (Custom Ranking)"),
    p("Custom Ranking computes a score for each gene using your chosen method and the two sample groups (numerator vs denominator). Methods are adapted from published evaluations (Zyla et al., 2017) to capture different patterns: raw change, scaled change, robustness to noise, and distribution shifts."),
    tags$ul(
      tags$li(strong("Signal-to-Noise:"), " Difference of group means divided by summed variation. Good default when changes are clear and variance is moderate."),
      tags$li(strong("T-Test:"), " Standardised mean difference. Works well for most contrasts; balances effect size and variability."),
      tags$li(strong("Ratio / log2 Ratio:"), " Fold change (raw or log2). Simple, direction‑aware. Use when magnitude and sign of change matter and variances are similar."),
      tags$li(strong("Difference of Means:"), " Raw mean difference. Use when absolute scale is meaningful (e.g. intensity units) and variability is comparable."),
      tags$li(strong("Sum of Ranks:"), " Non‑parametric (rank-based). Robust to outliers and non‑normal distributions."),
      tags$li(strong("Baumgartner-Weiss-Schindler:"), " Non‑parametric test sensitive to overall distribution shape; useful if shifts affect spread or tails, not only the mean."),
      tags$li(strong("Weighted Average Difference:"), " Mean difference with weights that reduce influence of noisy values; helpful when replicate variability is uneven."),
      tags$li(strong("Fold Change Rank Ordering Statistics (FCROS):"), " Fold changes evaluated across ordered resampled pairs; aims for stability against noise and sample imbalance."),
      tags$li(strong("MWT:"), " Modified statistical score combining difference and dispersion (robust variant). Use when you expect outliers or unequal variances."),
      tags$li(strong("Minimum Significant Difference (MSD):"), " Mean difference divided by pooled standard error; highlights genes whose change exceeds expected noise.")
    ),
    p(strong("Quick guidance:"), " Start with Signal-to-Noise or T-Test for general use. Choose rank-based (Sum of Ranks, BWS) if data look skewed or contain outliers. Use Weighted Average Difference or FCROS when replicate noise varies. Switch to MSD or MWT for contrasts with uneven variance."),

    h3("Advanced Ranking Options"),
    tags$ul(
      tags$li(
        strong("Absolute values:"),
        " Converts each gene’s ranking score to its magnitude (drops the sign). "
      ),
      tags$li(
        strong("Break ties:"),
        " Uses a secondary biological metric to order genes with identical primary scores ",
        "(variance for ratio/log2 ratio; absolute fold change for test/statistics methods; absolute fold change otherwise). ",
        "If ties remain (or no secondary metric is available) a tiny reproducible jitter is added."
      ),
      tags$li(
        strong("Down‑weight common genes (PADOG):"),
        " Applies a specificity weight based on how often each gene appears across all gene sets in the selected file."
      )
    ),

    # PADOG formula block without parentheses around the fraction
    div(
      class = "well",
      h4("PADOG Weight Formula"),
      tags$style(HTML("
    .padog-box { font-size:16px; line-height:1.55; }
    .padog-formula { margin:12px 0 10px 0; font-size:22px; font-weight:600; font-family: 'Helvetica','Arial',sans-serif; }
    .fraction {
      display:inline-block;
      vertical-align:middle;
      text-align:center;
    }
    .fraction .numerator {
      display:block;
      border-bottom:2px solid #333;
      padding:0 8px 4px 8px;
      font-size:18px;
    }
    .fraction .denominator {
      display:block;
      padding:4px 8px 0 8px;
      font-size:18px;
    }
    .legend-list { margin-top:10px; }
  ")),
      div(
        class = "padog-box",
        p("For each gene i the PADOG weight is:"),
        div(
          class = "padog-formula",
          HTML('w<sub>i</sub> = &radic;<span class="fraction"><span class="numerator">F<sub>max</sub> − f<sub>i</sub></span><span class="denominator">F<sub>max</sub> − F<sub>min</sub></span></span>')
        ),
        tags$ul(
          class = "legend-list",
          tags$li(HTML("<code>f<sub>i</sub></code> = number of gene sets containing gene i.")),
          tags$li(HTML("<code>F<sub>max</sub></code> = highest gene count (most frequent gene).")),
          tags$li(HTML("<code>F<sub>min</sub></code> = lowest gene count (least frequent gene).")),
          tags$li(HTML("<code>w<sub>i</sub></code> ∈ [0,1]: weight multiplied with the gene’s ranking score."))
        ),
        p(
          strong("Interpretation: "),
          HTML("Frequent genes (<code>f<sub>i</sub> ≈ F<sub>max</sub></code>) get weights near 0; rare genes (<code>f<sub>i</sub> ≈ F<sub>min</sub></code>) stay near 1. "),
          "Square root softens penalties for moderately frequent genes."
        ),
        p(
          strong("Edge cases: "),
          HTML("If <code>F<sub>max</sub> = F<sub>min</sub></code> → all weights = 1 (no change). "),
          "Genes not found in the file keep their original score."
        ),
        p(
          strong("Rationale: "),
          "Reduces dominance of broadly shared genes so pathway‑specific genes drive enrichment."
        ),
        p(
          strong("When to enable: "),
          "Large overlapping collections; disable for small distinct sets or if you want unadjusted scores."
        )
      )
    ),

    p(
      strong("Absolute values – when useful: "),
      "Choose only if you want to rank by strength regardless of direction (e.g. pathways with mixed up/down regulation). ",
      "Avoid when biological interpretation depends on knowing increase vs decrease."
    ),

    p(
      strong("Tie handling summary: "),
      "GSEA needs unique ranks. Without tie handling, genes with identical scores may be dropped from the ranked list and thus not contribute. ",
      "Enable tie handling when many ties occur to keep those genes in the analysis. ",
      "Disable only if you intentionally want exact ties excluded."
    ),

    # Data validation explanation
    div(
      class = "well",
      h4("Data Validation — Minimum Valid Values"),
      p("Control how many non‑imputed measurements a protein must have to be eligible for ranking. Proteins that do not meet the rule are excluded before ranking and GSEA."),
      tags$ul(
        tags$li(strong("Minimum valid values:"), " required count of non‑imputed (original, non‑missing) measurements."),
        tags$li(strong("Scope (how the count is checked):")),
        tags$ul(
          tags$li(strong("In total:"), " count across all selected samples (numerator + denominator) must be at least the chosen number."),
          tags$li(strong("One group:"), " at least one group (numerator or denominator) must reach the chosen number."),
          tags$li(strong("Each group:"), " both groups independently must reach the chosen number.")
        )
      ),
      p(
        strong("When to use which: ")
      ),
      tags$ul(
        tags$li(strong("In total:"), " flexible choice when replicates are uneven or one group has sparse data."),
        tags$li(strong("One group:"), " allow strong evidence in one condition even if the other is sparse."),
        tags$li(strong("Each group:"), " strict and robust; ensures stable estimates in both groups (recommended for fold‑change‑based methods).")
      ),
      p(
        strong("Tip: "),
        "If too many proteins are filtered out, lower the minimum or choose a more flexible scope (In total or One group). ",
        "If noisy results occur, raise the minimum or use Each group."
      )
    ),

    h3("Quick troubleshooting"),
    tags$ul(
      tags$li(strong("No gene sets found in the UI?"), " place valid gene set files into the ", code("./GSEA/"), " folder and click ", em("Refresh Gene Sets"), "."),
      tags$li(strong("Many genes unmapped?"), " check that identifier types in your data match the gene set IDs (SYMBOL vs ENTREZID)."),
      tags$li(strong("No significant sets?"), " increase permutations and verify ranking inputs, identifier matching, and gene set coverage.")
    )
  )
}

render_GSEA_customizing_content_GSEA <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Colors"),
    p("Use the colour controls to make GSEA plots easy to read and consistent with your figures."),

    div(
      class = "well",
      h4("Colour palette"),
      p(
        "The module provides three base colour pickers (low, medium, high). The app combines these into a palette or gradient depending on the plot."
      ),
      tags$dl(
        tags$dt("Low colour"),
        tags$dd("Used for the lower end of continuous scales (for example low enrichment or low NES)."),
        tags$dt("Medium colour"),
        tags$dd("Used for intermediate values in continuous scales."),
        tags$dt("High colour"),
        tags$dd("Used for the high end of continuous scales (for example strong enrichment).")
      ),
      p("Typical usage:"),
      tags$ul(
        tags$li(strong("Enrichment curves:"), " each selected gene set gets a distinct line colour derived from the palette so you can distinguish sets."),
        tags$li(strong("Ranked value views:"), " single‑set views use one colour; multi‑set comparisons use the palette so colours are consistent across panels."),
        tags$li(strong("Summary plots (NES/FDR):"), " continuous gradients (low → medium → high) map numeric scores such as NES or −log10(FDR).")
      ),
      div(
        class = "alert alert-success",
        HTML("<strong>Tip:</strong> Choose colours with good contrast and avoid very pale extremes. Use distinct hues for multiple lines and perceptually uniform gradients for numeric summaries.")
      )
    ),

    h3("Text and Legend Sizes"),
    p("Adjust text sizes to keep GSEA plots readable in the app and in exported figures."),

    tags$dl(
      tags$dt("Axis title size"),
      tags$dd("Controls the font size of axis titles (for example Rank or Enrichment Score)."),
      tags$dt("Tick label size"),
      tags$dd("Controls the font size of axis tick labels (numbers or ranks)."),
      tags$dt("Legend title size"),
      tags$dd("Controls the font size of legend titles (for example Gene set, NES)."),
      tags$dt("Legend text size"),
      tags$dd("Controls the font size of legend entries and labels.")
    ),
    p("All size controls accept numeric values: increase sizes for presentations and posters, reduce them for compact displays."),

    h3("Plot Themes"),
    p("Select a theme that matches your style and output needs:"),
    div(
      class = "well",
      tags$ul(
        tags$li(strong("Gray:"), " subtle gray background with grid lines."),
        tags$li(strong("Black & White:"), " white background, high-contrast lines — good for publications."),
        tags$li(strong("Linedraw:"), " clear line-only style with minimal shading."),
        tags$li(strong("Light:"), " light background and faint grids for screen viewing."),
        tags$li(strong("Dark:"), " dark background for presentations with dark slides."),
        tags$li(strong("Minimal:"), " minimal decorations; focuses attention on data."),
        tags$li(strong("Classic:"), " traditional bordered style with ticks."),
        tags$li(strong("Void:"), " empty canvas without axes or labels — useful for custom annotations.")
      ),
      div(
        class = "alert alert-info",
        HTML("<strong>Note:</strong> The chosen theme is applied uniformly to all GSEA plots so exported figures look consistent.")
      )
    ),

    h3("Legend Position"),
    p("Control where legends appear:"),
    tags$ul(
      tags$li("Right – vertical legend to the right of the plot."),
      tags$li("Left – vertical legend to the left of the plot."),
      tags$li("Top – horizontal legend above the plot."),
      tags$li("Bottom – horizontal legend below the plot."),
      tags$li("None – hide the legend completely.")
    ),
    p("Choose the position that best preserves plot area and readability; for multi‑panel figures, top/bottom often work best."),

    h3("Number of Sets and Plot Height"),
    p("Control how many gene sets are shown and how much vertical space the plot uses."),
    tags$dl(
      tags$dt("Maximum sets to display"),
      tags$dd("Limits how many gene sets are drawn at once. If more sets are selected, the module shows the most significant ones first to keep plots readable."),
      tags$dt("Plot height (pixels)"),
      tags$dd("Sets the vertical size of the plot. Increase for long labels or many sets; decrease for compact displays.")
    ),

    h3("Applying Changes"),
    p("Most customization settings take effect when you create or recreate a GSEA plot. After changing colours, theme, sizes or legend position, recreate the plot to apply the changes.")
  )
}

render_GSEA_plottypes_content_GSEA <- function() {
  div(
    h2("Plot Types"),
    hr(),

    h3("GSEA Plot Types in MiraProt"),
    p("The module offers several plot types. Each section explains what the plot shows, how to read it, when to use it, and what it does not provide."),

    # General Running Score Plot
    h3("General Running Score Plot"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A running enrichment score traced across the ranked gene list for one selected gene set."),
      tags$li("Markers (ticks) indicating where the set's genes appear in the ranking.")
    ),
    div(
      class = "alert alert-success",
      strong("How the running score is created"),
      p(
        "The running score is created by walking down your ranked gene list from the best-ranked gene to the worst. ",
        "At each gene the score moves in one of two ways:"
      ),
      tags$ul(
        tags$li("If the gene is a member of the tested set, the running score steps up. The step is larger for genes that are ranked more strongly (so highly ranked set members move the score more)."),
        tags$li("If the gene is not in the set, the running score steps down a little, distributing a small negative amount across all non‑set genes.")
      ),
      p(
        "The curve starts at zero. Where the upward steps outweigh the downward steps you see a positive peak (enrichment at the top of the list); ",
        "where downward steps dominate you see a negative trough (enrichment at the bottom). ",
        "The highest absolute deviation of this curve is the enrichment score for that gene set."
      ),
      p("This intuitive walk emphasises whether the set's genes concentrate at one end of the ranking and whether a few top genes or a broader group drive the signal.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("X‑axis: genes ordered from strongest to weakest according to your chosen ranking metric."),
      tags$li("Y‑axis: running enrichment score (peaks up = enrichment high in the list; peaks down = enrichment low in the list)."),
      tags$li("Dense clusters of ticks near a peak identify the leading edge.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Assessing whether enrichment is sharp (few key genes) or broad (many contributing genes)."),
      tags$li("Selecting leading‑edge genes for downstream inspection or validation.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Simultaneous comparison of many sets (single‑set focus)."),
      tags$li("A summary of overall statistics for multiple pathways—use the dotplot or summary views for that.")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use the running score plot to understand where and how a single gene set drives enrichment."),

    # Enrichment score dotplot
    h3("Enrichment Score Dotplot"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("Multiple enriched gene sets displayed as points with position, colour and/or size encoding enrichment strength, significance and/or set size."),
      tags$li("A quick comparative overview of several pathways.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Locate points farthest along the enrichment axis to find strongest signals."),
      tags$li("Use colour (often significance) and size (often gene count) per legend to prioritise sets."),
      tags$li("Filter or limit the number of sets for clarity.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Rapidly identifying top pathways."),
      tags$li("Comparing relative strength and support (significance, size) across sets.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Gene‑level positions—use a running score plot for that."),
      tags$li("Leading‑edge details—use exports or detailed views.")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use the dotplot to rank and compare multiple enriched gene sets at a glance."),

    # Cnet plots
    h3("Cnet Plots"),
    h4("What these plots show"),
    tags$ul(
      tags$li("Networks linking gene sets (pathways) to the genes they contain."),
      tags$li("Two variants: one coloured by fold change (log2FC), one coloured by the ranking metric used for GSEA.")
    ),
    h4("How to read them"),
    tags$ul(
      tags$li("Round or shaped nodes represent gene sets; smaller nodes represent genes."),
      tags$li("Edges connect genes to sets they belong to."),
      tags$li("Gene node colour highlights magnitude (fold change or ranking score).")
    ),
    h4("What they are useful for"),
    tags$ul(
      tags$li("Identifying 'hub' genes shared by multiple sets."),
      tags$li("Exploring how gene‑level behaviour supports overlapping pathways.")
    ),
    h4("What they do not provide"),
    tags$ul(
      tags$li("Precise numeric tables—export if you need exact values."),
      tags$li("Clear readability for very large selections (networks become dense).")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use cnet plots to see which genes link and drive multiple enriched pathways."),

    # Enrichment map
    h3("Enrichment Map"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("A network of enriched gene sets where connections reflect shared genes or similarity."),
      tags$li("Colour and size convey significance or pathway size.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Clusters of connected nodes point to related biological themes."),
      tags$li("Isolated nodes may represent distinct processes worth highlighting separately.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Grouping related pathways into thematic clusters."),
      tags$li("Selecting representative pathways from each cluster for reporting.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Gene‑level details (it summarises relationships between sets)."),
      tags$li("Rich information when only one pathway is selected (needs ≥2 sets).")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use the enrichment map to explore how enriched pathways overlap and form broader themes."),

    # Heatmaps
    h3("Heatmaps"),
    h4("What these plots show"),
    tags$ul(
      tags$li("Colour‑coded matrices of genes versus selected gene sets or categories."),
      tags$li("Two versions: one uses fold changes; the other uses the ranking metric scores.")
    ),
    h4("How to read them"),
    tags$ul(
      tags$li("Patterns of similar colours along a row or column indicate consistent behaviour across sets."),
      tags$li("Contrast in colours highlights subsets of genes with distinct responses.")
    ),
    h4("What they are useful for"),
    tags$ul(
      tags$li("Visualising consistency or divergence of gene responses across multiple pathways."),
      tags$li("Spotting clusters of genes with coordinated fold change or ranking signal.")
    ),
    h4("What they do not provide"),
    tags$ul(
      tags$li("Statistical significance on their own—they are descriptive."),
      tags$li("Comfortable readability with very large gene selections (consider narrowing focus).")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use heatmaps to inspect detailed gene‑level patterns across selected pathways."),

    # Ridgeline plot
    h3("Ridgeline Plot"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("Each pathway (gene set) is shown as a smooth hill‑shaped curve (“ridge”). The curve is a density: it summarizes where that pathway’s genes fall along the chosen ranking metric (e.g. fold change or a combined score)."),
      tags$li("All ridges share the same horizontal scale, stacked vertically only for space (vertical order does NOT indicate importance).")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Horizontal axis: the ranking metric values. Far right = higher values; far left = lower values (interpret in the context of your metric)."),
      tags$li("Peak position: where most genes of that pathway concentrate."),
      tags$li("Peak height: taller means a stronger concentration at that value."),
      tags$li("Width/spread: narrow peak = genes behave similarly; broad/flat ridge = genes vary widely."),
      tags$li("Multiple bumps in one ridge = distinct subgroups of genes with different behaviours."),
      tags$li("Ridges with similar shape and position suggest pathways responding in a similar way.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Comparing how concentrated or variable different pathways are across the ranking metric."),
      tags$li("Spotting pathways whose genes cluster at extreme high or low values."),
      tags$li("Detecting multi‑modal patterns (split behaviour) within a single pathway.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Exact ranks or identities of individual genes (it compresses them into a shape)."),
      tags$li("Clear interpretation if you include too many pathways (overlapping ridges become hard to distinguish)."),
      tags$li("Significance values—this view is descriptive, not a statistical test.")
    ),
    div(
      class = "alert alert-info",
      strong("Summary: "),
      "A ridgeline plot is a comparative ‘fingerprint’ of pathway gene value patterns—position shows where most genes lie, shape shows how uniformly or diversely they respond. Limit the number of pathways for clarity."
    ),

    # Running score variants (implemented: overlay of multiple pathways)
    h3("Running Score Plot (Multiple Pathways)"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("An overlay of running enrichment score curves for the selected pathways."),
      tags$li("Each pathway is shown with its own curve and colour so you can compare peak position (top vs bottom), peak height (strength), and curve shape (sharp vs broad) in one view.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Look for where each curve reaches its maximum upward or downward deviation — this marks the pathway’s strongest enrichment."),
      tags$li("Compare heights for relative strength, and shapes to judge whether enrichment is driven by a compact leading edge or many moderate contributors."),
      tags$li("Limit the number of pathways for clarity; too many curves will overlap.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Directly comparing enrichment dynamics of multiple pathways in the same ranked list."),
      tags$li("Selecting a handful of pathways for deeper inspection with gene‑level views.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Distribution shapes independent of rank order — use ridgeline for that."),
      tags$li("Gene identities — use tables or exports to list leading‑edge genes.")
    ),
    div(class = "alert alert-info", strong("Summary: "), "The overlay view shows where each pathway peaks and how strongly — an efficient way to compare several pathways side by side."),

    # Side-by-side comparison section
    h3("Running Score vs Ridgeline"),
    div(
      class = "well",
      h4("Running score (multiple pathways)"),
      p("Overlay of enrichment curves for the selected pathways. It shows positional enrichment along the ranked list."),
      tags$ul(
        tags$li("Where does each pathway reach its strongest peak (top or bottom of the ranking)?"),
        tags$li("How strong is the peak (height = enrichment strength)?"),
        tags$li("How sharp or broad is the peak (few leading‑edge genes vs many moderate contributors)?"),
        tags$li("Which pathways share similar peak locations or shapes?")
      ),
      p(strong("Use it to:"), " identify leading edges and compare enrichment dynamics directly."),

      h4("Ridgeline"),
      p("Smoothed distribution (density) of gene values (ranking metric) for each pathway."),
      tags$ul(
        tags$li("Are values shifted high or low overall?"),
        tags$li("Are values tightly clustered (narrow peak) or spread out (broad ridge)?"),
        tags$li("Are there multiple distinct peaks (multi‑modal behaviour)?"),
        tags$li("Do different pathways show similar or contrasting distribution shapes?")
      ),
      p(strong("Use it to:"), " compare overall value patterns and heterogeneity between pathways."),

      h4("How to combine them"),
      tags$ul(
        tags$li("Start with running score to see where each pathway concentrates (peak position and strength)."),
        tags$li("Then check ridgeline to see whether gene values are uniform (narrow) or heterogeneous (broad/multi‑modal)."),
        tags$li("Agreement (early sharp peak + narrow high ridge) increases confidence in a focused signal."),
        tags$li("Mismatch (strong peak + very broad ridge) suggests mixed gene contributions worth closer inspection."),
        tags$li("Limit the number of pathways in both views to keep curves and shapes readable.")
      ),

      h4("Quick takeaway"),
      tags$ul(
        tags$li("Running score: positional enrichment and leading edge."),
        tags$li("Ridgeline: distribution shape and spread of pathway gene values."),
        tags$li("Together: where enrichment happens AND how uniformly genes behave.")
      )
    ),

    # PubMed citations
    h3("PubMed Citations"),
    h4("What this plot shows"),
    tags$ul(
      tags$li("How often selected gene sets or their associated terms appear in recent literature."),
      tags$li("Panels typically show relative proportions and absolute publication counts over years.")
    ),
    h4("How to read it"),
    tags$ul(
      tags$li("Rising lines indicate growing attention; stable lines indicate steady interest."),
      tags$li("Compare sets by colour to see which themes dominate recent publications.")
    ),
    h4("What it is useful for"),
    tags$ul(
      tags$li("Prioritising pathways with strong or emerging literature support."),
      tags$li("Providing contextual background for enriched pathways in reports.")
    ),
    h4("What it does not provide"),
    tags$ul(
      tags$li("Assessment of publication quality or experimental evidence."),
      tags$li("Causal inference—counts reflect mention frequency, not validation strength.")
    ),
    div(class = "alert alert-info", strong("Summary: "), "Use PubMed citation plots to gauge literature interest and support for selected pathways."),

    h3("Putting the Plots Together"),
    p(
      "Suggested workflow: start with the enrichment score dotplot to identify key pathways, inspect single pathways with the running score plot to see the leading edge, use cnet plots or heatmaps for gene‑level context, explore overlaps with the enrichment map, compare distributions with ridgeline plots, and consult PubMed citations to assess literature support."
    ),

    h3("Practical Tips"),
    tags$ul(
      tags$li("Limit the number of pathways displayed simultaneously for clarity, especially in network and heatmap views."),
      tags$li("Confirm identifiers match between your data and gene set files before interpreting results."),
      tags$li("For downloadable figures, remember that PNG/JPEG/TIFF quality depends on ", code("resolution_DPI_GSEA"), " while PDF/SVG remain resolution-independent.")
    )
  )
}

render_GSEA_saveplots_content_GSEA <- function() {
  div(
    h2("Save and Export"),
    hr(),
    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download Plot:"), " saves the currently displayed GSEA pathway plot from the plot panel."),
      tags$li(tags$b("Add current plot to Grid:"), " sends the currently displayed GSEA plot to the Plot Grid for later multi-panel assembly."),
      tags$li("The exported figure reflects the current ranking context (selected contrast, ranking metric, and pathway selection).")
    ),
    h3("Plot file formats"),
    p("The plot download panel supports the following file formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats that depend on ", code("resolution_DPI_GSEA"), " (DPI)."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats that are resolution-independent and remain sharp when resized.")
    ),
    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " control the physical size of the exported pathway figure."),
      tags$li(tags$b("DPI ("), code("resolution_DPI_GSEA"), tags$b("):"), " controls output resolution for raster formats such as PNG, JPEG, and TIFF."),
      tags$li("Higher DPI is useful for print-quality raster output, whereas SVG and PDF are resolution-independent."),
      tags$li("For multi-pathway summaries (for example dotplots or enrichment maps), increase width and height to keep pathway labels readable.")
    ),
    div(
      class = "alert alert-info",
      p(
        strong("Practical tip:"),
        " If you plan to edit labels, pathway names, or annotation layers in Illustrator/Inkscape, use SVG or PDF because they are resolution-independent. ",
        "If your submission workflow requires raster files (PNG/JPEG/TIFF), increase ", code("resolution_DPI_GSEA"), " for sharper output."
      )
    ),
    h3("Add the current plot to the Plot Grid"),
    tags$ul(
      tags$li("Use the optional label field if you want a specific panel title in the Plot Grid."),
      tags$li("This is useful for assembling composite figures that compare GSEA views (running score, dotplot, cnet, or map) with other MiraProt modules."),
      tags$li("Make sure the selected pathway set, significance filters, and ranking setup are final before adding the panel to the grid."),
      tags$li("Check that color scales, axis ranges, and titles are consistent across panels before exporting a multi-panel figure.")
    ),
    h3("Notes about interactive plots"),
    p(
      "In the GSEA module, the primary export workflow is the module download panel: choose ",
      code("downloadFormat_GSEA"),
      ", set width/height, and (for raster files) set ",
      code("resolution_DPI_GSEA"),
      "."
    ),
    p(
      "Plot Grid integration is handled through ",
      code("add_to_grid"),
      ", which sends the currently displayed GSEA plot to the shared grid for later composition."
    ),
    h3("Good scientific practice"),
    tags$ul(
      tags$li("Report the gene set database and version used for the GSEA run (for example GO, KEGG, or Reactome releases)."),
      tags$li("Document key GSEA parameters alongside the figure, including ranking metric, gene set size limits, and significance cutoffs."),
      tags$li("State the biological contrast and direction used for ranking so readers can interpret NES signs and pathway regulation direction."),
      tags$li("When presenting pathway plots, pair figures with supporting tables (NES, adjusted p-values, and leading-edge information) for reproducibility.")
    )
  )
}
