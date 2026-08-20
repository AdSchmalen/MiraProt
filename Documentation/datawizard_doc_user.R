# ./Documentation/datawizard_doc_user.R
# Canonical repository path: Documentation/datawizard_doc_user.R
# Datawizard Documentation — User Guide Content
#
# Contains all content rendering functions for the User Guide sections.
# These functions are called by the documentation server in datawizard_doc_ui.R.
#
# Sections:
#   - Overview and workflow
#   - File Loader
#   - Data Tables
#   - Conditions & Metadata (Assign Rules)
#   - Auto-Assign Assistant
#   - Data Filtering
#   - Data Editing
#   - Missing Data Handling (Imputation)
#   - Custom Ratios & Statistics
#   - Batch Effects
#   - Pivot Operations
#   - Merge Operations
#   - Basemean Calculation

############
# Content Rendering Functions - User Guide

############
# Content Rendering Functions - User Guide

render_overview_content_dw <- function() {
  div(
    h2("Datawizard — User Guide Overview"),
    hr(),

    # Hero / purpose
    div(
      class = "alert alert-info",
      h4("What is Datawizard?"),
      p(
        "Datawizard is the central preprocessing hub of MiraProt. ",
        "It loads your primary table (and optional additional files), helps you define and manage metadata, ",
        "and provides tools to clean, filter, transform, impute, create ratios with statistics, ",
        "apply batch corrections, and enrich with baseline metrics. ",
        "The processed data is then passed to the rest of MiraProt (QA, Volcano, Heatmap, Pathways, Networks, Plot Grid, and Export)."
      )
    ),

    h3("What you can do in Datawizard"),
    tags$ul(
      tags$li("Load and validate a wide table of proteomics measurements (rows = proteins, columns = samples)."),
      tags$li("Optionally load additional tables (annotations, external statistics) and merge them."),
      tags$li("Define and manage metadata (groups/conditions, batch, timepoint) via Auto-Assign and Assign Rules."),
      tags$li("Filter obvious artifacts and low-quality rows; optionally edit, relabel, and extend columns."),
      tags$li("Handle missing values with configurable imputation strategies."),
      tags$li("Apply batch effect correction when needed."),
      tags$li("Create custom ratios between groups and run statistical tests."),
      tags$li("Add baseline summaries (e.g., basemean) to support downstream analysis."),
      tags$li("Provide a clean, analysis-ready dataset to all other MiraProt modules.")
    ),

    h3("How Datawizard fits into MiraProt"),
    p(
      "Datawizard writes updates to the central app state so that other modules always see the latest processed data. ",
      "Once you finish here, you can directly explore quality metrics (QA), run Volcano plots and statistics, ",
      "perform GO/GSEA pathway analysis, build STRING networks, make heatmaps/overlaps, and assemble figures."
    ),
    div(
      class = "well",
      h4("Behind the scenes: data lifecycle"),
      p(
        "For users, the important idea is simple: Datawizard keeps the original loaded file safe, works on a current working copy, ",
        "and sends the filtered or final result to tables, export, session save, and the other MiraProt modules."
      ),
      pre(
        style = "background-color:#f8f9fa; padding:10px; border-radius:6px; font-family:monospace;",
        'file load -> registry/core/rv mirrors -> submodule processing -> tables/export/session save -> restore/reset'
      ),
      tags$ul(
        tags$li(strong("Original data"), " stays available as the reset point."),
        tags$li(strong("Working data"), " is the copy changed by Datawizard tools such as merge, edit, imputation, ratios, and basemean."),
        tags$li(strong("Filtered/final data"), " is used for previews and export when you apply filters or final processing."),
        tags$li(strong("Metadata"), " travels with the primary table so downstream modules understand sample groups, ratios, and statistics."),
        tags$li(strong("Session restore"), " reloads the saved data, metadata, module settings, and UI choices before normal work continues.")
      )
    ),

    h3("Prepare your table before assigning metadata"),
    p(
      "Open ", strong("Guide to Preparing Your Data"),
      " from the Datawizard file-loading area whenever an imported table is not yet arranged with one protein per row. ",
      "The in-app guide is the quickest reference for choosing a reshape workflow."
    ),
    div(
      class = "alert", style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      p(
        strong("Target layout: "),
        "each row represents one unique protein. Keep a stable protein identifier column and put measurements, annotations, or contrasts in separate columns."
      )
    ),
    tags$ol(
      tags$li(
        strong("Proteins are in columns: "),
        "use Advanced Processing > Pivot > Transpose, then reshape again if necessary so protein IDs occupy rows."
      ),
      tags$li(
        strong("Annotations or results are in another file: "),
        "load it as Secondary Data and use Merge with matching identifier columns. A Left Join keeps every protein from Primary Data."
      ),
      tags$li(
        strong("A protein is repeated for each condition or contrast: "),
        "use Pivot > Wider; choose the contrast as Names from, the measurement as Values from, and the protein identifier as the ID column."
      ),
      tags$li(
        strong("A third-party tool requires long data: "),
        "use Pivot > Longer to gather measurement columns into a condition column and a value column. Pivot back to wide form before continuing in modules that expect one protein per row."
      ),
      tags$li(
        strong("A complex export needs both operations: "),
        "pivot Secondary Data to one row per protein first, then merge its new columns into Primary Data."
      )
    ),
    p(
      strong("Before continuing: "),
      "inspect the preview after every pivot or merge. Confirm that protein identifiers remain unique, expected columns are present, and a merge did not unexpectedly reduce the row count."
    ),

    hr(),
    div(class = "workflow-box",

        # Step 1: Load
        div(
          class = "panel panel-primary",
          div(class = "panel-heading", h4("Step 1 — Load primary data (optional: additional data)")),
          div(
            class = "panel-body",
            p(
              "Use the ", code("File Loader"), " to import your main wide table (CSV/TSV/XLSX). ",
              "If applicable, add auxiliary tables (e.g., annotations or external stats)."
            ),
            tags$ul(
              tags$li(strong("Tip:"), " keep a dedicated identifier column (e.g., ", code("Gene symbol"), ")."),
              tags$li(strong("Sheets & headers:"), " select the correct sheet (Excel files) and header row.")
            )
          )
        ),

        # Step 2: (Optional) Pivot & Merge
        div(
          class = "panel panel-info",
          div(class = "panel-heading", h4("Step 2 — (Optional) Pivot and Merge")),
          div(
            class = "panel-body",
            p(
              "If your input is not in strict wide format, use ", strong("Pivot"), " to reshape it. ",
              "Use ", strong("Merge"), " to combine primary and additional data by a shared key."
            ),
            div(
              class = "well",
              p(strong("Metadata independent:"), " Pivot and Merge do not require metadata upfront.")
            )
          )
        ),

        # Step 3: (Optional) Batch correction
        div(
          class = "panel panel-warning",
          div(class = "panel-heading", h4("Step 3 — (Optional) Batch Correction")),
          div(
            class = "panel-body",
            p(
              "Apply batch correction if you observe systematic shifts between sample batches. ",
              "Check the QA plots (abundance distributions, PCA/UMAP) before/after to confirm improvements."
            ),
            div(
              class = "well",
              p(strong("Metadata independent:"), " Batch correction can be run before metadata is finalized, ",
                "but accurate batch labels are recommended.")
            )
          )
        ),

        # Step 4: Metadata (required)
        div(
          class = "panel panel-success",
          div(class = "panel-heading", h4("Step 4 — Define metadata (required)")),
          div(
            class = "panel-body",
            p(
              "Create or refine sample annotations (groups/conditions, abundance type, ratio columns, statistics etc.). ",
              "Either assign metadata manually or load an .RDS template file with RegEx to assign metadata automatically."
            )
          )
        ),

        # Step 5: (Recommended) Filtering
        div(
          class = "panel panel-danger",
          div(class = "panel-heading", h4("Step 5 — Filter the data (optional but recommended)")),
          div(
            class = "panel-body",
            p(
              "Remove empty/contaminant rows and proteins with excessive missingness. ",
              "Filtering improves stability for statistics and downstream visualization."
            )
          )
        ),

        # Step 6: (Optional) Edit
        div(
          class = "panel panel-default",
          div(class = "panel-heading", h4("Step 6 — (Optional) Edit")),
          div(
            class = "panel-body",
            p(
              "Fix labels, rename columns, and create helper fields. ",
              "Use this step to harmonize naming and to attach annotations needed later.",
              "Perform simple mathematical operations."
            )
          )
        ),

        # Step 7: (Optional) Imputation
        div(
          class = "panel panel-default",
          div(class = "panel-heading", h4("Step 7 — (Optional) Missing data handling")),
          div(
            class = "panel-body",
            p(
              "Choose an imputation strategy suitable for your missingness pattern (e.g., left-censored, MNAR vs MAR). ",
              "Apply sparingly to avoid inflating significance."
            )
          )
        ),

        # Step 8: (Optional) Ratios & Stats
        div(
          class = "panel panel-default",
          div(class = "panel-heading", h4("Step 8 — (Optional) Custom ratios and statistics")),
          div(
            class = "panel-body",
            p(
              "Define group comparisons (e.g., ", code("Treatment vs Control"), ") and compute ratios with statistical tests. ",
              "Configure multiple comparisons and thresholds to match your study design."
            )
          )
        ),

        # Step 9: (Optional) Basemean
        div(
          class = "panel panel-default",
          div(class = "panel-heading", h4("Step 9 — (Optional) Add basemean values")),
          div(
            class = "panel-body",
            p(
              "Compute basemean values (mean abundance across selected samples) to support visualization."
            )
          )
        ),

        br(),

        # Finalization / hand-off
        div(
          class = "alert alert-success",
          h4("Minimum requirements"),
          p(
            "Load data and assign metadata. Have fun exploring your results."
          )
        ),

        hr(),

        h3("Good practice tips"),
        tags$ul(
          tags$li(strong("Reproducibility:"), " keep track of filters, thresholds, and versioned rule frames (export/import)."),
          tags$li(strong("Validate metadata early:"), " most downstream steps depend on correct grouping."),
          tags$li(strong("QA checkpoints:"), " inspect abundance and PCA/UMAP before/after batch correction.")
        )
    ),

    h3("Common questions"),
    tags$details(
      tags$summary("My groups don’t separate in PCA — what should I try?"),
      p("Revisit filtering thresholds, confirm log transforms, and verify batch labels. Double-check group assignments.")
    ),
    tags$details(
      tags$summary("Volcano shows too few hits — what could be wrong?"),
      p("Check that ratios compare the intended groups, confirm FDR/FC cutoffs, and ensure values are on the expected scale (log2).")
    ),
    tags$details(
      tags$summary("GO/GSEA returns few terms — likely reasons?"),
      p("Verify species and identifier mapping. Consider less stringent hit thresholds or switch to rank-based GSEA.")
    )
  )
}


render_file_loader_content <- function() {
  div(
    h2("File Loader — User Guide"),
    hr(),

    # Purpose / scope
    div(
      class = "alert alert-info",
      h4("What the File Loader does"),
      p(
        "The File Loader is your entry point into Datawizard. ",
        "It imports your primary proteomics table and (optionally) one additional table. ",
        "Primary data is the only dataset passed to other MiraProt modules. ",
        "Additional data becomes available to other modules only after you merge it into the primary data via ",
        strong("Merge Operations"), "."
      )
    ),

    h3("Supported formats"),
    tags$ul(
      tags$li(strong(".csv / .tsv / .txt"), " — wide tables (rows = proteins, columns = samples/metrics)."),
      tags$li(strong(".xlsx / .xls"), " — Excel workbooks; select the sheet after loading.")
    ),

    hr(),
    h3("How loading works"),

    # Step 1
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Step 1 — Select and load your file")),
      div(
        class = "panel-body",
        tags$ol(
          tags$li("Click ", strong("Browse"), " and choose your file (primary or additional)."),
          tags$li("The file is imported directly. ",
                  em("Only after the file is loaded"), " the header row and the sheet selector appear.")
        ),
        tags$ul(
          tags$li(strong("Separator detection:"), " done automatically (comma, tab, semicolon, pipe)."),
          tags$li(strong("Primary Data Tab:"), " passed to other MiraProt modules."),
          tags$li(strong("Additional Data Tab:"), " can be used to extend the primary data.")
        )
      )
    ),

    # Step 2
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 2 — Set header row (and sheet for Excel)")),
      div(
        class = "panel-body",
        p(
          "After loading, choose the ", strong("Header Row"), " (default 1). ",
          "For Excel files, pick the ", strong("Sheet"), " from the dropdown. ",
          "Changing header row or sheet re-reads the file and updates the preview. ",
          "When you switch the primary Excel sheet after processing, Data Wizard starts a fresh primary-data context for that sheet and clears derived filter/final-processing state from the previous sheet."
        ),
        div(
          class = "well",
          p(strong("Tips:")),
          tags$ul(
            tags$li("Keep a dedicated identifier column (e.g., ", code("Accession"), " or ", code("Gene symbol"), ")."),
            tags$li("Ensure column headers are unique and descriptive; duplicates are de-duplicated automatically.")
          )
        )
      )
    ),

    # Step 3
    div(
      class = "panel panel-success",
      div(class = "panel-heading", h4("Step 3 — Review the preview")),
      div(
        class = "panel-body",
        p(
          "Use the preview to verify that identifiers, sample columns, and numeric values look correct. ",
          "If something is off, adjust the header row and (for Excel) the sheet."
        )
      )
    ),

    hr(),
    h3("Primary vs Additional data"),
    div(
      class = "alert", style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      h4("Only primary data flows to other modules"),
      p(
        strong("Primary data"), " is the canonical dataset that QA, Volcano, Heatmap, Pathways, Networks, and Export will use."
      ),
      p(
        strong("Additional data"), " does not propagate by itself. ",
        "Use ", strong("Pivot / Merge Operations"), " to join it onto the primary table. ",
        "After a successful merge, the ", em("extended primary data"), " becomes available to all downstream modules."
      )
    ),

    hr(),
    h3("Guide to Preparing Your Data"),
    div(
      class = "container-fluid dw-doc-loading-guide",
      tags$style(HTML("
        .dw-doc-loading-guide {
          padding-right: 8px;
          font-size: 14px;
          line-height: 1.5;
        }
        .dw-doc-loading-guide .dw-guide-card {
          border: 1px solid #d7e3f4;
          border-radius: 8px;
          padding: 12px;
          margin-bottom: 14px;
          background: #fcfdff;
        }
        .dw-doc-loading-guide .dw-guide-title {
          margin-bottom: 6px;
          color: #1f4e8c;
          font-size: 16px;
          font-weight: 700;
        }
        .dw-doc-loading-guide .dw-guide-sub {
          margin-bottom: 8px;
          color: #2f3b4a;
          font-size: 13px;
        }
        .dw-doc-loading-guide .dw-guide-note {
          color: #3f4d5d;
          font-size: 12px;
        }
        .dw-doc-loading-guide .table-responsive {
          width: 100%;
          margin: 0;
        }
        .dw-doc-loading-guide .dw-guide-table {
          width: 100%;
        }
        .dw-doc-loading-guide .dw-guide-table th {
          background: #eaf2fd;
          color: #1f4e8c;
          font-weight: 700;
          text-align: center;
        }
        .dw-doc-loading-guide .dw-guide-table td,
        .dw-doc-loading-guide .dw-guide-table th {
          border: 1px solid #c9d9ee !important;
          padding: 5px 8px !important;
          font-size: 12px;
        }
        .dw-doc-loading-guide .dw-row-1 { background: #f3f8ff; }
        .dw-doc-loading-guide .dw-row-2 { background: #e8f2ff; }
        .dw-doc-loading-guide .dw-row-3 { background: #deebff; }
        .dw-doc-loading-guide .dw-row-4 { background: #d3e5ff; }
        .dw-doc-loading-guide .dw-row-5 { background: #c8deff; }
        .dw-doc-loading-guide .dw-badge-bad,
        .dw-doc-loading-guide .dw-badge-good {
          display: inline-block;
          border-radius: 12px;
          padding: 2px 8px;
          font-size: 11px;
          font-weight: 700;
        }
        .dw-doc-loading-guide .dw-badge-bad {
          border: 1px solid #f1b7b7;
          background: #ffe9e9;
          color: #9d1f1f;
        }
        .dw-doc-loading-guide .dw-badge-good {
          border: 1px solid #b4e1c2;
          background: #e8f8ee;
          color: #196b35;
        }
        .dw-doc-loading-guide .dw-pipeline {
          margin: 6px 0;
          color: #1f4e8c;
          font-size: 15px;
          font-weight: 700;
          text-align: center;
        }
        .dw-doc-loading-guide .row {
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .dw-doc-loading-guide .row > [class*='col-sm-']:empty {
          display: none;
        }
        .dw-doc-loading-guide .row > .col-sm-5 {
          flex: 1 1 0;
          width: auto;
        }
        .dw-doc-loading-guide .row > .col-sm-2 {
          flex: 0 0 16.66666667%;
          width: 16.66666667%;
        }
        .dw-doc-loading-guide .row > .col-sm-6,
        .dw-doc-loading-guide .row > .col-sm-8 {
          float: none;
          margin-right: auto;
          margin-left: auto;
        }
        @media (max-width: 767px) {
          .dw-doc-loading-guide .row {
            flex-direction: column;
          }
          .dw-doc-loading-guide .row > [class*='col-sm-'] {
            flex: 0 0 auto;
            width: 100%;
          }
          .dw-doc-loading-guide .row > .col-sm-2 .dw-pipeline:first-child {
            transform: rotate(90deg);
          }
        }
      ")),
      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "1) Core rule: one protein per row"),
          div(class = "dw-guide-sub", "MiraProt requires every row to be a unique protein entry. Keep each protein on its own line."),
          div(class = "table-responsive", tags$table(class = "table table-condensed dw-guide-table",
                     tags$thead(tags$tr(tags$th("ID"), tags$th("Measure A"), tags$th("Measure B"))),
                     tags$tbody(
                       tags$tr(class="dw-row-1", tags$td("Protein 1"), tags$td("12.4"), tags$td("0.67")),
                       tags$tr(class="dw-row-2", tags$td("Protein 2"), tags$td("9.8"), tags$td("0.43")),
                       tags$tr(class="dw-row-3", tags$td("Protein 3"), tags$td("15.1"), tags$td("0.88")),
                       tags$tr(class="dw-row-4", tags$td("Protein 4"), tags$td("7.6"), tags$td("0.31")),
                       tags$tr(class="dw-row-5", tags$td("Protein 5"), tags$td("11.2"), tags$td("0.72"))
                     )
          )),
          div(class = "dw-guide-note", "Tip: if your data table looks different, MiraProt offers a broad range of tools to convert it into the correct format. Just follow this guide.")
      ),

      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "2) If proteins are in columns instead of rows"),
          div(class = "dw-guide-sub", "Problem: proteins are spread across columns. Use the Pivot transpose function to flip the table before continuing."),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Click Advanced Processing"), " (right panel), then open the ", tags$strong("Pivot"), " tab."),
            tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
            tags$li("Set ", tags$strong("Pivot Operation"), " = Transpose."),
            tags$li("Click ", tags$strong("Apply Pivot"), ". The row/column orientation is now switched."),
            tags$li("If needed, run a second pivot step after transposing to place ", tags$strong("one Protein ID per row"), ".")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Before (proteins in columns)"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Sample"), tags$th("Protein 1"), tags$th("Protein 2"), tags$th("Protein 3"))),
                              tags$tbody(tags$tr(tags$td("S1"), tags$td("8.1"), tags$td("7.2"), tags$td("6.8")))
                   ))
            ),
            column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Transpose")),
            column(5, span(class="dw-badge-good", "After transpose"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Sample"), tags$th("S1"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("Protein 1"), tags$td("8.1")),
                                tags$tr(class="dw-row-3", tags$td("Protein 2"), tags$td("7.2")),
                                tags$tr(class="dw-row-4", tags$td("Protein 3"), tags$td("6.8"))
                              )
                   ))
            )
          )
      ),

      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "3) If your data is split across files"),
          div(class = "dw-guide-sub", "Use Merge to attach columns from a secondary table to your primary table using a shared ID column."),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Open Advanced Processing"), " and select the ", tags$strong("Merge"), " tab."),
            tags$li("Set ", tags$strong("Primary Data Join Column"), " to your ID in the main table (for example: Protein ID)."),
            tags$li("Set ", tags$strong("Secondary Data Join Column"), " to the matching ID column in the second table."),
            tags$li("Choose ", tags$strong("Additional Columns from Secondary Data"), " (for example: Gene Name, Pathway, p-value)."),
            tags$li("Set ", tags$strong("Join Type"), " = Left Join (recommended) to keep all primary proteins."),
            tags$li("Check ", tags$strong("Merge Preview"), " and click ", tags$strong("Apply Merge"), ".")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Primary Data"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"))),
                              tags$tbody(
                                tags$tr(tags$td("P001"), tags$td("12.4")),
                                tags$tr(tags$td("P002"), tags$td("9.8"))
                              )
                   ))
            ),
            column(2, div(class="dw-pipeline", "+")),
            column(5, span(class="dw-badge-bad", "Secondary Data"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Gene"), tags$th("Pathway"))),
                              tags$tbody(
                                tags$tr(tags$td("P001"), tags$td("TP53"), tags$td("DNA repair")),
                                tags$tr(tags$td("P002"), tags$td("EGFR"), tags$td("Signaling"))
                              )
                   ))
            )
          ),
          fluidRow(
            column(12, div(class="dw-pipeline", "⟱ Merge (Left Join) ⟱"))
          ),
          fluidRow(
            column(3),
            column(6, span(class="dw-badge-good", "After Merge"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"), tags$th("Gene"), tags$th("Pathway"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P001"), tags$td("12.4"), tags$td("TP53"), tags$td("DNA repair")),
                                tags$tr(class="dw-row-3", tags$td("P002"), tags$td("9.8"), tags$td("EGFR"), tags$td("Signaling"))
                              )
                   ))
            ),
            column(3)
          ),
          div(class = "dw-guide-note", "Tip: if row count drops unexpectedly, check that both join columns use the same ID format.")
      ),

      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "4) If proteins appear multiple times (conditions/contrasts)"),
          div(class = "dw-guide-sub", "If one Protein ID repeats by Condition/Contrast, use Pivot to convert repeated rows into one row per protein."),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Open Advanced Processing"), " and switch to the ", tags$strong("Pivot"), " tab."),
            tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
            tags$li("Set ", tags$strong("Pivot Operation"), " = Wider."),
            tags$li("Set ", tags$strong("Names from"), " = Contrast (this creates one column per contrast)."),
            tags$li("Set ", tags$strong("Values from"), " = Value."),
            tags$li("Set ", tags$strong("ID columns"), " = ID (keeps one row per protein)."),
            tags$li("Click ", tags$strong("Apply Pivot"), " and verify the result in the preview/table.")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Before Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("ID"), tags$th("Contrast"), tags$th("Value"))),
                              tags$tbody(
                                tags$tr(tags$td("P1"), tags$td("A_vs_B"), tags$td("1.3")),
                                tags$tr(tags$td("P1"), tags$td("A_vs_C"), tags$td("0.8")),
                                tags$tr(tags$td("P2"), tags$td("A_vs_B"), tags$td("1.1"))
                              )
                   ))
            ),
            column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Wider)")),
            column(5, span(class="dw-badge-good", "After Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P1"), tags$td("1.3"), tags$td("0.8")),
                                tags$tr(class="dw-row-3", tags$td("P2"), tags$td("1.1"), tags$td("NA"))
                              )
                   ))
            )
          )
      ),

      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "5) Reshape data tables for third party analysis tools"),
          div(class = "dw-guide-sub", "Your table can start with one protein per row, while condition measurements are spread across multiple columns (for example Intensity_Control and Intensity_Treated). Pivot Longer reshapes this if needed."),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Open Advanced Processing"), " and go to the ", tags$strong("Pivot"), " tab."),
            tags$li("Set ", tags$strong("Select Data to Pivot"), " = Primary Data."),
            tags$li("Set ", tags$strong("Pivot Operation"), " = Longer."),
            tags$li("Set ", tags$strong("Columns to pivot"), " = Intensity_Control, Intensity_Treated."),
            tags$li("Set ", tags$strong("Names to"), " = Condition."),
            tags$li("Set ", tags$strong("Values to"), " = Intensity."),
            tags$li("Click ", tags$strong("Apply Pivot"), " to create one value column with a condition label column.")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Before Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Intensity_Control"), tags$th("Intensity_Treated"))),
                              tags$tbody(
                                tags$tr(tags$td("P001"), tags$td("10.2"), tags$td("14.1")),
                                tags$tr(tags$td("P002"), tags$td("8.7"), tags$td("9.4"))
                              )
                   ))
            ),
            column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Longer)")),
            column(5, span(class="dw-badge-good", "After Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Condition"), tags$th("Intensity"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P001"), tags$td("Intensity_Control"), tags$td("10.2")),
                                tags$tr(class="dw-row-3", tags$td("P001"), tags$td("Intensity_Treated"), tags$td("14.1")),
                                tags$tr(class="dw-row-4", tags$td("P002"), tags$td("Intensity_Control"), tags$td("8.7"))
                              )
                   ))
            )
          ),
          div(class = "dw-guide-note", "Tip: Pivot Longer is usually a temporary preparation step (i.e. for other proteomic tools). If you need one protein per row again for downstream modules, pivot back to wider afterward or reset your data.")
      ),

      div(class = "dw-guide-card",
          div(class = "dw-guide-title", "6) Combine as needed"),
          div(class = "dw-guide-sub", "For complex data (for example Spectronaut-style data exports), combine Pivot and Merge."),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Open Advanced Processing"), " and switch to the ", tags$strong("Pivot"), " tab."),
            tags$li("Set ", tags$strong("Select Data to Pivot"), " = Secondary Data."),
            tags$li("Set ", tags$strong("Pivot Operation"), " = Wider."),
            tags$li("Set ", tags$strong("Names from"), " = Contrast (creates one column per contrast)."),
            tags$li("Set ", tags$strong("Values from"), " = Value (or Ratio)."),
            tags$li("Set ", tags$strong("ID columns"), " = Protein ID."),
            tags$li("Click ", tags$strong("Apply Pivot"), " and verify one row per protein in Secondary Data.")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Secondary Data Before Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Contrast"), tags$th("Value"))),
                              tags$tbody(
                                tags$tr(tags$td("P001"), tags$td("A_vs_B"), tags$td("1.30")),
                                tags$tr(tags$td("P001"), tags$td("A_vs_C"), tags$td("0.82")),
                                tags$tr(tags$td("P002"), tags$td("A_vs_B"), tags$td("1.12"))
                              )
                   ))
            ),
            column(2, div(class="dw-pipeline", "⟶"), div(class="dw-pipeline", "Pivot (Wider)")),
            column(5, span(class="dw-badge-good", "Secondary Data After Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P001"), tags$td("1.30"), tags$td("0.82")),
                                tags$tr(class="dw-row-3", tags$td("P002"), tags$td("1.12"), tags$td("NA"))
                              )
                   ))
            )
          ),
          tags$hr(style = "margin: 12px 0;"),
          tags$ul(
            style = "margin-bottom: 10px; padding-left: 20px;",
            tags$li(tags$strong("Open Advanced Processing"), " and select the ", tags$strong("Merge"), " tab."),
            tags$li("Set ", tags$strong("Primary Data Join Column"), " = Protein ID in the primary table."),
            tags$li("Set ", tags$strong("Secondary Data Join Column"), " = Protein ID in ", tags$strong("Secondary Data After Pivot"), "."),
            tags$li("Choose ", tags$strong("Additional Columns from Secondary Data"), " = Value_A_vs_B, Value_A_vs_C."),
            tags$li("Set ", tags$strong("Join Type"), " = Left Join (recommended) to keep all primary proteins."),
            tags$li("Check ", tags$strong("Merge Preview"), " and click ", tags$strong("Apply Merge"), ".")
          ),
          fluidRow(
            column(5, span(class="dw-badge-bad", "Primary Data"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"))),
                              tags$tbody(
                                tags$tr(tags$td("P001"), tags$td("12.4")),
                                tags$tr(tags$td("P002"), tags$td("9.8"))
                              )
                   ))
            ),
            column(2, div(class="dw-pipeline", "+")),
            column(5, span(class="dw-badge-good", "Secondary Data After Pivot"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P001"), tags$td("1.30"), tags$td("0.82")),
                                tags$tr(class="dw-row-3", tags$td("P002"), tags$td("1.12"), tags$td("NA"))
                              )
                   ))
            )
          ),
          fluidRow(
            column(12, div(class="dw-pipeline", "⟱ Merge (Left Join) ⟱"))
          ),
          fluidRow(
            column(2),
            column(8, span(class="dw-badge-good", "After Merge"),
                   div(class = "table-responsive", tags$table(class="table table-condensed dw-guide-table",
                              tags$thead(tags$tr(tags$th("Protein ID"), tags$th("Abundance"), tags$th("Value_A_vs_B"), tags$th("Value_A_vs_C"))),
                              tags$tbody(
                                tags$tr(class="dw-row-2", tags$td("P001"), tags$td("12.4"), tags$td("1.30"), tags$td("0.82")),
                                tags$tr(class="dw-row-3", tags$td("P002"), tags$td("9.8"), tags$td("1.12"), tags$td("NA"))
                              )
                   ))
            ),
            column(2)
          ),
          div(class = "dw-guide-note", "Tip: keep Protein ID names consistent across Primary Data and Secondary Data before Merge.")
      )
    ),
    hr(),
    h3("What is handled automatically"),
    tags$ul(
      tags$li(strong("Delimiter (separator):"), " auto-detected from the file content."),
      tags$li(strong("Column names:"), " cleaned and made unique; empty names are handled safely."),
      tags$li(strong("File size checks:"), " basic validation to avoid loading obviously problematic files.")
    ),

    hr(),
    h3("Troubleshooting"),
    tags$dl(
      tags$dt("Excel file shows the wrong columns/rows"),
      tags$dd("Select the correct ", strong("Sheet"), " and set the correct ", strong("Header Row"), "."),

      tags$dt("CSV/TSV looks scrambled"),
      tags$dd("The delimiter is auto-detected. If still wrong, check that the file is really CSV/TSV and not exported with exotic separators."),

      tags$dt("Strange characters / umlauts"),
      tags$dd("Encoding is detected automatically. If you still see issues, re-save the file as UTF-8 (e.g., via Excel → CSV UTF-8)."),

      tags$dt("Duplicate or missing column names"),
      tags$dd("The loader makes names unique and ignores truly empty names; consider fixing the source file for clarity."),

      tags$dt("Large files load slowly"),
      tags$dd("Be patient after clicking Load. Very large tables can take time; consider reducing columns/rows for initial testing.")
    )
  )
}

render_data_tables_content <- function() {
  div(
    h2("Data Tables"),
    hr(),

    h3("Overview"),
    p(
      "This chapter covers three connected views: ",
      strong("Primary Data Viewer"), ", ",
      strong("Additional Data Viewer"), ", and ",
      strong("Define Metadata"), ". Together, they let you inspect the working tables and describe the primary-data columns before continuing."
    ),
    div(
      class = "alert",
      style = "background-color: #3498db; border-color: #3498db; color: #fff;",
      h4("How data flows through MiraProt"),
      tags$ul(
        style = "margin-bottom: 0; padding-left: 20px;",
        tags$li("Primary data is the working table used by Data Wizard and handed to downstream MiraProt modules."),
        tags$li("Additional data is a separate secondary table."),
        tags$li("Additional data reaches downstream modules only after relevant columns are merged into primary data."),
        tags$li("Metadata describes the columns of the primary table and determines how downstream modules interpret them.")
      )
    ),

    hr(),
    h3("Primary Data Viewer"),
    p(
      "The Primary Data Viewer displays the current working primary table—not merely the originally uploaded file. ",
      "It therefore reflects accepted filtering, row or column removal, merges, and other Data Wizard operations."
    ),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Status, dimensions, and preview")),
      div(
        class = "panel-body",
        tags$ul(
          style = "margin-bottom: 0; padding-left: 20px;",
          tags$li(strong("RAW DATA"), " means the current table still matches the loaded primary data."),
          tags$li(strong("MODIFIED DATA"), " means filtering, removal, or another Data Wizard operation has changed the current table."),
          tags$li("The summary reports the current primary table's row and column dimensions and its status."),
          tags$li("The summary also reports how many rows and columns are displayed in the current preview, which may be smaller than the complete table.")
        )
      )
    ),
    h4("Explore and edit the table"),
    tags$ul(
      style = "padding-left: 20px;",
      tags$li(strong("Search:"), " use the search field to find matching values."),
      tags$li(strong("Sort:"), " click a column heading to change the row order."),
      tags$li(strong("Pagination:"), " move between pages of displayed rows with the table controls."),
      tags$li(strong("Single-row selection:"), " click one row when you need to act on it."),
      tags$li(strong("Remove primary column:"), " choose an unwanted column, then use the remove-column control."),
      tags$li(strong("Remove selected row:"), " select one row in the table, then click ", strong("Remove selected row"), ".")
    ),
    p(
      "Deleting a primary column changes the structure of the working table, so the corresponding metadata must remain aligned with the changed table. ",
      "Review the metadata feedback after a deletion."
    ),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Working with very large tables")),
      div(
        class = "panel-body",
        tags$ul(
          style = "margin-bottom: 0; padding-left: 20px;",
          tags$li("Very large tables initially use a limited preview for responsiveness."),
          tags$li(strong("Enable full-table interaction"), " requests the complete interactive table."),
          tags$li(strong("Return to preview"), " switches back to the lighter view."),
          tags$li("A limited display does not mean that undisplayed data has been deleted.")
        )
      )
    ),
    div(
      class = "alert",
      style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      strong("Recommended: "),
      "Verify the primary table before continuing, because this is the table used by downstream modules."
    ),

    hr(),
    h3("Additional Data Viewer"),
    p(
      "The Additional Data Viewer shows the secondary data table and appears only when additional data has been loaded. ",
      "Additional data remains separate from primary data until you perform a merge."
    ),
    p(
      "Loading or viewing additional data alone does not send it to QA, Volcano, Heatmap, Pathways, Networks, Export, or other downstream modules. ",
      "Only columns merged into the primary table become part of the data passed onward."
    ),
    h4("Explore and edit additional data"),
    tags$ul(
      style = "padding-left: 20px;",
      tags$li("Search for values, sort by column headings, and use pagination to move through the displayed rows."),
      tags$li("Use single-row selection before applying a row action."),
      tags$li("Choose a column with ", strong("Remove additional column"), " and use the remove-column control to delete it from additional data."),
      tags$li("Select one row and click ", strong("Remove selected row"), " to delete that row from additional data."),
      tags$li("For very large tables, the viewer may begin with a limited preview. Use ", strong("Enable full-table interaction"), " to request the complete interactive table and ", strong("Return to preview"), " to return to the lighter view."),
      tags$li("Rows or columns outside a limited display remain in the additional table; they have not been deleted.")
    ),
    div(
      class = "panel panel-success",
      div(class = "panel-heading", h4("Additional-data workflow")),
      div(
        class = "panel-body",
        tags$ul(
          style = "margin-bottom: 0; padding-left: 20px;",
          tags$li("Inspect the shared identifier in both tables."),
          tags$li("Prepare or pivot the additional table if necessary."),
          tags$li("Use Pivot / Merge Operations."),
          tags$li("Review the merged primary table afterward.")
        )
      )
    ),

    hr(),
    h3("Define Metadata"),
    p(
      "The metadata table contains one descriptive row for each primary-data column. Metadata tells MiraProt which columns contain identifiers, ",
      "abundance measurements, ratios, p-values, sample labels, and other content."
    ),
    p(
      "Downstream modules use the synchronized metadata to populate controls and interpret primary-data columns. ",
      "Edit supported cells directly in the table and use the provided dropdown choices where available."
    ),
    p(
      "Use this view to operate and review the metadata table; the exhaustive metadata-field reference remains in the ",
      strong("Conditions & Metadata"), " chapter. Check the metadata health/status feedback and resolve reported alignment or completeness problems before downstream analysis."
    ),

    h4("Live metadata synchronization"),
    p(strong("Pause metadata live synchronization"), " is selected by default in the current interface."),
    tags$ul(
      tags$li("When pause mode is off, each accepted metadata edit is synchronized automatically."),
      tags$li("When pause mode is on, edits appear immediately in the visible metadata table, but downstream modules continue using the last synchronized version.")
    ),
    p(
      "Pause mode is useful when you want to complete several related edits before triggering downstream updates. ",
      "The manual synchronization controls appear only while pause mode is active."
    ),
    p("While paused, the control reports one of these two messages:"),
    tags$ul(
      tags$li(code("Metadata edits are pending synchronization.")),
      tags$li(code("No pending metadata edits."))
    ),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Synchronize a set of metadata edits")),
      div(
        class = "panel-body",
        tags$ol(
          style = "margin-bottom: 0; padding-left: 20px;",
          tags$li("Leave or turn on pause mode."),
          tags$li("Make the required metadata edits."),
          tags$li("Review the visible table."),
          tags$li("Click ", strong("Synchronize metadata"), "."),
          tags$li("Wait until ", code("No pending metadata edits."), " is reported."),
          tags$li("Then continue to downstream modules.")
        )
      )
    ),
    div(
      class = "alert",
      style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      strong("Before analysis: "),
      "Synchronize pending metadata before running analyses that depend on metadata."
    ),

    hr(),
    h3("Recommended workflow"),
    tags$ol(
      tags$li("Load and inspect primary data."),
      tags$li("Load and inspect additional data if required."),
      tags$li("Remove unwanted rows or columns carefully."),
      tags$li("Define or review metadata for the primary table."),
      tags$li("Synchronize pending metadata edits."),
      tags$li("Merge required additional columns into primary data."),
      tags$li("Recheck primary data and metadata before continuing.")
    )
  )
}

render_assign_rules_content <- function() {
  div(
    h2("Conditions & Metadata"),
    hr(),

    h3("Overview"),
    p("The Conditions & Metadata Loader allows you to define experimental conditions or load an .RDS file with regular expressions for automatic metadata assignment."),

    h3("Defining Conditions"),
    div(
      class = "well",
      h4("Steps to Define Conditions:"),
      tags$ol(
        tags$li("Click ", strong("Add"), " to create a new condition group"),
        tags$li("Enter a meaningful name (e.g., 'Control', 'Treatment', 'Knockout')"),
        tags$li("Condition dropdowns update shortly after you pause typing, so text entry remains responsive while metadata options stay in sync."),
        tags$li("Repeat for all experimental conditions"),
        tags$li("Use ", strong("Remove"), " to delete the last condition if needed")
      )
    ),

    hr(),

    h3("Loading Metadata Rule Sets"),
    p("You can load .RDS files that contain regular expression to automatically assign conditions and define metadata:"),
    tags$ul(
      tags$li(strong("File Location:"), " Use AutoAssign/ for a source or local installation, or shiny-app/AutoAssign/ for a portable installation"),
      tags$li(strong("Create:"), " Use the Auto-Assign Assistant to create and export new regular expression rule sets"),
      tags$li(strong("Older files:"), " Existing rule files with additional settings can still be loaded")
    ),
    hr(),

    h3("Metadata Definition"),
    div(
      class = "well",
      h4("Metadata Table"),
      p("The metadata table is crucial for defining how each column should be interpreted:"),

      h5("Key Metadata Fields:"),
      tags$dl(
        tags$dt("Column:"),
        tags$dd("The original column name from your data file"),

        tags$dt("Content:"),
        tags$dd("Descriptive label for the column content"),
        tags$dt("Type:"),
        tags$dd(
          tags$ul(
            tags$li(strong("Identifier:"), " Protein identifier"),
            tags$li(strong("Abundance Ratio:"), " Comparison of the abundances between two groups"),
            tags$li(strong("Abundance Ratio p-Value:"), " Statistical significance of the abundance ratio"),
            tags$li(strong("Abundance Ratio Adj. p-Value:"), " Statistical significance of the abundance ratio corrected for multiple testing"),
            tags$li(strong("Raw Abundance:"), " Raw abundance values"),
            tags$li(strong("Normalized Abundance:"), " Abundance values normalized between samples"),
            tags$li(strong("Batch Corrected Abundance:"), " Abundance values after correction of batch effects"),
            tags$li(strong("Imputed Raw Abundance:"), " Raw abundances with imputed values"),
            tags$li(strong("Imputed Normalized Abundance:"), " Normalized abundances with imputed values"),
            tags$li(strong("Imputed Batch Corrected Abundance:"), " Batch corrected abundances with imputed values"),
            tags$li(strong("Found in Sample:"), " Protein identification per sample"),
            tags$li(strong("Found in File:"), " Protein identification per file"),
            tags$li(strong("Protein Confidence:"), " Confidence value or string"),
            tags$li(strong("# PSMs:"), " Number of peptide spectrum matches (required for DEqMS statistics)"),
            tags$li(strong("Additional Information:"), " irrelevant for futher analysis"),
            tags$li(strong("Description:"), " Protein description (irrelevant for further analysis)"),
            tags$li(strong("Row Index:"), " Unique row index (set automatically)")
          )
        ),

        tags$dt("Options:"),
        tags$dd("Additional details depending on the content type. The expected values are:"),
        tags$dd(
          tags$ul(
            tags$li(strong("Content 'Identifier':"), " Copy of the primary data's column header"),
            tags$li(strong("Abundance Ratio:"), " 'Ratio'"),
            tags$li(strong("Abundance Ratio p-Value:"), " 'Ratio'"),
            tags$li(strong("Abundance Ratio Adj. p-Value:"), " 'Ratio'"),
            tags$li(strong("Raw Abundance:"), " Experimental condition label"),
            tags$li(strong("Normalized Abundance:"), " Experimental condition label"),
            tags$li(strong("Batch Corrected Abundance:"), " Experimental condition label"),
            tags$li(strong("Imputed Raw Abundance:"), " Experimental condition label"),
            tags$li(strong("Imputed Normalized Abundance:"), " Experimental condition label"),
            tags$li(strong("Imputed Batch Corrected Abundance:"), " Experimental condition label"),
            tags$li(strong("Found in Sample:"), " Experimental condition label"),
            tags$li(strong("Found in File:"), " Experimental condition label")
          )
        ),
        tags$dd("Note: Experimental conditions have to be defined in the panel 'Conditions & Metadata Loader' to appear as drop down option."),

        tags$dt("Numerator:"),
        tags$dd("Only for rows labeled as 'Ratio' in the Options column"),
        tags$dd("Experimental condition label in the numerator of the ratio"),

        tags$dt("Denominator:"),
        tags$dd("Only for rows labeled as 'Ratio' in the Options column"),
        tags$dd("Experimental condition label in the denominator of the ratio"),

        tags$dt("Transformation:"),
        tags$dd("Has the original data been transformed before loading into MiraProt (e.g. log2 transformation)"),

        tags$dt("Sample:"),
        tags$dd("Sample identifer (unique for the corresponding Column and content pair)")
      )
    ),

    h3("Editing Metadata"),
    tags$ol(
      tags$li("Click on any cell in the metadata table to edit"),
      tags$li("Use dropdown menus for predefined options"),
      tags$li("Type directly for free-text fields"),
      tags$li("By default, metadata changes are synchronized automatically after each edit"),
      tags$li("For larger manual edits, activate 'Pause metadata synchronization while editing', make several changes, then click 'Synchronize metadata now'"),
      tags$li("When synchronization is paused, the table updates immediately, but downstream tools use the last synchronized metadata until you synchronize"),
      tags$li("Use Auto-Assign to automatically populate metadata based on patterns")
    ),

    h3("Metadata Details View"),
    p("Toggle 'Show metadata details' to see a read-only view of all metadata. This table is passed to the subsequent MiraProt modules")
  )
}

render_auto_assign_content <- function() {
  div(
    h2("Auto-Assign Assistant"),
    hr(),

    h3("Overview"),
    p("The Auto-Assign Assistant uses pattern recognition and rules to automatically populate metadata fields based on column names and data characteristics."),
    div(
      class = "alert alert-info",
      strong("Make rule sets available in future R sessions: "),
      "Place the exported ", code(".rds"), " file in ", code("AutoAssign/"),
      " for a source or local installation, or in ", code("shiny-app/AutoAssign/"),
      " for a portable installation. The rule set becomes available in the rule-set dropdown after MiraProt is restarted."
    ),
    p("The assistant presents its workflow in this order:"),
    tags$ol(
      tags$li("Auto RegEx"),
      tags$li("Content Rules"),
      tags$li("Condition Extraction Rules"),
      tags$li("Ratio Analysis Rules"),
      tags$li("Export")
    ),

    h3("Auto RegEx — Infer Rules from Existing Metadata"),
    p(
      "Auto RegEx learns reusable assignment rules from an existing metadata table. It does not modify the source data. ",
      "When inference succeeds, the resulting rules are transferred into the existing Auto-Assign editor, where they can be inspected and changed."
    ),

    h4("When to use Auto RegEx"),
    tags$ul(
      tags$li("The metadata already contains the expected Content assignments."),
      tags$li("Sample or condition labels are available where they apply."),
      tags$li("Numerator and Denominator values are available for ratio columns."),
      tags$li("You want reusable rules for files with a similar column structure.")
    ),

    h4("Choose a metadata source"),
    div(
      class = "well",
      tags$ul(
        tags$li(
          strong("Current MiraProt metadata:"),
          " Uses the metadata currently visible in Data Wizard. Choose this for work already prepared in the active session; review the readiness message before inference."
        ),
        tags$li(
          strong("Excel workbook:"),
          " Uploads a workbook, lets you select a worksheet, and provides field mapping. Choose this for an external reference table or a reusable training workbook. If the worksheet changes, review the mappings again."
        )
      )
    ),

    h4("Fields used for inference"),
    tags$ul(
      tags$li(strong("Column (required):"), " the original data column header from which patterns are learned."),
      tags$li(strong("Content:"), " the expected Content assignment used for content inference."),
      tags$li(strong("Options or mapped condition target:"), " the condition value used for condition inference."),
      tags$li(strong("Numerator and Denominator:"), " the expected pair used for ratio inference."),
      tags$li(strong("Transformation:"), " an existing transformation to retain in the inferred content rule.")
    ),

    h4("Auto RegEx workflow"),
    tags$ol(
      tags$li("Open the Auto-Assign Assistant and select the Auto RegEx tab."),
      tags$li(
        "Choose ",
        strong("Current MiraProt metadata"),
        " or ",
        strong("Excel workbook"),
        " as the source."
      ),
      tags$li(
        "For current metadata, review the readiness message. ",
        "For Excel, upload the workbook, select the worksheet, and check the field mappings."
      ),
      tags$li(
        "Review the metadata before inference. Incorrect or incomplete ",
        strong("Content"),
        ", ",
        strong("Options"),
        ", ",
        strong("Numerator"),
        ", or ",
        strong("Denominator"),
        " assignments can prevent or distort rule inference."
      ),
      tags$li("Choose the global Regex redundancy level."),
      tags$li(
        "Click ",
        strong("Infer Rules"),
        ". Auto RegEx validates the source, infers the rules, checks their replay, and transfers a valid result to the other Auto-Assign tabs."
      ),
      tags$li(
        "Open the diagnostic sections when you want to review how Content, conditions, or ratios were inferred."
      ),
      tags$li(
        "After inference, you can change the global redundancy or set a different redundancy for an individual Content rule. ",
        "These changes rebuild the displayed candidate from the cached inference result."
      ),
      tags$li(
        "Click ",
        strong("Transfer Rules"),
        " to copy the currently displayed Auto RegEx candidate to the Content, Condition, and Ratio rule tabs. ",
        "You can also use Transfer Rules again to restore the Auto RegEx version after manually editing those tabs."
      ),
      tags$li(
        "Review or edit the transferred rules, then use ",
        strong("Apply Metadata Rules"),
        " when you want to update the current metadata."
      ),
      tags$li("Export the finished rule set from the Export tab.")
    ),

    div(
      class = "well",
      h5("Infer, adjust, transfer, apply, and export"),
      tags$ul(
        tags$li(
          strong("Infer: "),
          "builds and validates a new candidate. A successful fresh inference is transferred automatically."
        ),
        tags$li(
          strong("Adjust: "),
          "changing redundancy rebuilds the candidate without rerunning the expensive inference steps."
        ),
        tags$li(
          strong("Transfer: "),
          "copies the currently displayed candidate to the editable Auto-Assign rule tabs."
        ),
        tags$li(
          strong("Apply: "),
          "uses the rules currently stored in those tabs to update metadata."
        ),
        tags$li(
          strong("Export: "),
          "saves the finished rule set and any selected settings as an RDS template."
        )
      )
    ),

    h4("Redundancy and content refinement"),
    p(
      "A redundancy value of 0 uses the least redundant Content pattern that Auto RegEx can prove gives the same complete-table assignment. ",
      "Higher values restore additional safe structure from the inferred rule before it was minimized."
    ),
    p(
      "A higher redundancy value does not necessarily change the regex. ",
      "If no additional safe structure is available, the rule stays at its last effective level. ",
      "For example, if the inferred rule before minimization is ",
      code("^Imput"),
      ", the available levels may be ",
      code("Imput"),
      " and ",
      code("^Imput"),
      "; Auto RegEx does not reconstruct the rest of the original column header from that shortened rule."
    ),
    p(
      "The global redundancy value is the default for all Content rules. ",
      "After inference, individual Content rules can use their own redundancy value."
    ),

    h4("Avoiding condition and ratio values in Content rules"),
    p(
      "After condition and ratio extraction, Auto RegEx checks the Content rules again. ",
      "Confirmed condition, numerator, and denominator text is treated as dataset-specific information and is avoided when a more reusable Content pattern can be used."
    ),
    p(
      "When it is structurally safe, variable values can be replaced by flexible patterns. ",
      "For example, alphabetic values may be represented by ",
      code("[[:alpha:]]+"),
      " and mixed letter/number values by combinations such as ",
      code("[[:alpha:]]+[[:digit:]]+"),
      ". A replacement is kept only when the resulting rules still replay correctly."
    ),

    h4("Content types with one row"),
    p(
      "A Content type does not need several rows in every case. ",
      "When only one row is available, Auto RegEx can start from the exact column header as a safe fallback and then remove unnecessary parts only while the complete metadata assignment remains unchanged."
    ),
    p(
      "A Content type can also have more than one rule when the same Content appears in clearly different header structures. ",
      "This is expected for heterogeneous files and is why the rule tables show a separate Rule ID for every rule."
    ),

    h4("Condition and Sample inference"),
    p(
      strong("Options"),
      " stores the extracted experimental condition. ",
      strong("Sample"),
      " is created afterward from the condition plus the smallest useful source-derived identifiers and is made unique within each Content type. ",
      "For example, a Found-in-Sample column with condition ",
      code("C"),
      " might receive a Sample name such as ",
      code("S6_F6_C"),
      "."
    ),

    h4("Ratio inference"),
    p(
      "Ratio inference learns how to extract Numerator and Denominator from ratio-like column headers. ",
      "A rule is accepted only when the required reference values can be reproduced correctly."
    ),
    p(
      "Some files contain several header structures for the same ratio Content type. ",
      "Auto RegEx can separate these structures into independent Content rules so that an extraction rule is used only where it is valid."
    ),
    p(
      "Related ratio, p-value, and adjusted p-value columns are kept together as one comparison family when their header structure supports that relationship. ",
      "If one member contains a real Numerator/Denominator pair, that pair can be reused for its matching siblings. ",
      "If no real pair is available, MiraProt can use one shared internal fallback pair so the related columns can still be matched downstream."
    ),

    div(
      class = "panel panel-default",
      div(
        class = "panel-heading",
        h5(
          "Understanding the outcome",
          style = "margin: 0;"
        )
      ),
      div(
        class = "panel-body",
        tags$ul(
          tags$li(
            strong("Success: "),
            "valid rules were inferred and transferred."
          ),
          tags$li(
            strong("Success with warnings: "),
            "usable rules were transferred, but some Content types or extraction rules could not be inferred safely. Review the diagnostics and the Metadata table."
          ),
          tags$li(
            strong("Failure: "),
            "Auto RegEx could not safely transfer the candidate. The previous Auto-Assign rules are preserved whenever rollback succeeds."
          )
        ),
        p(
          style = "margin-bottom: 0;",
          "Warnings do not always mean that the metadata is wrong, but incorrect or incomplete metadata assignments are an important cause and should be checked first."
        )
      )
    ),

    h4("Troubleshooting Auto RegEx"),
    div(
      style = "overflow-x: auto;",
      tags$table(
        class = "table table-sm table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Problem"), tags$th("What to do"))),
        tags$tbody(
          tags$tr(tags$td("Current metadata not ready"), tags$td("Complete or synchronize the visible metadata, then review the readiness message again.")),
          tags$tr(tags$td("Missing Column"), tags$td("Add or map the required Column field containing the original headers.")),
          tags$tr(tags$td("Excel worksheet or mapping changed"), tags$td("Select the intended worksheet and review every field mapping before running inference again.")),
          tags$tr(tags$td("Unresolved content class"), tags$td("Review its examples and diagnostics. Other valid classes may still have transferred successfully.")),
          tags$tr(tags$td("Ratio inference unavailable"), tags$td("Provide rows with Column, Numerator, and Denominator values that can be replayed exactly.")),
          tags$tr(tags$td("Rules transferred but metadata unchanged"), tags$td("This is expected after inference. Review the rule tabs, then use Apply Metadata Rules when ready.")),
          tags$tr(tags$td("Metadata synchronization paused"), tags$td("Click Synchronize metadata now so downstream tools use the latest edits, then rerun or apply rules as needed."))
        )
      )
    ),

    h3("Exporting Auto-Assign Rules"),
    div(
      class = "well",
      p(
        "Export Rule Set downloads an .rds rule file containing the Content Assignment, ",
        "Condition Extraction, and Ratio Analysis rules defined in the Auto-Assign tabs."
      ),
      p(
        "To reuse the downloaded rule file in MiraProt, place it in ",
        strong("AutoAssign/"), " for a source or local installation, or in ",
        strong("shiny-app/AutoAssign/"), " for a portable installation."
      )
    ),

    h3("Content Rules"),
    p(
      "Content Rules decide which metadata Content type is assigned to a column. ",
      "More than one rule can use the same Content type when different column-name structures need separate matching rules."
    ),
    tags$ul(
      tags$li(
        strong("Pattern Input Mode: "),
        "converts plain text into escaped regular expressions. Disable it when you want to enter regex directly."
      ),
      tags$li(
        strong("Content Type: "),
        "the metadata Content value assigned by this rule."
      ),
      tags$li(
        strong("Rule ID: "),
        "the read-only unique identity of the selected rule. Two rules may have the same Content type but always have different Rule IDs."
      ),
      tags$li(
        strong("Include Pattern: "),
        "the pattern a column header must match."
      ),
      tags$li(
        strong("Exclude Pattern: "),
        "an optional pattern that prevents a matching column from using the rule."
      ),
      tags$li(
        strong("Transformation: "),
        "the transformation associated with numeric Content types."
      ),
      tags$li(
        strong("Modify Content Rule: "),
        "changes only the currently selected Rule ID. Its identity is preserved."
      ),
      tags$li(
        strong("Add New Content Rule: "),
        "creates a separate Content rule with a new Rule ID. Use this when you want another rule instead of changing the selected one."
      ),
      tags$li(
        strong("Remove Content Rule: "),
        "removes only the currently selected Rule ID."
      ),
      tags$li(
        strong("Rule table: "),
        "click a row to select that exact rule. The red remove button in a row also removes that exact Rule ID."
      )
    ),

    div(
      class = "alert alert-info",
      h5("Example Content Rules:"),
      div(
        style = "background-color: white; padding: 8px; border-radius: 4px;",
        tags$table(
          class = "table table-sm table-bordered table-striped",
          tags$thead(
            tags$tr(
              tags$th("Content"),
              tags$th("Include (regex)"),
              tags$th("Exclude (regex)"),
              tags$th("Transformation")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Abundance Ratio"),
              tags$td("^Abundance\\s+Ratio"),
              tags$td("Adj\\.|P\\-Value|Variability"),
              tags$td("")
            ),
            tags$tr(
              tags$td("#PSMs"),
              tags$td("#PSMs"),
              tags$td(""),
              tags$td("")
            ),
            tags$tr(
              tags$td("Identifier"),
              tags$td("Accession|Gene symbol|Gene Symbol|Entrez Gene ID|Ensembl Gene ID"),
              tags$td(""),
              tags$td("")
            )
          )
        )
      )
    ),
    p("Meaning of the example content rules:"),
    tags$ul(
      tags$li(strong("Abundance Ratio:"), " Headers that contain the string 'Abundance Ratio' but do not contain any of the strings 'Adj.', 'P-Value' or 'Variability'"),
      tags$li(strong("#PSMs:"), " Headers that contain the string '#PSMs'"),
      tags$li(strong("Identifier:"), " Headers that contain any of the strings 'Accession', 'Gene symbol', 'Gene Symbol', 'Entrez Gene ID' or 'Ensembl Gene ID'"),
      tags$li("None of these examples contain transformed values")
    ),

    p("General notes on regular expressions (RegEx):"),
    tags$ul(
      tags$li("'|' is read as 'OR'"),
      tags$li("'&' is read as 'AND'"),
      tags$li("RegEx are case sensitive"),
      tags$li("White spaces are part of the RegEx. The RegEx 'Abundance & Ratio' is interpreted different from 'Abundance&Ratio'"),
      tags$li("'Pattern Input Mode' translates plain text into regular expressions (preserves ^ and $, escapes '/' as \\/, and turns spaces into \\s+)"),
      tags$li("Deactivate 'Pattern Input Mode' for more control (experienced users only)")
    ),

    h3("Condition Extraction Rules"),
    p(
      "Condition Extraction Rules define how an experimental condition is extracted from a column header into ",
      strong("Options"),
      ". ",
      strong("Sample"),
      " is created later from the extracted condition and useful source-derived identifiers."
    ),
    p(
      "Each condition rule has its own Rule ID and is linked to the Content rule it belongs to. ",
      strong("Modify Condition Rule"),
      " changes the selected rule, while ",
      strong("Add New Condition Rule"),
      " creates another condition rule for the selected Content rule."
    ),
    p(
      "When Pattern Input Mode is enabled for a method that uses Before or After patterns, plain text is converted to regex. ",
      "Spaces become ",
      code("\\s+"),
      ", forward slashes are escaped, and ",
      code("^"),
      " / ",
      code("$"),
      " anchors are preserved."
    ),

    div(
      class = "alert alert-info",
      h5("Example Extraction Rules:"),
      div(
        style = "background-color: white; padding: 8px; border-radius: 4px;",
        tags$table(
          class = "table table-sm table-bordered table-striped",
          tags$thead(
            tags$tr(
              tags$th("Content"),
              tags$th("Method"),
              tags$th("Before"),
              tags$th("After"),
              tags$th("Separator"),
              tags$th("Pos"),
              tags$th("Example Header"),
              tags$th("Extracted Sample")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Found in Sample"),
              tags$td("end"),
              tags$td("\\s+"),
              tags$td(""),
              tags$td("\\s+"),
              tags$td("1"),
              tags$td("Abundance Sample01"),
              tags$td("Sample01")
            ),
            tags$tr(
              tags$td("Normalized Abundance"),
              tags$td("start"),
              tags$td(""),
              tags$td("\\s+"),
              tags$td("\\s+"),
              tags$td("1"),
              tags$td("Sample01 Norm_Abundance"),
              tags$td("Sample01")
            ),
            tags$tr(
              tags$td("Raw Abundance"),
              tags$td("between"),
              tags$td("x_"),
              tags$td("_y"),
              tags$td("_"),
              tags$td("1"),
              tags$td("Abundance_x_Sample01_y"),
              tags$td("Sample01")
            ),
            tags$tr(
              tags$td("Found in Sample"),
              tags$td("phrase_position"),
              tags$td(""),
              tags$td(""),
              tags$td("_"),
              tags$td("4"),
              tags$td("Exp_Set1_Condition_Sample01"),
              tags$td("Sample01")
            ),
            tags$tr(
              tags$td("Found in File"),
              tags$td("pattern_detect"),
              tags$td(""),
              tags$td(""),
              tags$td("_"),
              tags$td("1"),
              tags$td("Data_Sample01_Rep1"),
              tags$td("Sample01")
            )
          )
        )
      )
    ),

    p(
      "Each rule specifies how to extract the relevant condition from the column header using position or pattern logic. ",
      "The extracted condition fills ",
      strong("Options"),
      ". MiraProt then creates the corresponding ",
      strong("Sample"),
      " identifier during metadata assignment."
    ),

    h5("Method definitions:"),
    tags$ul(
      tags$li(strong("end:"), " Extracts the substring at the end of the header that follows the 'before' pattern."),
      tags$li(strong("start:"), " Extracts the substring at the beginning of the header that precedes the 'after' pattern."),
      tags$li(strong("between:"), " Extracts the substring between the 'before' and 'after' patterns."),
      tags$li(strong("phrase_position:"), " Splits the header by the separator and selects the substring at position ", code("Pos"), "."),
      tags$li(strong("pattern_detect:"), " Groups header of same ", code("Content"), ". Splits the header by the separator and extracts unique phrase occurrences.")
    ),

    p(
      "These methods can be combined or customized to match complex naming schemes. ",
      "After applying a rule, the extracted text appears as the sample identifier in the metadata table, ",
      "enabling automated downstream mapping and condition assignment."
    ),

    h3("Define Ratio Extraction Rules"),
    p(
      "Ratio Extraction Rules define how Numerator and Denominator are parsed from ratio-like column headers. ",
      "The extracted labels identify which comparison a ratio, p-value, or adjusted p-value belongs to."
    ),
    p(
      "Each ratio rule has its own Rule ID and belongs to a particular Content rule. ",
      strong("Modify Ratio Rule"),
      " changes the currently selected rule. ",
      strong("Add New Ratio Rule"),
      " creates an additional ratio rule for the selected Content rule, and ",
      strong("Remove Ratio Rule"),
      " removes only the selected Rule ID."
    ),

    div(
      class = "alert alert-info",
      h5("Example Ratio Rules:"),
      div(
        style = "background-color: white; padding: 8px; border-radius: 4px;",
        tags$table(
          class = "table table-sm table-bordered table-striped",
          tags$thead(
            tags$tr(
              tags$th("Content"),
              tags$th("Method"),
              tags$th("Separator(s)"),
              tags$th("Invert"),
              tags$th("NumBefore"),
              tags$th("NumAfter"),
              tags$th("DenBefore"),
              tags$th("DenAfter"),
              tags$th("NumPos"),
              tags$th("DenPos"),
              tags$th("Example Header"),
              tags$th("Parsed Numerator"),
              tags$th("Parsed Denominator")
            )
          ),
          tags$tbody(
            # 1) Regular Expression
            tags$tr(
              tags$td("Abundance Ratio"),
              tags$td("Regular Expression"),
              tags$td(""),
              tags$td("false"),
              tags$td("Ratio:\\s*\\("),
              tags$td("\\)\\s*/\\s*\\("),
              tags$td(""),
              tags$td("\\)$"),
              tags$td(""),
              tags$td(""),
              tags$td("Abundance Ratio: (Treatment) / (Control)"),
              tags$td("Treatment"),
              tags$td("Control")
            ),
            # 2) Pattern Recognition
            tags$tr(
              tags$td("Abundance Ratio Adj. p-Value"),
              tags$td("Pattern Recognition"),
              tags$td("\\(|\\)|\\/|\\s+|_"),
              tags$td("false"),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td("Abundance Ratio (KO) / (WT) Adj. p-Value"),
              tags$td("KO"),
              tags$td("WT")
            ),
            # 3) Position in String
            tags$tr(
              tags$td("Abundance Ratio p-Value"),
              tags$td("Position in String"),
              tags$td("_"),
              tags$td("false"),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td(""),
              tags$td("1"),
              tags$td("2"),
              tags$td("Treatment_Control_pValue"),
              tags$td("Treatment"),
              tags$td("Control")
            )
          )
        )
      )
    ),

    p(
      "If ", strong("Invert"), " is enabled, the detected numerator and denominator are swapped after extraction."
    ),

    h5("Method definitions:"),
    tags$ul(
      tags$li(
        strong("Regular Expression:"),
        " Extracts labels using regex anchors. ",
        em("NumBefore/NumAfter"), " wrap the numerator; ",
        em("DenBefore/DenAfter"), " wrap the denominator. "
      ),
      tags$li(
        strong("Pattern Recognition:"),
        " Tokenizes headers by ", em("Separator(s)"), " and picks likely group labels using ",
        em("case-sensitive"), " matching against known samples; if no samples are available, it falls back to parentheses. Extra matches beyond the first two are ignored."
      ),
      tags$li(
        strong("Position in String:"),
        " Splits the header by the given separator and selects tokens at positions ",
        em("NumPos"), " and ", em("DenPos"), " as numerator and denominator, respectively."
      )
    ),

    p(
      "After applying a rule, the parsed labels populate the ",
      strong("Numerator"), " and ", strong("Denominator"),
      " fields used by the ratio/statistics module."
    ),

    p(
      "MiraProt also keeps related ratio, p-value, and adjusted p-value columns paired. ",
      "When one member of a clearly matched family already has a real Numerator/Denominator pair, that pair can be propagated to the matching siblings. ",
      "If no real pair is available, one shared internal fallback pair can be used for the family instead of giving each column an unrelated fallback."
    ),

    h3("Applying Auto-Assign"),
    div(
      class = "well",
      h4("Steps to Use Auto-Assign:"),
      tags$ol(
        tags$li("Load a template in 'Conditions & Metadata Loader' panel or define custom rules in the Auto-Assign Assistant"),
        tags$li("Review the pattern recognition rules"),
        tags$li("Use Export Rule Set to download the three Auto-Assign rule sets as an .rds file"),
        tags$li("Place the file in AutoAssign/ for a source or local installation, or shiny-app/AutoAssign/ for a portable installation"),
        tags$li("The file will be available in the 'Conditions & Metadata Loader' panel upon restart of the app"),
        tags$li("Selecting this file will apply the pattern recognition rules on your data")
      )
    ),

    h3("Best Practices"),
    tags$ul(
      tags$li("Start with simple patterns and refine as needed"),
      tags$li("Save successful rule sets as .rds rule files"),
      tags$li("Use case-sensitive matching when appropriate")
    )
  )
}

render_filtering_content_dw <- function() {
  div(
    h2("Data Filtering"),
    hr(),

    h3("Overview"),
    p("The Data Filtering module provides multiple strategies to remove low-quality or unwanted data from your analysis."),

    h3("Filter Types"),

    h4("1. Confidence Filtering"),
    p("Filter proteins based on identification confidence:"),
    tags$ul(
      tags$li(strong("Metadata dependent:"), " Requires one data column defined as ", em("Protein Confidence"), " in your metadata table."),
      tags$li(strong("Numeric Confidence Filtering:"), " Remove proteins above/below numeric confidence threshold"),
      tags$li(strong("String Confidence Filtering:"), " Exclude specific confidence labels")
    ),

    h4("2. Valid Value Filtering"),
    p("Remove proteins with excessive missing values:"),
    tags$ul(
      tags$li(strong("In total:"), " Minimum valid values across all samples"),
      tags$li(strong("One group:"), " Minimum valid values in at least one condition group"),
      tags$li(strong("Each group:"), " Minimum valid values in every condition group")
    ),

    div(
      class = "alert alert-info",
      h5("Example Scenarios:"),
      tags$ul(
        tags$li("'In total' with 3: Keep proteins with ≥3 valid measurements overall"),
        tags$li("'One group' with 2: Keep if any condition has ≥2 valid replicates"),
        tags$li("'Each group' with 2: Keep only if all conditions have ≥2 valid replicates")
      )
    ),

    h4("3. Custom Column Filtering"),
    p("Create complex filters on any column:"),
    tags$ul(
      tags$li("Select content category"),
      tags$li("Select target columns"),
      tags$li("Define comparison operators"),
      tags$li("Numeric: Available operators: >, ≥, <, ≤, == (equal to), != (not equal to)"),
      tags$li("Character: Available operators: 'Contains', 'Equals', 'Starts with', 'Ends with', 'Does not contain'"),
      tags$li("Select multiple columns to choose 'Column Combination Logic': ANY, ALL, SOME"),
      tags$li("Set threshold values"),
      tags$li("Combine multiple conditions with AND/OR logic (optional)"),
      tags$li("If second threshold value is empty, AND/OR logic is ignored"),
      tags$li("Define missing value handling of filtered columns"),
      tags$li("Add custom filter to the filter queue table"),
      tags$li(strong("Apply All Filters"))
    )
  )
}

render_editing_content <- function() {
  div(
    h2("Data Editing"),
    hr(),

    # Intro / purpose
    div(
      class = "alert alert-info",
      h4("What does the Edit module do?"),
      p(
        "The Edit module lets you update the content of selected columns. ",
        "You choose a content category, pick one or more columns from that category, and then build either ",
        strong("Replace"), " or ", strong("Edit"), " operations. Each operation goes into a queue and can be applied to the data."
      )
    ),

    h3("Workflow at a glance"),
    tags$ol(
      tags$li("Pick a ", strong("Content Category"), " — it filters the available columns."),
      tags$li("Select one or more ", strong("Columns"), " within that category."),
      tags$li("Open the ", strong("Replace"), " or ", strong("Edit"), " tab to configure the operation."),
      tags$li("Click ", strong("Add to Queue"), " to queue it."),
      tags$li("Review queued operations in the ", em("Queued Operations"), " table."),
      tags$li("Click ", strong("Apply Queue"), " to execute all operations, or ", strong("Clear Queue"), " to remove them.")
    ),

    hr(),
    h3("Column selection"),

    tags$ul(
      tags$li(strong("Content Category:"), " A dropdown filled from metadata. Selecting a category limits the next dropdown to columns mapped to that category."),
      tags$li(strong("Columns:"), " Multi-select of column headers (only those present in the current dataset).")
    ),
    div(
      class = "alert", style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      HTML(
        paste0(
          "<strong>Type compatibility:</strong> If you select multiple columns, they must share a compatible data type (numeric or character). ",
          "The module checks this and will block adding an operation when mixed types would break the chosen action."
        )
      )
    ),

    hr(),
    h3("Replace"),

    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Character columns — find & replace")),
      div(
        class = "panel-body",
        p("Use this when your selected columns are character/factor."),
        tags$ul(
          tags$li(strong("Search Type:"), " choose one of ", code("is equal"), ", ", code("starts with"), ", ",
                  code("ends with"), ", ", code("contains"), "."),
          tags$li(strong("Search Term:"), " the string to match."),
          tags$li(strong("Replacement Type:"), " choose ", code("Replace cell"), ", ", code("Replace substring"), ", or ", code("Clear cell"), "."),
          tags$li(strong("Replace With:"), " the text that will replace matched values. This input is not needed for ", code("Clear cell"), ".")
        )
      )
    ),

    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Numeric columns — conditional replace")),
      div(
        class = "panel-body",
        p("Use this when your selected columns are numeric."),
        tags$ul(
          tags$li(strong("Operator:"), " choose one of ", code("<"), ", ", code("<="), ", ", code("=="), ", ",
                  code("!="), ", ", code(">="), ", ", code(">"), "."),
          tags$li(strong("Threshold:"), " numeric value to compare against."),
          tags$li(strong("Replace with:"), " either ", code("NA"), " or ", code("Numeric"), ". ",
                  "If you choose ", code("Numeric"), ", provide the ", strong("Replacement Value"), ".")
        )
      )
    ),

    hr(),
    h3("Edit"),

    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Character columns — add text before/after")),
      div(
        class = "panel-body",
        p("Use this when your selected columns are character/factor."),
        tags$ul(
          tags$li(strong("Text to Add:"), " the text that will be inserted."),
          tags$li(strong("Position:"), " ", code("Before"), " or ", code("After"), " (relative to current values).")
        )
      )
    ),

    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Numeric columns — mathematical operations")),
      div(
        class = "panel-body",
        p("Use this when your selected columns are numeric."),
        tags$ul(
          tags$li(strong("Mathematical Operation:"), " choose one of ",
                  code("Add"), ", ", code("Subtract"), ", ", code("Multiply"), ", ", code("Divide"), ", ",
                  code("log"), ", ", code("-log"), ", ", code("raise to the power of"), "."),
          tags$li(HTML(paste0(
            strong("Operation Value:"), " required for ",
            code("Add/Subtract/Multiply/Divide"), "."
          ))),
          tags$li(HTML(paste0(
            strong("Exponent:"), " required when choosing ", code("raise to the power of"), "."
          ))),
          tags$li(HTML(paste0(
            strong("Logarithm Base:"), " required for ", code("log"), " or ", code("-log"), "."
          )))
        )
      )
    ),

    hr(),
    h3("Queued operations"),

    div(
      class = "alert alert-info",
      "Click ", strong("Add to Queue"), " to queue the operation."
    ),

    tags$ul(
      tags$li(strong("Queued Operations table:"), " shows the operations you added. ",
              "Each row includes operation type, affected columns, parameters, description, and execution status."),
      tags$li(strong("Apply Queue:"), " executes all not-yet-executed operations in order."),
      tags$li(strong("Clear Queue:"), " removes all operations from the queue.")
    )
  )
}


render_imputation_content <- function() {
  div(
    h2("Missing Data Imputation"),
    hr(),

    div(
      class = "alert alert-info",
      h4("What this module does"),
      p(
        "Fill missing numeric values for a chosen metadata ", code("Content"), " group (\"Data Types to Impute\"). ",
        "You select a method, pick the group, and run the imputation."
      )
    ),

    h3("Workflow"),
    tags$ol(
      tags$li("Pick an ", strong("Imputation Method"), ": ", code("Left-censored"), ", ", code("Random forest"), ", ", code("MICE - CART"), "."),
      tags$li("Choose the ", strong("Data Type to Impute"), " (a metadata ", code("Content"),
              " with numeric columns that currently have missing values)."),
      tags$li("Click ", strong("Apply Imputation"), ".")
    ),

    hr(),
    h3("User interface"),
    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Controls")),
      div(
        class = "panel-body",
        tags$ul(
          tags$li(strong("Imputation Method:"), " ", code("Left-censored"), " | ", code("Random forest"), " | ", code("MICE - CART"), "."),
          tags$li(strong("Data Types to Impute:"), " dropdown of metadata ", code("Content"),
                  " groups with numeric missing values."),
          tags$li(strong("Apply Imputation:"), " runs the selected method on the selected group.")
        )
      )
    ),

    hr(),
    h3("Methods explained"),

    # Left-censored
    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Left-censored")),
      div(
        class = "panel-body",
        tags$ul(
          tags$li(strong("Scope:"), " Column-wise imputation within the selected group."),
          tags$li(strong("Assumption:"), " Missing values and zeros are signals below detection (left-censored)."),
          tags$li(strong("Model:"), " Fits a log-normal distribution to the positive observed values using left-censoring at the smallest observed value (lower limit of quantification - LLOQ)."),
          tags$li(strong("Imputation:"), " Calculates the 5th percentile (q05) of observed values and samples only from the model’s lower tail up to q05. Imputed values are restricted to the interval between LLOQ and q05."),
          tags$li(strong("Checks:"), " Negative values stop with an error; columns with fewer than 2 observed values are left unchanged; zeros are set to missing before fitting.")
        )
      )
    ),

    # Random forest
    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Random forest")),
      div(
        class = "panel-body",
        tags$ul(
          tags$li(strong("Scope:"),
                  "Imputes the numeric columns within the selected group; character columns remain unchanged and are reattached afterwards."),
          tags$li(strong("Model:"),
                  "Builds random-forest models to predict each variable with missing values using the remaining numeric variables."),
          tags$li(strong("Imputation:"),
                  "Performs iterative updates so variables can better predict one another until changes are minimal."),
          tags$li(strong("Checks:"),
                  "If no numeric columns are present, or if imputation fails, the original data are returned."),
          tags$li(HTML("<em>Reference:</em> MissForest; Stekhoven &amp; Bühlmann (2012), <em>Bioinformatics</em>."))
        )
      )
    ),

    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("MICE — CART")),
      div(
        class = "panel-body",
        tags$ul(
          tags$li(strong("Scope:"),
                  "Imputes the numeric columns within the selected group; character columns remain unchanged and are reattached afterwards."),
          tags$li(strong("Model:"),
                  "Uses CART (Classification and Regression Trees) to estimate missing values from the other numeric variables."),
          tags$li(strong("Imputation:"),
                  "Updates predictions iteratively so variables can better inform each other as values are filled."),
          tags$li(strong("Checks:"),
                  "If no numeric columns are present, or if imputation fails, the original data are returned."),
          tags$li(HTML("<em>Reference:</em> mice package; van Buuren &amp; Groothuis-Oudshoorn (2011), <em>J. Stat. Softw.</em>."))
        )
      )
    ),

    hr(),
    h3("How to choose a method"),
    tags$ul(
      tags$li(
        strong("Left-censored:"),
        " Choose when missing (or zero) values are most likely due to low signal intensities falling below the detection limit.",
        " Example: a low-abundant peptide drops below the instrument’s detection sensitivity in some samples."
      ),
      tags$li(
        strong("Random forest:"),
        " Choose when proteins show correlated patterns across samples so other variables can reliably predict the missing ones.",
        " Example: proteins in the same pathway or complex rise and fall together and help estimate each other’s values."
      ),
      tags$li(
        strong("MICE — CART:"),
        " Choose when missingness relates to sample-specific structure or grouping, and multiple variables jointly provide information.",
        " Example: treatment groups or batches differ in detection success, and other protein intensities help recover these gaps."
      )
    ),

    div(
      class = "alert", style = "background-color: #3498db; border-color: #3498db; color: #fff;",
      HTML(
        "<strong>What does “Missing At Random (MAR)” mean?</strong> ",
        "Whether something is missing can be explained by other columns in the same row (for example, sample properties). ",
        "Put simply: after you consider the other information you recorded, the missingness is no longer mysterious."
      )
    ),

    hr(),
    h3("Notes"),
    tags$ul(
      tags$li("Only numeric columns in the selected ", code("Content"), " are imputed."),
      tags$li("If the selector is empty, there are no numeric missing values in that group right now.")
    )
  )
}


render_ratios_content <- function() {
  tagList(
    h2("Custom Ratios & Statistics"),
    tags$hr(),

    div(class = "alert alert-info",
        strong("What this adds"),
        p("Your dataset may already contain ratio results. This module adds ",
          em("custom"),
          " ratios computed here — including per-row statistics and multiple-testing correction — and writes them back to your data.")
    ),

    h3("What you select & why it matters"),
    tags$ul(
      tags$li(
        p(strong("Abundance data"),
          " — Pick the abundance layer to analyse (Raw, Normalized, Batch Corrected, or Imputed). ",
          "Only abundance types that exist and are defined in your metadata are shown; existing ratio/p-value content is excluded.")
      ),
      tags$li(
        p(strong("Numerator samples / Denominator samples"),
          " — Select individual samples for each side. The selected samples are treated as two groups (numerator vs. denominator) for the ratio and statistics. ",
          "Choices come from your metadata for the chosen abundance layer (uses “Sample” if available, otherwise “Options”).")
      ),
      tags$li(
        p(strong("Minimum valid values"),
          " — Excludes rows with too many imputed/invalid values ",
          em("within the selected samples only"),
          ". Decide where valid values must be present: ",
          em("In total"),
          " (across all selected samples), ",
          em("One group"),
          " (either numerator or denominator), or ",
          em("Each group"),
          " (both sides). ",
          "For imputed layers the check uses the corresponding ",
          em("original"),
          " values when available."
        )
      ),
      tags$li(
        p(strong("Statistical method"),
          " — Choose the per-row test: Student’s t-test, Welch’s t-test, Moderated Welch Test, Limma, DEqMS, or Mann–Whitney U.")
      ),
      tags$li(
        p(strong("p-value adjustment"),
          " — Multiple-testing correction (e.g., FDR/Benjamini–Hochberg, Bonferroni, Holm, Hochberg, Hommel, Benjamini–Yekutieli).")
      ),
      tags$li(
        p(strong("Comparison name"),
          " — Must be unique; it becomes the prefix of the output columns.")
      ),
      tags$li(
        p(strong("Queue & actions"),
          " — ",
          em("Add Ratio"),
          " adds the configuration to the queue (the queue section appears only when it contains items). ",
          em("Apply Queue"),
          " runs all queued items and appends results to your dataset; ",
          em("Clear Queue"),
          " removes all items.")
      )
    ),

    h3("How rows are screened before testing"),
    tags$ul(
      tags$li("Rows must pass your “Minimum valid values” rule ",
              em("within the selected numerator/denominator samples"),
              "."),
      tags$li("Additionally, each statistical test requires a minimum number of samples ",
              em("per group"),
              ":",
              tags$ul(
                tags$li(strong("At least 2 samples per group"),
                        ": Moderated Welch Test, Limma, DEqMS (also requires PSM data)"),
                tags$li(strong("At least 3 samples per group"),
                        ": Student’s t-Test, Welch’s t-Test, Mann–Whitney U-Test")
              )
      ),
      tags$li("If your metadata specifies a transformation for these columns, data may be re-transformed before testing.")
    ),

    h3("Example scenarios (validity rules)"),
    div(class = "well",
        tags$ul(
          tags$li(
            tags$b("In total"),
            ": You select 2 numerator and 2 denominator samples. ",
            em("Minimum valid values = 3"),
            " → at least 3 non-missing original values across those 4 selected samples, ",
            "and the chosen method’s per-group minimum (e.g., Limma ≥ 2 per group)."
          ),
          tags$li(
            tags$b("One group"),
            ": With the same selection, ",
            em("Minimum valid values = 2"),
            " → at least one side has ≥ 2 non-missing original values, plus the method’s per-group minimum."
          ),
          tags$li(
            tags$b("Each group"),
            ": With 2+2 samples, ",
            em("Minimum valid values = 2"),
            " → both numerator ",
            em("and"),
            " denominator have ≥ 2 non-missing original values, and also meet the method’s per-group minimum."
          )
        ),
        p(em("Note:"),
          " For imputed layers the validity check falls back to the corresponding non-imputed values when available.")
    ),

    h3("What gets written"),
    p("Each comparison creates:"),
    tags$ul(
      tags$li("“<ComparisonName>_Abundance Ratio”"),
      tags$li("“<ComparisonName>_Abundance Ratio p-Value”"),
      tags$li("“<ComparisonName>_Abundance Ratio Adj. p-Value”")
    ),
    p("Results are merged back by the original row index."),

    h3("Method notes"),
    tags$ul(
      tags$li("Limma requires the “limma” package."),
      tags$li("DEqMS requires the “DEqMS” package ",
              em("and"),
              " PSM information (e.g., PSM/peptide/spectrum counts) in your metadata; otherwise DEqMS will not run.")
    ),

    h3("Tips"),
    tags$ul(
      tags$li("Keep numerator and denominator sample sets disjoint."),
      tags$li("Use short, descriptive names; they become column prefixes."),
      tags$li("Queue multiple comparisons and apply them in one go.")
    ),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(em("Name already exists:"),
              " it will be made unique automatically and you’ll be notified."),
      tags$li(em("No samples to choose:"),
              " select an abundance layer that exists in your metadata."),
      tags$li(em("Few rows analysed:"),
              " lower the minimum valid values or relax where valid values must be present; also check the per-group minima for your chosen test."),
      tags$li(em("DEqMS won’t start:"),
              " ensure PSM data exists and both “DEqMS” and “limma” are available.")
    )
  )
}

render_batch_content <- function() {
  tagList(
    h2("Batch Effects"),
    tags$hr(),

    div(class = "alert alert-info",
        strong("What this does"),
        p("Removes technical differences between batches and appends new “Batch Corrected …” columns to your data. ",
          "When you run it again, previously created batch-corrected columns are removed and replaced.")
    ),

    h3("Interface"),
    tags$ul(
      tags$li(
        p(strong("Choose Method for Batch Correction"),
          " — ",
          "ComBat, Limma, Offset Correction, LOESS, Quantile.")
      ),
      tags$li(
        p(strong("Imputation Method"),
          " — None, Left censored, Random Forest, MICE CART. ",
          "Without imputation, only rows with complete data (in the selected columns) are corrected; with imputation, all rows are processed.")
      ),
      tags$li(
        p(strong("Data Transformation"),
          " — None, log2, log10, −log10. ",
          "Data are prepared on your chosen scale; correction runs on log2 internally; results are returned on your chosen scale.")
      ),
      tags$li(
        p(strong("Remove imputed values after correction"),
          " — If imputation was used, restore the original missing-value pattern (set imputed cells back to NA).")
      ),
      tags$li(
        p(strong("Batch Groups"),
          " — For each batch group, select the numeric columns that belong together. ",
          "Use “Add Batch Group” / “Remove Batch Group” to manage groups. ")
      ),
      tags$li(
        p(strong("Run"),
          " — Click ",
          em("Correct Batch Effects"),
          " to apply the selected method to the defined batch groups.")
      )
    ),

    h3("How it works"),
    tags$ul(
      tags$li("All numeric (or convertible-to-numeric) columns are available to select; “Row Index” is excluded."),
      tags$li("You must define at least two non-empty batch groups; columns must not overlap between groups."),
      tags$li("If non-positive values occur, a small offset is added automatically before the log2 step."),
      tags$li("No imputation: only complete rows (within the selected columns) are corrected; other rows stay unchanged."),
      tags$li("With imputation: missing values are imputed, all rows are corrected, and—if chosen—imputed cells are set back to NA afterwards."),
      tags$li("New columns are added with names ",
              em("“Batch Corrected <OriginalColumn>”"),
              "; any existing “Batch Corrected …” columns are removed first.")
    ),

    h3("Methods (what the module applies)"),
    p(em("If in doubt, start with ComBat.")),
    tags$ul(
      tags$li(
        p(strong("ComBat"),
          " — Runs ", em("sva::ComBat"), " on log2-transformed values using your batch groups as the batch factor (no additional covariates). ",
          "Per batch, ComBat estimates and removes batch-specific mean and variance effects; adjusted values are returned on your chosen scale. ",
          em("Requirement: at least 3 columns per batch."))
      ),
      tags$li(
        p(strong("Limma"),
          " — Calls ", em("limma::removeBatchEffect"), " with your batch groups as the batch factor. ",
          "Removes an additive batch component per feature on log2. ",
          em("Requirement: at least 2 columns per batch."))
      ),
      tags$li(
        p(strong("Offset Correction"),
          " — Computes one constant offset per batch (on log2) and adds/subtracts it so batch centers align. ",
          em("Requirement: at least 2 columns per batch."))
      ),
      tags$li(
        p(strong("LOESS"),
          " — Within each batch, fits a smooth curve (LOESS) of each column against the within-batch median reference and subtracts the fitted trend (non-linear drift correction, on log2). ",
          em("Requirement: at least 3 columns per batch."))
      ),
      tags$li(
        p(strong("Quantile"),
          " — Within each batch, applies ", em("limma::normalizeQuantiles"),
          " so all selected columns share the same empirical distribution (on log2). ",
          em("Requirement: at least 2 columns per batch."))
      )
    ),

    div(class = "alert alert-info",
        strong("Which method when?"),
        tags$ul(
          tags$li(
            strong("General choice / mixed issues across many batches:"),
            " ComBat — robust when batches differ in both overall level and variability (e.g., multiple instruments/projects)."
          ),
          tags$li(
            strong("Different overall signal levels between batches:"),
            " Offset Correction or Limma — useful when one batch is systematically higher/lower (e.g., day-to-day instrument sensitivity)."
          ),
          tags$li(
            strong("Intensity drift inside a batch:"),
            " LOESS — useful when values gradually rise/fall over a run (e.g., LC-MS drift from temperature, spray stability, column aging)."
          ),
          tags$li(
            strong("Different distribution shapes inside batches:"),
            " Quantile — useful when one batch looks more compressed or more spread (e.g., detector/dynamic range settings)."
          )
        )
    ),

    h3("Tips"),
    tags$ul(
      tags$li("Keep batch groups disjoint and aligned with your processing setup."),
      tags$li("If preserving missingness matters downstream, enable “Remove imputed values after correction”.")
    ),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(em("No numeric columns to select:"), " ensure measurement columns are numeric or convertible."),
      tags$li(em("Only one or empty groups:"), " assign columns to at least two batch groups."),
      tags$li(em("NAs after correction:"), " expected if you opted to restore imputed values to NA.")
    )
  )
}

render_pivot_content <- function() {
  tagList(
    h2("Pivot Operations"),
    tags$hr(),

    div(class = "alert alert-info",
        strong("What this does"),
        p("Transforms your table between long and wide formats. ",
          "You can pivot either the Primary Data or the Secondary Data. ",
          "The selected dataset is replaced by the pivoted result.")
    ),

    h3("Interface"),
    tags$ul(
      tags$li(
        p(strong("Select Data to Pivot"),
          " — Choose ",
          em("Primary Data"),
          " or ",
          em("Secondary Data"),
          ". The operation updates that dataset.")
      ),
      tags$li(
        p(strong("Pivot Operation"),
          " — ",
          em("Wider"),
          " (long → wide), ",
          em("Longer"),
          " (wide → long), or ",
          em("Transpose"),
          " (swap rows and columns).")
      ),
      tags$li(
        strong("Wider options"),
        tags$ul(
          tags$li(p(strong(code("Names from")), " — Column whose values become the ", em("new column headers"), " (one per unique value).")),
          tags$li(p(strong(code("Values from")), " — Column that provides the ", em("cell values"), " for those new headers.")),
          tags$li(p(strong(code("ID columns (optional)")), " — Identifier columns that stay as keys. Leave empty to auto-use all remaining columns."))
        )
      ),
      tags$li(
        strong("Longer options"),
        tags$ul(
          tags$li(p(strong(code("Columns to pivot")), " — One or more columns to unpivot (stack).")),
          tags$li(p(strong(code("Names to")), " — New column name that will hold the ", em("original column names"), ".")),
          tags$li(p(strong(code("Values to")), " — New column name that will hold the ", em("values"), "."))
        )
      ),
      tags$li(
        strong("Transpose options"),
        tags$ul(
          tags$li(p("No additional parameters are required. The operation reads the data as-is."))
        )
      ),
      tags$li(
        p(strong("Run"),
          " — Click ",
          em("Apply Pivot"),
          " to execute the transformation.")
      )
    ),

    h3("How it works"),
    tags$ul(
      tags$li("The app reads your chosen dataset (Primary or Secondary)."),
      tags$li(
        em("Wider"),
        ": New columns are created from unique values in ",
        code("Names from"),
        "; cells are filled from ",
        code("Values from"),
        ". Identifier columns remain unchanged. If ",
        code("ID columns"),
        " is empty, all non-pivot columns are used as identifiers."
      ),
      tags$li(
        em("Longer"),
        ": The selected ",
        code("Columns to pivot"),
        " are stacked into two columns — ",
        code("Names to"),
        " (original column names) and ",
        code("Values to"),
        " (values)."
      ),
      tags$li(
        em("Transpose"),
        ": The ",
        code("Row Index"),
        " helper column is removed before transposing and rebuilt afterwards as a fresh sequence (1, 2, 3, ...). ",
        "If your first data column contains labels (for example ",
        code("Sample"),
        " with values like ",
        code("Messung 1"),
        "), those labels are preserved in the transposed header layout."
      ),
      tags$li("The resulting table replaces the selected dataset."),
      tags$li("Performance safeguard: confirmation appears if ",
              em("Wider"),
              " would create more than ",
              strong("1,000 columns"),
              " or ",
              em("Longer"),
              " would create more than ",
              strong("500,000 rows"),
              " (Cancel or Continue Anyway).")
    ),

    h3("Methods / Options explained"),
    tags$ul(
      tags$li(
        p(strong("Wider"),
          " — Exactly one ",
          code("Names from"),
          " and one ",
          code("Values from"),
          " are required. If you specify ",
          code("ID columns"),
          ", they must exist; otherwise they are auto-derived as all remaining columns.")
      ),
      tags$li(
        p(strong("Longer"),
          " — Requires at least one ",
          code("Columns to pivot"),
          ". Provide new names for ",
          code("Names to"),
          " and ",
          code("Values to"),
          ". If ",
          code("Values to"),
          " already exists in the data and is not among the pivoted columns, choose a different name or include that column in ",
          code("Columns to pivot"),
          ".")
      ),
      tags$li(
        p(strong("Transpose"),
          " — No configuration required. The operation automatically locates the ",
          code("Row Index"),
          " helper column (if present), removes it for transposition, and then creates a new sequential ",
          code("Row Index"),
          " in the result. For matrix-like input tables, the first semantic label column (e.g. ",
          code("Sample"),
          ") remains readable after transposition (e.g. columns such as ",
          code("Sample"),
          ", ",
          code("Messung 1"),
          ", ",
          code("Messung 2"),
          "). If the data contains a mix of numeric and text columns (excluding ",
          code("Row Index"),
          "), all values in the result will be converted to text. A warning notification is shown when this happens.")
      )
    ),

    h3("Examples / Scenarios"),
    div(
      class = "dw-doc-pivot-scenarios",
      tags$style(HTML("
        .dw-doc-pivot-scenarios .dw-scenario-card {
          border: 1px solid #d7e3f4; border-radius: 8px; padding: 12px;
          margin-bottom: 14px; background: #fcfdff;
        }
        .dw-doc-pivot-scenarios .dw-scenario-title {
          display: block; margin-bottom: 6px; color: #1f4e8c;
          font-size: 16px; font-weight: 700;
        }
        .dw-doc-pivot-scenarios .dw-scenario-sub {
          margin-bottom: 8px; color: #2f3b4a; font-size: 13px;
        }
        .dw-doc-pivot-scenarios .table-responsive { width: 100%; margin: 0; }
        .dw-doc-pivot-scenarios .dw-scenario-table { width: 100%; }
        .dw-doc-pivot-scenarios .dw-scenario-table th {
          background: #eaf2fd; color: #1f4e8c; font-weight: 700; text-align: center;
        }
        .dw-doc-pivot-scenarios .dw-scenario-table td,
        .dw-doc-pivot-scenarios .dw-scenario-table th {
          border: 1px solid #c9d9ee !important; padding: 5px 8px !important; font-size: 12px;
        }
        .dw-doc-pivot-scenarios .dw-scenario-table tbody tr:nth-child(odd) { background: #f3f8ff; }
        .dw-doc-pivot-scenarios .dw-scenario-table tbody tr:nth-child(even) { background: #e8f2ff; }
        .dw-doc-pivot-scenarios .dw-scenario-bad,
        .dw-doc-pivot-scenarios .dw-scenario-good {
          display: inline-block; border-radius: 12px; padding: 2px 8px;
          margin-bottom: 4px; font-size: 11px; font-weight: 700;
        }
        .dw-doc-pivot-scenarios .dw-scenario-bad {
          border: 1px solid #f1b7b7; background: #ffe9e9; color: #9d1f1f;
        }
        .dw-doc-pivot-scenarios .dw-scenario-good {
          border: 1px solid #b4e1c2; background: #e8f8ee; color: #196b35;
        }
        .dw-doc-pivot-scenarios .row {
          display: flex; align-items: center; justify-content: center;
        }
        .dw-doc-pivot-scenarios .row > .col-sm-5 { flex: 1 1 0; width: auto; }
        .dw-doc-pivot-scenarios .row > .col-sm-2 { flex: 0 0 16.66666667%; width: 16.66666667%; }
        .dw-doc-pivot-scenarios .dw-scenario-flow {
          margin: 6px 0; color: #1f4e8c; font-size: 22px;
          font-weight: 700; text-align: center;
        }
        @media (max-width: 767px) {
          .dw-doc-pivot-scenarios .row { flex-direction: column; }
          .dw-doc-pivot-scenarios .row > [class*='col-sm-'] { flex: 0 0 auto; width: 100%; }
          .dw-doc-pivot-scenarios .dw-scenario-flow { transform: rotate(90deg); }
        }
      ")),
    # Long -> Wide (Wider) example
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Long → Wide (Wider)"),
        p(class = "dw-scenario-sub",
          code("Names from: Condition"),
          " · ",
          code("Values from: Intensity"),
          " · ",
          code("ID columns: ProteinID (and other identifiers)")
        ),
        fluidRow(
          column(
            width = 5,
            span(class = "dw-scenario-bad", "Before"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("ProteinID"),
                                    tags$th("Condition"),
                                    tags$th("Intensity")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("P001"), tags$td("A"), tags$td("10.5")),
                                  tags$tr(tags$td("2"), tags$td("P001"), tags$td("B"), tags$td("12.1")),
                                  tags$tr(tags$td("3"), tags$td("P002"), tags$td("A"), tags$td(" 8.9")),
                                  tags$tr(tags$td("4"), tags$td("P002"), tags$td("B"), tags$td("11.0"))
                                )
                     )
            )
          ),
          column(
            width = 2,
            tags$div(class = "dw-scenario-flow", HTML("&rarr;"))
          ),
          column(
            width = 5,
            span(class = "dw-scenario-good", "After"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("ProteinID"),
                                    tags$th("A"),
                                    tags$th("B")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("P001"), tags$td("10.5"), tags$td("12.1")),
                                  tags$tr(tags$td("2"), tags$td("P002"), tags$td(" 8.9"), tags$td("11.0"))
                                )
                     )
            )
          )
        )
    ),
    br(),

    # Wide -> Long (Longer) example
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Wide → Long (Longer)"),
        p(class = "dw-scenario-sub",
          code("Columns to pivot: Sample_A, Sample_B"),
          " · ",
          code("Names to: Sample"),
          " · ",
          code("Values to: Intensity")
        ),
        fluidRow(
          column(
            width = 5,
            span(class = "dw-scenario-bad", "Before"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("ProteinID"),
                                    tags$th("Sample_A"),
                                    tags$th("Sample_B")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("P001"), tags$td("10.5"), tags$td("12.1")),
                                  tags$tr(tags$td("2"), tags$td("P002"), tags$td(" 8.9"), tags$td("11.0"))
                                )
                     )
            )
          ),
          column(
            width = 2,
            tags$div(class = "dw-scenario-flow", HTML("&rarr;"))
          ),
          column(
            width = 5,
            span(class = "dw-scenario-good", "After"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("ProteinID"),
                                    tags$th("Sample"),
                                    tags$th("Intensity")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("P001"), tags$td("Sample_A"), tags$td("10.5")),
                                  tags$tr(tags$td("2"), tags$td("P001"), tags$td("Sample_B"), tags$td("12.1")),
                                  tags$tr(tags$td("3"), tags$td("P002"), tags$td("Sample_A"), tags$td(" 8.9")),
                                  tags$tr(tags$td("4"), tags$td("P002"), tags$td("Sample_B"), tags$td("11.0"))
                                )
                     )
            )
          )
        )
    ),
    br(),

    # Transpose example
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Transpose"),
        p(class = "dw-scenario-sub", "No configuration required. The ", code("Row Index"), " helper column is rebuilt automatically."),
        fluidRow(
          column(
            width = 5,
            span(class = "dw-scenario-bad", "Before"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("Protein"),
                                    tags$th("Sample_A"),
                                    tags$th("Sample_B")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("P001"), tags$td("10.5"), tags$td("12.1")),
                                  tags$tr(tags$td("2"), tags$td("P002"), tags$td("8.9"), tags$td("11.0"))
                                )
                     )
            )
          ),
          column(
            width = 2,
            tags$div(class = "dw-scenario-flow", HTML("&rarr;"))
          ),
          column(
            width = 5,
            span(class = "dw-scenario-good", "After"),
            tags$div(class = "table-responsive",
                     tags$table(class="table table-condensed dw-scenario-table",
                                tags$thead(
                                  tags$tr(
                                    tags$th("Row Index"),
                                    tags$th("Protein"),
                                    tags$th("P001"),
                                    tags$th("P002")
                                  )
                                ),
                                tags$tbody(
                                  tags$tr(tags$td("1"), tags$td("Sample_A"), tags$td("10.5"), tags$td("8.9")),
                                  tags$tr(tags$td("2"), tags$td("Sample_B"), tags$td("12.1"), tags$td("11.0"))
                                )
                     )
            )
          )
        )
    )

    ),

    h3("Tips"),
    tags$ul(
      tags$li("Leaving ",
              code("ID columns"),
              " empty (Wider) is fine — all non-pivot columns are used as identifiers."),
      tags$li("If a Wider result would create many columns, reduce distinct values in ",
              code("Names from"),
              " or filter first."),
      tags$li("For a very large Longer result, pivot fewer columns or filter rows first."),
      tags$li("Transpose is best applied to pure measurement matrices. Make sure the ",
              "first semantic label column (often ",
              code("Sample"),
              ") and its values are clean and unique enough for your workflow.")
    ),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(em("No data available:"),
              " ensure the selected dataset is loaded."),
      tags$li(em("Package required:"),
              " the ",
              code("tidyr"),
              " package must be available (Wider and Longer only)."),
      tags$li(em("Wider — No valid ID columns:"),
              " do not reuse the same field for both ",
              code("Names from"),
              " and ",
              code("Values from"),
              "; include at least one identifier column (or leave ",
              code("ID columns"),
              " empty to auto-use the remaining columns)."),
      tags$li(em("Longer — Column name conflict:"),
              " if ",
              code("Values to"),
              " already exists and is not among the pivoted columns, choose a different name or include that column in ",
              code("Columns to pivot"),
              "."),
      tags$li(em("Transpose — all values become text:"),
              " this happens when the data mixes numeric and character columns. ",
              "After transposing, convert columns back to numeric using the Edit module if needed."),
      tags$li(em("Transpose — list-column error:"),
              " columns that contain list values cannot be transposed. ",
              "Remove or flatten those columns first."),
      tags$li(em("Performance warning:"),
              " large Wider or Longer transformations show a confirmation dialog where you can cancel or continue.")
    )
  )
}

render_merge_content <- function() {
  tagList(
    h2("Merge Operations"),
    tags$hr(),

    div(class = "alert alert-info",
        strong("What this does"),
        p("Combines the Primary Data with the Secondary Data using matching key columns. ",
          "You choose the join columns, the Join Type, and which extra fields to bring from Secondary. ",
          "Appended columns from Secondary are prefixed with ", code("Merged_"), ". ",
          strong("The merged table replaces the Primary Data"),
          "; Secondary Data is not modified.")
    ),

    h3("Interface"),
    tags$ul(
      tags$li(p(strong("Primary Data Join Column"), " — Select the key column in Primary Data.")),
      tags$li(p(strong("Secondary Data Join Column"), " — Select the matching key column in Secondary Data.")),
      tags$li(p(strong("Additional Columns from Secondary Data"),
                " — Choose the extra Secondary columns to append to Primary Data. ",
                "Only the columns you select here (and the Secondary key used for matching) are added.")),
      tags$li(p(strong("Join Type"),
                " — ", em("Left Join (recommended)"), ", ", em("Inner Join"), ", ", em("Full Join"), ".")),
      tags$li(p(strong("Merge Preview"),
                " — Shows expected rows × columns and a small sample of the resulting table.")),
      tags$li(p(strong("Apply Merge"),
                " — Performs the join and writes the result back to Primary Data."))
    ),

    h3("How it works"),
    tags$ul(
      tags$li("Both Primary and Secondary Data must be available."),
      tags$li("Pick one column under Primary Data Join Column and one under Secondary Data Join Column (they must exist in their tables)."),
      tags$li("Pick any fields under Additional Columns from Secondary Data — only these (plus the Secondary key for matching) are appended."),
      tags$li("Select a Join Type and check the Merge Preview."),
      tags$li("Click Apply Merge: appended Secondary fields are written with prefix ", code("Merged_"), "; Primary Data is replaced; Secondary Data stays unchanged.")
    ),

    h3("Methods / Options explained"),
    tags$ul(
      tags$li(p(strong("Left Join (recommended)"),
                " — Keeps all rows from Primary Data. Unmatched Secondary values become ", code("NA"),
                "; if there are multiple matches in Secondary, rows are repeated accordingly (one-to-many).")),
      tags$li(p(strong("Inner Join"),
                " — Keeps only keys present in both datasets (drops non-matching rows).")),
      tags$li(p(strong("Full Join"),
                " — Keeps all keys from both; fills the missing side with ", code("NA"), "."))
    ),

    h3("Examples / Scenarios"),
    div(
      class = "dw-doc-merge-scenarios",
      tags$style(HTML("
        .dw-doc-merge-scenarios .dw-scenario-card {
          border: 1px solid #d7e3f4; border-radius: 8px; padding: 12px;
          margin-bottom: 14px; background: #fcfdff;
        }
        .dw-doc-merge-scenarios .dw-scenario-title {
          display: block; margin-bottom: 6px; color: #1f4e8c;
          font-size: 16px; font-weight: 700;
        }
        .dw-doc-merge-scenarios .table-responsive { width: 100%; margin: 0; }
        .dw-doc-merge-scenarios .dw-scenario-table { width: 100%; }
        .dw-doc-merge-scenarios .dw-scenario-table caption {
          padding: 0 0 4px; color: inherit; text-align: left;
        }
        .dw-doc-merge-scenarios .dw-scenario-table caption span {
          display: inline-block; border-radius: 12px; padding: 2px 8px;
          font-size: 11px; font-weight: 700;
        }
        .dw-doc-merge-scenarios .dw-scenario-input caption span {
          border: 1px solid #f1b7b7; background: #ffe9e9; color: #9d1f1f;
        }
        .dw-doc-merge-scenarios .dw-scenario-result caption span {
          border: 1px solid #b4e1c2; background: #e8f8ee; color: #196b35;
        }
        .dw-doc-merge-scenarios .dw-scenario-table th {
          background: #eaf2fd; color: #1f4e8c; font-weight: 700; text-align: center;
        }
        .dw-doc-merge-scenarios .dw-scenario-table td,
        .dw-doc-merge-scenarios .dw-scenario-table th {
          border: 1px solid #c9d9ee !important; padding: 5px 8px !important; font-size: 12px;
        }
        .dw-doc-merge-scenarios .dw-scenario-table tbody tr:nth-child(odd) { background: #f3f8ff; }
        .dw-doc-merge-scenarios .dw-scenario-table tbody tr:nth-child(even) { background: #e8f2ff; }
        .dw-doc-merge-scenarios .row {
          display: flex; align-items: center; justify-content: center;
        }
        .dw-doc-merge-scenarios .row > .col-sm-5 { flex: 1 1 0; width: auto; }
        .dw-doc-merge-scenarios .row > .col-sm-2 { flex: 0 0 16.66666667%; width: 16.66666667%; }
        .dw-doc-merge-scenarios .dw-scenario-flow {
          margin: 6px 0; color: #1f4e8c; font-size: 22px;
          font-weight: 700; text-align: center;
        }
        .dw-doc-merge-scenarios .dw-scenario-result {
          width: 66.66666667%; margin-right: auto; margin-left: auto;
        }
        @media (max-width: 767px) {
          .dw-doc-merge-scenarios .row { flex-direction: column; }
          .dw-doc-merge-scenarios .row > [class*='col-sm-'] { flex: 0 0 auto; width: 100%; }
          .dw-doc-merge-scenarios .row .dw-scenario-flow { transform: rotate(90deg); }
          .dw-doc-merge-scenarios .dw-scenario-result { width: 100%; }
        }
      ")),
    # Left Join
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Left Join"),
        fluidRow(
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Primary Data")),
                            tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"))),
                            tags$tbody(
                              tags$tr(tags$td("K1"), tags$td("a1")),
                              tags$tr(tags$td("K2"), tags$td("a2")),
                              tags$tr(tags$td("K3"), tags$td("a3"))
                            )
                 )
          ),
          column(2, tags$div(class = "dw-scenario-flow", HTML("&plus;"))),
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Secondary Data")),
                            tags$thead(tags$tr(tags$th("Secondary Data Join Column"), tags$th("B"))),
                            tags$tbody(
                              tags$tr(tags$td("K1"), tags$td("b1")),
                              tags$tr(tags$td("K2"), tags$td("b2"))
                            )
                 )
          )
        ),
        div(class = "dw-scenario-flow", HTML("&darr;")),
        tags$table(class="table table-condensed dw-scenario-table dw-scenario-result",
                   tags$caption(span("Result (Left Join)")),
                   tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"), tags$th("Merged_B"))),
                   tags$tbody(
                     tags$tr(tags$td("K1"), tags$td("a1"), tags$td("b1")),
                     tags$tr(tags$td("K2"), tags$td("a2"), tags$td("b2")),
                     tags$tr(tags$td("K3"), tags$td("a3"), tags$td(HTML("&nbsp;NA")))
                   )
        )
    ),
    br(),

    # Inner Join
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Inner Join"),
        fluidRow(
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Primary Data")),
                            tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"))),
                            tags$tbody(
                              tags$tr(tags$td("K1"), tags$td("a1")),
                              tags$tr(tags$td("K2"), tags$td("a2"))
                            )
                 )
          ),
          column(2, tags$div(class = "dw-scenario-flow", HTML("&plus;"))),
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Secondary Data")),
                            tags$thead(tags$tr(tags$th("Secondary Data Join Column"), tags$th("B"))),
                            tags$tbody(
                              tags$tr(tags$td("K2"), tags$td("b2")),
                              tags$tr(tags$td("K3"), tags$td("b3"))
                            )
                 )
          )
        ),
        div(class = "dw-scenario-flow", HTML("&darr;")),
        tags$table(class="table table-condensed dw-scenario-table dw-scenario-result",
                   tags$caption(span("Result (Inner Join)")),
                   tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"), tags$th("Merged_B"))),
                   tags$tbody(
                     tags$tr(tags$td("K2"), tags$td("a2"), tags$td("b2"))
                   )
        )
    ),
    br(),

    # Full Join
    div(class = "dw-scenario-card",
        span(class = "dw-scenario-title", "Full Join"),
        fluidRow(
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Primary Data")),
                            tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"))),
                            tags$tbody(
                              tags$tr(tags$td("K1"), tags$td("a1")),
                              tags$tr(tags$td("K4"), tags$td("a4"))
                            )
                 )
          ),
          column(2, tags$div(class = "dw-scenario-flow", HTML("&plus;"))),
          column(5,
                 tags$table(class="table table-condensed dw-scenario-table dw-scenario-input",
                            tags$caption(span("Secondary Data")),
                            tags$thead(tags$tr(tags$th("Secondary Data Join Column"), tags$th("B"))),
                            tags$tbody(
                              tags$tr(tags$td("K1"), tags$td("b1")),
                              tags$tr(tags$td("K3"), tags$td("b3"))
                            )
                 )
          )
        ),
        div(class = "dw-scenario-flow", HTML("&darr;")),
        tags$table(class="table table-condensed dw-scenario-table dw-scenario-result",
                   tags$caption(span("Result (Full Join)")),
                   tags$thead(tags$tr(tags$th("Primary Data Join Column"), tags$th("A"), tags$th("Merged_B"))),
                   tags$tbody(
                     tags$tr(tags$td("K1"), tags$td("a1"), tags$td("b1")),
                     tags$tr(tags$td("K3"), tags$td(HTML("&nbsp;NA")), tags$td("b3")),
                     tags$tr(tags$td("K4"), tags$td("a4"), tags$td(HTML("&nbsp;NA")))
                   )
        )
    )

    ),

    h3("Tips"),
    tags$ul(
      tags$li("Use ", em("Left Join (recommended)"), " to keep all rows from Primary Data."),
      tags$li("If the Secondary key repeats, joined rows repeat (one-to-many)."),
      tags$li("Select only necessary fields under ", strong("Additional Columns from Secondary Data"), " to keep the result small.")
    ),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(em("No data available:"), " load both Primary and Secondary Data."),
      tags$li(em("Join column missing:"), " ensure columns exist under “Primary Data Join Column” and “Secondary Data Join Column”."),
      tags$li(em("Package missing:"), " the dplyr package must be available."),
      tags$li(em("Unexpected row counts:"), " repeated keys lead to repeated rows; adjust keys or Join Type.")
    )
  )
}

render_basemean_content <- function() {
  tagList(
    h2("Basemean Calculation"),
    tags$hr(),

    div(class = "alert alert-info",
        strong("What this does"),
        p("Computes a mean across selected samples for a chosen abundance type and appends the result as a new Basemean column. ",
          "All Basemean columns can be removed at once via “Clear Basemeans”.")
    ),

    h3("Interface"),
    tags$ul(
      tags$li(
        p(strong("Select Abundance Type:"),
          " Choose the abundance layer to use. Options shown in the app include: ",
          em("Raw Abundance, Normalized Abundance, Imputed Raw Abundance, Imputed Normalized Abundance, Batch Corrected Abundance, Imputed Batch Corrected Abundance"),
          " (only if present in your data).")
      ),
      tags$li(
        p(strong("Samples"),
          " Select the samples to average for the chosen abundance type. Only samples available for that type are offered.")
      ),
      tags$li(
        p(strong("Suffix"),
          " Optional short text appended to the Basemean column name to distinguish multiple basemeans.")
      ),
      tags$li(
        p(strong("Add Basemean"),
          " Calculates the basemean and adds it to your data. If a Basemean with the same target name already exists, it will be overwritten.")
      ),
      tags$li(
        p(strong("Clear Basemeans"),
          " Removes all columns whose names start with ",
          code("Basemean"),
          ".")
      )
    ),

    h3("How it works"),
    tags$ul(
      tags$li("The sample list is filtered to the selected abundance type (only matching samples are eligible)."),
      tags$li("For each row, the module computes the arithmetic mean across the selected sample columns."),
      tags$li("A new column is added with a name beginning with ",
              code("Basemean"),
              " (your Suffix, if provided, is appended)."),
      tags$li("If the target Basemean column name already exists, it is overwritten (you are notified)."),
      tags$li("The metadata is extended with a new row describing the new Basemean column."),
      tags$li("Clicking ",
              em("Clear Basemeans"),
              " removes all columns whose names start with ",
              code("Basemean"),
              ".")
    )
  )
}

# Chapter 13: ID Annotation / Conversion
render_annotation_content <- function() {
  tagList(
    h2("ID Annotation and Conversion"),
    tags$hr(),

    div(
      class = "alert alert-info",
      strong("What this does"),
      p("Use ID Annotation / Conversion to convert identifiers within a species, map orthologs between species, or merge identifier columns already in the data.")
    ),
    tags$ul(
      tags$li("All original source columns are preserved."),
      tags$li("The original row count and row order are preserved."),
      tags$li("One new result column is created, with one result cell for every original row.")
    ),
    div(
      class = "alert",
      style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
      strong("Recommended: "),
      "Confirm the identifier type and source species before mapping, then spot-check familiar identifiers in the result."
    ),

    h3("Step-by-step workflow"),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Step 1 — Source Column")),
      div(class = "panel-body", tags$ul(
        tags$li("Choose the column containing the identifiers to convert."),
        tags$li("This control is hidden for Identifier Merging, which uses its ordered identifier list.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 2 — Source Species")),
      div(class = "panel-body", tags$ul(
        tags$li("Choose the species from which the source identifiers originate."),
        tags$li("This selection controls the OrgDb or BioMart source dataset.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 3 — Refresh Cache and Update Organisms")),
      div(class = "panel-body", tags$ul(
        tags$li(strong("Refresh Cache: "), "refresh the selected OrgDb in Annotation Hub mode, or open BioMart cache-management options."),
        tags$li(strong("Update Organisms: "), "refresh and expand organism choices; BioMart mode updates both source and target species metadata.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 4 — Source ID Type")),
      div(class = "panel-body", tags$ul(
        tags$li("Select the type actually stored in Source Column, for example SYMBOL, ENSEMBL, ENTREZID, or UNIPROT."),
        tags$li("Available choices depend on the species and mapping route.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 5 — Target ID Type")),
      div(class = "panel-body", tags$ul(
        tags$li("Select the identifier type for the new result column."),
        tags$li("BioMart limits this list to compatible types for the target species.")))
    ),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Step 6 — Annotation Strategy")),
      div(class = "panel-body", tags$ul(
        tags$li("Choose Annotation Hub Intraspecies Mapping, BioMart Intra-/Inter-species Mapping, or Identifier Merging."),
        tags$li("Only controls relevant to the selected strategy remain visible.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 7 — Target Species in BioMart mode")),
      div(class = "panel-body", tags$ul(
        tags$li("This control appears only for BioMart Intra-/Inter-species Mapping."),
        tags$li("Choose the source species for intraspecies mapping or a different species for ortholog mapping.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("Step 8 — Ambiguous Mapping Strategy")),
      div(class = "panel-body", tags$ul(
        tags$li(strong("First match: "), "retain one target for each source identifier."),
        tags$li(strong("Semicolon-separated: "), "retain all targets together in one cell."),
        tags$li("This control is hidden for Identifier Merging.")))
    ),
    div(
      class = "panel panel-success",
      div(class = "panel-heading", h4("Step 9 — Map IDs")),
      div(class = "panel-body", tags$ul(
        tags$li("Click Map IDs to run Annotation Hub or BioMart mapping."),
        tags$li("In Identifier Merging mode, use Merge Identifier instead.")))
    ),

    h3("Annotation strategies"),
    div(
      class = "panel panel-primary",
      div(class = "panel-heading", h4("Annotation Hub Intraspecies Mapping")),
      div(class = "panel-body", tags$ul(
        tags$li("Converts identifiers within Source Species using its organism annotation database (OrgDb)."),
        tags$li("Does not perform cross-species ortholog mapping."),
        tags$li("Can reuse a cached OrgDb; the first load or a refresh may require internet access.")))
    ),
    div(
      class = "panel panel-info",
      div(class = "panel-heading", h4("BioMart Intra-/Inter-species Mapping")),
      div(class = "panel-body", tags$ul(
        tags$li("Uses Ensembl BioMart for same-species mapping or known ortholog mapping between species."),
        tags$li("May differ from Annotation Hub even for the same species because it uses a different database route."),
        tags$li("Some routes obtain target-species Ensembl gene IDs first, then use the target-species OrgDb for final conversion.")))
    ),
    div(
      class = "alert",
      style = "background-color: #3498db; border-color: #3498db; color: #fff;",
      h4("BioMart mapping guidance"),
      p("Ensembl BioMart provides species-dependent identifier and ortholog relationships:"),
      tags$ul(
        tags$li("Ensembl BioMart performs same-species or ortholog mapping."),
        tags$li("Some routes use the target-species OrgDb for final conversion."),
        tags$li("Not every gene has a known ortholog."),
        tags$li("Coverage depends on database completeness and evolutionary distance."),
        tags$li("Available source and target ID types depend on BioMart compatibility."),
        tags$li("Types such as ", code("ALIAS"), " may be unavailable in BioMart mode."))
    ),
    div(
      class = "panel panel-success",
      div(class = "panel-heading", h4("Identifier Merging")),
      div(class = "panel-body", tags$ul(
        tags$li("Combines columns marked as Identifier without biological conversion."),
        tags$li(strong("Drag and drop: "), "set top-to-bottom identifier priority."),
        tags$li(strong("Remove identifiers: "), "click x to exclude a column from the merge."),
        tags$li(strong("Reset list: "), "restore the original identifier list and order."),
        tags$li(strong("First non-empty only: "), "use the first non-empty, non-missing value in priority order."),
        tags$li(strong("Concatenate all: "), "join all non-empty, non-missing values in list order with commas."),
        tags$li(strong("Merge Identifier: "), "run the selected Merge Behavior and add the result column.")))
    ),

    h3("Results and diagnostics"),
    h4("Interpret the result"),
    tags$ul(
      tags$li("Original source columns, row count, and row order remain unchanged."),
      tags$li("A descriptively named result column is added, and every original row receives one result cell."),
      tags$li("First match and Semicolon-separated collapse one-to-many mappings without adding rows."),
      tags$li("If no identifiers map, zero matches are reported and an all-missing result column is not added.")
    ),
    h4("Read the diagnostics"),
    tags$ul(
      tags$li("Last Mapping Result reports source and output columns, ID conversion, mode, strategy, mapped or merged count, unmapped or empty count, and percentage."),
      tags$li("BioMart status may identify a direct route, ortholog counts, or both BioMart and OrgDb stages."),
      tags$li("Cache dates appear when available.")),

    h3("Cache controls"),
    h4("Refresh Cache"),
    tags$ul(
      tags$li(strong("Annotation Hub: "), "download a fresh OrgDb for Source Species and reload its ID types."),
      tags$li(strong("BioMart metadata: "), "refresh all species and key types, refresh the current source and target key types, or load missing species key types."),
      tags$li(strong("BioMart mapping data: "), "load the current pair, preload default pairs, or download missing default pairs."),
      tags$li(strong("Clear controls: "), "clear the keytype/species cache, mapping database cache, or all BioMart cache as labeled."),
      tags$li("Downloads require internet access; cached data can be reused when available.")),
    h4("Update Organisms"),
    tags$ul(
      tags$li(strong("Annotation Hub: "), "update the Source Species choices."),
      tags$li(strong("BioMart: "), "rebuild species and key-type metadata and update source and target lists."),
      tags$li("If the service is unavailable, an existing cache or built-in fallback list may remain available.")),

    h3("Mapping limitations"),
    tags$ul(
      tags$li("Valid identifiers may remain unmapped when database records or requested relationships are absent."),
      tags$li("Cross-species coverage may be lower for distant species, lineage-specific genes, or incomplete ortholog annotations."),
      tags$li("BioMart offers only compatible species-specific ID types; an alias-like type may require Annotation Hub within one species."),
      tags$li("Mapping coverage is a diagnostic, not a guarantee that every source value will map.")),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(strong("Many identifiers are unmapped: "), "check Source Species and Source ID Type, and remove unintended prefixes or whitespace."),
      tags$li(strong("Cross-species coverage is low: "), "check Source Species, Target Species, and Target ID Type; biological limitations may still reduce coverage."),
      tags$li(strong("An ID type is missing in BioMart: "), "choose a compatible offered type or use Annotation Hub for a supported intraspecies conversion."),
      tags$li(strong("A database or species list will not load: "), "check the internet connection, retry when the remote service is available, or use a cached route."),
      tags$li(strong("No columns appear for merging: "), "mark the intended columns as Identifier in metadata, return to Identifier Merging, and use Reset list if necessary."),
      tags$li(strong("Before downstream analysis: "), "review counts, spot-check results, and record the route and cache date."))
    )
}
