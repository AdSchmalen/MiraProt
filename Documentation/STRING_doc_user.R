# ==============================================================================
# File: Documentation/STRING_doc_user.R
#
# Purpose:
#   User-facing guide for the STRING module. Audience: scientific users,
#   including users with limited proteomics background.
# ==============================================================================

render_STRING_overview_content_STRING <- function() {
  div(
    h2("STRING Module - Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("What this module is for"),
      p(
        "Use the STRING module to place your selected proteins into a biological context by visualizing ",
        "protein-protein interaction networks."
      ),
      p(
        "The module links proteins from your MiraProt dataset to the STRING database and shows how those proteins are connected.",
        " This supports pathway exploration, functional interpretation, and hypothesis generation."
      )
    ),

    h3("What the STRING database represents"),
    p(
      "STRING is a knowledge base of protein-protein associations. A connection can represent direct physical binding or ",
      "functional association (for example, proteins acting in the same pathway or process)."
    ),
    tags$ul(
      tags$li(strong("Experimental evidence:"), " interaction support from laboratory data."),
      tags$li(strong("Predicted evidence:"), " computational inference from genomic context and comparative signals."),
      tags$li(strong("Text-mining evidence:"), " co-mention patterns in scientific literature."),
      tags$li(strong("Co-expression evidence:"), " proteins with correlated expression profiles across conditions or studies."),
      tags$li(strong("Database/knowledge transfer evidence:"), " curated pathway/complex resources and orthology-based transfer.")
    ),

    h3("How proteins enter the network"),
    tags$ol(
      tags$li("Choose the identifier type that matches your dataset."),
      tags$li("Provide proteins manually and/or import proteins from GO, GSEA, or STRING clusters."),
      tags$li("Click Add to populate the Selected Proteins table."),
      tags$li("Create the network from the proteins in that table.")
    ),
    p(
      strong("Important:"),
      " the network is created from the Selected Proteins table, not directly from unconfirmed text in the input box."
    ),

    h3("Confidence score and interaction type"),
    tags$ul(
      tags$li(strong("Confidence score threshold:"), " controls how strong evidence must be before an edge is shown. Higher thresholds produce stricter, sparser networks."),
      tags$li(strong("Interaction type:"), " lets you emphasize broader functional associations, physical interactions, or the combined view."),
      tags$li("A dense network at low thresholds may hide structure; stricter thresholds can improve interpretability.")
    ),

    h3("How to interpret network structure"),
    tags$ul(
      tags$li(strong("Clusters:"), " groups of proteins with many internal connections; often reflect related biology or pathway modules."),
      tags$li(strong("Hubs:"), " proteins with many edges; these may represent central connectors in your selected context."),
      tags$li(strong("Connectivity:"), " isolated or weakly connected proteins may represent specific or peripheral roles, while dense regions suggest shared context."),
      tags$li("Interpret network topology together with abundance trends, differential results, and enrichment analyses.")
    ),

    h3("Typical scientific use cases"),
    tags$ul(
      tags$li("Explore whether proteins from enriched pathways form coherent network modules."),
      tags$li("Identify candidate hub proteins for follow-up validation."),
      tags$li("Compare network substructures with GO/GSEA interpretation to refine biological narratives."),
      tags$li("Prepare publication-ready network panels for multi-plot figures.")
    ),

    h3("Limitations and bias awareness"),
    tags$ul(
      tags$li(strong("Coverage bias:"), " some proteins, organisms, or processes are better represented than others."),
      tags$li(strong("Identifier dependency:"), " mismatched identifier systems reduce mapping success and can hide expected proteins."),
      tags$li(strong("Prediction uncertainty:"), " predicted and transferred evidence is informative but not equivalent to direct experimental confirmation."),
      tags$li(strong("Literature bias:"), " highly studied proteins often have richer annotation and more connections."),
      tags$li(strong("Interpretation scope:"), " network edges suggest association, not direct causality.")
    ),

    div(
      class = "alert alert-success",
      h4("Practical interpretation workflow"),
      tags$ol(
        tags$li("Build the first network with a moderate confidence score."),
        tags$li("Inspect clusters and hub candidates."),
        tags$li("Increase score threshold or degree filter if the plot is too dense."),
        tags$li("Cross-check key regions against GO/GSEA and quantitative results."),
        tags$li("Export the final network with clear labels and reproducible settings.")
      )
    )
  )
}
