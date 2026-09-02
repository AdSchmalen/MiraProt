# ==============================================================================
# File: Documentation/GSEA_doc_tech_core.R
#
# Purpose:
#   Core developer-facing technical documentation for the GSEA module.
#   Focused on architecture, reactive flow, integration contracts, and state.
# ==============================================================================

render_GSEA_tech_core_overview_content_GSEA <- function() {
  div(
    h2("Technical Overview"),
    hr(),
    h3("Architecture"),
    p(
      "GSEA is implemented as an orchestrator module in ",
      code("modules/GSEA_module.R"),
      " plus sourced files under ",
      code("modules/GSEA/"),
      ". The orchestrator coordinates data access, ranking, enrichment execution, plotting, import/export, and public API return values."
    ),
    h4("Implementation layout"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      paste(
        "modules/GSEA_module.R                            # module server entry + orchestration",
        "modules/GSEA/GSEA_module_logic.R                 # ranking + analysis execution path",
        "modules/GSEA/GSEA_module_Gene_Sets.R             # gene set discovery/selection helpers",
        "modules/GSEA/GSEA_module_parallelization.R       # enrichment execution utilities",
        "modules/GSEA/GSEA_module_state.R                 # module state helpers",
        "modules/GSEA/GSEA_module_observer.R              # UI observers + render bindings",
        "modules/GSEA/GSEA_ui.R                           # module UI definition",
        "modules/GSEA/GSEA_plots.R                        # plot constructors",
        "modules/GSEA/GSEA_export.R                       # save/load + export handlers (if loaded externally)",
        sep = "\n"
      )
    ),
    h3("State model"),
    p("The server uses reactiveVal-backed state for runtime results and UI-synchronized selections."),
    tags$ul(
      tags$li(code("res_GSEA"), ": canonical result wrapper (Results, GeneList, GeneList_FC, source, analysis_metadata)."),
      tags$li(code("current_rankings"), ": latest ranking payload; expected members include Ranks and FC."),
      tags$li(code("current_plot"), ": latest generated plot object used by render and download handlers."),
      tags$li(code("analysis_metadata"), ": analysis type/timestamps/status metadata consumed by info panels and integration helpers."),
      tags$li(code("selected_enrichment"), ": selected pathways synchronized with ", code("input$custom_Enrich_select"), "."),
      tags$li(code("imported_gsea_results"), " and ", code("import_status_message"), ": import lifecycle state.")
    ),
    h3("Actual implemented contract (result object)"),
    tags$ul(
      tags$li(strong("Required wrapper shape:"), " a list with ", code("Results"), " and optional ", code("GeneList"), ", ", code("GeneList_FC"), ", ", code("source"), ", ", code("analysis_metadata"), "."),
      tags$li(strong("Required class support:"), " ", code("Results"), " is expected to inherit from ", code("gseaResult"), " or ", code("enrichResult"), "."),
      tags$li(strong("Compatibility rule:"), " imported objects loaded through ", code("load_res_GSEA()"), " must match the same shape used for fresh calculations.")
    )
  )
}

render_GSEA_tech_core_dataproc_content_GSEA <- function() {
  div(
    h2("Data Processing"),
    hr(),
    h3("Processing contract map"),
    tags$table(
      class = "table table-sm table-striped",
      tags$thead(
        tags$tr(
          tags$th("Function group"),
          tags$th("Key entry points"),
          tags$th("Inputs consumed"),
          tags$th("Outputs/side effects"),
          tags$th("Failure/validation behavior")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Selector preparation"),
          tags$td(code("gsea_get_identifier_choices()"), tags$br(), code("gsea_get_sample_choices()"), tags$br(), code("gsea_get_ratio_choices()"), tags$br(), code("gsea_get_pvalue_choices()")),
          tags$td(code("rv$data_def"), ", content type, ratio context"),
          tags$td("Populates selector inputs used by ranking and analysis triggers."),
          tags$td("Empty/incompatible definitions degrade to empty choice sets.")
        ),
        tags$tr(
          tags$td("Ranking construction"),
          tags$td(code("compute_custom_ranks_GSEA()"), tags$br(), code("compute_precalculated_ranks_GSEA()"), tags$br(), code("validate_ranking_vector()")),
          tags$td(code("RankinkMethod_GSEA"), " dispatches to sample groups or precalculated-statistics columns"),
          tags$td("Creates ranked vector payload written to ", code("current_rankings"), "."),
          tags$td("Validation blocks enrichment for invalid or low-variance ranking vectors.")
        ),
        tags$tr(
          tags$td("Enrichment execution"),
          tags$td(code("run_gsea_analysis()")),
          tags$td("Ranked gene list, selected GMT path, permutation count (default 10,000), p-value cutoff"),
          tags$td("Produces enrichment result object and updates ", code("res_GSEA"), "."),
          tags$td("Missing prerequisites short-circuit with user-facing notification.")
        ),
        tags$tr(
          tags$td("State + render sync"),
          tags$td(code("res_GSEA"), " writers", tags$br(), code("analysis_metadata"), " writers", tags$br(), code("output$*"), " renderers"),
          tags$td("Result wrapper, current rankings, selected pathways, plot settings"),
          tags$td("Refreshes result tables, pathway selector state, and plot outputs."),
          tags$td("On failure, previous valid state is retained for downstream consumers.")
        )
      )
    ),
    h3("Extension points / interface contracts"),
    tags$ul(
      tags$li(strong("Ranking extension point:"), " preserve inputs/outputs expected by ", code("validate_ranking_vector()"), " and ", code("run_gsea_analysis()"), " when adding new rank builders."),
      tags$li(strong("Result wrapper contract:"), " maintain ", code("res_GSEA"), " list shape (", code("Results"), ", optional ranking metadata) so plotting/export/integration consumers remain compatible."),
      tags$li(strong("UI contract stability:"), " selectors and trigger IDs referenced by observers must remain stable unless companion UI/server updates are shipped together.")
    ),
    h3("Actual implemented contract (runtime prerequisites)"),
    tags$ul(
      tags$li(code("rv$data_mod"), " and ", code("rv$data_def"), " are the required shared app inputs."),
      tags$li(code("input$fileSelector_GSEA"), ": selected GMT file name used to construct the local gene set path."),
      tags$li(code("input$Identifier_GSEA"), ": selected identifier column used by ranking functions."),
      tags$li(code("input$RankinkMethod_GSEA"), ": single dispatch control; ", code("log2(FC)"), ", ", code("log2(FC) x -log10(p)"), ", and ", code("-log10(p)"), " select ", code("compute_precalculated_ranks_GSEA()"), " and are passed directly as its ", code("metric"), ". All other values resolve through ", code("rank_methods"), " and select ", code("compute_custom_ranks_GSEA()"), "."),
      tags$li("Session restore reads ", code("RankingMetric_GSEA_precalc"), " for legacy ", code("GSEA_type_select == \"Precalculated Ranking\""), " and intermediate ", code("RankinkMethod_GSEA == \"Precalculated statistics\""), " states; sample-derived methods restore normally.")
    )
  )
}

render_GSEA_tech_core_integration_content_GSEA <- function() {
  div(
    h2("Integration Details"),
    hr(),
    h3("Module return API"),
    p("The server returns reactive accessors and helper functions consumed by other MiraProt modules."),
    tags$ul(
      tags$li(code("get_results()"), ": reactive accessor for ", code("res_GSEA"), "."),
      tags$li(code("get_current_rankings()"), ": reactive accessor for ranking payload."),
      tags$li(code("get_selected_enrichment()"), " and ", code("get_selected_pathways()"), ": selected pathway accessors."),
      tags$li(code("get_analysis_metadata()"), ": reactive metadata accessor."),
      tags$li(code("has_results()"), ", ", code("analysis_ready()"), ", ", code("module_health_check()"), ": status/health helpers."),
      tags$li(code("get_significant_pathways()"), ", ", code("get_pathway_genes(pathway_name)"), ", ", code("get_gene_rankings()"), ": pathway/gene utility accessors."),
      tags$li(code("export_results_for_pca()"), ": structured handoff payload for PCA integration."),
      tags$li(code("clear_imported_results()"), ": resets imported-result state.")
    ),
    h3("Cross-module integration points"),
    tags$ul(
      tags$li("GSEA outputs are consumed by Volcano, Dot Plot, PCA, Venn, STRING, Heatmap, and Plot Grid flows through shared app/module output wiring."),
      tags$li("Add-to-grid flow validates ", code("current_plot()"), " as a ggplot object before forwarding to grid state."),
      tags$li("Import/export paths preserve the same wrapper contract so downstream consumers can treat imported and computed results identically.")
    ),
    h3("Actual implemented contract (integration surface)"),
    tags$ul(
      tags$li(strong("Stable accessor names:"), " maintain the API names listed above when refactoring internals."),
      tags$li(strong("Result compatibility:"), " preserve ", code("res_GSEA"), " wrapper semantics for both calculated and imported paths."),
      tags$li(strong("Pathway selection input:"), " ", code("input$custom_Enrich_select"), " remains the canonical selected-pathway UI contract.")
    )
  )
}
