# ==============================================================================
# File: Documentation/GSEA_doc_tech_functions.R
#
# Purpose:
#   Developer-facing function-group and UI/server contract reference for GSEA.
#   Concise, implementation-aligned, and contract-first.
# ==============================================================================

render_GSEA_tech_functions_content_GSEA_v2 <- function() {
  div(
    h2("Functions Reference"),
    hr(),
    h3("Function group to responsibility map"),
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
          tags$td("Integration/orchestration"),
          tags$td(code("GSEA_module_server()"), tags$br(), code("debug_log()")),
          tags$td(code("id"), ", ", code("rv"), ", module config / debug level"),
          tags$td("Initializes module state, observers, renderers, and returns integration API."),
          tags$td("Defensive guards in downstream observers prevent invalid execution paths.")
        ),
        tags$tr(
          tags$td("Data discovery"),
          tags$td(code("data_modified()"), tags$br(), code("df_data_definition_post_mod()"), tags$br(), code("gsea_get_*_choices()")),
          tags$td(code("rv$data_mod"), ", ", code("rv$data_def"), ", selected content/ratio context"),
          tags$td("Builds identifier, sample, ratio, and p-value selector options."),
          tags$td("Missing or incompatible schema results in empty selector choices.")
        ),
        tags$tr(
          tags$td("Ranking + enrichment"),
          tags$td(code("compute_custom_ranks_GSEA()"), tags$br(), code("compute_precalculated_ranks_GSEA()"), tags$br(), code("validate_ranking_vector()"), tags$br(), code("run_gsea_analysis()")),
          tags$td(code("RankinkMethod_GSEA"), " dispatches to sample-group inputs or precalculated-statistics columns; selected GMT and permutation/p-value settings"),
          tags$td("Produces ranked gene list, updates analysis result payload, and sets metadata."),
          tags$td("Invalid/low-variance ranking vectors abort analysis before enrichment.")
        ),
        tags$tr(
          tags$td("Plotting + rendering"),
          tags$td(code("output$gsea_results_table"), tags$br(), code("output$gene_rankings_table"), tags$br(), code("output$leading_edge_table"), tags$br(), code("output$GSEAplot_custom"), tags$br(), code("create_*_plot()")),
          tags$td("Current result wrapper, selected pathways, plot type/theme controls"),
          tags$td("Renders tables/plots and updates ", code("current_plot"), " for preview/export."),
          tags$td("Plot-specific prerequisites enforce selection/count constraints.")
        ),
        tags$tr(
          tags$td("Import/export"),
          tags$td(code("load_res_GSEA()"), tags$br(), code("save_res_GSEA()"), tags$br(), code("is_single_sheet_gsea()"), tags$br(), code("download_*")),
          tags$td("Uploaded files, current result state, export format + sizing controls"),
          tags$td("Loads persisted results, validates import shape, and emits table/plot downloads."),
          tags$td("Import validation rejects incompatible worksheet/object structures.")
        )
      )
    ),
    h3("Actual implemented UI contract"),
    p("Names below are the active input/output IDs referenced by the current server implementation."),
    h4("Core analysis inputs"),
    tags$ul(
      tags$li(code("createGSEA"), ": start analysis."),
      tags$li(code("RankinkMethod_GSEA"), ": single ranking dispatch control. Ordinary values use ", code("compute_custom_ranks_GSEA()"), " with ", code("Identifier_GSEA"), ", ", code("RefenceValues_GSEA"), " and sample groups. The values ", code("log2 Ratio (precalculated)"), ", ", code("log2 Ratio x -log10(p-Value)"), ", and ", code("-log10(p-Value)"), " use ", code("compute_precalculated_ranks_GSEA()"), " and are passed directly as its ", code("metric"), "."),
      tags$li(code("fileSelector_GSEA"), ": selected GMT file."),
      tags$li(code("numeratorSel_GSEA"), ", ", code("denominatorSel_GSEA"), ": sample-derived ranking groups."),
      tags$li(code("AbundanceRatio_GSEA_precalc"), ", ", code("pVal_GSEA_precalc"), ": Precalculated statistics selectors."),
      tags$li(code("custom_Enrich_select"), ": selected pathways."),
      tags$li(code("numPermutations_GSEA"), ": defaults to 10,000 and is passed to ", code("run_gsea_analysis()"), " as ", code("nPermSimple"), "."),
      tags$li("Legacy restore reads ", code("RankingMetric_GSEA_precalc"), " when ", code("GSEA_type_select == \"Precalculated Ranking\""), " or ", code("RankinkMethod_GSEA == \"Precalculated statistics\""), "; sample-derived sessions retain their saved method.")
    ),
    h4("Plot controls"),
    tags$ul(
      tags$li(code("create_gsea_plot"), ", ", code("plot_type_GSEA"), ": plot generation trigger and type."),
      tags$li(code("ThemeSelect_GSEA"), ", ", code("LegendPosition_GSEA"), ": theme and legend settings."),
      tags$li(code("GSEAColorInput_down"), ", ", code("GSEAColorInput_zero"), ", ", code("GSEAColorInput_up"), ": continuous color controls."),
      tags$li(code("AxisTitleSize_GSEA"), ", ", code("tickSize_GSEA"), ", ", code("legendTextSize_GSEA"), ", ", code("legendTitleSize_GSEA"), ", ", code("labelSize_GSEA"), ": size controls."),
      tags$li(code("GSEAplot_custom"), ": main plot output.")
    ),
    h4("Import/export + grid controls"),
    tags$ul(
      tags$li(code("gsea_import_file"), ", ", code("gsea_import_status"), ": import upload and status."),
      tags$li(code("download_res_GSEA"), ", ", code("download_results_csv"), ", ", code("download_results_xlsx"), ", ", code("download_gene_rankings"), ", ", code("download_leading_edge"), ": data export outputs."),
      tags$li(code("downloadFormat_GSEA"), ", ", code("plotWidthInch_GSEA"), ", ", code("plotHeightInch_GSEA"), ", ", code("resolution_DPI_GSEA"), ", ", code("downloadPlotButton_GSEA"), ": plot export controls."),
      tags$li(code("grid_label"), ", ", code("plot_type_grid"), ", ", code("add_to_grid"), ": plot-grid integration controls.")
    ),
    h3("Actual implemented server contract"),
    tags$ul(
      tags$li(strong("Reactive accessors:"), " ", code("get_results()"), ", ", code("get_current_rankings()"), ", ", code("get_selected_enrichment()"), ", ", code("get_selected_pathways()"), ", ", code("get_analysis_metadata()"), "."),
      tags$li(strong("Status helpers:"), " ", code("has_results()"), ", ", code("analysis_ready()"), ", ", code("module_health_check()"), "."),
      tags$li(strong("Data helpers:"), " ", code("get_significant_pathways()"), ", ", code("get_pathway_genes(pathway_name)"), ", ", code("get_gene_rankings()"), ", ", code("export_results_for_pca()"), "."),
      tags$li(strong("Lifecycle helper:"), " ", code("clear_imported_results()"), ".")
    )
  )
}
