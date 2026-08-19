# ==============================================================================
# File: Documentation/GO_doc_tech.R
#
# Purpose:
#   Developer-facing technical documentation for the GO module.
#   Structured around architecture, function groups, data flow, and integration
#   contracts to support maintenance and extension work.
# ==============================================================================

render_GO_tech_overview_content_GO <- function() {
  div(
    h2("Technical Overview"),
    hr(),
    h3("Architecture"),
    p(
      "The GO module performs over-representation analysis on filtered proteins, ",
      "builds a hierarchical GO-term selection tree, and generates multiple GO visualizations ",
      "through a shared styling and export pipeline."
    ),
    h4("Implementation layout"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      paste(
        "modules/GO_module.R                 # public module orchestrator and server wiring",
        "modules/GO/GO_ui.R                  # GO user interface",
        "modules/GO/GO_module_hub*.R         # public annotation resolvers, acquisition, discovery, and caching",
        "modules/GO/GO_module_logic.R        # readiness, pairing, identifiers, and enrichment",
        "modules/GO/GO_module_plots.R        # plot constructors and formatting",
        "modules/GO/GO_module_tree.R         # tree construction and selection helpers",
        "modules/GO/GO_module_state.R        # session reactive state",
        "modules/GO/GO_module_observer*.R    # observer, output, download, restore, and cleanup families",
        sep = "\n"
      )
    ),
    h3("High-level workflow"),
    tags$ol(
      tags$li("Read shared inputs from ", code("rv$data_mod"), " and ", code("rv$data_def"), "."),
      tags$li("Validate prerequisites and resolve identifier/abundance/p-value columns."),
      tags$li("Apply abundance and p-value filters to produce the enrichment gene set and background universe."),
      tags$li("Load organism annotation and key types with organism-specific caching."),
      tags$li("Run GO enrichment and package downstream result objects."),
      tags$li("Build/update hierarchical GO tree for term selection."),
      tags$li("Create selected plot type using shared styling inputs."),
      tags$li("Expose results for export and cross-module integration (for example Plot Grid).")
    ),
    h3("Subsystem responsibilities"),
    tags$table(
      class = "table table-sm table-striped",
      tags$thead(
        tags$tr(
          tags$th("Subsystem"),
          tags$th("Primary responsibilities"),
          tags$th("Key entry points")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Data readiness and selector discovery"),
          tags$td("Validate GO prerequisites and construct UI choices for identifiers, abundance columns, and p-values."),
          tags$td(code("check_go_data_readiness()"), tags$br(), code("check_pairing_prerequisites()"), tags$br(), code("get_gene_identifier_choices()"), tags$br(), code("get_column_choices_by_content()"), tags$br(), code("find_best_pvalue_partner()"))
        ),
        tags$tr(
          tags$td("Annotation loading and cache management"),
          tags$td("Resolve organism to OrgDb source, cache annotation databases as SQLite artifacts, and refresh available organism choices. OrgDb objects are reconstructed from cached .sqlite files via AnnotationDbi::loadDb() -- never RDS-serialized."),
          tags$td(code("organism_to_orgdb()"), tags$br(), code("load_annotation_hub_with_progress()"), tags$br(), code("load_organism_cache()/save_organism_cache()"), tags$br(), code("reconstruct_orgdb_from_sqlite()"), tags$br(), code("load_keytypes_from_cache()/save_keytypes_to_cache()"), tags$br(), code("update_available_organisms_safe()"))
        ),
        tags$tr(
          tags$td("Enrichment execution and result packaging"),
          tags$td("Transform UI parameters to internal codes, run enrichment, and construct reusable results bundle for plotting and integration."),
          tags$td(code("convert_ontology_input()"), tags$br(), code("convert_padjust_method()"), tags$br(), code("perform_go_enrichment()"), tags$br(), code("create_go_results_list_direct()"))
        ),
        tags$tr(
          tags$td("Tree building and selection management"),
          tags$td("Create grouped GO hierarchy, support fallback tree generation, and extract selected leaf terms robustly."),
          tags$td(code("create_go_tree_structure()"), tags$br(), code("create_simple_tree_fallback()"), tags$br(), code("extract_selected_terms_hierarchical()"), tags$br(), code("filter_terms_by_pvalue()"))
        ),
        tags$tr(
          tags$td("Plot construction and styling"),
          tags$td("Build GO visual outputs with shared theme, legend, and size contracts."),
          tags$td(code("create_go_dotplot()"), tags$br(), code("create_go_cnet_plot_fc_fixed()"), tags$br(), code("create_go_enrichment_map_fixed()"), tags$br(), code("create_go_pubmed_plot()"), tags$br(), code("get_selected_theme_go()"), tags$br(), code("apply_legend_position()"))
        )
      )
    ),
    h3("Result objects used across the module"),
    tags$ul(
      tags$li(code("go_results$Edo_GO"), ": canonical enrichment result object."),
      tags$li(code("go_results$Edo_GO_safe"), ": top-term bounded variant for safer downstream rendering."),
      tags$li(code("go_results$Edox_GO"), ": readable enrichment representation when annotation mapping is available."),
      tags$li(code("go_results$Edop_GO"), ": pairwise-term-similarity object for enrichment-map plotting."),
      tags$li(code("go_results$go_data_FC"), ": named fold-change vector for Cnet and related visualizations."),
      tags$li(code("go_results$go_data"), ": filtered analysis input retained for reproducibility and integration helpers.")
    ),
    h3("Robustness and performance design"),
    tags$ul(
      tags$li("Organism annotation databases are cached as SQLite files and reconstructed via AnnotationDbi::loadDb(), eliminating broken serialized handles across sessions."),
      tags$li("Cache metadata (cache_metadata.rds) tracks status, source, sqlite path, timestamps, and TTL for deterministic reload behaviour."),
      tags$li("Freshness policy is shared with Annotation: keytype caches have a strict 10-day TTL, organism caches have a 30-day TTL, and the startup default path treats 14-day keytypes as deferred/stale for direct loading by keeping static defaults without downloading."),
      tags$li("After startup, a strict keytype cache miss can still use organism-specific static defaults when the organism cache is within 30 days, avoiding unnecessary AnnotationHub downloads."),
      tags$li("Legacy marker-only cache layouts are detected and upgraded transparently on first successful download."),
      tags$li("Critical stages use guarded error handling and safe fallbacks to preserve module usability."),
      tags$li("Term-count limiting prevents overcrowded plots and improves rendering predictability."),
      tags$li("Tree and plot generation are separated, enabling independent maintenance and extension.")
    )
  )
}

render_GO_tech_functions_content_GO <- function() {
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
          tags$td("UI composition and synchronization"),
          tags$td(code("GO_UI()"), tags$br(), code("create_go_tree_ui()"), tags$br(), code("create_plot_area_ui()"), tags$br(), code("update_go_tree_display()"), tags$br(), code("update_plot_type_choices()"), tags$br(), code("update_integration_choices()")),
          tags$td("Namespace, module state, available plots/terms"),
          tags$td("Builds GO UI, updates tree/plot selectors, and propagates integration options."),
          tags$td("Unavailable data degrades to placeholder tree/empty selectors.")
        ),
        tags$tr(
          tags$td("Data readiness and metadata discovery"),
          tags$td(code("check_go_data_readiness()"), tags$br(), code("check_pairing_prerequisites()"), tags$br(), code("get_gene_identifier_choices()"), tags$br(), code("get_column_choices_by_content()"), tags$br(), code("find_best_pvalue_partner()")),
          tags$td(code("rv$data_mod"), ", ", code("rv$data_def"), ", selected p-value type and abundance column"),
          tags$td("Computes valid selector choices and optional ratio/p-value pairing suggestions."),
          tags$td("Returns FALSE/empty choices/NULL partner when prerequisites are not met.")
        ),
        tags$tr(
          tags$td("Similarity and pairing helpers"),
          tags$td(code("calculate_go_column_similarity()"), tags$br(), code("calculate_go_name_similarity()"), tags$br(), code("extract_go_name_parts()"), tags$br(), code("calculate_go_string_similarity()"), tags$br(), code("detect_go_related_keywords()")),
          tags$td("Ratio and p-value column names plus optional numerator/denominator metadata"),
          tags$td("Produces similarity scores used by automatic p-value column matching."),
          tags$td("Similarity below threshold yields no auto-selection.")
        ),
        tags$tr(
          tags$td("Annotation and organism management"),
          tags$td(code("organism_to_orgdb()"), tags$br(), code("load_annotation_hub*()"), tags$br(), code("force_refresh_safe()"), tags$br(), code("update_available_organisms_safe()"), tags$br(), code("update_organisms_with_fresh_cache()")),
          tags$td("Selected organism, cache age configuration"),
          tags$td("Returns OrgDb object/list, key types, and organism choice sets."),
          tags$td("Cache-miss or remote lookup errors fall back to safe retries or informative failure states.")
        ),
        tags$tr(
          tags$td("GO enrichment pipeline"),
          tags$td(code("convert_ontology_input()"), tags$br(), code("convert_padjust_method()"), tags$br(), code("perform_go_enrichment()"), tags$br(), code("create_go_results_list_direct()")),
          tags$td("Filtered genes, universe, ontology and p-adjust settings, OrgDb/keyType"),
          tags$td("Produces enrichment result and standardized result bundle consumed by tree/plots."),
          tags$td("Errors return NULL or fallback object variants to protect downstream renderers.")
        ),
        tags$tr(
          tags$td("Tree structure and term selection"),
          tags$td(code("classify_go_domains()"), tags$br(), code("group_biological_processes()"), tags$br(), code("group_molecular_functions()"), tags$br(), code("group_cellular_components()"), tags$br(), code("group_alphabetically()"), tags$br(), code("prepare_tree_for_shiny()"), tags$br(), code("extract_selected_terms_hierarchical()"), tags$br(), code("check_node_selection_enhanced()"), tags$br(), code("get_all_leaf_terms_enhanced()"), tags$br(), code("filter_terms_by_pvalue()")),
          tags$td("Enrichment results, shinyTree input, configured max term count"),
          tags$td("Creates hierarchical display structures and bounded selected-term lists."),
          tags$td("Falls back to simplified term tree when hierarchical grouping fails.")
        ),
        tags$tr(
          tags$td("Plot generation and styling"),
          tags$td(code("get_selected_theme_go()"), tags$br(), code("smart_wrap_go_terms()"), tags$br(), code("determine_smart_pvalue_format()"), tags$br(), code("format_pvalues_smart()"), tags$br(), code("get_color_limits()"), tags$br(), code("create_go_term_color_gradient()"), tags$br(), code("apply_legend_position()"), tags$br(), code("create_go_dotplot()"), tags$br(), code("create_go_cnet_plot_fc_fixed()"), tags$br(), code("create_go_enrichment_map_fixed()"), tags$br(), code("create_go_pubmed_plot()")),
          tags$td("Result objects, selected terms, theme/colors/sizes/legend controls"),
          tags$td("Returns standardized list payload (plot, dimensions, message) for rendering/download."),
          tags$td("Plot-specific constraints (for example minimum selected-term count for enrichment maps) are enforced.")
        )
      )
    ),
    h3("UI contract summary"),
    tags$ul(
      tags$li(strong("Analysis inputs:"), " identifier column, abundance/p-value selectors, thresholds, ontology, p-adjust method, gene-set limits, key type, organism, and run trigger."),
      tags$li(strong("Selection inputs:"), " hierarchical term tree and max-term limit for plotting."),
      tags$li(strong("Plot controls:"), " plot-type selector, create-plot trigger, colors, sizes, theme, legend position, plot height."),
      tags$li(strong("Export/integration controls:"), " format, DPI, dimensions, download trigger, plot-grid label/add action.")
    ),
    h3("Output contract summary"),
    tags$ul(
      tags$li(code("GOplot_1"), ": rendered GO plot output region."),
      tags$li(code("download_info_GO"), ": textual export-dimension summary."),
      tags$li(code("rv$go_results / GO_Result_List"), ": shared analysis object used by plot and integration workflows."),
      tags$li(code("output$analysis_status"), ": state channel for idle/running/error visualization.")
    )
  )
}

render_GO_tech_dataproc_content_GO <- function() {
  div(
    h2("Data Processing"),
    hr(),
    h3("Pipeline stages"),
    tags$table(
      class = "table table-sm table-striped",
      tags$thead(
        tags$tr(
          tags$th("Stage"),
          tags$th("Purpose"),
          tags$th("Key functions"),
          tags$th("Primary inputs"),
          tags$th("Primary outputs")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("1. Validate and discover columns"),
          tags$td("Ensure GO prerequisites and derive selectable identifier/abundance/p-value columns from metadata tags."),
          tags$td(code("check_go_data_readiness()"), tags$br(), code("check_pairing_prerequisites()"), tags$br(), code("get_gene_identifier_choices()"), tags$br(), code("get_column_choices_by_content()"), tags$br(), code("find_best_pvalue_partner()")),
          tags$td(code("rv$data_mod"), ", ", code("rv$data_def")),
          tags$td("Validated selectors and optional auto-paired p-value column.")
        ),
        tags$tr(
          tags$td("2. Build filtered GO input"),
          tags$td("Construct gene/abundance/p-value table and apply abundance + p-value thresholds."),
          tags$td("Server-side filtering workflow"),
          tags$td("Selected data columns and user thresholds"),
          tags$td(code("go_data"), " (filtered), ", code("genes_for_GO"), ", ", code("universe_genes"))
        ),
        tags$tr(
          tags$td("3. Resolve annotation context"),
          tags$td("Map organism to OrgDb, load cached or downloaded annotation/keytypes, and validate key type usage."),
          tags$td(code("organism_to_orgdb()"), tags$br(), code("load_annotation_hub_with_progress()"), tags$br(), code("load_keytypes_from_cache()")),
          tags$td("Organism and key-type UI inputs"),
          tags$td("OrgDb annotation object and available key types.")
        ),
        tags$tr(
          tags$td("4. Execute enrichment"),
          tags$td("Convert ontology/p-adjust UI labels to internal codes and run GO over-representation analysis."),
          tags$td(code("convert_ontology_input()"), tags$br(), code("convert_padjust_method()"), tags$br(), code("perform_go_enrichment()")),
          tags$td("Filtered genes, universe, annotation, ontology, method, cutoffs"),
          tags$td(code("edo"), " (enrichment result) or NULL on failure/no terms.")
        ),
        tags$tr(
          tags$td("5. Package reusable result state"),
          tags$td("Prepare safe/readable/similarity variants and fold-change payload for visual modules."),
          tags$td(code("create_go_results_list_direct()")),
          tags$td(code("edo"), ", filtered GO data, annotation context"),
          tags$td(code("rv$go_results / GO_Result_List"))
        ),
        tags$tr(
          tags$td("6. Build tree and resolve selected terms"),
          tags$td("Create hierarchical GO tree, capture selected terms, and enforce max selected-term bound for plotting."),
          tags$td(code("create_go_tree_structure()"), tags$br(), code("update_go_tree_display()"), tags$br(), code("extract_selected_terms_hierarchical()"), tags$br(), code("filter_terms_by_pvalue()")),
          tags$td("Enrichment results and tree selection input"),
          tags$td("Selected-term list ready for plot functions.")
        ),
        tags$tr(
          tags$td("7. Render selected plot"),
          tags$td("Apply shared style configuration and generate the chosen visualization."),
          tags$td(code("create_go_dotplot()"), tags$br(), code("create_go_cnet_plot_fc_fixed()"), tags$br(), code("create_go_enrichment_map_fixed()"), tags$br(), code("create_go_pubmed_plot()")),
          tags$td("Selected terms, result bundle, theme/colors/sizes/legend"),
          tags$td("Plot payload for UI display, export, and plot-grid integration.")
        )
      )
    ),
    h3("Failure handling model"),
    tags$ul(
      tags$li("Input/schema validation short-circuits analysis when required structures are missing."),
      tags$li("Enrichment, tree construction, and plotting use guarded execution with fallback behavior where feasible."),
      tags$li("When a stage fails, user-facing notifications and diagnostic messages are preferred over silent failure."),
      tags$li("Previously valid state can remain available for downstream consumers when a new run fails.")
    ),
    h3("Developer extension guidance"),
    tags$ul(
      tags$li("Preserve the result-list shape produced by ", code("create_go_results_list_direct()"), " to avoid breaking tree/plot/integration consumers."),
      tags$li("Treat term selection as a bounded contract; keep max-term filtering before plot construction."),
      tags$li("Any new plot function should accept the existing shared style contract (colors, sizes, theme, legend position) and return the standard plot payload list."),
      tags$li("When adding new metadata-driven selectors, keep compatibility with the Content-tag discovery model in ", code("rv$data_def"), ".")
    )
  )
}

render_GO_tech_integration_content_GO <- function() {
  div(
    h2("Integration Details"),
    hr(),
    h3("Required shared inputs"),
    tags$ul(
      tags$li(code("rv$data_mod"), ": processed feature table with columns referenced by GO selectors."),
      tags$li(code("rv$data_def"), ": metadata table defining Content tags, Column mapping, identifier options, and optional pairing hints (for example Numerator/Denominator).")
    ),
    h3("Metadata contract expected by GO"),
    tags$table(
      class = "table table-sm table-striped",
      tags$thead(
        tags$tr(
          tags$th("Metadata element"),
          tags$th("Required/optional"),
          tags$th("Usage in GO module")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(code("Content == 'Identifier'")),
          tags$td("Required"),
          tags$td("Builds gene-identifier choices for enrichment input mapping.")
        ),
        tags$tr(
          tags$td(code("Content == 'Abundance Ratio'")),
          tags$td("Required"),
          tags$td("Provides candidate abundance columns used for filtering and fold-change payloads.")
        ),
        tags$tr(
          tags$td(code("Content == 'Abundance Ratio p-Value' / 'Abundance Ratio Adj. p-Value'")),
          tags$td("At least one required"),
          tags$td("Provides p-value candidates for filtering and thresholding.")
        ),
        tags$tr(
          tags$td(code("Options")),
          tags$td("Required for identifiers"),
          tags$td("Maps identifier type labels (for example SYMBOL or UNIPROT) to data columns.")
        ),
        tags$tr(
          tags$td(code("Numerator / Denominator")),
          tags$td("Optional"),
          tags$td("Improves automatic ratio/p-value pairing quality.")
        )
      )
    ),
    h3("Cross-module integration surface"),
    tags$ul(
      tags$li(strong("Plot grid integration:"), " GO plot output is added to shared grid state through ", code("add_to_grid"), " with optional ", code("grid_label"), "."),
      tags$li(strong("Downstream module consumption:"), " selected GO terms and result-derived gene sets can be exposed through module output wiring (for example accessors under ", code("module_outputs$go_out"), ")."),
      tags$li(strong("Export contract:"), " GO plot download handlers use common width/height/DPI/format controls and should remain consistent with other analysis modules.")
    ),
    h3("Integration safety rules"),
    tags$ul(
      tags$li("Maintain stable input/output IDs referenced by observers and external module wiring."),
      tags$li("Keep GO result wrapper semantics stable so imported or computed states are interoperable."),
      tags$li("Avoid exposing raw intermediate objects unless consumers require them; prefer curated accessors/structures.")
    )
  )
}
