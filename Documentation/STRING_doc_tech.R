# ==============================================================================
# File: Documentation/STRING_doc_tech.R
#
# Purpose:
#   Developer-facing technical documentation for the STRING module.
#   This file documents architecture, function responsibilities, data flow,
#   reactive logic, integration contracts, and external dependencies.
# ==============================================================================

render_STRING_tech_overview_content_STRING <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Architecture Summary"),
    p(
      "The STRING module follows an orchestrator-and-factories pattern. ",
      code("modules/STRING_module.R"),
      " is the single public entry point and delegates implementation to focused submodules under ",
      code("modules/STRING/"),
      "."
    ),
    tags$ul(
      tags$li(strong("Orchestrator:"), " Initializes state, helpers, feature factories, and observer registration order."),
      tags$li(strong("Factory files:"), " Separate concerns across data sources, network creation, styling, interactions, and export."),
      tags$li(strong("Utility layer:"), " Stateless helper functions for parsing, layout, conversion, cluster labeling, and visNetwork synchronization."),
      tags$li(strong("Reactive state layer:"), " Centralized reactiveVals for graph objects, vis data, selections, labels, and filters.")
    ),

    h3("Implemented File Map"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      paste(
        "modules/STRING_module.R",
        "  - Public API: modSTRINGUI(), modSTRINGServer()",
        "  - Sources all sub-files and wires factories",
        "",
        "modules/STRING/STRING_ui.R",
        "  - UI layout and input/output IDs for STRING tab",
        "",
        "modules/STRING/STRING_server_reactive_state.R",
        "  - initialize_STRING_reactive_state()",
        "",
        "modules/STRING/STRING_server_helpers.R",
        "  - dropdown updates, STRING organism/species retrieval, neighbor parsing, STRING mapping with fallback, export device opener",
        "",
        "modules/STRING/STRING_server_data_sources.R",
        "  - GSEA/GO integration + protein selection workflow",
        "",
        "modules/STRING/STRING_server_network_build.R",
        "  - create_STRING observer and full network build pipeline",
        "",
        "modules/STRING/STRING_server_styling.R",
        "  - node/edge styling + label-variant logic",
        "",
        "modules/STRING/STRING_server_interactions.R",
        "  - layout updates, degree filtering, selection tracking, reset, display outputs",
        "",
        "modules/STRING/STRING_server_export_grid.R",
        "  - download handler + Plot Grid integration",
        "",
        "modules/STRING/STRING_utils.R",
        "  - conversion, layout, vis prep, parsing, cluster naming, UI sync helpers",
        sep = "\n"
      )
    ),

    h3("Core Responsibilities"),
    tags$ul(
      tags$li("Accept protein identifiers from manual input and from GO/GSEA/cluster imports."),
      tags$li("Map identifiers to STRING IDs via STRINGdb for the selected organism and optionally extend the network with neighbors."),
      tags$li("Build an igraph network, cluster it, and render an interactive visNetwork graph."),
      tags$li("Provide reversible filtering and incremental styling without rebuilding the external query."),
      tags$li("Convert interactive state to static outputs for download and Plot Grid composition.")
    ),

    h3("Reactive State Contract"),
    p("All module state is centralized in initialize_STRING_reactive_state()."),
    tags$ul(
      tags$li(code("res_STRING"), ": list(Mapped, DB, STRING_Graph, cluster)."),
      tags$li(code("test_g"), ": canonical igraph object of the active network."),
      tags$li(code("nodes / edges"), ": current visNetwork node/edge data including user styling."),
      tags$li(code("original_nodes / original_edges"), ": immutable snapshot after network creation for reversible degree filtering."),
      tags$li(code("selected_data_STRING / selected_protein_vector"), ": protein selection table state and vector used for mapping."),
      tags$li(code("selectedNodes / selectedEdges / last_selection_type"), ": UI selection state used by styling observers."),
      tags$li(code("label_alias_data / label_initial_labels"), ": label source metadata for dynamic node-label switching."),
      tags$li(code("current_min_degree"), ": active degree filter threshold.")
    ),

    h3("External Dependencies"),
    tags$ul(
      tags$li(strong("STRINGdb:"), " mapping (map), neighbor expansion (get_neighbors), and interaction retrieval (get_interactions)."),
      tags$li(strong("STRING species download files:"), " Update Organisms reads ", code("species.v<version>.txt"), " from STRING download hosts and maps displayed organism names to numeric STRING/NCBI taxonomy IDs."),
      tags$li(strong("igraph:"), " graph construction, clustering, degree computations, and static plotting."),
      tags$li(strong("visNetwork:"), " interactive graph rendering and proxy-based incremental updates."),
      tags$li(strong("dplyr:"), " row binding, joins, and summary helpers in processing steps."),
      tags$li(strong("DT:"), " selected-protein data table output."),
      tags$li(strong("RColorBrewer / grDevices:"), " cluster color palettes."),
      tags$li(strong("png + ggplot2:"), " network-to-raster conversion for Plot Grid integration.")
    )
  )
}

render_STRING_tech_functions_content_STRING <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    h3("Public Module API"),
    tags$ul(
      tags$li(code("modSTRINGUI(id)"), ": wraps create_STRING_ui(ns)."),
      tags$li(
        code("modSTRINGServer(id, rv, res_GSEA = NULL, GO_res = NULL, module_outputs = NULL, debug_level = 1)"),
        ": initializes factories, registers observers, and returns module runtime state through shared reactive values."
      )
    ),

    h3("Factory Initializers (Server Composition)"),
    tags$ul(
      tags$li(code("initialize_STRING_server_helpers"), ": helper closures for dropdown updates, STRING species list retrieval, mapping fallback, neighbor parsing, export devices."),
      tags$li(code("initialize_STRING_data_sources"), ": GSEA/GO access and selection-table workflow."),
      tags$li(code("initialize_STRING_styling"), ": node/edge style observers and label variant application."),
      tags$li(code("initialize_STRING_network_build"), ": create_STRING observer, network build and initial rendering."),
      tags$li(code("initialize_STRING_interactions"), ": layout, filter, selection, reset, and display outputs."),
      tags$li(code("initialize_STRING_export_grid"), ": download handler and add-to-grid observer.")
    ),

    h3("Key Utility Functions"),
    tags$ul(
      tags$li(code("get_filter_string_STRING(input_text, identifier_type, debug_log)"), ": line-based parser for manual protein input."),
      tags$li(code("get_string_organism_choices(version_selected)"), ": downloads and parses the STRING species file for the selected STRING version, returning UI choices named by organism and valued by taxonomy ID."),
      tags$li(code("reset_string_organism_choices()"), ": restores the organism dropdown to the built-in common-organism defaults with Homo sapiens selected."),
      tags$li(code("apply_layout_transformation(graph, layout_type, debug_log)"), ": applies FR, KK, random, circle, or star layout."),
      tags$li(code("prepare_vis_data(graph, membership_vector, ceb_color, ceb_border, node_names, debug_log)"), ": builds vis-compatible nodes and edges with default style fields."),
      tags$li(code("create_vis_network(nodes, edges, ns)"), ": configures visNetwork output and emits selection/drag events to Shiny inputs."),
      tags$li(code("convert_vis_to_igraph(nodes, edges, size_factor, debug_log)"), ": creates export-ready igraph + layout from interactive state."),
      tags$li(code("convert_string_to_ggplot(graph, nodes, edges, debug_log)"), ": wraps a rendered network image into a ggplot object for Plot Grid."),
      tags$li(code("load_string_aliases(string_db, ids, version, debug_log)"), ": loads alias table used for label-source switching."),
      tags$li(code("shape_mapping / shape_mapping_igraph / reverse_shape_mapping"), ": shape conversions between UI, visNetwork, and igraph."),
      tags$li(code("update_node_ui_controls / update_edge_ui_controls"), ": pushes selected element style values back into control widgets."),
      tags$li(code("ceb3_combined(cluster_list, debug_log)"), ": generates labeled cluster display strings in the format Cluster N: p1, p2, p3, ... (up to three proteins shown; single-member clusters must be excluded from cluster_list before calling this function). Callers should pass a display-label list (protein names as shown in the network) rather than raw node IDs so that checkbox labels match the active node label source.")
    ),

    h3("Input and Output Contract"),
    h4("Important inputs"),
    tags$ul(
      tags$li(code("identifier_type_STRING"), ": selected identifier type for mapping context."),
      tags$li(code("input_STRING"), ": manual protein text input."),
      tags$li(code("transferButton_STRING / clearButton_STRING"), ": selection table mutation actions (add all / clear all)."),
      tags$li(code("remove_protein_click"), ": custom Shiny input set by per-protein remove buttons in the Selected Proteins list; value is the protein identifier string."),
      tags$li(code("GSEA_STRING / GO_STRING / ClustersProtein_STRING"), ": pathway and cluster protein sources."),
      tags$li(code("create_STRING"), ": triggers STRING network build."),
      tags$li(code("version_STRING / score_STRING / edgetype_STRING"), ": STRING query parameters."),
      tags$li(code("organism_STRING / update_organisms_STRING"), ": selected organism taxonomy ID and action button that refreshes choices from STRING species download files."),
      tags$li(code("neighbor_count_STRING / neighbor_strategy_STRING"), ": optional graph expansion parameters."),
      tags$li(code("Layout_STRING / EdgeNum_STRING"), ": layout and structural filtering controls."),
      tags$li(code("Color_1_STRING ... EdgeType_STRING / label_variant_STRING"), ": style and label controls."),
      tags$li(code("downloadFormat_STRING / plotWidthInch_STRING / plotHeightInch_STRING / resolution_DPI_STRING"), ": download settings."),
      tags$li(code("add_to_grid"), ": Plot Grid integration action.")
    ),

    h4("Primary outputs"),
    tags$ul(
      tags$li(code("output$String_plot"), ": interactive visNetwork output."),
      tags$li(code("output$selectedGene_STRING"), ": selected protein list rendered as HTML (uiOutput / renderUI); each row includes a per-protein Remove button."),
      tags$li(code("output$geneSymbolList_STRING"), ": identifier suggestions display."),
      tags$li(code("output$downloadPlotButton_STRING"), ": exported static network file handler.")
    )
  )
}

render_STRING_tech_dataproc_content_STRING <- function() {
  div(
    h2("Data Processing"),
    hr(),

    h3("End-to-End Flow"),
    tags$ol(
      tags$li(strong("Selection assembly"), ": user enters IDs manually and/or imports from GSEA, GO, or previously detected clusters."),
      tags$li(strong("Selection table commit"), ": Add / per-protein Remove / Clear updates selected_data_STRING and selected_protein_vector."),
      tags$li(strong("STRING species selection"), ": organism_STRING stores the selected numeric STRING/NCBI taxonomy ID; Update Organisms refreshes names/IDs from the STRING species download file for the selected version."),
      tags$li(strong("STRING mapping"), ": selected_protein_vector is mapped with STRINGdb using the selected species; mapping fallback can switch to version 11.5 when needed."),
      tags$li(strong("Optional neighbor expansion"), ": additional STRING IDs are selected by strategy (most_edges or connecting_clusters)."),
      tags$li(strong("Interaction retrieval"), ": get_interactions returns edges for expanded STRING IDs."),
      tags$li(strong("Graph construction"), ": igraph object created with STRING IDs as vertex names and display labels preserved as attributes."),
      tags$li(strong("Clustering + vis preparation"), ": cluster_edge_betweenness and prepare_vis_data produce interactive node/edge tables."),
      tags$li(strong("Interactive lifecycle"), ": user applies layout, degree filter, selections, and styles via proxy updates."),
      tags$li(strong("Export"), ": current nodes/edges are converted back to igraph layout for static file rendering or Plot Grid conversion.")
    ),

    h3("Reactive Behavior and Dependencies"),
    tags$ul(
      tags$li("GSEA and GO dropdown observers refresh options whenever module outputs expose new results."),
      tags$li("Identifier options are derived from rv$data_def rows containing 'Identifier' in Content."),
      tags$li("Network build is strictly button-driven (create_STRING), avoiding expensive automatic rebuilds."),
      tags$li("Changing organism_STRING does not rebuild an existing network; the selection is applied on the next Create STRING network action."),
      tags$li("Styling observers act on selected subsets and use last_selection_type to decide node-driven vs edge-driven targets."),
      tags$li("Degree filtering always starts from original_nodes/original_edges and overlays current styles to preserve customization."),
      tags$li("Cluster checkbox selection reuses stored clustering in res_STRING()$cluster, avoiding repeated expensive clustering calls."),
      tags$li("Label variant changes re-map node labels from STRING ID, initial labels, or alias sources without rebuilding the network."),
      tags$li("Cluster checkbox labels always reflect the active node label source: cluster_section_STRING depends on cluster_labels_version(), a counter incremented only inside apply_label_variant(); node data is read with isolate() so that styling changes (color, shape, etc.) do not re-render the checkbox group and reset the selection state."),
      tags$li("Edge color can be modified for any selection target: selecting a cluster, a node, or a direct edge and then changing EdgeColor_STRING applies the chosen color to all edges connected to the selected nodes or to the directly selected edges, consistent with EdgeWidth_STRING and EdgeType_STRING behavior.")
    ),

    h3("Data Formats and Assumptions"),
    tags$ul(
      tags$li(strong("Protein input:"), " newline-delimited identifiers; first token per line is used if additional whitespace/comma text is present."),
      tags$li(strong("Selected protein state:"), " stored as a one-column data.frame and a parallel character vector."),
      tags$li(strong("STRING organism choices:"), " display scientific names in the UI and store taxonomy IDs internally; the built-in common-organism list is used if the remote STRING species file cannot be read."),
      tags$li(strong("STRING mapping result:"), " expected to include STRING_id plus the original query column."),
      tags$li(strong("Graph IDs:"), " node IDs in vis state are STRING IDs; labels can represent query symbols or alias-based alternatives."),
      tags$li(strong("Neighbor labeling:"), " neighbor nodes can be assigned aliases from STRING alias sources when available; otherwise STRING IDs are retained."),
      tags$li(strong("Export consistency:"), " download and Plot Grid conversion both derive from current interactive nodes/edges, not from an unstyled base graph.")
    ),

    h3("Failure Modes Handled in Code"),
    tags$ul(
      tags$li("No selected proteins -> notification and early return."),
      tags$li("Mapping returns no rows or missing STRING_id -> notification and abort."),
      tags$li("STRING species download unavailable or unparsable -> organism dropdown falls back to built-in common organisms and shows a warning notification."),
      tags$li("Too few interactions for meaningful graph -> notification and abort."),
      tags$li("Missing packages for grid conversion -> notification and no grid update."),
      tags$li("Export conversion failures -> notification and guarded device cleanup.")
    )
  )
}

render_STRING_tech_integration_content_STRING <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Host-App Requirements"),
    tags$ul(
      tags$li(code("rv$data_mod"), ": processed data table used for selection workflows."),
      tags$li(code("rv$data_def"), ": metadata/definition table used to detect identifier options."),
      tags$li(code("module_outputs$gsea_out"), ": optional provider of GSEA results via has_results()/get_results()."),
      tags$li(code("module_outputs$go_out"), ": optional provider of GO results via has_results()/get_results()."),
      tags$li(code("modEnv$add_to_grid"), ": required for Plot Grid integration.")
    ),

    h3("Cross-Module Data Use"),
    tags$ul(
      tags$li(strong("GSEA import:"), " pathway selections can pull proteins from core_enrichment or geneSets depending on CoreEnriched_STRING."),
      tags$li(strong("GO import:"), " selected GO terms are translated from slash-separated geneID strings."),
      tags$li(strong("Cluster reuse:"), " selected cluster labels can feed back into protein input as an additional protein source."),
      tags$li(strong("Session save/restore:"), " organism_STRING is saved as part of the STRING module UI state and restored into the dropdown; if the saved organism is not in the built-in defaults it is added back as a restored choice until the user refreshes organisms.")
    ),

    h3("Module Initialization Pattern"),
    p(
      "The module is initialized from ", code("app.R"), " through ", code("modEnv$modSTRINGServer"),
      " with support for both full integration (module_outputs + legacy wrappers) and simplified fallback signatures."
    ),

    h3("Return and Lifecycle"),
    tags$ul(
      tags$li("The module registers cleanup through cleanup_manager, not direct session$onSessionEnded calls."),
      tags$li("Cleanup clears graph, vis data, selections, labels, and selected protein state."),
      tags$li("The module exposes its runtime behavior through shared reactive state and output bindings rather than a large custom returned API object.")
    ),

    h3("Developer Notes"),
    tags$ul(
      tags$li("Keep orchestration logic in STRING_module.R thin; add behavior in the dedicated factory files."),
      tags$li("Preserve observer registration order (data sources -> styling -> build -> interactions -> export) to maintain expected dependencies."),
      tags$li("When extending label variants, update alias map generation and label_variant_STRING default-selection logic together."),
      tags$li("When adding structural filters, preserve the original_nodes/original_edges snapshot model to keep filtering reversible."),
      tags$li("When changing organism-selection behavior, keep the UI defaults, reset observer, STRING mapping species argument, and session save/restore payload in sync.")
    )
  )
}
