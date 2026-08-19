# ==============================================================================
# File: Documentation/venn_doc_tech.R
#
# Purpose:
#   Developer-facing technical documentation for the Venn / UpSet module.
#   The content in this file is aligned with the current implementation in:
#     - modules/Venn_module.R
#     - modules/venn/venn_ui.R
#     - modules/venn/venn_state.R
#     - modules/venn/venn_observer.R
#     - modules/venn/venn_observers_data_lists.R
#     - modules/venn/venn_observers_plot_interaction.R
#     - modules/venn/venn_observers_export_restore.R
#     - modules/venn/venn_logic.R
# ==============================================================================

render_tech_overview_content_venn <- function() {
  div(
    h2("Technical Overview"),
    hr(),
    h3("Module purpose"),
    p(
      "The Venn module compares user-defined protein sets and renders either a ",
      "classic Venn diagram or one of three UpSet-based views. The module also ",
      "exposes intersection data for export workflows and for cross-module use ",
      "through its returned API."
    ),
    h3("Current architecture"),
    p(
      "The implementation follows the same orchestration pattern used in other ",
      "refactored MiraProt modules: a thin entry file in ", code("modules/Venn_module.R"),
      " plus focused sub-files under ", code("modules/venn/"), "."
    ),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      paste(
        "modules/Venn_module.R           # Orchestrator: sources sub-files, defines modVennUI() / modVennServer()",
        "├── venn/venn_ui.R              # Declarative UI layout and dynamic list card builder",
        "├── venn/venn_state.R           # create_venn_state(): canonical module reactive containers",
        "├── venn/venn_observer.R        # Ordered coordinator and Phase 1 / Phase 2 pipeline",
        "├── venn/venn_observers_data_lists.R       # Dynamic lists, pathway/data-list observers",
        "├── venn/venn_observers_plot_interaction.R # Plot lifecycle, rendering, intersections, grid",
        "├── venn/venn_observers_export_restore.R   # Downloads, cleanup, session restore",
        "└── venn/venn_logic.R           # Plot helpers, data preparation, export helpers, grid conversion",
        sep = "\n"
      )
    ),
    h3("Core functionality"),
    tags$ul(
      tags$li(strong("Set comparison:"), " accepts manually entered lists and imported proteins from GSEA, GO, or sample-based extraction."),
      tags$li(strong("Plot families:"), " supports ", code("Venn"), ", ", code("UpSet"), ", ", code("UpSet with Abundances"), ", and ", code("UpSet with Abundance Ratios"), "."),
      tags$li(strong("Intersection inspection:"), " computes intersections centrally and exposes them through a dropdown, text area, Excel export, and the module return value ", code("get_intersection_list"), "."),
      tags$li(strong("Export and integration:"), " downloads plots, downloads an Excel workbook of intersections, and can push the current plot to the Plot Grid after conversion to ggplot when needed.")
    ),
    h3("Required inputs and upstream dependencies"),
    tags$ul(
      tags$li(code("rv$data_mod"), ": modified abundance table used for sample extraction and abundance/ratio UpSet variants."),
      tags$li(code("rv$data_def"), ": data-definition table used to discover abundance types, identifier columns, and sample names."),
      tags$li(code("module_outputs$gsea_out"), ": optional sibling-module contract used to populate GSEA pathway selectors and extract proteins from pathway results."),
      tags$li(code("module_outputs$go_out"), ": optional sibling-module contract used to populate GO term selectors; actual GO protein extraction currently falls back to ", code("modEnv$GO_Result_List()"), " when available."),
      tags$li(code("modEnv$add_to_grid"), ": required for Plot Grid integration."),
      tags$li("Installed packages in ", code("venn_logic.R"), ": ", code("VennDiagram"), ", ", code("ComplexUpset"), ", ", code("ggupset"), ", ", code("grid"), ", and ", code("ggplot2"), ". Some optional paths additionally require ", code("png"), " for Plot Grid conversion.")
    ),
    h3("Reactive state"),
    p("The canonical state is created once by ", code("create_venn_state()"), " and passed into ", code("register_venn_observers()"), ". The current implementation defines:"),
    tags$ul(
      tags$li(code("list_count_Venn"), ": number of active list cards; initialized to 3 and capped by the add/remove observers at 25."),
      tags$li(code("list_data_Venn"), ": persistent per-list state for names, free-text lists, colours, GSEA selections, GO selections, and sample selections."),
      tags$li(code("intersection_list"), ": named list of currently computed intersections used by the dropdown, export, and parent modules."),
      tags$li(code("num_intersections_export"), ": stores the effective intersection count for dynamic UpSet sizing and download-width updates."),
      tags$li(code("current_venn_plot"), " and ", code("current_plot_type"), ": cached plot object and its plot family for Plot Grid integration."),
      tags$li(code("plot_active"), ": render gate that prevents plot rendering before the first successful Create Plot action.")
    ),
    h3("Returned module API"),
    p(code("modVennServer()"), " currently returns the following reactive interface:"),
    tags$ul(
      tags$li(code("get_intersection_list"), ": reactive accessor returning the named intersection list."),
      tags$li(code("get_list_count"), ": reactive accessor returning the current number of active list cards."),
      tags$li(code("has_data"), ": TRUE when both ", code("rv$data_mod"), " and ", code("rv$data_def"), " are available."),
      tags$li(code("module_ready"), ": TRUE when shared data is available and at least one list slot exists."),
      tags$li(code("module_health_check"), ": reactive returning a named list with status, timestamp, list count, and whether intersections are present.")
    ),
    h3("Maintenance Notes"),
    tags$ul(
      tags$li("The observer layer uses a two-phase cache/render pipeline: ", code("venn_data_cache"), " prepares data on Create Plot, while ", code("generatePlot_Venn"), " re-renders from cached data on styling changes."),
      tags$li("Intersection lifecycle ownership is centralized in state: ", code("state$intersection_list"), " is refreshed during cache generation and then reused by the dropdown, download, and returned module API."),
      tags$li("Known implementation caveats: ", code("CoreEnriched_VENN_<i>"), " currently does not switch the GSEA extraction branch, and a ", code("Dark"), " UpSet theme exists in logic helpers but is not exposed in the UI.")
    )
  )
}

render_tech_functions_content_venn <- function() {
  div(
    h2("Functions and API"),
    hr(),
    h3("Orchestrator entry points"),
    tags$ul(
      tags$li(code("modVennUI(id)"), ": wraps ", code("create_venn_ui(ns)"), " and exports the namespaced UI."),
      tags$li(code("modVennServer(id, rv, res_GSEA = NULL, GO_res = NULL, module_outputs = NULL, debug_level = 1)"), ": creates state, registers observers, and returns the module API used by the host app and exports.")
    ),
    h3("UI contract implemented in modules/venn/venn_ui.R"),
    tags$ul(
      tags$li(strong("Primary actions:"), " ", code("create_plot_Venn"), ", ", code("add_proteins_Venn"), ", ", code("remove_proteins_Venn"), ", ", code("fill_random_proteins_Venn"), ", ", code("downloadPlot_Venn"), ", ", code("download_intersections_xlsx"), ", and ", code("add_to_grid"), "."),
      tags$li(strong("Plot selection and data context:"), " ", code("diagramType_Venn"), ", ", code("ReferenceValues_Venn"), ", and ", code("GeneIdentifierColumn_Venn"), "."),
      tags$li(strong("Venn-only options:"), " ", code("showPercentages_Venn"), ", ", code("showListTitles_Venn"), ", ", code("overlapNumberSize_Venn"), ", ", code("listTitleSize_Venn"), ", ", code("listTitleDistance_Venn"), ", ", code("catFont_Venn"), ", ", code("cat_FontStyle_Venn"), ", ", code("font_family_Venn"), ", and ", code("fontStyle_Venn"), "."),
      tags$li(strong("UpSet styling options:"), " ", code("ThemeSelect_Upset"), ", ", code("axis_title_size_Venn"), ", ", code("axis_text_size_Venn"), ", and ", code("label_text_size_Venn"), "."),
      tags$li(strong("Abundance-specific selectors:"), " ", code("data_abundance_Mean_Venn"), ", ", code("data_abundance_ratio_num_Venn"), ", and ", code("data_abundance_ratio_denom_Venn"), "."),
      tags$li(strong("Per-list dynamic controls:"), " ", code("name<i>"), ", ", code("list<i>"), ", ", code("color<i>"), ", ", code("GSEA_SELECT_<i>"), ", ", code("GO_SELECT_<i>"), ", ", code("Sample_SELECT_<i>"), ", ", code("CoreEnriched_VENN_<i>"), ", and ", code("copy_button_VENN_<i>"), ".")
    ),
    h3("Observer-layer responsibilities"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(tags$tr(tags$th("Function or block"), tags$th("Current responsibility"))),
      tags$tbody(
        tags$tr(tags$td(code("sync_list_data_Venn()")), tags$td("Copies live UI values back into persistent list state before structural list changes.")),
        tags$tr(tags$td(code("venn_data_cache <- eventReactive(input$create_plot_Venn, ...)")), tags$td("Phase 1 pipeline: collects lists, validates identifiers for data-backed UpSet variants, computes intersections, builds binary membership data, and pre-computes abundance or ratio data when needed.")),
        tags$tr(tags$td(code("generatePlot_Venn <- reactive({...})")), tags$td("Phase 2 pipeline: generates the current plot family from cached Phase 1 data and re-runs on styling changes without rebuilding the expensive data cache.")),
        tags$tr(tags$td(code("output$dynamicLists_Venn")), tags$td("Rebuilds the dynamic list-card UI from canonical state in venn_observers_data_lists.R.")),
        tags$tr(tags$td(code("observe({...}) for GSEA / GO / samples / identifiers")), tags$td("Synchronizes selector choices from shared data and sibling modules in venn_observers_data_lists.R.")),
        tags$tr(tags$td(code("observeEvent(input$create_plot_Venn, ...)")), tags$td("Activates the render gate in venn_observers_plot_interaction.R only when Phase 1 returns non-NULL cached data.")),
        tags$tr(tags$td(code("output$plotOutput_Venn")), tags$td("Renders grid grobs for Venn output and ggplot-based objects for UpSet output in venn_observers_plot_interaction.R.")),
        tags$tr(tags$td(code("observeEvent(input$add_to_grid, ...)")), tags$td("Converts the cached plot to ggplot in venn_observers_plot_interaction.R when required and forwards it to the shared Plot Grid.")),
        tags$tr(tags$td(code("downloadHandler(...)")), tags$td("Exports the current plot file and the intersection workbook from venn_observers_export_restore.R."))
      )
    ),
    h3("Logic-layer helpers in modules/venn/venn_logic.R"),
    tags$ul(
      tags$li(code("collect_input_lists()"), ": reads non-empty list text areas and returns a trimmed list-of-vectors."),
      tags$li(code("venn_get_sample_choices()"), ": resolves sample names for the selected abundance type."),
      tags$li(code("extract_sample_proteins()"), ": returns identifiers from rows with at least one non-zero, non-missing abundance in the chosen samples."),
      tags$li(code("build_venn_intersection_workbook()"), ": builds a single-sheet Excel workbook with input lists, cumulative intersections, and exclusive intersections."),
      tags$li(code("validate_identifier_match()"), ": blocks abundance-backed UpSet plots when none of the typed identifiers match the selected identifier column."),
      tags$li(code("create_venn_diagram()"), ": wraps ", code("VennDiagram::venn.diagram"), " and applies current Venn display controls."),
      tags$li(code("create_standard_upset()"), ", ", code("create_upset_with_abundances()"), ", and ", code("create_upset_with_ratios()"), ": build the three UpSet variants."),
      tags$li(code("prepare_abundance_data()"), " and ", code("prepare_ratio_data()"), ": join data-backed numeric summaries onto the binary membership table."),
      tags$li(code("save_plot_file()"), ": opens the requested graphics device and renders either a ggplot object or a Venn grob list."),
      tags$li(code("convert_venn_to_ggplot()"), ": rasterises Venn output or returns UpSet ggplots for Plot Grid compatibility.")
    ),
    h3("Implementation caveats worth preserving in documentation"),
    tags$ul(
      tags$li("Venn rendering is limited to 5 sets by ", code("create_venn_diagram()"), "; larger comparisons must use an UpSet variant."),
      tags$li("List-card count can grow well beyond 5 because UpSet views support more sets; the add-list observer caps the UI at 25 lists."),
      tags$li("The logic layer defines a ", code("Dark"), " theme in ", code("get_upset_theme()"), " and ", code("get_upset_panel_theme()"), " but the current UI does not expose that theme option."),
      tags$li("The current GSEA import path always uses the ", code("core_enrichment"), " column when it is present; the ", code("CoreEnriched_VENN_<i>"), " checkbox currently changes the log message but not the extraction branch.")
    )
  )
}

render_tech_data_processing_content_venn <- function() {
  div(
    h2("Reactive Data Flow"),
    hr(),
    h3("Reactive logic and dependencies"),
    p(
      "The current implementation is organized as a button-gated two-phase ",
      "pipeline. This is the main point that was insufficiently captured in the ",
      "older documentation."
    ),
    h4("Phase 1: input capture and data preparation"),
    tags$ol(
      tags$li(code("input$create_plot_Venn"), " triggers ", code("venn_data_cache"), "."),
      tags$li(code("collect_input_lists()"), " reads all non-empty list text areas and preserves their current names and colours."),
      tags$li("For data-backed UpSet variants, ", code("validate_identifier_match()"), " checks whether the typed identifiers exist in the selected identifier column before attempting joins."),
      tags$li("For the Venn plot family, the cache computes all non-empty intersections and stores them in ", code("state$intersection_list"), "."),
      tags$li("For UpSet families, the cache builds a binary membership matrix with one row per unique protein plus one logical column per set."),
      tags$li("For ", code("UpSet with Abundances"), " and ", code("UpSet with Abundance Ratios"), ", the cache also pre-computes numeric plot data through ", code("prepare_abundance_data()"), " or ", code("prepare_ratio_data()"), ".")
    ),
    h4("Phase 2: styling-reactive plot generation"),
    tags$ol(
      tags$li(code("generatePlot_Venn()"), " depends on the Phase 1 cache and on styling inputs such as theme, text sizes, and Venn label settings."),
      tags$li("Because the expensive data preparation already happened in Phase 1, styling changes can re-render the current plot without recomputing intersections or rejoining abundance tables."),
      tags$li("The rendered plot is cached into ", code("state$current_venn_plot"), " together with ", code("state$current_plot_type"), " for downstream download and grid workflows.")
    ),
    h4("Render gate and plot lifecycle"),
    tags$ul(
      tags$li(code("state$plot_active"), " stays FALSE until the Create Plot button produces a non-NULL cache result."),
      tags$li("This gate prevents empty placeholder plots from rendering before the user has requested a plot."),
      tags$li("The implementation comment in ", code("venn_observers_plot_interaction.R"), " explains that the historical two-click bug was fixed by using ", code("ignoreNULL = TRUE"), " instead of ", code("ignoreInit = TRUE"), " on the button-gated cache.")
    ),
    h3("Inputs and outputs"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(tags$tr(tags$th("Category"), tags$th("Current implementation details"))),
      tags$tbody(
        tags$tr(tags$td("Primary user inputs"), tags$td(code("list<i>"), ", ", code("name<i>"), ", ", code("diagramType_Venn"), ", ", code("ReferenceValues_Venn"), ", and ", code("GeneIdentifierColumn_Venn"), " control the dataset and plot family.")),
        tags$tr(tags$td("Optional imported inputs"), tags$td(code("GSEA_SELECT_<i>"), ", ", code("GO_SELECT_<i>"), ", and ", code("Sample_SELECT_<i>"), " feed proteins into each list card via the copy button observer.")),
        tags$tr(tags$td("Primary outputs"), tags$td(code("plotOutput_Venn"), ", ", code("intersection_dropdown"), ", and ", code("selectedIntersection"), " expose the current visual output and its intersection members.")),
        tags$tr(tags$td("Download outputs"), tags$td(code("downloadPlot_Venn"), " exports the current plot file; ", code("download_intersections_xlsx"), " exports a workbook assembled by ", code("build_venn_intersection_workbook()"), ".")),
        tags$tr(tags$td("Module outputs"), tags$td(code("get_intersection_list()"), ", ", code("get_list_count()"), ", ", code("has_data()"), ", ", code("module_ready()"), ", and ", code("module_health_check()"), " are the externally visible accessors returned by the server."))
      )
    ),
    h3("Key responsibilities by concern"),
    tags$ul(
      tags$li(strong("UI layer:"), " defines layout and namespaced inputs only."),
      tags$li(strong("State layer:"), " owns all persistent reactive containers."),
      tags$li(strong("Observer layer:"), " performs cross-module synchronization, list mutation, event handling, rendering, export, and cleanup."),
      tags$li(strong("Logic layer:"), " provides pure helpers for list parsing, sample extraction, numeric data preparation, plotting, sizing, export, and grid conversion."),
      tags$li(strong("Orchestrator layer:"), " wires the module together and exposes the public API.")
    ),
    h3("Review notes on outdated or missing content"),
    tags$ul(
      tags$li("Older documentation described functionality largely as a linear plot-generation flow; it now explicitly documents the split between button-gated caching and styling-driven rerendering."),
      tags$li("The current documentation now states that intersection computation happens in the observer-layer cache, not inside ", code("create_venn_diagram()"), "."),
      tags$li("The previous version understated the importance of ", code("state$current_venn_plot"), ", ", code("state$current_plot_type"), ", and ", code("plot_active"), " in the render/export lifecycle.")
    )
  )
}

render_tech_algorithms_content_venn <- function() {
  div(
    h2("Plot Logic"),
    hr(),
    h3("Venn plot logic"),
    tags$ul(
      tags$li("The Venn branch accepts only up to 5 sets. The module delegates geometry to ", code("VennDiagram::venn.diagram(..., filename = NULL)"), " and renders the returned grob list with grid graphics."),
      tags$li("Set colours are taken from the per-list colour inputs; alpha is fixed at 0.5 in the current implementation."),
      tags$li("List labels can be hidden entirely with ", code("showListTitles_Venn"), ", in which case category labels are replaced by ", code("NA"), " values and category text size is effectively zero."),
      tags$li("When percentages are enabled, the module rewrites numeric overlap labels to include a percentage based on the sum of individual list sizes, not on the size of the union. This behaviour should be documented exactly because it affects interpretation."),
      tags$li("Intersection data displayed in the dropdown is not extracted from the VennDiagram object. It is computed separately from the raw lists before plotting.")
    ),
    h3("UpSet plot logic"),
    tags$ul(
      tags$li("All UpSet branches start from a binary membership data frame with one ", code("Protein"), " column and one logical column per input set."),
      tags$li(code("create_standard_upset()"), " renders set sizes and intersection counts only."),
      tags$li(code("create_upset_with_abundances()"), " adds a boxplot and jitter layer of log2 mean abundances per intersection."),
      tags$li(code("create_upset_with_ratios()"), " adds a boxplot and jitter layer of log2 abundance ratios per intersection."),
      tags$li("Dynamic width is controlled indirectly: the plot code stores the observed intersection count in ", code("num_intersections_export"), " and the observer layer translates that into container width and download width."),
      tags$li("The helper ", code("prepare_upset_plot_data()"), " is currently a backward-compatibility no-op and should not be described as an active transformation step.")
    ),
    h3("Abundance and ratio preparation"),
    tags$ul(
      tags$li(code("prepare_abundance_data()"), " resolves the chosen identifier column, filters the selected abundance type, optionally narrows to selected samples, calculates row means, applies ", code("log2"), ", and merges the result onto the membership table by identifier."),
      tags$li(code("prepare_ratio_data()"), " resolves numerator and denominator sample groups inside the selected abundance type, computes mean numerator and denominator intensities, calculates ", code("log2(num_mean / denom_mean)"), ", and drops non-finite values before plotting."),
      tags$li("Both helpers depend on the selected identifier column matching the identifiers typed into the list cards. The validation step before data preparation prevents fully mismatched joins but still allows partial matches."),
      tags$li("Rows with all-missing values can generate ", code("NaN"), "; the ratio helper explicitly removes non-finite values before plotting to avoid repeated ggplot warnings.")
    ),
    h3("Export logic"),
    tags$ul(
      tags$li(code("build_venn_intersection_workbook()"), " writes a single worksheet called ", code("Venn_Export"), " with three vertically stacked sections: input lists, cumulative intersections, and exclusive intersections."),
      tags$li(code("save_plot_file()"), " supports ", code("png"), ", ", code("jpeg"), ", ", code("tiff"), ", ", code("svg"), ", and ", code("pdf"), "; ggplot objects are printed directly, while Venn grob lists are rendered through grid."),
      tags$li(code("convert_venn_to_ggplot()"), " rasterises non-ggplot output to a temporary PNG before wrapping it in a ggplot raster layer for the Grid module.")
    )
  )
}

render_tech_integration_content_venn <- function() {
  div(
    h2("Integration Details"),
    hr(),
    h3("Host application integration"),
    tags$ul(
      tags$li("The analysis tab is mounted from ", code("R/ui.R"), " with ", code('modEnv$modVennUI("venn")'), "."),
      tags$li("The server registration occurs in ", code("R/server_modules.R"), " through ", code(".init_integration_module(..., modEnv$modVennServer, 'venn', ...)"), "."),
      tags$li("The documentation tab is mounted from ", code("R/ui.R"), " with ", code('modEnv$modVennDocUI("venn_doc")'), ", so the exported documentation-module names must remain stable."),
      tags$li("The documentation server is initialized through ", code(".init_doc_module('modVennDocServer', 'venn_doc')"), " in ", code("R/server_modules.R"), ".")
    ),
    h3("Downstream export integration"),
    tags$ul(
      tags$li("The Excel export pipeline in ", code("R/export.R"), " reads ", code("module_outputs$venn_out$get_intersection_list()"), " when available and converts the named list into the workbook sheet for Venn overlaps."),
      tags$li("This means the name ", code("get_intersection_list"), " is part of the effective cross-module contract and should not be changed casually."),
      tags$li("The health and readiness reactives are diagnostic conveniences for the host app but are not currently used as deeply as ", code("get_intersection_list"), ".")
    ),
    h3("Sibling-module dependencies"),
    tags$ul(
      tags$li(strong("GSEA"), ": the module expects a sibling output object with at least ", code("has_results()"), " and ", code("get_results()"), " to populate pathway choices and extract core-enrichment proteins."),
      tags$li(strong("GO"), ": the module uses ", code("module_outputs$go_out"), " to populate term choices, but protein extraction currently relies on the global ", code("modEnv$GO_Result_List()"), " fallback for actual term membership access."),
      tags$li(strong("GO integration contract risk"), ": GO dropdown choices are sourced from ", code("module_outputs$go_out"), ", while GO protein extraction in the copy-button observer currently falls back to ", code("modEnv$GO_Result_List()"), ". This split contract is fragile during refactors; see ", code("modules/venn/venn_observers_data_lists.R"), " (GO extraction branch in ", code("observeEvent(input[[paste0('copy_button_VENN_', i)]], ...)"), ")."),
      tags$li(strong("Grid"), ": Plot Grid integration requires ", code("modEnv$add_to_grid(rv, id, plot, label, source)"), ".")
    ),
    h3("Maintenance implications"),
    tags$ul(
      tags$li("If the GO module changes how it publishes result membership, the Venn GO copy workflow in ", code("venn_observers_data_lists.R"), " must be updated alongside this documentation."),
      tags$li("If new plot families are added, update both the UI choices in ", code("venn_ui.R"), " and the Phase 1 / Phase 2 routing in ", code("venn_observer.R"), " before documenting them here."),
      tags$li("If the module API changes, keep the documentation synchronized with ", code("modules/Venn_module.R"), " and ", code("R/export.R"), " to avoid breaking workbook export expectations.")
    ),
    h3("Review result"),
    p(
      "The technical documentation has been updated to match the current code ",
      "layout, observer pipeline, state model, and integration contracts. The ",
      "most important corrections were the removal of stale one-file mental ",
      "models, the addition of the real returned API, and the clarification of ",
      "how intersections and data-backed UpSet variants are prepared."
    )
  )
}
