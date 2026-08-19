# ==============================================================================
# File: Documentation/pca_doc_tech.R
#
# Purpose:
#   Developer-facing technical documentation for the PCA / dimensionality
#   reduction module. Each function returns a Shiny tag list describing the
#   implementation as it exists in the current codebase.
#
# Note:
#   The orchestrator file pca_doc.R has been removed. All documentation
#   functions are defined in this file and in pca_doc_user.R. The UI layout
#   and server routing live in pca_doc_ui.R. These three files are sourced
#   into modEnv automatically by the alphabetical list.files() loop in app.R.
#
# Functions:
#   1. render_dimred_tech_overview_content()        - Architecture and state
#   2. render_dimred_tech_functions_content()       - Functions and UI contract
#   3. render_dimred_tech_data_processing_content() - Analysis/reactivity flow
#   4. render_dimred_tech_integration_content()     - Integration and API
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Technical Overview
# ------------------------------------------------------------------------------

render_dimred_tech_overview_content <- function() {
  div(
    h2("Technical Overview"),
    hr(),
    h3("Module Architecture"),
    p(
      "The PCA module is implemented as a thin orchestrator in ",
      code("modules/pca_module.R"),
      " plus seven sourced sub-files under ",
      code("modules/PCA/"),
      ". The orchestrator defines the public module wrappers and delegates all UI, state, observer, rendering, and analysis responsibilities to the sourced files."
    ),
    h4("Implementation Structure"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      paste(
        "modules/pca_module.R                     # Orchestrator: sys.source calls, modPCAUI(), modPCAServer()",
        "├── PCA/pca_module_UI.R                 # Declarative UI layout and namespaced input/output IDs",
        "├── PCA/pca_module_state.R              # init_pca_state(): module reactiveVals and result-routing helpers",
        "├── PCA/pca_module_utils.R              # Data validation, preparation, PCA/UMAP execution, export shaping",
        "├── PCA/pca_module_static.R             # Static ggplot2 scatter and scree plots",
        "├── PCA/pca_module_interactive.R        # Plotly conversion for the main scatter plot",
        "├── PCA/pca_module_server_observers.R   # Input synchronization, selection, labeling, UI state observers",
        "└── PCA/pca_module_server_pipeline.R    # Analysis eventReactive, plot rendering, plot/data export",
        sep = "\n"
      )
    ),
    h3("Responsibilities by File"),
    tags$ul(
      tags$li(strong("Orchestrator (pca_module.R):"), " initializes debug logging, constructs state via ", code("init_pca_state()"), ", registers observer/pipeline groups, pre-registers Plotly event IDs, and returns the module API."),
      tags$li(strong("UI (pca_module_UI.R):"), " defines the complete layout, including analysis controls, plotting area, labeling panels, and download/grid controls. It contains no server-side logic."),
      tags$li(strong("State (pca_module_state.R):"), " owns the module's reactiveVals plus helpers for storing and retrieving results by method/target combination."),
      tags$li(strong("Utilities (pca_module_utils.R):"), " performs data validation, metadata/sample alignment, matrix preparation, PCA/UMAP execution, axis management, plot-data assembly, and export data construction."),
      tags$li(strong("Static plotting (pca_module_static.R):"), " builds the main ggplot2 scatter plot for both sample and protein modes and the PCA-only scree plot."),
      tags$li(strong("Interactive plotting (pca_module_interactive.R):"), " recreates the main scatter plot as a Plotly object and registers the event sources consumed by server-side selection observers."),
      tags$li(strong("Observers (pca_module_server_observers.R):"), " synchronizes UI choices with metadata, manages protein/pathway imports, updates label settings, clears state, and controls collapsible UI sections."),
      tags$li(strong("Pipeline (pca_module_server_pipeline.R):"), " executes analyses on button click, renders plots and selection outputs, handles Plotly selection events, and registers export/add-to-grid behavior.")
    ),
    h3("Reactive State"),
    p("The canonical module state is the list returned by ", code("init_pca_state(input, debug_log)"), ". The following reactive containers are defined in ", code("pca_module_state.R"), " and extracted into local variables by the orchestrator:"),
    tags$ul(
      tags$li(code("analysis_results"), ": legacy reactiveVal updated by ", code("store_analysis_results()"), " alongside the method/target-specific stores. Used by rendering and export paths for backward compatibility."),
      tags$li(code("plots_ready"), ": logical flag used by conditional UI panels and render guards."),
      tags$li(code("selected_points_interactive"), ": data frame of Plotly-selected proteins in interactive mode."),
      tags$li(code("labeled_proteins"), ": legacy character vector for static plot labels."),
      tags$li(code("protein_suggestions"), ": suggested identifiers shown in the protein-selection typeahead UI."),
      tags$li(code("static_plot_obj"), " and ", code("interactive_plot_obj"), ": cached plot objects for the main scatter plot (ggplot2 and Plotly, respectively)."),
      tags$li(code("ggplot_object_PCATab"), ": legacy duplicate of the static plot handle, retained for downstream references that have not yet migrated to ", code("static_plot_obj"), "."),
      tags$li(code("scree_plot_obj"), ": cached PCA-only scree plot object."),
      tags$li(code("available_components"), ": list with ", code("x"), " and ", code("y"), " character vectors of valid axis choices exposed to the axis selectors."),
      tags$li(code("selected_items_vector_pca"), ": character vector of currently selected proteins or samples for labeling workflows."),
      tags$li(code("selected_data_pca"), ": data frame of selected item rows. Note: this reactiveVal is declared twice in the state factory; the second declaration (initialized as an empty ", code("data.frame()"), ") overwrites the first."),
      tags$li(code("selected_protein_vector_pca"), ": character vector of selected protein names used in export and selection display."),
      tags$li(code("item_label_settings_pca"), ": data frame with per-item label and dot-color settings (columns: ", code("item_id"), ", ", code("label_color"), ", ", code("dot_color"), ", ", code("use_custom_dot_color"), ")."),
      tags$li(code("sample_labeling_active_pca"), ": logical flag indicating whether sample labels are currently applied."),
      tags$li(code("sample_label_settings_pca"), ": list storing global sample label parameters (color, dot color, sizes, distances)."),
      tags$li(code("executed_method"), ": reactiveVal tracking the most recently executed analysis method."),
      tags$li(code("sample_pca_results"), ", ", code("protein_pca_results"), ", ", code("sample_umap_results"), ", ", code("protein_umap_results"), ": method/target-specific result stores. ", code("store_analysis_results()"), " dispatches to the appropriate store based on the result's method and comparison target, and also writes to the legacy ", code("analysis_results"), " reactiveVal.")
    ),
    p("In addition to reactive containers, the state factory returns three helper functions:"),
    tags$ul(
      tags$li(code("get_current_analysis_results"), ": reactive that routes reads to the method/target-specific store matching the current UI selection."),
      tags$li(code("get_all_analysis_results"), ": non-reactive snapshot function returning all populated result stores as a named list."),
      tags$li(code("store_analysis_results"), ": dispatches a result object to the correct per-type reactiveVal and to the legacy ", code("analysis_results"), " store.")
    ),
    h3("Returned Module API"),
    p("The ", code("modPCAServer()"), " return value exposes only the members actually returned by the orchestrator:"),
    tags$ul(
      tags$li(code("analysis_results"), ": reactive accessor backed by ", code("get_current_analysis_results"), "."),
      tags$li(code("plots_ready"), ": reactiveVal indicating whether a valid analysis has been stored."),
      tags$li(code("sample_pca_results"), ", ", code("protein_pca_results"), ", ", code("sample_umap_results"), ", ", code("protein_umap_results"), ": direct access to stored result reactiveVals."),
      tags$li(code("get_all_results"), ": snapshot function returning all stored result objects."),
      tags$li(code("has_any_results"), ": helper that reports whether any result store is populated."),
      tags$li(code("static_plot_obj"), ", ", code("interactive_plot_obj"), ", and ", code("scree_plot_obj"), ": plot-object handles exposed to callers."),
      tags$li(code("selected_data_pca"), " and ", code("selected_protein_vector_pca"), ": current protein-selection state."),
      tags$li(code("module_ready"), ": reactive that always returns ", code("TRUE"), "."),
      tags$li(code("module_health_check"), ": function returning the status string ", code("'PCA module operational'"), ".")
    ),
    h3("Implementation Notes"),
    tags$ul(
      tags$li("The current implementation does not define or return ", code("loadings_plot_obj"), "; technical documentation should not refer to that object."),
      tags$li("The module supports PCA and UMAP as dimensionality reduction methods."),
      tags$li("Plotly event IDs for ", code("plotly_selected-pca_plot"), " and ", code("plotly_click-pca_plot"), " are pre-registered into ", code("session$userData$plotlyShinyEventIDs"), " at startup to prevent warning messages before ", code("renderPlotly()"), " has executed."),
      tags$li("The sample-mode static plot optionally draws convex-hull polygons around condition groups when more than one condition is present. This is handled by ", code("create_convex_hull_data()"), " in ", code("pca_module_utils.R"), " and applied inside ", code("create_static_plot()"), ".")
    )
  )
}

# ------------------------------------------------------------------------------
# 2. Functions Reference
# ------------------------------------------------------------------------------

render_dimred_tech_functions_content <- function() {
  div(
    h2("Functions Reference"),
    hr(),
    h3("Orchestrator Entry Points"),
    tags$ul(
      tags$li(code("modPCAUI(id)"), ": wraps ", code("create_pca_ui(id)"), ", loads ", code("shinyjs"), ", and initializes the clipboard-ready input."),
      tags$li(code("modPCAServer(id, rv, res_GSEA = NULL, res_GO = NULL, module_outputs = NULL, debug_level = 1, modEnv)"), ": creates module state, registers observer/pipeline groups, and returns the public API described above.")
    ),
    h3("Core Utility Functions"),
    p("All utility functions reside in ", code("pca_module_utils.R"), " and have no Shiny dependency."),
    h4("Analysis preparation and execution"),
    tags$ul(
      tags$li(code("prepare_pca_analysis_data(data, metadata, selected_data_type, selected_samples, selected_identifier, debug_log)"), ": filters the requested abundance block and samples, aligns metadata, extracts the selected identifier column, and returns the prepared matrix plus aligned metadata, samples, and identifiers."),
      tags$li(code("get_pca_abundance_columns(data_def, selected_data_type, selected_samples, debug_log)"), ": resolves the set of abundance column names from the metadata/data-definition table for the chosen data type and sample selection."),
      tags$li(code("impute_missing_values(data_matrix, method, debug_log)"), ": handles missing values in the analysis matrix. Currently uses the ", code("'remove'"), " method (row removal)."),
      tags$li(code("validate_dimension_reduction_data(data, min_features, min_samples)"), ": pre-flight validation returning structured messages and a validity flag."),
      tags$li(code("perform_pca(data_matrix, params, debug_log)"), ": validates the matrix, removes constant columns, imputes missing values, centers the matrix, conditionally scales columns when ", code("params$pca_scale"), " is TRUE, and returns PCA coordinates, loadings, variance summaries, and the raw ", code("prcomp"), " result."),
      tags$li(code("perform_umap(data_matrix, params, debug_log)"), ": validates package availability and matrix dimensions, auto-adjusts ", code("n_neighbors"), " if it exceeds the sample count, removes missing/constant columns, runs ", code("umap::umap"), ", and returns a two-dimensional embedding with configuration metadata.")
    ),
    h4("Plot data assembly"),
    tags$ul(
      tags$li(code("create_plot_data(results, metadata, identifier_col, debug_log)"), ": converts analysis results into a plot-ready data frame, attaches sample conditions in sample mode and identifiers in protein mode."),
      tags$li(code("get_plot_coordinates(results, x_axis, y_axis)"), ": extracts the coordinate columns for the selected axis pair from the results object."),
      tags$li(code("get_axis_labels(results, x_axis, y_axis, debug_log)"), ": formats axis title strings, appending variance-explained percentages for PCA."),
      tags$li(code("manage_axis_choices(method, results, input)"), ": determines valid axis selections and defaults based on the current method and available components."),
      tags$li(code("extract_conditions_for_samples(metadata, sample_names, selected_data_type)"), ": maps sample names to their experimental condition from the metadata Options column."),
      tags$li(code("create_convex_hull_data(plot_data, debug_log)"), ": computes convex-hull polygon vertices for each condition group, used to draw group boundaries on sample-mode scatter plots.")
    ),
    h4("Labeling and search helpers"),
    tags$ul(
      tags$li(code("get_filter_string_pca(input_text, selected_identifier, debug_log)"), ": parses protein search input for the selection typeahead."),
      tags$li(code("get_default_colors_for_items_pca(items, comparison_target)"), ": returns default color assignments for items based on the comparison mode."),
      tags$li(code("create_pca_label_data(items_to_label, plot_data, item_settings, comparison_target)"), ": builds the label data frame consumed by ", code("apply_labels_to_pca_plot()"), "."),
      tags$li(code("apply_labels_to_pca_plot(base_plot, label_data, input, labeled_dot_size, debug_log)"), ": adds ggrepel text labels and optional per-item dot overlays to a base ggplot object.")
    ),
    h4("Export"),
    tags$ul(
      tags$li(code("prepare_export_data(results, include_loadings, debug_log)"), ": formats coordinates, variance explained, optional loadings, and parameter metadata into a named list suitable for multi-sheet Excel export."),
      tags$li(code("write_excel_multi_sheet(data_list, file_path)"), ": writes a named list of data frames to an Excel workbook with one sheet per list element.")
    ),
    h3("Plot Construction Functions"),
    tags$ul(
      tags$li(code("create_static_plot(results, plot_params, labeled_proteins, theme_name, font_sizes, enhanced_labeling, legend_position)"), " (", code("pca_module_static.R"), "): builds the main ggplot2 scatter plot for PCA or UMAP in either sample or protein comparison mode. In sample mode, optionally draws convex-hull polygons around condition groups. In protein mode, supports per-item label and dot color customization via the ", code("enhanced_labeling"), " parameter."),
      tags$li(code("create_pca_interactive_plot(results, plot_params, font_sizes, theme_name, legend_position)"), " (", code("pca_module_interactive.R"), "): rebuilds the main scatter plot as a Plotly object, sets source ", code("'pca_plot'"), ", and enables hover/click/selection events for protein-mode selection workflows."),
      tags$li(code("create_scree_plot(results, font_sizes, theme_name, debug_log)"), " (", code("pca_module_static.R"), "): returns the PCA-only scree plot with per-component variance bars, cumulative-variance overlay, and an 80% variance threshold line.")
    ),
    h3("Registered Server Functions Called by modPCAServer()"),
    tags$ul(
      tags$li(code("register_pca_input_observers()"), ": synchronizes available abundance types, identifier columns, and sample choices from ", code("rv$data_def"), "."),
      tags$li(code("register_pca_protein_selection_observers()"), ": adds/removes/clears selected proteins, updates suggestion displays, and manages protein-selection state."),
      tags$li(code("register_pca_pathway_observers()"), ": integrates GSEA/GO pathway selections into the protein-selection workflow."),
      tags$li(code("register_pca_ui_state_observers()"), ": updates axis choices from results, clears results on mode changes, and manages collapsible UI state."),
      tags$li(code("register_pca_label_management_observers()"), ": applies master/per-item color settings, copies selected proteins, and clears selection/label state."),
      tags$li(code("register_pca_analysis_core()"), ": owns the button-triggered analysis eventReactive."),
      tags$li(code("register_pca_rendering_core()"), ": owns plot rendering, Plotly selection handling, sample-label application, and plots_ready output binding."),
      tags$li(code("register_pca_export_handlers()"), ": owns download handlers and add-to-grid integration.")
    ),
    h3("Actual UI Contract"),
    p("The following namespaced IDs are defined in ", code("modules/PCA/pca_module_UI.R"), " and are part of the implemented UI contract used by the current server code."),
    h4("Data and analysis controls"),
    tags$ul(
      tags$li(code("create_plot"), ": starts analysis."),
      tags$li(code("custom_col_sel_pca"), ": selected abundance/data type."),
      tags$li(code("select_samples_pca"), ": selected samples."),
      tags$li(code("GeneIdentifierColumn_pca"), ": active identifier column."),
      tags$li(code("comparison_target"), ": ", code("samples"), " vs ", code("proteins"), "."),
      tags$li(code("analysis_method"), ": ", code("pca"), " vs ", code("umap"), "."),
      tags$li(code("axis_x"), ", ", code("axis_y"), ", ", code("show_scree"), ", and ", code("pca_scale"), ": PCA-specific controls; ", code("pca_scale"), " is restored as a checkbox/logical value."),
      tags$li(code("umap_neighbors"), ", ", code("umap_min_dist"), ", and ", code("umap_metric"), ": UMAP-specific controls.")
    ),
    h4("Plot and styling controls"),
    tags$ul(
      tags$li(code("interactive_plot"), ": toggles static vs Plotly rendering of the main scatter plot."),
      tags$li(code("plot_tabs"), ", ", code("static_plot"), ", ", code("interactive_plot_output"), ", ", code("scree_plot"), ", and ", code("plots_ready"), ": output IDs used by rendering and conditional panels."),
      tags$li(code("color_palette"), ", ", code("reverse_colors"), ", ", code("defaultProteinColor_pca"), ", ", code("plot_theme"), ", ", code("legend_position"), ", ", code("point_size"), ", ", code("AxisTitleSize_PCATab"), ", ", code("tickSize_PCATab"), ", ", code("LegendTitleSize_PCATab"), ", and ", code("LegendTextSize_PCATab"), ": plot styling controls.")
    ),
    h4("Protein selection and labeling controls"),
    tags$ul(
      tags$li(code("searchGene_pca"), ", ", code("GSEA_pca"), ", ", code("GO_pca"), ", ", code("transferButton_pca"), ", ", code("Protein_Input_pca"), ", ", code("Intersect_pca"), ", and ", code("CoreEnriched_pca"), ": protein import and pathway-selection inputs."),
      tags$li(code("geneSymbolList_pca"), ", ", code("enhanced_selectedItems_pca"), ", ", code("selected_items_list"), ", and ", code("selected_items_display"), ": protein selection displays."),
      tags$li(code("masterLabelColor_pca"), ", ", code("masterDotColor_pca"), ", ", code("masterCustomDot_pca"), ", ", code("maxOverlaps_pca"), ", ", code("labelDistance_pca"), ", ", code("lineThickness_pca"), ", ", code("labelSize_pca"), ", ", code("dotSizeLabeled_pca"), ", ", code("applySettings_pca"), ", ", code("resetColors_pca"), ", ", code("clearLabels_pca"), ", and ", code("clearSelection_pca"), ": protein-label configuration controls."),
      tags$li(code("copy_selection"), ": copies Plotly-selected proteins."),
      tags$li(code("clear_selection"), ": clears Plotly interactive selection."),
      tags$li(code("toggle_protein_controls"), ", ", code("protein_controls_icon"), ", and ", code("protein_controls_content"), ": collapsible protein-controls panel state.")
    ),
    h4("Sample-label and export/grid controls"),
    tags$ul(
      tags$li(code("masterLabelColor_samples"), ", ", code("masterDotColor_samples"), ", ", code("masterCustomDot_samples"), ", ", code("maxOverlaps_samples"), ", ", code("labelDistance_samples"), ", ", code("lineThickness_samples"), ", ", code("labelSize_samples"), ", ", code("dotSizeLabeled_samples"), ", ", code("applySampleLabeling_pca"), ", ", code("resetLabelSettings_pca"), ", ", code("clearSampleLabels_pca"), ": sample-label controls."),
      tags$li(code("resolution_DPI_PCATab"), ", ", code("plotWidthInch_PCATab"), ", ", code("plotHeightInch_PCATab"), ", ", code("downloadFormat_PCATab"), ", ", code("downloadPlotButton_PCATab"), ", ", code("downloadScreePlotButton_PCATab"), ", ", code("plot_type_grid"), ", ", code("grid_label"), ", and ", code("add_to_grid"), ": implemented plot export and grid controls."),
      tags$li(code("download_data"), ": implemented data-export handler."),
      tags$li(code("resetButton_PCATab"), ": restores the right-side control panels to their default UI values without clearing stored results."),
      tags$li(code("download_plot"), " and ", code("export_format"), ": UI elements currently declared in the layout but not backed by download handlers in the active server implementation.")
    ),
    h4("Removed from technical documentation"),
    tags$ul(
      tags$li(code("show_loadings"), ": not defined in the current UI and should not be documented."),
      tags$li(code("create_loadings_plot()"), ": not implemented in the current module and should not be referenced."),
      tags$li("Legacy loadings-specific download handlers are not part of the current server implementation.")
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Data Processing
# ------------------------------------------------------------------------------

render_dimred_tech_data_processing_content <- function() {
  div(
    h2("Data Processing and Reactivity"),
    hr(),
    h3("Developer Analysis Reference"),
    p(
      "This section is intended as a maintenance-oriented summary of the active dimensionality-reduction workflow implemented across ",
      code("modules/pca_module.R"),
      " and the sourced files under ",
      code("modules/PCA/"),
      ". It describes what the module currently does, which reactive contracts it depends on, how results move through the system, and which exported entry points must remain stable for documentation-module integration."
    ),
    h4("Core functionality"),
    tags$ul(
      tags$li(strong("PCA and UMAP:"), " the analysis pipeline only dispatches ", code("perform_pca()"), " and ", code("perform_umap()"), "; PCA additionally enables scree-plot generation, while UMAP produces only the main two-dimensional embedding."),
      tags$li(strong("Sample vs protein comparison modes:"), " ", code("input$comparison_target"), " determines whether the prepared matrix is transposed for sample comparison or left in protein orientation for protein comparison, which in turn controls point naming, metadata usage, labeling behavior, and interactive-selection eligibility."),
      tags$li(strong("Static plot, interactive plot, scree plot, and exports:"), " the module maintains a ggplot-based main scatter plot, a Plotly-based interactive scatter plot, a PCA-only scree plot, Excel data export, plot download handlers, and add-to-grid integration backed by cached plot objects.")
    ),
    h4("Inputs and outputs"),
    tags$ul(
      tags$li(strong("Required ", code("rv"), " members:"), " ", code("rv$data_mod"), " supplies the abundance/intensity table used for matrix construction, while ", code("rv$data_def"), " supplies the metadata/data-definition table used to discover available sample columns, identifier columns, and sample annotations."),
      tags$li(strong("Key UI inputs:"), " the execution path depends primarily on ", code("create_plot"), ", ", code("custom_col_sel_pca"), ", ", code("select_samples_pca"), ", ", code("GeneIdentifierColumn_pca"), ", ", code("comparison_target"), ", ", code("analysis_method"), ", ", code("axis_x"), ", ", code("axis_y"), ", ", code("interactive_plot"), ", and ", code("show_scree"), "; styling, labeling, download, and grid controls layer on top of those primary analysis selectors."),
      tags$li(strong("Public return value of ", code("modPCAServer()"), ":"), " the orchestrator returns the routed ", code("analysis_results"), " accessor, ", code("plots_ready"), ", the four method/target-specific result stores, ", code("get_all_results"), ", ", code("has_any_results"), ", cached plot-object handles, protein-selection state, ", code("module_ready"), ", and ", code("module_health_check"), " as the supported external interface.")
    ),
    h4("Reactive logic"),
    tags$ul(
      tags$li(strong("State initialization through ", code("init_pca_state()"), ":"), " ", code("modPCAServer()"), " creates the canonical ", code("pca_state"), " object once, then extracts every reactiveVal and helper from that list before registering any observers or renderers."),
      tags$li(strong("Observer registration flow in ", code("modPCAServer()"), ":"), " after state initialization, the orchestrator registers input observers, protein-selection observers, pathway observers, UI-state observers, label-management observers, the analysis core, Plotly event IDs, the rendering core, and finally export handlers, in that order."),
      tags$li(strong("Analysis execution via ", code("eventReactive(input$create_plot, ...)"), ":"), " the button-triggered reactive validates shared data, validates required user selections, prepares the analysis matrix with ", code("prepare_pca_analysis_data()"), ", dispatches to PCA or UMAP, enriches the returned object with host-app context, and marks plots as ready only after results are stored successfully."),
      tags$li(strong("Storage of results by method/target:"), " ", code("store_analysis_results()"), " writes the enriched result object into both the legacy ", code("analysis_results"), " reactiveVal and the specific store for sample PCA, protein PCA, sample UMAP, or protein UMAP; ", code("get_current_analysis_results()"), " routes reads back through the currently selected method/target pair."),
      tags$li(strong("Rendering dependencies for static, interactive, and scree outputs:"), " ", code("output$static_plot"), " depends on ", code("plots_ready()"), ", the routed current results, axis selections, labeling settings, and plot-style inputs; ", code("output$interactive_plot_output"), " depends on the same current results plus Plotly conversion via ", code("create_pca_interactive_plot()"), "; ", code("output$scree_plot"), " additionally requires PCA results and ", code("input$show_scree"), " to be true before it renders and caches the scree plot.")
    ),
    h4("Key function map"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(
        tags$tr(
          tags$th("Function"),
          tags$th("Responsibility"),
          tags$th("File")
        )
      ),
      tags$tbody(
        tags$tr(tags$td(code("modPCAServer()")), tags$td("Orchestrates state creation, registration order, Plotly event setup, and the public module return API."), tags$td(code("modules/pca_module.R"))),
        tags$tr(tags$td(code("init_pca_state()")), tags$td("Allocates reactiveVals, defines method/target result routing, and centralizes storage helpers."), tags$td(code("modules/PCA/pca_module_state.R"))),
        tags$tr(tags$td(code("register_pca_analysis_core()")), tags$td("Defines the button-triggered analysis eventReactive and data-availability guard."), tags$td(code("modules/PCA/pca_module_server_pipeline.R"))),
        tags$tr(tags$td(code("register_pca_rendering_core()")), tags$td("Defines static plot, interactive plot, scree plot, Plotly selection handling, and plot-related observer logic."), tags$td(code("modules/PCA/pca_module_server_pipeline.R"))),
        tags$tr(tags$td(code("register_pca_export_handlers()")), tags$td("Defines data export, plot download, scree download, and add-to-grid behavior."), tags$td(code("modules/PCA/pca_module_server_pipeline.R"))),
        tags$tr(tags$td(code("prepare_pca_analysis_data()")), tags$td("Builds the analysis-ready numeric matrix plus aligned metadata, samples, and identifiers."), tags$td(code("modules/PCA/pca_module_utils.R"))),
        tags$tr(tags$td(code("perform_pca() / perform_umap()")), tags$td("Execute the selected dimensionality-reduction method and return the core result object consumed downstream."), tags$td(code("modules/PCA/pca_module_utils.R"))),
        tags$tr(tags$td(code("create_plot_data() / get_plot_coordinates()")), tags$td("Convert analysis results into plot-ready data frames with coordinates, conditions, and identifiers."), tags$td(code("modules/PCA/pca_module_utils.R"))),
        tags$tr(tags$td(code("create_convex_hull_data()")), tags$td("Compute convex-hull polygon vertices per condition group for sample-mode plots."), tags$td(code("modules/PCA/pca_module_utils.R"))),
        tags$tr(tags$td(code("create_static_plot() / create_pca_interactive_plot() / create_scree_plot()")), tags$td("Construct the main static scatter plot, the Plotly scatter plot, and the PCA-only scree plot."), tags$td(code("modules/PCA/pca_module_static.R"), " / ", code("modules/PCA/pca_module_interactive.R")))
      )
    ),
    h4("Integration details"),
    tags$ul(
      tags$li("The documentation UI is mounted from ", code("R/ui.R"), " via ", code('modDimRedDocUI("pca_doc")'), ", so this technical content is part of the dimensionality-reduction documentation tab rather than the analysis module itself."),
      tags$li("The documentation server is initialized from ", code("R/server_modules.R"), " by calling ", code('.init_doc_module("modDimRedDocServer", "pca_doc")'), ", which confirms that the host application expects the documentation module to continue exposing those exported names."),
      tags$li("Because the host app wires documentation through ", code("modDimRedDocUI"), " and ", code("modDimRedDocServer"), " with the ", code("pca_doc"), " identifier, maintainers must preserve those exported names and the current document entry points when refactoring the PCA documentation module, even if the internal rendering helpers are reorganized.")
    ),
    h3("Button-Triggered Analysis Flow"),
    p(code("register_pca_analysis_core()"), " is the only place where analysis execution is triggered. The core flow is:"),
    tags$ol(
      tags$li(code("eventReactive(input$create_plot, { ... })"), " validates that ", code("rv$data_mod"), " and ", code("rv$data_def"), " are present and pass ", code("validate_dimension_reduction_data()"), "."),
      tags$li("The handler validates ", code("custom_col_sel_pca"), ", ", code("select_samples_pca"), ", and ", code("GeneIdentifierColumn_pca"), ", and requires at least two selected samples before preparation starts."),
      tags$li(code("prepare_pca_analysis_data()"), " extracts the selected abundance block, aligns the requested samples, and builds the matrix/metadata bundle used downstream."),
      tags$li("The pipeline chooses the analysis matrix based on ", code("comparison_target"), ": sample comparison transposes the prepared matrix; protein comparison uses it as-is."),
      tags$li(code("perform_pca()"), " or ", code("perform_umap()"), " is dispatched from ", code("input$analysis_method"), "."),
      tags$li("The returned result object is enriched with ", code("point_names"), ", ", code("comparison_target"), ", ", code("method"), ", prepared metadata, raw metadata, selected identifier, selected samples, selected data type, and the original full dataset."),
      tags$li("Results are stored in both the legacy ", code("analysis_results"), " reactiveVal and the method/target-specific stores via ", code("store_analysis_results()"), "; ", code("plots_ready(TRUE)"), " then enables rendering/UI output.")
    ),
    h3("Function-Level Data Responsibilities"),
    tags$ul(
      tags$li(code("prepare_pca_analysis_data()"), ": prepares a numeric analysis matrix and aligned metadata from the shared app data structures."),
      tags$li(code("perform_pca()"), ": removes zero-variance columns, handles missing values, centers the matrix, conditionally scales columns based on ", code("pca_scale"), ", and computes PCA coordinates/loadings/variance summaries."),
      tags$li(code("perform_umap()"), ": validates package availability, adjusts invalid neighborhood settings, removes incomplete/constant columns, and computes a two-dimensional UMAP embedding."),
      tags$li(code("create_plot_data()"), ": maps stored coordinates back to display names and sample conditions or protein identifiers."),
      tags$li(code("create_static_plot()"), ": constructs the main scatter plot, including condition coloring for samples and enhanced labeling for proteins/samples."),
      tags$li(code("create_pca_interactive_plot()"), ": constructs the Plotly version of the main scatter plot for interactive selection workflows."),
      tags$li(code("create_scree_plot()"), ": renders the PCA-only scree plot used in the scree tab and scree download handler."),
      tags$li(code("prepare_export_data()"), ": shapes the current results into export tables for coordinates, variance explained, loadings, and parameter metadata.")
    ),
    h3("Rendering Responsibilities"),
    p(code("register_pca_rendering_core()"), " owns all plot rendering and selection display responsibilities."),
    tags$ul(
      tags$li(code("output$static_plot"), ": builds and caches the main ggplot scatter plot via ", code("create_static_plot()"), ". In sample mode, convex-hull polygons are automatically drawn around condition groups when more than one condition is present. In protein mode, per-item label and dot colors from ", code("item_label_settings_pca"), " are applied when labeling is active."),
      tags$li(code("output$interactive_plot_output"), ": builds and caches the Plotly scatter plot via ", code("create_pca_interactive_plot()"), ". Interactive selection events (click, box, lasso) are handled only in protein comparison mode."),
      tags$li(code("output$scree_plot"), ": renders the PCA scree plot only when the current results are PCA and ", code("show_scree"), " is enabled."),
      tags$li("Additional responsibilities: sample-label application/reset/clear actions, automatic identifier-triggered re-analysis in protein mode, Plotly click/lasso selection handling, selection display outputs, and the ", code("output$plots_ready"), " reactive bridge for conditional panels.")
    ),
    h3("Export Responsibilities"),
    p(code("register_pca_export_handlers()"), " owns the implemented export and integration actions:"),
    tags$ul(
      tags$li(code("output$download_data"), ": exports analysis tables produced by ", code("prepare_export_data()"), " to a multi-sheet Excel workbook."),
      tags$li(code("output$downloadPlotButton_PCATab"), ": exports the cached main ggplot object in the selected device format."),
      tags$li(code("output$downloadScreePlotButton_PCATab"), ": regenerates and exports the scree plot for PCA results only."),
      tags$li(code("observeEvent(input$add_to_grid, ...)"), ": pushes the cached main or scree plot into ", code("modEnv$add_to_grid"), " using the selected grid label and plot type.")
    ),
    h3("Special Cases and Implemented Constraints"),
    tags$ul(
      tags$li("Analysis is blocked when required data/metadata are missing, no data type is selected, no samples are selected, fewer than two samples are selected, fewer than three observations remain for the chosen comparison target, or fewer than two features remain."),
      tags$li("Scree plotting and scree download are available only for PCA results."),
      tags$li("Protein-mode identifier changes trigger a full re-analysis because point identities change; sample-mode identifier changes update only the stored identifier reference."),
      tags$li("Interactive Plotly selection handling is implemented for protein comparison mode only."),
      tags$li("Convex-hull polygons in sample mode are drawn automatically when more than one condition is present. There is no UI toggle for this feature."),
      tags$li("Sample labeling applies labels to all displayed samples at once with global settings; there is no per-sample color customization. Protein labeling supports per-item color and dot customization.")
    )
  )
}

# ------------------------------------------------------------------------------
# 4. Integration Details
# ------------------------------------------------------------------------------

render_dimred_tech_integration_content <- function() {
  div(
    h2("Integration Details"),
    hr(),
    h3("Required Inputs from the Host App"),
    h4("Shared reactiveValues contract"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "rv <- reactiveValues(\n  data_mod = NULL,  # primary analysis data frame\n  data_def = NULL   # metadata/data-definition table used for sample and identifier mapping\n)"
    ),
    p(
      code("rv$data_mod"),
      " must contain the abundance/intensity data plus identifier columns. ",
      code("rv$data_def"),
      " must contain the metadata used to discover available abundance types, sample names, identifier columns, and sample conditions."
    ),
    h3("Module Registration"),
    h4("UI"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "tabPanel('Dimensionality Reduction',\n  modPCAUI('dimred_module')\n)"
    ),
    h4("Server"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      "pca_api <- modPCAServer(\n  'dimred_module',\n  rv = rv,\n  res_GSEA = res_GSEA,\n  res_GO = res_GO,\n  module_outputs = module_outputs,\n  debug_level = 1,\n  modEnv = modEnv\n)"
    ),
    h3("Available Return Interface"),
    p("Consumers of ", code("modPCAServer()"), " can rely on the following returned members:"),
    pre(
      style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px; font-family: monospace;",
      paste(
        "pca_api$analysis_results()",
        "pca_api$plots_ready()",
        "pca_api$sample_pca_results()",
        "pca_api$protein_pca_results()",
        "pca_api$sample_umap_results()",
        "pca_api$protein_umap_results()",
        "pca_api$get_all_results()",
        "pca_api$has_any_results()",
        "pca_api$static_plot_obj()",
        "pca_api$interactive_plot_obj()",
        "pca_api$scree_plot_obj()",
        "pca_api$selected_data_pca()",
        "pca_api$selected_protein_vector_pca()",
        "pca_api$module_ready()",
        "pca_api$module_health_check()",
        sep = "\n"
      )
    ),
    h3("Integration Notes"),
    tags$ul(
      tags$li("The host environment must provide ", code("modEnv"), " because the orchestrator sources sub-files into that environment and the export pipeline uses ", code("modEnv$add_to_grid"), "."),
      tags$li("The server pipeline uses the current UI state to route results by method and comparison target; callers should not assume a single global result object outside the returned API."),
      tags$li("Only currently implemented downloads should be exposed in downstream documentation: main plot, scree plot, and data export."),
      tags$li("Do not document loadings-specific plots or return objects unless they are added back into the implementation.")
    ),
    h3("Developer Maintenance Guidance"),
    tags$ul(
      tags$li("When adding new reactive state, update ", code("init_pca_state()"), ", the orchestrator extraction block, and the return interface together."),
      tags$li("When adding new UI IDs, verify that they are wired by one of the registration functions before documenting them as active behavior."),
      tags$li("When changing plot export behavior, keep ", code("register_pca_export_handlers()"), " and this technical document synchronized so the documentation remains implementation-aligned.")
    )
  )
}
