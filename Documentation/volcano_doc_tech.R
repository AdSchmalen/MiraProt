# ./Documentation/volcano_doc_tech.R
# Volcano technical documentation for developers

render_volcano_tech_architecture_content <- function() {
  div(
    h2("Volcano Module Architecture"),
    hr(),
    volcano_doc_callout(
      "Core functionality",
      p("The Volcano module detects abundance-ratio / p-value column pairs, creates one static ggplot volcano plot per detected comparison, stores the plot list in reactive state, and can render the selected plot as an interactive plotly scatter plot for hover-based inspection and point selection."),
      type = "info"
    ),
    h3("Module file layout"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "modules/volcano_module.R                 # orchestrator, module entry points, state wiring
modules/Volcano/volcano_reactive_state.R   # reactiveValues/reactiveVal initialization
modules/Volcano/volcano_module_UI.R        # module UI composition and control definitions
modules/Volcano/volcano_data_processing.R  # pairing, transformation handling, ranges, search parsing
modules/Volcano/volcano_plot_static.R      # ggplot creation, themes, labels, styling
modules/Volcano/volcano_plot_interactive.R # plotly conversion, hover text, interactive layout
modules/Volcano/volcano_export.R           # clipboard, download, grid helper, axis reset flags
modules/Volcano/volcano_observers.R        # coordinator for focused observer registrations
modules/Volcano/volcano_observers_data_choices.R      # data validation and UI choices
modules/Volcano/volcano_observers_protein_selection.R # pathway import and protein selection
modules/Volcano/volcano_observers_plot_lifecycle.R    # plot rendering, styling, and labeling
modules/Volcano/volcano_observers_selection_restore.R # interactive selection and restore"
    ),
    h3("Architectural responsibilities"),
    tags$ul(
      tags$li(strong("Orchestrator:"), " modVolcanoServer() creates data reactives, initializes state, pre-registers plotly event ids, and delegates reactive behaviour to register_volcano_observers()."),
      tags$li(strong("State layer:"), " init_volcano_state() creates the mutable stores used by rendering, labeling, interactive selection, and axis management."),
      tags$li(strong("Pure logic layers:"), " volcano_data_processing.R, volcano_plot_static.R, volcano_plot_interactive.R, and volcano_export.R expose helper functions that receive all required arguments explicitly."),
      tags$li(strong("Behaviour layer:"), " volcano_observers.R coordinates the focused data-choice, protein-selection, plot-lifecycle, and selection/restore observer peers."),
      tags$li(strong("UI layer:"), " volcano_module_UI.R defines the static/interactive plot switch, labeling panel, settings panels, and export controls without embedding observer logic.")
    )
  )
}

render_volcano_tech_io_content <- function() {
  div(
    h2("Inputs and Outputs"),
    hr(),

    h3("Server inputs"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "modVolcanoServer(
  id,
  rv,
  res_GSEA = NULL,
  GO_res = NULL,
  module_outputs = NULL,
  debug_level = 2,
  modEnv = new.env()
)"
    ),

    tags$ul(
      tags$li(strong("rv$data_mod:"), " analysis table consumed by both static and interactive plot generation."),
      tags$li(strong("rv$data_def:"), " metadata table with at least Column and Content; Transformation, Numerator, Denominator, and Options improve pairing and labeling behaviour."),
      tags$li(strong("res_GSEA / GO_res:"), " optional reactive wrappers supplied by the host app for pathway import in the labeling panel."),
      tags$li(strong("module_outputs / modEnv:"), " passed through for integration with sibling modules, shared helpers, and Plot Grid forwarding.")
    ),

    h3("Metadata assumptions"),
    tags$ul(
      tags$li("Content == 'Abundance Ratio' identifies x-axis source columns."),
      tags$li("Content matching adjusted or raw abundance-ratio p-values identifies y-axis candidates."),
      tags$li("Content == 'Identifier' provides selectable identifier columns for search, hover, and labels."),
      tags$li("Options is used to label identifier choices in the UI; the raw column name remains the selected value used for lookup."),
      tags$li("Transformation is interpreted per source column and may contain none, log2, log10, -log2, or -log10.")
    ),

    h3("Reactive state and internal stores"),
    tags$ul(
      tags$li(strong("volcano_state$plot_titles:"), " plot titles shown in the selector and returned to the host app."),
      tags$li(strong("volcano_state$static_plots:"), " named list of ggplot objects by comparison title."),
      tags$li(strong("volcano_state$current_plotly_data:"), " enriched point data cached for plotly click and selection handlers."),
      tags$li(strong("volcano_state$current_pairs:"), " stored pairing result reused by interactive rendering so the selected plotly view matches the static plot list."),
      tags$li(strong("volcano_state$selected_genes:"), " raw identifier strings from the search textarea."),
      tags$li(strong("volcano_state$auto_range_set / manual_axis_override / auto_axis_update_in_progress:"), " internal flags controlling auto-range initialization and manual axis overrides."),
      tags$li(strong("volcano_state$label_storage:"), " legacy placeholder label structure initialized during state setup but not the primary source of label rendering."),
      tags$li(strong("selected_data_Volcano / selected_protein_vector_Volcano:"), " static labeling selection data and identifier vector."),
      tags$li(strong("volcano_labels / protein_label_settings:"), " per-plot label data and per-protein style overrides."),
      tags$li(strong("selected_points_interactive_Volcano:"), " active interactive selection shown in the interactive selection panel.")
    ),

    h3("Module outputs"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "list(
  plots = reactive(volcano_state$static_plots),
  selected_data = reactive(volcano_state$selected_genes),
  plot_titles = reactive(volcano_state$plot_titles)
)"
    ),

    h3("Important contract note"),
    tags$p("The returned module contract is intentionally lightweight and explicit:"),
    tags$ul(
      tags$li(strong("plots:"), " named ggplot list keyed by generated comparison title."),
      tags$li(strong("selected_data:"), " search-derived identifier tokens produced by textarea parsing."),
      tags$li(strong("plot_titles:"), " generated comparison titles used for plot selection and naming.")
    ),

    tags$p("Not exported from this contract:"),
    tags$ul(
      tags$li(code("selected_points_interactive_Volcano"), " (interactive selection state)."),
      tags$li(code("volcano_labels"), " (enriched per-plot label tables)."),
      tags$li(code("volcano_state$current_pairs"), " (full pairing object used for plot generation).")
    ),

    tags$p(
      "Integration implication: downstream consumers must not assume ",
      code("selected_data"),
      " is a filtered analysis table; it is identifier-token input derived from the search textarea."
    )
  )
}

render_volcano_tech_reactivity_content <- function() {
  div(
    h2("Reactive Logic and Dependencies"),
    hr(),
    h3("Primary reactive flow"),
    tags$ol(
      tags$li(strong("Data access:"), " data_in() and data_def_in() wrap rv$data_mod and rv$data_def."),
      tags$li(strong("Choice synchronization:"), " observeEvent(data_def_in(), ...) and observeEvent(data_in(), ...) refresh identifier choices and plot-selector choices, and generate_plot_titles_robust() precomputes candidate titles from metadata."),
      tags$li(strong("Plot creation:"), " observeEvent(input$update_Volcano, ...) resets axis flags, computes optimal ranges, and then calls the internal helper generateVolcanoPlots_fixed()."),
      tags$li(strong("Static rendering:"), " output$volcanoPlot re-renders whenever plot_update_trigger() changes and resolves the active plot through get_current_display_plot()."),
      tags$li(strong("Interactive rendering:"), " output$volcanoPlotly requires stored static plots, PlotSelect_Volcano, and the interactive checkbox, then calls prepare_interactive_plot_data(), create_plotly_volcano(), configure_plotly_layout(), and add_plotly_interactivity()."),
      tags$li(strong("Selection handling:"), " plotly_selected and plotly_click events update selected_points_interactive_Volcano()."),
      tags$li(strong("Label application:"), " input$applySettings_Volcano builds label data for the selected static plot, stores it in volcano_labels(), and increments plot_update_trigger().")
    ),
    h3("Dependency patterns"),
    tags$ul(
      tags$li(strong("Style-only updates:"), " changes to colours, point sizes, theme, title, text sizes, and axis display settings call trigger_live_update(), which restyles stored ggplot objects without rebuilding the underlying transformed data."),
      tags$li(strong("Data-affecting updates:"), " changes to p-value type, p-value threshold, or fold-change threshold call trigger_data_update(), which regenerates stored plots from the pairing result in reactive state."),
      tags$li(strong("Axis auto-range:"), " after Create Plot, axis limits and tick spacing are computed from calculate_optimal_ranges(); the reset observer recalculates them again when data are available."),
      tags$li(strong("Plot-selection axis reset:"), " changing PlotSelect_Volcano resets the internal auto-axis mode so a newly selected comparison can re-enter the range calculation workflow."),
      tags$li(strong("Identifier choices:"), " update_ui_choices() reacts to metadata and populates Identifier_Volcano from Content == 'Identifier'.")
    ),
    h3("Selection model"),
    tags$ul(
      tags$li("Search textarea input updates volcano_state$selected_genes through observeEvent(input$searchGene_Volcano, ...)."),
      tags$li("transferButton_Volcano adds only exact identifier matches from the search box to selected_data_Volcano() and selected_protein_vector_Volcano()."),
      tags$li("Protein_Input_Volcano imports pathway-derived proteins into the search textarea first; the user must still add exact matches into the labeling selection."),
      tags$li("Interactive plotly selection populates selected_points_interactive_Volcano() only."),
      tags$li("There is no automatic reactive bridge from selected_points_interactive_Volcano() into selected_protein_vector_Volcano().")
    )
  )
}

render_volcano_tech_functions_content <- function() {
  div(
    h2("Key Functions and Responsibilities"),
    hr(),
    h3("Data processing"),
    tags$ul(
      tags$li(code("find_ratio_pvalue_pairs_smart()"), " orchestrates metadata-first pairing with pattern-based fallback."),
      tags$li(code("find_pairs_by_metadata()"), ", ", code("group_by_numerator_denominator()"), ", and ", code("create_pairs_within_group()"), " implement metadata-driven matching."),
      tags$li(code("find_pairs_by_pattern()"), " and supporting similarity helpers resolve pairings when metadata are incomplete or ambiguous."),
      tags$li(code("prepare_volcano_plot_data_safe()"), " is the canonical data-preparation path for transformed ratio and p-value columns."),
      tags$li(code("calculate_optimal_ranges()"), " computes dataset-level x/y ranges and tick spacing used during initial plot generation and reset."),
      tags$li(code("get_filter_string_Volcano()"), " parses the search textarea into one identifier token per entered line for matching and transfer workflows.")
    ),
    h3("Static plotting"),
    tags$ul(
      tags$li(code("extract_plot_parameters_safe()"), " captures the active UI state for plot construction or live restyling."),
      tags$li(code("create_single_volcano_plot_safe()"), " creates one ggplot volcano figure from a specific ratio/p-value pair."),
      tags$li(code("apply_live_styling_to_plot()"), " updates appearance without re-running data preparation."),
      tags$li(code("apply_all_labels_to_plot_enhanced()"), " overlays the stored label table for the selected plot."),
      tags$li(code("create_volcano_label_data_enhanced()"), " builds plot-space label rows from the selected protein list."),
      tags$li(code("get_default_dot_colors_for_proteins()"), " maps selected proteins to default colours based on the active thresholds.")
    ),
    h3("Interactive plotting"),
    tags$ul(
      tags$li(code("prepare_interactive_plot_data()"), " enriches canonical plot data with identifiers, categories, and hover text."),
      tags$li(code("create_plotly_volcano()"), " creates separate plotly traces for neutral, upregulated, and downregulated points."),
      tags$li(code("create_hover_text()"), " reconstructs raw p-values from the plotted y coordinate and formats the hover label."),
      tags$li(code("configure_plotly_layout()"), " applies titles, axes, theme styling, legend placement, and drag mode."),
      tags$li(code("add_plotly_threshold_lines()"), " adds the active fold-change and p-value guide lines in plotly space."),
      tags$li(code("add_plotly_interactivity()"), " registers plotly events for selection and click handling.")
    ),
    h3("Observer-layer helpers inside register_volcano_observers()"),
    tags$ul(
      tags$li(code("generateVolcanoPlots_fixed()"), " loops over detected pairs, stores the pairing result in volcano_state$current_pairs, builds one plot per pair, and names the list by comparison title."),
      tags$li(code("get_current_display_plot()"), " resolves the selected ggplot and reapplies stored labels when necessary."),
      tags$li(code("trigger_live_update()"), " restyles all stored plots after appearance changes."),
      tags$li(code("trigger_data_update()"), " rebuilds stored plots after threshold or p-value-type changes."),
      tags$li(code("update_ui_choices()"), " refreshes identifier and plot-selector choices from metadata and plot state."),
      tags$li(code("store_original_plots()"), " snapshots the active plot list before labels are applied so later label workflows retain an unlabeled base plot reference.")
    ),
    h3("Export and integration helpers"),
    tags$ul(
      tags$li(code("copy_to_clipboard()"), " sends newline-separated identifiers to the browser via JavaScript with a fallback path."),
      tags$li(code("save_volcano_plot()"), " wraps ggsave() for downloadHandler output."),
      tags$li(code("add_volcano_to_grid()"), " validates the selected ggplot and forwards it to the shared grid helper."),
      tags$li(code("reset_volcano_axis_settings()"), " resets internal auto-range flags, while the complete UI reset path is implemented in volcano_observers.R.")
    )
  )
}

render_volcano_tech_integration_content <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Host application integration"),
    tags$ul(
      tags$li("The analysis tab is mounted from ", code("R/ui.R"), " with ", code('modEnv$modVolcanoUI("volcano")'), "."),
      tags$li("The server registration occurs in ", code("R/server_modules.R"), " through ", code('.init_integration_module("Volcano Plot", modEnv$modVolcanoServer, "volcano", ...)'), "."),
      tags$li("The documentation tab is mounted from ", code("R/ui.R"), " with ", code('modEnv$modVolcanoDocUI("volcano_doc")'), ", so the exported documentation-module names must remain stable."),
      tags$li("The documentation server is initialized through ", code('.init_doc_module("modVolcanoDocServer", "volcano_doc")'), " in ", code("R/server_modules.R"), ".")
    ),

    h3("Returned module contract"),
    tags$ul(
      tags$li("The host application stores the server return value in ", code("module_outputs$volcano_out"), "."),
      tags$li("The stable exported accessors are ", code("plots"), ", ", code("selected_data"), ", and ", code("plot_titles"), "."),
      tags$li("The name ", code("plots"), " is already used by ", code("R/export.R"), " to extract ggplot data for downstream workbook-style export helpers."),
      tags$li("The export layer does not consume a dedicated ", code("get_plot_data"), " or ", code("get_analysis_data"), " accessor from the Volcano module, so documentation must reflect the actual lightweight contract.")
    ),

    h3("Important contract note"),
    tags$ul(
      tags$li(strong("plots:"), " named ggplot list."),
      tags$li(strong("selected_data:"), " search-derived identifier tokens from textarea parsing."),
      tags$li(strong("plot_titles:"), " generated comparison titles.")
    ),

    tags$p("The following internals are not exported and should not be treated as integration contract surface:"),
    tags$ul(
      tags$li(code("selected_points_interactive_Volcano"), " (interactive selection)."),
      tags$li(code("volcano_labels"), " (enriched label tables)."),
      tags$li(code("volcano_state$current_pairs"), " (full pairing object).")
    ),

    tags$p(
      "Downstream integrations must not interpret ",
      code("selected_data"),
      " as a filtered analysis table; it only carries parsed identifier tokens from search input."
    ),

    h3("Sibling-module and shared-helper dependencies"),
    tags$ul(
      tags$li(strong("GSEA:"), " the host app passes a reactive wrapper created by ", code(".gsea_results_reactive(module_outputs)"), ". The Volcano module uses it to populate pathway choices and to extract proteins from selected enrichment results."),
      tags$li(strong("GO:"), " the host app passes a reactive wrapper created by ", code(".go_results_reactive(module_outputs)"), ". The Volcano module uses it to populate GO term choices and to extract pathway members from the returned result table."),
      tags$li(strong("Grid:"), " Plot Grid integration requires ", code("modEnv$add_to_grid(rv, id, plot, label, source)"), " and is invoked through ", code("add_volcano_to_grid()"), "."),
      tags$li(strong("Data Wizard / metadata pipeline:"), " accurate automatic pairing depends on upstream metadata assignments in ", code("rv$data_def"), ", especially ", code("Content"), ", ", code("Transformation"), ", ", code("Numerator"), ", and ", code("Denominator"), ".")
    ),

    h3("Lazy pairing and identifier UI updates"),
    tags$p(
      "Volcano metadata pairing is intentionally button-gated: metadata/data changes before ",
      code("Create Plot"),
      " only refresh the identifier dropdown from the Data Wizard identifier projection and do not run ",
      code("find_ratio_pvalue_pairs_smart()"),
      " or update ",
      code("PlotSelect_Volcano"),
      ". Plot titles and the ",
      code("Select Plot"),
      " choices are generated after the explicit plot creation action, or during session restore when cached plots are replayed."
    ),

    h3("Maintenance implications"),
    tags$ul(
      tags$li("If the host app changes module ids or documentation ids, update both the mounting code in ", code("R/ui.R"), " and the initialization calls in ", code("R/server_modules.R"), " together with this documentation."),
      tags$li("If the Volcano module return value changes, keep ", code("Documentation/volcano_doc_tech.R"), ", ", code("R/export.R"), ", and any diagnostics that reference ", code("module_outputs$volcano_out"), " synchronized."),
      tags$li("If pathway result structures change in the GSEA or GO modules, revisit the import logic in ", code("volcano_observers_protein_selection.R"), " and update both the technical and user documentation accordingly."),
      tags$li("If manual pairing becomes active in the UI, the integration contract should document how that user override interacts with metadata-driven auto-pairing and with exported plot titles.")
    ),

    h3("Architecture summary"),
    p("The technical documentation describes the multi-file module layout, the reactive selection and labeling model, the host-application wiring, and the exported Volcano contract used by downstream integration code.")
  )
}
