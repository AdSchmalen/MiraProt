############
# Technical Documentation — Content

render_dotplot_tech_overview_content <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Module Architecture"),
    p(
      "The Dot Plot module is organized as an orchestrator plus focused server submodules. ",
      "It supports exploratory scatter plotting of two selected columns, optional axis transformations, ",
      "threshold-driven region styling, interactive selection, and static labeling/export workflows."
    ),

    h4("Implementation Structure"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      paste(
        "modules/dotplot_module.R                   # orchestrator: UI wrapper + server wiring",
        "├── dot/dotplot_UI.R                      # all Dot Plot UI controls and layout",
        "├── dot/dotplot_utils.R                   # core transforms and plotting helpers",
        "├── dot/dotplot_label_utils.R             # label parsing and styling helpers",
        "├── dot/dotplot_range_utils.R             # axis range and tick helpers",
        "├── dot/dotplot_interactive_utils.R       # interactive selection helpers",
        "├── dot/dotplot_reactive_state.R          # shared reactive state factory",
        "├── dot/dotplot_server_config.R           # axes, labels, transforms, presets, ranges/ticks",
        "├── dot/dotplot_server_plot.R             # plot generation, threshold CRUD, rendering, download/grid",
        "├── dot/dotplot_server_interaction.R      # plotly selection/click, clipboard, pathway import",
        "├── dot/dotplot_server_labeling.R         # labeling controls and label application",
        "└── dot/dotplot_server_regions.R          # region matrix UI and region-style observers",
        sep = "\n"
      )
    ),

    h3("Core Responsibilities"),
    tags$ul(
      tags$li(strong("Orchestration:"), " ", code("modDotPlotServer()"), " initializes state and registers all observer groups."),
      tags$li(strong("Data-to-plot pipeline:"), " columns are selected, transformed, filtered to finite values, then rendered via ggplot."),
      tags$li(strong("Threshold system:"), " threshold rows are added/edited/removed in a DT table and rendered as horizontal/vertical reference lines."),
      tags$li(strong("Region styling:"), " thresholds define a region matrix; each region can override point color, size, alpha, and shape."),
      tags$li(strong("Interactive selection:"), " Plotly click/box/lasso selections are captured and can be copied/cleared."),
      tags$li(strong("Labeling workflow:"), " selected identifiers are labeled on the static plot with master and per-item style controls."),
      tags$li(strong("Export/integration:"), " static plot download and Add-to-Grid are exposed in the module UI.")
    ),

    h3("Reactive State"),
    p("Shared state is created by ", code("dotplot_init_reactive_state(rv, dotplot_debug_log)"), ". Important containers include:"),
    tags$ul(
      tags$li(code("dotplot_state$current_plot"), ": cached ggplot object used for render and export."),
      tags$li(code("dotplot_state$axis_config"), ": selected X/Y columns, labels, and transformation settings."),
      tags$li(code("dotplot_state$thresholds"), ": list of threshold definitions (type/value/style/label)."),
      tags$li(code("dotplot_state$plot_ready"), ": indicates whether a plot has been generated."),
      tags$li(code("region_structure()"), ": derived matrix specification from vertical/horizontal thresholds."),
      tags$li(code("region_configs()"), ": per-region visual overrides."),
      tags$li(code("selected_points_interactive_dot()"), ": interactive selections from Plotly events."),
      tags$li(code("selected_protein_vector_dot()"), ": identifiers selected for labeling controls."),
      tags$li(code("dot_protein_labels()"), ": active label data applied to static plot."),
      tags$li(code("dot_plot_parameters()"), ": cached parameter snapshot for dynamic range labels/range control."),
      tags$li(code("plot_update_trigger()"), ": counter used for explicit live refreshes after threshold/region changes.")
    ),

    h3("Data Flow"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
      "rv$data_mod + rv$data_def\n  ↓\nAxis/identifier selection, transform choice, style and threshold inputs\n  ↓\nCreate/refresh static ggplot via dotplot_create_complete_plot_with_regions_enhanced(...)\n  ↓\nOptional plotly conversion via create_enhanced_interactive_plot(...)\n  ↓\nOptional labeling pass via apply_labels_to_dot_plot_enhanced_FIXED(...)\n  ↓\nDownload handler / Add-to-Grid integration"
    ),

    h3("Dependencies"),
    tags$ul(
      tags$li(strong("shiny"), " (module framework/reactivity)"),
      tags$li(strong("ggplot2"), " (static plot assembly and themes)"),
      tags$li(strong("plotly"), " (interactive rendering and selection events)"),
      tags$li(strong("DT"), " (threshold table UI)"),
      tags$li(strong("shinyjs"), " (clipboard and UI helpers)"),
      tags$li(strong("colourpicker"), " (color controls)")
    )
  )
}

render_dotplot_tech_functions_content <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    h3("Public Module Entry Points"),
    tags$ul(
      tags$li(code("modDotPlotUI(id)"), ": wraps ", code("dotplot_UI(ns)"), ", loads shinyjs, and emits startup-ready input signal."),
      tags$li(code("modDotPlotServer(id, rv, res_GSEA = NULL, res_GO = NULL, module_outputs = NULL, debug_level = 2, modEnv = new.env())"), ": creates shared state and registers config/plot/interaction/labeling/region observers.")
    ),

    h3("Key UI Builders"),
    tags$ul(
      tags$li(code("dotplot_UI(ns)"), ": top-level layout that combines plot panel and configuration panel."),
      tags$li(code("dotplot_config_panel(ns)"), ": Create Plot button, presets, axis/data transforms, threshold controls, and region styling."),
      tags$li(code("dotplot_plot_panel(ns)"), ": static/interactive output, protein labeling controls, download, and Add-to-Grid."),
      tags$li(code("dotplot_protein_labeling_panel(ns)"), ": search, pathway import, selected items, and label styling controls.")
    ),

    h3("Server Observer Groups"),
    tags$ul(
      tags$li(code("dotplot_init_config_observers(...)"), ": axis/identifier updates, preset application (Volcano/MA), range labels, range reset, tick controls."),
      tags$li(code("dotplot_init_plot_observers(...)"), ": threshold modal flow (add/edit/remove), plot creation, static/interactive renders, download/grid handlers."),
      tags$li(code("dotplot_init_interaction_observers(...)"), ": plotly click/lasso/box selection, clipboard copy, clear selection, text search and pathway import wiring."),
      tags$li(code("dotplot_init_labeling_observers(...)"), ": per-item color UI, master controls, apply/reset/clear label behavior."),
      tags$li(code("dotplot_init_region_observers(...)"), ": region matrix rendering, per-region style updates, reset styling, automatic remap on structure changes.")
    ),

    h3("Core Utility Functions"),
    tags$ul(
      tags$li(code("dotplot_apply_transform_safe(data_vector, transform_type, axis_name)"), ": safe axis transform function for raw/log2/log10/-log10 with guardrails for non-positive values and finite-value checks."),
      tags$li(code("dotplot_assign_points_to_regions(plot_data, region_structure)"), ": region assignment based on threshold-defined bins."),
      tags$li(code("create_enhanced_interactive_plot(ggplot_obj, data, axis_config, input)"), ": Plotly conversion that carries identifier-oriented hover and selection support."),
      tags$li(code("dotplot_create_complete_plot_with_regions_enhanced(...)"), ": high-level static plot constructor used in main generation and region refresh paths."),
      tags$li(code("dotplot_extract_plot_parameters(input)"), ": collects title/theme/text/tick/point/range settings from UI controls.")
    ),

    h3("Returned Module API"),
    p("The return object of ", code("modDotPlotServer()"), " exposes:"),
    tags$ul(
      tags$li(code("get_plot"), ": reactive accessor to cached static plot."),
      tags$li(code("get_config"), ": reactive accessor to axis configuration."),
      tags$li(code("get_thresholds"), ": reactive accessor to threshold list."),
      tags$li(code("has_plot"), ": reactive logical for plot availability."),
      tags$li(code("clear_plot()"), ": helper that clears the plot cache and readiness flag.")
    )
  )
}

render_dotplot_tech_dataproc_content <- function() {
  div(
    h2("Data Processing and Reactivity"),
    hr(),

    h3("Inputs and Outputs"),
    tags$ul(
      tags$li(strong("Required shared inputs:"), " ", code("rv$data_mod"), " (main data table) and ", code("rv$data_def"), " (column metadata)."),
      tags$li(strong("Primary user inputs:"), " axis columns/labels, transformations, theme/text/point settings, thresholds, region styles, interactive toggle, labeling controls, export controls."),
      tags$li(strong("Primary outputs:"), " static plot (", code("dotplot_main"), "), interactive plot (", code("dotplot_interactive"), "), threshold table, selection displays, and download/grid actions.")
    ),

    h3("Plot Generation Pipeline"),
    tags$ol(
      tags$li(strong("Configuration validation:"), " selected axis columns are checked before plotting (", code("dotplot_validate_config()"), ")."),
      tags$li(strong("Parameter extraction:"), " plotting parameters are read from UI (", code("dotplot_extract_plot_parameters()"), ")."),
      tags$li(strong("Transform and filter:"), " X/Y vectors are transformed safely and restricted to valid finite rows."),
      tags$li(strong("Static plot assembly:"), " ggplot is created with selected theme, title/text sizes, axis ranges, tick intervals, and point aesthetics."),
      tags$li(strong("Threshold overlay:"), " threshold lines and optional threshold labels are drawn in transformed axis space."),
      tags$li(strong("Region overlay:"), " if thresholds exist, region assignments and per-region styles are layered on top of global point style."),
      tags$li(strong("Optional interactive rendering:"), " when enabled, static ggplot is converted to Plotly with source = ", code("'dotplot_plot'"), "."),
      tags$li(strong("Optional labeling pass:"), " label data are generated from selected identifiers and applied to a clean base plot.")
    ),

    h3("Reactive Dependencies"),
    tags$ul(
      tags$li(strong("Button-triggered base generation:"), " ", code("input$generate_plot"), " is the primary generation event."),
      tags$li(strong("Live refresh trigger:"), " threshold/region updates increment ", code("plot_update_trigger()"), " when a plot already exists."),
      tags$li(strong("Auto-range logic:"), " axis/transformation changes can queue range recalculation after a plot is present."),
      tags$li(strong("Identifier consistency:"), " identifier-column changes reset selections to avoid stale label names."),
      tags$li(strong("Plotly events:"), " ", code("event_data('plotly_selected', source = 'dotplot_plot')"), " and ", code("event_data('plotly_click', source = 'dotplot_plot')"), " feed the interactive selection state.")
    ),

    h3("Threshold and Region Data Model"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "threshold <- list(\n  id         = 'thresh_1',\n  type       = 'vertical' | 'horizontal',\n  value      = numeric,\n  color      = '#RRGGBB',\n  style      = 'solid' | 'dashed' | 'dotted',\n  thickness  = numeric,\n  label      = 'optional text',\n  label_size = numeric\n)"
    ),
    p("Region structure is computed from sorted threshold values. The implementation keeps up to two vertical and two horizontal thresholds, yielding a maximum 3 × 3 region matrix."),

    h3("Error Handling"),
    tags$ul(
      tags$li("Most plotting and transformation steps are wrapped in ", code("tryCatch"), " with debug logging and user notifications."),
      tags$li("Invalid or empty selections are ignored in interaction observers to avoid NA/NULL event failures."),
      tags$li("Transformation helpers sanitize problematic inputs (for example, non-positive values before logarithmic transforms).")
    )
  )
}


render_dotplot_tech_integration_content <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Module Wiring"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "modDotPlotServer(\n  id             = 'dotplot',\n  rv             = rv,\n  res_GSEA       = res_GSEA,\n  res_GO         = res_GO,\n  module_outputs = module_outputs,\n  debug_level    = 2\n)"
    ),

    h3("Required Shared Data Contract"),
    h4("rv$data_mod"),
    tags$ul(
      tags$li("Tabular dataset with the columns used for X and Y selections."),
      tags$li("X and Y plotting columns must be numeric or coercible to numeric for transformation/plotting."),
      tags$li("Rows with non-finite transformed values are excluded during plot construction.")
    ),

    h4("rv$data_def"),
    tags$ul(
      tags$li("Metadata table describing columns, including ", code("Column"), ", ", code("Content"), " and typically ", code("Options"), "."),
      tags$li("Identifier choices are populated from rows where ", code("Content"), " matches identifier semantics.")
    ),

    h4("Minimal metadata example"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "data_def <- data.frame(\n  Column  = c('Ratio_A_B', 'AdjP_A_B', 'GeneSymbol'),\n  Content = c('Abundance Ratio', 'Abundance Ratio Adj. p-Value', 'Identifier'),\n  Options = c('SampleA_vs_SampleB', 'SampleA_vs_SampleB', 'Gene Symbol'),\n  stringsAsFactors = FALSE\n)"
    ),

    h3("Inter-module Inputs"),
    tags$ul(
      tags$li(code("res_GSEA"), " and ", code("res_GO"), " are optional and used for pathway-driven protein import in the labeling workflow."),
      tags$li(code("module_outputs"), " is accepted for application-level compatibility even when not directly consumed by every observer path.")
    ),

    h3("Integration Notes"),
    tags$ul(
      tags$li("Plotly event IDs are pre-registered in ", code("session$userData$plotlyShinyEventIDs"), " to suppress startup warnings before first render."),
      tags$li("The module exposes a compact return API (plot/config/threshold accessors + clear helper) suitable for dashboard-level orchestration."),
      tags$li("Download handlers export static plots; interactive Plotly state is not serialized as a standalone interactive artifact by this panel.")
    )
  )
}
