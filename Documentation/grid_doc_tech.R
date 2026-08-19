############
# Technical Documentation — Content (initial stubs for Plot Grid)

render_grid_tech_overview_content_grid <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Module Architecture"),
    p(
      "The Plot Grid module composes a multi‑panel figure from ggplot objects produced in other MiraProt modules. ",
      "It is implemented as a Shiny module that manages a shared selection of plots, optional span information, ",
      "grid‑level customization (alignment, labels, margins, legend overrides) and export via cowplot."
    ),

    h4("Module Structure"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "app.R                        # creates modEnv, rv; initializes modGridUI/modGridServer via modEnv
modules/Grid_module.R          # parent Plot Grid module; wires UI, server and utilities
modules/Grid/Grid_ui.R         # grid_UI(ns): UI for preview, download, grid options, plot options, spans and selection
modules/Grid/Grid_layout.R     # selection state, labels, span/margin state, layout optimization
modules/Grid/Grid_legend.R     # legend detection and forcing strategies
modules/Grid/Grid_composition.R # preparation, alignment, span placement, final composition"
    ),

    p(
      "At app startup, ", code("app.R"), " creates ", code("modEnv"), " and sources all module files into it. ",
      "For the grid, ", code("modules/Grid_module.R"), " explicitly sources ", code("Grid_ui.R"), ", ",
      code("Grid_layout.R"), ", ", code("Grid_legend.R"), " and ", code("Grid_composition.R"),
      " into ", code("modEnv"), " so they can be accessed as ", code("modEnv$grid_UI"),
      ", ", code("modEnv$add_to_grid"), ", ", code("modEnv$compose_grid"), " and related helpers."
    ),

    h3("App‑level Integration (app.R)"),
    p(
      "The Plot Grid module is integrated into the main app as a standard MiraProt module:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
      "ui:
  tabPanel(
    title = \"Plot Grid\",
    value = \"plot_grid\",
    modEnv$modGridUI(\"grid\")
  )

server:
  module_outputs$grid_out <- tryCatch({
    debug_log(\"Initializing Plot Grid module\", 2)

    if (exists(\"modGridServer\", envir = modEnv)) {
      # Try several signatures for robustness
      result <- tryCatch({
        modEnv$modGridServer(\"grid\", rv)
      }, error = function(e1) {
        debug_log(paste(\"Grid module simple init failed:\", e1$message), 2)
        tryCatch({
          modEnv$modGridServer(\"grid\", rv, debug_level = DEBUG_LEVEL)
        }, error = function(e2) {
          debug_log(paste(\"Grid module with debug_level failed:\", e2$message), 2)
          modEnv$modGridServer(id = \"grid\", rv = rv)
        })
      })
      ...
    } else {
      debug_log(\"modGridServer function not found in modEnv\", 1)
      ...
    }
  }, ...)"
    ),
    p(
      code("modGridUI(id)"), " is a thin wrapper around ", code("grid_UI(ns)"),
      " located in ", code("modules/Grid/Grid_ui.R"), ". ",
      code("modGridServer(id, rv, debug_level)"), " is defined in ", code("modules/Grid_module.R"),
      " and is responsible for wiring the UI inputs to the utilities in ", code("Grid_layout.R"),
      ", ", code("Grid_legend.R"), " and ", code("Grid_composition.R"),
      " and to the shared state stored in ", code("rv"), "."
    ),

    h3("Internal Module Structure (Grid_module.R)"),

    h4("UI Wrapper and Debugging"),
    tags$ul(
      tags$li(
        strong("UI entry: "),
        code("modGridUI <- function(id) { ... }"),
        " creates a namespace with ", code("NS(id)"), " and returns ",
        code("modEnv$grid_UI(ns)"), " as the full Plot Grid UI."
      ),
      tags$li(
        strong("Server entry: "),
        code("modGridServer <- function(id, rv, debug_level = 1) { moduleServer(id, function(input, output, session) { ... }) }"),
        " sets up a module‑local ", code("DEBUG_LEVEL"), " and ",
        code("debug_log(message, level)"), " which prefixes messages with ",
        code("[ GRID MODULE ... ]"), " and prints only when ", code("DEBUG_LEVEL >= level"), "."
      ),
      tags$li(
        strong("Safe helpers: "),
        code("key_for(x)"),
        " normalizes IDs for dynamic UI elements; ",
        code("`%||%`"),
        " is defined locally and assigned into ", code("modEnv"),
        " so it can be reused by utilities in ", code("Grid_layout.R"), "."
      )
    ),

    h4("Shared State in rv"),
    p(
      "The Plot Grid module does not keep its own copy of analysis data. It operates on ggplot objects stored in the shared ",
      code("rv"), " reactiveValues. The grid‑specific state is:"
    ),
    tags$ul(
      tags$li(
        code("rv$gridplot_selection"),
        " – named list of entries. Each entry has at least ",
        code("$plot"), " (ggplot), and typically ",
        code("$label"), ", ", code("$source"), ", ", code("$type"), ", ",
        code("$include_label"), " and ", code("$added_at"),
        " as set by ", code("add_to_grid()"), " or ", code("add_blank_to_grid()"), "."
      ),
      tags$li(
        code("rv$gridplot_order"),
        " – character vector of plot IDs specifying the global order of panels."
      ),
      tags$li(
        code("rv$plot_spans"),
        " – list of span settings per plot ID; each entry is a list with ",
        code("$colspan"), " and ", code("$rowspan"),
        " controlling how many columns/rows a panel should occupy."
      ),
      tags$li(
        code("rv$plot_margins"),
        " – list of per-plot margin offsets per plot ID; each entry is a list with ",
        code("$top"), ", ", code("$right"), ", ", code("$bottom"), ", ", code("$left"),
        " (numeric, in pt). These offsets are added to the global margins and applied ",
        "after alignment via ", code("apply_per_plot_margins()"), "."
      )
    ),
    p(
      "On module startup, ", code("modGridServer"), " ensures that ",
      code("rv$gridplot_selection"), ", ", code("rv$gridplot_order"),
      " and ", code("rv$plot_margins"),
      " are initialized (if NULL) to an empty list, character vector and empty list, respectively."
    ),

    h3("Grid Settings and UI Coupling"),

    h4("Collecting Settings from Grid_UI"),
    p(
      "The core configuration is captured in a reactive list ", code("grid_settings()"),
      " (defined in ", code("Grid_module.R"), ") built entirely from UI inputs of ", code("Grid_ui.R"), ":"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
      "grid_settings <- reactive({
  list(
    nrow   = { val <- input$nrow; if (is.null(val)) 1L else max(1L, as.integer(val)) },
    ncol   = { val <- input$ncol; if (is.null(val)) 2L else max(1L, as.integer(val)) },
    align  = { val <- input$align; if (is.null(val) || !nzchar(val)) \"none\" else val },
    labels_mode   = { val <- input$labels_mode; if (is.null(val) || !nzchar(val)) \"none\" else val },
    labels_custom = { val <- input$labels_custom %||% \"\" },
    label_size    = { val <- input$label_size; if (is.null(val)) 12 else max(6, min(72, as.numeric(val))) },
    hide_titles   = { val <- input$hide_titles; if (is.null(val)) FALSE else isTRUE(val) },
    margins = list(
      top    = { val <- input$margin_top;    if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      right  = { val <- input$margin_right;  if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      bottom = { val <- input$margin_bottom; if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      left   = { val <- input$margin_left;   if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) }
    ),
    plot_spans   = rv$plot_spans   %||% list(),
    plot_margins = rv$plot_margins %||% list(),
    force_legend_position = {
      val <- input$force_legend_position
      if (is.null(val) || !nzchar(val)) \"preserve\" else val
    }
  )
})"
    ),
    p(
      "These settings are passed to utility functions such as ",
      code("prepare_plots_for_grid()"), ", ", code("build_labels()"),
      " and ", code("compose_grid()"), " when creating the preview and download outputs."
    ),

    h3("Selection Management and Dynamic UI"),

    h4("Generating the Selection UI"),
    p(
      "The selection UI in the \"Selection\" well panel is generated by ",
      code("output$selection <- renderUI({ ... })"),
      " in ", code("Grid_module.R"), ". It uses ",
      code("rv$gridplot_order"), " to iterate through plots in order and ",
      code("rv$gridplot_selection"), " to retrieve metadata:"
    ),
    tags$ul(
      tags$li(
        "For each ID in ", code("rv$gridplot_order"), ", it creates a ", code("wellPanel"),
        " containing:",
        tags$ul(
          tags$li(code("checkboxInput('include_label_<id>')"), " -- toggles ", code("$include_label"), " for that entry."),
          tags$li(code("actionButton('move_up_<id>') / 'move_down_<id>'"), " -- change order via ", code(".move_swap()"), "."),
          tags$li(
            code("numericInput('colspan_<id>')"), " and ", code("numericInput('rowspan_<id>')"),
            " -- always visible, with bounds derived from current ", code("input$ncol"), " and ", code("input$nrow"), "."
          ),
          tags$li(
            "Per-plot margin offset inputs ", code("pm_top_<id>"), ", ", code("pm_right_<id>"), ", ",
            code("pm_bottom_<id>"), ", ", code("pm_left_<id>"),
            " -- numeric inputs (range -200 to +500 pt) read from ", code("rv$plot_margins[[id]]"), "."
          ),
          tags$li(code("actionButton('remove_<id>')"), " -- remove entry from selection and order.")
        )
      ),
      tags$li(
        "Span values are read from ", code("rv$plot_spans[[id]]"),
        " if present; otherwise defaults ", code("(1L, 1L)"), " are used."
      )
    ),

    h4("Reacting to Selection Controls"),
    p(
      "A set of observers in ", code("Grid_module.R"), " connects user actions in the selection UI back to ",
      code("rv"), ":"
    ),
    tags$ul(
      tags$li(
        strong("Include in labeling: "),
        "for each ID, an ", code("observeEvent(input$include_label_<id>)"),
        " updates ", code("rv$gridplot_selection[[id]]$include_label"), " and logs via ", code("debug_log"), "."
      ),
      tags$li(
        strong("Ordering: "),
        "for each ID, observers on ", code("move_up_<id>"), " and ", code("move_down_<id>"),
        " call ", code(".move_swap(rv$gridplot_order, pos, 'up'/'down')"),
        " and reassign the vector to ", code("rv$gridplot_order"), "."
      ),
      tags$li(
        strong("Span changes: "),
        "for each ID, observers on ", code("colspan_<id>"), " and ", code("rowspan_<id>"),
        " call ", code("ensure_span_entry(rv, id)"), " and clamp values based on ",
        code("input$ncol"), " and ", code("input$nrow"),
        ". The spans are stored in ", code("rv$plot_spans[[id]]$colspan / $rowspan"), "."
      ),
      tags$li(
        strong("Per-plot margin offsets: "),
        "for each ID, observers on ", code("pm_top_<id>"), ", ", code("pm_right_<id>"), ", ",
        code("pm_bottom_<id>"), ", ", code("pm_left_<id>"),
        " coerce values to numeric, clamp to [-200, 500] and store in ",
        code("rv$plot_margins[[id]]$top / $right / $bottom / $left"), ". ",
        "Initialises the entry to ", code("list(top=0, right=0, bottom=0, left=0)"), " if missing."
      ),
      tags$li(
        strong("Remove and clear: "),
        "per-entry remove buttons delete the entry from ", code("rv$gridplot_selection"),
        " and filter it out from ", code("rv$gridplot_order"),
        ". They also clean up associated ", code("rv$plot_margins[[id]]"), " and ",
        code("rv$plot_spans[[id]]"), " entries. ",
        "The global ", code("input$clear"), " button calls ", code("clear_grid(rv)"),
        " to reset selection/order only (it does not explicitly wipe all span/margin registries). ",
        "Any stale span/margin entries left after a full clear are ignored naturally because rendering ",
        "iterates over current selection/order IDs; they only matter again if the same IDs are reintroduced."
      ),
      tags$li(
        strong("Add blank: "),
        code("observeEvent(input$add_blank, ...)"),
        " calls ", code("generate_blank_id(rv)"),
        " and ", code("add_blank_to_grid(rv, id)"), " to insert a placeholder entry with ",
        code("type = 'blank'"), " and default spans (1x1)."
      ),
      tags$li(
        strong("Span clamping: "),
        "a standalone ", code("observe()"), " watches ", code("input$nrow"), " and ",
        code("input$ncol"), " and calls ", code("clamp_spans_to_grid(rv, nrow, ncol)"),
        " whenever the grid dimensions change, ensuring that no stored span exceeds the new bounds."
      )
    ),

    h3("Dual-Mode Rendering Pipeline"),

    h4("Architecture Overview"),
    p(
      "The module uses a dual-mode rendering system with a shared composition function and a ",
      "single top-level ", code("renderPlot"), ":"
    ),
    tags$ul(
      tags$li(
        code("current_plot"), " -- a ", code("reactiveVal(NULL)"), " that holds the current composed plot object."
      ),
      tags$li(
        code("compose_from(plots, settings, include_map = NULL)"), " -- a non-reactive function that ",
        "runs the full composition pipeline (prepare, label, compose). Used by both auto-update and manual paths."
      ),
      tags$li(
        code("output$preview <- renderPlot({ current_plot() })"), " -- the single top-level render ",
        "that displays whatever ", code("current_plot"), " holds."
      )
    ),

    h4("Reactive Dependency Map"),
    p("Maintenance summary of the active render graph:"),
    tags$ul(
      tags$li(
        code("selected_plots()"), " + ", code("grid_settings()"), " + ",
        code("get_include_map()"), " \u2192 ", code("render_data()"), "."
      ),
      tags$li(
        code("render_data_d <- debounce(render_data, 500)"), "."
      ),
      tags$li(
        strong("Auto path: "), code("observe(req(input$auto_update))"),
        " reads ", code("render_data_d()"), " and updates ", code("current_plot"), "."
      ),
      tags$li(
        strong("Manual path: "), code("observeEvent(input$create_plot)"),
        " recomposes immediately via ", code("compose_from(...)"), "."
      ),
      tags$li(
        code("renderPlot"), " outputs ", code("current_plot"), " as ",
        code("output$preview <- renderPlot({ current_plot() })"), "."
      )
    ),
    p(
      "Design intent: auto mode coalesces rapid UI churn through debounce; manual mode is a direct compose trigger for deterministic refreshes."
    ),

    h4("Shared Compose Function"),
    p(
      code("compose_from(plots, settings, include_map = NULL)"),
      " is the central non-reactive composition function:"
    ),
    tags$ul(
      tags$li(
        "Assigns names to plots from ", code("rv$gridplot_order"), " if missing."
      ),
      tags$li(
        "Calls ", code("prepare_plots_for_grid(plots, settings)"),
        " to optionally hide titles and apply global margins."
      ),
      tags$li(
        "Computes ", code("include_map"), " if not provided (manual path) via ",
        code("get_include_map(isolate(rv), plot_names)"), "."
      ),
      tags$li(
        "Calls ", code("build_labels(settings, plot_names, include_map)"),
        " to build panel labels."
      ),
      tags$li(
        "Calls ", code("compose_grid(prepared, settings, labels)"),
        " to create the final grid. On error, returns a placeholder ggplot with the error message."
      )
    ),

    h3("Composition Pipeline and Legend Control"),

    h4("Span‑aware Layout and Alignment"),
    p(
      "The main composition function ", code("compose_grid(plots, settings, labels, force_regular = FALSE)"),
      " in ", code("Grid_composition.R"), " is responsible for using cowplot to build the final grid. ",
      "Its behaviour can be summarized as follows:"
    ),
    tags$ul(
      tags$li(
        "If cowplot is not available, return a placeholder ggplot with a warning text."
      ),
      tags$li(
        "If ", code("settings$plot_spans"), " is non-empty and ",
        " is non‑empty, attempt a true span‑based layout via ",
        code("compose_grid_with_spans_safe(plots, settings, labels)"),
        ". This function uses ", code("create_span_layout_matrix_fixed()"), " and ",
        code("compute_span_coordinates()"), " to place plots into a canvas using ",
        code("cowplot::ggdraw()"), " and ", code("cowplot::draw_plot()"),
        " with consistent empty placeholders from ", code("create_empty_plot()"), "."
      ),
      tags$li(
        "If span composition fails or spans are disabled, fall back to a regular ",
        code("cowplot::plot_grid"), " layout with ",
        code("nrow = settings$nrow"), ", ", code("ncol = settings$ncol"),
        " and optional ", code("align"), " argument derived from ", code("settings$align"),
        " and grid size heuristics (downgrading ", code("\"hv\""), " in large grids)."
      )
    ),

    h4("Legend Forcing with Source‑based Detection"),
    p(
      "The grid offers an advanced legend control mechanism via ",
      code("settings$force_legend_position"),
      " (values: ", code("preserve"), ", ", code("bottom"), ", ", code("top"), ", ",
      code("right"), ", ", code("left"), ", ", code("none"), "). ",
      "This is implemented and tested in ", code("Grid_legend.R"), " as follows:"
    ),
    tags$ul(
      tags$li(
        strong("Detection: "),
        code("detect_plot_type_by_source(plot_obj, plot_name, source_info)"),
        " and ", code("analyze_converted_plot_structure()"),
        " classify plots based on ", code("entry$source"), ", plot name patterns (e.g. ",
        code("running score"), ", ", code("heatmap"), ", ", code("PubMed"), ") and structural cues. ",
        "The result flags complex types (e.g. heatmaps, composite GSEA plots) that may not respond well to simple ",
        code("theme(legend.position=...)"), " modifications."
      ),
      tags$li(
        strong("Theme test: "),
        code("test_legend_control_effectiveness(plot_obj, target_position)"),
        " applies a trial ", code("theme(legend.position = ...)"),
        " to check whether legend control works for that specific plot."
      ),
      tags$li(
        strong("Aggressive forcing: "),
        code("force_legend_maximum_aggression(plot_obj, legend_position, plot_name, source_info)"),
        " first tries normal ", code("theme()"), ", then increasingly aggressive strategies (double theme application, guide removal, ",
        "and rebuilding fallbacks). If all fail, it returns the original plot with a text annotation indicating that legend control failed."
      ),
      tags$li(
        strong("Integration in composition: "),
        "inside ", code("compose_grid()"),
        " each individual plot is passed through ",
        code("force_legend_maximum_aggression()"), " when ",
        code("settings$force_legend_position != 'preserve'"),
        ". Detailed source‑based statistics (counts per type, attempts, successes, failures, warnings) are logged via ",
        code("debug_log"), " at level 1–2."
      )
    ),

    h3("Download Handling"),
    p(
      "The download handler ", code("output$download"), " in ", code("Grid_module.R"),
      " builds a fresh grid at export time to ensure consistency with the current state:"
    ),
    tags$ul(
      tags$li(
        "Calls ", code("selected_plots()"), " and ", code("grid_settings()"),
        " to retrieve plots and settings."
      ),
      tags$li(
        "Runs ", code("prepare_plots_for_grid()"), ", ", code("get_include_map(rv, plot_names)"),
        " and ", code("build_labels(settings, plot_names, include_map)"),
        " in the same way as for the preview."
      ),
      tags$li(
        "Passes the plot list, settings and labels to ", code("compose_grid()"),
        " to obtain a combined ggplot object ", code("p"),
        ". If composition fails or results in ", code("NULL"),
        " a notification is shown and no file is written."
      ),
      tags$li(
        "Opens an appropriate graphics device based on ", code("input$downloadFormat"),
        " (PNG, JPEG, TIFF with ", code("units = 'in'"), " and ", code("res"),
        "; SVG via ", code("svglite::svglite"), "; PDF via ", code("grDevices::pdf"),
        ") using ", code("input$plotWidthInch"), ", ", code("input$plotHeightInch"),
        " and ", code("input$resolution_DPI"), " as width, height and pixel density for raster formats."
      ),
      tags$li(
        "Prints ", code("p"), " into the device, closes it on exit, and logs success or failure via ", code("debug_log"), "."
      )
    ),

    h3("Key Utility Functions in Grid_layout.R"),
    tags$ul(
      tags$li(
        code("add_to_grid(rv, id, plot, label = NULL, source = NULL)"),
        " – validates that ", code("plot"), " is a ggplot, then inserts or updates an entry in ",
        code("rv$gridplot_selection"),
        ", appends ", code("id"), " to ", code("rv$gridplot_order"),
        " (if not present) and initializes default spans in ", code("rv$plot_spans[[id]]"), "."
      ),
      tags$li(
        code("add_blank_to_grid(rv, id = NULL, label = NULL)"),
        " – creates an empty placeholder plot via ", code("create_empty_plot()"),
        " and registers it as a selection entry with ", code("type = 'blank'"),
        " and default spans."
      ),
      tags$li(
        code("remove_from_grid(rv, id)"), " / ", code("clear_grid(rv)"),
        " – remove individual entries or reset the entire selection and ordering."
      ),
      tags$li(
        code("compute_optimal_grid_layout(...)"),
        " – heuristic grid optimizer that uses ",
        code("n"), ", ", code("container_ratio"), ", ", code("max_rows"), ", ", code("max_cols"),
        " and optionally ", code("spans"), " and ", code("order_ids"),
        " to suggest a grid (rows, cols) that minimizes area and empty cells while respecting span packability ",
        "via ", code("can_pack_spans_greedy()"), "."
      ),
      tags$li(
        code("min_rows_for_cols(spans, ncol, order_ids, max_rows)"),
        " and ", code("reorder_compact_fit(spans, order_ids, ncol)"),
        " – used from ", code("modGridServer"), " to implement the \"Optimize grid layout\" logic and optional compact ordering."
      ),
      tags$li(
        code("clamp_spans_to_grid(rv, nrow, ncol)"),
        " – enforces that all stored ", code("$colspan"), " and ", code("$rowspan"),
        " values fit within the current grid size after an optimization or manual change."
      )
    ),

    h3("Dependencies, Robustness and Debugging"),

    h4("Dependencies"),
    tags$ul(
      tags$li(
        code("cowplot"),
        " – required for ", code("plot_grid"), ", ", code("ggdraw"), ", ", code("draw_plot"),
        " and ", code("draw_plot_label"), " used in ", code("compose_grid()"),
        " and span‑based layouts. The server checks availability via ",
        code("requireNamespace('cowplot', quietly = TRUE)"),
        " and shows a notification if it is missing."
      ),
      tags$li(
        code("ggplot2"),
        " – used throughout for ggplot checks, theme manipulation, margin handling, and placeholder plot creation."
      ),
      tags$li(
        code("svglite"),
        " – used for SVG downloads in the ", code("downloadHandler"),
        " when ", code("input$downloadFormat == 'svg'"), " (guarded by ",
        code("requireNamespace('svglite', quietly = TRUE)"), ")."
      )
    ),

    h4("Robustness Patterns"),
    tags$ul(
      tags$li(
        "Critical operations such as plot preparation, label building, grid composition, span layout construction and download are wrapped in ",
        code("tryCatch"), " blocks. Failures are logged via ", code("debug_log"),
        " and replaced with safe fallbacks (e.g. returning original plots, placeholder grids or informative error plots)."
      ),
      tags$li(
        "Inputs from the UI are validated and clamped (for example rows/columns are forced to be >= 1, margins are kept between 0 and 100, label size between 6 and 72)."
      ),
      tags$li(
        "Span operations use helper functions like ", code("ensure_span_entry()"), ", ", code("clamp_spans_to_grid()"),
        ", ", code("can_pack_spans_greedy()"), " and ", code("simulate_rows_no_backfill()"),
        " to avoid out‑of‑bounds access and to handle infeasible layouts gracefully."
      ),
      tags$li(
        "Legend forcing is guarded by explicit tests (", code("test_legend_control_effectiveness()"),
        ") and falls back to annotations when changes cannot reliably be applied."
      )
    ),

    h4("Debugging Support"),
    tags$ul(
      tags$li(
        "The module uses a scoped ", code("debug_log(message, level)"),
        " that prints tagged messages (", code("[ GRID MODULE ... ]"),
        ") to the console when ", code("DEBUG_LEVEL >= level"), "."
      ),
      tags$li(
        "Span‑related helpers (", code("create_span_layout_matrix_fixed()"), ", ", code("compute_span_coordinates()"),
        ", ", code("simulate_rows_no_backfill()"), ") emit detailed debug information about layout matrices, coordinates and required rows."
      ),
      tags$li(
        "Legend detection and forcing functions log intermediate decisions (e.g. detected plot types, legend aesthetics, manual scales, success/failure of each forcing approach)."
      )
    )
  )
}

render_grid_tech_functions_content_grid <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    h3("Public Interface (Grid_module.R)"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "modGridUI(id)
  # UI wrapper for the Plot Grid tab.
  #  - Creates a namespace 'ns' for the given id via NS(id).
  #  - Calls modEnv$grid_UI(ns) which is defined in modules/Grid/Grid_ui.R.
  #  - Returns the full Plot Grid UI for use in app.R:
  #        tabPanel(title = 'Plot Grid', value = 'plot_grid', modEnv$modGridUI('grid')).

modGridServer(id, rv, debug_level = 1)
  # Server module for the Plot Grid.
  #  - id: module id used with NS().
  #  - rv: shared reactiveValues object provided by app.R.
  #  - debug_level: numeric level for internal logging.
  #
  # Core responsibilities (see modules/Grid_module.R):
  #  - Setup:
  #      * Define local DEBUG_LEVEL <- debug_level.
  #      * Define debug_log(message, level = 1) helper.
  #      * Define %||% and assign it into modEnv.
  #      * Initialize rv$gridplot_selection <- list() when NULL.
  #      * Initialize rv$gridplot_order    <- character(0) when NULL.
  #  - Settings:
  #      * Define grid_settings() reactive that reads UI inputs:
  #            nrow, ncol, align,
  #            labels_mode, labels_custom, label_size,
  #            labels_include_blanks,
  #            hide_titles,
  #            margins (top/right/bottom/left),
  #            plot_margins (from rv$plot_margins),
  #            plot_spans (from rv$plot_spans),
  #            force_legend_position.
  #  - Utilities:
  #      * Import helpers from modEnv:
  #            add_to_grid, add_blank_to_grid, generate_blank_id,
  #            remove_from_grid, clear_grid,
  #            build_labels, compose_grid, prepare_plots_for_grid,
  #            get_include_map, ensure_span_entry,
  #            clamp_spans_to_grid, compute_optimal_grid_layout,
  #            .move_swap, can_pack_spans_greedy,
  #            min_rows_for_cols, reorder_compact_fit.
  #  - Selection state:
  #      * Define selected_plots() reactive over rv$gridplot_order + rv$gridplot_selection.
  #      * Render selection UI (output$selection) with include/ordering/span/remove controls.
  #  - Event handling:
  #      * Observe include_label_<id> checkboxes and update rv$gridplot_selection[[id]]$include_label.
  #      * Observe move_up_<id> / move_down_<id> and reorder rv$gridplot_order via .move_swap().
  #      * Observe colspan_<id> / rowspan_<id> and update rv$plot_spans[[id]] with clamping to nrow/ncol.
  #      * Observe remove_<id> and remove entries from rv$gridplot_selection and rv$gridplot_order.
  #      * Observe input$add_blank and call add_blank_to_grid(rv, id = generate_blank_id(rv)).
  #      * Observe input$clear and call clear_grid(rv).
  #      * Observe input$optimize_grid and call compute_optimal_grid_layout(), min_rows_for_cols(),
  #        reorder_compact_fit(), clamp_spans_to_grid(), plus optional order compaction.
  #  - Dual-mode rendering:
  #      * current_plot <- reactiveVal(NULL)   -- holds the current composed grid.
  #      * compose_from(plots, settings, include_map = NULL) -- non-reactive shared
  #            composition function: prepare -> label -> compose_grid. When include_map
  #            is NULL (manual path), computes it via get_include_map(isolate(rv), ...).
  #      * render_data <- reactive({...})      -- bundles selected_plots(),
  #            grid_settings() and include_map into one reactive.
  #      * render_data_d <- debounce(render_data, millis = 500)
  #      * Auto-update observe:
  #            - req(isTRUE(input$auto_update))
  #            - Reads render_data_d() and calls compose_from() to update current_plot.
  #      * Manual trigger: observeEvent(input$create_plot, ...) calls compose_from()
  #            directly and updates current_plot.
  #      * output$preview <- renderPlot({ current_plot() })  -- single top-level render.
  #      * output$download (downloadHandler):
  #            - Rebuilds prepared plots, labels and composed grid p.
  #            - Opens appropriate graphics device (png/jpeg/tiff/svg/pdf) with
  #              width = plotWidthInch, height = plotHeightInch, res = resolution_DPI.
  #            - Prints p and closes device.
  #  - Session cleanup:
  #      * session$onSessionEnded() clears rv$gridplot_selection and rv$gridplot_order
  #        (without explicitly wiping rv$plot_spans / rv$plot_margins)."
    ),

    h3("Module‑Local Helpers (Grid_module.R)"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "debug_log(message, level = 1)
  # Local function inside modGridServer.
  #  - Uses module-local DEBUG_LEVEL (captured from argument).
  #  - If DEBUG_LEVEL >= level:
  #       * Prints '[ GRID MODULE <timestamp> ] <message>' to stdout via cat().
  #  - Used throughout the module for diagnostic logging (selection changes,
  #    optimization decisions, composition steps, download events).

key_for(x)
  # Local helper defined inside modGridServer.
  #  - Returns a safe string ID for dynamic UI elements:
  #       paste0('id_', gsub('[^[:alnum:]_]+', '_', x)).
  #  - Used to construct input IDs such as include_label_<key_id>,
  #    move_up_<key_id>, colspan_<key_id>, etc.

`%||%`(a, b)
  # Null-coalescing helper defined inside modGridServer and also assigned into modEnv.
  #  - Returns 'b' if 'a' is NULL or has length 0, otherwise returns 'a'.
  #  - Used in both Grid_module.R and Grid_layout.R to simplify default handling.

grid_settings()
  # Reactive expression inside modGridServer.
  #  - Reads grid configuration from UI inputs (see modules/Grid/Grid_ui.R):
  #       * input$nrow, input$ncol
  #       * input$align
  #       * input$labels_mode, input$labels_custom, input$label_size
  #       * input$labels_include_blanks
  #       * input$hide_titles
  #       * margin_* (top/right/bottom/left)
  #       * rv$plot_margins
  #       * rv$plot_spans
  #       * input$force_legend_position
  #  - Returns a list with components:
  #       nrow, ncol, align,
  #       labels_mode, labels_custom, label_size,
  #       hide_titles,
  #       margins = list(top, right, bottom, left),
  #       plot_spans,
  #       plot_margins,
  #       force_legend_position.
  #  - Applies defaults and clamping:
  #       * nrow default 1L, ncol default 2L, minimum 1.
  #       * label_size default 12, clamped to [6, 72].
  #       * margins clamped to [0, 100] and default 10 when NULL.
  #       * hide_titles interpreted as logical (FALSE when NULL).
  #       * force_legend_position defaults to 'preserve' when NULL/empty.
  #       * plot_spans and plot_margins default to empty list() when NULL."
    ),

    h3("Selection & Layout Helpers (Grid_module.R)"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "selected_plots()
  # Reactive expression inside modGridServer.
  #  - Reads ids <- rv$gridplot_order.
  #  - For each id:
  #       entry <- rv$gridplot_selection[[id]]
  #       if (!is.null(entry) && !is.null(entry$plot)) adds entry$plot to result list.
  #  - Returns a named list of ggplot objects (names = ids with valid plots).
  #  - Used by preview and download paths.

output$selection
  # renderUI() generating the Selection panel.
  #  - Reads current rv$gridplot_selection and rv$gridplot_order.
  #  - For each id in order:
  #       * key_id <- gsub('[^[:alnum:]_]+', '_', id)
  #       * include_val <- entry$include_label %||% TRUE
  #       * spans <- rv$plot_spans[[id]] %||% list(colspan=1L, rowspan=1L)
  #       * Ensures cur_col/cur_row >= 1 and <= current ncol/nrow from input.
  #       * Creates a wellPanel with:
  #             - h5(id)
  #             - checkboxInput('include_label_<key_id>')
  #             - move_up_<key_id> / move_down_<key_id> buttons
  #             - numericInput colspan_<key_id>, rowspan_<key_id> (always visible,
  #               bounds from input$ncol / input$nrow)
  #             - Per-plot margin offset inputs pm_top_<key_id>, pm_right_<key_id>,
  #               pm_bottom_<key_id>, pm_left_<key_id> (range -200 to 500 pt,
  #               read from rv$plot_margins[[id]])
  #             - remove_<key_id> button.
  #  - Shows 'No plots selected yet.' when rv$gridplot_order is empty.

Observers for Selection Controls
  # The module defines grouped observers for:
  #
  # 1) Include checkboxes:
  #    observe({ for (id in rv$gridplot_order) { observeEvent(input[[include_label_<key_id>]], {...}) } })
  #      - Updates rv$gridplot_selection[[id]]$include_label based on checkbox state.
  #
  # 2) Move up/down buttons:
  #    observe({ for (id in rv$gridplot_order) { observeEvent(input[[move_up_<key_id>]], {...});
  #                                              observeEvent(input[[move_down_<key_id>]], {...}) } })
  #      - Computes pos <- match(id, current_order).
  #      - Calls .move_swap(current_order, pos, 'up'/'down') to update rv$gridplot_order.
  #
  # 3) Span numeric inputs:
  #    observe({ for (id in rv$gridplot_order) { observeEvent(input[[colspan_<key_id>]], {...});
  #                                              observeEvent(input[[rowspan_<key_id>]], {...}) } })
  #      - Calls ensure_span_entry(rv, id).
  #      - Reads value, coerces to integer, clamps to [1, input$ncol] or [1, input$nrow].
  #      - Assigns to rv$plot_spans[[id]]$colspan / $rowspan.
  #
  # 4) Per-plot margin offsets:
  #    observe({ for (id in rv$gridplot_order) {
  #      for (side in c('top','right','bottom','left')) {
  #        observeEvent(input[[pm_<side>_<key_id>]], {...})
  #      }
  #    }})
  #      - Coerces to numeric, clamps to [-200, 500].
  #      - Initialises rv$plot_margins[[id]] if NULL.
  #      - Stores in rv$plot_margins[[id]][[side]].
  #
  # 5) Remove buttons:
  #    observe({ for (id in rv$gridplot_order) { observeEvent(input[[remove_<key_id>]], {...}) } })
  #      - Removes entry from rv$gridplot_selection, rv$gridplot_order,
  #        rv$plot_margins[[id]] and rv$plot_spans[[id]].
  #
  # 6) Span clamping:
  #    observe({ clamp_spans_to_grid(rv, input$nrow, input$ncol) })
  #      - Fires whenever nrow/ncol change, ensuring all stored spans fit.

compose_from(plots, settings, include_map = NULL)
  # Non-reactive shared composition function inside modGridServer.
  #  - Returns a placeholder ggplot when plots is empty.
  #  - Assigns names from rv$gridplot_order if missing.
  #  - Calls prepare_plots_for_grid(plots, settings).
  #  - If include_map is NULL: computes via get_include_map(isolate(rv), plot_names).
  #  - Calls build_labels(settings, plot_names, include_map).
  #  - Calls compose_grid(prepared, settings, labels).
  #  - All steps wrapped in tryCatch with fallback plots.

render_data
  # Reactive expression bundling selected_plots(), grid_settings() and
  # include_map into a single list. include_map is computed here (not in
  # compose_from) to ensure include_label toggles produce a non-identical
  # output value, preventing debounce from silently suppressing the update.

render_data_d
  # debounce(render_data, millis = 500): collapses rapid successive changes
  # across settings, plots, spans, per-plot margins and include-label toggles
  # into a single downstream update.

current_plot
  # reactiveVal(NULL): holds the current composed grid object.
  # Written by the auto-update observe and the manual Create Plot handler.
  # Read by output$preview <- renderPlot({ current_plot() })."
    ),

    h3("Grid Utilities (Grid_layout.R) – Selection & Spans"),
    p(
      "The following functions are implemented in ", code("modules/Grid/Grid_layout.R"),
      " and imported into ", code("modGridServer"), " via ", code("modEnv"), "."
    ),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "add_to_grid(rv, id, plot, label = NULL, source = NULL)
  # Adds a ggplot object to the grid selection.
  #  - Ensures rv$gridplot_selection, rv$gridplot_order and rv$plot_spans exist.
  #  - Validates that 'plot' inherits from 'ggplot'; otherwise shows a notification and returns FALSE.
  #  - Creates entry <- list(
  #        plot = plot,
  #        label = label,
  #        source = source,
  #        type = 'ggplot',
  #        include_label = TRUE,
  #        added_at = Sys.time()
  #    ).
  #  - Stores entry in rv$gridplot_selection[[id]].
  #  - Appends id to rv$gridplot_order if not already present.
  #  - Initializes rv$plot_spans[[id]] <- list(colspan = 1L, rowspan = 1L) when missing.
  #  - Reassigns rv$gridplot_selection <- rv$gridplot_selection to trigger reactivity.
  #  - Logs via debug_log() and returns TRUE (invisible).

add_blank_to_grid(rv, id = NULL, label = NULL)
  # Adds a blank placeholder plot to the selection.
  #  - Ensures rv$gridplot_selection, rv$gridplot_order, rv$plot_spans exist.
  #  - If id is NULL: id <- generate_blank_id(rv).
  #  - Creates p <- create_empty_plot().
  #  - Creates entry <- list(
  #        plot = p,
  #        label = label %||% '',
  #        source = 'blank',
  #        type = 'blank',
  #        include_label = TRUE,
  #        added_at = Sys.time()
  #    ).
  #  - Inserts entry into rv$gridplot_selection and appends id to rv$gridplot_order when new.
  #  - Initializes rv$plot_spans[[id]] to (1L, 1L) when missing.
  #  - Reassigns rv$gridplot_selection to trigger UI updates.
  #  - Logs via debug_log() and returns TRUE (invisible).

generate_blank_id(rv)
  # Generates a unique ID for blank entries.
  #  - Uses base <- 'blank_', i <- 1L.
  #  - While paste0(base, i) is in names(rv$gridplot_selection): increment i.
  #  - Returns id <- paste0(base, i) and logs it.

remove_from_grid(rv, id)
  # Removes a single entry from the selection and order.
  #  - If rv$gridplot_selection or rv$gridplot_order is NULL: returns FALSE (invisible).
  #  - If id is in names(rv$gridplot_selection): removes that element and reassigns rv$gridplot_selection.
  #  - If id is in rv$gridplot_order: filters it out.
  #  - Also removes rv$plot_margins[[id]] and rv$plot_spans[[id]] when present.
  #  - Returns TRUE (invisible).

clear_grid(rv)
  # Clears the entire grid selection.
  #  - Sets rv$gridplot_selection <- list().
  #  - Sets rv$gridplot_order <- character(0).
  #  - Does NOT explicitly clear rv$plot_spans or rv$plot_margins.
  #  - Stale span/margin entries are harmless because active rendering only uses
  #    currently selected/ordered IDs; stale entries become relevant only if IDs reappear.
  #  - Returns TRUE (invisible).

reorder_grid(rv, new_order)
  # Respects a new order vector.
  #  - keep <- new_order[new_order %in% names(rv$gridplot_selection)].
  #  - Assigns rv$gridplot_order <- keep.

ensure_span_entry(rv, id)
  # Guarantees a valid span entry for given id.
  #  - If rv$plot_spans[[id]] is NULL:
  #       * Initializes to list(colspan = 1L, rowspan = 1L).
  #  - Else:
  #       * Coerces existing colspan/rowspan to integer >= 1:
  #             colspan <- as.integer(rv$plot_spans[[id]]$colspan %||% 1L)
  #             rowspan <- as.integer(rv$plot_spans[[id]]$rowspan %||% 1L)
  #       * Stores back into rv$plot_spans[[id]]."

    ),

    h3("Grid Utilities (Grid_layout.R) – Labels & Include Map"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "build_labels(settings, plot_names, include_map = NULL)
  # Builds panel labels based on settings and include map.
  #  - settings$labels_mode:
  #       * 'none'        -> returns NULL (no labels).
  #       * 'auto_letters'-> LETTERS[1:K] for K labeled panels.
  #       * 'auto_numbers'-> as.character(1:K).
  #       * 'custom'      -> parses settings$labels_custom, split by ',', trimmed; padded/truncated to K.
  #  - plot_names: character vector of plot IDs in display order.
  #  - include_map: named logical vector (names = plot_names) marking which should receive labels.
  #       * If NULL/invalid: defaults to all TRUE.
  #  - Returns a character vector 'labs' of length length(plot_names), names = plot_names,
  #    with labels assigned where include_map is TRUE and '' elsewhere.
  #  - Logs label assignments via debug_log() at level 2.

get_include_map(rv, plot_names)
  # Builds a logical map indicating which plots participate in labeling.
  #  - Initializes include <- rep(TRUE, length(plot_names)); names(include) <- plot_names.
  #  - For each name nm in plot_names:
  #       * If rv$gridplot_selection[[nm]]$include_label is non-NULL:
  #             include[nm] <- isTRUE(rv$gridplot_selection[[nm]]$include_label).
  #  - Logs the map and returns it.

has_visible_legend_comprehensive(plot)
  # Checks whether a ggplot has a visible legend.
  #  - Returns FALSE for non-ggplot objects.
  #  - Considers:
  #       * plot$theme$legend.position == 'none' -> immediately FALSE.
  #       * presence of legend-generating aesthetics in main mapping and layers
  #         (colour/color, fill, shape, size, alpha, linetype).
  #       * presence of scales affecting legend aesthetics.
  #  - Returns TRUE only when at least one legend-related aesthetic/scale is present
  #    and legend.position is not 'none'.
  #  - Uses tryCatch() to avoid errors and logs intermediate decisions via debug_log()."

    ),

    h3("Grid Utilities (Grid_legend.R) – Legend Forcing & Source Detection"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "detect_plot_type_by_source(plot_obj, plot_name = 'unknown', source_info = NULL)
  # Classifies a plot to decide if special legend handling is needed.
  #  - plot_obj: plot object (ggplot or other).
  #  - plot_name: character name, used for pattern matching.
  #  - source_info: e.g. entry$source from rv$gridplot_selection.
  #  - Returns list(type = <string>, needs_special_handling = <logical>, reason = <string>).
  #  - Logic:
  #       * NULL plot -> type = 'null'.
  #       * ComplexHeatmap classes -> type = 'complexheatmap_object', needs_special_handling = TRUE.
  #       * Non-ggplot -> type = 'non_ggplot'.
  #       * For ggplot:
  #            - Source-based detection (e.g. 'heatmap', 'gsea', 'go' in source_info).
  #            - Name-based detection (e.g. 'heatmap', 'running score', 'pubmed' in plot_name).
  #            - Deep structural analysis via analyze_converted_plot_structure().
  #       * Default -> type = 'ggplot_simple', needs_special_handling = FALSE.

analyze_converted_plot_structure(plot_obj)
  # Inspects ggplot internals (data, layers, faceting) to identify complex converted plots.
  #  - Looks for:
  #       * enrichplot-like columns (x, y, runningScore, position, gene, pvalue, NES).
  #       * heatmap-like data (value, Var1, Var2, fill) with many rows.
  #       * multi-layer structures (many different geoms).
  #       * non-trivial faceting.
  #  - Returns list(is_complex = TRUE/FALSE, detected_type = <string>, reason = <string>).

test_legend_control_effectiveness(plot_obj, target_position = 'bottom')
  # Tests whether theme(legend.position = target_position) works on a ggplot.
  #  - Returns list(works = TRUE/FALSE, reason = <string>).
  #  - Compares modified_plot$theme$legend.position with target_position (or 'none').
  #  - Used as a cheap probe before attempting more aggressive legend forcing.

force_legend_maximum_aggression(plot_obj, legend_position, plot_name = 'unknown', source_info = NULL)
  # Applies increasingly aggressive legend control to a single ggplot.
  #  - If legend_position == 'preserve': returns plot_obj unchanged.
  #  - Step 1: Uses test_legend_control_effectiveness():
  #       * If works:
  #           - Returns plot_obj + theme(legend.position = legend_position, legend.justification = 'center')
  #             or theme(legend.position = 'none') when legend_position == 'none'.
  #  - Step 2: If standard theme() fails:
  #       * Calls detect_plot_type_by_source() for diagnostics.
  #       * Tries a sequence of approaches:
  #             'double_theme', 'force_guides', 'rebuild_plot'
  #         each wrapped in tryCatch(), and tests each result again via test_legend_control_effectiveness().
  #       * Returns the first successful result.
  #  - Step 3: If all approaches fail:
  #       * Adds a red italic text annotation ('Legend position control failed...') to the plot.
  #       * Returns this annotated plot as final fallback."

    ),

    h3("Grid Utilities (Grid_layout.R) – Spans & Layout Optimization"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "create_span_layout_matrix_fixed(plots, plot_names, plot_spans, nrow, ncol)
  # Constructs a layout matrix and plot list for true span support (no backfill).
  #  - Initializes layout_matrix (nrow x ncol) with 0L.
  #  - Iterates over plot_names:
  #       * sp <- plot_spans[[nm]] %||% list(colspan = 1L, rowspan = 1L).
  #       * Clamps colspan to [1, ncol].
  #       * Uses a sequential cursor (cur_r, cur_c) to find the first free block of size
  #         rowspan x colspan; if none, logs a skip for that plot.
  #       * Fills layout_matrix with an integer index for each placed plot and records
  #         assignments[[nm]] = list(index, row, col, rowspan, colspan).
  #  - Builds plot_list: for each cell:
  #       * At main cell (row == a$row, col == a$col): inserts the original plot.
  #       * For other cells of the span or empty cells: inserts create_empty_plot().
  #  - Computes rel_heights, rel_widths to slightly boost rows/cols with spanning plots.
  #  - Returns list(matrix, plot_list, rel_heights, rel_widths, assignments).

compute_span_coordinates(assignments, nrow, ncol, gap = 0)
  # Converts span assignments to normalized coordinates for cowplot::ggdraw().
  #  - gap: fraction of total width/height (clamped to [0, 0.1]).
  #  - For each plot id:
  #       * Computes x, y, width, height in [0,1], applying gap as internal padding.
  #  - Returns a named list 'coords[[id]] <- list(x, y, width, height)'.

align_plots_if_requested(plots, settings)
  # Wrapper for alignment; currently defers to cowplot::plot_grid().
  #  - If settings$align == 'none': returns plots unchanged.
  #  - For 'h','v','hv': logs that alignment is delegated to plot_grid and returns plots.

compute_total_span_area(spans)
  # Computes lower bound on required grid cells:
  #  - Sums over ids:
  #       max(1, colspan) * max(1, rowspan)
  #  - Used by compute_optimal_grid_layout() and can_pack_spans_greedy().

can_pack_spans_greedy(spans, nrow, ncol, order_ids)
  # Fast greedy check for whether all spans can fit into an nrow x ncol grid.
  #  - Uses a simple fill algorithm over a matrix of zeros:
  #       * For each id in order_ids (or names(spans) if NULL):
  #             - Tries to place an rs x cs block into the first free region.
  #             - If any block cannot be placed: returns FALSE.
  #  - Returns TRUE if all blocks are placed.

simulate_rows_no_backfill(spans, ncol, order_ids)
  # Simulates row usage with a sequential cursor and no backfill.
  #  - Similar to create_span_layout_matrix_fixed but only tracks occupancy by row.
  #  - Returns max_row_used as integer (>= 1).
  #  - Used by min_rows_for_cols().

min_rows_for_cols(spans, ncol, order_ids, max_rows = 200L)
  # Minimal row count needed for a given number of columns to pack spans in the given order.
  #  - Calls simulate_rows_no_backfill(spans, ncol, order_ids).
  #  - Clamps result to max_rows and returns it.

reorder_compact_fit(spans, order_ids, ncol)
  # Heuristic to produce a compact ordering (First‑Fit‑Decreasing by colspan).
  #  - Computes widths for each id based on colspan (clamped to ncol).
  #  - Sorts ids by decreasing width, then packs them into rows respecting ncol.
  #  - Returns a flattened vector of ids grouped by these rows.

compute_optimal_grid_layout(n,
                            container_ratio = 1.4,
                            max_rows = 10L,
                            max_cols = 10L,
                            strategy = c('balanced'),
                            prefer_fewer_empty = TRUE,
                            spans = NULL,
                            order_ids = NULL,
                            enforce_pack = FALSE)
  # Suggests an (nrow, ncol) grid that fits n plots (or span area) with minimal cost.
  #  - If spans != NULL and enforce_pack = TRUE:
  #       * Sets n <- length(spans).
  #       * Computes min_cells <- compute_total_span_area(spans).
  #       * For each candidate r,c:
  #             - Requires r*c >= min_cells.
  #             - Requires can_pack_spans_greedy(spans, r, c, order_ids) == TRUE.
  #  - Else:
  #       * Requires r*c >= n.
  #  - Computes for each candidate:
  #       * empty cells, aspect ratio penalty vs container_ratio, skew penalty |r-c|/max(r,c).
  #       * Score = w_empty * empty + w_ar * ar_pen + w_skew * skew_pen.
  #  - Chooses layout with minimal score and returns list(nrow, ncol, score, empty, pack_ok).
  #  - If enforce_pack and no layout is feasible: falls back to an area-based suggestion."

    ),

    h3("Grid Utilities (Grid_composition.R) – Composition & Margins"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "compose_grid(plots, settings, labels = NULL, force_regular = FALSE)
  # Central grid composition function used by preview and download.
  #  - If cowplot is missing:
  #       * Returns a ggplot placeholder with message 'Package \"cowplot\" not installed'.
  #  - Span path:
  #       * If !force_regular and settings$plot_spans is non-empty:
  #             - Tries compose_grid_with_spans_safe(plots, settings, labels).
  #             - On success: returns span-based result.
  #             - On error or NULL: logs and falls back to regular grid.
  #  - Regular path:
  #       * If labels is NULL: tries build_labels(settings, names(plots)) (with error handling).
  #       * Computes base_rel_heights/widths from settings$margins.
  #       * Derives final_align from settings$align but may adjust:
  #             - 'hv' -> 'h' or 'v' for single row/column layouts.
  #             - 'hv' -> 'none' for large grids.
  #       * Extracts legend_position <- settings$force_legend_position %||% 'preserve'.
  #       * Builds source_lookup from rv$gridplot_selection[[id]]$source when available.
  #       * For each plot:
  #             - Classifies via detect_plot_type_by_source().
  #             - If legend_position != 'preserve':
  #                   applies force_legend_maximum_aggression().
  #       * Logs source statistics via debug_log().
  #       * Adjusts base_rel_heights/widths slightly when alignment is active.
  #       * Calls cowplot::plot_grid(plotlist = prepared_plots,
  #                                  nrow = settings$nrow,
  #                                  ncol = settings$ncol,
  #                                  align = final_align,
  #                                  rel_heights = base_rel_heights,
  #                                  rel_widths  = base_rel_widths,
  #                                  labels = labels (optional),
  #                                  label_size = settings$label_size (optional)).
  #       * On error: retries without alignment, or returns an error placeholder plot.

compose_grid_with_spans_safe(plots, settings, labels = NULL)
  # Span‑aware composition using cowplot::ggdraw() and draw_plot().
  #  - Validates that plots and settings$plot_spans are non-empty.
  #  - Derives plot_names from names(plots) or names(plot_spans).
  #  - Calls create_span_layout_matrix_fixed(plots, plot_names, valid_spans, nrow, ncol).
  #  - If labels is NULL: calls build_labels(settings, plot_names).
  #  - Derives gap from settings$cell_gap %||% 0.
  #  - Computes coords <- compute_span_coordinates(assignments, nrow, ncol, gap).
  #  - Calls align_plots_if_requested(plots, settings).
  #  - Builds p <- cowplot::ggdraw(), then for each plot:
  #       * Adds draw_plot(plots[[nm]], x = coords[[nm]]$x, y = coords[[nm]]$y, ...).
  #       * Optionally adds draw_plot_label() using labels[[nm]].
  #  - Returns composed plot or NULL on error.

create_empty_plot()
  # Creates a standardized empty ggplot used for blanks and filler cells.
  #  - Uses theme_void() and zero margins, blank panel and plot background.

prepare_plots_for_grid(plots, settings)
  # Applies title hiding and margin settings to each ggplot before composition.
  #  - Reads hide_titles <- settings$hide_titles.
  #  - Validates and clamps margins via validate_margin_value().
  #  - For each ggplot p:
  #       * If hide_titles:
  #             - Adds theme(plot.title    = element_blank(),
  #                       plot.subtitle = element_blank(),
  #                       plot.caption  = element_blank()).
  #       * Adds theme(plot.margin = margin(t = top, r = right, b = bottom, l = left, unit = 'pt')).
  #  - Non-ggplot objects are returned unchanged.
  #  - Returns list of processed plots.

validate_margin_value(value, name, default = 10)
  # Ensures a margin value is numeric, finite and within [0,100].
  #  - If value is NULL, non-numeric, NA or infinite: returns default.
  #  - Clamps numeric values to [0,100].
  #  - Logs any corrections via debug_log().

apply_per_plot_margins(plots, settings)
  # Applies individual margin offsets on top of global margins for each plot.
  #  - Reads per-plot offsets from settings$plot_margins (keyed by plot ID).
  #  - For each ggplot p with a matching entry in settings$plot_margins:
  #       * Reads offset values for top, right, bottom, left (default 0).
  #       * Adds offsets to the global margin values from settings$margins.
  #       * Clamps final margins to [-200, 500] pt per side.
  #       * Applies via ggplot2::theme(plot.margin = margin(..., unit = 'pt')).
  #  - Non-ggplot objects are returned unchanged.
  #  - Intentionally applied AFTER alignment so that cowplot::align_plots
  #    cannot equalise/strip the individual per-plot differences.
  #  - Logs detailed per-plot margin calculations via debug_log()."
    ),

    h3("Miscellaneous Helpers (Grid_layout.R)"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      ".move_swap(vec, idx_from, direction = c('up','down'))
  # Moves an element in a vector by swapping with its neighbour.
  #  - direction: 'up' or 'down'.
  #  - If idx_from is at boundary or vector length <= 1: returns vec unchanged.
  #  - Used to reorder rv$gridplot_order.

clamp_spans_to_grid(rv, nrow, ncol)
  # Clamps all stored spans to fit into the current grid size.
  #  - For each id in names(rv$plot_spans):
  #       * sp <- rv$plot_spans[[id]].
  #       * sp$colspan <- max(1L, min(as.integer(ncol), as.integer(sp$colspan %||% 1L))).
  #       * sp$rowspan <- max(1L, min(as.integer(nrow), as.integer(sp$rowspan %||% 1L))).
  #  - Logs when any span was changed.

compute_compact_order(rv, ord)
  # Builds a compact ordering heuristic based on blank status and area.
  #  - Constructs a data.frame with:
  #       id, idx, is_blank, area (colspan*rowspan).
  #  - Computes sort_key <- ifelse(is_blank, 1L, 0L).
  #  - Orders rows by sort_key, -area, idx.
  #  - Returns ordered id vector.

`%||%`(a, b)
  # Fallback %||% definition inside Grid_layout.R (if not already defined).
  #  - Same semantics as in Grid_module.R.
  #  - Ensures utilities work even if debug_log/%||% were not injected."
    )
  )
}

render_grid_tech_processing_content_grid <- function() {
  div(
    h2("Plot Processing and Composition"),
    hr(),

    h3("Selection Storage and State in rv"),
    p(
      "The Plot Grid stores all selection and layout information in the shared ",
      code("rv"), " object. The relevant slots are initialised at the start of ",
      code("modGridServer()"), " in ", code("modules/Grid_module.R"), " if they do not yet exist."
    ),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "if (is.null(isolate(rv$gridplot_selection))) {
  rv$gridplot_selection <- list()
  debug_log(\"Initialized rv$gridplot_selection\", 2)
}
if (is.null(isolate(rv$gridplot_order))) {
  rv$gridplot_order <- character(0)
  debug_log(\"Initialized rv$gridplot_order\", 2)
}"
    ),
    p(
      code("rv$gridplot_selection"), " is a named list keyed by plot IDs. Each entry is created ",
      "by ", code("add_to_grid()"), " or ", code("add_blank_to_grid()"), " in ",
      code("Grid_layout.R"), " and has at least:"
    ),
    tags$ul(
      tags$li(code("$plot"), " – the ggplot object (or an empty ggplot for blanks)."),
      tags$li(code("$label"), " – optional user‑visible label (string, may be empty)."),
      tags$li(code("$source"), " – source module identifier (e.g. 'Volcano', 'GSEA', 'blank')."),
      tags$li(code("$type"), " – 'ggplot' for normal plots, 'blank' for placeholders."),
      tags$li(code("$include_label"), " – logical flag controlling participation in labeling."),
      tags$li(code("$added_at"), " – timestamp added by the utility function.")
    ),
    p(
      code("rv$gridplot_order"), " is a character vector of plot IDs specifying the global order of panels. ",
      "This order drives both the Selection UI (", code("output$selection"), ") and the placement of panels in the grid."
    ),
    p(
      "Span information is stored separately in ", code("rv$plot_spans"), ", a named list where ",
      "each entry ", code("rv$plot_spans[[id]]"), " is a list with ",
      code("$colspan"), " and ", code("$rowspan"), " integers. ",
      "Entries are initialised by ", code("add_to_grid()"), " / ", code("add_blank_to_grid()"),
      " and normalised by ", code("ensure_span_entry()"), " and ", code("clamp_spans_to_grid()"),
      " in ", code("Grid_layout.R"), "."
    ),

    h3("Reading and Normalising Settings"),
    p(
      "The reactive ", code("grid_settings()"), " in ", code("Grid_module.R"),
      " is the central configuration object for the composer. It reads UI inputs from ",
      code("Grid_ui.R"), " and applies type conversion and range checks inline:"
    ),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px;",
      "grid_settings <- reactive({
  list(
    nrow   = { val <- input$nrow; if (is.null(val)) 1L else max(1L, as.integer(val)) },
    ncol   = { val <- input$ncol; if (is.null(val)) 2L else max(1L, as.integer(val)) },
    align  = { val <- input$align; if (is.null(val) || !nzchar(val)) \"none\" else val },
    labels_mode   = { val <- input$labels_mode; if (is.null(val) || !nzchar(val)) \"none\" else val },
    labels_custom = { val <- input$labels_custom %||% \"\" },
    label_size    = { val <- input$label_size; if (is.null(val)) 12 else max(6, min(72, as.numeric(val))) },
    hide_titles   = { val <- input$hide_titles; if (is.null(val)) FALSE else isTRUE(val) },
    margins = list(
      top    = { val <- input$margin_top;    if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      right  = { val <- input$margin_right;  if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      bottom = { val <- input$margin_bottom; if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) },
      left   = { val <- input$margin_left;   if (is.null(val)) 10 else max(0, min(100, as.numeric(val))) }
    ),
    plot_spans   = rv$plot_spans   %||% list(),
    plot_margins = rv$plot_margins %||% list(),
    force_legend_position = {
      val <- input$force_legend_position
      if (is.null(val) || !nzchar(val)) \"preserve\" else val
    }
  )
})"
    ),
    p(
      "The resulting list is passed unchanged into ", code("prepare_plots_for_grid()"), ", ",
      code("build_labels()"), " in ", code("Grid_layout.R"), " and ", code("compose_grid()"), " in ", code("Grid_composition.R"),
      " for preview and download. All conversions (integer casting, numeric clamping, ",
      "fallback values) happen at this stage to keep utilities free of UI‑specific assumptions."
    ),

    h3("From Selection to Prepared Plots"),
    p(
      "Both preview and download use the same basic preparation pipeline:"
    ),
    tags$ol(
      tags$li(
        strong("Collect plots in order: "),
        code("selected_plots()"), " in ", code("Grid_module.R"),
        " builds a named list of ggplot objects from ", code("rv$gridplot_selection"),
        " using the IDs in ", code("rv$gridplot_order"),
        ". Entries without a ", code("$plot"), " component are skipped."
      ),
      tags$li(
        strong("Apply plot‑level customisation: "),
        "the list of plots and ", code("settings <- grid_settings()"),
        " are passed to ", code("prepare_plots_for_grid(plots, settings)"),
        " in ", code("Grid_composition.R"), ". This function:",
        tags$ul(
          tags$li("Optionally removes titles, subtitles and captions when ", code("settings$hide_titles == TRUE"), "."),
          tags$li("Applies validated margins via ", code("ggplot2::theme(plot.margin = margin(..., unit = 'pt'))"), " to each plot.")
        )
      ),
      tags$li(
        strong("Determine label inclusion: "),
        code("get_include_map(rv, plot_names)"), " builds a logical vector marking which plots ",
        "participate in labeling, based on ", code("$include_label"), " flags stored in ",
        code("rv$gridplot_selection"), "."
      ),
      tags$li(
        strong("Generate labels: "),
        code("build_labels(settings, plot_names, include_map)"), " computes label strings ",
        "according to ", code("settings$labels_mode"), " (", code("none"), ", ", code("auto_letters"),
        ", ", code("auto_numbers"), ", ", code("custom"), "), ",
        code("settings$labels_custom"), " and the include map. ",
        "Labels are returned as a named character vector aligned with ", code("plot_names"), "."
      )
    ),
    p(
      "The combination of plot list, settings and labels is then passed into ",
      code("compose_grid()"), " to create the actual grid object."
    ),

    h3("Span Mechanism – Data Flow and Normalisation"),
    p(
      "The span system allows panels to occupy multiple grid cells in rows and columns. ",
      "Span information flows through the following components:"
    ),
    tags$ol(
      tags$li(
        strong("Initialisation: "),
        code("add_to_grid()"), " and ", code("add_blank_to_grid()"),
        " in ", code("Grid_layout.R"), " ensure that a new entry in ",
        code("rv$plot_spans[[id]]"), " exists with default ",
        code("list(colspan = 1L, rowspan = 1L)"), " whenever a plot or blank is added."
      ),
      tags$li(
        strong("UI binding: "),
        "the Selection UI in ", code("Grid_module.R"),
        " reads current spans from ", code("rv$plot_spans[[id]]"), " and exposes them via ",
        code("numericInput('colspan_<id>')"), " and ", code("numericInput('rowspan_<id>')"),
        ". Values are clamped to the current ",
        code("input$ncol"), " and ", code("input$nrow"), " when rendered."
      ),
      tags$li(
        strong("Span updates from UI: "),
        "dedicated ", code("observeEvent"), " handlers watch ",
        code("input$colspan_<id>"), " and ", code("input$rowspan_<id>"),
        " for each ID. For each change they:",
        tags$ul(
          tags$li("Call ", code("ensure_span_entry(rv, id)"), " to guarantee a non‑NULL entry."),
          tags$li(
            "Coerce the incoming value to integer and clamp it using ",
            code("input$ncol"), " / ", code("input$nrow"),
            " (with ", code("%||%"), " fallbacks)."
          ),
          tags$li("Store the result in ", code("rv$plot_spans[[id]]$colspan"), " / ", code("$rowspan"), ".")
        )
      ),
      tags$li(
        strong("Global clamping after grid size changes: "),
        "when the \"Optimize grid layout\" button adjusts ", code("input$nrow"),
        " or ", code("input$ncol"), " via ", code("updateNumericInput()"),
        " the module calls ", code("clamp_spans_to_grid(rv, new_r, new_c)"),
        " to enforce that all stored spans fit within the new bounds."
      ),
      tags$li(
        strong("Passing spans to the composer: "),
        "the current span configuration is embedded into ", code("grid_settings()"),
        " as ", code("settings$plot_spans"), " and passed to ",
        code("compose_grid()"), " and ", code("compose_grid_with_spans_safe()"),
        " in ", code("Grid_composition.R"), "."
      )
    ),

    h3("Span‑Aware Layout – Algorithm"),
    p(
      "When spans are enabled and span information is available, ",
      code("compose_grid()"), " in ", code("Grid_composition.R"),
      " attempts a true span‑based layout before falling back to a regular grid."
    ),
    tags$ol(
      tags$li(
        strong("Entry point: "),
        "in ", code("compose_grid()"),
        " the first branch checks:",
        tags$code("if (!force_regular && length(settings$plot_spans) > 0)"),
        " and then calls ",
        code("compose_grid_with_spans_safe(plots, settings, labels)"),
        " inside a ", code("tryCatch"), ". On success, the span‑aware result is returned directly."
      ),
      tags$li(
        strong("Layout matrix construction: "),
        code("compose_grid_with_spans_safe()"),
        " extracts ", code("plot_names"), " and filters ", code("settings$plot_spans"),
        " to those names. It then calls ",
        code("create_span_layout_matrix_fixed(plots, plot_names, valid_spans, nrow, ncol)"),
        " which:",
        tags$ul(
          tags$li("Initialises ", code("layout_matrix"), " (nrow × ncol) with 0L."),
          tags$li(
            "Iterates through ", code("plot_names"), " in the given order and, for each plot:",
            tags$ul(
              tags$li("Reads ", code("colspan"), " and ", code("rowspan"), " from ", code("plot_spans[[nm]]"), " with defaults 1L."),
              tags$li("Clamps ", code("colspan <= ncol"), " and ", code("rowspan >= 1"), "."),
              tags$li(
                "Searches for the first free block of size ", code("rowspan × colspan"),
                " using a sequential cursor ", code("(cur_r, cur_c)"),
                " without backfill into earlier rows."
              ),
              tags$li(
                "If a free block is found, fills the corresponding cells in ", code("layout_matrix"),
                " with a unique integer index and records ",
                code("plot_assignments[[nm]] <- list(index, row, col, rowspan, colspan)"), "."
              ),
              tags$li(
                "If no block fits within the visible grid, logs that the plot is skipped for span layout."
              )
            )
          ),
          tags$li(
            "Builds ", code("plot_list"), " by traversing the matrix row by row:",
            tags$ul(
              tags$li(
                "For each cell that is the main location of a span (row == a$row, col == a$col), ",
                "inserts the original plot."
              ),
              tags$li("For all other cells (within spans or empty), inserts ", code("create_empty_plot()"), ".")
            )
          ),
          tags$li(
            "Computes ", code("rel_heights"), " and ", code("rel_widths"),
            " by boosting rows/columns touched by large spans."
          ),
          tags$li(
            "Returns a list with ", code("matrix"), ", ", code("plot_list"), ", ",
            code("rel_heights"), ", ", code("rel_widths"), " and ", code("assignments"), "."
          )
        )
      ),
      tags$li(
        strong("Coordinate mapping: "),
        "back in ", code("compose_grid_with_spans_safe()"),
        " the function computes:",
        tags$ul(
          tags$li(
            code("coords <- compute_span_coordinates(assignments, nrow, ncol, gap = gap)"),
            " where ", code("gap"), " is derived from ", code("settings$cell_gap %||% 0"),
            " and limited to [0, 0.1]."
          ),
          tags$li(
            "Each entry ", code("coords[[nm]]"), " contains ",
            code("x, y, width, height"), " in [0,1], mapping the plot's span to a region on the canvas."
          )
        )
      ),
      tags$li(
        strong("Canvas composition with cowplot: "),
        "the final span‑aware grid is built as:",
        tags$ul(
          tags$li("Start with ", code("p <- cowplot::ggdraw()"), "."),
          tags$li(
            "Optionally align plots via ", code("align_plots_if_requested(plots, settings)"),
            " (currently returns plots unchanged; alignment is handled later by ",
            code("plot_grid"), " in the non‑span path)."
          ),
          tags$li(
            "For each plot name nm:",
            tags$ul(
              tags$li(
                "Adds ", code("cowplot::draw_plot(plots[[nm]], x = a$x, y = a$y, width = a$width, height = a$height)"),
                " where ", code("a <- coords[[nm]]"), "."
              ),
              tags$li(
                "If labels are available and non‑empty for nm, draws a label with ",
                code("cowplot::draw_plot_label()"), " positioned at the top‑left corner of the span, ",
                "scaled by ", code("settings$label_size"), "."
              )
            )
          )
        )
      ),
      tags$li(
        strong("Fallback: "),
        "if span layout fails at any stage (NULL result or error), ",
        code("compose_grid()"), " logs the error and falls back to a regular ",
        code("cowplot::plot_grid"), " layout described below."
      )
    ),

    h3("Regular Grid Composition and Customisation"),
    p(
      "When spans are disabled or a span‑based layout is not feasible, the module uses ",
      code("cowplot::plot_grid"), " to combine prepared plots into a regular grid."
    ),
    tags$ol(
      tags$li(
        strong("Alignment mode: "),
        "the ", code("align"), " argument is derived from ", code("settings$align"),
        " with additional heuristics in ", code("compose_grid()"), ":",
        tags$ul(
          tags$li("For ", code("align == 'hv'"), " and a single row: downgraded to 'h'."),
          tags$li("For ", code("align == 'hv'"), " and a single column: downgraded to 'v'."),
          tags$li("For ", code("align == 'hv'"), " and grids with few plots: often kept as 'h'."),
          tags$li("For large grids: alignment may be switched to 'none' to avoid over‑compression.")
        )
      ),
      tags$li(
        strong("Margins and relative sizes: "),
        "margins from ", code("settings$margins"), " are used to derive ",
        code("base_rel_heights"), " and ", code("base_rel_widths"),
        " that can expand rows or columns when margins are large. ",
        "When alignment is active (", code("h"), ", ", code("v"), ", or ", code("hv"),
        "), a spacing factor is applied to these vectors to avoid overly tight layouts."
      ),
      tags$li(
        strong("Legend forcing: "),
        "before calling ", code("plot_grid"), ", each plot is optionally processed by ",
        code("force_legend_maximum_aggression()"), " depending on ",
        code("settings$force_legend_position"), ". ",
        "This step uses source‑ and name‑based detection to treat problematic plots (e.g. heatmaps, complex GSEA plots) differently."
      ),
      tags$li(
        strong("Final composition: "),
        "the call to ", code("cowplot::plot_grid"), " is built as:",
        tags$ul(
          tags$li(
            code("do.call(cowplot::plot_grid, args)"), " where args includes:",
            tags$ul(
              tags$li("plotlist = prepared_plots"),
              tags$li("nrow = settings$nrow, ncol = settings$ncol"),
              tags$li("align = final_align"),
              tags$li("rel_heights, rel_widths"),
              tags$li("labels (if non‑NULL), label_size (if provided)")
            )
          ),
          tags$li(
            "On composition error, the function retries without alignment and ultimately ",
            "falls back to an error placeholder ggplot if all attempts fail."
          )
        )
      )
    ),

    h3("Optimisation of Grid Size and Order"),
    p(
      "The \"Optimize grid layout\" button in ", code("Grid_ui.R"),
      " triggers an observer in ", code("Grid_module.R"),
      " that uses span‑aware heuristics from ", code("Grid_layout.R"),
      " to suggest a more compact combination of rows and columns:"
    ),
    tags$ol(
      tags$li(
        strong("Baseline rows: "),
        "computes ", code("baseline_rows_needed"), " via ",
        code("min_rows_for_cols(spans, ncol = cur_c, order_ids = current_order, ...)"),
        " for the current number of columns and order."
      ),
      tags$li(
        strong("Compact order candidate: "),
        "builds ", code("compact_candidate <- reorder_compact_fit(spans, current_order, ncol = cur_c)"),
        " and computes ", code("compact_rows_needed"), " with the same function. ",
        "If ", code("compact_rows_needed"), " is strictly smaller than ", code("baseline_rows_needed"),
        " the compact order and a reduced row count are considered."
      ),
      tags$li(
        strong("Grid search with spans: "),
        "if spans are present, the observer calls ",
        code("compute_optimal_grid_layout(...)"), " twice: once with the current order and once with the compact order ",
        "(when ", code("input$opt_compact_order"), " is TRUE). ",
        "Both calls require ", code("can_pack_spans_greedy(spans, r, c, order_ids)"),
        " to be TRUE for candidate layouts."
      ),
      tags$li(
        strong("Acceptance criteria: "),
        "an improved layout is accepted only if it is strictly better than the current one, based on:",
        tags$ul(
          tags$li("Total area ", code("nrow * ncol"), " being smaller, or"),
          tags$li(
            "Number of empty cells decreasing while not increasing rows or columns beyond the current grid."
          )
        )
      ),
      tags$li(
        strong("Applying changes: "),
        "if an improved layout is found, the observer:",
        tags$ul(
          tags$li("Optionally updates ", code("rv$gridplot_order"), " to the compact order."),
          tags$li("Calls ", code("updateNumericInput()"), " for ", code("nrow"), " and ", code("ncol"), "."),
          tags$li("Calls ", code("clamp_spans_to_grid(rv, new_r, new_c)"), " to keep spans consistent."),
          tags$li("Shows a notification summarising the new grid size and whether compact order was applied.")
        )
      )
    ),

    h3("Download Handling and Reproducibility"),
    p(
      "The download handler ", code("output$download"), " in ", code("Grid_module.R"),
      " mirrors the preview path but writes the composed grid to disk:"
    ),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px;",
      "output$download <- downloadHandler(
  filename = function() { ... },
  content = function(file) {
    plots <- selected_plots()
    if (length(plots) == 0) {
      showNotification(\"No plots to download.\", type = \"error\")
      debug_log(\"Download aborted: no plots\", 1)
      return()
    }

    settings <- grid_settings()

    prepared <- tryCatch({
      prepare_plots_for_grid(plots, settings)
    }, error = function(e) {
      debug_log(paste(\"prepare_plots_for_grid failed during download:\", e$message), 1)
      plots
    })

    plot_names <- names(prepared)
    if (is.null(plot_names) || length(plot_names) == 0) {
      plot_names <- names(plots)
    }

    include_map <- tryCatch(get_include_map(rv, plot_names), error = function(e) {
      debug_log(paste(\"get_include_map failed (download):\", e$message), 1)
      setNames(rep(TRUE, length(plot_names)), plot_names)
    })

    labels <- tryCatch(
      build_labels(settings, plot_names, include_map),
      error = function(e) {
        debug_log(paste(\"build_labels failed during download:\", e$message), 1)
        NULL
      }
    )

    p <- tryCatch({
      compose_grid(prepared, settings, labels)
    }, error = function(e) {
      debug_log(paste(\"compose_grid failed during download:\", e$message), 1)
      NULL
    })

    if (is.null(p)) {
      showNotification(\"Could not compose the grid for download.\", type = \"error\")
      return()
    }

    widthIn  <- { x <- input$plotWidthInch;  if (is.null(x)) 14 else as.numeric(x) }
    heightIn <- { x <- input$plotHeightInch; if (is.null(x)) 10 else as.numeric(x) }
    res      <- { x <- input$resolution_DPI; if (is.null(x)) 600 else as.integer(x) }
    fmt      <- { x <- input$downloadFormat; if (is.null(x) || !nzchar(x)) \"png\" else x }

    debug_log(paste(\"Download requested as\", fmt,
                    \"| size:\", widthIn, \"x\", heightIn, \"in @\", res, \"ppi\"), 2)

    switch(fmt,
      png  = { grDevices::png (file, width = widthIn, height = heightIn, units = \"in\", res = res) },
      jpeg = { grDevices::jpeg(file, width = widthIn, height = heightIn, units = \"in\", res = res) },
      tiff = { grDevices::tiff(file, width = widthIn, height = heightIn, units = \"in\", res = res) },
      svg  = { svglite::svglite(file, width = widthIn, height = heightIn) },
      pdf  = { grDevices::pdf(file, width = widthIn, height = heightIn) },
      { grDevices::png(file, width = widthIn, height = heightIn, units = \"in\", res = res) }
    )
    on.exit(grDevices::dev.off(), add = TRUE)

    print(p)
    debug_log(\"Download completed\", 2)
  }
)"
    ),
    p(
      "Because the same utilities ", code("prepare_plots_for_grid()"), ", ",
      code("build_labels()"), " and ", code("compose_grid()"), " are used for both preview and download, ",
      "grid customisation (alignment, labels, margins, spans and legend overrides) is applied identically in both contexts."
    )
  )
}

render_grid_tech_integration_content_grid <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Integration Requirements"),

    h4("Required reactive values (from the main app)"),
    p(
      "The Plot Grid module operates entirely on ggplot objects and does not require direct access ",
      "to raw expression data. Its state is stored in the shared ", code("rv"), " object, which is ",
      "initialised in ", code("server()"), " of ", code("app.R"), " as:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "server <- function(input, output, session) {
  ...
  rv <- reactiveValues()
  debug_log(\"Reactive values initialized\", 2)
  ...
}"
    ),
    p(
      "The Plot Grid module itself initialises its grid‑specific slots when it starts (see ",
      code("modules/Grid_module.R"), "). No additional structure is required from the main app beyond ",
      "a writable ", code("rv"), " object."
    ),

    h4("Plot‑Grid specific state in rv"),
    p(
      "Inside ", code("modGridServer()"), " the following reactive values are created or initialised:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# In modGridServer(id, rv, debug_level)

# Selection and order (initialised if missing)
if (is.null(isolate(rv$gridplot_selection))) {
  rv$gridplot_selection <- list()
  debug_log(\"Initialized rv$gridplot_selection\", 2)
}
if (is.null(isolate(rv$gridplot_order))) {
  rv$gridplot_order <- character(0)
  debug_log(\"Initialized rv$gridplot_order\", 2)
}
if (is.null(isolate(rv$plot_margins))) {
  rv$plot_margins <- list()
  debug_log(\"Initialized rv$plot_margins\", 2)
}

# Spans and per-plot margins are initialised lazily in
# add_to_grid() / add_blank_to_grid() and maintained by
# ensure_span_entry() / clamp_spans_to_grid()."
    ),
    tags$ul(
      tags$li(code("rv$gridplot_selection"), " -- named list of plot entries (plot, label, source, type, include_label, added_at)."),
      tags$li(code("rv$gridplot_order"), " -- character vector of plot IDs specifying global order."),
      tags$li(code("rv$plot_spans"), " -- list of span settings (per ID: list(colspan, rowspan)), initialised on demand."),
      tags$li(code("rv$plot_margins"), " -- list of per-plot margin offsets (per ID: list(top, right, bottom, left)), initialised on demand.")
    ),

    h3("How the module is wired (UI & server)"),

    h4("UI wiring in app.R (Main Analysis → tabsetPanel)"),
    p(
      "The Plot Grid UI is integrated into the main tabset in ", code("app.R"),
      " using the exported UI function ", code("modGridUI()"), " from ",
      code("modules/Grid_module.R"), ". Because all module files have already been loaded into ",
      code("modEnv"), " at startup, ", code("modGridUI"), " is available as ", code("modEnv$modGridUI"), "."
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# UI definition in app.R (Main Analysis → tabsetPanel)

tabPanel(
  title = \"Plot Grid\",
  value = \"plot_grid\",
  modEnv$modGridUI(\"grid\")
)"
    ),
    p(
      code("modGridUI(id)"), " internally creates a namespace via ", code("NS(id)"),
      " and calls ", code("modEnv$grid_UI(ns)"), " (defined in ", code("modules/Grid/Grid_ui.R"),
      ") to assemble the full Plot Grid UI, including preview, download controls, grid options, plot options, spans and selection."
    ),

    h4("Server wiring in app.R"),
    p(
      "The server side of the Plot Grid is initialised in ", code("server()"), " of ", code("app.R"),
      " in the same section as other analysis modules. The result is stored in ",
      code("module_outputs$grid_out"), " for status tracking:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# ENHANCED Grid Module Initialization in app.R

module_outputs$grid_out <- tryCatch({
  debug_log(\"Initializing Plot Grid module\", 2)

  # Check if the Grid module function exists
  if (exists(\"modGridServer\", envir = modEnv)) {
    debug_log(\"Found modGridServer function\", 2)

    # Try simplified initialization first
    result <- tryCatch({
      modEnv$modGridServer(\"grid\", rv)
    }, error = function(e1) {
      debug_log(paste(\"Grid module simple init failed:\", e1$message), 2)

      # Try with debug level
      tryCatch({
        modEnv$modGridServer(\"grid\", rv, debug_level = DEBUG_LEVEL)
      }, error = function(e2) {
        debug_log(paste(\"Grid module with debug_level failed:\", e2$message), 2)

        # Try minimal approach
        modEnv$modGridServer(id = \"grid\", rv = rv)
      })
    })

    if (!is.null(result)) {
      debug_log(\"Grid module initialized successfully\", 1)
      result
    } else {
      debug_log(\"Grid module returned NULL\", 1)
      NULL
    }

  } else {
    debug_log(\"modGridServer function not found in modEnv\", 1)

    # Try to load the Grid module file if it exists
    if (file.exists(\"modules/Grid_module.R\")) {
      debug_log(\"Attempting to source Grid module file\", 1)
      source(\"modules/Grid_module.R\", local = modEnv)

      # Try again after sourcing
      if (exists(\"modGridServer\", envir = modEnv)) {
        debug_log(\"Grid module function found after sourcing\", 1)
        modEnv$modGridServer(\"grid\", rv)
      } else {
        debug_log(\"Grid module function still not found after sourcing\", 1)
        NULL
      }
    } else {
      debug_log(\"Grid module file not found: modules/Grid_module.R\", 1)
      NULL
    }
  }

}, error = function(e) {
  debug_log(paste(\"Error initializing Grid module:\", e$message), 1)
  showNotification(
    paste(\"Plot Grid module failed to load:\", e$message),
    type     = \"warning\",
    duration = 8
  )
  NULL
})"
    ),
    p(
      "There is no explicit API returned by ", code("modGridServer()"), " (the function is called for its side effects: ",
      "updating ", code("rv"), " and registering outputs/observers). ",
      "The presence or absence of ", code("module_outputs$grid_out"), " is used only for module health diagnostics."
    ),

    h3("Module Loading into modEnv"),
    p(
      "All module files, including ", code("modules/Grid_module.R"), " and its sub‑scripts, are loaded into ",
      code("modEnv"), " at application startup. This is done before the UI and server are defined:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# In app.R – Module Environment Setup

# Create or clear modEnv
if (exists(\"modEnv\", envir = globalenv())) {
  ...
} else {
  modEnv <- new.env(parent = globalenv())
  assign(\"modEnv\", modEnv, envir = globalenv())
}

# Load all module files into modEnv
for (f in list.files(\"modules\", pattern = \"\\\\.R$\", full.names = TRUE)) {
  tryCatch({
    sys.source(f, envir = modEnv)
    debug_log(paste(\"Loaded:\", basename(f)), 2)
  }, error = function(e) {
    debug_log(paste(\"Error loading\", basename(f), \":\", e$message), 1)
  })
}"

    ),
    p(
      "Within ", code("modules/Grid_module.R"), ", the Grid sub‑scripts are sourced a second time explicitly into ",
      code("modEnv"), " to ensure the expected naming and environment setup:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# modules/Grid_module.R

# Source sub-parts into modEnv to avoid name clashes and match project style
sys.source(\"./modules/Grid/Grid_ui.R\",    envir = modEnv)
sys.source(\"./modules/Grid/Grid_layout.R\",      envir = modEnv)
sys.source(\"./modules/Grid/Grid_legend.R\",      envir = modEnv)
sys.source(\"./modules/Grid/Grid_composition.R\", envir = modEnv)

modGridUI    <- function(id) { ... }
modGridServer <- function(id, rv, debug_level = 1) { ... }"
    ),
    p(
      "This ensures that ", code("grid_UI()"), " and all utility functions in ",
      code("Grid_layout.R"), ", ", code("Grid_legend.R"), " and ", code("Grid_composition.R"),
      " are available as ", code("modEnv$grid_UI"),
      ", ", code("modEnv$add_to_grid"), ", ", code("modEnv$compose_grid"), " and so on."
    ),

    h3("Interaction with Other Modules"),

    h4("How plots enter the Plot Grid"),
    p(
      "Other modules do not call ", code("modGridServer()"), " directly. Instead, they interact with the Plot Grid ",
      "by calling ", code("modEnv$add_to_grid(rv, ...)"), " (implemented in ",
      code("modules/Grid/Grid_layout.R"), ") when their own \"Add to grid\" buttons are pressed. ",
      "Typical patterns mirror the GSEA example shown in its own technical documentation: ",
      "modules check that the current plot is a non‑NULL ggplot, build a unique ", code("id"),
      " and a user‑visible ", code("label"), " and then call:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = \"<MODULE_NAME>\")"
    ),
    p(
      "The Plot Grid module itself does not manage these calls; it only responds to the resulting updates in ",
      code("rv$gridplot_selection"), ", ", code("rv$gridplot_order"), " and ", code("rv$plot_spans"),
      " by updating its Selection UI, composing the grid and enabling downloads."
    ),

    h4("External module call contract (required)"),
    p(
      "External modules should treat ", code("add_to_grid()"), " as a strict integration contract and call it as:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "add_to_grid(rv, id, plot, label, source)"
    ),
    tags$ul(
      tags$li(
        code("plot"), " must be a ", code("ggplot"), " object. Representative patterns in ",
        code("modules/PCA/pca_module_server_pipeline.R"), ", ",
        code("modules/venn/venn_observers_plot_interaction.R"), " and ",
        code("modules/Volcano/volcano_export.R"),
        " validate with ", code("inherits(p, 'ggplot')"), " before calling ", code("add_to_grid()"), "."
      ),
      tags$li(
        code("id"), " controls replacement versus insertion. If the same ID is submitted repeatedly, the existing ",
        "panel content is overwritten (new plot/label/source metadata), while the grid keeps a single ordered slot ",
        "for that ID in ", code("rv$gridplot_order"), ". A new ID creates a new slot."
      ),
      tags$li(
        code("label"), " is the user-visible panel label, and ", code("source"),
        " records the originating module for diagnostics and provenance."
      )
    ),
    p(
      "In practice, PCA, Venn and Volcano all derive ", code("id"),
      " from an optional ", code("grid_label"), " input (sanitized and prefixed with module-specific tokens). ",
      "This optional label allows users to create distinguishable panel IDs for variants of the same plot type (for example, ",
      "different conditions or styling runs) without introducing collisions. Leaving ", code("grid_label"),
      " empty intentionally falls back to a deterministic default ID so repeated exports replace the existing panel instead of ",
      "creating duplicates."
    ),

    h4("Shared structures in rv used by the Grid"),
    tags$ul(
      tags$li(
        code("rv$gridplot_selection"),
        " – consumed by ", code("selected_plots()"), " in ", code("Grid_module.R"),
        " and by ", code("get_include_map()"), " and diagnostic logging in ", code("Grid_layout.R"), "."
      ),
      tags$li(
        code("rv$gridplot_order"),
        " – drives iteration order in ", code("output$selection"),
        ", in span observers and in the optimisation logic using ",
        code("reorder_compact_fit()"), " and ", code("min_rows_for_cols()"), "."
      ),
      tags$li(
        code("rv$plot_spans"),
        " -- passed as ", code("settings$plot_spans"), " into ",
        code("compose_grid_with_spans_safe()"), ", ", code("compute_optimal_grid_layout()"),
        " and span-related heuristics."
      ),
      tags$li(
        code("rv$plot_margins"),
        " -- passed as ", code("settings$plot_margins"), " into ",
        code("apply_per_plot_margins()"), " which applies per-plot margin offsets ",
        "after alignment in both span-based and regular composition paths."
      )
    ),

    h3("Module Dependencies Actually Used"),
    p(
      "The Plot Grid relies on several packages that are loaded globally in ", code("app.R"),
      " and referenced from ", code("Grid_module.R"), ", ", code("Grid_layout.R"), ", ",
      code("Grid_legend.R"), " and ", code("Grid_composition.R"), ":"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# Core dependencies for Plot Grid

shiny
  # moduleServer(), reactiveValues(), observeEvent(), renderUI(), renderPlot(),
  # updateNumericInput(), showNotification().

ggplot2
  # Base plotting, used for:
  #   - placeholders (empty grids and error messages),
  #   - title/margin theming in prepare_plots_for_grid(),
  #   - legend detection via ggplot_build()/ggplot_gtable().

cowplot
  # Required for:
  #   - plot_grid() in compose_grid() (regular grid mode),
  #   - ggdraw(), draw_plot(), draw_plot_label() in compose_grid_with_spans_safe()
  #     (span-aware canvas composition).

svglite
  # Used in downloadHandler() for SVG exports:
  #   svglite::svglite(file, width = widthIn, height = heightIn).

later, sortable (from app.R)
  # Not used directly in Grid_module.R / Grid_layout.R / Grid_legend.R / Grid_composition.R in the referenced commit,
  # but relevant for general app behaviour and other modules.

# The Grid module also uses base R and methods:
#   - cat(), format(), tryCatch(), paste(), gsub(), etc.
#   - S3 class checks via inherits()."
    ),
    p(
      "The module performs a runtime check for ", code("cowplot"), " in ", code("modGridServer()"),
      " and issues a user‑visible notification if it is not installed:"
    ),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "observe({
  if (!requireNamespace(\"cowplot\", quietly = TRUE)) {
    showNotification(\"Package 'cowplot' is required for the grid composer.\",
                     type = \"error\", duration = 8)
    debug_log(\"cowplot not available - preview/download will show placeholder\", 1)
  }
})"
    ),

    h3("Diagnostics, Health Checks and Error Handling"),

    h4("Diagnostics at app level"),
    p(
      "The main app includes generic diagnostics for all modules, including Plot Grid. ",
      "Two helper functions in ", code("server()"), " of ", code("app.R"),
      " reference the Grid module explicitly:"
    ),
    tags$ul(
      tags$li(
        strong("Function existence check: "),
        code("check_module_prerequisites()"),
        " verifies that ", code("modGridServer"), " exists in ", code("modEnv"),
        " and logs remediation hints such as ",
        code("Try: source('modules/Grid_module.R', local = modEnv)"), "."
      ),
      tags$li(
        strong("File existence check: "),
        code("check_module_files()"),
        " verifies that ", code("modules/Grid_module.R"), " exists on disk and warns if it is missing."
      )
    ),

    h4("Error handling inside the Grid module"),
    p(
      "Within ", code("Grid_module.R"), ", ", code("Grid_layout.R"), ", ",
      code("Grid_legend.R"), " and ", code("Grid_composition.R"), ", critical operations are wrapped in ",
      code("tryCatch"), " and errors are surfaced via ", code("debug_log"), " and ",
      code("showNotification"), " without terminating the session:"
    ),
    tags$ul(
      tags$li(
        "Plot preparation (", code("prepare_plots_for_grid()"), ") and label building (",
        code("build_labels()"), ") are called inside ", code("tryCatch"),
        " blocks in both preview and download paths."
      ),
      tags$li(
        "Span layout construction and span‑aware composition (", code("compose_grid_with_spans_safe()"),
        ") are guarded; failures fall back to regular ", code("plot_grid"), " composition."
      ),
      tags$li(
        "Regular composition (", code("compose_grid()"), ") retries without alignment if the first attempt fails, ",
        "and finally returns a ggplot placeholder with an error message if no layout can be built."
      ),
      tags$li(
        "The download handler aborts gracefully when there are no plots or when composition returns ",
        code("NULL"), " and informs the user via notifications."
      )
    )
  )
}
