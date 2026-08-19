############
# Technical Documentation — Content (initial stubs)

render_heatmap_tech_overview_content_heatmap <- function() {
  div(
    h2("Technical Overview"),
    hr(),

    h3("Module Architecture"),
    p(
      "The Heatmap module creates aligned ComplexHeatmap views for abundance, protein correlation, sample correlation, ",
      "and optional basemean and abundance ratio summaries. ",
      "It is implemented as a Shiny module that consumes the processed data and metadata from the main app, ",
      "integrates optional pathway information from the GSEA and GO modules, and exposes a plotting API and Plot Grid integration."
    ),

    h4("Module Structure:"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "app.R                                        # top-level app; creates modEnv, rv, calls modHeatmapServer()
modules/Heatmap_module.R                     # module shell: public UI/server API, orchestration,
                                             #   inline data-access reactives, central ordering reactive,
                                             #   and inline renderPlot/observer bindings for grid outputs
modules/Heatmap/Heatmap_ui.R                 # create_heatmap_ui(ns): full UI definition
                                             #   (sidebar filters, plot tabs, download controls, grid integration)
modules/Heatmap/Heatmap_reactive_state.R     # all reactiveVal() declarations; sourced first, before every other sub-script
modules/Heatmap/Heatmap_creation.R           # UI-bound helpers: colour/font extraction, heatmap object builders,
                                             #   draw-padding helper; depends on heatmap_debug_log in closure
modules/Heatmap/Heatmap_create_expression.R  # expression heatmap creation flow and associated state updates
modules/Heatmap/Heatmap_create_correlation.R # protein and sample correlation heatmap flows;
                                             #   axis synchronization and validation helpers
modules/Heatmap/Heatmap_rendering.R          # draw functions (draw_grid_expr_corr_ui, draw_grid_expr_sample_ui),
                                             #   single-panel parity bundle helpers, refresh cache logic
modules/Heatmap/Heatmap_observers.R          # ordered observer loader/coordinator
modules/Heatmap/Heatmap_observers_data_choices.R      # data and selector choices
modules/Heatmap/Heatmap_observers_protein_selection.R # protein/pathway selection
modules/Heatmap/Heatmap_observers_plot_lifecycle.R    # plot creation and lifecycle
modules/Heatmap/Heatmap_observers_restore.R           # session restore
modules/Heatmap/Heatmap_download.R           # download handlers and Plot Grid integration (add_to_grid observer)
modules/Heatmap/Heatmap_utils.R              # pure utility functions: clustering, correlation helpers,
                                             #   basemean/ratio calculation, protein annotation, Plot Grid export;
                                             #   must not directly access module-scope input/output/reactiveVal
modules/Heatmap/Heatmap_statistical_analysis.R # statistical analysis helpers"
    ),

    h3("Data Flow"),

    h4("App-level Integration (app.R)"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
      "app.R:
  • creates global environment modEnv and loads Heatmap files into it:
      sys.source(\"modules/Heatmap_module.R\", envir = modEnv)
  • creates rv <- reactiveValues() as the central data hub
  • initializes the Heatmap module inside the main server:
      module_outputs$heatmap_out <- tryCatch({
        modEnv$modHeatmapServer(
          id           = \"heatmap\",
          rv           = rv,
          res_GSEA     = if (!is.null(module_outputs$gsea_out)) {
                           reactive({
                             if (\"get_results\" %in% names(module_outputs$gsea_out)) {
                               module_outputs$gsea_out$get_results()
                             } else NULL
                           })
                         } else NULL,
          GO_res       = if (!is.null(module_outputs$go_out)) {
                           reactive({
                             if (\"get_results\" %in% names(module_outputs$go_out)) {
                               module_outputs$go_out$get_results()
                             } else NULL
                           })
                         } else NULL,
          module_outputs = module_outputs,
          debug_level    = DEBUG_LEVEL,
          modEnv         = modEnv
        )
      }, error = function(e) {
        debug_log(\"Error initializing Enhanced Heatmap module:\", 1)
        showNotification(\"Enhanced Heatmap module failed to load\", type = \"warning\")
        NULL
      })
  • passes Heatmap outputs into the Plot Grid module via the shared rv/modEnv environment"
    ),

    h4("Within the Heatmap Module"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
      "modHeatmapServer(\"heatmap\", rv, res_GSEA, GO_res, module_outputs, debug_level, modEnv)
      ↓
Setup (sourcing order is a strict runtime requirement):
  # Before moduleServer() — available in module-file scope:
  source(\"./modules/Heatmap/Heatmap_ui.R\", local = TRUE)   # UI builder
  source(\"./R/utils.R\",                    local = TRUE)   # shared app utilities

  # Inside moduleServer() closure — in strict dependency order:
  1. source(\"./modules/Heatmap/Heatmap_utils.R\",              local = TRUE)
     # Pure utilities; uses heatmap_debug_log from closure; must precede all creation scripts.
  2. source(\"./modules/Heatmap/Heatmap_statistical_analysis.R\", local = TRUE)
     # Statistical analysis helpers; depends on Heatmap_utils.R.
  3. source(\"./modules/Heatmap/Heatmap_reactive_state.R\",     local = TRUE)
     # MUST be sourced first among state/creation scripts: defines every reactiveVal().
  4. source(\"./modules/Heatmap/Heatmap_creation.R\",           local = TRUE)
     # UI-bound helpers and heatmap builders; depends on reactive state symbols.
  5. source(\"./modules/Heatmap/Heatmap_create_expression.R\",  local = TRUE)
     # Expression heatmap creation; depends on reactive state and creation helpers.
  6. source(\"./modules/Heatmap/Heatmap_create_correlation.R\", local = TRUE)
     # Correlation heatmap creation; depends on expression context and shared state.
  7. source(\"./modules/Heatmap/Heatmap_rendering.R\",          local = TRUE)
     # Draw functions and single-panel bundle helpers; depends on creation scripts.
  8. source(\"./modules/Heatmap/Heatmap_observers.R\",          local = TRUE)
     # Ordered observer loader; sources data choices, protein selection, plot lifecycle, and restore peers.
  9. source(\"./modules/Heatmap/Heatmap_download.R\",           local = TRUE)
     # Download handlers and add-to-grid observer; depends on fixed-heatmap state.

  # Before source #5, an observeEvent keeps heatmap_applied_sort_state in sync with
  # the sort UI controls so that draw paths always use the last committed sort settings.

  # define heatmap_debug_log(): module-scoped logger prefixed \"[ HEATMAP MODULE ... ]\";
  #   debug_log is a legacy alias for helper signatures that still accept debug_log.

  • create reactiveVals for (all defined in Heatmap_reactive_state.R):
      - expression matrix and correlation matrices
      - fixed ComplexHeatmap objects (expression, protein correlation, sample correlation, basemean, ratio)
      - shared row/column order vectors for alignment
      - applied sort state (heatmap_applied_sort_state): stores the sort_proteins_by and
        sort_samples_by values that were used for the last successful create run;
        draw functions read this instead of live input values to avoid transient redraws
      - basemean and abundance ratio vectors
      - protein selection and highlighting state
      - plot containers and cluster information
      - selected row indices for extension heatmaps
      - single-tab refresh cache (heatmap_single_tab_refresh_cache) for avoiding redundant rebuilds

Data access via rv (reactive wrappers):
  heatmap_data_modified()        ← rv$data_mod      (processed data table)
  heatmap_df_data_definition()   ← rv$data_def      (column definitions / metadata)
  abundance_validate()           ← rv$ab_validate   (optional; passed through as-is)

Dynamic UI population (Heatmap_ui.R + observers):
  • populate abundance type selector from data_def$Content (Normalized, Raw, Batch Corrected, Imputed*)
  • populate identifier selector from data_def rows marked as Identifier
  • populate sample choices based on the selected abundance type and non-NA Sample entries
  • enable/disable and fill p-value and ratio selectors based on Content and on presence/validity of numeric data
  • populate GSEA and GO pathway selectors from res_GSEA() and GO_res(), handling both data.frame and enrichResult-like structures

Main analysis trigger:
  User clicks input$create_heatmap_btn
      ↓
  • perform statistical filtering and row selection on rv$data_mod using:
      - missing value filter (selected samples only)
      - optional p-value filter (Content-based, with validity checks and type/column selection)
      - optional abundance ratio filter (Content == \"Abundance Ratio\", absolute log2 ratio threshold)
      - optional identifier filter (custom identifier list)
      - maximum number of proteins (p-value based ranking if available, random subset otherwise)
  • store selected row indices for later basemean/ratio computation
  • build the expression z-score matrix (log2 transform + per-row scaling) for selected proteins × selected samples
  • compute clustering information for rows and columns

Heatmap creation:
  • create Abundance ComplexHeatmap object from the z-score matrix, including optional row/column dendrograms
  • compute and build aligned Protein Correlation heatmap:
      - Pearson correlation over rows of the expression matrix
      - optional dendrograms based on correlation distances
      - optional diagonal line (colour, width, orientation)
  • compute and build aligned Sample Correlation heatmap:
      - Pearson correlation over columns of the expression matrix
      - order columns according to clustering information from expression
      - optional diagonal line

Grid combination and fixed layouts:
  • combine expression + protein correlation horizontally, and optionally add basemean and ratio single-column heatmaps on the right
  • combine expression + sample correlation vertically in an alternative grid
  • store fixed ComplexHeatmap objects and shared row/column orders so that individual tabs and downloads reuse the same layout

Extension heatmaps (single columns):
  • based on selected row indices and rv$data_mod / rv$data_def:
      - compute log2 basemean per protein (respecting selected samples and abundance type)
      - retrieve log2 abundance ratio per protein from the selected ratio column
  • build one-column ComplexHeatmap objects aligned to the expression rows

Export and integration:
  • expose expression, protein correlation, sample correlation, basemean and ratio matrices via return list
  • provide a unified download handler for grid and individual views (PNG, JPEG, TIFF, PDF, SVG)
  • integrate with the Plot Grid module by converting ComplexHeatmap objects to raster images and wrapping them in ggplot for modEnv$add_to_grid()"
    ),

    h3("Key Components"),

    h4("Initialization, Debugging and Cleanup"),
    tags$ul(
      tags$li(
        strong("Debug logger: "),
        "the server receives ", code("debug_level"), " from app.R and stores it as ", code("DEBUG_LEVEL"),
        ", a module-local constant. ",
        code("heatmap_debug_log(message, level = 1)"), " is the primary scoped logger; it prints timestamped messages ",
        "prefixed with ", code("[ HEATMAP MODULE ... ]"),
        " whenever ", code("level <= DEBUG_LEVEL"), ". ",
        code("debug_log"), " is a legacy alias bound immediately after, for helper signatures that still accept ", code("debug_log"), ". ",
        "Level 1 covers control flow, state transitions, and validation checkpoints. ",
        "Level 2 covers matrix dimensions and intermediate diagnostics. ",
        "Direct use of ", code("print()"), ", ", code("message()"), ", ", code("warning()"), ", or ", code("cat()"),
        " outside ", code("heatmap_debug_log"), " is not permitted within the module."
      ),
      tags$li(
        strong("Module-scoped reactiveVals: "),
        "all reactiveVal objects are declared in ", code("Heatmap_reactive_state.R"), " and are visible to every ",
        "sub-script sourced inside the same ", code("moduleServer()"), " closure. ",
        "They cover matrices, fixed ComplexHeatmap objects, shared ordering vectors, applied sort state, ",
        "basemean/ratio values, protein selections and highlighting, plot containers, cluster metadata, ",
        "and the single-tab refresh cache. None are exported outside the module."
      ),
      tags$li(
        strong("Reset behaviour: "),
        "the reset button observer clears all module-internal reactiveVals (including fixed layouts and extension values) ",
        "and restores key UI inputs (sample groups, FDR threshold, basemean filter, max proteins, custom protein input, download dimensions) ",
        "to their default values. A notification confirms the reset."
      )
    ),

    h4("Data Access via rv and Internal Reactives"),
    tags$ul(
      tags$li(
        strong("Primary data sources: "),
        "reactive wrappers around ", code("rv$data_mod"),
        " and ", code("rv$data_def"),
        " provide the processed quantitative table and the associated metadata. ",
        "If these are NULL, the wrappers validate and abort downstream operations with a descriptive message."
      ),
      tags$li(
        strong("Abundance and sample mapping: "),
        "helpers inspect the metadata to find abundance columns that match the selected data type and the selected samples. ",
        "Sample groups for ratio-style calculations are derived by intersecting sample names with these abundance columns."
      ),
      tags$li(
        strong("Expression matrix store: "),
        "after z-score computation, the scaled expression matrix is cached for later use by correlation calculations, ",
        "single-view tabs and export functions."
      ),
      tags$li(
        strong("Fixed heatmap objects: "),
        "the module keeps separate reactiveVals for the fixed Abundance, Protein Correlation, Sample Correlation, Basemean and Abundance Ratio ",
        "ComplexHeatmap objects. ",
        "These are used for consistent rendering across tabs and for exporting the exact layout shown in the grid views."
      )
    ),

    h3("Extension Points"),
    tags$dl(
      tags$dt("Adding a new heatmap type"),
      tags$dd(
        "Declare a new ", code("reactiveVal(NULL)"), " for its data and one for its fixed ComplexHeatmap object in ",
        code("Heatmap_reactive_state.R"), ". ",
        "Add the builder function (following the pattern of ", code("create_basemean_heatmap"), ") in ",
        code("Heatmap_creation.R"), ". ",
        "Call the builder and store the fixed object inside ", code("run_heatmap_creation()"), " in ",
        code("Heatmap_observers_plot_lifecycle.R"), ". ",
        "Add a ", code("renderPlot"), " output binding in ", code("Heatmap_module.R"), " and the corresponding tab panel in ",
        code("Heatmap_ui.R"), ". ",
        "Extend the download handler in ", code("Heatmap_download.R"), " and the grid export helper ",
        code("get_current_heatmap_for_grid()"), " in ", code("Heatmap_utils.R"), " to cover the new tab."
      ),
      tags$dt("Adding new UI controls to the sidebar"),
      tags$dd(
        "Add the input element in ", code("create_heatmap_ui()"), " in ", code("Heatmap_ui.R"), ". ",
        "If the control drives rendering, read its value inside the relevant draw function in ",
        code("Heatmap_rendering.R"), " or the builder in ", code("Heatmap_creation.R"), ". ",
        "If it requires a new observer, add it in the appropriate focused observer peer loaded by ", code("Heatmap_observers.R"), ". ",
        "If the control should reset with the module, add an ", code("updateInput"), " call in the reset observer in ",
        code("Heatmap_module.R"), "."
      ),
      tags$dt("Adding new rendering or download functionality"),
      tags$dd(
        "New shared draw logic (used by both ", code("renderPlot"), " and the download handler) belongs in ",
        code("Heatmap_rendering.R"), " as a function that operates on the closure-scope fixed heatmap objects. ",
        "New download formats or export targets are handled in ", code("Heatmap_download.R"), ". ",
        "Helper functions that do not access module-scope ", code("input"), ", ", code("output"), ", or ",
        code("reactiveVal"), " directly belong in ", code("Heatmap_utils.R"), "."
      )
    ),

    h3("Developer Troubleshooting"),
    tags$dl(
      tags$dt("Sourcing order error: symbol not found"),
      tags$dd(
        "If a sub-script references a reactiveVal, draw function, or helper that has not been sourced yet, ",
        "R will throw a 'object not found' error at the point the sourced code executes. ",
        "Always maintain the sourcing order documented above: ",
        code("Heatmap_reactive_state.R"), " first, then ", code("Heatmap_creation.R"), ", then creation scripts, ",
        "then rendering, then observers, then download. Moving a source call earlier in this chain is safe; ",
        "moving it later will break code that depends on its symbols."
      ),
      tags$dt("Closure-scope assumption failures"),
      tags$dd(
        "All sub-scripts are sourced with ", code("local = TRUE"), " inside the same ", code("moduleServer()"), " call. ",
        "They share the same closure and can read ", code("input"), ", ", code("output"), ", ", code("session"), ", ",
        "all reactiveVals, and ", code("heatmap_debug_log"), " without passing them as arguments. ",
        "Functions defined in ", code("Heatmap_utils.R"), " are an intentional exception: they must receive all ",
        "needed values as explicit arguments and must not reference closure-scope symbols directly. ",
        "Moving logic that accesses ", code("input"), " or a reactiveVal into ", code("Heatmap_utils.R"), " will produce ",
        "'object not found' errors when that helper is called outside the module closure."
      ),
      tags$dt("Draw function produces a stale or incorrect order"),
      tags$dd(
        "Draw functions in ", code("Heatmap_rendering.R"), " read ", code("heatmap_applied_sort_state()"),
        " rather than live ", code("input$sort_proteins_by"), " / ", code("input$sort_samples_by"), " values. ",
        code("heatmap_applied_sort_state"), " is only updated at the end of a successful ", code("run_heatmap_creation()"), " run. ",
        "If a sort setting was changed in the UI but ", code("Create Plot"), " was not clicked, ",
        "the draw functions will use the previous committed sort settings. This is intentional."
      ),
      tags$dt("Fixed heatmap objects are NULL on a single tab"),
      tags$dd(
        "Single-view tabs (Abundance Only, Protein Correlation Only, etc.) read from the fixed-heatmap reactiveVals ",
        "populated by the grid draw paths. If a grid has not been drawn yet, those reactiveVals are NULL and the single tab ",
        "shows a placeholder message. The fix is to visit the grid tab or click ", code("Create Plot"),
        " first so that the fixed objects are populated."
      ),
      tags$dt("Download or add-to-grid produces a blank or error output"),
      tags$dd(
        "Both operations require fixed heatmap objects to be non-NULL. ",
        "Verify that heatmaps have been created by clicking ", code("Create Plot"),
        " and that the target tab has been rendered at least once. ",
        "Check the R console for ", code("heatmap_debug_log"), " messages at level 1 for the specific failure point."
      )
    )
  )
}

render_heatmap_tech_functions_content_heatmap <- function() {
  div(
    h2("Functions Reference"),
    hr(),

    h3("UI Composition"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "create_heatmap_ui(ns)
  # Main Heatmap UI definition (modules/Heatmap/Heatmap_ui.R):
  #  - Top-level:
  #      * Tab navigation between:
  #           - Overview, Protein selection & plotting, Customization, Plot Types, Technical docs (in Documentation/heatmap_doc.R)
  #      * Main Heatmap tab (used by modHeatmapUI()):
  #           - Sidebar (left):
  #                • Data and identifier selection:
  #                     - selectInput(ns('custom_col_sel_heatmap'))    # abundance data type
  #                     - selectInput(ns('GeneIdentifierColumn_Heatmap'))  # identifier column
  #                     - selectizeInput(ns('select_samples_heatmap'), multiple = TRUE)  # samples
  #                • Statistical filter options:
  #                     - checkboxInput(ns('enable_pvalue_filter_heatmap'))
  #                     - selectInput(ns('pval_type_heatmap'))
  #                     - selectInput(ns('pval_col_heatmap'))
  #                     - numericInput(ns('fdr_Heatmap'))             # p-value / FDR threshold
  #                     - checkboxInput(ns('enable_ratio_filter_heatmap'))
  #                     - selectInput(ns('abundance_ratio_col_heatmap'))
  #                     - selectInput(ns('ratio_filter_mode_heatmap')) # large/small |log2(ratio)|
  #                     - numericInput(ns('ratio_threshold_heatmap'))
  #                     - checkboxInput(ns('remove_missing_values_heatmap'))
  #                     - numericInput(ns('base_mean_Heatmap'))       # basemean threshold
  #                     - numericInput(ns('max_proteins_heatmap'))    # Max proteins to display
  #                     - textAreaInput(ns('identifier_filter_heatmap'))  # Enter proteins (Identifier Filter)
  #                • Protein Selection Panel:
  #                     - textAreaInput(ns('input_Heatmap'))          # Search for Gene Symbols
  #                     - verbatimTextOutput(ns('geneSymbolList_Heatmap'))  # Suggested Identifiers
  #                     - actionButton(ns('transferButton_Heatmap'), 'Add')
  #                     - actionButton(ns('removeButton_Heatmap'), 'Remove')
  #                     - actionButton(ns('clearButton_Heatmap'), 'Clear')
  #                     - actionButton(ns('copyBtn_Heatmap'), 'Copy to Clipboard')
  #                     - selectInput(ns('GSEA_Heatmap'), multiple = TRUE)  # Import proteins of enriched GSEA pathways
  #                     - selectInput(ns('GO_Heatmap'),   multiple = TRUE)  # Import proteins of enriched GO pathways
  #                     - checkboxInput(ns('Intersect_Heatmap'))      # Intersecting proteins
  #                     - checkboxInput(ns('CoreEnriched_Heatmap'))   # Core enriched (GSEA)
  #                     - actionButton(ns('Protein_Input_Heatmap'), 'Add Pathway proteins')
  #                     - dataTableOutput(ns('selectedGene_Heatmap')) # Selected Proteins (summary)
  #                • Highlighting and custom lists:
  #                     - textAreaInput(ns('custom_proteins_input'))  # Custom proteins (comma/line separated)
  #                     - actionButton(ns('highlightProteins_Heatmap'), 'Highlight Proteins')
  #                     - actionButton(ns('undoHighlight_Heatmap'), 'Undo')
  #                • Sorting and layout:
  #                     - selectInput(ns('sort_proteins_by'),
  #                                   choices = c('Z-Score' = 'z_score','Pearson r' = 'pearson_r'))
  #                     - checkboxInput(ns('show_row_dendrogram'))
  #                     - checkboxInput(ns('show_column_dendrogram'))
  #                • Label controls:
  #                     - checkboxInput(ns('show_expr_row_labels'))
  #                     - checkboxInput(ns('show_expr_col_labels'))
  #                     - checkboxInput(ns('show_corr_row_labels'))
  #                     - checkboxInput(ns('show_corr_col_labels'))
  #                     - checkboxInput(ns('show_abundance_ratio_row_labels'))
  #                     - checkboxInput(ns('show_abundance_ratio_col_labels'))
  #                • Sorting and layout (continued):
  #                     - selectInput(ns('sort_samples_by'),
  #                                   choices = c('None'='none','Alpha A-Z'='alpha_asc',
  #                                               'Alpha Z-A'='alpha_desc',
  #                                               'Pearson clustering'='pearson_cluster',
  #                                               'Distance clustering'='distance_cluster',
  #                                               'PCA1 asc'='pca1_asc','PCA1 desc'='pca1_desc'))
  #                • Colour and legend controls:
  #                     - colourpicker::colourInput(ns('Heatmap_ColorInput_1'))  # Low colour
  #                     - colourpicker::colourInput(ns('Heatmap_ColorInput_2'))  # Mid colour
  #                     - colourpicker::colourInput(ns('Heatmap_ColorInput_3'))  # High colour
  #                     - checkboxInput(ns('correlation_enhanced_contrast'))
  #                     - selectInput(ns('legend_side_heatmap'),
  #                                   choices = c('Right'='right','Left'='left','Top'='top','Bottom'='bottom'))
  #                • Diagonal line options (correlation heatmaps):
  #                     - checkboxInput(ns('show_correlation_diagonal'))
  #                     - colourInput(ns('diagonal_line_color'))
  #                     - sliderInput(ns('diagonal_line_width'), ...)
  #                     - checkboxInput(ns('rotate_diagonal_line'))
  #                • Basemean / ratio visibility:
  #                     - checkboxInput(ns('show_basemean_heatmap'))
  #                     - checkboxInput(ns('show_abundance_ratio_heatmap'))
  #                • Font sizes:
  #                     - numericInput(ns('row_font_size_heatmap'))
  #                     - numericInput(ns('col_font_size_heatmap'))
  #                     - numericInput(ns('legend_title_font_size_heatmap'))
  #                     - numericInput(ns('legend_text_font_size_heatmap'))
  #                • Actions:
  #                     - actionButton(ns('create_heatmap_btn'), 'Create Plot')
  #                     - actionButton(ns('reset_heatmap_btn'),  'Reset Heatmap Module')
  #
  #           - Main panel (right):
  #                • Tabset for:
  #                     - Abundance + Correlation Grid (output$heatmap_grid)
  #                     - Abundance + Sample Grid (output$heatmap_expr_sample_grid)
  #                     - Abundance Only (output$test_expression_only)
  #                     - Protein Correlation Only (output$test_protein_cor_only)
  #                     - Sample Correlation Only (output$test_sample_cor_only)
  #                     - Basemean Only (output$test_basemean_only)
  #                     - Abundance Ratio Only (output$test_abundance_ratio_only)
  #                • Each tab has a reactive title (e.g. output$grid_expr_corr_title) and a renderPlot().
  #                • Download controls:
  #                     - numericInput(ns('resolution_DPI_Heatmaps'))
  #                     - numericInput(ns('plotWidthInch_Heatmaps'))
  #                     - numericInput(ns('plotHeightInch_Heatmaps'))
  #                     - selectInput(ns('downloadFormat_Heatmaps'),
  #                                   choices = c('pdf','png','jpeg','tiff','svg'))
  #                     - downloadButton(ns('downloadPlotButton_Heatmaps'))
  #                     - textOutput(ns('current_tab_info'))
  #                • Plot Grid integration:
  #                     - textInput(ns('grid_label'))
  #                     - actionButton(ns('add_to_grid'), 'Add to Plot Grid')
  #                • Debug section:
  #                     - textOutput(ns('debug_info'))
  #                     - textOutput(ns('raw_debug_log'))
  #                     - (optional debug outputs are implementation-dependent in this final module version)"
    ),

    h3("UI & Plot Helper Functions (Server-Side)"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "extract_core_heatmap(ht_obj)
  # In Heatmap_utils.R:
  #  - If 'ht_obj' is a ComplexHeatmap::HeatmapList, extracts the first Heatmap.
  #  - If 'ht_obj' is already a Heatmap, returns it unchanged.
  #  - Used before accessing '@matrix' or drawing with ComplexHeatmap::draw().

legend_side_from_input(input)
legend_direction_from_input(input)
  # In Heatmap_creation.R:
  #  - legend_side_from_input():
  #       * Reads input$legend_side_heatmap and normalizes it to one of 'right','left','top','bottom'.
  #         Defaults to 'right' if input is missing/invalid.
  #  - legend_direction_from_input():
  #       * Returns 'horizontal' for legend_side in {'top','bottom'}, otherwise 'vertical'.

extract_font_settings(input)
  # In Heatmap_creation.R:
  #  - Reads numeric inputs (with legacy-ID fallbacks):
  #       input$row_font_size, input$col_font_size,
  #       input$legend_title_font_size, input$legend_text_font_size.
  #  - Returns a list(row_font_size, col_font_size, show_row_dend, show_col_dend,
  #                   legend_title_font_size, legend_text_font_size)
  #    with defaults if values are NULL or invalid.

extract_color_scheme(input)
  # In Heatmap_creation.R:
  #  - Reads colour inputs:
  #       input$Heatmap_ColorInput_1  (Low colour),
  #       input$Heatmap_ColorInput_2  (Mid colour),
  #       input$Heatmap_ColorInput_3  (High colour).
  #  - Returns a 3-element character vector c(low, mid, high) with fallbacks if needed.

heatmap_draw_padding_from_input(input, base_padding_mm = c(2, 2, 2, 2))
  # In Heatmap_creation.R:
  #  - Computes the padding unit vector passed to ComplexHeatmap::draw().
  #  - Inspects legend position and font sizes to determine adaptive padding.
  #  - Returns a grid::unit() object used as the 'padding' argument in draw calls.

safe_gp(size)
  # In Heatmap_utils.R:
  #  - Safely constructs a grid::gpar(fontsize = size) object.
  #  - If 'size' is NULL or non-finite, uses a default value.
  #  - Used for row/column label fonts and legend fonts in ComplexHeatmap."
    ),

    h3("Server / Workflow Helpers"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "compute_expression_clustering(scaled_matrix, input)
  # In Heatmap_utils.R:
  #  - Input: z-scored expression matrix (proteins x samples).
  #  - Computes row and column clustering based on Euclidean distances.
  #  - Returns a list with row_order_idx and col_order_idx used later to align
  #    sample correlation heatmaps and store clustering state.

create_expression_heatmap_object(scaled_matrix, groups = NULL, input, cluster_info)
  # In Heatmap_creation.R:
  #  - Builds a ComplexHeatmap::Heatmap object for the z-scored expression matrix.
  #  - Uses extract_color_scheme(input) for a symmetric colour scale around 0.
  #  - Uses extract_font_settings(input) for label sizes and legend fonts.
  #  - Uses cluster_info for row/column dendrograms (show/hide from input).
  #  - Returns a Heatmap object (no drawing).

create_protein_annotation(expr_row_names, highlighted_proteins, font_size, label_padding_mm)
  # In Heatmap_utils.R:
  #  - Creates a ComplexHeatmap row annotation that marks highlighted proteins.
  #  - expr_row_names: character vector of protein labels in expression row order.
  #  - highlighted_proteins: identifiers to annotate.
  #  - Returns an annotation Heatmap that can be prepended with '+' to an expression Heatmap.

col_fun_for_correlation(palette, enhanced_contrast = FALSE)
  # In Heatmap_utils.R:
  #  - Builds a colour function for correlation matrices using circlize::colorRamp2().
  #  - If enhanced_contrast = FALSE: linear mapping from -1 to 1 with the three palette colours.
  #  - If enhanced_contrast = TRUE:
  #       * Creates 51 values from -1 to 1, applies a tanh-based sigmoid transformation.
  #       * Result: colours near 0 are compressed; strong correlations are visually emphasized.

create_diagonal_line_cell_fun(input)
  # In Heatmap_creation.R:
  #  - Returns a ComplexHeatmap cell_fun that optionally draws a diagonal line
  #    on correlation heatmaps:
  #       * input$show_correlation_diagonal controls whether to draw.
  #       * input$diagonal_line_color and input$diagonal_line_width set appearance.
  #       * input$rotate_diagonal_line flips to the anti-diagonal when TRUE.
  #  - Passed as the 'cell_fun' argument to protein and sample correlation Heatmaps.

calculate_correct_basemean_internal(selected_row_indices, loadedData, data_def, input)
  # In Heatmap_utils.R:
  #  - Computes log2 mean abundance (basemean) for the selected proteins.
  #  - Uses input$custom_col_sel_heatmap and input$select_samples_heatmap to identify columns.
  #  - Computes mean across selected sample columns per row, then log2(mean + small offset).
  #  - Returns a named numeric vector (names = protein labels) or NULL if no valid data.

calculate_abundance_ratios_internal(selected_row_indices, loadedData, data_def, input)
  # In Heatmap_utils.R:
  #  - Extracts log2 abundance ratios for the selected proteins from input$abundance_ratio_col_heatmap.
  #  - Ensures values are finite and positive before applying log2 if not already log-scaled.
  #  - Returns a named numeric vector (names = protein labels) or NULL if no valid data.

create_basemean_heatmap(basemean_values, input)
create_abundance_ratio_heatmap(ratio_values, input)
  # In Heatmap_creation.R:
  #  - Build single-column ComplexHeatmap objects for log2(Basemean) and log2(Abundance Ratio).
  #  - Use extract_color_scheme(input) for a three-colour gradient over observed values.
  #  - Respect row/column label toggles and font settings from extract_font_settings(input).
  #  - Return Heatmap objects or NULL when values are missing."
    ),

    h3("Ordering, Alignment and Grid Rendering"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "compute_current_ordering()   [reactive]
  # In Heatmap_module.R:
  #  - Uses heatmap_plots() and input$sort_proteins_by to derive a deterministic
  #    row/column name order used by single-column tabs and dynamic titles.
  #       * 'z_score':  row order from heatmap_shared_row_order() (or expression matrix rownames);
  #                     col order from heatmap_shared_col_order() (or expression matrix colnames).
  #       * 'pearson_r': row order via compute_pearson_r_leaf_order(expr_matrix);
  #                      col order from heatmap_shared_col_order() when available.
  #  - Returns list(row_order = <character>, col_order = <character>, method = <string>).

compute_pearson_r_leaf_order(expr_matrix)
  # In Heatmap_utils.R:
  #  - Computes protein-protein Pearson correlation from expression matrix rows.
  #  - Runs hclust on the correlation distance matrix (1 - r) to derive a leaf order.
  #  - Returns list(order = <character vector of protein names>, dendrogram = <dendrogram>)
  #    or NULL on failure. Used by ordering logic and draw paths for Pearson r sort mode.

verify_heatmap_alignment()
  # In Heatmap_module.R:
  #  - Debug helper that checks protein and sample correlation matrix axes against
  #    the expression matrix dimensions and logs alignment flags via heatmap_debug_log().

draw_grid_expr_corr_ui()
  # In Heatmap_rendering.R:
  #  - Shared draw function for the Abundance + Protein Correlation grid.
  #  - Reads heatmap_applied_sort_state() (not live input) to determine row/column order.
  #  - Reorders the expression matrix, builds a fixed Abundance Heatmap and an aligned
  #    Protein Correlation Heatmap; optionally appends Basemean and Ratio columns and
  #    a protein annotation Heatmap for highlighted proteins.
  #  - Draws the combined HeatmapList via ComplexHeatmap::draw() with legend position and
  #    adaptive legend_gap derived from font sizes.
  #  - Stores the resulting fixed ComplexHeatmap objects in the shared reactiveVals so that
  #    single-tab renders and downloads can reuse the same layout.
  #  - Called by output$heatmap_grid (renderPlot in Heatmap_module.R) and by the download path.

draw_grid_expr_sample_ui()
  # In Heatmap_rendering.R:
  #  - Shared draw function for the Abundance + Sample Correlation vertical grid.
  #  - Reads heatmap_applied_sort_state() for ordering.
  #  - Builds a fixed Abundance Heatmap and an aligned Sample Correlation Heatmap;
  #    stacks them vertically with %v% and draws with shared legend settings.
  #  - Stores fixed objects and shared sample column order for single-tab reuse.
  #  - Called by output$heatmap_expr_sample_grid and the download path.

build_single_panel_parity_bundle(current_tab, context)
  # In Heatmap_rendering.R:
  #  - Prepares a 'bundle' (list of fixed heatmap object + ordering metadata) for
  #    the specified tab, ensuring fixed heatmaps are populated via
  #    refresh_fixed_heatmaps_for_single_tab() if needed.
  #  - Returns a list used by draw_single_panel_from_bundle().

draw_single_panel_from_bundle(bundle, panel_tag, context, row_label_toggle, col_label_toggle)
  # In Heatmap_rendering.R:
  #  - Draws a single fixed Heatmap (expression, protein correlation, or sample correlation)
  #    from a parity bundle.
  #  - Applies label toggles before drawing to override the stored label settings.
  #  - Used by the individual-tab renderPlot outputs and their matching download paths
  #    to guarantee pixel-for-pixel parity between screen and export.

refresh_fixed_heatmaps_for_single_tab(current_tab, context)
  # In Heatmap_rendering.R:
  #  - Ensures fixed heatmap objects are populated before a single-tab render or download.
  #  - Opens a temporary graphics device and calls the matching grid draw path:
  #       * expression/protein/basemean/ratio tabs -> draw_grid_expr_corr_ui()
  #       * sample-correlation tab                 -> draw_grid_expr_sample_ui()
  #  - Maintains a cache key (sort mode, sample sort mode, dendrogram toggles, create counter)
  #    via heatmap_single_tab_refresh_cache() to skip redundant rebuilds.

output$heatmap_grid (renderPlot)
  # In Heatmap_module.R:
  #  - Delegates entirely to draw_grid_expr_corr_ui().

output$heatmap_expr_sample_grid (renderPlot)
  # In Heatmap_module.R:
  #  - Delegates entirely to draw_grid_expr_sample_ui().

output$test_expression_only / $test_protein_cor_only / $test_sample_cor_only (renderPlot)
  # In Heatmap_module.R:
  #  - Each calls build_single_panel_parity_bundle() then draw_single_panel_from_bundle()
  #    for their respective panel_tag.

output$test_basemean_only / $test_abundance_ratio_only (renderPlot)
  # In Heatmap_module.R:
  #  - Read from heatmap_fixed_basemean() / heatmap_abundance_ratio_values() and apply
  #    compute_current_ordering() to align the single column with the current row order.
  #  - Draw inline (not via draw_single_panel_from_bundle) because ordering is applied
  #    dynamically at render time rather than stored in a fixed object.

output$heatmap_grid (renderPlot)
  # In Heatmap_module.R:
  #  - Renders the 'Abundance + Protein Correlation (+ Basemean/Ratio)' grid:
  #       1) Derives ordering based on input$sort_proteins_by
  #          (z-score, Pearson r, or Custom with input$custom_protein_fallback_sort).
  #       2) Reorders the expression matrix accordingly.
  #       3) Builds a fixed Abundance Heatmap (expr_ht_fixed) with:
  #            - extracted colour scheme,
  #            - optional dendrograms controlled by input$show_row_dendrogram
  #              and input$show_column_dendrogram.
  #       4) Computes aligned Protein Correlation matrix from the reordered expression matrix.
  #       5) Builds a Protein Correlation Heatmap with:
  #            - col_fun_for_correlation(), enhanced contrast toggle,
  #            - diagonal line via create_diagonal_line_cell_fun(input),
  #            - label toggles via input$show_corr_row_labels / show_corr_col_labels.
  #       6) Optionally appends Basemean and Ratio single-column Heatmaps (if values were computed
  #          and respective 'show_*_heatmap' checkboxes are TRUE).
  #       7) Optionally appends a protein annotation Heatmap for highlighted proteins.
  #       8) Draws the combined HeatmapList via ComplexHeatmap::draw() with:
  #            - heatmap_legend_side and annotation_legend_side set from legend_side_from_input().
  #            - Adaptive legend_gap (mm) if legend direction is horizontal (based on font sizes).

output$heatmap_expr_sample_grid (renderPlot)
  # In Heatmap_module.R:
  #  - Renders the 'Abundance + Sample Correlation' vertical grid:
  #       1) Determines ordering similarly to output$heatmap_grid, including Custom fallback ordering.
  #       2) Builds a fixed Abundance Heatmap (rows=proteins, cols=samples).
  #       3) Computes a sample-sample correlation matrix from the reordered expression matrix.
  #       4) Builds a Sample Correlation Heatmap with:
  #            - correlation colour function and enhanced contrast toggle,
  #            - diagonal line cell_fun for self-correlations,
  #            - label toggles for row/column names.
  #       5) Stacks Abundance (top) and Sample Correlation (bottom) using %v% and draws
  #          with shared legend position and adaptive legend_gap when horizontal."
    ),

    h3("Integration with Plot Grid and Downloads"),
    pre(
      style = "background:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "get_current_heatmap_for_grid(input, heatmap_fixed_expression,
                               heatmap_fixed_protein_correlation,
                               heatmap_fixed_sample_correlation,
                               heatmap_fixed_basemean,
                               heatmap_fixed_abundance_ratio,
                               debug_log)
  # In Heatmap_utils.R:
  #  - Determines which Heatmap or HeatmapList to export to the Plot Grid based on:
  #       input$heatmap_unified_tabs ('grid_expr_corr', 'grid_expr_sample',
  #                                   'expression_tab', 'protein_cor_tab',
  #                                   'sample_cor_tab', 'basemean_tab',
  #                                   'abundance_ratio_tab').
  #  - Uses the corresponding fixed_* reactiveVals:
  #       - Abundance + Protein (and possibly Basemean/Ratio) -> combined HeatmapList.
  #       - Abundance + Sample -> combined vertical HeatmapList.
  #       - Individual tabs -> a single Heatmap.
  #  - Returns list(status = 'success'/'error', object = <Heatmap/HeatmapList or NULL>,
  #                 title = <string>, message = <string>).

output$downloadPlotButton_Heatmaps (downloadHandler)
  # In Heatmap_module.R:
  #  - Creates a static file for the current tab's Heatmap:
  #       * Reads input$downloadFormat_Heatmaps (pdf, png, jpeg, tiff, svg).
  #       * Reads input$plotWidthInch_Heatmaps, plotHeightInch_Heatmaps, resolution_DPI_Heatmaps.
  #       * Based on input$heatmap_unified_tabs:
  #            - For grid tabs: rebuilds a HeatmapList from fixed_* objects and draws it.
  #            - For single tabs: draws the corresponding fixed_* Heatmap.
  #       * Uses pdf(), svg(), png(), jpeg(), or tiff() devices with appropriate units/resolution.
  #  - On failure: writes a short text file describing the error instead of a plot.

observeEvent(input$add_to_grid, { ... })
  # In Heatmap_module.R:
  #  - Triggered when the user clicks 'Add to Plot Grid'.
  #  - Steps:
  #       1) Calls get_current_heatmap_for_grid(...) to obtain a Heatmap/HeatmapList
  #          and a descriptive title for the current tab.
  #       2) Uses an internal 'adaptive_draw' helper to:
  #            - draw the Heatmap to a temporary SVG or PNG file,
  #            - read the resulting image via png::readPNG(),
  #            - wrap it in a ggplot2::ggplot() with annotation_raster().
  #       3) Derives a sanitized plot id from input$grid_label and ns('').
  #       4) Calls modEnv$add_to_grid(rv, id = plot_id, plot = ggplot_obj,
  #                                   label = <visible label>, source = 'Heatmap')
  #          inside tryCatch().
  #       5) Shows a notification on success or error."
    )
  )
}

render_heatmap_tech_dataproc_content_heatmap <- function() {
  div(
    h2("Data processing"),
    hr(),

    h3("Data Processing Pipeline (Heatmap Module)"),

    h4("Step 1: Column Identification & Sample Resolution"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# 1) Resolve identifier and measurement columns from metadata (rv$data_def)

# Identifier choices (Options tagged as 'Identifier' in data_def)
identifier_indices <- which(grepl('Identifier', rv$data_def$Content, ignore.case = TRUE))
identifier_choices <- rv$data_def$Options[identifier_indices]
# → used to populate input$GeneIdentifierColumn_Heatmap

# Abundance (data type) choices
possible_values <- c(
  'Raw Abundance',
  'Normalized Abundance',
  'Batch Corrected Abundance',
  'Imputed Raw Abundance',
  'Imputed Normalized Abundance'
)
available_choices <- possible_values[possible_values %in% rv$data_def$Content]
# → used to populate input$custom_col_sel_heatmap
#   (fallback to a subset if nothing matches; default prefers 'Normalized Abundance')

# Sample choices for the selected abundance type
data_type <- input$custom_col_sel_heatmap
sample_rows <- which(rv$data_def$Content == data_type & !is.na(rv$data_def$Sample))
sample_choices <- unique(rv$data_def$Sample[sample_rows])
# → used to populate input$select_samples_heatmap

# Abundance columns used for expression matrix:
# - must match the chosen data_type
# - must have a non-NA Sample name
# - must correspond to selected samples
abundance_cols <- which(
  rv$data_def$Content == data_type &
  !is.na(rv$data_def$Sample) &
  rv$data_def$Sample %in% input$select_samples_heatmap
)
# These column indices in rv$data_mod are used for expression values.

# P-value columns (for optional filter)
pvalue_content_types <- c(
  'P-Value','p-Value','p.Value','pvalue','p_value',
  'Adj. P-Value','Adj.P.Value','adj.p.value','adj_p_value',
  'FDR','adj.P.Val','p.adj',
  'Abundance Ratio p-Value','Abundance Ratio Adj. p-Value'
)
pvalue_cols <- which(rv$data_def$Content %in% pvalue_content_types)
# The observer checks for valid numeric values in rv$data_mod before enabling
# input$enable_pvalue_filter_heatmap and populating input$pval_type_heatmap /
# input$pval_col_heatmap.

# Abundance Ratio columns (for optional filter and ratio heatmap)
ratio_cols <- which(rv$data_def$Content == 'Abundance Ratio')
# The observer validates that these columns in rv$data_mod contain finite, >0 values
# before enabling:
#   - input$enable_ratio_filter_heatmap
#   - input$show_abundance_ratio_heatmap
# and populating input$abundance_ratio_col_heatmap.

# 2) Store identifier selection in server for later use

selected_identifier <- input$GeneIdentifierColumn_Heatmap
# Later used to:
#   - derive rownames of the expression matrix
#   - match user-entered identifiers for Add/Remove
#   - interpret custom identifier filters and pathway imports."
    ),

h4("Step 2: Statistical Filtering & Row Selection"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
  "# The main filtering and selection logic is encapsulated in a helper called from
# the 'Create Plot' observer. The resulting data.frame must contain a 'Row Index'
# column that refers back to rv$data_mod rows.

results <- heatmap_perform_statistical_analysis(rv = rv, input = input)

# 1) Start from the full set of rows in rv$data_mod.

# 2) Missing value filter (if enabled)
#    - Uses selected samples and abundance_cols identified in Step 1.
#    - For each row:
#         * Checks rv$data_mod[row, abundance_cols].
#         * If any selected sample has NA (or non-finite) abundance and
#           input$remove_missing_values_heatmap is TRUE:
#              → row is removed.
#    - If all rows are removed at this stage:
#         * returns NULL, and the server shows a notification such as
#           'Matrix contains no valid expression data'.

# 3) P-value filter (optional)
#    - Only active when:
#         * input$enable_pvalue_filter_heatmap is TRUE, and
#         * a valid p-value type (input$pval_type_heatmap) and column
#           (input$pval_col_heatmap) have been selected.
#    - Validation:
#         * Checks that in rv$data_mod[, selected_pval_col] there exist
#           finite numeric values between 0 and 1.
#         * If not, the filter is skipped and a debug message is logged.
#    - If valid:
#         * Keeps only rows with p-value ≤ input$fdr_Heatmap.
#    - If this step removes all rows, the function returns NULL and the
#      Create Plot observer aborts with an error notification.

# 4) Abundance Ratio filter (optional)
#    - Only active when:
#         * input$enable_ratio_filter_heatmap is TRUE, and
#         * a valid ratio column was detected in metadata and rv$data_mod.
#    - Validation:
#         * Confirms that the chosen ratio column has finite, positive numeric values.
#         * If not, the filter and ratio heatmap options are disabled.
#    - If valid:
#         * For each row, computes |log2(ratio)| (on the fly or from pre-logged data).
#         * Depending on input$ratio_filter_mode_heatmap:
#              - 'large changes' → keep rows with |log2(ratio)| > threshold.
#              - 'small changes' → keep rows with |log2(ratio)| < threshold.
#    - If this step removes all rows, the function returns NULL and the
#      Create Plot observer aborts.

# 5) Identifier Filter (optional)
#    - Parses input$identifier_filter_heatmap as a set of identifiers
#      (comma- or line-separated).
#    - Matches these identifiers against the selected identifier column
#      in rv$data_mod.
#    - Keeps only rows whose identifier is in this list.
#    - If this step removes all rows, the function returns NULL.

# 6) Max proteins to display
#    - After all above filters, count remaining rows (candidates).
#    - If candidates <= input$max_proteins_heatmap:
#         * keep all.
#    - If more candidates than allowed:
#         * If p-value filtering was active and a valid p-value column
#           was used:
#              - Sort rows by p-value (ascending) and keep the best
#                'max_proteins_heatmap' rows.
#         * Else:
#              - Draw a random subset of size input$max_proteins_heatmap
#                from the remaining rows.
#    - The final set of row indices is returned in a column named 'Row Index'.

# 7) Return structure
#    - A data.frame where at minimum:
#         * 'Row Index'   : integer indices into rv$data_mod
#      may be accompanied by p-values, ratios or auxiliary columns used
#      for messaging or export.

if (is.null(results) || !'Row Index' %in% names(results)) {
  # server: showNotification('Statistical analysis failed', type = 'error')
  return()
}

# The Create Plot observer stores:
heatmap_selected_row_indices(results$`Row Index`)"
),

h4("Step 3: Building the Expression Matrix and Z-scores"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
  "# Once 'results' is available, the module constructs the expression matrix for
# the selected proteins and samples.

dm <- rv$data_mod
dd <- rv$data_def

data_type        <- input$custom_col_sel_heatmap            # e.g. 'Normalized Abundance'
selected_samples <- input$select_samples_heatmap            # sample names

# 1) Abundance columns for selected samples and data type
abundance_cols <- which(
  dd$Content == data_type &
  !is.na(dd$Sample) &
  dd$Sample %in% selected_samples
)

if (length(abundance_cols) == 0) {
  # server: showNotification('No abundance columns found for selected samples', type = 'error')
  return(NULL)
}

# 2) Extract rows based on results$`Row Index`
selected_row_indices <- results$`Row Index`

heatmap_matrix <- as.matrix(dm[selected_row_indices, abundance_cols, drop = FALSE])

# 3) Set rownames (protein identifiers)
identifier_col <- input$GeneIdentifierColumn_Heatmap
if (!is.null(identifier_col) && nzchar(identifier_col)) {
  identifier_idx <- which(grepl(identifier_col, dd$Options))[1]
  if (!is.na(identifier_idx) && identifier_idx <= ncol(dm)) {
    valid_identifiers <- trimws(as.character(dm[[identifier_idx]][selected_row_indices]))
    rownames(heatmap_matrix) <- valid_identifiers
  } else {
    rownames(heatmap_matrix) <- paste0('Protein_', selected_row_indices)
  }
} else {
  rownames(heatmap_matrix) <- paste0('Protein_', selected_row_indices)
}

# 4) Set colnames (sample names)
sample_names <- dd$Sample[abundance_cols]
colnames(heatmap_matrix) <- sample_names

# 5) Handle invalid numeric values
heatmap_matrix[is.na(heatmap_matrix) | is.infinite(heatmap_matrix)] <- 0

if (all(heatmap_matrix == 0) || nrow(heatmap_matrix) == 0 || ncol(heatmap_matrix) == 0) {
  # server: showNotification('Matrix contains no valid expression data', type = 'error')
  return(NULL)
}

# 6) Log2 transformation
log2_matrix <- log2(heatmap_matrix + 1)  # pseudocount 1 to avoid log2(0)

if (any(!is.finite(log2_matrix))) {
  log2_matrix[!is.finite(log2_matrix)] <- 0
}

# 7) Z-score calculation by row
scaled_matrix <- t(scale(t(log2_matrix)))
scaled_matrix[is.na(scaled_matrix)] <- 0

# 8) Store for later use
heatmap_expression_matrix(scaled_matrix)

# 9) Compute clustering information (row/column order, dendrograms)
cl_info <- compute_expression_clustering(scaled_matrix, input)
heatmap_cluster_info(cl_info)

# 10) Create the ComplexHeatmap object for expression
expr_ht <- create_expression_heatmap_object(
  expr_matrix  = scaled_matrix,
  groups       = NULL,
  input        = input,
  cluster_info = cl_info
)

# 11) Optional: add protein annotation for highlighted proteins
custom_proteins <- heatmap_selected_proteins()
if (!is.null(custom_proteins) && length(custom_proteins) > 0) {
  protein_annotation <- create_protein_annotation(rownames(scaled_matrix), custom_proteins)
  if (!is.null(protein_annotation)) {
    expr_ht <- protein_annotation + expr_ht
  }
}

# expr_ht is then stored in heatmap_plots() for reuse across grid and single-view tabs."
),

h4("Step 4: Correlation Matrices and Derived Heatmaps"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
  "# 1) Protein Correlation Matrix

expr_matrix <- heatmap_expression_matrix()

# Compute correlation between protein profiles (rows)
cor_matrix_protein <- suppressWarnings(
  stats::cor(t(expr_matrix), use = 'pairwise.complete.obs', method = 'pearson')
)
cor_matrix_protein[is.na(cor_matrix_protein)] <- 0
rownames(cor_matrix_protein) <- rownames(expr_matrix)
colnames(cor_matrix_protein) <- rownames(expr_matrix)

# Align order with expression rows
ordered_names <- rownames(expr_matrix)   # may be refined by drawing expr_ht
# (when an abundance heatmap is provided, the module draws it and reads
#  ComplexHeatmap::row_order() to get a more precise row ordering.)

cor_ord <- cor_matrix_protein[ordered_names, ordered_names, drop = FALSE]

# Build ComplexHeatmap object with appropriate colour mapping
pal       <- extract_color_scheme(input)
enhanced  <- isTRUE(input$correlation_enhanced_contrast)
col_fun   <- col_fun_for_correlation(pal, enhanced_contrast = enhanced)
fs        <- extract_font_settings(input)
legend_dir <- legend_direction_from_input(input)

protein_ht <- ComplexHeatmap::Heatmap(
  matrix = cor_ord,
  name   = 'Protein Correlation',
  col    = col_fun,
  show_row_names    = isTRUE(input$show_corr_row_labels),
  show_column_names = isTRUE(input$show_corr_col_labels),
  row_names_side    = 'right',
  column_names_rot  = 90,
  row_names_gp      = safe_gp(fs$row_font_size),
  column_names_gp   = safe_gp(fs$col_font_size),
  row_order         = seq_len(nrow(cor_ord)),
  column_order      = seq_len(ncol(cor_ord)),
  cell_fun          = create_diagonal_line_cell_fun(input),
  heatmap_legend_param = list(
    title            = 'Pairwise Correlation\\nPearson r',
    legend_direction = legend_dir,
    title_position   = if (legend_dir == 'horizontal') 'topcenter' else 'topleft',
    title_gp         = grid::gpar(fontsize = fs$legend_title_font_size),
    labels_gp        = grid::gpar(fontsize = fs$legend_text_font_size)
  )
)

heatmap_protein_cor_matrix(cor_ord)

# 2) Sample Correlation Matrix

cl_info <- heatmap_cluster_info()
if (is.null(cl_info) || is.null(cl_info$col_order_idx)) {
  cl_info <- list(col_order_idx = seq_len(ncol(expr_matrix)))
}

cor_matrix_sample <- suppressWarnings(
  stats::cor(expr_matrix, use = 'pairwise.complete.obs', method = 'pearson')
)
cor_matrix_sample[is.na(cor_matrix_sample)] <- 0
rownames(cor_matrix_sample) <- colnames(expr_matrix)
colnames(cor_matrix_sample) <- colnames(expr_matrix)

ordered_sample_names <- colnames(expr_matrix)[cl_info$col_order_idx]
ordered_sample_names <- intersect(ordered_sample_names, rownames(cor_matrix_sample))

cor_ord_sample <- cor_matrix_sample[ordered_sample_names, ordered_sample_names, drop = FALSE]

pal       <- extract_color_scheme(input)
col_fun_s <- col_fun_for_correlation(pal, enhanced_contrast = isTRUE(input$correlation_enhanced_contrast))
fs        <- extract_font_settings(input)
legend_dir <- legend_direction_from_input(input)

sample_ht <- ComplexHeatmap::Heatmap(
  matrix             = cor_ord_sample,
  name               = 'Sample Correlation',
  col                = col_fun_s,
  show_row_names     = TRUE,
  show_column_names  = TRUE,
  row_names_gp       = safe_gp(fs$row_font_size),
  column_names_gp    = safe_gp(fs$col_font_size),
  column_names_rot   = 90,
  cluster_rows       = FALSE,
  cluster_columns    = FALSE,
  show_row_dend      = FALSE,
  show_column_dend   = FALSE,
  row_order          = seq_len(nrow(cor_ord_sample)),
  column_order       = seq_len(ncol(cor_ord_sample)),
  cell_fun           = create_diagonal_line_cell_fun(input),
  heatmap_legend_param = list(
    title            = 'Pairwise Correlation\\nPearson r',
    legend_direction = legend_dir,
    title_position   = if (legend_dir == 'horizontal') 'topcenter' else 'topleft',
    title_gp         = grid::gpar(fontsize = fs$legend_title_font_size),
    labels_gp        = grid::gpar(fontsize = fs$legend_text_font_size)
  )
)

heatmap_sample_cor_matrix(cor_ord_sample)

# 3) Basemean and Abundance Ratio values for side heatmaps

selected_rows <- heatmap_selected_row_indices()

basemean_values <- calculate_correct_basemean_internal(
  selected_rows,
  loadedData = heatmap_data_modified(),
  data_def   = heatmap_df_data_definition(),
  input      = input
)
if (!is.null(basemean_values)) {
  names(basemean_values) <- rownames(heatmap_expression_matrix())
  basemean_values <- basemean_values[rownames(expr_matrix)]
  heatmap_basemean_values(basemean_values)
}

ratio_values <- calculate_abundance_ratios_internal(
  selected_rows,
  loadedData = heatmap_data_modified(),
  data_def   = heatmap_df_data_definition(),
  input      = input
)
if (!is.null(ratio_values)) {
  names(ratio_values) <- rownames(heatmap_expression_matrix())
  ratio_values <- ratio_values[rownames(expr_matrix)]
  heatmap_abundance_ratio_values(ratio_values)
}"
),

h4("Step 3b: Sample (column) ordering and sort_samples_by"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
  "# After constructing the expression matrix and before log2/z-score transformation,\n",
  "# the module applies an optional reordering of sample columns based on input$sort_samples_by.\n",
  "\n",
  "# 1) Source of truth: selected samples and metadata-based columns\n",
  "selected_samples <- input$select_samples_heatmap %||% character(0)\n",
  "abundance_cols   <- integer(0)\n",
  "for (s in selected_samples) {\n",
  "  idx_s <- which(dd$Content == data_type & !is.na(dd$Sample) & dd$Sample == s)\n",
  "  if (length(idx_s) > 0) {\n",
  "    abundance_cols <- c(abundance_cols, idx_s)\n",
  "  }\n",
  "}\n",
  "abundance_cols <- unique(abundance_cols)\n",
  "\n",
  "# This preserves the order in which samples appear in input$select_samples_heatmap.\n",
  "\n",
  "# 2) Build initial expression matrix (proteins x selected samples)\n",
  "heatmap_matrix <- as.matrix(dm[selected_row_indices, abundance_cols, drop = FALSE])\n",
  "sample_names   <- dd$Sample[abundance_cols]\n",
  "colnames(heatmap_matrix) <- sample_names\n",
  "\n",
  "# 3) Apply sample sorting modes\n",
  "sample_sort_mode <- input$sort_samples_by %||% \"none\"\n",
  "if (!is.null(sample_names) && length(sample_names) > 1) {\n",
  "  sort_idx <- seq_along(sample_names)  # default: keep UI order\n",
  "\n",
  "  if (sample_sort_mode == \"alpha_asc\") {\n",
  "    sort_idx <- order(sample_names)\n",
  "\n",
  "  } else if (sample_sort_mode == \"alpha_desc\") {\n",
  "    sort_idx <- order(sample_names, decreasing = TRUE)\n",
  "\n",
  "  } else if (sample_sort_mode == \"pearson_cluster\") {\n",
  "    # Pearson r clustering: 1 - correlation distance, Ward.D2\n",
  "    suppressWarnings({\n",
  "      cor_mat <- tryCatch(\n",
  "        stats::cor(heatmap_matrix, use = \"pairwise.complete.obs\", method = \"pearson\"),\n",
  "        error = function(e) NULL\n",
  "      )\n",
  "      if (!is.null(cor_mat)) {\n",
  "        cor_mat[is.na(cor_mat)] <- 0\n",
  "        d <- tryCatch(stats::as.dist(1 - cor_mat), error = function(e) NULL)\n",
  "        if (!is.null(d)) {\n",
  "          hc <- tryCatch(stats::hclust(d, method = \"ward.D2\"), error = function(e) NULL)\n",
  "          if (!is.null(hc) && length(hc$order) == length(sample_names)) {\n",
  "            sort_idx <- hc$order\n",
  "          }\n",
  "        }\n",
  "      }\n",
  "    })\n",
  "\n",
  "  } else if (sample_sort_mode == \"distance_cluster\") {\n",
  "    # Euclidean distance clustering in log2 expression space\n",
  "    log2_tmp <- tryCatch({\n",
  "      m <- log2(heatmap_matrix + 1)\n",
  "      m[!is.finite(m)] <- 0\n",
  "      m\n",
  "    }, error = function(e) NULL)\n",
  "    if (!is.null(log2_tmp)) {\n",
  "      suppressWarnings({\n",
  "        d <- tryCatch(stats::dist(t(log2_tmp)), error = function(e) NULL)\n",
  "        if (!is.null(d)) {\n",
  "          hc <- tryCatch(stats::hclust(d, method = \"ward.D2\"), error = function(e) NULL)\n",
  "          if (!is.null(hc) && length(hc$order) == length(sample_names)) {\n",
  "            sort_idx <- hc$order\n",
  "          }\n",
  "        }\n",
  "      })\n",
  "    }\n",
  "\n",
  "  } else if (sample_sort_mode %in% c(\"pca1_asc\", \"pca1_desc\")) {\n",
  "    # PCA-based ordering on PC1\n",
  "    log2_tmp <- tryCatch({\n",
  "      m <- log2(heatmap_matrix + 1)\n",
  "      m[!is.finite(m)] <- 0\n",
  "      m\n",
  "    }, error = function(e) NULL)\n",
  "    if (!is.null(log2_tmp)) {\n",
  "      suppressWarnings({\n",
  "        pca <- tryCatch(stats::prcomp(t(log2_tmp), center = TRUE, scale. = TRUE), error = function(e) NULL)\n",
  "        if (!is.null(pca) && ncol(pca$x) >= 1) {\n",
  "          pc1 <- pca$x[, 1]\n",
  "          if (!is.null(names(pc1))) {\n",
  "            pc1 <- pc1[sample_names]\n",
  "          }\n",
  "          if (sample_sort_mode == \"pca1_asc\") {\n",
  "            sort_idx <- order(pc1, na.last = NA)\n",
  "          } else {\n",
  "            sort_idx <- order(pc1, decreasing = TRUE, na.last = NA)\n",
  "          }\n",
  "          if (length(sort_idx) != length(sample_names)) {\n",
  "            sort_idx <- seq_along(sample_names)\n",
  "          }\n",
  "        }\n",
  "      })\n",
  "    }\n",
  "  }\n",
  "\n",
  "  heatmap_matrix <- heatmap_matrix[, sort_idx, drop = FALSE]\n",
  "  sample_names   <- sample_names[sort_idx]\n",
  "  colnames(heatmap_matrix) <- sample_names\n",
  "}\n",
  "\n",
  "# 4) Store final sample order for reuse in other views\n",
  "heatmap_shared_col_order(sample_names)\n"
),

h3("Error Handling & Edge Cases"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px;",
  "# Throughout the Heatmap module, critical steps are guarded with validate(), req() and tryCatch().

# 1) Data availability
req(rv$data_mod, rv$data_def)
# If either is NULL, observers and reactive expressions abort and no plots are drawn.

# 2) Sample and abundance selection
if (length(input$select_samples_heatmap) == 0) {
  showNotification('No samples selected for heatmap', type = 'error', duration = 5)
  return(NULL)
}
if (length(abundance_cols) == 0) {
  showNotification('No abundance columns found for selected samples', type = 'error', duration = 5)
  return(NULL)
}

# 3) Invalid matrices
if (all(heatmap_matrix == 0) || nrow(heatmap_matrix) == 0 || ncol(heatmap_matrix) == 0) {
  showNotification('Matrix contains no valid expression data', type = 'error', duration = 5)
  return(NULL)
}

# 4) Statistical filter pipeline
results <- tryCatch(
  heatmap_perform_statistical_analysis(rv = rv, input = input),
  error = function(e) {
    debug_log(paste('Error in heatmap_perform_statistical_analysis:', e$message), 1)
    NULL
  }
)
if (is.null(results) || !'Row Index' %in% names(results)) {
  showNotification('Statistical analysis failed', type = 'error', duration = 5)
  return()
}

# 5) Correlation computation
cor_mat <- suppressWarnings(tryCatch({
  stats::cor(t(expr_matrix), use = 'pairwise.complete.obs', method = 'pearson')
}, error = function(e) {
  debug_log(paste('Correlation failed, falling back to zeros:', e$message), 1)
  matrix(0, nrow = nrow(expr_matrix), ncol = nrow(expr_matrix))
}))
cor_mat[is.na(cor_mat)] <- 0

# 6) Alignment / drawing
tryCatch({
  ComplexHeatmap::draw(ht_list,
    newpage = FALSE,
    merge_legends = TRUE,
    heatmap_legend_side = legend_side,
    annotation_legend_side = legend_side,
    auto_adjust = FALSE,
    padding = pad_vec
  )
}, error = function(e) {
  debug_log(paste('Final draw failed:', e$message), 1)
  plot.new()
  text(0.5, 0.5, paste('Error:', e$message), col = 'red', cex = 1)
})

# 7) Download and Plot Grid integration
output$downloadPlotButton_Heatmaps and the add_to_grid observer wrap all device
and file operations in tryCatch(), writing a human-readable error message to the
output file when something goes wrong (instead of silently failing)."
)
  )
}

render_heatmap_tech_integration_content_heatmap <- function() {
  div(
    h2("Integration Details"),
    hr(),

    h3("Integration Requirements"),

    h4("Required reactive values (from the main app)"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# The Heatmap module reads core data from rv (same pattern as other analysis modules):
rv <- reactiveValues(
  data_mod        = NULL,  # processed quantitative data; one row per protein/feature
  data_def        = NULL,  # column metadata (Content, Column, Options, Sample, etc.)
  ab_validate     = NULL   # optional: abundance validation information (passed through as-is)
  # other rv entries are ignored by the Heatmap module
)"
    ),

    h4("How the module is wired (UI & server)"),
    pre(
      style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
      "# UI wiring in app.R (Main Analysis → tabsetPanel)

tabPanel(
  title = \"Heatmap\",
  value = \"heatmap\",
  tryCatch({
    # Ensure Shiny functions are available in modEnv
    if (!exists(\"NS\", envir = modEnv)) {
      modEnv$NS <- shiny::NS
    }
    if (!exists(\"moduleServer\", envir = modEnv)) {
      modEnv$moduleServer <- shiny::moduleServer
    }

    # Heatmap UI function defined in modules/Heatmap_module.R
    if (exists(\"modHeatmapUI\", envir = modEnv)) {
      modEnv$modHeatmapUI(\"heatmap\")
    } else {
      div(
        style = \"text-align: center; padding: 50px; color: #999;\",
        h4(\"Heatmap Module Not Available\"),
        p(\"The enhanced Heatmap module could not be loaded.\"),
        p(\"Please check that all required files are present and Shiny is properly loaded.\"),
        actionButton(\"reload_heatmap\", \"Try Reload\",
                     onclick = \"location.reload();\",
                     class = \"btn-primary\")
      )
    }
  }, error = function(e) {
    div(
      style = \"text-align: center; padding: 50px; color: #d9534f;\",
      h4(\"Heatmap Module Error\"),
      p(\"Error loading Heatmap module:\"),
      verbatimTextOutput(\"heatmap_error_details\"),
      br(),
      actionButton(\"reload_app\", \"Reload Application\",
                   onclick = \"location.reload();\",
                   class = \"btn-warning\")
    )
  })
)

# Server wiring in app.R

# 1) All module files (including Heatmap) are loaded into modEnv before server() starts:
for (f in list.files(\"modules\", pattern = \"\\\\.R$\", full.names = TRUE)) {
  sys.source(f, envir = modEnv)
}

# 2) Heatmap module server is initialized and its return value stored in module_outputs$heatmap_out:
module_outputs$heatmap_out <- tryCatch({
  debug_log(\"Initializing Enhanced Heatmap module with GSEA and GO integration\", 2)

  modEnv$modHeatmapServer(
    id = \"heatmap\",
    rv = rv,
    res_GSEA = if (!is.null(module_outputs$gsea_out)) {
      reactive({
        if (\"get_results\" %in% names(module_outputs$gsea_out)) {
          module_outputs$gsea_out$get_results()
        } else {
          NULL
        }
      })
    } else {
      NULL
    },
    GO_res = if (!is.null(module_outputs$go_out)) {
      reactive({
        if (\"get_results\" %in% names(module_outputs$go_out)) {
          module_outputs$go_out$get_results()
        } else {
          NULL
        }
      })
    } else {
      NULL
    },
    module_outputs = module_outputs,
    debug_level    = DEBUG_LEVEL,
    modEnv         = modEnv
  )
}, error = function(e) {
  debug_log(paste(\"Error initializing Enhanced Heatmap module:\", e$message), 1)
  showNotification(
    paste(\"Enhanced Heatmap module failed to load:\", e$message),
    type     = \"warning\",
    duration = 8
  )
  NULL
})"
    ),

  h3("What the module expects"),

  h4("rv$data_mod (processed data)"),
  tags$ul(
    tags$li("A data.frame/tibble with one row per protein or feature."),
    tags$li("All column names referenced in ", code("rv$data_def$Column"), " must exist in ", code("rv$data_mod"), "."),
    tags$li("At minimum, columns for:"),
    tags$ul(
      tags$li("one or more abundance-type columns with ", code("Content"), " set to entries such as ", code("'Raw Abundance'"), ", ", code("'Normalized Abundance'"), ", ", code("'Batch Corrected Abundance'"), " or imputed variants, used to build the expression matrix;"),
      tags$li("optionally one or more ", code("'Abundance Ratio'"), " columns, used for ratio filtering and the Abundance Ratio side heatmap;"),
      tags$li("optionally one or more p‑value or FDR columns, with ", code("Content"), " values matching common p‑value types (e.g. ", code("'P-Value'"), ", ", code("'Adj. P-Value'"), ", ", code("'FDR'"), "), used in the p‑value filter;"),
      tags$li("at least one identifier column tagged as ", code("Content == 'Identifier'"), " with a non‑empty ", code("Options"), " value (e.g. a gene symbol type), used by ", code("GeneIdentifierColumn_Heatmap"), " for row labels and identifier matching.")
    ),
    tags$li("Missing values are handled explicitly during filtering and expression matrix construction; rows with missing values can be removed depending on the UI setting.")
  ),

  h4("rv$data_def (metadata) – relevant schema for Heatmap"),
  pre(
    style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
    "data_def <- data.frame(
  Column      = c('Norm. Abundance: SampleA',   'Norm. Abundance: SampleB',
                  'Abundance Ratio: A/B',       'Gene Symbol', 'P-Value: A/B'),
  Content     = c('Normalized Abundance',       'Normalized Abundance',
                  'Abundance Ratio',            'Identifier',  'P-Value'),
  Sample      = c('SampleA',                    'SampleB',
                  NA,                           NA,           NA),
  Options     = c(NA,                           NA,
                  NA,                           'SYMBOL',     NA),
  Numerator   = c(NA,                           NA,
                  'A',                          NA,           'A'),
  Denominator = c(NA,                           NA,
                  'B',                          NA,           'B'),
  stringsAsFactors = FALSE
)

# The Heatmap module relies on:
#   - Content:
#        'Identifier'          → identifier choices for GeneIdentifierColumn_Heatmap
#        'Raw Abundance'       → raw data
#        'Normalized Abundance'→ normalized data
#        'Batch Corrected Abundance'
#        'Imputed Raw Abundance'
#        'Imputed Normalized Abundance'
#        'Abundance Ratio'     → ratio filter and ratio side heatmap
#        p-value/FDR-like tags → p-value filter
#   - Sample:
#        non-NA Sample names are used to populate select_samples_heatmap and to
#        align abundance columns with selected samples.
#   - Options:
#        identifier types (e.g. 'SYMBOL') used to populate GeneIdentifierColumn_Heatmap.
#   - Numerator/Denominator:
#        used for matching ratio and p-value columns in a few validation steps."
  ),

h3("Inputs, Outputs, and Shared State"),

h4("Key Heatmap UI inputs"),
tags$ul(
  tags$li(code("custom_col_sel_heatmap"), ": abundance data type, derived from ", code("data_def$Content"), " (e.g. ", code("'Normalized Abundance'"), ", ", code("'Raw Abundance'"), ")."),
  tags$li(code("GeneIdentifierColumn_Heatmap"), ": identifier type used for row labels and identifier-based filters; choices derived from ", code("data_def$Options[data_def$Content == 'Identifier']"), "."),
  tags$li(code("select_samples_heatmap"), ": sample selection; choices derived from ", code("data_def$Sample"), " entries that match the selected abundance type."),
  tags$li(code("enable_pvalue_filter_heatmap"), ": toggles p‑value filtering; only enabled when suitable p‑value columns exist and contain numeric values in [0, 1]."),
  tags$li(code("pval_type_heatmap / pval_col_heatmap"), ": select which p‑value type and which column to use in the p‑value filter; both are inferred from ", code("data_def$Content"), " and ", code("data_def$Column"), "."),
  tags$li(code("fdr_Heatmap"), ": numeric threshold for p‑value/FDR filtering."),
  tags$li(code("enable_ratio_filter_heatmap"), ": toggles abundance ratio filtering; only enabled when suitable ", code("Content == 'Abundance Ratio'"), " columns exist and contain positive numeric values."),
  tags$li(code("abundance_ratio_col_heatmap"), ": specific abundance ratio column to use in ratio filtering and the ratio side heatmap."),
  tags$li(code("ratio_filter_mode_heatmap / ratio_threshold_heatmap"), ": define whether to keep large or small absolute log2 ratios and at which cut‑off."),
  tags$li(code("remove_missing_values_heatmap"), ": if TRUE, removes any protein with missing abundance values in any selected sample before other filters."),
  tags$li(code("base_mean_Heatmap"), ": numeric threshold that can be used in the statistical helper to filter on basemean (when implemented in the pipeline)."),
  tags$li(code("max_proteins_heatmap"), ": upper limit on the number of proteins kept after filtering; used to down‑select by p‑value or random sampling."),
  tags$li(code("identifier_filter_heatmap"), ": text area used to restrict the final set of proteins to a custom identifier list."),
  tags$li(code("input_Heatmap"), ": text area in the Protein Selection Panel used to search or paste gene symbols or identifiers."),
  tags$li(code("GSEA_Heatmap / GO_Heatmap"), ": pathway selectors populated from ", code("res_GSEA()"), " and ", code("GO_res()"), " to import pathway genes into the Protein Selection Panel."),
  tags$li(code("Intersect_Heatmap / CoreEnriched_Heatmap"), ": options that control how pathway imports are combined (intersection vs union; use of core enriched genes for GSEA)."),
  tags$li(code("highlightProteins_Heatmap / undoHighlight_Heatmap"), ": buttons used to mark selected proteins in the abundance heatmap using a ComplexHeatmap annotation."),
  tags$li(code("custom_proteins_input"), ": free‑text list of proteins used for highlighting and for building identifier lists."),
  tags$li(code("sort_proteins_by"), ": protein sorting mode; values ", code("'z_score'"), ", ", code("'pearson_r'"), " or ", code("'custom'"), " determine the canonical protein order."),
  tags$li(code("custom_protein_order"), ": comma- or line-separated priority list used only when ", code("sort_proteins_by == 'custom'"), "; missing identifiers are ignored and duplicate entries keep the first occurrence."),
  tags$li(code("custom_protein_fallback_sort"), ": fallback for proteins not listed in ", code("custom_protein_order"), "; values ", code("'z_score'"), " or ", code("'pearson_r'"), "."),
  tags$li(code("show_row_dendrogram / show_column_dendrogram"), ": toggles dendrogram visibility in the Abundance Heatmap."),
  tags$li(code("low_color_heatmap / mid_color_heatmap / high_color_heatmap"), ": three base colours used to construct all heatmap gradients."),
  tags$li(code("correlation_enhanced_contrast"), ": enables the sigmoid‑based contrast enhancement for correlation colour mapping."),
  tags$li(code("legend_side_heatmap"), ": legend position for ComplexHeatmap plots (", code("'right'"), ", ", code("'left'"), ", ", code("'top'"), ", ", code("'bottom'"), ")."),
  tags$li(code("show_correlation_diagonal / diagonal_line_color / diagonal_line_width / rotate_diagonal_line"), ": options used by the diagonal‑line cell function for correlation heatmaps."),
  tags$li(code("show_basemean_heatmap / show_abundance_ratio_heatmap"), ": toggles display of the basemean and abundance ratio side heatmaps."),
  tags$li(code("row_font_size_heatmap / col_font_size_heatmap / legend_title_font_size_heatmap / legend_text_font_size_heatmap"), ": font size controls used by ComplexHeatmap theme helpers."),
  tags$li(code("create_heatmap_btn"), ": triggers the main filtering and heatmap construction pipeline."),
  tags$li(code("reset_heatmap_btn"), ": resets Heatmap UI inputs and module‑internal reactive state to defaults."),
  tags$li(code("resolution_DPI_Heatmaps / plotWidthInch_Heatmaps / plotHeightInch_Heatmaps / downloadFormat_Heatmaps"), ": parameters driving the download device settings in the Heatmap download handler."),
  tags$li(code("grid_label"), ": optional label used to name plots sent to the Plot Grid module."),
  tags$li(code("add_to_grid"), ": sends the current Heatmap view to the Plot Grid via ", code("modEnv$add_to_grid()"), ".")
),

h4("Outputs and shared results"),
tags$ul(
  tags$li(code("output$heatmap_grid"), ": main combined view of Abundance + Protein Correlation (and optional Basemean/Ratio) as a ComplexHeatmap grid."),
  tags$li(code("output$heatmap_expr_sample_grid"), ": combined view of Abundance + Sample Correlation with shared sample order."),
  tags$li(code("output$test_expression_only / $test_protein_cor_only / $test_sample_cor_only / $test_basemean_only / $test_abundance_ratio_only"), ": individual views for abundance, protein correlation, sample correlation, basemean and ratio heatmaps."),
  tags$li(code("output$grid_expr_corr_title / $grid_expr_sample_title / ..."), ": dynamic plot titles summarising filter settings and matrix dimensions."),
  tags$li(code("output$current_tab_info"), ": text summarising the active Heatmap tab and current download parameters."),
  tags$li(code("output$debug_info / $raw_debug_log"), ": optional debug outputs used to surface internal state."),
  tags$li(code("output$download_info_Heatmaps"), ": text describing effective pixel dimensions for the current Heatmap download settings."),
  tags$li(code("downloadPlotButton_Heatmaps"), ": generic download handler that writes the currently selected Heatmap view to disk in the chosen graphics format.")
),

h4("Module-internal shared structures (server-side)"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
  "# Key reactives created in modHeatmapServer() and used internally:

# Core data and matrices
heatmap_data                 <- reactiveVal(NULL)  # filtered data.frame (after statistical filters)
heatmap_expression_matrix    <- reactiveVal(NULL)  # z-scored log2 expression matrix
heatmap_protein_cor_matrix   <- reactiveVal(NULL)  # protein-protein correlation matrix
heatmap_sample_cor_matrix    <- reactiveVal(NULL)  # sample-sample correlation matrix
heatmap_pure_expression      <- reactiveVal(NULL)  # original log2 expression matrix before scaling
heatmap_cluster_info         <- reactiveVal(NULL)  # clustering / ordering information for rows and columns
heatmap_selected_row_indices <- reactiveVal(NULL)  # indices into rv$data_mod used for basemean/ratio extraction

# ComplexHeatmap objects (per component)
heatmap_plots <- reactiveVal(list(
  expr     = NULL,  # base expression Heatmap
  prot     = NULL,  # base protein correlation Heatmap
  ratio    = NULL,  # base ratio Heatmap
  basemean = NULL   # base basemean Heatmap
))

# Fixed Heatmaps for reproducible layout across tabs and downloads
heatmap_fixed_expression         <- reactiveVal(NULL)
heatmap_fixed_protein_correlation<- reactiveVal(NULL)
heatmap_fixed_sample_correlation <- reactiveVal(NULL)
heatmap_fixed_basemean           <- reactiveVal(NULL)
heatmap_fixed_abundance_ratio    <- reactiveVal(NULL)

# Protein Selection Panel state
selected_data_Heatmap           <- reactiveVal(NULL)  # data.frame of matched proteins from identifier searches
selected_protein_vector_Heatmap <- reactiveVal(NULL)  # current set of selected identifiers (vector)
heatmap_highlighted_proteins    <- reactiveVal(NULL)  # identifiers to highlight on the expression Heatmap
heatmap_protein_annotation      <- reactiveVal(NULL)  # ComplexHeatmap annotation object for highlighting

# Other helpers (created via heatmap_data_modified(), heatmap_df_data_definition(), abundance_validate(), etc.)
# are defined as reactives inside modHeatmapServer() and are not exported outside the module."
),

h3("How Heatmap integrates with other modules"),

h4("Public API returned by modHeatmapServer()"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
  "# modHeatmapServer(...) returns a named list.
# app.R stores this in module_outputs$heatmap_out.
# The following elements are defined at the end of modules/Heatmap_module.R:

list(
  # Core expression matrix
  get_expression_matrix = reactive({
    heatmap_expression_matrix()         # z-scored log2 matrix; NULL before first create
  }),

  # Correlation matrices
  # get_protein_correlation returns the protein-protein Pearson correlation matrix.
  get_protein_correlation = reactive({
    heatmap_protein_cor_matrix()
  }),

  # get_sample_correlation applies a multi-method resolution strategy:
  #   Method 1: heatmap_sample_cor_matrix() if already computed and valid.
  #   Method 2: extract matrix from heatmap_fixed_sample_correlation() (ComplexHeatmap object).
  #   Method 3: recompute from heatmap_expression_matrix() columns.
  #   Method 4: recompute from heatmap_fixed_expression()@matrix columns.
  #   In all cases the result is axis-synchronized via heatmap_sync_sample_correlation_axes().
  get_sample_correlation = reactive({ ... }),

  # Selected protein identifiers (character vector from the Protein Selection Panel)
  get_selected_proteins = reactive({
    heatmap_selected_proteins()
  }),

  # Trigger a re-run of run_heatmap_creation() from outside the module
  refresh_heatmap = function() {
    run_heatmap_creation(trigger_source = 'external-refresh')
  },

  # Matrix export for basemean: returns the matrix backing heatmap_fixed_basemean(),
  # or a single-column matrix built from heatmap_basemean_values() as fallback.
  get_basemean_heatmap_for_export = reactive({ ... }),

  # Matrix export for abundance ratio: same strategy as basemean.
  get_abundance_ratio_heatmap_for_export = reactive({ ... }),

  # Diagnostic summary of available data; useful for debugging integration issues.
  get_export_summary = reactive({
    list(
      has_expression             = ...,
      has_protein_correlation    = ...,
      has_sample_correlation_regular = ...,
      has_sample_correlation_fixed   = ...,
      has_basemean_values        = ...,
      has_abundance_ratio_values = ...,
      has_fixed_basemean         = ...,
      has_fixed_abundance_ratio  = ...,
      expression_dimensions      = ...,
      basemean_count             = ...,
      ratio_count                = ...
    )
  })
)

# Note: the accessor names get_protein_correlation and get_sample_correlation
# (without '_matrix' suffix) are the names used in the actual return list."
),

h4("GSEA/GO integration"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
  "# The Heatmap module consumes GSEA and GO results provided by app.R:

# In app.R (server):
res_GSEA = if (!is.null(module_outputs$gsea_out)) {
  reactive({
    if (\"get_results\" %in% names(module_outputs$gsea_out)) {
      module_outputs$gsea_out$get_results()
    } else {
      NULL
    }
  })
} else {
  NULL
}

GO_res = if (!is.null(module_outputs$go_out)) {
  reactive({
    if (\"get_results\" %in% names(module_outputs$go_out)) {
      module_outputs$go_out$get_results()
    } else {
      NULL
    }
  })
} else {
  NULL
}

# In modHeatmapServer(...):
#  - res_GSEA and GO_res are stored as reactive expressions.
#  - Observers in Heatmap_module.R inspect these reactives to:
#       * populate input$GSEA_Heatmap with enriched GSEA pathway descriptions:
#             - if res_GSEA() is a list with $Results, uses as.data.frame(Results)$Description
#             - if it is a data.frame, uses its Description column directly (if present)
#       * populate input$GO_Heatmap with GO term descriptions (from GO_enrichment module).
#  - When the user presses 'Add Pathway proteins':
#       * For selected GSEA pathways:
#             - extracts 'core enriched' gene identifiers from the core_enrichment field (slash-separated).
#       * For selected GO terms:
#             - extracts gene identifiers from the GO results table.
#       * Combines them by intersection or union depending on input$Intersect_Heatmap.
#       * Writes the resulting identifiers into input$input_Heatmap (the search textarea)."
),

h4("Plot Grid integration"),
pre(
  style = "background-color:#f5f5f5; padding:10px; border-radius:5px; font-family:monospace;",
  "# The Heatmap module integrates with the shared Plot Grid via modEnv$add_to_grid()
# (implemented in Grid_module.R). This is triggered by input$add_to_grid and uses
# ComplexHeatmap objects converted to ggplot-compatible raster plots.

observeEvent(input$add_to_grid, {
  debug_log(\"Heatmap: add_to_grid clicked\", 2)

  plot_result <- tryCatch({
    get_current_heatmap_for_grid(
      input                         = input,
      heatmap_fixed_expression      = heatmap_fixed_expression,
      heatmap_fixed_protein_correlation = heatmap_fixed_protein_correlation,
      heatmap_fixed_sample_correlation  = heatmap_fixed_sample_correlation,
      heatmap_fixed_basemean        = heatmap_fixed_basemean,
      heatmap_fixed_abundance_ratio = heatmap_fixed_abundance_ratio,
      debug_log                     = debug_log
    )
  }, error = function(e) {
    debug_log(paste(\"Heatmap: error getting current plot:\", e$message), 1)
    list(status = \"error\", message = paste(\"Error accessing plot:\", e$message))
  })

  if (plot_result$status != \"success\") {
    showNotification(plot_result$message, type = \"error\")
    return()
  }

  plot_obj <- plot_result$object
  if (is.null(plot_obj)) {
    showNotification(\"No heatmap available to add. Please create a heatmap first.\", type = \"error\")
    return()
  }

  # Convert ComplexHeatmap object to a ggplot-like plot using an adaptive draw helper
  gg_heatmap <- tryCatch({
    adaptive_draw(
      ht      = plot_obj,
      debug_log = debug_log
    )
  }, error = function(e) {
    debug_log(paste(\"Heatmap: adaptive_draw failed:\", e$message), 1)
    NULL
  })

  if (is.null(gg_heatmap)) {
    showNotification(\"Failed to convert heatmap for Plot Grid.\", type = \"error\")
    return()
  }

  # Build a plot ID and visible label
  sanitize <- function(x) gsub(\"[^[:alnum:]_]+\", \"_\", x)
  lbl_raw <- input$grid_label
  lbl_id  <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) \"default\" else sanitize(lbl_raw)
  plot_id <- paste0(ns(\"\"), \"Heatmap_\", lbl_id)
  lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else \"Heatmap\"

  # Delegate registration to Grid module
  tryCatch({
    debug_log(paste(\"Heatmap: adding to grid id=\", plot_id), 2)
    modEnv$add_to_grid(rv, id = plot_id, plot = gg_heatmap, label = lbl_vis, source = \"Heatmap\")
    showNotification(\"Added Heatmap to Plot Grid.\", type = \"message\")
  }, error = function(e) {
    debug_log(paste(\"Heatmap: error adding to grid:\", e$message), 1)
    showNotification(\"Error adding Heatmap to Plot Grid. Check console for details.\", type = \"error\")
  })
})"
),

h3("Summary"),
tags$ul(
  tags$li(
    "The Heatmap module consumes ", code("rv$data_mod"), " and ", code("rv$data_def"),
    " in the same way as other analysis modules: ", code("data_def$Content"), ", ",
    code("data_def$Sample"), " and ", code("data_def$Options"),
    " drive all UI choices for abundance type, samples and identifier columns."
  ),
  tags$li(
    "Filtering, expression matrix construction, correlation calculation and ComplexHeatmap assembly are contained inside ",
    code("modHeatmapServer()"), " and its helpers in ", code("Heatmap_utils.R"),
    " and are triggered explicitly via ", code("input$create_heatmap_btn"), "."
  ),
  tags$li(
    "The public API returned by ", code("modHeatmapServer()"),
    " exposes the expression matrix (", code("get_expression_matrix"), "), protein and sample correlation matrices (",
    code("get_protein_correlation"), ", ", code("get_sample_correlation"), "), ",
    "the currently selected proteins (", code("get_selected_proteins"), "), ",
    "and matrix-form exports for basemean and abundance ratio data. ",
    "An external refresh trigger (", code("refresh_heatmap"), ") and a diagnostic summary (",
    code("get_export_summary"), ") are also exposed."
  ),
  tags$li(
    "Integration with other modules is read‑only: GSEA and GO results provide pathway membership for the Protein Selection Panel, ",
    "and the Plot Grid receives rasterised ComplexHeatmap views via ", code("modEnv$add_to_grid()"),
    " using a single add‑to‑grid observer."
  )
)
  )
}
