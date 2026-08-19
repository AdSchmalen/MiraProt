# ./Documentation/STRING_doc.R
# STRING Module Documentation
# Provides user guide and technical documentation for the STRING module

############
# UI

modSTRINGDocUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
:root{
  --dw-bg:#f5f7fb;
  --dw-surface:#ffffff;
  --dw-border:#e6e8ef;
  --dw-text:#1f2633;
  --dw-text-soft:#56607a;
  --dw-brand:#3c7bf1;
  --dw-brand-10:rgba(60,123,241,.10);
  --dw-shadow:0 8px 22px rgba(31,38,51,.10);
}
html, body { background: var(--dw-bg); }
html { scroll-behavior: smooth; }
.dw-guide-wrap {
  background: var(--dw-surface);
  border: 1px solid var(--dw-border);
  border-radius: 14px;
  box-shadow: var(--dw-shadow);
  padding: 24px;
}
.dw-guide-wrap .alert{
  border:1px solid var(--dw-border);
  border-radius:12px;
}
"))
    ),
    fluidRow(
      column(
        3,
        wellPanel(
          style = "background-color:#f8f9fa; position: sticky; top: 20px;",
          h4("Navigation", style = "margin-bottom: 20px;"),
          radioButtons(
            ns("_string_doc_type"),
            "Documentation Type:",
            choices = c("User Guide" = "user",
                        "Technical Documentation" = "technical"),
            selected = "user"
          ),
          hr(),
          # User Guide navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'user'", ns("_string_doc_type")),
            h5("User Guide"),
            div(
              class = "list-group",
              actionLink(ns("nav_overview_STRING"),        "Overview",                     class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_selection_STRING"),       "Protein selection and plotting", class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_customize_STRING"),       "Customization",               class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_saveplots_STRING"),       "Save Plots",                  class = "list-group-item list-group-item-action")
            )
          ),
          # Technical Documentation navigation
          conditionalPanel(
            condition = sprintf("input['%s'] == 'technical'", ns("_string_doc_type")),
            h5("Technical Documentation"),
            div(
              class = "list-group",
              actionLink(ns("nav_tech_overview_STRING"),   "Technical Overview",   class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_functions_STRING"),  "Functions Reference",  class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_dataproc_STRING"),   "Data processing",      class = "list-group-item list-group-item-action"),
              actionLink(ns("nav_tech_integration_STRING"),"Integration Details",  class = "list-group-item list-group-item-action")
            )
          )
        )
      ),
      column(
        9,
        div(class = "dw-guide-wrap", uiOutput(ns("_string_doc_content")))
      )
    )
  )
}

############
# Server

modSTRINGDocServer <- function(id, debug_level = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_section_STRING <- reactiveVal("overview")

    # User Guide navigation
    observeEvent(input$nav_overview_STRING,  { current_section_STRING("overview") })
    observeEvent(input$nav_selection_STRING, { current_section_STRING("selection") })
    observeEvent(input$nav_customize_STRING, { current_section_STRING("customize") })
    observeEvent(input$nav_saveplots_STRING, { current_section_STRING("saveplots") })

    # Technical navigation
    observeEvent(input$nav_tech_overview_STRING,    { current_section_STRING("tech_overview") })
    observeEvent(input$nav_tech_functions_STRING,   { current_section_STRING("tech_functions") })
    observeEvent(input$nav_tech_dataproc_STRING,    { current_section_STRING("tech_dataproc") })
    observeEvent(input$nav_tech_integration_STRING, { current_section_STRING("tech_integration") })

    # Switch between user / technical default sections
    observeEvent(input$`_string_doc_type`, {
      if (isTRUE(input$`_string_doc_type` == "user")) {
        current_section_STRING("overview")
      } else {
        current_section_STRING("tech_overview")
      }
    })

    output$`_string_doc_content` <- renderUI({
      switch(
        current_section_STRING(),
        # User Guide sections
        "overview"        = render_STRING_overview_content_STRING(),
        "selection"       = render_STRING_selection_plotting_content_STRING(),
        "customize"       = render_STRING_customizing_content_STRING(),
        "saveplots"       = render_STRING_saveplots_content_STRING(),
        # Technical sections
        "tech_overview"   = render_STRING_tech_overview_content_STRING(),
        "tech_functions"  = render_STRING_tech_functions_content_STRING(),
        "tech_dataproc"   = render_STRING_tech_dataproc_content_STRING(),
        "tech_integration"= render_STRING_tech_integration_content_STRING(),
        # Fallback
        div(
          class = "alert alert-info",
          "Please select a section from the navigation menu."
        )
      )
    })
  })
}

render_STRING_selection_plotting_content_STRING <- function() {
  div(
    h2("Protein selection and plotting"),
    hr(),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      # Step 1 — Load data and choose identifier (Data Wizard + STRING tab)
      div(
        class = "panel panel-primary",
        div(class = "panel-heading", h4("Step 1 — Load data and choose identifier")),
        div(
          class = "panel-body",
          p(
            "Use the ", strong("Data Wizard"), " to load your dataset and define metadata, ",
            "including at least one identifier column and the corresponding content tags. ",
            "The STRING module uses this information to offer the correct identifier choices."
          ),
          tags$ul(
            tags$li(
              strong("Identifier choice:"),
              " In the STRING tab, use the identifier selector to choose which identifier from your data ",
              "should be used to match proteins. ",
              "All text input, suggestions and mappings in this module refer to this identifier type."
            ),
            tags$li(
              strong("Data source:"),
              " The STRING module works on the processed data table from the Data Wizard. ",
              "Only identifiers that exist in this table can be selected and sent to STRING."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Note:</strong> The identifier you select here must match both your own data and the STRING database. ",
            "Only exact matches between the text input and this identifier column are considered."
          )
        )
      ),

      # Step 2 — Provide proteins and manage the selection table
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 2 — Provide proteins and build the selection table")),
        div(
          class = "panel-body",
          p(
            "The STRING network is always built from the proteins listed in the ",
            em("Selected Proteins"),
            " table. ",
            "You can fill this table in several ways and adjust it before creating the network."
          ),
          tags$ul(
            tags$li(
              strong("Manual text input:"),
              " Type or paste protein identifiers (one per line) into the text area. ",
              "When you click ", em("Add"), ", the module checks each line against the chosen identifier column ",
              "and keeps only exact matches that are present in your dataset. ",
              "Matching rows from the data table are added to the selection; other lines are ignored."
            ),
            tags$li(
              strong("Import from GSEA:"),
              " Use the GSEA selector to choose enriched pathways from the GSEA module. ",
              "For each selected pathway, the module retrieves the corresponding proteins from the GSEA results ",
              "and writes them into the text area. ",
              "You then use ", em("Add"), " to transfer the matching proteins into the selection table."
            ),
            tags$li(
              strong("Import from GO:"),
              " Use the GO selector to choose enriched terms from the GO module. ",
              "For each selected term, the module collects the associated proteins and appends them to the text area. ",
              "Again, only entries that exactly match the chosen identifier in your data are kept when you click ", em("Add"), "."
            ),
            tags$li(
              strong("Intersection vs union of pathways:"),
              " If you select several GSEA or GO entries, you can decide whether proteins must appear in all selected entries ",
              "(intersection) or can come from any of them (union). ",
              "This affects which proteins are written into the text area and are therefore available for addition."
            )
          ),
          p(
            "After proteins have been added, the ",
            em("Selected Proteins"),
            " table shows one row per selected entry from your dataset. ",
            "You can:"
          ),
          tags$ul(
            tags$li(
              strong("Use "),
              em("Remove"),
              strong(":"), " type or paste identifiers into the text area and click ", em("Remove"),
              " to delete matching entries from the selection table."
            ),
            tags$li(
              strong("Use "),
              em("Clear"),
              strong(":"), " remove all entries from the selection table at once."
            ),
            tags$li(
              strong("Copy to Clipboard:"),
              " copy the exact matches from the current text area (based on the selected identifier) as a plain list, for use outside the app."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Important:</strong> The STRING network is not built directly from the text in the input field. ",
            "It always uses the proteins currently listed in the ", em("Selected Proteins"), " table. ",
            "If a protein is not in this table, it will not be part of the network."
          )
        )
      ),

      # Step 3 — Configure STRING query and general options
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 3 — Configure STRING query and general options")),
        div(
          class = "panel-body",
          p("Define how STRING is queried and how dense the resulting network should be:"),
          tags$ul(
            tags$li(
              strong("STRING version:"),
              " Choose which version of the STRING database to use. ",
              "The module sends your selected proteins to this version when retrieving interactions."
            ),
            tags$li(
              strong("Score threshold:"),
              " Set the minimum interaction score. ",
              "Only interactions with a score at or above this value are kept, so higher thresholds give sparser but more confident networks."
            ),
            tags$li(
              strong("Interaction type:"),
              " Decide whether you want functional associations, physical interactions, or the full combined network. ",
              "All edges in the network come from the type you select."
            ),
            tags$li(
              strong("Layout type:"),
              " Choose a layout algorithm (for example force‑directed, circular or star‑like). ",
              "This affects how nodes are positioned when the network is drawn or when you change the layout."
            ),
            tags$li(
              strong("Minimum degree (number of edges):"),
              " Set a minimum number of connections required for a protein to remain visible. ",
              "Proteins with fewer connections than this threshold are hidden from the visualisation, ",
              "which helps to reduce clutter in large networks. ",
              "You can change this value at any time; the filter is reversible and does not change the underlying STRING data."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML(
            "<strong>Tip:</strong> Start with a moderate score threshold and a low minimum degree to see the overall structure. ",
            "If the network is too dense, increase the threshold or the minimum degree until the main patterns become clear."
          )
        )
      ),

      # Step 4 — Create and inspect the interactive network
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 4 — Create and inspect the interactive network")),
        div(
          class = "panel-body",
          p(
            "When you click ",
            strong("Create STRING network"),
            ", the module:"
          ),
          tags$ul(
            tags$li("takes the proteins from the ", em("Selected Proteins"), " table;"),
            tags$li("maps them to STRING identifiers and discards proteins that cannot be mapped;"),
            tags$li("retrieves interactions between the mapped proteins using your score and interaction settings;"),
            tags$li("clusters the network into groups of densely connected proteins;"),
            tags$li("draws an interactive network where nodes represent proteins and edges represent STRING interactions.")
          ),
          p(
            "You can interact with the network directly:"
          ),
          tags$ul(
            tags$li("Pan and zoom to explore different regions of the network."),
            tags$li("Click on nodes to select them and apply customised styling."),
            tags$li("Click on edges to focus on particular connections and adjust edge‑specific styling."),
            tags$li("Use the cluster checkboxes to select entire groups of proteins; the corresponding nodes and edges are highlighted together.")
          )
        ),
        div(
          class = "alert alert-success",
          HTML(
            "<strong>Tip:</strong> If you receive a message that no proteins are selected or mapped, ",
            "check that the ", em("Selected Proteins"), " table is not empty and that your chosen identifier type ",
            "matches the identifiers supported by the selected STRING version."
          )
        )
      ),

      # Step 5 — Customise network appearance and prepare for export
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5 — Customise appearance and prepare for export")),
        div(
          class = "panel-body",
          p(
            "After the network is created, you can adapt its appearance interactively through the controls in the STRING sidebar:"
          ),
          tags$ul(
            tags$li(
              strong("Node colours and size:"),
              " Change background and border colours, node size, frame width and shape for selected nodes. ",
              "Node styling is applied to the current node selection in the network view."
            ),
            tags$li(
              strong("Labels and fonts:"),
              " Adjust label size, font family and font style (plain, bold, italic or both) for selected nodes to highlight key proteins."
            ),
            tags$li(
              strong("Edge style:"),
              " Adapt edge colour, width and line type for selected edges or for edges connected to selected nodes. ",
              "This helps distinguish different parts of the network visually."
            ),
            tags$li(
              strong("Reset network:"),
              " Use the reset control to return the network to its initial state after creation, including layout and styling. ",
              "This is useful if you have experimented with many settings and want to start over without rerunning the STRING query."
            )
          ),
          p(
            "All of these changes are applied directly to the interactive network and are also respected when you export the network ",
            "or send it to the Plot Grid."
          )
        ),
        div(
          class = "alert alert-success",
          HTML(
            "<strong>Tip:</strong> Perform coarse filtering first (score threshold and minimum degree), ",
            "then use node and edge styling to highlight the most relevant clusters, hubs or connections before exporting the final figure."
          )
        )
      )
    ),

    # How to interpret the STRING network
    h3("Interpreting the STRING network"),
    div(
      class = "well",
      tags$ul(
        tags$li(
          strong("Clusters as functional groups:"),
          " Proteins are grouped into clusters of closely connected nodes. ",
          "These clusters often represent proteins that participate in related processes or pathways. ",
          "Use the cluster checkboxes to focus on one cluster at a time and inspect its members in more detail."
        ),
        tags$li(
          strong("Highly connected proteins (hubs):"),
          " Nodes with many edges can indicate central proteins within your selection. ",
          "They may connect several clusters or link different functional areas. ",
          "Highlighting such hubs with distinct colours or larger sizes can make them easier to identify in exported figures."
        ),
        tags$li(
          strong("Edge patterns:"),
          " Dense regions of edges suggest tightly connected modules, while sparse regions may indicate weaker or more isolated relationships. ",
          "If the network looks uniformly dense, increase the interaction score or minimum degree to reveal more structured patterns."
        ),
        tags$li(
          strong("Relation to your analysis:"),
          " The network always reflects only the proteins you selected from your own data. ",
          "If important proteins are missing, check that they were added to the selection table and can be mapped to STRING. ",
          "If clusters do not match expectations from GSEA or GO, review which pathways or terms were used as protein sources."
        ),
        tags$li(
          strong("Preparing figures:"),
          " Before exporting, ensure that labels remain readable (adjust font size and node size as needed) ",
          "and that colours are consistent with other figures from your analysis. ",
          "Use layout and filters to avoid overlapping labels and to keep the main message of the network clear."
        )
      )
    ),

    # Brief troubleshooting
    h3("Quick troubleshooting"),
    tags$ul(
      tags$li(
        strong("No proteins in selection table:"),
        " enter identifiers in the text area or import from GSEA/GO, then click ",
        em("Add"),
        " to fill the selection table."
      ),
      tags$li(
        strong("No network appears:"),
        " check that the selection table is not empty and that your identifier type matches the identifiers supported by STRING."
      ),
      tags$li(
        strong("Network too dense:"),
        " increase the interaction score threshold or raise the minimum degree until the main structure becomes visible."
      )
    )
  )
}

render_STRING_customizing_content_STRING <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Node Colours and Size"),
    p(
      "Use the node controls to highlight important proteins and keep the network readable."
    ),
    div(
      class = "well",
      h4("Colours"),
      tags$ul(
        tags$li(
          strong("Fill colour:"),
          " changes the inside colour of selected nodes. ",
          "It is applied to the nodes you selected in the network view. ",
          ""
        ),
        tags$li(
          strong("Border colour:"),
          " changes the outline colour of selected nodes. ",
          "This is useful to emphasise a subset of proteins while keeping their fill colour similar to the rest."
        )
      ),
      h4("Size"),
      tags$ul(
        tags$li(
          strong("Node size:"),
          " increases or decreases the overall size of selected nodes, including their label area. ",
          "This is applied to all currently selected nodes."
        ),
        tags$li(
          strong("Frame width:"),
          " controls how thick the node border appears. ",
          "A thicker frame can help mark key proteins such as hubs or cluster representatives."
        )
      ),
      div(
        class = "alert alert-success",
        HTML(
          "<strong>Tip:</strong> When adjusting size and colours, first select the nodes in the network view ",
          "and then change the values in the sidebar. ",
          "Changes apply immediately to the current selection and are preserved when you export or filter the network."
        )
      )
    ),

    h3("Node Shape and Labels"),
    p(
      "Shape and labels help distinguish protein groups visually without relying only on colour."
    ),
    tags$dl(
      tags$dt("Node shape"),
      tags$dd(
        "You can switch the geometric shape of selected nodes (for example between circular and alternative shapes). ",
        "The chosen shape applies to the current selection and is kept when you change the layout or export the network."
      ),
      tags$dt("Label size"),
      tags$dd(
        "Controls the font size of node labels. ",
        "Use larger text for a small set of key proteins and smaller text when many labels are shown in a dense area."
      ),
      tags$dt("Font family"),
      tags$dd(
        "Select a font family for labels (for example a standard sans‑serif). ",
        "All selected nodes use the chosen font, which is also used in exported images."
      ),
      tags$dt("Font style"),
      tags$dd(
        "Switch between plain, bold, italic, or bold‑italic for labels of selected nodes. ",
        "This is implemented by reformatting the label text and by tracking style flags internally, ",
        "so the chosen style remains active for these nodes until you reset the network or change it again."
      )
    ),

    h3("Edge Style"),
    p(
      "Use edge settings to emphasise particular connections or to tone down the background network."
    ),
    div(
      class = "well",
      h4("Width and Colour"),
      tags$ul(
        tags$li(
          strong("Edge width:"),
          " adjusts the thickness of selected edges or of edges connected to selected nodes, ",
          "depending on which elements you have clicked last. ",
          "This lets you, for example, highlight all edges around a protein of interest."
        ),
        tags$li(
          strong("Edge colour:"),
          " changes the colour of selected edges or of edges around selected nodes. ",
          "Use a distinct colour to mark specific interactions without changing the rest of the network."
        )
      ),
      h4("Line type"),
      tags$ul(
        tags$li(
          strong("Edge type:"),
          " chooses the line style for selected or connected edges (for example solid or dashed variants). ",
          "The module applies your choice to the current selection only and keeps other edges unchanged."
        )
      ),
      div(
        class = "alert alert-info",
        HTML(
          "<strong>Note:</strong> When both nodes and edges are selected, the module decides whether edge settings apply to ",
          "clicked edges only or to edges around selected nodes based on the most recent selection in the network view."
        )
      )
    ),

    h3("Layout, Filtering and Reset"),
    p(
      "Layout and filtering change how much of the network is visible and how it is arranged on screen."
    ),
    tags$dl(
      tags$dt("Layout selection"),
      tags$dd(
        "You can switch between several layout types. ",
        "When you change the layout, the module recomputes node positions from the current network, ",
        "updates all node coordinates and redraws the plot without changing node or edge styling."
      ),
      tags$dt("Minimum degree filter"),
      tags$dd(
        "Use the minimum degree setting to hide nodes with very few connections. ",
        "The module calculates for each protein how many edges it has and only keeps nodes and edges that meet the threshold. ",
        "Custom colours, shapes and label styles are preserved for the remaining nodes."
      ),
      tags$dt("Reset network"),
      tags$dd(
        "The reset control restores the network to its initial state after creation: ",
        "original positions, default node and edge styles, and the current layout preset. ",
        "Selections are cleared, and visual changes made since creation are discarded, ",
        "while the underlying STRING result itself is left unchanged."
      )
    ),

    h3("Interactive Features"),
    p(
      "The STRING view is fully interactive and tightly connected to the customization controls."
    ),
    tags$ul(
      tags$li(
        strong("Zoom and pan:"),
        " Use the mouse or trackpad to zoom in and out, and drag the background to move across the network. ",
        "This does not change the stored layout and can be used freely during exploration."
      ),
      tags$li(
        strong("Drag‑and‑drop positions:"),
        " You can drag individual nodes in the network to new positions. ",
        "The module records the new coordinates and updates the stored node positions so they remain fixed until you change the layout or reset the network."
      ),
      tags$li(
        strong("Click to select:"),
        " Clicking a node selects it and updates the sidebar controls to reflect the selected node’s current styling. ",
        "Clicking an edge selects that edge and updates the edge‑specific controls. ",
        "Cluster selection via checkboxes selects all nodes of the chosen clusters and their connecting edges."
      ),
      tags$li(
        strong("Selection scope:"),
        " Node-based settings affect the nodes selected in the network view, while edge-based settings affect the selected edges or edges connected to selected nodes."
      )
    ),

    h3("Applying Changes and Export"),
    p(
      "All customization settings act directly on the interactive network and are preserved for export. ",
      "When you use the download options or send the network to the Plot Grid, the exported figure reflects the current layout, ",
      "positions, colours and styles, including any manual drag‑and‑drop changes and node‑ or edge‑specific styling."
    )
  )
}

render_STRING_saveplots_content_STRING <- function() {
  div(
    h2("Save and Export"),
    hr(),

    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download Plot:"), " saves the current STRING network view as a static file."),
      tags$li(tags$b("Add current plot to Grid:"), " sends the network as a plot object to Plot Grid for composite figures.")
    ),

    h3("Plot file formats"),
    p("The download panel supports the following formats:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats that use DPI to define output sharpness."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats that remain sharp when resized and are suitable for figure editing workflows.")
    ),

    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " set the physical export size of the network figure."),
      tags$li(tags$b("DPI:"), " controls resolution for raster formats (PNG, JPEG, TIFF)."),
      tags$li("Higher DPI improves print quality for raster images but increases file size."),
      tags$li("SVG and PDF are resolution-independent and are typically preferred for post-processing in vector editors.")
    ),

    div(
      class = "alert alert-info",
      h4("Practical tip"),
      p(
        "Choose SVG or PDF when you plan to adjust labels, line weights, or panel layout in external software. ",
        "Choose TIFF/PNG when a raster-only workflow is required by journal or slide pipelines."
      )
    ),

    h3("Recommended export workflow"),
    tags$ol(
      tags$li("Finalize network content first (selected proteins, score threshold, interaction type, and optional neighbor expansion)."),
      tags$li("Adjust readability (layout, degree filter, node size, label size, and edge emphasis)."),
      tags$li("Choose format, width, height, and DPI according to target output."),
      tags$li("Download and inspect the file at intended display scale before reporting."),
      tags$li("If needed, refine settings and export again to preserve scientific clarity.")
    ),

    h3("Add the current network to Plot Grid"),
    tags$ul(
      tags$li("Use the optional label field to name the panel clearly for multi-panel figures."),
      tags$li("Grid integration stores the current network state, including active node/edge styling and label variant."),
      tags$li("Use this to combine STRING networks with PCA, volcano, heatmap, or enrichment panels in one assembled figure.")
    ),

    h3("Good scientific practice"),
    tags$ul(
      tags$li("Report key network settings with exported figures: identifier type, score threshold, interaction type, and any neighbor expansion/filter settings."),
      tags$li("Avoid over-interpretation of dense networks; show only readability-preserving views that support the stated conclusion."),
      tags$li("Retain underlying protein lists and supporting analyses (GO/GSEA/quantitative results) alongside final figures.")
    ),

    h3("Troubleshooting"),
    tags$ul(
      tags$li(tags$b("Download is unavailable:"), " create a STRING network first."),
      tags$li(tags$b("Figure looks too crowded:"), " increase score threshold, apply degree filtering, or reduce label density before export."),
      tags$li(tags$b("Raster output is blurry:"), " increase DPI and/or export at larger width and height."),
      tags$li(tags$b("Need editable graphics:"), " switch to SVG or PDF.")
    )
  )
}
