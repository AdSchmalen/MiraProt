############
# User Guide — Content (initial stubs for Plot Grid)

render_grid_overview_content_grid <- function() {
  div(
    h2("Plot Grid Module — Overview"),
    hr(),

    div(
      class = "alert alert-info",
      h4("Purpose"),
      p(
        "The Plot Grid module collects plots from other MiraProt modules and combines them into a single multi‑panel figure. ",
        "It does not run analyses itself. Instead, it takes existing plots (for example from Volcano, GSEA, GO, PCA or Heatmap) ",
        "and arranges them in a grid that you can preview in the app and export in different formats."
      )
    ),

    h3("General — What the Plot Grid Does in MiraProt"),

    h4("Where the plots come from"),
    p(
      "Plots are created in other analysis modules (such as Volcano, GSEA, GO, PCA or Heatmap). ",
      "These modules can send their current plot to the Plot Grid. ",
      "The Plot Grid keeps a shared selection of all plots that have been added and uses this selection as the basis for the grid."
    ),
    p(
      "Each entry in the selection represents one panel in the grid. ",
      "The selection also remembers the order of panels, whether a panel should receive a label and, if enabled, ",
      "how many rows and columns a panel should span."
    ),

    h4("What the Plot Grid can do"),
    tags$ul(
      tags$li(
        "Collect plots from multiple modules and place them together in one combined figure."
      ),
      tags$li(
        "Arrange panels in a grid with a configurable number of rows and columns."
      ),
      tags$li(
        "Make selected panels larger or smaller by letting them span multiple rows and columns. ",
        "Span controls are always available for every panel in the selection."
      ),
      tags$li(
        "Align panels horizontally and/or vertically using alignment modes provided by the underlying plotting library."
      ),
      tags$li(
        "Control panel labels (for example A, B, C, …), label size and whether empty panels should be labeled."
      ),
      tags$li(
        "Adjust outer margins around the combined figure."
      ),
      tags$li(
        "Fine-tune individual panel spacing with per-plot margin offsets (top, right, bottom, left) ",
        "that are applied on top of the global margins."
      ),
      tags$li(
        "Optionally hide individual plot titles in the combined figure."
      ),
      tags$li(
        "Unify or override legend positions for all panels (for example force all legends to the bottom or hide them)."
      ),
      tags$li(
        "Preview changes automatically with the auto-update toggle (enabled by default) ",
        "or manually via the Create Plot button."
      ),
      tags$li(
        "Export the final grid as PNG, JPEG, TIFF, PDF or SVG with configurable size and resolution."
      )
    ),

    h4("What the Plot Grid does not do"),
    tags$ul(
      tags$li(
        "It does not generate new analysis results. All data processing and statistical analysis happen in other modules."
      ),
      tags$li(
        "It does not change the content or style of individual plots beyond margins, titles and legend placement. ",
        "You should adjust colours, axis labels and other details in the original modules before sending plots to the grid."
      ),
      tags$li(
        "It expects plots that behave like standard ggplot objects. ",
        "Highly specialised or pre‑composed multi‑panel plots from some modules may have limited support for alignment and legend forcing."
      )
    ),

    h3("Key Features"),

    h4("Flexible grid layout"),
    tags$ul(
      tags$li(
        "You choose the number of rows and columns for the grid. ",
        "This defines the basic layout for how many panels can be shown and how they are arranged."
      ),
      tags$li(
        "An optional layout optimisation can suggest a more compact combination of rows and columns based on the number of panels, ",
        "the current spans and the intended output width-to-height ratio."
      ),
      tags$li(
        "Empty panels can be added as placeholders, for example to reserve space for plots that will be created later or will be added to the grid outside of MiraProt."
      )
    ),

    h4("Custom panel sizes with spans"),
    tags$ul(
      tags$li(
        "When span support is enabled, each panel can be configured to occupy more than one row or column in the grid."
      ),
      tags$li(
        "The module checks whether the requested combination of spans fits into the chosen grid size and tries to place all panels without overlap."
      ),
      tags$li(
        "If some span combinations cannot be realised within the current grid, they are skipped or adjusted; ",
        "you may need to change the number of rows and columns or simplify spans."
      )
    ),

    h4("Labeling and margins"),
    tags$ul(
      tags$li(
        "Panels can receive automatic labels (letters or numbers) or custom label text. ",
        "You can also choose to disable labels entirely."
      ),
      tags$li(
        "A per‑panel switch controls whether a panel is included in the label sequence. ",
        "This is useful when you want to skip labels for technical or empty panels."
      ),
      tags$li(
        "Outer margins can be adjusted on all four sides of the combined figure. ",
        "This helps to avoid clipped titles or axis text when the grid is used in reports or publications."
      )
    ),

    h4("Legend handling and complex plots"),
    tags$ul(
      tags$li(
        "By default, the Plot Grid keeps legend positions as defined in the original panels."
      ),
      tags$li(
        "If requested, the module tries to enforce a common legend position (top, bottom, left, right or hidden) for all panels in the grid."
      ),
      tags$li(
        "For some complex multi‑panel or converted plots (for example certain GSEA, GO or Heatmap outputs), ",
        "not all legend overrides are guaranteed to work. In these cases the module uses internal checks and fallbacks to avoid breaking the figure."
      )
    ),

    h3("Common Use Cases"),
    tags$ul(
      tags$li(
        "Combine several Volcano, GSEA, GO, PCA or Heatmap plots into a single multi‑panel figure for a manuscript."
      ),
      tags$li(
        "Create a consistent overview figure where each panel shares the same output size and resolution."
      ),
      tags$li(
        "Highlight one or two key panels by letting them span multiple rows or columns while keeping supporting panels smaller."
      ),
      tags$li(
        "Standardise legend placement across panels that were created in different modules."
      )
    ),

    h3("Typical Workflow"),
    tags$ol(
      tags$li(
        "In one of the analysis modules (for example Volcano, GSEA, GO, PCA or Heatmap), create a plot and send it to the Plot Grid."
      ),
      tags$li(
        "Repeat this for all plots you want to include. Each added plot appears in the Plot Grid selection list."
      ),
      tags$li(
        "Open the Plot Grid tab. Review the selection, adjust the order of panels and configure their spans."
      ),
      tags$li(
        "Set the number of rows and columns, choose alignment and label options, adjust margins and, if needed, legend behaviour."
      ),
      tags$li(
        "With the ", strong("Auto-update preview"), " toggle enabled (the default), the preview updates automatically ",
        "after each change. Alternatively, turn off auto-update and click ", strong("Create Plot"), " to update manually."
      ),
      tags$li(
        "If desired, use the layout optimisation to try a more compact grid based on the current selection and spans."
      ),
      tags$li(
        "Choose output size, resolution and file format, then download the final figure."
      )
    )
  )
}

render_grid_selection_content_grid <- function() {
  div(
    h2("Plot selection and interaction with the Plot Grid"),
    hr(),

    # ==============================
    # Topic 1: Sending plots to the Plot Grid
    # ==============================

    h3("Sending plots from other modules"),
    p(
      "Most plotting modules in MiraProt allow you either to download the current plot directly ",
      "or to send it to the global Plot Grid. This section explains the common behaviour across modules."
    ),

    h4("Creating a plot and adding it to the grid"),
    tags$ul(
      tags$li(
        "First create a plot in the respective analysis module (for example Volcano, GSEA, GO, PCA or Heatmap) ",
        "by choosing data, filters and appearance options and then generating the plot in that module."
      ),
      tags$li(
        "Once the plot has been created, it becomes the current plot in that module. ",
        "You can now use that module’s download controls if you only need a single‑panel figure."
      ),
      tags$li(
        "To use the same plot in a multi‑panel figure, click the control in that module that adds the current plot to the Plot Grid ",
        "(typically an ", strong("Add to Grid"), " button or similarly labelled action). ",
        "Each time you use this control, the current plot together with its title or label is added to the global Plot Grid selection."
      ),
      tags$li(
        "You can repeat this for multiple plots, possibly from different modules. ",
        "All plots sent in this way appear together in the Plot Grid tab, where they can be arranged into a grid."
      )
    ),
    div(
      class = "alert alert-info",
      strong("Note: "),
      "When adding a plot to the Plot Grid, modules offer an optional text field where you can provide a user‑defined tag or label. ",
      "This tag is appended to the module’s default plot name and makes it easier to distinguish multiple plots. ",
      "If you do not provide a tag, the module’s standard name is used. ",
      "Plots with the same resulting name will overwrite each other in the Plot Grid selection."
    ),

    # ==============================
    # Topic 2: Managing the selection inside the Plot Grid
    # ==============================

    h3("Working with the selection in the Plot Grid"),
    p(
      "Once plots have been sent from other modules, the Plot Grid tab shows them in a dedicated selection area on the right. ",
      "This section explains how to review the list of plots, control which panels are included, change the order, add blanks, adjust spans and ",
      "trigger the grid preview and download."
    ),

    h3("Step-by-Step Guide"),
    div(
      class = "workflow-box",

      # Step 1 — Open the Plot Grid and inspect the current list
      div(
        class = "panel panel-primary",
        div(class = "panel-heading", h4("Step 1 — Open the Plot Grid and inspect the current list")),
        div(
          class = "panel-body",
          p(
            "Go to the ", strong("Plot Grid"), " tab. On the right-hand side, the ", strong("Selection"),
            " panel lists all plots that have been added from other modules."
          ),
          tags$ul(
            tags$li(
              "Each entry in the list corresponds to one panel in the grid and shows its identifier and, where available, a label from the source module."
            ),
            tags$li(
              "If no plots have been added yet, the selection area displays a short message indicating that the grid is empty."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Tip:</strong> If you do not see the expected plot in the list, return to the original module, recreate the plot if needed and use the control that adds it to the Plot Grid again.")
        )
      ),

      # Step 2 — Decide which panels should receive a label
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 2 — Choose which panels are labeled")),
        div(
          class = "panel-body",
          p(
            "For each entry in the selection, there is a checkbox that controls whether the panel should be included ",
            "when the Plot Grid generates panel labels."
          ),
          tags$ul(
            tags$li(
              "Leave the checkbox selected if the panel should receive a label and be counted in the label sequence."
            ),
            tags$li(
              "Clear the checkbox if you want the panel to stay unlabeled even when labels are turned on in the grid options."
            ),
            tags$li(
              "This is useful for empty placeholders or technical plots that should not appear in the main label sequence."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Note:</strong> Labeling behaviour also depends on the global label options in the Plot Grid (label mode and label size). The checkbox here decides per panel whether it participates in labeling, including blank panels.")
        )
      ),

      # Step 3 — Change the order of panels
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 3 — Adjust the order of panels")),
        div(
          class = "panel-body",
          p(
            "The order of entries in the selection list determines how panels are placed into the grid. ",
            "Panels are filled from left to right and top to bottom, taking into account any custom spans."
          ),
          tags$ul(
            tags$li(
              "Use the ", strong("Up"), " and ", strong("Down"), " buttons next to a plot entry to move it earlier or later in the list."
            ),
            tags$li(
              "The Plot Grid remembers this order and uses it when composing the grid (either automatically or when you click ", strong("Create Plot"), ")."
            ),
            tags$li(
              "If you later change spans or grid dimensions, you can revisit the selection and adjust the order again for a clearer layout."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Tip:</strong> When you use the layout optimisation in the Plot Grid options, you can optionally allow it to compact the order. If this is enabled, the selection order may change slightly to achieve a denser arrangement.")
        )
      ),

      # Step 4 — Add blank panels as placeholders
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 4 — Add blank placeholders if needed")),
        div(
          class = "panel-body",
          p(
            "Sometimes you may want empty space in your grid, for example to reserve room for a plot you will create later or to separate groups of panels."
          ),
          tags$ul(
            tags$li(
              "Use the ", strong("Add blank"), " button in the Plot Grid controls to insert an empty panel into the selection."
            ),
            tags$li(
              "Blank panels appear in the selection list like regular entries and can be moved up or down with the same buttons."
            ),
            tags$li(
              "When custom plot sizes are enabled, blanks can receive their own span settings (rows and columns) just like real plots."
            ),
            tags$li(
              "Blank panels can also take part in labeling; you can enable or disable labeling participation for each blank panel directly in the Selection list using its ",
              strong("Include in labeling"),
              " checkbox."
            )
          )
        )
      ),

      # Step 5 — Adjust spans for individual panels
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5 — Configure custom panel sizes (spans)")),
        div(
          class = "panel-body",
          p(
            "If you want certain panels to be larger than others, you can let them span multiple rows or columns in the grid."
          ),
          tags$ul(
            tags$li(
              "Each entry in the selection always shows numeric inputs for ", em("Cols"), " (columns) and ", em("Rows"),
              " that define how many grid cells the panel occupies."
            ),
            tags$li(
              "These inputs are automatically limited to the current number of rows and columns in the grid. ",
              "When you reduce the grid size, any spans that exceed the new dimensions are clamped automatically."
            ),
            tags$li(
              "The Plot Grid tries to place all panels with their requested spans. ",
              "If a combination does not fit into the current grid, some panels may need adjustment or will effectively behave like smaller spans."
            ),
            tags$li(
              "With auto-update enabled, the preview refreshes automatically after span changes."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Tip:</strong> Large highlight panels often work well when they span two columns or two rows, while supporting panels use a span of 1 &times; 1. If the grid becomes too sparse or cannot fit all spans, reduce spans or increase the number of rows/columns.")
        )
      ),

      # Step 5b — Adjust per-plot margins
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 5b — Adjust per-plot margin offsets")),
        div(
          class = "panel-body",
          p(
            "Each panel in the selection provides individual margin offset controls (Top, Right, Bottom, Left) ",
            "that add to or subtract from the global margins set in the Plot Options."
          ),
          tags$ul(
            tags$li(
              "Values are specified in points (pt) and range from -200 to +500."
            ),
            tags$li(
              "Positive values add extra space around that panel; negative values reduce spacing, which can help ",
              "tighten the layout or compensate for axis labels that create uneven whitespace."
            ),
            tags$li(
              "Per-plot margins are applied after alignment, so they do not interfere with axis alignment between panels."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Tip:</strong> Use per-plot margin offsets to fine-tune individual panels without affecting the overall grid spacing. For example, a panel with a long y-axis label may benefit from a slightly larger left margin offset.")
        )
      ),

      # Step 6 — Customise grid-level options (size, labels, margins, legends)
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 6 — Customise grid-level options")),
        div(
          class = "panel-body",
          p(
            "In addition to per-panel settings, you can adjust several global options that affect the entire grid."
          ),
          tags$ul(
            tags$li(
              strong("Grid size:"),
              " set the number of rows and columns to control how many panels fit into the figure and how they are arranged."
            ),
            tags$li(
              strong("Alignment:"),
              " choose whether panels should be aligned horizontally, vertically, both or not enforced, depending on how similar their axes and layouts are."
            ),
            tags$li(
              strong("Labels:"),
              " choose a label mode (none, automatic letters or numbers, or custom labels) to define the sequence style, and adjust label size. Whether any panel (including blanks) participates is controlled per panel in Selection via ",
              strong("Include in labeling"),
              "."
            ),
            tags$li(
              strong("Titles:"),
              " optionally hide individual plot titles to keep the combined figure cleaner when many panels are shown."
            ),
            tags$li(
              strong("Margins:"),
              " adjust the outer margins (top, right, bottom, left) so that axis text and titles are not clipped in the final output."
            ),
            tags$li(
              strong("Legend control:"),
              " keep original legend positions or override them (e.g. move all legends to the bottom or hide them). ",
              "This is especially helpful when plots from different modules should share a unified legend placement."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Note:</strong> When <strong>Auto-update preview</strong> is enabled (the default), the preview refreshes automatically after each change, with a short delay to avoid flickering during rapid adjustments. You can also turn off auto-update and click <strong>Create Plot</strong> to update manually.")
        )
      ),

      # Step 7 — Remove single panels or clear the selection
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 7 — Remove panels or clear the list")),
        div(
          class = "panel-body",
          p(
            "You can clean up the selection at any time if some panels are no longer needed."
          ),
          tags$ul(
            tags$li(
              strong("Remove a single panel:"),
              " click the ", strong("Remove"), " button next to its entry in the selection. ",
              "The panel disappears from the list and will no longer be included in the grid."
            ),
            tags$li(
              strong("Clear the entire selection:"),
              " use the ", strong("Clear selection"), " button in the Plot Grid controls to remove all entries at once. ",
              "This is useful if you want to start over with a new set of plots."
            ),
            tags$li(
              "With auto-update enabled, the preview refreshes automatically after removing or clearing panels. ",
              "If no panels remain, the preview area shows a placeholder message instead of a grid."
            )
          )
        ),
        div(
          class = "alert alert-success",
          HTML("<strong>Tip:</strong> You can always go back to the original modules (e.g. Volcano, GSEA, GO, PCA, Heatmap), recreate plots and add them again to build a fresh grid layout.")
        )
      ),

      # Step 8 — Download the final grid
      div(
        class = "panel panel-default",
        div(class = "panel-heading", h4("Step 8 — Download the grid figure")),
        div(
          class = "panel-body",
          p(
            "After you are satisfied with the selection and layout, you can export the composed figure from the Plot Grid tab."
          ),
          tags$ul(
            tags$li(
              "In the download panel below the preview, choose the desired output resolution (PPI), width and height in inches, and the file format ",
              "(PNG, JPEG, TIFF, SVG or PDF)."
            ),
            tags$li(
              "Click ", strong("Download"), " to save the current grid preview to a file with the chosen settings."
            ),
            tags$li(
              "The download always rebuilds the grid from the current settings, so it reflects ",
              "the latest configuration regardless of whether auto-update is on or off."
            )
          )
        ),
        div(
          class = "alert alert-info",
          HTML("<strong>Note:</strong> If no plots are currently selected in the grid, the download is cancelled and an information message is shown instead of creating an empty file.")
        )
      )
    )
  )
}

render_grid_customizing_content_grid <- function() {
  div(
    h2("Customization"),
    hr(),

    h3("Grid Options"),
    p(
      "Use the controls in the ", strong("Grid options"), " panel to define the overall layout and basic appearance of the plot grid."
    ),

    h4("Adjust grid layout (Rows and Columns)"),
    p(
      "The grid layout is defined by the number of rows and columns."
    ),
    tags$dl(
      tags$dt("Rows"),
      tags$dd(
        "Sets how many horizontal rows of panels the grid has. Increase this value to add more horizontal layers of plots."
      ),
      tags$dt("Columns"),
      tags$dd(
        "Sets how many vertical columns of panels the grid has. Increase this value to place more plots next to each other."
      )
    ),
    p(
      "The maximum number of panels you can show is limited by the chosen combination of rows, columns and any custom plot sizes (spans) you define."
    ),

    h4("Align"),
    p(
      "The alignment option controls how panels are aligned relative to each other inside the grid."
    ),
    tags$ul(
      tags$li(strong("none:"), " no enforced alignment; each plot keeps its own axis size and layout."),
      tags$li(strong("h:"), " tries to align panels horizontally (for example shared vertical axes)."),
      tags$li(strong("v:"), " tries to align panels vertically (for example shared horizontal axes)."),
      tags$li(strong("hv:"), " requests both horizontal and vertical alignment when feasible.")
    ),
    p(
      "Alignment can improve comparability between panels with similar axes but may compress space when panels are very different."
    ),

    h4("Labels and titles"),
    p(
      "Label settings control how panel labels (for example A, B, C, …) are generated and displayed."
    ),
    tags$dl(
      tags$dt("Labels (mode)"),
      tags$dd(
        "Choose how labels are generated:",
        tags$ul(
          tags$li(strong("none:"), " no automatic labels are shown."),
          tags$li(strong("auto_letters:"), " panels are labeled with capital letters (A, B, C, …)."),
          tags$li(strong("auto_numbers:"), " panels are labeled with numbers (1, 2, 3, …)."),
          tags$li(strong("custom:"), " use your own label sequence.")
        )
      ),
      tags$dt("Custom labels (when custom mode is selected)"),
      tags$dd(
        "Free text field where you can enter labels separated by commas (for example ",
        code("A1, A2, B1, B2"),
        "). The sequence is applied in the order of panels in the Selection."
      ),
      tags$dt("Label size"),
      tags$dd(
        "Numeric control for the size of label text. Use larger values for presentations or high‑resolution exports, ",
        "smaller values for compact layouts."
      ),
      tags$dt("Label participation for blank panels"),
      tags$dd(
        "Blank panels can receive labels like other panels. Their participation is controlled per panel in the ",
        strong("Selection"),
        " area via the ",
        strong("Include in labeling"),
        " checkbox."
      ),
      tags$dt("Hide plot titles"),
      tags$dd(
        "Checkbox that removes the original titles from individual plots before they are combined in the grid. ",
        "Use this when you prefer to rely on panel labels and a shared figure caption instead of repeated titles."
      )
    ),
    div(
      class = "alert alert-info",
      HTML("<strong>Important:</strong> Global label settings apply to all panels. Per‑panel checkboxes in the Selection area decide whether a specific panel participates in labeling.")
    ),

    hr(),
    h3("Plot Options"),
    p(
      "Plot options in the Plot Grid control legend behaviour and outer margins for the combined figure."
    ),

    h4("Legend Control"),
    p(
      "Legend control lets you keep original legend positions from individual modules or enforce a unified position in the combined figure."
    ),
    tags$ul(
      tags$li(strong("Preserve original (recommended):"), " each plot keeps the legend position defined in its source module."),
      tags$li(strong("Force to bottom / top / left / right:"), " try to move all legends to the chosen side of the grid."),
      tags$li(strong("Hide all legends:"), " remove legends completely, if you add annotations or labels manually elsewhere.")
    ),
    p(
      "For most standard plots, forced legend positions work as expected. For some complex multi‑panel or converted plots, ",
      "legend overrides may be partially limited; the module falls back gracefully to avoid breaking the figure."
    ),

    h4("Plot Margins"),
    p(
      "Plot margins control the empty space around the entire multi‑panel figure."
    ),
    tags$dl(
      tags$dt("Top, Right, Bottom, Left"),
      tags$dd(
        "Each margin is specified as a numeric value. Larger values add more white space around the grid; ",
        "smaller values bring panels closer to the edge of the figure."
      )
    ),
    p(
      "Use larger margins if axis labels or legends appear too close to the border, and smaller margins if you need to fit the figure into a tight layout."
    ),

    hr(),
    h3("Custom Plot Sizes (Spans)"),
    p(
      "Custom plot sizes allow individual panels to occupy more than one grid cell in rows and/or columns."
    ),

    h4("Adjusting custom plot sizes"),
    p(
      "Each entry in the Selection area shows numeric inputs for ",
      em("Cols"),
      " (number of columns) and ",
      em("Rows"),
      " (number of rows) that the panel should span."
    ),
    tags$ul(
      tags$li("By default, each panel occupies exactly one cell in the grid (1 column x 1 row)."),
      tags$li("You can make key panels larger by increasing their column or row span."),
      tags$li("Span values are automatically limited so that no panel can request more columns or rows than the grid provides. ",
              "When you reduce the grid dimensions, any spans that exceed the new size are clamped automatically.")
    ),
    p(
      "The grid layout algorithm uses these spans when placing panels."
    ),

    hr(),
    h3("Per-Plot Margin Offsets"),
    p(
      "In addition to the global plot margins, each panel in the Selection area provides individual margin offset ",
      "controls for all four sides (Top, Right, Bottom, Left)."
    ),
    tags$dl(
      tags$dt("Range"),
      tags$dd(
        "Values are specified in points (pt) and range from -200 to +500."
      ),
      tags$dt("Positive values"),
      tags$dd(
        "Add extra whitespace around that specific panel, which can help separate it visually from neighbouring panels."
      ),
      tags$dt("Negative values"),
      tags$dd(
        "Reduce the spacing around that panel, useful for tightening layouts or compensating for axis labels ",
        "that create uneven whitespace."
      )
    ),
    p(
      "Per-plot margins are applied after panel alignment, so they do not interfere with axis alignment between panels."
    ),

    hr(),
    h3("Selection‑based Customization"),
    p(
      "Some customization options are controlled per panel in the ",
      strong("Selection"),
      " area on the right side of the Plot Grid tab."
    ),

    h4("Hide panels from labeling"),
    p(
      "Within the Selection list, each panel has a checkbox that controls whether it should be considered for labeling."
    ),
    tags$ul(
      tags$li("Leaving the checkbox selected means the panel can receive a label according to the global label settings."),
      tags$li("Clearing the checkbox excludes the panel from the label sequence, even when labels are enabled globally."),
      tags$li("This is useful for blank placeholders, helper panels or any plot that should appear without a label.")
    ),

    h4("Change order"),
    p(
      "The position of each panel in the Selection list determines its placement in the grid."
    ),
    tags$ul(
      tags$li(
        "Use the ", strong("Up"), " and ", strong("Down"), " buttons next to a panel entry to move it earlier or later in the list."
      ),
      tags$li(
        "Panels are filled into the grid from left to right and top to bottom, taking their spans into account."
      )
    ),

    h4("Building publication-ready multi-panel figures"),
    p(
      "Use this checklist while adjusting layout and panel order so readability and interpretation are preserved in the final figure."
    ),
    tags$ul(
      tags$li("Choose panel combinations that answer one biological question per figure."),
      tags$li("Enforce consistent axis ranges whenever cross-panel comparison is part of the intended interpretation."),
      tags$li("Keep colour semantics consistent across modules (for example identical group colours and gradient meanings)."),
      tags$li("Limit the number of panels per figure; split into multiple figures when readability starts to decline."),
      tags$li("Use blank placeholders sparingly and only when they support a deliberate layout or annotation plan."),
      tags$li("Validate label readability at the final export dimensions, not only in the on-screen preview.")
    ),

    hr(),
    h3("Applying Customization Changes"),
    p(
      "All customization settings in the Plot Grid -- including layout, alignment, labels, legend control, margins, ",
      "per-plot margin offsets, spans and selection order -- are applied to the preview and downloads."
    ),
    div(
      class = "alert alert-info",
      HTML("<strong>Note:</strong> When <strong>Auto-update preview</strong> is enabled (the default), the preview refreshes automatically after each change, with a short debounce delay (500 ms) to avoid flickering during rapid adjustments. You can also turn off auto-update and click <strong>Create Plot</strong> to apply changes manually.")
    )
  )
}

render_grid_saveplots_content_grid <- function() {
  div(
    h2("Save and Export"),
    hr(),
    h3("What can be exported"),
    tags$ul(
      tags$li(tags$b("Download:"), " saves the current multi-panel grid exactly as shown in the Plot Grid preview."),
      tags$li(tags$b("Add plots from source modules:"), " individual modules (for example Volcano, GSEA, GO, PCA, Heatmap, Venn/UpSet) send their current plots to the shared grid selection, and those selected panels define what is exported in the final grid file."),
      tags$li("Only panels currently present and enabled in the Plot Grid selection are included in the exported figure."),
      tags$li("Panel order, spans, label settings, margins, and legend options are applied to the downloaded file.")
    ),
    h3("Plot file formats"),
    p("The Plot Grid download panel supports both raster and vector output:"),
    tags$ul(
      tags$li(tags$b("PNG"), ", ", tags$b("JPEG"), ", and ", tags$b("TIFF"), ": raster formats. They are pixel-based and their sharpness depends on the chosen dimensions and DPI."),
      tags$li(tags$b("SVG"), " and ", tags$b("PDF"), ": vector formats. They remain sharp when resized and are usually preferred for figure editing and publication workflows.")
    ),
    h3("Dimensions and DPI"),
    tags$ul(
      tags$li(tags$b("Width and Height:"), " define the physical output size of the full grid, not individual panels."),
      tags$li("As you increase the number of panels, keep enough total width and height so each panel remains readable at the final placement size."),
      tags$li(tags$b("DPI:"), " affects raster exports (PNG/JPEG/TIFF) by controlling pixel density and print sharpness."),
      tags$li("SVG and PDF are resolution-independent vector outputs, so DPI does not determine line or text sharpness in the same way.")
    ),
    div(
      class = "alert alert-info",
      h4("Practical tip: vector vs raster"),
      p(
        "Choose ", strong("SVG or PDF"), " when you need to resize panels, refine labels, or integrate the figure into Illustrator/Inkscape workflows. ",
        "Choose ", strong("TIFF/PNG/JPEG"), " when a raster file is specifically required (for example, some submission systems or slide tools), and then set dimensions and DPI high enough for the final use."
      )
    ),
    h3("Multi-panel export guidance"),
    tags$ul(
      tags$li("Plan export size from the final destination first (journal column width, report page layout, or slide canvas), then tune grid rows/columns and panel spans accordingly."),
      tags$li("For dense grids, increase total figure dimensions so axis text, legends, and annotations remain readable in every panel."),
      tags$li("If one panel carries the key biological message, give it more space via row/column spans instead of shrinking all panels equally."),
      tags$li("In journal/report workflows, test the exported figure by placing it in the target document template and checking legibility at final print/display size."),
      tags$li("When collaboration or revision is expected, keep a vector master export (PDF/SVG) and derive raster versions only for submission endpoints that require them.")
    ),
    h3("Good scientific practice before submission"),
    tags$ul(
      tags$li("Keep axis scales comparable across panels whenever scientific interpretation depends on visual comparison."),
      tags$li("Use consistent colour mappings and legend semantics across modules before adding plots to the grid."),
      tags$li("Standardize labels, abbreviations, and units so readers can compare panels without ambiguity."),
      tags$li("Avoid overcrowding: reduce unnecessary panels, split into multiple figures, or simplify annotations if readability drops."),
      tags$li("Always verify final export legibility (titles, axis labels, tick marks, legends, and panel labels) at the exact size used for submission or presentation.")
    )
  )
}
