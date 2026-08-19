# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_ui.R
#
# Purpose:
#   Defines the static UI component for the Annotation submodule of the
#   Data Wizard. Contains only layout and input widget declarations.
#
# Architectural Role:
#   UI layer of the annotation module. Sourced into modEnv via
#   datawizard_annotation.R and called from modAnnotationUI() to build the
#   tab panel content. This file has no knowledge of server-side state.
#
# IMPORTANT:
#   - This UI is rendered INSIDE the existing "Data expansion" tabsetPanel.
#   - It must NOT wrap itself in a wellPanel or tabPanel.
#   - It must NOT redefine the tabsetPanel.
#
# Structure:
#   1. datawizard_annotation_UI() - Returns a tagList of all UI controls:
#      - Source column selector
#      - Annotation strategy dropdown (replaces cross-species checkbox)
#      - Species selector
#      - Source key type selector
#      - Target key type selector (shared for intra- and cross-species)
#      - Target species (BioMart mode only)
#      - Collapse strategy selector
#      - Action buttons (Map IDs)
#      - Merge controls panel (Identifier Merging mode only)
#      - Status output area
#
# Notes:
#   - All input IDs are namespaced via the `ns` function argument.
#   - Keytype dropdowns start with the full safe Homo sapiens defaults.
#     Cache refreshes only expand or replace these choices when needed.
#   - Choices for dropdowns may be updated at runtime by observers in
#     datawizard_annotation_observer.R.
#   - Do not add server logic, reactive expressions, or observers here.
# ==============================================================================


datawizard_annotation_UI <- function(ns) {
  # Prefer the shared default helper from datawizard_annotation_utils.R when
  # this UI is sourced into a context that already has it. Keep the mirrored
  # static vector here so the startup UI never falls back to the older four-item
  # subset if the helper has not been sourced yet.
  default_keytypes_annotation <- if (exists("get_default_keytypes_for_organism", mode = "function")) {
    get_default_keytypes_for_organism("org.Hs.eg.db")
  } else {
    c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT", "REFSEQ", "GENENAME", "ALIAS")
  }

  tagList(
    shinyjs::useShinyjs(),

    h4("ID Annotation / Conversion"),

    # ------------------------------------------------------------------
    # Info box: mapping types — only the active mode's description is shown.
    # Each mode description is wrapped in a div with a shinyjs-controlled ID.
    # The observer (s1 in datawizard_annotation_utils_observer.R) toggles
    # visibility when the annotation_strategy dropdown changes.
    # ------------------------------------------------------------------
    div(
      class = "alert alert-info",
      style = "font-size: 13px; margin-bottom: 15px;",
      tags$b("Identifier Mapping"),

      # Annotation Hub description — visible by default (initial strategy)
      div(
        id = ns("info_annothub"),
        tags$p(
          style = "margin-top: 5px; margin-bottom: 0;",
          tags$b("Annotation Hub Intraspecies Mapping"), " uses organism annotation ",
          "databases (OrgDb) for precise identifier conversion within the same species."
        )
      ),

      # BioMart description — hidden by default
      shinyjs::hidden(
        div(
          id = ns("info_biomart"),
          tags$p(
            style = "margin-top: 5px; margin-bottom: 0;",
            tags$b("BioMart Intra-/Inter-species Mapping"), " uses BioMart/Ensembl Compara to find ",
            "orthologous genes across species. Note that even for the same species, ",
            "BioMart mode uses a different database and may yield different results."
          )
        )
      ),

      # Identifier Merging description — hidden by default
      shinyjs::hidden(
        div(
          id = ns("info_merge"),
          tags$p(
            style = "margin-top: 5px; margin-bottom: 0;",
            tags$b("Identifier Merging"), " combines multiple identifier columns into a single ",
            "column. Choose 'First non-empty only' to keep the first valid value per row, ",
            "or 'Concatenate all' to join all non-empty values with commas."
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # Source column selector — hidden in Identifier Merging mode because
    # that mode derives its input columns from the drag-and-drop list.
    # ------------------------------------------------------------------
    div(
      id = ns("source_column_panel"),
      fluidRow(
        column(12,
          selectInput(
            ns("source_column_annotation"),
            "Source Column (identifiers to convert):",
            choices = NULL,
            width = "100%"
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # Species selector (initial preset; expands after Update Organisms)
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        selectInput(
          ns("species_annotation"),
          "Source Species:",
          choices = c("Homo sapiens"              = "Homo sapiens",
                      "Mus musculus"               = "Mus musculus",
                      "Rattus norvegicus"          = "Rattus norvegicus",
                      "Drosophila melanogaster"    = "Drosophila melanogaster",
                      "Caenorhabditis elegans"     = "Caenorhabditis elegans",
                      "Saccharomyces cerevisiae"   = "Saccharomyces cerevisiae",
                      "Bos taurus"                 = "Bos taurus",
                      "Sus scrofa"                 = "Sus scrofa",
                      "Equus caballus"             = "Equus caballus"),
          selected = "Homo sapiens",
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(6,
        actionButton(
          ns("refresh_cache_annotation"),
          "Refresh Cache",
          icon = icon("refresh"),
          class = "btn-default btn-sm",
          style = "background-color: #2c3e50; border-color: #2c3e50; color: #fff;",
          width = "100%"
        )
      ),
      column(6,
        actionButton(
          ns("update_organisms_annotation"),
          "Update Organisms",
          icon = icon("globe"),
          class = "btn-info btn-sm",
          width = "100%"
        )
      )
    ),
    br(),

    # ------------------------------------------------------------------
    # Source key type
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        selectInput(
          ns("from_keytype_annotation"),
          "Source ID Type (what your IDs are):",
          choices = default_keytypes_annotation,
          selected = "SYMBOL",
          width = "100%"
        )
      )
    ),

    # ------------------------------------------------------------------
    # Target key type
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        selectInput(
          ns("to_keytype_annotation"),
          "Target ID Type (what to convert to):",
          choices = default_keytypes_annotation,
          selected = "ENSEMBL",
          width = "100%"
        )
      )
    ),

    # ------------------------------------------------------------------
    # Annotation strategy dropdown (replaces cross-species checkbox)
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        selectInput(
          ns("annotation_strategy"),
          "Annotation Strategy:",
          choices = c(
            "Annotation Hub Intraspecies Mapping" = "annothub",
            "BioMart Intra-/Inter-species Mapping" = "biomart",
            "Identifier Merging" = "merge"
          ),
          selected = "annothub",
          width = "100%"
        )
      )
    ),

    # ------------------------------------------------------------------
    # Conditional: target species (BioMart mode only)
    # Note: Target ID Type uses the shared to_keytype_annotation dropdown
    # above. When BioMart mode is active, its choices are updated
    # to BioMart-compatible types for the selected target species.
    # ------------------------------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("target_species_panel"),
        fluidRow(
          column(12,
            selectInput(
              ns("target_species_annotation"),
              "Target Species:",
              choices = c("Homo sapiens"              = "Homo sapiens",
                          "Mus musculus"               = "Mus musculus",
                          "Rattus norvegicus"          = "Rattus norvegicus",
                          "Drosophila melanogaster"    = "Drosophila melanogaster",
                          "Caenorhabditis elegans"     = "Caenorhabditis elegans",
                          "Saccharomyces cerevisiae"   = "Saccharomyces cerevisiae",
                          "Bos taurus"                 = "Bos taurus",
                          "Sus scrofa"                 = "Sus scrofa",
                          "Equus caballus"             = "Equus caballus"),
              selected = "Mus musculus",
              width = "100%"
            )
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # Ambiguous mapping info box — visible in mapping modes, hidden in
    # Identifier Merging mode (which does not use the collapse strategy).
    # ------------------------------------------------------------------
    div(
      id = ns("ambiguous_mapping_info"),
      div(
        class = "alert alert-info",
        style = "font-size: 13px; margin-bottom: 10px;",
        tags$b("Ambiguous Mappings:"),
        tags$p(
          style = "margin-top: 5px; margin-bottom: 0;",
          "When an identifier maps to multiple targets (one-to-many), results are ",
          "collapsed to a single cell per row. Choose 'First match' for the first ",
          "result, or 'Semicolon-separated' to keep all values. Many-to-one and ",
          "many-to-many mappings are handled similarly. ",
          tags$b("No rows are added or removed.")
        )
      )
    ),

    # ------------------------------------------------------------------
    # Collapse strategy
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        selectInput(
          ns("collapse_strategy_annotation"),
          "Ambiguous Mapping Strategy:",
          choices = c(
            "First match"          = "first",
            "Semicolon-separated"  = "semicolon"
          ),
          selected = "first",
          width = "100%"
        )
      )
    ),

    # ------------------------------------------------------------------
    # Merge controls panel (hidden by default; shown in merge mode)
    # ------------------------------------------------------------------
    shinyjs::hidden(
      div(
        id = ns("merge_controls_panel"),
        style = "margin-top: 10px;",

        h5("Identifier Merging"),
        tags$p(
          style = "font-size: 13px; color: #555; margin-bottom: 10px;",
          "Drag to reorder columns. Click 'x' to remove a column from the merge. ",
          "Columns are processed top to bottom."
        ),

        # Sortable identifier list (rendered dynamically via renderUI
        # in datawizard_annotation_utils_observer.R; drag-and-drop is
        # attached by sortable::sortable_js() inside the renderUI output)
        uiOutput(ns("merge_identifier_list_ui")),

        # Reset list button — own row, right-aligned
        fluidRow(
          column(6, offset = 6,
            actionButton(
              ns("merge_reset_list"),
              "Reset list",
              icon = icon("undo"),
              class = "btn-default btn-sm",
              width = "100%"
            )
          )
        ),

        br(),

        # Merge Behavior — own row, full width
        fluidRow(
          column(12,
            selectInput(
              ns("merge_behavior"),
              "Merge Behavior:",
              choices = c(
                "First non-empty only" = "first_non_empty",
                "Concatenate all"      = "concatenate_all"
              ),
              selected = "first_non_empty",
              width = "100%"
            )
          )
        ),

        br(),
        fluidRow(
          column(12,
            actionButton(
              ns("merge_run"),
              "Merge Identifier",
              width = "100%",
              class = "btn-success",
              style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;"
            )
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # Action button
    # ------------------------------------------------------------------
    fluidRow(
      column(12,
        actionButton(
          ns("run_annotation"),
          "Map IDs",
          width = "100%",
          class = "btn-success",
          style = "background-color: #18bc9c; border-color: #18bc9c; color: #fff;"
        )
      )
    ),

    # ------------------------------------------------------------------
    # Status output
    # ------------------------------------------------------------------
    br(),
    fluidRow(
      column(12,
        uiOutput(ns("annotation_status"))
      )
    )
  )
}
