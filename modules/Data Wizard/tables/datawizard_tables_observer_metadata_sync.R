# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_metadata_sync.R
# Purpose:
#   Provide the tables observer metadata sync portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

register_tables_metadata_sync_rendering <- function(context) {
  with(context, {
  metadata_sync_pending <- context$metadata_sync_pending
  output$metadata_sync_help <- renderUI({
    if (isTRUE(input$pause_metadata_sync)) {
      return(NULL)
    }

    tags$small(
      class = "text-muted",
      "Metadata syncs automatically after each edit."
    )
  })

  output$metadata_sync_controls <- renderUI({
    if (!isTRUE(input$pause_metadata_sync)) {
      return(NULL)
    }

    actionButton(
      ns("sync_metadata_now"),
      "Synchronize metadata",
      class = "btn-primary btn-sm",
      width = "100%"
    )
  })

  output$metadata_sync_status <- renderUI({
    if (!isTRUE(metadata_sync_pending())) {
      return(NULL)
    }

    div(
      class = "btn-default",
      style = paste(
        "display: block;",
        "width: 100%;",
        "padding: 8px 12px;",
        "margin-bottom: 10px;",
        "border: 2px solid #e74c3c;",
        "border-radius: 4px;",
        "color: #e74c3c;",
        "font-weight: 700;",
        "text-align: left;"
      ),
      tags$strong(
        "Metadata edits are pending synchronization."
      )
    )
  })

  invisible(NULL)
  })
}
