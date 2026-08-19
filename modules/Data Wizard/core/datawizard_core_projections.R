# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_projections.R
# Purpose:
#   Provide the core projections portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard metadata projection helpers

#' Build a compact signature for metadata fields that affect content-driven UI.
#'
#' This intentionally ignores metadata fields such as Transformation
#' when they are irrelevant for consumers that only need content-based choices or
#' table coloring. Consumers can observe this signature instead of the full
#' metadata table to avoid unnecessary invalidations.
create_metadata_content_signature_dw <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    return("")
  }

  get_field <- function(field) {
    if (field %in% names(metadata)) {
      as.character(metadata[[field]])
    } else {
      rep(NA_character_, nrow(metadata))
    }
  }

  paste(
    paste(get_field("Column"), get_field("Content"), get_field("Options"), sep = "\002"),
    collapse = "\001"
  )
}

#' Extract central identifier-column choices from Data Wizard metadata.
#'
#' Downstream analysis modules use the same identifier dropdown semantics: display
#' the metadata Options label while storing the underlying data column name. This
#' helper keeps that projection in one place so modules do not need to re-scan
#' the full metadata table on every render.
create_datawizard_identifier_choices <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  required_fields <- c("Content", "Column", "Options")
  if (!all(required_fields %in% names(metadata))) {
    return(stats::setNames(character(0), character(0)))
  }

  identifier_rows <- which(trimws(as.character(metadata$Content)) == "Identifier")
  if (length(identifier_rows) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  columns <- as.character(metadata$Column[identifier_rows])
  labels <- as.character(metadata$Options[identifier_rows])
  valid <- !is.na(columns) & nzchar(trimws(columns)) &
    !is.na(labels) & nzchar(trimws(labels))

  if (!any(valid)) {
    return(stats::setNames(character(0), character(0)))
  }

  choices <- columns[valid]
  names(choices) <- labels[valid]
  choices[!duplicated(choices)]
}
