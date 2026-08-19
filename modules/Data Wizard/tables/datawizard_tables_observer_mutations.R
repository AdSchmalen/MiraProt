# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_mutations.R
# Purpose:
#   Provide the tables observer mutations portion of the Data Wizard without changing public behavior.
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

# Primary and additional table deletion observer family.
#
# This registration function is intentionally a mechanical extraction from the
# rendering observer. Keep the four registrations together and in their original
# order so primary authority, callback publication, metadata alignment, and
# revision signalling retain their established contracts.
register_tables_mutation_observers <- function(
    context,
    get_current_primary_df,
    get_current_additional_df,
    set_current_additional_df,
    refresh_server_table,
    record_primary_table_modification,
    mark_programmatic_metadata_sync,
    primary_table_output_id,
    additional_table_output_id) {
  input <- context$input
  rv <- context$rv
  primary_data <- context$primary_data
  set_primary_data <- context$set_primary_data
  set_metadata <- context$set_metadata
  debug_log <- context$debug_log
  current_handson_metadata <- context$current_handson_metadata
  suppress_next_final_metadata_sync <- context$suppress_next_final_metadata_sync
  metadata_write_back_guard <- context$metadata_write_back_guard
  primary_remove_in_progress <- context$primary_remove_in_progress
  additional_remove_in_progress <- context$additional_remove_in_progress

  observeEvent(input$primary_remove_row_btn, {
    if (isTRUE(primary_remove_in_progress())) return()
    primary_remove_in_progress(TRUE)
    on.exit(primary_remove_in_progress(FALSE), add = TRUE)

    current_primary <- get_current_primary_df()
    selection <- isolate(input$primary_selected_row_identity)
    identities <- if (is.list(selection) &&
      identical(selection$table_id, isolate(primary_table_output_id()))) selection$identities else NULL
    selected_rows_id <- paste0(isolate(primary_table_output_id()), "_rows_selected")
    selected_positions <- isolate(input[[selected_rows_id]])
    idx <- tryCatch(
      resolve_canonical_selected_positions(current_primary, selected_positions, identities),
      error = function(e) integer(0)
    )
    if (length(idx) == 0L) {
      showNotification("Selected rows could not be uniquely identified.", type = "warning", duration = 3)
      return()
    }

    row_identities <- if ("Row Index" %in% names(current_primary)) {
      as.character(current_primary[["Row Index"]][idx])
    } else {
      as.character(idx)
    }

    identity_summary <- paste(
      row_identities,
      collapse = ", "
    )

    updated_primary <- current_primary[-idx, , drop = FALSE]
    updated <- if (is.function(set_primary_data)) {
      tryCatch({
        set_primary_data(updated_primary, operation = "table row removal")
        TRUE
      }, error = function(e) FALSE)
    } else if (!is.null(rv)) {
      rv$data_mod <- updated_primary
      TRUE
    } else {
      FALSE
    }
    if (!isTRUE(updated)) {
      showNotification("Selected rows could not be removed.", type = "warning", duration = 3)
      return()
    }

    refresh_server_table("primary")
    record_primary_table_modification(
      "Primary rows removed",
      paste0("Count=", length(idx), "; Row Index identities=", identity_summary)
    )
    debug_log(
      paste0("Primary data rows removed: count=", length(idx), "; Row Index identities=", identity_summary),
      0
    )
    primary_noun <- if (length(idx) == 1L) "row" else "rows"
    showNotification(paste(length(idx), "primary data", primary_noun, "removed."),
                     type = "message", duration = 2)
  })

  observeEvent(input$primary_remove_col_btn, {
    req(!is.null(rv), !is.null(rv$data_mod))
    col_name <- input$primary_remove_col
    if (identical(col_name, "Row Index")) return()
    if (is.null(col_name) || !nzchar(col_name) || !col_name %in% names(rv$data_mod)) return()
    current_primary_df <- tryCatch(primary_data(), error = function(e) NULL)
    if (is.null(current_primary_df) || !is.data.frame(current_primary_df)) {
      current_primary_df <- tryCatch(rv$data_mod, error = function(e) NULL)
    }
    if (is.null(current_primary_df) || !is.data.frame(current_primary_df)) return()

    col_index <- match(col_name, names(current_primary_df))
    if (is.na(col_index)) return()

    updated_primary <- rv$data_mod[, setdiff(names(rv$data_mod), col_name), drop = FALSE]
    if (is.function(set_primary_data)) {
      tryCatch(
        set_primary_data(updated_primary, operation = "table column removal"),
        error = function(e) set_primary_data(updated_primary)
      )
    } else {
      rv$data_mod <- updated_primary
    }
    debug_log(
      paste0("Primary data column removed: name='", col_name, "'| Column index=", col_index),
      0
    )

    current_meta <- tryCatch(current_handson_metadata(), error = function(e) NULL)
    if (!is.null(current_meta) && nrow(current_meta) > 0) {
      new_meta <- current_meta
      if (nrow(current_meta) >= col_index) {
        new_meta <- current_meta[-col_index, , drop = FALSE]
      } else if ("Column" %in% names(current_meta) && col_name %in% current_meta$Column) {
        matched_row <- match(col_name, current_meta$Column)
        if (!is.na(matched_row)) {
          new_meta <- current_meta[-matched_row, , drop = FALSE]
        }
      }

      mark_programmatic_metadata_sync()
      current_handson_metadata(new_meta)
      if (is.function(set_metadata)) {
        suppress_next_final_metadata_sync(TRUE)
        metadata_write_back_guard$active <- TRUE
        set_metadata(new_meta)
      }
    }

    record_primary_table_modification(
      "Primary column removed",
      paste0("Column name='", col_name, "', Column index=", col_index)
    )

    showNotification(paste("Primary column", col_name, "removed."), type = "message", duration = 2)
  })

  observeEvent(input$additional_remove_row_btn, {
    if (isTRUE(additional_remove_in_progress())) return()
    additional_remove_in_progress(TRUE)
    on.exit(additional_remove_in_progress(FALSE), add = TRUE)

    current_additional <- get_current_additional_df()
    selection <- isolate(input$additional_selected_row_identity)
    identities <- if (is.list(selection) &&
      identical(selection$table_id, isolate(additional_table_output_id()))) selection$identities else NULL
    selected_rows_id <- paste0(isolate(additional_table_output_id()), "_rows_selected")
    selected_positions <- isolate(input[[selected_rows_id]])
    idx <- tryCatch(
      resolve_canonical_selected_positions(current_additional, selected_positions, identities),
      error = function(e) integer(0)
    )
    if (length(idx) == 0L) {
      showNotification("Selected rows could not be uniquely identified.", type = "warning", duration = 3)
      return()
    }

    new_additional <- current_additional[-idx, , drop = FALSE]
    if (!isTRUE(set_current_additional_df(new_additional))) {
      showNotification("Selected rows could not be removed.", type = "warning", duration = 3)
      return()
    }
    refresh_server_table("additional")
    additional_noun <- if (length(idx) == 1L) "row" else "rows"
    showNotification(paste(length(idx), "additional", additional_noun, "removed."),
                     type = "message", duration = 2)
  })

  observeEvent(input$additional_remove_col_btn, {
    current_additional <- get_current_additional_df()
    req(!is.null(current_additional))
    col_name <- input$additional_remove_col
    if (identical(col_name, "Row Index")) return()
    if (is.null(col_name) || !nzchar(col_name) || !col_name %in% names(current_additional)) return()

    original_col_index <- match(col_name, names(current_additional))
    new_additional <- current_additional[, setdiff(names(current_additional), col_name), drop = FALSE]
    set_current_additional_df(new_additional)
    debug_log(
      paste0("Additional data column removed: name='", col_name, "', Column index=", original_col_index),
      0
    )
    showNotification(paste("Additional data column", col_name, "removed."), type = "message", duration = 2)
  })

  invisible(NULL)
}
