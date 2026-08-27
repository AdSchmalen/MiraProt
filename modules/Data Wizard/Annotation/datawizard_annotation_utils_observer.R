# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_utils_observer.R
#
# Purpose:
#   Contains the strategy-mode switching observer and identifier merge
#   execution observer for the Annotation submodule.
#
# Architectural Role:
#   One of the concern-based observer files.  This file covers:
#     - Observer s1:  Strategy dropdown change (mode switching).
#     - Observer m1:  Merge Identifier button (merge execution).
#     - Observer m2:  Reset list button (restore default identifier list).
#
#   Called indirectly: register_annotation_observers() (the thin entrypoint in
#   datawizard_annotation_observer.R) delegates to
#   register_annotation_observers_strategy() defined here.
#
# Integration Points / Dependencies:
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via destructured handles.
#   - Merge helpers from datawizard_annotation_utils_merge.R:
#     get_identifier_columns(), merge_identifiers(), build_merge_col_name(),
#     validate_merge_inputs().
#   - add_annotation_column() from datawizard_annotation_utils.R.
#   - data_def reactive for metadata access.
#
# Maintenance Guidance:
#   - Keep this file focused on strategy/mode switching and merge execution.
#   - Target: stay below 500 lines.
# ==============================================================================


#' Register strategy-mode and merge annotation observers.
#'
#' @param input,output,session,ns  Shiny module objects.
#' @param state              Named list from create_annotation_state().
#' @param get_data           Function returning the current data frame.
#' @param set_data           Function to write back a modified data frame.
#' @param data_def           Reactive returning the metadata data frame.
#' @param debug_log          Logging function.
#' @param DEBUG_LEVEL        Numeric debug verbosity.
register_annotation_observers_strategy <- function(input, output, session, ns,
                                                    state, get_data, set_data,
                                                    data_def,
                                                    debug_log, DEBUG_LEVEL) {

  # -- Destructure state handles -----------------------------------------------
  last_mapping_result       <- state$last_mapping_result
  merge_identifier_list     <- state$merge_identifier_list
  merge_default_identifiers <- state$merge_default_identifiers

  # --------------------------------------------------------------------------
  # s1. Strategy dropdown change observer
  #     Switches the UI between Annotation Hub, BioMart, and Merge modes.
  #     Replaces the former cross-species checkbox toggle logic.
  # --------------------------------------------------------------------------

  observeEvent(input$annotation_strategy, {
    strategy <- input$annotation_strategy
    debug_log(sprintf("Strategy dropdown changed to: '%s'", strategy), 1)

    tryCatch({
      if (identical(strategy, "merge")) {
        # -- Entering Identifier Merging mode --

        # Hide mapping-specific controls
        shinyjs::hide("source_column_panel")
        shinyjs::hide("species_annotation")
        shinyjs::hide("refresh_cache_annotation")
        shinyjs::hide("update_organisms_annotation")
        shinyjs::hide("from_keytype_annotation")

        shinyjs::hide("to_keytype_annotation")
        shinyjs::hide("target_species_panel")
        shinyjs::hide("collapse_strategy_annotation")
        shinyjs::hide("ambiguous_mapping_info")
        shinyjs::hide("run_annotation")

        # Update info box to show only the merge description
        shinyjs::hide("info_annothub")
        shinyjs::hide("info_biomart")
        shinyjs::show("info_merge")

        # Show merge controls
        shinyjs::show("merge_controls_panel")

        # Populate identifier list from metadata
        tryCatch({
          meta <- data_def()
          if (!is.null(meta)) {
            id_cols <- get_identifier_columns(meta, debug_log = debug_log)
            merge_default_identifiers(id_cols)
            merge_identifier_list(id_cols)
            debug_log(sprintf("Merge mode: populated %d identifier column(s)", length(id_cols)), 1)
          } else {
            merge_default_identifiers(character(0))
            merge_identifier_list(character(0))
            debug_log("Merge mode: no metadata available", 1)
          }
        }, error = function(e) {
          debug_log(sprintf("Error loading identifier columns: %s", e$message), 1)
          merge_default_identifiers(character(0))
          merge_identifier_list(character(0))
        })

      } else {
        # -- Leaving Identifier Merging mode (or switching between mapping modes) --

        # Show mapping-specific controls
        shinyjs::show("source_column_panel")
        shinyjs::show("species_annotation")
        shinyjs::show("refresh_cache_annotation")
        shinyjs::show("update_organisms_annotation")
        shinyjs::show("from_keytype_annotation")

        shinyjs::show("to_keytype_annotation")
        shinyjs::show("collapse_strategy_annotation")
        shinyjs::show("ambiguous_mapping_info")
        shinyjs::show("run_annotation")

        # Update info box to show only the active mode's description
        shinyjs::hide("info_merge")
        if (identical(strategy, "biomart")) {
          shinyjs::hide("info_annothub")
          shinyjs::show("info_biomart")
          shinyjs::show("target_species_panel")
        } else {
          shinyjs::show("info_annothub")
          shinyjs::hide("info_biomart")
          shinyjs::hide("target_species_panel")
        }

        # Hide merge controls
        shinyjs::hide("merge_controls_panel")

        # Clear stale merge state
        merge_identifier_list(character(0))
        merge_default_identifiers(character(0))
      }

    }, error = function(e) {
      debug_log(sprintf("Error in strategy mode switch: %s", e$message), 1)
      showNotification(
        paste("Error switching annotation strategy:", e$message),
        type = "error", duration = 5
      )
    })
  }, ignoreInit = TRUE)


  # --------------------------------------------------------------------------
  # Merge identifier list render output
  # Renders the draggable/removable list of identifier columns.
  # --------------------------------------------------------------------------

  output$merge_identifier_list_ui <- renderUI({
    id_cols <- merge_identifier_list()

    if (is.null(id_cols) || length(id_cols) == 0) {
      return(div(
        class = "alert alert-warning",
        style = "margin-top: 10px;",
        "No identifier columns found in the current metadata. ",
        "Ensure that columns are tagged as 'Identifier' in the metadata table."
      ))
    }

    # Build sortable list items.  Each item carries a `data-rank-id`
    # attribute read by sortable_js_capture_input() to communicate the
    # reordered column names back to Shiny.
    item_tags <- lapply(seq_along(id_cols), function(i) {
      col_name <- id_cols[i]
      div(
        `data-rank-id` = col_name,
        class = "merge-id-item",
        `data-col` = col_name,
        style = paste0(
          "display: flex; align-items: center; justify-content: space-between; ",
          "padding: 8px 12px; margin-bottom: 4px; ",
          "background-color: #f5f5f5; border: 1px solid #ddd; ",
          "border-radius: 4px; cursor: grab;"
        ),
        span(
          style = "flex: 1; font-size: 14px;",
          col_name
        ),
        tags$button(
          type = "button",
          class = "btn btn-danger btn-xs",
          style = "margin-left: 10px; min-width: 28px; padding: 2px 6px;",
          onclick = paste0(
            "Shiny.setInputValue(",
            jsonlite::toJSON(ns("merge_remove_clicked"), auto_unbox = TRUE),
            ", ",
            jsonlite::toJSON(col_name, auto_unbox = TRUE),
            ", {priority: 'event'});"
          ),
          "x"
        )
      )
    })

    # Wrap items in a container with a known ID.
    # sortable_js() attaches drag-and-drop behavior to this container
    # and sends the reordered data-rank-id values to the Shiny input
    # merge_id_order whenever the user completes a drag.
    list_id <- ns("merge_sortable_list")

    tagList(
      div(
        id = list_id,
        style = "min-height: 40px;",
        item_tags
      ),
      sortable::sortable_js(
        css_id = list_id,
        options = sortable::sortable_options(
          animation  = 150,
          ghostClass = "sortable-ghost",
          onSort     = sortable::sortable_js_capture_input(
            input_id = ns("merge_id_order")
          )
        )
      )
    )
  })


  # --------------------------------------------------------------------------
  # Remove identifier from merge list
  # Uses a single input carrying the clicked identifier name.
  # --------------------------------------------------------------------------

  observeEvent(input$merge_remove_clicked, {
    remove_col <- input$merge_remove_clicked
    if (is.null(remove_col) || !nzchar(remove_col)) return()

    current <- isolate(merge_identifier_list())
    if (is.null(current) || length(current) == 0) return()

    remove_idx <- match(remove_col, current)
    if (is.na(remove_idx)) return()

    updated <- current[-remove_idx]
    merge_identifier_list(updated)
    debug_log(sprintf("Merge list: removed '%s' at position %d, %d remaining",
                      remove_col, remove_idx, length(updated)), 2)
  }, ignoreInit = TRUE)


  # --------------------------------------------------------------------------
  # Drag-and-drop reorder support
  # Uses Shiny.setInputValue from client-side JS to communicate the new
  # order after a drag event.  The sortable JS is injected in the UI.
  # --------------------------------------------------------------------------

  observeEvent(input$merge_id_order, {
    new_order <- input$merge_id_order
    if (is.null(new_order) || length(new_order) == 0) return()

    current <- isolate(merge_identifier_list())
    if (length(new_order) != length(current)) {
      debug_log(sprintf("Merge reorder: length mismatch (order=%d, list=%d) -- ignoring",
                        length(new_order), length(current)), 1)
      return()
    }

    # new_order contains the column names in the new drag-and-drop order
    valid <- all(new_order %in% current)
    if (!valid) {
      debug_log("Merge reorder: invalid column names in new order -- ignoring", 1)
      return()
    }

    merge_identifier_list(new_order)
    debug_log(sprintf("Merge list reordered: %s", paste(new_order, collapse = " > ")), 2)
  }, ignoreInit = TRUE)


  # --------------------------------------------------------------------------
  # m2. Reset list button
  #     Restores the identifier list to its original default order.
  # --------------------------------------------------------------------------

  observeEvent(input$merge_reset_list, {
    defaults <- merge_default_identifiers()
    merge_identifier_list(defaults)
    debug_log(sprintf("Merge list reset to %d default identifier(s)", length(defaults)), 1)
    showNotification("Identifier list reset to default.", type = "message", duration = 2)
  })


  # --------------------------------------------------------------------------
  # m1. Merge Identifier button observer
  #     Validates inputs, performs row-wise merge, writes back via set_data().
  # --------------------------------------------------------------------------

  observeEvent(input$merge_run, {
    debug_log("Merge Identifier button clicked", 1)

    tryCatch({
      data <- tryCatch(get_data(), error = function(e) NULL)
      req(!is.null(data))

      id_columns <- merge_identifier_list()
      merge_mode <- input$merge_behavior

      # Validate merge mode
      valid_modes <- c("first_non_empty", "concatenate_all")
      if (is.null(merge_mode) || !merge_mode %in% valid_modes) {
        showNotification("Please select a valid merge behavior.", type = "error")
        return(NULL)
      }

      # Validate
      validation <- validate_merge_inputs(data, id_columns, merge_mode,
                                          debug_log = debug_log)
      if (!isTRUE(validation$valid)) {
        showNotification(validation$error, type = "error", duration = 5)
        return(NULL)
      }

      # Inform user when only one column is selected (still works, but no
      # actual merging of multiple sources occurs)
      if (length(id_columns) == 1L) {
        debug_log(sprintf("Merge: only 1 column selected ('%s'), values will be copied as-is",
                          id_columns), 1)
      }

      # Perform merge
      withProgress(message = "Merging identifiers...", value = 0.2, {

        merge_result <- merge_identifiers(
          data       = data,
          id_columns = id_columns,
          merge_mode = merge_mode,
          debug_log  = debug_log
        )

        setProgress(0.7, detail = "Adding merged column...")

        # Build column name and handle collisions
        target_col_name <- build_merge_col_name(id_columns, merge_mode)
        actual_col_name <- target_col_name
        if (target_col_name %in% names(data)) {
          candidate_names <- make.unique(c(names(data), target_col_name))
          actual_col_name <- candidate_names[length(candidate_names)]
          debug_log(sprintf("Merge column '%s' exists, using '%s'",
                            target_col_name, actual_col_name), 1)
        }

        data[[actual_col_name]] <- merge_result$values

        setProgress(0.9, detail = "Saving data...")

        success <- tryCatch({
          set_data(data)
        }, error = function(e) {
          debug_log(paste("set_data() failed after merge:", e$message), 1)
          showNotification(paste("Error updating data:", e$message), type = "error")
          FALSE
        })

        if (isTRUE(success)) {
          # Build methods-section text for the merge report
          methods_text <- build_merge_methods_text(
            id_columns   = id_columns,
            merge_mode   = merge_mode,
            new_col_name = actual_col_name,
            n_total      = nrow(data),
            n_merged     = merge_result$n_merged,
            n_empty      = merge_result$n_empty
          )

          # Store result summary for status display
          last_mapping_result(list(
            source_col     = paste(id_columns, collapse = ", "),
            new_col        = actual_col_name,
            from_keytype   = "Identifier Merge",
            to_keytype     = if (identical(merge_mode, "first_non_empty"))
                               "First non-empty" else "Concatenate all",
            cross_species  = FALSE,
            merge_mode     = TRUE,
            strategy       = merge_mode,
            n_total        = nrow(data),
            n_mapped       = merge_result$n_merged,
            n_unmapped     = merge_result$n_empty,
            methods_text   = methods_text,
            timestamp      = Sys.time()
          ))

          debug_log(sprintf(
            "Merge complete: column '%s' added, %d/%d rows with values, %d empty",
            actual_col_name, merge_result$n_merged, nrow(data), merge_result$n_empty), 1)

          debug_log(
            sprintf(
              paste0(
                "Identifier merge summary",
                " | Mode: Identifier merging",
                " | Merge behavior: %s",
                " | Selected identifier columns (%d): [%s]",
                " | New column created: %s",
                " | Rows total: %d",
                " | Rows merged: %d",
                " | Rows empty after merge: %d"
              ),
              merge_mode,
              length(id_columns),
              if (length(id_columns) == 0) "none" else paste(id_columns, collapse = ", "),
              actual_col_name,
              nrow(data),
              merge_result$n_merged,
              merge_result$n_empty
            ),
            level = 0
          )

          showNotification(
            sprintf("Column '%s' added: %d/%d rows merged, %d empty.",
                    actual_col_name, merge_result$n_merged, nrow(data),
                    merge_result$n_empty),
            type = "message", duration = 5
          )
        } else {
          showNotification("Failed to apply merge results.", type = "error")
        }

        setProgress(1, detail = "Done")
      })

    }, error = function(e) {
      debug_log(paste("Error in merge handler:", e$message), 1)
      showNotification(paste("Error during identifier merge:", e$message),
                       type = "error", duration = 8)
    })
  })
}
