# Observer registration group extracted from volcano_observers.R.
# Reactive state and plot objects are supplied by the module entry point.

.run_volcano_restore_finalizer <- function(generation, callback, job_id,
                                            resolve_job, current_generation) {
  .run_session_restore_callback(
    owner = "Volcano", reason = "restore finalizer",
    generation = generation, phase = "finalizer", callback = callback,
    job_metadata = list(job_id = job_id, resolve_job = resolve_job,
                        current_generation = current_generation)
  )
}

register_volcano_selection_restore_observers <- function(
    input, output, session, rv,
    res_GSEA, GO_res, module_outputs,
    volcano_state, plot_update_trigger,
    selected_data_Volcano, selected_protein_vector_Volcano,
    volcano_original_plots, volcano_labels, protein_label_settings,
    selected_points_interactive_Volcano,
    data_in, data_def_in, debug_log, ns, modEnv,
    compute_data_signature, update_identifier_choices,
    calculate_optimal_ranges_for_selected_plot, store_original_plots,
    generateVolcanoPlots_fixed
) {
  # ============================================================================
  # SECTION 11: Interactive Selection
  # ============================================================================

  # Selection display
  output$selected_items_display_Volcano <- renderText({
    selected <- selected_points_interactive_Volcano()
    if (is.null(selected) || nrow(selected) == 0) return("No proteins selected")
    paste("Selected", nrow(selected), "proteins")
  })

  output$selected_items_list_Volcano <- renderText({
    selected <- selected_points_interactive_Volcano()
    if (is.null(selected) || nrow(selected) == 0) return("Select proteins in the plot above to see them here...")
    identifier_col <- if ("ID" %in% colnames(selected)) "ID" else if ("Name" %in% colnames(selected)) "Name" else "identifier"
    paste(selected[[identifier_col]], collapse = "\n")
  })

  # Plotly selection handler
  observeEvent(event_data("plotly_selected", source = "volcano_plot"), {
    debug_log("=== USING CUSTOMDATA FROM PLOTLY ===", 2)

    selection <- event_data("plotly_selected", source = "volcano_plot")

    if (is.null(selection) || !is.data.frame(selection)) {
      debug_log("Selection is NULL or not a data.frame - ignoring event", 2)
      return()
    }

    n_sel <- suppressWarnings(nrow(selection))
    if (is.na(n_sel) || n_sel == 0) {
      debug_log("Selection has zero or NA rows - ignoring event", 2)
      return()
    }

    debug_log(paste("plotly_selected event with", n_sel, "points"), 2)

    selected_identifiers <- c()

    if ("customdata" %in% colnames(selection)) {
      selected_identifiers <- as.character(selection$customdata)
      selected_identifiers <- selected_identifiers[!is.na(selected_identifiers) & nzchar(selected_identifiers)]
      debug_log(paste("Extracted", length(selected_identifiers), "identifiers from customdata"), 2)
    }

    if (length(selected_identifiers) == 0 && "x" %in% colnames(selection) && "y" %in% colnames(selection)) {
      debug_log("Trying x,y coordinate matching as fallback", 2)
      plot_data <- volcano_state$current_plotly_data

      if (!is.null(plot_data)) {
        for (i in seq_len(n_sel)) {
          sel_x <- selection$x[i]
          sel_y <- selection$y[i]
          tolerance <- 1e-12
          matches <- which(abs(plot_data$x - sel_x) < tolerance & abs(plot_data$y - sel_y) < tolerance)

          if (length(matches) > 0 && "identifier" %in% colnames(plot_data)) {
            selected_identifiers <- c(selected_identifiers, plot_data$identifier[matches[1]])
          }
        }
        debug_log(paste("Coordinate matching found", length(selected_identifiers), "identifiers"), 2)
      }
    }

    if (length(selected_identifiers) > 0) {
      selected_data <- data.frame(
        identifier = selected_identifiers, ID = selected_identifiers,
        stringsAsFactors = FALSE
      )

      if ("x" %in% colnames(selection) && "y" %in% colnames(selection)) {
        selected_data$x <- selection$x[seq_len(length(selected_identifiers))]
        selected_data$y <- selection$y[seq_len(length(selected_identifiers))]
      }

      selected_points_interactive_Volcano(selected_data)
      debug_log(paste("SUCCESS: Selected correct proteins:", paste(head(selected_identifiers, 5), collapse = ", ")), 2)
      showNotification(paste("Selected", length(selected_identifiers), "proteins"), type = "message", duration = 2)
    } else {
      debug_log("FAILED: Could not extract any identifiers", 1)
      showNotification("Selection failed - could not identify proteins", type = "error", duration = 3)
    }
  })

  # Static plot brush selection
  observeEvent(input$plot_brush, {
    debug_log("=== COORDINATE DEBUGGING ===", 2)
    if (!is.null(input$plot_brush)) {
      brush <- input$plot_brush
      debug_log(paste("Static plot brush - X:", brush$xmin, "to", brush$xmax, "Y:", brush$ymin, "to", brush$ymax), 2)
    }
  }, priority = 100)

  # Click selection for single points
  observeEvent(event_data("plotly_click", source = "volcano_plot"), {
    debug_log("=== CLICK WITH STORED DATA ===", 2)

    click_data <- event_data("plotly_click", source = "volcano_plot")
    if (is.null(click_data)) return()

    plot_data <- volcano_state$current_plotly_data
    if (is.null(plot_data)) {
      debug_log("No stored plotly data for click", 2)
      return()
    }

    if ("pointNumber" %in% colnames(click_data)) {
      point_number <- click_data$pointNumber[1] + 1
      debug_log(paste("Clicked point number:", point_number), 2)

      if (point_number > 0 && point_number <= nrow(plot_data)) {
        clicked_data <- plot_data[point_number, , drop = FALSE]

        if (!"identifier" %in% colnames(clicked_data)) {
          if ("ID" %in% colnames(clicked_data)) {
            clicked_data$identifier <- clicked_data$ID
          } else {
            clicked_data$identifier <- paste0("Point_", point_number)
          }
        }

        if (!"ID" %in% colnames(clicked_data)) {
          clicked_data$ID <- clicked_data$identifier
        }

        selected_points_interactive_Volcano(clicked_data)
        debug_log(paste("Clicked protein:", clicked_data$identifier[1]), 1)
        showNotification(paste("Selected:", clicked_data$identifier[1]), type = "message", duration = 2)
      }
    }
  })

  # Copy and clear selection
  observeEvent(input$copy_selection_Volcano, {
    tryCatch({
      selected <- selected_points_interactive_Volcano()

      if (is.null(selected) || nrow(selected) == 0) {
        showNotification("No proteins selected", type = "warning", duration = 2)
        return()
      }

      debug_log("Copy selection button clicked", 2)

      identifier_col <- if ("ID" %in% colnames(selected)) "ID"
                         else if ("identifier" %in% colnames(selected)) "identifier"
                         else if ("Name" %in% colnames(selected)) "Name"
                         else colnames(selected)[1]

      identifier <- selected[[identifier_col]]
      identifier <- unique(identifier[!is.na(identifier)])

      if (length(identifier) > 0) {
        clipboard_text <- paste(identifier, collapse = "\n")
        copy_to_clipboard(clipboard_text, debug_log)
        debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 1)
        showNotification(paste("Copied", length(identifier), "identifiers to clipboard"),
                         type = "message", duration = 3)
      } else {
        showNotification("No identifiers found to copy", type = "warning")
      }
    }, error = function(e) {
      debug_log(paste("Error in copy_selection_Volcano:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error")
    })
  })

  observeEvent(input$clear_selection_Volcano, {
    tryCatch({
      selected_points_interactive_Volcano(data.frame())
      debug_log("Cleared volcano interactive selection", 1)
      showNotification("Cleared selection", type = "message", duration = 2)
    }, error = function(e) {
      debug_log(paste("Error clearing volcano selection:", e$message), 1)
    })
  })

  # ==========================================================================
  # Session restore: rebuild plots from schema 2.0 state
  # ==========================================================================
  # Schema 2.0 snapshots persist UI inputs, lightweight plot data, labels, and
  # cache references. They do not persist ggplot/plotly/rendered plot objects.
  # This observer restores UI values after metadata choices are populated,
  # rebuilds plots from saved plot data or resolved cache data, and then selects
  # the previously active plot. Labels are applied by get_current_display_plot().
  observeEvent(rv$session_restore_trigger, {
    restore_generation <- isolate(rv$session_restore_generation %||% NA_integer_)
    register_restore_job <- session$userData$register_restore_job
    resolve_restore_job <- session$userData$resolve_restore_job
    finalizer_job <- if (is.function(register_restore_job)) {
      tryCatch(
        register_restore_job("Volcano", "restore finalizer", "finalizer", 15),
        error = function(e) {
          debug_log(paste("[Volcano] could not register restore finalizer:", e$message), 1)
          NULL
        }
      )
    } else NULL

    tryCatch({
      volcano_state$restore_in_progress <- TRUE

      finalizer_scheduled <- FALSE
      finalize_restore <- function(trigger_render = TRUE) {
        if (isTRUE(finalizer_scheduled)) return(invisible(FALSE))
        finalizer_scheduled <<- TRUE
        callback <- function() {
          .run_volcano_restore_finalizer(
            generation = restore_generation,
            job_id = finalizer_job,
            resolve_job = resolve_restore_job,
            current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_),
            callback = function() {
              current_generation <- isolate(rv$session_restore_generation %||% NA_integer_)
              if (!identical(as.integer(current_generation)[1L],
                             as.integer(restore_generation)[1L])) {
                stop("STALE_VOLCANO_GENERATION")
              }
              volcano_state$restore_in_progress <- FALSE
              volcano_state$pending_ui_inputs <- NULL
              if (isTRUE(trigger_render)) {
                plot_update_trigger(isolate(plot_update_trigger()) + 1L)
              }
              debug_log("[Volcano] session restore guard cleared; rendering re-enabled", 1)
            }
          )
        }
        if (is.function(session$onFlushed)) {
          session$onFlushed(once = TRUE, callback)
        } else {
          callback()
        }
        invisible(TRUE)
      }

      # ------------------------------------------------------------------
      # Phase 1: Push restored UI inputs back into widgets
      # ------------------------------------------------------------------
      update_identifier_choices(data_def_in())
      captured <- volcano_state$pending_ui_inputs
      restored_plot_selection <- NULL
      if (!is.null(captured) && is.list(captured)) {
        numeric_input_ids <- c(
          "maxOverlaps_Volcano", "labelDistance_Volcano",
          "lineThickness_Volcano_Label", "labelSize_Volcano",
          "dotSizeLabeled_Volcano", "plotTitleSize_Volcano",
          "AxisTitleSize_Volcano", "tickSize_Volcano",
          "pvalueInput_Volcano", "AbundanceInput_Volcano",
          "xTick_Volcano", "yTick_Volcano",
          "plotWidthInch_Volcano", "plotHeightInch_Volcano",
          "resolution_DPI", "plotWidth_Volcano", "plotHeight_Volcano"
        )
        text_input_ids <- c("plotTitle_Volcano")
        restored_plot_selection <- captured[["PlotSelect_Volcano"]]
        if (!is.character(restored_plot_selection) || length(restored_plot_selection) != 1L ||
            !nzchar(restored_plot_selection)) {
          restored_plot_selection <- NULL
        }
        session$onFlushed(function() {
          tryCatch({
            for (id in names(captured)) {
              val <- captured[[id]]
              if (is.null(val)) next
              if (is.numeric(val) && length(val) == 2L) {
                # Paired numeric => sliderInput range
                updateSliderInput(session, id, value = val)
              } else if (is.numeric(val) && length(val) == 1L) {
                updateNumericInput(session, id, value = val)
              } else if (is.logical(val)) {
                updateCheckboxInput(session, id, value = val)
              } else if (is.character(val)) {
                if (id %in% numeric_input_ids) {
                  updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
                } else if (id %in% text_input_ids) {
                  updateTextInput(session, id, value = as.character(val)[1])
                } else if (grepl("^#[0-9A-Fa-f]{6,8}$", val) &&
                    requireNamespace("colourpicker", quietly = TRUE)) {
                  tryCatch(
                    colourpicker::updateColourInput(session, id, value = val),
                    error = function(e) updateTextInput(session, id, value = val)
                  )
                } else {
                  updateSelectInput(session, id, selected = val)
                }
              }
            }
            debug_log("[Volcano] session restore: UI inputs synced from captured state", 1)
          }, error = function(e) {
            debug_log(paste("[Volcano] UI input sync failed:", e$message), 1)
          })
        }, once = TRUE)
      }

      # ------------------------------------------------------------------
      # Phase 2: Restore plot display from schema 2.0 cache/request state.
      # Saved ggplot/plotly/rendered objects and ggplot-derived plot data are
      # intentionally ignored; cached data/metadata plus cached UI are the
      # authoritative Volcano restore contract.
      # ------------------------------------------------------------------
      if (!isTRUE(volcano_state$had_static_plots_on_save)) {
        debug_log("[Volcano] session restore: no saved volcano plots, skipping regeneration", 1)
        volcano_state$preferred_plot_title <- NULL
        finalize_restore(trigger_render = FALSE)
        return()
      }

      if (!isTRUE(volcano_state$restore_rebuild_requested)) {
        debug_log("[Volcano] session restore: plot reconstruction was not requested", 2)
        finalize_restore(trigger_render = FALSE)
        return()
      }

      replay_ui <- volcano_state$plot_ui_cache
      effective_input <- if (is.list(replay_ui)) utils::modifyList(reactiveValuesToList(input), replay_ui, keep.null = TRUE) else input
      cached_pair <- volcano_state$restore_plot_data_cache
      cache_valid <- is.list(cached_pair) &&
        inherits(cached_pair$data_mod, "data.frame") &&
        inherits(cached_pair$data_def, "data.frame")
      if (!isTRUE(cache_valid)) {
        live_pair <- list(data_mod = data_in(), data_def = data_def_in())
        live_pair_valid <- inherits(live_pair$data_mod, "data.frame") &&
          inherits(live_pair$data_def, "data.frame") &&
          isTRUE(.module_restore_live_contract_compatible(
            volcano_state$restore_cache_contract, rv
          ))
        if (isTRUE(live_pair_valid)) {
          cached_pair <- live_pair
          cache_valid <- TRUE
          debug_log("[Volcano] session restore: using compatible live data/metadata fallback", 1)
        }
      }
      if (!isTRUE(cache_valid)) {
        debug_log("[Volcano] session restore: resolved cache and compatible live data are unavailable; skipping plot regeneration", 1)
        showNotification("Volcano plot could not be restored because cached plot data is unavailable.", type = "warning", duration = 6)
        volcano_state$had_static_plots_on_save <- FALSE
        volcano_state$restore_rebuild_requested <- FALSE
        finalize_restore(trigger_render = FALSE)
        return()
      }

      data <- cached_pair$data_mod
      data_def <- cached_pair$data_def
      debug_log("[Volcano] session restore: regenerating plots from restored data/cache", 1)
      optimal_settings <- calculate_optimal_ranges(
        data, data_def, debug_log,
        pairs = if (is.list(volcano_state$current_pairs)) volcano_state$current_pairs$pairs else NULL
      )
      volcano_state$static_plots <- generateVolcanoPlots_fixed(
        data = data, data_def = data_def,
        input = effective_input, debug_log = debug_log,
        optimal_settings = optimal_settings
      )
      volcano_state$source_data_signature <- compute_data_signature(data, data_def)

      if (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0) {
        volcano_state$plot_titles <- names(volcano_state$static_plots)
        titles <- volcano_state$plot_titles
        selected_title <- if (!is.null(restored_plot_selection) && restored_plot_selection %in% titles) {
          restored_plot_selection
        } else if (is.character(volcano_state$preferred_plot_title) && volcano_state$preferred_plot_title %in% titles) {
          volcano_state$preferred_plot_title
        } else {
          titles[1]
        }
        updateSelectInput(session, "PlotSelect_Volcano", choices = titles, selected = selected_title)
        volcano_state$preferred_plot_title <- selected_title
        volcano_state$plot_generation_counter <- (volcano_state$plot_generation_counter %||% 0L) + 1L
        store_original_plots()
        # plot_creation_cache is the canonical interaction/save-time pair.
        # The restored aliases are only reconstruction inputs and must not
        # remain as duplicate module state after a successful rebuild.
        volcano_state$restore_plot_data_cache <- NULL
        volcano_state$restore_plot_data_cache_by_title <- NULL
        debug_log("[Volcano] session restore: plots rebuilt successfully; labels will be applied at render time", 1)
      }

      volcano_state$had_static_plots_on_save <- FALSE
      volcano_state$restore_rebuild_requested <- FALSE
      finalize_restore(trigger_render = TRUE)
    }, error = function(e) {
      volcano_state$had_static_plots_on_save <- FALSE
      volcano_state$restore_rebuild_requested <- FALSE
      finalize_restore(trigger_render = FALSE)
      debug_log(paste("[Volcano] session restore plot recovery failed:", e$message), 1)
    })
  }, ignoreInit = TRUE)


}
