# Observer registration group extracted from volcano_observers.R.
# Reactive state and plot objects are supplied by the module entry point.

register_volcano_data_choice_observers <- function(
    input, output, session, rv,
    res_GSEA, GO_res, module_outputs,
    volcano_state, plot_update_trigger,
    selected_data_Volcano, selected_protein_vector_Volcano,
    volcano_original_plots, volcano_labels, protein_label_settings,
    selected_points_interactive_Volcano,
    data_in, data_def_in, debug_log, ns, modEnv,
    compute_data_signature, has_dataset_mismatch_with_existing_plots
) {
  # ============================================================================
  # SECTION 1: Data Validation and UI Updates
  # ============================================================================

  resolve_plot_selection <- function(available_titles) {
    if (is.null(available_titles) || length(available_titles) == 0) return("none")

    preferred <- volcano_state$preferred_plot_title
    pending <- NULL
    if (is.list(volcano_state$pending_ui_inputs)) {
      pending <- volcano_state$pending_ui_inputs[["PlotSelect_Volcano"]]
    }
    current <- isolate(input$PlotSelect_Volcano)

    for (candidate in c(preferred, pending, current)) {
      if (is.character(candidate) && length(candidate) == 1L && nzchar(candidate) &&
          candidate %in% available_titles) {
        return(candidate)
      }
    }

    available_titles[1]
  }

  volcano_plots_created <- function() {
    isTRUE((input$update_Volcano %||% 0) > 0) ||
      (!is.null(volcano_state$static_plots) && length(volcano_state$static_plots) > 0)
  }

  reset_preplot_skip_log_flags <- function() {
    volcano_state$preplot_skip_log_signature <- NULL
    volcano_state$preplot_metadata_skip_logged <- FALSE
    volcano_state$preplot_data_skip_logged <- FALSE
    invisible(NULL)
  }

  log_preplot_skip_once <- function(kind, message) {
    current_sig <- compute_data_signature(data_in(), data_def_in())
    previous_sig <- volcano_state$preplot_skip_log_signature %||% NA_character_
    if (!identical(previous_sig, current_sig)) {
      volcano_state$preplot_skip_log_signature <- current_sig
      volcano_state$preplot_metadata_skip_logged <- FALSE
      volcano_state$preplot_data_skip_logged <- FALSE
    }

    flag_name <- paste0("preplot_", kind, "_skip_logged")
    if (!isTRUE(volcano_state[[flag_name]])) {
      debug_log(message, level = 2)
      volcano_state[[flag_name]] <- TRUE
    }
    invisible(NULL)
  }

  get_central_identifier_choices <- function(data_def) {
    central_choices <- tryCatch(rv$datawizard_identifier_choices, error = function(e) NULL)
    if (!is.null(central_choices) && length(central_choices) > 0) {
      return(central_choices)
    }

    if (exists("create_datawizard_identifier_choices", mode = "function")) {
      return(create_datawizard_identifier_choices(data_def))
    }

    if (!is.data.frame(data_def) || !all(c("Content", "Column", "Options") %in% names(data_def))) {
      return(stats::setNames(character(0), character(0)))
    }

    identifier_rows <- which(data_def$Content == "Identifier")
    identifier_column_names <- data_def$Column[identifier_rows]
    identifier_labels <- data_def$Options[identifier_rows]
    valid_labels <- !is.na(identifier_labels) & nzchar(trimws(identifier_labels))
    identifier_options <- identifier_column_names[valid_labels]
    names(identifier_options) <- identifier_labels[valid_labels]
    identifier_options
  }

  update_identifier_choices <- function(data_def = data_def_in()) {
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log("Metadata assignment pending; deferring Volcano identifier choices", 2)
      return(invisible(FALSE))
    }
    identifier_options <- get_central_identifier_choices(data_def)
    if (length(identifier_options) == 0) {
      debug_log("No identifier choices available for Volcano identifier dropdown", 2)
      return(invisible(FALSE))
    }

    current_identifier <- isolate(input$Identifier_Volcano)
    selected_identifier <- if (is.character(current_identifier) &&
                               length(current_identifier) == 1L &&
                               current_identifier %in% unname(identifier_options)) {
      current_identifier
    } else {
      unname(identifier_options)[1]
    }

    updateSelectInput(session, "Identifier_Volcano",
                      choices = identifier_options,
                      selected = selected_identifier)

    debug_log(paste("Updated identifier choices:", length(identifier_options), "options"), 2)
    invisible(TRUE)
  }

  derive_plot_titles_from_matching <- function(data_def) {
    tryCatch({
      if (!is.data.frame(data_def) || nrow(data_def) == 0) return(character(0))
      if (!all(c("Content", "Column") %in% names(data_def))) return(character(0))

      pval_pref <- isolate(input$pValueSel_Volcano)
      if (!is.character(pval_pref) || length(pval_pref) != 1L || !nzchar(pval_pref)) {
        pval_pref <- "Adjusted p-value"
      }

      pairing_result <- find_ratio_pvalue_pairs_smart(data_def, pval_pref, debug_log)
      pairs <- pairing_result$pairs
      if (is.null(pairs) || length(pairs) == 0) return(character(0))

      titles <- vapply(pairs, function(pair) {
        tryCatch(generate_plot_title_from_pair(pair), error = function(e) "")
      }, character(1))

      titles <- titles[nzchar(titles)]
      unique(as.character(titles))
    }, error = function(e) {
      debug_log(paste("Failed to derive plot titles from matching:", e$message), 1)
      character(0)
    })
  }

  update_ui_choices <- function() {
    tryCatch({
      data_def <- data_def_in()
      if (!is.data.frame(data_def) || nrow(data_def) == 0) {
        updateSelectInput(session, "PlotSelect_Volcano",
                          choices = list("No plots available" = "none"),
                          selected = "none")
        debug_log("UI choices update skipped: metadata missing or empty", 1)
        return(invisible(NULL))
      }

      update_identifier_choices(data_def)

      # Update plot selection choices
      if (!is.null(volcano_state$plot_titles) && length(volcano_state$plot_titles) > 0) {
        plot_choices <- volcano_state$plot_titles
        names(plot_choices) <- volcano_state$plot_titles
        selected_title <- resolve_plot_selection(volcano_state$plot_titles)

        updateSelectInput(session, "PlotSelect_Volcano",
                          choices = plot_choices,
                          selected = selected_title)
        volcano_state$preferred_plot_title <- selected_title

        debug_log(paste("Updated plot choices:", length(plot_choices), "plots (selected:", selected_title, ")"), 2)
      } else {
        updateSelectInput(session, "PlotSelect_Volcano",
                          choices = list("No plots available" = "none"),
                          selected = "none")
      }

      debug_log("UI choices updated", 2)
    }, error = function(e) {
      debug_log(paste("UI choices update failed safely:", e$message), 1)
      updateSelectInput(session, "PlotSelect_Volcano",
                        choices = list("No plots available" = "none"),
                        selected = "none")
    })
  }

  observeEvent(data_def_in(), {
    req(data_def_in())
    update_identifier_choices(data_def_in())
    if (!isTRUE(volcano_plots_created())) {
      log_preplot_skip_once(
        "metadata",
        "Metadata changed before Create Plot; updated identifiers only and skipped volcano pairing"
      )
      return()
    }
    if (isTRUE(has_dataset_mismatch_with_existing_plots())) {
      debug_log("Metadata changed on new dataset; keeping existing volcano plots until Create Plot is pressed", 1)
      return()
    }
    volcano_state$plot_titles <- derive_plot_titles_from_matching(data_def_in())
    debug_log(paste("Metadata changed - generated", length(volcano_state$plot_titles), "plot titles"), 1)

    debug_log("Data definition changed - updating UI choices", 2)
    update_ui_choices()
  })

  observeEvent(data_in(), {
    req(data_in(), data_def_in())
    update_identifier_choices(data_def_in())
    if (!isTRUE(volcano_plots_created())) {
      log_preplot_skip_once(
        "data",
        "Data changed before Create Plot; updated identifiers only and skipped volcano pairing"
      )
      return()
    }
    if (isTRUE(has_dataset_mismatch_with_existing_plots())) {
      debug_log("Data changed on new dataset; keeping existing volcano plots until Create Plot is pressed", 1)
      return()
    }
    volcano_state$plot_titles <- derive_plot_titles_from_matching(data_def_in())
    debug_log(paste("Generated", length(volcano_state$plot_titles), "plot titles"), 1)

    debug_log("Data changed - updating UI choices", 2)
    update_ui_choices()
  }, priority = -1)

  observeEvent(input$pValueSel_Volcano, {
    req(data_def_in())
    if (!isTRUE(volcano_plots_created())) {
      debug_log("P-value mode changed before Create Plot; skipped volcano pairing", 1)
      return()
    }
    if (isTRUE(has_dataset_mismatch_with_existing_plots())) {
      debug_log("Skipping p-value mode auto-refresh on new dataset; waiting for explicit Create Plot", 1)
      return()
    }
    volcano_state$plot_titles <- derive_plot_titles_from_matching(data_def_in())
    debug_log(paste("P-value mode changed - generated", length(volcano_state$plot_titles), "plot titles"), 1)
    update_ui_choices()
  }, ignoreInit = FALSE)

  # ============================================================================
  # SECTION 2: Master Controls
  # ============================================================================

  observeEvent(input$masterLabelColor_Volcano, {
    tryCatch({
      selected_proteins <- selected_protein_vector_Volcano()
      master_color <- input$masterLabelColor_Volcano

      if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_color)) return()

      debug_log(paste("Updating all label colors to master color:", master_color), 1)
      for (i in seq_along(selected_proteins)) {
        updateColourInput(session, paste0("labelColor_Volcano_", i), value = master_color)
      }
      debug_log(paste("Updated", length(selected_proteins), "label colors"), 2)
    }, error = function(e) {
      debug_log(paste("Error updating master label color:", e$message), 1)
    })
  })

  observeEvent(input$masterDotColor_Volcano, {
    tryCatch({
      selected_proteins <- selected_protein_vector_Volcano()
      master_color <- input$masterDotColor_Volcano

      if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_color)) return()

      debug_log(paste("Updating all dot colors to master color:", master_color), 1)
      for (i in seq_along(selected_proteins)) {
        updateColourInput(session, paste0("dotColor_Volcano_", i), value = master_color)
      }
      debug_log(paste("Updated", length(selected_proteins), "dot colors"), 2)
    }, error = function(e) {
      debug_log(paste("Error updating master dot color:", e$message), 1)
    })
  })

  observeEvent(input$masterCustomDot_Volcano, {
    tryCatch({
      selected_proteins <- selected_protein_vector_Volcano()
      master_enabled <- input$masterCustomDot_Volcano

      if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_enabled)) return()

      debug_log(paste("Setting all custom dot color checkboxes to:", master_enabled), 1)
      for (i in seq_along(selected_proteins)) {
        updateCheckboxInput(session, paste0("useDotColor_Volcano_", i), value = master_enabled)
      }
      debug_log(paste("Updated", length(selected_proteins), "custom dot color checkboxes"), 2)
    }, error = function(e) {
      debug_log(paste("Error updating master custom dot checkbox:", e$message), 1)
    })
  })

  list(
    resolve_plot_selection = resolve_plot_selection,
    reset_preplot_skip_log_flags = reset_preplot_skip_log_flags,
    update_identifier_choices = update_identifier_choices
  )
}
