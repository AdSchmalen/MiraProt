# =============================================================================
#
# Purpose:
#   Ordered coordinator for Venn observer registration. Shared reactive
#   helpers are established here, then observer groups are registered in the
#   same order in which they historically appeared in this function.
#
# Observer implementations are split across peer files:
#   - venn_observers_data_lists.R
#   - venn_observers_plot_interaction.R
#   - venn_observers_export_restore.R
# Notes for future developers:
#   - Group registration calls below are intentionally ordered. In particular,
#     dynamic UI is registered before session-restore polling.
#   - Cleanup remains delegated to cleanup_manager$register_module().
# =============================================================================


#' Register all observers and outputs for the Venn module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param ns Namespace function
#' @param state Named list of reactive containers from create_venn_state()
#' @param rv Reactive values containing data and metadata from the parent app
#' @param module_outputs Named list of outputs from sibling modules
#' @param debug_log Logging function defined in modVennServer
register_venn_observers <- function(input, output, session, ns, state, rv,
                                    module_outputs, debug_log) {
  cleanup_mgr <- if (exists("cleanup_manager", inherits = TRUE)) cleanup_manager else NULL

  safe_list_value <- function(values, index, default = NULL) {
    if (!is.list(values) || length(values) < index) return(default)
    values[[index]]
  }

  is_default_venn_list_name <- function(value, index) {
    value <- trimws(as.character(value %||% ""))
    !nzchar(value) || value %in% c(paste0("List", index), paste("List", index))
  }

  is_dynamic_venn_ui_visible <- function() {
    identical(session$clientData$output_dynamicLists_Venn_hidden, FALSE)
  }

  has_venn_list_content <- function() {
    list_count <- state$list_count_Venn()
    if (!is.null(list_count) && list_count > 3L) return(TRUE)

    lists <- state$list_data_Venn$lists
    if (any(nzchar(trimws(as.character(unlist(lists, use.names = FALSE) %||% character()))))) {
      return(TRUE)
    }

    names <- state$list_data_Venn$names
    if (is.list(names) && length(names) > 0L) {
      for (i in seq_along(names)) {
        if (!is_default_venn_list_name(names[[i]], i)) return(TRUE)
      }
    }

    FALSE
  }

  selected_venn_source_label <- function(...) {
    selected <- unlist(list(...), use.names = FALSE)
    selected <- unique(trimws(as.character(selected %||% character())))
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0L) return(NULL)
    paste(selected, collapse = ", ")
  }

  has_intersection_dropdown_readiness <- function(intersection_data = NULL) {
    has_intersection_data <- !is.null(intersection_data) && length(intersection_data) > 0L
    venn_plot_created <- isTRUE(state$plot_active()) ||
      !is.null(state$current_venn_plot()) ||
      !is.null(state$restored_plot_cache())
    restore_active <- isTRUE(restore_poll_active())

    has_intersection_data || venn_plot_created || restore_active
  }

  log_intersection_dropdown_not_ready <- local({
    logged <- FALSE
    function() {
      if (!logged) {
        debug_log("Skipping intersection dropdown updates until Venn plot data is ready", 3)
        logged <<- TRUE
      }
    }
  })

  # ---------------------------------------------------------------------------
  # Local helper: snapshot current UI values into state$list_data_Venn
  # ---------------------------------------------------------------------------

  sync_list_data_Venn <- function(list_count) {
    if (is.null(list_count) || list_count <= 0) return()
    for (i in 1:list_count) {
      if (!is.null(input[[paste0("name", i)]])) {
        state$list_data_Venn$names[[i]] <- input[[paste0("name", i)]]
      }
      if (!is.null(input[[paste0("list", i)]])) {
        state$list_data_Venn$lists[[i]] <- input[[paste0("list", i)]]
      }
      if (!is.null(input[[paste0("color", i)]])) {
        state$list_data_Venn$colors[[i]] <- input[[paste0("color", i)]]
      }
      if (!is.null(input[[paste0("GSEA_SELECT_", i)]])) {
        state$list_data_Venn$gsea[[i]] <- input[[paste0("GSEA_SELECT_", i)]]
      }
      if (!is.null(input[[paste0("GO_SELECT_", i)]])) {
        state$list_data_Venn$go[[i]] <- input[[paste0("GO_SELECT_", i)]]
      }
      if (!is.null(input[[paste0("Sample_SELECT_", i)]])) {
        state$list_data_Venn$sample[[i]] <- input[[paste0("Sample_SELECT_", i)]]
      }
      if (!is.null(input[[paste0("CoreEnriched_VENN_", i)]])) {
        state$list_data_Venn$core_enriched[[i]] <- input[[paste0("CoreEnriched_VENN_", i)]]
      }
    }
  }

  restore_poll_active   <- reactiveVal(FALSE)
  restore_poll_attempt  <- reactiveVal(0L)
  restore_poll_captured <- reactiveVal(NULL)
  restore_poll_phase    <- reactiveVal("base")
  restore_phase_attempt <- reactiveVal(0L)
  restore_poll_generation <- reactiveVal(NA_integer_)
  restore_poll_job <- reactiveVal(NULL)
  restore_poll_job_settled <- reactiveVal(TRUE)

  settle_restore_poll <- function(outcome, error = NULL) {
    if (isTRUE(isolate(restore_poll_job_settled()))) return(invisible(FALSE))
    restore_poll_job_settled(TRUE)
    job_id <- isolate(restore_poll_job())
    resolver <- session$userData$resolve_restore_job
    if (is.null(job_id) || !is.function(resolver)) return(invisible(TRUE))
    invisible(tryCatch(
      resolver(job_id, outcome, error),
      error = function(e) {
        debug_log(paste("[Venn] restore settlement failed:", e$message), 1)
        FALSE
      }
    ))
  }

  update_last_plot_dimensions <- function(width = NULL, height = NULL, ppi = NULL, format = NULL) {
    ui <- isolate(state$last_plot_ui_inputs() %||% list())
    if (!is.list(ui)) ui <- list()
    if (is.numeric(width) && length(width) == 1L && is.finite(width)) ui$width_plot_Venn <- width
    if (is.numeric(height) && length(height) == 1L && is.finite(height)) ui$height_plot_Venn <- height
    if (is.numeric(ppi) && length(ppi) == 1L && is.finite(ppi)) ui$ppi_plot_Venn <- ppi
    if (!is.null(format) && nzchar(as.character(format)[1])) ui$format_file_Venn <- as.character(format)[1]
    state$last_plot_ui_inputs(ui)
    invisible(TRUE)
  }

  get_cached_plot_dimension <- function(id, fallback = NULL) {
    restored <- isolate(state$restored_plot_cache())
    if (is.list(restored) && is.list(restored$ui_snapshot) && !is.null(restored$ui_snapshot[[id]])) {
      return(restored$ui_snapshot[[id]])
    }
    pending <- isolate(state$pending_ui_inputs())
    if (is.list(pending) && !is.null(pending[[id]])) return(pending[[id]])
    saved <- isolate(state$last_plot_ui_inputs())
    if (is.list(saved) && !is.null(saved[[id]])) return(saved[[id]])
    fallback
  }

  finalize_restore_report <- function(status, source = NA_character_, reason = NA_character_, telemetry = NULL) {
    report <- list(
      status = status,
      source = source,
      reason = reason,
      phase = isolate(restore_poll_phase()),
      attempts = isolate(restore_poll_attempt()),
      timestamp = Sys.time()
    )
    if (is.list(telemetry) && length(telemetry) > 0L) {
      report <- c(report, telemetry)
    }
    state$last_restore_report(report)
    debug_log(paste0("[Venn] session restore outcome: ", status,
                     " (source=", source, ", reason=", reason, ")"), 1)
  }

  resolve_restore_data_pair <- function(source = "runtime") {
    use_restore_cache <- identical(source, "session_restore") &&
      is.list(state$restore_plot_data_cache())

    if (use_restore_cache) {
      cached <- state$restore_plot_data_cache()
      cached_mod <- cached$data_mod
      cached_def <- cached$data_def
      if (!is.null(cached_mod) && !is.null(cached_def)) {
        debug_log(paste0("[Venn] restore data source: cached pair (data_mod ",
                         NROW(cached_mod), "x", NCOL(cached_mod),
                         ", data_def ", NROW(cached_def), "x", NCOL(cached_def), ")"), 2)
        return(list(data_mod = cached_mod, data_def = cached_def, source = "cached"))
      }
      debug_log("[Venn] restore data source: cached pair missing/incomplete; falling back to live data", 2)
    }

    live_mod <- rv$data_mod
    live_def <- rv$data_def
    debug_log(paste0("[Venn] restore data source: live pair (data_mod ",
                     NROW(live_mod), "x", NCOL(live_mod),
                     ", data_def ", NROW(live_def), "x", NCOL(live_def), ")"), 2)
    list(data_mod = live_mod, data_def = live_def, source = "live")
  }

  # ---------------------------------------------------------------------------
  # Phase 1: button-gated data preparation (eventReactive)
  # Schema 2.0 restore enters here after Data Wizard-driven choices and saved
  # set selections have been replayed; it rebuilds Venn/UpSet data instead of
  # preferring any deserialized plot/grid object from the session payload.
  # ---------------------------------------------------------------------------
  # Runs only when "Create Plot" is clicked. Collects input lists, validates
  # identifiers (Bug 1), builds the binary membership matrix for UpSet types,
  # and pre-computes the merged abundance/ratio data for the expensive plot
  # variants. The resulting cache is consumed by generatePlot_Venn (Phase 2).
  # ---------------------------------------------------------------------------

  compute_venn_cache <- function(source = "runtime", source_mode) {
    if (missing(source_mode) || !source_mode %in% c("cached", "live")) {
      stop("compute_venn_cache() requires source_mode = 'cached' or 'live'.")
    }
    debug_log(paste("Preparing Venn/UpSet data cache (source:", source, ")"), 2)

    resolved_pair <- resolve_restore_data_pair(source)

    if (identical(source_mode, "cached") && !identical(resolved_pair$source, "cached")) {
      debug_log("[Venn] cached source_mode requested but cached metadata pair is unavailable; aborting cache build.", 1)
      return(NULL)
    }

    data_mod <- resolved_pair$data_mod
    data_def <- resolved_pair$data_def
    captured_restore_inputs <- if (identical(source, "session_restore")) {
      state$pending_ui_inputs()
    } else {
      NULL
    }
    snapshot_ui_inputs <- if (identical(source, "session_restore")) {
      state$last_plot_ui_inputs()
    } else {
      NULL
    }
    cached_restore_selector_ids <- c(
      "GeneIdentifierColumn_Venn", "ReferenceValues_Venn",
      "data_abundance_Mean_Venn",
      "data_abundance_ratio_num_Venn",
      "data_abundance_ratio_denom_Venn"
    )
    get_restore_value <- function(id, fallback = NULL, required_cached = TRUE) {
      if (identical(source, "session_restore") &&
          is.list(captured_restore_inputs) &&
          !is.null(captured_restore_inputs[[id]])) {
        return(captured_restore_inputs[[id]])
      }
      if (identical(source, "session_restore") &&
          is.list(snapshot_ui_inputs) &&
          !is.null(snapshot_ui_inputs[[id]])) {
        debug_log(paste0("[Venn] restore field ", id,
                         " resolved from last_plot_ui_inputs snapshot."), 2)
        return(snapshot_ui_inputs[[id]])
      }
      if (identical(source_mode, "cached")) {
        if (id %in% cached_restore_selector_ids) {
          if (isTRUE(required_cached)) {
            debug_log(paste0("[Venn] cached source_mode missing required restored selector field: ",
                             id, "; aborting without reading live input."), 1)
          }
          return(if (isTRUE(required_cached)) NULL else fallback)
        }
        if (!isTRUE(required_cached)) {
          return(fallback)
        }
        if (!is.null(fallback)) {
          debug_log(paste0("[Venn] cached source_mode missing restore field: ", id, "; using module snapshot fallback."), 1)
          return(fallback)
        }
        debug_log(paste0("[Venn] cached source_mode missing required restore field: ", id, "; aborting (no snapshot fallback available)."), 1)
        return(NULL)
      }
      if (identical(source, "session_restore")) {
        return(fallback)
      }
      val <- input[[id]]
      if (!is.null(val)) return(val)
      fallback
    }

    if (identical(source, "session_restore")) {
      inputLists <- list()
      for (i in seq_len(state$list_count_Venn())) {
        list_content <- state$list_data_Venn$lists[[i]]
        if (!is.null(list_content) && nzchar(list_content)) {
          proteins <- trimws(unlist(strsplit(list_content, "\n")))
          proteins <- proteins[nzchar(proteins)]
          if (length(proteins) > 0L) inputLists[[length(inputLists) + 1L]] <- proteins
        }
      }
    } else {
      inputLists <- collect_input_lists(state$list_count_Venn(), input, ns)
    }

    if (length(inputLists) == 0) {
      showNotification("Please enter at least one protein list.", type = "error")
      return(NULL)
    }

    debug_log(paste("Collected", length(inputLists), "input lists"), 2)

    names(inputLists) <- sapply(seq_along(inputLists), function(i) {
      get_restore_value(paste0("name", i), state$list_data_Venn$names[[i]])
    })

    colors <- sapply(seq_along(inputLists), function(i) {
      get_restore_value(paste0("color", i), state$list_data_Venn$colors[[i]])
    })

    if (identical(source_mode, "cached") &&
        (any(is.null(names(inputLists)) | !nzchar(names(inputLists))) ||
         any(is.null(colors) | !nzchar(colors)))) {
      debug_log("[Venn] cached source_mode missing list names or colors; aborting cache build.", 1)
      showNotification(
        "Cached Venn metadata is incomplete (list names/colors missing). Restore halted.",
        type = "warning"
      )
      return(NULL)
    }

    plot_type <- get_restore_value("diagramType_Venn", "Venn") %||% "Venn"
    if (identical(source_mode, "cached") && !nzchar(plot_type)) {
      debug_log("[Venn] cached source_mode missing diagramType_Venn; aborting cache build.", 1)
      return(NULL)
    }

    # Bug 1: validate that user-provided identifiers match the selected column
    # before attempting any data join. Only relevant for plot types that merge
    # against the data (abundance and ratio variants).
    if (plot_type %in% c("UpSet with Abundances", "UpSet with Abundance Ratios") &&
        !is.null(data_mod) && !is.null(data_def)) {
      all_user_proteins <- unique(unlist(inputLists))
      id_col <- get_restore_value(
        "GeneIdentifierColumn_Venn", input$GeneIdentifierColumn_Venn,
        required_cached = TRUE
      )
      if (identical(source_mode, "cached") && (is.null(id_col) || !nzchar(as.character(id_col)[1]))) {
        debug_log("[Venn] cached source_mode missing GeneIdentifierColumn_Venn; aborting before identifier validation.", 1)
        showNotification("Cached abundance metadata is incomplete: missing protein identifier column.", type = "warning")
        return(NULL)
      }
      debug_log(paste0("[Venn] identifier validation using ", resolved_pair$source,
                       " data; id column=", id_col, "; proteins=", length(all_user_proteins),
                       "; data_mod cols=", NCOL(data_mod), "; data_def cols=", NCOL(data_def)), 2)
      if (!validate_identifier_match(all_user_proteins, data_mod,
                                     data_def, id_col)) {
        debug_log("Identifier validation failed: no matching proteins found in selected column", 1)
        showNotification(
          "Couldn't find matching protein identifiers in the selected column. Check the 'Select protein identifier' setting.",
          type = "warning"
        )
        return(NULL)
      }
    }

    upset_data     <- NULL
    prepared_data  <- NULL

    if (plot_type == "Venn") {
      # Compute Venn intersections for the dropdown here (on button press) so
      # that intersection data is only recomputed when the user explicitly
      # requests a new plot, not on every subsequent styling change.
      intersection_data <- list()
      all_combinations  <- unlist(lapply(seq_along(inputLists), function(m) {
        combn(names(inputLists), m, simplify = FALSE)
      }), recursive = FALSE)
      for (comb in all_combinations) {
        proteins_in_comb <- Reduce(intersect, inputLists[comb])
        intersection_data[[paste(comb, collapse = " & ")]] <- proteins_in_comb
      }
      after_filter <- Filter(function(x) length(x) > 0 && any(nzchar(x)),
                             intersection_data)
      state$intersection_list(after_filter)
      debug_log(paste("Venn intersections computed:", length(after_filter)), 2)

    } else {
      # Build binary membership matrix
      all_proteins <- unique(unlist(inputLists))
      matrix_data  <- sapply(inputLists, function(lst) all_proteins %in% lst)
      colnames(matrix_data) <- names(inputLists)
      upset_data           <- as.data.frame(matrix_data)
      upset_data$Protein   <- all_proteins

      # Compute intersections for the dropdown (updates state as side effect)
      intersection_data <- list()
      for (m in seq_along(inputLists)) {
        combs <- combn(names(inputLists), m, simplify = FALSE)
        for (comb in combs) {
          proteins_in_comb <- all_proteins[
            Reduce(`&`, lapply(comb, function(set) upset_data[[set]]))
          ]
          if (length(proteins_in_comb) > 0) {
            intersection_data[[paste(comb, collapse = " & ")]] <- proteins_in_comb
          }
        }
      }
      after_filter <- Filter(function(x) length(x) > 0 && any(nzchar(x)),
                             intersection_data)
      state$intersection_list(after_filter)

      # For abundance / ratio variants: pre-compute the merged data frame so
      # that the expensive join is only executed on button press, not on every
      # subsequent styling change.
      if (plot_type %in% c("UpSet with Abundances", "UpSet with Abundance Ratios")) {
        reference_value <- get_restore_value("ReferenceValues_Venn", input$ReferenceValues_Venn)
        if (identical(source_mode, "cached") && (is.null(reference_value) || !nzchar(reference_value))) {
          debug_log("[Venn] cached source_mode missing ReferenceValues_Venn; aborting cache build.", 1)
          showNotification("Cached abundance metadata is incomplete: missing abundance type.", type = "warning")
          return(NULL)
        }
        if (is.null(reference_value) || !nzchar(reference_value)) {
          showNotification(
            "Please select an abundance type in 'Select abundance type for sample filtering'.",
            type = "error"
          )
          return(NULL)
        }
        data_def_check <- data_def
        if (!any(data_def_check$Content == reference_value)) {
          showNotification(
            paste("Selected abundance type", reference_value, "not found in data."),
            type = "error"
          )
          return(NULL)
        }

        if (plot_type == "UpSet with Abundances") {
          prepared_data <- tryCatch(
            prepare_abundance_data(
              data_mod, data_def,
              get_restore_value("GeneIdentifierColumn_Venn", input$GeneIdentifierColumn_Venn),
              reference_value, get_restore_value("data_abundance_Mean_Venn", input$data_abundance_Mean_Venn),
              upset_data, inputLists, debug_log
            ),
            error = function(e) {
              debug_log(paste("Data preparation error:", e$message), 1)
              showNotification(paste0("Error: ", e$message), type = "error")
              NULL
            }
          )
          if (is.null(prepared_data)) return(NULL)
        } else {
          prepared_data <- tryCatch(
            prepare_ratio_data(
              data_mod, data_def,
              get_restore_value("GeneIdentifierColumn_Venn", input$GeneIdentifierColumn_Venn),
              reference_value,
              get_restore_value("data_abundance_ratio_num_Venn", input$data_abundance_ratio_num_Venn),
              get_restore_value("data_abundance_ratio_denom_Venn", input$data_abundance_ratio_denom_Venn),
              upset_data, inputLists, debug_log
            ),
            error = function(e) {
              debug_log(paste("Data preparation error:", e$message), 1)
              showNotification(paste0("Error: ", e$message), type = "error")
              NULL
            }
          )
          if (is.null(prepared_data)) return(NULL)
        }

        if (!is.null(prepared_data)) {
          canonical_y_name <- "Value"
          source_y_name <- if (plot_type == "UpSet with Abundances") "Abundance" else "Abundance Ratio"
          if (!canonical_y_name %in% names(prepared_data) && source_y_name %in% names(prepared_data)) {
            prepared_data[[canonical_y_name]] <- prepared_data[[source_y_name]]
          }
          if (!canonical_y_name %in% names(prepared_data)) {
            prepared_data[[canonical_y_name]] <- NA_real_
          }
          attr(prepared_data, "source_mode") <- source_mode
          attr(prepared_data, "metadata_source") <- if (identical(source_mode, "cached")) "restore" else "live"

        }
      }
    }

    ui_snapshot <- list(
      diagramType_Venn = plot_type,
      ReferenceValues_Venn = get_restore_value(
        "ReferenceValues_Venn", input$ReferenceValues_Venn,
        required_cached = plot_type %in% c("UpSet with Abundances", "UpSet with Abundance Ratios")
      ),
      GeneIdentifierColumn_Venn = get_restore_value(
        "GeneIdentifierColumn_Venn", input$GeneIdentifierColumn_Venn,
        required_cached = plot_type %in% c("UpSet with Abundances", "UpSet with Abundance Ratios")
      ),
      showPercentages_Venn = get_restore_value("showPercentages_Venn", input$showPercentages_Venn),
      showListTitles_Venn = get_restore_value("showListTitles_Venn", input$showListTitles_Venn),
      overlapNumberSize_Venn = get_restore_value("overlapNumberSize_Venn", input$overlapNumberSize_Venn),
      listTitleSize_Venn = get_restore_value("listTitleSize_Venn", input$listTitleSize_Venn),
      listTitleDistance_Venn = get_restore_value("listTitleDistance_Venn", input$listTitleDistance_Venn),
      catFont_Venn = get_restore_value("catFont_Venn", input$catFont_Venn),
      cat_FontStyle_Venn = get_restore_value("cat_FontStyle_Venn", input$cat_FontStyle_Venn),
      font_family_Venn = get_restore_value("font_family_Venn", input$font_family_Venn),
      fontStyle_Venn = get_restore_value("fontStyle_Venn", input$fontStyle_Venn),
      ThemeSelect_Upset = get_restore_value("ThemeSelect_Upset", input$ThemeSelect_Upset),
      axis_title_size_Venn = get_restore_value("axis_title_size_Venn", input$axis_title_size_Venn),
      axis_text_size_Venn = get_restore_value("axis_text_size_Venn", input$axis_text_size_Venn),
      label_text_size_Venn = get_restore_value("label_text_size_Venn", input$label_text_size_Venn),
      showDotsInBoxplot_Venn = get_restore_value("showDotsInBoxplot_Venn", input$showDotsInBoxplot_Venn),
      data_abundance_Mean_Venn = get_restore_value(
        "data_abundance_Mean_Venn", input$data_abundance_Mean_Venn,
        required_cached = identical(plot_type, "UpSet with Abundances")
      ),
      data_abundance_ratio_num_Venn = get_restore_value(
        "data_abundance_ratio_num_Venn", input$data_abundance_ratio_num_Venn,
        required_cached = identical(plot_type, "UpSet with Abundance Ratios")
      ),
      data_abundance_ratio_denom_Venn = get_restore_value(
        "data_abundance_ratio_denom_Venn", input$data_abundance_ratio_denom_Venn,
        required_cached = identical(plot_type, "UpSet with Abundance Ratios")
      ),
      width_plot_Venn = get_restore_value("width_plot_Venn", input$width_plot_Venn),
      height_plot_Venn = get_restore_value("height_plot_Venn", input$height_plot_Venn),
      ppi_plot_Venn = get_restore_value("ppi_plot_Venn", input$ppi_plot_Venn),
      format_file_Venn = get_restore_value("format_file_Venn", input$format_file_Venn)
    )
    state$last_plot_ui_inputs(ui_snapshot)

    debug_log(paste("Data cache ready for plot type:", plot_type), 1)
    {
      list_summary_l0 <- paste(
        mapply(function(nm, sz) paste0(nm, " - ", sz, " proteins"),
               names(inputLists), sapply(inputLists, length)),
        collapse = " | "
      )
      id_col_l0    <- ui_snapshot$GeneIdentifierColumn_Venn
      plot_type_l0 <- plot_type

      if (plot_type_l0 == "UpSet with Abundances") {
        samples_l0     <- ui_snapshot$data_abundance_Mean_Venn
        samples_str_l0 <- if (length(samples_l0) > 0L) paste(samples_l0, collapse = ", ") else "all"
        debug_log(
          sprintf(
            paste0(
              "Venn/UpSet plot settings",
              " | Diagram type: %s",
              " | %s",
              " | Abundance type: %s",
              " | Identifier: %s",
              " | Samples: %s"
            ),
            plot_type_l0, list_summary_l0,
            ui_snapshot$ReferenceValues_Venn, id_col_l0, samples_str_l0
          ),
          level = 0
        )
      } else if (plot_type_l0 == "UpSet with Abundance Ratios") {
        num_l0       <- ui_snapshot$data_abundance_ratio_num_Venn
        denom_l0     <- ui_snapshot$data_abundance_ratio_denom_Venn
        num_str_l0   <- if (length(num_l0)   > 0L) paste(num_l0,   collapse = ", ") else "none"
        denom_str_l0 <- if (length(denom_l0) > 0L) paste(denom_l0, collapse = ", ") else "none"
        debug_log(
          sprintf(
            paste0(
              "Venn/UpSet plot settings",
              " | Diagram type: %s",
              " | %s",
              " | Abundance type: %s",
              " | Identifier: %s",
              " | Numerator samples: %s",
              " | Denominator samples: %s"
            ),
            plot_type_l0, list_summary_l0,
            ui_snapshot$ReferenceValues_Venn, id_col_l0, num_str_l0, denom_str_l0
          ),
          level = 0
        )
      } else {
        debug_log(
          sprintf(
            paste0(
              "Venn/UpSet plot settings",
              " | Diagram type: %s",
              " | %s",
              " | Identifier: %s"
            ),
            plot_type_l0, list_summary_l0, id_col_l0
          ),
          level = 0
        )
      }
    }
    state$go_results_for_extraction(list(
      type = plot_type,
      input_lists = inputLists,
      prepared_data = prepared_data
    ))

    metadata_source <- if (identical(source, "session_restore")) "restore" else "live"

    list(
      type          = plot_type,
      input_lists   = inputLists,
      colors        = colors,
      upset_data    = upset_data,
      prepared_data = prepared_data,
      cache_source  = source,
      source_mode   = source_mode,
      metadata_source = metadata_source,
      ui_snapshot = ui_snapshot,
      intersection_count = length(state$intersection_list() %||% list())
    )
  }

  venn_data_cache <- eventReactive(input$create_plot_Venn, {
    compute_venn_cache(source = "create_button", source_mode = "live")
  }, ignoreNULL = TRUE)

  # ---------------------------------------------------------------------------
  # Phase 2: styling-reactive plot generation (reactive)
  # ---------------------------------------------------------------------------
  # Re-runs whenever venn_data_cache() changes (new button press) OR whenever
  # a UI styling input changes (theme, font sizes, etc.). Data loading is NOT
  # repeated here; it uses the cache produced by Phase 1.
  # ---------------------------------------------------------------------------

  build_plot_from_cache <- function(cached, source_mode) {
    if (missing(source_mode) || !source_mode %in% c("cached", "live")) {
      stop("build_plot_from_cache() requires source_mode = 'cached' or 'live'.")
    }
    tryCatch({
      inputLists <- cached$input_lists
      plot_type  <- cached$type

      annotation_count <- if (plot_type %in% c("UpSet with Abundances",
                                               "UpSet with Abundance Ratios")) 2L else 1L
      num_intersections <- as.integer(cached$intersection_count %||%
                                        calculate_expected_intersections(length(inputLists)))
      dims <- if (plot_type != "Venn") {
        calculate_upset_plot_dimensions(
          num_intersections = num_intersections,
          num_sets = length(inputLists),
          annotation_count = annotation_count,
          title_size_pt = input$title_text_size_Venn %||% 14
        )
      } else NULL

      debug_log(paste("Plot type:", plot_type,
                      "Lists:", length(inputLists)), 2)
      if (!is.null(dims)) {
        debug_log(paste0("[Venn sizing] plot_type=", plot_type,
                         " num_intersections=", num_intersections,
                         " width=", dims$plot_width_px,
                         " height=", dims$plot_height_px,
                         " cached_mode=", identical(cached$cache_source, "session_restore")), 1)
        if (isTRUE(dims$top_padding_below_safe)) {
          debug_log(paste0("[Venn sizing warning] top padding below safe threshold: ",
                           dims$top_padding_px, "px < ",
                           dims$safe_top_padding_px, "px"), 1)
        }
      }

      if (plot_type == "Venn" && length(inputLists) <= 5) {
        debug_log("Generating Venn diagram", 2)
        create_venn_diagram(inputLists, cached$colors, input, debug_log)

      } else if (plot_type == "UpSet with Abundances") {
        debug_log("Generating UpSet with Abundances (from cache)", 2)
        p <- create_upset_with_abundances(
          cached$upset_data, inputLists, input,
          num_intersections_export = state$num_intersections_export,
          debug_log                = debug_log,
          pre_prepared_data        = cached$prepared_data,
          source_mode              = source_mode,
          metadata_source          = cached$metadata_source %||% "live",
          show_dots_override         = if (identical(cached$cache_source, "session_restore") && isTRUE(restore_poll_active())) cached$ui_snapshot$showDotsInBoxplot_Venn %||% NULL else NULL
        )
        p + theme(
          plot.margin = margin(t = dims$top_padding_px, r = 35, b = 20, l = 25, unit = "pt"),
          plot.title = element_text(margin = margin(b = 12))
        )

      } else if (plot_type == "UpSet with Abundance Ratios") {
        debug_log("Generating UpSet with Abundance Ratios (from cache)", 2)
        p <- create_upset_with_ratios(
          cached$upset_data, inputLists, input,
          num_intersections_export = state$num_intersections_export,
          debug_log                = debug_log,
          pre_prepared_data        = cached$prepared_data,
          source_mode              = source_mode,
          metadata_source          = cached$metadata_source %||% "live",
          show_dots_override         = if (identical(cached$cache_source, "session_restore") && isTRUE(restore_poll_active())) cached$ui_snapshot$showDotsInBoxplot_Venn %||% NULL else NULL
        )
        p + theme(
          plot.margin = margin(t = dims$top_padding_px, r = 35, b = 20, l = 25, unit = "pt"),
          plot.title = element_text(margin = margin(b = 12))
        )

      } else {
        debug_log("Generating standard UpSet plot", 2)
        p <- create_standard_upset(cached$upset_data, inputLists, input,
                                   state$num_intersections_export, debug_log)
        p + theme(
          plot.margin = margin(t = dims$top_padding_px, r = 35, b = 20, l = 25, unit = "pt"),
          plot.title = element_text(margin = margin(b = 12))
        )
      }
    }, error = function(e) {
      debug_log(paste("Plot generation error:", e$message), 1)
      showNotification(paste0("Error: ", e$message), type = "error")
      NULL
    })
  }

  generatePlot_Venn <- reactive({
    cached <- state$restored_plot_cache() %||% venn_data_cache()
    if (is.null(cached)) {
      # During restore race windows, keep last valid plot visible instead of
      # throwing a silent req() error that blanks the plot output.
      return(state$current_venn_plot())
    }
    build_plot_from_cache(cached, source_mode = cached$source_mode %||% "live")
  })


  # Register observer groups in behavioral order. Passing this execution
  # environment keeps the extracted blocks in the same lexical scope, so all
  # shared reactives and generated input/output bindings remain unchanged.
  observer_env <- environment()
  register_venn_data_list_observers(observer_env)
  register_venn_plot_interaction_observers(observer_env)
  register_venn_export_restore_observers(observer_env)

  invisible(NULL)
}
