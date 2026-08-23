# =============================================================================
# modules/venn/venn_observers_export_restore.R

# Purpose: Downloads, cleanup, and session-restore observers.

# This peer is invoked by register_venn_observers() in registration order.
# evalq deliberately installs observers in the coordinator execution environment
# to preserve shared state, lexical lookup, and nested observeEvent behavior.
# =============================================================================

register_venn_export_restore_observers <- function(observer_env) {
  evalq({
  # ---------------------------------------------------------------------------
  # Download Handlers
  # ---------------------------------------------------------------------------

  output$downloadPlot_Venn <- downloadHandler(
    filename = function() {
      paste0("venn_diagram", Sys.Date(), ".", input$format_file_Venn)
    },
    content = function(file) {
      debug_log("Starting plot download", 1)
      plot_obj <- generatePlot_Venn()
      if (is.null(plot_obj)) return()
      save_plot_file(file, plot_obj, input$format_file_Venn,
                     input$width_plot_Venn, input$height_plot_Venn,
                     input$ppi_plot_Venn,
                     cached_mode = isTRUE(is.list(state$restore_plot_data_cache())))
      debug_log("Plot download completed", 1)
    }
  )

  output$download_intersections_xlsx <- downloadHandler(
    filename = function() {
      paste0("venn_intersections_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      debug_log("Starting intersection Excel export", 1)
      tryCatch({
        cache <- tryCatch(isolate(venn_data_cache()), error = function(e) NULL)

        if (is.list(cache) && !is.null(cache$input_lists)) {
          input_lists <- cache$input_lists
          list_names  <- names(input_lists)
          if (is.null(list_names) || any(!nzchar(list_names))) {
            list_names <- paste0("List", seq_along(input_lists))
          }
        } else {
          list_count  <- state$list_count_Venn()
          list_names  <- character()
          input_lists <- list()

          for (i in seq_len(list_count)) {
            list_name <- input[[paste0("name", i)]]
            if (is.null(list_name) || !nzchar(list_name)) {
              list_name <- paste0("List", i)
            }
            list_content <- input[[paste0("list", i)]]
            proteins <- character()
            if (!is.null(list_content) && nzchar(list_content)) {
              proteins <- trimws(unlist(strsplit(list_content, "\n")))
              proteins <- proteins[nzchar(proteins)]
            }
            list_names <- c(list_names, list_name)
            input_lists[[length(input_lists) + 1]] <- proteins
          }
        }

        if (length(input_lists) == 0) {
          stop("No input lists available for intersection export.")
        }

        plot_type <- input$diagramType_Venn %||% "Venn"
        value_data <- NULL
        value_column_name <- NULL

        if (is.list(cache) && plot_type %in% c("UpSet with Abundances", "UpSet with Abundance Ratios")) {
          value_data <- cache$prepared_data
          value_column_name <- if (plot_type == "UpSet with Abundances") "Abundance" else "Abundance Ratio"
        }

        wb <- build_venn_intersection_workbook(
          list_names,
          input_lists,
          value_data = value_data,
          value_column_name = value_column_name
        )
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

        debug_log("Intersection Excel export completed", 1)
      }, error = function(e) {
        debug_log(paste("Intersection Excel export failed:", e$message), 1)
        showNotification(
          paste("Venn intersection export failed:", e$message),
          type = "error"
        )
        stop(e)
      })
    }
  )

  # ---------------------------------------------------------------------------
  # Session Cleanup
  # ---------------------------------------------------------------------------

  if (!is.null(cleanup_mgr)) {
    cleanup_mgr$register_module("Venn", function() {
      debug_log("Executing [Venn] cleanup", 2)
      state$current_venn_plot(NULL)
      state$intersection_list(NULL)
      state$go_results_for_extraction(NULL)
      state$num_intersections_export(NULL)
      debug_log("[Venn] cleanup completed", 2)
    })
  } else {
    debug_log("[Venn] cleanup_manager unavailable; skipping cleanup registration", 1)
  }

  # ==========================================================================
  # Session restore: replay UI and regenerate plot from restored inputs
  # ==========================================================================
  observeEvent(rv$session_restore_trigger, {
    restore_generation <- isolate(rv$session_restore_generation %||% NA_integer_)
    register_restore_job <- session$userData$register_restore_job
    replay_job <- if (is.function(register_restore_job)) tryCatch(
      register_restore_job("Venn", "poll and plot rebuild", "render", 31),
      error = function(e) {
        debug_log(paste("[Venn] could not register restore replay job:", e$message), 1)
        NULL
      }
    ) else NULL

    tryCatch({
      captured <- isolate(state$pending_ui_inputs())
      # A newer trigger supersedes an armed poll. Attempt its settlement before
      # replacing the job token; the registry itself rejects late generations.
      if (!isTRUE(isolate(restore_poll_job_settled()))) {
        settle_restore_poll("skipped", "STALE_GENERATION")
      }
      restore_poll_generation(restore_generation)
      restore_poll_job(replay_job)
      restore_poll_job_settled(FALSE)

      session$onFlushed(function() {
        armed <- .run_session_restore_callback(
          owner = "Venn", reason = "arm restore poll",
          generation = restore_generation, phase = "render",
          job_metadata = list(
            current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_)
          ),
          callback = function() {
            restore_poll_captured(captured)
            restore_poll_attempt(0L)
            restore_poll_phase("base")
            restore_phase_attempt(0L)
            state$last_restore_report(NULL)
            restore_poll_active(TRUE)
            debug_log("[Venn] session restore: poll armed (waiting for Data Wizard-driven choices)", 1)
          }
        )
        if (!isTRUE(armed)) settle_restore_poll("skipped", "STALE_GENERATION")
      }, once = TRUE)
    }, error = function(e) {
      debug_log(paste("[Venn] session restore failed:", e$message), 1)
      restore_poll_active(FALSE)
      settle_restore_poll("failure", e$message)
    })
  }, ignoreInit = TRUE)

  observe({
    expected_generation <- restore_poll_generation()
    current_generation <- rv$session_restore_generation %||% NA_integer_
    poll_active <- isTRUE(restore_poll_active())
    if (poll_active && !identical(as.integer(current_generation)[1L],
                   as.integer(expected_generation)[1L])) {
      # Clearing the active dependency suspends this observer until a current
      # generation is explicitly armed.
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      settle_restore_poll("skipped", "STALE_GENERATION")
      return()
    }
    if (!poll_active) return()

    tryCatch({
    captured <- restore_poll_captured()
    phase <- isolate(restore_poll_phase())
    restoring_with_cached <- is.list(state$restore_plot_data_cache())
    attempt <- isolate(restore_poll_attempt()) + 1L
    restore_poll_attempt(attempt)
    phase_attempt <- isolate(restore_phase_attempt()) + 1L
    restore_phase_attempt(phase_attempt)

    if (attempt > 600L) {
      debug_log("[Venn] restore timed out waiting for UI synchronization", 1)
      restore_poll_active(FALSE)
      restore_poll_phase("base")
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      finalize_restore_report("restore_timeout", reason = "global_timeout")
      settle_restore_poll("timeout", "global_timeout")
      return()
    }

    phase_budget <- switch(phase, "base" = 200L, "dependent" = 300L, 300L)
    if (phase_attempt > phase_budget) {
      restore_poll_active(FALSE)
      restore_poll_phase("base")
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      finalize_restore_report("restore_timeout", reason = paste0("phase_timeout_", phase))
      settle_restore_poll("timeout", paste0("phase_timeout_", phase))
      return()
    }

    list_count <- state$list_count_Venn()
    if (is.null(list_count) || list_count < 1L) {
      invalidateLater(50, session)
      return()
    }
    # Re-apply restored list/UI values on every poll cycle to handle dynamic
    # list textareas that may appear only after list_count/renderUI flushes.
    for (i in seq_len(list_count)) {
      if (i <= length(state$list_data_Venn$names) && !is.null(state$list_data_Venn$names[[i]])) {
        updateTextInput(session, paste0("name", i), value = state$list_data_Venn$names[[i]])
      }
      if (i <= length(state$list_data_Venn$lists) && !is.null(state$list_data_Venn$lists[[i]])) {
        updateTextAreaInput(session, paste0("list", i), value = state$list_data_Venn$lists[[i]])
      }
      if (i <= length(state$list_data_Venn$colors) && !is.null(state$list_data_Venn$colors[[i]]) &&
          requireNamespace("colourpicker", quietly = TRUE)) {
        tryCatch(
          colourpicker::updateColourInput(session, paste0("color", i),
                                          value = state$list_data_Venn$colors[[i]]),
          error = function(e) NULL
        )
      }
    }
    if (is.list(captured)) {
      numeric_input_ids <- c(
        "overlapNumberSize_Venn", "listTitleSize_Venn",
        "listTitleDistance_Venn", "axis_title_size_Venn",
        "axis_text_size_Venn", "label_text_size_Venn",
        "width_plot_Venn", "height_plot_Venn", "ppi_plot_Venn"
      )
      select_input_ids <- c(
        "diagramType_Venn", "ReferenceValues_Venn", "GeneIdentifierColumn_Venn",
        "catFont_Venn", "cat_FontStyle_Venn", "font_family_Venn",
        "fontStyle_Venn", "ThemeSelect_Upset", "format_file_Venn"
      )
      base_ids <- c(select_input_ids, numeric_input_ids,
                    "showPercentages_Venn", "showListTitles_Venn",
                    "showDotsInBoxplot_Venn")
      late_ids <- c("data_abundance_Mean_Venn",
                    "data_abundance_ratio_num_Venn",
                    "data_abundance_ratio_denom_Venn")
      ids_to_apply <- if (identical(phase, "base")) base_ids else c(late_ids, "showDotsInBoxplot_Venn")
      ref_for_selectize <- as.character((captured$ReferenceValues_Venn %||% input$ReferenceValues_Venn) %||% "")[1]
      selectize_choices <- NULL
      sample_pair <- resolve_restore_data_pair("session_restore")
      sample_data_def <- sample_pair$data_def
      if (!is.null(sample_data_def) && is.data.frame(sample_data_def) &&
          !is.na(ref_for_selectize) && nzchar(ref_for_selectize) &&
          ("Content" %in% colnames(sample_data_def)) &&
          isTRUE(any(sample_data_def$Content == ref_for_selectize, na.rm = TRUE))) {
        selectize_choices <- venn_get_sample_choices(sample_data_def, ref_for_selectize)
      }
      selectize_ids <- late_ids
      for (id in intersect(names(captured), ids_to_apply)) {
        val <- captured[[id]]
        if (is.null(val)) next
        if (is.logical(val)) {
          updateCheckboxInput(session, id, value = val)
        } else if (is.numeric(val)) {
          updateNumericInput(session, id, value = val)
        } else if (is.character(val)) {
          if (id %in% selectize_ids) {
            if (!is.null(selectize_choices) && length(selectize_choices) > 0) {
              tryCatch(
                updateSelectizeInput(session, id,
                                     choices = selectize_choices,
                                     selected = val,
                                     server = TRUE),
                error = function(e) {
                  debug_log(paste0("[Venn] restore selectize update skipped for ", id,
                                   ": ", e$message), 2)
                }
              )
            } else if (restoring_with_cached) {
              debug_log(paste0("[Venn] restore selectize update deferred for ", id,
                               ": no dependent choices in cached mode"), 2)
            }
          } else if (id %in% numeric_input_ids) {
            updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
          } else if (id %in% select_input_ids) {
            updateSelectInput(session, id, selected = val)
          }
        }
      }
    }

    if (!restoring_with_cached) {
      for (i in seq_len(list_count)) {
        expected_name <- state$list_data_Venn$names[[i]]
        expected_list <- state$list_data_Venn$lists[[i]]
        current_name  <- input[[paste0("name", i)]]
        current_list  <- input[[paste0("list", i)]]
        norm_text <- function(x) {
          x <- as.character(x %||% "")[1]
          x <- gsub("\\r\\n?", "\n", x)
          trimws(x)
        }
        if (!is.null(expected_name) &&
            !identical(norm_text(current_name), norm_text(expected_name))) {
          invalidateLater(50, session)
          return()
        }
        if (!is.null(expected_list) &&
            !identical(norm_text(current_list), norm_text(expected_list))) {
          invalidateLater(50, session)
          return()
        }
      }
    }

    # Guard: wait until key restored UI values are visible server-side.
    if (!restoring_with_cached && is.list(captured) && !is.null(captured$diagramType_Venn)) {
      if (!identical(as.character(input$diagramType_Venn)[1],
                     as.character(captured$diagramType_Venn)[1])) {
        invalidateLater(50, session)
        return()
      }
    }
    if (!restoring_with_cached && is.list(captured) && !is.null(captured$ReferenceValues_Venn)) {
      if (!identical(as.character(input$ReferenceValues_Venn)[1],
                     as.character(captured$ReferenceValues_Venn)[1])) {
        invalidateLater(50, session)
        return()
      }
    }

    mode <- as.character((captured$diagramType_Venn %||% input$diagramType_Venn) %||% "")
    selected_ref <- as.character((captured$ReferenceValues_Venn %||% input$ReferenceValues_Venn) %||% "")[1]
    sample_pair <- resolve_restore_data_pair("session_restore")
    sample_data_def <- sample_pair$data_def
    sample_data_ready <- !is.null(sample_data_def) && is.data.frame(sample_data_def) &&
      ("Content" %in% colnames(sample_data_def)) &&
      !is.na(selected_ref) && nzchar(selected_ref) &&
      isTRUE(any(sample_data_def$Content == selected_ref, na.rm = TRUE))

    if (!sample_data_ready) {
      if (restoring_with_cached) {
        debug_log("[Venn] restore phase gate bypass: sample/reference choices not currently resolvable; continuing in cached mode", 2)
      } else {
        debug_log("[Venn] restore waiting: sample/reference choices unavailable in current live data", 2)
        invalidateLater(50, session)
        return()
      }
    }

    sample_choices <- if (sample_data_ready) venn_get_sample_choices(sample_data_def, selected_ref) else character()
    if (sample_data_ready && length(sample_choices) == 0L) {
      if (restoring_with_cached) {
        debug_log("[Venn] restore phase gate bypass: no dependent sample choices; continuing in cached mode", 2)
      } else {
        debug_log("[Venn] restore waiting: dependent sample choices not yet populated", 2)
        invalidateLater(50, session)
        return()
      }
    }

    # Require Data Wizard-driven primary choices to exist before progressing.
    if (!restoring_with_cached && is.list(captured)) {
      expected_ref <- as.character(captured$ReferenceValues_Venn %||% "")[1]
      expected_id_col <- as.character(captured$GeneIdentifierColumn_Venn %||% "")[1]
      ref_choices_ready <- !is.na(expected_ref) && nzchar(expected_ref) &&
        expected_ref %in% unique(as.character(sample_data_def$Content))
      id_choices <- colnames(sample_pair$data_mod %||% data.frame())
      id_choices_ready <- !is.na(expected_id_col) && nzchar(expected_id_col) && expected_id_col %in% id_choices
      if (!ref_choices_ready || !id_choices_ready) {
        debug_log("[Venn] restore waiting: primary selector choices not available in live Data Wizard context", 2)
        invalidateLater(50, session)
        return()
      }
    }

    if (identical(phase, "base")) {
      restore_poll_phase("dependent")
      restore_phase_attempt(0L)
      invalidateLater(50, session)
      debug_log("[Venn] session restore: base controls synchronized; applying dependent sample selectors", 1)
      return()
    }

    if (!isTRUE(state$had_plot_on_save())) {
      restore_poll_active(FALSE)
      restore_poll_phase("base")
      restore_phase_attempt(0L)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      finalize_restore_report("restore_skipped", reason = "had_plot_on_save_false")
      settle_restore_poll("skipped", "had_plot_on_save_false")
      return()
    }

    if (!restoring_with_cached && is.list(captured) &&
        (identical(mode, "UpSet with Abundances") || identical(mode, "UpSet with Abundance Ratios")) &&
        !is.null(captured$showDotsInBoxplot_Venn)) {
      if (!identical(isTRUE(input$showDotsInBoxplot_Venn), isTRUE(captured$showDotsInBoxplot_Venn))) {
        debug_log("[Venn] restore waiting: boxplot dots checkbox not yet synchronized", 2)
        invalidateLater(50, session)
        return()
      }
    }

    if (!restoring_with_cached && identical(mode, "UpSet with Abundances") &&
        is.list(captured) && !is.null(captured$data_abundance_Mean_Venn)) {
      expected_mean <- sort(as.character(captured$data_abundance_Mean_Venn))
      current_mean  <- sort(as.character(input$data_abundance_Mean_Venn %||% character()))
      if (length(expected_mean) > 0L &&
          length(intersect(expected_mean, current_mean)) < length(expected_mean)) {
        debug_log("[Venn] restore waiting: mean sample selectize not yet synchronized", 2)
        invalidateLater(50, session)
        return()
      }
    }
    if (!restoring_with_cached && identical(mode, "UpSet with Abundance Ratios") && is.list(captured)) {
      expected_num <- sort(as.character(captured$data_abundance_ratio_num_Venn %||% character()))
      expected_den <- sort(as.character(captured$data_abundance_ratio_denom_Venn %||% character()))
      current_num  <- sort(as.character(input$data_abundance_ratio_num_Venn %||% character()))
      current_den  <- sort(as.character(input$data_abundance_ratio_denom_Venn %||% character()))
      if (length(current_num) == 0L || length(current_den) == 0L) {
        debug_log("[Venn] restore waiting: ratio sample selectize values still empty", 2)
        invalidateLater(50, session)
        return()
      }
      if (length(expected_num) > 0L &&
          length(intersect(expected_num, current_num)) < length(expected_num)) {
        debug_log("[Venn] restore waiting: numerator sample selectize not yet synchronized", 2)
        invalidateLater(50, session)
        return()
      }
      if (length(expected_den) > 0L &&
          length(intersect(expected_den, current_den)) < length(expected_den)) {
        debug_log("[Venn] restore waiting: denominator sample selectize not yet synchronized", 2)
        invalidateLater(50, session)
        return()
      }
    }

    rebuilt_cache <- compute_venn_cache(
      source = "session_restore",
      source_mode = if (restoring_with_cached) "cached" else "live"
    )
    if (is.null(rebuilt_cache)) {
      if (isTRUE(state$restore_require_cached_data()) &&
          !is.list(state$restore_plot_data_cache())) {
        restore_poll_active(FALSE)
        restore_poll_phase("base")
        state$pending_ui_inputs(NULL)
        state$had_plot_on_save(FALSE)
        finalize_restore_report("restore_validation_failed", source = "none",
                                reason = "cached_data_required_missing")
        settle_restore_poll("failure", "cached_data_required_missing")
        return()
      }
      if (restoring_with_cached) {
        restore_poll_active(FALSE)
        restore_poll_phase("base")
        state$pending_ui_inputs(NULL)
        state$had_plot_on_save(FALSE)
        finalize_restore_report("restore_validation_failed", source = "cached",
                                reason = "cached_cache_build_failed")
        settle_restore_poll("failure", "cached_cache_build_failed")
        return()
      }
      invalidateLater(50, session)
      return()
    }

    rebuilt_plot <- build_plot_from_cache(
      rebuilt_cache,
      source_mode = rebuilt_cache$source_mode %||% if (restoring_with_cached) "cached" else "live"
    )
    if (is.null(rebuilt_plot)) {
      restore_poll_active(FALSE)
      restore_poll_phase("base")
      restore_phase_attempt(0L)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      finalize_restore_report(
        "restore_reconstruction_failed",
        source = rebuilt_cache$source_mode %||% if (restoring_with_cached) "cached" else "live",
        reason = "plot_builder_returned_null"
      )
      settle_restore_poll("failure", "plot_builder_returned_null")
      return()
    }
    if (!is.null(rebuilt_plot)) {
      state$num_intersections_export(NULL)
      output$plotContainer_Venn <- renderUI({ NULL })
      state$restored_plot_cache(rebuilt_cache)
      state$current_venn_plot(rebuilt_plot)
      state$current_plot_type(rebuilt_cache$type %||% "Venn")
      state$plot_active(TRUE)
      if ((rebuilt_cache$type %||% "Venn") != "Venn") {
        annotation_count <- if ((rebuilt_cache$type %||% "") %in%
                                c("UpSet with Abundances", "UpSet with Abundance Ratios")) 2L else 1L
        dims <- calculate_upset_plot_dimensions(
          num_intersections = rebuilt_cache$intersection_count %||% 1L,
          num_sets = length(rebuilt_cache$input_lists %||% list()),
          annotation_count = annotation_count,
          title_size_pt = input$title_text_size_Venn %||% 14
        )
        output$plotContainer_Venn <- renderUI({
          div(
            style = if (needs_horizontal_scroll(dims$plot_width_px)) {
              "overflow-x: auto; width: 100%; border: 1px solid #ddd;"
            } else {
              "width: 100%;"
            },
            plotOutput(ns("plotOutput_Venn"),
                       width = paste0(dims$plot_width_px, "px"),
                       height = paste0(dims$plot_height_px, "px"))
          )
        })
      } else {
        output$plotContainer_Venn <- renderUI({
          plotOutput(ns("plotOutput_Venn"), height = "900px", width = "100%")
        })
        saved_width_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("width_plot_Venn", 14))[1])
        saved_height_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("height_plot_Venn", 900 / 96))[1])
        if (!is.finite(saved_width_inches)) saved_width_inches <- 14
        if (!is.finite(saved_height_inches)) saved_height_inches <- 900 / 96
        updateNumericInput(session, inputId = "width_plot_Venn", value = saved_width_inches)
        updateNumericInput(session, inputId = "height_plot_Venn", value = saved_height_inches)
        update_last_plot_dimensions(width = saved_width_inches, height = saved_height_inches,
                                    ppi = input$ppi_plot_Venn, format = input$format_file_Venn)
      }
      debug_log(paste("[Venn] session restore: plot rebuilt (type:",
                      state$current_plot_type(), ")"), 1)
    }

    restored_source <- rebuilt_cache$source_mode %||% resolve_restore_data_pair("session_restore")$source

    cached_inputs <- captured %||% list()
    mode <- rebuilt_cache$type %||% "Venn"
    cached_selected_samples_count <- 0L
    cached_matched_sample_cols_count <- NA_integer_
    abundance_mode_fallback_reason <- NA_character_
    if (identical(mode, "UpSet with Abundances")) {
      cached_samples <- as.character(cached_inputs$data_abundance_Mean_Venn %||% character())
      cached_samples <- cached_samples[nzchar(cached_samples)]
      cached_selected_samples_count <- length(cached_samples)
      ref_value <- as.character(cached_inputs$ReferenceValues_Venn %||% "")
      data_def_cached <- resolve_restore_data_pair("session_restore")$data_def
      if (!is.null(data_def_cached) && nzchar(ref_value) && "Content" %in% names(data_def_cached) && "Sample" %in% names(data_def_cached)) {
        abundance_idx <- which(as.character(data_def_cached$Content) == ref_value)
        sample_labels <- as.character(data_def_cached$Sample[abundance_idx])
        cached_matched_sample_cols_count <- sum(!is.na(sample_labels) & nzchar(sample_labels) & sample_labels %in% cached_samples)
      }
      if (is.na(cached_matched_sample_cols_count) || identical(cached_matched_sample_cols_count, 0L)) {
        abundance_mode_fallback_reason <- "cached_samples_unmatched"
      }
    }

    finite_vals <- if (!is.null(rebuilt_cache$prepared_data) && "Value" %in% names(rebuilt_cache$prepared_data)) {
      sum(is.finite(rebuilt_cache$prepared_data$Value))
    } else {
      NA_integer_
    }
    boxplot_n_finite_y <- as.integer(finite_vals)
    if (identical(mode, "UpSet with Abundances") && !is.na(boxplot_n_finite_y) && boxplot_n_finite_y == 0L) {
      abundance_mode_fallback_reason <- "no_finite_y"
    }
    intersection_count_for_layout <- as.integer(rebuilt_cache$intersection_count %||% length(rebuilt_cache$intersection_list %||% list()))

    telemetry <- list(
      cached_selected_samples_count = as.integer(cached_selected_samples_count),
      cached_matched_sample_cols_count = as.integer(cached_matched_sample_cols_count),
      abundance_mode_fallback_reason = abundance_mode_fallback_reason,
      boxplot_n_finite_y = boxplot_n_finite_y,
      intersection_count_for_layout = intersection_count_for_layout
    )

    degraded_restore <- !is.na(abundance_mode_fallback_reason)

    if (identical(restored_source, "live") && isTRUE(state$restore_require_cached_data())) {
      finalize_restore_report("restored_live_fallback", source = "live",
                              reason = "cached_unavailable", telemetry = telemetry)
    } else {
      finalize_restore_report(if (isTRUE(degraded_restore)) "restored_cached_degraded" else "restored_cached_ok",
                              source = restored_source,
                              reason = if (isTRUE(degraded_restore)) abundance_mode_fallback_reason else "plot_rebuilt",
                              telemetry = telemetry)
    }
    # restored_plot_cache is the small derived interaction model and the plot
    # has rendered successfully. Only a non-degraded reconstruction may release
    # the restore-only source; degraded/failed paths retain their sole valid pair.
    if (!isTRUE(degraded_restore) && !is.null(rebuilt_plot)) {
      completed_pair <- resolve_restore_data_pair("session_restore")
      if (inherits(completed_pair$data_mod, "data.frame") &&
          inherits(completed_pair$data_def, "data.frame")) {
        state$plot_creation_cache(list(
          data_mod = completed_pair$data_mod,
          data_def = completed_pair$data_def
        ))
        state$restore_plot_data_cache(NULL)
      }
    }
    restore_poll_active(FALSE)
    restore_poll_phase("base")
    restore_phase_attempt(0L)
    state$pending_ui_inputs(NULL)
    state$had_plot_on_save(FALSE)
    settle_restore_poll("success")
    }, error = function(e) {
      restore_poll_active(FALSE)
      restore_poll_phase("base")
      restore_phase_attempt(0L)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      finalize_restore_report("restore_reconstruction_failed",
                              reason = "observer_error")
      settle_restore_poll("failure", conditionMessage(e))
      debug_log(paste("[Venn] restore observer failed:", conditionMessage(e)), 1)
    })
  })

  }, envir = observer_env)
  invisible(NULL)
}
