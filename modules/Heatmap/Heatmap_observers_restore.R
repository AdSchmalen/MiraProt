
    apply_heatmap_restore_ui_inputs <- function(captured, ids = NULL) {
      if (is.null(captured) || !is.list(captured)) return(invisible(FALSE))
      color_ids <- c(
        "Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3",
        "missing_value_color_heatmap", "diagonal_line_color"
      )
      selectize_ids <- c("custom_col_sel_heatmap", "select_samples_heatmap")
      dynamic_ids <- heatmap_restore_dynamic_input_ids %||% character(0)
      target_ids <- ids
      if (is.null(target_ids)) target_ids <- names(captured)
      target_ids <- setdiff(target_ids, dynamic_ids)
      for (id in target_ids) {
        val <- captured[[id]]
        if (is.null(val)) next
        tryCatch({
          if (id %in% color_ids && is.character(val) && length(val) == 1L && nzchar(val) &&
              requireNamespace("colourpicker", quietly = TRUE)) {
            tryCatch(
              colourpicker::updateColourInput(session, id, value = val),
              error = function(e) updateTextInput(session, id, value = val)
            )
          } else if (is.logical(val) && length(val) == 1L) {
            updateCheckboxInput(session, id, value = isTRUE(val))
          } else if (is.numeric(val) && length(val) == 1L) {
            updateNumericInput(session, id, value = val)
          } else if (id %in% selectize_ids) {
            updateSelectizeInput(session, id, selected = val)
          } else if (is.character(val)) {
            updateSelectInput(session, id, selected = val)
          }
        }, error = function(e) {
          heatmap_debug_log(paste("[Heatmap] restore: failed to update", id, ":", e$message), 1)
        })
      }
      invisible(TRUE)
    }

    resolve_heatmap_restore_data_pair <- function() {
      pair <- tryCatch(isolate(data_pair_for_restore()), error = function(e) {
        heatmap_debug_log(paste("[Heatmap] restore data pair resolution skipped:", e$message), 2)
        NULL
      })
      if (is.list(pair) && (isTRUE(pair$stale) || isTRUE(pair$degraded))) {
        return(pair)
      }
      if (is.list(pair) && inherits(pair$data_mod, "data.frame") && inherits(pair$data_def, "data.frame")) {
        return(pair)
      }
      NULL
    }

    build_heatmap_restore_context <- function(input_values = NULL) {
      restore_pair <- resolve_heatmap_restore_data_pair()
      pending_ui_inputs <- heatmap_state$pending_ui_inputs
      plot_request <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
      live_input_values <- if (is.list(input_values)) {
        input_values
      } else {
        tryCatch(isolate(reactiveValuesToList(input, all.names = TRUE)), error = function(e) list())
      }

      list(
        data_mod = if (is.list(restore_pair)) restore_pair$data_mod else NULL,
        data_def = if (is.list(restore_pair)) restore_pair$data_def else tryCatch(isolate(rv$data_def), error = function(e) NULL),
        data_pair = restore_pair,
        plot_request = plot_request,
        matrix_payload = heatmap_state$pending_matrix_payload,
        pending_ui_inputs = pending_ui_inputs,
        input_values = live_input_values,
        expression_matrix = tryCatch(isolate(heatmap_expression_matrix()), error = function(e) NULL),
        basemean_values = tryCatch(isolate(heatmap_basemean_values()), error = function(e) NULL),
        abundance_ratio_values = tryCatch(isolate(heatmap_abundance_ratio_values()), error = function(e) NULL),
        cluster_info = tryCatch(isolate(heatmap_cluster_info()), error = function(e) NULL),
        shared_row_order = tryCatch(isolate(heatmap_shared_row_order()), error = function(e) NULL),
        shared_col_order = tryCatch(isolate(heatmap_shared_col_order()), error = function(e) NULL),
        highlighted_proteins = tryCatch(isolate(heatmap_highlighted_proteins()), error = function(e) NULL),
        fixed_plots = list(
          plots = tryCatch(isolate(heatmap_plots()), error = function(e) list()),
          expression = tryCatch(isolate(heatmap_fixed_expression()), error = function(e) NULL),
          protein_correlation = tryCatch(isolate(heatmap_fixed_protein_correlation()), error = function(e) NULL),
          sample_correlation = tryCatch(isolate(heatmap_fixed_sample_correlation()), error = function(e) NULL),
          basemean = tryCatch(isolate(heatmap_fixed_basemean()), error = function(e) NULL),
          abundance_ratio = tryCatch(isolate(heatmap_fixed_abundance_ratio()), error = function(e) NULL)
        )
      )
    }


    heatmap_restore_dynamic_input_ids <- c(
      "custom_col_sel_heatmap",
      "select_samples_heatmap",
      "GeneIdentifierColumn_Heatmap",
      "pval_type_heatmap",
      "pval_col_heatmap",
      "abundance_ratio_col_heatmap",
      "abundance_ratio_col_extension_heatmap",
      "GSEA_IdentifierFilter_Heatmap",
      "GO_IdentifierFilter_Heatmap"
    )

    heatmap_restore_saved_value_present <- function(value) {
      !is.null(value) && !(is.character(value) && !any(nzchar(value)))
    }

    heatmap_restore_choices_for_control <- function(id, captured = NULL, data_def = NULL) {
      dd <- data_def
      if (is.null(dd) || !is.data.frame(dd)) return(character(0))

      if (identical(id, "custom_col_sel_heatmap")) {
        possible_values <- c(
          "Raw Abundance", "Normalized Abundance", "Imputed Raw Abundance",
          "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance", "Batch Corrected Raw Abundance",
          "Batch Corrected Normalized Abundance"
        )
        return(possible_values[normalize_content_value(possible_values) %in% normalize_content_value(dd$Content)])
      }

      if (identical(id, "select_samples_heatmap")) {
        data_type <- captured[["custom_col_sel_heatmap"]]
        if (is.null(data_type) || !nzchar(data_type)) return(character(0))
        if (!"Sample" %in% names(dd)) return(character(0))
        sample_rows <- which(
          normalize_content_value(dd$Content) == normalize_content_value(data_type) & !is.na(dd$Sample)
        )
        return(unique(dd$Sample[sample_rows]))
      }

      if (identical(id, "GeneIdentifierColumn_Heatmap")) {
        identifier_rows <- which(dd$Content == "Identifier")
        if (length(identifier_rows) == 0) return(character(0))
        identifier_column_names <- dd$Column[identifier_rows]
        identifier_labels <- dd$Options[identifier_rows]
        valid_rows <- !is.na(identifier_labels) & nzchar(trimws(identifier_labels)) &
          !is.na(identifier_column_names) & nzchar(trimws(identifier_column_names))
        identifier_choices <- identifier_column_names[valid_rows]
        names(identifier_choices) <- identifier_labels[valid_rows]
        return(identifier_choices)
      }

      pvalue_content_types <- c(
        "P-Value", "p-Value", "p.Value", "pvalue", "p_value",
        "Adj. P-Value", "Adj.P.Value", "adj.p.value", "adj_p_value",
        "FDR", "adj.P.Val", "p.adj",
        "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value"
      )
      if (identical(id, "pval_type_heatmap")) {
        return(intersect(pvalue_content_types, dd$Content))
      }
      if (identical(id, "pval_col_heatmap")) {
        selected_pvalue_type <- captured[["pval_type_heatmap"]]
        if (is.null(selected_pvalue_type) || !nzchar(selected_pvalue_type)) return(character(0))
        pvalue_cols <- which(dd$Content == selected_pvalue_type)
        return(dd$Column[pvalue_cols])
      }

      if (id %in% c(
        "abundance_ratio_col_heatmap",
        "abundance_ratio_col_extension_heatmap"
      )) {
        ratio_cols <-
          which(
            dd$Content == "Abundance Ratio"
          )

        return(
          dd$Column[ratio_cols]
        )
      }

      character(0)
    }

    heatmap_format_saved_values_for_log <- function(value) {
      values <- as.character(value %||% character(0))
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0L) return("<empty>")
      paste(values, collapse = ", ")
    }

    heatmap_log_restore_missing_selection <- function(id, value) {
      heatmap_debug_log(
        paste(
          "[Heatmap] restore replay: saved selection not available in restored choices",
          "| field=", id,
          "| saved=", heatmap_format_saved_values_for_log(value)
        ),
        1
      )
    }

    heatmap_restore_apply_dynamic_control <- function(id, captured, data_def = NULL) {
      val <- captured[[id]]
      if (!heatmap_restore_saved_value_present(val)) {
        return(list(applied = TRUE, ready = TRUE, value = NULL, reason = "empty"))
      }

      choices <- heatmap_restore_choices_for_control(id, captured, data_def)
      choices_for_match <- unname(as.character(choices))
      if (length(choices_for_match) == 0L) {
        return(list(applied = FALSE, ready = FALSE, value = val, reason = "choices_not_ready"))
      }

      values_for_match <- unname(as.character(val))
      apply_value <- val
      missing_selection <- FALSE
      if (identical(id, "GeneIdentifierColumn_Heatmap")) {
        identifier_mapping <- heatmap_resolve_identifier_mapping(
          captured$identifier_column %||% captured$GeneIdentifierColumn_Heatmap %||% captured$identifier_label,
          data_def
        )
        if (isTRUE(identifier_mapping$resolved)) {
          apply_value <- identifier_mapping$identifier_column
          captured$identifier_label <- identifier_mapping$identifier_label
          captured$identifier_column <- identifier_mapping$identifier_column
          values_for_match <- unname(as.character(apply_value))
        }
      }
      if (identical(id, "select_samples_heatmap")) {
        keep <- values_for_match[values_for_match %in% choices_for_match]
        missing <- setdiff(values_for_match, keep)
        if (length(missing) > 0L) {
          heatmap_log_restore_missing_selection(id, missing)
          missing_selection <- TRUE
        }
        apply_value <- keep
      } else if (!all(values_for_match %in% choices_for_match)) {
        heatmap_log_restore_missing_selection(id, values_for_match[!values_for_match %in% choices_for_match])
        apply_value <- NULL
        missing_selection <- TRUE
      }

      ok <- tryCatch({
        if (id %in% c("select_samples_heatmap", "custom_col_sel_heatmap")) {
          updateSelectizeInput(session, id, choices = choices, selected = apply_value)
        } else {
          updateSelectInput(session, id, choices = choices, selected = apply_value)
        }
        TRUE
      }, error = function(e) {
        heatmap_debug_log(paste("[Heatmap] restore replay", id, ": update failed (", e$message, ")"), 1)
        FALSE
      })

      list(
        applied = isTRUE(ok),
        ready = TRUE,
        value = apply_value,
        reason = if (isTRUE(ok) && isTRUE(missing_selection)) "missing_selection" else if (isTRUE(ok)) "applied" else "update_failed"
      )
    }


    heatmap_infer_restore_data_type <- function(data_def, columns = NULL) {
      if (!inherits(data_def, "data.frame") || !all(c("Content", "Column", "Sample") %in% names(data_def))) return(NULL)
      possible_values <- c(
        "Normalized Abundance", "Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
        "Imputed Raw Abundance", "Batch Corrected Normalized Abundance",
        "Batch Corrected Raw Abundance"
      )
      abundance_rows <- normalize_content_value(data_def$Content) %in% normalize_content_value(possible_values)
      if (!any(abundance_rows)) return(NULL)
      columns <- as.character(columns %||% character(0))
      columns <- columns[nzchar(columns)]
      if (length(columns) > 0L) {
        matched_rows <- abundance_rows & (as.character(data_def$Column) %in% columns | as.character(data_def$Sample) %in% columns)
        if (any(matched_rows)) {
          counts <- sort(table(as.character(data_def$Content[matched_rows])), decreasing = TRUE)
          if (length(counts) > 0L) return(names(counts)[1])
        }
      }
      for (candidate in possible_values) {
        if (any(normalize_content_value(data_def$Content[abundance_rows]) == normalize_content_value(candidate))) return(candidate)
      }
      as.character(data_def$Content[which(abundance_rows)[1]])
    }

    heatmap_restore_enrich_captured_inputs <- function(captured,
                                                       data_def = NULL,
                                                       expression_matrix = NULL,
                                                       shared_col_order = NULL) {
      if (is.null(captured) || !is.list(captured)) return(captured)
      dd <- data_def

      selected_type <- captured$custom_col_sel_heatmap
      if (!heatmap_restore_saved_value_present(selected_type)) {
        restored_columns_for_type <- captured$shared_col_order %||% captured$column_order %||% shared_col_order
        if (is.null(restored_columns_for_type) || length(restored_columns_for_type) == 0L) {
          restored_matrix_for_type <- expression_matrix
          if (is.matrix(restored_matrix_for_type)) restored_columns_for_type <- colnames(restored_matrix_for_type)
        }
        selected_type <- heatmap_infer_restore_data_type(dd, restored_columns_for_type)
        if (heatmap_restore_saved_value_present(selected_type)) {
          captured$custom_col_sel_heatmap <- selected_type
          heatmap_debug_log(paste("[Heatmap] session restore: inferred abundance data type from saved columns/metadata:", selected_type), 1)
        }
      }
      selected_samples <- captured$select_samples_heatmap %||% character(0)
      selected_samples <- as.character(selected_samples)
      selected_samples <- selected_samples[nzchar(selected_samples)]

      if (length(selected_samples) == 0L) {
        restored_col_order <- shared_col_order
        if (is.null(restored_col_order) || length(restored_col_order) == 0L) {
          restored_matrix <- expression_matrix
          if (is.matrix(restored_matrix)) restored_col_order <- colnames(restored_matrix)
        }

        if (length(restored_col_order) > 0L && inherits(dd, "data.frame") &&
            !is.null(selected_type) && nzchar(selected_type) &&
            all(c("Content", "Sample", "Column") %in% names(dd))) {
          sample_rows <- which(
            normalize_content_value(dd$Content) == normalize_content_value(selected_type) &
              !is.na(dd$Sample) &
              (dd$Column %in% restored_col_order | dd$Sample %in% restored_col_order)
          )
          restored_samples <- unique(as.character(dd$Sample[sample_rows]))
          restored_samples <- restored_samples[nzchar(restored_samples)]
          if (length(restored_samples) > 0L) {
            captured$select_samples_heatmap <- restored_samples
            heatmap_debug_log(paste(
              "[Heatmap] session restore: recovered selected samples from restored column order:",
              paste(restored_samples, collapse = ", ")
            ), 1)
          }
        }

        if (!heatmap_restore_saved_value_present(captured$select_samples_heatmap) &&
            inherits(dd, "data.frame") &&
            !is.null(selected_type) && nzchar(selected_type) &&
            all(c("Content", "Sample") %in% names(dd))) {
          sample_rows <- which(
            normalize_content_value(dd$Content) == normalize_content_value(selected_type) &
              !is.na(dd$Sample)
          )
          restored_samples <- unique(as.character(dd$Sample[sample_rows]))
          restored_samples <- restored_samples[nzchar(restored_samples)]
          if (length(restored_samples) > 0L) {
            captured$select_samples_heatmap <- restored_samples
            heatmap_debug_log(paste(
              "[Heatmap] session restore: recovered selected samples from data type metadata:",
              paste(restored_samples, collapse = ", ")
            ), 1)
          }
        }
      }

      if (!is.null(captured$custom_col_sel_heatmap) && inherits(dd, "data.frame")) {
        identifier_value <- captured$GeneIdentifierColumn_Heatmap %||% captured$identifier_column %||% captured$identifier_label
        if (!heatmap_restore_saved_value_present(identifier_value)) {
          if (all(c("Content", "Options", "Column") %in% names(dd))) {
            identifier_rows <- which(normalize_content_value(dd$Content) == normalize_content_value("Identifier"))
            if (length(identifier_rows) > 0L) {
              identifier_columns <- as.character(dd$Column[identifier_rows])
              identifier_labels <- as.character(dd$Options[identifier_rows])
              valid_identifier_rows <- !is.na(identifier_columns) & nzchar(trimws(identifier_columns))
              if (any(valid_identifier_rows)) {
                identifier_value <- identifier_columns[valid_identifier_rows][1]
                captured$GeneIdentifierColumn_Heatmap <- identifier_value
                captured$identifier_column <- identifier_value
                captured$identifier_label <- identifier_labels[valid_identifier_rows][1]
              }
            }
          }
        } else {
          identifier_mapping <- heatmap_resolve_identifier_mapping(identifier_value, dd)
          if (isTRUE(identifier_mapping$resolved)) {
            captured$identifier_label <- identifier_mapping$identifier_label
            captured$identifier_column <- identifier_mapping$identifier_column
          } else if (!isTRUE(identifier_mapping$ready)) {
            captured$request_complete <- FALSE
            captured$incomplete_reason <- "identifier_metadata_not_ready"
          }
        }

        if (isTRUE(captured$enable_pvalue_filter_heatmap) &&
            (!heatmap_restore_saved_value_present(captured$pval_type_heatmap) ||
             !heatmap_restore_saved_value_present(captured$pval_col_heatmap))) {
          pvalue_content_types <- c(
            "P-Value", "p-Value", "p.Value", "pvalue", "p_value",
            "Adj. P-Value", "Adj.P.Value", "adj.p.value", "adj_p_value",
            "FDR", "adj.P.Val", "p.adj",
            "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value"
          )
          pvalue_rows <- which(dd$Content %in% pvalue_content_types)
          if (length(pvalue_rows) > 0L) {
            if (!heatmap_restore_saved_value_present(captured$pval_type_heatmap)) {
              captured$pval_type_heatmap <- as.character(dd$Content[pvalue_rows[1]])
            }
            pvalue_col_rows <- which(dd$Content == captured$pval_type_heatmap)
            if (length(pvalue_col_rows) > 0L &&
                !heatmap_restore_saved_value_present(captured$pval_col_heatmap)) {
              captured$pval_col_heatmap <- as.character(dd$Column[pvalue_col_rows[1]])
            }
          }
        }

        if (!heatmap_restore_saved_value_present(captured$abundance_ratio_col_heatmap)) {
          ratio_cols <- which(dd$Content == "Abundance Ratio")
          if (length(ratio_cols) > 0L) captured$abundance_ratio_col_heatmap <- dd$Column[ratio_cols[1]]
        }
        if (!heatmap_restore_saved_value_present(
          captured$abundance_ratio_col_extension_heatmap
        )) {

          # Old session files had only one ratio selector. Preserve their
          # previous behaviour by using that selection for the extension.
          if (heatmap_restore_saved_value_present(
            captured$abundance_ratio_col_heatmap
          )) {

            captured$abundance_ratio_col_extension_heatmap <-
              captured$abundance_ratio_col_heatmap

          } else {

            ratio_cols <-
              which(
                dd$Content == "Abundance Ratio"
              )

            if (length(ratio_cols) > 0L) {
              captured$abundance_ratio_col_extension_heatmap <-
                dd$Column[ratio_cols[[1L]]]
            }
          }
        }
      }

      captured
    }

    # `heatmap_state` is a plain lexical environment, not a Shiny reactive
    # container. Its reads and writes are intentionally direct; only reactiveVal(),
    # reactive(), `rv`, and `input` reads at imperative boundaries need isolation.
    heatmap_settle_restore_job <- function(outcome = "success", error = NULL) {
      if (isTRUE(heatmap_state$restore_job_settled) ||
          is.null(heatmap_state$restore_job_id)) return(invisible(FALSE))
      resolver <- session$userData$resolve_restore_job
      if (!is.function(resolver)) return(invisible(FALSE))
      settled <- tryCatch(
        isTRUE(resolver(heatmap_state$restore_job_id, outcome, error)),
        error = function(e) {
          heatmap_debug_log(paste("[Heatmap] restore job settlement failed:", e$message), 1)
          FALSE
        }
      )
      if (settled) heatmap_state$restore_job_settled <- TRUE
      invisible(settled)
    }

    heatmap_clear_restore_state <- function() {
      if (identical(heatmap_state$restore_callbacks_pending %||% 0L, 0L)) {
        heatmap_settle_restore_job("success")
      }
      heatmap_state$restore_in_progress <- FALSE
      heatmap_state$pending_ui_inputs   <- NULL
      heatmap_state$pending_dynamic_ui_inputs <- NULL
      heatmap_state$pending_matrix_payload <- NULL
      heatmap_state$pending_plot_data_cache_ref <- NULL
      heatmap_state$pending_plot_data_cache_fingerprint <- NULL
      heatmap_state$pending_data_mod_revision_id <- NULL
      heatmap_state$pending_data_def_revision_id <- NULL
      heatmap_state$pending_annotation_state <- NULL
      heatmap_state$pending_had_heatmap <- FALSE
    }


    heatmap_log_restore_callback_error <- function(callback_name, restore_phase, error, captured = NULL) {
      saved_request <- captured
      if (is.null(saved_request) || !is.list(saved_request)) {
        saved_request <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
      }
      if (is.null(saved_request) || !is.list(saved_request)) {
        saved_request <- heatmap_state$pending_ui_inputs
      }
      present_ids <- character(0)
      if (is.list(saved_request)) {
        present_ids <- heatmap_plot_request_input_ids[vapply(heatmap_plot_request_input_ids, function(id) {
          heatmap_restore_saved_value_present(saved_request[[id]])
        }, logical(1))]
      }
      heatmap_debug_log(paste(
        "[Heatmap] restore callback error",
        "| callback=", callback_name,
        "| phase=", restore_phase,
        "| error=", conditionMessage(error),
        "| restore_in_progress=", isTRUE(heatmap_state$restore_in_progress),
        "| saved_request_fields_present=", if (length(present_ids) > 0L) paste(present_ids, collapse = ", ") else "<none>"
      ), 1)
    }



    heatmap_restore_request_presence_summary <- function(saved_request = NULL) {
      if (is.null(saved_request) || !is.list(saved_request)) {
        saved_request <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
      }
      if (is.null(saved_request) || !is.list(saved_request)) {
        saved_request <- heatmap_state$pending_ui_inputs
      }
      present_ids <- character(0)
      if (is.list(saved_request)) {
        present_ids <- heatmap_plot_request_input_ids[vapply(heatmap_plot_request_input_ids, function(id) {
          heatmap_restore_saved_value_present(saved_request[[id]])
        }, logical(1))]
      }
      missing_ids <- setdiff(heatmap_plot_request_input_ids, present_ids)
      paste(
        "present=", if (length(present_ids) > 0L) paste(present_ids, collapse = ", ") else "<none>",
        " | missing=", if (length(missing_ids) > 0L) paste(missing_ids, collapse = ", ") else "<none>",
        sep = ""
      )
    }

    heatmap_safe_on_flushed <- function(label, fn) {
      generation <- heatmap_state$restore_generation
      heatmap_state$restore_callbacks_pending <-
        as.integer(heatmap_state$restore_callbacks_pending %||% 0L) + 1L

      # The raw Shiny callback invokes only the common restore runner. The
      # runner supplies the reactive isolate, generation check, diagnostics,
      # and error settlement for every transitively invoked Heatmap helper.
      session$onFlushed(function() {
        .run_session_restore_callback(
          owner = "Heatmap", reason = label, generation = generation,
          phase = "render",
          job_metadata = list(
            current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_),
            job_id = heatmap_state$restore_job_id,
            resolve_job = function(job_id, outcome, error = NULL) {
              if (!identical(outcome, "success")) {
                heatmap_state$restore_callbacks_pending <- 0L
                return(heatmap_settle_restore_job(outcome, error))
              }
              heatmap_state$restore_callbacks_pending <- max(
                0L, as.integer(heatmap_state$restore_callbacks_pending %||% 1L) - 1L
              )
              if (identical(heatmap_state$restore_callbacks_pending, 0L)) {
                return(heatmap_settle_restore_job("success"))
              }
              TRUE
            }
          ),
          callback = fn
        )
      }, once = TRUE)
    }


    heatmap_log_restore_request_fields_present <- function(captured) {
      present_ids <- heatmap_plot_request_input_ids[vapply(heatmap_plot_request_input_ids, function(id) {
        heatmap_restore_saved_value_present(captured[[id]])
      }, logical(1))]
      missing_ids <- setdiff(heatmap_plot_request_input_ids, present_ids)
      heatmap_debug_log(paste(
        "[Heatmap] restore replay: saved request fields present before replay",
        "| present=", if (length(present_ids) > 0L) paste(present_ids, collapse = ", ") else "<none>",
        "| missing=", if (length(missing_ids) > 0L) paste(missing_ids, collapse = ", ") else "<none>"
      ), 1)
    }

    replay_heatmap_ui_from_request <- function(captured = NULL,
                                               restored_data_def = NULL,
                                               attempt = 1L,
                                               max_attempts = 6L,
                                               clear_when_done = TRUE,
                                               restore_context = NULL) {
      if (is.null(restore_context) || !is.list(restore_context)) {
        restore_context <- build_heatmap_restore_context()
      }
      if (is.null(captured)) {
        captured <- restore_context$plot_request %||% restore_context$pending_ui_inputs
      }
      if (is.null(captured) || !is.list(captured)) {
        heatmap_debug_log("[Heatmap] restore replay: skipped (no pending UI inputs)", 1)
        if (isTRUE(clear_when_done)) heatmap_clear_restore_state()
        return(invisible(TRUE))
      }

      if (identical(attempt, 1L)) {
        heatmap_log_restore_request_fields_present(captured)
      }

      pending_dynamic <- heatmap_state$pending_dynamic_ui_inputs
      if (is.null(pending_dynamic) || !is.list(pending_dynamic)) {
        pending_dynamic <- captured[intersect(names(captured), heatmap_restore_dynamic_input_ids)]
        heatmap_state$pending_dynamic_ui_inputs <- pending_dynamic
      }
      dd <- restored_data_def
      if (is.null(dd) || !is.data.frame(dd)) {
        dd <- restore_context$data_def
      }
      pending_ids <- character(0)
      for (id in heatmap_restore_dynamic_input_ids) {
        val <- pending_dynamic[[id]]
        if (!heatmap_restore_saved_value_present(val)) {
          heatmap_debug_log(paste("[Heatmap] restore replay", id, ": skipped"), 1)
          pending_dynamic[[id]] <- NULL
          next
        }

        apply_result <- heatmap_restore_apply_dynamic_control(id, captured = captured, data_def = dd)
        if (isTRUE(apply_result$applied) && identical(apply_result$reason, "applied")) {
          heatmap_debug_log(paste("[Heatmap] restore replay", id, ": accepted"), 1)
          captured[[id]] <- apply_result$value
          pending_dynamic[[id]] <- NULL
        } else if (isTRUE(apply_result$applied) && identical(apply_result$reason, "missing_selection")) {
          pending_dynamic[[id]] <- NULL
        } else if (!isTRUE(apply_result$ready) && attempt < max_attempts) {
          heatmap_debug_log(paste("[Heatmap] restore replay", id, ": waiting for choices (attempt", attempt, "of", max_attempts, ")"), 1)
          pending_ids <- c(pending_ids, id)
        } else {
          heatmap_debug_log(paste("[Heatmap] restore replay", id, ":", apply_result$reason %||% "not applied"), 1)
        }
      }
      heatmap_state$pending_dynamic_ui_inputs <- pending_dynamic

      if (length(pending_ids) > 0 && attempt < max_attempts) {
        heatmap_safe_on_flushed("heatmap_restore_replay_dynamic_inputs", function() {
          replay_heatmap_ui_from_request(captured, restored_data_def, attempt + 1L, max_attempts, clear_when_done, restore_context)
        })
        return(invisible(FALSE))
      }

      if (isTRUE(clear_when_done)) {
        heatmap_clear_restore_state()
        heatmap_debug_log("[Heatmap] session restore complete; guard cleared", 1)
      }
      invisible(TRUE)
    }

    heatmap_infer_identifier_column_from_saved_rows <- function(captured, data_mod, data_def) {
      saved_ids <- as.character(captured$row_identifiers %||% character(0))
      saved_ids <- saved_ids[nzchar(saved_ids)]
      if (length(saved_ids) == 0L || !inherits(data_mod, "data.frame") || !inherits(data_def, "data.frame")) return(NULL)
      if (!all(c("Content", "Column") %in% names(data_def))) return(NULL)
      identifier_rows <- which(normalize_content_value(data_def$Content) == normalize_content_value("Identifier"))
      identifier_cols <- as.character(data_def$Column[identifier_rows])
      identifier_cols <- identifier_cols[identifier_cols %in% names(data_mod)]
      if (length(identifier_cols) == 0L) return(NULL)
      scores <- vapply(identifier_cols, function(col) {
        values <- as.character(data_mod[[col]])
        sum(saved_ids %in% values, na.rm = TRUE)
      }, integer(1))
      if (length(scores) == 0L || max(scores) <= 0L) return(NULL)
      identifier_cols[which.max(scores)]
    }

    heatmap_prepare_restore_extension_heatmaps <- function(results, effective_input) {
      if (is.null(results) || !"Row Index" %in% names(results)) return(invisible(FALSE))
      expr_matrix <- tryCatch(isolate(heatmap_expression_matrix()), error = function(e) NULL)
      if (!is.matrix(expr_matrix) || nrow(expr_matrix) == 0L) return(invisible(FALSE))
      row_names <- rownames(expr_matrix)
      result_ids <- as.character(results$Identifier %||% character(0))

      if (isTRUE(effective_input$show_basemean_heatmap) && "baseMean" %in% names(results)) {
        basemean_values <- suppressWarnings(as.numeric(results$baseMean))
        names(basemean_values) <- result_ids
        if (!is.null(row_names) && all(row_names %in% names(basemean_values))) {
          basemean_values <- basemean_values[row_names]
        }
        heatmap_basemean_values(basemean_values)
        bm_ht <- tryCatch(create_basemean_heatmap(basemean_values, input = effective_input), error = function(e) {
          heatmap_debug_log(paste("[Heatmap] session restore: basemean heatmap creation failed:", e$message), 1)
          NULL
        })
        if (!is.null(bm_ht)) heatmap_fixed_basemean(bm_ht)
      }

      if (isTRUE(effective_input$show_abundance_ratio_heatmap) && "Abundance.Ratio" %in% names(results)) {
        ratio_values <- suppressWarnings(as.numeric(results$Abundance.Ratio))
        names(ratio_values) <- result_ids
        if (!is.null(row_names) && all(row_names %in% names(ratio_values))) {
          ratio_values <- ratio_values[row_names]
        }
        heatmap_abundance_ratio_values(ratio_values)
        ratio_ht <- tryCatch(create_abundance_ratio_heatmap(ratio_values, input = effective_input), error = function(e) {
          heatmap_debug_log(paste("[Heatmap] session restore: abundance-ratio heatmap creation failed:", e$message), 1)
          NULL
        })
        if (!is.null(ratio_ht)) heatmap_fixed_abundance_ratio(ratio_ht)
      }
      invisible(TRUE)
    }

    heatmap_merge_saved_restore_inputs <- function(primary, fallback) {
      merged <- if (is.list(primary)) primary else list()
      if (is.list(fallback)) {
        for (input_name in names(fallback)) {
          if (!heatmap_restore_saved_value_present(merged[[input_name]]) &&
              heatmap_restore_saved_value_present(fallback[[input_name]])) {
            merged[[input_name]] <- fallback[[input_name]]
          }
        }
      }
      merged
    }

    validate_heatmap_restore_request <- function(captured, restore_pair) {
      dm <- restore_pair$data_mod
      dd <- restore_pair$data_def
      fail <- function(reason) list(ok = FALSE, reason = reason, captured = captured)
      if (!inherits(dm, "data.frame") || !inherits(dd, "data.frame")) return(fail("restored data_mod/data_def are not data frames"))
      required_dd <- c("Content", "Sample", "Column")
      missing_dd <- setdiff(required_dd, names(dd))
      if (length(missing_dd) > 0L) return(fail(paste("metadata missing columns", paste(missing_dd, collapse = ", "))))
      data_type <- as.character(captured$custom_col_sel_heatmap %||% "")[1]
      if (is.na(data_type) || !nzchar(data_type)) return(fail("plot request has no abundance data type"))
      samples <- as.character(captured$select_samples_heatmap %||% character(0))
      samples <- samples[nzchar(samples)]
      if (length(samples) == 0L) return(fail("plot request has no selected samples"))
      sample_rows <- which(normalize_content_value(dd$Content) == normalize_content_value(data_type) & !is.na(dd$Sample) & dd$Sample %in% samples)
      if (length(sample_rows) == 0L) return(fail(paste("no restored metadata rows match data type", data_type, "and saved samples")))
      missing_samples <- setdiff(samples, as.character(dd$Sample[sample_rows]))
      if (length(missing_samples) > 0L) return(fail(paste("restored metadata missing saved samples", paste(missing_samples, collapse = ", "))))
      missing_abundance_cols <- setdiff(as.character(dd$Column[sample_rows]), names(dm))
      if (length(missing_abundance_cols) > 0L) return(fail(paste("restored data missing abundance columns", paste(missing_abundance_cols, collapse = ", "))))
      identifier_col <- as.character(captured$identifier_column %||% captured$GeneIdentifierColumn_Heatmap %||% "")[1]
      if (is.na(identifier_col) || !nzchar(identifier_col)) {
        inferred_identifier_col <- heatmap_infer_identifier_column_from_saved_rows(captured, dm, dd)
        if (!is.null(inferred_identifier_col) && nzchar(inferred_identifier_col)) {
          captured$GeneIdentifierColumn_Heatmap <- inferred_identifier_col
          identifier_col <- inferred_identifier_col
          heatmap_debug_log(paste("[Heatmap] session restore: inferred identifier column from saved row identifiers:", identifier_col), 1)
        } else {
          return(fail("plot request has no gene identifier column"))
        }
      }
      saved_row_indices <- suppressWarnings(as.integer(captured$selected_row_indices %||% integer(0)))
      saved_row_indices <- saved_row_indices[is.finite(saved_row_indices) & saved_row_indices >= 1L & saved_row_indices <= nrow(dm)]
      if (length(saved_row_indices) > 0L) {
        captured$selected_row_indices <- unique(saved_row_indices)
        captured$max_proteins_heatmap <- length(captured$selected_row_indices)
      }
      if (!identifier_col %in% names(dm)) {
        identifier_rows <- which(normalize_content_value(dd$Content) == normalize_content_value("Identifier"))
        mapped_col <- character(0)
        if (length(identifier_rows) > 0L && all(c("Options", "Column") %in% names(dd))) {
          mapped_col <- as.character(dd$Column[identifier_rows][as.character(dd$Options[identifier_rows]) == identifier_col])
          mapped_col <- mapped_col[!is.na(mapped_col) & nzchar(mapped_col)]
        }
        if (length(mapped_col) > 0L && mapped_col[1] %in% names(dm)) {
          captured$identifier_column <- mapped_col[1]
          captured$GeneIdentifierColumn_Heatmap <- mapped_col[1]
        } else {
          return(fail(paste("restored data missing identifier column", identifier_col)))
        }
      }
      if (identifier_col %in% names(dm)) {
        captured$identifier_column <- identifier_col
        identifier_mapping <- heatmap_resolve_identifier_mapping(identifier_col, dd)
        if (isTRUE(identifier_mapping$resolved)) {
          captured$identifier_label <- identifier_mapping$identifier_label
        }
      }
      list(ok = TRUE, reason = "ok", captured = captured)
    }

    heatmap_restore_object_availability <- function(fixed_plots) {
      fixed_plots <- fixed_plots %||% list()
      plots <- fixed_plots$plots
      if (is.null(plots) || !is.list(plots)) plots <- list()
      has_expr_plot <- !is.null(plots$expr) || !is.null(plots$expression)
      flags <- list(
        plot_expression = isTRUE(has_expr_plot),
        fixed_expression = !is.null(fixed_plots$expression),
        fixed_protein_correlation = !is.null(fixed_plots$protein_correlation),
        fixed_sample_correlation = !is.null(fixed_plots$sample_correlation),
        fixed_basemean = !is.null(fixed_plots$basemean),
        fixed_abundance_ratio = !is.null(fixed_plots$abundance_ratio)
      )
      flags$required_ready <- isTRUE(flags$plot_expression) &&
        isTRUE(flags$fixed_expression) &&
        isTRUE(flags$fixed_protein_correlation) &&
        isTRUE(flags$fixed_sample_correlation)
      flags
    }

    format_heatmap_restore_object_availability <- function(flags) {
      if (is.null(flags) || !is.list(flags)) return("<unavailable>")
      paste(sprintf("%s=%s", names(flags), vapply(flags, function(x) {
        if (isTRUE(x)) "TRUE" else "FALSE"
      }, character(1))), collapse = ", ")
    }

    log_heatmap_restore_rebuild_result <- function(ok, context, restore_context = NULL) {
      if (is.null(restore_context) || !is.list(restore_context)) {
        restore_context <- build_heatmap_restore_context()
      }
      flags <- heatmap_restore_object_availability(restore_context$fixed_plots)
      success <- isTRUE(ok) && isTRUE(flags$required_ready)
      if (isTRUE(ok) && !isTRUE(flags$required_ready)) {
        heatmap_debug_log(paste(
          "[Heatmap] session restore warning:", context,
          "returned TRUE but required heatmap objects are missing |",
          format_heatmap_restore_object_availability(flags)
        ), 1)
      }
      heatmap_debug_log(paste(
        "[Heatmap] session restore:", context,
        "success=", success, "|",
        format_heatmap_restore_object_availability(flags)
      ), 1)
      invisible(success)
    }

    # ========================================================================
    # Session restore: data-driven heatmap rebuild (v2.0)
    # ========================================================================
    # set_session_state() stages raw data + UI input values and raises the
    # heatmap_state$restore_in_progress guard. This observer rebuilds every
    # heatmap object from scratch using the restored matrices, stored
    # canonical orderings, and captured UI inputs — exactly as if the user
    # had clicked "Create Heatmap". Dynamic-choices selectInputs are pushed
    # in a deferred onFlushed cascade so that reactive choice repopulation
    # (triggered by heatmap_data) completes first.
    restore_heatmap_plot_from_request <- function() {
      tryCatch({
        if (!isTRUE(heatmap_state$pending_had_heatmap)) {
          heatmap_debug_log("[Heatmap] session restore skipped: no heatmap was saved", 1)
          heatmap_clear_restore_state()
          return(invisible(FALSE))
        }
        restore_pending_ui_inputs <- heatmap_state$pending_ui_inputs
        restore_plot_request <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
        restore_defaults <- heatmap_restore_default_input_values()
        build_restore_context_snapshot <- function(input_values = restore_defaults, pending_ui_inputs = restore_pending_ui_inputs) {
          context <- build_heatmap_restore_context(input_values = input_values)
          context$pending_ui_inputs <- pending_ui_inputs
          context$plot_request <- restore_plot_request
          context
        }
        restore_context <- build_restore_context_snapshot()
        expr_matrix <- restore_context$expression_matrix
        if (is.null(expr_matrix) || !is.matrix(expr_matrix) || nrow(expr_matrix) == 0) {
          captured <- restore_context$pending_ui_inputs
          heatmap_debug_log("[Heatmap] session restore: no expression matrix payload; attempting rebuild from restored data/cache", 1)

          rebuild_from_restore_cache <- function(attempt = 1L, max_attempts = 6L) {
            restore_pair <- resolve_heatmap_restore_data_pair()
            if (is.list(restore_pair) && (isTRUE(restore_pair$stale) || isTRUE(restore_pair$degraded))) {
              heatmap_clear_restore_state()
              return(invisible(FALSE))
            }
            if (is.null(restore_pair)) {
              if (attempt < max_attempts) {
                heatmap_debug_log(paste(
                  "[Heatmap] session restore: waiting for Data Wizard data/cache before heatmap rebuild attempt",
                  attempt + 1L, "of", max_attempts
                ), 1)
                heatmap_safe_on_flushed("heatmap_restore_cache_rebuild_retry", function() {
                  rebuild_from_restore_cache(attempt + 1L, max_attempts)
                })
              } else {
                heatmap_debug_log("[Heatmap] session restore: data/cache unavailable after retries; heatmap rebuild skipped", 1)
                heatmap_clear_restore_state()
              }
              return(invisible(FALSE))
            }

            heatmap_debug_log(paste(
              "[Heatmap] session restore: resolved data pair for cache rebuild | source=",
              restore_pair$source %||% "unknown",
              " | data=", paste(dim(restore_pair$data_mod), collapse = "x"),
              " | metadata=", paste(dim(restore_pair$data_def), collapse = "x")
            ), 1)
            restore_context <- build_heatmap_restore_context()
            restore_context$data_mod <- restore_pair$data_mod
            restore_context$data_def <- restore_pair$data_def
            restore_context$data_pair <- restore_pair
            authoritative_request <- heatmap_merge_saved_restore_inputs(restore_context$plot_request, captured)
            captured <- heatmap_restore_enrich_captured_inputs(
              authoritative_request,
              restore_pair$data_def,
              expression_matrix = restore_context$expression_matrix,
              shared_col_order = restore_context$shared_col_order
            )
            heatmap_state$pending_ui_inputs <- captured

            validation <- validate_heatmap_restore_request(captured, restore_pair)
            if (!isTRUE(validation$ok)) {
              heatmap_debug_log(paste("[Heatmap] session restore: saved plot request cannot be applied:", validation$reason), 1)
              heatmap_clear_restore_state()
              return(invisible(FALSE))
            }
            captured <- validation$captured %||% captured
            heatmap_state$pending_ui_inputs <- captured

            ok <- tryCatch(
              isolate(isTRUE(run_heatmap_creation(
                trigger_source = "session_restore",
                data_pair_override = restore_pair,
                input_override = captured,
                silent_restore = TRUE
              ))),
              error = function(e) {
                heatmap_debug_log(paste("[Heatmap] session restore cache rebuild failed:", e$message), 1)
                FALSE
              }
            )
            restore_ok <- log_heatmap_restore_rebuild_result(ok, "rebuilt heatmaps from saved plot request and restored data/cache")
            if (isTRUE(restore_ok)) {
              heatmap_safe_on_flushed("heatmap_restore_cache_rebuild_render", function() {
                replay_heatmap_ui_from_request(captured, clear_when_done = TRUE, restore_context = build_heatmap_restore_context())
              })
            } else {
              heatmap_clear_restore_state()
            }
            invisible(TRUE)
          }

          rebuild_from_restore_cache()
          return()
        }
        restore_matrix_t0 <- Sys.time()
        heatmap_debug_log("[Heatmap] session restore: staging UI inputs before matrix/heatmap restore", 1)

        # --- Step 1: Push static-choice UI inputs immediately ---
        # Inputs whose selectInput choices are populated by reactive
        # observers (driven by heatmap_data) need the deferred cascade below;
        # everything else can be updated now.
        captured <- restore_context$pending_ui_inputs
        restored_dd_for_replay <- restore_context$data_def
        if (!is.null(captured) && is.list(captured)) {
          static_ids <- c(
            "sort_proteins_by", "sort_samples_by", "skip_log_transform_heatmap", "legend_position",
            "remove_na_abundance_heatmap", "min_abundance_values_per_row_heatmap",
            "max_proteins_heatmap", "custom_proteins_filter", "custom_protein_order", "custom_protein_fallback_sort",
            "enable_pvalue_filter_heatmap", "pval_threshold_heatmap",
            "enable_ratio_filter_heatmap", "ratio_filter_mode_heatmap", "ratio_threshold_heatmap",
            "show_expr_row_labels", "show_expr_col_labels",
            "show_corr_row_labels", "show_corr_col_labels",
            "show_sample_row_labels", "show_sample_col_labels",
            "show_basemean_row_labels", "show_basemean_col_labels",
            "show_abundance_ratio_row_labels", "show_abundance_ratio_col_labels",
            "show_row_dendrogram", "show_column_dendrogram",
            "row_font_size", "col_font_size",
            "legend_title_font_size", "legend_text_font_size", "legend_plot_gap_heatmap",
            "Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3",
            "missing_value_color_heatmap", "correlation_enhanced_contrast", "hideTitle_Heatmap",
            "show_basemean_heatmap", "show_abundance_ratio_heatmap",
            "show_correlation_diagonal", "diagonal_line_color", "diagonal_line_width", "diagonal_rotate",
            "resolution_DPI_Heatmaps", "plotWidthInch_Heatmaps", "plotHeightInch_Heatmaps", "downloadFormat_Heatmaps"
          )
          color_ids <- c("Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3",
                         "missing_value_color_heatmap", "diagonal_line_color")
          for (id in static_ids) {
            val <- captured[[id]]
            if (is.null(val)) next
            tryCatch({
              if (id %in% color_ids && is.character(val) && nzchar(val) &&
                  requireNamespace("colourpicker", quietly = TRUE)) {
                tryCatch(
                  colourpicker::updateColourInput(session, id, value = val),
                  error = function(e) updateTextInput(session, id, value = val)
                )
              } else if (is.logical(val)) {
                updateCheckboxInput(session, id, value = val)
              } else if (is.numeric(val)) {
                updateNumericInput(session, id, value = val)
              } else if (is.character(val)) {
                updateSelectInput(session, id, selected = val)
              }
            }, error = function(e) {
              heatmap_debug_log(paste("[Heatmap] restore: failed to update", id, ":", e$message), 1)
            })
          }
        }

        if (is.null(expr_matrix)) {
          local_captured <- heatmap_state$pending_ui_inputs
          heatmap_safe_on_flushed("heatmap_restore_matrix_ui_replay_outer", function() {
            replay_heatmap_ui_from_request(local_captured, restored_data_def = restored_dd_for_replay, clear_when_done = FALSE, restore_context = restore_context)
            heatmap_safe_on_flushed("heatmap_restore_matrix_ui_replay_inner", function() {
              heatmap_debug_log(sprintf(
                "[Heatmap] session restore timing: matrix cache/UI resolve %.3f sec; starting deferred heatmap render",
                as.numeric(difftime(Sys.time(), restore_matrix_t0, units = "secs"))
              ), 1)
              render_t0 <- Sys.time()
              ok <- tryCatch(
                isolate(run_heatmap_creation(trigger_source = "restore:cache_ref")),
                error = function(e) {
                  heatmap_debug_log(paste("[Heatmap] session restore deferred render failed:", e$message), 1)
                  FALSE
                }
              )
              heatmap_debug_log(sprintf(
                "[Heatmap] session restore timing: heatmap render %.3f sec",
                as.numeric(difftime(Sys.time(), render_t0, units = "secs"))
              ), 1)
              log_heatmap_restore_rebuild_result(ok, "deferred heatmap render", build_heatmap_restore_context())
              replay_heatmap_ui_from_request(local_captured, restored_data_def = restored_dd_for_replay, clear_when_done = TRUE, restore_context = build_heatmap_restore_context())
            })
          })
          return()
        }

        heatmap_debug_log(sprintf(
          "[Heatmap] session restore timing: matrix payload restore %.3f sec; rebuilding heatmap objects from stored matrices",
          as.numeric(difftime(Sys.time(), restore_matrix_t0, units = "secs"))
        ), 1)
        render_t0 <- Sys.time()

        # --- Step 2: Rebuild heatmap objects from data (honor stored order) ---
        # Use the staged plot request as the synchronous input snapshot for the
        # initial restore render. update*Input() calls above are asynchronous, so
        # reading `input` directly here can observe stale defaults.
        restore_input_list <- heatmap_merge_saved_restore_inputs(
          heatmap_merge_saved_restore_inputs(restore_plot_request, restore_pending_ui_inputs),
          restore_defaults
        )
        captured_inputs <- heatmap_restore_enrich_captured_inputs(
          restore_input_list,
          data_def = restore_context$data_def,
          expression_matrix = expr_matrix,
          shared_col_order = restore_context$shared_col_order
        )
        heatmap_state$pending_ui_inputs <- captured_inputs
        if (!is.null(captured_inputs) && is.list(captured_inputs)) {
          for (input_key in names(captured_inputs)) {
            val <- captured_inputs[[input_key]]
            if (!is.null(val)) restore_input_list[[input_key]] <- val
          }
        }

        # --- Step 2: Rebuild heatmap objects from data (honor stored order) ---
        cluster_info  <- restore_context$cluster_info
        row_order     <- restore_context$shared_row_order
        col_order     <- restore_context$shared_col_order
        basemean_vals <- NULL
        ratio_vals    <- NULL
        highlighted   <- restore_context$highlighted_proteins

        # Re-order the matrix to match the stored canonical order so that the
        # rebuilt heatmap rows match the saved ordering exactly (fixes the
        # "row order differs after restore" bug).  Dendrograms computed on the
        # reordered matrix then match what the user saw at save time.
        if (!is.null(row_order) && length(row_order) > 0) {
          valid_rows <- intersect(row_order, rownames(expr_matrix))
          if (length(valid_rows) > 0) {
            expr_matrix <- expr_matrix[valid_rows, , drop = FALSE]
            # cluster_info$row_order_idx was a permutation for the original
            # (pre-reorder) matrix.  Now that expr_matrix rows ARE already in
            # display order, reset to the identity so create_expression_heatmap_object
            # does not double-permute rows and mis-align the dendrogram labels.
            if (!is.null(cluster_info)) {
              cluster_info$row_order_idx <- seq_len(nrow(expr_matrix))
            }
          }
        }
        if (!is.null(col_order) && length(col_order) > 0) {
          valid_cols <- intersect(col_order, colnames(expr_matrix))
          if (length(valid_cols) > 0) expr_matrix <- expr_matrix[, valid_cols, drop = FALSE]
        }

        # Finalize extension values only after the restored row order and row
        # identifiers are fixed. This prevents later rendering from recalculating
        # basemean/ratio columns against a different default-limited matrix.
        final_row_ids <- rownames(expr_matrix)
        align_saved_extension_values <- function(values) {
          if (is.null(values) || length(values) == 0L) return(NULL)
          values <- suppressWarnings(as.numeric(values))
          saved_names <- names(values)
          if (!is.null(final_row_ids) && length(final_row_ids) > 0L &&
              !is.null(saved_names) && all(final_row_ids %in% saved_names)) {
            values <- values[final_row_ids]
          } else if (!is.null(final_row_ids) && length(values) == length(final_row_ids)) {
            names(values) <- final_row_ids
          } else {
            return(NULL)
          }
          names(values) <- final_row_ids
          values
        }
        saved_basemean_vals <- align_saved_extension_values(tryCatch(isolate(heatmap_basemean_values()), error = function(e) NULL))
        saved_ratio_vals <- align_saved_extension_values(tryCatch(isolate(heatmap_abundance_ratio_values()), error = function(e) NULL))

        restore_pair_for_extensions <- restore_context$data_pair
        if (is.list(restore_pair_for_extensions) &&
            inherits(restore_pair_for_extensions$data_mod, "data.frame") &&
            inherits(restore_pair_for_extensions$data_def, "data.frame")) {
          selected_rows <- suppressWarnings(as.integer(captured_inputs$selected_row_indices %||% captured_inputs$restored_row_indices %||% integer(0)))
          selected_rows <- selected_rows[!is.na(selected_rows) & selected_rows %in% seq_len(nrow(restore_pair_for_extensions$data_mod))]

          identifier_col <- as.character(captured_inputs$GeneIdentifierColumn_Heatmap %||% "")[1]
          restored_ids <- final_row_ids
          if (!is.na(identifier_col) && nzchar(identifier_col) && identifier_col %in% names(restore_pair_for_extensions$data_mod)) {
            restored_ids <- trimws(as.character(restore_pair_for_extensions$data_mod[[identifier_col]][selected_rows]))
          }

          align_restored_extension_values <- function(values) {
            if (is.null(values) || length(values) != length(selected_rows)) return(NULL)
            values <- suppressWarnings(as.numeric(values))
            names(values) <- restored_ids
            if (!is.null(final_row_ids) && length(final_row_ids) > 0L && all(final_row_ids %in% names(values))) {
              values <- values[final_row_ids]
            }
            names(values) <- final_row_ids
            values
          }

          if (length(selected_rows) > 0L) {
            basemean_vals <- align_restored_extension_values(calculate_correct_basemean_internal(
              selected_rows, restore_pair_for_extensions$data_mod, restore_pair_for_extensions$data_def, restore_input_list
            ))
            ratio_vals <- align_restored_extension_values(calculate_abundance_ratios_internal(
              selected_rows, restore_pair_for_extensions$data_mod, restore_pair_for_extensions$data_def, restore_input_list
            ))
          }
        }

        if (is.null(basemean_vals)) {
          basemean_vals <- saved_basemean_vals
          if (!is.null(basemean_vals)) heatmap_debug_log("[Heatmap] restore: basemean values recovered from cached extension snapshot", 1)
        }
        if (is.null(ratio_vals)) {
          ratio_vals <- saved_ratio_vals
          if (!is.null(ratio_vals)) heatmap_debug_log("[Heatmap] restore: abundance ratio values recovered from cached extension snapshot", 1)
        }
        if (!is.null(basemean_vals)) heatmap_basemean_values(basemean_vals)
        if (!is.null(ratio_vals)) heatmap_abundance_ratio_values(ratio_vals)

        expr_ht <- tryCatch(
          create_expression_heatmap_object(
            scaled_matrix = expr_matrix,
            groups        = NULL,
            input         = restore_input_list,
            cluster_info  = cluster_info
          ),
          error = function(e) {
            heatmap_debug_log(paste("[Heatmap] restore: expression heatmap rebuild failed:", e$message), 1)
            NULL
          }
        )
        if (is.null(expr_ht)) {
          heatmap_clear_restore_state()
          return()
        }

        # Re-apply labels from the Protein Selection Panel so highlighted
        # proteins remain labelled after restore.
        if (!is.null(highlighted) && length(highlighted) > 0) {
          protein_annotation <- tryCatch(
            create_protein_annotation(rownames(expr_matrix), highlighted),
            error = function(e) NULL
          )
          if (!is.null(protein_annotation)) {
            expr_ht <- tryCatch(protein_annotation + expr_ht, error = function(e) expr_ht)
          }
        }

        prot_ht <- tryCatch(
          heatmap_create_protein_correlation(expr_matrix, expr_ht),
          error = function(e) NULL
        )
        sample_ht <- tryCatch(
          heatmap_create_sample_correlation(expr_matrix, expr_ht),
          error = function(e) NULL
        )

        # Rebuild single-column extension heatmaps so the composite grid
        # includes basemean / abundance-ratio columns (fixes the "extensions
        # missing after restore" bug).

        basemean_ht <- if (!is.null(basemean_vals) && length(basemean_vals) > 0) {
          tryCatch(
            create_basemean_heatmap(basemean_vals, input = restore_input_list),
            error = function(e) {
              heatmap_debug_log(paste("[Heatmap] restore: basemean rebuild failed:", e$message), 1)
              NULL
            }
          )
        } else NULL
        ratio_ht <- if (!is.null(ratio_vals) && length(ratio_vals) > 0) {
          tryCatch(
            create_abundance_ratio_heatmap(ratio_vals, input = restore_input_list),
            error = function(e) {
              heatmap_debug_log(paste("[Heatmap] restore: abundance ratio rebuild failed:", e$message), 1)
              NULL
            }
          )
        } else NULL

        heatmap_fixed_expression(expr_ht)
        heatmap_fixed_protein_correlation(prot_ht)
        heatmap_fixed_sample_correlation(sample_ht)
        heatmap_fixed_basemean(basemean_ht)
        heatmap_fixed_abundance_ratio(ratio_ht)

        heatmap_plots(list(
          expr        = expr_ht,
          expression  = expr_ht,
          prot        = prot_ht,
          protein_cor = prot_ht,
          sample_cor  = sample_ht,
          ratio       = ratio_ht,
          basemean    = basemean_ht
        ))
        heatmap_debug_log(sprintf(
          "[Heatmap] session restore timing: matrix heatmap render %.3f sec",
          as.numeric(difftime(Sys.time(), render_t0, units = "secs"))
        ), 1)
        log_heatmap_restore_rebuild_result(
          TRUE,
          "rebuilt heatmaps from stored matrices",
          build_restore_context_snapshot(input_values = restore_input_list, pending_ui_inputs = captured_inputs)
        )

        # --- Step 3: Deferred push for dynamic-choices selectInputs ---
        # The observers that repopulate these selectInput choices depend on
        # heatmap_data and run on the next flush cycle. Wait a few flush
        # cycles so the choice lists are refreshed before we push the
        # selected value, otherwise updateSelectInput silently drops a value
        # not present in the current choices list.
        local_captured <- captured_inputs
        replay_restore_context <- build_restore_context_snapshot(input_values = restore_input_list, pending_ui_inputs = captured_inputs)
        heatmap_safe_on_flushed("heatmap_restore_matrix_payload_replay_outer", function() {
          heatmap_safe_on_flushed("heatmap_restore_matrix_payload_replay_inner", function() {
            replay_heatmap_ui_from_request(local_captured, restored_data_def = restored_dd_for_replay, clear_when_done = TRUE, restore_context = replay_restore_context)
          })
        })
      }, error = function(e) {
        heatmap_debug_log(paste("[Heatmap] session restore failed:", e$message), 1)
        heatmap_clear_restore_state()
      })
    }

    observeEvent(rv$session_restore_trigger, {
      restore_heatmap_plot_from_request()
    }, ignoreInit = TRUE)
