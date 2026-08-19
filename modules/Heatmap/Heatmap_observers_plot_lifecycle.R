    # --------------------------------------------------------------------------
    # Section 7: Reset button observer
    # --------------------------------------------------------------------------
    observeEvent(input$reset_heatmap_btn, {
      tryCatch({
        heatmap_debug_log("Resetting heatmap controls to UI defaults", 1)

        # Data selection controls with static defaults. Inputs whose initial
        # choices/selected values are NULL (for example sample, identifier,
        # p-value and ratio-column selectors) are intentionally skipped.
        updateSelectizeInput(session, "custom_col_sel_heatmap",
                             selected = "Normalized Abundance")
        updateSelectInput(session, "sort_samples_by", selected = "distance_cluster")
        updateSelectInput(session, "sort_proteins_by", selected = "z_score")
        updateCheckboxInput(session, "skip_log_transform_heatmap", value = FALSE)

        # Filtering options.
        updateCheckboxInput(session, "remove_na_abundance_heatmap", value = FALSE)
        colourpicker::updateColourInput(session, "missing_value_color_heatmap", value = "#E0E0E0")
        updateNumericInput(session, "min_abundance_values_per_row_heatmap", value = 1)
        updateCheckboxInput(session, "enable_pvalue_filter_heatmap", value = TRUE)
        updateNumericInput(session, "pval_threshold_heatmap", value = 0.05)
        updateCheckboxInput(session, "enable_ratio_filter_heatmap", value = FALSE)
        updateSelectInput(session, "ratio_filter_mode_heatmap", selected = "abs_log2_gt")
        updateNumericInput(session, "ratio_threshold_heatmap", value = 1.0)
        updateTextAreaInput(session, "custom_proteins_filter", value = "")
        updateNumericInput(session, "max_proteins_heatmap", value = 50)

        # Styling controls.
        colourpicker::updateColourInput(session, "Heatmap_ColorInput_1", value = "#440154FF")
        colourpicker::updateColourInput(session, "Heatmap_ColorInput_2", value = "white")
        colourpicker::updateColourInput(session, "Heatmap_ColorInput_3", value = "#EFC000FF")
        updateCheckboxInput(session, "correlation_enhanced_contrast", value = TRUE)
        updateCheckboxInput(session, "hideTitle_Heatmap", value = TRUE)
        updateCheckboxInput(session, "show_expr_row_labels", value = TRUE)
        updateCheckboxInput(session, "show_expr_col_labels", value = TRUE)
        updateCheckboxInput(session, "show_corr_row_labels", value = FALSE)
        updateCheckboxInput(session, "show_corr_col_labels", value = TRUE)
        updateCheckboxInput(session, "show_sample_row_labels", value = TRUE)
        updateCheckboxInput(session, "show_sample_col_labels", value = TRUE)
        updateCheckboxInput(session, "show_row_dendrogram", value = TRUE)
        updateCheckboxInput(session, "show_column_dendrogram", value = TRUE)
        updateNumericInput(session, "row_font_size", value = 10)
        updateNumericInput(session, "col_font_size", value = 10)
        updateNumericInput(session, "legend_title_font_size", value = 11)
        updateNumericInput(session, "legend_text_font_size", value = 9)
        updateSelectInput(session, "legend_position", selected = "right")

        # Extensions.
        updateCheckboxInput(session, "show_correlation_diagonal", value = TRUE)
        colourpicker::updateColourInput(session, "diagonal_line_color", value = "#7A7A7A")
        updateNumericInput(session, "diagonal_line_width", value = 2)
        updateCheckboxInput(session, "diagonal_rotate", value = FALSE)
        updateCheckboxInput(
          session,
          "show_basemean_heatmap",
          value = FALSE
        )
        updateCheckboxInput(session, "show_basemean_row_labels", value = FALSE)
        updateCheckboxInput(session, "show_basemean_col_labels", value = TRUE)
        updateCheckboxInput(
          session,
          "show_abundance_ratio_heatmap",
          value = FALSE
        )
        updateCheckboxInput(session, "show_abundance_ratio_row_labels", value = FALSE)
        updateCheckboxInput(session, "show_abundance_ratio_col_labels", value = TRUE)

        showNotification("Heatmap controls reset to defaults.", type = "message", duration = 3)
        heatmap_debug_log("Heatmap controls reset to UI defaults", 1)
      }, error = function(e) {
        heatmap_debug_log(paste("Error resetting heatmap controls:", e$message), 1)
        showNotification("Error resetting heatmap controls", type = "error", duration = 3)
      })
    })


    heatmap_restore_data_fingerprint <- function(data_mod = NULL, data_def = NULL) {
      helper <- get0(".plot_data_cache_fingerprint", envir = globalenv(), inherits = TRUE)
      if (is.function(helper)) {
        return(helper(data_mod = data_mod, data_def = data_def))
      }
      collect_df_fp <- function(df) {
        if (!inherits(df, "data.frame")) return("none")
        cn_txt <- paste(utils::head(colnames(df) %||% character(), 6L), collapse = "~")
        nr <- min(nrow(df), 3L)
        nc <- min(ncol(df), 3L)
        cell_txt <- ""
        if (nr > 0L && nc > 0L) {
          cells <- tryCatch(unlist(df[seq_len(nr), seq_len(nc), drop = FALSE], use.names = FALSE), error = function(e) character())
          cell_txt <- paste(utils::head(as.character(cells), 9L), collapse = "~")
        }
        sprintf("%08x", as.integer(sum(utf8ToInt(paste0(cn_txt, "||", cell_txt))) %% 2147483647L))
      }
      paste(collect_df_fp(data_mod), collect_df_fp(data_def), sep = "-")
    }

    heatmap_restore_cache_contract <- function() {
      list(
        plot_data_cache_ref = heatmap_state$pending_plot_data_cache_ref,
        data_mod_revision_id = heatmap_state$pending_data_mod_revision_id,
        data_def_revision_id = heatmap_state$pending_data_def_revision_id,
        plot_data_cache_fingerprint = heatmap_state$pending_plot_data_cache_fingerprint
      )
    }

    heatmap_restore_pair_fingerprint_valid <- function(pair) {
      if (!is.list(pair) || !inherits(pair$data_mod, "data.frame") || !inherits(pair$data_def, "data.frame")) {
        return(FALSE)
      }
      contract <- heatmap_restore_cache_contract()
      has_ref <- is.character(contract$plot_data_cache_ref) && length(contract$plot_data_cache_ref) == 1L && nzchar(contract$plot_data_cache_ref)
      has_fp <- is.character(contract$plot_data_cache_fingerprint) && length(contract$plot_data_cache_fingerprint) == 1L && nzchar(contract$plot_data_cache_fingerprint)
      if (!isTRUE(has_ref) && !isTRUE(has_fp)) return(TRUE)

      helper <- get0(".cache_ref_contract_compatible", envir = globalenv(), inherits = TRUE)
      if (is.function(helper) && isTRUE(has_ref)) {
        return(isTRUE(helper(contract, pair$data_mod, pair$data_def)))
      }
      current_fp <- heatmap_restore_data_fingerprint(pair$data_mod, pair$data_def)
      if (isTRUE(has_fp)) identical(as.character(contract$plot_data_cache_fingerprint)[1], current_fp) else TRUE
    }

    heatmap_restore_stale_pair <- function(pair) {
      expected_ref <- as.character(heatmap_state$pending_plot_data_cache_ref %||% "<none>")[1]
      expected_fp <- as.character(heatmap_state$pending_plot_data_cache_fingerprint %||% "<none>")[1]
      current_fp <- heatmap_restore_data_fingerprint(pair$data_mod, pair$data_def)
      heatmap_debug_log(paste(
        "[Heatmap] session restore stale data warning: automatic plot restore skipped;",
        "saved_cache_ref=", expected_ref,
        " | saved_fingerprint=", expected_fp,
        " | source=", pair$source %||% "unknown",
        " | data=", paste(dim(pair$data_mod), collapse = "x"),
        " | metadata=", paste(dim(pair$data_def), collapse = "x"),
        " | current_fingerprint=", current_fp
      ), 1)
      list(data_mod = NULL, data_def = NULL, source = "stale", stale = TRUE)
    }

    resolve_data_pair_local <- function(rv, restore_bundle = NULL, debug_log = NULL, module_label = "Heatmap") {
      bundle <- restore_bundle
      if (is.list(bundle) &&
          inherits(bundle$data_mod, "data.frame") &&
          inherits(bundle$data_def, "data.frame")) {
        if (is.function(debug_log)) {
          debug_log(sprintf("[%s] data_pair_for_restore resolved from staged restore bundle", module_label), 2)
        }
        return(list(data_mod = bundle$data_mod, data_def = bundle$data_def, source = "restore_bundle"))
      }

      live_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
      live_def <- tryCatch(rv$data_def, error = function(e) NULL)
      if (is.function(debug_log)) {
        debug_log(sprintf("[%s] data_pair_for_restore fell back to live rv$data_mod/rv$data_def", module_label), 2)
      }
      list(data_mod = live_mod, data_def = live_def, source = "live_rv")
    }

    data_pair_for_restore <- function() {
      if (!isTRUE(heatmap_state$restore_in_progress)) {
        live_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
        live_def <- tryCatch(rv$data_def, error = function(e) NULL)
        heatmap_debug_log("[Heatmap] manual/live data pair resolved from current rv$data_mod/rv$data_def", 2)
        return(list(data_mod = live_mod, data_def = live_def, source = "live_rv"))
      }

      restore_bundle <- tryCatch(rv$restore_plot_data_cache, error = function(e) NULL)
      pair <- if (exists("resolve_data_pair_for_restore", mode = "function")) {
        resolve_data_pair_for_restore(
          rv = rv,
          restore_bundle = restore_bundle,
          debug_log = heatmap_debug_log,
          module_label = "Heatmap",
          module_state = heatmap_restore_cache_contract()
        )
      } else {
        resolve_data_pair_local(
          rv = rv,
          restore_bundle = restore_bundle,
          debug_log = heatmap_debug_log,
          module_label = "Heatmap"
        )
      }

      if (is.list(pair) && inherits(pair$data_mod, "data.frame") && inherits(pair$data_def, "data.frame")) {
        if (!heatmap_restore_pair_fingerprint_valid(pair)) {
          return(heatmap_restore_stale_pair(pair))
        }
        if (identical(pair$source, "live_rv")) {
          heatmap_debug_log(paste(
            "[Heatmap] session restore: using Data Wizard-restored canonical live_rv data",
            "| data=", paste(dim(pair$data_mod), collapse = "x"),
            "| metadata=", paste(dim(pair$data_def), collapse = "x"),
            "| fingerprint=", heatmap_restore_data_fingerprint(pair$data_mod, pair$data_def)
          ), 1)
        }
      }
      pair
    }

    heatmap_plot_request_input_ids <- c(
      "custom_col_sel_heatmap", "select_samples_heatmap", "GeneIdentifierColumn_Heatmap",
      "sort_proteins_by", "sort_samples_by", "skip_log_transform_heatmap",
      "remove_na_abundance_heatmap", "min_abundance_values_per_row_heatmap",
      "enable_pvalue_filter_heatmap", "pval_type_heatmap", "pval_col_heatmap", "pval_threshold_heatmap",
      "enable_ratio_filter_heatmap", "abundance_ratio_col_heatmap",
      "abundance_ratio_col_extension_heatmap",
      "ratio_filter_mode_heatmap", "ratio_threshold_heatmap",
      "custom_proteins_filter", "custom_protein_order", "custom_protein_fallback_sort", "max_proteins_heatmap",
      "GSEA_IdentifierFilter_Heatmap", "GO_IdentifierFilter_Heatmap", "Intersect_IdentifierFilter_Heatmap", "CoreEnriched_IdentifierFilter_Heatmap",
      "Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3", "missing_value_color_heatmap",
      "correlation_enhanced_contrast", "legend_position", "legend_title_font_size", "legend_text_font_size", "legend_plot_gap_heatmap",
      "hideTitle_Heatmap",
      "show_expr_row_labels", "show_expr_col_labels", "show_corr_row_labels", "show_corr_col_labels",
      "show_sample_row_labels", "show_sample_col_labels", "show_basemean_row_labels", "show_basemean_col_labels",
      "show_abundance_ratio_row_labels", "show_abundance_ratio_col_labels",
      "row_font_size", "col_font_size", "show_row_dendrogram", "show_column_dendrogram",
      "show_basemean_heatmap", "show_abundance_ratio_heatmap",
      "show_correlation_diagonal", "diagonal_line_color", "diagonal_line_width", "diagonal_rotate",
      "resolution_DPI_Heatmaps", "plotWidthInch_Heatmaps", "plotHeightInch_Heatmaps", "downloadFormat_Heatmaps"
    )



    heatmap_restore_default_input_values <- function() {
      list(
        custom_col_sel_heatmap = "Normalized Abundance",
        select_samples_heatmap = character(0),
        GeneIdentifierColumn_Heatmap = NULL,
        sort_proteins_by = "z_score",
        sort_samples_by = "distance_cluster",
        skip_log_transform_heatmap = FALSE,
        remove_na_abundance_heatmap = FALSE,
        min_abundance_values_per_row_heatmap = 1L,
        enable_pvalue_filter_heatmap = TRUE,
        pval_type_heatmap = NULL,
        pval_col_heatmap = NULL,
        pval_threshold_heatmap = 0.05,
        enable_ratio_filter_heatmap = FALSE,
        abundance_ratio_col_heatmap = NULL,
        abundance_ratio_col_extension_heatmap = NULL,
        ratio_filter_mode_heatmap = "abs_log2_gt",
        ratio_threshold_heatmap = 1.0,
        custom_proteins_filter = "",
        GSEA_IdentifierFilter_Heatmap = character(0),
        GO_IdentifierFilter_Heatmap = character(0),
        Intersect_IdentifierFilter_Heatmap = FALSE,
        CoreEnriched_IdentifierFilter_Heatmap = TRUE,
        custom_protein_order = "",
        custom_protein_fallback_sort = "z_score",
        max_proteins_heatmap = 50L,
        Heatmap_ColorInput_1 = "#440154FF",
        Heatmap_ColorInput_2 = "white",
        Heatmap_ColorInput_3 = "#EFC000FF",
        missing_value_color_heatmap = "#E0E0E0",
        correlation_enhanced_contrast = TRUE,
        legend_position = "right",
        legend_title_font_size = 11L,
        legend_text_font_size = 9L,
        legend_plot_gap_heatmap = 6,
        hideTitle_Heatmap = FALSE,
        show_expr_row_labels = TRUE,
        show_expr_col_labels = TRUE,
        show_corr_row_labels = FALSE,
        show_corr_col_labels = TRUE,
        show_sample_row_labels = TRUE,
        show_sample_col_labels = TRUE,
        show_basemean_row_labels = FALSE,
        show_basemean_col_labels = TRUE,
        show_abundance_ratio_row_labels = FALSE,
        show_abundance_ratio_col_labels = TRUE,
        row_font_size = 10L,
        col_font_size = 10L,
        show_row_dendrogram = TRUE,
        show_column_dendrogram = TRUE,
        show_basemean_heatmap = FALSE,
        show_abundance_ratio_heatmap = FALSE,
        show_correlation_diagonal = TRUE,
        diagonal_line_color = "#7A7A7A",
        diagonal_line_width = 2,
        diagonal_rotate = FALSE,
        resolution_DPI_Heatmaps = 300L,
        plotWidthInch_Heatmaps = 12,
        plotHeightInch_Heatmaps = 10,
        downloadFormat_Heatmaps = "pdf"
      )
    }

    heatmap_resolve_identifier_mapping <- function(identifier_value, data_def) {
      identifier_value <- as.character(identifier_value %||% "")[1]
      if (is.na(identifier_value) || !nzchar(identifier_value)) {
        return(list(ready = TRUE, resolved = FALSE, identifier_label = NULL, identifier_column = NULL))
      }
      if (!inherits(data_def, "data.frame") ||
          !all(c("Content", "Column", "Options") %in% names(data_def))) {
        return(list(ready = FALSE, resolved = FALSE, identifier_label = NULL, identifier_column = NULL))
      }

      identifier_rows <- which(normalize_content_value(data_def$Content) == normalize_content_value("Identifier"))
      if (length(identifier_rows) == 0L) {
        return(list(ready = FALSE, resolved = FALSE, identifier_label = NULL, identifier_column = NULL))
      }

      identifier_columns <- as.character(data_def$Column[identifier_rows])
      identifier_labels <- as.character(data_def$Options[identifier_rows])
      valid_rows <- !is.na(identifier_columns) & nzchar(trimws(identifier_columns))
      identifier_columns <- identifier_columns[valid_rows]
      identifier_labels <- identifier_labels[valid_rows]

      column_match <- which(identifier_columns == identifier_value)
      if (length(column_match) > 0L) {
        idx <- column_match[1]
        label <- identifier_labels[idx]
        if (is.na(label) || !nzchar(trimws(label))) label <- identifier_columns[idx]
        return(list(ready = TRUE, resolved = TRUE, identifier_label = label, identifier_column = identifier_columns[idx]))
      }

      option_match <- which(!is.na(identifier_labels) & identifier_labels == identifier_value)
      if (length(option_match) > 0L) {
        idx <- option_match[1]
        return(list(ready = TRUE, resolved = TRUE, identifier_label = identifier_labels[idx], identifier_column = identifier_columns[idx]))
      }

      list(ready = TRUE, resolved = FALSE, identifier_label = NULL, identifier_column = NULL)
    }

    heatmap_enrich_request_identifier_mapping <- function(req, data_def = NULL) {
      mapping <- heatmap_resolve_identifier_mapping(req$GeneIdentifierColumn_Heatmap, data_def)
      if (isTRUE(mapping$resolved)) {
        req$identifier_label <- mapping$identifier_label
        req$identifier_column <- mapping$identifier_column
        req$request_complete <- TRUE
        req$incomplete_reason <- NULL
        return(req)
      }

      if (!isTRUE(mapping$ready)) {
        previous_request <- tryCatch(isolate(heatmap_last_plot_request()), error = function(e) NULL)
        if (is.list(previous_request) &&
            heatmap_restore_saved_value_present(previous_request$identifier_column) &&
            heatmap_restore_saved_value_present(previous_request$identifier_label)) {
          req$identifier_label <- previous_request$identifier_label
          req$identifier_column <- previous_request$identifier_column
        }
        req$request_complete <- FALSE
        req$incomplete_reason <- "identifier_metadata_not_ready"
        return(req)
      }

      req$request_complete <- FALSE
      req$incomplete_reason <- "identifier_mapping_unresolved"
      req
    }

    build_heatmap_plot_request <- function(input_values, trigger_source = "manual") {
      read_value <- function(id) {
        tryCatch(input_values[[id]], error = function(e) NULL)
      }
      req <- lapply(heatmap_plot_request_input_ids, read_value)
      names(req) <- heatmap_plot_request_input_ids
      req$skip_log_transform_heatmap <- isTRUE(req$skip_log_transform_heatmap)
      req$remove_na_abundance_heatmap <- isTRUE(req$remove_na_abundance_heatmap)
      req$enable_pvalue_filter_heatmap <- isTRUE(req$enable_pvalue_filter_heatmap)
      req$enable_ratio_filter_heatmap <- isTRUE(req$enable_ratio_filter_heatmap)
      req$custom_proteins_filter <- req$custom_proteins_filter %||% ""
      req$custom_protein_order <- req$custom_protein_order %||% ""
      req$custom_protein_fallback_sort <- req$custom_protein_fallback_sort %||% "z_score"
      req$max_proteins_heatmap <- suppressWarnings(as.integer(req$max_proteins_heatmap %||% 50L))
      if (!is.finite(req$max_proteins_heatmap) || is.na(req$max_proteins_heatmap)) req$max_proteins_heatmap <- 50L
      req$max_proteins_heatmap <- max(2L, req$max_proteins_heatmap)
      req$plot_request_version <- "1.0"
      req$trigger_source <- trigger_source
      req$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      req$matrix_source <- req$custom_col_sel_heatmap
      req$filters <- req[c(
        "remove_na_abundance_heatmap", "min_abundance_values_per_row_heatmap",
        "enable_pvalue_filter_heatmap", "pval_type_heatmap", "pval_col_heatmap", "pval_threshold_heatmap",
        "enable_ratio_filter_heatmap", "abundance_ratio_col_heatmap", "ratio_filter_mode_heatmap", "ratio_threshold_heatmap",
        "custom_proteins_filter", "custom_protein_order", "custom_protein_fallback_sort", "max_proteins_heatmap"
      )]
      req$colors <- req[c("Heatmap_ColorInput_1", "Heatmap_ColorInput_2", "Heatmap_ColorInput_3", "missing_value_color_heatmap", "correlation_enhanced_contrast", "diagonal_line_color")]
      req$labels <- req[c("show_expr_row_labels", "show_expr_col_labels", "show_corr_row_labels", "show_corr_col_labels", "show_sample_row_labels", "show_sample_col_labels", "show_basemean_row_labels", "show_basemean_col_labels", "show_abundance_ratio_row_labels", "show_abundance_ratio_col_labels", "row_font_size", "col_font_size")]
      req$dendrograms <- req[c("show_row_dendrogram", "show_column_dendrogram", "sort_proteins_by", "sort_samples_by")]
      req$extensions <- req[c(
        "show_basemean_heatmap",
        "show_basemean_row_labels",
        "show_basemean_col_labels",
        "show_abundance_ratio_heatmap",
        "show_abundance_ratio_row_labels",
        "show_abundance_ratio_col_labels",
        "abundance_ratio_col_extension_heatmap"
      )]
      req$rendering <- req[c("resolution_DPI_Heatmaps", "plotWidthInch_Heatmaps", "plotHeightInch_Heatmaps", "downloadFormat_Heatmaps")]
      heatmap_enrich_request_identifier_mapping(req, tryCatch(heatmap_df_data_definition(), error = function(e) rv$data_def))
    }

    # ========================================
    # Main Heatmap Creation Logic / Observer
    # ========================================
    heatmap_restore_trigger_sources <- c("session_restore", "restore:cache_ref")

    heatmap_is_restore_trigger <- function(trigger_source) {
      identical(trigger_source, "session_restore") ||
        grepl("^restore:", as.character(trigger_source %||% ""))
    }

    heatmap_finalize_plot_request <- function(plot_request, results, trigger_source) {
      final_expression_matrix <- tryCatch(heatmap_expression_matrix(), error = function(e) NULL)
      basemean_values <- tryCatch(heatmap_basemean_values(), error = function(e) NULL)
      abundance_ratio_values <- tryCatch(heatmap_abundance_ratio_values(), error = function(e) NULL)

      plot_request$trigger_source <- trigger_source
      plot_request$selected_row_indices <- results$`Row Index` %||% NULL
      plot_request$row_identifiers <- if (!is.null(final_expression_matrix)) rownames(final_expression_matrix) else NULL
      plot_request$column_order <- if (!is.null(final_expression_matrix)) colnames(final_expression_matrix) else NULL
      plot_request$shared_row_order <- tryCatch(heatmap_shared_row_order(), error = function(e) NULL)
      plot_request$shared_col_order <- tryCatch(heatmap_shared_col_order(), error = function(e) NULL)
      plot_request$highlighted_proteins <- heatmap_highlighted_proteins()

      if (!is.null(basemean_values)) {
        plot_request$basemean_values <- basemean_values
      }
      if (!is.null(abundance_ratio_values)) {
        plot_request$abundance_ratio_values <- abundance_ratio_values
      }

      plot_request
    }

    heatmap_store_plot_request <- function(plot_request, results, trigger_source) {
      canonical_request <- heatmap_finalize_plot_request(plot_request, results, trigger_source)
      heatmap_cached_plot_ui_inputs(canonical_request)
      heatmap_last_plot_request(canonical_request)
      invisible(canonical_request)
    }

    run_heatmap_creation <- function(trigger_source = "manual", data_pair_override = NULL, input_override = NULL, silent_restore = heatmap_is_restore_trigger(trigger_source)) {
      if (identical(trigger_source, "manual")) {
        heatmap_debug_log("Create heatmap button clicked - clustered expression + aligned correlations", 1)
      }

      effective_input <- input
      if (is.list(input_override)) {
        current_input <- tryCatch(
          reactiveValuesToList(input, all.names = TRUE),
          error = function(e) list()
        )
        for (input_id in names(input_override)) {
          current_input[[input_id]] <- input_override[[input_id]]
        }
        effective_input <- current_input
      }

      withProgress(message = "Creating aligned heatmaps...", value = 0, {
        combined_ht <- NULL
        expr_ht <- NULL
        protein_ht <- NULL
        sample_ht <- NULL

        incProgress(0.2, detail = "Performing statistical analysis...")
        plot_request <- build_heatmap_plot_request(effective_input, trigger_source = trigger_source)
        results <- heatmap_perform_statistical_analysis(
          rv = rv,
          input = effective_input,
          highlighted_proteins = heatmap_highlighted_proteins(),
          data_pair_for_restore = function() {
            if (is.list(data_pair_override) &&
                inherits(data_pair_override$data_mod, "data.frame") &&
                inherits(data_pair_override$data_def, "data.frame")) {
              return(data_pair_override)
            }
            data_pair_for_restore()
          },
          plot_request = plot_request,
          input_values = effective_input,
          silent_restore = isTRUE(silent_restore)
        )
        if (is.null(results)) {
          if (!isTRUE(silent_restore)) {
            showNotification("Statistical analysis failed", type = "error", duration = 5)
          } else {
            heatmap_debug_log(paste("[Heatmap]", trigger_source, "statistical analysis failed during restore; notification suppressed"), 1)
          }
          return(invisible(FALSE))
        }

        if (!is.null(results) && "Row Index" %in% names(results)) {
          heatmap_selected_row_indices(results$`Row Index`)
        }

        incProgress(0.35, detail = "Creating expression heatmap...")
        expr_ht <- tryCatch(heatmap_create_expression_heatmap(
          results,
          heatmap_debug_log,
          data_pair = attr(results, "data_pair", exact = TRUE),
          input_values = effective_input,
          plot_request = plot_request,
          silent_restore = isTRUE(silent_restore)
          ), error = function(e) {
          heatmap_debug_log(paste("Critical error creating expression heatmap:", e$message), 1)
          if (!isTRUE(silent_restore)) {
            showNotification(paste("Failed to create expression heatmap:", e$message), type = "error", duration = 5)
          }
          NULL
        })
        if (is.null(expr_ht)) return(invisible(FALSE))

        expr_matrix <- heatmap_expression_matrix()
        if (is.null(expr_matrix) || nrow(expr_matrix) == 0 || ncol(expr_matrix) == 0) {
          heatmap_applied_sort_state(list(
            sort_proteins_by = effective_input$sort_proteins_by %||% "z_score",
            sort_samples_by = effective_input$sort_samples_by %||% "none"
          ))
          plot_request$selected_row_indices <- results$`Row Index` %||% NULL
          plot_request$row_identifiers <- tryCatch(rownames(heatmap_expression_matrix()), error = function(e) NULL)
          plot_request$column_order <- tryCatch(colnames(heatmap_expression_matrix()), error = function(e) NULL)
          plot_request$highlighted_proteins <- heatmap_highlighted_proteins()
          heatmap_fixed_expression(expr_ht)
          heatmap_fixed_protein_correlation(NULL)
          heatmap_fixed_sample_correlation(NULL)
          heatmap_plots(list(expr = expr_ht, expression = expr_ht, prot = NULL, protein_cor = NULL, sample_cor = NULL, combined = expr_ht))
          heatmap_store_plot_request(plot_request, results, trigger_source)
          if (identical(trigger_source, "manual")) {
            showNotification("Heatmap created (expression only)", type = "message", duration = 3)
          }
          return(invisible(TRUE))
        }

        incProgress(0.55, detail = "Creating protein correlation (aligned)...")
        protein_ht <- heatmap_create_protein_correlation(expr_matrix, expr_ht)

        incProgress(0.7, detail = "Creating sample correlation (aligned)...")
        sample_ht <- heatmap_create_sample_correlation(expr_matrix, expr_ht)

        incProgress(0.85, detail = "Preparing heatmap layout...")

        # Optional alignment fix
        fixed <- tryCatch(
          align_expr_protein_by_row_labels(expr_ht, protein_ht),
          error = function(e) {
            heatmap_debug_log(paste("Alignment (expr+protein) via row labels failed:", e$message), 1)
            NULL
          }
        )
        if (!is.null(fixed)) {
          expr_ht    <- fixed$expr_ht_fixed
          protein_ht <- fixed$prot_ht_fixed
        }

        # Only cache a combined object for supported single-orientation layouts.
        # The full expression + protein + sample arrangement is rendered from the
        # synchronized components by the grid renderer; do not attempt the
        # unsupported mixed-orientation expression `(expr_ht + protein_ht) %v% sample_ht`.
        combined_ht <- tryCatch({
          if (!is.null(protein_ht) && is.null(sample_ht)) {
            expr_ht + protein_ht
          } else if (is.null(protein_ht) && !is.null(sample_ht)) {
            expr_ht %v% sample_ht
          } else if (is.null(protein_ht) && is.null(sample_ht)) {
            expr_ht
          } else {
            NULL
          }
        }, error = function(e) {
          NULL
        })

        heatmap_applied_sort_state(list(
          sort_proteins_by = effective_input$sort_proteins_by %||% "z_score",
          sort_samples_by = effective_input$sort_samples_by %||% "none"
        ))

        plot_request$selected_row_indices <- results$`Row Index` %||% NULL
        plot_request$row_identifiers <- tryCatch(rownames(heatmap_expression_matrix()), error = function(e) NULL)
        plot_request$column_order <- tryCatch(colnames(heatmap_expression_matrix()), error = function(e) NULL)
        plot_request$shared_row_order <- tryCatch(heatmap_shared_row_order(), error = function(e) NULL)
        plot_request$shared_col_order <- tryCatch(heatmap_shared_col_order(), error = function(e) NULL)
        plot_request$highlighted_proteins <- heatmap_highlighted_proteins()
        if (isTRUE(silent_restore)) heatmap_prepare_restore_extension_heatmaps(results, effective_input)
        heatmap_fixed_expression(expr_ht)
        heatmap_fixed_protein_correlation(protein_ht)
        heatmap_fixed_sample_correlation(sample_ht)

        heatmap_plots(list(
          expr        = expr_ht,
          expression  = expr_ht,
          prot        = protein_ht,
          protein_cor = protein_ht,
          sample_cor  = sample_ht,
          combined    = combined_ht,
          basemean    = tryCatch(isolate(heatmap_fixed_basemean()), error = function(e) NULL),
          ratio       = tryCatch(isolate(heatmap_fixed_abundance_ratio()), error = function(e) NULL)
        ))

        tryCatch({ verify_heatmap_alignment() }, error = function(e) {
          heatmap_debug_log(paste("Alignment verification failed (non-critical):", e$message), 2)
        })
        heatmap_store_plot_request(plot_request, results, trigger_source)

        if (identical(trigger_source, "manual")) {
          showNotification("ComplexHeatmap created: expression (clustered) + aligned correlations", type = "message", duration = 5)
        }
      })

      invisible(TRUE)
    }

    observeEvent(input$create_heatmap_btn, {
      run_heatmap_creation(trigger_source = "manual")
    })

    should_disable_live_heatmap_updates <- function() {

      max_proteins <- suppressWarnings(as.integer(input$max_proteins_heatmap %||% 50L))
      if (!is.finite(max_proteins) || is.na(max_proteins)) max_proteins <- 50L
      max_proteins <- max(2L, max_proteins)

      selected_samples <- input$select_samples_heatmap %||% character(0)
      n_samples <- length(unique(stats::na.omit(selected_samples)))
      n_samples <- max(1L, as.integer(n_samples))

      # Complexity estimate includes expression (rows x cols) and two correlation spaces.
      estimated_cells <- (max_proteins * n_samples) + (max_proteins^2) + (n_samples^2)
      threshold_cells <- 25000L

      list(
        disable = estimated_cells > threshold_cells,
        key = paste(max_proteins, n_samples, sep = "|"),
        estimated_cells = estimated_cells,
        threshold_cells = threshold_cells
      )
    }

    live_heatmap_no_prior_log_state <- new.env(parent = emptyenv())
    live_heatmap_no_prior_log_state$reason <- NULL
    live_heatmap_no_prior_log_state$time <- as.POSIXct(NA)
    live_heatmap_no_prior_log_timeout_secs <- 10

    should_log_no_prior_heatmap_skip <- function(reason, create_count) {
      # Once the user has explicitly requested a heatmap, stop throttling this
      # diagnostic so failed/cleared creations remain visible during debugging.
      if (!is.null(create_count) && create_count >= 1) {
        live_heatmap_no_prior_log_state$reason <- NULL
        live_heatmap_no_prior_log_state$time <- as.POSIXct(NA)
        return(TRUE)
      }

      now <- Sys.time()
      previous_time <- live_heatmap_no_prior_log_state$time
      should_log <- !identical(live_heatmap_no_prior_log_state$reason, reason) ||
        is.na(previous_time) ||
        difftime(now, previous_time, units = "secs") > live_heatmap_no_prior_log_timeout_secs

      if (isTRUE(should_log)) {
        live_heatmap_no_prior_log_state$reason <- reason
        live_heatmap_no_prior_log_state$time <- now
      }

      isTRUE(should_log)
    }

    trigger_live_heatmap_rebuild <- function(reason = "setting changed") {
      # Suppress live rebuilds during session restore — the restore observer
      # is authoritative and rebuilds with the captured UI inputs directly.
      # Without this guard, update*Input() echoes from the restore cascade
      # would kick off a stale rebuild that overwrites the restored state.
      if (isTRUE(heatmap_state$restore_in_progress)) {
        heatmap_debug_log(paste("Live update skipped (restore in progress):", reason), 2)
        return(invisible(FALSE))
      }
      create_count <- input$create_heatmap_btn
      if (is.null(heatmap_plots()$expr) || is.null(create_count) || create_count < 1) {
        if (should_log_no_prior_heatmap_skip(reason, create_count)) {
          heatmap_debug_log(paste("Live update skipped (no prior heatmap):", reason), 2)
        }
        return(invisible(FALSE))
      }

      selected_type <- input$custom_col_sel_heatmap
      selected_samples <- input$select_samples_heatmap %||% character(0)
      if (is.null(selected_type) || !nzchar(selected_type) || length(selected_samples) == 0) {
        heatmap_debug_log(paste("Live update deferred (incomplete inputs):", reason), 2)
        return(invisible(FALSE))
      }

      live_policy <- should_disable_live_heatmap_updates()
      if (isTRUE(live_policy$disable)) {
        notice_state <- heatmap_auto_update_notice_state() %||% list(key = NULL, time = as.POSIXct(NA))
        now <- Sys.time()
        should_notify <- !identical(notice_state$key, live_policy$key) ||
          is.na(notice_state$time) ||
          difftime(now, notice_state$time, units = "secs") > 10

        if (isTRUE(should_notify)) {
          showNotification(
            "Heatmap is too large for live updates. Changes will be applied after clicking Create plot again.",
            type = "message",
            duration = 6
          )
          heatmap_auto_update_notice_state(list(key = live_policy$key, time = now))
        }

        heatmap_debug_log(
          paste(
            "Live update disabled due to size:",
            "estimated_cells=", live_policy$estimated_cells,
            "threshold=", live_policy$threshold_cells,
            "reason=", reason
          ),
          1
        )
        return(invisible(FALSE))
      }

      heatmap_debug_log(paste("Live update triggered:", reason), 1)
      run_heatmap_creation(trigger_source = paste0("live:", reason))
      invisible(TRUE)
    }

    # Auto-recalculate heatmap when sort mode changes after first manual creation.
    # This keeps UI state and computed matrix ordering in sync.
    observeEvent(list(input$sort_proteins_by, input$sort_samples_by, input$skip_log_transform_heatmap, input$custom_protein_fallback_sort), {
      if (datawizard_restore_phase_active(rv)) {
        heatmap_debug_log("[Heatmap] restore phase active; skipping live sort/Z-score rebuild", 2)
        return()
      }
      trigger_live_heatmap_rebuild(reason = "sort/Z-score transform mode changed")
    }, ignoreInit = TRUE)

    debounced_identifier_filter <- shiny::debounce(
      reactive(input$custom_proteins_filter %||% ""),
      700
    )

    debounced_custom_protein_order <- shiny::debounce(
      reactive(input$custom_protein_order %||% ""),
      700
    )

    # Debounce the numeric abundance-value filter to prevent rapid typing from
    # firing a heatmap rebuild on every intermediate keystroke (e.g. clearing
    # the field before typing a new value briefly produces NA/1, which would
    # otherwise kick off a rebuild and then let the bounds observer clamp the
    # value, causing another rebuild – an infinite loop when used quickly).
    debounced_min_abundance_filter <- shiny::debounce(
      reactive(input$min_abundance_values_per_row_heatmap %||% 1L),
      500
    )

    debounced_live_heatmap_inputs <- shiny::debounce(
      reactive(list(
        custom_col_sel_heatmap = input$custom_col_sel_heatmap,
        select_samples_heatmap = input$select_samples_heatmap,
        remove_na_abundance_heatmap = input$remove_na_abundance_heatmap,
        min_abundance_values_per_row_heatmap = debounced_min_abundance_filter(),
        enable_pvalue_filter_heatmap = input$enable_pvalue_filter_heatmap,
        pval_type_heatmap = input$pval_type_heatmap,
        pval_col_heatmap = input$pval_col_heatmap,
        pval_threshold_heatmap = input$pval_threshold_heatmap,
        enable_ratio_filter_heatmap = input$enable_ratio_filter_heatmap,
        abundance_ratio_col_heatmap = input$abundance_ratio_col_heatmap,
        abundance_ratio_col_extension_heatmap =
          input$abundance_ratio_col_extension_heatmap,
        ratio_filter_mode_heatmap = input$ratio_filter_mode_heatmap,
        ratio_threshold_heatmap = input$ratio_threshold_heatmap,
        GeneIdentifierColumn_Heatmap = input$GeneIdentifierColumn_Heatmap,
        custom_proteins_filter = debounced_identifier_filter(),
        custom_protein_order = debounced_custom_protein_order(),
        custom_protein_fallback_sort = input$custom_protein_fallback_sort,
        max_proteins_heatmap = input$max_proteins_heatmap
      )),
      700
    )

    observeEvent(debounced_live_heatmap_inputs(), {
      if (datawizard_restore_phase_active(rv)) {
        heatmap_debug_log("[Heatmap] restore phase active; skipping live filter/data rebuild", 2)
        return()
      }
      trigger_live_heatmap_rebuild(reason = "filter/data setting changed")
    }, ignoreInit = TRUE)
