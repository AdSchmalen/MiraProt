# ==============================================================================
# 4. Analysis Execution Observer
# ==============================================================================

# ==============================================================================
# Reset controls with static UI defaults
# ==============================================================================

observeEvent(input$resetButton_GO, {
  tryCatch({
    debug_log("Resetting GO controls to UI defaults", 1)

    # Left-side plot selector and Plot Customization panel.
    updateSelectInput(session, "custom_EnrichPlot_select_GO",
                      selected = "Enrichment score dotplot")
    colourpicker::updateColourInput(session, "GOColorInput_down", value = "#440154FF")
    colourpicker::updateColourInput(session, "GOColorInput_zero", value = "#31688EFF")
    colourpicker::updateColourInput(session, "GOColorInput_up", value = "#EFC000FF")
    updateNumericInput(session, "AxisTitleSize_GO", value = 12)
    updateNumericInput(session, "tickSize_GO", value = 10)
    updateNumericInput(session, "LegendTitleSize_GO", value = 12)
    updateNumericInput(session, "LegendTextSize_GO", value = 10)
    updateSelectInput(session, "ThemeSelect_GO", selected = "Black and White")
    updateSelectInput(session, "LegendPosition_GO", selected = "right")
    updateNumericInput(session, "plot_height_go", value = 600)
    updateNumericInput(session, "max_terms_GO", value = 10)

    # Right-side GO Analysis Parameters with non-NULL static defaults.
    # Metadata-populated inputs that start with choices = NULL are intentionally
    # not touched here.
    updateCheckboxInput(session, "useMaxLog2FC_GO", value = FALSE)
    updateNumericInput(session, "AbundanceInput_GO", value = 1)
    updateNumericInput(session, "pvalueInput_GO", value = 0.05)
    updateSelectInput(session, "ont_GO", selected = "All")
    updateSelectInput(session, "padjustMethod_GO", selected = "Benjamini & Hochberg")
    updateNumericInput(session, "pvalueCutoff_GO", value = 0.05)
    updateNumericInput(session, "qvalueCutoff_GO", value = 0.2)
    updateNumericInput(session, "minGSSize_GO", value = 10)
    updateNumericInput(session, "maxGSSize_GO", value = 500)
    updateNumericInput(session, "randomSeed_GO", value = 12345)
    updateSelectInput(session, "keyType_GO", selected = "SYMBOL")
    updateSelectInput(session, "OrgDb_GO", selected = "Homo.sapiens")
    updateTextAreaInput(session, "universe_GO", value = "")

    showNotification("GO controls reset to defaults.", type = "message", duration = 3)
    debug_log("GO controls reset to UI defaults", 1)
  }, error = function(e) {
    debug_log(paste("Error resetting GO controls:", e$message), 1)
    showNotification("Error resetting GO controls", type = "error", duration = 3)
  })
})

run_go_analysis <- function(cache_policy = c("normal", "use_stale", "refresh"), pending = NULL) {
  cache_policy <- match.arg(cache_policy)
  debug_log("Starting GO analysis with direct approach", 1)
  go_analysis_status("running")
  go_results_ready_for_fallback(FALSE)
  go_initial_selection_applied(FALSE)
  go_errors(list())

  withProgress(message = 'Running GO Analysis...', value = 0, {
    tryCatch({
      req(rv$data_mod, rv$data_def)

      setProgress(value = 0.05, detail = "Validating input data")

      loadedData <- rv$data_mod
      metadata   <- rv$data_def

      if (is.null(loadedData) || is.null(metadata) || nrow(loadedData) == 0) {
        debug_log("Invalid input data", 1)
        go_analysis_status("error")
        showNotification("Please load valid proteomics data first.", type = "error", duration = 5)
        return(NULL)
      }

      debug_log("Validating input selections", 2)
      debug_log(paste("Gene ID column:", input$GeneIdentifierColumn_GO), 2)
      debug_log(paste("Abundance column:", input$AbundanceCol_GO), 2)
      debug_log(paste("P-value column:", input$pValCol_GO), 2)

      if (is.null(input$GeneIdentifierColumn_GO) || input$GeneIdentifierColumn_GO == "") {
        debug_log("No gene identifier column selected", 1)
        go_analysis_status("error")
        showNotification("Please select a gene identifier column.", type = "error", duration = 5)
        return(NULL)
      }

      if (is.null(input$AbundanceCol_GO) || input$AbundanceCol_GO == "") {
        debug_log("No abundance column selected", 1)
        go_analysis_status("error")
        showNotification("Please select an abundance ratio column.", type = "error", duration = 5)
        return(NULL)
      }

      if (is.null(input$pValCol_GO) || input$pValCol_GO == "") {
        debug_log("No p-value column selected", 1)
        go_analysis_status("error")
        showNotification("Please select a p-value column.", type = "error", duration = 5)
        return(NULL)
      }

      ididx <- which(metadata$Column == input$GeneIdentifierColumn_GO)
      fc_idx <- which(metadata$Column == input$AbundanceCol_GO)
      pv_idx <- which(metadata$Column == input$pValCol_GO)

      debug_log(paste("Found indices - Gene:", paste(ididx, collapse = ","),
                      "Abundance:", paste(fc_idx, collapse = ","),
                      "P-value:", paste(pv_idx, collapse = ",")), 2)

      if (length(ididx) == 0 || length(fc_idx) == 0 || length(pv_idx) == 0) {
        debug_log("Required data columns not found in metadata", 1)
        debug_log(paste("Available columns:", paste(metadata$Column, collapse = ", ")), 1)
        go_analysis_status("error")
        showNotification("Selected columns not found in data. Please check column selections.", type = "error", duration = 5)
        return(NULL)
      }

      ididx <- ididx[1]
      fc_idx <- fc_idx[1]
      pv_idx <- pv_idx[1]

      debug_log(paste("Using column indices - Gene:", ididx, "Abundance:", fc_idx, "P-value:", pv_idx), 1)

      setProgress(value = 0.12, detail = "Processing gene data")

      genes  <- as.character(loadedData[, ididx, drop = TRUE])
      fc_raw <- loadedData[, fc_idx, drop = TRUE]
      pv_raw <- loadedData[, pv_idx, drop = TRUE]

      debug_log(paste("Extracted data - Genes:", length(genes), "FC values:", length(fc_raw), "P-values:", length(pv_raw)), 2)

      go_data <- data.frame(
        Gene      = genes,
        Abundance = as.numeric(log2(as.numeric(fc_raw))),
        pVal      = as.numeric(pv_raw),
        stringsAsFactors = FALSE
      )

      debug_log(paste("Created data frame with", nrow(go_data), "rows"), 2)

      initial_count <- nrow(go_data)
      go_data_with_values <- go_data %>%
        dplyr::filter(!is.na(Abundance), !is.na(pVal), !is.na(Gene))
      go_data_non_empty_genes <- go_data_with_values %>%
        dplyr::filter(Gene != "", nzchar(Gene))
      abundance_threshold <- input$AbundanceInput_GO %||% if (isTRUE(input$useMaxLog2FC_GO)) Inf else -Inf
      go_data <- go_data_non_empty_genes %>%
        dplyr::filter(
          if (isTRUE(input$useMaxLog2FC_GO)) Abundance <= abundance_threshold else Abundance >= abundance_threshold,
          pVal <= (input$pvalueInput_GO %||% 1)
        )

      debug_log(paste("GO count stage 1 - filtered data rows after fold-change/p-value thresholds:", nrow(go_data)), 1)
      debug_log(paste("GO count stage 2 - non-empty gene rows:", nrow(go_data_non_empty_genes)), 1)
      debug_log(paste("GO count stage 3 - unique selected gene identifiers:", length(unique(go_data$Gene))), 1)
      debug_log(paste("Filtered from", initial_count, "to", nrow(go_data), "rows"), 1)

      if (nrow(go_data) == 0) {
        debug_log("No genes pass filtering criteria", 1)
        go_analysis_status("error")
        showNotification("No genes pass the filtering criteria. Try adjusting filter settings.", type = "error", duration = 5)
        return(NULL)
      }

      if (nrow(go_data) < 10) {
        debug_log(paste("Warning: Only", nrow(go_data), "genes pass filtering"), 1)
        showNotification(paste("Warning: Only", nrow(go_data), "genes pass filtering. Results may be limited."), type = "warning", duration = 5)
      }

      go_data_processed(go_data)

      setProgress(value = 0.25, detail = "Loading organism annotations")

      # Use session-level OrgDb cache when the organism has not changed since the
      # last run.  This avoids a redundant AnnotationHub BiocFileCache lookup on
      # every "Run GO Analysis" click for the same organism within one session.
      organism_display_go <- input$OrgDb_GO %||% "Homo sapiens"
      orgdb_name_go       <- organism_to_orgdb(organism_display_go)
      annotations         <- cached_go_org_db()

      if (is.null(annotations) || !identical(cached_go_orgdb_name(), orgdb_name_go)) {
        debug_log(paste("Session OrgDb cache miss for", orgdb_name_go,
                        "- resolving via GO annotation cache"), 1)
        if (identical(cache_policy, "use_stale")) {
          annotations <- pending$stale_db
          debug_log("GO annotation cache loaded with ignore_ttl=TRUE", 1)
        } else if (identical(cache_policy, "refresh")) {
          refresh_result <- force_refresh_safe(orgdb_name_go, debug_log = debug_log)
          if (isTRUE(refresh_result$success) && !is.null(refresh_result$data)) {
            annotations <- refresh_result$data
            debug_log("GO annotation cache refresh succeeded", 1)
          } else {
            debug_log("GO annotation cache refresh failed; falling back to existing stale cache", 1)
            showNotification(
              "Annotation update failed. The existing local cache is still available and will be used for this analysis.",
              type = "warning", duration = 8
            )
            annotations <- pending$stale_db
          }
        } else {
          annotations <- load_annotation_hub_with_progress(
            organism_display   = organism_display_go,
            debug_log          = debug_log,
            max_cache_age_days = 7
          )
        }
        if (!is.null(annotations)) {
          cached_go_org_db(annotations)
          cached_go_orgdb_name(orgdb_name_go)
          debug_log(paste("Session OrgDb cache populated for", orgdb_name_go), 1)
        }
      } else {
        attr(annotations, "go_annotation_resolver_source") <- "session cache"
        debug_log(paste("GO annotation resolver used session cache for", orgdb_name_go), 1)
      }

      if (is.null(annotations)) {
        debug_log("Failed to load annotations", 1)
        go_analysis_status("error")
        showNotification("Failed to load organism annotations.", type = "error", duration = 5)
        return(NULL)
      }

      setProgress(value = 0.38, detail = "Preparing GO universe")

      dataset_universe_raw <- as.character(loadedData[, ididx, drop = TRUE])
      dataset_universe_clean <- clean_go_identifiers(dataset_universe_raw)
      debug_log(paste("GO count stage 6 - raw dataset universe size:", length(dataset_universe_clean)), 1)

      enrichment_inputs <- prepare_go_enrichment_inputs(
        selected_genes        = go_data$Gene,
        dataset_universe      = dataset_universe_raw,
        custom_universe_text  = input$universe_GO,
        debug_log             = debug_log
      )

      if (isTRUE(enrichment_inputs$error)) {
        debug_log(enrichment_inputs$message, 1)
        go_analysis_status("error")
        showNotification(enrichment_inputs$message, type = "error", duration = 8)
        return(NULL)
      }

      debug_log(paste("GO count stage 4 - enrichment genes passed to enrichGO:", length(enrichment_inputs$genes)), 1)
      debug_log(paste("GO count stage 7 - universe size passed to enrichGO:", length(enrichment_inputs$universe)), 1)

      go_data_for_enrichment <- go_data[go_data$Gene %in% enrichment_inputs$genes, , drop = FALSE]
      debug_log(paste("GO count stage 5 - rows retained in go_data_for_enrichment:", nrow(go_data_for_enrichment)), 1)
      debug_log(paste("GO count detail - go_data_for_enrichment rows:", nrow(go_data_for_enrichment),
                      "unique genes:", length(unique(go_data_for_enrichment$Gene))), 1)
      go_data_processed(go_data_for_enrichment)

      if (isTRUE(enrichment_inputs$custom_universe_used)) {
        showNotification(
          paste0("Using custom GO universe with ", length(enrichment_inputs$universe), " identifiers."),
          type = "message", duration = 4
        )

        if (length(enrichment_inputs$removed_genes) > 0) {
          showNotification(
            paste0(
              length(enrichment_inputs$removed_genes),
              " filtered gene(s) were excluded because they are not present in the custom universe."
            ),
            type = "warning", duration = 8
          )
          debug_log(
            paste(
              "Filtered genes excluded from GO enrichment because they are absent from custom universe:",
              paste(head(enrichment_inputs$removed_genes, 20), collapse = ", ")
            ),
            1
          )
        }

        if (length(enrichment_inputs$universe_not_in_dataset) > 0) {
          debug_log(
            paste(
              "Custom universe identifiers not present in current dataset:",
              length(enrichment_inputs$universe_not_in_dataset),
              "examples:", paste(head(enrichment_inputs$universe_not_in_dataset, 20), collapse = ", ")
            ),
            1
          )
        }
      }

      setProgress(value = 0.45, detail = "Running GO enrichment")

      random_seed <- as.integer(input$randomSeed_GO %||% 12345)
      set.seed(random_seed)
      edo <- perform_go_enrichment(
        genes          = enrichment_inputs$genes,
        annotations    = annotations,
        keyType        = input$keyType_GO %||% "SYMBOL",
        ont            = convert_ontology_input(input$ont_GO %||% "BP"),
        pAdjustMethod  = convert_padjust_method(input$padjustMethod_GO %||% "BH"),
        pvalueCutoff   = input$pvalueCutoff_GO %||% 0.05,
        qvalueCutoff   = input$qvalueCutoff_GO %||% 0.2,
        minGSSize      = input$minGSSize_GO %||% 10,
        maxGSSize      = input$maxGSSize_GO %||% 500,
        universe       = enrichment_inputs$universe,
        debug_log      = debug_log
      )

      if (is.null(edo) || nrow(as.data.frame(edo)) == 0) {
        debug_log("No significant GO terms found", 1)
        go_analysis_status("completed")
        go_results_ready_for_fallback(FALSE)
        go_initial_selection_applied(FALSE)
        showNotification("No significant GO terms found. Try relaxing the p-value cutoff.", type = "warning", duration = 5)
        return(NULL)
      }

      debug_log(paste("GO count stage 8 - universe size stored in returned enrichResult:",
                      tryCatch(length(edo@universe), error = function(e) NA_integer_)), 1)
      debug_log(paste("GO enrichment completed with", nrow(edo@result), "significant terms"), 1)

      setProgress(value = 0.78, detail = "Processing enrichment results")

      results_list <- create_go_results_list_direct(
        edo         = edo,
        go_data     = go_data_for_enrichment,
        annotations = annotations,
        keyType     = input$keyType_GO %||% "SYMBOL",
        debug_log   = debug_log
      )

      results_list$parameters <- list(random_seed = random_seed)
      GO_Result_List(results_list)
      go_results_ready_for_fallback(TRUE)
      go_initial_selection_applied(FALSE)
      debug_log("Results stored with direct approach", 1)

      setProgress(value = 0.86, detail = "Building GO term tree")

      tryCatch({
        tree_structure <- create_go_tree_structure(
          edo       = results_list$Edo_GO,
          debug_log = debug_log
        )
        go_tree_structure(tree_structure)

        rv$go_results       <- results_list
        rv$go_tree          <- tree_structure
        rv$go_selected_terms <- character(0)

        if (!isTRUE(go_initial_selection_applied()) && length(rv$go_selected_terms) == 0) {
          result_df <- results_list$Edo_GO@result
          result_df <- result_df[order(result_df$p.adjust), ]
          max_terms <- input$max_terms_GO %||% 10
          initial_selection <- head(as.character(result_df$Description), max_terms)
          initial_selection <- initial_selection[!is.na(initial_selection) & nchar(trimws(initial_selection)) > 0]

          rv$go_selected_terms <- initial_selection
          updateSelectInput(session, "custom_Enrich_select_GO", selected = initial_selection)
          go_initial_selection_applied(TRUE)

          debug_log(paste("Initial GO term selection applied once with", length(initial_selection), "terms"), 1)
        }

        debug_log("Tree structure created and stored", 2)
      }, error = function(e) {
        debug_log(paste("Tree creation error:", e$message), 1)
      })

      setProgress(value = 0.93, detail = "Creating initial GO plot")

      initial_result <- tryCatch({
        debug_log("Creating enhanced initial plot with current UI parameters", 1)

        available_results <- results_list$Edo_GO@result
        if (nrow(available_results) == 0) {
          debug_log("No results available for initial plot", 1)
          return(list(plot = NULL, message = "No results available", height = 600, width = 800))
        }

        ordered_results <- available_results[order(available_results$p.adjust), ]
        n_terms    <- min(10, nrow(ordered_results))
        top_terms  <- head(ordered_results$Description, n_terms)
        top_terms  <- as.character(top_terms)

        debug_log(paste("Initial plot: using", length(top_terms), "top terms"), 1)

        ui_colors <- tryCatch({
          c(
            input$GOColorInput_down %||% "#440154FF",
            input$GOColorInput_zero %||% "#31688EFF",
            input$GOColorInput_up   %||% "#EFC000FF"
          )
        }, error = function(e) {
          debug_log("Using default colors for initial plot", 2)
          c("#440154FF", "#31688EFF", "#EFC000FF")
        })

        ui_sizes <- tryCatch({
          list(
            axisTitle   = as.numeric(input$AxisTitleSize_GO %||% 12),
            tick        = as.numeric(input$tickSize_GO      %||% 10),
            legendText  = as.numeric(input$LegendTextSize_GO  %||% 10),
            legendTitle = as.numeric(input$LegendTitleSize_GO %||% 12),
            label       = as.numeric(input$LabelSize_GO      %||% 12)
          )
        }, error = function(e) {
          debug_log("Using default sizes for initial plot", 2)
          list(axisTitle = 12, tick = 10, legendText = 10, legendTitle = 12, label = 12)
        })

        ui_legend_position <- tryCatch({
          legend_pos <- input$LegendPosition_GO %||% "right"
          debug_log(paste("Initial plot: using legend position", legend_pos), 2)
          legend_pos
        }, error = function(e) {
          debug_log("Using default legend position for initial plot", 2)
          "right"
        })

        debug_log(paste("Initial plot: using UI colors:", paste(ui_colors, collapse = ", ")), 2)
        debug_log(paste("Initial plot: using UI sizes - axisTitle:", ui_sizes$axisTitle, "label:", ui_sizes$label), 2)
        debug_log(paste("Initial plot: using legend position:", ui_legend_position), 2)

        ui_theme <- tryCatch({
          theme_name <- input$ThemeSelect_GO %||% "Black and White"
          debug_log(paste("Initial plot: using theme", theme_name), 2)

          switch(theme_name,
                 "Gray"          = theme_gray(),
                 "Black and White" = theme_bw(),
                 "Linedraw"      = theme_linedraw(),
                 "Light"         = theme_light(),
                 "Dark"          = theme_dark(),
                 "Minimal"       = theme_minimal(),
                 "Classic"       = theme_classic(),
                 "Void"          = theme_void(),
                 theme_bw())
        }, error = function(e) {
          debug_log("Using default theme for initial plot", 2)
          theme_bw()
        })

        create_go_dotplot(
          edo             = results_list$Edo_GO,
          selected_terms  = top_terms,
          colors          = ui_colors,
          sizes           = ui_sizes,
          theme           = ui_theme,
          legend_position = ui_legend_position
        )

      }, error = function(e) {
        debug_log(paste("Initial plot creation failed:", e$message), 1)
        list(plot = NULL, message = paste("Initial plot creation failed:", e$message), height = 600, width = 800)
      })

      setProgress(value = 0.98, detail = "Finalizing GO analysis")

      if (!is.null(initial_result) && !is.null(initial_result$plot)) {
        debug_log("Updating reactive values with initial plot", 1)

        tryCatch({
          current_plot_object(initial_result$plot)
          current_plot_message(initial_result$message %||% "Initial GO dotplot created with UI settings")
          current_plot_height(initial_result$height %||% 600)
          current_plot_width(initial_result$width   %||% 800)

          if (exists("plot_update_trigger") && is.function(plot_update_trigger)) {
            plot_update_trigger(plot_update_trigger() + 1)
          }

          debug_log("Initial plot reactive values updated successfully", 1)

        }, error = function(e) {
          debug_log(paste("Error updating reactive values:", e$message), 1)
        })

        showNotification(
          paste("GO analysis completed with", nrow(results_list$Edo_GO@result), "significant terms. Initial plot created with your current UI settings."),
          type = "message",
          duration = 5
        )

      } else {
        debug_log("Initial plot creation failed or returned NULL", 1)

        tryCatch({
          current_plot_message(initial_result$message %||% "Initial plot creation failed")
        }, error = function(e) {
          debug_log("Could not set error message", 2)
        })

        showNotification(
          paste("GO analysis completed with", nrow(results_list$Edo_GO@result), "significant terms, but initial plot creation failed. Use 'Create Plot' button to generate plots."),
          type = "warning",
          duration = 8
        )
      }
    }, error = function(e) {
      debug_log(paste("GO analysis error:", e$message), 1)
      go_analysis_status("error")
      go_results_ready_for_fallback(FALSE)
      go_initial_selection_applied(FALSE)
      go_errors(list(list(time = Sys.time(), message = e$message)))
      showNotification(paste("GO analysis failed:", e$message), type = "error", duration = 8)
    })
  })
}

# The cache-age threshold is advisory at the GO level.  Only structurally usable
# stale caches are offered to the user; corrupt caches continue through the
# normal resolver path.
observeEvent(input$createGO_button, {
  organism_display <- input$OrgDb_GO %||% "Homo sapiens"
  orgdb_name <- organism_to_orgdb(organism_display)

  if (!is.null(cached_go_org_db()) && identical(cached_go_orgdb_name(), orgdb_name)) {
    run_go_analysis("normal")
    return()
  }

  cache_age <- get_organism_cache_age_days(orgdb_name, debug_log = debug_log)
  if (!is.null(cache_age)) {
    debug_log(sprintf("GO annotation cache age for %s: %.1f days", orgdb_name, cache_age), 1)
  }
  decision <- isolate(go_stale_cache_decisions())[[orgdb_name]]
  stale_db <- NULL
  if (!is.null(cache_age) && cache_age > 7) {
    stale_db <- load_organism_cache(
      orgdb_name, max_cache_age_days = 7, ignore_ttl = TRUE,
      update_relocated_metadata = FALSE, debug_log = debug_log
    )
  }
  policy <- resolve_go_stale_cache_policy(cache_age, decision, !is.null(stale_db))
  if (!identical(policy, "normal")) {
    pending <- list(orgdb_name = orgdb_name, organism_display = organism_display,
                    cache_age = cache_age, stale_db = stale_db)
    if (identical(policy, "use_stale")) {
      run_go_analysis("use_stale", pending)
    } else if (identical(policy, "refresh")) {
      run_go_analysis("refresh", pending)
    } else {
      go_pending_stale_cache(pending)
      debug_log("GO annotation cache older than 7 days - awaiting user decision", 1)
      showModal(modalDialog(
          title = "Annotation Cache Outdated",
          tags$p("The local annotation cache for ", tags$b(organism_display),
                 sprintf(" is %.1f days old.", cache_age)),
          tags$p("Would you like to update the annotation database now or continue using the existing cache?"),
          footer = tagList(
            actionButton(session$ns("go_stale_cache_update"), "Update Cache", class = "btn-primary"),
            actionButton(session$ns("go_stale_cache_use_old"), "Use Existing Cache", class = "btn-default")
          ), easyClose = FALSE
        ))
    }
    return()
  }
  run_go_analysis("normal")
})

observeEvent(input$go_stale_cache_use_old, {
  pending <- isolate(go_pending_stale_cache())
  if (is.null(pending)) return()
  removeModal()
  decisions <- isolate(go_stale_cache_decisions())
  decisions[[pending$orgdb_name]] <- "use_old"
  go_stale_cache_decisions(decisions)
  go_pending_stale_cache(NULL)
  debug_log(sprintf("GO stale cache decision: use existing cache for %s", pending$orgdb_name), 1)
  run_go_analysis("use_stale", pending)
})

observeEvent(input$go_stale_cache_update, {
  pending <- isolate(go_pending_stale_cache())
  if (is.null(pending)) return()
  removeModal()
  decisions <- isolate(go_stale_cache_decisions())
  decisions[[pending$orgdb_name]] <- "update"
  go_stale_cache_decisions(decisions)
  go_pending_stale_cache(NULL)
  debug_log(sprintf("GO stale cache decision: update %s", pending$orgdb_name), 1)
  run_go_analysis("refresh", pending)
})

# ==============================================================================
# 5. Cache / Organism Management Observers
# ==============================================================================

observeEvent(input$refresh_cache, {
  withProgress(message = "Refreshing cache...", value = 0, {
    tryCatch({

      incProgress(0.1, detail = "Cleaning temporary caches")
      cleanup_temp_caches(debug_log = debug_log)

      incProgress(0.2, detail = "Cleaning corrupt AnnotationHub cache")
      clean_corrupt_annotationhub_cache(debug_log = debug_log)

      orgdb_name <- tryCatch({
        selected <- input$OrgDb_GO
        if (!is.null(selected) && is.character(selected) && nzchar(selected)) {
          organism_to_orgdb(selected)
        } else {
          "org.Hs.eg.db"
        }
      }, error = function(e) {
        debug_log(paste("Error getting organism selection, using default:", e$message), 1)
        "org.Hs.eg.db"
      })

      debug_log(paste("Starting force refresh for organism:", orgdb_name), 1)

      incProgress(0.3, detail = paste("Downloading fresh database for", orgdb_name))
      refresh_result <- force_refresh_safe(orgdb_name, debug_log = debug_log)

      incProgress(0.9, detail = "Finalizing")

      if (isTRUE(refresh_result$success) && !is.null(refresh_result$data)) {
        # Invalidate the session OrgDb cache so the next GO run picks up the
        # freshly downloaded database instead of the stale in-memory object.
        cached_go_org_db(NULL)
        cached_go_orgdb_name(NULL)
        shared_keytype_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
        shared_keytype_cache[[orgdb_name]] <- NULL
        session$userData$orgdb_keytypes_session_cache <- shared_keytype_cache
        debug_log("Session OrgDb/keytype caches invalidated after force refresh", 1)

        success_msg <- paste("Fresh annotation database for",
                             refresh_result$orgdb_name,
                             "downloaded successfully!")
        showNotification(success_msg, type = "message", duration = 8)
        debug_log("Force refresh completed successfully", 1)
      } else {
        error_msg <- if (!is.null(refresh_result$error)) {
          paste("Failed to download fresh database:", refresh_result$error)
        } else {
          "Failed to download fresh annotation database. Check your internet connection."
        }
        showNotification(error_msg, type = "error", duration = 10)
        debug_log(paste("Force refresh failed:", refresh_result$error), 1)
      }

    }, error = function(e) {
      debug_log(paste("Critical error in refresh handler:", e$message), 1)
      showNotification(
        "An unexpected error occurred during refresh. Please try again or restart the application.",
        type = "error", duration = 15
      )
    })
  })
})

observeEvent(input$update_organisms, {
  withProgress(message = "Updating organisms...", value = 0, {
    incProgress(0.2, detail = "Fetching organism list from AnnotationHub")
    result <- update_organisms_with_fresh_cache(debug_log = debug_log)

    incProgress(0.8, detail = "Updating UI")
    debug_log(paste("Result success:", result$success, "- Count:", result$organism_count), 1)

    if (is.list(result) && isTRUE(result$success)) {
      updateSelectInput(session, "OrgDb_GO",
                        choices  = result$organism_choices,
                        selected = "Homo sapiens")

      showNotification(
        paste("SUCCESS! Loaded", result$organism_count, "organisms!"),
        type = "message", duration = 5
      )
    } else {
      showNotification(
        paste("Failed:", result$error),
        type = "error", duration = 5
      )
    }
  })
})

observeEvent(input$clear_cache_go, {
  withProgress(message = "Clearing cache...", value = 0, {
    tryCatch({

      orgdb_name <- tryCatch({
        selected <- input$OrgDb_GO
        if (!is.null(selected) && is.character(selected) && nzchar(selected)) {
          organism_to_orgdb(selected)
        } else {
          "org.Hs.eg.db"
        }
      }, error = function(e) {
        debug_log(paste("Error getting organism selection, using default:", e$message), 1)
        "org.Hs.eg.db"
      })

      debug_log(paste("Clear cache requested for organism:", orgdb_name), 1)

      incProgress(0.4, detail = paste("Removing disk cache for", orgdb_name))
      removed_count <- clear_organism_cache(orgdb_name, debug_log = debug_log)

      incProgress(0.8, detail = "Invalidating session cache")
      cached_go_org_db(NULL)
      cached_go_orgdb_name(NULL)

      incProgress(1.0, detail = "Done")

      if (removed_count == 0) {
        showNotification(
          paste("No cache found for", orgdb_name, "(nothing to clear)."),
          type = "warning", duration = 5
        )
      } else {
        showNotification(
          paste("Cache cleared for", orgdb_name,
                sprintf("(%d file(s) removed).", removed_count)),
          type = "message", duration = 5
        )
      }

    }, error = function(e) {
      debug_log(paste("Error in clear cache handler:", e$message), 1)
      showNotification(
        paste("Could not clear cache (permission denied or unexpected error):", e$message),
        type = "error", duration = 8
      )
    })
  })
})

# OrgDb organism change / keytype loading observer
observeEvent(input$OrgDb_GO, {

  if (datawizard_restore_phase_active(rv)) {
    restored_keytype <- isolate(input$keyType_GO)
    if (!is.null(restored_keytype) && nzchar(as.character(restored_keytype)[1])) {
      update_keytype_select_input("keyType_GO", choices = restored_keytype, selected = restored_keytype)
    }
    debug_log("Restore phase active; preserving restored GO key type selection", 2)
    return()
  }

  if (isTRUE(keytype_loading()) || is.null(input$OrgDb_GO) || input$OrgDb_GO == "") {
    debug_log("KeyType: Skipping - invalid input or loading", 2)
    return()
  }

  organism_display <- normalize_organism_name(input$OrgDb_GO)
  orgdb_name       <- organism_to_orgdb(organism_display)
  suppress_keytype_notification <- is.null(keytype_last_organism())
  current_keytype <- input$keyType_GO
  default_keytypes <- c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")

  debug_log(paste("KeyType: Processing organism:", organism_display, "->", orgdb_name), 1)

  if (!isTRUE(go_keytype_initialized())) {
    go_keytype_initialized(TRUE)

    if (identical(orgdb_name, "org.Hs.eg.db") && isTRUE(current_keytype %in% default_keytypes)) {
      keytype_last_organism(orgdb_name)
      if (is.null(session$userData$orgdb_keytypes_session_cache) || !is.list(session$userData$orgdb_keytypes_session_cache)) {
        session$userData$orgdb_keytypes_session_cache <- list()
      }
      startup_result <- resolve_orgdb_keytypes(
        orgdb_name,
        mode = "startup",
        session_cache = session$userData$orgdb_keytypes_session_cache,
        max_keytype_cache_age_days = KEYTYPE_CACHE_MAX_AGE_DAYS,
        max_organism_cache_age_days = ORGANISM_CACHE_MAX_AGE_DAYS,
        debug_log = debug_log
      )
      shared_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
      shared_cache[[orgdb_name]] <- startup_result$keytypes
      session$userData$orgdb_keytypes_session_cache <- shared_cache
      debug_log(paste(
        "KeyType:", startup_result$message,
        "for", orgdb_name,
        "with key type", current_keytype
      ), 1)
      return()
    }
  }

  if (identical(orgdb_name, keytype_last_organism())) {
    debug_log("KeyType: Same organism, skipping", 2)
    return()
  }

  debug_log(paste("KeyType: User species change, keytype refresh running for", orgdb_name), 1)

  keytype_loading(TRUE)
  keytype_last_organism(orgdb_name)

  # Invalidate the session-level OrgDb cache when the organism changes so the
  # next GO run loads the correct database.
  if (!identical(cached_go_orgdb_name(), orgdb_name)) {
    cached_go_org_db(NULL)
    cached_go_orgdb_name(orgdb_name)
    debug_log(paste("Session OrgDb cache invalidated for organism change:", orgdb_name), 2)
  }

  tryCatch({

    if (is.null(session$userData$orgdb_keytypes_session_cache) || !is.list(session$userData$orgdb_keytypes_session_cache)) {
      session$userData$orgdb_keytypes_session_cache <- list()
    }

    result <- resolve_orgdb_keytypes(
      orgdb_name,
      mode = "user_change",
      session_cache = session$userData$orgdb_keytypes_session_cache,
      max_keytype_cache_age_days = KEYTYPE_CACHE_MAX_AGE_DAYS,
      max_organism_cache_age_days = ORGANISM_CACHE_MAX_AGE_DAYS,
      debug_log = debug_log
    )

    key_types <- result$keytypes
    if (!is.null(key_types) && length(key_types) > 0) {
      shared_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
      shared_cache[[orgdb_name]] <- key_types
      session$userData$orgdb_keytypes_session_cache <- shared_cache

      debug_log(paste(
        "KeyType: Resolved", length(key_types), "types for", orgdb_name,
        "from", result$source,
        if (!is.null(result$age_days)) paste0(" (age: ", round(result$age_days, 1), " days)") else ""
      ), 1)

      if (isTRUE(result$should_update_ui)) {
        selected_keytype <- if ("SYMBOL" %in% key_types) "SYMBOL" else key_types[1]
        update_keytype_select_input(
          "keyType_GO",
          choices = key_types,
          selected = selected_keytype
        )
      }

      if (!suppress_keytype_notification && isTRUE(result$should_update_ui)) {
        notification_type <- if (identical(result$source, "minimal")) "warning" else "message"
        showNotification(result$message, type = notification_type, duration = if (identical(notification_type, "warning")) 3 else 2)
      }
    } else {
      debug_log("KeyType: Resolver returned no keytypes; using minimal fallback", 1)
      minimal_types <- static_orgdb_keytypes(orgdb_name, minimal = TRUE)
      update_keytype_select_input(
        "keyType_GO",
        choices = minimal_types,
        selected = "SYMBOL"
      )
      if (!suppress_keytype_notification) {
        showNotification(paste("Using minimal key types for", organism_display),
                         type = "warning", duration = 3)
      }
    }

  }, finally = {
    keytype_loading(FALSE)
    debug_log(paste("KeyType: Processing completed for", orgdb_name), 2)
  })
})
