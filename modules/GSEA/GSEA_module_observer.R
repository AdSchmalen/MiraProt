# GSEA_module_observer.R
#
# Purpose:
#   Contains all observers, reactive triggers, and output definitions for the
#   GSEA module. Called from GSEA_module_server() via init_gsea_observers().
#
# Architecture:
#   This file is sourced by GSEA_module.R. init_gsea_observers() is called
#   once inside GSEA_module_server() after init_gsea_state() has run. It
#   takes all state reactives and the Shiny session objects as parameters, and
#   registers all observe/observeEvent/output/downloadHandler calls.
#
#   No server logic exists outside of init_gsea_observers(). The only
#   moduleServer() call in the GSEA module is in GSEA_module.R.
#
# Structure:
#   1.  Result readiness output flag
#   2.  Import controls: file validation, file import handler, download button,
#       import status
#   3.  Dynamic UI updates (identifier, pathway, sample, ratio, p-value,
#       gene set file, reference value choices)
#   4.  Validation output (insufficientColumns)
#   5.  Selection summary output
#   6.  Main GSEA analysis observer (createGSEA)
#   7.  Gene set refresh observer
#   8.  Plot height observer
#   9.  Enrichment selection observer
#   10. Results table output (gsea_results_table)
#   11. Gene rankings table output
#   12. Leading edge table output
#   13. Plot creation observer (create_gsea_plot)
#   14. Debug object export observer (explicit option + DEBUG_LEVEL >= 2)
#   15. Plot download handlers (multi-format)
#   16. Plot grid integration observer
#   17. Results download handlers (CSV, XLSX, gene rankings, leading edge)
#   18. Session cleanup registration
#
# Developer notes:
#   - debug_log is passed in from the server and used throughout.
#   - DEBUG_LEVEL is passed separately for use in functions that take it
#     as a numeric parameter (e.g. gsea_debug_log).
#   - modEnv is needed for the grid integration (modEnv$add_to_grid).
#   - rank_methods is the named vector from gsea_get_rank_methods().
#   - libs_loaded is the boolean from gsea_load_required_libraries().

#' Initialise all observers and outputs for the GSEA module
#'
#' Called once from GSEA_module_server() inside moduleServer(). Registers all
#' observe(), observeEvent(), output$..., and downloadHandler() calls.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rv reactiveValues; shared application state.
#' @param ns Namespace function from session$ns.
#' @param state Named list returned by init_gsea_state().
#' @param modEnv Environment; the module environment for grid integration.
#' @param DEBUG_LEVEL Integer; numeric debug level for legacy helper calls.
#' @param debug_log Function(message, level); the server's debug logger.
#' @param rank_methods Named character vector from gsea_get_rank_methods().
#' @param libs_loaded Logical; TRUE if critical libraries are available.
init_gsea_observers <- function(input, output, session, rv, ns, state,
                                 modEnv, DEBUG_LEVEL, debug_log,
                                 rank_methods, libs_loaded) {

  # Module-level constant for the Sheet 5 name used by the GSEA export system
  GSEA_SHEET5_NAME <- "GSEA_Analysis"

  # Unpack state for convenience
  data_modified               <- state$data_modified
  df_data_definition_post_mod <- state$df_data_definition_post_mod
  imputation_list             <- state$imputation_list
  res_GSEA                    <- state$res_GSEA
  current_rankings            <- state$current_rankings
  selected_enrichment         <- state$selected_enrichment
  plot_height                 <- state$plot_height
  current_plot                <- state$current_plot
  plot_recreation_state       <- state$plot_recreation_state
  last_workers_requested      <- state$last_workers_requested
  last_workers_effective      <- state$last_workers_effective
  analysis_metadata           <- state$analysis_metadata
  imported_gsea_results       <- state$imported_gsea_results
  import_status_message       <- state$import_status_message

  # ============================================================
  # 1. Result Readiness Flag
  # ============================================================

  output$res_GSEA_ready <- reactive({
    !is.null(res_GSEA()) &&
      is.list(res_GSEA()) &&
      "Results" %in% names(res_GSEA()) &&
      (inherits(res_GSEA()$Results, "gseaResult") ||
         inherits(res_GSEA()$Results, "enrichResult"))
  })
  outputOptions(output, "res_GSEA_ready", suspendWhenHidden = FALSE)

  # ============================================================
  # 2. Import Controls
  # ============================================================

  output$gsea_import_controls <- renderUI({
    req(res_GSEA())
    gsea_obj <- res_GSEA()

    type <- if (!is.null(gsea_obj$analysis_metadata$analysis_type)) {
      gsea_obj$analysis_metadata$analysis_type
    } else "unknown"

    n_terms   <- if (!is.null(gsea_obj$Results@result)) nrow(gsea_obj$Results@result) else 0
    n_sig     <- sum(gsea_obj$Results@result$p.adjust < 0.05, na.rm = TRUE)
    timestamp <- if (!is.null(gsea_obj$analysis_metadata$analysis_timestamp)) {
      as.character(gsea_obj$analysis_metadata$analysis_timestamp)
    } else "unknown"

    if (type == "imported_GSEA_result") {
      box_color <- "bg-primary-subtle border-primary"
      box_icon  <- icon("file-import")
      box_title <- "Imported GSEA Result"
      box_text  <- paste0("This GSEA result was imported and contains ",
                          n_terms, " pathways (", n_sig, " significant).<br>",
                          "Imported on: <b>", timestamp, "</b>")
    } else if (type == "original_calculation") {
      box_color <- "bg-success-subtle border-success"
      box_icon  <- icon("flask")
      box_title <- "Calculated GSEA Result"
      box_text  <- paste0("This result was generated in the current session.<br>",
                          n_terms, " pathways (", n_sig, " significant).<br>",
                          "Calculated on: <b>", timestamp, "</b>")
    } else {
      box_color <- "bg-warning-subtle border-warning"
      box_icon  <- icon("circle-question")
      box_title <- "Active GSEA Result"
      box_text  <- paste0("Active GSEA result with ", n_terms, " pathways.<br>",
                          "Type: <b>", type, "</b>")
    }

    tagList(div(
      class = paste("p-3 rounded-3 shadow-sm", box_color),
      div(class = "d-flex align-items-center mb-2",
          box_icon,
          tags$strong(class = "ms-2", box_title)),
      HTML(box_text)
    ))
  })

  # File validation observer
  observe({
    req(input$gsea_import_file)
    file_info <- input$gsea_import_file
    if (!is.null(file_info)) {
      file_ext <- tolower(tools::file_ext(file_info$name))
      if (file_ext == "rds") {
        import_status_message("RDS file selected")
        return()
      }
      tryCatch({
        sheet_names <- openxlsx::getSheetNames(file_info$datapath)
        debug_log(paste("Found sheets:", paste(sheet_names, collapse = ", ")), 2)

        has_sheet5_gsea <- is_single_sheet_gsea(file_info$datapath, sheet_name = GSEA_SHEET5_NAME)
        debug_log(paste("is_single_sheet_gsea result:", has_sheet5_gsea), 1)

        if (GSEA_SHEET5_NAME %in% sheet_names && !has_sheet5_gsea) {
          debug_log("Manual Sheet 5 verification triggered", 1)
          tryCatch({
            sheet_data        <- read_xlsx_preserve_names(file_info$datapath, sheet = GSEA_SHEET5_NAME, startRow = 1)
            manual_signatures <- c("GSEA Params", "GSEA Slots", "Integrity Digests")
            manual_found      <- sum(manual_signatures %in% names(sheet_data))
            manual_chunked    <- any(grepl("^Ranking Vector \\(\\d+\\)$", names(sheet_data))) ||
              any(grepl("^All Gene Sets \\(\\d+\\)$", names(sheet_data)))
            if (manual_found >= 2 || manual_chunked) {
              debug_log("Manual check confirms Sheet 5 format", 1)
              has_sheet5_gsea <- TRUE
            }
          }, error = function(e) {
            debug_log(paste("Manual Sheet 5 check failed:", e$message), 1)
          })
        }

        has_legacy_gsea <- any(grepl("GSEA", sheet_names, ignore.case = TRUE))

        if (has_sheet5_gsea) {
          import_status_message("Valid GSEA Excel file detected (Sheet 5 format)")
          debug_log("Valid Sheet 5 GSEA Excel file uploaded", 1)
        } else if (has_legacy_gsea) {
          import_status_message("Valid GSEA Excel file detected (legacy format)")
          debug_log("Valid legacy GSEA Excel file uploaded", 2)
        } else {
          import_status_message("No GSEA Analysis sheet found in file")
          debug_log("Invalid GSEA Excel file - no GSEA Analysis sheet", 1)
        }
      }, error = function(e) {
        import_status_message("Cannot read Excel file")
        debug_log(paste("Error reading uploaded file:", e$message), 1)
      })
    } else {
      import_status_message("")
    }
  })

  # Import button handler
  observeEvent(input$gsea_import_file, {
    req(input$gsea_import_file)
    file <- input$gsea_import_file$datapath
    tryCatch({
      res_GSEA(load_res_GSEA(file, debug_log = debug_log))
      n_sig_imported <- tryCatch(nrow(as.data.frame(res_GSEA()$Results)), error = function(e) NA_integer_)
      debug_log(
        sprintf(
          "GSEA result imported | File: %s | Significant terms: %s",
          input$gsea_import_file$name,
          as.character(n_sig_imported)
        ),
        level = 0
      )
      showNotification("GSEA result imported successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Import error:", e$message), type = "error")
    })
  })

  output$download_res_gsea_ui <- renderUI({
    if (!is.null(res_GSEA())) {
      downloadButton(ns("download_res_GSEA"),
                     label = "Download res_GSEA (.rds)",
                     class = "btn btn-primary",
                     style = "width: 100%;")
    } else {
      tags$button(class = "btn btn-primary", style = "width: 100%;",
                  disabled = "disabled", "Download res_GSEA (.rds)")
    }
  })

  output$download_res_GSEA <- downloadHandler(
    filename = function() paste0("res_GSEA_", Sys.Date(), ".rds"),
    content  = function(file) {
      if (is.null(res_GSEA())) {
        showNotification("No GSEA results available to download.", type = "error", duration = 4)
        shiny::req(FALSE)
      }
      default_fname <- paste0("res_GSEA_", Sys.Date(), ".rds")
      save_res_GSEA(res_GSEA(), file, debug_log = debug_log)
      debug_log(
        sprintf("GSEA result exported | Default file name: %s", default_fname),
        level = 0
      )
    },
    contentType = "application/octet-stream"
  )

  output$gsea_import_status <- renderText({
    import_status_message()
  })

  # ============================================================
  # 3. Dynamic UI Updates
  # ============================================================

  normalize_gsea_metadata_content <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
  }

  has_gsea_metadata_ready_content <- function(def) {
    if (is.null(def) || !is.data.frame(def) || nrow(def) == 0 ||
        !"Content" %in% names(def)) {
      return(FALSE)
    }

    content <- normalize_gsea_metadata_content(def$Content)
    content <- content[!is.na(content) & nzchar(content)]
    if (length(content) == 0) return(FALSE)

    meaningful_content <- setdiff(unique(content), "Row Index")
    has_meaningful_assignments <- length(meaningful_content) > 0

    usable_content_types <- normalize_gsea_metadata_content(c(
      "Identifier", "Abundance Ratio",
      "Normalized Abundance", "Imputed Raw Abundance",
      "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance", "Batch Corrected Raw Abundance",
      "Batch Corrected Normalized Abundance", "Raw Abundance"
    ))
    has_usable_gsea_content <- any(content %in% usable_content_types)

    only_row_index <- length(unique(content)) == 1 && identical(unique(content), "Row Index")
    row_index_is_only_column <- only_row_index && nrow(def) == 1

    has_meaningful_assignments || has_usable_gsea_content || row_index_is_only_column
  }

  last_gsea_metadata_choice_signature <- reactiveVal(NULL)

  gsea_metadata_revision_signature <- function() {
    rev_candidates <- c(
      "data_def_revision_id",
      "metadata_revision_id",
      "datawizard_metadata_revision_id"
    )
    rev_values <- vapply(rev_candidates, function(name) {
      value <- rv[[name]]
      if (is.null(value)) return(NA_character_)
      paste(as.character(value), collapse = ",")
    }, character(1), USE.NAMES = TRUE)
    paste(paste(names(rev_values), rev_values, sep = "="), collapse = "|")
  }

  gsea_metadata_choice_signature <- function(def) {
    if (is.null(def) || !is.data.frame(def)) {
      return("<no-metadata>")
    }

    signature_columns <- intersect(c("Content", "Sample", "Options"), names(def))
    column_signature <- if (length(signature_columns) == 0) {
      "<no-choice-columns>"
    } else {
      paste(
        vapply(signature_columns, function(col) {
          values <- normalize_gsea_metadata_content(def[[col]])
          values[is.na(values)] <- "<NA>"
          paste0(col, "=", paste(values, collapse = "\r"))
        }, character(1), USE.NAMES = FALSE),
        collapse = "\n"
      )
    }

    paste(
      paste0("nrow=", nrow(def)),
      paste0("ncol=", ncol(def)),
      paste0("metadata_rev=", gsea_metadata_revision_signature()),
      paste0("reference=", input$RefenceValues_GSEA %||% "<unset>"),
      paste0("ratio=", input$AbundanceRatio_GSEA_precalc %||% "<unset>"),
      column_signature,
      sep = "\n"
    )
  }

  refresh_gsea_metadata_choices <- function(def, reason = "metadata update") {
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log(paste("Metadata assignment pending; deferring GSEA metadata choices (", reason, ")", sep = ""), 2)
      return(invisible(FALSE))
    }
    if (!has_gsea_metadata_ready_content(def)) {
      debug_log(paste("Deferred GSEA metadata choices until Data Wizard metadata is assigned (", reason, ")", sep = ""), 2)
      return(invisible(FALSE))
    }

    signature <- gsea_metadata_choice_signature(def)
    pending_ui_restore <- tryCatch(rv$gsea_pending_ui_restore, error = function(e) NULL)
    if (!is.list(pending_ui_restore)) pending_ui_restore <- list()
    if (identical(signature, last_gsea_metadata_choice_signature()) && length(pending_ui_restore) == 0L) {
      debug_log(paste("Skipped unchanged GSEA metadata choices (", reason, ")", sep = ""), 3)
      return(invisible(FALSE))
    }
    last_gsea_metadata_choice_signature(signature)

    choose_restored_or_current <- function(id, current_value, choices, multiple = FALSE) {
      pending_value <- pending_ui_restore[[id]]
      if (!is.null(pending_value)) {
        pending_value <- as.character(pending_value)
        pending_value <- pending_value[!is.na(pending_value) & pending_value %in% choices]
        if (length(pending_value) > 0L) {
          pending_ui_restore[[id]] <<- NULL
          return(if (multiple) pending_value else pending_value[[1]])
        }
      }
      current_value <- as.character(current_value %||% character(0))
      current_value <- current_value[!is.na(current_value) & current_value %in% choices]
      if (length(current_value) > 0L) {
        return(if (multiple) current_value else current_value[[1]])
      }
      if (multiple) character(0) else if (length(choices) > 0L) choices[[1]] else NULL
    }

    central_id_choices <- rv$datawizard_identifier_option_choices %||% character(0)
    id_choices <- if (length(central_id_choices) > 0) central_id_choices else gsea_get_identifier_choices(def)
    if (length(id_choices) > 0) {
      selected_identifier <- choose_restored_or_current("Identifier_GSEA", input$Identifier_GSEA, id_choices)
      updateSelectInput(session, "Identifier_GSEA", choices = id_choices, selected = selected_identifier)
      debug_log(paste("Updated identifier choices:", length(id_choices), "selected:", selected_identifier), 2)
    }

    ratio_choices <- gsea_get_ratio_choices(def)
    selected_ratio <- input$AbundanceRatio_GSEA_precalc
    if (length(ratio_choices) > 0) {
      selected_ratio <- choose_restored_or_current("AbundanceRatio_GSEA_precalc", input$AbundanceRatio_GSEA_precalc, ratio_choices)
      updateSelectInput(session, "AbundanceRatio_GSEA_precalc",
                        choices = ratio_choices,
                        selected = selected_ratio)
      debug_log(paste("Updated ratio choices:", length(ratio_choices)), 2)
    }

    pval_choices <- gsea_get_pvalue_choices(def, selected_ratio)
    if (length(pval_choices) > 0) {
      selected_pval <- choose_restored_or_current("pVal_GSEA_precalc", input$pVal_GSEA_precalc, pval_choices)
      updateSelectInput(session, "pVal_GSEA_precalc",
                        choices = pval_choices,
                        selected = selected_pval)
      debug_log(paste("Updated p-value choices:", length(pval_choices)), 2)
    }

    exact_abundance_types <- c(
      "Normalized Abundance", "Imputed Raw Abundance",
      "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance", "Batch Corrected Raw Abundance",
      "Batch Corrected Normalized Abundance", "Raw Abundance"
    )
    content_all <- normalize_gsea_metadata_content(def$Content)
    content_choices <- unique(content_all[!is.na(content_all) & content_all != ""])
    ref_candidates <- content_choices[content_choices %in% normalize_gsea_metadata_content(exact_abundance_types)]
    if (length(ref_candidates) == 0) {
      non_row_index_choices <- setdiff(content_choices, "Row Index")
      ref_candidates <- if (length(non_row_index_choices) > 0) non_row_index_choices else content_choices
    }
    if (length(ref_candidates) > 0) {
      selected_ref <- choose_restored_or_current("RefenceValues_GSEA", input$RefenceValues_GSEA, ref_candidates)
      updateSelectInput(session, "RefenceValues_GSEA",
                        choices = ref_candidates,
                        selected = selected_ref)
      debug_log(paste("Updated reference value choices:", paste(ref_candidates, collapse = ", ")), 2)

      sample_choices <- gsea_get_sample_choices(def, selected_ref)
      if (length(sample_choices) > 0) {
        selected_numerator <- choose_restored_or_current("numeratorSel_GSEA", input$numeratorSel_GSEA, sample_choices, multiple = TRUE)
        selected_denominator <- choose_restored_or_current("denominatorSel_GSEA", input$denominatorSel_GSEA, sample_choices, multiple = TRUE)
        updateSelectizeInput(session, "numeratorSel_GSEA", choices = sample_choices, selected = selected_numerator, server = TRUE)
        updateSelectizeInput(session, "denominatorSel_GSEA", choices = sample_choices, selected = selected_denominator, server = TRUE)
        debug_log(paste("Updated sample choices:", length(sample_choices)), 2)
      }
    }

    tryCatch({
      rv$gsea_pending_ui_restore <- if (length(pending_ui_restore) > 0L) pending_ui_restore else NULL
    }, error = function(e) NULL)
    invisible(TRUE)
  }

  observe({
    req(df_data_definition_post_mod())
    refresh_gsea_metadata_choices(df_data_definition_post_mod(), "metadata observer")
  })

  observe({
    results <- res_GSEA()
    if (!is.null(results) && !is.null(results$Results)) {
      results_df <- as.data.frame(results$Results)
      if (nrow(results_df) > 0) {
        pathway_choices <- results_df$Description
        pending_ui_restore <- tryCatch(rv$gsea_pending_ui_restore, error = function(e) NULL)
        pending_pathways <- if (is.list(pending_ui_restore)) pending_ui_restore$custom_Enrich_select else NULL
        selected_pathways <- as.character(pending_pathways %||% isolate(input$custom_Enrich_select) %||% character(0))
        selected_pathways <- selected_pathways[!is.na(selected_pathways) & selected_pathways %in% pathway_choices]
        if (length(selected_pathways) > 0L && is.list(pending_ui_restore)) {
          pending_ui_restore$custom_Enrich_select <- NULL
          tryCatch({ rv$gsea_pending_ui_restore <- if (length(pending_ui_restore) > 0L) pending_ui_restore else NULL }, error = function(e) NULL)
        }
        updateSelectInput(session, "custom_Enrich_select",
                          choices  = pathway_choices,
                          selected = selected_pathways)
        debug_log(paste("Updated pathway choices:", length(pathway_choices), "pathways"), 1)
      } else {
        updateSelectizeInput(session, "custom_Enrich_select", choices = character(0), server = TRUE)
        debug_log("No significant pathways found", 1)
      }
    }
  })

  gmt_file_choices <- reactiveVal(gsea_list_gmt_files("./GSEA"))
  gmt_file_choices_initialized <- reactiveVal(FALSE)

  output$geneSetFileControl_GSEA <- renderUI({
    files <- gmt_file_choices()
    current_selection <- isolate(input$fileSelector_GSEA)
    selected_file <- if (!is.null(current_selection) && current_selection %in% files) {
      current_selection
    } else if (length(files) > 0L) {
      files[[1]]
    } else {
      NULL
    }

    gsea_gene_set_file_control_ui(session$ns, files, selected_file)
  })

  update_gmt_file_choices <- function(force_refresh = FALSE, notify = FALSE) {
    files <- gsea_list_gmt_files("./GSEA", force_refresh = force_refresh)
    gmt_file_choices(files)
    pending_ui_restore <- tryCatch(rv$gsea_pending_ui_restore, error = function(e) NULL)
    pending_file <- if (is.list(pending_ui_restore)) pending_ui_restore$fileSelector_GSEA else NULL
    selected_file <- as.character(pending_file %||% input$fileSelector_GSEA %||% character(0))
    selected_file <- selected_file[!is.na(selected_file) & selected_file %in% files]
    if (length(selected_file) > 0L && is.list(pending_ui_restore)) {
      pending_ui_restore$fileSelector_GSEA <- NULL
      tryCatch({ rv$gsea_pending_ui_restore <- if (length(pending_ui_restore) > 0L) pending_ui_restore else NULL }, error = function(e) NULL)
    }
    updateSelectInput(session, "fileSelector_GSEA",
                      choices  = files,
                      selected = if (length(selected_file) > 0L) selected_file[[1]] else if (length(files) > 0) files[1] else NULL)
    debug_log(paste("Loaded gene set choices:", length(files), "GMT files found"), 2)
    if (notify) {
      showNotification(paste("Found", length(files), "gene set files"),
                       type = "message", duration = 3)
    }
    invisible(files)
  }

  observeEvent(datawizard_import_ready_signature(rv), {
    if (datawizard_import_barrier_active(rv)) {
      debug_log("Import barrier active; preserving GSEA choices until ready", 2)
      return()
    }
    def <- isolate(df_data_definition_post_mod())
    req(def)
    if (isTRUE(gmt_file_choices_initialized())) return()

    if (!has_gsea_metadata_ready_content(def)) {
      # Keep the global GMT cache warm when possible. gsea_list_gmt_files() is
      # cheap after the first scan because it uses the process-wide cache, but
      # do not update the GSEA UI while Data Wizard is still exposing the
      # transient raw-load metadata skeleton (typically only Row Index).
      gsea_list_gmt_files("./GSEA", force_refresh = FALSE)
      debug_log("Deferred GSEA GMT/UI initialization until Data Wizard metadata is assigned", 2)
      return()
    }

    update_gmt_file_choices(force_refresh = FALSE)
    gmt_file_choices_initialized(TRUE)
  }, ignoreNULL = TRUE)

  # ============================================================
  # 4. Validation Output
  # ============================================================

  output$insufficientColumns <- renderText({
    def <- df_data_definition_post_mod()
    req(has_gsea_metadata_ready_content(def))

    if (input$GSEA_type_select == "Custom Ranking") {
      req(input$RefenceValues_GSEA)
      sample_choices <- gsea_get_sample_choices(def, input$RefenceValues_GSEA)
      if (length(sample_choices) < 4) {
        return("Warning: Need at least 4 samples (2 per group) for custom ranking analysis")
      }
    } else {
      ratio_choices <- gsea_get_ratio_choices(def)
      pval_choices  <- gsea_get_pvalue_choices(def, input$AbundanceRatio_GSEA_precalc)
      if (length(ratio_choices) == 0) return("Warning: No abundance ratio columns found")
      if (length(pval_choices)  == 0) return("Warning: No p-value columns found")
    }
    ""
  })

  # ============================================================
  # 5. Selection Summary Output
  # ============================================================

  output$selection_summary <- renderText({
    pathways  <- input$custom_Enrich_select
    plot_type <- input$plot_type_GSEA
    if (is.null(pathways) || length(pathways) == 0) return("No pathways selected")
    paste0("Plot Type: ", plot_type, "\n",
           "Selected Pathways: ", length(pathways), "\n",
           "First pathway: ", pathways[1],
           if (length(pathways) > 1) paste0("\n(and ", length(pathways) - 1, " more)"))
  })

  # ============================================================
  # 6. Main GSEA Analysis Observer
  # ============================================================

  observeEvent(input$createGSEA, {
    t_total <- proc.time()[3]

    withProgress(message = "GSEA Analysis", value = 0, {
      debug_log("Starting GSEA workflow", 1)

      req(df_data_definition_post_mod(), data_modified(),
          input$Identifier_GSEA, input$fileSelector_GSEA)

      setProgress(value = 0.1, detail = "Preparing data...")
      def <- df_data_definition_post_mod()
      raw <- data_modified()

      if (is.null(raw) || nrow(raw) == 0) {
        debug_log("No data available for GSEA", 1)
        showNotification("No data available for GSEA analysis", type = "error")
        return()
      }

      setProgress(value = 0.2, detail = "Computing rankings...")

      if (input$GSEA_type_select == "Custom Ranking") {
        debug_log("Using custom ranking method", 2)
        req(input$numeratorSel_GSEA, input$denominatorSel_GSEA,
            input$RefenceValues_GSEA, input$RankinkMethod_GSEA)

        nums <- input$numeratorSel_GSEA
        dens <- input$denominatorSel_GSEA

        if (length(nums) < 2 || length(dens) < 2) {
          debug_log("Insufficient samples in groups", 1)
          showNotification("Need at least 2 samples per group", type = "error")
          return()
        }

        debug_log(paste("Numerator samples:", length(nums), "Denominator:", length(dens)), 2)

        method <- rank_methods[input$RankinkMethod_GSEA]
        if (is.na(method)) {
          debug_log("Invalid ranking method selected", 1)
          showNotification("Invalid ranking method", type = "error")
          return()
        }

        valid_n_raw <- input$gsea_min_valid_values
        valid_n_num <- suppressWarnings(as.integer(
          if (is.null(valid_n_raw) || trimws(as.character(valid_n_raw)) == "") NA else valid_n_raw
        ))
        valid_g_val <- if (is.null(input$gsea_validation_rule) ||
                           !nzchar(input$gsea_validation_rule)) {
          "In one group"
        } else {
          input$gsea_validation_rule
        }

        debug_log(paste0("valid_n=", ifelse(is.na(valid_n_num), "NA", valid_n_num),
                          " | valid_g=", valid_g_val), 1)

        impute_arg <- if (is.null(imputation_list) || is.null(imputation_list())) NULL
                      else imputation_list()

        ranking_result <- compute_custom_ranks_GSEA(
          raw, def, nums, dens,
          input$Identifier_GSEA, input$RefenceValues_GSEA,
          method,
          input$absolute_GSEA,
          input$ties_GSEA,
          input$PADOG_GSEA,
          file.path("./GSEA", input$fileSelector_GSEA),
          valid_n_num,
          valid_g_val,
          impute_arg,
          DEBUG_LEVEL
        )

      } else {
        debug_log("Using precalculated ranking method", 2)
        req(input$AbundanceRatio_GSEA_precalc, input$pVal_GSEA_precalc,
            input$Identifier_GSEA, input$RankingMetric_GSEA_precalc)

        ranking_result <- compute_precalculated_ranks_GSEA(
          raw, def,
          input$AbundanceRatio_GSEA_precalc,
          input$pVal_GSEA_precalc,
          input$Identifier_GSEA,
          input$RankingMetric_GSEA_precalc,
          input$absolute_GSEA_precalc,
          input$ties_GSEA_precalc,
          input$PADOG_GSEA_precalc,
          file.path("./GSEA", input$fileSelector_GSEA),
          DEBUG_LEVEL
        )
      }

      if (is.null(ranking_result)) {
        debug_log("Ranking computation failed", 1)
        showNotification("Failed to compute gene rankings", type = "error")
        return()
      }

      ranks_vec <- ranking_result$Ranks
      fc_vec    <- ranking_result$FC

      debug_log(paste("Ranking completed with", length(ranks_vec), "genes"), 1)
      setProgress(value = 0.4, detail = "Validating rankings...")

      if (!validate_ranking_vector(ranks_vec, DEBUG_LEVEL)) {
        showNotification("Invalid ranking vector: insufficient variability", type = "error")
        return()
      }

      current_rankings(ranking_result)
      setProgress(value = 0.5, detail = "Running GSEA...")

      gene_set_file <- file.path("./GSEA", input$fileSelector_GSEA)
      if (!file.exists(gene_set_file)) {
        debug_log("Gene set file not found", 1)
        showNotification("Gene set file not found", type = "error")
        return()
      }

      # Worker auto-detection is deliberately per-run. Do not feed the worker
      # count observed on an earlier run back in as a requested limit: a small
      # workload or temporary sequential fallback must not constrain this run.
      requested_cores <- NULL
      last_workers_requested(requested_cores)

      gsea_result <- run_gsea_analysis(
        gene_list        = ranks_vec,
        gene_set_file    = gene_set_file,
        num_permutations = input$numPermutations_GSEA,
        p_value_cutoff   = 0.05,
        DEBUG_LEVEL      = DEBUG_LEVEL,
        requested_cores  = requested_cores,
        debug_log        = debug_log
      )

      if (is.null(gsea_result)) {
        debug_log("GSEA analysis failed", 1)
        res_GSEA(NULL)
        updateSelectizeInput(session, "custom_Enrich_select", choices = character(0), server = TRUE)
        showNotification("GSEA analysis failed", type = "error")
        return()
      }

      setProgress(value = 0.9, detail = "Processing results...")

      workers_used <- attr(gsea_result, "gsea_workers_used", exact = TRUE)
      if (!is.null(workers_used) && !is.na(suppressWarnings(as.integer(workers_used)))) {
        last_workers_effective(as.integer(workers_used))
        debug_log(sprintf("GSEA used %d worker(s)", as.integer(workers_used)), 2)
      } else {
        last_workers_effective(NULL)
      }

      current_metadata <- c(
        list(
          gmt_file          = input$fileSelector_GSEA %||% "UNKNOWN",
          gmt_full_path     = file.path("./GSEA", input$fileSelector_GSEA %||% ""),
          analysis_params   = list(
            pvalueCutoff   = 0.05,
            pAdjustMethod  = "BH",
            minGSSize      = 10,
            maxGSSize      = 500,
            eps            = 0,
            nPermSimple     = input$numPermutations_GSEA %||% 1000
          ),
          analysis_timestamp = Sys.time(),
          analysis_type      = "original_calculation"
        ),
        if (input$GSEA_type_select == "Custom Ranking") {
          list(
            ranking_type        = "Custom Ranking",
            ranking_method_name = input$RankinkMethod_GSEA
          )
        } else {
          list(
            ranking_type   = "Precalculated Ranking",
            ab_ratio_col   = input$AbundanceRatio_GSEA_precalc,
            pval_col       = input$pVal_GSEA_precalc,
            ranking_metric = input$RankingMetric_GSEA_precalc
          )
        }
      )
      analysis_metadata(current_metadata)
      debug_log(paste("Analysis metadata captured - GMT:", current_metadata$gmt_file), 1)

      complete_result <- list(
        Results          = gsea_result,
        GeneList         = ranks_vec,
        GeneList_FC      = fc_vec,
        source           = "calculated",
        analysis_metadata = current_metadata
      )

      debug_log(paste("GSEA result structure:", paste(names(complete_result), collapse = ", ")), 1)
      res_GSEA(complete_result)

      db_l0        <- tryCatch(current_metadata$gmt_file %||% current_metadata$gene_set, error = function(e) NA_character_)
      ranking_type <- tryCatch(current_metadata$ranking_type, error = function(e) NA_character_)
      pval_cut_l0  <- tryCatch(as.character(current_metadata$analysis_params$pvalueCutoff),    error = function(e) NA_character_)
      padj_l0      <- tryCatch(as.character(current_metadata$analysis_params$pAdjustMethod),   error = function(e) NA_character_)
      mingss_l0    <- tryCatch(as.character(current_metadata$analysis_params$minGSSize),       error = function(e) NA_character_)
      maxgss_l0    <- tryCatch(as.character(current_metadata$analysis_params$maxGSSize),       error = function(e) NA_character_)
      nperm_l0     <- tryCatch(as.character(
        current_metadata$analysis_params$nPermSimple %||%
          current_metadata$analysis_params$numPermutations
      ), error = function(e) NA_character_)
      n_input_l0   <- tryCatch(length(complete_result$GeneList), error = function(e) NA_integer_)
      ranking_desc_l0 <- if (isTRUE(ranking_type == "Custom Ranking")) {
        sprintf("Ranking: Custom Ranking | Method: %s",
                tryCatch(current_metadata$ranking_method_name, error = function(e) NA_character_))
      } else if (isTRUE(ranking_type == "Precalculated Ranking")) {
        sprintf("Ranking: Precalculated Ranking | Abundance ratio column: %s | p-value column: %s | Ranking metric: %s",
                tryCatch(current_metadata$ab_ratio_col,   error = function(e) NA_character_),
                tryCatch(current_metadata$pval_col,       error = function(e) NA_character_),
                tryCatch(current_metadata$ranking_metric, error = function(e) NA_character_))
      } else {
        sprintf("Analysis type: %s", ranking_type)
      }
      debug_log(
        sprintf(
          paste0(
            "GSEA analysis inputs",
            " | Gene set file: %s",
            " | %s",
            " | p-value cutoff: %s",
            " | p-adjust method: %s",
            " | Min GS size: %s",
            " | Max GS size: %s",
            " | nPermSimple: %s",
            " | Input genes: %d"
          ),
          db_l0, ranking_desc_l0, pval_cut_l0, padj_l0,
          mingss_l0, maxgss_l0, nperm_l0, n_input_l0
        ),
        level = 0
      )
      n_ranked_l0  <- n_input_l0
      n_in_sets_l0 <- tryCatch({
        gs_genes <- unique(unlist(complete_result$Results@geneSets))
        sum(names(complete_result$GeneList) %in% gs_genes)
      }, error = function(e) NA_integer_)
      n_sig_l0     <- tryCatch(nrow(as.data.frame(complete_result$Results)), error = function(e) NA_integer_)
      debug_log(
        sprintf(
          paste0(
            "GSEA analysis outcome",
            " | Genes ranked: %d",
            " | Genes recognized in gene sets: %d",
            " | Significant pathways: %d"
          ),
          n_ranked_l0, n_in_sets_l0, n_sig_l0
        ),
        level = 0
      )

      if (!is.null(gsea_result) && nrow(as.data.frame(gsea_result)) > 0) {
        enrichment_choices <- as.data.frame(gsea_result)$Description
        updateSelectizeInput(session, "custom_Enrich_select",
                             choices  = enrichment_choices,
                             selected = enrichment_choices[1],
                             server = TRUE)
        plot_inputs <- get_gsea_plot_input_values(selected_pathways_override = enrichment_choices[1])
        if (isTRUE(do.call(create_gsea_plot_from_current_inputs, plot_inputs))) {
          debug_log("Initial GSEA plot created after analysis", 1)
        }
        sig_count <- nrow(as.data.frame(gsea_result))
        showNotification(paste("GSEA completed:", sig_count, "significant terms found"),
                         type = "message", duration = 5)

        # Store original S4 object for development comparison (guarded)
        if (DEBUG_LEVEL >= 2) {
          tryCatch({
            original_s4 <- res_GSEA()$Results
            if (methods::is(original_s4, "gseaResult")) {
              assign("original_gsea_for_comparison", original_s4, envir = modEnv)
              debug_log("Stored original S4 object for comparison", 2)
            }
          }, error = function(e) {
            debug_log(paste("Could not store original S4:", e$message), 2)
          })
        }
      } else {
        updateSelectizeInput(session, "custom_Enrich_select", choices = character(0), server = TRUE)
        showNotification("No significant GSEA terms identified", type = "warning", duration = 5)
      }

      setProgress(value = 1.0, detail = "Complete")
    })

    total_sec <- round(proc.time()[3] - t_total, 1)
    debug_log(sprintf("GSEA analysis total time: %.1fs", total_sec), 1)
  })

  # ============================================================
  # 7. Gene Set Refresh Observer
  # ============================================================

  observeEvent(input$refresh_GeneSets, {
    files <- update_gmt_file_choices(force_refresh = TRUE, notify = TRUE)
    debug_log(paste("Refreshed gene sets:", length(files), "files found"), 1)
  })

  # ============================================================
  # 8. Plot Height Observer
  # ============================================================

  observeEvent(input$plot_height_gsea, {
    if (!is.null(input$plot_height_gsea) && input$plot_height_gsea >= 300) {
      plot_height(input$plot_height_gsea)
    }
  })

  # ============================================================
  # 8.5. Reset controls with static UI defaults
  # ============================================================

  observeEvent(input$resetButton_GSEA, {
    tryCatch({
      debug_log("Resetting GSEA controls to UI defaults", 1)

      # Left-side plot selector and Plot Customization panel.
      # Metadata/result-populated inputs that start with choices = NULL are
      # intentionally not touched here.
      updateSelectInput(session, "plot_type_GSEA", selected = "General Running Score Plot")
      colourpicker::updateColourInput(session, "GSEAColorInput_down", value = "#440154FF")
      colourpicker::updateColourInput(session, "GSEAColorInput_zero", value = "#31688EFF")
      colourpicker::updateColourInput(session, "GSEAColorInput_up", value = "#EFC000FF")
      updateNumericInput(session, "AxisTitleSize_GSEA", value = 12)
      updateNumericInput(session, "tickSize_GSEA", value = 10)
      updateNumericInput(session, "LegendTextSize_GSEA", value = 10)
      updateNumericInput(session, "LegendTitleSize_GSEA", value = 12)
      updateSelectInput(session, "ThemeSelect_GSEA", selected = "Black and White")
      updateSelectInput(session, "LegendPosition_GSEA", selected = "right")
      updateNumericInput(session, "LabelSize_GSEA", value = 12)
      updateNumericInput(session, "plot_height_gsea", value = 600)
      plot_height(600)
      updateNumericInput(session, "cnet_node_size", value = 5)
      updateNumericInput(session, "cnet_layout_method", value = 1)
      updateNumericInput(session, "emap_node_size", value = 8)
      updateNumericInput(session, "emap_layout", value = 0.7)
      updateCheckboxInput(session, "dotplot_swap_panels", value = FALSE)
      updateCheckboxInput(session, "dotplot_y_ticks_right", value = FALSE)

      # Right-side GSEA configuration controls with non-NULL static defaults.
      updateRadioButtons(session, "GSEA_type_select", selected = "Custom Ranking")
      updateNumericInput(session, "numPermutations_GSEA", value = 1000)
      updateSelectInput(session, "RankinkMethod_GSEA", selected = "MWT")
      updateCheckboxInput(session, "absolute_GSEA", value = FALSE)
      updateCheckboxInput(session, "ties_GSEA", value = FALSE)
      updateCheckboxInput(session, "PADOG_GSEA", value = FALSE)
      updateNumericInput(session, "gsea_min_valid_values", value = 1)
      updateSelectInput(session, "gsea_validation_rule", selected = "In total")
      updateSelectInput(session, "RankingMetric_GSEA_precalc", selected = "log2(FC) x -log10(p)")
      updateCheckboxInput(session, "absolute_GSEA_precalc", value = FALSE)
      updateCheckboxInput(session, "ties_GSEA_precalc", value = FALSE)
      updateCheckboxInput(session, "PADOG_GSEA_precalc", value = FALSE)

      showNotification("GSEA controls reset to defaults.", type = "message", duration = 3)
      debug_log("GSEA controls reset to UI defaults", 1)
    }, error = function(e) {
      debug_log(paste("Error resetting GSEA controls:", e$message), 1)
      showNotification("Error resetting GSEA controls", type = "error", duration = 3)
    })
  })

  # ============================================================
  # 9. Enrichment Selection Observer
  # ============================================================

  observeEvent(input$custom_Enrich_select, {
    selected_enrichment(input$custom_Enrich_select)
  })

  # ============================================================
  # 10. Results Table Output
  # ============================================================

  output$gsea_results_table <- renderDT({
    req(res_GSEA())
    gsea_results <- res_GSEA()$Results
    if (is.null(gsea_results)) return(NULL)

    tryCatch({
      results_df     <- as.data.frame(gsea_results)
      available_cols <- colnames(results_df)

      column_mapping <- list(
        "Pathway"              = c("Description", "ID", "Term", "pathway"),
        "Size"                 = c("setSize", "Count", "size", "GeneCount"),
        "ES"                   = c("enrichmentScore", "ES", "score"),
        "NES"                  = c("NES", "normalizedEnrichmentScore"),
        "P-value"              = c("pvalue", "p.value", "PValue", "pval"),
        "FDR"                  = c("p.adjust", "padj", "FDR", "qvalue"),
        "Q-value"              = c("qvalues", "qvalue", "q.value", "QValue"),
        "Core Enriched Genes"  = c("core_enrichment", "coreEnrichment", "leadingEdge", "core_genes")
      )

      selected_cols <- character(); final_names <- character()
      for (display_name in names(column_mapping)) {
        found_col <- intersect(column_mapping[[display_name]], available_cols)[1]
        if (!is.na(found_col)) {
          selected_cols <- c(selected_cols, found_col)
          final_names   <- c(final_names, display_name)
        }
      }

      if (length(selected_cols) == 0) {
        selected_cols <- head(available_cols, min(7, length(available_cols)))
        final_names   <- selected_cols
        gsea_debug_log("Using fallback column selection", 1, 1)
      }

      display_df              <- results_df[, selected_cols, drop = FALSE]
      colnames(display_df)    <- final_names

      core_genes_col <- which(final_names == "Core Enriched Genes")
      if (length(core_genes_col) > 0) {
        core_genes_data <- display_df[[core_genes_col]]
        if (is.factor(core_genes_data)) core_genes_data <- as.character(core_genes_data)
        display_df[[core_genes_col]] <- sapply(core_genes_data, function(x) {
          if (is.na(x) || x == "" || x == "NA") return("No genes")
          genes <- trimws(unlist(strsplit(as.character(x), "/")))
          genes <- genes[genes != ""]
          if (length(genes) == 0) return("No genes")
          if (length(genes) > 10) {
            paste(paste(genes[1:10], collapse = ", "), "... (", length(genes) - 10, " more)")
          } else {
            paste(genes, collapse = ", ")
          }
        })
      }

      numeric_cols <- sapply(display_df, is.numeric)
      if (any(numeric_cols)) {
        for (col_name in names(display_df)[numeric_cols]) {
          col_values <- display_df[[col_name]]
          if (col_name %in% c("P-value", "FDR", "Q-value")) {
            if (any(col_values < 1e-4, na.rm = TRUE)) {
              display_df[[col_name]] <- format(col_values, scientific = TRUE, digits = 3)
            } else {
              display_df[[col_name]] <- round(col_values, 4)
            }
          } else if (col_name == "Size") {
            display_df[[col_name]] <- round(col_values, 0)
          } else {
            display_df[[col_name]] <- round(col_values, 3)
          }
        }
      }

      dt_options <- list(
        pageLength  = 10,
        scrollX     = TRUE,
        columnDefs  = list(
          list(targets   = which(sapply(display_df, is.numeric)) - 1,
               className = "dt-right")
        )
      )
      if (length(core_genes_col) > 0) {
        dt_options$columnDefs <- append(dt_options$columnDefs, list(
          list(targets = core_genes_col - 1, width = "300px", className = "dt-left"),
          list(targets = core_genes_col - 1, render = DT::JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display' && data.length > 100) {",
            "    return '<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>';",
            "  } else { return data; }",
            "}"
          ))
        ))
      }

      DT::datatable(display_df,
                    options  = dt_options,
                    rownames = FALSE,
                    caption  = paste("GSEA Results -", nrow(display_df), "pathways found"),
                    escape   = FALSE)

    }, error = function(e) {
      debug_log(paste("Error in results table:", e$message), 1)
      results_df <- as.data.frame(gsea_results)
      DT::datatable(results_df,
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    caption  = "GSEA Results (raw format due to column mapping error)")
    })
  })

  # ============================================================
  # 11. Gene Rankings Table Output
  # ============================================================

  output$gene_rankings_table <- renderDT({
    req(current_rankings())
    rankings <- current_rankings()
    if (is.null(rankings$Ranks)) return(NULL)

    top_genes      <- rankings$Ranks
    actual_ranks   <- as.numeric(top_genes)
    decimal_places <- calculate_intelligent_decimals(actual_ranks, DEBUG_LEVEL)

    rankings_df <- data.frame(
      Gene        = names(top_genes),
      Rank        = round(actual_ranks, decimal_places),
      FoldChange  = round(rankings$FC[names(top_genes)], 4),
      stringsAsFactors = FALSE
    )
    rankings_df <- rankings_df[order(-rankings_df$Rank, rankings_df$Gene), ]
    n_ties       <- sum(duplicated(rankings_df$Rank))

    DT::datatable(rankings_df,
                  options  = list(
                    pageLength = 25,
                    scrollX    = TRUE,
                    columnDefs = list(
                      list(targets = 1, className = "dt-right"),
                      list(targets = 2, className = "dt-right")
                    )
                  ),
                  rownames = FALSE,
                  caption  = if (n_ties > 0) {
                    paste("Note:", n_ties, "genes have identical rankings (ties)")
                  } else NULL)
  })

  # ============================================================
  # 12. Leading Edge Table Output
  # ============================================================

  output$leading_edge_table <- renderDT({
    req(res_GSEA(), input$custom_Enrich_select)

    tryCatch({
      gsea_results  <- res_GSEA()$Results
      selected_term <- input$custom_Enrich_select
      rankings      <- current_rankings()

      if (is.null(gsea_results) || is.null(selected_term) || is.null(rankings)) return(NULL)

      results_df <- as.data.frame(gsea_results)
      term_row   <- which(results_df$Description == selected_term)
      if (length(term_row) == 0) return(NULL)

      core_genes <- results_df$core_enrichment[term_row]
      if (is.na(core_genes) || core_genes == "") return(NULL)

      leading_genes <- unlist(strsplit(core_genes, "/"))
      gene_ranks    <- rankings$Ranks[leading_genes]
      gene_fcs      <- rankings$FC[leading_genes]

      valid_genes   <- !is.na(gene_ranks)
      leading_genes <- leading_genes[valid_genes]
      gene_ranks    <- gene_ranks[valid_genes]
      gene_fcs      <- gene_fcs[valid_genes]

      if (length(leading_genes) == 0) return(NULL)

      actual_ranks   <- as.numeric(gene_ranks)
      decimal_places <- calculate_intelligent_decimals(actual_ranks, 1)

      leading_df <- data.frame(
        Gene        = leading_genes,
        Rank        = round(actual_ranks, decimal_places),
        FoldChange  = round(as.numeric(gene_fcs), 4),
        stringsAsFactors = FALSE
      )
      leading_df <- leading_df[order(-leading_df$Rank, leading_df$Gene), ]
      n_ties      <- sum(duplicated(leading_df$Rank))

      DT::datatable(leading_df,
                    options  = list(
                      pageLength = 15,
                      scrollX    = TRUE,
                      columnDefs = list(
                        list(targets = 1, className = "dt-right"),
                        list(targets = 2, className = "dt-right")
                      )
                    ),
                    rownames = FALSE,
                    caption  = if (n_ties > 0) {
                      paste("Leading edge genes for", selected_term,
                            "- Note:", n_ties, "genes have identical rankings")
                    } else {
                      paste("Leading edge genes for", selected_term)
                    })

    }, error = function(e) {
      debug_log(paste("Error in leading edge table:", e$message), 1)
      NULL
    })
  })

  # ============================================================
  # 13. Plot Creation Observer
  # ============================================================

  get_gsea_plot_input_values <- function(selected_pathways_override = NULL) {
    plot_type <- isolate(input$plot_type_GSEA %||% "General Running Score Plot")

    colors <- if (plot_type %in% c("Cnet plot (log2FC)", "Cnet plot (Ranking Metrics)",
                                    "Heatmap (log2FC)", "Heatmap (Ranking Metrics)",
                                    "Ridgeline plot", "Enrichment map",
                                    "Enrichment score dotplot", "Running score plot",
                                    "Pubmed citations")) {
      c(isolate(input$GSEAColorInput_down %||% "blue"),
        isolate(input$GSEAColorInput_zero %||% "#EDEDED"),
        isolate(input$GSEAColorInput_up   %||% "orange"))
    } else {
      c("blue", "#EDEDED", "orange")
    }

    list(
      results            = isolate(res_GSEA()),
      selected_pathways  = selected_pathways_override %||% isolate(input$custom_Enrich_select %||% character(0)),
      plot_type          = plot_type,
      legend_position    = tryCatch(
        isolate(input$LegendPosition_GSEA %||% "right"),
        error = function(e) { debug_log(paste("Error getting legend position:", e$message), 1); "right" }
      ),
      colors             = colors,
      sizes              = list(
        axisTitle   = as.numeric(isolate(input$AxisTitleSize_GSEA   %||% 12)),
        tick        = as.numeric(isolate(input$tickSize_GSEA        %||% 10)),
        legendText  = as.numeric(isolate(input$LegendTextSize_GSEA  %||% 10)),
        legendTitle = as.numeric(isolate(input$LegendTitleSize_GSEA %||% 12)),
        label       = as.numeric(isolate(input$LabelSize_GSEA       %||% 12)),
        labelSize   = isolate(input$LabelSize_GSEA %||% 12)
      ),
      theme              = get_selected_theme(isolate(input$ThemeSelect_GSEA %||% "Black and White")),
      plot_height_val    = isolate(input$plot_height_gsea %||% 600),
      dotplot_swap_panels = isTRUE(isolate(input$dotplot_swap_panels)),
      dotplot_y_ticks_right = isTRUE(isolate(input$dotplot_y_ticks_right))
    )
  }

  create_gsea_plot_from_current_inputs <- function(results, selected_pathways, plot_type,
                                                   legend_position, colors, sizes, theme,
                                                   plot_height_val, dotplot_swap_panels,
                                                   dotplot_y_ticks_right, progress_fn = NULL) {
    req(results, plot_type)
    debug_log(paste("Creating plot:", plot_type), 1)

    plot_result <- tryCatch({
      if (plot_type == "Enrichment map" && length(selected_pathways) < 2) {
        return(list(plot = NULL, message = "Select at least 2 pathways for Enrichment map."))
      }

      selection_required <- c("Cnet plot (log2FC)", "Cnet plot (Ranking Metrics)",
                               "Running score plot", "Running score plot with p-values",
                               "Pubmed citations")
      if (plot_type %in% selection_required && length(selected_pathways) == 0) {
        return(list(plot = NULL, message = paste("Select pathways for", plot_type)))
      }

      switch(plot_type,
        "General Running Score Plot"  = {
          idx <- get_pathway_index(results, selected_pathways, single = TRUE)
          create_general_running_score_plot(results, idx, theme, sizes)
        },
        "Enrichment score dotplot"    = {
          create_enrichment_dotplot(results, selected_pathways, colors, theme, sizes,
                                    legend_position,
                                    swap_panels  = dotplot_swap_panels,
                                    y_ticks_right = dotplot_y_ticks_right)
        },
        "Cnet plot (log2FC)"          = create_cnet_plot_fc(results, selected_pathways, colors, theme, sizes, legend_position),
        "Cnet plot (Ranking Metrics)" = create_cnet_plot_ranking(results, selected_pathways, colors, theme, sizes, legend_position),
        "Enrichment map"              = create_enrichment_map(results, selected_pathways, colors, theme, sizes, legend_position),
        "Heatmap (log2FC)"            = create_heatmap_fc(results, selected_pathways, colors, theme, sizes, legend_position),
        "Heatmap (Ranking Metrics)"   = create_heatmap_ranking(results, selected_pathways, colors, theme, sizes, legend_position),
        "Ridgeline plot"              = create_ridgeline_plot(results, selected_pathways, colors, theme, sizes, legend_position),
        "Running score plot"          = {
          ids <- get_pathway_indices(results, selected_pathways)
          create_running_score_plot(results, ids, theme, sizes, legend_position, colors)
        },
        "Pubmed citations"            = create_pubmed_plot(selected_pathways, colors, theme, sizes, legend_position,
                                                            progress_fn = progress_fn),
        list(plot = NULL, message = paste("Plot type not implemented:", plot_type))
      )
    }, error = function(e) {
      debug_log(paste("Error creating plot:", e$message), 1)
      list(plot = NULL, message = paste("Error:", e$message))
    })

    if (!is.null(plot_result$plot)) {
      current_plot(plot_result$plot)
      output$GSEAplot_container <- renderUI({
        tagList(
          div(style = paste0("background-color:#d4edda;border:1px solid #c3e6cb;",
                             "border-radius:4px;padding:10px;margin-bottom:10px;color:#155724;"),
              plot_result$message),
          plotOutput(ns("GSEAplot_custom"),
                     height = paste0(plot_height_val, "px"),
                     width  = "100%")
        )
      })
      output$GSEAplot_custom <- renderPlot({ print(plot_result$plot) }, height = plot_height_val)
      debug_log(paste("GSEA plot created: type =", plot_type,
                      "– pathway:",
                      if (length(selected_pathways) > 0)
                        paste(selected_pathways, collapse = ", ")
                      else "(default)"),
                level = 0)
      return(TRUE)
    }

    output$GSEAplot_container <- renderUI(div("No plot created"))
    FALSE
  }

  gsea_restore_pending <- reactiveVal(NULL)
  gsea_restore_last_applied_signature <- reactiveVal(NULL)
  gsea_restore_observer <- NULL

  gsea_restore_unavailable <- function(default) {
    function(e) {
      if (.is_shiny_context_error(e)) stop(e)
      default
    }
  }

  gsea_value_in_choices <- function(value, choices) {
    if (is.null(value) || length(value) == 0L) return(TRUE)
    if (length(choices) == 0L) return(FALSE)
    all(as.character(value) %in% as.character(choices))
  }

  gsea_restore_pathway_choices <- function() {
    results <- tryCatch(res_GSEA(), error = gsea_restore_unavailable(NULL))
    if (is.null(results) || is.null(results$Results)) return(character(0))
    choices <- tryCatch(as.data.frame(results$Results)$Description,
                        error = gsea_restore_unavailable(character(0)))
    choices[!is.na(choices) & nzchar(choices)]
  }

  gsea_restore_metadata_choices <- function(def, saved_inputs) {
    datawizard_choices_ready <- isTRUE(tryCatch(
      rv$datawizard_metadata_choices_ready,
      error = gsea_restore_unavailable(FALSE)
    )) && !is.null(tryCatch(
      rv$datawizard_metadata_choices_revision,
      error = gsea_restore_unavailable(NULL)
    ))
    if (!datawizard_choices_ready || is.null(def) || !has_gsea_metadata_ready_content(def) || datawizard_metadata_defer_downstream_choices(rv)) {
      return(NULL)
    }

    central_id_choices <- rv$datawizard_identifier_option_choices %||% character(0)
    id_choices <- if (length(central_id_choices) > 0) central_id_choices else gsea_get_identifier_choices(def)
    ratio_choices <- gsea_get_ratio_choices(def)
    selected_ratio <- saved_inputs$AbundanceRatio_GSEA_precalc %||% input$AbundanceRatio_GSEA_precalc
    pval_choices <- gsea_get_pvalue_choices(def, selected_ratio)

    exact_abundance_types <- c(
      "Normalized Abundance", "Imputed Raw Abundance",
      "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance", "Batch Corrected Raw Abundance",
      "Batch Corrected Normalized Abundance", "Raw Abundance"
    )
    content_all <- normalize_gsea_metadata_content(def$Content)
    content_choices <- unique(content_all[!is.na(content_all) & content_all != ""])
    ref_choices <- content_choices[content_choices %in% normalize_gsea_metadata_content(exact_abundance_types)]
    if (length(ref_choices) == 0L) {
      non_row_index_choices <- setdiff(content_choices, "Row Index")
      ref_choices <- if (length(non_row_index_choices) > 0L) non_row_index_choices else content_choices
    }
    selected_ref <- saved_inputs$RefenceValues_GSEA %||% input$RefenceValues_GSEA
    sample_choices <- if (length(ref_choices) > 0L && !is.null(selected_ref) && selected_ref %in% ref_choices) {
      gsea_get_sample_choices(def, selected_ref)
    } else character(0)

    list(
      Identifier_GSEA = id_choices,
      AbundanceRatio_GSEA_precalc = ratio_choices,
      pVal_GSEA_precalc = pval_choices,
      RankingMetric_GSEA_precalc = c("log2(FC)", "log2(FC) x -log10(p)", "-log10(p)"),
      RefenceValues_GSEA = ref_choices,
      numeratorSel_GSEA = sample_choices,
      denominatorSel_GSEA = sample_choices
    )
  }

  apply_gsea_restore_state <- function(restored_state) {
    saved_inputs <- restored_state$ui_inputs %||% list()
    signature <- restored_state$restore_signature %||% paste0("GSEA:", paste(unlist(lapply(saved_inputs, as.character), use.names = FALSE), collapse = "|"))
    generation <- isolate(rv$session_restore_generation %||%
                            restored_state$restore_generation %||% NA_integer_)

    if (!is.null(gsea_restore_observer)) {
      gsea_restore_observer$destroy()
    }

    gsea_restore_pending(list(
      state = restored_state,
      inputs = saved_inputs,
      signature = signature,
      generation = generation,
      attempts = 0L,
      max_attempts = 12L
    ))

    # Readiness belongs to this genuine reactive consumer.  The callback that
    # stages a restore never polls reactives imperatively.
    gsea_restore_observer <<- observe({
      pending <- gsea_restore_pending()
      if (is.null(pending) || identical(gsea_restore_last_applied_signature(), pending$signature)) {
        gsea_restore_observer$destroy()
        return()
      }
      current_generation <- rv$session_restore_generation %||% NA_integer_
      if (!identical(as.integer(current_generation)[1L], as.integer(pending$generation)[1L])) {
        gsea_restore_pending(NULL)
        gsea_restore_observer$destroy()
        debug_log("GSEA restore cancelled: stale session restore generation", 1)
        return()
      }
      if (exists("active_restore_signature", inherits = TRUE) && identical(active_restore_signature(), pending$signature)) active_restore_signature(NULL)
      pending$attempts <- as.integer(pending$attempts %||% 0L) + 1L
      gsea_restore_pending(pending)

      saved <- pending$inputs %||% list()
      pathway_choices <- gsea_restore_pathway_choices()
      metadata_choices <- gsea_restore_metadata_choices(df_data_definition_post_mod(), saved)
      file_choices <- gmt_file_choices()

      readiness <- .evaluate_restore_readiness("GSEA", function() {
        !is.null(res_GSEA()) &&
        length(pathway_choices) > 0L &&
        !is.null(metadata_choices) &&
        gsea_value_in_choices(saved$custom_Enrich_select, pathway_choices) &&
        gsea_value_in_choices(saved$fileSelector_GSEA, file_choices) &&
        gsea_value_in_choices(saved$Identifier_GSEA, metadata_choices$Identifier_GSEA) &&
        gsea_value_in_choices(saved$AbundanceRatio_GSEA_precalc, metadata_choices$AbundanceRatio_GSEA_precalc) &&
        gsea_value_in_choices(saved$pVal_GSEA_precalc, metadata_choices$pVal_GSEA_precalc) &&
        gsea_value_in_choices(saved$RankingMetric_GSEA_precalc, metadata_choices$RankingMetric_GSEA_precalc) &&
        gsea_value_in_choices(saved$RefenceValues_GSEA, metadata_choices$RefenceValues_GSEA) &&
        gsea_value_in_choices(saved$numeratorSel_GSEA, metadata_choices$numeratorSel_GSEA) &&
        gsea_value_in_choices(saved$denominatorSel_GSEA, metadata_choices$denominatorSel_GSEA)
      })
      ready <- readiness$ready

      if (!ready && !isTRUE(readiness$retry)) {
        gsea_restore_pending(NULL)
        gsea_restore_observer$destroy()
        stop(readiness$condition)
      }

      if (!ready) {
        if (pending$attempts >= pending$max_attempts) {
          unresolved <- c(
            if (is.null(tryCatch(res_GSEA(), error = gsea_restore_unavailable(NULL)))) "res_GSEA",
            if (length(pathway_choices) == 0L || !gsea_value_in_choices(saved$custom_Enrich_select, pathway_choices)) "custom_Enrich_select",
            if (!gsea_value_in_choices(saved$fileSelector_GSEA, file_choices)) "fileSelector_GSEA",
            if (is.null(metadata_choices)) "metadata choices"
          )
          gsea_restore_pending(NULL)
          if (exists("pending_session_ui_restore", inherits = TRUE)) pending_session_ui_restore(NULL)
          if (exists("active_restore_signature", inherits = TRUE)) active_restore_signature(NULL)
          debug_log(paste("GSEA restore dropped unresolved restored UI values:", paste(unique(unresolved), collapse = "; ")), 1)
          gsea_restore_observer$destroy()
          return()
        }
        if (exists("active_restore_signature", inherits = TRUE)) active_restore_signature(pending$signature)
        invalidateLater(250, session)
        return()
      }

      updateSelectInput(session, "fileSelector_GSEA", choices = file_choices, selected = saved$fileSelector_GSEA)
      updateSelectInput(session, "Identifier_GSEA", choices = metadata_choices$Identifier_GSEA, selected = saved$Identifier_GSEA)
      updateSelectInput(session, "AbundanceRatio_GSEA_precalc", choices = metadata_choices$AbundanceRatio_GSEA_precalc, selected = saved$AbundanceRatio_GSEA_precalc)
      updateSelectInput(session, "pVal_GSEA_precalc", choices = metadata_choices$pVal_GSEA_precalc, selected = saved$pVal_GSEA_precalc)
      updateSelectInput(session, "RankingMetric_GSEA_precalc", choices = metadata_choices$RankingMetric_GSEA_precalc, selected = saved$RankingMetric_GSEA_precalc)
      updateSelectInput(session, "RefenceValues_GSEA", choices = metadata_choices$RefenceValues_GSEA, selected = saved$RefenceValues_GSEA)
      updateSelectizeInput(session, "numeratorSel_GSEA", choices = metadata_choices$numeratorSel_GSEA, selected = saved$numeratorSel_GSEA, server = TRUE)
      updateSelectizeInput(session, "denominatorSel_GSEA", choices = metadata_choices$denominatorSel_GSEA, selected = saved$denominatorSel_GSEA, server = TRUE)
      updateSelectInput(session, "custom_Enrich_select", choices = pathway_choices, selected = saved$custom_Enrich_select)

      restore_select <- function(id, value) if (!is.null(value) && length(value) > 0L) tryCatch(updateSelectInput(session, id, selected = value), error = function(e) NULL)
      restore_numeric <- function(id, value) if (!is.null(value) && length(value) == 1L && !is.na(suppressWarnings(as.numeric(value)))) tryCatch(updateNumericInput(session, id, value = value), error = function(e) NULL)
      restore_select("GSEA_type_select", saved$GSEA_type_select)
      restore_select("plot_type_GSEA", saved$plot_type_GSEA)
      tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_down", value = saved$GSEAColorInput_down), error = function(e) NULL)
      tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_zero", value = saved$GSEAColorInput_zero), error = function(e) NULL)
      tryCatch(colourpicker::updateColourInput(session, "GSEAColorInput_up", value = saved$GSEAColorInput_up), error = function(e) NULL)
      restore_numeric("AxisTitleSize_GSEA", saved$AxisTitleSize_GSEA)
      restore_numeric("tickSize_GSEA", saved$tickSize_GSEA)
      restore_numeric("LegendTextSize_GSEA", saved$LegendTextSize_GSEA)
      restore_numeric("LegendTitleSize_GSEA", saved$LegendTitleSize_GSEA)
      restore_numeric("LabelSize_GSEA", saved$LabelSize_GSEA)
      restore_select("ThemeSelect_GSEA", saved$ThemeSelect_GSEA)
      restore_select("LegendPosition_GSEA", saved$LegendPosition_GSEA)
      if (!is.null(saved$dotplot_swap_panels)) updateCheckboxInput(session, "dotplot_swap_panels", value = isTRUE(saved$dotplot_swap_panels))
      if (!is.null(saved$dotplot_y_ticks_right)) updateCheckboxInput(session, "dotplot_y_ticks_right", value = isTRUE(saved$dotplot_y_ticks_right))
      if (!is.null(saved$plot_height)) {
        updateNumericInput(session, "plot_height_gsea", value = saved$plot_height)
        plot_height(saved$plot_height)
      }

      restored_pathways <- as.character(saved$custom_Enrich_select %||% character(0))
      selected_enrichment(restored_pathways)
      plot_inputs <- get_gsea_plot_input_values(selected_pathways_override = restored_pathways)
      plot_inputs$selected_pathways <- restored_pathways
      plot_inputs$plot_type <- saved$plot_type_GSEA %||% plot_inputs$plot_type
      if (!is.null(saved$plot_height)) plot_inputs$plot_height_val <- saved$plot_height
      register_job <- session$userData$register_restore_job
      resolve_job <- session$userData$resolve_restore_job
      plot_job <- if (is.function(register_job)) tryCatch(
        register_job("GSEA", "restored plot recreation", "render", 15),
        error = function(e) { debug_log(paste("GSEA plot settlement registration failed:", e$message), 1); NULL }
      ) else NULL
      session$onFlushed(once = TRUE, function() {
        .run_session_restore_callback(
          owner = "GSEA", reason = "restored plot recreation",
          generation = generation, phase = "render",
          job_metadata = list(job_id = plot_job, resolve_job = resolve_job,
            current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_)),
          callback = function() {
            selected_enrichment(restored_pathways)
            if (!gsea_value_in_choices(restored_pathways, pathway_choices)) {
              stop("Restored GSEA pathway choices are unavailable")
            }
            if (!isTRUE(do.call(create_gsea_plot_from_current_inputs, plot_inputs))) {
              stop("GSEA plot creation returned no plot")
            }
            debug_log("GSEA plot recreated from restored session UI state", 1)
          })
      })

      gsea_restore_last_applied_signature(pending$signature)
      if (exists("last_applied_restore_signature", inherits = TRUE)) last_applied_restore_signature(pending$signature)
      gsea_restore_pending(NULL)
      if (exists("pending_session_ui_restore", inherits = TRUE)) pending_session_ui_restore(NULL)
      if (exists("active_restore_signature", inherits = TRUE)) active_restore_signature(NULL)
      gsea_restore_observer$destroy()
    }, label = "GSEA restore readiness poll")

    invisible(TRUE)
  }
  assign("apply_gsea_restore_state", apply_gsea_restore_state, envir = modEnv)

  observeEvent(input$create_gsea_plot, {
    plot_inputs <- get_gsea_plot_input_values()
    if (identical(plot_inputs$plot_type, "Pubmed citations")) {
      withProgress(message = "Creating GSEA PubMed Citation Plot...", value = 0, {
        plot_inputs$progress_fn <- function(value, detail) {
          setProgress(value = value, detail = detail)
        }
        do.call(create_gsea_plot_from_current_inputs, plot_inputs)
      })
    } else {
      do.call(create_gsea_plot_from_current_inputs, plot_inputs)
    }
  })

  observeEvent(plot_recreation_state(), {
    recreation_state <- plot_recreation_state()
    req(recreation_state)

    if (!is.null(recreation_state$legacy_current_plot)) {
      render_gsea_plot(
        output,
        recreation_state$legacy_current_plot,
        height = recreation_state$plot_height %||% plot_height(),
        current_plot = current_plot
      )
      debug_log("[GSEA] legacy current_plot rendered during session restore", 1)
      return()
    }

    plot_inputs <- get_gsea_plot_input_values(
      selected_pathways_override = recreation_state$custom_Enrich_select %||%
        recreation_state$selected_pathways %||%
        character(0)
    )
    plot_inputs$plot_type <- recreation_state$plot_type_GSEA %||% plot_inputs$plot_type
    if (!is.null(recreation_state$plot_height_gsea)) {
      plot_inputs$plot_height_val <- recreation_state$plot_height_gsea
    } else if (!is.null(recreation_state$plot_height)) {
      plot_inputs$plot_height_val <- recreation_state$plot_height
    }
    if (!is.null(recreation_state$LegendPosition_GSEA)) {
      plot_inputs$legend_position <- recreation_state$LegendPosition_GSEA
    }
    if (!is.null(recreation_state$colors)) {
      plot_inputs$colors <- recreation_state$colors
    }
    if (!is.null(recreation_state$sizes)) {
      plot_inputs$sizes <- modifyList(plot_inputs$sizes, recreation_state$sizes)
    }
    if (!is.null(recreation_state$ThemeSelect_GSEA)) {
      plot_inputs$theme <- get_selected_theme(recreation_state$ThemeSelect_GSEA)
    }
    if (!is.null(recreation_state$dotplot_swap_panels)) {
      plot_inputs$dotplot_swap_panels <- isTRUE(recreation_state$dotplot_swap_panels)
    }
    if (!is.null(recreation_state$dotplot_y_ticks_right)) {
      plot_inputs$dotplot_y_ticks_right <- isTRUE(recreation_state$dotplot_y_ticks_right)
    }

    pathway_choices <- gsea_restore_pathway_choices()
    if (!gsea_value_in_choices(plot_inputs$selected_pathways, pathway_choices)) {
      debug_log("GSEA plot recreation skipped: restored pathway choices unavailable", 1)
      return()
    }

    if (isTRUE(do.call(create_gsea_plot_from_current_inputs, plot_inputs))) {
      debug_log("GSEA plot recreated from restored session UI state", 1)
    }
  }, ignoreNULL = TRUE)
  # ============================================================
  # 14. Debug Object Export (explicit option + DEBUG_LEVEL >= 2)
  # ============================================================

  observeEvent(input$create_gsea_plot, {
    if (!isTRUE(getOption("miraprot.gsea.export_debug_objects", FALSE))) return()
    if (DEBUG_LEVEL < 2) return()
    obj <- res_GSEA()
    if (is.null(obj)) { debug_log("No GSEA object available.", 2); return() }
    core <- if (inherits(obj, "gseaResult")) obj else {
      if (is.list(obj) && !is.null(obj$Results) && inherits(obj$Results, "gseaResult")) obj$Results else obj
    }
    assign("debug_gsea_wrapper",        obj,            envir = .GlobalEnv)
    assign("debug_gsea_core",           core,           envir = .GlobalEnv)
    assign("debug_gsea_ranking_vector", core@geneList,  envir = .GlobalEnv)
    assign("debug_gsea_gene_sets",      core@geneSets,  envir = .GlobalEnv)
    debug_log("Exported: debug_gsea_wrapper, debug_gsea_core, debug_gsea_ranking_vector, debug_gsea_gene_sets", 2)
  })

  # ============================================================
  # 15. Download Size Info + Plot Download Handlers
  # ============================================================

  output$download_info_GSEA <- renderText({
    req(input$plotWidthInch_GSEA, input$plotHeightInch_GSEA, input$resolution_DPI_GSEA)
    width_px  <- round(input$plotWidthInch_GSEA * input$resolution_DPI_GSEA)
    height_px <- round(input$plotHeightInch_GSEA * input$resolution_DPI_GSEA)
    paste0("Download size: ", width_px, " x ", height_px, " pixels")
  })

  output$downloadPlotButton_GSEA <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      plot_type <- gsub("[^A-Za-z0-9_]", "_", input$plot_type_GSEA %||% "GSEA_plot")
      format    <- input$downloadFormat_GSEA %||% "png"
      paste0("GSEA_", plot_type, "_", timestamp, ".", format)
    },
    content = function(file) {
      plot_obj <- current_plot()
      if (is.null(plot_obj)) {
        debug_log("No plot object available for download", 1)
        showNotification("No plot available. Please create a plot first.", type = "error", duration = 5)
        return()
      }
      tryCatch({
        width_inches  <- if (is.numeric(input$plotWidthInch_GSEA)  && input$plotWidthInch_GSEA  > 0) input$plotWidthInch_GSEA  else 10
        height_inches <- if (is.numeric(input$plotHeightInch_GSEA) && input$plotHeightInch_GSEA > 0) input$plotHeightInch_GSEA else 8
        dpi           <- if (is.numeric(input$resolution_DPI_GSEA) && input$resolution_DPI_GSEA > 0) input$resolution_DPI_GSEA else 300
        format        <- input$downloadFormat_GSEA %||% "png"

        debug_log(paste("Plot download: format=", format,
                         "size=", width_inches, "x", height_inches, "dpi=", dpi), 1)

        switch(format,
          png  = png(filename  = file, width = width_inches, height = height_inches, units = "in", res = dpi, bg = "white"),
          jpeg = jpeg(filename = file, width = width_inches, height = height_inches, units = "in", res = dpi, bg = "white"),
          tiff = tiff(filename = file, width = width_inches, height = height_inches, units = "in", res = dpi, bg = "white"),
          svg  = svg(filename  = file, width = width_inches, height = height_inches, bg = "white"),
          pdf  = pdf(file = file, width = width_inches, height = height_inches, bg = "white"),
          png(filename = file, width = width_inches, height = height_inches, units = "in", res = dpi, bg = "white")
        )

        print(plot_obj)
        dev.off()
        debug_log("Plot download completed", 1)
        showNotification(
          paste0("Downloaded as ", toupper(format),
                 " (", round(width_inches * dpi), "x", round(height_inches * dpi), " px)"),
          type = "message", duration = 3
        )
      }, error = function(e) {
        debug_log(paste("Plot download error:", e$message), 1)
        tryCatch(if (dev.cur() > 1) dev.off(), error = function(e2) NULL)
        showNotification(paste("Download failed:", e$message), type = "error", duration = 5)
      })
    }
  )

  output$download_plot_png <- downloadHandler(
    filename = function() paste0("GSEA_", gsub("[^A-Za-z0-9_]", "_", input$plot_type_GSEA %||% "plot"), "_", Sys.Date(), ".png"),
    content  = function(file) {
      if (is.null(current_plot())) { debug_log("No plot for PNG download", 1); return() }
      w <- if (is.na(as.numeric(input$plotWidthInch_GSEA))  || as.numeric(input$plotWidthInch_GSEA)  <= 0) 12 else as.numeric(input$plotWidthInch_GSEA)
      h <- if (is.na(as.numeric(input$plotHeightInch_GSEA)) || as.numeric(input$plotHeightInch_GSEA) <= 0) 8  else as.numeric(input$plotHeightInch_GSEA)
      d <- if (is.na(as.numeric(input$resolution_DPI_GSEA)) || as.numeric(input$resolution_DPI_GSEA) <= 0) 300 else as.numeric(input$resolution_DPI_GSEA)
      tryCatch(ggsave(file, current_plot(), device = "png", width = w, height = h, dpi = d),
               error = function(e) debug_log(paste("PNG download error:", e$message), 1))
    }
  )

  output$download_plot_pdf <- downloadHandler(
    filename = function() paste0("GSEA_", gsub("[^A-Za-z0-9_]", "_", input$plot_type_GSEA %||% "plot"), "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      if (is.null(current_plot())) { debug_log("No plot for PDF download", 1); return() }
      w <- if (is.na(as.numeric(input$plotWidthInch_GSEA))  || as.numeric(input$plotWidthInch_GSEA)  <= 0) 12 else as.numeric(input$plotWidthInch_GSEA)
      h <- if (is.na(as.numeric(input$plotHeightInch_GSEA)) || as.numeric(input$plotHeightInch_GSEA) <= 0) 8  else as.numeric(input$plotHeightInch_GSEA)
      tryCatch(ggsave(file, current_plot(), device = "pdf", width = w, height = h),
               error = function(e) debug_log(paste("PDF download error:", e$message), 1))
    }
  )

  output$download_plot_svg <- downloadHandler(
    filename = function() paste0("GSEA_", gsub("[^A-Za-z0-9_]", "_", input$plot_type_GSEA %||% "plot"), "_", Sys.Date(), ".svg"),
    content  = function(file) {
      if (is.null(current_plot())) { debug_log("No plot for SVG download", 1); return() }
      w <- if (is.na(as.numeric(input$plotWidthInch_GSEA))  || as.numeric(input$plotWidthInch_GSEA)  <= 0) 12 else as.numeric(input$plotWidthInch_GSEA)
      h <- if (is.na(as.numeric(input$plotHeightInch_GSEA)) || as.numeric(input$plotHeightInch_GSEA) <= 0) 8  else as.numeric(input$plotHeightInch_GSEA)
      tryCatch(ggsave(file, current_plot(), device = "svg", width = w, height = h),
               error = function(e) debug_log(paste("SVG download error:", e$message), 1))
    }
  )

  # ============================================================
  # 16. Plot Grid Integration
  # ============================================================

  observeEvent(input$add_to_grid, {
    debug_log("GSEA: add_to_grid clicked", 2)
    if (is.null(current_plot())) {
      showNotification("No plot available to add. Please create a GSEA plot first.", type = "error")
      debug_log("GSEA: current_plot is NULL", 1)
      return()
    }
    p <- tryCatch(current_plot(), error = function(e) {
      debug_log(paste("GSEA: error accessing plot:", e$message), 1)
      showNotification("Error accessing current plot.", type = "error")
      NULL
    })
    if (is.null(p)) { showNotification("No plot available to add.", type = "error"); return() }
    if (!inherits(p, "ggplot")) {
      showNotification("Only ggplot objects can be added to the grid.", type = "error")
      debug_log("GSEA: current plot is not a ggplot", 1)
      return()
    }

    sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
    lbl_raw  <- input$grid_label
    lbl_id   <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
    plot_id  <- paste0(ns(""), "GSEA_", lbl_id)
    lbl_vis  <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "GSEA"

    tryCatch({
      debug_log(paste("GSEA: adding to grid id=", plot_id), 2)
      modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "GSEA")
      showNotification("Added to grid selection.", type = "message")
    }, error = function(e) {
      debug_log(paste("GSEA: error adding to grid:", e$message), 1)
      showNotification("Error adding plot to grid.", type = "error")
    })
  })

  # ============================================================
  # 17. Results Download Handlers
  # ============================================================

  output$download_results_csv <- downloadHandler(
    filename = function() paste0("GSEA_results_", Sys.Date(), ".csv"),
    content  = function(file) {
      tryCatch({
        req(res_GSEA())
        results_df   <- as.data.frame(res_GSEA()$Results)
        metadata_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(results_df)))
        colnames(metadata_row) <- colnames(results_df)
        metadata_row[1, 1]    <- paste("# GSEA Results exported on", Sys.Date(),
                                        "- Total pathways:", nrow(results_df))
        write.csv(rbind(metadata_row, results_df), file, row.names = FALSE)
      }, error = function(e) debug_log(paste("Error downloading CSV:", e$message), 1))
    }
  )

  output$download_results_xlsx <- downloadHandler(
    filename = function() paste0("GSEA_results_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      tryCatch({
        req(res_GSEA())
        results_obj <- res_GSEA()$Results
        results_df  <- as.data.frame(results_obj)

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "GSEA_Results")
        openxlsx::writeData(wb, "GSEA_Results",
                            paste("GSEA Results exported on", Sys.Date()), startRow = 1)
        openxlsx::writeData(wb, "GSEA_Results",
                            paste("Total pathways:", nrow(results_df)), startRow = 2)
        openxlsx::writeData(wb, "GSEA_Results", results_df, startRow = 4)

        add_gene_list <- FALSE
        gene_list_src <- if (!is.null(res_GSEA()$GeneList) && length(res_GSEA()$GeneList) > 0) {
          res_GSEA()$GeneList
        } else if (!is.null(current_rankings()) && length(current_rankings()$Ranks) > 0) {
          current_rankings()$Ranks
        } else NULL

        if (!is.null(gene_list_src)) {
          fc_src <- if (!is.null(res_GSEA()$GeneList_FC) && length(res_GSEA()$GeneList_FC) > 0) {
            res_GSEA()$GeneList_FC
          } else if (!is.null(current_rankings())) {
            current_rankings()$FC
          } else NULL

          df_gl <- data.frame(Gene = names(gene_list_src),
                               Rank = as.numeric(gene_list_src),
                               stringsAsFactors = FALSE)
          if (!is.null(fc_src)) df_gl$FoldChange <- as.numeric(fc_src[names(gene_list_src)])
          openxlsx::addWorksheet(wb, "GeneList")
          openxlsx::writeData(wb, "GeneList", df_gl)
          add_gene_list <- TRUE
        }

        add_gene_sets <- FALSE
        gs_list <- NULL
        if (methods::is(results_obj, "gseaResult")) {
          slots <- tryCatch(methods::slotNames("gseaResult"), error = function(e) character(0))
          if ("geneSets" %in% slots) {
            gs_list <- tryCatch(methods::slot(results_obj, "geneSets"), error = function(e) NULL)
          }
        }
        if (is.null(gs_list)) {
          gmt_path <- tryCatch(file.path("./GSEA", input$fileSelector_GSEA %||% ""), error = function(e) "")
          if (nzchar(gmt_path) && file.exists(gmt_path)) {
            parse_gmt <- function(path) {
              lines <- readLines(path, warn = FALSE)
              out   <- vector("list", length(lines))
              names(out) <- character(length(lines))
              i <- 0
              for (ln in lines) {
                parts <- strsplit(ln, "\t", fixed = TRUE)[[1]]
                if (length(parts) >= 3) {
                  i <- i + 1
                  out[[i]]      <- parts[-c(1, 2)]
                  names(out)[i] <- parts[1]
                }
              }
              out[seq_len(i)]
            }
            gs_all <- parse_gmt(gmt_path)
            ids    <- results_df$ID %||% character(0)
            desc   <- results_df$Description %||% character(0)
            gs_list <- list()
            for (k in seq_along(ids)) {
              key <- ids[k]
              if (!is.null(gs_all[[key]])) gs_list[[key]] <- gs_all[[key]]
              else if (!is.null(gs_all[[desc[k]]])) gs_list[[key]] <- gs_all[[desc[k]]]
            }
          }
        }
        if (!is.null(gs_list) && length(gs_list) > 0) {
          df_gs <- data.frame(
            ID    = names(gs_list),
            Genes = vapply(gs_list, function(v) paste(v, collapse = "/"), character(1)),
            stringsAsFactors = FALSE
          )
          openxlsx::addWorksheet(wb, "GeneSets")
          openxlsx::writeData(wb, "GeneSets", df_gs)
          add_gene_sets <- TRUE
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

        if (!add_gene_list || !add_gene_sets) {
          missing_sheets <- paste(c(if (!add_gene_list) "GeneList",
                                     if (!add_gene_sets) "GeneSets"), collapse = " and ")
          showNotification(paste("XLSX exported (missing", missing_sheets, "sheet(s))."),
                           type = "warning", duration = 6)
        } else {
          showNotification("XLSX exported with GeneList and GeneSets sheets.",
                           type = "message", duration = 4)
        }

      }, error = function(e) {
        debug_log(paste("Error downloading XLSX:", e$message), 1)
        tryCatch({
          results_df <- as.data.frame(res_GSEA()$Results)
          openxlsx::write.xlsx(results_df, file)
          showNotification("XLSX export fallback used (results only).", type = "warning", duration = 6)
        }, error = function(e2) {
          debug_log(paste("XLSX fallback also failed:", e2$message), 1)
        })
      })
    }
  )

  output$download_gene_rankings <- downloadHandler(
    filename = function() paste0("GSEA_gene_rankings_", Sys.Date(), ".csv"),
    content  = function(file) {
      tryCatch({
        req(current_rankings())
        rankings       <- current_rankings()
        all_ranks      <- as.numeric(rankings$Ranks)
        decimal_places <- calculate_intelligent_decimals(all_ranks, DEBUG_LEVEL)

        rankings_df <- data.frame(
          Gene        = names(rankings$Ranks),
          Rank        = round(all_ranks, decimal_places),
          FoldChange  = round(rankings$FC[names(rankings$Ranks)], 4),
          stringsAsFactors = FALSE
        )
        rankings_df <- rankings_df[order(-rankings_df$Rank, rankings_df$Gene), ]
        n_ties       <- sum(duplicated(rankings_df$Rank))

        if (n_ties > 0) {
          comment_row <- data.frame(
            Gene       = paste("# Note:", n_ties, "genes have identical rankings (ties)"),
            Rank       = NA, FoldChange = NA, stringsAsFactors = FALSE
          )
          rankings_df <- rbind(comment_row, rankings_df)
        }

        write.csv(rankings_df, file, row.names = FALSE)
        debug_log(paste("Gene rankings exported:", decimal_places, "decimal places"), 2)
      }, error = function(e) debug_log(paste("Error downloading gene rankings:", e$message), 1))
    }
  )

  output$download_leading_edge <- downloadHandler(
    filename = function() paste0("GSEA_leading_edge_",
                                  gsub("[^A-Za-z0-9_]", "_", input$custom_Enrich_select),
                                  "_", Sys.Date(), ".csv"),
    content  = function(file) {
      tryCatch({
        req(res_GSEA(), input$custom_Enrich_select, current_rankings())
        gsea_results  <- res_GSEA()$Results
        results_df    <- as.data.frame(gsea_results)
        term_row      <- which(results_df$Description == input$custom_Enrich_select)

        if (length(term_row) > 0) {
          core_genes <- results_df$core_enrichment[term_row]
          if (!is.na(core_genes) && nzchar(core_genes)) {
            leading_genes  <- unlist(strsplit(core_genes, "/"))
            rankings       <- current_rankings()
            gene_ranks     <- as.numeric(rankings$Ranks[leading_genes])
            gene_fcs       <- as.numeric(rankings$FC[leading_genes])
            decimal_places <- calculate_intelligent_decimals(gene_ranks, 1)

            leading_df <- data.frame(
              Gene       = leading_genes,
              Rank       = round(gene_ranks, decimal_places),
              FoldChange = round(gene_fcs, 4),
              stringsAsFactors = FALSE
            )
            leading_df <- leading_df[!is.na(leading_df$Rank), ]
            leading_df <- leading_df[order(-leading_df$Rank, leading_df$Gene), ]
            n_ties      <- sum(duplicated(leading_df$Rank))

            if (n_ties > 0) {
              comment_row <- data.frame(
                Gene       = paste("# Leading edge for:", input$custom_Enrich_select,
                                    "- Note:", n_ties, "genes have identical rankings"),
                Rank       = NA, FoldChange = NA, stringsAsFactors = FALSE
              )
              leading_df <- rbind(comment_row, leading_df)
            }

            write.csv(leading_df, file, row.names = FALSE)
          }
        }
      }, error = function(e) debug_log(paste("Error downloading leading edge:", e$message), 1))
    }
  )

  # ============================================================
  # 18. Session Cleanup
  # ============================================================

  cleanup_manager$register_module("GSEA", function() {
    debug_log("Executing GSEA cleanup", 2)
    res_GSEA(NULL)
    current_rankings(NULL)
    current_plot(NULL)
    selected_enrichment(NULL)
    analysis_metadata(NULL)
    imported_gsea_results(NULL)
    import_status_message("")
    last_workers_requested(NULL)
    last_workers_effective(NULL)
    debug_log("GSEA cleanup completed", 2)
  })

  invisible(NULL)
}
