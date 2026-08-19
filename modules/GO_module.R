# ==============================================================================
# GO Module - Orchestrator
# ==============================================================================
#
# Purpose:
#   Single entry point for the GO enrichment analysis module. Defines the public
#   UI and server functions (modGOUI, modGOServer). Orchestrates sub-script
#   sourcing, debug logging initialization, reactive state setup, and observer
#   wiring. Contains no business logic, no AnnotationHub access, no observers,
#   and no output rendering code.
#
# Architecture role:
#   This file is the ONLY file in the GO module that defines a Shiny server
#   function. All logic, state, observers, and hub access are delegated to the
#   sub-scripts sourced below. The server function acts purely as an orchestrator.
#
# Sub-script responsibilities:
#   - modules/GO/GO_ui.R:              Module UI builder.
#   - modules/GO/GO_module_hub.R:      Organism mapping and public resolvers.
#   - modules/GO/GO_module_hub_cache.R: Persistent SQLite/cache metadata.
#   - modules/GO/GO_module_hub_annotationhub.R: AnnotationHub acquisition.
#   - modules/GO/GO_module_hub_discovery.R: Key-type/organism discovery.
#   - modules/GO/GO_module_logic.R:    Readiness, pairing, identifiers, and enrichment.
#   - modules/GO/GO_module_plots.R:    P-value formatting and GO plot constructors.
#   - modules/GO/GO_module_tree.R:     Tree construction and selection helpers.
#   - modules/GO/GO_module_state.R:    All reactiveVal() / reactiveValues()
#                                      definitions. Must be sourced first inside
#                                      the server.
#   - modules/GO/GO_module_observer.R: All observers, reactive expressions,
#                                      outputs, and download handlers. Must be
#                                      sourced after GO_module_state.R.
#
# Sourcing model:
#   - GO_ui.R, the GO_module_hub peer files, and the logic/plots/tree files are sourced at the module
#     file level (outside moduleServer), making them available in the closure
#     of the server function via the modEnv scope chain.
#   - GO_module_state.R and GO_module_observer.R are sourced with local = TRUE
#     inside moduleServer so that reactive values and observers are registered
#     within the correct session scope.
#
# Logging model:
#   - debug_log is defined once inside modGOServer and passed to all functions
#     that require it. level 1 = important state transitions; level 2 = verbose
#     tracing. Duplicate debug_log definitions previously scattered in
#     observers have been removed.
#
# Future developers:
#   - Do not add logic, state, observers, or output rendering to this file.
#   - Keep this file as a thin wiring layer only.
#   - Session cleanup and the module return interface are the only non-
#     orchestration concerns in this file.
# ==============================================================================


# Safely read optional S4 slots from enrichResult objects that may have been
# restored from sessions saved with a different clusterProfiler version.
safe_go_enrich_slot <- function(obj, slot_name, default = NA) {
  if (is.null(obj)) return(default)
  slots <- tryCatch(methods::slotNames(obj), error = function(e) character())
  if (!slot_name %in% slots) return(default)
  tryCatch(methods::slot(obj, slot_name), error = function(e) default)
}

.safe_go_result_df <- function(results) {
  edo <- tryCatch(results$Edo_GO, error = function(e) NULL)
  stable_df <- tryCatch(results$stable_snapshot$result_df, error = function(e) NULL)
  if (!is.null(stable_df)) return(as.data.frame(stable_df))
  df <- safe_go_enrich_slot(edo, "result", default = NULL)
  if (is.null(df)) return(NULL)
  tryCatch(as.data.frame(df), error = function(e) NULL)
}

.go_stable_snapshot_from_results <- function(results, input = NULL) {
  if (is.null(results) || !is.list(results)) return(NULL)
  edo <- tryCatch(results$Edo_GO, error = function(e) NULL)
  list(
    version = "1.1",
    result_df = .safe_go_result_df(results),
    geneSets = safe_go_enrich_slot(edo, "geneSets", default = tryCatch(results$geneSets, error = function(e) NULL)),
    go_data = tryCatch(results$go_data, error = function(e) NULL),
    go_data_FC = tryCatch(results$go_data_FC, error = function(e) NULL),
    parameters = list(
      organism = safe_go_enrich_slot(edo, "organism", default = NA),
      ontology = safe_go_enrich_slot(edo, "ontology", default = NA),
      keytype = safe_go_enrich_slot(edo, "keytype", default = NA),
      pAdjustMethod = safe_go_enrich_slot(edo, "pAdjustMethod", default = NA),
      pvalueCutoff = safe_go_enrich_slot(edo, "pvalueCutoff", default = NA),
      qvalueCutoff = safe_go_enrich_slot(edo, "qvalueCutoff", default = NA),
      minGSSize = safe_go_enrich_slot(edo, "minGSSize", default = NA),
      maxGSSize = safe_go_enrich_slot(edo, "maxGSSize", default = NA)
    )
  )
}

.go_results_from_stable_snapshot <- function(stable) {
  if (is.null(stable) || !is.list(stable)) return(NULL)
  result_df <- tryCatch(as.data.frame(stable$result_df), error = function(e) NULL)
  list(
    Edo_GO = NULL,
    Edo_GO_safe = NULL,
    Edox_GO = NULL,
    Edop_GO = NULL,
    result_df = result_df,
    geneSets = tryCatch(stable$geneSets, error = function(e) NULL),
    go_data = tryCatch(stable$go_data, error = function(e) NULL),
    go_data_FC = tryCatch(stable$go_data_FC, error = function(e) NULL),
    parameters = tryCatch(stable$parameters, error = function(e) list()),
    stable_snapshot = stable
  )
}

.go_tree_from_stable_snapshot <- function(stable) {
  result_df <- tryCatch(as.data.frame(stable$result_df), error = function(e) NULL)
  if (is.null(result_df) || nrow(result_df) == 0 || !"Description" %in% names(result_df)) {
    return(NULL)
  }
  terms <- head(as.character(result_df$Description), 100)
  stats <- rep("", length(terms))
  if ("p.adjust" %in% names(result_df)) {
    stats <- paste0(" (adj. p=", signif(head(result_df$p.adjust, 100), 3), ")")
  }
  leaves <- stats
  names(leaves) <- paste0(terms, stats)
  list("Restored GO terms" = as.list(leaves))
}

.go_session_safe_value <- function(x) {
  if (is.null(x) || is.atomic(x)) return(x)
  if (is.function(x) || is.environment(x)) return(NULL)
  if (inherits(x, "ggplot", which = FALSE)) return(NULL)
  if (isS4(x)) return(NULL)
  if (is.data.frame(x)) {
    x[] <- lapply(x, .go_session_safe_value)
    return(x)
  }
  if (is.list(x)) return(lapply(x, .go_session_safe_value))
  x
}

.go_session_contains_ggplot <- function(x, depth = 0, max_depth = 8) {
  if (depth > max_depth || is.null(x) || is.atomic(x) || is.function(x) || is.environment(x)) return(FALSE)
  if (inherits(x, "ggplot", which = FALSE)) return(TRUE)
  if (is.data.frame(x) || is.list(x)) {
    return(any(vapply(x, .go_session_contains_ggplot, logical(1), depth = depth + 1, max_depth = max_depth)))
  }
  if (isS4(x)) {
    slots <- tryCatch(methods::slotNames(x), error = function(e) character())
    return(any(vapply(slots, function(slot_name) {
      .go_session_contains_ggplot(tryCatch(methods::slot(x, slot_name), error = function(e) NULL), depth + 1, max_depth)
    }, logical(1))))
  }
  FALSE
}

# Source UI builder (module-level, outside server)
source("modules/GO/GO_ui.R", local = modEnv)

# Source hub, logic, plotting, and tree functions (module-level, outside server).
# Sourcing into modEnv makes these functions available to the server closure.
source("modules/GO/GO_module_hub.R",               local = modEnv)
source("modules/GO/GO_module_hub_cache.R",         local = modEnv)
source("modules/GO/GO_module_hub_annotationhub.R", local = modEnv)
source("modules/GO/GO_module_hub_discovery.R",     local = modEnv)
source("modules/GO/GO_module_logic.R", local = modEnv)
source("modules/GO/GO_module_plots.R", local = modEnv)
source("modules/GO/GO_module_tree.R",  local = modEnv)

# ==============================================================================
# Module UI
# ==============================================================================

modGOUI <- function(id) {
  ns <- NS(id)
  tagList(
    GO_UI(ns)
  )
}

# ==============================================================================
# Module Server (the only server function for this module)
# ==============================================================================

modGOServer <- function(id, rv, debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug logging
    # --------------------------------------------------------------------------
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "GO MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ GO MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("GO module server starting", 1)

    pending_session_ui_restore <- reactiveVal(NULL)
    last_applied_restore_signature <- reactiveVal(NULL)
    active_restore_signature <- reactiveVal(NULL)

    # --------------------------------------------------------------------------
    # Reactive state (must be sourced before observers)
    # --------------------------------------------------------------------------
    source("modules/GO/GO_module_state.R",    local = TRUE)

    # --------------------------------------------------------------------------
    # Observers, outputs, and reactive triggers
    # --------------------------------------------------------------------------
    source("modules/GO/GO_module_observer.R", local = TRUE)

    # --------------------------------------------------------------------------
    # Session cleanup
    # --------------------------------------------------------------------------
    cleanup_manager$register_module("GO", function() {
      debug_log("Executing [GO] cleanup", 2)
      cleanup_temp_caches(debug_log = debug_log)
      GO_Result_List(NULL)
      go_data_processed(NULL)
      go_tree_structure(NULL)
      current_plot_object(NULL)
      go_errors(list())
      debug_log("[GO] cleanup completed", 2)
    })

    # --------------------------------------------------------------------------
    # Module return interface
    # --------------------------------------------------------------------------
    return(list(
      get_results = function() {
        tryCatch(GO_Result_List(), error = function(e) NULL)
      },
      has_results = function() {
        !is.null(tryCatch(GO_Result_List(), error = function(e) NULL))
      },
      get_status         = function() go_analysis_status(),
      get_processed_data = function() go_data_processed(),
      get_tree_structure = function() go_tree_structure(),
      get_errors         = function() go_errors(),
      get_current_plot   = function() current_plot_object(),
      get_plot_message   = function() current_plot_message(),
      get_results_dataframe = function() {
        res <- tryCatch(GO_Result_List(), error = function(e) NULL)
        if (is.null(res)) return(NULL)
        # Return as-is; downstream code handles extraction
        res
      },
      set_results = function(results) {
        GO_Result_List(results)
        if (!is.null(results)) {
          go_analysis_status("complete")
          debug_log("GO results set via session restore", 1)
          # Level-0 logs for session-restored GO results
          edo_l0        <- tryCatch(results$Edo_GO, error = function(e) NULL)
          n_input_l0    <- tryCatch(length(unique(results$go_data$Gene)), error = function(e) NA_integer_)
          params_l0     <- tryCatch(results$parameters, error = function(e) list())
          org_l0        <- as.character(safe_go_enrich_slot(edo_l0, "organism",      params_l0$organism %||% NA_character_))
          ont_l0        <- as.character(safe_go_enrich_slot(edo_l0, "ontology",      params_l0$ontology %||% NA_character_))
          keytype_l0    <- as.character(safe_go_enrich_slot(edo_l0, "keytype",       params_l0$keytype %||% NA_character_))
          padj_l0       <- as.character(safe_go_enrich_slot(edo_l0, "pAdjustMethod", params_l0$pAdjustMethod %||% NA_character_))
          pval_cut_l0   <- as.character(safe_go_enrich_slot(edo_l0, "pvalueCutoff",  params_l0$pvalueCutoff %||% NA_character_))
          qval_cut_l0   <- as.character(safe_go_enrich_slot(edo_l0, "qvalueCutoff",  params_l0$qvalueCutoff %||% NA_character_))
          mingss_l0     <- as.character(safe_go_enrich_slot(edo_l0, "minGSSize",     params_l0$minGSSize %||% NA_character_))
          maxgss_l0     <- as.character(safe_go_enrich_slot(edo_l0, "maxGSSize",     params_l0$maxGSSize %||% NA_character_))
          abundance_cut_l0 <- as.character(input$AbundanceInput_GO %||% NA_character_)
          abundance_dir_l0 <- if (isTRUE(input$useMaxLog2FC_GO)) "maximum" else "minimum"
          ratio_pval_cut_l0 <- as.character(input$pvalueInput_GO %||% NA_character_)
          debug_log(
            sprintf(
              paste0(
                "GO analysis inputs",
                " | Organism: %s",
                " | Ontology: %s",
                " | Key type: %s",
                " | p-value cutoff: %s",
                " | q-value cutoff: %s",
                " | p-adjust method: %s",
                " | Min GS size: %s",
                " | Max GS size: %s",
                " | Fold-change abundance ratio threshold: %s (%s)",
                " | Ratio p-value threshold: %s",
                " | Input proteins: %d"
              ),
              org_l0, ont_l0, keytype_l0,
              pval_cut_l0, qval_cut_l0, padj_l0,
              mingss_l0, maxgss_l0,
              abundance_cut_l0, abundance_dir_l0, ratio_pval_cut_l0,
              n_input_l0
            ),
            level = 0
          )
          result_df_l0  <- .safe_go_result_df(results)
          n_sig_l0      <- tryCatch(nrow(result_df_l0), error = function(e) NA_integer_)
          n_enrich_l0   <- tryCatch(length(safe_go_enrich_slot(edo_l0, "gene", default = NULL)), error = function(e) NA_integer_)
          n_universe_l0 <- tryCatch(length(safe_go_enrich_slot(edo_l0, "universe", default = NULL)), error = function(e) NA_integer_)
          debug_log(
            sprintf(
              paste0(
                "GO analysis outcome",
                " | Proteins passed filter: %d",
                " | Proteins in enrichment: %d",
                " | Universe size: %d",
                " | Significant terms: %d"
              ),
              n_input_l0, n_enrich_l0, n_universe_l0, n_sig_l0
            ),
            level = 0
          )
        }
      },
      clear_results      = function() {
        GO_Result_List(NULL)
        go_data_processed(NULL)
        go_tree_structure(NULL)
        current_plot_object(NULL)
        go_analysis_status("idle")
        debug_log("GO results cleared", 1)
      },

      # Session save/restore interface
      get_session_state = function(mode = "session") {
        ui_inputs <- list(
          OrgDb_GO                    = tryCatch(input$OrgDb_GO, error = function(e) NULL),
          keyType_GO                  = tryCatch(input$keyType_GO, error = function(e) NULL),
          custom_EnrichPlot_select_GO = tryCatch(input$custom_EnrichPlot_select_GO, error = function(e) NULL),
          selected_terms              = tryCatch(isolate(rv$go_selected_terms %||% character(0)), error = function(e) character(0)),
          GOColorInput_down           = tryCatch(input$GOColorInput_down, error = function(e) NULL),
          GOColorInput_zero           = tryCatch(input$GOColorInput_zero, error = function(e) NULL),
          GOColorInput_up             = tryCatch(input$GOColorInput_up, error = function(e) NULL),
          AxisTitleSize_GO            = tryCatch(input$AxisTitleSize_GO, error = function(e) NULL),
          tickSize_GO                 = tryCatch(input$tickSize_GO, error = function(e) NULL),
          LegendTitleSize_GO          = tryCatch(input$LegendTitleSize_GO, error = function(e) NULL),
          LegendTextSize_GO           = tryCatch(input$LegendTextSize_GO, error = function(e) NULL),
          ThemeSelect_GO              = tryCatch(input$ThemeSelect_GO, error = function(e) NULL),
          LegendPosition_GO           = tryCatch(input$LegendPosition_GO, error = function(e) NULL),
          plot_height_go              = tryCatch(input$plot_height_go, error = function(e) NULL),
          max_terms_GO                = tryCatch(input$max_terms_GO, error = function(e) NULL),
          plotWidthInch_GO            = tryCatch(input$plotWidthInch_GO, error = function(e) NULL),
          plotHeightInch_GO           = tryCatch(input$plotHeightInch_GO, error = function(e) NULL),
          resolution_DPI_GO           = tryCatch(input$resolution_DPI_GO, error = function(e) NULL),
          GeneIdentifierColumn_GO    = tryCatch(input$GeneIdentifierColumn_GO, error = function(e) NULL),
          AbundanceCol_GO            = tryCatch(input$AbundanceCol_GO, error = function(e) NULL),
          pValType_GO                = tryCatch(input$pValType_GO, error = function(e) NULL),
          pValCol_GO                 = tryCatch(input$pValCol_GO, error = function(e) NULL),
          useMaxLog2FC_GO            = tryCatch(input$useMaxLog2FC_GO, error = function(e) NULL),
          AbundanceInput_GO          = tryCatch(input$AbundanceInput_GO, error = function(e) NULL),
          pvalueInput_GO             = tryCatch(input$pvalueInput_GO, error = function(e) NULL),
          ont_GO                     = tryCatch(input$ont_GO, error = function(e) NULL),
          padjustMethod_GO           = tryCatch(input$padjustMethod_GO, error = function(e) NULL),
          pvalueCutoff_GO            = tryCatch(input$pvalueCutoff_GO, error = function(e) NULL),
          qvalueCutoff_GO            = tryCatch(input$qvalueCutoff_GO, error = function(e) NULL),
          minGSSize_GO               = tryCatch(input$minGSSize_GO, error = function(e) NULL),
          maxGSSize_GO               = tryCatch(input$maxGSSize_GO, error = function(e) NULL),
          universe_GO                = tryCatch(input$universe_GO, error = function(e) NULL),
          downloadFormat_GO          = tryCatch(input$downloadFormat_GO, error = function(e) NULL),
          grid_label                 = tryCatch(input$grid_label, error = function(e) NULL)
        )

        state <- list(version = "1.2", mode = mode, ui_inputs = ui_inputs)
        if (identical(mode, "ui_only")) {
          state$go_analysis_status <- tryCatch(go_analysis_status(), error = function(e) "idle")
          state$import_status <- tryCatch(import_status_message(), error = function(e) "")
          return(state)
        }

        state$GO_Result_List      <- tryCatch(GO_Result_List(), error = function(e) NULL)
        state$GO_Result_Stable    <- tryCatch(.go_stable_snapshot_from_results(state$GO_Result_List, input), error = function(e) NULL)
        state$go_data_processed   <- tryCatch(go_data_processed(), error = function(e) NULL)
        state$go_analysis_status  <- tryCatch(go_analysis_status(), error = function(e) "idle")
        state$go_tree_structure   <- .go_session_safe_value(tryCatch(go_tree_structure(), error = function(e) NULL))
        state$import_status       <- tryCatch(import_status_message(), error = function(e) "")
        state$plot_height         <- tryCatch(current_plot_height(), error = function(e) 600)
        state$plot_width          <- tryCatch(current_plot_width(), error = function(e) 800)
        state$plot_config_colors  <- tryCatch(plot_config$colors, error = function(e) NULL)
        state$plot_config_sizes   <- tryCatch(plot_config$sizes, error = function(e) NULL)
        state$plot_config_dims    <- tryCatch(plot_config$plot_dimensions, error = function(e) NULL)
        state$plot_recreation_state <- list(
          plot_type = ui_inputs$custom_EnrichPlot_select_GO,
          selected_terms = ui_inputs$selected_terms,
          plot_height = ui_inputs$plot_height_go %||% state$plot_height,
          plot_width = state$plot_width
        )
        # Do not serialize current_plot_object / plot_config_theme here. Those
        # ggplot/theme trees dominate Data & Analysis sanitization time; restore
        # recreates the plot from results + saved UI settings instead.
        # Note: cached_go_org_db is an OrgDb handle (non-serializable) -- excluded
        state
      },
      set_session_state = function(state) {
        if (is.null(state)) return()
        go_restore_guard_active(TRUE)
        go_restore_row_index_skip_logged(FALSE)
        go_restore_trigger_baseline(tryCatch(isolate(rv$session_restore_trigger), error = function(e) NULL))
        if (!is.null(state$GO_Result_List)) {
          restored_raw <- tryCatch({
            # Probe optional and required slots without treating missing optionals
            # as app-level restore errors.
            raw_result_df <- .safe_go_result_df(state$GO_Result_List)
            stable_result_df <- tryCatch(state$GO_Result_Stable$result_df, error = function(e) NULL)
            if (is.null(raw_result_df) && !is.null(stable_result_df)) {
              stop("raw GO enrichResult has no readable result slot")
            }
            state$GO_Result_List
          }, error = function(e) {
            debug_log(paste("GO raw enrichResult restore probe failed; using stable snapshot when available:", e$message), 1)
            NULL
          })
          if (!is.null(restored_raw)) {
            GO_Result_List(restored_raw)
          } else if (!is.null(state$GO_Result_Stable)) {
            GO_Result_List(.go_results_from_stable_snapshot(state$GO_Result_Stable))
          }
        } else if (!is.null(state$GO_Result_Stable)) {
          GO_Result_List(.go_results_from_stable_snapshot(state$GO_Result_Stable))
        }
        if (!is.null(state$go_data_processed))  go_data_processed(state$go_data_processed)
        if (!is.null(state$go_analysis_status)) go_analysis_status(state$go_analysis_status)
        if (!is.null(state$go_tree_structure)) {
          go_tree_structure(state$go_tree_structure)
        } else if (!is.null(state$GO_Result_Stable)) {
          stable_tree <- .go_tree_from_stable_snapshot(state$GO_Result_Stable)
          if (!is.null(stable_tree)) go_tree_structure(stable_tree)
        }
        if (!is.null(state$import_status))      import_status_message(state$import_status)
        if (!is.null(state$plot_height))        current_plot_height(state$plot_height)
        if (!is.null(state$plot_width))         current_plot_width(state$plot_width)
        if (!is.null(state$plot_config_colors)) plot_config$colors <- state$plot_config_colors
        if (!is.null(state$plot_config_sizes))  plot_config$sizes  <- state$plot_config_sizes
        if (!is.null(state$plot_config_dims))   plot_config$plot_dimensions <- state$plot_config_dims
        restored_inputs <- state$ui_inputs %||% list()
        metadata_dependent_restore <- restored_inputs[c(
          "GeneIdentifierColumn_GO", "AbundanceCol_GO", "pValType_GO", "pValCol_GO"
        )]
        metadata_dependent_restore <- metadata_dependent_restore[!vapply(metadata_dependent_restore, is.null, logical(1))]
        if (length(metadata_dependent_restore) > 0L) {
          tryCatch({ rv$go_pending_ui_restore <- metadata_dependent_restore }, error = function(e) NULL)
        }
        restored_orgdb <- restored_inputs$OrgDb_GO %||% state$OrgDb_GO
        restored_keytype <- restored_inputs$keyType_GO %||% state$keyType_GO
        restored_ontology <- restored_inputs$ont_GO %||% state$ont_GO

        restore_signature <- paste0(
          "GO:",
          paste(
            vapply(
              list(
                selected_terms = restored_inputs$selected_terms,
                plot_type = restored_inputs$custom_EnrichPlot_select_GO,
                key_type = restored_keytype,
                organism = restored_orgdb,
                ontology = restored_ontology,
                abundance_col = restored_inputs$AbundanceCol_GO,
                pval_type = restored_inputs$pValType_GO,
                pval_col = restored_inputs$pValCol_GO,
                gene_identifier_col = restored_inputs$GeneIdentifierColumn_GO
              ),
              function(value) paste(as.character(value), collapse = ","),
              character(1),
              USE.NAMES = TRUE
            ),
            collapse = "|"
          )
        )
        restore_generation <- as.integer(Sys.time())

        metadata_dependent_inputs <- list(
          OrgDb_GO = restored_orgdb,
          keyType_GO = restored_keytype,
          ont_GO = restored_ontology,
          selected_terms = restored_inputs$selected_terms
        )

        restore_scalar_input <- function(id, value, updater = updateSelectInput) {
          if (!is.null(value) && length(value) == 1L && !is.na(value)) {
            tryCatch(updater(session, id, selected = value), error = function(e) NULL)
          }
        }
        restore_numeric_input <- function(id, value) {
          if (!is.null(value) && length(value) == 1L && !is.na(suppressWarnings(as.numeric(value)))) {
            tryCatch(updateNumericInput(session, id, value = value), error = function(e) NULL)
          }
        }

        restore_scalar_input <- function(id, value, updater = updateSelectInput) {
          if (!is.null(value) && length(value) == 1L && !is.na(value)) {
            tryCatch(updater(session, id, selected = value), error = function(e) NULL)
          }
        }
        restore_numeric_input <- function(id, value) {
          if (!is.null(value) && length(value) == 1L && !is.na(suppressWarnings(as.numeric(value)))) {
            tryCatch(updateNumericInput(session, id, value = value), error = function(e) NULL)
          }
        }
        apply_go_ui_inputs <- function() {
          restore_scalar_input("custom_EnrichPlot_select_GO", restored_inputs$custom_EnrichPlot_select_GO)
          restore_scalar_input("ThemeSelect_GO", restored_inputs$ThemeSelect_GO)
          restore_scalar_input("LegendPosition_GO", restored_inputs$LegendPosition_GO)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_down", value = restored_inputs$GOColorInput_down), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_zero", value = restored_inputs$GOColorInput_zero), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_up", value = restored_inputs$GOColorInput_up), error = function(e) NULL)
          restore_numeric_input("AxisTitleSize_GO", restored_inputs$AxisTitleSize_GO)
          restore_numeric_input("tickSize_GO", restored_inputs$tickSize_GO)
          restore_numeric_input("LegendTitleSize_GO", restored_inputs$LegendTitleSize_GO)
          restore_numeric_input("LegendTextSize_GO", restored_inputs$LegendTextSize_GO)
          restore_numeric_input("plot_height_go", restored_inputs$plot_height_go)
          restore_numeric_input("max_terms_GO", restored_inputs$max_terms_GO)
          restore_numeric_input("plotWidthInch_GO", restored_inputs$plotWidthInch_GO)
          restore_numeric_input("plotHeightInch_GO", restored_inputs$plotHeightInch_GO)
          restore_numeric_input("resolution_DPI_GO", restored_inputs$resolution_DPI_GO)
          restore_scalar_input("GeneIdentifierColumn_GO", restored_inputs$GeneIdentifierColumn_GO, updateSelectizeInput)
          restore_scalar_input("AbundanceCol_GO", restored_inputs$AbundanceCol_GO, updateSelectizeInput)
          restore_scalar_input("pValType_GO", restored_inputs$pValType_GO, updateSelectizeInput)
          restore_scalar_input("pValCol_GO", restored_inputs$pValCol_GO, updateSelectizeInput)
          if (!is.null(restored_inputs$useMaxLog2FC_GO)) {
            tryCatch(updateCheckboxInput(session, "useMaxLog2FC_GO", value = restored_inputs$useMaxLog2FC_GO), error = function(e) NULL)
          }
          restore_numeric_input("AbundanceInput_GO", restored_inputs$AbundanceInput_GO)
          restore_numeric_input("pvalueInput_GO", restored_inputs$pvalueInput_GO)
          restore_scalar_input("ont_GO", restored_inputs$ont_GO)
          restore_scalar_input("padjustMethod_GO", restored_inputs$padjustMethod_GO)
          restore_numeric_input("pvalueCutoff_GO", restored_inputs$pvalueCutoff_GO)
          restore_numeric_input("qvalueCutoff_GO", restored_inputs$qvalueCutoff_GO)
          restore_numeric_input("minGSSize_GO", restored_inputs$minGSSize_GO)
          restore_numeric_input("maxGSSize_GO", restored_inputs$maxGSSize_GO)
        }

        restore_scalar_input <- function(id, value, updater = updateSelectInput) {
          if (!is.null(value) && length(value) == 1L && !is.na(value)) {
            tryCatch(updater(session, id, selected = value), error = function(e) NULL)
          }
        }
        restore_numeric_input <- function(id, value) {
          if (!is.null(value) && length(value) == 1L && !is.na(suppressWarnings(as.numeric(value)))) {
            tryCatch(updateNumericInput(session, id, value = value), error = function(e) NULL)
          }
        }
        apply_go_ui_inputs <- function() {
          restore_scalar_input("custom_EnrichPlot_select_GO", restored_inputs$custom_EnrichPlot_select_GO)
          restore_scalar_input("ThemeSelect_GO", restored_inputs$ThemeSelect_GO)
          restore_scalar_input("LegendPosition_GO", restored_inputs$LegendPosition_GO)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_down", value = restored_inputs$GOColorInput_down), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_zero", value = restored_inputs$GOColorInput_zero), error = function(e) NULL)
          tryCatch(colourpicker::updateColourInput(session, "GOColorInput_up", value = restored_inputs$GOColorInput_up), error = function(e) NULL)
          restore_numeric_input("AxisTitleSize_GO", restored_inputs$AxisTitleSize_GO)
          restore_numeric_input("tickSize_GO", restored_inputs$tickSize_GO)
          restore_numeric_input("LegendTitleSize_GO", restored_inputs$LegendTitleSize_GO)
          restore_numeric_input("LegendTextSize_GO", restored_inputs$LegendTextSize_GO)
          restore_numeric_input("plot_height_go", restored_inputs$plot_height_go)
          restore_numeric_input("max_terms_GO", restored_inputs$max_terms_GO)
          restore_numeric_input("plotWidthInch_GO", restored_inputs$plotWidthInch_GO)
          restore_numeric_input("plotHeightInch_GO", restored_inputs$plotHeightInch_GO)
          restore_numeric_input("resolution_DPI_GO", restored_inputs$resolution_DPI_GO)
          restore_scalar_input("GeneIdentifierColumn_GO", restored_inputs$GeneIdentifierColumn_GO, updateSelectizeInput)
          restore_scalar_input("AbundanceCol_GO", restored_inputs$AbundanceCol_GO, updateSelectizeInput)
          restore_scalar_input("pValType_GO", restored_inputs$pValType_GO, updateSelectizeInput)
          restore_scalar_input("pValCol_GO", restored_inputs$pValCol_GO, updateSelectizeInput)
          if (!is.null(restored_inputs$useMaxLog2FC_GO)) {
            tryCatch(updateCheckboxInput(session, "useMaxLog2FC_GO", value = restored_inputs$useMaxLog2FC_GO), error = function(e) NULL)
          }
          restore_numeric_input("AbundanceInput_GO", restored_inputs$AbundanceInput_GO)
          restore_numeric_input("pvalueInput_GO", restored_inputs$pvalueInput_GO)
          restore_scalar_input("ont_GO", restored_inputs$ont_GO)
          restore_scalar_input("padjustMethod_GO", restored_inputs$padjustMethod_GO)
          restore_numeric_input("pvalueCutoff_GO", restored_inputs$pvalueCutoff_GO)
          restore_numeric_input("qvalueCutoff_GO", restored_inputs$qvalueCutoff_GO)
          restore_numeric_input("minGSSize_GO", restored_inputs$minGSSize_GO)
          restore_numeric_input("maxGSSize_GO", restored_inputs$maxGSSize_GO)
          if (!is.null(restored_inputs$universe_GO)) tryCatch(updateTextAreaInput(session, "universe_GO", value = restored_inputs$universe_GO), error = function(e) NULL)
          restore_scalar_input("downloadFormat_GO", restored_inputs$downloadFormat_GO)
          if (!is.null(restored_inputs$grid_label)) tryCatch(updateTextInput(session, "grid_label", value = restored_inputs$grid_label), error = function(e) NULL)
        }

        restored_terms <- as.character(restored_inputs$selected_terms %||% character(0))
        restored_terms <- restored_terms[!is.na(restored_terms) & nzchar(trimws(restored_terms))]
        if (length(restored_terms) > 0) {
          rv$go_selected_terms <- restored_terms
        }

        recreate_go_plot <- function() {
          results <- tryCatch(isolate(GO_Result_List()), error = function(e) NULL)
          result_df <- tryCatch(.safe_go_result_df(results), error = function(e) NULL)
          if (is.null(results) || is.null(result_df) || nrow(result_df) == 0 || length(restored_terms) == 0) {
            return(invisible(FALSE))
          }
          plot_type <- restored_inputs$custom_EnrichPlot_select_GO %||% "Enrichment score dotplot"
          colors <- c(restored_inputs$GOColorInput_down %||% "#440154FF",
                      restored_inputs$GOColorInput_zero %||% "#31688EFF",
                      restored_inputs$GOColorInput_up %||% "#EFC000FF")
          sizes <- list(
            axisTitle = as.numeric(restored_inputs$AxisTitleSize_GO %||% 12),
            tick = as.numeric(restored_inputs$tickSize_GO %||% 10),
            legendText = as.numeric(restored_inputs$LegendTextSize_GO %||% 10),
            legendTitle = as.numeric(restored_inputs$LegendTitleSize_GO %||% 12),
            label = 12
          )
          theme <- get_selected_theme_go(restored_inputs$ThemeSelect_GO %||% "Black and White")
          legend_position <- restored_inputs$LegendPosition_GO %||% "right"
          edo_for_plot <- if (!is.null(results$Edo_GO)) results$Edo_GO else result_df
          plot_result <- switch(plot_type,
            "Enrichment score dotplot" = create_go_dotplot(edo_for_plot, restored_terms, colors, sizes, theme, legend_position),
            "Cnet plot (log2FC)" = if (!is.null(results$Edo_GO)) create_go_cnet_plot_fc_fixed(results, restored_terms, colors, sizes, theme, legend_position) else list(plot = NULL),
            "Enrichment map" = if (!is.null(results$Edo_GO)) create_go_enrichment_map_fixed(results, restored_terms, colors, sizes, theme, legend_position) else list(plot = NULL),
            "Pubmed citations" = create_go_pubmed_plot(restored_terms, colors, sizes, theme, legend_position),
            list(plot = NULL)
          )
          if (!is.null(plot_result$plot)) {
            current_plot_object(plot_result$plot)
            current_plot_message(plot_result$message %||% paste("Restored GO", plot_type, "plot"))
            current_plot_height(plot_result$height %||% restored_inputs$plot_height_go %||% 600)
            current_plot_width(plot_result$width %||% 800)
            plot_update_trigger(isolate(plot_update_trigger()) + 1)
            debug_log("GO plot recreated from restored session UI state", 1)
            return(invisible(TRUE))
          }
          invisible(FALSE)
        }

        apply_go_ui_inputs()
        if (is.function(session$onFlushed)) {
          session$onFlushed(once = TRUE, function() {
            apply_go_ui_inputs()
            session$onFlushed(once = TRUE, function() {
              apply_go_ui_inputs()
              recreate_go_plot()
            })
          })
        } else {
          recreate_go_plot()
        }

        debug_log("GO module session state restored via set_session_state", 1)
      }
    ))
  })
}
