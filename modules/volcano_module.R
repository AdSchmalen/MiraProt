# ==============================================================================
# volcano_module.R
# ==============================================================================
#
# PURPOSE:
#   Orchestrator for the volcano plot module. Sources all sub-scripts, defines
#   the module UI and server entry points, initializes reactive state, sets up
#   debug logging, and wires together the data input reactives. All business
#   logic, observers, plot generation, and export functionality reside in the
#   sub-scripts sourced below.
#
# ARCHITECTURAL ROLE:
#   Orchestrator -- the single entry point for the volcano module. Responsible
#   for sourcing, initialization, and wiring only. Contains no business logic,
#   observer definitions, or rendering code.
#
# RESPONSIBILITIES:
#   - Source all volcano sub-scripts into modEnv
#   - Define modVolcanoUI() and modVolcanoServer()
#   - Initialize reactive state via init_volcano_state()
#   - Set up debug_log helper
#   - Define data_in / data_def_in computed reactives
#   - Call register_volcano_observers() to wire up all reactive behaviour
#   - Return module outputs
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Pure business-logic functions
#   - Inline JavaScript
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - volcano_reactive_state.R: init_volcano_state(), reset_volcano_labeling_system()
#     - volcano_module_UI.R: volcano_UI()
#     - volcano_plot_interactive.R: interactive plotly plot functions
#     - volcano_plot_static.R: static plot creation, styling, labels, theming
#     - volcano_data_processing.R: data prep, pairing, axis ranges, search
#     - volcano_export.R: clipboard, download, grid, reset helpers
#     - volcano_observers.R: register_volcano_observers() -- all observers/renders
#   External packages:
#     - shiny: moduleServer, NS, reactive, reactiveValues, reactiveVal
#     - shinyjs: useShinyjs
#
# INTERACTIONS:
#   Called by:
#     - app.R: calls modVolcanoUI() and modVolcanoServer()
#   Calls into:
#     - All volcano sub-scripts (sourced into modEnv)
#   Data flow:
#     - IN:  rv$data_mod, rv$data_def, res_GSEA, GO_res, module_outputs
#     - OUT: list(plots, selected_data, plot_titles) as reactive outputs
#
# LAST UPDATED: 2026-03-10
# ==============================================================================

# Load sub-modules
sys.source("modules/Volcano/volcano_reactive_state.R", envir = modEnv)
sys.source("modules/Volcano/volcano_plot_interactive.R", envir = modEnv)
sys.source("modules/Volcano/volcano_data_processing.R", envir = modEnv)
sys.source("modules/Volcano/volcano_plot_static.R", envir = modEnv)
sys.source("modules/Volcano/volcano_module_UI.R", envir = modEnv)
sys.source("modules/Volcano/volcano_export.R", envir = modEnv)
sys.source("modules/Volcano/volcano_observers_data_choices.R", envir = modEnv)
sys.source("modules/Volcano/volcano_observers_protein_selection.R", envir = modEnv)
sys.source("modules/Volcano/volcano_observers_plot_lifecycle.R", envir = modEnv)
sys.source("modules/Volcano/volcano_observers_selection_restore.R", envir = modEnv)
sys.source("modules/Volcano/volcano_observers.R", envir = modEnv)

modVolcanoUI <- function(id) {
  ns <- NS(id)

  tagList(
    volcano_UI(ns),
    useShinyjs(),
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        // Initialize clipboard functionality
        Shiny.setInputValue('%s', null);
      });
    ", ns("clipboard_ready"))))
  )
}

modVolcanoServer <- function(id, rv, res_GSEA = NULL, GO_res = NULL, module_outputs = NULL, debug_level = 0, modEnv = new.env()) {
  moduleServer(id, function(input, output, session, local = modEnv) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management
    # ========================================

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "VOLCANO", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ VOLCANO ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Volcano module server starting", 1)

    record_restore_report <- function(module_name, report) {
      if (!is.list(rv$restore_reports)) rv$restore_reports <- list()
      rv$restore_reports[[module_name]] <- report
      debug_log(sprintf("[%s] restore report: key=%s hit=%s source=%s reason=%s",
                        module_name,
                        as.character(report$cache_key %||% NA_character_),
                        as.character(report$cache_hit %||% NA),
                        as.character(report$data_source %||% NA_character_),
                        as.character(report$reason %||% NA_character_)), 1)
      if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
        rv$restore_diagnostics <- rv$restore_reports
      }
    }

    # ========================================
    # Data Input Reactives
    # ========================================

    data_in <- reactive({
      if (!is.null(rv$data_mod)) {
        debug_log("Accessing data_mod from rv", 4)
        return(rv$data_mod)
      }
      debug_log("No data available", 4)
      return(NULL)
    })

    data_def_in <- reactive({
      if (!is.null(rv$data_def)) {
        debug_log("Accessing data_def from rv", 4)
        return(rv$data_def)
      }
      debug_log("No metadata available", 4)
      return(NULL)
    })

    # ========================================
    # Reactive State (centralized in volcano_reactive_state.R)
    # ========================================

    vs <- init_volcano_state()

    # Unpack for convenient access throughout the module.
    # All reactive state lives in the 'vs' list; these aliases keep existing
    # code working without requiring a global rename in this WP.
    volcano_state                      <- vs$volcano_state
    plot_update_trigger                <- vs$plot_update_trigger
    selected_data_Volcano              <- vs$selected_data_Volcano
    selected_protein_vector_Volcano    <- vs$selected_protein_vector_Volcano
    volcano_original_plots             <- vs$volcano_original_plots
    volcano_labels                     <- vs$volcano_labels
    protein_label_settings             <- vs$protein_label_settings
    selected_points_interactive_Volcano <- vs$selected_points_interactive

    # Initialize labeling system
    reset_volcano_labeling_system(vs)
    debug_log("Volcano reactive state initialized (centralized)", 2)

    # Pre-register Plotly events so event_data() does not warn when observers
    # are first evaluated at module startup, before renderPlotly has run.
    # event_data() defers its registration check to session$onFlushed and
    # looks up session$userData$plotlyShinyEventIDs. That registry is normally
    # populated by register_plot_events() inside renderPlotly — but renderPlotly
    # has not yet run at startup. Directly writing the event IDs (format used by
    # plotly's register_plot_events: "<event>-<source>") into the session userData
    # replicates what renderPlotly would do and silences the startup warning.
    session$userData$plotlyShinyEventIDs <- unique(c(
      session$userData$plotlyShinyEventIDs,
      c("plotly_selected-volcano_plot", "plotly_click-volcano_plot")
    ))

    # ========================================
    # Register All Observers (volcano_observers.R)
    # ========================================

    register_volcano_observers(
      input, output, session, rv,
      res_GSEA, GO_res, module_outputs,
      volcano_state, plot_update_trigger,
      selected_data_Volcano,
      selected_protein_vector_Volcano,
      volcano_original_plots,
      volcano_labels,
      protein_label_settings,
      selected_points_interactive_Volcano,
      data_in, data_def_in,
      debug_log, ns, modEnv
    )

    # ========================================
    # Session Cleanup
    # ========================================
    cleanup_manager$register_module("Volcano", function() {
      debug_log("Executing [Volcano] cleanup", 2)
      # Heavy ggplot / plotly objects
      volcano_state$static_plots <- NULL
      volcano_state$current_plotly_data <- NULL
      volcano_state$label_storage <- NULL
      volcano_original_plots(list())
      volcano_labels(list())
      # Data frames / vectors
      selected_data_Volcano(data.frame())
      selected_protein_vector_Volcano(character())
      selected_points_interactive_Volcano(data.frame())
      protein_label_settings(data.frame(
        protein_id = character(), label_color = character(),
        dot_color = character(), use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))
      debug_log("[Volcano] cleanup completed", 2)
    })

    # --------------------------------------------------------------------------
    # UI input IDs whose values influence the rendered plot. These are captured
    # at save time and pushed back into the UI after restore.
    # --------------------------------------------------------------------------
    volcano_ui_input_ids <- c(
      "PlotSelect_Volcano",
      "plotTitle_Volcano", "hideTitle_Volcano",
      "plotTitleSize_Volcano", "AxisTitleSize_Volcano", "tickSize_Volcano",
      "ThemeSelect_Volcano",
      "pValueSel_Volcano", "Identifier_Volcano",
      "dotSizeInput_Volcano", "dotColorInput_Volcano",
      "dotSizeInputUp_Volcano", "dotColorInputUp_Volcano",
      "dotSizeInputDown_Volcano", "dotColorInputDown_Volcano",
      "pvalueInput_Volcano", "AbundanceInput_Volcano",
      "xLimInput_Volcano", "xTick_Volcano",
      "yLimInput_Volcano", "yTick_Volcano",
      "cechbox_interactive_Volcano",
      "masterLabelColor_Volcano", "masterDotColor_Volcano",
      "masterCustomDot_Volcano",
      "maxOverlaps_Volcano", "labelDistance_Volcano",
      "lineThickness_Volcano_Label", "labelSize_Volcano",
      "dotSizeLabeled_Volcano",
      "plotWidth_Volcano", "plotHeight_Volcano",
      "downloadFormat_Volcano", "plotWidthInch_Volcano",
      "plotHeightInch_Volcano", "resolution_DPI"
    )

    # ========================================
    # Return Values
    # ========================================

    return(list(
      plots = reactive(volcano_state$static_plots),
      selected_data = reactive(volcano_state$selected_genes),
      plot_titles = reactive(volcano_state$plot_titles),
      get_export_data = function() {
        plots <- isolate(volcano_state$static_plots)
        if (is.null(plots) || length(plots) == 0) return(NULL)

        plot_ui_cache <- tryCatch(isolate(volcano_state$plot_ui_cache), error = function(e) NULL)
        get_input_or_cache <- function(input_name, cache_name = input_name) {
          input_value <- tryCatch(isolate(input[[input_name]]), error = function(e) NULL)
          if (!is.null(input_value)) return(input_value)
          if (is.list(plot_ui_cache) && !is.null(plot_ui_cache[[cache_name]])) return(plot_ui_cache[[cache_name]])
          NULL
        }

        selected_title <- get_input_or_cache("PlotSelect_Volcano")
        if (!is.character(selected_title) || length(selected_title) != 1L ||
            !nzchar(selected_title) || !selected_title %in% names(plots)) {
          selected_title <- names(plots)[1]
        }

        selected_plot <- plots[[selected_title]]
        plot_data <- NULL
        if (!is.null(selected_plot) && !is.null(selected_plot$data) && is.data.frame(selected_plot$data)) {
          plot_data <- selected_plot$data
        }

        if (is.null(plot_data) || nrow(plot_data) == 0) {
          plot_data <- tryCatch(isolate(volcano_state$current_plotly_data), error = function(e) NULL)
        }

        if (is.null(plot_data) || !is.data.frame(plot_data)) plot_data <- data.frame()

        if (nrow(plot_data) > 0 && !("ID" %in% names(plot_data)) && ("identifier" %in% names(plot_data))) {
          plot_data$ID <- plot_data$identifier
        }

        if (nrow(plot_data) > 0 && ("ID" %in% names(plot_data))) {
          dot_size <- if ("point_size" %in% names(plot_data)) {
            plot_data$point_size
          } else if ("size" %in% names(plot_data)) {
            plot_data$size
          } else {
            rep(NA_real_, nrow(plot_data))
          }

          export_df <- data.frame(
            Identifier = as.character(plot_data$ID),
            log2_abundance_ratio = suppressWarnings(as.numeric(plot_data$x)),
            neg_log10_p_value = suppressWarnings(as.numeric(plot_data$y)),
            color = if ("point_color" %in% names(plot_data)) as.character(plot_data$point_color) else if ("color" %in% names(plot_data)) as.character(plot_data$color) else NA_character_,
            dot_size = suppressWarnings(as.numeric(dot_size)),
            stringsAsFactors = FALSE
          )
        } else {
          export_df <- data.frame(
            Identifier = character(0), log2_abundance_ratio = numeric(0),
            neg_log10_p_value = numeric(0), color = character(0), dot_size = numeric(0),
            stringsAsFactors = FALSE
          )
        }

        list(
          plot_title = selected_title,
          abundance_ratio_threshold = suppressWarnings(as.numeric(get_input_or_cache("AbundanceInput_Volcano"))),
          p_value_threshold = suppressWarnings(as.numeric(get_input_or_cache("pvalueInput_Volcano"))),
          x_axis_range = get_input_or_cache("xLimInput_Volcano"),
          y_axis_range = get_input_or_cache("yLimInput_Volcano"),
          selected_identifier = get_input_or_cache("Identifier_Volcano"),
          identifier_column = get_input_or_cache("Identifier_Volcano"),
          data = export_df
        )
      },



      .to_lightweight_plot_snapshot <- function(plot_obj) {
        # Deprecated compatibility hook. Volcano session snapshots must not
        # export ggplot-derived bundles; restore rebuilds plots from the cached
        # data/metadata pair plus cached UI and the cache key.
        NULL
      },

      .restore_plot_snapshot_compat <- function(plot_snapshot) {
        # Deprecated compatibility hook. Legacy ggplot-derived snapshots are no
        # longer authoritative for Volcano restore; use plot data cache instead.
        NULL
      },

      # Session save/restore interface
      get_session_state = function() {
        state <- list(
          version = "2.0",
          restore_cache_dependency = "shared_plot_data_cache_pool"
        )
        current_plots <- tryCatch(isolate(volcano_state$static_plots), error = function(e) NULL)
        current_titles <- tryCatch(names(current_plots), error = function(e) character(0))
        if (is.null(current_titles) || length(current_titles) == 0L) {
          current_titles <- tryCatch(isolate(volcano_state$plot_titles), error = function(e) character(0))
        }

        ui_inputs <- tryCatch({
          vals <- lapply(volcano_ui_input_ids, function(id) isolate(input[[id]]))
          names(vals) <- volcano_ui_input_ids
          vals
        }, error = function(e) NULL)
        plot_ui_cache <- tryCatch({
          pu <- volcano_state$plot_ui_cache
          if (!is.list(pu)) pu <- list()
          if (is.list(ui_inputs)) utils::modifyList(pu, ui_inputs, keep.null = TRUE) else pu
        }, error = function(e) NULL)
        selected_title <- tryCatch({
          as.character((ui_inputs$PlotSelect_Volcano %||% plot_ui_cache$PlotSelect_Volcano %||% current_titles[1]) %||% NA_character_)[1]
        }, error = function(e) NA_character_)

        state$ui_inputs <- ui_inputs
        state$plot_ui_inputs <- plot_ui_cache
        state$selected_plot_title <- selected_title
        had_static_plots <- length(current_titles) > 0L
        valid_cache_pair <- function(candidate) {
          is.list(candidate) && is.data.frame(candidate$data_mod) &&
            is.data.frame(candidate$data_def)
        }
        cached_pair <- NULL
        cache_capture_source <- NULL
        if (isTRUE(had_static_plots)) {
          creation_cache <- tryCatch(isolate(volcano_state$plot_creation_cache), error = function(e) NULL)
          restore_cache <- tryCatch(isolate(volcano_state$restore_plot_data_cache), error = function(e) NULL)
          if (valid_cache_pair(creation_cache)) {
            cached_pair <- creation_cache
            cache_capture_source <- "plot_creation_cache"
          } else if (valid_cache_pair(restore_cache)) {
            cached_pair <- restore_cache
            cache_capture_source <- "restore_plot_data_cache"
          } else {
            live_pair <- tryCatch(
              list(data_mod = isolate(data_in()), data_def = isolate(data_def_in())),
              error = function(e) NULL
            )
            plotted_signature <- tryCatch(isolate(volcano_state$source_data_signature), error = function(e) NA_character_)
            live_signature <- if (valid_cache_pair(live_pair)) {
              .volcano_data_signature(live_pair$data_mod, live_pair$data_def)
            } else {
              NA_character_
            }
            if (is.character(plotted_signature) && length(plotted_signature) == 1L &&
                !is.na(plotted_signature) && nzchar(plotted_signature) &&
                identical(plotted_signature, live_signature)) {
              cached_pair <- live_pair
              cache_capture_source <- "matching_live_pair"
            }
          }
        }
        valid_cache_ref <- function(ref) {
          is.character(ref) && length(ref) == 1L && !is.na(ref) && nzchar(ref)
        }
        cache_contract <- tryCatch({
          if (valid_cache_pair(cached_pair)) {
            .plot_data_cache_ref_contract(
              data_mod_revision_id = isolate(rv$data_mod_revision_id),
              data_def_revision_id = isolate(rv$data_def_revision_id),
              data_mod = cached_pair$data_mod,
              data_def = cached_pair$data_def
            )
          } else {
            NULL
          }
        }, error = function(e) NULL)
        cache_ref <- cache_contract$plot_data_cache_ref %||% NA_character_
        state$plot_data_cache_ref <- if (valid_cache_ref(cache_ref)) cache_ref else NULL
        if (is.list(cache_contract)) state[names(cache_contract)] <- cache_contract
        state$plot_data_cache_payload <- cached_pair
        state$cache_capture_source <- cache_capture_source
        if (isTRUE(had_static_plots) && !valid_cache_ref(state$plot_data_cache_ref)) {
          state$cache_capture_failed <- TRUE
          state$cache_capture_diagnostic <- "No compatible Volcano plot input data/metadata pair was available at snapshot time."
        }
        state$current_pairs <- isolate(volcano_state$current_pairs)
        state$plot_requests_by_title <- tryCatch({
          requests <- lapply(current_titles, function(title) {
            list(
              title = title,
              ui_inputs = plot_ui_cache,
              current_pairs = isolate(volcano_state$current_pairs),
              axis_settings = tryCatch(volcano_state$plot_axis_settings[[title]], error = function(e) NULL)
            )
          })
          names(requests) <- current_titles
          requests
        }, error = function(e) list())
        # Do not persist ggplot-derived plot data. Volcano restores are rebuilt
        # from plot_data_cache_ref / plot_data_cache_payload plus cached UI.
        state$labels_by_title <- tryCatch(isolate(volcano_labels()), error = function(e) list())
        state$volcano_labels <- state$labels_by_title
        state$protein_label_settings <- tryCatch(isolate(protein_label_settings()), error = function(e) NULL)
        state$selected_genes <- isolate(volcano_state$selected_genes)
        state$selected_data_Volcano <- tryCatch(isolate(selected_data_Volcano()), error = function(e) NULL)
        state$selected_protein_vector_Volcano <- tryCatch(isolate(selected_protein_vector_Volcano()), error = function(e) NULL)
        state$selected_points_interactive <- tryCatch(isolate(selected_points_interactive_Volcano()), error = function(e) NULL)
        state$auto_range_set <- isolate(volcano_state$auto_range_set)
        state$manual_axis_override <- isolate(volcano_state$manual_axis_override)
        state$plot_titles <- current_titles
        state$had_static_plots <- had_static_plots
        state$restore_cache_dependency <- if (isTRUE(had_static_plots)) {
          "shared_plot_data_cache_pool"
        } else "none"
        state$plot_cache_ref_by_title <- tryCatch({
          if (length(current_titles) > 0L && valid_cache_ref(cache_ref)) {
            refs_by_title <- vector("list", length(current_titles))
            names(refs_by_title) <- vapply(current_titles, function(title) {
              .build_canonical_plot_cache_key("volcano", title, "main")
            }, character(1L))
            for (i in seq_along(refs_by_title)) {
              refs_by_title[[i]] <- cache_ref
            }
            refs_by_title
          } else if (isTRUE(had_static_plots)) {
            volcano_state$plot_cache_ref_by_title
          } else NULL
        }, error = function(e) NULL)
        if (!isTRUE(had_static_plots)) {
          state$plot_data_cache_ref <- NULL
          state$plot_data_cache_payload <- NULL
          state$plot_cache_ref_by_title <- NULL
        }
        # Schema 2.0 does not persist restore_plot_data_cache or rendered plot
        # objects; those fields are legacy restore-only compatibility.
        # Intentionally exclude ggplot, plotly, rendered plot objects, and
        # ggplot-derived plot data in schema 2.0.
        state
      },
      set_session_state = function(state, phase = NULL) {
        canonical_plot_key <- function(plot_title = NULL) {
          .build_canonical_plot_cache_key(
            module = "volcano",
            logical_plot_id = as.character(plot_title %||% "default")[1],
            variant = "main"
          )
        }
        legacy_plot_key <- function(plot_title = NULL) {
          as.character(plot_title %||% "default")[1]
        }
        if (is.null(state)) return()

        # A phased restore hydrates the authoritative module state exactly once
        # in full_module_state.  The plots phase is deliberately bookkeeping
        # only: the global session restore trigger is the single reconstruction
        # boundary after Data Wizard has finished publishing its restored data.
        # Direct callers which omit phase retain the legacy one-call contract.
        if (identical(phase, "full_module_plots")) {
          volcano_state$restore_rebuild_requested <- isTRUE(state$had_static_plots)
          debug_log(sprintf(
            "[Volcano] reconstruction recorded for session restore trigger (requested=%s)",
            as.character(isTRUE(volcano_state$restore_rebuild_requested))
          ), 1)
          return()
        }
        if (!is.null(phase) && !identical(phase, "full_module_state")) return()

        if (!is.list(state$restore_plot_data_cache) && is.list(state$plot_data_cache_payload)) {
          state$restore_plot_data_cache <- state$plot_data_cache_payload
        }
        volcano_state$restore_in_progress <- TRUE
        selected_cache_key <- NA_character_
        used_legacy_fallback <- FALSE
        cache_hit <- FALSE
        cache_hit_reason <- "cache_miss_fallback_live"
        cache_intended_restore <- isTRUE(.module_restore_has_cache_intent(state))
        module_cache_candidate <- state$restore_plot_data_cache
        restore_cache_resolved <- isTRUE(state$restore_cache_resolved)
        restore_cache_mode <- as.character(state$restore_cache_resolution_mode %||% "none")[1]
        module_cache_valid <- is.list(module_cache_candidate) &&
          is.data.frame(module_cache_candidate$data_mod) &&
          is.data.frame(module_cache_candidate$data_def)
        if (isTRUE(module_cache_valid)) {
          cache_hit <- TRUE
          selected_cache_key <- as.character(state$plot_data_cache_ref %||% "<module_ref>")
          cache_hit_reason <- "cache_restored_module_ref"
        } else if (isTRUE(restore_cache_resolved) && identical(restore_cache_mode, "by_title") &&
                   is.list(state$restore_plot_data_cache_by_title) && is.list(state$plot_ui_inputs)) {
          canonical_key <- canonical_plot_key(state$plot_ui_inputs$PlotSelect_Volcano)
          selected_cache_key <- canonical_key
          cache_candidate <- state$restore_plot_data_cache_by_title[[canonical_key]]
          if (!is.list(cache_candidate)) {
            legacy_key <- legacy_plot_key(state$plot_ui_inputs$PlotSelect_Volcano)
            selected_cache_key <- legacy_key
            used_legacy_fallback <- TRUE
            cache_candidate <- state$restore_plot_data_cache_by_title[[legacy_key]]
          }
          candidate_valid <- is.list(cache_candidate) &&
            is.data.frame(cache_candidate$data_mod) &&
            is.data.frame(cache_candidate$data_def)
          if (isTRUE(candidate_valid)) {
            state$restore_plot_data_cache <- cache_candidate
            cache_hit <- TRUE
            cache_hit_reason <- "cache_restored"
          }
          debug_log(sprintf(
            "[Volcano] restore cache lookup key='%s' fallback_used=%s",
            as.character(selected_cache_key %||% "<none>"),
            if (isTRUE(used_legacy_fallback)) "TRUE" else "FALSE"
          ), 2)
        }
        degraded_cache_restore <- isTRUE(cache_intended_restore) && !isTRUE(cache_hit) &&
          !isTRUE(.module_restore_live_contract_compatible(state, rv))
        if (isTRUE(degraded_cache_restore)) {
          state$restore_plot_data_cache <- NULL
        }
        volcano_state$restore_plot_data_cache <- if (is.list(state$restore_plot_data_cache)) state$restore_plot_data_cache else NULL
        if (is.list(volcano_state$restore_plot_data_cache)) {
          volcano_state$plot_creation_cache <- volcano_state$restore_plot_data_cache
        }
        volcano_state$restore_plot_data_cache_by_title <- if (is.list(state$restore_plot_data_cache_by_title)) state$restore_plot_data_cache_by_title else NULL
        volcano_state$plot_cache_ref_by_title <- if (is.list(state$plot_cache_ref_by_title)) state$plot_cache_ref_by_title else NULL
        if (is.list(state$plot_ui_inputs) && is.list(state$ui_inputs) && !is.null(state$ui_inputs$hideTitle_Volcano)) {
          state$plot_ui_inputs$hideTitle_Volcano <- isTRUE(state$ui_inputs$hideTitle_Volcano)
        }
        volcano_state$plot_ui_cache <- if (is.list(state$plot_ui_inputs)) state$plot_ui_inputs else NULL
        if (!is.null(state$current_pairs))      volcano_state$current_pairs <- state$current_pairs
        if (!is.null(state$selected_genes))      volcano_state$selected_genes <- state$selected_genes
        if (!is.null(state$auto_range_set))      volcano_state$auto_range_set <- state$auto_range_set
        if (!is.null(state$manual_axis_override)) volcano_state$manual_axis_override <- state$manual_axis_override
        if (!is.null(state$labels_by_title))      volcano_labels(state$labels_by_title)
        else if (!is.null(state$volcano_labels))      volcano_labels(state$volcano_labels)
        if (!is.null(state$protein_label_settings)) protein_label_settings(state$protein_label_settings)
        if (!is.null(state$selected_data_Volcano)) selected_data_Volcano(state$selected_data_Volcano)
        if (!is.null(state$selected_protein_vector_Volcano)) selected_protein_vector_Volcano(state$selected_protein_vector_Volcano)
        if (!is.null(state$selected_points_interactive)) selected_points_interactive_Volcano(state$selected_points_interactive)
        # Schema 2.0 treats saved ggplot/plotly/rendered objects and
        # ggplot-derived plot data as non-authoritative.
        volcano_state$static_plots <- NULL
        volcano_state$plot_data_by_title <- NULL
        volcano_state$plot_requests_by_title <- if (is.list(state$plot_requests_by_title)) state$plot_requests_by_title else NULL
        if (!is.null(state$plot_titles))   volcano_state$plot_titles  <- state$plot_titles
        volcano_state$had_static_plots_on_save <- isTRUE(state$had_static_plots)
        volcano_state$restore_rebuild_requested <- is.null(phase) && isTRUE(state$had_static_plots)
        # Retain only the lightweight contract required to decide whether the
        # final, post-preprocessing live pair is a compatible fallback.
        volcano_state$restore_cache_contract <- list(
          plot_data_cache_ref = state$plot_data_cache_ref,
          plot_cache_ref_by_title = state$plot_cache_ref_by_title,
          data_mod_revision_id = state$data_mod_revision_id,
          data_def_revision_id = state$data_def_revision_id,
          plot_data_cache_fingerprint = state$plot_data_cache_fingerprint
        )
        if (!is.null(state$ui_inputs) && is.list(state$ui_inputs)) {
          restored_title <- state$selected_plot_title %||% state$ui_inputs[["PlotSelect_Volcano"]]
          volcano_state$preferred_plot_title <- if (is.character(restored_title) && length(restored_title) == 1L && nzchar(restored_title)) restored_title else NULL
        }
        # Stage captured UI inputs for the session_restore_trigger observer.
        if (!is.null(state$ui_inputs) && is.list(state$ui_inputs)) {
          volcano_state$pending_ui_inputs <- state$ui_inputs
        }
        report <- list(
          cache_key = selected_cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else "live",
          reason = if (isTRUE(cache_hit)) cache_hit_reason else if (isTRUE(degraded_cache_restore)) "cache_ref_unresolved_live_data_incompatible" else "cache_miss_fallback_live",
          restore_cache_degraded = isTRUE(degraded_cache_restore)
        )
        record_restore_report("Volcano", report)
        debug_log("[Volcano] session state restored via set_session_state", 1)
      }
    ))
  })
}
