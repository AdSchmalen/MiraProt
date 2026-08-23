# ==============================================================================
# File: modules/sampleids_module.R
#
# Purpose:
#   Orchestrator for the Sample IDs module. Sources all sub-files, defines the
#   UI wrapper (modSampleIDsUI), and wires the server function
#   (modSampleIDsServer). This is the only file in the module that contains a
#   server function.
#
# Architectural Role:
#   This file delegates concerns to four sub-files inside sampleids/:
#     - sampleids_logic.R    : Pure logic functions (no Shiny dependency)
#     - sampleids_state.R    : Reactive state factory (create_sampleids_state)
#     - sampleids_observer.R : All observe() / observeEvent() blocks and outputs
#     - sampleids_ui.R       : Static UI layout and input widgets
#
# Structure:
#   1. Source sub-files into modEnv
#   2. modSampleIDsUI()     - UI wrapper calling sampleids_UI()
#   3. modSampleIDsServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_sampleids_state()
#      c. Observer registration via register_sampleids_observers()
#      d. Return interface
#
# Return Interface (public API):
#   ggplot_object_SampleIDTab : reactiveVal holding the current ggplot2 object
#
# Notes for future developers:
#   - No server logic lives in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_sampleids_observers.
#   - debug_log is defined here and passed explicitly to any function that logs.
#   - No explicit session cleanup is registered because the module holds only
#     session-local reactive values that are garbage-collected automatically
#     when the session ends. Register cleanup via cleanup_manager if persistent
#     external resources are added in the future.
# ==============================================================================

source("modules/sampleids/sampleids_logic.R",    local = modEnv)
source("modules/sampleids/sampleids_state.R",    local = modEnv)
source("modules/sampleids/sampleids_observer.R", local = modEnv)
source("modules/sampleids/sampleids_ui.R",       local = modEnv)


# ==============================================================================
# UI
# ==============================================================================

modSampleIDsUI <- function(id) {
  ns <- NS(id)
  sampleids_UI(ns)
}


# ==============================================================================
# SERVER
# ==============================================================================

modSampleIDsServer <- function(id, rv, debug_level = 0) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "SAMPLEIDS", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ SAMPLEIDS ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Sample IDs module server starting", 1)

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

    # --------------------------------------------------------------------------
    # State initialization
    # --------------------------------------------------------------------------

    state <- create_sampleids_state()

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------

    register_sampleids_observers(
      input     = input,
      output    = output,
      session   = session,
      ns        = ns,
      state     = state,
      rv        = rv,
      debug_log = debug_log
    )

    debug_log("Sample IDs module initialized successfully", 1)

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    # All input widget IDs in this module that should be saved and restored.
    # Dynamic-choice inputs (FileSample, data, Sort) are included here so their
    # selected values are captured; the actual restoration of those inputs is
    # handled inside sampleids_observer.R where the choices are repopulated.
    sampleids_ui_input_ids <- c(
      "checkbox_interactive_SampleIDTab",
      "FileSample_SampleIDTab", "data_SampleIDTab",
      "Sort_SampleIDTab",
      "AbsRel_SampleIDTab", "NumericPlotType_SampleIDTab", "Transform_SampleIDTab",
      "label_SampleIDTab", "ThemeSelect_SampleIDTab", "col_SampleIDTab",
      "col_reverse_SampleIDTab", "plotTitle_SampleIDTab",
      "hideTitle_SampleIDTab", "TitleSize_SampleIDTab", "AxisTitleSize_SampleIDTab", "tickSize_SampleIDTab",
      "LegendTitleSize_SampleIDTab", "LegendTextSize_SampleIDTab",
      "sampleIDs_legend_position",
      "resolution_DPI_SampleIDTab", "plotWidthInch_SampleIDTab",
      "plotHeightInch_SampleIDTab", "downloadFormat_SampleIDTab"
    )

    list(
      ggplot_object_SampleIDTab = state$ggplot_object_SampleIDTab,

      # Session save/restore interface.
      # Saves UI input values and a lightweight "had plot" flag. The actual
      # plot is rebuilt during restore from rv$data_mod + restored inputs to
      # avoid serializing bulky ggplot objects.
      get_session_state = function() {
        ui_inputs <- tryCatch({
          vals <- lapply(sampleids_ui_input_ids, function(id) isolate(input[[id]]))
          names(vals) <- sampleids_ui_input_ids
          vals
        }, error = function(e) NULL)

        plot_ui_cache <- tryCatch({
          pu <- isolate(state$plot_ui_cache())
          if (is.list(pu)) pu else NULL
        }, error = function(e) NULL)

        # Persist the complete UI snapshot used by build_sampleids_plot_request().
        # Prefer the last plot-creation snapshot, then fill any missing fields
        # from current inputs so hidden/default controls are not dropped.
        request_source <- if (is.list(plot_ui_cache)) {
          utils::modifyList(ui_inputs %||% list(), plot_ui_cache)
        } else {
          ui_inputs
        }
        plot_request <- tryCatch({
          build_sampleids_plot_request(request_source %||% list())
        }, error = function(e) NULL)

        cached_pair <- tryCatch({
          cached <- isolate(state$plot_creation_cache())
          if (is.list(cached) && inherits(cached$data_mod, "data.frame") &&
              inherits(cached$data_def, "data.frame")) cached else NULL
        }, error = function(e) NULL)
        plot_data_cache_ref <- tryCatch({
          if (is.list(cached_pair)) {
            .build_plot_data_cache_id(data_mod = cached_pair$data_mod, data_def = cached_pair$data_def)
          } else {
            ref <- isolate(state$plot_data_cache_ref())
            if (is.character(ref) && length(ref) == 1L && nzchar(ref)) ref else NA_character_
          }
        }, error = function(e) NA_character_)

        debug_log(sprintf(
          "[SampleIDs] session save cache references | plot_data_cache_ref=%s | payload=%s",
          if (is.character(plot_data_cache_ref) && length(plot_data_cache_ref) == 1L &&
              !is.na(plot_data_cache_ref) && nzchar(plot_data_cache_ref)) plot_data_cache_ref else "<missing-or-malformed>",
          if (is.list(cached_pair)) "present" else "absent"
        ), 1)

        canonical_key <- tryCatch({
          .build_canonical_plot_cache_key(
            module = "sampleids",
            logical_plot_id = as.character(request_source$plotTitle_SampleIDTab %||% plot_request$title %||% "default")[1],
            variant = "main"
          )
        }, error = function(e) "sampleids::default::main")
        cache_ref_trace <- if (is.character(plot_data_cache_ref) && length(plot_data_cache_ref) == 1L &&
            !is.na(plot_data_cache_ref) && nzchar(plot_data_cache_ref)) plot_data_cache_ref else "<none>"
        debug_log(sprintf(
          "[SampleIDs] session save cache-reference fields | plot_data_cache_ref=%s | plot_cache_ref_by_title[%s]=%s",
          cache_ref_trace, canonical_key, cache_ref_trace
        ), 1)

        had_plot <- tryCatch({
          p <- isolate(state$ggplot_object_SampleIDTab())
          !is.null(p) && inherits(p, "ggplot")
        }, error = function(e) FALSE)

        list(
          version = "2.0",
          restore_cache_dependency = if (isTRUE(had_plot)) "shared_plot_data_cache_pool" else "none",
          ui_inputs = ui_inputs,
          plot_ui_inputs = request_source,
          plot_request = plot_request,
          plot_data_cache_ref = if (isTRUE(had_plot)) plot_data_cache_ref else NULL,
          # Transient save-time payload: session orchestration moves this exact
          # plot-creation pair into plot_data_cache_pool and clears the embedded
          # frames from the persisted module snapshot.
          plot_data_cache_payload = if (isTRUE(had_plot)) cached_pair else NULL,
          plot_cache_ref_by_title = if (isTRUE(had_plot) &&
              is.character(plot_data_cache_ref) && length(plot_data_cache_ref) == 1L &&
              !is.na(plot_data_cache_ref) && nzchar(plot_data_cache_ref)) {
            stats::setNames(list(plot_data_cache_ref), canonical_key)
          } else NULL,
          plot_ui_cache = request_source,
          had_plot = had_plot
        )
      },
      set_session_state = function(s) {
        canonical_plot_key <- function(plot_title = NULL) {
          .build_canonical_plot_cache_key(
            module = "sampleids",
            logical_plot_id = as.character(plot_title %||% "default")[1],
            variant = "main"
          )
        }
        legacy_plot_key <- function(plot_title = NULL) {
          as.character(plot_title %||% "default")[1]
        }
        if (is.null(s)) return()

        # Legacy/new compatibility: normalize older plot_ui_inputs and the 2.0
        # plot_request into the UI snapshot used by the restore observer.
        if (!is.list(s$plot_ui_inputs)) {
          if (is.list(s$plot_request)) {
            s$plot_ui_inputs <- sampleids_plot_request_to_ui(s$plot_request)
          } else if (is.list(s$plot_ui_cache)) {
            s$plot_ui_inputs <- s$plot_ui_cache
          }
          if (is.list(s$plot_ui_cache)) {
            s$plot_ui_inputs <- utils::modifyList(s$plot_ui_cache, s$plot_ui_inputs %||% list())
          }
        }

        had_plot_on_save <- isTRUE(s$had_plot) ||
          (!is.null(s$ggplot_object) && inherits(s$ggplot_object, "ggplot"))
        raw_cache_ref <- s$plot_data_cache_ref
        cache_ref_status <- if (is.null(raw_cache_ref)) {
          "missing"
        } else if (!is.character(raw_cache_ref) || length(raw_cache_ref) != 1L ||
                   is.na(raw_cache_ref) || !nzchar(raw_cache_ref)) {
          "malformed"
        } else {
          "unresolved"
        }
        cache_key <- if (identical(cache_ref_status, "unresolved")) raw_cache_ref else NA_character_
        if (!is.list(s$restore_plot_data_cache) && is.list(s$plot_data_cache_payload)) {
          s$restore_plot_data_cache <- s$plot_data_cache_payload
        }
        cache_hit <- is.list(s$restore_plot_data_cache) &&
          inherits(s$restore_plot_data_cache$data_mod, "data.frame") &&
          inherits(s$restore_plot_data_cache$data_def, "data.frame")
        if (isTRUE(cache_hit) && identical(cache_ref_status, "unresolved")) cache_ref_status <- "resolved"
        cache_hit_reason <- if (isTRUE(cache_hit)) "cache_restored_module_ref" else "cache_miss_fallback_live"
        if (is.list(s$restore_plot_data_cache_by_title) && is.list(s$plot_ui_inputs)) {
          key <- canonical_plot_key(s$plot_ui_inputs$plotTitle_SampleIDTab)
          cache_key <- if (is.na(cache_key) || !nzchar(cache_key)) key else cache_key
          cand <- s$restore_plot_data_cache_by_title[[key]]
          if (!is.list(cand)) {
            legacy_key <- legacy_plot_key(s$plot_ui_inputs$plotTitle_SampleIDTab)
            cand <- s$restore_plot_data_cache_by_title[[legacy_key]]
          }
          if (is.list(cand) &&
              inherits(cand$data_mod, "data.frame") &&
              inherits(cand$data_def, "data.frame")) {
            s$restore_plot_data_cache <- cand; cache_hit <- TRUE; cache_hit_reason <- "cache_restored_by_title"
            if (identical(cache_ref_status, "unresolved")) cache_ref_status <- "resolved"
          }
        }
        live_contract_compatible <- isTRUE(.module_restore_live_contract_compatible(s, rv))
        # Live fallback is plot-faithful only when both the saved/current data
        # and metadata contracts match. A reference problem never relaxes that
        # requirement.
        degraded_cache_restore <- isTRUE(had_plot_on_save) && !isTRUE(cache_hit) &&
          !isTRUE(live_contract_compatible)
        if (isTRUE(degraded_cache_restore)) {
          s$restore_plot_data_cache <- NULL
          had_plot_on_save <- FALSE
        }
        if (is.list(s$plot_ui_inputs)) {
          if (is.list(s$ui_inputs) && !is.null(s$ui_inputs$hideTitle_SampleIDTab)) {
            s$plot_ui_inputs$hideTitle_SampleIDTab <- isTRUE(s$ui_inputs$hideTitle_SampleIDTab)
          }
          state$plot_ui_cache(s$plot_ui_inputs)
        } else {
          state$plot_ui_cache(NULL)
        }
        state$plot_request(if (is.list(s$plot_request)) s$plot_request else NULL)
        state$plot_data_cache_ref(if (!is.na(cache_key) && nzchar(cache_key)) cache_key else NULL)
        if (is.list(s$restore_plot_data_cache)) {
          state$restore_plot_data_cache(s$restore_plot_data_cache)
        } else {
          state$restore_plot_data_cache(NULL)
        }
        state$had_plot_on_save(isTRUE(had_plot_on_save))
        # Clear rendered objects; restore rebuilds them from the saved request/UI
        # and resolved cache/live data. Legacy ggplot snapshots are the only
        # fallback that may be displayed without a rebuild.
        state$ggplot_object_SampleIDTab(NULL)
        state$plotly_object_SampleIDTab(NULL)
        if (!isTRUE(s$had_plot) && !is.null(s$ggplot_object) &&
            inherits(s$ggplot_object, "ggplot")) {
          state$ggplot_object_SampleIDTab(s$ggplot_object)
        }
        staged_inputs <- if (is.list(s$ui_inputs)) s$ui_inputs else list()
        if (is.list(s$plot_ui_inputs)) staged_inputs <- utils::modifyList(staged_inputs, s$plot_ui_inputs)
        if (length(staged_inputs) > 0L) state$pending_ui_inputs(staged_inputs)
        report <- list(
          cache_key = cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else if (isTRUE(degraded_cache_restore)) "none" else "live",
          reason = if (isTRUE(cache_hit)) cache_hit_reason else if (isTRUE(degraded_cache_restore)) {
            paste0("cache_ref_", cache_ref_status, "_live_data_metadata_incompatible")
          } else {
            paste0("cache_ref_", cache_ref_status, "_fallback_live_compatible")
          },
          cache_ref_status = cache_ref_status,
          live_data_metadata_compatible = live_contract_compatible,
          restore_cache_degraded = isTRUE(degraded_cache_restore)
        )
        debug_log(sprintf(
          "[SampleIDs] session restore cache references | plot_data_cache_ref=%s | status=%s | live_data_metadata_compatible=%s",
          if (is.na(cache_key)) "<none>" else cache_key, cache_ref_status,
          live_contract_compatible
        ), 1)
        if ((isTRUE(had_plot_on_save) || isTRUE(degraded_cache_restore)) && !isTRUE(cache_hit)) {
          showNotification(if (isTRUE(degraded_cache_restore)) .restore_cache_unavailable_dataset_mismatch_warning else "SampleIDs restored using current dataset (cached plot data unavailable).", type = "warning", duration = 6)
        }
        record_restore_report("SampleIDs", report)
        debug_log("[SampleIDs] session state restored via set_session_state", 1)
      }

    )

  })
}
