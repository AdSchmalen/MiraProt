# ==============================================================================
# File: modules/abundances_module.R
#
# Purpose:
#   Orchestrator for the Abundances module. Sources all sub-files, defines the
#   UI wrapper (modAbundancesUI), and wires the server function
#   (modAbundancesServer). This is the only file in the module that contains a
#   server function.
#
# Architectural Role:
#   This file delegates concerns to four sub-files inside abundances/:
#     - abundances_logic.R    : Pure logic functions (no Shiny dependency)
#     - abundances_state.R    : Reactive state factory (create_abundances_state)
#     - abundances_observer.R : All observe() / observeEvent() blocks and outputs
#     - abundances_ui.R       : Static UI layout and input widgets
#
# Structure:
#   1. Source sub-files into modEnv
#   2. modAbundancesUI()    - UI wrapper calling abundances_UI()
#   3. modAbundancesServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_abundances_state()
#      c. Observer registration via register_abundances_observers()
#      d. Return interface
#
# Return Interface (public API):
#   ggplot_object_abundanceTab : reactiveVal holding the current ggplot2 object
#   plot_object_abundanceTab   : reactiveVal holding the current plotly object
#
# Notes for future developers:
#   - No server logic lives in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_abundances_observers.
#   - debug_log is defined here and passed explicitly to any function that logs.
#   - No explicit session cleanup is registered because the module holds only
#     session-local ggplot/plotly objects that are garbage-collected automatically
#     when the session ends. Register cleanup via cleanup_manager if persistent
#     external resources are added in the future.
# ==============================================================================

source("modules/abundances/abundances_logic.R",    local = modEnv)
source("modules/abundances/abundances_state.R",    local = modEnv)
source("modules/abundances/abundances_observer.R", local = modEnv)
source("modules/abundances/abundances_ui.R",       local = modEnv)


# ==============================================================================
# UI
# ==============================================================================

modAbundancesUI <- function(id) {
  ns <- NS(id)
  abundances_UI(ns)
}


# ==============================================================================
# SERVER
# ==============================================================================

modAbundancesServer <- function(id, rv, debug_level = 0) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "ABUNDANCES", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ ABUNDANCES ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Abundances module server starting", 1)

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

    state <- create_abundances_state()

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------

    register_abundances_observers(
      input     = input,
      output    = output,
      session   = session,
      ns        = ns,
      state     = state,
      rv        = rv,
      debug_log = debug_log
    )

    debug_log("Abundances module initialized successfully", 1)

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    # All input widget IDs in this module that should be saved and restored.
    # data_abundanceTab has dynamic choices populated by an observer; its
    # selection is restored inside abundances_observer.R where the choices are
    # repopulated.
    abundances_ui_input_ids <- c(
      "checkbox_interactive_AbundanceTab",
      "data_abundanceTab", "label_abundanceTab",
      "col_abundanceTab", "ThemeSelect_Abundance",
      "plotTitle_Abundance", "hideTitle_Abundance", "PlotTitleSize_Abundances",
      "AxisTitleSize_Abundances", "tickSize_Abundance",
      "resolution_DPI_Abundances", "plotWidthInch_Abundances",
      "plotHeightInch_Abundances", "downloadFormat_Abundances"
    )

    list(
      ggplot_object_abundanceTab = state$ggplot_object_abundanceTab,
      plot_object_abundanceTab   = state$plot_object_abundanceTab,

      # Session save/restore interface.
      # Saves UI input values and a lightweight "had plot" flag.
      # The plot itself is rebuilt on restore from restored data + UI.
      get_session_state = function() {
        # Cache intent belongs to the plot that was actually captured, rather
        # than to whatever dataset happens to be live when the session is saved.
        had_plot <- tryCatch({
          p <- isolate(state$ggplot_object_abundanceTab())
          !is.null(p) && inherits(p, "ggplot")
        }, error = function(e) FALSE)
        canonical_plot_key <- function(data_type = NULL) {
          .build_canonical_plot_cache_key(
            module = "abundances",
            logical_plot_id = as.character(data_type %||% "default")[1],
            variant = "main"
          )
        }
        ui_inputs <- tryCatch({
          vals <- lapply(abundances_ui_input_ids, function(id) isolate(input[[id]]))
          names(vals) <- abundances_ui_input_ids
          vals
        }, error = function(e) NULL)
        plot_ui_snapshot <- tryCatch({
          pu <- isolate(state$plot_ui_cache())
          if (!is.list(pu)) list() else pu
        }, error = function(e) list())
        plot_ui_cache <- tryCatch({
          # Plot replay must use the UI values captured when the plot was built,
          # not current live widgets that may now reflect a different Data Wizard
          # dataset. Keep the complete snapshot lightweight (scalars only).
          plot_ui_snapshot
        }, error = function(e) NULL)
        cached_pair <- tryCatch({
          cached <- isolate(state$plot_creation_cache())
          if (.is_plot_cache_pair(cached)) cached else NULL
        }, error = function(e) NULL)
        if (isTRUE(had_plot) && !.is_plot_cache_pair(cached_pair)) {
          stop(paste(
            "Abundances cache-integrity failure:",
            "the saved plot has no valid plot_creation_cache data_mod/data_def pair"
          ), call. = FALSE)
        }
        selected_abundance <- as.character(
          plot_ui_snapshot$data_abundanceTab %||% ui_inputs$data_abundanceTab %||% ""
        )[1]
        label_mode <- as.character(
          plot_ui_snapshot$label_abundanceTab %||% ui_inputs$label_abundanceTab %||% "Column name"
        )[1]
        selected_samples <- tryCatch({
          if (!is.list(cached_pair) || !nzchar(selected_abundance)) character(0) else {
            idx <- which(trimws(gsub("[[:space:]]+", " ", as.character(cached_pair$data_def$Content))) ==
                           trimws(gsub("[[:space:]]+", " ", selected_abundance)))
            if (length(idx) == 0L) character(0) else {
              if (identical(label_mode, "Column name")) {
                as.character(cached_pair$data_def$Column[idx])
              } else {
                as.character(cached_pair$data_def$Sample[idx])
              }
            }
          }
        }, error = function(e) character(0))
        plot_request <- list(
          abundance_type = selected_abundance,
          selected_samples = selected_samples,
          selected_groups = character(0),
          transform_scaling = list(
            metadata_transformation = tryCatch({
              if (!is.list(cached_pair) || !nzchar(selected_abundance)) character(0) else {
                idx <- which(trimws(gsub("[[:space:]]+", " ", as.character(cached_pair$data_def$Content))) ==
                               trimws(gsub("[[:space:]]+", " ", selected_abundance)))
                unique(as.character(cached_pair$data_def$Transformation[idx]))
              }
            }, error = function(e) character(0)),
            y_axis_transform = "log2"
          ),
          labels = list(
            title = as.character(plot_ui_snapshot$plotTitle_Abundance %||% ui_inputs$plotTitle_Abundance %||% selected_abundance)[1],
            hide_title = isTRUE(plot_ui_snapshot$hideTitle_Abundance %||% ui_inputs$hideTitle_Abundance),
            label_mode = label_mode,
            plot_title_size = plot_ui_snapshot$PlotTitleSize_Abundances %||% ui_inputs$PlotTitleSize_Abundances,
            axis_title_size = plot_ui_snapshot$AxisTitleSize_Abundances %||% ui_inputs$AxisTitleSize_Abundances,
            tick_size = plot_ui_snapshot$tickSize_Abundance %||% ui_inputs$tickSize_Abundance
          ),
          color_theme = list(
            palette = as.character(plot_ui_snapshot$col_abundanceTab %||% ui_inputs$col_abundanceTab %||% "Viridis")[1],
            theme = as.character(plot_ui_snapshot$ThemeSelect_Abundance %||% ui_inputs$ThemeSelect_Abundance %||% "Classic")[1]
          )
        )
        cache_ref <- tryCatch({
          if (.is_plot_cache_pair(cached_pair)) {
            .build_plot_data_cache_id(data_mod = cached_pair$data_mod, data_def = cached_pair$data_def)
          } else {
            NULL
          }
        }, error = function(e) NULL)
        if (isTRUE(had_plot) &&
            (!is.character(cache_ref) || length(cache_ref) != 1L ||
             is.na(cache_ref) || !nzchar(cache_ref))) {
          stop(paste(
            "Abundances cache-integrity failure:",
            "the plot_creation_cache pair has no canonical cache reference"
          ), call. = FALSE)
        }
        s <- list(
          version = "2.0",
          restore_cache_dependency = if (isTRUE(had_plot)) "shared_plot_data_cache_pool" else "none",
          ui_inputs = ui_inputs,
          plot_ui_inputs = plot_ui_cache,
          plot_request = plot_request,
          plot_data_cache_ref = if (isTRUE(had_plot)) cache_ref else NULL,
          # Transient save-time payload: the session orchestrator moves this
          # exact plot-creation pair into plot_data_cache_pool and removes the
          # embedded frames from the saved module snapshot.
          plot_data_cache_payload = if (isTRUE(had_plot)) cached_pair else NULL,
          plot_ui_cache = plot_ui_cache,
          had_plot = had_plot
        )
        # Schema 2.0 saves only the cache reference; embedded data-frame cache
        # bundles are legacy restore-only compatibility and are not written here.
        s$plot_cache_ref_by_title <- if (isTRUE(had_plot)) tryCatch({
          key <- canonical_plot_key(selected_abundance)
          stats::setNames(list(cache_ref), key)
        }, error = function(e) {
          stop(paste(
            "Abundances cache-integrity failure:",
            "the plot_creation_cache canonical reference could not be indexed"
          ), call. = FALSE)
        }) else NULL
        s
      },
      set_session_state = function(s) {
        canonical_plot_key <- function(data_type = NULL) {
          .build_canonical_plot_cache_key(
            module = "abundances",
            logical_plot_id = as.character(data_type %||% "default")[1],
            variant = "main"
          )
        }
        legacy_plot_key <- function(data_type = NULL) {
          as.character(data_type %||% "default")[1]
        }
        if (is.null(s)) return()
        had_plot_on_save <- isTRUE(s$had_plot)
        if (!is.list(s$plot_ui_inputs)) {
          if (is.list(s$plot_request)) {
            s$plot_ui_inputs <- list(
              data_abundanceTab = s$plot_request$abundance_type,
              plotTitle_Abundance = s$plot_request$labels$title,
              ThemeSelect_Abundance = s$plot_request$color_theme$theme,
              hideTitle_Abundance = isTRUE(s$plot_request$labels$hide_title),
              label_abundanceTab = s$plot_request$labels$label_mode,
              col_abundanceTab = s$plot_request$color_theme$palette,
              PlotTitleSize_Abundances = s$plot_request$labels$plot_title_size,
              AxisTitleSize_Abundances = s$plot_request$labels$axis_title_size,
              tickSize_Abundance = s$plot_request$labels$tick_size
            )
          } else if (is.list(s$plot_ui_cache)) {
            s$plot_ui_inputs <- s$plot_ui_cache
          }
          if (is.list(s$plot_ui_cache)) {
            s$plot_ui_inputs <- utils::modifyList(s$plot_ui_cache, s$plot_ui_inputs %||% list())
          }
        }
        cache_key <- as.character(s$plot_data_cache_ref %||% NA_character_)[1]
        cache_intended_restore <- isTRUE(.module_restore_has_cache_intent(s))
        if (!is.list(s$restore_plot_data_cache) && is.list(s$plot_data_cache_payload)) {
          s$restore_plot_data_cache <- s$plot_data_cache_payload
        }
        cache_hit <- is.list(s$restore_plot_data_cache) &&
          inherits(s$restore_plot_data_cache$data_mod, "data.frame") &&
          inherits(s$restore_plot_data_cache$data_def, "data.frame")
        cache_hit_reason <- if (isTRUE(cache_hit)) "cache_restored_module_ref" else "cache_miss_fallback_live"
        if (is.list(s$restore_plot_data_cache_by_title) && is.list(s$plot_ui_inputs)) {
          key <- canonical_plot_key(s$plot_ui_inputs$data_abundanceTab)
          cache_key <- if (is.na(cache_key) || !nzchar(cache_key)) key else cache_key
          cand <- s$restore_plot_data_cache_by_title[[key]]
          if (!is.list(cand)) {
            legacy_key <- legacy_plot_key(s$plot_ui_inputs$data_abundanceTab)
            cand <- s$restore_plot_data_cache_by_title[[legacy_key]]
          }
          if (is.list(cand) &&
              inherits(cand$data_mod, "data.frame") &&
              inherits(cand$data_def, "data.frame")) {
            s$restore_plot_data_cache <- cand; cache_hit <- TRUE; cache_hit_reason <- "cache_restored_by_title"
          }
        }
        degraded_cache_restore <- isTRUE(cache_intended_restore) && !isTRUE(cache_hit) &&
          !isTRUE(.module_restore_live_contract_compatible(s, rv))
        if (isTRUE(degraded_cache_restore)) {
          s$restore_plot_data_cache <- NULL
          s$had_plot <- FALSE
        }
        if (is.list(s$plot_ui_inputs)) {
          if (is.list(s$ui_inputs) && !is.null(s$ui_inputs$hideTitle_Abundance)) {
            s$plot_ui_inputs$hideTitle_Abundance <- isTRUE(s$ui_inputs$hideTitle_Abundance)
          }
          state$plot_ui_cache(s$plot_ui_inputs)
        } else {
          state$plot_ui_cache(NULL)
        }
        restore_mode_cached <- FALSE
        if (is.list(s$restore_plot_data_cache)) {
          state$restore_plot_data_cache(s$restore_plot_data_cache)
          state$restore_mode_cached(TRUE)
          restore_mode_cached <- TRUE
          state$had_plot_on_save(isTRUE(s$had_plot))
          debug_log(paste0("[Abundances] staged restore_plot_data_cache: data_mod=",
                           nrow(s$restore_plot_data_cache$data_mod), "x",
                           ncol(s$restore_plot_data_cache$data_mod), ", data_def=",
                           nrow(s$restore_plot_data_cache$data_def), "x",
                           ncol(s$restore_plot_data_cache$data_def)), 1)
        } else {
          state$restore_plot_data_cache(NULL)
          state$restore_mode_cached(FALSE)
          restore_mode_cached <- FALSE
          debug_log("[Abundances] no restore_plot_data_cache staged", 1)
        }
        if (!isTRUE(restore_mode_cached)) state$had_plot_on_save(isTRUE(s$had_plot) && !isTRUE(degraded_cache_restore))
        state$ggplot_object_abundanceTab(NULL)
        state$plot_object_abundanceTab(NULL)
        # Backward compatibility for older snapshots that stored the ggplot.
        if (!isTRUE(s$had_plot) && !is.null(s$ggplot_object) &&
            inherits(s$ggplot_object, "ggplot")) {
          state$ggplot_object_abundanceTab(s$ggplot_object)
        }
        # Stage UI inputs for the session_restore_trigger observer so they are
        # applied after data has been restored and dynamic choices repopulated.
        if (!is.null(s$ui_inputs) && is.list(s$ui_inputs))
          state$pending_ui_inputs(s$ui_inputs)
        state$restore_rebuild_nonce(as.integer(stats::runif(1, min = 1, max = .Machine$integer.max)))
        report <- list(
          cache_key = cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else "live",
          reason = if (isTRUE(cache_hit)) cache_hit_reason else if (isTRUE(degraded_cache_restore)) "cache_ref_unresolved_live_data_incompatible" else "cache_miss_fallback_live",
          restore_cache_degraded = isTRUE(degraded_cache_restore),
          cached_restore_requested = isTRUE(cache_hit) || isTRUE(s$had_plot),
          cached_ui_snapshot_loaded = is.list(s$plot_ui_inputs) || is.list(s$plot_ui_cache),
          rebuild_from_cached_metadata_ok = NA,
          live_ui_sync_partial = FALSE
        )
        if (isTRUE(had_plot_on_save) && !isTRUE(cache_hit)) {
          showNotification(if (isTRUE(degraded_cache_restore)) .restore_cache_unavailable_dataset_mismatch_warning else "Abundances restored using current dataset (cached plot data unavailable).", type = "warning", duration = 6)
        }
        record_restore_report("Abundances", report)
        debug_log("[Abundances] session state restored via set_session_state", 1)
      }
    )

  })
}
