# ==============================================================================
# dotplot_module.R - Dotplot module orchestration
#
# Purpose: Provides Dotplot UI and server orchestration by initializing shared
# reactive state and delegating observer logic to dedicated server submodules.
#
# Structure:
#   - Module loading: Sources Dotplot UI, utility, and server submodules
#   - UI wrapper: Returns namespaced Dotplot UI and startup JavaScript
#   - Server orchestration: Defines shared state and initializes submodules
#
# Dependencies: shiny, shinyjs, module sub-scripts under modules/dot
# Called by: Parent application server via modDotPlotUI()/modDotPlotServer()
# ==============================================================================

sys.source("modules/dot/dotplot_UI.R", envir = modEnv)
sys.source("modules/dot/dotplot_utils.R", envir = modEnv)
sys.source("modules/dot/dotplot_label_utils.R", envir = modEnv)
sys.source("modules/dot/dotplot_range_utils.R", envir = modEnv)
sys.source("modules/dot/dotplot_interactive_utils.R", envir = modEnv)
sys.source("modules/dot/dotplot_reactive_state.R", envir = modEnv)
sys.source("modules/dot/dotplot_server_config.R", envir = modEnv)
sys.source("modules/dot/dotplot_server_plot.R", envir = modEnv)
sys.source("modules/dot/dotplot_server_interaction.R", envir = modEnv)
sys.source("modules/dot/dotplot_server_labeling.R", envir = modEnv)
sys.source("modules/dot/dotplot_server_regions.R", envir = modEnv)

# ------------------------------------------------------------------------------
# modDotPlotUI
# Purpose: Builds the Dotplot module UI with startup script initialization.
# Structure:
#   - Section 1: Build namespaced Dotplot UI content.
#   - Section 2: Attach shinyjs support and startup readiness signal.
# Parameters:
#   - id: [character] - Shiny module identifier.
# Returns: tagList containing the Dotplot UI.
# ------------------------------------------------------------------------------
modDotPlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    dotplot_UI(ns),
    useShinyjs(),
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('%s', null);
      });
    ", ns("dotplot_ready"))))
  )
}

# ------------------------------------------------------------------------------
# modDotPlotServer
# Purpose: Orchestrates Dotplot state initialization and submodule observer wiring.
# Structure:
#   - Section 1: Define module-scoped debug logger.
#   - Section 2: Create shared reactive state through the central factory.
#   - Section 3: Initialize all Dotplot observer submodules with explicit inputs.
# Parameters:
#   - id: [character] - Shiny module identifier.
#   - rv: [reactivevalues] - Shared application reactive values.
#   - res_GSEA: [any] - Optional GSEA result data for selection import.
#   - res_GO: [any] - Optional GO result data for selection import.
#   - module_outputs: [any] - Reserved compatibility parameter.
#   - debug_level: [numeric] - Dotplot debug verbosity level.
#   - modEnv: [environment] - Environment containing sourced Dotplot scripts.
# Returns: List of reactive accessors and control helpers.
# ------------------------------------------------------------------------------
modDotPlotServer <- function(id, rv, res_GSEA = NULL, res_GO = NULL, module_outputs = NULL, debug_level = 0, modEnv = new.env()) {
  moduleServer(id, function(input, output, session, local = modEnv) {
    ns <- session$ns

    dotplot_debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "DOTPLOT", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ DOTPLOT ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    # Make logger available to sourced dotplot helper functions in modEnv.
    modEnv$dotplot_debug_log <- dotplot_debug_log

    dotplot_debug_log("Dot plot module server starting", 1)

    record_restore_report <- function(module_name, report) {
      if (!is.list(rv$restore_reports)) rv$restore_reports <- list()
      rv$restore_reports[[module_name]] <- report
      dotplot_debug_log(sprintf("[%s] restore report: key=%s hit=%s source=%s reason=%s",
                        module_name,
                        as.character(report$cache_key %||% NA_character_),
                        as.character(report$cache_hit %||% NA),
                        as.character(report$data_source %||% NA_character_),
                        as.character(report$reason %||% NA_character_)), 1)
      if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
        rv$restore_diagnostics <- rv$restore_reports
      }
    }

    shared_state <- dotplot_init_reactive_state(rv = rv, dotplot_debug_log = dotplot_debug_log)

    data_in <- shared_state$data_in
    data_def_in <- shared_state$data_def_in
    plot_update_trigger <- shared_state$plot_update_trigger
    range_render_trigger <- shared_state$range_render_trigger
    dotplot_state <- shared_state$dotplot_state
    region_configs <- shared_state$region_configs
    selected_region <- shared_state$selected_region
    region_structure <- shared_state$region_structure
    selected_data_dot <- shared_state$selected_data_dot
    selected_protein_vector_dot <- shared_state$selected_protein_vector_dot
    selected_points_interactive_dot <- shared_state$selected_points_interactive_dot
    protein_label_settings_dot <- shared_state$protein_label_settings_dot
    dot_protein_labels <- shared_state$dot_protein_labels
    dot_plot_parameters <- shared_state$dot_plot_parameters
    user_range_settings <- shared_state$user_range_settings
    pending_ui_inputs <- shared_state$pending_ui_inputs

    # Names of widgets whose values influence the exported plot (axis ranges,
    # labels, tick / title / theme settings, region styling controls).  These
    # are captured at save time and pushed back to the UI after restore via
    # dotplot_sync_ui_from_state().
    dotplot_ui_input_ids <- c(
      "x_axis_column", "y_axis_column",
      "x_transform",   "y_transform",
      "x_axis_label",  "y_axis_label",
      "plot_title",    "hide_title",
      "theme_select",  "title_size", "axis_title_size", "tick_size",
      "x_axis_range",  "y_axis_range",
      "x_tick_interval", "y_tick_interval",
      "region_point_color", "region_point_size",
      "region_point_alpha", "region_point_shape",
      "masterLabelColor_dot", "masterDotColor_dot",
      "masterCustomDot_dot",
      "maxOverlaps_dot", "labelDistance_dot",
      "lineThickness_dot", "labelSize_dot",
      "dotSizeLabeled_dot",
      "GeneIdentifierColumn_dot"
    )

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
      c("plotly_selected-dotplot_plot", "plotly_click-dotplot_plot")
    ))

    dotplot_init_config_observers(input, output, session, ns, dotplot_debug_log, rv, dotplot_state, data_in, data_def_in,
      dot_plot_parameters, user_range_settings, plot_update_trigger, range_render_trigger, selected_points_interactive_dot, region_configs)
    dotplot_init_plot_observers(input, output, session, ns, dotplot_debug_log, rv, dotplot_state, data_in, data_def_in,
      region_configs, region_structure, plot_update_trigger, range_render_trigger, modEnv,
      selected_region = selected_region,
      pending_ui_inputs = pending_ui_inputs,
      dotplot_ui_input_ids = dotplot_ui_input_ids,
      dot_protein_labels = dot_protein_labels)
    dotplot_init_interaction_observers(input, output, session, ns, dotplot_debug_log, rv,
      selected_data_dot, selected_protein_vector_dot, selected_points_interactive_dot, res_GSEA, res_GO)
    dotplot_init_labeling_observers(input, output, session, ns, dotplot_debug_log, rv, dotplot_state,
      selected_protein_vector_dot, selected_data_dot, protein_label_settings_dot, dot_protein_labels)
    dotplot_init_region_observers(input, output, session, ns, dotplot_debug_log, dotplot_state, data_in, data_def_in,
      region_configs, selected_region, region_structure)

    return(list(
      get_plot = reactive(dotplot_state$current_plot),
      get_config = reactive(dotplot_state$axis_config),
      get_thresholds = reactive(dotplot_state$thresholds),
      has_plot = reactive(dotplot_state$plot_ready),
      clear_plot = function() {
        dotplot_state$current_plot <- NULL
        dotplot_state$plot_ready <- FALSE
        dotplot_debug_log("Plot cleared", 1)
      },


      .restore_plot_snapshot_compat <- function(plot_snapshot) {
        # Backward compatible: accepts legacy miraprot_plot_bundle payloads and
        # plain ggplot objects from old session files.  New saves never call the
        # bundle writer and never persist ggplot objects.
        .restore_plot_from_snapshot(plot_snapshot)
      },

      # Session save/restore interface
      get_session_state = function() {
        current_inputs <- tryCatch(dotplot_capture_ui_inputs(input, dotplot_ui_input_ids), error = function(e) list())
        plot_request <- tryCatch(dotplot_extract_plot_parameters(input), error = function(e) list())
        plot_title <- current_inputs$plot_title %||% plot_request$plot_title
        canonical_key <- dotplot_build_cache_key(plot_title)
        cached_pair <- tryCatch({
          if (is.list(dotplot_state$plot_creation_cache) &&
              inherits(dotplot_state$plot_creation_cache$data_mod, "data.frame") &&
              inherits(dotplot_state$plot_creation_cache$data_def, "data.frame")) {
            dotplot_state$plot_creation_cache
          } else NULL
        }, error = function(e) NULL)
        cache_key <- tryCatch({
          if (is.list(cached_pair)) {
            .build_plot_data_cache_id(data_mod = cached_pair$data_mod, data_def = cached_pair$data_def)
          } else {
            NA_character_
          }
        }, error = function(e) NA_character_)
        captured_ui_cache <- tryCatch(
          dotplot_capture_plot_ui_cache(input, dotplot_state, region_configs, region_structure),
          error = function(e) list()
        )
        stored_ui_cache <- tryCatch(isolate(dotplot_state$plot_ui_cache), error = function(e) NULL)
        ui_cache <- if (is.list(stored_ui_cache) && length(stored_ui_cache) > 0L) stored_ui_cache else captured_ui_cache
        if (is.list(ui_cache) && is.list(captured_ui_cache)) {
          # The stored plot UI cache is captured when the ggplot is built, but
          # Dotplot immediately updates axis sliders afterwards from the final
          # plot ranges. Refresh only the range/tick fields from the live UI at
          # save time so restored sessions use the exact visible slider values
          # while preserving cached axes/labels/regions from plot creation.
          captured_flat <- dotplot_flatten_plot_ui_cache_for_restore(captured_ui_cache) %||% list()
          for (range_id in c(
            "x_axis_range", "y_axis_range", "natural_x_range", "natural_y_range",
            "x_tick_interval", "y_tick_interval", "x_axis_slider_state", "y_axis_slider_state"
          )) {
            val <- captured_flat[[range_id]]
            if (!is.null(val)) ui_cache[[range_id]] <- val
          }
          if (is.list(captured_ui_cache$axis_ranges)) ui_cache$axis_ranges <- captured_ui_cache$axis_ranges
          if (is.list(captured_ui_cache$axis_ticks)) ui_cache$axis_ticks <- captured_ui_cache$axis_ticks
        }
        if (is.list(ui_cache) && is.list(ui_cache$region_styling)) {
          ui_cache$region_styling$selected_region <- tryCatch(isolate(selected_region()), error = function(e) NULL)
        }

        state <- list(
          version = DOTPLOT_SESSION_SCHEMA_VERSION,
          restore_cache_dependency = "shared_plot_data_cache_pool"
        )
        state$ui_inputs <- list(
          axes = current_inputs[intersect(names(current_inputs), c(
            "x_axis_column", "y_axis_column", "x_transform", "y_transform",
            "x_axis_label", "y_axis_label"
          ))],
          coloring = list(
            color_rules = isolate(dotplot_state$color_rules),
            region_point_color = current_inputs$region_point_color,
            masterLabelColor_dot = current_inputs$masterLabelColor_dot,
            masterDotColor_dot = current_inputs$masterDotColor_dot,
            masterCustomDot_dot = current_inputs$masterCustomDot_dot
          ),
          sizing = current_inputs[intersect(names(current_inputs), c(
            "region_point_size", "region_point_alpha", "region_point_shape",
            "title_size", "axis_title_size", "tick_size",
            "dotSizeLabeled_dot", "lineThickness_dot", "labelSize_dot"
          ))],
          filters = current_inputs[intersect(names(current_inputs), c("GeneIdentifierColumn_dot"))],
          theme = current_inputs[intersect(names(current_inputs), c("plot_title", "hide_title", "theme_select"))],
          label_options = current_inputs[intersect(names(current_inputs), c(
            "maxOverlaps_dot", "labelDistance_dot", "lineThickness_dot",
            "labelSize_dot", "dotSizeLabeled_dot", "masterLabelColor_dot",
            "masterDotColor_dot", "masterCustomDot_dot"
          ))],
          values = current_inputs
        )
        state$plot_request <- plot_request
        state$plot_request$axis_config <- isolate(dotplot_state$axis_config)
        state$plot_request$thresholds <- isolate(dotplot_state$thresholds)
        state$plot_request$color_rules <- isolate(dotplot_state$color_rules)
        state$plot_request$region_configs <- tryCatch(isolate(region_configs()), error = function(e) list())
        state$plot_request$selected_region <- tryCatch(isolate(selected_region()), error = function(e) NULL)
        state$plot_data_cache_ref <- cache_key
        cache_contract <- .plot_data_cache_ref_contract(
          data_mod_revision_id = rv$data_mod_revision_id,
          data_def_revision_id = rv$data_def_revision_id,
          data_mod = cached_pair$data_mod %||% NULL,
          data_def = cached_pair$data_def %||% NULL,
          plot_data_cache_ref = cache_key
        )
        state$data_mod_revision_id <- cache_contract$data_mod_revision_id
        state$data_def_revision_id <- cache_contract$data_def_revision_id
        state$plot_data_cache_fingerprint <- cache_contract$plot_data_cache_fingerprint
        state$plot_data_cache_payload <- cached_pair
        state$plot_ui_cache <- ui_cache
        state$labels <- list(
          manual_labels = tryCatch(isolate(dot_protein_labels()), error = function(e) list()),
          region_labels = tryCatch(isolate(region_configs()), error = function(e) list()),
          protein_label_settings = tryCatch(isolate(protein_label_settings_dot()), error = function(e) NULL),
          selected_data = tryCatch(isolate(selected_data_dot()), error = function(e) NULL),
          selected_proteins = tryCatch(isolate(selected_protein_vector_dot()), error = function(e) NULL)
        )
        state$selected_points <- isolate(dotplot_state$selected_points)
        state$highlighted_points <- tryCatch(isolate(selected_points_interactive_dot()), error = function(e) NULL)
        state$plot_ready <- isolate(dotplot_state$plot_ready)

        # UI state alone is not plot restore intent.  Do not publish dangling
        # cache references for a Dotplot that has never been rendered.
        if (!isTRUE(state$plot_ready)) {
          state$restore_cache_dependency <- "none"
          state$plot_data_cache_ref <- NULL
          state$plot_data_cache_payload <- NULL
        }

        # Compatibility fields used by the existing restore orchestration and
        # cache hydration. These are scalar/list metadata only: do not persist
        # current_plot, base_plot_without_labels, plotly objects, or ggplot objects.
        state$axis_config <- state$plot_request$axis_config
        state$thresholds <- state$plot_request$thresholds
        state$color_rules <- state$plot_request$color_rules
        state$region_configs <- state$plot_request$region_configs
        state$selected_region <- state$plot_request$selected_region
        state$plot_ui_inputs <- utils::modifyList(current_inputs, dotplot_flatten_plot_ui_cache_for_restore(ui_cache) %||% list(), keep.null = TRUE)
        state$ui_inputs_legacy <- state$plot_ui_inputs
        state$plot_cache_ref_by_title <- if (isTRUE(state$plot_ready) &&
            is.character(cache_key) && length(cache_key) == 1L &&
            !is.na(cache_key) && nzchar(cache_key)) {
          stats::setNames(list(cache_key), canonical_key)
        } else NULL
        # Schema 2.0 writes only plot_data_cache_ref/plot_cache_ref_by_title.
        # Legacy restore_plot_data_cache bundles with embedded data frames are
        # accepted by set_session_state(), but are not persisted by new saves.
        state$dot_protein_labels <- state$labels$manual_labels
        state$protein_label_settings_dot <- state$labels$protein_label_settings
        state$selected_data_dot <- state$labels$selected_data
        state$selected_protein_vector_dot <- state$labels$selected_proteins
        state$selected_points_interactive_dot <- state$highlighted_points
        state
      },
      set_session_state = function(state, phase = NULL) {
        legacy_plot_key <- function(plot_title = NULL) as.character(plot_title %||% "default")[1]
        if (is.null(state)) return()

        # Apply the snapshot only during the state phase. The plots phase merely
        # records whether the final session_restore_trigger reconstruction is
        # expected; replaying the setter here would duplicate state writes and
        # restore notifications.
        if (identical(phase, "full_module_plots")) {
          dotplot_state$restore_rebuild_requested <- isTRUE(state$plot_ready)
          dotplot_debug_log(sprintf(
            "[Dotplot] reconstruction recorded for session restore trigger (requested=%s)",
            as.character(isTRUE(dotplot_state$restore_rebuild_requested))
          ), 1)
          return()
        }
        if (!is.null(phase) && !identical(phase, "full_module_state")) return()

        # Legacy snapshots may contain serialized ggplots. Accept them only as
        # input for this restore and immediately migrate in memory to schema 2.0
        # by rebuilding from request/cache metadata below.
        legacy_current_plot <- NULL
        legacy_base_plot <- NULL
        if (!identical(as.character(state$version %||% "1.0"), DOTPLOT_SESSION_SCHEMA_VERSION)) {
          legacy_current_plot <- tryCatch(.restore_plot_snapshot_compat(state$current_plot), error = function(e) NULL)
          legacy_base_plot <- tryCatch(.restore_plot_snapshot_compat(state$base_plot_without_labels), error = function(e) NULL)
        }

        if (is.null(state$plot_ui_inputs)) {
          state$plot_ui_inputs <- state$ui_inputs$values %||% state$ui_inputs_legacy %||% state$ui_inputs
        }
        if (is.null(state$axis_config) && is.list(state$plot_request)) state$axis_config <- state$plot_request$axis_config
        if (is.null(state$thresholds) && is.list(state$plot_request)) state$thresholds <- state$plot_request$thresholds
        if (is.null(state$color_rules) && is.list(state$plot_request)) state$color_rules <- state$plot_request$color_rules
        if (is.null(state$region_configs) && is.list(state$plot_request)) state$region_configs <- state$plot_request$region_configs
        if (is.null(state$selected_region) && is.list(state$plot_request)) state$selected_region <- state$plot_request$selected_region
        if (!is.null(state$plot_ui_cache)) {
          state$plot_ui_inputs <- utils::modifyList(
            state$plot_ui_inputs %||% list(),
            dotplot_flatten_plot_ui_cache_for_restore(state$plot_ui_cache) %||% list(),
            keep.null = TRUE
          )
        }
        if (is.list(state$labels)) {
          state$dot_protein_labels <- state$labels$manual_labels %||% state$dot_protein_labels
          state$protein_label_settings_dot <- state$labels$protein_label_settings %||% state$protein_label_settings_dot
          state$selected_data_dot <- state$labels$selected_data %||% state$selected_data_dot
          state$selected_protein_vector_dot <- state$labels$selected_proteins %||% state$selected_protein_vector_dot
        }
        if (is.null(state$selected_points_interactive_dot)) state$selected_points_interactive_dot <- state$highlighted_points

        had_plot_on_save <- isTRUE(state$plot_ready)
        logical_plot_id <- state$plot_ui_inputs$plot_title
        canonical_key <- NULL
        cache_key <- NA_character_
        malformed_cache_key <- FALSE
        if (isTRUE(had_plot_on_save)) {
          canonical_key <- tryCatch(
            dotplot_build_cache_key(logical_plot_id),
            error = function(e) {
              dotplot_debug_log(paste("[Dotplot] malformed-cache-key:", e$message), 1)
              NULL
            }
          )
          cache_key <- as.character(state$plot_data_cache_ref %||% NA_character_)[1]
          malformed_cache_key <- is.null(canonical_key) || is.na(cache_key) ||
            !nzchar(trimws(cache_key))
        }
        cache_hit <- FALSE
        cache_source <- NULL
        cache_intended_restore <- isTRUE(had_plot_on_save) &&
          !isTRUE(malformed_cache_key) && isTRUE(.module_restore_has_cache_intent(state))
        restore_cache_resolved <- isTRUE(state$restore_cache_resolved)
        restore_cache_mode <- as.character(state$restore_cache_resolution_mode %||% "none")[1]

        dotplot_state$restore_in_progress <- TRUE
        dotplot_state$plot_from_restore_cache <- FALSE
        if (!is.list(state$restore_plot_data_cache) && is.list(state$plot_data_cache_payload)) {
          state$restore_plot_data_cache <- state$plot_data_cache_payload
        }
        module_cache_valid <- isTRUE(had_plot_on_save) && !isTRUE(malformed_cache_key) && is.list(state$restore_plot_data_cache) &&
          is.data.frame(state$restore_plot_data_cache$data_mod) &&
          is.data.frame(state$restore_plot_data_cache$data_def)
        dotplot_state$restore_plot_data_cache <- if (isTRUE(module_cache_valid)) state$restore_plot_data_cache else NULL
        if (is.list(dotplot_state$restore_plot_data_cache)) {
          cache_hit <- TRUE; cache_source <- "module_ref"
        } else if (isTRUE(had_plot_on_save) && !isTRUE(malformed_cache_key) && is.list(state$restore_plot_data_cache_by_title)) {
          cand <- state$restore_plot_data_cache_by_title[[canonical_key]]
          if (!is.list(cand)) cand <- state$restore_plot_data_cache_by_title[[legacy_plot_key(state$plot_ui_inputs$plot_title)]]
          if (is.list(cand) &&
              is.data.frame(cand$data_mod) &&
              is.data.frame(cand$data_def)) {
            dotplot_state$restore_plot_data_cache <- cand
            cache_hit <- TRUE; cache_source <- "by_title"
          }
        }
        degraded_cache_restore <- isTRUE(had_plot_on_save) && (isTRUE(malformed_cache_key) ||
          (isTRUE(cache_intended_restore) && !isTRUE(cache_hit) &&
             !isTRUE(.module_restore_live_contract_compatible(state, rv))))
        # Live data is never a generic cache-miss fallback. It is eligible only
        # when the saved identity/revisions/fingerprint match the canonical pair.
        live_fallback_available <- isTRUE(had_plot_on_save) && !isTRUE(malformed_cache_key) && !isTRUE(cache_hit) &&
          isTRUE(.module_restore_live_contract_compatible(state, rv))
        dotplot_state$restore_cache_resolved <- isTRUE(cache_hit)
        dotplot_state$restore_live_fallback_available <- isTRUE(live_fallback_available)
        dotplot_state$restore_rebuild_requested <- is.null(phase) && isTRUE(had_plot_on_save)
        dotplot_state$restore_notification_emitted <- FALSE
        if (isTRUE(degraded_cache_restore)) {
          state$plot_ready <- FALSE
          dotplot_state$restore_plot_data_cache <- NULL
        }
        if (is.list(dotplot_state$restore_plot_data_cache)) {
          dotplot_state$plot_from_restore_cache <- TRUE
          dotplot_state$plot_creation_cache <- dotplot_state$restore_plot_data_cache
          dm <- dotplot_state$restore_plot_data_cache$data_mod
          dd <- dotplot_state$restore_plot_data_cache$data_def
          dotplot_state$source_data_signature <- if (is.data.frame(dm) && is.data.frame(dd)) {
            paste0(nrow(dm), "x", ncol(dm), "::", nrow(dd), "x", ncol(dd), "::",
                   paste(colnames(dm), collapse = "|"), "::", paste(colnames(dd), collapse = "|"))
          } else NULL
        }
        dotplot_state$plot_ui_cache <- state$plot_ui_inputs
        if (is.list(state$plot_ui_inputs) &&
            is.numeric(state$plot_ui_inputs$natural_x_range) && length(state$plot_ui_inputs$natural_x_range) == 2L &&
            all(is.finite(state$plot_ui_inputs$natural_x_range)) &&
            is.numeric(state$plot_ui_inputs$natural_y_range) && length(state$plot_ui_inputs$natural_y_range) == 2L &&
            all(is.finite(state$plot_ui_inputs$natural_y_range))) {
          dotplot_state$natural_axis_ranges <- list(
            x_range = state$plot_ui_inputs$natural_x_range,
            y_range = state$plot_ui_inputs$natural_y_range
          )
        }
        if (is.list(state$plot_request)) dotplot_state$plot_request <- state$plot_request
        dotplot_state$plot_cache_ref_by_title <- if (!isTRUE(had_plot_on_save)) NULL else if (is.list(state$plot_cache_ref_by_title)) state$plot_cache_ref_by_title else if (!isTRUE(malformed_cache_key)) stats::setNames(list(""), canonical_key) else NULL
        if (!is.null(state$axis_config))    dotplot_state$axis_config <- state$axis_config
        if (is.list(dotplot_state$axis_config) && is.list(state$plot_ui_inputs)) {
          dotplot_state$axis_config$x_col <- dotplot_state$axis_config$x_col %||% state$plot_ui_inputs$x_axis_column
          dotplot_state$axis_config$y_col <- dotplot_state$axis_config$y_col %||% state$plot_ui_inputs$y_axis_column
          dotplot_state$axis_config$x_transform <- dotplot_state$axis_config$x_transform %||% state$plot_ui_inputs$x_transform %||% "raw"
          dotplot_state$axis_config$y_transform <- dotplot_state$axis_config$y_transform %||% state$plot_ui_inputs$y_transform %||% "raw"
          dotplot_state$axis_config$x_label <- dotplot_state$axis_config$x_label %||% state$plot_ui_inputs$x_axis_label
          dotplot_state$axis_config$y_label <- dotplot_state$axis_config$y_label %||% state$plot_ui_inputs$y_axis_label
        }
        if (!is.null(state$thresholds))     dotplot_state$thresholds  <- state$thresholds
        if (!is.null(state$color_rules))    dotplot_state$color_rules <- state$color_rules
        if (!is.null(state$selected_points)) dotplot_state$selected_points <- state$selected_points
        if (!is.null(state$region_configs)) region_configs(state$region_configs)
        if (!is.null(state$selected_region)) selected_region(state$selected_region)
        if (!is.null(state$selected_data_dot)) selected_data_dot(state$selected_data_dot)
        if (!is.null(state$selected_protein_vector_dot)) selected_protein_vector_dot(state$selected_protein_vector_dot)
        if (!is.null(state$protein_label_settings_dot)) protein_label_settings_dot(state$protein_label_settings_dot)
        if (!is.null(state$dot_protein_labels)) dot_protein_labels(state$dot_protein_labels)
        if (!is.null(state$dot_plot_parameters)) dot_plot_parameters(state$dot_plot_parameters)
        if (!is.null(state$user_range_settings)) user_range_settings(state$user_range_settings)
        if (!is.null(state$selected_points_interactive_dot)) selected_points_interactive_dot(state$selected_points_interactive_dot)
        if (is.list(state$plot_ui_inputs)) pending_ui_inputs(state$plot_ui_inputs)

        # Restore never trusts persisted plot objects for schema 2.0; legacy
        # ggplots are transient fallback input only until the post-flush rebuild
        # replaces them from the saved request and cache/live data.
        dotplot_state$current_plot <- legacy_current_plot
        dotplot_state$base_plot_without_labels <- legacy_base_plot
        dotplot_state$plot_ready <- isTRUE(state$plot_ready) && !isTRUE(degraded_cache_restore) &&
          (!is.null(legacy_current_plot) || isTRUE(cache_hit) || isTRUE(live_fallback_available))

        report <- list(
          cache_key = cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else if (isTRUE(live_fallback_available)) "live" else "none",
          reason = if (!isTRUE(had_plot_on_save)) {
            "none"
          } else if (isTRUE(malformed_cache_key)) {
            "malformed-cache-key"
          } else if (isTRUE(cache_hit)) {
            if (identical(cache_source, "module_ref")) "cache_restored_module_ref" else "cache_restored_by_title"
          } else if (isTRUE(degraded_cache_restore)) "cache_ref_unresolved_live_data_incompatible" else "cache_miss_fallback_live",
          restore_cache_degraded = isTRUE(degraded_cache_restore),
          cache_resolved = isTRUE(cache_hit),
          live_fallback_available = isTRUE(live_fallback_available),
          rebuild_requested = isTRUE(dotplot_state$restore_rebuild_requested)
        )
        record_restore_report("Dotplot", report)
        dotplot_debug_log("[Dotplot] session state restored via set_session_state (schema 2.0)", 1)
      }
    ))
  })
}
