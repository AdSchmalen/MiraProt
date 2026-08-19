# =============================================================================
# modules/Venn_module.R
#
# Purpose:
#   Pure orchestrator for the Venn module. Sources all sub-files, defines the
#   UI wrapper (modVennUI), and wires the server function (modVennServer).
#   This is the only file in the module that contains a server function.
#
# Architectural role:
#   This file delegates concerns to four sub-files inside modules/venn/:
#     - venn_logic.R     Pure functions and helpers (no Shiny dependency)
#     - venn_state.R     Reactive state factory (create_venn_state)
#     - venn_observers_*.R  Grouped observe/observeEvent blocks and outputs
#     - venn_observer.R    Ordered observer coordinator
#     - venn_ui.R        Static UI layout and input widgets
#
# File structure:
#   1. Source declarations
#   2. modVennUI()       -- UI wrapper calling create_venn_ui()
#   3. modVennServer()   -- Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via create_venn_state()
#      c. Observer registration via register_venn_observers()
#      d. Return interface
#
# Return interface (public API):
#   get_intersection_list  reactive returning current intersection data
#   get_list_count         reactive returning number of active lists
#   has_data               reactive returning TRUE when data is loaded
#   module_ready           reactive returning TRUE when module is ready
#   module_health_check    reactive returning a named status list
#
# Notes for future developers:
#   - No server logic lives in any sub-file. Sub-files only define functions
#     that are called from this orchestrator or from register_venn_observers().
#   - debug_log is defined here and passed explicitly to any function that logs.
#   - Cleanup uses cleanup_manager$register_module(); do NOT add
#     session$onSessionEnded() calls.
# =============================================================================

source("modules/venn/venn_logic.R",    local = modEnv)
source("modules/venn/venn_state.R",    local = modEnv)
source("modules/venn/venn_observers_data_lists.R",         local = modEnv)
source("modules/venn/venn_observers_plot_interaction.R",   local = modEnv)
source("modules/venn/venn_observers_export_restore.R",     local = modEnv)
source("modules/venn/venn_observer.R",                     local = modEnv)
source("modules/venn/venn_ui.R",       local = modEnv)


# =============================================================================
# UI
# =============================================================================

#' Venn Module UI
#' @param id Module namespace ID
modVennUI <- function(id) {
  ns <- NS(id)
  create_venn_ui(ns)
}


# =============================================================================
# SERVER
# =============================================================================

#' Venn Module Server
#'
#' @param id Module namespace ID
#' @param rv Reactive values containing data and metadata
#' @param res_GSEA Reactive containing GSEA results (optional, legacy)
#' @param GO_res Reactive containing GO results (optional, legacy)
#' @param module_outputs Named list of outputs from sibling modules
#' @param debug_level Debug verbosity level passed from the parent (1 or 2)
modVennServer <- function(id, rv, res_GSEA = NULL, GO_res = NULL,
                          module_outputs = NULL, debug_level = 0) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # -------------------------------------------------------------------------
    # Debug setup
    # -------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "VENN MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ VENN MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Venn module server starting", 1)

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

    # -------------------------------------------------------------------------
    # State initialization
    # -------------------------------------------------------------------------

    state <- create_venn_state()

    # Cache availability is only an input to restore, not its outcome.  Keep a
    # generation-scoped record until the phased restore observer has finished
    # replaying the lists/UI and has actually attempted plot reconstruction.
    restore_notice_context <- reactiveVal(NULL)
    restore_notice_sequence <- reactiveVal(0L)
    restore_notice_generations <- reactiveVal(character())

    # -------------------------------------------------------------------------
    # Observer registration
    # -------------------------------------------------------------------------

    register_venn_observers(
      input          = input,
      output         = output,
      session        = session,
      ns             = ns,
      state          = state,
      rv             = rv,
      module_outputs = module_outputs,
      debug_log      = debug_log
    )

    observeEvent(state$last_restore_report(), {
      report <- state$last_restore_report()
      context <- isolate(restore_notice_context())
      if (!is.list(report) || !is.list(context)) return()

      successful_reconstruction <- startsWith(as.character(report$status %||% ""), "restored_")
      terminal_failure <- as.character(report$status %||% "") %in% c(
        "restore_timeout", "restore_validation_failed", "restore_reconstruction_failed"
      )
      generation_key <- as.character(context$generation_key %||% "")[1]
      already_notified <- generation_key %in% isolate(restore_notice_generations())

      if (isTRUE(context$plot_requested) && isTRUE(context$cache_missing) &&
          isTRUE(terminal_failure) && !isTRUE(successful_reconstruction) &&
          !isTRUE(already_notified)) {
        showNotification(
          "The saved Venn plot could not be reconstructed because its shared cached data is unavailable.",
          type = "warning",
          duration = 6
        )
        restore_notice_generations(unique(c(
          isolate(restore_notice_generations()), generation_key
        )))
      }

      if (isTRUE(successful_reconstruction) || isTRUE(terminal_failure)) {
        restore_notice_context(NULL)
      }
    }, ignoreInit = TRUE)

    debug_log("Venn module server initialization completed", 1)

    # -------------------------------------------------------------------------
    # Return interface
    # -------------------------------------------------------------------------

    list(
      get_intersection_list = reactive({ state$intersection_list() }),
      get_list_count        = reactive({ state$list_count_Venn() }),
      has_data              = reactive({
        !is.null(rv$data_mod) && !is.null(rv$data_def)
      }),
      module_ready          = reactive({
        !is.null(rv$data_mod) && !is.null(rv$data_def) && state$list_count_Venn() > 0
      }),
      module_health_check   = reactive({
        list(
          status            = if (!is.null(rv$data_mod) && !is.null(rv$data_def))
                                "ready" else "waiting_for_data",
          timestamp         = Sys.time(),
          lists_count       = state$list_count_Venn(),
          has_intersections = !is.null(state$intersection_list()) &&
                              length(state$intersection_list()) > 0
        )
      }),


      get_venn_export_workbook = function() {
        export_payload <- tryCatch(isolate(state$go_results_for_extraction()), error = function(e) NULL)
        if (!is.list(export_payload) || is.null(export_payload$input_lists) || length(export_payload$input_lists) == 0) {
          return(NULL)
        }
        input_lists <- export_payload$input_lists
        list_names <- names(input_lists)
        if (is.null(list_names) || any(!nzchar(list_names))) {
          list_names <- paste0("List", seq_along(input_lists))
        }

        value_column_name <- NULL
        if (identical(export_payload$type, "UpSet with Abundances")) value_column_name <- "Abundance"
        if (identical(export_payload$type, "UpSet with Abundance Ratios")) value_column_name <- "Abundance Ratio"

        tryCatch({
          build_venn_intersection_workbook(
            list_names = list_names,
            input_lists = input_lists,
            value_data = export_payload$prepared_data,
            value_column_name = value_column_name
          )
        }, error = function(e) {
          debug_log(paste("[Venn] export workbook generation failed:", e$message), 1)
          NULL
        })
      },
      # Session save/restore interface (schema 2.0 — data-driven rebuild)
      get_session_state = function() {
        canonical_plot_key <- function(ui_inputs = list()) {
          logical_plot_id <- as.character(ui_inputs$diagramType_Venn %||% "Venn")[1]
          variant_fields <- list(
            reference_mode = as.character(ui_inputs$ReferenceValues_Venn %||% "none")[1],
            identifier_column = as.character(ui_inputs$GeneIdentifierColumn_Venn %||% "none")[1]
          )
          variant <- .serialize_plot_variant_spec(variant_fields)
          .build_canonical_plot_cache_key(
            module = "venn",
            logical_plot_id = logical_plot_id,
            variant = variant
          )
        }
        legacy_plot_key <- function(ui_inputs = list()) {
          paste(
            ui_inputs$diagramType_Venn %||% "Venn",
            ui_inputs$ReferenceValues_Venn %||% "none",
            ui_inputs$GeneIdentifierColumn_Venn %||% "none",
            sep = "::"
          )
        }
        s <- list(version = VENN_SESSION_SCHEMA_VERSION)
        s$list_count_Venn    <- tryCatch(isolate(state$list_count_Venn()), error = function(e) 3)
        s$intersection_list  <- tryCatch(isolate(state$intersection_list()), error = function(e) NULL)
        s$plot_active        <- tryCatch(isolate(state$plot_active()), error = function(e) FALSE)
        s$current_plot_type  <- tryCatch(isolate(state$current_plot_type()), error = function(e) NULL)
        s$had_plot           <- tryCatch({
          p <- isolate(state$current_venn_plot())
          isTRUE(isolate(state$plot_active())) ||
            !is.null(p) ||
            !is.null(isolate(state$current_plot_type()))
        }, error = function(e) FALSE)
        s$restore_cache_dependency <- if (isTRUE(s$had_plot)) {
          "shared_plot_data_cache_pool"
        } else "none"
        s$num_intersections_export <- tryCatch(isolate(state$num_intersections_export()), error = function(e) NULL)
        # New schema 2.0 saves resolve data through plot_data_cache_ref only.
        # Embedded restore_plot_data_cache data-frame bundles remain accepted by
        # set_session_state() for legacy files, but are no longer written here.
        # Persist the *live UI* list definitions (fallback to state cache when
        # an input is temporarily unavailable).
        s$list_data_names <- tryCatch({
          lapply(seq_len(s$list_count_Venn), function(i) {
            isolate(input[[paste0("name", i)]]) %||% state$list_data_Venn$names[[i]]
          })
        }, error = function(e) isolate(state$list_data_Venn$names))
        s$list_data_lists <- tryCatch({
          lapply(seq_len(s$list_count_Venn), function(i) {
            isolate(input[[paste0("list", i)]]) %||% state$list_data_Venn$lists[[i]]
          })
        }, error = function(e) isolate(state$list_data_Venn$lists))
        s$list_data_colors <- tryCatch({
          lapply(seq_len(s$list_count_Venn), function(i) {
            isolate(input[[paste0("color", i)]]) %||% state$list_data_Venn$colors[[i]]
          })
        }, error = function(e) isolate(state$list_data_Venn$colors))
        s$set_definitions <- list(
          count = s$list_count_Venn,
          names = s$list_data_names,
          lists = s$list_data_lists,
          colors = s$list_data_colors,
          gsea = tryCatch(isolate(state$list_data_Venn$gsea), error = function(e) NULL),
          go = tryCatch(isolate(state$list_data_Venn$go), error = function(e) NULL),
          sample = tryCatch(isolate(state$list_data_Venn$sample), error = function(e) NULL),
          core_enriched = tryCatch(isolate(state$list_data_Venn$core_enriched), error = function(e) NULL)
        )
        # Capture UI inputs that affect plot customization
        s$ui_inputs <- tryCatch({
          saved_ui <- isolate(state$last_plot_ui_inputs())
          list(
            diagramType_Venn        = saved_ui$diagramType_Venn %||% isolate(input$diagramType_Venn),
            ReferenceValues_Venn    = saved_ui$ReferenceValues_Venn %||% isolate(input$ReferenceValues_Venn),
            GeneIdentifierColumn_Venn = saved_ui$GeneIdentifierColumn_Venn %||% isolate(input$GeneIdentifierColumn_Venn),
            showPercentages_Venn    = isolate(input$showPercentages_Venn) %||% saved_ui$showPercentages_Venn,
            showListTitles_Venn     = isolate(input$showListTitles_Venn) %||% saved_ui$showListTitles_Venn,
            overlapNumberSize_Venn  = isolate(input$overlapNumberSize_Venn) %||% saved_ui$overlapNumberSize_Venn,
            listTitleSize_Venn      = isolate(input$listTitleSize_Venn) %||% saved_ui$listTitleSize_Venn,
            listTitleDistance_Venn  = isolate(input$listTitleDistance_Venn) %||% saved_ui$listTitleDistance_Venn,
            catFont_Venn           = isolate(input$catFont_Venn) %||% saved_ui$catFont_Venn,
            cat_FontStyle_Venn     = isolate(input$cat_FontStyle_Venn) %||% saved_ui$cat_FontStyle_Venn,
            font_family_Venn       = isolate(input$font_family_Venn) %||% saved_ui$font_family_Venn,
            fontStyle_Venn         = isolate(input$fontStyle_Venn) %||% saved_ui$fontStyle_Venn,
            ThemeSelect_Upset      = isolate(input$ThemeSelect_Upset) %||% saved_ui$ThemeSelect_Upset,
            axis_title_size_Venn   = isolate(input$axis_title_size_Venn) %||% saved_ui$axis_title_size_Venn,
            axis_text_size_Venn    = isolate(input$axis_text_size_Venn) %||% saved_ui$axis_text_size_Venn,
            label_text_size_Venn   = isolate(input$label_text_size_Venn) %||% saved_ui$label_text_size_Venn,
            showDotsInBoxplot_Venn = isolate(input$showDotsInBoxplot_Venn) %||% saved_ui$showDotsInBoxplot_Venn,
            data_abundance_Mean_Venn = saved_ui$data_abundance_Mean_Venn %||% isolate(input$data_abundance_Mean_Venn),
            data_abundance_ratio_num_Venn   = saved_ui$data_abundance_ratio_num_Venn %||% isolate(input$data_abundance_ratio_num_Venn),
            data_abundance_ratio_denom_Venn = saved_ui$data_abundance_ratio_denom_Venn %||% isolate(input$data_abundance_ratio_denom_Venn),
            width_plot_Venn        = isolate(input$width_plot_Venn) %||% saved_ui$width_plot_Venn,
            height_plot_Venn       = isolate(input$height_plot_Venn) %||% saved_ui$height_plot_Venn,
            ppi_plot_Venn          = isolate(input$ppi_plot_Venn) %||% saved_ui$ppi_plot_Venn,
            format_file_Venn       = isolate(input$format_file_Venn) %||% saved_ui$format_file_Venn
          )
        }, error = function(e) isolate(state$last_plot_ui_inputs()))
        s$plot_ui_inputs <- s$ui_inputs
        canonical_key <- tryCatch({
          cache_ui <- s$ui_inputs %||% isolate(state$last_plot_ui_inputs())
          canonical_plot_key(cache_ui)
        }, error = function(e) NULL)
        cached_pair <- tryCatch({
          pair <- isolate(state$plot_creation_cache())
          if (is.list(pair) &&
              inherits(pair$data_mod, "data.frame") &&
              inherits(pair$data_def, "data.frame")) {
            pair
          } else if (is.list(isolate(state$restore_plot_data_cache())) &&
                     inherits(isolate(state$restore_plot_data_cache())$data_mod, "data.frame") &&
                     inherits(isolate(state$restore_plot_data_cache())$data_def, "data.frame")) {
            # Legacy/in-flight restore only: expose it as a transient cache
            # candidate until reconstruction promotes it to the canonical pair.
            isolate(state$restore_plot_data_cache())
          } else if (inherits(rv$data_mod, "data.frame") && inherits(rv$data_def, "data.frame")) {
            list(data_mod = rv$data_mod, data_def = rv$data_def)
          } else {
            NULL
          }
        }, error = function(e) NULL)
        s$plot_data_cache_ref <- if (isTRUE(s$had_plot)) tryCatch({
          if (is.list(cached_pair)) {
            .build_plot_data_cache_id(data_mod = cached_pair$data_mod, data_def = cached_pair$data_def)
          } else {
            NA_character_
          }
        }, error = function(e) NA_character_) else NULL
        s$plot_data_cache_payload <- if (isTRUE(s$had_plot)) cached_pair else NULL
        # Compatibility with the shared session cache resolver. The schema 2.0
        # public field is plot_data_cache_ref; this named map lets the global
        # restore layer materialize restore_plot_data_cache_by_title without
        # storing rendered plot objects.
        if (is.character(s$plot_data_cache_ref) && length(s$plot_data_cache_ref) == 1L &&
            !is.na(s$plot_data_cache_ref) && nzchar(s$plot_data_cache_ref) &&
            !is.null(canonical_key)) {
          s$plot_cache_ref_by_title <- stats::setNames(list(s$plot_data_cache_ref), canonical_key)
        }
        s$plot_ui_cache <- s$plot_ui_inputs
        s$plot_request <- list(
          plot_type = s$ui_inputs$diagramType_Venn %||% s$current_plot_type %||% "Venn",
          labels = s$list_data_names,
          colors = s$list_data_colors,
          show_counts = TRUE,
          show_percentages = s$ui_inputs$showPercentages_Venn,
          show_list_titles = s$ui_inputs$showListTitles_Venn,
          theme = s$ui_inputs$ThemeSelect_Upset,
          text = list(
            overlap_number_size = s$ui_inputs$overlapNumberSize_Venn,
            list_title_size = s$ui_inputs$listTitleSize_Venn,
            list_title_distance = s$ui_inputs$listTitleDistance_Venn,
            category_font = s$ui_inputs$catFont_Venn,
            category_font_style = s$ui_inputs$cat_FontStyle_Venn,
            font_family = s$ui_inputs$font_family_Venn,
            font_style = s$ui_inputs$fontStyle_Venn,
            axis_title_size = s$ui_inputs$axis_title_size_Venn,
            axis_text_size = s$ui_inputs$axis_text_size_Venn,
            label_text_size = s$ui_inputs$label_text_size_Venn
          )
        )
        s
      },
      set_session_state = function(s) {
        canonical_plot_key <- function(ui_inputs = list()) {
          logical_plot_id <- as.character(ui_inputs$diagramType_Venn %||% "Venn")[1]
          variant_fields <- list(
            reference_mode = as.character(ui_inputs$ReferenceValues_Venn %||% "none")[1],
            identifier_column = as.character(ui_inputs$GeneIdentifierColumn_Venn %||% "none")[1]
          )
          variant <- .serialize_plot_variant_spec(variant_fields)
          .build_canonical_plot_cache_key(
            module = "venn",
            logical_plot_id = logical_plot_id,
            variant = variant
          )
        }
        legacy_plot_key <- function(ui_inputs = list()) {
          paste(
            ui_inputs$diagramType_Venn %||% "Venn",
            ui_inputs$ReferenceValues_Venn %||% "none",
            ui_inputs$GeneIdentifierColumn_Venn %||% "none",
            sep = "::"
          )
        }
        if (is.null(s)) return()
        had_plot_on_save <- isTRUE(s$had_plot)
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
          key <- canonical_plot_key(s$plot_ui_inputs)
          cache_key <- if (is.na(cache_key) || !nzchar(cache_key)) key else cache_key
          cand <- s$restore_plot_data_cache_by_title[[key]]
          if (!is.list(cand)) {
            legacy_key <- legacy_plot_key(s$plot_ui_inputs)
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
        }
        if (is.list(s$restore_plot_data_cache)) {
          state$restore_plot_data_cache(s$restore_plot_data_cache)
        } else {
          state$restore_plot_data_cache(NULL)
        }
        # Set list data BEFORE list count so that when the renderUI fires
        # (triggered by list_count_Venn change), the data is already in place.
        # Some older snapshots can contain a restored list_count that is longer
        # than the saved per-list helper vectors. Pad those module-local lists so
        # dynamic UI/update observers never index beyond their bounds.
        normalize_list_count <- function(count) {
          count <- tryCatch(suppressWarnings(as.integer(count)[1]), error = function(e) NA_integer_)
          if (is.na(count) || count < 1L) count <- 3L
          min(count, 25L)
        }
        set_defs <- if (is.list(s$set_definitions)) s$set_definitions else list()
        restored_names <- s$list_data_names %||% set_defs$names
        restored_lists <- s$list_data_lists %||% set_defs$lists
        restored_colors <- s$list_data_colors %||% set_defs$colors
        restored_list_count <- normalize_list_count(s$list_count_Venn %||% set_defs$count %||% length(restored_names))
        pad_list_data <- function(values, default_value) {
          values <- if (is.list(values)) values else as.list(values %||% list())
          if (length(values) < restored_list_count) {
            values <- c(values, rep(list(default_value), restored_list_count - length(values)))
          }
          values[seq_len(restored_list_count)]
        }
        pad_list_names <- function(values) {
          values <- if (is.list(values)) values else as.list(values %||% list())
          if (length(values) < restored_list_count) {
            missing_idx <- seq.int(length(values) + 1L, restored_list_count)
            values <- c(values, as.list(paste("List", missing_idx)))
          }
          values[seq_len(restored_list_count)]
        }
        state$list_data_Venn$names  <- pad_list_names(restored_names %||% isolate(state$list_data_Venn$names))
        state$list_data_Venn$lists  <- pad_list_data(restored_lists %||% isolate(state$list_data_Venn$lists), "")
        state$list_data_Venn$colors <- pad_list_data(restored_colors %||% isolate(state$list_data_Venn$colors), "#868686FF")
        state$list_data_Venn$gsea   <- pad_list_data(set_defs$gsea %||% isolate(state$list_data_Venn$gsea), NULL)
        state$list_data_Venn$go     <- pad_list_data(set_defs$go %||% isolate(state$list_data_Venn$go), NULL)
        state$list_data_Venn$sample <- pad_list_data(set_defs$sample %||% isolate(state$list_data_Venn$sample), NULL)
        state$list_data_Venn$core_enriched <- pad_list_data(set_defs$core_enriched %||% isolate(state$list_data_Venn$core_enriched), TRUE)

        list_data_lengths <- function() {
          c(
            names = length(isolate(state$list_data_Venn$names)),
            lists = length(isolate(state$list_data_Venn$lists)),
            colors = length(isolate(state$list_data_Venn$colors)),
            gsea = length(isolate(state$list_data_Venn$gsea)),
            go = length(isolate(state$list_data_Venn$go)),
            sample = length(isolate(state$list_data_Venn$sample))
          )
        }
        restored_list_lengths <- list_data_lengths()
        format_restored_list_lengths <- function(lengths) {
          paste(paste0(names(lengths), "=", as.integer(lengths)), collapse = ", ")
        }

        if (any(restored_list_lengths < restored_list_count)) {
          debug_log(paste0("[Venn] restored list data shorter than restored_list_count=",
                           restored_list_count, "; lengths=",
                           format_restored_list_lengths(restored_list_lengths),
                           "; padding again"), 1)
          state$list_data_Venn$names  <- pad_list_names(isolate(state$list_data_Venn$names))
          state$list_data_Venn$lists  <- pad_list_data(isolate(state$list_data_Venn$lists), "")
          state$list_data_Venn$colors <- pad_list_data(isolate(state$list_data_Venn$colors), "#868686FF")
          state$list_data_Venn$gsea   <- pad_list_data(isolate(state$list_data_Venn$gsea), NULL)
          state$list_data_Venn$go     <- pad_list_data(isolate(state$list_data_Venn$go), NULL)
          state$list_data_Venn$sample <- pad_list_data(isolate(state$list_data_Venn$sample), NULL)
          restored_list_lengths <- list_data_lengths()
        }

        state$list_count_Venn(restored_list_count)
        if (!is.null(s$intersection_list)) state$intersection_list(s$intersection_list)
        if (is.logical(s$plot_active))     state$plot_active(FALSE)
        if (!is.null(s$current_plot_type)) state$current_plot_type(s$current_plot_type)
        state$had_plot_on_save(isTRUE(s$had_plot))
        # A missing shared cache may still be harmless when the staged lists,
        # UI, and current live data can reproduce the requested plot.  Permit
        # that attempt and decide whether to notify only from its final report.
        state$restore_require_cached_data(
          isTRUE(s$had_plot) && (isTRUE(cache_hit) || isTRUE(degraded_cache_restore))
        )
        state$last_restore_report(NULL)
        if (!is.null(s$num_intersections_export)) state$num_intersections_export(s$num_intersections_export)
        # Do not restore bulky plot objects directly; regenerate from restored inputs.
        state$current_venn_plot(NULL)
        state$restored_plot_cache(NULL)
        # Stage captured UI inputs for the session_restore_trigger observer.
        # Prefer ui_inputs, then plot_ui_inputs (legacy/alternate key).
        staged_ui_inputs <- NULL
        if (!is.null(s$ui_inputs) && is.list(s$ui_inputs)) {
          staged_ui_inputs <- s$ui_inputs
        } else if (!is.null(s$plot_ui_inputs) && is.list(s$plot_ui_inputs)) {
          staged_ui_inputs <- s$plot_ui_inputs
        }
        if (!is.list(staged_ui_inputs)) {
          staged_ui_inputs <- list()
        }

        # Normalize dynamic list controls into the same IDs used by the UI.
        # Older/lean snapshots store list names, contents, and colors in the
        # module-level list_data_* vectors rather than as name1/list1/color1
        # entries inside ui_inputs.  The restore observer and cached rebuild
        # path both resolve UI fields by input ID, so materialize those fields
        # here to make restore deterministic without depending on browser echo.
        for (i in seq_len(restored_list_count)) {
          name_id <- paste0("name", i)
          list_id <- paste0("list", i)
          color_id <- paste0("color", i)
          if (is.null(staged_ui_inputs[[name_id]])) {
            staged_ui_inputs[[name_id]] <- isolate(state$list_data_Venn$names[[i]])
          }
          if (is.null(staged_ui_inputs[[list_id]])) {
            staged_ui_inputs[[list_id]] <- isolate(state$list_data_Venn$lists[[i]])
          }
          if (is.null(staged_ui_inputs[[color_id]])) {
            staged_ui_inputs[[color_id]] <- isolate(state$list_data_Venn$colors[[i]])
          }
        }

        # Ensure hidden/dependent selector values are present for cached replay
        # when they were available in the plot snapshot but absent from a legacy
        # ui_inputs payload.  Values that are genuinely irrelevant to the saved
        # diagram type remain NULL and are ignored by list-only plot builders.
        saved_plot_ui <- s$plot_ui_inputs %||% s$ui_inputs
        if (is.list(saved_plot_ui)) {
          for (id in names(saved_plot_ui)) {
            if (is.null(staged_ui_inputs[[id]])) {
              staged_ui_inputs[[id]] <- saved_plot_ui[[id]]
            }
          }
        }

        state$pending_ui_inputs(staged_ui_inputs)
        notice_sequence <- isolate(restore_notice_sequence()) + 1L
        restore_notice_sequence(notice_sequence)
        restore_generation <- isolate(rv$session_restore_generation %||% NA_integer_)
        generation_key <- if (length(restore_generation) == 1L && !is.na(restore_generation)) {
          paste0("session-", restore_generation)
        } else {
          paste0("setter-", notice_sequence)
        }
        restore_notice_context(list(
          generation_key = generation_key,
          plot_requested = isTRUE(had_plot_on_save),
          cache_missing = isTRUE(cache_intended_restore) && !isTRUE(cache_hit)
        ))
        report <- list(
          cache_key = cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else "live",
          reason = if (isTRUE(cache_hit)) cache_hit_reason else if (isTRUE(degraded_cache_restore)) "cache_ref_unresolved_live_data_incompatible" else "cache_miss_fallback_live",
          restore_cache_degraded = isTRUE(degraded_cache_restore)
        )
        record_restore_report("Venn", report)
        debug_log(paste0("[Venn] session state restored via set_session_state (had_plot=",
                         isTRUE(s$had_plot),
                         ", restored_list_count=", restored_list_count,
                         ", list_data_lengths=", format_restored_list_lengths(restored_list_lengths),
                         ")"), 1)
      }
    )

  })
}
