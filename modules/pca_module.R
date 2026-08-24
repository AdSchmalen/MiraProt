# ==============================================================================
# File: modules/pca_module.R
#
# Purpose:
#   Orchestrator for the PCA/Dimension Reduction module. Sources all sub-files,
#   defines the UI wrapper (modPCAUI), and wires the server function
#   (modPCAServer). This is the only file in the module that contains a server
#   function.
#
# Architectural Role:
#   This file delegates all responsibilities to the sub-files in modules/PCA/:
#     - pca_module_UI.R              : Static UI layout and input widgets
#     - pca_module_utils.R           : Pure utility functions (no Shiny dependency)
#     - pca_module_static.R          : Static ggplot2 plot creation
#     - pca_module_interactive.R     : Plotly interactive plot creation
#     - pca_module_state.R           : Reactive state factory (init_pca_state)
#     - pca_module_server_observers.R: All observer and output registrations
#     - pca_module_server_pipeline.R : Analysis pipeline and rendering pipeline
#
# Structure:
#   1. sys.source calls for all sub-files
#   2. modPCAUI()     - UI wrapper calling create_pca_ui()
#   3. modPCAServer() - Server function:
#      a. Debug setup (DEBUG_LEVEL, debug_log)
#      b. State initialization via init_pca_state()
#      c. Observer/pipeline registration via register_pca_* functions
#      d. Return interface
#
# Return Interface (public API):
#   analysis_results       : reactive returning current method/target results
#   plots_ready            : reactiveVal (logical)
#   sample_pca_results     : reactiveVal for Sample PCA
#   protein_pca_results    : reactiveVal for Protein PCA
#   sample_umap_results    : reactiveVal for Sample UMAP
#   protein_umap_results   : reactiveVal for Protein UMAP
#   get_all_results        : function returning all stored results as a list
#   has_any_results        : function returning TRUE if any result is stored
#   static_plot_obj        : reactiveVal for the current ggplot2 object
#   interactive_plot_obj   : reactiveVal for the current plotly object
#   scree_plot_obj         : reactiveVal for the scree plot
#   selected_data_pca      : reactiveVal for selected data rows
#   selected_protein_vector_pca : reactiveVal for selected protein names
#   module_ready           : reactive always returning TRUE
#   module_health_check    : function returning status string
#
# Notes for future developers:
#   - No server logic lives here. All observers, outputs, and reactives are
#     defined in the sub-files and registered through the register_pca_* calls.
#   - debug_log is defined here and passed explicitly to every registration
#     function that requires logging.
#   - No explicit session cleanup is registered as the module holds only
#     session-local reactive values that are garbage-collected automatically
#     when the Shiny session ends.
# ==============================================================================

for (.pca_restore_source in c(
  "session_save_restore_cache_keys.R",
  "session_save_restore_core_helpers.R",
  "session_save_restore_callbacks.R"
)) {
  .pca_restore_path <- file.path("R", "session_save_restore", .pca_restore_source)
  if (file.exists(.pca_restore_path)) sys.source(.pca_restore_path, envir = modEnv)
}

sys.source("modules/PCA/pca_module_UI.R",               envir = modEnv)
sys.source("modules/PCA/pca_module_utils.R",             envir = modEnv)
sys.source("modules/PCA/pca_module_static.R",            envir = modEnv)
sys.source("modules/PCA/pca_module_interactive.R",       envir = modEnv)
sys.source("modules/PCA/pca_module_state.R",             envir = modEnv)
sys.source("modules/PCA/pca_module_server_observers.R",  envir = modEnv)
sys.source("modules/PCA/pca_module_server_pipeline.R",   envir = modEnv)

# ==============================================================================
# UI
# ==============================================================================

modPCAUI <- function(id) {
  ns <- NS(id)

  tagList(
    create_pca_ui(id),
    useShinyjs(),
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('%s', null);
      });
    ", ns("clipboard_ready"))))
  )
}

# Normalize every supported PCA label snapshot into one mode-specific payload.
# This helper is deliberately free of reactive/Shiny dependencies so legacy
# session envelopes can be validated before any module state is mutated.
normalize_pca_label_restore_state <- function(label_state = NULL,
                                              legacy_labels = NULL,
                                              analysis_results = NULL,
                                              plot_ui_inputs = NULL,
                                              compatibility = list()) {
  normalize_mode <- function(value) {
    if (is.null(value) || length(value) != 1L || is.na(value)) return(NULL)
    value <- tolower(trimws(as.character(value)))
    if (value %in% c("sample", "samples")) return("samples")
    if (value %in% c("protein", "proteins")) return("proteins")
    NULL
  }
  clean_ids <- function(value) {
    if (is.null(value) || !(is.atomic(value) || is.factor(value))) return(character())
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(trimws(value))]
    unique(value)
  }
  scalar <- function(value, default, type = c("character", "numeric", "logical")) {
    type <- match.arg(type)
    if (is.null(value) || length(value) != 1L || is.na(value)) return(default)
    if (type == "character") return(as.character(value))
    if (type == "logical") return(if (is.logical(value)) value else default)
    value <- suppressWarnings(as.numeric(value))
    if (is.finite(value)) value else default
  }
  empty_items <- function() data.frame(
    item_id = character(), label_color = character(), dot_color = character(),
    use_custom_dot_color = logical(), stringsAsFactors = FALSE
  )
  clean_items <- function(value, selected) {
    required <- c("item_id", "label_color", "dot_color", "use_custom_dot_color")
    if (!inherits(value, "data.frame") || !all(required %in% names(value))) return(empty_items())
    value <- value[, required, drop = FALSE]
    valid <- !is.na(value$item_id) & nzchar(trimws(as.character(value$item_id))) &
      !is.na(value$label_color) & !is.na(value$dot_color) &
      is.logical(value$use_custom_dot_color) & !is.na(value$use_custom_dot_color)
    value <- value[valid, , drop = FALSE]
    value$item_id <- as.character(value$item_id)
    value$label_color <- as.character(value$label_color)
    value$dot_color <- as.character(value$dot_color)
    value <- value[value$item_id %in% selected & !duplicated(value$item_id), , drop = FALSE]
    # The selection vector owns editor order; reactive table row numbers do not.
    value <- value[match(selected[selected %in% value$item_id], value$item_id), , drop = FALSE]
    rownames(value) <- NULL
    value
  }
  is_current <- is.list(label_state) && !is.null(normalize_mode(label_state$mode))
  source <- if (is_current) label_state else if (is.list(legacy_labels)) legacy_labels else list()
  mode <- normalize_mode(source$mode) %||%
    normalize_mode(if (is.list(analysis_results)) analysis_results$comparison_target else NULL) %||%
    normalize_mode(if (is.list(plot_ui_inputs)) plot_ui_inputs$comparison_target else NULL) %||% "samples"
  fallback <- function(name) {
    value <- source[[name]]
    if (is.null(value) && is.list(plot_ui_inputs)) value <- plot_ui_inputs[[name]]
    if (is.null(value)) value <- compatibility[[name]]
    value
  }
  section_value <- function(section, name, legacy_name = NULL) {
    value <- if (is.list(source[[section]])) source[[section]][[name]] else NULL
    if (is.null(value)) value <- fallback(name)
    settings <- source$settings
    if (is.null(settings)) settings <- compatibility[[if (mode == "samples") "sample_label_settings_pca" else "item_label_settings_pca"]]
    if (is.null(value) && !is.null(legacy_name) && is.list(settings)) value <- settings[[legacy_name]]
    value
  }
  geometry <- if (mode == "samples") list(
    maxOverlaps_samples = scalar(section_value("geometry_controls", "maxOverlaps_samples", "max_overlaps"), 10, "numeric"),
    labelDistance_samples = scalar(section_value("geometry_controls", "labelDistance_samples", "label_distance"), .25, "numeric"),
    lineThickness_samples = scalar(section_value("geometry_controls", "lineThickness_samples", "line_thickness"), .5, "numeric"),
    labelSize_samples = scalar(section_value("geometry_controls", "labelSize_samples", "label_size"), 8, "numeric"),
    dotSizeLabeled_samples = scalar(section_value("geometry_controls", "dotSizeLabeled_samples", "labeled_dot_size"), 2, "numeric")
  ) else list(
    maxOverlaps_pca = scalar(section_value("geometry_controls", "maxOverlaps_pca"), 10, "numeric"),
    labelDistance_pca = scalar(section_value("geometry_controls", "labelDistance_pca"), .25, "numeric"),
    lineThickness_pca = scalar(section_value("geometry_controls", "lineThickness_pca"), .5, "numeric"),
    labelSize_pca = scalar(section_value("geometry_controls", "labelSize_pca"), 8, "numeric"),
    dotSizeLabeled_pca = scalar(section_value("geometry_controls", "dotSizeLabeled_pca"), 2, "numeric")
  )
  general <- if (mode == "samples") list(
    masterLabelColor_samples = scalar(section_value("general_controls", "masterLabelColor_samples", "master_label_color"), "#000000"),
    masterDotColor_samples = scalar(section_value("general_controls", "masterDotColor_samples", "master_dot_color"), "#E0E0E0"),
    masterCustomDot_samples = scalar(section_value("general_controls", "masterCustomDot_samples", "use_master_dot_color"), FALSE, "logical")
  ) else list(
    masterLabelColor_pca = scalar(section_value("general_controls", "masterLabelColor_pca"), "#000000"),
    masterDotColor_pca = scalar(section_value("general_controls", "masterDotColor_pca"), "#E0E0E0"),
    masterCustomDot_pca = scalar(section_value("general_controls", "masterCustomDot_pca"), FALSE, "logical")
  )
  if (mode == "samples") {
    active <- scalar(source$labeling_active %||% source$sample_labeling_active_pca %||%
                       compatibility$sample_labeling_active_pca, FALSE, "logical")
    sample_settings <- list(
      master_label_color = general$masterLabelColor_samples,
      master_dot_color = general$masterDotColor_samples,
      use_master_dot_color = general$masterCustomDot_samples,
      max_overlaps = geometry$maxOverlaps_samples,
      label_distance = geometry$labelDistance_samples,
      line_thickness = geometry$lineThickness_samples,
      label_size = geometry$labelSize_samples,
      labeled_dot_size = geometry$dotSizeLabeled_samples,
      active = active
    )
    return(list(mode = mode, labeling_active = active, selection = list(),
                general_controls = general, geometry_controls = geometry,
                item_controls = empty_items(), selected_items = character(),
                item_settings = empty_items(), sample_settings = sample_settings))
  }
  selection <- if (is.list(source$selection)) source$selection else list()
  selected <- clean_ids(selection$selected_items %||% source$selected_items %||%
                          source$selected_items_vector_pca %||% source$labeled_proteins %||%
                          compatibility$selected_items_vector_pca %||% compatibility$labeled_proteins)
  draft <- scalar(selection$searchGene_pca %||% source$searchGene_pca %||%
                    if (is.list(plot_ui_inputs)) plot_ui_inputs$searchGene_pca else NULL, "")
  items <- source$item_controls %||% source$settings %||% source$item_label_settings_pca %||%
    compatibility$item_label_settings_pca
  items <- clean_items(items, selected)
  list(mode = mode, labeling_active = FALSE,
       selection = list(selected_items = selected, searchGene_pca = draft),
       general_controls = general, geometry_controls = geometry,
       item_controls = items, selected_items = selected,
       item_settings = items, sample_settings = list())
}
# ==============================================================================
# SERVER
# ==============================================================================

modPCAServer <- function(id, rv, res_GSEA = NULL, res_GO = NULL, module_outputs = NULL, debug_level = 0, modEnv = new.env()) {
  DEBUG_LEVEL <- suppressWarnings(as.integer(debug_level))[1]
  if (length(DEBUG_LEVEL) == 0 || !is.finite(DEBUG_LEVEL)) DEBUG_LEVEL <- 0

  moduleServer(id, function(input, output, session, local = modEnv) {

    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug setup
    # --------------------------------------------------------------------------

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "PCA MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ PCA MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("PCA module server starting", 1)

    pca_build_cache_key <- function(module, logical_plot_id, variant) {
      .build_canonical_plot_cache_key(
        module = module, logical_plot_id = logical_plot_id, variant = variant
      )
    }

    record_restore_report <- function(module_name, report) {
      isolate({
        reports <- rv$restore_reports
        if (!is.list(reports)) reports <- list()
        reports[[module_name]] <- report
        rv$restore_reports <- reports
        if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
          rv$restore_diagnostics <- reports
        }
      })
      debug_log(sprintf("[%s] restore report: key=%s hit=%s source=%s reason=%s",
                        module_name,
                        as.character(report$cache_key %||% NA_character_),
                        as.character(report$cache_hit %||% NA),
                        as.character(report$data_source %||% NA_character_),
                        as.character(report$reason %||% NA_character_)), 1)
    }

    # --------------------------------------------------------------------------
    # State initialization
    # --------------------------------------------------------------------------

    pca_state <- init_pca_state(input = input, debug_log = debug_log)

    analysis_results            <- pca_state$analysis_results
    plots_ready                 <- pca_state$plots_ready
    selected_points_interactive <- pca_state$selected_points_interactive
    labeled_proteins            <- pca_state$labeled_proteins
    protein_suggestions         <- pca_state$protein_suggestions
    static_plot_obj             <- pca_state$static_plot_obj
    interactive_plot_obj        <- pca_state$interactive_plot_obj
    available_components        <- pca_state$available_components
    selected_data_pca           <- pca_state$selected_data_pca
    selected_protein_vector_pca <- pca_state$selected_protein_vector_pca
    ggplot_object_PCATab        <- pca_state$ggplot_object_PCATab
    scree_plot_obj              <- pca_state$scree_plot_obj
    selected_items_vector_pca   <- pca_state$selected_items_vector_pca
    item_label_settings_pca     <- pca_state$item_label_settings_pca
    sample_labeling_active_pca  <- pca_state$sample_labeling_active_pca
    sample_label_settings_pca   <- pca_state$sample_label_settings_pca
    executed_method             <- pca_state$executed_method
    render_nonce                <- pca_state$render_nonce
    sample_pca_results          <- pca_state$sample_pca_results
    protein_pca_results         <- pca_state$protein_pca_results
    sample_umap_results         <- pca_state$sample_umap_results
    protein_umap_results        <- pca_state$protein_umap_results

    get_current_analysis_results <- pca_state$get_current_analysis_results
    get_all_analysis_results     <- pca_state$get_all_analysis_results
    store_analysis_results       <- pca_state$store_analysis_results

    # --------------------------------------------------------------------------
    # Observer and pipeline registration
    # --------------------------------------------------------------------------

    register_pca_input_observers(
      input     = input,
      session   = session,
      rv        = rv,
      debug_log = debug_log,
      state     = pca_state
    )

    register_pca_protein_selection_observers(
      input                       = input,
      output                      = output,
      session                     = session,
      rv                          = rv,
      selected_items_vector_pca   = selected_items_vector_pca,
      selected_data_pca           = selected_data_pca,
      selected_protein_vector_pca = selected_protein_vector_pca,
      debug_log                   = debug_log
    )

    register_pca_pathway_observers(
      input     = input,
      session   = session,
      res_GSEA  = res_GSEA,
      res_GO    = res_GO,
      debug_log = debug_log
    )

    register_pca_ui_state_observers(
      input     = input,
      session   = session,
      rv        = rv,
      state     = pca_state,
      debug_log = debug_log
    )

    register_pca_label_management_observers(
      input     = input,
      output    = output,
      session   = session,
      ns        = ns,
      state     = pca_state,
      debug_log = debug_log
    )

    register_pca_analysis_core(
      input                  = input,
      rv                     = rv,
      analysis_results       = analysis_results,
      store_analysis_results = store_analysis_results,
      plots_ready            = plots_ready,
      debug_log              = debug_log,
      state                  = pca_state
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
      c("plotly_selected-pca_plot", "plotly_click-pca_plot")
    ))

    register_pca_rendering_core(
      input     = input,
      output    = output,
      session   = session,
      rv        = rv,
      state     = pca_state,
      debug_log = debug_log
    )

    register_pca_export_handlers(
      input     = input,
      output    = output,
      session   = session,
      rv        = rv,
      state     = pca_state,
      modEnv    = modEnv,
      ns        = ns,
      debug_log = debug_log
    )

    # --------------------------------------------------------------------------
    # UI input IDs whose values influence the rendered plot. These are captured
    # at save time and pushed back into the UI after restore.
    # --------------------------------------------------------------------------
    pca_ui_input_ids <- c(
      "analysis_method", "comparison_target",
      "axis_x", "axis_y",
      "point_size", "pca_scale",
      "color_palette", "reverse_colors",
      "defaultProteinColor_pca",
      "plot_theme", "legend_position",
      "AxisTitleSize_PCATab", "tickSize_PCATab",
      "LegendTitleSize_PCATab", "LegendTextSize_PCATab",
      "interactive_plot", "show_scree",
      "umap_neighbors", "umap_min_dist", "umap_metric",
      "custom_col_sel_pca", "select_samples_pca",
      "GeneIdentifierColumn_pca",
      "maxOverlaps_pca", "labelDistance_pca",
      "lineThickness_pca", "labelSize_pca", "dotSizeLabeled_pca",
      "masterLabelColor_pca", "masterDotColor_pca", "masterCustomDot_pca",
      "masterLabelColor_samples", "masterDotColor_samples",
      "masterCustomDot_samples",
      "maxOverlaps_samples", "labelDistance_samples",
      "lineThickness_samples", "labelSize_samples", "dotSizeLabeled_samples",
      "resolution_DPI_PCATab", "plotWidthInch_PCATab",
      "plotHeightInch_PCATab", "downloadFormat_PCATab",
      "searchGene_pca"
    )

    # Static PCA widgets grouped by their declared UI type. Dispatch during
    # restore is deliberately based on widget identity rather than the saved
    # value's R type (JSON round-trips can change that type). Dynamic
    # labelColor_pca_*, dotColor_pca_*, and customDot_pca_* controls are not
    # included: the identifier-aware label-editor restore owns those inputs.
    pca_ui_restore_ids <- list(
      updateRadioButtons = c("comparison_target"),
      updateCheckboxInput = c(
        "pca_scale", "reverse_colors", "interactive_plot", "show_scree",
        "masterCustomDot_pca", "masterCustomDot_samples"
      ),
      updateSliderInput = c("point_size"),
      updateNumericInput = c(
        "AxisTitleSize_PCATab", "tickSize_PCATab",
        "LegendTitleSize_PCATab", "LegendTextSize_PCATab",
        "umap_neighbors", "umap_min_dist",
        "maxOverlaps_pca", "labelDistance_pca",
        "lineThickness_pca", "labelSize_pca", "dotSizeLabeled_pca",
        "maxOverlaps_samples", "labelDistance_samples",
        "lineThickness_samples", "labelSize_samples", "dotSizeLabeled_samples",
        "resolution_DPI_PCATab", "plotWidthInch_PCATab",
        "plotHeightInch_PCATab"
      ),
      updateColourInput = c(
        "defaultProteinColor_pca",
        "masterLabelColor_pca", "masterDotColor_pca",
        "masterLabelColor_samples", "masterDotColor_samples"
      ),
      updateTextAreaInput = c("searchGene_pca"),
      updateSelectInput = c(
        "analysis_method", "axis_x", "axis_y", "color_palette",
        "plot_theme", "legend_position", "umap_metric",
        "GeneIdentifierColumn_pca", "downloadFormat_PCATab"
      ),
      updateSelectizeInput = c("custom_col_sel_pca", "select_samples_pca")
    )

    # --------------------------------------------------------------------------
    # Final Data Wizard restore trigger. Like Volcano, this is the sequencing
    # boundary: staged server state is already authoritative, while identifier
    # choices and ordinary browser inputs are restored before one post-flush
    # finalizer releases rendering.
    # --------------------------------------------------------------------------
    pca_restore_generation_is_current <- function(session_generation, pca_generation) {
      identical(isolate(rv$session_restore_generation %||% NA_integer_), session_generation) &&
        identical(isolate(pca_state$restore_generation()), pca_generation)
    }

    # A restore trigger is a notification, rather than a unique restore job.
    # Data Wizard (or another publisher sharing rv) may publish the same value
    # more than once, so remember both the in-flight and completed generation.
    # This is deliberately local server bookkeeping: it must not itself create
    # a reactive dependency or cause another render invalidation.
    pca_claimed_restore_generation <- NULL
    pca_finalized_restore_generation <- NULL
    pca_restore_generation_key <- function(session_generation, pca_generation) {
      paste(as.character(session_generation)[1], as.character(pca_generation)[1], sep = "::")
    }
    # Job ids are captured per composite generation. They are registered while
    # the restore transaction is still accepting work and settled only by the
    # corresponding finalizer/render callback.
    pca_restore_jobs <- new.env(parent = emptyenv())
    pca_jobs_for <- function(session_generation, pca_generation, create = FALSE) {
      key <- pca_restore_generation_key(session_generation, pca_generation)
      if (!exists(key, pca_restore_jobs, inherits = FALSE) && isTRUE(create))
        assign(key, list(finalizer = NULL, render = NULL), pca_restore_jobs)
      get0(key, pca_restore_jobs, inherits = FALSE, ifnotfound = list(finalizer = NULL, render = NULL))
    }
    pca_put_jobs <- function(session_generation, pca_generation, jobs) {
      assign(pca_restore_generation_key(session_generation, pca_generation), jobs, pca_restore_jobs)
    }
    pca_register_job <- function(reason, phase, timeout) {
      api <- session$userData$restore_jobs
      if (is.list(api) && is.function(api$register_restore_job))
        api$register_restore_job("PCA", reason, phase, timeout = timeout)
      else NULL
    }
    pca_resolve_job <- function(job_id, outcome, error = NULL) {
      api <- session$userData$restore_jobs
      if (is.null(job_id) || !is.list(api) || !is.function(api$resolve_restore_job)) return(FALSE)
      api$resolve_restore_job(job_id, outcome, error)
    }

    observeEvent(rv$session_restore_trigger, {
      observed_generation <- isolate(list(
        session = rv$session_restore_generation %||% NA_integer_,
        pca = pca_state$restore_generation()
      ))
      generation_key <- pca_restore_generation_key(
        observed_generation$session,
        observed_generation$pca
      )
      if (identical(generation_key, pca_claimed_restore_generation) ||
          identical(generation_key, pca_finalized_restore_generation)) {
        debug_log(sprintf(
          "[PCA] ignoring duplicate session restore trigger for generation %s",
          generation_key
        ), 2)
        return(invisible(NULL))
      }
      pca_claimed_restore_generation <<- generation_key
      debug_log(sprintf(
        "[PCA restore] Data Wizard trigger received (session_generation=%s, pca_generation=%s)",
        observed_generation$session, observed_generation$pca
      ), 1)

      # Retain a guard raised during staging, or raise it for legacy/direct
      # callers whose state arrived without the phased restore entry point.
      pca_state$restore_in_progress(TRUE)

      restore_context <- isolate(list(
        session_restore_generation = rv$session_restore_generation %||% NA_integer_,
        pca_restore_generation = pca_state$restore_generation(),
        datawizard_identifier_choices = rv$datawizard_identifier_option_choices %||% character(0),
        central_identifier_choices = rv$central_identifier_choices %||% character(0),
        data_def = rv$data_def,
        pending_ui_inputs = pca_state$pending_ui_inputs(),
        restored_identifier = pca_state$restored_identifier_column(),
        analysis_results = pca_state$analysis_results(),
        restore_cache = pca_state$restore_plot_data_cache(),
        plots_ready = pca_state$plots_ready(),
        rebuild_expected = isTRUE(rv$pca_restore_rebuild_expected),
        expected_input_echoes = pca_state$expected_restore_input_echoes() %||% list()
      ))

      finalized <- FALSE
      finalize_restore <- function(status = "complete", request_render = TRUE) {
        if (isTRUE(finalized) || !pca_restore_generation_is_current(
          restore_context$session_restore_generation,
          restore_context$pca_restore_generation
        )) return(invisible(FALSE))
        finalized <<- TRUE
        pca_state$pending_ui_inputs(NULL)
        pca_state$pending_label_ui_state(NULL)
        pca_state$ordinary_ui_restore_complete(FALSE)
        pca_state$label_restore_stage(list(
          generation = restore_context$pca_restore_generation,
          stage = status
        ))
        pca_state$restore_in_progress(FALSE)
        debug_log(sprintf("[PCA restore] restore guard released (status=%s)", status), 1)
        if (isTRUE(request_render)) {
          pca_state$render_nonce(isolate(pca_state$render_nonce()) + 1L)
          debug_log(sprintf(
            "[PCA restore] render requested (pca_generation=%s)",
            restore_context$pca_restore_generation
          ), 1)
        } else {
          debug_log(sprintf(
            "[PCA restore] no-plot restore completed (pca_generation=%s)",
            restore_context$pca_restore_generation
          ), 1)
        }
        pca_finalized_restore_generation <<- generation_key
        pca_claimed_restore_generation <<- NULL
        debug_log(sprintf("[PCA] session restore finalized (%s); rendering re-enabled", status), 1)
        # Session restore guard cleared; live refreshes re-enabled.
        invisible(TRUE)
      }
      schedule_finalizer <- function(status = "complete") {
        jobs <- pca_jobs_for(restore_context$session_restore_generation,
                            restore_context$pca_restore_generation, create = TRUE)
        if (is.null(jobs$finalizer)) {
          jobs$finalizer <- tryCatch(
            pca_register_job("restore finalizer", "finalizer", timeout = 15),
            error = function(e) NULL
          )
          pca_put_jobs(restore_context$session_restore_generation,
                       restore_context$pca_restore_generation, jobs)
        }
        finalizer_job <- jobs$finalizer
        session$onFlushed(function() {
          .run_session_restore_callback(
            owner = "PCA", reason = "restore finalizer",
            generation = restore_context$session_restore_generation,
            phase = "finalizer",
            callback = function() {
              if (!pca_restore_generation_is_current(
                    restore_context$session_restore_generation,
                    restore_context$pca_restore_generation))
                stop("STALE_PCA_GENERATION")
              if (!isTRUE(finalize_restore(status))) stop("PCA finalizer was not applied")
            },
            job_metadata = list(
              job_id = finalizer_job,
              resolve_job = pca_resolve_job,
              current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_)
            )
          )
        }, once = TRUE)
      }

      restore_status <- "complete"
      tryCatch({
        captured <- restore_context$pending_ui_inputs
        if (is.null(captured) || !is.list(captured)) captured <- list()

        published_identifier_choices <- restore_context$datawizard_identifier_choices
        if (length(published_identifier_choices) == 0L) {
          published_identifier_choices <- restore_context$central_identifier_choices
        }
        identifier_indices <- if (is.data.frame(restore_context$data_def) &&
                                  all(c("Content", "Options") %in% names(restore_context$data_def))) {
          which(grepl("Identifier", restore_context$data_def$Content, ignore.case = TRUE))
        } else integer(0)
        identifier_choices <- if (length(published_identifier_choices) > 0L) {
          published_identifier_choices
        } else if (length(identifier_indices) > 0L) {
          restore_context$data_def$Options[identifier_indices]
        } else character(0)
        identifier_choices <- unique(as.character(identifier_choices))
        identifier_choices <- identifier_choices[!is.na(identifier_choices) & nzchar(identifier_choices)]
        debug_log(sprintf(
          "[PCA restore] restored identifier choices available (count=%d)",
          length(identifier_choices)
        ), 1)

        saved_identifier <- as.character(captured$GeneIdentifierColumn_pca %||%
                                           restore_context$restored_identifier)[1]
        saved_plot_rebuild_expected <- isTRUE(restore_context$rebuild_expected)
        compact_pca_result_available <- is.list(restore_context$analysis_results) &&
          !is.null(restore_context$analysis_results$coordinates)
        valid_saved_cache_pair_available <- is.list(restore_context$restore_cache) &&
          inherits(restore_context$restore_cache$data_mod, "data.frame") &&
          inherits(restore_context$restore_cache$data_def, "data.frame")
        meaningful_current_live_metadata_available <- length(identifier_choices) > 0L
        saved_identifier_present_in_live_choices <-
          meaningful_current_live_metadata_available &&
          length(saved_identifier) > 0L && !is.na(saved_identifier) &&
          saved_identifier %in% identifier_choices

        if (saved_plot_rebuild_expected && compact_pca_result_available &&
            valid_saved_cache_pair_available &&
            !meaningful_current_live_metadata_available) {
          reports <- isolate(rv$restore_reports)
          if (!is.list(reports)) reports <- list()
          report <- reports$PCA %||% list()
          live_ui_note <- paste(
            "Cached PCA plot was restored; current live metadata cannot populate",
            "the identifier selector (live UI not applicable)."
          )
          report$live_ui_notes <- unique(c(report$live_ui_notes %||% character(0), live_ui_note))
          report$live_identifier_selector_status <- "not_applicable"
          reports$PCA <- report
          rv$restore_reports <- reports
          if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
            rv$restore_diagnostics <- reports
          }
          debug_log(paste("[PCA restore] live UI note:", live_ui_note), 1)
        } else if (meaningful_current_live_metadata_available &&
                   length(saved_identifier) > 0L && !is.na(saved_identifier) &&
                   !saved_identifier_present_in_live_choices) {
          reports <- isolate(rv$restore_reports)
          if (!is.list(reports)) reports <- list()
          report <- reports$PCA %||% list()
          warning_message <- paste(
            "Saved PCA identifier is unavailable in current live metadata;",
            "the live selector uses a compatible fallback without adding the cached identifier."
          )
          report$warnings <- unique(c(report$warnings %||% character(0), warning_message))
          reports$PCA <- report
          rv$restore_reports <- reports
          if (isTRUE(getOption("miraprot.restore_diagnostics_panel", FALSE))) {
            rv$restore_diagnostics <- reports
          }
          debug_log(paste("[PCA restore] warning:", warning_message), 1)
          restore_status <- "degraded"
        }

        if (length(identifier_choices) > 0L) {
          selected_identifier <- if (saved_identifier_present_in_live_choices) {
            saved_identifier
          } else identifier_choices[1]
          selection_source <- if (length(saved_identifier) > 0L && !is.na(saved_identifier) &&
                                  identical(selected_identifier, saved_identifier)) "saved" else "fallback"
          debug_log(sprintf(
            "[PCA restore] %s identifier selected (%s)",
            selection_source, selected_identifier
          ), 1)
          updateSelectInput(session, "GeneIdentifierColumn_pca",
                            choices = identifier_choices, selected = selected_identifier)
          expected <- restore_context$expected_input_echoes
          expected$GeneIdentifierColumn_pca <- selected_identifier
          pca_state$expected_restore_input_echoes(expected)
        }

        coerce_checkbox_value <- function(value) {
          if (is.logical(value)) return(isTRUE(value[1]))
          if (is.numeric(value)) return(isTRUE(value[1] >= .5))
          normalized <- tolower(trimws(as.character(value)[1]))
          normalized %in% c("true", "t", "yes", "y", "on", "1")
        }
        restore_updaters <- list(
          updateRadioButtons = function(id, value) updateRadioButtons(session, id, selected = value),
          updateCheckboxInput = function(id, value) updateCheckboxInput(session, id, value = coerce_checkbox_value(value)),
          updateSliderInput = function(id, value) updateSliderInput(session, id, value = suppressWarnings(as.numeric(value))),
          updateNumericInput = function(id, value) updateNumericInput(session, id, value = suppressWarnings(as.numeric(value)[1])),
          updateColourInput = function(id, value) colourpicker::updateColourInput(session, id, value = as.character(value)[1]),
          updateTextAreaInput = function(id, value) updateTextAreaInput(session, id, value = as.character(value)[1]),
          updateSelectInput = function(id, value) updateSelectInput(session, id, selected = value),
          updateSelectizeInput = function(id, value) updateSelectizeInput(session, id, selected = value)
        )
        for (updater_name in names(pca_ui_restore_ids)) {
          for (id in intersect(pca_ui_restore_ids[[updater_name]], names(captured))) {
            if (identical(id, "GeneIdentifierColumn_pca")) next
            value <- captured[[id]]
            if (!is.null(value)) restore_updaters[[updater_name]](id, value)
          }
        }

        restored_target <- tolower(trimws(as.character(
          captured$comparison_target %||% restore_context$analysis_results$comparison_target %||% ""
        )[1]))
        restored_selection <- isolate(selected_items_vector_pca())
        if (identical(restored_target, "proteins") && length(restored_selection) > 0L) {
          protein_controls_id <- session$ns("protein_controls_content")
          protein_controls_icon_id <- session$ns("protein_controls_icon")
          shinyjs::runjs(sprintf(
            paste0(
              "(function(){",
              "var content=document.getElementById('%s');",
              "if(!content||content.style.display!=='none')return;",
              "content.style.display='block';",
              "var icon=document.getElementById('%s');",
              "if(icon){icon.classList.remove('fa-chevron-right');icon.classList.add('fa-chevron-down');}",
              "})();"
            ),
            protein_controls_id,
            protein_controls_icon_id
          ))
        }

        if (saved_plot_rebuild_expected && !compact_pca_result_available) {
          stop("saved plot has no restored compact analysis result")
        }
        if (saved_plot_rebuild_expected && !valid_saved_cache_pair_available) {
          debug_log("[PCA] restored cache unavailable; compact result will render in degraded mode", 1)
          restore_status <- "degraded"
        }
        pca_state$ordinary_ui_restore_complete(TRUE)
        debug_log("[PCA restore] ordinary PCA UI synchronization completed", 1)
      }, error = function(e) {
        restore_status <<- "degraded"
        pca_state$ordinary_ui_restore_complete(TRUE)
        debug_log(paste("[PCA restore] ordinary PCA UI synchronization failed:", e$message), 1)
      })

      # A snapshot that did not contain a plot still restores its compact
      # analysis and UI state, but has no render to await. Release its guard
      # synchronously without registering a plot finalizer or advancing the
      # render nonce. Plot-bearing snapshots retain the flushed finalizer path.
      if (isTRUE(isolate(rv$pca_restore_rebuild_expected))) {
        schedule_finalizer(restore_status)
      } else {
        finalize_restore("no_plot", request_render = FALSE)
      }
    }, ignoreInit = TRUE)

    debug_log("PCA module initialized successfully", 1)

    # --------------------------------------------------------------------------
    # Return interface
    # --------------------------------------------------------------------------

    return(list(
      analysis_results    = get_current_analysis_results,
      plots_ready         = plots_ready,

      sample_pca_results  = sample_pca_results,
      protein_pca_results = protein_pca_results,
      sample_umap_results = sample_umap_results,
      protein_umap_results = protein_umap_results,

      get_all_results = get_all_analysis_results,
      has_any_results = function() {
        !is.null(sample_pca_results()) || !is.null(protein_pca_results()) ||
          !is.null(sample_umap_results()) || !is.null(protein_umap_results())
      },

      static_plot_obj             = static_plot_obj,
      interactive_plot_obj        = interactive_plot_obj,
      scree_plot_obj              = scree_plot_obj,

      selected_data_pca           = selected_data_pca,
      selected_protein_vector_pca = selected_protein_vector_pca,

      module_ready        = reactive(TRUE),
      module_health_check = function() "PCA module operational",

      # Session save/restore interface
      get_session_state = function() {
        canonical_plot_key <- function(comparison_target = NULL) {
          logical_plot_id <- as.character(comparison_target %||% "default")[1]
          pca_build_cache_key(module = "pca", logical_plot_id = logical_plot_id, variant = "main")
        }
        current_inputs <- tryCatch({
          vals <- lapply(pca_ui_input_ids, function(id) isolate(input[[id]]))
          names(vals) <- pca_ui_input_ids
          vals
        }, error = function(e) list())
        current_results <- tryCatch(isolate(analysis_results()), error = function(e) NULL)
        # Results describe the plot that was actually rendered and are therefore
        # authoritative when the UI has since changed. Keep the normalization
        # local to the session envelope so every label payload has one of the two
        # documented discriminator values.
        normalize_target <- function(value) {
          if (is.null(value) || length(value) == 0 || is.na(value[[1]])) return(NULL)
          value <- tolower(trimws(as.character(value[[1]])))
          if (value %in% c("sample", "samples")) return("samples")
          if (value %in% c("protein", "proteins")) return("proteins")
          NULL
        }
        result_target <- if (is.list(current_results)) current_results$comparison_target else NULL
        comparison_target <- normalize_target(result_target) %||%
          normalize_target(current_inputs$comparison_target) %||% "samples"
        canonical_key <- canonical_plot_key(comparison_target)
        cached_pair <- tryCatch({
          # The PCA plot must restore from the data/metadata used for the saved
          # analysis, not from whatever Data Wizard dataset is live at save time.
          if (is.list(current_results) && inherits(current_results$full_data, "data.frame") &&
              inherits(current_results$raw_metadata, "data.frame")) {
            list(data_mod = current_results$full_data, data_def = current_results$raw_metadata)
          } else if (inherits(rv$data_mod, "data.frame") && inherits(rv$data_def, "data.frame")) {
            list(data_mod = rv$data_mod, data_def = rv$data_def)
          } else NULL
        }, error = function(e) NULL)
        cache_key <- tryCatch({
          if (is.list(cached_pair)) {
            .build_plot_data_cache_id(data_mod = cached_pair$data_mod, data_def = cached_pair$data_def)
          } else {
            NA_character_
          }
        }, error = function(e) NA_character_)
        compact_result <- function(results) {
          if (!is.list(results)) return(NULL)
          list(
            method = results$method,
            comparison_target = results$comparison_target,
            coordinates = results$coordinates,
            explained_variance = results$var_explained,
            cumulative_explained_variance = results$cumvar_explained,
            selected_components = list(
              x = current_inputs$axis_x %||% results$x_axis,
              y = current_inputs$axis_y %||% results$y_axis
            ),
            point_names = results$point_names,
            metadata = results$metadata,
            raw_metadata = results$raw_metadata,
            identifier_col = results$identifier_col,
            selected_samples = results$selected_samples,
            selected_data_type = results$selected_data_type,
            n_components = results$n_components,
            sdev = results$sdev
          )
        }
        state <- list(
          version = PCA_SESSION_SCHEMA_VERSION,
          restore_cache_dependency = "shared_plot_data_cache_pool"
        )
        state$ui_inputs <- list(
          selected_method = current_inputs$analysis_method,
          sample_grouping = current_inputs$comparison_target,
          color_shape_label_options = current_inputs[intersect(names(current_inputs), c(
            "color_palette", "reverse_colors", "defaultProteinColor_pca",
            "custom_col_sel_pca", "GeneIdentifierColumn_pca",
            "maxOverlaps_pca", "labelDistance_pca", "lineThickness_pca",
            "labelSize_pca", "dotSizeLabeled_pca", "masterLabelColor_pca",
            "masterDotColor_pca", "masterCustomDot_pca", "masterLabelColor_samples",
            "masterDotColor_samples", "masterCustomDot_samples", "maxOverlaps_samples",
            "labelDistance_samples", "lineThickness_samples", "labelSize_samples",
            "dotSizeLabeled_samples"
          ))],
          dimensions = list(axis_x = current_inputs$axis_x, axis_y = current_inputs$axis_y),
          theme = list(plot_theme = current_inputs$plot_theme, legend_position = current_inputs$legend_position),
          values = current_inputs
        )
        state$analysis_result <- compact_result(current_results)
        # label_state is an additive, independently versioned part of the PCA
        # 2.0 envelope. Its mode discriminator prevents sample-wide controls and
        # per-protein selections from becoming competing sources of truth.
        label_state <- if (identical(comparison_target, "samples")) {
          list(
            version = "2.0",
            mode = "samples",
            labeling_active = tryCatch(isolate(sample_labeling_active_pca()), error = function(e) FALSE),
            general_controls = current_inputs[c(
              "masterLabelColor_samples", "masterDotColor_samples", "masterCustomDot_samples")],
            geometry_controls = current_inputs[c(
              "maxOverlaps_samples", "labelDistance_samples", "lineThickness_samples",
              "labelSize_samples", "dotSizeLabeled_samples")]
          )
        } else {
          selected_ids <- tryCatch(isolate(selected_items_vector_pca()), error = function(e) character())
          selected_ids <- unique(as.character(selected_ids))
          settings <- tryCatch(isolate(item_label_settings_pca()), error = function(e) NULL)
          if (inherits(settings, "data.frame") && "item_id" %in% names(settings)) {
            settings$item_id <- as.character(settings$item_id)
            settings <- settings[settings$item_id %in% selected_ids, , drop = FALSE]
          } else {
            settings <- data.frame()
          }
          list(
            version = "2.0",
            mode = "proteins",
            selection = list(
              selected_items = selected_ids,
              searchGene_pca = tryCatch(as.character(isolate(input$searchGene_pca) %||% "")[1],
                                        error = function(e) "")
            ),
            general_controls = current_inputs[c(
              "masterLabelColor_pca", "masterDotColor_pca", "masterCustomDot_pca")],
            geometry_controls = current_inputs[c(
              "maxOverlaps_pca", "labelDistance_pca", "lineThickness_pca",
              "labelSize_pca", "dotSizeLabeled_pca")],
            item_controls = settings
          )
        }
        state$plot_request <- list(
          selected_axes = list(x = current_inputs$axis_x, y = current_inputs$axis_y),
          comparison_target = comparison_target,
          gene_identifier_column = current_inputs$GeneIdentifierColumn_pca,
          label_state = label_state,
          colors = current_inputs[intersect(names(current_inputs), c("color_palette", "reverse_colors", "defaultProteinColor_pca"))],
          theme = state$ui_inputs$theme,
          point_size = current_inputs$point_size,
          title = paste(toupper(current_inputs$analysis_method %||% current_results$method %||% "PCA"),
                        "of", comparison_target)
        )
        state$plot_data_cache_ref <- cache_key
        state$plot_data_cache_payload <- cached_pair
        state$had_plot <- isTRUE(tryCatch(isolate(plots_ready()), error = function(e) FALSE)) && !is.null(current_results$coordinates)
        if (!isTRUE(state$had_plot)) {
          state$restore_cache_dependency <- "none"
          state$plot_data_cache_ref <- NULL
          state$plot_data_cache_payload <- NULL
        }

        # Backward-compatible scalar/reactive state needed by current restore observers.
        state$plot_ui_inputs <- current_inputs
        state$analysis_results <- state$analysis_result
        state$sample_pca_results <- compact_result(tryCatch(isolate(sample_pca_results()), error = function(e) NULL))
        state$protein_pca_results <- compact_result(tryCatch(isolate(protein_pca_results()), error = function(e) NULL))
        state$sample_umap_results <- compact_result(tryCatch(isolate(sample_umap_results()), error = function(e) NULL))
        state$protein_umap_results <- compact_result(tryCatch(isolate(protein_umap_results()), error = function(e) NULL))
        state$executed_method <- tryCatch(isolate(executed_method()), error = function(e) NULL)
        state$available_components <- tryCatch(isolate(available_components()), error = function(e) NULL)
        state$selected_data_pca <- tryCatch(isolate(selected_data_pca()), error = function(e) NULL)
        state$selected_protein_vector_pca <- tryCatch(isolate(selected_protein_vector_pca()), error = function(e) NULL)
        # Compatibility aliases are projections of label_state, not separately
        # captured reactive values. Remove these after legacy restore support is
        # retired.
        state$item_label_settings_pca <- if (identical(label_state$mode, "proteins")) label_state$item_controls else data.frame()
        state$sample_label_settings_pca <- if (identical(label_state$mode, "samples")) {
          normalize_pca_label_restore_state(label_state)$sample_settings
        } else list()
        state$sample_labeling_active_pca <- if (identical(label_state$mode, "samples")) label_state$labeling_active else FALSE
        state$selected_items_vector_pca <- if (identical(label_state$mode, "proteins")) label_state$selection$selected_items else character()
        state$labeled_proteins <- state$selected_items_vector_pca
        state$plots_ready <- state$had_plot
        state$plot_cache_ref_by_title <- if (isTRUE(state$had_plot) &&
            is.character(cache_key) && length(cache_key) == 1L &&
            !is.na(cache_key) && nzchar(cache_key)) {
          stats::setNames(list(cache_key), canonical_key)
        } else NULL
        state$plot_ui_cache <- current_inputs
        state
      },
      set_session_state = function(state, phase = NULL) {
        canonical_plot_key <- function(comparison_target = NULL) {
          logical_plot_id <- as.character(comparison_target %||% "default")[1]
          pca_build_cache_key(module = "pca", logical_plot_id = logical_plot_id, variant = "main")
        }
        legacy_plot_key <- function(comparison_target = NULL) {
          key <- .legacy_plot_cache_key(comparison_target)
          debug_log(sprintf("[PCA restore] checking explicit legacy cache key: %s", key), 2)
          key
        }
        if (is.null(state)) return()

        # Phased restores hydrate all authoritative state in
        # full_module_state.  The subsequent plot phase must not repeat those
        # reactive writes or stage another client-side input synchronization;
        # it only records that the fresh render is required once the restore
        # guard is released.  A missing phase retains the historical one-call
        # contract for direct/legacy callers.
        if (identical(phase, "full_module_plots")) {
          restored_results <- state$analysis_results %||% state$analysis_result
          coordinates_available <- is.list(restored_results) &&
            !is.null(restored_results$coordinates)
          saved_plot_intent <- pca_saved_plot_intent(state)
          rv$pca_restore_rebuild_expected <- saved_plot_intent
          restore_context <- isolate(list(
            session_generation = rv$session_restore_generation %||% NA_integer_,
            pca_generation = pca_state$restore_generation()
          ))
          restore_generation <- restore_context$pca_generation
          if (!isTRUE(saved_plot_intent)) {
            report <- isolate({
              reports <- rv$restore_reports
              if (!is.list(reports)) reports <- list()
              reports$PCA %||% list()
            })
            report$restore_generation <- restore_generation
            report$session_restore_generation <- restore_context$session_generation
            report$render_job_id <- NULL
            report$rebuild_requested <- FALSE
            report$plot_recreated <- FALSE
            report$render_completed <- FALSE
            report$render_failed <- FALSE
            report$render_timed_out <- FALSE
            report$render_status <- "no_plot_saved"
            report$restore_outcome <- "no_plot"
            record_restore_report("PCA", report)
            debug_log("[PCA] no saved plot requested; UI and analysis restoration retained", 1)
            return()
          }
          if (!isTRUE(coordinates_available)) {
            debug_log("[PCA] saved plot rebuild requested without compact coordinates", 1)
          }
          jobs <- pca_jobs_for(restore_context$session_generation, restore_context$pca_generation, create = TRUE)
          if (is.null(jobs$finalizer)) {
            jobs$finalizer <- pca_register_job("restore finalizer", "finalizer", timeout = 15)
          }
          timeout_seconds <- suppressWarnings(as.numeric(
            getOption("miraprot.pca_restore_render_timeout", 30)
          ))[1]
          if (!is.finite(timeout_seconds) || timeout_seconds < 0) timeout_seconds <- 30
          if (isTRUE(rv$pca_restore_rebuild_expected) && is.null(jobs$render)) {
            # Bounded independently from the finalizer: a cache hit supplies
            # inputs, but only the renderer can settle this expectation.
            jobs$render <- pca_register_job("render settlement", "render", timeout = timeout_seconds + 1)
          }
          pca_put_jobs(restore_context$session_generation, restore_context$pca_generation, jobs)
          report <- isolate({
            reports <- rv$restore_reports
            if (!is.list(reports)) reports <- list()
            reports$PCA %||% list()
          })
          report$restore_generation <- restore_generation
          report$session_restore_generation <- restore_context$session_generation
          report$render_job_id <- jobs$render
          report$rebuild_requested <- isTRUE(rv$pca_restore_rebuild_expected)
          report$plot_recreated <- FALSE
          report$render_completed <- FALSE
          report$render_failed <- FALSE
          report$render_timed_out <- FALSE
          report$render_status <- if (isTRUE(report$rebuild_requested)) "rebuild_requested" else "not_requested"
          record_restore_report("PCA", report)
          if (isTRUE(report$rebuild_requested) && requireNamespace("later", quietly = TRUE)) {
            # `later` retains this callback after the restore call returns. Keep
            # only immutable scalar diagnostics in its closure; the callback
            # must never use a captured reactive context or repair render state.
            timeout_session_generation <- restore_context$session_generation[1]
            timeout_pca_generation <- restore_context$pca_generation[1]
            timeout_render_job <- jobs$render
            later::later(function() {
              tryCatch({
                current <- isolate({
                  reports <- rv$restore_reports
                  if (!is.list(reports)) reports <- list()
                  list(
                    session_generation = rv$session_restore_generation %||% NA_integer_,
                    pca_generation = pca_state$restore_generation(),
                    report = reports$PCA %||% list()
                  )
                })
                if (!identical(current$session_generation, timeout_session_generation) ||
                    !identical(current$pca_generation, timeout_pca_generation)) return(invisible(NULL))
                current_report <- current$report
                if (!identical(current_report$restore_generation, timeout_pca_generation)) return(invisible(NULL))
                if (isTRUE(current_report$render_completed) ||
                    isTRUE(current_report$render_failed)) return(invisible(NULL))
                if (!isTRUE(current_report$rebuild_requested)) return(invisible(NULL))

                current_report$render_timed_out <- TRUE
                current_report$render_status <- "render_timed_out"
                record_restore_report("PCA", current_report)
                pca_resolve_job(timeout_render_job, "timeout", "PCA render expectation timed out")
                debug_log(sprintf(
                  "[PCA] restore render timed out (generation=%s, timeout=%ss)",
                  timeout_pca_generation, timeout_seconds
                ), 1)
              }, error = function(e) {
                debug_log(sprintf(
                  "[PCA] restore timeout diagnostic failed (generation=%s): %s",
                  timeout_pca_generation, conditionMessage(e)
                ), 1)
              })
            }, delay = timeout_seconds)
          }
          debug_log(sprintf(
            "[PCA] fresh plot rebuild requested after phased state restore (expected=%s)",
            as.character(isTRUE(rv$pca_restore_rebuild_expected))
          ), 1)
          return()
        }
        if (!is.null(phase) && !identical(phase, "full_module_state")) return()
        normalize_compact_result <- function(result) {
          if (!is.list(result)) return(result)
          if (is.null(result$var_explained) && !is.null(result$explained_variance)) {
            result$var_explained <- result$explained_variance
          }
          if (is.null(result$cumvar_explained) && !is.null(result$cumulative_explained_variance)) {
            result$cumvar_explained <- result$cumulative_explained_variance
          }
          if (is.list(result$selected_components)) {
            result$x_axis <- result$x_axis %||% result$selected_components$x
            result$y_axis <- result$y_axis %||% result$selected_components$y
          }
          result
        }
        if (is.null(state$plot_ui_inputs) && is.list(state$ui_inputs)) {
          state$plot_ui_inputs <- state$ui_inputs$values %||% state$ui_inputs
        }
        if (is.null(state$analysis_results) && is.list(state$analysis_result)) {
          state$analysis_results <- state$analysis_result
        }
        label_state <- if (is.list(state$plot_request)) state$plot_request$label_state else NULL
        normalized_labels <- normalize_pca_label_restore_state(
          label_state = label_state,
          legacy_labels = if (is.list(state$plot_request)) state$plot_request$labels else NULL,
          analysis_results = state$analysis_results,
          plot_ui_inputs = state$plot_ui_inputs,
          compatibility = state
        )
        state$analysis_results <- normalize_compact_result(state$analysis_results)
        state$sample_pca_results <- normalize_compact_result(state$sample_pca_results)
        state$protein_pca_results <- normalize_compact_result(state$protein_pca_results)
        state$sample_umap_results <- normalize_compact_result(state$sample_umap_results)
        state$protein_umap_results <- normalize_compact_result(state$protein_umap_results)
        if (is.list(state$analysis_results) && is.null(state$analysis_results$coordinates)) {
          state$analysis_results <- NULL
        }
        coordinates_available <- is.list(state$analysis_results) && !is.null(state$analysis_results$coordinates)
        had_plot_on_save <- pca_saved_plot_intent(state)
        restore_cache_resolved <- isTRUE(state$restore_cache_resolved)
        restore_cache_mode <- as.character(state$restore_cache_resolution_mode %||% "none")[1]
        cache_key <- as.character(state$plot_data_cache_ref %||% if (is.list(state$plot_ui_inputs)) canonical_plot_key(state$plot_ui_inputs$comparison_target) else NA_character_)[1]
        cache_hit <- is.list(state$restore_plot_data_cache) &&
          inherits(state$restore_plot_data_cache$data_mod, "data.frame") &&
          inherits(state$restore_plot_data_cache$data_def, "data.frame")
        cache_hit_reason <- if (isTRUE(cache_hit)) "cache_restored_module_ref" else "cache_miss_fallback_live"
        if (!isTRUE(cache_hit) &&
            is.list(state$restore_plot_data_cache_by_title) &&
            is.list(state$plot_ui_inputs)) {
          key <- canonical_plot_key(state$plot_ui_inputs$comparison_target)
          cand <- state$restore_plot_data_cache_by_title[[key]]
          if (!is.list(cand)) {
            legacy_key <- legacy_plot_key(state$plot_ui_inputs$comparison_target)
            cand <- state$restore_plot_data_cache_by_title[[legacy_key]]
          }
          if (is.list(cand)) { state$restore_plot_data_cache <- cand; cache_hit <- TRUE; cache_key <- key; cache_hit_reason <- "cache_restored_by_title" }
        }
        pca_state$restore_plot_data_cache(if (is.list(state$restore_plot_data_cache)) state$restore_plot_data_cache else NULL)
        if (isTRUE(cache_hit)) {
          restore_cache <- state$restore_plot_data_cache
          attach_restore_cache_to_results <- function(results_obj) {
            if (!is.list(results_obj)) return(results_obj)
            if (is.null(results_obj$selected_data_type) && is.list(state$plot_ui_inputs)) {
              results_obj$selected_data_type <- state$plot_ui_inputs$custom_col_sel_pca
            }
            if ((is.null(results_obj$selected_samples) || length(results_obj$selected_samples) == 0) &&
                is.list(state$plot_ui_inputs)) {
              results_obj$selected_samples <- state$plot_ui_inputs$select_samples_pca
            }
            pca_attach_cached_restore_metadata(results_obj, restore_cache)
          }
          state$analysis_results <- attach_restore_cache_to_results(state$analysis_results)
          state$sample_pca_results <- attach_restore_cache_to_results(state$sample_pca_results)
          state$protein_pca_results <- attach_restore_cache_to_results(state$protein_pca_results)
          state$sample_umap_results <- attach_restore_cache_to_results(state$sample_umap_results)
          state$protein_umap_results <- attach_restore_cache_to_results(state$protein_umap_results)
        }
        # Raise the restore guard BEFORE any reactive writes so the
        # comparison_target observer and renderPlot skip regeneration
        # while the restored plots are authoritative.
        pca_state$restore_in_progress(TRUE)
        pca_state$ordinary_ui_restore_complete(FALSE)
        restore_generation <- isolate(pca_state$restore_generation()) + 1L
        pca_state$restore_generation(restore_generation)
        hazardous_restore_ids <- c(
          "comparison_target", "GeneIdentifierColumn_pca", "analysis_method",
          "custom_col_sel_pca", "select_samples_pca",
          "masterLabelColor_pca", "masterDotColor_pca", "masterCustomDot_pca",
          "masterLabelColor_samples", "masterDotColor_samples",
          "masterCustomDot_samples"
        )
        expected_echoes <- state$plot_ui_inputs[
          intersect(hazardous_restore_ids, names(state$plot_ui_inputs %||% list()))
        ]
        pca_state$expected_restore_input_echoes(expected_echoes)
        # The compact result owns the identifier that generated the saved plot;
        # use the captured dropdown only for backward-compatible states that do
        # not carry that authoritative field.
        restored_identifier <- state$analysis_results$identifier_col %||%
          state$plot_ui_inputs$GeneIdentifierColumn_pca
        if (length(restored_identifier) > 0L) {
          restored_identifier <- restored_identifier[[1]]
        } else {
          restored_identifier <- NULL
        }
        pca_state$restored_identifier_column(restored_identifier)
        if (!is.null(state$sample_pca_results))   sample_pca_results(state$sample_pca_results)
        if (!is.null(state$protein_pca_results))   protein_pca_results(state$protein_pca_results)
        if (!is.null(state$sample_umap_results))   sample_umap_results(state$sample_umap_results)
        if (!is.null(state$protein_umap_results))  protein_umap_results(state$protein_umap_results)
        if (!is.null(state$selected_data_pca))      selected_data_pca(state$selected_data_pca)
        if (!is.null(state$selected_protein_vector_pca)) selected_protein_vector_pca(state$selected_protein_vector_pca)
        # Label state is replaced, rather than merged, while the restore guard
        # is active. This makes empty/disabled snapshots authoritative.
        if (identical(normalized_labels$mode, "samples")) {
          selected_items_vector_pca(character())
          item_label_settings_pca(normalized_labels$item_settings)
          labeled_proteins(character())
          sample_labeling_active_pca(normalized_labels$labeling_active)
          sample_label_settings_pca(normalized_labels$sample_settings)
        } else {
          sample_labeling_active_pca(FALSE)
          sample_label_settings_pca(list())
          selected_items_vector_pca(normalized_labels$selected_items)
          item_label_settings_pca(normalized_labels$item_settings)
          labeled_proteins(normalized_labels$selected_items)
        }
        debug_log(sprintf(
          "[PCA restore] labeling staged (selected_protein_count=%d, per_item_settings_count=%d)",
          length(normalized_labels$selected_items %||% character(0)),
          length(normalized_labels$item_settings %||% list())
        ), 1)
        pca_state$pending_label_ui_state(NULL)
        pca_state$label_restore_stage(list(
          generation = restore_generation,
          stage = "staged"
        ))
        if (isTRUE(state$plots_ready) && isTRUE(coordinates_available)) plots_ready(TRUE)
        if (!is.null(state$analysis_results))      analysis_results(state$analysis_results)
        # Stage captured UI inputs for the session_restore_trigger observer.
        if (is.null(state$plot_ui_inputs) || !is.list(state$plot_ui_inputs)) state$plot_ui_inputs <- list()
        # Static label controls are ordinary widgets. Dynamic per-protein rows
        # are not staged as inputs: renderUI initializes them directly from the
        # authoritative item_label_settings_pca reactive value.
        state$plot_ui_inputs <- utils::modifyList(
          state$plot_ui_inputs,
          c(normalized_labels$general_controls,
            normalized_labels$geometry_controls,
            list(searchGene_pca = normalized_labels$selection$searchGene_pca %||% "")),
          keep.null = TRUE
        )
        if (length(state$plot_ui_inputs) > 0L) {
          pca_state$pending_ui_inputs(state$plot_ui_inputs)
          # Track expected restored input values so the comparison_target
          # observer can distinguish restore echoes from user-initiated changes.
          restored_target <- state$plot_ui_inputs$comparison_target %||%
            state$analysis_results$comparison_target
          if (is.character(restored_target) && length(restored_target) > 0) {
            restored_target <- restored_target[[1]]
          }
          pca_state$restored_comparison_target(restored_target)
        }
        report <- list(
          cache_key = cache_key,
          cache_hit = isTRUE(cache_hit),
          data_source = if (isTRUE(cache_hit)) "cache" else "live",
          reason = if (isTRUE(cache_hit)) cache_hit_reason else "cache_miss_fallback_live",
          restore_generation = restore_generation,
          restore_state_applied = TRUE,
          rebuild_requested = FALSE,
          plot_recreated = FALSE,
          render_completed = FALSE,
          render_failed = FALSE,
          render_timed_out = FALSE,
          render_status = "restore_state_applied"
        )
        if (isTRUE(had_plot_on_save) && !isTRUE(cache_hit)) {
          showNotification("PCA restored using current dataset (cached plot data unavailable).", type = "warning", duration = 6)
        }
        # Legacy callers perform the complete restore in one invocation.  In a
        # phased restore, full_module_plots owns this request so the state phase
        # cannot accidentally initiate an early or duplicate rebuild.
        rv$pca_restore_rebuild_expected <- if (is.null(phase)) {
          isTRUE(had_plot_on_save) && isTRUE(coordinates_available)
        } else {
          FALSE
        }
        rv$pca_restore_selected_cache_key <- cache_key
        record_restore_report("PCA", report)
        debug_log(sprintf("[PCA] session state restored via set_session_state (selected_cache_key=%s, cache_hit=%s)",
                          as.character(cache_key), as.character(isTRUE(cache_hit))), 1)
      }
    ))
  })
}
