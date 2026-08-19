# ==============================================================================
# dotplot_reactive_state.R - Dotplot reactive state factory
#
# Purpose: Centralizes creation of all shared Dotplot reactives used across
# the main module and Dotplot server submodules.
#
# Structure:
#   - Data reactives: Source input data and metadata from shared app state
#   - Core state: Plot state, thresholds, regions, selection, and labeling stores
#   - Range and update state: Plot update triggers and user range tracking
#
# Dependencies: shiny
# Called by: modDotPlotServer()
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_reactive_state
# Purpose: Builds and initializes all shared Dotplot reactives in one place.
# Structure:
#   - Section 1: Create data reactives for expression data and metadata.
#   - Section 2: Create plot, threshold, region, selection, and labeling state.
#   - Section 3: Create update triggers and range-management reactives.
# Parameters:
#   - rv: [reactivevalues] - Shared application reactive values.
#   - dotplot_debug_log: [function] - Dotplot debug logger from parent module.
# Returns: Named list containing shared Dotplot reactives.
# ------------------------------------------------------------------------------
dotplot_init_reactive_state <- function(rv, dotplot_debug_log) {
  dotplot_state <- reactiveValues(
    session_schema_version = DOTPLOT_SESSION_SCHEMA_VERSION,
    current_plot = NULL,
    axis_config = list(x_col = NULL, y_col = NULL, x_transform = "raw", y_transform = "raw"),
    thresholds = list(),
    color_rules = list(list(id = "default", rule = "all", color = "#E0E0E0", priority = 0, label = "Default")),
    plot_ready = FALSE,
    selected_points = character(),
    last_update = Sys.time(),
    pending_ma_region_styles = NULL,
    pending_volcano_region_styles = NULL,
    restore_in_progress = FALSE,
    restore_plot_data_cache = NULL,
    plot_from_restore_cache = FALSE,
    restore_cache_resolved = FALSE,
    restore_live_fallback_available = FALSE,
    restore_rebuild_requested = FALSE,
    restore_notification_emitted = FALSE,
    plot_data_cache_ref = NULL,
    plot_request = NULL,
    plot_creation_cache = NULL,
    plot_ui_cache = NULL,
    source_data_signature = NULL,
    programmatic_range_update_pending = character(0),
    preset_update_in_progress = FALSE,
    preset_update_generation = 0L
  )

  compute_data_signature <- function(data, data_def) {
    if (!is.data.frame(data) || !is.data.frame(data_def)) return(NA_character_)
    paste0(nrow(data), "x", ncol(data), "::", nrow(data_def), "x", ncol(data_def), "::",
           paste(colnames(data), collapse = "|"), "::", paste(colnames(data_def), collapse = "|"))
  }

  last_data_source_log <- new.env(parent = emptyenv())
  log_data_source_transition <- function(context, source_key, message, level = 2) {
    previous_source_key <- last_data_source_log[[context]]
    if (!identical(previous_source_key, source_key)) {
      dotplot_debug_log(message, level)
      last_data_source_log[[context]] <- source_key
    }
  }

  data_in <- reactive({
    if (isTRUE(dotplot_state$plot_from_restore_cache) &&
        is.list(dotplot_state$restore_plot_data_cache) &&
        is.data.frame(dotplot_state$restore_plot_data_cache$data_mod)) {
      log_data_source_transition(
        "data_mod",
        "restore_plot_data_cache",
        "Using restored cached data_mod while restored dotplot is authoritative",
        1
      )
      return(dotplot_state$restore_plot_data_cache$data_mod)
    }
    if (isTRUE(dotplot_state$plot_ready) &&
        is.list(dotplot_state$plot_creation_cache) &&
        is.data.frame(rv$data_mod) && is.data.frame(rv$data_def)) {
      current_sig <- compute_data_signature(rv$data_mod, rv$data_def)
      source_sig <- dotplot_state$source_data_signature %||% NA_character_
      if (is.character(source_sig) && nzchar(source_sig) && !identical(source_sig, current_sig)) {
        log_data_source_transition(
          "data_mod",
          "plot_creation_cache",
          "Using cached data_mod because active dataset differs from dotplot source dataset",
          1
        )
        return(dotplot_state$plot_creation_cache$data_mod)
      }
    }
    if (!is.null(rv$data_mod)) {
      log_data_source_transition("data_mod", "rv$data_mod", "Accessing data_mod from rv", 2)
      return(rv$data_mod)
    }
    dotplot_debug_log("No data available", 1)
    NULL
  })

  data_def_in <- reactive({
    if (isTRUE(dotplot_state$plot_from_restore_cache) &&
        is.list(dotplot_state$restore_plot_data_cache) &&
        is.data.frame(dotplot_state$restore_plot_data_cache$data_def)) {
      log_data_source_transition(
        "data_def",
        "restore_plot_data_cache",
        "Using restored cached data_def while restored dotplot is authoritative",
        1
      )
      return(dotplot_state$restore_plot_data_cache$data_def)
    }
    if (isTRUE(dotplot_state$plot_ready) &&
        is.list(dotplot_state$plot_creation_cache) &&
        is.data.frame(rv$data_mod) && is.data.frame(rv$data_def)) {
      current_sig <- compute_data_signature(rv$data_mod, rv$data_def)
      source_sig <- dotplot_state$source_data_signature %||% NA_character_
      if (is.character(source_sig) && nzchar(source_sig) && !identical(source_sig, current_sig)) {
        log_data_source_transition(
          "data_def",
          "plot_creation_cache",
          "Using cached data_def because active dataset differs from dotplot source dataset",
          1
        )
        return(dotplot_state$plot_creation_cache$data_def)
      }
    }
    if (!is.null(rv$data_def)) {
      log_data_source_transition("data_def", "rv$data_def", "Accessing data_def from rv", 2)
      return(rv$data_def)
    }
    log_data_source_transition("data_def", "none", "No metadata available", 1)
    NULL
  })

  plot_update_trigger <- reactiveVal(0)
  range_render_trigger <- reactiveVal(0)


  is_active_dataset_mismatch <- function() {
    if (!isTRUE(dotplot_state$plot_ready)) return(FALSE)
    source_sig <- dotplot_state$source_data_signature %||% NA_character_
    if (!is.character(source_sig) || !nzchar(source_sig)) return(FALSE)
    current_sig <- compute_data_signature(rv$data_mod, rv$data_def)
    is.character(current_sig) && nzchar(current_sig) && !identical(source_sig, current_sig)
  }

  # dotplot_state declared above so data_in/data_def_in can consult source/cache

  region_configs <- reactiveVal(list())
  selected_region <- reactiveVal(NULL)

  region_structure <- reactive({
    thresholds <- dotplot_state$thresholds
    v_thresholds <- sapply(thresholds, function(t) if (t$type == "vertical") t$value else NULL)
    v_thresholds <- sort(as.numeric(v_thresholds[!sapply(v_thresholds, is.null)]))
    h_thresholds <- sapply(thresholds, function(t) if (t$type == "horizontal") t$value else NULL)
    h_thresholds <- sort(as.numeric(h_thresholds[!sapply(h_thresholds, is.null)]))

    if (length(v_thresholds) > 2) v_thresholds <- v_thresholds[1:2]
    if (length(h_thresholds) > 2) h_thresholds <- h_thresholds[1:2]

    n_v_regions <- length(v_thresholds) + 1
    n_h_regions <- length(h_thresholds) + 1

    list(
      v_thresholds = v_thresholds,
      h_thresholds = h_thresholds,
      n_v_regions = n_v_regions,
      n_h_regions = n_h_regions,
      total_regions = n_v_regions * n_h_regions
    )
  })

  selected_data_dot <- reactiveVal(NULL)
  selected_protein_vector_dot <- reactiveVal(character())
  selected_points_interactive_dot <- reactiveVal(data.frame())

  protein_label_settings_dot <- reactiveVal(data.frame(
    protein_id = character(),
    label_color = character(),
    dot_color = character(),
    use_custom_dot_color = logical(),
    stringsAsFactors = FALSE
  ))

  dot_protein_labels <- reactiveVal(list())
  dot_protein_labels(list())

  dot_plot_parameters <- reactiveVal(list(
    x_col = NULL,
    y_col = NULL,
    x_transform = NULL,
    y_transform = NULL,
    axis_labels = NULL,
    plot_timestamp = NULL
  ))

  user_range_settings <- reactiveVal(list(
    x_range_user_set = FALSE,
    y_range_user_set = FALSE,
    last_auto_update = NULL
  ))

  # Holds UI widget values captured at session-save time.  On restore, a
  # dedicated sync step reads this and pushes the values back into the
  # live input bindings via update*Input() calls once the UI has been
  # rendered (session$onFlushed).  Cleared (set to NULL) after the sync
  # has run so normal interaction is not hijacked.
  pending_ui_inputs <- reactiveVal(NULL)

  list(
    data_in = data_in,
    data_def_in = data_def_in,
    plot_update_trigger = plot_update_trigger,
    range_render_trigger = range_render_trigger,
    dotplot_state = dotplot_state,
    region_configs = region_configs,
    selected_region = selected_region,
    region_structure = region_structure,
    selected_data_dot = selected_data_dot,
    selected_protein_vector_dot = selected_protein_vector_dot,
    selected_points_interactive_dot = selected_points_interactive_dot,
    protein_label_settings_dot = protein_label_settings_dot,
    dot_protein_labels = dot_protein_labels,
    dot_plot_parameters = dot_plot_parameters,
    user_range_settings = user_range_settings,
    pending_ui_inputs = pending_ui_inputs,
    is_active_dataset_mismatch = is_active_dataset_mismatch
  )
}
