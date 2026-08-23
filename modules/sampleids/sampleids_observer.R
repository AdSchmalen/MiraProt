# ==============================================================================
# File: modules/sampleids/sampleids_observer.R
#
# Purpose:
#   Contains all observe() and observeEvent() blocks, renderUI/renderPlot/
#   renderPlotly outputs, and the download handler for the Sample IDs module.
#   This file centralizes all reactive side-effects so that the orchestrator
#   file stays lean and focused on wiring.
#
# Architectural Role:
#   Observer and output layer of the Sample IDs module. Called from
#   modSampleIDsServer() via register_sampleids_observers() after state is
#   initialized. All observers run inside the moduleServer() closure of the
#   orchestrator. Pure plotting logic is delegated to build_sampleids_char_plot()
#   and build_sampleids_num_plot() from sampleids_logic.R. Reactive state comes
#   from create_sampleids_state() via the `state` list argument.
#
# Structure:
#   1. register_sampleids_observers() - Registration function containing:
#      a. Column detection observer  - detects char/numeric indices, updates
#                                      FileSample and data type dropdowns.
#      b. Character level observer   - updates Sort_SampleIDTab choices when
#                                      FileSample or data type selection changes.
#      c. Dynamic plot UI output     - switches between static and interactive plot.
#      d. Static plot output         - renders ggplot2 object from state.
#      e. Plotly output              - renders plotly object from state.
#      f. Refresh plot observer      - validates inputs, calls logic, updates state.
#      g. Download handler           - exports ggplot2 plot to file.
#      h. Add-to-grid observer       - sends current plot to the Grid module.
#
# Notes for future developers:
#   - Observer registration order matters: the refresh observer depends on
#     both data_def and data_mod being available.
#   - debug_log is passed in from modSampleIDsServer; do not define a new one.
#   - All pure computation is delegated to build_sampleids_char_plot() and
#     build_sampleids_num_plot(). Keep observers thin: validate inputs, call
#     logic, update state, notify user.
#   - The add-to-grid observer reads modEnv$add_to_grid from the global module
#     environment; this is the established grid integration pattern.
#   - No session$onSessionEnded cleanup is required: the module holds only
#     session-local reactive values that are garbage-collected when the session
#     ends.
# ==============================================================================


#' Register all observers and outputs for the Sample IDs module.
#'
#' @param input     Shiny input object from moduleServer closure.
#' @param output    Shiny output object from moduleServer closure.
#' @param session   Shiny session object.
#' @param ns        Namespace function for this module.
#' @param state     Named list from create_sampleids_state().
#' @param rv        Global reactive values object (rv$data_def, rv$data_mod,
#'                  rv$height_px, rv$px_ratio).
#' @param debug_log Logging function with signature (message, level).
register_sampleids_observers <- function(input, output, session, ns,
                                         state, rv, debug_log) {
  restore_poll_active   <- reactiveVal(FALSE)
  restore_poll_attempt  <- reactiveVal(0L)
  restore_poll_captured <- reactiveVal(NULL)
  restore_poll_generation <- reactiveVal(NULL)
  restore_poll_job <- reactiveVal(NULL)

  restore_context_active <- function() {
    isTRUE(rv$session_restoring) ||
      isTRUE(restore_poll_active()) ||
      !is.null(state$pending_ui_inputs())
  }

  release_cached_restore_plot <- function() {
    restore_poll_active(FALSE)
    restore_poll_captured(NULL)
    state$pending_ui_inputs(NULL)
    state$had_plot_on_save(FALSE)
    state$restore_plot_data_cache(NULL)
    state$plot_from_restore_cache(FALSE)
  }

  settle_restore_poll <- function(outcome, error = NULL) {
    job_id <- isolate(restore_poll_job())
    resolver <- session$userData$resolve_restore_job
    restore_poll_job(NULL)
    if (!is.null(job_id) && is.function(resolver)) {
      tryCatch(resolver(job_id, outcome, error), error = function(e) FALSE)
    }
    invisible(TRUE)
  }

  has_valid_restore_plot_data_cache <- function(cache) {
    is.list(cache) &&
      inherits(cache$data_mod, "data.frame") &&
      inherits(cache$data_def, "data.frame")
  }

  data_pair_signature <- function(data_mod, data_def) {
    if (!inherits(data_mod, "data.frame") || !inherits(data_def, "data.frame")) return(NA_character_)
    paste0(nrow(data_mod), "x", ncol(data_mod), "::",
           nrow(data_def), "x", ncol(data_def), "::",
           paste(colnames(data_mod), collapse = "|"), "::",
           paste(colnames(data_def), collapse = "|"))
  }

  active_data_differs_from_plot_cache <- function() {
    cache <- isolate(state$plot_creation_cache())
    if (!has_valid_restore_plot_data_cache(cache) || !inherits(rv$data_mod, "data.frame") ||
        !inherits(rv$data_def, "data.frame")) return(FALSE)
    !identical(
      data_pair_signature(cache$data_mod, cache$data_def),
      data_pair_signature(rv$data_mod, rv$data_def)
    )
  }

  regenerate_sampleids_plot <- function(ui_override = NULL, force_cached = FALSE) {
    cache <- isolate(state$restore_plot_data_cache())
    has_cached_pair <- has_valid_restore_plot_data_cache(cache)
    # The restore cache is a manual-release replay source: restored plots stay
    # bound to their saved data until the user clicks Create Plot, which
    # clears the cache and rebuilds from current rv$data_mod/rv$data_def.
    use_restore_cache <- isTRUE(has_cached_pair) &&
      (isTRUE(force_cached) || isTRUE(restore_context_active()))
    data_mod_active <- if (isTRUE(use_restore_cache)) cache$data_mod else rv$data_mod
    data_def_active <- if (isTRUE(use_restore_cache)) cache$data_def else rv$data_def
    if (is.null(data_mod_active) || is.null(data_def_active)) return(FALSE)

    ui_value <- function(id, default = NULL) {
      if (is.list(ui_override) && !is.null(ui_override[[id]])) return(ui_override[[id]])
      v <- isolate(input[[id]])
      if (is.null(v)) default else v
    }

    file_sample <- as.character(ui_value("FileSample_SampleIDTab"))[1]
    data_type <- as.character(ui_value("data_SampleIDTab"))[1]
    if (!is.character(file_sample) || !nzchar(file_sample) ||
        !is.character(data_type) || !nzchar(data_type)) return(FALSE)

    debug_log("SampleIDs: starting plot generation", 1)

    data     <- data_mod_active
    data_def <- data_def_active

    char_indices_active <- which(sapply(data, is.character))
    num_indices_active  <- which(sapply(data, is.numeric))

    foundin_pattern <- if (file_sample == "Found in Sample") {
      "^Found in Sample$"
    } else if (file_sample == "Found in File") {
      "^Found in File$"
    } else {
      showNotification("Invalid data source selection.", type = "error")
      return(FALSE)
    }

    relevant_indices <- if (data_type == "Character strings") {
      char_indices_active
    } else if (data_type == "Numeric values") {
      num_indices_active
    } else {
      showNotification("Invalid data type selection.", type = "error")
      return(FALSE)
    }

    foundin_indices <- which(grepl(foundin_pattern, data_def$Content))
    col_indices     <- intersect(foundin_indices, relevant_indices)

    if (length(foundin_indices) == 0 || length(relevant_indices) == 0 ||
        length(col_indices) == 0) {
      showNotification("No valid data available to plot.", type = "error")
      return(FALSE)
    }
    col_indices <- col_indices[col_indices >= 1L & col_indices <= ncol(data) & col_indices <= nrow(data_def)]
    if (length(col_indices) == 0L) {
      debug_log("SampleIDs: selected content indices exceed active data dimensions; skipping rebuild", 1)
      showNotification("No valid data available to plot.", type = "error")
      return(FALSE)
    }

    df <- data[, col_indices, drop = FALSE]
    if (ncol(df) == 0) {
      showNotification("No valid data available to plot.", type = "error")
      return(FALSE)
    }

    color_input       <- tolower(as.character(ui_value("col_SampleIDTab"))[1])
    col_reverse       <- isTRUE(ui_value("col_reverse_SampleIDTab"))
    theme_name        <- ui_value("ThemeSelect_SampleIDTab")
    title             <- if (isTRUE(ui_value("hideTitle_SampleIDTab"))) NULL else ui_value("plotTitle_SampleIDTab")
    label_type        <- ui_value("label_SampleIDTab")
    title_size        <- as.numeric(ui_value("TitleSize_SampleIDTab"))
    axis_title_size   <- as.numeric(ui_value("AxisTitleSize_SampleIDTab"))
    tick_size         <- as.numeric(ui_value("tickSize_SampleIDTab"))
    legend_title_size <- as.numeric(ui_value("LegendTitleSize_SampleIDTab"))
    legend_text_size  <- as.numeric(ui_value("LegendTextSize_SampleIDTab"))
    legend_position   <- ui_value("sampleIDs_legend_position")

    result <- if (data_type == "Character strings") {
      if (!is.data.frame(df)) {
        showNotification("Selected data is not in the correct format.", type = "error")
        return(FALSE)
      }

      build_sampleids_char_plot(
        df              = df,
        col_indices     = col_indices,
        data_def        = data_def,
        sort_values     = ui_value("Sort_SampleIDTab"),
        abs_rel         = ui_value("AbsRel_SampleIDTab"),
        label_type      = label_type,
        color_input     = color_input,
        col_reverse     = col_reverse,
        theme_name      = theme_name,
        title           = title,
        title_size      = title_size,
        axis_title_size = axis_title_size,
        tick_size       = tick_size,
        legend_title_size = legend_title_size,
        legend_text_size  = legend_text_size,
        legend_position = legend_position,
        debug_log       = debug_log
      )

    } else {
      transformation_df <- data_def$Transformation[col_indices]
      data_rt <- retransform_data_global(data, col_indices, transformation_df)
      df      <- data_rt[, col_indices, drop = FALSE]

      transform_sel <- ui_value("Transform_SampleIDTab")
      if (!is.null(transform_sel) && !is.na(transform_sel)) {
        if (transform_sel == "log2") {
          df <- log2(df)
        } else if (transform_sel == "log10") {
          df <- log10(df)
        } else if (transform_sel == "-log10") {
          df <- -log10(df)
        }
      }

      if (!is.data.frame(df)) {
        showNotification("Selected data is not in the correct format.", type = "error")
        return(FALSE)
      }

      build_sampleids_num_plot(
        df              = df,
        col_indices     = col_indices,
        data_def        = data_def,
        plot_type       = ui_value("NumericPlotType_SampleIDTab"),
        label_type      = label_type,
        color_input     = color_input,
        col_reverse     = col_reverse,
        theme_name      = theme_name,
        title           = title,
        title_size      = title_size,
        axis_title_size = axis_title_size,
        tick_size       = tick_size,
        legend_title_size = legend_title_size,
        legend_text_size  = legend_text_size,
        legend_position = legend_position,
        debug_log       = debug_log
      )
    }

    if (is.null(result)) {
      showNotification("Invalid plot type selected.", type = "error")
      state$ggplot_object_SampleIDTab(NULL)
      state$plotly_object_SampleIDTab(NULL)
      return(FALSE)
    }

    state$ggplot_object_SampleIDTab(result$ggplot_object)
    state$plotly_object_SampleIDTab(result$plotly_object)
    state$plot_from_restore_cache(isTRUE(use_restore_cache))
    state$plot_creation_cache(list(
      data_mod = data_mod_active,
      data_def = data_def_active
    ))
    plot_ui_snapshot <- list(
      FileSample_SampleIDTab = file_sample,
      data_SampleIDTab = data_type,
      Sort_SampleIDTab = ui_value("Sort_SampleIDTab"),
      AbsRel_SampleIDTab = ui_value("AbsRel_SampleIDTab"),
      NumericPlotType_SampleIDTab = ui_value("NumericPlotType_SampleIDTab"),
      Transform_SampleIDTab = ui_value("Transform_SampleIDTab"),
      label_SampleIDTab = label_type,
      ThemeSelect_SampleIDTab = theme_name,
      col_SampleIDTab = ui_value("col_SampleIDTab"),
      col_reverse_SampleIDTab = col_reverse,
      plotTitle_SampleIDTab = ui_value("plotTitle_SampleIDTab"),
      hideTitle_SampleIDTab = isTRUE(ui_value("hideTitle_SampleIDTab")),
      TitleSize_SampleIDTab = title_size,
      AxisTitleSize_SampleIDTab = axis_title_size,
      tickSize_SampleIDTab = tick_size,
      LegendTitleSize_SampleIDTab = legend_title_size,
      LegendTextSize_SampleIDTab = legend_text_size,
      sampleIDs_legend_position = legend_position
    )
    # A restored plot remains bound to its saved UI/data snapshot until the
    # manual Create/Refresh action calls release_cached_restore_plot().  Do not
    # let subsequent default/live input echoes replace that saved snapshot.
    if (!isTRUE(isolate(state$plot_from_restore_cache())) || isTRUE(use_restore_cache)) {
      state$plot_ui_cache(plot_ui_snapshot)
      state$plot_request(build_sampleids_plot_request(plot_ui_snapshot))
    }
    cache_ref <- tryCatch({
      .build_plot_data_cache_id(data_mod = data_mod_active, data_def = data_def_active)
    }, error = function(e) NA_character_)
    state$plot_data_cache_ref(cache_ref)
    debug_log("SampleIDs: plot built and stored", 1)

    stack_info <- NULL

    # stacked bar info (nur numeric + stacked)
    if (data_type == "Numeric values" &&
        ui_value("NumericPlotType_SampleIDTab") == "stacked bar") {

      stack_info <- paste(colnames(df), collapse = ", ")
    }

    sort_val <- if (data_type == "Character strings") {
      paste(ui_value("Sort_SampleIDTab"), collapse = ", ")
    } else {
      NULL
    }

    absrel_val <- if (data_type == "Character strings") {
      paste(ui_value("AbsRel_SampleIDTab"), collapse = ", ")
    } else {
      NULL
    }

    transform_val <- if (data_type == "Numeric values") {
      as.character(ui_value("Transform_SampleIDTab"))
    } else {
      NULL
    }

    numeric_plot_type <- if (data_type == "Numeric values") {
      as.character(ui_value("NumericPlotType_SampleIDTab"))
    } else {
      NULL
    }

    debug_log(
      sprintf(
        paste0(
          "SampleID plot summary",
          " | Based on: %s",
          " | Data type: %s",
          " | Theme: %s",
          " | Label type: %s",
          " | Color palette: %s",
          " | Reverse colors: %s",
          " | Samples: %s",
          "%s%s%s%s%s"
        ),

        as.character(file_sample),
        as.character(data_type),
        as.character(theme_name),
        as.character(label_type),
        as.character(color_input),
        as.character(col_reverse),
        length(col_indices),

        # character-specific
        if (!is.null(sort_val))
          paste0(" | Sort: ", sort_val, " | Scale: ", absrel_val)
        else "",

        # numeric-specific
        if (!is.null(transform_val))
          paste0(" | Transform: ", transform_val,
                 " | Numeric plot type: ", numeric_plot_type)
        else "",

        # stacked bars
        if (!is.null(stack_info))
          paste0(" | Stacks: ", stack_info)
        else "",

        "", ""
      ),
      level = 0
    )

    TRUE
  }

  apply_sampleids_restored_inputs <- function(captured) {
    if (is.null(captured) || !is.list(captured)) return(invisible(FALSE))

    # Static-choice inputs that can be pushed directly.
    # Dynamic-choice inputs (FileSample, data, Sort) are handled in
    # the column-detection and character-level observers.
    static_ids <- c(
      "checkbox_interactive_SampleIDTab",
      "AbsRel_SampleIDTab", "NumericPlotType_SampleIDTab", "Transform_SampleIDTab",
      "label_SampleIDTab", "ThemeSelect_SampleIDTab", "col_SampleIDTab",
      "col_reverse_SampleIDTab", "plotTitle_SampleIDTab", "hideTitle_SampleIDTab",
      "TitleSize_SampleIDTab", "AxisTitleSize_SampleIDTab", "tickSize_SampleIDTab",
      "LegendTitleSize_SampleIDTab", "LegendTextSize_SampleIDTab",
      "sampleIDs_legend_position",
      "resolution_DPI_SampleIDTab", "plotWidthInch_SampleIDTab",
      "plotHeightInch_SampleIDTab", "downloadFormat_SampleIDTab"
    )

    numeric_input_ids <- c(
      "TitleSize_SampleIDTab", "AxisTitleSize_SampleIDTab",
      "tickSize_SampleIDTab", "LegendTitleSize_SampleIDTab",
      "LegendTextSize_SampleIDTab", "resolution_DPI_SampleIDTab",
      "plotWidthInch_SampleIDTab", "plotHeightInch_SampleIDTab"
    )

    for (id in static_ids) {
      val <- captured[[id]]
      if (is.null(val)) next
      if (id == "plotTitle_SampleIDTab") {
        updateTextInput(session, id, value = as.character(val)[1])
      } else if (is.logical(val)) {
        updateCheckboxInput(session, id, value = val)
      } else if (id %in% numeric_input_ids) {
        updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
      } else {
        updateSelectInput(session, id, selected = as.character(val))
      }
    }

    invisible(TRUE)
  }

  is_sampleids_restore_ready <- function(captured) {
    if (is.null(captured) || !is.list(captured)) return(FALSE)
    cache <- state$restore_plot_data_cache()
    data_mod_active <- if (is.list(cache) && inherits(cache$data_mod, "data.frame")) cache$data_mod else rv$data_mod
    data_def_active <- if (is.list(cache) && inherits(cache$data_def, "data.frame")) cache$data_def else rv$data_def
    if (is.null(data_mod_active) || is.null(data_def_active)) return(FALSE)

    same_single <- function(input_id) {
      expected <- captured[[input_id]]
      if (is.null(expected)) return(TRUE)
      current <- input[[input_id]]
      identical(as.character(current)[1], as.character(expected)[1])
    }

    # Dynamic selections must be echoed back from the browser before rebuild.
    if (!same_single("FileSample_SampleIDTab")) return(FALSE)
    if (!same_single("data_SampleIDTab")) return(FALSE)
    if (!same_single("plotTitle_SampleIDTab")) return(FALSE)
    if (!same_single("hideTitle_SampleIDTab")) return(FALSE)

    # For Character mode we also require Sort selection to be restored.
    expected_sort <- captured[["Sort_SampleIDTab"]]
    selected_type <- input$data_SampleIDTab
    if (identical(selected_type, "Character strings") && !is.null(expected_sort)) {
      cur <- sort(as.character(input$Sort_SampleIDTab %||% character()))
      exp <- sort(as.character(expected_sort))
      # Allow subset when some saved levels no longer exist in restored data.
      if (length(intersect(cur, exp)) == 0L && length(exp) > 0L) return(FALSE)
    }

    TRUE
  }

  # --------------------------------------------------------------------------
  # a. Column detection observer
  #    Runs whenever data_mod or data_def changes. Detects which columns are
  #    character vs. numeric and updates the FileSample and data-type dropdowns
  #    to reflect what is available in the current dataset.
  # --------------------------------------------------------------------------

  observe({
    data_mod_active <- rv$data_mod
    data_def_active <- rv$data_def
    req(data_mod_active, data_def_active)

    data     <- data_mod_active
    data_def <- data_def_active

    char_indices <- which(sapply(data, is.character))
    num_indices  <- which(sapply(data, is.numeric))

    state$character_indices(char_indices)
    state$numeric_indices(num_indices)

    found_in_sample_idx <- which(grepl("^Found in Sample$", data_def$Content))
    found_in_file_idx   <- which(grepl("^Found in File$",   data_def$Content))

    file_sample_choices <- character()
    if (length(found_in_sample_idx) > 0) {
      file_sample_choices <- c(file_sample_choices, "Found in Sample")
    }
    if (length(found_in_file_idx) > 0) {
      file_sample_choices <- c(file_sample_choices, "Found in File")
    }

    updateSelectInput(session, "FileSample_SampleIDTab", choices = file_sample_choices)

    sample_char_idx <- intersect(char_indices, found_in_sample_idx)
    sample_num_idx  <- intersect(num_indices,  found_in_sample_idx)
    file_char_idx   <- intersect(char_indices, found_in_file_idx)

    data_type_choices <- character()
    if (length(sample_char_idx) > 0 || length(file_char_idx) > 0) {
      data_type_choices <- c(data_type_choices, "Character strings")
    }
    if (length(sample_num_idx) > 0) {
      data_type_choices <- c(data_type_choices, "Numeric values")
    }

    updateSelectInput(session, "data_SampleIDTab",
                      choices  = data_type_choices,
                      selected = data_type_choices[1])

    # If a session restore is in progress, apply the saved selections for the
    # dynamic-choice inputs whose choices were just repopulated above.  The
    # reactive read of pending_ui_inputs() ensures this observer re-fires when
    # set_session_state() stages the captured inputs after the data has
    # already been restored (so the dynamic selections are not missed).
    pending <- state$pending_ui_inputs()
    if (!is.null(pending)) {
      if (!is.null(pending$FileSample_SampleIDTab) &&
          pending$FileSample_SampleIDTab %in% file_sample_choices) {
        updateSelectInput(session, "FileSample_SampleIDTab",
                          selected = pending$FileSample_SampleIDTab)
      }
      if (!is.null(pending$data_SampleIDTab) &&
          pending$data_SampleIDTab %in% data_type_choices) {
        updateSelectInput(session, "data_SampleIDTab",
                          selected = pending$data_SampleIDTab)
      }
    }
  })

  # --------------------------------------------------------------------------
  # b. Character level observer
  #    Updates Sort_SampleIDTab when either the FileSample or data type
  #    selection changes. Limits to 10 unique values; shows a notification if
  #    exceeded.
  # --------------------------------------------------------------------------

  observeEvent(list(input$FileSample_SampleIDTab, input$data_SampleIDTab, rv$data_mod, rv$data_def), {
    data_mod_active <- rv$data_mod
    data_def_active <- rv$data_def
    req(data_mod_active, data_def_active)

    data     <- data_mod_active
    data_def <- data_def_active

    selected_file_sample <- input$FileSample_SampleIDTab
    selected_data_type   <- input$data_SampleIDTab

    found_in_sample_idx <- which(grepl("^Found in Sample$", data_def$Content))
    found_in_file_idx   <- which(grepl("^Found in File$",   data_def$Content))
    char_indices        <- state$character_indices()

    sample_char_idx <- intersect(char_indices, found_in_sample_idx)
    file_char_idx   <- intersect(char_indices, found_in_file_idx)
    sample_char_idx <- sample_char_idx[sample_char_idx >= 1L & sample_char_idx <= ncol(data) & sample_char_idx <= nrow(data_def)]
    file_char_idx   <- file_char_idx[file_char_idx >= 1L & file_char_idx <= ncol(data) & file_char_idx <= nrow(data_def)]

    levels_data <- NULL

    if (!is.null(selected_data_type) && selected_data_type == "Character strings") {
      if (!is.null(selected_file_sample) && selected_file_sample == "Found in Sample") {
        levels_data <- unique(unlist(data[, sample_char_idx]))
      } else if (!is.null(selected_file_sample) && selected_file_sample == "Found in File") {
        levels_data <- unique(unlist(data[, file_char_idx]))
      }
    }

    if (!is.null(levels_data) && length(levels_data) > 0) {
      if (length(levels_data) > 10) {
        updateSelectInput(session, "Sort_SampleIDTab", choices = NULL)
        showNotification("Too many different character strings available.", type = "error")
      } else {
        # Default: select all available levels.
        sort_selected <- levels_data
        # If a session restore is pending, use the saved selection (intersected
        # with the available levels to drop any values no longer in the data).
        pending_all  <- state$pending_ui_inputs()
        pending_sort <- if (is.list(pending_all)) pending_all$Sort_SampleIDTab else NULL
        if (!is.null(pending_sort)) {
          valid_pending <- intersect(pending_sort, levels_data)
          if (length(valid_pending) > 0) sort_selected <- valid_pending
        }
        updateSelectInput(session, "Sort_SampleIDTab",
                          choices  = levels_data,
                          selected = sort_selected)
      }
    } else {
      updateSelectInput(session, "Sort_SampleIDTab", choices = NULL)
    }
  })

  # --------------------------------------------------------------------------
  # c. Dynamic plot UI output
  #    Switches between a plotlyOutput (interactive) and a plotOutput (static)
  #    depending on the checkbox state.
  # --------------------------------------------------------------------------

  output$UI_SampleIDTab <- renderUI({
    req(rv$height_px, rv$px_ratio)
    height_px <- paste0(rv$height_px * rv$px_ratio, "px")

    if (isTRUE(input$checkbox_interactive_SampleIDTab)) {
      plotlyOutput(ns("plotly_SampleIDTab"), height = height_px)
    } else {
      plotOutput(ns("plot_SampleIDTab"), height = height_px)
    }
  })

  # --------------------------------------------------------------------------
  # d. Static plot output
  # --------------------------------------------------------------------------

  output$plot_SampleIDTab <- renderPlot({
    req(state$ggplot_object_SampleIDTab())
    state$ggplot_object_SampleIDTab()
  })

  # --------------------------------------------------------------------------
  # e. Plotly output
  # --------------------------------------------------------------------------

  output$plotly_SampleIDTab <- renderPlotly({
    req(state$plotly_object_SampleIDTab())
    state$plotly_object_SampleIDTab()
  })

  # --------------------------------------------------------------------------
  # f. Refresh plot observer
  #    Validates inputs, determines column indices, delegates to the appropriate
  #    plot-building function, and stores the result in state.
  # --------------------------------------------------------------------------

  observeEvent(input$refresh_SampleIDTab, {
    release_cached_restore_plot()
    regenerate_sampleids_plot()
  })

  debounced_sampleids_plot_inputs <- debounce(reactive({
    list(
      FileSample_SampleIDTab = input$FileSample_SampleIDTab,
      data_SampleIDTab = input$data_SampleIDTab,
      Sort_SampleIDTab = input$Sort_SampleIDTab,
      AbsRel_SampleIDTab = input$AbsRel_SampleIDTab,
      NumericPlotType_SampleIDTab = input$NumericPlotType_SampleIDTab,
      Transform_SampleIDTab = input$Transform_SampleIDTab,
      label_SampleIDTab = input$label_SampleIDTab,
      ThemeSelect_SampleIDTab = input$ThemeSelect_SampleIDTab,
      col_SampleIDTab = input$col_SampleIDTab,
      col_reverse_SampleIDTab = input$col_reverse_SampleIDTab,
      plotTitle_SampleIDTab = input$plotTitle_SampleIDTab,
      hideTitle_SampleIDTab = input$hideTitle_SampleIDTab,
      TitleSize_SampleIDTab = input$TitleSize_SampleIDTab,
      AxisTitleSize_SampleIDTab = input$AxisTitleSize_SampleIDTab,
      tickSize_SampleIDTab = input$tickSize_SampleIDTab,
      LegendTitleSize_SampleIDTab = input$LegendTitleSize_SampleIDTab,
      LegendTextSize_SampleIDTab = input$LegendTextSize_SampleIDTab,
      sampleIDs_legend_position = input$sampleIDs_legend_position
    )
  }), 700)

  observeEvent(debounced_sampleids_plot_inputs(), {
    if (datawizard_restore_phase_active(rv) || isTRUE(restore_context_active()) || isTRUE(state$plot_from_restore_cache())) return()
    if (is.null(state$ggplot_object_SampleIDTab()) && is.null(state$plotly_object_SampleIDTab())) return()
    if (isTRUE(active_data_differs_from_plot_cache())) {
      debug_log("SampleIDs: input echo from different live data ignored; keeping plot creation cache", 1)
      return()
    }

    regenerate_sampleids_plot()
  }, ignoreInit = TRUE)

  observeEvent(list(rv$data_mod, rv$data_def), {
    if (datawizard_restore_phase_active(rv) || isTRUE(restore_context_active()) || isTRUE(state$plot_from_restore_cache())) return()
    if (is.null(state$ggplot_object_SampleIDTab()) && is.null(state$plotly_object_SampleIDTab())) return()

    # Existing Sample ID plots are data snapshots. Do not silently rebuild them
    # from a newly loaded/assigned Data Wizard dataset; the user can explicitly
    # click Create Plot to opt into the current live data.
    debug_log("SampleIDs: live data/metadata changed; keeping existing plot snapshot", 1)
    return()
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # g. Reset plot-options controls
  #    Restores only the Plot options wellPanel to its UI defaults. General
  #    options are intentionally left unchanged.
  # --------------------------------------------------------------------------

  observeEvent(input$resetButton_SampleIDTab, {
    tryCatch({
      debug_log("Resetting Sample IDs plot options to UI defaults", 1)

      updateSelectInput(session, "ThemeSelect_SampleIDTab", selected = "Classic")
      updateSelectInput(session, "col_SampleIDTab", selected = "Viridis")
      updateCheckboxInput(session, "col_reverse_SampleIDTab", value = TRUE)
      updateTextInput(session, "plotTitle_SampleIDTab", value = "Sample IDs")
      updateCheckboxInput(session, "hideTitle_SampleIDTab", value = TRUE)
      updateNumericInput(session, "TitleSize_SampleIDTab", value = 20)
      updateNumericInput(session, "AxisTitleSize_SampleIDTab", value = 20)
      updateNumericInput(session, "tickSize_SampleIDTab", value = 18)
      updateNumericInput(session, "LegendTitleSize_SampleIDTab", value = 20)
      updateNumericInput(session, "LegendTextSize_SampleIDTab", value = 18)
      updateSelectInput(session, "sampleIDs_legend_position", selected = "none")

      showNotification("Sample IDs plot options reset to defaults.", type = "message", duration = 3)
      debug_log("Sample IDs plot options reset to UI defaults", 1)
    }, error = function(e) {
      debug_log(paste("Error resetting Sample IDs plot options:", e$message), 1)
      showNotification("Error resetting Sample IDs plot options", type = "error", duration = 3)
    })
  })

  # --------------------------------------------------------------------------
  # h. Download handler
  #    Exports the current ggplot2 object to the user-specified file format
  #    using the device dimensions and resolution inputs.
  # --------------------------------------------------------------------------

  output$downloadPlotButton_SampleIDTab <- downloadHandler(
    filename = function() {
      paste0(input$plotTitle_SampleIDTab, "_IDplot.", input$downloadFormat_SampleIDTab)
    },
    content = function(file) {
      req(state$ggplot_object_SampleIDTab())

      width_in  <- input$plotWidthInch_SampleIDTab
      height_in <- input$plotHeightInch_SampleIDTab
      res       <- input$resolution_DPI_SampleIDTab
      fmt       <- input$downloadFormat_SampleIDTab
      p         <- state$ggplot_object_SampleIDTab()

      switch(fmt,
        png  = png (file, width = width_in, height = height_in, units = "in", res = res),
        tiff = tiff(file, width = width_in, height = height_in, units = "in", res = res),
        jpeg = jpeg(file, width = width_in, height = height_in, units = "in", res = res),
        svg  = svg (file, width = width_in, height = height_in),
        pdf  = pdf (file, width = width_in, height = height_in)
      )
      on.exit(try(dev.off(), silent = TRUE), add = TRUE)
      print(p)
    }
  )

  # --------------------------------------------------------------------------
  # h. Add-to-grid observer
  #    Sends the current ggplot2 object to the Grid module with the label
  #    specified by the user. Shows a notification on success or failure.
  # --------------------------------------------------------------------------

  observeEvent(input$add_to_grid, {
    debug_log("SampleIDs: add_to_grid clicked", 2)

    p <- tryCatch({
      state$ggplot_object_SampleIDTab()
    }, error = function(e) {
      debug_log(paste("SampleIDs: error accessing plot:", e$message), 1)
      NULL
    })

    if (is.null(p)) {
      showNotification("No plot available to add.", type = "error")
      return()
    }
    if (!inherits(p, "ggplot")) {
      showNotification("Only ggplot objects can be added to the grid (Phase 1).",
                       type = "error")
      return()
    }

    lbl_raw <- input$grid_label
    lbl_id  <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize_plot_id(lbl_raw)
    plot_id <- paste0(ns(""), "SampleIDs_", lbl_id)
    lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "Sample IDs"

    debug_log(paste("SampleIDs: adding", plot_id), 2)
    modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "SampleIDs")
    showNotification("Added to grid selection.", type = "message")
  })

  # --------------------------------------------------------------------------
  # i. Session restore trigger observer
  #    Fires after all module restore_fn()s have run and rv$session_restoring
  #    has been cleared. Pushes the staged pending_ui_inputs back into the
  #    static-choice widgets (inputs with dynamic choices are handled inside
  #    observer a and b above). Clears pending_ui_inputs when done.
  # --------------------------------------------------------------------------

  observeEvent(rv$session_restore_trigger, {
    captured <- isolate(state$pending_ui_inputs())
    generation <- isolate(rv$session_restore_generation %||% NA_integer_)
    register_job <- session$userData$register_restore_job
    job_id <- if (is.function(register_job)) tryCatch(
      register_job("SampleIDs", "poll and plot rebuild", "render", 2),
      error = function(e) NULL
    ) else NULL
    restore_poll_job(job_id)
    restore_poll_generation(generation)
    if (is.null(captured)) {
      state$had_plot_on_save(FALSE)
      settle_restore_poll("skipped", "NO_CAPTURED_SAMPLEIDS_INPUTS")
      return()
    }
    # onFlushed is imperative: protect it with the shared runner and only arm
    # the reactive poll. All reads, readiness checks and invalidateLater calls
    # deliberately remain in the observer below.
    session$onFlushed(function() {
      armed <- .run_session_restore_callback(
        owner = "SampleIDs", reason = "arm restore poll",
        generation = generation, phase = "render",
        job_metadata = list(
          current_generation = function() isolate(rv$session_restore_generation %||% NA_integer_)
        ),
        callback = function() {
          restore_poll_captured(captured)
          restore_poll_attempt(0L)
          restore_poll_active(TRUE)
          debug_log("[SampleIDs] session restore: UI input sync initiated", 1)
        }
      )
      if (!isTRUE(armed)) settle_restore_poll("skipped", "POLL_ARM_REJECTED")
    }, once = TRUE)
  }, ignoreInit = TRUE)

  # Poll in a reactive consumer until dynamic UI + restored data are in sync.
  observe({
    if (!isTRUE(restore_poll_active())) return()

    captured <- restore_poll_captured()
    expected_generation <- restore_poll_generation()
    current_generation <- rv$session_restore_generation %||% NA_integer_
    if (!identical(as.integer(current_generation)[1L], as.integer(expected_generation)[1L])) {
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      settle_restore_poll("skipped", "STALE_GENERATION")
      return()
    }
    if (is.null(captured) || !is.list(captured)) {
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      settle_restore_poll("skipped", "INVALID_CAPTURED_SAMPLEIDS_INPUTS")
      return()
    }

    attempt <- isolate(restore_poll_attempt()) + 1L
    restore_poll_attempt(attempt)
    if (attempt == 1L) apply_sampleids_restored_inputs(captured)
    if (attempt > 20L) {
      debug_log("[SampleIDs] restore timed out waiting for UI/data synchronization", 1)
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      settle_restore_poll("timeout", "SAMPLEIDS_UI_DATA_SYNC_TIMEOUT")
      return()
    }

    if (!isTRUE(state$had_plot_on_save())) {
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      settle_restore_poll("skipped", "NO_SAVED_SAMPLEIDS_PLOT")
      return()
    }

    if (isTRUE(is_sampleids_restore_ready(captured))) {
      rebuild_ok <- tryCatch({
        cache <- isolate(state$restore_plot_data_cache())
        if (has_valid_restore_plot_data_cache(cache)) {
          ui_snapshot <- isolate(state$plot_ui_cache())
          if (!is.list(ui_snapshot)) ui_snapshot <- captured
          rebuild_ok <- regenerate_sampleids_plot(ui_override = ui_snapshot, force_cached = TRUE)
          if (isTRUE(rebuild_ok)) state$restore_plot_data_cache(NULL)
          debug_log("[SampleIDs] session restore: plot rebuilt from cached restore data", 1)
          isTRUE(rebuild_ok)
        } else {
          rebuilt <- regenerate_sampleids_plot()
          debug_log("[SampleIDs] session restore: plot rebuilt from restored inputs", 1)
          isTRUE(rebuilt)
        }
      }, error = function(e) {
        debug_log(paste("[SampleIDs] restore plot rebuild failed:", e$message), 1)
        FALSE
      })
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$plot_from_restore_cache(has_valid_restore_plot_data_cache(isolate(state$restore_plot_data_cache())))
      settle_restore_poll(if (isTRUE(rebuild_ok)) "completed" else "error",
                          if (isTRUE(rebuild_ok)) NULL else "SAMPLEIDS_REBUILD_FAILED")
      return()
    }

    invalidateLater(50, session)
  })
}
