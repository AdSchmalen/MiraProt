# ==============================================================================
# File: modules/abundances/abundances_observer.R
#
# Purpose:
#   Contains all observe() and observeEvent() blocks, renderUI/renderPlot/
#   renderPlotly outputs, and the download handler for the Abundances module.
#   This file centralizes all reactive side-effects so that the orchestrator
#   file stays lean and focused on wiring.
#
# Architectural Role:
#   Observer and output layer of the Abundances module. Called from
#   modAbundancesServer() via register_abundances_observers() after state is
#   initialized. All observers run inside the moduleServer() closure of the
#   orchestrator. Pure plotting logic is delegated to build_abundances_plot()
#   from abundances_logic.R. Reactive state comes from create_abundances_state()
#   via the `state` list argument.
#
# Structure:
#   1. register_abundances_observers() - Registration function containing:
#      a. Data-type dropdown observer   - updates available choices from metadata
#      b. Plot-title sync observer      - mirrors data type selection to title input
#      c. Refresh plot observer         - builds and stores plot on button click
#      d. Dynamic plot UI output        - switches between static and interactive
#      e. Plotly output                 - renders interactive plot
#      f. Static plot output            - renders ggplot2 plot
#      g. Download handler              - exports ggplot2 plot to file
#      h. Add-to-grid observer          - sends current plot to the Grid module
#
# Notes for future developers:
#   - Observer registration order matters: the refresh observer depends on
#     both data_def and data_mod being available.
#   - debug_log is passed in from modAbundancesServer; do not define a new one.
#   - All pure computation is delegated to build_abundances_plot(). Keep
#     observers thin: validate inputs, call logic, update state, notify user.
#   - The add-to-grid observer reads modEnv$add_to_grid from the global module
#     environment; this is the established grid integration pattern.
# ==============================================================================


#' Register all observers and outputs for the Abundances module.
#'
#' @param input     Shiny input object from moduleServer closure.
#' @param output    Shiny output object from moduleServer closure.
#' @param session   Shiny session object.
#' @param ns        Namespace function for this module.
#' @param state     Named list from create_abundances_state().
#' @param rv        Global reactive values object (rv$data_def, rv$data_mod,
#'                  rv$height_px, rv$px_ratio).
#' @param debug_log Logging function with signature (message, level).
register_abundances_observers <- function(input, output, session, ns,
                                          state, rv, debug_log) {
  normalize_content_value <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
  }

  match_content_exact <- function(content, selected_content) {
    selected <- normalize_content_value(selected_content)[1]
    if (!is.character(selected) || !nzchar(selected)) return(integer(0))
    which(normalize_content_value(content) == selected)
  }

  plot_object_abundanceTab   <- state$plot_object_abundanceTab
  ggplot_object_abundanceTab <- state$ggplot_object_abundanceTab
  restore_poll_active        <- reactiveVal(FALSE)
  restore_poll_attempt       <- reactiveVal(0L)
  restore_poll_captured      <- reactiveVal(NULL)
  live_ui_sync_partial       <- reactiveVal(FALSE)

  restore_context_active <- function() {
    isTRUE(rv$session_restoring) ||
      isTRUE(restore_poll_active()) ||
      (isTRUE(state$restore_mode_cached()) && !is.null(state$pending_ui_inputs()))
  }

  release_cached_restore_plot <- function() {
    restore_poll_active(FALSE)
    restore_poll_captured(NULL)
    state$pending_ui_inputs(NULL)
    state$had_plot_on_save(FALSE)
    state$restore_mode_cached(FALSE)
    state$restore_plot_data_cache(NULL)
    state$plot_from_restore_cache(FALSE)
  }

  has_valid_plot_creation_cache <- function(cache) {
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
    if (!has_valid_plot_creation_cache(cache) || !inherits(rv$data_mod, "data.frame") ||
        !inherits(rv$data_def, "data.frame")) return(FALSE)
    !identical(
      data_pair_signature(cache$data_mod, cache$data_def),
      data_pair_signature(rv$data_mod, rv$data_def)
    )
  }

  set_restore_diag <- function(updates = list()) {
    reports <- isolate(rv$restore_reports)
    if (!is.list(reports)) reports <- list()
    base <- reports[["Abundances"]]
    if (!is.list(base)) base <- list()
    reports[["Abundances"]] <- utils::modifyList(base, updates)
    rv$restore_reports <- reports
  }

  build_abundances_plot_from_snapshot <- function(data_mod, data_def, ui_snapshot) {
    data_abundance <- as.character(ui_snapshot$data_abundanceTab %||% "")[1]
    abundance_title <- as.character(ui_snapshot$plotTitle_Abundance %||% data_abundance)[1]
    selected_layout <- ui_snapshot$ThemeSelect_Abundance
    label_input <- ui_snapshot$label_abundanceTab
    color_input <- ui_snapshot$col_abundanceTab

    abundance_index <- match_content_exact(data_def$Content, data_abundance)
    if (length(abundance_index) == 0) abundance_index <- NULL
    if (is.null(abundance_index)) return(NULL)
    abundance_index <- abundance_index[abundance_index >= 1L & abundance_index <= ncol(data_mod) & abundance_index <= nrow(data_def)]
    if (length(abundance_index) == 0L) {
      debug_log("Abundances restore skipped: cached abundance indices do not match cached data dimensions", 1)
      return(NULL)
    }

    transformation_df <- data_def$Transformation[abundance_index]
    data <- retransform_data_global(data_mod, abundance_index, transformation_df)
    df <- data[, abundance_index, drop = FALSE]
    if (identical(label_input, "Column name")) {
      new_labels <- data_def[abundance_index, ][["Column"]]
    } else {
      new_labels <- data_def[abundance_index, ][["Sample"]]
    }
    colnames(df) <- new_labels
    df <- melt(df, variable.name = "Variable", value.name = "Value")
    df <- df[is.finite(df$Value), ]

    build_abundances_plot(
      data_long       = df,
      data_abundance  = data_abundance,
      abundance_title = abundance_title,
      color_input     = color_input,
      selected_layout = selected_layout,
      hide_title      = isTRUE(ui_snapshot$hideTitle_Abundance),
      plot_title_size = as.numeric(ui_snapshot$PlotTitleSize_Abundances),
      axis_title_size = as.numeric(ui_snapshot$AxisTitleSize_Abundances),
      tick_size       = as.numeric(ui_snapshot$tickSize_Abundance),
      debug_log       = debug_log
    )
  }

  regenerate_abundances_plot <- function(ui_override = NULL, force_cached = FALSE) {
    cache <- state$restore_plot_data_cache()
    has_cached_pair <- is.list(cache) &&
      inherits(cache$data_mod, "data.frame") &&
      inherits(cache$data_def, "data.frame")
    use_restore_cache <- FALSE
    if (isTRUE(force_cached)) {
      if (!isTRUE(has_cached_pair)) return(FALSE)
      data_mod_active <- cache$data_mod
      data_def_active <- cache$data_def
      use_restore_cache <- TRUE
    } else {
      # The restore cache is a manual-release replay source: restored plots stay
      # bound to their saved data until the user clicks Create Plot, which
      # clears the cache and rebuilds from current rv$data_mod/rv$data_def.
      use_restore_cache <- isTRUE(has_cached_pair) && isTRUE(restore_context_active())
      data_mod_active <- if (isTRUE(use_restore_cache)) cache$data_mod else rv$data_mod
      data_def_active <- if (isTRUE(use_restore_cache)) cache$data_def else rv$data_def
      req(data_mod_active, data_def_active)
    }

    ui_value <- function(id, default = NULL) {
      if (is.list(ui_override) && !is.null(ui_override[[id]])) return(ui_override[[id]])
      v <- isolate(input[[id]])
      if (is.null(v)) default else v
    }

    # Ensure only scalar values are used. During session restore, the current
    # UI selection can be invalid for the *currently loaded* dataset while the
    # cached restore dataset is valid. Prefer the staged restore value in that
    # case so the plot can be rebuilt from the cached pair deterministically.
    pending_restore <- state$pending_ui_inputs()
    data_abundance  <- as.character(ui_value("data_abundanceTab"))[1]
    if ((!is.character(data_abundance) || !nzchar(data_abundance)) &&
        is.list(pending_restore) && !is.null(pending_restore$data_abundanceTab)) {
      data_abundance <- as.character(pending_restore$data_abundanceTab)[1]
    }
    abundance_title <- as.character(ui_value("plotTitle_Abundance"))[1]
    if ((!is.character(abundance_title) || !nzchar(abundance_title)) &&
        is.list(pending_restore) && !is.null(pending_restore$plotTitle_Abundance)) {
      abundance_title <- as.character(pending_restore$plotTitle_Abundance)[1]
    }
    selected_layout <- ui_value("ThemeSelect_Abundance")
    hide_title      <- isTRUE(ui_value("hideTitle_Abundance", FALSE))
    label_input     <- ui_value("label_abundanceTab")
    color_input     <- ui_value("col_abundanceTab")

    data_def <- data_def_active
    data     <- data_mod_active

    abundance_index <- match_content_exact(data_def$Content, data_abundance)
    if (length(abundance_index) == 0) {
      debug_log(paste0("Abundances: no matching content for '", data_abundance,
                       "' in active data_def (rows=", nrow(data_def), ")"), 1)
    }
    if (length(abundance_index) == 0) abundance_index <- NULL

    if (is.null(abundance_index)) {
      plot_object_abundanceTab(NULL)
      ggplot_object_abundanceTab(NULL)
      return(FALSE)
    }
    abundance_index <- abundance_index[abundance_index >= 1L & abundance_index <= ncol(data) & abundance_index <= nrow(data_def)]
    if (length(abundance_index) == 0L) {
      debug_log("Abundances: matching content indices exceed active data dimensions; skipping rebuild", 1)
      plot_object_abundanceTab(NULL)
      ggplot_object_abundanceTab(NULL)
      return(FALSE)
    }

    transformation_df <- data_def$Transformation[abundance_index]
    data <- retransform_data_global(data, abundance_index, transformation_df)
    df <- data[, abundance_index, drop = FALSE]

    if (label_input == "Column name") {
      new_labels <- data_def[abundance_index, ][["Column"]]
    } else {
      new_labels <- data_def[abundance_index, ][["Sample"]]
    }
    colnames(df) <- new_labels

    df <- melt(df, variable.name = "Variable", value.name = "Value")
    df <- df[is.finite(df$Value), ]

    result <- build_abundances_plot(
      data_long       = df,
      data_abundance  = data_abundance,
      abundance_title = abundance_title,
      color_input     = color_input,
      selected_layout = selected_layout,
      hide_title      = hide_title,
      plot_title_size = as.numeric(ui_value("PlotTitleSize_Abundances")),
      axis_title_size = as.numeric(ui_value("AxisTitleSize_Abundances")),
      tick_size       = as.numeric(ui_value("tickSize_Abundance")),
      debug_log       = debug_log
    )

    if (is.null(result)) {
      plot_object_abundanceTab(NULL)
      ggplot_object_abundanceTab(NULL)
      return(FALSE)
    }

    plot_object_abundanceTab(result$plotly_object)
    ggplot_object_abundanceTab(result$ggplot_object)
    state$plot_render_nonce(isolate(state$plot_render_nonce()) + 1L)
    state$plot_from_restore_cache(isTRUE(use_restore_cache))
    state$plot_creation_cache(list(
      data_mod = data_mod_active,
      data_def = data_def_active
    ))
    state$plot_ui_cache(list(
      data_abundanceTab = data_abundance,
      plotTitle_Abundance = abundance_title,
      ThemeSelect_Abundance = selected_layout,
      hideTitle_Abundance = hide_title,
      label_abundanceTab = label_input,
      col_abundanceTab = color_input,
      PlotTitleSize_Abundances = ui_value("PlotTitleSize_Abundances"),
      AxisTitleSize_Abundances = ui_value("AxisTitleSize_Abundances"),
      tickSize_Abundance = ui_value("tickSize_Abundance")
    ))
    debug_log("Abundances: plot built and stored", 1)
    set_restore_diag(list(rebuild_from_cached_metadata_ok = TRUE))
    TRUE
  }

  apply_abundances_restored_inputs <- function(captured) {
    if (is.null(captured) || !is.list(captured)) return(invisible(FALSE))

    static_ids <- c(
      "checkbox_interactive_AbundanceTab", "label_abundanceTab",
      "col_abundanceTab", "ThemeSelect_Abundance", "hideTitle_Abundance",
      "PlotTitleSize_Abundances", "AxisTitleSize_Abundances", "tickSize_Abundance",
      "resolution_DPI_Abundances", "plotWidthInch_Abundances",
      "plotHeightInch_Abundances", "downloadFormat_Abundances"
    )
    numeric_input_ids <- c(
      "PlotTitleSize_Abundances", "AxisTitleSize_Abundances",
      "tickSize_Abundance", "resolution_DPI_Abundances",
      "plotWidthInch_Abundances", "plotHeightInch_Abundances"
    )
    safe_select_update <- function(id, val) {
      if (is.null(val)) return(invisible(FALSE))
      static_choices <- list(
        label_abundanceTab = c("Column name", "Sample name"),
        col_abundanceTab = c("Magma", "Inferno", "Plasma", "Viridis", "Cividis", "Rocket", "Mako", "Turbo", "Black and White"),
        ThemeSelect_Abundance = c("Gray", "Black and White", "Linedraw", "Light", "Dark", "Minimal", "Classic", "Void"),
        downloadFormat_Abundances = c("png", "jpeg", "tiff", "svg", "pdf")
      )
      allowed <- static_choices[[id]]
      val_chr <- as.character(val)[1]
      if (id %in% numeric_input_ids) {
        updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
        return(invisible(TRUE))
      }
      if (is.null(allowed) || val_chr %in% allowed) {
        updateSelectInput(session, id, selected = val_chr)
        return(invisible(TRUE))
      }
      debug_log("cached_restore_ui_choice_missing_nonfatal", 1)
      live_ui_sync_partial(TRUE)
      invisible(FALSE)
    }
    for (id in static_ids) {
      val <- captured[[id]]
      if (is.null(val)) next
      if (is.logical(val)) {
        updateCheckboxInput(session, id, value = val)
      } else if (id %in% numeric_input_ids) {
        updateNumericInput(session, id, value = suppressWarnings(as.numeric(val)[1]))
      } else {
        safe_select_update(id, val)
      }
    }
    if (!is.null(captured$plotTitle_Abundance)) {
      updateTextInput(session, "plotTitle_Abundance",
                      value = as.character(captured$plotTitle_Abundance)[1])
    }
    invisible(TRUE)
  }

  is_abundances_restore_ready <- function(captured) {
    if (is.null(captured) || !is.list(captured)) return(FALSE)
    cache <- state$restore_plot_data_cache()
    data_mod_active <- if (is.list(cache) && inherits(cache$data_mod, "data.frame")) cache$data_mod else rv$data_mod
    data_def_active <- if (is.list(cache) && inherits(cache$data_def, "data.frame")) cache$data_def else rv$data_def
    if (is.null(data_mod_active) || is.null(data_def_active)) return(FALSE)
    snapshot <- state$plot_ui_cache()
    data_abundance <- if (is.list(snapshot) && !is.null(snapshot$data_abundanceTab)) {
      as.character(snapshot$data_abundanceTab)[1]
    } else {
      isolate(as.character(input$data_abundanceTab)[1])
    }
    if (!is.character(data_abundance) || !nzchar(data_abundance)) return(FALSE)

    same_single <- function(id) {
      expected <- captured[[id]]
      if (is.null(expected)) return(TRUE)
      identical(isolate(as.character(input[[id]])[1]), as.character(expected)[1])
    }

    if (!isTRUE(state$restore_mode_cached()) && !same_single("data_abundanceTab")) return(FALSE)
    if (!isTRUE(state$restore_mode_cached()) && !same_single("plotTitle_Abundance")) return(FALSE)
    if (!isTRUE(state$restore_mode_cached()) && !same_single("hideTitle_Abundance")) return(FALSE)
    TRUE
  }

  # --------------------------------------------------------------------------
  # a. Data-type dropdown observer
  #    Filters the data type selectInput to only the abundance types that
  #    are present in the current metadata. Runs whenever metadata changes.
  # --------------------------------------------------------------------------

  observe({
    req(rv$data_def, rv$data_mod)

    data_def <- rv$data_def
    possible_values <- c(
      "Raw Abundance", "Normalized Abundance",
      "Imputed Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
      "Batch Corrected Raw Abundance", "Batch Corrected Normalized Abundance"
    )
    content_values <- normalize_content_value(data_def$Content)
    possible_values_norm <- normalize_content_value(possible_values)
    available_choices <- possible_values[possible_values_norm %in% content_values]

    current_selection <- isolate(as.character(input$data_abundanceTab %||% "")[1])
    selected_choice <- if (nzchar(current_selection) &&
                           normalize_content_value(current_selection) %in% normalize_content_value(available_choices)) {
      available_choices[match(normalize_content_value(current_selection), normalize_content_value(available_choices))]
    } else {
      available_choices[1]
    }

    updateSelectInput(session, "data_abundanceTab",
                      choices = available_choices,
                      selected = selected_choice)

    # If a session restore is in progress, apply the saved data type selection
    # now that the valid choices have been repopulated.  Reading
    # pending_ui_inputs() reactively ensures this observer re-fires when
    # set_session_state() stages the captured inputs after the data has
    # already been restored (so the dynamic selection is not missed).
    pending <- state$pending_ui_inputs()
    if (isTRUE(restore_context_active()) && !is.null(pending) && !is.null(pending$data_abundanceTab)) {
      valid <- intersect(pending$data_abundanceTab, available_choices)
      if (length(valid) > 0) {
        updateSelectInput(session, "data_abundanceTab", selected = valid)
      } else {
        live_ui_sync_partial(TRUE)
      }
    }
  })

  # --------------------------------------------------------------------------
  # b. Plot-title sync observer
  #    Mirrors the selected data type to the plot title input when the user
  #    changes the data type selection.
  # --------------------------------------------------------------------------

  observeEvent(input$data_abundanceTab, {
    pending <- state$pending_ui_inputs()
    if (!is.null(pending) && !is.null(pending$plotTitle_Abundance)) return()
    updateTextInput(session, "plotTitle_Abundance",
                   value = as.character(input$data_abundanceTab)[1])
  })

  # --------------------------------------------------------------------------
  # c. Refresh plot observer
  #    Validates inputs, reshapes data, delegates to build_abundances_plot(),
  #    and stores the result in the reactive state values.
  # --------------------------------------------------------------------------

  observeEvent(input$refresh_abundanceTab, {
    release_cached_restore_plot()
    regenerate_abundances_plot()
  })

  debounced_abundances_plot_inputs <- debounce(reactive({
    list(
      data_abundanceTab = input$data_abundanceTab,
      label_abundanceTab = input$label_abundanceTab,
      col_abundanceTab = input$col_abundanceTab,
      ThemeSelect_Abundance = input$ThemeSelect_Abundance,
      plotTitle_Abundance = input$plotTitle_Abundance,
      hideTitle_Abundance = input$hideTitle_Abundance,
      PlotTitleSize_Abundances = input$PlotTitleSize_Abundances,
      AxisTitleSize_Abundances = input$AxisTitleSize_Abundances,
      tickSize_Abundance = input$tickSize_Abundance
    )
  }), 700)

  observeEvent(debounced_abundances_plot_inputs(), {
    if (datawizard_restore_phase_active(rv) || isTRUE(restore_context_active()) || isTRUE(state$plot_from_restore_cache())) return()
    if (is.null(plot_object_abundanceTab()) && is.null(ggplot_object_abundanceTab())) return()
    if (isTRUE(active_data_differs_from_plot_cache())) {
      debug_log("Abundances: input echo from different live data ignored; keeping plot creation cache", 1)
      return()
    }

    regenerate_abundances_plot(ui_override = debounced_abundances_plot_inputs())
  }, ignoreInit = TRUE)

  observeEvent(list(rv$data_mod, rv$data_def), {
    if (datawizard_restore_phase_active(rv) || isTRUE(restore_context_active()) || isTRUE(state$plot_from_restore_cache())) return()
    if (is.null(plot_object_abundanceTab()) && is.null(ggplot_object_abundanceTab())) return()

    # Existing plots are snapshots of the data/UI at creation time. Loading or
    # assigning a different live dataset in Data Wizard must not re-materialize
    # the plot against incompatible metadata and make it disappear. The explicit
    # Refresh button remains the only live-data rebuild path.
    debug_log("Abundances: live data/metadata changed; keeping existing plot snapshot", 1)
    return()
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # d. Reset plot-options controls
  #    Restores only the Plot options wellPanel to its UI defaults. General
  #    options are intentionally left unchanged.
  # --------------------------------------------------------------------------

  observeEvent(input$resetButton_Abundances, {
    tryCatch({
      debug_log("Resetting Abundances plot options to UI defaults", 1)

      updateSelectInput(session, "col_abundanceTab", selected = "Viridis")
      updateSelectInput(session, "ThemeSelect_Abundance", selected = "Classic")
      updateTextInput(session, "plotTitle_Abundance", value = "")
      updateCheckboxInput(session, "hideTitle_Abundance", value = TRUE)
      updateNumericInput(session, "PlotTitleSize_Abundances", value = 20)
      updateNumericInput(session, "AxisTitleSize_Abundances", value = 20)
      updateNumericInput(session, "tickSize_Abundance", value = 18)

      showNotification("Abundances plot options reset to defaults.", type = "message", duration = 3)
      debug_log("Abundances plot options reset to UI defaults", 1)
    }, error = function(e) {
      debug_log(paste("Error resetting Abundances plot options:", e$message), 1)
      showNotification("Error resetting Abundances plot options", type = "error", duration = 3)
    })
  })

  # --------------------------------------------------------------------------
  # e. Dynamic plot UI output
  #    Switches between a plotlyOutput (interactive) and a plotOutput (static)
  #    depending on the checkbox state.
  # --------------------------------------------------------------------------

  output$plot_abundanceUI <- renderUI({
    req(rv$height_px, rv$px_ratio)
    height_px <- paste0(rv$height_px * rv$px_ratio, "px")

    if (isTRUE(input$checkbox_interactive_AbundanceTab)) {
      plotlyOutput(ns("plot_abundanceTab"), height = height_px)
    } else {
      plotOutput(ns("plot_abundanceTab_static"), height = height_px)
    }
  })

  # --------------------------------------------------------------------------
  # e. Plotly output
  # --------------------------------------------------------------------------

  output$plot_abundanceTab <- renderPlotly({
    state$plot_render_nonce()
    req(plot_object_abundanceTab())
    plot_object_abundanceTab()
  })

  # --------------------------------------------------------------------------
  # f. Static plot output
  # --------------------------------------------------------------------------

  output$plot_abundanceTab_static <- renderPlot({
    state$plot_render_nonce()
    req(ggplot_object_abundanceTab())
    ggplot_object_abundanceTab()
  })

  # --------------------------------------------------------------------------
  # g. Download handler
  #    Exports the current ggplot2 object to the user-specified file format
  #    using the device dimensions and resolution inputs.
  # --------------------------------------------------------------------------

  output$downloadPlotButton_Abundances <- downloadHandler(
    filename = function() {
      paste0("abundance_plot.", input$downloadFormat_Abundances)
    },
    content = function(file) {
      req(ggplot_object_abundanceTab())

      width  <- input$plotWidthInch_Abundances
      height <- input$plotHeightInch_Abundances
      dpi    <- input$resolution_DPI_Abundances
      fmt    <- input$downloadFormat_Abundances
      plt    <- ggplot_object_abundanceTab()

      if (fmt %in% c("png", "jpeg", "tiff")) {
        dev_fun <- match.fun(fmt)
        dev_fun(file, width = width, height = height, units = "in", res = dpi)
      } else if (fmt == "svg") {
        svg(file, width = width, height = height)
      } else if (fmt == "pdf") {
        pdf(file, width = width, height = height)
      }
      on.exit(try(dev.off(), silent = TRUE), add = TRUE)
      print(plt)
    }
  )

  # --------------------------------------------------------------------------
  # h. Add-to-grid observer
  #    Sends the current ggplot2 object to the Grid module with the label
  #    specified by the user. Shows a notification on success or failure.
  # --------------------------------------------------------------------------

  observeEvent(input$add_to_grid, {
    debug_log("Abundances: add_to_grid clicked", 2)

    p <- tryCatch({
      ggplot_object_abundanceTab()
    }, error = function(e) {
      debug_log(paste("Abundances: error accessing plot:", e$message), 1)
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
    plot_id <- paste0(ns(""), "Abundances_", lbl_id)
    lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "Abundances"

    debug_log(paste("Abundances: adding", plot_id), 2)
    modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "Abundances")
    showNotification("Added to grid selection.", type = "message")
  })

  # --------------------------------------------------------------------------
  # i. Session restore trigger observer
  #    Fires after all module restore_fn()s have run and rv$session_restoring
  #    has been cleared. Pushes the staged pending_ui_inputs back into the
  #    static-choice widgets. The dynamic data_abundanceTab selection is handled
  #    inside observer a above. Clears pending_ui_inputs when done.
  # --------------------------------------------------------------------------

  observeEvent(list(rv$session_restore_trigger, state$restore_rebuild_nonce()), {
    live_ui_sync_partial(FALSE)
    captured <- isolate(state$pending_ui_inputs())
    if (is.null(captured)) {
      state$had_plot_on_save(FALSE)
      return()
    }
    session$onFlushed(function() {
      tryCatch({
        restore_mode_cached <- isolate(state$restore_mode_cached())
        if (isTRUE(restore_mode_cached)) {
          cached_rebuild_success <- FALSE
          cached_rebuild_nonce <- isolate(state$restore_rebuild_nonce())
          set_restore_diag(list(restore_rebuild_nonce = cached_rebuild_nonce))
          set_restore_diag(list(cached_ui_snapshot_loaded = is.list(isolate(state$plot_ui_cache()))))
          if (isTRUE(isolate(state$had_plot_on_save()))) {
            rebuild_stage <- isolate({
              list(
                cache = state$restore_plot_data_cache(),
                ui_snapshot = state$plot_ui_cache()
              )
            })
            cache <- rebuild_stage$cache
            ui_snapshot <- rebuild_stage$ui_snapshot
            has_cached_pair <- is.list(cache) &&
              inherits(cache$data_mod, "data.frame") &&
              inherits(cache$data_def, "data.frame")
            if (isTRUE(has_cached_pair)) {
              debug_log("[Abundances] cached_restore_rebuild_started", 1)
              rebuild_ok <- tryCatch({
                validate(
                  need(is.list(ui_snapshot), "cached_restore_missing_ui_snapshot"),
                  need(!is.null(ui_snapshot$data_abundanceTab) &&
                         nzchar(as.character(ui_snapshot$data_abundanceTab)[1]),
                       "cached_restore_missing_data_type")
                )
                result <- build_abundances_plot_from_snapshot(
                  data_mod = cache$data_mod,
                  data_def = cache$data_def,
                  ui_snapshot = ui_snapshot
                )
                validate(need(!is.null(result), "cached_restore_rebuild_returned_null"))
                plot_object_abundanceTab(result$plotly_object)
                ggplot_object_abundanceTab(result$ggplot_object)
                state$plot_render_nonce(isolate(state$plot_render_nonce()) + 1L)
                state$plot_creation_cache(list(
                  data_mod = cache$data_mod,
                  data_def = cache$data_def
                ))
                state$plot_from_restore_cache(TRUE)
                TRUE
              }, error = function(e) {
                set_restore_diag(list(
                  rebuild_from_cached_metadata_ok = FALSE,
                  reason = "restore_rebuild_ok",
                  restore_error = paste0("cached_restore_rebuild_error: ", e$message)
                ))
                debug_log(paste0("[Abundances] cached_restore_rebuild_error: ", e$message), 1)
                FALSE
              })
              if (!isTRUE(rebuild_ok)) {
                set_restore_diag(list(
                  rebuild_from_cached_metadata_ok = FALSE,
                  reason = "restore_rebuild_ok",
                  restore_error = "cached_restore_rebuild_returned_false"
                ))
              } else {
                cached_rebuild_success <- TRUE
                # The compact plot request/UI and canonical plot_creation_cache
                # now back all interaction; release the restore-only duplicate.
                state$restore_plot_data_cache(NULL)
                set_restore_diag(list(
                  rebuild_from_cached_metadata_ok = TRUE,
                  reason = "restore_rebuild_ok"
                ))
              }
              debug_log(paste0(
                "[Abundances] cached_restore_rebuild_finished plot_object_nonnull=",
                !is.null(isolate(plot_object_abundanceTab()))
              ), 1)
            } else {
              set_restore_diag(list(
                rebuild_from_cached_metadata_ok = FALSE,
                reason = "restore_rebuild_ok",
                restore_error = "cached_restore_missing_pair"
              ))
              debug_log("[Abundances] cached_restore_rebuild_started", 1)
              debug_log("[Abundances] cached_restore_rebuild_finished plot_object_nonnull=FALSE", 1)
            }
          }
          ui_sync_ok <- tryCatch({
            apply_abundances_restored_inputs(captured)
            TRUE
          }, error = function(e) {
            debug_log(paste0("[Abundances] ui_sync_partial: ", e$message), 1)
            FALSE
          })
          ui_sync_partial <- isTRUE(isolate(live_ui_sync_partial()))
          if (isTRUE(ui_sync_partial) || !isTRUE(ui_sync_ok)) {
            set_restore_diag(list(reason = "ui_sync_partial", live_ui_sync_partial = TRUE))
          } else {
            set_restore_diag(list(live_ui_sync_partial = FALSE))
          }
          retained_retry <- FALSE
          if (!isTRUE(cached_rebuild_success) &&
              (isTRUE(ui_sync_partial) || !isTRUE(ui_sync_ok))) {
            retained_retry <- TRUE
          }
          if (isTRUE(retained_retry)) {
            restore_poll_captured(captured)
            restore_poll_attempt(0L)
            restore_poll_active(TRUE)
            debug_log("[Abundances] session restore: retained pending UI inputs for retry", 1)
          } else {
            state$pending_ui_inputs(NULL)
            state$had_plot_on_save(FALSE)
            state$restore_mode_cached(FALSE)
          }
          return(invisible(NULL))
        }
        apply_abundances_restored_inputs(captured)
        restore_poll_captured(captured)
        restore_poll_attempt(0L)
        restore_poll_active(TRUE)
        debug_log("[Abundances] session restore: UI input sync initiated", 1)
      }, error = function(e) {
        debug_log(paste0("[Abundances] ui_sync_partial: ", e$message), 1)
        set_restore_diag(list(reason = "ui_sync_partial", live_ui_sync_partial = TRUE))
      })
    }, once = TRUE)
  }, ignoreInit = TRUE, priority = 100)

  observe({
    if (!isTRUE(restore_poll_active())) return()

    captured <- restore_poll_captured()
    if (is.null(captured) || !is.list(captured)) {
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      state$restore_mode_cached(FALSE)
      return()
    }

    attempt <- isolate(restore_poll_attempt()) + 1L
    restore_poll_attempt(attempt)
    if (attempt > 20L) {
      debug_log("[Abundances] restore timed out waiting for UI/data synchronization", 1)
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      state$restore_mode_cached(FALSE)
      return()
    }

    # Re-apply restored title while pending to prevent the data-type observer
    # from overwriting a user-customized title before rebuild.
    if (!is.null(captured$plotTitle_Abundance)) {
      updateTextInput(session, "plotTitle_Abundance",
                      value = as.character(captured$plotTitle_Abundance)[1])
    }
    if (!is.null(captured$hideTitle_Abundance)) {
      updateCheckboxInput(session, "hideTitle_Abundance",
                          value = isTRUE(captured$hideTitle_Abundance))
    }

    if (!isTRUE(state$had_plot_on_save())) {
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      state$restore_mode_cached(FALSE)
      return()
    }

    if (isTRUE(is_abundances_restore_ready(captured))) {
      tryCatch({
        regenerate_abundances_plot(ui_override = isolate(state$plot_ui_cache()))
        set_restore_diag(list(rebuild_from_cached_metadata_ok = TRUE))
        debug_log("[Abundances] session restore: plot rebuilt from restored inputs", 1)
      }, error = function(e) {
        set_restore_diag(list(rebuild_from_cached_metadata_ok = FALSE))
        debug_log(paste("[Abundances] restore plot rebuild failed:", e$message), 1)
      })
      restore_poll_active(FALSE)
      state$pending_ui_inputs(NULL)
      state$had_plot_on_save(FALSE)
      state$restore_mode_cached(FALSE)
      return()
    }

    invalidateLater(50, session)
  })
}
