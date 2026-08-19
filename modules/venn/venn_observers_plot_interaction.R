# =============================================================================
# modules/venn/venn_observers_plot_interaction.R

# Purpose: Plot creation, intersection, and grid observers.

# This peer is invoked by register_venn_observers() in registration order.
# evalq deliberately installs observers in the coordinator execution environment
# to preserve shared state, lexical lookup, and nested observeEvent behavior.
# =============================================================================

register_venn_plot_interaction_observers <- function(observer_env) {
  evalq({
  # ---------------------------------------------------------------------------
  # Create-Plot Observer
  # ---------------------------------------------------------------------------
  # Evaluates venn_data_cache() and, when the result is non-NULL, activates
  # the render gate by setting state$plot_active(TRUE).
  #
  # Root cause of the two-click bug (now fixed in venn_data_cache):
  #   venn_data_cache was defined with ignoreInit = TRUE. In Shiny's
  #   eventReactive implementation, ignoreInit = TRUE works by tracking an
  #   "initialized" flag. The first time the event fires, the flag is set to
  #   TRUE and the function returns early WITHOUT incrementing the internal
  #   counter, so the body never runs. Only the second click actually
  #   increments the counter and evaluates the body.
  #
  #   The fix is to use ignoreNULL = TRUE without ignoreInit = TRUE.
  #   ignoreNULL = TRUE is sufficient to suppress evaluation at app startup:
  #   actionButton starts at 0, and Shiny's isNullEvent() treats 0 the same
  #   as NULL, so the eventReactive will not fire before the user clicks.
  #
  #   state$plot_active is only set when the cache returns non-NULL, so an
  #   empty-list click still shows the error notification without activating
  #   the render gate.

  observeEvent(input$create_plot_Venn, {
    debug_log("Create plot button pressed", 1)
    state$restored_plot_cache(NULL)
    if (inherits(rv$data_mod, "data.frame") && inherits(rv$data_def, "data.frame")) {
      state$restore_plot_data_cache(list(data_mod = rv$data_mod, data_def = rv$data_def))
    }
    cache_val <- venn_data_cache()
    if (!is.null(cache_val)) {
      state$plot_active(TRUE)
    }
  }, ignoreNULL = TRUE)

  # ---------------------------------------------------------------------------
  # Reset controls with static UI defaults
  # ---------------------------------------------------------------------------

  observeEvent(input$resetButton_Venn, {
    tryCatch({
      debug_log("Resetting Venn controls to UI defaults", 1)

      # General options with static defaults. Metadata-populated inputs that
      # start with choices = NULL are intentionally not touched here.
      updateSelectInput(session, "diagramType_Venn", selected = "Venn")

      # Venn-specific customization controls.
      updateCheckboxInput(session, "showPercentages_Venn", value = FALSE)
      updateCheckboxInput(session, "showListTitles_Venn", value = TRUE)
      updateNumericInput(session, "overlapNumberSize_Venn", value = 1.5)
      updateNumericInput(session, "listTitleSize_Venn", value = 1.5)
      updateNumericInput(session, "listTitleDistance_Venn", value = 0.05)
      updateSelectInput(session, "catFont_Venn", selected = "sans")
      updateSelectInput(session, "cat_FontStyle_Venn", selected = "plain")
      updateSelectInput(session, "font_family_Venn", selected = "sans")
      updateSelectInput(session, "fontStyle_Venn", selected = "plain")

      # UpSet-specific customization controls.
      updateCheckboxInput(session, "showDotsInBoxplot_Venn", value = TRUE)
      updateSelectInput(session, "ThemeSelect_Upset", selected = "Minimal")
      updateNumericInput(session, "axis_title_size_Venn", value = 20)
      updateNumericInput(session, "axis_text_size_Venn", value = 18)
      updateNumericInput(session, "label_text_size_Venn", value = 10)

      showNotification("Venn controls reset to defaults.", type = "message", duration = 3)
      debug_log("Venn controls reset to UI defaults", 1)
    }, error = function(e) {
      debug_log(paste("Error resetting Venn controls:", e$message), 1)
      showNotification("Error resetting Venn controls", type = "error", duration = 3)
    })
  })

  # Top-level renderPlot: depends on generatePlot_Venn() reactive so that it
  # automatically re-renders whenever styling inputs change after the first
  # plot has been generated (plot_active gate prevents rendering beforehand).
  output$plotOutput_Venn <- renderPlot({
    req(state$plot_active())
    plot_obj <- generatePlot_Venn()
    req(plot_obj)

    tryCatch({
      # At this point generatePlot_Venn() succeeded, which means
      # venn_data_cache() returned non-NULL. Use isolate() to read the
      # cached type without adding a new reactive dependency.
      cached <- isolate(state$restored_plot_cache() %||% venn_data_cache())
      plot_type <- if (!is.null(cached) && !is.null(cached$type)) cached$type else "Venn"

      if (plot_type == "Venn") {
        if (is.list(plot_obj) && length(plot_obj) > 0) {
          debug_log("Rendering Venn diagram with grid graphics", 2)
          grid::grid.newpage()
          grid::grid.draw(plot_obj)
        } else {
          debug_log("Invalid Venn plot object", 1)
          plot.new()
          text(0.5, 0.5, "Venn diagram rendering failed", cex = 1.2,
               adj = 0.5)
        }
      } else {
        debug_log("Rendering UpSet plot", 2)
        print(plot_obj)
      }
    }, error = function(e) {
      debug_log(paste("Error rendering plot:", e$message), 1)
      plot.new()
      text(0.5, 0.5, paste("Plot rendering error:", e$message), cex = 1,
           adj = 0.5)
    })
  })

  # Keep state$current_venn_plot in sync with the latest generated plot so
  # that the grid-integration observer always has the most current version.
  observe({
    if (!isTRUE(state$plot_active())) return()
    plot_obj <- generatePlot_Venn()
    if (!is.null(plot_obj)) {
      state$current_venn_plot(plot_obj)
      # Use isolate() to read the cached type without creating a new reactive
      # dependency on venn_data_cache (the observer already depends on it
      # transitively via generatePlot_Venn).
      cached <- isolate(state$restored_plot_cache() %||% venn_data_cache())
      type   <- if (!is.null(cached) && !is.null(cached$type)) {
        cached$type
      } else {
        debug_log("Could not determine plot type from cache, defaulting to Venn", 1)
        "Venn"
      }
      state$current_plot_type(type)
      debug_log(paste("Plot stored for grid integration, type:",
                      state$current_plot_type()), 2)
    }
  })

  # ---------------------------------------------------------------------------
  # Dynamic Plot-Container Sizing
  # ---------------------------------------------------------------------------

  last_plot_container_signature <- reactiveVal(NULL)

  observe({
    venn_ui_visible <- is_dynamic_venn_ui_visible()
    restore_active <- isTRUE(restore_poll_active())
    current_plot    <- state$current_venn_plot()
    restored_cache  <- state$restored_plot_cache()
    cache_snapshot  <- isolate(venn_data_cache())
    venn_plot_created <- isTRUE(state$plot_active()) ||
      !is.null(current_plot) ||
      !is.null(restored_cache) ||
      !is.null(cache_snapshot)

    if (!venn_ui_visible && !venn_plot_created && !restore_active) {
      debug_log("Skipping plot container sizing until Venn is visible, a plot exists, or restore is active", 2)
      return()
    }

    list_count <- state$list_count_Venn()
    # Keep the create button as a dependency so the container can be initialized
    # immediately after the first plot request, but do not use the selected input
    # type to resize an already-created/restored plot.
    create_trigger <- input$create_plot_Venn
    current_type <- state$current_plot_type()
    restored_type <- if (!is.null(restored_cache) && !is.null(restored_cache$type)) {
      restored_cache$type
    } else {
      NULL
    }
    cached_type <- if (!is.null(cache_snapshot) && !is.null(cache_snapshot$type)) {
      cache_snapshot$type
    } else {
      NULL
    }
    diagram_type <- current_type %||% restored_type %||% cached_type
    if (is.null(diagram_type) && !venn_plot_created) {
      diagram_type <- input$diagramType_Venn
    }
    diagram_type <- diagram_type %||% "Venn"

    debug_log("Adjusting plot container and download width", 2)

    if (!is.null(diagram_type) && diagram_type != "Venn") {
      actual_intersections <- isolate(state$num_intersections_export())

      if (!is.null(actual_intersections) && actual_intersections > 0) {
        num_intersections <- actual_intersections
        debug_log(paste("Using actual intersections:", num_intersections), 2)
      } else if (!is.null(list_count) && list_count > 0) {
        num_intersections <- calculate_expected_intersections(list_count)
        debug_log(paste("Using estimated intersections:", num_intersections,
                        "for", list_count, "lists"), 2)
      } else {
        num_intersections <- 5
        debug_log("Using fallback intersections: 5", 2)
      }

      annotation_count <- if (diagram_type %in% c("UpSet with Abundances",
                                                  "UpSet with Abundance Ratios")) 2L else 1L
      dims <- calculate_upset_plot_dimensions(
        num_intersections = num_intersections,
        num_sets = list_count %||% 1L,
        annotation_count = annotation_count,
        title_size_pt = input$title_text_size_Venn %||% 14
      )
      plot_width            <- dims$plot_width_px
      plot_height           <- dims$plot_height_px
      download_width_inches <- convert_pixels_to_inches(plot_width)
      download_height_inches <- convert_pixels_to_inches(plot_height)
      sizing_signature <- paste(diagram_type, list_count, num_intersections,
                                plot_width, plot_height,
                                download_width_inches, download_height_inches,
                                sep = "|")
      if (identical(last_plot_container_signature(), sizing_signature)) {
        debug_log("Skipping unchanged plot container sizing update", 2)
        return()
      }

      saved_width_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("width_plot_Venn", download_width_inches))[1])
      saved_height_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("height_plot_Venn", download_height_inches))[1])
      if (!is.finite(saved_width_inches)) saved_width_inches <- download_width_inches
      if (!is.finite(saved_height_inches)) saved_height_inches <- download_height_inches
      updateNumericInput(session, inputId = "width_plot_Venn",
                         value = saved_width_inches)
      updateNumericInput(session, inputId = "height_plot_Venn",
                         value = saved_height_inches)
      update_last_plot_dimensions(width = saved_width_inches, height = saved_height_inches,
                                  ppi = input$ppi_plot_Venn, format = input$format_file_Venn)
      debug_log(paste("Updated download dimensions to",
                      round(saved_width_inches, 2), "x",
                      round(saved_height_inches, 2), "inches"), 2)

      if (needs_horizontal_scroll(plot_width)) {
        debug_log("Creating scrollable UpSet plot container", 2)
        output$plotContainer_Venn <- renderUI({
          div(
            style = "overflow-x: auto; width: 100%; border: 1px solid #ddd;",
            plotOutput(ns("plotOutput_Venn"),
                       width = paste0(plot_width, "px"), height = paste0(plot_height, "px"))
          )
        })
      } else {
        debug_log("Creating fixed-width UpSet plot container", 2)
        output$plotContainer_Venn <- renderUI({
          div(
            style = "width: 100%;",
            plotOutput(ns("plotOutput_Venn"),
                       width = paste0(plot_width, "px"), height = paste0(plot_height, "px"))
          )
        })
      }
      last_plot_container_signature(sizing_signature)
    } else {
      plot_width <- "100%"
      plot_height <- "900px"
      num_intersections <- NA_integer_
      download_width_inches <- 14
      download_height_inches <- convert_pixels_to_inches(900)
      sizing_signature <- paste(diagram_type, list_count, num_intersections,
                                plot_width, plot_height,
                                download_width_inches, download_height_inches,
                                sep = "|")
      if (identical(last_plot_container_signature(), sizing_signature)) {
        debug_log("Skipping unchanged plot container sizing update", 2)
        return()
      }

      debug_log("Creating standard Venn plot container", 2)
      output$plotContainer_Venn <- renderUI({
        plotOutput(ns("plotOutput_Venn"), height = plot_height, width = plot_width)
      })
      saved_width_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("width_plot_Venn", download_width_inches))[1])
      saved_height_inches <- suppressWarnings(as.numeric(get_cached_plot_dimension("height_plot_Venn", download_height_inches))[1])
      if (!is.finite(saved_width_inches)) saved_width_inches <- download_width_inches
      if (!is.finite(saved_height_inches)) saved_height_inches <- download_height_inches
      updateNumericInput(session, inputId = "width_plot_Venn", value = saved_width_inches)
      updateNumericInput(session, inputId = "height_plot_Venn", value = saved_height_inches)
      update_last_plot_dimensions(width = saved_width_inches, height = saved_height_inches,
                                  ppi = input$ppi_plot_Venn, format = input$format_file_Venn)
      last_plot_container_signature(sizing_signature)
    }
  })

  # ---------------------------------------------------------------------------
  # Intersection Dropdown Management
  # ---------------------------------------------------------------------------

  observe({
    intersection_data <- state$intersection_list()

    if (!has_intersection_dropdown_readiness(intersection_data)) {
      log_intersection_dropdown_not_ready()
      return()
    }

    if (!is.null(intersection_data) && length(intersection_data) > 0) {
      debug_log(paste("Updating intersection dropdown with",
                      length(intersection_data), "intersections"), 2)
      updateSelectInput(session, "intersection_dropdown",
                        choices  = names(intersection_data),
                        selected = names(intersection_data)[1])
    } else {
      debug_log("No intersection data available for dropdown", 2)
      updateSelectInput(session, "intersection_dropdown", choices = NULL)
    }
  })

  observeEvent(input$intersection_dropdown, {
    intersection_data <- state$intersection_list()

    if (!has_intersection_dropdown_readiness(intersection_data)) {
      log_intersection_dropdown_not_ready()
      return()
    }

    selected_name     <- input$intersection_dropdown

    if (!is.null(intersection_data) && !is.null(selected_name) &&
        selected_name %in% names(intersection_data)) {
      debug_log(paste("Intersection dropdown changed to:", selected_name), 2)
      selected_proteins <- intersection_data[[selected_name]]
      updateTextAreaInput(session, "selectedIntersection",
                          value = paste(selected_proteins, collapse = "\n"))
      debug_log(paste("Updated intersection results with",
                      length(selected_proteins), "proteins"), 2)
    } else {
      debug_log("No valid intersection selected or data missing", 2)
      updateTextAreaInput(session, "selectedIntersection", value = "")
    }
  })

  observeEvent(input$copy_intersection_Venn, {
    tryCatch({
      intersection_text <- input$selectedIntersection %||% ""
      intersection_text <- trimws(intersection_text)

      if (nzchar(intersection_text)) {
        identifier <- unlist(strsplit(intersection_text, "\r?\n"))
        identifier <- identifier[nzchar(trimws(identifier))]
        copy_to_clipboard(paste(identifier, collapse = "\n"), debug_log)
        debug_log(paste("Copied", length(identifier), "intersection identifiers to clipboard"), 1)
        showNotification(paste("Copied", length(identifier), "identifiers to clipboard"),
                         type = "message", duration = 3)
      } else {
        showNotification("No intersection identifiers available to copy", type = "warning")
      }
    }, error = function(e) {
      debug_log(paste("Error in copy_intersection_Venn:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error")
    })
  })

  # ---------------------------------------------------------------------------
  # Grid Integration
  # ---------------------------------------------------------------------------

  observeEvent(input$add_to_grid, {
    debug_log("Venn: add_to_grid clicked", 1)

    current_plot_obj <- tryCatch(state$current_venn_plot(), error = function(e) {
      debug_log(paste("Venn: error accessing plot data:", e$message), 1)
      NULL
    })
    current_type <- tryCatch(state$current_plot_type(), error = function(e) NULL)

    if (is.null(current_plot_obj)) {
      showNotification(
        "No diagram available to add. Please create a Venn diagram or UpSet plot first.",
        type = "error"
      )
      debug_log("Venn: current_venn_plot is NULL", 1)
      return()
    }

    if (is.null(current_type)) {
      current_type <- "Venn"
      debug_log("Venn: using fallback plot type", 2)
    }

    required_packages <- c("png", "ggplot2", "grid")
    missing_packages  <- required_packages[
      !sapply(required_packages, requireNamespace, quietly = TRUE)
    ]
    if (length(missing_packages) > 0) {
      showNotification(
        paste("Required packages missing for grid integration:",
              paste(missing_packages, collapse = ", ")),
        type = "error"
      )
      debug_log(paste("Venn: missing packages:",
                      paste(missing_packages, collapse = ", ")), 1)
      return()
    }

    if (!exists("modEnv") || !exists("add_to_grid", envir = modEnv)) {
      debug_log("Venn: modEnv or add_to_grid function not available", 1)
      showNotification("Grid system not properly initialized.", type = "error")
      return()
    }

    p <- tryCatch(
      convert_venn_to_ggplot(current_plot_obj, current_type, debug_log,
                             cached_mode = isTRUE(is.list(state$restore_plot_data_cache()))),
      error = function(e) {
        debug_log(paste("Venn: error converting to ggplot:", e$message), 1)
        showNotification("Error converting diagram to ggplot format.",
                         type = "error")
        NULL
      }
    )

    if (is.null(p)) {
      showNotification("Failed to convert diagram plot.", type = "error")
      return()
    }

    if (!inherits(p, "ggplot")) {
      showNotification("Diagram conversion did not produce ggplot object.",
                       type = "error")
      debug_log("Venn: converted object is not a ggplot", 1)
      return()
    }

    sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
    lbl_raw  <- input$grid_label
    lbl_id   <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default"
                else sanitize(lbl_raw)
    plot_id  <- paste0(ns(""), "Venn_", lbl_id)
    lbl_vis  <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw
                else current_type

    tryCatch({
      debug_log(paste("Venn: adding to grid id=", plot_id), 2)
      debug_log(paste("Venn: plot type=", current_type,
                      "lists=", state$list_count_Venn()), 2)
      source_tag <- if (grepl("upset", tolower(current_type %||% ""))) {
        "Venn-UpSet"
      } else {
        "Venn"
      }
      modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis,
                         source = source_tag)
      showNotification(
        paste(current_type, "diagram added to grid selection as static plot."),
        type = "message"
      )
    }, error = function(e) {
      debug_log(paste("Venn: error adding to grid:", e$message), 1)
      showNotification("Error adding diagram to grid. Check console for details.",
                       type = "error")
    })
  })

  }, envir = observer_env)
  invisible(NULL)
}
