restore_go_plot_from_recreation_state <- function(recreation_state) {
  if (is.null(recreation_state) || !is.list(recreation_state)) {
    return(invisible(FALSE))
  }

  go_results <- tryCatch(isolate(GO_Result_List()), error = function(e) NULL)
  if (is.null(go_results) || is.null(go_results$Edo_GO)) {
    debug_log("GO plot recreation skipped: no restorable enrichResult is available", 1)
    return(invisible(FALSE))
  }

  result_df <- tryCatch(go_results$Edo_GO@result, error = function(e) NULL)
  if (is.null(result_df) || nrow(result_df) == 0) {
    debug_log("GO plot recreation skipped: restored GO results are empty", 1)
    return(invisible(FALSE))
  }

  plot_type <- recreation_state$plot_type %||% recreation_state$visualization_mode %||%
    isolate(input$custom_EnrichPlot_select_GO %||% "Enrichment score dotplot")
  selected_terms <- recreation_state$selected_go_terms %||% recreation_state$selected_terms %||% character(0)
  selected_terms <- as.character(selected_terms[!is.na(selected_terms) & nzchar(trimws(selected_terms))])
  available_terms <- if ("Description" %in% colnames(result_df)) {
    terms <- as.character(result_df$Description)
    terms[!is.na(terms) & nzchar(trimws(terms))]
  } else character(0)
  if (length(selected_terms) == 0 || length(available_terms) == 0 || !all(selected_terms %in% available_terms)) {
    debug_log("GO plot recreation skipped: restored selected terms unavailable", 1)
    return(invisible(FALSE))
  }

  colors <- recreation_state$plot_config_colors %||% c("#440154FF", "#31688EFF", "#EFC000FF")
  sizes <- recreation_state$plot_config_sizes %||% list(axisTitle = 12, tick = 10, legendText = 10, legendTitle = 12, label = 12)
  theme_name <- recreation_state$theme_name %||% "Black and White"
  theme <- get_selected_theme_go(theme_name)
  legend_position <- recreation_state$legend_position %||% "right"
  plot_height_val <- as.numeric(recreation_state$plot_height %||% 600)
  if (is.na(plot_height_val) || plot_height_val < 200) plot_height_val <- 600

  plot_result <- tryCatch({
    switch(plot_type,
           "Enrichment score dotplot" = create_go_dotplot(go_results$Edo_GO, selected_terms, colors, sizes, theme, legend_position),
           "Cnet plot (log2FC)" = create_go_cnet_plot_fc_fixed(go_results, selected_terms, colors, sizes, theme, legend_position),
           "Enrichment map" = create_go_enrichment_map_fixed(go_results, selected_terms, colors, sizes, theme, legend_position),
           "Pubmed citations" = create_go_pubmed_plot(selected_terms, colors, sizes, theme, legend_position),
           list(plot = NULL, height = plot_height_val, width = recreation_state$plot_width %||% 800, message = paste("Plot type not implemented:", plot_type))
    )
  }, error = function(e) {
    debug_log(paste("GO plot recreation failed:", e$message), 1)
    NULL
  })

  if (is.null(plot_result) || is.null(plot_result$plot)) {
    return(invisible(FALSE))
  }

  current_plot_object(plot_result$plot)
  current_plot_message(plot_result$message %||% paste("Restored GO", plot_type, "plot from saved settings"))
  current_plot_height(plot_result$height %||% plot_height_val)
  current_plot_width(plot_result$width %||% recreation_state$plot_width %||% 800)
  plot_config$colors <- colors
  plot_config$sizes <- sizes
  if (!is.null(recreation_state$plot_config_dims)) plot_config$plot_dimensions <- recreation_state$plot_config_dims
  plot_config$theme <- theme
  rv$go_selected_terms <- selected_terms
  debug_log("GO plot recreated from restored session UI state", 1)
  invisible(TRUE)
}

# ==============================================================================
# 6. Plot Creation Observer
# ==============================================================================

observeEvent(input$create_go_plot, {

  debug_log("Starting ENHANCED GO plot creation with tree integration", 1)

  tryCatch({

    go_results <- NULL
    tryCatch({
      req(GO_Result_List())
      go_results <- isolate(GO_Result_List())
      debug_log("GO results validated successfully", 2)
    }, error = function(e) {
      debug_log("No GO results available", 1)
      showNotification("No GO results available. Please run GO analysis first.",
                       type = "error", duration = 5)
      return()
    })

    go_result_df <- tryCatch(.safe_go_result_df(go_results), error = function(e) NULL)
    if (is.null(go_result_df) || nrow(go_result_df) == 0) {
      debug_log("Invalid or empty GO results", 1)
      showNotification("No significant GO terms found. Try adjusting p-value cutoffs.",
                       type = "warning", duration = 5)
      return()
    }
    edo_for_plot <- if (!is.null(go_results$Edo_GO)) go_results$Edo_GO else go_result_df

    debug_log(paste("GO results validated:", nrow(go_result_df), "terms available"), 1)

    plot_type <- isolate(input$custom_EnrichPlot_select_GO)
    if (is.null(plot_type) || plot_type == "") {
      plot_type <- "Enrichment score dotplot"
    }
    debug_log(paste("Plot type:", plot_type), 1)

    selected_terms <- selected_go_terms()

    if (length(selected_terms) == 0) {
      debug_log("No terms available after robust selection", 1)
      showNotification("No GO terms available for plotting. Please check your analysis results.",
                       type = "warning", duration = 5)
      return()
    }

    debug_log(paste("Selected", length(selected_terms), "terms for plotting"), 1)
    debug_log(paste("Terms:", paste(head(selected_terms, 3), collapse = ", "),
                    if (length(selected_terms) > 3) "..." else ""), 2)

    colors <- tryCatch({
      if (plot_type %in% c("Cnet plot (log2FC)", "Enrichment map", "Enrichment score dotplot")) {
        c(
          isolate(input$GOColorInput_down %||% "#440154FF"),
          isolate(input$GOColorInput_zero %||% "#31688EFF"),
          isolate(input$GOColorInput_up   %||% "#EFC000FF")
        )
      } else {
        c("#440154FF", "#31688EFF", "#EFC000FF")
      }
    }, error = function(e) {
      debug_log(paste("Error getting colors:", e$message), 1)
      c("#440154FF", "#31688EFF", "#EFC000FF")
    })

    sizes <- tryCatch({
      list(
        axisTitle   = as.numeric(isolate(input$AxisTitleSize_GO   %||% 12)),
        tick        = as.numeric(isolate(input$tickSize_GO        %||% 10)),
        legendText  = as.numeric(isolate(input$LegendTextSize_GO  %||% 10)),
        legendTitle = as.numeric(isolate(input$LegendTitleSize_GO %||% 12)),
        label       = as.numeric(isolate(input$LabelSize_GO       %||% 12))
      )
    }, error = function(e) {
      debug_log(paste("Error getting sizes:", e$message), 1)
      list(axisTitle = 12, tick = 10, legendText = 10, legendTitle = 12, label = 12)
    })

    theme <- tryCatch({
      theme_name <- isolate(input$ThemeSelect_GO %||% "Black and White")
      get_selected_theme_go(theme_name)
    }, error = function(e) {
      debug_log(paste("Error getting theme:", e$message), 1)
      theme_bw()
    })

    legend_position <- tryCatch({
      isolate(input$LegendPosition_GO %||% "right")
    }, error = function(e) {
      debug_log(paste("Error getting legend position:", e$message), 1)
      "right"
    })

    plot_height_val <- as.numeric(isolate(input$plot_height_go %||% 600))
    if (is.na(plot_height_val) || plot_height_val < 200) {
      plot_height_val <- 600
    }

    debug_log(paste("Parameters ready - Colors:", length(colors), "Theme class:", class(theme), "Legend position:", legend_position), 2)

    plot_result <- tryCatch({

      switch(plot_type,
             "Enrichment score dotplot" = {
               create_go_dotplot(edo_for_plot, selected_terms, colors, sizes, theme, legend_position)
             },
             "Cnet plot (log2FC)" = {
               create_go_cnet_plot_fc_fixed(go_results, selected_terms, colors, sizes, theme, legend_position)
             },
             "Enrichment map" = {
               create_go_enrichment_map_fixed(go_results, selected_terms, colors, sizes, theme, legend_position)
             },
             "Pubmed citations" = {
               withProgress(message = "Creating GO PubMed Citation Plot...", value = 0, {
                 create_go_pubmed_plot(
                   selected_terms, colors, sizes, theme, legend_position,
                   progress_fn = function(value, detail) {
                     setProgress(value = value, detail = detail)
                   }
                 )
               })
             },
             {
               debug_log(paste("Unknown plot type:", plot_type), 1)
               list(plot = NULL, height = 600, width = 800, message = paste("Plot type not implemented:", plot_type))
             }
      )

    }, error = function(e) {
      debug_log(paste("Error in plot creation:", e$message), 1)
      list(
        plot    = NULL,
        message = paste("Plot creation failed:", e$message),
        height  = plot_height_val,
        width   = 800
      )
    })

    if (!is.null(plot_result)) {

      if (!is.null(plot_result$plot)) {

        debug_log("Plot created successfully, rendering UI", 1)

        output$GOplot_container <- renderUI({
          tagList(
            div(
              style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; padding: 10px; margin-bottom: 10px; color: #155724;",
              icon("check-circle"), " ", plot_result$message %||% paste("GO", plot_type, "created successfully")
            ),
            plotOutput(ns("GOplot_custom"),
                       height = paste0(plot_height_val, "px"),
                       width  = "100%")
          )
        })

        output$GOplot_custom <- renderPlot({
          tryCatch({
            go_plot_output_rendered(TRUE)
            print(plot_result$plot)
          }, error = function(e) {
            debug_log(paste("Error rendering plot:", e$message), 1)
            plot.new()
            text(0.5, 0.5, paste("Error rendering plot:", e$message),
                 cex = 1.2, adj = 0.5, col = "red")
          })
        }, height = plot_height_val)

        tryCatch({
          if (exists("current_plot_object") && is.function(current_plot_object)) {
            current_plot_object(plot_result$plot)
          }
          if (exists("current_plot_height") && is.function(current_plot_height)) {
            current_plot_height(plot_result$height %||% plot_height_val)
          }
          if (exists("current_plot_width") && is.function(current_plot_width)) {
            current_plot_width(plot_result$width %||% 800)
          }
        }, error = function(e) {
          debug_log(paste("Error updating reactive values:", e$message), 2)
        })

        showNotification(paste("GO", plot_type, "created successfully with", length(selected_terms), "terms!"),
                         type = "message", duration = 3)

      } else {

        debug_log("Plot creation returned NULL", 1)

        error_message <- plot_result$message %||% "Plot creation failed for unknown reason"

        output$GOplot_container <- renderUI({
          div(
            style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 10px; margin-bottom: 10px; color: #721c24;",
            icon("exclamation-triangle"), " ", error_message
          )
        })

        showNotification(error_message, type = "warning", duration = 5)
      }

    } else {

      debug_log("Plot result is NULL", 1)

      output$GOplot_container <- renderUI({
        div(
          style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 10px; margin-bottom: 10px; color: #721c24;",
          icon("exclamation-triangle"), " Plot creation failed - no result returned"
        )
      })

      showNotification("Plot creation failed. Please try again.", type = "error", duration = 5)
    }

  }, error = function(e) {

    debug_log(paste("Critical error in plot creation observer:", e$message), 1)

    output$GOplot_container <- renderUI({
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 20px; margin-bottom: 10px; color: #721c24;",
        icon("exclamation-triangle"),
        h5("Critical Plot Creation Error"),
        p(paste("An error occurred:", e$message)),
        p("Please check your GO analysis results and try again."),
        pre(style = "background-color: #f5f5f5; padding: 5px; font-size: 10px; margin-top: 10px;",
            paste("Debug info:", e$message))
      )
    })

    showNotification(paste("Critical error:", e$message), type = "error", duration = 8)
  })
})

# ==============================================================================
# 7. Tree Rendering and Term Selection
# ==============================================================================

output$goTree <- renderTree({
  req(go_tree_structure())
  tree_struct <- go_tree_structure()
  debug_log("Rendering GO tree with structure", 2)
  tree_struct
})

selected_go_terms <- reactive({

  tryCatch({
    if (!isTRUE(go_results_ready_for_fallback()) && is.null(go_tree_structure())) {
      return(character(0))
    }

    tree_input <- input$goTree
    if (is.null(tree_input)) {
      return(isolate(rv$go_selected_terms %||% character(0)))
    }

    max_terms  <- input$max_terms_GO %||% 10

    debug_log("Processing tree selection", 2)
    debug_log(paste("Maximum terms allowed:", max_terms), 2)

    selected <- character(0)

    if (length(tree_input) > 0) {
      debug_log("Tree input available - extracting selections", 2)

      selected <- extract_selected_terms_hierarchical(tree_input, debug_log = debug_log)
      debug_log(paste("Selected", length(selected), "terms from tree"), 1)

      if (length(selected) > max_terms && !is.null(GO_Result_List())) {
        debug_log(paste("Filtering to", max_terms, "most significant terms"), 1)

        selected <- filter_terms_by_pvalue(
          terms      = selected,
          go_results = GO_Result_List(),
          max_terms  = max_terms,
          debug_log  = debug_log
        )

        debug_log(paste("After filtering:", length(selected), "terms"), 1)

        showNotification(
          paste("Selected", length(selected), "most significant terms out of your selection."),
          type = "message",
          duration = 4
        )
      }
    }

    if (length(selected) == 0) {
      restored_selection <- isolate(rv$go_selected_terms %||% character(0))
      restored_selection <- as.character(restored_selection)
      restored_selection <- restored_selection[!is.na(restored_selection) & nzchar(trimws(restored_selection))]
      if (length(restored_selection) > 0) {
        selected <- restored_selection
        debug_log(paste("Using restored GO term selection fallback:", length(selected), "terms"), 1)
      }
    }

    if (length(selected) > 0) {
      debug_log(paste("Final selection:", paste(head(selected, 2), collapse = ", "), "..."), 2)
    } else {
      debug_log("No terms available after all selection attempts", 1)
    }

    return(selected)

  }, error = function(e) {
    debug_log(paste("Critical error in selected_go_terms reactive:", e$message), 1)
    return(character(0))
  })
})

# Synchronize tree selection with selectInput and shared rv state
observeEvent(selected_go_terms(), {
  tree_selection <- selected_go_terms()

  rv$go_selected_terms <- tree_selection

  if (length(tree_selection) > 0) {
    debug_log(paste("Updating selectInput with", length(tree_selection), "terms from tree"), 2)
    updateSelectInput(session, "custom_Enrich_select_GO", selected = tree_selection)
  }
}, ignoreNULL = FALSE, ignoreInit = TRUE)

# ==============================================================================
# 8. Plot Rendering Outputs
# ==============================================================================

output$GOplot_container <- renderUI({
  req(current_plot_object())
  plot_update_trigger()

  height <- current_plot_height() %||% 600
  width  <- current_plot_width()  %||% 800

  debug_log(paste("FIXED: Rendering plot container -", width, "x", height), 2)

  tagList(
    if (!is.null(current_plot_message()) && current_plot_message() != "") {
      div(style = "margin-bottom:10px; padding:5px; background-color:#f8f9fa; border-radius:3px;",
          current_plot_message())
    },
    plotOutput(
      ns("GOplot_custom"),
      height = paste0(height, "px"),
      width  = "100%"
    )
  )
})

output$GOplot_custom <- renderPlot({
  req(current_plot_object())
  plot_update_trigger()

  plot_obj <- current_plot_object()
  debug_log("FIXED: Rendering custom plot", 2)

  if (!is.null(plot_obj)) {
    go_plot_output_rendered(TRUE)
    print(plot_obj)
  } else {
    plot.new()
    text(0.5, 0.5, "Plot not available", cex = 1.2, adj = 0.5)
  }
}, height = function() current_plot_height() %||% 600)

output$GOplot_1 <- renderUI({
  plotOutput(ns("GOplot_1_actual"),
             height = paste0(current_plot_height(), "px"),
             width  = "100%")
})

output$GOplot_1_actual <- renderPlot({
  plot_obj <- current_plot_object()

  if (!is.null(plot_obj)) {
    go_plot_output_rendered(TRUE)
    print(plot_obj)
  } else {
    plot.new()
    text(0.5, 0.5, "GO analysis results will appear here after analysis.", cex = 1.2, adj = 0.5)
  }
})

# ==============================================================================
# 9. Download Dimension Tracking Observers and Outputs
# ==============================================================================

observeEvent(input$plotWidthInch_GO, {
  pending_width <- pending_programmatic_width()
  if (is_pending_programmatic_dimension(input$plotWidthInch_GO, pending_width)) {
    pending_programmatic_width(NULL)
    debug_log(paste("DOWNLOAD: Ignored programmatic width change to:", input$plotWidthInch_GO), 2)
    return(invisible(NULL))
  }

  last_manual_input_time(Sys.time())
  debug_log(paste("DOWNLOAD: Manual width change to:", input$plotWidthInch_GO), 2)
}, ignoreInit = TRUE)

observeEvent(input$plotHeightInch_GO, {
  pending_height <- pending_programmatic_height()
  if (is_pending_programmatic_dimension(input$plotHeightInch_GO, pending_height)) {
    pending_programmatic_height(NULL)
    debug_log(paste("DOWNLOAD: Ignored programmatic height change to:", input$plotHeightInch_GO), 2)
    return(invisible(NULL))
  }

  last_manual_input_time(Sys.time())
  debug_log(paste("DOWNLOAD: Manual height change to:", input$plotHeightInch_GO), 2)
}, ignoreInit = TRUE)

observeEvent(c(current_plot_height(), current_plot_width()), {
  req(current_plot_height(), current_plot_width())

  if (!should_initialize_download_dimensions()) {
    set_default_download_dimensions_silently()
    return(invisible(NULL))
  }

  debug_log("DOWNLOAD: Plot dimensions changed, updating download panel", 2)
  update_download_dimensions()
}, ignoreInit = TRUE)

observeEvent(c(input$window_inner_height, input$window_inner_width, input$devicePixelRatio), {
  req(input$window_inner_height, input$window_inner_width, input$devicePixelRatio)

  rv$height_px <- input$window_inner_height
  rv$width_px  <- input$window_inner_width
  rv$px_ratio  <- input$devicePixelRatio

  rv$height_inch <- round(input$window_inner_height / 96, 2)
  rv$width_inch  <- round(input$window_inner_width  / 96, 2)

  if (!should_initialize_download_dimensions()) {
    set_default_download_dimensions_silently()
    return(invisible(NULL))
  }

  debug_log(paste("DOWNLOAD: Window size changed - Height:", input$window_inner_height, "px (", rv$height_inch, "in), Width:", input$window_inner_width, "px (", rv$width_inch, "in)"), 2)

  update_download_dimensions()

}, ignoreInit = TRUE)

output$download_info_GO <- renderText({
  req(input$plotWidthInch_GO, input$plotHeightInch_GO, input$resolution_DPI_GO)

  width_px  <- round(input$plotWidthInch_GO  * input$resolution_DPI_GO)
  height_px <- round(input$plotHeightInch_GO * input$resolution_DPI_GO)

  paste0("Download size: ", width_px, " × ", height_px, " pixels")
})

# ==============================================================================
# 10. Grid Integration Observer
# ==============================================================================

observeEvent(input$add_to_grid, {
  debug_log("GO: add_to_grid clicked", 2)

  if (is.null(current_plot_object())) {
    showNotification("No plot available to add. Please create a GO plot first.", type = "error")
    debug_log("GO: current_plot_object is NULL", 1)
    return()
  }

  p <- tryCatch({
    current_plot_object()
  }, error = function(e) {
    debug_log(paste("GO: error accessing plot:", e$message), 1)
    NULL
  })

  if (is.null(p)) {
    showNotification("No plot available to add.", type = "error")
    return()
  }

  if (!inherits(p, "ggplot")) {
    showNotification("Only ggplot objects can be added to the grid (Phase 1).", type = "error")
    debug_log("GO: current plot is not a ggplot", 1)
    return()
  }

  sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
  lbl_raw  <- input$grid_label
  lbl_id   <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
  plot_id  <- paste0(ns(""), "GO_", lbl_id)

  lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "GO"

  debug_log(paste("GO: adding to grid id=", plot_id), 2)
  modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "GO")
  showNotification("Added to grid selection.", type = "message")
})

# ==============================================================================
# 11. Download Handlers
# ==============================================================================

output$downloadPlotButton_GO <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0("GO_plot_", timestamp, ".", input$downloadFormat_GO)
  },
  content = function(file) {
    req(current_plot_object())

    debug_log("DOWNLOAD: Starting plot download", 1)

    tryCatch({
      width_inches  <- input$plotWidthInch_GO
      height_inches <- input$plotHeightInch_GO
      dpi           <- input$resolution_DPI_GO
      format        <- input$downloadFormat_GO
      plot_obj      <- current_plot_object()

      debug_log(paste("DOWNLOAD: Dimensions -", width_inches, "×", height_inches, "inches at", dpi, "DPI"), 1)
      debug_log(paste("DOWNLOAD: Format:", format), 1)

      if (format %in% c("png", "jpeg", "tiff")) {
        device_function <- match.fun(format)
        device_function(
          filename = file,
          width    = width_inches,
          height   = height_inches,
          units    = "in",
          res      = dpi,
          bg       = "white"
        )
      } else if (format == "svg") {
        svg(
          filename = file,
          width    = width_inches,
          height   = height_inches,
          bg       = "white"
        )
      } else if (format == "pdf") {
        pdf(
          file   = file,
          width  = width_inches,
          height = height_inches,
          bg     = "white"
        )
      }

      print(plot_obj)
      dev.off()

      debug_log("DOWNLOAD: Plot download completed successfully", 1)

      showNotification(
        paste0("Plot downloaded successfully as ", format, " (",
               round(width_inches * dpi), "×", round(height_inches * dpi), " pixels)"),
        type = "message",
        duration = 3
      )

    }, error = function(e) {
      debug_log(paste("DOWNLOAD: Error during download:", e$message), 1)

      tryCatch(dev.off(), error = function(e2) {})

      showNotification(
        paste("Download failed:", e$message),
        type = "error",
        duration = 5
      )
    })
  }
)

output$download_res_go_ui <- renderUI({
  if (!is.null(GO_Result_List()) && !is.null(GO_Result_List()$Edo_GO)) {
    downloadButton(
      ns("download_res_GO"),
      label = "Download res_GO (.rds)",
      class = "btn btn-primary",
      style = "width: 100%;"
    )
  } else {
    tags$button(
      class    = "btn btn-primary",
      style    = "width: 100%;",
      disabled = "disabled",
      "Download res_GO (.rds)"
    )
  }
})

output$download_res_GO <- downloadHandler(
  filename = function() {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0("res_GO_", ts, ".rds")
  },
  content = function(file) {
    if (is.null(GO_Result_List()) || is.null(GO_Result_List()$Edo_GO)) {
      showNotification(
        "No GO results available to download. Please run GO analysis or import a GO .rds file first.",
        type = "error",
        duration = 4
      )
      shiny::req(FALSE)
    }
    default_fname <- paste0("res_GO_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    saveRDS(GO_Result_List(), file = file)
    debug_log(
      sprintf("GO result exported | Default file name: %s", default_fname),
      level = 0
    )
  }
)
