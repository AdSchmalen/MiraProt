# ==============================================================================
# STRING Module - Export and Grid Integration
# ==============================================================================
#
# Purpose:
#   Registers the network export download handler and the "add to grid"
#   observer that converts the STRING network to a ggplot object and
#   registers it with the plot grid module.
#
# Architecture role:
#   Factory function initialized inside modSTRINGServer. Receives the network
#   state (test_g, nodes, edges) and conversion utilities as parameters.
#
# File structure:
#   1. initialize_STRING_export_grid factory
#   2. register_export_grid_observers:
#      a. downloadPlotButton_STRING download handler
#      b. add_to_grid observer
#   3. Return list
#
# Future developers:
#   - All logging uses debug_log passed as a factory parameter.
#   - Grid integration requires the modEnv$add_to_grid function from the
#     plot grid module; check its availability before calling.
# ==============================================================================

initialize_STRING_export_grid <- function(
    input,
    output,
    session,
    ns,
    rv,
    debug_log,
    test_g,
    nodes,
    edges,
    open_STRING_export_device,
    convert_vis_to_igraph,
    convert_string_to_ggplot) {

  register_export_grid_observers <- function() {
    output$downloadPlotButton_STRING <- downloadHandler(
      filename = function() {
        paste("string_network.", input$downloadFormat_STRING, sep = "")
      },
      content = function(file) {
        req(test_g())

        debug_log("Starting network export", 1)

        widthIn <- input$plotWidthInch_STRING
        heightIn <- input$plotHeightInch_STRING
        resolution_DPI_STRING <- input$resolution_DPI_STRING

        debug_log(
          paste(
            "Export parameters: Format =", input$downloadFormat_STRING,
            ", Size =", widthIn, "x", heightIn, "inches",
            ", DPI =", resolution_DPI_STRING
          ),
          2
        )

        tryCatch({
          result_igraph <- convert_vis_to_igraph(
            nodes(),
            edges(),
            size_factor = 0.35,
            debug_log
          )

          g <- result_igraph$graph
          layout <- result_igraph$layout

          debug_log(
            paste("Converted to igraph:", igraph::vcount(g), "nodes,", igraph::ecount(g), "edges"),
            2
          )

          open_STRING_export_device(
            format_selected = input$downloadFormat_STRING,
            file = file,
            width_in = widthIn,
            height_in = heightIn,
            resolution_dpi = resolution_DPI_STRING
          )
          on.exit(try(dev.off(), silent = TRUE), add = TRUE)
          plot(g, layout = layout)

          debug_log("Export completed successfully", 1)

        }, error = function(e) {
          debug_log(paste("Export error:", e$message), 1)
          showNotification(paste("Export failed:", e$message), type = "error")
        })
      }
    )

    observeEvent(input$add_to_grid, {
      debug_log("STRING: add_to_grid clicked", 2)

      current_graph <- NULL
      current_nodes <- NULL
      current_edges <- NULL

      tryCatch({
        current_graph <- test_g()
        current_nodes <- nodes()
        current_edges <- edges()
      }, error = function(e) {
        debug_log(paste("STRING: error accessing network data:", e$message), 1)
      })

      if (is.null(current_graph) || is.null(current_nodes) || is.null(current_edges)) {
        showNotification("No network available to add. Please create a STRING network first.", type = "error")
        debug_log("STRING: network components not available", 1)
        return()
      }

      if (igraph::vcount(current_graph) == 0) {
        showNotification("Network is empty. Cannot add to grid.", type = "error")
        debug_log("STRING: network has 0 nodes", 1)
        return()
      }

      required_packages <- c("png", "ggplot2")
      missing_packages <- character()

      for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          missing_packages <- c(missing_packages, pkg)
        }
      }

      if (length(missing_packages) > 0) {
        showNotification(
          paste("Required packages missing for grid integration:", paste(missing_packages, collapse = ", ")),
          type = "error"
        )
        debug_log(paste("STRING: missing packages:", paste(missing_packages, collapse = ", ")), 1)
        return()
      }

      p <- tryCatch({
        convert_string_to_ggplot(current_graph, current_nodes, current_edges, debug_log)
      }, error = function(e) {
        debug_log(paste("STRING: error converting to ggplot:", e$message), 1)
        showNotification("Error converting network to ggplot format.", type = "error")
        return(NULL)
      })

      if (is.null(p)) {
        showNotification("Failed to convert network plot.", type = "error")
        return()
      }

      if (!inherits(p, "ggplot")) {
        showNotification("Network conversion did not produce ggplot object.", type = "error")
        debug_log("STRING: converted object is not a ggplot", 1)
        return()
      }

      if (!exists("modEnv") || !exists("add_to_grid", envir = modEnv)) {
        debug_log("STRING: modEnv or add_to_grid function not available", 1)
        showNotification("Grid system not properly initialized.", type = "error")
        return()
      }

      sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
      lbl_raw <- input$grid_label
      lbl_id <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
      plot_id <- paste0(ns(""), "STRING_", lbl_id)

      lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else "STRING"

      tryCatch({
        debug_log(paste("STRING: adding to grid id=", plot_id), 2)
        debug_log(
          paste("STRING: network has", igraph::vcount(current_graph), "nodes and", igraph::ecount(current_graph), "edges"),
          2
        )

        modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "STRING")
        showNotification("Network added to grid selection as static plot.", type = "message")

      }, error = function(e) {
        debug_log(paste("STRING: error adding to grid:", e$message), 1)
        showNotification("Error adding network to grid. Check console for details.", type = "error")
      })
    })
  }

  list(
    register_export_grid_observers = register_export_grid_observers
  )
}
