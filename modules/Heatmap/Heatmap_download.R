# ==============================================================================
# Heatmap Module - Download Handlers and Grid Export
# ==============================================================================
#
# Purpose:
#   Implements all download-related output bindings and the Grid export handler
#   for the Heatmap module.
#
# Architecture role:
#   This file is the download layer, sourced last among the Heatmap sub-scripts.
#   It depends on draw_current_tab_exactly_like_ui() from Heatmap_rendering.R
#   and on build_heatmap_htlist_for_tab() defined in this same file.
#
# Structure:
#   1. build_heatmap_htlist_for_tab - builds a HeatmapList for a given tab
#      (used by draw_current_tab_exactly_like_ui and the download handler)
#   2. output$downloadPlotButton_Heatmaps - downloadHandler
#   3. output$current_tab_info - renderText showing the active tab name
#   4. observeEvent(input$add_to_grid) - export current heatmap to Grid module
#
# Important notes for future developers:
#   - draw_current_tab_exactly_like_ui() is defined in Heatmap_rendering.R and
#     must be sourced before this file.
#   - The add_to_grid handler renders the heatmap to a temporary PNG/PDF and
#     wraps it in a ggplot for Grid module compatibility.
#
# Sourced by: Heatmap_module.R (after Heatmap_observers.R)
# ==============================================================================

    # Download/export always reads rebuilt runtime heatmap objects. These
    # objects are intentionally excluded from schema-2.0 session state; restore
    # rehydrates them from ui_inputs + plot_data_cache_ref/matrix_payload first.
    build_heatmap_htlist_for_tab <- function(current_tab) {
      # current_tab: "grid_expr_corr" | "grid_expr_sample" | "expression_tab" | ...
      legend_side <- legend_side_from_input(input)
      legend_dir  <- legend_direction_from_input(input)
      fs <- extract_font_settings(input)

      fixed_expr   <- heatmap_fixed_expression()
      fixed_prot   <- heatmap_fixed_protein_correlation()
      fixed_sample <- heatmap_fixed_sample_correlation()
      fixed_bm     <- heatmap_fixed_basemean()
      fixed_ratio  <- heatmap_fixed_abundance_ratio()

      if (is.null(fixed_expr)) return(NULL)

      spacer_name_counter <- 0L
      next_spacer_name <- function(prefix = "spacer") {
        spacer_name_counter <<- spacer_name_counter + 1L
        paste0(prefix, "_", spacer_name_counter)
      }

      # Helper: create tile-aligned spacer heatmap for horizontal layout
      make_spacer_h <- function(n_tile_rows, n_tile_cols = 2) {
        mat <- matrix(NA_real_, nrow = n_tile_rows, ncol = n_tile_cols)
        ComplexHeatmap::Heatmap(
          mat,
          name = next_spacer_name("spacer_h"),
          col = circlize::colorRamp2(c(0, 1), c("white", "white")),
          na_col = "white",
          show_heatmap_legend = FALSE,
          show_row_names = FALSE,
          show_column_names = FALSE,
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          border = FALSE
        )
      }

      # Helper: create tile-aligned spacer heatmap for vertical layout
      make_spacer_v <- function(n_tile_rows = 2, n_tile_cols) {
        mat <- matrix(NA_real_, nrow = n_tile_rows, ncol = n_tile_cols)
        ComplexHeatmap::Heatmap(
          mat,
          name = next_spacer_name("spacer_v"),
          col = circlize::colorRamp2(c(0, 1), c("white", "white")),
          na_col = "white",
          show_heatmap_legend = FALSE,
          show_row_names = FALSE,
          show_column_names = FALSE,
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          border = FALSE
        )
      }

      add_protein_selection_annotation <- function(ht_obj, row_names) {
        highlighted <- heatmap_highlighted_proteins()
        if (is.null(ht_obj) || is.null(highlighted) || length(highlighted) == 0 || is.null(row_names)) {
          return(ht_obj)
        }

        protein_annotation <- tryCatch(
          create_protein_annotation(
            row_names,
            highlighted,
            fs$row_font_size,
            label_padding_mm = legend_plot_gap_mm_from_input(input)
          ),
          error = function(e) {
            heatmap_debug_log(paste("Failed to build protein-selection annotation for tab", current_tab, ":", e$message), 1)
            NULL
          }
        )

        if (is.null(protein_annotation)) return(ht_obj)
        tryCatch(ht_obj + protein_annotation, error = function(e) {
          heatmap_debug_log(paste("Failed to append protein-selection annotation for tab", current_tab, ":", e$message), 1)
          ht_obj
        })
      }

      # For tile alignment we rely on the fixed expression matrix
      expr_mat <- tryCatch(fixed_expr@matrix, error = function(e) NULL)
      if (is.null(expr_mat) || !is.matrix(expr_mat)) return(NULL)

      # === Build per tab ===
      if (current_tab == "grid_expr_corr") {
        ht <- fixed_expr

        # Add expression -> (spacer) -> protein correlation
        if (!is.null(fixed_prot)) {
          spacer_h <- make_spacer_h(n_tile_rows = nrow(expr_mat), n_tile_cols = 2)
          ht <- ht + spacer_h + fixed_prot
        }

        # Add Basemean if user enabled it in UI AND object exists
        if (isTRUE(input$show_basemean_heatmap) && !is.null(fixed_bm)) {
          spacer_h <- make_spacer_h(n_tile_rows = nrow(expr_mat), n_tile_cols = 2)
          ht <- ht + spacer_h + fixed_bm
        }

        # Add Ratio if user enabled it in UI AND object exists
        if (isTRUE(input$show_abundance_ratio_heatmap) && !is.null(fixed_ratio)) {
          spacer_h <- make_spacer_h(n_tile_rows = nrow(expr_mat), n_tile_cols = 2)
          ht <- ht + spacer_h + fixed_ratio
        }

        ht <- add_protein_selection_annotation(ht, rownames(expr_mat))

        return(list(
          ht = ht,
          legend_side = legend_side,
          legend_dir = legend_dir,
          fs = fs,
          main_heatmap = if (!is.null(fixed_prot)) {
            sort_method_local <- input$sort_proteins_by %||% "z_score"
            if (identical(sort_method_local, "pearson_r")) {
              "Protein Correlation"
            } else if (isTRUE(input$show_row_dendrogram)) {
              "Expression"
            } else {
              "Protein Correlation"
            }
          } else NULL
        ))
      }

      if (current_tab == "grid_expr_sample") {
        ht <- fixed_expr

        if (!is.null(fixed_sample)) {
          # In UI you did: expr %v% spacer %v% sample
          spacer_v <- make_spacer_v(n_tile_rows = 2, n_tile_cols = ncol(expr_mat))
          ht <- ht %v% spacer_v %v% fixed_sample
        }

        return(list(
          ht = ht,
          legend_side = legend_side,
          legend_dir = legend_dir,
          fs = fs,
          main_heatmap = if (isTRUE(input$show_column_dendrogram) && !is.null(fixed_sample)) "Sample Correlation" else "Expression"
        ))
      }

      # Single tabs: just return the exact fixed object (no extra layout)
      if (current_tab == "expression_tab") {
        ht <- add_protein_selection_annotation(fixed_expr, rownames(expr_mat))
        return(list(ht = ht, legend_side = legend_side, legend_dir = legend_dir, fs = fs))
      }
      if (current_tab == "protein_cor_tab") {
        if (is.null(fixed_prot)) return(NULL)
        ht <- add_protein_selection_annotation(fixed_prot, rownames(expr_mat))
        return(list(ht = ht, legend_side = legend_side, legend_dir = legend_dir, fs = fs))
      }
      if (current_tab == "sample_cor_tab") {
        if (is.null(fixed_sample)) return(NULL)
        return(list(ht = fixed_sample, legend_side = legend_side, legend_dir = legend_dir, fs = fs))
      }
      if (current_tab == "basemean_tab") {
        if (is.null(fixed_bm)) return(NULL)
        return(list(ht = fixed_bm, legend_side = legend_side, legend_dir = legend_dir, fs = fs))
      }
      if (current_tab == "abundance_ratio_tab") {
        if (is.null(fixed_ratio)) return(NULL)
        return(list(ht = fixed_ratio, legend_side = legend_side, legend_dir = legend_dir, fs = fs))
      }

      NULL
    }


    # ========================================
    # Robust Download Handler (UPDATED)
    # ========================================
    output$downloadPlotButton_Heatmaps <- downloadHandler(
      filename = function() {
        format <- input$downloadFormat_Heatmaps %||% "pdf"
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        current_tab <- input$heatmap_unified_tabs %||% "grid_expr_corr"

        tab_names <- list(
          "grid_expr_corr"      = "Expression_Correlation_Grid",
          "grid_expr_sample"    = "Expression_Sample_Grid",
          "expression_tab"      = "Expression_Only",
          "protein_cor_tab"     = "Protein_Correlation",
          "sample_cor_tab"      = "Sample_Correlation",
          "basemean_tab"        = "Basemean_Only",
          "abundance_ratio_tab" = "Abundance_Ratio"
        )

        tab_name <- tab_names[[current_tab]] %||% "heatmap_plot"
        paste0(tab_name, "_", timestamp, ".", format)
      },

      content = function(file) {
        heatmap_debug_log("Starting download process (UI-first: exact same rendering functions as UI)", 1)

        # Dimensions and format from UI
        width_in  <- isolate(input$plotWidthInch_Heatmaps)  %||% 12
        height_in <- isolate(input$plotHeightInch_Heatmaps) %||% 10
        dpi       <- isolate(input$resolution_DPI_Heatmaps) %||% 300
        format    <- isolate(input$downloadFormat_Heatmaps) %||% "pdf"

        current_tab <- isolate(input$heatmap_unified_tabs) %||% "grid_expr_corr"

        heatmap_debug_log(
          paste("Download request - Tab:", current_tab,
                "Format:", format,
                "Size:", width_in, "x", height_in,
                "DPI:", dpi),
          1
        )

        open_device <- function() {
          if (format == "png") {
            grDevices::png(file, width = width_in, height = height_in, units = "in", res = dpi, type = "cairo")
          } else if (format == "jpeg") {
            grDevices::jpeg(file, width = width_in, height = height_in, units = "in", res = dpi, quality = 95)
          } else if (format == "tiff") {
            grDevices::tiff(file, width = width_in, height = height_in, units = "in", res = dpi, compression = "lzw")
          } else if (format == "svg") {
            grDevices::svg(file, width = width_in, height = height_in)
          } else if (format == "pdf") {
            grDevices::pdf(file, width = width_in, height = height_in)
          } else {
            stop(paste("Unsupported download format:", format))
          }
        }

        close_device_safely <- function() {
          if (grDevices::dev.cur() != 1) {
            tryCatch(grDevices::dev.off(), error = function(e) NULL)
          }
        }

        do_ui_identical_draw <- function() {
          draw_current_tab_exactly_like_ui(current_tab)
        }

        tryCatch({
          open_device()
          do_ui_identical_draw()
          close_device_safely()
          heatmap_debug_log(paste("Successfully created download file for tab:", current_tab), 1)
        }, error = function(e) {
          close_device_safely()
          heatmap_debug_log(paste("Download error:", e$message), 1)
          stop(e)
        })
      }
    )

    # ========================================
    # Current Tab Status Display
    # ========================================
    output$current_tab_info <- renderText({
      current_tab <- input$heatmap_unified_tabs %||% "grid_expr_corr"

      tab_names <- list(
        "grid_expr_corr"      = "Expression + Correlation Grid",
        "grid_expr_sample"    = "Expression + Sample Grid",
        "expression_tab"      = "Expression Only",
        "protein_cor_tab"     = "Protein Correlation Only",
        "sample_cor_tab"      = "Sample Correlation Only",
        "basemean_tab"        = "Basemean Only",
        "abundance_ratio_tab" = "Abundance Ratio Only"
      )

      current_name <- tab_names[[current_tab]] %||% current_tab
      paste0("Active tab: ", current_name)
    })

    # ========================================
    # PLOT GRID INTEGRATION
    # ========================================

    # Updated add_to_grid observer (nur vollständiger Block ersetzen)
    observeEvent(input$add_to_grid, {
      heatmap_debug_log("Heatmap: add_to_grid clicked", 2)

      current_tab <- input$heatmap_unified_tabs %||% "grid_expr_corr"
      tab_titles <- list(
        "grid_expr_corr" = "Expression_Correlation_Grid",
        "grid_expr_sample" = "Expression_Sample_Grid",
        "expression_tab" = "Expression_Only",
        "protein_cor_tab" = "Protein_Correlation",
        "sample_cor_tab" = "Sample_Correlation",
        "basemean_tab" = "Basemean_Only",
        "abundance_ratio_tab" = "Abundance_Ratio"
      )
      plot_title <- tab_titles[[current_tab]] %||% "Heatmap"

      # Render exactly like UI/download to preserve all active customizations.
      adaptive_draw <- function(file_type = c("svg", "png")) {
        file_type <- match.arg(file_type)

        if (file_type == "svg") {
          temp_svg <- tempfile(fileext = ".svg")
          svg(temp_svg, width = 12, height = 9)
          tryCatch({
            draw_current_tab_exactly_like_ui(current_tab)
          }, finally = {
            if (grDevices::dev.cur() != 1) grDevices::dev.off()
          })
          temp_svg
        } else {
          temp_png <- tempfile(fileext = ".png")
          png(temp_png, width = 1200, height = 900, res = 150, type = "cairo")
          tryCatch({
            draw_current_tab_exactly_like_ui(current_tab)
          }, finally = {
            if (grDevices::dev.cur() != 1) grDevices::dev.off()
          })
          temp_png
        }
      }

      p <- tryCatch({
        use_svg <- all(sapply(c("rsvg", "xml2"), function(pk) requireNamespace(pk, quietly = TRUE)))
        if (use_svg) {
          svg_path <- adaptive_draw("svg")
          png_path <- tempfile(fileext = ".png")
          rsvg::rsvg_png(svg_path, png_path, width = 1200, height = 900)
          img <- png::readPNG(png_path)
          unlink(c(svg_path, png_path))
        } else {
          png_path <- adaptive_draw("png")
          img <- png::readPNG(png_path)
          unlink(png_path)
        }
        p <- ggplot2::ggplot() +
          ggplot2::annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
          ggplot2::theme_void()
        if (!isTRUE(input$hideTitle_Heatmap)) {
          p <- p +
            ggplot2::ggtitle(paste("Heatmap -", plot_title)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14))
        }
        p
      }, error = function(e) {
        heatmap_debug_log(paste("Heatmap: add_to_grid rendering error:", e$message), 1)
        showNotification(paste("Error preparing current heatmap:", e$message), type = "error")
        NULL
      })

      if (is.null(p) || !inherits(p,"ggplot")) {
        showNotification("Failed to prepare plot for grid.", type = "error")
        return()
      }
      if (!exists("modEnv") || !exists("add_to_grid", envir = modEnv)) {
        showNotification("Grid system not initialized.", type = "error")
        return()
      }
      sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
      lbl_raw <- input$grid_label
      lbl_id <- if (is.null(lbl_raw) || !nzchar(lbl_raw)) "default" else sanitize(lbl_raw)
      plot_id <- paste0(ns(""), "Heatmap_", lbl_id)
      lbl_vis <- if (!is.null(lbl_raw) && nzchar(lbl_raw)) lbl_raw else paste("Heatmap", plot_title)

      tryCatch({
        modEnv$add_to_grid(rv, id = plot_id, plot = p, label = lbl_vis, source = "Heatmap")
        showNotification(
          paste("Heatmap", plot_title, "added to grid."),
          type = "message"
        )
      }, error = function(e) {
        showNotification("Error adding heatmap to grid.", type = "error")
      })
    })
