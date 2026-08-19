# ==============================================================================
# File: R/export/export_pipeline_optional_modules.R
#
# Purpose:
#   Execute optional module result sheet exports (PCA/Volcano/DotPlot/STRING).
#
# Architectural Role:
#   Stage helper for create_comprehensive_excel() pipeline orchestration.
#
# Responsibilities:
#   - Export optional module result/analysis worksheets when available.
#   - Preserve defensive extraction and per-module graceful degradation.
#   - Update shared sheet counters and diagnostics.
#
# Non-Responsibilities (Must NOT be here):
#   - Base data sheets, GO/GSEA staging, final workbook finalization.
#
# Public API:
#   export_pipeline_run_optional_modules()
# ==============================================================================

#' Pipeline stage: optional module result sheets
#'
#' Purpose:
#'   Run optional module exports in the shared context.
#'
#' Inputs/Parameters:
#'   @param ctx Environment-based export context.
#'
#' Outputs:
#'   - Invisibly returns NULL; mutates `ctx` in place.
#'
#' Side effects:
#'   - Adds optional module worksheets and updates counters.
#'
#' Failure behavior:
#'   - Logs module-level failures while keeping pipeline execution alive.
export_pipeline_run_optional_modules <- function(ctx) {
  with(ctx, {
    # ========================================
    # Additional Sheets: Other Modules (Optional)
    # ========================================

    # Only add other module sheets if we have module outputs
    if (!is.null(module_outputs)) {

      # PCA Module
      if (!is.null(module_outputs$pca_out)) {
        debug_log("Adding PCA analysis sheet", level = 1)
        tryCatch({
          # Try to extract PCA data using safe method
          pca_data <- safe_extract_module_data(module_outputs$pca_out, "get_plot_data", "PCA", debug_log)
          if (is.null(pca_data)) {
            pca_data <- safe_extract_module_data(module_outputs$pca_out, "get_analysis_data", "PCA", debug_log)
          }

          if (!is.null(pca_data)) {
            openxlsx::addWorksheet(wb, "PCA_Analysis")
            writeData_sanitized(wb, "PCA_Analysis", pca_data, startRow = 1, startCol = 1)
            openxlsx::addStyle(wb, "PCA_Analysis", headerStyle, rows = 1, cols = 1:ncol(pca_data))
            sheets_created <- sheets_created + 1
            debug_log("PCA analysis sheet created successfully", level = 1)
          }
        }, error = function(e) {
          debug_log(paste("Error creating PCA sheet:", e$message), level = 1)
        })
      }

      # Volcano Plot Module
      if (!is.null(module_outputs$volcano_out)) {
        debug_log("Adding Volcano Plot analysis sheet", level = 1)
        tryCatch({
          volcano_module <- module_outputs$volcano_out
          volcano_export_meta <- NULL
          if (is.list(volcano_module) && "get_export_data" %in% names(volcano_module) && is.function(volcano_module$get_export_data)) {
            volcano_export_meta <- tryCatch(volcano_module$get_export_data(), error = function(e) NULL)
          }
          if (is.list(volcano_module) && "plots" %in% names(volcano_module) && is.function(volcano_module$plots)) {
            volcano_plots <- volcano_module$plots()
            if (is.list(volcano_plots) && length(volcano_plots) > 0) {
              openxlsx::addWorksheet(wb, "Volcano_Result")

              info_df <- data.frame(
                Property = c(
                  "Abundance ratio threshold",
                  "p-value threshold",
                  "X-axis range",
                  "Y-axis range"
                ),
                Value = c(
                  as.character((volcano_export_meta$abundance_ratio_threshold %||% NA_real_)),
                  as.character((volcano_export_meta$p_value_threshold %||% NA_real_)),
                  paste((volcano_export_meta$x_axis_range %||% c(NA_real_, NA_real_)), collapse = " to "),
                  paste((volcano_export_meta$y_axis_range %||% c(NA_real_, NA_real_)), collapse = " to ")
                ),
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
              writeData_sanitized(wb, "Volcano_Result", info_df, startRow = 1, startCol = 1)
              openxlsx::addStyle(wb, "Volcano_Result", headerStyle, rows = 1, cols = 1:ncol(info_df))

              data_start_row <- nrow(info_df) + 4
              start_col <- 1
              for (plot_idx in seq_along(volcano_plots)) {
                plot_obj <- volcano_plots[[plot_idx]]
                if (is.null(plot_obj) || !inherits(plot_obj, "ggplot")) next
                plot_title <- names(volcano_plots)[plot_idx] %||% paste("Volcano", plot_idx)
                plot_data <- if (is.data.frame(plot_obj$data)) plot_obj$data else data.frame()

                if (nrow(plot_data) > 0 && !("ID" %in% names(plot_data)) && ("identifier" %in% names(plot_data))) {
                  plot_data$ID <- plot_data$identifier
                }

                if (nrow(plot_data) > 0 && !("ID" %in% names(plot_data)) && ("row_idx" %in% names(plot_data)) &&
                    !is.null(rv) && !is.null(rv$data_mod) && !is.null(rv$data_def) && is.data.frame(rv$data_def) &&
                    "Options" %in% names(rv$data_def)) {
                  selected_identifier <- volcano_export_meta$identifier_column %||% NA_character_
                  if (is.na(selected_identifier) || !nzchar(selected_identifier)) {
                    selected_identifier <- tryCatch(volcano_export_meta$selected_identifier, error = function(e) NA_character_)
                  }
                  if (is.na(selected_identifier) || !nzchar(selected_identifier)) {
                    selected_identifier <- tryCatch(module_outputs$volcano_out$get_session_state()$plot_ui_inputs$Identifier_Volcano, error = function(e) NA_character_)
                  }

                  identifier_idx <- which(grepl(selected_identifier, rv$data_def$Options, fixed = TRUE))
                  if (length(identifier_idx) > 0 && identifier_idx[1] <= ncol(rv$data_mod)) {
                    row_idx <- suppressWarnings(as.integer(plot_data$row_idx))
                    valid_idx <- !is.na(row_idx) & row_idx >= 1 & row_idx <= nrow(rv$data_mod)
                    recovered_id <- rep(NA_character_, nrow(plot_data))
                    recovered_id[valid_idx] <- as.character(rv$data_mod[row_idx[valid_idx], identifier_idx[1]])
                    if (any(!is.na(recovered_id))) plot_data$ID <- recovered_id
                  }
                }
                if (!("ID" %in% names(plot_data))) {
                  debug_log(paste("Skipping volcano plot", plot_title, "for export: missing ID/identifier column"), level = 1)
                  next
                }
                point_size <- if ("point_size" %in% names(plot_data)) plot_data$point_size else if ("size" %in% names(plot_data)) plot_data$size else NA_real_
                point_color <- if ("point_color" %in% names(plot_data)) plot_data$point_color else if ("color" %in% names(plot_data)) plot_data$color else NA_character_

                export_df <- data.frame(
                  "Identifier" = as.character(plot_data$ID),
                  "'log2 abundance ratio" = suppressWarnings(as.numeric(plot_data$x)),
                  "'-log10 p value" = suppressWarnings(as.numeric(plot_data$y)),
                  "color" = as.character(point_color),
                  "dot size" = suppressWarnings(as.numeric(point_size)),
                  stringsAsFactors = FALSE,
                  check.names = FALSE
                )

                writeData_sanitized(wb, "Volcano_Result", data.frame(plot_title, stringsAsFactors = FALSE),
                                    startRow = data_start_row - 1, startCol = start_col, colNames = FALSE)
                writeData_sanitized(wb, "Volcano_Result", export_df,
                                    startRow = data_start_row, startCol = start_col)
                openxlsx::addStyle(wb, "Volcano_Result", headerStyle,
                                   rows = data_start_row, cols = start_col:(start_col + ncol(export_df) - 1))
                start_col <- start_col + ncol(export_df) + 1
              }

              sheets_created <- sheets_created + 1
              debug_log("Volcano result sheet created successfully", level = 1)
            }
          }

          # Try to extract Volcano data using safe method
          volcano_data <- safe_extract_module_data(module_outputs$volcano_out, "get_plot_data", "Volcano", debug_log)
          if (is.null(volcano_data)) {
            volcano_data <- safe_extract_module_data(module_outputs$volcano_out, "get_analysis_data", "Volcano", debug_log)
          }

          if (!is.null(volcano_data)) {
            openxlsx::addWorksheet(wb, "Volcano_Analysis")
            writeData_sanitized(wb, "Volcano_Analysis", volcano_data, startRow = 1, startCol = 1)
            openxlsx::addStyle(wb, "Volcano_Analysis", headerStyle, rows = 1, cols = 1:ncol(volcano_data))
            sheets_created <- sheets_created + 1
            debug_log("Volcano analysis sheet created successfully", level = 1)
          }
        }, error = function(e) {
          debug_log(paste("Error creating Volcano sheet:", e$message), level = 1)
        })
      }

      # Dot Plot Module (Volcano-style result export)
      if (!is.null(module_outputs$dotplot_out)) {
        debug_log("Adding Dot Plot result sheet", level = 1)
        tryCatch({
          dotplot_module <- module_outputs$dotplot_out
          dotplot_plot <- NULL
          dotplot_cfg <- NULL
          dotplot_thresholds <- NULL
          dotplot_state <- NULL

          if (is.list(dotplot_module) && "get_plot" %in% names(dotplot_module) && is.function(dotplot_module$get_plot)) {
            dotplot_plot <- tryCatch(dotplot_module$get_plot(), error = function(e) NULL)
          }
          if (is.list(dotplot_module) && "get_config" %in% names(dotplot_module) && is.function(dotplot_module$get_config)) {
            dotplot_cfg <- tryCatch(dotplot_module$get_config(), error = function(e) NULL)
          }
          if (is.list(dotplot_module) && "get_thresholds" %in% names(dotplot_module) && is.function(dotplot_module$get_thresholds)) {
            dotplot_thresholds <- tryCatch(dotplot_module$get_thresholds(), error = function(e) NULL)
          }
          if (is.list(dotplot_module) && "get_session_state" %in% names(dotplot_module) && is.function(dotplot_module$get_session_state)) {
            dotplot_state <- tryCatch(dotplot_module$get_session_state(), error = function(e) NULL)
          }

          if (!is.null(dotplot_plot) && inherits(dotplot_plot, "ggplot")) {
            plot_data <- if (is.data.frame(dotplot_plot$data)) dotplot_plot$data else data.frame()
            openxlsx::addWorksheet(wb, "DotPlot_Result")

            ui_inputs <- tryCatch(dotplot_state$plot_ui_inputs, error = function(e) NULL)
            if (is.null(ui_inputs)) ui_inputs <- tryCatch(dotplot_state$ui_inputs, error = function(e) NULL)
            axis_cfg_state <- tryCatch(dotplot_state$axis_config, error = function(e) NULL)
            selected_region <- tryCatch(dotplot_state$selected_region, error = function(e) NULL)
            selected_theme <- as.character((ui_inputs$theme_select %||% NA_character_))[1]
            selected_identifier <- as.character((ui_inputs$GeneIdentifierColumn_dot %||% NA_character_))[1]
            selected_x_col <- as.character((axis_cfg_state$x_col %||% ui_inputs$x_axis_column %||% dotplot_cfg$x_col %||% dotplot_plot$labels$x %||% NA_character_))[1]
            selected_y_col <- as.character((axis_cfg_state$y_col %||% ui_inputs$y_axis_column %||% dotplot_cfg$y_col %||% dotplot_plot$labels$y %||% NA_character_))[1]
            selected_x_transform <- as.character((ui_inputs$x_transform %||% dotplot_cfg$x_transform %||% "raw"))[1]
            selected_y_transform <- as.character((ui_inputs$y_transform %||% dotplot_cfg$y_transform %||% "raw"))[1]
            selected_x_range <- ui_inputs$x_axis_range %||% dotplot_cfg$x_range %||% c(NA_real_, NA_real_)
            selected_y_range <- ui_inputs$y_axis_range %||% dotplot_cfg$y_range %||% c(NA_real_, NA_real_)
            if ((is.null(selected_x_range) || any(!is.finite(as.numeric(selected_x_range)))) && !is.null(dotplot_plot)) {
              ranges <- tryCatch(extract_ggplot_ranges(dotplot_plot), error = function(e) NULL)
              selected_x_range <- ranges$x_range %||% selected_x_range
              selected_y_range <- ranges$y_range %||% selected_y_range
            }

            threshold_lines <- "none"
            if (is.list(dotplot_thresholds) && length(dotplot_thresholds) > 0) {
              threshold_desc <- vapply(seq_along(dotplot_thresholds), function(i) {
                th <- dotplot_thresholds[[i]]
                th_type <- as.character(th$type %||% "unknown")
                th_value <- suppressWarnings(as.numeric(th$value %||% NA_real_))
                th_label <- as.character(th$label %||% th$id %||% paste0("threshold_", i))
                paste0(th_label, " [type=", th_type, ", value=", ifelse(is.na(th_value), "NA", format(th_value, trim = TRUE)), ", color=", as.character(th$color %||% NA_character_), ", style=", as.character(th$style %||% NA_character_), ", thickness=", as.character(th$thickness %||% NA_real_), "]")
              }, character(1))
              threshold_lines <- paste(threshold_desc, collapse = " | ")
            }

            if (nrow(plot_data) == 0) {
              built <- tryCatch(ggplot2::ggplot_build(dotplot_plot), error = function(e) NULL)
              layer_df <- tryCatch(built$data[[1]], error = function(e) NULL)
              if (is.data.frame(layer_df) && nrow(layer_df) > 0) {
                plot_data <- layer_df
                if ("customdata" %in% names(layer_df)) plot_data$ID <- as.character(layer_df$customdata)
              }
            }

            source_data_mod <- tryCatch(dotplot_state$restore_plot_data_cache$data_mod, error = function(e) NULL)
            source_data_def <- tryCatch(dotplot_state$restore_plot_data_cache$data_def, error = function(e) NULL)
            if (!is.data.frame(source_data_mod) || !is.data.frame(source_data_def)) {
              source_data_mod <- rv$data_mod
              source_data_def <- rv$data_def
            }

            if (nrow(plot_data) == 0 && is.data.frame(source_data_mod) && is.data.frame(source_data_def) &&
                is.list(axis_cfg_state) && exists("dotplot_prepare_plot_data_with_identifiers", mode = "function", inherits = TRUE)) {
              prepared_df <- tryCatch(
                dotplot_prepare_plot_data_with_identifiers(
                  data = source_data_mod,
                  data_def = source_data_def,
                  axis_config = axis_cfg_state,
                  identifier_col = selected_identifier
                ),
                error = function(e) NULL
              )
              if (is.data.frame(prepared_df) && nrow(prepared_df) > 0) {
                plot_data <- prepared_df
              }
            }

            if (nrow(plot_data) == 0 && is.data.frame(source_data_mod) &&
                !is.na(selected_x_col) && nzchar(selected_x_col) && selected_x_col %in% names(source_data_mod) &&
                !is.na(selected_y_col) && nzchar(selected_y_col) && selected_y_col %in% names(source_data_mod)) {
              x_raw <- suppressWarnings(as.numeric(source_data_mod[[selected_x_col]]))
              y_raw <- suppressWarnings(as.numeric(source_data_mod[[selected_y_col]]))

              # Local helper function:
              # Purpose: Apply selected axis transformation to numeric vectors.
              # Inputs: v (numeric vector), transform_name (character scalar).
              # Outputs: transformed numeric vector.
              # Side effects: none.
              # Failure behavior: unknown transform names return input unchanged.
              apply_transform <- function(v, transform_name) {
                tn <- tolower(as.character(transform_name %||% "raw"))
                if (tn %in% c("raw", "none", "")) return(v)
                if (tn == "log2") return(log2(v))
                if (tn == "log10") return(log10(v))
                if (tn %in% c("neg_log10", "-log10")) return(-log10(v))
                if (tn %in% c("neg_log2", "-log2")) return(-log2(v))
                v
              }

              x_vals <- suppressWarnings(apply_transform(x_raw, selected_x_transform))
              y_vals <- suppressWarnings(apply_transform(y_raw, selected_y_transform))

              if (!is.na(selected_identifier) && nzchar(selected_identifier) && selected_identifier %in% names(source_data_mod)) {
                id_vals <- as.character(source_data_mod[[selected_identifier]])
              } else {
                id_vals <- as.character(seq_len(nrow(source_data_mod)))
              }

              plot_data <- data.frame(ID = id_vals, x = x_vals, y = y_vals, stringsAsFactors = FALSE, check.names = FALSE)
              plot_data <- plot_data[is.finite(plot_data$x) & is.finite(plot_data$y), , drop = FALSE]
            }

            # Identifier recovery similar to Volcano export path
            if (!("ID" %in% names(plot_data)) && ("identifier" %in% names(plot_data))) {
              plot_data$ID <- plot_data$identifier
            }
            if (!("ID" %in% names(plot_data)) && ("Name" %in% names(plot_data))) {
              plot_data$ID <- plot_data$Name
            }
            if (!("ID" %in% names(plot_data)) && ("row_idx" %in% names(plot_data)) &&
                !is.null(rv) && !is.null(rv$data_mod) && !is.null(rv$data_def) && is.data.frame(rv$data_def) &&
                "Options" %in% names(rv$data_def) && !is.na(selected_identifier) && nzchar(selected_identifier)) {
              identifier_idx <- which(grepl(selected_identifier, rv$data_def$Options, fixed = TRUE))
              if (length(identifier_idx) > 0 && identifier_idx[1] <= ncol(rv$data_mod)) {
                row_idx <- suppressWarnings(as.integer(plot_data$row_idx))
                valid_idx <- !is.na(row_idx) & row_idx >= 1 & row_idx <= nrow(rv$data_mod)
                recovered_id <- rep(NA_character_, nrow(plot_data))
                recovered_id[valid_idx] <- as.character(rv$data_mod[row_idx[valid_idx], identifier_idx[1]])
                if (any(!is.na(recovered_id))) plot_data$ID <- recovered_id
              }
            }

            # If build layer data lacks IDs/customdata, map deterministically by row order
            # against the source identifier column used by the module.
            if (!("ID" %in% names(plot_data)) && nrow(plot_data) > 0 && is.data.frame(source_data_mod)) {
              if (!is.na(selected_identifier) && nzchar(selected_identifier) && selected_identifier %in% names(source_data_mod)) {
                src_ids <- as.character(source_data_mod[[selected_identifier]])
                if (length(src_ids) >= nrow(plot_data)) {
                  plot_data$ID <- src_ids[seq_len(nrow(plot_data))]
                }
              }
            }

            if (!("ID" %in% names(plot_data))) {
              debug_log("DotPlot_Result data export missing ID/identifier mapping; writing empty table with headers", level = 1)
              dotplot_export_df <- data.frame(
                "Identifier" = character(0),
                "X axis value" = numeric(0),
                "Y axis value" = numeric(0),
                "dot color" = character(0),
                "dot size" = numeric(0),
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            } else {
              point_color <- if ("point_color" %in% names(plot_data)) plot_data$point_color else if ("color" %in% names(plot_data)) plot_data$color else NA_character_
              point_size <- if ("point_size" %in% names(plot_data)) plot_data$point_size else if ("size" %in% names(plot_data)) plot_data$size else NA_real_

              x_values <- if ("x" %in% names(plot_data)) plot_data$x else if (!is.na(selected_x_col) && nzchar(selected_x_col) && selected_x_col %in% names(plot_data)) plot_data[[selected_x_col]] else NA_real_
              y_values <- if ("y" %in% names(plot_data)) plot_data$y else if (!is.na(selected_y_col) && nzchar(selected_y_col) && selected_y_col %in% names(plot_data)) plot_data[[selected_y_col]] else NA_real_

              dotplot_export_df <- data.frame(
                "Identifier" = as.character(plot_data$ID),
                "X axis value" = suppressWarnings(as.numeric(x_values)),
                "Y axis value" = suppressWarnings(as.numeric(y_values)),
                "dot color" = as.character(point_color),
                "dot size" = suppressWarnings(as.numeric(point_size)),
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            }


            # Derive actual ranges from final plotted data whenever possible.
            x_vals_final <- if ("x" %in% names(plot_data)) suppressWarnings(as.numeric(plot_data$x)) else NULL
            y_vals_final <- if ("y" %in% names(plot_data)) suppressWarnings(as.numeric(plot_data$y)) else NULL
            if (!is.null(x_vals_final) && any(is.finite(x_vals_final))) {
              selected_x_range <- range(x_vals_final[is.finite(x_vals_final)], na.rm = TRUE)
            }
            if (!is.null(y_vals_final) && any(is.finite(y_vals_final))) {
              selected_y_range <- range(y_vals_final[is.finite(y_vals_final)], na.rm = TRUE)
            }

            info_df <- data.frame(
              Property = c(
                "Threshold lines",
                "X transformation",
                "Y transformation",
                "X-axis range",
                "Y-axis range",
                "Identifier column",
                "X-axis column",
                "Y-axis column",
                "Data rows",
                "Theme"
              ),
              Value = c(
                threshold_lines,
                selected_x_transform,
                selected_y_transform,
                paste(selected_x_range, collapse = " to "),
                paste(selected_y_range, collapse = " to "),
                selected_identifier,
                selected_x_col,
                selected_y_col,
                as.character(nrow(plot_data)),
                selected_theme
              ),
              stringsAsFactors = FALSE
            )

            writeData_sanitized(wb, "DotPlot_Result", info_df, startRow = 1, startCol = 1)
            openxlsx::addStyle(wb, "DotPlot_Result", headerStyle, rows = 1, cols = 1:ncol(info_df))

            data_start_row <- nrow(info_df) + 3
            writeData_sanitized(wb, "DotPlot_Result", dotplot_export_df, startRow = data_start_row, startCol = 1)
            openxlsx::addStyle(wb, "DotPlot_Result", headerStyle, rows = data_start_row, cols = 1:ncol(dotplot_export_df))

            sheets_created <- sheets_created + 1
            debug_log("Dot Plot result sheet created successfully", level = 1)
          }
        }, error = function(e) {
          debug_log(paste("Error creating Dot Plot result sheet:", e$message), level = 1)
        })
      }
    }


      # STRING Network Module
      if (!is.null(module_outputs$STRING_out)) {
        debug_log("Adding STRING network result sheet", level = 1)
        tryCatch({
          string_module   <- module_outputs$STRING_out
          string_export   <- NULL

          if (is.list(string_module) &&
              "get_export_data" %in% names(string_module) &&
              is.function(string_module$get_export_data)) {
            string_export <- tryCatch(string_module$get_export_data(), error = function(e) NULL)
          }

          string_nodes  <- tryCatch(string_export$nodes,     error = function(e) NULL)
          string_edges  <- tryCatch(string_export$edges,     error = function(e) NULL)
          string_inputs <- tryCatch(string_export$ui_inputs, error = function(e) NULL)

          if (!is.null(string_nodes) && is.data.frame(string_nodes) && nrow(string_nodes) > 0) {
            openxlsx::addWorksheet(wb, "STRING_Result")

            # ---------- Properties table (UI inputs) ----------
            info_df <- data.frame(
              Property = c(
                "STRING DB version",
                "Score threshold",
                "Displayed interactions",
                "Include neighbours",
                "Neighbour strategy",
                "Node label source",
                "Minimum degree filter"
              ),
              Value = c(
                as.character(string_inputs$version           %||% NA_character_),
                as.character(string_inputs$score             %||% NA_real_),
                as.character(string_inputs$edge_type         %||% NA_character_),
                as.character(string_inputs$neighbor_count    %||% NA_real_),
                as.character(string_inputs$neighbor_strategy %||% NA_character_),
                as.character(string_inputs$label_variant     %||% NA_character_),
                as.character(string_inputs$min_degree        %||% NA_real_)
              ),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            writeData_sanitized(wb, "STRING_Result", info_df, startRow = 1, startCol = 1)
            openxlsx::addStyle(wb, "STRING_Result", headerStyle, rows = 1, cols = 1:ncol(info_df))

            # ---------- Spacer + data table ----------
            data_start_row <- nrow(info_df) + 3  # properties rows + 1 header row of info_df + 1 spacer row = data starts here

            # Build label lookup: STRING id -> display label
            label_lookup <- setNames(as.character(string_nodes$label), as.character(string_nodes$id))

            if (!is.null(string_edges) && is.data.frame(string_edges) && nrow(string_edges) > 0) {
              edges_from <- as.character(string_edges$from)
              edges_to   <- as.character(string_edges$to)

              string_rows <- lapply(seq_len(nrow(string_nodes)), function(i) {
                node_id    <- as.character(string_nodes$id[i])
                node_label <- as.character(string_nodes$label[i])
                cluster    <- as.character(string_nodes$group[i])

                neighbor_ids <- unique(c(
                  edges_to[edges_from == node_id],
                  edges_from[edges_to == node_id]
                ))
                neighbor_labels <- vapply(neighbor_ids, function(nid) {
                  lbl <- label_lookup[[nid]]
                  if (is.null(lbl) || is.na(lbl)) nid else lbl
                }, character(1), USE.NAMES = FALSE)

                data.frame(
                  Protein     = node_label,
                  Connections = paste(neighbor_labels, collapse = ", "),
                  Cluster     = cluster,
                  stringsAsFactors = FALSE
                )
              })

              string_export_df <- do.call(rbind, string_rows)
              rownames(string_export_df) <- NULL
            } else {
              string_export_df <- data.frame(
                Protein     = as.character(string_nodes$label),
                Connections = NA_character_,
                Cluster     = as.character(string_nodes$group),
                stringsAsFactors = FALSE
              )
            }

            string_export_df <- sanitize_for_excel(string_export_df, "STRING_Result", debug_log)
            writeData_sanitized(wb, "STRING_Result", string_export_df, startRow = data_start_row, startCol = 1)
            openxlsx::addStyle(wb, "STRING_Result", headerStyle,
                               rows = data_start_row, cols = 1:ncol(string_export_df))

            sheets_created <- sheets_created + 1
            debug_log(paste("STRING result sheet created with", nrow(string_export_df), "proteins"), level = 1)
          } else {
            debug_log("STRING export: no node data available", level = 1)
          }
        }, error = function(e) {
          debug_log(paste("Error creating STRING result sheet:", e$message), level = 1)
        })
      }

  })
  invisible(NULL)
}
