# ==============================================================================
# STRING Module - Visual Styling Observers
# ==============================================================================
#
# Purpose:
#   All visual property observers for the STRING network: node appearance
#   (color, border, size, shape, frame), font properties (size, typeface, style),
#   node label management (apply_label_variant, label_variant_STRING), and
#   edge appearance (color, width, type). Also defines apply_label_variant, which
#   is returned and used by STRING_server_network_build.R after network creation.
#
# Architecture role:
#   Factory function initialized inside modSTRINGServer. Returns
#   register_styling_observers (which wires all Shiny observers) and
#   apply_label_variant (used by the network build observer to apply the
#   preferred label source after a new network is created).
#
# File structure:
#   1. Factory function signature: initialize_STRING_styling
#   2. Internal helper: get_neighbor_nodes_for_selection
#   3. Internal helper: resolve_target_nodes_from_selection
#   4. Internal function: apply_label_variant
#   5. register_styling_observers: registers all visual property observers
#      a. Node color, border, size, shape, frame observers
#      b. Font size, typeface, style observers
#      c. Label variant observer
#      d. Edge color, width, type observers
#   6. Return list
#
# Future developers:
#   - All new visual property observers belong in register_styling_observers here.
#   - apply_label_variant is returned and must be exported for use by
#     initialize_STRING_network_build.
#   - Do not add network topology logic (layout, filtering, selection) here.
#   - All logging uses debug_log passed as a factory parameter.
# ==============================================================================

initialize_STRING_styling <- function(
    input,
    output,
    session,
    ns,
    debug_log,
    nodes,
    edges,
    vis_data,
    original_nodes,
    selectedNodes,
    selectedEdges,
    current_selection_type,
    last_selection_type,
    label_alias_data,
    label_initial_labels,
    format_node_label,
    shape_mapping,
    update_node_ui_controls,
    update_edge_ui_controls,
    cluster_labels_version,
    styling_ui_sync_state) {

  is_style_ui_sync_active <- function() {
    current_depth <- isolate(styling_ui_sync_state$depth)
    is.numeric(current_depth) && !is.na(current_depth) && current_depth > 0L
  }

  same_style_input_value <- function(current_value, expected_value) {
    if (is.null(current_value) || is.null(expected_value)) {
      return(FALSE)
    }

    if (is.numeric(current_value) && is.numeric(expected_value)) {
      return(isTRUE(all.equal(current_value, expected_value, check.attributes = FALSE)))
    }

    identical(tolower(as.character(current_value)), tolower(as.character(expected_value)))
  }

  consume_pending_style_ui_sync <- function(input_id, current_value) {
    pending <- isolate(styling_ui_sync_state$pending)
    if (is.null(pending) || !is.list(pending) || is.null(pending[[input_id]])) {
      return(FALSE)
    }

    if (!same_style_input_value(current_value, pending[[input_id]])) {
      pending[[input_id]] <- NULL
      styling_ui_sync_state$pending <- pending
      return(FALSE)
    }

    pending[[input_id]] <- NULL
    styling_ui_sync_state$pending <- pending
    TRUE
  }

  skip_during_style_ui_sync <- function(input_id, current_value = input[[input_id]]) {
    if (consume_pending_style_ui_sync(input_id, current_value)) {
      debug_log(paste("Skipping", input_id, "styling observer for pending programmatic UI synchronization"), 2)
      return(TRUE)
    }

    if (is_style_ui_sync_active()) {
      debug_log(paste("Skipping", input_id, "styling observer during selection UI synchronization"), 2)
      return(TRUE)
    }

    FALSE
  }

  apply_label_variant <- function(selection) {
    req(nodes())
    current_nodes <- nodes()
    alias_info <- label_alias_data()
    initial_labels <- label_initial_labels()

    if (is.null(current_nodes) || nrow(current_nodes) == 0) {
      return()
    }

    if (is.null(selection) || !nzchar(selection)) {
      return()
    }

    if (selection == "string_id") {
      new_labels <- current_nodes$id
    } else if (selection == "current") {
      if (is.null(initial_labels) || length(initial_labels) == 0) {
        new_labels <- current_nodes$original_label
      } else {
        new_labels <- initial_labels[current_nodes$id]
      }
    } else if (!is.null(alias_info) && selection %in% names(alias_info$alias_map)) {
      alias_map <- alias_info$alias_map[[selection]]
      new_labels <- alias_map[current_nodes$id]
    } else {
      new_labels <- current_nodes$original_label
    }

    new_labels[is.na(new_labels) | !nzchar(new_labels)] <- current_nodes$id
    font_style <- input$FontStyle_STRING
    formatted_labels <- vapply(new_labels, format_node_label, character(1), font_style = font_style)

    current_nodes$original_label <- new_labels
    current_nodes$label <- formatted_labels
    nodes(current_nodes)

    if (!is.null(original_nodes())) {
      orig_nodes <- original_nodes()
      match_idx <- match(current_nodes$id, orig_nodes$id)
      valid <- !is.na(match_idx)
      orig_nodes$original_label[match_idx[valid]] <- new_labels[valid]
      orig_nodes$label[match_idx[valid]] <- formatted_labels[valid]
      original_nodes(orig_nodes)
    }

    if (!is.null(vis_data())) {
      vis_data_temp <- vis_data()
      match_idx <- match(current_nodes$id, vis_data_temp$nodes$id)
      valid <- !is.na(match_idx)
      vis_data_temp$nodes$original_label[match_idx[valid]] <- new_labels[valid]
      vis_data_temp$nodes$label[match_idx[valid]] <- formatted_labels[valid]
      vis_data(vis_data_temp)
    }

    visNetworkProxy("String_plot") %>%
      visUpdateNodes(nodes = current_nodes)

    cluster_labels_version(cluster_labels_version() + 1L)
  }

  register_styling_observers <- function() {
    get_neighbor_nodes_for_selection <- function(selected_nodes_list, current_edges) {
      neighbor_nodes <- c()
      for (node_id in selected_nodes_list) {
        connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
        for (i in seq_len(nrow(connected_edges))) {
          neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
        }
      }
      neighbor_nodes <- unique(neighbor_nodes)
      neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
    }

    resolve_target_nodes_from_selection <- function(selected_nodes_list, selected_edges_list, include_neighbors, selection_flag) {
      target_nodes <- c()

      if (!is.null(selection_flag) && selection_flag == "node") {
        if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
          target_nodes <- selected_nodes_list
          debug_log(paste("NODE FLAG - targeting selected nodes:", paste(selected_nodes_list, collapse = ", ")), 2)

          if (isTRUE(include_neighbors)) {
            current_edges <- edges()
            neighbor_nodes <- get_neighbor_nodes_for_selection(selected_nodes_list, current_edges)
            target_nodes <- c(target_nodes, neighbor_nodes)
            debug_log(paste("Checkbox ACTIVE - adding neighbors:", paste(neighbor_nodes, collapse = ", ")), 2)
          } else {
            debug_log("Checkbox INACTIVE - NOT adding neighbors", 2)
          }
        }
      } else if (!is.null(selection_flag) && selection_flag == "edge") {
        if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
          current_edges <- edges()
          for (edge_id in selected_edges_list) {
            edge_data <- current_edges[current_edges$id == edge_id, ]
            if (nrow(edge_data) > 0) {
              target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
            }
          }
          debug_log(paste("EDGE FLAG - targeting connected nodes:", paste(unique(target_nodes), collapse = ", ")), 2)
        }
      }

      unique(target_nodes)
    }

    # Node background color - flag-based
    observeEvent(input$Color_1_STRING, {
      if (skip_during_style_ui_sync("Color_1_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        debug_log(paste("Color_1 - Flag:", selection_flag,
                        "Nodes:", paste(selected_nodes_list, collapse = ", "),
                        "Edges:", paste(selected_edges_list, collapse = ", "),
                        "Include neighbors:", include_neighbors), 2)

        target_nodes <- resolve_target_nodes_from_selection(
          selected_nodes_list = selected_nodes_list,
          selected_edges_list = selected_edges_list,
          include_neighbors = include_neighbors,
          selection_flag = selection_flag
        )

        # Apply color changes
        if (length(target_nodes) > 0) {
          debug_log(paste("FINAL target nodes count:", length(target_nodes)), 2)

          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$color.background[current_nodes$id == node_id] <- input$Color_1_STRING
            }
          }

          nodes(current_nodes)

          # Update visualization
          nodes_to_update <- current_nodes[current_nodes$id %in% target_nodes, ]
          update_data <- data.frame(
            id = nodes_to_update$id,
            color.background = nodes_to_update$color.background,
            color.border = nodes_to_update$color.border,
            stringsAsFactors = FALSE
          )

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = update_data)

          debug_log(paste("APPLIED background color to", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating node background color:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Node border color - flag-based
    observeEvent(input$Color_2_STRING, {
      if (skip_during_style_ui_sync("Color_2_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        debug_log(paste("Color_2 - Flag:", selection_flag, "Include neighbors:", include_neighbors), 2)

        target_nodes <- resolve_target_nodes_from_selection(
          selected_nodes_list = selected_nodes_list,
          selected_edges_list = selected_edges_list,
          include_neighbors = include_neighbors,
          selection_flag = selection_flag
        )

        # Apply color changes
        if (length(target_nodes) > 0) {
          debug_log(paste("FINAL target nodes count:", length(target_nodes)), 2)

          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$color.border[current_nodes$id == node_id] <- input$Color_2_STRING
            }
          }

          nodes(current_nodes)

          # Update visualization
          nodes_to_update <- current_nodes[current_nodes$id %in% target_nodes, ]
          update_data <- data.frame(
            id = nodes_to_update$id,
            color.background = nodes_to_update$color.background,
            color.border = nodes_to_update$color.border,
            stringsAsFactors = FALSE
          )

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = update_data)

          debug_log(paste("APPLIED border color to", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating node border color:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Node size - flag-based
    observeEvent(input$Size_1_STRING, {
      if (skip_during_style_ui_sync("Size_1_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply size changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$widthConstraint[current_nodes$id == node_id] <- input$Size_1_STRING
              current_nodes$heightConstraint[current_nodes$id == node_id] <- input$Size_1_STRING
              current_nodes$size[current_nodes$id == node_id] <- input$Size_1_STRING
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated size for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating node size:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Node shape - flag-based
    observeEvent(input$Shape_STRING, {
      if (skip_during_style_ui_sync("Shape_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply shape changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          shape_value <- shape_mapping(input$Shape_STRING)

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$shape[current_nodes$id == node_id] <- shape_value
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated shape for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating node shape:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Frame width - flag-based
    observeEvent(input$Frame_STRING, {
      if (skip_during_style_ui_sync("Frame_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply frame width changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$borderWidth[current_nodes$id == node_id] <- input$Frame_STRING
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated frame width for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating frame width:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # ========================================
    # FONT PROPERTY OBSERVERS (ROBUST)
    # ========================================

    # Text size - flag-based
    observeEvent(input$TextSize_STRING, {
      if (skip_during_style_ui_sync("TextSize_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply text size changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$font.size[current_nodes$id == node_id] <- input$TextSize_STRING
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated text size for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating text size:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Font type - flag-based
    observeEvent(input$FontType_STRING, {
      if (skip_during_style_ui_sync("FontType_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply font type changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              current_nodes$font.face[current_nodes$id == node_id] <- input$FontType_STRING
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated font type for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating font type:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Font style - flag-based
    observeEvent(input$FontStyle_STRING, {
      if (skip_during_style_ui_sync("FontStyle_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        include_neighbors <- input$include_neighbors_STRING
        selection_flag <- last_selection_type()

        target_nodes <- c()

        if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            target_nodes <- selected_nodes_list

            if (isTRUE(include_neighbors)) {
              current_edges <- edges()
              neighbor_nodes <- c()
              for (node_id in selected_nodes_list) {
                connected_edges <- current_edges[current_edges$from == node_id | current_edges$to == node_id, ]
                for (i in seq_len(nrow(connected_edges))) {
                  neighbor_nodes <- c(neighbor_nodes, connected_edges$from[i], connected_edges$to[i])
                }
              }
              neighbor_nodes <- unique(neighbor_nodes)
              neighbor_nodes <- neighbor_nodes[!neighbor_nodes %in% selected_nodes_list]
              target_nodes <- c(target_nodes, neighbor_nodes)
            }
          }
        }

        else if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            current_edges <- edges()
            for (edge_id in selected_edges_list) {
              edge_data <- current_edges[current_edges$id == edge_id, ]
              if (nrow(edge_data) > 0) {
                target_nodes <- c(target_nodes, edge_data$from, edge_data$to)
              }
            }
          }
        }

        # Apply font style changes
        if (length(target_nodes) > 0) {
          target_nodes <- unique(target_nodes)
          current_nodes <- nodes()

          for (node_id in target_nodes) {
            if (node_id %in% current_nodes$id) {
              # Get original label if available
              original_label <- if ("original_label" %in% colnames(current_nodes)) {
                current_nodes$original_label[current_nodes$id == node_id]
              } else {
                current_nodes$label[current_nodes$id == node_id]
              }

              # Format label based on style
              formatted_label <- switch(input$FontStyle_STRING,
                                        "Plain text" = original_label,
                                        "Bold" = paste0("<b>", original_label, "</b>"),
                                        "Italic" = paste0("<i>", original_label, "</i>"),
                                        "Bold and italic" = paste0("<b><i>", original_label, "</i></b>"),
                                        original_label)

              current_nodes$label[current_nodes$id == node_id] <- formatted_label

              # Set font flags
              current_nodes$font.bold[current_nodes$id == node_id] <-
                input$FontStyle_STRING %in% c("Bold", "Bold and italic")
              current_nodes$font.italic[current_nodes$id == node_id] <-
                input$FontStyle_STRING %in% c("Italic", "Bold and italic")
            }
          }

          nodes(current_nodes)

          visNetworkProxy("String_plot") %>%
            visUpdateNodes(nodes = current_nodes[current_nodes$id %in% target_nodes, ])

          debug_log(paste("Updated font style for", length(target_nodes), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating font style:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$label_variant_STRING, {
      tryCatch({
        apply_label_variant(input$label_variant_STRING)
        debug_log(paste("Updated node labels to variant:", input$label_variant_STRING), 1)
      }, error = function(e) {
        debug_log(paste("Error updating label variant:", e$message), 1)
      })
    }, ignoreInit = TRUE)


    # ========================================
    # ADDITIONAL EDGE PROPERTY OBSERVERS
    # ========================================

    # Edge color - flag-based
    observeEvent(input$EdgeColor_STRING, {
      if (skip_during_style_ui_sync("EdgeColor_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        selection_flag      <- last_selection_type()

        target_edges <- c()

        if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            target_edges <- selected_edges_list
          }
        } else if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            current_edges <- edges()
            for (node_id in selected_nodes_list) {
              connected_edges <- current_edges$id[current_edges$from == node_id | current_edges$to == node_id]
              target_edges <- c(target_edges, connected_edges)
            }
          }
        }

        if (length(target_edges) > 0) {
          target_edges  <- unique(target_edges)
          current_edges <- edges()

          for (edge_id in target_edges) {
            if (edge_id %in% current_edges$id) {
              current_edges$color[current_edges$id == edge_id] <- input$EdgeColor_STRING
            }
          }

          edges(current_edges)

          edges_to_update <- current_edges[current_edges$id %in% target_edges, ]

          visNetworkProxy("String_plot") %>%
            visUpdateEdges(edges = edges_to_update)

          debug_log(paste("Updated color for", length(target_edges), "edges"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating edge color:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Edge width - flag-based
    observeEvent(input$EdgeWidth_STRING, {
      if (skip_during_style_ui_sync("EdgeWidth_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()
        selection_flag <- last_selection_type()

        target_edges <- c()

        if (!is.null(selection_flag) && selection_flag == "edge") {
          if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
            target_edges <- selected_edges_list
          }
        }
        else if (!is.null(selection_flag) && selection_flag == "node") {
          if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
            current_edges <- edges()
            for (node_id in selected_nodes_list) {
              connected_edges <- current_edges$id[current_edges$from == node_id | current_edges$to == node_id]
              target_edges <- c(target_edges, connected_edges)
            }
          }
        }

        # Apply width changes
        if (length(target_edges) > 0) {
          target_edges <- unique(target_edges)
          current_edges <- edges()

          for (edge_id in target_edges) {
            if (edge_id %in% current_edges$id) {
              current_edges$width[current_edges$id == edge_id] <- input$EdgeWidth_STRING
            }
          }

          edges(current_edges)

          edges_to_update <- current_edges[current_edges$id %in% target_edges, ]

          visNetworkProxy("String_plot") %>%
            visUpdateEdges(edges = edges_to_update)

          debug_log(paste("Updated width for", length(target_edges), "edges"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating edge width:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Edge type - smart application
    observeEvent(input$EdgeType_STRING, {
      if (skip_during_style_ui_sync("EdgeType_STRING")) {
        return()
      }

      tryCatch({
        Sys.sleep(0.01)

        selected_nodes_list <- selectedNodes()
        selected_edges_list <- selectedEdges()

        target_edges <- c()

        debug_log(paste("EdgeType triggered - Nodes:", paste(selected_nodes_list, collapse = ", "), "Edges:", paste(selected_edges_list, collapse = ", ")), 2)

        # PRIORITY 1: If edges are selected, ONLY change selected edges
        if (!is.null(selected_edges_list) && length(selected_edges_list) > 0) {
          target_edges <- selected_edges_list
          debug_log(paste("Edge selection has priority - targeting ONLY selected edges:", paste(selected_edges_list, collapse = ", ")), 2)
        }

        # PRIORITY 2: If NO edges selected but nodes are selected, change connected edges
        else if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
          current_edges <- edges()
          for (node_id in selected_nodes_list) {
            connected_edges <- current_edges$id[current_edges$from == node_id | current_edges$to == node_id]
            target_edges <- c(target_edges, connected_edges)
          }
          debug_log(paste("No edge selection - targeting edges connected to nodes:", paste(unique(target_edges), collapse = ", ")), 2)
        }

        # Apply type changes
        if (length(target_edges) > 0) {
          target_edges <- unique(target_edges)
          current_edges <- edges()

          dashes_value <- switch(input$EdgeType_STRING,
                                 "Solid" = FALSE,
                                 "Dashed" = TRUE,
                                 "Dotted" = list(2, 10),
                                 "Double" = list(5, 5),
                                 FALSE)

          for (edge_id in target_edges) {
            if (edge_id %in% current_edges$id) {
              current_edges$dashes[current_edges$id == edge_id] <- list(dashes_value)
            }
          }

          edges(current_edges)

          # Update visualization
          edges_to_update <- current_edges[current_edges$id %in% target_edges, ]

          visNetworkProxy("String_plot") %>%
            visUpdateEdges(edges = edges_to_update)

          debug_log(paste("Updated type for", length(target_edges), "edges"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating edge type:", e$message), 1)
      })
    }, ignoreInit = TRUE)

  }

  list(
    apply_label_variant = apply_label_variant,
    register_styling_observers = register_styling_observers
  )
}
