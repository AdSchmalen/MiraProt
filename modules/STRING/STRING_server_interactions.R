# ==============================================================================
# STRING Module - Network Interaction Observers
# ==============================================================================
#
# Purpose:
#   Structural network interaction observers: layout changes, degree filtering,
#   node and edge selection tracking, node position updates (drag-and-drop),
#   control reset, and cluster selection via checkboxes. Also contains the
#   display output renderers (suggested identifiers, selected proteins table,
#   clipboard copy).
#
# Architecture role:
#   Factory function initialized inside modSTRINGServer. Registers all observers
#   that affect network topology, selection state, or displayed information.
#   Visual styling observers are in STRING_server_styling.R.
#
# File structure:
#   1. Factory function signature: initialize_STRING_interactions
#   2. register_interaction_observers:
#      a. Layout change observer (Layout_STRING)
#      b. Degree filter observer (EdgeNum_STRING)
#      c. Node selection observer (selectedNode_STRING)
#      d. Node position tracking (nodePositions_STRING)
#      e. Edge selection observer (selectedEdges_STRING)
#      f. Reset controls observer (resetButton_STRING)
#      g. Display outputs (geneSymbolList_STRING, selectedGene_STRING, copyBtn_STRING)
#      h. Cluster selection observer (ClustersProtein_STRING)
#   3. Return list
#
# Future developers:
#   - Visual property changes (node/edge appearance) live in STRING_server_styling.R.
#   - Protein input management lives in STRING_server_data_sources.R.
#   - Selection flag conventions: last_selection_type uses lowercase "node"/"edge";
#     current_selection_type uses capitalized "Node"/"Edge".
#   - All logging uses debug_log passed as a factory parameter.
# ==============================================================================

initialize_STRING_interactions <- function(
    input,
    output,
    session,
    ns,
    debug_log,
    rv,
    test_g,
    res_STRING,
    vis_data,
    nodes,
    edges,
    selected_data_STRING,
    original_nodes,
    original_edges,
    current_min_degree,
    selectedNodes,
    selectedEdges,
    current_selection_type,
    last_selection_type,
    apply_layout_transformation,
    update_node_ui_controls,
    update_edge_ui_controls,
    styling_ui_sync_state,
    ceb3_combined,
    cluster_labels_version,
    reset_string_organism_choices = NULL) {
  register_interaction_observers <- function() {
    # ========================================
    # Network Manipulation
    # ========================================

    # Layout change
    observeEvent(input$Layout_STRING, {
      req(test_g())

      debug_log(paste("Changing layout to:", input$Layout_STRING), 2)

      test_g_temp <- test_g()

      set.seed(1234)
      updated_layout_matrix <- apply_layout_transformation(test_g_temp,
                                                           input$Layout_STRING,
                                                           debug_log)

      current_nodes <- nodes()
      graph_node_ids <- igraph::V(test_g_temp)$name
      rownames(updated_layout_matrix) <- graph_node_ids
      layout_index <- match(current_nodes$id, graph_node_ids)
      valid_layout <- !is.na(layout_index)

      if (!all(valid_layout)) {
        debug_log(
          paste(
            "Layout nodes missing for:",
            paste(current_nodes$id[!valid_layout], collapse = ", ")
          ),
          1
        )
      }

      current_nodes$x[valid_layout] <- updated_layout_matrix[layout_index[valid_layout], 1] * 130
      current_nodes$y[valid_layout] <- updated_layout_matrix[layout_index[valid_layout], 2] * 130
      nodes(current_nodes)

      visNetworkProxy("String_plot") %>%
        visUpdateNodes(nodes = current_nodes)
    })

    # Degree filtering (REVERSIBLE + PRESERVES CUSTOMIZATIONS)
    observeEvent(input$EdgeNum_STRING, {
      tryCatch({
        min_degree <- input$EdgeNum_STRING

        debug_log(paste("Minimum degree changed to:", min_degree), 1)

        if (!is.null(original_nodes()) && !is.null(original_edges())) {

          orig_nodes <- original_nodes()
          orig_edges <- original_edges()
          current_nodes <- nodes()  # Get current customized nodes
          current_edges <- edges()  # Get current customized edges

          if (is.null(orig_nodes) || is.null(orig_edges) || is.null(min_degree)) {
            debug_log("Invalid filter parameters", 1)
            return()
          }

          debug_log(paste("STRING FILTER: Applying degree filter:", min_degree), 2)

          # Calculate degree for ALL nodes (including isolated ones)
          all_node_ids <- orig_nodes$id
          node_degrees <- sapply(all_node_ids, function(node_id) {
            sum(orig_edges$from == node_id) + sum(orig_edges$to == node_id)
          })
          names(node_degrees) <- all_node_ids

          # Find nodes that meet minimum degree requirement
          valid_nodes <- names(node_degrees)[node_degrees >= min_degree]

          # PRESERVE CUSTOMIZATIONS: Start with original nodes, then apply customizations
          filtered_nodes <- orig_nodes[orig_nodes$id %in% valid_nodes, ]

          # Transfer customizations from current nodes to filtered nodes
          if (!is.null(current_nodes) && nrow(current_nodes) > 0) {
            debug_log("Preserving node customizations and positions", 2)

            for (node_id in valid_nodes) {
              if (node_id %in% current_nodes$id) {
                current_row <- which(current_nodes$id == node_id)
                filtered_row <- which(filtered_nodes$id == node_id)

                # Preserve all customizable properties
                filtered_nodes$color.background[filtered_row] <- current_nodes$color.background[current_row]
                filtered_nodes$color.border[filtered_row] <- current_nodes$color.border[current_row]
                filtered_nodes$borderWidth[filtered_row] <- current_nodes$borderWidth[current_row]
                filtered_nodes$size[filtered_row] <- current_nodes$size[current_row]
                filtered_nodes$widthConstraint[filtered_row] <- current_nodes$widthConstraint[current_row]
                filtered_nodes$heightConstraint[filtered_row] <- current_nodes$heightConstraint[current_row]
                filtered_nodes$shape[filtered_row] <- current_nodes$shape[current_row]
                filtered_nodes$font.size[filtered_row] <- current_nodes$font.size[current_row]
                filtered_nodes$font.face[filtered_row] <- current_nodes$font.face[current_row]
                filtered_nodes$font.bold[filtered_row] <- current_nodes$font.bold[current_row]
                filtered_nodes$font.italic[filtered_row] <- current_nodes$font.italic[current_row]
                filtered_nodes$label[filtered_row] <- current_nodes$label[current_row]

                # PRESERVE POSITIONS (most important!)
                if ("x" %in% colnames(current_nodes) && "x" %in% colnames(filtered_nodes)) {
                  filtered_nodes$x[filtered_row] <- current_nodes$x[current_row]
                  filtered_nodes$y[filtered_row] <- current_nodes$y[current_row]
                }
              }
            }
          }

          # Filter edges (only keep edges between valid nodes)
          filtered_edges <- orig_edges[
            orig_edges$from %in% valid_nodes & orig_edges$to %in% valid_nodes,
          ]

          # Preserve edge customizations
          if (!is.null(current_edges) && nrow(current_edges) > 0) {
            debug_log("Preserving edge customizations", 2)

            for (i in 1:nrow(filtered_edges)) {
              edge_id <- filtered_edges$id[i]
              if (edge_id %in% current_edges$id) {
                current_edge_row <- which(current_edges$id == edge_id)

                # Preserve edge properties
                filtered_edges$color[i] <- current_edges$color[current_edge_row]
                filtered_edges$width[i] <- current_edges$width[current_edge_row]
                filtered_edges$dashes[i] <- current_edges$dashes[current_edge_row]
              }
            }
          }

          debug_log(paste("STRING FILTERED:", nrow(orig_nodes), "→", nrow(filtered_nodes), "nodes,",
                          nrow(orig_edges), "→", nrow(filtered_edges), "edges"), 1)

          # Update stored data
          nodes(filtered_nodes)
          edges(filtered_edges)
          current_min_degree(min_degree)

          # Clear selections since network structure changed
          selectedNodes(NULL)
          selectedEdges(NULL)
          last_selection_type(NULL)

          # UPDATE VISUALIZATION: Use visSetData to handle node removal/addition
          # (visUpdateNodes can't handle removed nodes)
          visNetworkProxy("String_plot") %>%
            visSetData(list(nodes = filtered_nodes, edges = filtered_edges))

          debug_log("STRING: Network filtered and updated (REVERSIBLE + CUSTOMIZATIONS PRESERVED)", 1)
        } else {
          debug_log("No original data available for filtering", 2)
        }

      }, error = function(e) {
        debug_log(paste("Error applying degree filter:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # Node selection observer - just updates selection
    observeEvent(input$selectedNode_STRING, {
      tryCatch({
        selected_id <- input$selectedNode_STRING

        debug_log(paste("=== selectedNode_STRING triggered ===:", paste(selected_id, collapse = ", ")), 2)

        if (!is.null(selected_id) && length(selected_id) > 0) {
          selectedNodes(selected_id)
          last_selection_type("node")  # SET FLAG IMMEDIATELY
          debug_log(paste("FLAG SET TO: node (user clicked node)"), 1)

          # Update UI controls
          if (length(selected_id) == 1) {
            current_nodes <- nodes()
            if (!is.null(current_nodes) && nrow(current_nodes) > 0) {
              selected_node_data <- current_nodes[current_nodes$id == selected_id, ]
              if (nrow(selected_node_data) > 0) {
                update_node_ui_controls(session, selected_node_data, debug_log, styling_ui_sync_state)
              }
            }
            # Also update edge color UI using the first connected edge
            current_edges <- edges()
            if (!is.null(current_edges) && nrow(current_edges) > 0) {
              connected_edge_ids <- current_edges$id[
                current_edges$from == selected_id | current_edges$to == selected_id
              ]
              if (length(connected_edge_ids) > 0) {
                first_edge_data <- current_edges[current_edges$id == connected_edge_ids[1], ]
                if (nrow(first_edge_data) > 0) {
                  update_edge_ui_controls(session, first_edge_data, debug_log, styling_ui_sync_state)
                }
              }
            }
          }
        } else {
          selectedNodes(NULL)
          debug_log("Node selection cleared", 2)
        }

      }, error = function(e) {
        debug_log(paste("Error in node selection observer:", e$message), 1)
      })
    })

    # ========================================
    # NODE POSITION TRACKING
    # ========================================

    # Observe position changes from drag and drop
    observeEvent(input$nodePositions_STRING, {
      tryCatch({
        new_positions <- input$nodePositions_STRING

        if (!is.null(new_positions) && length(new_positions) > 0) {
          debug_log("Node positions updated via drag&drop", 1)

          # Update node positions in the stored data
          current_nodes <- nodes()

          for (node_id in names(new_positions)) {
            if (node_id %in% current_nodes$id) {
              # Update x and y coordinates
              current_nodes$x[current_nodes$id == node_id] <- new_positions[[node_id]]$x
              current_nodes$y[current_nodes$id == node_id] <- new_positions[[node_id]]$y
            }
          }

          # Save updated node data with new positions
          nodes(current_nodes)

          # Set flag to "node" since user interacted with nodes via drag
          last_selection_type("node")
          debug_log("Flag set to 'node' after drag&drop", 2)

          debug_log(paste("Updated positions for", length(new_positions), "nodes"), 1)
        }

      }, error = function(e) {
        debug_log(paste("Error updating node positions:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # ========================================
    # EDGE SELECTION OBSERVER (CORRECT NAMESPACE)
    # ========================================

    # Edge selection observer - just updates selection
    observeEvent(input$selectedEdges_STRING, {
      tryCatch({
        selected_ids <- input$selectedEdges_STRING

        debug_log(paste("=== selectedEdges_STRING triggered ===:", paste(selected_ids, collapse = ", ")), 2)

        if (!is.null(selected_ids) && length(selected_ids) > 0) {
          selectedEdges(selected_ids)

          # Check if this is a direct edge click or auto-selection from node click
          current_nodes <- selectedNodes()

          if (is.null(current_nodes) || length(current_nodes) == 0) {
            # No nodes selected - this is definitely a direct edge click
            last_selection_type("edge")
            debug_log(paste("FLAG SET TO: edge (direct edge click, no nodes)"), 1)
          } else {
            # Nodes are selected - check if selected edges are exactly the connected edges
            current_edges_data <- edges()
            connected_edges <- c()

            for (node_id in current_nodes) {
              node_edges <- current_edges_data$id[current_edges_data$from == node_id | current_edges_data$to == node_id]
              connected_edges <- c(connected_edges, node_edges)
            }
            connected_edges <- unique(connected_edges)

            # If selected edges are NOT exactly the connected edges, it's a direct edge click
            if (!setequal(selected_ids, connected_edges)) {
              last_selection_type("edge")
              debug_log(paste("FLAG SET TO: edge (direct edge click, different from auto-selection)"), 1)
            } else {
              debug_log(paste("FLAG KEPT AS:", last_selection_type(), "(edges auto-selected from node)"), 1)
            }
          }

          # Update UI controls
          if (length(selected_ids) == 1) {
            current_edges <- edges()
            if (!is.null(current_edges) && nrow(current_edges) > 0) {
              selected_edge_data <- current_edges[current_edges$id == selected_ids[1], ]

              if (nrow(selected_edge_data) > 0) {
                update_edge_ui_controls(session, selected_edge_data, debug_log, styling_ui_sync_state)
              }
            }
          }
        } else {
          selectedEdges(NULL)
          debug_log("Edge selection cleared", 2)
        }

      }, error = function(e) {
        debug_log(paste("Error in edge selection observer:", e$message), 1)
      })
    })

    # ========================================
    # Reset controls with static UI defaults
    # ========================================

    observeEvent(input$resetButton_STRING, {
      tryCatch({
        debug_log("Resetting STRING controls to UI defaults", 1)

        # General options with static defaults. Metadata-populated inputs that
        # start with choices = NULL (for example label_variant_STRING) are not
        # touched here.
        updateSelectInput(session, inputId = "version_STRING", selected = "12.0")
        updateNumericInput(session, inputId = "score_STRING", value = 500)
        updateSelectInput(session, inputId = "edgetype_STRING", selected = "Full")
        if (is.function(reset_string_organism_choices)) {
          reset_string_organism_choices()
        } else {
          updateSelectInput(session, inputId = "organism_STRING", selected = "9606")
        }
        updateNumericInput(session, inputId = "neighbor_count_STRING", value = 0)
        updateSelectInput(session, inputId = "neighbor_strategy_STRING", selected = "most_edges")

        # Customization controls.
        updateSelectInput(session, inputId = "Layout_STRING", selected = "fr")
        updateNumericInput(session, inputId = "EdgeNum_STRING", value = 0)

        # Styling controls should visibly reset in the UI without firing the
        # styling observers that apply those values to the current selection.
        with_style_ui_sync(session, styling_ui_sync_state, debug_log, {
          update_synced_colour_input(session, styling_ui_sync_state, "Color_1_STRING", value = "#BEBEBE")
          update_synced_colour_input(session, styling_ui_sync_state, "Color_2_STRING", value = "#BEBEBE")
          update_synced_select_input(session, styling_ui_sync_state, "Shape_STRING", selected = "Circle")
          update_synced_numeric_input(session, styling_ui_sync_state, "Size_1_STRING", value = 50)
          update_synced_numeric_input(session, styling_ui_sync_state, "Frame_STRING", value = 2)
          update_synced_colour_input(session, styling_ui_sync_state, "EdgeColor_STRING", value = "#BEBEBE")
          update_synced_select_input(session, styling_ui_sync_state, "EdgeType_STRING", selected = "Solid")
          update_synced_numeric_input(session, styling_ui_sync_state, "EdgeWidth_STRING", value = 2)
          update_synced_select_input(session, styling_ui_sync_state, "FontType_STRING", selected = "Arial")
          update_synced_select_input(session, styling_ui_sync_state, "FontStyle_STRING", selected = "Plain text")
          update_synced_numeric_input(session, styling_ui_sync_state, "TextSize_STRING", value = 12)
        })

        showNotification("STRING controls reset to defaults.", type = "message", duration = 3)
        debug_log("STRING controls reset to UI defaults", 1)
      }, error = function(e) {
        debug_log(paste("Error resetting STRING controls:", e$message), 1)
        showNotification("Error resetting STRING controls", type = "error", duration = 3)
      })
    })

    # ========================================
    # Display Functions
    # ========================================

    # Dynamic search label follows the currently selected identifier column.
    output$search_identifier_label_STRING <- renderText({

      selected_identifier <-
        input$identifier_type_STRING

      current_data <-
        tryCatch(
          rv$data_mod,
          error = function(e) NULL
        )

      identifier_available <-
        is.character(selected_identifier) &&
        length(selected_identifier) == 1L &&
        !is.na(selected_identifier) &&
        nzchar(selected_identifier) &&
        is.data.frame(current_data) &&
        selected_identifier %in% names(current_data)

      if (!identifier_available) {
        return(
          "Select identifier column"
        )
      }

      paste0(
        "Search for ",
        selected_identifier,
        ":"
      )
    })

    # Display suggested identifiers
    output$geneSymbolList_STRING <- renderPrint({
      quiet_log <- function(...) invisible(NULL)

      tryCatch({
        req(rv$data_mod, rv$data_def)

        data <- rv$data_mod
        selected_identifier <- input$identifier_type_STRING
        identifier <- c()
        gene_symbols_text <- ""

        quiet_log("Generating suggested identifiers (PREFIX/partial matches enabled)", 2)

        # Raw query from the text area
        query_raw <- input$input_STRING

        if (!is.null(query_raw) && nzchar(trimws(query_raw)) &&
            !is.null(selected_identifier) && nzchar(selected_identifier)) {

          # Extract tokens (split on commas, semicolons, whitespace, newlines, tabs)
          tokens <- unique(trimws(unlist(strsplit(query_raw, "[,;\\n\\r\\t ]+"))))
          tokens <- tokens[nzchar(tokens)]

          if (length(tokens) > 0) {
            # Candidate values from the chosen identifier column
            vec <- data[[selected_identifier]]
            vec <- vec[!is.na(vec)]

            # Build a prefix OR-pattern: ^token1|^token2|...
            pattern <- paste0("^", tokens, collapse = "|")

            # Prefix/partial matches (case-insensitive)
            idx <- grepl(pattern, vec, ignore.case = TRUE)
            identifier <- sort(unique(vec[idx]))

            gene_symbols_text <- paste(identifier, collapse = "\n")
            if (length(identifier) == 0) {
              quiet_log("No local dataset prefix suggestions found; entered identifiers may still map through STRING", 1)
            } else {
              quiet_log(paste("Found", length(identifier), "local dataset prefix suggestions"), 1)
            }
          }
        }

        cat(gene_symbols_text)

      }, error = function(e) {
        quiet_log(paste("Error in geneSymbolList_STRING:", e$message), 1)
        cat("")
      })
    })

    # Display selected proteins with per-protein remove buttons
    output$selectedGene_STRING <- renderUI({
      tryCatch({
        selected_data <- selected_data_STRING()

        if (is.null(selected_data) || !is.data.frame(selected_data) || nrow(selected_data) == 0) {
          debug_log("No selected proteins to display", 2)
          return(
            tags$div(
              style = paste(
                "height: 150px; overflow-y: auto; border: 1px solid #ddd;",
                "border-radius: 4px; padding: 6px; background: #fff;"
              ),
              tags$p("No proteins selected",
                     style = "color: #666; margin: 0; text-align: center; padding-top: 30px;")
            )
          )
        }

        proteins <- as.character(selected_data[[1]])
        debug_log(paste("Rendering", length(proteins), "proteins with remove buttons"), 2)

        # Build one row per protein; each button sets a single shared Shiny input
        protein_rows <- lapply(proteins, function(protein) {
          js_call <- sprintf(
            "Shiny.setInputValue(\"%s\", \"%s\", {priority: \"event\"});",
            ns("remove_protein_click"),
            gsub('"', '\\\\"', protein, fixed = TRUE)
          )
          tags$div(
            style = paste(
              "display: flex; align-items: center; justify-content: space-between;",
              "padding: 2px 4px; border-bottom: 1px solid #eee; font-size: 12px;"
            ),
            tags$span(protein),
            tags$button(
              class = "btn btn-danger btn-xs",
              style = "padding: 1px 6px; font-size: 11px; line-height: 1.4;",
              onclick = js_call,
              icon("times")
            )
          )
        })

        tags$div(
          style = paste(
            "height: 150px; overflow-y: auto; overflow-x: auto;",
            "border: 1px solid #ddd; border-radius: 4px;",
            "background: #fff; padding: 4px;"
          ),
          protein_rows
        )
      }, error = function(e) {
        debug_log(paste("Error in selectedGene_STRING renderUI:", e$message), 1)
        tags$p("Display error", style = "color: red; font-size: 12px;")
      })
    })

    # Conditionally render cluster selection checkboxes in the Customization panel.
    # Returns NULL (hidden) until a network with multi-member clusters is available.
    output$cluster_section_STRING <- renderUI({
      ceb <- res_STRING()$cluster
      if (is.null(ceb)) return(NULL)

      graph <- res_STRING()$STRING_Graph
      if (is.null(graph)) return(NULL)

      node_names        <- igraph::V(graph)$name
      membership_vector <- igraph::membership(ceb)
      cluster_list      <- split(node_names, membership_vector)
      cluster_list      <- cluster_list[lengths(cluster_list) >= 2]

      if (length(cluster_list) == 0) return(NULL)

      # Map raw node IDs to current display labels.
      # Depend on cluster_labels_version() (incremented only when labels actually
      # change) rather than nodes() directly, so that node styling updates (color,
      # shape, etc.) do not cause this renderUI to re-render and lose the checkbox
      # selection state.
      cluster_labels_version()
      current_nodes <- isolate(nodes())
      if (!is.null(current_nodes) && nrow(current_nodes) > 0) {
        label_map <- stats::setNames(current_nodes$original_label, current_nodes$id)
        cluster_display_list <- lapply(cluster_list, function(ids) {
          labels <- label_map[ids]
          labels[is.na(labels)] <- ids[is.na(labels)]
          labels
        })
      } else {
        cluster_display_list <- cluster_list
      }

      cluster_names <- ceb3_combined(cluster_display_list, debug_log)

      tagList(
        hr(),
        h4("Clusters"),
        checkboxGroupInput(ns("ClustersProtein_STRING"),
                           label = NULL,
                           choices = cluster_names)
      )
    })

    # Copy the currently suggested identifiers to the clipboard.
    # Matching intentionally mirrors geneSymbolList_STRING above.
    observeEvent(input$copyBtn_STRING, {
      tryCatch({
        req(rv$data_mod, rv$data_def)

        data <- rv$data_mod
        selected_identifier <- input$identifier_type_STRING
        query_raw <- input$input_STRING

        if (is.null(query_raw) ||
            !nzchar(trimws(query_raw)) ||
            is.null(selected_identifier) ||
            !nzchar(selected_identifier)) {
          showNotification(
            "No data to copy",
            type = "warning",
            duration = 5
          )
          return()
        }

        tokens <- unique(
          trimws(
            unlist(
              strsplit(
                query_raw,
                "[,;\\n\\r\\t ]+"
              )
            )
          )
        )
        tokens <- tokens[nzchar(tokens)]

        if (length(tokens) == 0L) {
          showNotification(
            "No valid identifiers to copy",
            type = "warning",
            duration = 5
          )
          return()
        }

        vec <- data[[selected_identifier]]
        vec <- vec[!is.na(vec)]

        pattern <- paste0(
          "^",
          tokens,
          collapse = "|"
        )

        idx <- grepl(
          pattern,
          vec,
          ignore.case = TRUE
        )

        identifier <- sort(
          unique(
            vec[idx]
          )
        )

        if (length(identifier) > 0L) {
          gene_symbols_text <- paste(
            identifier,
            collapse = "\n"
          )

          session$sendCustomMessage(
            type = "copyToClipboard",
            message = gene_symbols_text
          )

          showNotification(
            paste(
              "Copied",
              length(identifier),
              "suggested identifiers to clipboard!"
            ),
            type = "message",
            duration = 5
          )

          debug_log(
            paste(
              "Copied",
              length(identifier),
              "suggested identifiers to clipboard (PREFIX/partial matches)"
            ),
            1
          )
        } else {
          showNotification(
            "No suggested identifiers to copy",
            type = "warning",
            duration = 5
          )
        }

      }, error = function(e) {
        debug_log(
          paste(
            "Error in copyBtn_STRING:",
            e$message
          ),
          1
        )
        showNotification(
          "Error copying to clipboard",
          type = "error",
          duration = 5
        )
      })
    })

    # ========================================
    # CLUSTER SELECTION VIA CHECKBOXES
    # ========================================

    # Observer for cluster selection via checkboxes - IMPROVED
    observeEvent(input$ClustersProtein_STRING, {
      # Force immediate evaluation of checkbox state
      checkbox_values <- input$ClustersProtein_STRING
      cluster_diagnostics_enabled <- isTRUE(getOption("miraprot.string.cluster_diagnostics", FALSE))

      if (cluster_diagnostics_enabled) {
        debug_log("=== CLUSTER OBSERVER TRIGGERED ===", 2)
        debug_log(paste("Current checkbox values:", if (is.null(checkbox_values)) "NULL" else paste(checkbox_values, collapse = ", ")), 2)
        debug_log(paste("Length of selection:", if (is.null(checkbox_values)) 0 else length(checkbox_values)), 2)
      }

      tryCatch({
        selected_clusters <- checkbox_values

        if (is.null(selected_clusters) || length(selected_clusters) == 0) {
          debug_log("Cluster selection cleared: no clusters selected", 2)
        } else {
          debug_log(sprintf("Cluster selection changed: %d clusters", length(selected_clusters)), 2)
        }

        # ALWAYS clear existing selection first
        selectedNodes(NULL)
        selectedEdges(NULL)
        last_selection_type(NULL)

        # Clear visual selection first
        visNetworkProxy("String_plot") %>%
          visSelectNodes(id = character(0)) %>%
          visSelectEdges(id = character(0))

        debug_log("Selection cleared in cluster observer", 2)

        # If no clusters selected, stop here (selection already cleared)
        if (is.null(selected_clusters) || length(selected_clusters) == 0) {
          debug_log("All clusters deselected - selection cleared and stopping", 1)
          return()
        }

        # Rest of the code for when clusters ARE selected...
        # Get cluster information from stored graph
        if (!is.null(test_g()) && !is.null(res_STRING())) {
          test_g_temp <- test_g()

          # Use the clustering result stored during network creation instead of
          # recomputing cluster_edge_betweenness (which is O(m*n + n^2 log n))
          # every time a cluster checkbox is toggled.
          ceb <- res_STRING()$cluster
          membership_vector <- igraph::membership(ceb)
          node_names <- igraph::V(test_g_temp)$name

          # Create cluster list (exclude single-member clusters, matching renderUI)
          cluster_list  <- split(node_names, membership_vector)
          cluster_list  <- cluster_list[lengths(cluster_list) >= 2]

          # Apply the same ID-to-label mapping as the renderUI so that
          # cluster_names matches the checkbox values the user selected.
          current_nodes_obs <- nodes()
          if (!is.null(current_nodes_obs) && nrow(current_nodes_obs) > 0) {
            label_map_obs <- stats::setNames(current_nodes_obs$original_label, current_nodes_obs$id)
            cluster_display_list_obs <- lapply(cluster_list, function(ids) {
              labels <- label_map_obs[ids]
              labels[is.na(labels)] <- ids[is.na(labels)]
              labels
            })
          } else {
            cluster_display_list_obs <- cluster_list
          }

          cluster_names <- ceb3_combined(cluster_display_list_obs, debug_log)

          debug_log(paste("Available cluster names:", paste(cluster_names, collapse = ", ")), 2)
          debug_log(paste("Selected cluster names:", paste(selected_clusters, collapse = ", ")), 2)

          # Find indices of selected clusters
          selected_indices <- which(cluster_names %in% selected_clusters)

          if (length(selected_indices) > 0) {
            # Get all nodes from ONLY the currently selected clusters
            selected_cluster_nodes <- c()
            for (idx in selected_indices) {
              cluster_nodes <- cluster_list[[idx]]
              selected_cluster_nodes <- c(selected_cluster_nodes, cluster_nodes)
              debug_log(paste("Including cluster", idx, "nodes:", paste(cluster_nodes, collapse = ", ")), 2)
            }

            selected_cluster_nodes <- unique(selected_cluster_nodes)
            debug_log(paste("COMPLETE RECALC: All selected cluster nodes:", paste(selected_cluster_nodes, collapse = ", ")), 2)

            # Find edges connected to ONLY the currently selected nodes
            current_edges <- edges()
            if (!is.null(current_edges) && nrow(current_edges) > 0) {
              connected_edges <- c()
              for (node_id in selected_cluster_nodes) {
                node_edges <- current_edges$id[current_edges$from == node_id | current_edges$to == node_id]
                connected_edges <- c(connected_edges, node_edges)
              }
              connected_edges <- unique(connected_edges)
              debug_log(paste("COMPLETE RECALC: Connected edges:", paste(connected_edges, collapse = ", ")), 2)

              # Set NEW selections (completely replacing old ones)
              selectedNodes(selected_cluster_nodes)
              selectedEdges(connected_edges)
              last_selection_type("node")  # Flag as node selection

              debug_log(paste("CLUSTER SELECTION COMPLETE: Selected", length(selected_cluster_nodes),
                              "nodes and", length(connected_edges), "edges from",
                              length(selected_clusters), "clusters"), 1)
              debug_log("FLAG SET TO: node (cluster selection)", 2)

              # Update visNetwork selection with NEW selection
              visNetworkProxy("String_plot") %>%
                visSelectNodes(id = selected_cluster_nodes) %>%
                visSelectEdges(id = connected_edges)

            } else {
              debug_log("No edges data available for cluster selection", 1)
            }
          } else {
            debug_log("No matching clusters found - selection remains cleared", 1)
          }
        } else {
          debug_log("No graph data available for cluster selection", 1)
        }

      }, error = function(e) {
        debug_log(paste("Error in cluster selection:", e$message), 1)
      })
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


  }

  list(
    register_interaction_observers = register_interaction_observers
  )
}
