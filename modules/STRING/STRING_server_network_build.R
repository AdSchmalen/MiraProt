# ==============================================================================
# STRING Module - Network Build Observer
# ==============================================================================
#
# Purpose:
#   Registers the main network creation observer (create_STRING button).
#   Orchestrates the full pipeline: input normalization, STRINGdb protein mapping,
#   optional neighbor expansion, interaction retrieval, igraph construction,
#   cluster detection, visualization data preparation, and initial rendering.
#
# Architecture role:
#   Factory function initialized inside modSTRINGServer after interactions and
#   styling are initialized. Receives apply_label_variant from the styling
#   factory so that the preferred label source can be applied after network
#   creation. The observer uses withProgress for user feedback.
#
# File structure:
#   1. Factory function signature: initialize_STRING_network_build
#   2. Internal helpers: normalize_network_build_inputs, determine_label_column,
#      validate_mapping_result, resolve_neighbor_strategy, build_go_path
#   3. register_network_build_observer: the main create_STRING observer
#      (all network creation logic is contained within this single observer)
#   4. Return list
#
# Future developers:
#   - The create_STRING observer is the longest single block in this module.
#     Keep all network building steps inside it to preserve withProgress tracking.
#   - apply_label_variant must be passed from the styling factory, not defined here.
#   - All logging uses debug_log passed as a factory parameter.
# ==============================================================================

initialize_STRING_network_build <- function(
    input,
    output,
    session,
    ns,
    debug_log,
    selected_protein_vector,
    vector_plotted_STRING,
    parse_neighbor_count_input,
    map_proteins_with_fallback,
    original_nodes,
    original_edges,
    current_min_degree,
    test_g,
    res_STRING,
    vis_data,
    nodes,
    edges,
    label_alias_data,
    label_initial_labels,
    load_string_aliases,
    apply_label_variant,
    create_vis_network,
    ceb3_combined,
    identifier_type_used = NULL) {

  normalize_network_build_inputs <- function() {
    selected_proteins <- selected_protein_vector()
    selected_proteins <- unique(selected_proteins[!is.na(selected_proteins) & nzchar(selected_proteins)])

    list(
      selected_proteins = selected_proteins,
      version = input$version_STRING,
      score = input$score_STRING,
      edge_type = input$edgetype_STRING,
      identifier = input$identifier_type_STRING,
      organism_species_id = input$organism_STRING,
      neighbor_count = parse_neighbor_count_input(input)
    )
  }


  determine_label_column <- function(genes_mapped) {
    if ("query" %in% names(genes_mapped)) {
      return("query")
    }
    if ("Symbol.ID" %in% names(genes_mapped)) {
      return("Symbol.ID")
    }
    names(genes_mapped)[1]
  }

  validate_mapping_result <- function(genes_mapped, proteins) {
    debug_log(paste("Mapped", nrow(genes_mapped), "of", nrow(proteins), "proteins"), 1)

    if (nrow(genes_mapped) == 0) {
      showNotification(
        "No proteins could be mapped to STRING. Check the identifier type and gene symbols.",
        type = "warning",
        duration = 6
      )
      debug_log("STRING mapping returned 0 rows; aborting network creation", 1)
      return(FALSE)
    }

    if (!("STRING_id" %in% names(genes_mapped))) {
      showNotification(
        "STRING mapping failed: STRING_id column missing.",
        type = "error",
        duration = 6
      )
      debug_log(
        paste("STRING mapping missing STRING_id. Columns:", paste(names(genes_mapped), collapse = ", ")),
        1
      )
      return(FALSE)
    }

    TRUE
  }

  resolve_neighbor_strategy <- function() {
    neighbor_strategy <- "most_edges"
    if ("neighbor_strategy_STRING" %in% names(input)) {
      neighbor_strategy <- input$neighbor_strategy_STRING
      if (is.null(neighbor_strategy) || !nzchar(neighbor_strategy)) {
        neighbor_strategy <- "most_edges"
      }
    }
    neighbor_strategy
  }

  build_go_path <- function(genes_mapped, label_column, extra_neighbors, neighbor_labels) {
    go_path <- data.frame(
      string_id = genes_mapped$STRING_id,
      label = genes_mapped[[label_column]],
      stringsAsFactors = FALSE
    )

    if (length(extra_neighbors) > 0) {
      neighbor_df <- data.frame(
        string_id = extra_neighbors,
        label = neighbor_labels,
        stringsAsFactors = FALSE
      )
      go_path <- unique(rbind(go_path, neighbor_df))
    }

    go_path
  }


  network_build_cache <- new.env(parent = emptyenv())
  network_build_cache$entries <- list()
  network_build_cache$order <- character(0)
  network_build_cache$max_entries <- 3L

  make_network_build_cache_key <- function(normalized_inputs, neighbor_strategy) {
    key_fields <- list(
      selected_proteins = sort(unique(as.character(normalized_inputs$selected_proteins))),
      version = as.character(normalized_inputs$version),
      score = as.character(normalized_inputs$score),
      edge_type = as.character(normalized_inputs$edge_type),
      organism_species_id = as.character(normalized_inputs$organism_species_id),
      neighbor_count = as.integer(normalized_inputs$neighbor_count),
      neighbor_strategy = as.character(neighbor_strategy),
      identifier = as.character(normalized_inputs$identifier)
    )

    paste(
      vapply(names(key_fields), function(field_name) {
        field_values <- key_fields[[field_name]]
        field_values[is.na(field_values)] <- "<NA>"
        paste0(
          field_name, "=",
          paste(encodeString(field_values, quote = '"'), collapse = ",")
        )
      }, character(1)),
      collapse = "|"
    )
  }

  get_cached_network_build <- function(cache_key) {
    cached <- network_build_cache$entries[[cache_key]]
    if (is.null(cached)) {
      return(NULL)
    }
    network_build_cache$order <- c(cache_key, setdiff(network_build_cache$order, cache_key))
    cached
  }

  set_cached_network_build <- function(cache_key, value) {
    network_build_cache$entries[[cache_key]] <- value
    network_build_cache$order <- c(cache_key, setdiff(network_build_cache$order, cache_key))

    while (length(network_build_cache$order) > network_build_cache$max_entries) {
      stale_key <- network_build_cache$order[[length(network_build_cache$order)]]
      network_build_cache$entries[[stale_key]] <- NULL
      network_build_cache$order <- head(network_build_cache$order, -1)
    }

    invisible(value)
  }

  register_network_build_observer <- function() {
    observeEvent(input$create_STRING, {
      debug_log("Starting STRING network creation", 1)

      withProgress(message = "Creating STRING network: ", value = 0, {

        # Step 1: Initial Data Setup
        incProgress(0.1, detail = "Preparing input data...")

        original_nodes(NULL)
        original_edges(NULL)
        current_min_degree(0)
        debug_log("Original data reset for new network", 2)

        normalized_inputs <- normalize_network_build_inputs()
        input_STRING_selected <- normalized_inputs$selected_proteins

        if (length(input_STRING_selected) == 0) {
          showNotification('No proteins selected. Did you remember to add your selection to the list of selected proteins?',
                           type = "error", duration = 5)
          debug_log("Network creation aborted - no proteins selected", 1)
          return(NULL)
        }
        vector_plotted_STRING(input_STRING_selected)

        input_version_STRING_selected  <- normalized_inputs$version
        input_score_STRING_selected    <- normalized_inputs$score
        input_edgetype_STRING_selected <- normalized_inputs$edge_type
        input_organism_species_selected <- normalized_inputs$organism_species_id
        selected_identifier <- normalized_inputs$identifier
        if (is.function(identifier_type_used)) {
          tryCatch(
            identifier_type_used(
              if (!is.null(selected_identifier) && nzchar(selected_identifier)) selected_identifier else NA_character_
            ),
            error = function(e) NULL
          )
        }
        set.seed(1234)

        debug_log(paste("Selected", length(input_STRING_selected), "proteins for network"), 2)
        debug_log(paste("Parameters: Version =", input_version_STRING_selected,
                        ", Score =", input_score_STRING_selected,
                        ", Edge type =", input_edgetype_STRING_selected,
                        ", Species ID =", input_organism_species_selected), 2)

        # Step 2: Mapping Proteins to STRING database
        incProgress(0.1, detail = "Mapping proteins to STRING database...")
        selected_identifier_local <- normalized_inputs$identifier
        neighbor_count <- normalized_inputs$neighbor_count
        neighbor_strategy <- resolve_neighbor_strategy()
        cache_key <- make_network_build_cache_key(normalized_inputs, neighbor_strategy)
        cached_network <- get_cached_network_build(cache_key)

        if (!is.null(cached_network)) {
          debug_log(paste("Reusing cached STRING network build for key", cache_key), 1)

          test_g(cached_network$graph)
          res_STRING(cached_network$res_STRING)
          original_nodes(cached_network$nodes)
          original_edges(cached_network$edges)
          vis_data(list(nodes = cached_network$nodes, edges = cached_network$edges))
          nodes(cached_network$nodes)
          edges(cached_network$edges)
          label_initial_labels(cached_network$initial_labels)
          label_alias_data(cached_network$alias_info)

          updateSelectInput(session, "label_variant_STRING",
                            choices = cached_network$label_choices,
                            selected = cached_network$selected_label_variant)
          apply_label_variant(cached_network$selected_label_variant)

          incProgress(0.6, detail = "Reusing cached network data...")
          debug_log(sprintf(
            "STRING network cache hit | key: %s | nodes: %d | edges: %d | label source: %s",
            cache_key,
            nrow(cached_network$nodes),
            nrow(cached_network$edges),
            cached_network$selected_label_variant
          ), level = 0)

          incProgress(0.1, detail = "Rendering network...")
          output$String_plot <- renderVisNetwork({
            tryCatch({
              req(nodes(), edges())
              current_nodes <- nodes()
              current_edges <- edges()

              debug_log(paste("Rendering cached visNetwork with", nrow(current_nodes), "nodes and", nrow(current_edges), "edges"), 1)

              vis_result <- create_vis_network(current_nodes, current_edges, ns = ns)
              debug_log("Cached visNetwork created successfully", 1)
              return(vis_result)
            }, error = function(e) {
              debug_log(paste("CRITICAL ERROR creating cached visNetwork:", e$message), 1)
              return(NULL)
            })
          })

          incProgress(0.1, detail = "Preparing clusters for selection...")
          debug_log("Cached network creation completed successfully", 1)
          return(NULL)
        }
        debug_log(paste("No cached STRING network build found for key", cache_key), 2)

        tryCatch({
          proteins <- unique(data.frame(query = input_STRING_selected, stringsAsFactors = FALSE))

          mapping_result <- map_proteins_with_fallback(
            proteins = proteins,
            version_selected = input_version_STRING_selected,
            score_selected = input_score_STRING_selected,
            edge_type_selected = input_edgetype_STRING_selected,
            species_id_selected = input_organism_species_selected
          )
          genes_mapped <- mapping_result$genes_mapped
          string_db <- mapping_result$string_db
          string_db_version_used <- mapping_result$string_db_version_used
          string_species_id_used <- mapping_result$species_id_used

          # genes_mapped <- string_db$map(proteins, "Symbol.ID", removeUnmappedRows = TRUE)
          if (!validate_mapping_result(genes_mapped, proteins)) {
            return(NULL)
          }
          label_column <- determine_label_column(genes_mapped)

          # Optional neighbors
          extra_neighbors <- character(0)
          neighbor_labels <- character(0)
          alias_df <- NULL
          if (neighbor_count > 0 && nrow(genes_mapped) > 0) {
            seed_ids <- unique(genes_mapped$STRING_id)
            all_nbs <- unique(as.character(unlist(lapply(seed_ids, function(sid) {
              nb <- string_db$get_neighbors(sid)
              as.character(nb)
            }))))
            if (length(all_nbs) > 0) {
              extra_candidates <- setdiff(all_nbs, seed_ids)
              if (length(extra_candidates) > 0) {
                candidate_ids <- unique(c(seed_ids, extra_candidates))
                candidate_edges <- string_db$get_interactions(candidate_ids)
                candidate_edges <- unique(data.frame(
                  from = candidate_edges$from,
                  to = candidate_edges$to,
                  stringsAsFactors = FALSE
                ))
                candidate_graph <- igraph::graph_from_data_frame(candidate_edges, directed = FALSE)
                candidate_degree <- igraph::degree(candidate_graph, v = extra_candidates)
                candidate_degree[is.na(candidate_degree)] <- 0
                seed_edge_mask <- (candidate_edges$from %in% seed_ids & candidate_edges$to %in% extra_candidates) |
                  (candidate_edges$to %in% seed_ids & candidate_edges$from %in% extra_candidates)
                seed_edges <- candidate_edges[seed_edge_mask, , drop = FALSE]
                seed_degree <- table(c(seed_edges$from, seed_edges$to))
                seed_degree <- seed_degree[setdiff(names(seed_degree), seed_ids)]
                seed_degree <- setNames(as.integer(seed_degree), names(seed_degree))
                candidate_seed_degree <- setNames(rep(0L, length(extra_candidates)), extra_candidates)
                candidate_seed_degree[names(seed_degree)] <- seed_degree
                ordered_candidates <- extra_candidates[order(-candidate_seed_degree, -candidate_degree, extra_candidates)]
                if (identical(neighbor_strategy, "connecting_clusters")) {
                  top_candidates <- head(ordered_candidates, min(100, length(ordered_candidates)))
                  eligible_candidates <- top_candidates[candidate_seed_degree[top_candidates] >= 2]
                  bridge_scores <- setNames(rep(0L, length(eligible_candidates)), eligible_candidates)
                  ceb_candidate <- NULL
                  if (length(eligible_candidates) > 0) {
                    candidate_ids_bridge <- unique(c(seed_ids, eligible_candidates))
                    candidate_edges_bridge <- string_db$get_interactions(candidate_ids_bridge)
                    candidate_edges_bridge <- unique(data.frame(
                      from = candidate_edges_bridge$from,
                      to = candidate_edges_bridge$to,
                      stringsAsFactors = FALSE
                    ))
                    candidate_graph_bridge <- igraph::graph_from_data_frame(candidate_edges_bridge, directed = FALSE)
                    if (igraph::ecount(candidate_graph_bridge) > 0) {
                      ceb_candidate <- tryCatch(
                        igraph::cluster_edge_betweenness(candidate_graph_bridge),
                        error = function(e) {
                          debug_log(paste("Neighbor clustering failed:", e$message), 1)
                          NULL
                        }
                      )
                    }
                  }
                  if (!is.null(ceb_candidate) && length(eligible_candidates) > 0) {
                    membership_vector <- igraph::membership(ceb_candidate)
                    if (length(unique(membership_vector)) > 1) {
                      edges_df <- as.data.frame(igraph::as_edgelist(candidate_graph_bridge), stringsAsFactors = FALSE)
                      colnames(edges_df) <- c("from", "to")
                      edges_df$from_cluster <- membership_vector[match(edges_df$from, names(membership_vector))]
                      edges_df$to_cluster <- membership_vector[match(edges_df$to, names(membership_vector))]
                      edges_df$inter_cluster <- edges_df$from_cluster != edges_df$to_cluster
                      bridge_scores <- vapply(eligible_candidates, function(node_id) {
                        sum(edges_df$inter_cluster & (edges_df$from == node_id | edges_df$to == node_id))
                      }, integer(1))
                    } else {
                      debug_log("Neighbor clustering produced a single cluster; falling back to degree ranking", 1)
                    }
                  }
                  if (any(bridge_scores > 0)) {
                    bridge_order <- order(-bridge_scores, -candidate_degree[names(bridge_scores)], names(bridge_scores))
                    ordered_candidates <- c(names(bridge_scores)[bridge_order],
                                            setdiff(ordered_candidates, names(bridge_scores)))
                  } else {
                    debug_log("No inter-cluster bridge scores found; falling back to degree ranking", 1)
                  }
                }
                extra_neighbors <- head(ordered_candidates, neighbor_count)
                neighbor_labels <- extra_neighbors

                # Load aliases for seeds + neighbors in a single call.
                # This result is also reused later for the label variant UI,
                # avoiding a second read of the same gzipped alias file.
                alias_df <- load_string_aliases(
                  string_db, unique(c(seed_ids, extra_neighbors)),
                  string_db_version_used, debug_log
                )

                if (!is.null(alias_df) && !is.data.frame(alias_df)) {
                  alias_df <- tryCatch(
                    as.data.frame(alias_df, stringsAsFactors = FALSE),
                    error = function(e) {
                      debug_log(paste("Neighbor alias normalization failed:", e$message), 1)
                      NULL
                    }
                  )
                }

                if (is.null(alias_df) || nrow(alias_df) == 0) {
                  debug_log("Neighbor alias lookup returned no rows", 1)
                } else if (!all(c("string_id", "source", "alias") %in% names(alias_df))) {
                  debug_log(paste(
                    "Neighbor alias lookup missing columns:",
                    paste(names(alias_df), collapse = ", ")
                  ), 1)
                } else {
                  debug_log(sprintf(
                    "Neighbor alias lookup: %d STRING IDs, %d alias rows, %d unique sources",
                    dplyr::n_distinct(alias_df$string_id),
                    nrow(alias_df),
                    dplyr::n_distinct(alias_df$source)
                  ), 2)
                  if (isTRUE(getOption("miraprot.string.alias_diagnostics", FALSE))) {
                    alias_overview <- alias_df %>%
                      dplyr::group_by(string_id) %>%
                      dplyr::summarise(
                        sources = paste(sort(unique(source)), collapse = ", "),
                        .groups = "drop"
                      )
                    for (i in seq_len(nrow(alias_overview))) {
                      debug_log(paste(
                        "Neighbor alias sources for", alias_overview$string_id[i], ":",
                        alias_overview$sources[i]
                      ), 2)
                    }
                  }
                  alias_df$source_lower <- tolower(alias_df$source)
                  preferred_sources <- character(0)
                  if (!is.null(selected_identifier_local) && nzchar(selected_identifier_local)) {
                    preferred_sources <- tolower(selected_identifier_local)
                    if (grepl("gene symbol|symbol", preferred_sources)) {
                      preferred_sources <- c("Ensembl_HGNC", "BioMart_HUGO", "Ensembl_HGNC_symbol", "Ensembl_UniProt", "UniProt_GN_Name")
                    }
                  }
                  debug_log(paste(
                    "Neighbor alias preferred sources (ordered):",
                    paste(c(preferred_sources, "uniprot_gn", "fallback"), collapse = " -> ")
                  ), 2)

                  alias_by_id <- split(alias_df, alias_df$string_id)
                  neighbor_labels <- vapply(extra_neighbors, function(id) {
                    df_id <- alias_by_id[[id]]
                    if (is.null(df_id) || nrow(df_id) == 0) {
                      return(id)
                    }
                    if (length(preferred_sources) > 0) {
                      pref_hits <- df_id[Reduce(
                        `|`,
                        lapply(preferred_sources, function(ps) grepl(ps, df_id$source_lower, fixed = TRUE))
                      ), , drop = FALSE]
                      if (nrow(pref_hits) > 0) {
                        return(pref_hits$alias[1])
                      }
                    }
                    uniprot_hits <- df_id[grepl("uniprot_gn", df_id$source_lower, fixed = TRUE), , drop = FALSE]
                    if (nrow(uniprot_hits) > 0) {
                      return(uniprot_hits$alias[1])
                    }
                    fallback_alias <- df_id$alias[1]
                    debug_log(paste("Neighbor alias fallback for", id, "->", fallback_alias), 2)
                    fallback_alias
                  }, character(1))
                }
              }
            }

            debug_log(paste("Neighbor request:", neighbor_count,
                            "| obtained:", length(extra_neighbors)), 1)
          }

          # Lookup: STRING IDs + Anzeige-Label (Seeds = Symbol, Nachbarn = STRING-ID Fallback)
          go_path <- build_go_path(
            genes_mapped = genes_mapped,
            label_column = label_column,
            extra_neighbors = extra_neighbors,
            neighbor_labels = neighbor_labels
          )

          expanded_ids <- unique(go_path$string_id)
          seed_string_ids <- unique(genes_mapped$STRING_id)
          debug_log(paste("Expanded nodes (STRING IDs):", length(expanded_ids),
                          "| seeds:", length(seed_string_ids),
                          "| neighbors:", length(extra_neighbors)), 1)

          # Step 3: Retrieving interactions and preparing graph
          incProgress(0.2, detail = "Retrieving interactions and preparing graph...")

          all_edges <- string_db$get_interactions(expanded_ids)
          edges_df <- unique(data.frame(from = all_edges$from, to = all_edges$to, stringsAsFactors = FALSE))

          vertices_df <- data.frame(
            name  = expanded_ids,
            label = go_path$label[match(expanded_ids, go_path$string_id)],
            stringsAsFactors = FALSE
          )

          g <- igraph::graph_from_data_frame(edges_df, directed = FALSE, vertices = vertices_df)

          # Erzwinge alle Expanded-IDs (auch isolierte Nachbarn)
          missing_ids <- setdiff(expanded_ids, igraph::V(g)$name)
          if (length(missing_ids) > 0) {
            missing_labels <- go_path$label[match(missing_ids, go_path$string_id)]
            missing_labels[is.na(missing_labels)] <- missing_ids
            g <- igraph::add_vertices(
              g,
              nv    = length(missing_ids),
              name  = missing_ids,
              label = missing_labels
            )
          }

          # Display-Label sichern (Seeds behalten Symbol, Nachbarn ID)
          igraph::V(g)$display_label <- igraph::V(g)$label

          debug_log(paste("Post-add_vertices graph:", igraph::vcount(g), "nodes,", igraph::ecount(g), "edges"), 1)
          debug_log(paste("Neighbors (unique, not in seeds):",
                          if (length(extra_neighbors)==0) "none" else paste(extra_neighbors, collapse=", ")), 1)

          if (igraph::vcount(g) < 2 || igraph::ecount(g) == 0) {
            showNotification(
              "STRING network could not be created: not enough interactions. Please select more proteins or relax the score threshold.",
              type = "warning", duration = 5
            )
            debug_log("Aborting STRING network creation: too few nodes/edges for visualization", 1)
            return(NULL)
          }

          # Ab hier: g NICHT mehr überschreiben
          test_g_temp <- g
          test_g(test_g_temp)

          # Step 4: Clustering and filtering graph
          incProgress(0.2, detail = "Clustering and filtering graph...")

          ceb <- igraph::cluster_edge_betweenness(test_g_temp)

          STRING_res_list <- list(
            Mapped       = genes_mapped,
            DB           = string_db,
            STRING_Graph = test_g_temp,
            cluster      = ceb
          )
          res_STRING(STRING_res_list)

          # Step 5: Prepare visualization data
          incProgress(0.2, detail = "Preparing visualization data...")

          membership_vector <- igraph::membership(ceb)
          node_names <- igraph::V(test_g_temp)$name

          num_clusters <- length(unique(membership_vector))
          desaturate_colors <- function(colors, saturation_factor = 0.45, brighten = 0.15) {
            hsv_matrix <- grDevices::rgb2hsv(grDevices::col2rgb(colors))
            hsv_matrix["s", ] <- pmin(1, hsv_matrix["s", ] * saturation_factor)
            hsv_matrix["v", ] <- pmin(1, hsv_matrix["v", ] + brighten)
            grDevices::hsv(h = hsv_matrix["h", ], s = hsv_matrix["s", ], v = hsv_matrix["v", ])
          }
          if (num_clusters > 8) {
            base_border_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(num_clusters)
            ceb_border <- base_border_colors
            ceb_color  <- desaturate_colors(base_border_colors)
          } else {
            base_border_colors <- RColorBrewer::brewer.pal(num_clusters, "Set1")
            ceb_border <- base_border_colors
            ceb_color  <- rep(desaturate_colors(base_border_colors), length.out = num_clusters)
          }

          if (is.null(node_names)) {
            node_names <- paste("Node", 1:igraph::vcount(test_g_temp))
            igraph::V(test_g_temp)$name <- node_names
          }

          vis_data_temp <- prepare_vis_data(test_g_temp, membership_vector,
                                            ceb_color, ceb_border, node_names, debug_log)

          # Override Labels mit display_label (Seeds = Symbol, Nachbarn = STRING-ID)
          if (!is.null(igraph::V(test_g_temp)$display_label)) {
            lbls <- igraph::V(test_g_temp)$display_label
            vis_data_temp$nodes$label <- lbls
            vis_data_temp$nodes$original_label <- lbls
          }

          # Nur Nachbarn grau färben (STRING-ID Vergleich)
          neighbor_ids <- setdiff(vis_data_temp$nodes$id, seed_string_ids)
          if (length(neighbor_ids) > 0) {
            idx <- vis_data_temp$nodes$id %in% neighbor_ids
            vis_data_temp$nodes$color.background[idx] <- "#d9d9d9"
            vis_data_temp$nodes$color.border[idx]     <- "#b3b3b3"
            vis_data_temp$nodes$size[idx]              <- 40
            vis_data_temp$nodes$widthConstraint[idx]   <- 40
            vis_data_temp$nodes$heightConstraint[idx]  <- 40
          }

          original_nodes(vis_data_temp$nodes)
          original_edges(vis_data_temp$edges)
          vis_data(vis_data_temp)
          nodes(vis_data_temp$nodes)
          edges(vis_data_temp$edges)

          debug_log(paste("Visualization data prepared:", nrow(vis_data_temp$nodes),
                          "nodes,", nrow(vis_data_temp$edges), "edges"), 2)

          label_initial_labels(setNames(vis_data_temp$nodes$original_label, vis_data_temp$nodes$id))

          alias_info <- list(available_sources = character(0), alias_map = list())
          # alias_df is NULL when no neighbors were requested; load it now.
          # When neighbors were present it was already loaded above (for all
          # expanded IDs), so the alias file is read at most once per network build.
          if (is.null(alias_df)) {
            alias_df <- load_string_aliases(string_db, expanded_ids, string_db_version_used, debug_log)
          }
          if (!is.null(alias_df) && is.data.frame(alias_df) && nrow(alias_df) > 0) {
            alias_df$source <- as.character(alias_df$source)
            alias_df$alias <- as.character(alias_df$alias)
            sources_by_id <- split(alias_df$source, alias_df$string_id)
            if (all(expanded_ids %in% names(sources_by_id))) {
              alias_info$available_sources <- Reduce(intersect, lapply(sources_by_id, unique))
            }
            alias_map <- lapply(split(alias_df, alias_df$source), function(df_source) {
              df_source <- df_source[!duplicated(df_source$string_id), , drop = FALSE]
              setNames(df_source$alias, df_source$string_id)
            })
            alias_info$alias_map <- alias_map
          }
          label_alias_data(alias_info)

          label_choices <- c("Current label" = "current", "STRING ID" = "string_id")
          if (length(alias_info$available_sources) > 0) {
            label_choices <- c(label_choices,
                               stats::setNames(alias_info$available_sources, alias_info$available_sources))
          }
          preferred_sources <- c("Ensembl_HGNC", "Ensembl_HGNC_symbol", "BioMart_HUGO", "UniProt_GN_Name")
          preferred_available <- intersect(preferred_sources, alias_info$available_sources)
          selected_label_variant <- if (length(preferred_available) > 0) {
            preferred_available[1]
          } else {
            "current"
          }

          updateSelectInput(session, "label_variant_STRING",
                            choices = label_choices,
                            selected = selected_label_variant)
          apply_label_variant(selected_label_variant)

          set_cached_network_build(cache_key, list(
            mapped_proteins = genes_mapped,
            graph = test_g_temp,
            nodes = vis_data_temp$nodes,
            edges = vis_data_temp$edges,
            alias_info = alias_info,
            label_choices = label_choices,
            selected_label_variant = selected_label_variant,
            initial_labels = setNames(vis_data_temp$nodes$original_label, vis_data_temp$nodes$id),
            res_STRING = STRING_res_list,
            alias_df = alias_df
          ))
          debug_log(paste("Cached STRING network build for key", cache_key), 2)

          # Debug level 0: UI settings required to recreate the plot
          debug_log(
            sprintf(
              paste0(
                "STRING network settings",
                " | STRING version: %s",
                " | Score threshold: %s",
                " | Displayed interactions: %s",
                " | Species ID: %s",
                " | First-order neighbors: %d",
                "%s"
              ),
              input_version_STRING_selected,
              input_score_STRING_selected,
              input_edgetype_STRING_selected,
              string_species_id_used,
              neighbor_count,
              if (neighbor_count > 0) sprintf(" | Neighbor selection: %s", neighbor_strategy) else ""
            ),
            level = 0
          )
          # Debug level 0: Summary of network outcome
          debug_log(
            sprintf(
              paste0(
                "STRING network outcome",
                " | Interactions: %d",
                " | Clusters: %d",
                " | Proteins in network: %d",
                " | Node label source: %s"
              ),
              nrow(vis_data_temp$edges),
              num_clusters,
              nrow(vis_data_temp$nodes),
              selected_label_variant
            ),
            level = 0
          )

          # Step 6: Render initial network
          incProgress(0.1, detail = "Rendering network...")
          debug_log("About to render visNetwork", 1)

          output$String_plot <- renderVisNetwork({
            tryCatch({
              req(nodes(), edges())
              current_nodes <- nodes()
              current_edges <- edges()

              debug_log(paste("Rendering visNetwork with", nrow(current_nodes), "nodes and", nrow(current_edges), "edges"), 1)

              if (!"x" %in% colnames(current_nodes) || !"y" %in% colnames(current_nodes)) {
                debug_log("No position data found, using auto-layout", 2)
              } else {
                debug_log("Using stored node positions", 2)
              }

              vis_result <- create_vis_network(current_nodes, current_edges, ns = ns)
              debug_log("VisNetwork created successfully", 1)
              return(vis_result)

            }, error = function(e) {
              debug_log(paste("CRITICAL ERROR creating visNetwork:", e$message), 1)
              return(NULL)
            })
          })

          # Step 7: Prepare clusters for user selection
          # Single-member clusters are excluded; the cluster section UI is driven
          # by renderUI in STRING_server_interactions.R reacting to res_STRING().
          incProgress(0.1, detail = "Preparing clusters for selection...")
          cluster_list <- split(node_names, membership_vector)
          cluster_list <- cluster_list[lengths(cluster_list) >= 2]

          debug_log("Network creation completed successfully", 1)

        }, error = function(e) {
          debug_log(paste("Error in STRING mapping:", e$message), 1)
          showNotification(paste("STRING mapping error:", e$message), type = "error")
          return(NULL)
        })
      })
    })
  }

  list(
    normalize_network_build_inputs = normalize_network_build_inputs,
    register_network_build_observer = register_network_build_observer
  )
}
