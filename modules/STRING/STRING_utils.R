# ==============================================================================
# STRING Module - Utility Functions
# ==============================================================================
#
# Purpose:
#   Pure utility and conversion functions shared across the STRING module.
#   Contains no Shiny reactive logic, no observers, and no server state.
#   All functions are stateless and receive any required dependencies
#   (including debug_log) as explicit parameters.
#
# Architecture role:
#   Sourced at the module level (outside the server function) by STRING_module.R.
#   Functions are therefore available to all factory-initialized sub-components
#   via the module environment.
#
# File structure:
#   1. Alias and label utilities (load_string_aliases, format_node_label)
#   2. Network conversion functions (convert_vis_to_igraph, convert_string_to_ggplot)
#   3. Layout utilities (apply_layout_transformation)
#   4. Visualization preparation (prepare_vis_data, create_vis_network)
#   5. Identifier/filtering utilities (get_filter_string_STRING)
#   6. Cluster naming (ceb3_combined)
#   7. Node/edge UI control updaters (update_node_ui_controls, update_edge_ui_controls)
#   8. Shape mapping (shape_mapping, reverse_shape_mapping, shape_mapping_igraph)
#   9. Degree filtering (filter_network_by_degree)
#
# Future developers:
#   - Do not add Shiny reactive logic or observers to this file.
#   - All logging must use debug_log passed as a parameter.
#   - Functions added here must be pure or accept all dependencies explicitly.
# ==============================================================================

# ==============================================================================
# Alias and Label Utilities
# ==============================================================================

#' Load STRING protein aliases from a gzipped alias file.
#'
#' Reads the species-specific alias file bundled with STRINGdb, normalises
#' column names (handles both headered and headerless variants), and returns
#' a tidy data frame filtered to \code{ids}.
#'
#' @param string_db  A STRINGdb instance.  Must expose \code{input_directory}
#'   and \code{species} fields.
#' @param ids        Character vector of STRING protein IDs to retain.
#' @param version    STRING database version string, e.g. \code{"11.5"}.
#' @param debug_log  Logging function with signature \code{(message, level)}.
#'
#' @return A \code{data.frame} with columns \code{string_id}, \code{alias},
#'   and \code{source}, or \code{NULL} if the file cannot be read or no rows
#'   match \code{ids}.
load_string_aliases <- function(string_db, ids, version, debug_log) {
  alias_file <- file.path(
    string_db$input_directory,
    paste0(string_db$species, ".protein.aliases.v", version, ".txt.gz")
  )
  if (!file.exists(alias_file)) {
    debug_log(paste("Alias file not found:", alias_file), 1)
    return(NULL)
  }
  header_line <- tryCatch(
    readLines(gzfile(alias_file), n = 1, warn = FALSE),
    error = function(e) {
      debug_log(paste("Alias file header read failed:", e$message), 1)
      NULL
    }
  )
  has_header <- !is.null(header_line) &&
    grepl("protein_id", header_line, fixed = TRUE) &&
    grepl("alias", header_line, fixed = TRUE) &&
    grepl("source", header_line, fixed = TRUE)
  alias_tbl <- tryCatch(
    utils::read.delim(
      alias_file,
      header = has_header,
      stringsAsFactors = FALSE,
      sep = "\t",
      comment.char = "",
      quote = ""
    ),
    error = function(e) {
      debug_log(paste("Alias file read failed:", e$message), 1)
      NULL
    }
  )
  if (is.null(alias_tbl)) {
    return(NULL)
  }
  if ("X.string_protein_id" %in% names(alias_tbl)) {
    names(alias_tbl)[names(alias_tbl) == "X.string_protein_id"] <- "string_protein_id"
  }
  if (has_header) {
    header_names <- names(alias_tbl)
    header_names[1] <- gsub("^#", "", header_names[1])
    names(alias_tbl) <- header_names
  } else if (ncol(alias_tbl) >= 3) {
    names(alias_tbl)[1:3] <- c("string_protein_id", "alias", "source")
  } else {
    debug_log("Alias file missing required columns", 1)
    return(NULL)
  }
  if (!"string_protein_id" %in% names(alias_tbl)) {
    debug_log("Alias file missing string_protein_id column", 1)
    return(NULL)
  }
  alias_tbl$protein_id <- alias_tbl$string_protein_id
  if (!all(c("protein_id", "alias", "source") %in% names(alias_tbl))) {
    debug_log("Alias file missing required columns", 1)
    return(NULL)
  }
  alias_tbl <- alias_tbl[alias_tbl$string_protein_id %in% ids, , drop = FALSE]
  if (nrow(alias_tbl) == 0) {
    return(NULL)
  }
  data.frame(
    string_id = alias_tbl$protein_id,
    alias = alias_tbl$alias,
    source = alias_tbl$source,
    stringsAsFactors = FALSE
  )
}

#' Apply HTML font styling to a node label.
#'
#' @param label      Character string — the raw node label.
#' @param font_style One of \code{"Plain text"}, \code{"Bold"},
#'   \code{"Italic"}, or \code{"Bold and italic"}.
#'
#' @return The label wrapped in the appropriate HTML tags, or the original
#'   label unchanged if \code{font_style} is unrecognised.
format_node_label <- function(label, font_style) {
  switch(font_style,
         "Plain text" = label,
         "Bold" = paste0("<b>", label, "</b>"),
         "Italic" = paste0("<i>", label, "</i>"),
         "Bold and italic" = paste0("<b><i>", label, "</i></b>"),
         label)
}

# ========================================
# Network Conversion Functions
# ========================================

#' #' Get Gene Identifier Choices
#' #'
#' #' Get available gene identifier column choices from data definition
#' #' @param data_def data definition dataframe
#' #' @param debug_level debug level for logging
#' #' @return named vector of gene identifier choices
#' get_gene_identifier_choices <- function(data_def, debug_level = 0) {
#'   if (debug_level >= 2) cat("[ STRING DB ] Searching for gene identifier columns\n")
#'
#'   # Validate input
#'   if (is.null(data_def) || nrow(data_def) == 0) {
#'     if (debug_level >= 1) cat("[ STRING DB ] Empty or NULL data_def provided\n")
#'     return(character(0))
#'   }
#'
#'   # Filter for rows where Content contains "Identifier"
#'   identifier_rows <- data_def[grepl("Identifier", data_def$Content, ignore.case = TRUE), ]
#'
#'   if (nrow(identifier_rows) == 0) {
#'     if (debug_level >= 1) cat("[ STRING DB ] No identifier rows found\n")
#'     return(character(0))
#'   }
#'
#'   # Get unique Options from identifier rows
#'   valid_options <- identifier_rows$Options[!is.na(identifier_rows$Options) &
#'                                              identifier_rows$Options != ""]
#'   choices <- unique(valid_options)
#'
#'   if (length(choices) == 0) {
#'     if (debug_level >= 1) cat("[ STRING DB ] No valid options found in identifier rows\n")
#'     return(character(0))
#'   }
#'
#'   # Create named vector KORREKT
#'   named_choices <- sapply(choices, function(opt) {
#'     matching_row <- identifier_rows[identifier_rows$Options == opt &
#'                                       !is.na(identifier_rows$Options), ][1, ]
#'     return(matching_row$Column)
#'   })
#'
#'   names(named_choices) <- choices
#'
#'   if (debug_level >= 1) {
#'     cat("[ STRING DB ] Found", length(named_choices), "gene identifier options\n")
#'   }
#'
#'   return(named_choices)
#' }

#' Get the correct visNetwork output ID for the current module
#' @return Character string of the output ID
get_vis_output_id <- function() {
  return("String_plot")  # This must match your renderVisNetwork output name
}

#' Convert VisNetwork to igraph for export
#' @param nodes Data frame of nodes from VisNetwork
#' @param edges Data frame of edges from VisNetwork
#' @param size_factor Scaling factor for node sizes (default 0.35)
#' @param debug_log Debug logging function
convert_vis_to_igraph <- function(nodes, edges, size_factor = 0.35, debug_log) {

  debug_log("Starting VisNetwork → igraph conversion", 1)

  # Windows font setup for compatibility
  if (.Platform$OS.type == "windows") {
    windowsFonts(
      "Arial" = windowsFont("Arial"),
      "Times New Roman" = windowsFont("Times New Roman"),
      "Courier New" = windowsFont("Courier New"),
      "Georgia" = windowsFont("Georgia")
    )
  }

  # Update node size constraints
  current_nodes <- update_constraints(nodes, size_factor, debug_log)

  debug_log("Processing node size constraints", 2)

  # Calculate font styles for all nodes
  font_styles <- mapply(get_font_style,
                        current_nodes$font.bold,
                        current_nodes$font.italic)

  debug_log("Converting shape mappings", 2)

  # Create empty igraph object
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)

  # Add vertices with all properties
  g <- igraph::add_vertices(g, nrow(current_nodes),
                            name = current_nodes$id,
                            color = current_nodes$color.background,
                            frame.color = current_nodes$color.border,
                            label = current_nodes$original_label,
                            size = current_nodes$widthConstraint,
                            size2 = current_nodes$widthConstraint,
                            shape = shape_mapping_igraph(current_nodes$shape),
                            label.cex = current_nodes$font.size/12,
                            frame.width = current_nodes$borderWidth,
                            label.font = font_styles,
                            label.color = "black",
                            label.family = current_nodes$font.face,
                            x = current_nodes$x,
                            y = current_nodes$y
  )

  debug_log("Calculating font styles", 2)
  debug_log("Transforming edge line types", 2)

  # Calculate edge line types
  edge_line_types <- sapply(edges$dashes, get_edge_line_type)

  # Add edges with properties
  g <- igraph::add_edges(g, c(rbind(edges$from, edges$to)),
                         color = edges$color,
                         width = edges$width,
                         lty = edge_line_types
  )

  debug_log("Creating igraph object", 1)

  # Create layout matrix from stored coordinates
  layout <- as.matrix(current_nodes[, c("x", "y")])

  # CRITICAL: Invert Y-axis for correct orientation
  layout[, 2] <- -layout[, 2]

  debug_log("Processing layout coordinates", 2)

  # Validate layout matrix
  if (any(is.na(layout)) || any(!is.finite(layout))) {
    stop("Layout contains invalid values (NA or Inf)")
  }

  debug_log(paste("Conversion complete:", vcount(g), "nodes,", ecount(g), "edges"), 1)

  return(list(graph = g, layout = layout))
}

#' Convert VisNetwork to igraph for grid export
#' @param nodes Data frame of nodes from VisNetwork
#' @param edges Data frame of edges from VisNetwork
#' @param debug_log Debug logging function
convert_vis_to_igraph_for_grid <- function(nodes, edges, debug_log) {
  result_igraph <- convert_vis_to_igraph(nodes, edges, size_factor = 0.55, debug_log)
  result_igraph$layout <- igraph::norm_coords(
    result_igraph$layout,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1
  )
  result_igraph
}

#' Update node size constraints for export
#' @param current_nodes Node data frame
#' @param size_factor Scaling factor
#' @param debug_log Debug logging function
update_constraints <- function(current_nodes, size_factor, debug_log) {

  # Check if columns exist, if not create them
  if (!"widthConstraint" %in% colnames(current_nodes)) {
    current_nodes$widthConstraint <- NA_real_
  }
  if (!"heightConstraint" %in% colnames(current_nodes)) {
    current_nodes$heightConstraint <- NA_real_
  }

  # Update constraints row-wise
  for (i in 1:nrow(current_nodes)) {
    # Update widthConstraint
    if (is.na(current_nodes$widthConstraint[i]) || is.null(current_nodes$widthConstraint[i])) {
      current_nodes$widthConstraint[i] <- current_nodes$size[i] * size_factor
    } else {
      current_nodes$widthConstraint[i] <- current_nodes$widthConstraint[i] * size_factor
    }

    # Update heightConstraint
    if (is.na(current_nodes$heightConstraint[i]) || is.null(current_nodes$heightConstraint[i])) {
      current_nodes$heightConstraint[i] <- current_nodes$size[i] * size_factor
    } else {
      current_nodes$heightConstraint[i] <- current_nodes$heightConstraint[i] * size_factor
    }
  }

  return(current_nodes)
}

# ========================================
# Shape and Style Mapping Functions
# ========================================

#' Map shape names from UI to VisNetwork format
#' @param shape Shape name from UI
shape_mapping <- function(shape) {
  switch(shape,
         "Circle" = "circle",
         "Box" = "box",
         "circle"  # Default
  )
}

#' Reverse map shape names from VisNetwork to UI format
#' @param shape Shape name from VisNetwork
reverse_shape_mapping <- function(shape) {
  switch(shape,
         "circle" = "Circle",
         "box" = "Box",
         "Circle"  # Default
  )
}

#' Map VisNetwork shapes to igraph shapes
#' @param shape_vector Vector of shape names
shape_mapping_igraph <- function(shape_vector) {
  sapply(shape_vector, function(shape) {
    switch(shape,
           "circle" = "circle",
           "box" = "rectangle",
           "circle"  # Default
    )
  })
}

#' Convert font style booleans to igraph numeric codes
#' @param is_bold Boolean for bold
#' @param is_italic Boolean for italic
get_font_style <- function(is_bold, is_italic) {
  if (is_bold && is_italic) {
    return(4)  # Bold and italic
  } else if (is_bold) {
    return(2)  # Bold
  } else if (is_italic) {
    return(3)  # Italic
  } else {
    return(1)  # Normal
  }
}

#' Convert edge dash patterns to igraph line types
#' @param dashes VisNetwork dash pattern
get_edge_line_type <- function(dashes) {
  if (is.logical(dashes) && dashes) {
    return(2)  # Dashed
  } else if (is.numeric(dashes) && identical(dashes, c(2, 10))) {
    return(3)  # Dotted
  } else if (is.numeric(dashes) && identical(dashes, c(5, 5))) {
    return(4)  # Double/dash-dot
  } else {
    return(1)  # Solid (default)
  }
}

# ========================================
# Data Processing Functions
# ========================================

#' Parse filter string for protein search
#' @param input_text Text input from user
#' @param identifier_type Selected identifier type
#' @param debug_log Debug logging function
get_filter_string_STRING <- function(input_text, identifier_type, debug_log) {

  lines <- unlist(strsplit(input_text, "\n"))
  lines <- trimws(lines[lines != ""])
  num_lines <- length(lines)

  if (num_lines == 0) {
    return(data.frame())
  }

  df <- data.frame(matrix(nrow = num_lines, ncol = 1))
  colnames(df) <- c(identifier_type)

  for (i in 1:num_lines) {
    line <- unlist(strsplit(lines[i], "[,\\s]+"))
    df[i, identifier_type] <- line[1]
  }

  debug_log(paste("Parsed", num_lines, "protein identifiers"), 2)

  return(df)
}

#' Combine cluster names for display
#' @param list_reduction List of clusters
#' @param debug_log Debug logging function
ceb3_combined <- function(list_reduction, debug_log) {
  sapply(seq_along(list_reduction), function(i) {
    subvector <- list_reduction[[i]]
    proteins <- if (length(subvector) > 3) {
      paste0(paste(subvector[1:3], collapse = ", "), ", ...")
    } else {
      paste(subvector, collapse = ", ")
    }
    paste0("Cluster ", i, ": ", proteins)
  })
}

# ========================================
# Layout Functions
# ========================================

#' Apply layout transformation to graph
#' @param graph igraph object
#' @param layout_type Type of layout to apply
#' @param debug_log Debug logging function
apply_layout_transformation <- function(graph, layout_type, debug_log) {

  set.seed(1234)

  layout_matrix <- switch(layout_type,
                          "fr" = igraph::layout_with_fr(graph),
                          "kk" = igraph::layout_with_kk(graph),
                          "random" = igraph::layout_randomly(graph),
                          "circle" = igraph::layout_in_circle(graph),
                          "star" = igraph::layout_as_star(graph),
                          igraph::layout_with_fr(graph)  # Default
  )

  debug_log(paste("Applied", layout_type, "layout"), 2)

  return(layout_matrix)
}

#' Filter nodes by minimum degree
#' @param graph Original igraph object
#' @param nodes_data Current nodes data
#' @param edges_data Current edges data
#' @param min_degree Minimum degree threshold
#' @param debug_log Debug logging function
filter_nodes_by_degree <- function(graph, nodes_data, edges_data, min_degree, debug_log) {

  # Find nodes below and above threshold
  nodes_below_threshold <- which(igraph::degree(graph) < min_degree)
  nodes_above_threshold <- which(igraph::degree(graph) >= min_degree)

  # Get node names
  nodes_to_keep <- V(graph)[nodes_above_threshold]$name
  nodes_to_remove <- V(graph)[nodes_below_threshold]$name

  debug_log(paste("Filtering:", length(nodes_to_remove), "nodes below degree", min_degree), 2)

  # Prepare removed data
  removed_nodes_data <- nodes_data[nodes_data$id %in% nodes_to_remove, ]
  removed_edges_data <- edges_data[
    edges_data$from %in% nodes_to_remove | edges_data$to %in% nodes_to_remove,
  ]

  # Filter to keep only valid nodes and edges
  filtered_nodes <- nodes_data[nodes_data$id %in% nodes_to_keep, ]
  filtered_edges <- edges_data[
    edges_data$from %in% nodes_to_keep & edges_data$to %in% nodes_to_keep,
  ]

  return(list(
    nodes = filtered_nodes,
    edges = filtered_edges,
    removed_nodes = removed_nodes_data,
    removed_edges = removed_edges_data
  ))
}

# ========================================
# Visualization Helper Functions
# ========================================

#' Prepare data for VisNetwork visualization
#' @param graph igraph object
#' @param membership_vector Cluster membership vector
#' @param ceb_color Node background colors
#' @param ceb_border Node border colors
#' @param node_names Node names
#' @param debug_log Debug logging function
prepare_vis_data <- function(graph, membership_vector, ceb_color, ceb_border, node_names, debug_log) {

  set.seed(1234)

  # Generate layout
  layout_matrix <- igraph::layout_with_fr(graph)

  debug_log(paste("Creating nodes for", length(node_names), "nodes"), 2)

  # Create nodes data frame
  nodes <- data.frame(
    id = node_names,
    label = node_names,
    original_label = node_names,
    group = as.character(membership_vector),
    color.background = ceb_color[membership_vector],
    color.border = ceb_border[membership_vector],
    size = 50,
    widthConstraint = 50,
    heightConstraint = 50,
    shape = "circle",
    borderWidth = 2,
    font.size = 12,
    font.face = "Arial",
    font.bold = FALSE,
    font.italic = FALSE,
    x = layout_matrix[, 1] * 130,
    y = layout_matrix[, 2] * 130,
    fixed = FALSE,
    stringsAsFactors = FALSE
  )

  debug_log(paste("Nodes data frame created with columns:", paste(colnames(nodes), collapse = ", ")), 2)

  # Create edges data frame
  edges <- as.data.frame(igraph::as_edgelist(graph), stringsAsFactors = FALSE)
  colnames(edges) <- c("from", "to")
  edges$id <- paste(edges$from, edges$to, sep = "_")

  # Add cluster information
  edges$from_cluster <- membership_vector[match(edges$from, node_names)]
  edges$to_cluster <- membership_vector[match(edges$to, node_names)]

  # Color edges based on cluster connection
  edges$color <- "#BEBEBE"  # Default color
  edges$color[edges$from_cluster != edges$to_cluster] <- "#FF0000"  # Red for inter-cluster

  edges$width <- 2
  edges$dashes <- FALSE

  debug_log(paste("Edges data frame created with columns:", paste(colnames(edges), collapse = ", ")), 2)
  debug_log("Prepared visualization data successfully", 1)

  return(list(nodes = nodes, edges = edges))
}

#' Create VisNetwork visualization
#' @param nodes Node data
#' @param edges Edge data
#' @param ns Namespace function from the module
create_vis_network <- function(nodes, edges, ns = NULL) {

  # Determine the correct input names based on whether we have a namespace
  if (!is.null(ns)) {
    node_input_name <- ns("selectedNode_STRING")
    edge_input_name <- ns("selectedEdges_STRING")
    position_input_name <- ns("nodePositions_STRING")  # ADD THIS LINE
  } else {
    node_input_name <- "selectedNode_STRING"
    edge_input_name <- "selectedEdges_STRING"
    position_input_name <- "nodePositions_STRING"      # ADD THIS LINE
  }

  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visNodes(
      borderWidth = 2,
      font = list(size = 12, face = "Arial", multi = TRUE),
      scaling = list(enabled = FALSE),
      labelHighlightBold = FALSE,
      borderWidthSelected = FALSE,
      shapeProperties = list(borderRadius = 0)
    ) %>%
    visNetwork::visEdges(
      smooth = list(enabled = FALSE)
    ) %>%
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      dragNodes = TRUE,
      multiselect = TRUE,
      hover = FALSE
    ) %>%
    visNetwork::visPhysics(
      enabled = FALSE,  # Keep physics disabled
      stabilization = list(
        enabled = TRUE,
        iterations = 1000,
        updateInterval = 50,
        fit = TRUE
      )
    ) %>%
    visNetwork::visEvents(
      selectNode = paste0(
        "function(properties) {",
        "  console.log('Node selected, setting input: ", node_input_name, "');",
        "  Shiny.setInputValue('", node_input_name, "', properties.nodes, {priority: 'event'});",
        "}"
      ),
      selectEdge = paste0(
        "function(properties) {",
        "  console.log('Edge selected, setting input: ", edge_input_name, "');",
        "  Shiny.setInputValue('", edge_input_name, "', properties.edges, {priority: 'event'});",
        "}"
      ),
      deselectNode = paste0(
        "function(properties) {",
        "  if(properties.nodes.length === 0) {",
        "    console.log('Node deselected, clearing input: ", node_input_name, "');",
        "    Shiny.setInputValue('", node_input_name, "', null, {priority: 'event'});",
        "  }",
        "}"
      ),
      deselectEdge = paste0(
        "function(properties) {",
        "  if(properties.edges.length === 0) {",
        "    console.log('Edge deselected, clearing input: ", edge_input_name, "');",
        "    Shiny.setInputValue('", edge_input_name, "', null, {priority: 'event'});",
        "  }",
        "}"
      ),
      dragEnd = paste0(
        "function(properties) {",
        "  console.log('Drag ended for nodes:', properties.nodes);",
        "  if(properties.nodes && properties.nodes.length > 0) {",
        "    try {",
        "      var positions = this.getPositions(properties.nodes);",
        "      console.log('Positions captured:', positions);",
        "      Shiny.setInputValue('", position_input_name, "', positions, {priority: 'event'});",  # FIXED: use variable
        "      Shiny.setInputValue('", node_input_name, "', properties.nodes, {priority: 'event'});",
        "    } catch(e) {",
        "      console.error('Error in dragEnd:', e);",
        "    }",
        "  }",
        "}"
      )
    )
}

# ========================================
# UI SYNCHRONIZATION FUNCTIONS
# ========================================


#' Run UI style-control synchronization with styling observers suppressed
#' @param session Shiny session
#' @param styling_ui_sync_state ReactiveValues guard for active/pending style UI syncs
#' @param debug_log Debug logging function
#' @param expr UI update expression to evaluate while the guard is active
with_style_ui_sync <- function(session, styling_ui_sync_state, debug_log, expr) {
  if (is.null(styling_ui_sync_state)) {
    force(expr)
    return(invisible(NULL))
  }

  current_depth <- isolate(styling_ui_sync_state$depth)
  if (is.null(current_depth) || is.na(current_depth)) {
    current_depth <- 0L
  }

  styling_ui_sync_state$depth <- current_depth + 1L

  on.exit({
    session$onFlushed(function() {
      flushed_depth <- isolate(styling_ui_sync_state$depth)
      if (is.null(flushed_depth) || is.na(flushed_depth) || flushed_depth <= 1L) {
        styling_ui_sync_state$depth <- 0L
      } else {
        styling_ui_sync_state$depth <- flushed_depth - 1L
      }
    }, once = TRUE)
  }, add = TRUE)

  force(expr)
  invisible(NULL)
}

#' Mark a style input value as programmatically synchronized
#' @param styling_ui_sync_state ReactiveValues guard for active/pending style UI syncs
#' @param input_id Input identifier that will be updated programmatically
#' @param value Value expected to echo back from the browser
mark_style_ui_sync_input <- function(styling_ui_sync_state, input_id, value) {
  if (is.null(styling_ui_sync_state) || is.null(input_id) || !nzchar(input_id)) {
    return(invisible(NULL))
  }

  pending <- isolate(styling_ui_sync_state$pending)
  if (is.null(pending) || !is.list(pending)) {
    pending <- list()
  }

  pending[[input_id]] <- value
  styling_ui_sync_state$pending <- pending
  invisible(NULL)
}

update_synced_colour_input <- function(session, styling_ui_sync_state, input_id, value, ...) {
  mark_style_ui_sync_input(styling_ui_sync_state, input_id, value)
  colourpicker::updateColourInput(session, input_id, value = value, ...)
}

update_synced_select_input <- function(session, styling_ui_sync_state, input_id, selected, ...) {
  mark_style_ui_sync_input(styling_ui_sync_state, input_id, selected)
  updateSelectInput(session, input_id, selected = selected, ...)
}

update_synced_numeric_input <- function(session, styling_ui_sync_state, input_id, value, ...) {
  mark_style_ui_sync_input(styling_ui_sync_state, input_id, value)
  updateNumericInput(session, input_id, value = value, ...)
}

#' Update UI controls based on selected node properties
#' @param session Shiny session
#' @param selected_node_data Data for selected node (single row)
#' @param debug_log Debug logging function
update_node_ui_controls <- function(session, selected_node_data, debug_log, style_ui_sync_guard = NULL) { # KEEP
  if (nrow(selected_node_data) == 0) {
    debug_log("No node data provided for UI update", 1)
    return()
  }

  tryCatch({
    with_style_ui_sync(session, style_ui_sync_guard, debug_log, {
      # Update node background color
      if ("color.background" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$color.background[1])) {
        update_synced_colour_input(session, style_ui_sync_guard, "Color_1_STRING",
                                   value = selected_node_data$color.background[1])
        debug_log(paste("Updated background color to:", selected_node_data$color.background[1]), 2)
      }

      # Update node border color
      if ("color.border" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$color.border[1])) {
        update_synced_colour_input(session, style_ui_sync_guard, "Color_2_STRING",
                                   value = selected_node_data$color.border[1])
        debug_log(paste("Updated border color to:", selected_node_data$color.border[1]), 2)
      }

      # Update shape
      if ("shape" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$shape[1])) {
        shape_value <- switch(as.character(selected_node_data$shape[1]),
                              "circle" = "Circle",
                              "box" = "Box",
                              "Circle")
        update_synced_select_input(session, style_ui_sync_guard, "Shape_STRING", selected = shape_value)
        debug_log(paste("Updated shape to:", shape_value), 2)
      }

      # Update size
      if ("widthConstraint" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$widthConstraint[1])) {
        update_synced_numeric_input(session, style_ui_sync_guard, "Size_1_STRING",
                                    value = selected_node_data$widthConstraint[1])
        debug_log(paste("Updated size to:", selected_node_data$widthConstraint[1]), 2)
      }

      # Update font properties
      if ("font.size" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$font.size[1])) {
        update_synced_numeric_input(session, style_ui_sync_guard, "TextSize_STRING",
                                    value = selected_node_data$font.size[1])
        debug_log(paste("Updated font size to:", selected_node_data$font.size[1]), 2)
      }

      if ("font.face" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$font.face[1])) {
        update_synced_select_input(session, style_ui_sync_guard, "FontType_STRING",
                                   selected = selected_node_data$font.face[1])
        debug_log(paste("Updated font face to:", selected_node_data$font.face[1]), 2)
      }

      # Update frame width
      if ("borderWidth" %in% colnames(selected_node_data) &&
          !is.na(selected_node_data$borderWidth[1])) {
        update_synced_numeric_input(session, style_ui_sync_guard, "Frame_STRING",
                                    value = selected_node_data$borderWidth[1])
        debug_log(paste("Updated border width to:", selected_node_data$borderWidth[1]), 2)
      }

      debug_log("Node UI controls updated successfully", 1)
    })
  }, error = function(e) {
    debug_log(paste("Error updating node UI controls:", e$message), 1)
  })
}

#' Update UI controls based on selected edge properties
#' @param session Shiny session
#' @param selected_edge_data Data for selected edge (single row)
#' @param debug_log Debug logging function
update_edge_ui_controls <- function(session, selected_edge_data, debug_log, style_ui_sync_guard = NULL) { # KEEP
  if (nrow(selected_edge_data) == 0) {
    debug_log("No edge data provided for UI update", 1)
    return()
  }

  tryCatch({
    with_style_ui_sync(session, style_ui_sync_guard, debug_log, {
      # Update edge color
      if ("color" %in% colnames(selected_edge_data) &&
          !is.na(selected_edge_data$color[1])) {
        update_synced_colour_input(session, style_ui_sync_guard, "EdgeColor_STRING",
                                   value = selected_edge_data$color[1])
        debug_log(paste("Updated edge color to:", selected_edge_data$color[1]), 2)
      }

      # Update edge width
      if ("width" %in% colnames(selected_edge_data) &&
          !is.na(selected_edge_data$width[1])) {
        update_synced_numeric_input(session, style_ui_sync_guard, "EdgeWidth_STRING",
                                    value = selected_edge_data$width[1])
        debug_log(paste("Updated edge width to:", selected_edge_data$width[1]), 2)
      }

      # Update edge type based on dashes property
      if ("dashes" %in% colnames(selected_edge_data)) {
        edge_type <- switch(as.character(selected_edge_data$dashes[1]),
                            "FALSE" = "Solid",
                            "TRUE" = "Dashed",
                            "c(2, 10)" = "Dotted",
                            "c(5, 5)" = "Double",
                            "Solid")
        update_synced_select_input(session, style_ui_sync_guard, "EdgeType_STRING", selected = edge_type)
        debug_log(paste("Updated edge type to:", edge_type), 2)
      }

      debug_log("Edge UI controls updated successfully", 1)
    })
  }, error = function(e) {
    debug_log(paste("Error updating edge UI controls:", e$message), 1)
  })
}

# ========================================
# DEGREE FILTER FUNCTION (REVERSIBLE)
# ========================================

#' Apply minimum degree filter to network data
#' @param orig_nodes Original node data
#' @param orig_edges Original edge data
#' @param min_degree Minimum number of connections required
apply_degree_filter <- function(orig_nodes, orig_edges, min_degree, debug_log = NULL) {
  if (is.null(orig_nodes) || is.null(orig_edges) || is.null(min_degree)) {
    return(list(nodes = orig_nodes, edges = orig_edges))
  }

  if (is.function(debug_log)) {
    debug_log(paste("Applying degree filter:", min_degree), 2)
  }

  # Calculate degree (number of connections) for each node
  node_degrees <- table(c(orig_edges$from, orig_edges$to))

  # Find nodes that meet minimum degree requirement
  valid_nodes <- names(node_degrees)[node_degrees >= min_degree]

  # Filter nodes
  filtered_nodes <- orig_nodes[orig_nodes$id %in% valid_nodes, ]

  # Filter edges (only keep edges between valid nodes)
  filtered_edges <- orig_edges[
    orig_edges$from %in% valid_nodes & orig_edges$to %in% valid_nodes,
  ]

  if (is.function(debug_log)) {
    debug_log(paste("Filtered:", nrow(orig_nodes), "→", nrow(filtered_nodes), "nodes,",
                    nrow(orig_edges), "→", nrow(filtered_edges), "edges"), 1)
  }

  return(list(
    nodes = filtered_nodes,
    edges = filtered_edges
  ))
}

#' Convert igraph network to ggplot for grid compatibility
#' @param graph igraph object
#' @param nodes Current nodes data
#' @param edges Current edges data
#' @param debug_log Debug logging function
#' @return ggplot object containing the network plot
convert_string_to_ggplot <- function(graph, nodes, edges, debug_log) {

  debug_log("Converting STRING network to ggplot for grid compatibility", 1)

  tryCatch({
    # Create a temporary file for the network plot
    temp_file <- tempfile(fileext = ".png")

    # Convert visNetwork data back to igraph for plotting
    result_igraph <- convert_vis_to_igraph_for_grid(nodes, edges, debug_log)
    g <- result_igraph$graph
    layout <- result_igraph$layout

    # Render igraph to PNG for grid export
    png(temp_file, width = 1600, height = 1600, res = 200, type = "cairo")
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    graphics::par(mar = c(0, 0, 0, 0))
    plot(
      g,
      layout = layout,
      rescale = FALSE,
      xlim = c(-1.15, 1.15),
      ylim = c(-1.15, 1.15),
      margin = 0,
      vertex.size = igraph::V(g)$size * 1.2,
      vertex.label.cex = igraph::V(g)$label.cex * 1.2
    )

    # Close graphics device before reading image to avoid partial/corrupt file reads
    dev.off()

    # Read the image back
    img <- png::readPNG(temp_file)
    unlink(temp_file)  # Clean up temp file

    # Create ggplot with the image
    ggplot_network <- ggplot() +
      annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      theme_void() +
      ggtitle("STRING Network") +
      theme(plot.title = element_text(hjust = 0.5, size = 14))

    debug_log("Successfully converted STRING network to ggplot", 1)
    return(ggplot_network)

  }, error = function(e) {
    debug_log(paste("Error converting STRING to ggplot:", e$message), 1)

    # Robust fallback: create placeholder ggplot
    fallback_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "STRING Network\n(Conversion Error)",
               size = 6, hjust = 0.5, vjust = 0.5) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      ggtitle("STRING Network") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        panel.background = element_rect(fill = "lightgray", color = "black")
      )

    debug_log("Using fallback ggplot for STRING", 1)
    return(fallback_plot)
  })
}
