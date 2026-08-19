# ==============================================================================
# STRING Module - Orchestrator
# ==============================================================================
#
# Purpose:
#   Single entry point for the STRING database interaction network module.
#   Defines the public UI function (modSTRINGUI) and the only server function
#   (modSTRINGServer). Orchestrates sub-script sourcing, debug logging
#   initialization, reactive state setup, factory initialization, and observer
#   registration. Contains no business logic, no observer definitions, and no
#   output rendering code.
#
# Architecture role:
#   This file is the ONLY file in the STRING module that defines a Shiny server
#   function. All logic, state, and observers are delegated to the sub-scripts
#   sourced below and their factory functions.
#
# Sub-script responsibilities:
#   - modules/STRING/STRING_ui.R:                  Module UI builder.
#   - modules/STRING/STRING_utils.R:               Pure utility and conversion
#                                                   functions (no reactive state).
#   - modules/STRING/STRING_server_reactive_state.R: All reactiveVal() definitions.
#   - modules/STRING/STRING_server_helpers.R:       Helper factories (dropdown
#                                                   updates, input parsing, mapping).
#   - modules/STRING/STRING_server_data_sources.R:  GSEA/GO data access and protein
#                                                   input selection management.
#   - modules/STRING/STRING_server_network_build.R: Network creation observer.
#   - modules/STRING/STRING_server_styling.R:       Visual property observers and
#                                                   apply_label_variant.
#   - modules/STRING/STRING_server_interactions.R:  Structural interaction observers
#                                                   (layout, filter, selection, display).
#   - modules/STRING/STRING_server_export_grid.R:   Export and grid integration.
#
# Initialization order inside modSTRINGServer:
#   1. debug_log definition
#   2. Reactive state initialization and unpacking
#   3. Helper factory initialization
#   4. Data source factory initialization
#   5. Styling factory initialization (returns apply_label_variant)
#   6. Network build factory initialization (receives apply_label_variant)
#   7. Interaction factory initialization
#   8. Export/grid factory initialization
#   9. Observer registration (data sources, styling, network build, interactions, export)
#   10. Cleanup registration
#
# Logging model:
#   debug_log is defined once inside modSTRINGServer and passed explicitly to
#   all factory functions. level 1 = important state transitions; level 2 = verbose
#   tracing.
#
# Cleanup:
#   All session cleanup is registered via cleanup_manager$register_module.
#   No session$onSessionEnded calls are used.
#
# Future developers:
#   - Do not add logic, observers, or output rendering to this file.
#   - Keep this file as a thin wiring layer only.
#   - The initialization order above must be respected (styling before network build).
# ==============================================================================

# Source UI builder (module level, outside server)
source("./modules/STRING/STRING_ui.R",                    local = TRUE)
source("./modules/STRING/STRING_utils.R",                 local = TRUE)
source("./modules/STRING/STRING_server_reactive_state.R", local = TRUE)
source("./modules/STRING/STRING_server_helpers.R",        local = TRUE)
source("./modules/STRING/STRING_server_data_sources.R",   local = TRUE)
source("./modules/STRING/STRING_server_network_build.R",  local = TRUE)
source("./modules/STRING/STRING_server_styling.R",        local = TRUE)
source("./modules/STRING/STRING_server_interactions.R",   local = TRUE)
source("./modules/STRING/STRING_server_export_grid.R",    local = TRUE)

# ==============================================================================
# Module UI
# ==============================================================================

#' STRING Module UI
#' @param id Module namespace ID
modSTRINGUI <- function(id) {
  ns <- NS(id)
  tagList(
    create_STRING_ui(ns)
  )
}

# ==============================================================================
# Module Server (the only server function for this module)
# ==============================================================================

#' STRING Module Server
#' @param id Module namespace ID
#' @param rv Reactive values containing data and metadata
#' @param res_GSEA Reactive containing GSEA results (deprecated - use module_outputs)
#' @param GO_res Reactive containing GO results (deprecated - use module_outputs)
#' @param module_outputs List for cross-module communication (preferred)
#' @param debug_level Debug level from parent (1 = important, 2 = verbose)
modSTRINGServer <- function(id, rv, res_GSEA = NULL, GO_res = NULL, module_outputs = NULL, debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --------------------------------------------------------------------------
    # Debug logging
    # --------------------------------------------------------------------------
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "STRING MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ STRING MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("STRING module server starting", 1)

    # Guard state used while selection synchronization updates styling controls.
    # The depth flag catches same-cycle observer invalidations, while pending
    # expected values catch the later client echo from update*Input calls.
    styling_ui_sync_state <- reactiveValues(depth = 0L, pending = list())

    # --------------------------------------------------------------------------
    # Reactive state
    # --------------------------------------------------------------------------
    reactive_state <- initialize_STRING_reactive_state()

    res_STRING              <- reactive_state$res_STRING
    vector_plotted_STRING   <- reactive_state$vector_plotted_STRING
    test_g                  <- reactive_state$test_g
    vis_data                <- reactive_state$vis_data
    nodes                   <- reactive_state$nodes
    edges                   <- reactive_state$edges
    removed_nodes           <- reactive_state$removed_nodes
    removed_edges           <- reactive_state$removed_edges
    selected_data_STRING    <- reactive_state$selected_data_STRING
    selected_protein_vector <- reactive_state$selected_protein_vector
    label_alias_data        <- reactive_state$label_alias_data
    label_initial_labels    <- reactive_state$label_initial_labels
    selectedNodes           <- reactive_state$selectedNodes
    selectedEdges           <- reactive_state$selectedEdges
    current_selection_type  <- reactive_state$current_selection_type
    last_selection_type     <- reactive_state$last_selection_type
    original_nodes          <- reactive_state$original_nodes
    original_edges          <- reactive_state$original_edges
    current_min_degree      <- reactive_state$current_min_degree
    cluster_labels_version  <- reactive_state$cluster_labels_version
    identifier_type_used    <- reactive_state$identifier_type_used

    debug_log("Reactive state initialized", 2)

    # --------------------------------------------------------------------------
    # Factory initialization
    # --------------------------------------------------------------------------
    helper_fns <- initialize_STRING_server_helpers(session, debug_log)

    data_source_fns <- initialize_STRING_data_sources(
      input                        = input,
      session                      = session,
      debug_log                    = debug_log,
      module_outputs               = module_outputs,
      res_GSEA                     = res_GSEA,
      GO_res                       = GO_res,
      update_gsea_dropdown_choices = helper_fns$update_gsea_dropdown_choices,
      update_go_dropdown_choices   = helper_fns$update_go_dropdown_choices,
      rv                           = rv,
      selected_data_STRING         = selected_data_STRING,
      selected_protein_vector      = selected_protein_vector,
      test_g                       = test_g,
      ceb3_combined                = ceb3_combined
    )

    styling_fns <- initialize_STRING_styling(
      input                   = input,
      output                  = output,
      session                 = session,
      ns                      = ns,
      debug_log               = debug_log,
      nodes                   = nodes,
      edges                   = edges,
      vis_data                = vis_data,
      original_nodes          = original_nodes,
      selectedNodes           = selectedNodes,
      selectedEdges           = selectedEdges,
      current_selection_type  = current_selection_type,
      last_selection_type     = last_selection_type,
      label_alias_data        = label_alias_data,
      label_initial_labels    = label_initial_labels,
      format_node_label       = format_node_label,
      shape_mapping           = shape_mapping,
      update_node_ui_controls = update_node_ui_controls,
      update_edge_ui_controls = update_edge_ui_controls,
      cluster_labels_version  = cluster_labels_version,
      styling_ui_sync_state   = styling_ui_sync_state
    )

    network_build_fns <- initialize_STRING_network_build(
      input                      = input,
      output                     = output,
      session                    = session,
      ns                         = ns,
      debug_log                  = debug_log,
      selected_protein_vector    = selected_protein_vector,
      vector_plotted_STRING      = vector_plotted_STRING,
      parse_neighbor_count_input = helper_fns$parse_neighbor_count_input,
      map_proteins_with_fallback = helper_fns$map_proteins_with_fallback,
      original_nodes             = original_nodes,
      original_edges             = original_edges,
      current_min_degree         = current_min_degree,
      test_g                     = test_g,
      res_STRING                 = res_STRING,
      vis_data                   = vis_data,
      nodes                      = nodes,
      edges                      = edges,
      label_alias_data           = label_alias_data,
      label_initial_labels       = label_initial_labels,
      load_string_aliases        = load_string_aliases,
      apply_label_variant        = styling_fns$apply_label_variant,
      create_vis_network         = create_vis_network,
      ceb3_combined              = ceb3_combined,
      identifier_type_used       = identifier_type_used
    )

    interaction_fns <- initialize_STRING_interactions(
      input                       = input,
      output                      = output,
      session                     = session,
      ns                          = ns,
      debug_log                   = debug_log,
      rv                          = rv,
      test_g                      = test_g,
      res_STRING                  = res_STRING,
      vis_data                    = vis_data,
      nodes                       = nodes,
      edges                       = edges,
      selected_data_STRING        = selected_data_STRING,
      original_nodes              = original_nodes,
      original_edges              = original_edges,
      current_min_degree          = current_min_degree,
      selectedNodes               = selectedNodes,
      selectedEdges               = selectedEdges,
      current_selection_type      = current_selection_type,
      last_selection_type         = last_selection_type,
      apply_layout_transformation = apply_layout_transformation,
      update_node_ui_controls     = update_node_ui_controls,
      update_edge_ui_controls     = update_edge_ui_controls,
      styling_ui_sync_state       = styling_ui_sync_state,
      ceb3_combined               = ceb3_combined,
      cluster_labels_version      = cluster_labels_version,
      reset_string_organism_choices = helper_fns$reset_string_organism_choices
    )

    export_grid_fns <- initialize_STRING_export_grid(
      input                    = input,
      output                   = output,
      session                  = session,
      ns                       = ns,
      rv                       = rv,
      debug_log                = debug_log,
      test_g                   = test_g,
      nodes                    = nodes,
      edges                    = edges,
      open_STRING_export_device = helper_fns$open_STRING_export_device,
      convert_vis_to_igraph    = convert_vis_to_igraph,
      convert_string_to_ggplot = convert_string_to_ggplot
    )

    # --------------------------------------------------------------------------
    # Observer registration
    # --------------------------------------------------------------------------
    data_source_fns$register_data_source_observers()
    data_source_fns$register_selection_observers()
    styling_fns$register_styling_observers()
    network_build_fns$register_network_build_observer()
    interaction_fns$register_interaction_observers()
    export_grid_fns$register_export_grid_observers()
    helper_fns$register_string_organism_observer(input)

    # --------------------------------------------------------------------------
    # Session cleanup
    # --------------------------------------------------------------------------
    cleanup_manager$register_module("STRING", function() {
      debug_log("Executing [STRING] cleanup", 2)
      res_STRING(NULL)
      test_g(NULL)
      vis_data(NULL)
      nodes(NULL)
      edges(NULL)
      original_nodes(NULL)
      original_edges(NULL)
      removed_nodes(data.frame())
      removed_edges(data.frame())
      selected_data_STRING(NULL)
      selected_protein_vector(NULL)
      selectedNodes(NULL)
      selectedEdges(NULL)
      label_alias_data(NULL)
      label_initial_labels(NULL)
      vector_plotted_STRING(NULL)
      cluster_labels_version(0L)
      debug_log("[STRING] cleanup completed", 2)
    })

    debug_log("STRING module server initialized", 1)

    # ==========================================================================
    # Session restore: re-register visNetwork renderer
    # ==========================================================================
    # The visNetwork output renderer is normally registered inside the
    # "create_STRING" button observer. After session restore the nodes/edges
    # are available but the renderer was never set up because the button was
    # not pressed. This observer detects restored nodes/edges and re-registers
    # the renderVisNetwork so the network displays immediately.
    observeEvent(rv$session_restore_trigger, {
      tryCatch({
        current_nodes <- nodes()
        current_edges <- edges()
        if (is.null(current_nodes) || !is.data.frame(current_nodes) ||
            nrow(current_nodes) == 0) {
          return()
        }

        debug_log("[STRING] session restore: re-registering visNetwork renderer", 1)
        output$String_plot <- renderVisNetwork({
          tryCatch({
            req(nodes(), edges())
            n <- nodes()
            e <- edges()
            debug_log(paste("[STRING] restore render: ", nrow(n), "nodes,", nrow(e), "edges"), 2)
            create_vis_network(n, e, ns = ns)
          }, error = function(e) {
            debug_log(paste("[STRING] restore render error:", e$message), 1)
            NULL
          })
        })
        debug_log("[STRING] session restore: visNetwork renderer registered", 1)
      }, error = function(e) {
        debug_log(paste("[STRING] session restore failed:", e$message), 1)
      })
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Module return interface
    # --------------------------------------------------------------------------
    #
    # Session save/restore design notes:
    #
    # The STRING network is reconstructible from the persisted data.frames plus
    # a small cluster-membership vector. We deliberately DO NOT serialize:
    #   - res_STRING$DB  (STRINGdb R6 object: wraps SQLite handle and a deep
    #                    environment chain; serialization hangs / times out).
    #   - res_STRING$STRING_Graph  (igraph; reconstructible from original_edges).
    #   - res_STRING$cluster       (communities object; only membership is used
    #                               downstream -- persisted as a tiny named int).
    #   - res_STRING$Mapped        (never read after network build).
    #   - test_g                   (derived from original_nodes/original_edges).
    #   - vis_data                 (derived from original_nodes/original_edges).
    #
    # On restore we rebuild test_g, vis_data, and a minimal res_STRING
    # (Mapped = NULL, DB = NULL, STRING_Graph = rebuilt graph, cluster =
    # make_clusters(graph, membership)). This keeps layout / cluster / reset /
    # filter observers fully functional without persisting heavy objects.

    return(list(
      # Excel export interface
      get_export_data = function() {
        list(
          nodes = tryCatch(isolate(nodes()), error = function(e) NULL),
          edges = tryCatch(isolate(edges()), error = function(e) NULL),
          ui_inputs = list(
            version           = tryCatch(isolate(input$version_STRING),           error = function(e) NA_character_),
            score             = tryCatch(isolate(input$score_STRING),             error = function(e) NA_real_),
            edge_type         = tryCatch(isolate(input$edgetype_STRING),          error = function(e) NA_character_),
            organism_species  = tryCatch(isolate(input$organism_STRING),          error = function(e) NA_character_),
            neighbor_count    = tryCatch(isolate(input$neighbor_count_STRING),    error = function(e) NA_real_),
            neighbor_strategy = tryCatch(isolate(input$neighbor_strategy_STRING), error = function(e) NA_character_),
            label_variant     = tryCatch(isolate(input$label_variant_STRING),     error = function(e) NA_character_),
            identifier_type   = tryCatch({
              stored <- isolate(identifier_type_used())
              if (length(stored) == 1L && !is.na(stored) && nzchar(stored)) {
                stored
              } else {
                fallback <- isolate(input$identifier_type_STRING)
                if (!is.null(fallback) && nzchar(fallback)) fallback else NA_character_
              }
            }, error = function(e) NA_character_),
            min_degree        = tryCatch(isolate(input$EdgeNum_STRING),           error = function(e) NA_real_)
          )
        )
      },

      # Session save/restore interface
      get_session_state = function() {
        state <- list(version = "2.0")
        state$vector_plotted_STRING   <- tryCatch(isolate(vector_plotted_STRING()), error = function(e) NULL)
        state$nodes                   <- tryCatch(isolate(nodes()), error = function(e) NULL)
        state$edges                   <- tryCatch(isolate(edges()), error = function(e) NULL)
        state$original_nodes          <- tryCatch(isolate(original_nodes()), error = function(e) NULL)
        state$original_edges          <- tryCatch(isolate(original_edges()), error = function(e) NULL)
        state$removed_nodes           <- tryCatch(isolate(removed_nodes()), error = function(e) NULL)
        state$removed_edges           <- tryCatch(isolate(removed_edges()), error = function(e) NULL)
        state$selected_data_STRING    <- tryCatch(isolate(selected_data_STRING()), error = function(e) NULL)
        state$selected_protein_vector <- tryCatch(isolate(selected_protein_vector()), error = function(e) NULL)
        state$label_alias_data        <- tryCatch(isolate(label_alias_data()), error = function(e) NULL)
        state$label_initial_labels    <- tryCatch(isolate(label_initial_labels()), error = function(e) NULL)
        state$current_min_degree      <- tryCatch(isolate(current_min_degree()), error = function(e) 0)
        state$identifier_type_used    <- tryCatch(isolate(identifier_type_used()), error = function(e) NULL)
        state$ui_inputs <- tryCatch({
          organism_species <- isolate(input$organism_STRING)
          list(
            organism_species = organism_species,
            organism_name    = helper_fns$get_string_organism_label(organism_species)
          )
        }, error = function(e) NULL)

        # Cluster membership (tiny named integer vector) + algorithm name.
        # Replaces res_STRING$cluster without persisting the communities object.
        cluster_info <- tryCatch({
          r <- isolate(res_STRING())
          if (is.null(r) || is.null(r$cluster)) {
            list(membership = NULL, algorithm = NULL)
          } else {
            mem <- igraph::membership(r$cluster)
            algo <- if (!is.null(r$cluster$algorithm)) as.character(r$cluster$algorithm) else "edge_betweenness"
            list(
              membership = setNames(as.integer(mem), names(mem)),
              algorithm  = algo
            )
          }
        }, error = function(e) {
          list(membership = NULL, algorithm = NULL)
        })
        state$cluster_membership <- cluster_info$membership
        state$cluster_algorithm  <- cluster_info$algorithm

        # Note: res_STRING, test_g, vis_data are derived and intentionally
        # excluded (see design notes above).
        state
      },
      set_session_state = function(state) {
        if (is.null(state)) return()

        # --- Phase 1: restore persisted reactive values ---
        if (!is.null(state$vector_plotted_STRING))    vector_plotted_STRING(state$vector_plotted_STRING)
        if (!is.null(state$nodes))                    nodes(state$nodes)
        if (!is.null(state$edges))                    edges(state$edges)
        if (!is.null(state$original_nodes))           original_nodes(state$original_nodes)
        if (!is.null(state$original_edges))           original_edges(state$original_edges)
        if (!is.null(state$removed_nodes))            removed_nodes(state$removed_nodes)
        if (!is.null(state$removed_edges))            removed_edges(state$removed_edges)
        if (!is.null(state$selected_data_STRING))     selected_data_STRING(state$selected_data_STRING)
        if (!is.null(state$selected_protein_vector))  selected_protein_vector(state$selected_protein_vector)
        if (!is.null(state$label_alias_data))         label_alias_data(state$label_alias_data)
        if (!is.null(state$label_initial_labels))     label_initial_labels(state$label_initial_labels)
        if (!is.null(state$current_min_degree))       current_min_degree(state$current_min_degree)
        if (!is.null(state$identifier_type_used))     identifier_type_used(state$identifier_type_used)

        # Restore UI-only STRING organism selection. The full species list is not
        # serialized; the saved selected organism is merged into the default list
        # when needed, and the Update Organisms button can refresh all choices.
        tryCatch({
          ui_inputs <- if (!is.null(state$ui_inputs) && is.list(state$ui_inputs)) state$ui_inputs else list()
          restored_species <- ui_inputs$organism_species
          restored_name <- ui_inputs$organism_name
          if (length(restored_species) == 1L && !is.na(restored_species) && nzchar(as.character(restored_species))) {
            organism_choices <- helper_fns$default_string_organism_choices()
            if (!(as.character(restored_species) %in% unname(organism_choices))) {
              display_name <- if (length(restored_name) == 1L && !is.na(restored_name) && nzchar(as.character(restored_name))) {
                as.character(restored_name)
              } else {
                paste("Restored species", as.character(restored_species))
              }
              organism_choices <- c(organism_choices, stats::setNames(as.character(restored_species), display_name))
            }
            helper_fns$set_current_string_organism_choices(organism_choices)
            updateSelectInput(session, "organism_STRING", choices = organism_choices, selected = as.character(restored_species))
          }
        }, error = function(e) {
          debug_log(paste("[STRING] restore: organism selection restore failed:", e$message), 1)
        })

        # --- Phase 2: rebuild derived objects (test_g, vis_data, res_STRING) ---
        # Old snapshots (version "1.0") may contain state$res_STRING with a
        # STRINGdb R6 object inside -- we intentionally ignore it and rebuild.
        tryCatch({
          orig_nodes <- state$original_nodes
          orig_edges <- state$original_edges

          if (!is.null(orig_nodes) && is.data.frame(orig_nodes) && nrow(orig_nodes) > 0 &&
              !is.null(orig_edges) && is.data.frame(orig_edges)) {

            vertices_df <- data.frame(
              name = as.character(orig_nodes$id),
              stringsAsFactors = FALSE
            )
            edges_df <- data.frame(
              from = as.character(orig_edges$from),
              to   = as.character(orig_edges$to),
              stringsAsFactors = FALSE
            )

            g <- igraph::graph_from_data_frame(
              edges_df,
              directed = FALSE,
              vertices = vertices_df
            )
            debug_log(paste("[STRING] restore: rebuilt test_g with",
                            igraph::vcount(g), "nodes,",
                            igraph::ecount(g), "edges"), 2)

            # Rebuild cluster. Prefer persisted membership; fall back to
            # recomputing edge-betweenness locally (no network call).
            ceb <- NULL
            saved_mem <- state$cluster_membership
            if (!is.null(saved_mem) && length(saved_mem) > 0) {
              aligned <- saved_mem[igraph::V(g)$name]
              if (!any(is.na(aligned)) && length(aligned) == igraph::vcount(g)) {
                ceb <- tryCatch(
                  igraph::make_clusters(
                    g,
                    membership = as.integer(aligned),
                    algorithm  = state$cluster_algorithm %||% "edge_betweenness",
                    modularity = FALSE
                  ),
                  error = function(e) {
                    debug_log(paste("[STRING] restore: make_clusters failed:", e$message), 1)
                    NULL
                  }
                )
              } else {
                debug_log("[STRING] restore: saved membership misaligned with graph; recomputing", 1)
              }
            }
            if (is.null(ceb)) {
              ceb <- tryCatch(
                igraph::cluster_edge_betweenness(g),
                error = function(e) {
                  debug_log(paste("[STRING] restore: cluster_edge_betweenness failed:", e$message), 1)
                  NULL
                }
              )
            }

            test_g(g)
            res_STRING(list(
              Mapped       = NULL,
              DB           = NULL,
              STRING_Graph = g,
              cluster      = ceb
            ))
            vis_data(list(nodes = orig_nodes, edges = orig_edges))

            debug_log("[STRING] restore: test_g, vis_data, res_STRING rebuilt", 1)
          } else {
            debug_log("[STRING] restore: no original_nodes/edges -- skipping derived rebuild", 2)
          }
        }, error = function(e) {
          debug_log(paste("[STRING] restore: derived object rebuild failed:", e$message), 1)
        })

        debug_log("[STRING] session state restored via set_session_state", 1)
      }
    ))
  })
}
