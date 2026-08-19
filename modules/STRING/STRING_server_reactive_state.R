# ==============================================================================
# STRING Module - Reactive State
# ==============================================================================
#
# Purpose:
#   Defines all reactiveVal() instances used across the STRING module.
#   Returned as a named list for unpacking inside modSTRINGServer.
#
# Architecture role:
#   initialize_STRING_reactive_state is sourced at the module level (outside
#   the server function). It is called as the first step inside modSTRINGServer
#   to create all reactive state for the session.
#
# File structure:
#   Single function initialize_STRING_reactive_state that returns a named list
#   of all reactiveVal instances grouped by concern:
#   - Network data (res_STRING, test_g, vis_data, nodes, edges)
#   - Original/filtered state (original_nodes, original_edges, current_min_degree)
#   - Removed elements tracking (removed_nodes, removed_edges)
#   - Selection state (selectedNodes, selectedEdges, current_selection_type, last_selection_type)
#   - Protein selection (selected_data_STRING, selected_protein_vector, vector_plotted_STRING)
#   - Label state (label_alias_data, label_initial_labels)
#
# Future developers:
#   - All new module-level reactive state must be added here.
#   - Do not add reactive expressions or observers to this file.
# ==============================================================================

initialize_STRING_reactive_state <- function() {
  state <- list(
    res_STRING = reactiveVal(NULL),
    vector_plotted_STRING = reactiveVal(NULL),
    test_g = reactiveVal(NULL),
    vis_data = reactiveVal(NULL),
    nodes = reactiveVal(NULL),
    edges = reactiveVal(NULL),
    removed_nodes = reactiveVal(data.frame()),
    removed_edges = reactiveVal(data.frame()),
    selected_data_STRING = reactiveVal(NULL),
    selected_protein_vector = reactiveVal(NULL),
    label_alias_data = reactiveVal(NULL),
    label_initial_labels = reactiveVal(NULL),
    selectedNodes = reactiveVal(NULL),
    selectedEdges = reactiveVal(NULL),
    current_selection_type = reactiveVal(NULL),
    last_selection_type = reactiveVal(NULL),
    original_nodes = reactiveVal(NULL),
    original_edges = reactiveVal(NULL),
    current_min_degree = reactiveVal(0),
    cluster_labels_version = reactiveVal(0L),
    identifier_type_used = reactiveVal(NULL)
  )

  state
}
