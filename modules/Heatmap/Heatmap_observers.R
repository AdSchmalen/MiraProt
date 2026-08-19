# ==============================================================================
# Heatmap Module - Observers and Event Coordination
# ==============================================================================
#
# Purpose:
#   Registers all observeEvent() and observe() handlers for the Heatmap module.
#   This includes UI update observers, data validation observers, protein
#   selection panel logic, sort-state synchronization, the main heatmap
#   creation pipeline, live-update triggers, and the control reset handler.
#
# Architecture role:
#   This file is the observer layer, sourced after Heatmap_rendering.R so that
#   compute_current_ordering, all draw functions, and all output$ bindings are
#   already defined in the closure. Heatmap_module.R delegates ALL observer
#   registration to this file.
#
# Structure:
#   1. Sort-state synchronization observer (pre-first-create sync)
#   2. UI update observers (data type, identifier columns, sample choices)
#   3. Data validation observers (abundance ratio, p-value controls)
#   4. Pathway dropdown observers (GSEA, GO)
#   5. Protein selection panel observers (suggest, add, remove, clear, pathway)
#   6. Protein input observers (custom text input, highlight button)
#   7. Reset button observer
#   8. run_heatmap_creation - main creation pipeline
#   9. Create button observeEvent
#  10. Live-update helpers and triggers
#
# Important notes for future developers:
#   - All observers run inside the moduleServer() closure via local = TRUE
#     sourcing, so input, output, session, rv, and all reactive state are
#     available without explicit passing.
#   - run_heatmap_creation is a plain function (not a reactive); it is called
#     from the create button observer and from live-update triggers.
#   - Debounced reactives (debounced_identifier_filter,
#     debounced_min_abundance_filter) must be defined before the observeEvent
#     that references them.
#
# Sourced by: Heatmap_module.R (after Heatmap_rendering.R)
# ==============================================================================

    # --------------------------------------------------------------------------
    # Observer groups are sourced in their original registration order.
    source("./modules/Heatmap/Heatmap_observers_data_choices.R", local = TRUE)
    source("./modules/Heatmap/Heatmap_observers_protein_selection.R", local = TRUE)
    source("./modules/Heatmap/Heatmap_observers_plot_lifecycle.R", local = TRUE)
    source("./modules/Heatmap/Heatmap_observers_restore.R", local = TRUE)
