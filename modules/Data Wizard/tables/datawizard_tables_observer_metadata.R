# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_metadata.R
# Purpose:
#   Provide the tables observer metadata portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard Tables metadata compatibility loader.
#
# Implementation ownership is delegated to the phase-specific files below.
# Source order is contractual: hydration, sync rendering, then editing.
source("modules/Data Wizard/tables/datawizard_tables_observer_metadata_hydration.R", local = TRUE)
source("modules/Data Wizard/tables/datawizard_tables_observer_metadata_sync.R", local = TRUE)
source("modules/Data Wizard/tables/datawizard_tables_observer_metadata_editing.R", local = TRUE)
