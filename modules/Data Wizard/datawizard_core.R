# ============================================================================
# MiraProt File Contract: modules/Data Wizard/datawizard_core.R
# Purpose:
#   Provide the core portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Compatibility loader and public Core implementation composition root.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core factories create the sole canonical reactive state and adapters used by the parent module.
# Mutation Authority:
#   Mutation remains limited to the existing public helper factories and registered observers.
# Source-Order Assumptions:
#   Utilities and the dataset registry load before focused Core implementation files; consumers source only this compatibility path.
# Session/Restore Implications:
#   Public state keys and restore-facing factory behavior remain unchanged.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_core.R
# Purpose:
#   Define canonical Data Wizard reactive state containers and core coordination
#   helpers used across orchestration, integration, and export flows.
#
# Architectural Role:
#   core coordination
#
# Responsibilities:
#   - Create and manage canonical reactive value groups for data and UI state.
#   - Provide core access, mutation, metadata, and configuration helper factories.
#   - Enforce defensive state-handling helpers reused by higher layers.
#
# Non-Responsibilities (Must NOT be here):
#   - Directly initialize Shiny submodules.
#   - Contain UI rendering or Data Wizard orchestration wiring logic.
#
# Allowed Dependencies:
#   - In-scope utility layer (`datawizard_utils.R`).
#   - Shiny reactive primitives and existing project dependencies.
#
# Interaction Boundaries:
#   - Inputs:
#     Reactive state references and configuration payloads from orchestration/integration.
#   - Outputs:
#     Factory functions returning reactive containers and core helper function lists.
#   - Out-of-Scope Integrations:
#     None directly; interactions occur through higher-level orchestration/integration.
#
# Stability Guarantees:
#   - Preserve reactive key names and helper function interfaces used by callers.
#   - Keep backward-compatible defaults and failure handling semantics.
#   - Avoid hidden side effects outside explicitly returned helpers.
# ============================================================================
# modules/Data Wizard/datawizard_core.R
# Data Wizard Core Functionality - State Management and Configuration

# Source utilities
source("modules/Data Wizard/datawizard_utils.R", local = TRUE)
source("modules/Data Wizard/datawizard_dataset_registry.R", local = TRUE)

# Incremental compatibility children share this loader's evaluation environment.
source("modules/Data Wizard/core/datawizard_core_projections.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_state_adapter.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_reactive_values.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_access_tracking.R", local = TRUE)

source("modules/Data Wizard/core/datawizard_core_ui_configuration.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_metadata_updates.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_safe_ui.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_submodule_session.R", local = TRUE)
source("modules/Data Wizard/core/datawizard_core_lifecycle_observers.R", local = TRUE)
