# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_diagnostics.R
# Purpose:
#   Provide the file loader diagnostics portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Mechanical observer/output-family extraction from datawizard_file_loader.R.
register_datawizard_file_loader_diagnostics <- function(loader_environment = parent.frame()) {
  evalq({
    # Log startup conditions
    observe({
      debug_log(paste("Startup check - Session active:", !is.null(session)), 2)
    }, priority = 1000)  # High priority to run early
    # Track changes using data_primary's exact reactiveVal identity.
    observe({
      current_primary <- data_primary()
      if (!is.null(current_primary)) {
        debug_log(paste("File loader: primary data changed to", nrow(current_primary), "x", ncol(current_primary)), level = 2)
        debug_log(paste("File loader: sample columns:", paste(head(names(current_primary), 3), collapse = ", ")), level = 2)
      }
    })
    # ========================================
    # Module Health Check
    # ========================================

    module_health_check <- function() {
      tryCatch({
        health_status <- list(
          module_name = "File Loader",
          status = "OK",
          loading_active = loading_active(),
          error_count = length(loading_errors()),
          debug_level = DEBUG_LEVEL,
          has_primary_data = !is.null(data_primary()),
          has_additional_data = !is.null(data_additional()),
          cache_size = length(file_cache()),
          current_operation = current_operation(),
          module_initialized = module_initialized()
        )

        warnings <- character()
        if (health_status$error_count > 3) {
          warnings <- c(warnings, paste("High error count:", health_status$error_count))
        }

        if (!health_status$module_initialized) {
          warnings <- c(warnings, "Module not yet initialized")
        }

        health_status$warnings <- warnings
        health_status$overall_health <- if (length(warnings) == 0) "Good" else "Warning"

        debug_log(paste("Health check - Status:", health_status$overall_health), 2)
        return(health_status)

      }, error = function(e) {
        debug_log(paste("Error in health check:", e$message), 1)
        return(list(module_name = "File Loader", status = "ERROR", overall_health = "Critical"))
      })
    }

    # ========================================
    # Session Cleanup
    # ========================================

    cleanup_manager$register_module("file_loader", function() {
      debug_log("Executing file loader cleanup", 2)

      # Clear all reactive values
      data_fixed(NULL)
      data2_fixed(NULL)
      data_primary(NULL)
      data_additional(NULL)
      loading_errors(list())
      loading_history(list())
      file_cache(list())
      loading_active(FALSE)
      current_operation("")
      module_initialized(FALSE)

      debug_log("File loader cleanup completed", 2)
    })
  }, envir = loader_environment)
  invisible(NULL)
}
