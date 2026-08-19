# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_access_tracking.R
# Purpose:
#   Provide the core access tracking portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard data access and modification tracking factories

#' Create data access helper functions
#' @param core_values list of core reactive values
#' @return list of data access functions
create_data_access_functions <- function(core_values) {
  primary_data_state <- create_primary_data_state_adapter(core_values = core_values)

  list(
    get_file_data = function() {
      if (core_values$filter_applied() && !is.null(core_values$filtered_data())) {
        return(core_values$filtered_data())
      } else {
        return(core_values$primary_data_raw())
      }
    },

    set_file_data = function(new_data) {
      if (!is.null(new_data)) {
        primary_data_state$set_raw_imported_data(new_data, "data access set_file_data")
        return(TRUE)
      }
      return(FALSE)
    }
  )
}

#' Create data modification tracking functions
#' @param core_values list of core reactive values
#' @return list of modification tracking functions
create_modification_tracking_functions <- function(core_values) {
  list(
    record_modification = function(operation, details = "") {
      current_history <- core_values$modification_history()
      new_entry <- list(
        timestamp = Sys.time(),
        operation = operation,
        details = details
      )
      core_values$modification_history(c(current_history, list(new_entry)))
      core_values$data_modified(TRUE)
      debug_log(paste("Data modification recorded:", operation), level = 2)
    },

    reset_modification_tracking = function() {
      core_values$data_modified(FALSE)
      core_values$modification_history(list())
      debug_log("Data modification tracking reset", level = 2)
    },

    is_data_modified = reactive({
      core_values$data_modified() ||
        core_values$filter_applied() ||
        (!is.null(core_values$primary_data_raw()) &&
           any(grepl("^Imputed |^Batch Corrected ", names(core_values$primary_data_raw()))))
    })
  )
}
#' Create metadata content status management
#' @param core_values list of core reactive values
#' @return reactive for metadata content readiness
create_metadata_content_status <- function(core_values) {
  reactive({
    tryCatch({
      current_metadata <- core_values$handson_metadata()

      if (is.null(current_metadata) || !is.data.frame(current_metadata) || nrow(current_metadata) == 0) {
        return(FALSE)
      }

      if (!"Content" %in% names(current_metadata)) {
        return(FALSE)
      }

      content_values <- current_metadata$Content
      valid_content <- content_values[!is.na(content_values) & nzchar(trimws(content_values))]

      if (length(valid_content) == 0) {
        return(FALSE)
      }

      non_row_index_content <- valid_content[valid_content != "Row Index"]

      if (length(non_row_index_content) == 0) {
        debug_log("Content column only contains 'Row Index' - metadata not ready", 2)
        return(FALSE)
      }

      debug_log(paste("Metadata content ready:", length(non_row_index_content), "meaningful assignments found"), 2)
      return(TRUE)

    }, error = function(e) {
      debug_log(paste("Error checking central metadata content status:", e$message), 1)
      return(FALSE)
    })
  })
}
