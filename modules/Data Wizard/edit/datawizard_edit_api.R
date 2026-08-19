# ============================================================================
# Sub-Script: Data Wizard Edit API
#
# Purpose:
#   Define all external-facing (programmatic) functions and build the
#   return-list that the orchestrator hands back from moduleServer().
#
# Architectural Role:
#   Public interface layer. Other modules (e.g., Templates, Core) interact
#   with the Edit module exclusively through the list returned by
#   register_edit_api(). No observer/handler logic belongs here — only
#   callable functions, reactive accessors, and the health-check.
#
# Structure:
#   register_edit_api(ctx) is called once from the orchestrator with the
#   moduleServer environment. It uses evalq() so that all reactive state
#   variables, debug_log, get_data, set_data, mark_operation_executed,
#   and utility functions are accessible.
#
#   Returns the list that becomes the module's public interface.
#
# Functions defined here:
#   - load_operations_table(ops_table)  : validate + load an external table
#   - apply_all_operations()            : programmatic batch apply (no UI)
#   - has_pending_operations()          : TRUE if un-executed ops remain
#   - get_operations_summary()          : human-readable status string
#
# Return-list members (reactive accessors):
#   pending_operations, load_operations_table, get_performance_metrics,
#   module_health_check, operations_table_applied,
#   operations_table_source_info, get_operations_table_errors,
#   apply_all_operations, has_pending_operations, get_operations_summary,
#   get_pending_operations_count, get_executed_operations_count,
#   get_original_data, has_original_data, get_selected_columns_info,
#   apply_trigger
#
# Dependencies (from parent environment):
#   input, get_data, set_data, debug_log, DEBUG_LEVEL
#   All reactive state variables from create_edit_reactive_state()
#   mark_operation_executed (from datawizard_edit_handlers.R)
#   validate_operations_table, apply_single_operation (from utils)
#
# Future Developer Notes:
#   - If you add a new function that callers need, define it here and
#     add it to the return list at the bottom.
#   - The health_check still references health_status$log_entries which
#     is from the removed operation-log feature. It is wrapped in
#     tryCatch so it fails silently, but can be cleaned up.
# ============================================================================


register_edit_api <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_edit_api requires an environment context")
  }

  evalq({

    # ==================================================================
    # API Function: Load Operations Table (external source)
    # ==================================================================

    load_operations_table <- function(operations_table) {
      if (is.null(operations_table)) {
        debug_log("No operations table to load", 2)
        return(TRUE)
      }

      tryCatch({
        debug_log(paste("Loading operations table with", nrow(operations_table), "operations"), 1)

        current_data <- get_data()
        available_columns <- if (!is.null(current_data)) names(current_data) else character(0)

        validation_result <- validate_operations_table(
          operations_table, available_columns, reset_executed = TRUE, debug_log = debug_log
        )

        if (validation_result$success) {
          pending_operations(validation_result$operations)
          operations_table_applied(TRUE)
          operations_table_source_info("external_load")
          operations_table_errors(list())

          if (validation_result$removed_count > 0) {
            showNotification(
              paste("Loaded", nrow(validation_result$operations), "operations.",
                    validation_result$removed_count, "operations removed due to missing columns."),
              type = "warning", duration = 3
            )
          }

          debug_log("Operations table loaded successfully via external function", 1)
          return(TRUE)
        } else {
          operations_table_errors(append(operations_table_errors(), "External load validation failed"))
          showNotification("Failed to load operations table: invalid structure", type = "error")
          return(FALSE)
        }

      }, error = function(e) {
        debug_log(paste("Error loading operations table:", e$message), 1)
        operations_table_errors(append(operations_table_errors(), paste("External load error:", e$message)))
        showNotification(paste("Error loading operations table:", e$message), type = "error")
        return(FALSE)
      })
    }

    # ==================================================================
    # API Function: Apply All Operations (programmatic, no progress bar)
    # ==================================================================

    apply_all_operations <- function() {
      tryCatch({
        pending_ops <- pending_operations()

        if (nrow(pending_ops) == 0) {
          debug_log("No operations to apply", 2)
          return(list(success = TRUE, message = "No operations to apply"))
        }

        current_data <- get_data()
        if (is.null(current_data)) {
          debug_log("No data available for operations", 1)
          return(list(success = FALSE, message = "No data available"))
        }

        if (block_if_pending_operations_have_missing_columns(pending_ops, names(current_data), notify = TRUE)) {
          return(list(
            success = FALSE,
            message = "Queued operations reference missing columns; no operations were applied"
          ))
        }

        pending_ops_to_apply <- pending_ops[!pending_ops$Executed, , drop = FALSE]

        if (nrow(pending_ops_to_apply) == 0) {
          debug_log("All operations already executed", 2)
          return(list(success = TRUE, message = "All operations already executed"))
        }

        debug_log(paste("Applying", nrow(pending_ops_to_apply), "pending operations"), 1)

        modified_data <- current_data
        applied_count <- 0

        for (i in seq_len(nrow(pending_ops_to_apply))) {
          op <- pending_ops_to_apply[i, ]
          debug_log(paste("Applying operation", i, ":", op$Description), 2)

          result <- apply_single_operation(modified_data, op, debug_log)
          if (!result$success) {
            error_msg <- paste("Failed at operation", i, ":", result$message)
            debug_log(error_msg, 1)
            return(list(success = FALSE, message = error_msg))
          }

          modified_data <- result$data
          applied_count <- applied_count + 1
          debug_log(paste("Operation", i, "completed"), 2)

          # Shared helper from handlers file
          mark_operation_executed(op)
        }

        success <- set_data(modified_data)
        if (success) {
          success_msg <- paste("Successfully applied", applied_count, "operations")
          debug_log(success_msg, 1)
          return(list(success = TRUE, message = success_msg))
        } else {
          error_msg <- "Failed to update data after operations"
          debug_log(error_msg, 1)
          return(list(success = FALSE, message = error_msg))
        }

      }, error = function(e) {
        error_msg <- paste("Error in apply_all_operations:", e$message)
        debug_log(error_msg, 1)
        return(list(success = FALSE, message = error_msg))
      })
    }

    # ==================================================================
    # API Function: Query Helpers
    # ==================================================================

    has_pending_operations <- function() {
      tryCatch({
        ops <- pending_operations()
        return(sum(!ops$Executed, na.rm = TRUE) > 0)
      }, error = function(e) {
        debug_log(paste("Error checking pending operations:", e$message), 1)
        return(FALSE)
      })
    }

    get_operations_summary <- function() {
      tryCatch({
        ops <- pending_operations()
        if (nrow(ops) == 0) return("No operations")

        total_ops <- nrow(ops)
        pending_ops <- sum(!ops$Executed, na.rm = TRUE)
        executed_ops <- sum(ops$Executed, na.rm = TRUE)

        return(paste0(total_ops, " total (", pending_ops, " pending, ", executed_ops, " executed)"))
      }, error = function(e) {
        debug_log(paste("Error getting operations summary:", e$message), 1)
        return("Summary unavailable")
      })
    }

    # ==================================================================
    # Build Return Interface
    # ==================================================================

    list(
      # Main export: the operations table
      pending_operations = pending_operations,
      load_operations_table = load_operations_table,

      # Performance metrics
      get_performance_metrics = reactive({
        list(
          last_operation_time = last_operation_time(),
          max_log_entries = max_log_entries,
          cache_size = length(column_type_cache()),
          original_data_integrity = !is.null(original_data_hash())
        )
      }),

      # Health check
      module_health_check = function() {
        tryCatch({
          health_status <- list(
            module_name = "Edit",
            status = "OK",
            original_data_available = !is.null(original_data()),
            original_data_integrity = !is.null(original_data_hash()),
            operations_count = nrow(pending_operations()),
            pending_operations = sum(!pending_operations()$Executed, na.rm = TRUE),
            executed_operations = sum(pending_operations()$Executed, na.rm = TRUE),
            template_applied = operations_table_applied(),
            error_count = length(operations_table_errors()),
            cache_entries = length(column_type_cache()),
            debug_level = DEBUG_LEVEL,
            performance_data = !is.null(last_operation_time())
          )

          warnings <- character()
          if (!health_status$original_data_available) {
            warnings <- c(warnings, "No original data backup available")
          }
          if (!health_status$original_data_integrity) {
            warnings <- c(warnings, "Original data integrity verification not available")
          }
          if (health_status$error_count > 0) {
            warnings <- c(warnings, paste("Template errors:", health_status$error_count))
          }
          if (health_status$pending_operations > 20) {
            warnings <- c(warnings, "Large number of pending operations may impact performance")
          }

          health_status$warnings <- warnings
          health_status$overall_health <- if (length(warnings) == 0) "Good" else if (length(warnings) <= 2) "Warning" else "Critical"

          debug_log(paste("Module health check - Status:", health_status$overall_health), 2)
          return(health_status)

        }, error = function(e) {
          debug_log(paste("Error in module health check:", e$message), 1)
          return(list(
            module_name = "Edit",
            status = "ERROR",
            error_message = e$message,
            overall_health = "Critical"
          ))
        })
      },

      # Template integration status
      operations_table_applied = reactive({ operations_table_applied() }),
      operations_table_source_info = reactive({ operations_table_source_info() }),
      get_operations_table_errors = reactive({ operations_table_errors() }),

      # Interface functions
      apply_all_operations = apply_all_operations,
      has_pending_operations = has_pending_operations,
      get_operations_summary = get_operations_summary,

      # Status functions
      get_pending_operations_count = function() {
        tryCatch({
          ops <- pending_operations()
          return(sum(!ops$Executed, na.rm = TRUE))
        }, error = function(e) {
          debug_log(paste("Error getting pending operations count:", e$message), 1)
          return(0)
        })
      },

      get_executed_operations_count = function() {
        tryCatch({
          ops <- pending_operations()
          return(sum(ops$Executed, na.rm = TRUE))
        }, error = function(e) {
          debug_log(paste("Error getting executed operations count:", e$message), 1)
          return(0)
        })
      },

      # Data management functions
      get_original_data = reactive({
        tryCatch({
          original_data()
        }, error = function(e) {
          debug_log(paste("Error getting original data:", e$message), 1)
          NULL
        })
      }),

      has_original_data = reactive({
        tryCatch({
          !is.null(original_data())
        }, error = function(e) {
          debug_log(paste("Error checking original data:", e$message), 1)
          FALSE
        })
      }),

      # Column information access
      get_selected_columns_info = reactive({
        tryCatch({
          selected_columns_info()
        }, error = function(e) {
          debug_log(paste("Error getting selected columns info:", e$message), 1)
          list(
            overall_type = "unknown",
            individual_types = character(0),
            type_summary = "Error getting column info",
            compatible = FALSE,
            existing_columns = character(0)
          )
        })
      }),

      # Trigger for manual apply (reactive counter) - kept for compatibility
      apply_trigger = reactive({ input$apply_all_operations })
    )

  }, envir = ctx)
}
