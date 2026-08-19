# ============================================================================
# Sub-Script: Data Wizard Edit Handlers
#
# Purpose:
#   Own all observe() and observeEvent() registrations for the Edit module,
#   plus internal helper functions that are only used by handlers.
#
# Architectural Role:
#   Event-wiring layer. Reacts to user input and external data changes,
#   mutates reactive state accordingly, and shows user notifications.
#   Must not define output renderers (those belong in the outputs file)
#   or external API functions (those belong in the api file).
#
# Structure:
#   register_edit_handlers(ctx) is called once from the orchestrator with
#   the moduleServer environment. It uses evalq() so that input, session,
#   ns, all reactive state variables, and utility functions are accessible.
#
#   Internal helpers defined here:
#     - mark_operation_executed(operation) : finds and marks a matching
#       operation row as Executed in pending_operations. Shared by the
#       interactive apply handler and the programmatic API.
#     - add_operation_to_queue(...)        : creates a new operation row
#       and appends it to the pending_operations queue.
#
#   Registered observers/handlers:
#     - Data integrity: original data backup + structure change monitoring
#     - Data column signature invalidation for stale selections/cache
#     - Operations table import from template
#     - Column type detection with caching
#     - Category dropdown update from metadata
#     - Column dropdown update from selected category
#     - add_replace button handler
#     - add_edit button handler
#     - apply_all_operations button handler (interactive, with progress bar)
#     - clear_operations button handler
#
# Dependencies (from parent environment):
#   input, session, ns, get_data, set_data, metadata_def, operations_table
#   All reactive state variables from create_edit_reactive_state()
#   debug_log, DEBUG_LEVEL
#   Utility functions from datawizard_edit_utils.R:
#     serialize_parameters, validate_operations_table,
#     detect_multi_column_types, apply_single_operation
#   Factory from datawizard_edit_reactive_state.R:
#     create_empty_operations_df, create_default_columns_info
#
# Future Developer Notes:
#   - If you add a new observeEvent or observe, place it in this file.
#   - mark_operation_executed() is also called from the API file
#     (apply_all_operations programmatic function). Keep its contract
#     stable: it takes a single operation row and returns nothing.
#   - The #add_to_log(...) comments are remnants of a removed operation
#     log feature. They are kept as breadcrumbs; remove them freely if
#     they become confusing.
# ============================================================================


register_edit_handlers <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("register_edit_handlers requires an environment context")
  }

  evalq({

    # Shared Edit primary-data readiness state. Observers keep their own guards,
    # but readiness logging is centralized so normal startup emits one pause
    # message instead of one per observer.
    edit_data_ready <- reactive({
      if (is.function(has_data)) isTRUE(has_data()) else is.data.frame(get_data())
    })

    edit_data_readiness_state <- new.env(parent = emptyenv())
    edit_data_readiness_state$last_ready <- NULL

    observe({
      ready <- isTRUE(edit_data_ready())

      if (!identical(edit_data_readiness_state$last_ready, ready)) {
        if (!ready) {
          debug_log(
            paste(
              "Edit data readiness changed to not-ready;",
              "Edit data observers are paused until primary data is available"
            ),
            2
          )
        }

        edit_data_readiness_state$last_ready <- ready
      }
    })

    # ==================================================================
    # Internal Helper: Mark a single operation as executed
    # ==================================================================
    #
    # Finds the first non-executed row in pending_operations that matches
    # the given operation on all key fields and sets Executed = TRUE.
    # Used by both the interactive apply handler and the programmatic
    # apply_all_operations function (in the API file).

    mark_operation_executed <- function(operation) {
      current_ops <- pending_operations()
      for (j in seq_len(nrow(current_ops))) {
        if (!current_ops$Executed[j] &&
            current_ops$Operation[j] == operation$Operation &&
            current_ops$Type[j] == operation$Type &&
            current_ops$Columns[j] == operation$Columns &&
            current_ops$Parameters[j] == operation$Parameters) {
          current_ops$Executed[j] <- TRUE
          break
        }
      }
      pending_operations(current_ops)
    }

    # ==================================================================
    # Internal Helper: Add operation to queue
    # ==================================================================

    add_operation_to_queue <- function(operation_type, data_type, columns, params, description) {
      tryCatch({
        serialized_params <- serialize_parameters(params)

        current_ops <- pending_operations()
        new_operation <- data.frame(
          Operation = operation_type,
          Type = data_type,
          Columns = paste(columns, collapse = "|"),
          Parameters = serialized_params,
          Description = description,
          Executed = FALSE,
          stringsAsFactors = FALSE
        )

        pending_operations(rbind(current_ops, new_operation))

        # Update source info
        if (!operations_table_applied() || operations_table_source_info() == "template_import") {
          operations_table_source_info("manual_addition")
        }

        debug_log(paste(operation_type, "operation added:", description), 2)
        return(TRUE)

      }, error = function(e) {
        debug_log(paste("Error adding operation to queue:", e$message), 1)
        return(FALSE)
      })
    }


    signature_delta_preview_limit <- 5L

    get_data_column_signature <- function(data_df) {
      revision_sig <- tryCatch(data_revision_signature(), error = function(e) NULL)
      if (!is.null(revision_sig)) {
        return(list(revision = revision_sig, columns = if (is.null(data_df)) NULL else names(data_df)))
      }
      if (is.null(data_df)) {
        return(NULL)
      }
      names(data_df)
    }

    truncate_debug_text <- function(text, max_chars = 220) {
      text <- as.character(text)
      if (!nzchar(text)) {
        return(text)
      }
      if (nchar(text, type = "chars") <= max_chars) {
        return(text)
      }
      paste0(substr(text, 1, max_chars - 3), "...")
    }
    get_column_signature_key <- function(signature) {
      if (is.null(signature)) {
        return("NULL")
      }
      if (length(signature) == 0) {
        return("empty")
      }
      substr(digest::digest(signature, algo = "xxhash64"), 1, 8)
    }
    format_column_list <- function(columns, max_items = 4) {
      if (length(columns) == 0) {
        return("none")
      }
      shown <- head(columns, max_items)
      if (length(columns) <= max_items) {
        return(paste(shown, collapse = ", "))
      }
      paste0(
        paste(shown, collapse = ", "),
        ", ... (+", length(columns) - max_items, " more)"
      )
    }

    extract_signature_columns <- function(signature) {
      if (is.null(signature)) {
        return(character(0))
      }
      if (is.list(signature) && "columns" %in% names(signature)) {
        columns <- signature$columns
        if (is.null(columns)) {
          return(character(0))
        }
        return(as.character(columns))
      }
      as.character(signature)
    }

    format_revision_signature <- function(signature) {

      revision <-
        if (is.list(signature) &&
            "revision" %in% names(signature)) {
          signature$revision
        } else {
          NULL
        }

      if (is.null(revision)) {
        return("none")
      }

      # metadata_content_signature can contain the complete metadata/header
      # signature and is far and is far too verbose for a debug message. Preserve its
      # diagnostic value as a short hash instead.
      metadata_content_signature <-
        revision[["metadata_content_signature"]]

      revision[["metadata_content_signature"]] <- NULL

      revision_values <-
        unlist(
          revision,
          use.names = TRUE
        )

      revision_parts <- character(0)

      if (length(revision_values) > 0L) {

        revision_names <-
          names(revision_values)

        if (is.null(revision_names)) {
          revision_names <-
            rep(
              "",
              length(revision_values)
            )
        }

        revision_parts <-
          vapply(
            seq_along(revision_values),
            function(i) {

              part_name <-
                revision_names[[i]]

              part_value <-
                as.character(
                  revision_values[[i]]
                )

              if (is.null(part_name) ||
                  !nzchar(part_name)) {
                return(part_value)
              }

              paste0(
                part_name,
                "=",
                part_value
              )
            },
            character(1)
          )
      }

      metadata_hash <-
        if (is.null(metadata_content_signature) ||
            length(metadata_content_signature) == 0L ||
            is.na(metadata_content_signature[[1L]]) ||
            !nzchar(as.character(metadata_content_signature[[1L]]))) {

          "none"

        } else {

          substr(
            digest::digest(
              as.character(metadata_content_signature[[1L]]),
              algo = "xxhash64"
            ),
            1L,
            8L
          )
        }

      revision_parts <- c(
        revision_parts,
        paste0(
          "metadata_content_hash=",
          metadata_hash
        )
      )

      paste(
        revision_parts,
        collapse = ";"
      )
    }

    # Full column-name signature logging is intentionally forbidden because
    # datasets may have many columns and sensitive headers.
    format_column_signature_compact <- function(signature) {
      if (is.null(signature)) {
        return("rev=none | cols=0 | hash=none")
      }
      columns <- extract_signature_columns(signature)
      column_hash <- if (length(columns) == 0) {
        "none"
      } else {
        substr(digest::digest(columns, algo = "md5"), 1, 8)
      }
      paste0(
        "rev=", format_revision_signature(signature),
        " | cols=", length(columns),
        " | hash=", column_hash
      )
    }


    summarize_signature_delta <- function(previous_signature, current_signature,
                                          max_names = signature_delta_preview_limit) {
      previous_columns <- extract_signature_columns(previous_signature)
      current_columns <- extract_signature_columns(current_signature)
      added <- setdiff(current_columns, previous_columns)
      removed <- setdiff(previous_columns, current_columns)
      preview <- function(values) {
        if (length(values) == 0) {
          return("none")
        }
        if (length(values) <= max_names) {
          return(paste(values, collapse = ", "))
        }
        paste0(paste(values[seq_len(max_names)], collapse = ", "), ", +", length(values) - max_names, " more")
      }
      paste0(
        "added=", length(added), " [", preview(added), "]",
        " | removed=", length(removed), " [", preview(removed), "]"
      )
    }

    get_operation_missing_columns <- function(operations, available_columns) {
      if (is.null(operations) || nrow(operations) == 0) {
        return(data.frame(
          Row = integer(),
          MissingColumns = character(),
          stringsAsFactors = FALSE
        ))
      }

      missing_rows <- list()

      for (i in seq_len(nrow(operations))) {
        columns_str <- operations$Columns[i]
        if (is.na(columns_str) || !nzchar(columns_str)) {
          missing_columns <- "<none specified>"
        } else {
          operation_columns <- strsplit(columns_str, "\\|")[[1]]
          missing_columns <- setdiff(operation_columns, available_columns)
        }

        if (length(missing_columns) > 0) {
          missing_rows[[length(missing_rows) + 1]] <- data.frame(
            Row = i,
            MissingColumns = paste(missing_columns, collapse = ", "),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(missing_rows) == 0) {
        return(data.frame(
          Row = integer(),
          MissingColumns = character(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, missing_rows)
    }

    flag_pending_operations_missing_columns <- function(available_columns) {
      current_ops <- pending_operations()
      missing_info <- get_operation_missing_columns(current_ops, available_columns)
      pending_row_indices <- which(!current_ops$Executed)
      missing_info <- missing_info[missing_info$Row %in% pending_row_indices, , drop = FALSE]

      if (nrow(missing_info) == 0) {
        return(missing_info)
      }

      for (i in seq_len(nrow(missing_info))) {
        row_index <- missing_info$Row[i]
        missing_columns <- missing_info$MissingColumns[i]
        invalid_note <- paste0("[Invalid: missing columns: ", missing_columns, "]")

        if (!grepl("\\[Invalid: missing columns:", current_ops$Description[row_index])) {
          current_ops$Description[row_index] <- paste(current_ops$Description[row_index], invalid_note)
        }
      }

      pending_operations(current_ops)
      debug_log(
        paste0(
          "Flagged ", nrow(missing_info),
          " pending operation(s) with missing columns: rows ",
          paste(missing_info$Row, collapse = ", ")
        ),
        1
      )
      missing_info
    }

    block_if_pending_operations_have_missing_columns <- function(operations, available_columns, notify = TRUE) {
      if (is.null(operations) || nrow(operations) == 0) {
        return(FALSE)
      }

      pending_row_indices <- which(!operations$Executed)
      if (length(pending_row_indices) == 0) {
        return(FALSE)
      }

      missing_info <- get_operation_missing_columns(operations, available_columns)
      missing_info <- missing_info[missing_info$Row %in% pending_row_indices, , drop = FALSE]

      if (nrow(missing_info) == 0) {
        return(FALSE)
      }

      flag_pending_operations_missing_columns(available_columns)

      details <- paste(
        paste0("operation ", missing_info$Row, " missing: ", missing_info$MissingColumns),
        collapse = "; "
      )
      message <- paste(
        "Cannot apply operations because queued operations reference missing columns.",
        details
      )
      debug_log(message, 1)

      if (notify) {
        showNotification(message, type = "error", duration = 6)
      }

      TRUE
    }

    update_column_select_choices <- function(current_data = NULL, selected = character(0),
                                             log_context = "column choices") {
      metadata <- isolate(metadata_def())
      category <- isolate(input$category_select)
      if (is.null(current_data)) {
        current_data <- tryCatch({ isolate(get_data()) }, error = function(e) NULL)
      }

      if (is.null(current_data)) {
        # When primary data is not loaded, avoid emitting a startup column-select
        # update/log. The input will be rebuilt once data exists or the user
        # interacts with the Edit UI.
        return(character(0))
      }

      if (is.null(category) || category == "" || is.null(metadata)) {
        updateSelectInput(session, "column_select", choices = character(0), selected = character(0))
        debug_log(paste("Updated column_select to empty choices for", log_context), 2)
        return(character(0))
      }

      category_rows <- which(metadata$Content == category)
      if (length(category_rows) == 0) {
        updateSelectInput(session, "column_select", choices = character(0), selected = character(0))
        debug_log(paste("Updated column_select to empty choices for", log_context, "- no category columns"), 2)
        return(character(0))
      }

      category_columns <- metadata$Column[category_rows]
      existing_columns <- intersect(category_columns, names(current_data))
      valid_selected <- intersect(selected, existing_columns)

      if (length(existing_columns) == 0) {
        updateSelectInput(session, "column_select", choices = character(0), selected = character(0))
        debug_log(paste("Updated column_select to empty choices for", log_context, "- no matching data columns"), 2)
        return(character(0))
      }

      choices <- setNames(existing_columns, existing_columns)
      updateSelectInput(session, "column_select", choices = choices, selected = valid_selected)
      debug_log(paste("Updated column_select choices for", log_context, "with", length(existing_columns), "valid columns"), 2)
      existing_columns
    }

    emit_operation_row_log <- function(operation_type, data_type, selected_columns, parameters,
                                       description) {
      current_ops <- pending_operations()
      row_index <- if (is.data.frame(current_ops)) nrow(current_ops) else NA_integer_

      params_clean <- parameters
      keep_param <- vapply(params_clean, function(v) {
        if (is.null(v)) return(FALSE)
        if (is.character(v) && !nzchar(v)) return(FALSE)
        TRUE
      }, logical(1))
      params_clean <- params_clean[keep_param]

      params_str <- if (length(params_clean) == 0) {
        "none"
      } else {
        paste(
          vapply(names(params_clean), function(k) {
            paste0(k, ": ", as.character(params_clean[[k]]))
          }, character(1)),
          collapse = "; "
        )
      }

      debug_log(
        sprintf(
          paste0(
            "Queue row: %s",
            " | Operation: %s",
            " | Column type: %s",
            " | Columns (%d): [%s]",
            " | Parameters: %s",
            " | Description: %s"
          ),
          ifelse(is.na(row_index), "NA", as.character(row_index)),
          as.character(operation_type),
          as.character(data_type),
          length(selected_columns),
          paste(selected_columns, collapse = ", "),
          params_str,
          as.character(description)
        ),
        level = 0
      )
    }

    # ==================================================================
    # Data Integrity Management
    # ==================================================================

    # Initialize original data backup with integrity check
    observe({
      if (!isTRUE(edit_data_ready())) {
        return(invisible(NULL))
      }
      current_data <- isolate(get_data())

      tryCatch({
        if (!is.null(current_data) && is.null(original_data())) {
          data_hash <- digest::digest(current_data, algo = "md5")
          original_data(current_data)
          original_data_hash(data_hash)
          debug_log(paste("Original data backed up - dimensions:",
                          nrow(current_data), "x", ncol(current_data),
                          "hash:", substr(data_hash, 1, 8)), 1)
        }
      }, error = function(e) {
        debug_log(paste("Error backing up original data:", e$message), 1)
      })
    })

    # Monitor data changes and update original backup if structure changes
    observe({
      if (!isTRUE(edit_data_ready())) {
        return(invisible(NULL))
      }
      current_data <- isolate(get_data())

      tryCatch({
        if (!is.null(current_data) && !is.null(original_data())) {
          current_hash <- digest::digest(current_data, algo = "md5")
          original_hash <- original_data_hash()

          if (!is.null(original_hash) && current_hash != original_hash) {
            original <- original_data()
            if (!is.null(original) &&
                (ncol(current_data) != ncol(original) ||
                 !identical(names(current_data), names(original)))) {
              debug_log("Data structure changed, updating original backup", 2)
              original_data(current_data)
              original_data_hash(current_hash)
            }
          }
        }
      }, error = function(e) {
        debug_log(paste("Error monitoring data changes:", e$message), 1)
      })
    })

    # Track data column signature and invalidate stale edit selections/cache
    observe({
      if (!isTRUE(edit_data_ready())) {
        return(invisible(NULL))
      }
      current_data <- isolate(get_data())

      tryCatch({
        current_signature <- get_data_column_signature(current_data)
        previous_signature <- current_data_column_signature()

        if (is.null(previous_signature) && is.null(current_signature)) {
          return()
        }

        if (is.null(previous_signature) && !is.null(current_signature)) {
          init_log_prefix <- "Initialized edit data column signature"
          current_data_column_signature(current_signature)
          update_column_select_choices(
            current_data = current_data,
            selected = character(0),
            log_context = "initial data column signature"
          )
          debug_log(paste0(init_log_prefix, " | ", format_column_signature_compact(current_signature)), 2)
          return()
        }

        if (identical(previous_signature, current_signature)) {
          return()
        }

        previous_columns <- extract_signature_columns(previous_signature)
        current_columns <- extract_signature_columns(current_signature)

        if (identical(previous_columns, current_columns)) {
          current_data_column_signature(current_signature)

          column_hash <- if (length(current_columns) == 0) {
            "none"
          } else {
            substr(digest::digest(current_columns, algo = "md5"), 1, 8)
          }
          debug_log(
            paste0(
              "Edit data revision changed without column changes",
              " | previous_rev=", format_revision_signature(previous_signature),
              " | current_rev=", format_revision_signature(current_signature),
              " | cols=", length(current_columns),
              " | hash=", column_hash
            ),
            2
          )
          return()
        }

        selected_columns <- if (is.null(input$column_select)) character(0) else input$column_select
        available_columns <- current_columns
        stale_columns <- setdiff(selected_columns, available_columns)
        debug_log(
          paste0(
            "Edit column signature changed | previous: ", format_column_signature_compact(previous_signature),
            " | current: ", format_column_signature_compact(current_signature),
            " | ", summarize_signature_delta(previous_signature, current_signature),
            " | invalidated_selections=", length(stale_columns)
          ),
          1
        )
        current_data_column_signature(current_signature)
        column_type_cache(list())
        selected_columns_info(create_default_columns_info())

        if (length(stale_columns) > 0) {
          debug_log(
            paste(
              "Invalidated stale edit column selection(s):",
              paste(stale_columns, collapse = ", ")
            ),
            1
          )
        }

        valid_selected <- intersect(selected_columns, available_columns)
        update_column_select_choices(
          current_data = current_data,
          selected = valid_selected,
          log_context = "data column signature change"
        )

        if (length(available_columns) > 0) {
          missing_info <- flag_pending_operations_missing_columns(available_columns)
          if (nrow(missing_info) > 0) {
            debug_log(
              paste(
                "Invalidated stale queued edit operation column reference(s):",
                paste(
                  paste0("row ", missing_info$Row, " -> ", missing_info$MissingColumns),
                  collapse = "; "
                )
              ),
              1
            )
          }
        }
      }, error = function(e) {
        debug_log(paste("Error tracking data column signature changes:", e$message), 1)
      })
    })

    # ==================================================================
    # Operations Table Import (template / external reactive)
    # ==================================================================

    observeEvent(operations_table(), {
      # Circular dependency prevention
      if (operations_table_update_in_progress()) {
        debug_log("Operations table update already in progress, skipping", 2)
        return()
      }

      operations_config <- operations_table()

      if (is.null(operations_config)) {
        debug_log("Operations table is NULL, no operations to import", 2)
        return()
      }

      tryCatch({
        operations_table_update_in_progress(TRUE)

        debug_log("Processing operations table import", 1)

        if (!is.data.frame(operations_config)) {
          error_msg <- "Operations table is not a data frame"
          operations_table_errors(append(operations_table_errors(), error_msg))
          debug_log(error_msg, 1)
          return()
        }

        if (nrow(operations_config) == 0) {
          debug_log("Empty operations table imported", 2)
          return()
        }

        debug_log(paste("Importing operations table with", nrow(operations_config), "operations"), 1)

        current_data <- get_data()
        available_columns <- if (!is.null(current_data)) names(current_data) else character(0)

        validation_result <- validate_operations_table(
          operations_config, available_columns, reset_executed = TRUE, debug_log = debug_log
        )

        if (validation_result$success) {
          pending_operations(validation_result$operations)
          operations_table_applied(TRUE)
          operations_table_source_info("template_import")

          if (length(validation_result$warnings) > 0) {
            critical_warnings <- grep("removed", validation_result$warnings, value = TRUE)
            operations_table_errors(validation_result$warnings)

            if (length(critical_warnings) > 0) {
              showNotification(
                paste("Loaded", nrow(validation_result$operations), "operations.",
                      validation_result$removed_count, "operations removed due to validation issues."),
                type = "warning", duration = 4
              )
            }
          } else {
            if (nrow(validation_result$operations) > 5) {
              showNotification(
                paste("Loaded", nrow(validation_result$operations), "operations from template."),
                type = "message", duration = 3
              )
            }
          }

          debug_log("Operations table imported successfully", 1)
        } else {
          error_msg <- "Failed to validate operations table structure"
          operations_table_errors(append(operations_table_errors(), error_msg))
          debug_log(error_msg, 1)
          showNotification("Failed to import operations table: invalid structure", type = "error")
        }

      }, error = function(e) {
        error_msg <- paste("Error importing operations table:", e$message)
        debug_log(error_msg, 1)
        operations_table_errors(append(operations_table_errors(), error_msg))
        showNotification(paste("Error importing operations table:", e$message), type = "error")
      }, finally = {
        operations_table_update_in_progress(FALSE)
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==================================================================
    # Column Type Detection with Caching
    # ==================================================================

    observeEvent(input$column_select, {
      tryCatch({
        current_data <- get_data()

        if (is.null(input$column_select) || length(input$column_select) == 0 || is.null(current_data)) {
          selected_columns_info(list(
            overall_type = "unknown",
            individual_types = character(0),
            type_summary = "No columns selected",
            compatible = FALSE,
            existing_columns = character(0),
            last_updated = Sys.time()
          ))
          return()
        }

        missing_selected_columns <- setdiff(input$column_select, names(current_data))
        if (length(missing_selected_columns) > 0) {
          debug_log(
            paste(
              "Invalidated stale edit selection(s) during type detection:",
              paste(missing_selected_columns, collapse = ", ")
            ),
            1
          )
          selected_columns_info(create_default_columns_info())
          update_column_select_choices(
            current_data = current_data,
            selected = intersect(input$column_select, names(current_data)),
            log_context = "stale type detection selection"
          )
          return()
        }

        # Check cache first
        cache_key <- paste(c(unlist(tryCatch(data_revision_signature(), error = function(e) NULL), use.names = TRUE), sort(input$column_select)), collapse = "|")
        cached_info <- column_type_cache()[[cache_key]]

        if (!is.null(cached_info) &&
            !is.null(cached_info$last_updated) &&
            difftime(Sys.time(), cached_info$last_updated, units = "secs") < 30) {
          debug_log("Using cached column type information", 2)
          selected_columns_info(cached_info)
          return()
        }

        # Detect types for all selected columns
        column_info <- detect_multi_column_types(current_data, input$column_select, debug_log)
        column_info$last_updated <- Sys.time()

        # Update cache (limit to 10 entries)
        cache <- column_type_cache()
        cache[[cache_key]] <- column_info
        if (length(cache) > 10) {
          cache <- cache[-(1:5)]
        }
        column_type_cache(cache)

        selected_columns_info(column_info)

      }, error = function(e) {
        debug_log(paste("Error in column type detection:", e$message), 1)
        selected_columns_info(list(
          overall_type = "unknown",
          individual_types = character(0),
          type_summary = "Type detection failed",
          compatible = FALSE,
          existing_columns = character(0),
          last_updated = Sys.time()
        ))
      })
    })

    # ==================================================================
    # Category / Column Dropdown Updates
    # ==================================================================

    edit_category_cache <- reactiveVal(list(signature = NULL, choices = character(0)))

    # Update available categories based on metadata
    observe({
      if (!isTRUE(edit_data_ready())) {
        return(invisible(NULL))
      }
      metadata <- isolate(metadata_def())
      current_data <- isolate(get_data())

      tryCatch({
        if (!is.null(metadata) && nrow(metadata) > 0) {
          category_signature <- paste(unlist(tryCatch(data_revision_signature(), error = function(e) NULL), use.names = TRUE), collapse = "|")
          cached_categories <- edit_category_cache()
          if (!is.null(cached_categories$signature) && identical(cached_categories$signature, category_signature)) {
            available_categories <- cached_categories$choices
          } else {
            available_categories <- unique(metadata$Content)
            available_categories <- available_categories[!is.na(available_categories) & nzchar(available_categories)]
            edit_category_cache(list(signature = category_signature, choices = available_categories))
          }

          if (length(available_categories) > 0) {
            updateSelectInput(
              session,
              "category_select",
              choices = c("Select category..." = "", available_categories),
              selected = ""
            )
            debug_log(paste("Updated categories:", length(available_categories), "available"), 2)
          } else {
            updateSelectInput(
              session,
              "category_select",
              choices = c("No categories available" = ""),
              selected = ""
            )
            debug_log("No categories available in metadata", 2)
          }
        }
      }, error = function(e) {
        debug_log(paste("Error updating categories:", e$message), 1)
        updateSelectInput(session, "category_select", choices = c("Error loading categories" = ""))
      })
    })

    # Update available columns based on selected category
    observeEvent(input$category_select, {
      tryCatch({
        update_column_select_choices(
          current_data = isolate(get_data()),
          selected = character(0),
          log_context = "selected category"
        )
      }, error = function(e) {
        debug_log(paste("Error updating columns:", e$message), 1)
        updateSelectInput(session, "column_select", choices = character(0), selected = character(0))
      })
    })


    # ==================================================================
    # Add Replace Operation
    # ==================================================================

    observeEvent(input$add_replace, {
      tryCatch({
        if (is.null(input$column_select) || length(input$column_select) == 0) {
          showNotification("Please select one or more columns first.", type = "warning")
          return()
        }

        column_info <- selected_columns_info()
        if (!column_info$compatible) {
          showNotification("Selected columns have incompatible types. Please select columns of the same type.", type = "warning")
          return()
        }

        column_type <- column_info$overall_type
        selected_columns <- column_info$existing_columns

        if (column_type == "character") {
          if (is.null(input$string_search_term) || !nzchar(input$string_search_term)) {
            showNotification("Please enter a search term.", type = "warning")
            return()
          }

          replacement_value <- ifelse(is.null(input$string_replacement), "", input$string_replacement)

          parameters <- list(
            search_type = input$string_search_type,
            search_term = input$string_search_term,
            replace_type = input$string_replace_type,
            replacement = replacement_value
          )

          description <- if (input$string_replace_type == "Clear cell") {
            paste0(
              "Clear cells where value ", input$string_search_type, " '", input$string_search_term,
              "' (", input$string_replace_type, ")"
            )
          } else {
            paste0(
              "Replace ", input$string_search_type, " '", input$string_search_term,
              "' with '", replacement_value,
              "' (", input$string_replace_type, ")"
            )
          }

        } else if (column_type == "numeric") {
          if (is.null(input$numeric_threshold)) {
            showNotification("Please enter a threshold value.", type = "warning")
            return()
          }

          replacement_value <- NULL
          if (!is.null(input$numeric_replace_with) && input$numeric_replace_with == "Numeric") {
            if (is.null(input$numeric_replacement_value)) {
              showNotification("Please enter a replacement value.", type = "warning")
              return()
            }
            replacement_value <- input$numeric_replacement_value
          }

          parameters <- list(
            operator = input$numeric_operator,
            threshold = input$numeric_threshold,
            replace_with = input$numeric_replace_with,
            replacement_value = replacement_value
          )

          description <- paste0(
            "Replace values ", input$numeric_operator, " ", input$numeric_threshold,
            " with ", ifelse(input$numeric_replace_with == "NA", "NA", replacement_value)
          )

        } else {
          showNotification("Unknown column type. Cannot add replacement.", type = "error")
          return()
        }

        success <- add_operation_to_queue("Replace", column_type, selected_columns, parameters, description)

        if (success) {
          showNotification("Replace operation added to queue.", type = "message", duration = 2)
          emit_operation_row_log("Replace", column_type, selected_columns, parameters, description)
        } else {
          showNotification("Failed to add replace operation.", type = "error")
        }

      }, error = function(e) {
        debug_log(paste("Error adding replace operation:", e$message), 1)
        showNotification(paste("Error adding replace operation:", e$message), type = "error")
      })
    })

    # ==================================================================
    # Add Edit Operation
    # ==================================================================

    observeEvent(input$add_edit, {
      tryCatch({
        if (is.null(input$column_select) || length(input$column_select) == 0) {
          showNotification("Please select one or more columns first.", type = "warning")
          return()
        }

        column_info <- selected_columns_info()
        if (!column_info$compatible) {
          showNotification("Selected columns have incompatible types. Please select columns of the same type.", type = "warning")
          return()
        }

        column_type <- column_info$overall_type
        selected_columns <- column_info$existing_columns

        if (column_type == "character") {
          if (is.null(input$string_edit_text) || !nzchar(input$string_edit_text)) {
            showNotification("Please enter text to add.", type = "warning")
            return()
          }

          parameters <- list(
            edit_text = input$string_edit_text,
            position = input$string_edit_position
          )

          description <- paste0(
            "Add '", input$string_edit_text, "' ", tolower(input$string_edit_position), " text"
          )

        } else if (column_type == "numeric") {
          operation <- input$numeric_operation
          value <- NULL
          base <- NULL

          if (operation %in% c("Add", "Subtract", "Multiply", "Divide")) {
            if (is.null(input$numeric_operation_value)) {
              showNotification("Please enter an operation value.", type = "warning")
              return()
            }
            value <- input$numeric_operation_value
          } else if (operation == "raise to the power of") {
            if (is.null(input$numeric_exponent)) {
              showNotification("Please enter an exponent value.", type = "warning")
              return()
            }
            value <- input$numeric_exponent
          } else if (operation %in% c("log", "-log")) {
            if (is.null(input$numeric_base) || input$numeric_base <= 0 || input$numeric_base == 1) {
              showNotification("Please enter a valid logarithm base (> 0, \u2260 1).", type = "warning")
              return()
            }
            base <- input$numeric_base
          }

          parameters <- list(
            operation = operation,
            value = value,
            base = base
          )

          if (operation %in% c("log", "-log")) {
            description <- paste0(operation, " (base ", base, ")")
          } else if (operation == "raise to the power of") {
            description <- paste0("Raise to power ", value)
          } else {
            description <- paste0(operation, " ", value)
          }

        } else {
          showNotification("Unknown column type. Cannot add edit.", type = "error")
          return()
        }

        success <- add_operation_to_queue("Edit", column_type, selected_columns, parameters, description)

        if (success) {
          showNotification("Edit operation added to queue.", type = "message", duration = 2)
          emit_operation_row_log("Edit", column_type, selected_columns, parameters, description)
        } else {
          showNotification("Failed to add edit operation.", type = "error")
        }

      }, error = function(e) {
        debug_log(paste("Error adding edit operation:", e$message), 1)
        showNotification(paste("Error adding edit operation:", e$message), type = "error")
      })
    })

    # ==================================================================
    # Apply All Operations (interactive, with progress bar)
    # ==================================================================

    observeEvent(input$apply_all_operations, {
      tryCatch({
        start_time <- Sys.time()

        operations <- pending_operations()

        if (nrow(operations) == 0) {
          showNotification("No operations in queue to apply.", type = "warning")
          return()
        }

        pending_ops <- operations[!operations$Executed, , drop = FALSE]

        if (nrow(pending_ops) == 0) {
          showNotification("All operations have already been executed.", type = "warning")
          return()
        }

        current_data <- get_data()
        if (is.null(current_data)) {
          showNotification("No data available.", type = "error")
          return()
        }

        if (block_if_pending_operations_have_missing_columns(operations, names(current_data), notify = TRUE)) {
          return()
        }

        debug_log(paste("Starting batch application of", nrow(pending_ops), "operations"), 1)

        working_data <- current_data
        applied_count <- 0
        failed_count <- 0

        withProgress(message = 'Applying operations...', value = 0, {
          for (i in seq_len(nrow(pending_ops))) {
            operation <- pending_ops[i, ]

            setProgress(i / nrow(pending_ops), detail = paste('Operation', i, 'of', nrow(pending_ops)))

            op_start_time <- Sys.time()
            result <- apply_single_operation(working_data, operation, debug_log)
            op_duration <- as.numeric(difftime(Sys.time(), op_start_time, units = "secs"))

            if (result$success) {
              working_data <- result$data
              applied_count <- applied_count + 1
              mark_operation_executed(operation)
            } else {
              failed_count <- failed_count + 1
              debug_log(paste("Operation", i, "failed:", result$message), 1)
            }
          }
        })

        # Update data and performance tracking
        total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        if (applied_count > 0) {
          success <- set_data(working_data)
          if (success) {
            last_operation_time(total_duration)
            debug_log(paste("Batch application completed:", applied_count, "successful,", failed_count, "failed"), 1)

            if (failed_count > 0) {
              showNotification(
                paste("Applied", applied_count, "operations,", failed_count, "failed."),
                type = "warning", duration = 3
              )
            } else {
              showNotification(
                paste("Applied", applied_count, "operations successfully."),
                type = "message", duration = 2
              )
            }
          } else {
            debug_log("Failed to update data after operations", 1)
            showNotification("Failed to update data.", type = "error")
          }
        } else {
          debug_log("No operations could be applied", 1)
          showNotification("No operations could be applied.", type = "warning")
        }

      }, error = function(e) {
        error_msg <- paste("Error applying operations:", e$message)
        debug_log(error_msg, 1)
        showNotification(error_msg, type = "error")
      })
    })

    # ==================================================================
    # Clear Operations
    # ==================================================================

    observeEvent(input$clear_operations, {
      tryCatch({
        current_count <- nrow(pending_operations())

        pending_operations(create_empty_operations_df())

        operations_table_applied(FALSE)
        operations_table_source_info("none")
        operations_table_errors(list())

        debug_log(paste("Cleared all operations:", current_count, "operations"), 1)
        showNotification("All operations cleared from queue.", type = "message", duration = 2)

      }, error = function(e) {
        debug_log(paste("Error clearing operations:", e$message), 1)
        showNotification("Error clearing operations.", type = "error")
      })
    })

  }, envir = ctx)
}
