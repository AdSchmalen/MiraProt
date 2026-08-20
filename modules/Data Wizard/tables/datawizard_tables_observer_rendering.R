# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_observer_rendering.R
# Purpose:
#   Provide the tables observer rendering portion of the Data Wizard without changing public behavior.
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

# Observer rendering and event registration for Data Wizard tables.
# This function evaluates in the one context created by the coordinator so all
# observer closures share state and incoming revision reactive identities.
register_tables_rendering <- function(context) {
  with(context, {

  safe_scalar_logical <- function(x, default = FALSE) {
    if (!is.logical(default) || length(default) < 1 || is.na(default[[1]])) {
      default <- FALSE
    } else {
      default <- isTRUE(default[[1]])
    }

    if (!is.logical(x) || length(x) < 1 || is.na(x[[1]])) {
      return(default)
    }

    isTRUE(x[[1]])
  }

  safe_scalar_string <- function(x, default = "") {
    if (length(x) < 1 || is.na(x[[1]])) {
      return(default)
    }

    value <- as.character(x[[1]])
    if (length(value) < 1 || is.na(value[[1]])) default else value[[1]]
  }

  primary_remove_in_progress <- context$primary_remove_in_progress
  additional_remove_in_progress <- context$additional_remove_in_progress
  primary_table_rendered <- context$primary_table_rendered
  additional_table_rendered <- context$additional_table_rendered
  primary_show_full_table <- context$primary_show_full_table
  additional_show_full_table <- context$additional_show_full_table
  table_duplicate_counts <- context$table_duplicate_counts
  count_table_work <- function(operation) {
    count <- (table_duplicate_counts[[operation]] %||% 0L) + 1L
    table_duplicate_counts[[operation]] <- count
    # Level 2 keeps counters disabled at normal logging levels.
    debug_log(sprintf("DW DUPLICATE WORK | module=tables | operation=%s | count=%d", operation, count), 2)
  }

  refresh_primary_table_style <- function(metadata = NULL, source = "metadata update") {
    if (!isTRUE(isolate(primary_table_rendered()))) return(invisible(FALSE))

    if (is.null(metadata)) metadata <- isolate(current_handson_metadata())
    if (!is.data.frame(metadata) || nrow(metadata) < 1L ||
        !all(c("Column", "Content", "Options") %in% names(metadata))) {
      return(invisible(FALSE))
    }

    primary_columns <- tryCatch(names(isolate(primary_data())), error = function(e) NULL)
    if (is.null(primary_columns)) return(invisible(FALSE))

    colors <- create_content_color_mapping(unique(metadata$Content), metadata)
    colors <- colors[intersect(primary_columns, names(colors))]
    session$sendCustomMessage("datawizard-table-style", list(
      id = ns(isolate(primary_table_output_id())),
      columns = unname(names(colors)),
      colors = unname(colors)
    ))
    debug_log(paste("Primary table style refreshed after", source), 2)
    invisible(TRUE)
  }


  record_primary_table_modification <- function(operation, details = "") {
    if (!is.function(record_modification)) return(invisible(FALSE))

    tryCatch({
      record_modification(operation, details)
      TRUE
    }, error = function(e) {
      debug_log(paste("Primary table modification recording failed:", e$message), 1)
      FALSE
    })
  }

  get_current_primary_df <- function() {
    df <- tryCatch({
      if (!is.null(rv) && !is.null(rv$data_mod) && is.data.frame(rv$data_mod)) {
        rv$data_mod
      } else {
        current_from_reactive <- primary_data()
        if (is.data.frame(current_from_reactive)) current_from_reactive else NULL
      }
    }, error = function(e) NULL)
    if (is.data.frame(df)) df else NULL
  }

  get_current_additional_df <- function() {
    df <- tryCatch({
      current_from_reactive <- additional_data()
      if (is.data.frame(current_from_reactive)) {
        current_from_reactive
      } else if (!is.null(rv) && !is.null(rv$data_additional) && is.data.frame(rv$data_additional)) {
        rv$data_additional
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.data.frame(df)) df else NULL
  }

  set_current_additional_df <- function(df) {
    updated <- FALSE
    if (is.function(set_additional_data)) {
      tryCatch({
        set_additional_data(df)
        updated <- TRUE
      }, error = function(e) {})
    }
    if (!updated && !is.null(rv)) {
      rv$data_additional <- df
      updated <- TRUE
    }
    if (!updated) {
      # Fallback when additional_data is a reactiveVal getter/setter.
      tryCatch({
        additional_data(df)
        updated <- TRUE
      }, error = function(e) {})
    }
    updated
  }

  refresh_server_table <- function(role) {
    output_id <- if (identical(role, "primary")) {
      isolate(primary_table_output_id())
    } else {
      isolate(additional_table_output_id())
    }
    tryCatch({
      proxy <- DT::dataTableProxy(output_id, session = session)
      # reloadData leaves the browser's column searches intact and avoids
      # returning the user to the first page after a row removal.
      DT::reloadData(proxy, resetPaging = FALSE, clearSelection = "row")
      TRUE
    }, error = function(e) {
      debug_log(paste(role, "table proxy refresh failed:", e$message), 1)
      FALSE
    })
  }

  get_removable_columns <- function() {
    current_primary_cols <- tryCatch({
      df <- get_current_primary_df()
      if (is.data.frame(df)) names(df) else character(0)
    }, error = function(e) character(0))

    current_primary_cols <- current_primary_cols[!is.na(current_primary_cols) & nzchar(current_primary_cols)]
    current_primary_cols <- setdiff(unique(current_primary_cols), "Row Index")
    if (length(current_primary_cols) == 0) return(character(0))

    metadata_order <- tryCatch({
      meta <- current_handson_metadata()
      if (is.data.frame(meta) && "Column" %in% names(meta)) as.character(meta$Column) else character(0)
    }, error = function(e) character(0))

    metadata_order <- metadata_order[!is.na(metadata_order) & nzchar(metadata_order)]
    metadata_order <- setdiff(unique(metadata_order), "Row Index")
    metadata_order <- metadata_order[metadata_order %in% current_primary_cols]

    unique(c(metadata_order, current_primary_cols))
  }


  get_additional_removable_columns <- function() {
    additional_cols <- tryCatch({
      df <- get_current_additional_df()
      if (is.data.frame(df)) setdiff(names(df), "Row Index") else character(0)
    }, error = function(e) character(0))

    if (length(additional_cols) > 0) return(unique(additional_cols))

    tryCatch({
      df <- additional_data()
      if (is.data.frame(df)) unique(setdiff(names(df), "Row Index")) else character(0)
    }, error = function(e) character(0))
  }

  make_column_signature <- function(df) {
    if (!is.data.frame(df)) return("")
    paste(names(df), collapse = "\r")
  }

  make_column_signature_key <- function(signature, prefix) {
    signature_ints <- utf8ToInt(signature)
    checksum <- if (length(signature_ints) > 0) {
      sum((seq_along(signature_ints) %% 997) * signature_ints) %% 1000000007
    } else {
      0
    }
    paste0(prefix, "_", length(signature_ints), "_", format(checksum, scientific = FALSE, trim = TRUE))
  }

  get_filter_state <- function() {
    tryCatch({
      if (!is.null(filter_applied)) safe_scalar_logical(filter_applied(), FALSE) else FALSE
    }, error = function(e) FALSE)
  }

  get_modified_state <- function(df = NULL) {
    is_modified <- tryCatch({
      if (!is.null(data_modified)) safe_scalar_logical(data_modified(), FALSE) else FALSE
    }, error = function(e) FALSE)
    is_modified <- safe_scalar_logical(is_modified, FALSE)

    if (!is_modified && is.data.frame(df)) {
      modified_prefixes <- c("^Imputed ", "^Batch Corrected ", "^Pivoted ", "^Merged ")
      is_modified <- any(vapply(modified_prefixes, function(prefix) {
        length(grep(prefix, names(df), value = TRUE)) > 0
      }, logical(1)))
    }
    is_modified
  }

  format_table_count <- function(value) {
    format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  primary_table_snapshot <- reactive({
    revision <- primary_working_revision_debounced()
    published_revision <- tryCatch(
      isolate(rv$datawizard_data_revision_id),
      error = function(e) revision
    )
    if (is.null(published_revision) || length(published_revision) != 1L ||
        is.na(published_revision)) {
      published_revision <- revision
    }

    # The state adapter publishes the data and its immediate revision before
    # the debounced revision is released.  Other lightweight table state (for
    # example filter/reset flags) can invalidate this reactive in that window.
    # Cancel that stale evaluation silently and let the debounced revision
    # trigger the one authoritative snapshot.  `req()` retains an existing
    # table during rapid consecutive updates and produces no user-facing error.
    req(as.integer(revision) >= as.integer(published_revision))

    count_table_work("primary_snapshot")
    snapshot_started <- unname(proc.time()[["elapsed"]])
    snapshot_correlation <- tryCatch(isolate(rv$datawizard_upload_correlation_id), error = function(e) "no-upload")
    snapshot_upload_started <- tryCatch(isolate(rv$datawizard_upload_monotonic_started), error = function(e) snapshot_started)
    debug_log(sprintf(paste0("DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=start | ",
                            "phase=table_snapshot_preparation"), snapshot_correlation %||% "no-upload",
                      1000 * (snapshot_started - snapshot_upload_started)), 1)
    # The data frame is published before the debounced revision is released.
    # Reading it reactively here caused an eager revision-0 snapshot followed by
    # the intended revision-1 snapshot.  Besides doing the expensive preview
    # preparation twice, the two overlapping DT responses could make the first
    # server request receive a non-DT response (DataTables tn/1).  The revision
    # is the canonical invalidation contract; read its corresponding frame only
    # after that signal fires.
    df <- isolate(get_current_primary_df())
    snapshot <- build_table_display_snapshot(
      df,
      role = "primary",
      revision = revision,
      show_full = primary_show_full_table(),
      modified = get_modified_state(df),
      filtered = get_filter_state(),
      final = FALSE,
      debug_log = debug_log
    )
    correlation_id <- tryCatch(isolate(rv$datawizard_upload_correlation_id), error = function(e) "no-upload")
    upload_started <- tryCatch(isolate(rv$datawizard_upload_monotonic_started), error = function(e) snapshot_started)
    debug_log(sprintf(paste0(
      "DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=end | phase=table_snapshot_preparation | ",
      "revision=%s | dimensions=%dx%d | visible_dimensions=%dx%d | object_mb=%.3f"),
      correlation_id %||% "no-upload", 1000 * (unname(proc.time()[["elapsed"]]) - upload_started),
      snapshot$revision %||% NA, snapshot$n_rows, snapshot$n_cols, snapshot$slice_rows, snapshot$slice_cols,
      as.numeric(object.size(snapshot$visible_slice)) / (1024^2)), 1)
    snapshot
  })

  additional_table_snapshot <- reactive({
    revision <- secondary_revision_debounced()
    # As with the primary table, the debounced revision is the invalidation
    # contract. Reading the canonical frame in isolation prevents unrelated
    # reactive updates from rebuilding and serializing the complete table.
    df <- isolate(get_current_additional_df())
    build_table_display_snapshot(
      df,
      role = "secondary",
      revision = revision,
      show_full = additional_show_full_table(),
      modified = FALSE,
      filtered = FALSE,
      final = FALSE,
      debug_log = debug_log
    )
  })

  primary_column_signature <- reactive({
    snapshot <- primary_table_snapshot()
    paste(c(snapshot$revision, snapshot$policy, snapshot$full_requested, snapshot$column_names), collapse = "\r")
  })

  additional_column_signature <- reactive({
    snapshot <- additional_table_snapshot()
    paste(c(snapshot$revision, snapshot$policy, snapshot$full_requested, snapshot$column_names), collapse = "\r")
  })

  primary_table_output_id <- reactive({
    datawizard_table_output_id("primary", primary_show_full_table())
  })

  additional_table_output_id <- reactive({
    datawizard_table_output_id("additional", additional_show_full_table())
  })

  primary_serialization_dispatched_at <- context$primary_serialization_dispatched_at

  destroy_table_instances <- function(container_id) {
    shinyjs::runjs(sprintf(
      "(function() {
        var container = document.getElementById('%s');
        if (!container || !window.jQuery || !jQuery.fn || !jQuery.fn.DataTable) return;
        jQuery(container).find('table.dataTable').each(function() {
          if (jQuery.fn.DataTable.isDataTable(this)) {
            jQuery(this).DataTable().clear().destroy(true);
          }
        });
      })();",
      container_id
    ))
  }

  table_rebuild_ui <- function(output_id, container_id) {
    tagList(
      tags$script(HTML(sprintf(
        "(function() {
          var container = document.getElementById('%s');
          if (!container || !window.jQuery || !jQuery.fn || !jQuery.fn.DataTable) return;
          jQuery(container).find('table.dataTable').each(function() {
            if (jQuery.fn.DataTable.isDataTable(this)) {
              jQuery(this).DataTable().clear().destroy(true);
            }
          });
        })();",
        container_id
      ))),
      DTOutput(ns(output_id))
    )
  }

  select_first_valid_column <- function(choices, selected) {
    choices <- choices[!is.na(choices) & nzchar(choices)]
    if (length(selected) == 1 && !is.na(selected) && selected %in% choices) {
      return(selected)
    }
    if (length(choices) > 0) choices[[1]] else character(0)
  }

  observeEvent(primary_column_signature(), {
    primary_cols <- get_removable_columns()
    selected <- select_first_valid_column(primary_cols, input$primary_remove_col)
    freezeReactiveValue(input, "primary_remove_col")
    updateSelectInput(
      session,
      "primary_remove_col",
      choices = primary_cols,
      selected = selected
    )
  }, ignoreInit = FALSE)

  observeEvent(additional_column_signature(), {
    additional_cols <- get_additional_removable_columns()
    selected <- select_first_valid_column(additional_cols, input$additional_remove_col)
    freezeReactiveValue(input, "additional_remove_col")
    updateSelectInput(
      session,
      "additional_remove_col",
      choices = additional_cols,
      selected = selected
    )
  }, ignoreInit = FALSE)



  ensure_unique_preview_names <- function(preview_df, table_label) {
    if (!is.data.frame(preview_df)) return(preview_df)

    preview_names <- names(preview_df)
    needs_name_repair <- is.null(preview_names) ||
      any(is.na(preview_names) | preview_names == "") ||
      anyDuplicated(preview_names) > 0L

    if (!isTRUE(needs_name_repair)) return(preview_df)

    if (is.null(preview_names)) {
      preview_names <- rep("", ncol(preview_df))
    }
    preview_names <- as.character(preview_names)
    missing_names <- is.na(preview_names) | preview_names == ""
    if (any(missing_names)) {
      preview_names[missing_names] <- paste0("Unnamed_", seq_len(sum(missing_names)))
    }

    names(preview_df) <- make.unique(preview_names, sep = "_dup_")
    debug_log(
      paste0(table_label, " preview: repaired non-unique column names before DT rendering."),
      1
    )
    preview_df
  }

  build_name_based_color_mapping <- function(column_names) {
    # Fallback: infer content types from stable column name prefixes while
    # metadata is absent or temporarily stale during reactive synchronization.
    content_mapping <- character(length(column_names))
    names(content_mapping) <- column_names

    for (i in seq_along(column_names)) {
      col_name <- column_names[i]
      if (grepl("^Imputed ", col_name)) {
        original_name <- gsub("^Imputed ", "", col_name)
        if (grepl("Raw", original_name, ignore.case = TRUE)) {
          content_mapping[col_name] <- "Imputed Raw Abundance"
        } else if (grepl("Normalized", original_name, ignore.case = TRUE)) {
          content_mapping[col_name] <- "Imputed Normalized Abundance"
        } else {
          content_mapping[col_name] <- "Imputed Data"
        }
      } else if (col_name == "Row Index") {
        content_mapping[col_name] <- "Row Index"
      } else {
        content_mapping[col_name] <- "Unknown"
      }
    }

    unique_types <- unique(content_mapping)
    type_colors  <- create_content_color_mapping(unique_types, NULL)

    column_color_mapping <- character(length(column_names))
    names(column_color_mapping) <- column_names
    for (col_name in column_names) {
      column_color_mapping[col_name] <- type_colors[content_mapping[col_name]]
    }

    column_color_mapping
  }

  register_tables_metadata_hydration(context)
  mark_programmatic_metadata_sync <- context$mark_programmatic_metadata_sync
  metadata_reference_df <- context$metadata_reference_df


  # --------------------------------------------------------------------------
  # c. output$data_status_indicator
  #    Renders a colored badge showing whether the displayed data is raw,
  #    modified, or filtered. Reads rv directly for the comparison because
  #    primary_data() already reflects filtering/modification.
  # --------------------------------------------------------------------------

  output$data_status_indicator <- renderUI({
    is_filtered <- FALSE
    is_modified <- FALSE

    if (!is.null(filter_applied)) {
      tryCatch({
        is_filtered <- safe_scalar_logical(filter_applied(), FALSE)
      }, error = function(e) {
        is_filtered <- FALSE
      })
    }

    tryCatch({
      if (!is.null(rv)) {
        current_data  <- rv$data_mod
        original_data <- rv$primary_data_raw
        if (!is.null(current_data) && !is.null(original_data)) {
          is_modified <- !identical(current_data, original_data)
        }
      }
    }, error = function(e) {
      is_modified <- FALSE
    })

    is_modified <- safe_scalar_logical(is_modified, FALSE)

    if (is_modified) {
      # Use Flatly theme status colors: info for modified data,
      # warning for filtered raw data, and success for unchanged raw data.
      status_color <- "#3498db"
      status_text  <- if (is_filtered) "MODIFIED DATA" else "MODIFIED DATA"
    } else {
      status_color <- if (is_filtered) "#3498db" else "#18bc9c"
      status_text  <- if (is_filtered) "MODIFIED DATA" else "RAW DATA"
    }

    status_color <- safe_scalar_string(status_color, "#18bc9c")
    status_text <- safe_scalar_string(status_text, "RAW DATA")

    div(
      style = "text-align: left; margin-bottom: 8px;",
      span(
        class = "label",
        style = paste0(
          "font-size: 12px; padding: 4px 8px; ",
          "background-color: ", status_color, "; color: white;"
        ),
        status_text
      )
    )
  })

  register_tables_metadata_sync_rendering(context)

  # --------------------------------------------------------------------------
  # e. output$primary_table_info
  #    Renders a one-line summary of the primary data dimensions and current
  #    modification/filter state.
  # --------------------------------------------------------------------------

  output$primary_table_info <- renderText({
    snapshot <- primary_table_snapshot()
    status <- c(
      if (snapshot$final) "final" else if (snapshot$modified) "modified" else "raw",
      if (snapshot$filtered) "filtered" else "unfiltered",
      if (snapshot$full_requested) "full interaction" else "50-row preview",
      paste0("revision=", snapshot$revision)
    )
    paste0(
      "Primary Data: ", format_table_count(snapshot$n_rows), " rows × ",
      format_table_count(snapshot$n_cols), " columns. ",
      DATAWIZARD_TABLE_PAGE_LENGTH, " rows per page; filters search all ",
      format_table_count(snapshot$n_rows), " rows. ",
      "Status: ", paste(status, collapse = ", "), "."
    )
  })

  # Truncation notices belong to a newly published data snapshot (or an
  # explicit switch between bounded and full-table modes), not to DT's own
  # search/filter redraw requests. Keeping them outside renderDT prevents an
  # interactive filter from repeatedly presenting the same warning.
  observeEvent(
    list(primary_working_revision_debounced(), primary_show_full_table()),
    {
      snapshot <- isolate(primary_table_snapshot())
      if (snapshot$col_truncated) {
        showNotification(
          paste0("Preview mode displays ", snapshot$slice_cols, " of ", snapshot$n_cols,
                 " columns for performance. Server-side filters search all ",
                 format_table_count(snapshot$n_rows), " rows."),
          type = "warning", duration = 5
        )
      }
    },
    ignoreInit = FALSE
  )

  output$primary_table_display_controls <- renderUI({
    snapshot <- primary_table_snapshot()
    if (snapshot$n_rows <= DATAWIZARD_TABLE_PAGE_LENGTH &&
        !snapshot$col_truncated && !snapshot$full_requested) return(NULL)
    tagList(
      tags$div(
        class = "alert alert-info",
        style = "padding: 6px 10px; margin-bottom: 6px;",
        if (snapshot$full_requested) {
          paste0("Full-interaction mode loads all ", snapshot$n_rows,
                 " rows in ", DATAWIZARD_TABLE_PAGE_LENGTH,
                 " rows per page; filters search all ",
                 format_table_count(snapshot$n_rows), " rows.")
        } else {
          paste0(
            "Preview mode displays the first ", snapshot$slice_rows,
            " rows. Filters search all ",
            format_table_count(snapshot$n_rows), " rows."
          )
        }
      ),
      actionButton(
        ns("primary_toggle_full_table"),
        if (snapshot$full_requested) "Return to preview" else "Enable full-table interaction",
        class = "btn-default btn-sm"
      )
    )
  })

  # Switching modes remounts DT under a mode-specific output id. renderDT's
  # `server` argument is captured when its output handler is registered, so a
  # remount is required when switching the independent column display policy.
  # Both modes use the complete row set with server-side DT processing.
  output$primary_table_preview_ui <- renderUI({
    DTOutput(ns(primary_table_output_id()))
  })

  output$additional_table_preview_ui <- renderUI({
    DTOutput(ns(additional_table_output_id()))
  })

  # --------------------------------------------------------------------------
  # e. Keyed primary and additional table previews
  # Column-removal selectors are static UI controls. Their choices are kept in
  # sync by the column-signature observers above so DT rebuilds cannot unmount
  # the row/column removal controls.

  #    Renders the primary data as a DT datatable with content-type based
  #    column background colors. Uses build_data_preview() for preparation and
  #    create_content_color_mapping() for coloring.
  # --------------------------------------------------------------------------

  observeEvent(primary_table_output_id(), {
    local_output_id <- primary_table_output_id()
    output[[local_output_id]] <- renderDT({
      primary_table_rendered(FALSE)
      snapshot <- primary_table_snapshot()
      metadata <- isolate(current_handson_metadata())

      tryCatch({
      if (snapshot$n_cols == 0L) {
        # An empty snapshot is the normal startup state before a file is
        # selected.  Render no widget yet; this is not an application error and
        # must not produce a startup notification.
        return(NULL)
      }

      header_reprocess_active <- !is.null(rv) && (
        isTRUE(rv$datawizard_header_reprocess_active) || isTRUE(rv$header_reprocess_active)
      )

      if (header_reprocess_active) {
        debug_log(
          "Primary table preview: header reprocess active; rendering lightweight update placeholder until data and metadata are synchronized.",
          2
        )
        return(datatable(
          data.frame(Status = "Updating table...", check.names = FALSE),
          rownames = FALSE,
          escape = TRUE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE, paging = FALSE, searching = FALSE, destroy = TRUE),
          filter = "none"
        ))
      }

      preparation_started <- Sys.time()
      # DT must receive the canonical row and column set in both display modes.
      # `ensure_unique_preview_names()` returns this same frame unless legacy
      # data actually requires a defensive name repair, avoiding another full
      # data-frame copy during the normal render path.
      preview_df <- ensure_unique_preview_names(snapshot$complete_frame, "Primary table")
      debug_log(sprintf("TABLE TIMING: server preparation %.3fs",
                        as.numeric(difftime(Sys.time(), preparation_started, units = "secs"))), 1)

      column_names <- names(preview_df)
      metadata_has_column_field <- is.data.frame(metadata) && "Column" %in% names(metadata)
      metadata_columns <- if (metadata_has_column_field) {
        as.character(metadata$Column)
      } else {
        character(0)
      }
      metadata_columns <- metadata_columns[!is.na(metadata_columns)]
      metadata_matches_display <- metadata_has_column_field && identical(metadata_columns, column_names)
      can_style_from_metadata <- !header_reprocess_active && metadata_matches_display &&
        "Content" %in% names(metadata) && "Options" %in% names(metadata) && nrow(metadata) > 0

      if (header_reprocess_active) {
        debug_log(
          "Primary table preview: header reprocess active; rendering without metadata styling until data and metadata are synchronized.",
          2
        )
      } else if (!metadata_matches_display && metadata_has_column_field && nrow(metadata) > 0) {
        metadata_only_columns <- setdiff(metadata_columns, column_names)
        display_only_columns  <- setdiff(column_names, metadata_columns)
        debug_log(
          paste0(
            "Primary table preview: metadata/display column mismatch; ",
            "using name-based fallback styling for this render cycle. ",
            "metadata_columns=", length(metadata_columns), ", ",
            "display_columns=", length(column_names), ", ",
            "metadata_only=",
            if (length(metadata_only_columns) > 0) paste(head(metadata_only_columns, 5), collapse = ", ") else "none",
            if (length(metadata_only_columns) > 5) ", ..." else "",
            "; display_only=",
            if (length(display_only_columns) > 0) paste(head(display_only_columns, 5), collapse = ", ") else "none",
            if (length(display_only_columns) > 5) ", ..." else ""
          ),
          2
        )
      } else if (metadata_matches_display && !can_style_from_metadata) {
        debug_log(
          "Primary table preview: metadata columns match display, but Content/Options fields are unavailable; using name-based fallback styling.",
          2
        )
      }

      if (header_reprocess_active) {
        color_mapping <- character(0)
      } else if (can_style_from_metadata) {
        color_mapping <- create_content_color_mapping(unique(metadata$Content), metadata)
      } else {
        color_mapping <- build_name_based_color_mapping(column_names)
      }

      dt <- DT::datatable(
        preview_df,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "multiple", target = "row"),
        options = build_datawizard_table_options(
          JS(sprintf("function(){var detail={id:'%s',readyAt:performance.now()};document.dispatchEvent(new CustomEvent('datawizard:dt-ready',{detail:detail}));Shiny.setInputValue('%s', detail, {priority:'event'});}", ns(local_output_id), ns("primary_table_browser_ready"))),
          text_renderer = JS(build_datawizard_text_renderer()),
          text_columns = which(vapply(preview_df, function(column) {
            is.character(column) || is.factor(column)
          }, logical(1))) - 1L,
          pagination_callback = JS(build_datawizard_pagination_callback(snapshot$full_requested))
        ),
        callback = JS(build_datawizard_row_identity_callback(
          ns("primary_selected_row_identity"),
          local_output_id,
          match("Row Index", column_names) - 1L
        )),
        filter = "top"
      )

      for (col_name in names(color_mapping)) {
        col_index <- which(column_names == col_name)
        if (length(col_index) > 0) {
          dt <- dt %>%
            formatStyle(
              columns         = col_index,
              backgroundColor = color_mapping[col_name],
              fontWeight      = "normal"
            )
        }
      }

      primary_table_rendered(TRUE)
      debug_log("TABLE TIMING: serialization dispatch queued", 1)
      primary_serialization_dispatched_at(Sys.time())
      return(dt)

    }, error = function(e) {
      primary_table_rendered(FALSE)
      showNotification(paste("Error displaying primary data table:", e$message),
                       type = "error")
      return(NULL)
      })
    # DT owns filtering/paging on the server in every display mode, so top
    # numeric ranges and text searches are evaluated against every row.
    }, server = TRUE)
  }, ignoreInit = FALSE, priority = 100)

  # Metadata edits do not change the DT identity or resend its data. They only
  # invalidate metadata/status outputs; the next data revision applies the
  # latest styling. This avoids remounting a large widget for a color change.
  observeEvent(metadata_content_signature_debounced(), {
    refresh_started <- unname(proc.time()[["elapsed"]])
    count_table_work("downstream_choice_refresh")
    correlation_id <- tryCatch(isolate(rv$datawizard_upload_correlation_id), error = function(e) "no-upload")
    upload_started <- tryCatch(isolate(rv$datawizard_upload_monotonic_started), error = function(e) refresh_started)
    debug_log(sprintf("DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=start | phase=downstream_choice_refresh",
                      correlation_id %||% "no-upload", 1000 * (refresh_started - upload_started)), 1)
    refresh_primary_table_style(source = "metadata content update")
    debug_log("Primary table metadata/style status updated without DT remount", 2)
    debug_log(sprintf(paste0("DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=end | ",
                            "phase=downstream_choice_refresh"), correlation_id %||% "no-upload",
                      1000 * (unname(proc.time()[["elapsed"]]) - upload_started)), 1)
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # f. Additional data section visibility observer
  #    Shows or hides the Secondary Data tab based on data presence.
  # --------------------------------------------------------------------------

  observeEvent(secondary_revision_debounced(), {
    additional_df <- isolate(additional_data())
    has_additional <- !is.null(additional_df) &&
      is.data.frame(additional_df) &&
      nrow(additional_df) > 0

    if (has_additional) {
      shinyjs::show("additional_data_section")
      shinyjs::show(selector = paste0(
        "#", ns("data_viewer_tabs"), " > li:nth-child(2)"
      ))
    } else {
      updateTabsetPanel(session, "data_viewer_tabs", selected = "primary_data")
      shinyjs::hide(selector = paste0(
        "#", ns("data_viewer_tabs"), " > li:nth-child(2)"
      ))
      shinyjs::hide("additional_data_section")
    }
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # g. output$additional_table_info
  #    Renders a one-line summary of the additional data dimensions.
  # --------------------------------------------------------------------------

  output$additional_table_info <- renderText({
    snapshot <- additional_table_snapshot()
    status <- c(
      if (snapshot$final) "final" else if (snapshot$modified) "modified" else "raw",
      if (snapshot$full_requested) "full interaction" else "preview columns",
      paste0("revision=", snapshot$revision)
    )
    paste0(
      "Additional Data: ", format_table_count(snapshot$n_rows), " rows × ",
      format_table_count(snapshot$n_cols), " columns. ",
      DATAWIZARD_TABLE_PAGE_LENGTH, " rows per page; filters search all ",
      format_table_count(snapshot$n_rows), " rows. ",
      "Status: ", paste(status, collapse = ", "), "."
    )
  })

  observeEvent(
    list(secondary_revision_debounced(), additional_show_full_table()),
    {
      snapshot <- isolate(additional_table_snapshot())
      if (snapshot$col_truncated) {
        showNotification(
          paste0("Preview mode displays ", snapshot$slice_cols, " of ", snapshot$n_cols,
                 " columns for performance. Server-side filters search all ",
                 format_table_count(snapshot$n_rows), " rows."),
          type = "warning", duration = 5
        )
      }
    },
    ignoreInit = FALSE
  )

  output$additional_table_display_controls <- renderUI({
    snapshot <- additional_table_snapshot()
    if (snapshot$n_rows <= DATAWIZARD_TABLE_PAGE_LENGTH &&
        !snapshot$col_truncated && !snapshot$full_requested) return(NULL)
    tagList(
      tags$div(
        class = "alert alert-info",
        style = "padding: 6px 10px; margin-bottom: 6px;",
        if (snapshot$full_requested) {
          paste0("Full-interaction mode loads all ", snapshot$n_rows,
                 " rows in ", DATAWIZARD_TABLE_PAGE_LENGTH,
                 " rows per page; filters search all ",
                 format_table_count(snapshot$n_rows), " rows.")
        } else {
          paste0(
            "Preview mode displays the first ", snapshot$slice_rows,
            " rows. Filters search all ",
            format_table_count(snapshot$n_rows), " rows."
          )
        }
      ),
      actionButton(
        ns("additional_toggle_full_table"),
        if (snapshot$full_requested) "Return to preview" else "Enable full-table interaction",
        class = "btn-default btn-sm"
      )
    )
  })

  # --------------------------------------------------------------------------
  # h. output$additional_table_preview
  #    Renders the additional data as a read-only DT datatable.
  # --------------------------------------------------------------------------

  observeEvent(additional_table_output_id(), {
    local_output_id <- additional_table_output_id()
    output[[local_output_id]] <- renderDT({
      additional_table_rendered(FALSE)
      snapshot <- additional_table_snapshot()
      df <- snapshot$complete_frame

      tryCatch({
      if (is.null(df) || ncol(df) == 0) {
        showNotification("No additional data available to preview.", type = "error")
        return(NULL)
      }

      preview_df <- ensure_unique_preview_names(snapshot$complete_frame, "Additional table")

      additional_table_rendered(TRUE)
      DT::datatable(
        preview_df,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "multiple", target = "row"),
        options = build_datawizard_table_options(
          JS(sprintf("function(){var detail={id:'%s',readyAt:performance.now()};document.dispatchEvent(new CustomEvent('datawizard:dt-ready',{detail:detail}));Shiny.setInputValue('%s', detail, {priority:'event'});}", ns(local_output_id), ns("additional_table_browser_ready"))),
          text_renderer = JS(build_datawizard_text_renderer()),
          text_columns = which(vapply(preview_df, function(column) {
            is.character(column) || is.factor(column)
          }, logical(1))) - 1L,
          pagination_callback = JS(build_datawizard_pagination_callback(snapshot$full_requested))
        ),
        callback = JS(build_datawizard_row_identity_callback(
          ns("additional_selected_row_identity"),
          local_output_id,
          match("Row Index", names(preview_df)) - 1L
        )),
        filter = "top"
      )

    }, error = function(e) {
      additional_table_rendered(FALSE)
      showNotification(paste("Error displaying additional data table:", e$message),
                       type = "error")
      return(NULL)
      })
    # The snapshot always supplies all canonical rows; server-side DT therefore
    # applies numeric and text filters to the complete additional dataset.
    }, server = TRUE)
  }, ignoreInit = FALSE, priority = 100)

  observeEvent(input$primary_table_browser_ready, {
    dispatched <- isolate(primary_serialization_dispatched_at())
    suffix <- if (!is.null(dispatched)) sprintf(" %.3fs after dispatch",
      as.numeric(difftime(Sys.time(), dispatched, units = "secs"))) else ""
    debug_log(paste0("TABLE TIMING: browser-ready acknowledgment (primary)", suffix), 1)
    if (!is.null(rv)) rv$datawizard_primary_table_client_ready <- input$primary_table_browser_ready
    if (!is.null(rv)) {
      import_started <- tryCatch(rv$datawizard_import_started_at, error = function(e) NULL)
      already_logged <- tryCatch(isTRUE(rv$datawizard_first_table_ready_logged), error = function(e) FALSE)
      if (!is.null(import_started) && !already_logged) {
        debug_log(sprintf("IMPORT TIMING: first-table readiness %.3fs",
                          as.numeric(difftime(Sys.time(), import_started, units = "secs"))), 1)
        rv$datawizard_first_table_ready_logged <- TRUE
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$additional_table_browser_ready, {
    debug_log("TABLE TIMING: browser-ready acknowledgment (additional)", 1)
    if (!is.null(rv)) rv$datawizard_additional_table_client_ready <- input$additional_table_browser_ready
  }, ignoreInit = TRUE)

  observeEvent(input$primary_toggle_full_table, {
    primary_show_full_table(!isTRUE(primary_show_full_table()))
  }, ignoreInit = TRUE)

  observeEvent(input$additional_toggle_full_table, {
    additional_show_full_table(!isTRUE(additional_show_full_table()))
  }, ignoreInit = TRUE)

  register_tables_mutation_observers(
    context = context,
    get_current_primary_df = get_current_primary_df,
    get_current_additional_df = get_current_additional_df,
    set_current_additional_df = set_current_additional_df,
    refresh_server_table = refresh_server_table,
    record_primary_table_modification = record_primary_table_modification,
    mark_programmatic_metadata_sync = mark_programmatic_metadata_sync,
    primary_table_output_id = primary_table_output_id,
    additional_table_output_id = additional_table_output_id
  )

  tables_api <- register_tables_metadata_editing(
    context,
    refresh_primary_table_style = refresh_primary_table_style
  )
  tables_api$refresh_primary_table_style <- refresh_primary_table_style
  tables_api

  })
}
