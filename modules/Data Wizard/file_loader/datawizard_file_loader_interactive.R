# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_interactive.R
# Purpose:
#   Provide the file loader interactive portion of the Data Wizard without changing public behavior.
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
register_datawizard_file_loader_interactive <- function(loader_environment = parent.frame()) {
  evalq({
    output$show_primary_header_row <- renderText({
      if (isTRUE(has_loaded_primary_data())) "true" else "false"
    })
    output$show_secondary_header_row <- renderText({
      if (isTRUE(has_loaded_secondary_data())) "true" else "false"
    })
    outputOptions(output, "show_primary_header_row", suspendWhenHidden = FALSE)
    outputOptions(output, "show_secondary_header_row", suspendWhenHidden = FALSE)
    output$sheetDropdown <- renderUI({
      tryCatch({
        selected_primary <- selected_sheet_primary() %||% input$sheetDropdown
        # Post-restore/session-RDS-upload: no live Excel workbook but we have
        # saved workbook metadata with sheet labels.
        if (!is_live_excel_upload(input$file)) {
          cached <- sheet_cache_primary()
          meta   <- primary_file_meta()
          choices <- get_sheet_choices_from_state(cached, meta)
          if (length(choices) > 0 && isTRUE(is_excel_file_meta(meta))) {
            if (is.null(selected_primary) || !selected_primary %in% choices) {
              selected_primary <- choices[1]
            }
            return(selectInput(ns("sheetDropdown"), "Select Sheet",
                               choices = choices,
                               selected = selected_primary))
          }
          return(NULL)
        }
        ext <- tolower(tools::file_ext(input$file$name))
        if (ext %in% c("xlsx", "xls")) {
          sheets <- tryCatch({
            readxl::excel_sheets(input$file$datapath)
          }, error = function(e) {
            debug_log(paste("Error reading Excel sheets:", e$message), 1)
            return(c("Sheet1"))
          })

          # UI in Variable speichern
          if (is.null(selected_primary) || !selected_primary %in% sheets) {
            selected_primary <- sheets[1]
          }
          ui <- selectInput(ns("sheetDropdown"), "Select Sheet",
                            choices = sheets, selected = selected_primary)

          # Danach Tooltip setzen
          session$sendCustomMessage('setSelectizeTitle', list(
            inputId = ns("sheetDropdown"),
            label   = sheets[1]
          ))

          # UI returnen
          return(ui)
        } else {
          NULL
        }
      }, error = function(e) {
        debug_log(paste("Error rendering sheet dropdown:", e$message), 1)
        return(NULL)
      })
    })


    output$sheetDropdown2 <- renderUI({
      tryCatch({
        selected_secondary <- selected_sheet_secondary() %||% input$sheetDropdown2
        # Post-restore/session-RDS-upload: no live Excel workbook but we have
        # saved workbook metadata with sheet labels.
        if (!is_live_excel_upload(input$file2)) {
          cached2 <- sheet_cache_secondary()
          meta2   <- secondary_file_meta()
          choices2 <- get_sheet_choices_from_state(cached2, meta2)
          if (length(choices2) > 0 && isTRUE(is_excel_file_meta(meta2))) {
            if (is.null(selected_secondary) || !selected_secondary %in% choices2) {
              selected_secondary <- choices2[1]
            }
            return(selectInput(ns("sheetDropdown2"), "Select Sheet",
                               choices = choices2,
                               selected = selected_secondary))
          }
          return(NULL)
        }
        ext2 <- tolower(tools::file_ext(input$file2$name))
        if (ext2 %in% c("xlsx", "xls")) {
          sheets2 <- tryCatch({
            readxl::excel_sheets(input$file2$datapath)
          }, error = function(e) {
            debug_log(paste("Error reading Excel sheets for additional file:", e$message), 1)
            return(c("Sheet1"))
          })

          if (is.null(selected_secondary) || !selected_secondary %in% sheets2) {
            selected_secondary <- sheets2[1]
          }
          ui <- selectInput(ns("sheetDropdown2"), "Select Sheet",
                            choices = sheets2, selected = selected_secondary)

          session$sendCustomMessage('setSelectizeTitle', list(
            inputId = ns("sheetDropdown2"),
            label   = sheets2[1]
          ))

          return(ui)   # <--- ganz wichtig
        } else {
          NULL
        }
      }, error = function(e) {
        debug_log(paste("Error rendering additional sheet dropdown:", e$message), 1)
        return(NULL)
      })
    })


    # ========================================
    # Module Initialization with Enhanced Logging
    # ========================================

    # Initialize the module after ensuring all inputs are available
    observe({
      # Check if all necessary inputs are available
      inputs_ready <- !is.null(input$header_row) && !is.null(input$header_row2)

      if (inputs_ready && !module_initialized()) {
        module_initialized(TRUE)
        debug_log("Module initialization completed - all header row inputs ready", 2)
      } else if (!module_initialized()) {
        # Re-check after a delay
        invalidateLater(100)
        debug_log("Waiting for inputs to initialize...", 2)
      }
    })
    # Primary file upload with comprehensive reset
    observeEvent(input$file, {
      if (isTRUE(restore_observer_guard_active("Primary file upload observer"))) {
        return()
      }

      import_started_at <- Sys.time()
      upload_telemetry$id <- new_upload_correlation_id()
      upload_telemetry$started <- telemetry_now()
      upload_telemetry$counters <- new.env(parent = emptyenv())
      if (!is.null(rv)) {
        rv$datawizard_upload_correlation_id <- upload_telemetry$id
        rv$datawizard_upload_monotonic_started <- upload_telemetry$started
      }
      telemetry_log("start", "upload")
      tryCatch({
        file_input <- input$file

        if (is.null(file_input) || is.null(file_input$datapath) || !file.exists(file_input$datapath)) {
          debug_log("No valid file input provided", 2)
          return()
        }

        if (!is_supported_datawizard_upload_dw(file_input)) {
          debug_log(paste("Unsupported primary file upload:", file_input$name), 1)
          showNotification(datawizard_unsupported_upload_message_dw, type = "error", duration = 8)
          return()
        }

        ext <- tolower(tools::file_ext(file_input$name %||% file_input$datapath %||% ""))
        if (identical(ext, "rds")) {
          loading_active(TRUE)
          current_operation("Restoring session file")
          handoff_session_restore_upload(file_input, "primary")
          return()
        }

        set_loader_mode("interactive_load", "primary file upload")

        # When new data is loaded, reactivate metadata observer only after the
        # upload extension has been accepted.
        if (!is.null(core_values)) {
          core_values$metadata_observer_active(TRUE)
          debug_log("Metadata observer reactivated for new data", level = 1)
        }

        debug_log(paste("Starting primary file upload:", private_label(file_input$name)), 1)
        primary_data_state$begin_import_generation()
        primary_data_state$set_import_phase("reading", import_started_at, "pre-read reset")
        if (!is.null(rv)) {
          rv$datawizard_import_started_at <- import_started_at
          rv$datawizard_first_table_ready_logged <- FALSE
        }
        loading_active(TRUE)
        current_operation("Loading primary file")

        sheet_names <- get_excel_sheet_names(file_input)
        primary_file_meta(make_excel_file_meta(file_input, sheet_names))
        sheet_cache_primary(new_sheet_cache(sheet_names))
        first_sheet <- if (length(sheet_names) > 0) sheet_names[[1]] else NULL
        selected_sheet_primary(first_sheet)
        updateNumericInput(session, "header_row", value = 1L)
        if (!is.null(first_sheet)) {
          skip_next_sheet_change_primary(first_sheet)
          updateSelectInput(session, "sheetDropdown", selected = first_sheet)
        }
        skip_next_programmatic_header_update_primary(TRUE)
        debug_log(paste("Primary file upload reset sheet/header to:",
                        first_sheet %||% "<non-excel>", "/ 1"), 1)

        # Step 1: Reset rv values BEFORE loading new data
        if (!is.null(rv)) {
          telemetry_log("start", "reset")
          reset_started_at <- Sys.time()
          reset_success <- reset_rv_for_primary_data(rv, preserve_additional = TRUE, debug_level = DEBUG_LEVEL)
          debug_log(sprintf("IMPORT TIMING: reset %.3fs", as.numeric(difftime(Sys.time(), reset_started_at, units = "secs"))), 1)
          telemetry_log("end", "reset", paste0("success=", isTRUE(reset_success)))
          if (reset_success) {
            debug_log("rv successfully reset for new primary data", 1)
          } else {
            debug_log("Warning: rv reset encountered issues", 1)
            showNotification("Warning: Data reset encountered issues. Some values may not be cleared.",
                             type = "warning", duration = 5)
          }
        }

        # Step 2: Load the new file
        parsing_started_at <- Sys.time()
        result <- load_file_enhanced(
          file_input,
          sheet_name = first_sheet,
          header_flag = TRUE,
          operation_name = "primary file loading",
          use_file_cache = is.null(first_sheet)
        )
        debug_log(sprintf("IMPORT TIMING: parsing %.3fs", as.numeric(difftime(Sys.time(), parsing_started_at, units = "secs"))), 1)

        if (safe_is_true(result)) {
          telemetry_log("start", "normalization", data_metrics(result$data), 1)
          # The recovery loader returns the canonical normalized frame for a
          # new upload. This marker separates that completed boundary from
          # publication without adding a second normalization pass.
          telemetry_log("end", "normalization", data_metrics(result$data), 1)
          publication_started_at <- Sys.time()
          primary_data_state$set_import_phase("publishing_raw")
          # Step 3: Set the new data which will trigger metadata recreation.
          # Publish the same loaded frame to all primary raw-data holders before
          # the next reactive flush. The programmatic header-update skip below only
          # blocks duplicate header reprocessing after resetting header_row; it must not
          # keep the initial upload from reaching loader_out$primary,
          # core_values$primary_data_raw, or rv$data_mod.
          data_fixed(result$data)
          data_primary(result$data)

          primary_data_state$set_raw_imported_data(result$data, paste("file upload:", private_label(file_input$name)))
          debug_log(sprintf("IMPORT TIMING: publication %.3fs", as.numeric(difftime(Sys.time(), publication_started_at, units = "secs"))), 1)

          # Construct and publish placeholder metadata before opening the
          # downstream barrier. This keeps registry roles, canonical values,
          # legacy mirrors and their revisions in one observable snapshot.
          metadata_started_at <- Sys.time()
          telemetry_log("start", "placeholder_metadata_creation", data_metrics(result$data))
          primary_data_state$set_import_phase("creating_metadata")
          data_cols <- names(result$data)
          placeholder_metadata <- data.frame(
            Column = data_cols,
            Content = rep(NA_character_, length(data_cols)),
            Options = rep(NA_character_, length(data_cols)),
            Numerator = rep(NA_character_, length(data_cols)),
            Denominator = rep(NA_character_, length(data_cols)),
            Transformation = rep(NA_character_, length(data_cols)),
            Sample = rep(NA_character_, length(data_cols)),
            stringsAsFactors = FALSE
          )
          row_index <- which(data_cols == "Row Index")
          if (length(row_index) == 0L && length(data_cols) > 0L &&
              grepl("Row|Index|ID", data_cols[[1]], ignore.case = TRUE)) row_index <- 1L
          if (length(row_index) > 0L) placeholder_metadata$Content[row_index[[1]]] <- "Row Index"
          primary_data_state$set_metadata_for_current_data(placeholder_metadata)
          if (is.function(core_values$metadata_lifecycle_state)) core_values$metadata_lifecycle_state("metadata_placeholder")
          if (!is.null(rv)) rv$datawizard_metadata_lifecycle_state <- "metadata_placeholder"
          debug_log(sprintf("IMPORT TIMING: metadata creation %.3fs", as.numeric(difftime(Sys.time(), metadata_started_at, units = "secs"))), 1)
          telemetry_log("end", "placeholder_metadata_creation", sprintf("metadata_dimensions=%dx%d", nrow(placeholder_metadata), ncol(placeholder_metadata)))

          # Snapshot the original raw data (first load wins) and lazily cache
          # only the selected Excel sheet. Other sheets load on demand.
          primary_data_original(result$data)
          if (!is.null(first_sheet)) {
            cache_loaded_sheet(sheet_cache_primary, first_sheet, result$data, "Primary upload")
            update_workbook_manifest(primary_file_meta, first_sheet, 1L, sheet_cache_primary())
          }

          debug_log("FILE UPLOAD: Updated primary data holders through state adapter", level = 1)

          debug_log(
            sprintf(
              "File load summary | Scope: Primary | Action: File loaded | File name: %s | Dimensions: %d x %d",
              private_label(file_input$name), nrow(result$data), ncol(result$data)
            ),
            level = 0
          )
          debug_log("Primary file loaded successfully - metadata will be recreated", 1)
          release_started_at <- Sys.time()
          primary_data_state$commit_import_generation(result$data, placeholder_metadata)
          debug_log(sprintf("IMPORT TIMING: downstream release %.3fs | committed generation",
                            as.numeric(difftime(Sys.time(), release_started_at, units = "secs"))), 1)
          telemetry_log("end", "upload", paste(data_metrics(result$data),
            paste0("| cache_hit=", isTRUE(result$cache_hit))))
          shinyjs::show(id = ns("header_sheet_1"), asis = TRUE)
          showNotification("Primary data file loaded successfully", type = "message", duration = 3)
        } else {
          error_msg <- if (!is.null(result$message)) result$message else "Unknown error during file loading"
          debug_log(paste("Primary file loading failed:", error_msg), 1)
          add_loading_log("primary file loading", "error", error_msg)
          showNotification(paste("Error loading primary file:", error_msg), type = "error", duration = 8)
          primary_data_state$abort_import_generation()
        }

      }, error = function(e) {
        error_msg <- safe_error_message(e)
        debug_log(paste("Error in primary file upload observer:", error_msg), 1)
        showNotification(paste("Error during file upload:", error_msg), type = "error", duration = 8)
        add_loading_log("primary file upload", "error", error_msg)
        primary_data_state$abort_import_generation()
      }, finally = {
        loading_active(FALSE)
        current_operation("")
      })
    })

    # Primary sheet change
    observeEvent(input$sheetDropdown, {
      if (isTRUE(restore_observer_guard_active("Primary sheet dropdown observer"))) {
        return()
      }

      tryCatch({
        req(input$sheetDropdown)
        skip_primary_sheet <- skip_next_sheet_change_primary()
        if (!is.null(skip_primary_sheet) && identical(as.character(input$sheetDropdown), as.character(skip_primary_sheet))) {
          skip_next_sheet_change_primary(NULL)
          selected_sheet_primary(input$sheetDropdown)
          debug_log(paste("Primary sheet dropdown programmatic update skipped:", input$sheetDropdown), 2)
          return()
        }
        skip_next_sheet_change_primary(NULL)
        selected_sheet_primary(input$sheetDropdown)
        if (isTRUE(reset_replay_active())) {
          debug_log("Primary sheet dropdown reset replay observed; skipping sheet reload", 2)
          return()
        }

        # A genuine (non-programmatic, non-restore) sheet switch always
        # starts a fresh header-row selection, same as a new file upload.
        # Reset the input and arm the one-shot skip so the header-row
        # observer above does not immediately reprocess the sheet a second
        # time with a stale header value.
        updateNumericInput(session, "header_row", value = 1L)
        skip_next_programmatic_header_update_primary(TRUE)
        debug_log(paste("Primary sheet change reset header row to 1 for sheet:", input$sheetDropdown), 1)

        # Post-restore path: the uploaded file is gone but we have the
        # sheet cache populated from get_session_state. Switch from the
        # in-memory cache and skip the file-reload branch.
        file_available <- is_live_excel_upload(input$file)
        if (!file_available) {
          if (isTRUE(skip_next_cached_sheet_apply_primary())) {
            skip_next_cached_sheet_apply_primary(FALSE)
            debug_log("Sheet cache apply skipped once after session restore", 1)
            return()
          }

          cached <- sheet_cache_primary()
          cached_data <- get_cached_sheet_data(cached, input$sheetDropdown)
          if (!is.null(cached_data)) {
            result <- normalize_cached_sheet_data(
              cached_data,
              1L,
              "primary cached sheet change"
            )

            if (result$success) {
              df_from_cache <- as.data.frame(result$data, check.names = FALSE)
              data_fixed(df_from_cache)
              data_primary(df_from_cache)
              clear_derived_primary_state_for_sheet_change("Cached primary sheet change:")
              publish_primary_current_sheet(df_from_cache, paste("cached sheet change:", input$sheetDropdown))
              if (!is.null(core_values)) {
                clear_stale_metadata_for_data(df_from_cache, "Cached primary sheet change:")
              }
              debug_log(paste("Sheet switched from cache (no file):",
                              input$sheetDropdown), 1)
            } else {
              showNotification(paste("Error changing sheet from cache:", result$error),
                               type = "error", duration = 8)
            }
          } else if (length(get_sheet_choices_from_state(cached, primary_file_meta())) > 0) {
            showNotification(
              "Only the restored current sheet is available because the uploaded Excel file is no longer present. Re-upload the workbook to load other sheets.",
              type = "warning", duration = 8
            )
          }
          return()
        }

        req(data_fixed())

        # When new data is loaded, reactivate metadata observer
        if (!is.null(core_values)) {
          core_values$metadata_observer_active(TRUE)
          debug_log("Metadata observer reactivated for new data", level = 1)
        }

        ext <- tolower(tools::file_ext(input$file$name))
        if (ext %in% c("xlsx", "xls")) {
          cached_live <- sheet_cache_primary()
          cached_live_data <- get_cached_sheet_data(cached_live, input$sheetDropdown)
          if (!is.null(cached_live_data)) {
            df_loaded <- as.data.frame(cached_live_data, check.names = FALSE)
            data_fixed(df_loaded)
            data_primary(df_loaded)
            clear_derived_primary_state_for_sheet_change("Cached primary sheet change:")
            publish_primary_current_sheet(df_loaded, paste("cached sheet change:", input$sheetDropdown))
            clear_stale_metadata_for_data(df_loaded, "Cached primary sheet change:")
            debug_log(paste("Sheet switched from lazy cache:", input$sheetDropdown), 1)
            showNotification("Sheet changed: loaded from cache", type = "message", duration = 3)
            return()
          }

          loading_active(TRUE)
          current_operation("Reloading primary sheet")

          result <- load_file_enhanced(
            file_input = input$file,
            sheet_name = input$sheetDropdown,
            header_flag = TRUE,
            operation_name = "primary sheet change",
            use_file_cache = FALSE
          )

          if (result$success) {
            df_loaded <- as.data.frame(result$data, check.names = FALSE)
            data_fixed(df_loaded)
            data_primary(df_loaded)
            clear_derived_primary_state_for_sheet_change("Primary sheet change:")

            debug_log(
              sprintf(
                "File load summary | Scope: Primary | Action: Sheet changed | Sheet: %s | Dimensions: %d x %d",
                input$sheetDropdown, nrow(df_loaded), ncol(df_loaded)
              ),
              level = 0
            )

            cache_loaded_sheet(sheet_cache_primary, input$sheetDropdown, df_loaded, "Primary sheet change")
            update_workbook_manifest(primary_file_meta, input$sheetDropdown, 1L, sheet_cache_primary())
            publish_primary_current_sheet(df_loaded, paste("sheet change:", input$sheetDropdown))
            debug_log("SHEET CHANGE: Updated primary data through state adapter", level = 1)

            # Update metadata compatibility state after the adapter publishes data.
            if (!is.null(core_values)) {
              debug_log("Sheet change: Checking metadata after adapter update", 1)

              # Clear metadata whenever the new sheet's columns (count OR names)
              # differ from the previous metadata. A count-only check leaks
              # stale assignments across sheets with equal column counts but
              # different column names, which later causes downstream modules
              # (e.g. Basemean) to operate on the wrong column set.
              current_meta <- core_values$handson_metadata()
              if (!is.null(current_meta)) {
                expected_cols <- ncol(df_loaded)
                meta_rows <- nrow(current_meta)
                data_col_names <- names(df_loaded)
                meta_col_names <- current_meta$Column
                columns_match <- meta_rows == expected_cols &&
                  identical(as.character(meta_col_names), as.character(data_col_names))
                if (!columns_match) {
                  debug_log("Sheet change: Clearing metadata due to column mismatch", 1)
                  primary_data_state$set_metadata_for_current_data(NULL)
                  if (!is.null(core_values$final_processed_metadata)) {
                    core_values$final_processed_metadata(NULL)
                  }
                  # The adapter also drops rv$data_def so modules that prefer it do not
                  # keep reading the previous sheet's metadata until the next
                  # auto-assign / rule pass rewrites it.
                }
              }
            }

            debug_log("Sheet change: Adapter updated legacy primary data holders", 2)

            showNotification(paste("Sheet changed:", result$message), type = "message", duration = 3)
          } else {
            showNotification(paste("Error changing sheet:", result$error), type = "error", duration = 8)
          }
        }
      }, error = function(e) {
        debug_log(paste("Error in primary sheet change:", e$message), 1)
      }, finally = {
        loading_active(FALSE)
        current_operation("")
      })
    })

    # Additional file upload
    observeEvent(input$file2, {
      if (isTRUE(restore_observer_guard_active("Additional file upload observer"))) {
        return()
      }

      upload_telemetry$id <- new_upload_correlation_id()
      upload_telemetry$started <- telemetry_now()
      upload_telemetry$counters <- new.env(parent = emptyenv())
      if (!is.null(rv)) {
        rv$datawizard_upload_correlation_id <- upload_telemetry$id
        rv$datawizard_upload_monotonic_started <- upload_telemetry$started
      }
      telemetry_log("start", "upload", "scope=secondary")

      tryCatch({
        file_input2 <- input$file2
        if (is.null(file_input2) || is.null(file_input2$datapath) ||
            !file.exists(file_input2$datapath)) {
          debug_log("No valid additional file input provided", 2)
          return()
        }

        if (!is_supported_datawizard_upload_dw(file_input2)) {
          debug_log(paste("Unsupported additional file upload:", file_input2$name), 1)
          showNotification(datawizard_unsupported_upload_message_dw, type = "error", duration = 8)
          return()
        }

        file_input <- file_input2
        ext <- tolower(tools::file_ext(file_input$name %||% file_input$datapath %||% ""))
        if (identical(ext, "rds")) {
          loading_active(TRUE)
          current_operation("Restoring session file")
          handoff_session_restore_upload(file_input, "secondary")
          return()
        }

        set_loader_mode("interactive_load", "secondary file upload")
        if (safe_is_true(loading_active())) {
          debug_log("Loading already active - ignoring additional file upload", 2)
          return()
        }

        loading_active(TRUE)
        current_operation("Loading additional file")

        sheet_names2 <- get_excel_sheet_names(file_input2)
        secondary_file_meta(make_excel_file_meta(file_input2, sheet_names2))
        sheet_cache_secondary(new_sheet_cache(sheet_names2))
        first_sheet2 <- if (length(sheet_names2) > 0) sheet_names2[[1]] else NULL
        selected_sheet_secondary(first_sheet2)
        updateNumericInput(session, "header_row2", value = 1L)
        if (!is.null(first_sheet2)) {
          skip_next_sheet_change_secondary(first_sheet2)
          updateSelectInput(session, "sheetDropdown2", selected = first_sheet2)
        }
        skip_next_programmatic_header_update_secondary(TRUE)
        debug_log(paste("Additional file upload reset sheet/header to:",
                        first_sheet2 %||% "<non-excel>", "/ 1"), 1)

        result <- load_file_enhanced(
          file_input = file_input2,
          sheet_name = first_sheet2,
          header_flag = TRUE,
          operation_name = "additional file loading",
          use_file_cache = is.null(first_sheet2)
        )

        if (result$success) {
          df2_loaded <- as.data.frame(result$data, check.names = FALSE)
          data2_fixed(df2_loaded)
          data_additional(df2_loaded)

          # Persist original + the selected sheet in the lazy cache for
          # post-restore reuse. Other sheets load on demand while the upload
          # datapath is still available.
          secondary_data_original(df2_loaded)
          primary_data_state$set_dataset("secondary_original", df2_loaded, source = paste("file upload:", private_label(file_input2$name)), allow_original_update = TRUE)
          publish_secondary_current_sheet(df2_loaded, paste("file upload:", private_label(file_input2$name)))
          if (!is.null(first_sheet2)) {
            cache_loaded_sheet(sheet_cache_secondary, first_sheet2, df2_loaded, "Additional upload")
            update_workbook_manifest(secondary_file_meta, first_sheet2, 1L, sheet_cache_secondary())
          }
          if (!is.null(rv)) rv$secondary_data_original <- df2_loaded

          debug_log(
            sprintf(
              "File load summary | Scope: Additional | Action: File loaded | File name: %s | Dimensions: %d x %d",
              private_label(file_input2$name), nrow(df2_loaded), ncol(df2_loaded)
            ),
            level = 0
          )
          telemetry_log("end", "upload", paste("scope=secondary |", data_metrics(df2_loaded),
            paste0("| cache_hit=", isTRUE(result$cache_hit))))
          shinyjs::show(id = ns("header_sheet_2"), asis = TRUE)
          showNotification(paste("Additional file loaded:", result$message), type = "message", duration = 3)
        } else {
          showNotification(paste("Error loading additional file:", result$error), type = "error", duration = 8)
        }

      }, error = function(e) {
        debug_log(paste("Critical error in additional file upload:", e$message), 1)
        showNotification("Critical error during additional file upload", type = "error", duration = 8)
      }, finally = {
        loading_active(FALSE)
        current_operation("")
      })
    }, ignoreNULL = TRUE)

    # Additional sheet change
    observeEvent(input$sheetDropdown2, {
      if (isTRUE(restore_observer_guard_active("Additional sheet dropdown observer"))) {
        return()
      }

      tryCatch({
        req(input$sheetDropdown2)
        skip_secondary_sheet <- skip_next_sheet_change_secondary()
        if (!is.null(skip_secondary_sheet) && identical(as.character(input$sheetDropdown2), as.character(skip_secondary_sheet))) {
          skip_next_sheet_change_secondary(NULL)
          selected_sheet_secondary(input$sheetDropdown2)
          debug_log(paste("Additional sheet dropdown programmatic update skipped:", input$sheetDropdown2), 2)
          return()
        }
        skip_next_sheet_change_secondary(NULL)
        selected_sheet_secondary(input$sheetDropdown2)
        if (isTRUE(reset_replay_active())) {
          debug_log("Additional sheet dropdown reset replay observed; skipping sheet reload", 2)
          return()
        }

        # A genuine (non-programmatic, non-restore) sheet switch always
        # starts a fresh header-row selection, same as a new file upload and
        # the primary sheet handler above.
        updateNumericInput(session, "header_row2", value = 1L)
        skip_next_programmatic_header_update_secondary(TRUE)
        debug_log(paste("Additional sheet change reset header row to 1 for sheet:", input$sheetDropdown2), 1)

        # Post-restore cache fallback (see primary sheet handler for rationale).
        file2_available <- is_live_excel_upload(input$file2)
        if (!file2_available) {
          cached2 <- sheet_cache_secondary()
          cached2_data <- get_cached_sheet_data(cached2, input$sheetDropdown2)
          if (isTRUE(skip_next_cached_sheet_apply_secondary())) {
            skip_next_cached_sheet_apply_secondary(FALSE)
            debug_log("Additional sheet cache apply skipped once after session restore", 1)
            return()
          }

          if (!is.null(cached2_data)) {
            result <- normalize_cached_sheet_data(
              cached2_data,
              1L,
              "additional cached sheet change"
            )

            if (result$success) {
              df2_from_cache <- as.data.frame(result$data, check.names = FALSE)
              data2_fixed(df2_from_cache)
              data_additional(df2_from_cache)
              publish_secondary_current_sheet(df2_from_cache, paste("cached sheet change:", input$sheetDropdown2))
              clear_stale_metadata_for_data(df2_from_cache, "Cached additional sheet change:")
              debug_log(paste("Additional sheet switched from cache (no file):",
                              input$sheetDropdown2), 1)
            } else {
              showNotification(paste("Error changing additional sheet from cache:", result$error),
                               type = "error", duration = 8)
            }
          } else if (length(get_sheet_choices_from_state(cached2, secondary_file_meta())) > 0) {
            showNotification(
              "Only the restored current additional sheet is available because the uploaded Excel file is no longer present. Re-upload the workbook to load other sheets.",
              type = "warning", duration = 8
            )
          }
          return()
        }

        req(data2_fixed())
        ext2 <- tolower(tools::file_ext(input$file2$name))
        if (ext2 %in% c("xlsx", "xls")) {
          cached2_live <- sheet_cache_secondary()
          cached2_live_data <- get_cached_sheet_data(cached2_live, input$sheetDropdown2)
          if (!is.null(cached2_live_data)) {
            df2_loaded <- as.data.frame(cached2_live_data, check.names = FALSE)
            data2_fixed(df2_loaded)
            data_additional(df2_loaded)
            publish_secondary_current_sheet(df2_loaded, paste("cached sheet change:", input$sheetDropdown2))
            clear_stale_metadata_for_data(df2_loaded, "Cached additional sheet change:")
            debug_log(paste("Additional sheet switched from lazy cache:", input$sheetDropdown2), 1)
            showNotification("Additional sheet changed: loaded from cache", type = "message", duration = 3)
            return()
          }

          loading_active(TRUE)
          current_operation("Reloading additional sheet")

          result <- load_file_enhanced(
            file_input = input$file2,
            sheet_name = input$sheetDropdown2,
            header_flag = TRUE,
            operation_name = "additional sheet change",
            use_file_cache = FALSE
          )

          if (result$success) {
            df2_loaded <- as.data.frame(result$data, check.names = FALSE)

            # Update internal reactive values
            data2_fixed(df2_loaded)
            data_additional(df2_loaded)
            publish_secondary_current_sheet(df2_loaded, paste("sheet change:", input$sheetDropdown2))
            cache_loaded_sheet(sheet_cache_secondary, input$sheetDropdown2, df2_loaded, "Additional sheet change")
            update_workbook_manifest(secondary_file_meta, input$sheetDropdown2, 1L, sheet_cache_secondary())

            debug_log(
              sprintf(
                "File load summary | Scope: Additional | Action: Sheet changed | Sheet: %s | Dimensions: %d x %d",
                input$sheetDropdown2, nrow(df2_loaded), ncol(df2_loaded)
              ),
              level = 0
            )
            showNotification(paste("Additional sheet changed:", result$message), type = "message", duration = 3)
          } else {
            showNotification(paste("Error changing additional sheet:", result$error), type = "error", duration = 8)
          }
        }
      }, error = function(e) {
        debug_log(paste("Error in additional sheet change:", e$message), 1)
      }, finally = {
        loading_active(FALSE)
        current_operation("")
      })
    })
  }, envir = loader_environment)
  invisible(NULL)
}
