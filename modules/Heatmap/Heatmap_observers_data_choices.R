    # Section 1: Sort-state synchronization
    # Purpose: Keeps heatmap_applied_sort_state in sync with the sort UI inputs
    #          before the first explicit Create run, so that individual-tab plots
    #          show consistent ordering before a heatmap has been created.
    # --------------------------------------------------------------------------
    observeEvent(list(input$sort_proteins_by, input$sort_samples_by), {
      # Skip during session restore: the captured heatmap_applied_sort_state
      # was already restored by set_session_state(), and the UI input echoes
      # from update*Input() would otherwise overwrite it with defaults.
      if (isTRUE(heatmap_state$restore_in_progress)) return()
      if (is.null(input$create_heatmap_btn) || input$create_heatmap_btn < 1) {
        heatmap_applied_sort_state(list(
          sort_proteins_by = input$sort_proteins_by %||% "z_score",
          sort_samples_by  = input$sort_samples_by  %||% "none"
        ))
      }
    }, ignoreInit = FALSE)

    # --------------------------------------------------------------------------
    # Section 2: UI Update Observers
    # --------------------------------------------------------------------------

    normalize_content_value <- function(x) {
      x <- trimws(as.character(x))
      x <- gsub("[[:space:]]+", " ", x)
      x
    }


    heatmap_metadata_availability_log_state <- new.env(parent = emptyenv())

    heatmap_metadata_availability_signature <- function(data_def = NULL) {
      if (!is.null(rv$datawizard_metadata_content_signature)) {
        return(as.character(rv$datawizard_metadata_content_signature))
      }

      dd <- data_def %||% rv$data_def
      if (is.null(dd) || !is.data.frame(dd)) {
        return("<no-data-def>")
      }

      signature_columns <- intersect(
        c("Column", "Content", "Sample", "Numerator", "Denominator", "Options"),
        colnames(dd)
      )
      if (length(signature_columns) == 0) {
        return(paste0("rows=", nrow(dd), ";cols=", ncol(dd)))
      }

      signature_values <- vapply(signature_columns, function(column_name) {
        paste0(column_name, "=", paste(normalize_content_value(dd[[column_name]]), collapse = "|"))
      }, character(1))

      paste(c(paste0("rows=", nrow(dd)), signature_values), collapse = ";")
    }

    heatmap_log_availability_once <- function(feature, state, message, level = 1, data_def = NULL) {
      signature <- heatmap_metadata_availability_signature(data_def)
      state_key <- paste(feature, state, signature, sep = "::")
      previous_key <- heatmap_metadata_availability_log_state[[feature]]

      if (!identical(previous_key, state_key)) {
        heatmap_debug_log(message, level)
        heatmap_metadata_availability_log_state[[feature]] <- state_key
      }
    }

    observe({
      datawizard_import_ready_signature(rv)
      if (datawizard_import_barrier_active(rv)) {
        heatmap_debug_log("Import barrier active; preserving Heatmap choices until ready", 2)
        return()
      }
      req(heatmap_data_modified(), heatmap_df_data_definition())
      if (datawizard_metadata_defer_downstream_choices(rv)) {
        heatmap_log_availability_once("abundance", "metadata_pending", "Metadata assignment pending; deferring abundance choices", 2)
        return()
      }
      tryCatch({
        data_def <- heatmap_df_data_definition()

        possible_values <- c(
          "Raw Abundance",
          "Normalized Abundance",
          "Imputed Raw Abundance",
          "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
          "Batch Corrected Raw Abundance",
          "Batch Corrected Normalized Abundance"
        )
        content_values <- normalize_content_value(data_def$Content)
        possible_values_norm <- normalize_content_value(possible_values)
        available_choices <- possible_values[possible_values_norm %in% content_values]

        if (length(available_choices) > 0) {
          current_selection <- input$custom_col_sel_heatmap
          selected_value <- if (is.null(current_selection) || !current_selection %in% available_choices) {
            if ("Normalized Abundance" %in% available_choices) "Normalized Abundance"
            else if ("Raw Abundance" %in% available_choices) "Raw Abundance"
            else available_choices[1]
          } else current_selection

          updateSelectizeInput(session, "custom_col_sel_heatmap",
                               choices = available_choices, selected = selected_value)
        } else {
          updateSelectizeInput(session, "custom_col_sel_heatmap",
                               choices = character(0), selected = NULL)
          if (datawizard_metadata_ready_for_abundance_warning(rv, data_def)) {
            heatmap_log_availability_once("abundance", "missing_ready", "No abundance data available. Prompting user.", 2, data_def)
          } else {
            heatmap_log_availability_once("abundance", "missing_metadata_not_ready", "No abundance data available; metadata not ready yet.", 3, data_def)
          }
          # showNotification("No abundance data available. Please process your data first.",
          #                  type = "warning", duration = 5)
        }
      }, error = function(e) {
        heatmap_debug_log(paste("Error updating custom_col_sel_heatmap choices:", e$message), 1)
        basic_choices <- c("Raw Abundance", "Normalized Abundance")
        updateSelectizeInput(session, "custom_col_sel_heatmap",
                             choices = basic_choices, selected = "Normalized Abundance")
      })
    })

    observe({
      req(heatmap_data_modified(), heatmap_df_data_definition())
      if (datawizard_metadata_defer_downstream_choices(rv)) {
        heatmap_debug_log("Metadata assignment pending; deferring identifier choices", 2)
        return()
      }
      tryCatch({
        data_def <- heatmap_df_data_definition()
        central_identifier_choices <- rv$datawizard_identifier_choices %||% character(0)
        identifier_rows <- which(data_def$Content == "Identifier")

        if (length(central_identifier_choices) > 0) {
          updateSelectInput(session, "GeneIdentifierColumn_Heatmap",
                            choices = central_identifier_choices,
                            selected = central_identifier_choices[1])
        } else if (length(identifier_rows) > 0) {
          identifier_column_names <- data_def$Column[identifier_rows]
          identifier_labels <- data_def$Options[identifier_rows]

          valid_labels <- !is.na(identifier_labels) & nzchar(trimws(identifier_labels))
          valid_columns <- !is.na(identifier_column_names) & nzchar(trimws(identifier_column_names))
          valid_rows <- valid_labels & valid_columns

          if (any(valid_rows)) {
            identifier_choices <- identifier_column_names[valid_rows]
            names(identifier_choices) <- identifier_labels[valid_rows]

            updateSelectInput(session, "GeneIdentifierColumn_Heatmap",
                              choices = identifier_choices,
                              selected = identifier_choices[1])
          }
        }
      }, error = function(e) {
        heatmap_debug_log(paste("Error updating identifier choices:", e$message), 1)
      })
    })

    # Update sample choices when data type changes
    # Re-initialize sample choices when the underlying data definition changes
    observeEvent(rv$data_def, {
      dd <- rv$data_def
      if (is.null(dd)) return()

      data_type <- input$custom_col_sel_heatmap
      if (is.null(data_type)) return()

      sample_rows <- which(
        normalize_content_value(dd$Content) == normalize_content_value(data_type) & !is.na(dd$Sample)
      )
      sample_choices <- if (length(sample_rows) > 0) {
        unique(dd$Sample[sample_rows])
      } else {
        character(0)
      }

      # Initialization: no explicit user selection yet -> keep selection empty
      updateSelectizeInput(session, "select_samples_heatmap",
                           choices  = sample_choices,
                           selected = NULL)
    })

    # Update sample choices when data type changes
    observeEvent(input$custom_col_sel_heatmap, {
      req(rv$data_def)

      data_type <- input$custom_col_sel_heatmap
      if (is.null(data_type)) return()

      dd <- rv$data_def

      sample_rows <- which(
        normalize_content_value(dd$Content) == normalize_content_value(data_type) & !is.na(dd$Sample)
      )
      sample_choices <- if (length(sample_rows) > 0) {
        unique(dd$Sample[sample_rows])
      } else {
        character(0)
      }

      # Aktuelle Auswahl holen
      current_selected <- isolate(input$select_samples_heatmap)

      # Keep only selections that remain valid
      if (!is.null(current_selected) && length(current_selected) > 0) {
        new_selected <- intersect(current_selected, sample_choices)
        if (length(new_selected) == 0) {
          new_selected <- NULL
        }
      } else {
        new_selected <- NULL
      }

      # Important: do not overwrite selected on every minor update,
      # only when the data type actually changes.
      updateSelectizeInput(session, "select_samples_heatmap",
                           choices  = sample_choices,
                           selected = new_selected)
    })

    # Keep minimum abundance-value filter bounds in sync with selected data type.
    # isolate() is used when reading the current input value so that this observer
    # does NOT take a reactive dependency on min_abundance_values_per_row_heatmap.
    # Without isolate(), every updateNumericInput() call below would re-invalidate
    # this observer → which calls updateNumericInput() again → infinite loop.
    # The value parameter is only included when the current value is out of the
    # new range (i.e. needs clamping). Updating only min/max/step does NOT cause
    # the client to send a new input value back to the server.
    observe({
      req(rv$data_def)

      data_type <- input$custom_col_sel_heatmap
      if (is.null(data_type) || !nzchar(data_type)) return()

      dd <- rv$data_def
      sample_rows <- which(
        normalize_content_value(dd$Content) == normalize_content_value(data_type) & !is.na(dd$Sample)
      )
      max_available_values <- max(1L, length(unique(dd$Sample[sample_rows])))

      # isolate: read current value without creating a reactive dependency on it
      current_value <- isolate(input$min_abundance_values_per_row_heatmap) %||% 1
      if (!is.finite(current_value)) current_value <- 1
      new_value <- min(max(1L, as.integer(round(current_value))), max_available_values)

      if (new_value != current_value) {
        # Value is out of the new range and must be clamped – include value update
        updateNumericInput(
          session,
          "min_abundance_values_per_row_heatmap",
          min = 1,
          max = max_available_values,
          step = 1,
          value = new_value
        )
      } else {
        # Value is already valid – only update the bounds, NOT the value.
        # This avoids sending a client-side change event that would re-trigger
        # the live-rebuild observeEvent unnecessarily.
        updateNumericInput(
          session,
          "min_abundance_values_per_row_heatmap",
          min = 1,
          max = max_available_values,
          step = 1
        )
      }
    })


    heatmap_should_clear_ratio_snapshot <- function() {
      if (isTRUE(heatmap_state$restore_in_progress)) return(FALSE)
      plots <- tryCatch(isolate(heatmap_plots()), error = function(e) list())
      fixed_expr <- tryCatch(isolate(heatmap_fixed_expression()), error = function(e) NULL)
      is.null(plots$expr) && is.null(fixed_expr)
    }

    heatmap_clear_ratio_snapshot_if_safe <- function(reason = "ratio unavailable") {
      if (isTRUE(heatmap_should_clear_ratio_snapshot())) {
        heatmap_abundance_ratio_values(NULL)
        heatmap_fixed_abundance_ratio(NULL)
      } else {
        heatmap_debug_log(paste("Preserving cached abundance ratio heatmap snapshot:", reason), 1)
      }
      invisible(TRUE)
    }

    # 1. Abundance Ratio observer
    observe({
      req(rv$data_def)

      tryCatch({
        dd <- rv$data_def
        if (datawizard_metadata_defer_downstream_choices(rv)) {
          heatmap_log_availability_once("ratio", "metadata_pending", "Metadata assignment pending; deferring ratio availability check", 2, dd)
          return()
        }
        ratio_cols <- which(dd$Content == "Abundance Ratio")

        if (length(ratio_cols) > 0) {
          # Additional validation: Check if ratio columns contain valid data
          has_valid_ratio_data <- FALSE

          if (!is.null(rv$data_mod)) {
            dm <- rv$data_mod

            for (col_idx in ratio_cols) {
              if (col_idx <= ncol(dm)) {
                col_data <- suppressWarnings(as.numeric(dm[[col_idx]]))
                if (any(is.finite(col_data) & col_data > 0)) {
                  has_valid_ratio_data <- TRUE
                  break
                }
              }
            }
          } else {
            # If no data_mod yet, assume ratios are valid
            has_valid_ratio_data <- TRUE
          }

          if (has_valid_ratio_data) {
            heatmap_log_availability_once("ratio", "available", "Abundance ratio data available and valid - enabling controls", 2, dd)

            # Enable ratio filter checkbox and hide warning message
            shinyjs::enable("enable_ratio_filter_heatmap")
            shinyjs::hide("ratio_unavailable_message")

            # Enable abundance ratio heatmap checkbox and hide warning message
            shinyjs::enable("show_abundance_ratio_heatmap")
            shinyjs::hide("abundance_ratio_heatmap_unavailable_message")

            # Update ratio column choices
            ratio_choices <- dd$Column[ratio_cols]
            names(ratio_choices) <- paste0(dd$Column[ratio_cols],
                                           " (", dd$Numerator[ratio_cols],
                                           " / ", dd$Denominator[ratio_cols], ")")

            updateSelectInput(
              session,
              "abundance_ratio_col_heatmap",
              choices = ratio_choices,
              selected = ratio_choices[1]
            )

            current_extension_ratio <-
              isolate(
                input$abundance_ratio_col_extension_heatmap
              )

            extension_ratio_selection <-
              if (
                !is.null(current_extension_ratio) &&
                length(current_extension_ratio) == 1L &&
                !is.na(current_extension_ratio) &&
                current_extension_ratio %in%
                unname(ratio_choices)
              ) {
                current_extension_ratio
              } else {
                unname(ratio_choices[[1L]])
              }

            updateSelectInput(
              session,
              "abundance_ratio_col_extension_heatmap",
              choices = ratio_choices,
              selected = extension_ratio_selection
            )

          } else {
            heatmap_log_availability_once("ratio", "invalid", "Abundance ratio columns found but contain no valid data", 1, dd)

            # Disable both ratio features
            # NOTE: Do not force checkbox values here; metadata can arrive sequentially
            # during startup/load and we must not overwrite user intent transiently.
            shinyjs::disable("enable_ratio_filter_heatmap")
            shinyjs::show("ratio_unavailable_message")

            shinyjs::disable("show_abundance_ratio_heatmap")
            shinyjs::show("abundance_ratio_heatmap_unavailable_message")

            # Clear choices
            updateSelectInput(session, "abundance_ratio_col_heatmap",
                              choices = character(0),
                              selected = NULL)

            updateSelectInput(
              session,
              "abundance_ratio_col_extension_heatmap",
              choices = character(0),
              selected = NULL
            )

            # Preserve existing plot snapshots when later live metadata does not expose ratios.
            heatmap_clear_ratio_snapshot_if_safe("ratio columns invalid in live metadata")
          }

        } else {
          heatmap_log_availability_once("ratio", "missing", "No abundance ratio columns found - disabling ratio controls", 1, dd)

          # Disable both ratio features
          shinyjs::disable("enable_ratio_filter_heatmap")
          shinyjs::show("ratio_unavailable_message")

          shinyjs::disable("show_abundance_ratio_heatmap")
          shinyjs::show("abundance_ratio_heatmap_unavailable_message")

          # Clear choices
          updateSelectInput(session, "abundance_ratio_col_heatmap",
                            choices = character(0),
                            selected = NULL)

          updateSelectInput(
            session,
            "abundance_ratio_col_extension_heatmap",
            choices = character(0),
            selected = NULL
          )

          # Preserve existing plot snapshots when later live metadata does not expose ratios.
          heatmap_clear_ratio_snapshot_if_safe("ratio columns missing in live metadata")
        }

      }, error = function(e) {
        heatmap_debug_log(paste("Error in abundance ratio observer:", e$message), 1)

        # Emergency fallback: disable everything
        shinyjs::disable("enable_ratio_filter_heatmap")
        shinyjs::disable("show_abundance_ratio_heatmap")

        # Preserve existing plot snapshots during transient metadata/load errors.
        heatmap_clear_ratio_snapshot_if_safe("ratio observer error")
      })
    })

    # 2. P-VALUE OBSERVER
    observe({
      req(rv$data_def)

      tryCatch({
        dd <- rv$data_def
        if (datawizard_metadata_defer_downstream_choices(rv)) {
          heatmap_log_availability_once("pvalue", "metadata_pending", "Metadata assignment pending; deferring p-value availability check", 2, dd)
          return()
        }

        # Look for p-value related content types directly in data_def
        pvalue_content_types <- c(
          "P-Value", "p-Value", "p.Value", "pvalue", "p_value",
          "Adj. P-Value", "Adj.P.Value", "adj.p.value", "adj_p_value",
          "FDR", "adj.P.Val", "p.adj",
          "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value"
        )

        pvalue_cols <- which(dd$Content %in% pvalue_content_types)

        if (length(pvalue_cols) > 0) {
          # Additional validation: Check if p-value columns contain valid data
          has_valid_pvalue_data <- FALSE

          if (!is.null(rv$data_mod)) {
            dm <- rv$data_mod

            for (col_idx in pvalue_cols) {
              if (col_idx <= ncol(dm)) {
                col_data <- suppressWarnings(as.numeric(dm[[col_idx]]))
                # Valid p-values should be between 0 and 1
                if (any(is.finite(col_data) & col_data >= 0 & col_data <= 1)) {
                  has_valid_pvalue_data <- TRUE
                  break
                }
              }
            }
          } else {
            # If no data_mod yet, assume p-values are valid
            has_valid_pvalue_data <- TRUE
          }

          if (has_valid_pvalue_data) {
            heatmap_log_availability_once("pvalue", "available", "P-value data available and valid - enabling p-value filter controls", 2, dd)

            # Enable the p-value filter checkbox and hide the warning message
            shinyjs::enable("enable_pvalue_filter_heatmap")
            shinyjs::hide("pvalue_unavailable_message")

            # Update p-value type choices
            available_pvalue_types <- intersect(pvalue_content_types, dd$Content)

            if (length(available_pvalue_types) > 0) {
              updateSelectInput(session, "pval_type_heatmap",
                                choices = available_pvalue_types,
                                selected = available_pvalue_types[1])
            }

          } else {
            heatmap_log_availability_once("pvalue", "invalid", "P-value columns found but contain no valid p-value data", 1, dd)

            # Disable p-value filter controls
            shinyjs::disable("enable_pvalue_filter_heatmap")
            updateCheckboxInput(session, "enable_pvalue_filter_heatmap", value = FALSE)
            shinyjs::show("pvalue_unavailable_message")

            # Clear choices
            updateSelectInput(session, "pval_type_heatmap",
                              choices = character(0),
                              selected = NULL)
            updateSelectInput(session, "pval_col_heatmap",
                              choices = character(0),
                              selected = NULL)
          }

        } else {
          heatmap_log_availability_once("pvalue", "missing", "No p-value columns found - disabling p-value filter controls", 1, dd)

          # Disable p-value filter controls
          shinyjs::disable("enable_pvalue_filter_heatmap")
          updateCheckboxInput(session, "enable_pvalue_filter_heatmap", value = FALSE)
          shinyjs::show("pvalue_unavailable_message")

          # Clear choices
          updateSelectInput(session, "pval_type_heatmap",
                            choices = character(0),
                            selected = NULL)
          updateSelectInput(session, "pval_col_heatmap",
                            choices = character(0),
                            selected = NULL)
        }

      }, error = function(e) {
        heatmap_debug_log(paste("Error in p-value observer:", e$message), 1)

        # Emergency fallback: disable p-value filtering
        shinyjs::disable("enable_pvalue_filter_heatmap")
        updateCheckboxInput(session, "enable_pvalue_filter_heatmap", value = FALSE)
      })
    })

    # 3. ENHANCED P-VALUE COLUMN OBSERVER:
    observe({
      req(rv$data_def, input$pval_type_heatmap, input$enable_pvalue_filter_heatmap)

      # Only update if p-value filtering is enabled
      if (isTRUE(input$enable_pvalue_filter_heatmap)) {
        tryCatch({
          dd <- rv$data_def
          pval_type <- input$pval_type_heatmap

          if (!is.null(pval_type) && nzchar(pval_type)) {
            pval_cols <- which(dd$Content == pval_type)

            if (length(pval_cols) > 0) {
              pval_choices <- dd$Column[pval_cols]

              # Enhanced naming with comparison info
              names(pval_choices) <- paste0(
                dd$Column[pval_cols],
                " (", dd$Numerator[pval_cols], " vs ", dd$Denominator[pval_cols], ")"
              )

              updateSelectInput(session, "pval_col_heatmap",
                                choices = pval_choices,
                                selected = pval_choices[1])
            } else {
              # Clear choices if no columns found
              updateSelectInput(session, "pval_col_heatmap",
                                choices = character(0),
                                selected = NULL)
              heatmap_debug_log("No p-value columns found for selected type", 2)
            }
          }
        }, error = function(e) {
          heatmap_debug_log(paste("Error updating p-value columns:", e$message), 1)
        })
      }
    })
