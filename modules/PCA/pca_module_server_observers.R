# ==============================================================================
# File: modules/PCA/pca_module_server_observers.R
#
# Purpose:
#   Defines all observer registration functions for the PCA module, covering
#   input synchronization, protein/item selection, pathway integration, UI
#   state management, and label/color management.
#
# Architectural Role:
#   This file contains four registration functions that are called once from
#   modPCAServer in pca_module.R. Each function wraps a related group of
#   observers and outputs. No server function or reactive state definitions
#   live here; all reactive containers are created in pca_module_state.R and
#   passed in via the state parameter.
#
# Structure:
#   1. register_pca_input_observers          - Data type/sample/identifier sync
#   2. register_pca_protein_selection_observers - Transfer/remove/clear proteins
#                                               and gene symbol search display
#   3. register_pca_pathway_observers        - GSEA/GO pathway integration
#   4. register_pca_ui_state_observers       - Method/target/axis/toggle handlers
#                                             and right-side control reset
#   5. register_pca_label_management_observers - Master color controls, per-item
#                                               settings, item removal, clipboard
#
# Notes for future developers:
#   - All functions receive debug_log explicitly; do not use cat() directly.
#   - level = 1 for important state transitions, level = 2 for verbose tracing.
#   - register_pca_ui_state_observers and register_pca_label_management_observers
#     receive the full pca_state list; extract individual handles at the top of
#     each function for clarity.
#   - A single observeEvent handles toggle_protein_controls with proper
#     namespace usage via session$ns.
# ==============================================================================

register_pca_input_observers <- function(input, session, rv, debug_log, state = NULL) {
  normalize_content_value <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
  }
  metadata_not_ready_abundance_logged <- FALSE
  last_pca_metadata_ui <- new.env(parent = emptyenv())
  last_pca_metadata_ui$abundance_signature <- NULL
  last_pca_metadata_ui$identifier_signature <- NULL
  last_pca_metadata_ui$sample_signature <- NULL

  # ========================================
  # CORRECTLY Consolidated Observer
  # This consolidates observers 1, 2, 3 while keeping exact working logic
  # ========================================
  observe({
    datawizard_import_ready_signature(rv)
    if (datawizard_import_barrier_active(rv)) {
      debug_log("Import barrier active; preserving PCA choices until ready", 2)
      return()
    }
    req(rv$data_mod, rv$data_def)
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log("Metadata assignment pending; deferring PCA metadata choices", 2)
      return()
    }

    tryCatch({
      data_def <- rv$data_def

      # ========================================
      # Observer 1 Logic: Update abundance data type choices
      # ========================================

      # Define possible abundance values - EXACT ORIGINAL LIST
      possible_values <- c(
        "Raw Abundance",
        "Normalized Abundance",
        "Imputed Raw Abundance",
        "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
        "Batch Corrected Raw Abundance",
        "Batch Corrected Normalized Abundance"
      )

      # Find which abundance types are available in the data
      content_values <- normalize_content_value(data_def$Content)
      possible_values_norm <- normalize_content_value(possible_values)
      available_choices <- possible_values[possible_values_norm %in% content_values]

      if (length(available_choices) == 0 &&
          !datawizard_metadata_ready_for_abundance_warning(rv, data_def)) {
        abundance_signature <- paste(c(character(0), NA_character_), collapse = "\r")
        if (!identical(last_pca_metadata_ui$abundance_signature, abundance_signature)) {
          updateSelectizeInput(session, "custom_col_sel_pca",
                               choices = character(0), selected = NULL)
          last_pca_metadata_ui$abundance_signature <- abundance_signature
        }

        sample_signature <- paste(character(0), collapse = "\r")
        if (!identical(last_pca_metadata_ui$sample_signature, sample_signature)) {
          updateSelectizeInput(session, "select_samples_pca",
                               choices = character(0), selected = character(0), server = TRUE)
          last_pca_metadata_ui$sample_signature <- sample_signature
        }

        if (!metadata_not_ready_abundance_logged) {
          debug_log("PCA abundance choices unavailable; metadata not ready", 3)
          metadata_not_ready_abundance_logged <<- TRUE
        }
        return()
      }

      selected_value <- NULL
      effective_data_type <- NULL
      if (length(available_choices) > 0) {
        metadata_not_ready_abundance_logged <<- FALSE
        restored_selection <- if (!is.null(state) && isTRUE(isolate(state$restore_in_progress()))) {
          isolate(state$pending_ui_inputs()$custom_col_sel_pca)
        } else NULL
        current_selection <- restored_selection %||% input$custom_col_sel_pca
        selected_value <- if (is.null(current_selection) || !current_selection %in% available_choices) {
          if ("Normalized Abundance" %in% available_choices) "Normalized Abundance"
          else if ("Raw Abundance" %in% available_choices) "Raw Abundance"
          else available_choices[1]
        } else current_selection

        effective_data_type <- selected_value
        abundance_signature <- paste(c(available_choices, selected_value), collapse = "\r")
        if (!identical(last_pca_metadata_ui$abundance_signature, abundance_signature)) {
          updateSelectizeInput(session, "custom_col_sel_pca",
                               choices = available_choices, selected = selected_value)
          last_pca_metadata_ui$abundance_signature <- abundance_signature
          debug_log(paste("Updated custom_col_sel_pca; selected:", selected_value), 2)
        }
      } else {
        abundance_signature <- paste(c(character(0), NA_character_), collapse = "\r")
        if (!identical(last_pca_metadata_ui$abundance_signature, abundance_signature)) {
          updateSelectizeInput(session, "custom_col_sel_pca",
                               choices = character(0), selected = NULL)
          last_pca_metadata_ui$abundance_signature <- abundance_signature
          if (datawizard_metadata_ready_for_abundance_warning(rv, data_def)) {
            debug_log("No abundance data available. Prompting user.", 2)
          } else {
            debug_log("No abundance data available; metadata not ready yet.", 3)
          }
        }
        # showNotification("No abundance data available. Please process your data first.",
        #                  type = "warning", duration = 5)
      }

      # ========================================
      # Observer 2 Logic: Update identifier column choices
      # ========================================

      central_identifier_choices <- rv$datawizard_identifier_option_choices %||% character(0)
      identifier_indices <- which(grepl("Identifier", data_def$Content, ignore.case = TRUE))

      # During restoration the captured-input updater owns this widget.  It
      # must install the restored metadata choices and the saved selection in
      # one browser message; selecting the first choice here would create a
      # transient identifier change before the saved value is applied.
      identifier_restore_pending <- !is.null(state) &&
        isTRUE(isolate(state$restore_in_progress())) &&
        !is.null(isolate(state$pending_ui_inputs()$GeneIdentifierColumn_pca))

      if (identifier_restore_pending) {
        debug_log("[PCA] Deferring identifier choices to session restore updater", 2)
      } else if (length(central_identifier_choices) > 0) {
        restored_identifier <- if (!is.null(state) && isTRUE(isolate(state$restore_in_progress()))) {
          isolate(state$pending_ui_inputs()$GeneIdentifierColumn_pca)
        } else NULL
        selected_identifier <- if (!is.null(restored_identifier) && restored_identifier %in% central_identifier_choices) {
          restored_identifier
        } else central_identifier_choices[1]
        identifier_signature <- paste(c(central_identifier_choices, selected_identifier), collapse = "\r")
        if (!identical(last_pca_metadata_ui$identifier_signature, identifier_signature)) {
          updateSelectInput(session, "GeneIdentifierColumn_pca",
                            choices = central_identifier_choices, selected = selected_identifier)
          last_pca_metadata_ui$identifier_signature <- identifier_signature
          debug_log(paste("Updated identifier choices from Data Wizard:", length(central_identifier_choices)), 2)
        }
      } else if (length(identifier_indices) > 0) {
        identifier_choices <- data_def$Options[identifier_indices]
        restored_identifier <- if (!is.null(state) && isTRUE(isolate(state$restore_in_progress()))) {
          isolate(state$pending_ui_inputs()$GeneIdentifierColumn_pca)
        } else NULL
        selected_identifier <- if (!is.null(restored_identifier) && restored_identifier %in% identifier_choices) {
          restored_identifier
        } else identifier_choices[1]
        identifier_signature <- paste(c(identifier_choices, selected_identifier), collapse = "\r")
        if (!identical(last_pca_metadata_ui$identifier_signature, identifier_signature)) {
          updateSelectInput(session, "GeneIdentifierColumn_pca",
                            choices = identifier_choices, selected = selected_identifier)
          last_pca_metadata_ui$identifier_signature <- identifier_signature
          debug_log(paste("Updated identifier choices:", length(identifier_choices)), 2)
        }
      }

      # ========================================
      # Observer 3 Logic: Update sample choices when data type changes
      # ========================================

      data_type <- effective_data_type
      if (is.null(data_type) || !nzchar(data_type)) {
        data_type <- input$custom_col_sel_pca
      }
      if (!is.null(data_type) && nzchar(data_type)) {

        content_col <- get_col(data_def, c("Content"))
        sample_col <- get_col(data_def, c("Sample", "Column"))

        sample_choices <- character(0)
        if (!is.null(content_col) && !is.null(sample_col)) {
          sample_rows <- which(
            normalize_content_value(data_def[[content_col]]) == normalize_content_value(data_type)
          )
          if (length(sample_rows) > 0) {
            sample_values <- as.character(data_def[[sample_col]][sample_rows])
            sample_values <- sample_values[!is.na(sample_values)]
            sample_values <- trimws(sample_values)
            sample_values <- sample_values[nzchar(sample_values)]
            sample_choices <- unique(sample_values)
          }
        }

        restored_samples <- if (!is.null(state) && isTRUE(isolate(state$restore_in_progress()))) {
          isolate(state$pending_ui_inputs()$select_samples_pca %||% character(0))
        } else character(0)
        previous_selection <- if (length(restored_samples) > 0L) restored_samples else {
          isolate(input$select_samples_pca %||% character(0))
        }
        retained_selection <- intersect(previous_selection, sample_choices)
        selected_samples <- if (length(retained_selection) > 0) retained_selection else sample_choices

        sample_signature <- paste(c(data_type, sample_choices, selected_samples), collapse = "\r")
        if (!identical(last_pca_metadata_ui$sample_signature, sample_signature)) {
          debug_log(paste("Updating sample choices for data type:", data_type), 2)
          debug_log(paste("Found", length(sample_choices), "samples for", data_type), 2)
          updateSelectizeInput(
            session,
            "select_samples_pca",
            choices = sample_choices,
            selected = selected_samples,
            server = TRUE
          )
          last_pca_metadata_ui$sample_signature <- sample_signature
        }
      }

    }, error = function(e) {
      debug_log(paste("Error in consolidated observer:", e$message), 1)

      # Fallback error handling for abundance types
      basic_choices <- c("Raw Abundance", "Normalized Abundance")
      updateSelectizeInput(session, "custom_col_sel_pca",
                           choices = basic_choices, selected = "Normalized Abundance")
    })
  })
}

register_pca_protein_selection_observers <- function(
    input,
    output,
    session,
    rv,
    selected_items_vector_pca,
    selected_data_pca,
    selected_protein_vector_pca,
    debug_log
) {
  # ========================================
  # Add Proteins Observer
  # ========================================
  observeEvent(input$transferButton_pca, {
    tryCatch({
      req(rv$data_mod, rv$data_def)

      df <- rv$data_mod
      selected_identifier <- input$GeneIdentifierColumn_pca

      if (is.null(selected_identifier) || selected_identifier == "") {
        showNotification("Please select an identifier type first", type = "warning")
        return()
      }

      if (is.null(input$searchGene_pca) || input$searchGene_pca == "") {
        showNotification("Please enter some proteins in the text area", type = "warning")
        return()
      }

      debug_log("Processing protein transfer for unified labeling system", 2)

      filter_data <- get_filter_string_pca(input$searchGene_pca, selected_identifier, debug_log)

      if (nrow(filter_data) > 0) {
        filter_data <- as.vector(filter_data[, 1])

        if (length(filter_data) > 0) {
          # EXACT MATCHING ONLY - use %in% instead of grep
          identifier <- df[[selected_identifier]][df[[selected_identifier]] %in% filter_data]
          identifier <- identifier[!is.na(identifier)]

          if (length(identifier) > 0) {
            # Get current selected items from unified system
            current_items <- selected_items_vector_pca() %||% character()

            # Combine with new identifiers and remove duplicates
            updated_items <- unique(c(current_items, identifier))

            # Store in unified system
            selected_items_vector_pca(updated_items)

            # Clear the text area after successful transfer
            updateTextAreaInput(session, "searchGene_pca", value = "")

            debug_log(paste("Added", length(identifier), "items to unified selection system"), 1)
            debug_log(paste("Total items now:", length(updated_items)), 2)

            showNotification(paste("Added", length(identifier), "proteins to selection (exact matches)"),
                             type = "message", duration = 3)
          } else {
            showNotification("No EXACT matching proteins found in dataset", type = "warning")
          }
        }
      } else {
        showNotification("No valid proteins found in input", type = "warning")
      }

    }, error = function(e) {
      debug_log(paste("Error in transferButton_pca:", e$message), 1)
      showNotification("Error adding proteins to selection", type = "error")
    })
  })

  # ========================================
  # Clear All Proteins Observer
  # ========================================
  observeEvent(input$clearButton_pca, {
    tryCatch({
      selected_data_pca(NULL)
      selected_protein_vector_pca(NULL)
      debug_log("Cleared all selected proteins", 2)
      showNotification("Cleared all selected proteins", type = "message", duration = 2)
    }, error = function(e) {
      debug_log(paste("Error in clearButton_pca:", e$message), 1)
    })
  })

  # ========================================
  # Selected Proteins Table Display
  # ========================================
  output$selectedGene_pca <- renderDataTable({
    tryCatch({
      selected_data <- selected_data_pca()

      if (is.null(selected_data)) {
        debug_log("No selected data available for table", 2)
        return(DT::datatable(data.frame(Info = "No proteins selected"),
                             options = list(dom = 't'), rownames = FALSE))
      }

      if (!is.data.frame(selected_data)) {
        debug_log("Selected data is not a data.frame", 1)
        return(DT::datatable(data.frame(Info = "Invalid data format"),
                             options = list(dom = 't'), rownames = FALSE))
      }

      if (nrow(selected_data) == 0) {
        debug_log("Selected data is empty", 2)
        return(DT::datatable(data.frame(Info = "No proteins selected"),
                             options = list(dom = 't'), rownames = FALSE))
      }

      selected_protein_vector_display <- selected_protein_vector_pca()

      if (!is.null(selected_protein_vector_display) && length(selected_protein_vector_display) > 0) {
        labeled_df <- data.frame(
          "Identifier" = selected_protein_vector_display,
          stringsAsFactors = FALSE
        )

        debug_log(paste("Displaying", nrow(labeled_df), "selected proteins"), 2)

        return(DT::datatable(
          labeled_df,
          options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = FALSE,
            scrollY = "120px",
            dom = 't'
          ),
          rownames = FALSE
        ))
      } else {
        return(DT::datatable(data.frame(Info = "No proteins selected"),
                             options = list(dom = 't'), rownames = FALSE))
      }

    }, error = function(e) {
      debug_log(paste("Error in selectedGene_pca table:", e$message), 1)
      return(DT::datatable(data.frame(Error = "Table display error"),
                           options = list(dom = 't'), rownames = FALSE))
    })
  })

  # ========================================
  # Gene Symbol Search Display
  # ========================================

  output$search_identifier_label_pca <- renderText({

    selected_identifier <-
      input$GeneIdentifierColumn_pca

    current_data <-
      tryCatch(
        rv$data_mod,
        error = function(e) NULL
      )

    identifier_available <-
      is.character(selected_identifier) &&
      length(selected_identifier) == 1L &&
      !is.na(selected_identifier) &&
      nzchar(selected_identifier) &&
      is.data.frame(current_data) &&
      selected_identifier %in% names(current_data)

    if (!identifier_available) {
      return(
        "Select identifier column"
      )
    }

    paste0(
      "Search for ",
      selected_identifier,
      ":"
    )
  })

  output$geneSymbolList_pca <- renderPrint({
    req(rv$data_mod, rv$data_def)

    quiet_log <- function(...) invisible(NULL)

    selected_identifier <- input$GeneIdentifierColumn_pca

    if (is.null(selected_identifier) || selected_identifier == "") {
      cat("Please select an identifier type first")
      return()
    }

    data <- rv$data_mod
    gene_symbols_text <- ""

    if (!is.null(input$searchGene_pca) && input$searchGene_pca != "") {
      filter_data <- get_filter_string_pca(input$searchGene_pca, selected_identifier, quiet_log)
      filter_data <- as.vector(filter_data[, 1])

      if (length(filter_data) > 0) {
        pattern <- paste(filter_data, collapse = "|")
        identifier <- grep(pattern, data[[selected_identifier]], ignore.case = TRUE, value = TRUE)
        gene_symbols_text <- paste(identifier, collapse = "\n")
      }
    }

    cat(gene_symbols_text)
  })

  # ========================================
  # Copy Gene Symbols to Clipboard
  # ========================================
  observeEvent(input$copyBtn_pca, {
    tryCatch({
      req(rv$data_mod, rv$data_def)

      data <- rv$data_mod
      selected_identifier <- input$GeneIdentifierColumn_pca
      identifier <- c()

      debug_log("Copy to clipboard button clicked", 2)

      if (!is.null(input$searchGene_pca) && input$searchGene_pca != "" &&
          !is.null(selected_identifier) && selected_identifier != "") {

        filter_data <- get_filter_string_pca(input$searchGene_pca, selected_identifier, debug_log)

        if (nrow(filter_data) > 0) {
          filter_data <- as.vector(filter_data[, 1])

          if (length(filter_data) > 0) {
            pattern <- paste(filter_data, collapse = "|")
            identifier <- grep(pattern, data[[selected_identifier]], ignore.case = TRUE, value = TRUE)
            identifier <- unique(identifier)

            if (length(identifier) > 0) {
              clipboard_text <- paste(identifier, collapse = "\n")

              escaped_text <- gsub("\\\\", "\\\\\\\\", clipboard_text)
              escaped_text <- gsub('"', '\\\\"', escaped_text)
              escaped_text <- gsub("\n", "\\\\n", escaped_text)
              escaped_text <- gsub("\r", "\\\\r", escaped_text)

              runjs(paste0('
                var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
                var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

                if (navigator.clipboard && navigator.clipboard.writeText) {
                  navigator.clipboard.writeText("', escaped_text, '").then(function() {
                    console.log("Copying to clipboard was successful!");
                  }, function(err) {
                    console.error("Could not copy text: ", err);
                    copyTextFallback("', escaped_text, '", currentScrollTop, currentScrollLeft);
                  });
                } else {
                  copyTextFallback("', escaped_text, '", currentScrollTop, currentScrollLeft);
                }

                function copyTextFallback(text, scrollTop, scrollLeft) {
                  var textArea = document.createElement("textarea");
                  textArea.value = text;
                  textArea.style.position = "fixed";
                  textArea.style.top = "0";
                  textArea.style.left = "0";
                  textArea.style.width = "2em";
                  textArea.style.height = "2em";
                  textArea.style.padding = "0";
                  textArea.style.border = "none";
                  textArea.style.outline = "none";
                  textArea.style.boxShadow = "none";
                  textArea.style.background = "transparent";
                  textArea.style.opacity = "0";
                  textArea.style.pointerEvents = "none";
                  document.body.appendChild(textArea);
                  textArea.focus();
                  textArea.select();
                  try {
                    var successful = document.execCommand("copy");
                    console.log("Fallback: Copying was " + (successful ? "successful" : "unsuccessful"));
                  } catch (err) {
                    console.log("Fallback: Unable to copy");
                  }
                  document.body.removeChild(textArea);
                  window.scrollTo(scrollLeft, scrollTop);
                }
              '))

              debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 2)
              showNotification(paste("Copied", length(identifier), "identifiers to clipboard"),
                               type = "message", duration = 3)
            } else {
              showNotification("No identifiers found to copy", type = "warning")
            }
          }
        }
      } else {
        showNotification("Please enter some text in the search field first", type = "warning")
      }

    }, error = function(e) {
      debug_log(paste("Error in copyBtn_pca:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error")
    })
  })
}

register_pca_pathway_observers <- function(input, session, res_GSEA, res_GO, debug_log) {
  # ========================================
  # Update Pathway Choices
  # ========================================

  # Update GSEA pathway choices
  observe({
    tryCatch({

      gsea_data <- NULL

      # Check if res_GSEA parameter is available and not NULL
      if (!is.null(res_GSEA) && is.function(res_GSEA)) {
        tryCatch({
          gsea_results <- res_GSEA()

          if (!is.null(gsea_results) && is.list(gsea_results) && "Results" %in% names(gsea_results)) {
            results_obj <- gsea_results$Results

            if (inherits(results_obj, "enrichResult") || inherits(results_obj, "gseaResult")) {
              tryCatch({
                gsea_data <- as.data.frame(results_obj)
                debug_log(paste("GSEA dataframe extracted - rows:", nrow(gsea_data), "cols:", ncol(gsea_data)), 2)
              }, error = function(e) {
                debug_log(paste("Error converting GSEA to dataframe:", e$message), 1)
              })
            } else if (is.data.frame(results_obj)) {
              gsea_data <- results_obj
              debug_log(paste("GSEA dataframe directly available - rows:", nrow(gsea_data)), 2)
            }
          }
        }, error = function(e) {
          debug_log(paste("Error accessing res_GSEA():", e$message), 1)
        })
      }

      # Update dropdown with GSEA pathway names
      if (!is.null(gsea_data) && nrow(gsea_data) > 0) {
        if ("Description" %in% colnames(gsea_data)) {
          pathway_names <- gsea_data$Description
          pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

          if (length(pathway_names) > 0) {
            updateSelectInput(session, "GSEA_pca", choices = pathway_names, selected = NULL)
            debug_log(paste("Updated GSEA dropdown with", length(pathway_names), "pathways"), 1)
          } else {
            debug_log("No valid pathway names after filtering", 1)
          }
        } else {
          debug_log("No valid pathway column found in GSEA data", 1)
        }
      }

    }, error = function(e) {
      debug_log(paste("Critical error in GSEA observer:", e$message), 1)
      # Continue silently - don't crash the app
    })
  })

  # Update GO pathway choices
  observe({
    tryCatch({

      go_data <- NULL

      # Check if res_GO parameter is available and not NULL
      if (!is.null(res_GO) && is.function(res_GO)) {
        tryCatch({
          go_results <- res_GO()

          if (!is.null(go_results) && is.list(go_results) && length(go_results) > 0) {
            # Try to extract from first element (typical GO pattern)
            if (!is.null(go_results[[1]])) {
              go_data_temp <- go_results[[1]]

              # Convert to dataframe if necessary
              if (inherits(go_data_temp, "enrichResult") || inherits(go_data_temp, "gseaResult")) {
                tryCatch({
                  go_data <- as.data.frame(go_data_temp)
                  debug_log(paste("GO dataframe extracted - rows:", nrow(go_data), "cols:", ncol(go_data)), 2)
                }, error = function(e) {
                  debug_log(paste("Error converting GO to dataframe:", e$message), 1)
                })
              } else if (is.data.frame(go_data_temp)) {
                go_data <- go_data_temp
                debug_log(paste("GO dataframe directly available - rows:", nrow(go_data)), 2)
              }
            }
          }
        }, error = function(e) {
          debug_log(paste("Error accessing res_GO():", e$message), 1)
        })
      }

      # Update dropdown with GO pathway names
      if (!is.null(go_data) && nrow(go_data) > 0) {
        if ("Description" %in% colnames(go_data)) {
          pathway_names <- go_data$Description
          pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

          if (length(pathway_names) > 0) {
            updateSelectInput(session, "GO_pca", choices = pathway_names, selected = NULL)
            debug_log(paste("Updated GO dropdown with", length(pathway_names), "pathways"), 1)
          } else {
            debug_log("No valid pathway names after filtering", 1)
          }
        } else {
          debug_log("No valid pathway column found in GO data", 1)
        }
      }

    }, error = function(e) {
      debug_log(paste("Critical error in GO observer:", e$message), 1)
      # Continue silently - don't crash the app
    })
  })

  # ========================================
  # Add Pathway Proteins Observer
  # ========================================

  # Enhanced Protein_Input_pca observer - CORRECTED VERSION
  observeEvent(input$Protein_Input_pca, {
    req(is.function(res_GSEA))
    req(is.function(res_GO))

    tryCatch({
      debug_log("Starting pathway protein transfer", 1)

      result_df <- list()  # Initialize as list for collecting pathways
      pathway_counter <- 0

      # Process GSEA results using res_GSEA parameter - CORRECTED LOGIC
      if (!is.null(res_GSEA) && is.function(res_GSEA)) {
        tryCatch({
          res_GSEA_intern <- res_GSEA()
          GSEA_pca_selected <- isolate(input$GSEA_pca)

          if (!is.null(res_GSEA_intern) && length(GSEA_pca_selected) > 0) {
            debug_log(paste("Processing", length(GSEA_pca_selected), "GSEA pathways"), 2)

            # Access the Results DataFrame - CORRECT STRUCTURE
            result_df_GSEA <- res_GSEA_intern$Results

            if (!is.null(result_df_GSEA)) {

              # Convert to dataframe if it's an S4 object
              if (inherits(result_df_GSEA, "enrichResult") || inherits(result_df_GSEA, "gseaResult")) {
                results_df <- as.data.frame(result_df_GSEA)
              } else if (is.data.frame(result_df_GSEA)) {
                results_df <- result_df_GSEA
              } else {
                debug_log("GSEA Results object has unexpected structure", 1)
                results_df <- NULL
              }

              if (!is.null(results_df) && nrow(results_df) > 0) {

                # Find the correct column for pathway identification
                pathway_col <- if ("Description" %in% colnames(results_df)) {
                  "Description"
                } else if ("ID" %in% colnames(results_df)) {
                  "ID"
                } else {
                  NULL
                }

                if (!is.null(pathway_col)) {
                  # Process EACH selected GSEA pathway separately
                  for (gsea_pathway in GSEA_pca_selected) {
                    tryCatch({
                      pathway_data <- results_df[results_df[[pathway_col]] == gsea_pathway, ]

                      if (nrow(pathway_data) > 0) {
                        pathway_proteins <- character()

                        if (input$CoreEnriched_pca == TRUE) {
                          # Use core enrichment genes
                          if ("core_enrichment" %in% colnames(pathway_data)) {
                            core_genes <- pathway_data$core_enrichment[1]
                            if (!is.na(core_genes) && nzchar(core_genes)) {
                              pathway_proteins <- unlist(strsplit(core_genes, "/"))
                              debug_log(paste("GSEA pathway", gsea_pathway, "- core enriched:", length(pathway_proteins), "proteins"), 2)
                            }
                          }
                        } else {
                          # Use ALL genes from geneSets - MORE COMPLEX ACCESS
                          gene_sets <- NULL

                          # Try different ways to access geneSets
                          tryCatch({
                            if (is.list(res_GSEA_intern) && "geneSets" %in% names(res_GSEA_intern)) {
                              gene_sets <- res_GSEA_intern$geneSets
                            } else if (inherits(result_df_GSEA, "gseaResult") && .hasSlot(result_df_GSEA, "geneSets")) {
                              gene_sets <- slot(result_df_GSEA, "geneSets")
                            }
                          }, error = function(e) {
                            debug_log(paste("Error accessing geneSets for", gsea_pathway, ":", e$message), 1)
                          })

                          if (!is.null(gene_sets) && gsea_pathway %in% names(gene_sets)) {
                            pathway_proteins <- gene_sets[[gsea_pathway]]
                            debug_log(paste("GSEA pathway", gsea_pathway, "- all geneSets:", length(pathway_proteins), "proteins"), 2)
                          } else {
                            # Fallback to core_enrichment
                            if ("core_enrichment" %in% colnames(pathway_data)) {
                              core_genes <- pathway_data$core_enrichment[1]
                              if (!is.na(core_genes) && nzchar(core_genes)) {
                                pathway_proteins <- unlist(strsplit(core_genes, "/"))
                                debug_log(paste("GSEA pathway", gsea_pathway, "- fallback core enriched:", length(pathway_proteins), "proteins"), 2)
                              }
                            }
                          }
                        }

                        # Add this pathway's proteins as separate entry
                        if (length(pathway_proteins) > 0) {
                          pathway_counter <- pathway_counter + 1
                          result_df[[pathway_counter]] <- pathway_proteins
                          debug_log(paste("Added GSEA pathway", gsea_pathway, "with", length(pathway_proteins), "proteins"), 2)
                        }
                      }
                    }, error = function(e) {
                      debug_log(paste("Error processing GSEA pathway", gsea_pathway, ":", e$message), 1)
                    })
                  }
                } else {
                  debug_log("Could not find Description or ID column in GSEA data", 1)
                }
              } else {
                debug_log("GSEA results dataframe is empty or NULL", 1)
              }
            } else {
              debug_log("GSEA Results object is NULL", 1)
            }
          } else {
            debug_log("No GSEA pathways selected or GSEA results not available", 2)
          }
        }, error = function(e) {
          debug_log(paste("Error processing GSEA results:", e$message), 1)
        })
      }

      # Process GO results using res_GO parameter - CORRECTED LOGIC
      if (!is.null(res_GO) && is.function(res_GO)) {
        tryCatch({
          res_GO_temp <- res_GO()
          GO_pca_selected <- isolate(input$GO_pca)

          if (!is.null(res_GO_temp) && length(GO_pca_selected) > 0) {
            debug_log(paste("Processing", length(GO_pca_selected), "GO pathways"), 2)

            # Access GO data structure correctly
            if (is.list(res_GO_temp) && length(res_GO_temp) > 0 && !is.null(res_GO_temp[[1]])) {
              go_data <- res_GO_temp[[1]]

              # Convert to dataframe if needed
              if (inherits(go_data, "enrichResult") || inherits(go_data, "gseaResult")) {
                go_df <- as.data.frame(go_data)
              } else if (is.data.frame(go_data)) {
                go_df <- go_data
              } else {
                go_df <- NULL
              }

              if (!is.null(go_df) && "Description" %in% colnames(go_df) && "geneID" %in% colnames(go_df)) {

                # Process EACH selected GO pathway separately
                for (go_pathway in GO_pca_selected) {
                  tryCatch({
                    # Find the GO term by exact match
                    pathway_row <- go_df[go_df$Description == go_pathway, ]

                    if (nrow(pathway_row) > 0) {
                      gene_ids <- pathway_row$geneID[1]
                      if (!is.na(gene_ids) && nzchar(gene_ids)) {
                        pathway_proteins <- unlist(strsplit(gene_ids, "/"))

                        if (length(pathway_proteins) > 0) {
                          pathway_counter <- pathway_counter + 1
                          result_df[[pathway_counter]] <- pathway_proteins
                          debug_log(paste("Added GO pathway", go_pathway, "with", length(pathway_proteins), "proteins"), 2)
                        }
                      }
                    }
                  }, error = function(e) {
                    debug_log(paste("Error processing GO pathway", go_pathway, ":", e$message), 1)
                  })
                }
              } else {
                debug_log("GO data structure not as expected", 1)
              }
            } else {
              debug_log("res_GO() returned unexpected structure", 1)
            }
          } else {
            debug_log("No GO pathways selected or GO results not available", 2)
          }
        }, error = function(e) {
          debug_log(paste("Error processing GO results:", e$message), 1)
        })
      }

      # Apply intersection or union logic based on checkbox
      proteins_GSEA_GO <- character()
      if (length(result_df) > 0) {
        debug_log(paste("Processing", length(result_df), "pathway result sets"), 2)

        if (input$Intersect_pca == TRUE) {
          # Find intersection of all selected pathways
          if (length(result_df) > 1) {
            proteins_GSEA_GO <- Reduce(intersect, result_df)
            debug_log(paste("Applied intersection logic, resulting in", length(proteins_GSEA_GO), "proteins"), 1)
          } else if (length(result_df) == 1) {
            proteins_GSEA_GO <- unique(result_df[[1]])
            debug_log(paste("Single pathway selected, using all", length(proteins_GSEA_GO), "proteins"), 2)
          }
        } else {
          # Find union of all selected pathways
          result_df_flat <- unlist(result_df)
          proteins_GSEA_GO <- unique(result_df_flat)
          proteins_GSEA_GO <- proteins_GSEA_GO[!is.na(proteins_GSEA_GO) & nzchar(proteins_GSEA_GO)]
          debug_log(paste("Applied union logic, resulting in", length(proteins_GSEA_GO), "proteins"), 1)
        }
      } else {
        debug_log("No pathway data collected", 1)
      }

      # Update text area with pathway proteins
      if (length(proteins_GSEA_GO) > 0) {
        updated_proteins_GSEA_GO <- paste(proteins_GSEA_GO, collapse = "\n")
        updateTextAreaInput(session, "searchGene_pca", value = updated_proteins_GSEA_GO)
        debug_log(paste("Updated text area with", length(proteins_GSEA_GO), "proteins"), 2)

        intersection_status <- if (input$Intersect_pca) "intersecting" else "all unique"
        showNotification(paste("Added", length(proteins_GSEA_GO), intersection_status, "proteins from pathways"),
                         type = "message", duration = 3)
      } else {
        debug_log("No proteins to update in text area", 2)
        if (input$Intersect_pca) {
          showNotification("No proteins found in intersection of all selected pathways", type = "warning")
        } else {
          showNotification("No valid proteins found to add", type = "warning")
        }
      }

      # Clear pathway selections
      updateSelectInput(session, "GSEA_pca", selected = NULL)
      updateSelectInput(session, "GO_pca", selected = NULL)

      debug_log(paste("Pathway protein transfer completed with", length(proteins_GSEA_GO), "proteins"), 1)

    }, error = function(e) {
      debug_log(paste("Error in Protein_Input_pca:", e$message), 1)
      showNotification("Error adding pathway proteins", type = "error")
    })
  })
}

# ==============================================================================
# register_pca_ui_state_observers
# Handles UI synchronization that depends on analysis results or user selection
# of method/target, plus right-side control reset and collapsible panel toggling.
# ==============================================================================
register_pca_ui_state_observers <- function(input, session, rv, state, debug_log) {

  analysis_results          <- state$analysis_results
  plots_ready               <- state$plots_ready
  selected_points_interactive <- state$selected_points_interactive
  labeled_proteins          <- state$labeled_proteins
  protein_suggestions       <- state$protein_suggestions
  static_plot_obj           <- state$static_plot_obj
  interactive_plot_obj      <- state$interactive_plot_obj
  available_components      <- state$available_components
  selected_items_vector_pca <- state$selected_items_vector_pca
  selected_data_pca         <- state$selected_data_pca
  selected_protein_vector_pca <- state$selected_protein_vector_pca
  item_label_settings_pca   <- state$item_label_settings_pca
  sample_labeling_active_pca <- state$sample_labeling_active_pca
  sample_label_settings_pca <- state$sample_label_settings_pca
  scree_plot_obj            <- state$scree_plot_obj
  resetting_ui_controls     <- reactiveVal(FALSE)

  consume_restore_echo <- function(id, value) {
    expected <- isolate(state$expected_restore_input_echoes())
    if (!id %in% names(expected)) return(FALSE)
    saved_value <- expected[[id]]
    expected[[id]] <- NULL
    state$expected_restore_input_echoes(expected)
    identical(unname(saved_value), unname(value))
  }

  # Log analysis method changes for diagnostics
  observeEvent(input$analysis_method, {
    if (isTRUE(isolate(state$restore_in_progress()))) {
      debug_log("[PCA] Skipping analysis method observer during session restore", 2)
      return()
    }
    if (consume_restore_echo("analysis_method", input$analysis_method)) {
      debug_log("[PCA] Consumed restored analysis method echo", 2)
      return()
    }
    debug_log(paste("Analysis method changed to:", input$analysis_method), 1)
    state$debug_stored_results()
  }, ignoreInit = TRUE)

  # Update axis component choices when new analysis results arrive
  observeEvent(analysis_results(), {
    results <- analysis_results()
    req(results)

    executed_method <- results$method %||% input$analysis_method
    axis_info <- manage_axis_choices(executed_method, results, input)

    current_choices_x <- isolate(available_components()$x)
    current_choices_y <- isolate(available_components()$y)

    # During session restore the server-side available_components() may
    # already match (it was pre-set in set_session_state), but the
    # CLIENT-side dropdowns still have the initial HTML choices (e.g.
    # PC1:PC10).  Force-push the correct choices so that, for instance,
    # UMAP axes (Dim1/Dim2) replace the default PCA choices.
    needs_update <- isTRUE(isolate(state$restore_in_progress())) ||
      !identical(axis_info$choices_x, current_choices_x) ||
      !identical(axis_info$choices_y, current_choices_y)

    if (needs_update) {

      available_components(list(x = axis_info$choices_x, y = axis_info$choices_y))

      if (!axis_info$selected_x %in% axis_info$choices_x)
        axis_info$selected_x <- axis_info$choices_x[1]
      if (!axis_info$selected_y %in% axis_info$choices_y)
        axis_info$selected_y <- axis_info$choices_y[1]

      updateSelectInput(session, "axis_x",
                        choices = axis_info$choices_x,
                        selected = axis_info$selected_x)
      updateSelectInput(session, "axis_y",
                        choices = axis_info$choices_y,
                        selected = axis_info$selected_y)

      debug_log("Axis choices updated", 2)
    }
  })

  # Reset right-side control panels to their UI defaults without clearing results
  observeEvent(input$resetButton_PCATab, {
    tryCatch({
      debug_log("Resetting PCA right-side controls to UI defaults", 1)
      if (datawizard_metadata_defer_downstream_choices(rv)) {
        debug_log("Metadata assignment pending; deferring PCA metadata choices", 2)
        return()
      }

      resetting_ui_controls(TRUE)
      session$onFlushed(function() {
        session$onFlushed(function() {
          resetting_ui_controls(FALSE)
        }, once = TRUE)
      }, once = TRUE)

      normalize_content_value <- function(x) {
        x <- trimws(as.character(x))
        x <- gsub("[[:space:]]+", " ", x)
        x
      }

      possible_values <- c(
        "Raw Abundance",
        "Normalized Abundance",
        "Imputed Raw Abundance",
        "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
        "Batch Corrected Raw Abundance",
        "Batch Corrected Normalized Abundance"
      )

      data_def <- rv$data_def
      selected_data_type <- "Normalized Abundance"
      sample_choices <- character(0)

      if (is.data.frame(data_def)) {
        content_col <- get_col(data_def, c("Content"))
        sample_col <- get_col(data_def, c("Sample", "Column"))

        if (!is.null(content_col)) {
          content_values <- normalize_content_value(data_def[[content_col]])
          possible_values_norm <- normalize_content_value(possible_values)
          available_choices <- possible_values[possible_values_norm %in% content_values]

          if (length(available_choices) > 0) {
            selected_data_type <- if ("Normalized Abundance" %in% available_choices) {
              "Normalized Abundance"
            } else if ("Raw Abundance" %in% available_choices) {
              "Raw Abundance"
            } else {
              available_choices[1]
            }

            updateSelectizeInput(session, "custom_col_sel_pca",
                                 choices = available_choices,
                                 selected = selected_data_type)
          } else {
            updateSelectizeInput(session, "custom_col_sel_pca",
                                 choices = character(0),
                                 selected = NULL)
          }

          if (!is.null(sample_col) && length(available_choices) > 0) {
            sample_rows <- which(content_values == normalize_content_value(selected_data_type))
            if (length(sample_rows) > 0) {
              sample_values <- as.character(data_def[[sample_col]][sample_rows])
              sample_values <- sample_values[!is.na(sample_values)]
              sample_values <- trimws(sample_values)
              sample_values <- sample_values[nzchar(sample_values)]
              sample_choices <- unique(sample_values)
            }
          }
        }

        identifier_indices <- if ("Content" %in% names(data_def)) {
          which(grepl("Identifier", data_def$Content, ignore.case = TRUE))
        } else {
          integer(0)
        }

        if (length(identifier_indices) > 0 && "Options" %in% names(data_def)) {
          identifier_choices <- data_def$Options[identifier_indices]
          updateSelectInput(session, "GeneIdentifierColumn_pca",
                            choices = identifier_choices,
                            selected = identifier_choices[1])
        }
      } else {
        updateSelectizeInput(session, "custom_col_sel_pca",
                             choices = possible_values,
                             selected = "Normalized Abundance")
      }

      updateSelectizeInput(session, "select_samples_pca",
                           choices = sample_choices,
                           selected = sample_choices,
                           server = TRUE)

      updateRadioButtons(session, "comparison_target", selected = "samples")
      updateSelectInput(session, "analysis_method", selected = "pca")

      updateSelectInput(session, "axis_x", choices = paste0("PC", 1:10), selected = "PC1")
      updateSelectInput(session, "axis_y", choices = paste0("PC", 1:10), selected = "PC2")
      available_components(list(x = paste0("PC", 1:10), y = paste0("PC", 1:10)))
      updateCheckboxInput(session, "show_scree", value = TRUE)
      updateCheckboxInput(session, "pca_scale", value = TRUE)

      updateNumericInput(session, "umap_neighbors", value = 15)
      updateNumericInput(session, "umap_min_dist", value = 0.1)
      updateSelectInput(session, "umap_metric", selected = "euclidean")

      updateSelectInput(session, "color_palette", selected = "Viridis")
      updateCheckboxInput(session, "reverse_colors", value = TRUE)
      colourpicker::updateColourInput(session, "defaultProteinColor_pca", value = "#3182bd")
      updateSelectInput(session, "plot_theme", selected = "theme_classic")
      updateSelectInput(session, "legend_position", selected = "right")
      updateSliderInput(session, "point_size", value = 3)
      updateNumericInput(session, "AxisTitleSize_PCATab", value = 20)
      updateNumericInput(session, "tickSize_PCATab", value = 18)
      updateNumericInput(session, "LegendTitleSize_PCATab", value = 20)
      updateNumericInput(session, "LegendTextSize_PCATab", value = 18)

      showNotification("PCA controls reset to defaults.", type = "message", duration = 3)
      debug_log("PCA right-side controls reset to UI defaults", 1)
    }, error = function(e) {
      debug_log(paste("Error resetting PCA controls:", e$message), 1)
      showNotification("Error resetting PCA controls", type = "error", duration = 3)
    })
  })

  # Reset state when the comparison target changes
  observeEvent(input$comparison_target, {
    # During session restore, the comparison_target input echo from
    # updateRadioButtons() must NOT clear the restored state.
    if (isTRUE(isolate(state$restore_in_progress()))) {
      debug_log("[PCA] Skipping comparison_target reset during session restore", 2)
      return()
    }

    if (isTRUE(isolate(resetting_ui_controls()))) {
      debug_log("[PCA] Skipping comparison_target reset during UI defaults reset", 2)
      return()
    }

    # Guard against input echo from session restore propagating after the
    # restore_in_progress flag has already been cleared.  When the echoed
    # value matches the value we explicitly pushed via updateRadioButtons
    # during restore, this is not a user-initiated change.
    restored_target <- isolate(state$restored_comparison_target())
    if (!is.null(restored_target) &&
        identical(input$comparison_target, restored_target)) {
      debug_log("[PCA] Skipping comparison_target reset: echo from session restore", 2)
      state$restored_comparison_target(NULL)
      return()
    }
    if (!is.null(restored_target) &&
        !identical(input$comparison_target, restored_target)) {
      # Stale marker from restore cycle: clear it so future user toggles are
      # never interpreted as restore echoes.
      state$restored_comparison_target(NULL)
    }

    tryCatch({
      debug_log(paste("Comparison target changed to:", input$comparison_target), 1)

      analysis_results(NULL)
      plots_ready(FALSE)
      selected_points_interactive(data.frame())
      labeled_proteins(character())
      protein_suggestions(character())
      static_plot_obj(NULL)
      interactive_plot_obj(NULL)
      scree_plot_obj(NULL)
      selected_items_vector_pca(character())
      selected_data_pca(data.frame())
      selected_protein_vector_pca(character())

      item_label_settings_pca(data.frame(
        item_id = character(),
        label_color = character(),
        dot_color = character(),
        use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      if (identical(input$comparison_target, "samples")) {
        sample_labeling_active_pca(TRUE)
        sample_label_settings_pca(list(
          master_label_color = "#000000",
          master_dot_color = "#E0E0E0",
          use_master_dot_color = FALSE,
          max_overlaps = 10,
          label_distance = 0.25,
          line_thickness = 0.5,
          label_size = 8,
          labeled_dot_size = 2,
          active = TRUE
        ))
      } else {
        sample_labeling_active_pca(FALSE)
        sample_label_settings_pca(list())
      }
      available_components(list(x = character(), y = character()))

      updateTextAreaInput(session, "search_proteins", value = "")
      updateColourInput(session, "masterLabelColor_pca", value = "#000000")
      updateColourInput(session, "masterDotColor_pca", value = "#E0E0E0")
      updateCheckboxInput(session, "masterCustomDot_pca", value = FALSE)

      debug_log("All results, plots, and selections cleared due to comparison target change", 1)
      showNotification(
        paste("Switched to", input$comparison_target, "comparison mode. Please run analysis again."),
        type = "message",
        duration = 4
      )

    }, error = function(e) {
      debug_log(paste("Error handling comparison target change:", e$message), 1)
      showNotification("Error switching comparison mode", type = "error")
    })
  }, ignoreInit = TRUE)

}

# ==============================================================================
# register_pca_label_management_observers
# Handles per-item label/dot color controls, master color synchronization,
# item removal with debounce, clipboard copy of selection, and the dynamic
# item color management UI.
# ==============================================================================
register_pca_label_management_observers <- function(input, output, session, ns, state, debug_log) {

  selected_items_vector_pca  <- state$selected_items_vector_pca
  selected_points_interactive <- state$selected_points_interactive
  item_label_settings_pca    <- state$item_label_settings_pca

  consume_restore_echo <- function(id, value) {
    expected <- isolate(state$expected_restore_input_echoes())
    if (!id %in% names(expected)) return(FALSE)
    saved_value <- expected[[id]]
    expected[[id]] <- NULL
    state$expected_restore_input_echoes(expected)
    identical(unname(saved_value), unname(value))
  }

  analysis_results           <- state$analysis_results

  # Copy selected protein names to clipboard
  observeEvent(input$copy_selection, {
    tryCatch({
      selected <- selected_points_interactive()

      if (is.null(selected) || nrow(selected) == 0) {
        showNotification("No proteins selected", type = "warning", duration = 2)
        return()
      }

      clipboard_text <- paste(selected$Name, collapse = "\n")

      escaped_text <- gsub("\\\\", "\\\\\\\\", clipboard_text)
      escaped_text <- gsub('"', '\\\\"', escaped_text)
      escaped_text <- gsub("\n", "\\\\n", escaped_text)
      escaped_text <- gsub("\r", "\\\\r", escaped_text)

      runjs(paste0('
        var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
        var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText("', escaped_text, '").then(function() {
            console.log("Copying to clipboard was successful!");
          }, function() {
            var textArea = document.createElement("textarea");
            textArea.value = "', escaped_text, '";
            textArea.style.position = "fixed";
            textArea.style.left = "-999999px";
            textArea.style.top = "-999999px";
            document.body.appendChild(textArea);
            textArea.focus();
            textArea.select();
            try {
              var successful = document.execCommand("copy");
              console.log("Fallback: Copying was " + (successful ? "successful" : "unsuccessful"));
            } catch (err) {
              console.log("Fallback: Unable to copy");
            }
            document.body.removeChild(textArea);
            window.scrollTo(currentScrollLeft, currentScrollTop);
          });
        }
      '))

      showNotification(paste("Copied", nrow(selected), "proteins to clipboard"),
                       type = "message", duration = 3)
      debug_log(paste("Copied", nrow(selected), "proteins to clipboard"), 2)

    }, error = function(e) {
      debug_log(paste("Error copying selection:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error", duration = 3)
    })
  })

  # Transfer interactive selection to the static plot label list
  observeEvent(input$add_to_labels, {
    tryCatch({
      selected <- selected_points_interactive()

      if (is.null(selected) || nrow(selected) == 0) {
        showNotification("No items selected", type = "warning", duration = 2)
        return()
      }

      updateCheckboxInput(session, "interactive_plot", value = FALSE)

      current_search <- input$search_proteins
      selected_names <- unique(selected$Name)

      if (is.null(current_search) || current_search == "") {
        new_search <- paste(selected_names, collapse = "\n")
      } else {
        existing <- unlist(strsplit(current_search, "[,\n\r;\\s]+"))
        existing <- trimws(existing[existing != ""])
        new_names <- setdiff(selected_names, existing)

        if (length(new_names) > 0) {
          new_search <- paste(c(current_search, new_names), collapse = "\n")
        } else {
          new_search <- current_search
        }
      }

      updateTextAreaInput(session, "search_proteins", value = new_search)
      shinyjs::click("label_proteins")

      showNotification(paste("Added", length(selected_names), "items to labels"),
                       type = "message", duration = 2)

    }, error = function(e) {
      debug_log(paste("Error adding to labels:", e$message), 1)
      showNotification("Error adding to labels", type = "error", duration = 2)
    })
  })

  # Propagate master label color to all individual color inputs
  observeEvent(input$masterLabelColor_pca, {
    tryCatch({
      if (isTRUE(isolate(state$restore_in_progress()))) {
        debug_log("[PCA] Skipping master label color propagation during session restore", 2)
        return()
      }
      if (consume_restore_echo("masterLabelColor_pca", input$masterLabelColor_pca)) {
        debug_log("[PCA] Consumed restored master label color echo", 2)
        return()
      }

      selected_items <- selected_items_vector_pca()
      master_color <- input$masterLabelColor_pca

      if (is.null(selected_items) || length(selected_items) == 0 || is.null(master_color)) return()

      debug_log(paste("Updating all label colors to master color:", master_color), 2)

      for (i in seq_along(selected_items)) {
        updateColourInput(session, paste0("labelColor_pca_", i), value = master_color)
      }

      debug_log(paste("Updated", length(selected_items), "label colors"), 2)

    }, error = function(e) {
      debug_log(paste("Error updating master label color:", e$message), 1)
    })
  })

  # Propagate master dot color to all individual color inputs
  observeEvent(input$masterDotColor_pca, {
    tryCatch({
      if (isTRUE(isolate(state$restore_in_progress()))) {
        debug_log("[PCA] Skipping master dot color propagation during session restore", 2)
        return()
      }
      if (consume_restore_echo("masterDotColor_pca", input$masterDotColor_pca)) {
        debug_log("[PCA] Consumed restored master dot color echo", 2)
        return()
      }

      selected_items <- selected_items_vector_pca()
      master_color <- input$masterDotColor_pca

      if (is.null(selected_items) || length(selected_items) == 0 || is.null(master_color)) return()

      debug_log(paste("Updating all dot colors to master color:", master_color), 2)

      for (i in seq_along(selected_items)) {
        updateColourInput(session, paste0("dotColor_pca_", i), value = master_color)
      }

      debug_log(paste("Updated", length(selected_items), "dot colors"), 2)

    }, error = function(e) {
      debug_log(paste("Error updating master dot color:", e$message), 1)
    })
  })

  # Propagate master custom-dot checkbox to all individual checkboxes
  observeEvent(input$masterCustomDot_pca, {
    tryCatch({
      if (isTRUE(isolate(state$restore_in_progress()))) {
        debug_log("[PCA] Skipping master custom-dot propagation during session restore", 2)
        return()
      }
      if (consume_restore_echo("masterCustomDot_pca", input$masterCustomDot_pca)) {
        debug_log("[PCA] Consumed restored master custom-dot echo", 2)
        return()
      }

      selected_items <- selected_items_vector_pca()
      master_enabled <- input$masterCustomDot_pca

      if (is.null(selected_items) || length(selected_items) == 0 || is.null(master_enabled)) return()

      debug_log(paste("Updating all custom dot checkboxes to:", master_enabled), 2)

      for (i in seq_along(selected_items)) {
        updateCheckboxInput(session, paste0("useDotColor_pca_", i), value = master_enabled)
      }

      debug_log(paste("Updated", length(selected_items), "custom dot checkboxes"), 2)

    }, error = function(e) {
      debug_log(paste("Error updating master custom dot control:", e$message), 1)
    })
  })

  # Collect per-item color settings and store them, then trigger re-render
  observeEvent(input$applySettings_pca, {
    tryCatch({
      req(analysis_results())

      current_identifier <- input$GeneIdentifierColumn_pca
      results <- analysis_results()
      if (!is.null(current_identifier) && !is.null(results)) {
        results$identifier_col <- current_identifier
        analysis_results(results)
        debug_log(paste("Synchronized identifier to:", current_identifier), 2)
      }

      selected_items <- selected_items_vector_pca()
      if (is.null(selected_items) || length(selected_items) == 0) {
        showNotification("No items selected for labeling", type = "warning", duration = 3)
        return()
      }

      debug_log(paste("Applying label & dot settings for", length(selected_items), "items"), 1)

      new_settings <- data.frame(
        item_id = selected_items,
        label_color = character(length(selected_items)),
        dot_color = character(length(selected_items)),
        use_custom_dot_color = logical(length(selected_items)),
        stringsAsFactors = FALSE
      )

      for (i in seq_along(selected_items)) {
        lbl_col <- input[[paste0("labelColor_pca_", i)]] %||% "#000000"
        dot_col <- input[[paste0("dotColor_pca_", i)]]   %||% "#E0E0E0"
        use_dot <- isTRUE(input[[paste0("useDotColor_pca_", i)]])

        new_settings$label_color[i]         <- lbl_col
        new_settings$dot_color[i]           <- dot_col
        new_settings$use_custom_dot_color[i] <- use_dot

        debug_log(paste("Captured settings for", selected_items[i],
                        "- Label:", lbl_col, "| Dot:", dot_col, "| CustomDot:", use_dot), 2)
      }

      item_label_settings_pca(new_settings)
      debug_log(paste("Stored", nrow(new_settings), "item label settings"), 1)

      # Light invalidation to trigger re-render
      analysis_results(analysis_results())

      showNotification(paste("Applied settings for", length(selected_items), "items"),
                       type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error in applySettings_pca:", e$message), 1)
      showNotification("Error applying settings", type = "error", duration = 3)
    })
  })

  # Reset all color settings to defaults
  observeEvent(input$resetColors_pca, {
    tryCatch({
      updateColourInput(session, "masterLabelColor_pca", value = "#000000")
      updateColourInput(session, "masterDotColor_pca", value = "#E0E0E0")
      updateCheckboxInput(session, "masterCustomDot_pca", value = FALSE)

      item_label_settings_pca(data.frame(
        item_id = character(),
        label_color = character(),
        dot_color = character(),
        use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      selected_items <- selected_items_vector_pca()
      if (!is.null(selected_items) && length(selected_items) > 0) {
        for (i in seq_along(selected_items)) {
          updateColourInput(session, paste0("labelColor_pca_", i), value = "#000000")
          updateColourInput(session, paste0("dotColor_pca_", i), value = "#E0E0E0")
          updateCheckboxInput(session, paste0("useDotColor_pca_", i), value = FALSE)
        }
      }

      debug_log("Reset all color settings including master controls", 2)
      showNotification("Reset all color settings to defaults", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error resetting colors:", e$message), 1)
      showNotification("Error resetting color settings", type = "error")
    })
  })

  # Clear label settings while preserving the current selection
  observeEvent(input$clearLabels_pca, {
    tryCatch({
      item_label_settings_pca(data.frame(
        item_id = character(),
        label_color = character(),
        dot_color = character(),
        use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      selected_items <- selected_items_vector_pca()
      if (!is.null(selected_items) && length(selected_items) > 0) {
        for (i in seq_along(selected_items)) {
          updateColourInput(session, paste0("labelColor_pca_", i), value = "#000000")
          updateColourInput(session, paste0("dotColor_pca_", i), value = "#E0E0E0")
          updateCheckboxInput(session, paste0("useDotColor_pca_", i), value = FALSE)
        }
      }

      debug_log("Cleared all labels but kept selection", 2)
      showNotification("Cleared all labels (selection preserved)", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error clearing labels:", e$message), 1)
      showNotification("Error clearing labels", type = "error")
    })
  })

  # Clear both selection and label settings
  observeEvent(input$clearSelection_pca, {
    tryCatch({
      selected_items_vector_pca(character())

      item_label_settings_pca(data.frame(
        item_id = character(),
        label_color = character(),
        dot_color = character(),
        use_custom_dot_color = logical(),
        stringsAsFactors = FALSE
      ))

      debug_log("Cleared all selection and labels", 2)
      showNotification("Cleared all selection and labels", type = "message", duration = 3)

    }, error = function(e) {
      debug_log(paste("Error clearing selection:", e$message), 1)
      showNotification("Error clearing selection", type = "error")
    })
  })

  # Remove individual items from selection via per-row button clicks
  observeEvent(input$remove_item_click_pca, {
    tryCatch({
      item_to_remove <- input$remove_item_click_pca
      if (is.null(item_to_remove) || !nzchar(item_to_remove)) return()

      debug_log(paste("Removing item from PCA:", item_to_remove), 1)

      current_items <- selected_items_vector_pca()
      selected_items_vector_pca(current_items[current_items != item_to_remove])

      current_settings <- item_label_settings_pca()
      if (nrow(current_settings) > 0) {
        item_label_settings_pca(
          current_settings[current_settings$item_id != item_to_remove, ]
        )
      }

      showNotification(paste("Removed", item_to_remove, "from selection"),
                       type = "message", duration = 2)

    }, error = function(e) {
      debug_log(paste("Error removing item:", e$message), 1)
      showNotification("Error removing item from selection", type = "error")
    })
  })

  # Render the per-item label and dot color management panel
  output$enhanced_selectedItems_pca <- renderUI({
    tryCatch({
      selected_items <- selected_items_vector_pca()
      comparison_target <- input$comparison_target

      if (is.null(selected_items) || length(selected_items) == 0) {
        return(div(
          style = "padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f8f9fa; min-height: 120px;",
          p("No proteins selected", style = "color: #666; margin: 0; text-align: center; padding-top: 30px;")
        ))
      }

      debug_log(paste("Generating enhanced UI for", length(selected_items), "items"), 2)

      current_settings  <- item_label_settings_pca()
      default_dot_colors <- get_default_colors_for_items_pca(selected_items, comparison_target)

      item_rows <- lapply(seq_along(selected_items), function(i) {
        item <- selected_items[i]

        existing_row <- current_settings[current_settings$item_id == item, ]

        if (nrow(existing_row) > 0) {
          label_color <- existing_row$label_color[1]
          dot_color   <- existing_row$dot_color[1]
          use_custom  <- existing_row$use_custom_dot_color[1]
        } else {
          label_color <- "#000000"
          dot_color   <- default_dot_colors[i]
          use_custom  <- FALSE
        }

        fluidRow(
          style = "border-bottom: 1px solid #eee; padding: 12px 5px; margin: 0px;",
          column(width = 3,
                 div(style = "padding-top: 10px;",
                     strong(substr(item, 1, 16))
                 )
          ),
          column(width = 3,
                 div(style = "padding: 2px;",
                     colourInput(
                       ns(paste0("labelColor_pca_", i)),
                       "Label:",
                       value = label_color
                     )
                 )
          ),
          column(width = 3,
                 div(style = "padding: 2px;",
                     colourInput(
                       ns(paste0("dotColor_pca_", i)),
                       "Dot:",
                       value = dot_color
                     )
                 )
          ),
          column(width = 2,
                 div(style = "padding-top: 18px;",
                     checkboxInput(
                       ns(paste0("useDotColor_pca_", i)),
                       "",
                       value = use_custom,
                       width = "100%"
                     )
                 )
          ),
          column(width = 1,
                 div(style = "padding-top: 15px; text-align: center;",
                     tags$button(
                       class = "btn btn-danger btn-xs",
                       style = "padding: 1px 6px; font-size: 11px; line-height: 1.4;",
                       onclick = sprintf(
                         "Shiny.setInputValue(\"%s\", \"%s\", {priority: \"event\"});",
                         ns("remove_item_click_pca"),
                         gsub('"', '\\\\"', item, fixed = TRUE)
                       ),
                       icon("times")
                     )
                 )
          )
        )
      })

      tagList(
        fluidRow(
          style = "background-color: #f8f9fa; padding: 12px; margin: 0px; border: 1px solid #ddd; border-bottom: none; font-weight: bold;",
          column(width = 3, if (comparison_target == "proteins") "Protein" else "Sample"),
          column(width = 3, "Label Color"),
          column(width = 3, "Dot Color"),
          column(width = 2, "Custom Dot"),
          column(width = 1, "Remove")
        ),
        div(
          style = "border: 1px solid #ddd; border-top: none; padding: 8px; max-height: 400px; min-height: 200px; overflow-y: auto;",
          item_rows
        )
      )

    }, error = function(e) {
      debug_log(paste("Error generating enhanced UI:", e$message), 1)
      return(div("Error generating item controls"))
    })
  })
  outputOptions(output, "enhanced_selectedItems_pca", suspendWhenHidden = FALSE)
}
