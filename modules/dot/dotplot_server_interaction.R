# ==============================================================================
# dotplot_server_interaction.R - Dotplot interaction observers
#
# Purpose: Hosts interaction observers for plotly selection, protein list updates,
# clipboard workflows, and pathway imports.
#
# Structure:
#   - Plotly click and selection handling
#   - Selection display and copy/clear actions
#   - Protein add/remove/clear and table rendering
#   - GSEA/GO import and suggested identifier helpers
#
# Dependencies: shiny, plotly, DT, shinyjs
# Called by: modDotPlotServer()
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_interaction_observers
# Purpose: Initializes Dotplot interaction observers and outputs.
# Structure:
#   - Section 1: Register plotly selection and click handlers.
#   - Section 2: Manage selected protein displays and clipboard actions.
#   - Section 3: Handle protein transfer/remove/clear and pathway imports.
# Parameters:
#   - input/output/session/ns/dotplot_debug_log: [various] - Standard module dependencies.
#   - rv: [reactivevalues] - Shared app state.
#   - selected_data_dot: [reactiveVal] - Selected protein table storage.
#   - selected_protein_vector_dot: [reactiveVal] - Selected protein identifiers.
#   - selected_points_interactive_dot: [reactiveVal] - Plotly-selected points store.
#   - res_GSEA: [any] - Optional GSEA result object.
#   - res_GO: [any] - Optional GO result object.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
dotplot_init_interaction_observers <- function(input, output, session, ns, dotplot_debug_log, rv,
                                               selected_data_dot, selected_protein_vector_dot,
                                               selected_points_interactive_dot, res_GSEA, res_GO) {
observeEvent(event_data("plotly_selected", source = "dotplot_plot"), {
  dotplot_debug_log("=== DOT PLOT SELECTION EVENT ===", 2)

  selection <- event_data("plotly_selected", source = "dotplot_plot")

  # ------------------------------------------------------------------
  # Robust guard against empty or invalid selection objects
  # Plotly can emit NULL, non-tabular objects, or objects for which
  # nrow() returns NA when no points are selected. In all these cases
  # we simply ignore the event to avoid if(NA) errors.
  # ------------------------------------------------------------------

  # Ignore NULL or non-data.frame selections
  if (is.null(selection) || !is.data.frame(selection)) {
    dotplot_debug_log("Selection is NULL or not a data.frame - ignoring event", 2)
    return()
  }

  # Safely compute number of rows and handle NA
  n_sel <- suppressWarnings(nrow(selection))
  if (is.na(n_sel) || n_sel == 0) {
    dotplot_debug_log("Empty selection (0 rows or NA) - ignoring event", 2)
    return()
  }
  # ------------------------------------------------------------------

  dotplot_debug_log(paste("Processing", n_sel, "selected points"), 2)

  tryCatch({
    selected_identifiers <- c()

    # PRIORITY 1: Use customdata (real identifiers)
    if ("customdata" %in% colnames(selection)) {
      dotplot_debug_log("Using customdata (real identifiers) for selection", 2)
      selected_identifiers <- selection$customdata
      selected_identifiers <- selected_identifiers[!is.na(selected_identifiers)]
      dotplot_debug_log(paste("Found", length(selected_identifiers), "real identifiers"), 2)

      # FALLBACK: Use pointNumber
    } else if ("pointNumber" %in% colnames(selection)) {
      dotplot_debug_log("Fallback: Using pointNumber for selection", 2)
      point_numbers <- selection$pointNumber + 1
      selected_identifiers <- paste0("Point_", point_numbers)
    }

    if (length(selected_identifiers) > 0) {
      selected_data <- data.frame(
        identifier = selected_identifiers,
        stringsAsFactors = FALSE
      )

      selected_points_interactive_dot(selected_data)

      dotplot_debug_log(paste("Stored", nrow(selected_data), "selected identifiers"), 1)
      showNotification(paste("Selected", nrow(selected_data), "proteins"),
                       type = "message", duration = 3)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error processing selection:", e$message), 1)
  })
})

# Click handler with real identifiers
observeEvent(event_data("plotly_click", source = "dotplot_plot"), {
  dotplot_debug_log("=== DOT PLOT CLICK EVENT ===", 2)

  click_event <- event_data("plotly_click", source = "dotplot_plot")

  if (is.null(click_event)) {
    dotplot_debug_log("Click event is null", 2)
    return()
  }

  tryCatch({
    selected_identifier <- NULL

    # PRIORITY 1: Use customdata (real identifier)
    if ("customdata" %in% colnames(click_event) && !is.na(click_event$customdata)) {
      selected_identifier <- click_event$customdata
      dotplot_debug_log(paste("Click selected real identifier:", selected_identifier), 2)

      # FALLBACK: Use pointNumber
    } else if ("pointNumber" %in% colnames(click_event)) {
      point_number <- click_event$pointNumber + 1
      selected_identifier <- paste0("Point_", point_number)
      dotplot_debug_log(paste("Click selected point number:", point_number), 2)
    }

    if (!is.null(selected_identifier)) {
      selected_data <- data.frame(
        identifier = selected_identifier,
        stringsAsFactors = FALSE
      )

      selected_points_interactive_dot(selected_data)

      dotplot_debug_log(paste("Selected identifier via click:", selected_identifier), 1)
      showNotification(paste("Selected:", selected_identifier),
                       type = "message", duration = 2)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error processing click:", e$message), 1)
  })
})

# ========================================
# Observer for Identifier Changes - ADD to dotplot_module.R
# ========================================

# Observer to update plot when identifier changes

output$selected_items_display_dot <- renderText({
  selected <- selected_points_interactive_dot()

  if (is.null(selected) || nrow(selected) == 0) {
    return("No proteins selected")
  }

  paste("Selected", nrow(selected), "proteins")
})

output$selected_items_list_dot <- renderText({
  selected <- selected_points_interactive_dot()

  if (is.null(selected) || nrow(selected) == 0) {
    return("Select proteins in the plot above to see them here...")
  }

  # Show all protein names - use appropriate identifier column
  identifier_col <- if ("identifier" %in% colnames(selected)) "identifier" else "Name"
  paste(selected[[identifier_col]], collapse = "\n")
})

# ========================================
# Interactive Selection Copy/Clear Handlers
# ========================================

# Copy Selection Handler
observeEvent(input$copy_selection_dot, {
  tryCatch({
    selected <- selected_points_interactive_dot()

    if (!is.null(selected) && nrow(selected) > 0) {
      # Get identifiers
      identifier_col <- if ("identifier" %in% colnames(selected)) "identifier" else "Name"
      identifier <- selected[[identifier_col]]

      if (length(identifier) > 0) {
        # Create clipboard text
        clipboard_text <- paste(identifier, collapse = "\n")

        # Escape for JavaScript (exact copy from volcano)
        escaped_text <- gsub("\\\\", "\\\\\\\\", clipboard_text)
        escaped_text <- gsub('"', '\\\\"', escaped_text)
        escaped_text <- gsub("\n", "\\\\n", escaped_text)
        escaped_text <- gsub("\r", "\\\\r", escaped_text)

        # JavaScript clipboard (exact copy from volcano)
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
      } else {
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
      }
    '))

        dotplot_debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 1)
        showNotification(paste("Copied", length(identifier), "identifiers to clipboard"),
                         type = "message", duration = 3)
      } else {
        showNotification("No identifiers found to copy", type = "warning")
      }
    } else {
      showNotification("No proteins selected", type = "warning")
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error in copy_selection_dot:", e$message), 1)
    showNotification("Error copying to clipboard", type = "error")
  })
})

# Clear Selection Handler
observeEvent(input$clear_selection_dot, {
  tryCatch({
    selected_points_interactive_dot(data.frame())
    dotplot_debug_log("Cleared dot plot interactive selection", 1)
    showNotification("Cleared selection", type = "message", duration = 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error clearing dot plot selection:", e$message), 1)
  })
})

output$search_identifier_label_dot <- renderText({

  selected_identifier <-
    input$GeneIdentifierColumn_dot

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

output$geneSymbolList_dot <- renderPrint({
  req(rv$data_mod, rv$data_def)

  quiet_log <- function(...) invisible(NULL)

  # Get current identifier selection (you'll need to add this UI element)
  selected_identifier <- input$GeneIdentifierColumn_dot  # Add this to UI

  if (is.null(selected_identifier) || selected_identifier == "") {
    cat("Please select an identifier type first")
    return()
  }

  data <- rv$data_mod
  identifier <- c()
  gene_symbols_text <- ""

  if (!is.null(input$searchGene_dot) && input$searchGene_dot != "") {
    filter_data <- get_filter_string_dot(input$searchGene_dot, selected_identifier, quiet_log)
    filter_data <- as.vector(filter_data[,1])

    if (length(filter_data) > 0) {
      pattern <- paste(filter_data, collapse = "|")
      identifier <- grep(pattern, data[[selected_identifier]], ignore.case = TRUE, value = TRUE)
      gene_symbols_text <- paste(identifier, collapse = "\n")
    }
  }

  cat(gene_symbols_text)
})

# ========================================
# Update Identifier Choices
# ========================================

observe({
  req(rv$data_def)
  if (datawizard_metadata_defer_downstream_choices(rv)) {
    dotplot_debug_log("Metadata assignment pending; deferring identifier choices", 2)
    return()
  }

  tryCatch({
    data_def <- rv$data_def
    central_identifier_choices <- rv$datawizard_identifier_option_choices %||% character(0)
    identifier_indices <- which(grepl("Identifier", data_def$Content, ignore.case = TRUE))
    if (length(central_identifier_choices) > 0) {
      updateSelectInput(session,"GeneIdentifierColumn_dot",
                        choices = central_identifier_choices, selected = central_identifier_choices[1])

      dotplot_debug_log(paste("Updated identifier choices from Data Wizard:", length(central_identifier_choices)), 2)
    } else if (length(identifier_indices) > 0) {
      identifier_choices <- data_def$Options[identifier_indices]
      updateSelectInput(session,"GeneIdentifierColumn_dot",
                        choices = identifier_choices, selected = identifier_choices[1])

      dotplot_debug_log(paste("Updated identifier choices:", length(identifier_choices)), 2)
    }
  }, error = function(e) {
    dotplot_debug_log(paste("Error updating identifier choices:", e$message), 1)
  })
})


# ========================================
# Add Proteins Observer
# ========================================

observeEvent(input$transferButton_dot, {
  tryCatch({
    req(rv$data_mod, rv$data_def)

    df <- rv$data_mod
    data_def <- rv$data_def
    original_col_order <- colnames(df)
    selected_identifier <- input$GeneIdentifierColumn_dot

    if (is.null(selected_identifier) || selected_identifier == "") {
      showNotification("Please select an identifier type first", type = "warning")
      return()
    }

    if (is.null(input$searchGene_dot) || input$searchGene_dot == "") {
      showNotification("Please enter some proteins in the text area", type = "warning")
      return()
    }

    filter_data <- get_filter_string_dot(input$searchGene_dot, selected_identifier, dotplot_debug_log)

    if (nrow(filter_data) > 0) {
      filter_data <- as.vector(filter_data[,1])

      if (length(filter_data) > 0) {
        # EXACT MATCHING ONLY - use %in% instead of grep
        identifier <- df[[selected_identifier]][df[[selected_identifier]] %in% filter_data]
        identifier <- identifier[!is.na(identifier)]

        if (length(identifier) > 0) {
          df_gene_identifier <- df[df[[selected_identifier]] %in% identifier, original_col_order, drop = FALSE]

          previous_data <- selected_data_dot()
          if (is.null(previous_data)) {
            selected_data_dot(df_gene_identifier)
          } else {
            combined_data <- dplyr::bind_rows(previous_data, df_gene_identifier)
            combined_data <- combined_data[!duplicated(combined_data), ]
            selected_data_dot(combined_data)
          }

          # Update selected protein vector
          if (selected_identifier %in% colnames(selected_data_dot())) {
            selected_protein_vector_temp <- selected_data_dot()[[selected_identifier]]
            selected_protein_vector_dot(selected_protein_vector_temp)
          }

          dotplot_debug_log(paste("Transferred", length(identifier), "proteins to selection (EXACT matches)"), 1)
          showNotification(paste("Added", length(identifier), "proteins to selection (exact matches)"),
                           type = "message", duration = 3)
        } else {
          showNotification("No EXACT matching proteins found in dataset", type = "warning")
        }
      }
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error in transferButton_dot:", e$message), 1)
    showNotification("Error adding proteins to selection", type = "error")
  })
})

# ========================================
# Clear All Proteins Observer
# ========================================

observeEvent(input$clearButton_dot, {
  tryCatch({
    selected_data_dot(NULL)
    selected_protein_vector_dot(NULL)
    dotplot_debug_log("Cleared all selected proteins", 1)
    showNotification("Cleared all selected proteins", type = "message", duration = 2)
  }, error = function(e) {
    dotplot_debug_log(paste("Error in clearButton_dot:", e$message), 1)
  })
})

# ========================================
# Update Pathway Choices
# ========================================

# Track whether dropdowns have ever had producer data.  Plain startup begins
# with NULL producers, so observers should stay silent until a real
# ready -> not-ready transition needs to clear previously populated choices.
has_seen_gsea_results_dot <- reactiveVal(FALSE)
has_seen_go_results_dot <- reactiveVal(FALSE)

# Update GSEA pathway choices
observe({
  req(!is.null(res_GSEA), is.function(res_GSEA))
  gsea_results <- res_GSEA()
  if (is.null(gsea_results)) {
    if (isTRUE(has_seen_gsea_results_dot())) {
      updateSelectInput(session, "GSEA_dot", choices = character(0), selected = character(0))
      has_seen_gsea_results_dot(FALSE)
    }
    req(FALSE)
  }
  has_seen_gsea_results_dot(TRUE)
  tryCatch({
    dotplot_debug_log("GSEA observer checking for results", 2)

    gsea_data <- NULL

    # Check if res_GSEA parameter is available and not NULL
    if (!is.null(gsea_results)) {
      tryCatch({
        dotplot_debug_log("Accessed GSEA results via res_GSEA() function", 2)

        if (!is.null(gsea_results) && is.list(gsea_results) && "Results" %in% names(gsea_results)) {
          results_obj <- gsea_results$Results

          if (inherits(results_obj, "enrichResult") || inherits(results_obj, "gseaResult")) {
            tryCatch({
              gsea_data <- as.data.frame(results_obj)
              dotplot_debug_log(paste("GSEA dataframe extracted - rows:", nrow(gsea_data), "cols:", ncol(gsea_data)), 1)
            }, error = function(e) {
              dotplot_debug_log(paste("Error converting GSEA to dataframe:", e$message), 2)
            })
          } else if (is.data.frame(results_obj)) {
            gsea_data <- results_obj
            dotplot_debug_log(paste("GSEA dataframe directly available - rows:", nrow(gsea_data)), 1)
          }
        }
      }, error = function(e) {
        dotplot_debug_log(paste("Error accessing res_GSEA():", e$message), 2)
      })
    } else {
      dotplot_debug_log("res_GSEA() function not available", 2)
    }

    # Update dropdown with GSEA pathway names
    if (!is.null(gsea_data) && nrow(gsea_data) > 0) {
      if ("Description" %in% colnames(gsea_data)) {
        pathway_names <- gsea_data$Description
        pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

        if (length(pathway_names) > 0) {
          updateSelectInput(session, "GSEA_dot", choices = pathway_names, selected = NULL)
          dotplot_debug_log(paste("Updated GSEA dropdown with", length(pathway_names), "pathways"), 1)
        } else {
          dotplot_debug_log("No valid pathway names after filtering", 1)
        }
      } else {
        dotplot_debug_log("No valid pathway column found in GSEA data", 1)
      }
    } else {
      dotplot_debug_log("No valid GSEA data available for dropdown update", 2)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Critical error in GSEA observer:", e$message), 1)
    # Continue silently - don't crash the app
  })
})

    # Update GO pathway choices
observe({
  req(!is.null(res_GO), is.function(res_GO))
  go_results <- res_GO()
  if (is.null(go_results)) {
    if (isTRUE(has_seen_go_results_dot())) {
      updateSelectInput(session, "GO_dot", choices = character(0), selected = character(0))
      has_seen_go_results_dot(FALSE)
    }
    req(FALSE)
  }
  has_seen_go_results_dot(TRUE)
  tryCatch({
    dotplot_debug_log("GO observer checking for results", 2)

    go_data <- NULL

    # Check if res_GO parameter is available and not NULL
    if (!is.null(go_results)) {
      tryCatch({
        dotplot_debug_log("Accessed GO results via res_GO() function", 2)

        if (!is.null(go_results) && is.list(go_results) && length(go_results) > 0) {
          # Try to extract from first element (typical GO pattern)
          if (!is.null(go_results[[1]])) {
            go_data_temp <- go_results[[1]]

            # Convert to dataframe if necessary
            if (inherits(go_data_temp, "enrichResult") || inherits(go_data_temp, "gseaResult")) {
              tryCatch({
                go_data <- as.data.frame(go_data_temp)
                dotplot_debug_log(paste("GO dataframe extracted - rows:", nrow(go_data), "cols:", ncol(go_data)), 1)
              }, error = function(e) {
                dotplot_debug_log(paste("Error converting GO to dataframe:", e$message), 2)
              })
            } else if (is.data.frame(go_data_temp)) {
              go_data <- go_data_temp
              dotplot_debug_log(paste("GO dataframe directly available - rows:", nrow(go_data)), 1)
            }
          }
        }
      }, error = function(e) {
        dotplot_debug_log(paste("Error accessing res_GO():", e$message), 2)
      })
    } else {
      dotplot_debug_log("res_GO() function not available", 2)
    }

    # Update dropdown with GO pathway names
    if (!is.null(go_data) && nrow(go_data) > 0) {
      if ("Description" %in% colnames(go_data)) {
        pathway_names <- go_data$Description
        pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

        if (length(pathway_names) > 0) {
          updateSelectInput(session, "GO_dot", choices = pathway_names, selected = NULL)
          dotplot_debug_log(paste("Updated GO dropdown with", length(pathway_names), "pathways"), 1)
        } else {
          dotplot_debug_log("No valid pathway names after filtering", 1)
        }
      } else {
        dotplot_debug_log("No valid pathway column found in GO data", 1)
      }
    } else {
      dotplot_debug_log("No valid GO data available for dropdown update", 2)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Critical error in GO observer:", e$message), 1)
    # Continue silently - don't crash the app
  })
})

# ========================================
# Add Pathway Proteins Observer
# ========================================

# Enhanced Protein_Input_dot observer - CORRECTED VERSION
observeEvent(input$Protein_Input_dot, {
  req(is.function(res_GSEA))
  req(is.function(res_GO))

  tryCatch({
    dotplot_debug_log("Starting pathway protein transfer", 1)

    result_df <- list()  # Initialize as list for collecting pathways
    pathway_counter <- 0

    # Process GSEA results using res_GSEA parameter - CORRECTED LOGIC
    if (!is.null(res_GSEA) && is.function(res_GSEA)) {
      tryCatch({
        res_GSEA_intern <- res_GSEA()
        GSEA_dot_selected <- isolate(input$GSEA_dot)

        if (!is.null(res_GSEA_intern) && length(GSEA_dot_selected) > 0) {
          dotplot_debug_log(paste("Processing", length(GSEA_dot_selected), "GSEA pathways"), 2)

          # Access the Results DataFrame - CORRECT STRUCTURE
          result_df_GSEA <- res_GSEA_intern$Results

          if (!is.null(result_df_GSEA)) {

            # Convert to dataframe if it's an S4 object
            if (inherits(result_df_GSEA, "enrichResult") || inherits(result_df_GSEA, "gseaResult")) {
              results_df <- as.data.frame(result_df_GSEA)
            } else if (is.data.frame(result_df_GSEA)) {
              results_df <- result_df_GSEA
            } else {
              dotplot_debug_log("GSEA Results object has unexpected structure", 1)
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
                for (gsea_pathway in GSEA_dot_selected) {
                  tryCatch({
                    pathway_data <- results_df[results_df[[pathway_col]] == gsea_pathway, ]

                    if (nrow(pathway_data) > 0) {
                      pathway_proteins <- character()

                      if (input$CoreEnriched_dot == TRUE) {
                        # Use core enrichment genes
                        if ("core_enrichment" %in% colnames(pathway_data)) {
                          core_genes <- pathway_data$core_enrichment[1]
                          if (!is.na(core_genes) && nzchar(core_genes)) {
                            pathway_proteins <- unlist(strsplit(core_genes, "/"))
                            dotplot_debug_log(paste("GSEA pathway", gsea_pathway, "- core enriched:", length(pathway_proteins), "proteins"), 2)
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
                          dotplot_debug_log(paste("Error accessing geneSets for", gsea_pathway, ":", e$message), 2)
                        })

                        if (!is.null(gene_sets) && gsea_pathway %in% names(gene_sets)) {
                          pathway_proteins <- gene_sets[[gsea_pathway]]
                          dotplot_debug_log(paste("GSEA pathway", gsea_pathway, "- all geneSets:", length(pathway_proteins), "proteins"), 2)
                        } else {
                          # Fallback to core_enrichment
                          if ("core_enrichment" %in% colnames(pathway_data)) {
                            core_genes <- pathway_data$core_enrichment[1]
                            if (!is.na(core_genes) && nzchar(core_genes)) {
                              pathway_proteins <- unlist(strsplit(core_genes, "/"))
                              dotplot_debug_log(paste("GSEA pathway", gsea_pathway, "- fallback core enriched:", length(pathway_proteins), "proteins"), 2)
                            }
                          }
                        }
                      }

                      # Add this pathway's proteins as separate entry
                      if (length(pathway_proteins) > 0) {
                        pathway_counter <- pathway_counter + 1
                        result_df[[pathway_counter]] <- pathway_proteins
                        dotplot_debug_log(paste("Added GSEA pathway", gsea_pathway, "with", length(pathway_proteins), "proteins"), 1)
                      }
                    }
                  }, error = function(e) {
                    dotplot_debug_log(paste("Error processing GSEA pathway", gsea_pathway, ":", e$message), 1)
                  })
                }
              } else {
                dotplot_debug_log("Could not find Description or ID column in GSEA data", 1)
              }
            } else {
              dotplot_debug_log("GSEA results dataframe is empty or NULL", 1)
            }
          } else {
            dotplot_debug_log("GSEA Results object is NULL", 1)
          }
        } else {
          dotplot_debug_log("No GSEA pathways selected or GSEA results not available", 2)
        }
      }, error = function(e) {
        dotplot_debug_log(paste("Error processing GSEA results:", e$message), 1)
      })
    } else {
      dotplot_debug_log("res_GSEA() function not available for pathway processing", 2)
    }

    # Process GO results using res_GO parameter - CORRECTED LOGIC
    if (!is.null(res_GO) && is.function(res_GO)) {
      tryCatch({
        res_GO_temp <- res_GO()
        GO_dot_selected <- isolate(input$GO_dot)

        if (!is.null(res_GO_temp) && length(GO_dot_selected) > 0) {
          dotplot_debug_log(paste("Processing", length(GO_dot_selected), "GO pathways"), 2)

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
              for (go_pathway in GO_dot_selected) {
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
                        dotplot_debug_log(paste("Added GO pathway", go_pathway, "with", length(pathway_proteins), "proteins"), 1)
                      }
                    }
                  }
                }, error = function(e) {
                  dotplot_debug_log(paste("Error processing GO pathway", go_pathway, ":", e$message), 1)
                })
              }
            } else {
              dotplot_debug_log("GO data structure not as expected", 1)
            }
          } else {
            dotplot_debug_log("res_GO() returned unexpected structure", 1)
          }
        } else {
          dotplot_debug_log("No GO pathways selected or GO results not available", 2)
        }
      }, error = function(e) {
        dotplot_debug_log(paste("Error processing GO results:", e$message), 1)
      })
    } else {
      dotplot_debug_log("res_GO() function not available for pathway processing", 2)
    }

    # Apply intersection or union logic based on checkbox
    proteins_GSEA_GO <- character()
    if (length(result_df) > 0) {
      dotplot_debug_log(paste("Processing", length(result_df), "pathway result sets"), 1)

      if (input$Intersect_dot == TRUE) {
        # Find intersection of all selected pathways
        if (length(result_df) > 1) {
          proteins_GSEA_GO <- Reduce(intersect, result_df)
          dotplot_debug_log(paste("Applied intersection logic, resulting in", length(proteins_GSEA_GO), "proteins"), 1)
        } else if (length(result_df) == 1) {
          proteins_GSEA_GO <- unique(result_df[[1]])
          dotplot_debug_log(paste("Single pathway selected, using all", length(proteins_GSEA_GO), "proteins"), 1)
        }
      } else {
        # Find union of all selected pathways
        result_df_flat <- unlist(result_df)
        proteins_GSEA_GO <- unique(result_df_flat)
        proteins_GSEA_GO <- proteins_GSEA_GO[!is.na(proteins_GSEA_GO) & nzchar(proteins_GSEA_GO)]
        dotplot_debug_log(paste("Applied union logic, resulting in", length(proteins_GSEA_GO), "proteins"), 1)
      }
    } else {
      dotplot_debug_log("No pathway data collected", 1)
    }

    # Update text area with pathway proteins
    if (length(proteins_GSEA_GO) > 0) {
      updated_proteins_GSEA_GO <- paste(proteins_GSEA_GO, collapse = "\n")
      updateTextAreaInput(session, "searchGene_dot", value = updated_proteins_GSEA_GO)
      dotplot_debug_log(paste("Updated text area with", length(proteins_GSEA_GO), "proteins"), 1)

      intersection_status <- if (input$Intersect_dot) "intersecting" else "all unique"
      showNotification(paste("Added", length(proteins_GSEA_GO), intersection_status, "proteins from pathways"),
                       type = "message", duration = 3)
    } else {
      dotplot_debug_log("No proteins to update in text area", 1)
      if (input$Intersect_dot) {
        showNotification("No proteins found in intersection of all selected pathways", type = "warning")
      } else {
        showNotification("No valid proteins found to add", type = "warning")
      }
    }

    # Clear pathway selections
    updateSelectInput(session, "GSEA_dot", selected = NULL)
    updateSelectInput(session, "GO_dot", selected = NULL)

    dotplot_debug_log(paste("Pathway protein transfer completed with", length(proteins_GSEA_GO), "proteins"), 1)

  }, error = function(e) {
    dotplot_debug_log(paste("Error in Protein_Input_dot:", e$message), 1)
    showNotification("Error adding pathway proteins", type = "error")
  })
})

# ========================================

observeEvent(input$copyBtn_dot, {
  req(rv$data_mod, rv$data_def)

  tryCatch({
    data <- rv$data_mod
    selected_identifier <- input$GeneIdentifierColumn_dot
    identifier <- c()

    dotplot_debug_log("Copy to clipboard button clicked", 2)

    if (!is.null(input$searchGene_dot) && input$searchGene_dot != "" &&
        !is.null(selected_identifier) && selected_identifier != "") {

      filter_data <- get_filter_string_dot(input$searchGene_dot, selected_identifier, dotplot_debug_log)

      if (nrow(filter_data) > 0) {
        filter_data <- as.vector(filter_data[,1])

        if (length(filter_data) > 0) {
          # PARTIAL MATCHING for suggestions - same logic as display
          all_identifiers <- data[[selected_identifier]]
          all_identifiers <- all_identifiers[!is.na(all_identifiers)]

          # Create pattern for partial matching (any of the input terms)
          pattern <- paste(filter_data, collapse = "|")

          # Find all partial matches
          identifier <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)
          identifier <- unique(identifier)

          if (length(identifier) > 0) {
            # Create text for clipboard
            clipboard_text <- paste(identifier, collapse = "\n")

            # Escape text for JavaScript (more robust escaping)
            escaped_text <- gsub("\\\\", "\\\\\\\\", clipboard_text)
            escaped_text <- gsub('"', '\\\\"', escaped_text)
            escaped_text <- gsub("\n", "\\\\n", escaped_text)
            escaped_text <- gsub("\r", "\\\\r", escaped_text)

            # Use JavaScript to copy to clipboard without scrolling
            runjs(paste0('
          // Store current scroll position
          var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
          var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

          if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText("', escaped_text, '").then(function() {
              console.log("Copying to clipboard was successful!");
            }, function(err) {
              console.error("Could not copy text: ", err);
              // Fallback method without focus/select to prevent scrolling
              copyTextFallback("', escaped_text, '", currentScrollTop, currentScrollLeft);
            });
          } else {
            // Fallback method without focus/select to prevent scrolling
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

            // Restore scroll position
            window.scrollTo(scrollLeft, scrollTop);
          }
        '))

            dotplot_debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 1)
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
    dotplot_debug_log(paste("Error in copyBtn_dot:", e$message), 1)
    showNotification("Error copying to clipboard", type = "error")
  })
})


  invisible(NULL)
}
