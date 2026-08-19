# Observer registration group extracted from volcano_observers.R.
# Reactive state and plot objects are supplied by the module entry point.

register_volcano_protein_selection_observers <- function(
    input, output, session, rv,
    res_GSEA, GO_res, module_outputs,
    volcano_state, plot_update_trigger,
    selected_data_Volcano, selected_protein_vector_Volcano,
    volcano_original_plots, volcano_labels, protein_label_settings,
    selected_points_interactive_Volcano,
    data_in, data_def_in, debug_log, ns, modEnv
) {
  # ============================================================================
  # SECTION 3: GSEA and GO Pathway Integration
  # ============================================================================

  # Update GSEA pathway choices
  observe({
    datawizard_import_ready_signature(rv)
    if (datawizard_import_barrier_active(rv)) {
      debug_log("Import barrier active; preserving Volcano choices until ready", 2)
      return()
    }
    req(!is.null(res_GSEA), is.function(res_GSEA))
    gsea_results <- res_GSEA()
    if (is.null(gsea_results)) {
      updateSelectInput(session, "GSEA_Volcano", choices = character(0), selected = character(0))
      req(FALSE)
    }
    tryCatch({
      debug_log("GSEA observer checking for results", 2)

      gsea_data <- NULL

      if (!is.null(gsea_results)) {
        tryCatch({
          debug_log("Accessed GSEA results via res_GSEA() function", 2)

          if (!is.null(gsea_results) && is.list(gsea_results) && "Results" %in% names(gsea_results)) {
            results_obj <- gsea_results$Results

            if (inherits(results_obj, "enrichResult") || inherits(results_obj, "gseaResult")) {
              tryCatch({
                gsea_data <- as.data.frame(results_obj)
                debug_log(paste("GSEA dataframe extracted - rows:", nrow(gsea_data), "cols:", ncol(gsea_data)), 1)
              }, error = function(e) {
                debug_log(paste("Error converting GSEA to dataframe:", e$message), 1)
              })
            } else if (is.data.frame(results_obj)) {
              gsea_data <- results_obj
              debug_log(paste("GSEA dataframe directly available - rows:", nrow(gsea_data)), 1)
            }
          }
        }, error = function(e) {
          debug_log(paste("Error accessing res_GSEA():", e$message), 1)
        })
      } else {
        debug_log("res_GSEA() function not available", 2)
      }

      if (!is.null(gsea_data) && nrow(gsea_data) > 0) {
        if ("Description" %in% colnames(gsea_data)) {
          pathway_names <- gsea_data$Description
          pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

          if (length(pathway_names) > 0) {
            updateSelectInput(session, "GSEA_Volcano", choices = pathway_names, selected = NULL)
            debug_log(paste("Updated GSEA dropdown with", length(pathway_names), "pathways"), 1)
          }
        }
      }
    }, error = function(e) {
      debug_log(paste("Critical error in GSEA observer:", e$message), 1)
    })
  })

  # Update GO pathway choices
  observe({
    req(!is.null(GO_res), is.function(GO_res))
    go_results <- GO_res()
    if (is.null(go_results)) {
      updateSelectInput(session, "GO_Volcano", choices = character(0), selected = character(0))
      req(FALSE)
    }
    tryCatch({
      debug_log("GO observer checking for results", 2)

      go_data <- NULL

      if (!is.null(go_results)) {
        tryCatch({
          debug_log("Accessed GO results via GO_res() function", 2)

          if (!is.null(go_results) && is.list(go_results) && length(go_results) > 0) {
            if (!is.null(go_results[[1]])) {
              go_data_temp <- go_results[[1]]

              if (inherits(go_data_temp, "enrichResult") || inherits(go_data_temp, "gseaResult")) {
                tryCatch({
                  go_data <- as.data.frame(go_data_temp)
                  debug_log(paste("GO dataframe extracted - rows:", nrow(go_data), "cols:", ncol(go_data)), 1)
                }, error = function(e) {
                  debug_log(paste("Error converting GO to dataframe:", e$message), 1)
                })
              } else if (is.data.frame(go_data_temp)) {
                go_data <- go_data_temp
                debug_log(paste("GO dataframe directly available - rows:", nrow(go_data)), 1)
              }
            }
          }
        }, error = function(e) {
          debug_log(paste("Error accessing GO_res():", e$message), 1)
        })
      } else {
        debug_log("GO_res() function not available", 2)
      }

      if (!is.null(go_data) && nrow(go_data) > 0) {
        if ("Description" %in% colnames(go_data)) {
          pathway_names <- go_data$Description
          pathway_names <- pathway_names[!is.na(pathway_names) & nzchar(trimws(pathway_names))]

          if (length(pathway_names) > 0) {
            updateSelectInput(session, "GO_Volcano", choices = pathway_names, selected = NULL)
            debug_log(paste("Updated GO dropdown with", length(pathway_names), "pathways"), 1)
          }
        }
      }
    }, error = function(e) {
      debug_log(paste("Critical error in GO observer:", e$message), 1)
    })
  })

  # Pathway protein transfer
  observeEvent(input$Protein_Input_Volcano, {
    req(is.function(res_GSEA))
    req(is.function(GO_res))

    tryCatch({
      debug_log("Starting pathway protein transfer", 1)

      result_df <- list()
      pathway_counter <- 0

      # Process GSEA results
      if (!is.null(res_GSEA) && is.function(res_GSEA)) {
        tryCatch({
          res_GSEA_intern <- res_GSEA()
          GSEA_Volcano_selected <- input$GSEA_Volcano

          if (!is.null(res_GSEA_intern) && length(GSEA_Volcano_selected) > 0) {
            debug_log(paste("Processing", length(GSEA_Volcano_selected), "GSEA pathways"), 2)
            result_df_GSEA <- res_GSEA_intern$Results

            if (!is.null(result_df_GSEA)) {
              if (inherits(result_df_GSEA, "enrichResult") || inherits(result_df_GSEA, "gseaResult")) {
                results_df <- as.data.frame(result_df_GSEA)
              } else if (is.data.frame(result_df_GSEA)) {
                results_df <- result_df_GSEA
              } else {
                results_df <- NULL
              }

              if (!is.null(results_df) && nrow(results_df) > 0) {
                pathway_col <- if ("Description" %in% colnames(results_df)) "Description"
                               else if ("ID" %in% colnames(results_df)) "ID"
                               else NULL

                if (!is.null(pathway_col)) {
                  for (gsea_pathway in GSEA_Volcano_selected) {
                    tryCatch({
                      pathway_data <- results_df[results_df[[pathway_col]] == gsea_pathway, ]

                      if (nrow(pathway_data) > 0) {
                        pathway_proteins <- character()

                        if (input$CoreEnriched_Volcano == TRUE) {
                          if ("core_enrichment" %in% colnames(pathway_data)) {
                            core_genes <- pathway_data$core_enrichment[1]
                            if (!is.na(core_genes) && nzchar(core_genes)) {
                              pathway_proteins <- unlist(strsplit(core_genes, "/"))
                            }
                          }
                        } else {
                          gene_sets <- NULL
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
                          } else if ("core_enrichment" %in% colnames(pathway_data)) {
                            core_genes <- pathway_data$core_enrichment[1]
                            if (!is.na(core_genes) && nzchar(core_genes)) {
                              pathway_proteins <- unlist(strsplit(core_genes, "/"))
                            }
                          }
                        }

                        if (length(pathway_proteins) > 0) {
                          pathway_counter <- pathway_counter + 1
                          result_df[[pathway_counter]] <- pathway_proteins
                          debug_log(paste("Added GSEA pathway", gsea_pathway, "with", length(pathway_proteins), "proteins"), 1)
                        }
                      }
                    }, error = function(e) {
                      debug_log(paste("Error processing GSEA pathway", gsea_pathway, ":", e$message), 1)
                    })
                  }
                }
              }
            }
          }
        }, error = function(e) {
          debug_log(paste("Error processing GSEA results:", e$message), 1)
        })
      }

      # Process GO results
      if (!is.null(GO_res) && is.function(GO_res)) {
        tryCatch({
          GO_res_temp <- GO_res()
          GO_Volcano_selected <- input$GO_Volcano

          if (!is.null(GO_res_temp) && length(GO_Volcano_selected) > 0) {
            debug_log(paste("Processing", length(GO_Volcano_selected), "GO pathways"), 2)

            if (is.list(GO_res_temp) && length(GO_res_temp) > 0 && !is.null(GO_res_temp[[1]])) {
              go_data <- GO_res_temp[[1]]

              if (inherits(go_data, "enrichResult") || inherits(go_data, "gseaResult")) {
                go_df <- as.data.frame(go_data)
              } else if (is.data.frame(go_data)) {
                go_df <- go_data
              } else {
                go_df <- NULL
              }

              if (!is.null(go_df) && "Description" %in% colnames(go_df) && "geneID" %in% colnames(go_df)) {
                for (go_pathway in GO_Volcano_selected) {
                  tryCatch({
                    pathway_row <- go_df[go_df$Description == go_pathway, ]

                    if (nrow(pathway_row) > 0) {
                      gene_ids <- pathway_row$geneID[1]
                      if (!is.na(gene_ids) && nzchar(gene_ids)) {
                        pathway_proteins <- unlist(strsplit(gene_ids, "/"))

                        if (length(pathway_proteins) > 0) {
                          pathway_counter <- pathway_counter + 1
                          result_df[[pathway_counter]] <- pathway_proteins
                          debug_log(paste("Added GO pathway", go_pathway, "with", length(pathway_proteins), "proteins"), 1)
                        }
                      }
                    }
                  }, error = function(e) {
                    debug_log(paste("Error processing GO pathway", go_pathway, ":", e$message), 1)
                  })
                }
              }
            }
          }
        }, error = function(e) {
          debug_log(paste("Error processing GO results:", e$message), 1)
        })
      }

      # Apply intersection or union logic
      proteins <- character()
      if (length(result_df) > 0) {
        if (input$Intersect_Volcano == TRUE) {
          if (length(result_df) > 1) {
            proteins <- Reduce(intersect, result_df)
          } else {
            proteins <- unique(result_df[[1]])
          }
        } else {
          proteins <- unique(unlist(result_df))
          proteins <- proteins[!is.na(proteins) & nzchar(proteins)]
        }
        debug_log(paste("Pathway result:", length(proteins), "proteins"), 1)
      }

      if (length(proteins) > 0) {
        updateTextAreaInput(session, "searchGene_Volcano", value = paste(proteins, collapse = "\n"))
        intersection_status <- if (input$Intersect_Volcano) "intersecting" else "all unique"
        showNotification(paste("Added", length(proteins), intersection_status, "proteins from pathways"),
                         type = "message", duration = 3)
      } else {
        if (input$Intersect_Volcano) {
          showNotification("No proteins found in intersection of all selected pathways", type = "warning")
        } else {
          showNotification("No valid proteins found to add", type = "warning")
        }
      }

      updateSelectInput(session, "GSEA_Volcano", selected = NULL)
      updateSelectInput(session, "GO_Volcano", selected = NULL)

      debug_log(paste("Pathway protein transfer completed with", length(proteins), "proteins"), 1)
    }, error = function(e) {
      debug_log(paste("Error in Protein_Input_Volcano:", e$message), 1)
      showNotification("Error adding pathway proteins", type = "error")
    })
  })

  # ============================================================================
  # SECTION 4: Clipboard and Protein Selection
  # ============================================================================

  # Copy suggested identifiers to clipboard
  observeEvent(input$copyBtn_Volcano, {
    tryCatch({
      req(rv$data_mod, rv$data_def)

      data <- rv$data_mod
      selected_identifier <- input$Identifier_Volcano
      identifier <- c()

      debug_log("Copy to clipboard button clicked", 2)

      if (!is.null(input$searchGene_Volcano) && input$searchGene_Volcano != "" &&
          !is.null(selected_identifier) && selected_identifier != "") {

        filter_data <- get_filter_string_Volcano(input$searchGene_Volcano, selected_identifier, debug_log)

        if (nrow(filter_data) > 0) {
          filter_data <- as.vector(filter_data[,1])

          if (length(filter_data) > 0) {
            all_identifiers <- data[[selected_identifier]]
            all_identifiers <- all_identifiers[!is.na(all_identifiers)]
            pattern <- paste(filter_data, collapse = "|")
            identifier <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)
            identifier <- unique(identifier)

            if (length(identifier) > 0) {
              clipboard_text <- paste(identifier, collapse = "\n")
              copy_to_clipboard(clipboard_text, debug_log)
              debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 1)
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
      debug_log(paste("Error in copyBtn_Volcano:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error")
    })
  })

  # Add proteins to selection
  observeEvent(input$transferButton_Volcano, {
    tryCatch({
      req(rv$data_mod, rv$data_def)

      df <- rv$data_mod
      data_def <- rv$data_def
      original_col_order <- colnames(df)
      selected_identifier <- input$Identifier_Volcano

      if (is.null(selected_identifier) || selected_identifier == "") {
        showNotification("Please select an identifier type first", type = "warning")
        return()
      }

      if (is.null(input$searchGene_Volcano) || input$searchGene_Volcano == "") {
        showNotification("Please enter some proteins in the text area", type = "warning")
        return()
      }

      filter_data <- get_filter_string_Volcano(input$searchGene_Volcano, selected_identifier, debug_log)

      if (nrow(filter_data) > 0) {
        filter_data <- as.vector(filter_data[,1])

        if (length(filter_data) > 0) {
          identifier <- df[[selected_identifier]][df[[selected_identifier]] %in% filter_data]
          identifier <- identifier[!is.na(identifier)]

          if (length(identifier) > 0) {
            df_gene_identifier <- df[df[[selected_identifier]] %in% identifier, original_col_order, drop = FALSE]

            previous_data <- selected_data_Volcano()
            if (is.null(previous_data)) {
              selected_data_Volcano(df_gene_identifier)
            } else {
              combined_data <- dplyr::bind_rows(previous_data, df_gene_identifier)
              combined_data <- combined_data[!duplicated(combined_data), ]
              selected_data_Volcano(combined_data)
            }

            if (selected_identifier %in% colnames(selected_data_Volcano())) {
              selected_protein_vector_Volcano(selected_data_Volcano()[[selected_identifier]])
            }

            debug_log(paste("Transferred", length(identifier), "proteins to selection (EXACT matches)"), 1)
            showNotification(paste("Added", length(identifier), "proteins to selection (exact matches)"),
                             type = "message", duration = 3)
          } else {
            showNotification("No EXACT matching proteins found in dataset", type = "warning")
          }
        }
      }
    }, error = function(e) {
      debug_log(paste("Error in transferButton_Volcano:", e$message), 1)
      showNotification("Error adding proteins to selection", type = "error")
    })
  })

  # Selected proteins table
  output$selectedGene_Volcano <- renderDataTable({
    tryCatch({
      selected_data <- selected_data_Volcano()

      if (is.null(selected_data) || !is.data.frame(selected_data) || nrow(selected_data) == 0) {
        return(DT::datatable(data.frame(Info = "No proteins selected"),
                             options = list(dom = 't'), rownames = FALSE))
      }

      selected_protein_vector_display <- selected_protein_vector_Volcano()

      if (!is.null(selected_protein_vector_display) && length(selected_protein_vector_display) > 0) {
        labeled_df <- data.frame(
          "Identifier" = selected_protein_vector_display,
          stringsAsFactors = FALSE
        )

        return(DT::datatable(
          labeled_df,
          options = list(paging = FALSE, searching = FALSE, info = FALSE,
                         autoWidth = FALSE, scrollY = "120px", dom = 't'),
          rownames = FALSE
        ))
      } else {
        return(DT::datatable(data.frame(Info = "No proteins selected"),
                             options = list(dom = 't'), rownames = FALSE))
      }
    }, error = function(e) {
      debug_log(paste("Error in selectedGene_Volcano table:", e$message), 1)
      return(DT::datatable(data.frame(Error = "Table display error"),
                           options = list(dom = 't'), rownames = FALSE))
    })
  })


}
