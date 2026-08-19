# ==============================================================================
# STRING Module - Data Sources and Protein Selection
# ==============================================================================
#
# Purpose:
#   Bridges the STRING module with external data sources (GSEA and GO module
#   outputs) and manages the protein selection/input workflow. Provides
#   reactive data access functions for GSEA and GO results, registers
#   dropdown update observers for pathway selection, and manages all protein
#   input list operations (transfer, remove, clear, and pathway-to-input).
#
# Architecture role:
#   Factory function initialized inside modSTRINGServer. All observers
#   registered here operate on the protein input field and the selected
#   protein list that feeds into the network build step.
#
# File structure:
#   1. Factory function signature: initialize_STRING_data_sources
#   2. Internal: get_gsea_results_payload, extract_gsea_dataframe, get_gsea_data
#   3. Internal: get_go_data
#   4. register_data_source_observers: GSEA and GO dropdown update observers
#   5. register_selection_observers:
#      a. Identifier type UI update observer
#      b. transferButton_STRING observer
#      c. removeButton_STRING observer
#      d. clearButton_STRING observer
#      e. Protein_Input_STRING observer (pathways to input)
#   6. Return list
#
# Future developers:
#   - register_data_source_observers and register_selection_observers are
#     separate because one deals with external module data and the other with
#     local input management. Both are called from modSTRINGServer.
#   - The Protein_Input_STRING observer references get_gsea_results_payload and
#     get_go_data from the same factory closure; do not extract them.
#   - All logging uses debug_log passed as a factory parameter.
# ==============================================================================

initialize_STRING_data_sources <- function(
    input,
    session,
    debug_log,
    module_outputs = NULL,
    res_GSEA = NULL,
    GO_res = NULL,
    update_gsea_dropdown_choices,
    update_go_dropdown_choices,
    rv = NULL,
    selected_data_STRING = NULL,
    selected_protein_vector = NULL,
    test_g = NULL,
    ceb3_combined = NULL) {

  get_gsea_results_payload <- function() {
    gsea_results <- NULL

    if (!is.null(module_outputs) && !is.null(module_outputs$gsea_out)) {
      gsea_out <- module_outputs$gsea_out

      if ("has_results" %in% names(gsea_out)) {
        tryCatch({
          has_results <- gsea_out$has_results()
          debug_log(paste("GSEA has_results():", has_results), 2)

          if (has_results) {
            if ("get_results" %in% names(gsea_out)) {
              tryCatch({
                gsea_results <- gsea_out$get_results()
                debug_log(paste("GSEA get_results() returned class:", class(gsea_results)), 3)
              }, error = function(e) {
                debug_log(paste("Error accessing GSEA get_results():", e$message), 1)
              })
            } else {
              debug_log("get_results function not found in gsea_out", 1)
            }
          }
        }, error = function(e) {
          debug_log(paste("Error checking GSEA has_results():", e$message), 1)
        })
      } else {
        debug_log("has_results function not found in gsea_out", 2)
      }
    } else if (!is.null(res_GSEA)) {
      legacy_gsea_results <- tryCatch({
        res_GSEA()
      }, error = function(e) {
        debug_log(paste("Error processing legacy GSEA data:", e$message), 1)
        NULL
      })

      if (!is.null(legacy_gsea_results)) {
        debug_log("Using legacy res_GSEA parameter", 2)
        gsea_results <- legacy_gsea_results
      }
    } else {
      debug_log("No GSEA data source available", 2)
    }

    gsea_results
  }

  extract_gsea_dataframe <- function(gsea_results) {
    if (is.null(gsea_results) || !is.list(gsea_results)) {
      return(NULL)
    }

    debug_log(paste("GSEA results structure:", paste(names(gsea_results), collapse = ", ")), 3)

    if (!("Results" %in% names(gsea_results))) {
      return(NULL)
    }

    results_obj <- gsea_results$Results

    if (inherits(results_obj, "enrichResult") || inherits(results_obj, "gseaResult")) {
      result_df <- tryCatch(
        as.data.frame(results_obj),
        error = function(e) {
          debug_log(paste("Error converting GSEA to dataframe:", e$message), 1)
          NULL
        }
      )

      if (!is.null(result_df)) {
        debug_log(paste("GSEA dataframe extracted - rows:", nrow(result_df), "cols:", ncol(result_df)), 1)
      }

      return(result_df)
    }

    if (is.data.frame(results_obj)) {
      debug_log("Legacy GSEA data used directly", 1)
      return(results_obj)
    }

    debug_log(paste("Unexpected GSEA results object class:", class(results_obj)), 1)
    NULL
  }

  get_gsea_data <- function() {
    gsea_results <- get_gsea_results_payload()
    gsea_data <- extract_gsea_dataframe(gsea_results)

    if (!is.null(gsea_data) && nrow(gsea_data) > 0) {
      if ("Description" %in% colnames(gsea_data)) {
        debug_log("GSEA data extracted successfully", 1)
      } else if ("ID" %in% colnames(gsea_data)) {
        debug_log("GSEA data extracted using ID column", 1)
      } else {
        debug_log(paste("GSEA dataframe columns:", paste(colnames(gsea_data), collapse = ", ")), 1)
      }
    }

    gsea_data
  }

  get_go_data <- function() {
    go_data <- NULL

    if (!is.null(module_outputs) && !is.null(module_outputs$go_out)) {
      go_out <- module_outputs$go_out

      if ("has_results" %in% names(go_out)) {
        tryCatch({
          has_results <- go_out$has_results()
          debug_log(paste("GO has_results():", has_results), 2)

          if (has_results) {
            if ("get_results" %in% names(go_out)) {
              tryCatch({
                go_results <- go_out$get_results()
                debug_log(paste("GO get_results() returned class:", class(go_results)), 2)

                if (!is.null(go_results) && is.list(go_results) && length(go_results) > 0 && !is.null(go_results[[1]])) {
                  go_data_temp <- go_results[[1]]

                  if (inherits(go_data_temp, "enrichResult") || inherits(go_data_temp, "gseaResult")) {
                    go_data <- tryCatch(
                      as.data.frame(go_data_temp),
                      error = function(e) {
                        debug_log(paste("Error converting GO to dataframe:", e$message), 2)
                        NULL
                      }
                    )
                    if (!is.null(go_data)) {
                      debug_log(paste("GO dataframe extracted - rows:", nrow(go_data), "cols:", ncol(go_data)), 1)
                    }
                  } else if (is.data.frame(go_data_temp)) {
                    go_data <- go_data_temp
                    debug_log("GO data is already a dataframe", 2)
                  }
                }
              }, error = function(e) {
                debug_log(paste("Error accessing GO get_results():", e$message), 1)
              })
            } else {
              debug_log("get_results function not found in go_out", 1)
            }
          }
        }, error = function(e) {
          debug_log(paste("Error checking GO has_results():", e$message), 1)
        })
      } else {
        debug_log("has_results function not found in go_out", 2)
      }
    } else if (!is.null(GO_res) && !is.null(GO_res())) {
      debug_log("Using legacy GO_res parameter", 2)
      go_results <- GO_res()
      if (length(go_results) > 0 && !is.null(go_results[[1]])) {
        go_data <- go_results[[1]]
      }
    } else {
      debug_log("No GO data source available", 2)
    }

    go_data
  }

  has_seen_gsea_results <- reactiveVal(FALSE)
  has_seen_go_results <- reactiveVal(FALSE)
  last_string_gsea_dropdown_signature <- reactiveVal(NULL)
  last_string_identifier_choice_signature <- reactiveVal(NULL)

  get_gsea_dropdown_signature <- function(gsea_data) {
    if (is.null(gsea_data) || !is.data.frame(gsea_data)) {
      return(list(row_count = 0L, pathways = character(0)))
    }

    pathway_values <- character(0)
    if ("Description" %in% colnames(gsea_data)) {
      pathway_values <- as.character(gsea_data$Description)
    } else if ("ID" %in% colnames(gsea_data)) {
      pathway_values <- as.character(gsea_data$ID)
    }

    list(
      row_count = nrow(gsea_data),
      pathways = pathway_values
    )
  }

  gsea_results_available <- function() {
    if (!is.null(module_outputs) && !is.null(module_outputs$gsea_out) &&
        "has_results" %in% names(module_outputs$gsea_out) &&
        is.function(module_outputs$gsea_out$has_results)) {
      return(isTRUE(tryCatch(module_outputs$gsea_out$has_results(), error = function(e) FALSE)))
    }

    if (!is.null(res_GSEA)) {
      legacy_gsea_results <- tryCatch(res_GSEA(), error = function(e) NULL)
      return(!is.null(legacy_gsea_results))
    }

    FALSE
  }

  go_results_available <- function() {
    if (!is.null(module_outputs) && !is.null(module_outputs$go_out) &&
        "has_results" %in% names(module_outputs$go_out) &&
        is.function(module_outputs$go_out$has_results)) {
      return(isTRUE(module_outputs$go_out$has_results()))
    }

    if (!is.null(GO_res)) {
      return(!is.null(GO_res()))
    }

    FALSE
  }

  register_data_source_observers <- function() {
    observe({
      tryCatch({
        if (!gsea_results_available()) {
          if (isTRUE(has_seen_gsea_results())) {
            update_gsea_dropdown_choices(NULL)
            has_seen_gsea_results(FALSE)
            last_string_gsea_dropdown_signature(NULL)
          }
          return(invisible(NULL))
        }

        has_seen_gsea_results(TRUE)
        debug_log("GSEA observer checking for results", 3)
        gsea_data <- get_gsea_data()
        gsea_dropdown_signature <- get_gsea_dropdown_signature(gsea_data)

        if (identical(gsea_dropdown_signature, last_string_gsea_dropdown_signature())) {
          return(invisible(NULL))
        }

        last_string_gsea_dropdown_signature(gsea_dropdown_signature)
        update_gsea_dropdown_choices(gsea_data)
        debug_log(sprintf("STRING GSEA dropdown updated: %d pathways", gsea_dropdown_signature$row_count), 1)
      }, error = function(e) {
        debug_log(paste("Critical error in GSEA observer:", e$message), 1)
      })
    })

    observe({
      tryCatch({
        if (!go_results_available()) {
          if (isTRUE(has_seen_go_results())) {
            update_go_dropdown_choices(NULL)
            has_seen_go_results(FALSE)
          }
          return(invisible(NULL))
        }

        has_seen_go_results(TRUE)
        debug_log("GO observer checking for results", 2)
        go_data <- get_go_data()
        update_go_dropdown_choices(go_data)
      }, error = function(e) {
        debug_log(paste("Critical error in GO observer:", e$message), 1)
      })
    })
  }

  register_selection_observers <- function() {
    # Update identifier dropdown when data changes
    observe({
      req(rv$data_mod)
      req(rv$data_def)

      data_def <- rv$data_def
      central_identifier_choices <- rv$datawizard_identifier_option_choices %||% character(0)
      identifier_choice_signature <- list(
        content = data_def$Content,
        options = data_def$Options,
        central_choices = central_identifier_choices
      )

      if (datawizard_metadata_defer_downstream_choices(rv)) {
        debug_log("Metadata assignment pending; deferring STRING identifier choices", 2)
        return()
      }

      if (identical(identifier_choice_signature, last_string_identifier_choice_signature())) {
        return()
      }

      last_string_identifier_choice_signature(identifier_choice_signature)
      Identifier_Index <- which(grepl("Identifier", data_def$Content))

      if (length(central_identifier_choices) > 0) {
        updateSelectInput(session, "identifier_type_STRING",
                          choices = central_identifier_choices,
                          selected = central_identifier_choices[1])
        debug_log(paste("Updated identifier choices from Data Wizard with", length(central_identifier_choices), "options"), 1)
      } else if (length(Identifier_Index) > 0) {
        Identifier <- data_def$Options[Identifier_Index]
        updateSelectInput(session, "identifier_type_STRING",
                          choices = Identifier,
                          selected = Identifier[1])
        debug_log(paste("Updated identifier choices with", length(Identifier), "options"), 1)
      }
    })

    # Transfer proteins to selection
    observeEvent(input$transferButton_STRING, {
      tryCatch({
        debug_log("Transfer button clicked", 2)

        selected_identifier <- input$identifier_type_STRING

        identifier_label <- if (!is.null(selected_identifier) && nzchar(selected_identifier)) {
          selected_identifier
        } else {
          "Identifier"
        }

        if (is.null(input$input_STRING) || input$input_STRING == "") {
          showNotification("Please enter some proteins in the text area", type = "warning")
          return()
        }

        filter_data <- get_filter_string_STRING(input$input_STRING, identifier_label, debug_log)

        if (nrow(filter_data) == 0) {
          showNotification("No valid proteins found in input", type = "warning")
          return()
        }

        filter_data <- as.vector(filter_data[, 1])

        if (length(filter_data) > 0) {
          previous_data <- selected_data_STRING()
          existing_ids <- character(0)
          if (!is.null(previous_data)) {
            if (is.data.frame(previous_data)) {
              if (identifier_label %in% names(previous_data)) {
                existing_ids <- as.character(previous_data[[identifier_label]])
              } else if (ncol(previous_data) > 0) {
                existing_ids <- as.character(previous_data[[1]])
              }
            }
          } else if (!is.null(selected_protein_vector())) {
            existing_ids <- as.character(selected_protein_vector())
          }

          new_ids <- setdiff(as.character(filter_data), existing_ids)
          if (length(new_ids) == 0) {
            showNotification("All listed proteins are already selected", type = "message", duration = 3)
            return()
          }

          df_gene_identifier <- data.frame(setNames(list(new_ids), identifier_label),
                                           stringsAsFactors = FALSE)
          if (is.null(previous_data) || !is.data.frame(previous_data)) {
            selected_data_STRING(df_gene_identifier)
          } else {
            if (!(identifier_label %in% names(previous_data))) {
              names(previous_data)[1] <- identifier_label
            }
            combined_data <- dplyr::bind_rows(previous_data, df_gene_identifier)
            combined_data <- combined_data[!duplicated(combined_data[[identifier_label]]), , drop = FALSE]
            selected_data_STRING(combined_data)
          }
          selected_protein_vector(unique(c(existing_ids, new_ids)))
          debug_log(paste("Transferred", length(new_ids), "entered identifiers to selection; STRING mapping will validate them during network creation"), 1)
          showNotification(paste("Added", length(new_ids), "identifiers to selection; STRING mapping will validate them during network creation"),
                           type = "message", duration = 3)
        }

      }, error = function(e) {
        debug_log(paste("Error in transferButton_STRING:", e$message), 1)
        showNotification("Error adding proteins to selection", type = "error")
      })
    })

    # Clear all selected proteins
    observeEvent(input$clearButton_STRING, {
      tryCatch({
        selected_data_STRING(NULL)
        selected_protein_vector(NULL)
        debug_log("Cleared all selected proteins", 1)
        showNotification("Cleared all selected proteins", type = "message", duration = 2)
      }, error = function(e) {
        debug_log(paste("Error in clearButton_STRING:", e$message), 1)
      })
    })

    # Remove a single protein via its per-row button in the Selected Proteins list
    observeEvent(input$remove_protein_click, {
      tryCatch({
        protein_to_remove <- input$remove_protein_click
        if (is.null(protein_to_remove) || !nzchar(protein_to_remove)) return()

        debug_log(paste("Removing protein:", protein_to_remove), 2)

        previous_data <- selected_data_STRING()
        if (is.null(previous_data) || !is.data.frame(previous_data) || nrow(previous_data) == 0) return()

        col_name <- names(previous_data)[1]
        filtered_data <- previous_data[previous_data[[col_name]] != protein_to_remove, , drop = FALSE]
        selected_data_STRING(filtered_data)

        if (nrow(filtered_data) > 0) {
          selected_protein_vector(unique(as.character(filtered_data[[col_name]])))
        } else {
          selected_protein_vector(NULL)
        }

        debug_log(paste("Removed protein:", protein_to_remove,
                        "- remaining:", nrow(filtered_data)), 1)
      }, error = function(e) {
        debug_log(paste("Error in remove_protein_click:", e$message), 1)
        showNotification("Error removing protein from selection", type = "error")
      })
    })

    # Add pathway proteins
    observeEvent(input$Protein_Input_STRING, {
      tryCatch({
        debug_log("Adding pathway proteins", 2)

        result_df <- list()
        pathway_counter <- 0

        # Process cluster selection
        tryCatch({
          cluster_selected <- input$ClustersProtein_STRING
          if (length(cluster_selected) > 0 && !is.null(test_g())) {
            test_g_temp <- test_g()
            ceb <- igraph::cluster_edge_betweenness(test_g_temp)
            membership_vector <- igraph::membership(ceb)
            node_names <- igraph::V(test_g_temp)$name
            cluster_list  <- split(node_names, membership_vector)
            cluster_list  <- cluster_list[lengths(cluster_list) >= 2]
            cluster_names <- ceb3_combined(cluster_list, debug_log)

            selected_indices <- which(cluster_names %in% cluster_selected)
            cluster_proteins <- cluster_list[selected_indices]

            for (i in seq_along(cluster_proteins)) {
              pathway_counter <- pathway_counter + 1
              result_df[[pathway_counter]] <- cluster_proteins[[i]]
            }
            debug_log(paste("Processed", length(cluster_proteins), "clusters"), 2)
          }
        }, error = function(e) {
          debug_log(paste("Error processing clusters:", e$message), 1)
        })

        # Process GSEA selection
        GSEA_STRING_selected <- input$GSEA_STRING
        if (length(GSEA_STRING_selected) > 0) {
          tryCatch({
            gsea_results <- get_gsea_results_payload()

            if (!is.null(gsea_results) && "Results" %in% names(gsea_results)) {
              tryCatch({
                results_obj <- gsea_results$Results

                results_df <- NULL
                if (inherits(results_obj, "enrichResult") || inherits(results_obj, "gseaResult")) {
                  results_df <- as.data.frame(results_obj)
                } else if (is.data.frame(results_obj)) {
                  results_df <- results_obj
                }

                if (!is.null(results_df) && nrow(results_df) > 0) {
                  pathway_col <- if ("Description" %in% colnames(results_df)) {
                    "Description"
                  } else if ("ID" %in% colnames(results_df)) {
                    "ID"
                  } else {
                    NULL
                  }

                  if (!is.null(pathway_col)) {
                    for (gsea_pathway in GSEA_STRING_selected) {
                      tryCatch({
                        pathway_data <- results_df[results_df[[pathway_col]] == gsea_pathway, ]

                        if (nrow(pathway_data) > 0) {
                          pathway_proteins <- character()

                          if (input$CoreEnriched_STRING) {
                            if ("core_enrichment" %in% colnames(pathway_data)) {
                              core_genes <- pathway_data$core_enrichment[1]
                              if (!is.na(core_genes)) {
                                pathway_proteins <- unlist(strsplit(core_genes, "/"))
                                debug_log(paste("GSEA pathway", gsea_pathway, "- core enriched:", length(pathway_proteins), "proteins"), 2)
                              }
                            }
                          } else {
                            gene_sets <- NULL

                            tryCatch({
                              if (is.list(gsea_results) && "geneSets" %in% names(gsea_results)) {
                                gene_sets <- gsea_results$geneSets
                              } else if (inherits(gsea_results$Results, "gseaResult") && .hasSlot(gsea_results$Results, "geneSets")) {
                                gene_sets <- slot(gsea_results$Results, "geneSets")
                              } else if (inherits(results_obj, "gseaResult") && .hasSlot(results_obj, "geneSets")) {
                                gene_sets <- slot(results_obj, "geneSets")
                              }
                            }, error = function(e) {
                              debug_log(paste("Error accessing geneSets for", gsea_pathway, ":", e$message), 2)
                            })

                            if (!is.null(gene_sets) && gsea_pathway %in% names(gene_sets)) {
                              pathway_proteins <- gene_sets[[gsea_pathway]]
                              debug_log(paste("GSEA pathway", gsea_pathway, "- all geneSets:", length(pathway_proteins), "proteins"), 2)
                            } else {
                              if ("core_enrichment" %in% colnames(pathway_data)) {
                                core_genes <- pathway_data$core_enrichment[1]
                                if (!is.na(core_genes)) {
                                  pathway_proteins <- unlist(strsplit(core_genes, "/"))
                                  debug_log(paste("GSEA pathway", gsea_pathway, "- fallback core enriched:", length(pathway_proteins), "proteins"), 2)
                                }
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
              }, error = function(e) {
                debug_log(paste("Error processing GSEA results:", e$message), 1)
              })
            }

          }, error = function(e) {
            debug_log(paste("Error in GSEA processing:", e$message), 1)
          })
        }

        # Process GO selection
        GO_STRING_selected <- input$GO_STRING
        if (length(GO_STRING_selected) > 0) {
          tryCatch({
            go_data <- get_go_data()

            if (!is.null(go_data) && is.data.frame(go_data) && "Description" %in% colnames(go_data)) {
              for (go_pathway in GO_STRING_selected) {
                tryCatch({
                  go_indices <- which(go_data$Description == go_pathway)

                  if (length(go_indices) > 0 && "geneID" %in% colnames(go_data)) {
                    pathway_proteins <- character()

                    for (idx in go_indices) {
                      gene_id <- go_data$geneID[idx]
                      if (!is.na(gene_id)) {
                        proteins <- unlist(strsplit(gene_id, "/"))
                        pathway_proteins <- c(pathway_proteins, proteins)
                      }
                    }

                    if (length(pathway_proteins) > 0) {
                      pathway_counter <- pathway_counter + 1
                      result_df[[pathway_counter]] <- unique(pathway_proteins)
                      debug_log(paste("Added GO pathway", go_pathway, "with", length(unique(pathway_proteins)), "proteins"), 1)
                    }
                  }
                }, error = function(e) {
                  debug_log(paste("Error processing GO pathway", go_pathway, ":", e$message), 1)
                })
              }
            }

          }, error = function(e) {
            debug_log(paste("Error in GO processing:", e$message), 1)
          })
        }

        # Apply intersection or union logic
        tryCatch({
          if (length(result_df) == 0 || all(sapply(result_df, length) == 0)) {
            debug_log("No proteins found from pathway selection", 1)
            showNotification("No proteins found for selected pathways", type = "warning")
            return()
          }

          result_df <- result_df[sapply(result_df, length) > 0]

          if (length(result_df) == 0) {
            debug_log("No valid pathways after cleaning", 1)
            showNotification("No valid pathways found", type = "warning")
            return()
          }

          debug_log(paste("Processing", length(result_df), "pathways for intersection/union"), 1)

          if (input$Intersect_STRING == TRUE) {
            if (length(result_df) == 1) {
              proteins <- unique(result_df[[1]])
            } else {
              proteins <- Reduce(intersect, result_df)
              proteins <- unique(proteins)
            }
            proteins <- proteins[proteins != ""]
            debug_log(paste("Intersection logic: found", length(proteins), "proteins in ALL pathways"), 1)
          } else {
            all_proteins <- unlist(result_df)
            proteins <- unique(all_proteins)
            proteins <- proteins[proteins != ""]
            debug_log(paste("Union logic: found", length(proteins), "unique proteins from all pathways"), 1)
          }

          if (length(proteins) > 0) {
            current_text <- input$input_STRING
            if (current_text == "" || is.null(current_text)) {
              new_text <- paste(proteins, collapse = "\n")
            } else {
              new_text <- paste(current_text, paste(proteins, collapse = "\n"), sep = "\n")
            }

            updateTextAreaInput(session, "input_STRING", value = new_text)
            debug_log(paste("Added", length(proteins), "proteins to input field"), 1)

            intersection_status <- if (input$Intersect_STRING) "intersecting" else "all unique"
            showNotification(paste("Added", length(proteins), intersection_status, "proteins from pathways"),
                             type = "message", duration = 3)
          } else {
            debug_log("No proteins found after intersection/union", 1)
            if (input$Intersect_STRING) {
              showNotification("No proteins found in intersection of all selected pathways", type = "warning")
            } else {
              showNotification("No valid proteins found to add", type = "warning")
            }
          }

        }, error = function(e) {
          debug_log(paste("Error in intersection/union logic:", e$message), 1)
          showNotification("Error processing pathway proteins", type = "error")
        })

      }, error = function(e) {
        debug_log(paste("Critical error in Protein_Input_STRING observer:", e$message), 1)
        showNotification("An error occurred while adding pathway proteins", type = "error")
      })
    })
  }

  list(
    get_gsea_results_payload = get_gsea_results_payload,
    get_gsea_data = get_gsea_data,
    get_go_data = get_go_data,
    register_data_source_observers = register_data_source_observers,
    register_selection_observers = register_selection_observers
  )
}
