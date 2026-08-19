# =============================================================================
# modules/venn/venn_observers_data_lists.R

# Purpose: Data-source and dynamic-list observers.

# This peer is invoked by register_venn_observers() in registration order.
# evalq deliberately installs observers in the coordinator execution environment
# to preserve shared state, lexical lookup, and nested observeEvent behavior.
# =============================================================================

register_venn_data_list_observers <- function(observer_env) {
  evalq({
  # ---------------------------------------------------------------------------
  # Dynamic List UI
  # ---------------------------------------------------------------------------

  output$dynamicLists_Venn <- renderUI({
    list_count <- state$list_count_Venn()

    if (is.null(list_count) || list_count < 1L) {
      debug_log("Skipping dynamic lists UI because no Venn lists are configured", 2)
      return(tagList())
    }

    debug_log("Generating dynamic lists UI", 2)
    list_ui <- lapply(1:list_count, function(i) {
      div(
        create_list_ui(ns, i, state$list_data_Venn),
        br()
      )
    })
    do.call(tagList, list_ui)
  })

  # Keep this output alive while hidden for the documented session-restore
  # requirement and to render the default Venn list inputs during startup.
  # Restored dynamic list inputs must exist before the restore poll replays
  # name/list/color values and rebuilds a saved Venn plot.
  outputOptions(output, "dynamicLists_Venn", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # Data-Source Observers
  # ---------------------------------------------------------------------------

  # GSEA results observer: populate GSEA pathway dropdowns
  observe({
    datawizard_import_ready_signature(rv)
    if (datawizard_import_barrier_active(rv)) {
      debug_log("Import barrier active; preserving Venn choices until ready", 2)
      return()
    }
    gsea_data <- NULL

    if (!is.null(module_outputs) && !is.null(module_outputs$gsea_out)) {
      gsea_out <- module_outputs$gsea_out

      if ("has_results" %in% names(gsea_out)) {
        req(isTRUE(gsea_out$has_results()))
        tryCatch({
          has_results <- TRUE

          if (has_results && "get_results" %in% names(gsea_out)) {
            tryCatch({
              gsea_results <- gsea_out$get_results()

              if (!is.null(gsea_results) && is.list(gsea_results) &&
                  "Results" %in% names(gsea_results)) {
                results_obj <- gsea_results$Results

                if (inherits(results_obj, "enrichResult") ||
                    inherits(results_obj, "gseaResult")) {
                  tryCatch({
                    result_df <- as.data.frame(results_obj)
                    debug_log(paste("GSEA dataframe extracted - rows:",
                                    nrow(result_df), "cols:", ncol(result_df)), 1)

                    if (nrow(result_df) > 0 &&
                        "Description" %in% colnames(result_df)) {
                      gsea_data <- result_df
                    } else {
                      debug_log(paste("GSEA dataframe columns:",
                                      paste(colnames(result_df), collapse = ", ")), 1)
                    }
                  }, error = function(e) {
                    debug_log(paste("Error converting GSEA to dataframe:",
                                    e$message), 1)
                  })
                } else {
                  debug_log(paste("Unexpected GSEA results object class:",
                                  class(results_obj)), 1)
                }
              }
            }, error = function(e) {
              debug_log(paste("Error accessing GSEA get_results():", e$message), 1)
            })
          } else if (!has_results) {
            # nothing to do
          } else {
            debug_log("get_results function not found in gsea_out", 1)
          }
        }, error = function(e) {
          debug_log(paste("Error checking GSEA has_results():", e$message), 1)
        })
      } else {
        debug_log("has_results function not found in gsea_out", 2)
      }
    } else {
      debug_log("module_outputs or gsea_out is NULL", 2)
    }

    if (!is.null(gsea_data) && "Description" %in% colnames(gsea_data)) {
      descriptions <- unique(
        gsea_data$Description[!is.na(gsea_data$Description) &
                                gsea_data$Description != ""]
      )
      debug_log(paste("Updating GSEA dropdowns with",
                      length(descriptions), "pathways"), 1)

      list_count <- state$list_count_Venn()
      if (!is.null(list_count) && list_count > 0) {
        for (i in 1:list_count) {
          updateSelectInput(session, paste0("GSEA_SELECT_", i),
                            choices  = descriptions,
                            selected = safe_list_value(state$list_data_Venn$gsea, i))
        }
      }
    }
  })

  clear_venn_reference_controls <- function() {
    updateSelectInput(session, "ReferenceValues_Venn",
                      choices = character(0), selected = character(0))
    updateSelectizeInput(session, "data_abundance_ratio_num_Venn",
                         choices = character(0), selected = character(0),
                         server = TRUE)
    updateSelectizeInput(session, "data_abundance_ratio_denom_Venn",
                         choices = character(0), selected = character(0),
                         server = TRUE)
    updateSelectizeInput(session, "data_abundance_Mean_Venn",
                         choices = character(0), selected = character(0),
                         server = TRUE)

    list_count <- state$list_count_Venn()
    if (!is.null(list_count) && list_count > 0) {
      for (i in seq_len(list_count)) {
        updateSelectizeInput(session, paste0("Sample_SELECT_", i),
                             choices = character(0), selected = character(0),
                             server = TRUE)
      }
    }
  }

  metadata_choice_signature <- function(data_def = NULL, identifier_choices = NULL) {
    content <- if (is.data.frame(data_def) && "Content" %in% names(data_def)) {
      trimws(as.character(data_def$Content))
    } else {
      character(0)
    }
    options <- if (is.data.frame(data_def) && "Options" %in% names(data_def)) {
      trimws(as.character(data_def$Options))
    } else {
      character(0)
    }
    identifiers <- trimws(as.character(identifier_choices %||% character(0)))

    paste(
      paste(content, collapse = "\r"),
      paste(options, collapse = "\r"),
      paste(identifiers, collapse = "\r"),
      sep = "\f"
    )
  }

  last_reference_choice_signature <- reactiveVal(NULL)
  last_identifier_choice_signature <- reactiveVal(NULL)

  # Abundance type choices for the central reference-value dropdown
  observe({
    reference_choice_signature <- paste(
      datawizard_metadata_defer_downstream_choices(rv),
      metadata_choice_signature(
        rv$data_def,
        rv$datawizard_identifier_option_choices
      ),
      sep = "\f"
    )
    if (identical(reference_choice_signature, last_reference_choice_signature())) {
      return()
    }
    last_reference_choice_signature(reference_choice_signature)

    # Check the Data Wizard assignment lifecycle before requiring metadata so
    # this observer exits during the pending window opened by auto-assignment
    # and closed only after the metadata update transaction commits. This avoids
    # inspecting transient placeholder metadata whose only meaningful Content is
    # the structural Row Index helper.
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      clear_venn_reference_controls()
      debug_log("Metadata assignment pending; Venn reference value choices left empty", 3)
      return()
    }
    req(rv$data_def)
    debug_log("Updating central abundance type choices for reference values", 2)

    def <- rv$data_def
    if (is.null(def) || !is.data.frame(def) || nrow(def) == 0 ||
        !"Content" %in% names(def)) {
      clear_venn_reference_controls()
      debug_log("Venn reference value choices unavailable: metadata is not ready", 3)
      return()
    }

    content_choices <- unique(trimws(as.character(def$Content)))
    content_choices <- content_choices[!is.na(content_choices) & nzchar(content_choices)]

    # Row Index is a structural helper, not an abundance-like reference value for
    # the Venn abundance controls. Keep it out of the automatic central selector
    # so placeholder metadata never drives sample-choice observers.
    non_row_index_choices <- content_choices[content_choices != "Row Index"]
    if (length(non_row_index_choices) == 0) {
      clear_venn_reference_controls()
      debug_log("Venn reference value choices unavailable: metadata only contains Row Index", 3)
      return()
    }

    # Match the supported abundance types used by the Heatmap data-type
    # selector. In particular, do not treat ratios, p-values, PSM counts, or
    # other quantitative metadata as sample-level abundance measurements.
    abundance_types <- c(
      "Raw Abundance",
      "Normalized Abundance",
      "Imputed Raw Abundance",
      "Imputed Normalized Abundance",
      "Imputed Batch Corrected Normalized Abundance",
      "Imputed Batch Corrected Raw Abundance",
      "Batch Corrected Raw Abundance",
      "Batch Corrected Normalized Abundance"
    )
    abundance_like <- non_row_index_choices[
      tolower(trimws(non_row_index_choices)) %in%
        tolower(trimws(abundance_types))
    ]

    if (length(abundance_like) == 0) {
      clear_venn_reference_controls()
      debug_log("Venn reference value choices unavailable: no abundance-like metadata found", 3)
      return()
    }

    priority <- c("Normalized Abundance", "Raw Abundance")
    ref_candidates <- unique(c(
      priority[priority %in% abundance_like],
      abundance_like[!(abundance_like %in% priority)]
    ))

    updateSelectInput(session, "ReferenceValues_Venn",
                      choices  = ref_candidates,
                      selected = ref_candidates[1])
    debug_log(paste("Updated reference value choices:",
                    paste(ref_candidates, collapse = ", ")), 2)
  })

  # Per-list sample choices for import, driven by central reference value
  observe({
    req(rv$data_def, input$ReferenceValues_Venn)
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log("Metadata assignment pending; deferring Venn sample choices", 2)
      return()
    }
    debug_log("Updating all sample choices based on central reference value", 2)

    sample_choices <- venn_get_sample_choices(rv$data_def,
                                              input$ReferenceValues_Venn)
    if (length(sample_choices) > 0) {
      list_count <- state$list_count_Venn()
      if (!is.null(list_count) && list_count > 0) {
        for (i in 1:list_count) {
          updateSelectizeInput(session, paste0("Sample_SELECT_", i),
                               choices  = sample_choices,
                               selected = safe_list_value(state$list_data_Venn$sample, i),
                               server   = TRUE)
        }
        debug_log(paste("Updated sample choices:", length(sample_choices)), 2)
      }
    }
  })

  # Abundance-ratio sample choices, also driven by the central reference value
  observe({
    req(rv$data_def, input$ReferenceValues_Venn)
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log("Metadata assignment pending; deferring Venn ratio sample choices", 2)
      return()
    }
    debug_log("Updating sample choices for abundance ratios", 2)

    reference_value <- input$ReferenceValues_Venn
    if (!is.null(reference_value) && reference_value != "") {
      sample_choices <- venn_get_sample_choices(rv$data_def, reference_value)
      if (length(sample_choices) > 0) {
        updateSelectizeInput(session, "data_abundance_ratio_num_Venn",
                             choices = sample_choices, selected = NULL)
        updateSelectizeInput(session, "data_abundance_ratio_denom_Venn",
                             choices = sample_choices, selected = NULL)
        updateSelectizeInput(session, "data_abundance_Mean_Venn",
                             choices = sample_choices, selected = NULL)
        debug_log(paste("Updated abundance ratio sample choices:",
                        length(sample_choices), "samples"), 2)
      }
    }
  })

  # Protein identifier column choices
  observe({
    identifier_choice_signature <- paste(
      datawizard_metadata_defer_downstream_choices(rv),
      metadata_choice_signature(
        rv$data_def,
        rv$datawizard_identifier_option_choices
      ),
      sep = "\f"
    )
    if (identical(identifier_choice_signature, last_identifier_choice_signature())) {
      return()
    }
    last_identifier_choice_signature(identifier_choice_signature)

    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log("Metadata assignment pending; deferring Venn identifier choices", 2)
      return()
    }
    req(rv$data_mod, rv$data_def)
    debug_log("Updating protein identifier choices", 2)

    data_def         <- rv$data_def
    central_identifier_choices <- rv$datawizard_identifier_option_choices %||% character(0)
    Identifier_Index <- which(grepl("Identifier", data_def$Content))
    if (length(central_identifier_choices) > 0) {
      updateSelectInput(session, "GeneIdentifierColumn_Venn",
                        choices  = central_identifier_choices,
                        selected = central_identifier_choices[1])
    } else if (length(Identifier_Index) > 0) {
      Identifier <- data_def$Options[Identifier_Index]
      updateSelectInput(session, "GeneIdentifierColumn_Venn",
                        choices  = Identifier,
                        selected = Identifier[1])
    }
  })

  # GO results observer: populate GO pathway dropdowns
  observe({
    go_data <- NULL

    if (!is.null(module_outputs) && !is.null(module_outputs$go_out)) {
      go_out <- module_outputs$go_out

      if ("has_results" %in% names(go_out)) {
        req(isTRUE(go_out$has_results()))
        tryCatch({
          has_results <- TRUE

          if (has_results && "get_results" %in% names(go_out)) {
            tryCatch({
              go_results <- go_out$get_results()

              if (!is.null(go_results) && is.list(go_results) &&
                  length(go_results) > 0 && !is.null(go_results[[1]])) {
                go_data_temp <- go_results[[1]]

                if (inherits(go_data_temp, "enrichResult") ||
                    inherits(go_data_temp, "gseaResult")) {
                  tryCatch({
                    go_data <- as.data.frame(go_data_temp)
                    debug_log(paste("GO dataframe extracted - rows:",
                                    nrow(go_data), "cols:", ncol(go_data)), 1)
                  }, error = function(e) {
                    debug_log(paste("Error converting GO to dataframe:",
                                    e$message), 1)
                  })
                } else if (is.data.frame(go_data_temp)) {
                  go_data <- go_data_temp
                  debug_log(paste("GO dataframe directly available - rows:",
                                  nrow(go_data)), 1)
                }
              }
            }, error = function(e) {
              debug_log(paste("Error accessing GO get_results():", e$message), 1)
            })
          }
        }, error = function(e) {
          debug_log(paste("Error checking GO has_results():", e$message), 1)
        })
      }
    }

    if (!is.null(go_data) && nrow(go_data) > 0 &&
        "Description" %in% colnames(go_data)) {
      descriptions <- unique(
        go_data$Description[!is.na(go_data$Description) &
                              go_data$Description != ""]
      )
      debug_log(paste("Updating GO dropdowns with",
                      length(descriptions), "terms"), 1)

      list_count <- state$list_count_Venn()
      if (!is.null(list_count) && list_count > 0) {
        for (i in 1:list_count) {
          updateSelectInput(session, paste0("GO_SELECT_", i),
                            choices  = descriptions,
                            selected = safe_list_value(state$list_data_Venn$go, i))
        }
      }
    } else {
      debug_log("No valid GO data available for dropdown update", 2)
    }
  })

  # ---------------------------------------------------------------------------
  # Copy-Button Observers (import proteins from GSEA / GO / Sample into list)
  # ---------------------------------------------------------------------------

  observe({
    list_count <- state$list_count_Venn()
    if (is.null(list_count) || list_count <= 0) return()

    for (i in 1:list_count) {
      local({
        current_i <- i

        observeEvent(input[[paste0("copy_button_VENN_", current_i)]], {
          debug_log(paste("Copy button pressed for list", current_i), 2)

          selected_proteins_gsea   <- input[[paste0("GSEA_SELECT_", current_i)]]
          selected_proteins_go     <- input[[paste0("GO_SELECT_", current_i)]]
          selected_proteins_sample <- input[[paste0("Sample_SELECT_", current_i)]]

          debug_log(paste(
            "Selected - GSEA:", length(selected_proteins_gsea),
            "GO:", length(selected_proteins_go),
            "Sample:", length(selected_proteins_sample)
          ), 2)

          proteins <- character()

          # --- GSEA proteins ---
          if (length(selected_proteins_gsea) > 0) {
            debug_log("Extracting GSEA proteins", 2)

            if (!is.null(module_outputs) && !is.null(module_outputs$gsea_out)) {
              gsea_out <- module_outputs$gsea_out

              if ("has_results" %in% names(gsea_out)) {
                tryCatch({
                  if (gsea_out$has_results() && "get_results" %in% names(gsea_out)) {
                    gsea_results <- gsea_out$get_results()

                    if (!is.null(gsea_results) && is.list(gsea_results) &&
                        "Results" %in% names(gsea_results)) {
                      results_obj <- gsea_results$Results

                      if (inherits(results_obj, "enrichResult") ||
                          inherits(results_obj, "gseaResult")) {
                        result_df <- as.data.frame(results_obj)

                        if ("Description" %in% colnames(result_df)) {
                          matching_indices <- which(
                            result_df$Description %in% selected_proteins_gsea
                          )

                          if (length(matching_indices) > 0) {
                            core_enriched <- input[[paste0("CoreEnriched_VENN_", current_i)]]

                            if ("core_enrichment" %in% colnames(result_df)) {
                              core_genes    <- result_df$core_enrichment[matching_indices]
                              proteins_gsea <- unique(unlist(strsplit(core_genes, "/")))
                              debug_log(paste(
                                if (core_enriched) "Using core enrichment -"
                                else "Using core enrichment as fallback -",
                                length(proteins_gsea), "genes"
                              ), 2)
                            } else {
                              proteins_gsea <- character()
                              debug_log("No core_enrichment column found", 1)
                            }

                            proteins_gsea <- proteins_gsea[
                              !is.na(proteins_gsea) & proteins_gsea != ""
                            ]
                            proteins <- c(proteins, proteins_gsea)
                            debug_log(paste(
                              "Added", length(proteins_gsea),
                              "GSEA proteins from pathways:",
                              paste(selected_proteins_gsea, collapse = ", ")
                            ), 1)
                          } else {
                            debug_log("No matching GSEA pathways found", 1)
                          }
                        }
                      }
                    }
                  }
                }, error = function(e) {
                  debug_log(paste("Error extracting GSEA proteins:", e$message), 1)
                  showNotification("Error extracting GSEA proteins",
                                   type = "warning", duration = 3)
                })
              }
            }
          }

          # --- GO proteins ---
          if (length(selected_proteins_go) > 0) {
            debug_log("Extracting GO proteins", 2)

            if (!is.null(module_outputs) && !is.null(module_outputs$go_out)) {
              tryCatch({
                go_module <- module_outputs$go_out

                if (is.list(go_module) && "has_results" %in% names(go_module) &&
                    is.function(go_module$has_results) && go_module$has_results()) {
                  if (exists("modEnv") && exists("GO_Result_List", envir = modEnv)) {
                    go_results <- get("GO_Result_List", envir = modEnv)()

                    if (!is.null(go_results) && length(go_results) > 0 &&
                        inherits(go_results[[1]], "enrichResult")) {
                      result_df <- as.data.frame(go_results[[1]])

                      if ("Description" %in% colnames(result_df) &&
                          "geneID" %in% colnames(result_df)) {
                        matching_indices <- which(
                          result_df$Description %in% selected_proteins_go
                        )

                        if (length(matching_indices) > 0) {
                          gene_ids    <- result_df$geneID[matching_indices]
                          proteins_go <- unique(unlist(strsplit(gene_ids, "/")))
                          proteins_go <- proteins_go[
                            !is.na(proteins_go) & proteins_go != ""
                          ]
                          proteins <- c(proteins, proteins_go)
                          debug_log(paste(
                            "Added", length(proteins_go),
                            "GO proteins from terms:",
                            paste(selected_proteins_go, collapse = ", ")
                          ), 1)
                        } else {
                          debug_log("No matching GO terms found", 1)
                        }
                      }
                    }
                  }
                }
              }, error = function(e) {
                debug_log(paste("Error extracting GO proteins:", e$message), 1)
                showNotification("Error extracting GO proteins",
                                 type = "warning", duration = 3)
              })
            }
          }

          # --- Sample proteins ---
          if (length(selected_proteins_sample) > 0 &&
              !is.null(rv$data_mod) && !is.null(rv$data_def)) {
            debug_log("Extracting Sample proteins", 2)
            proteins_sample <- extract_sample_proteins(
              rv$data_mod, rv$data_def, selected_proteins_sample,
              input$ReferenceValues_Venn, input$GeneIdentifierColumn_Venn
            )
            proteins <- c(proteins, proteins_sample)
          }

          proteins <- unique(proteins[proteins != ""])

          if (length(proteins) > 0) {
            updateTextAreaInput(session, paste0("list", current_i),
                                value = paste(proteins, collapse = "\n"))

            current_name <- input[[paste0("name", current_i)]]
            source_label <- selected_venn_source_label(
              selected_proteins_gsea,
              selected_proteins_go,
              selected_proteins_sample
            )
            if (is_default_venn_list_name(current_name, current_i) &&
                !is.null(source_label)) {
              updateTextInput(session, paste0("name", current_i),
                              value = source_label)
              state$list_data_Venn$names[[current_i]] <- source_label
              debug_log(paste("Updated list", current_i,
                              "name from selected source label"), 2)
            }

            debug_log(paste("Updated list", current_i, "with",
                            length(proteins), "proteins"), 1)
            showNotification(paste("Added", length(proteins),
                                   "proteins to List", current_i),
                             type = "message", duration = 3)
          } else {
            debug_log("No proteins found for any selected criteria", 1)
            showNotification("No proteins found for selected criteria",
                             type = "warning", duration = 3)
          }
        })
      })
    }
  })

  # ---------------------------------------------------------------------------
  # List Management (add / remove / fill random)
  # ---------------------------------------------------------------------------

  observeEvent(input$add_proteins_Venn, {
    current_count <- state$list_count_Venn()
    sync_list_data_Venn(current_count)
    new_count <- current_count + 1

    if (new_count < 26) {
      state$list_data_Venn$names  <- c(isolate(state$list_data_Venn$names),
                                       paste("List", new_count))
      state$list_data_Venn$lists  <- c(isolate(state$list_data_Venn$lists),  "")
      state$list_data_Venn$colors <- c(isolate(state$list_data_Venn$colors), "#868686FF")
      state$list_data_Venn$gsea   <- c(isolate(state$list_data_Venn$gsea),   list(NULL))
      state$list_data_Venn$go     <- c(isolate(state$list_data_Venn$go),     list(NULL))
      state$list_data_Venn$sample <- c(isolate(state$list_data_Venn$sample), list(NULL))
      state$list_data_Venn$core_enriched <- c(isolate(state$list_data_Venn$core_enriched), list(TRUE))
      state$list_count_Venn(new_count)
      debug_log(paste("Added new list, total count:", new_count), 1)
    }
  })

  remove_venn_list_at <- function(remove_index) {
    current_count <- state$list_count_Venn()

    if (is.null(current_count) || current_count <= 0) return()
    if (missing(remove_index) || length(remove_index) != 1L ||
        !is.numeric(remove_index) || is.na(remove_index) ||
        !remove_index %in% seq_len(current_count)) {
      return()
    }

    if (current_count <= 1L) {
      showNotification("At least one Venn list is required.",
                       type = "warning", duration = 3)
      return()
    }

    sync_list_data_Venn(current_count)
    keep <- setdiff(seq_len(current_count), remove_index)

    state$list_data_Venn$names  <- isolate(state$list_data_Venn$names)[keep]
    state$list_data_Venn$lists  <- isolate(state$list_data_Venn$lists)[keep]
    state$list_data_Venn$colors <- isolate(state$list_data_Venn$colors)[keep]
    state$list_data_Venn$gsea   <- isolate(state$list_data_Venn$gsea)[keep]
    state$list_data_Venn$go     <- isolate(state$list_data_Venn$go)[keep]
    state$list_data_Venn$sample <- isolate(state$list_data_Venn$sample)[keep]
    state$list_data_Venn$core_enriched <- isolate(state$list_data_Venn$core_enriched)[keep]
    state$list_count_Venn(length(keep))
    debug_log(paste0("Removed list ", remove_index,
                     "; total count: ", length(keep)), 1)
  }

  observe({
    list_count <- state$list_count_Venn()
    if (is.null(list_count) || list_count <= 0) return()

    for (i in seq_len(list_count)) {
      local({
        current_i <- i
        observeEvent(input[[paste0("remove_list_VENN_", current_i)]], {
          remove_venn_list_at(current_i)
        }, ignoreInit = TRUE)
      })
    }
  })

  observeEvent(input$remove_proteins_Venn, {
    remove_venn_list_at(state$list_count_Venn())
  })

  observeEvent(input$fill_random_proteins_Venn, {
    debug_log("Filling lists with random proteins", 1)
    set.seed(123)
    protein_pool <- paste0("Protein", 1:100)
    new_lists <- lapply(1:state$list_count_Venn(), function(i) {
      sample(protein_pool, size = sample(10:20, 1), replace = FALSE)
    })

    overlaps <- sample(1:10, 5)
    for (i in overlaps) {
      new_lists[[i %% state$list_count_Venn() + 1]] <- c(
        new_lists[[i %% state$list_count_Venn() + 1]],
        paste0("Protein", i)
      )
    }

    state$list_data_Venn$lists <- lapply(new_lists, function(lst) {
      paste(lst, collapse = "\n")
    })
    lapply(1:state$list_count_Venn(), function(i) {
      updateTextAreaInput(session, paste0("list", i),
                          value = state$list_data_Venn$lists[[i]])
    })
  })

  }, envir = observer_env)
  invisible(NULL)
}
