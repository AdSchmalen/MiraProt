    # ========================================
    # Protein Selection Panel - Observers
    # ========================================

    # Update GSEA dropdown
    observe({
      gsea_data <- tryCatch(
        heatmap_normalize_gsea_result(if (is.null(res_GSEA)) NULL else res_GSEA()),
        error = function(e) { heatmap_debug_log(paste("Error accessing res_GSEA():", e$message), 1); data.frame() }
      )
      choices <- heatmap_pathway_choices(gsea_data, id_fallback = TRUE)
      for (id in c("GSEA_Heatmap", "GSEA_IdentifierFilter_Heatmap")) {
        current <- isolate(input[[id]])
        updateSelectInput(session, id, choices = choices, selected = intersect(current %||% character(0), choices))
      }
      heatmap_debug_log(paste("Updating GSEA dropdowns with", length(choices), "pathways"), 1)
    })

    # Update GO dropdown
    observe({
      go_data <- tryCatch(
        heatmap_normalize_go_result(if (is.null(GO_res)) NULL else GO_res()),
        error = function(e) { heatmap_debug_log(paste("Error accessing GO_res():", e$message), 1); data.frame() }
      )
      choices <- heatmap_pathway_choices(go_data)
      for (id in c("GO_Heatmap", "GO_IdentifierFilter_Heatmap")) {
        current <- isolate(input[[id]])
        updateSelectInput(session, id, choices = choices, selected = intersect(current %||% character(0), choices))
      }
      heatmap_debug_log(paste("Updating GO dropdowns with", length(choices), "terms"), 1)
    })

        output$search_identifier_label_Heatmap <- renderText({

      selected_identifier <-
        input$GeneIdentifierColumn_Heatmap

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
    outputOptions(output, "search_identifier_label_Heatmap", suspendWhenHidden = FALSE)

    # Display suggested identifiers (partial and exact matches)
    output$geneSymbolList_Heatmap <- renderPrint({
      quiet_log <- function(...) invisible(NULL)

      tryCatch({
        req(rv$data_mod, rv$data_def)

        data <- rv$data_mod
        selected_identifier <- input$GeneIdentifierColumn_Heatmap
        identifier <- c()
        gene_symbols_text <- ""

        if (!is.null(input$input_Heatmap) && input$input_Heatmap != "" &&
            !is.null(selected_identifier) && selected_identifier != "") {

          filter_data <- get_filter_string_Heatmap(input$input_Heatmap, selected_identifier, quiet_log)

          if (nrow(filter_data) > 0) {
            filter_data <- as.vector(filter_data[,1])

            if (length(filter_data) > 0) {
              # PARTIAL MATCHING for suggestions - use grep with ignore.case
              all_identifiers <- data[[selected_identifier]]
              all_identifiers <- all_identifiers[!is.na(all_identifiers)]

              # Create pattern for partial matching (any of the input terms)
              pattern <- paste(filter_data, collapse = "|")

              # Find all partial matches
              identifier <- grep(pattern, all_identifiers, ignore.case = TRUE, value = TRUE)
              identifier <- unique(identifier)

              gene_symbols_text <- paste(identifier, collapse = "\n")
              quiet_log(paste("Found", length(identifier), "PARTIAL/EXACT matching identifiers"), 2)
            }
          }
        }

        cat(gene_symbols_text)

      }, error = function(e) {
        quiet_log(paste("Error in geneSymbolList_Heatmap:", e$message), 1)
        cat("")
      })
    })
    outputOptions(output, "geneSymbolList_Heatmap", suspendWhenHidden = FALSE)

    # Display selected proteins list with per-protein remove buttons
    output$selectedGene_Heatmap <- renderUI({
      tryCatch({
        selected_data <- selected_data_Heatmap()
        selected_identifier <- input$GeneIdentifierColumn_Heatmap

        if (is.null(selected_data) || !is.data.frame(selected_data) || nrow(selected_data) == 0 ||
            is.null(selected_identifier) || !nzchar(selected_identifier) ||
            !(selected_identifier %in% colnames(selected_data))) {
          return(
            tags$div(
              style = paste(
                "height: 150px; overflow-y: auto; border: 1px solid #ddd;",
                "border-radius: 4px; padding: 6px; background: #fff;"
              ),
              tags$p("No proteins selected",
                     style = "color: #666; margin: 0; text-align: center; padding-top: 30px;")
            )
          )
        }

        proteins <- unique(as.character(selected_data[[selected_identifier]]))
        heatmap_debug_log(paste("Rendering", length(proteins), "proteins with remove buttons"), 2)

        protein_rows <- lapply(proteins, function(protein) {
          js_call <- sprintf(
            "Shiny.setInputValue(\"%s\", \"%s\", {priority: \"event\"});",
            ns("remove_protein_click_Heatmap"),
            gsub('"', '\\\\"', protein, fixed = TRUE)
          )
          tags$div(
            style = paste(
              "display: flex; align-items: center; justify-content: space-between;",
              "padding: 2px 4px; border-bottom: 1px solid #eee; font-size: 12px;"
            ),
            tags$span(protein),
            tags$button(
              class = "btn btn-danger btn-xs",
              style = "padding: 1px 6px; font-size: 11px; line-height: 1.4;",
              onclick = js_call,
              icon("times")
            )
          )
        })

        tags$div(
          style = paste(
            "height: 150px; overflow-y: auto; overflow-x: auto;",
            "border: 1px solid #ddd; border-radius: 4px;",
            "background: #fff; padding: 4px;"
          ),
          protein_rows
        )
      }, error = function(e) {
        heatmap_debug_log(paste("Error in selectedGene_Heatmap renderUI:", e$message), 1)
        tags$p("Display error", style = "color: red; font-size: 12px;")
      })
    })

    # Add proteins to selection
    observeEvent(input$transferButton_Heatmap, {
      tryCatch({
        req(rv$data_mod, rv$data_def)

        df <- rv$data_mod
        original_col_order <- colnames(df)
        selected_identifier <- input$GeneIdentifierColumn_Heatmap

        if (is.null(selected_identifier) || selected_identifier == "") {
          showNotification("Please select an identifier type first", type = "warning")
          return()
        }

        if (is.null(input$input_Heatmap) || input$input_Heatmap == "") {
          showNotification("Please enter some proteins in the text area", type = "warning")
          return()
        }

        filter_data <- get_filter_string_Heatmap(input$input_Heatmap, selected_identifier, heatmap_debug_log)

        if (nrow(filter_data) > 0) {
          filter_data <- as.vector(filter_data[,1])

          if (length(filter_data) > 0) {
            # EXACT MATCHING ONLY - use %in% instead of grep
            identifier <- df[[selected_identifier]][df[[selected_identifier]] %in% filter_data]
            identifier <- identifier[!is.na(identifier)]

            if (length(identifier) > 0) {
              df_gene_identifier <- df[df[[selected_identifier]] %in% identifier, original_col_order, drop = FALSE]

              previous_data <- selected_data_Heatmap()
              if (is.null(previous_data)) {
                selected_data_Heatmap(df_gene_identifier)
              } else {
                combined_data <- dplyr::bind_rows(previous_data, df_gene_identifier)
                combined_data <- combined_data[!duplicated(combined_data), ]
                selected_data_Heatmap(combined_data)
              }

              # Update selected protein vector
              if (selected_identifier %in% colnames(selected_data_Heatmap())) {
                selected_protein_vector_temp <- as.vector(selected_data_Heatmap()[[selected_identifier]])
                selected_protein_vector_Heatmap(selected_protein_vector_temp)
              }

              heatmap_debug_log(paste("Transferred", length(identifier), "proteins to selection (EXACT matches)"), 1)
              showNotification(paste("Added", length(identifier), "proteins to selection (exact matches)"),
                               type = "message", duration = 3)
            } else {
              showNotification("No EXACT matching proteins found in dataset", type = "warning")
            }
          }
        }

      }, error = function(e) {
        heatmap_debug_log(paste("Error in transferButton_Heatmap:", e$message), 1)
        showNotification("Error adding proteins to selection", type = "error")
      })
    })

    # Clear all selected proteins
    observeEvent(input$clearButton_Heatmap, {
      tryCatch({
        selected_data_Heatmap(NULL)
        selected_protein_vector_Heatmap(NULL)
        heatmap_debug_log("Cleared all selected proteins", 1)
        showNotification("Cleared all selected proteins", type = "message", duration = 2)
      }, error = function(e) {
        heatmap_debug_log(paste("Error in clearButton_Heatmap:", e$message), 1)
      })
    })

    # Remove a single protein via its per-row button in the Selected Proteins list
    observeEvent(input$remove_protein_click_Heatmap, {
      tryCatch({
        protein_to_remove <- input$remove_protein_click_Heatmap
        if (is.null(protein_to_remove) || !nzchar(protein_to_remove)) return()

        heatmap_debug_log(paste("Removing protein:", protein_to_remove), 2)

        previous_data <- selected_data_Heatmap()
        if (is.null(previous_data) || !is.data.frame(previous_data) || nrow(previous_data) == 0) return()

        selected_identifier <- input$GeneIdentifierColumn_Heatmap
        if (is.null(selected_identifier) || !nzchar(selected_identifier) ||
            !(selected_identifier %in% colnames(previous_data))) return()

        filtered_data <- previous_data[previous_data[[selected_identifier]] != protein_to_remove, , drop = FALSE]
        selected_data_Heatmap(filtered_data)

        if (nrow(filtered_data) > 0) {
          selected_protein_vector_Heatmap(unique(as.character(filtered_data[[selected_identifier]])))
        } else {
          selected_protein_vector_Heatmap(NULL)
        }

        heatmap_debug_log(paste("Removed protein:", protein_to_remove,
                                "- remaining:", nrow(filtered_data)), 1)
      }, error = function(e) {
        heatmap_debug_log(paste("Error in remove_protein_click_Heatmap:", e$message), 1)
        showNotification("Error removing protein from selection", type = "error")
      })
    })

    # Add pathway proteins
    observeEvent(input$Protein_Input_Heatmap, {
      tryCatch({
        gsea_data <- heatmap_normalize_gsea_result(if (is.null(res_GSEA)) NULL else res_GSEA())
        go_data <- heatmap_normalize_go_result(if (is.null(GO_res)) NULL else GO_res())
        groups <- c(
          heatmap_extract_pathway_proteins(gsea_data, input$GSEA_Heatmap, "gsea"),
          heatmap_extract_pathway_proteins(go_data, input$GO_Heatmap, "go")
        )
        if (!isTRUE(input$CoreEnriched_Heatmap) && length(input$GSEA_Heatmap)) {
          heatmap_debug_log("All geneSets access not implemented in this version", 1)
          showNotification("All genes mode not yet implemented, using core enriched", type = "warning")
        }
        proteins <- heatmap_combine_pathway_proteins(groups, isTRUE(input$Intersect_Heatmap))
        if (length(proteins)) updateTextAreaInput(session, "input_Heatmap", value = paste(proteins, collapse = "\n"))
        updateSelectInput(session, "GSEA_Heatmap", selected = NULL)
        updateSelectInput(session, "GO_Heatmap", selected = NULL)
      }, error = function(e) {
        heatmap_debug_log(paste("Error in Protein_Input_Heatmap:", e$message), 1)
        showNotification("Error adding pathway proteins", type = "error")
      })
    })

    # Identifier Filter pathway import is deliberately independent of the
    # Protein Selection & Labeling controls above.
    observeEvent(input$Protein_Input_IdentifierFilter_Heatmap, {
      tryCatch({
        gsea_selected <- input$GSEA_IdentifierFilter_Heatmap %||% character(0)
        go_selected <- input$GO_IdentifierFilter_Heatmap %||% character(0)
        if (length(gsea_selected) && !isTRUE(input$CoreEnriched_IdentifierFilter_Heatmap)) {
          heatmap_debug_log("Identifier Filter cannot resolve unavailable non-core GSEA gene sets", 1)
          showNotification("Non-core GSEA gene sets are unavailable; no GSEA proteins were added.", type = "warning")
          gsea_selected <- character(0)
        }
        groups <- c(
          heatmap_extract_pathway_proteins(heatmap_normalize_gsea_result(if (is.null(res_GSEA)) NULL else res_GSEA()), gsea_selected, "gsea"),
          heatmap_extract_pathway_proteins(heatmap_normalize_go_result(if (is.null(GO_res)) NULL else GO_res()), go_selected, "go")
        )
        proteins <- heatmap_combine_pathway_proteins(groups, isTRUE(input$Intersect_IdentifierFilter_Heatmap))
        if (!length(proteins)) {
          heatmap_debug_log("Identifier Filter pathway import resolved no proteins", 1)
          showNotification("No proteins could be added from the selected terms.", type = "warning")
          return()
        }
        merged <- heatmap_merge_identifier_proteins(input$custom_proteins_filter, proteins)
        updateTextAreaInput(session, "custom_proteins_filter", value = paste(merged, collapse = "\n"))
        heatmap_debug_log(paste("Added", length(proteins), "pathway proteins to Identifier Filter"), 1)
      }, error = function(e) {
        heatmap_debug_log(paste("Identifier Filter pathway import failed:", e$message), 1)
        showNotification("No proteins could be added from the selected terms.", type = "warning")
      })
    })

    # Copy suggested identifiers to clipboard
    observeEvent(input$copyBtn_Heatmap, {
      tryCatch({
        req(rv$data_mod, rv$data_def)

        data <- rv$data_mod
        selected_identifier <- input$GeneIdentifierColumn_Heatmap
        identifier <- c()

        if (!is.null(input$input_Heatmap) && input$input_Heatmap != "" &&
            !is.null(selected_identifier) && selected_identifier != "") {

          filter_data <- get_filter_string_Heatmap(input$input_Heatmap, selected_identifier, heatmap_debug_log)

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

                heatmap_debug_log(paste("Copied", length(identifier), "identifiers to clipboard"), 1)
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
        heatmap_debug_log(paste("Error in copyBtn_Heatmap:", e$message), 1)
        showNotification("Error copying to clipboard", type = "error")
      })
    })

    # Highlight selected proteins
    observeEvent(input$highlightProteins_Heatmap, {
      tryCatch({
        selected_data <- selected_data_Heatmap()

        if (is.null(selected_data) || nrow(selected_data) == 0) {
          showNotification("No proteins selected for highlighting", type = "warning")
          return()
        }

        # Get identifier column
        selected_identifier <- input$GeneIdentifierColumn_Heatmap
        if (is.null(selected_identifier) || selected_identifier == "") {
          showNotification("No identifier column selected", type = "warning")
          return()
        }

        # Extract protein identifiers
        if (selected_identifier %in% colnames(selected_data)) {
          protein_ids <- as.vector(selected_data[[selected_identifier]])
          protein_ids <- protein_ids[!is.na(protein_ids) & protein_ids != ""]

          if (length(protein_ids) > 0) {
            heatmap_highlighted_proteins(protein_ids)
            heatmap_debug_log(paste("Highlighted", length(protein_ids), "proteins for heatmap annotation"), 1)
            showNotification(paste("Highlighted", length(protein_ids), "proteins - recreate heatmap to see changes"),
                             type = "message", duration = 4)
          } else {
            showNotification("No valid protein identifiers found", type = "warning")
          }
        } else {
          showNotification("Identifier column not found in selected proteins", type = "warning")
        }

      }, error = function(e) {
        heatmap_debug_log(paste("Error in highlightProteins_Heatmap:", e$message), 1)
        showNotification("Error highlighting proteins", type = "error")
      })
    })

    # Undo protein highlighting
    observeEvent(input$undoHighlight_Heatmap, {
      tryCatch({
        heatmap_highlighted_proteins(NULL)
        heatmap_protein_annotation(NULL)

        heatmap_debug_log("Protein highlighting cleared", 1)
        showNotification("Protein highlighting cleared - recreate heatmap to see changes",
                         type = "message", duration = 3)

      }, error = function(e) {
        heatmap_debug_log(paste("Error in undoHighlight_Heatmap:", e$message), 1)
        showNotification("Error clearing highlights", type = "error")
      })
    })

    # --------------------------------------------------------------------------
    # Section 6: Protein input observers (custom text input, highlight button)
    # --------------------------------------------------------------------------
    observeEvent(input$custom_proteins_input, {
      if (!is.null(input$custom_proteins_input) && input$custom_proteins_input != "") {
        proteins <- trimws(unlist(strsplit(input$custom_proteins_input, "[,;\\n]")))
        proteins <- proteins[proteins != ""]
        if (length(proteins) > 0) {
          heatmap_debug_log(paste("Custom proteins entered:", length(proteins)), 2)
          heatmap_selected_proteins(proteins)
        }
      }
    })

    observeEvent(input$highlight_proteins_btn, {
      proteins <- heatmap_selected_proteins()
      if (length(proteins) > 0) {
        heatmap_debug_log(paste("Highlighting", length(proteins), "proteins"), 1)
        showNotification(paste("Highlighting", length(proteins), "proteins in heatmap"),
                         type = "message", duration = 3)
        run_heatmap_creation(trigger_source = "protein-highlight-button")
      } else {
        showNotification("No proteins to highlight. Please enter proteins first.",
                         type = "warning", duration = 3)
      }
    })
