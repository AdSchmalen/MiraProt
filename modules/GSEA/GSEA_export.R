# GSEA_export.R
# Helper functions for GSEA results export to Excel
# Used by the comprehensive Excel export system

# ========================================
# GSEA Excel Export Functions
# ========================================

#' Extract GSEA data for Excel export
#' @param module_outputs List of all module outputs
#' @param debug_log Debug logging function
#' @return Data frame with GSEA results or NULL
extract_gsea_data_for_excel <- function(module_outputs, debug_log) {
  gsea_data <- NULL

  # Check if GSEA module has results
  if (!is.null(module_outputs) && !is.null(module_outputs$gsea_out)) {
    tryCatch({
      gsea_module <- module_outputs$gsea_out

      # Check if module has results function
      if (is.list(gsea_module) && "has_results" %in% names(gsea_module) && is.function(gsea_module$has_results)) {
        has_results <- gsea_module$has_results()
        debug_log(paste("GSEA has_results():", has_results), level = 1)

        if (has_results) {
          # Strategy 1: Access GSEA results from modEnv
          if (exists("modEnv", envir = globalenv())) {
            modEnv <- get("modEnv", envir = globalenv())

            if (exists("GSEA_Result_List", envir = modEnv)) {
              gsea_result_list_func <- get("GSEA_Result_List", envir = modEnv)

              if (is.function(gsea_result_list_func)) {
                gsea_results <- gsea_result_list_func()
                debug_log(paste("Retrieved GSEA results from modEnv"), level = 2)

                if (!is.null(gsea_results) && is.list(gsea_results)) {
                  # Extract Results component (enrichResult S4 object)
                  if ("Results" %in% names(gsea_results)) {
                    results_obj <- gsea_results$Results

                    if (inherits(results_obj, "enrichResult")) {
                      tryCatch({
                        gsea_data <- as.data.frame(results_obj)
                        debug_log(paste("Converted enrichResult to dataframe:", nrow(gsea_data), "pathways"), level = 1)
                      }, error = function(e) {
                        debug_log(paste("Error converting enrichResult:", e$message), level = 1)
                      })
                    } else if (inherits(results_obj, "gseaResult")) {
                      tryCatch({
                        gsea_data <- as.data.frame(results_obj)
                        debug_log(paste("Converted gseaResult to dataframe:", nrow(gsea_data), "pathways"), level = 1)
                      }, error = function(e) {
                        debug_log(paste("Error converting gseaResult:", e$message), level = 1)
                      })
                    } else if (is.data.frame(results_obj)) {
                      gsea_data <- results_obj
                      debug_log(paste("Using dataframe directly:", nrow(gsea_data), "pathways"), level = 1)
                    }
                  }
                }
              }
            }
          }

          # Strategy 2: Try direct module access if modEnv failed
          if (is.null(gsea_data) && "get_results" %in% names(gsea_module)) {
            tryCatch({
              gsea_results_direct <- gsea_module$get_results()
              if (!is.null(gsea_results_direct) && "Results" %in% names(gsea_results_direct)) {
                results_obj <- gsea_results_direct$Results
                if (inherits(results_obj, c("enrichResult", "gseaResult"))) {
                  gsea_data <- as.data.frame(results_obj)
                  debug_log(paste("Direct module access successful:", nrow(gsea_data), "pathways"), level = 1)
                }
              }
            }, error = function(e) {
              debug_log(paste("Direct module access failed:", e$message), level = 2)
            })
          }
        }
      }
    }, error = function(e) {
      debug_log(paste("Direct GSEA access failed:", e$message), level = 2)
    })
  }

  return(gsea_data)
}

#' Create GSEA Excel sheet - FIXED VERSION without protein limitation
#' @param wb Workbook object
#' @param gsea_data GSEA results data frame
#' @param headerStyle Header style object
#' @param debug_log Debug logging function
#' @return TRUE if sheet created successfully, FALSE otherwise
create_gsea_excel_sheet <- function(wb, gsea_data, headerStyle, debug_log) {
  tryCatch({
    if (is.null(gsea_data) || !is.data.frame(gsea_data) || nrow(gsea_data) == 0) {
      debug_log("No valid GSEA data available for Excel export", level = 2)
      return(FALSE)
    }

    debug_log(paste("Creating GSEA Analysis sheet with", nrow(gsea_data), "pathways"), level = 1)

    # Sort by significance (NES and then by p.adjust)
    if ("p.adjust" %in% colnames(gsea_data)) {
      gsea_data <- gsea_data[order(gsea_data$p.adjust), ]
    } else if ("pvalue" %in% colnames(gsea_data)) {
      gsea_data <- gsea_data[order(gsea_data$pvalue), ]
    }

    # Create compact results table for Excel - FIXED VERSION
    compact_gsea <- data.frame(
      Pathway_ID = if ("ID" %in% colnames(gsea_data)) as.character(gsea_data$ID) else paste0("PATHWAY_", seq_len(nrow(gsea_data))),
      Description = as.character(gsea_data$Description),
      Set_Size = if ("setSize" %in% colnames(gsea_data)) as.integer(gsea_data$setSize) else rep(NA, nrow(gsea_data)),
      Enrichment_Score = if ("enrichmentScore" %in% colnames(gsea_data)) round(as.numeric(gsea_data$enrichmentScore), 4) else rep(NA, nrow(gsea_data)),
      NES = if ("NES" %in% colnames(gsea_data)) round(as.numeric(gsea_data$NES), 4) else rep(NA, nrow(gsea_data)),
      P_Value = if ("pvalue" %in% colnames(gsea_data)) round(as.numeric(gsea_data$pvalue), 6) else rep(NA, nrow(gsea_data)),
      Adjusted_P_Value = if ("p.adjust" %in% colnames(gsea_data)) round(as.numeric(gsea_data$p.adjust), 6) else rep(NA, nrow(gsea_data)),
      Q_Value = if ("qvalues" %in% colnames(gsea_data)) round(as.numeric(gsea_data$qvalues), 6) else rep(NA, nrow(gsea_data)),
      Leading_Edge_Size = if ("leading_edge_num" %in% colnames(gsea_data)) as.integer(gsea_data$leading_edge_num) else rep(NA, nrow(gsea_data)),
      Core_Enrichment_Genes = character(nrow(gsea_data)),
      stringsAsFactors = FALSE
    )

    # Process core enrichment genes - FIXED VERSION: NO LIMITATION!
    for (i in 1:nrow(gsea_data)) {
      # Extract core enrichment genes
      if ("core_enrichment" %in% colnames(gsea_data)) {
        core_genes <- as.character(gsea_data$core_enrichment[i])
        if (!is.na(core_genes) && nzchar(core_genes)) {
          # Split genes (usually separated by "/")
          genes <- unlist(strsplit(core_genes, "/"))
          genes <- genes[!is.na(genes) & nzchar(genes)]
          if (length(genes) > 0) {
            # FIXED: Export ALL genes, comma-separated for readability
            # NO MORE 20-gene limitation!
            compact_gsea$Core_Enrichment_Genes[i] <- paste(genes, collapse = ", ")
            debug_log(paste("Exported", length(genes), "genes for pathway", i), level = 2)
          }
        }
      }
    }

    # Add worksheet to workbook
    openxlsx::addWorksheet(wb, "GSEA_Analysis")
    openxlsx::writeData(wb, "GSEA_Analysis", compact_gsea, startRow = 1, startCol = 1)

    # Apply header formatting
    if (ncol(compact_gsea) > 0) {
      openxlsx::addStyle(wb, "GSEA_Analysis", headerStyle, rows = 1, cols = 1:ncol(compact_gsea))
    }

    # Auto-adjust column widths for better readability
    openxlsx::setColWidths(wb, "GSEA_Analysis", cols = 1:ncol(compact_gsea), widths = "auto")

    debug_log("GSEA Analysis sheet created successfully with complete gene lists", level = 1)
    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error creating GSEA Analysis sheet:", e$message), level = 1)
    return(FALSE)
  })
}
