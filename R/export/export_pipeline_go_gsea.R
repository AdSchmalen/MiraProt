# ==============================================================================
# File: R/export/export_pipeline_go_gsea.R
#
# Purpose:
#   Execute GO and GSEA export staging for comprehensive workbook generation.
#
# Architectural Role:
#   Stage helper for create_comprehensive_excel() pipeline orchestration.
#
# Responsibilities:
#   - Extract GO data from module outputs and fallback rv state.
#   - Serialize and export GSEA result content and metadata columns.
#   - Update shared sheet counters and emit debug diagnostics.
#
# Non-Responsibilities (Must NOT be here):
#   - Base Data Wizard sheets, optional module sheets, finalization tasks.
#
# Public API:
#   export_pipeline_run_go_gsea()
# ==============================================================================

#' Pipeline stage: GO + GSEA sheets
#'
#' Purpose:
#'   Run GO and GSEA staging within the shared export context.
#'
#' Inputs/Parameters:
#'   @param ctx Environment-based export context.
#'
#' Outputs:
#'   - Invisibly returns NULL; mutates `ctx` in place.
#'
#' Side effects:
#'   - Adds GO/GSEA worksheets and updates `ctx$sheets_created`.
#'
#' Failure behavior:
#'   - Preserves per-block tryCatch behavior and logs recoverable failures.
export_pipeline_run_go_gsea <- function(ctx) {
  with(ctx, {
    # ========================================
    # Sheet 7: GO Analysis Results
    # ========================================
    debug_log("Checking for GO results", level = 1)
    go_data <- NULL

    # 1) modEnv$GO_Result_List (kann Funktion oder Objekt sein)
    if (exists("GO_Result_List", envir = modEnv)) {
      debug_log("GO: trying modEnv$GO_Result_List", level = 1)
      go_data <- tryCatch({
        obj <- get("GO_Result_List", envir = modEnv)
        res <- if (is.function(obj)) obj() else obj
        debug_log(paste("GO: modEnv$GO_Result_List returned class:", class(res)[1]), level = 1)
        res
      }, error = function(e) {
        debug_log(paste("GO: GO_Result_List() failed:", e$message), level = 1)
        NULL
      })
    }

    # 2) module_outputs$go_out$get_results()
    if (is.null(go_data) &&
        !is.null(module_outputs$go_out) &&
        "get_results" %in% names(module_outputs$go_out) &&
        is.function(module_outputs$go_out$get_results)) {
      debug_log("GO: trying module_outputs$go_out$get_results()", level = 1)
      go_data <- tryCatch({
        res <- module_outputs$go_out$get_results()
        debug_log(paste("GO: get_results returned class:", class(res)[1]), level = 1)
        res
      }, error = function(e) {
        debug_log(paste("GO: get_results() failed:", e$message), level = 1)
        NULL
      })
    }

    # 3) Fallback: rv$go_results
    if (is.null(go_data) && !is.null(rv) && !is.null(rv$go_results)) {
      debug_log("GO: trying rv$go_results", level = 1)
      go_data <- tryCatch({
        res <- rv$go_results
        debug_log(paste("GO: rv$go_results class:", class(res)[1]), level = 1)
        res
      }, error = function(e) {
        debug_log(paste("GO: rv$go_results failed:", e$message), level = 1)
        NULL
      })
    }

    # 4) Normalisieren auf data.frame
    if (!is.null(go_data) && !is.data.frame(go_data)) {
      # enrichResult/gseaResult direkt
      if (inherits(go_data, "enrichResult") || inherits(go_data, "gseaResult")) {
        go_data <- tryCatch({
          as.data.frame(go_data)
        }, error = function(e) {
          debug_log(paste("GO: as.data.frame(enrichResult) failed:", e$message), level = 1)
          NULL
        })
      }

      # Slot @result (S4)
      if (!is.null(go_data) && isS4(go_data) && "result" %in% slotNames(go_data)) {
        go_data <- tryCatch({
          res_df <- go_data@result
          debug_log(paste("GO: extracted @result with", nrow(res_df), "rows"), level = 1)
          res_df
        }, error = function(e) NULL)
      }

      # Liste: suche data.frame mit ID/Description oder enrichResult-Elemente
      if (!is.null(go_data) && is.list(go_data) && !is.data.frame(go_data)) {
        found_df <- NULL
        for (el in go_data) {
          if (inherits(el, "enrichResult") || inherits(el, "gseaResult")) {
            found_df <- tryCatch(as.data.frame(el), error = function(e) NULL)
          } else if (is.data.frame(el)) {
            found_df <- el
          } else if (!is.null(el$result) && is.data.frame(el$result)) {
            found_df <- el$result
          }
          if (!is.null(found_df) && is.data.frame(found_df)) break
        }
        if (!is.null(found_df) && is.data.frame(found_df)) {
          go_data <- found_df
          debug_log(paste("GO: extracted data.frame from list with", nrow(go_data), "rows"), level = 1)
        }
      }
    }

    # Sheet schreiben oder abbrechen
    if (!is.null(go_data) && is.data.frame(go_data) && nrow(go_data) > 0) {
      debug_log(paste("Creating GO Analysis sheet with", nrow(go_data), "terms"), level = 1)

      tryCatch({
        # Sort by significance
        if ("p.adjust" %in% colnames(go_data)) {
          go_data <- go_data[order(go_data$p.adjust), ]
        } else if ("pvalue" %in% colnames(go_data)) {
          go_data <- go_data[order(go_data$pvalue), ]
        }

        # Create compact results table
        compact_go <- data.frame(
          GO_ID = as.character(go_data$ID),
          Description = as.character(go_data$Description),
          Gene_Count = if ("Count" %in% colnames(go_data)) as.integer(go_data$Count) else rep(NA, nrow(go_data)),
          Gene_Ratio = if ("GeneRatio" %in% colnames(go_data)) as.character(go_data$GeneRatio) else rep("", nrow(go_data)),
          P_Value = if ("pvalue" %in% colnames(go_data)) as.numeric(go_data$pvalue) else rep(NA, nrow(go_data)),
          Adjusted_P_Value = if ("p.adjust" %in% colnames(go_data)) as.numeric(go_data$p.adjust) else rep(NA, nrow(go_data)),
          Enriched_Genes = character(nrow(go_data)),
          stringsAsFactors = FALSE
        )

        # Process genes for readability (comma-separated instead of slash-separated)
        for (i in 1:nrow(go_data)) {
          if ("geneID" %in% colnames(go_data)) {
            gene_ids <- as.character(go_data$geneID[i])
            if (!is.na(gene_ids) && nzchar(gene_ids)) {
              genes <- unlist(strsplit(gene_ids, "/"))
              genes <- genes[!is.na(genes) & nzchar(genes)]
              if (length(genes) > 0) {
                compact_go$Enriched_Genes[i] <- paste(genes, collapse = ", ")
              }
            }
          }
        }

        # Add to workbook
        openxlsx::addWorksheet(wb, "GO_Analysis")
        writeData_sanitized(wb, "GO_Analysis", compact_go, startRow = 1, startCol = 1)
        if (ncol(compact_go) > 0) {
          openxlsx::addStyle(wb, "GO_Analysis", headerStyle, rows = 1, cols = 1:ncol(compact_go))
        }
        sheets_created <- sheets_created + 1
        debug_log("GO Analysis sheet created successfully", level = 1)

      }, error = function(e) {
        debug_log(paste("Error creating GO Analysis sheet:", e$message), level = 1)
      })
    } else {
      debug_log("No valid GO data available for Excel export", level = 1)
    }

    # ========================================
    # Sheet 8: GSEA Analysis Results
    # ========================================
    debug_log("Creating Sheet 8: GSEA Analysis Results", level = 1)

    # 1. GSEA Ergebnisse holen
    gsea_raw <- NULL
    analysis_metadata <- NULL
    if (!is.null(module_outputs) &&
        !is.null(module_outputs$gsea_out) &&
        is.list(module_outputs$gsea_out) &&
        "get_results" %in% names(module_outputs$gsea_out) &&
        is.function(module_outputs$gsea_out$get_results)) {

      gsea_raw <- try(isolate(module_outputs$gsea_out$get_results()), silent = TRUE)
      if (inherits(gsea_raw, "try-error")) {
        debug_log(paste("GSEA get_results() error:", attr(gsea_raw, "condition")$message), level = 1)
        gsea_raw <- NULL
      }

      # 2. Analysis Metadata holen
      if ("get_analysis_metadata" %in% names(module_outputs$gsea_out) &&
          is.function(module_outputs$gsea_out$get_analysis_metadata)) {
        analysis_metadata <- try(isolate(module_outputs$gsea_out$get_analysis_metadata()), silent = TRUE)
        if (inherits(analysis_metadata, "try-error")) {
          debug_log("Could not retrieve analysis metadata", level = 2)
          analysis_metadata <- NULL
        }
      }
    } else {
      debug_log("GSEA get_results() not available – skipping Sheet 8", level = 2)
    }

    if (is.null(gsea_raw)) {
      debug_log("No GSEA results present – Sheet 8 skipped", level = 1)
    } else {
      # 3. gseaResult extrahieren und metadata verarbeiten
      gsea_obj <- NULL
      ranking_vector <- NULL
      fc_vector <- NULL
      gmt_file_used <- "UNKNOWN"
      original_params <- list()

      if (inherits(gsea_raw, "gseaResult")) {
        gsea_obj <- gsea_raw
      } else if (is.list(gsea_raw) && !is.null(gsea_raw$Results) && inherits(gsea_raw$Results, "gseaResult")) {
        gsea_obj <- gsea_raw$Results
        # optionale Vektoren
        for (cand in c("RankingVector", "ranking_vector", "GeneListOriginal")) {
          if (!is.null(gsea_raw[[cand]])) { ranking_vector <- gsea_raw[[cand]]; break }
        }
        for (cand in c("FoldChangeVector", "FCVector", "FoldChange", "fc_vector")) {
          if (!is.null(gsea_raw[[cand]])) { fc_vector <- gsea_raw[[cand]]; break }
        }

        # Analysis metadata extrahieren
        if (!is.null(gsea_raw$analysis_metadata)) {
          metadata <- gsea_raw$analysis_metadata
          gmt_file_used <- metadata$gmt_file %||% "UNKNOWN"
          original_params <- metadata$analysis_params %||% list()
          debug_log(paste("Extracted analysis metadata - GMT file:", gmt_file_used), level = 1)
        }
      }

      # Fallback: Wenn keine metadata in gsea_raw, nutze separate analysis_metadata
      if (gmt_file_used == "UNKNOWN" && !is.null(analysis_metadata)) {
        gmt_file_used <- analysis_metadata$gmt_file %||% "UNKNOWN"
        original_params <- analysis_metadata$analysis_params %||% list()
        debug_log(paste("Using separate analysis metadata - GMT file:", gmt_file_used), level = 1)
      }

      if (is.null(gsea_obj) || !inherits(gsea_obj, "gseaResult")) {
        debug_log("Structure returned by get_results() is not a gseaResult – skipping Sheet 8", level = 1)
      } else {
        if (is.null(ranking_vector)) ranking_vector <- gsea_obj@geneList
        if (is.null(fc_vector))       fc_vector     <- ranking_vector

        # Validierung Ranking
        if (length(ranking_vector) == 0 || is.null(names(ranking_vector)) || any(!nzchar(names(ranking_vector)))) {
          debug_log("Invalid ranking vector (empty or missing names) – skipping Sheet 8", level = 1)
        } else {
          # 4. Export-Parameter
          export_version_tag <- "SINGLE_SHEET_1.1_WITH_METADATA"
          chunk_size <- 30000
          include_digests <- TRUE

          if (!requireNamespace("jsonlite", quietly = TRUE)) {
            debug_log("jsonlite missing – cannot write GSEA sheet", level = 1)
          } else {
            if (include_digests && !requireNamespace("digest", quietly = TRUE)) {
              include_digests <- FALSE
              debug_log("digest missing – disabling digests", level = 2)
            }

            # --- Helper functions stay the same ---
            # Local helper function:
            # Purpose: Serialize a named numeric vector to "name=value;..." form.
            # Inputs: x (named vector).
            # Outputs: single character scalar serialization.
            # Side effects: none.
            # Failure behavior: returns a best-effort string.
            collapse_named_vector <- function(x) paste(paste(names(x), x, sep = "="), collapse = ";")
            # Local helper function:
            # Purpose: Serialize gene set list to compact "id|a,b||id2|c,d" format.
            # Inputs: gs (named list of character vectors).
            # Outputs: single character scalar serialization.
            # Side effects: none.
            # Failure behavior: returns best-effort serialized output.
            serialize_gene_sets <- function(gs) {
              paste(vapply(names(gs), function(id) {
                paste0(id, "|", paste(gs[[id]], collapse = ","))
              }, character(1)), collapse = "||")
            }
            # Local helper function:
            # Purpose: Split long strings into fixed-size chunks for sheet columns.
            # Inputs: s (string), size (chunk size).
            # Outputs: character vector of chunks (or empty).
            # Side effects: none.
            # Failure behavior: returns empty vector for empty/NA input.
            chunk_string <- function(s, size) {
              if (is.null(s) || is.na(s) || s == "") return(character(0))
              n <- nchar(s)
              if (n <= size) return(s)
              starts <- seq(1, n, by = size)
              vapply(starts, function(st) substr(s, st, min(st + size - 1, n)), character(1))
            }
            # Local helper function:
            # Purpose: Append chunk columns to first row while keeping other rows blank.
            # Inputs: df (data frame), base (column base name), chunks (character vector).
            # Outputs: data frame with added chunk columns.
            # Side effects: mutates and returns local data-frame copy.
            # Failure behavior: writes empty base column when no chunks are present.
            add_chunk_columns <- function(df, base, chunks) {
              if (length(chunks) == 0) {
                df[[base]] <- ifelse(seq_len(nrow(df)) == 1, "", "")
                return(df)
              }
              for (i in seq_along(chunks)) {
                nm <- if (i == 1) base else paste0(base, " (", i, ")")
                df[[nm]] <- ifelse(seq_len(nrow(df)) == 1, chunks[[i]], "")
              }
              df
            }

            # --- Basis DataFrame ---
            res_df <- as.data.frame(gsea_obj)
            geneSets_full <- gsea_obj@geneSets

            rename_map <- c(
              ID = "Pathway ID",
              Description = "Description",
              setSize = "Set Size",
              enrichmentScore = "Enrichment Score",
              NES = "NES",
              pvalue = "P Value",
              p.adjust = "Adjusted P Value",
              qvalue = "Q Value",
              rank = "Rank",
              leading_edge = "Leading Edge",
              core_enrichment = "Core Enrichment Genes"
            )
            enhanced_df <- res_df
            for (old in names(rename_map)) {
              if (old %in% names(enhanced_df)) {
                names(enhanced_df)[names(enhanced_df) == old] <- rename_map[[old]]
              }
            }
            if (!"Pathway ID" %in% names(enhanced_df)) {
              debug_log("Column 'Pathway ID' missing – aborting Sheet 8", level = 1)
            } else {
              enhanced_df[["Gene Set Members"]] <- vapply(
                enhanced_df[["Pathway ID"]],
                function(id) if (!is.null(geneSets_full[[id]])) paste(geneSets_full[[id]], collapse = ",") else "",
                character(1)
              )

              ranking_serial  <- collapse_named_vector(ranking_vector)
              fc_serial       <- collapse_named_vector(fc_vector)
              genesets_serial <- serialize_gene_sets(geneSets_full)

              # Enhanced params with analysis metadata
              params_with_metadata <- c(
                gsea_obj@params,
                analysis_metadata = list(list(
                  gmt_file_used = gmt_file_used,
                  original_params = original_params,
                  export_timestamp = Sys.time()
                ))
              )
              # params_serial <- jsonlite::toJSON(params_with_metadata, auto_unbox = TRUE, null = "null")

              # slots_serial  <- jsonlite::toJSON(list(
              #   organism = gsea_obj@organism,
              #   setType = gsea_obj@setType,
              #   keytype = gsea_obj@keytype,
              #   readable = gsea_obj@readable,
              #   method = gsea_obj@method,
              #   gene2Symbol_length = length(gsea_obj@gene2Symbol),
              #   n_geneSets = length(geneSets_full),
              #   n_geneList = length(gsea_obj@geneList),
              #   export_version = export_version_tag,
              #   gmt_file_used = gmt_file_used
              # ), auto_unbox = TRUE, null = "null")

              # wrapper_meta_serial <- jsonlite::toJSON(list(
              #   has_fc_vector = !is.null(fc_vector),
              #   has_ranking_vector = !is.null(ranking_vector),
              #   wrapper_hint = "wrapper=list(Results=<gseaResult>, RankingVector=<ranking>, FoldChangeVector=<fc>)",
              #   analysis_metadata = !is.null(analysis_metadata)
              # ), auto_unbox = TRUE, null = "null")

              # digests_serial <- ""
              # if (include_digests) {
              #   digests_serial <- jsonlite::toJSON(list(
              #     result_md5   = digest::digest(res_df,        algo = "md5", serialize = TRUE),
              #     geneSets_md5 = digest::digest(geneSets_full, algo = "md5", serialize = TRUE),
              #     geneList_md5 = digest::digest(gsea_obj@geneList, algo = "md5", serialize = TRUE),
              #     params_md5   = digest::digest(params_with_metadata, algo = "md5", serialize = TRUE)
              #   ), auto_unbox = TRUE)
              # }

              ranking_chunks  <- chunk_string(ranking_serial,  chunk_size)
              fc_chunks       <- chunk_string(fc_serial,       chunk_size)
              # genesets_chunks <- chunk_string(genesets_serial, chunk_size)

              enhanced_df <- add_chunk_columns(enhanced_df, "Ranking Vector", ranking_chunks)
              enhanced_df <- add_chunk_columns(enhanced_df, "Fold Change Vector", fc_chunks)
              # enhanced_df <- add_chunk_columns(enhanced_df, "All Gene Sets", genesets_chunks)

              # ADD ANALYSIS METADATA COLUMNS
              enhanced_df[["GMT File Used"]]        <- ifelse(seq_len(nrow(enhanced_df)) == 1, gmt_file_used, "")
              enhanced_df[["Original Parameters"]]  <- ifelse(seq_len(nrow(enhanced_df)) == 1,
                                                              jsonlite::toJSON(original_params, auto_unbox = TRUE, null = "null"), "")
              # enhanced_df[["GSEA Params"]]          <- ifelse(seq_len(nrow(enhanced_df)) == 1, params_serial, "")
              # enhanced_df[["GSEA Slots"]]           <- ifelse(seq_len(nrow(enhanced_df)) == 1, slots_serial, "")
              # enhanced_df[["Wrapper Meta"]]         <- ifelse(seq_len(nrow(enhanced_df)) == 1, wrapper_meta_serial, "")
              # enhanced_df[["Integrity Digests"]]    <- ifelse(seq_len(nrow(enhanced_df)) == 1, digests_serial, "")

              # WICHTIG: Keine Verwendung von openxlsx::getSheetNames(wb), sondern names(wb)
              if ("GSEA_Analysis" %in% names(wb)) {
                openxlsx::removeWorksheet(wb, "GSEA_Analysis")
              }
              openxlsx::addWorksheet(wb, "GSEA_Analysis")
              writeData_sanitized(wb, "GSEA_Analysis", enhanced_df, startRow = 1, startCol = 1)
              if (ncol(enhanced_df) > 0) {
                openxlsx::addStyle(wb, "GSEA_Analysis", headerStyle, rows = 1, cols = 1:ncol(enhanced_df))
              }

              debug_log(
                paste("Sheet 8 created with",
                      nrow(enhanced_df), "pathways,",
                      length(geneSets_full), "gene sets,",
                      length(ranking_vector), "ranking entries,",
                      "GMT file:", gmt_file_used),
                level = 1
              )
            }
          }
        }
      }
    }

  })
  invisible(NULL)
}
