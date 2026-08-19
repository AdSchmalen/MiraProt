# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Rule Engine
# Purpose:
#   Encapsulate auto-assign rule application logic (content, condition, ratio,
#   and full pipeline application) behind a reusable engine factory.
# Architectural Role:
#   Domain logic layer for rule execution. No module lifecycle ownership.
# Responsibilities:
#   - Provide stable rule application functions consumed by orchestrator.
#   - Preserve existing rule timing and mutation semantics.
# Non-Responsibilities:
#   - Must not register observers or own reactive state initialization.
# ============================================================================

if (!exists("datawizard_extract_condition_vector", mode = "function", inherits = TRUE)) {
  source("modules/Data Wizard/datawizard_condition_extraction.R", local = environment())
}

create_auto_assign_rule_engine <- function(
  debug_log,
  add_processing_log,
  rv_table_rules_autoassign_dw,
  rv_condition_rules_autoassign_dw,
  rv_rules_autoassign_dw,
  rules_loaded_centrally,
  extractedConds_autoassign_dw,
  progress_callback = function(stage) invisible(NULL),
  debug_level = 0
) {
    report_progress <- function(stage) {
      if (is.function(progress_callback)) progress_callback(stage)
      invisible(NULL)
    }
    apply_rule_autoassign_dw <- function(df, content_term, variant_id, include_pattern, exclude_pattern = NULL) {
      if (!"Column" %in% names(df)) {
        stop("'Column' column missing in data frame")
      }

      tryCatch({
        idx_inc <- grepl(include_pattern, df$Column, perl = TRUE)

        if (!is.null(exclude_pattern) && nzchar(exclude_pattern)) {
          idx_exc <- grepl(exclude_pattern, df$Column, perl = TRUE)
          idx <- idx_inc & !idx_exc
        } else {
          idx <- idx_inc
        }

        # df$Content[idx] <- content_term
        #
        # ratio_terms <- c(
        #   "Abundance Ratio",
        #   "Abundance Ratio p-Value",
        #   "Abundance Ratio Adj. p-Value"
        # )
        #
        # if (content_term %in% ratio_terms) {
        #   df$Options[idx] <- "Ratio"
        # }
        #
        # if (grepl("Identifier", content_term, fixed = TRUE)) {
        #   df$Options[idx] <- df$Column[idx]
        # }
        #
        # return(df)

        df$Content[idx] <- content_term
        variants <- attr(df,"variant_id",exact=TRUE); if(is.null(variants)) variants <- rep("",nrow(df))
        variants[idx] <- variant_id; attr(df,"variant_id") <- variants
        df <- set_ratio_or_identifier_options(df, idx, content_term)
        return(df)

      }, error = function(e) {
        debug_log(paste("Error applying rule:", e$message), 1)
        add_processing_log("apply_rule", "error", e$message)
        return(df)
      })
    }

    normalize_condition_rule_content <- function(content_type) {
      aliases <- c(
        "Imputed Raw Abundnace" = "Imputed Raw Abundance"
      )

      normalized <- as.character(content_type)
      matched_alias <- !is.na(normalized) & normalized %in% names(aliases)
      normalized[matched_alias] <- unname(aliases[normalized[matched_alias]])
      normalized
    }

    set_condition_rule_status <- function(df, status, lookup_col, matched_rows = 0L) {
      attr(df, "condition_rule_status") <- list(
        status = status,
        lookup_col = lookup_col,
        matched_rows = as.integer(matched_rows)
      )
      df
    }

    finalize_condition_sample_names <- function(df) {
      if (!is.data.frame(df)) stop("'df' must be a data frame")
      if (nrow(df) == 0L || !all(c("Column", "Content", "Options") %in% names(df))) {
        return(df)
      }
      if (!"Sample" %in% names(df)) df$Sample <- rep(NA_character_, nrow(df))

      eligible <- sample_name_eligible(df$Content)
      needed <- eligible & sample_name_needed(df$Sample)
      if (!any(needed)) return(df)

      content_groups <- unique(as.character(df$Content[needed]))
      for (content_type in content_groups) {
        group_rows <- which(eligible & !is.na(df$Content) & df$Content == content_type)
        group_needed <- sample_name_needed(df$Sample[group_rows])
        if (!any(group_needed)) next

        samples <- build_unique_sample_names(
          df$Column[group_rows],
          df$Options[group_rows],
          rep(content_type, length(group_rows))
        )

        preserved <- as.character(df$Sample[group_rows[!group_needed]])
        collision_messages <- character()
        occupied <- preserved
        for (position_index in which(group_needed)) {
          proposal <- samples[[position_index]]
          if (is.na(proposal) || !nzchar(trimws(proposal))) next
          original_proposal <- proposal
          suffix <- 2L
          while (proposal %in% occupied) {
            proposal <- paste0(original_proposal, "_", suffix)
            suffix <- suffix + 1L
          }
          if (!identical(proposal, original_proposal)) {
            collision_messages <- c(
              collision_messages,
              sprintf("row %d: proposed '%s' refined to '%s'",
                      group_rows[[position_index]], original_proposal, proposal)
            )
            samples[[position_index]] <- proposal
          }
          occupied <- c(occupied, proposal)
        }

        diagnostics <- attr(samples, "diagnostics")
        if (is.data.frame(diagnostics) && nrow(diagnostics) == length(group_rows)) {
          diagnostics$final_sample[!group_needed] <- df$Sample[group_rows[!group_needed]]
          diagnostics$final_sample[group_needed] <- samples[group_needed]
          diagnostics$preserved_existing <- !group_needed
          diagnostics$collision_refinement <- ""
          if (length(collision_messages)) {
            diagnostics$collision_refinement[group_needed] <-
              paste(collision_messages, collapse = "; ")
          }
          previous <- attr(df, "sample_name_diagnostics", exact = TRUE)
          if (is.data.frame(previous)) {
            all_columns <- union(names(previous), names(diagnostics))
            add_missing <- function(x) {
              for (column in setdiff(all_columns, names(x))) x[[column]] <- NA
              x[all_columns]
            }
            diagnostics <- rbind(add_missing(previous), add_missing(diagnostics))
            rownames(diagnostics) <- NULL
          }
          attr(df, "sample_name_diagnostics") <- diagnostics
        }
        if (length(collision_messages)) {
          add_processing_log(
            "finalize_condition_sample_names", "warning",
            paste("Generated sample-name collision with a preserved name;",
                  paste(collision_messages, collapse = "; "))
          )
        }
        df$Sample[group_rows[group_needed]] <- samples[group_needed]
      }
      df
    }

    apply_condition_autoassign_dw <- function(df, lookup_col, variant_id, position = c("between", "start", "end", "whole", "phrase_position", "pattern_detect"),
                                              before = NULL, after = NULL, phrase_pos = 1, sep = NULL, setter) {
      if (!is.function(setter)) {
        stop("'setter' must be a function")
      }

      tryCatch({
        lookup_col <- normalize_condition_rule_content(lookup_col)

        # Check if content type exists
        if (!lookup_col %in% df$Content) {
          debug_log(paste("Content type '", lookup_col, "' not found in current data - skipping condition rule"), 4)
          return(set_condition_rule_status(df, "skipped_missing_content", lookup_col))
        }

        if (!"Options" %in% names(df)) df$Options <- rep(NA_character_, nrow(df))
        variants <- attr(df,"variant_id",exact=TRUE); if(is.null(variants)) variants <- rep("",nrow(df))
        idx_lookup <- which(!is.na(df$Content) & df$Content == lookup_col & variants == variant_id)
        if (length(idx_lookup) == 0) {
          return(set_condition_rule_status(df, "skipped_no_matches", lookup_col))
        }

        cols <- df$Column[idx_lookup]
        finish_condition_rule <- function(data, extracted_conditions = NULL) {
          if (!is.null(extracted_conditions)) {
            data$Options[idx_lookup] <- extracted_conditions
            setter(extracted_conditions)
          }
          set_condition_rule_status(data, "applied", lookup_col, length(idx_lookup))
        }

        conds <- datawizard_extract_condition_vector(
          cols, position[1], before, after, sep, phrase_pos
        )

        if (all(is.na(conds) | conds == "")) {
          debug_log(sprintf("No conditions found for '%s' (mode = '%s')", lookup_col, position[1]), 3)
          return(set_condition_rule_status(df, "skipped_no_matches", lookup_col, length(idx_lookup)))
        }

        finish_condition_rule(df, conds)

      }, error = function(e) {
        debug_log(paste("Error applying condition rule:", e$message), 1)
        add_processing_log("apply_condition_rule", "error", e$message)
        return(set_condition_rule_status(df, "error", lookup_col))
      })
    }

    #' Apply ratio rules with fixed regex patterns
    #' @param df metadata data frame
    #' @param ratio_rules data frame with ratio extraction rules
    #' @return updated metadata data frame
    apply_ratio_rules_fixed <- function(df, ratio_rules) {
      debug_log("Applying ratio rules to metadata", 2)

      emit_ratio_rule_warning <- function(message) {
        debug_log(message, 1)
        if (exists("warn_auto_assign", mode = "function")) {
          try(warn_auto_assign(message), silent = TRUE)
        }
      }

      if (nrow(ratio_rules) == 0) {
        debug_log("No ratio rules to apply", 2)
        attr(df, "ratio_rule_status") <- list(
          total_rules = 0L,
          applied_rules = 0L,
          rows_updated = 0L,
          methods = "none"
        )
        return(df)
      }

      # Ensure required columns exist
      if (!"Numerator" %in% names(df)) df$Numerator <- NA_character_
      if (!"Denominator" %in% names(df)) df$Denominator <- NA_character_

      total_rules <- nrow(ratio_rules)
      applied_rules <- 0L
      rows_updated <- 0L
      methods <- if ("Method" %in% names(ratio_rules)) {
        unique(stats::na.omit(as.character(ratio_rules$Method)))
      } else {
        character(0)
      }
      method_summary <- if (length(methods) > 0) paste(methods, collapse = ", ") else "none"

      ratio_rules <- upgrade_rule_component(ratio_rules,"ratio")
      variants <- attr(df,"variant_id",exact=TRUE); if(is.null(variants)) variants <- rep("",nrow(df))
      for (i in seq_len(total_rules)) {
        rule <- ratio_rules[i, , drop = TRUE]
        content_type <- rule$Content
        method <- if ("Method" %in% names(rule) && !is.na(rule$Method) && nzchar(rule$Method)) rule$Method else "unspecified"

        tryCatch({
          debug_log(paste("Processing ratio rule for content:", content_type), 4)

          # Find rows with matching content
          content_rows <- which(df$Content == content_type & variants == rule$VariantId)

          if (length(content_rows) == 0) {
            emit_ratio_rule_warning(paste0(
              "Ratio rule failed: no rows found for content type '", content_type,
              "' | method=", method
            ))
          } else {
            debug_log(paste("Found", length(content_rows), "rows for content type:", content_type), 4)

            successful_extractions <- 0L

            for (row_idx in content_rows) {
              column_name <- df$Column[row_idx]
              debug_log(paste("Processing column:", column_name), 4)

              # Extract ratio components with fixed patterns
              components <- extract_ratio_components_from_rule(column_name, rule, debug_level)

              if (!is.null(components)) {
                df$Numerator[row_idx] <- components$numerator
                df$Denominator[row_idx] <- components$denominator
                successful_extractions <- successful_extractions + 1L
              }
            }

            debug_log(paste("Ratio rule for", content_type, ":", successful_extractions,
                            "successful extractions out of", length(content_rows), "rows"), 4)

            if (successful_extractions == 0L) {
              emit_ratio_rule_warning(paste0(
                "Ratio rule failed: zero rows extracted for content type '", content_type,
                "' | rows_checked=", length(content_rows), " | method=", method
              ))
            } else {
              applied_rules <- applied_rules + 1L
              rows_updated <- rows_updated + successful_extractions
            }
          }
        }, error = function(e) {
          emit_ratio_rule_warning(paste0(
            "Ratio rule failed for content type '", content_type,
            "' | method=", method, " | error=", e$message
          ))
        })
      }

      debug_log(
        sprintf(
          "Ratio rules applied: %d/%d | rows updated=%d | method=%s",
          applied_rules,
          total_rules,
          rows_updated,
          method_summary
        ),
        2
      )

      attr(df, "ratio_rule_status") <- list(
        total_rules = as.integer(total_rules),
        applied_rules = as.integer(applied_rules),
        rows_updated = as.integer(rows_updated),
        methods = method_summary
      )

      return(df)
    }

    build_content_assignment_counts <- function(content_values, unassigned_label = "<unassigned>") {
      content_labels <- as.character(content_values)
      content_labels[is.na(content_labels) | !nzchar(trimws(content_labels))] <- unassigned_label

      content_counts <- table(content_labels, useNA = "no")
      stats::setNames(as.integer(content_counts), names(content_counts))
    }

    apply_auto_assign_rules <- function(metadata_df) {
      req(metadata_df)

      tryCatch({
        debug_log("Starting auto-assign rules application", 2)
        df_working <- metadata_df

        # preserve & restore known samples option for pattern recognition
        .prev_dw_known_samples <- getOption("dw_known_samples", NULL)
        on.exit({
          options(dw_known_samples = .prev_dw_known_samples)
        }, add = TRUE)

        # WICHTIG: Erweiterte Prüfung auf alle Regel-Typen
        has_table_rules <- nrow(rv_table_rules_autoassign_dw()) > 0
        has_condition_rules <- nrow(rv_condition_rules_autoassign_dw()) > 0
        has_ratio_rules <- nrow(rv_rules_autoassign_dw()) > 0
        has_central_rules <- rules_loaded_centrally()

        if (!has_central_rules && !has_table_rules && !has_condition_rules && !has_ratio_rules) {
          debug_log("No rules available to apply", 2)
          return(df_working)
        }

        debug_log(paste("Available rules - Table:", has_table_rules, "Condition:", has_condition_rules,
                        "Ratio:", has_ratio_rules, "Central:", has_central_rules), 2)

        # ========================================
        # Step 1: Apply table rules (Content Assignment)
        # ========================================
        report_progress("Applying content rules")
        table_rules <- upgrade_rule_component(rv_table_rules_autoassign_dw(),"content")
        table_rules <- table_rules[order(table_rules$Priority,seq_len(nrow(table_rules))),,drop=FALSE]

        if (nrow(table_rules) > 0) {
          debug_log("STEP 1: Applying table rules (Content Assignment)", 2)
          for (i in seq_len(nrow(table_rules))) {
            rule <- table_rules[i, , drop = TRUE]

            df_working <- apply_rule_autoassign_dw(
              df_working,
              content_term = rule$Content,
              variant_id = rule$VariantId,
              include_pattern = cs_wrap(rule$Include),
              exclude_pattern = if (nzchar(rule$Exclude)) cs_wrap(rule$Exclude) else NULL
            )

          }
          # Transformation is owned by the winning header family, not by the
          # Content label.  Resolve it only after all last-match-wins selectors
          # have run, so a losing variant can never leave stale state behind.
          winning_variants <- attr(df_working,"variant_id",exact=TRUE)
          assigned_rows <- nzchar(winning_variants)
          df_working$Transformation[assigned_rows] <- NA_character_
          for (i in seq_len(nrow(table_rules))) {
            rule <- table_rules[i, , drop = TRUE]
            content_idx <- which(df_working$Content == rule$Content &
              winning_variants == rule$VariantId)
            if (length(content_idx) > 0L) {
              df_working$Transformation[content_idx] <- rule$Transformation
            }
          }
          debug_log(paste("Applied", nrow(table_rules), "table rules"), 2)

          # Debug: Show content assignment results
          content_counts <- build_content_assignment_counts(df_working$Content)
          debug_log("Content assignment results:", 2)
          for (content_type in names(content_counts)) {
            debug_log(paste("  ", content_type, ":", content_counts[content_type], "rows"), 2)
          }
        }

        # ========================================
        # Step 2: Apply condition rules (Sample Assignment)
        # ========================================
        report_progress("Applying sample rules")
        condition_rules <- upgrade_rule_component(rv_condition_rules_autoassign_dw(),"condition")

        if (nrow(condition_rules) > 0) {
          debug_log("STEP 2: Applying condition rules (Sample Assignment)", 2)

          condition_rules_evaluated <- 0L
          condition_rules_applied <- 0L
          condition_rules_skipped_missing_content <- 0L
          condition_rules_skipped_no_matches <- 0L
          condition_rules_errors <- 0L
          skipped_missing_content_types <- character(0)

          for (i in seq_len(nrow(condition_rules))) {
            rule <- condition_rules[i, , drop = TRUE]
            condition_rules_evaluated <- condition_rules_evaluated + 1L

            df_working <- apply_condition_autoassign_dw(
              df_working,
              lookup_col = rule$Content,
              variant_id = rule$VariantId,
              position = rule$Method,
              before = cs_wrap(rule$Before),
              after = cs_wrap(rule$After),
              phrase_pos = if (!is.na(rule$Pos)) rule$Pos else 1,
              sep = rule$Separators,
              setter = function(x) NULL
            )

            rule_status <- attr(df_working, "condition_rule_status", exact = TRUE)
            status <- if (!is.null(rule_status$status)) rule_status$status else "applied"

            if (identical(status, "applied")) {
              condition_rules_applied <- condition_rules_applied + 1L
            } else if (identical(status, "skipped_missing_content")) {
              condition_rules_skipped_missing_content <- condition_rules_skipped_missing_content + 1L
              skipped_missing_content_types <- c(skipped_missing_content_types, rule_status$lookup_col)
            } else if (identical(status, "skipped_no_matches")) {
              condition_rules_skipped_no_matches <- condition_rules_skipped_no_matches + 1L
            } else if (identical(status, "error")) {
              condition_rules_errors <- condition_rules_errors + 1L
            }
          }

          debug_log(
            sprintf(
              paste0(
                "Condition rules evaluated: %d | applied: %d | skipped missing content: %d",
                " | skipped no matches: %d | errors: %d"
              ),
              condition_rules_evaluated,
              condition_rules_applied,
              condition_rules_skipped_missing_content,
              condition_rules_skipped_no_matches,
              condition_rules_errors
            ),
            2
          )

          skipped_missing_content_types <- unique(stats::na.omit(skipped_missing_content_types))
          if (length(skipped_missing_content_types) > 0) {
            debug_log(
              paste0(
                "Condition rules skipped missing content types: ",
                paste(skipped_missing_content_types, collapse = ", ")
              ),
              2
            )
          }

        }

        # Finalize sample names once all condition rules have had the chance to
        # assign Options. This phase is deliberately independent of the
        # presence of condition rules: table/central rules can leave a
        # sample-bearing group with usable Options and missing Sample cells.
        df_working <- finalize_condition_sample_names(df_working)

        sample_diagnostics <- attr(df_working, "sample_name_diagnostics", exact = TRUE)
        if (is.data.frame(sample_diagnostics)) {
          debug_log(sprintf(
            "Sample-name diagnostics attached | rows: %d | columns: %d",
            nrow(sample_diagnostics), ncol(sample_diagnostics)
          ), 2)
        }

        .known <- character(0)
        if ("Options" %in% names(df_working)) {
          .known <- unique(stats::na.omit(df_working$Options))
          .known <- unique(trimws(as.character(.known)))
          .known <- .known[nzchar(.known)]
          options(dw_known_samples = .known)
          debug_log(paste("Registered", length(.known), "known samples for Pattern Recognition"), 2)

          # Pattern Recognition cannot resolve safely without known samples.
          ratio_rules <- rv_rules_autoassign_dw()
          has_pr <- nrow(ratio_rules) > 0 && any(grepl("^Pattern Recognition$", ratio_rules$Method, ignore.case = TRUE), na.rm = TRUE)
          if (has_pr && length(.known) == 0) {
            warn_auto_assign("Pattern Recognition selected but no samples are available; ratio extraction will abstain.")
          }

        } else {
          debug_log("No 'Options' column found after Step 2; Pattern Recognition will abstain.", 2)
        }

        # ========================================
        # Step 3: Apply ratio rules (Numerator/Denominator Assignment)
        # ========================================
        report_progress("Applying ratio rules")
        ratio_rules <- rv_rules_autoassign_dw()

        if (nrow(ratio_rules) > 0) {
          debug_log("STEP 3: Applying ratio rules (Numerator/Denominator Assignment)", 2)

          # Prüfe nochmal ob Content jetzt gesetzt ist
          ratio_content_types <- c("Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value")
          ratio_rows <- which(df_working$Content %in% ratio_content_types)

          if (length(ratio_rows) > 0) {
            debug_log(paste("Found", length(ratio_rows), "rows with ratio content types"), 4)

            # Show sample ratio columns
            sample_columns <- df_working$Column[ratio_rows[1:min(3, length(ratio_rows))]]
            debug_log(paste("Sample ratio columns:", paste(sample_columns, collapse = ", ")), 4)

            df_working <- apply_ratio_rules_fixed(df_working, ratio_rules)
          } else {
            available_content <- unique(df_working$Content[!is.na(df_working$Content)])
            content_summary <- if (length(available_content) > 0) {
              paste(available_content, collapse = ", ")
            } else {
              "none"
            }
            debug_log(paste0(
              "Ratio rules skipped: no ratio content columns found | content_types=",
              content_summary
            ), 2)
          }
        }

        # ========================================
        # Post-ratio relationship enrichment
        # ========================================
        #
        # Real ratio extraction has finished at this point. Rows whose biological
        # Numerator/Denominator were extractable already contain those real values.
        #
        # For rule-assigned ratio rows that remain blank, establish a deterministic
        # pairing surrogate only when a complete and unambiguous ratio/p-value/
        # adjusted-p-value triplet can be proven from ContrastId or header structure.
        #
        # Keep this OUTSIDE Auto RegEx inference: synthetic pairing identifiers are
        # compatibility metadata, not evidence extracted from the header.

        assigned_variants <- attr(
          df_working,
          "variant_id",
          exact = TRUE
        )

        pairing_rows <- integer()

        if (!is.null(assigned_variants) &&
            length(assigned_variants) == nrow(df_working)) {

          assigned_variants <- as.character(
            assigned_variants
          )

          pairing_rows <- which(
            !is.na(assigned_variants) &
              nzchar(assigned_variants)
          )
        }

        if (length(pairing_rows)) {

          df_working <-
            finalize_ratio_pairing_surrogates(
              df = df_working,
              eligible_rows = pairing_rows,
              debug_log = debug_log
            )
        }

        report_progress("Finalizing metadata")
        debug_log("All auto-assign rules applied successfully", 2)

        table_rules_final     <- rv_table_rules_autoassign_dw()
        condition_rules_final <- rv_condition_rules_autoassign_dw()
        ratio_rules_final     <- rv_rules_autoassign_dw()

        content_counts <- build_content_assignment_counts(df_working$Content)
        content_summary <- if (length(content_counts) > 0) {
          paste(paste0(names(content_counts), "=", content_counts), collapse = ", ")
        } else {
          "none"
        }

        ratio_methods_used <- if (nrow(ratio_rules_final) > 0 && "Method" %in% names(ratio_rules_final)) {
          paste(unique(stats::na.omit(as.character(ratio_rules_final$Method))), collapse = ", ")
        } else {
          "none"
        }

        debug_log(
          sprintf(
            paste0(
              "Auto-assign summary",
              " | Table rules: %d",
              " | Condition rules: %d",
              " | Ratio rules: %d",
              " | Ratio methods: %s",
              " | Central rules loaded: %s",
              " | Metadata rows processed: %d",
              " | Content distribution: {%s}"
            ),
            nrow(table_rules_final),
            nrow(condition_rules_final),
            nrow(ratio_rules_final),
            ratio_methods_used,
            as.character(isTRUE(rules_loaded_centrally())),
            nrow(df_working),
            content_summary
          ),
          level = 0
        )
        return(df_working)

      }, error = function(e) {
        debug_log(paste("Error applying auto-assign rules:", e$message), 1)
        if (exists("add_processing_log")) {
          add_processing_log("apply_rules", "error", e$message)
        }
        return(metadata_df)
      })
    }


  list(
    apply_rule_autoassign_dw = apply_rule_autoassign_dw,
    # Public, domain-oriented name used by inference/runtime integrations.
    apply_condition_rule = apply_condition_autoassign_dw,
    apply_condition_autoassign_dw = apply_condition_autoassign_dw,
    finalize_condition_sample_names = finalize_condition_sample_names,
    apply_ratio_rules_fixed = apply_ratio_rules_fixed,
    apply_auto_assign_rules = apply_auto_assign_rules
  )
}
