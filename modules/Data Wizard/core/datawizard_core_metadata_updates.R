# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_metadata_updates.R
# Purpose:
#   Provide the core metadata updates portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

#' Create metadata update helper functions for processed columns
#' @param core_values list of core reactive values
#' @return list of metadata update functions
create_metadata_update_functions <- function(core_values) {

  # Helper function to create ratio metadata rows
  create_ratio_metadata_rows <- function(new_cols, applied_configs) {
    new_metadata_rows <- data.frame(
      Column = new_cols,
      Content = NA_character_,
      Options = NA_character_,
      Numerator = NA_character_,
      Denominator = NA_character_,
      Transformation = NA_character_,
      Sample = NA_character_,
      stringsAsFactors = FALSE
    )

    set_ratio_groups <- function(row_index, comparison_name) {
      if (!is.null(applied_configs) && nrow(applied_configs) > 0) {
        config_match <- which(applied_configs$Title == comparison_name)
        if (length(config_match) > 0) {
          config_row <- applied_configs[config_match[1], ]
          numerator_groups <- config_row$Numerator[[1]]
          denominator_groups <- config_row$Denominator[[1]]

          new_metadata_rows$Numerator[row_index] <<- paste(numerator_groups, collapse = ", ")
          new_metadata_rows$Denominator[row_index] <<- paste(denominator_groups, collapse = ", ")
        }
      }
    }

    for (i in seq_len(nrow(new_metadata_rows))) {
      col_name <- new_metadata_rows$Column[i]

      if (grepl("_Abundance Ratio$", col_name)) {
        comparison_name <- sub("_Abundance Ratio$", "", col_name)
        new_metadata_rows$Content[i] <- "Abundance Ratio"
        new_metadata_rows$Options[i] <- "Ratio"
        new_metadata_rows$Transformation[i] <- "None"
        set_ratio_groups(i, comparison_name)

      } else if (grepl("_Abundance Ratio p-Value$", col_name)) {
        comparison_name <- sub("_Abundance Ratio p-Value$", "", col_name)
        new_metadata_rows$Content[i] <- "Abundance Ratio p-Value"
        new_metadata_rows$Options[i] <- "Ratio"
        new_metadata_rows$Transformation[i] <- "None"
        set_ratio_groups(i, comparison_name)

      } else if (grepl("_Abundance Ratio Adj\\. p-Value$", col_name)) {
        comparison_name <- sub("_Abundance Ratio Adj\\. p-Value$", "", col_name)
        new_metadata_rows$Content[i] <- "Abundance Ratio Adj. p-Value"
        new_metadata_rows$Options[i] <- "Ratio"
        new_metadata_rows$Transformation[i] <- "None"
        set_ratio_groups(i, comparison_name)

      } else if (grepl("^Ratio_", col_name)) {
        if (grepl("Abundance\\.Ratio", col_name)) {
          new_metadata_rows$Content[i] <- "Abundance Ratio"
        } else if (grepl("p\\.value|P\\.Value", col_name)) {
          new_metadata_rows$Content[i] <- "Abundance Ratio p-Value"
        } else if (grepl("adj\\.p\\.value|adj\\.P\\.Val", col_name)) {
          new_metadata_rows$Content[i] <- "Abundance Ratio Adj. p-Value"
        } else {
          new_metadata_rows$Content[i] <- "Ratio Analysis Result"
        }
        new_metadata_rows$Options[i] <- "Ratio"
        new_metadata_rows$Transformation[i] <- "None"
      } else {
        new_metadata_rows$Content[i] <- "Unknown Ratio Column"
        new_metadata_rows$Options[i] <- "Ratio"
        new_metadata_rows$Transformation[i] <- "None"
      }
    }

    return(new_metadata_rows)
  }

  list(
    update_metadata_for_ratio_columns = function(new_data, ratios_out) {
      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta) || nrow(current_meta) == 0 || is.null(new_data)) return()

      tryCatch({
        current_cols <- current_meta$Column
        new_cols <- setdiff(names(new_data), current_cols)

        if (length(new_cols) > 0) {
          debug_log(paste("Adding metadata for", length(new_cols), "new ratio columns"), level = 2)

          applied_configs <- NULL
          if (!is.null(ratios_out)) {
            applied_configs <- safe_module_call(ratios_out$get_last_applied_configs,
                                                default_return = NULL,
                                                context = "ratio_applied_configs")
          }

          new_metadata_rows <- create_ratio_metadata_rows(new_cols, applied_configs)
          current_meta <- datawizard_drop_deprecated_metadata_columns(current_meta)
          for (field in setdiff(names(current_meta), names(new_metadata_rows))) new_metadata_rows[[field]] <- NA
          new_metadata_rows <- new_metadata_rows[, names(current_meta), drop = FALSE]
          updated_meta <- rbind(current_meta, new_metadata_rows)
          core_values$handson_metadata(updated_meta)
        }
      }, error = function(e) {
        debug_log(paste("Could not update metadata after ratio analysis:", e$message), level = 1)
        showNotification("Warning: Metadata not updated after ratio analysis. Please check manually.",
                         type = "warning", duration = 5)
      })
    },

    # Batch correction preserves the source content name and prefixes it with
    # "Batch Corrected".  In particular, the canonical abundance mappings are:
    #   Raw Abundance        -> Batch Corrected Raw Abundance
    #   Normalized Abundance -> Batch Corrected Normalized Abundance
    # Downstream dropdowns intentionally enumerate those canonical values; this
    # metadata propagation is not a reason to match arbitrary "Abundance" text.
    update_metadata_for_batch_corrected_columns = function(new_data) {
      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta) || nrow(current_meta) == 0 || is.null(new_data)) return()

      tryCatch({
        current_cols <- current_meta$Column
        new_cols <- setdiff(names(new_data), current_cols)

        if (length(new_cols) > 0) {
          debug_log(paste("Adding metadata for", length(new_cols), "new batch corrected columns"), level = 2)

          new_metadata_rows <- data.frame(
            Column = new_cols,
            Content = NA_character_,
            Options = NA_character_,
            Numerator = NA_character_,
            Denominator = NA_character_,
            Transformation = NA_character_,
            Sample = NA_character_,
            stringsAsFactors = FALSE
          )

          for (i in seq_len(nrow(new_metadata_rows))) {
            col_name <- new_metadata_rows$Column[i]

            if (grepl("^Batch Corrected ", col_name)) {
              original_name <- gsub("^Batch Corrected ", "", col_name)
              original_idx <- which(current_meta$Column == original_name)

              if (length(original_idx) > 0) {
                original_content <- current_meta$Content[original_idx[1]]
                if (!is.na(original_content) && nzchar(original_content)) {
                  new_metadata_rows$Content[i] <- paste("Batch Corrected", original_content)
                } else {
                  new_metadata_rows$Content[i] <- "Batch Corrected Abundance"
                }
                new_metadata_rows$Options[i] <- current_meta$Options[original_idx[1]]
                new_metadata_rows$Sample[i] <- current_meta$Sample[original_idx[1]]
                new_metadata_rows$Transformation[i] <- current_meta$Transformation[original_idx[1]]
                new_metadata_rows$Numerator[i] <- current_meta$Numerator[original_idx[1]]
                new_metadata_rows$Denominator[i] <- current_meta$Denominator[original_idx[1]]
              } else {
                new_metadata_rows$Content[i] <- "Batch Corrected Abundance"
              }
            } else {
              new_metadata_rows$Content[i] <- "Batch Corrected Data"
            }
          }

          updated_meta <- rbind(current_meta, new_metadata_rows)
          core_values$handson_metadata(updated_meta)
        }
      }, error = function(e) {
        debug_log(paste("Could not update metadata after batch correction:", e$message), level = 1)
        showNotification("Warning: Metadata not updated after batch correction. Please check manually.",
                         type = "warning", duration = 5)
      })
    },

    # Imputation likewise preserves the complete source content name and adds
    # an "Imputed" prefix.  For batch-corrected canonical content this gives:
    #   Batch Corrected Raw Abundance
    #     -> Imputed Batch Corrected Raw Abundance
    #   Batch Corrected Normalized Abundance
    #     -> Imputed Batch Corrected Normalized Abundance
    update_metadata_for_imputed_columns = function(new_data) {
      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta) || nrow(current_meta) == 0 || is.null(new_data)) {
        debug_log("Cannot update metadata - missing data or metadata", level = 1)
        return()
      }

      tryCatch({
        current_cols <- current_meta$Column
        new_cols <- setdiff(names(new_data), current_cols)

        if (length(new_cols) > 0) {
          debug_log(paste("Adding metadata for", length(new_cols), "new columns"), level = 1)

          # Create comprehensive metadata for new columns
          new_metadata_rows <- data.frame(
            Column = new_cols,
            Content = NA_character_,
            Options = NA_character_,
            Numerator = NA_character_,
            Denominator = NA_character_,
            Transformation = NA_character_,
            Sample = NA_character_,
            stringsAsFactors = FALSE
          )

          # Enhanced mapping for imputed columns
          for (i in seq_len(nrow(new_metadata_rows))) {
            col_name <- new_metadata_rows$Column[i]

            if (grepl("^Imputed ", col_name)) {
              # Extract original column name, stripping any "_dupN" suffix that
              # was added when the same abundance type was imputed more than once.
              original_name <- gsub("^Imputed ", "", col_name)
              dup_suffix    <- ""
              dup_match     <- regexpr("_dup[0-9]+$", original_name)
              if (dup_match > 0L) {
                dup_suffix    <- regmatches(original_name, dup_match)
                original_name <- substring(original_name, 1L, dup_match - 1L)
              }
              original_idx <- which(current_meta$Column == original_name)

              if (length(original_idx) > 0) {
                # Copy metadata from original column
                original_content <- current_meta$Content[original_idx[1]]
                new_metadata_rows$Content[i] <- paste("Imputed", original_content)
                new_metadata_rows$Options[i] <- current_meta$Options[original_idx[1]]
                # Append the dup suffix to the Sample entry so it remains unique
                # across repeated imputations of the same abundance type.
                orig_sample <- current_meta$Sample[original_idx[1]]
                new_metadata_rows$Sample[i] <- if (!is.na(orig_sample) && nchar(orig_sample) > 0L) {
                  paste0(orig_sample, dup_suffix)
                } else {
                  orig_sample
                }
                # Imputed columns are always in raw domain: data was retransformed before
                # imputation and no back-transformation is applied, so Transformation must
                # be "None" regardless of the original column's transformation label.
                new_metadata_rows$Transformation[i] <- "None"
                new_metadata_rows$Numerator[i] <- current_meta$Numerator[original_idx[1]]
                new_metadata_rows$Denominator[i] <- current_meta$Denominator[original_idx[1]]

                debug_log(paste("Mapped imputed column:", col_name, "->", new_metadata_rows$Content[i]), level = 2)
              } else {
                new_metadata_rows$Content[i] <- "Imputed Data"
                debug_log(paste("Could not find original for:", col_name, "- using default"), level = 1)
              }
            } else {
              new_metadata_rows$Content[i] <- "Additional Data"
            }
          }

          # Update metadata
          updated_meta <- rbind(current_meta, new_metadata_rows)
          core_values$handson_metadata(updated_meta)

          # Also update final processed metadata if it exists
          if (!is.null(core_values$final_processed_metadata())) {
            core_values$final_processed_metadata(updated_meta)
          }

          debug_log("Metadata successfully extended with imputed columns", level = 1)

          # Notify user
          showNotification(
            paste("Metadata updated with", length(new_cols), "imputed columns"),
            type = "message",
            duration = 3
          )

        } else {
          debug_log("No new columns to add to metadata", level = 2)
        }

      }, error = function(e) {
        debug_log(paste("Error updating metadata for imputed columns:", e$message), level = 1)
        showNotification("Warning: Could not update metadata for imputed columns. Please check manually.",
                         type = "warning", duration = 5)
      })
    },

    update_metadata_for_new_columns = function(new_data, operation_name = "Data Processing") {
      if (is.null(new_data)) return()

      tryCatch({
        # Step 1: Get EXACT final column names from actual data
        final_data_columns <- names(new_data)
        debug_log(paste(operation_name, "SYNC: Final data has", length(final_data_columns), "columns"), level = 1)

        current_meta <- core_values$handson_metadata()

        if (is.null(current_meta) || nrow(current_meta) == 0) {
          # Create completely new metadata
          debug_log(paste(operation_name, "SYNC: Creating new metadata from scratch"), level = 1)

          new_metadata <- data.frame(
            Column = final_data_columns,
            Content = rep(NA_character_, length(final_data_columns)),
            Options = rep(NA_character_, length(final_data_columns)),
            Numerator = rep(NA_character_, length(final_data_columns)),
            Denominator = rep(NA_character_, length(final_data_columns)),
            Transformation = rep(NA_character_, length(final_data_columns)),
            Sample = rep(NA_character_, length(final_data_columns)),
            stringsAsFactors = FALSE
          )

          # Set Row Index if exists
          row_index_idx <- which(new_metadata$Column == "Row Index")
          if (length(row_index_idx) > 0) {
            new_metadata$Content[row_index_idx[1]] <- "Row Index"
          }

        } else {
          # Step 2: Preserve existing metadata for columns that still exist
          debug_log(paste(operation_name, "SYNC: Preserving existing metadata where possible"), level = 1)

          existing_columns <- current_meta$Column
          preserved_columns <- intersect(final_data_columns, existing_columns)
          new_columns <- setdiff(final_data_columns, existing_columns)

          debug_log(paste(operation_name, "SYNC: Preserving", length(preserved_columns), "existing columns"), level = 2)
          debug_log(paste(operation_name, "SYNC: Creating", length(new_columns), "new columns"), level = 2)

          # Create base metadata structure with exact column order from data
          new_metadata <- data.frame(
            Column = final_data_columns,
            Content = rep(NA_character_, length(final_data_columns)),
            Options = rep(NA_character_, length(final_data_columns)),
            Numerator = rep(NA_character_, length(final_data_columns)),
            Denominator = rep(NA_character_, length(final_data_columns)),
            Transformation = rep(NA_character_, length(final_data_columns)),
            Sample = rep(NA_character_, length(final_data_columns)),
            stringsAsFactors = FALSE
          )

          # Step 3: Copy existing metadata for preserved columns
          for (col in preserved_columns) {
            old_idx <- which(current_meta$Column == col)[1]
            new_idx <- which(new_metadata$Column == col)[1]

            if (!is.na(old_idx) && !is.na(new_idx)) {
              new_metadata$Content[new_idx] <- current_meta$Content[old_idx]
              new_metadata$Options[new_idx] <- current_meta$Options[old_idx]
              new_metadata$Numerator[new_idx] <- current_meta$Numerator[old_idx]
              new_metadata$Denominator[new_idx] <- current_meta$Denominator[old_idx]
              new_metadata$Transformation[new_idx] <- current_meta$Transformation[old_idx]
              new_metadata$Sample[new_idx] <- current_meta$Sample[old_idx]
              if (grepl("^Imputed ", col)) new_metadata$Transformation[new_idx] <- "None"
            }
          }

          # Step 4: Set smart defaults for new columns
          if (length(new_columns) > 0) {
            for (col in new_columns) {
              new_idx <- which(new_metadata$Column == col)[1]

              if (!is.na(new_idx)) {
                if (grepl("^Merged_", col)) {
                  new_metadata$Content[new_idx] <- paste("Merged Data")
                  new_metadata$Options[new_idx] <- "Merged Data"
                } else if (grepl("^Imputed ", col)) {
                  new_metadata$Content[new_idx] <- "Imputed Data"
                  new_metadata$Transformation[new_idx] <- "None"
                } else if (grepl("^Batch Corrected ", col)) {
                  new_metadata$Content[new_idx] <- "Batch Corrected Data"
                } else if (grepl("^Pivoted_", col)) {
                  new_metadata$Content[new_idx] <- "Pivoted Data"
                } else if (grepl("^Ratio_", col)) {
                  new_metadata$Content[new_idx] <- "Ratio Analysis"
                  new_metadata$Options[new_idx] <- "Ratio"
                } else {
                  new_metadata$Content[new_idx] <- "Processed Data"
                }

              }
            }
          }
        }

        # Step 5: FINAL VALIDATION - Guarantee exact match
        if (!identical(new_metadata$Column, final_data_columns)) {
          debug_log(paste(operation_name, "SYNC: CRITICAL ERROR - Column order mismatch!"), level = 1)
          # Force exact order match
          new_metadata <- new_metadata[match(final_data_columns, new_metadata$Column), ]
        }

        # Final check
        if (length(new_metadata$Column) != length(final_data_columns) ||
            !identical(new_metadata$Column, final_data_columns)) {
          stop("SYNC FAILED: Metadata columns do not match data columns exactly")
        }

        # Update metadata
        core_values$handson_metadata(new_metadata)

        debug_log(paste(operation_name, "SYNC: SUCCESS - Metadata exactly matches data:"), level = 1)
        debug_log(paste(operation_name, "SYNC: Data columns:", length(final_data_columns)), level = 1)
        debug_log(paste(operation_name, "SYNC: Metadata rows:", nrow(new_metadata)), level = 1)

      }, error = function(e) {
        debug_log(paste(operation_name, "SYNC: ERROR -", e$message), level = 1)
        showNotification(paste("Error syncing metadata:", e$message), type = "error", duration = 5)
      })
    },

    update_metadata_for_pivoted_data = function(new_data) {
      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta) || nrow(current_meta) == 0 || is.null(new_data)) return()

      tryCatch({
        new_data_cols <- names(new_data)

        if (length(new_data_cols) > 1000) {
          showNotification(paste("Warning: Pivot created", length(new_data_cols), "columns. This may impact performance."),
                           type = "warning", duration = 8)
        }

        new_metadata <- data.frame(
          Column = new_data_cols,
          Content = NA_character_,
          Options = NA_character_,
          Numerator = NA_character_,
          Denominator = NA_character_,
          Transformation = NA_character_,
          Sample = NA_character_,
          stringsAsFactors = FALSE
        )

        for (i in seq_len(nrow(new_metadata))) {
          col_name <- new_metadata$Column[i]
          old_meta_idx <- which(current_meta$Column == col_name)

          if (length(old_meta_idx) > 0) {
            old_row <- current_meta[old_meta_idx[1], ]
            new_metadata$Content[i] <- old_row$Content
            new_metadata$Options[i] <- old_row$Options
            new_metadata$Numerator[i] <- old_row$Numerator
            new_metadata$Denominator[i] <- old_row$Denominator
            new_metadata$Transformation[i] <- old_row$Transformation
            new_metadata$Sample[i] <- old_row$Sample
          }
        }

        core_values$handson_metadata(new_metadata)

        retained_cols <- sum(!is.na(new_metadata$Content))
        new_cols <- sum(is.na(new_metadata$Content))
        debug_log(paste("Metadata updated for pivot:", retained_cols, "retained,", new_cols, "new columns"), level = 2)

      }, error = function(e) {
        debug_log(paste("Could not update metadata after pivot operation:", e$message), level = 1)
        showNotification("Warning: Metadata not updated after pivot operation. Please check manually.",
                         type = "warning", duration = 5)
      })
    },

    update_metadata_for_basemean_columns = function(new_data, basemean_out = NULL) {
      if (is.null(new_data)) return()

      tryCatch({
        current_meta <- core_values$handson_metadata()
        data_cols <- names(new_data)
        basemean_cols <- data_cols[grepl("^Basemean", data_cols)]
        non_basemean_cols <- setdiff(data_cols, basemean_cols)

        # Detect stale metadata: if the existing metadata's non-Basemean
        # columns no longer align with the current data's non-Basemean columns
        # (e.g. after a sheet switch that reused this metadata), we must not
        # rbind new Basemean rows onto the wrong column set. Rebuild a fresh
        # skeleton from the actual data, preserving any prior assignments
        # keyed by column name.
        meta_non_basemean <- if (!is.null(current_meta) && nrow(current_meta) > 0) {
          current_meta$Column[!grepl("^Basemean", current_meta$Column)]
        } else {
          character(0)
        }

        stale_meta <- is.null(current_meta) || nrow(current_meta) == 0 ||
          !identical(as.character(meta_non_basemean),
                     as.character(non_basemean_cols))

        if (stale_meta) {
          debug_log("Basemean metadata update: rebuilding metadata because current_meta does not match new_data columns", level = 1)

          rebuilt <- data.frame(
            Column         = data_cols,
            Content        = rep(NA_character_, length(data_cols)),
            Options        = rep(NA_character_, length(data_cols)),
            Numerator      = rep(NA_character_, length(data_cols)),
            Denominator    = rep(NA_character_, length(data_cols)),
            Transformation = rep(NA_character_, length(data_cols)),
            Sample         = rep(NA_character_, length(data_cols)),
            stringsAsFactors = FALSE
          )

          # Preserve prior per-column assignments where the column still exists.
          if (!is.null(current_meta) && nrow(current_meta) > 0) {
            keep <- match(rebuilt$Column, current_meta$Column)
            has_prior <- !is.na(keep)
            if (any(has_prior)) {
              prior <- current_meta[keep[has_prior], , drop = FALSE]
              for (col in intersect(names(rebuilt), names(prior))) {
                rebuilt[[col]][has_prior] <- prior[[col]]
              }
            }
          }

          # Apply Basemean defaults to any Basemean columns that do not
          # already carry a Content assignment from a preserved prior row.
          bm_rows <- which(grepl("^Basemean", rebuilt$Column))
          needs_default <- bm_rows[is.na(rebuilt$Content[bm_rows]) |
                                     !nzchar(rebuilt$Content[bm_rows])]
          if (length(needs_default) > 0) {
            rebuilt$Content[needs_default]        <- "Basemean"
            rebuilt$Transformation[needs_default] <- "Average"
          }

          core_values$handson_metadata(rebuilt)
          if (!is.null(core_values$final_processed_metadata) &&
              !is.null(core_values$final_processed_metadata())) {
            core_values$final_processed_metadata(rebuilt)
          }
          debug_log(paste("Metadata rebuilt from current data:", nrow(rebuilt), "rows"), level = 1)
          return()
        }

        # Happy path: metadata already matches non-Basemean data columns;
        # just append rows for any newly introduced Basemean columns.
        new_cols <- setdiff(basemean_cols, current_meta$Column)

        if (length(new_cols) > 0) {
          debug_log(paste("Adding metadata for", length(new_cols), "new Basemean columns"), level = 2)

          new_metadata_rows <- data.frame(
            Column = new_cols,
            Content = rep("Basemean", length(new_cols)),
            Options = rep(NA_character_, length(new_cols)),
            Numerator = rep(NA_character_, length(new_cols)),
            Denominator = rep(NA_character_, length(new_cols)),
            Transformation = rep("Average", length(new_cols)),
            Sample = rep(NA_character_, length(new_cols)),
            stringsAsFactors = FALSE
          )

          updated_meta <- rbind(current_meta, new_metadata_rows)
          core_values$handson_metadata(updated_meta)
          if (!is.null(core_values$final_processed_metadata) &&
              !is.null(core_values$final_processed_metadata())) {
            core_values$final_processed_metadata(updated_meta)
          }

          debug_log(paste("Metadata updated for", length(new_cols), "Basemean columns"), level = 1)
        }
      }, error = function(e) {
        debug_log(paste("Could not update metadata for Basemean columns:", e$message), level = 1)
        showNotification("Warning: Metadata not updated after Basemean operation.", type = "warning", duration = 5)
      })
    },

    update_metadata_for_annotation_columns = function(new_data) {
      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta) || nrow(current_meta) == 0 || is.null(new_data)) return()

      tryCatch({
        current_cols <- current_meta$Column
        new_cols <- setdiff(names(new_data), current_cols)

        if (length(new_cols) > 0) {
          debug_log(paste("Adding metadata for", length(new_cols), "new Annotation columns"), level = 2)

          new_metadata_rows <- data.frame(
            Column = new_cols,
            Content = rep("Identifier", length(new_cols)),
            Options = new_cols,
            Numerator = rep(NA_character_, length(new_cols)),
            Denominator = rep(NA_character_, length(new_cols)),
            Transformation = rep(NA_character_, length(new_cols)),
            Sample = rep(NA_character_, length(new_cols)),
            stringsAsFactors = FALSE
          )

          updated_meta <- rbind(current_meta, new_metadata_rows)
          core_values$handson_metadata(updated_meta)

          if (!is.null(core_values$final_processed_metadata())) {
            core_values$final_processed_metadata(updated_meta)
          }

          debug_log(paste("Metadata updated for", length(new_cols), "Annotation columns"), level = 1)
        }
      }, error = function(e) {
        debug_log(paste("Could not update metadata for Annotation columns:", e$message), level = 1)
        showNotification("Warning: Metadata not updated after Annotation operation.", type = "warning", duration = 5)
      })
    }
  )
}
