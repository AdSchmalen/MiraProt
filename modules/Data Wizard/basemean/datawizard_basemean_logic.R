# ==============================================================================
# File: modules/Data Wizard/basemean/datawizard_basemean_logic.R
#
# Purpose:
#   Contains all pure logic functions for the Basemean submodule of the
#   Data Wizard. These functions compute basemean columns, extend metadata,
#   and clear basemean columns. They carry no Shiny dependency.
#
# Architectural Role:
#   Logic layer of the basemean module. Called by observer functions defined
#   in datawizard_basemean_observer.R. Sourced into modEnv via
#   datawizard_basemean.R so that observer code can call these functions
#   directly by name.
#
# Structure:
#   1. compute_basemean()         - Calculate rowMeans for selected columns
#   2. update_basemean_metadata() - Append a metadata row for a new basemean
#                                   column
#   3. clear_basemean_columns()   - Remove all Basemean columns and their
#                                   metadata rows
#
# Notes for future developers:
#   - Every function in this file must remain Shiny-free (no input, output,
#     session, reactive, observe). This preserves unit-testability.
#   - All functions accept debug_log as the last argument. Pass the debug_log
#     closure from modBasemeanServer when calling these functions.
#   - Return values follow the convention: named list on success, NULL (or the
#     original argument) on error.
#   - Do not introduce global state, side-effects, or reactive wrappers here.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Compute basemean column
# ------------------------------------------------------------------------------

#' Calculate a basemean column for the given data and metadata.
#'
#' @param data           Data frame with abundance columns.
#' @param def            Metadata data frame with Content, Column, Sample columns.
#' @param abundance_type Character; the abundance type to select columns for.
#' @param samples        Character vector; the sample names to include.
#' @param suffix         Character; optional suffix appended to "Basemean_".
#' @param debug_log      Logging function with signature (message, level).
#' @return Named list with elements `data` (updated data frame) and `new_col`
#'   (character column name), or NULL on error.
compute_basemean <- function(data, def, abundance_type, samples, suffix, debug_log) {
  tryCatch({
    if (is.null(data) || is.null(def)) {
      stop("Missing data or def argument")
    }

    idx <- which(def$Content == abundance_type & def$Sample %in% samples)
    if (length(idx) == 0) {
      stop("No matching columns found for the selected abundance type and samples.")
    }

    new_col    <- if (nzchar(suffix)) paste0("Basemean_", suffix) else "Basemean"
    new_values <- rowMeans(data[, idx, drop = FALSE], na.rm = TRUE)

    if (new_col %in% names(data)) {
      data[[new_col]] <- new_values
      debug_log(paste("Overwrote existing Basemean column:", new_col), 1)
    } else {
      data[[new_col]] <- new_values
      debug_log(paste("Added new Basemean column:", new_col), 1)
    }

    samples_str <- paste(samples, collapse = ", ")

    debug_log(
      sprintf(
        "Basemean summary | Abundance type: %s | Selected samples (%d): [%s] | New column created: %s",
        abundance_type,
        length(samples),
        if (length(samples) == 0) "none" else samples_str,
        new_col
      ),
      level = 0
    )

    list(data = data, new_col = new_col)

  }, error = function(e) {
    debug_log(paste("compute_basemean failed:", e$message), 1)
    NULL
  })
}


# ------------------------------------------------------------------------------
# 2. Update metadata with a new basemean row
# ------------------------------------------------------------------------------

#' Append a metadata row for a newly created basemean column.
#'
#' @param def              Metadata data frame.
#' @param data             Data frame; used only for column-count consistency check.
#' @param new_col          Character; column name of the new basemean column.
#' @param selected_type    Character; abundance type used for the calculation.
#' @param selected_samples Character vector; samples used for the calculation.
#' @param debug_log        Logging function with signature (message, level).
#' @return Updated metadata data frame. Returns the original def on error.
update_basemean_metadata <- function(def, data, new_col, selected_type,
                                     selected_samples, debug_log) {
  tryCatch({
    if (is.null(def) || is.null(data)) {
      debug_log("update_basemean_metadata: def or data is NULL — skipping", 1)
      return(def)
    }

    if (ncol(data) != nrow(def)) {
      debug_log(paste(
        "update_basemean_metadata: column/row count mismatch — data cols:", ncol(data),
        "def rows:", nrow(def), "— appending anyway"
      ), 2)
    }

    required_cols <- c("Content", "Column", "Sample")
    for (col in required_cols) {
      if (!(col %in% names(def))) {
        def[[col]] <- NA_character_
      }
    }

    new_row <- data.frame(
      Content = "Basemean",
      Column  = new_col,
      Sample  = paste0(selected_samples, collapse = ", "),
      stringsAsFactors = FALSE
    )

    for (col in setdiff(names(def), names(new_row))) {
      new_row[[col]] <- NA
    }
    new_row <- new_row[names(def)]
    def     <- rbind(def, new_row)

    debug_log(paste(
      "Metadata extended: added", new_col, "for", selected_type,
      "with", length(selected_samples), "samples — total rows:", nrow(def)
    ), 2)

    def

  }, error = function(e) {
    debug_log(paste("update_basemean_metadata failed:", e$message), 1)
    def
  })
}


# ------------------------------------------------------------------------------
# 3. Clear all Basemean columns
# ------------------------------------------------------------------------------

#' Remove all Basemean columns from data and their rows from metadata.
#'
#' @param data      Data frame with abundance columns.
#' @param def       Metadata data frame.
#' @param debug_log Logging function with signature (message, level).
#' @return Named list with elements `data` (updated) and `def` (updated).
#'   Returns the original inputs unchanged if no Basemean columns exist.
clear_basemean_columns <- function(data, def, debug_log) {
  tryCatch({
    basemean_cols <- grep("^Basemean", names(data), value = TRUE)

    if (length(basemean_cols) == 0) {
      debug_log("clear_basemean_columns: no Basemean columns found", 2)
      return(list(data = data, def = def))
    }

    data <- data[, !(names(data) %in% basemean_cols), drop = FALSE]
    def  <- def[!def$Content %in% "Basemean", , drop = FALSE]

    debug_log(paste("Cleared", length(basemean_cols), "Basemean column(s)"), 1)

    list(data = data, def = def)

  }, error = function(e) {
    debug_log(paste("clear_basemean_columns failed:", e$message), 1)
    list(data = data, def = def)
  })
}
