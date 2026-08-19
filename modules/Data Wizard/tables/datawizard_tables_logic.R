# ============================================================================
# MiraProt File Contract: modules/Data Wizard/tables/datawizard_tables_logic.R
# Purpose:
#   Provide the tables logic portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Tables implementation unit loaded by the historical datawizard_tables.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   One module-scoped Tables context owns local table and metadata presentation state; canonical data remains externally owned.
# Mutation Authority:
#   Only registered handlers using that single shared context and injected setters may request canonical mutations.
# Source-Order Assumptions:
#   Source through datawizard_tables.R in its declared dependency order; observer phases are hydration, rendering/mutations, then metadata editing.
# Session/Restore Implications:
#   Tables rehydrates from injected canonical reactives; it must not create an independent session-restore authority.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# ==============================================================================
# File: modules/Data Wizard/tables/datawizard_tables_logic.R
#
# Purpose:
#   Contains all pure logic functions for the Tables submodule of the
#   Data Wizard. These functions handle data truncation, content-type color
#   mapping, and color shade generation. They carry no Shiny dependency and
#   can be unit-tested in isolation.
#
# Architectural Role:
#   Logic layer of the tables module. Called by observer/output functions
#   defined in datawizard_tables_observer.R. Sourced into modEnv via
#   datawizard_tables.R so that all helper functions are available by name
#   inside the moduleServer() closure.
#
# Structure:
#   1. truncate_text()                - Truncate character cells for display
#   2. get_base_color()               - Resolve hex color for a content type
#   3. should_use_shading()           - Decide whether option-based shading applies
#   4. generate_color_shades()        - Produce lighter-to-darker color variants
#   5. create_content_color_mapping() - Build column-to-color mapping from metadata
#   6. build_datawizard_table_options()- Build shared DT viewer options
#   7. build_data_preview()           - Prepare a data frame for DT display
#   8. validate_metadata_health()     - Report metadata consistency issues
#
# Notes for future developers:
#   - Every function in this file must remain Shiny-free (no input, output,
#     session, reactive, observe). This preserves unit-testability.
#   - Functions that require logging accept debug_log as the last argument.
#     Pass the debug_log closure from modDataTablesServer() when calling them.
#   - Do not introduce global state, side-effects, or reactive wrappers here.
#   - The color palette is defined inside create_content_color_mapping() so
#     that it is co-located with the function that uses it. Extend the palette
#     there when new content types are added.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Truncate text
# ------------------------------------------------------------------------------

#' Preserve source cells used by DataTables filtering and sorting.
#'
#' @param df         Data frame to process.
#' @param max_length Retained for backwards compatibility; truncation is now
#'                   performed by `build_datawizard_text_renderer()`.
#' @return The input data frame, unchanged.
truncate_text <- function(df, max_length = 50) {
  # Never shorten the R values supplied to DT. Server-side filtering operates
  # on this frame, so changing a character cell here makes suffix text
  # unsearchable and can also coerce factor/non-character columns.
  df
}

#' Build a safe orthogonal DataTables renderer for long character cells.
#'
#' DataTables calls renderers with an operation type. The complete source value
#' is returned for filtering and sorting; only the browser display is shortened
#' and HTML-escaped.
#'
#' @param max_length Maximum number of Unicode characters shown in a cell.
#' @return JavaScript function source suitable for `DT::JS()`.
build_datawizard_text_renderer <- function(max_length = 50L) {
  max_length <- as.integer(max_length[[1L]])
  if (is.na(max_length) || max_length < 1L) stop("max_length must be a positive integer")
  sprintf(
    paste0(
      "function(data,type,row,meta){",
      "if(type!=='display'||data===null||data===undefined)return data;",
      "var value=String(data),chars=Array.from(value);",
      "if(chars.length>%d)value=chars.slice(0,%d).join('')+' [...]';",
      "var node=document.createElement('div');node.textContent=value;return node.innerHTML;",
      "}"
    ),
    max_length, max_length
  )
}


# ------------------------------------------------------------------------------
# 2. Build shared DataTables viewer options
# ------------------------------------------------------------------------------

DATAWIZARD_TABLE_COMPACT_HEIGHT <- "340px"

#' Build the common DataTables options for Data Wizard data viewers.
#'
#' Searching remains enabled for DataTables' API, while the DOM layout omits
#' the unused global-search and export controls. The supplied callback is kept
#' as an argument so this helper remains independent of Shiny and DT.
#'
#' @param init_complete Browser-ready callback to run after DT initialization.
#' @param text_renderer Optional orthogonal renderer for character/factor cells.
#' @param text_columns Zero-based DataTables indexes receiving `text_renderer`.
#' @return Named list suitable for the `options` argument to DT::datatable().
build_datawizard_table_options <- function(init_complete, text_renderer = NULL,
                                           text_columns = integer(0),
                                           pagination_callback = NULL) {
  options <- list(
    pageLength = DATAWIZARD_TABLE_PAGE_LENGTH,
    paging = TRUE,
    searching = TRUE,
    scrollX = TRUE,
    scrollY = DATAWIZARD_TABLE_COMPACT_HEIGHT,
    scrollCollapse = TRUE,
    deferRender = TRUE,
    processing = TRUE,
    ordering = FALSE,
    autoWidth = TRUE,
    destroy = FALSE,
    dom = "tip",
    initComplete = init_complete
  )
  if (!is.null(pagination_callback)) options$drawCallback <- pagination_callback
  if (!is.null(text_renderer) && length(text_columns) > 0L) {
    options$columnDefs <- list(list(
      targets = as.integer(text_columns),
      render = text_renderer
    ))
  }
  options
}

#' Build a draw callback that keeps an unfiltered preview on its first page.
#' Filtering always reveals pagination for the complete result set, while the
#' explicit full-table mode reveals pagination even without an active filter.
build_datawizard_pagination_callback <- function(show_full = FALSE) {
  sprintf(
    paste0(
      "function(){var api=this.api(),filtered=api.search().length>0||",
      "api.columns().search().toArray().some(function(x){return x.length>0;});",
      "$(api.table().container()).closest('.dataTables_wrapper')",
      ".find('.dataTables_paginate').toggle(%s||filtered);}"),
    if (isTRUE(show_full)) "true" else "false"
  )
}


# ------------------------------------------------------------------------------
# 3. Get base color for a content type
# ------------------------------------------------------------------------------

#' Resolve the hex background color for a given content type.
#'
#' Falls back to pattern matching when the content type is not present in the
#' palette. Returns the palette default color for completely unknown types.
#'
#' @param content_type  Character string naming the content type.
#' @param color_palette Named character vector mapping content types to hex colors.
#' @return Single hex color string.
get_base_color <- function(content_type, color_palette) {
  if (content_type %in% names(color_palette)) {
    return(color_palette[content_type])
  }

  # Pattern-based fallbacks for unknown content types
  if (grepl("^Imputed", content_type, ignore.case = TRUE)) {
    return("#b3d9ff")
  } else if (grepl("Abundance", content_type, ignore.case = TRUE)) {
    return("#d4edda")
  } else if (grepl("p-Value|P-Value", content_type, ignore.case = TRUE)) {
    return("#ffeaa7")
  } else if (grepl("Ratio", content_type, ignore.case = TRUE)) {
    return("#fff3cd")
  } else if (grepl("Found in", content_type, ignore.case = TRUE)) {
    return("#fff9c4")
  } else {
    return(color_palette["default"])
  }
}


# ------------------------------------------------------------------------------
# 4. Decide whether to use option-based shading
# ------------------------------------------------------------------------------

#' Determine whether color shading should be applied for a content type.
#'
#' Shading is only useful when there are 2-6 unique option values and the
#' content type is in the set of shade-eligible types.
#'
#' @param content_type  Character string naming the content type.
#' @param unique_options Character vector of unique option values for this type.
#' @return Logical; TRUE if shading should be applied.
should_use_shading <- function(content_type, unique_options) {
  if (length(unique_options) < 2) return(FALSE)
  if (length(unique_options) > 6) return(FALSE)

  shade_eligible_types <- c(
    "Raw Abundance", "Normalized Abundance", "Batch Corrected Abundance",
    "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance",
    "Imputed Raw Abundance", "Imputed Normalized Abundance",
    "Imputed Batch Corrected Abundance",
    "Imputed Batch Corrected Normalized Abundance",
    "Imputed Batch Corrected Raw Abundance",
    "Found in Sample", "Found in File",
    "Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value"
  )

  is_eligible <- content_type %in% shade_eligible_types ||
    grepl("Abundance|Found in|Ratio", content_type, ignore.case = TRUE)

  return(is_eligible)
}


# ------------------------------------------------------------------------------
# 5. Generate color shades
# ------------------------------------------------------------------------------

#' Generate lighter-to-darker variants of a base color for a set of options.
#'
#' @param base_color Hex color string to vary.
#' @param options    Character vector of option names (determines number of shades).
#' @return Named character vector mapping each option to a hex color.
generate_color_shades <- function(base_color, options) {
  n_options <- length(options)
  rgb_base <- col2rgb(base_color)

  intensity_range <- seq(0.3, 1.0, length.out = n_options)
  shaded_colors <- character(n_options)
  names(shaded_colors) <- options

  for (i in seq_along(options)) {
    intensity <- intensity_range[i]
    if (intensity < 1.0) {
      white_blend <- 1 - intensity
      new_rgb <- rgb_base * intensity + 255 * white_blend
    } else {
      new_rgb <- rgb_base
    }
    new_rgb <- pmax(0, pmin(255, new_rgb))
    shaded_colors[options[i]] <- rgb(new_rgb[1], new_rgb[2], new_rgb[3],
                                     maxColorValue = 255)
  }

  return(shaded_colors)
}


# ------------------------------------------------------------------------------
# 6. Create content color mapping
# ------------------------------------------------------------------------------

#' Build a column-to-color mapping for the primary data table.
#'
#' When metadata is available, creates a precise per-column mapping that can
#' apply option-based color shading within the same content type. When no
#' metadata is provided, returns a simpler content-type-level mapping.
#'
#' @param content_types Character vector of content type names.
#' @param metadata      Data frame with Column, Content, and Options columns,
#'                      or NULL / empty data frame for the simple mapping.
#' @return Named character vector mapping column names to hex colors.
create_content_color_mapping <- function(content_types, metadata = NULL) {
  base_color_palette <- c(
    "Raw Abundance"                    = "#d4edda",
    "Normalized Abundance"             = "#c3e6cb",
    "Batch Corrected Abundance"        = "#b3d9b3",
    "Imputed Raw Abundance"            = "#b3d9ff",
    "Imputed Normalized Abundance"     = "#99ccff",
    "Imputed Batch Corrected Abundance"= "#80bfff",
    "Imputed Batch Corrected Normalized Abundance" = "#66b3ff",
    "Imputed Batch Corrected Raw Abundance" = "#4da6ff",
    "Abundance Ratio"                  = "#fff3cd",
    "Abundance Ratio p-Value"          = "#ffeaa7",
    "Abundance Ratio Adj. p-Value"     = "#fdcb6e",
    "Identifier"                       = "#f8f9fa",
    "Description"                      = "#e9ecef",
    "Additional Information"           = "#dee2e6",
    "Protein Confidence"               = "#f3e5f5",
    "# PSMs"                           = "#e1bee7",
    "Found in Sample"                  = "#fff9c4",
    "Found in File"                    = "#fff59d",
    "Row Index"                        = "#ffffff",
    "default"                          = "#f0f0f0"
  )

  # Simple mapping when no metadata is provided
  if (is.null(metadata) || nrow(metadata) == 0) {
    mapping <- character(length(content_types))
    names(mapping) <- content_types
    for (content_type in content_types) {
      mapping[content_type] <- get_base_color(content_type, base_color_palette)
    }
    return(mapping)
  }

  # Per-column mapping with optional option-based shading
  column_mapping <- character()

  for (content_type in content_types) {
    if (is.na(content_type) || !nzchar(content_type)) next

    base_color <- get_base_color(content_type, base_color_palette)
    content_rows <- which(metadata$Content == content_type)

    if (length(content_rows) == 0) next

    options_for_content <- metadata$Options[content_rows]
    options_for_content <- options_for_content[
      !is.na(options_for_content) & nzchar(options_for_content)
    ]
    unique_options <- unique(options_for_content)

    use_shading <- should_use_shading(content_type, unique_options)

    if (use_shading && length(unique_options) > 1) {
      shaded_colors <- generate_color_shades(base_color, unique_options)
      for (row_idx in content_rows) {
        col_name <- metadata$Column[row_idx]
        option_value <- metadata$Options[row_idx]
        if (!is.na(option_value) && nzchar(option_value) &&
            option_value %in% names(shaded_colors)) {
          column_mapping[col_name] <- shaded_colors[option_value]
        } else {
          column_mapping[col_name] <- base_color
        }
      }
    } else {
      for (row_idx in content_rows) {
        col_name <- metadata$Column[row_idx]
        column_mapping[col_name] <- base_color
      }
    }
  }

  return(column_mapping)
}


# ------------------------------------------------------------------------------
# 7. Build data preview
# ------------------------------------------------------------------------------

#' Prepare a data frame for DT display.
#'
#' Cleans column names and creates a bounded display snapshot. The caller keeps
#' the canonical frame separately for server-side filtering and mutations.
#'
#' @param df            Data frame to prepare.
#' @param max_cols      Integer; maximum number of columns to display.
#' @param max_rows      Numeric; maximum number of rows in the display snapshot.
#' @param truncate_cells Retained for backwards compatibility. Source values
#'                       are never truncated; use the DT display renderer.
#' @param debug_log     Optional logging function with signature (message, level).
#' @return List with elements:
#'   \describe{
#'     \item{data}{Prepared data frame ready for datatable().}
#'     \item{row_truncated}{Logical; TRUE when rows were trimmed.}
#'     \item{col_truncated}{Logical; TRUE when columns were trimmed.}
#'   }
DATAWIZARD_TABLE_PAGE_LENGTH <- 50L
DATAWIZARD_PREVIEW_MAX_ROWS <- DATAWIZARD_TABLE_PAGE_LENGTH
DATAWIZARD_PREVIEW_MAX_COLS <- 200L
DATAWIZARD_LARGE_CELL_COUNT <- 500000L
DATAWIZARD_LARGE_SERIALIZED_BYTES <- 8L * 1024L * 1024L

# A mode-specific output forces Shiny to create a new DT binding when the
# payload and processing mode change.  In particular, renderDT's `server`
# argument is fixed when the renderer is registered; it cannot safely be
# switched by merely invalidating the renderer's reactive expression.
datawizard_table_output_id <- function(role, show_full = FALSE) {
  role <- match.arg(role, c("primary", "additional"))
  paste0(role, "_table_preview_", if (isTRUE(show_full)) "full" else "bounded")
}

estimate_frame_serialized_size <- function(df, sample_rows = 100L) {
  if (!is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) return(0)
  sampled <- df[seq_len(min(nrow(df), sample_rows)), , drop = FALSE]
  as.numeric(object.size(sampled)) * nrow(df) / nrow(sampled)
}

#' Resolve a selected preview row against the complete canonical frame.
#' Row Index is the durable identity across bounded previews and restorations.
resolve_preview_row_position <- function(complete_frame, visible_slice, selected_row) {
  if (length(selected_row) < 1L) return(NA_integer_)
  selected_row <- suppressWarnings(as.integer(selected_row[[1L]]))
  if (!is.data.frame(complete_frame) || !is.data.frame(visible_slice) ||
      is.na(selected_row) || selected_row < 1L || selected_row > nrow(visible_slice)) return(NA_integer_)
  if ("Row Index" %in% names(complete_frame) && "Row Index" %in% names(visible_slice)) {
    identity <- as.character(visible_slice[["Row Index"]][selected_row])
    return(resolve_preview_row_identity_position(complete_frame, visible_slice, identity))
  }
  preview_names <- rownames(visible_slice)
  complete_names <- rownames(complete_frame)
  if (!is.null(preview_names) && !is.null(complete_names)) {
    match_pos <- match(preview_names[[selected_row]], complete_names)
    if (!is.na(match_pos)) return(match_pos)
  }
  if (selected_row <= nrow(complete_frame)) selected_row else NA_integer_
}

#' Resolve a DT-captured Row Index identity against canonical data.
#'
#' Only the current canonical frame participates in validation. In particular,
#' the selected identity need not be present in a column-bounded preview.
#' Missing and duplicate identities are unsafe to mutate and return `NA`.
resolve_canonical_row_identity_position <- function(complete_frame, selected_identity) {
  if (!is.data.frame(complete_frame) ||
      !"Row Index" %in% names(complete_frame) ||
      length(selected_identity) != 1L || is.na(selected_identity)) return(NA_integer_)

  identity <- as.character(selected_identity)
  if (!nzchar(identity)) return(NA_integer_)
  complete_matches <- which(!is.na(complete_frame[["Row Index"]]) &
    as.character(complete_frame[["Row Index"]]) == identity)
  if (length(complete_matches) != 1L) return(NA_integer_)
  complete_matches[[1L]]
}

#' Resolve a complete row selection against the current canonical frame.
#'
#' Positions are validated as a unit: an invalid position or durable identity
#' makes the whole selection unsafe.  When Row Index identities are supplied,
#' they take precedence over DT's display-relative positions.
resolve_canonical_selected_positions <- function(complete_frame, selected_positions,
                                                 selected_identities = NULL) {
  if (!is.data.frame(complete_frame)) stop("complete_frame must be a data frame")
  if (length(selected_positions) == 0L) return(integer(0))

  numeric_positions <- suppressWarnings(as.numeric(selected_positions))
  positions_valid <- length(numeric_positions) == length(selected_positions) &&
    all(!is.na(numeric_positions)) && all(is.finite(numeric_positions)) &&
    all(numeric_positions == floor(numeric_positions)) &&
    all(numeric_positions > 0) && all(numeric_positions <= nrow(complete_frame))
  if (!positions_valid) stop("Selected row positions are invalid or stale")
  positions <- sort(unique(as.integer(numeric_positions)))

  if (!is.null(selected_identities) && "Row Index" %in% names(complete_frame)) {
    if (length(selected_identities) != length(selected_positions) ||
        anyNA(selected_identities) || any(!nzchar(as.character(selected_identities)))) {
      stop("Selected Row Index identities are missing")
    }
    resolved <- vapply(selected_identities, function(identity) {
      resolve_canonical_row_identity_position(complete_frame, identity)
    }, integer(1))
    if (anyNA(resolved)) stop("Selected Row Index identities are missing or ambiguous")
    positions <- sort(unique(resolved))
  }

  positions
}

#' Resolve a DT-captured Row Index identity against canonical data.
#'
#' DT selection indexes can describe the filtered display rather than the
#' source frame.  Callers should capture the selected row's data in the browser
#' and pass its Row Index here. `visible_slice` is retained for API
#' compatibility, but is deliberately not used for identity validation.
resolve_preview_row_identity_position <- function(complete_frame, visible_slice, selected_identity) {
  resolve_canonical_row_identity_position(complete_frame, selected_identity)
}

# Pure string builder; the observer wraps the result in DT::JS.
build_datawizard_row_identity_callback <- function(input_id, table_id, row_index_column) {
  input_json <- encodeString(input_id, quote = '"')
  table_json <- encodeString(table_id, quote = '"')
  sprintf(
    paste0(
      "table.on('select.dt deselect.dt',function(e,dt,type,indexes){",
      "if(type!=='row')return;var rows=table.rows({selected:true}).data().toArray();",
      "var identities=rows.map(function(row){return row[%d]==null?null:String(row[%d]);});",
      "Shiny.setInputValue(%s,{identities:identities,table_id:%s,nonce:Date.now()},",
      "{priority:'event'});});"
    ),
    as.integer(row_index_column), as.integer(row_index_column), input_json, table_json
  )
}

build_data_preview <- function(df, max_cols = DATAWIZARD_PREVIEW_MAX_COLS,
                               max_rows = Inf,
                               truncate_cells = TRUE,
                               debug_log = NULL) {
  row_truncated <- FALSE
  col_truncated <- FALSE

  # Work on a preview copy so defensive repairs and display-only truncation can
  # never alter the canonical frame held by the caller.
  preview <- df

  # Defensive fallback only: data should already be canonicalized upstream by
  # clean_and_index(). Keep preview display robust for legacy/restored frames
  # without changing normal Data Wizard state.
  clean_names <- colnames(preview)
  needs_name_repair <- is.null(clean_names) ||
    any(is.na(clean_names) | clean_names == "") ||
    anyDuplicated(clean_names) > 0L
  if (isTRUE(needs_name_repair)) {
    if (is.null(clean_names)) {
      clean_names <- rep("", ncol(preview))
    }
    missing_names <- is.na(clean_names) | clean_names == ""
    clean_names[missing_names] <- paste0("Unnamed_", seq_len(sum(missing_names)))
    clean_names <- make.unique(clean_names, sep = "_dup_")
    colnames(preview) <- clean_names
    if (!is.null(debug_log)) {
      debug_log("Preview applied defensive column-name repair; upstream data should already be canonicalized", 1)
    }
  }

  # Row bounding applies only to the display snapshot. The canonical frame is
  # retained separately by build_table_display_snapshot() and remains the
  # source for server-side filters and data mutations.
  max_rows <- suppressWarnings(as.numeric(max_rows)[1L])
  if (is.na(max_rows) || max_rows < 0) {
    stop("max_rows must be a non-negative number")
  }
  if (is.finite(max_rows) && nrow(preview) > max_rows) {
    preview <- preview[seq_len(as.integer(max_rows)), , drop = FALSE]
    row_truncated <- TRUE
    if (!is.null(debug_log)) {
      debug_log(paste("Display truncated to", as.integer(max_rows), "rows"), 2)
    }
  }

  # Column protection is independent of row paging. It remains a deliberate
  # product limit for extremely wide tables; it must never imply a row limit.
  if (ncol(preview) > max_cols) {
    preview <- preview[, seq_len(max_cols), drop = FALSE]
    col_truncated <- TRUE
    if (!is.null(debug_log)) {
      debug_log(paste("Display truncated to", max_cols, "columns"), 2)
    }
  }

  rownames(preview) <- NULL

  # Source values remain complete and retain their classes. Long character
  # cells are shortened only by the orthogonal browser renderer.

  list(data = preview, row_truncated = row_truncated, col_truncated = col_truncated)
}


# ------------------------------------------------------------------------------
# 6b. Table display policy and slice helper
# ------------------------------------------------------------------------------

#' Build a policy-aware table display snapshot.
#'
#' @param df Data frame to describe and slice.
#' @param role Character role label (primary, secondary/additional, metadata).
#' @param revision Current debounced revision value for this role.
#' @param show_full Logical; TRUE when the user explicitly requests full-table interaction.
#' @param modified Logical; TRUE when the source data is modified.
#' @param filtered Logical; TRUE when filters are applied.
#' @param final Logical; TRUE when the dataset represents final processed state.
#' @param small_rows Maximum rows for small-data client-side rendering.
#' @param large_rows Maximum rows before very-large preview controls are used.
#' @param max_cols Maximum preview columns for large and very large datasets.
#' @param debug_log Optional logging function with signature (message, level).
#' @return List with dataset dimensions, column names, visible slice, role/revision, and status flags.
build_table_display_snapshot <- function(df, role, revision, show_full = FALSE,
                                         modified = FALSE, filtered = FALSE, final = FALSE,
                                         small_rows = 10000, large_rows = 50000,
                                         max_cols = DATAWIZARD_PREVIEW_MAX_COLS,
                                         large_cells = DATAWIZARD_LARGE_CELL_COUNT,
                                         large_serialized_bytes = DATAWIZARD_LARGE_SERIALIZED_BYTES,
                                         debug_log = NULL) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(
      n_rows = 0L, n_cols = 0L, column_names = character(0), visible_slice = data.frame(),
      complete_frame = df, estimated_cells = 0, estimated_serialized_bytes = 0,
      role = role, revision = revision, policy = "empty", server_side = TRUE,
      row_truncated = FALSE, col_truncated = FALSE, modified = isTRUE(modified),
      filtered = isTRUE(filtered), final = isTRUE(final), full_requested = isTRUE(show_full),
      slice_rows = 0L, slice_cols = 0L
    ))
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  estimated_cells <- as.double(n_rows) * as.double(n_cols)
  estimated_serialized_bytes <- estimate_frame_serialized_size(df)
  exceeds_payload_budget <- estimated_cells > large_cells ||
    estimated_serialized_bytes > large_serialized_bytes
  policy <- if (!exceeds_payload_budget && n_rows <= small_rows && n_cols <= max_cols) {
    "small"
  } else if (isTRUE(show_full)) {
    "large"
  } else {
    "very_large"
  }

  # The bounded snapshot is the initial 50-row preview. The renderer keeps the
  # canonical frame separately so server-side DT can filter every row without
  # publishing unrestricted pagination until the user opts in.
  slice_row_limit <- if (isTRUE(show_full)) n_rows else DATAWIZARD_PREVIEW_MAX_ROWS
  slice_col_limit <- if (!isTRUE(show_full) && n_cols > max_cols) max_cols else n_cols
  preview_result <- build_data_preview(
    df,
    max_rows = slice_row_limit,
    max_cols = slice_col_limit,
    # Server-side filters must see canonical cell values, including long text.
    truncate_cells = FALSE,
    debug_log = debug_log
  )

  list(
    n_rows = n_rows,
    n_cols = n_cols,
    column_names = names(df),
    complete_frame = df,
    estimated_cells = estimated_cells,
    estimated_serialized_bytes = estimated_serialized_bytes,
    visible_slice = preview_result$data,
    role = role,
    revision = revision,
    policy = policy,
    server_side = TRUE,
    row_truncated = preview_result$row_truncated,
    col_truncated = preview_result$col_truncated,
    modified = isTRUE(modified),
    filtered = isTRUE(filtered),
    final = isTRUE(final),
    full_requested = isTRUE(show_full),
    slice_rows = nrow(preview_result$data),
    slice_cols = ncol(preview_result$data)
  )
}

# ------------------------------------------------------------------------------
# 8. Metadata health check
# ------------------------------------------------------------------------------

#' Build health check messages for the editable metadata table.
#'
#' Checks for metadata configurations that can make downstream analyses
#' ambiguous, such as duplicate samples inside the same abundance-like content
#' category, mixed transformations within one category, and unmatched abundance
#' ratio / p-value annotations.
#'
#' @param metadata Metadata data frame from the Data Wizard table.
#' @return A list with status ("ok", "info", or "warning"), summary, and issues.
build_metadata_healthcheck <- function(metadata) {
  empty_result <- list(
    status = "ok",
    summary = "Metadata looks good.",
    issues = character(0)
  )

  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0) {
    return(list(
      status = "info",
      summary = "No metadata available yet.",
      issues = character(0)
    ))
  }

  required_cols <- c("Content", "Sample", "Transformation")
  missing_cols <- setdiff(required_cols, names(metadata))
  if (length(missing_cols) > 0) {
    return(list(
      status = "warning",
      summary = "Metadata health check is incomplete.",
      issues = paste(
        "Missing metadata column(s):",
        paste(missing_cols, collapse = ", "),
        "- some consistency checks were skipped."
      )
    ))
  }

  normalize_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }

  is_blank <- function(x) {
    !nzchar(normalize_text(x))
  }

  content <- normalize_text(metadata$Content)
  sample <- normalize_text(metadata$Sample)
  transformation <- normalize_text(metadata$Transformation)
  column <- if ("Column" %in% names(metadata)) normalize_text(metadata$Column) else rep("", nrow(metadata))
  transformation[is_blank(transformation)] <- "None"

  nonempty_rows <- nzchar(content)
  content <- content[nonempty_rows]
  sample <- sample[nonempty_rows]
  transformation <- transformation[nonempty_rows]
  column <- column[nonempty_rows]

  if (length(content) == 0) {
    return(list(
      status = "info",
      summary = "No metadata available yet.",
      issues = character(0)
    ))
  }

  abundance_like <- grepl("Abundance", content, ignore.case = TRUE) &
    !content %in% c(
      "Abundance Ratio",
      "Abundance Ratio p-Value",
      "Abundance Ratio Adj. p-Value"
    )
  checked_categories <- unique(c(
    content[abundance_like],
    intersect(c("Found in Sample", "Found in File"), unique(content))
  ))

  issues <- character(0)

  format_values <- function(values, max_items = 6) {
    values <- unique(values[nzchar(values)])
    if (length(values) == 0) return("<empty>")
    shown <- head(values, max_items)
    suffix <- if (length(values) > max_items) paste0(" and ", length(values) - max_items, " more") else ""
    paste0("'", paste(shown, collapse = "', '"), "'", suffix)
  }

  describe_rows <- function(row_columns, row_count, max_items = 6) {
    row_columns <- unique(row_columns[nzchar(row_columns)])
    if (length(row_columns) > 0) {
      return(paste0("column(s): ", format_values(row_columns, max_items = max_items)))
    }
    paste(row_count, "row(s)")
  }

  row_index_idx <- which(content == "Row Index")
  invalid_row_index_idx <- row_index_idx[column[row_index_idx] != "Row Index"]
  if (length(row_index_idx) > 1 || length(invalid_row_index_idx) > 0) {
    invalid_columns <- column[invalid_row_index_idx]
    invalid_detail <- if (length(invalid_columns) > 0) {
      paste0(" It is currently assigned to ", describe_rows(invalid_columns, length(invalid_row_index_idx)), ".")
    } else {
      ""
    }
    issues <- c(
      issues,
      paste0(
        "Only the column named 'Row Index' may use Content 'Row Index', and it may appear only once.",
        invalid_detail
      )
    )
  }

  if (!any(content != "Row Index") && length(issues) == 0) {
    return(list(
      status = "info",
      summary = "No metadata available yet.",
      issues = character(0)
    ))
  }

  for (category in checked_categories) {
    idx <- which(content == category)

    missing_sample_idx <- idx[!nzchar(sample[idx])]
    if (length(missing_sample_idx) > 0) {
      issues <- c(
        issues,
        paste0(
          category,
          " has ",
          describe_rows(column[missing_sample_idx], length(missing_sample_idx)),
          " without a sample name. Sample names are required for reliable downstream grouping and selection."
        )
      )
    }

    if (length(idx) < 2) next

    sample_values <- sample[idx]
    sample_values <- sample_values[nzchar(sample_values)]
    if (length(sample_values) > 0) {
      sample_counts <- table(sample_values, useNA = "no")
      duplicate_samples <- names(sample_counts)[sample_counts > 1]
      if (length(duplicate_samples) > 0) {
        issues <- c(
          issues,
          paste0(
            category,
            " contains duplicate sample name(s): ",
            format_values(duplicate_samples),
            ". Each sample should usually appear once within this metadata category."
          )
        )
      }
    }

    transform_values <- unique(transformation[idx])
    transform_values <- transform_values[nzchar(transform_values)]
    if (length(transform_values) > 1) {
      has_none <- any(tolower(transform_values) %in% c("none", "untransformed"))
      if (has_none) {
        issues <- c(
          issues,
          paste0(
            category,
            " mixes transformed and non-transformed columns (transformations: ",
            format_values(transform_values),
            "). Use one transformation state per category unless this is intentional."
          )
        )
      } else {
        issues <- c(
          issues,
          paste0(
            category,
            " uses multiple transformations (",
            format_values(transform_values),
            "). Downstream analysis may be ambiguous."
          )
        )
      }
    }
  }

  has_ratio <- any(content == "Abundance Ratio")
  has_ratio_p <- any(content == "Abundance Ratio p-Value")
  has_ratio_adj_p <- any(content == "Abundance Ratio Adj. p-Value")

  if (has_ratio) {
    missing_ratio_partners <- c(
      if (!has_ratio_p) "Abundance Ratio p-Value" else character(0),
      if (!has_ratio_adj_p) "Abundance Ratio Adj. p-Value" else character(0)
    )
    if (length(missing_ratio_partners) > 0) {
      issues <- c(
        issues,
        paste0(
          "Abundance Ratio is present, but missing: ",
          paste(missing_ratio_partners, collapse = " and "),
          ". Downstream analysis in ratio-dependent submodules may be unavailable."
        )
      )
    }
  } else {
    orphan_ratio_stats <- c(
      if (has_ratio_p) "Abundance Ratio p-Value" else character(0),
      if (has_ratio_adj_p) "Abundance Ratio Adj. p-Value" else character(0)
    )
    if (length(orphan_ratio_stats) > 0) {
      issues <- c(
        issues,
        paste0(
          paste(orphan_ratio_stats, collapse = " and "),
          " present without Abundance Ratio. Add the matching Abundance Ratio column(s) or remove/reclassify the orphan statistic columns."
        )
      )
    }
  }

  issues <- unique(issues)
  if (length(issues) == 0) {
    return(empty_result)
  }

  list(
    status = "warning",
    summary = paste(length(issues), "metadata issue(s) detected."),
    issues = issues
  )
}
