# ==============================================================================
# GO Module - Observers, Reactive Triggers, and Outputs
# ==============================================================================
#
# Purpose:
#   Contains all observers, observeEvent() handlers, reactive expressions,
#   output$ render functions, and download handlers for the GO enrichment module.
#   Also contains helper functions that depend on Shiny reactive context
#   (add_error_to_log, module_health_check, update_download_dimensions,
#   initialize_download_dimensions, inspect_organism_cache).
#
# Architecture role:
#   Sourced with local = TRUE inside modGOServer() in GO_module.R, after both
#   GO_module_state.R and the logic/hub files. Relies on all reactive state
#   created by GO_module_state.R and all logic functions from GO_module_logic.R
#   and GO_module_hub.R being available in the server closure.
#
# Structure:
#   1. Helper functions (add_error_to_log, module_health_check,
#      update_download_dimensions, initialize_download_dimensions,
#      inspect_organism_cache)
#   2. Data-readiness observers (go_ready, rv$data_mod/data_def change)
#   3. Column selection observers (pValType_GO, pValCol_GO, AbundanceCol_GO)
#   4. Analysis execution observer (createGO_button)
#   5. Cache / organism management observers (refresh_cache, update_organisms,
#      OrgDb_GO key type loading)
#   6. Plot creation observer (create_go_plot)
#   7. Tree rendering and term selection (goTree output, selected_go_terms)
#   8. Plot rendering outputs (GOplot_container, GOplot_custom, GOplot_1)
#   9. Download dimension tracking observers and outputs
#  10. Grid integration observer (add_to_grid)
#  11. Download handlers (downloadPlotButton_GO, download_res_GO)
#  12. RDS import observer and status output (go_import_rds)
#  13. Module lifecycle (onStop, initialization observer)
#
# Future developers:
#   - All new observers and outputs belong here.
#   - Never define reactive state here; add it to GO_module_state.R.
#   - Never add pure logic here; add it to GO_module_logic.R.
#   - Functions that need session or reactive context belong in this file.
#   - Use debug_log (available from server closure) for all logging.
# ==============================================================================

# Strict keytype cache TTL: keytypes older than this are not loaded directly.
# Organism cache TTL: a recent OrgDb cache can still use static keytype defaults
# to avoid startup/species-change downloads after a strict keytype cache miss.
KEYTYPE_CACHE_MAX_AGE_DAYS <- 10
ORGANISM_CACHE_MAX_AGE_DAYS <- 30

# ==============================================================================
# 1. Helper Functions
# ==============================================================================

add_error_to_log <- function(error_message) {
  current_errors <- go_errors()
  new_error <- list(
    timestamp = Sys.time(),
    message = error_message
  )

  updated_errors <- c(list(new_error), current_errors)
  if (length(updated_errors) > 10) {
    updated_errors <- updated_errors[1:10]
  }

  go_errors(updated_errors)
  last_error_time(Sys.time())
}

module_health_check <- function() {
  list(
    status = go_analysis_status(),
    data_ready = go_ready(),
    results_available = !is.null(GO_Result_List()),
    selected_terms_count = length(selected_go_terms()),
    error_count = length(go_errors()),
    last_error = last_error_time(),
    debug_level_value = DEBUG_LEVEL,
    plot_object_available = !is.null(current_plot_object()),
    current_height = current_plot_height(),
    current_width = current_plot_width()
  )
}

valid_go_results_available <- function() {
  tryCatch({
    go_results <- GO_Result_List()

    !is.null(go_results) &&
      !is.null(go_results$Edo_GO) &&
      !is.null(go_results$Edo_GO@result) &&
      nrow(go_results$Edo_GO@result) > 0
  }, error = function(e) {
    debug_log(paste("Error checking GO results availability:", e$message), 2)
    FALSE
  })
}

last_keytype_select_snapshots <- reactiveVal(list())
pending_programmatic_width <- reactiveVal(NULL)
pending_programmatic_height <- reactiveVal(NULL)
pending_programmatic_pval_type <- reactiveVal(NULL)
pending_programmatic_pval_col <- reactiveVal(NULL)
go_plot_output_rendered <- reactiveVal(FALSE)
last_go_choice_signature <- reactiveVal(NULL)
last_go_metadata_choice_signature <- reactiveVal(NULL)
last_go_pvalue_choice_cache <- reactiveVal(NULL)
go_initial_selection_applied <- reactiveVal(FALSE)

is_go_restore_guard_active <- function() {
  isTRUE(tryCatch(isolate(rv$session_restoring), error = function(e) FALSE)) ||
    isTRUE(tryCatch(isolate(go_restore_guard_active()), error = function(e) FALSE))
}

go_metadata_has_meaningful_assignments <- function(data_def) {
  if (is.null(data_def) || !is.data.frame(data_def) || !("Content" %in% names(data_def))) {
    return(FALSE)
  }

  readiness <- compute_go_readiness(data_def)
  isTRUE(readiness$has_abundance) && isTRUE(readiness$has_pvalue)
}

go_metadata_is_row_index_only <- function(data_def) {
  if (is.null(data_def) || !is.data.frame(data_def) || !("Content" %in% names(data_def))) {
    return(FALSE)
  }

  content <- trimws(as.character(data_def$Content))
  content <- content[!is.na(content) & nzchar(content)]
  normalized <- tolower(content)
  length(normalized) > 0L && all(normalized %in% c("row-index", "row index"))
}

clear_go_restore_guard_if_finalized <- function(data_def, reason = "metadata") {
  if (!isTRUE(tryCatch(isolate(go_restore_guard_active()), error = function(e) FALSE))) {
    return(invisible(FALSE))
  }

  restore_trigger <- tryCatch(isolate(rv$session_restore_trigger), error = function(e) NULL)
  trigger_baseline <- tryCatch(isolate(go_restore_trigger_baseline()), error = function(e) NULL)
  trigger_fired <- !is.null(restore_trigger) && !identical(restore_trigger, trigger_baseline)

  if (isTRUE(trigger_fired) && go_metadata_has_meaningful_assignments(data_def)) {
    go_restore_guard_active(FALSE)
    go_restore_row_index_skip_logged(FALSE)
    go_restore_trigger_baseline(NULL)
    debug_log(sprintf("GO restore guard cleared after %s metadata became ready", reason), 2)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

get_go_pval_type_choices <- function(data_def) {
  pval_patterns <- c("^Abundance Ratio p-Value$", "^Abundance Ratio Adj\\. p-Value$")
  pval_type_choices <- character(0)

  for (pattern in pval_patterns) {
    matches <- grep(pattern, data_def$Content, ignore.case = TRUE)
    if (length(matches) > 0) {
      content_values <- unique(data_def$Content[matches])
      names(content_values) <- content_values
      pval_type_choices <- c(pval_type_choices, content_values)
    }
  }

  pval_type_choices
}


compact_go_pvalue_metadata_signature <- function(data_def) {
  if (is.null(data_def) || !is.data.frame(data_def)) {
    return(NULL)
  }

  signature_columns <- intersect(c("Column", "Content", "Options", "Option"), names(data_def))
  if (length(signature_columns) == 0) {
    return(NULL)
  }

  list(
    rows = nrow(data_def),
    columns = data_def[, signature_columns, drop = FALSE]
  )
}

compact_go_metadata_choice_signature <- function(data_def) {
  if (is.null(data_def) || !is.data.frame(data_def) || !("Content" %in% names(data_def))) {
    return(NULL)
  }

  content <- as.character(data_def$Content)
  identifier_rows <- which(grepl("Identifier", content, ignore.case = TRUE))
  identifier_columns <- intersect(c("Column", "Content", "Options", "Option"), names(data_def))
  identifier_signature <- if (length(identifier_rows) > 0 && length(identifier_columns) > 0) {
    data_def[identifier_rows, identifier_columns, drop = FALSE]
  } else {
    data.frame()
  }

  revision_fields <- c(
    "data_def_revision_id",
    "metadata_revision",
    "metadata_content_signature",
    "datawizard_metadata_revision",
    "datawizard_metadata_content_signature"
  )
  revisions <- lapply(revision_fields, function(field) {
    value <- tryCatch(rv[[field]], error = function(e) NULL)
    if (is.function(value)) {
      value <- tryCatch(value(), error = function(e) NULL)
    }
    value
  })
  names(revisions) <- revision_fields
  revisions <- Filter(Negate(is.null), revisions)

  list(
    rows = nrow(data_def),
    content = content,
    identifier_rows = identifier_signature,
    identifier_option_choices = rv$datawizard_identifier_option_choices %||% character(0),
    revisions = revisions
  )
}

refresh_go_metadata_choices <- function(reason = "metadata") {
  isolate({
    if (datawizard_metadata_defer_downstream_choices(rv)) {
      debug_log(sprintf("Metadata assignment pending; deferring GO choices from %s", reason), 2)
      return(invisible(FALSE))
    }

    data_def <- rv$data_def
    if (!go_metadata_has_meaningful_assignments(data_def)) {
      debug_log(sprintf("GO metadata lacks abundance ratio and p-value assignments from %s; skipping UI choice update", reason), 2)
      return(invisible(FALSE))
    }

    signature <- compact_go_metadata_choice_signature(data_def)

    if (is.null(signature)) {
      debug_log(sprintf("GO metadata not ready from %s; skipping UI choice update", reason), 2)
      return(invisible(FALSE))
    }

    if (identical(last_go_metadata_choice_signature(), signature)) {
      debug_log(sprintf(
        "GO metadata signature unchanged after %s; skipping choice refresh",
        reason
      ), 3)
      return(invisible(FALSE))
    }

    updated <- update_go_choices_from_metadata(data_def, source = reason)
    last_go_metadata_choice_signature(signature)

    updated
  })
}

update_go_choices_from_metadata <- function(data_def, source = "metadata") {
  if (datawizard_metadata_defer_downstream_choices(rv)) {
    debug_log(sprintf("Metadata assignment pending; deferring GO choices from %s", source), 2)
    return(invisible(FALSE))
  }
  if (is.null(data_def) || !is.data.frame(data_def) || !("Content" %in% names(data_def))) {
    debug_log(sprintf("GO metadata not ready from %s; skipping UI choice update", source), 2)
    return(invisible(FALSE))
  }
  if (!go_metadata_has_meaningful_assignments(data_def)) {
    debug_log(sprintf("GO metadata lacks abundance ratio and p-value assignments from %s; skipping UI choice update", source), 2)
    return(invisible(FALSE))
  }

  abundance_choices <- get_column_choices_by_content(
    data_def,
    "Abundance Ratio",
    debug_log = debug_log,
    exact_match = TRUE
  )
  pval_type_choices <- get_go_pval_type_choices(data_def)

  selected_pval_type <- isolate(input$pValType_GO)
  pval_type_values <- unname(pval_type_choices)
  if (is.null(selected_pval_type) || identical(selected_pval_type, "") ||
      !(selected_pval_type %in% pval_type_values)) {
    selected_pval_type <- if (length(pval_type_values) > 0) pval_type_values[[1]] else ""
  }

  pval_choices <- if (!is.null(selected_pval_type) && nzchar(selected_pval_type)) {
    get_column_choices_by_content(data_def, selected_pval_type, debug_log = debug_log)
  } else {
    character(0)
  }

  central_gene_choices <- rv$datawizard_identifier_option_choices %||% character(0)
  gene_choices <- if (length(central_gene_choices) > 0) {
    central_gene_choices
  } else {
    get_gene_identifier_choices(data_def, debug_log = debug_log)
  }

  choice_signature <- list(
    abundance_choices = abundance_choices,
    pval_type_choices = pval_type_choices,
    pval_choices = pval_choices,
    gene_choices = gene_choices
  )

  debug_log(sprintf(
    "GO metadata ready: abundance_choices=%d, pvalue_choices=%d, identifiers=%d",
    length(abundance_choices),
    length(pval_choices),
    length(gene_choices)
  ), 2)

  pending_ui_restore <- tryCatch(rv$go_pending_ui_restore, error = function(e) NULL)
  if (!is.list(pending_ui_restore)) pending_ui_restore <- list()
  if (identical(last_go_choice_signature(), choice_signature) && length(pending_ui_restore) == 0L) {
    debug_log(sprintf(
      "GO choices unchanged after %s update; skipping selectize choice updates",
      source
    ), 3)
    return(invisible(FALSE))
  }

  choose_restored_or_current <- function(id, current_value, choices) {
    pending_value <- pending_ui_restore[[id]]
    if (!is.null(pending_value)) {
      pending_value <- as.character(pending_value)
      pending_value <- pending_value[!is.na(pending_value) & pending_value %in% choices]
      if (length(pending_value) > 0L) return(pending_value[[1]])
    }
    current_value <- as.character(current_value %||% character(0))
    current_value <- current_value[!is.na(current_value) & current_value %in% choices]
    if (length(current_value) > 0L) return(current_value[[1]])
    NULL
  }

  updateSelectizeInput(session, "AbundanceCol_GO",
                       choices = abundance_choices,
                       selected = choose_restored_or_current("AbundanceCol_GO", input$AbundanceCol_GO, abundance_choices))
  debug_log(paste("Abundance choices updated:", length(abundance_choices), "options"), 3)

  updateSelectizeInput(session, "pValType_GO",
                       choices = pval_type_choices,
                       selected = choose_restored_or_current("pValType_GO", input$pValType_GO, pval_type_choices))
  debug_log(paste("P-value type choices updated:", length(pval_type_choices), "options"), 3)

  updateSelectizeInput(session, "pValCol_GO",
                       choices = pval_choices,
                       selected = choose_restored_or_current("pValCol_GO", input$pValCol_GO, pval_choices))
  debug_log(paste("P-value column choices updated:", length(pval_choices), "options"), 3)

  updateSelectizeInput(session, "GeneIdentifierColumn_GO",
                       choices = gene_choices,
                       selected = choose_restored_or_current("GeneIdentifierColumn_GO", input$GeneIdentifierColumn_GO, gene_choices))
  debug_log(paste("Gene identifier choices updated:", length(gene_choices), "options"), 3)

  if (length(pending_ui_restore) > 0L) {
    tryCatch({ rv$go_pending_ui_restore <- NULL }, error = function(e) NULL)
  }
  last_go_choice_signature(choice_signature)

  invisible(TRUE)
}

is_pending_programmatic_dimension <- function(value, pending_value) {
  if (is.null(pending_value) || is.null(value)) {
    return(FALSE)
  }

  identical(value, pending_value) ||
    (is.numeric(value) && is.numeric(pending_value) && isTRUE(all.equal(value, pending_value)))
}

is_pending_programmatic_pval_selection <- function(value, pending_value) {
  if (is.null(pending_value) || is.null(value)) {
    return(FALSE)
  }

  identical(value, pending_value)
}

is_go_pvalue_selection_valid <- function(selection, choices) {
  if (is.null(selection) || identical(selection, "")) {
    return(FALSE)
  }

  selection %in% choices || selection %in% names(choices)
}

same_choices_and_selection <- function(input_id, choices, selected) {
  snapshots <- last_keytype_select_snapshots()
  snapshot <- snapshots[[input_id]]
  if (is.null(snapshot)) {
    return(FALSE)
  }

  current_selected <- isolate(input[[input_id]])
  identical(snapshot$choices, choices) &&
    identical(current_selected %||% character(0), selected %||% character(0))
}

update_keytype_select_input <- function(input_id, choices, selected) {
  if (same_choices_and_selection(input_id, choices, selected)) {
    debug_log(sprintf(
      "KeyType: Skipping %s update; choices and selected value are unchanged",
      input_id
    ), 2)
    return(invisible(FALSE))
  }

  updateSelectInput(session, input_id, choices = choices, selected = selected)

  snapshots <- last_keytype_select_snapshots()
  snapshots[[input_id]] <- list(choices = choices, selected = selected)
  last_keytype_select_snapshots(snapshots)

  invisible(TRUE)
}

is_go_data_ready <- function() {
  tryCatch({
    isTRUE(go_ready())
  }, error = function(e) {
    FALSE
  })
}

is_go_tab_visible <- function() {
  tryCatch({
    root_input <- session$rootScope()$input
    identical(isolate(root_input$analysis_tabs), "go")
  }, error = function(e) {
    FALSE
  })
}

restored_session_requires_dimension_inputs <- function() {
  tryCatch({
    !is.null(isolate(rv$session_restore_trigger)) &&
      (!is.null(isolate(current_plot_object())) || valid_go_results_available())
  }, error = function(e) {
    FALSE
  })
}

should_initialize_download_dimensions <- function() {
  valid_go_results_available() ||
    isTRUE(go_plot_output_rendered()) ||
    is_go_tab_visible() ||
    restored_session_requires_dimension_inputs()
}

set_default_download_dimensions_silently <- function() {
  pending_programmatic_width(10)
  pending_programmatic_height(8)

  updateNumericInput(session, "plotWidthInch_GO",  value = 10)
  updateNumericInput(session, "plotHeightInch_GO", value = 8)
}

#' Update Download Panel Dimensions
#'
#' Update the download panel input fields with current plot dimensions
update_download_dimensions <- function() {
  tryCatch({
    current_height_px <- current_plot_height()
    current_width_px  <- current_plot_width()

    debug_log(paste("DOWNLOAD: Current dimensions - Height:", current_height_px, "px, Width:", current_width_px, "px"), 2)

    dimensions <- calculate_current_plot_dimensions(current_height_px, current_width_px)

    debug_log(paste("DOWNLOAD: Calculated dimensions - Height:", dimensions$height, "in, Width:", dimensions$width, "in"), 2)

    current_time <- Sys.time()
    if (is.null(last_manual_input_time()) ||
        difftime(current_time, last_manual_input_time(), units = "secs") > 2) {

      pending_programmatic_width(dimensions$width)
      pending_programmatic_height(dimensions$height)

      updateNumericInput(session, "plotWidthInch_GO",  value = dimensions$width)
      updateNumericInput(session, "plotHeightInch_GO", value = dimensions$height)

      debug_log("DOWNLOAD: Updated dimension inputs with calculated values", 2)
    } else {
      debug_log("DOWNLOAD: Skipped update due to recent manual input", 2)
    }

  }, error = function(e) {
    debug_log(paste("DOWNLOAD: Error updating dimensions:", e$message), 1)
  })
}

initialize_download_dimensions <- function() {
  if (!should_initialize_download_dimensions()) {
    set_default_download_dimensions_silently()
    return(invisible(FALSE))
  }

  if (!is.null(current_plot_height()) && !is.null(current_plot_width())) {
    update_download_dimensions()
  } else {
    set_default_download_dimensions_silently()
  }

  debug_log("DOWNLOAD: Download dimensions initialized", 2)
  invisible(TRUE)
}

inspect_organism_cache <- function() {
  cache_contents <- reactiveValuesToList(organism_cache)

  debug_log("=== ORGANISM CACHE INSPECTION ===", 1)
  debug_log(paste("Total cached organisms:", length(cache_contents)), 1)

  if (length(cache_contents) > 0) {
    for (org_name in names(cache_contents)) {
      key_types <- cache_contents[[org_name]]
      debug_log(paste("  ", org_name, ":", length(key_types), "key types"), 1)
      debug_log(paste("    Types:", paste(head(key_types, 5), collapse = ", ")), 2)
    }
  } else {
    debug_log("  Cache is empty", 1)
  }
  debug_log("=== END CACHE INSPECTION ===", 1)

  return(cache_contents)
}

# ==============================================================================
# 2. File Validation Observer
# ==============================================================================

observe({
  req(input$go_import_file)

  file_info <- input$go_import_file
  if (!is.null(file_info)) {
    tryCatch({
      sheet_names <- openxlsx::getSheetNames(file_info$datapath)
      has_go_sheet <- any(grepl("GO_Analysis|GO\\.Analysis", sheet_names, ignore.case = TRUE))

      if (has_go_sheet) {
        import_status_message("✓ Valid GO Excel file detected")
        debug_log("Valid GO Excel file uploaded", 2)
      } else {
        import_status_message("✗ No GO Analysis sheet found in file")
        debug_log("Invalid GO Excel file - no GO Analysis sheet", 1)
      }
    }, error = function(e) {
      import_status_message("✗ Cannot read Excel file")
      debug_log(paste("Error reading uploaded file:", e$message), 1)
    })
  } else {
    import_status_message("")
  }
})

# ==============================================================================
# 2. Data-Readiness Observers
# ==============================================================================

# Update UI choices when data becomes ready (go_ready trigger)
observeEvent(go_ready(), {
  req(go_ready())
  if (datawizard_restore_phase_active(rv)) {
    debug_log("Restore phase active; deferring GO metadata choice refresh from go_ready", 2)
    return()
  }

  debug_log("Updating UI choices", 2)

  tryCatch({
    updated <- refresh_go_metadata_choices("go_ready")

    if (isTRUE(updated)) {
      user_manual_pval_selection(FALSE)
      last_auto_paired_pval(NULL)
      last_go_pvalue_choice_cache(NULL)
    }
  }, error = function(e) {
    message <- conditionMessage(e)
    if (!nzchar(message)) {
      message <- "unknown error"
    }
    debug_log(paste("Error updating UI choices:", message), 1)
    add_error_to_log(paste("UI update error:", message))
  })
})

# Update UI choices when underlying data changes
observeEvent(datawizard_import_ready_signature(rv), {

  if (datawizard_import_barrier_active(rv)) {
    debug_log("Import barrier active; preserving GO choices until ready", 2)
    return()
  }

  if (datawizard_restore_phase_active(rv)) {
    debug_log("Restore phase active; preserving restored GO results and UI on data change", 2)
    return()
  }

  debug_log("Data change detected, updating UI choices", 3)

  restore_guard <- is_go_restore_guard_active()
  metadata_meaningful <- go_metadata_has_meaningful_assignments(rv$data_def)

  clear_go_restore_guard_if_finalized(rv$data_def, "data_change")
  restore_guard <- is_go_restore_guard_active()

  if (isTRUE(restore_guard) && !metadata_meaningful) {
    if (go_metadata_is_row_index_only(rv$data_def)) {
      if (!isTRUE(go_restore_row_index_skip_logged())) {
        debug_log("GO restore guard: metadata is Row-Index-only during restore; skipping readiness downgrade until restore finalization", 2)
        go_restore_row_index_skip_logged(TRUE)
      }
    } else {
      debug_log("GO restore guard: metadata is not meaningful yet; skipping readiness downgrade until restore finalization", 2)
    }
    return()
  }

  readiness <- compute_go_readiness(rv$data_def)
  debug_log(sprintf(
    "GO readiness: ready=%s, abundance=%s (%d), p-value=%s (%d)",
    readiness$ready,
    readiness$has_abundance,
    readiness$abundance_count,
    readiness$has_pvalue,
    readiness$pvalue_count
  ), 3)

  data_ready <- tryCatch({
    check_go_data_readiness(rv, debug_log = debug_log)
  }, error = function(e) {
    debug_log(paste("Error checking data readiness:", e$message), 1)
    return(FALSE)
  })

  if (!data_ready) {
    if (isTRUE(restore_guard)) {
      debug_log("GO restore guard: data not ready during restore; preserving restored GO results and UI", 2)
      return()
    }
    debug_log("Data not ready for GO analysis", 2)
    return()
  }

  debug_log("Data ready, updating UI choices", 2)

  tryCatch({
    updated <- refresh_go_metadata_choices("data_change")

    if (isTRUE(updated)) {
      user_manual_pval_selection(FALSE)
      last_auto_paired_pval(NULL)
      last_go_pvalue_choice_cache(NULL)
    }
  }, error = function(e) {
    message <- conditionMessage(e)
    if (!nzchar(message)) {
      message <- "unknown error"
    }
    debug_log(paste("Error updating UI choices:", message), 1)
    add_error_to_log(paste("UI choice update error:", message))
  })
}, ignoreInit = TRUE)

observeEvent(rv$session_restore_trigger, {
  clear_go_restore_guard_if_finalized(rv$data_def, "session_restore_trigger")

  if (isTRUE(is_go_restore_guard_active())) {
    debug_log("GO restore guard remains active after session_restore_trigger; waiting for meaningful metadata", 2)
  }
}, ignoreInit = TRUE)

# ==============================================================================
# 3. Column Selection Observers
# ==============================================================================

observeEvent(input$pValType_GO, {

  if (!is_go_data_ready()) {
    debug_log("Ignoring p-value type change until GO data is ready", 2)
    return()
  }

  pending_pval_type <- pending_programmatic_pval_type()
  if (is_pending_programmatic_pval_selection(input$pValType_GO, pending_pval_type)) {
    pending_programmatic_pval_type(NULL)
    debug_log(paste("Ignoring programmatic p-value type change to:", input$pValType_GO), 2)
    return(invisible(NULL))
  }

  if (is.null(input$pValType_GO) || input$pValType_GO == "") {
    debug_log("P-value type cleared", 2)
    return()
  }

  debug_log(paste("P-value type changed to:", input$pValType_GO), 2)

  tryCatch({
    if (is.null(rv$data_def)) {
      debug_log("data_def not available", 2)
      return()
    }

    pvalue_metadata_signature <- compact_go_pvalue_metadata_signature(rv$data_def)
    pvalue_choice_cache <- last_go_pvalue_choice_cache()
    selected_pval_col <- isolate(input$pValCol_GO)

    if (!is.null(pvalue_choice_cache) &&
        identical(pvalue_choice_cache$pval_type, input$pValType_GO) &&
        identical(pvalue_choice_cache$metadata_signature, pvalue_metadata_signature) &&
        is_go_pvalue_selection_valid(selected_pval_col, pvalue_choice_cache$pval_choices)) {
      debug_log(sprintf(
        "GO p-value choices unchanged for %s; keeping selected column %s",
        input$pValType_GO,
        selected_pval_col
      ), 3)
      return(invisible(NULL))
    }

    pval_choices <- get_column_choices_by_content(
      rv$data_def,
      input$pValType_GO,
      debug_log = debug_log
    )

    user_manual_pval_selection(FALSE)

    choice_signature <- last_go_choice_signature()
    if (!is.null(choice_signature) && identical(choice_signature$pval_choices, pval_choices)) {
      debug_log("P-value column choices unchanged; skipping update", 2)
    } else {
      updateSelectizeInput(session, "pValCol_GO", choices = pval_choices)
      debug_log(paste("P-value column choices updated:", length(pval_choices), "options"), 3)

      if (is.null(choice_signature)) {
        choice_signature <- list()
      }
      choice_signature$pval_choices <- pval_choices
      last_go_choice_signature(choice_signature)
    }

  }, error = function(e) {
    debug_log(paste("Error updating p-value columns:", e$message), 1)
    add_error_to_log(paste("P-value column update error:", e$message))
  })
}, ignoreInit = TRUE)

# Track manual p-value column selections
observeEvent(input$pValCol_GO, {
  if (!is_go_data_ready()) {
    debug_log("Ignoring p-value column change until GO data is ready", 2)
    return()
  }

  current_selection <- input$pValCol_GO
  pending_pval_col <- pending_programmatic_pval_col()
  if (is_pending_programmatic_pval_selection(current_selection, pending_pval_col)) {
    pending_programmatic_pval_col(NULL)
    debug_log(paste("Ignoring programmatic p-value column change to:", current_selection), 2)
    return(invisible(NULL))
  }

  if (is.null(current_selection) || identical(current_selection, "")) {
    debug_log("Ignoring empty p-value column selection", 2)
    return()
  }

  last_auto <- last_auto_paired_pval()
  if (is.null(last_auto)) {
    debug_log("Ignoring p-value column change before automatic pairing has completed", 2)
    return()
  }

  if (!identical(current_selection, last_auto)) {
    debug_log("User manually changed p-value column selection", 2)
    user_manual_pval_selection(TRUE)
  }
}, ignoreInit = TRUE)

# Automatic pairing when abundance ratio column changes
observeEvent(input$AbundanceCol_GO, {

  if (!is_go_data_ready()) {
    debug_log("Ignoring abundance column change until GO data is ready", 2)
    return()
  }

  debug_log("Abundance ratio column selection changed", 2)

  if (is.null(input$AbundanceCol_GO) || input$AbundanceCol_GO == "") {
    debug_log("No abundance column selected, skipping automatic pairing", 2)
    return()
  }

  if (is.null(input$pValType_GO) || input$pValType_GO == "") {
    debug_log("No p-value type selected, skipping automatic pairing", 2)
    return()
  }

  debug_log(paste("Processing abundance column:", input$AbundanceCol_GO), 2)
  debug_log(paste("P-value type:", input$pValType_GO), 2)

  prerequisites_met <- tryCatch({
    check_pairing_prerequisites(rv, debug_log = debug_log)
  }, error = function(e) {
    debug_log(paste("Error checking prerequisites:", e$message), 1)
    add_error_to_log(paste("Prerequisites check error:", e$message))
    return(FALSE)
  })

  if (!prerequisites_met) {
    debug_log("Prerequisites not met for automatic pairing", 2)
    return()
  }

  user_made_manual_selection <- tryCatch({
    user_manual_pval_selection()
  }, error = function(e) {
    debug_log("Error checking manual selection status, assuming FALSE", 2)
    return(FALSE)
  })

  if (isTRUE(user_made_manual_selection)) {
    debug_log("Skipping automatic pairing - user has made manual selection", 2)
    return()
  }

  tryCatch({
    debug_log("Attempting automatic p-value column pairing", 2)

    if (is.null(rv$data_def)) {
      debug_log("data_def not available for pairing", 1)
      return()
    }

    best_pval_col <- tryCatch({
      find_best_pvalue_partner(
        selected_ratio_col  = input$AbundanceCol_GO,
        pval_type_selection = input$pValType_GO,
        data_def            = rv$data_def,
        debug_log           = debug_log
      )
    }, error = function(e) {
      debug_log(paste("Error finding p-value partner:", e$message), 1)
      return(NULL)
    })

    if (!is.null(best_pval_col) && nzchar(best_pval_col)) {
      debug_log(paste("Automatically pairing with p-value column:", best_pval_col), 1)

      current_choices <- tryCatch({
        get_column_choices_by_content(
          rv$data_def,
          input$pValType_GO,
          debug_log = debug_log
        )
      }, error = function(e) {
        debug_log(paste("Error getting current p-value choices:", e$message), 1)
        return(character(0))
      })

      if (best_pval_col %in% current_choices || best_pval_col %in% names(current_choices)) {
        tryCatch({
          last_auto_paired_pval(best_pval_col)
          last_go_pvalue_choice_cache(list(
            pval_type = input$pValType_GO,
            metadata_signature = compact_go_pvalue_metadata_signature(rv$data_def),
            pval_choices = current_choices
          ))
        }, error = function(e) {
          debug_log("Error storing automatic pairing info", 2)
        })

        tryCatch({
          if (identical(isolate(input$pValCol_GO), best_pval_col)) {
            debug_log("P-value column selection already matches automatic pairing", 2)
          } else {
            pending_programmatic_pval_col(best_pval_col)
            updateSelectizeInput(session, "pValCol_GO", selected = best_pval_col)
            debug_log("P-value column selection updated programmatically", 2)
          }
        }, error = function(e) {
          pending_programmatic_pval_col(NULL)
          debug_log(paste("Error updating p-value column selection:", e$message), 1)
          add_error_to_log(paste("UI update error:", e$message))
        })
      } else {
        debug_log(paste("Best match", best_pval_col, "not available in current choices"), 1)
      }

    } else {
      debug_log("No suitable p-value partner found for automatic pairing", 2)
    }

  }, error = function(e) {
    debug_log(paste("Critical error in automatic pairing:", e$message), 1)
    add_error_to_log(paste("Automatic pairing critical error:", e$message))

    showNotification(
      "Automatic column pairing encountered an error. Please select columns manually.",
      type = "warning",
      duration = 3
    )
  })
}, ignoreInit = TRUE)
