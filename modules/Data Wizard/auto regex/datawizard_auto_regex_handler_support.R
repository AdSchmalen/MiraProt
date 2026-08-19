# ============================================================================
# Sub-script: Auto Regex Shiny event and rendering adapter
# Purpose: Register session-scoped observers/renderers around injected state.
# Owns: source selection and Excel reads, UI rendering, inference invocation,
# stale-result detection, host notifications/log calls, and transactional rule
# transfer through Auto-Assign's public load_rules_directly adapter/reactives.
# Does not own: inference algorithms, state-transition implementation, package
# installation, downloads, a Session tab, app startup/shutdown, an independent
# logger, direct access to private Auto-Assign internals, or a second rule engine.
# Private interface: auto_regex_register_handlers(input, output, session, state,
# logger = state$logger); returns observer registrations invisibly.
# Transaction: freeze source -> infer/stage -> validate -> public transfer ->
# verify all three public rule reactives -> rollback their exact snapshots on
# failure -> mark transferred on success. No partial authoritative write remains.
# Signatures: a canonical effective-source descriptor owns a bounded checksum;
# the frozen normalized metadata remains separate and is compared before commit.
# Namespace: derives ns exclusively from session$ns.
# ============================================================================

# Classify a frozen metadata/data pair without consulting reactive state.  Keep
# this small and pure so the readiness message and the inference gate cannot
# drift apart as synchronized Tables metadata is replaced.
auto_regex_current_readiness <- function(metadata, data, editable_metadata = metadata) {
  if (!is.data.frame(data)) return("no_active_dataset")
  if (!is.data.frame(metadata)) return("metadata_unavailable")
  if (!("Column" %in% names(metadata))) return("missing_column")
  keys <- trimws(as.character(metadata$Column))
  if (anyDuplicated(keys[nzchar(keys)])) return("duplicate_column")
  if (!metadata_matches_dataset(metadata, data)) return("misaligned")
  if (is.data.frame(editable_metadata) &&
      !isTRUE(all.equal(editable_metadata, metadata, check.attributes = FALSE))) {
    return("pending_synchronization")
  }
  if (!is_meaningful_metadata(metadata)) return("assignments_required")
  "ready"
}

auto_regex_source_diagnostic <- function(metadata, data) {
  keys <- if (is.data.frame(metadata) && "Column" %in% names(metadata))
    trimws(as.character(metadata$Column)) else character()
  duplicates <- unique(keys[nzchar(keys) & duplicated(keys)])
  technical <- if (is.data.frame(data)) names(data)[names(data) == "Row Index"] else character()
  list(active_columns = if (is.data.frame(data)) ncol(data) else 0L,
       metadata_rows = if (is.data.frame(metadata)) nrow(metadata) else 0L,
       technical = technical, duplicates = duplicates)
}

# Return only bounded aggregate facts needed to audit condition references.
# Never include complete metadata rows in this diagnostic.
auto_regex_condition_reference_summary <- function(metadata, target = "Options",
                                                   label_limit = 12L) {
  content <- if (is.data.frame(metadata) && "Content" %in% names(metadata))
    trimws(as.character(metadata$Content)) else character()
  sample_rows <- length(content) > 0L & is_sample_bearing_content(content)
  references <- if (is.data.frame(metadata) && target %in% names(metadata))
    trimws(as.character(metadata[[target]])) else rep("", length(content))
  references[is.na(references)] <- ""
  labels <- unique(content[sample_rows & nzchar(content)])
  unavailable <- labels[vapply(labels, function(label) {
    rows <- sample_rows & content == label
    !any(nzchar(references[rows]))
  }, logical(1))]
  shown <- utils::head(labels, label_limit)
  unavailable_shown <- utils::head(unavailable, label_limit)
  list(target = target, applicable_rows = sum(sample_rows), sample_rows = sum(sample_rows),
       reference_rows = sum(sample_rows & nzchar(references)),
       labels = if (length(shown)) paste(shown, collapse = ", ") else "none",
       labels_omitted = max(0L, length(labels) - length(shown)),
       unavailable_labels = unavailable,
       unavailable_labels_display = if (length(unavailable_shown))
         paste(unavailable_shown, collapse = ", ") else "none",
       unavailable_labels_omitted = max(0L, length(unavailable) - length(unavailable_shown)),
       status = if (!sum(sample_rows)) "not_applicable" else if (length(unavailable))
         "warning" else "ready")
}

auto_regex_verify_condition_reference_transfer <- function(canonical, selected,
                                                           target = "Options") {
  expected <- auto_regex_condition_reference_summary(canonical, target)
  received <- auto_regex_condition_reference_summary(selected, target)
  if (expected$reference_rows > received$reference_rows) stop(
    "Condition references were lost while transferring the synchronized canonical source to the inference frame.",
    call. = FALSE)
  invisible(received)
}

# Set one worksheet state by submitting the complete visibility vector.  In
# particular, do not use `sheetVisibility(wb)[sheet] <- value`: replacement
# dispatch for a subset was not reliable across the supported openxlsx range.
auto_regex_set_sheet_visibility <- function(workbook, sheet, value = "hidden",
                                            visibility_api = NULL) {
  if (is.null(visibility_api)) visibility_api <- list(
    get = getExportedValue("openxlsx", "sheetVisibility"),
    set = getExportedValue("openxlsx", "sheetVisibility<-")
  )
  visibility <- visibility_api$get(workbook)
  sheet_index <- match(sheet, names(visibility))
  if (is.na(sheet_index)) stop(sprintf("Worksheet '%s' does not exist.", sheet),
                               call. = FALSE)
  visibility[[sheet_index]] <- value
  visibility_api$set(workbook, visibility)
  invisible(visibility)
}

auto_regex_write_metadata_template <- function(file, dataset, logger,
                                                openxlsx_available = NULL) {
  if (is.null(openxlsx_available)) openxlsx_available <-
    requireNamespace("openxlsx", quietly = TRUE)
  if (!isTRUE(openxlsx_available)) {
    message <- paste(
      "Metadata template download failed: required bootstrap dependency",
      "'openxlsx' is unavailable."
    )
    logger(message, 1L)
    stop(message, call. = FALSE)
  }

  content_choices <- datawizard_metadata_content_choices(include_blank = FALSE)
  template <- auto_regex_build_metadata_template(
    dataset, AUTO_REGEX_METADATA_SCHEMA,
    example_columns = auto_regex_metadata_example_columns(content_choices))
  name_issues <- if (is.data.frame(dataset) && ncol(dataset))
    auto_regex_template_name_issues(names(dataset)) else character()

  tryCatch({
    workbook <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(workbook, "Metadata")
    openxlsx::writeData(workbook, "Metadata", template, keepNA = FALSE)

    if (length(name_issues)) {
      openxlsx::addWorksheet(workbook, "Instructions")
      openxlsx::writeData(workbook, "Instructions", data.frame(
        Instructions = c(
          paste("Column names are preserved exactly. Resolve these issues",
                "before importing if needed:"),
          utils::head(name_issues, 10L)
        ), check.names = FALSE
      ))
      logger(paste("Metadata template column-name warning:",
        paste(utils::head(name_issues, 10L), collapse = " ")), 1L)
    }

    validation_error <- tryCatch({
      openxlsx::addWorksheet(workbook, "Content Choices")
      openxlsx::writeData(workbook, "Content Choices", c("", content_choices),
                          colNames = FALSE)
      openxlsx::dataValidation(
        workbook, "Metadata", cols = match("Content", names(template)),
        rows = seq_len(nrow(template)) + 1L, type = "list",
        value = sprintf("'Content Choices'!$A$1:$A$%d",
                        length(content_choices) + 1L), allowBlank = TRUE
      )
      auto_regex_set_sheet_visibility(workbook, "Content Choices", "hidden")
      NULL
    }, error = function(e) {
      # The validation may fail after creating its helper sheet. Submit a
      # complete vector again so older openxlsx releases still hide it.
      try(auto_regex_set_sheet_visibility(workbook, "Content Choices", "hidden"),
          silent = TRUE)
      conditionMessage(e)
    })
    if (!is.null(validation_error)) logger(sprintf(
      "Metadata template exported without Content validation: %s",
      validation_error), 1L)
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    invisible(list(sheets = names(openxlsx::sheetVisibility(workbook)),
                   visibility = unname(openxlsx::sheetVisibility(workbook)),
                   validation_attached = is.null(validation_error)))
  }, error = function(e) {
    logger(sprintf("Metadata template workbook writing failed: %s",
                   conditionMessage(e)), 1L)
    stop(e)
  })
}

# Freeze provenance at the same source boundary as metadata.  Workbook mode is
# deliberately self-contained: the injected provenance reader and active data
# are never evaluated, and only scalar fields persisted in the selected sheet
# are admitted as authority.
auto_regex_source_provenance <- function(mode, metadata, working_data, injected_provenance = NULL) {
  if (identical(mode, "excel")) {
    columns <- if (is.data.frame(metadata) && "Column" %in% names(metadata))
      unique(trimws(as.character(metadata$Column))) else character()
    columns <- columns[nzchar(columns)]
    workbook_shape <- as.data.frame(stats::setNames(replicate(length(columns),
      logical(), simplify = FALSE), columns), check.names = FALSE)
    return(list(source = "workbook", data = workbook_shape,
      configurations = list(), contrast_mapping_collection = NULL))
  }
  supplied <- if (is.function(injected_provenance)) injected_provenance() else injected_provenance
  if (is.null(supplied)) supplied <- list()
  if (!is.list(supplied)) stop("provenance must resolve to NULL or a list.", call. = FALSE)
  configurations <- supplied$configurations %||% supplied$provenance %||% list()
  collection <- supplied$contrast_mapping_collection %||% NULL
  source_revision <- supplied$source_revision %||% NULL
  collection_matches_source <- auto_regex_contrast_collection_matches_source(
    collection, source_revision)
  if (collection_matches_source) {
    generated <- unlist(lapply(collection$mappings, function(mapping)
      if (is.list(mapping) && is.data.frame(mapping$Columns))
        as.character(mapping$Columns$Column) else character()), use.names = FALSE)
    configurations$ratio <- list(generated_columns = unique(generated))
  }
  list(source = "active_datawizard", data = working_data,
    configurations = configurations,
    source_revision = source_revision,
    contrast_mapping_collection = if (collection_matches_source) collection else NULL)
}

auto_regex_create_handler_context <- function(input, output, session, state, logger) {
  # Only the newest initialization for a namespace may handle a run. Shiny can
  # retain observers from an earlier module instance during restore/hot reload.
  handler_token <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(1e9, 1L))
  handler_key <- paste0("auto-regex:", session$ns(""))
  if (is.null(session$userData$auto_regex_handler_tokens))
    session$userData$auto_regex_handler_tokens <- list()
  session$userData$auto_regex_handler_tokens[[handler_key]] <- handler_token
  shared <- state$shared
  ns <- session$ns
  panel_names <- c("validation", "content_rules", "content_diagnostics",
    "semantic_spans", "content_refinement_lineage",
    "condition_rules", "condition_diagnostics", "ratio_rules",
    "ratio_diagnostics", "run_diagnostics")
  panel_open <- shiny::reactiveValues(.list = stats::setNames(
    as.list(rep(FALSE, length(panel_names))), panel_names))
  # Keep the UI declaration explicit for its static bounded-render contract;
  # the pure helper exposes the same bound to runtime portability tests.
  diagnostic_row_limit <- 500L
  stopifnot(identical(diagnostic_row_limit,AUTO_REGEX_DIAGNOSTIC_ROW_LIMIT))

  injected <- function(name, default = NULL) {
    value <- if (is.list(shared)) shared[[name]] else NULL
    if (is.function(value)) tryCatch(value(), error = function(e) default) else default
  }
  current_snapshot <- function() {
    metadata <- injected("metadata")
    data <- injected("data")
    normalized <- datawizard_normalize_technical_pair(data, metadata)
    data <- normalized$data
    metadata <- datawizard_migrate_metadata_technical_keys(normalized$metadata)
    revision <- injected("revision")
    editable_metadata <- if (is.list(revision) &&
        is.data.frame(revision$editable_metadata)) revision$editable_metadata else metadata
    editable_metadata <- datawizard_migrate_metadata_technical_keys(editable_metadata)
    # The synchronization revision belongs to the inference tuple; the live
    # editable buffer is deliberately readiness-only.
    if (is.list(revision)) revision$editable_metadata <- NULL
    readiness <- auto_regex_current_readiness(metadata, data, editable_metadata)
    aligned <- readiness %in% c("assignments_required", "ready")
    meaningful <- identical(readiness, "ready")
    list(metadata = metadata, editable_metadata = editable_metadata,
         data = data, revision = revision,
         readiness = readiness, aligned = aligned, meaningful = meaningful)
  }
  source_snapshot <- function(current = NULL) {
    mode <- isolate(input$source %||% "current_metadata")
    if (identical(mode, "excel")) {
      frame <- isolate(state$worksheet_data())
      mapping <- isolate(state$mapping())
      keep <- if (is.null(mapping) || !is.data.frame(frame)) logical() else
        nzchar(mapping) & mapping %in% names(frame)
      value <- if (is.data.frame(frame) && !is.null(mapping)) frame[, mapping[keep], drop = FALSE] else NULL
      if (is.data.frame(value)) names(value) <- names(mapping)[keep]
      persisted <- intersect(provenance_fields, names(frame))
      if (is.data.frame(value) && length(persisted))
        value[persisted] <- frame[persisted]
      value <- datawizard_migrate_metadata_technical_keys(value)
      return(auto_regex_source_descriptor(mode, value, frame,
        isolate(state$workbook()), isolate(state$worksheet()), mapping,
        list(workbook = isolate(state$workbook())$revision %||% NULL)))
    } else {

      if (is.null(current)) {
        current <-
          current_snapshot()
      }

      value <-
        current$metadata
    }

    # Current-metadata identity is defined by the canonical metadata used for
    # inference and the ordered active column names. Data values, row counts,
    # and synchronization bookkeeping do not change a header-regex problem.
    descriptor <-
      auto_regex_source_descriptor(
        mode,
        value,
        current$data,
        revisions = NULL
      )
    # Retain the actual active snapshot in the frozen current-mode tuple while
    # keeping only its bounded checksum in source identity.
    if (!identical(mode, "excel")) descriptor$data <- current$data
    descriptor
  }

  mapping_fields <- c("Column", "Content", "Options", "Transformation",
                      "Numerator", "Denominator")
  provenance_fields <- DATAWIZARD_PROVENANCE_FIELDS
  content_redundancy_input_id <- auto_regex_content_redundancy_input_id
  normalize_redundancy_overrides <- auto_regex_normalize_redundancy_overrides
  collect_content_redundancy_overrides <- function(rules)
    auto_regex_collect_content_redundancy_overrides(
      rules, input, state, content_redundancy_input_id)
  structure(list(
    input = input, output = output, session = session, state = state, logger = logger,
    handler_token = handler_token, handler_key = handler_key, shared = shared, ns = ns,
    panel_names = panel_names, panel_open = panel_open,
    diagnostic_row_limit = diagnostic_row_limit, injected = injected,
    current_snapshot = current_snapshot, source_snapshot = source_snapshot,
    mapping_fields = mapping_fields, provenance_fields = provenance_fields,
    content_redundancy_input_id = content_redundancy_input_id,
    normalize_redundancy_overrides = normalize_redundancy_overrides,
    collect_content_redundancy_overrides = collect_content_redundancy_overrides
  ), class = "auto_regex_handler_context")
}
