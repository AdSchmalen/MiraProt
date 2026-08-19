# ============================================================================
# Sub-script: Auto Regex session-local state machine
# Purpose: Allocate reactive values and implement source/run/transfer transitions.
# Owns: reset/invalidate/begin/complete/fail/stale/transfer/cleanup transitions,
# candidate-versus-authoritative markers, run identity, and diagnostic retention.
# Does not own: UI, observers/renderers, inference, source I/O, installation,
# downloads, notifications, logging storage, app lifecycle, or Auto-Assign writes.
# Private interface: auto_regex_create_state(shared, logger) returns the state and
# its transition closures; auto_regex_* transition functions support that object.
# Transaction boundary: results remain candidates until handler-side transfer is
# verified; complete_transfer records success but never writes Auto-Assign state.
# Fingerprints: source_fingerprint is a monotonic invalidation generation; each
# run captures it and completion is accepted only while run id/status/generation
# all remain current. It is distinct from the handler's serialized source hash.
# ============================================================================

auto_regex_state_read <- function(x) {
  shiny::isolate(x())
}

auto_regex_create_state <- function(rv = NULL,
                                    logger = function(...) invisible(NULL)) {
  state <- list(
    shared = rv,
    logger = logger,

    # Source/workbook/worksheet
    source = shiny::reactiveVal(NULL),
    workbook = shiny::reactiveVal(NULL),
    worksheets = shiny::reactiveVal(character()),
    worksheet = shiny::reactiveVal(NULL),
    worksheet_data = shiny::reactiveVal(NULL),

    # Mapping and validation
    mapping = shiny::reactiveVal(NULL),
    validation = shiny::reactiveVal(NULL),
    source_descriptor = shiny::reactiveVal(NULL),
    processing = shiny::reactiveVal(FALSE),
    current_processing_stage = shiny::reactiveVal(NULL),

    # Run identity and lifecycle
    next_run_id = shiny::reactiveVal(0L),
    active_run_id = shiny::reactiveVal(NULL),
    completed_run_id = shiny::reactiveVal(NULL),
    run_status = shiny::reactiveVal("idle"),
    run_source_fingerprint = shiny::reactiveVal(NULL),
    run_context = shiny::reactiveVal(NULL),

    # Results are staged as candidates until a successful current run commits.
    candidate_rules = shiny::reactiveVal(NULL),
    candidate_payload = shiny::reactiveVal(NULL),

    # Expensive full-inference cache. It contains the pre-compaction Content table,
    # final condition/ratio rules, semantic spans, frozen metadata and source hash.
    redundancy_base = shiny::reactiveVal(NULL),

    # Global redundancy is a UI preference rather than source evidence.
    # Keep it across modal destruction/recreation and source changes.
    global_redundancy = shiny::reactiveVal(0L),

    # Named integer vector:
    #   names = canonical Content RuleId
    #   value = per-rule redundancy
    # Missing RuleIds inherit the global input.
    redundancy_overrides = shiny::reactiveVal(integer()),

    payload = shiny::reactiveVal(NULL),
    transfer = shiny::reactiveVal(NULL),

    diagnostics = shiny::reactiveVal(NULL),
    warnings = shiny::reactiveVal(character()),
    errors = shiny::reactiveVal(character()),
    timings = shiny::reactiveVal(NULL),
    refinement_counts = shiny::reactiveVal(NULL),

    # This is an invalidation generation, rather than a content hash.  It must
    # never be reset during the lifetime of a state object.
    source_fingerprint = shiny::reactiveVal(0L),
    stale = shiny::reactiveVal(FALSE),
    initialized = shiny::reactiveVal(TRUE)
  )

  # Expose transitions on the factory result as well as by their named
  # functions.  Handler code can therefore mutate state only through this API.
  state$reset_source_specific_state <- function(source = NULL) {
    auto_regex_reset_source_specific_state(state, source)
  }
  state$reset_workbook_state <- function(workbook = NULL, worksheets = character()) {
    auto_regex_reset_workbook_state(state, workbook, worksheets)
  }
  state$reset_worksheet_state <- function(worksheet = NULL, data = NULL) {
    auto_regex_reset_worksheet_state(state, worksheet, data)
  }
  state$begin_run <- function(...) auto_regex_begin_run(state, ...)
  state$complete_run <- function(...) auto_regex_complete_run(state, ...)
  state$fail_run <- function(...) auto_regex_fail_run(state, ...)
  state$refresh_candidate <- function(...) {
    auto_regex_refresh_candidate(state, ...)
  }
  state$mark_stale <- function() auto_regex_mark_stale(state)
  state$complete_transfer <- function(...) auto_regex_complete_transfer(state, ...)
  state$cleanup <- function() auto_regex_cleanup(state)
  state
}

auto_regex_clear_current_results <- function(
    state,
    preserve_transfer = FALSE,
    preserve_redundancy_base = FALSE,
    preserve_redundancy_overrides = FALSE) {
  state$candidate_rules(NULL)
  state$candidate_payload(NULL)

  if (!isTRUE(
    preserve_redundancy_base
  )) {
    state$redundancy_base(NULL)
  }

  if (!isTRUE(
    preserve_redundancy_overrides
  )) {
    state$redundancy_overrides(
      integer()
    )
  }

  if (!isTRUE(preserve_transfer)) {
    state$payload(NULL)
    state$transfer(NULL)
  }
  state$diagnostics(NULL)
  state$warnings(character())
  state$errors(character())
  state$timings(NULL)
  state$refinement_counts(NULL)
  state$completed_run_id(NULL)
  invisible(state)
}

auto_regex_invalidate <- function(state, status = "stale") {
  fingerprint <- auto_regex_state_read(state$source_fingerprint) + 1L
  state$source_fingerprint(fingerprint)
  state$active_run_id(NULL)
  state$run_source_fingerprint(NULL)
  state$run_context(NULL)
  state$run_status(status)
  state$stale(identical(status, "stale"))
  auto_regex_clear_current_results(state)
  invisible(fingerprint)
}

auto_regex_invalidate_effective_source <- function(state) {
  had_completed_result <- !is.null(auto_regex_state_read(state$completed_run_id)) ||
    !is.null(auto_regex_state_read(state$candidate_rules)) ||
    !is.null(auto_regex_state_read(state$transfer)) ||
    identical(auto_regex_state_read(state$run_status), "stale")
  auto_regex_invalidate(state, if (had_completed_result) "stale" else
    if (!is.null(auto_regex_state_read(state$validation))) "ready" else "idle")
}

auto_regex_reset_source_specific_state <- function(state, source = NULL) {
  auto_regex_invalidate_effective_source(state)
  state$source(source)
  state$workbook(NULL)
  state$worksheets(character())
  state$worksheet(NULL)
  state$worksheet_data(NULL)
  state$mapping(NULL)
  state$validation(NULL)
  state$source_descriptor(NULL)
  invisible(state)
}

auto_regex_reset_workbook_state <- function(state, workbook = NULL,
                                             worksheets = character()) {
  auto_regex_invalidate_effective_source(state)
  state$workbook(workbook)
  state$worksheets(worksheets)
  state$worksheet(NULL)
  state$worksheet_data(NULL)
  state$mapping(NULL)
  state$validation(NULL)
  state$source_descriptor(NULL)
  invisible(state)
}

auto_regex_reset_worksheet_state <- function(state, worksheet = NULL, data = NULL) {
  auto_regex_invalidate_effective_source(state)
  state$worksheet(worksheet)
  state$worksheet_data(data)
  state$mapping(NULL)
  state$validation(NULL)
  state$source_descriptor(NULL)
  invisible(state)
}

auto_regex_begin_run <- function(state, source_mode = NULL,
                                 source_value_fingerprint = NULL) {
  run_id <- auto_regex_state_read(state$next_run_id) + 1L
  state$next_run_id(run_id)
  # Starting another candidate must not erase the marker/snapshot for rules
  # already transferred to Auto-Assign.  A failed or superseded run therefore
  # cannot make the authoritative rules appear to have vanished.
  auto_regex_clear_current_results(
    state,
    preserve_transfer = TRUE,
    preserve_redundancy_base = TRUE,
    preserve_redundancy_overrides = TRUE
  )
  state$active_run_id(run_id)
  source_fingerprint <- auto_regex_state_read(state$source_fingerprint)
  state$run_source_fingerprint(source_fingerprint)
  state$run_context(list(
    run_id = run_id,
    source_mode = as.character(source_mode %||% "unknown")[[1L]],
    source_fingerprint = as.character(source_value_fingerprint %||%
      source_fingerprint)[[1L]],
    started_at = Sys.time()
  ))
  state$run_status("running")
  state$stale(FALSE)
  list(run_id = run_id, source_fingerprint = source_fingerprint)
}

auto_regex_run_is_current <- function(state, run_id, source_fingerprint) {
  identical(auto_regex_state_read(state$run_status), "running") &&
    identical(auto_regex_state_read(state$active_run_id), run_id) &&
    identical(auto_regex_state_read(state$run_source_fingerprint), source_fingerprint) &&
    identical(auto_regex_state_read(state$source_fingerprint), source_fingerprint)
}

auto_regex_complete_run <- function(
    state,
    run_id,
    source_fingerprint,
    candidate_rules,
    diagnostics = NULL,
    warnings = character(),
    timings = NULL,
    payload = NULL,
    validation = NULL,
    redundancy_base = NULL) {
  if (!auto_regex_run_is_current(state, run_id, source_fingerprint)) {
    return(invisible(FALSE))
  }
  state$candidate_rules(candidate_rules)
  state$candidate_payload(payload)
  state$redundancy_base(
    redundancy_base
  )
  context <- auto_regex_state_read(state$run_context)
  completed_at <- Sys.time()
  state$diagnostics(c(context, list(
    completed_at = completed_at,
    elapsed_ms = as.numeric(difftime(completed_at, context$started_at,
      units = "secs")) * 1000,
    timings = timings, tables = diagnostics
  )))
  state$warnings(warnings)
  state$errors(character())
  state$timings(timings)
  counts <- if (is.list(diagnostics)) diagnostics$refinement_counts else NULL
  state$refinement_counts(counts)
  state$validation(validation)
  state$completed_run_id(run_id)
  state$active_run_id(NULL)
  state$run_status("complete")
  state$stale(FALSE)
  invisible(TRUE)
}

auto_regex_refresh_candidate <- function(
    state,
    candidate_rules,
    payload = candidate_rules,
    redundancy_lineage = NULL,
    rebuild_ms = NULL) {

  if (isTRUE(auto_regex_state_read(state$stale)) ||
      is.null(auto_regex_state_read(state$completed_run_id)) ||
      is.null(auto_regex_state_read(state$run_context))) {
    return(invisible(FALSE))
  }

  if (!is.list(candidate_rules) ||
      !is.list(payload)) {
    return(invisible(FALSE))
  }

  state$candidate_rules(candidate_rules)
  state$candidate_payload(payload)

  diagnostic_record <-
    auto_regex_state_read(
      state$diagnostics
    )

  if (is.list(diagnostic_record) &&
      is.data.frame(redundancy_lineage)) {

    if (!is.list(diagnostic_record$tables)) {
      diagnostic_record$tables <- list()
    }

    diagnostic_record$tables$content_redundancy <-
      redundancy_lineage
  }

  timings <-
    auto_regex_state_read(
      state$timings
    )

  if (!is.null(rebuild_ms)) {

    if (is.null(timings)) {
      timings <- numeric()
    }

    timings[["redundancy_rebuild"]] <-
      as.numeric(rebuild_ms)

    state$timings(timings)

    if (is.list(diagnostic_record)) {
      diagnostic_record$timings <- timings
    }
  }

  if (is.list(diagnostic_record)) {
    state$diagnostics(
      diagnostic_record
    )
  }

  transferred_payload <-
    auto_regex_state_read(
      state$payload
    )

  # If the rebuilt candidate is byte-for-byte the same as the last payload
  # transferred to Auto-Assign, it is still authoritative. Otherwise it is a
  # valid untransferred candidate.
  if (!is.null(transferred_payload) &&
      identical(payload, transferred_payload)) {

    state$run_status("transferred")

  } else {

    state$run_status("complete")
  }

  state$stale(FALSE)

  invisible(TRUE)
}

auto_regex_fail_run <- function(state, run_id, source_fingerprint,
                                errors, diagnostics = NULL,
                                warnings = character(), timings = NULL) {
  if (!auto_regex_run_is_current(state, run_id, source_fingerprint)) {
    return(invisible(FALSE))
  }
  auto_regex_clear_current_results(
    state,
    preserve_transfer = TRUE,
    preserve_redundancy_base = TRUE,
    preserve_redundancy_overrides = TRUE
  )
  state$errors(as.character(errors))
  context <- auto_regex_state_read(state$run_context)
  completed_at <- Sys.time()
  state$diagnostics(c(context, list(
    completed_at = completed_at,
    elapsed_ms = as.numeric(difftime(completed_at, context$started_at,
      units = "secs")) * 1000,
    timings = timings, tables = diagnostics
  )))
  state$warnings(warnings)
  state$timings(timings)
  counts <- if (is.list(diagnostics)) diagnostics$refinement_counts else NULL
  state$refinement_counts(counts)
  state$active_run_id(NULL)
  state$run_status("failed")
  invisible(TRUE)
}

auto_regex_mark_stale <- function(state) {
  auto_regex_invalidate(state, "stale")
  invisible(state)
}

auto_regex_complete_transfer <- function(state, run_id, source_fingerprint,
                                         payload = auto_regex_state_read(
                                           state$candidate_payload)) {
  current <-
    auto_regex_state_read(state$run_status) %in%
    c("complete", "transferred") &&
    identical(
      auto_regex_state_read(state$completed_run_id),
      run_id
    ) &&
    identical(
      auto_regex_state_read(state$source_fingerprint),
      source_fingerprint
    ) &&
    identical(
      auto_regex_state_read(state$run_source_fingerprint),
      source_fingerprint
    )
  if (!current || is.null(auto_regex_state_read(state$candidate_rules))) {
    return(invisible(FALSE))
  }
  state$payload(payload)
  state$transfer(list(run_id = run_id, source_fingerprint = source_fingerprint,
                      completed_at = Sys.time()))
  state$run_status("transferred")
  invisible(TRUE)
}

auto_regex_cleanup <- function(state) {
  if (!isTRUE(auto_regex_state_read(state$initialized))) return(invisible(state))
  auto_regex_invalidate(state, "cleaned")
  state$source(NULL)
  state$workbook(NULL)
  state$worksheets(character())
  state$worksheet(NULL)
  state$worksheet_data(NULL)
  state$mapping(NULL)
  state$validation(NULL)
  state$source_descriptor(NULL)
  state$processing(FALSE)
  state$current_processing_stage(NULL)
  state$initialized(FALSE)
  invisible(state)
}
