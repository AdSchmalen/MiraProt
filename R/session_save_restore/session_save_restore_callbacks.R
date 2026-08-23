# Shared imperative callback runner for session restore work only.

.compact_restore_call_summary <- function(calls = sys.calls(), limit = 8L) {
  if (!length(calls)) return(NULL)
  calls <- tail(calls, max(1L, as.integer(limit)[1L]))
  text <- vapply(calls, function(call) {
    value <- paste(deparse(call, width.cutoff = 80L), collapse = " ")
    trimws(gsub("[[:space:]]+", " ", value))
  }, character(1))
  paste(text, collapse = " <- ")
}

.is_shiny_context_error <- function(condition) {
  any(grepl(c("Operation not allowed without an active reactive context",
              "Can't access reactive value .* outside of reactive consumer",
              "current reactive context"),
            conditionMessage(condition), ignore.case = TRUE))
}

# Evaluate a readiness predicate without confusing "not ready" with an error.
# Imperative restore code must use this boundary rather than `tryCatch(...,
# error = FALSE)`: a reactive-context violation is a programming error and will
# never become ready merely by polling again.
.evaluate_restore_readiness <- function(owner, predicate, job_metadata = NULL) {
  if (!is.function(predicate)) stop("Restore readiness predicate must be a function", call. = FALSE)
  condition <- NULL
  value <- tryCatch(predicate(), error = function(e) {
    condition <<- e
    FALSE
  })
  if (is.null(condition)) {
    return(list(ready = isTRUE(value), retry = !isTRUE(value), code = NULL,
                condition = NULL))
  }

  code <- if (.is_shiny_context_error(condition)) {
    "REACTIVE_CONTEXT_VIOLATION"
  } else {
    "READINESS_CONDITION"
  }
  metadata <- if (is.list(job_metadata)) job_metadata else list()
  resolver <- metadata$resolve_job %||% metadata$resolve_restore_job %||% NULL
  job_id <- metadata$job_id %||% metadata$id %||% NULL
  outcome <- metadata$condition_outcome %||%
    if (identical(code, "REACTIVE_CONTEXT_VIOLATION")) "failure" else "degraded"
  if (!is.null(job_id) && is.function(resolver)) {
    tryCatch(resolver(job_id, outcome, paste0(code, ": ", conditionMessage(condition))),
             error = function(e) FALSE)
  }
  debug_log(paste0("[RestoreReadiness:error] owner=", owner, " code=", code,
                   " error=", conditionMessage(condition)), 1L)
  list(ready = FALSE,
       # Caught conditions are not absence. In particular, polling cannot fix a
       # missing Shiny consumer, so terminate/degrade the named job now.
       retry = FALSE, code = code, condition = condition)
}

# Internal capability for imperative Category 1 session-restore replay only.
.run_session_restore_callback <- function(owner, reason, generation, phase,
                                          callback, job = NULL,
                                          job_metadata = job) {
  if (!is.function(callback)) stop("Restore callback must be a function", call. = FALSE)
  metadata <- if (is.list(job_metadata)) job_metadata else list(job_id = job_metadata)
  current_generation_fn <- metadata$current_generation %||% metadata$generation_fn %||% NULL
  resolver <- metadata$resolve_job %||% metadata$resolve_restore_job %||% NULL
  resolver <- resolver %||% metadata$settle %||% NULL
  condition_recorder <- metadata$record_condition %||% NULL
  job_id <- metadata$job_id %||% metadata$id %||% NULL
  include_calls <- isTRUE(metadata$include_calls) || isTRUE(metadata$capture_calls)
  owner_field <- metadata$diagnostic_owner_field %||% "owner"

  log_event <- function(event, detail = NULL, level = 2L) {
    context <- if (isTRUE(metadata$legacy_diagnostics)) "" else
      paste0(" generation=", generation, " phase=", phase)
    debug_log(paste0("[RestoreCallback:", event, "] ", owner_field, "=", owner,
                     " reason=", reason, context, detail %||% ""), level = level)
  }
  resolve <- function(outcome, error = NULL) {
    if (is.null(job_id) || !is.function(resolver)) return(TRUE)
    ok <- tryCatch(isTRUE(resolver(job_id, outcome, error)), error = function(e) {
      log_event("error", paste0(" code=SETTLEMENT_FAILURE error=", conditionMessage(e)), 1L)
      FALSE
    })
    if (!ok) log_event("error", " code=SETTLEMENT_REJECTED error=job resolution rejected", 1L)
    ok
  }

  current_generation <- if (is.function(current_generation_fn)) {
    tryCatch(current_generation_fn(), error = function(e) NA_integer_)
  } else metadata$current_generation %||% generation
  if (!identical(as.integer(current_generation)[1L], as.integer(generation)[1L])) {
    log_event("error", " code=STALE_GENERATION error=callback skipped", 1L)
    resolve("skipped", "STALE_GENERATION")
    return(invisible(FALSE))
  }

  deadline <- metadata$deadline %||% NULL
  timed_out <- isTRUE(metadata$timed_out) ||
    (!is.null(deadline) && isTRUE(Sys.time() >= as.POSIXct(deadline)))
  if (timed_out) {
    log_event("error", " code=TIMEOUT error=callback deadline elapsed", 1L)
    resolve("timeout", "TIMEOUT")
    return(invisible(FALSE))
  }

  log_event("start")
  error_record <- NULL
  ok <- tryCatch({
    shiny::isolate(callback())
    TRUE
  }, error = function(e) {
    error_record <<- list(message = conditionMessage(e), classes = class(e),
      calls = if (include_calls) .compact_restore_call_summary(sys.calls()) else NULL,
      code = if (.is_shiny_context_error(e)) "REACTIVE_CONTEXT_VIOLATION" else "CALLBACK_ERROR")
    FALSE
  })
  if (ok) {
    settled <- resolve("success")
    if (settled) log_event("done")
    return(invisible(settled))
  }
  if (is.function(condition_recorder)) {
    recorded <- tryCatch({ condition_recorder(error_record); TRUE }, error = function(e) {
      log_event("error", paste0(" code=CONDITION_RECORD_FAILURE error=",
                                conditionMessage(e)), 1L)
      FALSE
    })
    if (!recorded) error_record$message <- paste0(error_record$message,
                                                   "; condition record failed")
  }
  detail <- paste0(" code=", error_record$code, " error=", error_record$message,
                   " classes=", paste(error_record$classes, collapse = ","))
  if (!is.null(error_record$calls)) detail <- paste0(detail, " calls=", error_record$calls)
  log_event("error", detail, 1L)
  resolve("failure", error_record$message)
  invisible(FALSE)
}
