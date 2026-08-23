# ============================================================================
# Sub-script: R/session_save_restore/session_save_restore_core_helpers.R
# Purpose:
#   Define schema/version constants, registry primitives, snapshot envelope
#   validation/unwrapping, plot bundle converters, and serialization safety
#   helpers used by session save/restore orchestration and module registration.
#
# Architectural Role:
#   shared primitives and serialization utilities
#
# Responsibilities:
#   - Provide the session registry factory and snapshot validation helpers.
#   - Provide session envelope builders/unwrappers and complex object bundlers.
#   - Provide defensive safe-call and sanitization helpers used across save/restore.
#
# Non-Responsibilities (Must NOT be here):
#   - Wire Shiny download/upload handlers (orchestration only).
#   - Register module-specific participant closures.
# ============================================================================

# Restore work is deliberately separate from the snapshot participant registry.
# A registry instance belongs to one Shiny session and is created by
# setup_session_save_restore(); job ids carry their generation so a late callback
# can never accidentally complete work belonging to a subsequent import.
.create_restore_job_registry <- function(generation_fn, schedule_timeout,
                                         settled_callback = function(report) NULL) {
  state <- new.env(parent = emptyenv())
  state$jobs <- new.env(parent = emptyenv())
  state$generations <- new.env(parent = emptyenv())
  state$sequence <- 0L

  generation_record <- function(generation, create = FALSE) {
    key <- as.character(as.integer(generation))
    if (!exists(key, state$generations, inherits = FALSE) && create) {
      assign(key, list(generation = as.integer(generation), phase = "HYDRATED",
                       sealed = FALSE, reported = FALSE), state$generations)
    }
    get0(key, state$generations, inherits = FALSE, ifnotfound = NULL)
  }
  put_generation <- function(record) {
    assign(as.character(record$generation), record, state$generations)
  }
  jobs_for <- function(generation) {
    ids <- ls(state$jobs, all.names = TRUE)
    jobs <- lapply(ids, function(id) get(id, state$jobs, inherits = FALSE))
    Filter(function(job) identical(job$generation, as.integer(generation)), jobs)
  }
  report_if_ready <- function(generation) {
    record <- generation_record(generation)
    if (is.null(record) || !isTRUE(record$sealed) || isTRUE(record$reported)) return(FALSE)
    jobs <- jobs_for(generation)
    pending <- Filter(function(job) is.null(job$resolved_at), jobs)
    if (length(pending)) return(FALSE)
    outcomes <- vapply(jobs, function(job) job$outcome %||% "completed", character(1))
    errors <- Filter(Negate(is.null), lapply(jobs, `[[`, "error"))
    failed <- outcomes %in% c("error", "failed")
    degraded <- outcomes %in% c("timeout", "skipped", "degraded")
    record$phase <- if (any(failed)) "FAILED" else if (any(degraded) || length(errors)) "DEGRADED" else "SETTLED"
    record$reported <- TRUE
    put_generation(record)
    settled_callback(list(
      generation = record$generation, state = record$phase, jobs = jobs,
      errors = errors, skipped = sum(outcomes == "skipped"),
      timeouts = sum(outcomes == "timeout"), outcomes = outcomes,
      outstanding = jobs[outcomes %in% c("timeout", "error", "failed", "skipped", "degraded")]
    ))
    TRUE
  }
  start_generation <- function(generation) {
    generation_record(generation, create = TRUE)
    invisible(as.integer(generation))
  }
  set_phase <- function(generation, phase) {
    stopifnot(phase %in% c("HYDRATED", "REPLAYING"))
    record <- generation_record(generation, create = TRUE)
    record$phase <- phase
    put_generation(record)
    invisible(phase)
  }
  register_restore_job <- function(owner, reason, phase, timeout = 15) {
    generation <- as.integer(generation_fn())
    if (length(generation) != 1L || is.na(generation)) stop("No active restore generation")
    record <- generation_record(generation)
    if (is.null(record) || isTRUE(record$sealed)) stop("Restore transaction is not accepting jobs")
    state$sequence <- state$sequence + 1L
    id <- sprintf("restore-%d-%d", generation, state$sequence)
    job <- list(id = id, generation = generation, owner = as.character(owner),
                reason = as.character(reason), phase = as.character(phase),
                timeout = as.numeric(timeout), registered_at = Sys.time(),
                resolved_at = NULL, outcome = NULL, error = NULL)
    assign(id, job, state$jobs)
    if (is.finite(job$timeout) && job$timeout > 0) {
      schedule_timeout(function() {
        # resolve_restore_job performs both the generation and exactly-once checks.
        resolve_restore_job(id, "timeout", sprintf("timed out after %ss", job$timeout))
      }, job$timeout)
    }
    id
  }
  resolve_restore_job <- function(job_id, outcome, error = NULL) {
    job <- get0(job_id, state$jobs, inherits = FALSE, ifnotfound = NULL)
    if (is.null(job) || !identical(as.integer(generation_fn()), job$generation) ||
        !is.null(job$resolved_at)) return(FALSE)
    job$resolved_at <- Sys.time()
    job$outcome <- as.character(outcome)[1L]
    job$error <- if (is.null(error)) NULL else as.character(error)[1L]
    assign(job_id, job, state$jobs)
    report_if_ready(job$generation)
    TRUE
  }
  outstanding_restore_jobs <- function(generation = generation_fn()) {
    Filter(function(job) is.null(job$resolved_at), jobs_for(as.integer(generation)))
  }
  seal_generation <- function(generation) {
    record <- generation_record(generation, create = TRUE)
    record$sealed <- TRUE
    put_generation(record)
    report_if_ready(generation)
    invisible(record$phase)
  }
  list(start_generation = start_generation, set_phase = set_phase,
       register_restore_job = register_restore_job,
       resolve_restore_job = resolve_restore_job,
       outstanding_restore_jobs = outstanding_restore_jobs,
       seal_generation = seal_generation)
}

# R/session_save_restore.R
# ========================================
# Session Save/Restore: Download and Upload Session State
# ========================================
# Sets up the session save (download) and restore (upload) functionality.
# Called from the server function in app.R.
# Depends on: DEBUG_LEVEL, debug_log() from R/bootstrap.R

# Schema version for the session snapshot format.
# Increment when the snapshot structure changes in a backward-incompatible way.
#
# v3.0.0 supports an in-list `payload_inline` transport. Historical binary
# payloads using the obsolete qs transport are intentionally unsupported.
#
# v4.0.0 introduces the "qs2 envelope" / inline transport:
#   - New saves put the payload in `payload_qs2`, produced by
#     qs2::qs_serialize(), or use `payload_inline` when qs2 is unavailable or
#     serialization fails.
#   - v4 payload_qs2 data is read with qs2 checksum validation.
MIRAPROT_SESSION_SCHEMA_VERSION <- "4.0.0"

# Known compatible schema versions that can be restored by this code.
MIRAPROT_SESSION_COMPATIBLE_VERSIONS <- c("1.0.0", "2.0.0", "3.0.0", "4.0.0")

# MiraProt application version stored in the snapshot for informational purposes.
MIRAPROT_APP_VERSION <- "2.0"

# Discriminator written when a download fails after its response has started.
# Keep this separate from `miraprot_session`: error stubs are identifiable as
# MiraProt files, but must never enter normal session schema validation.
MIRAPROT_SESSION_ERROR_MARKER <- "miraprot_session_save_error"

.sanitize_session_error_message <- function(error) {
  message <- if (is.character(error) && length(error) > 0L) error[[1L]] else ""
  message <- enc2utf8(message)
  message <- gsub("[[:cntrl:]]+", " ", message)
  message <- trimws(gsub("[[:space:]]+", " ", message))
  if (!nzchar(message)) message <- "Unknown session-save error."
  substr(message, 1L, 2000L)
}

# Save level constants (control which tiers are included in the snapshot)
#
# Session save/restore contract by level:
#   Data & Metadata (SESSION_SAVE_LEVEL_DATA / "data_only")
#     - Save Data Wizard canonical data (`data_mod` and raw/imported data).
#     - Save Data Wizard metadata (`data_def` and meaningful processed metadata).
#     - Save the complete Data Wizard module and submodule UI/settings so the
#       Data Wizard workflow can be resumed exactly where the user left it.
#     - Do not save non-Data-Wizard module UI, plots, or visualization state.
#     - Do not build plot-data cache pools; no non-Data-Wizard plot snapshots
#       should be present at this level.
#   Data & Analysis (SESSION_SAVE_LEVEL_ANALYSIS / "data_and_analysis")
#     - Save canonical data/metadata plus registered analysis result objects.
#     - Keep Data Wizard state lean and avoid the full Data Wizard UI snapshot
#       unless a specific analysis restore path explicitly requires it.
#   Full Session (SESSION_SAVE_LEVEL_FULL / "full_session")
#     - Save all supported module UI, plot bundles, and module state needed to
#       restore the complete application session.
SESSION_SAVE_LEVEL_DATA       <- "data_only"
SESSION_SAVE_LEVEL_ANALYSIS   <- "data_and_analysis"
SESSION_SAVE_LEVEL_FULL       <- "full_session"

# Module snapshot cache dependency declarations. New saves must explicitly
# declare how each module_state expects data needed for restore-time plot
# rebuilding. Missing declarations are treated as legacy-only compatibility
# signals by restore/upgrade code and must not cause new saves to create a
# shared plot-data cache pool.
SESSION_RESTORE_CACHE_DEPENDENCIES <- c(
  "shared_plot_data_cache_pool",
  "live_rv",
  "module_matrix_payload",
  "none"
)

.normalize_restore_cache_dependency <- function(dependency, default = "none") {
  dep <- as.character(dependency %||% default)[1]
  if (!is.character(dep) || length(dep) != 1L || is.na(dep) || !nzchar(dep) ||
      !dep %in% SESSION_RESTORE_CACHE_DEPENDENCIES) {
    dep <- default
  }
  dep
}

.set_module_restore_cache_dependency <- function(module_state, dependency) {
  if (!is.list(module_state)) module_state <- list()
  module_state$restore_cache_dependency <-
    .normalize_restore_cache_dependency(dependency, default = "none")
  module_state
}

.module_uses_shared_plot_data_cache <- function(module_state = NULL, legacy_full_session = FALSE) {
  if (!is.list(module_state)) return(FALSE)
  dep <- module_state$restore_cache_dependency
  if (!is.character(dep) || length(dep) != 1L || is.na(dep) || !nzchar(dep)) {
    return(isTRUE(legacy_full_session))
  }
  identical(.normalize_restore_cache_dependency(dep, default = "none"),
            "shared_plot_data_cache_pool")
}


.session_nonempty_cache_ref <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.character(x)) return(any(!is.na(x) & nzchar(x)))
  if (is.list(x)) return(length(x) > 0L)
  FALSE
}

.session_module_snapshot_needs_plot_pool <- function(module_id, module_state) {
  if (identical(module_id, "datawizard") || !is.list(module_state)) return(FALSE)

  # An explicit shared-pool dependency is a restore contract: the module has
  # declared that a pending restore needs plot data from the pool.
  if (isTRUE(.module_uses_shared_plot_data_cache(module_state, legacy_full_session = FALSE))) {
    return(TRUE)
  }
  # Unlike title-level entries, the scalar reference is not itself evidence
  # that a plot is live.  It is commonly left behind when a module is cleared.
  if (isTRUE(.session_nonempty_cache_ref(module_state$plot_cache_ref_by_title))) {
    return(TRUE)
  }

  plot_present_fields <- c(
    "had_plot", "has_plot", "plot_present", "had_rendered_plot",
    "has_rendered_plot", "plot_available", "has_saved_plot", "had_saved_plot",
    "had_static_plots", "plot_ready", "plots_ready", "had_heatmap",
    "plot_reconstruction_pending", "pending_plot_reconstruction",
    "plot_rebuild_pending", "rebuild_requested"
  )
  for (field in intersect(plot_present_fields, names(module_state))) {
    if (isTRUE(module_state[[field]])) return(TRUE)
  }

  FALSE
}

.restore_cache_unavailable_dataset_mismatch_warning <- paste(
  "Cached plot data was unavailable; plot was not regenerated from the current dataset",
  "because it differs from the saved plot dataset."
)

.module_restore_has_cache_intent <- function(module_state) {
  if (!is.list(module_state)) return(FALSE)
  isTRUE(.session_nonempty_cache_ref(module_state$plot_data_cache_ref)) ||
    isTRUE(.session_nonempty_cache_ref(module_state$plot_cache_ref_by_title)) ||
    !is.null(module_state$plot_data_cache_payload)
}

.module_restore_contract_with_by_title_ref <- function(module_state) {
  contract <- .module_cache_ref_contract(module_state)
  if (!is.character(contract$plot_data_cache_ref) ||
      length(contract$plot_data_cache_ref) != 1L ||
      !nzchar(contract$plot_data_cache_ref)) {
    by_title <- module_state$plot_cache_ref_by_title
    if (is.list(by_title) && length(by_title) > 0L) {
      ref <- .session_scalar_chr(unlist(by_title, use.names = FALSE)[1])
      if (nzchar(ref)) contract$plot_data_cache_ref <- ref
    }
  }
  contract
}

.module_restore_live_contract_compatible <- function(module_state, rv = NULL) {
  if (!is.list(module_state) || is.null(rv)) return(FALSE)
  live_data_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
  live_data_def <- tryCatch(rv$data_def, error = function(e) NULL)
  .cache_ref_contract_compatible(
    .module_restore_contract_with_by_title_ref(module_state),
    live_data_mod,
    live_data_def
  )
}

.session_snapshots_need_plot_pool <- function(module_snapshots = NULL) {
  if (!is.list(module_snapshots) || length(module_snapshots) == 0L) return(FALSE)
  any(vapply(names(module_snapshots), function(mid) {
    .session_module_snapshot_needs_plot_pool(
      mid,
      tryCatch(module_snapshots[[mid]]$module_state, error = function(e) NULL)
    )
  }, logical(1)))
}

# ========================================
# Session Registry: Module Participation
# ========================================
# Each module that wants to participate in session save/restore registers
# a save_fn and restore_fn via the registry. The central save/restore logic
# calls all registered getters/setters in priority order.

#' Create a new session registry
#'
#' Returns a list with registration, save, and restore methods.
#' @return A list acting as the session registry.
create_session_registry <- function() {
  participants <- list()
  current_restore_snapshot_ids <- character()

  list(
    #' Register a module for session save/restore participation.
    #'
    #' @param module_id Unique string identifying the module.
    #' @param save_fn Function returning a named list of serializable state.
    #' @param restore_fn Function accepting a state list and restoring internal state.
    #' @param priority Integer controlling restore order (lower = earlier). Default 50.
    #' @param save_level Minimum save level required to include this module.
    #'   One of SESSION_SAVE_LEVEL_DATA, SESSION_SAVE_LEVEL_ANALYSIS,
    #'   SESSION_SAVE_LEVEL_FULL. Default SESSION_SAVE_LEVEL_DATA.
    register = function(module_id, save_fn, restore_fn, priority = 50L,
                        save_level = SESSION_SAVE_LEVEL_DATA) {
      stopifnot(is.character(module_id), length(module_id) == 1L)
      stopifnot(is.function(save_fn))
      stopifnot(is.function(restore_fn))
      participants[[module_id]] <<- list(
        save_fn   = save_fn,
        restore_fn = restore_fn,
        priority   = as.integer(priority),
        save_level = save_level
      )
      debug_log(paste("Session registry: registered", module_id,
                       "(priority", priority, ", level", save_level, ")"), 2)
    },

    #' Collect snapshots from all registered modules.
    #'
    #' @param save_level The selected save level from the UI.
    #' @return Named list of module snapshots.
    collect_snapshots = function(save_level = SESSION_SAVE_LEVEL_FULL,
                                progress_fn = NULL) {
      level_order <- c(SESSION_SAVE_LEVEL_DATA,
                       SESSION_SAVE_LEVEL_ANALYSIS,
                       SESSION_SAVE_LEVEL_FULL)
      selected_idx <- match(save_level, level_order)
      if (!is.numeric(selected_idx) || length(selected_idx) != 1L || is.na(selected_idx)) {
        selected_idx <- length(level_order)
      }

      # Determine eligible modules first so progress_fn knows the total.
      # Save snapshots are collected in the same priority order used by
      # restores so modules with data/UI dependencies can rely on one
      # canonical registry ordering for both save and restore paths.
      eligible_ids <- character()
      for (mod_id in names(participants)) {
        entry <- participants[[mod_id]]
        entry_idx <- match(entry$save_level, level_order)
        if (!is.numeric(entry_idx) || length(entry_idx) != 1L || is.na(entry_idx)) entry_idx <- 1L
        if (isTRUE(entry_idx <= selected_idx)) {
          eligible_ids <- c(eligible_ids, mod_id)
        }
      }
      if (length(eligible_ids) > 0L) {
        eligible_priorities <- vapply(eligible_ids, function(id) {
          participants[[id]]$priority
        }, integer(1L))
        eligible_ids <- eligible_ids[order(eligible_priorities)]
      }

      snapshots <- list()
      for (i in seq_along(eligible_ids)) {
        mod_id <- eligible_ids[i]
        if (is.function(progress_fn)) {
          progress_fn(mod_id, i, length(eligible_ids))
        }

        participant_priority <- participants[[mod_id]]$priority
        participant_start <- proc.time()[["elapsed"]]
        snap <- tryCatch({
          save_fn <- participants[[mod_id]]$save_fn
          save_formals <- tryCatch(formals(save_fn), error = function(e) NULL)
          supports_save_level <- !is.null(save_formals) &&
            ("save_level" %in% names(save_formals) || "..." %in% names(save_formals))

          isolate({
            if (isTRUE(supports_save_level)) {
              save_fn(save_level = save_level)
            } else {
              save_fn()
            }
          })
        }, error = function(e) {
          debug_log(paste("Session save failed for module", mod_id, ":", e$message), 1)
          NULL
        }, finally = {
          participant_elapsed <- proc.time()[["elapsed"]] - participant_start
          debug_log(paste0(
            "[SaveModuleTiming] module=", mod_id,
            " priority=", participant_priority,
            " elapsed=", round(participant_elapsed, 3)
          ), 1)
        })
        if (!is.null(snap)) {
          snapshots[[mod_id]] <- snap
        }
      }
      .log_session_save_contract_debug(save_level, snapshots)
      snapshots
    },

    #' Restore module state from collected snapshots.
    #'
    #' Calls each module's restore_fn in priority order (low to high).
    #' @param module_snapshots Named list as produced by collect_snapshots.
    #' @param restore_context_snapshots Complete snapshot set for restore diagnostics;
    #'   defaults to \code{module_snapshots} for non-phased callers.
    #' @return Character vector of module IDs that failed to restore.
    restore_snapshots = function(module_snapshots, progress_fn = NULL, phase = NULL,
                                 restore_context_snapshots = module_snapshots) {
      if (is.null(module_snapshots) || length(module_snapshots) == 0L) {
        return(character())
      }

      # Phased restores dispatch only a subset of the payload at a time. Keep
      # the IDs from the complete restore payload available to participants so
      # their diagnostics describe what this restore will replay, rather than
      # every module that happens to be registered in the running application.
      previous_restore_snapshot_ids <- current_restore_snapshot_ids
      current_restore_snapshot_ids <<- names(restore_context_snapshots) %||% character()
      on.exit(current_restore_snapshot_ids <<- previous_restore_snapshot_ids, add = TRUE)

      # Build ordered list of modules to restore
      ids_to_restore <- intersect(names(module_snapshots), names(participants))
      if (length(ids_to_restore) == 0L) return(character())

      priorities <- vapply(ids_to_restore, function(id) {
        participants[[id]]$priority
      }, integer(1L))
      ids_to_restore <- ids_to_restore[order(priorities)]

      failed_modules <- character()
      for (i in seq_along(ids_to_restore)) {
        mod_id <- ids_to_restore[i]
        if (is.function(progress_fn)) {
          progress_fn(mod_id, i, length(ids_to_restore))
        }
        participant_priority <- participants[[mod_id]]$priority
        participant_start <- proc.time()[["elapsed"]]
        tryCatch({
          restore_fn <- participants[[mod_id]]$restore_fn
          restore_formals <- tryCatch(formals(restore_fn), error = function(e) NULL)
          supports_phase <- !is.null(restore_formals) &&
            ("phase" %in% names(restore_formals) || "..." %in% names(restore_formals))
          if (isTRUE(supports_phase)) {
            restore_fn(module_snapshots[[mod_id]], phase = phase)
          } else {
            restore_fn(module_snapshots[[mod_id]])
          }
          debug_log(paste("Session restore: module", mod_id, "restored"), 2)
        }, error = function(e) {
          debug_log(paste("Session restore failed for module", mod_id, ":", e$message), 1)
          failed_modules <<- c(failed_modules, mod_id)
        }, finally = {
          participant_elapsed <- proc.time()[["elapsed"]] - participant_start
          debug_log(paste0(
            "[RestoreModuleTiming] module=", mod_id,
            " phase=", phase %||% "default",
            " priority=", participant_priority,
            " elapsed=", round(participant_elapsed * 1000, 0), "ms"
          ), 1)
        })
      }
      failed_modules
    },

    #' Return the list of currently registered participant IDs.
    #' @return Character vector.
    registered_ids = function() {
      names(participants)
    },

    #' Return module IDs in the snapshot currently being restored.
    #'
    #' During a phased restore this reflects the complete payload supplied as
    #' restore_context_snapshots, not merely the modules dispatched in one phase.
    current_restore_snapshot_ids = function() {
      current_restore_snapshot_ids
    }
  )
}


.session_runtime_object_label <- function(x) {
  if (is.environment(x)) return("environment")
  if (is.function(x)) return("function")
  if (typeof(x) == "externalptr") return("externalptr")
  if (inherits(x, "miraprot_plot_bundle", which = FALSE) ||
      (is.list(x) && identical(x$kind, "ggplot"))) return("miraprot_plot_bundle")
  if (inherits(x, "ggplot", which = FALSE)) return("ggplot")
  if (inherits(x, "plotly", which = FALSE)) return("plotly")
  if (inherits(x, "htmlwidget", which = FALSE)) return("htmlwidget")
  if (inherits(x, "ggproto", which = FALSE)) return("ggproto")
  if (inherits(x, "shiny.tag", which = FALSE)) return("shiny.tag")
  NULL
}

.find_session_runtime_objects <- function(x, path = character(0L), max_hits = 25L) {
  hits <- character()
  walk <- function(value, current_path) {
    if (length(hits) >= max_hits) return(invisible(NULL))
    label <- .session_runtime_object_label(value)
    if (!is.null(label)) {
      hits <<- c(hits, paste0(
        if (length(current_path) == 0L) "<root>" else paste(current_path, collapse = "$"),
        ":", label
      ))
      return(invisible(NULL))
    }
    if (is.list(value) && !isS4(value)) {
      nms <- names(value) %||% as.character(seq_along(value))
      for (i in seq_along(value)) {
        nm <- nms[[i]]
        if (!nzchar(nm)) nm <- paste0("[[", i, "]]")
        walk(value[[i]], c(current_path, nm))
        if (length(hits) >= max_hits) break
      }
    }
    invisible(NULL)
  }
  walk(x, path)
  hits
}

.session_schema_approved_module_fields <- function() {
  c(
    "module_id", "module_state", "ui_inputs", "loader_state",
    "submodule_ui_states", "submodule_states", "assign_rules_condition_state",
    "data_mod", "data_def",
    "primary_data_raw", "primary_data_raw_rv", "final_processed_data",
    "final_processed_metadata", "handson_metadata", "session_safe",
    "session_payload_shape", "contains_plot_object", "restore_cache_dependency",
    "plot_data_cache_payload", "plot_data_cache_ref", "plot_data_cache_fingerprint",
    "data_mod_revision_id", "data_def_revision_id"
  )
}

is_canonical_plot_data_pair <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(FALSE)
  if (!all(c("data_mod", "data_def") %in% names(x))) return(FALSE)
  if (!is.data.frame(x$data_mod) || !is.data.frame(x$data_def)) return(FALSE)

  has_runtime_column <- function(df) {
    cols <- unclass(df)
    any(vapply(cols, function(col) {
      is.list(col) || is.environment(col) || is.function(col) ||
        isS4(col) || identical(typeof(col), "externalptr")
    }, logical(1L)))
  }

  !has_runtime_column(x$data_mod) && !has_runtime_column(x$data_def)
}

.is_known_schema_v2_module_snapshot <- function(snapshot) {
  if (!is.list(snapshot)) return(FALSE)
  version_fields <- list(
    snapshot$schema_version, snapshot$module_schema_version,
    snapshot$session_schema_version, snapshot$payload_schema_version,
    snapshot$version
  )
  any(vapply(version_fields, function(v) {
    is.character(v) && length(v) == 1L && identical(v, "2.0")
  }, logical(1L))) ||
    (isTRUE(snapshot$session_safe) &&
       identical(snapshot$session_payload_shape, "analysis_result_plus_ui_v1"))
}

.sanitize_schema_v2_module_snapshot <- function(snapshot, mod_id = "<unknown>", memo = NULL) {
  if (!is.list(snapshot)) {
    return(structure(list(value = NULL, dropped = "<root>", quarantined = character()), class = "miraprot_sanitizer_result"))
  }
  approved <- .session_schema_approved_module_fields()
  legacy_patterns <- "(^|_)(legacy|fallback|raw_result|result_object|results|plot|ggplot|plotly|cache)(_|$)|restore_plot_data_cache"
  out <- snapshot
  dropped <- character()
  quarantined <- character()
  quarantine <- list()
  nms <- names(snapshot) %||% character()

  for (field in nms) {
    value <- snapshot[[field]]
    field_path <- c(mod_id, field)
    if (field %in% c("plot_data_cache_payload", "restore_plot_data_cache") &&
        is_canonical_plot_data_pair(value)) {
      debug_log(paste0(
        "schema sanitizer: trusted canonical plot data pair for ",
        mod_id, "$", field
      ), 2)
      next
    }
    if (field %in% approved &&
        field %in% c("ui_inputs", "plot_ui_inputs", "plot_ui_cache", "plot_request")) {
      if (is_plain_ui_payload(value)) {
        next
      }
      cleaned <- .sanitize_for_serialization(value, path = field_path, memo = memo)
      if (is.null(cleaned) && !is.null(value)) {
        out[[field]] <- NULL
        dropped <- c(dropped, field)
      } else {
        out[field] <- list(cleaned)
      }
      next
    }

    runtime_hits <- .find_session_runtime_objects(value, field_path)
    if (length(runtime_hits) > 0L) {
      out[[field]] <- NULL
      quarantine[[field]] <- list(reason = "unexpected_runtime_object", hits = runtime_hits)
      quarantined <- c(quarantined, field)
      next
    }

    if (field %in% approved && (is.null(value) || is.atomic(value) || is.data.frame(value) || is.list(value))) {
      next
    }

    if (grepl(legacy_patterns, field, ignore.case = TRUE)) {
      cleaned <- .sanitize_for_serialization(value, path = field_path, memo = memo)
      if (is.null(cleaned) && !is.null(value)) {
        out[[field]] <- NULL
        dropped <- c(dropped, field)
      } else {
        out[field] <- list(cleaned)
      }
      next
    }

    if (!(is.null(value) || is.atomic(value) || is.data.frame(value) || .is_plain_serializable_list(value))) {
      out[[field]] <- NULL
      dropped <- c(dropped, field)
    }
  }

  if (length(quarantine) > 0L) {
    out$.session_restore_quarantine <- quarantine
  }
  structure(list(value = out, dropped = unique(dropped), quarantined = unique(quarantined)),
            class = "miraprot_sanitizer_result")
}

.session_snapshot_shape_value <- function(x) {
  if (is.null(x)) return("absent")
  if (inherits(x, "data.frame")) {
    return(paste0("data.frame[", nrow(x), "x", ncol(x), "]"))
  }
  if (is.list(x)) return(paste0("list[", length(x), "]"))
  paste0(class(x)[1], "[", length(x), "]")
}

.log_session_save_contract_debug <- function(save_level, module_snapshots) {
  dw <- if (is.list(module_snapshots)) module_snapshots$datawizard else NULL
  loader_state <- if (is.list(dw)) dw$loader_state else NULL
  loader_mode <- if (is.list(loader_state) && !is.null(loader_state$mode)) {
    as.character(loader_state$mode)[1]
  } else {
    NA_character_
  }
  submodule_ui_count <- if (is.list(dw) && is.list(dw$submodule_ui_states)) {
    length(dw$submodule_ui_states$submodules %||% list())
  } else {
    0L
  }
  non_datawizard_count <- if (is.list(module_snapshots)) {
    length(setdiff(names(module_snapshots) %||% character(), "datawizard"))
  } else {
    0L
  }

  canonical_data <- if (is.list(dw)) dw$data_mod else NULL
  metadata <- if (is.list(dw)) dw$data_def else NULL

  debug_log(paste(
    "Session save contract debug:",
    "save_level=", save_level,
    "| canonical_data_present=", !is.null(canonical_data),
    "| canonical_data_shape=", .session_snapshot_shape_value(canonical_data),
    "| metadata_present=", !is.null(metadata),
    "| metadata_shape=", .session_snapshot_shape_value(metadata),
    "| loader_state_mode=", loader_mode %||% "absent",
    "| submodule_ui_count=", submodule_ui_count,
    "| non_datawizard_module_count=", non_datawizard_count
  ), 1)

  invisible(NULL)
}

# ============================================================================
# Legacy restore migration helpers (read-only compatibility)
# ============================================================================
# Schema 2.0 module save paths must not write rendered ggplot/plotly/current
# plot objects, ComplexHeatmap/rendered plot objects, or old
# restore_plot_data_cache bundles with embedded data frames.  The helpers below
# are intentionally restore-only: they normalize older snapshots so current
# restore harnesses can rebuild plots from schema 2.0 fields while keeping the
# legacy object fields clearly quarantined from new saves.

restore_legacy_session_state_v1 <- function(state) {
  if (!is.list(state)) return(state)
  migrated <- state
  migrated$.legacy_restore_source <- migrated$.legacy_restore_source %||% "v1"
  migrated$.legacy_runtime_fields <- intersect(
    names(migrated) %||% character(),
    c(
      "ggplot_object", "plotly_object", "current_plot",
      "current_plot_object", "plot_object", "base_plot_without_labels",
      "rendered_heatmap", "rendered_plot", "heatmap_object",
      "restore_plot_data_cache", "restore_plot_data_cache_by_title"
    )
  )
  migrated
}

migrate_legacy_plot_object_state <- function(state) {
  if (!is.list(state)) return(state)
  migrated <- state
  legacy_fields <- c(
    "ggplot_object", "plotly_object", "current_plot",
    "current_plot_object", "plot_object", "base_plot_without_labels",
    "rendered_heatmap", "rendered_plot", "heatmap_object"
  )
  present <- intersect(names(migrated) %||% character(), legacy_fields)
  if (length(present) > 0L) {
    migrated$.legacy_plot_object_fields <- present
  }
  migrated
}

#' Validate a session snapshot object
#'
#' Checks that the object read from an RDS file conforms to the expected
#' session snapshot schema. Returns a list with \code{valid} (logical) and
#' \code{message} (character) fields.
#'
#' Accepts v1 / v2 (rv_snapshot at top level) and v3 inline envelope shapes.
#' Envelope snapshots are unwrapped first via
#' \code{\link{unwrap_snapshot}}.
#'
#' @param snapshot The object returned by \code{readRDS()}.
#' @return A list with \code{valid} and \code{message}.
validate_session_snapshot <- function(snapshot) {
  # 1. Must be a named list
  if (!is.list(snapshot) || is.null(names(snapshot))) {
    return(list(
      valid = FALSE,
      message = "The file does not contain a valid session snapshot (not a named list)."
    ))
  }

  # Failed downloads use an explicit discriminator. Detect it before version,
  # envelope, or canonical-data checks so the stub (including its deliberately
  # absent data_mod) cannot be mistaken for a malformed regular session. An
  # `error` field alone is intentionally insufficient: valid legacy envelopes
  # did not have this discriminator and remain eligible for normal validation.
  if (identical(snapshot$session_file_type, MIRAPROT_SESSION_ERROR_MARKER)) {
    save_error <- .sanitize_session_error_message(snapshot$error)
    return(list(
      valid = FALSE,
      message = paste0(
        "This download contains a session-save error marker: ", save_error
      )
    ))
  }

  # 2. Magic marker
  if (!.is_supported_session_marker(snapshot)) {
    return(list(
      valid = FALSE,
      message = "The file is not a MiraProt session file (missing identification marker)."
    ))
  }

  # 3. Schema version present and is a character string
  if (!is.character(snapshot$version) || length(snapshot$version) != 1) {
    return(list(
      valid = FALSE,
      message = "The session file has no valid schema version."
    ))
  }

  # 4. Version compatibility
  if (!(snapshot$version %in% MIRAPROT_SESSION_COMPATIBLE_VERSIONS)) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Incompatible session file version: ", snapshot$version, ". ",
        "This version of MiraProt supports: ",
        paste(MIRAPROT_SESSION_COMPATIBLE_VERSIONS, collapse = ", "), "."
      )
    ))
  }

  # 4b. Unwrap v3 payload so downstream checks see rv_snapshot at top level.
  snapshot <- unwrap_snapshot(snapshot)
  if (!is.null(snapshot$.unwrap_error)) {
    return(list(valid = FALSE, message = snapshot$.unwrap_error))
  }

  # 5. rv_snapshot must be a list containing data_mod as a data.frame
  if (!is.list(snapshot$rv_snapshot)) {
    return(list(
      valid = FALSE,
      message = "The session file does not contain the expected data snapshot (rv_snapshot)."
    ))
  }

  if (!inherits(snapshot$rv_snapshot$data_mod, "data.frame")) {
    return(list(
      valid = FALSE,
      message = "The session file does not contain valid processed data (data_mod must be a data.frame)."
    ))
  }

  # All checks passed
  list(valid = TRUE, message = "Session file is valid and ready for restoration.")
}

# ========================================
# v3/v4 envelope helpers (inline and current qs2 payloads)
# ========================================

.is_supported_session_marker <- function(snapshot) {
  is.list(snapshot) &&
    (isTRUE(snapshot$miraprot_session) || isTRUE(snapshot$shinyprot_session))
}

#' Is the qs2 package available for current session payloads?
#' @keywords internal
.qs2_available <- function() {
  requireNamespace("qs2", quietly = TRUE)
}

#' Build a v4 snapshot envelope
#'
#' Serializes the payload with qs2 when available. Availability and
#' serialization failures fall back to an inline RDS payload.
#'
#' @return A named list suitable for \code{saveRDS()}.
#' @keywords internal
.build_v4_envelope <- function(rv_snapshot, module_snapshots, save_level,
                               failed_modules = character(),
                               qs_preset = "balanced") {
  data_dims <- tryCatch({
    if (inherits(rv_snapshot$data_mod, "data.frame")) {
      c(nrow(rv_snapshot$data_mod), ncol(rv_snapshot$data_mod))
    } else c(NA_integer_, NA_integer_)
  }, error = function(e) c(NA_integer_, NA_integer_))
  qs_preset <- if (is.character(qs_preset) && length(qs_preset) >= 1L &&
                   nzchar(qs_preset[[1L]])) qs_preset[[1L]] else "balanced"
  payload <- list(rv_snapshot = rv_snapshot, module_snapshots = module_snapshots)
  envelope <- list(
    miraprot_session = TRUE,
    version = MIRAPROT_SESSION_SCHEMA_VERSION,
    app_version = MIRAPROT_APP_VERSION,
    created_at = Sys.time(),
    save_level = save_level,
    manifest = list(
      module_ids = names(module_snapshots) %||% character(),
      failed_modules = as.character(failed_modules),
      data_dims = as.integer(data_dims),
      transport = "inline_rds",
      transport_preset = qs_preset
    )
  )

  qs2_available <- tryCatch(.qs2_available(), error = function(e) {
    message("MiraProt session save: qs2 availability check failed; using inline RDS fallback: ",
            conditionMessage(e))
    FALSE
  })
  serialized <- if (isTRUE(qs2_available)) {
    tryCatch(
      qs2::qs_serialize(
        payload,
        compress_level = if (identical(save_level, SESSION_SAVE_LEVEL_DATA)) 1L else 3L,
        nthreads = 1L,
        shuffle = TRUE
      ),
      error = function(e) {
        message("MiraProt session save: qs2 serialization failed; using inline RDS fallback: ",
                conditionMessage(e))
        NULL
      }
    )
  } else {
    message("MiraProt session save: qs2 package is unavailable; using inline RDS fallback transport.")
    NULL
  }
  if (is.raw(serialized)) {
    envelope$manifest$transport <- "qs2"
    envelope$payload_qs2 <- serialized
  } else {
    envelope$payload_inline <- payload
  }
  envelope
}

#' Build a v3 snapshot envelope
#'
#' Builds the historical v3 inline envelope used by compatibility tests and
#' readers. The envelope itself is written with \code{saveRDS()}.
#'
#' @param rv_snapshot Shared rv state (list).
#' @param module_snapshots Named list of module snapshots (may be NULL).
#' @param save_level One of the SESSION_SAVE_LEVEL_* constants.
#' @param failed_modules Character vector of module IDs that failed to snapshot.
#' @param qs_preset Preset used to select base-R fallback compression.
#' @return A named list suitable for \code{saveRDS()}.
#' @keywords internal
.build_v3_envelope <- function(rv_snapshot, module_snapshots, save_level,
                               failed_modules = character(),
                               qs_preset = "balanced") {
  data_dims <- tryCatch({
    if (inherits(rv_snapshot$data_mod, "data.frame")) {
      c(nrow(rv_snapshot$data_mod), ncol(rv_snapshot$data_mod))
    } else c(NA_integer_, NA_integer_)
  }, error = function(e) c(NA_integer_, NA_integer_))

  qs_preset <- if (is.character(qs_preset) && length(qs_preset) >= 1L &&
                   nzchar(qs_preset[[1L]])) {
    qs_preset[[1L]]
  } else {
    "balanced"
  }

  manifest <- list(
    module_ids       = names(module_snapshots) %||% character(),
    failed_modules   = as.character(failed_modules),
    data_dims        = as.integer(data_dims),
    transport        = "inline_rds",
    transport_preset = qs_preset
  )

  payload <- list(
    rv_snapshot      = rv_snapshot,
    module_snapshots = module_snapshots
  )

  envelope <- list(
    miraprot_session = TRUE,
    version           = "3.0.0",
    app_version       = MIRAPROT_APP_VERSION,
    created_at        = Sys.time(),
    save_level        = save_level,
    manifest          = manifest
  )

  envelope$payload_inline <- payload
  envelope
}

#' Select base-R saveRDS compression for the inline fallback transport
#'
#' qs presets are reused as the public transport preset knob so payload
#' semantics remain unchanged while the base-R fallback can prefer speed or
#' size.  saveRDS accepts TRUE/"gzip", "bzip2", "xz", or FALSE.
#'
#' @param transport Transport name from the v3 manifest.
#' @param transport_preset Preset name from the v3 manifest.
#' @return Compression argument suitable for saveRDS().
#' @keywords internal
.session_rds_compress_for_transport_preset <- function(transport, transport_preset) {
  if (!identical(transport, "inline_rds") && !identical(transport, "inline")) {
    return(TRUE)
  }
  preset <- if (is.character(transport_preset) && length(transport_preset) >= 1L) {
    transport_preset[[1L]]
  } else {
    "balanced"
  }
  switch(
    preset,
    "fast" = "gzip",
    "balanced" = "gzip",
    "high" = "xz",
    "archive" = "xz",
    "small" = "xz",
    TRUE
  )
}

#' Unwrap a snapshot to expose rv_snapshot / module_snapshots at top level
#'
#' Accepts any of the v1 / v2 / v3 / v4 on-disk shapes and returns an object
#' whose top-level fields \code{rv_snapshot} and \code{module_snapshots}
#' are populated.  Used by \code{\link{validate_session_snapshot}} and by
#' the restore observer so downstream code does not have to care about
#' the transport format.
#'
#' @param snapshot Result of \code{readRDS()} on a session file.
#' @return The snapshot list, possibly with the payload merged in.
#' @keywords internal
unwrap_snapshot <- function(snapshot) {
  if (!is.list(snapshot)) return(snapshot)

  version <- if (is.character(snapshot$version) && length(snapshot$version) == 1L) {
    snapshot$version
  } else NA_character_

  merge_payload <- function(payload) {
    if (!is.list(payload)) return()
    if (!is.null(payload$rv_snapshot)) snapshot$rv_snapshot <<- payload$rv_snapshot
    if (!is.null(payload$module_snapshots)) snapshot$module_snapshots <<- payload$module_snapshots
  }
  transport <- tryCatch(snapshot$manifest$transport, error = function(e) NULL)
  transport <- if (is.character(transport) && length(transport) == 1L &&
                   !is.na(transport)) transport else NA_character_

  # Select the schema first so payloads are never considered outside their
  # own version branch.
  if (!is.na(version) && identical(version, "4.0.0")) {
    if (identical(transport, "qs2")) {
      if (!is.raw(snapshot$payload_qs2)) {
        snapshot$.unwrap_error <- "The v4 qs2 transport requires a raw payload_qs2 vector."
        return(snapshot)
      }
      if (!isTRUE(tryCatch(.qs2_available(), error = function(e) FALSE))) {
        snapshot$.unwrap_error <- paste(
          "This session file uses the v4 qs2 transport, but the qs2 package",
          "is unavailable. Install 'qs2' and try again."
        )
        return(snapshot)
      }
      payload <- tryCatch(
        qs2::qs_deserialize(snapshot$payload_qs2, validate_checksum = TRUE),
        error = function(e) {
          snapshot$.unwrap_error <<- paste(
            "The v4 qs2 payload could not be deserialized or failed checksum validation:",
            conditionMessage(e)
          )
          NULL
        }
      )
      merge_payload(payload)
      return(snapshot)
    }
    if (identical(transport, "inline_rds")) {
      if (!is.list(snapshot$payload_inline)) {
        snapshot$.unwrap_error <- "The v4 inline_rds transport requires a list payload_inline."
        return(snapshot)
      }
      merge_payload(snapshot$payload_inline)
      return(snapshot)
    }
    snapshot$.unwrap_error <- "Unsupported v4 session transport; expected 'qs2' or 'inline_rds'."
    return(snapshot)
  }

  if (!is.na(version) && identical(version, "3.0.0")) {
    if (transport %in% c("inline_rds", "inline")) {
      if (!is.list(snapshot$payload_inline)) {
        snapshot$.unwrap_error <- "The v3 inline transport requires a list payload_inline."
        return(snapshot)
      }
      merge_payload(snapshot$payload_inline)
      return(snapshot)
    }
    snapshot$.unwrap_error <- paste(
      "Unsupported v3 session transport; expected 'inline_rds' or 'inline'."
    )
    return(snapshot)
  }

  # v1 / v2: already has rv_snapshot / module_snapshots at the top
  snapshot
}

# ========================================
# Legacy restore compatibility shim
# ========================================

.session_restore_is_df <- function(x) inherits(x, "data.frame") && nrow(x) >= 0L && ncol(x) > 0L

.session_restore_meaningful_metadata <- function(meta) {
  if (!is.data.frame(meta) || nrow(meta) == 0L || !"Content" %in% names(meta)) return(FALSE)
  vals <- trimws(as.character(meta$Content))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  length(vals) > 0L && any(tolower(vals) != "row index")
}

.session_restore_metadata_matches <- function(meta, data) {
  is.data.frame(meta) && is.data.frame(data) &&
    "Column" %in% names(meta) &&
    nrow(meta) == ncol(data) &&
    identical(as.character(meta$Column), as.character(names(data)))
}

.session_restore_extract_legacy_field <- function(snapshot, field) {
  candidates <- list(
    tryCatch(snapshot$rv_snapshot[[field]], error = function(e) NULL),
    tryCatch(snapshot$module_snapshots$datawizard$module_state[[field]], error = function(e) NULL),
    tryCatch(snapshot$module_snapshots$datawizard[[field]], error = function(e) NULL),
    tryCatch(snapshot[[field]], error = function(e) NULL)
  )
  for (cand in candidates) {
    if (!is.null(cand)) return(cand)
  }
  NULL
}

.session_restore_build_canonical_bundle <- function(snapshot) {
  data_mod <- .session_restore_extract_legacy_field(snapshot, "data_mod")
  final_processed_data <- .session_restore_extract_legacy_field(snapshot, "final_processed_data")
  primary_data_raw <- .session_restore_extract_legacy_field(snapshot, "primary_data_raw")
  primary_data_raw_rv <- .session_restore_extract_legacy_field(snapshot, "primary_data_raw_rv")

  processed <- if (.session_restore_is_df(data_mod)) data_mod else
    if (.session_restore_is_df(final_processed_data)) final_processed_data else
      if (.session_restore_is_df(primary_data_raw)) primary_data_raw else NULL
  raw <- if (.session_restore_is_df(primary_data_raw_rv)) primary_data_raw_rv else
    if (.session_restore_is_df(primary_data_raw)) primary_data_raw else
      if (.session_restore_is_df(processed)) processed else NULL

  metadata_candidates <- list(
    data_def = .session_restore_extract_legacy_field(snapshot, "data_def"),
    final_processed_metadata = .session_restore_extract_legacy_field(snapshot, "final_processed_metadata"),
    handson_metadata = .session_restore_extract_legacy_field(snapshot, "handson_metadata")
  )
  valid_metadata <- metadata_candidates[vapply(metadata_candidates, function(m) {
    .session_restore_metadata_matches(m, processed) &&
      .session_restore_meaningful_metadata(m)
  }, logical(1))]
  metadata <- if (length(valid_metadata) > 0L) valid_metadata[[1L]] else NULL

  list(
    data_mod = processed,
    primary_data_raw = raw,
    data_def = metadata,
    metadata_pending = is.null(metadata) && .session_restore_is_df(processed),
    metadata_source = if (length(valid_metadata) > 0L) names(valid_metadata)[1L] else "pending",
    data_source = if (.session_restore_is_df(data_mod)) "data_mod" else
      if (.session_restore_is_df(final_processed_data)) "final_processed_data" else
        if (.session_restore_is_df(primary_data_raw)) "primary_data_raw" else "none"
  )
}

.session_restore_is_native_current_schema <- function(snapshot) {
  if (!is.list(snapshot) ||
      !identical(snapshot$version, MIRAPROT_SESSION_SCHEMA_VERSION) ||
      !isTRUE(snapshot$miraprot_session) ||
      !is.list(snapshot$manifest)) {
    return(FALSE)
  }

  transport <- snapshot$manifest$transport
  preset <- snapshot$manifest$transport_preset
  valid_transport <- is.character(transport) && length(transport) == 1L &&
    !is.na(transport) && transport %in% c("qs2", "inline_rds")
  valid_preset <- is.character(preset) && length(preset) == 1L &&
    !is.na(preset) && nzchar(preset)
  valid_transport_payload <- if (isTRUE(valid_transport) && identical(transport, "qs2")) {
    is.raw(snapshot$payload_qs2)
  } else if (isTRUE(valid_transport)) {
    payload <- snapshot$payload_inline
    is.list(payload) &&
      all(c("rv_snapshot", "module_snapshots") %in% names(payload)) &&
      is.list(payload$rv_snapshot) &&
      (is.null(payload$module_snapshots) || is.list(payload$module_snapshots))
  } else {
    FALSE
  }

  # unwrap_snapshot() retains the envelope fields while exposing the payload
  # members (module_snapshots is optional). Requiring the rv payload shape
  # distinguishes a successfully unwrapped current envelope from a version-stamped,
  # structurally legacy top-level snapshot.
  valid_unwrapped_payload <- is.list(snapshot$rv_snapshot) &&
    (is.null(snapshot$module_snapshots) || is.list(snapshot$module_snapshots))

  isTRUE(valid_transport) && isTRUE(valid_preset) &&
    isTRUE(valid_transport_payload) && isTRUE(valid_unwrapped_payload)
}

upgrade_session_snapshot_to_current_schema <- function(snapshot) {
  if (!is.list(snapshot)) return(snapshot)

  native_current <- .session_restore_is_native_current_schema(snapshot)
  if (isTRUE(native_current) && is.null(snapshot$plot_data_cache_pool)) {
    snapshot$plot_data_cache_pool <- list()
  }
  bundle <- .session_restore_build_canonical_bundle(snapshot)
  mode <- if (isTRUE(native_current)) "native_current_schema" else "legacy_upgraded_schema"

  if (!is.list(snapshot$rv_snapshot)) snapshot$rv_snapshot <- list()
  if (.session_restore_is_df(bundle$data_mod)) snapshot$rv_snapshot$data_mod <- bundle$data_mod
  if (.session_restore_is_df(bundle$primary_data_raw)) snapshot$rv_snapshot$primary_data_raw <- bundle$primary_data_raw
  if (is.data.frame(bundle$data_def)) {
    snapshot$rv_snapshot$data_def <- bundle$data_def
  } else {
    snapshot$rv_snapshot$data_def <- NULL
  }
  snapshot$rv_snapshot$restore_plot_data_cache <- NULL

  if (!is.list(snapshot$module_snapshots)) snapshot$module_snapshots <- list()
  dw <- snapshot$module_snapshots$datawizard
  if (!is.list(dw)) dw <- list(module_id = "datawizard", module_state = list())
  st <- dw$module_state
  if (!is.list(st)) st <- dw
  if (.session_restore_is_df(bundle$data_mod)) st$data_mod <- bundle$data_mod
  if (.session_restore_is_df(bundle$primary_data_raw)) {
    st$primary_data_raw <- bundle$primary_data_raw
    st$primary_data_raw_rv <- bundle$primary_data_raw
  }
  if (is.data.frame(bundle$data_def)) {
    st$data_def <- bundle$data_def
    st$handson_metadata <- bundle$data_def
    st$final_processed_metadata <- bundle$data_def
  }
  st$legacy_restore_metadata_pending <- isTRUE(bundle$metadata_pending)
  st$legacy_restore_metadata_source <- bundle$metadata_source
  dw$module_state <- st
  snapshot$module_snapshots$datawizard <- dw

  legacy_full_session <- !isTRUE(native_current) &&
    (identical(snapshot$save_level, SESSION_SAVE_LEVEL_FULL) ||
       identical(snapshot$save_level, "full_session") ||
       is.null(snapshot$save_level))
  if (isTRUE(legacy_full_session) &&
      .session_restore_is_df(bundle$data_mod) && is.data.frame(bundle$data_def)) {
    fallback_cache <- list(data_mod = bundle$data_mod, data_def = bundle$data_def)
    cache_id <- .build_plot_data_cache_id(data_mod = bundle$data_mod, data_def = bundle$data_def)
    if (!is.list(snapshot$plot_data_cache_pool)) snapshot$plot_data_cache_pool <- list()
    put_result <- .safe_cache_pool_put(snapshot$plot_data_cache_pool, cache_id, fallback_cache)
    snapshot$plot_data_cache_pool <- put_result$pool
    for (mid in setdiff(names(snapshot$module_snapshots), "datawizard")) {
      mst <- snapshot$module_snapshots[[mid]]$module_state
      if (!is.list(mst)) next
      if (is.null(mst$restore_cache_dependency)) {
        mst$restore_cache_dependency <- "shared_plot_data_cache_pool"
      }
      if (!is.list(mst$restore_plot_data_cache)) {
        mst$restore_plot_data_cache <- fallback_cache
        mst$restore_cache_resolution_mode <- "legacy_canonical_fallback"
      }
      if (is.null(mst$plot_data_cache_ref)) mst$plot_data_cache_ref <- cache_id
      snapshot$module_snapshots[[mid]]$module_state <- mst
    }
  }

  snapshot$compatibility_upgrade <- list(
    mode = mode,
    data_source = bundle$data_source,
    metadata_source = bundle$metadata_source,
    metadata_pending = isTRUE(bundle$metadata_pending),
    note = if (isTRUE(native_current)) "native current schema" else "legacy snapshot normalized before module restore"
  )
  debug_log(paste(
    "Session restore compatibility:",
    snapshot$compatibility_upgrade$mode,
    "| data=", bundle$data_source,
    "| metadata=", bundle$metadata_source,
    "| metadata_pending=", bundle$metadata_pending
  ), 1)
  snapshot
}

# ========================================
# Plot bundle helpers (ggplot + data)
# ========================================

#' Package a ggplot and its underlying data for session storage
#'
#' Stores the ggplot object with its plot/layer environments neutralized
#' (see \code{\link{.strip_ggplot_env}}) alongside the data frame used to
#' build it.  This guarantees that a round-trip through disk yields both
#' a *rendered* plot and the *tidy data* that produced it -- addressing
#' the user request to "save ggplot elements and the underlying data
#' together".
#'
#' Note: the \code{build_data} slot (pre-computed layer data from
#' \code{ggplot2::ggplot_build()}) is retained in the bundle shape for
#' backward compatibility with older readers, but is populated as
#' \code{NULL}.  \code{ggplot_build()} is extremely expensive (it runs
#' every stat / scale / coord transformation) and \code{.restore_plot_from_snapshot()}
#' never reads \code{build_data} -- so computing it on every save was
#' pure waste and is the single biggest contributor to slow snapshot
#' creation for modules like Volcano with thousands of points.
#'
#' @param p A ggplot object (or anything else, returned unchanged).
#' @return The input plot/object unchanged. New session saves must not write
#'   \code{miraprot_plot_bundle} payloads.
#' @keywords internal
.prepare_plot_for_snapshot <- function(p) {
  # Legacy writer disabled: restoring old miraprot_plot_bundle objects remains
  # supported by .restore_plot_from_snapshot(), but new exports must never
  # create bundle payloads because they duplicate plot/data trees in snapshots.
  p
}

#' Restore a ggplot from a bundle (inverse of .prepare_plot_for_snapshot)
#'
#' @param b A plot bundle produced by \code{.prepare_plot_for_snapshot}.
#'   Any other input is returned unchanged so callers can use this
#'   transparently on lists containing a mix of bundled and plain objects.
#' @return A ggplot object (or the unchanged input).
#' @keywords internal
.restore_plot_from_snapshot <- function(b) {
  if ((inherits(b, "miraprot_plot_bundle") || (is.list(b) && identical(b$kind, "ggplot"))) &&
      !isTRUE(getOption("miraprot.allow_legacy_plot_bundle_restore", TRUE))) {
    return(NULL)
  }
  if (inherits(b, "miraprot_plot_bundle") || (is.list(b) && identical(b$kind, "ggplot"))) {
    p <- b$stripped_gg
    if (!is.null(p) && !is.null(b$data)) {
      # Re-attach the original data frame to the ggplot so downstream code
      # (e.g. layer_data(), ggplot_build) sees a proper data source.
      tryCatch(p$data <- b$data, error = function(e) NULL)
    }
    return(p)
  }
  b
}

#' Apply a transformation to every plot in a named list
#'
#' Used by modules (volcano, heatmap) to wrap/unwrap their stored plot
#' collections on the save / restore boundary.
#'
#' @param x A named list of plots (or NULL).
#' @param fn Either \code{.prepare_plot_for_snapshot} or
#'   \code{.restore_plot_from_snapshot}.
#' @return A named list of the same length, or \code{NULL} if \code{x} is NULL.
#' @keywords internal
.map_plot_list <- function(x, fn) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(x)
  lapply(x, function(item) {
    tryCatch(fn(item), error = function(e) {
      debug_log(paste("plot bundle map failed:", e$message), 2)
      NULL
    })
  })
}

# ========================================
# ComplexHeatmap grob bundle helpers
# ========================================

#' Convert a ComplexHeatmap (S4) object to a serializable grob bundle
#'
#' ComplexHeatmap / Heatmap / HeatmapList objects carry function slots
#' (\code{cell_fun}, \code{cluster_rows}, colour mapping closures) whose
#' environments usually capture the enclosing module -- they therefore
#' fail to serialize, and the previous sanitizer dropped them wholesale
#' so heatmap users lost all plots on session save.
#'
#' This helper draws the heatmap once into a grid display-list grob via
#' \code{grid::grid.grabExpr()}; grobs are pure grid primitives and
#' serialize cleanly.  On restore, \code{grid::grid.draw()} reproduces
#' the exact rendered output.
#'
#' @param ht A ComplexHeatmap / HeatmapList object.
#' @return A list with class \code{"miraprot_ch_bundle"}, or \code{ht}
#'   unchanged if it is not a ComplexHeatmap-like object.
#' @keywords internal
.ch_to_grob_bundle <- function(ht) {
  if (is.null(ht)) return(NULL)
  is_ch <- isS4(ht) && (inherits(ht, "Heatmap") ||
                        inherits(ht, "HeatmapList") ||
                        inherits(ht, "AdditiveUnit"))
  if (!is_ch) return(ht)
  grob <- tryCatch(
    grid::grid.grabExpr({
      if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
        ComplexHeatmap::draw(ht)
      } else {
        grid::grid.draw(ht)
      }
    }, warn = 0L),
    error = function(e) {
      debug_log(paste(".ch_to_grob_bundle draw failed:", e$message), 1)
      NULL
    }
  )
  if (is.null(grob)) return(NULL)
  structure(
    list(kind = "ch_grob", grob = grob),
    class = "miraprot_ch_bundle"
  )
}

#' Restore a rendered grob from a CH bundle (callers draw with grid::grid.draw)
#' @keywords internal
.restore_ch_from_bundle <- function(b) {
  if (inherits(b, "miraprot_ch_bundle") || (is.list(b) && identical(b$kind, "ch_grob"))) {
    return(b$grob)
  }
  b
}

#' Apply .ch_to_grob_bundle (or inverse) across a list of heatmap slots
#' @keywords internal
.map_ch_list <- function(x, fn) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(fn(x))
  lapply(x, function(item) tryCatch(fn(item), error = function(e) NULL))
}



# Build canonical per-plot cache keys across modules.
# Schema: <module>::<logical_plot_id>::<variant>
.build_canonical_plot_cache_key <- function(module, logical_plot_id = "default", variant = "main") {
  module <- as.character(module %||% "module")[1]
  logical_plot_id <- as.character(logical_plot_id %||% "default")[1]
  variant <- as.character(variant %||% "main")[1]
  paste(module, logical_plot_id, variant, sep = "::")
}

# Deterministically serialize a variant-spec list for canonical key usage.
# Keep this as a lightweight, stable string without adding a new dependency.
.serialize_plot_variant_spec <- function(spec = NULL) {
  if (is.null(spec)) return("main")
  if (!is.list(spec)) return(as.character(spec)[1])
  nms <- names(spec) %||% character()
  if (length(spec) == 0L || length(nms) == 0L) return("main")
  ord <- order(nms)
  nms <- nms[ord]
  vals <- spec[ord]
  parts <- vapply(seq_along(vals), function(i) {
    val <- vals[[i]]
    val_txt <- paste(as.character(val %||% "none"), collapse = "~")
    paste0(nms[[i]], "=", val_txt)
  }, character(1L))
  paste(parts, collapse = "__")
}

# Scalar coercion helpers for cache reference contracts.
.session_scalar_chr <- function(x, default = "") {
  value <- tryCatch(
    suppressWarnings(as.character(x %||% default)),
    error = function(e) character()
  )
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) < 1L) {
    value <- tryCatch(
      suppressWarnings(as.character(default %||% "")),
      error = function(e) character()
    )
    value <- value[!is.na(value) & nzchar(value)]
  }
  if (length(value) < 1L) "" else value[[1L]]
}

.is_plot_cache_pair <- function(value) {
  is.list(value) &&
    inherits(value$data_mod, "data.frame") &&
    inherits(value$data_def, "data.frame")
}

.safe_cache_pool_get <- function(pool, cache_id) {
  tryCatch({
    if (!is.character(cache_id) || length(cache_id) != 1L ||
        is.na(cache_id) || !nzchar(cache_id) || !is.list(pool)) return(NULL)

    pool_ids <- names(pool) %||% character()
    if (length(pool_ids) == 0L) return(NULL)

    if (cache_id %in% pool_ids) {
      return(pool[[cache_id]])
    }

    cache_legacy <- sub("\\|fp:.*$", "", cache_id)
    if (!nzchar(cache_legacy)) return(NULL)

    legacy_idx <- which(vapply(pool_ids, function(id) {
      identical(sub("\\|fp:.*$", "", id), cache_legacy)
    }, logical(1L)))
    if (length(legacy_idx) < 1L) return(NULL)

    pool[[pool_ids[[legacy_idx[[1L]]]]]]
  }, error = function(e) NULL)
}

# Validate the per-plot index without conflating its logical keys with cache
# identifiers. Names identify plots; only scalar string values address the pool.
.validate_plot_cache_ref_by_title <- function(by_title, pool = list()) {
  if (is.null(by_title)) return(list(valid = TRUE, resolved = list(), invalid_keys = character()))
  if (!is.list(by_title) || length(by_title) == 0L) {
    return(list(valid = FALSE, resolved = list(), invalid_keys = "<map>"))
  }
  keys <- names(by_title)
  if (is.null(keys) || length(keys) != length(by_title)) keys <- rep("", length(by_title))
  resolved <- list()
  invalid <- character()
  for (i in seq_along(by_title)) {
    key <- keys[[i]]
    ref <- by_title[[i]]
    valid_key <- is.character(key) && length(key) == 1L && !is.na(key) && nzchar(key)
    valid_ref <- is.character(ref) && length(ref) == 1L && !is.na(ref) && nzchar(ref)
    pair <- if (valid_ref) .safe_cache_pool_get(pool, ref) else NULL
    if (!valid_key || !valid_ref || !.is_plot_cache_pair(pair)) {
      invalid <- c(invalid, if (valid_key) key else paste0("<entry:", i, ">"))
    } else {
      resolved[[key]] <- pair
    }
  }
  list(valid = length(invalid) == 0L, resolved = resolved, invalid_keys = unique(invalid))
}

.safe_cache_pool_put <- function(pool, cache_id, value) {
  tryCatch({
    if (!is.list(pool)) pool <- list()
    cache_id <- .session_scalar_chr(cache_id)
    if (!nzchar(cache_id)) {
      return(list(pool = pool, status = "invalid_cache_id"))
    }
    if (!.is_plot_cache_pair(value)) {
      return(list(pool = pool, status = "invalid_value"))
    }
    if (!is.null(.safe_cache_pool_get(pool, cache_id))) {
      return(list(pool = pool, status = "already_present"))
    }
    pool[[cache_id]] <- value
    list(pool = pool, status = "stored")
  }, error = function(e) {
    list(pool = if (is.list(pool)) pool else list(), status = "error")
  })
}

.session_scalar_int <- function(x, default = 0L) {
  value <- tryCatch(
    suppressWarnings(as.integer(x %||% default)),
    error = function(e) integer()
  )
  value <- value[!is.na(value)]
  if (length(value) < 1L) {
    value <- tryCatch(
      suppressWarnings(as.integer(default %||% 0L)),
      error = function(e) integer()
    )
    value <- value[!is.na(value)]
  }
  if (length(value) < 1L) 0L else value[[1L]]
}

# Normalize revision identifiers without consulting any enclosing environment.
# Revision IDs are scalar by contract; absent, non-numeric, and NA first values
# all represent the initial revision.
.plot_data_cache_revision_int <- function(x = NULL) {
  value <- tryCatch(
    suppressWarnings(as.integer(x %||% 0L)),
    error = function(e) integer()
  )
  if (length(value) < 1L || is.na(value[[1L]])) 0L else value[[1L]]
}

# Build a stable cache id for a data_mod/data_def pair using revision ids.
# New IDs append a low-cost fingerprint segment while keeping the legacy base
# token intact for backward-compatibility lookups.
.plot_data_cache_legacy_id <- function(data_mod_revision_id = NULL,
                                       data_def_revision_id = NULL,
                                       data_mod = NULL,
                                       data_def = NULL) {
  mod_rev <- .plot_data_cache_revision_int(data_mod_revision_id)
  def_rev <- .plot_data_cache_revision_int(data_def_revision_id)
  mod_dim <- if (inherits(data_mod, "data.frame")) paste0(nrow(data_mod), "x", ncol(data_mod)) else "0x0"
  def_dim <- if (inherits(data_def, "data.frame")) paste0(nrow(data_def), "x", ncol(data_def)) else "0x0"
  .session_scalar_chr(
    paste(mod_rev, def_rev, mod_dim, def_dim, sep = "|"),
    default = "0|0|0x0|0x0"
  )
}

.plot_data_cache_fingerprint <- function(data_mod = NULL,
                                         data_def = NULL,
                                         n_names = 6L,
                                         n_row = 3L,
                                         n_col = 3L) {
  collect_df_fp <- function(df) {
    if (!inherits(df, "data.frame")) return("none")
    cn <- colnames(df) %||% character()
    cn <- head(cn, n_names)
    cn_txt <- if (length(cn) > 0L) paste(cn, collapse = "~") else ""

    nr <- min(nrow(df), n_row)
    nc <- min(ncol(df), n_col)
    if (nr > 0L && nc > 0L) {
      cells <- tryCatch(unlist(df[seq_len(nr), seq_len(nc), drop = FALSE], use.names = FALSE),
                        error = function(e) character())
      cell_txt <- paste(utils::head(as.character(cells), n_row * n_col), collapse = "~")
    } else {
      cell_txt <- ""
    }

    raw <- paste0(cn_txt, "||", cell_txt)
    sprintf("%08x", as.integer(sum(utf8ToInt(raw)) %% 2147483647L))
  }

  paste(collect_df_fp(data_mod), collect_df_fp(data_def), sep = "-")
}

.build_plot_data_cache_id <- function(data_mod_revision_id = NULL,
                                      data_def_revision_id = NULL,
                                      data_mod = NULL,
                                      data_def = NULL) {
  legacy_id <- .plot_data_cache_legacy_id(
    data_mod_revision_id = data_mod_revision_id,
    data_def_revision_id = data_def_revision_id,
    data_mod = data_mod,
    data_def = data_def
  )
  fingerprint <- .session_scalar_chr(
    .plot_data_cache_fingerprint(data_mod = data_mod, data_def = data_def),
    default = "none-none"
  )
  cache_id <- .session_scalar_chr(paste0(legacy_id, "|fp:", fingerprint))
  if (!nzchar(cache_id)) .plot_data_cache_legacy_id() else cache_id
}

.plot_data_cache_entry_signature <- function(cache_id = NULL, entry = NULL) {
  if (!is.list(entry)) return(NULL)

  dm <- entry$data_mod
  dd <- entry$data_def
  legacy <- sub("\\|fp:.*$", "", as.character(cache_id %||% "")[1])
  fp <- if (inherits(dm, "data.frame") && inherits(dd, "data.frame")) {
    .plot_data_cache_fingerprint(data_mod = dm, data_def = dd)
  } else if (!is.null(entry$fingerprint) || !is.null(entry$plot_data_cache_fingerprint)) {
    as.character(entry$fingerprint %||% entry$plot_data_cache_fingerprint %||% NA_character_)[1]
  } else if (is.character(cache_id) && length(cache_id) == 1L &&
             grepl("\\|fp:", cache_id, fixed = TRUE)) {
    sub("^.*\\|fp:", "", cache_id)
  } else {
    NA_character_
  }
  rev_parts <- strsplit(legacy, "\\|", fixed = FALSE)[[1]] %||% character()
  rev_mod <- if (length(rev_parts) >= 1L) rev_parts[[1]] else NA_character_
  rev_def <- if (length(rev_parts) >= 2L) rev_parts[[2]] else NA_character_

  list(
    cache_id = as.character(cache_id %||% "")[1],
    data_mod_revision_id = suppressWarnings(as.integer(entry$data_mod_revision_id %||% rev_mod)),
    data_def_revision_id = suppressWarnings(as.integer(entry$data_def_revision_id %||% rev_def)),
    data_mod_nrow = if (inherits(dm, "data.frame")) nrow(dm) else suppressWarnings(as.integer(entry$data_mod_nrow %||% NA_integer_)),
    data_mod_ncol = if (inherits(dm, "data.frame")) ncol(dm) else suppressWarnings(as.integer(entry$data_mod_ncol %||% NA_integer_)),
    data_def_nrow = if (inherits(dd, "data.frame")) nrow(dd) else suppressWarnings(as.integer(entry$data_def_nrow %||% NA_integer_)),
    data_def_ncol = if (inherits(dd, "data.frame")) ncol(dd) else suppressWarnings(as.integer(entry$data_def_ncol %||% NA_integer_)),
    fingerprint = as.character(fp %||% NA_character_)[1]
  )
}

.is_canonical_active_dataset_cache_alias <- function(entry) {
  is.list(entry) && identical(entry$kind, "canonical_active_dataset_ref")
}

# Resolve one pool entry without allowing the lightweight active-dataset alias
# to become a second, independent source of truth. Ordinary entries retain the
# historical lookup behaviour; aliases are usable only when their complete
# identity agrees with the authoritative Data Wizard pair supplied by the
# caller.
.resolve_plot_data_cache_pool_entry <- function(plot_data_cache_pool = NULL,
                                                cache_id = NULL,
                                                data_mod = NULL,
                                                data_def = NULL,
                                                data_mod_revision_id = NULL,
                                                data_def_revision_id = NULL) {
  entry <- .safe_cache_pool_get(plot_data_cache_pool, cache_id)
  if (is.null(entry) || !.is_canonical_active_dataset_cache_alias(entry)) {
    return(entry)
  }

  cache_id <- if (is.character(cache_id) && length(cache_id) == 1L &&
                    !is.na(cache_id)) cache_id else ""
  if (!nzchar(cache_id) || !inherits(data_mod, "data.frame") ||
      !inherits(data_def, "data.frame")) return(NULL)

  authoritative_id <- .build_plot_data_cache_id(
    data_mod_revision_id, data_def_revision_id, data_mod, data_def
  )
  expected <- list(
    kind = "canonical_active_dataset_ref",
    cache_id = authoritative_id,
    data_mod_revision_id = .plot_data_cache_revision_int(data_mod_revision_id),
    data_def_revision_id = .plot_data_cache_revision_int(data_def_revision_id),
    data_mod_nrow = nrow(data_mod),
    data_mod_ncol = ncol(data_mod),
    data_def_nrow = nrow(data_def),
    data_def_ncol = ncol(data_def),
    fingerprint = .plot_data_cache_fingerprint(data_mod, data_def)
  )
  required <- names(expected)
  if (!identical(cache_id, authoritative_id) ||
      length(names(entry)) != length(required) ||
      !setequal(names(entry), required) ||
      !all(vapply(required, function(field) {
        identical(entry[[field]], expected[[field]])
      }, logical(1L)))) return(NULL)

  list(data_mod = data_mod, data_def = data_def)
}

.active_dataset_cache_entry_matches <- function(cache_id,
                                                entry,
                                                active_dataset_id,
                                                rv_snapshot) {
  if (!is.character(cache_id) || length(cache_id) != 1L ||
      !is.character(active_dataset_id) || length(active_dataset_id) != 1L ||
      !nzchar(cache_id) || !nzchar(active_dataset_id) ||
      !identical(cache_id, active_dataset_id)) {
    return(FALSE)
  }
  if (!inherits(rv_snapshot$data_mod, "data.frame") ||
      !inherits(rv_snapshot$data_def, "data.frame")) {
    return(FALSE)
  }
  if (!is.list(entry) ||
      !inherits(entry$data_mod, "data.frame") ||
      !inherits(entry$data_def, "data.frame")) {
    return(FALSE)
  }

  expected <- .plot_data_cache_entry_signature(active_dataset_id, list(
    data_mod = rv_snapshot$data_mod,
    data_def = rv_snapshot$data_def,
    data_mod_revision_id = rv_snapshot$data_mod_revision_id,
    data_def_revision_id = rv_snapshot$data_def_revision_id
  ))
  actual <- .plot_data_cache_entry_signature(cache_id, entry)
  identical(actual$data_mod_revision_id, expected$data_mod_revision_id) &&
    identical(actual$data_def_revision_id, expected$data_def_revision_id) &&
    identical(actual$data_mod_nrow, expected$data_mod_nrow) &&
    identical(actual$data_mod_ncol, expected$data_mod_ncol) &&
    identical(actual$data_def_nrow, expected$data_def_nrow) &&
    identical(actual$data_def_ncol, expected$data_def_ncol) &&
    identical(actual$fingerprint, expected$fingerprint)
}

.alias_active_dataset_cache_entries <- function(plot_data_cache_pool = NULL,
                                                active_dataset_id = NA_character_,
                                                rv_snapshot = NULL) {
  pool <- if (is.list(plot_data_cache_pool)) plot_data_cache_pool else list()
  if (length(pool) == 0L || !is.list(rv_snapshot)) return(pool)
  ids <- names(pool) %||% character()
  if (length(ids) == 0L) return(pool)

  for (cache_id in ids) {
    entry <- pool[[cache_id]]
    if (!.active_dataset_cache_entry_matches(cache_id, entry, active_dataset_id, rv_snapshot)) next
    sig <- .plot_data_cache_entry_signature(cache_id, entry)
    pool[[cache_id]] <- list(
      kind = "canonical_active_dataset_ref",
      cache_id = sig$cache_id,
      data_mod_revision_id = sig$data_mod_revision_id,
      data_def_revision_id = sig$data_def_revision_id,
      data_mod_nrow = sig$data_mod_nrow,
      data_mod_ncol = sig$data_mod_ncol,
      data_def_nrow = sig$data_def_nrow,
      data_def_ncol = sig$data_def_ncol,
      fingerprint = sig$fingerprint
    )
  }
  pool
}

.materialize_active_dataset_cache_aliases <- function(plot_data_cache_pool = NULL,
                                                      rv_snapshot = NULL) {
  pool <- if (is.list(plot_data_cache_pool)) plot_data_cache_pool else list()
  if (length(pool) == 0L || !is.list(rv_snapshot) ||
      !inherits(rv_snapshot$data_mod, "data.frame") ||
      !inherits(rv_snapshot$data_def, "data.frame")) {
    return(pool)
  }
  ids <- names(pool) %||% character()
  if (length(ids) == 0L) return(pool)

  for (cache_id in ids) {
    entry <- pool[[cache_id]]
    if (!.is_canonical_active_dataset_cache_alias(entry)) next
    resolved <- .resolve_plot_data_cache_pool_entry(
      pool, cache_id, rv_snapshot$data_mod, rv_snapshot$data_def,
      rv_snapshot$data_mod_revision_id, rv_snapshot$data_def_revision_id
    )
    if (.is_plot_cache_pair(resolved)) pool[[cache_id]] <- resolved
  }
  pool
}

.plot_data_cache_ref_contract <- function(data_mod_revision_id = NULL,
                                          data_def_revision_id = NULL,
                                          data_mod = NULL,
                                          data_def = NULL,
                                          plot_data_cache_ref = NULL) {
  mod_rev <- .plot_data_cache_revision_int(data_mod_revision_id)
  def_rev <- .plot_data_cache_revision_int(data_def_revision_id)
  ref <- as.character(plot_data_cache_ref %||% "")
  ref <- ref[!is.na(ref) & nzchar(ref)]
  ref <- if (length(ref) > 0L) ref[[1L]] else ""
  if (!nzchar(ref)) {
    ref <- .build_plot_data_cache_id(
      data_mod_revision_id = mod_rev,
      data_def_revision_id = def_rev,
      data_mod = data_mod,
      data_def = data_def
    )
  }
  list(
    plot_data_cache_ref = .session_scalar_chr(ref, default = .build_plot_data_cache_id(
      data_mod_revision_id = mod_rev,
      data_def_revision_id = def_rev,
      data_mod = data_mod,
      data_def = data_def
    )),
    data_mod_revision_id = mod_rev,
    data_def_revision_id = def_rev,
    plot_data_cache_fingerprint = .session_scalar_chr(
      .plot_data_cache_fingerprint(
        data_mod = data_mod,
        data_def = data_def
      ),
      default = "none-none"
    )
  )
}

.cache_ref_contract_compatible <- function(contract, data_mod = NULL, data_def = NULL) {
  if (!is.list(contract)) return(FALSE)
  if (!inherits(data_mod, "data.frame") || !inherits(data_def, "data.frame")) return(FALSE)
  expected <- .build_plot_data_cache_id(
    data_mod_revision_id = contract$data_mod_revision_id,
    data_def_revision_id = contract$data_def_revision_id,
    data_mod = data_mod,
    data_def = data_def
  )
  ref <- .session_scalar_chr(contract$plot_data_cache_ref)
  fp <- .session_scalar_chr(contract$plot_data_cache_fingerprint)
  ref_legacy <- sub("\\|fp:.*$", "", ref)
  expected_legacy <- sub("\\|fp:.*$", "", expected)
  expected_fp <- .plot_data_cache_fingerprint(data_mod = data_mod, data_def = data_def)
  identical(ref, expected) ||
    (nzchar(ref_legacy) && identical(ref_legacy, expected_legacy) &&
       (!nzchar(fp) || identical(fp, expected_fp)))
}

.module_cache_ref_contract <- function(module_state) {
  if (!is.list(module_state)) return(list())
  list(
    plot_data_cache_ref = module_state$plot_data_cache_ref,
    data_mod_revision_id = module_state$data_mod_revision_id,
    data_def_revision_id = module_state$data_def_revision_id,
    plot_data_cache_fingerprint = module_state$plot_data_cache_fingerprint
  )
}

.normalize_module_cache_ref_contract <- function(module_state,
                                                 data_mod = NULL,
                                                 data_def = NULL,
                                                 data_mod_revision_id = NULL,
                                                 data_def_revision_id = NULL,
                                                 drop_embedded_pair = TRUE) {
  if (!is.list(module_state)) return(module_state)

  pair <- module_state$restore_plot_data_cache
  if (!(is.list(pair) && inherits(pair$data_mod, "data.frame") && inherits(pair$data_def, "data.frame"))) {
    pair <- module_state$plot_data_cache_payload
  }
  if (is.list(pair) &&
      inherits(pair$data_mod, "data.frame") &&
      inherits(pair$data_def, "data.frame")) {
    data_mod <- pair$data_mod
    data_def <- pair$data_def
  }
  if (!inherits(data_mod, "data.frame") || !inherits(data_def, "data.frame")) {
    return(module_state)
  }

  contract <- .plot_data_cache_ref_contract(
    data_mod_revision_id = module_state$data_mod_revision_id %||% data_mod_revision_id,
    data_def_revision_id = module_state$data_def_revision_id %||% data_def_revision_id,
    data_mod = data_mod,
    data_def = data_def,
    plot_data_cache_ref = module_state$plot_data_cache_ref
  )
  module_state[names(contract)] <- contract
  if (isTRUE(drop_embedded_pair)) {
    module_state$restore_plot_data_cache <- NULL
    module_state$plot_data_cache_payload <- NULL
    module_state$data_mod <- NULL
    module_state$data_def <- NULL
  }
  module_state
}

.attach_plot_data_cache_ref <- function(module_state, rv_snapshot) {
  if (!is.list(module_state)) return(module_state)
  data_mod <- rv_snapshot$data_mod
  data_def <- rv_snapshot$data_def
  if (!inherits(data_mod, "data.frame") || !inherits(data_def, "data.frame")) return(module_state)

  contract <- .plot_data_cache_ref_contract(
    data_mod_revision_id = rv_snapshot$data_mod_revision_id,
    data_def_revision_id = rv_snapshot$data_def_revision_id,
    data_mod = data_mod,
    data_def = data_def
  )
  module_state[names(contract)] <- contract
  module_state
}


# Resolve a module-local data pair for restore-aware rebuild paths.
#
# Contract:
#   1) If a restored matrix bundle is present (restore_plot_data_cache or
#      equivalent list containing data_mod/data_def), use that pair.
#   2) Otherwise fall back to live rv$data_mod / rv$data_def.
#
# Returns list(data_mod=..., data_def=..., source="restore_cache"|"live_rv").
resolve_data_pair_for_restore <- function(rv,
                                          restore_bundle = NULL,
                                          debug_log = NULL,
                                          module_label = "module",
                                          module_state = NULL) {
  bundle <- restore_bundle
  if (is.null(bundle) || !is.list(bundle)) {
    bundle <- tryCatch(rv$restore_plot_data_cache, error = function(e) NULL)
  }

  if (is.list(bundle) &&
      inherits(bundle$data_mod, "data.frame") &&
      inherits(bundle$data_def, "data.frame")) {
    if (is.function(debug_log)) {
      debug_log(sprintf("[%s] data_pair_for_restore resolved from restore_plot_data_cache", module_label), 2)
    }
    return(list(
      data_mod = bundle$data_mod,
      data_def = bundle$data_def,
      source = "restore_cache"
    ))
  }

  live_data_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
  live_data_def <- tryCatch(rv$data_def, error = function(e) NULL)
  contract <- .module_cache_ref_contract(module_state)
  has_contract <- is.character(contract$plot_data_cache_ref) &&
    length(contract$plot_data_cache_ref) == 1L &&
    nzchar(contract$plot_data_cache_ref)
  if (isTRUE(has_contract) &&
      !.cache_ref_contract_compatible(contract, live_data_mod, live_data_def)) {
    if (is.function(debug_log)) {
      debug_log(sprintf("[%s] data_pair_for_restore degraded: cache ref unresolved and live data revision/fingerprint incompatible", module_label), 1)
    }
    return(list(
      data_mod = NULL,
      data_def = NULL,
      source = "degraded",
      degraded = TRUE
    ))
  }
  if (is.function(debug_log)) {
    debug_log(sprintf("[%s] data_pair_for_restore fell back to live rv$data_mod/rv$data_def", module_label), 2)
  }
  list(data_mod = live_data_mod, data_def = live_data_def, source = "live_rv")
}

.resolve_plot_data_cache_for_module <- function(module_state, plot_data_cache_pool = list()) {
  if (!is.list(module_state)) return(module_state)

  # 1) Embedded payloads are authoritative for legacy/current snapshots that
  # carry a valid restore-ready data pair. Resolve them before consulting the
  # shared pool so restore does not accidentally bind to live rv$data_mod.
  embedded_pair <- module_state$restore_plot_data_cache
  if (!.is_plot_cache_pair(embedded_pair)) {
    embedded_pair <- module_state$plot_data_cache_payload
  }
  if (.is_plot_cache_pair(embedded_pair)) {
    module_state$restore_plot_data_cache <- embedded_pair
    module_state$restore_cache_degraded <- FALSE
    module_state$restore_cache_resolved <- TRUE
    module_state$restore_cache_resolution_mode <- "embedded_payload"
    return(module_state)
  }

  # This field is a restore product, not a persisted source of cache identity.
  # Rebuild it exclusively from the validated title map and shared pool.
  module_state$restore_plot_data_cache_by_title <- NULL

  if (!is.list(plot_data_cache_pool)) plot_data_cache_pool <- list()

  .lookup_cache_entry <- function(cache_id) {
    .safe_cache_pool_get(plot_data_cache_pool, cache_id)
  }

  # 2) Module-level: exact-id match, then legacy-id match.
  ref <- module_state$plot_data_cache_ref
  cached <- .lookup_cache_entry(ref)
  if (.is_plot_cache_pair(cached)) {
    module_state$restore_cache_resolution_mode <- "module_ref"
  }

  # 3) Per-plot mapping. Names are logical plot identifiers (or canonical
  # title keys); values alone are data-cache ids. Legacy key-as-id repair is
  # restricted to the explicitly marked schema-upgrade path.
  by_title <- module_state$plot_cache_ref_by_title
  if (is.list(by_title) && length(by_title) > 0L) {
    title_validation <- .validate_plot_cache_ref_by_title(by_title, plot_data_cache_pool)
    module_state$plot_cache_ref_by_title_valid <- title_validation$valid
    module_state$plot_cache_ref_by_title_invalid_keys <- title_validation$invalid_keys
    if (isTRUE(title_validation$valid) && length(title_validation$resolved) > 0L) {
      module_state$restore_plot_data_cache_by_title <- title_validation$resolved
      if (!.is_plot_cache_pair(cached)) {
        module_state$restore_cache_resolution_mode <- "by_title"
      }
    }
  }

  # 5) Singleton pool fallback for current-schema shared-cache modules.
  # Some historical saves wrote module refs that do not match the top-level pool
  # key even though the pool contains exactly one canonical data/metadata pair for
  # the session. In that shape, prefer the sole pool entry over falling back to
  # incompatible live data; this keeps restore cache-driven without reintroducing
  # embedded ggplot/plotly snapshots.
  if (!.is_plot_cache_pair(cached) &&
      identical(module_state$restore_cache_dependency, "shared_plot_data_cache_pool")) {
    pool_ids <- names(plot_data_cache_pool) %||% character()
    if (length(pool_ids) == 1L) {
      sole <- plot_data_cache_pool[[pool_ids[[1L]]]]
      if (.is_plot_cache_pair(sole)) {
        cached <- sole
        pool_id <- pool_ids[[1L]]
        module_state$plot_data_cache_ref <- pool_id
        if (is.list(by_title) && length(by_title) > 0L) {
          title_refs <- vector("list", length(by_title))
          names(title_refs) <- names(by_title)
          for (i in seq_along(title_refs)) {
            title_refs[[i]] <- pool_id
          }
          module_state$plot_cache_ref_by_title <- title_refs
        }
        module_state$restore_cache_resolution_mode <- "singleton_pool_fallback"
      }
    }
  }

  # 6) Module-level fallback: keep/replace restore_plot_data_cache when available.
  if (.is_plot_cache_pair(cached)) {
    module_state$restore_plot_data_cache <- cached
    module_state$restore_cache_degraded <- FALSE
    module_state$restore_cache_resolved <- TRUE
  } else if (length(module_state$restore_plot_data_cache_by_title %||% list()) > 0L) {
    module_state$restore_cache_degraded <- FALSE
    module_state$restore_cache_resolved <- TRUE
  }
  module_state
}

# Build a lightweight index describing the plot-data cache pool.
# Each entry maps cache id -> minimal metadata (dims/revisions/fingerprint).
.build_plot_data_cache_index <- function(plot_data_cache_pool = NULL) {
  pool <- plot_data_cache_pool
  if (!is.list(pool) || length(pool) == 0L) return(list())

  ids <- names(pool) %||% character()
  if (length(ids) == 0L) return(list())

  idx <- list()
  for (i in seq_along(ids)) {
    cache_id <- ids[[i]]
    entry <- pool[[cache_id]]
    if (!is.list(entry)) next
    sig <- .plot_data_cache_entry_signature(cache_id, entry)
    idx[[cache_id]] <- list(
      cache_id = cache_id,
      legacy_id = sub("\\|fp:.*$", "", cache_id),
      data_mod_revision_id = sig$data_mod_revision_id,
      data_def_revision_id = sig$data_def_revision_id,
      data_mod_nrow = sig$data_mod_nrow,
      data_mod_ncol = sig$data_mod_ncol,
      data_def_nrow = sig$data_def_nrow,
      data_def_ncol = sig$data_def_ncol,
      fingerprint = sig$fingerprint
    )
  }
  idx
}


.validate_cache_ref_integrity <- function(module_snapshots = NULL, plot_data_cache_pool = NULL) {
  snaps <- module_snapshots
  pool <- plot_data_cache_pool

  if (!is.list(snaps) || length(snaps) == 0L) {
    return(list(valid = TRUE, missing_refs = character(), total_refs = 0L, checked_refs = 0L))
  }
  if (!is.list(pool)) pool <- list()

  required_ids <- .compute_required_cache_ids(snaps)
  if (length(required_ids) == 0L) {
    return(list(valid = TRUE, missing_refs = character(), total_refs = 0L, checked_refs = 0L))
  }

  pool_ids <- names(pool) %||% character()
  pool_legacy <- vapply(pool_ids, function(id) sub("\\|fp:.*$", "", id), character(1L))
  required_legacy <- vapply(required_ids, function(id) sub("\\|fp:.*$", "", id), character(1L))

  missing <- required_ids[!(required_ids %in% pool_ids | required_legacy %in% pool_legacy)]
  list(
    valid = length(missing) == 0L,
    missing_refs = unique(as.character(missing)),
    total_refs = length(required_ids),
    checked_refs = length(unique(required_legacy))
  )
}

.gc_plot_data_cache_pool <- function(module_snapshots = NULL,
                                     plot_data_cache_pool = NULL,
                                     plot_data_cache_index = NULL,
                                     active_dataset_id = NA_character_) {
  mode <- tolower(as.character(getOption("miraprot.session_gc_mode", "hard"))[1])
  if (!mode %in% c("off", "dry_run", "hard")) mode <- "dry_run"

  orig_pool <- if (is.list(plot_data_cache_pool)) plot_data_cache_pool else list()
  orig_index <- if (is.list(plot_data_cache_index)) plot_data_cache_index else list()
  pool <- orig_pool
  index <- orig_index

  pool_ids <- names(orig_pool) %||% character()
  required_ids <- .compute_required_cache_ids(module_snapshots)
  required_ref_diagnostics <- attr(required_ids, "cache_liveness_diagnostics", exact = TRUE) %||% list()
  # Cache liveness is authoritative only in module snapshots.  In particular,
  # being the active dataset does not by itself make a pool entry live.
  active_dataset_kept <- intersect(intersect(pool_ids, active_dataset_id), required_ids)
  stale_entries <- setdiff(pool_ids, required_ids)
  stale_candidates <- stale_entries
  unknown_kept <- character()
  unresolved_refs <- setdiff(required_ids, pool_ids)
  unresolved_refs_count <- length(unresolved_refs)
  required_found_count <- length(intersect(required_ids, pool_ids))

  # Unknown metadata must be preserved; only delete candidates with known metadata.
  legacy_candidates <- stale_candidates[!grepl("|fp:", stale_candidates, fixed = TRUE)]
  if (length(legacy_candidates) > 0L) {
    unknown_kept <- union(unknown_kept, legacy_candidates)
  }
  stale_known <- setdiff(stale_candidates, legacy_candidates)
  if (length(stale_known) > 0L) {
    stale_with_unknown_index <- stale_known[vapply(stale_known, function(id) {
      meta <- orig_index[[id]]
      fingerprint <- if (is.list(meta)) meta$fingerprint else NULL
      !is.list(meta) || length(meta) == 0L ||
        !is.character(fingerprint) || length(fingerprint) != 1L ||
        is.na(fingerprint) || !nzchar(fingerprint)
    }, logical(1L))]
    if (length(stale_with_unknown_index) > 0L) {
      unknown_kept <- union(unknown_kept, stale_with_unknown_index)
    }
  }
  stale_candidates <- setdiff(stale_candidates, unknown_kept)

  integrity <- .validate_cache_ref_integrity(module_snapshots = module_snapshots, plot_data_cache_pool = orig_pool)
  confidence <- if (length(stale_candidates) == 0L || (integrity$valid && unresolved_refs_count == 0L)) "high" else "low"

  removed_ids <- character()
  rollback_reason <- NULL
  removed_bytes_estimate <- 0

  if (identical(mode, "hard") && length(stale_candidates) > 0L) {
    if (identical(confidence, "high") && isTRUE(integrity$valid) && unresolved_refs_count == 0L) {
      removed_ids <- stale_candidates
      removed_bytes_estimate <- sum(vapply(removed_ids, function(id) {
        as.numeric(utils::object.size(orig_pool[[id]] %||% NULL))
      }, numeric(1L)))
      pool[removed_ids] <- NULL
      index[removed_ids] <- NULL
    }
  }

  if (identical(mode, "hard")) {
    hard_allowed <- identical(confidence, "high") && isTRUE(integrity$valid) && unresolved_refs_count == 0L
    if (!hard_allowed && length(stale_candidates) > 0L) {
      pool <- orig_pool
      index <- orig_index
      removed_ids <- character()
      rollback_reason <- if (unresolved_refs_count > 0L) {
        "unresolved_refs_present"
      } else if (!isTRUE(integrity$valid)) {
        "integrity_validation_failed"
      } else {
        "insufficient_confidence"
      }
      debug_log(paste("Session save GC rollback:", rollback_reason), 1)
    }
  }

  list(
    pool = pool,
    index = index,
    report = list(
      mode = mode,
      confidence = confidence,
      integrity_valid = isTRUE(integrity$valid),
      integrity_missing_refs = integrity$missing_refs %||% character(),
      unresolved_refs = unresolved_refs %||% character(),
      unresolved_refs_count = as.integer(unresolved_refs_count),
      rollback_reason = rollback_reason,
      removed_ids = removed_ids,
      total_entries = length(pool_ids),
      required_entries = length(required_ids),
      required_cache_references = required_ref_diagnostics,
      required_refs_found = as.integer(required_found_count),
      active_dataset_kept_entries = length(active_dataset_kept),
      unused_ids = stale_entries,
      unused_entries = length(stale_entries),
      stale_candidate_entries = length(stale_candidates),
      entries_deleted = length(removed_ids),
      stale_removed_entries = length(removed_ids),
      unknown_kept_entries = length(unknown_kept),
      removed_bytes_estimate = removed_bytes_estimate
    )
  )
}

# Collect cache ids referenced by module snapshots (required for restore liveness).
.compute_required_cache_ids <- function(module_snapshots = NULL) {
  snaps <- module_snapshots
  if (!is.list(snaps) || length(snaps) == 0L) return(character())

  required <- character()
  diagnostics <- list()
  record_ref <- function(cache_id, module_id, plot_title, reference_field) {
    required <<- c(required, cache_id)
    diagnostics[[cache_id]] <<- c(
      diagnostics[[cache_id]] %||% list(),
      list(list(
        module_id = module_id,
        plot_title = plot_title,
        reference_field = reference_field
      ))
    )
  }
  for (mid in names(snaps)) {
    mstate <- snaps[[mid]]$module_state
    if (!is.list(mstate)) next

    # Keep cache-reference collection aligned with the single module-level
    # restore-liveness predicate used by save/restore orchestration.
    if (!isTRUE(.session_module_snapshot_needs_plot_pool(mid, mstate))) next

    # A scalar reference requires module-level reconstruction intent.  Check
    # that intent without title entries, since an explicitly persisted title
    # must not accidentally revive an unrelated stale scalar reference.
    scalar_state <- mstate
    scalar_state$plot_cache_ref_by_title <- NULL
    ref <- .session_scalar_chr(mstate$plot_data_cache_ref)
    if (nzchar(ref) &&
        isTRUE(.session_module_snapshot_needs_plot_pool(mid, scalar_state))) {
      record_ref(ref, mid, "<default>", "plot_data_cache_ref")
    }

    by_title <- mstate$plot_cache_ref_by_title
    if (is.list(by_title) && length(by_title) > 0L) {
      titles <- names(by_title)
      if (is.null(titles)) titles <- rep("", length(by_title))
      for (i in seq_along(by_title)) {
        title <- .session_scalar_chr(titles[[i]])
        if (!nzchar(title)) title <- paste0("<plot-", i, ">")
        title_ref <- .session_scalar_chr(by_title[[i]])
        if (nzchar(title_ref)) {
          record_ref(title_ref, mid, title, "plot_cache_ref_by_title")
        }
      }
    }
  }
  required <- unique(as.character(required))
  if (length(required) > 0L) {
    attr(required, "cache_liveness_diagnostics") <- diagnostics[required]
  }
  required
}
#' Set up session save/restore server logic
#'
#' Registers the download handler for session snapshots and the observer
#' for restoring uploaded session files.
#'
#' @param input,output,session Standard Shiny server parameters.
#' @param rv The shared \code{reactiveValues} object.
#' @param session_registry The session registry created by
#'   \code{create_session_registry()}.

# ========================================
# Progress Helper: Human-readable Module Names
# ========================================

#' Map a module ID to a user-friendly display name for progress messages.
#' @param mod_id Character string (e.g. "datawizard").
#' @return Display name (e.g. "Data Wizard").
#' @keywords internal
.module_display_name <- function(mod_id) {
  nms <- c(
    datawizard  = "Data Wizard",
    go          = "GO",
    gsea        = "GSEA",
    volcano     = "Volcano",
    pca         = "Dimensionality Reduction",
    heatmap     = "Heatmap",
    dotplot     = "Dot Plot",
    string      = "STRING",
    venn        = "Venn",
    abundances  = "Abundances",
    sampleids   = "Sample IDs"
  )
  unname(nms[mod_id]) %||% mod_id
}

# ========================================
# Internal Helpers for Safe Reactive Access
# ========================================

#' Safely read a reactiveVal (returns NULL on error)
#' @param rv_fn A reactiveVal function.
#' @return The current value or NULL.
.safe_rv_read <- function(rv_fn) {
  if (is.null(rv_fn) || !is.function(rv_fn)) return(NULL)
  tryCatch(isolate(rv_fn()), error = function(e) NULL)
}

#' Safely read a field from a reactiveValues container (returns NULL on error)
#' @param rv A reactiveValues container (or NULL).
#' @param name Character field name.
#' @return Current value of \code{rv[[name]]} or NULL.
.safe_rv_read_from <- function(rv, name) {
  if (is.null(rv) || !is.character(name) || length(name) != 1L) return(NULL)
  tryCatch(isolate(rv[[name]]), error = function(e) NULL)
}

#' Safely write to a reactiveVal (no-op on error or if value is not a function)
#' @param rv_fn A reactiveVal function.
#' @param value The value to set.
.safe_rv_write <- function(rv_fn, value) {
  if (is.null(rv_fn) || !is.function(rv_fn)) return(invisible(NULL))
  tryCatch(rv_fn(value), error = function(e) {
    debug_log(paste("safe_rv_write failed:", e$message), 2)
  })
  invisible(NULL)
}

#' Safely call a zero-argument function (returns NULL on error)
#' @param fn A function.
#' @return The return value or NULL.
.safe_fn_call <- function(fn) {
  if (is.null(fn) || !is.function(fn)) return(NULL)
  tryCatch(isolate(fn()), error = function(e) NULL)
}

#' Safely call a single-argument function (no-op if fn or arg is NULL)
#' @param fn A function accepting one argument.
#' @param arg The argument value.
.safe_fn_call_arg <- function(fn, arg) {
  if (is.null(fn) || !is.function(fn)) return(invisible(NULL))
  if (is.null(arg)) return(invisible(NULL))
  tryCatch(fn(arg), error = function(e) {
    debug_log(paste("safe_fn_call_arg failed:", e$message), 2)
    NULL
  })
}

#' Compact structure summary for debug logs
#' @param x Object to summarize.
#' @return Single-line character summary.
.describe_snapshot_object <- function(x) {
  if (is.null(x)) return("NULL")
  cls <- paste(class(x), collapse = "/")
  if (is.data.frame(x)) {
    return(sprintf("data.frame[%d x %d] cols={%s}",
                   nrow(x), ncol(x), paste(names(x), collapse = ",")))
  }
  if (is.list(x)) {
    return(sprintf("list(len=%d, names={%s})",
                   length(x), paste(names(x) %||% character(), collapse = ",")))
  }
  if (is.atomic(x)) {
    preview <- paste(utils::head(as.character(x), 3), collapse = ",")
    return(sprintf("%s(len=%d, preview=%s)", cls, length(x), preview))
  }
  sprintf("%s(len=%d)", cls, length(x))
}

#' Recursively strip closures/functions from a nested structure
#'
#' R serialization preserves function objects (closures) verbatim. Any closure
#' that rides along inside a UI
#' config list will later reach \code{updateSelectizeInput(selected = v)}
#' on restore and trigger \dQuote{cannot coerce type 'closure' to vector
#' of type 'character'}. This helper walks a (possibly nested) list and
#' replaces any \code{is.function(v)} leaf with \code{NULL}, leaving
#' atomic leaves intact. It is intentionally narrow: data.frames, factors,
#' Date/POSIXt, and S4 objects are returned unchanged — only plain lists
#' are descended into.
#'
#' @param x Any R object.
#' @param path Character vector of ancestor names, for logging.
#' @return \code{x} with function leaves replaced by \code{NULL}.
.increment_session_save_counter <- function(counter_name, amount = 1L) {
  counters <- getOption("miraprot.session_save_restore_debug_counters", NULL)
  if (is.environment(counters)) {
    current <- get0(counter_name, envir = counters, inherits = FALSE, ifnotfound = 0L)
    assign(counter_name, current + amount, envir = counters)
  }
  invisible(NULL)
}

.strip_closures <- function(x, path = character(0L)) {
  path_label <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
  if (is.function(x)) {
    .increment_session_save_counter("stripped_closure_count")
    debug_log(paste0("Stripped closure from UI config at: ", path_label), 1)
    return(NULL)
  }
  # Environments cannot be serialized cleanly either — a live environment
  # reference (e.g. reactive scope, module state container) reaches the
  # sanitizer's base-serialization probe unchanged and triggers a full-subtree
  # failure. Treat environments the same way we treat closures: drop them
  # with a breadcrumb and let the outer code fall back to NULL for the
  # enclosing field.
  if (is.environment(x)) {
    .increment_session_save_counter("stripped_closure_count")
    debug_log(paste0("Stripped environment from UI config at: ", path_label), 1)
    return(NULL)
  }
  if (!is.list(x) || is.data.frame(x) || isS4(x)) return(x)

  n <- length(x)
  nms <- names(x)
  # Defensive normalization for malformed list objects where names(x)
  # exists but has a different length than the list payload.
  if (!is.null(nms) && length(nms) != n) {
    debug_log(paste0(
      "strip_closures: normalized malformed names at ", path_label,
      " (names=", length(nms), ", values=", n, ")"
    ), 1)
    nms_fixed <- rep.int("", n)
    if (length(nms) > 0L) {
      copy_n <- min(length(nms), n)
      nms_fixed[seq_len(copy_n)] <- nms[seq_len(copy_n)]
    }
    names(x) <- nms_fixed
    nms <- nms_fixed
  }

  for (i in seq_len(n)) {
    child_name <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else paste0("[[", i, "]]")
    child <- .strip_closures(x[[i]], c(path, child_name))
    # IMPORTANT: preserve list length; x[[i]] <- NULL deletes the element and
    # can corrupt names() length, causing "'names' attribute ... must match"
    # during subsequent recursion/sanitization.
    x[i] <- list(child)
  }
  x
}

#' Strip heavy environments from a ggplot object
#'
#' ggplot objects keep references to \code{plot_env} and to the
#' evaluation environments of quosures in their mappings. Those
#' environment chains are the main cause of
#' \dQuote{evaluation nested too deeply} errors during \code{saveRDS()}.
#' This helper replaces those environments with \code{emptyenv()} while
#' leaving data, scales, layers, and labels intact so the plot can still
#' be rendered after a round-trip through disk.
#'
#' Accepts any input and returns it unchanged when it is not a ggplot.
#'
#' @param p Possibly a ggplot object.
#' @return The same object with environment references neutralized.
.strip_ggplot_env <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  clean_quosures <- function(q) {
    if (inherits(q, "quosures")) {
      q <- lapply(q, function(qi) {
        if (inherits(qi, "quosure")) attr(qi, ".Environment") <- emptyenv()
        qi
      })
      class(q) <- "quosures"
    }
    q
  }
  # Re-parent a ggproto environment to emptyenv() so its super$ chain
  # cannot drag the module closure (and the ggplot2 namespace) into the
  # serialize() walk.  The inherited fields remain visible via the ggproto
  # class dispatch during rendering, but the environment itself now has no
  # recursive env-refs.  Guarded because some user layers are not true
  # ggproto environments.
  neutralize_env <- function(e) {
    if (is.environment(e)) {
      tryCatch(parent.env(e) <- emptyenv(), error = function(err) NULL)
    }
    e
  }
  p$plot_env <- emptyenv()
  p$mapping <- clean_quosures(p$mapping)
  if (is.list(p$layers)) {
    p$layers <- lapply(p$layers, function(layer) {
      if (is.environment(layer)) {
        # ggproto: cannot safely mutate; wrap mapping cleanup in tryCatch
        tryCatch({
          if (!is.null(layer$mapping))    layer$mapping    <- clean_quosures(layer$mapping)
          if (!is.null(layer$aes_params)) layer$aes_params <- clean_quosures(layer$aes_params)
          # Neutralize the ggproto parent chain (geom/stat/position proto
          # envs hold closures that capture the calling module).
          neutralize_env(layer)
          for (slot in c("geom", "stat", "position")) {
            if (is.environment(layer[[slot]])) neutralize_env(layer[[slot]])
          }
        }, error = function(e) NULL)
      } else if (is.list(layer)) {
        if (!is.null(layer$mapping)) layer$mapping <- clean_quosures(layer$mapping)
      }
      layer
    })
  }
  # Scales, coord, facet are ggproto too.
  if (is.environment(p$coord))  neutralize_env(p$coord)
  if (is.environment(p$facet))  neutralize_env(p$facet)
  if (!is.null(p$scales) && is.environment(p$scales)) {
    neutralize_env(p$scales)
    if (is.list(p$scales$scales)) {
      for (s in p$scales$scales) if (is.environment(s)) neutralize_env(s)
    }
  }
  p
}

#' Is this object trivially serializable? (fast-path predicate)
#'
#' Returns \code{TRUE} only for shapes that are known to round-trip
#' through base \code{serialize()} without any
#' special handling: \code{NULL}, atomic vectors (including character),
#' factors, \code{Date} / \code{POSIXt}, and data.frames / matrices
#' whose columns are themselves trivially serializable atomic/factor
#' types.  Any list column, S4 slot, environment, function / closure,
#' or ggplot object causes this predicate to return \code{FALSE} so
#' the caller falls through to the full base-serialization probe.
#'
#' This is purely an optimisation: the predicate is conservative -- a
#' \code{FALSE} return is never wrong, it just means the caller does
#' more work than strictly necessary.  A \code{TRUE} return guarantees
#' the object is safe to serialize without probing.
#'
#' @keywords internal
.is_plain_serializable_atomic <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.atomic(x) && !is.list(x) && !isS4(x)) return(TRUE)
  if (inherits(x, "factor")) return(TRUE)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(TRUE)
  if (is.matrix(x) || is.array(x)) {
    return(is.atomic(x) && !is.list(x) && !isS4(x))
  }
  FALSE
}


#' Check whether a restored UI payload is made only from plain values
#'
#' UI state snapshots intentionally contain nested lists of input values.
#' This predicate is stricter than a general serialization check: it accepts
#' only NULL, atomic vectors, plain lists that recursively contain plain UI
#' payloads, and data frames whose columns are plain atomic vectors. Runtime
#' objects such as functions, environments, external pointers, S4 instances,
#' ggplot objects, and htmlwidget/plotly payloads are rejected so callers can
#' fall back to the deeper sanitizer instead of trusting them wholesale.
#'
#' @param x Any object to validate as a UI payload.
#' @return TRUE when x is a plain UI payload; otherwise FALSE.
#' @keywords internal
is_plain_ui_payload <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.function(x) || is.environment(x) || isS4(x) ||
      identical(typeof(x), "externalptr")) {
    return(FALSE)
  }
  if (inherits(x, c("ggplot", "gg", "htmlwidget", "plotly"))) return(FALSE)

  if (is.atomic(x) && !is.list(x)) return(TRUE)

  if (is.data.frame(x)) {
    cols <- unclass(x)
    return(all(vapply(cols, function(col) {
      is.atomic(col) && !is.list(col) && !isS4(col) &&
        !is.function(col) && !is.environment(col) &&
        !identical(typeof(col), "externalptr")
    }, logical(1L))))
  }

  if (is.list(x)) {
    if (is.object(x)) return(FALSE)
    return(all(vapply(x, is_plain_ui_payload, logical(1L))))
  }

  FALSE
}

.is_plain_serializable_dataframe <- function(x) {
  if (!inherits(x, c("data.frame", "tbl", "tbl_df", "data.table"))) return(FALSE)
  for (col in x) {
    if (is.list(col) || is.environment(col) || is.function(col) || isS4(col)) return(FALSE)
    if (!.is_plain_serializable_atomic(col)) return(FALSE)
  }
  TRUE
}

.is_plain_serializable_list <- function(x) {
  if (!is.list(x) || is.data.frame(x) || isS4(x) || is.object(x)) return(FALSE)
  all(vapply(x, .is_plain_serializable_atomic, logical(1)))
}

.is_trivially_serializable <- function(x) {
  .is_plain_serializable_atomic(x) ||
    .is_plain_serializable_dataframe(x) ||
    .is_plain_serializable_list(x)
}

#' Recursively sanitize an object for serialization
#'
#' Walks into named and unnamed lists, cleaning ggplot objects in place
#' and testing each element with \code{serialize()}. Elements that still
#' fail are dropped (or, for lists, descended into so only the offending
#' children are removed). The serialization test itself is run with an
#' elevated \code{expressions} limit so that genuinely deep - but
#' finite - structures survive.
#'
#' @param x Any R object (typically a named list).
#' @param path Character vector of ancestor names, used for logging.
#' @return \code{x} with non-serializable pieces removed.
.sanitize_for_serialization <- function(x, path = character(0L), depth = 0L,
                                        max_depth = 2000L, memo = NULL) {
  # One memo table per save transaction (created by caller). If absent,
  # create a local one so historical call sites remain backward-compatible.
  if (is.null(memo) || !is.environment(memo)) {
    memo <- new.env(parent = emptyenv())
  }

  # Best-effort stable identity key for object-like/container values.
  # We intentionally avoid atomic values because copying is cheap and
  # reference identity is not meaningful there for deduplication.
  memo_key <- NULL
  if (!is.null(x) && !is.atomic(x) &&
      (is.list(x) || is.object(x) || is.environment(x))) {
    memo_key <- tryCatch({
      if (is.environment(x)) {
        paste0("env:", format(x))
      } else {
        tm <- tracemem(x)
        on.exit(try(untracemem(x), silent = TRUE), add = TRUE)
        paste0(typeof(x), ":", tm)
      }
    }, error = function(e) NULL)
  }
  if (!is.null(memo_key) && exists(memo_key, envir = memo, inherits = FALSE)) {
    memo_entry <- get(memo_key, envir = memo, inherits = FALSE)
    if (isTRUE(memo_entry$is_null)) return(NULL)
    return(memo_entry$value)
  }

  # Hard cap on R-level recursion so a pathologically deep container cannot
  # blow the interpreter stack before we even get to the serialize() test.
  # 2000 is well below R's default limit (~5000) yet deeper than any
  # realistic module snapshot.
  if (depth > max_depth) {
    full_path <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
    debug_log(paste0("Sanitizer reached max depth at ", full_path,
                     " - dropping."), 1)
    return(NULL)
  }

  # Trust fast-path: for obviously safe types (NULL, atomic vectors,
  # factors, Date/POSIXt, and data.frames / matrices whose columns are
  # all atomic/factor/Date/POSIXt), skip the expensive serialization probe
  # entirely.  These shapes serialize cleanly in 100% of realistic
  # cases; the probe was serializing potentially
  # huge data frames (data_mod, expression matrices, etc.) merely to
  # test -- and the actual transport in .build_v4_envelope() then
  # serializes them again.  Cutting the probe here removes that
  # duplicate work, which is a major contributor to slow session saves.
  #
  # Any type we don't recognise as trivially safe falls through to the
  # full probe below, preserving existing robustness for ggplots,
  # environments, S4 objects, and list columns with closures.
  if (.is_trivially_serializable(x)) return(x)

  .increment_session_save_counter("deep_sanitize_count")

  # Neutralize ggplot environments before testing, regardless of depth.
  x <- .strip_ggplot_env(x)

  is_plain_list_container <- is.list(x) &&
    !is.object(x) &&
    !inherits(x, c("data.frame", "tbl", "tbl_df", "data.table")) &&
    !isS4(x)

  # Expensive serialization probes are reserved for non-list runtime objects
  # and classed containers.  Plain lists are cheaper and safer to sanitize by
  # walking their children directly; probing the whole list first serialized
  # entire rv/module snapshots merely to decide that they were serializable.
  if (!is_plain_list_container) {
    ok <- tryCatch({
      base::serialize(x, connection = NULL)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      if (!is.null(memo_key)) {
        assign(memo_key, list(is_null = FALSE, value = x), envir = memo)
      }
      return(x)
    }

    # Not serializable. If it is a classed list, descend as a last resort;
    # otherwise drop. Descending into a data.frame or S4 object would destroy
    # its class, so drop those wholesale when they cannot be serialized.
    if (!is.list(x) || length(x) == 0L ||
        inherits(x, c("data.frame", "tbl", "tbl_df", "data.table")) ||
        isS4(x)) {
      full_path <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
      debug_log(paste0("Sanitized out non-serializable field: ", full_path), 1)
      if (!is.null(memo_key)) {
        assign(memo_key, list(is_null = TRUE), envir = memo)
      }
      return(NULL)
    }
  } else if (length(x) == 0L) {
    if (!is.null(memo_key)) {
      assign(memo_key, list(is_null = FALSE, value = x), envir = memo)
    }
    return(x)
  }

  nms <- names(x)
  n <- length(x)
  # Some restored/sanitized UI payloads have been observed with malformed
  # names attributes (for example after one child was pruned from a dynamic
  # submodule list). Normalize names before indexing or assigning them so a
  # single malformed submodule cannot drop the entire Data Wizard UI payload.
  if (!is.null(nms) && length(nms) != n) {
    full_path <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
    debug_log(paste0(
      "Sanitizer normalized malformed names at ", full_path,
      " (names=", length(nms), ", values=", n, ")"
    ), 1)
    nms_fixed <- rep.int("", n)
    if (length(nms) > 0L) {
      copy_n <- min(length(nms), n)
      nms_fixed[seq_len(copy_n)] <- nms[seq_len(copy_n)]
    }
    names(x) <- nms_fixed
    nms <- nms_fixed
  }
  out <- vector("list", n)
  keep_idx <- logical(n)
  for (i in seq_len(n)) {
    child_name <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else paste0("[[", i, "]]")
    child_path <- c(path, child_name)
    orig <- x[[i]]
    cleaned <- .sanitize_for_serialization(orig, child_path,
                                           depth = depth + 1L,
                                           max_depth = max_depth,
                                           memo = memo)
    if (is.null(cleaned) && !is.null(orig)) {
      keep_idx[i] <- FALSE
    } else {
      # Preserve retained NULL slots. With [[<- a NULL child deletes the slot
      # from this preallocated list, so the later names assignment can see the
      # observed four-name/three-value mismatch and fail the whole payload.
      out[i] <- list(cleaned)
      keep_idx[i] <- TRUE
    }
  }
  if (!is.null(nms)) names(out) <- nms
  x <- out[keep_idx]

  # Final safety check: plain lists whose children were sanitized are safe
  # without another full-tree serialization probe. Classed lists still get one
  # final probe because attributes/classes may carry non-portable state.
  if (!is_plain_list_container) {
    ok2 <- tryCatch({
      base::serialize(x, connection = NULL)
      TRUE
    }, error = function(e) FALSE)
    if (!ok2) {
      full_path <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
      debug_log(paste0("Dropped non-serializable container after pruning: ",
                       full_path), 1)
      if (!is.null(memo_key)) {
        assign(memo_key, list(is_null = TRUE), envir = memo)
      }
      return(NULL)
    }
  }
  if (!is.null(memo_key)) {
    assign(memo_key, list(is_null = FALSE, value = x), envir = memo)
  }
  x
}
