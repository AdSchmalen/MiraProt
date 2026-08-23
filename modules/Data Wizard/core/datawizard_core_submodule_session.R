# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_submodule_session.R
# Purpose:
#   Provide the core submodule session portion of the Data Wizard without changing public behavior.
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

.run_submodule_restore_callback <- function(callback, module_label, callback_reason) {
  debug_log(paste0("[RestoreCallback:start] module=", module_label,
                   " reason=", callback_reason), level = 2)
  tryCatch({
    # Deferred restore replay is imperative. Reactive reads are snapshots and
    # must neither require a consumer nor establish dependencies.
    shiny::isolate(callback())
    debug_log(paste0("[RestoreCallback:done] module=", module_label,
                     " reason=", callback_reason), level = 2)
    invisible(TRUE)
  }, error = function(e) {
    debug_log(paste0("[RestoreCallback:error] module=", module_label,
                     " reason=", callback_reason,
                     " error=", conditionMessage(e)), level = 1)
    invisible(FALSE)
  })
}

#' Create a session-restore state bridge for a Data Wizard submodule
#'
#' Generalises the File Loader's parameter-passing pattern (see
#' \code{modules/Data Wizard/datawizard_file_loader.R}):
#'   1. \code{get_session_state()} enumerates known input IDs and captures
#'      their current values via \code{isolate(input[[id]])} — this always
#'      returns plain atomics (never closures), eliminating the root cause
#'      of "cannot coerce type 'closure' to vector of type 'character'"
#'      at restore time.
#'   2. \code{set_session_state(state)} stages the payload in a local
#'      \code{pending_ui_state} reactiveVal. It does NOT schedule any
#'      \code{update*Input} calls — that responsibility belongs to the
#'      submodule's own post-restore observer.
#'   3. When a \code{restore_trigger} reactive is supplied, the factory
#'      installs an \code{observeEvent(restore_trigger(), ...)} that drains
#'      the pending payload and dispatches the type-correct
#'      \code{update*Input} calls inside a \code{session$onFlushed(once = TRUE)}
#'      callback. A double-nested flush mirrors the loader's pattern so
#'      dynamic selectize \code{choices} have time to repopulate before
#'      \code{selected} values are pushed.
#'
#' Callers pass an \code{input_specs} list mapping each input ID to its
#' widget type. Supported types:
#' \itemize{
#'   \item \code{"selectInput"}, \code{"selectizeInput"} (character scalar/vector)
#'   \item \code{"textInput"} (character scalar)
#'   \item \code{"numericInput"} (numeric)
#'   \item \code{"checkboxInput"} (logical)
#'   \item \code{"radioButtons"}, \code{"checkboxGroupInput"}
#' }
#'
#' @param session The Shiny moduleServer \code{session}.
#' @param input The Shiny moduleServer \code{input}.
#' @param input_specs Named list: names are input IDs, values are widget
#'   types (see above).
#' @param module_label Short label for debug_log entries
#'   (e.g. \code{"Ratios"}).
#' @param on_apply Optional function invoked inside the trigger observer
#'   *after* the inputs are applied. Receives the pending ui_inputs list.
#' @param get_extra Optional getter called at save time; its return value
#'   is stored under \code{state$extra} (typically a queue reactiveVal).
#' @param apply_extra Optional setter called synchronously on restore as
#'   soon as \code{set_session_state(state)} fires. Meant for queue-like
#'   state whose downstream consumers (DT renderers) depend on the queue
#'   reactiveVal directly.
#' @param restore_trigger A reactive that fires once per session restore
#'   (typically \code{reactive(rv$session_restore_trigger)}). When NULL
#'   the factory only stages state and the caller is responsible for
#'   draining \code{pending_ui_state} itself.
#' @param is_ready Optional predicate function returning TRUE when module
#'   dependencies (data, metadata, dynamic UI scaffolding) are ready for
#'   replay. When FALSE, replay is deferred and retried deterministically.
#' @param max_restore_attempts Maximum replay attempts before a staged
#'   payload is dropped with a warning log.
#' @return list with \code{get_session_state}, \code{set_session_state},
#'   \code{pending_ui_state}.
create_submodule_session_state <- function(session, input,
                                           input_specs,
                                           module_label = "submodule",
                                           on_apply = NULL,
                                           get_extra = NULL,
                                           apply_extra = NULL,
                                           restore_trigger = NULL,
                                           is_ready = NULL,
                                           max_restore_attempts = 5L) {
  pending_ui_state <- reactiveVal(NULL)
  # Keep as plain closure state (not reactiveVal): apply_inputs() can run from
  # session$onFlushed callbacks outside reactive consumers.
  last_applied_signature <- NULL
  last_deferred_signature <- NULL
  last_deferred_reason <- NULL
  last_deferred_unbound_ids <- character(0)
  active_retry_keys <- new.env(parent = emptyenv())
  exhausted_restore_keys <- new.env(parent = emptyenv())
  applying_restore <- FALSE
  restore_generation <- 0L

  is_applyable <- function(v) {
    !is.function(v) && (is.null(v) || is.atomic(v))
  }

  get_session_state <- function() {
    ids <- names(input_specs)
    ui_inputs <- stats::setNames(
      lapply(ids, function(id) {
        v <- tryCatch(isolate(input[[id]]), error = function(e) NULL)
        if (is_applyable(v)) v else NULL
      }),
      ids
    )
    ui_inputs <- ui_inputs[!vapply(ui_inputs, is.null, logical(1))]
    extra_snapshot <- NULL
    if (is.function(get_extra)) {
      extra_snapshot <- tryCatch(get_extra(), error = function(e) {
        debug_log(paste0("[", module_label, "] get_extra() failed: ",
                         e$message), level = 1)
        NULL
      })
    }
    list(
      version   = "1.0",
      module    = module_label,
      ui_inputs = ui_inputs,
      extra     = extra_snapshot
    )
  }

  pending_state_signature <- function(pending) {
    tryCatch({
      if (requireNamespace("digest", quietly = TRUE)) {
        return(digest::digest(pending, algo = "xxhash64", serialize = TRUE))
      }
      paste(utils::capture.output(str(pending, give.attr = FALSE)), collapse = "\n")
    }, error = function(e) {
      paste(names(pending), collapse = "\001")
    })
  }

  pending_signature_key <- function(st) {
    paste0(as.integer(st$generation %||% 0L), ":", as.character(st$signature %||% ""))
  }

  restore_retry_key <- function(st, reason) {
    paste(
      as.integer(st$generation %||% 0L),
      as.character(st$signature %||% ""),
      as.character(reason %||% "unspecified"),
      sep = "\001"
    )
  }

  format_restore_ids <- function(ids) {
    ids <- unique(as.character(ids %||% character(0)))
    if (length(ids) == 0L) return("<none>")
    paste(ids, collapse = ",")
  }

  log_deferred_once <- function(st, reason, deferred_reason, unbound_ids, level = 2) {
    signature_key <- pending_signature_key(st)
    unbound_ids <- unique(as.character(unbound_ids %||% character(0)))
    changed <- !identical(last_deferred_signature, signature_key) ||
      !identical(last_deferred_reason, deferred_reason) ||
      !identical(last_deferred_unbound_ids, unbound_ids)
    if (!isTRUE(changed)) return(invisible(FALSE))

    last_deferred_signature <<- signature_key
    last_deferred_reason <<- deferred_reason
    last_deferred_unbound_ids <<- unbound_ids
    debug_log(paste0("[", module_label,
                     "] restore deferred (generation=", st$generation,
                     ", signature=", st$signature,
                     ", reason=", reason,
                     ", deferred_reason=", deferred_reason,
                     ", unresolved_ids=", format_restore_ids(unbound_ids),
                     ", attempt=", as.integer(st$attempt %||% 0L), ")"),
              level = level)
    invisible(TRUE)
  }

  drop_exhausted_restore <- function(st, reason, unresolved_ids) {
    signature_key <- pending_signature_key(st)
    exhausted_key <- paste(signature_key, "exhausted", sep = "\001")
    if (!exists(exhausted_key, envir = exhausted_restore_keys, inherits = FALSE)) {
      assign(exhausted_key, TRUE, envir = exhausted_restore_keys)
      debug_log(paste0("[", module_label,
                       "] restore warning: dropping unresolved inputs after ",
                       max_restore_attempts, " attempt(s)",
                       " (generation=", st$generation,
                       ", signature=", st$signature,
                       ", reason=", reason,
                       ", unresolved_ids=", format_restore_ids(unresolved_ids), ")"),
                level = 1)
    }
    pending_ui_state(NULL)
    invisible(TRUE)
  }

  set_session_state <- function(state) {
    if (is.null(state) || !is.list(state)) return(invisible(NULL))
    pending <- if (!is.null(state$ui_inputs)) state$ui_inputs else state
    if (!is.list(pending)) return(invisible(NULL))
    pending <- pending[vapply(pending, is_applyable, logical(1))]
    restore_generation <<- as.integer(restore_generation %||% 0L) + 1L
    signature <- pending_state_signature(pending)
    pending_ui_state(list(
      ui_inputs = pending,
      extra = state$extra,
      attempt = 0L,
      staged_at = Sys.time(),
      generation = restore_generation,
      signature = signature
    ))
    last_deferred_signature <<- NULL
    last_deferred_reason <<- NULL
    last_deferred_unbound_ids <<- character(0)
    if (!is.null(state$extra) && is.function(apply_extra)) {
      tryCatch(apply_extra(state$extra), error = function(e) {
        debug_log(paste0("[", module_label, "] apply_extra() failed: ",
                         e$message), level = 1)
      })
    }
    debug_log(paste0("[", module_label,
                     "] session state staged; awaiting restore trigger (",
                     length(pending), " inputs; generation=", restore_generation,
                     ", signature=", signature, ")"), level = 2)
    invisible(NULL)
  }

  apply_one <- function(id, val) {
    spec <- input_specs[[id]]
    if (is.null(spec)) spec <- "selectizeInput"
    tryCatch(
      switch(spec,
        "selectInput"        = updateSelectInput(session, id, selected = val),
        "selectizeInput"     = updateSelectizeInput(session, id, selected = val),
        "textInput"          = updateTextInput(session, id, value = val),
        "numericInput"       = updateNumericInput(session, id, value = val),
        "checkboxInput"      = updateCheckboxInput(session, id, value = isTRUE(val)),
        "radioButtons"       = updateRadioButtons(session, id, selected = val),
        "checkboxGroupInput" = updateCheckboxGroupInput(session, id, selected = val),
        updateSelectizeInput(session, id, selected = val)
      ),
      error = function(e) {
        debug_log(paste0("[", module_label,
                         "] restore: update*Input failed for ", id, ": ",
                         e$message), level = 1)
      }
    )
  }

  if (!is.null(restore_trigger)) {
    is_globally_ready <- function() {
      if (!is.function(is_ready)) return(TRUE)
      isTRUE(tryCatch(is_ready(), error = function(e) FALSE))
    }

    input_is_bound <- function(id) {
      tryCatch({
        id %in% isolate(names(input))
      }, error = function(e) {
        FALSE
      })
    }

    ordered_ids <- function(ids) {
      spec_ids <- names(input_specs) %||% character(0)
      c(intersect(spec_ids, ids), setdiff(ids, spec_ids))
    }

    current_input_value <- function(id) {
      tryCatch(isolate(input[[id]]), error = function(e) NULL)
    }

    values_match <- function(id, expected) {
      spec <- input_specs[[id]] %||% "selectizeInput"
      actual <- current_input_value(id)
      if (is.null(expected) && is.null(actual)) return(TRUE)
      if (is.null(expected) || is.null(actual)) return(FALSE)

      if (spec %in% c("selectInput", "selectizeInput", "checkboxGroupInput")) {
        exp_chr <- as.character(expected)
        act_chr <- as.character(actual)
        return(setequal(exp_chr, act_chr) && length(exp_chr) == length(act_chr))
      }
      if (spec %in% c("textInput", "radioButtons")) {
        return(identical(as.character(actual), as.character(expected)))
      }
      if (spec == "numericInput") {
        return(isTRUE(all.equal(suppressWarnings(as.numeric(actual)),
                                suppressWarnings(as.numeric(expected)))))
      }
      if (spec == "checkboxInput") {
        return(identical(isTRUE(actual), isTRUE(expected)))
      }
      identical(actual, expected)
    }

    schedule_restore_callback <- function(st, callback_reason, callback) {
      run_restore_callback <- function() {
        .run_submodule_restore_callback(callback, module_label, callback_reason)
      }

      if (!is.function(session$onFlushed)) {
        force(callback)
        return(run_restore_callback())
      }

      retry_key <- restore_retry_key(st, callback_reason)
      if (exists(retry_key, envir = active_retry_keys, inherits = FALSE)) {
        return(invisible(FALSE))
      }

      assign(retry_key, TRUE, envir = active_retry_keys)
      session$onFlushed(once = TRUE, function() {
        if (exists(retry_key, envir = active_retry_keys, inherits = FALSE)) {
          rm(list = retry_key, envir = active_retry_keys)
        }
        run_restore_callback()
      })
      invisible(TRUE)
    }

    apply_pending_once <- function(reason = "trigger") {
      if (isTRUE(applying_restore)) return(invisible(NULL))
      st <- pending_ui_state()
      if (is.null(st) || !is.list(st)) return(invisible(NULL))
      pending <- st$ui_inputs
      if (!is.list(pending)) return(invisible(NULL))
      if (is.null(st$generation)) st$generation <- 0L
      if (is.null(st$signature)) st$signature <- pending_state_signature(pending)
      signature_key <- pending_signature_key(st)

      if (!is.null(last_applied_signature) &&
          identical(last_applied_signature, signature_key)) {
        debug_log(paste0("[", module_label,
                         "] restore skipped; generation/signature already applied ",
                         "(generation=", st$generation,
                         ", signature=", st$signature, ")"), level = 2)
        pending_ui_state(NULL)
        return(invisible(NULL))
      }

      attempt <- as.integer(st$attempt %||% 0L)
      if (!is_globally_ready()) {
        if (attempt + 1L >= max_restore_attempts) {
          drop_exhausted_restore(st, reason, names(pending))
          return(invisible(NULL))
        }
        st$attempt <- attempt + 1L
        pending_ui_state(st)
        log_deferred_once(st, reason, "module_not_ready", names(pending), level = 2)
        return(invisible(NULL))
      }

      if (length(pending) == 0L) {
        last_applied_signature <<- signature_key
        pending_ui_state(NULL)
        if (is.function(on_apply)) {
          tryCatch(on_apply(list()), error = function(e) {
            debug_log(paste0("[", module_label,
                             "] restore: on_apply hook failed: ",
                             e$message), level = 1)
          })
        }
        return(invisible(NULL))
      }

      bound_ids <- Filter(input_is_bound, names(pending))
      deferred_ids <- setdiff(names(pending), bound_ids)
      to_apply <- pending[bound_ids]

      if (length(to_apply) == 0L) {
        if (length(deferred_ids) == 0L) {
          pending_ui_state(NULL)
          return(invisible(NULL))
        }
        if (attempt + 1L >= max_restore_attempts) {
          drop_exhausted_restore(st, reason, deferred_ids)
          return(invisible(NULL))
        }
        pending_ui_state(st)
        log_deferred_once(st, reason, "inputs_not_bound", deferred_ids, level = 2)
        return(invisible(NULL))
      }

      if (length(deferred_ids) == 0L &&
          all(vapply(names(to_apply), function(id) values_match(id, to_apply[[id]]), logical(1)))) {
        last_applied_signature <<- signature_key
        last_deferred_signature <<- NULL
        last_deferred_reason <<- NULL
        last_deferred_unbound_ids <<- character(0)
        pending_ui_state(NULL)
        if (is.function(on_apply)) {
          tryCatch(on_apply(to_apply), error = function(e) {
            debug_log(paste0("[", module_label,
                             "] restore: on_apply hook failed: ",
                             e$message), level = 1)
          })
        }
        return(invisible(NULL))
      }

      apply_inputs <- function() {
        applying_restore <<- TRUE
        on.exit({
          applying_restore <<- FALSE
        }, add = TRUE)
        ids_to_apply <- ordered_ids(names(to_apply))

        debug_log(paste0("[", module_label, "] restore: applying ",
                         length(to_apply), " bound inputs (deferred ",
                         length(deferred_ids), ", reason=", reason,
                         ", generation=", st$generation,
                         ", signature=", st$signature,
                         ", unresolved_ids=", format_restore_ids(deferred_ids),
                         ", attempt=", attempt + 1L, ")"), level = 1)

        apply_pass <- function(pass_label) {
          for (id in ids_to_apply) {
            apply_one(id, to_apply[[id]])
          }
          debug_log(paste0("[", module_label, "] restore: completed ", pass_label,
                           " pass for ", length(ids_to_apply), " input(s)"), level = 2)
        }

        finalize_restore <- function() {
          if (is.function(on_apply)) {
            tryCatch(on_apply(to_apply), error = function(e) {
              debug_log(paste0("[", module_label,
                               "] restore: on_apply hook failed: ",
                               e$message), level = 1)
            })
          }

          # Re-queue unresolved dependent controls whose selected/value did
          # not stick yet (e.g. dynamic selectize choices still rebuilding).
          unresolved_ids <- Filter(function(id) {
            !values_match(id, to_apply[[id]])
          }, names(to_apply))

          remaining_ids <- unique(c(deferred_ids, unresolved_ids))
          if (length(remaining_ids) > 0L) {
            if (attempt + 1L >= max_restore_attempts) {
              drop_exhausted_restore(st, reason, remaining_ids)
              return(invisible(NULL))
            }
            st$ui_inputs <- pending[remaining_ids]
            st$attempt <- attempt + 1L
            pending_ui_state(st)
            log_deferred_once(
              st,
              reason,
              "inputs_unresolved_after_apply",
              remaining_ids,
              level = 2
            )
            schedule_restore_callback(st, "post_stabilization_retry", function() {
              tryCatch({
                isolate(apply_pending_once("post_stabilization_retry"))
              }, error = function(e) {
                debug_log(paste0("[", module_label,
                                 "] restore: post-stabilization retry failed: ",
                                 e$message), level = 1)
              })
            })
          } else {
            last_applied_signature <<- signature_key
            last_deferred_signature <<- NULL
            last_deferred_reason <<- NULL
            last_deferred_unbound_ids <<- character(0)
            pending_ui_state(NULL)
          }
        }

        # Pass 1 applies parent/select inputs first (ordered by input_specs).
        apply_pass("primary")

        # Pass 2 re-asserts values after observer chains dependent on Pass 1
        # (dynamic choices, conditional UI, etc.) have run.
        if (is.function(session$onFlushed)) {
          schedule_restore_callback(st, paste0(reason, ":stabilization"), function() {
            apply_pass("stabilization")
            finalize_restore()
          })
        } else {
          apply_pass("stabilization")
          finalize_restore()
        }
      }

      schedule_restore_callback(st, reason, function() {
        schedule_restore_callback(st, paste0(reason, ":nested"), apply_inputs)
      })
      invisible(NULL)
    }

    # Double-nested onFlushed mirrors the File Loader pattern and retries
    # pending values deterministically until bound/ready or attempts exhausted.
    observeEvent(restore_trigger(), {
      apply_pending_once("restore_trigger")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observe({
      if (isTRUE(applying_restore)) return()
      pending <- pending_ui_state()
      if (is.null(pending) || !is.list(pending)) return()
      input_names <- names(input)
      if (length(input_names) == 0L) return()
      apply_pending_once("input_bind_change")
    })
  }

  list(
    get_session_state = get_session_state,
    set_session_state = set_session_state,
    pending_ui_state  = pending_ui_state
  )
}
