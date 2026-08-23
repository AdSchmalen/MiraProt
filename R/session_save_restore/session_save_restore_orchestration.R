# ============================================================================
# Sub-script: R/session_save_restore/session_save_restore_orchestration.R
# Purpose:
#   Implement the top-level Shiny session save/restore orchestration entrypoint
#   that wires download/upload flows, progress reporting, persistence, and
#   restore-phase coordination.
#
# Architectural Role:
#   orchestration
#
# Responsibilities:
#   - Define setup_session_save_restore() and its end-to-end save/restore workflow.
#   - Preserve schema compatibility, UX progress updates, and restore diagnostics.
#   - Coordinate snapshot sanitization, persistence, and module restore execution.
#
# Non-Responsibilities (Must NOT be here):
#   - Define generic serialization helper primitives.
#   - Define module registration contracts and module-specific save/restore closures.
# ============================================================================

#' Session save/restore API inventory and visibility contract
#'
#' Public API (externally callable):
#'   - setup_session_save_restore()
#'   - register_module_session_participants()
#'
#' Internal helpers/constants (module-private in modEnv):
#'   - all SESSION_SAVE_LEVEL_* and MIRAPROT_SESSION_* constants
#'   - create_session_registry(), validate_session_snapshot(), unwrap_snapshot()
#'   - resolve_data_pair_for_restore() and all dot-prefixed helpers
#'
#' Legacy compatibility aliases (temporary):
#'   - none currently exported; obsolete typo-prone aliases are intentionally absent
#'
#' The loader reads `.session_save_restore_api` and only re-exports
#' `public_api` names, while internal helpers remain encapsulated in `modEnv`.
.session_save_restore_api <- list(
  public_api = c(
    "setup_session_save_restore",
    "create_session_registry",
    "register_module_session_participants"
  ),
  internal_helpers = c(
    "SESSION_SAVE_LEVEL_DATA",
    "SESSION_SAVE_LEVEL_ANALYSIS",
    "SESSION_SAVE_LEVEL_FULL",
    "MIRAPROT_APP_VERSION",
    "MIRAPROT_SESSION_COMPATIBLE_VERSIONS",
    "MIRAPROT_SESSION_SCHEMA_VERSION",
    ".run_session_restore_callback",
    ".build_v4_envelope",
    ".qs2_available",
    "resolve_data_pair_for_restore",
    "unwrap_snapshot",
    "validate_session_snapshot"
  ),
  legacy_aliases = character()
)



.normalize_restore_runtime_logicals <- function(rv) {
  fields <- c(
    "filter_applied",
    "data_modified",
    "metadata_ready",
    "metadata_pending"
  )
  normalized <- character(0)
  existing_fields <- tryCatch(
    names(isolate(reactiveValuesToList(rv))) %||% character(0),
    error = function(e) character(0)
  )

  for (field_name in fields) {
    value <- if (field_name %in% existing_fields) {
      tryCatch(isolate(rv[[field_name]]), error = function(e) NULL)
    } else {
      NULL
    }
    should_normalize <- !(field_name %in% existing_fields) ||
      is.null(value) ||
      length(value) == 0L ||
      (is.logical(value) && length(value) == 1L && is.na(value))

    if (isTRUE(should_normalize)) {
      tryCatch({
        rv[[field_name]] <- isTRUE(value)
        normalized <- c(normalized, field_name)
      }, error = function(e) {
        debug_log(paste0(
          "[RestoreStage:runtime_logical_normalization] failed=",
          field_name,
          " error=", e$message
        ), 1)
      })
    }
  }

  debug_log(paste0(
    "[RestoreStage:runtime_logical_normalization] normalized=",
    paste(normalized, collapse = ",")
  ), 1)
  invisible(normalized)
}


.prune_rv_snapshot_runtime_fields <- function(rv_snapshot) {
  if (!is.list(rv_snapshot)) return(rv_snapshot)

  nms <- names(rv_snapshot) %||% character(0)
  if (length(nms) == 0L) return(rv_snapshot)

  explicit_runtime_fields <- c(
    "restore_session_from_file",
    "session", "shiny_session", "session_proxy",
    "download_handler", "upload_handler"
  )
  dropped <- character(0)
  for (field_name in nms) {
    value <- tryCatch(rv_snapshot[[field_name]], error = function(e) NULL)
    drop_field <- field_name %in% explicit_runtime_fields ||
      is.function(value) ||
      is.environment(value) ||
      inherits(value, c("Observer", "ShinySession", "session_proxy"))
    if (isTRUE(drop_field)) {
      rv_snapshot[[field_name]] <- NULL
      dropped <- c(dropped, field_name)
    }
  }

  if (length(dropped) > 0L) {
    debug_log(paste0(
      "[SaveStage:rv_runtime_prune] dropped runtime-only rv field(s): ",
      paste(dropped, collapse = ",")
    ), 1)
  }

  rv_snapshot
}


.write_session_error_stub <- function(file, err_msg) {
  tryCatch({
    stub <- list(
      miraprot_session = TRUE,
      session_file_type = modEnv$MIRAPROT_SESSION_ERROR_MARKER,
      version           = modEnv$MIRAPROT_SESSION_SCHEMA_VERSION,
      app_version       = modEnv$MIRAPROT_APP_VERSION,
      created_at        = Sys.time(),
      error             = modEnv$.sanitize_session_error_message(err_msg)
    )
    saveRDS(stub, file = file, compress = TRUE)
  }, error = function(e2) {
    # Last-resort: write an empty file so the download is at least
    # completed as a (tiny) binary, not the HTML error page.
    con <- base::file(file, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(raw(0L), con)
  })
}

.write_session_envelope_with_inline_fallback <- function(envelope,
                                                        file,
                                                        module_snapshots) {
  save_err <- tryCatch({
    saveRDS(envelope, file = file, compress = TRUE)
    NULL
  }, error = function(e) conditionMessage(e))

  if (is.null(save_err)) {
    return(list(
      saved = TRUE,
      envelope = envelope,
      fallback_used = FALSE,
      dropped_modules = character(),
      error = NULL
    ))
  }

  transport <- envelope$manifest$transport %||% NA_character_
  if (identical(transport, "qs2")) {
    return(list(
      saved = FALSE,
      envelope = envelope,
      fallback_used = FALSE,
      dropped_modules = character(),
      error = save_err
    ))
  }

  if (!identical(transport, "inline_rds")) {
    return(list(
      saved = FALSE,
      envelope = envelope,
      fallback_used = FALSE,
      dropped_modules = character(),
      error = paste0("Unsupported session transport during write: ", transport)
    ))
  }

  # Inline-RDS fallback: drop modules one at a time, largest first, instead of
  # nuking every module snapshot in one shot. Most real-world overflows originate
  # in a single heavy module, so incremental dropping keeps every other module
  # round-trippable.
  debug_log(paste("saveRDS failed in inline payload; retrying after",
                  "dropping heaviest module(s):", save_err), 1)
  dropped_modules <- character()
  last_err <- save_err
  current_snaps <- envelope$payload_inline$module_snapshots %||% list()

  while (length(current_snaps) > 0L) {
    sizes <- vapply(current_snaps,
                    function(m) as.numeric(object.size(m)),
                    numeric(1))
    heaviest <- names(current_snaps)[which.max(sizes)]
    current_snaps[[heaviest]] <- NULL
    dropped_modules <- c(dropped_modules, heaviest)
    envelope$payload_inline$module_snapshots <-
      if (length(current_snaps) > 0L) current_snaps else NULL
    envelope$manifest$module_ids <- names(current_snaps) %||% character()
    envelope$manifest$failed_modules <- union(
      envelope$manifest$failed_modules,
      dropped_modules
    )

    retry_err <- tryCatch({
      saveRDS(envelope, file = file, compress = TRUE)
      NULL
    }, error = function(e) conditionMessage(e))

    if (is.null(retry_err)) {
      debug_log(paste0("Session saved after dropping module(s): ",
                       paste(dropped_modules, collapse = ", ")), 1)
      return(list(
        saved = TRUE,
        envelope = envelope,
        fallback_used = TRUE,
        dropped_modules = dropped_modules,
        error = last_err
      ))
    }
    last_err <- retry_err
  }

  # All modules dropped and saveRDS still failing -- last ditch.
  envelope$payload_inline$module_snapshots <- NULL
  envelope$manifest$failed_modules <- union(
    envelope$manifest$failed_modules,
    names(module_snapshots)
  )
  envelope$manifest$module_ids <- character()
  saveRDS(envelope, file = file, compress = TRUE)
  list(
    saved = TRUE,
    envelope = envelope,
    fallback_used = TRUE,
    dropped_modules = names(module_snapshots) %||% character(),
    error = last_err,
    modules_removed_entirely = TRUE
  )
}

#' Build the shared reactive-values snapshot for a session save level.
#'
#' The shared snapshot is deliberately allowlisted at every save level. Module
#' participants own their analysis and UI state; copying every value in `rv`
#' would duplicate that state and, more importantly, capture restore machinery
#' and live Shiny objects. Grid layout is the one cross-module UI registry that
#' does not have a registered session participant. Its entries are reduced to
#' metadata and a stable source/plot reference before generic sanitization.
#' @keywords internal
.shared_rv_snapshot_allowed_fields <- function() {
  c(
    # Canonical, authoritative data and metadata.
    "data_mod",
    "data_def",
    "primary_data_raw",
    "primary_data_raw_rv",
    "primary_data_original",
    "raw_data",
    "original_data",
    "final_processed_data",
    "final_processed_metadata",
    "data_fixed",
    "data2_fixed",
    "handson_metadata",
    "condition_groups",
    "condition_choices",
    "datawizard_identifier_option_choices",
    "central_identifier_choices",
    "central_rule_file",
    "central_loaded_rules",
    # Revision and cache-identity fields.
    "data_mod_revision_id",
    "data_def_revision_id",
    "datawizard_data_revision_id",
    "datawizard_metadata_revision_id",
    # Meaningful global state (rather than transient restore state).
    "filter_applied",
    "data_modified",
    "legacy_restore_metadata_pending",
    "legacy_restore_metadata_source",
    "datawizard_metadata_meaningful_ready",
    "datawizard_metadata_lifecycle_state",
    "datawizard_metadata_assignment_pending",
    "metadata_ready",
    "metadata_pending",
    "metadata_source",
    "px_ratio",
    "width_px",
    "width_inch",
    "height_px",
    "height_inch",
    # Lightweight Grid state is shared by every plot-producing module and has
    # no registered module snapshot owner.
    "gridplot_order",
    # Produced by the writer from gridplot_order/gridplot_selection.
    "grid_session_payload"
  )
}

.build_rv_snapshot_for_save_level <- function(rv, save_level) {
  # grid_session_payload is output-only; attempting to read it here could carry
  # a payload restored by an older writer forward without re-validating it.
  allowed_fields <- setdiff(.shared_rv_snapshot_allowed_fields(),
                            "grid_session_payload")

  snapshot <- list()
  for (field_name in unique(allowed_fields)) {
    value <- tryCatch(isolate(rv[[field_name]]), error = function(e) NULL)
    if (!is.null(value)) {
      snapshot[[field_name]] <- value
    }
  }

  # Do not copy or recursively sanitize entry$plot: a rendered ggplot can hold
  # large data and environments. Select only scalar layout metadata, and add an
  # explicit stable reference that a plot-producing module can resolve after
  # its own state has been restored. Older imported snapshots are untouched by
  # this writer and may therefore still contain their historical plot objects.
  grid_selection <- tryCatch(isolate(rv[["gridplot_selection"]]),
                             error = function(e) NULL)
  if (is.list(grid_selection)) {
    order <- snapshot$gridplot_order %||% names(grid_selection)
    order <- order[order %in% names(grid_selection)]
    spans <- tryCatch(isolate(rv[["plot_spans"]]), error = function(e) list())
    margins <- tryCatch(isolate(rv[["plot_margins"]]), error = function(e) list())
    snapshot$grid_session_payload <- lapply(
      seq_along(order),
      function(i) {
        plot_id <- order[[i]]
        entry <- grid_selection[[plot_id]]
        if (!is.list(entry)) entry <- list()
        source_ref <- as.character(entry$source %||% "")
        if (length(source_ref) == 0L || is.na(source_ref[[1L]])) source_ref <- ""
        list(
          stable_plot_id = plot_id,
          source_module = source_ref[[1L]],
          source_plot_id = as.character(entry$source_plot_id %||% entry$label %||% plot_id)[1L],
          display_label = as.character(entry$label %||% plot_id)[1L],
          include_label = isTRUE(entry$include_label %||% TRUE),
          order = as.integer(i),
          span = spans[[plot_id]] %||% list(colspan = 1L, rowspan = 1L),
          margins = margins[[plot_id]] %||% list(top = 0, right = 0, bottom = 0, left = 0)
        )
      }
    )
    snapshot$gridplot_order <- NULL
    .assert_lightweight_grid_payload(snapshot$grid_session_payload)
  }

  snapshot
}

.rv_snapshot_size_inventory <- function(rv_list, largest = 10L) {
  nms <- names(rv_list) %||% character(0)
  plot_classes <- c(
    "ggplot", "plotly", "htmlwidget", "ggproto", "grob", "gTree",
    "gList", "miraprot_plot_bundle"
  )
  contains_runtime_plot <- function(x) {
    if (inherits(x, plot_classes, which = FALSE) ||
        (is.list(x) && identical(x$kind, "ggplot"))) return(TRUE)
    if (!is.list(x) || isS4(x)) return(FALSE)
    any(vapply(x, contains_runtime_plot, logical(1L)))
  }
  size_bytes <- vapply(rv_list, function(value) {
    tryCatch(as.numeric(utils::object.size(value)), error = function(e) NA_real_)
  }, numeric(1L))
  inventory <- data.frame(
    field = nms,
    class_type = vapply(rv_list, function(value) {
      classes <- class(value)
      paste0(if (length(classes)) paste(classes, collapse = "/") else "<none>",
             " (", typeof(value), ")")
    }, character(1L)),
    size_bytes = unname(size_bytes),
    contains_runtime_plot = unname(vapply(
      rv_list, contains_runtime_plot, logical(1L)
    )),
    contract = ifelse(nms %in% .shared_rv_snapshot_allowed_fields(),
                      "kept", "pruned"),
    stringsAsFactors = FALSE
  )
  if (nrow(inventory) > 0L) {
    inventory <- inventory[order(inventory$size_bytes, decreasing = TRUE,
                                 na.last = TRUE), , drop = FALSE]
    rownames(inventory) <- NULL
  }
  attr(inventory, "total_size_bytes") <- sum(size_bytes, na.rm = TRUE)
  attr(inventory, "largest") <- utils::head(inventory, max(0L, as.integer(largest)))
  inventory
}

.log_rv_snapshot_size_inventory <- function(rv_list, stage, largest = 10L,
                                            contributors = TRUE) {
  inventory <- .rv_snapshot_size_inventory(rv_list, largest = largest)
  largest_contributors <- attr(inventory, "largest")
  if (isTRUE(contributors) && nrow(largest_contributors) > 0L) {
    apply(largest_contributors, 1L, function(item) {
      debug_log(paste0(
        "[SaveStage:rv_size_inventory:", stage, "] field=", item[["field"]],
        " class_type=", item[["class_type"]],
        " size_bytes=", item[["size_bytes"]],
        " contains_runtime_plot=", item[["contains_runtime_plot"]],
        " contract=", item[["contract"]]
      ), 2)
    })
  }
  debug_log(paste0(
    "[SaveStage:rv_size_inventory:", stage, "] field_count=", nrow(inventory),
    " total_size_bytes=", attr(inventory, "total_size_bytes")
  ), 2)
  invisible(inventory)
}

.assert_lightweight_grid_payload <- function(payload) {
  forbidden <- function(x, path = "grid_session_payload") {
    bad <- is.environment(x) || inherits(x, c(
      "ggplot", "plotly", "htmlwidget", "grob", "gTree", "gList"
    ))
    if (bad) stop("Grid session payload contains forbidden object at ", path,
                  " (", paste(class(x), collapse = "/"), ").", call. = FALSE)
    if (is.list(x)) for (i in seq_along(x)) {
      nm <- names(x)[i] %||% as.character(i)
      forbidden(x[[i]], paste0(path, "$", nm))
    }
    invisible(TRUE)
  }
  forbidden(payload)
}

.session_enforce_post_normalization_snapshot_invariants <- function(module_snapshots,
                                                                    plot_data_cache_pool = NULL,
                                                                    debug_log = NULL,
                                                                    record_warning = NULL,
                                                                    data_mod = NULL,
                                                                    data_def = NULL,
                                                                    data_mod_revision_id = NULL,
                                                                    data_def_revision_id = NULL) {
  if (!is.list(module_snapshots) || length(module_snapshots) == 0L) {
    return(module_snapshots)
  }

  plot_data_cache_pool <- .materialize_active_dataset_cache_aliases(
    plot_data_cache_pool,
    list(
      data_mod = data_mod,
      data_def = data_def,
      data_mod_revision_id = data_mod_revision_id,
      data_def_revision_id = data_def_revision_id
    )
  )

  .log_invariant_warning <- function(mid, fields, action) {
    msg <- paste0(
      "[SaveStage:post_normalization_invariant] module=", mid,
      " unexpected embedded cache/data field(s)=", paste(fields, collapse = ","),
      "; action=", action
    )
    if (is.function(debug_log)) {
      debug_log(msg, 1)
    }
    if (is.function(record_warning)) record_warning(mid, msg)
    warning(msg, call. = FALSE)
  }

  .log_invariant_action <- function(mid, fields, action) {
    msg <- paste0(
      "[SaveStage:post_normalization_invariant] module=", mid,
      " unexpected embedded cache/data field(s)=", paste(fields, collapse = ","),
      "; action=", action
    )
    if (is.function(debug_log)) {
      debug_log(msg, 1)
    }
  }

  .uses_shared_cache_pool <- function(mstate) {
    dep <- mstate$restore_cache_dependency
    is.character(dep) && length(dep) == 1L &&
      identical(dep, "shared_plot_data_cache_pool")
  }

  # A missing or unusable cache reference is not evidence that a module was
  # inactive.  Only durable, module-owned plot-presence fields may explicitly
  # opt an otherwise shared-cache declaration out of plot restoration.
  .explicitly_has_no_restore_intent <- function(mid, mstate) {
    indicators <- switch(mid,
      sampleids = "had_plot",
      pca = c("had_plot", "plots_ready"),
      volcano = "had_static_plots",
      dotplot = "plot_ready",
      venn = c("had_plot", "plot_active"),
      abundances = "had_plot",
      heatmap = "had_heatmap",
      character()
    )
    present <- intersect(indicators, names(mstate))
    if (length(present) == 0L ||
        !all(vapply(mstate[present], identical, logical(1L), FALSE))) {
      return(FALSE)
    }

    # Do not let a false summary flag override another recognized, durable
    # representation of pending plot work.
    intent_payloads <- switch(mid,
      pca = c("analysis_result", "sample_pca_results", "protein_pca_results",
              "sample_umap_results", "protein_umap_results"),
      volcano = c("plot_titles", "plot_requests_by_title"),
      heatmap = c("plot_request", "matrix_payload", "heatmap_expression_matrix",
                  "heatmap_protein_cor_matrix", "heatmap_sample_cor_matrix"),
      character()
    )
    !any(vapply(intersect(intent_payloads, names(mstate)), function(field) {
      value <- mstate[[field]]
      !is.null(value) && length(value) > 0L
    }, logical(1L)))
  }

  .direct_data_fields_approved <- function(mid, mstate) {
    identical(mid, "datawizard") || !isTRUE(.uses_shared_cache_pool(mstate))
  }

  .get_pool_entry <- function(cache_id) {
    .resolve_plot_data_cache_pool_entry(
      plot_data_cache_pool, cache_id, data_mod, data_def,
      data_mod_revision_id, data_def_revision_id
    )
  }

  .resolved_contract_pair <- function(mstate, cache_id) {
    cache_id <- .session_scalar_chr(cache_id)
    if (!nzchar(cache_id)) return(NULL)
    pair <- .get_pool_entry(cache_id)
    if (!.is_plot_cache_pair(pair)) return(NULL)
    contract <- .module_cache_ref_contract(mstate)
    if (!is.null(data_mod_revision_id)) {
      contract$data_mod_revision_id <- .plot_data_cache_revision_int(data_mod_revision_id)
    }
    if (!is.null(data_def_revision_id)) {
      contract$data_def_revision_id <- .plot_data_cache_revision_int(data_def_revision_id)
    }
    contract$plot_data_cache_ref <- cache_id
    if (!isTRUE(.cache_ref_contract_compatible(
      contract, pair$data_mod, pair$data_def
    ))) return(NULL)
    pair
  }

  .shared_cache_contract_failure <- function(mstate, intended_pair = NULL) {
    primary_ref <- .session_scalar_chr(mstate$plot_data_cache_ref)
    if (!nzchar(primary_ref)) return("primary_reference")
    primary_pair <- .get_pool_entry(primary_ref)
    if (!.is_plot_cache_pair(primary_pair)) return("missing_pool_entry")

    by_title <- mstate$plot_cache_ref_by_title
    if (!is.null(by_title)) {
      validation <- .validate_plot_cache_ref_by_title(by_title, plot_data_cache_pool)
      if (!isTRUE(validation$valid)) return("title_reference")
    }

    refs <- c(list(primary_ref), if (is.list(by_title)) by_title else list())
    if (!all(vapply(refs, function(ref) {
      !is.null(.resolved_contract_pair(mstate, ref))
    }, logical(1L)))) return("contract_compatibility")

    if (.is_plot_cache_pair(intended_pair) && !all(vapply(refs, function(ref) {
      resolved <- .get_pool_entry(.session_scalar_chr(ref))
      .is_plot_cache_pair(resolved) &&
        identical(resolved$data_mod, intended_pair$data_mod) &&
        identical(resolved$data_def, intended_pair$data_def)
    }, logical(1L)))) return("intended_pair_mismatch")

    NULL
  }

  .all_cache_refs_resolve <- function(mstate) {
    is.null(.shared_cache_contract_failure(mstate))
  }

  .resolved_refs_point_to_pair <- function(mstate, intended_pair) {
    if (!.is_plot_cache_pair(intended_pair)) return(TRUE)
    refs <- c(
      list(.session_scalar_chr(mstate$plot_data_cache_ref)),
      if (is.list(mstate$plot_cache_ref_by_title)) mstate$plot_cache_ref_by_title else list()
    )
    length(refs) > 0L && all(vapply(refs, function(ref) {
      resolved <- .get_pool_entry(.session_scalar_chr(ref))
      .is_plot_cache_pair(resolved) &&
        identical(resolved$data_mod, intended_pair$data_mod) &&
        identical(resolved$data_def, intended_pair$data_def)
    }, logical(1L)))
  }

  .repair_shared_cache_ref <- function(mstate, intended_pair) {
    if (!.is_plot_cache_pair(intended_pair)) return(NULL)
    contract <- .plot_data_cache_ref_contract(
      data_mod_revision_id = data_mod_revision_id %||% mstate$data_mod_revision_id,
      data_def_revision_id = data_def_revision_id %||% mstate$data_def_revision_id,
      data_mod = intended_pair$data_mod,
      data_def = intended_pair$data_def,
      plot_data_cache_ref = NULL
    )
    canonical_id <- .session_scalar_chr(contract$plot_data_cache_ref)
    resolved <- .get_pool_entry(canonical_id)
    if (!.is_plot_cache_pair(resolved) ||
        !identical(resolved$data_mod, intended_pair$data_mod) ||
        !identical(resolved$data_def, intended_pair$data_def)) return(NULL)

    mstate[names(contract)] <- contract
    if (is.list(mstate$plot_cache_ref_by_title)) {
      mstate$plot_cache_ref_by_title <- lapply(
        mstate$plot_cache_ref_by_title, function(unused) canonical_id
      )
    }
    if (!isTRUE(.all_cache_refs_resolve(mstate)) ||
        !isTRUE(.resolved_refs_point_to_pair(mstate, intended_pair))) return(NULL)
    mstate
  }

  .valid_embedded_pair <- function(mstate) {
    if (.is_plot_cache_pair(mstate$restore_plot_data_cache)) {
      mstate$restore_plot_data_cache
    } else if (.is_plot_cache_pair(mstate$plot_data_cache_payload)) {
      mstate$plot_data_cache_payload
    } else NULL
  }

  .log_keep_embedded_plot_cache_payload <- function(mid, reason, failure = NULL) {
    msg <- paste0(
      "[SaveStage:post_normalization_invariant] module=", mid,
      " keeping one embedded plot-data recovery pair; failure=",
      failure %||% "unknown", "; reason=", reason
    )
    if (is.function(debug_log)) debug_log(msg, 1)
    if (is.function(record_warning)) record_warning(mid, msg)
    warning(msg, call. = FALSE)
  }

  for (mid in names(module_snapshots)) {
    tryCatch({
      mstate <- tryCatch(module_snapshots[[mid]]$module_state, error = function(e) NULL)
      if (is.list(mstate)) {
        unexpected <- intersect(
          c("plot_data_cache_payload", "restore_plot_data_cache",
            "restore_plot_data_cache_by_title"),
          names(mstate)[!vapply(mstate, is.null, logical(1))]
        )
        direct_data_fields <- intersect(
          c("data_mod", "data_def"),
          names(mstate)[!vapply(mstate, is.null, logical(1))]
        )
        if (length(direct_data_fields) > 0L && !isTRUE(.direct_data_fields_approved(mid, mstate))) {
          unexpected <- union(unexpected, direct_data_fields)
        }

        if (isTRUE(.uses_shared_cache_pool(mstate))) {
            recovery_pair <- .valid_embedded_pair(mstate)
            contract_failure <- .shared_cache_contract_failure(mstate, recovery_pair)
            cache_contract_valid <- is.null(contract_failure)
            if (!cache_contract_valid && !is.null(recovery_pair)) {
              repaired_state <- .repair_shared_cache_ref(mstate, recovery_pair)
              if (is.list(repaired_state)) {
                mstate <- repaired_state
                cache_contract_valid <- TRUE
                contract_failure <- NULL
              }
            }
            fields_to_remove <- if (cache_contract_valid) unexpected else character()
            if (!cache_contract_valid) {
              if (isTRUE(.explicitly_has_no_restore_intent(mid, mstate))) {
                inactive_fields <- intersect(
                  c("plot_data_cache_ref", "plot_cache_ref_by_title",
                    "plot_data_cache_payload", "restore_plot_data_cache",
                    "restore_plot_data_cache_by_title"),
                  names(mstate)
                )
                mstate$restore_cache_dependency <- "none"
                mstate$plot_data_cache_ref <- NULL
                mstate$plot_cache_ref_by_title <- NULL
                mstate$plot_data_cache_payload <- NULL
                mstate$restore_plot_data_cache <- NULL
                mstate$restore_plot_data_cache_by_title <- NULL
                module_snapshots[[mid]]$module_state <- mstate
                .log_invariant_action(
                  mid, inactive_fields,
                  "removed_inactive_shared_cache_declaration"
                )
              } else if (is.null(recovery_pair)) {
                stop(
                  "shared cache declaration is unresolved and has no valid embedded recovery pair",
                  call. = FALSE
                )
              } else {
                # The final pool cannot represent the intended pair.  Stop
                # declaring a shared dependency and retain exactly one sanitized
                # embedded recovery pair instead.
                mstate$restore_cache_dependency <- "none"
                mstate$plot_data_cache_ref <- NULL
                mstate$plot_cache_ref_by_title <- NULL
                mstate$plot_data_cache_payload <- recovery_pair
                mstate$restore_plot_data_cache <- NULL
                mstate$restore_plot_data_cache_by_title <- NULL
                mstate$restore_cache_degraded <- FALSE
                mstate$restore_cache_degraded_reason <- "embedded_recovery"
                module_snapshots[[mid]]$module_state <- mstate
                .log_keep_embedded_plot_cache_payload(
                  mid,
                  "shared cache contract unresolved; downgraded to embedded recovery",
                  contract_failure
                )
              }
            }
            if (length(fields_to_remove) > 0L) {
              .log_invariant_action(mid, fields_to_remove, "removed_for_shared_plot_data_cache_pool")
              for (field in fields_to_remove) {
                mstate[[field]] <- NULL
              }
              module_snapshots[[mid]]$module_state <- mstate
            }
        } else if (identical(.session_scalar_chr(mstate$restore_cache_degraded_reason),
                             "embedded_recovery") &&
                   .is_plot_cache_pair(mstate$plot_data_cache_payload) &&
                   is.null(mstate$restore_plot_data_cache) &&
                   is.null(mstate$restore_plot_data_cache_by_title)) {
          # This is the stable fallback shape produced above.  Revalidating an
          # already-finalized snapshot must not warn again or mutate it.
          unexpected <- setdiff(unexpected, "plot_data_cache_payload")
          if (length(unexpected) > 0L) {
            .log_invariant_warning(mid, unexpected, "left_in_place_non_shared_cache_dependency")
          }
        } else if (length(unexpected) > 0L) {
          .log_invariant_warning(mid, unexpected, "left_in_place_non_shared_cache_dependency")
        }
      }
    }, error = function(e) {
      msg <- paste0(
        "[SaveStage:post_normalization_invariant] module=", mid,
        " failed: ", conditionMessage(e)
      )
      if (is.function(debug_log)) debug_log(msg, 1)
      stop(msg, call. = FALSE)
    })
  }

  module_snapshots
}

.session_assert_finalized_snapshot_cache_invariants <- function(module_snapshots,
                                                                 plot_data_cache_pool = NULL,
                                                                 rv_snapshot = NULL) {
  validation_pool <- .materialize_active_dataset_cache_aliases(
    plot_data_cache_pool,
    rv_snapshot
  )
  if (!is.list(module_snapshots) || length(module_snapshots) == 0L) {
    return(invisible(TRUE))
  }

  for (mid in names(module_snapshots)) {
    mstate <- tryCatch(module_snapshots[[mid]]$module_state, error = function(e) NULL)
    if (!is.list(mstate)) next

    dependency <- .session_scalar_chr(mstate$restore_cache_dependency)
    if (!identical(dependency, "shared_plot_data_cache_pool")) {
      # An embedded pair is the intentional, finalized fallback produced when
      # the mutating post-GC pass cannot make a shared contract self-contained.
      # It is not an unexpected field at envelope time.
      next
    }

    primary_ref <- .session_scalar_chr(mstate$plot_data_cache_ref)
    if (!nzchar(primary_ref)) {
      stop("Envelope cache invariant failed for module ", mid,
           ": shared declaration has no primary reference.", call. = FALSE)
    }

    by_title <- mstate$plot_cache_ref_by_title
    if (!is.null(by_title)) {
      validation <- .validate_plot_cache_ref_by_title(by_title, validation_pool)
      if (!isTRUE(validation$valid)) {
        stop("Envelope cache invariant failed for module ", mid,
             ": shared title reference is missing from the pool or malformed.",
             call. = FALSE)
      }
    }
    refs <- c(list(primary_ref), if (is.list(by_title)) by_title else list())
    resolved_pairs <- lapply(refs, function(ref) {
      cache_id <- .session_scalar_chr(ref)
      pair <- .safe_cache_pool_get(validation_pool, cache_id)
      if (!.is_plot_cache_pair(pair)) {
        stop("Envelope cache invariant failed for module ", mid,
             ": shared reference is missing from the pool: ", cache_id,
             call. = FALSE)
      }
      contract <- .module_cache_ref_contract(mstate)
      contract$plot_data_cache_ref <- cache_id
      if (!isTRUE(.cache_ref_contract_compatible(
        contract, pair$data_mod, pair$data_def
      ))) {
        stop("Envelope cache invariant failed for module ", mid,
             ": shared reference has an incompatible revision, dimension, or fingerprint contract: ",
             cache_id, call. = FALSE)
      }
      pair
    })

    primary_pair <- resolved_pairs[[1L]]
    same_pair <- vapply(resolved_pairs, function(pair) {
      identical(pair$data_mod, primary_pair$data_mod) &&
        identical(pair$data_def, primary_pair$data_def)
    }, logical(1L))
    if (!all(same_pair)) {
      stop("Envelope cache invariant failed for module ", mid,
           ": shared references resolve to different data pairs.", call. = FALSE)
    }

    embedded_pair <- if (.is_plot_cache_pair(mstate$restore_plot_data_cache)) {
      mstate$restore_plot_data_cache
    } else if (.is_plot_cache_pair(mstate$plot_data_cache_payload)) {
      mstate$plot_data_cache_payload
    } else NULL
    if (.is_plot_cache_pair(embedded_pair) &&
        (!identical(embedded_pair$data_mod, primary_pair$data_mod) ||
           !identical(embedded_pair$data_def, primary_pair$data_def))) {
      stop("Envelope cache invariant failed for module ", mid,
           ": shared reference resolves to a different pair than embedded recovery.",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}

.is_go_gsea_result_object_field <- function(path) {
  nms <- tolower(as.character(path))
  if (length(nms) == 0L) return(FALSE)
  leaf <- nms[[length(nms)]]
  leaf %in% c(
    "results",
    "edo_go",
    "edo_go_safe",
    "res_gsea",
    "res_gsea_safe",
    "gsea_result",
    "gsea_result_safe",
    "enrich_result",
    "enrich_result_safe",
    "result_object",
    "result_objects"
  ) || grepl("(^|_)(edo|enrich|gsea).*result", leaf)
}

.validate_go_gsea_result_object <- function(x, path) {
  if (is.function(x) || is.environment(x)) {
    stop("GO/GSEA fast-path result field contains a function or environment")
  }
  if (inherits(x, "ggplot", which = FALSE)) {
    stop("GO/GSEA fast-path result field contains a ggplot object")
  }
  ok <- tryCatch({
    base::serialize(x, connection = NULL)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    full_path <- if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
    stop("GO/GSEA fast-path result field is not serializable: ", full_path)
  }
  x
}

.sanitize_known_session_safe_snapshot <- function(x, path = character(0L)) {
  if (is.null(x) || is.atomic(x)) return(x)
  if (.is_go_gsea_result_object_field(path)) {
    return(.validate_go_gsea_result_object(x, path))
  }
  if (is.function(x) || is.environment(x)) {
    stop("session-safe snapshot contains a function or environment")
  }
  if (inherits(x, "ggplot", which = FALSE)) {
    stop("session-safe snapshot contains a ggplot object")
  }
  if (isS4(x)) {
    return(.validate_go_gsea_result_object(x, path))
  }
  if (is.data.frame(x)) {
    x[] <- Map(function(value, name) {
      .sanitize_known_session_safe_snapshot(value, c(path, name))
    }, x, names(x) %||% as.character(seq_along(x)))
    return(x)
  }
  if (is.list(x)) {
    nms <- names(x) %||% as.character(seq_along(x))
    return(Map(function(value, name) {
      .sanitize_known_session_safe_snapshot(value, c(path, name))
    }, x, nms))
  }
  x
}

.session_assert_no_legacy_plot_bundles <- function(x, context = "session export") {
  hits <- .find_session_runtime_objects(x, path = context, max_hits = 25L)
  bundle_hits <- hits[grepl(":miraprot_plot_bundle$", hits)]
  if (length(bundle_hits) > 0L) {
    stop(
      "New session exports must not contain miraprot_plot_bundle payloads: ",
      paste(bundle_hits, collapse = "; ")
    )
  }
  invisible(TRUE)
}


.session_go_gsea_snapshot_field_present <- function(x, field_patterns) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    nms <- names(x) %||% character()
    if (length(nms) > 0L && any(vapply(field_patterns, function(pattern) {
      any(grepl(pattern, nms, ignore.case = TRUE))
    }, logical(1)))) {
      return(TRUE)
    }
    return(any(vapply(x, .session_go_gsea_snapshot_field_present, logical(1),
                      field_patterns = field_patterns)))
  }
  FALSE
}

.session_snapshot_contains_ggplot <- function(x) {
  if (is.null(x)) return(FALSE)
  if (inherits(x, "ggplot", which = FALSE)) return(TRUE)
  if (is.list(x)) {
    return(any(vapply(x, .session_snapshot_contains_ggplot, logical(1))))
  }
  FALSE
}

.session_safe_object_size <- function(x) {
  tryCatch(
    format(utils::object.size(x), units = "auto"),
    error = function(e) "unavailable"
  )
}


.session_approx_serialized_size_bytes <- function(x) {
  tryCatch(
    as.numeric(utils::object.size(x)),
    error = function(e) NA_real_
  )
}

.session_format_size <- function(bytes) {
  if (!is.numeric(bytes) || length(bytes) != 1L || is.na(bytes)) {
    return("unavailable")
  }
  tryCatch(
    format(structure(bytes, class = "object_size"), units = "auto"),
    error = function(e) paste0(round(bytes), " bytes")
  )
}

.session_module_schema_version <- function(snapshot) {
  version_fields <- list(
    if (is.list(snapshot)) snapshot$schema_version else NULL,
    if (is.list(snapshot)) snapshot$module_schema_version else NULL,
    if (is.list(snapshot)) snapshot$session_schema_version else NULL,
    if (is.list(snapshot)) snapshot$payload_schema_version else NULL,
    if (is.list(snapshot)) snapshot$version else NULL
  )
  for (version in version_fields) {
    if ((is.character(version) || is.numeric(version)) && length(version) == 1L && !is.na(version)) {
      return(as.character(version))
    }
  }
  "unknown"
}

.session_plot_object_presence <- function(raw_snapshot, cleaned_snapshot, sanitizer_dropped = character(), sanitizer_quarantined = character()) {
  raw_declared <- isTRUE(tryCatch(raw_snapshot$contains_plot_object, error = function(e) FALSE))
  clean_declared <- isTRUE(tryCatch(cleaned_snapshot$contains_plot_object, error = function(e) FALSE))
  raw_names <- tryCatch(names(raw_snapshot) %||% character(), error = function(e) character())
  clean_detected <- clean_declared || .session_snapshot_contains_ggplot(cleaned_snapshot)
  dropped_fields <- unique(c(sanitizer_dropped, sanitizer_quarantined))
  plot_named_raw <- length(raw_names) > 0L && any(grepl("plot|ggplot|plotly|htmlwidget", raw_names, ignore.case = TRUE))
  plot_named_drop <- length(dropped_fields) > 0L && any(grepl("plot|ggplot|plotly|htmlwidget", dropped_fields, ignore.case = TRUE))
  raw_detected <- raw_declared || plot_named_raw || plot_named_drop
  list(
    present = isTRUE(raw_detected),
    dropped = isTRUE((raw_detected && !clean_detected) || plot_named_drop),
    present_after = isTRUE(clean_detected)
  )
}

.session_object_size_summary <- function(envelope_or_parts) {
  payload_inline <- if (is.list(envelope_or_parts$payload_inline)) {
    envelope_or_parts$payload_inline
  } else {
    envelope_or_parts
  }

  rv_snapshot <- payload_inline$rv_snapshot
  module_snapshots <- payload_inline$module_snapshots %||% list()
  plot_data_cache_pool <- envelope_or_parts$plot_data_cache_pool %||%
    payload_inline$plot_data_cache_pool
  debug_log_buffer <- envelope_or_parts$debug_log_buffer %||%
    payload_inline$debug_log_buffer
  plot_data_cache_index <- envelope_or_parts$plot_data_cache_index %||%
    payload_inline$plot_data_cache_index

  module_each <- if (is.list(module_snapshots) && length(module_snapshots) > 0L) {
    vapply(module_snapshots, .session_approx_serialized_size_bytes, numeric(1))
  } else {
    numeric()
  }

  summary <- list(
    rv_snapshot = .session_approx_serialized_size_bytes(rv_snapshot),
    module_snapshots = .session_approx_serialized_size_bytes(module_snapshots),
    module_snapshot_by_id = module_each,
    plot_data_cache_pool = .session_approx_serialized_size_bytes(plot_data_cache_pool),
    debug_log_buffer = .session_approx_serialized_size_bytes(debug_log_buffer),
    plot_data_cache_index = .session_approx_serialized_size_bytes(plot_data_cache_index)
  )

  contributors <- c(
    rv_snapshot = summary$rv_snapshot,
    module_snapshots_total = summary$module_snapshots,
    stats::setNames(summary$module_snapshot_by_id, paste0("module:", names(summary$module_snapshot_by_id))),
    plot_data_cache_pool = summary$plot_data_cache_pool,
    debug_log_buffer = summary$debug_log_buffer,
    plot_data_cache_index = summary$plot_data_cache_index
  )
  contributors <- contributors[!is.na(contributors)]
  largest <- "none"
  if (length(contributors) > 0L) {
    contributors <- sort(contributors, decreasing = TRUE)
    top <- head(contributors, 5L)
    largest <- paste0(names(top), "=", vapply(top, .session_format_size, character(1)), collapse = ", ")
  }
  module_detail <- "none"
  if (length(summary$module_snapshot_by_id) > 0L) {
    module_detail <- paste0(
      names(summary$module_snapshot_by_id),
      "=",
      vapply(summary$module_snapshot_by_id, .session_format_size, character(1)),
      collapse = ", "
    )
  }

  debug_log(paste0(
    "[SaveSizeInventory] rv_snapshot=", .session_format_size(summary$rv_snapshot),
    " module_snapshots=", .session_format_size(summary$module_snapshots),
    " plot_data_cache_pool=", .session_format_size(summary$plot_data_cache_pool),
    " debug_log_buffer=", .session_format_size(summary$debug_log_buffer),
    " plot_data_cache_index=", .session_format_size(summary$plot_data_cache_index),
    " module_snapshot_by_id=", module_detail,
    " largest=", largest
  ), 1)

  invisible(summary)
}

.session_log_snapshot_size_summary <- function(rv_snapshot, module_snapshots, plot_data_cache_pool) {
  .session_object_size_summary(list(
    rv_snapshot = rv_snapshot,
    module_snapshots = module_snapshots,
    plot_data_cache_pool = plot_data_cache_pool
  ))
}

.log_go_gsea_sanitized_snapshot_summary <- function(mod_id, snapshot) {
  if (!mod_id %in% c("go", "gsea")) return(invisible(NULL))

  module_state <- if (is.list(snapshot)) snapshot$module_state else NULL
  raw_present <- !is.null(snapshot$results) ||
    .session_go_gsea_snapshot_field_present(module_state, c("(^|_)raw(_|$)", "raw.*result", "result.*raw"))
  stable_fallback_present <- .session_go_gsea_snapshot_field_present(
    module_state,
    c("stable", "fallback", "result_df", "results_df", "safe")
  )
  plot_recreation_present <- .session_go_gsea_snapshot_field_present(
    module_state,
    c("plot.*recreat", "recreat.*plot", "plot_state", "plot.*state")
  )
  ggplot_absent <- !.session_snapshot_contains_ggplot(snapshot)

  debug_log(paste0(
    "[SaveModuleObjectSummary] module=", mod_id,
    " raw_result_present=", isTRUE(raw_present),
    " stable_fallback_present=", isTRUE(stable_fallback_present),
    " plot_recreation_state_present=", isTRUE(plot_recreation_present),
    " ggplot_absent=", isTRUE(ggplot_absent),
    " object_size=", .session_safe_object_size(snapshot)
  ), 2)
  invisible(NULL)
}

.session_diag_summary <- function(x) {
  nm <- tryCatch(names(x), error = function(e) NULL)
  nm_len <- if (is.null(nm)) 0L else length(nm)
  paste0("is.list=", is.list(x), ", length=", length(x), ", names_len=", nm_len)
}

.session_log_optional_module_skip <- function(stage, mod_id, reason, level = 1) {
  debug_log(paste0(
    "[SaveStage:", stage, "] module=", mod_id,
    " skipped: ", reason
  ), level)
}

.session_optional_module_phase <- function(stage, mod_id, expr, rollback = NULL) {
  tryCatch(
    force(expr),
    error = function(e) {
      .session_log_optional_module_skip(stage, mod_id, conditionMessage(e), 1)
      if (is.function(rollback)) {
        tryCatch(rollback(), error = function(rollback_error) {
          .session_log_optional_module_skip(
            stage, mod_id,
            paste0("rollback failed: ", conditionMessage(rollback_error)),
            1
          )
        })
      }
      NULL
    }
  )
}

.resolve_session_save_level <- function(input) {
  save_level <- isolate(input$session_save_level)
  if (!is.character(save_level) || length(save_level) != 1L ||
      !(save_level %in% c(SESSION_SAVE_LEVEL_DATA,
                          SESSION_SAVE_LEVEL_ANALYSIS,
                          SESSION_SAVE_LEVEL_FULL))) {
    save_level <- SESSION_SAVE_LEVEL_FULL
  }
  save_level
}


.collect_sanitized_rv_snapshot_for_save <- function(rv, save_level) {
  # Take a snapshot of rv, excluding internal flags.  Non-full save
  # levels restore canonical Data Wizard state plus module-owned analysis
  # snapshots, so the shared rv snapshot should not also walk arbitrary
  # module-local analysis objects.  Keep canonical data and debug/log
  # fields here; GO/GSEA results and UI are preserved in module_snapshots.
  field_count_before <- tryCatch(length(names(rv)), error = function(e) NA_integer_)
  collect_start <- Sys.time()
  rv_list <- .build_rv_snapshot_for_save_level(rv, save_level)
  rv_list <- .prune_rv_snapshot_runtime_fields(rv_list)
  collect_ms <- round(
    as.numeric(difftime(Sys.time(), collect_start, units = "secs")) * 1000,
    3
  )
  field_count_collected <- length(names(rv_list) %||% character())
  rv_list$session_restoring <- NULL
  rv_list$session_restore_trigger <- NULL
  rv_list$session_restore_generation <- NULL
  if (is.null(rv_list$data_mod_revision_id)) rv_list$data_mod_revision_id <- 0L
  if (is.null(rv_list$data_def_revision_id)) rv_list$data_def_revision_id <- 0L
  if (!identical(save_level, SESSION_SAVE_LEVEL_FULL)) {
    keep_exact <- c(
      "primary_data_raw", "primary_data_raw_rv", "primary_data_original",
      "final_processed_data", "final_processed_metadata",
      "data_mod", "data_def", "data_fixed", "data2_fixed",
      "handson_metadata", "condition_groups", "condition_choices",
      "datawizard_identifier_option_choices", "central_identifier_choices",
      "central_rule_file", "central_loaded_rules",
      "data_mod_revision_id", "data_def_revision_id",
      "grid_session_payload",
      "session_save_level", "session_restore_phase"
    )
    keep_patterns <- c("log", "debug", "telemetry", "notification")
    rv_names <- names(rv_list) %||% character(0)
    keep <- rv_names %in% keep_exact |
      grepl(paste(keep_patterns, collapse = "|"), rv_names, ignore.case = TRUE)
    dropped <- setdiff(rv_names, rv_names[keep])
    rv_list <- rv_list[rv_names[keep]]
    debug_log(paste0(
      "[SaveStage:rv_prune] save_level=", save_level,
      " kept=", length(rv_list),
      " dropped=", length(dropped)
    ), 1)
  }

  # Drop non-serializable entries (e.g. ggplot objects with circular
  # environment references stored in rv$gridplot_selection).
  inventory_before <- .log_rv_snapshot_size_inventory(rv_list, "before")
  sanitize_start <- Sys.time()
  rv_list <- .sanitize_for_serialization(rv_list, memo = new.env(parent = emptyenv()))
  if (!is.null(rv_list$grid_session_payload)) {
    .assert_lightweight_grid_payload(rv_list$grid_session_payload)
  }
  sanitize_ms <- round(
    as.numeric(difftime(Sys.time(), sanitize_start, units = "secs")) * 1000,
    3
  )
  if (is.null(rv_list)) rv_list <- list()
  field_count_after <- length(names(rv_list) %||% character())
  inventory_after <- .log_rv_snapshot_size_inventory(
    rv_list, "after", contributors = FALSE
  )
  debug_log(paste0(
    "[SaveStage:sanitization] rv_list ", .session_diag_summary(rv_list),
    " rv_snapshot_collect_ms=", collect_ms,
    " rv_snapshot_sanitize_ms=", sanitize_ms,
    " field_count_before=", field_count_before,
    " field_count_collected=", field_count_collected,
    " field_count_after=", field_count_after
  ), 1)

  list(
    rv_list = rv_list,
    field_count_before = field_count_before,
    field_count_collected = field_count_collected,
    field_count_after = field_count_after,
    collect_ms = collect_ms,
    sanitize_ms = sanitize_ms,
    inventory_before = inventory_before,
    inventory_after = inventory_after
  )
}

.collect_sanitized_module_snapshots_for_save <- function(session_registry, save_level, progress, sanitize_memo) {
  # Collect module snapshots with PER-MODULE isolation: a failure in
  # one module's save_fn (or in the subsequent sanitization of that
  # module's tree) drops that module only; the rest still ship.  This
  # replaces the previous all-or-nothing retry.
  module_snapshots <- list()
  failed_modules <- character()
  raw_snapshots <- list()
  if (!is.null(session_registry)) {
    debug_log("[SaveStage:collect_snapshots:start]", 1)
    raw_snapshots <- tryCatch({
      session_registry$collect_snapshots(
        save_level,
        progress_fn = function(mod_id, step, total) {
          frac <- 0.15 + (step - 1) / max(total, 1L) * 0.35
          progress$set(
            value   = frac,
            message = paste0("Saving ", .module_display_name(mod_id),
                             " snapshot...")
          )
        }
      )
    }, error = function(e) {
      debug_log(paste0(
        "[SaveStage:collect_snapshots] skipped: ",
        conditionMessage(e)
      ), 1)
      failed_modules <<- union(failed_modules, "session_registry")
      list()
    })
    debug_log(paste0("[SaveStage:collect_snapshots:done] raw_snapshots ", .session_diag_summary(raw_snapshots)), 1)
    debug_log(paste("Collected snapshots from",
                    length(raw_snapshots), "module(s)"), 1)
    debug_log(paste("Snapshot module IDs:",
                    paste(names(raw_snapshots), collapse = ", ")), 1)
    n_raw <- length(raw_snapshots)
    snapshot_names <- names(raw_snapshots)
    for (i in seq_along(snapshot_names)) {
      mod_id <- snapshot_names[i]
      progress$set(
        value   = 0.50 + (i - 1) / max(n_raw, 1L) * 0.25,
        message = paste0("Sanitizing ", .module_display_name(mod_id),
                         " snapshot...")
      )
      sanitize_start <- Sys.time()
      raw_snapshot <- raw_snapshots[[mod_id]]
      already_sanitized <- isTRUE(attr(raw_snapshot, "miraprot_sanitized", exact = TRUE))
      known_schema_v2 <- .is_known_schema_v2_module_snapshot(raw_snapshot)
      known_session_safe <- mod_id %in% c("go", "gsea") &&
        isTRUE(raw_snapshot$session_safe) &&
        identical(raw_snapshot$session_payload_shape,
                  "analysis_result_plus_ui_v1") &&
        !isTRUE(raw_snapshot$contains_plot_object)
      sanitizer_dropped <- character()
      sanitizer_quarantined <- character()
      sanitize_mode <- if (already_sanitized) {
        "skipped_known_safe"
      } else if (known_schema_v2 || known_session_safe) {
        "schema_shallow"
      } else {
        "legacy_deep"
      }
      cleaned <- tryCatch({
        if (already_sanitized) {
          # Data Wizard and any future field-quarantined snapshots mark
          # themselves after sanitizing each exported field. Avoid an
          # expensive second recursive traversal here; only validate that
          # the module returned a list-like snapshot and drop the internal
          # marker before serialization.
          if (!is.list(raw_snapshot)) {
            stop("pre-sanitized module snapshot is not list-like")
          }
          attr(raw_snapshot, "miraprot_sanitized") <- NULL
          raw_snapshot
        } else if (known_schema_v2 || known_session_safe) {
          # Known v2 module snapshots have schema-approved atomic,
          # data-frame, and list fields. Avoid a full recursive deep
          # sanitizer for those canonical fields; only deep-clean
          # legacy/fallback fields and quarantine unexpected runtime
          # objects (environments, functions, external pointers, ggplot,
          # plotly/htmlwidget payloads).
          result <- .sanitize_schema_v2_module_snapshot(raw_snapshot,
                                                        mod_id = mod_id,
                                                        memo = sanitize_memo)
          sanitizer_dropped <<- result$dropped %||% character()
          sanitizer_quarantined <<- result$quarantined %||% character()
          result$value
        } else {
          .sanitize_for_serialization(raw_snapshot,
                                      path = mod_id,
                                      memo = sanitize_memo)
        }
      }, error = function(e) {
        debug_log(paste0("Sanitize failed for module '", mod_id,
                         "': ", e$message), 1)
        if (isTRUE(known_schema_v2 || known_session_safe) && !isTRUE(already_sanitized)) {
          sanitize_mode <<- "legacy_deep"
          tryCatch(
            .sanitize_for_serialization(raw_snapshot,
                                        path = mod_id,
                                        memo = sanitize_memo),
            error = function(deep_error) {
              debug_log(paste0("Fallback sanitize failed for module '",
                               mod_id, "': ", deep_error$message), 1)
              sanitize_mode <<- "failed"
              NULL
            }
          )
        } else {
          sanitize_mode <<- "failed"
          NULL
        }
      })
      sanitize_ms <- round(
        as.numeric(difftime(Sys.time(), sanitize_start, units = "secs")) * 1000,
        3
      )
      if (is.null(cleaned) && already_sanitized) {
        sanitize_mode <- "failed"
      }
      snapshot_size_bytes <- .session_approx_serialized_size_bytes(cleaned)
      plot_object_status <- tryCatch(
        .session_plot_object_presence(
          raw_snapshot,
          cleaned,
          sanitizer_dropped = sanitizer_dropped,
          sanitizer_quarantined = sanitizer_quarantined
        ),
        error = function(e) {
          .session_log_optional_module_skip(
            "sanitization:module", mod_id,
            paste0("plot-object presence inspection failed: ", conditionMessage(e)),
            1
          )
          list(present = FALSE, dropped = FALSE, present_after = FALSE)
        }
      )
      debug_log(paste0(
        "[SaveModuleSnapshot] module=", mod_id,
        " schema_version=", .session_module_schema_version(cleaned %||% raw_snapshot),
        " approx_size=", .session_format_size(snapshot_size_bytes),
        " sanitizer_mode=", sanitize_mode,
        " elapsed_ms=", sanitize_ms,
        " plot_object_present=", isTRUE(plot_object_status$present),
        " plot_object_dropped=", isTRUE(plot_object_status$dropped)
      ), 2)
      debug_log(paste0(
        "[SaveStage:sanitization:module] module=", mod_id,
        " mode=", sanitize_mode,
        " already_sanitized=", isTRUE(already_sanitized),
        " elapsed_ms=", sanitize_ms,
        " fields_dropped=", paste(sanitizer_dropped, collapse = "|"),
        " fields_quarantined=", paste(sanitizer_quarantined, collapse = "|")
      ), 2)
      if (mod_id %in% c("go", "gsea") && !is.null(cleaned)) {
        .log_go_gsea_sanitized_snapshot_summary(mod_id, cleaned)
      }
      if (is.null(cleaned)) {
        debug_log(paste0("Module snapshot dropped during sanitization: '",
                         mod_id, "'"), 1)
        failed_modules <- c(failed_modules, mod_id)
        next
      }
      if (identical(mod_id, "datawizard")) {
        tryCatch({
          debug_log(paste(
            "Data Wizard save: central_rule_file BEFORE module sanitize ->",
            .describe_snapshot_object(raw_snapshots[[mod_id]]$central_rule_file)
          ), 1)
          debug_log(paste(
            "Data Wizard save: central_rule_file AFTER module sanitize ->",
            .describe_snapshot_object(cleaned$central_rule_file)
          ), 1)
        }, error = function(e) {
          condition_class <- datawizard_condition_class(e)
          .session_log_optional_module_skip(
            "sanitization:module", mod_id,
            paste0("Data Wizard diagnostics failed [", condition_class, "]: ", conditionMessage(e)),
            1
          )
          if (identical(condition_class, "reactive_context_violation")) stop(e)
        })
      }
      module_snapshots[[mod_id]] <- cleaned
    }
  }

  debug_log(paste0("[SaveStage:sanitization] module_snapshots ", .session_diag_summary(module_snapshots)), 1)

  list(
    module_snapshots = module_snapshots,
    failed_modules = failed_modules,
    raw_snapshots = raw_snapshots
  )
}

.collect_sanitize_module_snapshots <- function(session_registry, save_level, progress, sanitize_memo) {
  .collect_sanitized_module_snapshots_for_save(
    session_registry = session_registry,
    save_level = save_level,
    progress = progress,
    sanitize_memo = sanitize_memo
  )
}

.build_save_time_plot_data_cache_bundle <- function(module_snapshots, rv_list, save_level) {
  # Build plot-data cache pool (restore-only): modules keep only a
  # lightweight reference id, while data_mod/data_def are stored once in the
  # snapshot-level pool. Data & Metadata saves intentionally skip this path to
  # stay fast and because they do not restore analysis plots.
  pre_cache_module_snapshots <- if (is.list(module_snapshots)) module_snapshots else list()
  input_module_ids <- names(pre_cache_module_snapshots) %||% character()
  result_module_snapshots <- pre_cache_module_snapshots
  failed_modules <- character()
  warnings <- character()

  # A save has one authoritative revision namespace.  Restored module states
  # may still carry cache aliases from the session in which they were created;
  # those aliases describe a data pair, but must not choose the namespace used
  # by the new snapshot.
  selected_revisions <- list(
    data_mod_revision_id = .plot_data_cache_revision_int(rv_list$data_mod_revision_id),
    data_def_revision_id = .plot_data_cache_revision_int(rv_list$data_def_revision_id)
  )
  .selected_plot_cache_contract <- function(pair, plot_data_cache_ref = NULL) {
    .plot_data_cache_ref_contract(
      data_mod_revision_id = selected_revisions$data_mod_revision_id,
      data_def_revision_id = selected_revisions$data_def_revision_id,
      data_mod = pair$data_mod,
      data_def = pair$data_def,
      plot_data_cache_ref = plot_data_cache_ref
    )
  }
  .selected_plot_cache_id <- function(pair) {
    .session_scalar_chr(.selected_plot_cache_contract(pair, NULL)$plot_data_cache_ref)
  }

  .record_plot_cache_warning <- function(mid = NULL, stage, message) {
    module_note <- if (is.character(mid) && length(mid) == 1L && nzchar(mid)) {
      paste0("module=", mid, " ")
    } else {
      ""
    }
    warning_msg <- paste0("[SaveStage:", stage, "] ", module_note, message)
    warnings <<- c(warnings, warning_msg)
    warning_msg
  }

  .finalize_plot_cache_bundle <- function(result_module_snapshots,
                                          plot_data_cache_pool,
                                          plot_data_cache_index,
                                          gc_report) {
    returned_module_ids <- names(result_module_snapshots) %||% character()
    removed_module_ids <- setdiff(input_module_ids, returned_module_ids)
    if (!all(returned_module_ids %in% input_module_ids)) {
      stop("plot-cache bundle returned module IDs that were not present in input: ",
           paste(setdiff(returned_module_ids, input_module_ids), collapse = ", "),
           call. = FALSE)
    }
    if (length(removed_module_ids) > 0L &&
        !all(removed_module_ids %in% failed_modules)) {
      stop("plot-cache bundle removed module IDs without listing them in failed_modules: ",
           paste(setdiff(removed_module_ids, failed_modules), collapse = ", "),
           call. = FALSE)
    }
    list(
      module_snapshots = result_module_snapshots,
      plot_data_cache_pool = plot_data_cache_pool %||% list(),
      plot_data_cache_index = plot_data_cache_index %||% list(),
      gc_report = gc_report %||% list(),
      failed_modules = failed_modules,
      warnings = warnings
    )
  }

  skipped_report <- function(mode) {
    list(mode = mode, integrity_valid = TRUE, integrity_missing_refs = character())
  }

  shared_cache_required <- FALSE
  if (identical(save_level, SESSION_SAVE_LEVEL_FULL) && is.list(result_module_snapshots)) {
    for (mid in names(pre_cache_module_snapshots)) {
      uses_cache <- tryCatch({
        isTRUE(.module_uses_shared_plot_data_cache(
          tryCatch(result_module_snapshots[[mid]]$module_state, error = function(e) NULL),
          legacy_full_session = FALSE
        ))
      }, error = function(e) {
        msg <- paste0("shared-cache predicate failed: ", conditionMessage(e))
        .record_plot_cache_warning(mid, "plot_data_cache_pool:predicate", msg)
        .session_log_optional_module_skip(
          "plot_data_cache_pool:predicate", mid, msg, 1
        )
        FALSE
      })
      if (isTRUE(uses_cache)) {
        shared_cache_required <- TRUE
        break
      }
    }
  }

  debug_log(paste0(
    "[SaveStage:plot_data_cache_pool] shared_cache_required=",
    isTRUE(shared_cache_required)
  ), 1)

  if (!isTRUE(shared_cache_required)) {
    debug_log(paste0(
      "[SaveStage:plot_data_cache_pool] skipped for save level ", save_level,
      "; no explicit shared-cache requirement"
    ), 1)
    return(.finalize_plot_cache_bundle(
      result_module_snapshots = result_module_snapshots,
      plot_data_cache_pool = list(),
      plot_data_cache_index = list(),
      gc_report = skipped_report(
        if (identical(save_level, SESSION_SAVE_LEVEL_DATA)) "skipped_data_only" else "skipped_not_required"
      )
    ))
  }

  plot_data_cache_pool <- list()

  .log_plot_cache_phase <- function(phase, mid = NULL) {
    module_note <- if (is.character(mid) && length(mid) == 1L && nzchar(mid)) {
      paste0(" module=", mid)
    } else {
      ""
    }
    debug_log(paste0(
      "[SaveStage:plot_data_cache_pool:", phase, "]",
      " module_snapshots_len=", length(result_module_snapshots),
      " plot_data_cache_pool_len=", length(plot_data_cache_pool),
      module_note
    ), 1)
    invisible(FALSE)
  }

  .log_plot_cache_module_skip <- function(mid, reason) {
    debug_log(paste0(
      "[SaveStage:plot_data_cache_pool] module=", mid,
      " optional cache processing skipped: ", reason
    ), 1)
  }

  .with_plot_cache_module_mutation <- function(phase, mid, expr) {
    original_pool <- plot_data_cache_pool
    original_snapshot <- tryCatch(result_module_snapshots[[mid]], error = function(e) NULL)
    tryCatch(
      force(expr),
      error = function(e) {
        reason <- conditionMessage(e)
        .record_plot_cache_warning(mid, paste0("plot_data_cache_pool:", phase), reason)
        plot_data_cache_pool <<- original_pool
        if (!is.null(original_snapshot) || mid %in% names(result_module_snapshots)) {
          result_module_snapshots[[mid]] <<- original_snapshot
        }
        debug_log(paste0(
          "[SaveStage:plot_data_cache_pool:", phase, "] rollback module=",
          mid, " error=", reason
        ), 1)
        NULL
      }
    )
  }

  .uses_shared_plot_data_cache <- function(mstate = NULL) {
    .module_uses_shared_plot_data_cache(mstate, legacy_full_session = FALSE)
  }
  .cache_pool_has_id <- function(pool, cache_id) !is.null(.safe_cache_pool_get(pool, cache_id))
  .valid_plot_cache_pair <- function(pair) .is_plot_cache_pair(pair)
  .hydrate_missing_cache_ref_from_payload <- function(mid, mstate) {
    if (!is.list(mstate)) return(mstate)
    pair <- if (.valid_plot_cache_pair(mstate$restore_plot_data_cache)) {
      mstate$restore_plot_data_cache
    } else if (.valid_plot_cache_pair(mstate$plot_data_cache_payload)) {
      mstate$plot_data_cache_payload
    } else {
      NULL
    }
    if (!.valid_plot_cache_pair(pair)) return(mstate)

    # Prepare both objects off to the side.  Nothing below mutates the live
    # bundle until the complete cache contract resolves from the candidate pool.
    # The embedded pair is recovery evidence, not merely a fallback for the
    # saved reference.  Always derive a fresh canonical identity from it so an
    # incompatible pre-existing reference can never select a different pair.
    contract <- .plot_data_cache_ref_contract(
      data_mod_revision_id = selected_revisions$data_mod_revision_id,
      data_def_revision_id = selected_revisions$data_def_revision_id,
      data_mod = pair$data_mod,
      data_def = pair$data_def,
      plot_data_cache_ref = NULL
    )
    cache_id <- .session_scalar_chr(contract$plot_data_cache_ref)
    if (!nzchar(cache_id)) stop("cache contract produced an empty reference", call. = FALSE)

    pool_size_before <- length(plot_data_cache_pool)
    put_result <- .safe_cache_pool_put(plot_data_cache_pool, cache_id, pair)
    if (!put_result$status %in% c("stored", "already_present")) {
      stop("invalid transient cache payload/status: ", put_result$status, call. = FALSE)
    }
    candidate_pool <- put_result$pool

    candidate_state <- mstate
    candidate_state[names(contract)] <- contract
    candidate_state$data_mod_nrow <- nrow(pair$data_mod)
    candidate_state$data_mod_ncol <- ncol(pair$data_mod)
    candidate_state$data_def_nrow <- nrow(pair$data_def)
    candidate_state$data_def_ncol <- ncol(pair$data_def)
    if (is.list(candidate_state$plot_cache_ref_by_title) &&
        length(candidate_state$plot_cache_ref_by_title) > 0L) {
      candidate_state$plot_cache_ref_by_title <- lapply(
        candidate_state$plot_cache_ref_by_title,
        function(unused) cache_id
      )
    }

    resolved_pair <- .safe_cache_pool_get(candidate_pool, candidate_state$plot_data_cache_ref)
    if (!.valid_plot_cache_pair(resolved_pair) ||
        !identical(resolved_pair$data_mod, pair$data_mod) ||
        !identical(resolved_pair$data_def, pair$data_def)) {
      stop("candidate cache reference did not resolve to its payload", call. = FALSE)
    }
    expected_pool_size <- pool_size_before + as.integer(identical(put_result$status, "stored"))
    if (!identical(length(candidate_pool), expected_pool_size)) {
      stop("candidate pool did not add the first distinct pair or reused a duplicate incorrectly",
           call. = FALSE)
    }
    if (!isTRUE(.cache_ref_contract_compatible(
      .module_cache_ref_contract(candidate_state),
      resolved_pair$data_mod,
      resolved_pair$data_def
    ))) {
      stop("candidate cache contract is incompatible with its recovered payload", call. = FALSE)
    }
    by_title <- candidate_state$plot_cache_ref_by_title
    if (is.list(by_title) && length(by_title) > 0L) {
      title_validation <- .validate_plot_cache_ref_by_title(by_title, candidate_pool)
      title_contracts_resolve <- isTRUE(title_validation$valid) && all(vapply(
        by_title,
        function(ref) {
          title_pair <- .safe_cache_pool_get(candidate_pool, .session_scalar_chr(ref))
          .valid_plot_cache_pair(title_pair) && isTRUE(.cache_ref_contract_compatible(
            .module_cache_ref_contract(candidate_state),
            title_pair$data_mod,
            title_pair$data_def
          ))
        },
        logical(1L)
      ))
      if (!isTRUE(title_contracts_resolve)) {
        stop("candidate title cache references did not resolve compatibly", call. = FALSE)
      }
    }

    # Preserve one recovery pair through indexing and GC.  Final shared-pool
    # resolution, below, is the only phase allowed to remove it.
    candidate_state$plot_data_cache_payload <- pair
    candidate_state$restore_plot_data_cache <- NULL
    candidate_state$restore_plot_data_cache_by_title <- NULL
    candidate_state$data_mod <- NULL
    candidate_state$data_def <- NULL

    # Commit the pool and module snapshot together only after all validation.
    candidate_snapshot <- result_module_snapshots[[mid]]
    candidate_snapshot$module_state <- candidate_state
    plot_data_cache_pool <<- candidate_pool
    result_module_snapshots[[mid]] <<- candidate_snapshot
    debug_log(paste0(
      "[SaveStage:plot_data_cache_pool] rehydrated missing cache ref for module=",
      mid, " from transient payload"
    ), 1)
    candidate_state
  }

  .restore_missing_pre_cache_module_snapshots <- function(phase) {
    missing_module_ids <- setdiff(input_module_ids, names(result_module_snapshots) %||% character())
    if (length(missing_module_ids) > 0L) {
      debug_log(paste0(
        "[SaveStage:plot_data_cache_pool:post_invariant] phase=", phase,
        " restored missing pre-cache module snapshot(s): ",
        paste(missing_module_ids, collapse = ",")
      ), 1)
      for (missing_mid in missing_module_ids) {
        result_module_snapshots[[missing_mid]] <<- pre_cache_module_snapshots[[missing_mid]]
      }
    }
    if (length(result_module_snapshots) != length(pre_cache_module_snapshots)) {
      debug_log(paste0(
        "[SaveStage:plot_data_cache_pool:post_invariant] phase=", phase,
        " module snapshot count mismatch after missing-module repair; before=",
        length(pre_cache_module_snapshots),
        " after=", length(result_module_snapshots)
      ), 1)
    }
    invisible(missing_module_ids)
  }

  .heatmap_has_matrix_payload <- function(mstate = NULL) {
    if (!is.list(mstate)) return(FALSE)
    any(vapply(
      list(
        mstate$heatmap_expression_matrix,
        mstate$heatmap_protein_cor_matrix,
        mstate$heatmap_sample_cor_matrix
      ),
      function(x) is.matrix(x) && nrow(x) > 0L,
      logical(1L)
    ))
  }

  .log_plot_cache_phase("first_pass:start")
  for (mid in names(pre_cache_module_snapshots)) {
    .log_plot_cache_phase(paste0("first_pass:module=", mid), mid)
    .with_plot_cache_module_mutation("first_pass", mid, {
      mstate <- tryCatch(result_module_snapshots[[mid]]$module_state, error = function(e) NULL)
      # `expr` is a lazy promise evaluated by .with_plot_cache_module_mutation().
      # A non-local exit here would leave this enclosing bundle builder, not just
      # skip the current module, truncating the first pass and its snapshots.
      if (is.list(mstate) && isTRUE(.uses_shared_plot_data_cache(mstate))) {
        pair <- if (.valid_plot_cache_pair(mstate$restore_plot_data_cache)) {
          mstate$restore_plot_data_cache
        } else if (.valid_plot_cache_pair(mstate$plot_data_cache_payload)) {
          mstate$plot_data_cache_payload
        } else {
          NULL
        }
        if (.valid_plot_cache_pair(pair)) {
          .hydrate_missing_cache_ref_from_payload(mid, mstate)
        } else {
          rv_pair <- list(data_mod = rv_list$data_mod, data_def = rv_list$data_def)
          contract <- .selected_plot_cache_contract(rv_pair, NULL)
          mstate[names(contract)] <- contract
          cache_id <- .session_scalar_chr(mstate$plot_data_cache_ref)
          if (nzchar(cache_id) &&
              inherits(rv_list$data_mod, "data.frame") && inherits(rv_list$data_def, "data.frame")) {
            put_result <- .safe_cache_pool_put(
              plot_data_cache_pool,
              cache_id,
              list(data_mod = rv_list$data_mod, data_def = rv_list$data_def)
            )
            plot_data_cache_pool <<- put_result$pool
          }
          result_module_snapshots[[mid]]$module_state <- mstate
        }
      }
    })
    .log_plot_cache_phase(paste0("first_pass:module=", mid, ":done"), mid)
  }

  .log_plot_cache_phase("repair:start")
  for (mid in names(pre_cache_module_snapshots)) {
    .log_plot_cache_phase(paste0("repair:module=", mid), mid)
    .with_plot_cache_module_mutation("repair", mid, {
      mstate <- tryCatch(result_module_snapshots[[mid]]$module_state, error = function(e) NULL)
      if (is.list(mstate) && isTRUE(.uses_shared_plot_data_cache(mstate))) {
        ref <- .session_scalar_chr(mstate$plot_data_cache_ref)
        needs_repair <- (!nzchar(ref) || !.cache_pool_has_id(plot_data_cache_pool, ref)) &&
          (.valid_plot_cache_pair(mstate$restore_plot_data_cache) ||
             .valid_plot_cache_pair(mstate$plot_data_cache_payload))
        if (isTRUE(needs_repair)) {
          .hydrate_missing_cache_ref_from_payload(mid, mstate)
        }
      }
    })
    .log_plot_cache_phase(paste0("repair:module=", mid, ":done"), mid)
  }

  .log_plot_cache_phase("heatmap_liveness:start")
  .with_plot_cache_module_mutation("heatmap_liveness", "heatmap", {
    hm_state <- tryCatch(result_module_snapshots$heatmap$module_state, error = function(e) NULL)
    if (.heatmap_has_matrix_payload(hm_state) && length(plot_data_cache_pool) == 0L &&
        inherits(rv_list$data_mod, "data.frame") && inherits(rv_list$data_def, "data.frame")) {
      cache_id <- .selected_plot_cache_id(list(
        data_mod = rv_list$data_mod, data_def = rv_list$data_def
      ))
      plot_data_cache_pool <<- .safe_cache_pool_put(
        plot_data_cache_pool,
        cache_id,
        list(data_mod = rv_list$data_mod, data_def = rv_list$data_def)
      )$pool
    }
  })

  .log_plot_cache_phase("full_session_fallback:start")
  fallback_cache_id <- ""
  snapshots_need_plot_pool <- FALSE
  for (mid in names(pre_cache_module_snapshots)) {
    needs_pool <- tryCatch(
      .session_module_snapshot_needs_plot_pool(mid, result_module_snapshots[[mid]]$module_state),
      error = function(e) {
        msg <- paste0("cache liveness predicate failed: ", conditionMessage(e))
        .record_plot_cache_warning(mid, "plot_data_cache_pool:liveness", msg)
        .log_plot_cache_module_skip(mid, msg)
        FALSE
      }
    )
    if (isTRUE(needs_pool)) {
      snapshots_need_plot_pool <- TRUE
      break
    }
  }
  if (identical(save_level, SESSION_SAVE_LEVEL_FULL) && length(plot_data_cache_pool) == 0L &&
      isTRUE(snapshots_need_plot_pool) &&
      inherits(rv_list$data_mod, "data.frame") && inherits(rv_list$data_def, "data.frame")) {
    plot_data_cache_pool <- tryCatch({
      fallback_cache_id <- .selected_plot_cache_id(list(
        data_mod = rv_list$data_mod, data_def = rv_list$data_def
      ))
      candidate_pool <- .safe_cache_pool_put(
        plot_data_cache_pool,
        fallback_cache_id,
        list(data_mod = rv_list$data_mod, data_def = rv_list$data_def)
      )$pool
      if (!.valid_plot_cache_pair(.safe_cache_pool_get(candidate_pool, fallback_cache_id))) {
        stop("generated fallback cache reference did not resolve", call. = FALSE)
      }
      candidate_pool
    }, error = function(e) {
      fallback_cache_id <<- ""
      msg <- paste0("full-session fallback skipped: ", conditionMessage(e))
      .record_plot_cache_warning(NULL, "plot_data_cache_pool:full_session_fallback", msg)
      debug_log(paste0("[SaveStage:plot_data_cache_pool] skipped: ", msg), 1)
      plot_data_cache_pool
    })
  }

  # The fallback is not useful unless the module contracts that caused it to
  # be created can actually reach it.  Rebind only states whose saved plot used
  # the canonical rv pair.  A module-local payload is authoritative evidence of
  # a different plot dataset and must remain a separate pool entry.
  if (nzchar(fallback_cache_id)) {
    fallback_pair <- list(data_mod = rv_list$data_mod, data_def = rv_list$data_def)
    for (mid in names(pre_cache_module_snapshots)) {
      mstate <- tryCatch(result_module_snapshots[[mid]]$module_state, error = function(e) NULL)
      intended_restore <- tryCatch(
        .session_module_snapshot_needs_plot_pool(mid, mstate),
        error = function(e) FALSE
      )
      if (!is.list(mstate) || !isTRUE(.uses_shared_plot_data_cache(mstate)) ||
          !isTRUE(intended_restore)) next

      tryCatch({
        payload_pair <- if (.valid_plot_cache_pair(mstate$restore_plot_data_cache)) {
          mstate$restore_plot_data_cache
        } else if (.valid_plot_cache_pair(mstate$plot_data_cache_payload)) {
          mstate$plot_data_cache_payload
        } else {
          NULL
        }
        current_ref <- .session_scalar_chr(mstate$plot_data_cache_ref)
        described_pair <- payload_pair
        if (is.null(described_pair) && nzchar(current_ref)) {
          described_pair <- .safe_cache_pool_get(plot_data_cache_pool, current_ref)
        }
        uses_fallback_pair <- is.null(described_pair) ||
          (identical(described_pair$data_mod, fallback_pair$data_mod) &&
             identical(described_pair$data_def, fallback_pair$data_def))

        if (!isTRUE(uses_fallback_pair)) {
          distinct_contract <- .selected_plot_cache_contract(described_pair, NULL)
          distinct_id <- .session_scalar_chr(distinct_contract$plot_data_cache_ref)
          distinct_put <- .safe_cache_pool_put(plot_data_cache_pool, distinct_id, described_pair)
          distinct_resolved <- .safe_cache_pool_get(distinct_put$pool, distinct_id)
          if (!.valid_plot_cache_pair(distinct_resolved) ||
              !identical(distinct_resolved$data_mod, described_pair$data_mod) ||
              !identical(distinct_resolved$data_def, described_pair$data_def)) {
            stop("distinct module payload cache reference did not resolve", call. = FALSE)
          }
          plot_data_cache_pool <- distinct_put$pool
          mstate[names(distinct_contract)] <- distinct_contract
          if (is.list(mstate$plot_cache_ref_by_title)) {
            mstate$plot_cache_ref_by_title <- lapply(
              mstate$plot_cache_ref_by_title, function(unused) distinct_id
            )
          }
        } else {
          if (!nzchar(current_ref) ||
              is.null(.safe_cache_pool_get(plot_data_cache_pool, current_ref))) {
            mstate$plot_data_cache_ref <- fallback_cache_id
          }
          if (is.list(mstate$plot_cache_ref_by_title)) {
            mstate$plot_cache_ref_by_title <- lapply(
              mstate$plot_cache_ref_by_title, function(unused) fallback_cache_id
            )
          }
          fallback_contract <- .selected_plot_cache_contract(
            fallback_pair, fallback_cache_id
          )
          mstate[names(fallback_contract)] <- fallback_contract
          resolved_fallback <- .safe_cache_pool_get(
            plot_data_cache_pool, mstate$plot_data_cache_ref
          )
          if (!.valid_plot_cache_pair(resolved_fallback) ||
              !identical(resolved_fallback$data_mod, fallback_pair$data_mod) ||
              !identical(resolved_fallback$data_def, fallback_pair$data_def)) {
            stop("rewritten fallback cache reference did not resolve", call. = FALSE)
          }
        }
        # Keep recovery evidence until the final post-GC invariant check.
        mstate$plot_data_cache_payload <- if (.valid_plot_cache_pair(payload_pair)) {
          payload_pair
        } else {
          fallback_pair
        }
        mstate$restore_plot_data_cache <- NULL
        mstate$restore_cache_degraded <- NULL
        mstate$restore_cache_degraded_reason <- NULL
        result_module_snapshots[[mid]]$module_state <- mstate
      }, error = function(e) {
        msg <- paste0("fallback cache contract repair skipped: ", conditionMessage(e))
        .record_plot_cache_warning(mid, "plot_data_cache_pool:full_session_fallback", msg)
        .log_plot_cache_module_skip(mid, msg)
      })
    }
  }

  debug_log(paste0("[SaveStage:plot_data_cache_pool] pool ", .session_diag_summary(plot_data_cache_pool)), 1)

  .log_plot_cache_phase("index:start")
  plot_data_cache_index <- tryCatch(
    .build_plot_data_cache_index(plot_data_cache_pool),
    error = function(e) {
      msg <- paste0("index build skipped: ", conditionMessage(e))
      .record_plot_cache_warning(NULL, "plot_data_cache_pool:index", msg)
      debug_log(paste0("[SaveStage:plot_data_cache_pool:index] skipped: ", conditionMessage(e)), 1)
      list()
    }
  )
  active_dataset_id <- if (inherits(rv_list$data_mod, "data.frame") && inherits(rv_list$data_def, "data.frame")) {
    .selected_plot_cache_id(list(
      data_mod = rv_list$data_mod, data_def = rv_list$data_def
    ))
  } else NA_character_

  .log_plot_cache_phase("gc:start")
  gc_module_snapshots <- list()
  for (mid in names(pre_cache_module_snapshots)) {
    tryCatch({
      .compute_required_cache_ids(result_module_snapshots[mid])
      gc_module_snapshots[[mid]] <- result_module_snapshots[[mid]]
    }, error = function(e) {
      msg <- paste0("cache-ref integrity precheck failed: ", conditionMessage(e))
      .record_plot_cache_warning(mid, "plot_data_cache_pool:gc", msg)
      .session_log_optional_module_skip(
        "plot_data_cache_pool:gc", mid, msg, 1
      )
    })
  }
  gc_result <- tryCatch(
    .gc_plot_data_cache_pool(
      module_snapshots = gc_module_snapshots,
      plot_data_cache_pool = plot_data_cache_pool,
      plot_data_cache_index = plot_data_cache_index,
      active_dataset_id = active_dataset_id
    ),
    error = function(e) {
      msg <- paste0("gc skipped: ", conditionMessage(e))
      .record_plot_cache_warning(NULL, "plot_data_cache_pool:gc", msg)
      debug_log(paste0("[SaveStage:plot_data_cache_pool:gc] skipped: ", conditionMessage(e)), 1)
      list(
        pool = plot_data_cache_pool,
        index = plot_data_cache_index,
        report = list(
          mode = "skipped_error",
          integrity_valid = FALSE,
          integrity_missing_refs = character(),
          skip_reason = conditionMessage(e)
        )
      )
    }
  )
  plot_data_cache_pool <- gc_result$pool %||% list()
  plot_data_cache_index <- gc_result$index %||% list()
  gc_report <- gc_result$report %||% list()

  plot_data_cache_pool <- tryCatch(
    .alias_active_dataset_cache_entries(
      plot_data_cache_pool = plot_data_cache_pool,
      active_dataset_id = active_dataset_id,
      rv_snapshot = rv_list
    ),
    error = function(e) {
      msg <- paste0("active dataset alias skipped: ", conditionMessage(e))
      .record_plot_cache_warning(NULL, "plot_data_cache_pool:gc", msg)
      debug_log(paste0("[SaveStage:plot_data_cache_pool:gc] active dataset alias skipped: ", conditionMessage(e)), 1)
      plot_data_cache_pool
    }
  )
  .log_plot_cache_phase("index:start")
  plot_data_cache_index <- tryCatch(
    .build_plot_data_cache_index(plot_data_cache_pool),
    error = function(e) {
      msg <- paste0("post-gc index rebuild skipped: ", conditionMessage(e))
      .record_plot_cache_warning(NULL, "plot_data_cache_pool:index", msg)
      debug_log(paste0("[SaveStage:plot_data_cache_pool:index] post-gc rebuild skipped: ", conditionMessage(e)), 1)
      plot_data_cache_index
    }
  )

  .log_plot_cache_phase("post_invariant")
  .restore_missing_pre_cache_module_snapshots("final")
  # GC and active-dataset aliasing can invalidate a reference that was sound
  # earlier.  Validate the final pool/state combination before it can reach a
  # current session file, and retain a recovery pair (with a save warning) when
  # the shared declaration cannot be made self-contained.
  result_module_snapshots <- .session_enforce_post_normalization_snapshot_invariants(
    module_snapshots = result_module_snapshots,
    plot_data_cache_pool = plot_data_cache_pool,
    debug_log = debug_log,
    record_warning = function(mid, message) {
      .record_plot_cache_warning(mid, "plot_data_cache_pool:post_invariant", message)
    },
    data_mod = rv_list$data_mod,
    data_def = rv_list$data_def,
    data_mod_revision_id = selected_revisions$data_mod_revision_id,
    data_def_revision_id = selected_revisions$data_def_revision_id
  )
  .finalize_plot_cache_bundle(
    result_module_snapshots = result_module_snapshots,
    plot_data_cache_pool = plot_data_cache_pool,
    plot_data_cache_index = plot_data_cache_index,
    gc_report = gc_report
  )
}

.build_session_plot_data_cache_bundle <- .build_save_time_plot_data_cache_bundle
.decorate_session_v4_envelope <- function(envelope, rv_snapshot, module_snapshots,
                                          plot_data_cache_pool,
                                          plot_data_cache_index, gc_report) {
  .session_assert_finalized_snapshot_cache_invariants(
    module_snapshots = module_snapshots,
    plot_data_cache_pool = plot_data_cache_pool,
    rv_snapshot = rv_snapshot
  )

  .session_assert_no_legacy_plot_bundles(module_snapshots, context = "module_snapshots")
  .session_assert_no_legacy_plot_bundles(rv_snapshot, context = "rv_snapshot")
  .session_assert_no_legacy_plot_bundles(plot_data_cache_pool, context = "plot_data_cache_pool")
  if (!identical(envelope$save_level, SESSION_SAVE_LEVEL_DATA)) {
    envelope$plot_data_cache_pool <- if (length(plot_data_cache_pool) > 0L) plot_data_cache_pool else NULL
  }
  envelope$plot_data_cache_index <- if (length(plot_data_cache_index) > 0L) plot_data_cache_index else list()
  envelope$manifest$gc_report <- gc_report
  envelope$manifest$gc_mode <- gc_report$mode %||% "off"
  envelope$manifest$cache_ref_integrity <- list(
    valid = isTRUE(gc_report$integrity_valid),
    missing_refs = gc_report$integrity_missing_refs %||% character()
  )

  .session_assert_no_legacy_plot_bundles(envelope, context = "session_envelope")

  if (identical(envelope$manifest$transport, "inline_rds")) {
    debug_log(paste0(
      "qs2 package unavailable or serialization failed; using inline RDS fallback transport ",
      "with saveRDS(compress = TRUE)."
    ), 1)
  }

  debug_log(paste0("[SaveStage:envelope_build] envelope manifest ids_len=", length(envelope$manifest$module_ids %||% character()), " failed_len=", length(envelope$manifest$failed_modules %||% character())), 1)

  envelope
}

.attach_session_save_diagnostics_to_envelope <- function(envelope, session) {
  # Embed the current daily debug log file as a top-level envelope
  # field so it travels with the snapshot for post-hoc diagnostics.
  # Kept outside the versioned transport payload so it can be read without
  # deserializing the heavy payload.  Missing / unreadable log is
  # silently skipped (stored as NULL).
  # Embed the log buffer so the level-filtered view works identically
  # after restore.  Two fields are stored:
  #   debug_log_buffer — structured data frame (new) or character (portable legacy)
  #   debug_log_level  — active level at save time (used to pre-select selector)
  # Legacy readers that only know about debug_log will see NULL for this field;
  # the restore path handles the old envelope$debug_log field as a fallback.
  envelope$debug_log_buffer <- tryCatch({
    log_dir <- Sys.getenv("MIRAPROT_LOG_DIR", "")
    if (nzchar(log_dir)) {
      # Portable mode: store as character lines (file-based, existing behaviour)
      log_file <- file.path(
        log_dir,
        paste0("miraprot-", format(Sys.time(), "%Y-%m-%d"), ".log")
      )
      if (file.exists(log_file)) readLines(log_file, warn = FALSE) else NULL
    } else {
      # All other modes: store the structured data frame
      buf <- get0(".miraprot_log_buffers", envir = globalenv(), inherits = FALSE)
      if (is.list(buf) && length(buf) > 0L) buf else NULL
    }
  }, error = function(e) NULL)

  envelope$debug_log_level <- tryCatch(
    get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
    error = function(e) 0L
  )
  envelope$session_token <- tryCatch(as.character(session$token), error = function(e) NA_character_)

  envelope
}

.write_session_save_envelope <- function(envelope, file, module_snapshots, progress) {
  # Final serialization. With qs2, the heavy tree is already a raw vector;
  # with inline_rds it remains a sanitized per-module tree. The inline-RDS
  # retry policy lives in a
  # top-level helper to keep the Shiny download handler parse-shallow and
  # avoid fragile nested brace/tryCatch structures in this orchestrator.
  progress$set(value = 0.90, message = "Writing session file...")
  save_result <- .write_session_envelope_with_inline_fallback(
    envelope = envelope,
    file = file,
    module_snapshots = module_snapshots
  )
  if (!isTRUE(save_result$saved)) {
    stop(save_result$error %||% "unknown session serialization error")
  }
  envelope <- save_result$envelope

  list(
    envelope = envelope,
    save_result = save_result
  )
}

.session_save_level_label <- function(save_level) {
  save_level_scalar <- if (is.character(save_level) && length(save_level) >= 1L) {
    save_level[[1L]]
  } else {
    SESSION_SAVE_LEVEL_FULL
  }
  if (identical(save_level_scalar, SESSION_SAVE_LEVEL_DATA)) {
    "Data & Metadata"
  } else if (identical(save_level_scalar, SESSION_SAVE_LEVEL_ANALYSIS)) {
    "Data & Analysis Results"
  } else if (identical(save_level_scalar, SESSION_SAVE_LEVEL_FULL)) {
    "Full Session State"
  } else {
    as.character(save_level_scalar)
  }
}

.notify_session_save_result <- function(save_result, n_modules, failed_modules, save_level_label) {
  force(save_level_label)
  if (isTRUE(save_result$fallback_used)) {
    if (isTRUE(save_result$modules_removed_entirely)) {
      showNotification(
        paste0("Session saved without module snapshots (", save_result$error, ")."),
        type = "warning", duration = 10
      )
    } else {
      showNotification(
        paste0("Session saved without the following module(s) due ",
               "to serialization errors: ",
               paste(save_result$dropped_modules, collapse = ", "),
               " (", save_result$error, ")."),
        type = "warning", duration = 12
      )
    }
    return(invisible(NULL))
  }

  if (length(failed_modules) > 0L) {
    showNotification(
      paste0("Session saved: ", n_modules, " module(s) included. ",
             "Dropped: ", paste(failed_modules, collapse = ", "), "."),
      type = "warning", duration = 10
    )
  } else {
    showNotification(
      paste0("Session saved successfully (", n_modules,
             " module(s) included)."),
      type = "message", duration = 5
    )
  }

  invisible(NULL)
}

setup_session_save_restore <- function(input, output, session, rv,
                                       session_registry = NULL) {
  SESSION_SAVE_LEVEL_DATA <- modEnv$SESSION_SAVE_LEVEL_DATA
  SESSION_SAVE_LEVEL_ANALYSIS <- modEnv$SESSION_SAVE_LEVEL_ANALYSIS
  SESSION_SAVE_LEVEL_FULL <- modEnv$SESSION_SAVE_LEVEL_FULL
  MIRAPROT_SESSION_SCHEMA_VERSION <- modEnv$MIRAPROT_SESSION_SCHEMA_VERSION
  MIRAPROT_APP_VERSION <- modEnv$MIRAPROT_APP_VERSION

  forbidden_symbols <- c(
    "SESSION_SAVE_LEVEL_DATA",
    "SESSION_SAVE_LEVEL_ANALYSIS",
    "SESSION_SAVE_LEVEL_FULL",
    "MIRAPROT_APP_VERSION",
    "MIRAPROT_SESSION_COMPATIBLE_VERSIONS",
    "MIRAPROT_SESSION_SCHEMA_VERSION",
    "resolve_data_pair_for_restore",
    "unwrap_snapshot",
    "validate_session_snapshot"
  )
  # When this loader is sourced directly into .GlobalEnv (for example by
  # Shiny's top-level R/ auto-source), these helpers are the runtime closure
  # environment for setup_session_save_restore() and must remain available.
  # Only report them as leaks when the session subsystem is actually hosted
  # outside .GlobalEnv (the app's modEnv architecture).
  helper_owner_env <- environment(sys.function())
  if (!identical(helper_owner_env, globalenv())) {
    leaked_symbols <- intersect(ls(envir = globalenv(), all.names = TRUE), forbidden_symbols)
    if (length(leaked_symbols) > 0L) {
      debug_log(
        paste(
          "[session_save_restore] Forbidden session symbols leaked into .GlobalEnv:",
          paste(leaked_symbols, collapse = ", ")
        ),
        1
      )
    }
  }

  try(assign("MIRAPROT_SESSION_TOKEN", as.character(session$token), envir = globalenv()), silent = TRUE)

  session_debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "SESSION", message)
    } else {
      debug_log(message, level)
    }
  }

  # --- Internal state for the restore workflow ---
  restore_validation  <- reactiveVal(NULL)
  session_log_content <- reactiveVal(NULL)
  active_restore_upload_paths <- new.env(parent = emptyenv())

  .normalized_existing_path <- function(path) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
      return(NULL)
    }
    tryCatch(
      normalizePath(path, winslash = "/", mustWork = TRUE),
      error = function(e) NULL
    )
  }

  .path_is_inside <- function(path, directory) {
    directory <- .normalized_existing_path(directory)
    if (is.null(directory)) return(FALSE)
    identical(path, directory) || startsWith(path, paste0(directory, "/"))
  }

  # Shiny normally places uploads below tempdir(). Deployments which relocate
  # uploads can explicitly identify that storage without making arbitrary paths
  # supplied through the public restore hook disposable.
  .configured_restore_upload_dirs <- function() {
    option_dirs <- unlist(
      getOption("miraprot.upload.dirs", getOption("shiny.upload.dir", character())),
      use.names = FALSE
    )
    env_dirs <- Sys.getenv(c("MIRAPROT_UPLOAD_DIR", "SHINY_UPLOAD_DIR"), unset = "")
    unique(c(tempdir(), option_dirs, env_dirs[nzchar(env_dirs)]))
  }

  .restore_non_upload_roots <- function() {
    env_roots <- Sys.getenv(
      c("MIRAPROT_DATA_DIR", "MIRAPROT_USER_DATA_DIR", "MIRAPROT_GO_CACHE",
        "ANNOTATION_HUB_CACHE", "R_USER_CACHE_DIR"),
      unset = ""
    )
    option_roots <- unlist(
      getOption("miraprot.protected.dirs", character()),
      use.names = FALSE
    )
    # Some persistent caches deliberately fall back under tempdir(); do not
    # mistake those well-known locations for Shiny's per-upload storage.
    temp_cache_roots <- file.path(
      tempdir(),
      c("MiraProt_GO_Cache", "MiraProt_BioMart_Cache", "Rtmp-cache")
    )
    unique(c(getwd(), env_roots[nzchar(env_roots)], option_roots, temp_cache_roots))
  }

  .claim_owned_restore_upload <- function(path) {
    regular_file <- isTRUE(file_test("-f", path))
    in_upload_storage <- regular_file && any(vapply(
      .configured_restore_upload_dirs(),
      function(root) .path_is_inside(path, root),
      logical(1L)
    ))
    in_protected_storage <- any(vapply(
      .restore_non_upload_roots(),
      function(root) .path_is_inside(path, root),
      logical(1L)
    ))
    already_claimed <- exists(path, envir = active_restore_upload_paths, inherits = FALSE)
    owned <- isTRUE(in_upload_storage) && !isTRUE(in_protected_storage) && !already_claimed
    if (owned) assign(path, TRUE, envir = active_restore_upload_paths)
    owned
  }

  .read_restore_snapshot <- function(path, owned_temporary_path) {
    on.exit({
      cleanup_errors <- list()
      if (isTRUE(owned_temporary_path)) {
        unlink_error <- tryCatch({
            unlink_status <- unlink(path, force = TRUE)
            if (!identical(unlink_status, 0L)) {
              stop("unlink returned status ", unlink_status)
            }
            if (file.exists(path)) stop("upload path still exists after unlink")
            debug_log(paste("Session restore temporary upload removed:", path), 2)
            NULL
          }, error = function(e) e)
        if (!is.null(unlink_error)) {
          cleanup_errors[["temporary upload removal"]] <- unlink_error
        }
      }

      claim_error <- tryCatch({
        if (exists(path, envir = active_restore_upload_paths, inherits = FALSE)) {
          rm(list = path, envir = active_restore_upload_paths)
        }
        NULL
      }, error = function(e) e)
      if (!is.null(claim_error)) {
        cleanup_errors[["upload path claim removal"]] <- claim_error
      }

      for (cleanup_name in names(cleanup_errors)) {
        try(debug_log(paste(
          "Session restore cleanup failed (", cleanup_name, "):", path, "-",
          conditionMessage(cleanup_errors[[cleanup_name]])
        ), 1), silent = TRUE)
      }
    }, add = TRUE)
    readRDS(path)
  }

  set_restored_log_content <- function(x) {
    session_log_content(x)
    assign('.miraprot_restored_log_buffers', x, envir = globalenv())
  }

  # These bindings belong to the live process and must never be cleared merely
  # because they are (correctly) absent from a serialized rv snapshot.
  persistent_runtime_bindings <- c(
    "restore_session_from_file"
  )

  # These values describe the restore currently in progress.  They are also
  # maintained by the live process rather than replaced from snapshot state.
  restore_runtime_state_fields <- c(
    "session_restoring",
    "restore_phase",
    "session_restore_phase",
    "session_restore_trigger",
    "session_restore_generation",
    "restored_session_token"
  )

  # Single allowlist used by both halves of the restore transaction: stale
  # live-state cleanup and snapshot application.  Keeping the combined value
  # in scope avoids the cleanup/application lists drifting apart.
  restore_runtime_fields <- unique(c(
    persistent_runtime_bindings,
    restore_runtime_state_fields
  ))

  restore_generation_current <- function(generation) {
    identical(isolate(rv$session_restore_generation %||% NA_integer_), generation)
  }

  restore_settlement <- function(report) {
    if (!restore_generation_current(report$generation)) return(invisible(FALSE))
    jobs <- report$jobs
    failed_fields <- isolate(rv$.restore_failed_fields %||% character())
    failed_modules <- isolate(rv$.restore_failed_modules %||% character())
    warning_parts <- character()
    if (length(failed_fields)) warning_parts <- c(warning_parts, paste0(length(failed_fields), " rv field(s) failed"))
    if (length(failed_modules)) warning_parts <- c(warning_parts, paste0(length(failed_modules), " module(s) failed: ", paste(failed_modules, collapse = ", ")))
    if (report$skipped) warning_parts <- c(warning_parts, paste0(report$skipped, " callback(s) skipped"))
    if (report$timeouts) warning_parts <- c(warning_parts, paste0(report$timeouts, " callback(s) timed out"))
    if (length(report$errors)) warning_parts <- c(warning_parts, paste0(length(report$errors), " callback error(s)"))
    n_modules <- isolate(rv$.restore_module_count %||% 0L) - length(failed_modules)
    if (identical(report$state, "SETTLED") && !length(warning_parts)) {
      showNotification(paste0("Session restored successfully. ", n_modules,
                              " module(s) restored. Modules will re-process the data."),
                       type = "message", duration = 8)
    } else {
      outstanding_names <- vapply(report$outstanding, function(job) paste(job$owner, job$reason, sep = ":"), character(1))
      debug_log(paste("Session restore settlement", report$state, "outstanding jobs:",
                      paste(outstanding_names, collapse = ", ")), 1)
      showNotification(paste0("Session restored ", tolower(report$state), ": ",
                              paste(warning_parts, collapse = "; "), "."),
                       type = if (identical(report$state, "FAILED")) "error" else "warning", duration = 10)
    }
    debug_log(paste("Session restoration completed.", n_modules, "module(s) restored, state", report$state,
                    "outcomes:", paste(names(report$outcomes), report$outcomes, sep = "=", collapse = ", ")), 1)
    invisible(TRUE)
  }
  restore_jobs <- .create_restore_job_registry(
    generation_fn = function() isolate(rv$session_restore_generation %||% NA_integer_),
    schedule_timeout = function(callback, delay) later::later(callback, delay = delay),
    settled_callback = restore_settlement
  )
  # Restore-only capability object: modules receive it through session$userData;
  # it intentionally has no save/snapshot operations.
  session$userData$restore_jobs <- restore_jobs
  session$userData$register_restore_job <- restore_jobs$register_restore_job
  session$userData$resolve_restore_job <- restore_jobs$resolve_restore_job
  session$userData$outstanding_restore_jobs <- restore_jobs$outstanding_restore_jobs

  reset_rv_for_session_restore <- function(rv_snapshot) {
    incoming_fields <- names(rv_snapshot) %||% character()
    current_fields <- tryCatch(
      names(isolate(reactiveValuesToList(rv))),
      error = function(e) character()
    )
    stale_fields <- setdiff(
      current_fields,
      c(incoming_fields, restore_runtime_fields)
    )
    failed_reset_fields <- character()
    for (field_name in stale_fields) {
      tryCatch({
        rv[[field_name]] <- NULL
      }, error = function(e) {
        debug_log(paste("Could not reset stale rv field:", field_name, "-", e$message), 1)
        failed_reset_fields <<- c(failed_reset_fields, field_name)
      })
    }
    if (length(stale_fields) > 0L) {
      debug_log(paste(
        "Session restore: reset",
        length(stale_fields) - length(failed_reset_fields),
        "stale rv field(s) before applying snapshot"
      ), 2)
    }
    failed_reset_fields
  }

  # ========================================
  # Download Handler
  # ========================================

  output$session_download <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      paste0("MiraProt_session_", timestamp, ".rds")
    },
    content = function(file) {
      # Provide extra expression depth for the inline-RDS fallback. Current
      # session payloads normally use qs2; this does not guarantee stack safety
      # for arbitrary objects accepted from module snapshots.
      old_expr <- getOption("expressions")
      options(expressions = 500000L)
      on.exit(options(expressions = old_expr), add = TRUE)

      # Bind GC mode for this save operation from UI control.
      old_gc_mode <- getOption("miraprot.session_gc_mode", "hard")
      gc_control <- isolate(input$session_gc_control)
      gc_mode <- if (is.character(gc_control) && length(gc_control) >= 1L && identical(gc_control[[1L]], "inactive")) "off" else "hard"
      options(miraprot.session_gc_mode = gc_mode)
      on.exit(options(miraprot.session_gc_mode = old_gc_mode), add = TRUE)

      # Progress bar for session download
      progress <- shiny::Progress$new(session, min = 0, max = 1)
      progress$set(value = 0, message = "Preparing session snapshot...")
      on.exit(progress$close(), add = TRUE)

      tryCatch({
        debug_log("Starting session snapshot creation", 1)

        save_level <- .resolve_session_save_level(input)
        sanitize_memo <- new.env(parent = emptyenv())

        progress$set(value = 0.05, message = "Collecting shared data...")
        shared_snapshot <- .collect_sanitized_rv_snapshot_for_save(
          rv = rv,
          save_level = save_level
        )
        progress$set(value = 0.10, message = "Shared data sanitized.")
        rv_list <- shared_snapshot$rv_list

        module_snapshot_bundle <- .collect_sanitized_module_snapshots_for_save(
          session_registry = session_registry,
          save_level = save_level,
          progress = progress,
          sanitize_memo = sanitize_memo
        )
        module_snapshots <- module_snapshot_bundle$module_snapshots
        failed_modules <- module_snapshot_bundle$failed_modules

        pre_cache_module_snapshots <- module_snapshots
        module_snapshot_count_before_cache <- length(module_snapshots)
        module_snapshot_ids_before_cache <- names(module_snapshots) %||% character()

        plot_cache_bundle <- .build_save_time_plot_data_cache_bundle(
          module_snapshots = module_snapshots,
          rv_list = rv_list,
          save_level = save_level
        )
        module_snapshots <- plot_cache_bundle$module_snapshots
        plot_data_cache_pool <- plot_cache_bundle$plot_data_cache_pool
        plot_data_cache_index <- plot_cache_bundle$plot_data_cache_index
        gc_report <- plot_cache_bundle$gc_report
        failed_modules <- union(failed_modules, plot_cache_bundle$failed_modules %||% character())
        plot_cache_warnings <- plot_cache_bundle$warnings %||% character()
        if (length(plot_cache_warnings) > 0L) {
          debug_log(paste0(
            "[SaveStage:plot_data_cache_pool] warning_count=",
            length(plot_cache_warnings)
          ), 1)
        }

        debug_log(paste0(
          "[SaveStage:plot_data_cache_pool:post_invariant] before=",
          module_snapshot_count_before_cache,
          " after=", length(module_snapshots),
          " failed=", length(failed_modules)
        ), 1)

        if (is.list(pre_cache_module_snapshots) &&
            length(module_snapshots) != length(pre_cache_module_snapshots)) {
          missing_module_ids <- setdiff(
            module_snapshot_ids_before_cache,
            names(module_snapshots) %||% character()
          )
          if (length(missing_module_ids) > 0L) {
            debug_log(paste0(
              "[SaveStage:plot_data_cache_pool:post_invariant] restored missing ",
              "pre-cache module snapshot(s) after plot-cache processing; modules=",
              paste(missing_module_ids, collapse = ",")
            ), 1)
            for (repair_mid in missing_module_ids) {
              module_snapshots[[repair_mid]] <- pre_cache_module_snapshots[[repair_mid]]
            }
          }
          if (length(module_snapshots) != length(pre_cache_module_snapshots)) {
            debug_log(paste0(
              "[SaveStage:plot_data_cache_pool:post_invariant] module snapshot count remains changed ",
              "after missing-module repair; before=", length(pre_cache_module_snapshots),
              " after=", length(module_snapshots),
              " modules=", paste(module_snapshot_ids_before_cache, collapse = ",")
            ), 1)
          }
        }

        progress$set(
          value = 0.80,
          message = "Serializing session payload (qs2 with inline RDS fallback)..."
        )
        envelope <- .build_v4_envelope(
          rv_snapshot = rv_list,
          module_snapshots = module_snapshots,
          save_level = save_level,
          failed_modules = failed_modules
        )
        envelope <- .decorate_session_v4_envelope(
          envelope = envelope,
          rv_snapshot = rv_list,
          module_snapshots = module_snapshots,
          plot_data_cache_pool = plot_data_cache_pool,
          plot_data_cache_index = plot_data_cache_index,
          gc_report = gc_report
        )

        envelope <- .attach_session_save_diagnostics_to_envelope(
          envelope = envelope,
          session = session
        )

        write_bundle <- .write_session_save_envelope(
          envelope = envelope,
          file = file,
          module_snapshots = module_snapshots,
          progress = progress
        )
        envelope <- write_bundle$envelope
        save_result <- write_bundle$save_result
        n_modules <- length(module_snapshots)
        save_level_label <- .session_save_level_label(save_level)
        default_fname <- paste0("MiraProt_session_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds")
        debug_log(paste0("Session snapshot saved successfully (",
                         n_modules, " module(s), level: ", save_level,
                         ", transport: ",
                         envelope$manifest$transport, ")"), 1)
        session_debug_log(
          sprintf(
            "MiraProt session exported | Default file name: %s | Save level: %s | Modules exported: %s",
            default_fname,
            save_level_label,
            as.character(n_modules)
          ),
          level = 0
        )
        .notify_session_save_result(
          save_result = save_result,
          n_modules = n_modules,
          failed_modules = failed_modules,
          save_level_label = save_level_label
        )
        if (isTRUE(save_result$fallback_used)) {
          return(invisible(NULL))
        }
      }, error = function(e) {
        debug_log(paste0("[SaveStage:fatal] Session snapshot failed: ", conditionMessage(e)), 1)
        .write_session_error_stub(file, e$message)
        showNotification(
          paste("Failed to save session:", e$message,
                "- downloaded file contains only an error marker."),
          type = "error",
          duration = 10
        )
      })
    },
    contentType = "application/octet-stream"
  )

  # ========================================
  # Restore: Validate on file upload
  # ========================================

  restore_session_from_file <- function(file_info, source_label = "Session restore upload") {
    restore_validation(NULL)

    restore_error_context <- function(snapshot = NULL,
                                      module_id = "unknown",
                                      current_phase = "file_read") {
      manifest <- if (is.list(snapshot)) snapshot$manifest else NULL
      paste0(
        "module_id=", paste(module_id %||% "unknown", collapse = ","),
        " | save_level=", as.character(if (is.list(snapshot)) snapshot$save_level %||% "unknown" else "unknown"),
        " | schema_version=", as.character(if (is.list(snapshot)) snapshot$version %||% "unknown" else "unknown"),
        " | transport=", as.character(if (is.list(manifest)) manifest$transport %||% "unknown" else "unknown"),
        " | current_phase=", current_phase %||% "unknown"
      )
    }

    log_restore_stage_failure <- function(stage_message,
                                          error,
                                          snapshot = NULL,
                                          module_id = "unknown",
                                          current_phase = "unknown") {
      debug_log(paste0(
        stage_message,
        ": ", conditionMessage(error),
        " | ", restore_error_context(snapshot, module_id, current_phase)
      ), 1)
    }

    restore_stage_error <- function(user_message, error) {
      restore_validation(list(valid = FALSE, message = user_message))
      showNotification(user_message, type = "error", duration = 10)
      return(invisible(NULL))
    }

    if (is.null(file_info)) return()

    # Capture the upload path once. In particular, do not retain or re-read a
    # reactive fileInput datapath after the upload lifecycle advances.
    restore_path <- .normalized_existing_path(file_info$datapath)

    # Check file extension (quick check, no progress needed)
    ext <- tolower(tools::file_ext(file_info$name))
    if (ext != "rds") {
      restore_validation(list(
        valid = FALSE,
        message = "Only .rds files are accepted. Please select a valid MiraProt session file."
      ))
      return()
    }

    # Validation and automatic restoration wrapped in a single progress bar
    withProgress(message = "Reading session file...", value = 0, {
      setProgress(value = 0.05, message = "Reading session file...")

      owned_temporary_path <- if (!is.null(restore_path)) {
        .claim_owned_restore_upload(restore_path)
      } else {
        FALSE
      }

      snapshot <- tryCatch(
        if (is.null(restore_path)) stop("restore file path does not exist") else
          .read_restore_snapshot(restore_path, owned_temporary_path),
        error = function(e) {
          debug_log(paste0(
            "Session restore file read/corruption error: ", conditionMessage(e),
            " | ", restore_error_context(NULL, current_phase = "file_read")
          ), 1)
          NULL
        }
      )

      if (is.null(snapshot)) {
        restore_validation(list(
          valid = FALSE,
          message = "The file could not be read. It may be corrupted or not a valid RDS file."
        ))
        return()
      }

      # Validation internally unwraps v3 envelopes so downstream reads of
      # rv_snapshot / module_snapshots work regardless of on-disk format.
      setProgress(value = 0.10, message = "Validating session snapshot...")
      snapshot <- tryCatch(
        unwrap_snapshot(snapshot),
        error = function(e) {
          log_restore_stage_failure("Session restore snapshot unwrap failed", e, snapshot, current_phase = "unwrap_snapshot")
          restore_stage_error(paste("Session restore snapshot unwrap failed:", conditionMessage(e)), e)
          NULL
        }
      )
      if (is.null(snapshot)) return()
      snapshot <- tryCatch(
        upgrade_session_snapshot_to_current_schema(snapshot),
        error = function(e) {
          log_restore_stage_failure("Session restore schema upgrade failed", e, snapshot, current_phase = "schema_upgrade")
          restore_stage_error(paste("Session restore schema upgrade failed:", conditionMessage(e)), e)
          NULL
        }
      )
      if (is.null(snapshot)) return()
      result <- validate_session_snapshot(snapshot)

      if (!result$valid) {
        restore_validation(result)
        return()
      }

      # Rehydrate per-level debug log buffers from the snapshot.
      # Priority: debug_log_buffer (new structured format) > debug_log (legacy).
      restored_buf <- if (!is.null(snapshot$debug_log_buffer)) {
        snapshot$debug_log_buffer
      } else if (is.character(snapshot$debug_log) && length(snapshot$debug_log) == 1L) {
        snapshot$debug_log  # legacy single-string field
      } else {
        NULL
      }

      if (is.list(restored_buf) && all(c("0", "1", "2") %in% names(restored_buf))) {
        # Normalize to cumulative semantics:
        # level 0 logger contains only level-0 messages;
        # level 1 logger contains level 0 + 1;
        # level 2 logger contains level 0 + 1 + 2.
        base0 <- restored_buf[["0"]]
        base1 <- restored_buf[["1"]]
        base2 <- restored_buf[["2"]]
        if (!is.data.frame(base0)) base0 <- data.frame(time = as.POSIXct(character(0)), level = integer(0), tag = character(0), message = character(0), line = character(0), stringsAsFactors = FALSE)
        if (!is.data.frame(base1)) base1 <- data.frame(time = as.POSIXct(character(0)), level = integer(0), tag = character(0), message = character(0), line = character(0), stringsAsFactors = FALSE)
        if (!is.data.frame(base2)) base2 <- data.frame(time = as.POSIXct(character(0)), level = integer(0), tag = character(0), message = character(0), line = character(0), stringsAsFactors = FALSE)
        merged1 <- unique(rbind(base1, base0))
        merged2 <- unique(rbind(base2, merged1))
        set_restored_log_content(list(`0` = base0, `1` = merged1, `2` = merged2))
      } else if (is.data.frame(restored_buf)) {
        # Legacy structured format: build cumulative buckets by level.
        lvl_vec <- suppressWarnings(as.integer(restored_buf$level))
        keep0 <- !is.na(lvl_vec) & lvl_vec <= 0L
        keep1 <- !is.na(lvl_vec) & lvl_vec <= 1L
        keep2 <- !is.na(lvl_vec) & lvl_vec <= 2L
        split_buf <- list(
          `0` = restored_buf[keep0, , drop = FALSE],
          `1` = restored_buf[keep1, , drop = FALSE],
          `2` = restored_buf[keep2, , drop = FALSE]
        )
        set_restored_log_content(split_buf)
      } else if (is.character(restored_buf)) {
        if (length(restored_buf) > 1L) {
          # Character vector from portable-mode (readLines) — wrap as level=0 rows
          # so they show under every filter setting.
          restored_df <- data.frame(
            time = Sys.time(), level = 0L, tag = "RESTORED",
            message = restored_buf, line = restored_buf,
            stringsAsFactors = FALSE
          )
          set_restored_log_content(list(`0` = restored_df, `1` = restored_df, `2` = restored_df))
        } else if (length(restored_buf) == 1L && nzchar(restored_buf)) {
          # Legacy single-string (pre-structured-buffer saves): wrap likewise.
          lines_vec <- strsplit(restored_buf, "\n", fixed = TRUE)[[1L]]
          restored_df <- data.frame(
            time = Sys.time(), level = 0L, tag = "RESTORED",
            message = lines_vec, line = lines_vec,
            stringsAsFactors = FALSE
          )
          set_restored_log_content(list(`0` = restored_df, `1` = restored_df, `2` = restored_df))
        } else {
          set_restored_log_content(NULL)
        }
      } else {
        set_restored_log_content(NULL)
      }

      # Pre-select the debug level that was active at save time.
      # updateSelectInput fires the observer in server_coordination.R once;
      # the no-op guard there prevents a redundant write if the level is
      # already at the saved value.
      if (!is.null(snapshot$debug_log_level) &&
          is.numeric(snapshot$debug_log_level) &&
          snapshot$debug_log_level %in% 0:2) {
        updateSelectInput(session, "debug_level_select",
                          selected = as.character(as.integer(snapshot$debug_log_level)))
      }

      # Preserve session token from snapshot for diagnostics/correlation.
      # Shiny session$token is runtime-managed and may not be writable in all
      # environments, so restore defensively and always keep a fallback copy.
      restored_session_token <- NULL
      if (is.character(snapshot$session_token) &&
          length(snapshot$session_token) == 1L &&
          nzchar(snapshot$session_token) &&
          !is.na(snapshot$session_token)) {
        restored_session_token <- snapshot$session_token
        try(session$userData$restored_session_token <- restored_session_token, silent = TRUE)
        try(rv$restored_session_token <- restored_session_token, silent = TRUE)
      }

      compat_note <- snapshot$compatibility_upgrade %||% list()
      if (length(compat_note) > 0L) {
        debug_log(paste(
          "Restore diagnostics:",
          compat_note$mode %||% "unknown_schema",
          "| data source:", compat_note$data_source %||% "unknown",
          "| metadata source:", compat_note$metadata_source %||% "unknown",
          "| metadata pending:", isTRUE(compat_note$metadata_pending)
        ), 1)
      }

      plot_data_cache_pool <- snapshot$plot_data_cache_pool %||% list()
      # Reconnect validated active-dataset aliases to the one authoritative
      # Data Wizard pair before module cache references are resolved. Stale or
      # malformed aliases remain non-pairs and therefore cannot redirect plots.
      plot_data_cache_pool <- .materialize_active_dataset_cache_aliases(
        plot_data_cache_pool, snapshot$rv_snapshot
      )
      # Only legacy-upgraded snapshots should infer a shared plot-data cache
      # dependency from save_level alone. Current schema module snapshots declare
      # their dependency explicitly; treating every full-session module as shared
      # made datawizard/GO/GSEA/STRING look degraded during restore even when they
      # do not consume plot_data_cache_pool.
      legacy_full_session_cache_dependency <- (!is.null(snapshot$compatibility_upgrade) &&
        identical(snapshot$compatibility_upgrade$mode, "legacy_upgraded_schema")) ||
        (!identical(snapshot$version, MIRAPROT_SESSION_SCHEMA_VERSION) &&
           (identical(snapshot$save_level, SESSION_SAVE_LEVEL_FULL) ||
              identical(snapshot$save_level, "full_session")))
      .uses_shared_plot_data_cache <- function(mstate = NULL) {
        if (!is.list(mstate)) return(FALSE)
        .module_uses_shared_plot_data_cache(
          mstate,
          legacy_full_session = legacy_full_session_cache_dependency
        )
      }

      .heatmap_has_matrix_payload <- function(mstate = NULL) {
        if (!is.list(mstate)) return(FALSE)
        any(vapply(
          list(
            mstate$heatmap_expression_matrix,
            mstate$heatmap_protein_cor_matrix,
            mstate$heatmap_sample_cor_matrix
          ),
          function(x) is.matrix(x) && nrow(x) > 0L,
          logical(1)
        ))
      }
      preprocess_result <- tryCatch({
        if (is.list(snapshot$module_snapshots)) {
          for (mid in names(snapshot$module_snapshots)) {
            st <- snapshot$module_snapshots[[mid]]$module_state
            if (!is.list(st)) next
            had_module_cache <- is.list(st$restore_plot_data_cache) &&
              inherits(st$restore_plot_data_cache$data_mod, "data.frame") &&
              inherits(st$restore_plot_data_cache$data_def, "data.frame")
            by_title_key_count <- if (is.list(st$plot_cache_ref_by_title)) length(st$plot_cache_ref_by_title) else 0L
            has_embedded_plot_cache <- (is.list(st$restore_plot_data_cache) &&
              inherits(st$restore_plot_data_cache$data_mod, "data.frame") &&
              inherits(st$restore_plot_data_cache$data_def, "data.frame")) ||
              (is.list(st$plot_data_cache_payload) &&
                 inherits(st$plot_data_cache_payload$data_mod, "data.frame") &&
                 inherits(st$plot_data_cache_payload$data_def, "data.frame"))
            if (isTRUE(has_embedded_plot_cache) || isTRUE(.uses_shared_plot_data_cache(st))) {
              snapshot$module_snapshots[[mid]]$module_state <-
                .resolve_plot_data_cache_for_module(st, plot_data_cache_pool)
            } else if (identical(mid, "heatmap") && .heatmap_has_matrix_payload(st)) {
              # Matrix-native Heatmap restore still keeps dataset live for GC.
              if (is.null(st$plot_data_cache_ref) && length(names(plot_data_cache_pool)) > 0L) {
                st$plot_data_cache_ref <- names(plot_data_cache_pool)[1L]
              }
              snapshot$module_snapshots[[mid]]$module_state <- st
            }
            resolved_st <- snapshot$module_snapshots[[mid]]$module_state
              by_title_hit_count <- if (is.list(resolved_st$restore_plot_data_cache_by_title)) {
              length(resolved_st$restore_plot_data_cache_by_title)
            } else 0L
            final_has_restore_cache <- is.list(resolved_st$restore_plot_data_cache) &&
              inherits(resolved_st$restore_plot_data_cache$data_mod, "data.frame") &&
              inherits(resolved_st$restore_plot_data_cache$data_def, "data.frame")
            valid_resolution_modes <- c(
              "embedded_payload", "module_ref", "by_title",
              "singleton_pool_fallback", "live_rv_compatible", "none"
            )
            resolved_mode <- resolved_st$restore_cache_resolution_mode %||% "none"
            if (!is.character(resolved_mode) || length(resolved_mode) != 1L ||
                is.na(resolved_mode) || !nzchar(resolved_mode) ||
                !resolved_mode %in% valid_resolution_modes) {
              resolved_mode <- "none"
            }
            if (!isTRUE(final_has_restore_cache) && by_title_hit_count > 0L &&
                identical(resolved_mode, "none")) {
              resolved_mode <- "by_title"
            }
            if (!isTRUE(final_has_restore_cache) && by_title_hit_count < 1L &&
                isTRUE(.uses_shared_plot_data_cache(resolved_st))) {
              live_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
              live_def <- tryCatch(rv$data_def, error = function(e) NULL)
              if (.cache_ref_contract_compatible(.module_cache_ref_contract(resolved_st), live_mod, live_def)) {
                resolved_st$restore_plot_data_cache <- list(data_mod = live_mod, data_def = live_def)
                final_has_restore_cache <- TRUE
                resolved_mode <- "live_rv_compatible"
                debug_log(sprintf(
                  "Restore preprocess [%s]: plot_data_cache_ref unresolved; using compatible live rv$data_mod/rv$data_def",
                  mid
                ), 1)
              } else {
                resolved_st$restore_cache_degraded <- TRUE
                resolved_st$restore_cache_degraded_reason <- "cache_ref_unresolved_live_data_incompatible"
                resolved_mode <- "none"
                debug_log(sprintf(
                  "Restore preprocess [%s] degraded: plot_data_cache_ref unresolved and live data revision/fingerprint incompatible",
                  mid
                ), 1)
              }
            }
            resolved_st$restore_cache_resolved <- isTRUE(final_has_restore_cache) || by_title_hit_count > 0L
            resolved_st$restore_cache_resolution_mode <- resolved_mode
            snapshot$module_snapshots[[mid]]$module_state <- resolved_st
            debug_log(sprintf(
              "Restore preprocess [%s]: plot_data_cache_ref_exists=%s by_title_key_count=%d by_title_hit_count=%d restore_plot_data_cache_available=%s restore_cache_resolution_mode=%s",
              mid,
              as.character(is.character(st$plot_data_cache_ref) && length(st$plot_data_cache_ref) == 1L && nzchar(st$plot_data_cache_ref)),
              as.integer(by_title_key_count),
              as.integer(by_title_hit_count),
              as.character(final_has_restore_cache || had_module_cache),
              resolved_mode
            ), 1)
          }
        }
        TRUE
      }, error = function(e) {
        failed_mid <- if (exists("mid", inherits = FALSE)) mid else "unknown"
        log_restore_stage_failure(
          "Session restore preprocessing failed",
          e,
          snapshot,
          module_id = failed_mid,
          current_phase = "preprocess_plot_data_cache"
        )
        restore_stage_error(paste("Session restore preprocessing failed:", conditionMessage(e)), e)
        FALSE
      })
      if (!isTRUE(preprocess_result)) return()

      # Build metadata message for the validation status display
      created_str <- if (inherits(snapshot$created_at, "POSIXt")) {
        format(snapshot$created_at, "%Y-%m-%d %H:%M:%S")
      } else {
        "unknown"
      }
      app_ver <- if (is.character(snapshot$app_version)) snapshot$app_version else "unknown"
      schema_ver <- snapshot$version
      saved_level <- if (is.character(snapshot$save_level)) snapshot$save_level else "unknown"
      n_modules <- if (is.list(snapshot$module_snapshots)) length(snapshot$module_snapshots) else 0L
      module_ids <- if (n_modules > 0L) paste(names(snapshot$module_snapshots), collapse = ", ") else "none"
      failed_note <- ""
      if (is.list(snapshot$manifest) &&
          length(snapshot$manifest$failed_modules) > 0L) {
        failed_note <- paste0(" Dropped on save: ",
                              paste(snapshot$manifest$failed_modules, collapse = ", "), ".")
      }

      result$message <- paste0(
        "Valid session file. ",
        "Created: ", created_str, ". ",
        "App version: ", app_ver, ". ",
        "Schema version: ", schema_ver, ". ",
        "Save level: ", saved_level, ". ",
        "Module snapshots: ", n_modules, " (", module_ids, ").",
        failed_note,
        " Data dimensions: ", nrow(snapshot$rv_snapshot$data_mod), " rows x ",
        ncol(snapshot$rv_snapshot$data_mod), " columns."
      )
      restore_validation(result)
      if (is.list(snapshot$module_snapshots$datawizard)) {
        debug_log(paste(
          "Data Wizard load: central_rule_file AFTER file load ->",
          .describe_snapshot_object(snapshot$module_snapshots$datawizard$central_rule_file)
        ), 1)
      }

      # --- Validation passed: proceed with automatic restoration ---
      tryCatch({
        debug_log("Starting session restoration (auto-restore after validation)", 1)

        # Set guard flag before any state changes. The generation token
        # lets delayed onFlushed callbacks from an earlier restore no-op if a
        # second restore starts before they run.
        restore_generation <- isolate((rv$session_restore_generation %||% 0L) + 1L)
        rv$session_restore_generation <- restore_generation
        restore_jobs$start_generation(restore_generation)
        rv$session_restoring <- TRUE
        restore_jobs$set_phase(restore_generation, "HYDRATED")
        set_session_restore_phase(rv, "canonical_data")

        # --- Phase 1: Reset and restore rv fields ---
        setProgress(value = 0.20, message = "Restoring shared data...")
        rv_snapshot <- snapshot$rv_snapshot
        failed_fields <- reset_rv_for_session_restore(rv_snapshot)
        for (field_name in names(rv_snapshot)) {
          # Skip internal/ephemeral fields
          if (field_name %in% restore_runtime_fields) next

          tryCatch({
            rv[[field_name]] <- rv_snapshot[[field_name]]
          }, error = function(e) {
            debug_log(paste("Could not restore rv field:", field_name, "-", e$message), 1)
            failed_fields <<- c(failed_fields, field_name)
          })
        }

        # Report any failed rv fields
        if (length(failed_fields) > 0) {
          debug_log(paste("Failed to restore", length(failed_fields), "rv field(s):",
                          paste(failed_fields, collapse = ", ")), 1)
        }

        .normalize_restore_runtime_logicals(rv)

        # Restore diagnostics are generated by the current module restore pass;
        # clear any values carried by the previous app state or snapshot so
        # telemetry cannot bleed across sequential imports.
        rv$restore_reports <- list()
        rv$restore_diagnostics <- list()

        # --- Phase 2: Restore module snapshots in explicit orchestration phases ---
        failed_modules <- character()
        module_snapshots <- snapshot$module_snapshots
        if (!is.null(session_registry) && is.list(module_snapshots) &&
            length(module_snapshots) > 0L) {
          debug_log(paste("Restoring", length(module_snapshots), "module snapshot(s)"), 1)
          restore_jobs$set_phase(restore_generation, "REPLAYING")
          restore_phases <- c(
            "canonical_data",
            "datawizard_ui",
            "analysis_results",
            "analysis_ui",
            "full_module_state",
            "full_module_plots"
          )
          failed_by_phase <- list()
          for (phase_idx in seq_along(restore_phases)) {
            phase <- restore_phases[[phase_idx]]
            set_session_restore_phase(rv, phase)
            phase_module_snapshots <- switch(phase,
              canonical_data = module_snapshots[intersect(names(module_snapshots), "datawizard")],
              datawizard_ui = module_snapshots[intersect(names(module_snapshots), "datawizard")],
              analysis_results = module_snapshots[intersect(names(module_snapshots), c("go", "gsea"))],
              analysis_ui = module_snapshots[intersect(names(module_snapshots), c("go", "gsea"))],
              full_module_state = module_snapshots[setdiff(names(module_snapshots), c("datawizard", "go", "gsea"))],
              full_module_plots = module_snapshots[setdiff(names(module_snapshots), c("datawizard", "go", "gsea"))],
              module_snapshots
            )
            if (length(phase_module_snapshots) == 0L) next
            phase_failed <- tryCatch(
              session_registry$restore_snapshots(
                phase_module_snapshots,
                phase = phase,
                restore_context_snapshots = module_snapshots,
                progress_fn = function(mod_id, step, total) {
                  phase_frac <- (phase_idx - 1) / length(restore_phases)
                  module_frac <- (step - 1) / max(total, 1L) / length(restore_phases)
                  frac <- 0.30 + (phase_frac + module_frac) * 0.60
                  setProgress(
                    value   = frac,
                    message = paste0("Restoring ", .module_display_name(mod_id), " (", phase, ")...")
                  )
                }
              ),
              error = function(e) {
                log_restore_stage_failure(
                  "Session restore phase failed",
                  e,
                  snapshot,
                  module_id = names(phase_module_snapshots) %||% "unknown",
                  current_phase = phase
                )
                stop(e)
              }
            )
            if (length(phase_failed) > 0L) {
              failed_by_phase[[phase]] <- phase_failed
              failed_modules <- unique(c(failed_modules, phase_failed))
            }
          }
          set_session_restore_phase(rv, "complete")
        }

        restore_reports <- rv$restore_reports %||% list()
        cache_hit_count <- if (is.list(restore_reports) && length(restore_reports) > 0L) {
          sum(vapply(restore_reports, function(rep) isTRUE(rep$cache_hit), logical(1L)))
        } else {
          0L
        }
        gc_report <- snapshot$manifest$gc_report %||% list()
        restore_telemetry <- list(
          cache_hit_on_restore = as.integer(cache_hit_count),
          gc_bytes_reclaimed_estimate = as.numeric(gc_report$removed_bytes_estimate %||% 0),
          rollback_count = as.integer(if (!is.null(gc_report$rollback_reason) && nzchar(gc_report$rollback_reason)) 1L else 0L),
          unknown_kept_count = as.integer(gc_report$unknown_kept_entries %||% 0L)
        )
        rv$restore_telemetry <- restore_telemetry
        session_debug_log(
          sprintf(
            "Session restore telemetry | cache-hit=%d | gc-bytes-reclaimed-est=%s | rollback-count=%d | unknown-kept=%d",
            restore_telemetry$cache_hit_on_restore,
            format(restore_telemetry$gc_bytes_reclaimed_estimate, scientific = FALSE, trim = TRUE),
            restore_telemetry$rollback_count,
            restore_telemetry$unknown_kept_count
          ),
          level = 0
        )

        datawizard_restore_controls_finalization <- is.list(module_snapshots) &&
          is.list(module_snapshots$datawizard) &&
          datawizard_restore_phase_active(rv)

        # Clear guard flag and fire trigger.
        # Defer both the flag flip and the restore-trigger bump until AFTER
        # the current reactive flush finishes so that observers reacting to
        # Phase 1 rv writes still see `rv$session_restoring == TRUE` and bail
        # out via their guards. Without this deferral those observers fire
        # in the same flush that Phase 1 triggered, read the FINAL value of
        # rv$session_restoring (FALSE), and clobber restored state
        # (e.g. rv$data_def) before our restore_fns have a chance to win.
        setProgress(value = 0.95, message = "Finalizing session restoration...")
        finalization_job <- restore_jobs$register_restore_job(
          "session", "final reactive flush", "finalizer", timeout = 15
        )
        if (is.function(session$onFlushed)) {
          session$onFlushed(once = TRUE, function() {
            isolate({
              if (restore_generation_current(restore_generation)) {
                if (datawizard_restore_phase_active(rv) &&
                    !isTRUE(datawizard_restore_controls_finalization)) {
                  .normalize_restore_runtime_logicals(rv)
                  set_session_restore_phase(rv, "complete")
                  rv$session_restoring <- FALSE
                  rv$session_restore_trigger <-
                    (rv$session_restore_trigger %||% 0L) + 1L
                }
              } else {
                debug_log("Session restore: skipped stale finalization callback", 2)
              }
              restore_jobs$resolve_restore_job(finalization_job, "completed")
            })
          })
        } else {
          if (restore_generation_current(restore_generation)) {
            if (datawizard_restore_phase_active(rv) &&
                !isTRUE(datawizard_restore_controls_finalization)) {
              .normalize_restore_runtime_logicals(rv)
              set_session_restore_phase(rv, "complete")
              rv$session_restoring <- FALSE
              rv$session_restore_trigger <- isolate(
                (rv$session_restore_trigger %||% 0L) + 1L
              )
            }
            restore_jobs$resolve_restore_job(finalization_job, "completed")
          }
        }

        # Seal only after every deferred action required by the synchronous
        # transaction has registered its job. Settlement owns user-visible success.
        rv$.restore_failed_fields <- failed_fields
        rv$.restore_failed_modules <- failed_modules
        rv$.restore_module_count <- length(module_snapshots)
        n_modules_restored <- length(module_snapshots) - length(failed_modules)
        restore_jobs$seal_generation(restore_generation)
        imported_file_name <- if (!is.null(file_info$name) &&
                                  is.character(file_info$name) &&
                                  nzchar(file_info$name)) {
          file_info$name
        } else {
          "unknown"
        }
        session_debug_log(
          sprintf(
            "MiraProt session imported | File: %s | Modules restored: %s",
            imported_file_name,
            as.character(n_modules_restored)
          ),
          level = 0
        )

        # The deferred finalization callback only needs scalar generation and
        # phase flags. Drop materialized snapshot/cache bindings now so its
        # closure environment cannot retain a second copy while rendering
        # catches up. Normal R allocation pressure will collect these objects;
        # do not force a global collection during the active render flush.
        if (is.list(snapshot)) {
          snapshot$compatibility_upgrade <- NULL
          snapshot$plot_data_cache_pool <- NULL
          snapshot$plot_data_cache_index <- NULL
        }
        snapshot <- NULL
        rv_snapshot <- NULL
        module_snapshots <- NULL
        plot_data_cache_pool <- NULL
        restored_buf <- NULL
        preprocess_result <- NULL

      }, error = function(e) {
        if (exists("restore_generation", inherits = FALSE) &&
            restore_generation_current(restore_generation)) {
          rv$session_restoring <- FALSE
        }
        debug_log(paste("Session restoration failed:", e$message), 1)
        showNotification(
          paste("Session restoration failed:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  }

  # Expose the same restore path to modules that accept MiraProt .rds files
  # through their own file inputs (for example Data Wizard uploads).
  rv$restore_session_from_file <- restore_session_from_file
  try(session$userData$handle_session_restore_upload <- restore_session_from_file, silent = TRUE)

  observeEvent(input$session_file, {
    restore_session_from_file(input$session_file, source_label = "Session restore panel")
  })

  # ========================================
  # Restore: Validation status output
  # ========================================

  # ========================================
  # Session Debug Log: reactive level-filtered display
  # ========================================
  #
  # Architecture:
  #   log_buffer_version  — reactivePoll that checks the globalenv version
  #                         counter (O(1) integer comparison, no payload copy).
  #   ui_filtered_log_text — reactive that reads input$debug_level_select and
  #                         input$session_log_tag_filter, then subsets only
  #                         the Session-tab presentation text.
  #   output$session_log_display — trivial renderText wrapper.
  #
  # Guarantees:
  #   (a) Lowering the level hides high-level entries immediately without
  #       flushing the buffer.
  #   (b) Raising the level retroactively reveals already-captured entries
  #       that were recorded silently (console gate fires at capture time only;
  #       the display gate fires at render time).
  #   (c) The tag filter is UI-only: it affects the Session-tab display,
  #       clipboard copy, and Session-tab text download without mutating the
  #       original structured log buffers used by other export paths.
  #   (d) No cat() calls inside the reactive chain — no console replay.

  log_buffer_version <- reactivePoll(
    intervalMillis = 500L,
    session        = session,
    checkFunc      = function() {
      v <- get0(".miraprot_log_version", envir = globalenv(), inherits = FALSE)
      if (is.null(v)) 0L else v
    },
    valueFunc      = function() {
      v <- get0(".miraprot_log_version", envir = globalenv(), inherits = FALSE)
      if (is.null(v)) 0L else v
    }
  )

  .empty_session_log_buffer <- function(template = NULL) {
    if (is.data.frame(template)) return(template[0, , drop = FALSE])
    data.frame(
      time = as.POSIXct(character(0)), level = integer(0),
      tag = character(0), message = character(0), line = character(0),
      run_id = character(0), event_id = character(0),
      stringsAsFactors = FALSE
    )
  }

  get_session_log_sections <- function(level) {
    lvl <- suppressWarnings(as.integer(level))
    if (length(lvl) < 1L || is.na(lvl[[1L]])) lvl <- 0L
    lvl <- lvl[[1L]]
    lvl_key <- as.character(max(min(lvl, 2L), 0L))

    # Keep the restored and live buffers separate until the last text assembly
    # step.  This prevents one UI/export/copy path from serializing a combined
    # buffer and then appending the same DOM/rendered text again.
    live_buffers <- get0(".miraprot_log_buffers", envir = globalenv(), inherits = FALSE)
    live_buf <- if (is.list(live_buffers)) live_buffers[[lvl_key]] else NULL
    if (!is.data.frame(live_buf)) live_buf <- .empty_session_log_buffer()

    restored <- session_log_content()
    restored_buf <- if (is.list(restored)) restored[[lvl_key]] else NULL

    list(restored = restored_buf, live = live_buf)
  }

  filter_session_log_buffer <- function(buf, selected_tag) {
    if (!is.data.frame(buf)) return(buf)
    if (!nzchar(selected_tag)) return(buf)

    if ("tag" %in% names(buf)) {
      buf <- buf[!is.na(buf$tag) & as.character(buf$tag) == selected_tag, , drop = FALSE]
    } else if ("line" %in% names(buf)) {
      line_tag <- sub("^\\[ ([^]]+?) [0-9]{2}:[0-9]{2}:[0-9]{2} \\].*$", "\\1", as.character(buf$line), perl = TRUE)
      buf <- buf[!is.na(line_tag) & line_tag == selected_tag, , drop = FALSE]
    }
    buf
  }

  session_log_separator <- paste(
    "----- Restored session log above; current live session log below -----"
  )

  session_log_record_key <- function(buf, source_block) {
    if (!is.data.frame(buf) || nrow(buf) == 0L) return(character())

    event_id <- if ("event_id" %in% names(buf)) trimws(as.character(buf$event_id)) else
      rep("", nrow(buf))
    has_event_id <- !is.na(event_id) & nzchar(event_id)
    source_col <- rep(as.character(source_block %||% ""), nrow(buf))
    module_col <- if ("tag" %in% names(buf)) {
      as.character(buf$tag)
    } else {
      rep(NA_character_, nrow(buf))
    }

    time_col <- if ("time" %in% names(buf)) {
      if (inherits(buf$time, "POSIXt")) {
        sprintf("%.6f", as.numeric(buf$time))
      } else {
        as.character(buf$time)
      }
    } else {
      rep(NA_character_, nrow(buf))
    }

    message_col <- if ("message" %in% names(buf)) {
      as.character(buf$message)
    } else {
      rep(NA_character_, nrow(buf))
    }

    if ("line" %in% names(buf)) {
      line_col <- as.character(buf$line)

      missing_module <- is.na(module_col) | !nzchar(module_col)
      if (any(missing_module)) {
        parsed_module <- sub("^\\[\\s*([^]]+?)\\s+(?:[0-9]{2}:[0-9]{2}:[0-9]{2})?\\s*\\].*$", "\\1", line_col, perl = TRUE)
        parsed_module[parsed_module == line_col] <- NA_character_
        module_col[missing_module] <- trimws(parsed_module[missing_module])
      }

      missing_time <- is.na(time_col) | !nzchar(time_col)
      if (any(missing_time)) {
        parsed_time <- sub("^\\[\\s*.*?\\s+([0-9]{2}:[0-9]{2}:[0-9]{2})\\s*\\].*$", "\\1", line_col, perl = TRUE)
        parsed_time[parsed_time == line_col] <- NA_character_
        time_col[missing_time] <- parsed_time[missing_time]
      }

      missing_message <- is.na(message_col) | !nzchar(message_col)
      if (any(missing_message)) {
        parsed_message <- sub("^\\[\\s*.*?\\s+(?:[0-9]{2}:[0-9]{2}:[0-9]{2})?\\s*\\]\\s*", "", line_col, perl = TRUE)
        message_col[missing_message] <- parsed_message[missing_message]
      }
    }

    legacy_key <- paste(
      ifelse(is.na(source_col), "", trimws(source_col)),
      ifelse(is.na(module_col), "", trimws(module_col)),
      ifelse(is.na(time_col), "", trimws(time_col)),
      ifelse(is.na(message_col), "", trimws(message_col)),
      sep = "\r"
    )
    ifelse(has_event_id, paste0("event\r", event_id), legacy_key)
  }

  dedupe_session_log_buffer <- function(buf, source_block = "") {
    if (!is.data.frame(buf) || nrow(buf) == 0L) return(buf)
    key <- session_log_record_key(buf, source_block)
    buf[!duplicated(key), , drop = FALSE]
  }

  observe({
    log_buffer_version()   # take a dependency on the version counter

    lvl <- input$debug_level_select
    sections <- get_session_log_sections(lvl)

    known_tags <- c("MAIN APP", "FILE LOADER", "LAUNCHER")
    tags <- character()
    for (buf in sections) {
      if (is.data.frame(buf) && "tag" %in% names(buf)) {
        buf_tags <- unique(trimws(as.character(buf$tag)))
        buf_tags <- buf_tags[!is.na(buf_tags) & nzchar(buf_tags)]
        tags <- c(tags, buf_tags)
      }
    }
    tags <- sort(unique(c(known_tags, tags)), method = "radix")

    choices <- c("All debug calls" = "", stats::setNames(tags, tags))
    selected <- isolate(input$session_log_tag_filter)
    selected <- if (is.character(selected) && length(selected) >= 1L) {
      selected[[1L]]
    } else {
      NA_character_
    }

    if (is.null(selected) || length(selected) == 0L || is.na(selected) ||
        identical(selected, "All debug calls") || !(selected %in% unname(choices))) {
      selected <- ""
    }

    updateSelectInput(
      session,
      "session_log_tag_filter",
      choices = choices,
      selected = selected
    )
  })

  authoritative_session_log_buffer <- function(level, selected_tag) {
    lvl <- level
    selected_tag <- if (is.character(selected_tag) && length(selected_tag) >= 1L) {
      selected_tag[[1L]]
    } else {
      NA_character_
    }

    if (is.null(selected_tag) || length(selected_tag) == 0L || is.na(selected_tag) ||
        identical(selected_tag, "All debug calls")) {
      selected_tag <- ""
    }
    has_tag_filter <- nzchar(selected_tag)

    sections <- get_session_log_sections(lvl)
    restored_buf <- dedupe_session_log_buffer(
      filter_session_log_buffer(sections$restored, selected_tag),
      source_block = "restored"
    )
    live_buf <- dedupe_session_log_buffer(
      filter_session_log_buffer(sections$live, selected_tag),
      source_block = "live"
    )

    # Build one authoritative vector of stored records.  UI rendering,
    # clipboard copy, and text download all serialize this same vector instead
    # of mixing already-rendered text with the backing log buffers.
    buffer_lines <- function(buf) {
      if (!is.data.frame(buf) || nrow(buf) == 0L || !("line" %in% names(buf))) return(character())
      lines <- as.character(buf$line)
      lines[!is.na(lines) & nzchar(trimws(lines))]
    }

    restored_lines <- buffer_lines(restored_buf)
    live_lines <- buffer_lines(live_buf)

    if (is.data.frame(restored_buf) && is.data.frame(live_buf) &&
        "event_id" %in% names(restored_buf) && "event_id" %in% names(live_buf)) {
      restored_ids <- trimws(as.character(restored_buf$event_id))
      live_ids <- trimws(as.character(live_buf$event_id))
      duplicate_event <- !is.na(live_ids) & nzchar(live_ids) &
        live_ids %in% restored_ids[!is.na(restored_ids) & nzchar(restored_ids)]
      live_buf <- live_buf[!duplicate_event, , drop = FALSE]
      live_lines <- buffer_lines(live_buf)
    }

    # The restored and live buffers are concatenated exactly once here.  Do not
    # feed rendered UI text back into this vector; repeated source rows have
    # already been removed by source/module/timestamp/message tuple above, while
    # legitimate repeated messages with different timestamps remain intact.
    combined_lines <- c(
      restored_lines,
      if (length(restored_lines) > 0L && length(live_lines) > 0L) session_log_separator else character(),
      live_lines
    )
    if (length(combined_lines) > 0L) return(combined_lines)

    # Fall back to legacy character vectors (portable-mode file logs restored
    # as character) or file-based log for portable mode with no buffer.
    lvl_int <- suppressWarnings(as.integer(lvl))
    if (length(lvl_int) < 1L || is.na(lvl_int[[1L]])) lvl_int <- 0L
    lvl_key <- as.character(max(min(lvl_int[[1L]], 2L), 0L))
    restored <- session_log_content()
    restored_chr <- if (is.list(restored)) restored[[lvl_key]] else NULL
    if (is.character(restored_chr) && length(restored_chr) > 0L) {
      if (has_tag_filter) {
        line_tag <- sub("^\\[ ([^]]+?) [0-9]{2}:[0-9]{2}:[0-9]{2} \\].*$", "\\1", restored_chr, perl = TRUE)
        restored_chr <- restored_chr[!is.na(line_tag) & line_tag == selected_tag]
        if (length(restored_chr) == 0L) return("(No matching log entries.)")
      }
      return(restored_chr)
    }

    log_dir <- Sys.getenv("MIRAPROT_LOG_DIR", "")
    if (nzchar(log_dir)) {
      log_file <- file.path(
        log_dir,
        paste0("miraprot-", format(Sys.time(), "%Y-%m-%d"), ".log")
      )
      if (!file.exists(log_file)) return(paste("(Log file not found:", log_file, ")"))
      return(tryCatch({
        lines <- readLines(log_file, warn = FALSE)
        if (has_tag_filter) {
          line_tag <- sub("^\\[ ([^]]+?) [0-9]{2}:[0-9]{2}:[0-9]{2} \\].*$", "\\1", lines, perl = TRUE)
          lines <- lines[!is.na(line_tag) & line_tag == selected_tag]
          if (length(lines) == 0L) return("(No matching log entries.)")
        }
        lines
      }, error = function(e) paste("(Could not read log file:", e$message, ")")))
    }

    if (has_tag_filter) return("(No matching log entries.)")
    "(No log entries yet.)"
  }

  serialize_session_log_buffer <- function(records) {
    join_records <- get0(".miraprot_log_records_to_text", envir = globalenv(), inherits = FALSE)
    if (is.function(join_records)) join_records(records) else paste(records, collapse = "\n")
  }

  authoritative_session_log_text <- function(level, selected_tag) {
    serialize_session_log_buffer(authoritative_session_log_buffer(level, selected_tag))
  }

  ui_filtered_log_text <- reactive({
    log_buffer_version()   # take a dependency on the version counter
    authoritative_session_log_text(
      level = input$debug_level_select,
      selected_tag = input$session_log_tag_filter
    )
  })

  session_log_export_text <- function() {
    # Central copy/download path: read the authoritative structured log source
    # exactly once, then normalize the serialized text edge.  This deliberately
    # bypasses output$session_log_display and the browser DOM so exported text is
    # never a concatenation of rendered UI text plus the backing log buffer.
    records <- authoritative_session_log_buffer(
      level = input$debug_level_select,
      selected_tag = input$session_log_tag_filter
    )
    normalize_session_log_export_text(serialize_session_log_buffer(records))
  }

  output$session_log_display <- renderText({ ui_filtered_log_text() })

  normalize_session_log_export_text <- function(text) {
    text <- paste(as.character(text %||% ""), collapse = "\n")
    if (nzchar(text) && !grepl("\n$", text)) paste0(text, "\n") else text
  }

  output$session_log_download <- downloadHandler(
    filename = function() {
      paste0("miraprot-session-log-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt")
    },
    content = function(file) {
      log_text <- session_log_export_text()
      writeBin(charToRaw(enc2utf8(log_text)), con = file, useBytes = TRUE)
    }
  )

  copy_session_log_to_clipboard <- function(text) {
    # Pass exactly the server-rendered log string to JavaScript.  The copy path
    # intentionally does not read from #session_log_display, so it cannot append
    # browser DOM text to the already serialized buffer.
    js_text <- jsonlite::toJSON(paste(text, collapse = "\n"), auto_unbox = TRUE)

    shinyjs::runjs(paste0('
      (function() {
        var text = ', js_text, ';
        var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
        var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

        function fallbackCopy(copyText) {
          var textArea = document.createElement("textarea");
          textArea.value = copyText;
          textArea.style.position = "fixed";
          textArea.style.left = "-999999px";
          textArea.style.top = "-999999px";
          textArea.style.width = "2em";
          textArea.style.height = "2em";
          textArea.style.padding = "0";
          textArea.style.border = "none";
          textArea.style.outline = "none";
          textArea.style.boxShadow = "none";
          textArea.style.background = "transparent";
          textArea.style.opacity = "0";
          textArea.style.pointerEvents = "none";
          document.body.appendChild(textArea);
          textArea.focus();
          textArea.select();
          try {
            document.execCommand("copy");
          } catch (err) {
            console.error("Fallback clipboard copy failed:", err);
          }
          document.body.removeChild(textArea);
          window.scrollTo(currentScrollLeft, currentScrollTop);
        }

        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function() {
            console.log("Session log clipboard copy successful");
          }, function(err) {
            console.error("navigator.clipboard failed, using fallback:", err);
            fallbackCopy(text);
          });
        } else {
          fallbackCopy(text);
        }
      })();
    '))
  }

  observeEvent(input$session_log_copy, {
    log_text <- session_log_export_text()
    copy_session_log_to_clipboard(log_text)
    showNotification("Session log copied to clipboard.", type = "message", duration = 2)
  })

  observeEvent(input$session_deep_cleanup_shutdown, {
    showNotification(
      "Running deep cleanup and closing app...",
      type = "message",
      duration = 4
    )

    old_close_connections <- getOption("miraprot.cleanup.close_connections", FALSE)
    old_run_gc <- getOption("miraprot.cleanup.run_gc", FALSE)

    options(
      miraprot.cleanup.close_connections = TRUE,
      miraprot.cleanup.run_gc = TRUE
    )

    on.exit(
      options(
        miraprot.cleanup.close_connections = old_close_connections,
        miraprot.cleanup.run_gc = old_run_gc
      ),
      add = TRUE
    )

    tryCatch({
      withProgress(message = "Closing app with deep cleanup...", value = 0, {
        setProgress(value = 0.10, detail = "Enabling deep cleanup options")
        setProgress(value = 0.30, detail = "Running centralized cleanup")
        cleanup_manager$execute_cleanup(mode = "app_stop")
        setProgress(value = 0.80, detail = "Removing session lock")
        remove_session_lock_file()
        setProgress(value = 1.00, detail = "Stopping app")
      })
    }, error = function(e) {
      debug_log(paste("Deep cleanup shutdown failed:", e$message), 1)
    })

    later::later(shiny::stopApp, delay = 0)
  })

  output$session_restore_status <- renderUI({
    val <- restore_validation()
    if (is.null(val)) return(NULL)

    if (val$valid) {
      tags$div(
        class = "alert alert-success",
        style = "margin-top: 10px;",
        tags$strong("Validation passed: "),
        val$message
      )
    } else {
      tags$div(
        class = "alert alert-danger",
        style = "margin-top: 10px;",
        tags$strong("Validation failed: "),
        val$message
      )
    }
  })

  invisible(restore_jobs[c(
    "register_restore_job", "resolve_restore_job", "outstanding_restore_jobs"
  )])
}
