# ============================================================================
# Sub-script: R/session_save_restore/session_save_restore_module_registration.R
# Purpose:
#   Register all module session participants and preserve backward-compatible
#   module save/restore behavior across session save levels.
#
# Architectural Role:
#   integration adapter / module contract layer
#
# Responsibilities:
#   - Define register_module_session_participants() and module contracts.
#   - Preserve module restore priority ordering and feature parity.
#   - Keep module-specific snapshot save/restore logic centralized and stable.
#
# Non-Responsibilities (Must NOT be here):
#   - Wire upload/download Shiny handlers.
#   - Define low-level serialization algorithms.
# ============================================================================

.ensure_datawizard_session_dependency <- function(function_name, source_path) {
  is_available <- function(env = parent.frame()) {
    exists(function_name, envir = env, mode = "function", inherits = TRUE)
  }
  if (is_available(parent.env(environment()))) {
    return(TRUE)
  }

  # Most Data Wizard helpers are sourced into modEnv by the normal module loader.
  # Prefer that already-loaded copy before private sys.source() fallback so startup
  # logs do not report benign missing-dependency fallbacks at debug level 1 and so
  # registration avoids re-sourcing large Data Wizard files.
  module_env <- get0("modEnv", envir = globalenv(), inherits = TRUE, ifnotfound = NULL)
  if (is.environment(module_env) &&
      exists(function_name, envir = module_env, mode = "function", inherits = TRUE)) {
    assign(function_name, get(function_name, envir = module_env), envir = parent.env(environment()))
    debug_log(paste(
      "Data Wizard session dependency", paste0(function_name, "()"),
      "resolved from loaded module environment"
    ), 2)
    return(TRUE)
  }

  if (!file.exists(source_path)) {
    debug_log(paste(
      "Data Wizard session dependency fallback skipped; source file missing:",
      source_path
    ), 1)
    return(FALSE)
  }

  debug_log(paste(
    "Data Wizard session dependency", paste0(function_name, "()"),
    "not found in loaded module environment; using private source fallback from", source_path
  ), 2)

  fallback_env <- new.env(parent = parent.frame())
  fallback_ok <- tryCatch({
    sys.source(source_path, envir = fallback_env)
    exists(function_name, envir = fallback_env, mode = "function", inherits = FALSE)
  }, error = function(e) {
    debug_log(paste(
      "Data Wizard session dependency fallback failed for",
      paste0(function_name, ":"), e$message
    ), 1)
    FALSE
  })

  if (isTRUE(fallback_ok)) {
    assign(function_name, get(function_name, envir = fallback_env), envir = parent.env(environment()))
    debug_log(paste(
      "Data Wizard session dependency", paste0(function_name, "()"),
      "loaded via private source fallback"
    ), 2)
  }

  rm(fallback_env)
  exists(function_name, mode = "function")
}

.get_datawizard_session_dependency_status <- function() {
  status <- c(
    create_primary_data_state_adapter = exists("create_primary_data_state_adapter", mode = "function"),
    metadata_matches_dataset = exists("metadata_matches_dataset", mode = "function"),
    is_meaningful_metadata = exists("is_meaningful_metadata", mode = "function"),
    restore_has_valid_canonical_pair = exists("restore_has_valid_canonical_pair", mode = "function")
  )

  if (!isTRUE(status[["create_primary_data_state_adapter"]])) {
    status[["create_primary_data_state_adapter"]] <-
      .ensure_datawizard_session_dependency(
        "create_primary_data_state_adapter",
        "modules/Data Wizard/datawizard_core.R"
      )
  }
  for (fn in c("metadata_matches_dataset", "is_meaningful_metadata", "restore_has_valid_canonical_pair")) {
    if (!isTRUE(status[[fn]])) {
      status[[fn]] <- .ensure_datawizard_session_dependency(
        fn,
        "modules/Data Wizard/datawizard_utils.R"
      )
    }
  }

  status <- c(
    create_primary_data_state_adapter = exists("create_primary_data_state_adapter", mode = "function"),
    metadata_matches_dataset = exists("metadata_matches_dataset", mode = "function"),
    is_meaningful_metadata = exists("is_meaningful_metadata", mode = "function"),
    restore_has_valid_canonical_pair = exists("restore_has_valid_canonical_pair", mode = "function")
  )
  debug_log(paste(
    "Data Wizard session dependency status:",
    paste(paste0(names(status), "=", ifelse(status, "available", "missing")),
          collapse = ", ")
  ), 1)
  status
}

.heatmap_has_valid_restore_cache_dependency <- function(module_state) {
  if (!is.list(module_state)) return(FALSE)
  declared <- module_state$restore_cache_dependency
  is.character(declared) && length(declared) == 1L &&
    !is.na(declared) && nzchar(declared) &&
    declared %in% SESSION_RESTORE_CACHE_DEPENDENCIES
}

.heatmap_registration_restore_cache_dependency <- function(module_state) {
  if (!is.list(module_state)) module_state <- list()
  if (.heatmap_has_valid_restore_cache_dependency(module_state)) {
    return(module_state$restore_cache_dependency)
  }

  valid_matrix <- function(x) {
    is.matrix(x) && length(dim(x)) == 2L && all(dim(x) > 0L)
  }
  matrix_payload <- module_state$matrix_payload
  matrix_candidates <- c(
    module_state[c(
      "heatmap_expression_matrix",
      "heatmap_protein_cor_matrix",
      "heatmap_sample_cor_matrix"
    )],
    if (is.list(matrix_payload)) matrix_payload[c(
      "expression_matrix",
      "protein_cor_matrix",
      "sample_cor_matrix"
    )] else list()
  )
  if (any(vapply(matrix_candidates, valid_matrix, logical(1)))) {
    return("module_matrix_payload")
  }

  rebuild_flags <- c(
    module_state$had_heatmap,
    module_state$rebuild_requested,
    module_state$restore_rebuild_requested,
    module_state$pending_had_heatmap
  )
  has_rebuild_intent <- any(vapply(rebuild_flags, isTRUE, logical(1))) ||
    (is.list(module_state$plot_request) && length(module_state$plot_request) > 0L)
  if (has_rebuild_intent) {
    return("shared_plot_data_cache_pool")
  }

  "none"
}

# Registration must not turn the mere presence of a module into plot restore
# intent.  Module-owned declarations win; older module states are upgraded only
# when one of that module's durable, type-checked intent fields says that a plot
# actually existed.
.registration_module_has_restore_intent <- function(module_id, module_state) {
  if (!is.list(module_state)) return(FALSE)
  nonempty_list <- function(x) is.list(x) && length(x) > 0L
  nonempty_matrix <- function(x) is.matrix(x) && length(dim(x)) == 2L && all(dim(x) > 0L)

  switch(module_id,
    sampleids = isTRUE(module_state$had_plot),
    pca = isTRUE(module_state$had_plot) || isTRUE(module_state$plots_ready) ||
      nonempty_list(module_state$analysis_result) ||
      any(vapply(module_state[c("sample_pca_results", "protein_pca_results",
                                "sample_umap_results", "protein_umap_results")],
                 nonempty_list, logical(1))),
    volcano = isTRUE(module_state$had_static_plots) ||
      length(module_state$plot_titles %||% character()) > 0L ||
      nonempty_list(module_state$plot_requests_by_title),
    dotplot = isTRUE(module_state$plot_ready),
    venn = isTRUE(module_state$had_plot) || isTRUE(module_state$plot_active),
    abundances = isTRUE(module_state$had_plot),
    heatmap = isTRUE(module_state$had_heatmap) ||
      any(vapply(module_state[c("heatmap_expression_matrix",
                                "heatmap_protein_cor_matrix",
                                "heatmap_sample_cor_matrix")],
                 nonempty_matrix, logical(1))) ||
      (is.list(module_state$matrix_payload) &&
         any(vapply(module_state$matrix_payload[c("expression_matrix",
                                                  "protein_cor_matrix",
                                                  "sample_cor_matrix")],
                    nonempty_matrix, logical(1)))),
    FALSE
  )
}

.registration_restore_cache_dependency <- function(state, module_id) {
  if (!is.list(state)) state <- list()
  module_state <- if (is.list(state$module_state)) state$module_state else list()
  declared <- module_state$restore_cache_dependency
  declared_valid <- is.character(declared) && length(declared) == 1L &&
    !is.na(declared) && nzchar(declared) &&
    declared %in% SESSION_RESTORE_CACHE_DEPENDENCIES

  dependency <- if (declared_valid) {
    declared
  } else if (.registration_module_has_restore_intent(module_id, module_state)) {
    if (identical(module_id, "heatmap")) {
      .heatmap_registration_restore_cache_dependency(module_state)
    } else {
      "shared_plot_data_cache_pool"
    }
  } else {
    "none"
  }

  module_state <- .set_module_restore_cache_dependency(module_state, dependency)
  if (identical(dependency, "none")) {
    module_state$plot_data_cache_ref <- NULL
    module_state$plot_cache_ref_by_title <- NULL
    module_state$plot_data_cache_payload <- NULL
    module_state$restore_plot_data_cache <- NULL
  }
  state$module_state <- module_state
  state
}

.register_degraded_datawizard_session_participant <- function(session_registry,
                                                             save_level,
                                                             rv = NULL) {
  can_restore_rv <- !is.null(rv)
  if (!can_restore_rv) {
    debug_log(paste(
      "Data Wizard degraded session participant not registered;",
      "shared rv is unavailable, so canonical data tables cannot be safely restored"
    ), 1)
    return(FALSE)
  }

  session_registry$register(
    module_id = "datawizard",
    priority = 10L,
    save_level = save_level,
    save_fn = function() {
      list(
        version = "1.0-degraded",
        degraded = TRUE,
        data_mod = .safe_rv_read_from(rv, "data_mod"),
        data_def = .safe_rv_read_from(rv, "data_def"),
        primary_data_raw = .safe_rv_read_from(rv, "primary_data_raw")
      )
    },
    restore_fn = function(state, phase = NULL) {
      if (is.null(state)) return()
      if (!is.null(phase) && !identical(phase, "canonical_data")) return()
      rv_writes <- list(
        data_mod = state$data_mod,
        data_def = state$data_def,
        primary_data_raw = state$primary_data_raw %||% state$primary_data_raw_rv
      )
      rv_writes <- rv_writes[!vapply(rv_writes, is.null, logical(1))]
      for (field in names(rv_writes)) {
        tryCatch(isolate({ rv[[field]] <- rv_writes[[field]] }), error = function(e) {
          debug_log(paste0(
            "Degraded Data Wizard restore: rv$", field, " restore failed: ",
            e$message
          ), 1)
        })
      }
      debug_log(paste(
        "Degraded Data Wizard session state restored; rv fields:",
        paste(names(rv_writes), collapse = ", ")
      ), 1)
    }
  )
  debug_log(paste(
    "Data Wizard registered as degraded session participant;",
    "restore limited to rv$data_mod, rv$data_def, and rv$primary_data_raw"
  ), 1)
  TRUE
}


.datawizard_snapshot_pool_ref <- function(pool_id, role) {
  list(ref = pool_id, role = role)
}

.is_datawizard_snapshot_pool_ref <- function(x) {
  is.list(x) && is.character(x$ref) && length(x$ref) == 1L &&
    is.character(x$role) && length(x$role) == 1L &&
    setequal(names(x), c("ref", "role"))
}

.datawizard_snapshot_digest <- function(object, serialize = TRUE) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(object, algo = "xxhash64", serialize = serialize))
  }
  raw <- serialize(object, NULL, xdr = FALSE)
  paste0(length(raw), "-", sum(as.integer(raw)) %% 1000000007L)
}

.datawizard_snapshot_pool_add <- function(pool, object, role, cache_tag = NULL) {
  if (!is.data.frame(object)) {
    return(list(pool = pool, ref = object, pool_id = NULL))
  }
  if (is.null(pool) || !is.list(pool)) pool <- list()
  if (is.null(pool$objects)) pool$objects <- list()
  if (is.null(pool$metadata)) pool$metadata <- list()
  if (is.null(pool$signatures)) pool$signatures <- list()

  for (existing_id in names(pool$objects)) {
    if (identical(pool$objects[[existing_id]], object)) {
      existing_meta <- pool$metadata[[existing_id]] %||% list()
      existing_meta$roles <- unique(c(existing_meta$roles %||% existing_meta$role, role))
      existing_meta$cache_tags <- c(existing_meta$cache_tags %||% list(), list(cache_tag))
      pool$metadata[[existing_id]] <- existing_meta
      return(list(pool = pool, ref = .datawizard_snapshot_pool_ref(existing_id, role), pool_id = existing_id))
    }
  }

  dims <- as.integer(dim(object))
  columns <- names(object)
  cache_tag_chr <- as.character(unlist(cache_tag, recursive = TRUE, use.names = TRUE) %||% character(0))
  stable_parts <- list(
    dims = dims,
    columns = columns,
    cache_tag = cache_tag_chr
  )
  stable_signature <- .datawizard_snapshot_digest(stable_parts, serialize = TRUE)

  candidates <- names(pool$signatures)[vapply(pool$signatures, identical, logical(1), stable_signature)]
  for (candidate_id in candidates) {
    if (identical(pool$objects[[candidate_id]], object)) {
      return(list(pool = pool, ref = .datawizard_snapshot_pool_ref(candidate_id, role), pool_id = candidate_id))
    }
  }

  content_hash <- NULL
  pool_id <- paste("df", stable_signature, sep = "_")
  if (!is.null(pool$objects[[pool_id]]) && !identical(pool$objects[[pool_id]], object)) {
    content_hash <- .datawizard_snapshot_digest(object, serialize = TRUE)
    pool_id <- paste("df", stable_signature, content_hash, sep = "_")
  }
  suffix <- 1L
  base_pool_id <- pool_id
  while (!is.null(pool$objects[[pool_id]]) && !identical(pool$objects[[pool_id]], object)) {
    if (is.null(content_hash)) content_hash <- .datawizard_snapshot_digest(object, serialize = TRUE)
    suffix <- suffix + 1L
    pool_id <- paste(base_pool_id, suffix, sep = "_")
  }

  if (is.null(pool$objects[[pool_id]])) {
    pool$objects[[pool_id]] <- object
    pool$metadata[[pool_id]] <- list(
      role = role,
      roles = role,
      dims = dims,
      columns = columns,
      cache_tag = cache_tag,
      cache_tags = list(cache_tag),
      stable_signature = stable_signature,
      content_hash = content_hash
    )
    pool$signatures[[pool_id]] <- stable_signature
  }
  list(pool = pool, ref = .datawizard_snapshot_pool_ref(pool_id, role), pool_id = pool_id)
}

.datawizard_snapshot_pool_cache_tag <- function(state, role) {
  if (!is.list(state)) return(NULL)
  switch(role,
    data_mod = list(
      data_mod_revision_id = state$data_mod_revision_id,
      datawizard_data_revision_id = state$datawizard_data_revision_id
    ),
    primary_data_raw = list(
      primary_raw_revision = state$primary_raw_revision,
      data_mod_revision_id = state$data_mod_revision_id
    ),
    primary_data_raw_rv = list(
      primary_raw_revision = state$primary_raw_revision,
      data_mod_revision_id = state$data_mod_revision_id
    ),
    final_processed_data = list(
      primary_working_revision = state$primary_working_revision,
      data_mod_revision_id = state$data_mod_revision_id
    ),
    loader_data_fixed = .loader_cache_tag_count(state$loader_state),
    loader_primary_data_original = .loader_cache_tag_count(state$loader_state),
    submodule_loader_data_fixed = .loader_cache_tag_count(state$loader_state),
    submodule_loader_primary_data_original = .loader_cache_tag_count(state$loader_state),
    NULL
  )
}

.datawizard_snapshot_pool_compact <- function(state) {
  if (!is.list(state)) return(state)
  pool <- state$data_frame_pool %||% list()
  add_path <- function(container, path, role) {
    if (!is.list(container) || length(path) == 0L) return(container)
    key <- path[[1L]]
    if (length(path) == 1L) {
      pooled <- .datawizard_snapshot_pool_add(
        pool,
        container[[key]],
        role = role,
        cache_tag = .datawizard_snapshot_pool_cache_tag(state, role)
      )
      pool <<- pooled$pool
      container[[key]] <- pooled$ref
      return(container)
    }
    container[[key]] <- add_path(container[[key]], path[-1L], role)
    container
  }
  candidates <- list(
    list(path = c("primary_data_raw"), role = "primary_data_raw"),
    list(path = c("primary_data_raw_rv"), role = "primary_data_raw_rv"),
    list(path = c("data_mod"), role = "data_mod"),
    list(path = c("final_processed_data"), role = "final_processed_data"),
    list(path = c("loader_state", "data_fixed"), role = "loader_data_fixed"),
    list(path = c("loader_state", "primary_data_original"), role = "loader_primary_data_original"),
    list(path = c("submodule_ui_states", "submodules", "loader_out", "data_fixed"), role = "submodule_loader_data_fixed"),
    list(path = c("submodule_ui_states", "submodules", "loader_out", "primary_data_original"), role = "submodule_loader_primary_data_original")
  )
  for (candidate in candidates) {
    state <- add_path(state, candidate$path, candidate$role)
  }
  if (length(pool$objects %||% list()) > 0L) {
    state$data_frame_pool <- pool
  }
  state
}

.datawizard_snapshot_pool_resolve <- function(state) {
  if (!is.list(state) || is.null(state$data_frame_pool)) return(state)
  objects <- state$data_frame_pool$objects %||% state$data_frame_pool
  resolve <- function(x) {
    if (.is_datawizard_snapshot_pool_ref(x)) {
      pooled <- objects[[x$ref]]
      if (is.null(pooled)) {
        debug_log(paste("Data Wizard restore: missing data_frame_pool ref", x$ref, "for role", x$role), 1)
        return(NULL)
      }
      return(pooled)
    }
    if (is.list(x)) {
      for (nm in names(x)) x[[nm]] <- resolve(x[[nm]])
    }
    x
  }
  resolve(state)
}

.datawizard_loader_reference <- function() {
  list(version = "1.0", ref = "loader_state")
}

.is_datawizard_loader_reference <- function(x) {
  is.list(x) && identical(x$ref, "loader_state")
}

.resolve_datawizard_loader_state <- function(st, submodule_payload = NULL) {
  loader_state <- if (is.list(st)) st$loader_state else NULL
  if (!is.null(loader_state) && !.is_datawizard_loader_reference(loader_state)) {
    return(loader_state)
  }
  if (is.null(submodule_payload) && is.list(st)) {
    submodule_payload <- st$submodule_ui_states %||% st$submodule_states %||% st$ui_submodule_states
  }
  sub_loader <- NULL
  if (is.list(submodule_payload)) {
    sub_loader <- submodule_payload$submodules$loader_out %||% submodule_payload$loader_out
  }
  if (.is_datawizard_loader_reference(sub_loader)) {
    return(if (is.list(st)) st$loader_state else NULL)
  }
  sub_loader
}

.loader_cache_tag_count <- function(loader_state) {
  if (!is.list(loader_state)) return(0L)
  tag_fields <- c("cache_tags", "cache_tag", "primary_cache_tags", "secondary_cache_tags")
  explicit <- unlist(loader_state[intersect(names(loader_state), tag_fields)], recursive = TRUE, use.names = FALSE)
  explicit <- explicit[!is.na(explicit) & nzchar(as.character(explicit))]
  if (length(explicit) > 0L) return(length(unique(as.character(explicit))))
  cache_fields <- c("sheet_cache_primary", "sheet_cache_secondary", "all_sheets_primary", "all_sheets_secondary")
  sum(vapply(loader_state[intersect(names(loader_state), cache_fields)], function(x) {
    if (is.list(x)) length(x) else 0L
  }, integer(1)))
}

.loader_data_dimensions <- function(loader_state) {
  fields <- c("data_fixed", "data2_fixed", "primary_data_original", "secondary_data_original")
  dims <- lapply(fields, function(field) {
    x <- if (is.list(loader_state)) loader_state[[field]] else NULL
    if (is.data.frame(x) || is.matrix(x)) as.integer(dim(x)) else NULL
  })
  names(dims) <- fields
  dims
}

.assert_datawizard_loader_snapshot_integrity <- function(before_loader_state, snapshot_state, after_loader_state = NULL) {
  after_loader_state <- after_loader_state %||% .resolve_datawizard_loader_state(snapshot_state)
  sub_loader <- if (is.list(snapshot_state$submodule_ui_states)) {
    snapshot_state$submodule_ui_states$submodules$loader_out %||% snapshot_state$submodule_ui_states$loader_out
  } else {
    NULL
  }
  duplicate_payload <- is.list(snapshot_state$loader_state) && is.list(sub_loader) &&
    !.is_datawizard_loader_reference(sub_loader) && identical(snapshot_state$loader_state, sub_loader)
  before_tags <- .loader_cache_tag_count(before_loader_state)
  after_tags <- .loader_cache_tag_count(after_loader_state)
  before_dims <- .loader_data_dimensions(before_loader_state)
  after_dims <- .loader_data_dimensions(after_loader_state)
  dims_equal <- identical(before_dims, after_dims)
  valid <- !isTRUE(duplicate_payload) && identical(before_tags, after_tags) && isTRUE(dims_equal)
  if (!valid) {
    debug_log(paste0(
      "Data Wizard loader snapshot integrity warning:",
      " duplicate_payload=", duplicate_payload,
      " cache_tags_before=", before_tags,
      " cache_tags_after=", after_tags,
      " dims_equal=", dims_equal
    ), 1)
  }
  invisible(valid)
}

assert_datawizard_data_only_restore_invariants <- function(rv, strict = FALSE) {
  diagnostics <- list(
    timestamp = Sys.time(),
    strict = isTRUE(strict),
    data_mod_is_data_frame = FALSE,
    data_def_is_data_frame = FALSE,
    data_def_has_column = FALSE,
    metadata_columns_match_data = FALSE,
    metadata_meaningful = FALSE,
    handson_metadata_meaningful = FALSE,
    final_processed_metadata_meaningful = FALSE,
    meaningful_metadata_source = NA_character_,
    valid = FALSE,
    failures = character(0)
  )

  data_mod <- tryCatch(isolate(rv$data_mod), error = function(e) NULL)
  data_def <- tryCatch(isolate(rv$data_def), error = function(e) NULL)
  handson_metadata <- tryCatch(isolate(rv$handson_metadata), error = function(e) NULL)
  final_processed_metadata <- tryCatch(isolate(rv$final_processed_metadata), error = function(e) NULL)

  diagnostics$data_mod_is_data_frame <- is.data.frame(data_mod)
  diagnostics$data_def_is_data_frame <- is.data.frame(data_def)
  diagnostics$data_def_has_column <- is.data.frame(data_def) && "Column" %in% names(data_def)
  diagnostics$metadata_columns_match_data <- diagnostics$data_mod_is_data_frame &&
    diagnostics$data_def_has_column &&
    identical(as.character(data_def$Column), as.character(names(data_mod)))
  diagnostics$metadata_meaningful <- diagnostics$data_def_is_data_frame &&
    isTRUE(is_meaningful_metadata(data_def))
  diagnostics$handson_metadata_meaningful <- is.data.frame(handson_metadata) &&
    isTRUE(is_meaningful_metadata(handson_metadata))
  diagnostics$final_processed_metadata_meaningful <- is.data.frame(final_processed_metadata) &&
    isTRUE(is_meaningful_metadata(final_processed_metadata))
  meaningful_sources <- names(which(c(
    data_def = diagnostics$metadata_meaningful,
    handson_metadata = diagnostics$handson_metadata_meaningful,
    final_processed_metadata = diagnostics$final_processed_metadata_meaningful
  )))
  diagnostics$meaningful_metadata_source <- if (length(meaningful_sources) > 0L) {
    meaningful_sources[[1L]]
  } else {
    NA_character_
  }

  invariant_labels <- c(
    data_mod_is_data_frame = "rv$data_mod must be a data.frame",
    data_def_is_data_frame = "rv$data_def must be a data.frame",
    data_def_has_column = "rv$data_def must contain a Column field",
    metadata_columns_match_data = "rv$data_def$Column must exactly match names(rv$data_mod)",
    metadata_meaningful = "rv$data_def must contain meaningful metadata"
  )
  invariant_values <- unlist(diagnostics[names(invariant_labels)], use.names = TRUE)
  diagnostics$failures <- unname(invariant_labels[!invariant_values])
  diagnostics$valid <- length(diagnostics$failures) == 0L

  if (!is.null(rv)) {
    tryCatch(isolate({
      rv$datawizard_restore_diagnostics <- diagnostics
    }), error = function(e) {
      debug_log(paste("Data Wizard restore: failed to store restore diagnostics:", e$message), 1)
    })
  }

  if (!isTRUE(diagnostics$valid)) {
    message <- paste(
      "Data Wizard data-only canonical restore invariant validation failed:",
      paste(diagnostics$failures, collapse = "; ")
    )
    if (isTRUE(strict)) {
      stop(message, call. = FALSE)
    }
    warning(message, call. = FALSE, immediate. = TRUE)
    debug_log(message, 1)
  }

  invisible(diagnostics)
}



.sanitize_datawizard_ui_state_for_save <- function(x, path = character(0L)) {
  path_label <- function(path) {
    if (length(path) == 0L) "<root>" else paste(path, collapse = "$")
  }

  make_valid_names <- function(nms, n, prefix = "V") {
    fixed <- rep.int("", n)
    if (length(nms) > 0L && n > 0L) {
      copy_n <- min(length(nms), n)
      fixed[seq_len(copy_n)] <- as.character(nms[seq_len(copy_n)])
    }
    missing <- is.na(fixed) | !nzchar(fixed)
    fixed[missing] <- paste0(prefix, which(missing))
    fixed
  }

  repair_list_names <- function(value, path) {
    if (!is.list(value)) return(value)
    n <- length(value)
    nms <- attr(value, "names", exact = TRUE)
    if (!is.null(nms) && length(nms) != n) {
      fixed <- make_valid_names(nms, n)
      attr(value, "names") <- fixed
      debug_log(paste0(
        "Data Wizard save: normalized malformed names at ", path_label(path),
        " (names=", length(nms), ", values=", n, ")"
      ), 1)
    }
    value
  }

  repair_dataframe_names <- function(value, path) {
    n <- tryCatch(ncol(value), error = function(e) NULL)
    if (is.null(n)) return(value)

    nms <- attr(value, "names", exact = TRUE)
    if (!is.null(nms) && length(nms) != n) {
      fixed <- make_valid_names(nms, n)
      attr(value, "names") <- fixed
      debug_log(paste0(
        "Data Wizard save: normalized malformed data.frame names at ", path_label(path),
        " (names=", length(nms), ", columns=", n, ")"
      ), 1)
    }

    dimnames_value <- attr(value, "dimnames", exact = TRUE)
    if (is.list(dimnames_value)) {
      col_nms <- if (length(dimnames_value) >= 2L) dimnames_value[[2L]] else NULL
      if (!is.null(col_nms) && length(col_nms) != n) {
        fixed <- make_valid_names(col_nms, n)
        dimnames_value[[2L]] <- fixed
        attr(value, "dimnames") <- dimnames_value
        debug_log(paste0(
          "Data Wizard save: normalized malformed data.frame colnames at ", path_label(path),
          " (colnames=", length(col_nms), ", columns=", n, ")"
        ), 1)
      }
    }
    value
  }

  if (is.null(x) || !is.list(x)) return(x)
  if (is.data.frame(x)) {
    n <- ncol(x)
    nms <- names(x)
    if (!is.null(nms) && length(nms) != n) {
      fixed <- rep.int("", n)
      if (length(nms) > 0L && n > 0L) {
        copy_n <- min(length(nms), n)
        fixed[seq_len(copy_n)] <- nms[seq_len(copy_n)]
      }
      missing <- !nzchar(fixed)
      fixed[missing] <- paste0("V", which(missing))
      names(x) <- fixed
      debug_log(paste0(
        "Data Wizard save: normalized malformed names in submodule UI data.frame",
        " (names=", length(nms), ", columns=", n, ")"
      ), 1)
    }
    return(x)
  }

  normalize_list_names <- function(value, path_label) {
    if (!is.list(value) || is.data.frame(value)) return(value)
    n <- length(value)
    nms <- names(value)
    if (!is.null(nms) && length(nms) != n) {
      fixed <- rep.int("", n)
      if (length(nms) > 0L && n > 0L) {
        copy_n <- min(length(nms), n)
        fixed[seq_len(copy_n)] <- nms[seq_len(copy_n)]
      }
      names(value) <- fixed
      debug_log(paste0(
        "Data Wizard save: normalized malformed names in ", path_label,
        " (names=", length(nms), ", values=", n, ")"
      ), 1)
    }
    value
  }

  x <- normalize_list_names(x, "submodule UI state")
  nms <- names(x) %||% character(0)
  if ("pause_metadata_sync" %in% nms) x$pause_metadata_sync <- NULL

  # Normalize every nested plain-list branch before any field-level sanitizer sees
  # it. Iterate by position rather than name: malformed or blank names are exactly
  # what this guard is repairing, and name-based indexing can re-trigger the
  # original names-length error or skip unnamed NULL children.
  if (length(x) > 0L) {
    for (i in seq_along(x)) {
      child <- x[[i]]
      if (is.list(child) && !is.data.frame(child)) {
        x[i] <- list(.sanitize_datawizard_ui_state_for_save(child))
      }
    }
  }

  if (is.list(x$inputs)) x$inputs$pause_metadata_sync <- NULL
  if (is.list(x$ui_inputs)) x$ui_inputs$pause_metadata_sync <- NULL
  x
}

.restore_datawizard_canonical_data_transaction <- function(datawizard_state, dw, rv, state,
                                                           restored_metadata,
                                                           final_metadata = NULL,
                                                           reason = "session restore canonical data",
                                                           assert_invariants = TRUE) {
  canonical_raw <- state$primary_data_raw_rv %||% state$primary_data_raw
  final_data <- state$final_processed_data %||% state$data_mod
  final_metadata <- final_metadata %||% state$final_processed_metadata %||% restored_metadata

  datawizard_state$begin_datawizard_transaction(reason)
  if (is.data.frame(canonical_raw)) {
    datawizard_state$set_raw_imported_data(canonical_raw, paste(reason, "raw"))
  }
  if (is.data.frame(state$data_mod)) {
    datawizard_state$set_modified_data(state$data_mod, paste(reason, "working data"))
  }
  if (is.data.frame(restored_metadata) && isTRUE(is_meaningful_metadata(restored_metadata))) {
    datawizard_state$set_metadata_for_current_data(restored_metadata)
  } else if (!is.null(restored_metadata)) {
    debug_log("Data Wizard restore: skipped placeholder/Row-Index-only metadata during canonical transaction", 1)
  }
  if (is.data.frame(final_data)) {
    datawizard_state$set_final_data(final_data, paste(reason, "final data"))
  }
  if (is.data.frame(final_metadata) && isTRUE(is_meaningful_metadata(final_metadata))) {
    datawizard_state$set_metadata_final(final_metadata, paste(reason, "final metadata"))
  }

  .safe_rv_write(dw$primary_data_raw, canonical_raw)
  .safe_rv_write(dw$handson_metadata, restored_metadata)
  .safe_rv_write(dw$final_processed_data, final_data)
  .safe_rv_write(dw$final_processed_metadata, final_metadata)
  debug_log("Data Wizard restore: metadata sync forced to canonical restored metadata with pause off", 1)

  if (!is.null(rv)) {
    tryCatch(isolate({
      if (is.data.frame(canonical_raw)) rv$primary_data_raw <- canonical_raw
      if (is.data.frame(state$data_mod)) rv$data_mod <- state$data_mod
      if (is.data.frame(restored_metadata) && isTRUE(is_meaningful_metadata(restored_metadata))) {
        rv$data_def <- restored_metadata
        rv$handson_metadata <- restored_metadata
      }
      if (is.data.frame(final_data)) rv$final_processed_data <- final_data
      if (is.data.frame(final_metadata) && isTRUE(is_meaningful_metadata(final_metadata))) {
        rv$final_processed_metadata <- final_metadata
      }
      rv$datawizard_metadata_sync_paused <- FALSE
      rv$datawizard_metadata_sync_pending <- FALSE
    }), error = function(e) {
      debug_log(paste("Data Wizard restore: canonical rv mirror write failed:", e$message), 1)
    })
  }

  datawizard_state$publish_legacy_mirrors()
  datawizard_state$commit_datawizard_transaction()
  if (isTRUE(assert_invariants)) {
    assert_datawizard_data_only_restore_invariants(rv, strict = FALSE)
  }
  invisible(list(
    canonical_raw = canonical_raw,
    restored_metadata = restored_metadata,
    final_data = final_data,
    final_metadata = final_metadata
  ))
}


.safe_fn_call_session_mode <- function(fn, mode = "session") {
  if (is.null(fn) || !is.function(fn)) return(NULL)
  tryCatch({
    fn_args <- tryCatch(names(formals(fn)), error = function(e) character())
    if ("mode" %in% fn_args || "..." %in% fn_args) {
      isolate(fn(mode = mode))
    } else {
      # Older handles keep their zero-argument legacy/full behavior.
      isolate(fn())
    }
  }, error = function(e) {
    debug_log(paste("safe_fn_call_session_mode failed:", e$message), 2)
    NULL
  })
}

# Restore the Assign Rules bridge from callbacks that may run outside a
# reactive consumer (notably the deferred Data Wizard onFlushed dispatch).
# Keep the outcome explicit so callers never report success after an error.
.restore_assign_rules_payload <- function(assign_rules_out, payload, log_suffix) {
  if (!is.list(payload) || !is.list(assign_rules_out)) return("unavailable")

  restore_fn <- assign_rules_out$restore_condition_state
  success_message <- paste(
    "Data Wizard restore: restored Assign Rules condition state", log_suffix
  )
  failure_context <- "condition-state"

  if (!is.function(restore_fn)) {
    restore_fn <- assign_rules_out$set_session_state
    success_message <- paste(
      "Data Wizard restore: restored Assign Rules state", log_suffix,
      "via legacy setter"
    )
    failure_context <- "legacy state"
  }
  if (!is.function(restore_fn)) return("unavailable")

  error <- tryCatch({
    shiny::isolate(restore_fn(payload))
    NULL
  }, error = identity)
  if (!is.null(error)) {
    debug_log(paste(
      "Data Wizard restore: Assign Rules", failure_context,
      "restore failed:", conditionMessage(error)
    ), 1)
    return("error")
  }

  debug_log(success_message, 1)
  "success"
}

# ========================================
# Module Session Participant Registration
# ========================================
# Registers save/restore functions for each module.
# Called once from app.R after module initialization.
#
# Priority levels:
#   10 = Data Wizard (core data pipeline -- must restore first)
#   30 = Analysis modules (GO, GSEA -- results needed by viz modules)
#   40-60 = Full Session-only modules (explicit dependency/UI replay order)

#' Register all module session participants
#'
#' Iterates over available module outputs and registers save/restore
#' functions with the session registry for each supported module.
#'
#' @param session_registry The registry created by \code{create_session_registry()}.
#' @param module_outputs The \code{reactiveValues} holding all module outputs.
#' @param rv Optional shared \code{reactiveValues}. When supplied, hooks that
#'   need direct access to \code{rv} (notably the Data Wizard \code{data_def}
#'   re-assert on restore) can reach it from their closure. Safe to omit for
#'   back-compat with older callers.
#' @param session Optional Shiny session object. When supplied, hooks can
#'   schedule deferred work via \code{session$onFlushed()}.
register_module_session_participants <- function(session_registry, module_outputs,
                                                 rv = NULL, session = NULL) {
  SESSION_SAVE_LEVEL_DATA <- modEnv$SESSION_SAVE_LEVEL_DATA
  SESSION_SAVE_LEVEL_ANALYSIS <- modEnv$SESSION_SAVE_LEVEL_ANALYSIS
  SESSION_SAVE_LEVEL_FULL <- modEnv$SESSION_SAVE_LEVEL_FULL
  resolve_data_pair_for_restore <- modEnv$resolve_data_pair_for_restore

  full_session_participant_priorities <- c(
    abundances = 40L,
    sampleids = 41L,
    pca = 50L,
    volcano = 51L,
    dotplot = 52L,
    string = 53L,
    venn = 54L,
    heatmap = 60L
  )

  if (is.null(session_registry)) return(invisible(NULL))

  # ------------------------------------------------------------------
  # Data Wizard (priority 10, level: data_only)
  # ------------------------------------------------------------------
  # Save-level contract:
  #   - Data & Metadata: Data Wizard is the only module participant. It saves
  #     canonical data, Data Wizard metadata, and the complete Data Wizard
  #     module/submodule UI and settings; it intentionally excludes all
  #     non-Data-Wizard module UI/plots and does not create plot-data caches.
  #   - Data & Analysis: Data Wizard contributes canonical data/metadata in a
  #     lean shape while analysis participants add result objects; full Data
  #     Wizard UI is avoided unless explicitly required by a restore path.
  #   - Full Session: Data Wizard contributes its complete UI/settings state and
  #     full-session participants contribute supported UI/plots/state.
  tryCatch({
    dw <- isolate(module_outputs$datawizard_out)
    if (!is.null(dw) && is.list(dw)) {
      dependency_status <- .get_datawizard_session_dependency_status()
      missing_dependencies <- names(dependency_status)[!dependency_status]
      if (length(missing_dependencies) > 0L) {
        debug_log(paste(
          "Data Wizard full session participant not registered because",
          "required restore dependencies are unavailable after fallback:",
          paste(paste0(missing_dependencies, "()"), collapse = ", ")
        ), 1)
        .register_degraded_datawizard_session_participant(
          session_registry = session_registry,
          save_level = SESSION_SAVE_LEVEL_DATA,
          rv = rv
        )
      } else {
        session_registry$register(
        module_id  = "datawizard",
        priority   = 10L,
        save_level = SESSION_SAVE_LEVEL_DATA,

        save_fn = function(save_level = SESSION_SAVE_LEVEL_FULL) {
          state <- list(version = "2.0", save_level = save_level)
          debug_log(paste("Data Wizard save_fn: collecting state for", save_level), 1)

          # Per-field safety net: any single assignment that throws leaves
          # that field NULL and logs the breadcrumb; the save continues for
          # the rest of the module so one bad submodule can never null the
          # whole datawizard snapshot at the outer sanitize step.
          set_field <- function(name, expr) {
            tryCatch(expr, error = function(e) {
              debug_log(paste0("Data Wizard save: field '", name,
                               "' failed: ", e$message), 1)
              NULL
            })
          }

          quarantine_counters <- new.env(parent = emptyenv())
          quarantine_counters$fast_path_dataframe_count <- 0L
          quarantine_counters$deep_sanitize_count <- 0L
          quarantine_counters$stripped_closure_count <- 0L

          quarantine <- function(name, raw) {
            if (is.null(raw)) return(NULL)
            if (name %in% c("submodule_ui_states", "loader_state")) {
              debug_log(paste0(
                "Data Wizard save: ", name, " raw -> ",
                .describe_snapshot_object(raw)
              ), 2)
            }

            dedicated_ui_sanitized <- NULL
            if (name %in% c("submodule_ui_states", "loader_state") && is.list(raw)) {
              dedicated_ui_sanitized <- .sanitize_datawizard_ui_state_for_save(
                raw,
                path = name
              )
              raw <- dedicated_ui_sanitized
              debug_log(paste0(
                "Data Wizard save: ", name, " dedicated UI sanitize -> ",
                .describe_snapshot_object(raw)
              ), 2)
            }

            if (.is_plain_serializable_dataframe(raw)) {
              quarantine_counters$fast_path_dataframe_count <-
                quarantine_counters$fast_path_dataframe_count + 1L
              return(raw)
            }

            if (.is_plain_serializable_atomic(raw) || .is_plain_serializable_list(raw)) {
              return(raw)
            }

            old_counters <- getOption("miraprot.session_save_restore_debug_counters", NULL)
            options(miraprot.session_save_restore_debug_counters = quarantine_counters)
            on.exit(options(miraprot.session_save_restore_debug_counters = old_counters), add = TRUE)

            stripped <- tryCatch(.strip_closures(raw, name),
                                 error = function(e) {
                                   debug_log(paste0("Data Wizard save: strip_closures failed for '",
                                                    name, "': ", e$message), 1)
                                   NULL
                                 })
            if (is.null(stripped)) return(NULL)
            if (name %in% c("submodule_ui_states", "loader_state")) {
              stripped <- .sanitize_datawizard_ui_state_for_save(stripped)
              debug_log(paste0(
                "Data Wizard save: ", name, " stripped -> ",
                .describe_snapshot_object(stripped)
              ), 2)
            }
            tryCatch(.sanitize_for_serialization(stripped,
                                                 path = paste0("datawizard$", name)),
                     error = function(e) {
                       debug_log(paste0("Data Wizard save: sanitize failed for '",
                                        name, "': ", e$message), 1)
                       if (identical(name, "submodule_ui_states") && !is.null(dedicated_ui_sanitized)) {
                         debug_log(paste0(
                           "Data Wizard save: using dedicated submodule_ui_states sanitizer fallback after serialize sanitize failure"
                         ), 1)
                         return(dedicated_ui_sanitized)
                       }
                       NULL
                     })
          }

          # Core data state
          state$primary_data_raw         <- set_field("primary_data_raw",         .safe_rv_read(dw$primary_data_raw))
          state$handson_metadata         <- set_field("handson_metadata",         .safe_rv_read(dw$handson_metadata))
          state$final_processed_data     <- set_field("final_processed_data",     .safe_rv_read(dw$final_processed_data))
          state$final_processed_metadata <- set_field("final_processed_metadata", .safe_rv_read(dw$final_processed_metadata))
          state$apply_triggered          <- set_field("apply_triggered",          .safe_rv_read(dw$apply_triggered))
          state$filter_applied           <- set_field("filter_applied",           .safe_rv_read(dw$filter_applied))
          state$filtered_data            <- set_field("filtered_data",            .safe_rv_read(dw$filtered_data))

          # Canonical tables consumed by downstream modules (Volcano, PCA,
          # Venn, Heatmap, ...). Captured from the shared `rv` so the DW
          # restore_fn can re-assert them after the loader cascade settles.
          state$data_mod          <- set_field("data_mod",          quarantine("data_mod",          .safe_rv_read_from(rv, "data_mod")))
          state$data_def          <- set_field("data_def",          quarantine("data_def",          .safe_rv_read_from(rv, "data_def")))

          metadata_sync_paused <- isTRUE(set_field("metadata_sync_paused",
            .safe_rv_read_from(rv, "datawizard_metadata_sync_paused")))
          metadata_sync_pending <- isTRUE(set_field("metadata_sync_pending",
            .safe_rv_read_from(rv, "datawizard_metadata_sync_pending")))
          pending_handson_metadata <- set_field("pending_handson_metadata", {
            if (is.list(dw$tables_out) && is.function(dw$tables_out$current_handson_metadata)) {
              isolate(dw$tables_out$current_handson_metadata())
            } else {
              NULL
            }
          })
          pending_handson_matches_data <- is.data.frame(pending_handson_metadata) &&
            is.data.frame(state$data_mod) &&
            isTRUE(metadata_matches_dataset(pending_handson_metadata, state$data_mod))

          if (metadata_sync_paused && metadata_sync_pending && pending_handson_matches_data) {
            state$handson_metadata <- set_field("handson_metadata", quarantine("handson_metadata", pending_handson_metadata))
            state$data_def <- set_field("data_def", quarantine("data_def", pending_handson_metadata))
            debug_log("Data Wizard save: pending Handsontable metadata is authoritative because synchronization is paused with pending edits", 1)
          } else {
            debug_log(paste(
              "Data Wizard save: canonical rv$data_def is authoritative",
              paste0("(metadata_sync_paused=", metadata_sync_paused,
                     ", metadata_sync_pending=", metadata_sync_pending,
                     ", pending_handson_matches_data=", pending_handson_matches_data, ")")
            ), 1)
          }

          # The pair selected above is authoritative.  Validate it before any
          # snapshot compaction, cache pooling, or serialization can obscure
          # its provenance; plot caches are historical module inputs and must
          # never be used to repair canonical Data Wizard state.
          if (!isTRUE(metadata_matches_dataset(state$data_def, state$data_mod))) {
            pair_dimensions <- function(x) {
              if (is.data.frame(x) || is.matrix(x)) {
                paste0(nrow(x), " x ", ncol(x))
              } else {
                "not a data frame"
              }
            }
            ordered_columns_match <- is.data.frame(state$data_def) &&
              "Column" %in% names(state$data_def) &&
              is.data.frame(state$data_mod) &&
              identical(
                as.character(state$data_def$Column),
                as.character(names(state$data_mod))
              )
            stop(paste0(
              "Data Wizard save refused: canonical data and metadata are misaligned; ",
              "data dimensions=", pair_dimensions(state$data_mod),
              ", metadata dimensions=", pair_dimensions(state$data_def),
              ", ordered Column values match names(data_mod)=",
              if (ordered_columns_match) "TRUE" else "FALSE"
            ), call. = FALSE)
          }

          state$metadata_sync_paused <- NULL
          state$metadata_sync_pending <- NULL
          state$primary_data_raw_rv <- set_field("primary_data_raw_rv", quarantine("primary_data_raw_rv", .safe_rv_read_from(rv, "primary_data_raw")))
          state$data_mod_revision_id <- set_field("data_mod_revision_id", .safe_rv_read_from(rv, "data_mod_revision_id"))
          state$data_def_revision_id <- set_field("data_def_revision_id", .safe_rv_read_from(rv, "data_def_revision_id"))
          state$datawizard_data_revision_id <- set_field("datawizard_data_revision_id", .safe_rv_read_from(rv, "datawizard_data_revision_id"))

          # Modification tracking
          state$modification_history <- set_field("modification_history", .safe_rv_read(dw$modification_history))

          # Logging and settings
          state$imputation_log         <- set_field("imputation_log",         .safe_rv_read(dw$imputation_log))
          state$imputation_setting     <- set_field("imputation_setting",     .safe_rv_read(dw$imputation_setting))
          state$filtering_confidence   <- set_field("filtering_confidence",   .safe_rv_read(dw$filtering_confidence))
          state$filtering_valid_values <- set_field("filtering_valid_values", .safe_rv_read(dw$filtering_valid_values))
          state$filtered_conditions    <- set_field("filtered_conditions",    .safe_rv_read(dw$filtered_conditions))
          state$filtering_log          <- set_field("filtering_log",          .safe_rv_read(dw$filtering_log))

          # Rule management
          # Root-cause fix: central_rule_file in some sessions was a placeholder
          # (e.g. ".") instead of actual rule payload. Snapshot the embedded
          # Auto-Assign payload as authoritative fallback for full-session saves.
          raw_central_rule_file <- .safe_rv_read(dw$central_rule_file)
          raw_central_loaded_rules <- .safe_rv_read(dw$central_loaded_rules)
          condition_groups <- state$filtered_conditions
          if (is.null(condition_groups) && !is.null(state$final_processed_metadata)) {
            condition_groups <- tryCatch({
              metadata_df <- state$final_processed_metadata
              if (is.data.frame(metadata_df) && "Options_condition" %in% names(metadata_df)) {
                values <- unique(trimws(as.character(metadata_df$Options_condition)))
                values[!is.na(values) & nzchar(values)]
              } else {
                NULL
              }
            }, error = function(e) NULL)
          }
          state$condition_groups <- set_field("condition_groups", condition_groups)

          state$save_level <- if (identical(save_level, SESSION_SAVE_LEVEL_DATA)) {
            "data_only"
          } else if (identical(save_level, SESSION_SAVE_LEVEL_ANALYSIS)) {
            "data_and_analysis"
          } else {
            SESSION_SAVE_LEVEL_FULL
          }

          # Data & Metadata saves are intentionally scoped to the Data Wizard
          # module at the registry level, but within Data Wizard they must keep
          # the module and submodule UI/settings complete. Only analysis-level
          # saves use the lean canonical-data shape below.
          if (identical(save_level, SESSION_SAVE_LEVEL_ANALYSIS)) {
            keep_fields <- c(
              "version", "save_level", "primary_data_raw", "data_mod", "data_def",
              "handson_metadata", "final_processed_data", "final_processed_metadata",
              "apply_triggered",
              "condition_groups"
            )
            state <- state[intersect(names(state), keep_fields)]
            state$loader_state <- set_field("loader_state", quarantine("loader_state", {
              if (is.list(dw$loader_out) && is.function(dw$loader_out$get_minimal_session_state)) {
                .safe_fn_call(dw$loader_out$get_minimal_session_state)
              } else if (is.list(dw$loader_out) && is.function(dw$loader_out$get_session_state)) {
                .safe_fn_call_arg(dw$loader_out$get_session_state, "labels_only")
              } else if (is.function(dw$get_loader_session_state)) {
                # Backward-compatible fallback for older Data Wizard handles.
                # If only the legacy full loader hook is exposed, the restore
                # path still marks it labels-only before staging; current
                # handles should use one of the lightweight branches above.
                legacy_loader_state <- .sanitize_datawizard_ui_state_for_save(.safe_fn_call(dw$get_loader_session_state))
                if (is.list(legacy_loader_state)) {
                  legacy_loader_state$mode <- legacy_loader_state$mode %||% "labels_only"
                  legacy_loader_state$data_fixed <- NULL
                  legacy_loader_state$data2_fixed <- NULL
                  legacy_loader_state$primary_data_original <- NULL
                  legacy_loader_state$secondary_data_original <- NULL
                  legacy_loader_state$sheet_cache_primary <- list()
                  legacy_loader_state$sheet_cache_secondary <- list()
                  legacy_loader_state$all_sheets_primary <- NULL
                  legacy_loader_state$all_sheets_secondary <- NULL
                }
                legacy_loader_state
              } else {
                NULL
              }
            }))
            state$submodule_ui_states <- NULL
            if (is.list(dw$assign_rules_out) &&
                is.function(dw$assign_rules_out$get_session_state)) {
              state$assign_rules_condition_state <- set_field(
                "assign_rules_condition_state",
                quarantine("assign_rules_condition_state",
                  .safe_fn_call(dw$assign_rules_out$get_session_state))
              )
            }
            if (!is.null(raw_central_loaded_rules) || is.null(state$condition_groups)) {
              state$central_loaded_rules <- set_field(
                "central_loaded_rules",
                quarantine("central_loaded_rules", raw_central_loaded_rules)
              )
            }
            if (!is.null(raw_central_rule_file) &&
                (!is.null(state$central_loaded_rules) || is.null(state$condition_groups))) {
              state$central_rule_file <- set_field(
                "central_rule_file",
                quarantine("central_rule_file", raw_central_rule_file)
              )
            }
            # Data Wizard applies field-level quarantine/sanitization above. Mark
            # the snapshot so orchestration can avoid a second deep traversal.
            state <- .datawizard_snapshot_pool_compact(state)
            attr(state, "miraprot_sanitized") <- TRUE
            return(state)
          }

          submodule_snapshot_raw <- .sanitize_datawizard_ui_state_for_save(.safe_fn_call(dw$get_all_submodule_ui_states))
          if (is.list(dw$assign_rules_out) &&
              is.function(dw$assign_rules_out$get_session_state)) {
            state$assign_rules_condition_state <- set_field(
              "assign_rules_condition_state",
              quarantine("assign_rules_condition_state",
                .safe_fn_call(dw$assign_rules_out$get_session_state))
            )
          }
          autoassign_embedded <- NULL
          if (is.list(submodule_snapshot_raw)) {
            autoassign_embedded <- submodule_snapshot_raw$submodules$auto_assign_out$extra$current_loaded
          }
          central_rule_file_is_placeholder <- is.character(raw_central_rule_file) &&
            length(raw_central_rule_file) == 1L &&
            trimws(raw_central_rule_file) %in% c("", ".", "./")

          resolved_central_rule_file <- if (!central_rule_file_is_placeholder) {
            raw_central_rule_file
          } else if (is.list(autoassign_embedded)) {
            autoassign_embedded
          } else {
            raw_central_rule_file
          }
          resolved_central_loaded_rules <- if (!is.null(raw_central_loaded_rules)) {
            raw_central_loaded_rules
          } else if (is.list(autoassign_embedded)) {
            autoassign_embedded
          } else {
            NULL
          }
          debug_log(paste("Data Wizard save: central_rule_file BEFORE sanitize ->",
                          .describe_snapshot_object(resolved_central_rule_file)), 1)

          state$central_rule_file <- set_field(
            "central_rule_file",
            quarantine("central_rule_file", resolved_central_rule_file)
          )
          state$central_loaded_rules <- set_field(
            "central_loaded_rules",
            quarantine("central_loaded_rules", resolved_central_loaded_rules)
          )
          debug_log(paste("Data Wizard save: central_rule_file AFTER field sanitize ->",
                          .describe_snapshot_object(state$central_rule_file)), 1)

          # UI configs: strip closures so update*Input on restore cannot trip
          # "cannot coerce type 'closure' to vector of type 'character'".
          state$imputation_ui_config    <- set_field("imputation_ui_config",    quarantine("imputation_ui_config",    .safe_fn_call(dw$get_imputation_ui_config_for_export)))
          state$filtering_ui_config     <- set_field("filtering_ui_config",     quarantine("filtering_ui_config",     .safe_fn_call(dw$get_filtering_ui_config_for_export)))
          state$batch_effects_ui_config <- set_field("batch_effects_ui_config", quarantine("batch_effects_ui_config", .safe_fn_call(dw$get_batch_effects_ui_config_for_export)))
          state$ratios_ui_config        <- set_field("ratios_ui_config",        quarantine("ratios_ui_config",        .safe_fn_call(dw$get_ratios_ui_config_for_export)))
          state$pivot_ui_config         <- set_field("pivot_ui_config",         quarantine("pivot_ui_config",         .safe_fn_call(dw$get_pivot_ui_config_for_export)))
          state$merge_ui_config         <- set_field("merge_ui_config",         quarantine("merge_ui_config",         .safe_fn_call(dw$get_merge_ui_config_for_export)))
          state$basemean_ui_config      <- set_field("basemean_ui_config",      quarantine("basemean_ui_config",      .safe_fn_call(dw$get_basemean_ui_config_for_export)))

          # Legacy queues (edit operations, ratio configurations)
          state$edit_operations      <- set_field("edit_operations",      .safe_fn_call(dw$get_edit_operations_for_export))
          state$ratio_configurations <- set_field("ratio_configurations", .safe_fn_call(dw$get_ratio_configurations_for_export))

          # Per-submodule UI input snapshots (session-restore bridge).
          # Rehydrates every user-visible input across every DW submodule on
          # restore, independent of whether the submodule's tab was opened at
          # save time.
          state$submodule_ui_states <- set_field("submodule_ui_states",
            quarantine("submodule_ui_states", submodule_snapshot_raw))
          # Dedicated loader snapshot for robustness: older restores may skip
          # generic submodule UI replay, but loader state is required to
          # rehydrate Excel sheet caches, header rows and additional dataset.
          state$loader_state <- set_field("loader_state", quarantine("loader_state", {
            loader_state <- NULL
            if (is.function(dw$get_loader_session_state)) {
              loader_state <- .safe_fn_call(dw$get_loader_session_state)
            } else if (is.list(dw$loader_out) && is.function(dw$loader_out$get_session_state)) {
              loader_state <- .safe_fn_call(dw$loader_out$get_session_state)
            }
            if (is.null(loader_state) && is.list(state$submodule_ui_states)) {
              loader_state <- state$submodule_ui_states$submodules$loader_out %||%
                state$submodule_ui_states$loader_out
            }
            loader_state
          }))
          if (is.list(state$loader_state)) {
            # Explicit loader data mirrors keep the Data & Metadata snapshot
            # robust if generic submodule UI state is unavailable or a future
            # loader-state schema omits cache entries.  Full Data Wizard
            # loader snapshots include workbook sheet caches when the source
            # Excel upload is still available, so restored sessions can switch
            # sheets without requiring a re-upload.
            state$loader_data_fixed <- set_field(
              "loader_data_fixed",
              quarantine("loader_data_fixed", state$loader_state$data_fixed)
            )
            state$loader_primary_data_original <- set_field(
              "loader_primary_data_original",
              quarantine("loader_primary_data_original", state$loader_state$primary_data_original)
            )
            state$loader_primary_file_meta <- set_field(
              "loader_primary_file_meta",
              quarantine("loader_primary_file_meta", state$loader_state$primary_file_meta)
            )
            state$loader_secondary_file_meta <- set_field(
              "loader_secondary_file_meta",
              quarantine("loader_secondary_file_meta", state$loader_state$secondary_file_meta)
            )
          }
          if (is.list(state$submodule_ui_states)) {
            n <- length(state$submodule_ui_states$submodules %||% list())
            debug_log(paste(
              "Data Wizard save: submodule_ui_states sanitized",
              n, "submodule entries",
              paste0("(raw entries=", n_submodule_entries_before_sanitize,
                     ", datawizard_submodule_capture_ms=",
                     datawizard_submodule_capture_ms, ")"),
              "retained fields=",
              paste(names(state$submodule_ui_states$submodules %||% list()), collapse = ",")
            ), 2)
          }

          # Data Wizard applies field-level quarantine/sanitization above. Mark
          # the snapshot so orchestration can avoid a second deep traversal.
          state <- .datawizard_snapshot_pool_compact(state)
          attr(state, "miraprot_sanitized") <- TRUE
          debug_log(paste0(
            "Data Wizard save quarantine counters: fast_path_dataframe_count=",
            quarantine_counters$fast_path_dataframe_count,
            " deep_sanitize_count=", quarantine_counters$deep_sanitize_count,
            " stripped_closure_count=", quarantine_counters$stripped_closure_count
          ), 1)
          state
        },

        restore_fn = function(state, restore_save_level = NULL, ...) {
          if (is.null(state)) return()
          restore_args <- list(...)
          requested_phase <- restore_args$phase %||% "canonical_data"
          if (!requested_phase %in% c("canonical_data", "datawizard_ui")) {
            return()
          }
          if (identical(requested_phase, "datawizard_ui")) {
            debug_log("Data Wizard restore: datawizard_ui phase acknowledged after canonical restore dispatch", 2)
            return()
          }
          state <- .datawizard_snapshot_pool_resolve(state)
          restore_generation <- if (!is.null(rv)) {
            tryCatch(isolate(rv$session_restore_generation %||% NA_integer_),
                     error = function(e) NA_integer_)
          } else {
            NA_integer_
          }
          restore_generation_current <- function() {
            is.null(rv) || is.na(restore_generation) ||
              identical(
                tryCatch(isolate(rv$session_restore_generation %||% NA_integer_),
                         error = function(e) NA_integer_),
                restore_generation
              )
          }
          metadata_matches_data <- metadata_matches_dataset
          canonical_pair_valid <- restore_has_valid_canonical_pair(state$data_mod, state$data_def)
          metadata_authoritative <- canonical_pair_valid && is_meaningful_metadata(state$data_def)
          if (isTRUE(state$legacy_restore_metadata_pending)) {
            debug_log(
              "Data Wizard restore warning: legacy session contained no meaningful metadata; data restored with metadata pending",
              1
            )
          }
          if (!is.null(rv)) {
            tryCatch(isolate({
              rv$datawizard_metadata_choices_ready <- FALSE
            }), error = function(e) {
              debug_log(paste("Data Wizard restore: metadata choices ready reset failed:", e$message), 1)
            })
          }
          restore_save_level_resolved <- restore_save_level %||% state$save_level %||% NA_character_
          data_dims <- function(x) {
            if (is.data.frame(x) || is.matrix(x)) {
              paste0(nrow(x), "x", ncol(x))
            } else {
              "NA"
            }
          }
          restore_condition_choices <- function(st, metadata) {
            choices <- NULL
            if (is.data.frame(metadata) && "Options_condition" %in% names(metadata)) {
              choices <- unique(trimws(as.character(metadata$Options_condition)))
            }
            if (length(choices) == 0L || all(is.na(choices) | !nzchar(choices))) {
              choices <- st$condition_groups
            }
            assign_rules_payload <- st$assign_rules_condition_state %||%
              st$submodule_ui_states$submodules$assign_rules_out %||%
              st$submodule_ui_states$assign_rules_out
            if ((length(choices) == 0L || all(is.na(choices) | !nzchar(choices))) &&
                is.list(assign_rules_payload)) {
              choices <- assign_rules_payload$extra$Options_condition
            }
            choices <- unique(trimws(as.character(choices %||% character(0))))
            choices[!is.na(choices) & nzchar(choices)]
          }
          restore_submodule_payload_count <- function(st) {
            payload <- st$submodule_ui_states %||% st$submodule_states %||% st$ui_submodule_states
            if (is.list(payload) && is.list(payload$submodules)) {
              length(payload$submodules)
            } else if (is.list(payload)) {
              length(payload)
            } else {
              0L
            }
          }
          restore_loader_mode <- function(st) {
            loader_state <- .resolve_datawizard_loader_state(st)
            mode <- loader_state$mode %||% loader_state$restore_mode
            if (isTRUE(loader_state$restore_labels_only)) {
              mode <- "labels_only"
            } else if (isTRUE(loader_state$restore_skip_publish_working_data)) {
              mode <- paste(c(mode %||% "full", "skip_publish"), collapse = "+")
            }
            as.character(mode %||% "none")
          }
          non_datawizard_module_counts <- function() {
            registered_ids <- tryCatch(
              session_registry$registered_ids(),
              error = function(e) character(0)
            )
            restored_ids <- tryCatch(
              session_registry$current_restore_snapshot_ids(),
              error = function(e) character(0)
            )
            list(
              registered = length(setdiff(registered_ids, "datawizard")),
              restored = length(setdiff(restored_ids, "datawizard"))
            )
          }
          restore_phase_log <- function(phase, replay_enabled, metadata = NULL) {
            metadata <- metadata %||% state$data_def %||% state$handson_metadata %||%
              state$final_processed_metadata
            condition_choices <- restore_condition_choices(state, metadata)
            module_counts <- non_datawizard_module_counts()
            debug_log(paste(
              "Data Wizard restore phase",
              "generation=", restore_generation,
              "save_level=", restore_save_level_resolved,
              "phase=", phase,
              "raw_dims=", data_dims(state$primary_data_raw_rv %||% state$primary_data_raw),
              "working_dims=", data_dims(state$data_mod %||% state$final_processed_data),
              "metadata_dims=", data_dims(metadata),
              "metadata_meaningful=", isTRUE(is_meaningful_metadata(metadata)),
              "condition_count=", length(condition_choices),
              "condition_choices=", paste(condition_choices, collapse = ","),
              "loader_restore_mode=", restore_loader_mode(state),
              "submodule_ui_payload_count=", restore_submodule_payload_count(state),
              "registered_non_datawizard_module_count=", module_counts$registered,
              "restored_non_datawizard_module_count=", module_counts$restored,
              "submodule_replay=", isTRUE(replay_enabled),
              sep = " | "
            ), 1)
          }
          set_restore_phase <- function(phase, replay_enabled = TRUE, metadata = NULL) {
            if (!is.null(rv)) {
              tryCatch(isolate({
                set_session_restore_phase(rv, phase)
              }), error = function(e) {
                debug_log(paste("Data Wizard restore: failed to set restore phase", phase, e$message), 1)
              })
            }
            restore_phase_log(phase, replay_enabled = replay_enabled, metadata = metadata)
          }
          is_data_only_snapshot <- function(st) {
            save_level_value <- st$save_level %||% restore_save_level
            version_value <- as.character(st$version %||% "")[1L]

            no_replay_payload <- is.null(st$submodule_ui_states) &&
              is.null(st$imputation_ui_config) &&
              is.null(st$filtering_ui_config) &&
              is.null(st$batch_effects_ui_config) &&
              is.null(st$ratios_ui_config) &&
              is.null(st$pivot_ui_config) &&
              is.null(st$merge_ui_config) &&
              is.null(st$basemean_ui_config) &&
              is.null(st$edit_operations) &&
              is.null(st$ratio_configurations)

            is_data_only_level <- identical(save_level_value, SESSION_SAVE_LEVEL_DATA) ||
              identical(save_level_value, "data_only")

            has_explicit_save_level <-
              is.character(save_level_value) &&
              length(save_level_value) == 1L &&
              !is.na(save_level_value) &&
              nzchar(save_level_value)

            # Compatibility fallback for old v2 snapshots that did not record a level.
            legacy_v2_data_only <-
              identical(version_value, "2.0") &&
              !isTRUE(has_explicit_save_level)

            (isTRUE(is_data_only_level) || isTRUE(legacy_v2_data_only)) &&
              isTRUE(no_replay_payload)
          }
          extract_saved_condition_choices <- function(st, metadata) {
            choices <- NULL
            if (is.data.frame(metadata) && "Options_condition" %in% names(metadata)) {
              choices <- unique(trimws(as.character(metadata$Options_condition)))
            }
            if (length(choices) == 0L || all(is.na(choices) | !nzchar(choices))) {
              choices <- st$condition_groups
            }
            assign_rules_payload <- st$assign_rules_condition_state %||%
              st$submodule_ui_states$submodules$assign_rules_out %||%
              st$submodule_ui_states$assign_rules_out
            if ((length(choices) == 0L || all(is.na(choices) | !nzchar(choices))) &&
                is.list(assign_rules_payload)) {
              choices <- assign_rules_payload$extra$Options_condition
            }
            choices <- unique(trimws(as.character(choices %||% character(0))))
            choices <- choices[!is.na(choices) & nzchar(choices)]
            if (length(choices) == 0L) {
              c(NA_character_, "Condition 1")
            } else {
              unique(c(NA_character_, choices))
            }
          }
          build_synthetic_assign_rules_payload <- function(st, metadata) {
            labels <- NULL
            if (is.data.frame(metadata) && "Options_condition" %in% names(metadata)) {
              labels <- metadata$Options_condition
            }
            labels <- trimws(as.character(labels %||% character(0)))
            labels <- labels[!is.na(labels) & nzchar(labels)]
            if (length(labels) == 0L) {
              labels <- trimws(as.character(st$condition_groups %||% character(0)))
              labels <- labels[!is.na(labels) & nzchar(labels)]
            }
            labels <- unique(labels)
            if (length(labels) == 0L) {
              labels <- "Condition 1"
            }
            condition_ids <- paste0("Condition_", seq_along(labels))
            text_input_ids <- paste0("textin", seq_along(labels))
            list(
              version = "1.0",
              module = "Assign Rules",
              ui_inputs = stats::setNames(as.list(labels), text_input_ids),
              extra = list(
                condition_inputs = stats::setNames(as.list(labels), condition_ids),
                counter_condition = length(labels),
                Options_condition = c(NA_character_, labels)
              )
            )
          }
          extract_saved_assign_rules_payload <- function(st) {
            st$assign_rules_condition_state %||%
              st$submodule_ui_states$submodules$assign_rules_out %||%
              st$submodule_ui_states$assign_rules_out %||%
              st$submodule_states$submodules$assign_rules_out %||%
              st$submodule_states$assign_rules_out %||%
              st$ui_submodule_states$submodules$assign_rules_out %||%
              st$ui_submodule_states$assign_rules_out
          }
          restore_assign_rules_payload <- function(assign_rules_payload, log_suffix) {
            .restore_assign_rules_payload(
              dw$assign_rules_out, assign_rules_payload, log_suffix
            )
          }
          publish_metadata_choices_ready <- function() {
            if (!is.null(rv)) {
              tryCatch(isolate({
                rv$datawizard_metadata_choices_ready <- TRUE
                rv$datawizard_metadata_choices_revision <- (rv$datawizard_metadata_choices_revision %||% 0L) + 1L
              }), error = function(e) {
                debug_log(paste("Data Wizard restore: metadata choices ready signal failed:", e$message), 1)
              })
            }
          }
          restore_metadata_choices <- function(st, metadata) {
            condition_choices <- extract_saved_condition_choices(st, metadata)
            if (is.list(dw$assign_rules_out) &&
                is.function(dw$assign_rules_out$condition_options)) {
              tryCatch(dw$assign_rules_out$condition_options(condition_choices),
                       error = function(e) {
                         debug_log(paste("Data Wizard restore: condition choices restore failed:", e$message), 1)
                       })
            }
            identifier_choices <- if (exists("create_datawizard_identifier_choices", mode = "function")) {
              create_datawizard_identifier_choices(metadata)
            } else {
              stats::setNames(character(0), character(0))
            }
            if (is.function(dw$identifier_choices)) {
              tryCatch(dw$identifier_choices(identifier_choices), error = function(e) NULL)
            }
            if (!is.null(rv)) {
              tryCatch(isolate({
                rv$datawizard_identifier_choices <- identifier_choices
                option_choices <- names(identifier_choices)
                option_choices <- option_choices[nzchar(option_choices)]
                names(option_choices) <- option_choices
                rv$datawizard_identifier_option_choices <- option_choices
                rv$datawizard_metadata_meaningful_ready <- isTRUE(is_meaningful_metadata(metadata))
                rv$datawizard_metadata_lifecycle_state <- if (isTRUE(is_meaningful_metadata(metadata))) {
                  "metadata_ready"
                } else {
                  "metadata_placeholder"
                }
                rv$datawizard_metadata_assignment_pending <- FALSE
              }), error = function(e) {
                debug_log(paste("Data Wizard restore: central identifier choices restore failed:", e$message), 1)
              })
            }
          }
          datawizard_state <- create_primary_data_state_adapter(
            rv = rv,
            core_values = dw,
            debug_log_fn = debug_log
          )

          if (isTRUE(is_data_only_snapshot(state))) {
            canonical_raw <- state$primary_data_raw_rv %||% state$primary_data_raw
            restored_metadata <- if (isTRUE(canonical_pair_valid)) {
              state$data_def
            } else {
              state$handson_metadata %||% state$final_processed_metadata %||% state$data_def
            }

            set_restore_phase("canonical_data", replay_enabled = FALSE, metadata = restored_metadata)
            canonical_restore <- .restore_datawizard_canonical_data_transaction(
              datawizard_state = datawizard_state,
              dw = dw,
              rv = rv,
              state = state,
              restored_metadata = restored_metadata,
              reason = "session restore data-only canonical data"
            )
            canonical_raw <- canonical_restore$canonical_raw
            restored_metadata <- canonical_restore$restored_metadata
            .safe_rv_write(dw$central_rule_file, state$central_rule_file)
            .safe_rv_write(dw$central_loaded_rules, state$central_loaded_rules)

            set_restore_phase("datawizard_ui", replay_enabled = FALSE, metadata = restored_metadata)
            loader_state <- state$loader_state
            if (is.null(loader_state) &&
                (!is.null(state$loader_data_fixed) || !is.null(state$loader_primary_data_original))) {
              loader_state <- list(version = "1.0", mode = "full", inputs = list())
            }
            if (is.list(loader_state)) {
              loader_state$data_fixed <- loader_state$data_fixed %||% state$loader_data_fixed
              loader_state$primary_data_original <- loader_state$primary_data_original %||%
                state$loader_primary_data_original
              loader_state$primary_file_meta <- loader_state$primary_file_meta %||%
                state$loader_primary_file_meta
              loader_state$secondary_file_meta <- loader_state$secondary_file_meta %||%
                state$loader_secondary_file_meta
            }
            if (!is.null(loader_state) &&
                (is.function(dw$set_loader_session_state) ||
                 (is.list(dw$loader_out) && is.function(dw$loader_out$set_session_state)))) {
              loader_state$restore_skip_publish_working_data <- TRUE
              loader_state$restore_generation <- restore_generation
              loader_state$restore_labels_only <- TRUE
              if (is.function(dw$set_loader_session_state)) {
                .safe_fn_call_arg(dw$set_loader_session_state, loader_state)
              } else {
                .safe_fn_call_arg(dw$loader_out$set_session_state, loader_state)
              }
            }

            set_restore_phase("datawizard_ui", replay_enabled = FALSE, metadata = restored_metadata)
            restore_metadata_choices(state, restored_metadata)
            saved_assign_rules_payload <- extract_saved_assign_rules_payload(state)
            if (is.list(saved_assign_rules_payload)) {
              restore_assign_rules_payload(
                saved_assign_rules_payload,
                "from saved condition state during data-only restore"
              )
            } else {
              synthetic_assign_rules_payload <- build_synthetic_assign_rules_payload(state, restored_metadata)
              restore_assign_rules_payload(
                synthetic_assign_rules_payload,
                "from synthetic metadata condition groups during data-only restore"
              )
            }
            publish_metadata_choices_ready()

            if (!is.null(rv) && restore_generation_current()) {
              isolate({
                if (exists(".normalize_restore_runtime_logicals", mode = "function")) {
                  .normalize_restore_runtime_logicals(rv)
                }
                set_session_restore_phase(rv, "complete")
                rv$session_restoring <- FALSE
                rv$session_restore_trigger <- (rv$session_restore_trigger %||% 0L) + 1L
              })
            }
            assert_datawizard_data_only_restore_invariants(rv, strict = FALSE)
            restore_phase_log("complete", replay_enabled = FALSE, metadata = restored_metadata)
            debug_log("Data Wizard data-only session state restored via canonical fast path", 1)
            return(invisible(NULL))
          }

          # ----------------------------------------------------------------
          # Phase 1: restore canonical data through the shared state adapter.
          # ----------------------------------------------------------------
          set_restore_phase("canonical_data")
          canonical_raw <- state$primary_data_raw_rv %||% state$primary_data_raw
          restored_handson_metadata <- state$handson_metadata
          if (isTRUE(canonical_pair_valid)) {
            restored_handson_metadata <- state$data_def
            debug_log("Data Wizard restore: using state$data_def as canonical handson_metadata", 1)
          }

          canonical_restore <- .restore_datawizard_canonical_data_transaction(
            datawizard_state = datawizard_state,
            dw = dw,
            rv = rv,
            state = state,
            restored_metadata = restored_handson_metadata,
            reason = "session restore canonical data",
            assert_invariants = FALSE
          )
          canonical_raw <- canonical_restore$canonical_raw
          restored_handson_metadata <- canonical_restore$restored_metadata
          .safe_rv_write(dw$apply_triggered, state$apply_triggered)
          .safe_rv_write(dw$filter_applied, state$filter_applied)
          .safe_rv_write(dw$filtered_data, state$filtered_data)
          .safe_rv_write(dw$modification_history, state$modification_history)
          .safe_rv_write(dw$imputation_log, state$imputation_log)
          .safe_rv_write(dw$imputation_setting, state$imputation_setting)
          .safe_rv_write(dw$filtering_confidence, state$filtering_confidence)
          .safe_rv_write(dw$filtering_valid_values, state$filtering_valid_values)
          .safe_rv_write(dw$filtered_conditions, state$filtered_conditions)
          .safe_rv_write(dw$filtering_log, state$filtering_log)
          .safe_rv_write(dw$central_rule_file, state$central_rule_file)
          .safe_rv_write(dw$central_loaded_rules, state$central_loaded_rules)
          datawizard_state$publish_legacy_mirrors()
          datawizard_state$commit_datawizard_transaction()

          if (!is.null(rv)) {
            tryCatch(isolate({
              if (is.data.frame(state$data_mod)) rv$data_mod <- state$data_mod
              if (is.data.frame(state$data_def) && isTRUE(is_meaningful_metadata(state$data_def))) {
                rv$data_def <- state$data_def
              }
              if (is.data.frame(canonical_raw)) rv$primary_data_raw <- canonical_raw
              if (is.data.frame(state$loader_primary_data_original)) {
                rv$primary_data_original <- state$loader_primary_data_original
              }
            }), error = function(e) {
              debug_log(paste("Data Wizard restore: canonical rv reassert failed:", e$message), 1)
            })
          }

          post_restore_data_mod <- tryCatch(rv$data_mod, error = function(e) NULL)
          post_restore_data_def <- tryCatch(rv$data_def, error = function(e) NULL)
          post_restore_columns_identical <- is.data.frame(post_restore_data_mod) &&
            is.data.frame(post_restore_data_def) &&
            "Column" %in% names(post_restore_data_def) &&
            identical(as.character(post_restore_data_def$Column), names(post_restore_data_mod))
          post_restore_metadata_meaningful <- isTRUE(is_meaningful_metadata(post_restore_data_def))
          # During full-session restore, Auto-Assign and Data Wizard UI replay may
          # make placeholder metadata meaningful a few flushes later. Keep the
          # breadcrumb for diagnostics, but do not surface it as a level-1 error.
          assertion_log_level <- if (isTRUE(post_restore_columns_identical) &&
                                      isTRUE(post_restore_metadata_meaningful)) 1 else 2
          debug_log(paste(
            "Data Wizard restore assertion:",
            "ncol(rv$data_mod)=", if (is.data.frame(post_restore_data_mod)) ncol(post_restore_data_mod) else NA_integer_,
            "| nrow(rv$data_def)=", if (is.data.frame(post_restore_data_def)) nrow(post_restore_data_def) else NA_integer_,
            "| columns_identical=", post_restore_columns_identical,
            "| metadata_meaningful=", post_restore_metadata_meaningful
          ), assertion_log_level)

          # ----------------------------------------------------------------
          # Phase 2: stage loader-local restore without publishing working data.
          # ----------------------------------------------------------------
          set_restore_phase("datawizard_ui")
          local_state <- state
          loader_state <- local_state$loader_state
          extract_submodule_state_payload <- function(st) {
            if (!is.list(st)) return(NULL)
            candidates <- list(
              st$submodule_ui_states,
              st$submodule_states,
              st$ui_submodule_states
            )
            for (cand in candidates) {
              if (is.list(cand) && is.list(cand$submodules)) return(cand)
              if (is.list(cand)) return(list(version = "1.0", submodules = cand))
            }
            NULL
          }
          submodule_state_payload <- extract_submodule_state_payload(local_state)
          if (is.null(loader_state) && is.list(submodule_state_payload)) {
            loader_state <- submodule_state_payload$submodules$loader_out %||%
              submodule_state_payload$loader_out
          }
          if (is.null(loader_state) &&
              (!is.null(local_state$loader_data_fixed) || !is.null(local_state$loader_primary_data_original))) {
            loader_state <- list(version = "1.0", mode = "full", inputs = list())
          }
          if (is.list(loader_state)) {
            loader_state$data_fixed <- loader_state$data_fixed %||% local_state$loader_data_fixed
            loader_state$primary_data_original <- loader_state$primary_data_original %||%
              local_state$loader_primary_data_original
            loader_state$primary_file_meta <- loader_state$primary_file_meta %||%
              local_state$loader_primary_file_meta
            loader_state$secondary_file_meta <- loader_state$secondary_file_meta %||%
              local_state$loader_secondary_file_meta
          }
          if (is.data.frame(local_state$loader_primary_data_original)) {
            datawizard_state$set_dataset(
              "primary_original",
              local_state$loader_primary_data_original,
              source = "session restore loader immutable original",
              allow_original_update = TRUE
            )
            if (!is.null(rv)) {
              tryCatch(isolate({
                rv$primary_data_original <- local_state$loader_primary_data_original
              }), error = function(e) {
                debug_log(paste("Data Wizard restore: loader primary original rv reassert failed:", e$message), 1)
              })
            }
          }
          if (!is.null(loader_state) &&
              (is.function(dw$set_loader_session_state) ||
               (is.list(dw$loader_out) && is.function(dw$loader_out$set_session_state)))) {
            loader_state$restore_skip_publish_working_data <- TRUE
            loader_state$restore_generation <- restore_generation
            debug_log(paste0(
              "Data Wizard restore: staging loader state without working-data publish (primary sheets=",
              length(loader_state$all_sheets_primary %||% list()),
              ", secondary sheets=",
              length(loader_state$all_sheets_secondary %||% list()),
              ")"
            ), 2)
            if (is.function(dw$set_loader_session_state)) {
              .safe_fn_call_arg(dw$set_loader_session_state, loader_state)
            } else {
              .safe_fn_call_arg(dw$loader_out$set_session_state, loader_state)
            }
            .assert_datawizard_loader_snapshot_integrity(state$loader_state, state, loader_state)
          } else if (!is.null(loader_state)) {
            debug_log("Data Wizard restore: loader state present but loader set_session_state unavailable", 1)
          }

          # ----------------------------------------------------------------
          # Phase 3: let metadata-dependent dynamic choices rebuild.
          # ----------------------------------------------------------------
          set_restore_phase("datawizard_ui")
          restore_metadata_choices(local_state, restored_handson_metadata)

          # Rule data can now be pre-applied against canonical data/metadata.
          # During data-only restores, an authoritative canonical data/metadata
          # pair must remain the source of truth. Preserve central rule payloads
          # in Data Wizard state for provenance/future UI display, but do not
          # invoke Auto-Assign pre-application because that mutates metadata.
          restore_mode_data_only <- identical(restore_save_level_resolved, SESSION_SAVE_LEVEL_DATA) ||
            identical(restore_save_level_resolved, "data_only")
          skip_autoassign_preapply <- isTRUE(metadata_authoritative) && isTRUE(restore_mode_data_only)
          debug_log(paste("Data Wizard restore: central_rule_file in incoming state ->",
                          .describe_snapshot_object(local_state$central_rule_file)), 1)
          debug_log(paste("Data Wizard restore: central_loaded_rules in incoming state ->",
                          .describe_snapshot_object(local_state$central_loaded_rules)), 1)

          has_rule_tables <- function(x) {
            is.list(x) &&
              (is.data.frame(x$table) || is.data.frame(x$condition) || is.data.frame(x$ratio))
          }

          autoassign_rules_payload <- NULL
          autoassign_source <- "none"
          if (has_rule_tables(local_state$central_loaded_rules)) {
            autoassign_rules_payload <- local_state$central_loaded_rules
            autoassign_source <- "central_loaded_rules"
          } else if (has_rule_tables(local_state$central_rule_file)) {
            autoassign_rules_payload <- local_state$central_rule_file
            autoassign_source <- "central_rule_file"
          }

          autoassign_preapplied <- FALSE
          if (isTRUE(skip_autoassign_preapply)) {
            debug_log(
              "Data Wizard restore: skipped Auto-Assign pre-application during data-only restore because canonical metadata is authoritative",
              1
            )
          } else if (has_rule_tables(autoassign_rules_payload) &&
              is.list(dw$auto_assign_out) &&
              is.function(dw$auto_assign_out$load_rules_directly)) {
            autoassign_preapplied <- isTRUE(
              .safe_fn_call_arg(dw$auto_assign_out$load_rules_directly, autoassign_rules_payload)
            )
            if (isTRUE(autoassign_preapplied)) {
              debug_log(paste(
                "Data Wizard restore: pre-applied Auto-Assign rules from",
                autoassign_source, "before UI replay"), 1)
            }
          }

          # Defer the legacy import calls AND the per-submodule UI dispatch
          # until AFTER the current flush so dynamic selectize `choices`
          # (which depend on the just-restored data) rebuild before the
          # submodules' `update*Input(selected=…)` calls fire — Shiny
          # silently discards selected values that aren't in the current
          # `choices`.
          #
          # `state` is captured by closure so the callback survives
          # restore_fn returning. Falls back to synchronous calls when
          # session$onFlushed is unavailable (e.g. testing harness).
          dispatch_now <- function() {
            if (!restore_generation_current()) {
              debug_log("Data Wizard restore: skipped stale deferred UI dispatch", 2)
              return(invisible(NULL))
            }
            extract_submodule_state_payload <- function(st) {
              if (!is.list(st)) return(NULL)
              candidates <- list(
                st$submodule_ui_states,
                st$submodule_states,
                st$ui_submodule_states
              )
              for (cand in candidates) {
                if (is.list(cand) && is.list(cand$submodules)) return(cand)
                if (is.list(cand)) return(list(version = "1.0", submodules = cand))
              }
              NULL
            }

            backfill_legacy_submodule_payload <- function(payload, st) {
              payload <- payload %||% list(version = "1.0", submodules = list())
              payload$submodules <- payload$submodules %||% list()

              if (is.null(payload$submodules$edit_out) &&
                  is.data.frame(st$edit_operations)) {
                payload$submodules$edit_out <- list(
                  version = "1.0",
                  module = "Edit",
                  ui_inputs = list(),
                  extra = list(pending_operations = st$edit_operations)
                )
              }

              if (is.null(payload$submodules$ratios_out) &&
                  is.data.frame(st$ratio_configurations)) {
                payload$submodules$ratios_out <- list(
                  version = "1.0",
                  module = "Ratios",
                  ui_inputs = list(),
                  extra = list(ratio_configurations = st$ratio_configurations)
                )
              }

              if (is.null(payload$submodules$filtering_out) &&
                  is.list(st$filtering_ui_config) &&
                  is.data.frame(st$filtering_ui_config$custom)) {
                payload$submodules$filtering_out <- list(
                  version = "1.0",
                  module = "Filtering",
                  ui_inputs = list(),
                  extra = list(custom_conditions = st$filtering_ui_config$custom)
                )
              }

              payload
            }

            submodule_state_payload <- extract_submodule_state_payload(local_state)

            assign_rules_payload <- local_state$assign_rules_condition_state %||%
              submodule_state_payload$submodules$assign_rules_out %||%
              submodule_state_payload$assign_rules_out
            if (!is.list(assign_rules_payload)) {
              assign_rules_payload <- build_synthetic_assign_rules_payload(local_state, restored_handson_metadata)
            }
            set_restore_phase("datawizard_ui")
            assign_rules_restore_status <- restore_assign_rules_payload(
              assign_rules_payload,
              "before downstream submodule UI"
            )
            if (identical(assign_rules_restore_status, "unavailable")) {
              debug_log("Data Wizard restore: no Assign Rules condition-state payload to restore before downstream submodule UI", 2)
            }
            publish_metadata_choices_ready()

            set_restore_phase("datawizard_ui")

            n_legacy <- sum(!vapply(list(
              local_state$imputation_ui_config,
              local_state$filtering_ui_config,
              local_state$batch_effects_ui_config,
              local_state$ratios_ui_config,
              local_state$pivot_ui_config,
              local_state$merge_ui_config,
              local_state$basemean_ui_config
            ), is.null, logical(1)))
            existing_submodule_entries <- length(submodule_state_payload$submodules %||% list())
            should_dispatch_legacy_imports <- (existing_submodule_entries == 0L)

            # Deterministic precedence:
            # - Prefer canonical per-submodule session payload when present.
            # - Fall back to legacy config imports only for older snapshots
            #   that do not carry submodule_ui_states.
            debug_log(paste(
              "Data Wizard restore: legacy configs=", n_legacy,
              "| canonical submodule entries=", existing_submodule_entries,
              "| legacy import dispatch=", should_dispatch_legacy_imports
            ), 2)

            if (should_dispatch_legacy_imports) {
              .safe_fn_call_arg(dw$set_imputation_ui_config_from_import,    local_state$imputation_ui_config)
              .safe_fn_call_arg(dw$set_filtering_ui_config_from_import,     local_state$filtering_ui_config)
              .safe_fn_call_arg(dw$set_batch_effects_ui_config_from_import, local_state$batch_effects_ui_config)
              .safe_fn_call_arg(dw$set_ratios_ui_config_from_import,        local_state$ratios_ui_config)
              .safe_fn_call_arg(dw$set_pivot_ui_config_from_import,         local_state$pivot_ui_config)
              .safe_fn_call_arg(dw$set_merge_ui_config_from_import,         local_state$merge_ui_config)
              .safe_fn_call_arg(dw$set_basemean_ui_config_from_import,      local_state$basemean_ui_config)
            }

            submodule_state_payload <- backfill_legacy_submodule_payload(
              submodule_state_payload,
              local_state
            )
            if (is.list(submodule_state_payload)) {
              submodule_state_payload$loader_state <- submodule_state_payload$loader_state %||% loader_state
              attr(submodule_state_payload, "loader_state") <- loader_state
            }
            if (is.list(submodule_state_payload$submodules)) {
              submodule_state_payload$submodules$assign_rules_out <- NULL
            }
            submodule_state_payload$assign_rules_out <- NULL
            n_submodule_entries <- length(submodule_state_payload$submodules %||% list())
            if (n_submodule_entries > 0L && is.function(dw$set_all_submodule_ui_states)) {
              assign_rules_condition_options <- if (is.list(assign_rules_payload) &&
                                                    is.list(assign_rules_payload$extra)) {
                assign_rules_payload$extra$Options_condition
              } else {
                NULL
              }
              n_assign_rules_condition_options <- length(assign_rules_condition_options %||% character(0))
              debug_log(paste(
                "Data Wizard restore: module_ui phase has",
                n_assign_rules_condition_options,
                "Assign Rules condition option(s) before downstream submodule dispatch"), 2)
              debug_log(paste(
                "Data Wizard restore: dispatching submodule UI state payload for",
                n_submodule_entries, "submodule(s)"), 2)
              .safe_fn_call_arg(dw$set_all_submodule_ui_states, submodule_state_payload)
              debug_log(paste(
                "Data Wizard restore: dispatched submodule UI state for",
                n_submodule_entries, "submodule(s) - awaiting restore trigger apply"), 1)
            } else {
              debug_log("Data Wizard restore: no submodule UI state payload to dispatch", 2)
            }

            if (isTRUE(autoassign_preapplied)) {
              debug_log("Data Wizard restore: Auto-Assign rules were pre-applied from central rule payload", 2)
            }

            # Phase 5: notify loader/submodule restore observers, then release
            # the guard after those replay consumers have completed a flush.
            if (!is.null(rv) && restore_generation_current()) {
              isolate({
                if (exists(".normalize_restore_runtime_logicals", mode = "function")) {
                  .normalize_restore_runtime_logicals(rv)
                }
                set_session_restore_phase(rv, "complete")
                rv$session_restore_trigger <- (rv$session_restore_trigger %||% 0L) + 1L
              })
              finalize_restore_guard <- function() {
                isolate({
                  if (restore_generation_current()) {
                    rv$session_restoring <- FALSE
                  }
                })
              }
              if (!is.null(session) && is.function(session$onFlushed)) {
                session$onFlushed(once = TRUE, finalize_restore_guard)
              } else {
                finalize_restore_guard()
              }
              debug_log("Data Wizard restore: completed UI replay phase, bumped restore trigger, and deferred guard release until replay consumers flushed", 1)
            }
          }

          if (!is.null(session) && is.function(session$onFlushed)) {
            tryCatch({
              session$onFlushed(once = TRUE, dispatch_now)
            }, error = function(e) {
              dispatch_now()
              debug_log(paste(
                "Data Wizard: dispatch onFlushed unavailable, ran synchronously:",
                e$message), 1)
            })
          } else {
            dispatch_now()
          }

          debug_log(paste("Data Wizard session state restored; fields:",
                          paste(names(state)[!vapply(state, is.null, logical(1))],
                                collapse = ", ")), 1)
        }
        )
        debug_log("Data Wizard registered as session participant", 1)
      }
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Data Wizard for session:", e$message), 1)
  })

  add_restore_cache_dependency <- function(state, dependency) {
    if (!is.list(state)) state <- list()
    state$module_state <- .set_module_restore_cache_dependency(
      state$module_state,
      dependency
    )
    state
  }

  module_restore_cache_hit <- function(state) {
    st <- if (is.list(state)) state$module_state else NULL
    isTRUE(st$restore_cache_resolved) ||
      (is.list(st$restore_plot_data_cache) &&
         inherits(st$restore_plot_data_cache$data_mod, "data.frame") &&
         inherits(st$restore_plot_data_cache$data_def, "data.frame"))
  }

  invoke_module_set_session_state <- function(module, module_state, phase) {
    if (is.null(module_state) ||
        !"set_session_state" %in% names(module) ||
        !is.function(module$set_session_state)) {
      return(FALSE)
    }
    set_session_state <- module$set_session_state
    set_formals <- tryCatch(formals(set_session_state), error = function(e) NULL)
    supports_phase <- !is.null(set_formals) &&
      ("phase" %in% names(set_formals) || "..." %in% names(set_formals))
    if (identical(phase, "full_module_plots") && !isTRUE(supports_phase)) {
      return(FALSE)
    }
    if (isTRUE(supports_phase)) {
      set_session_state(module_state, phase = phase)
    } else {
      set_session_state(module_state)
    }
    TRUE
  }

  log_module_restore_timing <- function(module_id, phase, state, plot_recreated, start_time,
                                        restore_state_applied = FALSE,
                                        rebuild_requested = FALSE,
                                        render_completed = FALSE,
                                        render_failed = FALSE,
                                        render_timed_out = FALSE) {
    elapsed_ms <- round((proc.time()[["elapsed"]] - start_time) * 1000, 0)
    debug_log(paste0(
      "[RestoreModulePhaseTiming] module=", module_id,
      " phase=", phase %||% "default",
      " cache=", if (isTRUE(module_restore_cache_hit(state))) "hit" else "miss",
      " restore_state_applied=", if (isTRUE(restore_state_applied)) "yes" else "no",
      " rebuild_requested=", if (isTRUE(rebuild_requested)) "yes" else "no",
      " render_completed=", if (isTRUE(render_completed)) "yes" else "no",
      " render_failed=", if (isTRUE(render_failed)) "yes" else "no",
      " render_timed_out=", if (isTRUE(render_timed_out)) "yes" else "no",
      " plot_recreated=", if (isTRUE(plot_recreated)) "yes" else "no",
      " elapsed_ms=", elapsed_ms
    ), 1)
  }

  # ------------------------------------------------------------------
  # GO Module (priority 30, level: data_and_analysis)
  #
  # Required restore ordering:
  #   1. Data Wizard restores canonical data/metadata and Data Wizard UI.
  #   2. GO restores result/UI/plot-recreation state.
  #   3. GSEA restores result/UI/plot-recreation state.
  # ------------------------------------------------------------------
  tryCatch({
    go <- isolate(module_outputs$go_out)
    if (!is.null(go) && is.list(go)) {
      session_registry$register(
        module_id  = "go",
        priority   = 30L,
        save_level = SESSION_SAVE_LEVEL_ANALYSIS,

        save_fn = function() {
          has_results <- if ("has_results" %in% names(go) && is.function(go$has_results)) {
            tryCatch(go$has_results(), error = function(e) FALSE)
          } else if ("get_results" %in% names(go) && is.function(go$get_results)) {
            !is.null(tryCatch(go$get_results(), error = function(e) NULL))
          } else {
            FALSE
          }

          state <- list(version = "1.2", has_results = isTRUE(has_results))
          # Prefer module-owned session state. When no GO results exist, save
          # only UI state so Data & Analysis exports do not sanitize empty
          # result/plot payloads.
          if ("get_session_state" %in% names(go) && is.function(go$get_session_state)) {
            mode <- if (isTRUE(has_results)) "session" else "ui_only"
            state$module_state <- tryCatch(go$get_session_state(mode), error = function(e) NULL)
          }

          # Legacy fallback only: do not duplicate the result when module_state
          # is present.
          if (is.null(state$module_state) && isTRUE(has_results) &&
              "get_results" %in% names(go) && is.function(go$get_results)) {
            state$results <- tryCatch(go$get_results(), error = function(e) NULL)
          }
          add_restore_cache_dependency(state, "live_rv")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("go", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("analysis_results", "analysis_ui")) return()
          stable_go_results <- function(x) {
            if (!is.list(x)) return(x)
            edo <- tryCatch(x$Edo_GO, error = function(e) NULL)
            if (!methods::is(edo, "enrichResult") && !methods::is(edo, "gseaResult")) return(x)
            slots <- tryCatch(methods::slotNames(edo), error = function(e) character())
            slot_or <- function(nm, default = NA) {
              if (!nm %in% slots) return(default)
              tryCatch(methods::slot(edo, nm), error = function(e) default)
            }
            result_df <- tryCatch(as.data.frame(slot_or("result", NULL)), error = function(e) NULL)
            list(
              Edo_GO = NULL,
              Edo_GO_safe = NULL,
              result_df = result_df,
              geneSets = slot_or("geneSets", tryCatch(x$geneSets, error = function(e) NULL)),
              go_data = tryCatch(x$go_data, error = function(e) NULL),
              go_data_FC = tryCatch(x$go_data_FC, error = function(e) NULL),
              parameters = list(
                organism = slot_or("organism", NA),
                ontology = slot_or("ontology", NA),
                keytype = slot_or("keytype", NA),
                pAdjustMethod = slot_or("pAdjustMethod", NA),
                pvalueCutoff = slot_or("pvalueCutoff", NA),
                qvalueCutoff = slot_or("qvalueCutoff", NA),
                minGSSize = slot_or("minGSSize", NA),
                maxGSSize = slot_or("maxGSSize", NA)
              )
            )
          }
          # Restore via session interface if available
          restored_via_module_state <- FALSE
          if (!is.null(state$module_state) &&
              "set_session_state" %in% names(go) && is.function(go$set_session_state)) {
            restored_via_module_state <- tryCatch({
              invoke_module_set_session_state(go, state$module_state, phase)
              TRUE
            }, error = function(e) {
              debug_log(paste("GO set_session_state failed:", e$message), 1)
              FALSE
            })
          }
          # Backward compatibility: older snapshots store only raw S4 results.
          # Newer module_state carries a stable data-frame/list fallback, so avoid
          # overwriting that restored state with a raw enrichResult that may have
          # incompatible slots/classes.
          if (!restored_via_module_state && !is.null(state$results) &&
              "set_results" %in% names(go) && is.function(go$set_results)) {
            tryCatch(go$set_results(stable_go_results(state$results)), error = function(e) {
              debug_log(paste("GO set_results failed:", e$message), 1)
            })
          }
          debug_log("GO module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register GO for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # GSEA Module (priority 31, level: data_and_analysis; after GO)
  # ------------------------------------------------------------------
  tryCatch({
    gsea <- isolate(module_outputs$gsea_out)
    if (!is.null(gsea) && is.list(gsea)) {
      session_registry$register(
        module_id  = "gsea",
        priority   = 31L,
        save_level = SESSION_SAVE_LEVEL_ANALYSIS,

        save_fn = function() {
          .safe_module_accessor <- function(fn, default = NULL) {
            tryCatch(isolate(fn()), error = function(e) default)
          }

          has_results <- if ("has_results" %in% names(gsea) && is.function(gsea$has_results)) {
            isTRUE(.safe_module_accessor(gsea$has_results, FALSE))
          } else if ("get_results" %in% names(gsea) && is.function(gsea$get_results)) {
            !is.null(.safe_module_accessor(gsea$get_results, NULL))
          } else {
            FALSE
          }

          state <- list(version = "1.1", has_results = isTRUE(has_results))
          if ("get_session_state" %in% names(gsea) && is.function(gsea$get_session_state)) {
            mode <- if (isTRUE(has_results)) "session" else "ui_only"
            state$module_state <- tryCatch(gsea$get_session_state(mode), error = function(e) NULL)
          }

          # Legacy fallback only: avoid saving res_GSEA twice when module_state
          # already carries the module-owned session payload.
          if (is.null(state$module_state) && isTRUE(has_results) &&
              "get_results" %in% names(gsea) && is.function(gsea$get_results)) {
            state$results <- .safe_module_accessor(gsea$get_results, NULL)
          }
          if (is.null(state$module_state) && isTRUE(has_results) &&
              "get_analysis_metadata" %in% names(gsea) && is.function(gsea$get_analysis_metadata)) {
            state$analysis_metadata <- .safe_module_accessor(gsea$get_analysis_metadata, NULL)
          }
          state
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          restored_via_module_state <- FALSE
          if (!is.null(state$module_state) &&
              "set_session_state" %in% names(gsea) && is.function(gsea$set_session_state)) {
            restored_via_module_state <- tryCatch({
              gsea$set_session_state(state$module_state)
              TRUE
            }, error = function(e) {
              debug_log(paste("GSEA set_session_state failed:", e$message), 1)
              FALSE
            })
          }
          if (!isTRUE(restored_via_module_state) && !is.null(state$results) &&
              "set_results" %in% names(gsea) && is.function(gsea$set_results)) {
            tryCatch(gsea$set_results(state$results), error = function(e) {
              debug_log(paste("GSEA set_results failed:", e$message), 1)
            })
          }
          debug_log("GSEA module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register GSEA for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # Volcano Module (priority 51, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    volcano <- isolate(module_outputs$volcano_out)
    if (!is.null(volcano) && is.list(volcano)) {
      session_registry$register(
        module_id  = "volcano",
        priority   = full_session_participant_priorities[["volcano"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "2.0")
          if ("get_session_state" %in% names(volcano) && is.function(volcano$get_session_state)) {
            state$module_state <- tryCatch(volcano$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "volcano")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          restore_state_applied <- FALSE
          rebuild_requested <- FALSE
          on.exit(log_module_restore_timing(
            "volcano", phase, state, plot_recreated, start_time,
            restore_state_applied = restore_state_applied,
            rebuild_requested = rebuild_requested
          ), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            # As with PCA, retain both orchestration calls: state is applied in
            # the first phase and the second only requests reconstruction at
            # the later session_restore_trigger boundary.
            invoked <- invoke_module_set_session_state(volcano, state$module_state, phase)
            restore_state_applied <- isTRUE(invoked) && identical(phase, "full_module_state")
            rebuild_requested <- isTRUE(invoked) && identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("Volcano set_session_state failed:", e$message), 1)
          })
          debug_log("Volcano module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Volcano for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # PCA Module (priority 50, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    pca <- isolate(module_outputs$pca_out)
    if (!is.null(pca) && is.list(pca)) {
      session_registry$register(
        module_id  = "pca",
        priority   = full_session_participant_priorities[["pca"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(pca) && is.function(pca$get_session_state)) {
            state$module_state <- tryCatch(pca$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "pca")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          restore_state_applied <- FALSE
          rebuild_requested <- FALSE
          on.exit(log_module_restore_timing(
            "pca", phase, state, plot_recreated, start_time,
            restore_state_applied = restore_state_applied,
            rebuild_requested = rebuild_requested
          ), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            # PCA deliberately retains both registry calls. The state phase
            # only applies/stages its server snapshot; the plots phase only
            # records a rebuild request and its diagnostics. Data Wizard's
            # later session_restore_trigger is the sole PCA UI/release/render
            # boundary.
            invoked <- invoke_module_set_session_state(pca, state$module_state, phase)
            restore_state_applied <- isTRUE(invoked) && identical(phase, "full_module_state")
            rebuild_requested <- isTRUE(invoked) && identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("PCA set_session_state failed:", e$message), 1)
          })
          debug_log("PCA module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register PCA for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # Heatmap Module (priority 60, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    heatmap <- isolate(module_outputs$heatmap_out)
    if (!is.null(heatmap) && is.list(heatmap)) {
      session_registry$register(
        module_id  = "heatmap",
        priority   = full_session_participant_priorities[["heatmap"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(heatmap) && is.function(heatmap$get_session_state)) {
            state$module_state <- tryCatch(heatmap$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "heatmap")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("heatmap", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(heatmap, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("Heatmap set_session_state failed:", e$message), 1)
          })
          debug_log("Heatmap module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Heatmap for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # Dotplot Module (priority 52, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    dotplot <- isolate(module_outputs$dotplot_out)
    if (!is.null(dotplot) && is.list(dotplot)) {
      session_registry$register(
        module_id  = "dotplot",
        priority   = full_session_participant_priorities[["dotplot"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(dotplot) && is.function(dotplot$get_session_state)) {
            state$module_state <- tryCatch(dotplot$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "dotplot")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("dotplot", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(dotplot, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("Dotplot set_session_state failed:", e$message), 1)
          })
          debug_log("Dotplot module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Dotplot for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # STRING Module (priority 53, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    string <- isolate(module_outputs$STRING_out)
    if (!is.null(string) && is.list(string)) {
      session_registry$register(
        module_id  = "string",
        priority   = full_session_participant_priorities[["string"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(string) && is.function(string$get_session_state)) {
            state$module_state <- tryCatch(string$get_session_state(), error = function(e) NULL)
            state$ui_inputs <- tryCatch(state$module_state$ui_inputs, error = function(e) NULL)
          }
          add_restore_cache_dependency(state, "none")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("string", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          if (!is.null(state$module_state) && is.null(state$module_state$ui_inputs) && !is.null(state$ui_inputs)) {
            state$module_state$ui_inputs <- state$ui_inputs
          }
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(string, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("STRING set_session_state failed:", e$message), 1)
          })
          debug_log("STRING module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register STRING for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # Venn Module (priority 54, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    venn <- isolate(module_outputs$venn_out)
    if (!is.null(venn) && is.list(venn)) {
      session_registry$register(
        module_id  = "venn",
        priority   = full_session_participant_priorities[["venn"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "2.0")
          if ("get_session_state" %in% names(venn) && is.function(venn$get_session_state)) {
            state$module_state <- tryCatch(venn$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "venn")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("venn", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(venn, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("Venn set_session_state failed:", e$message), 1)
          })
          debug_log("Venn module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Venn for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # Abundances Module (priority 40, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    abundances <- isolate(module_outputs$abundance_out)
    if (!is.null(abundances) && is.list(abundances)) {
      session_registry$register(
        module_id  = "abundances",
        priority   = full_session_participant_priorities[["abundances"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(abundances) && is.function(abundances$get_session_state)) {
            state$module_state <- tryCatch(
              abundances$get_session_state(),
              error = function(e) stop(
                paste0("Abundances session save failed: ", conditionMessage(e)),
                call. = FALSE
              )
            )
          }
          .registration_restore_cache_dependency(state, "abundances")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("abundances", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(abundances, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("Abundances set_session_state failed:", e$message), 1)
          })
          debug_log("Abundances module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register Abundances for session:", e$message), 1)
  })

  # ------------------------------------------------------------------
  # SampleIDs Module (priority 41, level: full_session)
  # ------------------------------------------------------------------
  tryCatch({
    sampleids <- isolate(module_outputs$sampleid_out)
    if (!is.null(sampleids) && is.list(sampleids)) {
      session_registry$register(
        module_id  = "sampleids",
        priority   = full_session_participant_priorities[["sampleids"]],
        save_level = SESSION_SAVE_LEVEL_FULL,

        save_fn = function() {
          state <- list(version = "1.0")
          if ("get_session_state" %in% names(sampleids) && is.function(sampleids$get_session_state)) {
            state$module_state <- tryCatch(sampleids$get_session_state(), error = function(e) NULL)
          }
          .registration_restore_cache_dependency(state, "sampleids")
        },

        restore_fn = function(state, phase = NULL) {
          if (is.null(state)) return()
          start_time <- proc.time()[["elapsed"]]
          plot_recreated <- FALSE
          on.exit(log_module_restore_timing("sampleids", phase, state, plot_recreated, start_time), add = TRUE)
          if (!phase %in% c("full_module_state", "full_module_plots")) return()
          tryCatch({
            plot_recreated <- invoke_module_set_session_state(sampleids, state$module_state, phase) &&
              identical(phase, "full_module_plots")
          }, error = function(e) {
            debug_log(paste("SampleIDs set_session_state failed:", e$message), 1)
          })
          debug_log("SampleIDs module session state restored", 1)
        }
      )
    }
  }, error = function(e) {
    debug_log(paste("Failed to register SampleIDs for session:", e$message), 1)
  })

  registered_full_session_ids <- intersect(
    names(full_session_participant_priorities),
    session_registry$registered_ids()
  )
  if (length(registered_full_session_ids) > 0L) {
    registered_full_session_ids <- registered_full_session_ids[order(
      full_session_participant_priorities[registered_full_session_ids]
    )]
    debug_log(paste(
      "Session registry: Full Session participants:",
      paste(
        paste0(registered_full_session_ids, "=",
               full_session_participant_priorities[registered_full_session_ids]),
        collapse = ", "
      )
    ), 1)
  }

  debug_log(paste("Session registry: registered",
                   length(session_registry$registered_ids()), "module(s)"), 1)
  invisible(NULL)
}
