# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_utils.R
# Purpose:
#   Supply shared utility functions for logging, validation, safe execution,
#   metadata safety checks, and generic helper behavior across Data Wizard layers.
#
# Architectural Role:
#   utility
#
# Responsibilities:
#   - Provide reusable debug logging and error-handling wrappers.
#   - Provide generic validators for reactive values, data frames, and metadata safety.
#   - Provide pure/helper utilities consumed by core, integration, and export layers.
#
# Non-Responsibilities (Must NOT be here):
#   - Contain orchestration wiring, module initialization, or UI composition.
#   - Encode module-specific business workflows.
#
# Allowed Dependencies:
#   - Base R and existing project/runtime dependencies required by utility helpers.
#   - No dependency on orchestration/core/integration/export implementation internals.
#
# Interaction Boundaries:
#   - Inputs:
#     Generic values, reactive accessors, metadata/data payloads, and callable handlers.
#   - Outputs:
#     Reusable utility functions with deterministic fallback behavior.
#   - Out-of-Scope Integrations:
#     None directly; utilities are consumed by in-scope layers.
#
# Stability Guarantees:
#   - Preserve utility function signatures used by callers.
#   - Preserve fallback behavior on errors and null inputs.
#   - Keep side effects limited to documented logging/notification behavior.
# ============================================================================
# modules/Data Wizard/datawizard_utils.R
# Data Wizard Utilities - Debug, Validation and Error Management

# Single canonical vocabulary for metadata Content editors and exports.
datawizard_metadata_content_choices <- function(include_blank = TRUE) {
  choices <- c(
    "# PSMs", "Abundance Ratio Adj. p-Value", "Abundance Ratio p-Value",
    "Abundance Ratio", "Normalized Abundance", "Batch Corrected Abundance",
    "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance",
    "Imputed Raw Abundance", "Imputed Normalized Abundance",
    "Imputed Batch Corrected Normalized Abundance",
    "Imputed Batch Corrected Raw Abundance", "Imputed Batch Corrected Abundance",
    "Identifier", "Additional Information", "Description", "Found in Sample",
    "Found in File", "Raw Abundance", "Row Index", "Protein Confidence", "Basemean"
  )
  if (isTRUE(include_blank)) c(NA_character_, choices) else choices
}

# Remove fields that are not part of the public Data Wizard metadata schema.
# Applying this at metadata boundaries keeps old sessions readable without
# exposing private lineage fields in live tables or new exports.
datawizard_drop_deprecated_metadata_columns <- function(metadata) {
  if (!is.data.frame(metadata)) return(metadata)
  private_fields <- c("Custom", "ContrastId", "VariantId")
  metadata[, !names(metadata) %in% private_fields, drop = FALSE]
}

#' Debug logging with controlled output levels
#' @param message debug message to log
#' @param level debug level (1=critical, 2=verbose)
debug_log <- function(message, level = 1) {
  rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
  if (is.function(rec)) {
    rec(level, "DATA WIZARD", message)
  } else {
    effective_level <- tryCatch(
      get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
      error = function(e) as.numeric(Sys.getenv("DW_DEBUG_LEVEL", "0"))
    )
    if (is.numeric(effective_level) && effective_level >= level) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      cat(paste0("[ DATA WIZARD ", timestamp, " ] ", message), "\n")
    }
  }
}

#' Check whether a data object can be displayed by the Data Wizard tables viewer
#' @param data Candidate primary data object.
#' @return TRUE when data is a data frame with at least one column.
datawizard_is_displayable_primary_data <- function(data) {
  is.data.frame(data) && !is.null(names(data)) && ncol(data) > 0
}

#' Resolve a Data Wizard dataset from the central registry by semantic role.
#'
#' The resolver gives export and integration code a single place to ask for the
#' data frame that represents a dataset role without reaching directly into
#' loader/current reactive state. Short role names are accepted for callers that
#' want to describe intent (`original`, `working`, `filtered`, etc.); they are
#' mapped onto the explicit registry roles used by the dataset registry.
#'
#' @param role Semantic role or explicit registry role.
#' @param core_values Core reactive values container, or NULL.
#' @param rv Shared app reactiveValues, or NULL.
#' @param fallback_roles Optional registry roles to try if `role` is missing.
#' @param fallback_data Optional data frame used only when registry lookup fails.
#' @param fallback_label Label recorded when `fallback_data` is returned.
#' @return A list with `data`, `entry`, `role`, `resolved_role`, `revision`, and
#'   `source` fields. `data` is NULL when no dataset can be resolved.
resolve_datawizard_dataset <- function(role = c("original", "raw", "working", "filtered", "final", "display", "export"),
                                       core_values = NULL,
                                       rv = NULL,
                                       fallback_roles = character(0),
                                       fallback_data = NULL,
                                       fallback_label = "legacy fallback") {
  requested_role <- if (length(role) > 1L) match.arg(role) else as.character(role)

  read_reactive <- function(value, default = NULL) {
    tryCatch({
      if (is.function(value)) value() else if (!is.null(value)) value else default
    }, error = function(e) default)
  }

  role_to_registry <- function(dataset_role) {
    switch(
      dataset_role,
      original = "primary_original",
      raw = "primary_raw",
      working = "primary_working",
      filtered = "primary_filtered",
      final = "primary_final",
      display = {
        if (isTRUE(read_reactive(core_values$filter_applied, FALSE))) "primary_filtered" else "primary_working"
      },
      export = {
        if (isTRUE(read_reactive(core_values$apply_triggered, FALSE))) {
          "primary_final"
        } else if (isTRUE(read_reactive(core_values$filter_applied, FALSE))) {
          "primary_filtered"
        } else {
          "primary_working"
        }
      },
      dataset_role
    )
  }

  resolve_registry <- function() {
    registry <- NULL
    if (!is.null(core_values) && !is.null(core_values$dataset_registry)) {
      registry <- read_reactive(core_values$dataset_registry, NULL)
    }
    if (is.null(registry) && !is.null(rv)) {
      registry <- tryCatch(rv$dataset_registry, error = function(e) NULL)
    }
    registry
  }

  candidate_roles <- unique(c(role_to_registry(requested_role), vapply(fallback_roles, role_to_registry, character(1))))
  registry <- resolve_registry()
  if (!is.null(registry) && is.function(registry$get_latest_entry)) {
    for (candidate_role in candidate_roles) {
      entry <- tryCatch(registry$get_latest_entry(candidate_role), error = function(e) NULL)
      if (!is.null(entry) && !is.null(entry$data)) {
        return(list(
          data = entry$data,
          entry = entry,
          role = requested_role,
          resolved_role = candidate_role,
          revision = entry$revision,
          source = "registry"
        ))
      }
    }
  }

  if (!is.null(fallback_data)) {
    return(list(
      data = fallback_data,
      entry = NULL,
      role = requested_role,
      resolved_role = candidate_roles[[1]],
      revision = NA_integer_,
      source = fallback_label
    ))
  }

  list(
    data = NULL,
    entry = NULL,
    role = requested_role,
    resolved_role = candidate_roles[[1]],
    revision = NA_integer_,
    source = "unresolved"
  )
}



#' Safely read a reactive or plain value.
#' @keywords internal
.datawizard_read_value <- function(value, default = NULL) {
  tryCatch({
    if (is.function(value)) value() else if (!is.null(value)) value else default
  }, error = function(e) default)
}

#' Resolve a Data Wizard object from common caller/global state containers.
#' @keywords internal
.datawizard_lookup_state <- function(name, env = parent.frame()) {
  cur <- env
  while (!identical(cur, emptyenv())) {
    if (exists(name, envir = cur, inherits = FALSE)) {
      return(get(name, envir = cur, inherits = FALSE))
    }
    cur <- parent.env(cur)
  }
  get0(name, envir = globalenv(), inherits = TRUE, ifnotfound = NULL)
}

#' Resolve the current Data Wizard dataset registry.
#' @keywords internal
.datawizard_resolve_registry <- function(core_values = NULL, rv = NULL) {
  registry <- NULL
  if (!is.null(core_values) && !is.null(core_values$dataset_registry)) {
    registry <- .datawizard_read_value(core_values$dataset_registry, NULL)
  }
  if (is.null(registry) && !is.null(rv)) {
    registry <- tryCatch(rv$dataset_registry, error = function(e) NULL)
  }
  registry
}

#' Create safe skeleton metadata for a data frame.
#' @keywords internal
.datawizard_safe_metadata_skeleton <- function(data) {
  cols <- if (is.data.frame(data) && !is.null(names(data))) names(data) else character(0)
  if (length(cols) == 0L) {
    return(NULL)
  }
  data.frame(
    Column = cols,
    Content = ifelse(cols == "Row Index", "Row Index", NA_character_),
    Options = NA_character_,
    Numerator = NA_character_,
    Denominator = NA_character_,
    Transformation = NA_character_,
    Sample = NA_character_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Check whether metadata describes the supplied dataset.
#'
#' @param meta Metadata data frame with a Column field.
#' @param data Data frame whose columns should be described by \\code{meta}.
#' @return TRUE when both inputs are data frames and meta Column values exactly
#'   match dataset columns one-to-one.
metadata_matches_dataset <- function(meta, data) {
  is.data.frame(meta) &&
    is.data.frame(data) &&
    nrow(meta) == ncol(data) &&
    "Column" %in% names(meta) &&
    identical(as.character(meta$Column), as.character(names(data)))
}

#' Apply the documented technical-column migration to a data/metadata pair.
#'
#' Only `Row Index` is technical. `Row_Index` is recognized solely as the
#' historical session alias and is migrated only when unambiguous or verified
#' equal to an existing canonical column. All other derived columns remain data.
datawizard_normalize_technical_pair <- function(data, metadata = NULL) {
  canonical <- "Row Index"; legacy <- "Row_Index"
  if (!is.data.frame(data)) return(list(data = data, metadata = metadata,
    technical = character(), migrated = FALSE))
  data <- as.data.frame(data, check.names = FALSE)
  cn <- names(data); migrated <- FALSE
  ci <- which(cn == canonical); li <- which(cn == legacy)
  if (!length(ci) && length(li) == 1L) {
    cn[li] <- canonical; names(data) <- cn; migrated <- TRUE
  } else if (length(ci) == 1L && length(li)) {
    equal_alias <- vapply(li, function(i) identical(as.character(data[[i]]),
      as.character(data[[ci]])), logical(1))
    if (any(equal_alias)) {
      data <- data[, -li[equal_alias], drop = FALSE]; migrated <- TRUE
    }
  }
  if (is.data.frame(metadata) && "Column" %in% names(metadata)) {
    keys <- as.character(metadata$Column)
    if (migrated) keys[keys == legacy] <- canonical
    metadata$Column <- keys
    # Verified aliases converge through migration; duplicate canonical keys are
    # intentionally left for validation rather than silently collapsed.
  }
  list(data = data, metadata = metadata,
       technical = names(data)[names(data) == canonical], migrated = migrated)
}

# Migrate the exact legacy metadata key without guessing from row-index-like
# names. When both keys occur, only byte-for-byte equivalent metadata rows are
# folded; conflicting rows remain duplicates for validation.
datawizard_migrate_metadata_technical_keys <- function(metadata) {
  if (!is.data.frame(metadata) || !"Column" %in% names(metadata)) return(metadata)
  keys <- as.character(metadata$Column)
  canonical <- which(keys == "Row Index"); legacy <- which(keys == "Row_Index")
  if (!length(canonical) && length(legacy) == 1L) {
    metadata$Column[legacy] <- "Row Index"
  } else if (length(canonical) == 1L && length(legacy)) {
    compare <- metadata; compare$Column <- NULL
    equivalent <- vapply(legacy, function(i) {
      isTRUE(all.equal(compare[i, , drop = FALSE], compare[canonical, , drop = FALSE],
                       check.attributes = FALSE))
    }, logical(1))
    if (any(equivalent)) metadata <- metadata[-legacy[equivalent], , drop = FALSE]
  }
  metadata
}

#' Check whether metadata has meaningful user/content assignments.
#'
#' A bare skeleton with only a Row Index content assignment is not meaningful.
#'
#' @param meta Metadata data frame.
#' @return TRUE when Content contains at least one non-empty value other than
#'   Row Index.
is_meaningful_metadata <- function(meta) {
  if (!is.data.frame(meta) || nrow(meta) == 0L || !("Content" %in% names(meta))) {
    return(FALSE)
  }

  content_values <- trimws(as.character(meta$Content))
  content_values <- content_values[!is.na(content_values) & nzchar(content_values)]
  if (length(content_values) == 0L) {
    return(FALSE)
  }

  any(tolower(content_values) != "row index")
}

#' Check whether restored canonical data and metadata form a valid pair.
#'
#' @param data_mod Restored/current primary data frame.
#' @param data_def Restored/current metadata definition.
#' @return TRUE when both inputs are data frames, metadata matches the data, and
#'   metadata contains meaningful assignments beyond Row Index.
restore_has_valid_canonical_pair <- function(data_mod, data_def) {
  is.data.frame(data_mod) &&
    is.data.frame(data_def) &&
    metadata_matches_dataset(data_def, data_mod) &&
    is_meaningful_metadata(data_def)
}

#' Rebuild metadata for a dataset role.
#'
#' @param dataset_role Registry dataset role to rebuild against.
#' @param header_row Header row passed to init_handson_table_dw when available.
#' @return Metadata skeleton or NULL when no dataset exists.
rebuild_metadata_for_dataset <- function(dataset_role, header_row = 1L) {
  env <- parent.frame()
  core_values <- .datawizard_lookup_state("core_values", env)
  rv <- .datawizard_lookup_state("rv", env)
  dataset <- resolve_datawizard_dataset(dataset_role, core_values = core_values, rv = rv)
  data <- dataset$data
  if (!is.data.frame(data)) {
    return(NULL)
  }

  init_meta <- .datawizard_lookup_state("init_meta", env)
  if (!is.function(init_meta)) {
    loader_out <- .datawizard_lookup_state("loader_out", env)
    init_meta <- tryCatch(loader_out$init_meta, error = function(e) NULL)
  }
  if (!is.function(init_meta)) {
    init_meta <- get0("init_handson_table_dw", envir = globalenv(), inherits = TRUE, ifnotfound = NULL)
  }

  rebuilt <- if (is.function(init_meta)) {
    tryCatch(init_meta(data, header_row = header_row), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(rebuilt) || !metadata_matches_dataset(rebuilt, data)) {
    rebuilt <- .datawizard_safe_metadata_skeleton(data)
  }
  rebuilt
}

#' Store metadata for a dataset role in the registry and legacy mirrors.
#'
#' @param metadata Metadata data frame.
#' @param dataset_role Registry dataset role the metadata describes.
#' @param source Source label for registry metadata.
#' @return The metadata, invisibly.
set_metadata_for_dataset <- function(metadata, dataset_role, source) {
  metadata <- datawizard_drop_deprecated_metadata_columns(metadata)
  env <- parent.frame()
  core_values <- .datawizard_lookup_state("core_values", env)
  rv <- .datawizard_lookup_state("rv", env)
  registry <- .datawizard_resolve_registry(core_values, rv)
  if (!is.null(registry) && is.function(registry$set)) {
    registry$set("metadata_working", metadata, source_metadata = list(source = source, dataset_role = dataset_role))
  }
  if (!is.null(core_values) && is.function(core_values$handson_metadata)) {
    core_values$handson_metadata(metadata)
  }
  if (!is.null(rv)) {
    rv$data_def <- metadata
    rv$handson_metadata <- metadata
  }
  invisible(metadata)
}

#' Resolve metadata for the current Data Wizard working dataset.
#'
#' @param reference_dataset_role Dataset role used to validate rv$data_def.
#' @return Metadata data frame, registry fallback, rebuilt skeleton, or NULL.
resolve_current_metadata <- function(reference_dataset_role = "primary_working") {
  env <- parent.frame()
  core_values <- .datawizard_lookup_state("core_values", env)
  rv <- .datawizard_lookup_state("rv", env)
  ref <- resolve_datawizard_dataset(reference_dataset_role, core_values = core_values, rv = rv)
  ref_data <- ref$data
  if (!is.data.frame(ref_data) && !is.null(rv)) {
    ref_data <- tryCatch(rv$data_mod, error = function(e) NULL)
  }

  rv_meta <- if (!is.null(rv)) tryCatch(rv$data_def, error = function(e) NULL) else NULL
  if (metadata_matches_dataset(rv_meta, ref_data)) {
    return(datawizard_drop_deprecated_metadata_columns(rv_meta))
  }

  registry <- .datawizard_resolve_registry(core_values, rv)
  if (!is.null(registry) && is.function(registry$get_latest_entry)) {
    for (role in c("metadata_working", "metadata_final")) {
      entry <- tryCatch(registry$get_latest_entry(role), error = function(e) NULL)
      if (!is.null(entry) && metadata_matches_dataset(entry$data, ref_data)) {
        return(datawizard_drop_deprecated_metadata_columns(entry$data))
      }
    }
  }

  core_meta <- if (!is.null(core_values) && is.function(core_values$handson_metadata)) {
    core_values$handson_metadata()
  } else {
    NULL
  }
  if (metadata_matches_dataset(core_meta, ref_data)) {
    return(datawizard_drop_deprecated_metadata_columns(core_meta))
  }

  rebuilt <- rebuild_metadata_for_dataset(reference_dataset_role)
  if (!is.null(rebuilt)) {
    set_metadata_for_dataset(rebuilt, reference_dataset_role, "metadata resolver rebuild")
  }
  rebuilt
}

#' Select the Data Wizard primary display data using tables precedence
#'
#' Centralizes the primary-data selection order used by the Data Wizard tables
#' preview so lifecycle observers can build metadata for the same data frame the
#' viewer can actually display. Precedence is:
#' 1. filtered data when filters are applied,
#' 2. modified working data when it differs from canonical raw data or contains
#'    known processing columns,
#' 3. canonical raw primary data.
#'
#' If legacy working data is available during initial loading before canonical
#' raw data has been published, the helper can publish it as raw first. This
#' prevents metadata from being created against a transient working object that
#' the table reactive would otherwise not be able to select consistently.
#'
#' @param core_values Core reactive values container.
#' @param rv Shared app reactiveValues, or NULL.
#' @param context Short log context label.
#' @param debug_log_fn Optional logger function(message, level).
#' @param publish_raw_if_missing Whether to write rv$data_mod to
#'   core_values$primary_data_raw when raw data is still NULL.
#' @return Displayable data frame, or NULL when no displayable candidate exists.
select_datawizard_primary_display_data <- function(core_values, rv = NULL,
                                                   context = "Tables",
                                                   debug_log_fn = NULL,
                                                   publish_raw_if_missing = TRUE) {
  log_selection <- function(message, level = 2) {
    if (is.function(debug_log_fn)) {
      debug_log_fn(message, level)
    } else if (exists("debug_log", mode = "function")) {
      debug_log(message, level)
    }
  }

  read_reactive <- function(accessor, default = NULL) {
    tryCatch({
      if (is.function(accessor)) accessor() else default
    }, error = function(e) default)
  }

  filtered_applied <- read_reactive(core_values$filter_applied, FALSE)
  filtered_data <- read_reactive(core_values$filtered_data, NULL)
  raw_data <- read_reactive(core_values$primary_data_raw, NULL)
  modified_data <- tryCatch({
    if (!is.null(rv)) rv$data_mod else NULL
  }, error = function(e) NULL)

  # Priority 1: show filtered data if filters are applied and displayable.
  if (isTRUE(filtered_applied)) {
    if (datawizard_is_displayable_primary_data(filtered_data)) {
      log_selection(paste(context, ": Showing filtered data -", nrow(filtered_data), "rows"), level = 2)
      return(filtered_data)
    }
    log_selection(paste(context, ": Filtered data is active but not displayable; deferring to next candidate"), level = 2)
  }

  # During startup, legacy rv$data_mod can be hydrated before canonical raw data.
  # Publish it as raw before deciding display precedence so downstream metadata
  # creation and the viewer operate on the same canonical data frame.
  if (is.null(raw_data) &&
      isTRUE(publish_raw_if_missing) &&
      datawizard_is_displayable_primary_data(modified_data) &&
      !is.null(core_values) &&
      is.function(core_values$primary_data_raw)) {
    log_selection(paste(context, ": Publishing rv$data_mod as canonical raw data during initial load"), level = 1)
    tryCatch({
      core_values$primary_data_raw(modified_data)
      raw_data <- modified_data
      if (!is.null(rv)) {
        rv$primary_data_raw <- modified_data
      }
    }, error = function(e) {
      log_selection(paste(context, ": Failed to publish rv$data_mod as raw data:", e$message), level = 1)
    })
  }

  # Priority 2: show modified data if it differs from raw data or has known
  # processing column prefixes. This intentionally preserves the legacy tables
  # condition that raw data must already be available before modified data is
  # considered a stable display candidate.
  if (datawizard_is_displayable_primary_data(modified_data) &&
      datawizard_is_displayable_primary_data(raw_data)) {
    data_differs <- !identical(modified_data, raw_data)
    has_processing_cols <- any(grepl("^Imputed |^Batch Corrected |^Pivoted |^Merged |^Ratio_", names(modified_data)))

    if (data_differs || has_processing_cols) {
      log_selection(paste(context, ": Showing modified data -", nrow(modified_data), "rows x", ncol(modified_data), "columns"), level = 2)
      log_selection(paste(context, ": Modified data reason: data_differs =", data_differs, "| has_processing_cols =", has_processing_cols), level = 2)
      return(modified_data)
    }
  } else if (!is.null(modified_data) && !datawizard_is_displayable_primary_data(modified_data)) {
    log_selection(paste(context, ": rv$data_mod exists but is not displayable; ignoring for primary display selection"), level = 2)
  }

  # Priority 3: default to raw data when displayable.
  if (datawizard_is_displayable_primary_data(raw_data)) {
    log_selection(paste(context, ": Showing raw primary data -", nrow(raw_data), "rows"), level = 2)
    return(raw_data)
  }

  if (!is.null(raw_data)) {
    log_selection(paste(context, ": Raw primary data exists but is not displayable; returning NULL"), level = 2)
  } else {
    log_selection(paste(context, ": No canonical displayable primary data available yet"), level = 2)
  }
  NULL
}

#' Validate data frame structure and content
#' @param df data frame to validate
#' @param name name for debugging purposes
#' @return validated data frame or NULL
validate_data_frame <- function(df, name = "data") {
  if (is.null(df)) return(NULL)
  if (!is.data.frame(df)) {
    debug_log(paste("Warning:", name, "is not a data frame"), level = 1)
    return(NULL)
  }
  if (nrow(df) == 0) {
    debug_log(paste("Warning:", name, "is empty"), level = 2)
  }
  return(df)
}

#' Safely access reactive values with error handling
#' @param rv reactive value or function
#' @param name name for debugging
#' @return reactive value or NULL on error
validate_reactive_value <- function(rv, name = "reactive") {
  tryCatch({
    if (is.reactive(rv)) {
      return(rv())
    } else {
      return(rv)
    }
  }, error = function(e) {
    debug_log(paste("Error accessing reactive", name, ":", e$message), level = 1)
    return(NULL)
  })
}

#' Safe module function execution with retry logic
#' @param module_func function to execute
#' @param default_return default return value on failure
#' @param context context name for debugging
#' @param max_retries maximum retry attempts
#' @return function result or default_return
safe_module_call <- function(module_func, default_return = NULL, context = "unknown", max_retries = 1) {
  if (is.null(module_func)) {
    debug_log(paste("Module function not available in", context), level = 2)
    return(default_return)
  }

  if (!is.function(module_func)) {
    debug_log(paste("Module", context, "is not a function"), level = 1)
    return(default_return)
  }

  for (attempt in 1:max_retries) {
    tryCatch({
      result <- module_func()
      return(result)
    }, error = function(e) {
      if (attempt < max_retries) {
        debug_log(paste("Error in module call", context, "(attempt", attempt, "):", e$message, "- retrying"), level = 2)
        Sys.sleep(0.1)
      } else {
        debug_log(paste("Error in module call", context, ":", e$message), level = 1)
      }
    })
  }

  return(default_return)
}

#' Initialize module with error handling wrapper
#' @param module_name name of module for debugging
#' @param module_func function that initializes the module
#' @return module result or NULL on error
initialize_datawizard_module_safely <- function(module_name, module_func) {
  tryCatch({
    result <- module_func()
    debug_log(paste(module_name, "module initialized successfully"), level = 2)
    return(result)
  }, error = function(e) {
    debug_log(paste("Error initializing", module_name, "module:", e$message), level = 1)
    showNotification(paste(module_name, "module failed to initialize"), type = "error", duration = 5)
    return(NULL)
  })
}

#' Validate column data type for metadata safety
#' @param column_data vector of column data
#' @return detected data type ("numeric", "character", "unknown")
validate_column_type <- function(column_data) {
  tryCatch({
    test_data <- column_data[!is.na(column_data)]

    if (length(test_data) == 0) return("unknown")
    if (all(test_data == "" | test_data == " ")) return("character")

    numeric_result <- suppressWarnings(as.numeric(as.character(test_data)))
    successful_conversions <- sum(!is.na(numeric_result))
    total_values <- length(test_data)

    if (total_values > 0 && (successful_conversions / total_values) >= 0.8) {
      return("numeric")
    } else {
      return("character")
    }

  }, error = function(e) {
    debug_log(paste("Error in validate_column_type:", e$message), level = 1)
    return("character")
  })
}

#' Get required data type for content classification
#' @param content_type content type string
#' @return required data type ("numeric", "character", "any")
get_required_data_type <- function(content_type) {
  if (is.na(content_type) || content_type == "" || !nzchar(content_type)) {
    return("any")
  }

  numeric_required <- c(
    "# PSMs", "Abundance Ratio", "Abundance Ratio Adj. p-Value",
    "Abundance Ratio p-Value", "Normalized Abundance", "Batch Corrected Abundance",
    "Batch Corrected Normalized Abundance", "Batch Corrected Raw Abundance",
    "Imputed Raw Abundance", "Imputed Normalized Abundance",
    "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
    "Imputed Batch Corrected Abundance", "Raw Abundance"
  )

  character_required <- c("Identifier", "Description")

  if (content_type %in% numeric_required) {
    return("numeric")
  } else if (content_type %in% character_required) {
    return("character")
  }

  if (grepl("PSMs|Abundance|p-Value|P-Value|Ratio", content_type, ignore.case = TRUE)) {
    if (grepl("Abundance|Ratio|PSMs|p-Value|P-Value", content_type, ignore.case = TRUE)) {
      return("numeric")
    }
  }

  if (grepl("Identifier|Description", content_type, ignore.case = TRUE)) {
    return("character")
  }

  return("any")
}

#' Perform metadata safety validation against data structure
#' @param data data frame to validate against
#' @param metadata metadata data frame
#' @return list with corrected metadata, warnings, and correction status
perform_metadata_safety_check <- function(data, metadata) {
  if (is.null(data) || is.null(metadata) || nrow(metadata) == 0) {
    return(list(metadata = metadata, warnings = character(), corrections_made = FALSE))
  }

  tryCatch({
    corrected_metadata <- metadata
    warnings <- character()
    corrections_made <- FALSE

    debug_log(paste("Metadata safety check:", nrow(metadata), "entries against", ncol(data), "columns"), level = 2)

    for (i in seq_len(nrow(metadata))) {
      column_name <- metadata$Column[i]
      content_type <- metadata$Content[i]

      if (is.na(content_type) || content_type == "" || !nzchar(content_type)) next
      if (!column_name %in% names(data)) next

      required_type <- get_required_data_type(content_type)
      if (required_type == "any") next

      column_data <- data[[column_name]]
      actual_type <- validate_column_type(column_data)

      if (required_type != actual_type && actual_type != "unknown") {
        original_content <- content_type
        corrected_metadata$Content[i] <- NA_character_
        corrected_metadata$Options[i] <- NA_character_
        corrected_metadata$Transformation[i] <- NA_character_

        warning_msg <- paste0(
          "Column '", column_name, "': Content type '", original_content,
          "' requires ", required_type, " data, but column contains ",
          actual_type, " data. Content type has been reset."
        )

        warnings <- c(warnings, warning_msg)
        corrections_made <- TRUE
      }
    }

    debug_log(paste("Safety check completed: corrections made =", corrections_made), level = 2)

    return(list(
      metadata = corrected_metadata,
      warnings = warnings,
      corrections_made = corrections_made
    ))

  }, error = function(e) {
    debug_log(paste("Error in metadata safety check:", e$message), level = 1)
    return(list(metadata = metadata, warnings = character(), corrections_made = FALSE))
  })
}

#' Clean metadata to ensure proper Content-Type separation for filtering
#' @param metadata current metadata data.frame
#' @return cleaned metadata with distinct Content types
clean_metadata_for_filtering <- function(metadata) {
  if (is.null(metadata) || nrow(metadata) == 0) {
    return(metadata)
  }

  tryCatch({
    cleaned_metadata <- metadata
    content_types <- unique(metadata$Content[!is.na(metadata$Content)])
    debug_log(paste("Processing", length(content_types), "content types for filtering"), level = 2)

    for (i in seq_len(nrow(cleaned_metadata))) {
      col_name <- cleaned_metadata$Column[i]
      current_content <- cleaned_metadata$Content[i]

      if (is.na(current_content) || !nzchar(current_content)) next

      if (grepl("^Imputed ", col_name) && !grepl("^Imputed ", current_content)) {
        cleaned_metadata$Content[i] <- paste("Imputed", current_content)
      } else if (grepl("^Batch Corrected ", col_name) && !grepl("^Batch Corrected ", current_content)) {
        cleaned_metadata$Content[i] <- paste("Batch Corrected", current_content)
      }
    }

    return(cleaned_metadata)

  }, error = function(e) {
    debug_log(paste("Error cleaning metadata for filtering:", e$message), level = 1)
    return(metadata)
  })
}

#' Validate filtering configuration structure
#' @param config filtering configuration
#' @return validated configuration or NULL
validate_filtering_config_structure <- function(config) {
  if (is.null(config) || !is.list(config)) {
    return(NULL)
  }

  tryCatch({
    validated_config <- list()

    if (!is.null(config$confidence)) {
      if (is.list(config$confidence)) {
        validated_config$confidence <- config$confidence
      } else {
        debug_log("Invalid confidence section in filtering config", level = 1)
        validated_config$confidence <- list(
          numeric_enabled = FALSE,
          string_enabled = FALSE,
          numeric_max = NULL,
          numeric_min = NULL,
          string_input = ""
        )
      }
    }

    if (!is.null(config$valid_values)) {
      if (is.list(config$valid_values)) {
        validated_config$valid_values <- config$valid_values
      } else {
        debug_log("Invalid valid_values section in filtering config", level = 1)
        validated_config$valid_values <- list(
          group_selection = "In total",
          min_count = 1
        )
      }
    }

    if (!is.null(config$custom)) {
      if (is.data.frame(config$custom)) {
        validated_config$custom <- config$custom
      } else {
        debug_log("Invalid custom section in filtering config", level = 1)
        validated_config$custom <- data.frame(
          Column = character(),
          Operator_1 = character(),
          Value_1 = character(),
          Logic = character(),
          Operator_2 = character(),
          Value_2 = character(),
          Empty_Filter = character(),
          Multi_Column_Logic = character(),
          stringsAsFactors = FALSE
        )
      }
    }

    return(validated_config)

  }, error = function(e) {
    debug_log(paste("Error validating filtering config structure:", e$message), level = 1)
    return(NULL)
  })
}

#' Manual filtering test function for debugging
test_filtering_manually <- function(primary_data_raw, handson_metadata, filtering_valid_values, filtering_out) {
  debug_log("=== MANUAL FILTER TEST ===", level = 2)

  current_data <- primary_data_raw()
  current_metadata <- handson_metadata()

  if (is.null(current_data) || is.null(current_metadata)) {
    debug_log("Data or metadata missing", level = 1)
    return()
  }

  debug_log(paste("Input:", nrow(current_data), "rows"), level = 2)

  valid_settings <- filtering_valid_values()
  if (!is.null(valid_settings)) {
    debug_log(paste("Valid values filter - Group:", valid_settings$valid_filtering_group_dw,
                    "Min count:", valid_settings$valid_filtering_value_dw), level = 2)

    if (!is.null(filtering_out$apply_valid_values_filter)) {
      result <- filtering_out$apply_valid_values_filter(current_data, current_metadata, valid_settings)
      debug_log(paste("Manual filter result:", nrow(result), "rows remaining"), level = 2)
    }
  }

  debug_log("=== END MANUAL TEST ===", level = 2)
}

#' Reset reactive values when primary data is loaded, preserving additional data
#' @param rv reactive values object to reset
#' @param preserve_additional logical, whether to preserve additional data (default TRUE)
#' @param debug_level debug level for logging
#' @export
reset_rv_for_primary_data <- function(rv, preserve_additional = TRUE, debug_level = 0) {
  # Compatibility reset inventory (classification is architectural, not an
  # invitation to remove these mirrors while external modules retain `rv`):
  # canonical data: data_mod, data_def, data_raw.
  # derived cache: metadata_content_ready, metadata_skeleton, init_metadata,
  #   current_metadata, filtered_dataset_log, imputation_log, filtering_log,
  #   tables_metadata, filtering_metadata, edit_metadata.
  # module state: filter_applied, apply_triggered, data_modified,
  #   modification_history, imputation_setting, filtering_confidence,
  #   filtering_valid_values, filtered_conditions, ab_validate,
  #   ui_config_errors, filtering_config_errors, last_config_application_time,
  #   central_rule_file, central_loaded_rules, rule_application_state.
  # legacy mirror: handson_metadata, primary_data_raw, filtered_data,
  #   final_processed_data, final_processed_metadata.

  # Helper function for controlled debug output
  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "RV RESET", message)
    } else {
      effective_level <- tryCatch(
        get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
        error = function(e) debug_level
      )
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ RV RESET ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  tryCatch({
    debug_log("Starting comprehensive rv reset for primary data loading", 1)

    # Backup additional data if preservation is requested
    additional_data_backup <- NULL
    additional_metadata_backup <- NULL

    if (preserve_additional) {
      if (!is.null(rv$data_additional)) {
        additional_data_backup <- rv$data_additional
        debug_log("Additional data backed up for preservation", 2)
      }
      if (!is.null(rv$data_def_additional)) {
        additional_metadata_backup <- rv$data_def_additional
        debug_log("Additional metadata backed up for preservation", 2)
      }
    }

    # CRITICAL: Reset ALL metadata-related values first
    rv$data_mod <- NULL
    rv$data_def <- NULL
    rv$data_raw <- NULL
    rv$handson_metadata <- NULL
    rv$primary_data_raw <- NULL

    # Force clear any cached metadata states
    rv$metadata_content_ready <- FALSE
    rv$metadata_skeleton <- NULL
    rv$init_metadata <- NULL
    rv$current_metadata <- NULL

    # Reset processing states that could interfere with metadata creation
    rv$filter_applied <- FALSE
    rv$filtered_data <- NULL
    rv$filtered_dataset_log <- NULL
    rv$apply_triggered <- FALSE
    rv$final_processed_data <- NULL
    rv$final_processed_metadata <- NULL

    # Reset modification tracking
    rv$data_modified <- FALSE
    rv$modification_history <- list()

    # Reset processing logs and settings
    rv$imputation_log <- NULL
    rv$imputation_setting <- NULL
    rv$filtering_confidence <- NULL
    rv$filtering_valid_values <- NULL
    rv$filtered_conditions <- NULL
    rv$filtering_log <- NULL

    # Reset validation states
    rv$ab_validate <- FALSE

    # Reset UI configuration states that might prevent metadata updates
    rv$ui_config_errors <- list()
    rv$filtering_config_errors <- list()
    rv$last_config_application_time <- NULL

    # Reset rule management but keep available rules
    rv$central_rule_file <- ""
    rv$central_loaded_rules <- NULL
    rv$rule_application_state <- "idle"

    # Clear any module-specific metadata caches
    rv$tables_metadata <- NULL
    rv$filtering_metadata <- NULL
    rv$edit_metadata <- NULL

    debug_log("All rv values reset completed", 1)

    # Restore additional data if preservation was requested
    if (preserve_additional) {
      if (!is.null(additional_data_backup)) {
        rv$data_additional <- additional_data_backup
        debug_log("Additional data restored", 2)
      }
      if (!is.null(additional_metadata_backup)) {
        rv$data_def_additional <- additional_metadata_backup
        debug_log("Additional metadata restored", 2)
      }
    }

    debug_log("Comprehensive rv reset for primary data loading completed successfully", 1)
    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error during comprehensive rv reset:", e$message), 1)
    return(FALSE)
  })
}

#' Force reset metadata in core_values when new primary data is loaded
#' @param core_values core reactive values object
#' @param debug_level debug level for logging
#' @export
reset_core_metadata_for_new_data <- function(core_values, debug_level = 0) {

  # Helper function for controlled debug output
  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "CORE METADATA RESET", message)
    } else {
      effective_level <- tryCatch(
        get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
        error = function(e) debug_level
      )
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ CORE METADATA RESET ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  tryCatch({
    debug_log("Starting core metadata reset for new primary data", 1)

    # Force clear all metadata-related reactive values
    core_values$handson_metadata(NULL)
    core_values$final_processed_metadata(NULL)

    # Clear any cached metadata states
    if (!is.null(core_values$metadata_skeleton)) {
      core_values$metadata_skeleton(NULL)
    }

    debug_log("Core metadata reset completed", 1)
    return(TRUE)

  }, error = function(e) {
    debug_log(paste("Error during core metadata reset:", e$message), 1)
    return(FALSE)
  })
}

# modules/Data Wizard/datawizard_auto_assign.R (oder Utils-Datei)

set_ratio_or_identifier_options <- function(df, idx, content_term) {
  # Ratio-Typen, die Options = "Ratio" erhalten sollen
  ratio_terms <- c(
    "Abundance Ratio",
    "Abundance Ratio p-Value",
    "Abundance Ratio Adj. p-Value"
  )

  if (content_term %in% ratio_terms) {
    df$Options[idx] <- "Ratio"
  }

  if (grepl("Identifier", content_term, fixed = TRUE)) {
    df$Options[idx] <- df$Column[idx]
  }

  df
}
