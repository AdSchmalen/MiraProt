# ============================================================================
# Module/Sub-script: modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R
# Purpose:
#   Shared helper library for assign-rules workflows, including payload
#   validation, rule-config extraction, metadata processing utilities, and
#   operational logging helpers used by the assign-rules server.
#
# Architectural Role:
#   business utils
#
# Responsibilities:
#   - Validate assign-rules configuration payloads and processing prerequisites.
#   - Extract typed UI configurations from imported rule-set structures.
#   - Provide reusable data-processing and operational helper functions.
#
# Non-Responsibilities:
#   - Own end-to-end module event orchestration or UI rendering.
#   - Define caller-facing module contracts outside helper function boundaries.
#
# Allowed Dependencies:
#   - Base R and Shiny utility functions needed by helper implementations.
#   - Tidyverse/dplyr data-manipulation helpers used by processing functions.
#
# Interaction Boundaries:
#   - Inputs: rule payload objects, metadata frames, optional reactive handles,
#             and optional debug callbacks.
#   - Outputs: validated/extracted payloads, transformed data frames, and status
#              values consumed by assign-rules orchestration.
#   - Side Effects: optional logging/error reactive updates when handles exist.
#
# Stability Guarantees:
#   - Keep helper function names and semantics stable for orchestrator usage.
#   - Keep graceful fallback behavior for malformed or partial rule payloads.
# ============================================================================


############
# Core Helper Functions for Sample Processing

#' Check whether metadata still needs automatic sample assignment
#'
#' @param df A validated metadata data frame.
#' @return `TRUE` when at least one sample-bearing row has no sample value.
metadata_needs_sample_processing <- function(df) {
  if (!"Sample" %in% names(df)) {
    return(any(is_sample_bearing_content(df$Content)))
  }

  sample_missing <- is.na(df$Sample) | !nzchar(trimws(as.character(df$Sample)))
  any(is_sample_bearing_content(df$Content) & sample_missing)
}

#' Process dataframe to automatically generate sample names
#'
#' Analyzes column names to identify unique patterns and generate sample identifiers
#' @param df metadata data.frame with Column, Content, Options columns
#' @return processed data.frame with Sample column populated
#' @export
process_dataframe <- function(df) {
  # Input validation
  required_cols <- c("Column", "Content", "Options")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # Ensure Sample column exists and is character type
  if (!"Sample" %in% names(df)) {
    df$Sample <- NA_character_
  }

  if (!is.character(df$Sample)) {
    df$Sample <- as.character(df$Sample)
  }

  # Rows without a column name cannot provide evidence for sample generation.
  # Determine all work up front so fully populated data does not call
  # process_sample() as a side effect of eager vector evaluation.
  eligible <- !is.na(df$Column) & nzchar(df$Column) &
    is_sample_bearing_content(df$Content)
  eligible_and_missing <- eligible & (
    is.na(df$Sample) | !nzchar(trimws(df$Sample))
  )

  if (!any(eligible_and_missing)) {
    return(df)
  }

  # Group all eligible rows, not just the missing rows: existing rows in an
  # affected group are still needed when process_sample() compares columns.
  eligible_rows <- which(eligible)
  grouped_rows <- df[eligible_rows, , drop = FALSE] %>%
    dplyr::group_by(Content, Options) %>%
    dplyr::group_rows()

  for (group_positions in grouped_rows) {
    group_rows <- eligible_rows[group_positions]
    missing_positions <- which(eligible_and_missing[group_rows])

    if (length(missing_positions) == 0L) {
      next
    }

    generated <- process_sample(
      df$Column[group_rows],
      df$Options[group_rows]
    )
    df$Sample[group_rows[missing_positions]] <- generated[missing_positions]
  }

  return(df)
}

#' Process sample names from column names and options
#'
#' @param strings character vector of column names
#' @param options character string with additional options
#' @return character vector of processed sample names
#' @export
process_sample <- function(strings, options) {
  if (length(strings) < 1) {
    return(character(0))
  }

  # Handle single string case
  if (length(strings) == 1) {
    safe_options <- ifelse(is.na(options), "", options)
    result <- gsub("_+", "_", safe_options)
    return(result)
  }

  # Handle multiple strings
  valid_strings <- strings[!is.na(strings)]
  valid_strings <- gsub("_", " ", valid_strings)

  if (length(valid_strings) <= 1) {
    return(rep(NA_character_, length(strings)))
  }

  tryCatch({
    differences <- compare_consecutive_strings(valid_strings)
    combined_strings <- combine_strings(differences)
    combined_strings <- unlist(combined_strings)

    # Ensure result length matches input
    if (length(combined_strings) < length(strings)) {
      combined_strings <- c(combined_strings, rep(NA, length(strings) - length(combined_strings)))
    }

    safe_options <- ifelse(is.na(options), "", options)

    # Combine strings with options
    result <- ifelse(!is.na(combined_strings) & nzchar(combined_strings),
                     paste0(combined_strings, "_", safe_options),
                     NA_character_)
    result <- gsub("_+", "_", result)

    return(result)
  }, error = function(e) {
    return(rep(NA_character_, length(strings)))
  })
}

#' Post-process dataframe to clean up sample assignments
#'
#' @param df processed metadata data.frame
#' @return cleaned data.frame with appropriate NA assignments
#' @export
postprocess_dataframe <- function(df) {
  required_cols <- c("Content", "Options", "Sample")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  df %>%
    dplyr::mutate(
      Sample = dplyr::if_else(
        is_sample_bearing_content(Content),
        Sample,
        NA_character_,
        missing = Sample
      )
    )
}

#' Enhanced boolean check function with robust validation
#' @param x value to check
#' @return logical TRUE/FALSE
safe_is_true <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)

  # Handle different input types
  if (is.logical(x)) return(isTRUE(x[1]))
  if (is.numeric(x)) return(x[1] > 0)
  if (is.character(x)) return(tolower(x[1]) %in% c("true", "t", "yes", "y", "1"))
  if (is.list(x)) {
    # Check for success indicators
    if (!is.null(x$success)) return(safe_is_true(x$success))
    if (!is.null(x$data)) return(TRUE)
    return(length(x) > 0)
  }

  return(FALSE)
}

############
# UI Configuration Validation Functions

#' Validate imputation UI configuration with comprehensive checks
validate_ui_imputation_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    required_fields <- c("imputation_method_select", "imputation_column_select")
    missing_fields <- setdiff(required_fields, names(ui_config))

    if (length(missing_fields) > 0) {
      return(FALSE)
    }

    # Validate method
    valid_methods <- c("None", "left-censored", "Random forest", "MICE - CART")
    if (!is.null(ui_config$imputation_method_select) &&
        !ui_config$imputation_method_select %in% valid_methods) {
      return(FALSE)
    }

    # Validate columns
    if (!is.null(ui_config$imputation_column_select) &&
        !is.character(ui_config$imputation_column_select)) {
      return(FALSE)
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate filtering UI configuration
validate_ui_filtering_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    # Check sections if present
    if (!is.null(ui_config$confidence) && !is.list(ui_config$confidence)) return(FALSE)
    if (!is.null(ui_config$valid_values) && !is.list(ui_config$valid_values)) return(FALSE)
    if (!is.null(ui_config$custom) && !is.data.frame(ui_config$custom)) return(FALSE)

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate batch effects UI configuration
validate_ui_batch_effects_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    if (!is.null(ui_config$batch_method)) {
      valid_methods <- c("Offset Correction", "ComBat", "Limma", "LOESS", "Quantile")
      if (!ui_config$batch_method %in% valid_methods) {
        return(FALSE)
      }
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate pivot UI configuration
validate_ui_pivot_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    if (!is.null(ui_config$pivot_type_dw)) {
      valid_types <- c("wider", "longer")
      if (!ui_config$pivot_type_dw %in% valid_types) {
        return(FALSE)
      }
    }

    if (!is.null(ui_config$pivot_data_dw)) {
      valid_data <- c("primary", "secondary")
      if (!ui_config$pivot_data_dw %in% valid_data) {
        return(FALSE)
      }
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate merge UI configuration
validate_ui_merge_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    if (!is.null(ui_config$join_type)) {
      valid_types <- c("left", "inner", "full")
      if (!ui_config$join_type %in% valid_types) {
        return(FALSE)
      }
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate ratios UI configuration
validate_ui_ratios_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    # Basic structure validation for ratios
    if (!is.null(ui_config$ratio_configurations) && !is.data.frame(ui_config$ratio_configurations)) {
      return(FALSE)
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate edit UI configuration
validate_ui_edit_config <- function(ui_config) {
  if (is.null(ui_config)) return(TRUE)
  if (!is.list(ui_config)) return(FALSE)

  tryCatch({
    # Ensure operations_table is always present as data.frame
    if (is.null(ui_config$operations_table)) {
      # Create empty operations table with required structure
      ui_config$operations_table <- data.frame(
        Operation = character(0),
        Type = character(0),
        Columns = character(0),
        Parameters = character(0),
        Description = character(0),
        Executed = logical(0),
        stringsAsFactors = FALSE
      )
    }

    # Validate operations_table structure
    if (!is.data.frame(ui_config$operations_table)) {
      return(FALSE)
    }

    # Check required columns for operations table
    required_cols <- c("Operation", "Type", "Columns", "Parameters", "Description")
    if (!all(required_cols %in% names(ui_config$operations_table))) {
      return(FALSE)
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate ratio configurations structure
validate_ratio_configurations_structure <- function(ratio_configs) {
  if (is.null(ratio_configs)) return(TRUE)
  if (!is.data.frame(ratio_configs)) return(FALSE)

  tryCatch({
    required_cols <- c("Title", "Content", "Numerator", "Denominator", "Statistics")
    missing_cols <- setdiff(required_cols, names(ratio_configs))

    if (length(missing_cols) > 0) {
      return(FALSE)
    }

    # Validate data types and constraints
    if (nrow(ratio_configs) > 0) {
      if (!is.list(ratio_configs$Numerator) || !is.list(ratio_configs$Denominator)) {
        return(FALSE)
      }

      valid_methods <- c("ANOVA", "Welch's T-Test", "Moderated Welch Test", "Limma", "DEqMS", "Mann-Whitney U Test")
      invalid_methods <- setdiff(ratio_configs$Statistics, valid_methods)
      if (length(invalid_methods) > 0) {
        return(FALSE)
      }

      if (length(unique(ratio_configs$Title)) != nrow(ratio_configs)) {
        return(FALSE)
      }
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# ===============================================================
# Validate Basemean UI configuration structure
# ===============================================================
validate_ui_basemean_config <- function(config) {
  if (is.null(config)) return(FALSE)
  tryCatch({
    if (!is.list(config)) return(FALSE)
    required_fields <- c("abundance_type", "samples")
    missing <- setdiff(required_fields, names(config))
    if (length(missing) > 0) return(FALSE)
    if (is.null(config$abundance_type) || !is.character(config$abundance_type) || any(is.na(config$abundance_type))) return(FALSE)
    if (is.null(config$samples) || !is.character(config$samples) || any(is.na(config$samples))) return(FALSE)
    if (!is.null(config$suffix) && (!is.character(config$suffix) || length(config$suffix) != 1)) return(FALSE)
    return(TRUE)
  }, error = function(e) FALSE)
}

############
# Generic UI Configuration Management Functions

#' Generic function to extract UI configuration with retry logic
#'
#' @param rule_data loaded rule data structure
#' @param config_path path to configuration in rule_data
#' @param validator validation function
#' @param config_name name for logging/error reporting
#' @param max_retries maximum number of retry attempts
#' @param debug_log debug logging function
#' @param ui_config_errors reactive for storing errors
#' @return configuration object or NULL
extract_ui_config <- function(rule_data, config_path, validator, config_name, max_retries = 2, debug_log = NULL, ui_config_errors = NULL, log_missing_path = FALSE) {
  if (!is.list(rule_data)) {
    if (!is.null(debug_log)) debug_log(paste("Rule data is not a list for", config_name), 2)
    return(NULL)
  }

  for (attempt in seq_len(max_retries)) {
    if (attempt > 1) {
      if (!is.null(debug_log)) debug_log(paste("Retry attempt", attempt, "for", config_name, "config extraction"), 2)
      Sys.sleep(0.1)  # Brief pause before retry
    }

    result <- tryCatch({
      config <- rule_data
      for (path_element in config_path) {
        config <- config[[path_element]]
        if (is.null(config)) {
          if (log_missing_path && !is.null(debug_log)) debug_log(paste("Path element", path_element, "not found for", config_name), 2)
          return(NULL)
        }
      }

      if (validator(config)) {
        if (!is.null(debug_log)) debug_log(paste("Successfully extracted", config_name, "config on attempt", attempt), 2)
        return(config)
      } else {
        if (!is.null(debug_log)) debug_log(paste("Invalid", config_name, "config structure on attempt", attempt), 1)
        if (attempt == max_retries && !is.null(ui_config_errors)) {
          current_errors <- ui_config_errors()
          ui_config_errors(append(current_errors, paste("Invalid", config_name, "config structure")))
        }
        return(NULL)
      }
    }, error = function(e) {
      if (attempt == max_retries) {
        if (!is.null(debug_log)) debug_log(paste(config_name, "config extraction error:", e$message), 1)
        if (!is.null(ui_config_errors)) {
          current_errors <- ui_config_errors()
          ui_config_errors(append(current_errors, paste(config_name, "config extraction error:", e$message)))
        }
      }
      return(NULL)
    })

    if (!is.null(result)) {
      return(result)
    }
  }

  return(NULL)
}

#' Generic function to set UI configuration safely with enhanced validation
#'
#' @param ui_config configuration to set
#' @param reactive_setter reactive setter function
#' @param validator validation function
#' @param config_name name for notifications
#' @param source_name source identifier
#' @param max_retries maximum number of retry attempts
#' @param debug_log debug logging function
#' @param ui_config_errors reactive for storing errors
#' @param ui_config_sources reactive for tracking sources
#' @return logical indicating success
set_ui_config_safe <- function(ui_config, reactive_setter, validator, config_name, source_name, max_retries = 2,
                               debug_log = NULL, ui_config_errors = NULL, ui_config_sources = NULL) {
  if (!validator(ui_config)) {
    if (!is.null(debug_log)) debug_log(paste("Invalid", config_name, "config provided"), 1)
    if (!is.null(ui_config_errors)) {
      current_errors <- ui_config_errors()
      ui_config_errors(append(current_errors, paste("Invalid", config_name, "config provided")))
    }
    return(FALSE)
  }

  for (attempt in seq_len(max_retries)) {
    if (attempt > 1) {
      if (!is.null(debug_log)) debug_log(paste("Retry attempt", attempt, "for setting", config_name, "config"), 2)
      Sys.sleep(0.1)
    }

    success <- tryCatch({
      # Use isolate to avoid reactive dependencies during setting
      current_config <- isolate(reactive_setter())

      if (!identical(current_config, ui_config)) {
        reactive_setter(ui_config)

        # Update tracking
        if (!is.null(ui_config_sources)) {
          current_sources <- isolate(ui_config_sources())
          current_sources[[source_name]] <- "rule_file"
          ui_config_sources(current_sources)
        }

        if (!is.null(debug_log)) debug_log(paste(config_name, "config applied successfully on attempt", attempt), 2)
        return(TRUE)
      }
      return(TRUE)

    }, error = function(e) {
      if (attempt == max_retries) {
        if (!is.null(debug_log)) debug_log(paste(config_name, "config setting error:", e$message), 1)
        if (!is.null(ui_config_errors)) {
          current_errors <- ui_config_errors()
          ui_config_errors(append(current_errors, paste(config_name, "config setting error:", e$message)))
        }
      }
      return(FALSE)
    })

    if (success) {
      return(TRUE)
    }
  }

  return(FALSE)
}

############
# UI Configuration Extractors

#' Detect whether loaded rule data contains optional UI configuration payloads
#'
#' Rule files can be rules-only templates or full templates that also carry UI
#' state. Missing optional UI payloads are expected for rules-only templates and
#' should be reported once by the load handler rather than once per module.
#'
#' @param rule_data loaded rule data structure
#' @return TRUE when at least one known UI configuration payload is present
rule_data_has_optional_ui_config <- function(rule_data) {
  if (!is.list(rule_data)) return(FALSE)

  optional_top_level_sections <- c(
    "UI_config",
    "filter_template",
    "imputation_defaults",
    "ratio_configurations",
    "basemean_configurations"
  )

  any(optional_top_level_sections %in% names(rule_data))
}

# Rule-file import is deliberately split from the observer so every consumer
# gets the same, side-effect-free gate before looking at optional UI state.
ASSIGN_RULES_FORMAT_VERSION <- 2L
ASSIGN_RULES_CAPABILITIES <- c(
  "grouped-variants-v1", "stable-rule-id-v1", "explicit-priority-v1",
  "transformation-owner-v1"
)

assign_rules_load_status <- function(ok, phase, code = if (ok) "ok" else "failed",
                                     message = "", details = list()) {
  structure(list(ok = isTRUE(ok), phase = as.character(phase), code = as.character(code),
                 message = as.character(message), details = details),
            class = c("assign_rules_load_status", "list"))
}

.assign_rules_abort <- function(phase, code, message, details = list()) {
  condition <- simpleError(message)
  condition$assign_rules_status <- assign_rules_load_status(FALSE, phase, code, message, details)
  stop(condition)
}

.assign_rules_legacy_frames <- function(x) {
  is.list(x) && all(c("table", "condition", "ratio") %in% names(x)) &&
    all(vapply(x[c("table", "condition", "ratio")], is.data.frame, logical(1)))
}

# Deterministic legacy migration intentionally adds no columns and performs no
# coercion: the Auto-Assign contract boundary owns the historical row upgrade.
# Keeping these frames byte/value/order equivalent prevents this UI loader from
# becoming a second, subtly different migration implementation.
migrate_assign_rules_legacy_envelope <- function(x) {
  if (!.assign_rules_legacy_frames(x))
    .assign_rules_abort("migration", "invalid_legacy", "Legacy rule files require table, condition, and ratio data frames.")
  rules <- lapply(x[c("table", "condition", "ratio")], function(frame) frame[, names(frame), drop = FALSE])
  extras <- x[setdiff(names(x), c("table", "condition", "ratio"))]
  list(
    RuleFormatVersion = 1L,
    RequiredCapabilities = character(),
    MigrationProfile = "legacy-v1-preserved",
    table = rules$table, condition = rules$condition, ratio = rules$ratio,
    provenance = extras$provenance %||% list(),
    contrast_mappings = extras$contrast_mappings %||% list(),
    UI_config = extras$UI_config,
    legacy_optional = extras
  )
}

validate_assign_rules_rule_envelope <- function(envelope) {
  if (!is.list(envelope)) .assign_rules_abort("validation", "not_envelope", "Rule envelope must be a list.")
  version <- envelope$RuleFormatVersion
  capabilities <- envelope$RequiredCapabilities
  if (length(version) != 1L || is.na(version) || !is.integer(version))
    .assign_rules_abort("validation", "invalid_version", "RuleFormatVersion must be one integer value.")
  if (version < 1L || version > ASSIGN_RULES_FORMAT_VERSION)
    .assign_rules_abort("validation", "unsupported_version", paste("Unsupported rule format version:", version))
  if (!is.character(capabilities) || anyNA(capabilities) || any(!nzchar(capabilities)))
    .assign_rules_abort("validation", "invalid_capabilities", "RequiredCapabilities must be a non-missing character vector.")
  unknown <- setdiff(capabilities, ASSIGN_RULES_CAPABILITIES)
  if (length(unknown)) .assign_rules_abort("validation", "unsupported_capability",
    paste("Unsupported rule capabilities:", paste(unknown, collapse = ", ")), list(capabilities = unknown))
  rules <- if (.assign_rules_legacy_frames(envelope$rules)) envelope$rules else envelope
  if (!.assign_rules_legacy_frames(rules))
    .assign_rules_abort("validation", "missing_rules", "Envelope must contain complete table, condition, and ratio rule frames.")

  # V1 remains losslessly preserved for the existing Auto-Assign migrator.  A
  # declared V2 payload, however, must satisfy the complete grouped contract.
  if (identical(version, 2L)) {
    missing_caps <- setdiff(ASSIGN_RULES_CAPABILITIES, capabilities)
    if (length(missing_caps)) .assign_rules_abort("validation", "incomplete_capabilities",
      paste("Grouped format is missing capabilities:", paste(missing_caps, collapse = ", ")))
    required <- list(
      table = c("RuleId", "VariantId", "Priority", "Content", "Include", "Exclude", "Transformation", "TransformationOwner"),
      condition = c("RuleId", "VariantId", "Priority", "Content", "Method", "Before", "After", "Separators", "Pos"),
      ratio = c("RuleId", "VariantId", "Priority", "Content", "Method", "Separators", "Invert", "NumBefore", "NumAfter", "DenBefore", "DenAfter", "NumPos", "DenPos")
    )
    for (kind in names(required)) if (!identical(names(rules[[kind]]), required[[kind]]))
      .assign_rules_abort("validation", "invalid_schema", paste("Invalid", kind, "rule schema."), list(component = kind))
    all_ids <- unlist(lapply(rules, `[[`, "RuleId"), use.names = FALSE)
    if (!is.character(all_ids) || anyNA(all_ids) || any(!nzchar(all_ids)) || anyDuplicated(all_ids))
      .assign_rules_abort("validation", "invalid_identity", "RuleId values must be nonblank and globally unique.")
    variants <- rules$table$VariantId
    if (!is.character(variants) || anyNA(variants) || any(!nzchar(variants)))
      .assign_rules_abort("validation", "invalid_variant", "Content VariantId values must be nonblank.")
    for (kind in names(rules)) {
      priority <- rules[[kind]]$Priority
      if (!is.integer(priority) || anyNA(priority) || any(priority < 1L) || anyDuplicated(priority) ||
          !identical(priority, sort(priority)))
        .assign_rules_abort("validation", "invalid_priority", paste("Invalid", kind, "priorities."))
    }
    for (kind in c("condition", "ratio")) {
      if (any(!rules[[kind]]$VariantId %in% variants))
        .assign_rules_abort("validation", "foreign_key", paste(kind, "contains an unknown VariantId."))
      lookup <- stats::setNames(rules$table$Content, rules$table$VariantId)
      if (any(unname(lookup[rules[[kind]]$VariantId]) != rules[[kind]]$Content))
        .assign_rules_abort("validation", "content_foreign_key", paste(kind, "Content does not match its variant."))
    }
    row_index <- which(rules$table$Content == "Row Index")
    if (length(row_index) != 1L || !identical(rules$table$Include[row_index], "Row Index") ||
        !identical(rules$table$Exclude[row_index], "") || !is.na(rules$table$Transformation[row_index]) ||
        !isTRUE(rules$table$TransformationOwner[row_index]) ||
        any(rules$condition$VariantId %in% variants[row_index]) || any(rules$ratio$VariantId %in% variants[row_index]))
      .assign_rules_abort("validation", "row_index", "The canonical Row Index rule is invalid.")
    owner_count <- vapply(split(rules$table$TransformationOwner, variants), function(x) sum(x %in% TRUE), integer(1))
    if (any(owner_count != 1L)) .assign_rules_abort("validation", "transformation_owner", "Each variant requires exactly one transformation owner.")
  }
  if (!is.list(envelope$provenance %||% list()))
    .assign_rules_abort("validation", "invalid_provenance", "Provenance mappings must be a list.")
  if (!is.list(envelope$contrast_mappings %||% list()))
    .assign_rules_abort("validation", "invalid_contrasts", "Contrast mappings must be a list.")
  invisible(envelope)
}

prepare_assign_rules_rule_envelope <- function(rule_data) {
  migrated <- is.null(rule_data$RuleFormatVersion)
  envelope <- if (migrated) migrate_assign_rules_legacy_envelope(rule_data) else rule_data
  validate_assign_rules_rule_envelope(envelope)
  frames <- if (.assign_rules_legacy_frames(envelope$rules)) envelope$rules else envelope[c("table", "condition", "ratio")]
  list(envelope = envelope, migrated = migrated,
       rules_data = if (migrated) rule_data else c(frames, list(
         provenance = envelope$provenance, contrast_mappings = envelope$contrast_mappings,
         required_capabilities = envelope$RequiredCapabilities,
         UI_config = envelope$UI_config)))
}


#' Extract functions for different module configurations
extract_ui_imputation_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  # Try primary path
  config <- extract_ui_config(rule_data, c("UI_config", "UI_imputation"), validate_ui_imputation_config, "imputation",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)
  if (!is.null(config)) return(config)

  # Try backward compatibility
  config <- extract_ui_config(rule_data, c("imputation_defaults"), function(x) TRUE, "imputation",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)
  if (!is.null(config)) {
    converted_config <- list(
      imputation_method_select = config$method %||% "None",
      imputation_column_select = config$columns %||% character(0)
    )
    if (validate_ui_imputation_config(converted_config)) {
      if (!is.null(debug_log)) debug_log("Converted legacy imputation config successfully", 2)
      return(converted_config)
    }
  }

  return(NULL)
}

extract_ui_filtering_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  # Try primary path
  config <- extract_ui_config(rule_data, c("UI_config", "filtering"), validate_ui_filtering_config, "filtering",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)
  if (!is.null(config)) return(config)

  # Try backward compatibility
  config <- extract_ui_config(rule_data, c("filter_template"), validate_ui_filtering_config, "filtering",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)
  return(config)
}

extract_ui_batch_effects_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  return(extract_ui_config(rule_data, c("UI_config", "batch_effects"), validate_ui_batch_effects_config, "batch_effects",
                           debug_log = debug_log, ui_config_errors = ui_config_errors))
}

extract_ui_pivot_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  return(extract_ui_config(rule_data, c("UI_config", "pivot"), validate_ui_pivot_config, "pivot",
                           debug_log = debug_log, ui_config_errors = ui_config_errors))
}

extract_ui_merge_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  return(extract_ui_config(rule_data, c("UI_config", "merge"), validate_ui_merge_config, "merge",
                           debug_log = debug_log, ui_config_errors = ui_config_errors))
}

extract_ui_edit_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  config <- extract_ui_config(rule_data, c("UI_config", "edit"), validate_ui_edit_config, "edit",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)

  # Always ensure operations_table exists with proper structure
  if (!is.null(config)) {
    if (is.null(config$operations_table) || !is.data.frame(config$operations_table)) {
      config$operations_table <- data.frame(
        Operation = character(0),
        Type = character(0),
        Columns = character(0),
        Parameters = character(0),
        Description = character(0),
        Executed = logical(0),
        stringsAsFactors = FALSE
      )
      if (!is.null(debug_log)) debug_log("Created empty operations_table for edit config", 2)
    } else {
      # Validate existing operations_table has correct columns
      required_cols <- c("Operation", "Type", "Columns", "Parameters", "Description")
      if (!all(required_cols %in% names(config$operations_table))) {
        if (!is.null(debug_log)) debug_log("Operations table missing required columns, creating empty table", 1)
        config$operations_table <- data.frame(
          Operation = character(0),
          Type = character(0),
          Columns = character(0),
          Parameters = character(0),
          Description = character(0),
          Executed = logical(0),
          stringsAsFactors = FALSE
        )
      } else {
        # Ensure Executed column exists
        if (!"Executed" %in% names(config$operations_table)) {
          config$operations_table$Executed <- FALSE
          if (!is.null(debug_log)) debug_log("Added Executed column to operations_table", 2)
        }
      }
    }
  }

  return(config)
}

extract_ui_ratios_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  # Try primary path
  config <- extract_ui_config(rule_data, c("UI_config", "ratios"), validate_ui_ratios_config, "ratios",
                              debug_log = debug_log, ui_config_errors = ui_config_errors)
  if (!is.null(config)) return(config)

  # Try direct ratio_configurations
  ratio_configs <- extract_ui_config(rule_data, c("ratio_configurations"), validate_ratio_configurations_structure, "ratio_configurations",
                                     debug_log = debug_log, ui_config_errors = ui_config_errors)
  if (!is.null(ratio_configs)) {
    ui_config <- list(
      ratio_settings = list(
        custom_col_sel = "Normalized Abundance",
        statistics_sel = "Limma",
        adjust_sel = "FDR",
        column_prefix = "Ratio_"
      ),
      ratio_configurations = ratio_configs
    )
    if (validate_ui_ratios_config(ui_config)) {
      if (!is.null(debug_log)) debug_log("Created ratios config from configurations", 2)
      return(ui_config)
    }
  }

  return(NULL)
}

extract_ui_basemean_config <- function(rule_data, debug_log = NULL, ui_config_errors = NULL) {
  tryCatch({
    # First, check nested UI_config structure
    config <- extract_ui_config(rule_data, c("UI_config", "basemean"),
                                validate_ui_basemean_config, "basemean",
                                debug_log = debug_log, ui_config_errors = ui_config_errors)
    if (!is.null(config)) {
      if (!is.null(debug_log)) debug_log("Basemean UI configuration extracted from UI_config", 2)
      return(config)
    }

    # Then, check direct entry (like in your RDS structure)
    if (!is.null(rule_data$basemean_configurations)) {
      basemean_config <- rule_data$basemean_configurations

      if (validate_ui_basemean_config(basemean_config)) {
        if (!is.null(debug_log)) debug_log("Basemean UI configuration extracted from basemean_configurations", 2)
        return(basemean_config)
      } else {
        if (!is.null(debug_log)) debug_log("Basemean configuration found but invalid", 2)
      }
    }

    # Nothing found
    if (!is.null(debug_log)) debug_log("No Basemean configuration found in imported data", 2)
    return(NULL)
  }, error = function(e) {
    if (!is.null(debug_log)) debug_log(paste("Error extracting Basemean configuration:", e$message), 1)
    if (!is.null(ui_config_errors)) ui_config_errors$add(paste("Basemean extraction failed:", e$message))
    return(NULL)
  })
}

############
# Error and Log Management Functions

#' Add entry to processing log with enhanced error tracking
add_processing_log <- function(step, status, message = "", duration = 0, processing_log = NULL, processing_errors = NULL, processing_warnings = NULL, debug_log = NULL) {
  tryCatch({
    # Check if we're in a reactive context
    in_reactive_context <- tryCatch({
      shiny:::getCurrentReactiveContext()
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (in_reactive_context && !is.null(processing_log)) {
      current_log <- processing_log()
      new_entry <- list(
        timestamp = Sys.time(),
        step = step,
        status = status,
        message = message,
        duration = duration
      )
      processing_log(c(current_log, list(new_entry)))

      # Enhanced debug logging based on status
      if (status == "error" && !is.null(processing_errors)) {
        if (!is.null(debug_log)) debug_log(paste("ERROR in", step, ":", message), 1)
        current_errors <- processing_errors()
        processing_errors(c(current_errors, list(new_entry)))
      } else if (status == "warning" && !is.null(processing_warnings)) {
        if (!is.null(debug_log)) debug_log(paste("WARNING in", step, ":", message), 1)
        current_warnings <- processing_warnings()
        processing_warnings(c(current_warnings, list(new_entry)))
      } else if (status == "success") {
        if (!is.null(debug_log)) debug_log(paste("SUCCESS in", step, "- duration:", sprintf("%.2fs", duration)), 2)
      } else {
        if (!is.null(debug_log)) debug_log(paste("INFO", step, ":", message), 2)
      }
    } else {
      # Just log to debug when not in reactive context
      if (!is.null(debug_log)) {
        if (status == "error") {
          debug_log(paste("ERROR in", step, ":", message), 1)
        } else if (status == "warning") {
          debug_log(paste("WARNING in", step, ":", message), 1)
        } else if (status == "success") {
          debug_log(paste("SUCCESS in", step, "- duration:", sprintf("%.2fs", duration)), 2)
        } else {
          debug_log(paste("INFO", step, ":", message), 2)
        }
      }
    }

  }, error = function(e) {
    if (!is.null(debug_log)) debug_log(paste("Error adding to processing log:", e$message), 1)
  })
}

#' Clear processing errors
clear_processing_errors <- function(processing_errors = NULL, processing_warnings = NULL, debug_log = NULL) {
  tryCatch({
    if (!is.null(processing_errors)) processing_errors(list())
    if (!is.null(processing_warnings)) processing_warnings(list())
    if (!is.null(debug_log)) debug_log("Processing errors and warnings cleared", 2)
  }, error = function(e) {
    if (!is.null(debug_log)) debug_log(paste("Error clearing processing errors:", e$message), 1)
  })
}

############
# Input Validation Functions

#' Comprehensive validation of processing inputs
validate_processing_inputs <- function(rule_files = NULL, metadata_current = NULL, condition_inputs = NULL, debug_log = NULL) {
  tryCatch({
    errors <- character()
    warnings <- character()

    # Check rule files availability
    if (!is.null(rule_files)) {
      tryCatch({
        available_files <- if (is.reactive(rule_files)) rule_files() else rule_files
        if (is.null(available_files) || length(available_files) == 0) {
          warnings <- c(warnings, "No rule files available")
        } else {
          if (!is.null(debug_log)) debug_log(paste("Rule files available:", length(available_files)), 2)
        }
      }, error = function(e) {
        errors <- c(errors, paste("Cannot access rule files:", e$message))
      })
    }

    # Check metadata availability
    if (!is.null(metadata_current)) {
      tryCatch({
        current_metadata <- if (is.reactive(metadata_current)) metadata_current() else metadata_current
        if (is.null(current_metadata)) {
          warnings <- c(warnings, "Metadata is NULL")
        } else if (!is.data.frame(current_metadata)) {
          warnings <- c(warnings, "Metadata is not a data frame")
        } else if (nrow(current_metadata) == 0) {
          warnings <- c(warnings, "Metadata is empty")
        } else {
          if (!is.null(debug_log)) debug_log(paste("Metadata available:", nrow(current_metadata), "rows"), 2)
        }
      }, error = function(e) {
        errors <- c(errors, paste("Cannot access metadata:", e$message))
      })
    }

    # Validate condition inputs
    if (!is.null(condition_inputs)) {
      current_conditions <- if (is.reactive(condition_inputs)) condition_inputs() else condition_inputs
      if (length(current_conditions) == 0) {
        warnings <- c(warnings, "No condition groups defined")
      } else {
        empty_conditions <- sapply(current_conditions, function(x) is.null(x) || x == "")
        if (any(empty_conditions)) {
          warnings <- c(warnings, paste("Empty condition groups:", sum(empty_conditions)))
        }
      }
    }

    if (!is.null(debug_log)) {
      debug_log(paste("Input validation - Errors:", length(errors),
                      "Warnings:", length(warnings)), 2)
    }

    return(list(
      valid = length(errors) == 0,
      errors = errors,
      warnings = warnings
    ))

  }, error = function(e) {
    error_msg <- paste("Error during input validation:", e$message)
    if (!is.null(debug_log)) debug_log(error_msg, 1)
    return(list(
      valid = FALSE,
      errors = error_msg,
      warnings = character()
    ))
  })
}
