# Auto-Assign helper family. Loaded by datawizard_auto_assign_utils.R.

#' Central metadata content availability checker for all modules
#' @param metadata_df resolved metadata data frame
#' @param debug_level debug level for logging
#' @return logical indicating if metadata content is properly assigned
check_metadata_content_available_central <- function(metadata_df, debug_level = 0) {
  tryCatch({
    if (is.null(metadata_df)) {
      return(FALSE)
    }

    # This is deliberately a pure helper. Reactive inputs must be resolved by
    # the observer/reactive expression that owns their dependency context.
    if (!is.data.frame(metadata_df)) {
      return(FALSE)
    }

    if (nrow(metadata_df) == 0) {
      return(FALSE)
    }

    if (!all(c("Content", "Column") %in% names(metadata_df))) {
      return(FALSE)
    }

    # Check if Content column is actually filled with meaningful data
    content_values <- metadata_df$Content

    # Remove NA and empty strings
    valid_content <- content_values[!is.na(content_values) & nzchar(trimws(content_values))]

    # Check if we have any meaningful content assignments
    if (length(valid_content) == 0) {
      debug_auto_assign("Content column exists but contains no meaningful assignments", 2, debug_level)
      return(FALSE)
    }

    # Check for specific content types that indicate proper assignment
    meaningful_content_types <- c(
      "Protein Confidence", "Raw Abundance", "Normalized Abundance",
      "Batch Corrected Abundance", "Batch Corrected Normalized Abundance",
      "Batch Corrected Raw Abundance", "Imputed Raw Abundance",
      "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance", "Imputed Batch Corrected Abundance",
      "Abundance Ratio", "Description", "Identifier", "Basemean"
    )

    has_meaningful_content <- any(sapply(meaningful_content_types, function(content_type) {
      any(grepl(content_type, valid_content, ignore.case = TRUE))
    }))

    if (!has_meaningful_content) {
      debug_auto_assign("Content column filled but contains no recognized content types", 2, debug_level)
      return(FALSE)
    }

    debug_auto_assign(paste("Metadata content available:", length(valid_content), "assigned,",
                            nrow(current_metadata), "total rows"), 2, debug_level)
    return(TRUE)

  }, error = function(e) {
    debug_auto_assign(paste("Error checking metadata content availability:", e$message), 1, debug_level)
    return(FALSE)
  })
}

#' Escape regex metacharacters for safe pattern matching
#' @param x character string to escape
#' @return escaped string suitable for regex use
escape_regex_autoassign_dw <- function(x) {
  if (is.na(x) || x == "") return(x)

  if (grepl("^ +$", x)) {
    return("\\s+")
  }

  # Replace multiple spaces with \s+
  x <- gsub(" +", "\\\\s+", x, perl = TRUE)
  # Escape meta-characters except '\' and '+'
  x <- stringr::str_replace_all(
    x,
    "([\\.\\*\\?\\[\\]\\^\\$\\(\\)\\{\\}\\=\\!\\<\\>\\|\\:-])",
    "\\\\\\1"
  )
  # Escape literal '+' but not the '+' in '\s+'
  x <- stringr::str_replace_all(x, "(?<!\\\\s)\\+", "\\\\+")
  return(x)
}

#' Create AND regex pattern from multiple terms
#' @param pat pattern string with & separators
#' @return lookahead regex pattern
make_and_regex_autoassign_dw <- function(pat) {
  if (is.na(pat) || pat == "") return(pat)

  parts <- strsplit(pat, "\\s*&\\s*")[[1]]
  parts <- vapply(parts, escape_regex_autoassign_dw, character(1))
  return(paste0(lapply(parts, function(p) sprintf("(?=.*%s)", p)), collapse = ""))
}

#' Create OR regex pattern from multiple terms
#' @param pat pattern string with | separators
#' @return alternation regex pattern
make_or_regex_autoassign_dw <- function(pat) {
  if (is.na(pat) || pat == "") return(pat)

  parts <- strsplit(pat, "\\s*\\|\\s*")[[1]]
  parts_esc <- vapply(parts, escape_regex_autoassign_dw, character(1))
  return(paste(parts_esc, collapse = "|"))
}

#' Convert regex pattern back to plain text for UI display
#' @param pat regex pattern string
#' @return plain text version
regex_to_plain_dw <- function(pat) {
  if (is.na(pat) || pat == "") return(pat)

  if (grepl("^\\\\s\\+$", pat)) {
    return(" ")
  }

  # Extract AND lookarounds
  la <- stringr::str_match_all(pat, "\\(\\?=\\.\\*(.*?)\\)")[[1]][,2]
  if (length(la) > 1) {
    parts <- la
    parts <- gsub("\\\\s\\+", " ", parts, perl = TRUE)
    parts <- gsub("\\\\(.)", "\\1", parts, perl = TRUE)
    return(paste(parts, collapse = "&"))
  }

  # Handle OR patterns
  or_parts <- strsplit(pat, "(?<!\\\\)\\|", perl = TRUE)[[1]]
  if (length(or_parts) > 1) {
    parts <- or_parts
    parts <- gsub("\\\\s\\+", " ", parts, perl = TRUE)
    parts <- gsub("\\\\(.)", "\\1", parts, perl = TRUE)
    return(paste(parts, collapse = "|"))
  }

  # Fallback: simple unescape
  plain <- pat
  plain <- gsub("\\\\s\\+", " ", plain, perl = TRUE)
  plain <- gsub("\\\\(.)", "\\1", plain, perl = TRUE)
  return(plain)
}

#' Enhanced boolean check function with robust validation
#' @param x value to check
#' @return logical TRUE/FALSE
safe_is_true <- function(x) {
  # First level: NULL and length checks
  if (is.null(x)) {
    return(FALSE)
  }

  if (length(x) == 0) {
    return(FALSE)
  }

  # Safe handling of different data types
  tryCatch({
    if (is.logical(x)) {
      # Handle logical vectors with potential NA values
      if (any(is.na(x))) {
        return(FALSE)
      }
      result <- isTRUE(x[1])
      return(result)
    }

    if (is.numeric(x)) {
      # Handle numeric vectors with potential NA/NaN values
      if (any(is.na(x)) || any(is.nan(x))) {
        return(FALSE)
      }
      result <- x[1] > 0
      return(result)
    }

    if (is.character(x)) {
      # Handle character vectors with potential empty strings
      if (any(is.na(x))) {
        return(FALSE)
      }

      char_val <- as.character(x[1])
      if (length(char_val) == 0 || !nzchar(char_val)) {
        return(FALSE)
      }

      result <- tolower(char_val) %in% c("true", "t", "yes", "y", "1")
      return(result)
    }

    if (is.list(x)) {
      # Check for success indicators
      if (!is.null(x$success)) {
        result <- safe_is_true(x$success)
        return(result)
      }

      if (!is.null(x$data)) {
        return(TRUE)
      }

      result <- length(x) > 0
      return(result)
    }

    # Fallback for unknown types
    return(FALSE)

  }, error = function(e) {
    return(FALSE)
  })
}

#' Identify the correct update function for UI elements
#' @param input_id shiny input ID
#' @return function name or "unknown"
identify_update_function <- function(input_id) {
  # Common patterns for different input types
  if (grepl("_select$|_dw$|_sel$", input_id)) {
    return("updateSelectInput")
  } else if (grepl("text|string|pattern", input_id, ignore.case = TRUE)) {
    return("updateTextInput")
  } else if (grepl("numeric|value|count|min|max", input_id, ignore.case = TRUE)) {
    return("updateNumericInput")
  } else if (grepl("check|enable|toggle", input_id, ignore.case = TRUE)) {
    return("updateCheckboxInput")
  } else {
    return("updateSelectInput")  # Default fallback
  }
}

#' Safe input update function with enhanced validation
#' @param session Shiny session object
#' @param input_id input element ID
#' @param value new value to set
#' @param update_function update function to use
#' @return logical indicating success
safe_update_input <- function(session, input_id, value, update_function) {
  tryCatch({
    # Basic validation
    if (is.null(value)) {
      return(FALSE)
    }

    if (length(value) == 0) {
      return(FALSE)
    }

    # Validate session object
    if (is.null(session)) {
      return(FALSE)
    }

    # Validate update function
    if (!is.function(update_function)) {
      return(FALSE)
    }

    # Handle updateSelectInput specifically
    if (identical(update_function, updateSelectInput)) {
      # For selectInput: Value must be non-empty string and use 'selected' parameter
      if (is.na(value[1]) || !nzchar(as.character(value[1]))) {
        return(FALSE)
      }

      value_to_use <- as.character(value[1])
      updateSelectInput(session, input_id, selected = value_to_use)

    } else if (identical(update_function, updateTextInput)) {
      # For textInput: Empty strings are allowed and use 'value' parameter
      value_to_use <- if (is.na(value[1])) "" else as.character(value[1])
      updateTextInput(session, input_id, value = value_to_use)

    } else if (identical(update_function, updateNumericInput)) {
      # For numericInput: Handle numeric conversion and use 'value' parameter
      value_to_use <- if (is.na(value[1])) NULL else as.numeric(value[1])
      updateNumericInput(session, input_id, value = value_to_use)

    } else if (identical(update_function, updateCheckboxInput)) {
      # For checkboxInput: Handle logical conversion and use 'value' parameter
      value_to_use <- if (is.na(value[1])) FALSE else as.logical(value[1])
      updateCheckboxInput(session, input_id, value = value_to_use)

    } else {
      # Generic fallback - try to determine from input_id pattern
      if (grepl("_select$|_dw$|_sel$", input_id)) {
        # Likely a selectInput
        if (is.na(value[1]) || !nzchar(as.character(value[1]))) {
          return(FALSE)
        }
        value_to_use <- as.character(value[1])
        update_function(session, input_id, selected = value_to_use)
      } else {
        # Try with 'value' parameter
        update_function(session, input_id, value = value)
      }
    }

    return(TRUE)

  }, error = function(e) {
    return(FALSE)
  })
}

############
# Helper Functions for Condition Extraction
############

#' Extract sample from column rule - helper function for condition extraction
#' @param metadata_df metadata data frame
#' @param rule extraction rule
#' @return character vector of extracted samples
extract_sample_from_column_rule <- function(metadata_df, rule) {
  extracted_samples <- character()

  tryCatch({
    lookup_col <- rule$Content
    method <- rule$Method
    before_pattern <- rule$Before
    after_pattern <- rule$After
    separators <- rule$Separators
    position <- if (!is.na(rule$Pos)) rule$Pos else 1

    content_rows <- which(metadata_df$Content == lookup_col)
    if (length(content_rows) == 0) {
      return(character())
    }

    column_names <- metadata_df$Column[content_rows]

    for (col_name in column_names) {
      extracted_sample <- extract_sample_from_column_name(
        col_name, method, before_pattern, after_pattern, separators, position
      )
      if (!is.na(extracted_sample) && nzchar(extracted_sample)) {
        extracted_samples <- c(extracted_samples, extracted_sample)
      }
    }

    return(unique(extracted_samples))

  }, error = function(e) {
    return(character())
  })
}

#' Extract sample from column name - core extraction logic
#' @param column_name column name to process
#' @param method extraction method
#' @param before_pattern pattern before target
#' @param after_pattern pattern after target
#' @param separators separator characters
#' @param position position in split string
#' @return extracted sample name
extract_sample_from_column_name <- function(column_name, method, before_pattern, after_pattern, separators, position) {
  tryCatch({
    switch(method,
           "between" = {
             if (!is.na(before_pattern) && !is.na(after_pattern)) {
               pattern <- paste0("(?<=", before_pattern, ").+?(?=", after_pattern, ")")
               match <- stringr::str_extract(column_name, pattern)
               return(if (!is.na(match)) match else "")
             }
             return("")
           },
           "start" = {
             if (!is.na(after_pattern)) {
               parts <- stringr::str_split(column_name, after_pattern)[[1]]
               return(if (length(parts) > 0) parts[1] else "")
             }
             return(column_name)
           },
           "end" = {
             if (!is.na(before_pattern)) {
               parts <- stringr::str_split(column_name, before_pattern)[[1]]
               return(if (length(parts) > 1) parts[length(parts)] else "")
             }
             return(column_name)
           },
           "phrase_position" = {
             if (!is.na(separators)) {
               parts <- stringr::str_split(column_name, separators)[[1]]
               return(if (length(parts) >= position) parts[position] else "")
             }
             return("")
           },
           "pattern_detect" = {
             if (!is.na(separators)) {
               parts <- stringr::str_split(column_name, separators)[[1]]
               return(if (length(parts) > 0) parts[length(parts)] else "")
             }
             return("")
           },
           "whole" = {
             return(column_name)
           },
           ""
    )
  }, error = function(e) {
    return("")
  })
}

############
# Enhanced Module Validation and State Collection

#' Validate module reference with enhanced checks
#' @param module_ref reactive or list reference to module
#' @param module_name character name for logging
#' @param debug_level debug level
#' @return logical indicating if module is valid
validate_module_reference <- function(module_ref, module_name, debug_level = 0) {
  if (is.null(module_ref)) {
    debug_auto_assign(paste("Module reference is NULL for:", module_name), level = 2, debug_level)
    return(FALSE)
  }

  tryCatch({
    if (is.reactive(module_ref)) {
      if (!shiny::is.reactive(module_ref)) {
        debug_auto_assign(paste("Invalid reactive object for:", module_name), level = 1, debug_level)
        return(FALSE)
      }
      mod_obj <- module_ref()
    } else {
      mod_obj <- module_ref
    }

    if (is.null(mod_obj)) {
      debug_auto_assign(paste("Module object is NULL for:", module_name), level = 2, debug_level)
      return(FALSE)
    }

    if (!is.list(mod_obj)) {
      debug_auto_assign(paste("Module object is not a list for:", module_name), level = 1, debug_level)
      return(FALSE)
    }

    return(TRUE)

  }, error = function(e) {
    debug_auto_assign(paste("Error validating module reference for", module_name, ":", e$message), level = 1, debug_level)
    return(FALSE)
  })
}

#' Safely get module reference with proper error handling
#' @param module_ref reactive or list reference
#' @param debug_level debug level
#' @return module object or NULL
get_module_safely <- function(module_ref, debug_level = 0) {
  if (is.null(module_ref)) return(NULL)

  tryCatch({
    if (is.reactive(module_ref)) {
      return(module_ref())
    } else {
      return(module_ref)
    }
  }, error = function(e) {
    debug_auto_assign(paste("Error getting module reference:", e$message), level = 1, debug_level)
    return(NULL)
  })
}

#' Collect UI state from module with fallback options
#' @param module_ref reference to the module
#' @param config_name name for logging
#' @param get_state_function primary function name to call
#' @param fallback_functions vector of fallback function names
#' @param debug_level debug level
#' @return collected UI state or NULL
collect_module_ui_state <- function(module_ref, config_name,
                                    get_state_function = "get_current_ui_state",
                                    fallback_functions = NULL,
                                    debug_level = 0) {
  if (!validate_module_reference(module_ref, config_name, debug_level)) {
    return(NULL)
  }

  mod_ref <- get_module_safely(module_ref, debug_level)
  if (is.null(mod_ref)) return(NULL)

  # Try primary function
  if (!is.null(mod_ref[[get_state_function]]) && is.function(mod_ref[[get_state_function]])) {
    tryCatch({
      ui_state <- mod_ref[[get_state_function]]()
      if (!is.null(ui_state)) {
        debug_auto_assign(paste("State collected for", config_name), level = 2, debug_level)
        return(ui_state)
      }
    }, error = function(e) {
      debug_auto_assign(paste("Primary function failed for", config_name, ":", e$message), level = 2, debug_level)
    })
  }

  # Try fallback functions
  if (!is.null(fallback_functions)) {
    for (fallback_func in fallback_functions) {
      if (!is.null(mod_ref[[fallback_func]]) && is.function(mod_ref[[fallback_func]])) {
        tryCatch({
          ui_state <- mod_ref[[fallback_func]]()
          if (!is.null(ui_state)) {
            debug_auto_assign(paste("Fallback state collected for", config_name), level = 2, debug_level)
            return(ui_state)
          }
        }, error = function(e) {
          debug_auto_assign(paste("Fallback function failed for", config_name, ":", e$message), level = 2, debug_level)
        })
      }
    }
  }

  debug_auto_assign(paste("No working state collection function found for", config_name), level = 2, debug_level)
  return(NULL)
}

# -------------------------------------------------------------------
# Collect Basemean UI Configuration
# -------------------------------------------------------------------
collect_basemean_configurations <- function(basemean_module, debug_fn = NULL, module_call_fn = NULL) {
  log_fn <- if (is.function(debug_fn)) {
    debug_fn
  } else {
    function(message, level = 1) debug_auto_assign(message, level = level)
  }

  safe_call <- if (is.function(module_call_fn)) {
    module_call_fn
  } else if (exists("safe_module_call", mode = "function")) {
    get("safe_module_call", mode = "function")
  } else {
    function(fn, default_return = NULL, context = "") {
      tryCatch(fn(), error = function(e) default_return)
    }
  }

  tryCatch({
    if (is.null(basemean_module)) {
      log_fn("collect_basemean_configurations: No Basemean module found", 2)
      return(NULL)
    }

    if (is.null(basemean_module$get_ui_config) || !is.function(basemean_module$get_ui_config)) {
      log_fn("collect_basemean_configurations: Basemean module has no get_ui_config()", 1)
      return(NULL)
    }

    config <- safe_call(
      basemean_module$get_ui_config,
      default_return = NULL,
      context = "basemean_ui_export"
    )

    if (!is.null(config) && length(config) > 0) {
      log_fn("Collected Basemean configuration successfully", 2)
      return(config)
    }

    log_fn("No Basemean UI configuration available for export", 2)
    return(NULL)
  }, error = function(e) {
    log_fn(paste("Error collecting Basemean UI configuration:", e$message), 1)
    return(NULL)
  })
}

# ===============================================================
# Extract Basemean UI Configuration (from RDS)
# ===============================================================
extract_ui_basemean_config <- function(rule_data, debug_log, ui_config_errors) {
  tryCatch({
    if (!is.null(rule_data$basemean_configurations)) {
      config <- rule_data$basemean_configurations
      debug_log("Basemean UI configuration extracted successfully", 2)
      return(config)
    } else {
      debug_log("No Basemean configuration found in imported data", 2)
      return(NULL)
    }
  }, error = function(e) {
    debug_log(paste("Error extracting Basemean configuration:", e$message), 1)
    ui_config_errors$add(paste("Basemean extraction failed:", e$message))
    return(NULL)
  })
}

set_basemean_ui_config <- function(config, basemean_module_ref = NULL, debug_fn = NULL, module_call_fn = NULL) {
  log_fn <- if (is.function(debug_fn)) {
    debug_fn
  } else {
    function(message, level = 1) debug_auto_assign(message, level = level)
  }

  safe_call <- if (is.function(module_call_fn)) {
    module_call_fn
  } else if (exists("safe_module_call", mode = "function")) {
    get("safe_module_call", mode = "function")
  } else {
    function(fn, default_return = NULL, context = "") {
      tryCatch(fn(), error = function(e) default_return)
    }
  }

  tryCatch({
    if (is.null(config) || length(config) == 0) {
      log_fn("Basemean UI config is empty — nothing to apply", 1)
      return(FALSE)
    }

    basemean_target <- basemean_module_ref

    # Backward-compatible fallback for legacy callers that still rely on global module registry.
    if (is.null(basemean_target) && exists("modules", inherits = TRUE)) {
      candidate_modules <- get("modules", inherits = TRUE)
      if (is.list(candidate_modules) && !is.null(candidate_modules$basemean_out)) {
        basemean_target <- candidate_modules$basemean_out
      }
    }

    if (!is.null(basemean_target) && is.function(basemean_target$apply_ui_config)) {
      safe_call(
        function() basemean_target$apply_ui_config(config),
        default_return = NULL,
        context = "set_basemean_ui_config"
      )
      log_fn("Basemean UI configuration applied successfully", 1)
      return(TRUE)
    }

    log_fn("Basemean module not available for UI config application", 1)
    return(FALSE)
  }, error = function(e) {
    log_fn(paste("Error applying Basemean UI config:", e$message), 1)
    return(FALSE)
  })
}

############
# Data Structure Creation

#' Create empty data structures for templates
#' @param type type of empty structure to create
#' @return empty data.frame with correct structure
create_empty_structure <- function(type) {
  switch(type,
         "operations" = data.frame(
           Operation = character(),
           Type = character(),
           Columns = character(),
           Parameters = character(),
           Description = character(),
           Executed = logical(),
           stringsAsFactors = FALSE
         ),
         "ratio_configurations" = data.frame(
           Title = character(),
           Content = character(),
           Numerator = I(list()),
           Denominator = I(list()),
           Statistics = character(),
           "Adjustment Method" = character(),
           "Valid Count" = numeric(),
           "Valid Logic" = character(),
           stringsAsFactors = FALSE,
           check.names = FALSE
         ),
         "filter_state" = list(
           confidence = list(
             numeric_enabled = FALSE,
             string_enabled = FALSE,
             numeric_max = NULL,
             numeric_min = NULL,
             string_input = ""
           ),
           valid_values = list(
             group_selection = "In total",
             min_count = 1
           ),
           custom = data.frame(
             Column = character(),
             Operator_1 = character(),
             Value_1 = character(),
             Logic = character(),
             Operator_2 = character(),
             Value_2 = character(),
             Empty_Filter = character(),
             Multi_Column_Logic = character(),
             stringsAsFactors = FALSE
           ),
           collection_status = "empty"
         ),
         NULL
  )
}

#' Enhanced processing log function with error handling
#' @param step processing step name
#' @param status status level ("success", "error", "warning", "info")
#' @param message log message
#' @param duration processing duration in seconds
add_processing_log <- function(step, status, message = "", duration = 0) {
  tryCatch({
    # Validate status parameter
    valid_statuses <- c("success", "error", "warning", "info")
    if (!status %in% valid_statuses) {
      status <- "info"  # Default fallback
    }

    new_entry <- list(
      timestamp = Sys.time(),
      step = step,
      status = status,
      message = message,
      duration = duration
    )

    # Enhanced debug logging based on status
    if (status == "error") {
      debug_auto_assign(paste("ERROR in", step, ":", message), 1)
    } else if (status == "warning") {
      debug_auto_assign(paste("WARNING in", step, ":", message), 1)
    } else if (status == "success") {
      debug_auto_assign(paste("SUCCESS in", step, "- duration:", sprintf("%.2fs", duration)), 2)
    } else {
      debug_auto_assign(paste("INFO", step, ":", message), 2)
    }

  }, error = function(e) {
    debug_auto_assign(paste("Error adding to processing log:", e$message), 1)
  })
}
