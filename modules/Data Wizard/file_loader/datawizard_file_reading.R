# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_reading.R
# Purpose:
#   Provide the file reading portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# Data Wizard file-reading utilities.

# Supported Data Wizard upload extensions.
datawizard_table_extensions_dw <- c("csv", "tsv", "txt", "xls", "xlsx")
datawizard_session_extensions_dw <- c("rds")
datawizard_unsupported_upload_message_dw <- paste(
  "Unsupported file type. Supported formats are .csv, .tsv, .txt, .xls, .xlsx, and .rds."
)

#' Check whether an uploaded file uses a supported Data Wizard extension.
#'
#' @param file_input Shiny file input object.
#' @return Logical indicating whether the file extension is supported.
#' @export
is_supported_datawizard_upload_dw <- function(file_input) {
  ext <- tolower(tools::file_ext(file_input$name %||% file_input$datapath %||% ""))
  ext %in% c(datawizard_table_extensions_dw, datawizard_session_extensions_dw)
}

#' Enhanced delimiter detection using multiple lines for better accuracy.
#'
#' Checks multiple lines for the most consistent delimiter occurrence of:
#'   • Tab ("\t")
#'   • Comma (",")
#'   • Semicolon (";")
#'   • Pipe ("|")
#'
#' @param file_path Path to the file to inspect.
#' @param max_lines Maximum number of lines to check (default: 5).
#' @return One of "\t", ",", ";", "|" if found; otherwise an empty string.
#' @export
check_csv_separator_dw <- function(file_path, max_lines = 5) {
  # Enhanced input validation
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("File path is NULL or file does not exist")
  }

  # Read multiple lines for better detection
  lines <- tryCatch({
    readLines(file_path, n = max_lines, warn = FALSE)
  }, error = function(e) {
    stop("Error reading file lines: ", e$message)
  })

  if (length(lines) == 0) {
    warning("File appears to be empty")
    return("")
  }

  # Remove empty lines
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    warning("File contains no non-empty lines")
    return("")
  }

  # Candidate separators to test
  candidates <- c("\t", ",", ";", "|")

  # Calculate consistency score for each separator
  scores <- tryCatch({
    sapply(candidates, function(sep) {
      splits <- strsplit(lines, sep, fixed = TRUE)
      lengths <- sapply(splits, length)

      # Consistent column count across lines = better score
      if (length(unique(lengths)) == 1 && mean(lengths) > 1) {
        return(mean(lengths))  # Higher score for more columns
      }
      return(0)
    })
  }, error = function(e) {
    warning("Error analyzing separators, falling back to comma: ", e$message)
    return(c(0, 1, 0, 0))  # Default to comma
  })

  # Pick the separator with highest consistency score
  best <- which.max(scores)
  if (scores[best] > 0) {
    return(candidates[best])
  }

  warning("Unable to identify CSV delimiter; returning empty string.")
  return("")
}

#' Validate file size and provide appropriate warnings/errors.
#'
#' @param file_path Path to the file to check.
#' @param max_size_mb Maximum allowed size in MB (default: 500).
#' @param warning_size_mb Size threshold for warnings in MB (default: 100).
#' @return List with status, size_mb, and message.
#' @export
validate_file_size_dw <- function(file_path, max_size_mb = 500, warning_size_mb = 100) {
  if (is.null(file_path) || !file.exists(file_path)) {
    return(list(status = "error", size_mb = 0, message = "File does not exist"))
  }

  file_info <- tryCatch({
    file.info(file_path)
  }, error = function(e) {
    return(list(status = "error", size_mb = 0, message = paste("Error reading file info:", e$message)))
  })

  if (is.na(file_info$size) || file_info$size == 0) {
    return(list(status = "error", size_mb = 0, message = "File appears to be empty"))
  }

  size_mb <- file_info$size / (1024^2)

  if (size_mb > max_size_mb) {
    return(list(
      status = "error",
      size_mb = size_mb,
      message = sprintf("File too large (%.1f MB > %.0f MB limit)", size_mb, max_size_mb)
    ))
  }

  if (size_mb > warning_size_mb) {
    return(list(
      status = "warning",
      size_mb = size_mb,
      message = sprintf("Large file (%.1f MB) - processing may take time", size_mb)
    ))
  }

  return(list(
    status = "ok",
    size_mb = size_mb,
    message = sprintf("File size: %.1f MB", size_mb)
  ))
}

#' Detect and handle file encoding for robust text reading.
#'
#' @param file_path Path to the file to process.
#' @return Path to file (original or converted temp file).
#' @export
detect_and_handle_encoding_dw <- function(file_path) {
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("File path is NULL or file does not exist")
  }

  # Try to detect encoding
  encoding_result <- tryCatch({
    readr::guess_encoding(file_path, n_max = 1000)
  }, error = function(e) {
    # If detection fails, assume UTF-8
    return(data.frame(encoding = "UTF-8", confidence = 1, stringsAsFactors = FALSE))
  })

  if (nrow(encoding_result) == 0) {
    # No encoding detected, assume UTF-8
    return(file_path)
  }

  detected_encoding <- encoding_result$encoding[1]
  confidence <- encoding_result$confidence[1]

  # If already UTF-8 or high confidence UTF-8, return original
  if (detected_encoding == "UTF-8" || (detected_encoding == "ASCII" && confidence > 0.8)) {
    return(file_path)
  }

  # Convert to UTF-8 if different encoding detected with reasonable confidence
  if (confidence > 0.5) {
    tryCatch({
      content <- readLines(file_path, encoding = detected_encoding, warn = FALSE)
      temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
      writeLines(content, temp_file, useBytes = FALSE)
      return(temp_file)
    }, error = function(e) {
      # If conversion fails, return original file
      warning("Encoding conversion failed, using original file: ", e$message)
      return(file_path)
    })
  }

  # Return original file if confidence is low
  return(file_path)
}

#' Perform garbage collection for large data objects to manage memory.
#'
#' @param data_object The data object to check.
#' @param threshold_mb Memory threshold in MB for triggering GC (default: 50).
#' @return The original data object (unchanged).
#' @export
manage_memory_after_loading_dw <- function(data_object, threshold_mb = 50) {
  if (is.null(data_object)) {
    return(data_object)
  }

  tryCatch({
    obj_size_mb <- as.numeric(object.size(data_object)) / (1024^2)

    if (obj_size_mb > threshold_mb) {
      # Force garbage collection for large objects
      gc(verbose = FALSE)
    }
  }, error = function(e) {
    # Silently continue if memory check fails
  })

  return(data_object)
}

#' Read delimited files with streaming support for large files.
#'
#' @param file_path Path to the text file.
#' @param sep Field delimiter.
#' @param header Logical. Use first line as column names.
#' @param stringsAsFactors Logical. Convert character columns to factors.
#' @param stream_threshold_mb Size threshold for streaming in MB (default: 50).
#' @return A data.frame.
#' @export
robust_read_table <- function(file_path,
                              sep              = NULL,
                              header           = TRUE,
                              stringsAsFactors = FALSE,
                              stream_threshold_mb = 50) {

  # Enhanced input validation
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("File path is NULL or file does not exist")
  }

  # Check file size for streaming decision
  size_check <- validate_file_size_dw(file_path)
  use_streaming <- size_check$size_mb > stream_threshold_mb

  # Handle encoding
  processed_file_path <- detect_and_handle_encoding_dw(file_path)

  # Auto-detect separator if not provided
  if (is.null(sep)) {
    sep <- tryCatch({
      check_csv_separator_dw(processed_file_path)
    }, error = function(e) {
      stop("Could not determine delimiter automatically: ", e$message)
    })

    if (sep == "") {
      stop("Could not determine delimiter automatically.")
    }
  }

  # Choose reading method based on file size
  if (use_streaming && requireNamespace("data.table", quietly = TRUE)) {
    # Use data.table for large files (streaming approach)
    df <- tryCatch({
      dt_result <- data.table::fread(
        processed_file_path,
        sep = sep,
        header = header,
        stringsAsFactors = stringsAsFactors,
        showProgress = FALSE,
        data.table = FALSE
      )
      dt_result
    }, error = function(e) {
      # Fallback to standard method if data.table fails
      warning("Streaming read failed, using standard method: ", e$message)
      NULL
    })

    if (!is.null(df)) {
      return(manage_memory_after_loading_dw(df))
    }
  }

  # Standard reading method (fallback or small files)
  lines <- tryCatch({
    readLines(processed_file_path, warn = FALSE)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })

  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0) {
    warning("File contains no non-empty lines. Returning an empty data.frame.")
    return(data.frame())
  }

  # Split lines and create matrix
  parts_list <- tryCatch({
    strsplit(lines, sep, fixed = TRUE)
  }, error = function(e) {
    stop("Error splitting lines with delimiter '", sep, "': ", e$message)
  })

  max_fields <- max(lengths(parts_list))

  if (header) {
    header_fields <- parts_list[[1]]
    data_parts    <- parts_list[-1]
  } else {
    header_fields <- NULL
    data_parts    <- parts_list
  }

  # Build matrix
  mat <- tryCatch({
    t(vapply(data_parts, function(x) {
      length(x) <- max_fields
      x
    }, character(max_fields)))
  }, error = function(e) {
    stop("Error creating data matrix: ", e$message)
  })

  # Convert to data.frame
  df <- tryCatch({
    as.data.frame(mat,
                  stringsAsFactors = stringsAsFactors,
                  check.names       = FALSE)
  }, error = function(e) {
    stop("Error converting to data.frame: ", e$message)
  })

  # Assign column names
  if (header) {
    n_head <- length(header_fields)
    if (n_head < max_fields) {
      extra <- paste0("_", seq(n_head + 1, max_fields))
      header_fields <- c(header_fields, extra)
    }
    colnames(df) <- header_fields[seq_len(max_fields)]
  } else {
    colnames(df) <- paste0("V", seq_len(max_fields))
  }

  # Clean up temp file if created
  if (processed_file_path != file_path && file.exists(processed_file_path)) {
    tryCatch({
      file.remove(processed_file_path)
    }, error = function(e) {
      # Silently continue if temp file cleanup fails
    })
  }

  return(manage_memory_after_loading_dw(df))
}

#' Enhanced file loading with recovery mechanisms.
#'
#' @param file Shiny file object.
#' @param sheet Optional sheet name for Excel files.
#' @param header Logical. Use first line as column names.
#' @param recovery_attempts Number of recovery attempts (default: 3).
#' @return List with data and type.
#' @export
load_file_with_recovery_dw <- function(file, sheet = NULL, header = TRUE, recovery_attempts = 3) {
  # Enhanced input validation
  if (is.null(file)) {
    stop("File object is NULL")
  }

  if (is.null(file$name) || is.null(file$datapath)) {
    stop("Invalid file object: missing name or datapath")
  }

  if (!file.exists(file$datapath)) {
    stop("File does not exist at specified path")
  }

  # Validate file size
  size_check <- validate_file_size_dw(file$datapath)
  if (size_check$status == "error") {
    stop(size_check$message)
  }

  ext <- tolower(tools::file_ext(file$name))

  # Define recovery strategies
  recovery_strategies <- list()

  if (ext %in% c("csv", "tsv", "txt")) {
    recovery_strategies <- list(
      # Primary strategy: robust_read_table
      function() robust_read_table(file$datapath, sep = NULL, header = header, stringsAsFactors = FALSE),

      # Fallback 1: readr with encoding guess
      function() {
        if (requireNamespace("readr", quietly = TRUE)) {
          df <- readr::read_delim(file$datapath,
                                  delim = check_csv_separator_dw(file$datapath),
                                  locale = readr::locale(encoding = "UTF-8"),
                                  col_names = header,
                                  show_col_types = FALSE)
          return(as.data.frame(df))
        }
        stop("readr package not available")
      },

      # Fallback 2: base R with different encoding
      function() {
        read.csv(file$datapath,
                 header = header,
                 sep = check_csv_separator_dw(file$datapath),
                 fileEncoding = "latin1",
                 stringsAsFactors = FALSE)
      }
    )
  } else if (ext %in% c("xlsx", "xls")) {
    recovery_strategies <- list(
      # Primary strategy: readxl
      function() {
        sheets <- readxl::excel_sheets(file$datapath)
        chosen <- if (!is.null(sheet) && sheet %in% sheets) sheet else sheets[1]
        readxl::read_excel(file$datapath, sheet = chosen, .name_repair = "minimal")
      },

      # Fallback: readxl with different options
      function() {
        sheets <- readxl::excel_sheets(file$datapath)
        chosen <- if (!is.null(sheet) && sheet %in% sheets) sheet else sheets[1]
        readxl::read_excel(file$datapath,
                           sheet = chosen,
                           .name_repair = "minimal",
                           col_names = header,
                           trim_ws = TRUE)
      }
    )
  } else {
    stop("Unsupported file format: .", ext)
  }

  # Attempt loading with recovery
  last_error <- NULL
  for (attempt in seq_len(min(recovery_attempts, length(recovery_strategies)))) {
    result <- tryCatch({
      df <- recovery_strategies[[attempt]]()

      if (is.null(df) || nrow(df) == 0) {
        stop("No data found in file")
      }

      # Clean and index the data
      df_clean <- clean_and_index(df)
      return(list(data = df_clean, type = ext))

    }, error = function(e) {
      last_error <<- e$message
      return(NULL)
    })

    if (!is.null(result)) {
      return(result)
    }
  }

  # All strategies failed
  stop("All recovery attempts failed. Last error: ", last_error)
}
