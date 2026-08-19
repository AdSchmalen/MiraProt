# ==============================================================================
# File: R/export/export_debug_utils.R
#
# Purpose:
#   Export debug/logging and Excel sanitization utilities shared by the
#   comprehensive workbook export pipeline.
#
# Architectural Role:
#   Utility layer for export orchestration and sheet writers.
#
# Structure:
#   1. Debug logger factory (create_excel_debug_system)
#   2. Excel-safe sanitizers (sanitize_for_excel / sanitize_excel_object)
#   3. Sanitized writer wrapper (writeData_sanitized)
#   4. Level-0 log extraction helper (extract_level0_debug_entries)
#
# Return Interface (public API):
#   create_excel_debug_system()
#   sanitize_for_excel()
#   sanitize_excel_object()
#   writeData_sanitized()
#   extract_level0_debug_entries()
#
# Notes for future developers:
#   - Keep these helpers side-effect free except for debug logging and writes.
#   - This file must not contain module-specific extraction logic.
#   - Workbook/sheet business rules belong in orchestration or sheet helpers.
# ==============================================================================

# ========================================
# Enhanced Debug Management for Excel Export
# ========================================

create_excel_debug_system <- function(debug_level = 1) {
  list(
    debug_log = function(message, level = 1, module_name = "EXCEL_EXPORT") {
      if (debug_level >= level) {
        timestamp <- format(Sys.time(), "%H:%M:%S")
        cat("[ ", module_name, " ", timestamp, " ] ", message, "\n")
      }
    },
    DEBUG_LEVEL = debug_level
  )
}





sanitize_for_excel <- function(df, sheet_name, debug_log = NULL, max_chars = 32767L) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) return(df)

  # Normalize text-like columns before length checks:
  # - trim leading/trailing whitespace
  # - flatten list-cells to deterministic character values
  # This prevents accidental text inflation before writeData().
  text_cols <- which(vapply(df, function(col) is.character(col) || is.factor(col) || is.list(col), logical(1)))
  if (length(text_cols) == 0L) return(df)

  truncated_cells <- 0L
  trimmed_cells <- 0L
  for (col_idx in text_cols) {
    col <- df[[col_idx]]

    values <- if (is.list(col)) {
      vapply(col, function(cell) {
        if (length(cell) == 0L || all(is.na(cell))) return(NA_character_)
        paste(as.character(cell), collapse = " | ")
      }, character(1), USE.NAMES = FALSE)
    } else {
      as.character(col)
    }

    before_trim <- values
    values <- trimws(values)
    trimmed_cells <- trimmed_cells + sum(!is.na(before_trim) & !is.na(values) & before_trim != values)

    valid <- !is.na(values)
    if (!any(valid)) next

    too_long <- valid & nchar(values, type = "chars", allowNA = FALSE, keepNA = FALSE) > max_chars
    if (!any(too_long)) next

    values[too_long] <- substr(values[too_long], 1L, max_chars)
    df[[col_idx]] <- values
    truncated_cells <- truncated_cells + sum(too_long)
  }

  if (trimmed_cells > 0L && is.function(debug_log)) {
    debug_log(paste0("Trimmed leading/trailing whitespace in ", trimmed_cells,
                     " text cells in sheet '", sheet_name, "'"), level = 2)
  }

  if (truncated_cells > 0L && is.function(debug_log)) {
    debug_log(paste0("Truncated ", truncated_cells, " text cells in sheet '", sheet_name,
                     "' to Excel's ", max_chars, " character limit"), level = 1)
  }

  df
}

sanitize_excel_object <- function(x, sheet_name = "Sheet", debug_log = NULL, max_chars = 32767L) {
  truncate_chr <- function(v) {
    if (!is.character(v)) return(v)
    v <- trimws(v)
    valid <- !is.na(v)
    too_long <- valid & nchar(v, type = "chars", allowNA = FALSE, keepNA = FALSE) > max_chars
    if (any(too_long)) v[too_long] <- substr(v[too_long], 1L, max_chars)
    v
  }

  if (is.data.frame(x)) return(sanitize_for_excel(x, sheet_name = sheet_name, debug_log = debug_log, max_chars = max_chars))
  if (is.matrix(x) && is.character(x)) return(matrix(truncate_chr(as.vector(x)), nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x)))
  if (is.factor(x)) return(truncate_chr(as.character(x)))
  if (is.character(x)) return(truncate_chr(x))
  x
}

writeData_sanitized <- function(wb, sheet, x, debug_log = NULL, max_chars = 32767L, ...) {
  x2 <- sanitize_excel_object(x, sheet_name = as.character(sheet), debug_log = debug_log, max_chars = max_chars)
  withCallingHandlers(
    openxlsx::writeData(wb, sheet, x2, ...),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("32767|exeed the limit of 32767|exceed the limit of 32767", msg, ignore.case = TRUE)) {
        if (is.function(debug_log)) {
          debug_log(paste0("Suppressed openxlsx 32767-char warning for sheet '", sheet, "' after sanitization."), level = 2)
        }
        invokeRestart("muffleWarning")
      }
    }
  )
}

extract_level0_debug_entries <- function(rv = NULL) {
  dedupe_debug_rows <- function(buf) {
    if (!is.data.frame(buf) || nrow(buf) == 0L) return(buf)

    module_col <- if ('tag' %in% names(buf)) as.character(buf$tag) else rep(NA_character_, nrow(buf))
    time_col <- if ('time' %in% names(buf)) {
      if (inherits(buf$time, 'POSIXt')) sprintf('%.6f', as.numeric(buf$time)) else as.character(buf$time)
    } else {
      rep(NA_character_, nrow(buf))
    }
    message_col <- if ('message' %in% names(buf)) as.character(buf$message) else rep(NA_character_, nrow(buf))

    if ('line' %in% names(buf)) {
      line_col <- as.character(buf$line)

      missing_module <- is.na(module_col) | !nzchar(module_col)
      if (any(missing_module)) {
        parsed_module <- sub('^\\[\\s*([^]]+?)\\s+(?:[0-9]{2}:[0-9]{2}:[0-9]{2})?\\s*\\].*$', '\\1', line_col, perl = TRUE)
        parsed_module[parsed_module == line_col] <- NA_character_
        module_col[missing_module] <- trimws(parsed_module[missing_module])
      }

      missing_time <- is.na(time_col) | !nzchar(time_col)
      if (any(missing_time)) {
        parsed_time <- sub('^\\[\\s*.*?\\s+([0-9]{2}:[0-9]{2}:[0-9]{2})\\s*\\].*$', '\\1', line_col, perl = TRUE)
        parsed_time[parsed_time == line_col] <- NA_character_
        time_col[missing_time] <- parsed_time[missing_time]
      }

      missing_message <- is.na(message_col) | !nzchar(message_col)
      if (any(missing_message)) {
        parsed_message <- sub('^\\[\\s*.*?\\s+(?:[0-9]{2}:[0-9]{2}:[0-9]{2})?\\s*\\]\\s*', '', line_col, perl = TRUE)
        message_col[missing_message] <- parsed_message[missing_message]
      }
    }

    key <- paste(
      ifelse(is.na(module_col), '', trimws(module_col)),
      ifelse(is.na(time_col), '', trimws(time_col)),
      ifelse(is.na(message_col), '', trimws(message_col)),
      sep = '\r'
    )

    buf[!duplicated(key), , drop = FALSE]
  }

  parse_log_line <- function(line_text) {
    fallback <- list(module = NA_character_, event_time = NA_character_, log = NA_character_)
    if (!is.character(line_text) || length(line_text) == 0L || is.na(line_text)) return(fallback)

    txt <- trimws(line_text[1])
    if (identical(txt, "")) return(fallback)

    with_time <- regexec('^\\[\\s*(.*?)\\s+(\\d{2}:\\d{2}:\\d{2})\\s*\\]\\s*(.*)$', txt, perl = TRUE)
    hit_time <- regmatches(txt, with_time)[[1]]
    if (length(hit_time) == 4L) {
      return(list(module = trimws(hit_time[2]), event_time = hit_time[3], log = trimws(hit_time[4])))
    }

    no_time <- regexec('^\\[\\s*(.*?)\\s*\\]\\s*(.*)$', txt, perl = TRUE)
    hit_no_time <- regmatches(txt, no_time)[[1]]
    if (length(hit_no_time) == 3L) {
      return(list(module = trimws(hit_no_time[2]), event_time = NA_character_, log = trimws(hit_no_time[3])))
    }

    fallback$log <- txt
    fallback
  }

  rows <- NULL

  try({
    live_buffers <- get0('.miraprot_log_buffers', envir = globalenv(), inherits = FALSE)
    live_buf <- if (is.list(live_buffers)) live_buffers[['0']] else NULL
    if (is.data.frame(live_buf) && 'level' %in% names(live_buf)) {
      live_buf <- live_buf[!is.na(live_buf$level) & as.integer(live_buf$level) == 0L, , drop = FALSE]
    }

    restored_buffers <- get0('.miraprot_restored_log_buffers', envir = globalenv(), inherits = FALSE)
    restored_buf <- if (is.list(restored_buffers)) restored_buffers[['0']] else NULL
    if (is.data.frame(restored_buf) && 'level' %in% names(restored_buf)) {
      restored_buf <- restored_buf[!is.na(restored_buf$level) & as.integer(restored_buf$level) == 0L, , drop = FALSE]
    }

    restored_buf <- dedupe_debug_rows(restored_buf)
    live_buf <- dedupe_debug_rows(live_buf)

    if (is.data.frame(restored_buf) && nrow(restored_buf) > 0L) {
      live_rows <- if (is.data.frame(live_buf)) live_buf else restored_buf[0, , drop = FALSE]

      # Restored snapshots can contain the same module-filtered entries that
      # still exist in the live global buffer after restore/hot-reload.  Keep
      # the restored copy and remove exact duplicate live records before
      # concatenating sections so GO/GSEA/Dotplot/PCA/Heatmap logs do not show
      # a duplicated full log in exports.
      if (nrow(live_rows) > 0L && 'line' %in% names(restored_buf) && 'line' %in% names(live_rows)) {
        restored_lines <- as.character(restored_buf$line)
        live_rows <- live_rows[!(as.character(live_rows$line) %in% restored_lines), , drop = FALSE]
      }

      sep_row <- data.frame(
        time = as.POSIXct(NA),
        level = 0L,
        tag = "SESSION",
        message = "--- Current session ---",
        line = "[ SESSION ] --- Current session ---",
        run_id = "",
        event_id = "",
        stringsAsFactors = FALSE
      )

      rows <- dplyr::bind_rows(
        restored_buf,
        sep_row,
        live_rows
      )
    } else if (is.data.frame(live_buf)) {
      rows <- dedupe_debug_rows(live_buf)
    }
  })

  if ((is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) && !is.null(rv)) {
    candidate_names <- c('debug_logs', 'log_history', 'logs', 'debug_log_history')
    for (nm in candidate_names) {
      obj <- tryCatch(rv[[nm]], error = function(e) NULL)
      if (!is.null(obj) && is.data.frame(obj) && nrow(obj) > 0L) {
        rows <- obj
        break
      }
    }
  }

  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) {
    return(data.frame(Module = character(0), Time = character(0), Log = character(0), stringsAsFactors = FALSE))
  }

  rows <- dedupe_debug_rows(rows)

  module_col <- if ('tag' %in% names(rows)) as.character(rows$tag) else rep(NA_character_, nrow(rows))
  time_col <- rep(NA_character_, nrow(rows))
  if ('time' %in% names(rows)) {
    time_col <- format(rows$time, '%H:%M:%S')
  }
  log_col <- if ('message' %in% names(rows)) as.character(rows$message) else rep(NA_character_, nrow(rows))
  line_col <- if ('line' %in% names(rows)) as.character(rows$line) else rep(NA_character_, nrow(rows))

  parsed <- lapply(line_col, parse_log_line)
  parsed_module <- vapply(parsed, `[[`, character(1), 'module')
  parsed_time <- vapply(parsed, `[[`, character(1), 'event_time')
  parsed_log <- vapply(parsed, `[[`, character(1), 'log')

  use_module <- ifelse(!is.na(module_col) & nzchar(module_col), module_col, parsed_module)
  use_time <- ifelse(!is.na(time_col) & nzchar(time_col), time_col, parsed_time)
  use_log <- ifelse(!is.na(log_col) & nzchar(log_col), log_col, parsed_log)

  out <- data.frame(
    Module = ifelse(is.na(use_module) | !nzchar(use_module), 'UNKNOWN', use_module),
    Time = ifelse(is.na(use_time), '', use_time),
    Log = ifelse(is.na(use_log), '', use_log),
    stringsAsFactors = FALSE
  )

  out
}
