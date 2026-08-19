# Auto-Assign helper family. Loaded by datawizard_auto_assign_utils.R.

#######################
# --- NEW: Ratio extraction helpers (keep simple & robust) ---

# Extract substring between optional before/after regex boundaries
extract_between_boundaries <- function(x, before = NA_character_, after = NA_character_) {
  tryCatch({
    if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
    start_idx <- 1L
    end_idx   <- nchar(x)

    if (!is.null(before) && !is.na(before) && nzchar(before)) {
      loc <- stringr::str_locate(x, before)[1, ]
      if (is.na(loc[1])) return(NA_character_)
      start_idx <- loc[2] + 1L
    }

    if (!is.null(after) && !is.na(after) && nzchar(after)) {
      subx <- substr(x, start_idx, nchar(x))
      loc  <- stringr::str_locate(subx, after)[1, ]
      if (is.na(loc[1])) return(NA_character_)
      end_idx <- start_idx + loc[1] - 2L
    }

    if (end_idx < start_idx) return(NA_character_)
    out <- trimws(substr(x, start_idx, end_idx))
    if (!nzchar(out)) return(NA_character_)
    out
  }, error = function(e) {
    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(paste("extract_between_boundaries error:", e$message), level = 2)
    }
    NA_character_
  })
}

# Extract numerator/denominator using regex boundary fields from rules table
extract_ratio_components_regex <- function(column_name, nb, na, db, da, invert = FALSE, context = NULL) {
  tryCatch({
    norm <- normalize_ratio_boundaries(nb, na, db, da, context = context)
    if (isTRUE(norm$skip)) return(NULL)

    nb <- norm$nb; na <- norm$na; db <- norm$db; da <- norm$da

    # unabhängige Extraktion
    num <- extract_between_boundaries(column_name, nb, na)
    den <- extract_between_boundaries(column_name, db, da)

    # zu NA normalisieren (keine Listen, keine Vektoren)
    res_num <- if (!is.null(num) && !is.na(num) && nzchar(num)) as.character(num) else NA_character_
    res_den <- if (!is.null(den) && !is.na(den) && nzchar(den)) as.character(den) else NA_character_

    # wenn beide NA -> kein Treffer
    if (is.na(res_num) && is.na(res_den)) return(NULL)

    # invert nur wenn beide vorhanden
    if (isTRUE(invert) && !is.na(res_num) && !is.na(res_den)) {
      tmp <- res_num; res_num <- res_den; res_den <- tmp
    }

    list(numerator = res_num, denominator = res_den)
  }, error = function(e) {
    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(paste("extract_ratio_components_regex error:", e$message), level = 1)
    }
    NULL
  })
}

# Extract numerator/denominator by token positions
extract_ratio_components_position <- function(column_name, separators, num_pos, den_pos, invert = FALSE) {
  tryCatch({
    if (is.na(separators) || !nzchar(separators)) return(NULL)
    toks <- unlist(stringr::str_split(column_name, separators))
    toks <- trimws(toks)
    if (length(toks) == 0) return(NULL)

    get_at <- function(v, idx) {
      if (is.na(idx) || idx < 1 || idx > length(v)) return(NA_character_)
      v[[idx]]
    }
    num <- get_at(toks, as.integer(num_pos))
    den <- get_at(toks, as.integer(den_pos))
    if (is.na(num) || !nzchar(num) || is.na(den) || !nzchar(den)) return(NULL)
    if (isTRUE(invert)) {
      tmp <- num; num <- den; den <- tmp
    }
    list(numerator = num, denominator = den)
  }, error = function(e) {
    debug_auto_assign(paste("extract_ratio_components_position error:", e$message), level = 1)
    NULL
  })
}

# Wrapper: extract ratio components using a rule row
# Supports Method: "Regular Expression", "Regular Expressions", "Position in String"
extract_ratio_components_from_rule <- function(column_name, rule, debug_level = 0) {
  tryCatch({
    method <- as.character(if (!is.null(rule$Method)) rule$Method else "")
    method <- trimws(method)
    invert <- isTRUE(rule$Invert)

    # --- Regular Expression ---
    if (grepl("^Regular Expression", method, ignore.case = TRUE)) {
      nb <- if (!is.null(rule$NumBefore)) rule$NumBefore else NA_character_
      na <- if (!is.null(rule$NumAfter))  rule$NumAfter  else NA_character_
      db <- if (!is.null(rule$DenBefore)) rule$DenBefore else NA_character_
      da <- if (!is.null(rule$DenAfter))  rule$DenAfter  else NA_character_
      ctx <- if (!is.null(rule$Content)) paste0("content='", rule$Content, "', header='", column_name, "'") else column_name
      return(extract_ratio_components_regex(column_name, nb, na, db, da, invert, context = ctx))
    }

    # --- Position in String ---
    if (grepl("^Position in String$", method, ignore.case = TRUE)) {
      seps <- if (!is.null(rule$Separators)) rule$Separators else NA_character_
      np <- suppressWarnings(as.integer(if (!is.null(rule$NumPos)) rule$NumPos else NA_integer_))
      dp <- suppressWarnings(as.integer(if (!is.null(rule$DenPos)) rule$DenPos else NA_integer_))
      return(extract_ratio_components_position(column_name, seps, np, dp, invert))
    }

    # --- Pattern Recognition ---
    if (grepl("^Pattern Recognition$", method, ignore.case = TRUE)) {
      seps <- if (!is.null(rule$Separators) && nzchar(rule$Separators)) rule$Separators else NA_character_
      return(extract_ratio_components_pattern(column_name, seps, invert))
    }

    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(sprintf("Unsupported ratio method '%s' for header '%s'", method, column_name),
                        level = 2, debug_level)
    }
    NULL
  }, error = function(e) {
    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(paste("extract_ratio_components_from_rule error:", e$message), level = 1, debug_level)
    }
    NULL
  })
}

# --- NEW: notification helper (warning) ---
warn_auto_assign <- function(msg) {
  # Shiny-Notification wenn möglich, sonst warning()
  try({
    if (requireNamespace("shiny", quietly = TRUE)) {
      shiny::showNotification(msg, type = "warning", duration = 6)
    } else {
      warning(msg, call. = FALSE)
    }
  }, silent = TRUE)
  # Zusätzlich ins Debug-Log, falls vorhanden
  if (exists("debug_auto_assign", mode = "function")) {
    try(debug_auto_assign(msg, level = 2), silent = TRUE)
  }
}

# --- NEW: normalize/couple NumAfter & DenBefore as specified ---
normalize_ratio_boundaries <- function(nb, na, db, da, context = NULL) {
  to_na <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_character_)
    x <- as.character(x)
    if (!nzchar(x)) return(NA_character_)
    x
  }
  nb <- to_na(nb); na <- to_na(na); db <- to_na(db); da <- to_na(da)

  # Wenn NumAfter und DenBefore beide fehlen -> warn & skip
  if ((is.na(na) || !nzchar(na)) && (is.na(db) || !nzchar(db))) {
    ctx <- if (!is.null(context) && nzchar(context)) paste0(" (", context, ")") else ""
    warn_auto_assign(paste0("Ratio rule skipped: both NumAfter and DenBefore are missing", ctx, "."))
    return(list(skip = TRUE))
  }

  # Genau eine fehlt -> aneinander koppeln
  if (is.na(na) || !nzchar(na)) na <- db
  if (is.na(db) || !nzchar(db)) db <- na

  list(skip = FALSE, nb = nb, na = na, db = db, da = da)
}

###############
# --- Ratio: flow text <-> regex conversion helpers (only used when auto_convert_regex_dw = TRUE) ---

# Convert user flow-text to a safe regex boundary (ratio fields)
# Rules:
# - If input already looks like regex (contains backslash), return as-is (avoid double-escaping)
# - Escape regex metacharacters . + * ? ^ $ ( ) [ ] { } | :
# - Escape forward slash '/'
# - Collapse whitespace runs to \\s+
# - Trim
ratio_flow_to_regex <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  x <- as.character(x)
  if (!nzchar(x)) return(NA_character_)
  # Heuristic: already regex? (contains a backslash)
  if (grepl("\\\\", x)) return(x)

  s <- x

  # escape meta-chars (handle ':' too, common in 'Ratio: (')
  meta <- c(".", "+", "*", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|", ":")
  for (m in meta) {
    s <- gsub(paste0("\\", m), paste0("\\\\", m), s, perl = TRUE)
  }

  # escape forward slash
  s <- gsub("/", "\\\\/", s, perl = TRUE)

  # whitespace -> \s+
  s <- gsub("\\s+", "\\\\s+", s, perl = TRUE)

  # trim (no-op for regex but nice to keep consistent)
  s <- trimws(s)
  if (!nzchar(s)) return(NA_character_)
  s
}

# Convert stored regex boundary back to a readable flow-text (ratio fields)
# Rules:
# - Replace \\s+ (and any \\s{1,}) by single space
# - Unescape '\/' -> '/'
# - Unescape '\(' -> '(' and '\)' -> ')', and common meta back to literal where safe
# - Collapse multiple spaces to single space, trim
ratio_regex_to_flow <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  s <- as.character(x)
  if (!nzchar(s)) return(NA_character_)

  # \s+ or \s{...} -> single space
  s <- gsub("\\\\s\\+(?!\\w)", " ", s, perl = TRUE)           # \s+ -> space
  s <- gsub("\\\\s\\{\\d+(,\\d+)?\\}", " ", s, perl = TRUE)   # \s{m} or \s{m,n} -> space

  # '\/' -> '/'
  s <- gsub("\\\\/", "/", s, perl = TRUE)

  # unescape parens
  s <- gsub("\\\\\\(", "(", s, perl = TRUE)
  s <- gsub("\\\\\\)", ")", s, perl = TRUE)

  # unescape colon if present
  s <- gsub("\\\\:", ":", s, perl = TRUE)

  # many other meta remain escaped in stored regex; for readability we can safely unescape these common ones
  # ONLY if they are not part of a character class or quantifier (keep it simple):
  s <- gsub("\\\\([\\.\\+\\*\\?\\^\\$\\[\\]\\{\\}\\|])", "\\1", s, perl = TRUE)

  # normalize spaces
  s <- gsub("\\s+", " ", s, perl = TRUE)
  s <- trimws(s)
  if (!nzchar(s)) return(NA_character_)
  s
}

# --- Ratio tokenization helper ---
# Split header by a separators-regex (if provided), trim tokens, drop empties.
ratio_tokenize <- function(x, separators_regex = NA_character_) {
  tryCatch({
    if (is.null(x) || is.na(x) || !nzchar(x)) {
      return(character(0))
    }

    # 1) Build the effective separator regex
    sep <- if (!is.null(separators_regex) && nzchar(separators_regex)) {
      separators_regex
    } else {
      # default from original implementation
      "\\s+|\\(|\\)|\\[|\\]|\\{|\\}|/|_|-"
    }

    # 2) Compute token boundaries using regex matches on the *original* string x.
    #    We do NOT split first and reconstruct later; instead we track indices
    #    so that we can always slice out exact substrings (for n-grams).
    #
    #    Example: x = "A_B_C", sep = "_"
    #    -> separator matches at positions 2 and 4
    #       tokens:
    #         [1,1] -> "A"
    #         [3,3] -> "B"
    #         [5,5] -> "C"

    # Ensure we are working with a single string
    x <- as.character(x)[1]

    # Find all separator occurrences (start/end indices)
    # We wrap sep in a non-capturing group to avoid issues with alternations.
    locs <- tryCatch(
      stringr::str_locate_all(x, paste0("(", sep, ")"))[[1]],
      error = function(e) {
        if (exists("debug_auto_assign", mode = "function")) {
          debug_auto_assign(paste("ratio_tokenize str_locate_all error:", e$message), level = 2)
        }
        matrix(numeric(0), ncol = 2)
      }
    )

    nchar_x <- nchar(x)

    # If no separators are found, we fall back to the whole string as single token
    if (is.null(dim(locs)) || nrow(locs) == 0) {
      tok <- trimws(x)
      if (!nzchar(tok)) return(character(0))
      return(tok)
    }

    # Build atomic token ranges [start, end] between separators
    starts <- c(1L, locs[, "end"] + 1L)
    ends   <- c(locs[, "start"] - 1L, nchar_x)

    # Sanity clamp (in case separators are at boundaries)
    starts[starts < 1L] <- 1L
    ends[ends > nchar_x] <- nchar_x

    # Filter out invalid ranges
    valid <- which(starts <= ends)
    starts <- starts[valid]
    ends   <- ends[valid]

    # Extract atomic tokens
    atomic_tokens <- character(0)
    atomic_ranges <- list()
    if (length(starts)) {
      for (i in seq_along(starts)) {
        s <- starts[i]
        e <- ends[i]
        token_raw <- substr(x, s, e)
        token <- trimws(token_raw)
        if (nzchar(token)) {
          # Avoid consecutive duplicates (same as original behaviour)
          if (!length(atomic_tokens) || tail(atomic_tokens, 1) != token) {
            atomic_tokens <- c(atomic_tokens, token)
            atomic_ranges[[length(atomic_ranges) + 1L]] <- c(s, e)
          }
        }
      }
    }

    if (!length(atomic_tokens)) {
      return(character(0))
    }

    # 3) Build n-grams as *exact substrings* from original string x.
    #
    #    For each contiguous range of tokens i..j, we take:
    #      start = start_of_token_i
    #      end   = end_of_token_j
    #    and slice substr(x, start, end).
    #
    #    This is independent of the exact separator characters inside.

    n <- length(atomic_tokens)
    ngrams <- character(0)

    if (n >= 2L) {
      for (len in 2L:n) {
        for (i in seq_len(n - len + 1L)) {
          j <- i + len - 1L
          range_i <- atomic_ranges[[i]]
          range_j <- atomic_ranges[[j]]
          s <- range_i[1]
          e <- range_j[2]
          if (!is.na(s) && !is.na(e) && s <= e) {
            ng_raw <- substr(x, s, e)
            ng <- trimws(ng_raw)
            if (nzchar(ng)) {
              ngrams <- c(ngrams, ng)
            }
          }
        }
      }
    }

    # 4) Combine atomic tokens + n-grams, preserving order of first occurrence,
    #    and keeping behaviour consistent with previous implementation.
    all_tokens <- c(atomic_tokens, ngrams)
    if (length(all_tokens)) {
      all_tokens <- all_tokens[!duplicated(all_tokens)]
    }

    all_tokens
  }, error = function(e) {
    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(paste("ratio_tokenize error:", e$message), level = 2)
    }
    character(0)
  })
}


# --- Known samples registry for pattern recognition ---
# Set this once (e.g., after applying sample rules): options(dw_known_samples = unique(na.omit(df$Sample)))
get_known_samples_dw <- function() {
  ks <- tryCatch(getOption("dw_known_samples", default = character(0)), error = function(e) character(0))
  ks <- unique(trimws(as.character(ks)))
  ks[nzchar(ks)]
}

# --- Position in String method (robust & partial-friendly) ---
extract_ratio_components_position <- function(column_name, separators, num_pos, den_pos, invert = FALSE) {
  tryCatch({
    toks <- ratio_tokenize(column_name, separators)
    get_at <- function(v, idx) {
      if (is.na(idx) || is.null(idx)) return(NA_character_)
      idx <- suppressWarnings(as.integer(idx))
      if (is.na(idx) || idx < 1 || idx > length(v)) return(NA_character_)
      v[[idx]]
    }
    num <- get_at(toks, num_pos)
    den <- get_at(toks, den_pos)

    res_num <- if (!is.null(num) && !is.na(num) && nzchar(num)) num else NA_character_
    res_den <- if (!is.null(den) && !is.na(den) && nzchar(den)) den else NA_character_

    if (isTRUE(invert) && !is.na(res_num) && !is.na(res_den)) {
      tmp <- res_num; res_num <- res_den; res_den <- tmp
    }
    if (is.na(res_num) && is.na(res_den)) return(NULL)
    list(numerator = res_num, denominator = res_den)
  }, error = function(e) {
    if (exists("debug_auto_assign", mode = "function")) {
      debug_auto_assign(paste("extract_ratio_components_position error:", e$message), level = 1)
    }
    NULL
  })
}

# --- Pattern Recognition method (shared, span-based semantics) ---
extract_ratio_components_pattern <- function(column_name, separators, invert = FALSE) {
  tryCatch({
    # Span resolution is deliberately shared with Auto RegEx.  Do not fall
    # back to token or parenthesis ordering: ambiguous/repeated samples must
    # abstain identically during inference and runtime replay.
    return(datawizard_resolve_known_sample_ratio(
      column_name, get_known_samples_dw(), invert = invert))
  }, error = function(e) {
    debug_auto_assign(paste("extract_ratio_components_pattern error:", e$message), level = 1)
    NULL
  })
}
