# ============================================================================
# Auto Regex regex and transformation primitives.
# Sourced into the caller environment by datawizard_auto_regex_utils.R.
# ============================================================================

MAX_REGEX_LENGTH <- 2000L
MAX_REGEX_COMPLEXITY <- 100L
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# Escape only PCRE metacharacters.  Whitespace and slash are ordinary PCRE
# literals and are intentionally not rewritten here.
regex_escape_literal <- function(x) {
  x <- chr(x)
  gsub("([\\\\.^$|()\\[\\]{}*+?])", "\\\\\\1", x, perl=TRUE)
}

regex_atom_for_token <- function(token, whitespace=c("evidence", "exact", "one_or_more", "optional"), evidence=token) {
  whitespace <- match.arg(whitespace)
  token <- chr(token)
  if (!length(token)) return(character())
  is_ws <- grepl("^[[:space:]]*$", token) & nzchar(token)
  out <- regex_escape_literal(token)
  if (any(is_ws)) {
    mode <- whitespace
    if (identical(mode, "evidence")) {
      observed <- chr(evidence)
      observed <- observed[!nzchar(observed) | grepl("^[[:space:]]+$", observed)]
      if (!length(observed)) observed <- token[is_ws]
      mode <- if (any(!nzchar(observed))) "optional" else if (length(unique(observed)) > 1L) "one_or_more" else "exact"
    }
    out[is_ws] <- switch(mode, exact=regex_escape_literal(token[is_ws]),
      one_or_more="\\s+", optional="\\s*")
  }
  out
}

regex_join_atoms <- function(atoms, operator=c("concatenate", "alternation")) {
  operator <- match.arg(operator)
  paste(chr(atoms), collapse=if (operator == "alternation") "|" else "")
}

# Data Wizard flow conversion escapes slash for content and regex-boundary
# fields, but separator selections are persisted directly.  A small scanner
# makes conversion idempotent and preserves literal backslashes.
regex_map_slashes <- function(pattern, store=TRUE) {
  map_one <- function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    chars <- strsplit(value, "", fixed=TRUE)[[1L]]; out <- character(); run <- 0L
    for (ch in chars) {
      if (ch == "\\") { run <- run + 1L; next }
      if (ch == "/") {
        if (store && run %% 2L == 0L) run <- run + 1L
        if (!store && run %% 2L == 1L) run <- run - 1L
      }
      out <- c(out, rep("\\", run), ch); run <- 0L
    }
    paste0(c(out, rep("\\", run)), collapse="")
  }
  vapply(as.character(pattern), map_one, character(1), USE.NAMES=FALSE)
}

regex_to_miraprot_storage <- function(pattern, field=c("content", "condition_boundary", "ratio_boundary", "separator")) {
  field <- match.arg(field)
  if (field == "separator") as.character(pattern) else regex_map_slashes(pattern, TRUE)
}
regex_from_miraprot_storage <- function(pattern, field=c("content", "condition_boundary", "ratio_boundary", "separator")) {
  field <- match.arg(field)
  if (field == "separator") as.character(pattern) else regex_map_slashes(pattern, FALSE)
}

regex_validation_result <- function(valid, engine, message, pattern) structure(list(
  valid=isTRUE(valid), engine=engine, message=as.character(message),
  length=if (length(pattern) && !is.na(pattern)) nchar(pattern, type="chars") else NA_integer_,
  complexity=if (length(pattern) && !is.na(pattern)) regex_complexity(pattern) else NA_integer_
), class="miraprot_regex_validation")

validate_pcre <- function(pattern) {
  pattern <- if (length(pattern)) as.character(pattern[[1L]]) else NA_character_
  issue <- NULL
  ok <- if (is.na(pattern) || !nzchar(pattern)) TRUE else isTRUE(tryCatch({
    grepl(pattern, "", perl=TRUE); TRUE
  }, warning=function(w) { issue <<- conditionMessage(w); FALSE }, error=function(e) { issue <<- conditionMessage(e); FALSE }))
  regex_validation_result(ok, "PCRE", if (ok) "" else issue, pattern)
}
validate_stringr_pattern <- function(pattern) {
  pattern <- if (length(pattern)) as.character(pattern[[1L]]) else NA_character_
  issue <- NULL
  ok <- if (is.na(pattern) || !nzchar(pattern)) TRUE else if (!requireNamespace("stringr", quietly=TRUE)) {
    issue <- "Package 'stringr' is not installed."; FALSE
  } else isTRUE(tryCatch({ stringr::str_detect("", stringr::regex(pattern)); TRUE },
    warning=function(w) { issue <<- conditionMessage(w); FALSE }, error=function(e) { issue <<- conditionMessage(e); FALSE }))
  regex_validation_result(ok, "stringr/ICU", if (ok) "" else issue, pattern)
}

# Compatibility names now expose the stricter centralized semantics.
escape_regex <- regex_escape_literal
valid_regex <- validate_pcre
safe_grepl <- function(pattern, x) {
  validation <- validate_pcre(pattern)
  if (!validation$valid || validation$length > MAX_REGEX_LENGTH) return(rep(FALSE, length(x)))
  tryCatch(grepl(paste0("(?-i:", pattern, ")"), chr(x), perl=TRUE), error=function(e) rep(FALSE,length(x)))
}

# Infer the Data Wizard value from reference rows only.  A separate details
# object supports actionable validation and bounded logging while this public
# helper retains the persisted scalar contract.
content_transformation_details <- function(df, label) {

  label <- as.character(label)[1L]

  answer <- list(
    value = NA_character_,
    source = "not_applicable",
    message = "",
    values = character(),
    row_ids = integer()
  )

  if (is.na(label) ||
      identical(label, "Row Index") ||
      !label %in% TRANSFORMATION_CONTENT_TYPES) {
    return(answer)
  }

  rows <- if (is.data.frame(df) &&
              "Content" %in% names(df)) {

    which(
      !is.na(df$Content) &
        as.character(df$Content) == label
    )

  } else {

    integer()
  }

  if (!"Transformation" %in% names(df)) {

    answer$value <- "None"
    answer$source <- "compatible_default"

    answer$message <- paste(
      "Transformation metadata is absent;",
      "using the MiraProt-compatible default 'None'."
    )

    return(answer)
  }

  raw <- as.character(
    df$Transformation[rows]
  )

  normalized <- trimws(raw)

  nonblank <-
    !is.na(normalized) &
    nzchar(normalized)

  if (!any(nonblank)) {

    answer$value <- "None"
    answer$source <- "compatible_default"

    answer$message <- paste(
      "All reference transformations are blank or missing;",
      "using the MiraProt-compatible default 'None'."
    )

    return(answer)
  }

  supported_rows <-
    nonblank &
    normalized %in%
    SUPPORTED_TRANSFORMATIONS

  ignored_rows <-
    nonblank &
    !normalized %in%
    SUPPORTED_TRANSFORMATIONS

  supported_values <- unique(
    normalized[supported_rows]
  )

  ignored_values <- unique(
    normalized[ignored_rows]
  )

  ignored_row_ids <- rows[
    ignored_rows
  ]

  # Unknown metadata values are not inference authority. If no supported
  # transformation remains, use the same safe default as missing metadata.
  if (!length(supported_values)) {

    answer$value <- "None"

    if (length(ignored_values)) {

      answer$source <-
        "compatible_default_ignored_unknown"

      answer$values <-
        ignored_values

      answer$row_ids <-
        ignored_row_ids

      answer$message <- sprintf(
        paste0(
          "%s has unknown Transformation value(s) %s at row ID(s) %s; ",
          "these values are ignored and Auto RegEx uses 'None'."
        ),
        label,
        paste(
          sprintf(
            "'%s'",
            ignored_values
          ),
          collapse = ", "
        ),
        paste(
          ignored_row_ids,
          collapse = ", "
        )
      )

    } else {

      answer$source <-
        "compatible_default"
    }

    return(answer)
  }

  # Unknown values do not participate in the conflict test. A conflict exists
  # only when the metadata supplies more than one different supported value.
  if (length(supported_values) > 1L) {

    supported_row_ids <- rows[
      supported_rows
    ]

    answer$source <- "conflict"

    answer$values <-
      supported_values

    answer$row_ids <-
      supported_row_ids

    answer$message <- sprintf(
      paste0(
        "%s has conflicting supported Transformation values %s ",
        "at row ID(s) %s; select one supported value manually ",
        "in the editable Content Rules table before export."
      ),
      label,
      paste(
        sprintf(
          "'%s'",
          supported_values
        ),
        collapse = ", "
      ),
      paste(
        supported_row_ids,
        collapse = ", "
      )
    )

    return(answer)
  }

  answer$value <-
    supported_values[[1L]]

  if (length(ignored_values)) {

    answer$source <-
      "metadata_with_ignored_unknowns"

    answer$values <-
      ignored_values

    answer$row_ids <-
      ignored_row_ids

    answer$message <- sprintf(
      paste0(
        "%s uses supported Transformation '%s'; unknown value(s) %s ",
        "at row ID(s) %s are ignored."
      ),
      label,
      answer$value,
      paste(
        sprintf(
          "'%s'",
          ignored_values
        ),
        collapse = ", "
      ),
      paste(
        ignored_row_ids,
        collapse = ", "
      )
    )

  } else {

    answer$source <- "metadata"
  }

  answer
}

infer_content_transformation <- function(df, label) {
  details <- content_transformation_details(df,label)
  if (details$source %in% c("unsupported", "conflict")) {
    stop(structure(list(message=details$message, call=NULL, content=as.character(label)[1L],
      values=details$values, row_ids=details$row_ids, reason=details$source),
      class=c(if(details$source=="conflict")"miraprot_transformation_ambiguity" else
          "miraprot_transformation_unsupported",
        paste0("miraprot_transformation_",details$source),
        "miraprot_transformation_validation_error","error","condition")))
  }
  details$value
}

normalize_transformation_values <- function(content, transformation) {
  content <- as.character(content)
  transformation <- as.character(transformation)
  transformation <- trimws(transformation)
  missing <- is.na(transformation) | !nzchar(transformation)
  capable <- !is.na(content) & content %in% TRANSFORMATION_CONTENT_TYPES
  transformation[missing & capable] <- "None"
  transformation[missing & !capable] <- NA_character_
  transformation
}
