# ============================================================================
# Shared canonical condition-rule extraction
#
# This dependency is intentionally outside both Auto RegEx and Auto-Assign so
# either module can source it without acquiring a dependency on the other.
# ============================================================================

# Extract a vector using the persisted condition-rule fields. Boundary patterns
# are always case-sensitive, including when the caller's regex defaults differ.
# For `between`, each left boundary is paired with the first right boundary that
# starts after it ends, and the first such ordered pair wins. This also defines
# identical before/after boundaries: the text between their first two
# non-overlapping occurrences is selected.
datawizard_extract_condition_vector <- function(x, method, before = "", after = "",
    separators = "", pos = 1L) {
  x <- as.character(x)
  n <- length(x)
  method <- if (length(method)) as.character(method[[1L]]) else ""
  scalar <- function(value, default = "") {
    if (!length(value) || is.na(value[[1L]])) default else as.character(value[[1L]])
  }
  before <- scalar(before)
  after <- scalar(after)
  separators <- scalar(separators)
  pos <- if (length(pos) && !is.na(pos[[1L]])) as.integer(pos[[1L]]) else 1L
  case_sensitive <- function(pattern) {
    if (nzchar(pattern) && !startsWith(pattern, "(?-i:"))
      paste0("(?-i:", pattern, ")")
    else pattern
  }
  before_cs <- case_sensitive(before)
  after_cs <- case_sensitive(after)
  locate <- function(value, pattern) {
    if (!nzchar(pattern))
      return(matrix(c(1L, 0L), nrow = 1L,
        dimnames = list(NULL, c("start", "end"))))
    stringr::str_locate_all(value, pattern)[[1L]]
  }
  extract_one <- function(value) tryCatch(switch(method,
    between = {
      left <- locate(value, before_cs)
      right <- locate(value, after_cs)
      if (!nrow(left) || !nrow(right)) return(NA_character_)
      pairs <- do.call(rbind, lapply(seq_len(nrow(left)), function(i) {
        j <- which(right[, "start"] > left[i, "end"])[1L]
        if (length(j)) c(left[i, "end"] + 1L, right[j, "start"] - 1L)
        else NULL
      }))
      if (is.null(pairs) || !nrow(pairs)) NA_character_
      else substr(value, pairs[1L, 1L], pairs[1L, 2L])
    },
    start = {
      if (nzchar(after_cs)) {
        location <- stringr::str_locate(value, after_cs)[1L, "start"]
        if (!is.na(location) && location > 1L) substr(value, 1L, location - 1L)
        else value
      } else value
    },
    end = {
      if (nzchar(before_cs)) {
        locations <- stringr::str_locate_all(value, before_cs)[[1L]]
        if (nrow(locations)) {
          last <- locations[nrow(locations), "end"]
          if (last < nchar(value)) substr(value, last + 1L, nchar(value)) else ""
        } else value
      } else value
    },
    whole = value,
    phrase_position = {
      parts <- stringr::str_split(value, separators, simplify = TRUE)
      if (ncol(parts) >= pos) parts[pos] else NA_character_
    },
    NA_character_), error = function(e) NA_character_)

  # pattern_detect is defined over the batch, because eligibility depends on
  # variation down a token column rather than on an individual input value.
  if (identical(method, "pattern_detect")) return(tryCatch({
    pieces <- strsplit(x, separators, perl = TRUE)
    width <- max(lengths(pieces))
    matrix_values <- t(vapply(pieces, function(piece) {
      length(piece) <- width
      piece
    }, character(width)))
    matrix_values <- apply(matrix_values, c(1L, 2L), function(value)
      gsub("^[[:punct:]]+|[[:punct:]]+$", "", trimws(value)))
    if (is.null(dim(matrix_values))) matrix_values <- matrix(matrix_values, nrow = n)
    eligible <- which(vapply(seq_len(ncol(matrix_values)), function(j) {
      unique_values <- unique(matrix_values[, j])
      length(unique_values) > 1L && length(unique_values) < nrow(matrix_values)
    }, logical(1L)))
    if (length(eligible) >= pos) matrix_values[, eligible[pos]]
    else rep(NA_character_, n)
  }, error = function(e) rep(NA_character_, n)))

  vapply(x, extract_one, character(1L))
}
